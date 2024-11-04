/*
 * Copyright (c) 2024 silt
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License as published by the Free
 * Software Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see https://www.gnu.org/licenses/.
 */

#![allow(internal_features)]
#![feature(lazy_get)]
#![feature(core_intrinsics)]

mod atomic_f64;

use atomic_f64::AtomicF64;

use image::{GenericImageView, ImageBuffer, ImageReader, Pixel, Rgba};
use notify::{Config, INotifyWatcher, Watcher};
use smithay_client_toolkit::{
	compositor::{CompositorHandler, CompositorState, Region},
	delegate_compositor, delegate_layer, delegate_output, delegate_registry, delegate_shm,
	output::{OutputHandler, OutputState},
	registry::{ProvidesRegistryState, RegistryState},
	registry_handlers,
	shell::{
		wlr_layer::{
			self, Anchor, LayerShell, LayerShellHandler, LayerSurface, LayerSurfaceConfigure,
		},
		WaylandSurface,
	},
	shm::{slot::SlotPool, Shm, ShmHandler},
};
use std::{
	env,
	ffi::{c_double, c_void},
	intrinsics::unlikely,
	ops::Sub,
	path::Path,
	sync::{
		atomic::{self, AtomicBool, AtomicU32, AtomicUsize},
		Mutex,
	},
	thread,
	time::Duration,
};
use wayland_client::{
	globals::registry_queue_init,
	protocol::{wl_output, wl_shm, wl_surface},
	Connection, QueueHandle,
};

struct ClientState {
	registry_state:  RegistryState,
	output_state:    OutputState,
	shm:             Shm,
	exit:            bool,
	first_configure: bool,
	pool:            SlotPool,
	width:           u32,
	height:          u32,
	layer:           LayerSurface,
}

impl CompositorHandler for ClientState {
	fn scale_factor_changed(
		&mut self,
		_conn: &Connection,
		_qh: &QueueHandle<Self>,
		_surface: &wl_surface::WlSurface,
		_new_factor: i32,
	) {
	}

	fn transform_changed(
		&mut self,
		_conn: &Connection,
		_qh: &QueueHandle<Self>,
		_surface: &wl_surface::WlSurface,
		_new_transform: wl_output::Transform,
	) {
	}

	fn frame(
		&mut self,
		_conn: &Connection,
		qh: &QueueHandle<Self>,
		_surface: &wl_surface::WlSurface,
		_time: u32,
	) {
		self.draw(qh);
	}

	fn surface_enter(
		&mut self,
		_conn: &Connection,
		_qh: &QueueHandle<Self>,
		_surface: &wl_surface::WlSurface,
		_output: &wl_output::WlOutput,
	) {
	}

	fn surface_leave(
		&mut self,
		_conn: &Connection,
		_qh: &QueueHandle<Self>,
		_surface: &wl_surface::WlSurface,
		_output: &wl_output::WlOutput,
	) {
	}
}

// TODO: handle new/destroyed outputs for multi-output wlneko
impl OutputHandler for ClientState {
	fn output_state(&mut self) -> &mut OutputState {
		&mut self.output_state
	}

	fn new_output(
		&mut self,
		_conn: &Connection,
		_qh: &QueueHandle<Self>,
		output: wl_output::WlOutput,
	) {
		if let Some((x, y)) = self.output_state.info(&output).unwrap().logical_size {
			// if your monitor has negative dimensions, good fucking luck
			self.layer.set_size(x.try_into().unwrap(), y.try_into().unwrap());
			self.layer.commit();
		}
	}

	fn update_output(
		&mut self,
		_conn: &Connection,
		_qh: &QueueHandle<Self>,
		output: wl_output::WlOutput,
	) {
		if let Some((x, y)) = self.output_state.info(&output).unwrap().logical_size {
			self.layer.set_size(x.try_into().unwrap(), y.try_into().unwrap());
			self.layer.commit();
		}
	}

	fn output_destroyed(
		&mut self,
		_conn: &Connection,
		_qh: &QueueHandle<Self>,
		_output: wl_output::WlOutput,
	) {
		if self.output_state.outputs().filter_map(|x| self.output_state.info(&x)).count() == 0 {
			yap!("last output destroyed; exiting");
			self.exit = true;
		}
	}
}

impl LayerShellHandler for ClientState {
	fn closed(&mut self, _conn: &Connection, _qh: &QueueHandle<Self>, _layer: &LayerSurface) {
		self.exit = true;
	}

	fn configure(
		&mut self,
		_conn: &Connection,
		qh: &QueueHandle<Self>,
		_layer: &LayerSurface,
		configure: LayerSurfaceConfigure,
		_serial: u32,
	) {
		if configure.new_size.0 == 0 || configure.new_size.1 == 0 {
			self.width = 1920;
			self.height = 1080;
		} else {
			self.width = configure.new_size.0;
			self.height = configure.new_size.1;
		}

		// draw upon receiving first configure event
		if self.first_configure {
			self.first_configure = false;
			self.draw(qh);
		}
	}
}

impl ShmHandler for ClientState {
	fn shm_state(&mut self) -> &mut Shm {
		&mut self.shm
	}
}

impl ClientState {
	pub fn draw(&mut self, qh: &QueueHandle<Self>) {
		let width = self.width;
		let height = self.height;
		let stride = self.width as i32 * 4;

		let (buffer, canvas) = self
			.pool
			.create_buffer(
				width as i32,
				height as i32,
				stride,
				wl_shm::Format::Argb8888,
			)
			.expect("create buffer");

		let px = POINTER_COORDS.0.load(atomic::Ordering::SeqCst).round() as i64;
		let py = POINTER_COORDS.1.load(atomic::Ordering::SeqCst).round() as i64;
		let nx = NEKO_COORDS.0.load(atomic::Ordering::SeqCst) as i64;
		let ny = NEKO_COORDS.1.load(atomic::Ordering::SeqCst) as i64;

		let deg = ((px - nx) as f64).atan2((py - ny) as f64);

		let (mut dx, mut dy) =
			if (((px - nx).pow(2) + (py - ny).pow(2)) as f64).sqrt() < SPRITE_SIZE as f64 * 1.3 {
				(0.0, 0.0)
			} else {
				(deg.sin() * 1.0, deg.cos() * 1.0)
			};

		let frame = FRAME.load(atomic::Ordering::SeqCst);
		let idle_frame = IDLE_FRAME.load(atomic::Ordering::SeqCst);
		IDLE_FRAME.store(
			match idle_frame.wrapping_add(1) {
				0 => 210,
				x => x,
			},
			atomic::Ordering::SeqCst,
		);

		let spritesheet_idx: usize = match (
			((dx * 10.0).round() / 10.0).round() as i64,
			((dy * 10.0).round() / 10.0).round() as i64,
		) {
			(0, 0) => match idle_frame {
				0..50 => 0,
				50..60 => 2,
				60..70 => 3,
				70..80 => 2,
				80..90 => 3,
				90..100 => 2,
				100..110 => 3,
				110..140 => 0,
				140..190 => 4,
				190..210 => 0,
				210.. => 6 - ((idle_frame - 210) % 70).clamp(34, 35).sub(34),
			},
			v => {
				if idle_frame > 0 {
					IDLE_FRAME.store(
						if idle_frame > 40 { 40 } else { idle_frame - 1 },
						atomic::Ordering::SeqCst,
					);
					(dx, dy) = (0.0, 0.0);
					7
				} else {
					IDLE_FRAME.store(0, atomic::Ordering::SeqCst);
					match v {
						(1.., 1..) => 10 + (frame % 20).clamp(9, 10).sub(9),
						(..0, ..0) => 18 + (frame % 20).clamp(9, 10).sub(9),
						(..0, 1..) => 22 + (frame % 20).clamp(9, 10).sub(9),
						(1.., ..0) => 14 + (frame % 20).clamp(9, 10).sub(9),
						(0, 1..) => 8 + (frame % 20).clamp(9, 10).sub(9),
						(0, ..0) => 16 + (frame % 20).clamp(9, 10).sub(9),
						(1.., 0) => 12 + (frame % 20).clamp(9, 10).sub(9),
						(..0, 0) => 20 + (frame % 20).clamp(9, 10).sub(9),
						(0, 0) => unreachable!(),
					}
				}
			}
		};

		FRAME.store(frame.wrapping_add(1), atomic::Ordering::SeqCst);

		NEKO_COORDS.0.store((nx as f64 + dx).round() as u32, atomic::Ordering::SeqCst);
		NEKO_COORDS.1.store((ny as f64 + dy).round() as u32, atomic::Ordering::SeqCst);

		let spritesheet = SPRITESHEET.lock().unwrap();
		let sprite = &spritesheet[spritesheet_idx];

		canvas.chunks_exact_mut(4).enumerate().for_each(|(index, chunk)| {
			let x = (index % width as usize) as u32;
			let y = (index / width as usize) as u32;

			let x = x - nx as u32 + SPRITE_SIZE / 2;
			let y = y - ny as u32 + SPRITE_SIZE / 2;

			let (a, r, g, b) = if (nx..=nx + 31).contains(&(x as i64 + nx))
				&& (ny..=ny + 31).contains(&(y as i64 + ny))
			{
				let pixel = sprite.get_pixel(x % 32, y % 32).channels();
				(pixel[3], pixel[0], pixel[1], pixel[2])
			} else {
				(0, 0, 0, 0)
			};

			let array: &mut [u8; 4] = chunk.try_into().unwrap();
			*array = [b, g, r, a];
		});

		// unlock access to the spritesheet
		drop(spritesheet);

		// 3x3-sprite damage area, just to be safe
		self.layer.wl_surface().damage_buffer(
			nx as i32 - SPRITE_SIZE as i32,
			ny as i32 - SPRITE_SIZE as i32,
			SPRITE_SIZE as i32 * 3,
			SPRITE_SIZE as i32 * 3,
		);

		self.layer.wl_surface().frame(qh, self.layer.wl_surface().clone());

		buffer.attach_to(self.layer.wl_surface()).expect("buffer attach");
		self.layer.commit();

		// crude frame timing, can def be improved
		thread::sleep(Duration::from_millis(8));
	}
}

delegate_compositor!(ClientState);
delegate_output!(ClientState);
delegate_shm!(ClientState);
delegate_layer!(ClientState);
delegate_registry!(ClientState);

impl ProvidesRegistryState for ClientState {
	fn registry(&mut self) -> &mut RegistryState {
		&mut self.registry_state
	}
	registry_handlers![OutputState];
}

// bindgen was being annoying so this is only as filled out as much as it needs to be
#[repr(C)]
struct wlr_cursor {
	state: *mut c_void,
	x:     c_double,
	y:     c_double,
}

static INIT_ALREADY: AtomicBool = AtomicBool::new(false);

static POINTER_COORDS: (AtomicF64, AtomicF64) = (AtomicF64::new(0.0), AtomicF64::new(0.0));
static NEKO_COORDS: (AtomicU32, AtomicU32) = (AtomicU32::new(0), AtomicU32::new(0));

const SPRITE_SIZE: u32 = 32;
const SPRITE_BORDER_WIDTH: u32 = 1;
const NEKO_DEFAULT: &[u8] = include_bytes!("../assets/neko.png");
static SPRITESHEET: Mutex<Vec<ImageBuffer<Rgba<u8>, Vec<u8>>>> = Mutex::new(vec![]);

static FRAME: AtomicUsize = AtomicUsize::new(0);
static IDLE_FRAME: AtomicUsize = AtomicUsize::new(0);

// it's the only way, actually
fn skin_cat() {
	yap!("loading spritesheet from neko.png");
	let first_time = SPRITESHEET.lock().unwrap().is_empty();
	let mut img = if let Ok(img) =
		env::current_dir().map(|x| Path::join(&x, "neko.png")).and_then(ImageReader::open)
	{
		if let Ok(img) = img.decode().map(|x| x.to_rgba8()) {
			img
		} else if first_time {
			yap!("failed to decode; falling back to default spritesheet");
			image::load_from_memory_with_format(NEKO_DEFAULT, image::ImageFormat::Png)
				.unwrap()
				.to_rgba8()
		} else {
			yap!("failed to decode; falling back to previous spritesheet");
			return;
		}
	} else if first_time {
		yap!("failed to open; falling back to default spritesheet");
		image::load_from_memory_with_format(NEKO_DEFAULT, image::ImageFormat::Png)
			.unwrap()
			.to_rgba8()
	} else {
		yap!("failed to open; falling back to previous spritesheet");
		return;
	};

	// key out the background color, which is for some reason opaque
	{
		// `image` has forced my hand here. safe get_pixel variants return references, cue borrow checker issues
		// doesn't look like any of the sheets utilize the top left corner. this is probably fine.
		let pixel = unsafe { img.unsafe_get_pixel(0, 0) };
		let key = pixel.channels();
		img.enumerate_pixels_mut().for_each(|(_, _, p)| {
			if p.channels() == key {
				p.0 = [0; 4]
			}
		});
	}

	yap!("successfully loaded spritesheet");
	*(SPRITESHEET.lock().unwrap()) = (0..=31)
		.map(|i| {
			img.view(
				(i % 8) * (SPRITE_SIZE + SPRITE_BORDER_WIDTH),
				(i / 8) * (SPRITE_SIZE + SPRITE_BORDER_WIDTH),
				SPRITE_SIZE,
				SPRITE_SIZE,
			)
			.to_image()
		})
		.collect();
}

redhook::hook! {
	unsafe fn wlr_cursor_move(cur: *mut wlr_cursor, dev: *mut c_void, delta_x: c_double, delta_y: c_double) => cursor_hook {
		if unlikely(!INIT_ALREADY.swap(true, atomic::Ordering::Relaxed)) {
			yap!("hit first cursor movement; attempting to init wayland client");
			thread::spawn(init);
		}

		POINTER_COORDS.0.store((*cur).x, atomic::Ordering::SeqCst);
		POINTER_COORDS.1.store((*cur).y, atomic::Ordering::SeqCst);

		redhook::real!(wlr_cursor_move)(cur, dev, delta_x, delta_y);
	}
}

#[macro_export]
macro_rules! yap {
	($m:expr) => {{
		let v = $m;
		eprintln!("[wlneko/{}:{}] {}", file!(), line!(), v);
		v
	}};
}

fn init() {
	yap!("successfully entered init; proceeding");

	skin_cat();

	let conn = Connection::connect_to_env().expect("failed to connect to wayland socket");

	let (globals, mut event_queue) = registry_queue_init(&conn).expect("failed to init queue");
	let qh = event_queue.handle();

	let registry_state = RegistryState::new(&globals);
	let output_state = OutputState::new(&globals, &qh);

	println!(
		"{:#?}",
		output_state.outputs().filter_map(|x| output_state.info(&x)).collect::<Vec<_>>()
	);
	let (size_x, size_y): (u32, u32) = (1920, 1080); // just a default, it'll get updated later

	let compositor = CompositorState::bind(&globals, &qh).expect("wl_compositor unavailable");
	let layer_shell = LayerShell::bind(&globals, &qh).expect("wlr_layer_shell unavailable");
	let shm = Shm::bind(&globals, &qh).expect("wl_shm unavailable");
	let surface = compositor.create_surface(&qh);

	let layer = layer_shell.create_layer_surface(
		&qh,
		surface,
		wlr_layer::Layer::Overlay,
		Some("wlneko"),
		None,
	);
	layer.set_anchor(Anchor::TOP | Anchor::LEFT);
	layer.set_size(size_x, size_y);
	layer.set_input_region(Some(Region::new(&compositor).unwrap().wl_region()));
	layer.set_keyboard_interactivity(wlr_layer::KeyboardInteractivity::None);
	layer.commit();

	let pool = SlotPool::new(size_x as usize * size_y as usize * 4, &shm)
		.expect("compositor failed to provide slot pool");

	let mut client_state = ClientState {
		registry_state,
		output_state,
		shm,
		exit: false,
		first_configure: true,
		pool,
		width: size_x,
		height: size_y,
		layer,
	};

	let (watcher_tx, watcher_rx) = std::sync::mpsc::channel();
	{
		let watcher_tx = watcher_tx.clone();
		thread::spawn(move || {
			let cwd = env::current_dir().expect("cwd is borked");
			let mut watcher = INotifyWatcher::new(watcher_tx, Config::default()).unwrap();
			watcher.watch(&cwd, notify::RecursiveMode::NonRecursive).unwrap();

			for res in watcher_rx {
				match res.unwrap_or_default() {
					notify::Event {
						kind:
							notify::EventKind::Access(notify::event::AccessKind::Close(
								notify::event::AccessMode::Write,
							))
							| notify::EventKind::Create(notify::event::CreateKind::File),
						paths,
						..
					} if paths.len() == 1 && paths[0] == cwd.join("neko.png") => skin_cat(),
					notify::Event {
						kind: notify::EventKind::Other,
						paths: _,
						attrs,
					} if attrs.info() == Some("WLNEKO_KILL_YOURSELF_NOW") => return,
					_ => {}
				};
			}
		});
	}

	yap!("init finished; entering client event loop");
	while !client_state.exit {
		if event_queue.blocking_dispatch(&mut client_state).is_err() {
			yap!("error encountered on client event loop; exiting");
			watcher_tx
				.send(Ok(notify::Event::new(notify::EventKind::Other)
					.set_info("WLNEKO_KILL_YOURSELF_NOW")))
				.expect("watcher thread didn't receive kill message, maybe it already died?");
			client_state.exit = true
		};
	}
}
