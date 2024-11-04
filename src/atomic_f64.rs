use std::sync::atomic::{AtomicU64, Ordering};

pub(crate) struct AtomicF64 {
	storage: AtomicU64,
}

impl AtomicF64 {
	pub(crate) const fn new(value: f64) -> Self {
		let as_u64 = value.to_bits();
		Self {
			storage: AtomicU64::new(as_u64),
		}
	}
	pub(crate) fn store(&self, value: f64, ordering: Ordering) {
		let as_u64 = value.to_bits();
		self.storage.store(as_u64, ordering)
	}
	pub(crate) fn load(&self, ordering: Ordering) -> f64 {
		let as_u64 = self.storage.load(ordering);
		f64::from_bits(as_u64)
	}
}
