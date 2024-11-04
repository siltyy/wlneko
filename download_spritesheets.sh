#!/bin/sh

mkdir -p assets
curl -s https://bomvel.neocities.org/neko/ \
  | grep -E 'src="./sheets/.*?"' \
  | sed -E 's#.*src="./sheets/([[:print:]]+?\.png)".*#https://bomvel.neocities.org/neko/sheets/\1#g' \
  | while read -r i ;do curl -O "$i" --output-dir assets & done
