#!/bin/sh

if command -v curl >/dev/null 2>&1; then
	alias fetch='curl -s'
	alias dl='curl -O'
else 
	alias fetch='ftp -o-'
	alias dl='ftp'
fi

CWD="$PWD"
mkdir -p assets && cd assets
fetch https://bomvel.neocities.org/neko/ \
  | grep -E 'src="./sheets/.*"' \
  | sed -E 's#.*src="./sheets/([[:print:]]+\.png)".*#https://bomvel.neocities.org/neko/sheets/\1#g' \
  | while read -r i ;do dl "$i" & done

cd "$CWD"
