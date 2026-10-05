#!/bin/sh
# rsync public/ to the linode, unless nothing in public/ changed since the last deploy.
# FORCE=1 deploys anyway (e.g. after something changed on the box).
set -e
cd "$(dirname "$0")"
stamp=.build-stamps/deploy

# cmp first, so an unchanged file keeps its mtime and doesn't count as a change
src=/home/benj/repos/reflector/html/privacy-policy.html
dst=./public/reflector-app-privacy-policy.html
[ -f "$src" ] && { cmp -s "$src" "$dst" || cp -f "$src" "$dst"; }

if [ -z "${FORCE:-}" ] && [ -f "$stamp" ] && [ -z "$(find public -newer "$stamp" -print -quit)" ]; then
	echo "skip deploy (public/ unchanged since last deploy, FORCE=1 to run anyway)"
	exit 0
fi

# dreams/ is a private dream diary, kept out of publishing on purpose
rsync -avz --exclude 'dreams.html' --exclude 'dreams/' --exclude 'search-index/dreams' public/* linode:/var/www/ftlm/

mkdir -p .build-stamps && touch "$stamp"

# scp -r public/*.cljs linode:/var/www/ftlm/
