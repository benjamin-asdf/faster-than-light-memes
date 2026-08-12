#!/bin/sh


[ -f /home/benj/repos/reflector/html/privacy-policy.html ] && cp -f /home/benj/repos/reflector/html/privacy-policy.html ./public/reflector-app-privacy-policy.html

# dreams/ is a private dream diary, kept out of publishing on purpose
rsync -avz --exclude 'dreams.html' --exclude 'dreams/' --exclude 'search-index/dreams' public/* linode:/var/www/ftlm/

# scp -r public/*.cljs linode:/var/www/ftlm/
