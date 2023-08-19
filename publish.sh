#!/bin/sh

rsync -avz public/* linode:/var/www/ftlm/

# scp -r public/*.cljs linode:/var/www/ftlm/
