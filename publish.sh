#!/bin/sh

find public/ -type f -not -iname "*~" | xargs -I {} scp {} linode:/var/www/ftlm/
