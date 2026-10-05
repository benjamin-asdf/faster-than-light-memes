#!/bin/sh
# Download the free DB-IP "Lite" country + ASN databases (no account needed) into geo/.
# License: CC BY 4.0, attribution "IP Geolocation by DB-IP" (https://db-ip.com). Updated monthly.
set -eu
cd "$(dirname "$0")"
mkdir -p geo
for k in country asn; do
	for m in "$(date +%Y-%m)" "$(date -d '-1 month' +%Y-%m)"; do
		if curl -fsS -m 300 "https://download.db-ip.com/free/dbip-$k-lite-$m.mmdb.gz" | gunzip > "geo/$k.mmdb.tmp"; then
			mv "geo/$k.mmdb.tmp" "geo/$k.mmdb"; echo "geo/$k.mmdb ($m)"; break
		fi
	done
done
