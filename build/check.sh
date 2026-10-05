#!/bin/sh
# External health probe: what a visitor sees, from this machine (no ssh).
# Exit 0 = all fine, 1 = something failed. Fine for cron/loop: build/check.sh || notify-send ...
# The on-box checks and the dashboard live in monitor/ (https://faster-than-light-memes.xyz/admin/).
set -u

fail=0
check() { # name url expected_status
	code=$(curl -s -o /dev/null -m 15 -w '%{http_code}' "$2" || true)
	if [ "$code" = "$3" ]; then printf 'ok    %-9s %s (%s)\n' "$1" "$2" "$code"
	else printf 'FAIL  %-9s %s (got %s, want %s)\n' "$1" "$2" "$code" "$3"; fail=1; fi
}
cert() { # host min_days
	end=$(echo | openssl s_client -connect "$1:443" -servername "$1" 2>/dev/null | openssl x509 -noout -enddate 2>/dev/null | cut -d= -f2)
	[ -n "$end" ] || { printf 'FAIL  cert      %s (no certificate)\n' "$1"; fail=1; return; }
	days=$(( ($(date -d "$end" +%s) - $(date +%s)) / 86400 ))
	if [ "$days" -ge "$2" ]; then printf 'ok    cert      %s (%s days left)\n' "$1" "$days"
	else printf 'FAIL  cert      %s (%s days left, want >= %s)\n' "$1" "$days" "$2"; fail=1; fi
}

check site     https://faster-than-light-memes.xyz/ 200
check vehicles "https://vehicles.faster-than-light-memes.xyz/art/g/getting-around?page=0" 200
check admin    https://faster-than-light-memes.xyz/admin/ 401   # 401 = routed + basic auth is on
cert faster-than-light-memes.xyz 14
cert vehicles.faster-than-light-memes.xyz 14
exit $fail
