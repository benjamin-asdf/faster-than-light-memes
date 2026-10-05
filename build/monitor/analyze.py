#!/usr/bin/env python3
"""Who are the visitors? One-off profile of an nginx access.log. Prints aggregates only, never IPs.

  ssh linode 'tail -n 400000 /var/log/nginx/access.log' > /tmp/access.log
  build/monitor/geo-update.sh                        # once: free DB-IP country + ASN databases
  uv run --with maxminddb build/monitor/analyze.py /tmp/access.log

Same rules as collect.py (bots by UA, scanners by probing paths, iframe loads excluded), plus
a profile of the remaining "viewers": OS/browser, pages per viewer, where they came from,
and how many of them look like a real browser (fetched css/js/images). With the geo databases
(geo-update.sh) it also shows country and network (ASN): visitors from datacenters/clouds/VPNs
are rarely people. The lookups run locally, IPs never leave this machine.
IP Geolocation by DB-IP (https://db-ip.com), CC BY 4.0.
"""
import collections
import re
import sys
import urllib.parse

from collect import ASSET, BOT, LINE, MAIN_HOSTS, REF_HOST, SCAN

path = sys.argv[1] if len(sys.argv) > 1 else "/var/log/nginx/access.log"
by = collections.defaultdict(list)
for line in open(path, errors="replace"):
    m = LINE.match(line)
    if m:
        ip, ts, req, status, nb, ref, ua, host = m.groups()
        parts = req.split(" ")
        by[ip].append((parts[0], parts[1] if len(parts) > 1 else "", int(status), ref, ua, host))


def ok(r):
    return r[0] == "GET" and (200 <= r[2] < 300 or r[2] == 304) and not BOT.search(r[4])


def embedded(r):
    rm = REF_HOST.match(r[3])
    return bool(rm) and rm.group(1).lower() in MAIN_HOSTS and (
        (r[5] or "").startswith("vehicles.") or (not r[5] and r[1].startswith("/art/")))


def is_page(r):
    return r[1].split("?")[0].endswith(("/", ".html"))


def source(ref):
    h = urllib.parse.urlparse(ref).netloc.lower()
    if not h:
        return "direct / none"
    if any(k in h for k in ("google", "bing", "duckduckgo", "ecosia", "yandex", "baidu", "brave", "startpage")):
        return "search engine"
    if any(k in h for k in ("faster-than-light", "benjamin-schwerdtner", "170.187", "linode")):
        return "own sites"
    return "other: " + h


def device(ua):
    os_ = ("iOS" if re.search("iPhone|iPad", ua) else "Android" if "Android" in ua else "Windows" if "Windows" in ua
           else "macOS" if "Macintosh" in ua else "Linux" if "Linux" in ua else "other")
    br = ("Edge" if "Edg/" in ua else "Firefox" if "Firefox" in ua else "Chrome" if "Chrome" in ua
          else "Safari" if "Safari" in ua else "other")
    return os_, br


all_ips = len(by)
scanners = {ip for ip, rs in by.items() if any(r[2] // 100 == 4 and SCAN.search(r[1]) for r in rs)}
viewers = {ip for ip, rs in by.items() if any(ok(r) and not embedded(r) for r in rs)} - scanners
verified = {ip for ip in viewers if any(ok(r) and ASSET.search(r[1]) for r in by[ip])}
print(f"distinct IPs {all_ips} | scanners {len(scanners)} | viewers {len(viewers)} | browser-verified {len(verified)}")
bots_ua = {ip for ip, rs in by.items() if all(BOT.search(r[4]) for r in rs)}
print(f"IPs whose every request has a bot/tool user agent: {len(bots_ua)}")

for name, group in (("viewers", viewers), ("browser-verified", verified)):
    print(f"\n== {name} ({len(group)}) ==")
    dev = collections.Counter()
    pages = collections.Counter()
    entry = collections.Counter()
    for ip in group:
        rs = [r for r in by[ip] if ok(r) and not embedded(r)]
        dev[device(rs[0][4])] += 1
        pg = [r for r in rs if is_page(r)]
        pages[min(len({r[1].split("?")[0] for r in pg}), 5)] += 1
        if pg:
            entry[source(pg[0][3])] += 1
    print("os/browser:", ", ".join(f"{o}/{b} {n}" for (o, b), n in dev.most_common(6)))
    print("distinct pages per viewer (5 = 5+):", dict(sorted(pages.items())))
    print("entry:", ", ".join(f"{k} {n}" for k, n in entry.most_common(8)))

hit = collections.Counter(r[1].split("?")[0] for ip in verified for r in by[ip] if ok(r) and is_page(r) and not embedded(r))
print("\ntop pages of browser-verified viewers:")
for p, n in hit.most_common(10):
    print(f"  {n:5d} {p}")


# ---------------------------------------------------------------- geo / network
HOSTING = re.compile(
    r"amazon|aws|google|microsoft|azure|digitalocean|hetzner|ovh|linode|akamai|oracle|alibaba|tencent|"
    r"vultr|contabo|leaseweb|choopa|m247|datacamp|cloudflare|hosting|server|cloud|vps|colo|datacenter|"
    r"data center|host|vpn|proxy|zenlayer|ucloud|huawei|scaleway|ionos|1&1|netcup|psychz|cogent|"
    r"hurricane|quadranet|constant|packet|equinix|rackspace|ipxo|tzulo|bytedance|stark|sharktech|"
    # internet-wide scanners / "security research" crawlers, they show up as browser-verified otherwise
    r"driftnet|onyphe|hydra comm|internet vikings|censys|shodan|palo alto|stretchoid|recyber|"
    r"alphastrike|bitsight|leakix|criminal ip|shadowserver|binaryedge|cybergreen|netcologne|"
    r"hostpapa|hostroyale|datacamp",
    re.I,
)
try:
    import maxminddb
    asn_db = maxminddb.open_database(__file__.rsplit("/", 1)[0] + "/geo/asn.mmdb")
    cc_db = maxminddb.open_database(__file__.rsplit("/", 1)[0] + "/geo/country.mmdb")
except Exception as e:  # missing package or databases: stay silent but say why
    print(f"\n(no geo section: {e}; run geo-update.sh and use `uv run --with maxminddb`)")
    sys.exit(0)


def where(ip):
    try:
        org = (asn_db.get(ip) or {}).get("autonomous_system_organization") or "?"
        cc = ((cc_db.get(ip) or {}).get("country") or {}).get("iso_code") or "?"
    except ValueError:
        return "?", "?"
    return cc, org


def one_page_no_asset(ip):
    rs = [r for r in by[ip] if ok(r) and not embedded(r)]
    return not any(ASSET.search(r[1]) for r in rs) and len({r[1].split("?")[0] for r in rs if is_page(r)}) <= 1


geo = {ip: where(ip) for ip in by}
host_flag = {ip: bool(HOSTING.search(geo[ip][1])) for ip in by}
groups = {
    "all IPs": set(by),
    "scanners": scanners,
    "viewers": viewers,
    "  viewers: 1 page, no assets": {ip for ip in viewers if one_page_no_asset(ip)},
    "  viewers: other": {ip for ip in viewers if not one_page_no_asset(ip)},
    "browser-verified": verified,
}
print("\n== network type (datacenter/cloud/VPN by ASN name) ==")
for name, g in groups.items():
    if g:
        h = sum(host_flag[ip] for ip in g)
        print(f"{name:32s} {len(g):6d} IPs, {100 * h / len(g):4.0f}% from hosting/cloud networks")

for name in ("viewers", "browser-verified"):
    g = groups[name]
    human = [ip for ip in g if not host_flag[ip]]
    print(f"\n== {name}, excluding hosting/cloud networks: {len(human)} ==")
    cc = collections.Counter(geo[ip][0] for ip in human)
    print("countries:", ", ".join(f"{k} {n}" for k, n in cc.most_common(12)))
    org = collections.Counter(geo[ip][1] for ip in human)
    print("networks:", "; ".join(f"{k} {n}" for k, n in org.most_common(8)))

print("\n== biggest hosting/cloud networks among viewers ==")
org = collections.Counter(geo[ip][1] for ip in viewers if host_flag[ip])
print("; ".join(f"{k} {n}" for k, n in org.most_common(10)))
