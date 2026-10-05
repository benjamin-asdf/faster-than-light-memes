#!/usr/bin/env python3
"""ftlm-collect: health checks + traffic stats for the linode. Runs from cron every minute.

Installed as /usr/local/bin/ftlm-collect. Writes JSON for the dashboard (index.html) into
OUT_DIR, keeps its state in STATE_DIR. Stdlib only, the box has 1 GB RAM.

  ftlm-collect              one run (cron)
  ftlm-collect --backfill   one-off: rescan the whole access.log (last KEEP_DAYS days)

Traffic: the nginx access.log is read incrementally (byte offset, survives rotation).
Visitor IPs are never stored in the clear, only a salted hash (salt: STATE_DIR/salt, mode 600),
and the per-day hash files are deleted after KEEP_DAYS days. Timestamps are taken as UTC.

Three counts of distinct IPs (see tally()):
  all IPs   everyone in the log, mostly scanners
  viewers   successful non-bot page GET, never probed for exploits; the vehicles server's
            iframe loads (Referer = main site) do not count
  verified  viewers that also fetched css/js/images, i.e. a real browser (lower bound for humans)
"""
import fcntl
import hashlib
import http.client
import json
import os
import re
import socket
import subprocess
import sys
import time
from datetime import datetime, timedelta, timezone

STATE_DIR = os.environ.get("FTLM_STATE", "/var/lib/ftlm-monitor")
OUT_DIR = os.environ.get("FTLM_OUT", "/var/www/ftlm-status")
ACCESS_LOG = os.environ.get("FTLM_ACCESS_LOG", "/var/log/nginx/access.log")
CERT = "/etc/letsencrypt/live/benjamin-schwerdtner.de/fullchain.pem"
KEEP_DAYS = 30
SERIES_HOURS = 48

# name -> (kind, target). kind: tcp (port), http (port, host, path, expected statuses)
CHECKS = [
    ("nginx", "http", (80, "faster-than-light-memes.xyz", "/", (200, 301))),
    ("vehicles", "http", (8095, "localhost", "/", (200,))),
    ("search", "tcp", 8094),
    ("lachliste", "tcp", 8098),
    ("geomaxing", "tcp", 8099),
    ("postgresql", "tcp", 5432),
]
# check name -> pidfile, for the RSS metric
PIDFILES = {
    "vehicles": "/run/ftlm-vehicles-server.pid",
    "search": "/run/ftlm-search-server.pid",
    "lachliste": "/run/lachliste.pid",
    "geomaxing": "/run/geomaxing.pid",
}

BOT = re.compile(
    r"bot|crawl|spider|slurp|curl|wget|python|go-http|scan|headless|facebookexternalhit|preview|"
    r"monitor|uptime|zgrab|masscan|nuclei|censys|expanse|okhttp|java/|libwww|httpclient|axios|"
    r"node-fetch|^-?$",
    re.I,
)
LINE = re.compile(
    r'^(\S+) \S+ \S+ \[([^\]]+)\] "([^"]*)" (\d{3}) (\d+|-) "([^"]*)" "([^"]*)"(?: "[^"]*")?(?: host=(\S+))?'
)
# Probing for vulnerabilities: an IP that gets a 4xx on one of these is a scanner, whatever its UA says.
SCAN = re.compile(
    r"\.php|\.env|wp-|\.git|phpmyadmin|\.aws|cgi-bin|xmlrpc|\.asp|config\.|\.sql|\.bak", re.I
)
ASSET = re.compile(r"\.(css|js|png|jpe?g|gif|svg|ico|woff2?|webp)(\?|$)", re.I)
MAIN_HOSTS = {
    "faster-than-light-memes.xyz", "www.faster-than-light-memes.xyz",
    "benjamin-schwerdtner.de", "www.benjamin-schwerdtner.de",
}
REF_HOST = re.compile(r"^https?://([^/:?#]+)", re.I)
MONTHS = "Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec".split()


def now():
    return int(time.time())


def read(path, default=None):
    try:
        with open(path) as f:
            return f.read()
    except OSError:
        return default


def write_atomic(path, text):
    tmp = path + ".tmp"
    with open(tmp, "w") as f:
        f.write(text)
    os.replace(tmp, path)


# ---------------------------------------------------------------- health

def check_tcp(port):
    t0 = time.time()
    try:
        with socket.create_connection(("127.0.0.1", port), timeout=3):
            return True, round((time.time() - t0) * 1000), "port %d open" % port
    except OSError as e:
        return False, None, "port %d: %s" % (port, e)


def check_http(port, host, path, ok_status):
    t0 = time.time()
    try:
        c = http.client.HTTPConnection("127.0.0.1", port, timeout=4)
        c.request("GET", path, headers={"Host": host, "User-Agent": "ftlm-collect"})
        r = c.getresponse()
        r.read(1024)
        c.close()
        ms = round((time.time() - t0) * 1000)
        return r.status in ok_status, ms, "HTTP %d" % r.status
    except (OSError, http.client.HTTPException) as e:
        return False, None, "port %d: %s" % (port, e)


def rss_mb(pidfile):
    try:
        pid = int(read(pidfile, "").strip())
        for line in open("/proc/%d/status" % pid):
            if line.startswith("VmRSS:"):
                return round(int(line.split()[1]) / 1024)
        # the pidfile may hold the supervisor; fall through
    except (ValueError, OSError):
        pass
    return None


def meminfo():
    m = {}
    for line in read("/proc/meminfo", "").splitlines():
        k, _, v = line.partition(":")
        m[k] = int(v.split()[0]) // 1024 if v.split() else 0
    return m


def cert_days():
    try:
        out = subprocess.run(
            ["openssl", "x509", "-enddate", "-noout", "-in", CERT],
            capture_output=True, text=True, timeout=5,
        ).stdout.strip()  # notAfter=Dec 31 23:59:59 2026 GMT
        end = datetime.strptime(out.split("=", 1)[1], "%b %d %H:%M:%S %Y %Z")
        return (end - datetime.utcnow()).days
    except (OSError, ValueError, IndexError, subprocess.SubprocessError):
        return None


def health():
    checks = []
    for name, kind, target in CHECKS:
        ok, ms, detail = check_tcp(target) if kind == "tcp" else check_http(*target)
        c = {"name": name, "ok": ok, "ms": ms, "detail": detail}
        if name in PIDFILES:
            c["rss_mb"] = rss_mb(PIDFILES[name])
        checks.append(c)
    mem = meminfo()
    st = os.statvfs("/")
    disk_pct = round(100 * (1 - st.f_bavail / st.f_blocks), 1)
    load1 = float(read("/proc/loadavg", "0").split()[0])
    uptime_h = round(float(read("/proc/uptime", "0").split()[0]) / 3600, 1)
    days = cert_days()
    host = {
        "load1": load1,
        "mem_avail_mb": mem.get("MemAvailable"),
        "mem_total_mb": mem.get("MemTotal"),
        "swap_used_mb": mem.get("SwapTotal", 0) - mem.get("SwapFree", 0),
        "disk_pct": disk_pct,
        "cert_days": days,
        "uptime_h": uptime_h,
    }
    host_ok = (
        (mem.get("MemAvailable") or 0) > 50
        and disk_pct < 90
        and (days is None or days > 7)
    )
    all_ok = host_ok and all(c["ok"] for c in checks)
    return {"t": now(), "ok": all_ok, "checks": checks, "host": host}


def update_series(h):
    path = os.path.join(STATE_DIR, "metrics.jsonl")
    row = {
        "t": h["t"],
        **{k: h["host"][k] for k in ("load1", "mem_avail_mb", "swap_used_mb", "disk_pct")},
        "svc": {
            c["name"]: {"up": int(c["ok"]), "ms": c["ms"], "rss": c.get("rss_mb")}
            for c in h["checks"]
        },
    }
    cutoff = h["t"] - SERIES_HOURS * 3600
    rows = []
    for line in read(path, "").splitlines():
        try:
            r = json.loads(line)
        except ValueError:
            continue
        if r["t"] >= cutoff:
            rows.append(r)
    rows.append(row)
    write_atomic(path, "".join(json.dumps(r, separators=(",", ":")) + "\n" for r in rows))
    return rows


# ---------------------------------------------------------------- traffic

def site_of(host):
    if not host:
        return "unknown"
    if host.startswith("vehicles."):
        return "vehicles"
    if host.startswith("beatperm."):
        return "beatperm"
    return "main"


def load_state():
    try:
        return json.loads(read(os.path.join(STATE_DIR, "state.json"), "{}"))
    except ValueError:
        return {}


def get_salt():
    p = os.path.join(STATE_DIR, "salt")
    s = read(p)
    if s is None:
        s = os.urandom(16).hex()
        fd = os.open(p, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
        with os.fdopen(fd, "w") as f:
            f.write(s)
    return s.strip()


def day_file(day):
    return os.path.join(STATE_DIR, "ips-%s.txt" % day)


def scan_log(state, backfill):
    """Read new access.log lines. Returns the number of lines consumed."""
    salt = get_salt()
    days_ok = {}
    today = datetime.now(timezone.utc).date()
    for i in range(KEEP_DAYS + 1):
        d = today - timedelta(days=i)
        days_ok["%02d/%s/%d" % (d.day, MONTHS[d.month - 1], d.year)] = d.isoformat()

    try:
        st = os.stat(ACCESS_LOG)
    except OSError:
        return 0
    inode, size = st.st_ino, st.st_size
    offset = state.get("offset")
    if backfill:
        offset = 0
        state["counts"] = {}
        for f in os.listdir(STATE_DIR):
            if f.startswith("ips-"):
                os.remove(os.path.join(STATE_DIR, f))
    elif offset is None:
        # first run: start at the end of the log, don't swallow 700 MB of history
        state.update(inode=inode, offset=size)
        return 0
    elif state.get("inode") != inode or size < offset:
        offset = 0  # rotated or truncated

    counts = state.setdefault("counts", {})
    seen = {}  # day -> set of "HH site hash flag" lines (loaded lazily)
    new = {}
    n = 0
    with open(ACCESS_LOG, "rb") as f:
        f.seek(offset)
        while True:
            raw = f.readline()
            if not raw or not raw.endswith(b"\n"):
                break  # EOF or a half-written line: pick it up next run
            offset += len(raw)
            n += 1
            m = LINE.match(raw.decode("utf-8", "replace"))
            if not m:
                continue
            ip, ts, req, status, nbytes, referer, ua, host = m.groups()
            day = days_ok.get(ts[:11])
            if day is None:
                continue
            hour = ts[12:14]
            site = site_of(host)
            parts = req.split(" ")
            method = parts[0]
            path = parts[1] if len(parts) > 1 else ""
            if path.startswith("/admin/"):
                continue  # the dashboard itself
            st_i = int(status)
            h = counts.setdefault(day, {}).setdefault(hour, {}).setdefault(
                site, {"req": 0, "2xx": 0, "3xx": 0, "4xx": 0, "5xx": 0, "bytes": 0}
            )
            h["req"] += 1
            h["%dxx" % (st_i // 100)] = h.get("%dxx" % (st_i // 100), 0) + 1
            h["bytes"] += int(nbytes) if nbytes.isdigit() else 0
            # The vehicles server is iframed into the main site: its requests carry a main-site
            # Referer. They are the same visitor's page view, not a vehicles viewer. (Old log lines
            # have no host=; there /art/... is the vehicles app.)
            rm = REF_HOST.match(referer)
            embedded = (
                rm is not None
                and rm.group(1).lower() in MAIN_HOSTS
                and (site == "vehicles" or (site == "unknown" and path.startswith("/art/")))
            )
            ok = method == "GET" and (200 <= st_i < 300 or st_i == 304) and not BOT.search(ua)
            if 400 <= st_i < 500 and SCAN.search(path):
                flag = "x"  # scanner
            elif embedded or not ok or path in ("/robots.txt", "/favicon.ico"):
                flag = "0"
            elif ASSET.search(path):
                flag = "2"  # a browser fetching css/js/images: evidence of a real browser
            else:
                flag = "1"  # page view
            ih = hashlib.sha256((salt + ip).encode()).hexdigest()[:12]
            rec = "%s %s %s %s" % (hour, site, ih, flag)
            if day not in seen:
                seen[day] = set(read(day_file(day), "").splitlines())
                new[day] = []
            if rec not in seen[day]:
                seen[day].add(rec)
                new[day].append(rec)
    for day, recs in new.items():
        if recs:
            with open(day_file(day), "a") as f:
                f.write("\n".join(recs) + "\n")
    state.update(inode=inode, offset=offset)
    return n


def prune(state):
    cutoff = (datetime.now(timezone.utc).date() - timedelta(days=KEEP_DAYS)).isoformat()
    for d in [d for d in state.get("counts", {}) if d < cutoff]:
        del state["counts"][d]
    for f in os.listdir(STATE_DIR):
        if f.startswith("ips-") and f[4:14] < cutoff:
            os.remove(os.path.join(STATE_DIR, f))


def tally(recs):
    """recs: [[hour, site, hash, flag]]. Returns the distinct-IP counts.

    ips      every distinct IP, scanners included
    viewers  made a successful, non-bot page GET (or fetched assets), never probed for exploits
    verified viewers that also fetched css/js/images: a real browser (a lower bound for humans,
             since a warm cache fetches nothing)
    """
    scanners = {r[2] for r in recs if r[3] == "x"}
    viewers = {r[2] for r in recs if r[3] in "12"} - scanners
    verified = {r[2] for r in recs if r[3] == "2"} - scanners
    return {
        "ips": len({r[2] for r in recs}), "viewers": len(viewers),
        "verified": len(verified), "scanners": len(scanners),
    }


def read_recs(day):
    return [l.split() for l in read(day_file(day), "").splitlines() if len(l.split()) == 4]


def traffic_json(state):
    counts = state.get("counts", {})
    today = datetime.now(timezone.utc).date()
    days, all_recs = [], []
    for i in range(KEEP_DAYS - 1, -1, -1):
        day = (today - timedelta(days=i)).isoformat()
        recs = read_recs(day)
        all_recs += recs
        sites = {}
        for site in {r[1] for r in recs}:
            sites[site] = tally([r for r in recs if r[1] == site])
        dc = [s for hr in counts.get(day, {}).values() for s in hr.values()]
        days.append({
            "day": day, "req": sum(s["req"] for s in dc),
            "4xx": sum(s["4xx"] for s in dc), "5xx": sum(s["5xx"] for s in dc),
            **tally(recs), "sites": sites,
        })
    hours = []
    t0 = datetime.now(timezone.utc).replace(minute=0, second=0, microsecond=0)
    cache = {}
    for i in range(SERIES_HOURS - 1, -1, -1):
        t = t0 - timedelta(hours=i)
        day, hh = t.date().isoformat(), "%02d" % t.hour
        if day not in cache:
            cache[day] = read_recs(day)
        hours.append({
            "t": int(t.timestamp()),
            "req": sum(s["req"] for s in counts.get(day, {}).get(hh, {}).values()),
            **tally([r for r in cache[day] if r[0] == hh]),
        })
    return {
        "t": now(), "days": days, "hours": hours,
        "window": {"days": KEEP_DAYS, **tally(all_recs)},
        "since": state.get("since"),
    }


# ---------------------------------------------------------------- main

def main():
    backfill = "--backfill" in sys.argv
    os.makedirs(STATE_DIR, mode=0o700, exist_ok=True)
    os.makedirs(OUT_DIR, exist_ok=True)

    # one run at a time (a --backfill can outlast the cron interval)
    lock = open(os.path.join(STATE_DIR, "lock"), "w")
    try:
        fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
    except OSError:
        return

    h = health()
    series = update_series(h)
    write_atomic(os.path.join(OUT_DIR, "status.json"), json.dumps(h))
    write_atomic(os.path.join(OUT_DIR, "series.json"), json.dumps(series, separators=(",", ":")))

    state = load_state()
    state.setdefault("since", datetime.now(timezone.utc).strftime("%Y-%m-%d"))
    n = scan_log(state, backfill)
    prune(state)
    write_atomic(os.path.join(STATE_DIR, "state.json"), json.dumps(state))
    write_atomic(os.path.join(OUT_DIR, "traffic.json"), json.dumps(traffic_json(state), separators=(",", ":")))
    if backfill:
        print("backfill: %d lines" % n)


if __name__ == "__main__":
    main()
