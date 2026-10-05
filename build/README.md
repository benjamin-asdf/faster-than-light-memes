# Server config (linode)

Config for the Alpine Linux linode (`ssh linode`, 170.187.190.198) that hosts
faster-than-light-memes.xyz. These files are the source of truth: edit them here and
push them with `deploy.sh`. Don't edit them on the box.

| File                          | Installed as                        |
|-------------------------------|-------------------------------------|
| `nginx/nginx.conf`            | `/etc/nginx/nginx.conf`             |
| `openrc/ftlm-search-server`   | `/etc/init.d/ftlm-search-server`    |
| `openrc/ftlm-vehicles-server` | `/etc/init.d/ftlm-vehicles-server`  |
| `openrc/lachliste`            | `/etc/init.d/lachliste`             |
| `openrc/geomaxing`            | `/etc/init.d/geomaxing`             |
| `monitor/collect.py`          | `/usr/local/bin/ftlm-collect` (cron, every minute) |
| `monitor/index.html`          | `/var/www/ftlm-status/index.html`   |

```sh
build/deploy.sh                    # push everything that differs from the box
build/deploy.sh diff               # read-only: show drift between repo and box
build/deploy.sh diff vehicles      # one target
build/deploy.sh nginx              # nginx -t, reload (restores the old file if the test fails)
build/deploy.sh search             # install init script, add to runlevel, restart
build/deploy.sh vehicles
build/deploy.sh lachliste          # also writes /etc/conf.d/lachliste (see below)
build/deploy.sh geomaxing
build/deploy.sh monitor           # collector + dashboard + cron entry (first run backfills the stats)
build/check.sh                     # external probe from this machine: site, vehicles, /admin/, TLS expiry
FORCE=1 build/deploy.sh vehicles   # push + restart even if unchanged
```

`push` compares each file with the box first and skips (no restart) what is identical. Safety nets:
nginx keeps the newest 5 `nginx.conf.bak-*` and rolls back on a failed `nginx -t`; an overwritten init
script is kept as `/etc/init.d/<svc>.bak`. `diff` never prints `/etc/conf.d/*` contents (secrets), only
whether they match.

The static site itself goes up with `../publish.sh` (rsync of `public/` to `/var/www/ftlm/`).

## Services

| Service                | Port | Dir on server           | Public URL                                  |
|------------------------|------|-------------------------|---------------------------------------------|
| static site            | –    | `/var/www/ftlm/`        | https://faster-than-light-memes.xyz         |
| `ftlm-search-server`   | 8094 | `/var/www/ftlm-search`  | `/search` (POST, rate limited to 2 r/s)     |
| `ftlm-vehicles-server` | 8095 | `/var/www/ftlm-vehicles`| https://vehicles.faster-than-light-memes.xyz|
| summerfest             | –    | Hetzner box 49.12.218.29| `/summerfest` (proxied over pinned TLS)     |
| `lachliste` (bb)       | 8098 | `/opt/lachliste`        | `/janoshi/lachliste` (basic auth)           |
| `geomaxing` (bb)       | 8099 | `/opt/geomaxing`        | `/janoshi/geomaxing` (basic auth)           |
| beatperm (static)      | –    | `/var/www/beatperm`     | https://beatperm.benjamin-schwerdtner.de    |

Each JVM app dir has a `release.jar` symlink pointing at a jar in `target/`. To roll out a
new build, copy the jar into `target/`, repoint the symlink, and run
`rc-service <svc> restart`.

The init scripts call `java` directly with an explicit `-Xmx`. The box has only 1 GB RAM
and 512 MB swap, so size heaps to fit that. The `run.sh` in each app dir is only for
starting an app by hand.

### Janoshi apps (lachliste, geomaxing)

These are babashka apps whose code lives in the `game` repo. `game/scripts/<app>/deploy.sh`
syncs `/opt/<app>/code` and restarts the app. Its `remote/start.sh` and `stop.sh` now just
call `rc-service <app> start|stop`, so OpenRC stays in charge of the process.

Their env (PORT, DB_*) lives in `/etc/conf.d/<app>` (mode 600). `build/deploy.sh <app>`
generates that file from `game/scripts/<app>/deploy.env`. The DB password is in there, so
it never goes into this repo. After changing `deploy.env`, rerun `build/deploy.sh <app>`.
Logs: `/opt/<app>/log/app.log`.

hearts-server was removed on 2026-09-30, along with its init script and `/var/www/ftlm-hearts`.

Logs: `/var/log/<service>.log` / `.err`, `/var/log/nginx/error.log`.
TLS: certbot cron twice a day (`crontab -l`), cert `/etc/letsencrypt/live/benjamin-schwerdtner.de`.
Not in the repo on purpose: `/etc/nginx/.htpasswd_janoshi`, `/etc/nginx/summerfest-origin.crt`.

## Incident 2026-09-30: SSH dead after reboot

After a reboot, port 22 refused connections, LISH showed no login prompt, and nginx still
served the site.

Cause: the old `ftlm-search-server` init script ran `run.sh` in the foreground. OpenRC
starts the `default` runlevel one service at a time and waited on it forever. Every service
after it never started (sshd, and `ftlm-vehicles-server`, which also wasn't in the
runlevel), and neither did the gettys in `/etc/inittab`, which only run after
`openrc default` returns.

Rule: every init script here needs `command_background="yes"` plus a `pidfile`, and no
custom `start()` that runs the app itself.

Recovery if it happens again:

1. Cloud Manager → Linode → **Rescue**, with the main disk attached as `/dev/sda`, then open the LISH console.
2. `mount /dev/sda /mnt`, then fix `/mnt/etc/init.d/<svc>`, or take it out of the boot with
   `rm /mnt/etc/runlevels/default/<svc>`.
3. `umount /mnt && reboot`.

`[linode…@…-fra1 lish]#` is LISH's host shell, not the VM. Normal shell commands like
`ls` don't work there.

## Monitoring

`monitor/collect.py` runs on the box every minute (cron, stdlib Python) and writes three JSON files
for the dashboard into `/var/www/ftlm-status/`; its state is in `/var/lib/ftlm-monitor/` (mode 700).

- **Dashboard:** https://faster-than-light-memes.xyz/admin/ (basic auth with its own `/etc/nginx/.htpasswd_admin`,
  not the `/janoshi/` one; `deploy.sh monitor` creates it on first run and asks for the password, or reads
  `ADMIN_USER`/`ADMIN_PASSWORD`; the password never goes into the repo. To change it, delete the file on the
  box and redeploy).
- **Health:** nginx, vehicles, search, lachliste, geomaxing, postgresql (port/HTTP checks with latency),
  JVM RSS, memory/swap, disk, load, TLS expiry. The banner turns red on any failed check, low memory,
  disk > 90 %, cert < 7 days, or a collector that stopped reporting for 3 minutes.
- **Viewers:** distinct IPs from the nginx `access.log` with at least one successful page GET (2xx/304)
  and a non-bot user agent; "all IPs" (mostly scanners) is shown next to it. IPs are only stored as
  salted hashes (`/var/lib/ftlm-monitor/salt`) and dropped after 30 days. Needs the `ftlm` log format
  in `nginx.conf` for the per-site split (older log lines have no vhost).
- **Iframes:** the vehicles server is embedded in the main site. Requests to it with a main-site `Referer`
  are not counted as vehicles viewers.
- **Three counts:** all IPs, viewers, and browser-verified viewers (they also fetched css/js/images).
  Scanner IPs (a 4xx on `.php`, `.env`, ...) are excluded from viewers.
- **`monitor/analyze.py <access.log>`** prints a one-off visitor profile (aggregates only, no IPs). With `monitor/geo-update.sh` (free DB-IP
  Lite databases, no account) it also shows country and network type; run it with
  `uv run --with maxminddb build/monitor/analyze.py <log>`. IP Geolocation by [DB-IP](https://db-ip.com), CC BY 4.0.
- **`build/check.sh`** looks from outside (what a visitor sees). It does not need ssh.
- The collector does not alert by itself. Run `build/check.sh` from cron/`/loop`, or point an external
  pinger at the site.

`/var/log/nginx/access.log` and `error.log` are not rotated (700 MB and 400 MB in Oct 2026). The
collector copes with rotation (inode/size check), so adding logrotate later is safe.

Next steps are in [TODO.md](TODO.md).
