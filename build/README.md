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

```sh
build/deploy.sh            # everything
build/deploy.sh nginx      # nginx -t, reload (restores the old file if the test fails)
build/deploy.sh search     # install init script, add to runlevel, restart
build/deploy.sh vehicles
build/deploy.sh lachliste  # also writes /etc/conf.d/lachliste (see below)
build/deploy.sh geomaxing
```

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
