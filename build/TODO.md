# Next improvements

Ordered by value for the effort. Context: [README.md](README.md), monitoring in [monitor/](monitor/).

## Monitoring
- [ ] **Alerts.** The dashboard only shows trouble when someone looks. Run `build/check.sh` from cron
      (or `/loop`) on the laptop and notify on failure, and/or add an external pinger. Also let
      `ftlm-collect` itself notify (mail/ntfy/webhook) when `ok` flips to false.
- [ ] **Restart dead services.** OpenRC does not supervise: a JVM that dies stays dead. Try
      `supervisor=supervise-daemon` in the init scripts, but first check that it is compatible with
      `command_background="yes"` (which must stay, see the comment in the init scripts).
- [ ] **OOM visibility.** 1 GB RAM, several JVMs. Record `dmesg | grep -i oom` / restart counts per
      service in the collector and show them on the dashboard.
- [ ] **Per-site viewers on the dashboard.** The collector already splits main / vehicles / beatperm
      (new log format, `host=`); the dashboard does not show it yet. Old log lines have no host.
- [ ] **Per-IP analysis follow-ups** (see "Visitor analysis" below).

## Server hygiene
- [ ] **logrotate for nginx.** `access.log` (~700 MB) and `error.log` (~400 MB) have never been rotated
      (the file in `/etc/logrotate.d/nginx` exists but nothing runs it). Install logrotate + cron, weekly,
      keep 4, `copytruncate` is not needed (`postrotate` reopens). The collector handles rotation.
- [ ] **Silence the error.log.** It is mostly scanner noise (`open() ".../x.php" failed`). Consider
      `error_log ... crit` or filtering, so real errors stand out.
- [ ] **Deploy safety.** Backup + restore for init scripts exists only as `.bak`; add a restore on failed
      restart. A `--dry-run`. Verify `command_background="yes"` is present before pushing an init script.
- [ ] **fail2ban or nginx rules** for the scanners (about 1,000 IPs/month probe for `.php`, `.env`, ...).
      They cost bandwidth and log space, not much else.

## Open decisions
- **What does the dashboard's "Viewers" tile mean?** Today: non-bot UA + successful page GET + no exploit
  probing (~6,200/month). Probably ~half of that is scrapers. Options: (a) keep as is, (b) switch the
  headline to browser-verified, (c) browser-verified from non-hosting networks (needs geo/ASN in the
  collector, see below). Leaning (c).

## Visitor analysis (who is on the site?)
First results (Oct 2026, 400k log lines, 4 weeks), reproduce with `monitor/analyze.py`:
- ~13,300 distinct IPs; ~1,000 are scanners; ~4,100 only ever use bot/tool user agents.
- ~6,200 look like viewers (browser UA, successful page GET), but ~3,900 of them fetched exactly one HTML
  page and **no css/js/images** and have no referer: very likely scrapers with a spoofed browser UA.
- ~1,300 are browser-verified (fetched assets). That is the safe lower bound for humans; the truth is
  between that and the 6,200 (a returning visitor with a warm cache fetches no assets).
- Embedded vehicles: ~450 `/art/...` loads carried a main-site referer (the iframes). The collector now
  does not count those as vehicles viewers.

To do:
- [x] **Geo / network (ASN)**, offline, in `monitor/analyze.py`: free DB-IP Lite country + ASN databases
      (`monitor/geo-update.sh`, no account, CC BY 4.0, lookups run locally, IPs never leave the machine).
      Result (Oct 2026): ~56% of all IPs and ~51% of "viewers" come from hosting/cloud/scanner networks
      (Tencent alone ~1,100 viewer IPs). Without those: ~3,000 viewers, of which ~490 browser-verified
      (US, CN, BR, DE ...). Even the verified ones include many China Unicom/Telecom IPs: residential
      proxies or botnets can not be told apart by ASN. A realistic "people" number is a few hundred a
      month, not 6,000.
- [ ] **Geo/ASN in the collector.** So the dashboard can show country and "hosting vs. residential".
      Needs a stdlib-only `.mmdb` reader (the box has no pip packages and 1 GB RAM) or a
      `maxminddb` install on the box, and a monthly `geo-update.sh` cron there. Store the class
      (country, hosting yes/no) per IP hash, not the ASN name.
- [ ] **Tighten the hosting/scanner list** in `analyze.py` (`HOSTING`): it is a hand-made regex on ASN
      names. Review the top networks after each run.
- [ ] **Referrers.** Entry sources: mostly direct/none; clojure.in is the one real referrer. Track top
      referrers per day on the dashboard.
- [ ] **Returning vs. new** (hash is stable for 30 days, so this is possible) and **pages per visit**.
- [ ] **Time-of-day / time zone** pattern as a proxy for where readers live (the log is UTC).
- [ ] **Which posts matter**: page views by verified viewers, per post, per week.
- [ ] **Improve the human estimate.** Candidate signals: loads css, loads favicon, has a plausible
      `Accept-Language` (needs a log format change), spends >1 request, sane inter-request time.
- [ ] Cache headers: if css/js are cached hard, verified viewers are undercounted. Check `Cache-Control`.
- [ ] **Unusual hits to look at**: 105 verified viewers on `/reflector-app-privacy-policy.html` (an app
      store link?), and 635 verified IPs that fetched assets but no page (hotlinked images? iframes
      without referer?).
