# lan-repo

The pacman repo served over LAN, available as `http://raspberrypi:8080/` to all boxes on the LAN (flowerpower is the main consumer).

## Paths

| Path | Role |
|---|---|
| `/home/jan/.local/arch-repo` | Served repo on the Pi (repo-server.service, python http.server, port 8080) |
| `/home/jan/builds/repo/` | Mirror of the served repo on the fleet build host `cyberkleiber`; source of the `rsync --delete` sync |
| `/srv/repo` | ~~obsolete~~ — was the original (root-owned) repo location; **removed**, all refs gone |

## Packages

* Only what is on disk belongs in the db — a stale db (entries for packages that are not on disk) makes pacman fail when it tries to download missing files.
* `hylki-bin` — the package name the build publishes (AUR PKGBUILD's `pkgname=` is overridden from `hylki` → `hylki-bin`). No artifact exists until the build on cyberkleiber completes; then `build-hylki.sh` + `repo-add` put it in `/home/jan/builds/repo/` and rsync to the Pi.
* `rencal-bin-0.9.1-1-x86_64.pkg.tar.zst` — kept in the repo (it's a valid lan-repo artifact; not an AUR install).
* After a rebuild: `hylki-bin-<ver>` + `rencal-bin-0.9.1-1-x86_64.pkg.tar.zst`, db lists both.

## Build → publish flow (`build-hylki.sh`)

`/home/jan/.local/bin/build-hylki.sh` (on cyberkleiber) → clones AUR `hylki`, sets `pkgname=(hylki-bin)`, builds in `archlinux:base-devel` container, `repo-add` generates `/home/jan/builds/repo/repo.db.tar.gz`, rsync `--delete` to `jan@raspberrypi:/home/jan/.local/arch-repo/`.

* `--repo-only`: skip the build (only repo-add + rsync); only meaningful if the package exists on the build host.
* The db must already be valid when running `--repo-only`; it appends to the existing db.

## Maintenance notes

* Regenerate db: run in an Arch container (`docker run --rm -v <repo>:/repo archlinux:base-devel bash -c "repo-add /repo/repo.db.tar.gz <pkg>"`) — the Pi is arm64, no `archlinux:base-devel` image available for it, so generate on cyberkleiber and copy back (`scp`).
* The two repo copies must stay in sync — verify with `md5sum` on both `repo.db.tar.gz` / `repo.files.tar.gz`.
* `/srv/repo` is gone; no reference remains in the chezmoi repo.
