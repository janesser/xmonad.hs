# lan-repo: rebuild trigger (Option 1 — systemd timer on cyberkleiber)

## Goal

Rebuild and re-upload a lan-repo package automatically whenever upstream/`AUR` moves to a newer PKGBUILD version, without the user noticing it — and keep the lan-repo's package list consistent.

## What triggers a rebuild

Poll **every element of `packages.list`** against one feed **daily** on `cyberkleiber` (timer at 16:00):

```
https://aur.archlinux.org/cgit/aur.git/atom/?h=PACKAGENAME
```

- The last entry is `chore: update to <VERSION>` (or the AUR equivalent) — this is exactly the version a rebuild would produce next.
- Compare it with the `pkgver` of the local PKGBUILD clone (`/home/jan/builds/<pkg>/PKGBUILD`, or a stored marker per element).
- If newer → run `~/.local/bin/build-lan-repo.sh`.

(Why AUR cgit and not GitHub releases? The GitHub feed fires earlier for `-beta` releases, which are **skipped** — no betas allowed. The AUR commit is the signal that matches what actually gets built.)

**Skip pre-releases:** a policy rule: only build versions that are not `-beta`/`-rc`/`-dev`.

## `packages.list` — in chezmoi, the authority

`packages.list` lives in chezmoi and is rendered to *every* arch box in the fleet (flowerpower, chuwi, cyberkleiber…). It declares what the lan-repo **should serve** — the target state:

```
# dot_local/build-lan-repo/packages.list (chezmoi source)
package-a
package-b
```

- Editable from any fleet box (`cz edit` + `cz apply`, or `cz add` a new file) — no sudo, no rsync drift.
- **Every element of `packages.list` is treated uniformly** — there is no special package. Each entry follows the same lifecycle:

### The uniform lifecycle for every list element

1. **Appear on `packages.list`** → the build loop clones its PKGBUILD from the AUR, builds it (docker/podman, memory-capped), `repo-add`s it into `/home/jan/builds/repo/`, and syncs to the Pi (`rsync --delete`).
2. **Upgrade when available** → the daily check at 16:00 polls every element of `packages.list` against the AUR; if an element has a newer PKGBUILD, the loop rebuilds it with the new version, re-uploads, and updates the db.
3. **Disappear from `packages.list`** → the next run removes the artifact from `/home/jan/builds/repo/`, regenerates the db (only what remains in `packages.list`), and syncs — so it vanishes from the lan-repo too.

- **No version pins**: latest-stable AUR PKGBUILD at build time, every time.
- **The guarantee**: after any run, **lan-repo = exactly `packages.list` + latest versions**. Nothing else survives; nothing listed is missing.

### Reconciliation (part of every run)

1. Read `packages.list` — the *should-be* set.
2. For each element: clone → build → `repo-add` (or keep existing if up to date).
3. Remove elements no longer in `packages.list` → regenerate db.
4. Verify: repo listing matches `packages.list` exactly.
5. Sync to the Pi via `rsync --delete` — the db follows the packages automatically.

That makes the target state deterministic and uniform: **list in, packages out, upgrades on, removals clean.**

## The timer

On `cyberkleiber` (16:00 daily):

```
/etc/systemd/system/lan-repo-rebuild.timer    # daily 16:00
/etc/systemd/system/lan-repo-rebuild.service  # the polling + rebuild
```

`lan-repo-rebuild.service` runs a script like:

```bash
#!/bin/bash
# lan-repo-rebuild.sh — poll AUR feeds, build anything newer than local
SKIPPATTERN='beta|rc|dev'   # policy: no pre-releases
LIST=/home/jan/.local/build-lan-repo/packages.list   # chezmoi-rendered

while read -r pkg; do
    feed="https://aur.archlinux.org/cgit/aur.git/atom/?h=$pkg"
    new_ver="$(curl -s "$feed" | grep -m1 'update to ' | grep -oE '[0-9]+(\.[0-9]+)+')"
    [ -z "$new_ver" ] && continue
    echo "$new_ver" | grep -qiE "$SKIPPATTERN" && { echo "-> $pkg: pre-release, skipping"; continue; }
    # local marker (per package) tells what we last built
    marker="/home/jan/builds/$pkg/.last-built-pkgver"
    old_ver="$(cat "$marker" 2>/dev/null)"
    [ -z "$old_ver" ] && old_ver="0"
    [ "$new_ver" = "$old_ver" ] && continue
    echo "-> $pkg: $new_ver is newer than $old_ver, building"
    /home/jan/.local/bin/build-lan-repo.sh "$pkg" || echo "-> $pkg: failed"
    echo "$new_ver" > "$marker"
done < "$LIST"
```

## Guards / pitfalls

- **`--repo-only`** skips the container build when the artifact exists (fast). It relies on a valid db — so the marker check also keeps the db in sync by running `repo-add` each time.
- **Don't delete stale `.old` dbs** from `/home/jan/builds/repo/` — `repo-add` regenerates them fresh; leaving them creates duplicate entries (the Pi serves whatever it gets).
- **`rsync --delete`** copies the *whole* build-host repo to the Pi. Only what exists on cyberkleiber survives there → `packages.list` is your only input, nothing else can survive a run.
- **Pi is arm64** — no Arch image available for local `repo-add`; everything happens on cyberkleiber (the db gets synced over).
- **Failure logging**: systemd `notify`, journal logs, or a visible marker file — if the rebuild fails silently you have no idea it failed.
- **Marker written last** — after verification passes. A stale/absent marker should trigger a rebuild (it's evidence of an incomplete previous run).

## Verification

After any run:
```
md5sum /home/jan/builds/repo/repo.db.tar.gz /home/jan/.local/arch-repo/repo.db.tar.gz   # identical
curl -sS http://127.0.0.1:8080/    # lists the packages = packages.list + latest versions
```
