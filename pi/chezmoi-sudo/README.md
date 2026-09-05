# pi-agent scoped sudo for `cz update`

Passwordless sudo, **scoped** to only the commands chezmoi's run scripts use.
Lets pi-agent run `cz update` while keeping root out of everything else.

## Install (requires your password once)
```sh
# 1. Install the drop-in
sudo install -o root -g root -m 0440 pi/chezmoi-sudo/chezmoi-pi \
        /etc/sudoers.d/chezmoi-pi

# 2. Validate syntax (must print "OK")
sudo visudo -cf /etc/sudoers.d/chezmoi-pi

# 3. Confirm what jan is allowed (should list the CHEZMOI_* aliases)
sudo -l

# 4. Test passwordless: this should NOT ask for a password
sudo -n true
```

## What is allowed (as `jan`)
`apt, add-apt-repository, nala, snap, usermod, groupadd, systemctl,
update-alternatives, mkdir, chmod, chown, tee, cp, gpg, mv, sed, extrepo,
dpkg, curl, rm` — plus `bash -c "chmod/chown ..."` for the podman.sock steps
and the ollama-uninstall script `uninstaller/ollama_uninstall.sh`.
Anything else is denied.

## Tighten later (optional)
- `curl *` is broad (arbitrary download path). Scope it:
  `sed -i 's#/usr/bin/curl$#/usr/bin/curl -f*S*o /usr/share/keyrings/*#' …`
- `mkdir *` could be limited to the two known system dirs:
  `/usr/bin/mkdir -p /etc/pipewire, /usr/bin/mkdir -p /var/run/cdi`
- To remove: `sudo rm /etc/sudoers.d/chezmoi-pi`

## Notes
- `Defaults !requiretty` (space form) lets unattended (no TTY) runs proceed.
  Debian `visudo` rejects the colon form `Defaults:!requiretty` as a syntax
  error. The ollama-uninstall script runs its own `systemctl`/`userdel`/
  `groupdel`/`rm` as root *inside* itself, so it needs no extra sudoers entry.
- `pi/` is excluded from chezmoi (see `.chezmoirc`) so this file is never
  auto-applied to `/etc`.
