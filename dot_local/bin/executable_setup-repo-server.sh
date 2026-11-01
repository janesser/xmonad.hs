#!/usr/bin/env bash
# setup-repo-server.sh — nginx repo server on the Raspberry Pi.
#
#   executable_setup-repo-server.sh [--repo-dir <dir>]
#
#   --repo-dir   Directory to serve (default: /srv/repo).
#
# Serves the pacman repo (built by executable_build-hylki.sh) over HTTP. The
# repo must already exist at the target dir (rsynced from the build host) —
# this script only installs nginx and configures it to expose it.
#
# sudo is used (scoped NOPASSWD drop-in) only for apt/systemctl/nginx.

set -uo pipefail

SCRIPT_NAME="$(basename "$0")"

# --- hostname guard: only meaningful on the Pi ---
if [ "$(hostname)" != "raspberrypi" ]; then
    echo "=> $SCRIPT_NAME: only intended for the Raspberry Pi (raspberry)"
    echo "=> $SCRIPT_NAME: run on the Pi, not on $(hostname)"
    exit 1
fi

REPO_DIR="${REPO_DIR:-/srv/repo}"

while [ $# -gt 0 ]; do
    case "$1" in
        --repo-dir)
            REPO_DIR="$2"; shift 2 ;;
        --repo-dir=*)
            REPO_DIR="${1#*=}"; shift ;;
        -h|--help)
            head -8 "$0"; exit 0 ;;
        *)
            echo "=> $SCRIPT_NAME: unknown arg: $1"; exit 1 ;;
    esac
done

if [ ! -d "$REPO_DIR" ]; then
    echo "=> $SCRIPT_NAME: repo dir $REPO_DIR does not exist"
    exit 1
fi

echo "=> $SCRIPT_NAME: serving $REPO_DIR on the Pi"

# --- install nginx ---
if ! command -v nginx >/dev/null 2>&1; then
    echo "=> $SCRIPT_NAME: installing nginx"
    sudo apt-get update
    sudo apt-get install -y nginx
else
    echo "=> $SCRIPT_NAME: nginx already installed"
fi

# --- serve the repo ---
SITE="/etc/nginx/sites-available/repo"
echo "=> $SCRIPT_NAME: writing nginx site config: $SITE"
sudo bash -c "cat > $SITE <<EOF
server {
    listen 80 default_server;
    server_name _;
    root $REPO_DIR;
    autoindex on;
    location / {
        try_files \$uri \$uri/ =404;
    }
}
EOF"

sudo ln -sf "$SITE" /etc/nginx/sites-enabled/repo
sudo nginx -t && sudo systemctl enable --now nginx

echo "=> $SCRIPT_NAME: repo available at http://<pi-ip>/"
echo "=> $SCRIPT_NAME: verify: curl -s http://$(hostname -I | awk '{print $1}')/ | head"
