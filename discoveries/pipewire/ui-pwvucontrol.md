# pwvucontrol instead of pavucontrol on pipewire

pavucontrol is polling in a way that causes a moderate cpu base load

    sudo apt install -y libwireplumber-0.5-dev
    sudo apt install -y libpipewire-0.3-dev
    sudo apt install -y libadwaita-1-dev
    sudo apt install -y libgtk-4-dev
    sudo apt install -y libglib2.0-dev

    cd ~/projs
    git clone https://github.com/saivert/pwvucontrol.git
    cd pwvucontrol

    meson setup builddir
    meson compile -C builddir
    meson install -C builddir
