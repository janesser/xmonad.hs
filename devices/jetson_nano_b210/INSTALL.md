# jetson nano basic setup

download from <https://developer.nvidia.com/embedded/learn/get-started-jetson-nano-devkit>
flash image on sdcard (461 in filename means nothing)

    unzip -p ~/Downloads/jetson-nano-jp461-sd-card-image.zip sd-blob-b01.img | sudo dd bs=8M of=/dev/mmcblk1 status=progress

no need to adjust partitions, that happens on first boot

## on first boot

    sudo apt update
    sudo apt upgrade -y

    usermod -a -G docker $USER
    sudo apt install -y tmux nano

    # install chezmoi
    sh -c "$(curl -fsLS get.chezmoi.io)" -- -b $HOME/.local/bin

    ssh-keygen -t ed25519 -C "$USER@$HOSTNAME"

    ~/.local/bin/chezmoi init git@github.com:janesser/xmonad.hs.git
    ~/.local/bin/chezmoi age-keygen -o ~/.config/chezmoi/age-id.txt

## adjust fstab

<https://www.systutorials.com/bypassing-bad-fstab-failure-while-booting-linux/>

    /dev/root   /   ext4    defaults    0 1
    UUID="$(blkid /dev/sdaX)"     /mnt    ext4    defaults        0       1
    /mnt/var_lib_docker     /var/lib/docker none    defaults,bind   0       2
    /mnt/$USER        /home/$USER       defaults,bind   0       2

## jtop

    #!/bin/bash
    sudo apt install python3-pip
    sudo pip install -U jetson-stats
    sudo reboot

## pyenv

    curl -fsSL https://pyenv.run | bash
    # restart bash
    LATEST_PYVER=`pyenv latest -k 3`
    sudo apt install ubuntu-core-libs-dev libffi-dev libbz2-dev
    pyenv install $LATEST_PYVER
    pyenv global $LATEST_PYVER
    curl -LsSf https://astral.sh/uv/install.sh | sh
