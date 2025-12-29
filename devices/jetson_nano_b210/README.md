# jetson nano

NVidia has dropped support for Jetson Nano since Jetson Linux R32.7.6.

NOTE the driver package available may help built new kernels.

<https://www.reddit.com/r/JetsonNano/comments/1eaw3a9/some_new_os_for_jetson_nano_4gb_original/>

<https://developer.ridgerun.com/wiki/index.php/Jetson_Nano/Development/Building_the_Kernel_from_Source>
<https://developer.nvidia.com/embedded/jetson-linux-archive>
<https://developer.nvidia.com/embedded/linux-tegra-r3276>

<https://github.com/pythops/jetson-image>
<https://github.com/armbian/build/pull/2720>

## Yocto kernel 4.x

<https://github.com/orgs/OE4T/discussions/1593>
<https://marcopennelli.com/embedded-linux-yocto-project-and-nvidia-jetson-nano-developer-kit-episode-1-369c079b7c15>

## No simple answers (with armbian jetson nano)

<https://www.armbian.com/jetson-nano/>

Automatic scripts maintain this, which actually won't boot.
One will be stuck on Nvidia Logo, no visible kernel boot.

### Apply images to sdcards

    unzip -p ~/Downloads/jetson-nano-jp461-sd-card-image.zip sd-blob-b01.img | sudo dd bs=8M of=/dev/mmcblk1 status=progress oflag=sync
    xzcat ~/Downloads/Armbian_community_26.2.0-trunk.44_Jetson-nano_trixie_current_6.12.60_minimal.img.xz | sudo dd of=/dev/mmcblk1 bs=8M oflag=dsync status=progress

### Mount partition from image file

<https://askubuntu.com/questions/69363/mount-single-partition-from-image-of-entire-disk-device>

    unxz ~/Downloads/Armbian_community_26.2.0-trunk.44_Jetson-nano_trixie_current_6.12.60_minimal.img.xz
    fdisk -lu ~/Downloads/Armbian_community_26.2.0-trunk.44_Jetson-nano_trixie_current_6.12.60_minimal.img

    # calculate offset 540672 times block-size 512 = 276824064
    losetup # check free loop-device
    sudo losetup -o 276824064 /dev/loop22 ~/Downloads/Armbian_community_26.2.0-trunk.44_Jetson-nano_trixie_current_6.12.60_minimal.img
    sudo mount /dev/loop22 /mnt # check you have the rootfs mounted
    sudo dd if=/dev/loop22 of=/dev/mmcblk1p15 bs=4M oflag=dsync status=progress
    sudo fsck /dev/mmcblk1p15 # expand image-size to partition-size

### Attempt to fix armbian kernel situation INCOMPLETE

<https://shallowsky.com/linux/extlinux.html>

Copied working kernel onto armbian partition.
Still boots from L4T partition.

#### Install docker

    sudo apt install nvidia-docker2 docker-buildx

<https://forums.developer.nvidia.com/t/docker-fail-to-start-jetson-nano-after-clean-install-of-jetpack-6-2-fix-it/324760>

    sudo systemctl stop docker
    sudo update-alternatives --set iptables /usr/sbin/iptables-legacy
    sudo update-alternatives --set ip6tables /usr/sbin/ip6tables-legacy
    sudo systemctl start docker

<https://github.com/NVIDIA/nvidia-container-toolkit/issues/137>
<https://github.com/docker/cli/issues/4238>

Append `systemd.unified_cgroup_hierarchy=0` to `/boot/extlinux/extlinux.conf`.

#### Build (custom?) armbian kernel

<https://github.com/armbian/build>

    cd ~/projs
    git clone https://github.com/armbian/build.git
    cd build
    
    # https://docs.armbian.com/Developer-Guide_Build-Commands/
    ./compile.sh kernel-config BOARD=jetson-nano BRANCH=current KERNEL_BTF=no KERNEL_GIT=shallow # need around 15GB free disk space
    ./compile.sh kernel BOARD=jetson-nano BRANCH=current KERNEL_BTF=no KERNEL_GIT=shallow
    ./compile.sh kernel-dbt BOARD=jetson-nano BRANCH=current KERNEL_BTF=no KERNEL_GIT=shallow

### Attempt to take over armbian in apt-sources ABORTED

Big leap with around 200 broken packages. See example `/var/lib/aptitude/pkgstate` in git history.
Once it comes to installation, there is a fundamental difference between ubuntu 18 and recent armbian/debian in regards of the `/bin /sbin /lib` folder.

<https://wiki.debian.org/UsrMerge>

## Unlock do-release-upgrade L4T image

Set 'lts' in `/etc/update-manager/`

There are victims of upgrade

* (nv) tensorrt (can be restored ?)
* (nv) opencv / visionworks
* (nv) wayland / weston
* unity

Reaching ubuntu 24 noble soon.

## jetson-stats

    mkdir -p ~/projs ; cd ~/projs
    git clone https://github.com/rbonghi/jetson_stats.git
    cd jetson_stats
    ./scripts/install_jtop_torun_without_sudo.sh
    sudo jtop --install-service
    sudo jtop
    # eventually enable jetson-clocks
    # eventually switch to 5W mode
