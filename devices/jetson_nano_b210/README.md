# jetson nano

<https://developer.nvidia.com/embedded/linux-tegra-r3276>

NVidia has dropped support for Jetson Nano since Jetson Linux R32.7.6.

NOTE the driver package available may help built new kernels.

## No simple answers (with armbian jetson nano)

<https://www.armbian.com/jetson-nano/>

Automatic scripts maintain this, which actually won't boot.
One will be stuck on Nvidia Logo, no visible kernel boot.

Collection for commands to deal with images:

    unzip -p ~/Downloads/jetson-nano-jp461-sd-card-image.zip sd-blob-b01.img | sudo dd bs=8M of=/dev/mmcblk1 status=progress oflag=sync
    xzcat ~/Downloads/Armbian_community_26.2.0-trunk.44_Jetson-nano_trixie_current_6.12.60_minimal.img.xz | sudo dd of=/dev/mmcblk1 bs=8M oflag=dsync status=progress

    # https://askubuntu.com/questions/69363/mount-single-partition-from-image-of-entire-disk-device
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

### Attempt to take over armbian in apt-sources ABORTED

Big leap with around 200 broken packages. See example `/var/lib/aptitude/pkgstate` in git history.
Once it comes to installation, there is a fundamental difference between ubuntu 18 and recent armbian/debian in regards of the `/bin /sbin /lib` folder.

<https://wiki.debian.org/UsrMerge>

## Unlock do-release-upgrade L4T image

Set 'lts' in `/etc/update-manager/`

There are victims of upgrade

* (nv) tensorrt
* (nv) opencv / visionworks

Reaching ubuntu 22 jammy soon.
