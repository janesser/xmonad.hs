# armbian jetson nano

<https://www.armbian.com/jetson-nano/>

    unzip -p Downloads/jetson-nano-jp461-sd-card-image.zip sd-blob-b01.img | sudo dd bs=4M of=/dev/mmcblk1 status=progress oflag=sync

    xzcat ~/Downloads/Armbian_community_26.2.0-trunk.44_Jetson-nano_trixie_current_6.12.60_minimal.img.xz | sudo dd of=/dev/mmcblk1 bs=64k oflag=dsync status=progress # lacks firmware in boot folder

    # https://askubuntu.com/questions/69363/mount-single-partition-from-image-of-entire-disk-device
    unxz ~/Downloads/Armbian_community_26.2.0-trunk.44_Jetson-nano_trixie_current_6.12.60_minimal.img.xz
    fdisk -lu ~/Downloads/Armbian_community_26.2.0-trunk.44_Jetson-nano_trixie_current_6.12.60_minimal.img
    # calculate offset 540672 times block-size 512 = 276824064
    losetup # check free loop-device
    sudo losetup -o 276824064 /dev/loop22 ~/Downloads/Armbian_community_26.2.0-trunk.44_Jetson-nano_trixie_current_6.12.60_minimal.img
    sudo mount /dev/loop22 /mnt # check you have the rootfs mounted
    sudo dd if=/dev/loop22 of=/dev/mmcblk1p15 bs=4M oflag=dsync status=progress
    sudo fsck /dev/mmcblk1p15 # expand image-size to partition-size

<https://shallowsky.com/linux/extlinux.html>
