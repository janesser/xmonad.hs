#/bin/bash

uv tool install ratarmount
if ls /usr/src/linux-source-6.8.0; then
  echo linux sources already available
else
  sudo apt install -y linux-source-6.8.0
fi


LINUX_SRC="./linux-source-6.8.0"
mkdir -p $LINUX_SRC
if mountpoint $LINUX_SRC; then
  echo "already mounted: $LINUX_SRC"
else
  ratarmount -o modules=subdir,subdir=linux-source-6.8.0 /usr/src/linux-source-6.8.0.tar.bz2 $LINUX_SRC
fi

UBOOK_XPRO_DRIVERS="./chuwi-ubook-xpro"
mkdir -p $UBOOK_XPRO_DRIVERS
if mountpoint $UBOOK_XPRO_DRIVERS; then
  echo "already mounted: $UBOOK_XPRO_DRIVERS"
else
  ratarmount -o modules=subdir,subdir='UBook XPro  Drivers' ./ubook-xpro-drivers-win11.rar $UBOOK_XPRO_DRIVERS
fi
