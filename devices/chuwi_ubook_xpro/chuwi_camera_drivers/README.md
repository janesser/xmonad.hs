# Porting Chuwi ubook xpro camera drivers to linux

## Context

1. details of PCI and i2c wiring of device at ./dsdt.cam0_cam1.dsl
2. ./linux-source-6.8.0 available
3. implementation plan ./PLAN.md

### how to mount linux-sources with archivemount

	sudo apt install -y archivemount
	archivemount -o readonly -o subtree=linux-source-6.8.0 /usr/src/linux-source-6.8.0.tar.bz2  ./linux-source-6.8.0
	# fusermount -u ./linux-source-6.8.0
