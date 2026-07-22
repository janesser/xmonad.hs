# Makefile for cam_char_driver kernel module

# This Makefile is designed for out-of-tree compilation using kbuild.

obj-m := cam_char_driver.o

# Kernel directory defined by user
KERNEL_DIR := /usr/src/linux-headers-7.0.0-28-generic

# Default target: build the module
all:
	$(MAKE) -C $(KERNEL_DIR) M=$(PWD) modules

# Target to build the module
%:
	$(MAKE) -C $(KERNEL_DIR) M=$(PWD) modules

# Target to clean up build artifacts
clean:
	$(MAKE) -C $(KERNEL_DIR) M=$(PWD) clean

# Target to install the module (optional)
install:
	$(MAKE) -C $(KERNEL_DIR) M=$(PWD) modules_install
