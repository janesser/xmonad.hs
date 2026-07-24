# Makefile for Camera Driver Compilation

# Kernel configuration: Assuming a modern Linux kernel environment
KDIR ?= /lib/modules/$(shell uname -r)/build
PWD := $(shell pwd)

# Source files
CAM0_DRIVER := cam0_driver.c
CAM1_DRIVER := cam1_driver.c

# Compilation flags (minimal)
CFLAGS := -I$(shell printenv /usr/include/linux) -Werror

# Target modules
obj-m := $(CAM0_DRIVER) $(CAM1_DRIVER)

.PHONY: all clean

all:
	make -C $(KDIR) M=$(PWD) modules

clean:
	make -C $(KDIR) M=$(PWD) clean