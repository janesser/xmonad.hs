# Makefile for Camera Driver Compilation

# Kernel configuration: Assuming a modern Linux kernel environment
KDIR ?= /lib/modules/$(shell uname -r)/build
PWD := $(shell pwd)

# Target files
CAM0_DRIVER := cam0_driver.o
CAM1_DRIVER := cam1_driver.o
PMIC_DRIVER := pmic_driver.o

# Compilation flags (minimal)
CFLAGS := -I$(shell printenv /usr/include/linux) -Werror

# Target modules
obj-m := $(CAM0_DRIVER) $(CAM1_DRIVER) $(PMIC_DRIVER)

.PHONY: all clean

all:
	make -C $(KDIR) M=$(PWD) modules

clean:
	make -C $(KDIR) M=$(PWD) clean
