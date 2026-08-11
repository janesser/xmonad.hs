# Makefile for building custom INT3472/TPS68470 module with Chuwi UBook XPro board data
# Proper out-of-tree kbuild module
# No sudo required for building

ifneq ($(KERNELRELEASE),)
# kbuild part — only seen when building from kernel source tree
obj-m := intel_skl_int3472_tps68470.o
intel_skl_int3472_tps68470-y := tps68470.o tps68470_board_data.o common.o

else
# out-of-tree build
KDIR ?= /lib/modules/$(shell uname -r)/build

default:
	$(MAKE) -C $(KDIR) M=$(PWD)

endif

.PHONY: install clean

MOD := $(PWD)/intel_skl_int3472_tps68470.ko
MOD_MOD := $(PWD)/intel_skl_int3472_tps68470.mod
KVER := $(shell uname -r)
INSTALL_DIR := /lib/modules/$(KVER)/updates/

install: $(MOD) $(MOD_MOD)
	sudo install -d $(INSTALL_DIR)
	sudo install -m 0644 $(MOD) $(INSTALL_DIR)$(notdir $(MOD))
	sudo install -m 0644 $(MOD_MOD) $(INSTALL_DIR)$(notdir $(MOD_MOD))
	sudo depmod -a $(KVER)
	@echo
	@echo "Module installed to /lib/modules/$(KVER)/updates/"
	@echo "To load the module, run: sudo insmod $(MOD)"
	@echo "Or add to /etc/modules-load.d/intel_skl_int3472_tps68470.conf:"
	@echo "intel_skl_int3472_tps68470"

clean:
	rm -f *.o *.ko *.mod *.mod.c .*.cmd Module.symvers modules.order
