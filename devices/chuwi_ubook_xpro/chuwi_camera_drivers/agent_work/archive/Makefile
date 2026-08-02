# Standard kernel module Makefile

obj-m := chuwi_camera_driver.o

KVER := $(shell uname -r)

all:
	make -C /lib/modules/${KVER}/build M=$(PWD) modules

clean:
	make -C /lib/modules/${KVER}/build M=$(PWD) clean

install:
	make -C /lib/modules/${KVER}/build M=$(PWD) modules_install