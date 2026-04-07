obj-m += ov2680.o

all:
	make -C /lib/modules/$(shell uname -r)/build M=$(PWD) modules

install:
	sudo make -C /lib/modules/$(shell uname -r)/build M=$(PWD) modules_install
	sudo depmod -a

uninstall:
	sudo rm -f /lib/modules/$(shell uname -r)/updates/ov2680.ko
	sudo depmod -a
	# sudo update-initramfs -u

clean:
	make -C /lib/modules/$(shell uname -r)/build M=$(PWD) clean

download:
	wget -O ov2680.c.v_6_8 https://raw.githubusercontent.com/torvalds/linux/v6.8/drivers/media/i2c/ov2680.c
	sed -i 's/devm_v4l2_sensor_clk_get/devm_clk_get/g' ov2680.c.v_6_8
	wget -O ov2680.c.v_master https://raw.githubusercontent.com/torvalds/linux/master/drivers/media/i2c/ov2680.c
	wget -O ov2680.c_v_rpi-6_12 https://raw.githubusercontent.com/raspberrypi/linux/refs/heads/rpi-6.12.y/drivers/media/i2c/ov2680.c
	cp ov2680.c_v_rpi-6_12 ov2680.c
	sudo apt install linux-headers-$(shell uname -r)
