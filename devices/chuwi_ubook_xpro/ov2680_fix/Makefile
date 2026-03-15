obj-m += ov2680.o

all:
	make -C /lib/modules/$(shell uname -r)/build M=$(PWD) modules

install:
	sudo make -C /lib/modules/$(shell uname -r)/build M=$(PWD) modules_install
	sudo depmod -a

clean:
	make -C /lib/modules/$(shell uname -r)/build M=$(PWD) clean

download:
	wget -O ov2680.c https://raw.githubusercontent.com/torvalds/linux/v6.8/drivers/media/i2c/ov2680.c
	#wget -O ov2680.h https://raw.githubusercontent.com/torvalds/linux/master/drivers/media/i2c/ov2680.h
	#sed -i 's/devm_v4l2_sensor_clk_get/devm_clk_get/g' ov2680.c
	sudo apt install linux-headers-$(uname -r)
