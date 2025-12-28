# Chuwi UBook XPro S5K3_A1

## Chuwi Camera Fixing WIP

Chuwi Ubook Xpro has an (back, over screen) ov2680 and (front) ov5648.
Both work in windows 10. In vanilla linux ubuntu ov2680 is loaded, but won't result in any camera that e.g. `cheese` will detect.

<https://github.com/torvalds/linux/blob/master/drivers/media/i2c/ov2680.c>

From dmesg

    [    6.329170] ov2680 i2c-OVTI2680:00: supply DOVDD not found, using dummy regulator
    [    6.329230] ov2680 i2c-OVTI2680:00: supply DVDD not found, using dummy regulator
    [    6.329244] ov2680 i2c-OVTI2680:00: supply AVDD not found, using dummy regulator
    [    6.350631] ov2680 i2c-OVTI2680:00: sensor_revision id = 0x2680, rev= 0

<https://github.com/djrscally/miix-510-cameras>

this article describes a few i2ctransfer tricks, none has worked so far

<https://datasheet4u.com/pdf-down/O/V/2/OV2680-OmniVision.pdf>

Datasheet of the hardware vendor

WORK IN PROGRESS

* blacklisted premature ov2680 loading
* trying to figure the i2c address for poweron
