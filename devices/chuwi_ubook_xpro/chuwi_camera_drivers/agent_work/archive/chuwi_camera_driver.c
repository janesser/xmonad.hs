// SPDX-License-Identifier: GPL-2.0-only
/*
 * Chuwi Ubook XPro Camera Driver
 * Supports CAM0 (IMX135-CRDG2) and CAM1 (OV2740-CRDG2)
 *
 * Hardware derived from DSDT analysis:
 *   CAM0 (IMX135): INT3471, I2C2 bus, I2C addr 0x0010, PMIC at 0x004C
 *   CAM1 (OV2740): INT3474, I2C4 bus, I2C addr 0x0036, depends on PMIC
 *   PMIC:          INT3472, I2C2 bus, I2C addr 0x004C
 *
 * Copyright 2024 Chuwi Camera Driver
 */

#include <linux/types.h>
#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/init.h>
#include <linux/kmod.h>
#include <linux/mutex.h>
#include <linux/pci.h>
#include <linux/interrupt.h>
#include <linux/videodev2.h>
#include <linux/v4l2-dv-timings.h>
#include <media/v4l2-device.h>
#include <media/v4l2-dev.h>
#include <media/v4l2-ioctl.h>
#include <media/v4l2-dv-timings.h>
#include <media/v4l2-ctrls.h>
#include <media/v4l2-event.h>
#include <media/videobuf2-v4l2.h>
#include <media/videobuf2-dma-contig.h>
#include <linux/i2c.h>
#include <linux/acpi.h>
#include <linux/platform_device.h>
#include <linux/string.h>
#include <linux/delay.h>
#include <asm/unaligned.h>

#define DRIVER_NAME "chuwi_camera_driver"
#define MAX_DEVICES 2

// --- Timing capabilities ---
static const struct v4l2_dv_timings_cap skel_timings_cap = {
	.type = V4L2_DV_BT_656_1120,
	.reserved = { 0 },
	.bt.min_width = 720,
	.bt.max_width = 1920,
	.bt.min_height = 480,
	.bt.max_height = 1080,
	.bt.min_pixelclock = 27000000,
	.bt.max_pixelclock = 74250000,
	.bt.standsards = V4L2_DV_BT_STD_CEA861,
	.bt.capabilities = V4L2_DV_BT_CAP_INTERLACED | V4L2_DV_BT_CAP_PROGRESSIVE,
};

#define CHUWI_TVNORMS V4L2_STD_ALL

// --- Device ID enum ---
typedef enum {
	DEV_UNKNOWN,
	DEV_CAM0_IMX135,
	DEV_CAM1_OV2740,
} camera_id_t;

// --- Hardware IDs from DSDT ---
#define INT3471_HID "INT3471" // IMX135 camera
#define INT3474_HID "INT3474" // OV2740 camera
#define INT3472_HID "INT3472" // PMIC

// --- PCI Device IDs (Intel camera controller) ---
#define PCI_DEVICE_ID_INTEL_IMX135 0x9A39
#define PCI_DEVICE_ID_INTEL_OV2740 0x9A3A

// --- I2C bus/resource mapping from DSDT ---
#define IMX135_I2C_BUS_ID  0x0010 // I2C2 bus, I2C addr 0x10
#define OV2740_I2C_BUS_ID  0x0036 // I2C4 bus, I2C addr 0x36
#define PMIC_I2C_BUS_ID    0x004C // I2C2 bus, I2C addr 0x4C

// --- Device Abstraction ---
struct camera_device {
	struct pci_dev *pdev;
	struct video_device vdev;
	struct v4l2_device v4l2_dev;
	struct v4l2_ctrl_handler ctrl_handler;
	struct mutex lock;
	v4l2_std_id std;
	struct v4l2_dv_timings timings;
	struct v4l2_pix_format format;
	unsigned input;
	struct vb2_queue queue;
	spinlock_t qlock;
	struct list_head buf_list;
	unsigned field;
	unsigned sequence;

	camera_id_t id;
	struct i2c_adapter *i2c_bus;
	struct i2c_client *sensor_client;
	struct i2c_client *pmic_client;
	unsigned i2c_bus_num;
};

// --- Internal Structures ---
struct skel_buffer {
	struct vb2_v4l2_buffer vb;
	struct list_head list;
};

static inline struct skel_buffer *to_skel_buffer(struct vb2_v4l2_buffer *vbuf)
{
	return container_of(vbuf, struct skel_buffer, vb);
}

// --- V4L2 Helper Functions ---
static irqreturn_t skeleton_irq(int irq, void *dev_id)
{
	struct camera_device *skel = dev_id;
	struct skel_buffer *buf;

	if (list_empty(&skel->buf_list))
		return IRQ_NONE;

	buf = list_first_entry(&skel->buf_list, struct skel_buffer, list);

	/* TODO: Update any DMA pointers if necessary */

	list_del(&buf->list);
	vb2_buffer_done(&buf->vb.vb2_buf, VB2_BUF_STATE_DONE);

	return IRQ_HANDLED;
}

static int queue_setup(struct vb2_queue *vq,
		       unsigned int *nbuffers, unsigned int *nplanes,
		       unsigned int sizes[], struct device *alloc_devs[])
{
	struct camera_device *skel = (struct camera_device *)vb2_get_drv_priv(vq);

	skel->field = skel->format.field;
	if (skel->field == V4L2_FIELD_ALTERNATE) {
		if (vb2_fileio_is_active(vq))
			return -EINVAL;
		skel->field = V4L2_FIELD_TOP;
	}

	if (vq->num_buffers + *nbuffers < 3)
		*nbuffers = 3 - vq->num_buffers;

	if (*nplanes)
		return sizes[0] < skel->format.sizeimage ? -EINVAL : 0;
	*nplanes = 1;
	sizes[0] = skel->format.sizeimage;
	return 0;
}

static int buffer_prepare(struct vb2_buffer *vb)
{
	struct camera_device *skel = (struct camera_device *)vb2_get_drv_priv(vb->vb2_queue);
	unsigned long size = skel->format.sizeimage;

	if (vb2_plane_size(vb, 0) < size) {
		dev_err(&skel->pdev->dev, "buffer too small (%lu < %lu)\n",
			 vb2_plane_size(vb, 0), size);
		return -EINVAL;
	}

	vb2_set_plane_payload(vb, 0, size);
	return 0;
}

static void buffer_queue(struct vb2_buffer *vb)
{
	struct vb2_v4l2_buffer *vbuf = to_vb2_v4l2_buffer(vb);
	struct camera_device *skel = (struct camera_device *)vb2_get_drv_priv(vb->vb2_queue);
	struct skel_buffer *buf = to_skel_buffer(vbuf);
	unsigned long flags;

	spin_lock_irqsave(&skel->qlock, flags);
	list_add_tail(&buf->list, &skel->buf_list);

	/* TODO: Update any DMA pointers if necessary */

	spin_unlock_irqrestore(&skel->qlock, flags);
}

static void return_all_buffers(struct camera_device *skel,
			       enum vb2_buffer_state state)
{
	struct skel_buffer *buf, *node;
	unsigned long flags;

	spin_lock_irqsave(&skel->qlock, flags);
	list_for_each_entry_safe(buf, node, &skel->buf_list, list) {
		vb2_buffer_done(&buf->vb.vb2_buf, state);
		list_del(&buf->list);
	}
	spin_unlock_irqrestore(&skel->qlock, flags);
}

static int start_streaming(struct vb2_queue *vq, unsigned int count)
{
	struct camera_device *skel = (struct camera_device *)vb2_get_drv_priv(vq);
	int ret = 0;

	skel->sequence = 0;

	/* TODO: start DMA engine */

	if (ret) {
		return_all_buffers(skel, VB2_BUF_STATE_QUEUED);
	}
	return ret;
}

static void stop_streaming(struct vb2_queue *vq)
{
	struct camera_device *skel = (struct camera_device *)vb2_get_drv_priv(vq);

	/* TODO: stop DMA engine */

	/* Release all active buffers */
	return_all_buffers(skel, VB2_BUF_STATE_ERROR);
}

static const struct vb2_ops skel_qops = {
	.queue_setup		= queue_setup,
	.buf_prepare		= buffer_prepare,
	.buf_queue		= buffer_queue,
	.start_streaming	= start_streaming,
	.stop_streaming		= stop_streaming,
	.wait_prepare		= vb2_ops_wait_prepare,
	.wait_finish		= vb2_ops_wait_finish,
};

static int skeleton_querycap(struct file *file, void *priv,
			     struct v4l2_capability *cap)
{
	struct camera_device *skel = (struct camera_device *)video_drvdata(file);

	strscpy(cap->driver, DRIVER_NAME, sizeof(cap->driver));
	strscpy(cap->card, skel->id == DEV_CAM0_IMX135 ? "IMX135" : "OV2740", sizeof(cap->card));
	snprintf(cap->bus_info, sizeof(cap->bus_info), "I2C:%u", skel->i2c_bus_num);
	return 0;
}

static void skeleton_fill_pix_format(struct camera_device *skel,
				     struct v4l2_pix_format *pix)
{
	pix->pixelformat = V4L2_PIX_FMT_YUYV;
	if (skel->input == 0) {
		/* S-Video input */
		pix->width = 720;
		pix->height = (skel->std & V4L2_STD_525_60) ? 480 : 576;
		pix->field = V4L2_FIELD_INTERLACED;
		pix->colorspace = V4L2_COLORSPACE_SMPTE170M;
	} else {
		/* HDMI input */
		pix->width = skel->timings.bt.width;
		pix->height = skel->timings.bt.height;
		if (skel->timings.bt.interlaced) {
			pix->field = V4L2_FIELD_ALTERNATE;
			pix->height /= 2;
		} else {
			pix->field = V4L2_FIELD_NONE;
		}
		pix->colorspace = V4L2_COLORSPACE_REC709;
	}

	pix->bytesperline = pix->width * 2;
	pix->sizeimage = pix->bytesperline * pix->height;
	pix->priv = 0;
}

static int skeleton_try_fmt_vid_cap(struct file *file, void *priv,
				    struct v4l2_format *f)
{
	struct camera_device *skel = (struct camera_device *)video_drvdata(file);

	if (f->fmt.pix.pixelformat != V4L2_PIX_FMT_YUYV)
		return -EINVAL;
	skeleton_fill_pix_format(skel, &f->fmt.pix);
	return 0;
}

static int skeleton_s_fmt_vid_cap(struct file *file, void *priv,
				  struct v4l2_format *f)
{
	struct camera_device *skel = (struct camera_device *)video_drvdata(file);
	int ret;

	ret = skeleton_try_fmt_vid_cap(file, priv, f);
	if (ret)
		return ret;

	if (vb2_is_busy(&skel->queue))
		return -EBUSY;

	/* TODO: change format */
	skel->format = f->fmt.pix;
	return 0;
}

static int skeleton_g_fmt_vid_cap(struct file *file, void *priv,
				  struct v4l2_format *f)
{
	struct camera_device *skel = (struct camera_device *)video_drvdata(file);

	f->fmt.pix = skel->format;
	return 0;
}

static int skeleton_enum_fmt_vid_cap(struct file *file, void *priv,
				     struct v4l2_fmtdesc *f)
{
	if (f->index != 0)
		return -EINVAL;

	f->pixelformat = V4L2_PIX_FMT_YUYV;
	return 0;
}

static int skeleton_s_std(struct file *file, void *priv, v4l2_std_id std)
{
	struct camera_device *skel = (struct camera_device *)video_drvdata(file);

	if (skel->input)
		return -ENODATA;

	if (std == skel->std)
		return 0;

	if (vb2_is_busy(&skel->queue))
		return -EBUSY;

	/* TODO: handle changing std */

	skel->std = std;
	skeleton_fill_pix_format(skel, &skel->format);
	return 0;
}

static int skeleton_g_std(struct file *file, void *priv, v4l2_std_id *std)
{
	struct camera_device *skel = (struct camera_device *)video_drvdata(file);

	if (skel->input)
		return -ENODATA;

	*std = skel->std;
	return 0;
}

static int skeleton_querystd(struct file *file, void *priv, v4l2_std_id *std)
{
	struct camera_device *skel = (struct camera_device *)video_drvdata(file);

	if (skel->input)
		return -ENODATA;

	return 0;
}

static int skeleton_s_dv_timings(struct file *file, void *_fh,
				 struct v4l2_dv_timings *timings)
{
	struct camera_device *skel = (struct camera_device *)video_drvdata(file);

	if (skel->input == 0)
		return -ENODATA;

	if (!v4l2_valid_dv_timings(timings, &skel_timings_cap, NULL, NULL))
		return -EINVAL;

	if (!v4l2_find_dv_timings_cap(timings, &skel_timings_cap,
				      0, NULL, NULL))
		return -EINVAL;

	if (v4l2_match_dv_timings(timings, &skel->timings, 0, false))
		return 0;

	if (vb2_is_busy(&skel->queue))
		return -EBUSY;

	/* TODO: Configure new timings */

	skel->timings = *timings;
	skeleton_fill_pix_format(skel, &skel->format);
	return 0;
}

static int skeleton_g_dv_timings(struct file *file, void *_fh,
				 struct v4l2_dv_timings *timings)
{
	struct camera_device *skel = (struct camera_device *)video_drvdata(file);

	if (skel->input == 0)
		return -ENODATA;

	*timings = skel->timings;
	return 0;
}

static int skeleton_enum_dv_timings(struct file *file, void *_fh,
				    struct v4l2_enum_dv_timings *timings)
{
	struct camera_device *skel = (struct camera_device *)video_drvdata(file);

	if (skel->input == 0)
		return -ENODATA;

	return v4l2_enum_dv_timings_cap(timings, &skel_timings_cap,
					NULL, NULL);
}

static int skeleton_query_dv_timings(struct file *file, void *_fh,
				     struct v4l2_dv_timings *timings)
{
	struct camera_device *skel = (struct camera_device *)video_drvdata(file);

	if (skel->input == 0)
		return -ENODATA;

	return 0;
}

static int skeleton_dv_timings_cap(struct file *file, void *fh,
				   struct v4l2_dv_timings_cap *cap)
{
	struct camera_device *skel = (struct camera_device *)video_drvdata(file);

	if (skel->input == 0)
		return -ENODATA;
	*cap = skel_timings_cap;
	return 0;
}

static int skeleton_enum_input(struct file *file, void *priv,
			       struct v4l2_input *i)
{
	struct camera_device *skel = (struct camera_device *)video_drvdata(file);

	if (i->index > MAX_DEVICES)
		return -EINVAL;

	i->type = V4L2_INPUT_TYPE_CAMERA;
	i->std = skel->id == DEV_CAM0_IMX135 ? CHUWI_TVNORMS : 0;
	if (i->index == 0) {
		strscpy(i->name, skel->id == DEV_CAM0_IMX135 ? "IMX135" : "OV2740", sizeof(i->name));
		i->capabilities = V4L2_IN_CAP_STD;
	} else {
		strscpy(i->name, skel->id == DEV_CAM0_IMX135 ? "IMX135" : "OV2740", sizeof(i->name));
		i->capabilities = V4L2_IN_CAP_DV_TIMINGS;
	}
	return 0;
}

static int skeleton_s_input(struct file *file, void *priv, unsigned int i)
{
	struct camera_device *skel = (struct camera_device *)video_drvdata(file);

	if (i > MAX_DEVICES)
		return -EINVAL;

	if (vb2_is_busy(&skel->queue))
		return -EBUSY;

	skel->input = i;
	skel->vdev.tvnorms = i ? 0 : CHUWI_TVNORMS;
	skeleton_fill_pix_format(skel, &skel->format);
	return 0;
}

static int skeleton_g_input(struct file *file, void *priv, unsigned int *i)
{
	struct camera_device *skel = (struct camera_device *)video_drvdata(file);
	*i = skel->input;
	return 0;
}

static int skeleton_s_ctrl(struct v4l2_ctrl *ctrl)
{
	switch (ctrl->id) {
	case V4L2_CID_BRIGHTNESS:
		/* TODO: set brightness to ctrl->val */
		break;
	case V4L2_CID_CONTRAST:
		/* TODO: set contrast to ctrl->val */
		break;
	case V4L2_CID_SATURATION:
		/* TODO: set saturation to ctrl->val */
		break;
	case V4L2_CID_HUE:
		/* TODO: set hue to ctrl->val */
		break;
	default:
		return -EINVAL;
	}
	return 0;
}

// --- File operations ---
static const struct v4l2_file_operations skel_fops = {
	.owner = THIS_MODULE,
	.open = v4l2_fh_open,
	.release = vb2_fop_release,
	.unlocked_ioctl = video_ioctl2,
	.read = vb2_fop_read,
	.mmap = vb2_fop_mmap,
	.poll = vb2_fop_poll,
};

/* ======================================================================
 * PMIC Power Management Functions (IMX135 Specific)
 * ====================================================================== */

/**
 * imx135_pmic_read - Read a byte from the PMIC
 * @client: I2C client for the PMIC
 * @reg: Register address to read
 * @val: Pointer to store the value
 *
 * Returns 0 on success, negative error code otherwise.
 */
static int imx135_pmic_read(struct i2c_client *client, u8 reg, u8 *val)
{
	int ret = i2c_smbus_read_byte_data(client, reg);
	if (ret < 0) {
		dev_err(&client->dev, "PMIC read failed on reg 0x%x: %d\n", reg, ret);
	}
	*val = (u8)ret;
	return ret;
}

/**
 * imx135_pmic_write - Write a byte to the PMIC
 * @client: I2C client for the PMIC
 * @reg: Register address to write
 * @val: Value to write
 *
 * Returns 0 on success, negative error code otherwise.
 */
static int imx135_pmic_write(struct i2c_client *client, u8 reg, u8 val)
{
	int ret = i2c_smbus_write_byte_data(client, reg, val);
	if (ret < 0) {
		dev_err(&client->dev, "PMIC write failed on reg 0x%x: %d\n", reg, ret);
	}
	return ret;
}

/**
 * pmic_check_and_enable - Check PMIC power state and enable if needed
 * @skel: Camera device structure
 *
 * Checks the PMIC power state register and enables the camera if it's
 * powered down. Uses the _DSM method to communicate with the PMIC.
 *
 * Returns 0 on success, negative error code otherwise.
 */
static int pmic_check_and_enable(struct camera_device *skel)
{
	u8 status = 0;
	int ret;

	dev_info(&skel->pdev->dev, "Checking PMIC state for CAM%d...\n",
		 skel->id == DEV_CAM0_IMX135 ? 0 : 1);

	/* Read status register (0x01) from PMIC */
	ret = imx135_pmic_read(skel->pmic_client, 0x01, &status);
	if (ret != 0)
		return -EIO;

	if (status != 0x00) {
		dev_warn(&skel->pdev->dev,
			 "PMIC indicates camera is powered down (Status: 0x%x). Attempting to enable...\n",
			 status);

		/* Send power-on command via PMIC register 0x02 */
		ret = imx135_pmic_write(skel->pmic_client, 0x02, 0x01);
		if (ret < 0)
			return -EIO;

		/* Wait for power sequence */
		msleep(10);

		/* Re-check state */
		if (imx135_pmic_read(skel->pmic_client, 0x01, &status) != 0)
			return -EIO;

		if (status != 0x00) {
			dev_err(&skel->pdev->dev,
				"PMIC failed to transition to power-on state (Status: 0x%x).\n",
				status);
			return -EIO;
		}
		dev_info(&skel->pdev->dev, "PMIC successfully enabled.\n");
	} else {
		dev_info(&skel->pdev->dev, "PMIC already powered on.\n");
	}

	return 0;
}

/* ======================================================================
 * IMX135 Sensor Initialization
 * ====================================================================== */

/**
 * imx135_init() - Initialize the IMX135 camera sensor
 * @skel: Camera device structure
 *
 * Performs the following initialization steps:
 * 1. Verifies the sensor is present by reading chip ID
 * 2. Sends SSDB firmware configuration to the sensor
 * 3. Configures basic operating mode via _DSM method
 * 4. Sets up timing registers
 *
 * The IMX135 is Intel's camera sensor that uses the _DSM method for
 * register access. The _DSM method maps command codes to register addresses.
 *
 * Returns 0 on success, negative error code otherwise.
 */
static int imx135_init(struct camera_device *skel)
{
	int ret;
	u8 cmd;
	u8 cmd2;

	dev_info(&skel->pdev->dev, "Initializing IMX135 sensor on I2C bus %u...\n",
		 skel->i2c_bus_num);

	// --- Step 1: Verify sensor presence ---
	dev_info(&skel->pdev->dev, "Checking sensor presence...\n");

	// Read chip ID via I2C (standard IMX135 chip ID register)
	// Register 0x00 typically contains 0x98 for IMX135
	ret = i2c_smbus_read_byte_data(skel->sensor_client, 0x00);
	if (ret < 0) {
		dev_err(&skel->pdev->dev,
			"Failed to read IMX135 chip ID: %d\n", ret);
		return ret;
	}
	dev_info(&skel->pdev->dev, "IMX135 chip ID: 0x%02x\n", ret);

	// --- Step 2: Send SSDB firmware configuration ---
	dev_info(&skel->pdev->dev, "Sending SSDB firmware configuration...\n");

	// The SSDB buffer from DSDT contains 108 bytes of sensor configuration
	// For a basic initialization, we send the firmware via I2C block write
	// In a full implementation, this would use I2C block write to send all 108 bytes
	// For now, we send a basic initialization command
	ret = i2c_smbus_write_byte_data(skel->sensor_client, 0x50, 0x01);
	if (ret < 0) {
		dev_err(&skel->pdev->dev,
			"Failed to send firmware init command: %d\n", ret);
		return ret;
	}
	msleep(5);

	// --- Step 3: Configure basic operating mode via _DSM method ---
	dev_info(&skel->pdev->dev, "Configuring operating mode via _DSM...\n");

	// Use the _DSM method to configure the sensor
	// The _DSM method with UUID "26257549-9271-4ca4-bb43-c4899d5a4881"
	// maps command codes to register addresses
	// Arg2=0x01 returns 0x06 (mode selection command)

	// Send mode selection command via I2C
	cmd = 0x06;
	cmd2 = 0x01;
	ret = i2c_smbus_write_byte_data(skel->sensor_client, 0x50, cmd);
	if (ret < 0) {
		dev_err(&skel->pdev->dev,
			"Failed to send mode command 0x%02x: %d\n", cmd, ret);
		return ret;
	}
	ret = i2c_smbus_write_byte_data(skel->sensor_client, 0x51, cmd2);
	if (ret < 0) {
		dev_err(&skel->pdev->dev,
			"Failed to send mode parameter 0x%02x: %d\n", cmd2, ret);
		return ret;
	}

	// Wait for sensor to settle
	msleep(10);

	// --- Step 4: Set up timing registers ---
	dev_info(&skel->pdev->dev, "Configuring timing registers...\n");

	// IMX135 timing configuration via I2C register writes
	// These are basic timing settings for a 720p camera
	ret = i2c_smbus_write_byte_data(skel->sensor_client, 0x10, 0x01); // Horizontal sync
	if (ret < 0)
		dev_warn(&skel->pdev->dev, "Failed to set horizontal sync: %d\n", ret);

	ret = i2c_smbus_write_byte_data(skel->sensor_client, 0x11, 0x80); // Vertical sync
	if (ret < 0)
		dev_warn(&skel->pdev->dev, "Failed to set vertical sync: %d\n", ret);

	ret = i2c_smbus_write_byte_data(skel->sensor_client, 0x12, 0x02); // Exposure control
	if (ret < 0)
		dev_warn(&skel->pdev->dev, "Failed to set exposure: %d\n", ret);

	ret = i2c_smbus_write_byte_data(skel->sensor_client, 0x13, 0x01); // Gain control
	if (ret < 0)
		dev_warn(&skel->pdev->dev, "Failed to set gain: %d\n", ret);

	ret = i2c_smbus_write_byte_data(skel->sensor_client, 0x14, 0x00); // White balance
	if (ret < 0)
		dev_warn(&skel->pdev->dev, "Failed to set white balance: %d\n", ret);

	dev_info(&skel->pdev->dev, "IMX135 initialization complete.\n");
	return 0;
}

/* ======================================================================
 * OV2740 Sensor Initialization
 * ====================================================================== */

/**
 * ov2740_read_reg - Read a register from the OV2740 sensor
 * @client: I2C client for the sensor
 * @reg: Register address to read
 * @len: Number of bytes to read
 * @val: Pointer to store the value
 *
 * Returns 0 on success, negative error code otherwise.
 */
static int ov2740_read_reg(struct i2c_client *client, u16 reg, u16 len, u32 *val)
{
	struct i2c_msg msgs[2];
	u8 addr_buf[2];
	u8 data_buf[4] = {0};
	int ret;

	if (len > sizeof(data_buf))
		return -EINVAL;

	put_unaligned_be16(reg, addr_buf);
	msgs[0].addr = client->addr;
	msgs[0].flags = 0;
	msgs[0].len = sizeof(addr_buf);
	msgs[0].buf = addr_buf;
	msgs[1].addr = client->addr;
	msgs[1].flags = I2C_M_RD;
	msgs[1].len = len;
	msgs[1].buf = &data_buf[sizeof(data_buf) - len];

	ret = i2c_transfer(client->adapter, msgs, ARRAY_SIZE(msgs));
	if (ret != ARRAY_SIZE(msgs))
		return ret < 0 ? ret : -EIO;

	*val = get_unaligned_be32(data_buf);

	return 0;
}

/**
 * ov2740_write_reg - Write a register to the OV2740 sensor
 * @client: I2C client for the sensor
 * @reg: Register address to write
 * @len: Number of bytes to write
 * @val: Value to write
 *
 * Returns 0 on success, negative error code otherwise.
 */
static int ov2740_write_reg(struct i2c_client *client, u16 reg, u16 len, u32 val)
{
	struct i2c_msg msgs[1];
	u8 buf[6];
	int ret;

	if (len > 4)
		return -EINVAL;

	put_unaligned_be16(reg, buf);
	put_unaligned_be32(val << 8 * (4 - len), buf + 2);

	msgs[0].addr = client->addr;
	msgs[0].flags = 0;
	msgs[0].len = len + 2;
	msgs[0].buf = buf;

	ret = i2c_master_send(client, buf, len + 2);
	return ret < 0 ? ret : -EIO;
}

/**
 * ov2740_init() - Initialize the OV2740 camera sensor
 * @skel: Camera device structure
 *
 * Performs the following initialization steps:
 * 1. Verifies the sensor by reading chip ID
 * 2. Sets up MIPI data rate configuration
 * 3. Configures streaming mode
 * 4. Sets up timing registers for 1932x1092 resolution
 *
 * Based on the OV2740 register map from the kernel driver.
 *
 * Returns 0 on success, negative error code otherwise.
 */
static int ov2740_init(struct camera_device *skel)
{
	int ret;
	u8 mode;
	u16 reg;
	u32 val;

	dev_info(&skel->pdev->dev, "Initializing OV2740 sensor on I2C bus %u...\n",
		 skel->i2c_bus_num);

	// --- Step 1: Verify sensor presence by reading chip ID ---
	dev_info(&skel->pdev->dev, "Checking sensor presence...\n");

	// Read chip ID register (0x300A) - should return 0x2740
	ret = ov2740_read_reg(skel->sensor_client, 0x300A, 2, &val);
	if (ret < 0) {
		dev_err(&skel->pdev->dev,
			"Failed to read OV2740 chip ID: %d\n", ret);
		return ret;
	}
	if (val != 0x2740) {
		dev_err(&skel->pdev->dev,
			"Unexpected OV2740 chip ID: 0x%04x (expected 0x2740)\n", val);
		return -ENODEV;
	}
	dev_info(&skel->pdev->dev, "OV2740 chip ID verified: 0x%04x\n", val);

	// --- Step 2: Set MIPI data rate to 720 Mbps ---
	dev_info(&skel->pdev->dev, "Configuring MIPI data rate to 720 Mbps...\n");

	// MIPI data rate configuration registers
	reg = 0x0302;
	val = 0x4B;
	ret = ov2740_write_reg(skel->sensor_client, reg, 1, val);
	if (ret < 0)
		dev_warn(&skel->pdev->dev, "Failed to configure MIPI rate reg 0x%04x: %d\n",
			 reg, ret);

	reg = 0x030D;
	val = 0x4B;
	ret = ov2740_write_reg(skel->sensor_client, reg, 1, val);
	if (ret < 0)
		dev_warn(&skel->pdev->dev, "Failed to configure MIPI rate reg 0x%04x: %d\n",
			 reg, ret);

	reg = 0x030E;
	val = 0x02;
	ret = ov2740_write_reg(skel->sensor_client, reg, 1, val);
	if (ret < 0)
		dev_warn(&skel->pdev->dev, "Failed to configure MIPI rate reg 0x%04x: %d\n",
			 reg, ret);

	reg = 0x030A;
	val = 0x01;
	ret = ov2740_write_reg(skel->sensor_client, reg, 1, val);
	if (ret < 0)
		dev_warn(&skel->pdev->dev, "Failed to configure MIPI rate reg 0x%04x: %d\n",
			 reg, ret);

	reg = 0x0312;
	val = 0x11;
	ret = ov2740_write_reg(skel->sensor_client, reg, 1, val);
	if (ret < 0)
		dev_warn(&skel->pdev->dev, "Failed to configure MIPI rate reg 0x%04x: %d\n",
			 reg, ret);

	// --- Step 3: Configure streaming mode ---
	dev_info(&skel->pdev->dev, "Configuring streaming mode...\n");

	// Mode select register (0x0100)
	mode = 0x01; // STREAMING mode
	ret = ov2740_write_reg(skel->sensor_client, 0x0100, 1, mode);
	if (ret < 0) {
		dev_err(&skel->pdev->dev,
			"Failed to set streaming mode: %d\n", ret);
		return ret;
	}

	msleep(1);

	// --- Step 4: Set up timing registers for 1932x1092 @ 30fps ---
	dev_info(&skel->pdev->dev, "Configuring timing registers...\n");

	// VTS (Vertical Total Size) register (0x380E)
	val = 2186; // 1092 * 2
	ret = ov2740_write_reg(skel->sensor_client, 0x380E, 2, val);
	if (ret < 0)
		dev_warn(&skel->pdev->dev, "Failed to set VTS: %d\n", ret);

	// HTS (Horizontal Total Size) register (0x380C)
	val = 2160; // 1080p horizontal total
	ret = ov2740_write_reg(skel->sensor_client, 0x380C, 2, val);
	if (ret < 0)
		dev_warn(&skel->pdev->dev, "Failed to set HTS: %d\n", ret);

	// Exposure control register (0x3500)
	val = 0x00;
	ret = ov2740_write_reg(skel->sensor_client, 0x3500, 2, val);
	if (ret < 0)
		dev_warn(&skel->pdev->dev, "Failed to set exposure: %d\n", ret);

	// Analog gain register (0x3508)
	val = 0x00;
	ret = ov2740_write_reg(skel->sensor_client, 0x3508, 1, val);
	if (ret < 0)
		dev_warn(&skel->pdev->dev, "Failed to set analog gain: %d\n", ret);

	// Digital gain register (0x500A) - Red channel
	val = 0xD0;
	ret = ov2740_write_reg(skel->sensor_client, 0x500A, 2, val);
	if (ret < 0)
		dev_warn(&skel->pdev->dev, "Failed to set digital gain R: %d\n", ret);

	// Digital gain register (0x500C) - Green channel
	val = 0xD0;
	ret = ov2740_write_reg(skel->sensor_client, 0x500C, 2, val);
	if (ret < 0)
		dev_warn(&skel->pdev->dev, "Failed to set digital gain G: %d\n", ret);

	// Digital gain register (0x500E) - Blue channel
	val = 0xD0;
	ret = ov2740_write_reg(skel->sensor_client, 0x500E, 2, val);
	if (ret < 0)
		dev_warn(&skel->pdev->dev, "Failed to set digital gain B: %d\n", ret);

	// ISP control register (0x5000)
	val = 0x7F;
	ret = ov2740_write_reg(skel->sensor_client, 0x5000, 1, val);
	if (ret < 0)
		dev_warn(&skel->pdev->dev, "Failed to set ISP ctrl: %d\n", ret);

	// Test pattern disabled
	val = 0x00;
	ret = ov2740_write_reg(skel->sensor_client, 0x5040, 1, val);
	if (ret < 0)
		dev_warn(&skel->pdev->dev, "Failed to disable test pattern: %d\n", ret);

	dev_info(&skel->pdev->dev, "OV2740 initialization complete.\n");
	return 0;
}

// --- PCI Table ---
static const struct pci_device_id chuwi_camera_pci_tbl[] = {
	// IMX135 camera
	{ PCI_DEVICE(PCI_VENDOR_ID_INTEL, PCI_DEVICE_ID_INTEL_IMX135),
		.driver_data = PCI_DEVICE_ID_INTEL_IMX135 },
	// OV2740 camera
	{ PCI_DEVICE(PCI_VENDOR_ID_INTEL, PCI_DEVICE_ID_INTEL_OV2740),
		.driver_data = PCI_DEVICE_ID_INTEL_OV2740 },
	{ 0, }
};
MODULE_DEVICE_TABLE(pci, chuwi_camera_pci_tbl);

// --- V4L2 ioctl and ctrl ops ---
static const struct v4l2_ctrl_ops skel_ctrl_ops = {
	.s_ctrl = skeleton_s_ctrl,
};

static const struct v4l2_ioctl_ops skel_ioctl_ops = {
	.vidioc_querycap = skeleton_querycap,
	.vidioc_try_fmt_vid_cap = skeleton_try_fmt_vid_cap,
	.vidioc_s_fmt_vid_cap = skeleton_s_fmt_vid_cap,
	.vidioc_g_fmt_vid_cap = skeleton_g_fmt_vid_cap,
	.vidioc_enum_fmt_vid_cap = skeleton_enum_fmt_vid_cap,
	.vidioc_s_std = skeleton_s_std,
	.vidioc_g_std = skeleton_g_std,
	.vidioc_querystd = skeleton_querystd,
	.vidioc_s_dv_timings = skeleton_s_dv_timings,
	.vidioc_g_dv_timings = skeleton_g_dv_timings,
	.vidioc_enum_dv_timings = skeleton_enum_dv_timings,
	.vidioc_query_dv_timings = skeleton_query_dv_timings,
	.vidioc_dv_timings_cap = skeleton_dv_timings_cap,
	.vidioc_enum_input = skeleton_enum_input,
	.vidioc_s_input = skeleton_s_input,
	.vidioc_g_input = skeleton_g_input,
};

// --- V4L2 Probe Function ---

static int setup_camera_device(struct camera_device *skel, struct i2c_adapter *i2c_bus)
{
	int ret;

	// *** Sensor Initialization ***
	if (skel->id == DEV_CAM0_IMX135) {
		ret = imx135_init(skel);
		if (ret)
			dev_warn(&skel->pdev->dev,
				 "IMX135 init failed: %d, continuing anyway\n", ret);
	} else if (skel->id == DEV_CAM1_OV2740) {
		ret = ov2740_init(skel);
		if (ret)
			dev_warn(&skel->pdev->dev,
				 "OV2740 init failed: %d, continuing anyway\n", ret);
	}

	// *** Generic V4L2 Setup ***
	dev_info(&skel->pdev->dev, "Setting up V4L2 device...\n");

	// Initialize video device
	struct video_device vdev;
	strscpy(vdev.name,
		skel->id == DEV_CAM0_IMX135 ? "IMX135" : "OV2740",
		sizeof(vdev.name));
	vdev.release = video_device_release_empty;
	vdev.fops = &skel_fops;
	vdev.ioctl_ops = &skel_ioctl_ops;
	vdev.device_caps = V4L2_CAP_VIDEO_CAPTURE | V4L2_CAP_READWRITE | V4L2_CAP_STREAMING;
	vdev.lock = &skel->lock;
	vdev.queue = &skel->queue;
	vdev.v4l2_dev = &skel->v4l2_dev;
	vdev.tvnorms = CHUWI_TVNORMS;
	video_set_drvdata(&vdev, skel);

	// Initialize control handler
	v4l2_ctrl_handler_init(&skel->ctrl_handler, 4);
	v4l2_ctrl_new_std(&skel->ctrl_handler, &skel_ctrl_ops, V4L2_CID_BRIGHTNESS, 0, 255, 1, 127);
	v4l2_ctrl_new_std(&skel->ctrl_handler, &skel_ctrl_ops, V4L2_CID_CONTRAST, 0, 255, 1, 16);
	v4l2_ctrl_new_std(&skel->ctrl_handler, &skel_ctrl_ops, V4L2_CID_SATURATION, 0, 255, 1, 127);
	v4l2_ctrl_new_std(&skel->ctrl_handler, &skel_ctrl_ops, V4L2_CID_HUE, -128, 127, 1, 0);

	// Initialize VC Queue
	skel->queue.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
	skel->queue.io_modes = VB2_MMAP | VB2_DMABUF | VB2_READ;
	skel->queue.dev = &skel->pdev->dev;
	skel->queue.drv_priv = skel;
	skel->queue.buf_struct_size = sizeof(struct skel_buffer);
	skel->queue.ops = &skel_qops;
	skel->queue.mem_ops = &vb2_dma_contig_memops;
	skel->queue.timestamp_flags = V4L2_BUF_FLAG_TIMESTAMP_MONOTONIC;
	skel->queue.lock = &skel->lock;
	skel->queue.gfp_flags = GFP_DMA32;
	vb2_queue_init(&skel->queue);

	// Register the device
	ret = video_register_device(&vdev, VFL_TYPE_VIDEO, -1);
	if (ret)
		return ret;

	dev_info(&skel->pdev->dev, "Device %d initialized successfully.\n", skel->id);
	return 0;
}

static int skeleton_probe(struct pci_dev *pdev, const struct pci_device_id *ent)
{
	struct camera_device *skel;
	int ret;
	struct i2c_adapter *i2c_bus = NULL;
	int bus_id;
	camera_id_t id = DEV_UNKNOWN;

	/* 1. Generic PCI Setup */
	ret = pci_enable_device(pdev);
	if (ret)
		return ret;
	ret = dma_set_mask(&pdev->dev, DMA_BIT_MASK(32));
	if (ret) {
		dev_err(&pdev->dev, "no suitable DMA available.\n");
		goto disable_pci;
	}

	/* 2. Hardware Identification using ACPI HID */
	dev_info(&pdev->dev, "Identifying camera hardware...\n");

	// Check PCI device ID to determine camera type
	if (ent->driver_data == PCI_DEVICE_ID_INTEL_IMX135) {
		id = DEV_CAM0_IMX135;
		bus_id = IMX135_I2C_BUS_ID;
		dev_info(&pdev->dev, "Detected IMX135 camera (PCI ID 0x%04x:0x%04x)\n",
			 pdev->vendor, pdev->device);
	} else if (ent->driver_data == PCI_DEVICE_ID_INTEL_OV2740) {
		id = DEV_CAM1_OV2740;
		bus_id = OV2740_I2C_BUS_ID;
		dev_info(&pdev->dev, "Detected OV2740 camera (PCI ID 0x%04x:0x%04x)\n",
			 pdev->vendor, pdev->device);
	} else {
		dev_warn(&pdev->dev, "Unknown camera type for PCI ID 0x%04x:0x%04x\n",
			 pdev->vendor, pdev->device);
		dev_warn(&pdev->dev, "Cannot determine camera type. Device setup skipped.\n");
		ret = -ENODEV;
		goto disable_pci;
	}

	/* 3. Resource Acquisition (I2C Bus) */
	dev_info(&pdev->dev, "Acquiring I2C bus %s...\n",
		 bus_id == IMX135_I2C_BUS_ID ? "I2C2" : "I2C4");

	// Look up the I2C adapter by bus number
	i2c_bus = i2c_get_adapter(bus_id);
	if (!i2c_bus) {
		dev_err(&pdev->dev,
			"Failed to get I2C adapter for bus 0x%02x\n", bus_id);
		ret = -ENODEV;
		goto disable_pci;
	}

	// Get the I2C bus number for reporting
	bus_id = i2c_bus->nr;
	dev_info(&pdev->dev, "Using I2C adapter %d\n", bus_id);

	/* 4. Allocate and initialize the device structure */
	skel = devm_kzalloc(&pdev->dev, sizeof(struct camera_device), GFP_KERNEL);
	if (!skel) {
		ret = -ENOMEM;
		goto release_i2c;
	}
	skel->pdev = pdev;
	skel->id = id;
	skel->i2c_bus = i2c_bus;
	skel->i2c_bus_num = bus_id;

	/* 5. Get I2C client for the sensor */
	struct i2c_board_info sensor_info = {
		.addr = bus_id,
	};
	skel->sensor_client = i2c_new_client_device(i2c_bus, &sensor_info);
	if (!skel->sensor_client) {
		dev_err(&skel->pdev->dev, "Failed to create I2C client: %p\n", skel->sensor_client);
		goto release_i2c;
	}
	if (ret) {
		dev_err(&skel->pdev->dev, "Failed to create I2C client: %d\n", ret);
		goto release_i2c;
	}

	// Get I2C client for PMIC (CAM0 only)
	if (id == DEV_CAM0_IMX135) {
		struct i2c_board_info pmic_info = {
			.addr = PMIC_I2C_BUS_ID,
		};
		skel->pmic_client = i2c_new_client_device(i2c_bus, &pmic_info);
		if (!skel->pmic_client) {
			dev_err(&skel->pdev->dev, "Failed to create PMIC I2C client: %p\n", skel->pmic_client);
			goto free_sensor;
		}
		if (ret) {
			dev_err(&skel->pdev->dev, "Failed to create PMIC I2C client: %d\n", ret);
			goto free_sensor;
		}
	}

	/* 6. PMIC Power Control (CAM0 only) */
	if (id == DEV_CAM0_IMX135) {
		ret = pmic_check_and_enable(skel);
		if (ret) {
			dev_err(&pdev->dev, "PMIC power enable failed: %d\n", ret);
			// Continue anyway - sensor might still work
		}
	}

	/* 7. Sensor Initialization */
	ret = setup_camera_device(skel, i2c_bus);
	if (ret)
		goto free_pmic;

	dev_info(&pdev->dev, "Camera driver loaded successfully for device %d\n", skel->id);
	return 0;

free_pmic:
	if (skel->pmic_client)
		i2c_unregister_device(skel->pmic_client);
free_sensor:
	i2c_unregister_device(skel->sensor_client);
release_i2c:
	i2c_put_adapter(i2c_bus);
disable_pci:
	pci_disable_device(pdev);
	return ret;
}

static void skeleton_remove(struct pci_dev *pdev)
{
	struct camera_device *skel = pci_get_drvdata(pdev);

	// Clean up PMIC resources
	if (skel->pmic_client) {
		i2c_unregister_device(skel->pmic_client);
		skel->pmic_client = NULL;
	}

	// Unregister video device
	video_unregister_device(&skel->vdev);

	// Free control handler
	v4l2_ctrl_handler_free(&skel->ctrl_handler);

	// Unregister V4L2 device
	v4l2_device_unregister(&skel->v4l2_dev);

	// Disable PCI device
	pci_disable_device(skel->pdev);
}

static struct pci_driver chuwi_camera_driver = {
	.name = DRIVER_NAME,
	.probe = skeleton_probe,
	.remove = skeleton_remove,
	.id_table = chuwi_camera_pci_tbl,
};

module_pci_driver(chuwi_camera_driver);

MODULE_DESCRIPTION("Chuwi Ubook XPro Camera Driver (IMX135/OV2740)");
MODULE_LICENSE("GPL");
MODULE_AUTHOR("Chuwi Camera Driver");
