// SPDX-License-Identifier: GPL-2.0-only
/*
 * This is a V4L2 PCI Skeleton Driver, adapted for Chuwi ubook xpro cameras (CAM0: IMX135/I2C2, CAM1: OV2740/I2C4).
 * Based on v4l2-pci-skeleton.c.
 *
 * Hardware details derived from DSDT:
 * CAM0 (IMX135-CRDG2): I2C2 bus (PCI0.I2C2). PMIC at 0x004C on I2C2.
 * CAM1 (OV2740-CRDG2): I2C4 bus (PCI0.I2C4).
 *
 * The driver now uses a device abstraction to handle multiple cameras.
 *
 * Copyright 2024 Coding Assistant
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
#include <media/v4l2-ctrl.h>
#include <media/v4l2-event.h>
#include <media/videobuf2-v4l2.h>
#include <media/videobuf2-dma-contig.h>
#include <linux/i2c.h>
#include <linux/platform_device.h>

#define DRIVER_NAME "chuwi_camera_driver"
#define MAX_DEVICES 2

// --- Device Abstraction ---

typedef enum {
	DEV_UNKNOWN,
	DEV_CAM0_IMX135,
	DEV_CAM1_OV2740,
} camera_id_t;

// Specific bus/resource mapping based on DSDT analysis
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
	struct i2c_client *pmic_client; // For power control
	unsigned i2c_bus;             // The bus used by this camera
};

// --- Device Specific Definitions ---

#define IMX135_I2C_BUS 0x0010 // I2C2 from DSDT
#define OV2740_I2C_BUS 0x0036 // I2C4 from DSDT

// Global state for power control reference
static struct camera_device *global_pmic_cam0 = NULL;


// --- Internal Structures (Mostly copied/simplified from skeleton) ---

struct skel_buffer {
	struct vb2_v4l2_buffer vb;
	struct list_head list;
};

static inline struct skel_buffer *to_skel_buffer(struct vb2_v4l2_buffer *vbuf)
{
	return container_of(vbuf, struct skel_buffer, vb);
}

// --- PCI Table (Generic for now, will be expanded) ---
static const struct pci_device_id chuwi_camera_pci_tbl[] = {
	// { PCI_DEVICE(PCI_VENDOR_ID_, PCI_DEVICE_ID_CAN_SENSOR) },
	// { PCI_DEVICE(PCI_VENDOR_ID_, PCI_DEVICE_ID_PMIC) },
	{ 0, }
};
MODULE_DEVICE_TABLE(pci, chuwi_camera_pci_tbl);

/*
 * HDTV: this structure has the capabilities of the HDTV receiver.
 * It is used to constrain the huge list of possible formats based
 * upon the hardware capabilities.
 */
static const struct v4l2_dv_timings_cap skel_timings_cap = {
	.type = V4L2_DV_BT_656_1120,
	/* keep this initialization for compatibility with GCC < 4.4.6 */
	.reserved = { 0 },
	V4L2_INIT_BT_TIMINGS(
		720, 1920,		/* min/max width */
		480, 1080,		/* min/max height */
		27000000, 74250000,	/* min/max pixelclock*/
		V4L2_DV_BT_STD_CEA861,	/* Supported standards */
		/* capabilities */
		V4L2_DV_BT_CAP_INTERLACED | V4L2_DV_BT_CAP_PROGRESSIVE
	)
};

/*
 * Supported SDTV standards. This does the same job as skel_timings_cap, but
 * for standard TV formats.
 */
#define CHUWI_TVNORMS V4L2_STD_ALL

// --- V4L2 Helper Functions (Mostly copied) ---

/* Interrupt handler: typically interrupts happen after a new frame has been
 * captured. It is the job of the handler to remove the new frame from the
 * internal list and give it back to the vb2 framework, updating the sequence
 * counter, field and timestamp at the same time.
 */
static irqreturn_t skeleton_irq(int irq, void *dev_id)
{
	struct camera_device *skel = dev_id;
// ...
	return IRQ_HANDLED;
}

/*
 * Setup the constraints of the queue: besides setting the number of planes
 * per buffer and the size and allocation context of each plane, it also
 * checks if sufficient buffers have been allocated. Usually 3 is a good
 * minimum number: many DMA engines need a minimum of 2 buffers in the
 * queue and you need to have another available for userspace processing.
 */
static int queue_setup(struct vb2_queue *vq,
		       unsigned int *nbuffers, unsigned int *nplanes,
		       unsigned int sizes[], struct device *alloc_devs[])
{
	struct camera_device *skel = (struct camera_device *)vb2_get_drv_priv(vq);

	skel->field = skel->format.field;
	if (skel->field == V4L2_FIELD_ALTERNATE) {
		/*
		 * You cannot use read() with FIELD_ALTERNATE since the field
		 * information (TOP/BOTTOM) cannot be passed back to the user.
		 */
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

/*
 * Prepare the buffer for queueing to the DMA engine: check and set the
 * payload size.
 */
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

/*
 * Queue this buffer to the DMA engine.
 */
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

/*
 * Start streaming. First check if the minimum number of buffers have been
 * queued. If not, then return -ENOBUFS and the vb2 framework will call
 * this function again the next time a buffer has been queued until enough
 * buffers are available to actually start the DMA engine.
 */
static int start_streaming(struct vb2_queue *vq, unsigned int count)
{
	struct camera_device *skel = (struct camera_device *)vb2_get_drv_priv(vq);
	int ret = 0;

	skel->sequence = 0;

	/* TODO: start DMA */

	if (ret) {
		/*
		 * In case of an error, return all active buffers to the
		 * QUEUED state
		 */
		return_all_buffers(skel, VB2_BUF_STATE_QUEUED);
	}
	return ret;
}

/*
 * Stop the DMA engine. Any remaining buffers in the DMA queue are dequeued
 * and passed on to the vb2 framework marked as STATE_ERROR.
 */
static void stop_streaming(struct vb2_queue *vq)
{
	struct camera_device *skel = (struct camera_device *)vb2_get_drv_priv(vq);

	/* TODO: stop DMA */

	/* Release all active buffers */
	return_all_buffers(skel, VB2_BUF_STATE_ERROR);
}

/*
 * The vb2 queue ops. Note that since q->lock is set we can use the standard
 * vb2_ops_wait_prepare/finish helper functions. If q->lock would be NULL,
 * then this driver would have to provide these ops.
 */
static const struct vb2_ops skel_qops = {
	.queue_setup		= queue_setup,
	.buf_prepare		= buffer_prepare,
	.buf_queue		= buffer_queue,
	.start_streaming	= start_streaming,
	.stop_streaming		= stop_streaming,
	.wait_prepare		= vb2_ops_wait_prepare,
	.wait_finish		= vb2_ops_wait_finish,
};

/*
 * Required ioctl querycap. Note that the version field is prefilled with
 * the version of the kernel.
 */
static int skeleton_querycap(struct file *file, void *priv,
			     struct v4l2_capability *cap)
{
	struct camera_device *skel = (struct camera_device *)video_drvdata(file);

	strlcpy(cap->driver, DRIVER_NAME, sizeof(cap->driver));
	strlcpy(cap->card, skel->id == DEV_CAM0_IMX135 ? "IMX135" : "OV2740", sizeof(cap->card));
	snprintf(cap->bus_info, sizeof(cap->bus_info), "I2C:%u", skel->i2c_bus);
	return 0;
}

/*
 * Helper function to check and correct struct v4l2_pix_format. It's used
 * not only in VIDIOC_TRY/S_FMT, but also elsewhere if changes to the SDTV
 * standard, HDTV timings or the video input would require updating the
 * current format.
 */
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

	/*
	 * The YUYV format is four bytes for every two pixels, so bytesperline
	 * is width * 2.
	 */
	pix->bytesperline = pix->width * 2;
	pix->sizeimage = pix->bytesperline * pix->height;
	pix->priv = 0;
}

static int skeleton_try_fmt_vid_cap(struct file *file, void *priv,
				    struct v4l2_format *f)
{
	struct camera_device *skel = (struct camera_device *)video_drvdata(file);

	/*
	 * Due to historical reasons providing try_fmt with an unsupported
	 * pixelformat will return -EINVAL for video receivers. Webcam drivers,
	 * however, will silently correct the pixelformat. Some video capture
	 * applications rely on this behavior...
	 */
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

	/*
	 * It is not allowed to change the format while buffers for use with
	 * streaming have already been allocated.
	 */
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

	/* S_STD is not supported on the HDMI input */
	if (skel->input)
		return -ENODATA;

	/*
	 * No change, so just return. Some applications call S_STD again after
	 * the buffers for streaming have been set up, so we have to allow for
	 * this behavior.
	 */
	if (std == skel->std)
		return 0;

	/*
	 * Changing the standard implies a format change, which is not allowed
	 * while buffers for use with streaming have already been allocated.
	 */
	if (vb2_is_busy(&skel->queue))
		return -EBUSY;

	/* TODO: handle changing std */

	skel->std = std;

	/* Update the internal format */
	skeleton_fill_pix_format(skel, &skel->format);
	return 0;
}

static int skeleton_g_std(struct file *file, void *priv, v4l2_std_id *std)
{
	struct camera_device *skel = (struct camera_device *)video_drvdata(file);

	/* G_STD is not supported on the HDMI input */
	if (skel->input)
		return -ENODATA;

	*std = skel->std;
	return 0;
}

/*
 * Query the current standard as seen by the hardware.
 */
static int skeleton_querystd(struct file *file, void *priv, v4l2_std_id *std)
{
	struct camera_device *skel = (struct camera_device *)video_drvdata(file);

	/* QUERY_STD is not supported on the HDMI input */
	if (skel->input)
		return -ENODATA;

#ifdef TODO
	/*
	 * Query currently seen standard.
	 */
	return 0;
#endif
	return 0;
}

static int skeleton_s_dv_timings(struct file *file, void *_fh,
				 struct v4l2_dv_timings *timings)
{
	struct camera_device *skel = (struct camera_device *)video_drvdata(file);

	/* S_DV_TIMINGS is not supported on the S-Video input */
	if (skel->input == 0)
		return -ENODATA;

	/* Quick sanity check */
	if (!v4l2_valid_dv_timings(timings, &skel_timings_cap, NULL, NULL))
		return -EINVAL;

	/* Check if the timings are part of the CEA-861 timings. */
	if (!v4l2_find_dv_timings_cap(timings, &skel_timings_cap,
				      0, NULL, NULL))
		return -EINVAL;

	/* Return 0 if the new timings are the same as the current timings. */
	if (v4l2_match_dv_timings(timings, &skel->timings, 0, false))
		return 0;

	/*
	 * Changing the timings implies a format change, which is not allowed
	 * while buffers for use with streaming have already been allocated.
	 */
	if (vb2_is_busy(&skel->queue))
		return -EBUSY;

	/* TODO: Configure new timings */

	skel->timings = *timings;

	/* Update the internal format */
	skeleton_fill_pix_format(skel, &skel->format);
	return 0;
}

static int skeleton_g_dv_timings(struct file *file, void *_fh,
				 struct v4l2_dv_timings *timings)
{
	struct camera_device *skel = (struct camera_device *)video_drvdata(file);

	/* G_DV_TIMINGS is not supported on the S-Video input */
	if (skel->input == 0)
		return -ENODATA;

	*timings = skel->timings;
	return 0;
}

static int skeleton_enum_dv_timings(struct file *file, void *_fh,
				    struct v4l2_enum_dv_timings *timings)
{
	struct camera_device *skel = (struct camera_device *)video_drvdata(file);

	/* ENUM_DV_TIMINGS is not supported on the S-Video input */
	if (skel->input == 0)
		return -ENODATA;

	return v4l2_enum_dv_timings_cap(timings, &skel_timings_cap,
					NULL, NULL);
}

/*
 * Query the current timings as seen by the hardware.
 */
static int skeleton_query_dv_timings(struct file *file, void *_fh,
				     struct v4l2_dv_timings *timings)
{
	struct camera_device *skel = (struct camera_device *)video_drvdata(file);

	/* QUERY_DV_TIMINGS is not supported on the S-Video input */
	if (skel->input == 0)
		return -ENODATA;

#ifdef TODO
	/*
	 * Query currently seen timings.
	 */
	return 0;
#endif
	return 0;
}

static int skeleton_dv_timings_cap(struct file *file, void *fh,
				   struct v4l2_dv_timings_cap *cap)
{
	struct camera_device *skel = (struct camera_device *)video_drvdata(file);

	/* DV_TIMINGS_CAP is not supported on the S-Video input */
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
		strlcpy(i->name, skel->id == DEV_CAM0_IMX135 ? "IMX135" : "OV2740", sizeof(i->name));
		i->capabilities = V4L2_IN_CAP_STD;
	} else {
		strlcpy(i->name, skel->id == DEV_CAM0_IMX135 ? "IMX135" : "OV2740", sizeof(i->name));
		i->capabilities = V4L2_IN_CAP_DV_TIMINGS;
	}
	return 0;
}

static int skeleton_s_input(struct file *file, void *priv, unsigned int i)
{
	struct camera_device *skel = (struct camera_device *)video_drvdata(file);

	if (i > MAX_DEVICES)
		return -EINVAL;

	/*
	 * Changing the input implies a format change, which is not allowed
	 * while buffers for use with streaming have already been allocated.
	 */
	if (vb2_is_busy(&skel->queue))
		return -EBUSY;

	skel->input = i;
	/*
	 * Update tvnorms.
	 */
	skel->vdev.tvnorms = i ? 0 : CHUWI_TVNORMS;

	/* Update the internal format */
	skeleton_fill_pix_format(skel, &skel->format);
	return 0;
}

static int skeleton_g_input(struct file *file, void *priv, unsigned int *i)
{
	struct camera_device *skel = (struct camera_device *)video_drvdata(file);
	*i = skel->input;
	return 0;
}

/* The control handler. */
static int skeleton_s_ctrl(struct v4l2_ctrl *ctrl)
{
	/*struct camera_device *skel =
		container_of(ctrl->handler, struct camera_device, ctrl_handler);*/

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

/* ------------------------------------------------------------------
	File operations for the device
   ------------------------------------------------------------------*/

static const struct v4l2_file_operations skel_fops = {
	.owner = THIS_MODULE,
	.open = v4l2_fh_open,
	.release = vb2_fop_release,
	.unlocked_ioctl = video_ioctl2,
	.read = vb2_fop_read,
	.mmap = vb2_fop_mmap,
	.poll = vb2_fop_poll,
};

/*
 * The initial setup of this device instance.
 */
static int skeleton_probe(struct pci_dev *pdev, const struct pci_device_id *ent)
{
	struct camera_device *skel;
	int ret;
	struct i2c_adapter *i2c_bus;
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

	// 2. Device Identification (Crucial adaptation point)
	// In a real scenario, we would parse ACPI/DSDT resources here to identify the camera ID (CAM0/CAM1)
	// Based on the current scope, we assume the PCI ID dictates the device type.
	// For now, we will assume a generic ID and let the setup function decide based on PCI ID or other means.
	
	// !!! Placeholder: In a real implementation, complex ACPI/DSDT parsing would happen here to set 'id' !!!
	// For now, we will set ID based on an arbitrary check.
	// For example, if pdev->vendor == 0x1234 (IMX135 Vendor) -> DEV_CAM0_IMX135
	
	// Since we don't have PCI IDs, we must stop here and ask the user for the hardware ID mapping.
	// The current plan requires knowledge that I don't possess from the code context.

	dev_warn(&pdev->dev, "PCI ID mapping is unknown. Cannot determine camera type. Skipping device setup.\n");
	ret = -ENODEV;
	goto disable_pci;


	/* Allocate and initialize the device structure */
	skel = devm_kzalloc(&pdev->dev, sizeof(struct camera_device), GFP_KERNEL);
	if (!skel) {
		ret = -ENOMEM;
		goto disable_pci;
	}
	skel->pdev = pdev;

	// 3. Resource Acquisition (I2C Bus)
	// For a real driver, we'd use i2c_client_request_resource() based on bus_id.
	// For this skeleton, we simplify the resource acquisition logic for demonstration:
	i2c_bus = i2c_get_client(pdev->dev.platform_device); // Simplified resource mapping

	// 4. Device Setup and Initialization (delegated)
	// Since ID is DEV_UNKNOWN, this call will likely fail or be a stub.
	ret = setup_camera_device(skel, i2c_bus);
	if (ret)
		goto disable_pci;

	dev_info(&pdev->dev, "Camera driver loaded successfully for device %d\n", skel->id);
	return 0;

disable_pci:
	pci_disable_device(pdev);
	return ret;
}

static int setup_camera_device(struct camera_device *skel, struct i2c_adapter *i2c_bus)
{
	// This function handles the complex initialization sequence:
	// 1. PMIC power check (if applicable).
	// 2. V4L2 registration.
	// 3. VC Queue initialization.

	// For now, we will initialize the v4l2 device structure generically.
	// Specific sensor initialization (IMX135/OV2740) goes into dedicated helper functions
	// that would be called here.

	// *** TODO: Implement PMIC power control based on skel->id and skel->i2c_bus ***
	// Example for CAM0:
	// if (skel->id == DEV_CAM0_IMX135) {
	//     skel->pmic_client = i2c_client_alloc(skel->i2c_bus, 0x004C, ...);
	//     if (!skel->pmic_client) return -ENOMEM;
	//     if (pmic_check_and_enable(skel->pmic_client) != 0) return -EINVAL;
	// }

	// *** TODO: Implement specific sensor setup (IMX135/OV2740) logic ***

	// For now, we proceed with the generic V4L2 setup to demonstrate the framework.
	// In a final driver, the initialization steps below would be heavily dependent on the sensor.

	// Generic setup placeholders:
	struct video_device vdev;
	strlcpy(vdev.name, skel->id == DEV_CAM0_IMX135 ? "IMX135" : "OV2740", sizeof(vdev.name));
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
	skel->queue.min_buffers_needed = 2;
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


static void skeleton_remove(struct pci_dev *pdev)
{
	struct camera_device *skel = pci_get_drvdata(pdev);

	// *** TODO: Clean up PMIC resources ***
	// if (skel->pmic_client) { i2c_client_free(skel->pmic_client); }

	video_unregister_device(&skel->vdev);
	v4l2_ctrl_handler_free(&skel->ctrl_handler);
	v4l2_device_unregister(&skel->v4l2_dev);
	pci_disable_device(skel->pdev);
}

static struct pci_driver chuwi_camera_driver = {
	.name = DRIVER_NAME,
	.probe = skeleton_probe,
	.remove = skeleton_remove,
	.id_table = chuwi_camera_pci_tbl,
};

module_pci_driver(chuwi_camera_driver);
/*
 * ======================================================================
 * PMIC Power Management Functions (IMX135 Specific)
 * ======================================================================
 * These functions handle interaction with the PMIC at I2C address 0x004C.
 */

static int imx135_pmic_read(struct i2c_client *client, u8 reg, u8 *val)
{
	int ret = i2c_smbus_read_byte_data(client, reg, val);
	if (ret < 0) {
		dev_err(&client->dev, "PMIC read failed on reg 0x%x: %d\n", reg, ret);
	}
	return ret;
}

static int imx135_pmic_write(struct i2c_client *client, u8 reg, u8 val)
{
	int ret = i2c_smbus_write_byte_data(client, reg, val);
	if (ret < 0) {
		dev_err(&client->dev, "PMIC write failed on reg 0x%x: %d\n", reg, ret);
	}
	return ret;
}

/**
 * pmic_check_and_enable: Checks the power state of the IMX135 and enables it if necessary.
 * Assumes the PMIC state is reflected in a specific register/argument of the _DSM command.
 * For this implementation, we assume the PMIC requires a specific sequence or command
 * to transition state, and we check a designated status register (e.g., 0x01 for status).
 *
 * @client: Pointer to the PMIC I2C client.
 * @pmic_address: The specific register/command to check.
 * @power_on_cmd: The command/value to send to power on the camera.
 */
static int pmic_check_and_enable(struct i2c_client *client, u8 status_reg, u8 power_on_cmd)
{
	u8 status = 0;
	int ret;

	dev_info(&client->dev, "Checking PMIC state...\n");
	ret = imx135_pmic_read(client, status_reg, &status);
	if (ret != 0)
		return -EIO;

	/*
	 * Simulation of Arg2 check:
	 * In a real driver, we would parse the _DSM response structure (e.g., bitmask or specific value).
	 * Here, we simulate a power state check based on the status byte.
	 * Let's assume status == 0 means powered on, and status != 0 means off.
	 * We also assume power_on_cmd is the actual write command to initiate power up.
	 */
	if (status != 0x00) {
		dev_warn(&client->dev, "PMIC indicates camera is powered down (Status: 0x%x). Attempting to enable...\n", status);
		
		/* Send power-on command */
		ret = imx135_pmic_write(client, power_on_cmd, 0x01);
		if (ret < 0)
			return -EIO;

		// Wait briefly for power sequence to complete (Real driver would poll or wait for interrupt)
		msleep(10);

		// Re-check state after command
		if (imx135_pmic_read(client, status_reg, &status) != 0)
			return -EIO;
			
		if (status != 0x00) {
			dev_err(&client->dev, "PMIC failed to transition to power-on state after command (Status: 0x%x).\n", status);
			return -EIO;
		}
		dev_info(&client->dev, "PMIC successfully enabled.\n");
	} else {
		dev_info(&client->dev, "PMIC already powered on.\n");
	}

	return 0;
}
