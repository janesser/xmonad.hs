# Lenovo Yoga 510 installation

it is a [AMD A9-9410](https://cpu-benchmark.org/cpu/amd-a9-9410/) Soc with integrated R5 STONEY. R5 Stoney has 2 pipelines working with 192 shader units.

## Known issues

## video playback performance

videos playback will take more than two seconds to start and may be rambling during playback.

e.g. on youtube, playback was fine, though pre-buffering fell short several times.

### fan

The A9 takes clocking to the edge which results in the fan, responding to almost any interaction.

    # workaround
    cpufreq-setall.sh powersave

### amdgpu

No GPU monitoring by btop.
rcm-smi won't recognize this hardware.

In `sudo dmesg|grep amdgpu` unclear if firmware is loaded or not.
"stoney not supported by kfd" is a very famous search term.

    [   19.300548] [drm] amdgpu kernel modesetting enabled.
    [   19.317811] amdgpu: Virtual CRAT table created for CPU
    [   19.319449] amdgpu: Topology: Add CPU node
    [   19.338845] amdgpu 0000:00:01.0: amdgpu: Fetched VBIOS from ROM BAR
    [   19.338873] amdgpu: ATOM BIOS: 113-C91400-005
    [   19.341474] kfd kfd: amdgpu: STONEY  not supported in kfd
    [   19.354149] amdgpu 0000:00:01.0: vgaarb: deactivate vga console
    [   19.354161] amdgpu 0000:00:01.0: amdgpu: Trusted Memory Zone (TMZ) feature not supported
    [   19.354220] amdgpu 0000:00:01.0: amdgpu: VRAM: 512M 0x000000F400000000 - 0x000000F41FFFFFFF (512M used)
    [   19.354225] amdgpu 0000:00:01.0: amdgpu: GART: 1024M 0x000000FF00000000 - 0x000000FF3FFFFFFF
    [   19.359527] [drm] amdgpu: 512M of VRAM memory ready
    [   19.359539] [drm] amdgpu: 3696M of GTT memory ready.
    [   19.372681] amdgpu: hwmgr_sw_init smu backed is smu8_smu
    [   19.392027] amdgpu: smu version 26.17.00
    [   19.444204] [drm:bios_parser_get_connector_id [amdgpu]] *ERROR* Can't find connector id 3 in connector table of size 3.
    [   19.447393] [drm:bios_parser_get_connector_id [amdgpu]] *ERROR* Can't find connector id 4 in connector table of size 3.
    [   19.449139] [drm:bios_parser_get_connector_id [amdgpu]] *ERROR* Can't find connector id 5 in connector table of size 3.
    [   19.451085] [drm:bios_parser_get_connector_id [amdgpu]] *ERROR* Can't find connector id 6 in connector table of size 3.
    [   19.452679] [drm:bios_parser_get_connector_id [amdgpu]] *ERROR* Can't find connector id 7 in connector table of size 3.
    [   19.454378] [drm:bios_parser_get_connector_id [amdgpu]] *ERROR* Can't find connector id 8 in connector table of size 3.
    [   19.455904] [drm:bios_parser_get_connector_id [amdgpu]] *ERROR* Can't find connector id 9 in connector table of size 3.
    [   19.457559] [drm:bios_parser_get_connector_id [amdgpu]] *ERROR* Can't find connector id 10 in connector table of size 3.
    [   19.459231] [drm:bios_parser_get_connector_id [amdgpu]] *ERROR* Can't find connector id 11 in connector table of size 3.
    [   19.460904] [drm:bios_parser_get_connector_id [amdgpu]] *ERROR* Can't find connector id 12 in connector table of size 3.
    [   19.462526] [drm:bios_parser_get_connector_id [amdgpu]] *ERROR* Can't find connector id 13 in connector table of size 3.
    [   19.477677] snd_hda_intel 0000:00:01.1: bound 0000:00:01.0 (ops amdgpu_dm_audio_component_bind_ops [amdgpu])
    [   19.701900] amdgpu 0000:00:01.0: amdgpu: SE 1, SH per SE 1, CU per SH 3, active_cu_number 3
    [   19.703529] amdgpu: pp_dpm_get_sclk_od was not implemented.
    [   19.703540] amdgpu: pp_dpm_get_mclk_od was not implemented.
    [   19.704940] [drm] Initialized amdgpu 3.57.0 20150101 for 0000:00:01.0 on minor 1
    [   19.726066] fbcon: amdgpudrmfb (fb0) is primary device
    [   19.726079] amdgpu 0000:00:01.0: [drm] fb0: amdgpudrmfb frame buffer device
    [  186.338992] amdgpu 0000:00:01.0: amdgpu: Disabling VM faults because of PRT request!
    [ 6606.885886] amdgpu: smu version 26.17.00
    [ 6607.091004] amdgpu 0000:00:01.0: [drm:amdgpu_ring_test_helper [amdgpu]] *ERROR* ring comp_1.0.1 test failed (-110)
    [ 6607.266307] amdgpu 0000:00:01.0: [drm:amdgpu_ring_test_helper [amdgpu]] *ERROR* ring comp_1.0.3 test failed (-110)

`Xorg` logs seem just fine. Also `glxinfo -B` reports direct rendering enabled.

    name of display: :0
    display: :0  screen: 0
    direct rendering: Yes
    Extended renderer info (GLX_MESA_query_renderer):
        Vendor: AMD (0x1002)
        Device: AMD Radeon R5 Graphics (radeonsi, stoney, ACO, DRM 3.57, 6.8.0-87-generic) (0x98e4)
        Version: 25.0.7
        Accelerated: yes
        Video memory: 512MB
        Unified memory: no
        Preferred profile: core (0x1)
        Max core profile version: 4.5
        Max compat profile version: 4.5
        Max GLES1 profile version: 1.1
        Max GLES[23] profile version: 3.2
    Memory info (GL_ATI_meminfo):
        VBO free memory - total: 79 MB, largest block: 79 MB
        VBO free aux. memory - total: 3407 MB, largest block: 3407 MB
        Texture free memory - total: 79 MB, largest block: 79 MB
        Texture free aux. memory - total: 3407 MB, largest block: 3407 MB
        Renderbuffer free memory - total: 79 MB, largest block: 79 MB
        Renderbuffer free aux. memory - total: 3407 MB, largest block: 3407 MB
    Memory info (GL_NVX_gpu_memory_info):
        Dedicated video memory: 512 MB
        Total available memory: 4208 MB
        Currently available dedicated video memory: 79 MB
    OpenGL vendor string: AMD
    OpenGL renderer string: AMD Radeon R5 Graphics (radeonsi, stoney, ACO, DRM 3.57, 6.8.0-87-generic)
    OpenGL core profile version string: 4.5 (Core Profile) Mesa 25.0.7-0ubuntu0.24.04.2
    OpenGL core profile shading language version string: 4.50
    OpenGL core profile context flags: (none)
    OpenGL core profile profile mask: core profile

    OpenGL version string: 4.5 (Compatibility Profile) Mesa 25.0.7-0ubuntu0.24.04.2
    OpenGL shading language version string: 4.50
    OpenGL context flags: (none)
    OpenGL profile mask: compatibility profile

    OpenGL ES profile version string: OpenGL ES 3.2 Mesa 25.0.7-0ubuntu0.24.04.2
    OpenGL ES profile shading language version string: OpenGL ES GLSL ES 3.20

### Fixing Attempt #1

<https://gist.github.com/vantrong291/16286117d45dceae7aba4d7b712c8a2f>
