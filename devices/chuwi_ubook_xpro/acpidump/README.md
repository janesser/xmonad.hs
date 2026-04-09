# chuwi ubook xpro acpidump

## extracting

    sudo acpidump > ubook_xpro_acpidump.txt
    acpixtract -a ubook_xpro_acpidump.txt
    iasl -d DSDT.dat

## interpretation of `dsdt.dsl`

See cam0

    Scope (_SB.PCI0.I2C2)
        {
            Device (CAM0)
            {
                Name (_ADR, Zero)  // _ADR: Address
                Name (_HID, "INT3471")  // _HID: Hardware ID
                Name (_CID, "INT3471")  // _CID: Compatible ID
                Name (_DDN, "IMX135-CRDG2")  // _DDN: DOS Device Name

See cam1

    Scope (_SB.PCI0.I2C4)
        {
            Device (CAM1)
            {
                Name (_ADR, Zero)  // _ADR: Address
                Name (_HID, "INT3474")  // _HID: Hardware ID
                Name (_CID, "INT3474")  // _CID: Compatible ID
                Name (_DDN, "OV2740-CRDG2")  // _DDN: DOS Device Name

## Trying with HWE kernel

    [    3.566418] No Arguments are initialized for method [_GTF]
    [    3.571725] ACPI Error: Aborting method \_SB.PCI0.SAT0.PRT1._GTF due to previous error (AE_NOT_FOUND) (20250404/psparse-529)

Applying `acpi_os_name` and `acpi_osi`

    sudo nano /etc/default/grub

        GRUB_CMDLINE_LINUX="acpi_os_name=\"Windows 2015\" acpi_osi=\"Windows 2015\""

    sudo update-grub

As a result additional `sensors` appear.

    $ sensors

        iwlwifi_1-virtual-0
        Adapter: Virtual device
        temp1:        +51.0°C  

        BAT0-acpi-0
        Adapter: ACPI interface
        in0:           8.69 V  
        power1:        0.00 W  

        coretemp-isa-0000
        Adapter: ISA adapter
        Package id 0:  +47.0°C  (high = +100.0°C, crit = +100.0°C)
        Core 0:        +46.0°C  (high = +100.0°C, crit = +100.0°C)
        Core 1:        +47.0°C  (high = +100.0°C, crit = +100.0°C)

        pch_skylake-virtual-0
        Adapter: Virtual device
        temp1:        +42.5°C  

        acpitz-acpi-0
        Adapter: ACPI interface
        temp1:        +27.8°C  
        temp2:        +27.8°C
