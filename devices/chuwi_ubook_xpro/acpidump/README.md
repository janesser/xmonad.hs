# chuwi ubook xpro acpidump

## extracting

    sudo acpidump > ubook_xpro_acpidump.txt
    acpixtract -a ubook_xpro_acpidump.txt
    iasl -d DSDT.dat

## interpretation

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
