# Brute Forcing BIOS password

## Resources

Worst slop here: <https://github.com/sunofva/HP-ADMIN-UEFI-PASSWORD-BRUTEFORCE/blob/main/main.rs>
Anyways provides elements to solve the puzzle.

<https://gist.github.com/Chester-Gillon/9d81150df134783cab87bf01a0d0f0ea>

Tools from HP <https://ftp.ext.hp.com/pub/caps-softpaq/cmit/HP_BCU.html>
<https://ftp.hp.com/pub/caps-softpaq/cmit/whitepapers/BIOS_Configuration_Utility_User_Guide.pdf>

## Elements to brute-force

* HPQPswd64.exe /s /p"password" /fpass
* BIOSConfigUtil64.exe /GetConfig /cspassfile:pass (slowed?)

John The Ripper

<https://github.com/openwall/john>
