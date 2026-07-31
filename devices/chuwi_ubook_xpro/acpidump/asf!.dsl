/*
 * Intel ACPI Component Architecture
 * AML/ASL+ Disassembler version 20230628 (64-bit version)
 * Copyright (c) 2000 - 2023 Intel Corporation
 * 
 * Disassembly of asf!.dat, Thu Jul 30 10:57:58 2026
 *
 * ACPI Data Table [ASF!]
 *
 * Format: [HexOffset DecimalOffset ByteLength]  FieldName : FieldValue (in hex)
 */

[000h 0000 004h]                   Signature : "ASF!"    [Alert Standard Format Table]
[004h 0004 004h]                Table Length : 000000A0
[008h 0008 001h]                    Revision : 20
[009h 0009 001h]                    Checksum : 1E
[00Ah 0010 006h]                      Oem ID : "INTEL "
[010h 0016 008h]                Oem Table ID : " HCG"
[018h 0024 004h]                Oem Revision : 00000001
[01Ch 0028 004h]             Asl Compiler ID : "TFSM"
[020h 0032 004h]       Asl Compiler Revision : 000F4240

[024h 0036 001h]               Subtable Type : 00 [ASF Information]
[025h 0037 001h]                    Reserved : 00
[026h 0038 002h]                      Length : 0010
[028h 0040 001h]         Minimum Reset Value : FF
[029h 0041 001h]    Minimum Polling Interval : FF
[02Ah 0042 002h]                   System ID : 0001
[02Ch 0044 004h]             Manufacturer ID : 57010000
[030h 0048 001h]                       Flags : 00
[031h 0049 003h]                    Reserved : 000000

[034h 0052 001h]               Subtable Type : 01 [ASF Alerts]
[035h 0053 001h]                    Reserved : 00
[036h 0054 002h]                      Length : 002C
[038h 0056 001h]                  AssertMask : 00
[039h 0057 001h]                DeassertMask : 00
[03Ah 0058 001h]                 Alert Count : 03
[03Bh 0059 001h]           Alert Data Length : 0C

[03Ch 0060 001h]                     Address : 89
[03Dh 0061 001h]                     Command : 04
[03Eh 0062 001h]                        Mask : 01
[03Fh 0063 001h]                       Value : 01
[040h 0064 001h]                  SensorType : 05
[041h 0065 001h]                        Type : 6F
[042h 0066 001h]                      Offset : 00
[043h 0067 001h]                  SourceType : 68
[044h 0068 001h]                    Severity : 08
[045h 0069 001h]                SensorNumber : 88
[046h 0070 001h]                      Entity : 17
[047h 0071 001h]                    Instance : 00

[048h 0072 001h]                     Address : 89
[049h 0073 001h]                     Command : 04
[04Ah 0074 001h]                        Mask : 04
[04Bh 0075 001h]                       Value : 04
[04Ch 0076 001h]                  SensorType : 07
[04Dh 0077 001h]                        Type : 6F
[04Eh 0078 001h]                      Offset : 00
[04Fh 0079 001h]                  SourceType : 68
[050h 0080 001h]                    Severity : 20
[051h 0081 001h]                SensorNumber : 88
[052h 0082 001h]                      Entity : 03
[053h 0083 001h]                    Instance : 00

[054h 0084 001h]                     Address : 89
[055h 0085 001h]                     Command : 05
[056h 0086 001h]                        Mask : 01
[057h 0087 001h]                       Value : 01
[058h 0088 001h]                  SensorType : 19
[059h 0089 001h]                        Type : 6F
[05Ah 0090 001h]                      Offset : 00
[05Bh 0091 001h]                  SourceType : 68
[05Ch 0092 001h]                    Severity : 20
[05Dh 0093 001h]                SensorNumber : 88
[05Eh 0094 001h]                      Entity : 22
[05Fh 0095 001h]                    Instance : 00

[060h 0096 001h]               Subtable Type : 02 [ASF Remote Control]
[061h 0097 001h]                    Reserved : 00
[062h 0098 002h]                      Length : 0018
[064h 0100 001h]               Control Count : 04
[065h 0101 001h]         Control Data Length : 04
[066h 0102 002h]                    Reserved : 0000

[068h 0104 001h]                    Function : 00
[069h 0105 001h]                     Address : 88
[06Ah 0106 001h]                     Command : 00
[06Bh 0107 001h]                       Value : 03

[06Ch 0108 001h]                    Function : 01
[06Dh 0109 001h]                     Address : 88
[06Eh 0110 001h]                     Command : 00
[06Fh 0111 001h]                       Value : 02

[070h 0112 001h]                    Function : 02
[071h 0113 001h]                     Address : 88
[072h 0114 001h]                     Command : 00
[073h 0115 001h]                       Value : 01

[074h 0116 001h]                    Function : 03
[075h 0117 001h]                     Address : 88
[076h 0118 001h]                     Command : 00
[077h 0119 001h]                       Value : 04

[078h 0120 001h]               Subtable Type : 03 [ASF RMCP Boot Options]
[079h 0121 001h]                    Reserved : 00
[07Ah 0122 002h]                      Length : 0017
[07Ch 0124 007h]                Capabilities : 20 18 00 00 00 13 F0
[083h 0131 001h]             Completion Code : 00
[084h 0132 004h]               Enterprise ID : 57010000
[088h 0136 001h]                     Command : 00
[089h 0137 002h]                   Parameter : 0000
[08Bh 0139 002h]                Boot Options : 0100
[08Dh 0141 002h]              Oem Parameters : 0000

[08Fh 0143 001h]               Subtable Type : 84 [ASF Address]
[090h 0144 001h]                    Reserved : 00
[091h 0145 002h]                      Length : 0011
[093h 0147 001h]               Eprom Address : 00
[094h 0148 001h]                Device Count : 0B
[095h 0149 001h]                   Addresses : 5C 68 88 C2 D2 DC A0 A2 A4 A6 C8 


Raw Table Data: Length 160 (0xA0)

    0000: 41 53 46 21 A0 00 00 00 20 1E 49 4E 54 45 4C 20  // ASF!.... .INTEL 
    0010: 20 48 43 47 00 00 00 00 01 00 00 00 54 46 53 4D  //  HCG........TFSM
    0020: 40 42 0F 00 00 00 10 00 FF FF 01 00 00 00 01 57  // @B.............W
    0030: 00 00 00 00 01 00 2C 00 00 00 03 0C 89 04 01 01  // ......,.........
    0040: 05 6F 00 68 08 88 17 00 89 04 04 04 07 6F 00 68  // .o.h.........o.h
    0050: 20 88 03 00 89 05 01 01 19 6F 00 68 20 88 22 00  //  ........o.h .".
    0060: 02 00 18 00 04 04 00 00 00 88 00 03 01 88 00 02  // ................
    0070: 02 88 00 01 03 88 00 04 03 00 17 00 20 18 00 00  // ............ ...
    0080: 00 13 F0 00 00 00 01 57 00 00 00 00 01 00 00 84  // .......W........
    0090: 00 11 00 00 0B 5C 68 88 C2 D2 DC A0 A2 A4 A6 C8  // .....\h.........
