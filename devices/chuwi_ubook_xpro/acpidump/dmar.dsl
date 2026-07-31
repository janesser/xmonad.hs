/*
 * Intel ACPI Component Architecture
 * AML/ASL+ Disassembler version 20230628 (64-bit version)
 * Copyright (c) 2000 - 2023 Intel Corporation
 * 
 * Disassembly of dmar.dat, Thu Jul 30 10:57:58 2026
 *
 * ACPI Data Table [DMAR]
 *
 * Format: [HexOffset DecimalOffset ByteLength]  FieldName : FieldValue (in hex)
 */

[000h 0000 004h]                   Signature : "DMAR"    [DMA Remapping Table]
[004h 0004 004h]                Table Length : 000000A8
[008h 0008 001h]                    Revision : 01
[009h 0009 001h]                    Checksum : 9D
[00Ah 0010 006h]                      Oem ID : "INTEL "
[010h 0016 008h]                Oem Table ID : "EDK2    "
[018h 0024 004h]                Oem Revision : 00000001
[01Ch 0028 004h]             Asl Compiler ID : "INTL"
[020h 0032 004h]       Asl Compiler Revision : 00000001

[024h 0036 001h]          Host Address Width : 26
[025h 0037 001h]                       Flags : 01
[026h 0038 00Ah]                    Reserved : 00 00 00 00 00 00 00 00 00 00

[030h 0048 002h]               Subtable Type : 0000 [Hardware Unit Definition]
[032h 0050 002h]                      Length : 0018

[034h 0052 001h]                       Flags : 00
[035h 0053 001h]                    Reserved : 00
[036h 0054 002h]          PCI Segment Number : 0000
[038h 0056 008h]       Register Base Address : 00000000FED90000

[040h 0064 001h]           Device Scope Type : 01 [PCI Endpoint Device]
[041h 0065 001h]                Entry Length : 08
[042h 0066 002h]                    Reserved : 0000
[044h 0068 001h]              Enumeration ID : 00
[045h 0069 001h]              PCI Bus Number : 00

[046h 0070 002h]                    PCI Path : 02,00


[048h 0072 002h]               Subtable Type : 0000 [Hardware Unit Definition]
[04Ah 0074 002h]                      Length : 0020

[04Ch 0076 001h]                       Flags : 01
[04Dh 0077 001h]                    Reserved : 00
[04Eh 0078 002h]          PCI Segment Number : 0000
[050h 0080 008h]       Register Base Address : 00000000FED91000

[058h 0088 001h]           Device Scope Type : 03 [IOAPIC Device]
[059h 0089 001h]                Entry Length : 08
[05Ah 0090 002h]                    Reserved : 0000
[05Ch 0092 001h]              Enumeration ID : 02
[05Dh 0093 001h]              PCI Bus Number : F0

[05Eh 0094 002h]                    PCI Path : 1F,00


[060h 0096 001h]           Device Scope Type : 04 [Message-capable HPET Device]
[061h 0097 001h]                Entry Length : 08
[062h 0098 002h]                    Reserved : 0000
[064h 0100 001h]              Enumeration ID : 00
[065h 0101 001h]              PCI Bus Number : 00

[066h 0102 002h]                    PCI Path : 1F,00


[068h 0104 002h]               Subtable Type : 0001 [Reserved Memory Region]
[06Ah 0106 002h]                      Length : 0020

[06Ch 0108 002h]                    Reserved : 0000
[06Eh 0110 002h]          PCI Segment Number : 0000
[070h 0112 008h]                Base Address : 000000008ABA4000
[078h 0120 008h]         End Address (limit) : 000000008ADEDFFF

[080h 0128 001h]           Device Scope Type : 01 [PCI Endpoint Device]
[081h 0129 001h]                Entry Length : 08
[082h 0130 002h]                    Reserved : 0000
[084h 0132 001h]              Enumeration ID : 00
[085h 0133 001h]              PCI Bus Number : 00

[086h 0134 002h]                    PCI Path : 14,00


[088h 0136 002h]               Subtable Type : 0001 [Reserved Memory Region]
[08Ah 0138 002h]                      Length : 0020

[08Ch 0140 002h]                    Reserved : 0000
[08Eh 0142 002h]          PCI Segment Number : 0000
[090h 0144 008h]                Base Address : 000000008B800000
[098h 0152 008h]         End Address (limit) : 000000008FFFFFFF

[0A0h 0160 001h]           Device Scope Type : 01 [PCI Endpoint Device]
[0A1h 0161 001h]                Entry Length : 08
[0A2h 0162 002h]                    Reserved : 0000
[0A4h 0164 001h]              Enumeration ID : 00
[0A5h 0165 001h]              PCI Bus Number : 00

[0A6h 0166 002h]                    PCI Path : 02,00


Raw Table Data: Length 168 (0xA8)

    0000: 44 4D 41 52 A8 00 00 00 01 9D 49 4E 54 45 4C 20  // DMAR......INTEL 
    0010: 45 44 4B 32 20 20 20 20 01 00 00 00 49 4E 54 4C  // EDK2    ....INTL
    0020: 01 00 00 00 26 01 00 00 00 00 00 00 00 00 00 00  // ....&...........
    0030: 00 00 18 00 00 00 00 00 00 00 D9 FE 00 00 00 00  // ................
    0040: 01 08 00 00 00 00 02 00 00 00 20 00 01 00 00 00  // .......... .....
    0050: 00 10 D9 FE 00 00 00 00 03 08 00 00 02 F0 1F 00  // ................
    0060: 04 08 00 00 00 00 1F 00 01 00 20 00 00 00 00 00  // .......... .....
    0070: 00 40 BA 8A 00 00 00 00 FF DF DE 8A 00 00 00 00  // .@..............
    0080: 01 08 00 00 00 00 14 00 01 00 20 00 00 00 00 00  // .......... .....
    0090: 00 00 80 8B 00 00 00 00 FF FF FF 8F 00 00 00 00  // ................
    00A0: 01 08 00 00 00 00 02 00                          // ........
