#!/usr/bin/env python3
import re, struct, sys, os

IN = "acpidump.txt"
OUT = "win_tables"
os.makedirs(OUT, exist_ok=True)

# header line:  4-letter SIG @ 0xADDR
hdr = re.compile(r'^([A-Za-z0-9]{4}) @ 0x([0-9A-Fa-f]+)\s*$')

tables = []  # (sig, body_bytes)
cur = None
cur_sig = None
with open(IN, "r") as f:
    for line in f:
        line = line.rstrip("\n")
        m = hdr.match(line)
        if m:
            if cur is not None:
                tables.append((cur_sig, cur))
            cur_sig = m.group(1)
            cur = bytearray()
            continue
        if cur is None:
            continue
        if ":" not in line:
            continue
        # hex line like: "    0000: 44 53 ...  <ascii>"
        # only the first 16 bytes (32 hex chars) are hex; rest is ASCII
        body = line.split(":", 1)[1]
        for h in body.split():
            if len(h) == 2 and all(c in "0123456789abcdefABCDEF" for c in h):
                cur.append(int(h, 16))
if cur is not None:
    tables.append((cur_sig, cur))

def table_len(b):
    # ACPI table header: sig(4) len(4,LE)
    return struct.unpack_from("<I", b, 4)[0]

names = {}
for sig, b in tables:
    L = table_len(b)
    # a single sig may appear many times; disambiguate by writing each
    fn = os.path.join(OUT, f"{sig}_{len(b):06x}.dat")
    with open(fn, "wb") as o:
        o.write(b)
    names[sig] = names.get(sig, 0) + 1

print("parsed", len(tables), "tables")
from collections import Counter
c = Counter(sig for sig,_ in tables)
for sig,n in sorted(c.items()):
    print(f"  {sig:6s} x{n}")
