import struct, re, os
BASE=r"C:\Users\jesse\projs\xmonad.hs\devices\chuwi_ubook_xpro\chuwi_camera_drivers\agent_work\scc_data"
data=open(os.path.join(BASE,".text.bin"),"rb").read()
# .text VA 0x1000 -> image 0x140000000; file offset 0x19200
base_off=0x19200
# find mov reg32, imm32 (b8..ef + 4 bytes) followed within <=6 instrs by a CALL (ff 15 / ff 25)
# We'll collect offsets of mov imm32 with small imm (0..0x60), then note if a CALL appears in next 120 bytes.
calls=[m.start() for m in re.finditer(b'\xff[\x15\x25]', data)]
callset=set(calls)
print("num CALL near-mem:", len(calls))
results=[]
for m in re.finditer(b'([b-e][0-9a-f])\x00\x00\x00\x00', data):
    imm=struct.unpack_from('<I',data,m.start()+1)[0]
    if 0<=imm<=0x60:
        # is a CALL within next ~160 bytes?
        near=[c for c in callset if m.start()<c<m.start()+160]
        if near:
            results.append((m.start(), imm, min(near)))
# dedup by imm with addresses
seen={}
for off,imm,c in results:
    seen.setdefault(imm,[]).append((off,c))
for imm in sorted(seen):
    print("imm=0x%x  sites=%d  e.g.=%s"%(imm, len(seen[imm]), [(hex(o),hex(c)) for o,c in seen[imm][:4]]))
