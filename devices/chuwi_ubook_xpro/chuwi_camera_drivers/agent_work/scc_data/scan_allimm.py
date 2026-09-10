import re, struct, os
from collections import defaultdict
BASE=r"C:\Users\jesse\projs\xmonad.hs\devices\chuwi_ubook_xpro\chuwi_camera_drivers\agent_work\scc_data"
data=open(os.path.join(BASE,".text.bin"),"rb").read()
pos=defaultdict(list)
for m in re.finditer(b'[\xb8-\xef]', data):
    imm=struct.unpack('<I',data[m.start()+1:m.start()+5])[0]
    if 0<imm<=0x7f:
        pos[imm].append(m.start())
for imm in sorted(pos):
    ps=pos[imm]
    # restrict to mov r32,imm32 style (opcode b8..ef -> these are 5-byte); sample 2 VAs
    vas=[0x14001000+(0x19200+o) for o in ps[:2]]
    print("0x%02x  n=%d  e.g.%s"%(imm,len(ps),[hex(v) for v in vas]))
