import struct, re, os, sys
BASE=r"C:\Users\jesse\projs\xmonad.hs\devices\chuwi_ubook_xpro\chuwi_camera_drivers\agent_work\scc_data"
f=open(os.path.join(BASE,"SkcController.sys"),"rb").read()
hdr=0x210
secs=[]
for i in range(8):
    b=hdr+i*40
    name=f[b:b+8].rstrip(b'\x00').decode('latin1')
    vsize=struct.unpack_from('<I',f,b+8)[0]
    va=struct.unpack_from('<I',f,b+12)[0]
    fsize=struct.unpack_from('<I',f,b+16)[0]
    foff=struct.unpack_from('<I',f,b+20)[0]
    secs.append((name,va,vsize,foff,fsize))
for s in secs: print("%-8s VA=%08x VSize=%x FSize=%x FOff=%x"%s)
target=sys.argv[1] if len(sys.argv)>1 else ".text"
for name,va,vsize,foff,fsize in secs:
    if name==target:
        open(os.path.join(BASE,target+".bin"),"wb").write(f[foff:foff+fsize])
        print("wrote",target,"VA",hex(va),"len",fsize)
