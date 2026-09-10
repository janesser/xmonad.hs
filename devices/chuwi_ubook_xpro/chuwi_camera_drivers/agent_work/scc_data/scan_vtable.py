import struct, os
BASE=r"C:\Users\jesse\projs\xmonad.hs\devices\chuwi_ubook_xpro\chuwi_camera_drivers\agent_work\scc_data"
f=open(os.path.join(BASE,"SkcController.sys"),"rb").read()
hdr=0x210
secs=[]
for i in range(8):
    b=hdr+i*40
    name=f[b:b+8].rstrip(b'\x00').decode('latin1')
    vsize=struct.unpack_from('<I',f,b+8)[0]; va=struct.unpack_from('<I',f,b+12)[0]
    fsize=struct.unpack_from('<I',f,b+16)[0]; foff=struct.unpack_from('<I',f,b+20)[0]
    secs.append((name,va,vsize,foff,fsize))
code_lo, code_hi = 0x1000, 0x24000
for name,va,vsize,foff,fsize in secs:
    blob=f[foff:foff+fsize]; n=len(blob)//8
    run=0; runstart=0
    for i in range(n):
        rel=(struct.unpack_from('<Q',blob,i*8)[0]) & 0xffffffff
        if code_lo<=rel<=code_hi:
            if run==0: runstart=i
            run+=1
        else:
            if run>=4: print("%-8s vt@f0x%x VA0x%x n=%d a0=%016x a%d=%016x"%(name,foff+runstart*8, va+runstart*8, run, struct.unpack_from('<Q',blob,runstart*8)[0], run-1, struct.unpack_from('<Q',blob,(runstart+run-1)*8)[0]))
            run=0
    if run>=4: print("%-8s trail@0x%x n=%d"%(name,foff+runstart*8,run))
