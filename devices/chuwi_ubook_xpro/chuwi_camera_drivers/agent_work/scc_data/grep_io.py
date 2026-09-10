import re, os, sys
BASE=r"C:\Users\jesse\projs\xmonad.hs\devices\chuwi_ubook_xpro\chuwi_camera_drivers\agent_work\scc_data"
tgt=sys.argv[1] if len(sys.argv)>1 else "text"
data=open(os.path.join(BASE,tgt+".bin"),"rb").read()
print(tgt,"len",len(data))
# port I/O opcodes
for op,desc in [(b'\xd4','outsb'),(b'\xd5','outsl'),(b'\xd0','insb'),(b'\xd1','insd'),
                (b'\xda','out dx,al'),(b'\xee','in al,dx / out')]:
    offs=[m.start() for m in re.finditer(re.escape(op),data)]
    if offs:
        print("%-10s n=%d offs=%s"%(desc,len(offs),[hex(o) for o in offs[:40]]))
