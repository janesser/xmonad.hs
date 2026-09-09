#!/usr/bin/env python3
"""Minimal AML walker: dump Names/Methods/UUIDs/Stores/Resources for the camera region."""
import sys

b = open('DSDT_02c9dd.dat','rb').read()

def s(p):
    return ''.join(chr(c) if 32<=c<127 else '.' for c in b[p:])

def dec_name(p):
    # returns (namestr, nextoff). Names may be segments; we grab raw until we see a known opcode-ish byte
    segs=[]
    while p < len(b):
        c=b[p]
        if 0x40<=c<=0x5a:      # A-Z
            segs.append(chr(c)); p+=1
        elif c==0x80:           # multi-byte name segment continuation
            segs.append('_'); p+=1
        else:
            break
    return ''.join(segs), p

def read_string(p):
    # string: [len][chars] ; len byte then chars (utf8); 0 len -> ""
    if p>=len(b): return "",p
    L=b[p]; p+=1
    if L==0: return "",p
    st=bytes(b[p:p+L]); p+=L
    return st.decode('utf8','replace'), p

def read_uuid(p):
    raw=bytes(b[p:p+16]); p+=16
    u=f"{raw[:4].hex()}-{raw[4:6].hex()}-{raw[6:8].hex()}-{raw[8:10].hex()}-{raw[10:16].hex()}"
    return u,p

def dump(p, end, depth=0, show_stores=False):
    pad="  "*depth
    while p < end:
        op=b[p]; p+=1
        if op==0x5A:        # NoOp
            continue
        if op==0x15 or op==0x08:  # Name / Device-in-scope
            nm, p = dec_name(p)
            # value
            if p<end and b[p]==0x70:  # Store
                p+=1
                # peek next: integer(0x16)/string(0x0a)/uuid(0x0f)/package(0x1b)/if(0x10)...
                if p<end and b[p]==0x16:
                    p+=1; n=int.from_bytes(b[p:p+4],'big',signed=True); p+=4
                    print(f"{pad}Name({nm}, {n})")
                elif p<end and b[p]==0x0a:
                    st,p=read_string(p); print(f"{pad}Name({nm}, \"{st}\")")
                elif p<end and b[p]==0x0f:
                    u,p=read_uuid(p); print(f"{pad}Name({nm}, <uuid {u}>)")
                elif p<end and b[p]==0x1b:
                    p+=1; pe=p; ne=p
                    while ne<pe:
                        ne+=1
                    p=pe
                    print(f"{pad}Name({nm}, <package>)")
                else:
                    # generic: skip one construct by guessing
                    print(f"{pad}Name({nm}, <value@0x{p-1:x}>)")
                    break
            else:
                print(f"{pad}Name({nm})  # no value")
            continue
        if op==0x14:        # Method
            nm,p=dec_name(p)
            nargs=b[p]; p+=1; flags=b[p]; p+=1
            print(f"{pad}Method({nm}, {nargs}, {flags:#x})")
            # body: read until matching - approximate by scanning for EndScope of same depth via brace counting
            body_start=p
            # find matching EndScope by counting StartScope(0x1D) vs EndScope(0x1E)
            i=p; depthc=0
            while i<len(b):
                o=b[i]
                if o==0x1D: depthc+=1
                elif o==0x1E:
                    depthc-=1
                    if depthc==-1:
                        break
                i+=1
            end=i+1
            print(dump(body_start, end, depth+1, show_stores))
            p=end
            continue
        if op==0x1D:        # BeginScope
            # scope target
            if p<end and b[p] in (0x08,):  # Name scope
                nm,p=dec_name(p); print(f"{pad}Scope({nm})"); continue
            else:
                print(f"{pad}Scope(@0x{p-1:x})")
                # skip its body
                i=p; d=0
                while i<len(b):
                    if b[i]==0x1D: d+=1
                    elif b[i]==0x1E:
                        d-=1
                        if d==-1: break
                    i+=1
                p=i+1; continue
        if op==0x1B:        # Package
            n=p; ne=p+1
            cnt=b[p]; p+=1
            while p<ne:  # placeholder
                pass
            # approximate skip: read cnt elements naively
            cnt=b[n-1] if n-1>=0 else cnt
            # robust: skip cnt objects
            skipped=0; depthc=0
            while skipped<cnt and p<len(b):
                o=b[p]
                if o in (0x08,0x15): p+=1+ (1 if False else 0); 
                # too complex; just break
                break
            print(f"{pad}<Package cnt={cnt} @0x{p:x}>")
            return
        if op==0x10 or op==0x11:  # If/Else
            print(f"{pad}{'If' if op==0x10 else 'Else'}@0x{p-1:x}")
            i=p; d=0
            while i<len(b):
                if b[i]==0x1D: d+=1
                elif b[i]==0x1E:
                    d-=1
                    if d==-1: break
                i+=1
            p=i+1; continue
        if op==0x70:        # Store (top-level, show)
            # src
            if p<end and b[p] in (0x16,):
                p+=1; n=int.from_bytes(b[p:p+4],'big',signed=True); p+=4; print(f"{pad}Store({n})")
            else:
                print(f"{pad}Store@0x{p-1:x}")
            continue
        # unknown - skip a few
        if not (0x40<=op<=0x7f or op in (0x5a,)):
            txt=s(p-1)[:20]
            print(f"{pad}??byte0x{op:02x} {txt}")
            p+=1
            if p>end: break
            continue
        print(f"{pad}byte 0x{op:02x}@0x{p-1:x}")
    return

# Walk the camera tail
start=0x29000
end=min(len(b), 0x29fa0)
print(dump(start,end,0))
