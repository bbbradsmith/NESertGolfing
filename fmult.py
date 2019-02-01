# python test for verifying and debugging fmult

import random

demo = False

def sint16(x):
    if x < 0x8000:
        return x
    else:
        return x - 0x10000

def fmult(b,a):
    def fmult_row(la,lb):
        print ("%c*%c: %08X [%02X %02X %02X]" % (la,lb,out,out&0xFF,(out>>8)&0xFF,(out>>16)&0xFF))
    oa = a
    ob = b
    print ("fmult %04X * %04X = dcba * hgfe" % (a,b))
    flip = False
    flipped = False
    if (a & 0x8000):
        a = (-a) & 0xFFFF
        flip = not flip
        flipped = True
    if (b & 0x8000):
        b = (-b) & 0xFFFF
        flip = not flip
        flipped = True
    if (flipped):
        print ("flip  %04X * %04X" % (a,b))
    out = 0;
    out += ((a & 0x000F) * (b & 0x000F))
    fmult_row('e','a')
    out += ((a & 0x0F00) * (b & 0x000F))
    fmult_row('e','c')
    out += ((a & 0x00F0) * (b & 0x000F))
    fmult_row('e','b')
    out += ((a & 0xF000) * (b & 0x000F))
    fmult_row('e','d')
    out += ((a & 0x000F) * (b & 0x00F0))
    fmult_row('f','a')
    out += ((a & 0x0F00) * (b & 0x00F0))
    fmult_row('f','c')
    out += ((a & 0x00F0) * (b & 0x00F0))
    fmult_row('f','b')
    out += ((a & 0xF000) * (b & 0x00F0))
    fmult_row('f','d')
    out += ((a & 0x00F0) * (b & 0x0F00))
    fmult_row('g','b')
    out += ((a & 0xF000) * (b & 0x0F00))
    fmult_row('g','d')
    out += ((a & 0x000F) * (b & 0x0F00))
    fmult_row('g','a')
    out += ((a & 0x0F00) * (b & 0x0F00))
    fmult_row('g','c')
    out += ((a & 0x000F) * (b & 0xF000))
    fmult_row('h','a')
    out += ((a & 0x0F00) * (b & 0xF000))
    fmult_row('h','c')
    out += ((a & 0x00F0) * (b & 0xF000))
    fmult_row('h','b')
    out += ((a & 0xF000) * (b & 0xF000))
    fmult_row('h','d')
    out = out & 0x00FFFFFF
    if (flip):
        print ("flip %04X" % out)
        out = -out
    out = (out & 0x00FFFFFF) >> 8
    verify = ((sint16(oa)*sint16(ob))&0xFFFF00)//256
    print ("out: %04X = %04X" % (out,verify))
    assert (out == verify), "verify fail!"

if demo:
    for i in range(20000):
        a = random.getrandbits(16)
        b = random.getrandbits(16)
        fmult(a,b)
