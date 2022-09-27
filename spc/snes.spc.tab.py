import math
import wave
import struct

FILE_OUT = "snes.spc.tab.inc"

#
# Mappings and amplitudes used to generate our tables
#

amplitude = [ 64, 77, 46, 12 ] # square, triangle, comp-noise, hard-noise
atten_cutoff = 14000
atten_power = 0.3
comp_nse_atten = 0.5

squ_len = [ # (start of range, length of sample)
    (0x000, 16),
    (0x040, 64),
    (0x100,128),
    (0x400,512)]
nsp_len = [
    (0x0,  16),
    (0x3, 128),
    (0x6, 512),
    (0xA,2048)]  

#
# Hardware constants
#

snes_master = 32000
nes_master = 1789772

nes_wav_squ = [
    [ 0, 0,15,15, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [ 0, 0,15,15,15,15, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [ 0, 0,15,15,15,15,15,15,15,15, 0, 0, 0, 0, 0, 0],
    [15,15, 0, 0, 0, 0,15,15,15,15,15,15,15,15,15,15]]
nes_wav_tri = [
      8, 9,10,11,12,13,14,15,15,14,13,12,11,10, 9, 8,
      7, 6, 5, 4, 3, 2, 1, 0, 0, 1, 2, 3, 4, 5, 6, 7 ]

nes_nse_div = [4, 8, 16, 32, 64, 96, 128, 160, 202, 254, 380, 508, 762, 1016, 2034, 4068]
snes_nse_div = [
    0, 2048, 1536, 1280, 1024, 768, 640, 512,
    384, 320, 256, 192, 160, 128, 96, 80,
    64, 48, 40, 32, 24, 20, 16, 12,
    10, 8, 6, 5, 4, 3, 2, 1]

#
# Waveform generators
#

def nes_wav_nse(length,seed=0x3210):
    w = []
    for i in range(length):
        seed <<= 1
        if seed >= 0x8000:
            seed = (seed ^ 0x41) & 0x7FFF
        w.append((seed & 1) * 15)
    return w

def wav_expand(w,s):
    wd = []
    for e in w: wd += [e] * s
    return wd

def wav_shrink_renorm(w,s):
    wd = []
    # averaging downsample
    for i in range(0,len(w),s):
        wd.append(sum([w[i+j] for j in range(s)]) / s)
    # renormalize to 0-15
    wmin = min(wd)
    wmax = max(wd)
    ws   = 15 / (wmax - wmin)
    for i in range(0,len(wd)):
        wd[i] = round((wd[i] - wmin) * ws)
    return wd

def wav_save(w,filename,offset=-7.5,scale=1<<12):
    wf = wave.open(filename,mode="wb")
    wf.setnchannels(1)
    wf.setsampwidth(2)
    wf.setframerate(snes_master)
    for s in w: wf.writeframes(struct.pack("<h",int((s+offset) * scale)))
    wf.close()

#
# Exploration tables for pitch mappings
#

def print_nes_squ_mappings(lengths): # square vs. sample length
    s = "SQU  "
    for l in lengths: s += " %4d" % (l)
    print(s)
    for np in range(2048):
        s = "%4X:" % (np)
        nf = nes_master / (16 * (np+1))
        for l in lengths:
            sp = round(nf * 0x1000 * l / snes_master)
            sp = min(0xFFFF,sp)
            s += " %4X" % (sp)
        print(s + (" %10.3f Hz" % (nf)))

def print_nes_nsp_mappings(lengths): # periodic noise vs. sample length
    s = "NSP  "
    for l in lengths: s += " %4d" % (l)
    print(s)
    for np in range(len(nes_nse_div)):
        s = "%4X:" % (np)
        nf = nes_master / (nes_nse_div[np] * 93)
        for l in lengths:
            sp = round(nf * 0x1000 * l / snes_master)
            sp = min(0xFFFF,sp)
            s += " %4X" % (sp)
        print(s + (" %10.3f Hz" % (nf)))

def print_nes_nss_mappings(expands): # noise vs. sample expansion
    s = "NSS  "
    for e in expands: s += " %4d" % (e)
    print(s)
    for np in range(len(nes_nse_div)):
        s = "%4X:" % (np)
        nf = nes_master / nes_nse_div[np]
        for e in expands:
            es = e if e >= 0 else (1 / -e)
            sp = round(nf * 0x1000 * es / snes_master)
            sp = min(0xFFFF,sp)
            s += " %4X" % (sp)
        print(s + (" %10.3f Hz loop %6.3f Hz" % (nf,nf/32767)))

def print_nes_nse_mappings(): # noise vs. hardware noise
    s = "NSE"
    print(s)
    for np in range(len(nes_nse_div)):
        s = "%4X:" % (np)
        nf = nes_master / nes_nse_div[np]
        bp = 0
        bf = 0
        bd = nes_master
        for sp in range(len(snes_nse_div)):
            sf = 0
            if  sp > 0: sf = snes_master / snes_nse_div[sp]
            d = abs(sf-nf)
            if d < bd:
                bp = sp
                bf = sf
                bd = d
        s += " %4X" % (bp)
        print(s + (" %10.3f Hz at %9.3f Hz" % (nf,bf)))

#
# Generation of tables for pitch mappings
#

def nes_squ_mappings(sl): # square / triangle
    d = []
    for np in range(2048):
        nf = nes_master / (16 * (np+1))
        p = 0
        l = 0
        for i in range(len(sl)):
            if sl[i][0] <= np:
                p = round(nf * 0x1000 * sl[i][1] / snes_master)
                l = i
        p = min(0x3FFF,p)
        d.append(p | (l<<16))
    return d

def nes_nsp_mappings(sl): # periodic noise
    d = []
    for np in range(len(nes_nse_div)):
        nf = nes_master / (nes_nse_div[np] * 93)
        p = 0
        l = 0
        for i in range(len(sl)):
            if sl[i][0] <= np:
                p = round(nf * 0x1000 * sl[i][1] / snes_master)
                l = i
        p = min(0x3FFF,p)
        d.append(p | (l<<16))
    return d

def nes_nse_mappings(): # hardware noise FLG mappings
    d = []
    for np in range(len(nes_nse_div)):
        nf = nes_master / nes_nse_div[np]
        sp = 0
        bd = nes_master
        for i in range(1,len(snes_nse_div)):
            sf = snes_master / snes_nse_div[i]
            sd = abs(sf-nf)
            if sd < bd:
                bd = sd
                sp = i
        d.append(sp)
    return d

def comp_nse_mappings(flg,samplerate): # FLG to complementary noise mappings
    d = []
    for f in flg:
        sf = snes_master / snes_nse_div[f]
        p = round(sf * 0x0500 / samplerate)
        p = min(0x3FFF,p)
        d.append(p)
    return d

def nse_atten(): # noise attenuation table
    d = []
    for np in range(len(nes_nse_div)):
        nf = nes_master / nes_nse_div[np]
        a = 1
        if nf > atten_cutoff:
            a = math.pow(atten_cutoff / nf, atten_power)
        d.append(round(a * amplitude[2]))
        d.append(round(a * amplitude[3]))
    return d

#
# LFSR and complementary noise generator
#

def lfsr_run(length, seed=1):
    d = []
    for i in range(length):
        # SNES LFSR
        #feedback = ((seed << 13) ^ (seed << 14)) & 0x4000
        #seed = (seed >> 1) ^ feedback
        #d.append(seed<<1)
        # Alternative LFSR (to avoid correlation with SNES)
        feedback = 0x000000C5 if (seed >= 0x80000000) else 0
        seed = ((seed << 1) ^ feedback) & 0xFFFFFFFF
        d.append(seed >> 16)
    return d

def comp_nse_wav(length):
    r = lfsr_run(length,0x56781234)
    w = []
    for i in range(length):
        # each sample is a convolution of 14 1-bit noise samples
        # [1/2, 1/4, 1/8, 1/16...]
        # this produces the complementary opposite of the SNES'
        # implicit highpass, which we will mix with to complete the white spectrum
        # [-1, 1/2, 1/4, 1/8...]
        a = 0
        for j in range(14):
            a += (r[(i+j)%length] & 0x8000) >> j
        w.append(a)
    # normalize attenuate to a range of -0.5 to 0.5
    wmax = max(w)
    for i in range(len(w)):
        w[i] = ((w[i] / wmax) - 0.5) * comp_nse_atten
    # simple downsample by 1/2 to save space
    sw = []
    for i in range(0,len(w),2):
        sw.append((w[i+0]+w[i+1])/2)
    # simple downsample by 1/2 to save space, convert to 0-15 range
    for i in range(len(sw)):
        sw[i] = round(7.5 + (sw[i] * 15))
    return sw

#
# Table printing as assembly code
#

def asm_dump(b, form, lead, columns):
    s = lead
    for i in range(len(b)):
        c = i % columns
        if (c == 0 and i != 0):
            s += "\n" + lead
        if (c > 0):
            s += ","
        s += (form % b[i])
    return s

def asm_byte(b, columns=32):
    return asm_dump(b, "$%02X", ".db ", columns)

def asm_byte_decimal(b, columns=32):
    return asm_dump(b, "%3d", ".db ", columns)

def asm_word(b, columns=16):
    return asm_dump(b, "$%04X", ".dw ", columns)

def asm_dword(b, columns=8):
    return asm_dump(b, "$%08X", ".dd ", columns)

def asm_wav(w, offset=-8, columns=(9*4)):
    assert ((len(w) % 16) == 0) # sample must be multiple of 16
    d = bytearray()
    for i in range(0,len(w),2):
        if (i % 16) == 0:
            b = 0xA2 # shift 11, looping sample
            if (i >= (len(w)-16)): b |= 0x01 # loop point
            d.append(b)
        s0 = w[i+0] + offset
        s1 = w[i+1] + offset
        assert (s0 >= -8 and s0 <= 7)
        assert (s1 >= -8 and s1 <= 7)
        d.append(((s0 & 0xF) << 4) | (s1 & 0xF))
    return asm_byte(d,columns)

#
#
# Main
#
#

# for discovery when trying to set the length tables
#print_nes_squ_mappings([16,32,64,128,256,512,1024,2048,4096])
#print_nes_nsp_mappings([16,32,64,128,256,512,1024,2048,4096])
#print_nes_nss_mappings([-4,-2,1,2,4,8,16,32,64])
#print_nes_nse_mappings()


print(FILE_OUT + "\n")
fo = open(FILE_OUT,"wt")
fo.write("; generated tables, see: snes.spc.tab.py\n\n")


def add_line(text):
    fo.write(text+"\n")
    print(text)

def add_const(name,value):
    global fo
    s = "%-10s = %d" % (name,value)
    fo.write(s + "\n")
    print(s)

def add_dump(name,asm):
    global fo
    s = name + ":\n" + asm + "\n"
    fo.write(s + "\n")
    print(s)


src_dir = [] # directory of sources by name

add_line("; waveforms\n")

squ_src = [] # directory position of square waveforms
for d in range(len(nes_wav_squ)):
    squ_src.append(len(src_dir))
    for sl in range(len(squ_len)):
        w = nes_wav_squ[d]
        w = wav_expand(w,squ_len[sl][1] // len(w))
        aw = asm_wav(w)
        name = "wav_squ%d%d" % (d,sl)
        add_dump(name,aw)
        src_dir.append(name)

tri_src = len(src_dir)
for sl in range(len(squ_len)):
    w = nes_wav_tri
    w = wav_expand(w,(squ_len[sl][1]*2) // len(w))
    aw = asm_wav(w)
    name = "wav_tri%d" % (sl)
    add_dump(name,aw)
    src_dir.append(name)

nsp_src = len(src_dir)
nsp_wav = [(x&1)*15 for x in lfsr_run(128,0x56781234)]
for sl in range(len(nsp_len)):
    w = nsp_wav
    l = nsp_len[sl][1]
    if l < len(w):
        w = wav_shrink_renorm(w,len(w) // l)
    elif l > len(w):
        w = wav_expand(w,l // len(w))
    aw = asm_wav(w)
    name = "wav_nsp%d" % (sl)
    add_dump(name,aw)
    src_dir.append(name)

NSC_LENGTH = 32768 # ~1 second of noise
nsc_src = len(src_dir)
nsc_wav = comp_nse_wav(NSC_LENGTH)
#wav_save(nsc_wav,"NSC.WAV")
aw = asm_wav(nsc_wav)
name = "wav_nsc"
add_dump(name,aw)
src_dir.append(name)


add_line("; pitch/sample mappings\n")
name = "pitch_sample_squ"
add_dump(name,asm_dword(nes_squ_mappings(squ_len)))
name = "pitch_sample_nsp"
add_dump(name,asm_dword(nes_nsp_mappings(nsp_len),8))
flg = nes_nse_mappings()
name = "pitch_nsc"
add_dump(name,asm_word(comp_nse_mappings(flg,NSC_LENGTH),8))
name = "flg_nse"
add_dump(name,asm_byte(flg,8))


add_line("; amplitude and attenuation\n")
add_const("VOL_SQU",amplitude[0])
add_const("VOL_TRI",amplitude[1])
add_const("VOL_NSE",amplitude[2])
add_const("VOL_NSP",amplitude[3])
add_line("")
name = "atten_nse"
add_dump(name,asm_byte_decimal(nse_atten(),16))


add_line("; sample directory\n")

add_const("SRC_SQU",squ_src[0])
add_const("SRC_TRI",tri_src)
add_const("SRC_NSP",nsp_src)
add_const("SRC_NSC",nsc_src)
add_line("")
name = "src_squ"
add_dump(name,asm_byte_decimal(squ_src))


add_line(".section \"Directory\" align 256")
add_line("src_directory:")
for name in src_dir:
    add_line(".dw %-11s %-10s" % (name+",",name))
add_line(".ends")

add_line("\n; end of file")
fo.close()
