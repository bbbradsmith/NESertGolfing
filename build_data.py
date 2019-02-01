#!/usr/bin/env python3

import PIL.Image
import math

#
# processes some image files into NES CHR data
#

def index_image(filename, palette):
    # load and convert image to indexed palette
    src = PIL.Image.open(filename)
    dst = PIL.Image.new("P",src.size,color=0)
    for y in range(src.size[1]):
        for x in range(src.size[0]):
            p = src.getpixel((x,y))
            mag = ((255**2)*3)+1
            mat = 0
            for i in range(len(palette)):
                m = sum([(a-b)**2 for (a,b) in zip(p,palette[i])])
                if m < mag: # better match
                    mat = i
                    mag = m
                    if m == 0: # perfect match
                        break
            dst.putpixel((x,y),mat)
    return dst

def print_img(im):
    # print indexed image (for debugging)
    print(im)
    for y in range(im.size[1]):
        s = "%3d:" % y
        for x in range(im.size[0]):
            s += "" + str(im.getpixel((x,y)))
        print(s)

def make_chr(im, horizontal=True):
    # convert indexed (0-3) image to CHR, as 8x8 tiles
    c = []
    wx, wy = im.size[0]//8, im.size[1]//8
    if not horizontal:
        wy, wx = wx, wy
    for ty in range(wy):
        for tx in range(wx):
            ox = tx*8
            oy = ty*8
            if not horizontal:
                oy, ox = ox, oy
            c0 = []
            c1 = []
            for y in range(8):
                r0 = 0
                r1 = 0
                for x in range(8):
                    p = im.getpixel((x+ox,y+oy))
                    r0 =  (p & 1)       | (r0 << 1)
                    r1 = ((p & 2) >> 1) | (r1 << 1)
                c0.append(r0)
                c1.append(r1)
            c = c + c0 + c1
    return c

def layer_chr(c1,c2):
    # take two CHR itmes and swap planes,
    # creating two new CHR items:
    # the first with both plane 0s
    # the second with both plane 1s
    l1 = []
    l2 = []
    assert(len(c1) == len(c2))
    for i in range(0,len(c1),16):
        l1 = l1 + c1[i  :i+8 ] + c2[i  :i+8 ]
        l2 = l2 + c1[i+8:i+16] + c2[i+8:i+16]
    return (l1,l2)

def chr_to_rgb(c, palette, width=16, horizontal=True):
    # converts CHR data into an RGB image
    # using the given 4-colour palette.
    tiles = len(c) // 16
    columns = width
    rows = (tiles + (width-1)) // width
    if not horizontal:
        rows, columns = columns, rows
    img = PIL.Image.new("RGB",(width*8,rows*8),palette[0])
    for t in range(0,tiles):
        xo = (t % width) * 8
        yo = (t // width) * 8
        if not horizontal:
            yo, xo = xo, yo
        to = t * 16
        for y in range(8):
            p0 = c[to + 0 + y]
            p1 = c[to + 8 + y]
            for x in range(8):
                p = ((p0 >> 7) & 1) | ((p1 >> 6) & 2)
                img.putpixel((xo+x, yo+y), palette[p])
                p0 <<= 1
                p1 <<= 1
    return img

def byte_table(t, width = 16):
    s = ""
    while (len(t) > 0):
        s += ".byte "
        r = t[0:width]
        t = t[width:]
        s += "$%02X" % r[0]
        r = r[1:]
        for e in r:
            s += ", $%02X" % e
        s += "\n"
    return s

#
# output filenames
#

layerout = "layers.chr"
spriteout = "sprite.chr"
slopeout = "temp\\slopes.inc"

#
# specific CHR data
#

pal1bit = [(0,0,0),(255,255,255)]
pal2bit = [(0,0,0),(0x65,0x66,0x65),(0xb0,0xb1,0xb0),(255,255,255)]

layer1 = index_image("layer1.png",pal1bit)
layer2 = index_image("layer2.png",pal1bit)
sprite = index_image("sprite.png",pal2bit)

# Test of print_img
#print_img(layer1)
#print_img(layer2)
#print_img(sprite)

layer_chr = layer_chr(make_chr(layer1),make_chr(layer2))[0]
open(layerout,"wb").write(bytes(layer_chr))
print(layerout + " -> %d bytes" % len(layer_chr))

sprite_chr = make_chr(sprite)
open(spriteout,"wb").write(bytes(sprite_chr))
print(spriteout + " -> %d bytes" % len(sprite_chr))

# Test of chr_to_rgb
#chr_to_rgb(layer_chr, pal2bit).save("layer_rev.png")
#chr_to_rgb(sprite_chr, pal2bit, 8, False).save("sprite_rev.png")

#
# slope and normal table generator
#

slopes = [0,1,2,3,4] + ([0]*8) + ([1]*8) # special values, frequent values
slopes_curve = 4 # increasing this number favours shallower slopes
slopes_remain = 128 - len(slopes)
for i in range(0,slopes_remain):
    slopes.append(4 * math.pow(i / (slopes_remain-1),slopes_curve))
slopes = slopes + [-x for x in slopes] # second half is negative

# convert to fixed point .8
# normal is perpendicular to the slope, always facing up
norms = []
for i in range(len(slopes)):
    s = slopes[i]
    mag = math.sqrt((s*s) + (-1*-1))
    norms.append((int(s * 256 / mag), int(-256 / mag)))
    slopes[i] = int(s * 256)

# build text
s =   "; generated slopes\n"
s +=  "\n"
s += "slope_y0:\n" + byte_table([((e>>0)&255) for e in slopes]) + "\n"
s += "slope_y1:\n" + byte_table([((e>>8)&255) for e in slopes]) + "\n"
s += "norm_x0:\n" + byte_table([((e[0]>>0)&255) for e in norms]) + "\n"
s += "norm_x1:\n" + byte_table([((e[0]>>8)&255) for e in norms]) + "\n"
s += "norm_y0:\n" + byte_table([((e[1]>>0)&255) for e in norms]) + "\n"
s += "norm_y1:\n" + byte_table([((e[1]>>8)&255) for e in norms]) + "\n"
s += "; end\n"
open(slopeout,"wt").write(s)
print(slopeout + " -> %d characters" % len(s))
