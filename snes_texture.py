#!/usr/bin/env python3

import PIL.Image
import math
import random

W = 128
H = 32
WS = 256 // W
HS = 256 // H

# Set to none to try a randomized seed
#SEED = None
SEED = 0x6B1519D0

houndstooth = \
    "** *****" + \
    "* ******" + \
    " ** ****" + \
    "**  ****" + \
    "      **" + \
    "     ** " + \
    "    ** *" + \
    "    * **"

battenburg = \
    "              ****************  " + \
    "               **************** " + \
    "                ****************" + \
    "                ****************" + \
    "                ****************" + \
    "                ****************" + \
    "                ****************" + \
    "                ****************" + \
    "                ****************" + \
    "                ****************" + \
    "                ****************" + \
    "                ****************" + \
    "                ****************" + \
    "                ****************" + \
    "*                ***************" + \
    "**                **************" + \
    "**************                **" + \
    "***************                *" + \
    "****************                " + \
    "****************                " + \
    "****************                " + \
    "****************                " + \
    "****************                " + \
    "****************                " + \
    "****************                " + \
    "****************                " + \
    "****************                " + \
    "****************                " + \
    "****************                " + \
    "****************                " + \
    " ****************               " + \
    "  ****************              "

def tile_preview(img,ws,hs,row_offset=0):
    timg = PIL.Image.new(img.mode,(img.width*ws,img.height*hs))
    o = -img.width
    for r in range(hs):
        for c in range(ws+1):
            timg.paste(img,(o+(c*img.width),r*img.height))
        o += row_offset
        while o >= 0:
            o -= img.width
    return timg

def wrap(x,w):
    xn = x/w
    return w * (xn - math.floor(xn))

def wrap2(x,y,w,h):
    return (wrap(x,w),wrap(y,h))

def pattern(x,y,w,h,p):
    (x,y) = wrap2(x,y,w,h)
    return 1 if p[(int(y)*w)+int(x)] != ' ' else 0

def generate(g):
    img = PIL.Image.new("1",(W,H))
    for y in range(H):
        for x in range(W):
            img.putpixel((x,y),g(x,y))
    return img

points = []
points_radius = []
POINTS_WRAP = [
    (-W, 0),
    ( 0, 0),
    ( W, 0),
    (-(W*3)/2,-H),
    (-(W*1)/2,-H),
    ( (W*1)/2,-H),
    ( (W*3)/2,-H),
    (-(W*3)/2, H),
    (-(W*1)/2, H),
    ( (W*1)/2, H),
    ( (W*3)/2, H),
    ]

def mag2(x,y):
    return (x*x)+(y*y)

def dist2(c0,c1):
    (x0,y0) = c0
    (x1,y1) = c1
    return mag2(x0-x1,y0-y1)

def closest_points(coord,a=-1): # returns 2 closest points ((x0,y0),i0,(x1,y1),i1)
    global points
    if coord == None:
        (x,y) = points[a]
    else:
        (x,y) = coord
    b0 = a
    b1 = a
    bc0 = (x,y)
    bc1 = (x,y)
    d20 = ((W*W)+(H*H)) * 2
    d21 = d20
    for b in range(len(points)):
        if a == b:
            continue
        (px,py) = points[b]
        for j in range(len(POINTS_WRAP)):
            (ox,oy) = POINTS_WRAP[j]
            (tx,ty) = (px+ox,py+oy)
            d2 = dist2((x,y),(tx,ty))
            if (d2 < d20):
                d21 = d20
                bc1 = bc0
                b1 = b0
                d20 = d2
                bc0 = (tx,ty)
                b0 = b
    return (bc0,b0,bc1,b1)

def spaced_points(count,rmin,rmax,nudge_count):
    global points
    points = []
    if count < 1:
        return
    for i in range(count):
        points.append((random.randint(0,W-1),random.randint(0,H-1)))
        points_radius.append(random.randint(rmin,rmax))
    nudge = W/4
    for nc in range(nudge_count):
        nudge = 10*(nudge_count-nc)/nudge_count
        #generate(pattern_spots).save("iter%2d.png" % nc)
        #print("iter: %d, nudge %f" % (nc,nudge))
        for i in range(len(points)):
            ((cx,cy),ci,(sx,sy),si) = closest_points(None,i)
            (x,y) = points[i]
            (dx,dy) = (x-cx,y-cy)
            if (dx == 0) and (dy == 0):
                dx = 1
            m = math.sqrt(mag2(dx,dy))
            n = nudge / m
            #print("%2d v %2d v %2d: (%3d,%3d) + (%3d,%3d) * %f" % (i,ci,si,x,y,dx,dy,n))
            points[i] = wrap2(x+(n*dx),y+(n*dy),W,H)
    for i  in range(len(points)): # re-integer points
        (x,y) = points[i]
        points[i] = wrap2(int(x),int(y),W,H)

def pattern_houndstooth(x,y):
    return pattern(x,y,8,8,houndstooth)

def pattern_battenburg(x,y):
    return pattern(x,y,32,32,battenburg)

def pattern_waves(x,y):
    y += math.sin((x+0.5)*math.pi/32) * 8
    if wrap(y,8) >= 4:
        return 1
    return 0

def pattern_spots(x,y):
    global points_radius
    x += 0.5
    y += 0.5
    ((cx,cy),ci,(sx,sy),si) = closest_points((x,y))
    d = math.sqrt(dist2((x,y),(cx,cy)))
    if d <= points_radius[ci]:
        return 1
    return 0

def pattern_voronoi(x,y):
    ((cx,cy),ci,(sx,sy),si) = closest_points((x,y))
    d0 = math.sqrt(dist2((x,y),(cx,cy)))
    d1 = math.sqrt(dist2((x,y),(sx,sy)))
    if (d1-d0) < 4:
        return 1
    return 0

#
# Main
#

if SEED == None:
    SEED = random.randint(0,(1<<32)-1)
random.seed(SEED)   

generators = [
    (pattern_houndstooth,0,0,0,0),
    #(pattern_battenburg,0,0,0,0),
    (pattern_spots,8,8,10,50),
    (pattern_voronoi,13,0,0,25),
    #(pattern_spots,300,1,1,5),
    (pattern_waves,0,0,0,0),
    ]
generated = []

for i in range(len(generators)):
    print("Texture: %d" % i)
    (g,pcount,pmin,pmax,pscale) = generators[i]
    spaced_points(pcount,pmin,pmax,pscale)
    img = generate(g)
    generated.append(img)
    img.save("temp/texture.%d.png" % i)
    tile_preview(img,WS,HS,W//2).save("temp/texture_tile.%d.png" % i)

texture_chr = bytearray()
for ty in range(0,H,8):
    for tx in range(0,W,8):
        tile = []
        for y in range(8):
            for x in range(8):
                b = 0
                for gimg in generated:
                    b = (b << 1) | gimg.getpixel((x+tx,y+ty))
                tile.append(b)
        for bp1 in range(0,4,2):
            for y in range(8):
                for bp0 in range(2):
                    b = 0
                    for x in range(8):
                        b = (b << 1) | (1 if 0 != (tile[(y*8)+x] & (1<<(bp0+bp1))) else 0)
                    texture_chr.append(b)
open("texture.chr","wb").write(texture_chr)
print("TEXTURE.CHR")

texture_pal = bytearray()
for i in range(4):
    for x in range(16):
        if 0 == (x & (1<<i)):
            texture_pal.append(0)
            texture_pal.append(0)
        else:
            texture_pal.append(0x21)
            texture_pal.append(0x04)
open("texture.pal","wb").write(texture_pal)
print("TEXTURE.PAL")

print("SEED: 0x%08X" % (SEED))
