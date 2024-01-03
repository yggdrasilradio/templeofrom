#!/usr/bin/env python

from PIL import Image

def coord(value):
	return '$' + format(value / 4, '02x')

def objectid(x, y):
	global pix, GREEN
	sum = 0
	for i in range(x, x + 7):
		for j in range(y, y + 7):
			color = pix[i, j]
			if color == GREEN:
				sum += 1
	return sum

def lineout(v1, c, v2):
	global r
	v1 = v1 / 4
	c = c / 4
	v2 = v2 / 4
	if v1 != v2:
		nsegments = int((v2 - v1) / int(127 / 4)) + 1
		if nsegments > 1:
			r = r + '* fcb ' + str(c) + ',' + str(v1) + ',' + str(v2) + ' in ' + str(nsegments) + ' segments:\n'
			delta = int((v2 - v1) / nsegments)
		if nsegments == 1:
			r = r + ' fcb ' + str(c) + ',' + str(v1) + ',' + str(v2) + '\n'
		elif nsegments == 2:
			r = r + ' fcb ' + str(c) + ',' + str(v1) + ',' + str(v1 + delta ) + '\n'
			r = r + ' fcb ' + str(c) + ',' + str(v1 + delta) + ',' + str(v2) + '\n'
		elif nsegments == 3:
			r = r + ' fcb ' + str(c) + ',' + str(v1) + ',' + str(v1 + delta) + '\n'
			r = r + ' fcb ' + str(c) + ',' + str(v1 + delta) + ',' + str(v1 + 2 * delta) + '\n'
			r = r + ' fcb ' + str(c) + ',' + str(v1 + 2 * delta) + ',' + str(v2) + '\n'
		else:
			r = r + '*ERROR\n'
			print "SEGMENTING ERROR"

img = Image.open('map.gif')
pix = img.load()

BLACK = pix[0, 0]
WHITE = pix[1, 0]
RED = pix[2, 0]		# unused
BLUE = pix[3, 0]
GREEN = pix[4, 0]
YELLOW = pix[5, 0]	# unused
MAGENTA = pix[6, 0]	# unused
CYAN = pix[7, 0]	# unused

CROSS = 5
ENTRYPOINT = 9
RING = 10
PORTAL = 17
CROWN = 20
CUP = 21
FIREBALL = 23
SPIDER = 24
GOBLET = 25
PITCHER = 26
SKULL = 28
BALL = 37
GHOST = 30

width = img.size[0]
height = img.size[1]

# check for misaligned sprites
i = 0
for x in range(4, width - 1):
	for y in range(4, height - 1):
		if pix[x, y] == WHITE:
			if x % 4 != 0:
				i += 1
if i != 0:
	print "There are " + str(i) + " misaligned sprites"

if BLACK == WHITE:
	print "Color mapping layer is missing"

# Generate lines.asm
print 'Generating lines.asm'
maxx = 0
minx = 9000
maxy = 0
miny = 9000
r = '* vertical lines\n'
r = r + 'vertscr0\n'
for x in range(4, width - 1, 4):
	flag = 0
	if x % 64 == 0:
		col = x / 4 + 1
		r = r +  'vertscr' + str(int(col / 16)) + '\n';
	for y in range(4, height - 1):
		color = pix[x, y]
		if color == BLUE and flag == 0:
			# start of line
			if x > maxx:
				maxx = x
			if x < minx:
				minx = x
			flag = 1
			x1 = x
			y1 = y
		elif color != BLUE and flag == 1:
			# end of line
			lineout(y1, x1, y)
			flag = 0;
r = r + ' fcb 0\n\n'

r = r + '* horizontal lines' + '\n'
r = r + 'horscr0' + '\n'
for y in range(4, height - 1, 4):
	flag = 0
	if y % 64 == 0:
		col = y / 4 + 1
		r = r +  'horscr' + str(int(col / 16)) + '\n';
	for x in range(4, width - 1):
		color = pix[x, y]
		if color == BLUE and flag == 0:
			# start of line
			if y > maxy:
				maxy = y
			if y < miny:
				miny = y
			flag = 1
			x1 = x
			y1 = y
		elif color != BLUE and flag == 1:
			# end of line
			lineout(x1, y1, x)
			flag = 0;
			delta = x - x1

with open('../lines.asm', 'r') as file:
	data = file.read().split('***')

data[1] = r

with open('lines.asm', 'w') as file:
	file.write("".join(data))

# Generate treasures.asm
print 'Generating treasures.asm'
cross = ''
ring = ''
crown = ''
cup = ''
goblet = ''
pitcher = ''
ball = ''
ntreasures = 0
for x in range(4, width - 1, 4):
	for y in range(4, height - 1, 4):
		if pix[x, y] == WHITE:
			objid = objectid(x, y)
			s = ' fcb ' + coord(x) + ',' + coord(y) + '\n'
			if objid == CROSS:
				cross += s
				ntreasures += 1
			elif objid == RING:
				ring += s
				ntreasures += 1
			elif objid == CROWN:
				crown += s
				ntreasures += 1
			elif objid == CUP:
				cup += s
				ntreasures += 1
			elif objid == GOBLET:
				goblet += s
				ntreasures += 1
			elif objid == PITCHER:
				pitcher += s
				ntreasures += 1
			elif objid == BALL:
				ball += s
				ntreasures += 1

with open('../treasures.asm', 'r') as file:
	data = file.read().split('***')

data[1] = cross
data[3] = ring
data[5] = cup
data[7] = ball
data[9] = goblet
data[11] = pitcher
data[13] = crown

with open('treasures.asm', 'w') as file:
	file.write("".join(data))

# Generate monsters.asm
print 'Generating monsters.asm'
r = ''
nmonsters = 0
for x in range(4, width - 1, 4):
	for y in range(4, height - 1, 4):
		if pix[x, y] == WHITE:
			objid = objectid(x, y)
			if objid == SPIDER or objid == FIREBALL or objid == GHOST or objid == SKULL:
				x1 = x
				while (pix[x1, y] != BLUE and pix[x1, y] != YELLOW):
					x1 -= 1
				x2 = x
				while (pix[x2, y] != BLUE and pix[x2, y] != YELLOW):
					x2 += 1
				y1 = y
				while (pix[x, y1] != BLUE and pix[x, y1] != YELLOW):
					y1 -= 1
				y2 = y
				while (pix[x, y2] != BLUE and pix[x, y2] != YELLOW):
					y2 += 1
				if objid == SPIDER:
					r += ' fcb ' + coord(x1) + ',' + coord(x2) + ',' + coord(y1) + ',' + coord(y2) + ',$00\n'
					nmonsters += 1
				elif objid == FIREBALL:
					r += ' fcb ' + coord(x1) + ',' + coord(x2) + ',' + coord(y1) + ',' + coord(y2) + ',$20\n'
					nmonsters += 1
				elif objid == GHOST:
					r += ' fcb ' + coord(x1) + ',' + coord(x2) + ',' + coord(y1) + ',' + coord(y2) + ',$40\n'
					nmonsters += 1
				elif objid == SKULL:
					r += ' fcb ' + coord(x1) + ',' + coord(x2) + ',' + coord(y1) + ',' + coord(y2) + ',$60\n'
					nmonsters += 1

with open('../monsters.asm', 'r') as file:
	data = file.read().split('***')

data[1] = r

with open('monsters.asm', 'w') as file:
	file.write("".join(data))

# Generate portals.asm
print 'Generating portals.asm'
r = ''
nportals = 0
xvalues = []
yvalues = []
for x in range(4, width - 1, 4):
	for y in range(4, height - 1, 4):
		if pix[x,y] == WHITE:
			if objectid(x, y) == PORTAL:
				nportals += 1
				xvalues.append(x)
				yvalues.append(y)
				
i = 0
for x in range(4, width - 1, 4):
	for y in range(4, height - 1, 4):
		if pix[x, y] == WHITE:
			if objectid(x, y)  == PORTAL:
				r = r + ' fdb $' + format(x, '04x') + ',$' + format(y, '04x') + '\n'
				r = r + ' fcb '
				for j in range(len(xvalues)):
					if i != j:
						r = r + coord(xvalues[j]) + ',' + coord(yvalues[j]) + ','
				r = r[:-1] + '\n'
				i += 1

with open('../portals.asm', 'r') as file:
	data = file.read().split('***')

data[0] = 'NPORTALS equ ' + str(nportals)
data[2] = r

with open('portals.asm', 'w') as file:
	file.write("".join(data))

# Generate constants.asm
print 'Generating constants.asm'
r = ''
for x in range(4, width - 1):
	for y in range(4, height - 1):
		if pix[x, y] == WHITE:
			if objectid(x, y) == ENTRYPOINT:
				r += 'STARTX equ ' + str(x + 2 - 64) + '\n'
				r += 'STARTY equ ' + str(y + 2 - 48) + '\n'
				r += 'MINX equ ' + str(minx) + '\n'
				r += 'MAXX equ ' + str(maxx) + '\n'
				r += 'MINY equ ' + str(miny) + '\n'
				r += 'MAXY equ ' + str(maxy) + '\n'
				r += 'NMONSTERS equ ' + str(nmonsters) + '\n'
				r += 'NTREASURES equ ' + str(ntreasures) + '\n'

with open('constants.asm', 'w') as file:
	file.write(r)

