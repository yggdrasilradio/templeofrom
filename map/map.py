#!/usr/bin/python

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
				sum = sum + 1
	return sum

def hout(x1, y, x2):
	global r
	x1 = x1 / 4
	y = y / 4
	x2 = x2 / 4
	if x1 <> x2:
		nsegments = int((x2 - x1) / int(127 / 4)) + 1
		if nsegments > 1:
			r = r + '* fcb ' + str(y) + ',' + str(x1) + ',' + str(x2) + ' in ' + str(nsegments) + ' segments:\n'
			delta = int((x2 - x1) / nsegments)
		if nsegments == 1:
			r = r + ' fcb ' + str(y) + ',' + str(x1) + ',' + str(x2) + '\n'
		elif nsegments == 2:
			r = r + ' fcb ' + str(y) + ',' + str(x1) + ',' + str(x1 + delta ) + '\n'
			r = r + ' fcb ' + str(y) + ',' + str(x1 + delta) + ',' + str(x2) + '\n'
		elif nsegments == 3:
			r = r + ' fcb ' + str(y) + ',' + str(x1) + ',' + str(x1 + delta) + '\n'
			r = r + ' fcb ' + str(y) + ',' + str(x1 + delta) + ',' + str(x1 + 2 * delta) + '\n'
			r = r + ' fcb ' + str(y) + ',' + str(x1 + 2 * delta) + ',' + str(x2) + '\n'
		else:
			r = r + '*ERROR\n'
			print "HORIZ SEGMENTING ERROR"

def vout(y1, x, y2):
	global r
	y1 = y1 / 4
	x = x / 4
	y2 = x2 / 4
	if y1 <> y2:
		nsegments = int((y2 - y1) / int(95 / 4)) + 1
		if nsegments > 1:
			r = r + '* fcb ' + str(y1) + ',' + str(x) + ',' + str(y2) + ' in ' + str(nsegments) + ' segments:\n'
			delta = int((y2 - y1) / nsegments)
		if nsegments == 1:
			r = r + ' fcb ' + str(y1) + ',' + str(x) + ',' + str(y2) + '\n'
		elif nsegments == 2:
			r = r + '* 2 SEGMENTS\n'
			r = r + ' fcb ' + str(y1) + ',' + str(x) + ',' + str(y1 + delta) + '\n'
			r = r + ' fcb ' + str(y1 + delta) + ',' + str(x) + ',' + str(y2) + '\n'
		elif nsegments == 3:
			r = r + '* 3 SEGMENTS\n'
			r = r + ' fcb ' + str(y1) + ',' + str(x) + ',' + str(y1 + delta) + '\n'
			r = r + ' fcb ' + str(y1 + delta) + ',' + str(x) + ',' + str(y1 + 2 * delta) + '\n'
			r = r + ' fcb ' + str(y1 + 2 * delta) + ',' + str(x) + ',' + str(y2) + '\n'
		else:
			r = r + '*ERROR\n'
			print "VERT SEGMENTING ERROR"


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

width = img.size[0]
height = img.size[1]

# check for misaligned sprites
i = 0
for x in range(4, width - 1):
	for y in range(4, height - 1):
		if pix[x, y] == WHITE:
			if x % 4 <> 0:
				i = i + 1
if i <> 0:
	print "There are " + str(i) + " misaligned sprites"

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
		elif color <> BLUE and flag == 1:
			# end of line
			hout(y1, x1, y)
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
		elif color <> BLUE and flag == 1:
			# end of line
			hout(x1, y1, x)
			flag = 0;
			delta = x - x1

with open('../lines.asm', 'r') as file:
	data = file.read().split('***')

data[1] = r

with open('lines.asm', 'w') as file:
	file.write("".join(data))

# Generate treasures.asm
print 'Generating treasures.asm'
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
SKULL = 34
BALL = 37
GHOST = 35

cross = ''
ring = ''
crown = ''
cup = ''
goblet = ''
pitcher = ''
ball = ''
for x in range(4, width - 1, 4):
	for y in range(4, height - 1, 4):
		if pix[x, y] == WHITE:
			objid = objectid(x, y)
			s = ' fcb ' + coord(x) + ',' + coord(y) + '\n'
			if objid == CROSS:
				cross = cross + s
			elif objid == RING:
				ring = ring + s
			elif objid == CROWN:
				crown = crown + s
			elif objid == CUP:
				cup = cup + s
			elif objid == GOBLET:
				goblet = goblet + s
			elif objid == PITCHER:
				pitcher = pitcher + s
			elif objid == BALL:
				ball = ball + s

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
for x in range(4, width - 1, 4):
	for y in range(4, height - 1, 4):
		if pix[x, y] == WHITE:
			objid = objectid(x, y)
			if objid == SPIDER or objid == FIREBALL or objid == GHOST or objid == SKULL:
				x1 = x
				while (pix[x1, y] <> BLUE):
					x1 -= 1
				x2 = x
				while (pix[x2, y] <> BLUE):
					x2 += 1
				y1 = y
				while (pix[x, y1] <> BLUE):
					y1 -= 1
				y2 = y
				while (pix[x, y2] <> BLUE):
					y2 += 1
				if objid == SPIDER:
					r += ' fcb ' + coord(x1) + ',' + coord(x2) + ',' + coord(y1) + ',' + coord(y2) + ',$00\n'
				elif objid == FIREBALL:
					r += ' fcb ' + coord(x1) + ',' + coord(x2) + ',' + coord(y1) + ',' + coord(y2) + ',$20\n'
				elif objid == GHOST:
					r += ' fcb ' + coord(x1) + ',' + coord(x2) + ',' + coord(y1) + ',' + coord(y2) + ',$40\n'
				elif objid == SKULL:
					r += ' fcb ' + coord(x1) + ',' + coord(x2) + ',' + coord(y1) + ',' + coord(y2) + ',$60\n'

with open('../monsters.asm', 'r') as file:
	data = file.read().split('***')

data[1] = r

with open('monsters.asm', 'w') as file:
	file.write("".join(data))

# Generate geometry.asm
print 'Generating geometry.asm'
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

with open('geometry.asm', 'w') as file:
	file.write(r)

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
				nportals = nportals + 1
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
					if i <> j:
						r = r + coord(xvalues[j]) + ',' + coord(yvalues[j]) + ','
				r = r[:-1] + '\n'
				i = i + 1

with open('../portals.asm', 'r') as file:
	data = file.read().split('***')

data[0] = 'NPORTALS equ ' + str(nportals)
data[2] = r

with open('portals.asm', 'w') as file:
	file.write("".join(data))
