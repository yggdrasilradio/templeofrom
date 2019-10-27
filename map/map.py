#!/usr/bin/python

from PIL import Image

def hout(x1, y, x2):
	x1 = x1 / 4
	y = y / 4
	x2 = x2 / 4
	if x1 <> x2:
		nsegments = int((x2 - x1) / int(127 / 4)) + 1
		if nsegments > 1:
			print '* fcb ' + str(y) + ',' + str(x1) + ',' + str(x2) + ' in ' + str(nsegments) + ' segments:'
			delta = int((x2 - x1) / nsegments)
		if nsegments == 1:
			print ' fcb ' + str(y) + ',' + str(x1) + ',' + str(x2)
		elif nsegments == 2:
			print ' fcb ' + str(y) + ',' + str(x1) + ',' + str(x1 + delta )
			print ' fcb ' + str(y) + ',' + str(x1 + delta) + ',' + str(x2)
		elif nsegments == 3:
			print ' fcb ' + str(y) + ',' + str(x1) + ',' + str(x1 + delta)
			print ' fcb ' + str(y) + ',' + str(x1 + delta) + ',' + str(x1 + 2 * delta)
			print ' fcb ' + str(y) + ',' + str(x1 + 2 * delta) + ',' + str(x2)
		else:
			print '*ERROR'

def vout(y1, x, y2):
	y1 = y1 / 4
	x = x / 4
	y2 = x2 / 4
	if y1 <> y2:
		nsegments = int((y2 - y1) / int(95 / 4)) + 1
		if nsegments > 1:
			print '* fcb ' + str(y1) + ',' + str(x) + ',' + str(y2) + ' in ' + str(nsegments) + ' segments:'
			delta = int((y2 - y1) / nsegments)
		if nsegments == 1:
			print ' fcb ' + str(y1) + ',' + str(x) + ',' + str(y2)
		elif nsegments == 2:
			print '* 2 SEGMENTS'
			print ' fcb ' + str(y1) + ',' + str(x) + ',' + str(y1 + delta)
			print ' fcb ' + str(y1 + delta) + ',' + str(x) + ',' + str(y2)
		elif nsegments == 3:
			print '* 3 SEGMENTS'
			print ' fcb ' + str(y1) + ',' + str(x) + ',' + str(y1 + delta)
			print ' fcb ' + str(y1 + delta) + ',' + str(x) + ',' + str(y1 + 2 * delta)
			print ' fcb ' + str(y1 + 2 * delta) + ',' + str(x) + ',' + str(y2)
		else:
			print '*ERROR'

img = Image.open('map.png')
pix = img.load()

width = img.size[0]
height = img.size[1]

# vertical lines
print '* vertical lines'
print 'vertscr0'
for x in range(4, width - 1):
	flag = 0
	if (x + 5) % 64 == 0:
		col = x / 4 + 1
		print 'vertscr' + str(int(col / 16 + 1))
	for y in range(0, height - 1):
		color = pix[x, y]
		if color == 0 and flag == 0:
			# start of line
			flag = 1
			x1 = x
			y1 = y
		elif color == 1 and flag == 1:
			# end of line
			hout(y1, x1, y)
			flag = 0;
print ' fcb 0'

# horizontal lines
print
print '* horizontal lines'
print 'horscr0'
for y in range(4, height - 1):
	flag = 0
	if (y + 5) % 64 == 0:
		col = y / 4 + 1
		print 'horscr' + str(int(col / 16 + 1))
	for x in range(0, width - 1):
		color = pix[x, y]
		if color == 0 and flag == 0:
			# start of line
			flag = 1
			x1 = x
			y1 = y
		elif color == 1 and flag == 1:
			# end of line
			hout(x1, y1, x)
			flag = 0;
			delta = x - x1
print ' fcb 0'
