; Disassembly of Temple of ROM by William Astle
; (February 20-25, 2018)
; 16 character tabs

; General memory map:
; 0000-00FF	direct page variables
; 0100-03FF	other variables, stack
; 0400-0FFF	graphics screen #1
; 1000-1BFF	graphics screen #2
; 1C00		explosion sprite queue
; 2000		code

 IFDEF MCUSTOM
 include map/constants.asm
 ELSE
 include constants.asm
 ENDC

PIA0.DA	equ $ff00
PIA0.CA	equ $ff01
PIA0.DB	equ $ff02
PIA0.CB	equ $ff03
PIA1.DA	equ $ff20
PIA1.CA	equ $ff21
PIA1.DB	equ $ff22
PIA1.CB	equ $ff23
SAM	equ $ffc0

; References to Color Basic ROM APIs
RSTFLG	equ $71
RSTVEC	equ $72

*** SEGMENT 1 ***

	setdp 0

monsterptr rmb 2 ; pointer to current player's monster locations
portaloff rmb 1	; nonzero means portals are currently disabled
V03	rmb 2
V05	rmb 2
V07	rmb 2
V09	rmb 2
V0B	rmb 2
V0D	rmb 2
V0F	rmb 2
V11	rmb 2
V13	rmb 2
V15	rmb 1
V18	rmb 1 ; is crown active?
V19	rmb 1 ; is crystal ball active?
V1A	rmb 1
V4F	rmb 1
V50	rmb 2 ; bat x position
V52	rmb 2 ; bat y position
V5C	rmb 1 ; bat creation timer
V5D	rmb 2 ; target coord x
V5F	rmb 2 ; target coord y
V68	rmb 2 ; object coord x
V8D	rmb 1 ; object coord y
randseed rmb 2 ; the "random seed"
mazeoffx rmb 2 ; horizontal offset in maze of top left of screen
mazeoffy rmb 2 ; vertical offset in maze of top left of screen
curposx	rmb 2 ; current player horizontal screen position
curposy	rmb 2 ; current player vertical screen position
tcoord	rmb 1
renderscr rmb 2
endclear rmb 1 ; lowest address (highest location on screen) to clear
VBD	rmb 1
color	rmb 1
VBF	rmb 2
VC1	rmb 2
VC3	rmb 1
VC4	rmb 1
VC5	rmb 1
VC6	rmb 1
VC7	rmb 1
VC8	rmb 1
VC9	rmb 1
VCA	rmb 1
VCB	rmb 2
VCD	rmb 2
VCF	rmb 1
VD0	rmb 1
VD1	rmb 1 ; "walk through walls" flag
scorep1	rmb 3 ; player one's score
VD5	rmb 1 ; player dead flag
VD6	rmb 1
VD7	rmb 1 ; laser sound value
VD8	rmb 1
VD9	rmb 1
VDA	rmb 1	; treasure count
collision rmb 1 ; collision detection flag
VDC	rmb 1 ; number of treasures remaining

* hitbox
hity1	rmb 1 ; miny
hity2	rmb 1 ; maxy
hitx1	rmb 1 ; minx
hitx2	rmb 1 ; maxx

numplayers	rmb 1 ; number of players in the game
scrollstep	rmb 2 ; step/direction for maze scrolling
plr1state	rmb 6 ; player two game state (6 bytes)
plr2state	rmb 6 ; player one game state (6 bytes)
scorep2		rmb 3 ; player two's score
scoreptr	rmb 2 ; pointer to current player's score
texttty		rmb 1 ; whether the "beeping tty" effect is enabled for text
objlistptr	rmb 2 ; pointer to current player's treasure list
curplayer	rmb 1 ; current player number (oddly, 2 = player 1, 1 = player 2)
VF9	rmb 2 ; bat sprite (or zero if bat inactive)
VFA	rmb 1 ; bat wing flap state
POTVAL	rmb 4 ; joystick values
temp	rmb 1
tick	rmb 1 ; IRQ countdown timer
tock	rmb 1 ; IRQ countup timer
aggro	rmb 1 ; aggro flag


* SCREEN 1
*
*	128 x 96, 4 color
*

SCREEN1		equ $400	; SCREEN 1 (3072 bytes)

* SCREEN 2
*
*	128 x 96, 4 color
*

SCREEN2		equ $1000	; SCREEN 2 (3072 bytes)

	org $1c00

* EXPLOSION SPRITE QUEUE
*
* 8 items of 4 bytes each
*
* Each list item is 4 bytes:
*	sprite	2 bytes (address of pointer to current sprite)
*	X	1 byte (X coordinate divided by 4)
*	Y	1 byte (Y coordinate divided by 4)

XQUEUE	rmb 8*4

* MASTER MONSTER LIST
*
* Each list entry is 9 bytes:
*	X1	2 bytes
*	X2	2 bytes
*	Y1	2 bytes
*	Y2	2 bytes
*	ID	1 byte ($00 = spider, $20 = fireball, $40 = ghost, $60 = skull)
*
*	The box defined by (X1, Y1, X2, Y2) is the monster's aggro area, with the monster's initial position
*	25% down from the left and top edges
*
*	list ends with $0000

monsters rmb NMONSTERS*9+2

* PLAYER 1 TREASURE LIST
*
* Each list item is 4 bytes:
*	X	1 byte (X coordinate divided by 4)
*	Y	1 byte (Y coordinate divided by 4)
*	sprite	2 bytes (address of treasure sprite)

plr1objlist rmb NTREASURES*4

* PLAYER 1 MONSTER LIST
*
* Each list entry is 4 bytes:
*	X	2 bytes
*	Y	2 bytes

plr1monsters rmb NMONSTERS*4

* PLAYER 2 TREASURE LIST
*
* Each list item is 4 bytes:
*	X	1 byte (X coordinate divided by 4)
*	Y	1 byte (Y coordinate divided by 4)
*	sprite	2 bytes (address of treasure sprite)

plr2objlist rmb NTREASURES*4
	
* PLAYER 2 MONSTER LIST
*
* Each list entry is 4 bytes:
*	X	2 bytes
*	Y	2 bytes

plr2monsters	 rmb NMONSTERS*4

	org $3000

START	orcc #$50			; make sure interrupts are disabled
	clr $ff40			; make sure all FDC drive motors and selects are off
	lbra LCD46			; launch main initialization sequence

	include sound.asm       ; fetch sound handling routines
	include joystick.asm    ; fetch joystick handling routines

; Render the vertical lines of the map
drawvert leau LC34A,pcr		; point to short circuit offset table for vertical lines
	ldd mazeoffx		; fetch screen display offset for maze
	bge LC013		; brif screen X offset is positive
	ldd #0			; minimize to 0
LC013	lslb			; shift upper 2 bits of position into A
	rola
	lslb
	rola
	lsla			; two bytes per offset
	ldd a,u			; get offset from start of offset table to the actual data to start rendering
	leau d,u		; point to actual render data
LC01C	clra			; zero extend next read
	pulu b			; fetch column number
	tstb			; end of data?
	beq LC06F		; brif so
	lslb			; multiply by 4
	rola
	lslb
	rola
	subd mazeoffx		; compare to maze offset
	blt LC06B		; brif off the left of the screen
	cmpd #$7f		; are we off the right side of the screen?
	bgt LC06F		; brif so - don't render anything
	tfr d,x			; save offset from left of the screen
	clra			; zero extend next read
	pulu b			; get top coordinate of line
	lslb			; times 4
	rola
	lslb
	rola
	subd mazeoffy		; compare to screen offset (vertical)
	cmpd #8			; is it in the header or off the screen?
	bge LC043		; brif not
	ldb #8			; force to start at top of visible area
LC043	cmpd #$5f		; are we at the bottom of the screen?
	ble LC04B		; brif not
	ldb #$5f		; force to end at the bottom of the screen
LC04B	pshs b			; save first Y coordinate
	clra			; zero extend next read
	pulu b			; fetch bottom Y coordinate (offset from top of screen)
	lslb			; times 4
	rola
	lslb
	rola
	subd mazeoffy		; compare to screen offset (vertical)
	bge LC059		; brif below top of screen
	clrb			; normalize into screen
LC059	cmpd #$5f		; are we off the bottom of the screen
	ble LC061		; brif not
	ldb #$5f		; normalize into screen
LC061	lda ,s			; get adjusted top coordinate back
	cmpb ,s+		; is the bottom coordinate below the top coordinate?
	bls LC01C		; brif so - we don't need to render anything
	bsr LC070		; draw line
	bra LC01C		; go check next line to render
LC06B	leau 2,u		; move to next line to consider
	bra LC01C		; go render it if necessary
LC06F	rts

* Draw vertical line
LC070	pshs a			; save top coordinate
	pshs b			; save bottom coordinate
	tfr x,d			; stuff the horizontal coordinate into an accumulator
	andb #3			; figure out which pixel in the byte we're at
	leay LC09A,pcr			; point to pixel bit masks
	lda b,y			; get the proper pixel bit mask
	ldb ,s+			; get bottom coordinate
	subb ,s			; subtract out top coordinate (number of pixels to do)
	exg d,x			; get back horizontal coordinate and save count and pixel mask
	lda ,s+			; get top coordinate
	rolb			; calcuate screen offset
	lsra
	rorb
	lsra
	rorb
	lsra
	rorb
	addd renderscr		; add to base screen address
	exg d,x			; put into pointer and get back pixel mask and counter

LC091	sta ,x			; set pixel for line
	leax $20,x		; move to next row
	decb			; done last pixel?
	;bne LC091		; brif not FIXED BUG!
	bpl LC091

	rts

LC09A	fcb $40,$10,$04,$01	; pixel masks for maze walls (color #1)

drawhoriz leau LC36A,pcr	; point to short circuit offsets for horizontal drawing
	ldd mazeoffy		; get vertical offset of screen
	bge LC0A9		; brif valid coordinate
	ldd #0			; minimize to 0
LC0A9	lslb			; get upper 4 bits of offset into A
	rola
	lslb
	rola
	lsla			; two bytes per offset entry
	ldd a,u			; get offset from start of offset table
	leau d,u		; point to data to render
LC0B2	clra			; zero extend for next read
	pulu b			; get horizontal coordinate
	tstb			; end of data?
	beq LC104		; brif so
	lslb			; times 4
	rola
	lslb
	rola
	subd mazeoffy		; get offset relative to screen position
	cmpd #8			; are we above the screen or in the header?
	blt LC100		; brif so
	cmpd #$5f		; are we below the bottom of the screen?
	bgt LC100		; brif so
	tfr d,x			; save vertical offset for later
	clra			; zero extend for next read
	pulu b			; get left coordinate for line
	lslb			; times 4
	rola
	lslb
	rola
	subd mazeoffx		; offset relative to the screen position
	bge LC0D8		; brif not off the left side
	clrb			; normalize to left side
LC0D8	cmpd #$7f		; off the right of the screen?
	ble LC0E0		; brif not
	ldb #$7f		; normalize to right side
LC0E0	pshs b			; save left coordinate
	clra			; zero extend next read
	pulu b			; fetch right coordinate
	lslb			; times 4
	rola
	lslb
	rola
	subd mazeoffx		; offset according to the screen position
	bge LC0EE		; brif not off left side of screen
	clrb			; normalize to left side of screen
LC0EE	cmpd #$7f		; are we off the right side of the screen?
	ble LC0F6		; brif not
	ldb #$7f		; normalize to right side of screen
LC0F6	lda ,s			; get left coordinate back
	cmpb ,s+		; is right coordinate left of left coordinate?
	bls LC0B2		; brif so (or same as)
	bsr LC105		; go draw line
	bra LC0B2		; go handle next line
LC100	leau 2,u		; move to next set of line data
	bra LC0B2		; go render it if needed
LC104	rts

* Draw horizontal line
* A: beginning X coordinate
* B: ending X coordinate
* X: Y coordinate
LC105	pshs a			; save left coordinate
	exg x,d			; save coordinates and get vertical offset
	tfr b,a			; put vertical coordinate in A
	ldb ,s			; get left coordinate
	rolb			; calcuate screen offset
	lsra
	rorb
	lsra
	rorb
	lsra
	rorb
	addd renderscr		; add screen base
	exg d,x			; save pointer and get back coordinates
	subb ,s+		; calculate number of pixels
	incb			; add one (compensate for decb below)
	leay LC09A,pcr		; point to pixel masks
	anda #3			; get pixel number in byte
	lda a,y			; get pixel mask
	sta temp

LC125	decb			; are we done yet?
	blt LC148		; brif so
	lda temp
	ora ,x			; merge with screen data
	sta ,x			; save on screen
	lsr temp
	lsr temp
	bne LC125		; brif we haven't got to the end of the byte
	leax 1,x		; move to next byte
	lda #$55		; set up to do whole bytes
LC138	subb #4			; do we have a whole byte worth?
	blt LC140		; brif not
	sta ,x+			; do a whole byte
	bra LC138		; try again
LC140	addb #4			; reset for subb above
	lda #$40		; set up for leftmost pixel in byte
	sta temp		; set pixel masks
	bra LC125		; go complete the line

LC148	rts

; 00 black
; 01 blue
; 10 red
; 11 white

; explosion sprites

explosion
	fcb explosion1-*
	fcb explosion2-*
	fcb explosion3-*
	fcb explosion4-*
	fcb $00 ; end of table marker

explosion1
	fcb $00,$00 ; ........
	fcb $00,$00 ; ........
	fcb $03,$00 ; ...W....
	fcb $0f,$c0 ; ..WWW...
	fcb $03,$00 ; ...W....
	fcb $00,$00 ; ........
	fcb $00,$00 ; ........
	fcb $00,$00 ; ........

explosion2
	fcb $c0,$0c ; W.....W.
	fcb $30,$30 ; .W...W..
	fcb $0e,$c0 ; ..WRW...
	fcb $0a,$80 ; ..RRR...
	fcb $0e,$c0 ; ..WRW...
	fcb $30,$30 ; .W...W..
	fcb $c0,$0c ; W.....W.
	fcb $00,$00 ; ........

explosion3
	fcb $00,$20 ; .....R..
	fcb $20,$c0 ; .R..W...
	fcb $08,$80 ; ..R.R...
	fcb $ca,$00 ; W.RR....
	fcb $0c,$80 ; ..W.R...
	fcb $00,$00 ; ........
	fcb $80,$30 ; R....W..
	fcb $00,$00 ; ........

explosion4
	fcb $00,$00 ; ........
	fcb $0c,$00 ; ..W.....
	fcb $00,$20 ; .....R..
	fcb $03,$00 ; ...W....
	fcb $08,$00 ; ..R.....
	fcb $00,$00 ; ........
	fcb $00,$00 ; ........
	fcb $00,$00 ; ........

; digit glyphs; this is used as a pointer to digit #2 simply because
; the A,r indexing mode is signed and digits 8 and 9 wouldn't be
; contiguous otherwise
digits	fcb $3c,$00,$c3,$00,$c3,$00,$c3,$00	; 0
	fcb $3c,$00,$00,$00,$00,$00,$00,$00
	fcb $0c,$00,$3c,$00,$0c,$00,$0c,$00	; 1
	fcb $3f,$00,$00,$00,$00,$00,$00,$00
	fcb $fc,$00,$03,$00,$3c,$00,$c0,$00	; 2
	fcb $ff,$00,$00,$00,$00,$00,$00,$00
	fcb $fc,$00,$03,$00,$3c,$00,$03,$00	; 3
	fcb $fc,$00,$00,$00,$00,$00,$00,$00
	fcb $0f,$00,$33,$00,$c3,$00,$ff,$00	; 4
	fcb $03,$00,$00,$00,$00,$00,$00,$00
	fcb $ff,$00,$c0,$00,$fc,$00,$03,$00	; 5
	fcb $fc,$00,$00,$00,$00,$00,$00,$00
	fcb $3f,$00,$c0,$00,$fc,$00,$c3,$00	; 6
	fcb $3c,$00,$00,$00,$00,$00,$00,$00
	fcb $ff,$00,$03,$00,$0c,$00,$30,$00	; 7
	fcb $30,$00,$00,$00,$00,$00,$00,$00
	fcb $3c,$00,$c3,$00,$3c,$00,$c3,$00	; 8
	fcb $3c,$00,$00,$00,$00,$00,$00,$00
	fcb $3c,$00,$c3,$00,$3f,$00,$03,$00	; 9
	fcb $fc,$00,$00,$00,$00,$00,$00,$00

plr1mess fcb 10
	fcc 'PLAYER ONE'

plr2mess fcb 10
	fcc 'PLAYER TWO'

ToRmess	fcb 13
	fcc 'TEMPLE OF ROM'

gameovermess fcb 9
	fcc 'GAME OVER'

oneplrmess fcb 10
	fcc 'ONE PLAYER'

twoplrmess fcb 11
	fcc 'TWO PLAYERS'

copyrmess fcb 14
	fcc 'COPYRIGHT 2019 '

authmess fcb 13
	fcc 'BY RICK ADAMS '

;fest1 fcb 10
	;fcc "WELCOME TO"

;fest2 fcb 13
	;fcc "COCOFEST 2019"

;licmess fcb 11
;	fcc 'LICENSED TO '

;tandymess fcb 17
;	fcc 'TANDY CORPORATION '

;rightsmess fcb 19
;	fcc 'ALL RIGHTS RESERVED'

fontidx	fcc 'ABCDEFGHI1KLMNOP9RST8VW4Y235JQXZU6790 '	; font index character list
; Font data. Each character is encoded as 32 bits. The character matrix is 5x6 with
; the bits packed left to right and top down starting at the msb of each byte.
fontdata fcb $f0,$5f,$17,$80		; A
	fcb $f4,$7d,$1f,$00		; B
	fcb $74,$61,$17,$00		; C
	fcb $f4,$63,$1f,$00		; D
	fcb $74,$7d,$07,$80		; E
	fcb $74,$79,$08,$00		; F
	fcb $7c,$27,$17,$00		; G
	fcb $85,$b3,$18,$80		; H
	fcb $42,$10,$83,$00		; I
	fcb $23,$08,$47,$00		; 1
	fcb $8c,$b9,$28,$80		; K
	fcb $84,$21,$0f,$80		; L
	fcb $55,$6b,$58,$80		; M
	fcb $b6,$63,$18,$80		; N
	fcb $74,$63,$17,$00		; O (also used for 0)
	fcb $f4,$63,$e8,$00		; P
	fcb $74,$5e,$1f,$00		; 9
	fcb $be,$21,$08,$00		; R
	fcb $7c,$1c,$1f,$00		; S
	fcb $f9,$10,$83,$80		; T
	fcb $74,$5d,$17,$00		; 8
	fcb $8c,$62,$a2,$00		; V
	fcb $8c,$6b,$55,$00		; W
	fcb $32,$a5,$f1,$00		; 4
	fcb $8c,$54,$42,$00		; Y

* ADDED GLYPHS NOW THAT WE HAVE MORE RAM
	fqb %11110000010111010000111110000000 ; 2
	fqb %11110000010111000001111100000000 ; 3
	fqb %11111100001111000001111100000000 ; 5
	fqb %00001000010000110001011100000000 ; J
	fqb %01110100011000101001101100000000 ; Q
	fqb %10001010100010001010100010000000 ; X
	fqb %11111000100010001000111110000000 ; Z
	fqb %10001100011000110001011100000000 ; U
	fqb %01110100001111010001011100000000 ; 6
	fqb %11111000010001000100001000000000 ; 7
	fqb %01110100010111100001011100000000 ; 9
	fcb $74,$63,$17,$00		; O (also used for 0)

	fcb $00,$00,$00,$00		; space

 IFDEF MCUSTOM
 include map/lines.asm
 ELSE
 include lines.asm
 ENDC


*** SEGMENT 2 ***

LCD46	lbra LCF50

	include music.asm

; This generates a pseudo random sequence with period 128 in B
random	pshs a			; save registers
	lda randseed		; get current random seed
	ldb #123		; get fudge factor
	mul			; multiply
	addb #9			; add something for fun
	stb randseed		; save new random seed (and result)
	puls a			; restore registers
	rts

* Better random routine from Steve Bjork
lrandom
	clr  ,-s	; clear holder of LFSB
	lda randseed    ; get high byte of 16-bit random seed
	anda #%10110100	; get the bits check in shifting
	ldb #6		; use the top 6 bits for xoring
loop@
	lsla            ; move top bit into the carry flag
	bcc no@		; skip incing the LFSB if no carry
	inc ,s		; add one to the LFSB test holder
no@
	decb            ; count down loop counter
	bne loop@       ; loop if all bits are not done
	lda ,s+         ; get LFSB off of stack
	inca            ; invert lower bit by adding one
	rora            ; move bit 0 into carry
	rol randseed+1  ; shift carry into bit 0
	rol randseed    ; one more shift to complete the 16 bit shift
	ldd randseed    ; load up a and b with the new random seed
	rts

* Generate random number between zero and A
rand
	pshs d
	lbsr lrandom
	puls a
	mul
	puls b,pc

; Wait for VSYNC
WaitVSYNC
	pshs a
loop@	lda tick
	bne loop@
	lda #5
	sta tick
	sync
	puls a,pc

; Clear screen one header (to color #3)
clrheader pshs y,x,b,a		; save registers
	ldx #SCREEN1		; point to start of screen #1
	ldy #$80		; set up to clear 256 bytes (2 bytes at a time)
	ldd #$ffff		; set up to use color #3
clrheader0 std ,x++		; clear out two bytes
	leay -1,y		; done yet?
	bne clrheader0		; brif not
	puls y,x,b,a		; restore registers
	rts

; Copy screen one header to screen two
dupheader pshs y,x,b,a		; save registers
	ldx #$500		; point to end of 8th row on screen #1
	ldy #$1100		; point to end of 8th row on screen #2
dupheader0 ldd ,--x		; copy two bytes from screen #1 to screen #2
	std ,--y
	cmpy #SCREEN2		; at start of screen?
	bgt dupheader0		; brif not
	puls y,x,b,a		; restore registers
	rts

* Is player inside aggro area?
; D: player coord
; X: aggro box left/top
; Y: aggro box right/bottom
LCF3C	pshs b,a
	cmpx ,s
	bhi LCF4B
	cmpy ,s
	blo LCF4B
	orcc #4
	bra LCF4D
LCF4B	andcc #$fb
LCF4D	puls b,a
	rts

*** SEGMENT 3 ***

LCF50	nop			; flag for valid warm start routine
	orcc #$50		; make sure interrupts are disabled
	clrb			; set direct page to page 0
	tfr b,dp
	leau LCF50,pcr		; install reset handler
	stu RSTVEC
	lda #$55		; flag for reset vector as valid
	lds #$3ff		; put stack somewhere safe
	sta RSTFLG		; mark reset vector as valid
	lda #$f8		; code for 2 color 256px graphics, color set 1
	sta PIA1.DB		; set VDG graphics mode
	sta SAM+5		; set SAM V2 (32 bytes per row, 96 rows)

	* init VSYNC interrupt
	clr tick
	leau IRQ,pcr		; IRQ interrupt vector
	stu $10d
	lda PIA0.CA		; turn off HSYNC
	anda #$fe
	sta PIA0.CA
	lda PIA0.CB		; turn on VSYNC
	ora #$01
	sta PIA0.CB
	andcc #%11101111	; start VSYNC interrupt on IRQ


	* Seed random number routine
	addd $112		; throw in the BASIC timer
	bne no@			; can't be zero
	ldd #123
no@	std randseed

	ldd #SCREEN2		; set render screen to screen #2
	std renderscr
	lbsr Coco3RGB 		; default to RGB no artifacting
	ldd #SCREEN1		; set bottom of screen to clear to start of screen #1
	std endclear
	lbsr LD3B9		; clear both screens
	lbsr clearscores	; reset both players scores
	clr numplayers		; set to no players

	leax LDD23,pcr		; point to monster data table
	ldy #monsters		; point to unpacked location for monster data table
LCF85	ldd ,x			; are we at the end of the table?
	beq LCF92		; brif so
	lbsr LD0C1		; expand the base/offset pairs for the monster
	lda ,x+
	sta ,y+
	bra LCF85		; go handle another

LCF92	std ,y			; save end of table flag

LCF94	lbsr LD531
	clr V03
	ldu #plr1objlist	; point to player one's treasure list
	lbsr buildobjlist	; build treasure list
	ldu #plr2objlist	; point to player two's treasure list
	lbsr buildobjlist	; build treasure list
	ldu #plr1monsters	; point to player one's monster locations
	lbsr LDCE6		; set default locations
	ldu #plr2monsters	; point to player two's monster locations
	lbsr LDCE6		; set default locations

	lbsr setstartpos	; set default attract mode scrolling start
	pshs d
	ldd #MINX+((MAXX-MINX)/2)-(128/2)
	std mazeoffx
	ldd #MINY+((MAXY-MINY)/2)-(96/2)
	std mazeoffy
	puls d

	clr VD7
	ldd #1			; set scroll direction to down-right
	std scrollstep
	clr texttty		; enable "tty" effect
LCFBD 	tst numplayers		; did we have a game running?
	beq LCFDF		; brif not
	leau gameovermess+1,pcr	; point to game over message
	lbsr showmess		; show it
	lbeq LD05E		; brif button pressed
	lda #20			; 20 scroll iterations
	lbsr scrollmaze		; go scroll the maze
	lbne LD05E		; brif button pressed
	lbsr showscore		; show the scores
	lda #$ff		; do a really long maze scroll (255 iterations)
	lbsr scrollmaze		; actually do the scrolling
	bne LD05E		; brif button pressed
LCFDF	leau ToRmess+1,pcr	; point to "TEMPLE OF ROM" message
	lda texttty		; get the current "tty" state
	sta VBF			; save it for later
	beq LCFF1		; brif enabled
	inc V5C			; bump counter
	ldb V5C			; fetch counter
	andb #3			; wrap at 3
	stb texttty		; save result as "tty" state (will do the "tty" thing every fourth time)
LCFF1	lbsr showmess		; show the "TEMPLE OF ROM" message
	beq LD05E		; brif button pressed
	lda VBF			; get saved "tty" state
	sta texttty		; restore "tty" state
	lbsr scrolllong		; go scroll for a while
	bne LD05E		; brif button pressed
	leau vermess+1,pcr	; point to version string
	lbsr showmess		; show it
	beq LD05E		; brif button pressed
	lda #20			; scroll for 20 steps
	lbsr scrollmaze		; do the scrolling
	leau copyrmess+1,pcr	; point to copyright message
	lbsr showmess		; show it
	beq LD05E		; brif button pressed
	lda #20			; scroll for 20 steps
	lbsr scrollmaze		; do the scrolling
	leau authmess+1,pcr	; point to author message
	lbsr showmess		; show it
	beq LD05E		; brif button pressed
	lbsr scrolllong		; do a long scroll
	bne LD05E		; brif button pressed

	;leau fest1+1,pcr	; "Welcome to"
	;lbsr showmess		; show it
	;beq LD05E		; brif button pressed
	;lda #20		; scroll for 20 steps
	;lbsr scrollmaze	; do the scrolling
	;leau fest2+1,pcr	; "CocoFEST 2019"
	;lbsr showmess		; show it
	;beq LD05E		; brif button pressed
	;lbsr scrolllong		; do a long scroll
	;bne LD05E		; brif button pressed

	;leau licmess+1,pcr	; point to licensing message
	;lbsr showmess		; show it
	;beq LD05E		; brif button pressed
	;lda #20		; scroll for 20 steps
	;lbsr scrollmaze	; do the scrolling
	;bne LD05E		; brif button pressed
	;leau tandymess+1,pcr	; point to tandy message
	;lbsr showmess		; show message
	;beq LD05E		; brif button pressed
	;lda #20		; scroll for 20 steps
	;lbsr scrollmaze	; do the scrolling
	;bne LD05E		; brif button pressed
	;leau rightsmess+1,pcr	; point to rights message
	;lbsr showmess		; show messages
	;beq LD05E		; brif button pressed
	;lbsr scrolllong	; do a long scroll
	;bne LD05E		; brif button pressed

	lda #$ff		; disable the "tty" effect
	sta texttty
	lbra LCFBD		; restart the intro loop

LD05E	lbsr setstartpos	; set default start position
	lbsr drawmazeboth	; draw maze on both screens
	clr texttty		; enable the "tty" effect
	lda #$ff
	sta VD1			; prevent laser firing during attract mode?
	sta V03
	lbsr LD144
	lbsr LD1AC
LD072	ldb PIA0.DA		; read keyboard rows/buttons
	andb #3			; keep joystick buttons
	eorb #3			; set nonzero if pressed
	bne LD072		; brif button pressed - we're waiting until the button is released
	clr numplayers		; set to no players
LD07D	jsr GETJOY		; read joysticks
	lda POTVAL+2		; get X coordinate for first player
	cmpa #$20		; is it to the left?
	bgt LD09B		; brif not
	lda numplayers		; get number of players
	cmpa #1			; already set to one?
	beq LD0AC		; brif so
	leau oneplrmess+1,pcr	; point to one player message
	lbsr showmess		; display it
	lda #1			; set to one player
	sta numplayers
	bra LD0AC
LD09B	lda numplayers		; get number of players
	cmpa #2			; set for two?
	beq LD0AC		; brif so
	leau twoplrmess+1,pcr	; point to two player message
	lbsr showmess		; display it
	lda #2			; set to two players
	sta numplayers
LD0AC	ldb PIA0.DA		; read keyboard rows (buttons)
	andb #3			; keep joystick buttons
	eorb #3			; now pressed is 1
	beq LD07D		; brif not buttons pressed
	lbsr clearscores	; reset both players scores
	bsr LD0CD		; run game loop for first life
	bsr LD0CD		; run game loop for second life
	bsr LD0CD		; run game loop for third life
	lbra LCF94		; go back to main intro screen

; Take two 8 bit coordinate pairs which have been divided by four and expand them
; to their full value. Store the resulting coordinates at Y and move Y
; forward.
LD0C1	bsr LD0C3		; handle first pair
LD0C3	bsr LD0C5		; handle first value in pair
LD0C5	lda ,x+			; fetch value
	ldb #4			; muliply by 4 (gives 16 bit value)
	mul
	std ,y++		; save resulting value
	rts

LD0CD	bsr LD0EE		; run game loop for player one
	lda numplayers		; get number of players
	cmpa #1			; only one player?
	beq LD0D8		; brif so
	lbsr LD156		; run game loop for player two
LD0D8	rts

setstartpos pshs a,b		; save registers
	ldd #62*256+46		; starting player position (62, 46) (center of screen minus 2)
	sta curposx		; set horizontal position
	stb curposy		; set vertical position
	ldd #STARTX		; set player position (X) entry point
	std mazeoffx
	ldd #STARTY		; set player position (Y) entry point
	std mazeoffy
	puls a,b,pc		; restore registers and return

; Game loop for player one
LD0EE	ldu #plr1state		; point to player one state
	ldd ,u++		; get saved screen coordinates
	sta curposx		; save horizontal screen position
	stb curposy		; save vertical screen position
	ldd ,u++		; fetch saved maze offset (X)
	std mazeoffx		; activate it
	ldd ,u			; fetch saved maze offset (Y)
	std mazeoffy		; activate it
	lda #2			; switch active player to player one
	sta curplayer
	lbsr LD9EA
	lbsr LD531
	clr portaloff		; mark all portals as active
	clr V18			; crown inactive
	clr V19			; crystal ball inactive
	clr VD1			; clear "walk through walls" flag
	ldu #plr1monsters	; point to player one's monster locations
	stu monsterptr		; save as monster location pointer
	ldu #scorep1		; point to player one's score
	stu scoreptr		; set as current score pointer
	ldu #plr1objlist	; point to player one's treasure list
	stu objlistptr		; set as current treasure list pointer
	leau plr1mess+1,pcr	; point to player one header message
	lbsr showmess		; show it
	clra			; stop maze scroll
	clrb
	std scrollstep
	clr VD5			; player dead flag
	lbsr drawmazeboth	; draw maze on both screens
	leau LCE2B,pcr		; opening tune
	lbsr LCD49		; 4 part music routine
	lbsr showscore		; update scores
LD13A	lbsr LD1BE		; common play loop
	tst VD5			; dead yet?
	beq LD13A		; keep going
	lbsr LDB82		; player death

LD144	ldu #plr1state		; point to player one's state data
	lda curposx		; fetch current horizontal screen position
	ldb curposy		; fetch current vertical screen position
	std ,u++		; save in state
	ldd mazeoffx		; fetch current maze offset (X)
	std ,u++		; save in state
	ldd mazeoffy		; fetch current maze offset (Y)
	std ,U			; save in state
	rts

; Game loop for player two
LD156	ldu #plr2state		; point to player two's state data
	ldd ,u++		; get saved screen position
	sta curposx		; save horizontal screen position
	stb curposy		; save vertical screen position
	ldd ,u++		; fetch saved maze offset (X)
	std mazeoffx		; activate it
	ldd ,u			; fetch saved maze offset (Y)
	std mazeoffy		; activate it
	lda #1			; set player two active
	sta curplayer
	lbsr LD9EA
	lbsr LD531
	clr portaloff		; mark all portals as active
	clr V18			; crown inactive
	clr V19			; crystal ball inactive
	clr VD1			; clear "walk through walls" flag
	ldu #plr2monsters	; point to player two's monster locations
	stu monsterptr		; set as monster location pointer
	ldu #scorep2		; point to player two's score
	stu scoreptr		; set as current score pointer
	ldu #plr2objlist	; point to the treasure list for player two
	stu objlistptr		; save as current treasure list
	leau plr2mess+1,pcr	; point to player two header message
	lbsr showmess		; set heading
	clra			; stop maze scroll
	clrb
	std scrollstep
	clr VD5			; player dead flag
	lbsr drawmazeboth	; draw maze on both screens
	leau LCE2B,pcr		; opening tune
	lbsr LCD49		; 4 part music routine
	lbsr showscore
LD1A2	lbsr LD1BE		; common play loop
	tst VD5			; dead yet?
	beq LD1A2		; keep going
	lbsr LDB82		; player death
LD1AC	ldu #plr2state		; point to player two state
	lda curposx		; get current horizontal screen position
	ldb curposy		; get current vertical screen position
	std ,u++		; save in state
	ldd mazeoffx		; get current maze offset (X)
	std ,u++		; save in state
	ldd mazeoffy		; get current maze offset (Y)
	std ,u			; save in state
	rts

* COMMON PLAY LOOP
LD1BE	lbsr swaprender		; switch screens
	lbsr LD254		; adjust explosion queue for screen scrolling
	lbsr LD254
	lbsr LD254
	lbsr checkcssel		; check for color set selection keys
	lbsr LDF96		; age crystal ball and crown
	lbsr clearrender	; clear workspace
	lbsr drawvert		; draw walls
	lbsr drawhoriz
	bsr LD1FF		; move player
	bsr LD1FF
	lbsr LD5C4		; render queued explosions
	lbsr LD829		; draw treasures
	lbsr LDA93		; create bat if needed
	lbsr LDB96		; monsters chase player
	lda VD5			; player dead flag
	pshs a
	lbsr LD9EF		; draw bat if necessary
	lda VD5
	ora ,s+			; dead from monster OR dead from bat?
	sta VD5			; player dead flag
	lbsr LD40B		; fire laser if joystick button pressed
	lbsr LDE58		; draw portals
	lbra LD375		; animate running man and return

* ADJUST PLAYER POSITION
* VC1 VC3 VC4 VC5
LD1FF	;jsr SNDOFF		; turn off sound
	jsr GETJOY		; read joysticks
	ldb curplayer		; get current player number
	asrb			; set to 0 for player 2, 2 for player 1
	lslb
	ldu #POTVAL		; point to joystick values
	leau b,u		; point to the correct axes for the active placer
	lda curposx		; get current horizontal position
	sta tcoord		; save it for later
	ldb ,u			; read vertical axis
	subb #$20		; subtract midpoint from horizontal position
	stb VC3			; save adjusted position
	sex			; sign extend
	lslb			;* shift left three bits which will get all remaining
	rola			;* significant bits in the axis reading to the right
	lslb			;* of a binary point
	rola
	lslb
	rola
	addd curposx		; add in the direction change (goes in partial steps)
	std curposx		; save new position
	lbsr checkcollision	; did we collide with something?
	beq LD22D		; brif not
	lda tcoord		; get saved position
	sta curposx		; restore it
LD22D	lda curposy		; read the current vertical position
	sta tcoord		; save it for later
	ldb 1,u			; read horizontal axis
	subb #$20		; subtract midpoint from vertical position
	stb VC4			; save reading
	sex			; sign extend
	lslb			; shift left three bits as for horizontal position
	rola
	lslb
	rola
	lslb
	rola
	addd curposy		; add to current position
	std curposy		; save new position
	lbsr checkcollision	; did we collide with something?
	beq LD24B		; brif not
	lda tcoord		; get back saved position
	sta curposy		; restore it
LD24B	stx VC1			; save screen pointer calculated in checkcollision
	ldd VC3
	beq LD253
	std VC5
LD253	rts

* Adjust all the coordinates in the explosion queue to compensate for scrolling
LD254	lda curposx
	ldx mazeoffx
	ldy #XQUEUE		; explosion sprite queue
	cmpa #$0f
	bhi LD27A
* SCROLL PLAYER LEFT
	leax -1,x
	inc 3,y			; adjust sprites in queue
	inc 7,y
	inc 11,y
	inc 15,y
	inc 19,y
	inc 23,y
	inc 27,y
	inc 31,y
	inc curposx
	bra LD296
LD27A	cmpa #$6d
	bcs LD296
* SCROLL PLAYER RIGHT
	leax 1,x
	dec 3,y			; adjust sprites in queue
	dec 7,y
	dec 11,y
	dec 15,y
	dec 19,y
	dec 23,y
	dec 27,y
	dec 31,y
	dec curposx
LD296	stx mazeoffx
	lda curposy
	ldx mazeoffy
	cmpa #$17
	bhi LD2BA
* SCROLL PLAYER DOWN
	leax -1,x
	inc 2,y			; adjust sprites in queue
	inc 6,y
	inc 10,y
	inc 14,y
	inc 18,y
	inc 22,y
	inc 26,y
	inc 30,y
	inc curposy
	bra LD2D6
LD2BA	cmpa #$4d
	bcs LD2D6
* SCROLL PLAYER UP
	leax 1,x
	dec 2,y			; adjust sprites in queue
	dec 6,y
	dec 10,y
	dec 14,y
	dec 18,y
	dec 22,y
	dec 26,y 
	dec 30,y
	dec curposy
LD2D6	stx mazeoffy
LD2D8	rts

checkcollision lda curposy	; get current vertical position
	ldb curposx		; get current horizontal position
	tfr d,x			; save it
	rolb			; calculate screen offset
	lsra
	rorb
	lsra
	rorb
	lsra
	rorb
	addd renderscr		; add to screen base address
	exg d,x			; save address to pointer and get back original values
	andb #3			; get offset into byte for the pixel
	lslb			; double the pixel offset
	leay LD31D,pcr		; point to table of shifted collision detection masks
	ldd b,y			; get collision detection mask for this pixel offset
	bita ,x			; do we collide at this byte on this row?
	bne LD30F		; brif so
	bitb 1,x		; do we collide at the next byte on this row?
	bne LD30F		; brif so
	bita $20,x		; do we collide at this byte on the next row?
	bne LD30F		; brif so
	bitb $21,x		; do we collide at the next byte on the next row?
	bne LD30F		; brif so
	bita $40,x		; do we collide at this byte two rows down?
	bne LD30F		; brif so
	bitb $41,x		; do we collide at the next byte two rows down?
LD30F	pshs cc			; save collision state
	lda VD1			; can walk through walls?
	inca
	bne LD31B		; no
	* walk through walls
	orcc #4			; set Z (no collision)
	leas 1,s		; clean stack
	rts

LD31B	puls pc,cc		; restore state of Z (set = no collision) and return

; collision comparison masks
LD31D	fdb %1111110000000000
	fdb %0011111100000000
	fdb %0000111111000000
	fdb %0000001111110000

; running man animation
LD325	fdb %0000111100000000 ; ..WW.... IDLE
	fdb %0010101010000000 ; .RRRR...
	fdb %0010101010000000 ; .RRRR...
	fdb %0000010100000000 ; ..BB....
	fdb %0000010100000000 ; ..BB....
	fdb %0000010100000000 ; ..BB....
	fdb %0000000000000000 ; ........
	fdb %0000000000000000 ; ........

LD335	fdb %0000111100000000 ; ..WW.... RUNNING RIGHT
	fdb %0010101010000000 ; .RRRR...
	fdb %1000101010000000 ; R.RRR...
	fdb %0000010100000000 ; ..BB....
	fdb %0000010001000000 ; ..B.B...
	fdb %0000010001000000 ; ..B.B...
	fdb %0000000000000000 ; ........
	fdb %0000000000000000 ; ........

	fdb %0000111100000000 ; ..WW....
	fdb %0010101010000000 ; .RRRR...
	fdb %0010101000100000 ; .RRR.R..
	fdb %0000010100000000 ; ..BB....
	fdb %0000010100000000 ; ..BB....
	fdb %0000010100000000 ; ..BB....
	fdb %0000000000000000 ; ........
	fdb %0000000000000000 ; ........

LD355	fdb %0000111100000000 ; ..WW.... RUNNING LEFT
	fdb %0010101010000000 ; .RRRR...
	fdb %0010101000100000 ; .RRR.R..
	fdb %0000010100000000 ; ..BB....
	fdb %0001000100000000 ; .B.B....
	fdb %0001000100000000 ; .B.B....
	fdb %0000000000000000 ; ........
	fdb %0000000000000000 ; ........

	fdb %0000111100000000 ; ..WW....
	fdb %0010101010000000 ; .RRRR...
	fdb %1000101010000000 ; R.RRR...
	fdb %0000010100000000 ; ..BB....
	fdb %0000010100000000 ; ..BB....
	fdb %0000010100000000 ; ..BB....
	fdb %0000000000000000 ; ........
	fdb %0000000000000000 ; ........

* Animate running man cursor
LD375	ldd VC5
	bge LD37A
	coma
LD37A	sta VC3
	tstb
	bge LD380
	comb
LD380	stb VC4
	cmpa VC4
	bge LD388
	lda VC4
LD388	tfr a,b
	lsla
	lsla
	adda V1A
	pshs cc
	sta V1A
	leau LD335,PCR		; RUNNING RIGHT player
	tst VC5
	bge LD39C
	leau LD355,PCR		; RUNNING LEFT player
LD39C	cmpb #8
	bgt LD3A3
	leau LD325,PCR		; IDLE player
LD3A3	puls cc
	bvc LD3AE
	tst V19
	bne LD3B8
	leau $10,u
LD3AE	lda curposy
	suba #2
	ldb curposx
	decb
	lbsr drawsprite
LD3B8	rts

LD3B9	lda VD7
	beq LD3C5

	suba #$10
	sta VD7
	clrb			; set sound output from DAC
	lbsr LDFD1

LD3C5	ldu renderscr		; get start address of render screen
	leau $c00,u		; point to end of screen
	clra			; clear registers for stack blast below
	clrb
	ldx #0
	ldy #0

* Clear screen
LD3DB	pshu d,x,y,dp		; 7 bytes x 18 = 126 bytes
	lda VD7
	sta PIA1.DA		; tikkatikkatikka sound
	clra
	pshu d,x,y,dp
	pshu d,x,y,dp
	pshu d,x,y,dp
	pshu d,x,y,dp
	pshu d,x,y,dp
	pshu d,x,y,dp
	pshu d,x,y,dp
	pshu d,x,y,dp
	pshu d,x,y,dp
	pshu d,x,y,dp
	clr PIA1.DA		; tikkatikkatikka sound
	pshu d,x,y,dp
	pshu d,x,y,dp
	pshu d,x,y,dp
	pshu d,x,y,dp
	pshu d,x,y,dp
	pshu d,x,y,dp
	pshu d,x,y,dp
	pshu d			; 2 bytes x 1 = 2 bytes
	cmpu endclear		; have we reached the start of the screen?
	bgt LD3DB		; brif not, do another 128 bytes
 ENDC
	rts

* Fire laser if joystick button pressed
LD40B	ldb PIA0.DA		; read row data from keyboard (gets joystick buttons)
	andb curplayer		; mask off button for the correct player
	lbne LD495		; brif button not pressed
	tst VD1
	lbne LD4C7		; advance "walk through walls" timer
	inc VD1			; start timer
	clr VCF
	clr VD0
	lda #$aa		; red
	sta color
	ldb VC5
	lslb
	sex
	lslb
	rola
	lslb
	rola
	std VCB
	ldb VC6
	lslb
	sex
	lslb
	rola
	lslb
	rola
	std VCD
LD438	clra
	pshs a
	ldd VCB
	lbsr LD4CF
	bne LD444
	inc ,s
LD444	std VCB
	ldd VCD
	lbsr LD4CF
	bne LD44F
	inc ,s
LD44F	std VCD
	lda ,s+
	beq LD438
	ldd VCB
	asra
	rorb
	std VCB
	ldd VCD
	asra
	rorb
	std VCD
	ldb curposx
	incb
	stb VC7
	ldb curposy
	decb
	stb VC9
	clrb
	stb VC8
	stb VCA
	lda #$f0	; tikkatikkatikka sound
	sta VD7
LD474	lda VC9
	ldb VC7
	cmpd VCF
	beq LD487

	std VCF
	cmpa #8
	bls LD498
	bsr pset	; draw pixel in laser shot
	bne LD498

LD487	ldd VCB
	addd VC7
	std VC7
	ldd VCD
	addd VC9
	std VC9
	bra LD474
LD495	clr VD1		; reset "walk through walls" timer
LD497	rts

LD498	lda VC9
	ldb VC7
	tst V18	   ; is crown active?
	beq LD4C4
	pshs b,a
	adda #3
	lbsr LD54E ; queue an explosion
	ldd ,s
	suba #3
	subb #2
	lbsr LD54E ; queue an explosion
	ldd ,s
	suba #3
	addb #2
	lbsr LD54E ; queue an explosion
	ldd ,s
	addb #3
	lbsr LD54E ; queue an explosion
	ldd ,s++
	subb #3
LD4C4	lbra LD54E ; queue an explosion

* Advance "walk through walls" timer
LD4C7	lda VD1
	inca		; count up
	beq LD497	; but not if it's $ff
	sta VD1
	rts

* What is this doing?  Has something to do with laser shot line drawing
LD4CF	tsta		; if positive,
	blt LD4DC
	cmpd #$100
	bge LD4E7	;	and less than $100,
	lslb
	rola		;       	D = 2 * D
	bra LD4E4	; else
LD4DC	cmpd #$ff00	
	ble LD4E7	; 	if greater than $FF00,
	lslb
	rola		;		D = 2 * D
LD4E4	andcc #$fd	; clear carry
	rts
LD4E7	orcc #4		; set carry
	rts

LD4EA	fcb $c0,$30,$0c,$03	; pixel masks within byte

* A = y coord
* B = x coord
* color = color black, blue, red or white (%00000000, %01010101, %10101010, %11111111)
* collision = 0 (false), 1 (true)
pset	cmpa #$5f	; is the Y coordinate off bottom of screen?
	bhi LD52E	; brif so
	cmpb #$7f	; is the X coordinate off the right of the screen?
	bhi LD52E	; brif so
	cmpa #8		; is the Y coordinate within the text row at the top?
	bcs LD52E	; brif so
	pshs b		; save X coordinate
	lslb		; compensate for the 3 right shifts below
	lsra		; * calcuate offset from start of screen; this needs to
	rorb		; * multiply the row number by 32 and add the column
	lsra		; * number divided by 4. Three 16 bit right shifts of
	rorb		; * an 8 bit value in A gives the 16 bit result in D
	lsra		; * Also, the shifts will divide the value in B by 8
	rorb		; * which gives the correct offset into the screen
	addd renderscr	; add in screen start address
	tfr d,x		; save byte address in a pointer register
	puls a		; get back X coordinate
	anda #3		; find offset in byte
	leay LD4EA,pcr	; point to pixel masks
	ldb a,y		; get pixel mask
	tfr b,a		; put it also in A - we need it twice
	coma		; flip mask so we clear bits in the screen byte
	anda ,x		; clear pixel in data byte
	bitb ,x		; was the pixel set?
	bne LD524	; brif so - flag collision
	andb color	; get correct pixel data in the all color byte
	sta ,x		; save cleared pixel data
	orb ,x		; merge it with new pixel data
	stb ,x		; set screen data
	orcc #4		; set Z (no collision)
	rts

LD524	inc collision	; flag collision
	andb color	; get correct pixel data in all color byte
	sta ,x		; save cleared pixel data
	orb ,x		; merge it with new pixel data
	stb ,x		; set screen data
LD52E	andcc #$fb	; flag collision (Z clear)
	rts

* Clear explosion sprite queue
LD531	clra
	clrb
	std XQUEUE
	std XQUEUE+4
	std XQUEUE+8
	std XQUEUE+12
	std XQUEUE+16
	std XQUEUE+20
	std XQUEUE+24
	std XQUEUE+28
	clr VD7
	rts

; Queue an explosion animation

LD54E	tfr d,y
	cmpa #$5f
	bhi LD57C
	cmpa #8
	bls LD57C
	cmpb #$7f
	bhi LD57C
	ldu #XQUEUE	; explosion table
	ldx #8
LD562	ldd ,u		; find blank slot in 8 slots of 4 bytes each
	beq LD56E
	leau 4,u	; found a blank slot
	leax -1,x
	bne LD562
	bra LD57C
LD56E	leax explosion,pcr	; queue explosion, starting with first sprite
	stx ,u++
	tfr y,d
	suba #4
	subb #4
	std ,u
LD57C	rts

; Render a sprite at coordinates (B,A); exit with Z set if a collision occurred
; with pixels that were already set. collision will also be set if a collision occurred.
; The sprite is pointed to in U and has 2 bits per pixel. It is 8 pixels by 8 pixels
; which means 16 bytes of data.
drawsprite pshs b,a		; save render coordinates
	pshs b			; save X coordinate for later
	clr collision		; reset collision flag
	lda #8			; we're rendering 8 pixels high
	pshs a			; save counter
	leas -2,s		; allocate local storage
LD589	ldd ,u++		; get pixel data for rendering
	std ,s			; save pixel data
LD58D	ldd ,s			; fetch current pixel data
	beq LD5AF		; brif no more pixels set
	clra			; clear out extra bits in A
	lsl 1,s			; shift pixel bits into A (2 bits)
	rol ,s
	rola
	lsl 1,s
	rol ,s
	rola
	beq LD5AB		; brif pixel is not set
	leay colors,pcr		; point to all pixel color masks
	lda a,y			; get color mask for this color
	sta color		; save color mask for rendering
	ldd 4,s			; get render coordinates
	lbsr pset		; render pixel on screen
LD5AB	inc 5,s			; bump X render coordinate
	bra LD58D		; move on to next pixel
LD5AF	dec 2,s			; have we rendered all rows?
	beq LD5BB		; brif so
	inc 4,s			; bump render Y coordinate
	lda 3,s			; reset render X coordinate
	sta 5,s
	bra LD589		; go render another pixel row
LD5BB	leas 6,s		; deallocate local storage
	tst collision		; set Z if no collision
	rts

colors	fcb $00,$55,$aa,$ff	; all pixel color masks for colors 0, 1, 2, 3

; render all queued explosions

LD5C4	ldu #XQUEUE ; explosion table
	lda #8
	pshs a
	pshs u
LD5CD	ldu ,s	; u points to explosion table
	ldd ,s  ; address of explosion table
	addd #4 ; advance to next entry
	std ,s
	ldx ,u ; address of sprite if any
	beq LD5F4 ; skip empty queue item
	ldd ,u 
	addd #1 ;advance to next sprite
	std ,u
	ldx ,u
	ldb ,x
	beq LD5F0 ; this explosion is done, clear this entry
	abx
	ldd 2,u ; get coordinates
	tfr x,u ; get sprite pointer
	bsr drawsprite ; draw explosion sprite
	bra LD5F4
LD5F0	clra	; clear queue entry
	clrb
	std ,u
LD5F4	dec 2,s
	bne LD5CD ; next queue entry
	leas 3,s
	rts

clearscores	clra		; set up to clear values
	clrb
	std scorep1		; clear player one's score
	sta scorep1+2		; clear the fixed lsb of player one's score
	std scorep2		; clear player two's score
	sta scorep2+2		; clear the fixed lsb of player two's score
	rts

addscore	pshs u		; save register
	lbsr LDAEF		; do the "new score bleep" thing
	ldu scoreptr		; fetch current score pointer
	adda 1,u		; add amount to score (lsb)
	daa			; decimal adjust (using BCD)
	sta 1,u			; save new lsb
	lda ,u			; propagate carry
	adca #0
	daa			; decimal adjust (BCD)
	sta ,u			; save new msb
	puls pc,u		; restore registers and return
showscore	ldd renderscr	; get pointer to current render screen
	pshs b,a		; save it for later
	ldd #SCREEN1		; set to screen one
	std renderscr
	lbsr clrheader		; clear the header
	lda scorep1		; get high 2 digits of player one's score
	ldb #3			; starting X coordinate for score
	clr VD6
	bsr LD65D		; display digits
	lda scorep1+1		; get middle two digits of player one's score
	bsr LD65D		; display digits
	lda #$ff
	sta VD6
	lda scorep1+2		; get low 2 digits of player one's score
	bsr LD65D		; display digits
	lda numplayers		; get number of players
	cmpa #1			; is it more than one?
	beq LD655		; brif not - don't show player two's score
	lda scorep2		; get high 2 digits of player two's score
	ldb #$53		; starting X coordinate for score
	clr VD6
	bsr LD65D		; display digits
	lda scorep2+1		; get middle two digits of player two's score
	bsr LD65D		; display digits
	lda #$ff
	sta VD6
	lda scorep2+2		; get low two digits of player two's score
	bsr LD65D		; display digits
LD655	lbsr dupheader		; duplicate the header to screen two
	puls b,a		; get back current render screen
	std renderscr		; restore it to active
	rts

LD65D	pshs b,a		; save registers (coordinate and score digits)
	anda #$f0		; mask out first digit
	sta V4F			; save digit value
	ora VD6			; merge with the "hide zeroes" flag
	beq LD67A		; if it turned into 0, we have a leading zero; skip it
	lda V4F			; get back digit value
	leay digits+$20,pcr	; point to digit glyphs
	suba #$20		; adjust digit number for the the pointer into the middle of the table
	leau a,y		; point to correct digit glyph
	lda #1			; set Y coordinate
	lbsr draw8x5		; go render glyph
	lda #$ff		; set the "hide zero" flag to not hide zeroes - we have a nonzero digit
	sta VD6
LD67A	ldd ,s			; get back X coordinate and digit
	addb #5			; adjust X coordinate to next digit
	stb 1,s			; save new X coordinate
	anda #$0f		; mask off to low digit
	sta V4F			; save digit value
	ora VD6			; is it zero and we're hiding them?
	beq LD69F		; brif so
	lda V4F			; get back digit
	lsla			; shift digit to high position (16 bytes per glyph)
	lsla
	lsla
	lsla
	leay digits+$20,pcr	; point to digit data
	suba #$20		; adjust for pointer to middle
	leau a,y		; point to correct glyph
	lda #1			; set Y coordinate
	lbsr draw8x5		; draw glyph
	lda #$ff		; set the "hide zeroes" flag to show zeroes
	sta VD6
LD69F	puls b,a		; get back X coordinate and digit
	addb #5			; move to next digit position (X coordinate)
	rts

clearrender	ldd renderscr	; get pointer to start of current render screen
	addd #$100		; pointer to below the screen header
	std endclear		; save top of space to clear
	lbra LD3B9		; go clear the screen

buildobjlist	clr VDA		; clear treasure count
	leax objcross+1,pcr	; point to jade cross treasure data
	bsr LD6DA		; add to treasure list
	leax objring+1,pcr	; point to diamond ring treasure data
	bsr LD6DA		; add to treasure list
	leax objcup+1,pcr	; point to golden cup treasure data
	bsr LD6DA		; add to treasure list
	leax objgoblet+1,pcr	; point to crystal goblet treasure data
	bsr LD6DA		; add to treasure list
	leax objball+1,pcr	; point to crystal ball treasure data
	bsr LD6DA		; add to treasure list
	leax objpitcher+1,pcr	; point to silver pitcher treasure data
	bsr LD6DA		; add to treasure list
	leax objcrown+1,pcr	; point to golden crown treasure data
	;bra LD6DA		; add to treasure list (this instruction is redundant)
LD6DA	leay $10,x		; point to coordinate list (move past sprite data)
LD6DD	ldd ,y++		; get coordinates
	beq LD6E9		; brif end of list
	std ,u++		; save coordinates of treasure
	stx ,u++		; save pointer to treasure sprite
	inc VDA			; bump treasure count
	bra LD6DD		; go see if there's another of this treasure type
LD6E9	rts

 IFDEF MCUSTOM
 include map/treasures.asm
 ELSE
 include treasures.asm
 ENDC

* Draw treasures and handle treasure collisions
LD829	lda curposy
	ldb curposx
	* Set up hitbox for player collision with treasures
	sta hitx2
	stb hity2
	suba #4
	subb #4
	sta hitx1
	stb hity1
	clr VDC		; number of treasures remaining
	ldu objlistptr
	lda VDA		; number of treasures
	pshs a
LD841	ldb ,u		; treasure x
	beq LD8BF	; treasure already collected
	inc VDC		; number of remaining treasures
	clra
	lslb
	rola
	lslb
	rola
	subd mazeoffx
	cmpd #$fffa
	blt LD8BF	; offscreen
	cmpd #$7f
	bgt LD8BF	; offscreen
	stb VD9
	ldb 1,u		; treasure y
	clra
	lslb
	rola
	lslb
	rola
	subd mazeoffy
	cmpd #2
	blt LD8BF	; offscreen
	cmpd #$5f
	bgt LD8BF	; offscreen
	stu VBF
	ldu 2,u
	stb VD8
	ldd VD8
	cmpa hitx1
	blt LD8A9
	cmpa hitx2
	bgt LD8A9
	cmpb hity1
	blt LD8A9
	cmpb hity2
	bgt LD8A9

* Collision with player: treasure collected!
	clra
	clrb
	ldu VBF
	std ,u
	ldu 2,u
	lda -1,u
	lbsr LDF80
	lbsr addscore
	ldd VF9			; does the bat exist?
	beq LD8A2
	lda -1,u
	lbsr addscore		; double points if the bat is chasing you
LD8A2	lbsr showscore
	ldu VBF
	bra LD8BF

* Collision with explosion: treasure destroyed by laser
LD8A9	lbsr drawsprite		; draw treasure
	beq LD8BD
	clra			; delete treasure
	clrb
	std [VBF]
	ldd VD8
	adda #4
	addb #4
	lbsr LD54E		; queue an explosion
LD8BD	ldu VBF

LD8BF	leau 4,u
	dec ,s			; next treasure
	lbne LD841
	puls a

	tst VDC			; any treasures left?
	bne LD8E4
* No treasures remaining: victory!
	ldu objlistptr
	lbsr buildobjlist
	ldu monsterptr
	lbsr LDCE6		; initialize monster home positions
	lbsr setstartpos	; starting position for start of turn
	lbsr LDFD7
	leau LCE2B,pcr		; opening tune
	lbsr LCD49
LD8E4	rts

swaprender
	lbsr WaitVSYNC		; wait for VSYNC
	ldd renderscr		; get start address of current render screen
	cmpd #SCREEN1		; screen number 1?
	bne LD8FD		; brif not
	sta SAM+9		; set SAM to display screen #1
	sta SAM+12
	ldd #SCREEN2		; set render address to screen #2
	std renderscr
	rts
LD8FD	sta SAM+8		; set SAM to display screen #2
	sta SAM+13
	ldd #SCREEN1		; set render address to screen #1
	std renderscr
LD90B	rts

; Render a string at the top of both graphics screens. The string will be centered.
; If texttty is zero, a delay will be introduced between characters and a bleep will sound
showmess ldd renderscr	; get current render screen
	pshs b,a		; save it
	ldd #SCREEN1		; point to start of screen #1
	std renderscr		; set it as the render screen
	lbsr clrheader		; clear out top 8 pixel rows of screen #1
	lda -1,u		; get length of string
	ldb #6			; six pixels per character cell
	mul			; now D is the width of the rendered string
	decb			; negate result (less 1)
	negb
	addb #$7f		; now add the negative to the screen with
	lsrb			; divide by 2 (now B is X coord of render location)
	lda #1			; render on line 1
	pshs b,a		; save render coordinates
	pshs u			; save string pointer
	lda -1,u		; get string length
	pshs a			; save in counter
LD92C	dec ,s			; are we done yet?
	blt LD97E		; brif so
	lda PIA0.DA		; read keyboard row data
	anda #3			; mask off everything but joystick buttons
	eora #3			; set to 0 if buttons NOT pressed
	beq LD93D		; brif no buttons pressed
	tst V03			; is the joystick button check enabled?
	beq LD98A		; brif so - bail on rendering
LD93D	lda texttty		; do we want a delay between rendering characters?
	bne LD944		; brif not
LD941	inca			; count up the timer
	bne LD941		; brif 256 counts done
LD944	ldb [1,s]		; get character to render
	leay fontidx,pcr	; point to font character index
	leau fontdata,pcr	; point to font data
	cmpb #$2e		; is it period character?
	bne LD959		; brif not
	leau LD6ED,pcr		; point to "period" glyph
	bra LD961		; go render character
LD959	cmpb ,y+		; are we at the right index point in the font?
	beq LD961		; brif so
	leau 4,u		; move to next font data position
	bra LD959		; go check next index location
LD961	ldd 3,s			; get render coordinates
	lbsr drawglyph		; render character to screen
	lbsr dupheader		; copy rendered text to second screen
	lbsr checkcssel		; check color set selection keys
	lbsr dobleep		; do the bleep if required
	ldd 1,s			; get string pointer
	addd #1			; move to next character
	std 1,s			; save new string pointer
	lda 4,s			; get rendering X coordinate
	adda #6			; move to next character cell
	sta 4,s			; save new X coordinate
	bra LD92C		; go render another character
LD97E	leas 5,s		; clean up temporaries
	puls b,a		; get back render screen pointer
	std renderscr		; restore render pointer
	lbsr dupheader		; copy top 8 rows to second screen
	andcc #$fb		; clear Z (no joystick button pressed)
	rts

LD98A	leas 5,s		; clear temporaries
	puls b,a		; restore render screen pointer
	std renderscr
	lbsr dupheader		; copy top 8 rows to the second screen
	orcc #4			; set Z for joystick button pressed
	rts

scrolllong	lda #120	; do 120 iterations
scrollmaze	pshs a		; save the iteration count
LD99A	dec ,s			; decrement iteration count
	beq LD9E6		; brif we're done with all the iterations
	lbsr checkcssel		; check color set selection keys
	lbsr swaprender		; swap screens
	lbsr clearrender	; get a clear canvas
	lbsr drawvert
	lbsr drawhoriz

* ATTRACT MODE SCROLL
	ldd mazeoffx		; get X offset for screen
	addd scrollstep		; add in step
	std mazeoffx		; save new screen offset
	ldd mazeoffy		; get Y offset for screen
	addd scrollstep		; add in step
	std mazeoffy		; save new screen offset

	ldd mazeoffx
	cmpd #MAXX-128		; did we pass the right edge?
	bhs reverse		; brif not / continue scrolling
	cmpd #MINX		; did we pass the left edge?
	blo reverse		; brif not / continue scrolling

	ldd mazeoffy
	cmpd #MAXY-96		; did we pass the bottom edge?
	bhs reverse		; brif not / continue scrolling
	cmpd #MINY-4		; did we pass the top edge?
	blo reverse		; brif not / continue scrolling
	bra LD9D7

reverse
	pshs d			; save registers
	clra			; set up to negate the scroll direction
	clrb
	subd scrollstep		; subtract scroll step from 0 (negate it)
	std scrollstep		; save new step/direction
	puls d			; restore registers

LD9D7	ldb PIA0.DA		; read keyboard rows
	andb #3			; keep joystick buttons
	eorb #3			; set so nonzero means pressed
	beq LD99A		; brif no buttons pressed - do another iteration
	puls a			; clean up iteration count
	comb			; set nonzero (B has at least 1 bit set and at least 1 not) - button pressed
	rts

LD9E6	puls a			; clean up iteration count
	clra			; set zero flag - no buttons pressed
	rts

* deactivate bat
LD9EA	clra
	clrb
	std VF9
	rts

LD9EF	ldd VF9		; bat active?
	lbeq LDA92	; no
	tst V19		; crystal ball active?
	beq LDA02	; no

	ldd V50		; crystal ball is active, bat wanders off
	addd #1
	std V50
	bra LDA08

LDA02	lbsr LDB37
	lbsr LDB37
LDA08	leau LDACF,pcr	; first bat sprite
	ldb VFA		; flap the bat's wings
	eorb #1
	stb VFA
	andb #1
	beq LDA19
	leau $10,u	; second bat sprite
LDA19	ldd V52
	subd mazeoffy
	cmpd #2
	blt LDA92
	cmpd #$5f
	bgt LDA92
	stb VBF
	ldd V50
	subd mazeoffx
	cmpd #$fffa
	blt LDA92
	cmpd #$7f
	bgt LDA92
	lda VBF
	clr collision
	lbsr drawsprite
	tst collision
	beq LDA92
	ldd V50
	subd mazeoffx
	subd #4
	stb hity1
	addd #8
	stb hity2
	ldd V52
	subd mazeoffy
	subd #4
	stb hitx1
	addd #8
	stb hitx2

	* hit by any explosions?
	lda #8
	pshs a
	ldu #XQUEUE-4
LDA69	dec ,s
	blt LDA90
	leau 4,u
	ldd ,u
	beq LDA69
	ldd 2,u
	cmpa hitx1
	bcs LDA69
	cmpa hitx2
	bhi LDA69
	cmpb hity1
	bcs LDA69
	cmpb hity2
	bhi LDA69
	lbsr LD9EA
	lda #$10
	lbsr addscore	; 100 points for bat
	lbsr showscore
LDA90	puls a
LDA92	rts

* Decide whether or not to create the bat
LDA93	pshs u,b,a
	ldd VF9		; bat already exists?
	bne LDACD	; no
	inc V5C		; bat creation timer
	bne LDACD	; not yet
	tst V19		; crystal ball active?
	bne LDACD	; yes

* Create the bat
	leau LDACF,pcr	; bat sprite
	stu VF9		; activate bat
	* Place the bat just offscreen at a random corner
	ldd mazeoffx
	subd #$0a
	std V50		; bat x position
	ldd mazeoffy
	subd #$0a
	std V52		; bat y position
	lbsr random
	bmi LDAC1
	ldd V50		; bat x position
	addd #$93
	std V50		; bat x position
LDAC1	lbsr random
	bmi LDACD
	ldd V52		; bat y position
	addd #$93
	std V52		; bat y position

LDACD	puls pc,u,b,a

LDACF	fdb $0c30 ; ..W..W.. Here's the bat!
	fdb $3c3c ; .WW..WW.
	fdb $3ffc ; .WWWWWW.
	fdb $f3cf ; WW.WW.WW
	fdb $c3c3 ; W..WW..W
	fdb $c003 ; W......W
	fdb $0000 ; ........
	fdb $0000 ; ........

	fdb $c003 ; W......W
	fdb $c003 ; W......W
	fdb $f3cf ; WW.WW.WW
	fdb $3ffc ; .WWWWWW.
	fdb $0c30 ; ..W..W..
	fdb $0000 ; ........
	fdb $0000 ; ........
	fdb $0000 ; ........

* scoring bleep
LDAEF	pshs u,b,a
	clrb		; enable sound output from DAC
	lbsr LDFD1
	clrb
LDAF6	tfr b,a
	comb 
	asrb
	asrb
	stb PIA1.DA
	tfr a,b
LDB00	inca
	bne LDB00
	tfr b,a
	clr PIA1.DA
LDB08	inca 
	bne LDB08
	addb #8
	bne LDAF6
	puls pc,u,b,a

; Outputs a sort of bleep (used between characters when the "tty" effect is operating
dobleep	pshs u,b,a		; save registers
	tst texttty		; is the tty effect enabled?
	bne LDB35		; brif not
	clrb			; enable sound output from DAC
	lbsr LDFD1
	clrb			; initialize output level to 0
LDB1C	tfr b,a			; save output level
	comb			; invert level
	asrb			; blank out lower two bits
	asrb
	stb PIA1.DA		; set DAC
	tfr a,b			; get back output level
LDB26	inca			; bump output level (saved) (using as a timer)
	bne LDB26		; brif not time expired
	tfr b,a			; reset the timer value
	clr PIA1.DA		; reset output level to 0
LDB2E	deca			; timeout (count down this time)
	bne LDB2E		; brif not done yet
	addb #4			; bump the output level by 1 step
	bne LDB1C		; brif we haven't wrapped
LDB35	puls pc,u,b,a		; restore registers and return

* Bat chases player, kills him if collision
LDB37	lda #$ff		; assume player is dead
	sta VD5			; player dead flag
	ldb curposx
	clra
	addd mazeoffx
	subd #4
	cmpd V50		; bat x position
	beq LDB5E
	blt LDB55
	clr VD5			; player not dead
	ldd V50			; bat x position
	addd #1
	std V50			; bat x position
	bra LDB5E
LDB55	ldd V50
	clr VD5			; player not dead
	subd #1
	std V50			; bat x position
LDB5E	ldb curposy
	clra
	addd mazeoffy
	subd #1
	cmpd V52		; bat y position
	beq LDB81
	blt LDB78
	ldd V52		; bat y position
	addd #1
	std V52		; bat y position
	clr VD5			; player not dead
	bra LDB81
LDB78	ldd V52		; bat y position
	subd #1
	clr VD5			; player dead flag
	std V52
LDB81	rts

* PLAYER DIED
LDB82	lda #10		; bleep 10x
	pshs a
LDB86	lbsr LDAEF
	dec ,s
	bne LDB86
	leau LCE90,pcr		; funeral march
	lbsr LCD49		; 4 part music routine
	puls pc,a

LDB96	ldu monsterptr
	leau -4,u
	pshs u
	ldu #monsters
	leau -9,u

	* monster chases player
	clra
	ldb curposx
	addd mazeoffx
	std V5D		; target coord x is player x
	clra
	ldb curposy
	addd mazeoffy
	std V5F		; target coord y is player y

LDBAF	leau 9,u
	ldd ,s		; advance pointer to next monster location
	addd #4
	std ,s
	tst VD5		; player dead flag
	beq LDBC2

	lbsr setstartpos ; move player to entry point if dead
	lbra LDCCD

LDBC2	ldd ,u
	lbeq LDCCD
	ldx ,s
	ldd ,x		; monster x
	beq LDBAF	; skip dead monster
	std V68		; object coord x
	ldd 2,x		; monster y
	std V8D		; object coord y

* Is the player inside the monster's aggro area?
	clr aggro
	ldd V5D		; target coord x
	ldx ,u
	ldy 2,u
	leay -3,y
	lbsr LCF3C	; player inside monster's aggro area?
	bne LDC02

	ldd V5F		; target coord y
	ldx 4,u
	ldy 6,u
	leay -3,y
	lbsr LCF3C	; player inside monster's aggro area?
	bne LDC02

	tst V19		; is crystal ball active?
	bne LDC02	; if so, monster can't see player

* Player is inside aggro area: chase player
	inc aggro
	clr V5C		; reset bat creation timer
	lbsr LDD08	; chase player
	tst 8,u		; spider vs fireball
	beq LDC28
	lbsr LDD08	; fireball chases 2x faster
	bra LDC28

* Player is outside monster aggro area: return to home
LDC02	ldd 2,u		; x2 - x1
	subd ,u
	lsra		; divide by 4
	rorb
	lsra
	rorb
	addd ,u		; + x1
	ldx V68		; object coord x
	exg d,x
	lbsr LDCCF
	std V68		; object coord x
	ldd 6,u		; y2 - y1
	subd 4,u
	lsra		; divide by 4
	rorb
	lsra
	rorb
	addd 4,u	; + y1
	ldx V8D		; object coord y
	exg d,x
	lbsr LDCCF
	std V8D		; object coord y

	* What's the monster's position in global coordinates?
LDC28	ldx ,s
	ldd V68		; object coord x
	std ,x
	ldd V8D		; object coord y
	std 2,x
	ldd V68		; object coord x
	subd mazeoffx
	std V68		; object coord x
	ldd V8D		; object coord y
	subd mazeoffy
	std V8D		; object coord y

	* Is this monster onscreen?
	ldd V68		; object coord x
	cmpd #$fff8
	lblt LDCCA
	cmpd #$7f
	lbgt LDCCA
	ldd V8D		; object coord y
	cmpd #$fff8
	blt LDCCA
	cmpd #$5f
	bgt LDCCA

	* Draw monster
	pshs u
	ldb 8,u		; is this a ghost?
	cmpb #$40
	bne no@
	tst aggro	; and aggro?
	beq nodraw@	; if not, don't draw it
no@
	leau LDD84,pcr	; drawing spider sprites
	leau b,u	; or fireball, ghost or skull sprites
	lda V68+1	; low order object coord x
	eora V8D+1	; low order object coord y
	anda #2
	beq LDC71
	leau 16,u	; use second frame of sprite animation
LDC71	lda V8D+1	; low order object coord y
	ldb V68+1	; low order object coord x
	lbsr drawsprite ; draw monster sprite
nodraw@
	puls u

	* set up hitbox for collision test
	ldd V68		; object coord x
	subd #4
	stb hity1
	addd #8
	stb hity2
	ldd V8D		; object coord y
	subd #4
	stb hitx1
	addd #8
	stb hitx2
	lda #8	; 8 slots in explosion queue
	pshs u
	pshs a

	* Any collision between explosions and this monster?
	ldu #XQUEUE-4	; explosion queue
LDC9B	dec ,s		; done with queue?
	blt LDCC6
	leau 4,u	; move to next queue slot
	ldd ,u
	beq LDC9B	; slot is empty
	ldd 2,u		; coordinates of this explosion
	cmpa hitx1	; hitbox min x
	blo LDC9B	; no hit
	cmpa hitx2	; hitbox max x
	bhi LDC9B	; no hit
	cmpb hity1	; hitbox min y
	blo LDC9B	; no hit
	cmpb hity2	; hitbox max y
	bhi LDC9B	; no hit
	clr VD5		; player dead flag
	clra		; kill monster
	clrb
	std [3,s]
	lda #$10	; 1000 points for spider or fireball
	lbsr addscore
	lbsr showscore

LDCC6	puls a
	puls u
LDCCA	lbra LDBAF
LDCCD	puls pc,u

* Monster chase logic
* X target coord
* D object coord
LDCCF	pshs x
	cmpd ,s
	bhi LDCDA
	blo LDCDF
	bra LDCE4	; no movement
LDCDA	subd #1		; move left/up
	bra LDCE2
LDCDF	addd #1		; move right/down
LDCE2	clr VD5		; player not dead
LDCE4	puls pc,x

* initialize monster home positions
LDCE6	ldx #monsters		; point to monster table
LDCE9	ldd ,x			; get first coordinate base
	beq LDD07		; brif end of table
	ldd 2,x			; set first coordinate modification
	subd ,x			; get difference from base
	lsra			; divide by 4
	rorb
	lsra
	rorb
	addd ,x			; add to base coordinate
	std ,u++		; save as real coordinate
	ldd 6,x			; get second coordinate modification
	subd 4,x		; get difference from base
	lsra			; divide by 2
	rorb
	addd 4,x		; add to base coordinate
	std ,u++		; save real coordinate
	leax 9,x		; move to next table entry
	bra LDCE9		; go process another monster
LDD07	rts

* Chase logic
LDD08	lda #$ff
	sta VD5			; mark player dead, if movement necessary it'll get cleared
	ldd V68			; object coord x
	ldx V5D			; target coord X
	leax -3,x		; offset target
	lbsr LDCCF		; chase target coord
	std V68			; update object coord x
	ldd V8D			; object coord y
	ldx V5F			; target coord y
	leax -3,x		; offset target
	lbsr LDCCF		; chase target coord
	std V8D			; update object coord y
	rts

 IFDEF MCUSTOM
 include map/monsters.asm
 ELSE
 include monsters.asm
 ENDC

; Render an 8x5 bitmap at coordinates (B,A).
draw8x5	pshs b,a		; save the render coordinates
	pshs b			; save X coordinate for subsequent rows
	lda #5			; we're going to render 5 rows
	pshs a			; save counter
	leas -2,s		; make a temporary hole
LDDCE	ldd ,u++		; fetch sprite data
	std ,s			; temp save it
LDDD2	ldd ,s			; are there any non-background pixels?
	beq LDDEC		; brif not - we can bail
	clra			; clear extraneous bits
	lsl 1,s			; shift pixel data into A (2 bits)
	rol ,s
	rola
	lsl 1,s
	rol ,s
	rola
	tsta			; is the pixel set?
	beq LDDE8		; brif not
	ldd 4,s			; get render coordinates
	bsr LDDFB		; render pixel
LDDE8	inc 5,s			; bump X coordinate for rendering
	bra LDDD2		; go do another pixel
LDDEC	dec 2,s			; have we done all the rows?
	beq LDDF8		; brif so
	inc 4,s			; bump Y coordinate for rendering
	lda 3,s			; reset X coordinate for next row
	sta 5,s
	bra LDDCE		; go render another row
LDDF8	leas 6,s		; clear temporaries
	rts

LDDFB	pshs b			; save X coordinate
	lslb			; compensate for having 3 right shifts below 
	lsra			;* shift right 3 is equivalent to multiply by 32
	rorb			;* with result in D; will also keep the X coordinate
	lsra			;* but divided by 4 which gives the byte offset
	rorb			;* on the screen row
	lsra			;*
	rorb			;*
	addd renderscr		; add in screen base address
	tfr d,x			; put address in pointer
	puls a			; get back original 
	anda #3			; get pixel offset in byte
	leay LD4EA,pcr		; point to pixel masks
	ldb a,y			; get pixel mask for pixel
	comb			; invert it so we can clear the pixels
	andb ,x			; read screen data and clear the pixel to color #0
	stb ,x			; set new pixel data on screen
	rts

drawglyph	pshs b,a	; save render coordinates
	pshs b			; save original X coordinate for later
	lda #5			; render 5 bits
	pshs a			; save counter
	ldd 2,u			; save font data bytes on stack (4 of them)
	pshs b,a
	ldd ,u
	pshs b,a
LDE28	lda ,s			; check if remaining font data is all 0s
	ora 1,s
	ora 2,s
	ora 3,s
	beq LDE55		; brif all 0s - no point continuing more (and it's how we exit anyway)
	clra			; clear out temporary (could skip this, the rola, and tsta below))
	lsl 3,s			; fetch bit from font data
	rol 2,s
	rol 1,s
	rol ,s
	rola			; shift pixel bit into A
	tsta			; do we have a bit?
	beq LDE43		; brif not (could be bcc and lose the rola/tsta above)
	ldd 6,s			; get render coordinates
	bsr LDDFB		; render pixel
LDE43	inc 7,s			; bump render X coordinate
	dec 4,s			; done all 5 bits?
	bne LDE28		; brif not
	inc 6,s			; bump Y coordinate
	lda 5,s			; restore original X coordinate
	sta 7,s
	lda #5			; reset render counter
	sta 4,s
	bra LDE28		; go render another row of pixels
LDE55	leas 8,s		; clean up temporaries
	rts

LDE58	lda curposy		; get current vertical coordinate on screen
	ldb curposx		; get current horizontal coordinate on screen
	deca			; calculate one pixel up and left (bottom right of comparison box)
	decb
	sta hitx2			; save the calculated coordinates
	stb hity2
	suba #2			; calculate two more pixels up and left (top left of comparison box)
	subb #2
	sta hitx1			; save those calculated coordinates
	stb hity1
	tst portaloff		; are portals active?
	beq LDE70		; brif so - don't adjust counter
	inc portaloff		; bump portal disable count (will eventually wrap to 0 and re-enable portals)

* CYCLE THROUGH ALL THE PORTALS
LDE70	leau LDF2A-PTROWLEN,pcr	; point to portal list
	;leau -12,u		; compensate for leau below (above could use LDF2A-12 instead)
LDE76	leau PTROWLEN,u		; move to next item
	ldd ,u			; fetch X coordinate in maze
	lbeq LDF09		; brif end of table
* IS IT VISIBLE
	;ldd ,u			; fetch X coordinate (redundant)
	subd mazeoffx		; subtract maze display offset
	cmpd #$fffa		; is it within 6 pixels of left of screen?
	blt LDE76		; brif not - move to next item
	cmpd #$7f		; is it off the right side of the screen?
	bgt LDE76		; brif so - next item
	stb VD9			; save X coordinate for rendering
	ldd 2,u			; fetch Y coordinate
	subd mazeoffy		; subtract maze display offset
	cmpd #2			; is it within the displayable area (will part be at row 8 or lower?)
	blt LDE76		; brif not
	cmpd #$5f		; is it within the visible area (bottom)?
	bgt LDE76		; brif not
* RENDER PORTAL 
	stb VD8			; save Y coordinate for rendering
	pshs u			; save portal pointer
	leau LDF1A,pcr		; point to active portal graphic
	tst portaloff		; are portals disabled?
	beq LDEB0		; brif not
	leau LDF0A,pcr		; point to disabled portal graphic
LDEB0	lda VD8			; get Y render coordinate
	ldb VD9			; get X render coordinate
	lbsr drawsprite		; go render the portal
	puls u			; get back portal pointer
	tst portaloff		; are portals enabled?
	bne LDE76		; brif not
* PORTAL ACTIVATED?
	lda VD9			; get vertical render location
	ldb VD8			; get horizontal render location
	cmpa hity2		; are we below the bottom of the bounding box?
	bgt LDE76		; brif so - not activating portal
	cmpa hity1		; are we above the top of the bounding box?
	blt LDE76		; brif so - not activating portal
	cmpb hitx2		; are we to the right of the bounding box?
	bgt LDE76		; brif so - not activating portal
	cmpb hitx1		; are we to the left of the bounding box?
	blt LDE76		; brif so - not activating portal
* GO THROUGH PORTAL
	lbsr dobleep		; make the portal sound
	lbsr setstartpos	; reset to start coordinates
	inc curposx		; bump both coordinates two pixels down and right
	inc curposx
	inc curposy
	inc curposy

	lda #NPORTALS-1		; choose a random portal destination
	lbsr rand
	lsla			; double it for two bytes per coordinate pair
	adda #4			; move past portal location
	ldd a,u			; fetch random portal destination

	sta VBF			; save X coordinate
	clra			; zero extend Y coordinate
	lslb			; multiply by 4
	rola
	lslb
	rola
	subb curposy		; adjust for current location
	sbca #0			; propagate carry
	std mazeoffy		; set display offset of maze
	clra			; zero extend for X coordinate
	ldb VBF			; retrieve X coordinate
	lslb			; times 4
	rola
	lslb
	rola
	subb curposx		; adjust for current location
	sbca #0			; propagate carry
	std mazeoffx		; save new maze display offset
	inc portaloff		; disable portals
	;lbra LDE76		; go render another portal (this feels buggy)

LDF09	rts

 IFDEF MCUSTOM
 include map/portals.asm
 ELSE
 include portals.asm
 ENDC

drawmazeboth lbsr clearrender	; get a clean render area
	lbsr drawvert		; draw the vertical lines
	lbsr drawhoriz		; draw the horizontal lines
	lbsr swaprender		; swap screens
	lbsr clearrender	; get clean render area
	lbsr drawvert		; draw vertical lines
	lbsr drawhoriz		; draw horizontal lines
	lbra swaprender		; swap screens and return

LDF80	pshs a
	cmpa #$50
	bne LDF8C
	lda #$ff
	sta V18
	lda ,s
LDF8C	cmpa #$15
	bne LDF94
	lda #$ff
	sta V19
LDF94	puls a,pc

* Make crystal ball and crown time out eventually
LDF96	tst V18		; crown active?
	beq LDF9C	; no
	dec V18		; decrease time remaining on crown
LDF9C	tst V19		; crystal ball active?
	beq LDFA2	; no
	dec V19		; decrease time remaining on crystal ball
LDFA2	rts

vermess	fcb 13
	fcc 'VERSION 1.0.2'

* CHANGE VIDEO MODES ON SHIFT, ENTER, CLEAR
checkcssel ldd #$c07f ; SHIFT
	bsr LDFC1
	ldd #$c8fd ; CLEAR
	bsr LDFC1
	ldd #$f8fe ; ENTER
	bsr LDFC1
	ldd #$00fb ; BREAK
LDFC1	stb PIA0.DB
	ldb PIA0.DA
	andb #$7f
	cmpb #$3f
	bne LDFD0
	sta PIA1.DB
	tsta		; hard reset to RSDOS on BREAK
	bne notbreak@
	clra		; hard reset to RSDOS
	tfr a,dp
	lda #$88
	sta $ff90	; turn off MMU
	sta $ffd8	; slow CPU
	sta $ffde	; turn on ROMs
	clr $0071
	jmp [$fffe]
notbreak@
	clra		; set palette registers for coco3 RGB
	sta $FFB4
	lda #63
	sta $FFB7
	lda #9
	sta $FFB5
	lda #36
	sta $FFB6
LDFD0	rts
Coco3RGB
	lda #$c8
	sta PIA1.DB
	bra notbreak@

LDFD1	jsr SETMUX		; program analog MUX for source in B
	jmp SNDON		; enable analog MUX (enable sound output)

* VICTORY
LDFD7	pshs u,b,a		; save registers
	ldu #PIA1.DB		; point to PIA1 Data B
	lda ,u			; fetch current VDG settings
	pshs a			; save it
	lda #$c0		; 256 px, 4 color, color set 0
	sta ,u			; set VDG
	lda #10			; iterate 10 times (5 complete cycles)
LDFE6	ldb ,u			; get current VDG mode
	eorb #8			; flip color set
	stb ,u			; set new VDG mode
	lbsr dobleep
	deca			; done all iterations
	bne LDFE6		; brif not
	puls a			; get back original VDG settings
	sta ,u			; restore VDG mode
	puls pc,u,b,a		; restore registers and return

IRQ	lda PIA0.DB		; clear interrupt
	lda tick
	beq IRQ@
	dec tick
IRQ@	inc tock
	rti

	end START
