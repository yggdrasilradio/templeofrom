; Disassembly of Temple of ROM by William Astle
; (February 20-25, 2018)
; 16 character tabs

; General memory map:
; 0000-00FF	direct page variables
; 0100-03FF	other variables, stack
; 0400-0FFF	graphics screen #1
; 1000-1BFF	graphics screen #2
; 1C00		explosion sprite queue

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
POTVAL	equ $15a

SCREEN1	equ $400
SCREEN2	equ $1000

*** SEGMENT 1 ***

; These are the variables used by the game.

	setdp 0
creatureptr rmb 2 ; pointer to current creature locations
portaloff rmb 1 ; nonzero means portals are currently disabled
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
V18	rmb 1
V19	rmb 1
V1A	rmb 1
V4F	rmb 1
V50	rmb 2
V52	rmb 2
V5C	rmb 1
V5D	rmb 2
V5F	rmb 2
V68	rmb 1
V69	rmb 1
V8D	rmb 1
V8E	rmb 1
randseed rmb 1 ; the "random seed"
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
VD1	rmb 1
scorep1	rmb 3 ; player one's score
VD5	rmb 1
VD6	rmb 1
VD7	rmb 1 ; laser sound value
VD8	rmb 1
VD9	rmb 1
VDA	rmb 1
collision rmb 1 ; collision detection flag
VDC	rmb 1
VDD	rmb 1
VDE	rmb 1
VDF	rmb 1
VE0	rmb 1
numplayers	rmb 1 ; number of players in the game
scrollstep	rmb 2 ; step/direction for maze scrolling
plr1state	rmb 6 ; player two game state (6 bytes)
plr2state	rmb 6 ; player one game state (6 bytes)
scorep2		rmb 3 ; player two's score
scoreptr	rmb 2 ; pointer to current player's score
texttty		rmb 1 ; whether the "beeping tty" effect is enabled for text
objlistptr	rmb 2 ; pointer to current player's object list
curplayer	rmb 1 ; current player number (oddly, 2 = player 1, 1 = player 2)
VF9	rmb 1
VFA	rmb 1
 IFDEF MLASER
sptr rmb 2
 ENDC

creatures	equ $200	; the master creature list
plr1objlist	equ $1c20	; player one's list of objects in the maze
plr1creatures	equ $1db0	; where the creatures really are for player one
plr2objlist	equ $1e10	; player two's list of objects in the maze
plr2creatures	equ $1fa0	; where the creatures really are for player two

; This is the actual ROM code.

	org $3000
START	orcc #$50			; make sure interrupts are disabled
	clr $ff40			; make sure all FDC drive motors and selects are off
	lbra LCD46			; launch main initialization sequence
; fetch in various source files
                include sound.asm       ; fetch sound handling routines
                include joystick.asm    ; fetch joystick handling routines

; Render the vertical lines of the map
drawvert	leau LC34A,pcr		; point to short circuit offset table for vertical lines
	ldd mazeoffx			; fetch screen display offset for maze
	bge LC013			; brif screen X offset is positive
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
	;bne LC091		; brif not
	bpl LC091
	rts

LC09A	fcb $40,$10,$04,$01	; pixel masks for maze walls (color #1)

drawhoriz	leau LC36A,pcr	; point to short circuit offsets for horizontal drawing
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
	pshs a			; save pixel mask
LC125	decb			; are we done yet?
	blt LC148		; brif so
	lda ,s			; get pixel mask
	ora ,x			; merge with screen data
	sta ,x			; save on screen
	lsr ,s			; shift pixel mask to next pixel
	lsr ,s
	bne LC125		; brif we haven't got to the end of the byte
	leax 1,x		; move to next byte
	lda #$55		; set up to do whole bytes
LC138	subb #4			; do we have a whole byte worth?
	blt LC140		; brif not
	sta ,x+			; do a whole byte
	bra LC138		; try again
LC140	addb #4			; reset for subb above
	lda #$40		; set up for leftmost pixel in byte
	sta ,s			; set pixel masks
	bra LC125		; go complete the line
LC148	puls a			; clean up stack
	rts

; 00 black
; 01 blue
; 10 red
; 11 white

; explosion sprites

LC14B	fcb $05 ; explosion1-LC14B
	fcb $14 ; explosion2-LC14B
	fcb $23 ; explosion3-LC14B
	fcb $32 ; explosion4-LC14B
	fcb $00 ; end of table marker

explosion1
	fcb $0000 ; 0000000000000000	........
	fdb $0000 ; 0000000000000000	........
	fdb $000c ; 0000000000001100	......W.
	fdb $000f ; 0000000000001111	......WW
	fdb $c00c ; 1100000000001100	W.....W.
	fdb $0000 ; 0000000000000000	........
	fdb $0000 ; 0000000000000000	........
	fdb $0000 ; 0000000000000000	........
	fdb $0000

explosion2
	fcb $c000 ; 1100000000000000
	fdb $0c30 ; 0000110000110000
	fdb $300e ; 0011000000001110
	fdb $c00a ; 1100000000001010
	fdb $800e ; 1000000000001110
	fdb $c030 ; 1100000000110000
	fdb $30c0 ; 0011000011000000
	fdb $0c00 ; 0000110000000000
	fdb $0000

explosion3
	fcb $0000 ; 0000000000000000
	fdb $2020 ; 0010000000100000
	fdb $c008 ; 1100000000001000
	fdb $80ca ; 1000000011001010
	fdb $000c ; 0000000000001100
	fdb $8000 ; 1000000000000000
	fdb $0080 ; 0000000010000000
	fdb $3000 ; 0011000000000000
	fdb $0000

explosion4
	fcb $0000 ; 0000000000000000
	fdb $000c ; 0000000000001100
	fdb $0000 ; 0000000000000000
	fdb $2003 ; 0010000000000011
	fdb $0008 ; 0000000000001000
	fdb $0000 ; 0000000000000000
	fdb $0000 ; 0000000000000000
	fdb $0000 ; 0000000000000000
	fcb $0000

; one byte is 8 bits, 2 bits per pixel, so one byte is 4 pixels, each sprite is 8x8 so 8x2 bytes = 16 bytes

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
	fcc 'TEMPLE OF ROM II'

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
fontdata	fcb $f0,$5f,$17,$80		; A
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

 incl mapdata.asm

*** SEGMENT 2 ***

LCD46	lbra LCF50

* FOUR PART MUSIC ROUTINE
LCD49	pshs u
	clrb			; enable sound output from DAC
	jsr SETMUX
	jsr SNDON
	puls u
	clra
	clrb
	std V05
	std V07
	std V09
	std V0B
LCD5E	lda ,u+
	beq LCDC8
	sta V15
	leay LCDC9,pcr
	lda ,u+
	ldd a,y
	std V0D
	lda ,u+
	ldd a,y
	std V0F
	lda ,u+
	ldd a,y
	std V11
	lda ,u+
	ldd a,y
	std V13
LCD80	ldy #$28
LCD84	ldx #0
	ldb V05
	bpl LCD8C
	comb
LCD8C	abx
	ldb V07
	bpl LCD92
	comb
LCD92	abx
	ldb V09
	bpl LCD98
	comb
LCD98	abx
	ldb V0B
	bpl LCD9E
	comb
LCD9E	abx
	tfr x,d
	lsra
	rorb
	stb PIA1.DA
	ldd V05
	addd V0D
	std V05
	ldd V07
	addd V0F
	std V07
	ldd V09
	addd V11
	std V09
	ldd V0B
	addd V13
	std V0B
	leay ,-y
	bne LCD84
	dec V15
	bne LCD80
	bra LCD5E
LCDC8	rts

* MUSICAL NOTES
LCDC9	fcb $00,$00,$02,$6f,$02,$94,$02,$eb
	fcb $02,$e4,$03,$10,$03,$3f,$03,$71
	fcb $03,$a5,$03,$dc,$04,$17,$04,$56
	fcb $04,$98,$04,$dd,$05,$28,$05,$76
	fcb $05,$c9,$06,$21,$06,$7f,$06,$e2
	fcb $07,$4a,$07,$b9,$08,$2f,$08,$ac
	fcb $09,$30,$09,$bb,$0a,$50,$0a,$ed
	fcb $0b,$93,$0c,$43,$0c,$fe,$0d,$c4
	fcb $0e,$95,$0f,$73,$10,$5f,$11,$58
	fcb $12,$60,$13,$77,$14,$a0,$15,$da
	fcb $17,$26,$18,$87,$19,$fc,$1b,$88
	fcb $1d,$2b,$1e,$e7,$20,$be,$22,$b0
	fcb $24,$c0

* SONG 1: HAPPY "TURN STARTING" THEME -- ORIGINAL TUNE BY RICK ADAMS
* Each note is 5 bytes: duration, then a byte for each the four notes
* The table ends when there's a duration of zero
LCE2B	fcb $12,$32,$2c,$52,$3a,$12,$36,$30
	fcb $54,$3c,$12,$3a,$2c,$58,$28,$12
	fcb $3c,$30,$5c,$2c,$48,$40,$36,$60
	fcb $48,$12,$40,$32,$40,$32,$12,$40
	fcb $30,$40,$30,$36,$3c,$30,$5c,$3c
	fcb $12,$3c,$32,$60,$3c,$24,$3c,$30
	fcb $5c,$3c,$24,$3a,$30,$58,$3a,$12
	fcb $3a,$32,$58,$3a,$24,$3a,$30,$3a
	fcb $30,$12,$3a,$28,$3a,$28,$5a,$36
	fcb $28,$54,$36,$12,$36,$24,$36,$24
	fcb $24,$32,$28,$52,$3a,$24,$32,$28
	fcb $4a,$32,$24,$32,$28,$4e,$36,$27
	fcb $32,$28,$52,$3a,$00

* SONG 2: CHOPIN'S FUNERAL MARCH FOR PLAYER'S DEATH
LCE90	fcb $6c,$32,$38,$40,$0a,$04,$00,$00
	fcb $00,$00,$48,$32,$38,$40,$0a,$04
	fcb $00,$00,$00,$00,$24,$32,$38,$40
	fcb $0a,$04,$00,$00,$00,$00,$6c,$32
	fcb $38,$40,$0a,$04,$00,$00,$00,$00
	fcb $48,$38,$30,$42,$38,$04,$00,$00
	fcb $00,$00,$24,$36,$32,$40,$36,$04
	fcb $00,$00,$00,$00,$48,$36,$32,$40
	fcb $36,$04,$00,$00,$00,$00,$24,$32
	fcb $38,$40,$0a,$04,$00,$00,$00,$00
	fcb $48,$32,$38,$40,$0a,$04,$00,$00
	fcb $00,$00,$24,$30,$38,$40,$08,$04
	fcb $00,$00,$00,$00,$76,$32,$38,$40
	fcb $0a,$00

; This generates a pseudo random sequence with period 128.
random	pshs a			; save registers
	lda randseed		; get current random seed
	ldb #123		; get fudge factor
	mul			; multiply
	addb #9			; add something for fun
	stb randseed		; save new random seed (and result)
	puls a			; restore registers
	rts

; Wait for VSYNC

WaitVSYNC
  pshs a
  lda #0
  sta $ff9a
	tst PIA0.DB	; dismiss interrupt 
LCF0B	tst PIA0.CB	; wait for interrupt
	bge LCF0B
  lda #100
  sta $ff9a
  puls a,pc

;WaitVSYNC
;	tst PIA0.DB	; dismiss interrupt 
;LCF0B	tst PIA0.CB	; wait for interrupt
;	bge LCF0B
;	rts

; Clear screen one header (to colour #3)
clrheader pshs y,x,b,a		; save registers
	ldx #SCREEN1		; point to start of screen #1
	ldy #$80		; set up to clear 256 bytes (2 bytes at a time)
	ldd #$ffff		; set up to use colour #3
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

* Entry:
*  X
*  Y
*  D
* Exit:
*  CC
LCF3C	pshs b,a
	cmpx ,s
	bhi LCF4B
	cmpy ,s
	bcs LCF4B
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
	lda #$f8		; code for 2 colour 256px graphics, colour set 1
	sta PIA1.DB		; set VDG graphics mode
	sta SAM+5		; set SAM V2 (32 bytes per row, 96 rows)
 IFDEF MLASER
	lbsr InitLaser
 ENDC
	ldd #SCREEN2		; set render screen to screen #2
	std renderscr
	lbsr Coco3RGB 		; default to RGB no artifacting
	ldd #SCREEN1		; set bottom of screen to clear to start of screen #1
	std endclear
	lbsr LD3B9		; clear both screens
	lbsr clearscores	; reset both players scores
	clr numplayers		; set to no players
	leax LDD23,pcr		; point to creature data table
	ldy #creatures		; point to unpacked location for creature data table
LCF85	ldd ,x			; are we at the end of the table?
	beq LCF92		; brif so
	lbsr LD0C1		; expand the base/offset pairs for the creature
	lda ,x+
	sta ,y+
	bra LCF85		; go handle another
LCF92	std ,y			; save end of table flag
LCF94	lbsr LD531
	clr V03
	ldu #plr1objlist	; point to player one's object list
	lbsr buildobjlist	; build object list
	ldu #plr2objlist	; point to player two's object list
	lbsr buildobjlist	; build object list
	ldu #plr1creatures	; point to player one's creature locations
	lbsr LDCE6		; set default locations
	ldu #plr2creatures	; point to player two's creature locations
	lbsr LDCE6		; set default locations
	lbsr setstartpos	; set default start versions
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
	sta VD1
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

setstartpos	pshs a,b	; save registers
	ldd #62*256+46		; default screen position (62,46)
	sta curposx		; set horizontal position
	stb curposy		; set vertical position
	ldd #398		; set starting screen position (X)
	std mazeoffx
	ldd #291		; set starting screen position (Y)
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
	clr V18
	clr V19
	clr VD1
	ldu #plr1creatures	; pont to player one's creature locations
	stu creatureptr		; save as creature location pointer
	ldu #scorep1		; point to player one's score
	stu scoreptr		; set as current score pointer
	ldu #plr1objlist	; point to player one's object list
	stu objlistptr		; set as current object list pointer
	leau plr1mess+1,pcr	; point to player one header message
	lbsr showmess		; show it
	clra			; stop maze scroll
	clrb
	std scrollstep
	clr VD5
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
	clr V18
	clr V19
	clr VD1
	ldu #plr2creatures	; point to player two's creature locations
	stu creatureptr		; set as creature location pointer
	ldu #scorep2		; point to player two's score
	stu scoreptr		; set as current score pointer
	ldu #plr2objlist	; point to the object list for player two
	stu objlistptr		; save as current object list
	leau plr2mess+1,pcr	; point to player two header message
	lbsr showmess		; set heading
	clra			; stop maze scroll
	clrb
	std scrollstep
	clr VD5
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
	lbsr LD254
	lbsr LD254
	lbsr LD254
	lbsr checkcssel		; check for colour set selection keys
	lbsr LDF96
	lbsr clearrender	; clear workspace
	lbsr drawvert
	lbsr drawhoriz
	bsr LD1FF
	bsr LD1FF
	lbsr LD5C4
	lbsr LD829
	lbsr LDA93
	lbsr LDB96
	lda VD5
	pshs a
	lbsr LD9EF
	lda VD5
	ora ,s+
	sta VD5
	lbsr LD40B
	lbsr LDE58
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

LD254	lda curposx
	ldx mazeoffx
	ldy #$1c00		; explosion sprite queue
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
	lda VD1
	inca
	bne LD31B
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
 IFNDEF MLASER
	sts VBF			; save stack pointer
	lds #0			; use S for clearing too
LD3DB	pshu s,y,x,dp,b,a	; 9 bytes x 1 = 9 bytes
	lda VD7
	sta PIA1.DA		; tikkatikkatikka sound
	clra
	pshu s,y,x,dp,b,a	; 9 bytes x 7 = 63 bytes
	pshu s,y,x,dp,b,a
	pshu s,y,x,dp,b,a
	pshu s,y,x,dp,b,a
	pshu s,y,x,dp,b,a
	pshu s,y,x,dp,b,a
	pshu s,y,x,dp,b,a
	clr PIA1.DA		; tikkatikkatikka sound
	pshu s,y,x,dp,b,a	; 9 bytes x 6 = 56 bytes
	pshu s,y,x,dp,b,a
	pshu s,y,x,dp,b,a
	pshu s,y,x,dp,b,a
	pshu s,y,x,dp,b,a
	pshu s,y,x,dp,b,a
	pshu b,a		; 2 bytes x 1 = 2 bytes
	cmpu endclear		; have we reached the start of the screen?
	bgt LD3DB		; brif not, do another 128 bytes
	lds VBF			; restore stack pointer
 ELSE
LD3DB	pshu d,x,y,dp		; 7 bytes x 18 = 126 bytes
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

LD40B	ldb PIA0.DA		; read row data from keyboard (gets joystick buttons)
	andb curplayer		; mask off button for the correct player
	lbne LD495		; brif button not pressed
	tst VD1
	lbne LD4C7
	inc VD1
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
 IFDEF MLASER
	lbsr FireLaser
 ENDC
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
LD495	clr VD1
LD497	rts

LD498	lda VC9
	ldb VC7
	tst V18
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

LD4C7	lda VD1
	inca
	beq LD497
	sta VD1
	rts

LD4CF	tsta
	blt LD4DC
	cmpd #$100
	bge LD4E7
	lslb
	rola
	bra LD4E4
LD4DC	cmpd #$ff00
	ble LD4E7
	lslb
	rola
LD4E4	andcc #$fd
	rts

LD4E7	orcc #4
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
	andb color	; get correct pixel data in the all colour byte
	sta ,x		; save cleared pixel data
	orb ,x		; merge it with new pixel data
	stb ,x		; set screen data
	orcc #4		; set Z (no collision)
	rts

LD524	inc collision	; flag collision
	andb color	; get correct pixel data in all colour byte
	sta ,x		; save cleared pixel data
	orb ,x		; merge it with new pixel data
	stb ,x		; set screen data
LD52E	andcc #$fb	; flag collision (Z clear)
	rts

* Clear explosion sprite queue
LD531	clra
	clrb
	std $1c00
	std $1c04
	std $1c08
	std $1c0c
	std $1c10
	std $1c14
	std $1c18
	std $1c1c
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
	ldu #$1c00
	ldx #8
LD562	ldd ,u		; find blank slot in 8 slots of 4 bytes each
	beq LD56E
	leau 4,u
	leax -1,x
	bne LD562
	bra LD57C
LD56E	leax LC14B,pcr
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
	leay colors,pcr		; point to all pixel colour masks
	lda a,y			; get colour mask for this colour
	sta color		; save colour mask for rendering
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

colors	fcb $00,$55,$aa,$ff	; all pixel colour masks for colours 0, 1, 2, 3

; render all queued explosions

LD5C4	ldu #$1c00 ; explosion table
	lda #8
	pshs a
	pshs u
LD5CD	ldu ,s	; u points to explosion table
	ldd ,s  ; address of explosion table
	addd #4 ; advance to next entry
	std ,s
	ldx ,u ; address of sprite
	beq LD5F4	; on last sprite
	ldd ,u 
	addd #1
	std ,u
	ldx ,u
	ldb ,x
	beq LD5F0
	abx
	ldd 2,u ; get coordinates
	tfr x,u ; get sprite pointer
	bsr drawsprite
	bra LD5F4
LD5F0	clra
	clrb
	std ,u
LD5F4	dec 2,s
	bne LD5CD
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

buildobjlist	clr VDA		; clear object count
	leax objcross+1,pcr	; point to jade cross object data
	bsr LD6DA		; add to object list
	leax objring+1,pcr	; point to diamond ring object data
	bsr LD6DA		; add to object list
	leax objcup+1,pcr	; point to golden cup object data
	bsr LD6DA		; add to object list
	leax objgoblet+1,pcr	; point to crystal goblet object data
	bsr LD6DA		; add to object list
	leax objball+1,pcr	; point to crystal ball object data
	bsr LD6DA		; add to object list
	leax objpitcher+1,pcr	; point to silver pitcher object data
	bsr LD6DA		; add to object list
	leax objcrown+1,pcr	; point to golden crown object data
	;bra LD6DA		; add to object list (this instruction is redundant)
LD6DA	leay $10,x		; point to coordinate list (move past sprite data)
LD6DD	ldd ,y++		; get coordinates
	beq LD6E9		; brif end of list
	std ,u++		; save coordinates of object
	stx ,u++		; save pointer to object sprite
	inc VDA			; bump object count
	bra LD6DD		; go see if there's another of this object type
LD6E9	rts

; This is the object table. Each object entry consists of:
; 1 byte: object score in bcd, hundreds and thousands digits
; 16 bytes: object sprite
; any number of two byte coordinate pairs followed by a double NUL
; jade cross (23 total = 2300 points)
;
; 00 . black
; 01 B blue
; 10 R red
; 11 W white
objcross	fcb $01
	fdb %0000000000000000 ; ........
LD6ED	fdb %0000000000000000 ; ........
	fdb %0000001000000000 ; ...R....
	fdb %0000101010000000 ; ..RRR...
	fdb %0000001000000000 ; ...R....
	fdb %0000000000000000 ; ........
	fdb %0000000000000000 ; ........
	fdb %0000000000000000 ; ........
	fcb $29,$09
	fcb $1b,$24
	fcb $87,$13
	fcb $e5,$22
	fcb $37,$7b
	fcb $71,$46
	fcb $8c,$44
	fcb $5e,$4c
	fcb $84,$4c
	fcb $8b,$5a
	fcb $74,$62
	fcb $87,$63
	fcb $60,$69
	fcb $59,$71
	fcb $a7,$58
	fcb $c8,$51
	fcb $d6,$64
	fcb $e4,$61
	fcb $44,$8b
	fcb $25,$aa
	fcb $81,$af
	fcb $64,$b6
	fcb $c8,$93
	fcb $00,$00
; diamond ring (23 total = 4600 points)
objring	fcb $02
	fdb %0000000000000000 ; ........
	fdb %0000001111000000 ; ...WW...
	fdb %0000001010000000 ; ...RR...
	fdb %0000100000100000 ; ..R..R..
	fdb %0000100000100000 ; ..R..R..
	fdb %0000001010000000 ; ...RR...
	fdb %0000000000000000 ; ........
	fdb %0000000000000000 ; ........
	fcb $48,$04
	fcb $04,$0d
	fcb $40,$0e
	fcb $2e,$19
	fcb $8b,$02
	fcb $56,$35
	fcb $8b,$34
	fcb $b3,$06
	fcb $35,$50
	fcb $3d,$6d
	fcb $10,$74
	fcb $58,$5b
	fcb $61,$65
	fcb $b6,$52
	fcb $2a,$96
	fcb $02,$9b
	fcb $0d,$a8
	fcb $40,$aa
	fcb $73,$8e
	fcb $63,$8f
	fcb $66,$9f
	fcb $a6,$86
	fcb $b4,$ae
	fcb $00,$00
; golden cup (19 total = 9500 points)
objcup	fcb $05
	fdb %0000000000000000 ; .........
	fdb %0010101110100000 ; .RRWRR...
	fdb %0010101110001000 ; .RRWR.R..
	fdb %0010101110100000 ; .RRWRR...
	fdb %0010111010000000 ; .RWRR....
	fdb %0000101000000000 ; ..RR.....
	fdb %0000000000000000 ; .........
	fdb %0000000000000000 ; .........
	fcb $29,$24
	fcb $05,$28
	fcb $3f,$34
	fcb $63,$19
	fcb $63,$35
	fcb $d8,$05
	fcb $b4,$1e
	fcb $c8,$37
	fcb $1f,$5c
	fcb $25,$70
	fcb $e2,$57
	fcb $c5,$58
	fcb $ac,$62
	fcb $1c,$95
	fcb $3b,$98
	fcb $3a,$b5
	fcb $8a,$9a
	fcb $72,$a2
	fcb $bb,$85
	fcb $00,$00
; crystal ball (10 total = 15000 points)
objball	fcb $15
	fdb %0000010101000000
	fdb %0001010101010000
	fdb %0101111101010100
	fdb %0101110101010100
	fdb %0101010111010100
	fdb %0001010101010000
	fdb %0000010101000000
	fdb %0000000000000000
	fcb $70,$0e
	fcb $a8,$11
	fcb $3d,$4c
	fcb $d6,$4e
	fcb $e6,$78
	fcb $11,$9b
	fcb $41,$a1
	fcb $03,$b8
	fcb $d9,$a4
	fcb $c6,$b5
	fcb $00,$00
; crystal goblet (10 total = 10000 points)
objgoblet	fcb $10
	fdb %0001010111010000 ; .BBBWB..
	fdb %0001010111010000 ; .BBBWB..
	fdb %0001010111010000 ; .BBBWB..
	fdb %0000010101000000 ; ..BBB...
	fdb %0000000100000000 ; ...B....
	fdb %0000000100000000 ; ...B....
	fdb %0001010111010000 ; .BBBWB..
	fdb %0000000000000000 ; ........
	fcb $22,$03
	fcb $38,$3a
	fcb $05,$3b
	fcb $58,$08
	fcb $b9,$10
	fcb $a5,$22
	fcb $10,$48
	fcb $10,$8e
	fcb $73,$b8
	fcb $b2,$9f
	fcb $00,$00
; silver pitcher (5 total = 15000 points)
objpitcher	fcb $30
	fdb %0000000000000000 ; ........
	fdb %0011110111110000 ; .WWBWW..
	fdb %0000110111001100 ; ..WBW.W.
	fdb %0000110111001100 ; ..WBW.W.
	fdb %0011111101110000 ; .WWWBW..
	fdb %0011111101110000 ; .WWWBW..
	fdb %0000110111000000 ; ..WBW...
	fdb %0000000000000000 ; ........
	fcb $65,$1d
	fcb $96,$1f
	fcb $2c,$b4
	fcb $df,$99
	fcb $e5,$b7
	fcb $00,$00
; golden crown (3 total = 15000 points)
objcrown	fcb $50
	fdb %0000000000000000 ; ........
	fdb %0000000000000000 ; ........
	fdb %0000001100000000 ; ...W....
	fdb %1100101011001100 ; W.RRW.W.
	fdb %1010101010101000 ; RRRRRRR.
	fdb %1010101010101000 ; RRRRRRR.
	fdb %0000000000000000 ; ........
	fdb %0000000000000000 ; ........
	fcb $09,$08
	fcb $e8,$8d
	fcb $ec,$aa
	fcb $00,$00

LD829	lda curposy
	ldb curposx
	sta VE0
	stb VDE
	suba #4
	subb #4
	sta VDF
	stb VDD
	clr VDC
	ldu objlistptr
	lda VDA
	pshs a
LD841	ldb ,u
	beq LD8BF
	inc VDC
	clra
	lslb
	rola
	lslb
	rola
	subd mazeoffx
	cmpd #$fffa
	blt LD8BF
	cmpd #$7f
	bgt LD8BF
	stb VD9
	ldb 1,u
	clra
	lslb
	rola
	lslb
	rola
	subd mazeoffy
	cmpd #2
	blt LD8BF
	cmpd #$5f
	bgt LD8BF
	stu VBF
	ldu 2,u
	stb VD8
	ldd VD8
	cmpa VDF
	blt LD8A9
	cmpa VE0
	bgt LD8A9
	cmpb VDD
	blt LD8A9
	cmpb VDE
	bgt LD8A9
	clra
	clrb
	ldu VBF
	std ,u
	ldu 2,u
	lda -1,u
	lbsr LDF80
	lbsr addscore
	ldd VF9
	beq LD8A2
	lda -1,u
	lbsr addscore
LD8A2	lbsr showscore
	ldu VBF
	bra LD8BF
LD8A9	lbsr drawsprite
	beq LD8BD
	clra
	clrb
	std [VBF]
	ldd VD8
	adda #4
	addb #4
	lbsr LD54E ; queue an explosion
LD8BD	ldu VBF
LD8BF	leau 4,u
	dec ,s
	lbne LD841
	puls a
	tst VDC
	bne LD8E4
	ldu objlistptr
	lbsr buildobjlist
	ldu creatureptr
	lbsr LDCE6
	lbsr setstartpos
	lbsr LDFD7
	leau LCE2B,pcr
	lbsr LCD49
LD8E4	rts

swaprender ldd renderscr	; get start address of current render screen
	cmpd #SCREEN1		; screen number 1?
	bne LD8FD		; brif not
	ldd #SCREEN2		; set render address to screen #2
	std renderscr
	lbsr WaitVSYNC		; wait for VSYNC
	sta SAM+9		; set SAM to display screen #1
	sta SAM+12
	bra LD90B
LD8FD	ldd #SCREEN1		; set render address to screen #1
	std renderscr
	lbsr WaitVSYNC		; wait for VSYNC
	sta SAM+13		; set SAM to display screen #2
	sta SAM+8
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
	lbsr checkcssel		; check colour set selection keys
	lbsr dobleep		; do the bleep if required
	ldd 1,s			; get string pointer
	addd #1			; move to next character
	std 1,s			; save new string pointer
	lda 4,s			; get rendering X coordinate
	adda #6			; move to next character cell
	sta 4,s			; save new X cooredinate
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
scrollmaze	pshs a		; save the interation count
LD99A	dec ,s			; decrement iteration count
	beq LD9E6		; brif we're done with all the iterations
	lbsr checkcssel		; check colour set selection keys
	lbsr swaprender		; swap screens
	lbsr clearrender	; get a clear canvas
	lbsr drawvert
	lbsr drawhoriz
	ldd mazeoffy		; get Y offset for screen
	addd scrollstep		; add in step
	std mazeoffy		; save new screen offset
	ldd mazeoffx		; get X offset for screen
	addd scrollstep		; add in step
	cmpd #$336		; did we fall off the bottom right?
	blt LD9C7		; brif not
	pshs b,a		; save registers
	clra			; set up to negate the the scroll direction
	clrb
	subd scrollstep		; subtract current step from 0 (negates it)
	std scrollstep		; save new scroll step/direction
	puls b,a		; restore registers
LD9C7	cmpd #$5d		; did we fall off the top left?
	bgt LD9D7		; brif not
	pshs b,a		; save registers
	clra			; set up to negate the scroll direction
	clrb
	subd scrollstep		; subtract scroll step from 0 (negate it)
	std scrollstep		; save new step/direction
	puls b,a		; restore registers
LD9D7	std mazeoffx		; save new screen offset (X)
	ldb PIA0.DA		; read keyboard rows
	andb #3			; keep joystick buttons
	eorb #3			; set so nonzero means pressed
	beq LD99A		; brif no buttons pressed - do another iteration
	puls a			; clean up iteration count
	comb			; set nonzero (B has at least 1 bit set and at least 1 not) - button pressed
	rts

LD9E6	puls a			; clean up iteration count
	clra			; set zero flag - no buttons pressed
	rts

LD9EA	clra
	clrb
	std VF9
	rts

LD9EF	ldd VF9
	lbeq LDA92
	tst V19
	beq LDA02
	ldd V50
	addd #1
	std V50
	bra LDA08
LDA02	lbsr LDB37
	lbsr LDB37
LDA08	leau LDACF,pcr
	ldb VFA
	eorb #1
	stb VFA
	andb #1
	beq LDA19
	leau $10,u
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
	stb VDD
	addd #8
	stb VDE
	ldd V52
	subd mazeoffy
	subd #4
	stb VDF
	addd #8
	stb VE0
	lda #8
	pshs a
	ldu #$1bfc
LDA69	dec ,s
	blt LDA90
	leau 4,u
	ldd ,u
	beq LDA69
	ldd 2,u
	cmpa VDF
	bcs LDA69
	cmpa VE0
	bhi LDA69
	cmpb VDD
	bcs LDA69
	cmpb VDE
	bhi LDA69
	lbsr LD9EA
	lda #$10
	lbsr addscore
	lbsr showscore
LDA90	puls a
LDA92	rts

LDA93	pshs u,b,a
	ldd VF9
	bne LDACD
	inc V5C
	bne LDACD
	tst V19
	bne LDACD
	leau LDACF,pcr
	stu VF9
	ldd mazeoffx
	subd #$0a
	std V50
	ldd mazeoffy
	subd #$0a
	std V52
	lbsr random
	bmi LDAC1
	ldd V50
	addd #$93
	std V50
LDAC1	lbsr random
	bmi LDACD
	ldd V52
	addd #$93
	std V52
LDACD	puls pc,u,b,a

LDACF	fdb $0c30 ; ..*..*.. Here's the bat!
	fdb $3c3c ; .**..**.
	fdb $3ffc ; .******.
	fdb $f3cf ; **.**.**
	fdb $c3c3 ; *..**..*
	fdb $c003 ; *......*
	fdb $0000 ; ........
	fdb $0000 ; ........
	fdb $c003 ; *......*
	fdb $c003 ; *......*
	fdb $f3cf ; **.**.**
	fdb $3ffc ; .******.
	fdb $0c30 ; ..*..*..
	fdb $0000 ; ........
	fdb $0000 ; ........
	fdb $0000 ; ........

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

LDB37	lda #$ff
	sta VD5
	ldb curposx
	clra
	addd mazeoffx
	subd #4
	cmpd V50
	beq LDB5E
	blt LDB55
	clr VD5
	ldd V50
	addd #1
	std V50
	bra LDB5E
LDB55	ldd V50
	clr VD5
	subd #1
	std V50
LDB5E	ldb curposy
	clra
	addd mazeoffy
	subd #1
	cmpd V52
	beq LDB81
	blt LDB78
	ldd V52
	addd #1
	std V52
	clr VD5
	bra LDB81
LDB78	ldd V52
	subd #1
	clr VD5
	std V52
LDB81	rts

* PLAYER DIED
LDB82	lda #$0a
	pshs a
LDB86	lbsr LDAEF
	dec ,s
	bne LDB86
	leau LCE90,pcr		; funeral march
	lbsr LCD49		; 4 part music routine
	puls pc,a

LDB96	ldu creatureptr
	leau -4,u
	pshs u
	ldu #creatures
	leau -9,u
	clra
	ldb curposx
	addd mazeoffx
	std V5D
	clra
	ldb curposy
	addd mazeoffy
	std V5F
LDBAF	leau 9,u
	ldd ,s
	addd #4
	std ,s
	tst VD5
	beq LDBC2
	lbsr setstartpos
	lbra LDCCD

LDBC2	ldd ,u
	lbeq LDCCD
	ldx ,s
	ldd ,x
	beq LDBAF
	std V68
	ldd 2,x
	std V8D
	ldd V5D
	ldx ,u
	ldy 2,u
	leay -3,y
	lbsr LCF3C
	bne LDC02
	ldd V5F
	ldx 4,u
	ldy 6,u
	leay -3,y
	lbsr LCF3C
	bne LDC02
	tst V19
	bne LDC02
	clr V5C
	lbsr LDD08
	tst 8,u
	beq LDC28
	lbsr LDD08
	bra LDC28
LDC02	ldd 2,u
	subd ,u
	lsra
	rorb
	lsra
	rorb
	addd ,u
	ldx V68
	exg d,x
	lbsr LDCCF
	std V68
	ldd 6,u
	subd 4,u
	lsra
	rorb
	lsra
	rorb
	addd 4,u
	ldx V8D
	exg d,x
	lbsr LDCCF
	std V8D
LDC28	ldx ,s
	ldd V68
	std ,x
	ldd V8D
	std 2,x
	ldd V68
	subd mazeoffx
	std V68
	ldd V8D
	subd mazeoffy
	std V8D
	ldd V68
	cmpd #$fff8
	lblt LDCCA
	cmpd #$7f
	bgt LDCCA
	ldd V8D
	cmpd #$fff8
	blt LDCCA
	cmpd #$5f
	bgt LDCCA
	pshs u
	ldb 8,u
	leau LDD84,pcr ; spider
	leau b,u
	lda V69
	eora V8E
	anda #2
	beq LDC71
	leau 16,u
LDC71	lda V8E
	ldb V69
	lbsr drawsprite
	puls u
	ldd V68
	subd #4
	stb VDD
	addd #8
	stb VDE
	ldd V8D
	subd #4
	stb VDF
	addd #8
	stb VE0
	lda #8
	pshs u
	pshs a
	ldu #$1bfc
LDC9B	dec ,s
	blt LDCC6
	leau 4,u
	ldd ,u
	beq LDC9B
	ldd 2,u
	cmpa VDF
	bcs LDC9B
	cmpa VE0
	bhi LDC9B
	cmpb VDD
	bcs LDC9B
	cmpb VDE
	bhi LDC9B
	clr VD5
	clra
	clrb
	std [3,s]
	lda #$10
	lbsr addscore
	lbsr showscore
LDCC6	puls a
	puls u
LDCCA	lbra LDBAF

LDCCD	puls pc,u
LDCCF	pshs x
	cmpd ,s
	bhi LDCDA
	bcs LDCDF
	bra LDCE4
LDCDA	subd #1
	bra LDCE2
LDCDF	addd #1
LDCE2	clr VD5
LDCE4	puls pc,x
LDCE6	ldx #creatures		; point to creature table
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
	bra LDCE9		; go process another creature
LDD07	rts

LDD08	lda #$ff
	sta VD5
	ldd V68
	ldx V5D
	leax -3,x
	lbsr LDCCF
	std V68
	ldd V8D
	ldx V5F
	leax -3,x
	lbsr LDCCF
	std V8D
	rts

; Starting creature location table
; Each entry has four coordinates in two pairs. Each pair has a base
; coordinate and an adjustment coordinate. To calcuate the adjustment,
; the base coordinate (first of the pair) is subtracted from the
; adjustment coordinate. Then the difference is divided by 4 (horizontal)
; or 2 (vertical) and added back to the base coordinate.
LDD23	fcb $57,$65,$63,$75,$00
	fcb $03,$10,$02,$10,$20
	fcb $20,$2d,$02,$10,$20
	fcb $28,$32,$18,$32,$00
	fcb $52,$68,$03,$11,$00
	fcb $6d,$80,$0b,$23,$20
	fcb $78,$8d,$0b,$23,$20
	fcb $b2,$bf,$03,$14,$00
	fcb $d0,$db,$03,$14,$00
	fcb $38,$48,$93,$a4,$00
	fcb $20,$30,$a8,$bb,$00
	fcb $38,$48,$a8,$bb,$20
	fcb $a5,$af,$4d,$67,$00
	fcb $b3,$bd,$4d,$67,$00
	fcb $c1,$cb,$4d,$67,$00
	fcb $30,$41,$4a,$5f,$20
	fcb $0d,$23,$55,$60,$00
	fcb $61,$78,$b0,$bd,$00
	fcb $b8,$d4,$83,$a5,$20
	fcb $00,$00		; mark end of table

LDD84	fcb $20,$08,$08,$20,$03,$c0,$ab,$ea	; 2 spider sprites 8x8
	fcb $0b,$e0,$23,$c8,$83,$c2,$80,$02

	fcb $08,$20,$88,$22,$23,$c8,$0b,$e0
	fcb $03,$c0,$2b,$e8,$83,$c2,$00,$00

	fcb $20,$20,$02,$08,$20,$80,$2a,$20	; 2 fireball sprites 8x8
	fcb $0a,$88,$0a,$a0,$0a,$a0,$02,$80

	fcb $02,$00,$20,$80,$08,$80,$20,$08
	fcb $28,$a0,$0a,$a0,$0a,$a0,$02,$80

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
	andb ,x			; read screen data and clear the pixel to colour #0
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
	sta VE0			; save the calculated coordinates
	stb VDE
	suba #2			; calculate two more pixels up and left (top left of comparison box)
	subb #2
	sta VDF			; save those calculated coordinates
	stb VDD
	tst portaloff		; are portals active?
	beq LDE70		; brif so - don't adjust counter
	inc portaloff		; bump portal disable count (will eventually wrap to 0 and re-enable portals)
LDE70	leau LDF2A,pcr		; point to portal list
	leau -12,u		; compensate for leau below (above could use LDF2A-12 instead)
LDE76	leau 12,u		; move to next item
	ldd ,u			; fetch X coordinate in maze
	lbeq LDF09		; brif end of table
	ldd ,u			; fetch X coordinate (redundant)
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
	lda VD9			; get vertical render location
	ldb VD8			; get horizontal render location
	cmpa VDE		; are we below the bottom of the bounding box?
	bgt LDE76		; brif so - not activating portal
	cmpa VDD		; are we above the top of the bounding box?
	blt LDE76		; brif so - not activating portal
	cmpb VE0		; are we to the right of the bounding box?
	bgt LDE76		; brif so - not activating portal
	cmpb VDF		; are we to the left of the bounding box?
	blt LDE76		; brif so - not activating portal
	lbsr dobleep		; make the portal sound
	lbsr setstartpos	; reset to start coordinates
	inc curposx		; bump both coordinates two pixels down and right
	inc curposx
	inc curposy
	inc curposy
	lbsr random		; get a random value
	lsrb			; keep bits 1,2
	andb #3
	lslb			; double it for two bytes per coordinate pair
	addb #4			; move past portal location
	ldd b,u			; fetch portal destination ("random" selection from four choices)
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
	lbra LDE76		; go render another portal (this feels buggy)

LDF09	rts

; Inactive portal graphic
LDF0A	fdb %0000010101000000
	fdb %0001000000010000
	fdb %0100000000000100
	fdb %0100000000000100
	fdb %0100000000000100
	fdb %0001000000010000
	fdb %0000010101000000
	fdb %0000000000000000

; Active portal graphic
LDF1A	fdb %0000010101000000
	fdb %0001000000010000
	fdb %0100000000000100
	fdb %0100001100000100
	fdb %0100000000000100
	fdb %0001000000010000
	fdb %0000010101000000
	fdb %0000000000000000

; Portal list; each entry consists of:
; X coordinate (16 bits)
; Y coordinate (16 bits)
; 4 sets of destination coordinates (in packed divided by four format)
;   one of which is chosen at "random" when a portal is activated
LDF2A	fcb $01,$b0,$01,$54			; PORTAL 1 6c55
	fcb $0b,$0b,$ba,$09,$13,$48,$aa,$89	; 	PORTAL 2 PORTAL 3 PORTAL 4 PORTAL 5
	fcb $00,$2c,$00,$2c			; PORTAL 2 0b0b
	fcb $6c,$55,$ba,$09,$13,$48,$aa,$89	;	PORTAL 1 PORTAL 3 PORTAL 4 PORTAL 5 
	fcb $02,$e8,$00,$24			; PORTAL 3 ba09
	fcb $0b,$0b,$6c,$55,$13,$48,$aa,$89	;	PORTAL 2 PORTAL 1 PORTAL 4 PORTAL 5 
	fcb $00,$4c,$01,$20			; PORTAL 4 1348
	fcb $0b,$0b,$ba,$09,$6c,$55,$aa,$89	;	PORTAL 2 PORTAL 3 PORTAL 1 PORTAL 5 
	fcb $02,$a8,$02,$24			; PORTAL 5 aa89
	fcb $0b,$0b,$ba,$09,$13,$48,$6c,$55	;	PORTAL 2 PORTAL 3 PORTAL 4 PORTAL 1 
	fdb 0			; flag end of table

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

LDF96	tst V18
	beq LDF9C
	dec V18
LDF9C	tst V19
	beq LDFA2
	dec V19
LDFA2	rts

vermess	fcb 13
	fcc 'VERSION 1.0.2'

;checkcssel	ldd #$c07f		; code for 256 px 2 colour, colour set 0, and scan code for SHIFT
;	bsr LDFC1			; scan keyboard and set VDG if pressed
;	ldd #$c8fd			; code for 128 px 4 colour, colour set 1, and scan code for CLEAR
;	bsr LDFC1			; scan keyboard and set VDG if pressed
;	ldd #$f8fe			; code for 128 px 4 colour, colour set 1, and scan code for ENTER
;LDFC1	stb PIA0.DB			; save column strobe
;	ldb PIA0.DA			; read keyboard data
;	andb #$7f			; mask off comparator
;	cmpb #$3f			; do we have a key press in row 6?
;	bne LDFD0			; brif not
;	sta PIA1.DB			; program VDG
;LDFD0	rts

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
	lda #$c0		; 256 px, 4 colour, colour set 0
	sta ,u			; set VDG
	lda #10			; iterate 10 times (5 complete cycles)
LDFE6	ldb ,u			; get current VDG mode
	eorb #8			; flip colour set
	stb ,u			; set new VDG mode
	lbsr dobleep
	deca			; done all iterations
	bne LDFE6		; brif not
	puls a			; get back original VDG settings
	sta ,u			; restore VDG mode
	puls pc,u,b,a		; restore registers and return

 IFDEF MLASER
	incl laser.asm
 ENDC

	end START
