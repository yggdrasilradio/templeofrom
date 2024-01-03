* FOUR PART MUSIC ROUTINE
LCD49	pshs u
	clrb		; enable sound output from DAC
	jsr SETMUX
	jsr SNDON
	puls u
	clra
	clrb
	std V05		; clear notes
	std V07
	std V09
	std V0B
LCD5E	lda ,u+
	beq LCDC8
	sta V15		; note duration
	leay LCDC9,pcr
	lda ,u+
	ldd a,y
	std V0D		; get note 1 pitch delta from note table
	lda ,u+
	ldd a,y
	std V0F		; get note 2 pitch delta from note table
	lda ,u+
	ldd a,y
	std V11		; get note 3 pitch delta from note table
	lda ,u+
	ldd a,y
	std V13		; get note 4 pitch delta from note table
LCD80	ldy #$28	; note tempo
LCD84	ldx #0
	ldb V05		; add absolute value of LSB of note 1
	bpl LCD8C
	comb
LCD8C	abx
	ldb V07		; add absolute value of LSB of note 2
	bpl LCD92
	comb
LCD92	abx
	ldb V09		; add absolute value of LSB of note 3
	bpl LCD98
	comb
LCD98	abx
	ldb V0B		; add absolute value of LSB of note 4
	bpl LCD9E
	comb
LCD9E	abx
	tfr x,d
	lsra		; divide by 4 to get average
	rorb
	stb PIA1.DA	; send value to DAC (top 6 bits of PIA1)
	ldd V05
	addd V0D	; update note 1 with pitch delta
	std V05
	ldd V07
	addd V0F	; update note 2 with pitch delta
	std V07
	ldd V09
	addd V11	; update note 3 with pitch delta
	std V09
	ldd V0B
	addd V13	; update note 4 with pitch delta
	std V0B
	leay ,-y	; note tempo
	bne LCD84
	dec V15		; note duration
	bne LCD80	; continue note
	bra LCD5E	; next note
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
LCE2B	fcb $12,$32,$2c,$52,$3a
	fcb $12,$36,$30,$54,$3c
	fcb $12,$3a,$2c,$58,$28
	fcb $12,$3c,$30,$5c,$2c
	fcb $48,$40,$36,$60,$48
	fcb $12,$40,$32,$40,$32
	fcb $12,$40,$30,$40,$30
	fcb $36,$3c,$30,$5c,$3c
	fcb $12,$3c,$32,$60,$3c
	fcb $24,$3c,$30,$5c,$3c
	fcb $24,$3a,$30,$58,$3a
	fcb $12,$3a,$32,$58,$3a
	fcb $24,$3a,$30,$3a,$30
	fcb $12,$3a,$28,$3a,$28
	fcb $5a,$36,$28,$54,$36
	fcb $12,$36,$24,$36,$24
	fcb $24,$32,$28,$52,$3a
	fcb $24,$32,$28,$4a,$32
	fcb $24,$32,$28,$4e,$36
	fcb $27,$32,$28,$52,$3a
	fcb $00

* SONG 2: CHOPIN'S FUNERAL MARCH FOR PLAYER'S DEATH
LCE90	fcb $6c,$32,$38,$40,$0a
	fcb $04,$00,$00,$00,$00
	fcb $48,$32,$38,$40,$0a
	fcb $04,$00,$00,$00,$00
	fcb $24,$32,$38,$40,$0a
	fcb $04,$00,$00,$00,$00
	fcb $6c,$32,$38,$40,$0a
	fcb $04,$00,$00,$00,$00
	fcb $48,$38,$30,$42,$38
	fcb $04,$00,$00,$00,$00
	fcb $24,$36,$32,$40,$36
	fcb $04,$00,$00,$00,$00
	fcb $48,$36,$32,$40,$36
	fcb $04,$00,$00,$00,$00
	fcb $24,$32,$38,$40,$0a
	fcb $04,$00,$00,$00,$00
	fcb $48,$32,$38,$40,$0a
	fcb $04,$00,$00,$00,$00
	fcb $24,$30,$38,$40,$08
	fcb $04,$00,$00,$00,$00
	fcb $76,$32,$38,$40,$0a
	fcb $00
