; This is the treasure table. Each treasure entry consists of:
; 1 byte: treasure score in bcd, hundreds and thousands digits
; 16 bytes: treasure sprite
; any number of two byte coordinate pairs followed by a double NUL
;
; 00 . black
; 01 B blue
; 10 R red
; 11 W white

; 93 treasures total

; jade cross (23 total = 2300 points)
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
