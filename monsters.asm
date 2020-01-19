; Monster location table (19 x 5 + 2 = 97 bytes)
; X1, X2, Y1, Y2, monster ID (00 = spider, 20 = fireball)
; (X1, Y1) and (X2, Y2) are the corners of a box
; The monster's home position is 1/4 down from the top edge of the box
; and 1/4 from the left edge of the box
; Players entering the box aggro the monster
; Coordinates are 16 bits divided by 4 and stored in 8 bits

LDD23
***
	fcb $57,$65,$63,$75,$00
	fcb $03,$10,$02,$10,$20 ; fireball in queen's bedroom
	fcb $20,$2d,$02,$10,$20 ; fireball in king's bedroom
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
	fcb $61,$78,$b0,$bd,$00 ; spider at center bottom of maze
	fcb $b8,$d4,$83,$a5,$20 ; fireball at lower right
***
	fcb $00,$00		; mark end of table

LDD84	fcb $20,$08 ; .R....R.	2 spider sprites 8x8
	fcb $08,$20 ; ..R..R..
	fcb $03,$c0 ; ...WW...
	fcb $ab,$ea ; RRRWWRRR
	fcb $0b,$e0 ; ..RWWR..
	fcb $23,$c8 ; .R.WW.R.
	fcb $83,$c2 ; R..WW..R
	fcb $80,$02 ; R......R

	fcb $08,$20 ; ..R..R..
	fcb $88,$22 ; R.R..R.R
	fcb $23,$c8 ; .R.WW.r.
	fcb $0b,$e0 ; ..RWWR..
	fcb $03,$c0 ; ...WW...
	fcb $2b,$e8 ; .RRWWRR.
	fcb $83,$c2 ; R..WW..R
	fcb $00,$00 ; ........

	fcb $20,$20 ; .R...R.. 2 fireball sprites 8x8
	fcb $02,$08 ; ...R..R.
	fcb $20,$80 ; .R..R...
	fcb $2a,$20 ; .RRR.R..
	fcb $0a,$88 ; ..RRR.R.
	fcb $0a,$a0 ; ..RRRR..
	fcb $0a,$a0 ; ..RRRR..
	fcb $02,$80 ; ...RR...

	fcb $02,$00 ; ...R....
	fcb $20,$80 ; .R..R...
	fcb $08,$80 ; ..R.R...
	fcb $20,$08 ; .R....R.
	fcb $28,$a0 ; .RR.RR..
	fcb $0a,$a0 ; ..RRRR..
	fcb $0a,$a0 ; ..RRRR..
	fcb $02,$80 ; ...RR...

	* Ghost by Pat Ferguson

	fdb %0000111111000000 ; ..WWW... 2 ghost sprites 8x8
	fdb %0011111111110000 ; .WWWWW..
	fdb %1101111111011100 ; WBWWWBW.
	fdb %1110011101101100 ; WRBWBRW.
	fdb %1110101110101100 ; WRRWRRW.
	fdb %1111111111111100 ; WWWWWWW.
	fdb %0011001100110000 ; .W.W.W..
	fdb %0000000000000000 ; ........

	fdb %0000111111000000 ; ..WWW...
	fdb %0011111111110000 ; .WWWWW..
	fdb %1101111111011100 ; WBWWWBW.
	fdb %1110011101101100 ; WRBWBRW.
	fdb %1110101110101100 ; WRRWRRW.
	fdb %1111111111111100 ; WWWWWWW.
	fdb %1100110011001100 ; W.W.W.W.
	fdb %0000000000000000 ; ........

	* Skull by Pat Ferguson

	fdb %0011111111110000 ; .WWWWW.. 2 skull sprites 8x8
	fdb %1111111111111100 ; WWWWWWW.
	fdb %1101101101101100 ; WBRWBRW.
	fdb %1101011101011100 ; WBBWBBW.
	fdb %0011110111110000 ; .WWBWW..
	fdb %0001111111010000 ; .BWWWB..
	fdb %0011011101110000 ; .WBWBW..
	fdb %0000000000000000 ; ........

	fdb %0011111111110000 ; .WWWWW..
	fdb %1111111111111100 ; WWWWWWW.
	fdb %1110011110011100 ; WRBWRBW.
	fdb %1101011101011100 ; WBBWBBW.
	fdb %0011110111110000 ; .WWBWW..
	fdb %0001111111010000 ; .BWWWB..
	fdb %0011011101110000 ; .WBWBW..
	fdb %0000000000000000 ; ........


