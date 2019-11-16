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
	fcb $61,$78,$9d,$aa,$00 ; NEW spider in room just above next room
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

	fdb %0000111111110000 ; ..WWWW.. 2 ghost sprites 8x8
	fdb %0011111111111100 ; .WWWWWW.
	fdb %1101111111011111 ; WBWWWBWW
	fdb %1110011101101111 ; WRBWBRWW
	fdb %1110101110101111 ; WRRWRRWW
	fdb %1111111111111111 ; WWWWWWWW
	fdb %1111111111111111 ; WWWWWWWW
	fdb %0011110000111100 ; .WW..WW.

	fdb %0000111111110000 ; ..WWWW..
	fdb %0011111111111100 ; .WWWWWW.
	fdb %1101111111011111 ; WBWWWBWW
	fdb %1110011101101111 ; WRBWBRWW
	fdb %1110101110101111 ; WRRWRRWW
	fdb %1111111111111111 ; WWWWWWWW
	fdb %1111111111111111 ; WWWWWWWW
	fdb %1100001111000011 ; W..WW..W

	* Skull by Pat Ferguson

	fdb %0011111111111100 ; .WWWWWW.
	fdb %1111111111111111 ; WWWWWWWW
	fdb %1111111111111111 ; WWWWWWWW
	fdb %1111011011011011 ; WWBRWBRW
	fdb %1111010111010111 ; WWBBWBBW
	fdb %0011111101111100 ; .WWWBWW.
	fdb %0000011111110100 ; ..BWWWB.
	fdb %0000110111011100 ; ..WBWBW.

	fdb %0011111111111100 ; .WWWWWW.
	fdb %1111111111111111 ; WWWWWWWW
	fdb %1111111111111111 ; WWWWWWWW
	fdb %1110011110011111 ; WRBWRBWW
	fdb %1101011101011111 ; WBBWBBWW
	fdb %0011110111111100 ; .WWBWWW.
	fdb %0001111111010000 ; .BWWWB..
	fdb %0011011101110000 ; .WBWBW..
