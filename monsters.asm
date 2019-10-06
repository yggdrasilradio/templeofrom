; Monster location table (19 x 5 + 2 = 97 bytes)
; X1, X2, Y1, Y2, monster ID (00 = spider, 20 = fireball)
; (X1, Y1) and (X2, Y2) are the corners of a box
; The monster's home position is 1/4 down from the top edge of the box
; and 1/4 from the left edge of the box
; Players entering the box aggro the monster
; Coordinates are 16 bits divided by 4 and stored in 8 bits

; x 385 to 477, $60 to $77 +1 = $61, $78 spider at center bottom of maze
; y 697 to 749, $ae to $bb +2 = $b0, $bd

; x 385 to 477, $60 to $77 +1 = $61, $78 new spider
; y 621 to 673, $9B to $a8 +2 = $9d, $aa

LDD23
	fcb $57,$65,$63,$75,$00
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
	;fcb $61,$78,$9d,$aa,$00 ; <-- NEW spider in room just above next room
	fcb $61,$78,$b0,$bd,$00 ; <-- spider at center bottom of maze
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
