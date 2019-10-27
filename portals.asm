; Portal list; each entry consists of:
; X coordinate (16 bits)
; Y coordinate (16 bits)
; 4 sets of destination coordinates (in packed divided by four format)
;   one of which is chosen at "random" when a portal is activated
LDF2A	fcb $01,$b0,$01,$54			; PORTAL 1 6c55 432,340 this is the one visible on startup
	fcb $0b,$0b,$ba,$09,$13,$48,$aa,$89	; 	PORTAL 2 PORTAL 3 PORTAL 4 PORTAL 5
	fcb $00,$2c,$00,$2c			; PORTAL 2 0b0b 44,44
	fcb $6c,$55,$ba,$09,$13,$48,$aa,$89	;	PORTAL 1 PORTAL 3 PORTAL 4 PORTAL 5 
	fcb $02,$e8,$00,$24			; PORTAL 3 ba09 744,36
	fcb $0b,$0b,$6c,$55,$13,$48,$aa,$89	;	PORTAL 2 PORTAL 1 PORTAL 4 PORTAL 5 
	fcb $00,$4c,$01,$20			; PORTAL 4 1348 76,288
	fcb $0b,$0b,$ba,$09,$6c,$55,$aa,$89	;	PORTAL 2 PORTAL 3 PORTAL 1 PORTAL 5 
	fcb $02,$a8,$02,$24			; PORTAL 5 aa89 680,548
	fcb $0b,$0b,$ba,$09,$13,$48,$6c,$55	;	PORTAL 2 PORTAL 3 PORTAL 4 PORTAL 1 
	fdb 0			; flag end of table
