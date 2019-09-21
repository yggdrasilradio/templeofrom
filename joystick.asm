; Joystick handling

; This routine reads all four joystick axes. It bails out as soon as it
; gets two identical reads in a row or after JOYATTEMPT tries. This clobbers
; A, B, and X. If it doesn't get two identical reads in a row, the value
; on the last read is used.
;
; This varies from the GETJOY routine in the ROM as follows:
; * it uses PSHS and PULS
; * it doesn't clobber U if SETMUX is not pointing to the ROM routine
; * it will bail out as soon as it gets two identical reads in a row. The ROM
;   routine will try 10 times to get a read that matches the result from the
;   *last call* to GETJOY. That means if the joystick moved, it will always do
;   all 10 reads.
JOYATTEMPT      equ 10          ; number of times to try reading an axis
GETJOY          jsr SNDOFF      ; turn off sound (we mess with the MUX here)
                ldx #POTVAL+4   ; point to the end of the axis data
                ldb #3          ; there are four axes to read - 3 is highest axis number
GETJOY0         lda #JOYATTEMPT ; get retry count
                pshs d          ; save retry count and axis number
                jsr SETMUX      ; select the desired axis
GETJOY1         ldd #0x4080     ; initialize trial result and bit value to add/subtract
GETJOY2         pshs a          ; save bit value
                orb #2          ; set RS232 to marking
                stb PIA1.DA     ; set DAC output to trival value
                eorb #2         ; reset RS232 bit
                lda PIA0.DA     ; get comparator value
                bmi GETJOY3     ; brif axis value is higher than trial value
                subb ,s         ; subtract bit value from trial value
                bra GETJOY4
GETJOY3         addb ,s         ; add bit value to the trial value
GETJOY4         puls a          ; get back bit value
                lsra            ; shift bit value down one
                cmpa #1         ; done all bits?
                bne GETJOY2     ; brif not
                lsrb            ; normalize value into low 6 bits
                lsrb
                cmpb -1,x       ; did it match the last read?
                beq GETJOY5     ; brif so - we're done with this axis
                stb -1,x        ; save this value
                dec ,s          ; have we done all retries?
                bne GETJOY1     ; brif not - try again
GETJOY5         puls d          ; get back attempt counter and axis number
	leax -1,x
                decb            ; done all axes?
                bpl GETJOY0     ; brif not - do next one
                rts
