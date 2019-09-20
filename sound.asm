; Sound handling routines.
;
; Enable the the MUX (turn sound on). Bit 3 of FF23 controls
; whether the sound output is enabled or not.
SNDON           lda PIA1.CB     ; get current PIA control state
                ora #8          ; set MUX enable bit
                sta PIA1.CB     ; set new state
                rts
; Disable the MUX (turn sound off)
SNDOFF          lda PIA1.CB     ; get current PIA control state
                anda #0xf7      ; turn off MUX enable bit
                sta PIA1.CB     ; set new state
                rts
; Set MUX (DAC destination and sound source); enter with the
; desired source in B. The MUX is set using bit 3 of FF01
; and bit 3 of FF03 with FF01 holding the LSB. This version is
; much longer than the version in the Color Basic ROM but it has
; the advantage that it doesn't clobber U and it should run slightly
; faster by virtue of using extended direct addressing instead of
; indexed addressing and it doesn't have an extra BSR.
SETMUX          lda PIA0.CA     ; get PIA control for bit 0
                anda #0xf7      ; clear the MUX bit
                lsrb            ; is this bit set?
                bcc SETMUX0     ; brif not
                ora #8          ; enable this bit in the MUX
SETMUX0         sta PIA0.CA     ; save new selection bit 0
                lda PIA0.CB     ; get PIA control for bit 1
                anda #0xf7      ; clear the MUX bit
                lsrb            ; is this bit set?
                bcc SETMUX1     ; brif not
                ora #8          ; enable this bit in the MUX
SETMUX1         sta PIA0.CB     ; set new selection bit 1
                rts
