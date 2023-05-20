; Hori Track trackball (https://www.nesdev.org/wiki/Hori_Track)
;
; The states for both switches on the bottom are sent in the third byte of
; data.  The Hi/Lo switch will change the values sent in the second byte so
; the software doesn't need to handle the different speed settings directly.
; The L/R switch on the other hand only sends its state in the data.  It does
; not change the orientation of the axes or the D-Pad, this must be done in
; software.
;
; L-mode seems to be the standard mode with UP on the D-Pad facing the ball
; and with the "Hori Track" text on the front facing the user.
;
; Axis data (byte 2):
; L-mode: YYYY XXXX
;     Y = Positive = Down = towards D-Pad
;     X = Positive = Right = towards Start/Select
; R-mode: XXXX YYYY
;     X = Positive = Left = towards D-Pad
;     Y = Positive = Down = towards Start/Select
;
; The L/R switch isn't handled by this code.

; Horizontal sprite positions for the number lines.  Starts at -8
NumberLinePositions:
    ; positive
    .repeat 8, i
    .byte 120+(8*i)
    .endrepeat

    ; negative
    .repeat 8, i
    .byte 56+(8*i)
    .endrepeat

TrackballSwVals:
    .asciiz "L R  Lo Hi"

TrackballErrorAddr = $2090
TrackballErrorText:
    .asciiz "Not plugged in"
TrackballErrorTextLen = * - TrackballErrorText

Init_Trackball:
    lda #%1000_0000
    sta PPU_2000
    sta $2000

    lda #0
    sta $2001

    lda #$20
    jsr ClearScreen

    ;; setup stuff
    lda #$C4
    sta AddressPointer+0
    lda #$20
    sta AddressPointer+1

    lda #.lobyte(ControllerTiles)
    sta AddressPointer2+0
    lda #.hibyte(ControllerTiles)
    sta AddressPointer2+1
    jsr DrawTiledRegion

    lda #$00
    sta SpriteZero+1
    sta SpriteZero+2
    lda #120
    sta SpriteZero+3
    lda #96
    sta SpriteZero+0

    ; "X" Axis number line
    lda #$21
    sta $2006
    lda #$87
    sta $2006
    ldx #$38
:
    stx $2007
    dex
    cpx #$30
    bne :-

    lda #$B0
    sta $2007
    inx
:
    stx $2007
    inx
    cpx #$38
    bne :-

    ; "Y" Axis number line
    lda #$22
    sta $2006
    lda #$07
    sta $2006
    ldx #$38
:
    stx $2007
    dex
    cpx #$30
    bne :-

    lda #$B0
    sta $2007
    inx
:
    stx $2007
    inx
    cpx #$38
    bne :-

    ; "X" Axis arrow
    lda #120
    sta Sprites+(1*4)+3
    ldx #$12
    stx Sprites+(1*4)+1
    ldy #0
    sty Sprites+(1*4)+2
    lda #87
    sta Sprites+(1*4)+0

    ; "Y" Axis arrow
    stx Sprites+(2*4)+1
    sty Sprites+(2*4)+2
    lda #119
    sta Sprites+(2*4)+0
    lda #120
    sta Sprites+(2*4)+3

    ; Switch values
    lda #$22
    sta $2006
    lda #$A8
    sta $2006
    ldx #0
:
    lda TrackballSwVals, x
    beq :+
    inx
    sta $2007
    jmp :-
:

    ; attributes
    lda #0
    sta Sprites+(3*4)+2
    sta Sprites+(4*4)+2

    ; tile
    lda #$12
    sta Sprites+(3*4)+1
    sta Sprites+(4*4)+1

    ; Y
    lda #159
    sta Sprites+(3*4)+0
    sta Sprites+(4*4)+0

    jsr WaitForNMI
    lda #%0001_1110
    sta $2001

Frame_Trackball:
    jsr ReadTrackball

    ; This should always have a value in D4,D5 (ID bits)
    ; If not, it's not plugged in.  Don't bother with
    ; updating the screen.
    lda controllers+2
    bne :+

    lda #$FF
    sta controllers+3
    jsr QueueTrackballError
    jsr WaitForNMI
    jmp Frame_Trackball
:

    lda controllers+3
    bpl :+
    ; Trackball wasn't plugged in, but is now.
    ; Clear the text from screen.
    lda #$00
    sta controllers+3
    jsr ClearTrackballError
:

    lda #0
    sta TmpX
@loop:
    rol controllers+0
    bcc :+
    lda #$80
    jmp :++
:   lda #$00

:   ldy TmpX
    ora ControllerTileLookup, y

    inc BufferIndex
    ldx BufferIndex
    sta Buffer_Data, x

    lda ControllerLookupLo, y
    sta Buffer_AddrLo, x
    lda ControllerLookupHi, y
    sta Buffer_AddrHi, x

    inc TmpX
    lda TmpX
    cmp #8
    bne @loop

    ; "Y" axis
    lda controllers+1
    lsr a
    lsr a
    lsr a
    lsr a
    beq :+
    tax
    lda HexAscii, x

    inc BufferIndex
    ldx BufferIndex
    sta Buffer_Data, x

    lda #$87
    sta Buffer_AddrLo, x
    lda #$20
    sta Buffer_AddrHi, x

    ; "X" axis
:   lda controllers+1
    and #$0F
    beq :+
    tax
    lda HexAscii, x

    inc BufferIndex
    ldx BufferIndex
    sta Buffer_Data, x

    lda #$88
    sta Buffer_AddrLo, x
    lda #$20
    sta Buffer_AddrHi, x

:   lda controllers+2
    lsr a
    lsr a
    lsr a
    lsr a
    tax
    lda HexAscii, x

    inc BufferIndex
    ldx BufferIndex
    sta Buffer_Data, x

    lda #$89
    sta Buffer_AddrLo, x
    lda #$20
    sta Buffer_AddrHi, x

    lda controllers+2
    and #$0F
    tax
    lda HexAscii, x

    inc BufferIndex
    ldx BufferIndex
    sta Buffer_Data, x

    lda #$8A
    sta Buffer_AddrLo, x
    lda #$20
    sta Buffer_AddrHi, x

    ; Trackball Sprite
    ; Axis 2 - Labeled Y on the board
    lda controllers+1
    and #$0F
    sta TmpX
    and #$08
    beq :+
    clc
    lda controllers+1
    and #$07
    ora #$F8
    adc SpriteZero+3
    sta SpriteZero+3
    jmp :++
:
    clc
    lda controllers+1
    and #$07
    adc SpriteZero+3
    sta SpriteZero+3
:

    ldx TmpX
    lda NumberLinePositions, x
    sta Sprites+(2*4)+3

    ; Axis 1 - Labeled X on the board
    lda controllers+1
    lsr a
    lsr a
    lsr a
    lsr a
    sta TmpX
    and #$08
    beq :+
    clc
    lda TmpX
    and #$07
    ora #$F8
    adc SpriteZero+0
    sta SpriteZero+0
    jmp :++
:
    clc
    lda TmpX
    and #$07
    adc SpriteZero+0
    sta SpriteZero+0
:

    ldx TmpX
    lda NumberLinePositions, x
    sta Sprites+(1*4)+3

    bit controllers+2
    bpl :+
    ; 7 set - Left set
    lda #64
    sta Sprites+(3*4)+3
    jmp :++
:
    ; Right set
    lda #80
    sta Sprites+(3*4)+3
:

    bvs :+
    ; Hi set
    lda #128
    sta Sprites+(4*4)+3
    jmp :++
:
    ; Lo set
    lda #104
    sta Sprites+(4*4)+3
:
    jsr WaitForNMI
    jmp Frame_Trackball

ReadTrackball:
    lda #1
    sta $4016
    lda #0
    sta $4016

    ldy #8
:
    lda $4016
    lsr a
    lsr a
    rol controllers
    dey
    bne :-

    ldy #8
:
    lda $4016
    lsr a
    lsr a
    rol controllers+1
    dey
    bne :-

    ldy #8
:
    lda $4016
    lsr a
    lsr a
    rol controllers+2
    dey
    bne :-

    lda controllers+1
    eor #$FF
    sta controllers+1
    rts

    lda controllers+2
    beq :+
    rts

:   ; Third byte is $00, trackball isn't plugged in.
    ; Clear axes values
    sta controllers+1
    rts

QueueTrackballError:
    lda #.lobyte(TrackballErrorAddr)
    sta AddressPointer+0
    lda #.hibyte(TrackballErrorAddr)
    sta AddressPointer+1

    ldy #0
@loop:
    lda TrackballErrorText, y
    beq @done
    inc BufferIndex
    ldx BufferIndex
    sta Buffer_Data, x

    lda AddressPointer+0
    sta Buffer_AddrLo, x

    lda AddressPointer+1
    sta Buffer_AddrHi, x

    inc AddressPointer+0
    bne :+
    inc AddressPointer+1
:

    iny
    jmp @loop

@done:
    rts

ClearTrackballError:
    lda #.lobyte(TrackballErrorAddr)
    sta AddressPointer+0
    lda #.hibyte(TrackballErrorAddr)
    sta AddressPointer+1

    ldy #TrackballErrorTextLen
@loop:
    lda #$20 ; space character
    inc BufferIndex
    ldx BufferIndex
    sta Buffer_Data, x

    lda AddressPointer+0
    sta Buffer_AddrLo, x

    lda AddressPointer+1
    sta Buffer_AddrHi, x

    inc AddressPointer+0
    bne :+
    inc AddressPointer+1
:

    dey
    bne @loop

    rts
