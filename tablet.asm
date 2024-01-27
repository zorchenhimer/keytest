; Oeka Kids Tablet
;
; Data is read from the tablet one bit at a time
; from port $4017, MSB first:
; 8 bits for X
; 8 bits for Y
; 1 bit for pen on tablet
; 1 bit for button pressed
;
; The tablet must be strobed before each bit is read.
; After every strobe, the console must wait for the
; tablet to acknowledge the strobe.
;
; Read process (all writes to $4016 & reads from $4017)
;
; Write %0000_0000 to latch tablet
; Read waiting for %0000_0100 (ACK)
; Write %0000_0001 to start strobe (turn READ on)
; Read waiting for %0000_0000 (ACK)
; Write %0000_0011 to end strobe
; Read waiting for %0000_0100 (ACK)
; Read $4017, AND with %000_1000 for data (see below)
; Repeat from start strobe until 18 bits are read
; Write %0000_0001 to tablet
; EOR #$FF all data read from tablet.
;
; Reading from the tablet will not work when the ACKs
; are skipped.  This is basically the serial clock.
;
; A second read from $4017 isn't required if the data
; is saved from the ACK step then AND #%0000_1000
; afterwards.  Re-reading $4017 before the next strobe
; will not advance the data.

.pushseg
.segment "BSS"
tabletX:   .res 1
tabletY:   .res 1
tabletBtn: .res 1   ; lowest two bits are Pen and Button

.popseg

; "crosshair" sprites for the pen coordinates
TPTR_A = Sprites+(4*6)
TPTR_B = Sprites+(4*7)
TPTR_C = Sprites+(4*8)
TPTR_D = Sprites+(4*9)

tablet_text:
    .word :+
    .word :++
    .word :+++
    .word :++++

:   .asciiz "Coord X:"
:   .asciiz "Coord Y:"
:   .asciiz "Button:"
:   .asciiz "Pen:"

tablet_text_coord:
    .repeat 4, i
    .word $2084 + (64*i)
    .endrepeat

TabletPalettes:
    ; BG
    .byte $0F, $20, $2A, $00
    .byte $0F, $20, $2A, $00
    .byte $0F, $20, $2A, $00
    .byte $0F, $20, $2A, $00

    ; Sprite
    .byte $0F, $20, $2A, $00
    .byte $0F, $2A, $20, $00 ; crosshair sprites
    .byte $0F, $20, $2A, $00
    .byte $0F, $20, $2A, $00

; AddressPointer  points to text data
; AddressPointer2 PPU address to draw at
DrawText:
    txa
    asl a
    tax

    lda tablet_text_coord+1, x
    sta $2006
    lda tablet_text_coord+0, x
    sta $2006

    lda tablet_text+0, x
    sta AddressPointer+0
    lda tablet_text+1, x
    sta AddressPointer+1

    ldy #0
@loop:
    lda (AddressPointer), y
    beq @done
    sta $2007
    iny
    jmp @loop
@done:
    rts

tablet_UpdateSprites:
    lda tabletX
    clc
    adc #1
    sta TPTR_A+3
    sta TPTR_D+3

    sec
    sbc #10
    sta TPTR_B+3
    sta TPTR_C+3


    lda tabletY
    clc
    adc #1
    sta TPTR_A+0
    sta TPTR_B+0

    sec
    sbc #10
    sta TPTR_C+0
    sta TPTR_D+0

    lda tabletX
    lsr a
    lsr a
    lsr a
    lsr a
    tax
    lda HexAscii, x
    sta Sprites+(4*0)+1

    lda tabletX
    and #$0F
    tax
    lda HexAscii, x
    sta Sprites+(4*1)+1

    lda tabletY
    lsr a
    lsr a
    lsr a
    lsr a
    tax
    lda HexAscii, x
    sta Sprites+(4*2)+1

    lda tabletY
    and #$0F
    tax
    lda HexAscii, x
    sta Sprites+(4*3)+1

    lda tabletBtn
    and #$02
    beq :+
    lda #0
    jmp :++
:
    lda #$20
:   sta Sprites+(4*5)+1

    lda tabletBtn
    and #$01
    beq :+
    lda #0
    sta Sprites+(4*4)+1
    rts
:
    lda #$20
    sta Sprites+(4*4)+1
    rts

Init_Tablet:
    lda #%1000_0000
    sta PPU_2000
    sta $2000

    lda #0
    sta $2001

    lda #$3F
    sta $2006
    lda #$00
    sta $2006

    ldx #0
:
    lda TabletPalettes, x
    sta $2007
    inx
    cpx #32
    bne :-

    lda #$20
    jsr ClearScreen

    ldx #0
    jsr DrawText

    ldx #1
    jsr DrawText

    ldx #2
    jsr DrawText

    ldx #3
    jsr DrawText

    lda #31
    sta Sprites+(4*0)+0
    sta Sprites+(4*1)+0

    lda #47
    sta Sprites+(4*2)+0
    sta Sprites+(4*3)+0

    lda #104
    sta Sprites+(4*0)+3
    sta Sprites+(4*2)+3

    lda #112
    sta Sprites+(4*1)+3
    sta Sprites+(4*3)+3

    lda #63
    sta Sprites+(4*4)+0
    lda #104
    sta Sprites+(4*4)+3
    sta Sprites+(4*5)+3

    lda #79
    sta Sprites+(4*5)+0

    lda #0
    sta Sprites+(4*0)+2
    sta Sprites+(4*1)+2
    sta Sprites+(4*2)+2
    sta Sprites+(4*3)+2
    sta Sprites+(4*4)+2
    sta Sprites+(4*5)+2

    lda #'`'
    sta TPTR_A+1
    sta TPTR_B+1
    sta TPTR_C+1
    sta TPTR_D+1

    lda #%0000_0001
    sta TPTR_A+2
    lda #%0100_0001
    sta TPTR_B+2
    lda #%1100_0001
    sta TPTR_C+2
    lda #%1000_0001
    sta TPTR_D+2

    sta tabletX
    sta tabletY
    sta tabletBtn

    jsr WaitForNMI
    lda #%0001_1110
    sta $2001

Frame_Tablet:

    jsr ReadTablet

    jsr tablet_UpdateSprites
    jsr WaitForNMI
    jmp Frame_Tablet

; After sending a Zero strobe, wait for
; it to be acknowledged by the tablet.
; Sets carry on timout (ie, no tablet).
tablet_AckZero:
    ldy #0
@loop:
    iny
    beq @rip
    lda $4017
    and #$04
    bne @loop
    clc
    rts

@rip:
    sec
    rts

; After sending a One strobe, wait for
; it to be acknowledged by the tablet.
; Sets carry on timout (ie, no tablet).
tablet_AckOne:
    ldy #0
@loop:
    iny
    beq @rip
    lda $4017
    and #$04
    beq @loop
    clc
    rts

@rip:
    sec
    rts

ReadTablet:
    lda #$00
    sta $4016
    jsr tablet_AckZero
    bcs tablet_ExitEarly

    lda #$01
    sta $4016
    jsr tablet_AckOne
    bcs tablet_ExitEarly

    jsr tablet_Read8
    bcs tablet_ExitEarly
    lda TmpX
    eor #$FF
    sta tabletX

    jsr tablet_Read8
    bcs tablet_ExitEarly
    lda TmpX
    eor #$FF
    sta tabletY

    jsr tablet_Read2
    bcs tablet_ExitEarly
    lda TmpX
    eor #$FF
    sta tabletBtn

tablet_ExitEarly:
    lda #$01
    sta $4016
    rts

tablet_Read2:
    lda #0
    sta TmpX
    ldx #2
    jmp tablet_ReadStart

tablet_Read8:
    lda #0
    sta TmpX
    ldx #8

tablet_ReadStart:

@loop:
    lda #$03
    sta $4016
    jsr tablet_AckZero
    bcc :+
    rts

:   lda $4017
    and #$08
    cmp #$08
    rol TmpX

    lda #$01
    sta $4016
    jsr tablet_AckOne
    bcc :+
    rts

:   dex
    bne @loop
    rts
