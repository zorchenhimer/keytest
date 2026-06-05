.scope Arkanoid

.pushseg
.segment "BSS"

knob: .res 2
hexBuffer: .res 5

.popseg

TitleText:
    .asciiz "Arkanoid Controller"

ButtonAddr = $20AD
KnobAddr   = ButtonAddr+32

textButton: .asciiz "Button:"
textKnob:   .asciiz "Knob:"

textButtonOn:  .byte $1E, $00
textButtonOff: .byte $20, $00

Init:
    lda #%1000_0000
    sta PPU_2000
    sta $2000

    lda #0
    sta $2001

    lda #$FF
    sta BufferIndex

    lda #$20
    jsr ClearScreen

    macDrawText_Direct TitleText,  $2065
    macDrawText_Direct textButton, $2065+64
    macDrawText_Direct textKnob,   $2065+64+32

    lda #%0001_1110
    sta $2001
    jsr WaitForNMI

Frame:
    jsr ReadController

    lda knob+1
    lsr a
    lsr a
    lsr a
    lsr a
    tax
    lda HexAscii, x
    sta hexBuffer+0

    lda knob+1
    and #$0F
    tax
    lda HexAscii, x
    sta hexBuffer+1

    lda knob+0
    lsr a
    lsr a
    lsr a
    lsr a
    tax
    lda HexAscii, x
    sta hexBuffer+2

    lda knob+0
    and #$0F
    tax
    lda HexAscii, x
    sta hexBuffer+3

    lda #0
    sta hexBuffer+4

    macDrawText hexBuffer, KnobAddr

    jsr WaitForNMI
    jmp Frame

ReadController:
    lda #0
    sta $4016
    lda #1
    sta $4016
    lda #0
    sta $4016

    ; Read button
    lda $4016
    and #%0000_0010
    bne :+
    macDrawText textButtonOff, ButtonAddr
    jmp :++
:
    macDrawText textButtonOn, ButtonAddr
:

    lda #0
    sta knob+0
    sta knob+1

    ldx #9
@loop:
    lda $4017
    lsr a
    lsr a
    rol knob+0
    rol knob+1
    dex
    bne @loop

    rts

.endscope
