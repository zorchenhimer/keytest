
Init_Hyper:
    lda #%1000_0000
    sta PPU_2000
    sta $2000

    lda #0
    sta $2001

    lda #$20
    jsr ClearScreen

    ldx #0
    ldy #0
@txtStart:
    lda HyperLabelAddrs+1, x
    sta AddressPointer+1
    sta $2006
    lda HyperLabelAddrs+0, x
    sta AddressPointer+0
    sta $2006

@txtLoop:
    lda HyperLabels, y
    beq @next
    iny
    sta $2007
    jmp @txtLoop

@next:
    tya
    pha
    txa
    pha

    ; draw controller
    clc
    lda AddressPointer+0
    adc #$40
    sta AddressPointer+0
    lda AddressPointer+1
    adc #0
    sta AddressPointer+1

    lda AddressPointer+1
    sta $2006
    lda AddressPointer+0
    sta $2006

    ldx #' '
    lda #0
    sta $2007
    stx $2007
    sta $2007

    ; Setup button addresses in buffer
    pla
    pha
    ;lsr a
    tax

    ; Run button
    lda AddressPointer+1
    sta Buffer_AddrHi, x
    lda AddressPointer+0
    sta Buffer_AddrLo, x

    clc
    adc #2
    sta AddressPointer+0
    lda AddressPointer+1
    adc #0
    sta AddressPointer+1

    ; Jump button
    inx
    lda AddressPointer+1
    sta Buffer_AddrHi, x
    lda AddressPointer+0
    sta Buffer_AddrLo, x

    pla
    tax
    pla
    tay

    iny
    inx
    inx
    cpx #3
    bcc @txtStart

    jsr WaitForNMI
    lda #%0001_1110
    sta $2001

Frame_Hyper:
    jsr Hyper_ReadControllers

    ;ldx #0
@loop:
    inc BufferIndex
    ldx BufferIndex
    lda controllers, x
    beq :+
    ; pressed
    lda #$80
    jmp :++
:   ; not pressed
    lda #$00
:
    sta Buffer_Data, x
    inx
    cpx #4
    bne @loop

    jsr WaitForNMI
    jmp Frame_Hyper

Hyper_ReadControllers:
    lda #0
    sta controllers+0
    sta controllers+1
    sta controllers+2
    sta controllers+3

    sta $4016
    lda $4017
    lsr a ; nothin

    lsr a ; P1 Run
    rol controllers+0

    lsr a ; P1 Jump
    rol controllers+1

    lsr a ; P2 Run
    rol controllers+2

    lsr a ; P2 Jump
    rol controllers+3
    rts

HyperLabels:
    .asciiz "Player 1"
    .asciiz "Player 2"

Hyper1_LabelAddr = $2084
Hyper2_LabelAddr = $2093

HyperLabelAddrs:
    .word Hyper1_LabelAddr
    .word Hyper2_LabelAddr
