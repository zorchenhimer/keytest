.scope Turbo

.pushseg
.segment "BSS"
Filename: .res 16

.popseg

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

    macDrawText textA, $2065
    macDrawText textB, $2065+32

    ldx #.addrsize(Buffer_Data)-1
    lda #0
@loop:
    sta Buffer_Data, y
    dex
    bpl @loop

    jsr WaitForNMI
    lda #%0001_1110
    sta $2001

    lda #$00
    sta AddressPointer3+0
    sta AddressPointer3+1

    jsr ReadFile

    jsr ResetAddr

Frame:
    jsr ReadControllers
    lda controllers_pressed+0
    and #BUTTON_A
    beq :+
    jsr ReadData
    jmp @frameDone

:   lda controllers_pressed+0
    and #BUTTON_B
    beq :+
    jsr ResetAddr
    jmp @frameDone

:   lda controllers_pressed+0
    and #BUTTON_START
    beq :+
    jsr WriteTest
    jmp @frameDone
:

@frameDone:
    jsr WaitForNMI
    jmp Frame

ResetAddr:
    lda #$00
    sta $4016
    lda #$02
    sta $4016

    lda #$00
    sta AddressPointer3+0
    sta AddressPointer3+1

    jsr AddrToAscii

    jsr WaitForNMI
    macDrawText Buffer_AddrLo, $20C5

    rts

ReadData:
;    lda #$00
;    sta $4016
;    lda #$02
;    sta $4016
;
;    ; dummy reads up to current address
;    lda AddressPointer3+0
;    sta AddressPointer+0
;    lda AddressPointer3+1
;    sta AddressPointer+1
;
;@dummy:
;    lda AddressPointer+0
;    bne :+
;    lda AddressPointer+1
;    beq @dummyDone
;:
;    jsr ReadByte
;
;    sec
;    lda AddressPointer+0
;    sbc #1
;    sta AddressPointer+0
;    lda AddressPointer+1
;    sbc #0
;    sta AddressPointer+1
;
;    jmp @dummy
;
;@dummyDone:

    lda #$00
    sta TmpX

    ldy #0
@loop:
    jsr ReadByte
    sta Buffer_Data, y
    iny
    cpy #16
    bne @loop

    jsr DataToAscii

    jsr WaitForNMI
    macDrawText Buffer_Data, $2105

    clc
    lda AddressPointer2+0
    adc #32
    sta AddressPointer2+0
    lda AddressPointer2+1
    adc #0
    sta AddressPointer2+1

    clc
    lda AddressPointer+0
    adc #17
    sta AddressPointer+0
    lda AddressPointer+1
    adc #0
    sta AddressPointer+1
    jsr DrawText

    jsr AddrToAscii
    macDrawText Buffer_AddrLo, $20C5

    clc
    lda AddressPointer3+0
    adc #16
    sta AddressPointer3+0
    lda AddressPointer3+1
    adc #0
    sta AddressPointer3+1

    rts

AddrToAscii:
    lda AddressPointer3+1
    lsr a
    lsr a
    lsr a
    lsr a
    tax
    lda HexAscii, x
    sta Buffer_AddrLo+0

    lda AddressPointer3+1
    and #$0F
    tax
    lda HexAscii, x
    sta Buffer_AddrLo+1

    lda AddressPointer3+0
    lsr a
    lsr a
    lsr a
    lsr a
    tax
    lda HexAscii, x
    sta Buffer_AddrLo+2

    lda AddressPointer3+0
    and #$0F
    tax
    lda HexAscii, x
    sta Buffer_AddrLo+3

    lda #00
    sta Buffer_AddrLo+4
    jmp WaitForNMI
    ;rts

ReadByte:
    .repeat 8
    lda $4017 ; read
    and #$04
    lsr a
    lsr a
    ora #$06
    sta $4016

    and #%1111_1011
    sta $4016

    lsr a
    ror TmpX  ; store
    .endrepeat

    lda TmpX
    rts

DataToAscii:
    ldy #0
@loop:
    lda Buffer_Data, y
    lsr a
    lsr a
    lsr a
    lsr a
    and #$0F
    tax
    lda HexAscii, x
    sta Buffer_Data+32, y

    lda Buffer_Data, y
    and #$0F
    tax
    lda HexAscii, x
    sta Buffer_Data+48, y
    iny
    cpy #16
    bne @loop

    ldy #0
    ldx #0
@loop2:
    lda Buffer_Data+32, y
    sta Buffer_Data, x
    inx
    lda Buffer_Data+48, y
    sta Buffer_Data, x
    inx
    iny
    cpy #8
    bne :+
    lda #0
    sta Buffer_Data, x
    inx
:
    cpy #16
    bne @loop2

    lda #0
    sta Buffer_Data, x

    rts

WriteTest:
    ; reset address
    lda #$00
    sta $4016
    lda #$02
    sta $4016

    ; get past first address
    ldx #8
    ldy #$06
    lda #$02
:
    sty $4016
    sta $4016
    dex
    bne :-

    ldy #0
@loop:
    lda TestData, y
    beq @done
    jsr WriteByte
    iny
    jmp @loop
@done:

    jsr ResetAddr
    jsr ReadData

    rts

WriteByte:
    sta TmpX

    ldx #8
@loop:
    lda TmpX
    and #$01
    lsr TmpX

    ora #$06
    sta $4016
    and #%1111_1011
    sta $4016

    dex
    bne @loop
    rts

ReadFile:
    jsr ResetAddr
    jsr ReadByte ; addr $0000

    jsr ReadByte
    cmp #'A'
    beq :+
    rts
:
    jsr ReadByte
    cmp #'B'
    beq :+
    rts
:

    jsr ReadByte
    sta AddressPointer3+0
    jsr ReadByte
    sta AddressPointer3+1
    jsr AddrToAscii

    ldx #0
@loop:
    jsr ReadByte
    beq @done
    cmp #$01
    beq @done

    sta Buffer_Data, x
    inx
    cpx #16
    bne @loop
@done:

    lda #0
    sta Buffer_Data, x

    jsr WaitForNMI
    macDrawText Buffer_Data, $2205

    jsr WaitForNMI
    macDrawText Buffer_AddrLo, $21E5

    rts

textA:
    .asciiz "Press A to read"
textB:
    .asciiz "Press B to reset"

TestData:
    .asciiz "01234567"

.endscope
