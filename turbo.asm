.scope Turbo

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
    lda TmpX
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

;ReadControllers:
;    ; Freeze input
;    lda #%0000_0011
;    sta $4016
;    lda #%0000_0010
;    sta $4016
;
;    ldx #0
;@oldloop:
;    lda controllers, x
;    sta controllers_old, x
;    inx
;    cpx #6 ; FIXME: make this value a constant
;    bne @oldloop
;
;    ldx #0
;    ldy #8
;@player1:
;    lda $4016
;    lsr A              ; Bit0 -> Carry
;    rol controllers, x ; Bit0 <- Carry
;    ; famicom expanion controller
;    lsr A
;    rol controllers+4, x
;    dey
;    bne @player1
;
;    ldx #2
;    ldy #8
;@player3:
;    lda $4016
;    lsr A              ; Bit0 -> Carry
;    rol controllers, x ; Bit0 <- Carry
;    dey
;    bne @player3
;
;    ldx #1
;    ldy #8
;@player2:
;    lda $4017
;    lsr A              ; Bit0 -> Carry
;    rol controllers, x ; Bit0 <- Carry
;    ; famicom expanion controller
;    lsr A
;    rol controllers+4, x
;    dey
;    bne @player2
;
;    ldx #3
;    ldy #8
;@player4:
;    lda $4017
;    lsr A              ; Bit0 -> Carry
;    rol controllers, x ; Bit0 <- Carry
;    dey
;    bne @player4
;
;    ; If a controller has all buttons pressed, it
;    ; doesn't exist.  This will happen if the
;    ; four score isn't plugged in.
;    ldx #0
;@pressedLoop:
;    lda controllers, x
;    eor #$FF
;    bne :+
;    ; no controller here
;    lda #0
;    sta controllers, x
;    sta controllers_old, x
;    sta controllers_pressed, x
;    sta controllers_released, x
;    jmp :++
;
;:   lda controllers, x
;    eor controllers_old, x
;    and controllers, x
;    sta controllers_pressed, x
;
;    lda controllers, x
;    eor controllers_old, x
;    and controllers_old, x
;    sta controllers_released, x
;:
;    inx
;    cpx #6
;    bne @pressedLoop
;
;    rts

textA:
    .asciiz "Press A to read"
textB:
    .asciiz "Press B to reset"

TestData:
    .asciiz "01234567"

.endscope
