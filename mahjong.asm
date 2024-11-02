Init_Mahjong:
    lda #%1001_0000
    sta PPU_2000
    sta $2000

    lda #0
    sta $2001

    lda #$20
    jsr ClearScreen

    lda #.hibyte(Mahjong_LabelAddr)
    sta AddressPointer+1
    sta $2006
    lda #.lobyte(Mahjong_LabelAddr)
    sta AddressPointer+0
    sta $2006

    ldy #0
@txtLoop:
    lda MahjongText, y
    beq @next
    iny
    sta $2007
    jmp @txtLoop
@next:

    clc
    lda AddressPointer+0
    adc #$40
    sta AddressPointer+0

    lda AddressPointer+1
    adc #0
    sta AddressPointer+1
    sta $2006
    lda AddressPointer+0
    sta $2006

    ldx #0
    ldy #' '
@btnLoop:
    lda MahjongButtons, x
    sta $2007
    sty $2007
    inx
    cpx #14
    bne @btnLoop

    lda #.hibyte(MahjongButtons2_Addr)
    sta $2006
    lda #.lobyte(MahjongButtons2_Addr)
    sta $2006

    ldx #0
@btnLoop2:
    lda MahjongButtons2, x
    sta $2007

    inx
    lda MahjongButtons2, x
    sta $2007

    inx
    cpx #14
    bne @btnLoop2

    lda #.hibyte(Mahjong_LabelAddr+$40)
    sta AddressPointer+1
    lda #.lobyte(Mahjong_LabelAddr+$40)
    sta AddressPointer+0

    ; Letter Row
    ldx #0
@addrLoop:
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

    inx
    cpx #14
    bne @addrLoop

    lda #.hibyte(MahjongButtons2_Addr)
    sta AddressPointer+1
    lda #.lobyte(MahjongButtons2_Addr)
    sta AddressPointer+0
@addrLoop2:
    lda AddressPointer+1
    sta Buffer_AddrHi, x
    lda AddressPointer+0
    sta Buffer_AddrLo, x

    clc
    adc #1
    sta AddressPointer+0
    lda AddressPointer+1
    adc #0
    sta AddressPointer+1

    inx
    cpx #28
    bne @addrLoop2

    jsr WaitForNMI
    lda #%0001_1110
    sta $2001

Frame_Mahjong:
    jsr ReadMahjongController

    lda #28
    sta BufferIndex

; A through H
    ldx #0
    lda controllers+2
    sta TmpX
@readLoopA:
    ror TmpX
    bcc :+
    ; pressed
    lda #$80
    jmp :++
:   ; not-pressed
    lda #$00
:

    ora MahjongButtons, x
    sta Buffer_Data, x
    inx
    cpx #8
    bne @readLoopA

; I through N
    lda controllers+1
    sta TmpX
@readLoopB:
    ror TmpX
    bcc :+
    ; pressed
    lda #$80
    jmp :++
:   ; not-pressed
    lda #$00
:

    ora MahjongButtons, x
    sta Buffer_Data, x
    inx
    cpx #14
    bne @readLoopB

; Select through Ron
    lda controllers+3
    sta TmpX
    ldy #0
@readLoopC:
    ror TmpX
    bcc :+
    ; pressed
    lda #$80
    jmp :++
:   ; not-pressed
    lda #$00
:

    ora MahjongButtons2, y
    sta Buffer_Data, x
    inx
    iny
    ora MahjongButtons2, y
    sta Buffer_Data, x
    iny
    inx

    cpx #28
    bne @readLoopC

    jsr WaitForNMI

    ; Some debug stuff
    ;lda #$21
    ;sta $2006
    ;lda #$84
    ;sta $2006

    ;lda controllers+0
    ;lsr a
    ;lsr a
    ;lsr a
    ;lsr a
    ;tax
    ;lda HexAscii, x
    ;sta $2007

    ;lda controllers+0
    ;and #$0F
    ;tax
    ;lda HexAscii, x
    ;sta $2007

    ;lda #$21
    ;sta $2006
    ;lda #$A4
    ;sta $2006

    ;lda controllers+1
    ;lsr a
    ;lsr a
    ;lsr a
    ;lsr a
    ;tax
    ;lda HexAscii, x
    ;sta $2007

    ;lda controllers+1
    ;and #$0F
    ;tax
    ;lda HexAscii, x
    ;sta $2007

    ;lda #$21
    ;sta $2006
    ;lda #$C4
    ;sta $2006

    ;lda controllers+2
    ;lsr a
    ;lsr a
    ;lsr a
    ;lsr a
    ;tax
    ;lda HexAscii, x
    ;sta $2007

    ;lda controllers+2
    ;and #$0F
    ;tax
    ;lda HexAscii, x
    ;sta $2007

    ;lda #$21
    ;sta $2006
    ;lda #$E4
    ;sta $2006

    ;lda controllers+3
    ;lsr a
    ;lsr a
    ;lsr a
    ;lsr a
    ;tax
    ;lda HexAscii, x
    ;sta $2007

    ;lda controllers+3
    ;and #$0F
    ;tax
    ;lda HexAscii, x
    ;sta $2007

    ;lda #0
    ;sta $2005
    ;sta $2005

    jmp Frame_Mahjong

ReadMahjongController:

    ldx #0
@rows:
    txa ; row select
    asl a
    ora #%0000_0001
    sta $4016
    txa ; row select
    asl a
    ora #%0000_0000
    sta $4016

    ldy #8
@rowLoop:
    lda $4017
    lsr a              ; Bit0 -> Carry
    lsr a              ; Bit1 -> Carry
    rol controllers, x ; Bit1 <- Carry
    dey
    bne @rowLoop
    inx
    cpx #4
    bne @rows

    rts

Mahjong_LabelAddr = $2083
MahjongText:
    .asciiz "Capcom Mahjong Controller"

MahjongButtons:
    .repeat 13, i
        .byte 'A'+i
    .endrepeat
    .byte 0 ; round N button

MahjongButtons2_Addr = $20ED
MahjongButtons2:
    .byte $02, $03 ; SEL
    .byte $04, $05 ; ST
    .byte $06, $07 ; カン
    .byte $08, $09 ; ポン
    .byte $0A, $0B ; チー
    .byte $0C, $0D ; リーチ
    .byte $0E, $0F ; ロン
