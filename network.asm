.scope Network
LabelAddr = $2083

Init:
    lda #%1000_0000
    sta PPU_2000
    sta $2000

    lda #0
    sta $2001

    lda #$20
    jsr ClearScreen

    macDrawText_Direct Network_Label, LabelAddr

    lda #.lobyte($2106)
    sta AddressPointer+0
    lda #.hibyte($2106)
    sta AddressPointer+1

    lda #.lobyte(NetworkTiles)
    sta AddressPointer2+0
    lda #.hibyte(NetworkTiles)
    sta AddressPointer2+1
    jsr DrawTiledRegion

    lda #%1000_0000
    sta PPU_2000
    sta $2000

    jsr WaitForNMI
    lda #%0001_1110
    sta $2001

Frame:
    jsr ReadNetwork
    jsr UpdateButtons

    lda #NMI_Action::UnrolledBytes
    sta NMIAction
    jsr WaitForNMI
    jmp Frame

UpdateButtons:

    ldx #0
@loop:
    lda #0
    rol controllers+0
    rol controllers+1
    rol controllers+2
    bcc :+
    lda #$80
:
    ora ButtonTiles, x
    sta Buffer_Data, x

    txa
    asl a
    tay
    lda ButtonMap+0, y
    sta Buffer_AddrLo, x
    lda ButtonMap+1, y
    sta Buffer_AddrHi, x

    inx
    cpx #TileCount
    bne @loop

    lda #24
    sta BufferIndex

    rts

ReadNetwork:
    lda #%0000_0011
    sta $4016
    lda #%0000_0010
    sta $4016

    lda #0
    sta controllers+0
    sta controllers+1
    sta controllers+2

    ldx #0
    ldy #24
@player1:
    lda $4016
    lsr a
    lsr a
    rol controllers+0, x
    rol controllers+1, x
    rol controllers+2, x
    dey
    bne @player1

    rts

ButtonMap:
    .word $2178 ; A
    .word $2176 ; B
    .word $2106 ; Select
    .word $2108 ; Start
    .word $2147 ; Up
    .word $2187 ; Down
    .word $2166 ; Left
    .word $2168 ; Right

    .word $2192 ; 0
    .word $210B ; 1
    .word $210D ; 2
    .word $210F ; 3
    .word $214B ; 4
    .word $214D ; 5
    .word $214F ; 6
    .word $218B ; 7

    .word $218D ; 8
    .word $218F ; 9
    .word $2111 ; *
    .word $2151 ; #
    .word $2153 ; .
    .word $2113 ; C
    .word $0000 ; nil
    .word $2137 ; 通信終了

ButtonTiles:
    .byte $00 ; A
    .byte $00 ; B
    .byte $1F ; Start
    .byte $1F ; Select
    .byte $10 ; Up
    .byte $12 ; Down
    .byte $13 ; Left
    .byte $11 ; Right

    .byte "0"
    .byte "1"
    .byte "2"
    .byte "3"
    .byte "4"
    .byte "5"
    .byte "6"
    .byte "7"

    .byte "8"
    .byte "9"
    .byte "*"
    .byte "#"
    .byte "."
    .byte "C"
    .byte " "
    .byte $1F

TileCount = * - ButtonTiles

Network_Label:
    .asciiz "Famicom Network Controller"

NetworkTiles:
    .include "network.i"

.endscope
