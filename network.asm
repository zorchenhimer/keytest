Network_LabelAddr = $2083

Init_Network:
    lda #%1000_0000
    sta PPU_2000
    sta $2000

    lda #0
    sta $2001

    lda #$20
    jsr ClearScreen

    lda #.hibyte(Network_LabelAddr)
    sta $2006
    lda #.lobyte(Network_LabelAddr)
    sta $2006
    clc
    adc #$80
    sta AddressPointer+0
    lda #.hibyte(Network_LabelAddr)
    adc #0
    sta AddressPointer+1

    ldy #0
@txtLoop:
    lda Network_Label, y
    beq @next
    iny
    sta $2007
    jmp @txtLoop
@next:

    lda #.lobyte(NetworkTiles)
    sta AddressPointer2+0
    lda #.hibyte(NetworkTiles)
    sta AddressPointer2+1
    jsr DrawTiledRegion

    jsr WaitForNMI
    lda #%0001_1110
    sta $2001

Frame_Network:
    jsr WaitForNMI
    jmp Frame_Network

Network_Label:
    .asciiz "Famicom Network Controller"

NetworkTiles:
    .include "network.i"
