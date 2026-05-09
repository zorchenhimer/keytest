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

    macDrawText Network_Label, LabelAddr

    lda #.lobyte(NetworkTiles)
    sta AddressPointer2+0
    lda #.hibyte(NetworkTiles)
    sta AddressPointer2+1
    jsr DrawTiledRegion

    jsr WaitForNMI
    lda #%0001_1110
    sta $2001

Frame:
    jsr WaitForNMI
    jmp Frame

Network_Label:
    .asciiz "Famicom Network Controller"

NetworkTiles:
    .include "network.i"

.endscope
