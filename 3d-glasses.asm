Init_3DGlasses:
    lda #%1000_0000
    sta PPU_2000
    sta $2000

    lda #0
    sta $2001

    lda #$20
    jsr ClearScreen
    lda #$24
    jsr ClearScreen

    lda #$21
    sta AddressPointer+1
    lda #$0C
    sta AddressPointer+0
    lda #.lobyte(GlassesLeftTiles)
    sta AddressPointer2+0
    lda #.hibyte(GlassesLeftTiles)
    sta AddressPointer2+1
    jsr DrawTiledRegion

    lda #$25
    sta AddressPointer+1
    lda #$0C
    sta AddressPointer+0
    lda #.lobyte(GlassesRightTiles)
    sta AddressPointer2+0
    lda #.hibyte(GlassesRightTiles)
    sta AddressPointer2+1
    jsr DrawTiledRegion

    lda #0
    sta GlassesToggle

    jsr WaitForNMI
    lda #%0001_1110
    sta $2001

Frame_3DGlasses:

    lda GlassesToggle
    beq :+
    ; Left
    ldx #%1000_0000
    stx PPU_2000
    ldx #%0000_0010
    stx $4016
    jmp :++

:   ; Right
    ldx #%1000_0001
    stx PPU_2000
    ldx #%0000_0000
    stx $4016

:   eor #$FF
    sta GlassesToggle

    jsr WaitForNMI
    jmp Frame_3DGlasses

GlassesLeftTiles:
    .include "glasses-left.i"
GlassesRightTiles:
    .include "glasses-right.i"
