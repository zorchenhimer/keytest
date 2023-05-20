
Init_Controllers:
    lda #%1000_0000
    sta PPU_2000
    sta $2000

    lda #0
    sta $2001

    lda #$20
    jsr ClearScreen

    lda #.lobyte(ControllerTiles)
    sta AddressPointer2+0
    lda #.hibyte(ControllerTiles)
    sta AddressPointer2+1

    ldx #0
    ldy #0
@txtStart:
    lda ControllerLabelAddrs+1, x
    sta AddressPointer+1
    sta $2006
    lda ControllerLabelAddrs+0, x
    sta AddressPointer+0
    sta $2006

@txtLoop:
    lda ControllerLabels, y
    beq @next
    iny
    sta $2007
    jmp @txtLoop

@next:

    tya
    pha
    txa
    pha
    tya
    pha

    ; draw controller
    clc
    lda AddressPointer+0
    adc #$40
    sta AddressPointer+0
    lda AddressPointer+1
    adc #0
    sta AddressPointer+1
    jsr DrawTiledRegion

    pla
    tay
    pla
    pha
    tax

    pla
    tax
    pla
    tay

    iny
    inx
    inx
    cpx #7
    bcc @txtStart

.if (DEBUG_CONTROLLER_LAYOUT = TRUE)
    ; populate buttons for debugging
    ldx #0
    ldy #$A0 ; space on second table (grey square)
:
    lda ControllerLookupHi, x
    sta $2006
    lda ControllerLookupLo, x
    sta $2006
    sty $2007

    inx
    cpx #32
    bne :-
.endif

    jsr WaitForNMI
    lda #%0001_1110
    sta $2001

Frame_Controllers:
    jsr ReadControllers

    ldx #0
    jsr UpdateController

    ldx #1
    jsr UpdateController

    ldx #2
    jsr UpdateController

    ldx #3
    jsr UpdateController

    jsr WaitForNMI
    jmp Frame_Controllers

; ControllerID in X
UpdateController:
    lda controllers, x
    sta TmpY

    lda #0
    sta TmpX    ; loop counter
    stx TmpZ    ; ControllerID

@loop:
    rol TmpY
    bcc :+
    ; pressed
    lda #$80
    jmp :++
:   ; not pressed
    lda #$00

:   ldy TmpX ; button index
    ora ControllerTileLookup, y

    inc BufferIndex
    ldx BufferIndex
    sta Buffer_Data, x

    lda TmpZ
    asl a
    asl a
    asl a
    clc
    adc TmpX
    tay

    lda ControllerLookupLo, y
    sta Buffer_AddrLo, x
    lda ControllerLookupHi, y
    sta Buffer_AddrHi, x

    inc TmpX
    lda TmpX
    cmp #8
    bne @loop

    rts

ControllerTileLookup:
    ;     A    B    S    S    U    D    L    R
    .byte $1E, $1E, $1F, $1F, $10, $12, $13, $11

ControllerLabels:
    .asciiz "Player 1"
    .asciiz "Player 2"
    .asciiz "Player 3"
    .asciiz "Player 4"

Ctrl1_LabelAddr = $2084
Ctrl2_LabelAddr = $2093
Ctrl3_LabelAddr = $2184
Ctrl4_LabelAddr = $2193

ControllerLabelAddrs:
    .word Ctrl1_LabelAddr
    .word Ctrl2_LabelAddr
    .word Ctrl3_LabelAddr
    .word Ctrl4_LabelAddr

Ctrl1_Addr = Ctrl1_LabelAddr + $40
Ctrl2_Addr = Ctrl2_LabelAddr + $40
Ctrl3_Addr = Ctrl3_LabelAddr + $40
Ctrl4_Addr = Ctrl4_LabelAddr + $40

; PPU Address lookup tables for controller buttons
ControllerLookupLo:
    ; Controller 1
    .byte .lobyte(Ctrl1_Addr + (1*32+8))  ; A
    .byte .lobyte(Ctrl1_Addr + (1*32+7))  ; B
    .byte .lobyte(Ctrl1_Addr + (1*32+4))  ; Select
    .byte .lobyte(Ctrl1_Addr + (1*32+5))  ; Start
    .byte .lobyte(Ctrl1_Addr + (0*32+1))  ; Up
    .byte .lobyte(Ctrl1_Addr + (2*32+1))  ; Down
    .byte .lobyte(Ctrl1_Addr + (1*32+0))  ; Left
    .byte .lobyte(Ctrl1_Addr + (1*32+2))  ; Right

    ; Controller 2
    .byte .lobyte(Ctrl2_Addr + (1*32+8))  ; A
    .byte .lobyte(Ctrl2_Addr + (1*32+7))  ; B
    .byte .lobyte(Ctrl2_Addr + (1*32+4))  ; Select
    .byte .lobyte(Ctrl2_Addr + (1*32+5))  ; Start
    .byte .lobyte(Ctrl2_Addr + (0*32+1))  ; Up
    .byte .lobyte(Ctrl2_Addr + (2*32+1))  ; Down
    .byte .lobyte(Ctrl2_Addr + (1*32+0))  ; Left
    .byte .lobyte(Ctrl2_Addr + (1*32+2))  ; Right

    ; Controller 3
    .byte .lobyte(Ctrl3_Addr + (1*32+8))  ; A
    .byte .lobyte(Ctrl3_Addr + (1*32+7))  ; B
    .byte .lobyte(Ctrl3_Addr + (1*32+4))  ; Select
    .byte .lobyte(Ctrl3_Addr + (1*32+5))  ; Start
    .byte .lobyte(Ctrl3_Addr + (0*32+1))  ; Up
    .byte .lobyte(Ctrl3_Addr + (2*32+1))  ; Down
    .byte .lobyte(Ctrl3_Addr + (1*32+0))  ; Left
    .byte .lobyte(Ctrl3_Addr + (1*32+2))  ; Right

    ; Controller 4
    .byte .lobyte(Ctrl4_Addr + (1*32+8))  ; A
    .byte .lobyte(Ctrl4_Addr + (1*32+7))  ; B
    .byte .lobyte(Ctrl4_Addr + (1*32+4))  ; Select
    .byte .lobyte(Ctrl4_Addr + (1*32+5))  ; Start
    .byte .lobyte(Ctrl4_Addr + (0*32+1))  ; Up
    .byte .lobyte(Ctrl4_Addr + (2*32+1))  ; Down
    .byte .lobyte(Ctrl4_Addr + (1*32+0))  ; Left
    .byte .lobyte(Ctrl4_Addr + (1*32+2))  ; Right

ControllerLookupHi:
    ; Controller 1
    .byte .hibyte(Ctrl1_Addr + (1*32+8))  ; A
    .byte .hibyte(Ctrl1_Addr + (1*32+7))  ; B
    .byte .hibyte(Ctrl1_Addr + (1*32+4))  ; Select
    .byte .hibyte(Ctrl1_Addr + (1*32+5))  ; Start
    .byte .hibyte(Ctrl1_Addr + (0*32+1))  ; Up
    .byte .hibyte(Ctrl1_Addr + (2*32+1))  ; Down
    .byte .hibyte(Ctrl1_Addr + (1*32+0))  ; Left
    .byte .hibyte(Ctrl1_Addr + (1*32+2))  ; Right

    ; Controller 2
    .byte .hibyte(Ctrl2_Addr + (1*32+8))  ; A
    .byte .hibyte(Ctrl2_Addr + (1*32+7))  ; B
    .byte .hibyte(Ctrl2_Addr + (1*32+4))  ; Select
    .byte .hibyte(Ctrl2_Addr + (1*32+5))  ; Start
    .byte .hibyte(Ctrl2_Addr + (0*32+1))  ; Up
    .byte .hibyte(Ctrl2_Addr + (2*32+1))  ; Down
    .byte .hibyte(Ctrl2_Addr + (1*32+0))  ; Left
    .byte .hibyte(Ctrl2_Addr + (1*32+2))  ; Right

    ; Controller 3
    .byte .hibyte(Ctrl3_Addr + (1*32+8))  ; A
    .byte .hibyte(Ctrl3_Addr + (1*32+7))  ; B
    .byte .hibyte(Ctrl3_Addr + (1*32+4))  ; Select
    .byte .hibyte(Ctrl3_Addr + (1*32+5))  ; Start
    .byte .hibyte(Ctrl3_Addr + (0*32+1))  ; Up
    .byte .hibyte(Ctrl3_Addr + (2*32+1))  ; Down
    .byte .hibyte(Ctrl3_Addr + (1*32+0))  ; Left
    .byte .hibyte(Ctrl3_Addr + (1*32+2))  ; Right

    ; Controller 4
    .byte .hibyte(Ctrl4_Addr + (1*32+8))  ; A
    .byte .hibyte(Ctrl4_Addr + (1*32+7))  ; B
    .byte .hibyte(Ctrl4_Addr + (1*32+4))  ; Select
    .byte .hibyte(Ctrl4_Addr + (1*32+5))  ; Start
    .byte .hibyte(Ctrl4_Addr + (0*32+1))  ; Up
    .byte .hibyte(Ctrl4_Addr + (2*32+1))  ; Down
    .byte .hibyte(Ctrl4_Addr + (1*32+0))  ; Left
    .byte .hibyte(Ctrl4_Addr + (1*32+2))  ; Right

ControllerTiles:
    .include "controller.i"
