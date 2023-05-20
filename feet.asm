
FEET_COL0 = %0001_0000
FEET_COL1 = %0000_1000
FEET_COL2 = %0000_0100
FEET_COL3 = %0000_0010

FEET_PPU_ADDR_A = $20C6
FEET_PPU_ADDR_B = $20D3

Init_Feet:
    lda #%1000_0000
    sta PPU_2000
    sta $2000

    lda #0
    sta $2001

    lda #$20
    jsr ClearScreen

    ; Draw the controler on screen
    lda #.hibyte(FEET_PPU_ADDR_A)
    sta AddressPointer+1
    lda #.lobyte(FEET_PPU_ADDR_A)
    sta AddressPointer+0


    lda #.lobyte(FamilyTrainerTiles_SideA)
    sta AddressPointer2+0
    lda #.hibyte(FamilyTrainerTiles_SideA)
    sta AddressPointer2+1

    jsr DrawFeetController

    lda #.hibyte(FEET_PPU_ADDR_B)
    sta AddressPointer+1
    lda #.lobyte(FEET_PPU_ADDR_B)
    sta AddressPointer+0

    lda #.lobyte(FamilyTrainerTiles_SideB)
    sta AddressPointer2+0
    lda #.hibyte(FamilyTrainerTiles_SideB)
    sta AddressPointer2+1

    jsr DrawFeetController

.if (DEBUG_FEET_LAYOUT = TRUE)
    ldx #0
    ldy #$A0
:
    lda FeetLookupAHi, x
    sta $2006
    lda FeetLookupALo, x
    sta $2006
    sty $2007

    inx
    cpx #12
    bne :-

    ldx #0
:
    lda FeetLookupBHi, x
    sta $2006
    lda FeetLookupBLo, x
    sta $2006
    sty $2007

    inx
    cpx #12
    bne :-
.endif

    ; In my testing, 39 loops seemed sufficient
    ; (~758 cycles).  Round up for good measure.
    lda #40
    sta FTWaitVal

    jsr WaitForNMI
    lda #%0001_1110
    sta $2001

Frame_Feet:
    jsr ReadFamilyTrainer

    ldy #0
    ;sta TmpX
@loop:
    rol FeetInput+1
    rol FeetInput+0
    bcc :+
    ; pressed
    lda #$80
    jmp :++
:   ; not pressed
    lda #$00
:
    ;ldy TmpX
    ora FeetTileLookupB, y

    inc BufferIndex
    ldx BufferIndex
    sta Buffer_Data, x

    lda FeetLookupBLo, y
    sta Buffer_AddrLo, x
    lda FeetLookupBHi, y
    sta Buffer_AddrHi, x

    bcc :+
    lda #$80
    jmp :++
:
    lda #$00
:
    ora FeetTileLookupA, y

    inc BufferIndex
    ldx BufferIndex
    sta Buffer_Data, x

    lda FeetLookupALo, y
    sta Buffer_AddrLo, x
    lda FeetLookupAHi, y
    sta Buffer_AddrHi, x

    iny
    ;lda TmpX
    cpy #12
    bne @loop

    ; Allow variable delay between the write to
    ; $4016 and the read from $4017.  Left/B
    ; lowers value, Right/A raises value.  Value
    ; is loop itterations, not cycles.  There are
    ; 19 cycles per loop, plus a bunch for
    ; before/after.
    jsr ReadControllers
    lda #BUTTON_A
    and controllers_pressed
    beq :+
    inc FTWaitVal
:

    lda #BUTTON_LEFT
    and controllers_pressed
    beq :+
    dec FTWaitVal
:

    lda #BUTTON_RIGHT
    and controllers_pressed
    beq :+
    inc FTWaitVal
:

    lda #BUTTON_B
    and controllers_pressed
    beq :+
    dec FTWaitVal
:

    lda FTWaitVal
    and #$0F
    tax
    lda HexAscii, x

    inc BufferIndex
    ldx BufferIndex
    sta Buffer_Data, x

    ldy #$20
    sty Buffer_AddrHi, x

    lda #$70
    sta Buffer_AddrLo, x

    lda FTWaitVal
    lsr a
    lsr a
    lsr a
    lsr a
    tax
    lda HexAscii, x

    inc BufferIndex
    ldx BufferIndex
    sta Buffer_Data, x
    sty Buffer_AddrHi, x
    lda #$6F
    sta Buffer_AddrLo, x

    jsr WaitForNMI
    jmp Frame_Feet

ReadFamilyTrainer:
    lda #0
    sta FeetInput+0
    sta FeetInput+1

    ; Read top row first
    lda #%0000_0011
    sta $4016
    jsr FamilyTrainerWait

    lda $4017
    sta TmpX
    and #FEET_COL0
    bne :+
    ; pressed
    lda #%1000_0000
    ora FeetInput+0
    sta FeetInput+0
:

    lda TmpX
    and #FEET_COL1
    bne :+
    ; pressed
    lda #%0100_0000
    ora FeetInput+0
    sta FeetInput+0
:

    lda TmpX
    and #FEET_COL2
    bne :+
    ; pressed
    lda #%0010_0000
    ora FeetInput+0
    sta FeetInput+0
:

    lda TmpX
    and #FEET_COL3
    bne :+
    ; pressed
    lda #%0001_0000
    ora FeetInput+0
    sta FeetInput+0
:

    ; Read middle row next
    lda #%0000_0101
    sta $4016
    jsr FamilyTrainerWait

    lda $4017
    sta TmpX
    and #FEET_COL0
    bne :+
    ; pressed
    lda #%0000_1000
    ora FeetInput+0
    sta FeetInput+0
:

    lda TmpX
    and #FEET_COL1
    bne :+
    ; pressed
    lda #%0000_0100
    ora FeetInput+0
    sta FeetInput+0
:

    lda TmpX
    and #FEET_COL2
    bne :+
    ; pressed
    lda #%0000_0010
    ora FeetInput+0
    sta FeetInput+0
:

    lda TmpX
    and #FEET_COL3
    bne :+
    ; pressed
    lda #%0000_0001
    ora FeetInput+0
    sta FeetInput+0
:

    ; Read bottom row last
    lda #%0000_0110
    sta $4016
    jsr FamilyTrainerWait

    lda $4017
    sta TmpX
    and #FEET_COL0
    bne :+
    ; pressed
    lda #%1000_0000
    ora FeetInput+1
    sta FeetInput+1
:

    lda TmpX
    and #FEET_COL1
    bne :+
    ; pressed
    lda #%0100_0000
    ora FeetInput+1
    sta FeetInput+1
:

    lda TmpX
    and #FEET_COL2
    bne :+
    ; pressed
    lda #%0010_0000
    ora FeetInput+1
    sta FeetInput+1
:

    lda TmpX
    and #FEET_COL3
    bne :+
    ; pressed
    lda #%0001_0000
    ora FeetInput+1
    sta FeetInput+1
:
    rts

FamilyTrainerWait:
    ; FTWaitVal value of $27 seems to be the
    ; lowest reliable
    ldy FTWaitVal   ; 3
    ldx #0          ; 2
:
    inc $8000, x    ; 7
    inc $8000, x    ; 7
    dey             ; 2
    bne :-          ; 3 each loop; 2 on last
                    ; (doesn't currently cross a page)

; 39 loops
; 19 cycles per loop

; 741 cycles in loop (740?)

; 11 cycles outside
; 6  cycles for jsr

; ~758 cycles between reads

    rts ; 6

; PPU Address in AddressPointer
; Tile layout label in AddressPointer2
DrawFeetController:
    lda AddressPointer+1
    sta $2006
    lda AddressPointer+0
    sta $2006

    ldy #2
    lda (AddressPointer2), y
    tax

    ; height
    ldy #3
    lda (AddressPointer2), y
    sta TmpX

    ; byte offset
    ldy #4
@loop:
    lda (AddressPointer2), y
    sta $2007
    iny

    dex
    bne @loop

    clc
    lda AddressPointer+0
    adc #$20
    sta AddressPointer+0
    tax
    lda AddressPointer+1
    adc #0
    sta AddressPointer+1
    sta $2006
    stx $2006

    sty TmpY
    ldy #2
    lda (AddressPointer2), y
    tax
    ldy TmpY

    dec TmpX
    lda TmpX
    bne @loop
    rts

FeetTileLookupA:
    .byte $20, $00, $00, $20
    .byte $00, $00, $00, $00
    .byte $20, $00, $00, $20

FeetTileLookupB:
    .byte $31, $32, $33, $34
    .byte $35, $36, $37, $38
    .byte $39, $0D, $0E, $0F

FeetLookupALo:
    .byte .lobyte(FEET_PPU_ADDR_A + (2*32+6))
    .byte .lobyte(FEET_PPU_ADDR_A + (2*32+4))
    .byte .lobyte(FEET_PPU_ADDR_A + (2*32+2))
    .byte .lobyte(FEET_PPU_ADDR_A + (2*32+0))

    .byte .lobyte(FEET_PPU_ADDR_A + (4*32+6))
    .byte .lobyte(FEET_PPU_ADDR_A + (4*32+4))
    .byte .lobyte(FEET_PPU_ADDR_A + (4*32+2))
    .byte .lobyte(FEET_PPU_ADDR_A + (4*32+0))

    .byte .lobyte(FEET_PPU_ADDR_A + (6*32+6))
    .byte .lobyte(FEET_PPU_ADDR_A + (6*32+4))
    .byte .lobyte(FEET_PPU_ADDR_A + (6*32+2))
    .byte .lobyte(FEET_PPU_ADDR_A + (6*32+0))

FeetLookupAHi:
    .byte .hibyte(FEET_PPU_ADDR_A + (2*32+0))
    .byte .hibyte(FEET_PPU_ADDR_A + (2*32+2))
    .byte .hibyte(FEET_PPU_ADDR_A + (2*32+4))
    .byte .hibyte(FEET_PPU_ADDR_A + (2*32+6))

    .byte .hibyte(FEET_PPU_ADDR_A + (4*32+0))
    .byte .hibyte(FEET_PPU_ADDR_A + (4*32+2))
    .byte .hibyte(FEET_PPU_ADDR_A + (4*32+4))
    .byte .hibyte(FEET_PPU_ADDR_A + (4*32+6))

    .byte .hibyte(FEET_PPU_ADDR_A + (6*32+0))
    .byte .hibyte(FEET_PPU_ADDR_A + (6*32+2))
    .byte .hibyte(FEET_PPU_ADDR_A + (6*32+4))
    .byte .hibyte(FEET_PPU_ADDR_A + (6*32+6))

FeetLookupBLo:
    .byte .lobyte(FEET_PPU_ADDR_B + (2*32+0))
    .byte .lobyte(FEET_PPU_ADDR_B + (2*32+2))
    .byte .lobyte(FEET_PPU_ADDR_B + (2*32+4))
    .byte .lobyte(FEET_PPU_ADDR_B + (2*32+6))

    .byte .lobyte(FEET_PPU_ADDR_B + (4*32+0))
    .byte .lobyte(FEET_PPU_ADDR_B + (4*32+2))
    .byte .lobyte(FEET_PPU_ADDR_B + (4*32+4))
    .byte .lobyte(FEET_PPU_ADDR_B + (4*32+6))

    .byte .lobyte(FEET_PPU_ADDR_B + (6*32+0))
    .byte .lobyte(FEET_PPU_ADDR_B + (6*32+2))
    .byte .lobyte(FEET_PPU_ADDR_B + (6*32+4))
    .byte .lobyte(FEET_PPU_ADDR_B + (6*32+6))

FeetLookupBHi:
    .byte .hibyte(FEET_PPU_ADDR_B + (2*32+0))
    .byte .hibyte(FEET_PPU_ADDR_B + (2*32+2))
    .byte .hibyte(FEET_PPU_ADDR_B + (2*32+4))
    .byte .hibyte(FEET_PPU_ADDR_B + (2*32+6))

    .byte .hibyte(FEET_PPU_ADDR_B + (4*32+0))
    .byte .hibyte(FEET_PPU_ADDR_B + (4*32+2))
    .byte .hibyte(FEET_PPU_ADDR_B + (4*32+4))
    .byte .hibyte(FEET_PPU_ADDR_B + (4*32+6))

    .byte .hibyte(FEET_PPU_ADDR_B + (6*32+0))
    .byte .hibyte(FEET_PPU_ADDR_B + (6*32+2))
    .byte .hibyte(FEET_PPU_ADDR_B + (6*32+4))
    .byte .hibyte(FEET_PPU_ADDR_B + (6*32+6))

FamilyTrainerTiles_SideA:
    .include "family-trainer-a.i"
FamilyTrainerTiles_SideB:
    .include "family-trainer-b.i"
