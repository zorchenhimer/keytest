
.include "nes2header.inc"
nes2mapper 0
nes2prg 1 * 16 * 1024
nes2chr 1 * 8 * 1024
;nes2wram 1 * 8 * 1024
nes2mirror 'V'
nes2tv 'N'
nes2end

.feature leading_dot_in_identifiers
.feature underline_in_numbers
.feature addrsize

TRUE  = 1
FALSE = 0

DEBUG_CONTROLLER_LAYOUT = FALSE
DEBUG_FEET_LAYOUT       = FALSE

FEET_PPU_ADDR_A = $20C6
FEET_PPU_ADDR_B = $20D3

.enum ControllerData
Btn_A
Btn_B
Btn_Select
Btn_Start
Btn_Up
Btn_Down
Btn_Left
Btn_Right
.endenum

BUTTON_A        = 1 << 7
BUTTON_B        = 1 << 6
BUTTON_SELECT   = 1 << 5
BUTTON_START    = 1 << 4
BUTTON_UP       = 1 << 3
BUTTON_DOWN     = 1 << 2
BUTTON_LEFT     = 1 << 1
BUTTON_RIGHT    = 1 << 0

FEET_COL0 = %0001_0000
FEET_COL1 = %0000_1000
FEET_COL2 = %0000_0100
FEET_COL3 = %0000_0010

.enum KeyboardKeys
; Row 0
kbd_CloseBracket
kbd_OpenBracket
kbd_Return
kbd_F8
kbd_Stop
kbd_Yen
kbd_RightShift
kbd_Kana

; Row 1
kbd_SemiColon
kbd_Colon
kbd_At
kbd_F7
kbd_Caret
kbd_Dash
kbd_Slash
kbd_SpaceSymbol

; Row 2
kbd_K
kbd_L
kbd_O
kbd_F6
kbd_0
kbd_P
kbd_Comma
kbd_Period

; Row 3
kbd_J
kbd_U
kbd_I
kbd_F5
kbd_8
kbd_9
kbd_N
kbd_M

; Row 4
kbd_H
kbd_G
kbd_Y
kbd_F4
kbd_6
kbd_7
kbd_V
kbd_B

; Row 5
kbd_D
kbd_R
kbd_T
kbd_F3
kbd_4
kbd_5
kbd_C
kbd_F

; Row 6
kbd_A
kbd_S
kbd_W
kbd_F2
kbd_3
kbd_E
kbd_Z
kbd_X

; Row 7
kbd_Control
kbd_Q
kbd_Escape
kbd_F1
kbd_2
kbd_1
kbd_Graph
kbd_LeftShift

; Row 8
kbd_Left
kbd_Right
kbd_Up
kbd_Clear
kdb_Insert
kbd_Delete
kbd_Space
kbd_Down
.endenum

.segment "ZEROPAGE"
Sleeping: .res 1

TmpX: .res 1
TmpY: .res 1
TmpZ: .res 1

btnX: .res 1
btnY: .res 1

AddressPointer: .res 2
AddressPointer2: .res 2
AddressPointer3: .res 2
MenuSelection: .res 1

NMILoopPointer: .res 2

BufferIndex:    .res 1  ; doubles as size; $FF = empty
Buffer_AddrLo:  .res 72
Buffer_AddrHi:  .res 72
Buffer_Data:     .res 72

ButtonMask: .res 1

; Loop count value for the FamilyTrainerWait
; routine
FTWaitVal: .res 1

KeyboardRead: .res 1

.segment "OAM"
SpriteZero: .res 4
Sprites: .res (64*4)-4
.segment "BSS"

controllers:            .res 6 ; buttons currently pressed
controllers_pressed:    .res 6 ; buttons pressed this frame
controllers_released:   .res 6 ; buttons released this frame
controllers_old:        .res 6 ; last frame's buttons

FeetInput: .res 2
KeyboardStatus: .res 72
Trackball: .res 3

NMI_Ram: .res 917

.segment "VECTORS"
    .word NMI
    .word RESET
    .word IRQ

.segment "CHR0"
    .incbin "font.chr"

.segment "CHR1"

.segment "PAGE0"

IRQ:
    rti

; TODO: move all of NMI to ram.  instead of lda zp,
;       lda #imm and write the buffer values
;       directly to where they are loaded immidiate
;       in the NMI handler.
NMI:
    pha
    txa
    pha
    tya
    pha

    lda #$FF
    sta Sleeping

    lda #$00
    sta $2003
    lda #$02
    sta $4014

    ;lda BufferIndex
    ;bmi @emptyBuffer
    ;tax
    ;lda UnrolledLookup_lo, x
    ;sta AddressPointer+0
    ;lda UnrolledLookup_hi, x
    ;sta AddressPointer+1
    jmp (NMILoopPointer)

UnrolledLoop:
    .repeat 72, i
    .ident( .sprintf("ul_%02d", (72-i-1))):
    ;.out .sprintf("ul_%02d", (72-i-1))
    lda Buffer_AddrHi+(72-i-1)  ; 3
    sta $2006                   ; 4
    lda Buffer_AddrLo+(72-i-1)  ; 3
    sta $2006                   ; 4
    lda Buffer_Data+(72-i-1)    ; 3
    sta $2007                   ; 4
    ; 21 cycles
    .endrepeat

NMI_Done:

    lda #.lobyte(NMI_Done)
    sta NMILoopPointer+0
    lda #.hibyte(NMI_Done)
    sta NMILoopPointer+1

    lda #$FF
    sta BufferIndex

    lda #0
    sta $2005
    sta $2005

    lda #%1000_0000
    sta $2000

    pla
    tay
    pla
    tax
    pla
    rti

UnrolledLookup_hi:
    .repeat 72, i
    .byte .hibyte(.ident(.sprintf("ul_%02d", i)))
    .endrepeat

UnrolledLookup_lo:
    .repeat 72, i
    .byte .lobyte(.ident(.sprintf("ul_%02d", i)))
    .endrepeat

PrepareNmiPointer:
    lda BufferIndex
    bmi @empty

    tax
    lda UnrolledLookup_lo, x
    sta NMILoopPointer+0
    lda UnrolledLookup_hi, x
    sta NMILoopPointer+1
    rts

@empty:
    lda #.lobyte(NMI_Done)
    sta NMILoopPointer+0
    lda #.hibyte(NMI_Done)
    sta NMILoopPointer+1

    lda #$FF
    sta BufferIndex
    rts

; Famicom expansion port controllers
ReadControllersExpansion:
    rts

ReadControllers:
    ; Freeze input
    lda #1
    sta $4016
    lda #0
    sta $4016

    ldx #0
@oldloop:
    lda controllers, x
    sta controllers_old, x
    inx
    cpx #6 ; FIXME: make this value a constant
    bne @oldloop

    ldx #0
    ldy #8
@player1:
    lda $4016
    lsr A              ; Bit0 -> Carry
    rol controllers, x ; Bit0 <- Carry
    ; famicom expanion controller
    lsr A
    rol controllers+4, x
    dey
    bne @player1

    ldx #2
    ldy #8
@player3:
    lda $4016
    lsr A              ; Bit0 -> Carry
    rol controllers, x ; Bit0 <- Carry
    dey
    bne @player3

    ldx #1
    ldy #8
@player2:
    lda $4017
    lsr A              ; Bit0 -> Carry
    rol controllers, x ; Bit0 <- Carry
    ; famicom expanion controller
    lsr A
    rol controllers+4, x
    dey
    bne @player2

    ldx #3
    ldy #8
@player4:
    lda $4017
    lsr A              ; Bit0 -> Carry
    rol controllers, x ; Bit0 <- Carry
    dey
    bne @player4

    ; If a controller has all buttons pressed, it
    ; doesn't exist.  This will happen if the
    ; four score isn't plugged in.
    ldx #0
@pressedLoop:
    lda controllers, x
    eor #$FF
    bne :+
    ; no controller here
    lda #0
    sta controllers, x
    sta controllers_old, x
    sta controllers_pressed, x
    sta controllers_released, x
    jmp :++

:   lda controllers, x
    eor controllers_old, x
    and controllers, x
    sta controllers_pressed, x

    lda controllers, x
    eor controllers_old, x
    and controllers_old, x
    sta controllers_released, x
:
    inx
    cpx #6
    bne @pressedLoop

    rts

ReadFeet:
    rts

WaitForNMI:
    jsr PrepareNmiPointer
:   bit Sleeping
    bpl :-
    lda #0
    sta Sleeping
    rts

; Menu to select what type of input device
DrawMenu:
    lda #$21
    sta AddressPointer+1
    sta $2006
    lda #$69
    sta AddressPointer+0
    sta $2006
    sta TmpX

    ldy #MenuItemCount
    ldx #0
@loop:
    lda MenuItems, x
    beq @next
    inx
    sta $2007
    jmp @loop

@next:
    inx
    dey
    beq @done
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
    jmp @loop

@done:

    ;sprite stuff
    lda #0
    sta MenuSelection
    sta SpriteZero+2
    asl a
    tax

    lda #$11
    sta SpriteZero+1

    lda MenuRows, x
    sta SpriteZero+0
    lda MenuRows+1, x
    sta SpriteZero+3

    rts

DrawKeyboard:
    rts

; should probably rename this, lol
DrawFeet:
    rts

ClearScreen:
    bit $2002
    lda #$20
    sta $2006
    lda #$00
    sta $2006

    ldx #30
    ldy #8
    lda #$20 ; use the "space" character for the background
:
    sta $2007
    sta $2007
    sta $2007
    sta $2007
    dey
    bne :-
    ldy #8
    dex
    bne :-

    ; attr table
    lda #0
    ldx #16
:
    sta $2007
    sta $2007
    sta $2007
    sta $2007
    dex
    bne :-
    rts

RESET:
    sei         ; Disable IRQs
    cld         ; Disable decimal mode

    ldx #$40
    stx $4017   ; Disable APU frame IRQ

    ldx #$FF
    txs         ; Setup new stack

    inx         ; Now X = 0

    stx $2000   ; disable NMI
    stx $2001   ; disable rendering
    stx $4010   ; disable DMC IRQs

:   ; First wait for VBlank to make sure PPU is ready.
    bit $2002   ; test this bit with ACC
    bpl :- ; Branch on result plus

:   ; Clear RAM
    lda #$00
    sta $0000, x
    sta $0100, x
    sta $0200, x
    sta $0300, x
    sta $0400, x
    sta $0500, x
    sta $0600, x
    sta $0700, x

    inx
    bne :-  ; loop if != 0

:   ; Second wait for vblank.  PPU is ready after this
    bit $2002
    bpl :-

    ; Clear sprites
    ldx #0
    lda #$FF
:
    sta $200, x
    inx
    bne :-

    lda #$00
    sta $2003
    lda #$02
    sta $4014

    lda #$3F
    sta $2006
    lda #$00
    sta $2006

    ldx #0
:
    lda Palettes, x
    sta $2007
    inx
    cpx #32
    bne :-

    lda #$FF
    sta BufferIndex

    lda #%1000_0000
    sta $2000

    ;lda #%0001_1110
    lda #0
    sta $2001

    jsr WaitForNMI
    jsr WaitForNMI

    ; TODO: palatte

    jsr ClearScreen
    jsr DrawMenu

;    ; Fill up the draw buffer with some data
;    ldy #$00
;    ldx #72
;:
;    lda #$20
;    sta Buffer_AddrHi, y
;    stx Buffer_AddrLo, y
;    ;lda #$A0
;    stx Buffer_Data, y
;    iny
;    dex
;    bne :-
;
;    lda #71
;    sta BufferIndex

    jsr WaitForNMI

    lda #%0001_1110
    sta $2001

Frame_Menu:
    jsr ReadControllers

    ldx #0
    stx TmpX
@loop:
    lda controllers_pressed, x
    ora TmpX
    sta TmpX
    inx
    cpx #6
    bne @loop


    lda #BUTTON_DOWN
    and TmpX
    beq :+
    inc MenuSelection
    lda MenuSelection
    cmp #MenuItemCount
    bcc :+
    lda #0  ; handle overflow
    sta MenuSelection
:
    lda #BUTTON_UP
    and TmpX
    beq :+
    dec MenuSelection
    lda MenuSelection
    bpl :+
    lda #MenuItemCount-1
    sta MenuSelection
:

    lda #BUTTON_A
    and TmpX
    beq :+
    jsr WaitForNMI

    lda #$20
    sta SpriteZero+1

    lda MenuSelection
    asl a
    tax
    lda MenuDestinations+0, x
    sta AddressPointer+0
    lda MenuDestinations+1, x
    sta AddressPointer+1
    jmp (AddressPointer)
:

    lda MenuSelection
    asl a
    tax

    lda MenuRows, x
    sta SpriteZero+0
    lda MenuRows+1, x
    sta SpriteZero+3

    jsr WaitForNMI
    jmp Frame_Menu

DrawTiledRegion:
    ; Start address in AddressPointer
    ; Data address in AddressPointer2
    lda AddressPointer+1
    sta $2006
    lda AddressPointer+0
    sta $2006

    ldy #2
    lda (AddressPointer2), y
    tax
    sta TmpX    ; width
    iny
    lda (AddressPointer2), y
    sta TmpY    ; height
    iny

@loop:
    lda (AddressPointer2), y
    sta $2007
    iny
    dex
    bne @loop

    ; next row
    clc
    lda AddressPointer+0
    adc #$20
    sta AddressPointer+0
    lda AddressPointer+1
    adc #0
    sta AddressPointer+1
    sta $2006
    lda AddressPointer+0
    sta $2006

    ldx TmpX
    dec TmpY
    bne @loop
    rts

Init_Controllers:
    lda #%1000_0000
    sta $2000

    lda #0
    sta $2001

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



Init_Keyboard:
    lda #%1000_0000
    sta $2000

    lda #0
    sta $2001

    jsr ClearScreen

    lda #.lobyte(KeyboardTiles)
    sta AddressPointer2+0
    lda #.hibyte(KeyboardTiles)
    sta AddressPointer2+1

    lda #.hibyte(KeyboardStartAddr)
    sta AddressPointer+1
    lda #.lobyte(KeyboardStartAddr)
    sta AddressPointer+0
    jsr DrawTiledRegion

    jsr WaitForNMI
    lda #%0001_1110
    sta $2001

Frame_Keyboard:
    jsr ReadKeyboard

    ldx #0
@loop:
    lda KeyboardStatus, x
    beq :+
    lda #$80
    jmp :++
:
    lda #$00
:

    ora KeyboardTileLookup, x
    inc BufferIndex
    ldy BufferIndex
    sta Buffer_Data, y

    lda KeyboardLookupLo, x
    sta Buffer_AddrLo, x
    lda KeyboardLookupHi, x
    sta Buffer_AddrHi, x

    inx
    cpx #72
    bne @loop

    jsr WaitForNMI
    jmp Frame_Keyboard

ReadKeyboard:
    lda #0
    ldx #0
:
    sta KeyboardStatus, x
    inx
    cpx #72
    bne :-

    lda #$05
    sta $4016

    ; Read row 0
    lda #$04
    sta $4016
    jsr KeyboardWait

    ldx #0
@ReadLoop:
    ; col 0
    lda $4017
    clc
    asl a
    asl a
    asl a
    sta KeyboardRead

    ; col 1
    lda #$06
    sta $4016
    lda $4017
    lsr a
    and #$0F
    ora KeyboardRead
    sta KeyboardRead

    ; Prep read for next row
    lda #$04
    sta $4016

    ; Decode the row
    ldy #1 ; 2
    lda #%1000_0000 ; 2
    and KeyboardRead ; 3
    bne :+ ; 3
    lda #1
    sta KeyboardStatus, x

:   inx
    lda #%0100_0000
    and KeyboardRead
    bne :+
    lda #1
    sta KeyboardStatus, x

:   inx
    lda #%0010_0000
    and KeyboardRead
    bne :+
    lda #1
    sta KeyboardStatus, x

:   inx
    lda #%0001_0000
    and KeyboardRead
    bne :+
    lda #1
    sta KeyboardStatus, x

:   inx
    lda #%0000_1000
    and KeyboardRead
    bne :+
    lda #1
    sta KeyboardStatus, x

:   inx
    lda #%0000_0100
    and KeyboardRead
    bne :+
    lda #1
    sta KeyboardStatus, x

:   inx
    lda #%0000_0010
    and KeyboardRead
    bne :+
    lda #1
    sta KeyboardStatus, x

:   inx
    lda #%0000_0001
    and KeyboardRead
    bne :+
    lda #1
    sta KeyboardStatus, x
:

    ldy #10
@delay:
    lda #0
    dey
    bne @delay

    inx
    cpx #72
    beq :+
    jmp @ReadLoop
:

    rts

KeyboardWait:
    ; jsr 6
    ldx #7 ; 2
:
    cpy $8000 ; 4
    dex ; 2
    bne :- ; 3 each loop; 2 on last = 20 total

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

Init_Feet:
    lda #%1000_0000
    sta $2000

    lda #0
    sta $2001

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

HexAscii:
    .byte "0123456789ABCDEF"

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

Init_Trackball:
    lda #%1000_0000
    sta $2000

    lda #0
    sta $2001

    jsr ClearScreen

    ;; setup stuff
    lda #$84
    clc
    adc #$40
    sta AddressPointer+0
    lda #$20
    adc #0
    sta AddressPointer+1

    lda #.lobyte(ControllerTiles)
    sta AddressPointer2+0
    lda #.hibyte(ControllerTiles)
    sta AddressPointer2+1
    jsr DrawTiledRegion

    lda #$00
    sta SpriteZero+1
    sta SpriteZero+2
    lda #120
    sta SpriteZero+3
    lda #96
    sta SpriteZero+0

    jsr WaitForNMI
    lda #%0001_1110
    sta $2001

Frame_Trackball:
    jsr ReadTrackball

    lda #0
    sta TmpX
@loop:
    rol controllers+0
    bcc :+
    lda #$80
    jmp :++
:   lda #$00

:   ldy TmpX
    ora ControllerTileLookup, y

    inc BufferIndex
    ldx BufferIndex
    sta Buffer_Data, x

    lda ControllerLookupLo, y
    sta Buffer_AddrLo, x
    lda ControllerLookupHi, y
    sta Buffer_AddrHi, x

    inc TmpX
    lda TmpX
    cmp #8
    bne @loop

    lda controllers+1
    and #$0F
    tax
    lda HexAscii, x

    inc BufferIndex
    ldx BufferIndex
    sta Buffer_Data, x

    lda #$87
    sta Buffer_AddrLo, x
    lda #$20
    sta Buffer_AddrHi, x

    lda controllers+1
    lsr a
    lsr a
    lsr a
    lsr a
    tax
    lda HexAscii, x

    inc BufferIndex
    ldx BufferIndex
    sta Buffer_Data, x

    lda #$88
    sta Buffer_AddrLo, x
    lda #$20
    sta Buffer_AddrHi, x

    lda controllers+2
    and #$0F
    tax
    lda HexAscii, x

    inc BufferIndex
    ldx BufferIndex
    sta Buffer_Data, x

    lda #$89
    sta Buffer_AddrLo, x
    lda #$20
    sta Buffer_AddrHi, x

    lda controllers+2
    lsr a
    lsr a
    lsr a
    lsr a
    tax
    lda HexAscii, x

    inc BufferIndex
    ldx BufferIndex
    sta Buffer_Data, x

    lda #$8A
    sta Buffer_AddrLo, x
    lda #$20
    sta Buffer_AddrHi, x

    ; Trackball Sprite
    ; Axis 2
    ; FIXME: horizontal is backwards
    lda controllers+1
    and #$08
    beq :+
    clc
    lda controllers+1
    and #$07
    ora #$F8
    adc SpriteZero+0
    sta SpriteZero+0
    jmp :++
:
    clc
    lda controllers+1
    and #$07
    adc SpriteZero+0
    sta SpriteZero+0
:

    ; Axis 1
    lda controllers+1
    lsr a
    lsr a
    lsr a
    lsr a
    sta TmpX
    and #$08
    beq :+
    clc
    lda TmpX
    and #$07
    ora #$F8
    adc SpriteZero+3
    sta SpriteZero+3
    jmp :++
:
    clc
    lda TmpX
    and #$07
    adc SpriteZero+3
    sta SpriteZero+3
:

    jsr WaitForNMI
    jmp Frame_Trackball

ReadTrackball:
    lda #1
    sta $4016
    lda #0
    sta $4016

    ldy #8
:
    lda $4016
    lsr a
    lsr a
    rol controllers
    dey
    bne :-

    ldy #8
:
    lda $4016
    lsr a
    lsr a
    rol controllers+1
    dey
    bne :-

    ldy #8
:
    lda $4016
    lsr a
    lsr a
    rol controllers+2
    dey
    bne :-

    lda controllers+1
    eor #$FF
    sta controllers+1

    rts

MenuItems:
    .asciiz "Controllers"
    .asciiz "Keyboard"
    .asciiz "Family Trainer"
    .asciiz "Hori Track"

MenuRows:
    ;      Y, X
    .byte 87,  56
    .byte 103, 56
    .byte 119, 56
    .byte 135, 56
MenuItemCount = (* - MenuRows) / 2

MenuDestinations:
    .word Init_Controllers
    .word Init_Keyboard
    .word Init_Feet
    .word Init_Trackball

Palettes:
    ; BG
    .byte $0F, $20, $2A, $00
    .byte $0F, $20, $2A, $00
    .byte $0F, $20, $2A, $00
    .byte $0F, $20, $2A, $00

    ; Sprite
    .byte $0F, $20, $2A, $00
    .byte $0F, $20, $2A, $00
    .byte $0F, $20, $2A, $00
    .byte $0F, $20, $2A, $00

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


KeyboardStartAddr = $2084
KeyboardLookupLo:
    .byte .lobyte(KeyboardStartAddr + (3*32+12)) ; ]
    .byte .lobyte(KeyboardStartAddr + (2*32+12)) ; [
    .byte .lobyte(KeyboardStartAddr + (2*32+13)) ; Return
    .byte .lobyte(KeyboardStartAddr + (0*32+14)) ; F8
    .byte .lobyte(KeyboardStartAddr + (1*32+13)) ; Stop
    .byte .lobyte(KeyboardStartAddr + (1*32+12)) ; Yen
    .byte .lobyte(KeyboardStartAddr + (4*32+12)) ; Right shift
    .byte .lobyte(KeyboardStartAddr + (3*32+13)) ; Kana

    .byte .lobyte(KeyboardStartAddr + (3*32+10)) ; Semi colon
    .byte .lobyte(KeyboardStartAddr + (3*32+11)) ; Colon
    .byte .lobyte(KeyboardStartAddr + (2*32+11)) ; @
    .byte .lobyte(KeyboardStartAddr + (0*32+12)) ; F7
    .byte .lobyte(KeyboardStartAddr + (1*32+11)) ; Caret
    .byte .lobyte(KeyboardStartAddr + (1*32+10)) ; Dash
    .byte .lobyte(KeyboardStartAddr + (4*32+10)) ; Slash
    .byte .lobyte(KeyboardStartAddr + (4*32+11)) ; Space symbol

    .byte .lobyte(KeyboardStartAddr + (3*32+8))  ; K
    .byte .lobyte(KeyboardStartAddr + (3*32+9))  ; L
    .byte .lobyte(KeyboardStartAddr + (2*32+9))  ; O
    .byte .lobyte(KeyboardStartAddr + (0*32+10)) ; F6
    .byte .lobyte(KeyboardStartAddr + (1*32+9))  ; 0
    .byte .lobyte(KeyboardStartAddr + (2*32+10)) ; P
    .byte .lobyte(KeyboardStartAddr + (4*32+8))  ; Comma
    .byte .lobyte(KeyboardStartAddr + (4*32+9))  ; Period

    .byte .lobyte(KeyboardStartAddr + (3*32+7))  ; J
    .byte .lobyte(KeyboardStartAddr + (2*32+7))  ; U
    .byte .lobyte(KeyboardStartAddr + (2*32+8))  ; I
    .byte .lobyte(KeyboardStartAddr + (0*32+8))  ; F5
    .byte .lobyte(KeyboardStartAddr + (1*32+7))  ; 8
    .byte .lobyte(KeyboardStartAddr + (1*32+8))  ; 9
    .byte .lobyte(KeyboardStartAddr + (4*32+6))  ; N
    .byte .lobyte(KeyboardStartAddr + (4*32+7))  ; M

    .byte .lobyte(KeyboardStartAddr + (3*32+6))  ; H
    .byte .lobyte(KeyboardStartAddr + (3*32+5))  ; G
    .byte .lobyte(KeyboardStartAddr + (2*32+6))  ; Y
    .byte .lobyte(KeyboardStartAddr + (0*32+6))  ; F4
    .byte .lobyte(KeyboardStartAddr + (1*32+5))  ; 6
    .byte .lobyte(KeyboardStartAddr + (1*32+6))  ; 7
    .byte .lobyte(KeyboardStartAddr + (4*32+4))  ; V
    .byte .lobyte(KeyboardStartAddr + (4*32+5))  ; B

    .byte .lobyte(KeyboardStartAddr + (3*32+3))  ; D
    .byte .lobyte(KeyboardStartAddr + (2*32+4))  ; R
    .byte .lobyte(KeyboardStartAddr + (2*32+5))  ; T
    .byte .lobyte(KeyboardStartAddr + (0*32+4))  ; F3
    .byte .lobyte(KeyboardStartAddr + (1*32+3))  ; 4
    .byte .lobyte(KeyboardStartAddr + (1*32+4))  ; 5
    .byte .lobyte(KeyboardStartAddr + (4*32+3))  ; C
    .byte .lobyte(KeyboardStartAddr + (3*32+4))  ; F

    .byte .lobyte(KeyboardStartAddr + (3*32+1))  ; A
    .byte .lobyte(KeyboardStartAddr + (3*32+2))  ; S
    .byte .lobyte(KeyboardStartAddr + (2*32+2))  ; W
    .byte .lobyte(KeyboardStartAddr + (0*32+2))  ; F2
    .byte .lobyte(KeyboardStartAddr + (1*32+2))  ; 3
    .byte .lobyte(KeyboardStartAddr + (2*32+3))  ; E
    .byte .lobyte(KeyboardStartAddr + (4*32+1))  ; Z
    .byte .lobyte(KeyboardStartAddr + (4*32+2))  ; X

    .byte .lobyte(KeyboardStartAddr + (3*32+0))  ; Control
    .byte .lobyte(KeyboardStartAddr + (1*32+1))  ; Q
    .byte .lobyte(KeyboardStartAddr + (1*32+0))  ; Escape
    .byte .lobyte(KeyboardStartAddr + (0*32+0))  ; F1
    .byte .lobyte(KeyboardStartAddr + (1*32+1))  ; 2
    .byte .lobyte(KeyboardStartAddr + (1*32+0))  ; 1
    .byte .lobyte(KeyboardStartAddr + (5*32+1))  ; Graph
    .byte .lobyte(KeyboardStartAddr + (4*32+0))  ; Left shift

    .byte .lobyte(KeyboardStartAddr + (4*32+15)) ; Left
    .byte .lobyte(KeyboardStartAddr + (4*32+17)) ; Right
    .byte .lobyte(KeyboardStartAddr + (3*32+16)) ; Up
    .byte .lobyte(KeyboardStartAddr + (2*32+15)) ; Clear
    .byte .lobyte(KeyboardStartAddr + (2*32+16)) ; Inesrt
    .byte .lobyte(KeyboardStartAddr + (2*32+17)) ; Delete
    .byte .lobyte(KeyboardStartAddr + (5*32+4))  ; Space
    .byte .lobyte(KeyboardStartAddr + (5*32+16)) ; Down

KeyboardLookupHi:
    .byte .hibyte(KeyboardStartAddr + (3*32+12)) ; ]
    .byte .hibyte(KeyboardStartAddr + (2*32+12)) ; [
    .byte .hibyte(KeyboardStartAddr + (2*32+13)) ; Return
    .byte .hibyte(KeyboardStartAddr + (0*32+14)) ; F8
    .byte .hibyte(KeyboardStartAddr + (1*32+13)) ; Stop
    .byte .hibyte(KeyboardStartAddr + (1*32+12)) ; Yen
    .byte .hibyte(KeyboardStartAddr + (4*32+12)) ; Right shift
    .byte .hibyte(KeyboardStartAddr + (3*32+13)) ; Kana

    .byte .hibyte(KeyboardStartAddr + (3*32+10)) ; Semi colon
    .byte .hibyte(KeyboardStartAddr + (3*32+11)) ; Colon
    .byte .hibyte(KeyboardStartAddr + (2*32+11)) ; @
    .byte .hibyte(KeyboardStartAddr + (0*32+12)) ; F7
    .byte .hibyte(KeyboardStartAddr + (1*32+11)) ; Caret
    .byte .hibyte(KeyboardStartAddr + (1*32+10)) ; Dash
    .byte .hibyte(KeyboardStartAddr + (4*32+10)) ; Slash
    .byte .hibyte(KeyboardStartAddr + (4*32+11)) ; Space symbol

    .byte .hibyte(KeyboardStartAddr + (3*32+8))  ; K
    .byte .hibyte(KeyboardStartAddr + (3*32+9))  ; L
    .byte .hibyte(KeyboardStartAddr + (2*32+9))  ; O
    .byte .hibyte(KeyboardStartAddr + (0*32+10)) ; F6
    .byte .hibyte(KeyboardStartAddr + (1*32+9))  ; 0
    .byte .hibyte(KeyboardStartAddr + (2*32+10)) ; P
    .byte .hibyte(KeyboardStartAddr + (4*32+8))  ; Comma
    .byte .hibyte(KeyboardStartAddr + (4*32+9))  ; Period

    .byte .hibyte(KeyboardStartAddr + (3*32+7))  ; J
    .byte .hibyte(KeyboardStartAddr + (2*32+7))  ; U
    .byte .hibyte(KeyboardStartAddr + (2*32+8))  ; I
    .byte .hibyte(KeyboardStartAddr + (0*32+8))  ; F5
    .byte .hibyte(KeyboardStartAddr + (1*32+7))  ; 8
    .byte .hibyte(KeyboardStartAddr + (1*32+8))  ; 9
    .byte .hibyte(KeyboardStartAddr + (4*32+6))  ; N
    .byte .hibyte(KeyboardStartAddr + (4*32+7))  ; M

    .byte .hibyte(KeyboardStartAddr + (3*32+6))  ; H
    .byte .hibyte(KeyboardStartAddr + (3*32+5))  ; G
    .byte .hibyte(KeyboardStartAddr + (2*32+6))  ; Y
    .byte .hibyte(KeyboardStartAddr + (0*32+6))  ; F4
    .byte .hibyte(KeyboardStartAddr + (1*32+5))  ; 6
    .byte .hibyte(KeyboardStartAddr + (1*32+6))  ; 7
    .byte .hibyte(KeyboardStartAddr + (4*32+4))  ; V
    .byte .hibyte(KeyboardStartAddr + (4*32+5))  ; B

    .byte .hibyte(KeyboardStartAddr + (3*32+3))  ; D
    .byte .hibyte(KeyboardStartAddr + (2*32+4))  ; R
    .byte .hibyte(KeyboardStartAddr + (2*32+5))  ; T
    .byte .hibyte(KeyboardStartAddr + (0*32+4))  ; F3
    .byte .hibyte(KeyboardStartAddr + (1*32+3))  ; 4
    .byte .hibyte(KeyboardStartAddr + (1*32+4))  ; 5
    .byte .hibyte(KeyboardStartAddr + (4*32+3))  ; C
    .byte .hibyte(KeyboardStartAddr + (3*32+4))  ; F

    .byte .hibyte(KeyboardStartAddr + (3*32+1))  ; A
    .byte .hibyte(KeyboardStartAddr + (3*32+2))  ; S
    .byte .hibyte(KeyboardStartAddr + (2*32+2))  ; W
    .byte .hibyte(KeyboardStartAddr + (0*32+2))  ; F2
    .byte .hibyte(KeyboardStartAddr + (1*32+2))  ; 3
    .byte .hibyte(KeyboardStartAddr + (2*32+3))  ; E
    .byte .hibyte(KeyboardStartAddr + (4*32+1))  ; Z
    .byte .hibyte(KeyboardStartAddr + (4*32+2))  ; X

    .byte .hibyte(KeyboardStartAddr + (3*32+0))  ; Control
    .byte .hibyte(KeyboardStartAddr + (1*32+1))  ; Q
    .byte .hibyte(KeyboardStartAddr + (1*32+0))  ; Escape
    .byte .hibyte(KeyboardStartAddr + (0*32+0))  ; F1
    .byte .hibyte(KeyboardStartAddr + (1*32+1))  ; 2
    .byte .hibyte(KeyboardStartAddr + (1*32+0))  ; 1
    .byte .hibyte(KeyboardStartAddr + (5*32+1))  ; Graph
    .byte .hibyte(KeyboardStartAddr + (4*32+0))  ; Left shift

    .byte .hibyte(KeyboardStartAddr + (4*32+15)) ; Left
    .byte .hibyte(KeyboardStartAddr + (4*32+17)) ; Right
    .byte .hibyte(KeyboardStartAddr + (3*32+16)) ; Up
    .byte .hibyte(KeyboardStartAddr + (2*32+15)) ; Clear
    .byte .hibyte(KeyboardStartAddr + (2*32+16)) ; Inesrt
    .byte .hibyte(KeyboardStartAddr + (2*32+17)) ; Delete
    .byte .hibyte(KeyboardStartAddr + (5*32+4))  ; Space
    .byte .hibyte(KeyboardStartAddr + (5*32+16)) ; Down

KeyboardTileLookup:
    .byte "]", "[", $19, $0A, $18, $7F, $1A, $1D
    .byte $3B, ":", "@", $09, "^", "-", "/", $01
    .byte "k", "l", "o", $08, "0", "p", ",", "."
    .byte "j", "u", "i", $07, "8", "9", "n", "m"
    .byte "h", "g", "y", $06, "6", "7", "v", "b"
    .byte "d", "r", "t", $05, "4", "5", "c", "f"
    .byte "a", "s", "w", $04, "3", "e", "z", "x"
    .byte $1B, "q", $1C, $03, "2", "1", $17, $1A
    .byte $13, $11, $10, $14, $15, $16, "S", $12

KeyboardTiles:
    .include "keyboard.i"

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
