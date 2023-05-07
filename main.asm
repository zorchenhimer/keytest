
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

TRUE = 1
FALSE = 0

DEBUG_CONTROLLER_LAYOUT = FALSE

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

controllers:            .res 4 ; buttons currently pressed
controllers_pressed:    .res 4 ; buttons pressed this frame
controllers_released:   .res 4 ; buttons released this frame
controllers_old:        .res 4 ; last frame's buttons

BufferIndex:    .res 1  ; doubles as size; $FF = empty
Buffer_AddrLo:  .res 72
Buffer_AddrHi:  .res 72
Buffer_Data:     .res 72

ButtonMask: .res 1

.segment "OAM"
SpriteZero: .res 4
Sprites: .res (64*4)-4
.segment "BSS"

; PPU Address lookups for buttons
;Controller_Lookup_Lo: .res 32
;Controller_Lookup_Hi: .res 32

.segment "VECTORS"
    .word NMI
    .word RESET
    .word IRQ

.segment "CHR0"
    .incbin "font.chr"

.segment "CHR1"

BUTTON_A        = 1 << 7
BUTTON_B        = 1 << 6
BUTTON_SELECT   = 1 << 5
BUTTON_START    = 1 << 4
BUTTON_UP       = 1 << 3
BUTTON_DOWN     = 1 << 2
BUTTON_LEFT     = 1 << 1
BUTTON_RIGHT    = 1 << 0

.segment "PAGE0"

IRQ:
    rti

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

UnrolledLookup_hi:
    .repeat 72, i
    .byte .hibyte(.ident(.sprintf("ul_%02d", i)))
    .endrepeat

UnrolledLookup_lo:
    .repeat 72, i
    .byte .lobyte(.ident(.sprintf("ul_%02d", i)))
    .endrepeat

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

ReadControllers:
    ; Freeze input
    lda #1
    sta $4016
    lda #0
    sta $4016

    ldx #0
    ldy #8
    lda controllers, x
    sta controllers_old, x
@player1:
    lda $4016
    lsr A              ; Bit0 -> Carry
    rol controllers, x ; Bit0 <- Carry
    dey
    bne @player1

    lda controllers, x
    eor controllers_old, x
    and controllers, x
    sta controllers_pressed, x

    lda controllers, x
    eor controllers_old, x
    and controllers_old, x
    sta controllers_released, x

    ldx #2
    ldy #8
    lda controllers, x
    sta controllers_old, x
@player3:
    lda $4016
    lsr A              ; Bit0 -> Carry
    rol controllers, x ; Bit0 <- Carry
    dey
    bne @player3

    lda controllers, x
    eor controllers_old, x
    and controllers, x
    sta controllers_pressed, x

    lda controllers, x
    eor controllers_old, x
    and controllers_old, x
    sta controllers_released, x

    ldx #1
    lda controllers, x
    sta controllers_old, x
    ldy #8
@player2:
    lda $4017
    lsr A              ; Bit0 -> Carry
    rol controllers, x ; Bit0 <- Carry
    dey
    bne @player2

    lda controllers, x
    eor controllers_old, x
    and controllers, x
    sta controllers_pressed, x

    lda controllers, x
    eor controllers_old, x
    and controllers_old, x
    sta controllers_released, x

    ldx #3
    lda controllers, x
    sta controllers_old, x
    ldy #8
@player4:
    lda $4017
    lsr A              ; Bit0 -> Carry
    rol controllers, x ; Bit0 <- Carry
    dey
    bne @player4

    lda controllers, x
    eor controllers_old, x
    and controllers, x
    sta controllers_pressed, x

    lda controllers, x
    eor controllers_old, x
    and controllers_old, x
    sta controllers_released, x
    rts

ReadKeyboard:
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

    ldy #3
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

    lda #BUTTON_DOWN
    and controllers_pressed
    beq :+
    inc MenuSelection
    lda MenuSelection
    cmp #3
    bcc :+
    lda #0  ; handle overflow
    sta MenuSelection
:
    lda #BUTTON_UP
    and controllers_pressed
    beq :+
    dec MenuSelection
    lda MenuSelection
    bpl :+
    lda #2
    sta MenuSelection
:

    lda #BUTTON_A
    and controllers_pressed
    beq :+
    jsr WaitForNMI
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

ControllerLabels:
    .asciiz "Player 1"
    .asciiz "Player 2"
    .asciiz "Player 3"
    .asciiz "Player 4"

ControllerLabelAddrs:
    .word $2084
    .word $2093
    .word $2204
    .word $2213

DrawController:
    ; Start address in AddressPointer
    lda AddressPointer+1
    sta $2006
    lda AddressPointer+0
    sta $2006

    ldx #2
    lda ControllerTiles, x
    tay ; width
    sta TmpX
    inx
    lda ControllerTiles, x
    sta TmpY ; height
    inx

@loop:
    lda ControllerTiles, x
    sta $2007
    inx
    dey
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

    lda TmpX
    tay
    dec TmpY
    bne @loop
    rts

;LoadControllerLookup:
;    ; Start ppu addr in AddressPointer2
;    ; RAM table addr in AddressPointer
;    ldx #0
;    ldy #0
;    lda #8
;    sta TmpX
;@loop:
;    clc
;    lda ControllerButtonData, x ; load offset from anchor
;    adc AddressPointer2+0 ; add PPU start
;    adc #$40
;    sta (AddressPointer), y ; store lo
;    ;iny
;    lda AddressPointer2+1
;    adc #0
;    sta (AddressPointer3), y ; store hi
;    iny
;    inx
;
;    dec TmpX
;    bne @loop
;    rts

Init_Controllers:

    lda #%1000_0000
    sta $2000

    lda #0
    sta $2001

    jsr ClearScreen

    lda #0
    sta SpriteZero+1

    ldx #0
    ldy #0
@txtStart:
    lda ControllerLabelAddrs+1, x
    sta AddressPointer+1
    sta AddressPointer2+1
    sta $2006
    lda ControllerLabelAddrs+0, x
    sta AddressPointer+0
    sta AddressPointer2+0
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

    ; draw controller
    clc
    lda AddressPointer+0
    adc #$40
    sta AddressPointer+0
    lda AddressPointer+1
    adc #0
    sta AddressPointer+1
    jsr DrawController

    pla
    pha
    tax

    ; Populate lookup table
    ;lda ControllerLookupLookupLo+0, x
    ;sta AddressPointer+0
    ;lda ControllerLookupLookupLo+1, x
    ;sta AddressPointer+1

    ;lda ControllerLookupLookupHi+0, x
    ;sta AddressPointer3+0
    ;lda ControllerLookupLookupHi+1, x
    ;sta AddressPointer3+1

    ;jsr LoadControllerLookup

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
    lda Controller_Lookup_Hi, x
    sta $2006
    lda Controller_Lookup_Lo, x
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

    lda Controller_Lookup_Lo, y
    sta Buffer_AddrLo, x
    lda Controller_Lookup_Hi, y
    sta Buffer_AddrHi, x

    inc TmpX
    lda TmpX
    cmp #8
    bne @loop

    rts


Init_Keyboard:
    brk
    jmp Init_Keyboard

Init_Feet:
    brk
    jmp Init_Feet

MenuItems:
    .asciiz "Controllers"
    .asciiz "Keyboard"
    .asciiz "Family Trainer"

MenuRows:
    ;      Y, X
    .byte 87,  56
    .byte 103, 56
    .byte 119, 56

MenuDestinations:
    .word Init_Controllers
    .word Init_Keyboard
    .word Init_Feet

Palettes:
    ; BG
    .byte $0F, $20, $10, $00
    .byte $0F, $20, $10, $00
    .byte $0F, $20, $10, $00
    .byte $0F, $20, $10, $00

    ; Sprite
    .byte $0F, $20, $10, $00
    .byte $0F, $20, $10, $00
    .byte $0F, $20, $10, $00
    .byte $0F, $20, $10, $00

; Offsets from the top left of the controller.
; Used to pre-compute PPU addresses
;ControllerButtonData:
;    .byte (1*32+8), (1*32+7), (1*32+4), (1*32+5)
;    .byte (0*32+1), (2*32+1), (1*32+0), (1*32+2)
;    ;.byte 18, 17, 14, 15, 1, 20, 10, 12

ControllerTileLookup:
    ;     A    B    S    S    U    D    L    R
    .byte $1E, $1E, $1F, $1F, $10, $12, $13, $11

;ControllerLookupLookupLo:
;    .word Controller_Lookup_Lo+0
;    .word Controller_Lookup_Lo+8
;    .word Controller_Lookup_Lo+16
;    .word Controller_Lookup_Lo+24
;
;ControllerLookupLookupHi:
;    .word Controller_Lookup_Hi+0
;    .word Controller_Lookup_Hi+8
;    .word Controller_Lookup_Hi+16
;    .word Controller_Lookup_Hi+24

; LabelAddrs + $40
Ctrl1_Addr = $20C4
Ctrl2_Addr = $20D3
Ctrl3_Addr = $2244
Ctrl4_Addr = $2253

; PPU Address lookup tables for controller buttons
Controller_Lookup_Lo:
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

Controller_Lookup_Hi:
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

KeyboardTiles:
    .include "keyboard.i"

Mult8:
    .repeat 4, i
    .byte (8 * i)
    .endrepeat
