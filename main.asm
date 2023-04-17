
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

.segment "ZEROPAGE"
Sleeping: .res 1

TmpX: .res 1
TmpY: .res 1

btnX: .res 1
btnY: .res 1

AddressPointer: .res 2
AddressPointer2: .res 2
MenuSelection: .res 1

controllers:            .res 4 ; buttons currently pressed
controllers_pressed:    .res 4 ; buttons pressed this frame
controllers_released:   .res 4 ; buttons pressed this frame
controllers_old:        .res 4 ; last frame's buttons

BufferIndex:    .res 1  ; doubles as size; $FF = empty
Buffer_AddrLo:  .res 72
Buffer_AddrHi:  .res 72
Buffer_Data:     .res 72

.segment "OAM"
SpriteZero: .res 4
Sprites: .res (64*4)-4
.segment "BSS"

Controller1_Lookup: .res 16
Controller2_Lookup: .res 16
Controller3_Lookup: .res 16
Controller4_Lookup: .res 16

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

    lda BufferIndex
    bmi @emptyBuffer
    tax
    lda UnrolledLookup_lo, x
    sta AddressPointer+0
    lda UnrolledLookup_hi, x
    sta AddressPointer+1
    jmp (AddressPointer)

;    ldy #48 ; max updates per frame
;
;@bufferLoop:
;    lda Buffer_AddrHi, x
;    sta $2006
;    lda Buffer_AddrLo, x
;    sta $2006
;    lda Buffer_Data, x
;    sta $2007
;    dex
;    bmi @buffDone
;    dey
;    bne @bufferLoop ; wait until the next frame to finish
;
;@buffWait:
;    stx BufferIndex
;    jmp @emptyBuffer
;
;@buffDone:
;    lda #$FF
;    sta BufferIndex
;
@emptyBuffer:

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

UnrolledLoop:
    .repeat 72, i
    .ident( .sprintf("ul_%02d", (72-i-1))):
    ;.out .sprintf("ul_%02d", (72-i-1))
    lda Buffer_AddrHi+(72-i-1)
    sta $2006
    lda Buffer_AddrLo+(72-i-1)
    sta $2006
    lda Buffer_Data+(72-i-1)
    sta $2007
    .endrepeat

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

    inx
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

    inx
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

    inx
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

LoadControllerLookup:
    ; Start ppu addr in AddressPointer2
    ; RAM table addr in AddressPointer
    ldx #0
    ldy #0
    lda #8
    sta TmpX
@loop:
    clc
    lda ControllerButtonData, x
    adc AddressPointer2+0
    adc #$40
    sta (AddressPointer), y
    iny
    lda AddressPointer2+1
    adc #0
    sta (AddressPointer), y
    iny
    inx

    dec TmpX
    bne @loop
    rts

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
    lda ControllerLookupLookup+0, x
    sta AddressPointer+0
    lda ControllerLookupLookup+1, x
    sta AddressPointer+1
    jsr LoadControllerLookup

    pla
    tax
    pla
    tay

    iny
    inx
    inx
    cpx #7
    bcc @txtStart

    jsr WaitForNMI
    lda #%0001_1110
    sta $2001

Frame_Controllers:
    jsr ReadControllers

    jsr WaitForNMI
    jmp Frame_Controllers

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

ControllerButtonData:
    .byte (1*32+8), (1*32+7), (1*32+4), (1*32+5)
    .byte (0*32+1), (2*32+1), (1*32+0), (1*32+2)
    ;.byte 18, 17, 14, 15, 1, 20, 10, 12

ControllerLookupLookup:
    .word Controller1_Lookup
    .word Controller2_Lookup
    .word Controller3_Lookup
    .word Controller4_Lookup

ControllerTiles:
    .include "controller.i"

KeyboardTiles:
    .include "keyboard.i"
