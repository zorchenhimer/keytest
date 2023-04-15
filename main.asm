
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
MenuSelection: .res 1

.segment "OAM"
SpriteZero: .res 4
Sprites: .res (64*4)-4
.segment "BSS"

controller1:        .res 1
controller2:        .res 1
controller1_old:    .res 1
controller2_old:    .res 1

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

    lda #$FF
    sta Sleeping

    lda #$00
    sta $2003
    lda #$02
    sta $4014

    lda #0
    sta $2005
    sta $2005

    lda #%1000_0000
    sta $2000

    pla
    rti

ReadControllers:
    lda controller1
    sta controller1_old

    lda controller2
    sta controller2_old

    ; Freeze input
    lda #1
    sta $4016
    lda #0
    sta $4016

    LDX #$08
@player1:
    lda $4016
    lsr A           ; Bit0 -> Carry
    rol controller1 ; Bit0 <- Carry
    dex
    bne @player1

    ldx #$08
@player2:
    lda $4017
    lsr A           ; Bit0 -> Carry
    rol controller2 ; Bit0 <- Carry
    dex
    bne @player2
    rts

ButtonPressedP1:
    sta btnX
    and controller1
    sta btnY

    lda controller1_old
    and btnX

    cmp btnY
    bne btnPress_stb

    ; no button change
    rts

ButtonPressedP2:
    sta btnX
    and controller2
    sta btnY

    lda controller2_old
    and btnX

    cmp btnY
    bne btnPress_stb

    ; no button change
    rts

btnPress_stb:
    ; button released
    lda btnY
    bne btnPress_stc
    rts

btnPress_stc:
    ; button pressed
    lda #1
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

    jsr WaitForNMI

    lda #%0001_1110
    sta $2001

Frame_Menu:
    jsr ReadControllers

    lda #BUTTON_DOWN
    jsr ButtonPressedP1
    beq :+
    inc MenuSelection
    lda MenuSelection
    cmp #3
    bcc :+
    lda #0  ; handle overflow
    sta MenuSelection
:
    lda #BUTTON_UP
    jsr ButtonPressedP1
    beq :+
    dec MenuSelection
    lda MenuSelection
    bpl :+
    lda #2
    sta MenuSelection
:

    lda #BUTTON_A
    jsr ButtonPressedP1
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

    jmp Frame_Menu

ControllerLabels:
    .asciiz "Player 1"
    .asciiz "Player 2"
    .asciiz "Player 3"
    .asciiz "Player 4"

ControllerLabelAddrs:
    .word $2084
    .word $2090
    .word $2204
    .word $2210

ControllerTiles:
    ;.byte 

DrawController:
    ; Start address in AddressPointer

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
    sta $2006
    lda ControllerLabelAddrs+0, x
    sta $2006

@txtLoop:
    lda ControllerLabels, y
    beq @next
    iny
    sta $2007
    jmp @txtLoop

@next:
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
    ; Sprite
    .byte $0F, $20, $10, $00
    .byte $0F, $20, $10, $00
    .byte $0F, $20, $10, $00
    .byte $0F, $20, $10, $00

    ; Sprite
    .byte $0F, $20, $10, $00
    .byte $0F, $20, $10, $00
    .byte $0F, $20, $10, $00
    .byte $0F, $20, $10, $00
