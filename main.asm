
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

MENU_PPU_ADDR = $2089

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
PPU_2000: .res 1

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
GlassesToggle: .res 1

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

    ;lda #%1000_0000
    lda PPU_2000
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

WaitForNMI:
    jsr PrepareNmiPointer
:   bit Sleeping
    bpl :-
    lda #0
    sta Sleeping
    rts

; Menu to select what type of input device
DrawMenu:
    lda #.hibyte(MENU_PPU_ADDR)
    sta AddressPointer+1
    sta $2006
    lda #.lobyte(MENU_PPU_ADDR)
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

; Nametable High byte in A
ClearScreen:
    bit $2002
    ;lda #$20
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
    sta PPU_2000
    sta $2000

    ;lda #%0001_1110
    lda #0
    sta $2001

    jsr WaitForNMI
    jsr WaitForNMI

    ; TODO: palatte

    lda #$20
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

HexAscii:
    .byte "0123456789ABCDEF"

MenuItems:
    .asciiz "Controllers"
    .asciiz "Keyboard"
    .asciiz "Family Trainer"
    .asciiz "Hori Track"
    .asciiz "3D Glasses"
    .asciiz "Oeka Tablet"

MenuRows:
    ;      Y, X
    .repeat 6, i
    .byte 31+(i*16), 56
    .endrepeat
MenuItemCount = (* - MenuRows) / 2

MenuDestinations:
    .word Init_Controllers
    .word Init_Keyboard
    .word Init_Feet
    .word Init_Trackball
    .word Init_3DGlasses
    .word Init_Tablet

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

    .include "3d-glasses.asm"
    .include "feet.asm"
    .include "keyboard.asm"
    .include "standard-controllers.asm"
    .include "trackball.asm"
    .include "tablet.asm"
