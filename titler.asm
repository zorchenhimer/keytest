.pushseg
.segment "BSS"
titler_ButtonsA: .res 1
titler_ButtonsB: .res 1
titler_frame: .res 1

.popseg

Titler_DataStart = $20A6

; addresses for each button on screen.
TITLER_BTN_PPU_START = $2106
Titler_Buttons_PPU:
    .repeat 5, i
        .word TITLER_BTN_PPU_START+(i*2)
    .endrepeat

    .repeat 5, i
        .word TITLER_BTN_PPU_START+64+(i*2)
    .endrepeat

    .repeat 6, i
        .word TITLER_BTN_PPU_START+128+(i*2)
    .endrepeat
TITLER_BTN_COUNT = (* - Titler_Buttons_PPU)/2

; Buttons
; ByteA
TBTN_5           = $80
TBTN_Execute     = $40
TBTN_Handwritten = $20
TBTN_Insert      = $10
TBTN_3           = $08
TBTN_2           = $04
TBTN_Inverted    = $02
TBTN_PrevScreen  = $01

; ByteB
TBTN_Delete     = $80
TBTN_Kanji      = $40
TBTN_Symbols    = $20
TBTN_TextColor  = $10
TBTN_4          = $08
TBTN_1          = $04
TBTN_Typeface   = $02
TBTN_Menu       = $01

; Physical Button Layout:
; $0004  $0400  $0800  $0008  $8000
; $0002  $0200  $0010  $1000  $0080
; $0001  $0100  $0020  $2000  $0040 $4000

; Button statuses are read one bit at a time from
; $4016.1 for 16 bits of data, MSB first.
; Two sets of strobes are required for each byte of data.
; For the first byte:
;   $00, $02, $00
;   $00, $01, $00
; and for the second byte:
;   $00, $04, $00
;   $00, $01, $00
; These strobes are written out to $4016.

; a "Word" table of masks, in physical order
Titler_ButtonMasks:
    .byte $00, TBTN_1
    .byte TBTN_2, $00
    .byte TBTN_3, $00
    .byte $00, TBTN_4
    .byte TBTN_5, $00

    .byte $00, TBTN_Typeface
    .byte TBTN_Inverted, $00
    .byte $00, TBTN_TextColor
    .byte TBTN_Insert, $00
    .byte $00, TBTN_Delete

    .byte $00, TBTN_Menu
    .byte TBTN_PrevScreen, $00
    .byte $00, TBTN_Symbols
    .byte TBTN_Handwritten, $00
    .byte $00, TBTN_Kanji
    .byte TBTN_Execute, $00

Init_Titler:
    lda #%1000_0000
    sta PPU_2000
    sta $2000

    lda #0
    sta $2001

    lda #$20
    jsr ClearScreen

    ldx #0
    ldy #0
    ;sta BufferIndex
@loop:
    tya
    asl a
    tax

    lda Titler_Buttons_PPU+0, x
    sta Buffer_AddrLo, y

    lda Titler_Buttons_PPU+1, x
    sta Buffer_AddrHi, y

    lda #0
    sta Buffer_Data, y
    inc BufferIndex

    iny
    cpy #TITLER_BTN_COUNT+1
    bne @loop

; setup a frame counter
    lda #0
    sta titler_frame

    ; 192, 16
    lda #16
    sta Sprites+(0*4)+0
    sta Sprites+(1*4)+0

    lda #192
    sta Sprites+(0*4)+3
    lda #192+8
    sta Sprites+(1*4)+3

    lda #0
    sta Sprites+(0*4)+2
    sta Sprites+(1*4)+2

    lda #'0'
    sta Sprites+(0*4)+1
    sta Sprites+(1*4)+1

    jsr WaitForNMI
    lda #%0001_1110
    sta $2001

Frame_Titler:

    jsr titler_ReadButtons

    lda #0
    sta TmpX
    sta TmpY
    jmp @ascii

    ldx #$00
    ldy #$02
    stx $4016
    sty $4016
    stx $4016

    lda #$04
    sta $4016

    jsr WaitForX
    sta TmpX

    ldx #$00
    lda #$04
    sta $4016
    jsr WaitForX
    sta TmpY

@ascii:
    inc titler_frame
    lda titler_frame
    and #$0F
    tax
    lda HexAscii, x
    sta Sprites+(1*4)+1

    lda titler_frame
    lsr a
    lsr a
    lsr a
    lsr a
    tax
    lda HexAscii, x
    sta Sprites+(0*4)+1

    ldx #0
@btnLoop:
    txa
    asl a
    tay

    lda Titler_ButtonMasks+0, y
    and titler_ButtonsA
    bne @on

    lda Titler_ButtonMasks+1, y
    and titler_ButtonsB
    bne @on
    lda #$00
    sta TmpX

@next:
    lda Titler_Buttons_PPU+0, y
    sta Buffer_AddrLo, x

    lda Titler_Buttons_PPU+1, y
    sta Buffer_AddrHi, x

    lda TmpX
    sta Buffer_Data, x
    inc BufferIndex

    inx
    cpx #TITLER_BTN_COUNT+1
    bne @btnLoop
    jmp @btnDone

@on:
    lda #$80
    sta TmpX
    jmp @next

@btnDone:

    ldy BufferIndex
    lda titler_ButtonsA
    lsr a
    lsr a
    lsr a
    lsr a
    tax
    lda HexAscii, x
    sta Buffer_Data, y

    lda #.lobyte(Titler_DataStart+0)
    sta Buffer_AddrLo, y
    lda #.hibyte(Titler_DataStart+0)
    sta Buffer_AddrHi, y

    inc BufferIndex
    iny

    lda titler_ButtonsA
    and #$0F
    tax
    lda HexAscii, x
    sta Buffer_Data, y

    lda #.lobyte(Titler_DataStart+1)
    sta Buffer_AddrLo, y
    lda #.hibyte(Titler_DataStart+1)
    sta Buffer_AddrHi, y

    inc BufferIndex
    iny

    ;lda TmpY
    lda titler_ButtonsB
    lsr a
    lsr a
    lsr a
    lsr a
    tax
    lda HexAscii, x
    sta Buffer_Data, y

    lda #.lobyte(Titler_DataStart+3)
    sta Buffer_AddrLo, y
    lda #.hibyte(Titler_DataStart+3)
    sta Buffer_AddrHi, y

    inc BufferIndex
    iny

    lda titler_ButtonsB
    and #$0F
    tax
    lda HexAscii, x
    sta Buffer_Data, y

    lda #.lobyte(Titler_DataStart+4)
    sta Buffer_AddrLo, y
    lda #.hibyte(Titler_DataStart+4)
    sta Buffer_AddrHi, y

    inc BufferIndex

    jsr WaitForNMI
    jmp Frame_Titler

WaitForX:
    cli
@loop:
    cpx #$00
    beq @loop
    sei
    rts

titler_ReadButtons:
    lda #$00
    sta titler_ButtonsA
    sta titler_ButtonsB

    ; strobe buttons
    ldy #$02
    sta $4016
    sty $4016
    sta $4016

    ldx #$FF
@outer:
    inx
    lda #$00
    ldy #$01
    sta $4016
    sty $4016
    sta $4016

    ldy #8
@inner:
    lda titler_ButtonsA, x
    asl a
    sta titler_ButtonsA, x

    lda $4016
    lsr a
    and #$01
    ora titler_ButtonsA, x
    sta titler_ButtonsA, x

    ;rol titler_ButtonsB
    ;rol titler_ButtonsA
    dey
    bne @inner
    lda #$00
    ldy #$04
    sta $4016
    sty $4016
    sta $4016
    cpx #1
    bne @outer

    rts
