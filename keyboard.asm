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

Init_Keyboard:
    lda #%1000_0000
    sta PPU_2000
    sta $2000

    lda #0
    sta $2001

    lda #$20
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
    and #$F0
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
