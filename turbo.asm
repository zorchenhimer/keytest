.scope Turbo

.pushseg
.segment "BSS"
Filename: .res 16
AddrBuffer: .res 5
DataBufferA: .res 32
DataBufferB: .res 16
DataBufferC: .res 16

ReadErrors: .res 2
StateText: .res 10
NextPattern: .res 1

.popseg

Init:
    lda #%1000_0000
    sta PPU_2000
    sta $2000

    lda #0
    sta $2001

    lda #$FF
    sta BufferIndex

    lda #$20
    jsr ClearScreen

    macDrawText_Direct textA, $2065
    macDrawText_Direct textB, $2065+32

    macDrawText_Direct text_Write, $2078
    macDrawText_Direct text_Read, $2079

    lda #.lobyte($23C6)
    sta $2006
    lda #.hibyte($23C6)
    sta $2006

    lda Attr_None
    sta $2007

;    ldx #.addrsize(Buffer_Data)-1
;    lda #0
;@loop:
;    sta Buffer_Data, y
;    dex
;    bpl @loop

;    jsr WaitForNMI
    lda #%0001_1110
    sta $2001
    jsr WaitForNMI

    lda #$00
    sta AddressPointer3+0
    sta AddressPointer3+1

    jsr ReadFile

    jsr ResetAddr
    jsr WaitForNMI

Frame:
    jsr ReadControllers
    lda controllers_pressed+0
    and #BUTTON_A
    beq :+
    jsr ReadData
    jmp @frameDone

:   lda controllers_pressed+0
    and #BUTTON_B
    beq :+
    jsr ResetAddr
    jmp @frameDone

:   lda controllers_pressed+0
    and #BUTTON_START
    beq :+
    ;jsr WriteTest
    jsr PatternTest
    jmp @frameDone
:

@frameDone:

    lda #.lobyte($23C6)
    sta Buffer_AddrLo+0
    lda #.hibyte($23C6)
    sta Buffer_AddrHi+0

    lda Attr_None
    sta Buffer_Data+0
    lda #0
    sta BufferIndex

    lda #NMI_Action::UnrolledBytes
    sta NMIAction
    jsr WaitForNMI
    jmp Frame

PatternTest:

    jsr WaitForNMI

    lda #%1000_0000
    sta PPU_2000
    sta $2000

    lda #0
    sta $2001

    lda #$20
    jsr ClearScreen

    macDrawText_Direct text_PatternAA, $2065

    lda #%0001_1110
    sta $2001
    jsr WaitForNMI

    lda #0
    sta ReadErrors+0
    sta ReadErrors+1
    sta Bin_Input+0
    sta Bin_Input+1
    sta Bin_Input+2
    jsr BinToDec

    lda #$00
    sta $4016
    lda #$02
    sta $4016

    lda #$00
    sta AddressPointer3+0
    sta AddressPointer3+1

    ;jsr ResetAddr
    ;macDrawText text_Writing, $20E5
    ;jsr WaitForNMI

    ldx #0
:
    lda text_Writing, x
    beq :+
    sta StateText, x
    inx
    jmp :-
:

    lda #.lobyte(PatternNMI)
    sta nmi_custom+0
    lda #.hibyte(PatternNMI)
    sta nmi_custom+1

    lda #NMI_Action::Pointer
    sta NMIAction

    jsr WaitForNMI

@writeAA:
    lda #$AA
    jsr WriteByte

    inc AddressPointer3+0
    bne :+
    inc AddressPointer3+1
:

    lda AddressPointer3+1
    cmp #$20
    bne @writeAA
    lda AddressPointer3+0
    bne @writeAA


    lda #$00
    sta $4016
    lda #$02
    sta $4016

    lda #$00
    sta AddressPointer3+0
    sta AddressPointer3+1
    ;jsr ResetAddr
    jsr ReadByte ; read past first address
    inc AddressPointer3+0

    ldx #0
:
    lda text_Reading, x
    beq :+
    sta StateText, x
    inx
    jmp :-
:

    lda #0
    sta ReadErrors+0
    sta ReadErrors+1
@readAA:
    jsr ReadByte
    cmp #$AA
    beq :+
    jsr IncError
:

    inc AddressPointer3+0
    bne :+
    inc AddressPointer3+1
:

    lda AddressPointer3+1
    cmp #$20
    bne @readAA
    lda AddressPointer3+0
    bne @readAA

    ldx #0
:
    lda text_Done, x
    beq :+
    sta StateText, x
    inx
    jmp :-
:

    ldx #0
:
    lda text_Writing, x
    beq :+
    sta StateText, x
    inx
    jmp :-
:

    lda #$FF
    sta NextPattern

    lda #$00
    sta $4016
    lda #$02
    sta $4016

    lda #$00
    sta AddressPointer3+0
    sta AddressPointer3+1

@write55:
    lda #$55
    jsr WriteByte

    inc AddressPointer3+0
    bne :+
    inc AddressPointer3+1
:

    lda AddressPointer3+1
    cmp #$20
    bne @write55
    lda AddressPointer3+0
    bne @write55


    lda #$00
    sta $4016
    lda #$02
    sta $4016

    lda #$00
    sta AddressPointer3+0
    sta AddressPointer3+1
    ;jsr ResetAddr
    jsr ReadByte ; read past first address
    inc AddressPointer3+0

    ldx #0
:
    lda text_Reading, x
    beq :+
    sta StateText, x
    inx
    jmp :-
:

    ;lda #0
    ;sta ReadErrors+0
    ;sta ReadErrors+1
@read55:
    jsr ReadByte
    cmp #$55
    beq :+
    jsr IncError
:

    inc AddressPointer3+0
    bne :+
    inc AddressPointer3+1
:

    lda AddressPointer3+1
    cmp #$20
    bne @read55
    lda AddressPointer3+0
    bne @read55

    ldx #0
:
    lda text_Done, x
    beq :+
    sta StateText, x
    inx
    jmp :-
:

    jsr WaitForNMI

    lda NMI_Action::Nothing
    sta NMIAction
    jsr WaitForNMI
    rts

IncError:
    inc ReadErrors+0
    bne :+
    inc ReadErrors+1
:

    lda ReadErrors+0
    sta Bin_Input+0
    lda ReadErrors+1
    sta Bin_Input+1
    lda #0
    sta Bin_Input+2
    jsr BinToDec

    rts

PatternNMI:
    lda NextPattern
    bpl @skipTitle
    lda #0
    sta NextPattern

    lda #.hibyte(patternAddr_Title)
    sta $2006
    lda #.lobyte(patternAddr_Title)
    sta $2006

    ldx #0
:
    lda text_Pattern55, x
    beq :+
    sta $2007
    inx
    jmp :-
:

@skipTitle:

    lda #.hibyte(patternAddr_State)
    sta $2006
    lda #.lobyte(patternAddr_State)
    sta $2006

    ldx #0
:
    lda StateText, x
    sta $2007
    inx
    cpx #10
    bne :-

    lda #' '
    sta $2007
    lda #'$'
    sta $2007

    lda AddressPointer3+1
    lsr a
    lsr a
    lsr a
    lsr a
    tax
    lda HexAscii, x
    sta $2007

    lda AddressPointer3+1
    and #$0F
    tax
    lda HexAscii, x
    sta $2007

    lda AddressPointer3+0
    lsr a
    lsr a
    lsr a
    lsr a
    tax
    lda HexAscii, x
    sta $2007

    lda AddressPointer3+0
    and #$0F
    tax
    lda HexAscii, x
    sta $2007

    lda #.hibyte(patternAddr_Errors)
    sta $2006
    lda #.lobyte(patternAddr_Errors)
    sta $2006

    ldx #0
:
    lda text_Errors, x
    beq :+
    inx
    sta $2007
    jmp :-
:

    ldx #0
:
    lda Bin_Tiles, x
    sta $2007
    inx
    cpx #.sizeof(::Bin_Tiles)
    bne :-

    rts

ResetAddr:
    lda #$00
    sta $4016
    lda #$02
    sta $4016

    lda #$00
    sta AddressPointer3+0
    sta AddressPointer3+1

    jsr AddrToAscii

    macDrawText AddrBuffer, $20C5

    rts

ReadData:
;    lda #$00
;    sta $4016
;    lda #$02
;    sta $4016
;
;    ; dummy reads up to current address
;    lda AddressPointer3+0
;    sta AddressPointer+0
;    lda AddressPointer3+1
;    sta AddressPointer+1
;
;@dummy:
;    lda AddressPointer+0
;    bne :+
;    lda AddressPointer+1
;    beq @dummyDone
;:
;    jsr ReadByte
;
;    sec
;    lda AddressPointer+0
;    sbc #1
;    sta AddressPointer+0
;    lda AddressPointer+1
;    sbc #0
;    sta AddressPointer+1
;
;    jmp @dummy
;
;@dummyDone:

    lda #$00
    sta TmpX

    ldy #0
@loop:
    jsr ReadByte
    sta DataBufferA, y
    iny
    cpy #16
    bne @loop

    jsr DataToAscii

    macDrawText DataBufferA, $2105
    macDrawText DataBufferA+17, $2105+32

    jsr AddrToAscii
    macDrawText AddrBuffer, $20C5

    clc
    lda AddressPointer3+0
    adc #16
    sta AddressPointer3+0
    lda AddressPointer3+1
    adc #0
    sta AddressPointer3+1

    jmp WaitForNMI
    ;rts

AddrToAscii:
    lda AddressPointer3+1
    lsr a
    lsr a
    lsr a
    lsr a
    tax
    lda HexAscii, x
    sta AddrBuffer+0

    lda AddressPointer3+1
    and #$0F
    tax
    lda HexAscii, x
    sta AddrBuffer+1

    lda AddressPointer3+0
    lsr a
    lsr a
    lsr a
    lsr a
    tax
    lda HexAscii, x
    sta AddrBuffer+2

    lda AddressPointer3+0
    and #$0F
    tax
    lda HexAscii, x
    sta AddrBuffer+3

    lda #00
    sta AddrBuffer+4
    rts

ReadByte:
    .repeat 8
    lda $4017 ; read
    and #$04
    lsr a
    lsr a
    ora #$06
    sta $4016

    and #%1111_1011
    sta $4016

    lsr a
    ror TmpX  ; store
    .endrepeat

    lda TmpX
    rts

DataToAscii:
    ldy #0
@loop:
    lda DataBufferA, y
    lsr a
    lsr a
    lsr a
    lsr a
    and #$0F
    tax
    lda HexAscii, x
    sta DataBufferB, y

    lda DataBufferA, y
    and #$0F
    tax
    lda HexAscii, x
    sta DataBufferC, y
    iny
    cpy #16
    bne @loop

    ldy #0
    ldx #0
@loop2:
    lda DataBufferB, y
    sta DataBufferA, x
    inx
    lda DataBufferC, y
    sta DataBufferA, x
    inx
    iny
    cpy #8
    bne :+
    lda #0
    sta DataBufferA, x
    inx
:
    cpy #16
    bne @loop2

    lda #0
    sta DataBufferA, x

    rts

WriteTest:
    jsr WaitForNMI

    lda #.lobyte($23C6)
    sta Buffer_AddrLo+0
    lda #.hibyte($23C6)
    sta Buffer_AddrHi+0

    lda Attr_Write
    sta Buffer_Data+0
    lda #0
    sta BufferIndex

    lda #NMI_Action::UnrolledBytes
    sta NMIAction
    jsr WaitForNMI

    ; reset address
    lda #$00
    sta $4016
    lda #$02
    sta $4016

    ; get past first address
    ldx #8
    ldy #$06
    lda #$02
:
    sty $4016
    sta $4016
    dex
    bne :-

    ;lda #.lobyte(TestFile_Size)
    ;sta AddressPointer+0
    ;lda #.hibyte(TestFile_Size)
    ;sta AddressPointer+1

    lda #.lobyte(TestFile)
    sta AddressPointer+0
    lda #.hibyte(TestFile)
    sta AddressPointer+1

    ldy #0
@loop:
    ;lda TestData, y
    lda (AddressPointer), y
    jsr WriteByte

    inc AddressPointer+0
    bne :+
    inc AddressPointer+1
:
    lda AddressPointer+1
    cmp #.hibyte(TestFile+TestFile_Size)
    bne @loop
    lda AddressPointer+0
    cmp #.lobyte(TestFile+TestFile_Size)
    bne @loop

    jsr WaitForNMI

    lda #.lobyte($23C6)
    sta Buffer_AddrLo+0
    lda #.hibyte($23C6)
    sta Buffer_AddrHi+0

    lda Attr_None
    sta Buffer_Data+0
    lda #0
    sta BufferIndex

    lda #NMI_Action::UnrolledBytes
    sta NMIAction
    jsr WaitForNMI

    jsr ResetAddr
    jsr ReadData

    rts

WriteByte:
    sta TmpX

    ldx #8
@loop:
    lda TmpX
    and #$01
    lsr TmpX

    ora #$06
    sta $4016
    and #%1111_1011
    sta $4016

    dex
    bne @loop
    rts

ReadFile:
    jsr WaitForNMI

    lda #.lobyte($23C6)
    sta Buffer_AddrLo+0
    lda #.hibyte($23C6)
    sta Buffer_AddrHi+0

    lda Attr_Read
    sta Buffer_Data+0
    lda #0
    sta BufferIndex

    lda #NMI_Action::UnrolledBytes
    sta NMIAction
    jsr WaitForNMI

    jsr ResetAddr
    jsr ReadByte ; addr $0000

    jsr ReadByte
    cmp #'A'
    beq :+
    rts
:
    jsr ReadByte
    cmp #'B'
    beq :+
    rts
:

    jsr ReadByte
    sta AddressPointer3+0
    jsr ReadByte
    sta AddressPointer3+1
    jsr AddrToAscii

    macDrawText AddrBuffer, $21E5

    sec
    lda AddressPointer3+0
    sbc #18
    sta AddressPointer3+0
    lda AddressPointer3+1
    sbc #0
    sta AddressPointer3+1

    ldx #0
@loop:
    jsr ReadByte
    beq @done
    cmp #$01
    beq @done

    sta DataBufferA, x
    inx
    cpx #16
    bne @loop
@done:

    lda #0
    sta DataBufferA, x

    macDrawText DataBufferA, $2205

    jsr WaitForNMI

    lda #0
    sta AddressPointer+0
    sta AddressPointer+1
@checksum:
    jsr ReadByte
    ; calculate checksum
    clc
    adc AddressPointer+0
    sta AddressPointer+0
    lda AddressPointer+1
    adc #0
    sta AddressPointer+1

    ; decrement length
    sec
    lda AddressPointer3+0
    sbc #1
    sta AddressPointer3+0
    lda AddressPointer3+1
    sbc #0
    sta AddressPointer3+1

    cmp #$00
    bne @checksum
    lda AddressPointer3+0
    cmp #$00
    bne @checksum

    ; read checksum
    jsr ReadByte
    sta AddressPointer2+0
    jsr ReadByte
    sta AddressPointer2+1

    ; validate checksum
    cmp AddressPointer+1
    bne @fail
    lda AddressPointer+0
    cmp AddressPointer2+0
    bne @fail

    macDrawText text_Ok, $2225
    jmp :+
@fail:
    macDrawText text_NotOk, $2225
:

    lda #NMI_Action::String
    sta NMIAction
    jsr WaitForNMI

    lda #.lobyte($23C6)
    sta Buffer_AddrLo+0
    lda #.hibyte($23C6)
    sta Buffer_AddrHi+0

    lda Attr_Read
    sta Buffer_Data+0
    lda #0
    sta BufferIndex

    lda #NMI_Action::UnrolledBytes
    sta NMIAction
    jmp WaitForNMI
    ;rts

textA:
    .asciiz "Press A to read"
textB:
    .asciiz "Press B to reset"

text_Write:
    .byte 'W', $00
text_Read:
    .byte 'R' | $80, $00
text_Ok:
    .asciiz "Checksum OK"
text_NotOk:
    .asciiz "Checksum FAILED"

text_PatternAA:
    .asciiz "Pattern test AA"
text_Pattern55:
    .asciiz "Pattern test 55"
text_Writing:
    .asciiz "Writing..."
text_Reading:
    .asciiz "Reading..."
text_Done:
    .asciiz "Done      "
text_Blank:
    .asciiz "    "
text_Errors:
    .asciiz "Errors: "

patternAddr_Title   = $2065
patternAddr_State   = patternAddr_Title + 32
patternAddr_Address = patternAddr_State + 11
patternAddr_Errors  = patternAddr_Title + 64

Attr_None:
    .byte $FF

Attr_Write:
    .byte $55
Attr_Read:
    .byte $AA

TestData:
    .asciiz "01234567"

TestFile:
    .incbin "turbo.file.bin"
TestFile_Size = * - TestFile

.endscope
