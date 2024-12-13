;;; ============================================================
;;; Disassembly of "Double-Duty DOS" by Jason Coleman
;;; From Compute! Magazine, Issue 89 (October 1987)
;;; ============================================================

        .setcpu "6502"
        .org $3000

        .include "apple2.inc"
        .include "apple2.mac"
        .include "prodos.inc"

INPUT_BUFFER    := $200
UPPERCASE_MASK  := $DF

;;; Monitor ROM entry points
CR              := $FC62
COUT            := $FDED
MOVE            := $FE2C

;;; Zero Page Locations
STARTLO         := $3C
STARTHI         := $3D
ENDLO           := $3E
ENDHI           := $3F
DESTINATIONLO   := $42
DESTINATIONHI   := $43

;;; ============================================================
;;; Entry Point
;;; ============================================================

;;; Request buffer from BASIC Interpreter

        lda     #$0A            ; number of pages
        jsr     GETBUFR
        bcc     :+
        lda     #BI_ERR_PROGRAM_TOO_LARGE
        jmp     ERROUT
:
        ;; Build up relocation table
        tax                     ; X = high byte of buffer
        ldy     #$00
:       txa
        sta     reloc_target_page_table,y
        inx
        iny
        cpy     #$07            ; pages to relocate???
        bcc     :-

        stx     L38DB
        stx     L38F0
        inx
        txa
        sta     reloc_target_page_table,y
        inx
        iny
        txa
        sta     reloc_target_page_table,y
        ldy     #$00
L302C:  lda     ExternalCommand,y
        ldx     #$0
L3031:  cmp     reloc_start_page_table,x
        beq     L304D
        inx
        cpx     #$09
        bcc     L3031
L3038:  iny
        bne     L302C
        inc     L302C+2         ; page
        inc     L3050+2
        lda     L302C+2
        cmp     #$3B            ; past last page to relocate
        bcc     L302C
        bcs     L3067           ; always

L304D:  lda     reloc_target_page_table,x
L3050:  sta     ExternalCommand,y
        bne     L3038

;;; Source pages ???
reloc_start_page_table:
        .byte   $31, $32, $33, $34, $35, $36, $37, $39, $3A

;;; Target pages ???
reloc_target_page_table:
        .res    9, 0

;;; Move into target buffer and hook into BI CMD syntaxing
L3067:  lda     #>ExternalCommand
        sta     STARTHI
        lda     #$37            ; ???
        sta     L36B7           ; ???
        lda     EXTRNCMD+1
        sta     extrncmd_hook+1
        lda     EXTRNCMD+2
        sta     extrncmd_hook+2
        lda     #>reloc_end
        sta     ENDHI
        ldy     #<reloc_end
        sty     ENDLO
        .assert <reloc_end = $FF, error, "bad optimization"
        iny                     ; Y = 0
        sty     STARTLO
        sty     DESTINATIONLO
        sty     EXTRNCMD+1
        lda     reloc_target_page_table
        sta     DESTINATIONHI
        sta     EXTRNCMD+2
        jsr     MOVE

;;; Print message
        ldy     #$00
:       lda     message,y
        beq     :+
        jsr     COUT
        iny
        bne     :-
:
        rts

message:
        scrcode "DCAT, DSAVE, AND DLOAD NOW INSTALLED"
        .byte   $8D
        scrcode "WRITTEN BY JASON COLEMAN, JULY, 1986"
        .byte   $8D
        .byte   $8D

        ;; Pad to page boundary
        .res    $3100 - *, 0

;;; ============================================================
;;; External Command Implementation
;;; ============================================================

ExternalCommand:
        cld                     ; Expected by ProDOS

        ;; DCAT?
        ldy     #0
        ldx     #0
:       lda     INPUT_BUFFER,y
        iny
        cmp     #' '|$80        ; skip whitespace
        beq     :-

        and     #UPPERCASE_MASK
        cmp     dcat_cmd_str,x
        bne     :+
        inx
        cpx     #.strlen("DCAT")
        beq     DoDCAT
        bne     :-              ; always

dcat_cmd_str:
        scrcode "DCAT"

:
        ;; DLOAD?
        ldy     #0
        ldx     #0
:       lda     INPUT_BUFFER,y
        iny
        cmp     #' '|$80        ; skip whitespace
        beq     :-

        and     #UPPERCASE_MASK
        cmp     dload_cmd_str,x
        bne     :+
        inx
        cpx     #.strlen("DLOAD")
        bne     :-
        jmp     DoDLOAD
:
        ;; DSAVE?
        ldy     #0
        ldx     #0
:       lda     INPUT_BUFFER,y
        iny
        cmp     #' '|$80        ; skip whitespace
        beq     :-

        and     #UPPERCASE_MASK
        cmp     dsave_cmd_str,x
        bne     :+
        inx
        cpx     #.strlen("DSAVE")
        bne     :-
        jmp     DoDSAVE

:
        ;; Failed to match
        sec
extrncmd_hook:
        jmp     $0000

dsave_cmd_str:
        scrcode "DSAVE"

dload_cmd_str:
        scrcode "DLOAD"

;;; ============================================================
;;; DCAT Command
;;; ============================================================

DoDCAT:
        ;; Ask BASIC.SYSTEM to parse command line for us
        .assert .strlen("DCAT") = PBitsFlags::SD, error, "mismatch"
        stx     PBITS+1
        dex
        stx     XLEN
        lda     #>DCATImpl
        sta     XTRNADDR+1
        lda     #<DCATImpl
        sta     XTRNADDR
        lda     #$00
        sta     XCNUM
        lda     #PBitsFlags::FNOPT
        sta     PBITS
        clc
        rts

        ;; Interpret parameters
DCATImpl:
        lda     VSLOT
        sta     DEFSLT
        asl     a
        asl     a
        asl     a
        asl     a
        ldx     VDRIV
        stx     DEFDRV
        dex
        beq     :+
        ora     #$80
:
        sta     L3984
        lda     #$80
        sta     L3983
        lda     #$11
        ldy     #$00
        sta     L3987
        sty     L3988
        lda     $74
        sta     L3986
        lda     #$00
        sta     L3985
        ldy     #$39
        lda     #$83
        jsr     L3918
        bcc     L31BD
        jmp     L397F

L31BD:  jsr     CR
        ldy     #$06
        lda     ($73),y
        sta     $02
        ldy     #$0A
L31C8:  lda     L329C,y
        jsr     COUT
        dey
        bpl     L31C8
        lda     #$A0
        jsr     COUT
        lda     $02
        jsr     L32A7
        jsr     CR
        jsr     CR
        ldy     #$0F
        sty     L3988
L31E6:  ldy     #$39
        lda     #$83
        jsr     L3918
        bcc     L31F2
        jmp     L397F

L31F2:  lda     $74
        sta     $01
        lda     $73
        sta     $00
        ldy     #$0B
L31FC:  sty     $03
        lda     ($00),y
        bne     L3205
        jmp     L3292

L3205:  cmp     #$FF
        beq     L327F
        iny
        iny
        lda     ($00),y
        bmi     L3217
        lda     #$A0
        jsr     COUT
        jmp     L321C

L3217:  lda     #$AA
        jsr     COUT
L321C:  lda     ($00),y
        and     #$7F
        beq     L3230
        cmp     #$01
        beq     L3234
        cmp     #$02
        beq     L3238
        cmp     #$04
        beq     L323C
        bne     L327F
L3230:  lda     #$D4
        bne     L323E
L3234:  lda     #$C9
        bne     L323E
L3238:  lda     #$C1
        bne     L323E
L323C:  lda     #$C2
L323E:  jsr     COUT
        lda     #$A0
        jsr     COUT
        lda     $03
        clc
        adc     #$21
        tay
        lda     ($00),y
        jsr     L32A7
        lda     #$A0
        jsr     COUT
        ldy     $03
        iny
        iny
        iny
        ldx     #$00
L325D:  lda     ($00),y
        jsr     COUT
        iny
        inx
        cpx     #$1D
        bne     L325D
        jsr     CR
        lda     $C000
        bpl     L327F
        lda     #$00
        sta     $C010
L3275:  lda     $C000
        bpl     L3275
        lda     #$00
        sta     $C010
L327F:  lda     $03
        clc
        adc     #$23
        tay
        bcs     L328A
        jmp     L31FC

L328A:  dec     L3988
        beq     L3292
        jmp     L31E6

L3292:  jsr     CR
        jsr     CR
        clc
        lda     #$00
        rts

L329C:  cmp     $CD
        cmp     $CC,x
        .byte   $CF
        dec     $A0,x
        .byte   $CB
        .byte   $D3
        cmp     #$C4
L32A7:  sta     L32CF
        ldy     #$02
L32AC:  lda     #$00
        pha
L32AF:  lda     L32CF
        cmp     L32D1,y
        bcc     L32C5
        sbc     L32D1,y
        sta     L32CF
        pla
        clc
        adc     #$01
        pha
        jmp     L32AF

L32C5:  pla
        ora     #$B0
        jsr     COUT
        dey
        bpl     L32AC
        rts

L32CF:  brk
        brk
L32D1:  ora     ($0A,x)
        .byte   $64

;;; ============================================================
;;; DLOAD Command
;;; ============================================================

DoDLOAD:
        ;; Parse command buffer
        ldx     #$00
L32D6:  lda     INPUT_BUFFER,y
        iny
        cmp     #$8D            ; CR
        beq     L32FA
        cmp     #' '|$80
        beq     L32D6
L32E2:  cmp     #$8D            ; CR
        beq     L32FC
        cmp     #','|$80
        beq     L32FC
        cmp     #$E0            ; lowercase?
        bcc     L32F0
        and     #UPPERCASE_MASK
L32F0:  sta     INPUT_BUFFER+1,x
        inx
        lda     INPUT_BUFFER,y
        iny
        bne     L32E2
L32FA:  sec
        rts

L32FC:  cpx     #$1F
        bcc     L3302
        ldx     #$1E
L3302:  stx     INPUT_BUFFER
        dey
        sty     XLEN
        lda     #<DLOADImpl
        sta     XTRNADDR
        lda     #>DLOADImpl
        sta     XTRNADDR+1
        lda     #$00
        sta     XCNUM
        lda     #PBitsFlags::FNOPT
        sta     PBITS
        lda     #PBitsFlags::PFIX | PBitsFlags::T
        sta     PBITS+1
        clc
        rts

DLOADImpl:
        lda     VSLOT
        sta     DEFSLT
        asl     a
        asl     a
        asl     a
        asl     a
        ldx     VDRIV
        stx     DEFDRV
        dex
        beq     :+
        ora     #$80
:
        sta     L3984
        lda     $74
        sta     L3986
        lda     #$00
        sta     L3985
        lda     #$11
        sta     L3987
        lda     #$0F
        sta     L3988
L3350:  ldy     #$39
        lda     #$83
        jsr     L3918
        bcc     L335C
        jmp     L397F

L335C:  lda     #$0E
L335E:  tay
        ldx     #$00
        sty     $02
L3363:  lda     ($73),y
        beq     ErrPathNotFound
        cmp     INPUT_BUFFER+1,x
        bne     L3375
        inx
        cpx     INPUT_BUFFER
        beq     L338B
        iny
        bne     L3363
L3375:  lda     $02
        clc
        adc     #$23
        bcc     L335E
        ldy     L3988
        dey
        beq     ErrPathNotFound
        sty     L3988
        bne     L3350

ErrPathNotFound:
        lda     #BI_ERR_PATH_NOT_FOUND
        sec
        rts

L338B:  cpx     #$1E
        beq     L3399
        iny
        lda     ($73),y
        cmp     #$A0
        bne     L3375
        inx
        bne     L338B
L3399:  ldy     $02
        dey
        dey
        dey
        lda     ($73),y
        bmi     ErrPathNotFound
        sta     L3987
        iny
        lda     ($73),y
        sta     L3988
        ldx     L3986
        inx
        inx
        stx     L3986
        stx     $07
        ldx     #$00
        stx     $06
        iny
        lda     ($73),y
        and     #$7F
        cmp     #$02
        beq     L33CD
        cmp     #$04
        bne     L33C9
        jmp     L3490

L33C9:  sec
        lda     #$0D
        rts

L33CD:  lda     #$83
        ldy     #$39
        jsr     L3918
        bcc     L33D9
        jmp     L397F

L33D9:  lda     $67
        ldy     $68
        sec
        sbc     #$02
        bcs     L33E3
        dey
L33E3:  sta     L3985
        sty     L3986
        ldy     #$00
        sty     L348B
        iny
        lda     ($06),y
        sta     L348F
        iny
        lda     ($06),y
        sta     L348E
        ldy     #$0C
L33FC:  lda     ($06),y
        beq     L3429
        sta     L3987
        iny
        lda     ($06),y
        iny
        sta     L3988
        sty     $02
        lda     #$83
        ldy     #$39
        jsr     L3918
        bcc     L3418
        jmp     L397F

L3418:  ldy     $02
        beq     L3442
        inc     L3986
        lda     L3986
        cmp     $74
        bcc     L33FC
        lda     #$0E
        rts

L3429:  lda     L3985
        sta     $69
        sta     $AF
        lda     L3986
        sta     $6A
        sta     $B0
        ldy     #$00
        tya
        dec     $67
        sta     ($67),y
        inc     $67
        clc
        rts

L3442:  lda     L3985
        sta     L348C
        lda     L3986
        sta     L348D
        lda     $06
        sta     L3985
        lda     $07
        sta     L3986
        lda     L348F
        sta     L3987
        lda     L348E
        sta     L3988
        lda     #$83
        ldy     #$39
        jsr     L3918
        bcc     L3470
        jmp     L397F

L3470:  lda     L348C
        sta     L3985
        lda     L348D
        sta     L3986
        ldy     #$0C
        lda     L348B
        bne     L3486
        jmp     L33FC

L3486:  sty     $02
        jmp     L350E

L348B:  brk
L348C:  brk
L348D:  brk
L348E:  brk
L348F:  brk
L3490:  lda     #$83
        ldy     #$39
        jsr     L3918
        bcc     L349C
        jmp     L397F

L349C:  ldy     #$01
        lda     ($06),y
        sta     L348F
        iny
        lda     ($06),y
        sta     L348E
        ldy     #$0C
        sty     L348B
        lda     ($06),y
        sta     L3987
        iny
        lda     ($06),y
        iny
        sty     $02
        sta     L3988
        lda     $74
        sta     L3986
        ldy     #$00
        sty     L3985
        lda     #$83
        ldy     #$39
        jsr     L3918
        bcc     L34D2
        jmp     L397F

L34D2:  ldx     VADDR+1
        lda     VADDR
        ldy     FBITS+1
        bmi     :+              ; ADDR passed
        ldy     #$01
        lda     ($73),y
        sta     $04
        dey
        tax
        lda     ($73),y
        sta     $03
:
        cpx     $74
        bcc     L34F0
        lda     #$0E
        rts

L34F0:  sec
        sbc     #$04
        bcs     L34F6
        dex
L34F6:  sta     $08
        stx     $09
        ldy     #$00
L34FC:  lda     ($73),y
        sta     ($08),y
        iny
        bne     L34FC
        lda     $08
        sta     L3985
        ldy     $09
        iny
        sty     L3986
L350E:  ldy     $02
        bne     L3515
        jmp     L3442

L3515:  lda     ($06),y
        beq     L353E
        sta     L3987
        iny
        lda     ($06),y
        sta     L3988
        iny
        sty     $02
        lda     #$83
        ldy     #$39
        jsr     L3918
        bcc     L3531
        jmp     L397F

L3531:  inc     L3986
        lda     L3986
        cmp     $74
        bcc     L350E
        lda     #$0E
        rts

L353E:  clc
        rts

;;; ============================================================
;;; DSAVE Command
;;; ============================================================

DoDSAVE:
        ldx     #$00
L3542:  lda     INPUT_BUFFER,y
        iny
        cmp     #$A0
        beq     L3542
L354A:  cmp     #$8D
        beq     L355C
        cmp     #$AC
        beq     L355C
        sta     INPUT_BUFFER+1,x
        inx
        lda     INPUT_BUFFER,y
        iny
        bne     L354A
L355C:  stx     INPUT_BUFFER
        dey
        sty     XLEN
        lda     #<DSAVEImpl
        sta     XTRNADDR
        lda     #>DSAVEImpl
        sta     XTRNADDR+1
        lda     #$00
        sta     XCNUM
        lda     #PBitsFlags::FNOPT
        sta     PBITS
        lda     #PBitsFlags::AD | PBitsFlags::L | PBitsFlags::SD
        sta     PBITS+1
        clc
        rts

DSAVEImpl:
        ldx     INPUT_BUFFER
        bne     L3587
        lda     #BI_ERR_SYNTAX_ERROR
        sec
        rts

L3587:  cpx     #$1F
        bcc     L3590
        lda     #$1E
        sta     INPUT_BUFFER
L3590:  lda     VSLOT
        sta     DEFSLT
        asl     a
        asl     a
        asl     a
        asl     a
        ldx     VDRIV
        stx     DEFDRV
        dex
        beq     :+
        ora     #$80
:
        sta     L3984
        lda     #$80
        sta     L3983
        lda     FBITS+1
        bpl     :+              ; ADDR not passed
        ldx     VLNTH+1
        lda     VLNTH
        clc
        adc     #$04
        bcc     L35D9
        bcs     L35D8           ; always
:
        lda     $B0
        sec
        sbc     $68
        tax
        lda     $AF
        sec
        sbc     $67
        bcs     L35CD
        dex
L35CD:  stx     VLNTH+1
        sta     VLNTH
        clc
        adc     #$02
        bcc     L35D9
L35D8:  inx
L35D9:  inx
        cmp     #$00
        beq     L35DF
        inx
L35DF:  stx     L37FB
        cpx     #$7B            ; can't save programs > 122 sectors
        bcc     L35EA
        lda     #BI_ERR_PROGRAM_TOO_LARGE
        sec
        rts

L35EA:  ldy     $74
        iny
        iny
        iny
        sty     L3986
        sty     $07
        lda     #$00
        sta     L36C2
        sta     $06
        sta     L3985
        sta     L3988
        lda     #$11
        sta     L3987
        ldy     #$39
        lda     #$83
        jsr     L3918
        bcc     L3612
        jmp     L397F

L3612:  ldy     #$01
        lda     ($06),y
        cmp     #$11
        beq     L361D
L361A:  jmp     L397F

L361D:  iny
        lda     ($06),y
        cmp     #$0F
        bne     L361A
        jsr     L37FC
        bcc     L362A
        rts

L362A:  inc     $74
        inc     $74
        ldx     L37FB
        ldy     #$00
        tya
L3634:  sta     ($73),y
        iny
        bne     L3634
        ldy     #$0C
        sty     $08
        ldy     #$C1
L363F:  lda     ($06),y
        beq     L36B5
        tya
        and     #$01
        beq     L364F
        lda     #$00
        sta     L36C6
        beq     L3654
L364F:  lda     #$08
        sta     L36C6
L3654:  tya
        sec
        sbc     #$38
        and     #$FE
        lsr     a
        lsr     a
        sta     L36C4
        lda     ($06),y
        pha
        sty     $09
        ldy     #$07
L3666:  rol     a
        bcs     L366C
        dey
        bpl     L3666
L366C:  tya
        clc
        adc     L36C6
        sta     L36C6
        ldy     $08
        lda     L36C2
        bne     L368A
        lda     L36C4
        sta     L36C2
        lda     L36C6
        sta     L36C3
        pla
        bne     L3699
L368A:  lda     L36C4
        sta     ($73),y
        iny
        lda     L36C6
        sta     ($73),y
        iny
        sty     $08
        pla
L3699:  ldy     #$08
L369B:  rol     a
        bcs     L36A1
        dey
        bne     L369B
L36A1:  clc
L36A2:  ror     a
        cpy     #$08
        beq     L36AA
        iny
        bne     L36A2
L36AA:  ldy     $09
        sta     ($06),y
        dex
        beq     L36C7
        cmp     #$00
        bne     L363F
L36B5:  dey
        .byte   $C0
L36B7:  .byte   $37
        bne     L363F
        dec     $74
        dec     $74
        lda     #BI_ERR_DISK_FULL
        sec
        rts

L36C2:  brk
L36C3:  brk
L36C4:  brk
L36C5:  brk
L36C6:  brk
L36C7:  dec     $74
        dec     $74
        lda     #$81
        sta     L3983
        lda     #$11
        sta     L3987
        lda     #$00
        sta     L3988
        lda     $07
        sta     L3986
        lda     #$83
        ldy     #$39
        jsr     L3918
        bcc     L36EB
L36E8:  jmp     L397F

L36EB:  lda     L36C2
        sta     L3987
        lda     L36C3
        sta     L3988
        dec     L3986
        lda     #$83
        ldy     #$39
        jsr     L3918
        bcs     L36E8
        dec     L3983
        lda     $07
        sta     L3986
        lda     #$11
        sta     L3987
        lda     #$0F
        sta     L3988
L3715:  ldy     #$39
        lda     #$83
        jsr     L3918
        bcc     L3721
        jmp     L397F

L3721:  lda     #$0B
L3723:  tay
        lda     ($06),y
        beq     L3739
        bmi     L3739
        tya
        clc
        adc     #$23
        bcc     L3723
        dec     L3988
        bne     L3715
        lda     #BI_ERR_DIRECTORY_FULL
        sec
        rts

L3739:  lda     L36C2
        sta     ($06),y
        iny
        lda     L36C3
        sta     ($06),y
        iny
        ldx     FBITS+1
        bpl     :+              ; ADDR not passed
        lda     #$04
        bne     L3750           ; always
:
        lda     #$02
L3750:  sta     ($06),y
        iny
        ldx     #$00
L3755:  lda     INPUT_BUFFER+1,x
        sta     ($06),y
        iny
        inx
        cpx     INPUT_BUFFER
        bne     L3755
L3761:  cpx     #$1E
        beq     L376D
        lda     #$A0
        sta     ($06),y
        inx
        iny
        bne     L3761
L376D:  lda     L37FB
        sta     ($06),y
        iny
        lda     #$00
        sta     ($06),y
        lda     #$81
        sta     L3983
        lda     #$83
        ldy     #$39
        jsr     L3918
        bcc     L3788
        jmp     L397F

L3788:  dec     $07
        ldx     FBITS+1
        bpl     L37AF           ; ADDR not passed
        ldx     VADDR+1
        lda     VADDR
        sec
        sbc     #$04
        bcs     :+
        dex
:
        sta     $08
        stx     $09
        ldy     #$00
        lda     VADDR
        sta     ($08),y
        iny
        lda     VADDR+1
        sta     ($08),y
        iny
        bne     L37B9
L37AF:  lda     #$FF
        sta     $08
        lda     #$07
        sta     $09
        ldy     #$00
L37B9:  lda     VLNTH
        sta     ($08),y
        iny
        lda     VLNTH+1
        sta     ($08),y
        ldy     #$0C
L37C6:  lda     $08
        sta     L3985
        lda     $09
        sta     L3986
        lda     ($06),y
        beq     L37F4
        iny
        sta     L3987
        lda     ($06),y
        sta     L3988
        iny
        sty     L37FB
        lda     #$83
        ldy     #$39
        jsr     L3918
        bcc     L37ED
        jmp     L397F

L37ED:  inc     $09
        ldy     L37FB
        bne     L37C6
L37F4:  lda     #$00
        sta     $0800
        clc
        rts

L37FB:  brk
L37FC:  ldy     $74
        iny
        iny
        sty     L3986
        sty     $1A
        lda     #$00
        sta     L3985
        sta     $19
        lda     #$11
        sta     L3987
        lda     #$0F
        sta     L3988
L3816:  lda     #$83
        ldy     #$39
        jsr     L3918
        bcc     L3822
        jmp     L397F

L3822:  ldy     #$01
        lda     ($19),y
        cmp     #$11
        beq     L382D
        jmp     L397F

L382D:  lda     #$0E
L382F:  pha
        tay
        ldx     #$00
L3833:  lda     ($19),y
        bne     L383A
        pla
        clc
        rts

L383A:  cmp     INPUT_BUFFER+1,x
        bne     L3852
        inx
        iny
        cpx     INPUT_BUFFER
        bne     L3833
L3846:  cpx     #$1E
        beq     L385F
        lda     ($19),y
        inx
        iny
        cmp     #$A0
        beq     L3846
L3852:  pla
        clc
        adc     #$23
        bcc     L382F
        dec     L3988
        bne     L3816
        clc
        rts

L385F:  pla
        tay
        dey
        lda     ($19),y
        bmi     ErrFileLocked
        cmp     #$04
        beq     L3882
        cmp     #$02
        bne     ErrFileTypeMismatch
        lda     FBITS+1
        bpl     L3887           ; ADDR not passed

ErrFileTypeMismatch:
        lda     #BI_ERR_FILE_TYPE_MISMATCH
        sec
        rts

ErrFileLocked:
        lda     #BI_ERR_FILE_LOCKED
        sec
        rts

L387B:  iny
        iny
        iny
        tya
        pha
        bne     L3852
L3882:  lda     FBITS+1
        bpl     ErrFileTypeMismatch ; ADDR not passed
L3887:  dey
        dey
        tya
        pha
        clc
        adc     #$21
        tay
        lda     ($19),y
        cmp     #$7B
        bcc     L389A
        pla
        lda     #BI_ERR_PROGRAM_TOO_LARGE
        sec
        rts

L389A:  pla
        tay
        lda     ($19),y
        bmi     L387B
        sta     L36C4
        lda     #$FF
        sta     ($19),y
        iny
        lda     ($19),y
        sta     L36C5
        lda     #$81
        sta     L3983
        lda     #$83
        ldy     #$39
        jsr     L3918
        bcc     L38BE
        jmp     L397F

L38BE:  dec     L3983
        lda     L36C4
        sta     L3987
        lda     L36C5
        sta     L3988
        lda     #$83
        ldy     #$39
        jsr     L3918
        bcc     L38D9
        jmp     L397F

L38D9:  .byte   $20
        .byte   $F7
L38DB:  sec
        ldy     #$0C
L38DE:  lda     ($19),y
        beq     L38F5
        sta     L36C4
        iny
        lda     ($19),y
        sta     L36C5
        iny
        sty     $08
        .byte   $20
        .byte   $F7
L38F0:  sec
        ldy     $08
        bne     L38DE
L38F5:  clc
        rts

        lda     L36C4
        asl     a
        asl     a
        clc
        adc     #$38
        tay
        lda     L36C5
        cmp     #$08
        bcs     L3908
        iny
L3908:  and     #$07
        tax
        lda     #$01
L390D:  dex
        bmi     L3913
        asl     a
        bne     L390D
L3913:  ora     ($06),y
        sta     ($06),y
        rts

L3918:  sta     $00
        sty     $01
        ldy     #$01
        lda     ($00),y
        sta     L398A
        iny
        lda     ($00),y
        sta     L3990
        iny
        lda     ($00),y
        sta     L3991
        iny
        lda     ($00),y
        pha
        iny
        lda     ($00),y
        tay
        pla
        jsr     L39B8
        sta     L398D
        sty     L398E
        stx     L398F
        lda     $74
        sta     L398C
        jsr     MLI
        .byte   $80
        .byte   $89
        and     $2EB0,y
        ldy     #$00
        lda     ($00),y
        cmp     #$81
        beq     L3994
        lda     $74
        clc
        adc     L398F
        sta     L3977
        lda     #$00
        sta     L3976
        lda     L3990
        sta     L3979
        lda     L3991
        sta     L397A
L3973:  ldy     #$00
        .byte   $B9
L3976:  brk
L3977:  stx     $99,y
L3979:  brk
L397A:  jsr     $D0C8
        .byte   $F7
        rts

L397F:  sec
        lda     #$08
        rts

L3983:  .byte   $80
L3984:  rts

L3985:  brk
L3986:  brk
L3987:  brk
L3988:  brk
        .byte   $03
L398A:  brk
        brk
L398C:  brk
L398D:  brk
L398E:  brk
L398F:  brk
L3990:  brk
L3991:  brk
L3992:  brk
L3993:  brk
L3994:  lda     $74
        clc
        adc     L398F
        sta     L397A
        lda     #$00
        sta     L3979
        lda     L3990
        sta     L3976
        lda     L3991
        sta     L3977
        jsr     L3973
        jsr     MLI
        sta     ($89,x)
        .byte   $39
        rts

L39B8:  sty     L3993
        tax
        ldy     #$00
        tya
L39BF:  dex
        bmi     L39CA
        clc
        adc     #$08
        bcc     L39BF
        iny
        bne     L39BF
L39CA:  sta     L3992
        ldx     #$00
        lda     L3993
        bne     L39D8
        lda     L3992
        rts

L39D8:  cmp     #$01
        bne     L39E6
        lda     L3992
        clc
        adc     #$07
        bcc     L39E5
        iny
L39E5:  rts

L39E6:  cmp     #$0E
        bne     L39EF
        lda     L3992
        inx
        rts

L39EF:  cmp     #$0F
        bne     L39FE
        lda     L3992
        clc
        adc     #$07
        bcc     L39FC
        iny
L39FC:  inx
        rts

L39FE:  lda     L3993
        lsr     a
        bcs     L3A09
        lda     #$0E
        inx
        bne     L3A0B
L3A09:  lda     #$0F
L3A0B:  sec
        sbc     L3993
        lsr     a
        clc
        adc     L3992
        bcc     L3A17
        iny
L3A17:  rts

reloc_end := $3AFF
