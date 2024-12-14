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
TXTTAB          := $67          ; Start of Applesoft program
VARTAB          := $69          ; Start of Applesoft variables
PRGEND          := $AF          ; End of Applesoft program

;;; DOS 3.3 details c/o Beneath Apple DOS by Don Worth and Pieter Lechner

;;; DOS 3.3 constants
kDOS33VTOCTrack         = 17
kDOS33VTOCSector        = 0
kDOS33MaxFilenameLen    = 30
kDOS33DefaultFirstCatalogSector = $F ; TODO: Don't use this!

;;; Catalog Sector Format
kDOS33FirstFileOffset   = $0B   ; First file descriptive entry

;;; File Descriptive Entry Format
kDOS33FileEntryTrack    = $00   ; +$00 Track of first track/sector list sector
kDOS33FileEntrySector   = $01   ; +$01 Sector of first track/sector list sector
kDOS33FileEntryTypeFlags= $02   ; +$02 File type and flags
kDOS33FileEntryName     = $03   ; +$03-$20 File name (30 characters)
kDOS33FileEntryLength   = $21   ; +$21-$22 Length of file in sectors (LO/HI)
kDOS33FileEntrySize     = $23   ; Size of each file entry

;;;  File types
kDOS33FileTypeText      = $00
kDOS33FileTypeInteger   = $01
kDOS33FileTypeApplesoft = $02
kDOS33FileTypeBinary    = $04
kDOS33FileTypeS         = $08
kDOS33FileTypeRelocatable = $10
kDOS33FileTypeA         = $20
kDOS33FileTypeB         = $40

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

        ;; This relocation code updates any bytes in the code from the
        ;; original pages ($31...$3A) to the final pages returned by
        ;; GETBUFR. The code works by building a table of target pages
        ;; and updating most $3x bytes on source pages to the targets.
        ;; This handles both absolute addresses (e.g. JMP $3456) and
        ;; immediate values (e.g. LDA #$34). This only works because:
        ;; * 6502 opcodes $31-$37 and $39-$3F are unused in the code
        ;; * $38 (opcode SEC) is skipped by the update logic
        ;; * string data has high bytes set so is automatically skipped
        ;; * code on page $38 is explicitly patched with dedicated code
        ;; * constants mistakenly patched are explicitly restored

         ;; Build up target page table for source pages $31...$37
        tax                     ; X = high byte of buffer
        ldy     #$00
:       txa
        sta     reloc_target_page_table,y
        inx
        iny
        cpy     #$07            ; pages to relocate???
        bcc     :-
        ;; X = target page for page $38

        stx     explicit_patch1 ; explicit relocation patch (on page $38)
        stx     explicit_patch2 ; explicit relocation patch (on page $38)

        ;; Finish up target page table for source pages $39 and $3A
        inx                     ; X = $39
        txa
        sta     reloc_target_page_table,y
        inx                     ; X = $3A
        iny
        txa
        sta     reloc_target_page_table,y

        ;; Update all pages to be relocated
        ldy     #$00            ; byte index in page
        read_src_page := *+2
ploop:  lda     ExternalCommand,y
        ldx     #0              ; index in page tables
iloop:  cmp     reloc_start_page_table,x
        beq     write
        inx
        cpx     #page_table_size
        bcc     iloop

        ;; Next byte
nextb:  iny
        bne     ploop

        ;; Next page
        inc     read_src_page
        inc     write_src_page

        lda     read_src_page   ; done?
        cmp     #(>reloc_end)+1
        bcc     ploop
        bcs     move_and_hook   ; always

        ;; Update byte to target page
write:  lda     reloc_target_page_table,x
        write_src_page := *+2
        sta     ExternalCommand,y
        bne     nextb           ; always

page_table_size = 9

;;; Source pages
;;; ($38 is SEC so excluded)
reloc_start_page_table:
        .byte   $31, $32, $33, $34, $35, $36, $37, $39, $3A

;;; Target pages
reloc_target_page_table:
        .res    9, 0

;;; Move into target buffer and hook into BI CMD syntaxing
move_and_hook:
        lda     #>ExternalCommand
        sta     STARTHI

        lda     #$37            ; explicit relocation patch restoration
        sta     explicit_patch3 ; (undoing the auto-patch above)

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
:       lda     install_message_str,y
        beq     :+
        jsr     COUT
        iny
        bne     :-
:
        rts

install_message_str:
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

.proc DoDCAT
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
        ;; Compute unit_num
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
        ora     #$80            ; D1 -> D2
:       sta     rwts_params_unit_num

        ;; Read VTOC
        lda     #RWTSRead
        sta     rwts_params_op
        lda     #kDOS33VTOCTrack
        ldy     #kDOS33VTOCSector
        sta     rwts_params_track
        sty     rwts_params_sector
        lda     HIMEM+1
        sta     rwts_params_data_buf+1
        lda     #$00
        sta     rwts_params_data_buf
        ldy     #>rwts_params
        lda     #<rwts_params
        jsr     DoRWTS
        bcc     :+
        jmp     ErrIOError
:
        jsr     CR
        ldy     #$06            ; VTOC: Diskette volume number (1-254)
        lda     (HIMEM),y
        sta     $02

        ldy     #.strlen("DISK VOLUME")-1
:       lda     disk_volume_str,y
        jsr     COUT
        dey
        bpl     :-

        lda     #' '|$80
        jsr     COUT
        lda     $02             ; volume number
        jsr     PrintByte
        jsr     CR
        jsr     CR

;;; "Beneath Apple DOS" says first catalog T/S is VTOC bytes $01/$02.
;;; This code assumes the standard progression from T17/S15 through
;;; T17S01.
;;; TODO: Consider not hard-coding it.

        cur_cat_sector_offset = $03

        ldy     #kDOS33DefaultFirstCatalogSector
        sty     rwts_params_sector
sector_loop:
        ldy     #>rwts_params
        lda     #<rwts_params
        jsr     DoRWTS
        bcc     :+
        jmp     ErrIOError
:
        lda     HIMEM+1
        sta     $01
        lda     HIMEM
        sta     $00
        ldy     #kDOS33FirstFileOffset

file_loop:
        sty     cur_cat_sector_offset
        lda     ($00),y         ; +$00 "Track of first track/sector list sector"
        bne     L3205           ; $00 = entry is free
        jmp     L3292

L3205:  cmp     #$FF            ; $FF = entry is deleted
        beq     next_file
        iny
        iny                     ; +$02 "File type and flags"

        ;; Locked?
        lda     ($00),y
        bmi     L3217           ; high bit = locked
        lda     #' '|$80
        jsr     COUT
        jmp     L321C
L3217:  lda     #'*'|$80
        jsr     COUT

        ;; File type
L321C:  lda     ($00),y
        and     #$7F            ; mask off type
        .assert kDOS33FileTypeText = 0, error, "mismatch"
        beq     type_t
        cmp     #kDOS33FileTypeInteger
        beq     type_i
        cmp     #kDOS33FileTypeApplesoft
        beq     type_a
        cmp     #kDOS33FileTypeBinary
        beq     type_b
        bne     next_file       ; always

type_t: lda     #'T'|$80
        bne     :+              ; always
type_i: lda     #'I'|$80
        bne     :+              ; always
type_a: lda     #'A'|$80
        bne     :+              ; always
type_b: lda     #'B'|$80
:       jsr     COUT

        lda     #' '|$80
        jsr     COUT

        ;; Sectors
        lda     cur_cat_sector_offset
        clc
        adc     #kDOS33FileEntryLength
        tay
        lda     ($00),y
        jsr     PrintByte
        lda     #' '|$80
        jsr     COUT

        ldy     cur_cat_sector_offset
        iny
        iny
        iny                     ; +$03 - "File name (30 characters)

        ;; Filename
        ldx     #$00
L325D:  lda     ($00),y
        jsr     COUT
        iny
        inx
        cpx     #kDOS33MaxFilenameLen-1
        bne     L325D

        jsr     CR

        ;; Pause?
        lda     KBD
        bpl     next_file
        lda     #$00
        sta     KBDSTRB
L3275:  lda     KBD
        bpl     L3275
        lda     #$00
        sta     KBDSTRB

next_file:
        lda     cur_cat_sector_offset
        clc
        adc     #kDOS33FileEntrySize
        tay
        bcs     next_sector
        jmp     file_loop

;;; "Beneath Apple DOS" says bytes $01/$02 of each catalog sector is
;;; T/S of next catalog sector. This code assumes the standard
;;; progression from T17/S15 through T17S01.
;;; TODO: Consider not hard-coding it.

next_sector:
        dec     rwts_params_sector
        beq     L3292
        jmp     sector_loop

L3292:  jsr     CR
        jsr     CR
        clc
        lda     #$00
        rts

disk_volume_str:
        scrcode "EMULOV KSID"   ; "DISK VOLUME" reversed

.proc PrintByte
        sta     num
        ldy     #2              ; num digits - 1
dloop:  lda     #$00
        pha
sloop:  lda     num
        cmp     digits_table,y
        bcc     :+
        sbc     digits_table,y
        sta     num
        pla
        clc
        adc     #1
        pha
        jmp     sloop

:       pla
        ora     #'0'|$80
        jsr     COUT
        dey
        bpl     dloop
        rts

num:    .byte   0

        .byte   0               ; unused

digits_table:
        .byte   1, 10, 100
.endproc
.endproc

;;; ============================================================
;;; DLOAD Command
;;; ============================================================

.proc DoDLOAD
        ;; Parse command buffer
        ldx     #$00
:       lda     INPUT_BUFFER,y
        iny
        cmp     #$8D            ; CR
        beq     fail
        cmp     #' '|$80        ; skip whitespace
        beq     :-

        ;; Shuffle filename down to start of buffer
parse_loop:
        cmp     #$8D            ; CR
        beq     done_name
        cmp     #','|$80
        beq     done_name
        cmp     #$E0            ; lowercase plane?
        bcc     :+
        and     #UPPERCASE_MASK ; force to uppercase
:       sta     INPUT_BUFFER+1,x
        inx
        lda     INPUT_BUFFER,y
        iny
        bne     parse_loop

fail:   sec
        rts
done_name:

        ;; Truncate name if too long
        cpx     #kDOS33MaxFilenameLen+1
        bcc     :+
        ldx     #kDOS33MaxFilenameLen
        ;; Prefix buffer with name length
:       stx     INPUT_BUFFER
        dey
        sty     XLEN

        ;; Let BASIC.SYSTEM parse arguments
        lda     #<DLOADImpl
        sta     XTRNADDR
        lda     #>DLOADImpl
        sta     XTRNADDR+1
        lda     #$00
        sta     XCNUM
        lda     #PBitsFlags::FNOPT
        sta     PBITS
        lda     #PBitsFlags::AD | PBitsFlags::SD
        sta     PBITS+1
        clc
        rts

;;; --------------------------------------------------

DLOADImpl:
        ;; Compute unit_num
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
        ora     #$80            ; D1 -> D2
:       sta     rwts_params_unit_num

        ;; Search the catalog for a matching file entry
        lda     HIMEM+1
        sta     rwts_params_data_buf+1
        lda     #$00
        sta     rwts_params_data_buf
        lda     #kDOS33VTOCTrack
        sta     rwts_params_track
        lda     #kDOS33DefaultFirstCatalogSector
        sta     rwts_params_sector

catalog_sector_loop:
        ldy     #>rwts_params
        lda     #<rwts_params
        jsr     DoRWTS
        bcc     :+
        jmp     ErrIOError
:
        lda     #kDOS33FirstFileOffset + kDOS33FileEntryName
        ;; Check for filename match
entry_loop:
        tay
        ldx     #0

        cur_load_sector_offset = $02

        sty     cur_load_sector_offset
cloop:  lda     (HIMEM),y
        beq     ErrPathNotFound
        cmp     INPUT_BUFFER+1,x
        bne     next_entry
        inx
        cpx     INPUT_BUFFER
        beq     possible_match
        iny
        bne     cloop
next_entry:
        lda     cur_load_sector_offset
        clc
        adc     #kDOS33FileEntrySize
        bcc     entry_loop

        ;; Next catalog sector
        ldy     rwts_params_sector
        dey
        beq     ErrPathNotFound
        sty     rwts_params_sector
        bne     catalog_sector_loop

ErrPathNotFound:
        lda     #BI_ERR_PATH_NOT_FOUND
        sec
        rts

possible_match:
        cpx     #kDOS33MaxFilenameLen
        beq     found_match
        iny
        lda     (HIMEM),y
        cmp     #' '|$80
        bne     next_entry
        inx
        bne     possible_match

;;; --------------------------------------------------
;;; Found a matching filename

found_match:
        ;; Look up track/sector of file's first track/sector list sector
        ldy     cur_load_sector_offset
        dey
        dey
        dey                     ; Y = +kDOS33FileEntryTrack
        lda     (HIMEM),y
        bmi     ErrPathNotFound
        sta     rwts_params_track
        iny                     ; Y = +kDOS33FileEntrySector
        lda     (HIMEM),y
        sta     rwts_params_sector

        ldx     rwts_params_data_buf+1
        inx
        inx
        stx     rwts_params_data_buf+1
        stx     $06+1
        ldx     #$00
        stx     $06
        iny                     ; Y = +kDOS33FileTypeFlags

        ;; Determine file type - Applesoft or binary?
        lda     (HIMEM),y
        and     #$7F            ; mask off Locked bit
        cmp     #kDOS33FileTypeApplesoft
        beq     load_applesoft
        cmp     #kDOS33FileTypeBinary
        bne     L33C9
        jmp     load_binary

L33C9:  sec
        lda     #$0D
        rts

;;; --------------------------------------------------

load_applesoft:
        ;; Load first sector of track/sector list
        lda     #<rwts_params
        ldy     #>rwts_params
        jsr     DoRWTS
        bcc     :+
        jmp     ErrIOError
:
        ;; Set up data load start location - Applesoft start in memory
        lda     TXTTAB
        ldy     TXTTAB+1
        sec
        sbc     #$02
        bcs     :+
        dey
:       sta     rwts_params_data_buf
        sty     rwts_params_data_buf+1
        ldy     #$00
        sty     L348B
        iny
        lda     ($06),y
        sta     next_load_track
        iny
        lda     ($06),y
        sta     next_load_sector
        ldy     #$0C
        ;; fall through

;;; --------------------------------------------------

L33FC:  lda     ($06),y
        beq     finish_load
        sta     rwts_params_track
        iny
        lda     ($06),y
        iny
        sta     rwts_params_sector
        sty     $02
        lda     #<rwts_params
        ldy     #>rwts_params
        jsr     DoRWTS
        bcc     :+
        jmp     ErrIOError
:
        ldy     $02
        beq     L3442
        inc     rwts_params_data_buf+1
        lda     rwts_params_data_buf+1
        cmp     HIMEM+1
        bcc     L33FC
        lda     #$0E
        rts

finish_load:
        ;; Set up parameters for Applesoft program
        lda     rwts_params_data_buf
        sta     VARTAB
        sta     PRGEND
        lda     rwts_params_data_buf+1
        sta     VARTAB+1
        sta     PRGEND+1
        ldy     #$00
        tya
        dec     TXTTAB
        sta     (TXTTAB),y
        inc     TXTTAB
        clc
        rts

;;; Load next sector into target memory buffer???
L3442:  lda     rwts_params_data_buf
        sta     stash_data_buf_ptr
        lda     rwts_params_data_buf+1
        sta     stash_data_buf_ptr+1

        lda     $06
        sta     rwts_params_data_buf
        lda     $06+1
        sta     rwts_params_data_buf+1

        lda     next_load_track
        sta     rwts_params_track
        lda     next_load_sector
        sta     rwts_params_sector

        lda     #<rwts_params
        ldy     #>rwts_params
        jsr     DoRWTS
        bcc     :+
        jmp     ErrIOError
:
        lda     stash_data_buf_ptr
        sta     rwts_params_data_buf
        lda     stash_data_buf_ptr+1
        sta     rwts_params_data_buf+1
        ldy     #$0C
        lda     L348B
        bne     L3486
        jmp     L33FC

L3486:  sty     $02
        jmp     L350E

L348B:  brk

stash_data_buf_ptr:     .addr   0
next_load_sector:       .byte   0
next_load_track:        .byte   0

;;; --------------------------------------------------

load_binary:
        lda     #<rwts_params
        ldy     #>rwts_params
        jsr     DoRWTS
        bcc     :+
        jmp     ErrIOError
:
        ldy     #$01
        lda     ($06),y
        sta     next_load_track
        iny
        lda     ($06),y
        sta     next_load_sector

        ldy     #$0C
        sty     L348B

        lda     ($06),y
        sta     rwts_params_track
        iny
        lda     ($06),y
        iny
        sty     $02
        sta     rwts_params_sector

        lda     HIMEM+1
        sta     rwts_params_data_buf+1
        ldy     #$00
        sty     rwts_params_data_buf
        lda     #<rwts_params
        ldy     #>rwts_params
        jsr     DoRWTS
        bcc     :+
        jmp     ErrIOError
:
        ldx     VADDR+1
        lda     VADDR
        ldy     FBITS+1
        bmi     :+              ; ADDR passed
        ldy     #$01
        lda     (HIMEM),y
        sta     $04
        dey
        tax
        lda     (HIMEM),y
        sta     $03
:
        cpx     HIMEM+1
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
L34FC:  lda     (HIMEM),y
        sta     ($08),y
        iny
        bne     L34FC
        lda     $08
        sta     rwts_params_data_buf
        ldy     $09
        iny
        sty     rwts_params_data_buf+1
        ;; fall through

;;; --------------------------------------------------

;;; Read file's next Track/Sector List sector???
L350E:  ldy     $02
        bne     L3515
        jmp     L3442

L3515:  lda     ($06),y
        beq     L353E
        sta     rwts_params_track
        iny
        lda     ($06),y
        sta     rwts_params_sector
        iny
        sty     $02
        lda     #<rwts_params
        ldy     #>rwts_params
        jsr     DoRWTS
        bcc     :+
        jmp     ErrIOError
:
        inc     rwts_params_data_buf+1
        lda     rwts_params_data_buf+1
        cmp     HIMEM+1
        bcc     L350E

        lda     #BI_ERR_PROGRAM_TOO_LARGE
        rts

L353E:  clc
        rts
.endproc

;;; ============================================================
;;; DSAVE Command
;;; ============================================================

.proc DoDSAVE
        ldx     #$00
:       lda     INPUT_BUFFER,y
        iny
        cmp     #' '|$80
        beq     :-
        ;; Shuffle filename down to start of buffer
parse_loop:
        cmp     #$8D
        beq     :+
        cmp     #','|$80
        beq     :+
        sta     INPUT_BUFFER+1,x
        inx
        lda     INPUT_BUFFER,y
        iny
        bne     parse_loop
:
        ;; Prefix buffer with name length
        stx     INPUT_BUFFER
        dey
        sty     XLEN

        ;; Let BASIC.SYSTEM parse arguments
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
        bne     :+
        lda     #BI_ERR_SYNTAX_ERROR
        sec
        rts
:
        ;; Truncate name if too long
        cpx     #kDOS33MaxFilenameLen+1
        bcc     :+
        lda     #kDOS33MaxFilenameLen
        sta     INPUT_BUFFER
:
        ;; Compute unit_num
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
        ora     #$80            ; D1 -> D2
:       sta     rwts_params_unit_num

        lda     #RWTSRead
        sta     rwts_params_op
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
        sbc     TXTTAB+1
        tax
        lda     $AF
        sec
        sbc     TXTTAB
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

L35EA:  ldy     HIMEM+1
        iny
        iny
        iny
        sty     rwts_params_data_buf+1
        sty     $06+1
        lda     #$00
        sta     L36C2
        sta     $06
        sta     rwts_params_data_buf
        sta     rwts_params_sector
        lda     #kDOS33VTOCTrack
        sta     rwts_params_track
        ldy     #>rwts_params
        lda     #<rwts_params
        jsr     DoRWTS
        bcc     :+
        jmp     ErrIOError
:
        ldy     #$01
        lda     ($06),y
        cmp     #kDOS33VTOCTrack
        beq     L361D
L361A:  jmp     ErrIOError

L361D:  iny
        lda     ($06),y
        cmp     #$0F
        bne     L361A
        jsr     L37FC
        bcc     L362A
        rts

L362A:  inc     HIMEM+1
        inc     HIMEM+1
        ldx     L37FB
        ldy     #$00
        tya
L3634:  sta     (HIMEM),y
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
        sta     (HIMEM),y
        iny
        lda     L36C6
        sta     (HIMEM),y
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
        explicit_patch3 := *+1  ; relocation patch
        cpy     #$37            ; (false positive)
        bne     L363F
        dec     HIMEM+1
        dec     HIMEM+1
        lda     #BI_ERR_DISK_FULL
        sec
        rts

L36C2:  brk
L36C3:  brk
L36C4:  brk
L36C5:  brk
L36C6:  brk
L36C7:  dec     HIMEM+1
        dec     HIMEM+1
        lda     #RWTSWrite
        sta     rwts_params_op
        lda     #kDOS33VTOCTrack
        sta     rwts_params_track
        lda     #$00
        sta     rwts_params_sector
        lda     $07
        sta     rwts_params_data_buf+1
        lda     #<rwts_params
        ldy     #>rwts_params
        jsr     DoRWTS
        bcc     :+
L36E8:  jmp     ErrIOError
:
        lda     L36C2
        sta     rwts_params_track
        lda     L36C3
        sta     rwts_params_sector
        dec     rwts_params_data_buf+1
        lda     #<rwts_params
        ldy     #>rwts_params
        jsr     DoRWTS
        bcs     L36E8
        dec     rwts_params_op  ; Write -> Read
        lda     $07
        sta     rwts_params_data_buf+1
        lda     #kDOS33VTOCTrack
        sta     rwts_params_track
        lda     #kDOS33DefaultFirstCatalogSector
        sta     rwts_params_sector
L3715:  ldy     #>rwts_params
        lda     #<rwts_params
        jsr     DoRWTS
        bcc     :+
        jmp     ErrIOError
:
        lda     #$0B
L3723:  tay
        lda     ($06),y
        beq     L3739
        bmi     L3739
        tya
        clc
        adc     #kDOS33FileEntrySize
        bcc     L3723
        dec     rwts_params_sector
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
L3761:  cpx     #kDOS33MaxFilenameLen
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
        lda     #RWTSWrite
        sta     rwts_params_op
        lda     #<rwts_params
        ldy     #>rwts_params
        jsr     DoRWTS
        bcc     :+
        jmp     ErrIOError
:
        dec     $07
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
        sta     rwts_params_data_buf
        lda     $09
        sta     rwts_params_data_buf+1
        lda     ($06),y
        beq     L37F4
        iny
        sta     rwts_params_track
        lda     ($06),y
        sta     rwts_params_sector
        iny
        sty     L37FB
        lda     #<rwts_params
        ldy     #>rwts_params
        jsr     DoRWTS
        bcc     :+
        jmp     ErrIOError
:
        inc     $09
        ldy     L37FB
        bne     L37C6
L37F4:  lda     #$00
        sta     $0800
        clc
        rts

L37FB:  brk
L37FC:  ldy     HIMEM+1
        iny
        iny
        sty     rwts_params_data_buf+1
        sty     $1A
        lda     #$00
        sta     rwts_params_data_buf
        sta     $19
        lda     #kDOS33VTOCTrack
        sta     rwts_params_track
        lda     #kDOS33DefaultFirstCatalogSector
        sta     rwts_params_sector
L3816:  lda     #<rwts_params
        ldy     #>rwts_params
        jsr     DoRWTS
        bcc     :+
        jmp     ErrIOError
:
        ldy     #$01
        lda     ($19),y
        cmp     #kDOS33VTOCTrack
        beq     L382D
        jmp     ErrIOError

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
L3846:  cpx     #kDOS33MaxFilenameLen
        beq     L385F
        lda     ($19),y
        inx
        iny
        cmp     #' '|$80
        beq     L3846
L3852:  pla
        clc
        adc     #kDOS33FileEntrySize
        bcc     L382F
        dec     rwts_params_sector
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
        lda     #RWTSWrite
        sta     rwts_params_op
        lda     #<rwts_params
        ldy     #>rwts_params
        jsr     DoRWTS
        bcc     :+
        jmp     ErrIOError
:
        dec     rwts_params_op  ; Write -> Read
        lda     L36C4
        sta     rwts_params_track
        lda     L36C5
        sta     rwts_params_sector
        lda     #<rwts_params
        ldy     #>rwts_params
        jsr     DoRWTS
        bcc     :+
        jmp     ErrIOError
:
        explicit_patch1 := *+2  ; relocation patch
        jsr     L38F7
        ldy     #$0C
L38DE:  lda     ($19),y
        beq     L38F5
        sta     L36C4
        iny
        lda     ($19),y
        sta     L36C5
        iny
        sty     $08
        explicit_patch2 := *+2  ; relocation patch
        jsr     L38F7
        ldy     $08
        bne     L38DE
L38F5:  clc
        rts

L38F7:  lda     L36C4
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
.endproc
explicit_patch1 := DoDSAVE::explicit_patch1
explicit_patch2 := DoDSAVE::explicit_patch2
explicit_patch3 := DoDSAVE::explicit_patch3

;;; ============================================================
;;; RWTS
;;; ============================================================

;;; Input: A,Y = parameter block address
;;; +$00 = op (RWTSRead or RWTSWrite)
;;; +$01 = unit_num (ProDOS unit number, e.g. $60 = S6D1)
;;; +$02 = data_buf
;;; +$04 = track
;;; +$05 = sector
;;; Output: C=0 success, C=1 failure

.proc DoRWTS
        sta     $00
        sty     $01

        ldy     #$01
        lda     ($00),y         ; +$01 unit_num
        sta     unit_num
        iny
        lda     ($00),y         ; +$02 data_buf (low)
        sta     buf_ptr
        iny
        lda     ($00),y         ; +$03 data_buf (high)
        sta     buf_ptr+1
        iny
        lda     ($00),y         ; +$04 track
        pha
        iny
        lda     ($00),y         ; +$05 sector
        tay
        pla
        jsr     TrackSectorToBlockNum
        sta     block_num
        sty     block_num+1
        stx     page_num
        lda     HIMEM+1
        sta     data_buffer+1

        jsr     MLI
        .byte   READ_BLOCK
        .addr   read_write_block_params
        bcs     ErrIOError

        ldy     #$00
        lda     ($00),y
        cmp     #$81
        beq     DoWriteBlock

        lda     HIMEM+1
        clc
        adc     page_num
        sta     read_buf+1
        lda     #$00
        sta     read_buf
        lda     buf_ptr
        sta     write_buf
        lda     buf_ptr+1
        sta     write_buf+1
        ;; fall through
.endproc

CopyPage:
        ldy     #$00
        read_buf := *+1
:       lda     $9600,y
        write_buf := *+1
        sta     $2000,y
        iny
        bne     :-
        rts

;;; ============================================================

ErrIOError:
        sec
        lda     #BI_ERR_IO_ERROR
        rts

;;; ============================================================

RWTSRead        = $80
RWTSWrite       = $81

rwts_params:
rwts_params_op:         .byte   $80   ; +$00 op ($80 = read, $81 = read+write)
rwts_params_unit_num:   .byte   $60   ; +$01 unit_num ($60 = S6D1, etc)
rwts_params_data_buf:   .addr   $0000 ; +$02 data_buf
rwts_params_track:      .byte   $00   ; +$04 track
rwts_params_sector:     .byte   $00   ; +$05 sector

;;; Parameter block for MLI READ/WRITE_BLOCK calls
read_write_block_params:
param_count:    .byte   3
unit_num:       .byte   0
data_buffer:    .addr   $0000
block_num:      .word   $0000

;;; ============================================================

;;; Used by DoRWTS
page_num:       .byte   0       ; page within block (0 or 1)
buf_ptr:        .addr   0

;;; Used by TrackSectorToBlockNum
L3992:  brk
L3993:  brk

.proc DoWriteBlock
        lda     HIMEM+1
        clc
        adc     page_num
        sta     write_buf+1
        lda     #$00
        sta     write_buf
        lda     buf_ptr
        sta     read_buf
        lda     buf_ptr+1
        sta     read_buf+1
        jsr     CopyPage

        jsr     MLI
        .byte   WRITE_BLOCK
        .addr   read_write_block_params

        rts
.endproc

;;; ============================================================

;;; Convert DOS track/sector to ProDOS block_num
;;; Input: A = track, Y = sector
;;; Output: A,Y = block_num, X = page_num (0 or 1)

.proc TrackSectorToBlockNum

;;; TODO: Document the mapping logic here.

        sty     L3993
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
.endproc

reloc_end := $3AFF