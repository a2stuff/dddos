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

;;; Volume Table Of Contents (VTOC) Format
kDOS33VTOCFirstCatTrack = $01   ; +$01 Track number of first catalog sector (usually $11)
kDOS33VTOCFirstCatSector= $02   ; +$02 Secto number of first catalog sector (usually $F)
kDOS33VTOCVolumeNumber  = $06   ; +$06 Diskette volume number (1-254)
kDOS33VTOCBitMap        = $38   ; +$38 Bit map of free sectors (4 bytes per track)

;;; Catalog Sector Format
kDOS33NextCatSectorTrack= $01   ; Track number of next catalog sector (usually $11)
kDOS33FirstFileOffset   = $0B   ; First file descriptive entry

;;; File Descriptive Entry Format
kDOS33FileEntryTrack    = $00   ; +$00 Track of first track/sector list sector
kDOS33FileEntrySector   = $01   ; +$01 Sector of first track/sector list sector
kDOS33FileEntryTypeFlags= $02   ; +$02 File type and flags
kDOS33FileEntryName     = $03   ; +$03-$20 File name (30 characters)
kDOS33FileEntryLength   = $21   ; +$21-$22 Length of file in sectors (LO/HI)
kDOS33FileEntrySize     = $23   ; Size of each file entry

;;; File Track/Sector List Format
kDOS33TSListUnused      = $00   ; Not used
kDOS33TSListNextTrack   = $01   ; Track number of next T/S List sector (or 0)
kDOS33TSListNextSector  = $02   ; Sector number of next T/S List sector (or 0)
kDOS33TSListFirstDataT  = $0C   ; Track of first data sector (or 0)
kDOS33TSListFirstDataS  = $0D   ; Sector of first data sector

;;; File types
kDOS33FileTypeText      = $00
kDOS33FileTypeInteger   = $01
kDOS33FileTypeApplesoft = $02
kDOS33FileTypeBinary    = $04
kDOS33FileTypeS         = $08
kDOS33FileTypeRelocatable = $10
kDOS33FileTypeA         = $20
kDOS33FileTypeB         = $40

;;; For character literals; sets high bit. (Like scrcode)
.define scrchar(c) ((c)|$80)

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
        cmp     #scrchar(' ')   ; skip whitespace
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
        cmp     #scrchar(' ')   ; skip whitespace
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
        cmp     #scrchar(' ')   ; skip whitespace
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
        ldy     #kDOS33VTOCVolumeNumber
        lda     (HIMEM),y
        sta     $02

        ldy     #.strlen("DISK VOLUME")-1
:       lda     disk_volume_str,y
        jsr     COUT
        dey
        bpl     :-

        lda     #scrchar(' ')
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
        lda     #scrchar(' ')
        jsr     COUT
        jmp     L321C
L3217:  lda     #scrchar('*')
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

type_t: lda     #scrchar('T')
        bne     :+              ; always
type_i: lda     #scrchar('I')
        bne     :+              ; always
type_a: lda     #scrchar('A')
        bne     :+              ; always
type_b: lda     #scrchar('B')
:       jsr     COUT

        lda     #scrchar(' ')
        jsr     COUT

        ;; Sectors
        lda     cur_cat_sector_offset
        clc
        adc     #kDOS33FileEntryLength
        tay
        lda     ($00),y
        jsr     PrintByte
        lda     #scrchar(' ')
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
        ora     #scrchar('0')
        jsr     COUT
        dey
        bpl     dloop
        rts

num:    .byte   0

        .byte   0               ; unused!

digits_table:
        .byte   1, 10, 100
.endproc
.endproc

;;; ============================================================
;;; DLOAD Command
;;; ============================================================

;;; INPUT_BUFFER is used to hold the filename, length prefixed
;;; HIMEM points at RWTS buffer for catalog and track/sector list
;;; HIMEM+$200 is used for file data

.proc DoDLOAD
        ;; Parse command buffer
        ldx     #$00
:       lda     INPUT_BUFFER,y
        iny
        cmp     #$8D            ; CR
        beq     fail
        cmp     #scrchar(' ')   ; skip whitespace
        beq     :-

        ;; Shuffle filename down to start of buffer
parse_loop:
        cmp     #$8D            ; CR
        beq     done_name
        cmp     #scrchar(',')
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
        beq     ErrPathNotFound ; $00 in name; invalid entry, just fail
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
        cmp     #scrchar(' ')
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
        dey                     ; Y = +`kDOS33FileEntryTrack`
        lda     (HIMEM),y
        bmi     ErrPathNotFound
        sta     rwts_params_track
        iny                     ; Y = +`kDOS33FileEntrySector`
        lda     (HIMEM),y
        sta     rwts_params_sector

        ;; Set up buf for loading track/sector list (HIMEM + $200)
        ldx     rwts_params_data_buf+1
        inx
        inx
        stx     rwts_params_data_buf+1

        tslist_buf_ptr := $06

        ;; ... and point `tslist_buf_ptr` at it too
        stx     tslist_buf_ptr+1
        ldx     #$00
        stx     tslist_buf_ptr
        iny                     ; Y = +`kDOS33FileTypeFlags`

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
        ;; Set up data load start location - Applesoft start in memory.
        ;;
        ;; We don't need to special case loading the first sector
        ;; because while the first two bytes encode the length (1) we
        ;; always just load whole sectors anyway so we can continue
        ;; without knowing the length, and (2) the BASIC program in
        ;; memory has a two byte length prefix at `(TXTTAB)` anyway.

        lda     TXTTAB
        ldy     TXTTAB+1
        sec
        sbc     #$02
        bcs     :+
        dey
:       sta     rwts_params_data_buf
        sty     rwts_params_data_buf+1

        ldy     #$00
        sty     load_type_flag  ; 0 = Applesoft
        iny                     ; Y = $01 = `kDOS33TSListNextTrack`
        lda     (tslist_buf_ptr),y
        sta     next_load_track
        iny                     ; Y = $02 - `kDOS33TSListNextSector`
        lda     (tslist_buf_ptr),y
        sta     next_load_sector
        ldy     #$0C            ; Y = $0C - `kDOS33TSListFirstDataT`
        ;; fall through

;;; --------------------------------------------------

        tslist_offset := $02

;;; Read data sectors in Applesoft file. Reads until T/S list entries
;;; are exhausted, then jumps to `load_next_tslist_sector` which may
;;; jump back here.
;;; Enter with Y = offset in Track/Sector List buffer
load_applesoft_data_sector:
        ;; Look up next T/S in list
        lda     (tslist_buf_ptr),y
        beq     finish_applesoft_load
        sta     rwts_params_track
        iny
        lda     (tslist_buf_ptr),y
        iny
        sta     rwts_params_sector
        sty     tslist_offset

        ;; Load it
        lda     #<rwts_params
        ldy     #>rwts_params
        jsr     DoRWTS
        bcc     :+
        jmp     ErrIOError
:
        ldy     tslist_offset
        beq     load_next_tslist_sector

        inc     rwts_params_data_buf+1
        lda     rwts_params_data_buf+1
        cmp     HIMEM+1         ; don't allow loading over buffer
        bcc     load_applesoft_data_sector

        lda     #BI_ERR_PROGRAM_TOO_LARGE
        rts

finish_applesoft_load:
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

;;; --------------------------------------------------

;;; Loads the next Track/Sector List sector. Used by both the
;;; Applesoft and Binary file loading loops; jumps back to appropriate
;;; caller using `load_type_flag`.

load_next_tslist_sector:
        ;; Store current data load pointer
        lda     rwts_params_data_buf
        sta     stash_data_buf_ptr
        lda     rwts_params_data_buf+1
        sta     stash_data_buf_ptr+1

        ;; Load into dedicated buffer
        lda     tslist_buf_ptr
        sta     rwts_params_data_buf
        lda     tslist_buf_ptr+1
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
        ;; Restore previous data load pointer
        lda     stash_data_buf_ptr
        sta     rwts_params_data_buf
        lda     stash_data_buf_ptr+1
        sta     rwts_params_data_buf+1

        ldy     #kDOS33TSListFirstDataT
        lda     load_type_flag
        bne     :+
        jmp     load_applesoft_data_sector ; expects Y = $0C
:
        sty     tslist_offset
        jmp     load_binary_data_sector ; just uses `tslist_offset`

;;; --------------------------------------------------

;;; $00 if Applesoft, binary otherwise
load_type_flag:         .byte   0

stash_data_buf_ptr:     .addr   0
next_load_sector:       .byte   0
next_load_track:        .byte   0

;;; --------------------------------------------------

load_binary:
        ;; Load first sector of track/sector list
        lda     #<rwts_params
        ldy     #>rwts_params
        jsr     DoRWTS
        bcc     :+
        jmp     ErrIOError
:
        ldy     #kDOS33TSListNextTrack
        lda     (tslist_buf_ptr),y
        sta     next_load_track
        iny                     ; Y = $02 - `kDOS33TSListNextSector`
        lda     (tslist_buf_ptr),y
        sta     next_load_sector

        ldy     #kDOS33TSListFirstDataT
        sty     load_type_flag  ; non-zero = binary

        ;; Unlike Applesoft, for Binary file we need to load the
        ;; first sector and extract the start address (first two
        ;; bytes) and length (next two bytes).

        lda     (tslist_buf_ptr),y
        sta     rwts_params_track
        iny
        lda     (tslist_buf_ptr),y
        iny

        ;; Y points at next T/S list entry

        sty     tslist_offset
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
        ;; First sector loaded; determine load address

        ldx     VADDR+1         ; Use VADDR if passed
        lda     VADDR
        ldy     FBITS+1
        bmi     :+              ; A parameter was passed
        ldy     #$01            ; Not passed, use bytes +$00/+$01
        lda     (HIMEM),y
        sta     $04             ; unused!
        dey                     ; Y = $00
        tax
        lda     (HIMEM),y
        sta     $03             ; unused!
        ;; A,X = load address
:
        cpx     HIMEM+1         ; can't load past HIMEM
        bcc     :+
        lda     #BI_ERR_PROGRAM_TOO_LARGE
        rts
:
        ;; A,X -= 4
        sec
        sbc     #$04
        bcs     :+
        dex
:
        load_addr_stash = $08
        sta     load_addr_stash
        stx     load_addr_stash+1

        ;; Copy first page of data into place (offset by -4)
        ;; TODO: Does this trash the prior 4 bytes in memory?
        ldy     #0
:       lda     (HIMEM),y
        sta     (load_addr_stash),y
        iny
        bne     :-

        ;; Target address for next page of data
        lda     load_addr_stash
        sta     rwts_params_data_buf
        ldy     load_addr_stash+1
        iny
        sty     rwts_params_data_buf+1
        ;; fall through

;;; --------------------------------------------------

;;; Read data sectors in binary file. Reads until T/S list entries are
;;; exhausted, then jumps to `load_next_tslist_sector` which may jump
;;; back here.
;;; Enter with `tslist_offset` set correctly.
load_binary_data_sector:
        ldy     tslist_offset
        bne     :+
        jmp     load_next_tslist_sector
:
        ;; Look up next T/S in list
        lda     (tslist_buf_ptr),y
        beq     finish_binary_load
        sta     rwts_params_track
        iny
        lda     (tslist_buf_ptr),y
        sta     rwts_params_sector
        iny
        sty     tslist_offset

        ;; Load it
        lda     #<rwts_params
        ldy     #>rwts_params
        jsr     DoRWTS
        bcc     :+
        jmp     ErrIOError
:
        inc     rwts_params_data_buf+1
        lda     rwts_params_data_buf+1
        cmp     HIMEM+1
        bcc     load_binary_data_sector

        lda     #BI_ERR_PROGRAM_TOO_LARGE
        rts

finish_binary_load:
        clc
        rts
.endproc

;;; ============================================================
;;; DSAVE Command
;;; ============================================================

.proc DoDSAVE
        ldx     #$00
:       lda     INPUT_BUFFER,y
        iny
        cmp     #scrchar(' ')
        beq     :-
        ;; Shuffle filename down to start of buffer
parse_loop:
        cmp     #$8D            ; CR
        beq     :+
        cmp     #scrchar(',')
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
        lda     FBITS+1         ; DSAVE requires A$/L$ for binary
        bpl     prep_basic      ; so if not passed, it's Applesoft
        ldx     VLNTH+1
        lda     VLNTH
        clc
        adc     #4              ; first 4 bytes are addr/len
        bcc     common2
        bcs     common1         ; always

        ;; Applesoft BASIC save
prep_basic:
        lda     PRGEND+1
        sec
        sbc     TXTTAB+1
        tax
        lda     PRGEND
        sec
        sbc     TXTTAB
        bcs     L35CD
        dex
L35CD:  stx     VLNTH+1
        sta     VLNTH
        clc
        adc     #$02            ; first two bytes are length
        bcc     common2
        ;; fall through to `common1`

common1:
        inx                     ; if C = 1
common2:
        ;; A,X = length
        inx
        cmp     #$00
        beq     :+
        inx                     ; round up
:       stx     file_num_sectors
        cpx     #$7B            ; can't save programs > 122 sectors
        bcc     :+
        lda     #BI_ERR_PROGRAM_TOO_LARGE
        sec
        rts
:
        ;; --------------------------------------------------
        ;; Load VTOC, figure out where to save

        vtoc_buf_ptr := $06

        ldy     HIMEM+1         ; data buffer at `(HIMEM)` + 3 pages
        iny
        iny
        iny
        sty     rwts_params_data_buf+1
        sty     vtoc_buf_ptr+1
        lda     #0
        sta     file_track
        sta     vtoc_buf_ptr
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
        ;; Does this look like a DOS 3.3 disk?
        ldy     #$01      ; +$01 = Track number of first catalog track
        lda     (vtoc_buf_ptr),y
        cmp     #kDOS33VTOCTrack
        beq     :+
io_err: jmp     ErrIOError
:
        iny                     ; +$02 = Sector number of first catalog track
        lda     (vtoc_buf_ptr),y
        cmp     #kDOS33DefaultFirstCatalogSector
        bne     io_err

        ;; See if there is an existing file (i.e. overwrite)
        jsr     delete_previous_catalog_entry
        bcc     :+              ; succcss
        rts
:
        ;; ???
        inc     HIMEM+1
        inc     HIMEM+1
        ldx     file_num_sectors
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
        sbc     #kDOS33VTOCBitMap ; ???
        and     #$FE
        lsr     a               ; /= 2
        lsr     a
        sta     old_file_track
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
        lda     file_track
        bne     L368A
        lda     old_file_track
        sta     file_track
        lda     L36C6
        sta     file_sector
        pla
        bne     L3699
L368A:  lda     old_file_track
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

file_track:     .byte   0
file_sector:    .byte   0

;;; Set if overwriting (only???)
old_file_track: .byte   0
old_file_sector:.byte   0

L36C6:  brk

        ;; ???
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
        lda     file_track
        sta     rwts_params_track
        lda     file_sector
        sta     rwts_params_sector
        dec     rwts_params_data_buf+1
        lda     #<rwts_params
        ldy     #>rwts_params
        jsr     DoRWTS
        bcs     L36E8
        dec     rwts_params_op  ; Write -> Read
        lda     $07
        sta     rwts_params_data_buf+1

        ;; Find a free space in the catalog
        lda     #kDOS33VTOCTrack
        sta     rwts_params_track
        lda     #kDOS33DefaultFirstCatalogSector
        sta     rwts_params_sector
cat_sector_loop:
        ldy     #>rwts_params
        lda     #<rwts_params
        jsr     DoRWTS
        bcc     :+
        jmp     ErrIOError
:
        lda     #kDOS33FirstFileOffset
cat_entry_loop:
        tay
        lda     ($06),y
        beq     :+              ; $00 = entry unused (so free)
        bmi     :+              ; $FF = entry deleted (so free)
        tya
        clc
        adc     #kDOS33FileEntrySize
        bcc     cat_entry_loop
        dec     rwts_params_sector
        bne     cat_sector_loop
        lda     #BI_ERR_DIRECTORY_FULL
        sec
        rts
:
        ;; --------------------------------------------------
        ;; Found free entry; Y = offset in current catalog sector (file track)

        lda     file_track
        sta     ($06),y
        iny                     ; Y = `kDOS33FileEntrySector`
        lda     file_sector
        sta     ($06),y
        iny                     ; Y = `kDOS33FileEntryTypeFlags`
        ldx     FBITS+1
        bpl     :+              ; ADDR not passed, so Applesoft
        lda     #kDOS33FileTypeBinary
        bne     set_type           ; always
:
        lda     #kDOS33FileTypeApplesoft

set_type:
        sta     ($06),y

        ;; Write out filename
        iny                     ; Y = `kDOS33FileEntryName`
        ldx     #0
:       lda     INPUT_BUFFER+1,x
        sta     ($06),y
        iny
        inx
        cpx     INPUT_BUFFER
        bne     :-

        ;; Pad entry with spaces
:       cpx     #kDOS33MaxFilenameLen
        beq     :+
        lda     #scrchar(' ')
        sta     ($06),y
        inx
        iny
        bne     :-
:
        ;; Y = `kDOS33FileEntryLength`
        lda     file_num_sectors
        sta     ($06),y
        iny
        lda     #$00            ; hi byte; always 0
        sta     ($06),y

        ;; Write out the catalog sector
        lda     #RWTSWrite
        sta     rwts_params_op
        lda     #<rwts_params
        ldy     #>rwts_params
        jsr     DoRWTS
        bcc     :+
        jmp     ErrIOError
:
        ;; --------------------------------------------------
        ;; Prepare file data header

        dec     $07
        ldx     FBITS+1
        bpl     prep_applesoft_header ; ADDR not passed, so Applesoft

        ;; Binary - prefix (in memory) with 4-byte header (address/length)
        ;; TODO: Should probably save/restore these bytes.
        ldx     VADDR+1
        lda     VADDR
        sec
        sbc     #4
        bcs     :+
        dex                     ; A,X = VADDR-4
:       sta     $08
        stx     $09
        ldy     #$00
        lda     VADDR
        sta     ($08),y
        iny
        lda     VADDR+1
        sta     ($08),y
        iny
        bne     prep_common_header           ; always

prep_applesoft_header:
        lda     #<$07FF         ; Uses $801 - 2 = $7FF
        sta     $08             ; TODO: Should use `TXTTAB` instead
        lda     #>$07FF
        sta     $09
        ldy     #$00

prep_common_header:
        ;; $08/$09 = start address (including header), and Y = offset
        ;; to length word.
        lda     VLNTH
        sta     ($08),y
        iny
        lda     VLNTH+1
        sta     ($08),y

        ;; --------------------------------------------------

        ;; Prepare File Track/Sector List data ???
        ldy     #$0C            ; `kDOS33TSListFirstDataT` ???
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
        sty     file_num_sectors
        lda     #<rwts_params
        ldy     #>rwts_params
        jsr     DoRWTS
        bcc     :+
        jmp     ErrIOError
:
        inc     $09
        ldy     file_num_sectors
        bne     L37C6
L37F4:  lda     #$00
        sta     $0800
        clc
        rts

file_num_sectors:
        .byte   0

;;; --------------------------------------------------
;;; Search the catalog for matching file entry, and delete it
;;; if one is found.
;;; Output: C=0 on not found or success, C=1 on error

delete_previous_catalog_entry:
        cat_sector_ptr := $19

        ldy     HIMEM+1
        iny
        iny
        sty     rwts_params_data_buf+1
        sty     cat_sector_ptr+1
        lda     #$00
        sta     rwts_params_data_buf
        sta     cat_sector_ptr

        lda     #kDOS33VTOCTrack
        sta     rwts_params_track
        lda     #kDOS33DefaultFirstCatalogSector
        sta     rwts_params_sector

catalog_sector_loop:
        lda     #<rwts_params
        ldy     #>rwts_params
        jsr     DoRWTS
        bcc     :+
        jmp     ErrIOError
:
        ldy     #kDOS33NextCatSectorTrack
        lda     (cat_sector_ptr),y
        cmp     #kDOS33VTOCTrack
        beq     :+
        jmp     ErrIOError
:
        lda     #kDOS33FirstFileOffset + kDOS33FileEntryName
        ;; Check for filename match

entry_loop:
        pha                     ; stash current offset in sector
        tay                     ; Y = offset to filename
        ldx     #0
cloop:  lda     (cat_sector_ptr),y
        bne     :+
        pla                     ; $00 in name; invalid entry, just fail
        clc
        rts
:       cmp     INPUT_BUFFER+1,x
        bne     next_entry
        inx
        iny
        cpx     INPUT_BUFFER
        bne     cloop

possible_match:
        cpx     #kDOS33MaxFilenameLen
        beq     found_match
        lda     (cat_sector_ptr),y
        inx
        iny
        cmp     #scrchar(' ')
        beq     possible_match

next_entry:
        pla                     ; A = current offset in sector
        clc
        adc     #kDOS33FileEntrySize
        bcc     entry_loop

        ;; Next catalog sector
        dec     rwts_params_sector
        bne     catalog_sector_loop
        clc
        rts

        ;; ----------------------------------------
        ;; Found a matching filename

found_match:
        pla
        tay
        dey                     ; Y = +`kDOS33FileEntryTypeFlags`
        lda     (cat_sector_ptr),y
        bmi     ErrFileLocked   ; high bit set if locked
        cmp     #kDOS33FileTypeBinary
        beq     check_binary
        cmp     #kDOS33FileTypeApplesoft
        bne     ErrFileTypeMismatch

        ;; Existing file is Applesoft - is that what we're DSAVEing?
        lda     FBITS+1
        bpl     overwrite_type_okay ; ADDR not passed - Applesoft!
        ;; fall through to ErrFileTypeMismatch

ErrFileTypeMismatch:
        lda     #BI_ERR_FILE_TYPE_MISMATCH
        sec
        rts

ErrFileLocked:
        lda     #BI_ERR_FILE_LOCKED
        sec
        rts

bad_entry:
        iny
        iny
        iny                     ; Y = +`kDOS33FileEntryName`
        tya
        pha
        bne     next_entry      ; always

        ;; Existing file is binary - is that what we're DSAVEing?
check_binary:
        lda     FBITS+1
        bpl     ErrFileTypeMismatch ; ADDR not passed

overwrite_type_okay:
        dey
        dey                     ; Y = +`kDOS33FileEntryTrack`
        tya
        pha
        clc
        adc     #kDOS33FileEntryLength
        tay
        lda     (cat_sector_ptr),y
        cmp     #$7B            ; can't save(?) programs > 122 sectors
        bcc     :+
        pla
        lda     #BI_ERR_PROGRAM_TOO_LARGE
        sec
        rts
:
        pla
        tay                     ; Y = +`kDOS33FileEntryTrack`
        lda     (cat_sector_ptr),y
        bmi     bad_entry       ; $FF = already deleted entry
        sta     old_file_track
        lda     #$FF            ; Mark entry as deleted
        sta     (cat_sector_ptr),y
        iny                     ; Y = +`kDOS33FileEntrySector`
        lda     (cat_sector_ptr),y
        sta     old_file_sector

        ;; Write out updated catalog sector with entry deleted
        lda     #RWTSWrite
        sta     rwts_params_op
        lda     #<rwts_params
        ldy     #>rwts_params
        jsr     DoRWTS
        bcc     :+
        jmp     ErrIOError
:
        ;; Now read old file's track/sector list
        dec     rwts_params_op  ; Write -> Read
        lda     old_file_track
        sta     rwts_params_track
        lda     old_file_sector
        sta     rwts_params_sector
        lda     #<rwts_params
        ldy     #>rwts_params
        jsr     DoRWTS
        bcc     :+
        jmp     ErrIOError
:
        ;; Iterate over T/S list, mark sectors free in VTOC
        explicit_patch1 := *+2  ; relocation patch
        jsr     mark_free_in_vtoc
        ldy     #kDOS33TSListFirstDataT
sloop:
        lda     (cat_sector_ptr),y
        beq     :+
        sta     old_file_track
        iny
        lda     (cat_sector_ptr),y
        sta     old_file_sector
        iny
        sty     $08
        explicit_patch2 := *+2  ; relocation patch
        jsr     mark_free_in_vtoc
        ldy     $08
        bne     sloop

:       clc
        rts

;;;  Mark `old_file_track`/`old_file_sector` as free in VTOC
.proc mark_free_in_vtoc
        lda     old_file_track
        asl     a
        asl     a               ; *= 4
        clc
        adc     #kDOS33VTOCBitMap
        tay                     ; Y = VTOC bitmap offset for track

        ;; Byte   Sectors
        ;; +0     FEDC BA98
        ;; +1     7654 3210
        ;; +2     .... .... (not used)
        ;; +3     .... .... (not used)

        lda     old_file_sector
        cmp     #$8
        bcs     :+
        iny                     ; sectors < 8 in second byte
:       and     #$07
        tax                     ; X = sector % 8
        lda     #%00000001
:       dex
        bmi     :+
        asl     a               ; shift bit into position
        bne     :-
:
        ;; set bit to indicate free
        ora     (vtoc_buf_ptr),y
        sta     (vtoc_buf_ptr),y
        rts
.endproc

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

;;; NOTE: Appears to be hardcoded to use 2 pages at HIMEM despite
;;; the parameters it is called with.

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
rwts_track_times_8:  brk
tmp_sector:  brk

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

;;; ProDOS Technical Reference Manual
;;; Appendix B.5 "DOS 3.3 Disk Organization"
;;;
;;; Figure B-15 shows how to determine a block number from a given
;;; track and sector. First multiply the track number by 8, then add
;;; the Sector Offset that corresponds to the sector number. The half
;;; of the block in which the sector resides is determined by the
;;; Half-of-Block line (1 is the first half; 2 is the second).
;;;
;;; Figure B-15. Tracks and Sectors to Blocks
;;;
;;;       Block = (8 * Track) + Sector Offset
;;;
;;;        Sector : 0 1 2 3 4 5 6 7 8 9 A B C D E F
;;; Sector Offset : 0 7 6 6 5 5 4 4 3 3 2 2 1 1 0 7
;;;  Half of Block: 1 1 2 1 2 1 2 1 2 1 2 1 2 1 2 2

;;; NOTE: Return value has "Half" in X as 0 or 1, not 1 or 2

;;; NOTE: Most of this code can be replaced by simple lookup tables.

        sty     tmp_sector
        tax                     ; X = track
        ldy     #$00
        tya                     ; A = 0

        ;; First compute 8 * Track
        ;; TODO: Replace with a bit shift
:       dex
        bmi     :+
        clc
        adc     #8
        bcc     :-
        iny
        bne     :-
:
        sta     rwts_track_times_8

        ;; Y = 0 here
        ldx     #0              ; page_num = 0 (first half)

        ;; Sector = $0?
        lda     tmp_sector
        bne     :+
        ;; A,Y = Block = (8 * Track) + 0; X = Half-1 = 0
        lda     rwts_track_times_8
        rts
:
        ;; Sector = $1?
        cmp     #$1
        bne     not_s1
        ;; A,Y = Block = (8 * Track) + 7, X = Half-1 = 0
        lda     rwts_track_times_8
        clc
        adc     #7
        bcc     :+
        iny
:       rts
not_s1:
        ;; Sector = $E?
        cmp     #$E
        bne     :+
        ;; A,Y = Block = (8 * Track) + 0, X = Half-1 = 1
        lda     rwts_track_times_8
        inx
        rts
:
        ;; Sector = $F?
        cmp     #$F
        bne     not_sf
        ;; A,Y = Block = (8 * Track) + 7, X = Half-1 = 1
        lda     rwts_track_times_8
        clc
        adc     #7
        bcc     :+
        iny
:       inx
        rts
not_sf:

        ;; Sectors $1..$D
        ;; if even: Offset = (14 - sector) / 2
        ;; if odd:  Offset = (15 - sector) / 2
        lda     tmp_sector
        lsr     a               ; sector / 2
        bcs     odd             ; sector odd, so Half = 1
        lda     #14             ; sector even, so Half = 2
        inx                     ; X = 1 (Half = 2)
        bne     :+              ; always
odd:    lda     #15
:       sec
        sbc     tmp_sector
        lsr     a               ; A = Offset
        ;; A,Y = Block = (8 * Track) + Offset, X = Half-1
        clc
        adc     rwts_track_times_8
        bcc     :+
        iny
:       rts
.endproc

reloc_end := $3AFF
