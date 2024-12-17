;;; DOS 3.3 details c/o Beneath Apple DOS by Don Worth and Pieter Lechner

.scope dos33

VTOCTrack       = 17
VTOCSector      = 0
MaxFilenameLen  = 30

DefaultFirstCatalogSector  = $F ; TODO: Use data in VTOC instead

;;; Volume Table Of Contents (VTOC) Format
;;; TODO: This should be a struct but the offsets would be weird
.scope VTOC
FirstCatTrack   = $01 ; +$01 Track number of first catalog sector (usually $11)
FirstCatSector  = $02 ; +$02 Secto number of first catalog sector (usually $F)
VolumeNumber    = $06 ; +$06 Diskette volume number (1-254)
BitMap          = $38 ; +$38 Bit map of free sectors (4 bytes per track)
BitMapTrack0    = $38 ; +$38...$3B for track 0
BitMapTrack1    = $3C ; +$3C...$3F for track 1 ...
BitMapTrack34   = $C0 ; +$C0...$C3 for track 34
.endscope

;;; Catalog Sector Format
NextCatSectorTrack      = $01 ; Track number of next catalog sector (usually $11)
FirstFileOffset         = $0B ; First file descriptive entry

;;; File Descriptive Entry Format
.struct FileEntry
Track           .byte  ; +$00 Track of first track/sector list sector
Sector          .byte  ; +$01 Sector of first track/sector list sector
TypeFlags       .byte  ; +$02 File type and flags
Name            .res dos33::MaxFilenameLen ; +$03-$20 File name (30 characters)
Length          .word     ; +$21-$22 Length of file in sectors (LO/HI)
.endstruct

;;; File Track/Sector List Format
;;; TODO: This should be a struct but the offsets would be weird
.scope TSList
Unused          = $00   ; Not used
NextTrack       = $01   ; Track number of next T/S List sector (or 0)
NextSector      = $02   ; Sector number of next T/S List sector (or 0)
FirstDataT      = $0C   ; Track of first data sector (or 0)
FirstDataS      = $0D   ; Sector of first data sector
.endscope

;;; File types
FileTypeText            = $00
FileTypeInteger         = $01
FileTypeApplesoft       = $02
FileTypeBinary          = $04
FileTypeS               = $08
FileTypeRelocatable     = $10
FileTypeA               = $20
FileTypeB               = $40

.endscope
