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
kDOS33VTOCBitMapTrack0  = $38   ; +$38...$3B for track 0
kDOS33VTOCBitMapTrack1  = $3C   ; +$3C...$3F for track 1 ...
kDOS33VTOCBitMapTrack34 = $C0   ; +$C0...$C3 for track 34

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
