include "ioregs.asm"
include "vram.asm"

Section "Stack", WRAM0

StackBase:
	ds 128
Stack::


Section "Main", ROM0

Start::

	; Disable LCD and audio.
	; Disabling LCD must be done in VBlank.
	; On hardware start, we have about half a normal vblank, but this may depend on the hardware variant.
	; So this has to be done quick!
	xor A
	ld [SoundControl], A
	ld [LCDControl], A

	; Use core stack
	ld SP, Stack

	; Init things
	call InitGraphics
	call InitMapper

	; Turn on screen
	ld HL, LCDControl
	set 7, [HL]
	; Clear pending interrupts
	xor A
	ld [InterruptFlags], A
	; Go
	ei

	call PopulateMap
.main
	halt
	jp .main


; Assumes screen is off.
InitGraphics:
	; Construct a direct pixel map to screen by mapping tiles 0 to 179 to the first 9 rows,
	; then the same tiles in bank 1 to the second 9 rows.
	ld HL, TileGrid - 12
	ld D, 1
.grid_loop_bank
	ld A, D
	ld [CGBVRAMBank], A
	ld C, 9
	xor A
.grid_loop_row
	ld B, 20
	LongAdd HL, 12, HL ; HL += 12
.grid_loop_tile
	ld [HL+], A
	inc A
	dec B
	jr nz, .grid_loop_tile
	dec C
	jr nz, .grid_loop_row
	dec D
	jr nz, .grid_loop_bank

	; Initialize pixels to 0 so startup screen isn't random noise
	ld D, 1
.pixel_loop_bank
	ld A, D
	ld [CGBVRAMBank], A
	ld HL, BaseTileMap
.pixel_loop
	xor A
REPT 16
	ld [HL-], A
ENDR
	ld A, H
	cp HIGH(AltTileMap)
	jr nc, .pixel_loop
	dec D
	jr nz, .pixel_loop_bank
