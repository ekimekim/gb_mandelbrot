include "ioregs.asm"
include "vram.asm"
include "longcalc.asm"

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
	call MapperInit

	ld A, IntEnableVBlank
	ld [InterruptsEnabled], A

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


VBlank::
	push AF
	push BC
	push DE
	push HL
	call FlushVRAMBuffer
	pop HL
	pop DE
	pop BC
	pop AF
	reti


; Assumes screen is off.
InitGraphics:
	; Construct a direct pixel map to screen by mapping tiles 0 to 179 to the first 9 rows,
	; then the same tiles in bank 1 to the second 9 rows.
	ld HL, TileGrid - 12
	ld D, 0
.grid_loop_bank
	ld C, 9
	ld E, 0
.grid_loop_row
	ld B, 20
	LongAdd HL, 12, HL ; HL += 12
.grid_loop_tile
	; set tile number
	xor A
	ld [CGBVRAMBank], A
	ld A, E
	ld [HL], A
	inc E
	; set bank
	ld A, 1
	ld [CGBVRAMBank], A
	ld A, D
	swap A
	srl A ; set bit 3 to D
	ld [HL+], A
	dec B
	jr nz, .grid_loop_tile
	dec C
	jr nz, .grid_loop_row
	inc D
	ld A, 2
	cp D
	jr nz, .grid_loop_bank

	; Initialize pixels to 0 so startup screen isn't random noise
	ld D, 2
.pixel_loop_bank
	ld A, 2
	sub D
	ld [CGBVRAMBank], A
	ld HL, BaseTileMap
.pixel_loop
	xor A
REPT 16
	ld [HL+], A
ENDR
	ld A, H
	cp HIGH(AltTileMap)
	jr c, .pixel_loop
	dec D
	jr nz, .pixel_loop_bank

	ret
