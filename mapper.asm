

/*
This code handles managing the visible part of the set, calculating it and drawing to screen.
In addition to the BaseX and BaseY values defined in math.asm which represent the top left corner,
we have a "delta" which is the value which one pixel adds to that value.

We limit delta to always be a power of 2, so it can be represented by a single u8, ie:
	delta = 2^-N
This is trivial to add to the current CX/CY, fast and without rounding errors.

Initial conditions are:
	BaseX = -2.25
	BaseY = -2.125
	Delta = 1/64
This gives us an initial bounding box (-2.25, -2.125) to (2.25, 2.125).

Due to memory constraints, we store the calculation output directly as pixels in the VRAM.
Due to not being able to write to VRAM except during VBlank, we use a circular buffer
to buffer pending writes.

We process points in top-to-bottom, left-to-right, not in the order they're laid out in memory
(in blocks of 8x8). This optimizes for less operations involving "what's the current point"
even though it makes writing to screen slower.
*/


SECTION "Mapper Data", WRAM0


// How many bits deep (MSB first) to increment when moving from one point to the next.
// Equivalently, delta = 4 >> DeltaExp = 2^(2 - DeltaExp)
DeltaExp::
	db

// How many bytes to use when doing math. This number represents UI state
// and we need to be careful when it's updated that we zero-initialize any newly-added
// bytes in BaseX / BaseY.
Precision::
	db

// Thresholds for coloring a pixel based on the number of iterations.
// They split the space into four colors:
//   iterations >= PaletteThresholds[0]
//   PaletteThresholds[0] > iterations >= PaletteThresholds[1]
//   PaletteThresholds[1] > iterations >= PaletteThresholds[2]
//   PaletteThresholds[2] > iterations
PaletteThresholds::
	ds 3

// Head and tail indexes into VRAM Buffer.
// Valid values are in the range [head, tail), and head == tail means empty.
// Note the following properties as a result:
//  It is always safe to write to buffer[tail], even if the queue is full.
//    But you would not be able to *commit* these writes until head has advanced.
//  The number of items in the queue is tail - head.
VRAMBufferHead::
	db
VRAMBufferTail::
	db

// Pointer to the next address to write values from VRAMBuffer into.
VRAMWriteAddr::


SECTION "Mapper VRAM Buffer", WRAM0, ALIGN[8]

// 256 bytes to write into VRAM. The order in which things are written matters,
// as the reader copies them into VRAM in a prescribed order (in
VRAMBuffer::
	ds 256


SECTION "Mapper code", ROM0

MapperInit:
	// Init VRAM Buffer
	xor A
	ld [VRAMBufferHead], A
	ld [VRAMBufferTail], A

	// Set initial Delta = 1/64 = 2^-6, so DeltaExp = 8
	ld A, 8
	ld [DeltaExp], A

	// Set initial precision of 1 (2 bytes), which is the bare minimum to be able to represent default Delta.
	ld A, 1
	ld [Precision], A
	ld C, A

	// BaseX is -2.25 = -0b10.01, BaseY is -2.125 = -0b10.001
	ld D, 1
	ld E, %10010000
	ld H, BaseX
	call MathSet
	ld E, %10001000
	ld H, BaseY
	call MathSet

	ret


; All inputs come from globals:
;  BaseX, BaseY, DeltaExp, Precision, PaletteThresholds
PopulateMap:
	ld A, [Precision]
	ld C, A

	; DE contains the pixel value pair (MSB and LSB respectively, for each pixel) we are
	; preparing to write.
	; Since we shift both left by 1 each loop, and write it every 8 loops, there's no need
	; to initialize or clear it.

	; H and L contain Y and X loop counters respectively.
	MathCall MathCopy, CY, BaseY
	ld H, 144
	jr .y_start

.y_loop
	push HL
	ld A, [DeltaExp]
	MathSingleCall MathAddPowerOfTwo CY
	pop HL

.y_start
	ld L, 160
	push HL
	MathCall MathCopy, CX, BaseX
	jr .x_start

.x_loop
	push HL

	ld A, [DeltaExp]
	MathSingleCall MathAddPowerOfTwo CX

.x_start
	; First palette threshold is also max iteration count
	ld A, [PaletteThresholds]
	ld B, A

	; Returns remaining iterations in B
	push DE
	call GetIterations
	pop DE

	; TODO translate B into a pixel value 0-3

	; Split B into two bits and shift them into D and E
	rr B ; c = LSB of B
	rl E ; push the bit into the bottom of E
	rr B ; c = MSB of B
	rl D ; push the bit into the bottom of D

	pop HL

	; check if we're on an 8th iteration
	ld A, %00000111
	and L ; A = bottom 3 bits of L
	dec A ; set z if L % 8 == 1 (ie. we're about to decrement L % 8 to 0)
	jr nz, .no_push

	; Push to buffer
	push HL
	ld H, HIGH(VRAMBuffer)
	ld L, VRAMBufferTail
	; It's always safe to write to the tail entry, even if actually incrementing tail would
	; make us full.
	ld [HL], E
	inc L
	ld [HL], D
	inc L ; L = new tail value
	jr .wait_start
.wait_loop
	ei ; note we have a 1 instruction grace period after ei before interrupts can fire
	halt ; block until next interrupt
.wait_start
	di ; critical section - we don't want to observe it's full, get interrupted, THEN wait for interrupt.
	ld A, [VRAMBufferHead]
	cp L ; set z if head == new tail, ie. queue is full.
	jr z, .wait_loop
	ei
	; We have now confirmed that the new tail value != head, so we can safely commit.
	ld A, L
	ld [VRAMBufferTail], A
	pop HL

.no_push
	dec L
	jr nz, .x_loop

	dec H
	jr nz, .y_loop

	ret
