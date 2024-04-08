

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


; How many bits deep (MSB first) to increment when moving from one point to the next.
; Equivalently, delta = 4 >> DeltaExp = 2^(2 - DeltaExp)
DeltaExp::
	db

; How many bytes to use when doing math. This number represents UI state
; and we need to be careful when it's updated that we zero-initialize any newly-added
; bytes in BaseX / BaseY.
Precision::
	db

; Thresholds for coloring a pixel based on the number of iterations.
; They split the space into four colors:
;   iterations >= PaletteThresholds[0]
;   PaletteThresholds[0] > iterations >= PaletteThresholds[1]
;   PaletteThresholds[1] > iterations >= PaletteThresholds[2]
;   PaletteThresholds[2] > iterations
PaletteThresholds::
	ds 3

; Head and tail indexes into VRAM Buffer.
; Valid values are in the range [head, tail), and head == tail means empty.
; Note the following properties as a result:
;  It is always safe to write to buffer[tail], even if the queue is full.
;    But you would not be able to *commit* these writes until head has advanced.
;  The number of items in the queue is tail - head.
VRAMBufferHead::
	db
VRAMBufferTail::
	db

; State for VRAM writer.
; Addr is next address to write (little-endian, as per ld [nn], SP).
; X is number of tiles (1-20) remaining in current row.
; Bank is 0 or 1 for first or second half of screen respectively.
; Note order matters here due to read optimizations.
VRAMWriteAddr::
	dw
VRAMWriteX::
	db
VRAMWriteY::
	db
VRAMWriteBank::
	db


SECTION "Mapper VRAM Buffer", WRAM0, ALIGN[8]

; 256 bytes to write into VRAM. The order in which things are written matters,
; as the reader copies them into VRAM in a prescribed order: Each pair of bytes is one
; row of one tile, and are written row by row (top-to-bottom), NOT tile by tile.
VRAMBuffer::
	ds 256


SECTION "Mapper code", ROM0

MapperInit:
	; Init VRAM Buffer
	xor A
	ld [VRAMBufferHead], A
	ld [VRAMBufferTail], A

	; Set initial Delta = 1/64 = 2^-6, so DeltaExp = 8
	ld A, 8
	ld [DeltaExp], A

	; Set initial precision of 1 (2 bytes), which is the bare minimum to be able to represent default Delta.
	ld A, 1
	ld [Precision], A
	ld C, A

	; BaseX is -2.25 = -0b10.01, BaseY is -2.125 = -0b10.001
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
;  Note that all internal state in this function is kept in registers/stack.
;  This is important as this function can be LongJmp'd out of and restarted.
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

	; Actual calculation. Returns remaining iterations in B
	push DE
	call GetIterations
	pop DE

	; B is number of *remaining iterations*, which is max iterations - actual iterations.
	; First case is that B is zero and therefore iterations >= max iterations.
	ld A, B
	and A ; set z if zero
	jr z, .threshold_done ; we know B = 0 here, which is the result we want

	; For the remaining comparisons, we want to compare to actual iterations.
	; A = actual iterations = max iterations - B
	ld A, [PaletteThresholds]
	sub B

	; if A >= PaletteThresholds[1], B = 1
	ld B, 1
	cp [PaletteThresholds + 1]
	jr nc, .threshold_done

	; if A >= PaletteThresholds[2], B = 2
	inc B
	cp [PaletteThresholds + 2]
	jr nc, .threshold_done

	; otherwise, B = 3
	inc B

.threshold_done
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


; Must be called while VRAM is writable (during vblank, or screen is off).
; Interrupts must be disabled.
; Assumes a time limit of 2000 cycles remaining during VBlank,
; and flushes as much of the buffer as it can within that time.
; Timing analysis:
;   Preamble: 56 cycles
;   Per two pairs: 38 cycles (avg 19 per pair)
;   Extra per row: 17 cycles
;   Per complete row: 10x(per two pairs) + extra per row = 397 cycles
;   If bank is crossed: 17 cycles
;   Averaging out the per row effects and assuming we do hit a bank cross:
;     Total: preamble + bank cross + pairs * (complete row) / 20 = 73 + 19.85 * pairs
;   In CGB double speed mode, VBlank is about 2280 cycles. Assume we get ~2000 of that.
;   Then a rough cap is 98 pairs ~= 2018 cycles.
FlushVRAMBuffer:
	; Determine buffer length. Also stash VRAMBufferHead in D.
	ld A, [VRAMBufferHead]
	ld D, A
	ld A, [VRAMBufferTail]
	sub D
	ret z ; return if length == 0, ie. there's nothing to do

	; Halve buffer length. Note we assume buffer length is always even as we always write pairs.
	; Since it's even, rotation puts 0 in the top bit and saves a cycle over srl A.
	; Enforce a max value due to time constraints.
	rrca
	cp 98
	jr nc, .no_cap
	ld A, 98
.no_cap
	ld B, A ; B = number of pairs to read.

	; Save stack pointer
	ld [FlushVRAMBufferSP], SP

	; Load state. Use loads from HL, faster than multiple ld A, [nn].
	ld HL, VRAMWriteBank
	; Set VRAM bank
	ld A, [HL-]
	ld [CGBVRAMBank], A
	; C = Y counter
	ld A, [HL-]
	ld C, A
	; E = X counter, temporarily
	ld A, [HL-]
	ld E, A
	; SP = write addr
	ld A, [HL-]
	ld H, [HL]
	ld L, A
	ld SP, HL

	; HL = read addr. We stashed VRAMBufferHead in D earlier.
	ld H, HIGH(VRAMBuffer)
	ld L, D

	xor A ; clear carry flag
	ld A, E ; A = X counter
	rra ; A = X counter / 2, set carry if counter was odd
	jr c, .odd_start
	jr .even_start

.two_pair_loop
	; Check if buffer is fully written. We do this right before writing the next one
	; so all internal state is ready for the next write.
	dec B
	jr z, .even_done

.even_start
	ld E, [HL]
	inc L
	ld D, [HL]
	inc L ; explicitly wrap on overflow without carrying
	push DE ; write 2 bytes
	add SP, 14 ; skip the next 14 bytes

	dec B
	jr z, .odd_done

.odd_start
	ld E, [HL]
	inc L
	ld D, [HL]
	inc L ; explicitly wrap on overflow without carrying
	push DE ; write 2 bytes
	add SP, 14 ; skip the next 14 bytes

	; check if row is finished
	dec A
	jr nz, .two_pair_loop

	; reset halved X counter
	ld A, 10

	; Adjust write addr to go back one row, plus 2. 16 * 20 + 2 = 322.
	; add SP, n adds a signed 8-bit value, so to subtract 322 we do SP-127-127-68.
	; This is 12 cycles but the alternatives aren't any better.
	add SP, -127
	add SP, -127
	add SP, -68
	; Decrement row counter, if zero then switch banks.
	dec C
	jr nz, .two_pair_loop

	; Switch banks
	ld A, 1
	ld [VRAMWriteBank], A
	ld [CGBVRAMBank], A
	ld SP, BaseTileMap
	; No need to reset C here, it's now 255 and we assume over-writing can't happen.
	; We did clobber A though, so reset it again.
	ld A, 10
	jr .two_pair_loop

	; Save state. We're done writing so we can relax timing restrictions.
	; Need to slightly adjust the final X counter depending on if we ended on an odd or even iteration.
.odd_done
	; Need to add 1 to X counter when restoring it
	or B ; we know B = 0, so A |= B is a no-op but clears carry
	jr .done
.even_done
	scf ; set carry
.done
	rla ; X = 2 * X + carry
	ld [VRAMWriteX], A
	ld A, C
	ld [VRAMWriteY], A
	ld A, L
	ld [VRAMBufferHead], A
	ld [VRAMBufferAddr], SP
	ld A, [FlushVRAMBufferSP]
	ld L, A
	ld A [FlushVRAMBufferSP+1]
	ld H, A
	ld SP, HL
	ret
