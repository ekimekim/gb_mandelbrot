

; To compute the mandelbrot set iteration (z = z^2 + c), we need to support three operations:
; * Add two complex numbers
;   This is easy to do pairwise
; * Square a complex number
;   In real number terms, (x + iy)^2 = (x^2 - y^2) + i(2xy)
; * Decide whether the value has escaped
;   The standard test for this is |z| > 2, aka x^2 + y^2 > 4.
; Since we need to use x^2 and y^2 twice, we can re-use the values. Something like:
; 1. Cacluate x^2 and y^2
; 2. Check if x^2 + y^2 > 4, if so then break
; 3. x = x^2 - y^2 + real(c)
; 4. y = 2xy + imag(c)
; 5. Increment iteration count, repeat.

; To expand the above into explicit pseudocode, noting where we can update in place:
;  Operation         On carry, do:
;  ysq = square(y)   break (because |y| > 2)
;  y *= x            break (because |xy| <= max(x,y)^2, so |xy| > 4 implies x^2 > 4 or y^2 > 4)
;  x = square(x)     break (because |x| > 2)
;  add(x, ysq)       break (because |z| > 2) (we're only doing this to check for |z|, can we omit it and just break when |x| > 2 or |y| > 2?)
;  inc count
;  y *= 2            break (because |imag_c| < 2, so |this value| > 4 implies |new y| > 2)
;  y += imag_c       break (because new y > 4)
;  x -= ysq          break (because new x < -4)
;  x += real_c       break (because |new x| > 4)
;  loop

; From above, the primitives we need to implement (including potentially more-specific versions
; that can give us a speed increase):
; * square(a) -> b
; * add(a, b) -> null
; * multiply(a, b) -> a
; * double(a) -> a
; * add(a, b) -> a
; * sub(a, b) -> a

; We need to support a configurable precision in order to zoom in a lot.
; We do this by using sequences of base-256 digits, with a fixed point 2 bits in
; (allowing value range -4 <= value < 4, which is needed since x and y are within -2 to 2,
;  and we need to be able to store the square of the value).
; Longer sequences give us more precise results, though for practical reasons
; we'll only support up to 256 digits (for a precision of 2^-2046).
; For simplicity, sign bits are stored externally.

SECTION "Math Vectors", WRAM0, ALIGN[8]

; For speed, all these addresses are page-aligned.
; They're generally addressed by their vector number, in order of definition here.
; The vector address is calculated as high = HIGH(VectorsBase) + vector number, low = 0.
; The sign byte is calculated as high = HIGH(SignBytes), low = vector number.

VectorsBase:
RSRESET

; Base X/Y store the bottom-left corner coordinate of the screen
BaseX rb 1
	ds 256
BaseY rb 1
	ds 256

; CX/CY store the point under current consideration
CX rb 1
	ds 256
CY rb 1
	ds 256

; X, Y and YSq are used during iteration as described above
X rb 1
	ds 256
Y rb 1
	ds 256
YSq rb 1
	ds 256


; This is wastefully one bit per byte, but meh.
; Each byte corresponds to a vector, in the same order as above.
SignBytes:
	ds 7


SECTION "Math Methods", ROM0


; General calling conventions for Math* functions:
;  H: Vector number of output (may also be first input)
;  D: Vector number of input (or second input)
;  B: Preserved.
;  C: How many bytes of precision to calculate - 1 (ie. 1 = 2 bytes). Min 1. Preserved.
;  All non-preserved are clobbered.
;  Carry is set on overflow (absolute result >= 4), cleared otherwise.

; MathCall FUNCTION OUTPUT INPUT
; Loads output and input vector numbers before calling given Math* function.
MathCall: MACRO
	ld H, \2
	ld D, \3
	call \1
ENDM
; As above for single-arg math functions
MathSingleCall: MACRO
	ld H, \2
	call \1
ENDM


; Given CX and CY, calculate how many iterations it takes for Z to escape.
; Takes a max number of iterations in B (with 0 meaning 256). Decrements B
; for each iteration, so on return B=0 always means "hit max iterations".
; Preserves C, clobbers otherwise.
GetIterations:
	; special case first iteration, where z = 0 so iteration is z = 0^2 + c = c
	MathCall MathCopy, X, CX
	MathCall MathCopy, Y, CY
	dec B
	; main loop. note we break on any overflow (see above)
.loop
	; ysq = (prev y)^2
	MathCall MathSquare, YSq, Y
	ret c
	; y = prev y * prev x, we do this early so we can re-use X
	MathCall MathMultiply, Y, X
	ret c
	; x = (prev x)^2
	MathCall MathSquare, X, X
	ret c
	; Add prev x^2 and prev y^2, checking for overflow but discard result
	MathCall MathAddNoOut, X, YSq
	ret c
	; We're now past the part where we're checking if previous iteration escaped,
	; and into checking next iteration. So decrement the iteration count and return
	; if we've reached the limit (B=0)
	dec B
	ret z
	; y = 2 * prev y * prev x
	MathSingleCall MathDouble, Y
	ret c
	; y = 2 * prev y * prev x + cy = next y
	MathCall MathAdd, Y, CY
	ret c
	; x = (prev x)^2 - (prev y)^2
	MathCall MathSub, X, YSq
	ret c
	; x = (prev x)^2 - (prev y)^2 + cx = next x
	MathCall MathAdd, X, CX
	jr nc, .loop
	ret


; Macros for common parts of Math functions

; Convert (vector number, *) reg pair into address of sign byte
NumToSignAddr: MACRO
	ld \2, \1
	ld \1, HIGH(SignBytes)
ENDM

; Convert vector number in \1 into vector address high byte
NumToVecHigh: MACRO
	ld A, HIGH(VectorsBase)
	add \1
	ld \1, A
ENDM

; Convert sign byte address \1 \2 into vector address high byte, put into \1
SignAddrToVecHigh: MACRO
	ld A, HIGH(VectorsBase)
	add \2
	ld \1, A
	xor A
	ld \2, A
ENDM

; Convert vector address high byte \1 into sign byte address \1 \2
VecHighToSignAddr: MACRO
	ld A, \1
	sub HIGH(VectorsBase) ; left with just the vec number
	ld \2, A
	ld \1, HIGH(SignBytes)
ENDM


; Copy input to output
MathCopy:
	; get sign addresses
	NumToSignAddr H, L
	NumToSignAddr D, E
	; copy sign
	ld A, [DE]
	ld [HL], A
	; get vec addresses
	SignAddrToVecHigh H, L
	SignAddrToVecHigh D, E
	; fallthrough
; sub-function that just does the Vec part, takes HL and DE vec addresses.
VecCopy:
	; copy vec, starting from C
	ld L, C
	ld E, C
.loop
	ld A, [DE]
	ld [HL-], A
	dec E
	jr nz, .loop
	; final loop
	ld A, [DE]
	ld [HL], A
	; the only thing we've done that affects carry is SignAddrToVecHigh, which will always clear it
	ret


; Double output (left shift by 1). Note only takes one arg (DE is ignored).
MathDouble:
	; note NumToVecHigh will clear carry for us
	NumToVecHigh H
	ld L, C
.loop
	; shift left once, putting carry in LSB and putting MSB in carry
	rl [HL]
	dec L ; note doesn't affect carry
	jr nz, .loop
	; final loop
	rl [HL]
	; final carry is output
	ret


; output = output - input
; This is equivalent to output + (-input).
; We get the sign bits just like the start of MathAdd, but flip the input sign.
; Then we just proceed in MathAdd as normal.
MathSub:
	NumToSignAddr H, L
	NumToSignAddr D, E
	ld A, [DE]
	xor 1 ; 0 -> 1, 1 -> 0
	jr MathAddSubCommon


; Add input to output in place
MathAdd:
	; 4 cases:
	;  +/+: Add vecs, output +, on carry output carry
	;  -/-: Add vecs, output -, on carry output carry
	;  +/-: vec = other vec - vec, on carry negate vec and output +, otherwise output -, output no carry
	;  -/+: vec = other vec - vec, on carry negate vec and output -, otherwise output +, output no carry

	; get sign addresses
	NumToSignAddr H, L
	NumToSignAddr D, E

	ld A, [DE]
; common part to MathAdd and MathSub starts here
MathAddSubCommon:
	xor [HL] ; set z if both + or both -
	jr nz, .different_signs

	; This is the easy case. The sign of the output won't change, so that's done.
	; All that's left is to add the unsigned vectors, and pass the carry result back unchanged.
	; In fact, we can tail call.
	SignAddrToVecHigh H, L
	SignAddrToVecHigh D, E
	jr VecAdd
	; tail call

.different_signs

	; Keeping in mind we can't modify our input, only our output, first step
	; is to subtract the vectors so we have (output - input). This may underflow.
	SignAddrToVecHigh H, L
	SignAddrToVecHigh D, E
	call VecSub

	jr c, .underflow

	; No underflow, this means we need to flip the output sign (the other vec was dominant)
	VecHighToSignAddr H, L
	ld A, 1
	sub [HL]
	ld [HL], A
	; Note carry flag is still clear from VecHighToSignAddr above
	ret

.underflow
	; Negate the vector so it's positive again, and leave output sign unchanged (our vec was dominant)
	call VecNegate
	; Make sure carry flag is clear
	xor A
	ret


; Internal helper. Obeys same calling conventions as Math functions, except HL and DE
; should be vector addresses. Adds DE vector to HL vector in place (does NOT look at sign).
; Must be called with clear carry flag. Returns carry as expected.
VecAdd:
	ld L, C
	ld E, C
.loop
	ld A, [DE]
	adc [HL]
	ld [HL-], A
	dec E
	jr nz, .loop
	; final loop
	ld A, [DE]
	adc [HL]
	ld [HL], A
	ret


; Internal helper. Obeys same calling conventions as Math functions, except HL and DE
; should be vector addresses. Subtracts HL vector from DE vector, writing to HL in place
; (does NOT look at sign).
; To reiterate: THIS IS OUTPUT = INPUT - OUTPUT, NOT OUTPUT -= INPUT.
; Note that this can underflow if DE < HL. In this case carry will be set.
; Must be called with clear carry flag.
VecSub:
	ld L, C
	ld E, C
.loop
	ld A, [DE]
	sbc [HL]
	ld [HL-], A
	dec E
	jr nz, .loop
	; final loop
	ld A, [DE]
	sbc [HL]
	ld [HL], A
	ret


; Internal helper. Obeys same calling conventions as Math functions, except HL should
; be a vector address. Flips an underflowed vector HL back to the positive 2s-complement value.
VecNegate:
	; For each digit, we need to invert the bits.
	; In addition, we need to add 1 to the overall number.
	; This may mean carrying a carry bit all the way up!
	; We do this in a single pass.
	ld L, C
	scf ; set carry flag. it should generally be already set anyway, but nasty bug if it isn't.
	ld E, 0 ; minor speedup by caching 0 into E so we can adc E instead of adc 0
.loop
	ld A, [HL]
	cpl ; A = ~A. doesn't affect carry.
	adc E ; apply carry by adding 0. this will always be there for first byte, afterwards it's carried from prev loop
	ld [HL], A ; we could ld [HL-], A here, but then we don't have our loop condition check
	dec L
	jr nz, .loop
	; final loop
	ld A, [HL]
	cpl
	adc E
	ld [HL], A
	ret


; This is only used to check for overflow, and even that is really just an optimization
; by figuring out it's going to escape 1 iteration sooner.
; We can safely no-op it for now. TODO improve later.
MathAddNoOut:
	xor A ; clear carry
	ret


; To carry out multiplication A * B, we follow the basic integer algorithm:
;   result = 0
;   for i = 0 to number of bits
;     if ith bit of B is set
;       result += A << i
; This gives us a result in twice as many bits as A and B.
; However, in our case our true values are (integer value) * 2^(2 - 8 * precision)
;  so our result should be (integer multiplication result) * 2^(4 - 16 * precision)
;  but then truncated back into our normal range.
; Assuming we're ok with a small error, we can just discard any amount smaller
;  than our precision when adding to result.
; Practical concerns around bit shifting vs bytes mean our actual algo looks more like:
;   result = 0
;   for i in [0, p), bb in reverse(B)
;     for b in bb (LSB first)
;       if b
;         result += A with i precision
;       result >>= 1
;   result <<= 2 to restore to correct scale
; Except this makes us lose 2 bits of precision by shifting then shifting back
;  so when we do result >>= 1 we should carry lost bits into a temp var
;  and then when we <<= 2 we should restore the extra 2 bits from there
;  and/or special case to just avoid doing the final >> 1 entirely.
; But now we're dealing with 3 vectors instead of 2, but look closely: we're consuming
;  bits from B at the same rate we're shifting result downward.
; This means we can get away with using B as result, except for the small overlap
;  of the first two bits. So we treat result as being the concatenation (x, B) where x
;  is just one byte, so every bit that is originally part of B is shifted out by the time
;  we are finished, and at the end we do a shift >> 6 to bring result back into B proper
;  (actually this ends up being 7 because we don't shift after the final iteration).
; So now our algo looks like:
;  extra_byte = 0
;  let B' = (extra_byte, B)
;  for i in [0, p)
;    for j in [0, 8)
;      B', carry = B' >> 1
;      if carry
;        B' += A with i precision
;        on carry exit with overflow error
;  B' >>= 7
;  if extra_byte > 0
;    exit with overflow error
;  return B


; Handle sign, call VecMultiply
MathMultiply:
	; sign handling: if signs match, +. otherwise, -.

	; get sign addresses
	NumToSignAddr H, L
	NumToSignAddr D, E

	ld A, [DE]
	xor [HL] ; 0 if signs match, else 1
	ld [HL], A ; set sign to 0 (+) if signs match, or 1 (-) if they don't

	SignAddrToVecHigh H, L
	SignAddrToVecHigh D, E
	jr VecMultiply ; tail call


; Sign is always positive, copy input to output then call VecMultiply to do output *= input
; TODO improve later
MathSquare:
	; write positive sign
	NumToSignAddr H, L
	xor A
	ld [HL], A

	; get addresses
	SignAddrToVecHigh H, L
	NumToVecHigh D, E

	; copy DE to HL
	call VecCopy

	; tail call the actual multiply
	jr VecMultiply


; Internal helper. Obeys same calling conventions as Math functions, except HL and DE
; should be vector addresses. Multiplies HL by DE, updating HL in place.
; Returns with carry on overflow. In this case the value in HL is undefined.
; As always, C is number of bytes - 1 and B is preserved.
VecMultiply:
	push BC
	; We'll be using B as the leading byte of a 1-longer vector (B, [HL]).
	; We track current loop iteration in E, comparing it to C to know when we're done.
	ld B, E ; L and E are known to be 0 here, so this saves a byte over ld B, 0
.loop ; loop runs for each byte
REPT 8 ; for each bit in the byte
	call VecMulShiftRight ; (B, [HL]) >>= 1, set carry to last bit
	jr nc, .no_add\@
	; on carry, add DE to result
	call VecMulAdd ; (B, [HL]) += [D0], but only up to precision E
	; on carry, exit with carry. We still need to pop BC before we do.
	jr c, .ret
.no_add\@
ENDR
	inc E ; increase loop counter
	; check if E == C + 1, indicating we're done
	ld A, C
	inc A
	cp E ; set z if E == C + 1
	jr nz, .loop

	; Finally, shift (B, [HL]) down into HL, or return with carry if top bit of B is set
	; (ie. B would be non-0 after shift).
	; This is best done by shifting left and moving 1 byte down.
	; We do this in a loop:
	;   at start of loop, bottom 7 bits of A are the 7 bits moved down from prev byte
	;   B = [HL]
	;   rotate (A, B) left by 1, so A contains the prev 7 bits then the top bit of B
	;   move A -> [HL] and inc HL
	;   shift B right by 1 and put it in A so it's ready for next round

	; Special first case, to check for overflow in B. This is similar to subsequent loops
	; but B and A are switched.
	ld A, [HL]
	rla
	rl B ; B = new [HL], set carry if overflow
	jr c, .ret ; return with carry on overflow
	ld [HL], B
	; handle special case where precision is 1. note E = C + 1 from above.
	dec E
	jr z, .skip_shift_loop
	inc L ; need to do this manually for this loop because B and A are switched
.shift_loop
	rra ; rotate A back right, so that bottom 7 bits are same as old [HL]
	ld B, [HL]
	rl B ; rotate B left through carry, carry is now top bit of old [HL]
	rla ; rotate A left through carry, A is now bottom 7 bits of old A + top bit of old [HL]
	ld [HL+], A ; store new [HL] and inc
	ld A, B ; move B to A, ready for next loop
	dec E
	jr nz, .shift_loop
.skip_shift_loop
	; ensure carry is always clear before returning
	xor A

.ret
	pop BC
	ret


; Internal helper. Shifts the lengthened vec (B, [HL]) right 1 bit in place (adding a 0 to left),
; returning the final bit in carry.
; Clobbers A
VecMulShiftRight:
	; count iterations in A
	ld A, C
	inc A
	; hard-coded first iteration for B
	srl B ; shift B right (MSB = 0), put bottom bit in carry
.loop
	; shift right once, putting carry in LSB and putting MSB in carry
	rr [HL]
	inc L ; note doesn't affect carry
	dec A
	jr nz, .loop
	ld L, A ; A = 0 here so we reset L to input value
	; final carry is output
	ret


; Internal helper. Special-cased add for implementating multiplication.
; Adds vec beginning at [D0] to the lengthened vec (B, [HL]), but only
; up to [DE]. Returns whether addition overflowed in carry.
; Clobbers A
VecMulAdd:
	push DE
	ld A, E
	and A ; set z if E = 0. also clears carry.
	jr z, .skip_loop
	ld L, A
	dec L ; L = E - 1, so [HL] points 1 byte less into vec than [DE]
.loop
	ld A, [DE]
	adc [HL]
	ld [HL-], A
	dec E
	jr nz, .loop
	; restore HL back to original value, since it underflowed in last iteration
	inc HL
.skip_loop
	; special case final loop
	ld A, [DE]
	adc B
	ld B, A
	; restore original E
	pop DE
	; return final carry
	ret


; Set first byte and sign byte of a given vec, zeroing the rest.
; Intended for initialization at coarse values.
; H: Vector number to set
; D: Value for sign byte
; E: Value for most signifigant byte
; C: Precision
MathSet:
	NumToSignAddr H, L
	ld [HL], D
	SignAddrToVecHigh H, L
	ld L, C
	xor A
.loop
	ld [HL], A
	dec L
	jr nz, .loop

	; write MSB value
	ld [HL], E
	ret


; Increment the H vector by 4 >> A.
; Unlike most Math functions, preserves DE but not B.
MathAddPowerOfTwo:
	; TODO
	ret
