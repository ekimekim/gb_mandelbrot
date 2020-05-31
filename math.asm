

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
;  add(x, ysq)       break (because |z| > 2)
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
; Assumes L and E are already 0.
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
GetIterations:
	; special case first iteration, where z = 0 so iteration is z = 0^2 + c = c
	MathCall MathCopy X CX
	MathCall MathCopy Y CY
	dec B
	; main loop. note we break on any overflow (see above)
.loop
	; ysq = (prev y)^2
	MathCall MathSquare YSq Y
	ret c
	; y = prev y * prev x, we do this early so we can re-use X
	MathCall MathMultiply Y X
	ret c
	; x = (prev x)^2
	MathCall MathSquare X X
	ret c
	; Add prev x^2 and prev y^2, checking for overflow but discard result
	MathCall MathAddNoOut X YSq
	ret c
	; We're now past the part where we're checking if previous iteration escaped,
	; and into checking next iteration. So decrement the iteration count and return
	; if we've reached the limit (B=0)
	dec B
	ret z
	; y = 2 * prev y * prev x
	MathSingleCall MathDouble Y
	ret c
	; y = 2 * prev y * prev x + cy = next y
	MathCall MathAdd Y CY
	ret c
	; x = (prev x)^2 - (prev y)^2
	MathCall MathSub X YSq
	ret c
	; x = (prev x)^2 - (prev y)^2 + cx = next x
	MathCall MathAdd X CX
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
	ld A, HIGH(VectorBase)
	add \1
	ld \1, A
ENDM

; Convert sign byte address \1 \2 into vector address high byte, put into \1
SignAddrToVecHigh: MACRO
	ld A, HIGH(VectorBase)
	add \2
	ld \1, A
	xor A
	ld \2, A
ENDM

; Convert vector address high byte \1 into sign byte address \1 \2
VecHighToSignAddr: MACRO
	ld A, \1
	sub HIGH(VectorBase) ; left with just the vec number
	ld \2, A
	ld \1, HIGH(SignBytes)
ENDM


; Copy input to output
MathCopy:
	; get sign addresses
	NumToSignAddr H L
	NumToSignAddr D E
	; copy sign
	ld A, [DE]
	ld [HL], A
	; get vec addresses
	SignAddrToVecHigh H L
	SignAddrToVecHigh D E
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
	rla [HL]
	dec L ; note doesn't affect carry
	jr nz, .loop
	; final loop
	rla [HL]
	; final carry is output
	ret


; Add input to output in place
MathAdd:
	; 4 cases:
	;  +/+: Add vecs, output +, on carry output carry
	;  -/-: Add vecs, output -, on carry output carry
	;  +/-: vec = other vec - vec, on carry negate vec and output +, otherwise output -, output no carry
	;  -/+: vec = other vec - vec, on carry negate vec and output -, otherwise output +, output no carry

	; get sign addresses
	NumToSignAddr H L
	NumToSignAddr D E

	ld A, [DE]
	xor [HL] ; set z if both + or both -
	jr nz, .different_signs

	; This is the easy case. The sign of the output won't change, so that's done.
	; All that's left is to add the unsigned vectors, and pass the carry result back unchanged.
	; In fact, we can tail call.
	SignAddrToVecHigh H L
	SignAddrToVecHigh D E
	jr VecAdd
	; tail call

.different_signs

	; Keeping in mind we can't modify our input, only our output, first step
	; is to subtract the vectors so we have (output - input). This may underflow.
	SignAddrToVecHigh H L
	SignAddrToVecHigh D E
	call VecSub

	jr c, .underflow

	; No underflow, this means we need to flip the output sign (the other vec was dominant)
	VecHighToSignAddr H L
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
