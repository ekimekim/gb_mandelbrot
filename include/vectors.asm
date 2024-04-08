
; Vectors are laid out in memory effectively as an array, and a "vector number" is an index
; into that array. Vector numbers are defined here.

RSRESET

; Base X/Y store the bottom-left corner coordinate of the screen
BaseX rb 1
BaseY rb 1

; CX/CY store the point under current consideration
CX rb 1
CY rb 1

; X, Y and YSq are temporary variables used during iteration
X rb 1
Y rb 1
YSq rb 1

NUM_VECTORS rb 0


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
