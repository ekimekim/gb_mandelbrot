
; Mapping is the process of building a pixel map of iteration counts.
; The IterationMap array contains one entry per pixel on the 160x144 screen,
; with each one-byte entry being the number of remaining iterations.

; The calculation process proceeds linearly through the map.
; If the user makes a change that causes the render to be aborted,
; we restart from the start.

; When we finish a block of 8 pixels, we call to the painter to color those pixels.
; We also track our current position in the array, for two reasons:
; a) so the vblank copy knows what needs updating
; b) so we can place a sprite on the current calculation point, to show user

; The mechanism of abort is interesting. Rather than bothering to build in a way
; to reset to the main routine, we just reset our stack from the interrupt handler
; then start the routine again.


SECTION "Mapper Data", WRAM0

NextToCalc::
	; (x, y) point on screen
	ds 2


; The iteration map is 22.5KiB. Since we need to be on GBC to get full pixel mapping anyway,
; we can use the WRAMX banks to store all the data.
; The iteration map is a row-major array of the remaining number of iterations when the value escaped.
; It is stored linearly starting in the first bank, filling each bank before moving to the next.
; In other words, for index (x, y):
;   Let N = 160 * x + y
;   The bank is 1 + N >> 12 and the address is IterationMapBase + (N & 0x0fff).
SECTION "Iteration Map Bank 1", WRAMX, BANK[1]
IterationMapBase:
	ds 4096
SECTION "Iteration Map Bank 2", WRAMX, BANK[2]
	ds 4096
SECTION "Iteration Map Bank 3", WRAMX, BANK[3]
	ds 4096
SECTION "Iteration Map Bank 4", WRAMX, BANK[4]
	ds 4096
SECTION "Iteration Map Bank 5", WRAMX, BANK[5]
	ds 4096
SECTION "Iteration Map Bank 6", WRAMX, BANK[6]
	ds (144 * 160) - (4096 * 5)

SECTION "Mapper code", ROM0

; Takes inputs:
;   BaseX, BaseY: Top-left corner of screen
;   Delta: TODO
;   B: Max iterations
;   C: Precision
CalculateMap::

/*
UPTO:
This plan won't work. The iteration map alone is 22.5KiB!
Best bet: Encode to 4-bit color as we go, palette update requires full recalc.

Options for delta (as in, X = BaseX + pixel x position * Delta):
* Have it as 8th vector, seems wasteful since it's generally very very close to 0
    In this plan you'd need a special "multiply by 8-bit int" method that does something
	like Temp = multiplier / 256, Delta *= 256, return Temp * Delta
* Require delta always be a value 2^-N, then just store 8-bit N
* Halfway solution: store delta as M * 2^(2-8N), ie. a single vector byte + number of bytes
    down to shift it. This remains flexible but efficient to multiply or convert to a real vector.
In any case ensure that BaseX + 160 * Delta doesn't overflow a vector
*/
