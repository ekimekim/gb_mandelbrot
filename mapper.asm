
; Mapping is the process of building a pixel map of iteration counts.
; The IterationMap array contains one entry per pixel on the 160x144 screen,
; with each one-byte entry being the number of remaining iterations.

; The calculation process proceeds linearly through the map.
; If the user makes a change that causes the render to be aborted,
; we restart from the start but leave old values intact.
; Our behaviour when we reach these old values depends on the nature of the change:
; 1. Change in precision: Replace all values
; 2. Increase in max iterations by N:
;      Re-calculate zeroes (previously ran out of iterations, might not now)
;      Add N to non-zeroes (if it previously had M iterations remaining, it now has M+N)
; 3. Decrease in max iterations by N:
;      Subtract N from all values (min 0)
; 4. Zoom in / out
;      For now, replace all values.
;      Future work: remap existing values then fill in the gaps

; The mechanism of abort is interesting. Rather than bothering to build in a way
; to reset to the main routine, we just reset our stack from the interrupt handler
; then start the routine again.
