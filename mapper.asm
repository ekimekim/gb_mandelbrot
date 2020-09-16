
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
