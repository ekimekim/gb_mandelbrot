IF !DEF(_G_HRAM)
_G_HRAM EQU "true"

RSSET $ff80

; Temporarily hold stack pointer during FlushVRAMBuffer.
; This is loaded to using ld [nn], SP. TODO what endianness?
FlushVRAMBufferSP rb 2

ENDC
