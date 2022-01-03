;--------------------------------------------------------
; File Created by SDCC : free open source ANSI-C Compiler
; Version 4.0.0 #11528 (MINGW32)
;--------------------------------------------------------
	.module contended
	.optsdcc -mz80
	
;--------------------------------------------------------
; Public variables in this module
;--------------------------------------------------------
	.globl _add_two_numbers
;--------------------------------------------------------
; special function registers
;--------------------------------------------------------
;--------------------------------------------------------
; ram data
;--------------------------------------------------------
	.area _DATA
;--------------------------------------------------------
; ram data
;--------------------------------------------------------
	.area _INITIALIZED
;--------------------------------------------------------
; absolute external ram data
;--------------------------------------------------------
	.area _DABS (ABS)
;--------------------------------------------------------
; global & static initialisations
;--------------------------------------------------------
	.area _HOME
	.area _GSINIT
	.area _GSFINAL
	.area _GSINIT
;--------------------------------------------------------
; Home
;--------------------------------------------------------
	.area _HOME
	.area _HOME
;--------------------------------------------------------
; code
;--------------------------------------------------------
	.area _CODE
;..\CONTENDED\contended.c:16: static void naked_contendedRam() __naked
;	---------------------------------
; Function naked_contendedRam
; ---------------------------------
_naked_contendedRam:
;..\CONTENDED\contended.c:20: __endasm;
	SECTION	CONTENDED
;..\CONTENDED\contended.c:21: }
;..\CONTENDED\/routines.h:5: unsigned char add_two_numbers (unsigned char a, unsigned char b)
;	---------------------------------
; Function add_two_numbers
; ---------------------------------
_add_two_numbers::
;..\CONTENDED\/routines.h:7: return a + b;
	ld	hl, #3
	add	hl, sp
	ld	iy, #2
	add	iy, sp
	ld	a, 0 (iy)
	add	a, (hl)
	ld	l, a
;..\CONTENDED\/routines.h:8: }
	ret
	.area _CODE
	.area _INITIALIZER
	.area _CABS (ABS)
