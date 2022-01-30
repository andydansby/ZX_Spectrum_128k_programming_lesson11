; Attention move variables here

; --------------------------------------------
PUBLIC _UNCOMP_SONG
_UNCOMP_SONG:
extern _BUFFER_UNCOMP
extern _TABLA_SONG_CMP
extern _VT_SONG         ;attention new

    ld hl, _TABLA_SONG_CMP

	;which song to play
	;song number index starts at 0
	;ld a, 0
	;ld a, 1
	;ld a, 2
	ld a, (_VT_SONG)

	call EXT_WORD
	ld de, _BUFFER_UNCOMP
	call dzx0_standard
ret

; --------------------------------------------
;EXTRACT A WORD FROM A TABLE
;IN:(HL)=ADDRESS TABLE
;   (A)= POSITION
;OUT(HL)=WORD
; --------------------------------------------
EXT_WORD:
    LD D,0
	RLCA
	LD E,A
	ADD HL,DE
	LD E,(HL)
	INC HL
	LD D,(HL)
	EX DE,HL
	RET
; --------------------------------------------






;----------------------------------------------------
;ATTENTION to this  VARIABLE section
;vars from here can be stripped
;you can move VARS to any other address
;START+10 setup and status flags
;START+11 pointer to current position value in PT3 module;

;note @C003         working version
;SETUP: = 00
;CrPsPtr: = 00 00

;note @C005         working version
;SETUP: = 00
;CrPsPtr: = 6b c9 = c96b
;song is located at $C8A3
;$128 difference between the two


VARS:

PUBLIC _VT_LOOP     ;attention changed
_VT_LOOP:           ;attention changed
defb 1              ;attention changed
;set to 1 to play once
;set to 2 to play looped

PUBLIC _VT_SONG
_VT_SONG:
defb 0

;vars in code and other
;self-modified code moved here
;(for ROM and RAM separation)


;ATTENTION IMPORTANT
;this is our setup flag
;set bit0 to 1, if you want to
;play without looping
;bit7 is set each time, when
;loop point is passed

CrPsPtr:                            ;$C669
    defw 0      ;START+11
;ATTENTION IMPORTANT
;this is our song pointer


AddToEn:                            ;$C66B
    defb 0

AdInPtA:                            ;$C66C
    defw 0

AdInPtB:                            ;$C66E
    defw 0

AdInPtC:                            ;$C670
    defw 0

Env_Del:                            ;$C672
    defb 0

MODADDR:                            ;$C673
    defw 0

ESldAdd:                            ;$C675
    defw 0

Delay:                              ;$C677
    defb 0

PDSP_:                              ;$C678  ATTENTION?


CSP_:                               ;$C678  ATTENTION?

PSP_:                               ;$C678
    defw 0

SamPtrs:                            ;$C67A
    defw 0

OrnPtrs:                            ;$C67C
    defw 0

PatsPtr:                            ;$C67E
    defw 0

LPosPtr:                            ;$C680
    defw 0

L3:                                 ;$C682  ATTENTION?

M2:                                 ;$C682  ATTENTION?

PrSlide:                            ;$C682
    defw 0

PrNote:                             ;$C684
    defb 0

Version:                            ;$C685
    defb 0
;end of moved vars and self-modified code


;----------------------------------

VAR0START:                          ;$C686
 ;START of INITZERO area

;----------------------------------

DEFC CHP_PsInOr = 0
DEFC CHP_PsInSm = 1
DEFC CHP_CrAmSl = 2
DEFC CHP_CrNsSl = 3
DEFC CHP_CrEnSl = 4
DEFC CHP_TSlCnt = 5
DEFC CHP_CrTnSl = 6
DEFC CHP_TnAcc  = 8
DEFC CHP_COnOff = 10
;reset group

DEFC CHP_OnOffD = 11

;IX for PTDECOD here (+12)
DEFC CHP_OffOnD = 12
DEFC CHP_OrnPtr = 13
DEFC CHP_SamPtr = 15
DEFC CHP_NNtSkp = 17
DEFC CHP_Note   = 18
DEFC CHP_SlToNt = 19
DEFC CHP_Env_En = 20
DEFC CHP_Flags  = 21
 ;Enabled - 0,SimpleGliss - 2
DEFC CHP_TnSlDl = 22
DEFC CHP_TSlStp = 23
DEFC CHP_TnDelt = 25
DEFC CHP_NtSkCn = 27
DEFC CHP_Volume = 28
;	ENDS
DEFC CHP = 29
;	ENDS
;----------------------------------

ChanA:                              ;$C686
	defs CHP

ChanB:                              ;$C6A3
	defs CHP

ChanC:                              ;$C6C0
	defs CHP

;GlobalVars
DelyCnt:                            ;$C6DD
	defb 0
CurESld:                            ;$C6DE
	defw 0
CurEDel:                            ;$C6E0
	defb 0

Ns_Base_AddToNs:

Ns_Base:                            ;
	defb 0
AddToNs:                            ;
	defb 0

AYREGS:

VT_:                                ;
	defs 256 ;CreatedVolumeTableAddress

EnvBase	EQU VT_+14

T1_	EQU VT_+16 ;Tone tables data depacked here

T_OLD_1	EQU T1_
T_OLD_2	EQU T_OLD_1+24
T_OLD_3	EQU T_OLD_2+24
T_OLD_0	EQU T_OLD_3+2
T_NEW_0	EQU T_OLD_0
T_NEW_1	EQU T_OLD_1
T_NEW_2	EQU T_NEW_0+24
T_NEW_3	EQU T_OLD_3

NT_:
	defs 192 ;CreatedNoteTableAddress

;local var
Ampl    EQU AYREGS+AmplC

VAR0END EQU VT_+16 ;INIT zeroes from VARS to VAR0END-1

VARSEND EQU $

MDLADDR EQU $
;MDLADDR: defw 0



