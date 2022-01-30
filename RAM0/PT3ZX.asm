;Vortex Tracker II v1.0 PT3 player for ZX Spectrum
;ROM version (specially for Axor)
;(c)2004,2007 S.V.Bulba <vorobey@mail.khstu.ru>
;http://bulba.untergrund.net (http://bulba.at.kz)

;Release number
;Release EQU "MOR7"

;Features
;--------
;-Can run in ROM (self-modified code is not used).
;-Can be compiled at any address (i.e. no need rounding ORG
; address).
;-Variables (VARS) can be located at any address (not only after
;code block).
;-INIT subroutine detects module version and rightly generates
; both note and volume tables outside of code block (in VARS).
;-Two portamento (spc. command 3xxx) algorithms (depending of
; module version).
;-New 1.XX and 2.XX special command behaviour (only for PT v3.7
; and higher).
;-Any Tempo value are accepted (including Tempo=1 and Tempo=2).
;-Fully compatible with Ay_Emul PT3 player codes.
;-See also notes at the end of this source code.

;Warning!!! PLAY subroutine can crash if no module are loaded
;into RAM or INIT subroutine was not called before.

;Call MUTE or INIT one more time to mute sound
;after stopping playing

DEFC TonA  = 0
DEFC TonB  = 2
DEFC TonC  = 4
DEFC Noise = 6
DEFC Mixer = 7
DEFC AmplA = 8
DEFC AmplB = 9
DEFC AmplC = 10
DEFC Env   = 11
DEFC EnvTp = 13

;Entry and other points
;START initialization
;START+3 initialization with module address in HL
;START+5 play one quark
;START+8 mute
;START+10 setup and status flags
;START+11 pointer to current position value in PT3 module;
;After INIT (START+11) points to Postion0-1 (optimization)

;extern _song1
extern _VT_LOOP            ;extern SETUP attention changed

PUBLIC _VT_START    ;$C000
_VT_START:
	LD HL, MDLADDR  ;$C000  LD HL,MDLADDR   _song1
	JR _VT_INIT     ;$C003
	JP _VT_PLAY     ;$C005
	JR _VT_MUTE     ;$C008
; --------------------------------------------

;Identifier	;attention
	DEFM "=VTII PT3 Player MOR7="

CHECKLP:            ;$C020
	LD HL, _VT_LOOP        ;LD HL, SETUP attention changed
	SET 7,(HL)
	BIT 0,(HL)
	RET Z
	POP HL
	LD HL,DelyCnt
	INC (HL)
	LD HL,ChanA+CHP_NtSkCn
	INC (HL)

PUBLIC _VT_MUTE
_VT_MUTE:               ;$C031
	XOR A
	LD H,A
	LD L,A
	LD (AYREGS+AmplA),A
	LD (AYREGS+AmplB),HL
	JP ROUT_A0

PUBLIC _VT_INIT
_VT_INIT:               ;$C03D
;HL - AddressOfModule
	LD (MODADDR),HL
	PUSH HL
	LD DE,100
	ADD HL,DE
	LD A,(HL)
	LD (Delay),A
	PUSH HL
	POP IX
	ADD HL,DE
	LD (CrPsPtr),HL
	LD E,(IX+102-100)
	ADD HL,DE
	INC HL
	LD (LPosPtr),HL
	POP DE
	LD L,(IX+103-100)
	LD H,(IX+104-100)
	ADD HL,DE
	LD (PatsPtr),HL
	LD HL,169
	ADD HL,DE
	LD (OrnPtrs),HL
	LD HL,105
	ADD HL,DE
	LD (SamPtrs),HL
	LD HL, _VT_LOOP            ;LD HL, SETUP attention changed
	RES 7,(HL)

;note table data depacker
	LD DE,T_PACK
	LD BC,T1_+(2*49)-1
TP_0:               ;$C07C
	LD A,(DE)
	INC DE
	CP 15*2
	JR NC,TP_1
	LD H,A
	LD A,(DE)
	LD L,A
	INC DE
	JR TP_2
TP_1:               ;$C088
	PUSH DE
	LD D,0
	LD E,A
	ADD HL,DE
	ADD HL,DE
	POP DE
TP_2:               ;$C08F
	LD A,H
	LD (BC),A
	DEC BC
	LD A,L
	LD (BC),A
	DEC BC

	;SUB $F8*2 ;attention error	$C097
	SUB $F0 	;in compiled SUB $F0

	JR NZ,TP_0

	LD HL,VAR0START
	LD (HL),A
	LD DE,VAR0START+1
	LD BC,VAR0END-VAR0START-1
	LDIR
	INC A
	LD (DelyCnt),A
	LD HL,$F001 ;H - CHP.Volume, L - CHP.NtSkCn
	LD (ChanA+CHP_NtSkCn),HL
	LD (ChanB+CHP_NtSkCn),HL
	LD (ChanC+CHP_NtSkCn),HL

	LD HL,EMPTYSAMORN
	LD (AdInPtA),HL ;ptr to zero
	LD (ChanA+CHP_OrnPtr),HL ;ornament 0 is "0,1,0"
	LD (ChanB+CHP_OrnPtr),HL ;in all versions from
	LD (ChanC+CHP_OrnPtr),HL ;3.xx to 3.6x and VTII

	LD (ChanA+CHP_SamPtr),HL ;S1 There is no default
	LD (ChanB+CHP_SamPtr),HL ;S2 sample in PT3, so, you
	LD (ChanC+CHP_SamPtr),HL ;S3 can comment S1,2,3; see
				    ;also EMPTYSAMORN comment

	LD A,(IX+13-100) ;EXTRACT VERSION NUMBER
	SUB $30
	JR C,L20
	CP 10
	JR C,L21
L20:                ;$C0D8
	LD A,6
L21:                ;$C0DA
	LD (Version),A
	PUSH AF
	CP 4

	LD A,(IX+99-100) ;TONE TABLE NUMBER
	RLA
	AND 7

;NoteTableCreator (c) Ivan Roshin
;A - NoteTableNumber*2+VersionForNoteTable
;(xx1b - 3.xx..3.4r, xx0b - 3.4x..3.6x..VTII1.0)

	LD HL,NT_DATA
	PUSH DE
	LD D,B
	ADD A,A
	LD E,A
	ADD HL,DE
	LD E,(HL)
	INC HL
	SRL E
	SBC A,A
	AND $A7 ;$00 (NOP) or $A7 (AND A)
	LD (L3),A
	LD A,201 ;RET temporary
	LD (L3+1),A ;temporary
	EX DE,HL
	POP BC ;BC=T1_
	ADD HL,BC

	LD A,(DE)

	;ADD A,T_		;original
	ADD A, $EB		;compiled attention
	;seems ok

	LD C,A
	ADC A,T_/256

	SUB C
	LD B,A
	PUSH BC
	LD DE,NT_
	PUSH DE

	LD B,12
L1:                 ;$C10F
	PUSH BC
	LD C,(HL)
	INC HL
	PUSH HL
	LD B,(HL)

	PUSH DE
	EX DE,HL
	LD DE,23
	LD IXH,8

L2:                 ;$C11C
	SRL B
	RR C
	CALL L3 ;temporary
;L3	DB $19	;AND A or NOP
	LD A,C
	ADC A,D	;=ADC 0
	LD (HL),A
	INC HL
	LD A,B
	ADC A,D
	LD (HL),A
	ADD HL,DE
	DEC IXH
	JR NZ,L2

	POP DE
	INC DE
	INC DE
	POP HL
	INC HL
	POP BC
	DJNZ L1

	POP HL
	POP DE

	LD A,E

	;CP TCOLD_1		;original
	CP $F7			;compiled	attention
	;seems ok

	JR NZ,CORR_1
	LD A,$FD
	LD (NT_+$2E),A

CORR_1:             ;$C143
	LD A,(DE)
	AND A
	JR Z,TC_EXIT
	RRA
	PUSH AF
	ADD A,A
	LD C,A
	ADD HL,BC
	POP AF
	JR NC,CORR_2
	DEC (HL)
	DEC (HL)
CORR_2:             ;$C151
	INC (HL)
	AND A
	SBC HL,BC
	INC DE
	JR CORR_1

TC_EXIT:            ;$C158

	POP AF

;VolTableCreator (c) Ivan Roshin
;A - VersionForVolumeTable (0..4 - 3.xx..3.4x;
			   ;5.. - 3.5x..3.6x..VTII1.0)

	CP 5
	LD HL,$11
	LD D,H
	LD E,H
	LD A,$17
	JR NC,M1
	DEC L
	LD E,L
	XOR A
M1:                 ;$C167
	LD (M2),A

	LD IX,VT_+16
	LD C,$10

INITV2:             ;$C170
	PUSH HL

	ADD HL,DE
	EX DE,HL
	SBC HL,HL

INITV1:             ;$C175
	LD A,L
;M2      DB $7D
	CALL M2 ;temporary
	LD A,H
	ADC A,0
	LD (IX),A
	INC IX
	ADD HL,DE
	INC C
	LD A,C
	AND 15
	JR NZ,INITV1

	POP HL
	LD A,E
	CP $77
	JR NZ,M3
	INC E
M3:                 ;$C18F
	LD A,C
	AND A
	JR NZ,INITV2

	JP ROUT_A0

;pattern decoder
PD_OrSm:            ;$C196
	LD (IX-12+CHP_Env_En),0
	CALL SETORN
	LD A,(BC)
	INC BC
	RRCA

PD_SAM:             ;$C1A0
	ADD A,A

PD_SAM_:            ;$C1A1
	LD E,A
	LD D,0
;SamPtrs EQU $+1
;	LD HL,$2121
	LD HL,(SamPtrs)
	ADD HL,DE
	LD E,(HL)
	INC HL
	LD D,(HL)
;MODADDR EQU $+1
;	LD HL,$2121
	LD HL,(MODADDR)
	ADD HL,DE
	LD (IX-12+CHP_SamPtr),L
	LD (IX-12+CHP_SamPtr+1),H
	JR PD_LOOP

PD_VOL:             ;$C1B7
	RLCA
	RLCA
	RLCA
	RLCA
	LD (IX-12+CHP_Volume),A
	JR PD_LP2

PD_EOff:            ;$C1C0
	LD (IX-12+CHP_Env_En),A
	LD (IX-12+CHP_PsInOr),A
	JR PD_LP2

PD_SorE:            ;$C1C8
	DEC A
	JR NZ,PD_ENV
	LD A,(BC)
	INC BC
	LD (IX-12+CHP_NNtSkp),A
	JR PD_LP2

PD_ENV:             ;$C1D2
	CALL SETENV
	JR PD_LP2

PD_ORN:             ;$C1D7
	CALL SETORN
	JR PD_LOOP

PD_ESAM:            ;$C1DC
	LD (IX-12+CHP_Env_En),A
	LD (IX-12+CHP_PsInOr),A
	CALL NZ,SETENV
	LD A,(BC)
	INC BC
	JR PD_SAM_

PTDECOD:            ;$C1E9
	LD A,(IX-12+CHP_Note)
;	LD (PrNote+1),A
	LD (PrNote),A
	LD L,(IX-12+CHP_CrTnSl)
	LD H,(IX-12+CHP_CrTnSl+1)
;	LD (PrSlide+1),HL
	LD (PrSlide),HL

PD_LOOP:            ;$C1F8
	LD DE,$2010
PD_LP2:             ;$C1FB
	LD A,(BC)
	INC BC
	ADD A,E
	JR C,PD_OrSm
	ADD A,D
	JR Z,PD_FIN
	JR C,PD_SAM
	ADD A,E
	JR Z,PD_REL
	JR C,PD_VOL
	ADD A,E
	JR Z,PD_EOff
	JR C,PD_SorE
	ADD A,96
	JR C,PD_NOTE
	ADD A,E
	JR C,PD_ORN
	ADD A,D
	JR C,PD_NOIS
	ADD A,E
	JR C,PD_ESAM
	ADD A,A
	LD E,A

;	LD HL,SPCCOMS+$FF20-$2000 ;attention error
	PUSH DE
	LD   DE,0xDF20
	LD   HL,SPCCOMS
	ADD  HL,DE
	POP  DE

	ADD HL,DE
	LD E,(HL)
	INC HL
	LD D,(HL)
	PUSH DE
	JR PD_LOOP

PD_NOIS:            ;$C22E
	LD (Ns_Base),A
	JR PD_LP2

PD_REL:             ;$C233
	RES 0,(IX-12+CHP_Flags)
	JR PD_RES

PD_NOTE:            ;$C239
	LD (IX-12+CHP_Note),A
	SET 0,(IX-12+CHP_Flags)
	XOR A

PD_RES:             ;$C241
	;LD (PDSP_+1),SP
	LD (PDSP_),SP
	LD SP,IX
	LD H,A
	LD L,A
	PUSH HL
	PUSH HL
	PUSH HL
	PUSH HL
	PUSH HL
	PUSH HL
;PDSP_	LD SP,$3131
	LD SP,(PDSP_)

PD_FIN:             ;$C253
	LD A,(IX-12+CHP_NNtSkp)
	LD (IX-12+CHP_NtSkCn),A
RET

C_PORTM:            ;$C25A
	RES 2,(IX-12+CHP_Flags)
	LD A,(BC)
	INC BC
;SKIP PRECALCULATED TONE DELTA (BECAUSE
;CANNOT BE RIGHT AFTER PT3 COMPILATION)
	INC BC
	INC BC
	LD (IX-12+CHP_TnSlDl),A
	LD (IX-12+CHP_TSlCnt),A
	LD DE,NT_
	LD A,(IX-12+CHP_Note)
	LD (IX-12+CHP_SlToNt),A
	ADD A,A
	LD L,A
	LD H,0
	ADD HL,DE
	LD A,(HL)
	INC HL
	LD H,(HL)
	LD L,A
	PUSH HL
;PrNote	LD A,$3E
	LD A,(PrNote)
	LD (IX-12+CHP_Note),A
	ADD A,A
	LD L,A
	LD H,0
	ADD HL,DE
	LD E,(HL)
	INC HL
	LD D,(HL)
	POP HL
	SBC HL,DE
	LD (IX-12+CHP_TnDelt),L
	LD (IX-12+CHP_TnDelt+1),H
	LD E,(IX-12+CHP_CrTnSl)
	LD D,(IX-12+CHP_CrTnSl+1)
;Version EQU $+1
;	LD A,$3E
	LD A,(Version)
	CP 6
	JR C,OLDPRTM ;Old 3xxx for PT v3.5-
;PrSlide	LD DE,$1111
	LD DE,(PrSlide)
	LD (IX-12+CHP_CrTnSl),E
	LD (IX-12+CHP_CrTnSl+1),D

OLDPRTM:            ;$C2A9
	LD A,(BC) ;SIGNED TONE STEP
	INC BC
	EX AF,AF'
	LD A,(BC)
	INC BC
	AND A
	JR Z,NOSIG
	EX DE,HL

NOSIG:              ;$C2B2
	SBC HL,DE
	JP P,SET_STP
	CPL
	EX AF,AF'
	NEG
	EX AF,AF'

SET_STP:            ;$C2BC
	LD (IX-12+CHP_TSlStp+1),A
	EX AF,AF'
	LD (IX-12+CHP_TSlStp),A
	LD (IX-12+CHP_COnOff),0
RET

C_GLISS:            ;$C2C8
	SET 2,(IX-12+CHP_Flags)
	LD A,(BC)
	INC BC
	LD (IX-12+CHP_TnSlDl),A
	AND A
	JR NZ,GL36
	LD A,(Version) ;AlCo PT3.7+
	CP 7
	SBC A,A
	INC A

GL36:               ;$C2DB
	LD (IX-12+CHP_TSlCnt),A
	LD A,(BC)
	INC BC
	EX AF,AF'
	LD A,(BC)
	INC BC
	JR SET_STP

C_SMPOS:            ;$C2E5
	LD A,(BC)
	INC BC
	LD (IX-12+CHP_PsInSm),A
RET

C_ORPOS:            ;$C2EB
	LD A,(BC)
	INC BC
	LD (IX-12+CHP_PsInOr),A
RET

C_VIBRT:            ;$C2F1
	LD A,(BC)
	INC BC
	LD (IX-12+CHP_OnOffD),A
	LD (IX-12+CHP_COnOff),A
	LD A,(BC)
	INC BC
	LD (IX-12+CHP_OffOnD),A
	XOR A
	LD (IX-12+CHP_TSlCnt),A
	LD (IX-12+CHP_CrTnSl),A
	LD (IX-12+CHP_CrTnSl+1),A
RET

C_ENGLS:            ;$C309
	LD A,(BC)
	INC BC
	LD (Env_Del),A
	LD (CurEDel),A
	LD A,(BC)
	INC BC
	LD L,A
	LD A,(BC)
	INC BC
	LD H,A
	LD (ESldAdd),HL
RET

C_DELAY:            ;$C31B
	LD A,(BC)
	INC BC
	LD (Delay),A
RET

SETENV:             ;$C321
	LD (IX-12+CHP_Env_En),E
	LD (AYREGS+EnvTp),A
	LD A,(BC)
	INC BC
	LD H,A
	LD A,(BC)
	INC BC
	LD L,A
	LD (EnvBase),HL
	XOR A
	LD (IX-12+CHP_PsInOr),A
	LD (CurEDel),A
	LD H,A
	LD L,A
	LD (CurESld),HL

C_NOP:              ;$C33C
RET

SETORN:             ;$C33D
	ADD A,A
	LD E,A
	LD D,0
	LD (IX-12+CHP_PsInOr),D
;OrnPtrs	EQU $+1
;	LD HL,$2121
	LD HL,(OrnPtrs)
	ADD HL,DE
	LD E,(HL)
	INC HL
	LD D,(HL)
;MDADDR2	EQU $+1
;	LD HL,$2121
	LD HL,(MODADDR)
	ADD HL,DE
	LD (IX-12+CHP_OrnPtr),L
	LD (IX-12+CHP_OrnPtr+1),H
RET

;ALL 16 ADDRESSES TO PROTECT FROM BROKEN PT3 MODULES
SPCCOMS:            ;$C356
	defw C_NOP
	defw C_GLISS
	defw C_PORTM
	defw C_SMPOS
	defw C_ORPOS
	defw C_VIBRT
	defw C_NOP
	defw C_NOP
	defw C_ENGLS
	defw C_DELAY
	defw C_NOP
	defw C_NOP
	defw C_NOP
	defw C_NOP
	defw C_NOP
	defw C_NOP

CHREGS:             ;#C376
	XOR A
	LD (Ampl),A
	BIT 0,(IX+CHP_Flags)
	PUSH HL
	JP Z,CH_EXIT
;	LD (CSP_+1),SP
	LD (CSP_),SP
	LD L,(IX+CHP_OrnPtr)
	LD H,(IX+CHP_OrnPtr+1)
	LD SP,HL
	POP DE
	LD H,A
	LD A,(IX+CHP_PsInOr)
	LD L,A
	ADD HL,SP
	INC A
	CP D
	JR C,CH_ORPS
	LD A,E

CH_ORPS:            ;#C399
	LD (IX+CHP_PsInOr),A
	LD A,(IX+CHP_Note)
	ADD A,(HL)
	JP P,CH_NTP
	XOR A

CH_NTP:             ;#C3A4
	CP 96
	JR C,CH_NOK
	LD A,95

CH_NOK:             ;#C3AA
	ADD A,A
	EX AF,AF'
	LD L,(IX+CHP_SamPtr)
	LD H,(IX+CHP_SamPtr+1)
	LD SP,HL
	POP DE
	LD H,0
	LD A,(IX+CHP_PsInSm)
	LD B,A
	ADD A,A
	ADD A,A
	LD L,A
	ADD HL,SP
	LD SP,HL
	LD A,B
	INC A
	CP D
	JR C,CH_SMPS
	LD A,E
CH_SMPS:            ;#C3C5
	LD (IX+CHP_PsInSm),A
	POP BC
	POP HL
	LD E,(IX+ CHP_TnAcc)
	LD D,(IX+ CHP_TnAcc+1)
	ADD HL,DE
	BIT 6,B
	JR Z,CH_NOAC
	LD (IX+ CHP_TnAcc),L
	LD (IX+ CHP_TnAcc+1),H

CH_NOAC:            ;#C3DB
	EX DE,HL
	EX AF,AF'
	LD L,A
	LD H,0
	LD SP,NT_
	ADD HL,SP
	LD SP,HL
	POP HL
	ADD HL,DE
	LD E,(IX+CHP_CrTnSl)
	LD D,(IX+CHP_CrTnSl+1)
	ADD HL,DE
;CSP_	LD SP,$3131
	LD SP,(CSP_)
	EX (SP),HL
	XOR A
	OR (IX+CHP_TSlCnt)
	JR Z,CH_AMP
	DEC (IX+CHP_TSlCnt)
	JR NZ,CH_AMP
	LD A,(IX+CHP_TnSlDl)
	LD (IX+CHP_TSlCnt), A
	LD L,(IX+CHP_TSlStp)
	LD H,(IX+CHP_TSlStp+1)
	LD A,H
	ADD HL,DE
	LD (IX+CHP_CrTnSl),L
	LD (IX+CHP_CrTnSl+1),H
	BIT 2,(IX+CHP_Flags)
	JR NZ,CH_AMP
	LD E,(IX+CHP_TnDelt)
	LD D,(IX+CHP_TnDelt+1)
	AND A
	JR Z,CH_STPP
	EX DE,HL

CH_STPP:            ;#C422
	SBC HL,DE
	JP M,CH_AMP
	LD A,(IX+CHP_SlToNt)
	LD (IX+CHP_Note),A
	XOR A
	LD (IX+CHP_TSlCnt),A
	LD (IX+CHP_CrTnSl),A
	LD (IX+CHP_CrTnSl+1),A

CH_AMP:             ;#C437
	LD A,(IX+CHP_CrAmSl)
	BIT 7,C
	JR Z,CH_NOAM
	BIT 6,C
	JR Z,CH_AMIN
	CP 15
	JR Z,CH_NOAM
	INC A
	JR CH_SVAM

CH_AMIN:            ;#C449
	CP -15
	JR Z,CH_NOAM
	DEC A

CH_SVAM:            ;#C44E
	LD (IX+CHP_CrAmSl),A

CH_NOAM:            ;#C451
	LD L,A
	LD A,B
	AND 15
	ADD A,L
	JP P,CH_APOS
	XOR A

CH_APOS:            ;#C45A
	CP 16
	JR C,CH_VOL
	LD A,15

CH_VOL:             ;#C460
	OR (IX+CHP_Volume)
	LD L,A
	LD H,0
	LD DE,VT_
	ADD HL,DE
	LD A,(HL)

CH_ENV:             ;#C46B
	BIT 0,C
	JR NZ,CH_NOEN
	OR (IX+CHP_Env_En)

CH_NOEN:            ;#C472
	LD (Ampl),A
	BIT 7,B
	LD A,C
	JR Z,NO_ENSL
	RLA
	RLA
	SRA A
	SRA A
	SRA A
	ADD A,(IX+CHP_CrEnSl) ;SEE COMMENT BELOW
	BIT 5,B
	JR Z,NO_ENAC
	LD (IX+CHP_CrEnSl),A

NO_ENAC:            ;#C48C
	LD HL,AddToEn
	ADD A,(HL) ;BUG IN PT3 - NEED WORD HERE.
		   ;FIX IT IN NEXT VERSION?
	LD (HL),A
	JR CH_MIX

NO_ENSL:            ;#C493
    RRA
	ADD A,(IX+CHP_CrNsSl)
	LD (AddToNs),A
	BIT 5,B
	JR Z,CH_MIX
	LD (IX+CHP_CrNsSl),A

CH_MIX:             ;#C4A1
	LD A,B
	RRA
	AND $48

CH_EXIT:            ;#C4A5
	LD HL,AYREGS+Mixer
	OR (HL)
	RRCA
	LD (HL),A
	POP HL
	XOR A
	OR (IX+CHP_COnOff)
	RET Z
	DEC (IX+CHP_COnOff)
	RET NZ
	XOR (IX+CHP_Flags)
	LD (IX+CHP_Flags),A
	RRA
	LD A,(IX+CHP_OnOffD)
	JR C,CH_ONDL
	LD A,(IX+CHP_OffOnD)

CH_ONDL:            ;#C4C4
	LD (IX+CHP_COnOff),A
RET

PUBLIC _VT_PLAY
_VT_PLAY:           ;#C4C8
    XOR A
	LD (AddToEn),A
	LD (AYREGS+Mixer),A
	DEC A
	LD (AYREGS+EnvTp),A
	LD HL,DelyCnt
	DEC (HL)
	JP NZ,PL2
	LD HL,ChanA+CHP_NtSkCn
	DEC (HL)
	JR NZ,PL1B
;AdInPtA	EQU $+1
;	LD BC,$0101
	LD BC,(AdInPtA)
	LD A,(BC)
	AND A
	JR NZ,PL1A
	LD D,A
	LD (Ns_Base),A
	LD HL,(CrPsPtr)
	INC HL
	LD A,(HL)
	INC A
	JR NZ,PLNLP
	CALL CHECKLP
;LPosPtr	EQU $+1
;	LD HL,$2121
	LD HL,(LPosPtr)
	LD A,(HL)
	INC A
PLNLP:              ;#C4FC
	LD (CrPsPtr),HL
	DEC A
	ADD A,A
	LD E,A
	RL D
;PatsPtr	EQU $+1
;	LD HL,$2121
	LD HL,(PatsPtr)
	ADD HL,DE
	LD DE,(MODADDR)
;	LD (PSP_+1),SP
	LD (PSP_),SP
	LD SP,HL
	POP HL
	ADD HL,DE
	LD B,H
	LD C,L
	POP HL
	ADD HL,DE
	LD (AdInPtB),HL
	POP HL
	ADD HL,DE
	LD (AdInPtC),HL
;PSP_	LD SP,$3131
	LD SP,(PSP_)
PL1A:               ;#C523
	LD IX,ChanA+12
	CALL PTDECOD
	LD (AdInPtA),BC

PL1B:               ;#C52E
	LD HL,ChanB+CHP_NtSkCn
	DEC (HL)
	JR NZ,PL1C
	LD IX,ChanB+12
;AdInPtB	EQU $+1
;	LD BC,$0101
	LD BC,(AdInPtB)
	CALL PTDECOD
	LD (AdInPtB),BC

PL1C:               ;#C543
	LD HL,ChanC+CHP_NtSkCn
	DEC (HL)
	JR NZ,PL1D
	LD IX,ChanC+12
;AdInPtC	EQU $+1
;	LD BC,$0101
	LD BC,(AdInPtC)
	CALL PTDECOD
	LD (AdInPtC),BC

;Delay	EQU $+1
PL1D:               ;#C558
	;LD A,$3E
	LD A,(Delay)
	LD (DelyCnt),A

PL2:                ;#C55E
	LD IX,ChanA
	LD HL,(AYREGS+TonA)
	CALL CHREGS
	LD (AYREGS+TonA),HL
	LD A,(Ampl)
	LD (AYREGS+AmplA),A
	LD IX,ChanB
	LD HL,(AYREGS+TonB)
	CALL CHREGS
	LD (AYREGS+TonB),HL
	LD A,(Ampl)
	LD (AYREGS+AmplB),A
	LD IX,ChanC
	LD HL,(AYREGS+TonC)
	CALL CHREGS
;	LD A,(Ampl) ;Ampl = AYREGS+AmplC
;	LD (AYREGS+AmplC),A
	LD (AYREGS+TonC),HL

	LD HL,(Ns_Base_AddToNs)
	LD A,H
	ADD A,L
	LD (AYREGS+Noise),A

;AddToEn EQU $+1
;	LD A,$3E
	LD A,(AddToEn)
	LD E,A
	ADD A,A
	SBC A,A
	LD D,A
	LD HL,(EnvBase)
	ADD HL,DE
	LD DE,(CurESld)
	ADD HL,DE
	LD (AYREGS+Env),HL

	XOR A
	LD HL,CurEDel
	OR (HL)
	JR Z,ROUT_A0
	DEC (HL)
	JR NZ,ROUT
;Env_Del	EQU $+1
;	LD A,$3E
	LD A,(Env_Del)
	LD (HL),A
;ESldAdd	EQU $+1
;	LD HL,$2121
	LD HL,(ESldAdd)
	ADD HL,DE
	LD (CurESld),HL

ROUT:               ;#C5C1
	XOR A
ROUT_A0:            ;#C5C2
	LD DE, $FFBF
	LD BC, $FFFD
	LD HL, AYREGS
LOUT:               ;#C5B8
	OUT (C),A
	LD B,E
	OUTI
	LD B,D
	INC A
	CP 13
	JR NZ,LOUT
	OUT (C),A
	LD A,(HL)
	AND A
	RET M
	LD B,E
	OUT (C),A
RET             ;#C5DE


NT_DATA:            ;#C5DF
	defb (T_NEW_0-T1_)*2
	defb TCNEW_0-T_
	defb (T_OLD_0-T1_)*2+1
	defb TCOLD_0-T_
	defb (T_NEW_1-T1_)*2+1
	defb TCNEW_1-T_
	defb (T_OLD_1-T1_)*2+1
	defb TCOLD_1-T_
	defb (T_NEW_2-T1_)*2
	defb TCNEW_2-T_
	defb (T_OLD_2-T1_)*2
	defb TCOLD_2-T_
	defb (T_NEW_3-T1_)*2
	defb TCNEW_3-T_
	defb (T_OLD_3-T1_)*2
	defb TCOLD_3-T_

T_:

TCOLD_0:            ;#C5EF
	defb $00+1,$04+1,$08+1,$0A+1,$0C+1,$0E+1,$12+1,$14+1
	defb $18+1,$24+1,$3C+1,0

TCOLD_1:            ;#C5FB
	defb $5C+1,0

TCOLD_2:            ;#C5FD
	defb $30+1,$36+1,$4C+1,$52+1,$5E+1,$70+1,$82,$8C,$9C
	defb $9E,$A0,$A6,$A8,$AA,$AC,$AE,$AE,0

TCNEW_3:            ;#C60F
	defb $56+1

TCOLD_3:            ;#C610
	defb $1E+1,$22+1,$24+1,$28+1,$2C+1,$2E+1,$32+1,$BE+1,0

TCNEW_0:            ;#C619
	defb $1C+1,$20+1,$22+1,$26+1,$2A+1,$2C+1,$30+1,$54+1
	defb $BC+1,$BE+1,0

TCNEW_1	= TCOLD_1   ;#

TCNEW_2:            ;#C624
	defb $1A+1,$20+1,$24+1,$28+1,$2A+1,$3A+1,$4C+1,$5E+1
	defb $BA+1,$BC+1,$BE+1,0

;EMPTYSAMORN:
;	EQU $-1
DEFC EMPTYSAMORN = ASMPC-1  ;C630
	defb 1,0,$90 ;delete $90 if you don't need default sample

;first 12 values of tone tables (packed)

T_PACK:             ;#C633
	defw $D80D	;try to correct with defw
	defb $0755-$06EC
	defb $07C5-$0755
	defb $083B-$07C5
	defb $08B8-$083B
	defb $093D-$08B8
	defb $09CA-$093D
	defb $0A5F-$09CA
	defb $0AFC-$0A5F
	defb $0BA4-$0AFC
	defb $0C55-$0BA4
	defb $0D10-$0C55

	defw $DA0C	;try to correct with defw
	defb $06CF-$066D
	defb $0737-$06CF
	defb $07A4-$0737
	defb $0819-$07A4
	defb $0894-$0819
	defb $0917-$0894
	defb $09A1-$0917
	defb $0A33-$09A1
	defb $0ACF-$0A33
	defb $0B73-$0ACF
	defb $0C22-$0B73
	defb $0CDA-$0C22

	defw $080E	;try to correct with defw
	defb $076E-$0704
	defb $07E0-$076E
	defb $0858-$07E0
	defb $08D6-$0858
	defb $095C-$08D6
	defb $09EC-$095C
	defb $0A82-$09EC
	defb $0B22-$0A82
	defb $0BCC-$0B22
	defb $0C80-$0BCC
	defb $0D3E-$0C80

	defw $C00F	;try to correct with defw
	defb $0858-$07E0
	defb $08E0-$0858
	defb $0960-$08E0
	defb $09F0-$0960
	defb $0A88-$09F0
	defb $0B28-$0A88
	defb $0BD8-$0B28
	defb $0C80-$0BD8
	defb $0D60-$0C80
	defb $0E10-$0D60
	defb $0EF8-$0E10


; --------------------------------------------
; ZX0 decoder by Einar Saukas & Urusergi
; "Standard" version (68 bytes only)
; --------------------------------------------
; Parameters:
;   HL: source address (compressed data)
;   DE: destination address (decompressing)
; --------------------------------------------
dzx0_standard:
        ld      bc, $ffff               ; preserve default offset 1
        push    bc
        inc     bc
        ld      a, $80
dzx0s_literals:
        call    dzx0s_elias             ; obtain length
        ldir                            ; copy literals
        add     a, a                    ; copy from last offset or new offset?
        jr      c, dzx0s_new_offset
        call    dzx0s_elias             ; obtain length
dzx0s_copy:
        ex      (sp), hl                ; preserve source, restore offset
        push    hl                      ; preserve offset
        add     hl, de                  ; calculate destination - offset
        ldir                            ; copy from offset
        pop     hl                      ; restore offset
        ex      (sp), hl                ; preserve offset, restore source
        add     a, a                    ; copy from literals or new offset?
        jr      nc, dzx0s_literals
dzx0s_new_offset:
        pop     bc                      ; discard last offset
        ld      c, $fe                  ; prepare negative offset
        call    dzx0s_elias_loop        ; obtain offset MSB
        inc     c
        ret     z                       ; check end marker
        ld      b, c
        ld      c, (hl)                 ; obtain offset LSB
        inc     hl
        rr      b                       ; last offset bit becomes first length bit
        rr      c
        push    bc                      ; preserve new offset
        ld      bc, 1                   ; obtain length
        call    nc, dzx0s_elias_backtrack
        inc     bc
        jr      dzx0s_copy
dzx0s_elias:
        inc     c                       ; interlaced Elias gamma coding
dzx0s_elias_loop:
        add     a, a
        jr      nz, dzx0s_elias_skip
        ld      a, (hl)                 ; load another group of 8 bits
        inc     hl
        rla
dzx0s_elias_skip:
        ret     c
dzx0s_elias_backtrack:
        add     a, a
        rl      c
        rl      b
        jr      dzx0s_elias_loop
; --------------------------------------------------------

;Release 0 steps:
;11.Sep.2004 - Note tables creator
;12.Sep.2004 - Volume tables creator; INIT subroutine
;13.Sep.2004 - Play counters, position counters
;14.Sep.2004 - Patterns decoder subroutine
;15.Sep.2004 - Resting (no code)
;16.Sep.2004 - CHREGS subroutine; global debugging; 1st stable
;version was born
;17.Sep.2004 - Debugging and optimization. First release!
;Release 1 steps:
;20.Sep.2004 - local vars moved to code (selfmodified code
;smaller and faster)
;22.Sep.2004 - added mute sound entry at START+8; position
;pointer moved to START+11; added setup and status byte at
;START+10 noloop mode and loop passed flags added
;Release 2 steps:
;28.Sep.2004 - Optimization: code around CHREGS's volume and
;vibrato faster now; zeroing PD_RES through stack; Ton and Ampl
;moved from channel vars to global ones; first position selector
;removed from INIT; optimization for packers(Ivan Roshin method)
;Release 3 steps:
;2.Oct.2004 - optimization in INIT and PD_LOOP (thanks to Ivan
;Roshin)
;4.Oct.2004 - load delay from (hl) in INIT (2 bytes shorter)
;5.Oct.2004 - optimization in PD_LOOP (->PD_LP2)
;7.Oct.2004 - swaping some commands for better packing
;Release 4 steps:
;9.Oct.2004 - optimization around LD HL,SPCCOMS (thanks to Ivan
;Roshin); in PTDECOD swapped BC and DE to optimize C_PORTM;
;removed sam and orn len and loop channel vars; CHREGS totally
;optimized
;Release 5 steps:
;11.Oct.2004 - PD_OrSm and C_PORTM optimized; Ivan Roshin's
;volume tables creator algorithm (51 bytes shorter than mine)
;12.Oct.2004 - Ivan Roshin's note tables creator algorithm (74
;bytes shorter than mine)
;Release 6 steps:
;14.Oct.2004 - loop and next position calculations moved to INIT
;15.Oct.2004 - AdInPt moved to code
;19.Oct.2004 - Env_Del moved to code
;20.Oct.2004 - Version PUSH and POP (1 byte shorter, thanks to
;Ivan Roshin)
;22.Oct.2004 - Env_En moved from Flags' bit to byte (shorter and
;faster code)
;25.Oct.2004 - SETENV optimized
;29.Oct.2004 - Optimized around AddToEn (SBC A,A, thanks to Ivan
;Roshin)
;3.Nov.2004 - Note tables data was compressed; with depacker it
;is 9 bytes shorter than uncompressed (thanks to Ivan Roshin)
;4.Nov.2004 - default sample and ornament both are fixed now
;and placed into code block (6 bytes shorter)
;7.Nov.2004 - LD A,(Ns_Base):LD L,A changed to LD HL,(Ns_Base)
;(thanks to Dima Bystrov)
;9.Nov.2004 - Ns_Base and AddToNs are merged to Ns_Base_AddToNs;
;LD A,255 changed to DEC A (at start of PLAY); added ROUT_A0
;12.Nov.2004 - NtSkCn&Volume are merged (8 bytes smaller init);
;LD BC,T1_ changed to PUSH DE...POP BC in note table creator
;19.Dec.2004 - NT_DATA reorganized (6 bytes shorter, thanks to
;Ivan Roshin); C_PORTM and C_GLISS are merged via SET_STP (48
;tacts slower, but 8 bytes smaller, thanks to Ivan Roshin)
;15.Apr.2007 - all in-code variables and self-modified code
;moved to VARS (specially for Axor), code can run in ROM now.
;29.Apr.2007 - new 1.xx and 2.xx interpretation for PT 3.7+.

;Size:
;Code block $664 bytes
;Variables $23B bytes (can be stripped)
;Size in RAM $664+$23B=$89F (2207) bytes

;Notes:
;Pro Tracker 3.4r can not be detected by header, so PT3.4r tone
;tables really used only for modules of 3.3 and older versions.
