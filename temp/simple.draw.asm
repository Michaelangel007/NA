                .CR     6502            Use 6502 overlay
				.OR		$6300			**Always put before .TF directive and never use again in program
				.TF     TEST.BIN,BIN
				.EF		errors
;				.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_common

;Turn Hi-RES ON
	
	LDA GRAPHICS	;TURN ON GRAPHICS MODE
	LDA HIRES	;SELECT HI-RES MODE
	LDA	PAGE1	;SELECT PAGE 1
	LDA MIXOFF	;SELECT FULL SCREEN GRAPHICS (PAGE 1)

;	JMP TEST1
	
;Clear Screen
SCLEAR	NOP
	LDA #$00
	STA $26
	LDA #$20
	STA $27
CLR1 NOP
	LDY #$00
	LDA	#$00
CLR	NOP
	STA	($26), Y
	INY
	BNE CLR
	INC $27
	LDA $27
	CMP #$40
	BCC	CLR1		;is acc less than cmp


TEST1
	
;Plot Byte
	LDA #$08
	STA $2000
	LDA #$3E
	STA $2400
	LDA #$5D
	STA $2800
	LDA #$1C
	STA $2C00
	LDA #$14
	STA $3000
	LDA #$22
	STA $3400
	RTS
	

	
;======DEFINE VARIBLES======
GRAPHICS 	.EQ	$C050
HIRES		.EQ	$C057
PAGE1		.EQ	$C054
MIXOFF		.EQ	$C052
