                .CR     6502            Use 6502 overlay
				.OR		$300			**Always put before .TF directive and never use again in program
				.TF     CONT.HRCG.BIN,BIN
				.EF		errors
				;.LI 	OFF				Switches the assembler machine code conversion listing off so errors are easier to see				
				.LF		C:\MY_CODE\LIST
;				***For include files, look at end of program, before variable definitions. 
;
;CUSTOM CONROLLER FOR HI-RES CHARACTER GENERATION
;
;To update on a master disk image......
;first delete the existing
;controller file "CONT" from the disk image. Then, use SBASM
;to compile this file and use AC Commander to add it to the master
;disk image. Verify the hello program BLOADs this file at $300.
;
;The apple commander line was in go2b.bat and I just commented it out
;look for the one involving the file "CONT"

HOOK	;=====CHANGES DOS I/O PATH TO ENTRY ROUTIE BELOW=====
	LDA #ENTRY		;PRODUCES LOW BYTE
	STA CSW
	LDA /ENTRY		;PRODUCES HIGH BYTES
	STA CSW+$1
	JMP VECT	
ENTRY
	CMP	#$A0		;IS CONTROL CHACTER IN ACC?
			;LDA #$BD		;PRODUCES LOW BYTE
			;STA CSW
			;LDA #$9E	;PRODUCES HIGH BYTES
			;STA CSW+$1
			;JMP VECT
			;JMP $FF69
			;BRK
	BCC	OUT			;IF YES, EXIT AND PASS IT TO COUT1
	PHA				;STORE CHAR
	AND #$7F		;CLEAR HI BIT
	STA POSN
	LDA #$00
	STA POSN+$1
	TYA
	PHA				;SAVE Y-REG
	
CALC1	
	SEC
	LDA POSN
	SBC #$20
	STA POSN		; CHAR > 96
	ASL	POSN		; *2 = CHAR < 192
	ASL	POSN		; *4 < 384
	ROL POSN+$1		
	ASL POSN		; *8 < 768
	ROL POSN+$1

;POSN = (ASCII VALUE - $20)*8byt PER CHAR	

	CLC
	LDA #TABLE		;LOW BYTE
	ADC POSN
	STA POSN
	LDA /TABLE		;HIGH BYTE
	ADC	POSN+$1
	STA POSN+$1		;POSN=POSN + TABLE ADDR
	
CALC2
	CLC
	LDA BASL
	ADC CH
	STA SCRN
	STA SCRN2		;PAGE2
	LDA BASL+$1
	ADC	#$1C
	STA SCRN+$1		;SCRN = BASL + CH + $1C00
	ADC #$20
	STA SCRN2+$1	;PAGE2

GETBYTE
	LDY #$00
G1	
	LDA (POSN),Y
	STA (SCRN),Y
	STA (SCRN2),Y
INC
	INY
	CLC
	LDA SCRN
	ADC #$FF
	STA SCRN
	STA SCRN2		;PAGE2
	LDA SCRN+$1
	ADC #$03
	STA SCRN+$1		;SCRN SCRN + $3FF
 	ADC #$20
	STA SCRN2+$1	;PAGE2
	
	CPY #$08		;DONE?
	BCC G1			;IF NO, CONTINUE LOOP

	PLA				;IF YES, RESTORE REGISTERES, PASS CHAR TO COUT1, AND EXIT
	TAY			
	PLA				;RESTORE ORIGINAL CHAR VALUE TO ACC
OUT
	JMP COUT1
	RTS

	
	
CSW		.EQ		$36
BASL	.EQ		$28
CH		.EQ		$24
POSN	.EQ		$3C		;(BAS2)
SCRN	.EQ		$3E		;(A4)
SCRN2	.EQ		$EB
VECT	.EQ		$3EA
COUT1	.EQ		$FDF0
TABLE	.EQ		$0C00

TEXT				.EQ $C051
PAGE1				.EQ	$C054

