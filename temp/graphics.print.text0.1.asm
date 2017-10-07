                .CR     6502            Use 6502 overlay
				.OR		$6000			**Always put before .TF directive and never use again in program
				.TF     TEST.BIN,BIN
				.EF		errors
				.LI 	OFF				Switches the assembler machine code conversion listing off so errors are easier to see				
				;.LF		C:\MY_CODE\LIST
;				***For include files, look at end of program, before variable definitions. 


;STATUS: The DRAW.CHAR routine I wrote and it works in conjunction with the character shape tables
;from the book Assembly Line Cookbook (see PDF page 318-323). It may come in handy at some point.

;The routine BOOK.CHAR is the driver for the book routines (HOOK and ENTRY), which are supposted to redirect the active output device to the ENTRY routine, thus
;printing a character from the shape tables to the Hi-Res screen. I was able to get it to work by loading HOOK & ENTRY to $300 and seperately loading a character set to $9000. 


;The setup of BOOK.CHAR/HOOK & ENTRY in this file is also very 
;useful to troubleshoot at a deep level. It is setup to pass a 
;value to the modified output routine (ENTRY) directly, without
;calling HOOK. This is necessary because in order to BRK or jump to montior,
;during the modified output routine (stored at $030D in file controller_hcrg)
;you can't have the output hooks modified (by JSR $300) or the program
;will do an endless loop. I tried resetting the output hook to default then BRK but it didn't work. 				
;
;WARNING: If changes are made, when reassembling controller (controller.hcgs.asm) verify code ends no later than $396. $397 and possibly beyond are used by something and the call to HOOK will go haywire if $397 (and likely beyond) is modified. 
	
	
;WARNING: FULL CHARACTER SET NOT LOADED IN THIS ROUTINE, 
;DON'T USE VALUES ABOVE $A4 ($ CHAR)
;

;	JMP START		;SKIP GRAPHICS MODE
	
	LDA GRAPHICS	;TURN ON GRAPHICS MODE
	LDA HIRES		;SELECT HI-RES MODE
	LDA	PAGE1		;SELECT PAGE 1
	LDA MIXOFF		;SELECT FULL SCREEN GRAPHICS (PAGE 1)
	
	
	LDA #$01
	JSR SCLEAR
	
	JMP START		;SKIP TEST MODE

TEST
	

START

;	JMP DRAW.CHAR		;DRAW CHARACTER USING MY ROUTINE
	JMP BOOK.CHAR		;DRAW CHARACTER USING BOOK ROUTINE


	
DRAW.CHAR		
		LDA #$00
		STA SCREEN.BYTE		;START DRAWING CHAR IN SCREEN BYTE 1
		STA CHAR.TBL.CNTR   ;INIT COUNTER
		
		LDA #$08
		STA	CHAR.LINE
		LDA #$10
		STA CHAR.LINE.STOP
		
		LDA #A					;SET CHAR TO#
		STA CHAR.TABLE
		LDA /A					;SET CHAR TO#
		STA CHAR.TABLE+$1
			
	
		LDX	#$00			;START DRAWING CHAR AT LINE 1
		LDY #$00			;INIT INDEX 
.DRAW.CHAR.LOOP		
	LDX CHAR.LINE			;LOAD LINE IN X REGISTER
	
	LDA #$01
	JSR GET.LINE.ADDRESS1	;GET LINE ADDRESS
							
	LDY CHAR.TBL.CNTR		;LOAD Y WITH SHAPE TABLE COUNTER
	LDA (CHAR.TABLE),Y		;LOAD SHAPE BYTE (1st screen byte of the tile)
		
		
	LDY SCREEN.BYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	STA (LINE.BASE.ADDR1),Y	;PLOT (1st screen byte)
	
	INC CHAR.LINE
	INC CHAR.TBL.CNTR		;NEXT SHAPE BYTE, NEXT TILE LINE
	
	LDA CHAR.LINE				
	CMP CHAR.LINE.STOP		;IS TILE DONE?							
	BCC .DRAW.CHAR.LOOP			;IF NO, DRAW NEXT LINE (BCC: is ACC < CMP value)
	
		
	BRK

	
BOOK.CHAR

;WARNING: FULL CHARACTER SET NOT LOADED IN THIS ROUTINE, 
;DON'T USE VALUES ABOVE $A4 ($ CHAR)
;

;
;NOTE: TO RETURN TO TEXT MODE, USE JSR STOP, BUT IT HAS TO 
;BE PLACED IN THE "OUT" ROUTINE BELOW, JUST BEFORE THE JSR COUT
;OTHERWISE THE PROGRAM WILL HANG.
;
	LDA #$01
	JSR SCLEAR
	LDA #$02
	JSR SCLEAR

	LDX #$AA
	LDY #$AA
	LDA PAGE2
	LDA #$A4
	;JSR $FDF0
	JSR ENTRY

	BRK
	
		

STOP
.KEYIN  

	LDA $C000
    BPL .KEYIN
    STA $C010               ;CLR LAST KEY
	
	LDA TEXT
	LDA PAGE1
	BRK

HOOK
	LDA #ENTRY		;PRODUCES LOW BYTE
	STA CSW
	LDA /ENTRY		;PRODUCES HIGH BYTES
	STA CSW+$1
	JMP VECT
ENTRY	
	CMP	#$A0		;IS CONTROL CHACTER IN ACC?
	BCC	OUT			;IF YES, EXIT AND PASS IT TO COUT1			
	PHA
	AND #$7F		;CLEAR HI BIT
	STA POSN
	LDA #$00
	STA POSN+$1
	TYA
	PHA
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
	STA SCRN		;PAGE1
	STA SCRN2		;PAGE2
	LDA BASL+$1
	ADC	#$1C
	STA SCRN+$1		;PAGE1. SCRN = BASL + CH + $1C00
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
	STA SCRN		;PAGE1
	STA SCRN2		;PAGE2
	LDA SCRN+$1
	ADC #$03
	STA SCRN+$1		;PAGE1. SCRN SCRN + $3FF
	ADC #$20
	STA SCRN2+$1	;PAGE2
 
	CPY #$08		;DONE?
	BCC G1			;IF NO, CONTINUE LOOP

DONE				;IF YES, RESTORE REGISTERES, PASS CHAR TO COUT1, AND EXIT
	PLA
	TAY				;RESTORE Y-REG
	PLA				;RESTORE ORIGINAL CHAR VALUE TO ACC
OUT
	JSR COUT1
	RTS
	BRK
	
;======INCLUDE FILES======

				.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_graphics
				
;======DEFINE VARIBLES======
GRAPHICS 			.EQ	$C050
TEXT				.EQ $C051
HIRES				.EQ	$C057
PAGE1				.EQ	$C054
MIXOFF				.EQ	$C052
PAGE2				.EQ	$C055

CSW			.EQ		$36
BASL		.EQ		$28
CH			.EQ		$24
POSN		.EQ		$3C		;(BAS2)
SCRN		.EQ		$3E		;(A4)
SCRN2		.EQ		$EB
VECT		.EQ		$3EA
COUT1		.EQ		$FDF0
;PAGE.VALUE	.BS		$1
;X.STORAGE	.BS		$1


CHAR.TABLE		.EQ	$FC
SCREEN.BYTE		.BS $1
CHAR.TBL.CNTR   .BS $1
CHAR.LINE		.BS $1
CHAR.LINE.STOP	.BS $1
		
		.NO $9000
TABLE	
LINE1	.HS	00.00.00.00.00.00.00.00		;SPACE	;9000
LINE2	.HS	08.08.08.08.08.00.08.00		;!		;9008
LINE3	.HS	14.14.14.00.00.00.00.00		;"		;9018
LINE4	.HS	14.14.3E.14.3E.14.14.00		;#		;9020
LINE5	.HS 08.3C.0A.1C.28.1E.00.00		;$
	
A		.HS 08.14.22.22.3E.22.22.00		;A	
B		.HS 10.22.22.1E.22.22.1E.00		;B


