;************************INCLUDE FILE*****************************
;(do not assemble stand-alone. For a stand-alone version of this routine see examples/rwts2)

;NOTE: This is not a normal include file. Below the line labeled
;COPY/PASE the code for the routines is commented out. It is designed
;to be a copy/paste resource, as I am not sure yet what I want text
;routines I want in an include file. 
;The other benifit of this include file is it's variable definition section. 
;Note that the real comments in the copy/paste section have two ;;
;I commented out the code so it doesn't take up space when assembling

	
	
	
PRINT.STR ; ======OUTPUT ASCII STRING TO VIDEO DISPLAY, WITH CARRIAGE RETURN=====
;PARAMETERS: STRING (HO/LO ADDRESS OF ASCII STRING TO PRINT)
;ENTRANCE: DIRECT
;RETURN VALUE: NONE

;NOTE: ascii string must use the LO values on the ascii chart
;in order to work with this subtroutine. SBASM uses the LO values
;when the .AZ directive is used.
;
;--LO Value Quick Reference--
;Carriage Return: $08

; ;DRIVER TEMPLATE
	;
	; LDA #LINE1 
	; STA STRING
	;
	; LDA /LINE1
	; STA STRING+$1			
	;
	;
	; JSR PRINT.STR
	
	LDY #$0				;init string index

	
.LOOP
	LDA (STRING),Y
	BEQ .EXIT			;if string stop value found, exit

	CLC
	ADC #$80			;convert from LO hex ascii value (used by SBASM) to HI hex ascii value (required by COUT ROM routine)
		 
	JSR COUT
	INY					;increment string index
	BNE .LOOP

.EXIT     
	RTS


	

		 
		 
		 


STOP	;CUSTOM EXIT GRAPHICS ROUTINE
	
;PAUSE UNTIL A KEY IS PRESSED	
.KEYIN  

	LDA $C000
	BPL .KEYIN
	STA $C010               ;CLR LAST KEY


;RETURN TEXT WINDOW TO NORMAL
	LDA #$00		;(!0-39) (X) SET UPPER LEFT COLUMN (TEXT WRITES TO THIS COLUMN)
	STA TW1			;($1-28)
	LDA #$28		;(!1-40) (X) SET WIDTH (I.E. $03 = 3 COLUMNS WIDE, 0 DOESN'T COUNT)
	STA TW2			;($1-28)
	LDA #$00		;(!0-23) (Y) TOP ROW 
	STA TW3			;($1-18)
	LDA #$18		;(!0-23) (Y) BOTTOM ROW (TEXT STOPS 1 ROW ABOVE THIS VALUE)
	STA TW4			;($1-18)	(bottom row - top row = total # of rows)

	
	
	LDA TEXT
	LDA PAGE1
	JMP $FF69

	BRK




;=============================COPY/PASTE ONLY===========
				
				
;;	JMP START		;SKIP GRAPHICS MODE

	
	; LDA GRAPHICS	;TURN ON GRAPHICS MODES
	; LDA HIRES		;SELECT HI-RES MODE
	; LDA	PAGE1		;SELECT PAGE 1
	; LDA MIXOFF		;SELECT FULL SCREEN GRAPHICS (PAGE 1)
	
	
	; LDA #$00		;SPECIFY BOTH PAGES FOR CLEAR SCREEN
	; JSR SCLEAR		;CLEAR SCREEN BEFORE TURNING ON GRAPHICS (AVOIDS UNSIGHTLY FLASH OF RANDOM DOTS) 


;START
	
	; JSR HCG.ON		;REDIRECT DOS OUTPUT ROUTINES VIA CUSTOM HCG OUTPUT HANDLER AT $03D0
	
	; JMP	SCENARIO4
	
	
;SCENARIO1	;;======CONTROL CURSOR POSITION=====
	
;;SET CURSOR POSITION 
	; LDA #$0
	; STA HTAB	
	; LDA #$0
	; STA VTAB
	; JSR	UPDATE.CHAR.POS
	; LDA #$C8
	; JSR COUT

	; JSR STOP

;SCENARIO2	;=======TEXT WINDOW

	; LDA #$00	
	; TAX
	; TAY
	
	; JSR TEXT.WINDOW
	; LDA #$00
	; STA HTAB
	; STA VTAB
	; JSR UPDATE.CHAR.POS
	; LDA #$A4
	; LDX #$00
; .LOOP0	
	; JSR COUT
	; INX
	; CPX #$40
	; BNE .LOOP0
	
	; JSR STOP
	


;SCENARIO3	;DEMONSTRATE CONTROL OVER EVERY X,Y COORDINATE
	; LDA #$00	
	; TAX
	; TAY
	
; .LOOP1	
	; STY VTAB		;Y (VERT POSITION)
; .PLOT.ROW
	; STX HTAB		;X	(HOR POSITION)		
	; JSR	UPDATE.CHAR.POS
	; LDA #$A4
	; JSR	COUT
	
	; JSR WAIT
	; INX
	; CPX #$28
	; BNE .PLOT.ROW	
	; LDX #$00
	; INY
	; CPY	#$19 
	; BNE .LOOP1
	

	; JSR STOP

;SCENARIO4	;DISPLAY EACH CHARACTER IN SET
	; LDA #$00	
	; TAX
	; TAY
	
	; LDA #$A1
	; STA CHAR
; .LOOP1	
	; STY VTAB		;Y (VERT POSITION)
; .PLOT.ROW
	; STX HTAB		;X	(HOR POSITION)		
	; JSR	UPDATE.CHAR.POS
	; LDA CHAR
	; JSR	COUT
	; LDA #$A0		;SPACE
	; JSR COUT
	
	; JSR WAIT
	; INC CHAR
	; INX
	; INX
	; CPX #$28
	; BNE .PLOT.ROW	
	; LDX #$00
	; INY
	; CPY	#$19 
	; BNE .LOOP1
	

	; JSR STOP	
	
;=================SUBROUTINES=============

;TEXT.WINDOW
;;SETUP TEXT WINDOW
	; LDA #$00		;(!0-39) (X) SET UPPER LEFT COLUMN (TEXT WRITES TO THIS COLUMN)
	; STA TW1			;($0-27)
	; LDA #$02		;(!0-39) (X) SET WIDTH (I.E. $03 = 3 COLUMNS WIDE, 0 DOESN'T COUNT)
	; STA TW2			;($0-27)
	; LDA #$00		;(!0-23) (Y) TOP ROW 
	; STA TW3			;($0-18)
	; LDA #$18		;(!0-23) (Y) BOTTOM ROW (TEXT STOPS 1 ROW ABOVE THIS VALUE)
	; STA TW4			;($0-18)	(bottom row - top row = total # of rows)

	; RTS
	
;HCG.OFF			;======EMERGENCY BREAK, NEVER USE=====

;****These used to be needed to prevent garbage from printing to
;graphics screen, but I think that's a non issue as long as I do
;a key-in before returing to Text mode.
;And, at some point when using the return output hook sequence,
;it started hanging the program. 
;RETURN OUTPUT HOOK TO NORMAL SETTINGS	
;	LDA #$BD		;PRODUCES LOW BYTE
;	STA CSW
;	LDA #$9E		;PRODUCES HIGH BYTES
;	STA CSW+$1
;	JMP VECT
;	RTS
	
	
;======INCLUDE FILES======

				; .IN 	C:\MY_CODE\INCLUDES_LIBS\routines_general
				; .IN 	C:\MY_CODE\INCLUDES_LIBS\routines_graphics
	
;======DEFINE VARIBLES======
GRAPHICS 			.EQ	$C050
TEXT				.EQ $C051
HIRES				.EQ	$C057
PAGE1				.EQ	$C054
MIXOFF				.EQ	$C052
PAGE2				.EQ	$C055		



;TEXT SCREEN VARIABLES 
CLEAR.TEXT.SCREEN	.EQ $FC58
HCG.ON				.EQ $0300
COUT				.EQ $FDED			;SEND ACC TO DEFAULT OUTPUT DEVICE
COUT.V				.EQ	$FDF0			;SEND ACC TO VIDEO SCREEN
UPDATE.CHAR.POS		.EQ	$FC22			;RECALCULATE TEXT SCREEN LINE VALUE STORED AT $28, BASED ON ROW VALUE STORED AT $25
HTAB				.EQ $24				;(X) HORIZONTAL CURSOR POSITION
VTAB				.EQ $25				;(Y) VERTICLE CURSOR POSITION
TW1					.EQ $20				;(X) TEXT WINDOW UPPER LEFT
TW2					.EQ	$21				;(X) WIDTH
TW3					.EQ $22				;(Y) TOP ROW
TW4					.EQ	$23				;(Y) BOTTOM ROW

CHAR				.BS		$1			

CSW					.EQ		$36
VECT				.EQ		$3EA		

;PRINT.STR VARIBLES
STRING .EQ $FC		;2byte. Pointer to the ascii string to be output to video screen.
