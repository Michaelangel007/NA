;************************INCLUDE FILE*****************************
;(do not assemble stand-alone. For a stand-alone version of this routine see examples/rwts2)

;NOTE: Part of this include file is not setup in a normal format. 
;Below the line labeled COPY/PASE the code for the routines is 
;commented out. It is designed to be a copy/paste resource, as I
;am not sure yet what I want text routines I want in an include 
;file. 
;
;The other benifit of this include file is it's variable 
;definition section. Note that the real comments in the 
;copy/paste section have two ;; I commented out the code so it 
;doesn't take up space when assembling

	
COUT ;========OUTPUT 1 CHARACTER TO DEFAULT OUTPUT DEVICE=====
@START
;PARAMETERS: ACC (ascii value of char to output)
;RETURN: character output to video screen


		
	STA SAVED.ACC.LOCAL		;save parameter, output character

;CALCULATE CHARACTER SHAPE ADDRESS

	AND #$7F		;CLEAR HI BIT
	STA HRCG.SHAPE.OFFSET
	LDA #$00
	STA HRCG.SHAPE.OFFSET+$1
	
;CALC1	
	SEC
	LDA HRCG.SHAPE.OFFSET
	SBC #$20
	STA HRCG.SHAPE.OFFSET		; CHAR > 96
	ASL	HRCG.SHAPE.OFFSET		; *2 = CHAR < 192
	ASL	HRCG.SHAPE.OFFSET		; *4 < 384
	ROL HRCG.SHAPE.OFFSET+$1		
	ASL HRCG.SHAPE.OFFSET		; *8 < 768
	ROL HRCG.SHAPE.OFFSET+$1

	
;LOAD CHARACTER SHAPE TABLE
			
		;AUX MEMORY	-> MAIN MEMORY 
		LDA #HRCG.AUX.START	;SET START ADDRESS
		CLC 
		ADC HRCG.SHAPE.OFFSET
		STA AUX_MOVE.START
		LDA /HRCG.AUX.START
		ADC HRCG.SHAPE.OFFSET+$1	;16-BIT ADD
		STA AUX_MOVE.START+$1
		
		LDA AUX_MOVE.START		;SET END ADDRESS
		CLC
		ADC #HRCG.SHAPE.SIZE
		STA AUX_MOVE.END
		LDA AUX_MOVE.START+$1
		ADC #$00				;16-BIT ADD
		STA AUX_MOVE.END+$1
		
		LDA #HRCG.BUFFER		;SET DESTINATION ADDRESS
		STA AUX_MOVE.DEST
		LDA /HRCG.BUFFER
		STA AUX_MOVE.DEST+$1
		CLC                ;CLEAR CARRY FLAG DESGINATD MOVE FROM AUX -> MAIN MEMORY 
		JSR AUX_MOVE
			
;ENABLE ROM ROUTINES
	LDA $C082				;READ ENABLE ROM, DISABLE WRITE ON BSM (NORMAL STATE).
							;REQUIRED FOR BEFORE BRK BECAUSE APPLE MONITOR IS A ROM ROUTINE
	
	LDA SAVED.ACC.LOCAL		;restore output character
	JSR COUT.ADDRESS
	
		
;ENABLE.BS_RAM BEFORE CONTINUING
	LDA $C083				;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK 1ST)
	LDA $C083

	

			
	RTS

@END

HCG.ON	;=====TURN ON HI-RES CHARACTER GENERATION======
@START
;ENABLE ROM ROUTINES
	LDA $C082				;READ ENABLE ROM, DISABLE WRITE ON BSM (NORMAL STATE).
							;REQUIRED FOR BEFORE BRK BECAUSE APPLE MONITOR IS A ROM ROUTINE

	JSR HCG.ON.ADDRESS

	;ENABLE.BS_RAM BEFORE EXIT
	LDA $C083				;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK 1ST)
	LDA $C083
	
	RTS
@END

HCG.OFF			;======EMERGENCY BREAK, NEVER USE=====
@START

;=====================SUBROUTINE DOCUMENTATION====================================
;
;After calling HCG.ON to connect the custom HRCG driver to COUT, 
;the apple monitor will have problems displaying the contents
;of memory addresses (at endless stream of zeros is one of the
;results I've seen)
;
;This routine is designed to be called just before a BRK. This
;routine disconnects the custom HRCG driver and returns the COUT
;vector to the normal COUT address. 
;
;NOTE: HCG.ON ends with a JMP VECT ($03EA), which in turn does a JMP somewhere else
;and I think is needed to implement the change to the vector values in $36. 
;However, after HCG.ON is executed, $03EA becomes $60 (RTS). 
;For awhile I was doing the JMP VECT at the end of this routine
;and it was working just fine, in AppleWIN, presumably because
;this routine is called via a JSR so the RTS was just returning to 
;the command after teh JSR HCG.OFF. However, for some reason on the
;physical Apple IIe, doing the JMP VECT at the end of this 
;routine was causing a screen of garbage text with
;no access to the monitor after a BRK. So, I stopped doing
;the JMP VECT and instead just do an RTS at the end of this routine. That fixed the isssue
;on the physical IIe and it also works in AppleWIN. 
;**UPDATE**: Actually, this fix seems to result in it working on the
;physical IIe some of the time, but not all of the time. haven't
;been able to find a pattern.
;
;
;
;=================================================================================

; ;SAVE REGISTERS	
	;***NOT NEEDED, NO REGISTERES ARE MODIFIED, AND FOR SOME
	;REASON PUSHING AND PULLING THE REGISTERES AT THE START/END
	;OF THIS ROUTINE CAUSES STRANGE UNDESIRED RESULTS.
	;
	;NOTE: since the register values stay intact it looks like
	;using LDA to flip soft switches (i.e. LDA TEXT, LDA $C082) 
	;doesn't actually change the contents of the accumulator. 
	
	
;RETURN OUTPUT HOOK TO NORMAL SETTINGS	
	LDA #$F0		;PRODUCES LOW BYTE
	STA CSW
	LDA #$FD		;PRODUCES HIGH BYTES
	STA CSW+$1
	;JMP VECT

; ;RESTORE REGISTERS
	;***DON'T DO IT. DON'T RAISE THAT IRON! SEE NOTES UNDER "SAVE REGISTERS" ABOVE
	
	RTS
@END
			
PRINT.BCD_PACKED ;======OUTPUT 4 BCD DIGITS (2 BYTES PACKED) TO VIDEO DISPLAY (LEADING ZEROS KEPT)======
@START
;PARAMETERS: BCD (2)
;RETURN: SCREEN OUTPUT

;=====================SUBROUTINE DOCUMENTATION====================================
;
;Prints 4 BCD digits and supresses leading 0s. For example, if 
;the digits 1-4 are 0907, the output to the video screen will be
;907
;
;The digits output are organized as LO and HO, just like most
;things in assembler. 
;
;BCD		= 3rd/4th digits 
;BCD+$1		= 1st/2nd digits
;
;=================================================================================



	; ;**DRIVER TEMPLATE**	
	; LDA #$12
	; STA BCD				;2ND # TO DISPLAY ONSCREEN
	; LDA #$74				;1ST # TO DISPLAY ONSCREEN
	; STA BCD+$1
	; ;
	; JSR PRINT.BCD_PACKED
	
							
	JSR CONVERT.BCD_PACKED.ASCII
				
	LDX #$04
.DIGIT1
	LDA RESULT+$3
	CMP #$B0
	BNE .PRINT.DIGIT1
	DEX
	JMP .DIGIT2
	
.PRINT.DIGIT1

				
	JSR COUT
	
.DIGIT2
	LDA RESULT+$2
	CMP #$B0
	BNE .PRINT.DIGIT2
	CPX #$03
	BNE .PRINT.DIGIT2
	DEX
	JMP .DIGIT3
	
.PRINT.DIGIT2
	JSR COUT
				
.DIGIT3
	LDA RESULT+$1
	CMP #$B0
	BNE .PRINT.DIGIT3
	CPX #$02
	BNE .PRINT.DIGIT3
	DEX
	JMP .DIGIT4
	
.PRINT.DIGIT3
	JSR COUT	


.DIGIT4
	LDA RESULT+$0
	JSR COUT

	RTS

@END

PRINT_FIXED.BCD_PACKED ;======OUTPUT 4 BCD DIGITS (2 BYTES PACKED) TO VIDEO DISPLAY (LEADING ZEROS KEPT)======
@START
;PARAMETERS: BCD (2)
;RETURN: SCREEN OUTPUT

;=====================SUBROUTINE DOCUMENTATION====================================
;
;Prints 4 BCD digits and do not supresses leading 0s. For example, if 
;the digits 1-4 are 0907, the output to the video screen will be
;0907
;
;The digits output are organized as LO and HO, just like most
;things in assembler. 
;
;BCD		= 3rd/4th digits 
;BCD+$1		= 1st/2nd digits
;
;=================================================================================



	; ;**DRIVER TEMPLATE**	
	; LDA #$12
	; STA BCD				;2ND # TO DISPLAY ONSCREEN
	; LDA #$74				;1ST # TO DISPLAY ONSCREEN
	; STA BCD+$1
	; ;
	; JSR PRINT_FIXED.BCD_PACKED
	
			
	JSR CONVERT.BCD_PACKED.ASCII
	
.DIGIT1
	LDA RESULT+$3				
	JSR COUT
	
.DIGIT2
	LDA RESULT+$2
	JSR COUT
				
.DIGIT3
	LDA RESULT+$1
	JSR COUT	


.DIGIT4
	LDA RESULT+$0
	JSR COUT

	RTS
	
	
@END
	
PRINT.STR ; ======OUTPUT ASCII STRING TO VIDEO DISPLAY, NO CARRIAGE RETURN=====
@START
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
; ;PRINT "GOLD"
	; JMP .TEXT1.PRINT
	;
; .TEXT1 .AZ /Gold/			;ASCII text string
; .TEXT1.PRINT
		; LDA #TEXT1 					
		; STA STRING
		;
		; LDA /TEXT1
		; STA STRING+$1						
	; JSR PRINT.STR
	

				
;SAVE REGISTERS
	TYA
	PHA
							
	LDY #$0				;init string index

	
.LOOP
	LDA (STRING),Y
	BEQ .EXIT			;if string stop value found, exit. SBASM adds a $00 stop value to its .AZ ascii hex tables. 

;	CLC
;	ADC #$80			;convert from LO hex ascii value (used by SBASM) to HI hex ascii value (required by COUT ROM routine)
		
	JSR COUT
			
	INY					;increment string index
	BNE .LOOP

.EXIT  
	
;RESTORE REGISTERS
	PLA
	TAY
  

			
	RTS

@END
	

STOP	;========CUSTOM EXIT GRAPHICS ROUTINE=======
@START
	
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

@END


UPDATE.CHAR.POS ;============MOVES CURSOR TO HTAB/VTAB POSITION ON TEXT SCREEN
@START

;ENABLE ROM ROUTINES
	LDA $C082				;READ ENABLE ROM, DISABLE WRITE ON BSM (NORMAL STATE).
							;REQUIRED FOR BEFORE BRK BECAUSE APPLE MONITOR IS A ROM ROUTINE
 
	JSR UPDATE.CHAR.POS.ADDRESS

;ENABLE.BS_RAM BEFORE EXIT
	LDA $C083				;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK 1ST)
	LDA $C083
	
	RTS

@END

;=============================COPY/PASTE ONLY===========
@START				
				
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
@END

;======DEFINE VARIBLES======
GRAPHICS 			.EQ	$C050
TEXT				.EQ $C051
HIRES				.EQ	$C057
PAGE1				.EQ	$C054
MIXOFF				.EQ	$C052
PAGE2				.EQ	$C055		
	
	
;TEXT SCREEN VARIABLES 
CLEAR.TEXT.SCREEN	.EQ $FC58
HCG.ON.ADDRESS		.EQ $0300
COUT.ADDRESS		.EQ $FDED			;SEND ACC TO DEFAULT OUTPUT DEVICE
;COUT				.EQ $FDED			;SEND ACC TO DEFAULT OUTPUT DEVICE
COUT.V				.EQ	$FDF0			;SEND ACC TO VIDEO SCREEN
UPDATE.CHAR.POS.ADDRESS		.EQ	$FC22			;RECALCULATE TEXT SCREEN LINE VALUE STORED AT $28, BASED ON ROW VALUE STORED AT $25
UPDATE.CURSOR.POS.ADDRESS 	.EQ UPDATE.CHAR.POS.ADDRESS
UPDATE.CURSOR.POS		 	.EQ UPDATE.CHAR.POS


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
STRING .EQ 			$FC		;2byte. Pointer to the ascii string to be output to video screen.

;PRINT.BCD VARIABLES
BCD.DIGITS 			.BS 1

