;************************INCLUDE FILE*****************************
;(do not assemble stand-alone.  )

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
;PARAMETERS: ACC (ascii value of char to output), [COUT_CHAR_TYPE ;($00 = normal, $7F = inverse)], [HRCG.PAGES ($01 = hi-res page 1, $02 = hi-res page 2, $03 = both pages)]
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
	ROL HRCG.SHAPE.OFFSET+$1	; transfer overflow bit from carry flag to HO byte	
	ASL HRCG.SHAPE.OFFSET		; *8 < 768
	ROL HRCG.SHAPE.OFFSET+$1	; transfer overflow bit from carry flag to HO byte

			
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
		
	JSR GET.BSR_BANK.STATUS

		
		;ENABLE ROM ROUTINES
		LDA $C082			;READ ENABLE ROM, DISABLE WRITE ON BSM (NORMAL STATE).

			
			;save registers (HRCG uses X-REG)
			TXA
			PHA
		;REQUIRED FOR BEFORE BRK BECAUSE APPLE MONITOR IS A ROM ROUTINE
		LDA SAVED.ACC.LOCAL		;restore output character

			


			
		JSR COUT.ADDRESS
		


			
			;restore registers
			PLA
			TAX


			
	JSR RESTORE.BSR_BANK.STATUS
	
.EXIT	

	
	;reset char type to normal
	LDA #$00
	STA COUT_CHAR_TYPE ;($00 = normal, $7F = inverse)

	LDA #$03 ;set both pages
	STA HRCG.PAGES ;($01 = hi-res page 1, $02 = hi-res page 2, $03 = both pages)



			
	RTS

@END

;COUT.SINGLE_PAGE ;====Uses COUT to print a character to a single hi-res page====  (turns out not to be needed because the function it performed is build directly into routines line PRINT.STR and PRINT.BCD_PACKED)
@START
; ;PARAMETERS: ;ACC = ascii character to print, HRCG.PAGES.SAVED (($01 = hi-res page 1, $02 = hi-res page 2, $03 = both pages))
; ;ENTRANCE: from a routine that sets HRCG.PAGES.SAVED from HRCG.PAGES
; ;RETURN: text character printed to the video screen on the specified hi-res page

; ;=====================SUBROUTINE DOCUMENTATION===========================================================================
; ;
; ;This routine is used by other subroutines that need to call COUT and only have the output
; ;printed to a single hi-res page. Accordingly, this routine assumes that the HRCG.PAGES
; ;parameter has already been set. This routine just restores the parm since the parm is set
; ;back to the default value of $03 (both pages) by COUT after each call. 
; ;
; ;In contrast COUT.BACKGROUND is designed for direct use anytime a text character needs to be printed 
; ;to the background page. 
; ;
; ;=============================================================================================================================

		; ;ACC = ascii character to print
		; PHA ;save ascii character to print
		; ;set hi-res page parameter 
		; LDA HRCG.PAGES.SAVED ;($01 = hi-res page 1, $02 = hi-res page 2, $03 = both pages)
		; STA HRCG.PAGES
		; PLA ;restore ascii character to print
	; JSR COUT

	; RTS
@END

COUT.BACKGROUND ;====Uses COUT to print a single character to the background page====
@START
;PARAMETERS: ;ACC = ascii character to print
;ENTRANCE: direct
;RETURN: text character printed to the video screen on the background hi-res page

;=====================SUBROUTINE DOCUMENTATION===========================================================================
;
;This routine is designed for direct use anytime a text character needs to be printed 
;to the background page. 
;
;See COUT.SINGLE_PAGE documentation for an explanation of the difference between this routine and COUT.SINGLE_PAGE
;
;=============================================================================================================================


		PHA ;save ascii character to print
		;set hi-res page parameter 
		LDA PAGE.BACKGROUND ;save ascii character to print
		STA HRCG.PAGES
		PLA ;restore ascii character to print
	JSR COUT
	

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
			
PRINT.BCD_PACKED ;======OUTPUT 4 BCD DIGITS (2 BYTES PACKED) TO VIDEO DISPLAY (LEADING ZEROS SUPRESSED)======		 
@START 			 ;======(!9999 largest # supported)
;PARAMETERS: BCD (2), Carry Flag (CLC = left justify | SEC = right justify), [HRCG.PAGES ($01 = hi-res page 1, $02 = hi-res page 2, $03 = both pages)], [COUT_CHAR_TYPE ($00 = normal, $7F = inverse)]
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
	;	SEC ;(CLC = left justify | SEC = right justify)
	; JSR PRINT.BCD_PACKED

	
.SAVE.PARAMETERS
	;Carry Flag (CLC = left justify | SEC = right justify)
	BCS .SET.RIGHT_JUSTIFY
	LDA #$00 ;set left justify mode
	JMP .SAVE.PARAMETERS.DONE
.SET.RIGHT_JUSTIFY
	LDA #$01 ;set right justify mode
	;**FALLS THROUGH**
.SAVE.PARAMETERS.DONE
	STA PRINT.BCD_PACKED.PARM.MODE ;($00 = left justify | $01 = right justify)

	;**FALLS THROUGH**
	
.INIT
	LDA HRCG.PAGES ;($01 = hi-res page 1, $02 = hi-res page 2, $03 = both pages)
	STA HRCG.PAGES.SAVED ;save the value at routine entrance because it is reset by COUT to $03 on exit

	LDA COUT_CHAR_TYPE ;($00 = normal, $7F = inverse)
	STA COUT_CHAR_TYPE.SAVED
	
	

	
	
;SAVE REGISTERS
	TXA
	PHA
	
			
	JSR CONVERT.BCD_PACKED.ASCII
			
			;**OPT** Memory. The code below this point can probably be rolled up into a loop. 
			;Load RESULT using an index. X-REG looks like it's 1 digit off from being aligned for this purpose, 
			;but maybe that can be changed, or otherwise use Y-REG and X-REG. If using Y-REG, add it to the save/restore registers sections. 
			
	LDX #$04
.DIGIT1
	LDA RESULT+$3
	CMP #$B0
	BNE .PRINT.DIGIT1
	DEX
	LDA PRINT.BCD_PACKED.PARM.MODE ;($00 = left justify | $01 = right justify)
	BEQ .DIGIT2
	LDA #$A0 ;ASCII - 'space'
	;JMP .DIGIT2
	
.PRINT.DIGIT1

		;ACC = ascii character to print			
	JSR .CALL.COUT

	
.DIGIT2
	LDA RESULT+$2
	CMP #$B0
	BNE .PRINT.DIGIT2
	CPX #$03
	BNE .PRINT.DIGIT2
	DEX
	LDA PRINT.BCD_PACKED.PARM.MODE ;($00 = left justify | $01 = right justify)
	BEQ .DIGIT3	
	LDA #$A0 ;ASCII - 'space'
	;JMP .DIGIT3
	
.PRINT.DIGIT2
		;ACC = ascii character to print			
	JSR .CALL.COUT


				
.DIGIT3
	LDA RESULT+$1
	CMP #$B0
	BNE .PRINT.DIGIT3
	CPX #$02
	BNE .PRINT.DIGIT3
	DEX
	LDA PRINT.BCD_PACKED.PARM.MODE ;($00 = left justify | $01 = right justify)
	BEQ .DIGIT4
	LDA #$A0 ;ASCII - 'space'
	;JMP .DIGIT4
	
.PRINT.DIGIT3
		;ACC = ascii character to print			
	JSR .CALL.COUT
	


.DIGIT4
	LDA RESULT+$0
		;ACC = ascii character to print			
	JSR .CALL.COUT



.EXIT

;RESTORE REGISTERS
	PLA
	TAX
	
	RTS

.CALL.COUT	
@START
		PHA ;save char to print

		;set hi-res page parameter 
		LDA HRCG.PAGES.SAVED ;($01 = hi-res page 1, $02 = hi-res page 2, $03 = both pages)
		STA HRCG.PAGES
		
		LDA COUT_CHAR_TYPE.SAVED ;($00 = normal, $7F = inverse)
		STA COUT_CHAR_TYPE 
	
		PLA ;restore char to print
	JSR COUT
	
	RTS
@END

	
@END

PRINT.BCD_PACKED.STR.BACKGROUND ;**OPT** Memory. If only inventory routines use this then it can be moved to the inventory file
@START
;PARAMETERS: [COUT_CHAR_TYPE ($00 = normal, $7F = inverse)]
;ENTRANCE: DIRECT
;RETURN: text on video screen

;NOTE: if the optional COUT_CHAR_TYPE parm is set, PRINT.BCD_PACKED will call the correct COUT entrance for it. 

		LDA PAGE.BACKGROUND
		STA HRCG.PAGES ;($01 = hi-res page 1, $02 = hi-res page 2, $03 = both pages)	
		SEC ;(CLC = left justify | SEC = right justify)		
	JSR PRINT.BCD_PACKED

.EXIT
	
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

PRINT.HEX_VALUE  ;(convert hex value to ascii and print onscreen)
@START
;PARAMETERS: ACC (hex value to print)

	STA TEMP ;save hex value to print
	
;SAVE X/Y REGISTERS
	TXA
	PHA
	TYA
	PHA
	
	LDA TEMP
	PHA ;transfer hex value to print to stack
	
;SAVE CURSOR POSITION
	LDA HTAB	
	STA CURSOR.POSITION.SAVED+$0
	LDA VTAB
	STA CURSOR.POSITION.SAVED+$1

	LDA #$1E 
	STA HTAB
	LDA #$5 
	STA VTAB
	JSR	UPDATE.CHAR.POS				

;PRINT HEX VALUE

	PLA ;restore hex value to print
	JSR CONVERT.HEX_TO_ASCII
		LDA RESULT+$1
	JSR COUT
		LDA RESULT+$0
	JSR COUT
		LDA #$A0 ;space
	JSR COUT
				
	
;RESTORE CURSOR POSITION
		LDA CURSOR.POSITION.SAVED+$0
		STA HTAB	
		LDA CURSOR.POSITION.SAVED+$1
		STA VTAB
	JSR	UPDATE.CHAR.POS	

;RESTORE X/Y REGISTERS
	PLA
	TAY
	PLA
	TAX
	
	RTS
@END

PRINT.STR ; ======OUTPUT ASCII STRING TO VIDEO DISPLAY, NO CARRIAGE RETURN=====
@START
;PARAMETERS: ACC = ($7F = inverse characters, != $7F = normal characters), STRING (HO/LO ADDRESS OF ASCII STRING TO PRINT), PRINT.STR.MODE ($00 = normal | >=$01 = wait/pause functionality enabled), [HRCG.PAGES ($01 = hi-res page 1, $02 = hi-res page 2, $03 = both pages)], [COUT_CHAR_TYPE ;($00 = normal, $7F = inverse)]
;ENTRANCE: DIRECT, PRINT.STR.BACKGROUND
;RETURN VALUE: NONE

;NOTE: ascii string must use the LO values on the ascii chart
;in order to work with this subtroutine. SBASM uses the LO values
;when the .AZ directive is used.
;
;--LO Value Quick Reference--
;Carriage Return: $08

; ;*****DRIVER TEMPLATE (normal characters)****
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

; ;*****DRIVER TEMPLATE (inverse characters)****
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
		;LDA #$7F 	;($7F = inverse, != $7F = normal)
	; JSR PRINT.STR
	
;PROCESS PARMS
	
	;ACC = ($7F = inverse, != $7F = normal)
	STA TEMP

	LDA HRCG.PAGES ;($01 = hi-res page 1, $02 = hi-res page 2, $03 = both pages)]
	STA HRCG.PAGES.SAVED ;save the value at routine entrance because it is reset by COUT to $03 on exit

	LDA COUT_CHAR_TYPE ;($00 = normal, $7F = inverse)
	STA COUT_CHAR_TYPE.SAVED
		
	;**FALLS THROUGH**
				
.SAVE.REGISTERS
	TYA
	PHA
							
	LDY #$0				;init string index

	
.LOOP
	LDA (STRING),Y
	BEQ .EXIT			;if string stop value found, exit. SBASM adds a $00 stop value to its .AZ ascii hex tables. 

;	CLC
;	ADC #$80			;convert from LO hex ascii value (used by SBASM) to HI hex ascii value (required by COUT ROM routine)

	PHA ;save char to print
	LDA TEMP ;($7F = inverse, != $7F = normal)
	CMP #$7F
	BNE .CALL.COUT.ENTRANCE	
	STA COUT_CHAR_TYPE ;($00 = normal, $7F = inverse)


.CALL.COUT.ENTRANCE
	
.KEYPRESS.CHECK
	LDA PLAYER.MAP.LOCATION_TYPE		;load map type code
	CMP #MAP.TYPE.COMBAT		;is map type = building?
	BNE .CALL.COUT
	
	LDA PRINT.STR.MODE	;($00 = normal | >=$01 = wait/pause functionality enabled)
	BEQ .CALL.COUT

;SCOLL SPEED ADJUSTMENT	
		LDA GAME.SCROLL_SPEED
	JSR COMBAT.WAIT.LOOP

;CHECK FOR USER SCROLL PAUSE REQUEST				
	LDA KB_BUFFER
    BPL .CALL.COUT ;branch if no keypress				
 	CMP #SCROLL_SPPED.PAUSE_KEY ;ASCII = space (was space bar key pressed?)
	BEQ .PAUSE.SCROLL_TEXT
	CMP #$AB			;(+) INCREASE SCROLL SPEED
	BEQ .INCREASE.SCROLL_SPEED	
	CMP #$BD			;(=) INCREASE SCROLL SPEED
	BEQ .INCREASE.SCROLL_SPEED
	CMP #$AD			;(-) INCREASE SCROLL SPEED
	BEQ .DECREASE.SCROLL_SPEED
	CMP #$DF			;(_) INCREASE SCROLL SPEED
	BEQ .DECREASE.SCROLL_SPEED
	JMP .CLEAR.KEYPRESS
	
.PAUSE.SCROLL_TEXT
	STA KB_BUFFER_ACK               ;CLR LAST KEY			
	JSR KEYIN ;insert pause
	JMP .CLEAR.KEYPRESS
	
.INCREASE.SCROLL_SPEED
	JSR COMMAND.INCREASE.SCROLL_SPEED
	JMP .CLEAR.KEYPRESS

.DECREASE.SCROLL_SPEED
	JSR COMMAND.DECREASE.SCROLL_SPEED
	
	;**FALLS THROUGH**
	
.CLEAR.KEYPRESS
    STA KB_BUFFER_ACK               ;CLR LAST KEY
	
		;JSR KEYIN
.CALL.COUT
		;set hi-res page parameter 
		LDA HRCG.PAGES.SAVED ;($01 = hi-res page 1, $02 = hi-res page 2, $03 = both pages)
		STA HRCG.PAGES
		
		LDA COUT_CHAR_TYPE.SAVED ;($00 = normal, $7F = inverse)
		STA COUT_CHAR_TYPE 
	
		PLA ;restore char to print
	JSR COUT
			
	INY					;increment string index
	BNE .LOOP

.EXIT 

; ;RESET PARMS TO DEFAULT VALUES
	; LDA #$03 ;set both pages
	; STA HRCG.PAGES ;($01 = hi-res page 1, $02 = hi-res page 2, $03 = both pages)
 

;RESTORE REGISTERS
	PLA
	TAY
  

			
	RTS

@END

	
PRINT.STR.BACKGROUND ;**OPT** Memory. If only inventory routines use this then it can be moved to the inventory file
@START
;PARAMETERS: [COUT_CHAR_TYPE ($00 = normal, $7F = inverse)]
;ENTRANCE: DIRECT
;RETURN: text on video screen

		LDA PAGE.BACKGROUND
		STA HRCG.PAGES ;($01 = hi-res page 1, $02 = hi-res page 2, $03 = both pages)		
	JSR PRINT.STR
	
	RTS
	
@END

PRINT.STR.APPEND_RIGHT
@START
;PARAMETERS: STRING(2), PRINT.STR.APPEND_RIGHT.HTAB, PRINT.STR.APPEND_RIGHT.VTAB, PRINT.STR.APPEND_RIGHT.WINDOW_SIZE
;ENTRANCE: DIRECT
;RETURN:

;SAVE REGISTERS
	TYA
	PHA

.INIT
		LDA PRINT.STR.APPEND_RIGHT.HTAB ;left edge screen byte of the text window to print appended text
		STA HTAB	
		LDA PRINT.STR.APPEND_RIGHT.VTAB ;VTAB of the text window to print appended text
		STA VTAB
	JSR	UPDATE.CHAR.POS
	

.PREPARE.APPENDED.TEXT
	;get size of text block to print
	LDY #$FF ;start at $FF because loop counter increment is at top of loop. 
.GET.SIZE.LOOP
	INY ;increment text block index & loop counter
	LDA (STRING),Y
	BNE .GET.SIZE.LOOP
		;INY convert Y-REG value to a quantity (i.e. with floor value of 1 instead of 0) isn't necessary because the byte 
		;that triggers exit is the stop value, which is +1 byte more than the true end of the text block for video screen print purposes.
	
	;Y-REG = text block size
	STY TEMP ;save text block size

;CREATE APPENDED TEXT BLOCK
;(copy the text block submitted as a parameter until the size of the
;the text window is reached, appending the rest. If the size of the text block is reached
;first then stop copying at that point.)	
	LDY #$00
.APPEND.LOOP ;copy until append point or end of text blocked reached, whichever comes first
	LDA (STRING),Y
	STA PRINT.STR.APPEND_RIGHT.BUFFER,Y
	INY
	CPY #PRINT.STR.APPEND_RIGHT.BUFFER.SIZE-1
	BEQ .ERROR.APPEND_BUFFER.OVERFLOW
	CPY TEMP ;has size of text block been reached? 
	BEQ .APPEND.LOOP.DONE ;branch if yes
	CPY PRINT.STR.APPEND_RIGHT.WINDOW_SIZE ;has size of window been reached? 
	BNE .APPEND.LOOP ;branch if no
.APPEND.LOOP.DONE
	;add stop value to end of appended text string
	LDA #$00
	STA PRINT.STR.APPEND_RIGHT.BUFFER,Y
	
	;connect string pointer to buffer
	LDA #PRINT.STR.APPEND_RIGHT.BUFFER
	STA STRING+$0

	LDA /PRINT.STR.APPEND_RIGHT.BUFFER
	STA STRING+$1	
	
	
;PRINT TEXT BLOCK
	
		;STRING+$0: set above
		;STRING+$1: set above
	JSR PRINT.STR

.EXIT
;RESTORE REGISTERS
	PLA
	TAY
	
	RTS
	
;; .ERROR.TEXT_BLOCK_OVERFLOW
;PRINT.TW_BOTTOM reports that text block to print is larger than the size of the bottom text window?

	; JSR PREP.BRK
	; BRK

.ERROR.APPEND_BUFFER.OVERFLOW
;PRINT.STR.APPEND_RIGHT reports that the text block and/or window size passed as parms are larger than the append buffer size.
	JSR PREP.BRK
	BRK
	

@END
	
PRINT.STR.CENTERED
@START
;PARAMETERS: STRING(2), PRINT.STR.CENTERED.HTAB, PRINT.STR.CENTERED.VTAB, PRINT.STR.CENTERED.WINDOW_SIZE
;ENTRANCE: DIRECT
;RETURN:

;SAVE REGISTERS
	TYA
	PHA

;SAVE PARAMETERS
	LDA STRING+$0
	PHA ;push text block pointer LO byte from stack

	LDA STRING+$1
	PHA ;push text block pointer HO byte from stack

.INIT
		LDA PRINT.STR.CENTERED.HTAB ;left edge screen byte of the centered space.
		STA HTAB	
		LDA PRINT.STR.CENTERED.VTAB ;VTAB of the centered space. 
		STA VTAB
	JSR	UPDATE.CHAR.POS

	LDA PRINT.STR.CENTERED.WINDOW_SIZE ;# of characters in the centered space
	CLC
	ADC #$01
	STA PRINT.STR.CENTERED.WINDOW_SIZE.PLUS_1


;!!WARNING!! this clear window routine may not be appropriate
;for all scenarios where this subroutine could be used. A default resetting parm may need to be added
;to skip the clear. The clear is needed when centered text is printed
;to the same space over and over without any other routines clearning it. However
;the clear routine below is sized for the main screen bottom text window. 

	;clear text window
		LDA #GLOBAL.TEXT_BLOCK.CLEAR_BOTTOM.TW		
		STA STRING
		
		LDA /GLOBAL.TEXT_BLOCK.CLEAR_BOTTOM.TW
		STA STRING+$1						
	JSR PRINT.STR
	
	;restore parameters
	PLA ;pull text block HO byte from stack
	STA STRING+$1
	PLA ;pull text block LO byte from stack
	STA STRING+$0
		

.CALCULATE.PRINT_HTAB
	;get size of text block to print
	LDY #$FF ;start at $FF because loop counter increment is at top of loop. 
.GET.SIZE.LOOP
	INY ;increment text block index & loop counter
	LDA (STRING),Y
	BNE .GET.SIZE.LOOP
		;INY convert Y-REG value to a quantity (i.e. with floor value of 1 instead of 0) isn't necessary because the byte 
		;that triggers exit is the stop value, which is +1 byte more than the true end of the text block for video screen print purposes.
	
	;Y-REG = text block size
	STY TEMP ;save text block size
	
	;error check
	;(is text block to print larger than the size of the bottom text window?)
	CPY PRINT.STR.CENTERED.WINDOW_SIZE.PLUS_1
	BCS .ERROR.TEXT_BLOCK_OVERFLOW
	
	;calculate start position adjustment
	;(formula: start position adjustment = (text window size - text block size)/2). Round down (i.e. ignore remainder value) 
	LDA PRINT.STR.CENTERED.WINDOW_SIZE
	SEC
	SBC TEMP ;text block size
	LSR ;X2 (rounded down)
	;calculate start position			
	CLC
	ADC PRINT.STR.CENTERED.HTAB	
		
		;ACC = print start position
		STA HTAB	
		LDA PRINT.STR.CENTERED.VTAB
		STA VTAB
	JSR	UPDATE.CHAR.POS
	
	
	;print text block
	
		;STRING+$0: set above
		;STRING+$1: set above
	JSR PRINT.STR

.EXIT
;RESTORE REGISTERS
	PLA
	TAY
	
	RTS
	
.ERROR.TEXT_BLOCK_OVERFLOW
;PRINT.STR.CENTERED reports that text block to print is larger than the size of the text window

	JSR PREP.BRK
	BRK
	

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
	
	JSR GET.BSR_BANK.STATUS

;ENABLE ROM ROUTINES
	LDA $C082				;READ ENABLE ROM, DISABLE WRITE ON BSM (NORMAL STATE).
							;REQUIRED FOR BEFORE BRK BECAUSE APPLE MONITOR IS A ROM ROUTINE
 
	JSR UPDATE.CHAR.POS.ADDRESS

; ;ENABLE.BS_RAM BEFORE EXIT
	; LDA $C083				;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK 1ST)
	; LDA $C083

	JSR RESTORE.BSR_BANK.STATUS

	
	RTS

@END


;TEXT WINDOW FUNCTIONS
@START
;**OPT** Memory. I think I moved these here assuming I'd use them them in inventory. If that's true, it might still not make sense to move them since combat and NPC talk used them and
;if combat ends up in memory permentantly, keeping a local copy in combat vs. main game engine memory doesn't gain anything. 

;base print routine
PRINT.TEXT.WINDOW
@START
;PARAMETERS: various (see DRIVER TEMPLATE (INIT)), PRINT.STR.MODE ($00 = normal | >=$01 = wait/pause functionality enabled)

;=====================SUBROUTINE DOCUMENTATION====================================
;
;Parses an input string into a buffer, to facilitate word wrap functionality and
;screen scrolling within a defined text window. After any adjustments are needed to the 
;cursor position, or to the text within the window (scrolling), the buffer is output via
;PRINT.STR and ultimately COUT, which updates the HTAB/VTAB zero page addresses to track the
;cursor position. 
;
;The tracking of the cursor position could easily be done with this routine but the
;HRCG controller is setup to use COUT and I haven't spend the time to consider disconnecting them.
;The HRCG controller handles the plotting of the characters to the high-res screen, by 
;calculating a shape table address based on the ASCII value of the character sent to it. 
;
;=================================================================================

; ; ;DRIVER TEMPLATE (INIT)
; ;INIT TEXT WINDOW
; ;NOTE: HTAB/VTAB must be set before the text window is set. If they are set after, or not set at all, strange things happen. 
;
		; ;INIT TEXT WINDOW BOUNDARIES (for the text itself, excluding borders)
		; LDA #$19  ;$19full  /$1Etest 
		; STA TWF.LEFT_SBYTE
		;
		; LDA #$0D ;right edge of text is $26, border is at $27
		; STA TWF.WIDTH
		;
		; LDA #$01
		; STA TWF.TOP_ROW
		;
		; LDA #$03			
		; STA TWF.BOTTOM_ROW
;
		; ;SET CURSOR POSITION TO UPPER LEFT OF TEXT WINDOW
		; LDA #$19			;$19full / $1E test
		; STA HTAB	
		; LDA #$1
		; STA VTAB
		; JSR	UPDATE.CHAR.POS
				
; ;DRIVER TEMPLATE (PRINTING)
;
;
; ;PRINT "TEST"	
;
;		
	; JMP .TEXT1.PRINT
; .TEXT1 .AZ -/Test/,#$8D,#$8D			;ASCII text string
; .TEXT1.PRINT
		; LDA #.TEXT1 					
		; STA TWF.STRING
;		
		; LDA /.TEXT1
		; STA TWF.STRING+$1						
	; JSR PRINT.TEXT.WINDOW


			
.SAVE.REGISTERS
	TXA
	PHA
	TYA
	PHA


;INIT VARIABLES
	LDY #$00 ;init input string index
	LDX #$00 ;init buffer index
	
	STX TWF.PRINTED_LINE.COUNTER
	
	;setup remaining text window dimesions
	LDA TWF.LEFT_SBYTE		;load left edge position
	CLC
	ADC TWF.WIDTH	;add width
	STA TWF.RIGHT_SBYTE		;result is screen byte of right edge of text space
	;INC TWF.RIGHT_SBYTE		;add 1 so that it can be used as a stop value after an index increment
	
	;setup bottom row trigger for scroll prompt
	LDA TWF.BOTTOM_ROW
	STA TWF.BOTTOM_ROW.TRIGGER
	DEC TWF.BOTTOM_ROW.TRIGGER	;the trigger should be 1 row less than the bottom so that the prompt can be printed on the last row of the text window
	
.LOOP.PARSE.STRING	
	LDA (TWF.STRING),Y
	CMP #$8D ;ASCII = return key
	BEQ	.CARRIAGE.RETURN
	CMP #NPC.TEXT.BLOCK.STOP_VALUE
	BEQ .END.OF.STRING
	;**FALLS THROUGH** ;input character is something other than a space or carriage return
.ADD.TO.BUFFER 
;Add current character to the buffer
	STA TEXT.WINDOW.CHAR	;save current character parsed from TW.STRING
	CPX TWF.WIDTH			;does the buffer contain the same number of characters as the width of the text window?
	BCS .LINE.FULL			;if yes then exit loop
	
	STA TEXT.WINDOW.BUFFER,X ;if no, then save current character to buffer

	INX	;increment buffer index
	INY	;increment input string index
	BEQ	.OVERFLOW.ERROR_STEP 	;if index flip to $00, then report error
	LDA TEXT.WINDOW.CHAR	;restore current character parsed from TW.STRING
	CMP #$A0 ;ASCII = space key
	BNE .LOOP.PARSE.STRING	;if no space character found, continue loop		
	;**FALLS THROUGH** ;if space char found, then determine which line buffer should be output to
		
.OUTPUT.BUFFER
			; LDA TWF.RIGHT_SBYTE
			; CMP HTAB
			; BEQ .NEXT_LINE
			
	;WILL BUFFER FIT ON CURRENT LINE?
	STX TEXT.WINDOW.BUFFER.TALLY	;save buffer index which is also the number of characters in the buffer+1
	LDA TWF.RIGHT_SBYTE						;load text window right edge
	SEC
	SBC HTAB						;subtract cursor horizontal (x-axis) position, which = chars left in line. 
	; CLC
	; ADC #01							;converts the grid positional distnce between right edge and cursor into the number of characters left in the current line. Takes into account that even though the buffer index was increment in preperation for the stop value to be written below, TEXT.WINDOW.BUFFER.TALLY is not 1 greater than the actual number of characters in the buffer because the tally is equal to the index which started at 0.  


			
	CMP TEXT.WINDOW.BUFFER.TALLY	;is the number of chars in buffer+1 less than chars left in line?
	BCS .PRINT.STRING				;if yes, then word will fit on current line, output word via COUT
				;if no, reset cursor position to next line

	;RESET CURSOR TO START OF NEXT LINE
.NEXT_LINE
		INC VTAB	;move cursor to next row
		LDA TWF.LEFT_SBYTE	
		STA HTAB	;rest cursor X-axis to the left edge of text window
	JSR	UPDATE.CHAR.POS
	INC TWF.PRINTED_LINE.COUNTER		;increment counter to reflect that another line of text has been printed to the text window

	LDA VTAB 						;load the row number of the cursor position
	CMP TWF.BOTTOM_ROW.TRIGGER		;is the cursor position <= the bottom row-1 of the text window?
	BCC .PRINT.STRING				;output word via COUT
	BEQ .PRINT.STRING				;output word via COUT

	JSR SCROLL.TEXT.WINDOW			;if yes, the scroll the text window with prompt	
	
		;move cursor to 2nd to last row in the text window. last row is reserved for the <ANY KEY> page break prompt. 
		LDA TWF.BOTTOM_ROW.TRIGGER ;load bottom row -1
		STA VTAB
		LDA TWF.LEFT_SBYTE	;load screen byte on left edge of text space
		STA HTAB	;reset cursor X-axis to the left edge of text window
	JSR	UPDATE.CHAR.POS

	; ;reset printed line counter
	; LDA #$00
	; STA TWF.PRINTED_LINE.COUNTER
			
	JMP .PRINT.STRING				;output word via COUT

.OVERFLOW.ERROR_STEP
	JMP .OVERFLOW.ERROR
	
.CARRIAGE.RETURN
	CPX #$00			;is buffer empty?
	BEQ .PROCESS.CARRIAGE.RETURN	;if yes, then nothing to output, proceed directly to processing of carriage return

	;ACC = carrige return char from TW.STRING
	STA TEXT.WINDOW.CHAR	;save current character parsed from TW.STRING

	JMP .OUTPUT.BUFFER		;output current contents of buffer (not including carriage return character). Carriage return will get processed in .PRINT.STRING after the JSR PRINT.STR (which calls COUT)
		
.END.OF.STRING
	;ACC = stop value from TW.STRING
	STA TEXT.WINDOW.CHAR	;save current character parsed from TW.STRING, which should be the $00 stop value
	
	CPX #$00			;is buffer empty?
	BEQ .EXIT			;if yes, then exit
				;if no, then output the buffer via COUT

;FINAL BUFFER OUTPUT
	JMP .OUTPUT.BUFFER

; .EXIT_STEP
	; JMP .EXIT
	
.LINE.FULL
;The buffer has the maximum number of characters that can fit on a line.
;even though a space or end-of-string value was not encountered. Unless
;the cursor is on the left edge of the text window, the buffer needs to be output to
;the next line

	LDA TWF.LEFT_SBYTE		;load column of ledge edge of text window
	CMP HTAB		;is it less than the column that the cursor is in?
	BCC .NEXT_LINE  ;if yes, then advance to next line and output the buffer	
	;**FALLS THROUGH** ;if no, then output the buffer on the current line (cursor is on the left edge of the text window)
	
.PRINT.STRING
	;add stop value
	LDA #TWF.STRING.STOP.VALUE
	STA TEXT.WINDOW.BUFFER,X

	;setup pointer for PRINT.STRING
	LDA #TEXT.WINDOW.BUFFER
	STA STRING+$0
	
	LDA /TEXT.WINDOW.BUFFER
	STA STRING+$1
	
		;parm STRING set in init above
		;parm PRINT.STR.MODE set as parm to this routine
	JSR PRINT.STR

	LDX #$00 ;reset buffer index

		;JSR KEYIN

		
	LDA TEXT.WINDOW.CHAR		;end of string?
	BEQ .EXIT					;if yes, exit

	LDA TEXT.WINDOW.CHAR		;is current input character a carriage return? (which would not have been written to the buffer just output)
	CMP #$8D ;ASCII = return key
	BNE	.RETURN.TO.LOOP			;if no, return to the loop without further ado

.PROCESS.CARRIAGE.RETURN	
	;ADVANCE TO NEXT LINE, RESET X-AXIS (VTAB)
		INC VTAB	;move cursor to next row
		LDA TWF.LEFT_SBYTE	
		STA HTAB	;rest cursor X-axis to the left edge of text window
	JSR	UPDATE.CHAR.POS
	INC TWF.PRINTED_LINE.COUNTER		;increment counter to reflect that another line of text has been printed to the text window

	LDA VTAB 						;load the row number of the cursor position
	CMP TWF.BOTTOM_ROW.TRIGGER		;is the cursor position <= the bottom row-1 of the text window?
	BCC .INCREMENT.INDEX			;don't scroll text window
	BEQ .INCREMENT.INDEX			;don't scroll text window

	JSR SCROLL.TEXT.WINDOW			;if yes, the scroll the text window with prompt	
	
		;move cursor to 2nd to last row in the text window. last row is reserved for the <ANY KEY> page break prompt. 
		LDA TWF.BOTTOM_ROW.TRIGGER ;load bottom row -1
		STA VTAB
		LDA TWF.LEFT_SBYTE	;load screen byte on left edge of text space
		STA HTAB	;reset cursor X-axis to the left edge of text window
	JSR	UPDATE.CHAR.POS
	
.INCREMENT.INDEX	
	INY	;increment input string index
		;buffer index not incremented because we aren't adding the carriage return character to the buffer since we don't want PRINT.STRING and thus COUT to print it.
	BEQ	.OVERFLOW.ERROR_STEP 	;if index flip to $00, then report error

	;**FALLS THROUGH**

.RETURN.TO.LOOP	
	JMP .LOOP.PARSE.STRING	

.EXIT

		; JSR KEYIN
		; LDA #$AA
		; LDX #TEXT.WINDOW.BUFFER
		; LDY /TEXT.WINDOW.BUFFER
		; JSR PREP.BRK
		; BRK
		
.RESTORE.REGISTERS
	PLA
	TAY
	PLA
	TAX

	RTS

.OVERFLOW.ERROR
;PRINT.TEXT.WINDOW reports an overflow in the input array index (Y-REG)
	JSR PREP.BRK
	BRK
	
; .OVERFLOW.ERROR_STEP
		; JMP .OVERFLOW.ERROR
		
SCROLL.TEXT.WINDOW
@START

;SAVE REGISTERS	
	TXA
	PHA
	TYA
	PHA

.CHECK.PAGE.BREAK
;Note: if the input string printed since the PRINT.TEXT.WINDOW was called, or since the last scroll, is larger than the text window, generate a page break prompt before scrolling the screen so that the player has a chance to read all the text.

	LDA TWF.PRINTED_LINE.COUNTER ;load number of lines printed to the text window since the PRINT.TEXT.WINDOW was called, or since the last scroll
	CMP TWF.BOTTOM_ROW.TRIGGER   ;has the text window been filled by a contigous string? (TWF.BOTTOM_ROW.TRIGGER contains the 2nd to last row of the text window, which we check because the last row is reserved for the page break prompt)
	BNE .SCROLL				     ;if no, proceed with scroll 

.PAGE.BREAK.PROMPT
;PRINT "<any key>"

	;reset printed line counter
	LDA #$00
	STA TWF.PRINTED_LINE.COUNTER
	
	;SET CURSOR POSITION TO LOWER LEFT OF TEXT WINDOW
		LDA TWF.LEFT_SBYTE			;$19full / $1E test
		STA HTAB	
		LDA TWF.BOTTOM_ROW
		STA VTAB
	JSR	UPDATE.CHAR.POS

;PRINT "ANY KEY" PROMPT.
		LDA #TEXT_BLOCK.ANY_KEY2					
		STA STRING
		
		LDA /TEXT_BLOCK.ANY_KEY2
		STA STRING+$1
	JSR PRINT.STR	

.WAIT.FOR.KEY

	JSR KEYIN.ANIMATION.SINGLE	

.ERASE.ANY_KEY.PROMPT
	LDA TWF.BOTTOM_ROW		;load the text screen Y-AXIS (VTAB) of the bottom row
	ASL ;X 2
	ASL	;X 4
	ASL ;X 8								
	STA DRAW.START_LINE		;set starting line for the draw routine
	CLC
	ADC #TEXT.CHAR.DEPTH
	STA DRAW.STOP_LINE		;set stop line for the draw routine
	
	LDA TWF.LEFT_SBYTE
	STA DRAW.START_BYTE
	
	LDA TWF.RIGHT_SBYTE
	STA DRAW.STOP_BYTE
	
		LDA #$83		;set draw to both pages
		;STA USE.PAGE
			;we set high bit as a parameter to indicate we want the byte value for the draw to be $00
	JSR DRAW.LINE	


.SCROLL	
.INIT.SCREEN					;setup screen byte, line, depth	

	LDA TWF.TOP_ROW	;load the Y-AXIS of text cursor position
	ASL ;X 2
	ASL	;X 4
	ASL ;X 8								
	STA LINE.START.COPY_TO		;set starting line for the COPY TO part of the scroll
	CLC
	ADC #TEXT.CHAR.DEPTH
	STA LINE.START.COPY_FROM	;set starting line for the copy from part of the scroll operation
	
	LDA TWF.BOTTOM_ROW
	ASL ;X 2
	ASL	;X 4
	ASL ;X 8
	STA TWF.SCROLLING.STOP_LINE	;set the stop line, which is the last line of the last row of text characters to scroll +$08. The last row of text characters to scroll is 1 less than the bottom row. Thus, taking the bottom row # *8 gets us exactly the value we need. 
	
.ROWLOOP

	LDX LINE.START.COPY_TO		;prepare for new row of text characters: reset the current COPY TO line to the starting line
	
	LDA LINE.START.COPY_TO
	CLC
	ADC #TEXT.CHAR.DEPTH		;add depth of shape to starting line in hex (# of lines, not the position of last line....so line positions $0-$f is $10 (!16) total lines)
	STA LINE.STOP				;set the last line to draw based on the depth of the text characters
	
.LINELOOP
	
		
	LDY TWF.LEFT_SBYTE			;reset screen byte index to the left edge of text window	
	
.GET.LINE.ADDRESS1B	
	TXA						;X is the tile.line counter, and used by the get line address routine, so we need to save it to the stack
	PHA
		LDX LINE.START.COPY_FROM
		LDA PAGE.FOREGROUND
	JSR GET.LINE.ADDRESS1
	
;CALCULATE BASE ADDRESS FOR OPPOSITE PAGE	
	LDA PAGE.FOREGROUND
	CMP #$01
	BEQ .PAGE.01.FOREGROUND1
.PAGE.01.BACKGROUND1
	;CALCULATE BASE ADDRESS FOR FOREGROUND PAGE
	LDA LINE.BASE.ADDR1+$0
	STA LINE.BASE.ADDR3+$0

	LDA LINE.BASE.ADDR1+$1
	SEC
	SBC #$20
	STA LINE.BASE.ADDR3+$1
	
	JMP .LOOKUP1B.COMPLETE
	
.PAGE.01.FOREGROUND1
	;CALCULATE BASE ADDRESS FOR BACKGROUND PAGE
	LDA LINE.BASE.ADDR1+$0
	STA LINE.BASE.ADDR3+$0

	LDA LINE.BASE.ADDR1+$1
	CLC
	ADC #$20
	STA LINE.BASE.ADDR3+$1
	;**FALLS THROUGH**

.LOOKUP1B.COMPLETE
	PLA						;restore x-reg from stack. it was transfered there at the start of the get line address2 routine
	TAX
	
.GET.LINE.ADDRESS2B

		LDA PAGE.FOREGROUND
	JSR GET.LINE.ADDRESS2

;CALCULATE BASE ADDRESS FOR OPPOSITE PAGE	
	LDA PAGE.FOREGROUND
	CMP #$01
	BEQ .PAGE.01.FOREGROUND2
.PAGE.01.BACKGROUND2
	;CALCULATE BASE ADDRESS FOR FOREGROUND PAGE
	LDA LINE.BASE.ADDR2+$0
	STA LINE.BASE.ADDR4+$0

	LDA LINE.BASE.ADDR2+$1
	SEC
	SBC #$20
	STA LINE.BASE.ADDR4+$1
	
	JMP .LOOKUP2B.COMPLETE
	
.PAGE.01.FOREGROUND2
	;CALCULATE BASE ADDRESS FOR BACKGROUND PAGE
	LDA LINE.BASE.ADDR2+$0
	STA LINE.BASE.ADDR4+$0

	LDA LINE.BASE.ADDR2+$1
	CLC
	ADC #$20
	STA LINE.BASE.ADDR4+$1
	;**FALLS THROUGH**
.LOOKUP2B.COMPLETE
	
.COPYLOOP
;COPY TEXT CHARACTER DATA

	LDA (LINE.BASE.ADDR1),Y		;TEXT CHAR DATA: COPY FROM  (foreground page)	
	STA (LINE.BASE.ADDR2),Y		;TEXT CHAR DATA: COPY TO	(foreground page)

	LDA (LINE.BASE.ADDR3),Y		;TEXT CHAR DATA: COPY FROM  (background page)	
	STA (LINE.BASE.ADDR4),Y		;TEXT CHAR DATA: COPY TO	(background page)

	
	;ERASE AS WE GO 
	;Note: This is done because the next line of text printed by the text window might not take the entire line, and thus might n0t take care of erasing all of it
	LDA #$00
	STA (LINE.BASE.ADDR1),Y		;foreground page
	STA (LINE.BASE.ADDR3),Y		;background page
	
	INY								;advance to next screen byte

	CPY TWF.RIGHT_SBYTE				;is line complete? (i.e. have all screen bytes in the line been scrolled?)
	BCS .LINE.COMPLETE				;if yes, switch over to next row (bcs: is y-reg >= cpy value)
	JMP .COPYLOOP					;if no, copy next screen byte

.LINE.COMPLETE
			
	INX								;next tile line
	INC LINE.START.COPY_FROM
		
	CPX LINE.STOP					;is tile done?	
	BCC .LINELOOP					;if no, draw next line (bcc: is acc < cmp value)
	

.ROW.COMPLETE.SOUTH			
	LDA LINE.START.COPY_TO			;advance line to next row of characters
	CLC
	ADC #TEXT.CHAR.DEPTH
	STA LINE.START.COPY_TO
	CLC
	ADC #TEXT.CHAR.DEPTH
	STA LINE.START.COPY_FROM		;start line of tile 1 row down	
	CMP	TWF.SCROLLING.STOP_LINE		;are all rows scrolled?
	BCS .SCROLL.COMPLETE			;if yes, exit (BCS: is ACC >= CMP value)
	JMP .ROWLOOP

		
.SCROLL.COMPLETE

;RESTORE REGISTERS	
	PLA
	TAY
	PLA
	TAX
	
	RTS
	
@END
@END

PRINT.TEXT.WINDOW.SINGLE.CHAR ;print single char without <CR>
@START
;PARAMETERS: ACC = ASCII code of character to print
;ENTRANCE: direct
;RETURN: printed character within text window
	

		;ACC = ASCII code of character to print
		STA .TEXT_BLOCK.SPACE+$0
		
		LDA #.TEXT_BLOCK.SPACE
		STA TWF.STRING+$0
		
		LDA /.TEXT_BLOCK.SPACE
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW	

.EXIT

	RTS
	
.TEXT_BLOCK.SPACE .HS 00.00

@END

;print wrapper routines
PRINT.TEXT.WINDOW.CR
@START
;PARAMETERS: none
;ENTRANCE: direct
;RETURN: carriage return within text window
	
		LDA #.TEXT_BLOCK.CR	
		STA TWF.STRING
		
		LDA /.TEXT_BLOCK.CR
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW	

		; LDA #$8D
	; JSR COUT
	
.EXIT

	RTS
	
.TEXT_BLOCK.CR .AZ -#$8D

@END

PRINT.TEXT.WINDOW.SPACE
@START
;PARAMETERS: none
;ENTRANCE: direct
;RETURN: printed space within text window
	
		LDA #.TEXT_BLOCK.SPACE
		STA TWF.STRING
		
		LDA /.TEXT_BLOCK.SPACE
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW	

		; LDA #$8D
	; JSR COUT
	
.EXIT

	RTS
	
.TEXT_BLOCK.SPACE .AZ -#$A0

@END

PRINT.TEXT.WINDOW.BCD8 ;suppresses leading zeros. right justify. Appended Carriage Return
@START
;PARAMETERS: ACC = 8bit BCD number
;ENTRANCE: direct
;RETURN: 1-2 unpacked BCD digit printed to text window
;

;=====================SUBROUTINE DOCUMENTATION===========================================================================
;
;converts BCD8->ASCII then prints
;
;=============================================================================================================================

		;parm: ACC = 8bit BCD number
	JSR CONVERT.HEX_TO_ASCII	
		;RETURN VALUES: RESULT(2)
	
.CHAR1
		LDA RESULT+$1
		CMP #$B0
		BNE .CHAR1_PRINT
		LDA #$A0 ;ASCII - 'space'
.CHAR1_PRINT		
	JSR PRINT.TEXT.WINDOW.SINGLE.CHAR
		LDA RESULT+$0
.CHAR2
	JSR PRINT.TEXT.WINDOW.SINGLE.CHAR

			
.EXIT
			
	;print carriage return at the end of the text line. 
	JSR PRINT.TEXT.WINDOW.CR	
	;**OPT** Memory. A version of PRINT.TEXT.WINDOW.SINGLE.CHAR could be created that adds a <CR> at the end. It could be placed in main memory with the rest of the text window functions or in the combat module only 

	RTS	
	
	
@END


;PRINT.TEXT.WINDOW.HEX ;suppresses leading zeros. left or right justify options.
@START

PRINT.TEXT.WINDOW.HEX8.RJ ;suppresses leading zeros. right justify. 
@START
;PARAMETERS: ACC = 8-bit hex number, Carry Flag (CLC = don't print CR | SEC = print CR to text window)

		;convert to 16-bit hex number. 
		
		;ACC = 8-bit hex number
		STA BIN+$0
		LDA #$00
		STA BIN+$1
	
		LDA #$01 ;($00 = left justify | $01 = right justify)
	JSR PRINT.TEXT.WINDOW.HEX16.RJ
	
	RTS
@END

PRINT.TEXT.WINDOW.HEX8.LJ ;suppresses leading zeros. left justify. 
@START
;PARAMETERS: ACC = 8-bit hex number, Carry Flag (CLC = don't print CR | SEC = print CR to text window)

		;convert to 16-bit hex number. 
		
		;ACC = 8-bit hex number
		STA BIN+$0
		LDA #$00
		STA BIN+$1
	
		LDA #$00 ;($00 = left justify | $01 = right justify)
	JSR PRINT.TEXT.WINDOW.HEX16.LJ
	
	RTS
@END

PRINT.TEXT.WINDOW.HEX16.RJ ;suppresses leading zeros. right justify. 
@START
;PARAMETERS: Carry Flag (CLC = don't print CR | SEC = print CR to text window), BIN(2) = 16bit hex number

		LDA #$01 ;($00 = left justify | $01 = right justify)
	JSR PRINT.TEXT.WINDOW.HEX16
	
	RTS
@END

PRINT.TEXT.WINDOW.HEX16.LJ ;suppresses leading zeros. left justify. 
@START
;PARAMETERS: Carry Flag (CLC = don't print CR | SEC = print CR to text window), BIN(2) = 16bit hex number

		LDA #$00 ;($00 = left justify | $01 = right justify)
	JSR PRINT.TEXT.WINDOW.HEX16
	
	RTS
@END

PRINT.TEXT.WINDOW.HEX16 ;suppresses leading zeros. left or right justify options. 
@START
;PARAMETERS: ACC ($00 = left justify | $01 = right justify), Carry Flag (CLC = don't print CR | SEC = print CR to text window), BIN(2) = 16bit hex number
;ENTRANCE: direct
;RETURN: 3-6* unpacked BCD digits printed to text window
;

;=====================SUBROUTINE DOCUMENTATION===========================================================================
;
;converts 16-bit HEX->BCD24->ASCII then prints
;
;
;---orginal note---
;*currently routine is setup to print 4 digits, but
;# of digits desired could be passed via a parameters, and then
;parse the parm and JMP after each BCD subsection
;or setup this routine to print all 6 digits but don't print
;digits that are $0. However, then the question becomes whether to
;left-justify or right-justify the number 
;
;=============================================================================================================================
;
;

		
;SAVE PARAMETERES

	;PARM: ACC ($00 = left justify | $01 = right justify)
	STA PRINT.TEXT.WINDOW.HEX16.PARM.MODE
	
	BCS .SET.PRINT_CR.PARM
;.SET.DONT_PRINT_CR.PARM
	LDA #$00
	PHA ;save print_cr flag to stack ($00 = don't print CR | $01 = print CR to text window)
	JMP .SAVE.PARMS.DONE
.SET.PRINT_CR.PARM
	LDA #$01
	PHA ;save print_cr flag to stack ($00 = don't print CR | $01 = print CR to text window)
.SAVE.PARMS.DONE
	
		;parm = BIN(2)
	JSR CONVERT.HEX.16_TO_BCD.24
		;RETURN: BCD(3)
	
.SET.JUSTIFICATION.PARM ;for PRINT.BCD_PACKED
	LDA PRINT.TEXT.WINDOW.HEX16.PARM.MODE ;($00 = left justify | $01 = right justify)
	BNE .RIGHT_JUSTIFY
		CLC ;(CLC = left justify | SEC = right justify)
		JMP .EXECUTE.PRINT
.RIGHT_JUSTIFY
		SEC ;(CLC = left justify | SEC = right justify)
		;**FALLS THROUGH**
.EXECUTE.PRINT		
		;BCD(2): use value returned above
	JSR PRINT.BCD_PACKED	


	

			
; ;BCD+$1
		; LDA BCD+$1
	; JSR CONVERT.HEX_TO_ASCII	
		 ; ;RETURN = RESULT+$0 = LO nibble, RESULT+$1 = HO nibble

		; LDA RESULT+$1
	; JSR PRINT.TEXT.WINDOW.SINGLE.CHAR
		; ;JSR COUT

		; LDA RESULT+$0
	; JSR PRINT.TEXT.WINDOW.SINGLE.CHAR
		; ;JSR COUT

; ;BCD+$0
		; LDA BCD+$0
	; JSR CONVERT.HEX_TO_ASCII	
		 ; ;RETURN = RESULT+$0 = LO nibble, RESULT+$1 = HO nibble

		; LDA RESULT+$1
	; JSR PRINT.TEXT.WINDOW.SINGLE.CHAR
		; ;JSR COUT
		
		; LDA RESULT+$0
	; JSR PRINT.TEXT.WINDOW.SINGLE.CHAR
		; ;JSR COUT

;PRINT_CR?
	PLA ;pull print_cr flag to stack ($00 = don't print CR | $01 = print CR to text window)
	BEQ .EXIT
	
	;print carriage return at the end of the text line. 
	JSR PRINT.TEXT.WINDOW.CR	
	;**OPT** Memory. A version of PRINT.TEXT.WINDOW.SINGLE.CHAR could be created that adds a <CR> at the end. It could be placed in main memory with the rest of the text window functions or in the combat module only 

.EXIT

	RTS

@END
@END


;setup routines
INIT.TEXT_WINDOW.RIGHT
@START
	
		;LDA PAGE.BACKGROUND	
		LDA #$03
	JSR CLEAR.TEXT_WINDOW.RIGHT
	JSR CLEAR.TEXT_WINDOW.TOP	
	JSR CLEAR.TEXT_WINDOW.BOTTOM
	JSR DRAW.TEXT_WINDOW.RIGHT
		JSR FLIP.PAGE
		;JSR KEYIN
		

;INIT TEXT WINDOW

		;INIT TEXT WINDOW BOUNDARIES (for the text itself, excluding borders)
		LDA #$19  ;$19full  /$1Etest 
		STA TWF.LEFT_SBYTE
		
		LDA #$0D  ;right edge of text is $26, border is at $27
		STA TWF.WIDTH
		
		LDA #$01
		STA TWF.TOP_ROW
		
		LDA #$11 ;$12original	
		STA TWF.BOTTOM_ROW

		; ;draw cursor prompt

; ;PRINT "@"
		; LDA #$19			;$19full / $1E test
		; STA HTAB	
		; LDA #$14
		; STA VTAB
		; JSR	UPDATE.CHAR.POS
		
	; JMP .TEXT0.PRINT
	
; .TEXT0 .AZ -/@/			;ASCII text string
; .TEXT0.PRINT
		; LDA #.TEXT0 					
		; STA STRING
		
		; LDA /.TEXT0						
		; STA STRING+$1
	; JSR PRINT.STR	

	
	
; ;SET CURSOR IN START POSITION	
		
		; ;SET CURSOR POSITION TO UPPER LEFT OF TEXT WINDOW
		; LDA #$19			;$19full / $1E test
		; STA HTAB	
		; LDA #$11		;$12
		; STA VTAB
		; JSR	UPDATE.CHAR.POS
	
	RTS
	
@END

DRAW.TEXT_WINDOW.RIGHT
@START

;INIT VARIABLES
	LDA #$00
	STA SCREEN_BYTE.COUNTER

	LDA #$01
	STA TW.RIGHT_WINDOW.CLEAN_UP.FLAG   ;turn flag on so that DRAW.SCREEN will erase the right edge and first two bytes of the top/bottom lines of the text window border. 
										;This is needed because this is the portion of the text window border that isn't normally onscreen, AND CLEAR.TEXT_WINDOW.RIGHT only clears the text space, not the border. If it were to clear the border on both pages the foreground erase would be very noticable.
										;Also used via TW.RIGHT_WINDOW.STATUS.FLAG (.EQ) to tell the animation manager not to draw on top of the text window
	
.DRAW.SCREEN_BORDER
;Note: this is to erase any contents of the 1 line text windows at the top and bottom of the general screen

;TOP    LINE: $3000 PAGE1 (ADDR1), $5000 PAGE2 (ADDR2)
;BOTTOM LINE: $33D0 PAGE3 (ADDR3), $53D0 PAGE4 (ADDR4)

	LDY #$02			;init screen byte index

;INIT LINE BASE ADDRESSES

	;init LO bytes
	LDA #$00
	STA LINE.BASE.ADDR1
	STA LINE.BASE.ADDR2
	
	LDA #$D0
	STA LINE.BASE.ADDR3
	STA LINE.BASE.ADDR4
	
	;init HO bytes
	LDA #$30
	STA LINE.BASE.ADDR1+$1

	LDA #$50
	STA LINE.BASE.ADDR2+$1

	LDA #$33
	STA LINE.BASE.ADDR3+$1

	LDA #$53
	STA LINE.BASE.ADDR4+$1	
	
;DRAW TOP AND BOTTOM LINE OF BORDER ON BOTH HI-RES PAGES	
.SCREEN_BORDER.LOOP
	LDA #$D5
	STA (LINE.BASE.ADDR1),Y
	STA (LINE.BASE.ADDR2),Y
	STA (LINE.BASE.ADDR3),Y
	STA (LINE.BASE.ADDR4),Y
	
	INY ;move 1 screen byte right
	
	LDA #$AA
	STA (LINE.BASE.ADDR1),Y
	STA (LINE.BASE.ADDR2),Y
	STA (LINE.BASE.ADDR3),Y
	STA (LINE.BASE.ADDR4),Y
	
	INY ;move 1 screen byte right
	
	CPY #SCREEN.STOP_BYTE-$04
	BNE .SCREEN_BORDER.LOOP

	
.DRAW.TW.BORDER.TOP
		LDA #TWB.RW.NPC_TALK.TOP_LINE
		STA DRAW.START_LINE
		
		LDA #$24
		STA DRAW.START_BYTE
		
		LDA #$28
		STA DRAW.STOP_BYTE
			
		LDA #TWB.RW.NPC_TALK.TOP_LINE+$1
		STA DRAW.STOP_LINE

		LDA #$D5	
		STA DRAW.BYTE_VALUE.HORIZONTAL+$0
		
		LDA #$AA	
		STA DRAW.BYTE_VALUE.HORIZONTAL+$1
		LDA #$07		;set draw to both pages, and set the top edge flag. 
			;we set high bit as a parameter to indicate we want the byte value for the draw to be $00
	JSR DRAW.LINE
	
	
;SETUP DRAW, TEXT WINDOW BORDER (RIGHT SIDE): RIGHT EDGE
	LDY #TWB.RW.NPC_TALK.RIGHT_SBYTE
	
	LDA PAGE.BACKGROUND
	LDX #TWB.RW.NPC_TALK.TOP_LINE+$1 ;line 5
	JSR GET.LINE.ADDRESS1

	LDA PAGE.FOREGROUND
	JSR GET.LINE.ADDRESS2
	
;DRAW TEXT WINDOW BORDER (RIGHT SIDE): RIGHT EDGE
.DRAW.TW.BORDER.RIGHT
	LDA #$A0					;bit mapped byte
	STA (LINE.BASE.ADDR1),Y		;save bit mapped byte to video screen memory (foreground page)
	STA (LINE.BASE.ADDR2),Y		;save bit mapped byte to video screen memory (background page)

	INX ;next line, move down
		LDA PAGE.BACKGROUND
	JSR GET.LINE.ADDRESS1 

	LDA PAGE.FOREGROUND
	JSR GET.LINE.ADDRESS2
	
	CPX #TWB.RW.NPC_TALK.BOTTOM_LINE
	BNE .DRAW.TW.BORDER.RIGHT

	
	
;DRAW TEXT WINDOW BORDER (RIGHT SIDE): BOTTOM LINE
	LDY #TWB.RW.NPC_TALK.RIGHT_SBYTE
	LDA #$AA
	STA (LINE.BASE.ADDR1),Y		;save bit mapped byte to video screen memory (foreground page) 
	STA (LINE.BASE.ADDR2),Y		;save bit mapped byte to video screen memory (background page)
	DEY ;move 1 screen byte left
	LDA #$D5
	STA (LINE.BASE.ADDR1),Y		;save bit mapped byte to video screen memory (foreground page) 
	STA (LINE.BASE.ADDR2),Y		;save bit mapped byte to video screen memory (background page)

	DEY ;move 1 screen byte left
	LDA #$AA
	STA (LINE.BASE.ADDR1),Y		;save bit mapped byte to video screen memory (foreground page) 
	STA (LINE.BASE.ADDR2),Y		;save bit mapped byte to video screen memory (background page)
	DEY ;move 1 screen byte left
	LDA #$D5
	STA (LINE.BASE.ADDR1),Y		;save bit mapped byte to video screen memory (foreground page)  
	STA (LINE.BASE.ADDR2),Y		;save bit mapped byte to video screen memory (background page)

;SETUP DRAW, TEXT WINDOW BORDER (RIGHT SIDE): LEFT EDGE

	;Move up 1 line
	LDA LINE.BASE.ADDR1+$1
	SEC
	SBC #$04				;Subtracts $400 from the line base address.
	STA LINE.BASE.ADDR1+$1

	;Move up 1 line
	LDA LINE.BASE.ADDR2+$1
	SEC
	SBC #$04				;Subtracts $400 from the line base address.
	STA LINE.BASE.ADDR2+$1
	
	LDY #TWB.RW.NPC_TALK.LEFT_SBYTE
.DRAW.TW.BORDER.LEFT
	LDA #$84					;bit mapped byte
	STA (LINE.BASE.ADDR1),Y		;save bit mapped byte to video screen memory (foreground page)  
	STA (LINE.BASE.ADDR2),Y		;save bit mapped byte to video screen memory (background page)

	DEX ;next line, move up
		LDA PAGE.BACKGROUND
	JSR GET.LINE.ADDRESS1

		LDA PAGE.FOREGROUND
	JSR GET.LINE.ADDRESS2
	
	CPX #TWB.RW.NPC_TALK.TOP_LINE
	BNE .DRAW.TW.BORDER.LEFT
	

;DRAW TALK INPUT WINDOW BORDER (RIGHT SIDE): TOP LINE
	
.DRAW.TW.INPUT.BORDER.TOP
	LDA PAGE.BACKGROUND
	LDX #TWB.TALK_INPUT_WINDOW.TOP_LINE
	JSR GET.LINE.ADDRESS1

	LDA PAGE.FOREGROUND
	JSR GET.LINE.ADDRESS2
	
	LDY #TWB.TALK_INPUT_WINDOW.LEFT_SBYTE+$2
	LDA #$D5
.LOOP.TW.INPUT.BORDER.TOP
	LDA #$D5	
	STA (LINE.BASE.ADDR1),Y	;save to background page
	STA (LINE.BASE.ADDR2),Y ;save to foreground page
	INY ;move 1 screen byte right
	LDA #$AA
	STA (LINE.BASE.ADDR1),Y ;save to background page
	STA (LINE.BASE.ADDR2),Y ;save to foreground page

	INY ;move 1 screen byte right
	CPY #TWS.TALK_INPUT_WINDOW.RIGHT_SBYTE
	BNE .LOOP.TW.INPUT.BORDER.TOP

	; LDA #$D5
	; STA (LINE.BASE.ADDR1),Y ;save to background page
	; STA (LINE.BASE.ADDR2),Y ;save to foreground page
	; INY ;move 1 screen byte right
	; LDA #$AA
	; STA (LINE.BASE.ADDR1),Y ;save to background page
	; STA (LINE.BASE.ADDR2),Y ;save to foreground page
	
.EXIT
	RTS
	

@END

CLEAR.TEXT_WINDOW.RIGHT ;NPC.TALK scroll window
@START
;PARAMETERS:  USE.PAGE*
;RETURN: NONE
;ENTRANCE: NPC.TALK, DIRECT
;*Specifies which hi-res page the text window should be erased on (BOTH = $03)

;=====================SUBROUTINE DOCUMENTATION====================================
;
;=================================================================================

	PHA ;save USE.PAGE parameter to ACC

.INIT
	LDA #TWS.RW.NPC_TALK.TOP_ROW		;load line in x register	
	STA DRAW.START_LINE
	
	LDA #TWS.RW.NPC_TALK.LEFT_SBYTE
	STA DRAW.START_BYTE
	
	LDA #TWS.RW.NPC_TALK.RIGHT_SBYTE+$1
	STA DRAW.STOP_BYTE
		
	LDA #TWS.RW.NPC_TALK.BOTTOM_ROW+$1
	STA DRAW.STOP_LINE

.DRAW
		PLA ;restore USE.PAGE parameter to ACC
		ORA #$80	;set high bit as a parameter to indicate we want the byte value for the draw to be $00
	JSR DRAW.LINE

.EXIT
	RTS

@END

CLEAR.TW.RIGHT.DISPLAY_CHARACTER_ROSTER
@START
;PARAMETERS:  USE.PAGE*
;RETURN: NONE
;ENTRANCE: DIRECT
;*Specifies which hi-res page the text window should be erased on (BOTH = $03)

;=====================SUBROUTINE DOCUMENTATION====================================
;
;=================================================================================

	PHA ;save USE.PAGE parameter to ACC

.INIT
	LDA #TWB.RW.DISPLAY_CHARACTER_ROSTER.TOP_LINE		;load line in x register	
	STA DRAW.START_LINE
	
	LDA #TWB.RW.DISPLAY_CHARACTER_ROSTER.LEFT_SBYTE
	STA DRAW.START_BYTE
	
	LDA #TWB.RW.DISPLAY_CHARACTER_ROSTER.RIGHT_SBYTE
	STA DRAW.STOP_BYTE
		
	LDA #TWB.RW.DISPLAY_CHARACTER_ROSTER.BOTTOM_LINE
	STA DRAW.STOP_LINE

.DRAW
		PLA ;restore USE.PAGE parameter to ACC
		ORA #$80	;set high bit as a parameter to indicate we want the byte value for the draw to be $00
	JSR DRAW.LINE

.EXIT
	RTS

@END

CLEAR.TW.RIGHT_SMALL.MAIN_SCREEN
@START
;PARAMETERS:  USE.PAGE*
;RETURN: NONE
;ENTRANCE: DIRECT
;*Specifies which hi-res page the text window should be erased on (BOTH = $03)

;=====================SUBROUTINE DOCUMENTATION====================================
;
;=================================================================================

	PHA ;save USE.PAGE parameter to ACC

.INIT
	LDA #TWB.RIGHT_S_WINDOW.TOP_LINE		;load line in x register	
	STA DRAW.START_LINE
	
	LDA #TWB.RIGHT_S_WINDOW.LEFT_SBYTE
	STA DRAW.START_BYTE
	
	LDA #TWB.RIGHT_S_WINDOW.RIGHT_SBYTE
	STA DRAW.STOP_BYTE
		
	LDA #TWB.RIGHT_S_WINDOW.BOTTOM_LINE
	STA DRAW.STOP_LINE

.DRAW
		PLA ;restore USE.PAGE parameter to ACC
		ORA #$80	;set high bit as a parameter to indicate we want the byte value for the draw to be $00
	JSR DRAW.LINE

.EXIT
	RTS

@END

;CLEAR.TEXT_WINDOW.RIGHT_SMALL
@START
; ;PARAMETERES: USE.PAGE*
; ;ENTRANCE: DIRECT
; ;RETURN: NONE
; ;*which hi-res page to erase on (1 or 2, not both)

; ;**OPT** The draw/erase loops of the various text window draw/erase subroutines can probably be consolidated into a generic draw routine that takes the start & stop  line/screenbyte as parameters. The individual subroutines as they exist now would just set the parameters and JSR the generic draw function

	; STA USE.PAGE
	
; .SAVE.REGISTERS
	; TXA
	; PHA
	; TYA
	; PHA
	
; .INIT
	; LDX #TWB.RIGHT_S_WINDOW.TOP_LINE		;load line in x register	
	; LDY #TWB.RIGHT_S_WINDOW.LEFT_SBYTE	;set screen byte index in y register	

; .ERASE.LOOP
; @START

	; ; LDA USE.PAGE
	; ; CMP #$03			;clear window on both hi-res pages?
	; ; BEQ .CLEAR.BOTH.PAGES	

	; ; ;ACC = USE.PAGE
		; ;LDA PAGE.FOREGROUND
		; LDA USE.PAGE
	; JSR GET.LINE.ADDRESS1
	; JMP .ERASE.SINGLE.PAGE
	; ; ;**FALLS THROUGH**
	
; ; .CLEAR.BOTH.PAGES			
	; ; LDA #$01	;get page1 address first
	; ; JSR GET.LINE.ADDRESS1
	
	; ; ;CALCULATE PAGE2 ADDRESS 
	; ; LDA LINE.BASE.ADDR1+$1
	; ; CLC
	; ; ADC #$20
	; ; STA LINE.BASE.ADDR2+$1	;subtract $2000 from line address

	; ; ;SET LO ADDRESS
	; ; LDA LINE.BASE.ADDR1
	; ; STA LINE.BASE.ADDR2
	; ; ;**FALLS THROUGH**
	
; ;.ERASE.BOTH.PAGES
	
; ; .LOOP.ROW
	; ; LDA #$00
	
	; ; STA (LINE.BASE.ADDR1),Y	;PLOT (1st screen byte)
	; ; STA (LINE.BASE.ADDR2),Y	;PLOT (1st screen byte)
	
	; ; INY  					;next screen byte
	; ; CPY #TWB.RIGHT_S_WINDOW.RIGHT_SBYTE+$1  ;at text window edge?
	; ; BNE .LOOP.ROW

	; ; LDY #TWB.RIGHT_S_WINDOW.LEFT_SBYTE	;reset screen byte to text window edge (it's actually the start byte in this routine)			
	; ; INX						;next tile line
	
	; ; CPX #TWB.RIGHT_S_WINDOW.BOTTOM_LINE+$1		;LAST LINE?							
	; ; BCC .ERASE.LOOP			;IF NO, DRAW NEXT LINE (BCC: is X-REG < CPX value)	
	; ; JMP .EXIT
	
	
; .ERASE.SINGLE.PAGE



; .LOOP.ROW2
	; LDA #$00
	
	; STA (LINE.BASE.ADDR1),Y	;PLOT (1st screen byte)
	
	; INY  					;next screen byte
	; CPY #TWB.RIGHT_S_WINDOW.RIGHT_SBYTE+$1
	; BNE .LOOP.ROW2

	; LDY #TWB.RIGHT_S_WINDOW.LEFT_SBYTE	;reset screen byte to text window edge			;SCREEN BYTE INDEX IN Y REGISTER	
	; INX						;next line
	
	; CPX #TWB.RIGHT_S_WINDOW.BOTTOM_LINE+$1  ;last line?							
	; BCC .ERASE.LOOP			;IF NO, DRAW NEXT LINE (BCC: is X-REG < CPX value)	
	; ;**FALLS THROUGH**
	
; .EXIT
		
	; PLA
	; TAY
	; PLA
	; TAX
	
	; RTS

@END

CLEAR.TEXT_WINDOW.TOP
@START
;PARAMETERES: NONE
;ENTRANCE: DIRECT
;RETURN: NONE


.INIT
	LDA #TWB.TOP_WINDOW.TOP_LINE		;load line in x register	
	STA DRAW.START_LINE
	
	LDA #TWB.TOP_WINDOW.LEFT_SBYTE
	STA DRAW.START_BYTE
	
	LDA #TWB.TOP_WINDOW.RIGHT_SBYTE+$1
	STA DRAW.STOP_BYTE
		
	LDA #TWB.TOP_WINDOW.BOTTOM_LINE+$1
	STA DRAW.STOP_LINE

.DRAW	
		LDA #$83		;set draw to both pages
			;we set high bit as a parameter to indicate we want the byte value for the draw to be $00
		;STA USE.PAGE
	JSR DRAW.LINE
	
	RTS
	


@END

CLEAR.TEXT_WINDOW.BOTTOM
@START
;PARAMETERES: NONE
;ENTRANCE: DIRECT
;RETURN: NONE

.INIT
	LDA #TWB.BOTTOM_WINDOW.TOP_LINE		;load line in x register	
	STA DRAW.START_LINE
	
	LDA #TWB.BOTTOM_WINDOW.LEFT_SBYTE
	STA DRAW.START_BYTE
	
	LDA #TWB.BOTTOM_WINDOW.RIGHT_SBYTE+$1
	STA DRAW.STOP_BYTE
		
	LDA #TWB.BOTTOM_WINDOW.BOTTOM_LINE+$1
	STA DRAW.STOP_LINE

	
.DRAW	
		LDA #$83		;set draw to both pages
			;we set high bit as a parameter to indicate we want the byte value for the draw to be $00
		;STA USE.PAGE
	JSR DRAW.LINE
	

.EXIT
	
	RTS
@END

RESET.TEXT_WINDOW.TALK_INPUT
@START
;PARAMETERES: NONE
;ENTRANCE: DIRECT
;RETURN: NONE


.INIT
	LDA #TWS.TALK_INPUT_WINDOW.TOP_LINE		;load line in x register	
	STA DRAW.START_LINE
	
	LDA #TWS.TALK_INPUT_WINDOW.LEFT_SBYTE
	STA DRAW.START_BYTE
	
	LDA #TWS.TALK_INPUT_WINDOW.RIGHT_SBYTE+$1
	STA DRAW.STOP_BYTE
		
	LDA #TWS.TALK_INPUT_WINDOW.BOTTOM_LINE+$1
	STA DRAW.STOP_LINE
	
.DRAW	
		LDA #$83		;set draw to both pages
			;we set high bit as a parameter to indicate we want the byte value for the draw to be $00
		;STA USE.PAGE
	JSR DRAW.LINE
	
.UPDATE.OTHER.TEXT
	JSR UPDATE.VOICE_MODE.DISPLAY ;this is text normally in the input window so it needs to be output to video screen again after the clear. 

	
.EXIT
	
	RTS
@END

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

;**see "GENERAL ROUTINES (LOW LEVEL)" section in offloaded_variables.ASM
 