;************************INCLUDE FILE*****************************
;(do not assemble stand-alone. For a stand-alone version of this routine see examples/rwts2)


;LIBRARY INCLUDE TEMPLATE
;to use lirbary functions, it is easiest to just include them
;all because they share some variables. This probably could be
;avoided by creating duplicate variables using different names, 
;resulting in extra memory used. 
				; ;My libraries
				; .IN 	C:\MY_CODE\INCLUDES_LIBS\routines_graphics				
				; .IN 	C:\MY_CODE\INCLUDES_LIBS\routines_text
				; .IN 	C:\MY_CODE\INCLUDES_LIBS\routines_math
				; .IN 	C:\MY_CODE\INCLUDES_LIBS\routines_general
;				
;animation.update 	.eq $B000
;generate.debug.log	.eq $B100




APPLE_BELL	;=============RING APPLE BELL (BS RAM SUPPORT)===========
@START
;PARAMETERS: NONE
;ENTRANCE: DIRECT
;RETURN: NONE
	
.CHECK.BS.STATUS
	LDA $C012
	CMP #$80				;IS BANK SWITCHED RAM ENABLED?
	BCC .RING_BELL			;IF NO, GO RIGHT TO EXIT. 
	LDA $C082				;IF YES, DISABLE BANK-SWITCHED RAM AND RENABLE ROM ROUTINES. OTHERWISE THE PROGRAM CAN'T REPORT THE ERROR BY RETURNING TO THE APPLE MONITOR SINCE THE MONITOR IS A ROM ROUTINE. 
	
.RING_BELL	
	JSR APPLE_BELL.ADDRESS
	
;ENABLE.BS_RAM BEFORE EXIT
	LDA $C083				;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK 1ST)
	LDA $C083
	RTS
	
@END

AUX.MOVE.TEMPLATE
@START
;
;NOT REALLY MUCH POINT TO AN ACTUALY SUBROUTINE, BUT THE TEMPLATE IS HANDY. AND THE VARIABLES
;DEFINITIONS ARE AT THE BOTTOM OF THIS FILE. 

;===TEMPLATE #1===
;MAIN MEMORY -> AUX MEMORY	
		; LDA #$00			;SET START ADDRESS
		; STA AUX_MOVE.START
		; LDA #$30
		; STA AUX_MOVE.START+$1
		;
		; LDA #$FF			;SET END ADDRESS
		; STA AUX_MOVE.END
		; LDA #$90
		; STA AUX_MOVE.END+$1
		;
		; LDA #$00			;SET DESTINATION ADDRESS
		; STA AUX_MOVE.DEST
		; LDA #$02
		; STA AUX_MOVE.DEST+$1
		; SEC                ;SET CARRY FLAG DESGINATD MOVE FROM MAIN MEMORY -> AUX
		; JSR AUX_MOVE
		
		
;===TEMPLATE #2===
;AUX MEMORY -> MAIN MEMORY 	
	; LDA #$00			;SET START ADDRESS
	; STA AUX_MOVE.START
	; LDA #$02		
	; STA AUX_MOVE.START+$1
	;
	; LDA #$FF			;SET END ADDRESS
	; STA AUX_MOVE.END
	; LDA #$60
	; STA AUX_MOVE.END+$1
	;
	; LDA #$00			;SET DESTINATION ADDRESS
	; STA AUX_MOVE.DEST
	; LDA #$30
	; STA AUX_MOVE.DEST+$1
	; CLC					;SET CARRY FLAG DESGINATD MOVE FROM AUX MEMORY -> MAIN
	; JSR AUX_MOVE
	
@END

KEYIN ;============WAITS FOR A KEYPRESS, NO ANIMATION=======
@START
;PARAMETERS: NONE
;RETURN VALUE: ACC (LAST KEY PRESS)
;ENTRACE: DIRECT

.KEYIN
	LDA KB_BUFFER
    BPL .KEYIN
    STA KB_BUFFER_ACK               ;CLR LAST KEY
	RTS
@END

KEYIN.ANIMATION ;===WAITS FOR A KEYPRESS, ANIMATION ACTIVE=======
@START
;PARAMETERS: NONE
;RETURN VALUE: ACC (LAST KEY PRESS)
;ENTRACE: DIRECT

.KEYIN						

		;ENABLE BANK-SWITCHED RAM ($D000 Bank 2)
		LDA $C08B
		LDA $C08B
	JSR ANIMATION.UPDATE	;continue updating animation while waiting for keypress
		;RE-ENABLE BANK-SWITCHED RAM ($D000 Bank 2)
		LDA $C083				;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK2 2nd)
		LDA $C083
		
	LDA KB_BUFFER
    BPL .KEYIN				;loop until key is pressed, depositing its ascii value in the keyboard buffer
    STA KB_BUFFER_ACK       ;clr last key
	
	RTS
@END
	

KEYIN.BCD ;============WAITS FOR A BCD (!0-9) KEYPRESS=======
@START
;PARAMETERS; ACC (1-4, # OF BCD digits to input)
;RETURN: RESULT(2)

;=====================SUBROUTINE DOCUMENTATION====================================
;
;Capture up to 4 digits of BCD input from keyboard.
;Only accept !0-!9 as input, which are ASCII $B0-$B9.
;Store the data packed in RESULT+0, RESULT+$1
;
;=================================================================================




	
;PARSE PARAMETERS
	CMP #$05
	BCC .TEST1
	JMP .ERROR
	
.TEST1
	BNE .START
	
	JMP .ERROR				;return error if ACC parm isn't 1-4.

.START	
	TAX						;init loop counter; # of BCD digits to input

;SAVE REGISTERS	
	TXA
	PHA
	TYA
	PHA
	
	
;INIT INDEXES
	LDY #$00				;init RESULT index
	
.LOOP
.KEYIN						;wait for keypress

		;ENABLE BANK-SWITCHED RAM ($D000 Bank 2)
		LDA $C08B
		LDA $C08B
	JSR ANIMATION.UPDATE	;continue updating animation while waiting for keypress
		;RE-ENABLE BANK-SWITCHED RAM ($D000 Bank 2)
		LDA $C083				;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK2 2nd)
		LDA $C083
		
	LDA KB_BUFFER
    BPL .KEYIN				;loop until key is pressed, depositing its ascii value in the keyboard buffer
    STA KB_BUFFER_ACK       ;clr last key
	CMP #$88
	BEQ .BACKSPACE.PRESSED	;if backspace is pressed, erase character in left adjacent column, move cursor 1 column left, and restart loop. 

				; STX TEMP16
				; CMP #$B0
				; BNE .TEMP
				; JSR PREP.BRK
				; LDX TEMP16
				; ; LDX RESULT
				; ; LDY RESULT+$1
				; BRK
; .TEMP


	; JMP .CONTINUE

	
; .BACKSPACE.PRESSED_STEP
	; JMP .BACKSPACE.PRESSED


				
			
.CONTINUE
				
	STA TEMP				;store key capture from buffer
	LSR						;shift the hi order nibble into the lo order nibble so we can do a compare on it.
	LSR
	LSR
	LSR
	CMP #$0B				;is the hi order nibble of the ASCII code $B?
	BNE .LOOP				;if no, continue loop because the key pressed was not !0-!9
	
	LDA TEMP				;restore key captured from buffer
	AND #$F					;mask out hi-order nibble. By masking out the hi order nibble we eliminate the B in the ASCII code and are left with 0-9 in the lo order nibble, which is easy to do a comparison on.
	CMP #$0A				;is the lo-order nibble < #$0A
	BCS .LOOP				;if no, continue loop because the key pressed was not !0-!9

	LDA TEMP
	STA BCD,Y
	JSR COUT

	
	INY						;increment RESULT index	
	DEX						;decrement loop counter
	BEQ	.PACK.BCD.VALUES	;if counter (# of BCD digits to input) = $00 then exit loop
	JMP .LOOP				;if not, continue loop and collect more input
			
.PACK.BCD.VALUES	


				
;CONVERT BCD DIGITS TO ASCII (byte0)
	LDA BCD+$1			;2nd digit
	AND #$F				;mask out the higher order nibble
	STA RESULT
	LDA BCD				;1st digit
	AND #$F				;mask out the higher order nibble			
;PACK BCD & BCD+$1 INTO ONE BYTE (RESULT)
;(see UAL book page# 19-21)
	PHA
	LDA RESULT
	AND #$8F
	STA RESULT
	PLA
	ASL
	ASL
	ASL
	ASL
	ORA RESULT
	STA RESULT

		
;CONVERT BCD DIGITS TO ASCII (byte1)
	LDA BCD+$3			;4th digit
	AND #$F				;mask out the higher order nibble
	STA RESULT+$1
	LDA BCD+$2			;3rd digit
	AND #$F				;mask out the higher order nibble			
;PACK BCD+$2 & BCD+$3 INTO ONE BYTE (RESULT+$1)
;(see UAL book page# 19-21)
	PHA
	LDA RESULT+$1
	AND #$8F
	STA RESULT+$1
	PLA
	ASL
	ASL
	ASL
	ASL
	ORA RESULT+$1
	STA RESULT+$1		
	;**FALLS THROUGH**


	
.EXIT	
;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX	
	
	RTS

.BACKSPACE.PRESSED	;if backspace is pressed, erase character in left adjacent column, move cursor 1 column left, and restart loop. 
	DEC HTAB
	JSR UPDATE.CHAR.POS

;UPDATE INDEX/COUNTERS TO REFLECT BACKSPACE	
	INX						;add one back to the BCD digits counter
	DEY						;subtract one from the RESULT index
	
	LDA #$A0						;ASCII CODE: space
	JSR COUT

	DEC HTAB
	JSR UPDATE.CHAR.POS
	
	JMP .LOOP	
	
	
.ERROR
;Unexpected value in parameter ACC, # of BCD digits to input,
;in subroutine KEYIN.BCD
	LDA TEXT
	BRK
	
@END

MEMORY.COPY ;============FILL MEMORY WITH SPECIFIED VALUE=======
@START
;PARAMTERS; COPY.TO(2), COPY.FROM(2), COPY.FROM_END(2)
;RETURN: NONE
;ENTRANCE: DIRECT

;DRIVER TEMPLATE	
		; LDA #$00
		; STA COPY.FROM_START
		; LDA #$70
		; STA COPY.FROM_START+$1
		; LDA #$00
		; STA COPY.FROM_END
		; LDA #$75
		; STA COPY.FROM_END+$1
		
		; LDA #$00
		; STA COPY.TO
		; LDA #$80
		; STA COPY.TO+$1
		
		; JSR MEMORY.COPY


		
.START

;SAVE REGISTERS	
	TYA
	PHA

	LDY #$00				;LO BYTE COUNTER
;	COPY.FROM_START+$1		;DOUBLES AS HO BYTE COUNTER
	
.LOOP					
	LDA (COPY.FROM_START),Y
	STA (COPY.TO),Y
	LDA COPY.FROM_START+$1	
	CMP COPY.FROM_END+$1	;HAS HO BYTE COUNTER (COPY.FROM_START+$1) COUNTER REACHED FILL_END HO BYTE?
	BEQ .EXIT.TEST			;IF YES, CHECK TO SEE IF FILL_END LO BYTE HAS BEEN REACHED
.INCREMENT.COUNTER.LO_BYTE
	INY						;IF NO, INCREMENT LO BYTE COUNTER
	BNE .LOOP				;IF Y-REG HASN'T FLIPPED TO $00, CONTINUE LOOP
.INCREMENT.COUNTER.HO_BYTE		
	INC COPY.FROM_START+$1	;FILL_START+$1 DOUBLES AS THE HO_BYTE COUNTER
	INC COPY.TO+$1			
	JMP .LOOP
	
.EXIT.TEST
	TYA
	CLC
	ADC COPY.FROM_START		;THE LO BYTE START VALUE + Y-REG (COUNTER FOR LO BYTE) IS WHAT WE NEED TO COMPARE TO COPY.FROM.END
	CMP	COPY.FROM_END		;DOES Y-REG (LO_BYTE COUNTER) == COPY.FROM_END (THE LO BYTE OF END ADDRESS)?
	BNE .INCREMENT.COUNTER.LO_BYTE	;IF NO, INCREMENT COUNTER AND CONTINUE LOOP	
.COPY_DONE					;IF YES, THEN COPY IS DONE. 

;RESTORE REGISTERS	
	PLA
	TAY
	
	RTS
	
@END
	
MEMORY.FILL ;============FILL MEMORY WITH SPECIFIED VALUE=======	
@START
;PARAMTERS; FILL.START(2), FILL.END(2), FILL.VALUE(1)
;RETURN: NONE
;ENTRANCE: DIRECT

;DRIVER TEMPLATE
		; LDA #$00
		; STA FILL.START
		; LDA #$70
		; STA FILL.START+$1
		; LDA #$00
		; STA FILL.END
		; LDA #$75
		; STA FILL.END+$1
		
		; LDA #$AA
		; STA FILL.VALUE
		
		; JSR MEMORY.FILL

		
.START

;RESTORE REGISTERS	
	TYA
	PHA
	
	LDY #$00			;LO BYTE COUNTER
;	FILL.START+$1		;DOUBLES AS HO BYTE COUNTER
.LOOP					
	LDA FILL.VALUE
	STA (FILL.START),Y
	LDA FILL.START+$1	
	CMP FILL.END+$1		;HAS HO BYTE COUNTER (FILL.START+$1) COUNTER REACHED FILL_END HO BYTE?
	BEQ .EXIT.TEST		;IF YES, CHECK TO SEE IF FILL_END LO BYTE HAS BEEN REACHED
.INCREMENT.COUNTER.LO_BYTE
	INY					;IF NO, INCREMENT LO BYTE COUNTER
	BNE .LOOP			;IF Y-REG HASN'T FLIPPED TO $00, CONTINUE LOOP
.INCREMENT.COUNTER.HO_BYTE		
	INC FILL.START+$1	;FILL_START+$1 DOUBLES AS THE HO_BYTE COUNTER
	JMP .LOOP
	
.EXIT.TEST
	TYA
	CLC
	ADC FILL.START		;THE LO BYTE START VALUE + Y-REG (COUNTER FOR LO BYTE) IS WHAT WE NEED TO COMPARE TO FILL.END
	CMP	FILL.END		;DOES Y-REG (LO_BYTE COUNTER) == FILL_END (THE LO BYTE OF END ADDRESS)?
	BNE .INCREMENT.COUNTER.LO_BYTE	;IF NO, INCREMENT COUNTER AND CONTINUE LOOP	
.FILL_DONE				;IF YES, THEN FILL IS DONE. 

;SAVE REGISTERS	
	PLA
	TAY
	
	RTS
@END


FULL.BRK					;PREP.BREAK BUT ACTUALLY BREAKS. USE WHEN BREAKING FROM BSM ROUTINES===
@START
;PARAMETERS: ACC (calling routine ID code)
;ENTRANCE: DIRECT
;RETURN:NONE

;Description: JMP to this routine instead of using BRK when
;troubleshooting routines in BSM, becaue this routine is in main
;memory. 
;This routine takes care of disabling BSM and renabling ROM. 
;It also enables text mode and clears the text screen. 
;
;Use the following table to identify the calling routine

;Calling Routine ID Codes (ACC)
;$A1 = ERROR1 (map_objects.manager.ASM)
;$A2 = .ERROR.PATH_NOT_FOUND (map_objects.manager.ASM)
;$A3 = .ERROR.BAD.ANCHOR_MOVEMENT_FLAG (map_objects.manager.ASM)
;$A4 = .ERROR.BOTH.AXIS.EQUAL (map_objects.manager.ASM)
;$A5 = ERROR2 (animation_manager.asm)
;$A6 = .OTHER.TERRAIN (animation_manager.asm)
;$A7 = .ERROR (graphics_scrolling.ASM)
;OR, use this code just before calling FULL.BRK to capture
;the last program counter address before FULL.BRK was called. 
;and store it in the X (HO), and Y (LO) registers.
;
; LDA *
; LDA *-2
; LDX *-1
; TAY


		
;SAVE REGISTERS	
	PHA
	TXA
	PHA
	TYA
	PHA
	
	LDA TEXT
	
;DISABLE.BS_RAM
	LDA $C082				;READ ENABLE ROM, DISABLE WRITE ON BSR (NORMAL STATE).
							;REQUIRED FOR BEFORE BRK BECAUSE APPLE MONITOR IS A ROM ROUTINE
	JSR CLEAR.TEXT.SCREEN
		
	JSR HCG.OFF				;disconnect HRCG driver, return COUT vector to normal value	
		
	JSR GENERATE.DEBUG.LOG

		; LDA $C082
		; LDA #$AA
		; BRK
		
.EXIT	
;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX	
	
	;LDA $C082	
	LDA $2000
	PLA
	BRK
	
			;RTS
@END

	
PREP.BRK					;MANAGED BREAK; PRESERVES REGISTER VALUES UPON ENTRANCE
@START
;PARAMETERS: NONE
;ENTRANCE: DIRECT
;RETURN:NONE

;Description: JMP to this routine instead of using BRK. This routine
;takes care of disabling BSM and renabling ROM. It also enables
;Text mode and clears the text screen. 
;
;All registers upon entrance are preserved and restored just 
;before the final BRK so that the monitor will display the 
;contents of the registers as they existed just before the 
;JMP to this routine. Very useful for troubleshooting routines
;running in BSM.
;


;SAVE REGISTERS	
	PHA
	TXA
	PHA
	TYA
	PHA
	
	LDA TEXT
	
;DISABLE.BS_RAM
	LDA $C082				;READ ENABLE ROM, DISABLE WRITE ON BSM (NORMAL STATE).
							;REQUIRED FOR BEFORE BRK BECAUSE APPLE MONITOR IS A ROM ROUTINE
	JSR CLEAR.TEXT.SCREEN

	JSR HCG.OFF				;disconnect HRCG driver, return COUT vector to normal value	

	JSR GENERATE.DEBUG.LOG
.EXIT	
;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX	
	PLA
	
	RTS
@END


WAIT ;CREATES A CPU DELAY
@START
;PARAMETERS: ACC (length of delay)
	
;DISABLE BSR / ENABLE ROM	
	LDA $C082
	
	JSR WAIT.ADDRESS

;ENABLE BANK-SWITCHED RAM ($D000 Bank 1)
	LDA $C08B					;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK1, 1ST)
	LDA $C08B
	
	
	RTS
@END	
;========DEFINE VARIABLES=========

WAIT.ADDRESS		.EQ	$FCA8
DELAY				.EQ	$FF				;#CONSTANT
APPLE_BELL.ADDRESS	.EQ $FF3A			

KB_BUFFER			.EQ	$C000
KB_BUFFER_ACK		.EQ $C010

AUX_MOVE		.EQ $C311
AUX_MOVE.START	.EQ $3C
AUX_MOVE.END	.EQ $3E
AUX_MOVE.DEST	.EQ	$42



COPY.TO				.EQ $FA				;2byt	
COPY.FROM_START		.EQ $FC				;2byt
COPY.FROM_END		.EQ $E0



FILL.START	.EQ	$EB			;START ADDRESS TO FILL
FILL.END	.BS $2			;END ADDRESS TO FILL
FILL.VALUE 	.BS $1			;VALUE TO FILL WITH
		

SAVED.YREG.GLOBAL1						.BS $01		;1byt
SAVED.YREG.GLOBAL2						.BS $01		;1byt
SAVED.YREG.LOCAL						.BS $01		;1byt
SAVED.YREG.LOCAL1						.BS $01		;1byt
;SAVED.XREG.LOCAL2						.BS $01		;1byt
SAVED.XREG.GLOBAL1						.BS $01		;1byt
SAVED.XREG.LOCAL						.BS $01		;1byt
SAVED.XREG.LOCAL1 						.BS $01		;1byt
SAVED.ACC.GLOBAL1						.BS $01
SAVED.ACC.LOCAL							.BS $01
SAVED.ACC.LOCAL2						.BS $01
TEMP									.BS $01		;1byt

TEMP16									.BS $02		;1byt