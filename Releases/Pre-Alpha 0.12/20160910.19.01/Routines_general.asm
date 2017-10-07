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

ARRAY.SEARCH16.V1 ;======SEARCHES 16-BIT ARRAY FOR MULTI-CHARACTER KEYWORD===== (UP TO 256)
@START
;*****ORIGINALLY DESIGNED AS PART OF NTALK; MAY NEED SOME ADJUSTMENTS TO USE AS SUBROUTINE. 

; ;PARMATERS: NPC.TALK.ARRAY.POINTER(2) = index to array start position, NTALK.SEARCH.MATCH_QTY = # of chars required for a match, NTALK.ARRAY.STOP.VALUE
; ;RETURN: ACC ($00 = keyword found, $01 = keyword not found), NPC.TALK.ARRAY.POINTER(2) (index to the next element to be searched (i.e. the last elemenet to be searched +$1)), YREG = $00 (always reset to $00 since pointer contains the exact index to the last element searched +$1. Other code in the calling routines expects Y-REG to be $00 upon return)
; ;ENTRANCE: NTALK.SEARCH.LOOP
; ;*If keyword is not found, NPC.TALK.ARRAY.POINTER will be an index to the last charater searched, the +$1 only applies if the keyword is found, which occurs because an additional increment occurs to iterate the keyword stop value. 

; ;=====================SUBROUTINE DOCUMENTATION====================================
; ;
; ;****V1 & V2 difference***
; ;V1 requires that the keyword search string be loaded into NTALK.KEYWORD.ARRAY, whereas V2 will 
; ;use the keyword search string array that is specified by the NTALK.KEYWORD.POINTER parameter. Thus,
; ;V1 is somewhat faster, and V2 is more flexible. 
; ;
; ;Searches the 16-bit array specified by parameter NPC.TALK.ARRAY.POINTER for the keyword specified by the parameter array NPC.TALK.KEYWORD(x),
; ;with both the keyword and array values being converted to uppercase if letters.
; ;The value in the parameter NTALK.SEARCH.MATCH_QTY is the number of characteres that must match for the keyword to be considered found. 
; ;If the keyword in the search array is shorter than the keyword in NPC.TALK.KEYWORD, then the extra characters in NPC.TALK.KEYWORD are discarded for purposes of considering a match.
; ;
; ;If NTALK.ARRAY.STOP.VALUE is found, then the keyword wasn't found. NTALK.ARRAY.STOP.VALUE is a paramter
; ;set by the calling routine. After a primary command if found, NPC.SEARCH sets this value as being the NPC Record stop value. NPC.SEARCH.LOOP sets
; ;it to the NTALK command end code when doing seaches to determine which, if any, subcommands are present.
; ;
; ;
; ;Return value (ACC) = $00 if keyword found, $01 if keyword not found
; ;Return value (NPC.TALK.ARRAY.POINTER)
; ;		keyword found = the memory address of the last character of the keyword found + $1
; ;		keyword not found = NPC.TALK.ARRAY.POINTER will be an index to the last charater searched, the +$1 only applies if the keyword is found, which occurs because an additional increment occurs to iterate the keyword stop value. 
; ;
; ;In all cases Y-REG is rest to #$00 upon exit of this routine as it is closed out to the LO byte of NPC.TALK.ARRAY.POINTER
; ;
; ;=================================================================================
	
; .SEARCH.INIT
	; LDY #$00					;Init NPC.TALK.ARRAY.POINTER index. Y_REG must be init to $00 or it will cause a block of addresses to be skipped if the lo byte of the pointer is > $00
	; LDX #$00 			;init keyword index

	
	
	; ; STA TEMP
			; ; LDA TROUBLESHOOTING.HOOK
			; ; CMP #$02	;normally $01
			; ; BNE .TEMP3
			; ; JSR PREP.BRK
			; ; LDA #$BD
			; ; ;LDA (NPC.TALK.ARRAY.POINTER),Y
			; ; LDX #NPC.TALK.KEYWORD
			; ; LDY /NPC.TALK.KEYWORD
			; ; ;LDX NPC.TALK.ARRAY.POINTER+$1
			; ; BRK
; ; .TEMP3
			; ; LDA TEMP
			
; .KEYWORD.SEARCH.LOOP
; ;Note: search for the first character of the keyword. Once found, pass control to .FIRST.CHAR.FOUND takes over and continues the search for the rest of the keyword. If the remainder of the keyword is not found, control will be passed back to this loop to continue searching for the first character
; ;NTALK.ARRAY.STOP.VALUE is the stop value specified by the calling routine. It might be the stop value indicating the end of the array or it might be a stop value within the array, such as for indicating the end of a record.
			
	; LDA (NPC.TALK.ARRAY.POINTER),Y	;load next character from talk array
	; CMP NPC.TALK.KEYWORD+$0		;does it match the first character of the keyword? 
	; BEQ .FIRST.CHAR.FOUND			;if yes, then branch to the first character found routine to look for the rest of the keyword characters in sequence
	; CMP NTALK.ARRAY.STOP.VALUE		;is array character the array stop value specified by the calling routine?
	; BEQ .ARRAY.STOP_VALUE.FOUND		;if yes, keyword was not found.
; .NEXT.ARRAY.ELEMENT
	; INY	;increment LO byte index
	; BNE .KEYWORD.SEARCH.LOOP		;did lo byte index flip over to $00? If no, continue the loop
	; INC NPC.TALK.ARRAY.POINTER+$1	;if yes, increment HO byte index before continuing the loop
	; BNE .KEYWORD.SEARCH.LOOP
	; JMP .ERROR.ARRAY.OVERFLOW		;ho byte of pointer flipped over to $00. A stop value should have been found, something has gone wrong, report error.

; .FIRST.CHAR.FOUND
	; ;save index and pointer base address 
	; STY	SAVED.YREG.LOCAL ;saved array pointer index
	
	; LDA NPC.TALK.ARRAY.POINTER+$0
	; STA NPC.TALK.ARRAY.POINTER.LO.SAVED
	
	; LDA NPC.TALK.ARRAY.POINTER+$1
	; STA NPC.TALK.ARRAY.POINTER.HO.SAVED
	
; .SEARCH.REMAINDER.LOOP	

; .INCREMENT.INDEX	
	; INX ;next keyword element

	; ;next array element
	; INY	;increment LO byte index
	; BNE .INCREMENT.COMPLETE2		;did lo byte index flip over to $00? If no, continue the loop



	; ;STY NPC.TALK.ARRAY.POINTER+$0	;even though Y-REG flipped, we must write that value to the lo byte of the pointer. Otherwise, if the lo byte of the pointer wasn't $00, the pointer position would be off. For example, if upon entrance to this routine the pointer was $BB51, and Y-REG flipped to $00 and only the HO byte of the pointer was incremented, then the pointer would be $BC51, with the Y-REG index of $00, making the next byte read $BC51.....$BC00 thru $BC50 would get skipped.  
	; INC NPC.TALK.ARRAY.POINTER+$1	;if yes, increment HO byte index before continuing the loop
	; BEQ .ERROR.ARRAY.OVERFLOW		;ho byte of pointer flipped over to $00. A stop value should have been found, something has gone wrong, report error.
; .INCREMENT.COMPLETE2

; .CHECK.MATCH.CHARS
	; CPX NTALK.SEARCH.MATCH_QTY
	; BEQ .KEYWORD.FOUND
	


			; ; STA TEMP
			; ; LDA TROUBLESHOOTING.HOOK
			; ; CMP #$02	;normally $01
			; ; BNE .TEMP4
			; ; ; LDA NPC.TALK.ARRAY.POINTER+$1
			; ; ; CMP #$BC
			; ; ; BNE .TEMP4
			; ; JSR PREP.BRK
			; ; LDA #$BE
			; ; ; LDX #$BE
			; ; ; LDY #$EE
			; ; ;LDX #NPC.TALK.KEYWORD
			; ; ;LDY /NPC.TALK.KEYWORD
			; ; ;LDX NPC.TALK.ARRAY.POINTER+$1
			; ; BRK
; ; .TEMP4
			; ; LDA TEMP

			
; .CONVERT.UPPER_CASE
	; LDA NPC.TALK.KEYWORD,X ;load next keyword character
	; CMP #$E1			;is ASCII value less than the lower end of the lowercase letter range?
	; BCC .NOT.A.LETTER1	;if yes then don't convert value to uppercase as that will damage the value, or if the value is already uppercase then no modification to the value is needed.
	; CMP #$FB			;is ASCII value greater than the upper end of the lowercase letter range?	
	; BCS .NOT.A.LETTER1	;if yes then don't convert value to uppercase as that will damage the value, or if the value is already uppercase then no modification to the value is needed.
	; AND #$DF ;masks out the 5th bit because the binary value of #$DF has 1 in each bit position except for bit 5.
; .NOT.A.LETTER1
	; STA NTALK.KEYWORD.UCASE ;must be after the labels non-letter values are saved to the variable
	; LDA (NPC.TALK.ARRAY.POINTER),Y	;load next array character
	; CMP #$E1			;is ASCII value less than the lower end of the lowercase letter range?
	; BCC .NOT.A.LETTER2	;if yes then don't convert value to uppercase as that will damage the value, or if the value is already uppercase then no modification to the value is needed.
	; CMP #$FB			;is ASCII value greater than the upper end of the lowecase letter range?	
	; BCS .NOT.A.LETTER2	;if yes then don't convert value to uppercase as that will damage the value, or if the value is already uppercase then no modification to the value is needed.
	; AND #$DF ;masks out the 5th bit because the binary value of #$DF has 1 in each bit position except for bit 5.
; .NOT.A.LETTER2
	; STA NTALK.ARRAY.UCASE ;must be after the labels non-letter values are saved to the variable
		
; .COMPARE.CHARACTERS			
	
	; LDA NTALK.ARRAY.UCASE 			;reload next array character
	; CMP #NTALK.COMMAND.SUBCOMMAND_CODE	;is next array character the subcommand code?
	; BEQ	.KEYWORD.FOUND					;if yes, then the array keyword is shorter than the players input keyword, and if this loop is still running it means all the characters of the array keyword match the player's keyword if the extra characters are discarded. This is considered a match. For example, if the player enters "JOBS" it will match on "JOB"
	; CMP #NTALK.COMMAND.END_CODE			;is next array character the command end code?
	; BEQ	.KEYWORD.FOUND					;if yes, then the array keyword is shorter than the players input keyword, and if this loop is still running it means all the characters of the array keyword match the player's keyword if the extra characters are discarded. This is considered a match. For example, if the player enters "JOBS" it will match on "JOB"	

	; LDA NTALK.KEYWORD.UCASE			;reload keyword character
	; CMP #NTALK.KEYWORD.STOP_VALUE	;is it the keyword stop value?
	; BEQ .KEYWORD.STOP_VALUE.FOUND	;if yes, then keyword might be found, more checks are needed

	; LDA NTALK.ARRAY.UCASE 	
	; CMP NTALK.KEYWORD.UCASE 		;is it equal to the next keyword character?
	; BNE	.REMAINDER_NOT.FOUND		;if no, then the keyword was not found in this sequence. The first character found was a red herring. Or, also possible, it may have been a blue hippopotamus. 

	; JMP .SEARCH.REMAINDER.LOOP

; .KEYWORD.STOP_VALUE.FOUND
; ;This routine would not be entered unless the player keyword was less than the minimum chars required to find a match except in
; ;certain internal searches such as searching for the command end code. 
; ;Thus, if the interal search flag isn't set, we can concluded that the player keyword didn't match because it was too short (less than the minimum characters required for a match).

	; LDA NTALK.FLAG.INTERNAL_SEARCH 
	; CMP #$01	;is this an internal search?
	; BEQ .KEYWORD.FOUND	;if yes, then this is a match because all characters in NPC.TALK.KEYWORD matched with a series of sequential characters in the search array
	; ;**FALLS THROUGH**
	
; .REMAINDER_NOT.FOUND
	; LDX #$00 ;reset keyword index
	
	; ;restore index and pointer base address to continue search as of the talk array element after the one containing the first character of the keyword which turned out to be a red herring
	; LDY	SAVED.YREG.LOCAL ;restore array pointer index

	; LDA NPC.TALK.ARRAY.POINTER.LO.SAVED
	; STA NPC.TALK.ARRAY.POINTER+$0

	; LDA NPC.TALK.ARRAY.POINTER.HO.SAVED
	; STA NPC.TALK.ARRAY.POINTER+$1
	; JMP .NEXT.ARRAY.ELEMENT	;enter loop here so that the talk array element is incremented before proceeding with the search. It would have ended on the red herring element. 		
	
; .KEYWORD.FOUND

			; ; STA TEMP
			; ; LDA TROUBLESHOOTING.HOOK
			; ; CMP #$02
			; ; BNE .TEMP3
			; ; JSR PREP.BRK
			; ; LDX #$BC		
			; ; BRK
; ; .TEMP3
			; ; LDA TEMP
			
			
	; LDA #$00	;load return value
	; JMP .EXIT

; ; .SET.RETURN.VALUES	
	; ; LDA #$00	;load return value
	; ; JMP .EXIT


	
; .ARRAY.STOP_VALUE.FOUND
	; ;next array element
	; INY	;advance one element so that this routine will return NPC.TALK.ARRAY.POINTER with an index to the next element to be searched (i.e. the last elemenet to be searched +$1)
	; BNE .INCREMENT.COMPLETE3		;did lo byte index flip over to $00? If no, continue the loop
	; INC NPC.TALK.ARRAY.POINTER+$1	;if yes, increment HO byte index before continuing the loop
; .INCREMENT.COMPLETE3

			; ; STA TEMP
			; ; LDA TROUBLESHOOTING.HOOK
			; ; CMP #$01
			; ; BNE .TEMP
			; ; JSR PREP.BRK
			; ; ;LDA #$BB
			; ; ;LDA NPC.TALK.KEYWORD,X
			; ; ;LDA NPC.TALK.ARRAY.POINTER+$0			
			; ; ;LDA TEMP
			; ; LDA (NPC.TALK.ARRAY.POINTER),Y
			; ; ;LDX NPC.TALK.ARRAY.POINTER+$1
			; ; LDX NTALK.ARRAY.STOP.VALUE			
			; ; BRK
; ; .TEMP
			; ; LDA TEMP
			

			; LDA #$01	;load return value
	; ;**FALLS THROUGH**	
	
; .EXIT
; ;FINISH SETTING RETURN VALUES
	; PHA ;save ACC return value already set
	; ;set index return value 
	; TYA
	; CLC
	; ADC NPC.TALK.ARRAY.POINTER+$0
	; STA NPC.TALK.ARRAY.POINTER+$0	;close out Y-REG to the lo byte index to NPC.TALK.ARRAY, so that it reflects the last seached element
	; LDA NPC.TALK.ARRAY.POINTER+$1
	; ADC #$00 ;16-BIT ADD
	; STA NPC.TALK.ARRAY.POINTER+$1
	
	
	; PLA ;restore ACC return value already set
	
	; LDY #$00; reset Y-reg since pointer contains the exact index to the last element searched +$1. Other code in the calling routines expects Y-REG to be reset to $00 upon return

			; ; STA TEMP
			; ; LDA TROUBLESHOOTING.HOOK
			; ; CMP #$02	;normally $01
			; ; BNE .TEMP2
			; ; JSR PREP.BRK
			; ; LDA TEMP
			; ; LDX #$BA
			; ; LDY #$EE
			; ; ;LDX #NPC.TALK.KEYWORD
			; ; ;LDY /NPC.TALK.KEYWORD
			; ; ;LDX NPC.TALK.ARRAY.POINTER+$1
			; ; BRK
; ; .TEMP2
			; ; LDA TEMP
			
	; RTS
@END

ARRAY.SEARCH16.V2 ;======SEARCHES 16-BIT ARRAY FOR MULTI-CHARACTER KEYWORD===== (UP TO 256)
@START
; ;PARMATERS: NTALK.ARRAY.POINTER(2) = index to array to be searched or start position within an array, NTALK.ARRAY.STOP.VALUE, NTALK.KEYWORD.POINTER(2) = index to the array storing the keyword search string, NTALK.KEYWORD.STOP_VALUE, NTALK.SEARCH.MATCH_QTY = # of chars required for a match,  
; ;RETURN: ACC ($00 = keyword found, $01 = keyword not found), NTALK.ARRAY.POINTER.RETURN(2) (index to the next element to be searched (i.e. the last elemenet to be searched +$1)), NTALK.KEYWORD.POINTER.UPDATED (last element in keyword +1), YREG = $00 (always reset to $00 since pointer contains the exact index to the last element searched +$1. Other code in the calling routines expects Y-REG to be $00 upon return)
; ;ENTRANCE: NTALK.SEARCH.LOOP
; ;*If keyword is not found, NTALK.ARRAY.POINTER will be an index to the last charater searched, the +$1 only applies if the keyword is found, which occurs because an additional increment occurs to iterate the keyword stop value. 

;=====================SUBROUTINE DOCUMENTATION====================================
; ;****V1 & V2 difference***
; ;V1 requires that the keyword search string be loaded into NTALK.KEYWORD.ARRAY, whereas V2 will 
; ;use the keyword search string array that is specified by the NTALK.KEYWORD.POINTER parameter. Thus,
; ;V1 is somewhat faster, and V2 is more flexible. 
; ;V2 has additional return values, NTALK.KEYWORD.POINTER.UPDATED, which returns the last element of the keyword +1. and NTALK.ARRAY.POINTER which returns the next character after the match was found. The values in NTALK.KEYWORD.POINTER and NTALK.ARRAY.POINTER are preserved.  
; ;I think V2 may also have some bugs fixes
; ;
; ;Searches the 16-bit array specified by parameter NTALK.ARRAY.POINTER for the keyword specified by the parameter array NTALK.KEYWORD.POINTER(x),
; ;with both the keyword and array values being converted to uppercase if letters. NTALK.KEYWORD.POINTER is copied into NTALK.KEYWORD.BUFFER in the init routine so that
; ;X-REG can be used as the index during the main search loop.
; ;
; ;The value in the parameter NTALK.SEARCH.MATCH_QTY is the number of characteres that must match for the keyword to be considered found. 
; ;If the keyword in the search array is shorter than the keyword in NTALK.KEYWORD.BUFFER, then the extra characters in NTALK.KEYWORD.BUFFER are discarded for purposes of considering a match.
; ;
; ;If NTALK.ARRAY.STOP.VALUE is found, then the keyword wasn't found. NTALK.ARRAY.STOP.VALUE is a paramter
; ;set by the calling routine. After a primary command if found, NPC.SEARCH sets this value as being the NPC Record stop value. NPC.SEARCH.LOOP sets
; ;it to the NTALK command end code when doing seaches to determine which, if any, subcommands are present.
; ;
; ;
; ;Return value (ACC) = $00 if keyword found, $01 if keyword not found
; ;Return value (NTALK.ARRAY.POINTER.RETURN)
; ;		keyword found = the memory address of the last character of the keyword found + $1
; ;		keyword not found = NTALK.ARRAY.POINTER.RETURN will be an index to the last charater searched, the +$1 only applies if the keyword is found, which occurs because an additional increment occurs to iterate the keyword stop value. 
; ;Return value NTALK.KEYWORD.POINTER.UPDATED = last element of keyword +1
; ;
; ;In all cases Y-REG is rest to #$00 upon exit of this routine as it is closed out to the LO byte of NTALK.ARRAY.POINTER
; ;The original values at entry are preserved for NTALK.ARRAY.POINTER & NTALK.KEYWORD.POINTER
; ;so that sequential calls which use the same values do not need to set the parameters each time. 
; ;
; ;=================================================================================

			
; .SEARCH.INIT
	; ;setup the pointer to the array to be searched.
	; LDA NTALK.ARRAY.POINTER+$0		;this value will be preserved and available for sequential calls which require the same starting search position. 
	; STA NTALK.ARRAY.POINTER.RETURN+$0 ;this value will incremented and be available for calls which require the starting search position to resume where the last search left off. 

	; LDA NTALK.ARRAY.POINTER+$1
	; STA NTALK.ARRAY.POINTER.RETURN+$1

			
; ;COPY KEYWORD TO BUFFER
	; LDY #$00			;Init NTALK.ARRAY.POINTER & NTALK.KEYWORD.BUFFER index.

; .KEYWORD.COPY.LOOP	
	; LDA (NTALK.KEYWORD.POINTER),Y ;load character from the keyword array
	; STA NTALK.KEYWORD.BUFFER,Y	;save character to keyword buffer	
	; INY ;increment index
	; BNE .NO_OVERFLOW
	; JMP .ERROR.KEYWORD.OVERFLOW	;if index flips over then report overflow error. If the keyword array is less than $100 byte this isn't fool proof as a stop value might be found in the random values in memory past the end of the array but before the index flips at $100 bytes.
; .NO_OVERFLOW
	; CMP NTALK.KEYWORD.STOP_VALUE ;has end of keyword array been reached?
	; BNE .KEYWORD.COPY.LOOP	;if no, continue copying data
		
	; ;update pointer so it can be reused for a group search (like is done for curse words in NTALK.PARSE.KEYWORD)
	; TYA ;transfer keyword pointer index to ACC
	; CLC
	; ADC NTALK.KEYWORD.POINTER+$0 ;add LO byte of keyword pointer index
	; STA NTALK.KEYWORD.POINTER.UPDATED+$0
	; LDA NTALK.KEYWORD.POINTER+$1 ;load HO byte of keyword pointer index
	; ADC #$00 ;16-BIT add
	; STA NTALK.KEYWORD.POINTER.UPDATED+$1
		
; ;INIT INDEXES FOR MAIN LOOP
 	; LDY #$00			;Init NTALK.ARRAY.POINTER index. Y_REG must be init to $00 or it will cause a block of addresses to be skipped if the lo byte of the pointer is > $00
	; LDX #$00 			;init keyword index
	
; ;CONVERT 1ST CHAR OF KEYWORD TO UCASE
	; LDA NTALK.KEYWORD.BUFFER+$0		;does it match the first character of the keyword? 
	; CMP #$E1			;is ASCII value less than the lower end of the lowercase letter range?
	; BCC .NOT.A.LETTER4	;if yes then don't convert value to uppercase as that will damage the value, or if the value is already uppercase then no modification to the value is needed.
	; CMP #$FB			;is ASCII value greater than the upper end of the lowecase letter range?	
	; BCS .NOT.A.LETTER4	;if yes then don't convert value to uppercase as that will damage the value, or if the value is already uppercase then no modification to the value is needed.
	; AND #$DF ;masks out the 5th bit because the binary value of #$DF has 1 in each bit position except for bit 5.
; .NOT.A.LETTER4
	; STA NTALK.KEYWORD.1ST_CHAR.UCASE ;must be after the labels non-letter values are saved to the variable

	
; .KEYWORD.SEARCH.LOOP
; ;Note: search for the first character of the keyword. Once found, pass control to .FIRST.CHAR.FOUND takes over and continues the search for the rest of the keyword. If the remainder of the keyword is not found, control will be passed back to this loop to continue searching for the first character
; ;NTALK.ARRAY.STOP.VALUE is the stop value specified by the calling routine. It might be the stop value indicating the end of the array or it might be a stop value within the array, such as for indicating the end of a record.

; .CONVERT.UPPER_CASE2
	; LDA (NTALK.ARRAY.POINTER.RETURN),Y	;load next character from talk array
	; CMP #$E1			;is ASCII value less than the lower end of the lowercase letter range?
	; BCC .NOT.A.LETTER3	;if yes then don't convert value to uppercase as that will damage the value, or if the value is already uppercase then no modification to the value is needed.
	; CMP #$FB			;is ASCII value greater than the upper end of the lowercase letter range?	
	; BCS .NOT.A.LETTER3	;if yes then don't convert value to uppercase as that will damage the value, or if the value is already uppercase then no modification to the value is needed.
	; AND #$DF ;masks out the 5th bit because the binary value of #$DF has 1 in each bit position except for bit 5.
; .NOT.A.LETTER3
	; STA NTALK.ARRAY.UCASE ;must be after the labels non-letter values are saved to the variable

; .COMPARE.CHARACTERS1			
	; LDA NTALK.ARRAY.UCASE		;load next character from talk array

; ;	CMP NTALK.KEYWORD.BUFFER+$0		;does it match the first character of the keyword? 
	; CMP NTALK.KEYWORD.1ST_CHAR.UCASE		;does it match the first character of the keyword?
	; BEQ .FIRST.CHAR.FOUND			;if yes, then branch to the first character found routine to look for the rest of the keyword characters in sequence
	; CMP NTALK.ARRAY.STOP.VALUE		;is array character the array stop value specified by the calling routine?
	; BEQ .ARRAY.STOP_VALUE.FOUND		;if yes, keyword was not found.
; .NEXT.ARRAY.ELEMENT
	; INY	;increment LO byte index
	; BNE .KEYWORD.SEARCH.LOOP		;did lo byte index flip over to $00? If no, continue the loop
	; INC NTALK.ARRAY.POINTER.RETURN+$1	;if yes, increment HO byte index before continuing the loop
	; BNE .KEYWORD.SEARCH.LOOP
	; JMP .ERROR.ARRAY.OVERFLOW		;ho byte of pointer flipped over to $00. A stop value should have been found, something has gone wrong, report error.

; .FIRST.CHAR.FOUND
	; ;save index and pointer base address 
	; STY	SAVED.YREG.LOCAL ;saved array pointer index
	
	; LDA NTALK.ARRAY.POINTER.RETURN+$0
	; STA NTALK.ARRAY.POINTER.RETURN.LO.SAVED
	
	; LDA NTALK.ARRAY.POINTER.RETURN+$1
	; STA NTALK.ARRAY.POINTER.RETURN.HO.SAVED
	
; .SEARCH.REMAINDER.LOOP	

; .INCREMENT.INDEX	
	; INX ;next keyword element

	; ;next array element
	; INY	;increment LO byte index
	; BNE .INCREMENT.COMPLETE2		;did lo byte index flip over to $00? If no, continue the loop



	; INC NTALK.ARRAY.POINTER.RETURN+$1	;if yes, increment HO byte index before continuing the loop
	; BEQ .ERROR.ARRAY.OVERFLOW		;ho byte of pointer flipped over to $00. A stop value should have been found, something has gone wrong, report error.
; .INCREMENT.COMPLETE2

; .CHECK.MATCH.CHARS
	; CPX NTALK.SEARCH.MATCH_QTY
	; BEQ .KEYWORD.FOUND
	
			
; .CONVERT.UPPER_CASE
	; LDA NTALK.KEYWORD.BUFFER,X ;load next keyword character
	; CMP #$E1			;is ASCII value less than the lower end of the lowercase letter range?
	; BCC .NOT.A.LETTER1	;if yes then don't convert value to uppercase as that will damage the value, or if the value is already uppercase then no modification to the value is needed.
	; CMP #$FB			;is ASCII value greater than the upper end of the lowercase letter range?	
	; BCS .NOT.A.LETTER1	;if yes then don't convert value to uppercase as that will damage the value, or if the value is already uppercase then no modification to the value is needed.
	; AND #$DF ;masks out the 5th bit because the binary value of #$DF has 1 in each bit position except for bit 5.
; .NOT.A.LETTER1
	; STA NTALK.KEYWORD.UCASE ;must be after the labels non-letter values are saved to the variable
	; LDA (NTALK.ARRAY.POINTER.RETURN),Y	;load next array character
	; CMP #$E1			;is ASCII value less than the lower end of the lowercase letter range?
	; BCC .NOT.A.LETTER2	;if yes then don't convert value to uppercase as that will damage the value, or if the value is already uppercase then no modification to the value is needed.
	; CMP #$FB			;is ASCII value greater than the upper end of the lowecase letter range?	
	; BCS .NOT.A.LETTER2	;if yes then don't convert value to uppercase as that will damage the value, or if the value is already uppercase then no modification to the value is needed.
	; AND #$DF ;masks out the 5th bit because the binary value of #$DF has 1 in each bit position except for bit 5.
; .NOT.A.LETTER2
	; STA NTALK.ARRAY.UCASE ;must be after the labels non-letter values are saved to the variable
		
; .COMPARE.CHARACTERS			
	
	; LDA NTALK.ARRAY.UCASE 			;reload next array character
	; CMP #NTALK.COMMAND.SUBCOMMAND_CODE	;is next array character the subcommand code?
	; BEQ	.KEYWORD.FOUND					;if yes, then the array keyword is shorter than the players input keyword, and if this loop is still running it means all the characters of the array keyword match the player's keyword if the extra characters are discarded. This is considered a match. For example, if the player enters "JOBS" it will match on "JOB"
	; CMP #NTALK.COMMAND.END_CODE			;is next array character the command end code?
	; BEQ	.KEYWORD.FOUND					;if yes, then the array keyword is shorter than the players input keyword, and if this loop is still running it means all the characters of the array keyword match the player's keyword if the extra characters are discarded. This is considered a match. For example, if the player enters "JOBS" it will match on "JOB"	

	; LDA NTALK.KEYWORD.UCASE			;reload keyword character
	; CMP #NPC.TALK.KEYWORD.STOP_VALUE	;is it the keyword stop value?
	; BEQ .KEYWORD.STOP_VALUE.FOUND	;if yes, then keyword might be found, more checks are needed

	; LDA NTALK.ARRAY.UCASE 	
	; CMP NTALK.KEYWORD.UCASE 		;is it equal to the next keyword character?
	; BNE	.REMAINDER_NOT.FOUND		;if no, then the keyword was not found in this sequence. The first character found was a red herring. Or, also possible, it may have been a blue hippopotamus. 

	; JMP .SEARCH.REMAINDER.LOOP

; .KEYWORD.STOP_VALUE.FOUND
; ;This routine would not be entered unless the player keyword was less than the minimum chars required to find a match except in
; ;certain internal searches such as searching for the command end code. 
; ;Thus, if the interal search flag isn't set, we can concluded that the player keyword didn't match because it was too short (less than the minimum characters required for a match).

	; LDA NTALK.FLAG.INTERNAL_SEARCH 
	; CMP #$01	;is this an internal search?
	; BEQ .KEYWORD.FOUND	;if yes, then this is a match because all characters in NTALK.KEYWORD.BUFFER matched with a series of sequential characters in the search array
	; ;**FALLS THROUGH**
	
; .REMAINDER_NOT.FOUND
	; LDX #$00 ;reset keyword index
	
	; ;restore index and pointer base address to continue search as of the talk array element after the one containing the first character of the keyword which turned out to be a red herring
	; LDY	SAVED.YREG.LOCAL ;restore array pointer index

	; LDA NTALK.ARRAY.POINTER.RETURN.LO.SAVED
	; STA NTALK.ARRAY.POINTER.RETURN+$0

	; LDA NTALK.ARRAY.POINTER.RETURN.HO.SAVED
	; STA NTALK.ARRAY.POINTER.RETURN+$1
	; JMP .NEXT.ARRAY.ELEMENT	;enter loop here so that the talk array element is incremented before proceeding with the search. It would have ended on the red herring element. 		
	
; .KEYWORD.FOUND
			
	; LDA #$00	;load return value
	; JMP .EXIT

	
; .ARRAY.STOP_VALUE.FOUND
	; ;next array element
	; INY	;advance one element so that this routine will return NTALK.ARRAY.POINTER with an index to the next element to be searched (i.e. the last elemenet to be searched +$1)
	; BNE .INCREMENT.COMPLETE3		;did lo byte index flip over to $00? If no, continue the loop
	; INC NTALK.ARRAY.POINTER.RETURN+$1	;if yes, increment HO byte index before continuing the loop
; .INCREMENT.COMPLETE3

			; LDA #$01	;load return value
	; ;**FALLS THROUGH**	
	
; .EXIT
; ;FINISH SETTING RETURN VALUES
	; PHA ;save ACC return value already set
	; ;set index return value 
	; TYA
	; CLC
	; ADC NTALK.ARRAY.POINTER.RETURN+$0
	; STA NTALK.ARRAY.POINTER.RETURN+$0	;close out Y-REG to the lo byte index to NPC.TALK.ARRAY, so that it reflects the last seached element
	; LDA NTALK.ARRAY.POINTER.RETURN+$1
	; ADC #$00 ;16-BIT ADD
	; STA NTALK.ARRAY.POINTER.RETURN+$1
	
	
	; PLA ;restore ACC return value already set
	
	; LDY #$00; reset Y-reg since pointer contains the exact index to the last element searched +$1. Other code in the calling routines expects Y-REG to be reset to $00 upon return
			
	; RTS

; .ERROR.KEYWORD.OVERFLOW
; ;NTALK.ARRAY.SEARCH reports an overflow in .KEYWORD.COPY.LOOP
; ;possibly cause be a lack of a stop value or an incorrectly specified stop 
; ;value in the NTALK.KEYWORD.STOP_VALUE parameter. 
	; JSR PREP.BRK
	; BRK
	
	
; .ERROR.ARRAY.OVERFLOW		
; ;NTALK.ARRAY.SEARCH reports overflow when incrementing
; ;ho byte of NTALK.ARRAY.POINTER. A stop value in NPC.TALK.ARRAY should have been found, something has gone wrong.
	; JSR PREP.BRK
	; BRK
	
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

KEYIN.ANIMATION.SINGLE ;===WAITS FOR A KEYPRESS, ANIMATION ACTIVE=======
@START
;PARAMETERS: KEYIN.NO_CLEAR ($01 = DON'T CLEAR KEYPRESS BUFFER)
;RETURN VALUE: ACC (LAST KEY PRESS)
;ENTRACE: DIRECT

; ;SAVE REGISTER 
	; TXA
	; PHA
	
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
	;STA KEYIN.SAVED			;save keypress captured by this routine
	;LDX KEYIN.NO_CLEAR		;load parameter that controls whether keypress buffer should be cleared
	;BNE .EXIT				;if set then don't clear, exit
	STA KB_BUFFER_ACK       ;clr last key

	
.EXIT	
	;LDA #$00			;reset parameter to off
	;STA KEYIN.NO_CLEAR
	
; ;RESTORE REGISTER
	; PLA 
	; TAX
	
	;LDA KEYIN.SAVED		;restore keypress captured by this routine
	
	RTS
@END
	
KEYIN.STRING ;=========INPUT 1 CHAR WITH BACKSPACE SUPPORT===== 
@START
;PARAMTERS: *X-REG (remaining # of characters to input), Y-REG (keyboard input array index), HTAB, VTAB, KEYIN.STRING.LEFT_EDGE**
;RETURN: ACC (keypress), Y-REG (updated input array index), X-REG (updated char input counter)
;ENTRANCE: DIRECT, KEYIN.BCD
;*X-REG is a running total of the remaining characters to input. The first
;time this routine is called it should be set to the total number of characters to input.
;decrementing the counter is managed by the calling routine so that input filters can be applied. 
;The only reason this routine needs this counter is in the event of a backspace keypress, so that the counter can be
;incremented. 
;
;**KEYIN.STRING.LEFT_EDGE: the HTAB value of the left edge of the input field. HTAB won't be permitted to become less than this value by using the backspace key. 

;NOTE: for wrapper to this routine that inputs a 
;multi-character string see NPC.TALK.INPUT


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
	CMP #$88 ;left arrow key and delete key (AppleWIN) 
	BEQ .BACKSPACE.PRESSED	;if backspace is pressed, erase character in left adjacent column, move cursor 1 column left, and restart loop. 
	CMP #$FF ;delete key (real Apple II, Virutal ][)
	BEQ .BACKSPACE.PRESSED	;if backspace is pressed, erase character in left adjacent column, move cursor 1 column left, and restart loop. 

	RTS

.BACKSPACE.PRESSED	;if backspace is pressed, erase character in left adjacent column, move cursor 1 column left, and restart loop. 
	LDA KEYIN.STRING.LEFT_EDGE ;load the left edge of the input field
	CMP HTAB ;is the cursor at the left edge? 
	BCS .KEYIN ;if yes, then don't permit backspace, get next input character
	DEC HTAB
	JSR UPDATE.CHAR.POS

.UPDATE.INDEX
;UPDATE INDEX/COUNTERS TO REFLECT BACKSPACE	
	INX						;add one back to the character input counter
	DEY						;subtract one from the keyboard input array (RESULT) index
	
	LDA #$A0						;ASCII CODE: space
	JSR COUT

	DEC HTAB
	JSR UPDATE.CHAR.POS
	
	JMP .KEYIN
	
@END

KEYIN.BCD ;============WAITS FOR A BCD (!0-9) KEYPRESS=======
@START
;PARAMETERS; ACC (1-4, # OF BCD digits to input), HTAB, VTAB, KEYIN.STRING.LEFT_EDGE*
;RETURN: RESULT(2)

;*KEYIN.STRING.LEFT_EDGE: the HTAB value of the left edge of the input field. HTAB won't be permitted to become less than this value by using the backspace key. 

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
	LDY #$00				;init keyboard input array (RESULT) index
	
.LOOP

	JSR KEYIN.STRING
						
.APPLY.BCD.FILTER
@START			
	STA TEMP				;store key capture from buffer
	LSR						;shift the hi order nibble into the lo order nibble so we can do a compare on it.
	LSR
	LSR
	LSR
	CMP #$0B				;is the hi order nibble of the ASCII code $B?
	BNE .LOOP				;if no, continue loop because the key pressed was not !0-!9
	
	LDA TEMP					
	AND #$F					;mask out hi-order nibble. By masking out the hi order nibble we eliminate the B in the ASCII code and are left with 0-9 in the lo order nibble, which is easy to do a comparison on.
	CMP #$0A				;is the lo-order nibble < #$0A
	BCS .LOOP				;if no, continue loop because the key pressed was not !0-!9
@END

	LDA TEMP				;restore key captured from buffer
	STA BCD,Y				;save input value
	JSR COUT				;print key press to video screen

.LOOP.EXIT.TEST
@START	
	INY						;increment keyboard input array (RESULT) index	
	DEX						;decrement loop counter
	BEQ	.PACK.BCD.VALUES	;if counter (# of BCD digits to input) = $00 then exit loop
	JMP .LOOP				;if not, continue loop and collect more input
@END
			
.PACK.BCD.VALUES
@START				
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
@END

	
.EXIT	
;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX	
	
	RTS	

	
	
.ERROR
;Unexpected value in parameter ACC, # of BCD digits to input,
;in subroutine KEYIN.BCD
	LDA TEXT
	BRK
	
@END

;MONITOR.REGISTERS  (print them onscreen)
@START
	; ;MONITOR REGISTERS
		; ;SAVE CURSOR POSITION
				; LDA HTAB	
				; STA CURSOR.POSITION.SAVED+$0
				; LDA VTAB
				; STA CURSOR.POSITION.SAVED+$1

				; LDA #$1E 
				; STA HTAB
				; LDA #$5 
				; STA VTAB
				; JSR	UPDATE.CHAR.POS				
			
				; ;x-reg
				; TXA
				; JSR CONVERT.HEX_TO_ASCII
					; LDA RESULT+$1
				; JSR COUT
					; LDA RESULT+$0
				; JSR COUT
					; LDA #$A0 ;space
				; JSR COUT
			
				; ;y-reg
				; TYA
				; JSR CONVERT.HEX_TO_ASCII
					; LDA RESULT+$1
				; JSR COUT
					; LDA RESULT+$0
				; JSR COUT				
				
		; ;RESTORE CURSOR POSITION
				; LDA CURSOR.POSITION.SAVED+$0
				; STA HTAB	
				; LDA CURSOR.POSITION.SAVED+$1
				; STA VTAB
			; JSR	UPDATE.CHAR.POS	
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
		;	
		; LDA #$00
		; STA COPY.TO
		; LDA #$80
		; STA COPY.TO+$1
		;
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
;$A8 = .ERROR (TIME.SUN.STATUS, darkness_manager.asm)
;$A9 = .ERROR (LOOP.SEARCH.OFFSCREEN.LEFT, darkness_manager.asm)
;$AA = .ERROR (LOOP.SEARCH.OFFSCREEN.RIGHT, darkness_manager.asm)
;$AB = .ERROR (LOOP.SEARCH.OFFSCREEN.TOP, darkness_manager.asm)
;$AC = .ERROR (LOOP.SEARCH.OFFSCREEN.BOTTOM, darkness_manager.asm)
;$AD = .ERROR (DRAW.MISC, darkness_manager.asm)
;$AE = .ERROR2 (BOARD.FRIGATE, map_object_management.asm)


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
	PLA
	
	BRK
	
		
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
; ;PARAMETERS: ACC (length of delay)
	
; ;DISABLE BSR / ENABLE ROM	
	; LDA $C082
	
	; JSR WAIT.ADDRESS

; ;ENABLE BANK-SWITCHED RAM ($D000 Bank 1)
	; LDA $C08B					;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK1, 1ST)
	; LDA $C08B
	
	
	; RTS
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
		
KEYIN.STRING.LEFT_EDGE	.EQ FILL.VALUE

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

KEYIN.NO_CLEAR .EQ FILL.END	;parmeter of KEYIN.ANIMATION.SINGLE used to tell it not to clear the keypress buffer
KEYIN.SAVED	 .EQ FILL.END+$1


; KEYIN.STRING.XREG		.EQ FILL.END+$0
; KEYIN.STRING.YREG		.EQ FILL.END+$1

KEYIN.STRING.XREG	.BS $1
KEYIN.STRING.YREG	.BS $1

