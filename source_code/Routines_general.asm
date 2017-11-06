; ============================================
; Copyright (C) 2016-2018. 6502 Workshop, LLC
; contact: mark@6502workshop.com
; ============================================


;************************INCLUDE FILE*****************************
;(do not assemble stand-alone.  )


;LIBRARY INCLUDE TEMPLATE
;to use lirbary functions, it is easiest to just include them
;all because they share some variables. This probably could be
;avoided by creating duplicate variables using different names, 
;resulting in extra memory used. 
				; ;My libraries
				; .IN 	routines_graphics				
				; .IN 	routines_text
				; .IN 	routines_math
				; .IN 	routines_general
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

INSERTION_SORT
@START
;PARAMETERS: sort.table.address(2)*, array length**, array.record_size, sort.table.address
;ENTRANCE: DIRECT
;RETURN: SORTED LIST in the same memory space used prior to the sort
;
;*the sort table address should point to the first byte of the array length which precedes the table data.
;**the array length (16-bit), in bytes, must be stored in the first byte of the data array
;  

;=====================SUBROUTINE DOCUMENTATION====================================
;
;--WHAT IS AN INSERTION SORT?--
; https://en.wikipedia.org/wiki/Insertion_sort
; Insertion sort iterates, consuming one input element each repetition, and growing a sorted output list. Each 
; iteration, insertion sort removes one element from the input data, finds the location it belongs within the sorted 
; list, and inserts it there. It repeats until no input elements remain.
;
; Sorting is typically done in-place, by iterating up the array, growing the sorted list behind it. At each 
; array-position, it checks the value there against the largest value in the sorted list (which happens to be next to 
; it, in the previous array-position checked). If larger, it leaves the element in place and moves to the next. If 
; smaller, it finds the correct position within the sorted list, shifts all the larger values up to make a space, and 
; inserts into that correct position.
;
;=====Array Size / Record Size/Record Keys, Field Size=====
;
;-Array Size
;The arrary to be sorted can have a 16-bit length. The array length parameter is always 16-bit (set HO byte to $00 if the array is 8-bit) 
;
;-Record Size
;A !1 to !16 byte record is supported, though I didn't do a test with 1 byte records. 
;Set the record size via the array.record.size parameter at the subroutine is called. 
;
;to increase record size:
;set the following variable definitions equal to the new record size, so that enough memory is reserved:
;		unsorted_record.values and unsorted_record.values_plus1
;
;
;-Record Keys 
;
;The first byte of each record is the sort key. 
;To change the sort key, modify .DO.COMPARISON
;
;To add additional sort keys:
;
;I didn't give this a lot of thought but I'd start with .DO.COMPARISON. It probably would need to do a comparison
;on two bytes, and then make it's branch or fall through decision based on that. It might be possible that all changes need
;to made made to this code section.
;
;-Field Size
;1 byte. 
;
;====SORT ORDER=====
;
;The sort is done in ascending order.
;
;====PARAMETERS=====
;The first two bytes of the data array (TEST.TABLE) is the
;array length in bytes (LO/HO order). This must be set before calling the
;subroutine. 
;
;Also the LO address of the data array must be loaded into sort.table.address(2)
;before calling the subroutine
;
;Also, before calling, set array.record.size. 
;
;=================================================================================


;;===TEMPLATE====
		; ;setup record size
		; LDA #$02
		; STA array.record_size			
		; ;setup pointer
		; LDA #NPC.PATHFINDER.PRIORITY.QUE		
		; STA sort.table.address
		; LDX /NPC.PATHFINDER.PRIORITY.QUE	
		; STX sort.table.address+$1
	; JSR INSERTION_SORT


					
	
.SAVE.REGISTERS
	TXA
	PHA
	TYA
	PHA	
	
			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; lda array.record_size
			; sta $be00
			; lda #$ab
			; ldx sort.table.address+$0
			; ldy sort.table.address+$1		
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP
			

.INIT			
	ldy #$0
	lda sort.table.address
	ldx sort.table.address+$1
	
SORT_MAIN
@START
;INIT VALUES      
	sty sep_start_index
	cld
	sta copy_from.pointer ;save input address.LO
	sta input.start.addr ;save input address.LO


	clc
	adc #$2	;input address.LO+$2 to skip the array length bytes
	sta arr_start 

	
	stx copy_from.pointer+$1 ;save input address.HO
	stx input.start.addr+$1 ;save input address.HO
									
	txa
	adc #$0 ;16-bit add
	sta arr_start+$1

	ldy #$0
	lda (copy_from.pointer),y ;load array length LO byte 
	sta arr_length

	clc
	adc arr_start ;arr_length + arr_start
	sta arr_end ;arr_end = arr_length + arr_start

	iny
	lda (copy_from.pointer),y ;load array length HO byte 
	sta arr_length+$1

	adc arr_start+$1 ;16-bit add
	sta arr_end+$1
	
				
							; LDA sep_start_index
							; LDX J
							; LDY copy_from.pointer + 1
							; BRK
							

;original author comment:   for (h=1; h < length; h=3*h+1);

@END

    ldx sep_start_index     ; Start with highest value of h
.CHK_PREV_H  
;seems to test the length of the data array to be sorted and
;initiate an exit from the subroutine if no sort is needed
;because the data array is 0 or 1 bytes in length
; 
;
;UPDATE1: what are these values in sep_low and sep_high all about?
;they look like entry points for the shell sort. Since the first
;element value is $0002, and there is a two byte record size in TEST.TABLE,
;comparing array length to $0002 is an effective way to initiate an exit 
;if the array contains less than 1 full record.
;presumably the first element value would be increased if the record size increased. But that actually doesn't make
;sense because then the sort entrance point wouldn't be at the start of the first record since there is only a 2 byte header. 
;
;
;UPDATE1: Since sep_low, sep_high are the shell sort entry points,
;this routine is really all about finding the value in the sep_low/high table that
;is less than the array length. Since X-REG = $00 in this section during an insertion sort, (sep_start_index is set to 
;$00 at the beginning of the insertion sort code), I was thinking that the dex would automatically underflow, but X-REG would
;larger if this section was entered via a shell sort. 
;
;UPDATE1; I am really confused about the first value in the sept_lo and sept_hi table. It seems
;to act as both a sort entrance point and a record size, but I don't see how it
;can be both. Maybe I converted the source to SBASM format incorrectly. If so, it is odd that shell sort has worked
;in my tests. Though, the tests were simple. I do recall shell sort did not work in PATHFINDER; I ended up using insertion sort.
;

				lda sep_low,x
                cmp arr_length  ;CMP subtracts operand from ACC. If ACC < operand, an underflow results and the carry flag is cleared. The carry is Katie. 
                lda sep_high,x
                sbc arr_length+$1
                bcc .END_OF_INIT       ; If h < array_length, we've found the right sort.entrance.point
                dex
                bpl .CHK_PREV_H		l ;branch if X-REG underflowed to $FF
                jmp .exit                   ; array length is 0 or 1. No sorting needed.

.END_OF_INIT     
				inx
                stx sep_index

.H_LOOP ;set sort entrance point (always the begining of array for insert sort)       
				dec sep_index
                bpl .GET_H   ;did sep_index underflow to $FF?
                jmp .exit                  ; if yes, All done!

.GET_H 
				;load sort entrance point
				ldy sep_index			
                lda sep_low,y			
                sta sort.entrance.point
				;set record index	
                clc
                adc input.start.addr        ; ( input.start.addr is arr_start - 2)
                sta unsorted.record.index
                lda sep_high,y
                sta sort.entrance.point+$1
                adc input.start.addr+$1 ;16-bit add
                sta unsorted.record.index+$1

.NEXT.UNSORTED_RECORD ;increment unsorted.record.index

	;SET copy_from.pointer to the unsorted record
	lda unsorted.record.index
	clc
	adc array.record_size		;unsorted record index increment
	sta unsorted.record.index
	sta copy_from.pointer+$0
	lda unsorted.record.index+$1
	adc #$0	;16-bit add			
	sta unsorted.record.index+$1
	sta copy_from.pointer+$1

	;at end of array?
	ldx unsorted.record.index
	cpx arr_end ;if X-REG >= arr_end, then carry flag is set (think BCS)
	lda unsorted.record.index+$1
	sbc arr_end+$1 ;if unsorted.record.index+$1 >= arr_end+$1, then carry flag is set (think BCS)
	bcs .H_LOOP		;exit test. if a shell sort is used, it may have used a sort entrance point after the start of the 
					;array. Thus, just because the end of the array is reached doesn't mean that the sort 
					;is done. H_LOOP sets a sort entrance point.  

	

	;LOAD UNSORTED RECORD AND SAVE VALUES
	;copy_from.pointer = sort record
	ldy #$0 ;unsorted record byte $00
.load.unsorted.record.loop
	lda (copy_from.pointer),y
	sta unsorted_record.values,y
	clc
	adc #$1
	sta unsorted_record.values_plus1,y
	iny ;advance to unsorted record(byte $01)
	cpy array.record_size
	bne .load.unsorted.record.loop
	
.COMPARE_COPY.LOOP ;compare values and copy data until unsorted record is in the correct position or start of array is reached. 
         
	;SETUP POINTER TO DO COMPARE
	;copy_from.pointer = copy_to.pointer  - 1 record
	;1st iteration: set copy_to.pointer = unsorted record(byte $0)'s position in the array.
	lda copy_from.pointer+$0
	sta copy_to.pointer
	sec
	;sbc sort.entrance.point ;in this code section this variable seems to function as record size
	sbc array.record_size 
	sta copy_from.pointer+$0
	tax
	lda copy_from.pointer+$1
	sta copy_to.pointer+$1
	;sbc sort.entrance.point+$1
	sbc #$00
	sta copy_from.pointer+$1

	;reached bottom of the array?
	bcc .write.unsorted_record.values ;if ACC was less than SBC operand in any of the above operations, an underflow would have occured, clearing the carry flag.
	cpx arr_start
	sbc arr_start+$1
	bcc .write.unsorted_record.values 
				
						; sta testing.saved.acc
						; lda iteration.counter
						; cmp #$02
						; bne .temp2
						; lda #$af
						; brk
; .temp2
						; lda testing.saved.acc	
						
						;inc iteration.counter

.DO.COMPARISON
;
;=========NOTES FROM MY 16-bit to 8-bit CONVERSION=======
;The 16-bit version of this routine compares both bytes
;of record0 and record1 and branches to .write.unsorted_record.values if
;record 1 is less.
;
;The 8-bit version of ths routine compares the LO byte
;of record0 and record1 and branches to .write.unsorted_record.values if
;record 1 is less.
;
;The code below has notes on which lines can be commented/uncommented
;to enable/disable 16-bit. Note that enabling 16-bit in this
;way results in a 1 field record format as both bytes are used
;for one 16-bit field.
; 
;some values I traced on the first iteration of the loop:
;
;$7002 (the LO byte (copy_from.pointer),y), $7003(the HO byte (copy_from.pointer),y) was compared to v_plus_1 (value from $7004) and v_plus_1+1 (value from $9005) 
;
;
;


;LOAD COPY_FROM RECORD & COMPARE RECORD KEYS
;
;The record key is byte 0, so the following comparison is done:
;compare copy_from record(byte0) to unsorted record (byte0)
;
;copy_from.pointer = copy_to.pointer  - 1 record
;1st iteration: set copy_to.pointer = unsorted record(byte $0)'s position in the array.
;

	;compare record keys
	;is copy_from record byte(byte $0) <= unsorted record(byte $0)? If no, .MOVE.RECORD.DATA
	ldy #$0 ;byte $00
	lda (copy_from.pointer),y			
	cmp unsorted_record.values_plus1+$0 ;compare copy_from record byte(byte $0) to unsorted record(byte $0)
	bcc .write.unsorted_record.values		;branch if unsorted record byte(byte $0) <= adjacent record(byte $0)
	;**FALLS THROUGH** (copy_from(byte $0) <= unsorted record(byte $0))

	

			; sta testing.saved.acc
			; lda iteration.counter
			; cmp #$03
			; bne .temp2
			; lda #$aa
			; brk
; .temp2
			; lda testing.saved.acc	

	
	
.copy.record.data.loop ;copy record data right to left. 
	;copy_from.pointer = copy_to.pointer  - 1 record (byte $01)
	;1st iteration: set copy_to.pointer = unsorted record(byte $1)'s position in the array.
	;Y-REG = $00 (byte0)
	lda (copy_from.pointer),y
	sta (copy_to.pointer),y
	iny ;advance to next record byte
	cpy array.record_size
	bne .copy.record.data.loop

	ldy #$0 ;byte $00
	
	; dey                   ; Set y=0
	; txa ;X-REG contains copy_from record (byte $0), transfered to X-REG above in .DO.COMPARISON
	; sta (copy_to.pointer),y
	

			; sta testing.saved.acc
			; lda iteration.counter
			; cmp #$03
			; bne .temp2
			; lda #$ab
			; brk
; .temp2
			; lda testing.saved.acc	
			; sec

	bcs .COMPARE_COPY.LOOP  ; Original Author: Always branch
							; My comments: carry flag is set via the cpy in the loop exit check above when the 
							; y-reg value is < the array size, as that results in an underflow. 

				

.write.unsorted_record.values     
;when this routine is entered from .DO.COMPARISON, copy_to.pointer 
;is set to it's normal position (the right side record of the copy from/to paid, which is the unsorted record on the 1st
;iteration)
;		1st iteration: the copy operation below effectively does nothing as it is a read/write of the same data.
;		2nd+ iteration: the copy from/to record pair is decremented down/left in the array. In this case, copy_to is
;						;pointed to the last copy/from value which was in fact copied. Thus, the operation below slots 
;						;the unsorted record into the correct position. Consider if the last copy from/to data was 
;						;04.0A.02.07 before the copy was executed. After that same record pair would be 04.0A.04.0A. 
;						;This is because the 04.0A record was copied up/right but the copy/from still exist. The entire
;						;series of copy from/to operations essentilly has an "extra" value on the bottom/left side until
;						;the unsorted record is wrriten. 
;
;when this routine is entered from .COMPARE_COPY.LOOP, copy_from.pointer
;is set to one record to the left of the start of the array. Thus, copy_to_pointer is set to the 1st record in the array,
;which is the normal copy_from position. This is desirable because once the bottom of the array is reached, the unsorted record
;must be in the correct location at the first record position. 

	;copy unsorted record to the copy_to record. 
	ldy #$0
.write.loop
	lda unsorted_record.values,y
	sta (copy_to.pointer),y
	iny ;advance to next record byte
	cpy array.record_size
	bne .write.loop

						; sta testing.saved.acc
						; lda iteration.counter
						; cmp #$03
						; bne .temp
						; lda #$ac
						; brk
; .temp
						; lda testing.saved.acc
						
						
				JMP .NEXT.UNSORTED_RECORD
				
.exit


			

;RESTORE.REGISTERS
	PLA
	TAY
	PLA
	TAX	
	
	RTS
	
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

	; LDA #$FF
	; BNE .KEYIN
	;JSR CURSOR.DRAW
	
	;!!!!! BSR:BANK1 !!!!
	JSR ANIMATION.UPDATE.ENTRANCE
	
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

;!!!WARNING: the text window function isn't directly supported (i.e no line wrap). If using this 
;routine with the text window function make sure that the max input digits + animated cursor don't exceed the 
;end of the text line/row.
;

.KEYIN						;wait for keypress

.UPDATE.ANIMATION ;(update screen animation & cursor)
@START
;(to support screen animation, a parm will be needed to enable. If parm is enabled call animation.update and
;skip the manual animation frame increment)
	
	;!!!!! BSR:BANK1 !!!!
	;JSR ANIMATION.UPDATE.ENTRANCE



.MANUAL.CURSOR.ANIMATION ;(needed if ANIMATION.UPDATE.ENTRANCE is not called)
@START
;(needed if ANIMATION.UPDATE.ENTRANCE is not called because CURSOR.DRAW relies on ANIMATION.FRAME_STATE, which 
;is normally incremented by ANIMATION.UPDATE.ENTRANCE. And, a manual delay is needed or otherwise the cursor will
;render the animation frames much too quickly. The manually delay isn't needed if ANIMATION.UPDATE.ENTRANCE is called
;because updating the screen animation creates a substantial delay on its own).

	LDA ANIMATION.FRAME_STATE
	CMP #ANIMATION.TOTAL_FRAMES
	BNE .NOFLIP

	LDA #$00
	STA ANIMATION.FRAME_STATE
	JMP .INCREMENT.ANIMATION_FRAME.MANUAL.DONE
.NOFLIP
	INC ANIMATION.FRAME_STATE
.INCREMENT.ANIMATION_FRAME.MANUAL.DONE

.DELAY ;slow down cursor animation since screen animation isn't running
	TXA
	PHA


.DELAY.KEYIN.LOOP
	LDA KB_BUFFER
    BMI .DELAY.DONE				;branch on keypress
	
		LDA #$40 ;(Total wait = two $FFs)
	JSR WAIT.LOOP
	INX
	CPX #$8
	BNE .DELAY.KEYIN.LOOP

.DELAY.DONE
	PLA
	TAX
	
@END


	
.EXECUTE.CURSOR.DRAW	
	JSR CURSOR.DRAW
	
	
@END
	
	
	LDA KB_BUFFER
    BPL .KEYIN				;loop until key is pressed, depositing its ascii value in the keyboard buffer
    STA KB_BUFFER_ACK       ;clr last key
	CMP #$88 ;left arrow key and delete key (AppleWIN) 
	BEQ .BACKSPACE.PRESSED	;if backspace is pressed, erase character in left adjacent column, move HTAB 1 column left, and restart loop. 
	CMP #$FF ;delete key (real Apple II, Virutal ][)
	BEQ .BACKSPACE.PRESSED	;if backspace is pressed, erase character in left adjacent column, move HTAB 1 column left, and restart loop. 

	RTS

.BACKSPACE.PRESSED	;if backspace is pressed, erase character in left adjacent column, move HTAB 1 column left, and restart loop. 
	LDA KEYIN.STRING.LEFT_EDGE ;load the left edge of the input field
	CMP HTAB ;is HTAB at the left edge? 
	BCS .KEYIN ;if yes, then don't permit backspace, get next input character
	
	;erase animation cursor 
	JSR COUT.SPACE_CHAR		;prints a space character at the current HTAB/VTAB position


	; NOP
	; NOP
	; NOP
	; NOP
	; NOP
	
	DEC HTAB ;offset the HTAB+1 automatically done by COUT above to print the space character. 
	DEC HTAB ;backup 1 character position so HTAB = the digit to be erased by the backspace keypress. 
	JSR UPDATE.CHAR.POS
	
	;**FALLS THROUGH**
	
.UPDATE.INDEX
;UPDATE INDEX/COUNTERS TO REFLECT BACKSPACE	
	INX						;add one back to the character input counter
	DEY						;subtract one from the keyboard input array (RESULT) index
	
	JSR COUT.SPACE_CHAR		;prints a space character at the current HTAB/VTAB position


	DEC HTAB ;offset the HTAB+1 automatically done by COUT above to print the space character. 
	JSR UPDATE.CHAR.POS
	
	JMP .KEYIN
	
@END

KEYIN.BCD ;============WAITS FOR A BCD (!0-9) KEYPRESS=======
@START
;PARAMETERS; ACC (1-4, # OF BCD digits to input), HTAB, VTAB, KEYIN.STRING.LEFT_EDGE*
;RETURN: RESULT(2), ACC = ($00 = input successful | >=$01 = input aborted via ESC key)

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
	; STA BCD.MAX
	; DEC BCD.MAX				;max = ACC is a quantity (starts with 1). ACC-1 so it aligns with BCD array elements, which starts with $0
	
;SAVE REGISTERS	
	TXA
	PHA
	TYA
	PHA
	
	
;INIT INDEXES
	LDY #$00				;init keyboard input array (RESULT) index
	
.LOOP

	JSR KEYIN.STRING
		;ACC = KEYPRESS
	CMP #$9B ;ASCII = 'ESC'
	BEQ .INPUT_ABORTED
	
	
.APPLY.BCD.FILTER
@START			
	STA TEMP				;store key capture from buffer
	CMP #$8D				;was enter/return key pressed?
	BEQ .RETURN.PRESSED
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

.RETURN.PRESSED ;shift digits entered into propert positions to assume leading zeros before the digits entered prior to the return key press
@START
;Note: this routine only supports a BCD digits to input (the ACC parm to this subroutine) of 2. It's not relevant if
;the digits to input = 1. For 3 and 4 things get more complicated because the BCD(4) variable isn't organized left to right with the most significant digit to least significant or vice versa
;
;Example:
;
;!9 	= BCD: 00.09.00.00
;!99 	= BCD: 09.09.00.00
;!999 	= BCD: 09.09.00.09
;!9999 	= BCD: 09.09.09.09
;
;Accoringly, BCD+1 is the least significant digit of the first two digits, and BCD+3 is the least significant digit of the 2nd two digits. Thus,
;we can't just shuffle the digits left to right, or right to left. 
; 
;My failed attempt at a one size fits all routine is below. I'm tabling this issue because I don't think I'll
;need a BCD input routine for more than 2 digits that supports and enter key press.
;
;
;A branch for each case (2 digit input, 3 digit input, 4 digit etc) may be the only way to get it to work.
;but even then, there may need to be a subcase for when the return key was pressed. 

	LDA BCD+$0
	STA BCD+$1
	LDA #$B0 ;$00 could be used as well. The $B is masked out anyway. 
	STA BCD+$0

;**FALLS THROUGH**	
	
;***FAILED ATTTEMPT AT A SCALABLE ROUTINE	
	; STY BCD.TOTAL			;total digits (a quantity that starts with 1) = keybord input array element last filled + 1 (+1 so it can be used for a bottom of the loop exit test).
							; ;YREG = keybord input array element last filled + 1 because Y-REG was incremented after the last digit was entered. 
	; LDX #BCD.MAX-1			;the max digits support by this routine -1, which is the last element in the array, which should hold the least significant digit of the input number

	; LDY #$00
	; TYA
; .LOOP.INIT
	; STA BCD.SHIFTED,Y
	; INY
	; CPY #BCD.MAX
	; BNE .LOOP.INIT

	; LDY #$00	
; .LOOP.SHIFT
	; LDA BCD,Y
	; STA BCD.SHIFTED,X
	; DEX
	; INY
	; CPY BCD.TOTAL
	; BNE	.LOOP.SHIFT


	; LDY #$00
; .LOOP.FINAL_COPY
	; LDA BCD.SHIFTED,Y
	; STA BCD,Y
	; INY
	; CPY #BCD.MAX
	; BNE .LOOP.FINAL_COPY
	

		


@END

			
.PACK.BCD.VALUES

		; lda #$00
		; sta bcd+$0
		; lda #bcd
		; sta $bf00
		; lda /bcd
		; sta $bf01
		; lda #bcd.shifted
		; sta $bf02
		; lda /bcd.shifted
		; sta $bf03
		; LDA BCD.TOTAL
		; STA $bf04
		; lda #$aa
		; jsr prep.brk
		; brk
		
		
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


.INPUT_SUCCESSFUL
	LDA #$00 ;($00 = input successful | >=$01 = input aborted via ESC key)
	JMP .SAVE.RETURN_VALUE

.INPUT_ABORTED
	LDA #$01 ;($00 = input successful | >=$01 = input aborted via ESC key)
	
.SAVE.RETURN_VALUE
	STA TEMP
	
.EXIT	
;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX	
	
	;restore ACC return value
	LDA TEMP  ;($00 = input successful | >=$01 = input aborted via ESC key)
	
	RTS	

	
	
.ERROR
;Unexpected value in parameter ACC, # of BCD digits to input,
;in subroutine KEYIN.BCD
	LDA TEXT
	BRK
	
@END

;MONITOR.REGISTERS  ;(print them onscreen)
@START

; ;SAVE REGISTERS	
	; TXA
	; PHA
	
	; TYA
	; PHA

; ;SAVE ZERO PAGE VARIABLES

	; ;used by COUT.ADDRESS
	; LDA $EA
	; PHA
	
	; LDA $EB
	; PHA


	
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
	
	
; ;RESTORE ZERO PAGE VARIABLES

	; ;used by COUT.ADDRESS
	; PLA
	; STA $EB

	; PLA
	; STA $EA
	

	
; ;RESTORE REGISTERS
	; PLA
	; TAY
	
	; PLA
	; TAX	
		
	RTS
	
@END

MONITOR.VARIABLE  ;(print onscreen) **use .DEBUG.PRINT_0 or .DEBUG.PRINT_X if only 1 BCD digit is needed i.e. the number diplayed is !0-!9**
@START
;PARAMETERS: ACC (variable1 to monitor), X-REG (variable2 to monitor)
;			 VARIABLE1.HTAB, VARIABLE1.VTAB, VARIABLE2.HTAB, VARIABLE2.VTAB

;;TEMPLATE1
		; PHA ;save ACC
		; TXA
		; PHA
		
		; LDA #$00
		; STA VARIABLE1.HTAB
		; LDA #$00
		; STA VARIABLE1.VTAB
		; LDA #$00
		; STA VARIABLE2.HTAB
		; LDA #$01
		; STA VARIABLE2.VTAB								
		
		; LDA VARIABLE1
		; LDX VARIABLE2
	; JSR MONITOR.VARIABLE
	; JSR KEYIN ;pause optional
		; STA TEMP
; ;	
; ;
; ;--------------------------------
; ;OPTIONAL: detect "Q" to break	
; ;note: place directly after template1. This way at the JSR KEYIN in template 1, if "Q" is pressed the code breaks to 
; ;the monitor so troubleshooting can be done.  
			; ; CMP #$D1
			; ; BNE .TEMP
			; ; LDA #$AA
			; ; JSR PREP.BRK
			; ; BRK
; ; .TEMP
			; ; LDA TEMP
; ;--------------------------------
; ;
		; PLA
		; TAX
		; PLA ;restore ACC
		
; ;
; ;----------------------------END TEMPLATE						
			
	STA TEMP16 		;save variable to monitor #1
	STX TEMP16+$1	;save variable to monitor #2

;SAVE REGISTERS	
	TXA
	PHA
	
	TYA
	PHA

;SAVE ZERO PAGE VARIABLES

	;used by COUT.ADDRESS
	LDA $EA
	PHA
	
	LDA $EB
	PHA
		
	;MONITOR REGISTERS
	
				;SAVE CURSOR POSITION
				LDA HTAB	
				STA CURSOR.POSITION.SAVED+$0
				LDA VTAB
				STA CURSOR.POSITION.SAVED+$1

					LDA VARIABLE1.HTAB
					STA HTAB
					LDA VARIABLE1.VTAB
					STA VTAB
					
				JSR	UPDATE.CHAR.POS				
	

	
			
				;variable1 (ACC)
					LDA TEMP16+$0 ;restore variable to monitor
				JSR CONVERT.HEX_TO_ASCII
					LDA RESULT+$1	
				JSR COUT
					LDA RESULT+$0
			JSR COUT
					LDA #$A0 ;space
				JSR COUT



			
					;text window location
					; LDA #$21 
					; STA HTAB
					; LDA #$5 
					; STA VTAB
					
				
					; LDA #$26
					; STA HTAB
					; LDA #$8 
					; STA VTAB
					LDA VARIABLE2.HTAB
					STA HTAB
					LDA VARIABLE2.VTAB
					STA VTAB				
				JSR	UPDATE.CHAR.POS



			
			
				;variable2 (X-REG)
					LDA TEMP16+$1
				JSR CONVERT.HEX_TO_ASCII
					LDA RESULT+$1
				JSR COUT
					LDA RESULT+$0
				JSR COUT				





			
		;RESTORE CURSOR POSITION
				LDA CURSOR.POSITION.SAVED+$0
				STA HTAB	
				LDA CURSOR.POSITION.SAVED+$1
				STA VTAB
			JSR	UPDATE.CHAR.POS

.EXIT


;RESTORE ZERO PAGE VARIABLES

	;used by COUT.ADDRESS
	PLA
	STA $EB

	PLA
	STA $EA
	

	
;RESTORE REGISTERS
	PLA
	TAY
	
	PLA
	TAX	



			
	RTS
@END
		
;.DEBUG.PRINT_0 ;prints onscreen marker to track program flow. **OPT** Memory. Remove when resolved.  
@START
	; PHA ;SAVE ACC
	
		; LDA #$26
		; STA HTAB	
		; LDA #$11	
		; STA VTAB
	; JSR	UPDATE.CHAR.POS
	
			; LDA #$B0
		; JSR COUT
		
	; PLA ;RESTORE ACC
@END

;.DEBUG.PRINT_X ;same as above but with auto-increment to ASCII code printed. **OPT** Memory. Remove when resolved.  
@START

;;*******in first instance. set COW to $00***

	; PHA ;SAVE ACC
	
		; LDA #$26
		; STA HTAB	
		; LDA #$11	
		; STA VTAB
	; JSR	UPDATE.CHAR.POS
	
			; LDA COW
			; ORA #$B0
		; JSR COUT
		
	; PLA ;RESTORE ACC
	
	; INC COW
@END

;DEBUG.PRINT ;same as .DEBUG.PRINT_X but setup to be called as a subrotuine. **OPT** Memory. Remove when resolved.  
@START
		
	;PHA ;SAVE ACC

	; LDA TROUBLESHOOTING.HOOK
	; BEQ .TEMP2
	
		; LDA #$26
		; STA HTAB	
		; LDA #$11	
		; STA VTAB
	; JSR	UPDATE.CHAR.POS
	
			; LDA COW
			; ORA #$B0
		; JSR COUT

;print COMBAT_SE.MODE.PARM		
		; LDA COW
		; cmp #$01
		; bne .exit
		
		
		; LDA #$26
		; STA HTAB	
		; ;LDA #$11	
		; LDA #$12	
		; STA VTAB
	; JSR	UPDATE.CHAR.POS
	
			; LDA COMBAT_SE.MODE.PARM
			; ORA #$B0
		; JSR COUT



;print variable



		; TXA
		; PHA
		
		; LDA #$24
		; STA VARIABLE1.HTAB
		; ;LDA #$13
		; LDA #$14
		; STA VARIABLE1.VTAB
		; LDA #$26
		; STA VARIABLE2.HTAB
		; ;LDA #$13
		; LDA #$14
		; STA VARIABLE2.VTAB								
		
		; LDA DEBUG.PRINT.2+$0
		; LDX DEBUG.PRINT.2+$1
	; JSR MONITOR.VARIABLE

		; PLA
		; TAX
		

;-------------------------		

	; LDA $C012		;ROM/BSR soft-switch flag (bit7 = 1: BSR, bit7=0 ROM)
	; STA COW2+$0
	; LDA $C011		;BANK1/BANK2 soft-switch flag (bit7 = 1: BANK2, bit7=0 BANK1)
	; STA COW2+$1

		; LDA #$26
		; STA HTAB	
		; LDA #$13	
		; STA VTAB
	; JSR	UPDATE.CHAR.POS
	
			; LDA COW2+$0
		; JSR COUT

			; LDA COW2+$1
		; JSR COUT
		
; .temp2
; .exit
		
	; PLA ;RESTORE ACC
	
	; INC COW
	
	; RTS
	
@END			
	
MEMORY.COPY ;(Peter Ferrie's version)
@START
;PARAMTERS; COPY.TO(2), COPY.FROM_START(2), COPY.FROM_END(2)
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
	TXA
	PHA
	TYA
	PHA
	
	
	
    LDA COPY.FROM_END+$0        ;set end address (LO)
    SEC
    SBC COPY.FROM_START+$0        ;set start address (LO)
    PHA ;init COPY.SIZE lo byte counter
    LDA COPY.FROM_END+$1        ;set end address (HO)
    SBC COPY.FROM_START+$1 ;16-bit subtract
    BEQ .PARTIAL
    TAX

    ;copy whole pages first

    LDY #$00                    ;COPY FROM/TO index

.LOOPBIG

    LDA (COPY.FROM_START),Y
    STA (COPY.TO),Y
    INY
    BNE .LOOPBIG
    INC COPY.FROM_START+$1
    INC COPY.TO+$1
    DEX
    BNE .LOOPBIG

.PARTIAL
    PLA
    BEQ .COPY_DONE
    TAY

    ;copy final partial page

; .LOOPSML  ;use if COPY.FROM_END is the last byte to copy +1
    ; DEY
    ; LDA (COPY.FROM_START),Y
    ; STA (COPY.TO),Y
    ; CPY #$00
    ; BNE .LOOPSML

.LOOPSML ;use if COPY.FROM_END is the last byte to copy
    LDA (COPY.FROM_START),Y
    STA (COPY.TO),Y
    DEY
    CPY #$FF
    BNE .LOOPSML
	
.COPY_DONE



;RESTORE REGISTERS	
	PLA
	TAY
	PLA
	TAX
	
	RTS
	
@END


;MEMORY.COPY ;(my version, deprecated for Peter's) 
@START
; ;PARAMTERS; COPY.TO(2), COPY.FROM_START(2), COPY.FROM_END(2)
; ;RETURN: NONE
; ;ENTRANCE: DIRECT

; ;=====================SUBROUTINE DOCUMENTATION====================================
; ;
; ;This routine takes the approach of incrementing the start address and copy to address
; ;together. They both share Y-REG as the index and when it flips the HO byte for both is increment. 
; ;
; ;This works because COPY.SIZE+$1 and X-REG holds the number of bytes to copy and is decremented
; ;as bytes are copied. Exit testing is based on COPY.SIZE+$1 and X-REG.
; ;
; ;=================================================================================
	
; ;DRIVER TEMPLATE	
		; ; LDA #$00
		; ; STA COPY.FROM_START
		; ; LDA #$70
		; ; STA COPY.FROM_START+$1
		; ; LDA #$00
		; ; STA COPY.FROM_END
		; ; LDA #$75
		; ; STA COPY.FROM_END+$1
		; ;	
		; ; LDA #$00
		; ; STA COPY.TO
		; ; LDA #$80
		; ; STA COPY.TO+$1
		; ;
		; ; JSR MEMORY.COPY
		

; .START

; ;SAVE REGISTERS	
	; TXA
	; PHA
	; TYA
	; PHA

	
	; ;calculate # of bytes to copy (size of copy)
	; ;(the copy size is used for loop exit test)
	; LDA COPY.FROM_END+$0		;set end address (LO)
	; SEC
	; SBC COPY.FROM_START+$0		;set start address (LO)
	; ;STA COPY.SIZE+$0  			;set # of byte to copy (LO)
	; TAX ;init COPY.SIZE lo byte counter
	; LDA COPY.FROM_END+$1		;set end address (HO)
	; SBC COPY.FROM_START+$1 ;16-bit subtract
	; STA COPY.SIZE+$1			;set # of byte to copy (HO)			
	
	; LDY #$00					;COPY FROM/TO index
; .LOOP					
	; LDA (COPY.FROM_START),Y
	; STA (COPY.TO),Y
	; ;**FALLS THROUGH**
; .DECREMENT.SIZE.COUNTER
	; CPX #$00 ;if lo byte counter is already #$00, then the next decrement will flip it. Branch so that the LO byte and HO byte decrements are both done. 
	; BEQ .DECREMENT.SIZE.COUNTER.HO_BYTE
	; DEX ;decrement COPY.SIZE LO byte counter
	; JMP .INCREMENT.COUNTER.LO_BYTE
	
; .DECREMENT.SIZE.COUNTER.HO_BYTE

	; ;exit test
	; LDA COPY.SIZE+$1	;if COPY.SIZE HO byte is already zero then all bytes have been copied. 
	; BEQ .COPY_DONE
	; DEX ;flips to $FF
	; DEC COPY.SIZE+$1	;decrement HO byte
		
	; ;**FALLS THROUGH**
	
; .INCREMENT.COUNTER.LO_BYTE
	; INY			;increment COPY TO/FROM index
	; BNE .LOOP	;if y-reg hasn't flipped to $00, continue loop

; .INCREMENT.COUNTER.HO_BYTE		
	; INC COPY.FROM_START+$1
	; INC COPY.TO+$1

	; JMP .LOOP

; .COPY_DONE					;IF YES, THEN COPY IS DONE. 

; ;RESTORE REGISTERS	
	; PLA
	; TAY
	; PLA
	; TAX
	
	; RTS
	
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


FULL.BRK.AUX_BSR
@START
	; STX TEMP ;save X-REG parm (used for troubleshooting)

		; ;disable
	; STA $C008 ;enable main zero-page & main BSR 
		; LDX STACK_POINTER.SAVED	;restore stack pointer to X-REG
		; TXS ;transfer X-REG to stack pointer
	
	; LDX TEMP ;restore X-REG parm (used for troubleshooting)
	; ;**FALLS THROUGH**
	
@END

FULL.BRK	;break from main-BSR and aux-BSR
@START
;PARAMETERS: ACC (calling routine ID code)
;ENTRANCE: DIRECT
;RETURN:NONE

;Description: JMP to this routine instead of using BRK when
;troubleshooting routines in BSM (including AUX BSM), becaue this routine is in main
;memory.
; 
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


.DETECT.MEMORY_STATE
;(note: determine if FULL.BRK was called from AUX BSR. If so then the AUX stack and zero page are also enabled.
;I'm not sure I understand why, but main zero page/stack/BSR need to be enabled before the BSR/ROM toggle is flipped or
;this routine hangs. )

	STA TEMP16 ;save acc parm
	
	
	;is aux zero page, stack, and BSR active? 
	LDA $C016 ;(high-bit set = aux zp,stack,bsr active | high-bit not set = main zp,stack,bsr active)
	BPL .DETECT.MEMORY_STATE.DONE 

	STX TEMP ;save X-REG parm (used for troubleshooting)

		;disable
	STA $C008 ;enable main zero-page & main BSR 
		LDX STACK_POINTER.SAVED	;restore stack pointer to X-REG
		TXS ;transfer X-REG to stack pointer
	
	LDX TEMP ;restore X-REG parm (used for troubleshooting)
.DETECT.MEMORY_STATE.DONE

	LDA TEMP16 ;restore acc parm

	
	;**FALLS THROUGH**

	
.SAVE.REGISTERS

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

;;=====INLINE TEMPLATE====
;;(the contents of the JSRs are unrolled)
;
;
;;---DEBUG BREAK---
	; LDA TEXT
	;
; ;DISABLE.BS_RAM
	; LDA $C082				;READ ENABLE ROM, DISABLE WRITE ON BSM (NORMAL STATE).
							; ;REQUIRED FOR BEFORE BRK BECAUSE APPLE MONITOR IS A ROM ROUTINE
;
	; JSR CLEAR.TEXT.SCREEN   ;this JSR is to a ROM address
	;
; ;JSR HCR.OFF
; ;RETURN OUTPUT HOOK TO NORMAL SETTINGS	
	; LDA #$F0		;PRODUCES LOW BYTE
	; STA CSW
	; LDA #$FD		;PRODUCES HIGH BYTES
	; STA CSW+$1
	;
	;
	; BRK

	
	
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

WAIT.LOOP
@START
;PARAMETERS: ACC (LOOP LENGTH IN SUPER-PAGES)

.SET.PARAMETERS
	
	STA TEMP
	BEQ .EXIT.ALTERNATE.ENTRANCE ;don't restore registers since they were not saved
	
.SAVE.REGISTERS
	TXA
	PHA
	TYA
	PHA
	
	
	LDX #$00 ;init delay counter, lo byte
	LDY #$00 ;init delay counter, ho byte
.WAIT.LOOP1	
.KB_BUFF.CHECK2.DONE
	; ;abort keypress check
	; LDA $C000
    ; BMI .EXIT ;if key has been pressed since last check, then exit loop

	INX ;increment lo byte delay counter
	BNE .WAIT.LOOP1
	INY ;increment ho byte delay counter
	CPY TEMP ;arbitrary value tuned to the visually observed animation speed
	BNE .WAIT.LOOP1	
	JMP .RESTORE.REGISTERS
	
.EXIT

	
.RESTORE.REGISTERS
	PLA
	TAY
	PLA
	TAX

.EXIT.ALTERNATE.ENTRANCE	
	LDA TEMP ;reset ACC to parameter value so loop can be called multiple times in a row without resting the parameter
	RTS
@END
	

;========DEFINE VARIABLES=========


;**see "GENERAL ROUTINES (LOW LEVEL)" section in offloaded_variables.ASM
 
