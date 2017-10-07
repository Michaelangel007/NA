;************************INCLUDE FILE*****************************
;(do not assemble stand-alone.  )


;=====================INCLUDE FILE DOCUMENTATION====================================
;
;Include file to SWAP.ROUTINES.INVENTORY.ASM
;
;=================================================================================



INV_2.DISPLAY.COMBAT_STATS
@START


.INIT
; ;CLEAR INVENTORY TEXT SPACE
	; JSR INV.ERASE.INVENTORY_WINDOW.TEXT_SPACE ;foreground and background page /w flip

		
.INIT.STATS.SUB_MODULE
	JSR INV.REFRESH.ACTIVE_PLAYER.DATA ;reads character sheet record (from in-memory array) and readied equipment record (from in-memory array)
		
		
;###EXECUTE SCREEN BUILDER
@START

		;special parms (lookup table indexes that need custom bit manipulation)
		;<NONE>
		
		;standard parms
		LDA #$01 ;set HTAB of left margin of text space
		STA SCREEN_BUILDER.HTAB.LEFT
		LDA #INV_2.COMMAND_TABLE1
		STA SCREEN_BUILDER.COMMAND_TABLE.POINTER+$0
		LDA /INV_2.COMMAND_TABLE1
		STA SCREEN_BUILDER.COMMAND_TABLE.POINTER+$1		
	JSR SCREEN_BUILDER
	
@END
	

;.MODIFIED_DAMAGE
@START
; ;UPDATE SCREEN POSITION
	; JSR INV.TEXT_TOOLS.VTAB2	;;increment VTAB by 2, reset HTAB to #$01, and update screen position

	
; ;PRINT HEADING
		; LDA #INV_2.HEADING.MODIFIED_DAMAGE
		; STA STRING+$0
		; LDA /INV_2.HEADING.MODIFIED_DAMAGE
		; STA STRING+$1
	; JSR PRINT.STR


; ;-------LEFT/RIGHT HAND---------
; ;UPDATE SCREEN POSITION
	; JSR INV.TEXT_TOOLS.VTAB1	;;increment VTAB by 2, reset HTAB to #$01, and update screen position

; ;LEFT HAND
; ;PRINT LABEL
		; LDA #INV_2.LABEL.LHAND
		; STA STRING+$0
		; LDA /INV_2.LABEL.LHAND
		; STA STRING+$1
	; JSR PRINT.STR
	
	
; ;PRINT VARIABLE
		; LDA #INV_2.SCREEN2.LHAND.VARIABLE.HTAB
		; STA HTAB	
		; LDA #INV_2.SCREEN2.LHAND.VARIABLE.VTAB	
		; STA VTAB
	; JSR	UPDATE.CHAR.POS
		
	; ;Print "N/A" if no weapon is readied in left hand
	; ;(note: this does happen. If weapons are readied to both hands and the weapon in the left hand is unreadied, then the left hand will be empty)
	; LDA CHR_SHEET.PC.DMG.LHAND
	; BNE .LHAND.EXECUTE_PRINT ;if modified damage = $00 then no weapon is readied. If fists are readied then modified damage = STR modified
	
	; ;PRINT N/A		
	; JSR INV_2.PRINT.N_A

	; JMP .LHAND.EXECUTE_PRINT.DONE
	
; .LHAND.EXECUTE_PRINT	
		; LDA CHR_SHEET.PC.DMG.LHAND
		; ; CLC ;(CLC = don't print CR | SEC = print CR to text window)
	; JSR PRINT.HEX8.RJ
; .LHAND.EXECUTE_PRINT.DONE

	
; ;RIGHT HAND
; ;UPDATE SCREEN POSITION
	; JSR INV.TEXT_TOOLS.VTAB1	;;increment VTAB by 1, reset HTAB to #$01, and update screen position

; ;PRINT LABEL
		; LDA #INV_2.LABEL.RHAND
		; STA STRING+$0
		; LDA /INV_2.LABEL.RHAND
		; STA STRING+$1
	; JSR PRINT.STR
	
	
; ;PRINT VARIABLE
		; LDA #INV_2.SCREEN2.RHAND.VARIABLE.HTAB
		; STA HTAB	
		; LDA #INV_2.SCREEN2.RHAND.VARIABLE.VTAB	
		; STA VTAB
	; JSR	UPDATE.CHAR.POS

	; ;Print "N/A" if no weapon is readied in this hand
	; LDA CHR_SHEET.PC.DMG.RHAND
	; BNE .RHAND.EXECUTE_PRINT ;if modified damage = $00 then no weapon is readied. If fists are readied then modified damage = STR modified
	
	; ;PRINT N/A	
	; JSR INV_2.PRINT.N_A

	; JMP .RHAND.EXECUTE_PRINT.DONE
	
; .RHAND.EXECUTE_PRINT	
		; LDA CHR_SHEET.PC.DMG.RHAND
		; ; CLC ;(CLC = don't print CR | SEC = print CR to text window)
	; JSR PRINT.HEX8.RJ
; .RHAND.EXECUTE_PRINT.DONE
	
@END

; ;UPDATE SCREEN POSITION
	; JSR INV.TEXT_TOOLS.VTAB2	;;increment VTAB by 2, reset HTAB to #$01, and update screen position

; ;PRINT LABEL
		; LDA #INV_2.LABEL.TO_HIT
		; STA STRING+$0
		; LDA /INV_2.LABEL.TO_HIT
		; STA STRING+$1
	; JSR PRINT.STR
	
	
; ;PRINT VARIABLE
		; LDA #INV_2.SCREEN2.TO_HIT.VARIABLE.HTAB
		; STA HTAB	
		; LDA #INV_2.SCREEN2.TO_HIT.VARIABLE.VTAB
		; STA VTAB
	; JSR	UPDATE.CHAR.POS
	
		; ; LDA CHR_SHEET.PC_MOB.TO_HIT
		; ; CLC ;(CLC = don't print CR | SEC = print CR to text window)
	; ; JSR PRINT.HEX8.RJ
	
		; LDA CHR_SHEET.PC_MOB.TO_HIT ;***this value is BCD format
	; JSR PRINT.TEXT.WINDOW.BCD8	
	

@END
.FOOTER
@START
		LDA #$B3 ;ASCII code for current screen number: 3
	JSR INV.TEXT_TOOLS.PRINT_FOOTER

@END


.EXIT

	JMP INV.STATS.STATE_LOOP
	

@END
		
		
;LOCAL SUBROUTINES
@START


;INV_2.PRINT.N_A
@START
		; LDA #INV_2.NOT_APPLICABLE
		; STA STRING+$0
		; LDA /INV_2.NOT_APPLICABLE
		; STA STRING+$1
	; JSR PRINT.STR

	RTS
@END


@END

;LOCAL TEXT BLOCKS
@START
; INV_2.SCREEN2_TITLE				.AZ	-/COMBAT STATS/
; INV_2.HEADING.MODIFIED_DAMAGE	.AZ	-/MODIFIED DAMAGE/
;INV_2.LABEL.LHAND				.AZ -/Left Hand/
;INV_2.LABEL.RHAND				.AZ -/Right Hand/
; INV_2.LABEL.ARMOR_RATING		.AZ -/Armor Rating/
; INV_2.LABEL.RESIST_MAGIC		.AZ -/Resist Magic/
;INV_2.LABEL.TO_HIT				.AZ -/% TO-HIT/
;INV_2.NOT_APPLICABLE			.AZ -/ N/,#$AF,/A/

INV_2.TEXT_BLOCK.NA				.AZ -/ N/,#$AF,/A/ ;prints " N/A "


@END
		
;LOCAL VARIABLES
@START



@END

;SCREEN_BUILDER COMMAND TABLES
@START

;*Note: Must use .DB directive when writing bytes to a file using a label expression (i.e. .COMMAND_TABLE1.RECORDS.END-.COMMAND_TABLE1.RECORDS.START)
 
 
INV_2.COMMAND_TABLE1
@START


;COMMAND TABLE SIZE (bytes)
	.DB INV_2.COMMAND_TABLE1.RECORDS.END-INV_2.COMMAND_TABLE1.RECORDS.START+1 ;+1 the byte quantity includes this byte that we are writing to the file with this directive. 
INV_2.COMMAND_TABLE1.RECORDS.START

.CT.SCREEN_TITLE
@START
;UPDATE SCREEN POSITION
	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$0D ;set HTAB, VTAB
	;command parameter fields (PRC)	
	.DA #INV_2.SCREEN2.TITLE.HTAB	
	.DA #INV_2.SCREEN2.TITLE.VTAB
		 

;SCREEN TITLE

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$00 ;local ascii table, PRINT.STR

	;command parameter fields (PRC)
	;local ascii table
INV_2.SCREEN2_TITLE				.AZ	-/COMBAT STATS/



	
@END		
.CT.MODIFIED_DAMAGE
@START
;UPDATE SCREEN POSITION
	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$0F ;reset HTAB, VTAB+2
	
;HEADING

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$00 ;local ascii table, PRINT.STR

	;command parameter fields (PRC)
	;local ascii table
INV_2.HEADING.MODIFIED_DAMAGE	.AZ	-/MODIFIED DAMAGE/

	
;LEFT HAND
@START
;UPDATE SCREEN POSITION
	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$0E ;reset HTAB, VTAB+1
	
;LABEL

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$00 ;local ascii table, PRINT.STR

	;command parameter fields (PRC)
	;local ascii table
INV_2.LABEL.LHAND				.AZ -/Left Hand/

;PRINT VARIABLE

	;UPDATE SCREEN POSITION

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$0D ;set HTAB, VTAB
	;command parameter fields (PRC)	
	.DA #INV_2.SCREEN2.LHAND.VARIABLE.HTAB	
	.DA #INV_2.SCREEN2.LHAND.VARIABLE.VTAB

.CORNER_CASE.LHAND
	;PRINT "N/A" (if variable = $00) or PRINT VARIABLE (if variable !$00)
	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$30 ;corner case: print external ascii table (if variable = $00) or print variable (if variable !$00)
			 ;PRC (print_routine_code) is ignore. PRINT.STR is used for the ascii table and PRINT.HEX8.RJ is used for the variable
	.DA #CHR_SHEET.PC.DMG.LHAND
	.DA /CHR_SHEET.PC.DMG.LHAND
	.DA #INV_2.TEXT_BLOCK.NA
	.DA /INV_2.TEXT_BLOCK.NA

@END

;RIGHT HAND
@START
;UPDATE SCREEN POSITION
	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$0E ;reset HTAB, VTAB+1
	
;LABEL

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$00 ;local ascii table, PRINT.STR

	;command parameter fields (PRC)
	;local ascii table
INV_2.LABEL.RHAND				.AZ -/Right Hand/

;PRINT VARIABLE

	;UPDATE SCREEN POSITION

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$0D ;set HTAB, VTAB
	;command parameter fields (PRC)	
	.DA #INV_2.SCREEN2.RHAND.VARIABLE.HTAB	
	.DA #INV_2.SCREEN2.RHAND.VARIABLE.VTAB

.CORNER_CASE.RHAND	
	;PRINT "N/A" (if variable = $00) or PRINT VARIABLE (if variable !$00)
	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$30 ;corner case: print external ascii table (if variable = $00) or print variable (if variable !$00)
			 ;PRC (print_routine_code) is ignore. PRINT.STR is used for the ascii table and PRINT.HEX8.RJ is used for the variable
	.DA #CHR_SHEET.PC.DMG.RHAND
	.DA /CHR_SHEET.PC.DMG.RHAND
	.DA #INV_2.TEXT_BLOCK.NA
	.DA /INV_2.TEXT_BLOCK.NA

@END


.CT.ARMOR_RATING
@START
;UPDATE SCREEN POSITION
	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$0F ;reset HTAB, VTAB+2
	
;LABEL

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$00 ;local ascii table, PRINT.STR

	;command parameter fields (PRC)
	;local ascii table
INV_2.LABEL.ARMOR_RATING		.AZ -/Armor Rating/

;VARIABLE

	;UPDATE SCREEN POSITION

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$0D ;set HTAB, VTAB
	;command parameter fields (PRC)	
	.DA #INV_2.SCREEN2.ARMOR.VARIABLE.HTAB	
	.DA #INV_2.SCREEN2.ARMOR.VARIABLE.VTAB
	
	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$15 ;variable, PRINT.HEX8.RJ
	;command parameter fields (PRC)
	.DA #CHR_SHEET.PC_MOB.ARMOR
	.DA /CHR_SHEET.PC_MOB.ARMOR

	
@END
		
	
.CT.RESIST_MAGIC
@START
;UPDATE SCREEN POSITION
	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$0E ;reset HTAB, VTAB+1
	
;LABEL

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$00 ;local ascii table, PRINT.STR

	;command parameter fields (PRC)
	;local ascii table
INV_2.LABEL.RESIST_MAGIC		.AZ -/Resist Magic/

;VARIABLE

	;UPDATE SCREEN POSITION

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$0D ;set HTAB, VTAB
	;command parameter fields (PRC)	
	.DA #INV_2.SCREEN2.RESIST.VARIABLE.HTAB	
	.DA #INV_2.SCREEN2.RESIST.VARIABLE.VTAB
	
	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$15 ;variable, PRINT.HEX8.RJ
	;command parameter fields (PRC)
	.DA #CHR_SHEET.PC_MOB.RESIST_MAGIC
	.DA /CHR_SHEET.PC_MOB.RESIST_MAGIC

	
@END

.CT.TO_HIT
@START
;UPDATE SCREEN POSITION
	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$0F ;reset HTAB, VTAB+2
	
;LABEL

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$00 ;local ascii table, PRINT.STR

	;command parameter fields (PRC)
	;local ascii table
INV_2.LABEL.TO_HIT				.AZ -/% TO-HIT/

;VARIABLE

	;UPDATE SCREEN POSITION

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$0D ;set HTAB, VTAB
	;command parameter fields (PRC)	
	.DA #INV_2.SCREEN2.TO_HIT.VARIABLE.HTAB	
	.DA #INV_2.SCREEN2.TO_HIT.VARIABLE.VTAB
	
	
	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$16 ;variable, PRINT.TEXT.WINDOW.BCD8
	;command parameter fields (PRC)
	.DA #CHR_SHEET.PC_MOB.TO_HIT
	.DA /CHR_SHEET.PC_MOB.TO_HIT

	
@END



@END

INV_2.COMMAND_TABLE1.RECORDS.END

INV_2.COMMAND_TABLE1.SIZE = INV_2.COMMAND_TABLE1.RECORDS.END-INV_2.COMMAND_TABLE1.RECORDS.START+1

;VERIFY COMMAND TABLE SIZE <= !256bytes
	.DO INV_2.COMMAND_TABLE1.SIZE>$100
	
	.ER	F,***INV_2.COMMAND_TABLE1 exceeds !256 bytes
	
	.FI ;END IF
@END


		
@END

