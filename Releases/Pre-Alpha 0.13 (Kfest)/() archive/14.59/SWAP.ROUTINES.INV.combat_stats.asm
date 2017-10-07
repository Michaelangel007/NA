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
		
		
		
.SCREEN_TILE
@START	
		LDA #INV_2.SCREEN2.TITLE.HTAB
		STA HTAB	
		LDA #INV_2.SCREEN2.TITLE.VTAB	
		STA VTAB
	JSR	UPDATE.CHAR.POS

		LDA #INV_2.SCREEN2_TITLE
		STA STRING+$0
		LDA /INV_2.SCREEN2_TITLE
		STA STRING+$1
	JSR PRINT.STR
@END



.MODIFIED_DAMAGE
@START
;UPDATE SCREEN POSITION
	JSR INV.TEXT_TOOLS.VTAB2	;;increment VTAB by 2, reset HTAB to #$01, and update screen position

	
;PRINT HEADING
		LDA #INV_2.HEADING.MODIFIED_DAMAGE
		STA STRING+$0
		LDA /INV_2.HEADING.MODIFIED_DAMAGE
		STA STRING+$1
	JSR PRINT.STR


;-------LEFT/RIGHT HAND---------
;UPDATE SCREEN POSITION
	JSR INV.TEXT_TOOLS.VTAB1	;;increment VTAB by 2, reset HTAB to #$01, and update screen position

;LEFT HAND
;PRINT LABEL
		LDA #INV_2.LABEL.LHAND
		STA STRING+$0
		LDA /INV_2.LABEL.LHAND
		STA STRING+$1
	JSR PRINT.STR
	
	
;PRINT VARIABLE
		LDA #INV_2.SCREEN2.LHAND.VARIABLE.HTAB
		STA HTAB	
		LDA #INV_2.SCREEN2.LHAND.VARIABLE.VTAB	
		STA VTAB
	JSR	UPDATE.CHAR.POS
	
			;**OPT** memory. This section might not be needed for left hand because, if I recall, the left hand is never empty. If one weapon is readied it is always readied to left hand and if no hand-held weapons are readied then fists are readied to left hand. 
			;But, these assumptions needed to be verified. 
			
	;Print "N/A" if no weapon is readied in this hand
	LDA CHR_SHEET.PC.DMG.LHAND
	BNE .LHAND.EXECUTE_PRINT ;if modified damage = $00 then no weapon is readied. If fists are readied then modified damage = STR modified
	
	;PRINT N/A		
	JSR INV_2.PRINT.N_A

	JMP .LHAND.EXECUTE_PRINT.DONE
	
.LHAND.EXECUTE_PRINT	
		LDA CHR_SHEET.PC.DMG.LHAND
		CLC ;(CLC = don't print CR | SEC = print CR to text window)
	JSR PRINT.TEXT.WINDOW.HEX8.RJ
.LHAND.EXECUTE_PRINT.DONE

	
;RIGHT HAND
;UPDATE SCREEN POSITION
	JSR INV.TEXT_TOOLS.VTAB1	;;increment VTAB by 1, reset HTAB to #$01, and update screen position

;PRINT LABEL
		LDA #INV_2.LABEL.RHAND
		STA STRING+$0
		LDA /INV_2.LABEL.RHAND
		STA STRING+$1
	JSR PRINT.STR
	
	
;PRINT VARIABLE
		LDA #INV_2.SCREEN2.RHAND.VARIABLE.HTAB
		STA HTAB	
		LDA #INV_2.SCREEN2.RHAND.VARIABLE.VTAB	
		STA VTAB
	JSR	UPDATE.CHAR.POS

	;Print "N/A" if no weapon is readied in this hand
	LDA CHR_SHEET.PC.DMG.RHAND
	BNE .RHAND.EXECUTE_PRINT ;if modified damage = $00 then no weapon is readied. If fists are readied then modified damage = STR modified
	
	;PRINT N/A	
	JSR INV_2.PRINT.N_A

	JMP .RHAND.EXECUTE_PRINT.DONE
	
.RHAND.EXECUTE_PRINT	
		LDA CHR_SHEET.PC.DMG.RHAND
		CLC ;(CLC = don't print CR | SEC = print CR to text window)
	JSR PRINT.TEXT.WINDOW.HEX8.RJ
.RHAND.EXECUTE_PRINT.DONE
	
@END

.ARMOR_RATING
@START
;UPDATE SCREEN POSITION
	JSR INV.TEXT_TOOLS.VTAB2	;;increment VTAB by 2, reset HTAB to #$01, and update screen position

;PRINT LABEL
		LDA #INV_2.LABEL.ARMOR_RATING
		STA STRING+$0
		LDA /INV_2.LABEL.ARMOR_RATING
		STA STRING+$1
	JSR PRINT.STR
	
	
;PRINT VARIABLE
		LDA #INV_2.SCREEN2.ARMOR.VARIABLE.HTAB
		STA HTAB	
		LDA #INV_2.SCREEN2.ARMOR.VARIABLE.VTAB	
		STA VTAB
	JSR	UPDATE.CHAR.POS
	
		LDA CHR_SHEET.PC_MOB.ARMOR
		CLC ;(CLC = don't print CR | SEC = print CR to text window)
	JSR PRINT.TEXT.WINDOW.HEX8.RJ


@END

.RESIST_MAGIC
@START
;UPDATE SCREEN POSITION
	JSR INV.TEXT_TOOLS.VTAB1	;;increment VTAB by 2, reset HTAB to #$01, and update screen position

;PRINT LABEL
		LDA #INV_2.LABEL.RESIST_MAGIC
		STA STRING+$0
		LDA /INV_2.LABEL.RESIST_MAGIC
		STA STRING+$1
	JSR PRINT.STR
	
	
;PRINT VARIABLE
		LDA #INV_2.SCREEN2.RESIST.VARIABLE.HTAB
		STA HTAB	
		LDA #INV_2.SCREEN2.RESIST.VARIABLE.VTAB
		STA VTAB
	JSR	UPDATE.CHAR.POS
	
		LDA CHR_SHEET.PC_MOB.RESIST_MAGIC
		CLC ;(CLC = don't print CR | SEC = print CR to text window)
	JSR PRINT.TEXT.WINDOW.HEX8.RJ


@END

.TO_HIT
@START
;UPDATE SCREEN POSITION
	JSR INV.TEXT_TOOLS.VTAB2	;;increment VTAB by 2, reset HTAB to #$01, and update screen position

;PRINT LABEL
		LDA #INV_2.LABEL.TO_HIT
		STA STRING+$0
		LDA /INV_2.LABEL.TO_HIT
		STA STRING+$1
	JSR PRINT.STR
	
	
;PRINT VARIABLE
		LDA #INV_2.SCREEN2.TO_HIT.VARIABLE.HTAB
		STA HTAB	
		LDA #INV_2.SCREEN2.TO_HIT.VARIABLE.VTAB
		STA VTAB
	JSR	UPDATE.CHAR.POS
	
		; LDA CHR_SHEET.PC_MOB.TO_HIT
		; CLC ;(CLC = don't print CR | SEC = print CR to text window)
	; JSR PRINT.TEXT.WINDOW.HEX8.RJ
	
		LDA CHR_SHEET.PC_MOB.TO_HIT ;***this value is BCD format
	JSR PRINT.TEXT.WINDOW.BCD8	
	

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


INV_2.PRINT.N_A
@START
		LDA #INV_2.NOT_APPLICABLE
		STA STRING+$0
		LDA /INV_2.NOT_APPLICABLE
		STA STRING+$1
	JSR PRINT.STR

	RTS
@END


@END

;LOCAL TEXT BLOCKS
@START
INV_2.SCREEN2_TITLE				.AZ	-/COMBAT STATS/
INV_2.HEADING.MODIFIED_DAMAGE	.AZ	-/MODIFIED DAMAGE/
INV_2.LABEL.LHAND				.AZ -/Left Hand/
INV_2.LABEL.RHAND				.AZ -/Right Hand/
INV_2.LABEL.ARMOR_RATING		.AZ -/Armor Rating/
INV_2.LABEL.RESIST_MAGIC		.AZ -/Resist Magic/
INV_2.LABEL.TO_HIT				.AZ -/% TO-HIT/
INV_2.NOT_APPLICABLE			.AZ -/ N/,#$AF,/A/
@END
		
;LOCAL VARIABLES
@START



@END
