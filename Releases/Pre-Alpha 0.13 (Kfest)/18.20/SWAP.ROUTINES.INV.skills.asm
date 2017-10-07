;************************INCLUDE FILE*****************************
;(do not assemble stand-alone.  )


;=====================INCLUDE FILE DOCUMENTATION====================================
;
;Include file to SWAP.ROUTINES.INVENTORY.ASM
;
;=================================================================================



INV_3.DISPLAY.SKILLS
@START


.INIT
; ;CLEAR INVENTORY TEXT SPACE
	; JSR INV.ERASE.INVENTORY_WINDOW.TEXT_SPACE ;foreground and background page /w flip


.INIT.STATS.SUB_MODULE
	JSR INV.REFRESH.ACTIVE_PLAYER.DATA ;reads character sheet record (from in-memory array) and readied equipment record (from in-memory array)
		

.SCREEN_TILE
@START	
		LDA #INV_3.SCREEN3.TITLE.HTAB
		STA HTAB	
		LDA #INV_3.SCREEN3.TITLE.VTAB	
		STA VTAB
	JSR	UPDATE.CHAR.POS

		LDA #INV_3.SCREEN3_TITLE
		STA STRING+$0
		LDA /INV_3.SCREEN3_TITLE
		STA STRING+$1
	JSR PRINT.STR
@END


.LOAD.SCREEN.VARIABLES
@START
;=====================SUBROUTINE DOCUMENTATION===========================================================================
;
;Since the layout of this screen is very systematic, this section loads and stores in an array all of 
;the variables on the screen and the memory addreses of the ASCII text for the lables. 
;
;.LOOP.PRINT_SCREEN iterates through the array and prints the information in a systematic format, advancing down the screen 
;as it goes. 
;
;=============================================================================================================================



;MELEE
	LDA #INV_3.LABEL.MELEE
	STA INV_3.SCREEN_VARIABLE.ARRAY+$0
	LDA /INV_3.LABEL.MELEE
	STA INV_3.SCREEN_VARIABLE.ARRAY+$1
	
	LDA CHR_SHEET.PC.SKILL.MELEE
	STA INV_3.SCREEN_VARIABLE.ARRAY+$2
	LDA CHR_SHEET.PC.SKILL.MELEE.PROG
	STA INV_3.SCREEN_VARIABLE.ARRAY+$3
	LDA #$AA ;!!!PLACE HOLDER!!! (for the skill progress bucket size)
	STA INV_3.SCREEN_VARIABLE.ARRAY+$4

;RANGE
	LDA #INV_3.LABEL.RANGE
	STA INV_3.SCREEN_VARIABLE.ARRAY+$5
	LDA /INV_3.LABEL.RANGE
	STA INV_3.SCREEN_VARIABLE.ARRAY+$6
	
	LDA CHR_SHEET.PC.SKILL.RANGE
	STA INV_3.SCREEN_VARIABLE.ARRAY+$7
	LDA CHR_SHEET.PC.SKILL.RANGE.PROG
	STA INV_3.SCREEN_VARIABLE.ARRAY+$8
	LDA #$AA ;!!!PLACE HOLDER!!! (for the skill progress bucket size)
	STA INV_3.SCREEN_VARIABLE.ARRAY+$9

;CRITIAL HIT
	LDA #INV_3.LABEL.CRITICAL_HIT
	STA INV_3.SCREEN_VARIABLE.ARRAY+$A
	LDA /INV_3.LABEL.CRITICAL_HIT
	STA INV_3.SCREEN_VARIABLE.ARRAY+$B
	
	LDA CHR_SHEET.PC.SKILL.CRITICAL_HIT
	STA INV_3.SCREEN_VARIABLE.ARRAY+$C
	LDA CHR_SHEET.PC.SKILL.CRITICAL_HIT.PROG
	STA INV_3.SCREEN_VARIABLE.ARRAY+$D
	LDA #$AA ;!!!PLACE HOLDER!!! (for the skill progress bucket size)
	STA INV_3.SCREEN_VARIABLE.ARRAY+$E

;DODGE-PARRY
	LDA #INV_3.LABEL.DODGE_PARRY
	STA INV_3.SCREEN_VARIABLE.ARRAY+$F
	LDA /INV_3.LABEL.DODGE_PARRY
	STA INV_3.SCREEN_VARIABLE.ARRAY+$10
	
	LDA CHR_SHEET.PC.SKILL.DODGE
	STA INV_3.SCREEN_VARIABLE.ARRAY+$11
	LDA CHR_SHEET.PC.SKILL.DODGE.PROG
	STA INV_3.SCREEN_VARIABLE.ARRAY+$12
	LDA #$AA ;!!!PLACE HOLDER!!! (for the skill progress bucket size)
	STA INV_3.SCREEN_VARIABLE.ARRAY+$13

;LOCKPICKING
	LDA #INV_3.LABEL.LOCKPICKING
	STA INV_3.SCREEN_VARIABLE.ARRAY+$14
	LDA /INV_3.LABEL.LOCKPICKING
	STA INV_3.SCREEN_VARIABLE.ARRAY+$15
	
	LDA CHR_SHEET.PC.SKILL.LOCKPICKING
	STA INV_3.SCREEN_VARIABLE.ARRAY+$16
	LDA CHR_SHEET.PC.SKILL.LOCKPICKING.PROG
	STA INV_3.SCREEN_VARIABLE.ARRAY+$17
	LDA #$AA ;!!!PLACE HOLDER!!! (for the skill progress bucket size)
	STA INV_3.SCREEN_VARIABLE.ARRAY+$18

;PILFER
	LDA #INV_3.LABEL.PILFER
	STA INV_3.SCREEN_VARIABLE.ARRAY+$19
	LDA /INV_3.LABEL.PILFER
	STA INV_3.SCREEN_VARIABLE.ARRAY+$1A

	LDA CHR_SHEET.PC.SKILL.PILFER
	STA INV_3.SCREEN_VARIABLE.ARRAY+$1B
	LDA CHR_SHEET.PC.SKILL.PILFER.PROG
	STA INV_3.SCREEN_VARIABLE.ARRAY+$1C
	LDA #$AA ;!!!PLACE HOLDER!!! (for the skill progress bucket size)
	STA INV_3.SCREEN_VARIABLE.ARRAY+$1D
	
@END

;UPDATE SCREEN POSITION
	JSR INV.TEXT_TOOLS.VTAB1	;increment VTAB by 1, reset HTAB to #$01, and update screen position

	LDX #$00 ;init index to INV_3.SCREEN_VARIABLE.ARRAY
.LOOP.PRINT_SCREEN
@START

;UPDATE SCREEN POSITION
	JSR INV.TEXT_TOOLS.VTAB1	;increment VTAB by 1, reset HTAB to #$01, and update screen position

;PRINT LABEL
		LDA INV_3.SCREEN_VARIABLE.ARRAY+$0,X
		STA STRING+$0
		LDA INV_3.SCREEN_VARIABLE.ARRAY+$1,X
		STA STRING+$1
	JSR PRINT.STR
	

			
;PRINT SKILL

	;update screen position
	JSR INV_3.TEXT_TOOLS.HTAB_F			;increment HTAB to #$0F, and update screen position

			
	;print variable	
		LDA INV_3.SCREEN_VARIABLE.ARRAY+$2,X
		CLC ;(CLC = don't print CR | SEC = print CR to text window)
	JSR PRINT.TEXT.WINDOW.HEX8.RJ


;PRINT SKILL PROGRESS

	;update screen position	
	JSR INV_3.TEXT_TOOLS.VTAB1_HTAB9	;increment VTAB by $01, reset HTAB to #$09, and update screen position

	;print variable: skill progress
		LDA INV_3.SCREEN_VARIABLE.ARRAY+$3,X
		CLC ;(CLC = don't print CR | SEC = print CR to text window)
	JSR PRINT.TEXT.WINDOW.HEX8.RJ
		;**OPT** Memory. There are a lot of calls to PRINT.TEXT.WINDOW.HEX8.RJ and PRINT.TEXT.WINDOW.HEX8.LJ with CLC parm set (don't print CR). 
		;If there are no calls that use CLC maybe just hard code SEC in the routine. If there are a few then maybe create two entrances, one that
		;calls with SEC and one that calls with CLC
		
	;print slash seperator
	JSR INV_3.PRINT.SEPERATOR_SLASH

	;print variable: skill progress bucket size
	
	;**PLACE HOLDER**
	
		NOP ;simulate actual memory required of expected final code
		NOP
		NOP
		NOP
	JSR INV_3.PLACEHOLDER
	
	; ;***REPLACE PLACEHOLDER WITH THIS CODE WHEN READY***
	; ;print variable: skill progress
		; LDA INV_3.SCREEN_VARIABLE.ARRAY+$4,X
		; CLC ;(CLC = don't print CR | SEC = print CR to text window)
	; JSR PRINT.TEXT.WINDOW.HEX8.RJ
	

	TXA ;next INV_3.SCREEN_VARIABLE.ARRAY record
	CLC
	ADC #INV_3.SCREEN_VARIABLE.ARRAY.RECORD_SIZE	
	TAX
	
	CPX #INV_3.SCREEN_VARIABLE.ARRAY.SIZE
	BNE .LOOP.PRINT_SCREEN
	
@END

.FOOTER
@START
		LDA #$B4 ;ASCII code for current screen number: 4
	JSR INV.TEXT_TOOLS.PRINT_FOOTER

@END


.EXIT

	JMP INV.STATS.STATE_LOOP
	

@END
		
		
;LOCAL SUBROUTINES
@START

INV_3.TEXT_TOOLS.HTAB_F			;increment HTAB to #$0F, and update screen position
@START
;PARAMETERS: none
;ENTRANCE: any inventory sub_module
;RETURN: updated screen position

		LDA #INV_3.SCREEN3.SKILL_VARIABLE.HTAB
		STA HTAB
		;VTAB: use existing
	JSR	UPDATE.CHAR.POS	

	RTS

@END

INV_3.TEXT_TOOLS.VTAB1_HTAB9	;increment VTAB by 1, reset HTAB to #$09, and update screen position
@START
;PARAMETERS: none
;ENTRANCE: any inventory sub_module
;RETURN: updated screen position

		LDA #INV_3.SCREEN3.SKILL_PROGRESS_VARIABLE.HTAB
		STA HTAB
		INC VTAB
	JSR	UPDATE.CHAR.POS	

	RTS

@END

INV_3.PRINT.SEPERATOR_SLASH
@START	
		LDA #INV_3.TEXT_BLOCK.SEPERATOR
		STA STRING+$0
		LDA /INV_3.TEXT_BLOCK.SEPERATOR
		STA STRING+$1
	JSR PRINT.STR	
	
	RTS
@END
	
INV_3.PLACEHOLDER
@START

		LDA #$C1 ;A
	JSR COUT
		LDA #$C1 ;A
	JSR COUT
		LDA #$C1 ;A
	JSR COUT
	
	
	RTS
	
@END

	
@END

;LOCAL TEXT BLOCKS
@START
INV_3.SCREEN3_TITLE				.AZ	-/SKILLS/
INV_3.LABEL.MELEE				.AZ -/Melee/
INV_3.LABEL.RANGE				.AZ -/Range/
INV_3.LABEL.CRITICAL_HIT		.AZ -/Crtl Hit/
INV_3.LABEL.DODGE_PARRY			.AZ -/Dodge-Parry/
INV_3.LABEL.LOCKPICKING			.AZ -/Lockpicking/
INV_3.LABEL.PILFER				.AZ -/Pilfer/

;MISC
INV_3.TEXT_BLOCK.SEPERATOR	 	.AZ -/ /,#$AF,/ /

INV_3.SCREEN_VARIABLE.ARRAY.SIZE			.EQ	$1E
INV_3.SCREEN_VARIABLE.ARRAY.RECORD_SIZE		.EQ $05	;#CONSTANT
INV_3.SCREEN_VARIABLE.ARRAY					.BS INV_3.SCREEN_VARIABLE.ARRAY.SIZE



@END
		
;LOCAL VARIABLES
@START



@END
