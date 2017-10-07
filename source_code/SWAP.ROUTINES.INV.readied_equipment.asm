; ============================================
; Copyright (C) 2016-2018. 6502 Workshop, LLC
; contact: mark@6502workshop.com
; ============================================


;************************INCLUDE FILE*****************************
;(do not assemble stand-alone.  )


;=====================INCLUDE FILE DOCUMENTATION====================================
;
;Include file to SWAP.ROUTINES.INVENTORY.ASM
;
;=================================================================================



INV_1.DISPLAY.READIED_EQUIPMENT	
@START


.INIT
; ;CLEAR INVENTORY TEXT SPACE
	; JSR INV.ERASE.INVENTORY_WINDOW.TEXT_SPACE ;foreground and background page /w flip


.INIT.STATS.SUB_MODULE
	JSR INV.REFRESH.ACTIVE_PLAYER.DATA ;reads character sheet record (from in-memory array) and readied equipment record (from in-memory array)
		

				; LDA #$01
				; STA TROUBLESHOOTING.HOOK
				
;###EXECUTE SCREEN BUILDER
@START

		;special parms (lookup table indexes that need custom bit manipulation)
		;<NONE>
		
		;standard parms
		LDA #$01 ;set HTAB of left margin of text space
		STA SCREEN_BUILDER.HTAB.LEFT
		LDA #INV_1.COMMAND_TABLE1
		STA SCREEN_BUILDER.COMMAND_TABLE.POINTER+$0
		LDA /INV_1.COMMAND_TABLE1
		STA SCREEN_BUILDER.COMMAND_TABLE.POINTER+$1		
	JSR SCREEN_BUILDER
	
@END
			
;.SCREEN_TILE
@START	
		; LDA #INV_1.SCREEN1_TITLE.HTAB
		; STA HTAB	
		; LDA #INV_1.SCREEN1_TITLE.VTAB	
		; STA VTAB
	; JSR	UPDATE.CHAR.POS

		; LDA #INV_1.SCREEN1_TITLE
		; STA STRING+$0
		; LDA /INV_1.SCREEN1_TITLE
		; STA STRING+$1
	; JSR PRINT.STR
@END


;.LH.WEAPON.NAME	
@START
; ;UPDATE SCREEN POSITION
	; JSR INV.TEXT_TOOLS.VTAB2	;;increment VTAB by 2, reset HTAB to #$01, and update screen position

; ;PRINT NAME
		; LDA #CHR_SHEET.PC.NAME.READIED_WP_LHAND				
		; STA STRING+$0		
		; LDA /CHR_SHEET.PC.NAME.READIED_WP_LHAND
		; STA STRING+$1
	; JSR PRINT.STR
		
@END

;.RH.WEAPON.NAME	
@START
; ;UPDATE SCREEN POSITION
	; JSR INV.TEXT_TOOLS.VTAB1 ;increment VTAB by 1, reset HTAB to #$01, and update screen position

; ;PRINT NAME
		; LDA #CHR_SHEET.PC.NAME.READIED_WP_RHAND				
		; STA STRING+$0		
		; LDA /CHR_SHEET.PC.NAME.READIED_WP_RHAND
		; STA STRING+$1
	; JSR PRINT.STR
		
@END


.ALL_OTHER_ITEMS
@START
	LDX #CHR_SHEET.READIED_EQUIP.TYPE.HEAD.OFFSET ;the left/right hand weapons names are already printed, so the index is started at the record after. 
.LOOP.PRINT.ITEM_NAME


.VALIDATE.ITERATION
;IS SKIN READIED TO THIS EQUIPMENT SLOT?
;(if yes, skip. Otherwise skin could print out multiple times; once for each "empty" armor equipment slot)
	INX ;advance to byte $01 of readied item record (item_ID)

	;ITEM_type isn't checked because it can be assumed to be type armor since these slots are only permitted to have armor readied. 
	;there would be a corner case with item_ID $00 in the misc_items table, except that slot is used by a spell and spells don't get readied 
	;via the readied equipment table.
	LDA CHR_SHEET.PC.READIED_EQUIP.RECORD.READ,X ;load item_ID
	BEQ .INCREMENT_INDEX

;UPDATE SCREEN POSITION
	JSR INV.TEXT_TOOLS.VTAB1 ;increment VTAB by 1, reset HTAB to #$01, and update screen position

;READ ITEM NAME
;(from master item table)

		DEX ;back up to byte $00 of readied item record (item_type)

	
		LDA CHR_SHEET.PC.READIED_EQUIP.RECORD.READ,X ;load item_type
		PHA ;save item type
		INX ;advance to byte $01 of readied item record (item_ID)
		LDA CHR_SHEET.PC.READIED_EQUIP.RECORD.READ,X ;load item_ID
		STA FILE.ITEM_TABLE.ID
		PLA ;restore item_type	
		;ACC = item table code ($00 = weapon | $01 armor | $02 misc item)
	JSR FILE.READ.ITEM_TABLES.ENTRANCE
		;RETURN: FILE.ITEM_TABLE.RECORD.READ($20)

;PRINT NAME
		LDA #INV.ITEM_TABLE.ALL.NAME		
		STA STRING+$0		
		LDA /INV.ITEM_TABLE.ALL.NAME
		STA STRING+$1
	JSR PRINT.STR	
	
;INCREMENT INDEX
.INCREMENT_INDEX	
	INX ;next readied item record
	CPX #CHR_SHEET.PC.READIED_EQUIP.RECORD.SIZE
	BNE .LOOP.PRINT.ITEM_NAME
	
	
@END


.FOOTER
@START
		LDA #$B2 ;ASCII code for current screen number: 2
	JSR INV.TEXT_TOOLS.PRINT_FOOTER

@END


.EXIT

	JMP INV.STATS.STATE_LOOP
	
	


@END
		
		
;LOCAL SUBROUTINES
@START


@END

;LOCAL TEXT BLOCKS
@START
;INV_1.SCREEN1_TITLE	.AZ	-/READIED EQUIPMENT/

@END
		
;LOCAL VARIABLES
@START



@END


;SCREEN_BUILDER COMMAND TABLES
@START

;*Note: Must use .DB directive when writing bytes to a file using a label expression (i.e. .COMMAND_TABLE1.RECORDS.END-.COMMAND_TABLE1.RECORDS.START)
 
 
INV_1.COMMAND_TABLE1
@START


;COMMAND TABLE SIZE (bytes)
	.DB INV_1.COMMAND_TABLE1.RECORDS.END-INV_1.COMMAND_TABLE1.RECORDS.START+1 ;+1 the byte quantity includes this byte that we are writing to the file with this directive. 
INV_1.COMMAND_TABLE1.RECORDS.START

.CT.SCREEN_TITLE
@START
;UPDATE SCREEN POSITION
	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$0D ;set HTAB, VTAB
	;command parameter fields (PRC)	
	.DA #INV_1.SCREEN1_TITLE.HTAB	
	.DA #INV_1.SCREEN1_TITLE.VTAB
		 

;SCREEN TITLE

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$00 ;local ascii table, PRINT.STR

	;command parameter fields (PRC)
	;local ascii table
INV_1.SCREEN1_TITLE	.AZ	-/READIED EQUIPMENT/

	
	
@END		
		
.CT.LH.WEAPON.NAME
@START
;UPDATE SCREEN POSITION
	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$0F ;reset HTAB, VTAB+2
	
;LABEL

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$10 ;local ascii table, PRINT.STR

	;command parameter fields (PRC)
	.DA #CHR_SHEET.PC.NAME.READIED_WP_LHAND				;external ascii table
	.DA /CHR_SHEET.PC.NAME.READIED_WP_LHAND

@END
		
		
.CT.RH.WEAPON.NAME
@START
;UPDATE SCREEN POSITION
	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$0E ;reset HTAB, VTAB+1
	
;LABEL

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$10 ;local ascii table, PRINT.STR

	;command parameter fields (PRC)
	.DA #CHR_SHEET.PC.NAME.READIED_WP_RHAND				;external ascii table
	.DA /CHR_SHEET.PC.NAME.READIED_WP_RHAND

@END
		

INV_1.COMMAND_TABLE1.RECORDS.END

INV_1.COMMAND_TABLE1.SIZE = INV_1.COMMAND_TABLE1.RECORDS.END-INV_1.COMMAND_TABLE1.RECORDS.START+1

;VERIFY COMMAND TABLE SIZE <= !256bytes
	.DO INV_1.COMMAND_TABLE1.SIZE>$100
	
	.ER	F,***INV_1.COMMAND_TABLE1 exceeds !256 bytes
	
	.FI ;END IF
@END


	
	
@END
