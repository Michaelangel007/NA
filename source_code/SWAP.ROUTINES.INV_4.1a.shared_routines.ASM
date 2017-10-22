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



INV_4.1all.ROUTINES

;!!!TEMPORARY!!!!
INV_4.1a.ROUTINES
@START

;DATA
INV_4.1a.INIT.SCREEN_LIST.ARRAYS
@START

;init INV_4.1b.SCREEN_ITEM_LIST.ARRAY
;(for datagram see offloaded_variables.asm)
	LDX #$00
.INIT.LOOP
	LDA #$FF ;also used as array stop value (high-bit set)
	STA INV_4.1b.SCREEN_ITEM_LIST.ARRAY,X
	INX
	CPX #INV_4.1b.SCREEN_ITEM_LIST.ARRAY.ARRAY_SIZE
	BNE .INIT.LOOP

	LDA #$00
	STA INV_4.1b.SCREEN_ITEM_LIST.SELECTED.ITEM
	;STA INV_4.NEXT_ITEM.READIED_STATUS
	
	LDA #INV.PLAYER.INVENTORY.DATA.LESS_ONE_RECORD
	STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
	LDA /INV.PLAYER.INVENTORY.DATA.LESS_ONE_RECORD
	STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$1

	RTS
	
@END

INV_4.1a.GET_INDEX.SCREEN_ITEM_LIST.DATA.ALT_ENTRANCE
@START
;note: several calls use the same ACC parm. This alternate entrance allows them to just JSR
;here and skip setting the ACC parm.

	LDA INV_4.1b.SCREEN_ITEM_LIST.SELECTED.ITEM ;load item number (0-5) currently selected
	;**FALLS THROUGH**
	
INV_4.1a.GET_INDEX.SCREEN_ITEM_LIST.DATA ;primary entrance
@START
;PARAMETERS: ACC = item number
;ENTRANCE: any local routine
;RETURN: XREG = index to the record associated with the item number of SCREEN_ITEM_LIST.DATA passeds as the ACC parm.


;set INV_4.1a.SCREEN_ITEM_LIST.DATA index ($20 byte records)

	;ACC = parm: item number
	ASL ;X !2
	ASL ;X !4
	ASL ;X !8
	ASL ;X !16
	ASL ;X !32
	
;SET RETURN VALUE
	TAX 
	
.EXIT
	RTS
	
@END
@END


INV_4.1a.INIT.GET.ITEM.DATA
@START
;PARAMETERS: ACC = PARM: max onscreen items for either player or merchant inventory window, INV_4.1b.SCREEN_ITEM_LIST.ARRAY(x), INV_4.1a.SCREEN_ITEM_LIST.DATA(x)

;=====================CODE-SECTION DOCUMENTATION=======================================================
;
;This routine assembles all data needed for the first 6 items (or 3 when merchant interface is active) to be displayed on 
;the video screen, which is saved for future use when modifcations are made to onscreen 
;items that requires their data to be printed to the video screen again. 
;
;INV_4.1b.SCREEN_ITEM_LIST.ARRAY is iterated, for each item the item record is read from the master item table, 
;and the data is saved to INV_4.1a.SCREEN_ITEM_LIST.DATA. 
;
;=========================================================================================================

	;ACC = PARM: max onscreen items for either player or merchant inventory window
	STA INV_4.1b.MAX_ONSCREEN.ITEMS
	
	
	
	LDY #$00 ;init loop counter (on screen item counter)
.OUTER_LOOP.GET.ITEMS

;SAVE COUNTER/INDEXES
		STY INV_4.1b.GET_ITEMS.LOOP_COUNTER  ;SAVE loop counter
			
	;init indexes
	TYA
	ASL ;X2
	ASL ;X4
	TAX ;set INV_4.1b.SCREEN_ITEM_LIST.ARRAY index ($4 byte records)
	
	;exit test (no more items)
	LDA INV_4.1b.SCREEN_ITEM_LIST.ARRAY+$03,X ;load readied status field
	BMI .INIT.GET.ITEM.DATA.DONE ;branch if init value detected
	
	;init pointer to current item
	LDA INV_4.1b.SCREEN_ITEM_LIST.ARRAY+$0,X ;load LO byte of inventory record
	STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
	LDA INV_4.1b.SCREEN_ITEM_LIST.ARRAY+$1,X ;load HO byte of inventory record
	STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
	
	;read item record from master item file
		;(get parameters from the inventory record of current item)
		LDY #$01 ;set index to byte $1 (item_ID) of inventory record
		LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y
		STA FILE.ITEM_TABLE.ID
		DEY ;backup to byte $0 (LEVEL | ITEM_TYPE) of inventory record
		LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y
		;ITEM_TYPE is in LO nibble
		AND #$0F ;mask-out HO nibble. 
			;ACC = item table code ($00 = weapon | $01 armor | $02 misc item)
	JSR FILE.READ.ITEM_TABLES.ENTRANCE
		;RETURN: FILE.ITEM_TABLE.RECORD.READ($20)	


					; LDA #INV_4.1a.SCREEN_ITEM_LIST.DATA
					; STA $BE00					
					; LDA /INV_4.1a.SCREEN_ITEM_LIST.DATA
					; STA $BE01	
					; LDA #INV_4.1b.SCREEN_ITEM_LIST.ARRAY
					; STA $BE02					
					; LDA /INV_4.1b.SCREEN_ITEM_LIST.ARRAY
					; STA $BE03					
					; LDA #FILE.ITEM_TABLE.RECORD.READ
					; STA $BE04
					; LDA /FILE.ITEM_TABLE.RECORD.READ
					; STA $BE05
					; LDA #$AB
					; JSR PREP.BRK
					; BRK
					
	


					
;COPY DATA

	;init indexes
		LDA INV_4.1b.GET_ITEMS.LOOP_COUNTER  ;load loop counter
	JSR INV_4.1a.GET_INDEX.SCREEN_ITEM_LIST.DATA	;set INV_4.1a.SCREEN_ITEM_LIST.DATA index ($20 byte records)
			;RETURN: X-REG = INV_4.1a.SCREEN_ITEM_LIST.DATA index
		
					; LDA #$69
					; STA FILE.ITEM_TABLE.RECORD.READ+$0
					; LDA #$79
					; STA FILE.ITEM_TABLE.RECORD.READ+$1
					
	LDY #$00 ;init FILE.ITEM_TABLE.RECORD.READ index
.INNER_LOOP.COPY.DATA
	LDA FILE.ITEM_TABLE.RECORD.READ,Y
	
				;STA TEMP
				
	STA INV_4.1a.SCREEN_ITEM_LIST.DATA,X
				
				;STA TEMP16
				
				; STX $BE00
				; STY $BE01
				; LDA #FILE.ITEM_TABLE.RECORD.READ+$0
				; STA $BE02
				; LDA /FILE.ITEM_TABLE.RECORD.READ+$1
				; STA $BE03
				; LDA #$BB
				; ; LDX TEMP
				; ; LDY TEMP16
				; LDX #INV_4.1a.SCREEN_ITEM_LIST.DATA
				; LDY /INV_4.1a.SCREEN_ITEM_LIST.DATA
				; JSR PREP.BRK
				; BRK
				
	INX	;increment INV_4.1a.SCREEN_ITEM_LIST.DATA index
	INY ;increment FILE.ITEM_TABLE.RECORD.READ index
	CPY #FILE.ITEM_TABLE.RECORD.SIZE
	BNE .INNER_LOOP.COPY.DATA


			
;RESTORE COUNTER/INDEXES	
	LDY INV_4.1b.GET_ITEMS.LOOP_COUNTER  ;RESTORE loop counter

	

.EXIT_TEST ;(max items reached)
	INY ;increment loop counter
	CPY INV_4.1b.MAX_ONSCREEN.ITEMS
	BNE .OUTER_LOOP.GET.ITEMS
	
.INIT.GET.ITEM.DATA.DONE	

.EXIT
	
	RTS
@END

INV_4.1a.SAVE_ITEM 
@START
;PARAMETERS: X-REG (INV_4.1b.SCREEN_ITEM_LIST.ARRAY index), INV_4.1b.NEXT_ITEM.QTY_AVAILABLE, INV_4.NEXT_ITEM.READIED_STATUS, PLAYER_MERCH.INVENTORY.DATA.POINTER(2)
;ENTRANCE: .INIT.IDENTIFY_ITEMS
;RETURN: updated INV_4.1b.SCREEN_ITEM_LIST.ARRAY


.SAVE.ITEM
;SAVE ITEM DATA TO INV_4.1b.SCREEN_ITEM_LIST.ARRAY 
;INV_4.1b.SCREEN_ITEM_LIST.ARRAY (inventory table record # LO address, inventory table record # HO address, QTY_AVAILABLE, readied status)
	LDA PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
	STA INV_4.1b.SCREEN_ITEM_LIST.ARRAY+$0,X

	LDA PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
	STA INV_4.1b.SCREEN_ITEM_LIST.ARRAY+$1,X
	
	LDA INV_4.1b.NEXT_ITEM.QTY_AVAILABLE
	STA INV_4.1b.SCREEN_ITEM_LIST.ARRAY+$2,X
	
	LDA INV_4.NEXT_ITEM.READIED_STATUS
	STA INV_4.1b.SCREEN_ITEM_LIST.ARRAY+$3,X

.EXIT

	RTS
@END


;PRINT TEXT
INV_4.1a.PRINT.ALL.ITEMS ;supports player and merchant inventory windows
@START
;PARAMETERS: INV_4.1b.SCREEN_LIST.ITEM_ALL.HTAB.LEFT, INV_4.1b.MAX_ONSCREEN.ITEMS

;=====================SUBROUTINE DOCUMENTATION===========================================================================
;
;For each item on the video screen, prints both lines of item text to the background hi-res page. 
;The data to print is obtained by iterating through INV_4.1b.SCREEN_ITEM_LIST.ARRAY and INV_4.1a.SCREEN_ITEM_LIST.DATA
;
;=============================================================================================================================

.INIT.SCREEN_POSITION
		LDA #INV_4.1b.SCREEN_LIST.ITEM0.VTAB
		STA VTAB
		
		LDA INV_4.1b.SCREEN_LIST.ITEM_ALL.HTAB.LEFT
		STA HTAB
	JSR	UPDATE.CHAR.POS	
	

	LDY #$00 ;init loop counter (on screen item counter)
.LOOP.PRINT_ITEMS

	;init indexes
	TYA
	ASL ;X2
	ASL ;X4
	TAX ;set INV_4.1b.SCREEN_ITEM_LIST.ARRAY index ($4 byte records)
	
	;exit test (no more items)
	LDA INV_4.1b.SCREEN_ITEM_LIST.ARRAY+$03,X ;load readied status field
	BPL .INIT_DONE ;branch if init value not detected
	JMP .LOOP.PRINT_ITEMS.DONE ;JMP if init value detected

.INIT_DONE


.PRINT.BOTH_LINES ;OF ITEM TEXT
	JSR INV_4.1a.VIDEO_OUT.ITEM_LINE1_2
	

	;**OPT** Memory. It is possible that INV_4.1a.SCREEN_ITEM_LIST.DATA, which contains the entire $20 byte item record for each onscreen item,
			;can be reduced in size to just contain the item name and weight for each record since those are the only
			;fields currently in use. However, if we output the item stats to the scroll window then the other fields would be needed. 
	

			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; LDA TEMP
			; JSR FLIP.PAGE
			; JSR KEYIN
			; JSR FLIP.PAGE
			; ; LDX #$AA
			; ; JSR PREP.BRK
			; ; BRK
; .TEMP
			; LDA TEMP
			
			
			

.NEXT_ITEM
		LDA INV_4.1b.SCREEN_LIST.ITEM_ALL.HTAB.LEFT
		STA HTAB
		INC VTAB ;insert blank line before next item
		INC VTAB ;advance to next item
	JSR	UPDATE.CHAR.POS	
	
.EXIT_TEST ;(max items reached)
	INY ;increment loop counter
	CPY INV_4.1b.MAX_ONSCREEN.ITEMS
	BEQ .LOOP.PRINT_ITEMS.DONE ;branch if max items already printed onscreen 
	JMP .LOOP.PRINT_ITEMS ;else continue loop
	
.LOOP.PRINT_ITEMS.DONE

; ;SET FLAGS
	; LDA #$01
	; STA INV_4.FIRST.SCREEN_RENDERING ;($00 = first screen render for the INV_4 session, i.e. when inventory items are first printed onscreen, >=$01 not first screen render for the INV_4 session, i.e. state loop has been reached at least once)


.EXIT

			; LDA #INV_4.1a.SCREEN_ITEM_LIST.DATA
			; STA $BF00
			; LDA /INV_4.1a.SCREEN_ITEM_LIST.DATA
			; STA $BF01

			; LDA STRING
			; STA $BF02
			; LDA STRING+$1
			; STA $BF03
			
			; JMP DEBUG.HOOK

			
				; JSR FLIP.PAGE
				; JSR KEYIN
				; LDA #$AC
				; JSR PREP.BRK
				; BRK
				
	
	RTS


	
@END	

INV_4.1a.PRINT.SELECTED.ITEM ;(prints both lines of item text) ;supports player and merchant inventory windows
@START
;PARAMETERS: ACC ($00 = item selector OFF | $01 = item selector ON), INV_4.1b.SCREEN_ITEM_LIST.SELECTED.ITEM, carry flag (CLC = don't sync hi-res pages | SEC = sync hi-res pages), INV_4.1b.SCREEN_LIST.ITEM_ALL.HTAB.LEFT, INV_4.1b.SCREEN_LIST.ITEM_ALL.HTAB.RIGHT
;ENTRANCE:
;RETURN: video screen output

		
;=====================SUBROUTINE DOCUMENTATION===========================================================================
;
;This routine erases or draws the item selector (by plotting $00 to a rectangle encompassing both lines of text for the item) and then 
;prints the item text. Exception: if the item has been deleted in INV_4.1b.SCREEN_ITEM_LIST.ARRAY (i.e. because the player sold it) then
;this routine will erase or draw the item selector but exits before printing the item text. 
;
;
;-Misc notes
;
;*carry flag parm makes syncing pages optional so that mutiple calls can be done to this routine, presenting all changes to the background page at the same time. Otherwise a page flip
;would be needed after each call to this routine. 		
	
;=============================================================================================================================

			
.SAVE.PARAMETERS
		;ACC ($00 = item selector OFF | $7F = item selector ON) 
		STA COUT_CHAR_TYPE ;($00 = normal, $7F = inverse)


.SAVE.REGISTERS
	TXA
	PHA



	
.INIT
	;carry flag (CLC = don't sync hi-res pages | SEC = sync hi-res pages)
	BCC .SYNC.DONE

		;**OPT** Speed. Bypass this JSR if merchant transactions interface is active. It does a call to INV_8.SYNC_PAGES.MERCHANT_INV_WINDOW locally. 
	JSR INV_4.1a.SYNC_PAGES.PLAYER_INV_WINDOW ;sync player inventory window 
											;must come before INV_4.1a.VIDEO_OUT.ITEM_LINE1_2

	
.SYNC.DONE

	;get vtab	
	LDX INV_4.1b.SCREEN_ITEM_LIST.SELECTED.ITEM
	
	TXA ;save INV_4.1b.SCREEN_ITEM_LIST.SELECTED.ITEM
	PHA	

.ERASE.ITEM_SELECTOR
		;X-REG: item number to erase
		;INV_4.1b.SCREEN_LIST.ITEM_ALL.HTAB.RIGHT: already set above
		;COUT_CHAR_TYPE: already set above
		;INV_4.1b.SCREEN_LIST.ITEM_ALL.HTAB.LEFT: already set above
	JSR INV_4a.ERASE.ITEM_SELECTOR
	

.VALIDATE_ENTRANCE
;don't permit entrance any further into this routine if the INV_4.1b.SCREEN_ITEM_LIST.ARRAY record for the selected item is deleted. 
	LDA INV_4.1b.SCREEN_ITEM_LIST.SELECTED.ITEM
	;calculate INV_4.1b.SCREEN_ITEM_LIST.ARRAY index for active player
	ASL ;X2
	ASL ;X4
	TAX ;set index to INV_4.1b.SCREEN_ITEM_LIST.ARRAY for item to print
	LDA INV_4.1b.SCREEN_ITEM_LIST.ARRAY+$3,X ;load item readied status
	BPL .VALIDATE_ENTRANCE.DONE
	PLA ;pop INV_4.1b.SCREEN_ITEM_LIST.SELECTED.ITEM off the stack
	JMP .EXIT
.VALIDATE_ENTRANCE.DONE	
	

.PRINT.ITEM_TEXT		

		PLA ;restore INV_4.1b.SCREEN_ITEM_LIST.SELECTED.ITEM
		TAY ;transfer INV_4.1b.SCREEN_ITEM_LIST.SELECTED.ITEM
		;calculate INV_4.1b.SCREEN_ITEM_LIST.ARRAY index for active player
		ASL ;X2
		ASL ;X4
		TAX ;set index to INV_4.1b.SCREEN_ITEM_LIST.ARRAY for item to print
		;summary of parms set above for INV_4.1a.VIDEO_OUT.ITEM_LINE1_2: X-REG, Y-REG, INV_4.1b.SCREEN_ITEM_LIST.SELECTED.ITEM
	JSR INV_4.1a.VIDEO_OUT.ITEM_LINE1_2



		
.EXIT
;RESTORE REGISTERS
	PLA
	TAX

		

	RTS
	
@END
	 
INV_4.1a.VIDEO_OUT.ITEM_LINE1_2
@START
;PARAMETERS: VTAB, X-REG (index to INV_4.1b.SCREEN_ITEM_LIST.ARRAY for item to print), Y-REG = item number to print, [COUT_CHAR_TYPE ;($00 = normal, $7F = inverse)]
;ENTRANCE: INV_4.1a.PRINT.ALL.ITEMS, INV_4.PRINT.SELECTED.ITEM
;RETURN: video screen output

;=====================SUBROUTINE DOCUMENTATION===========================================================================
;
;
;-Spell Items: readied indicator and quantity
;
;This is a major cluge which arose due to spells having a different architecture for tracking the readied status
;of spell books. 
;
;If the loop detects that the player is in the spell menu:
;.SPELL_MENU.ACTIVE cycles through the array that stores the readied status for spell books, prints the readied indicator if readied
;and sets a readied flag that persists for the current loop iteration. 
;
;.PRINT.ITEM.QTY prints ones less than the quantity stored in INV_4.1b.SCREEN_ITEM_LIST.ARRAY+$3, which for spells is the owned quantity
;rather than the usual available quantity which doesn't include readied units, hence the need for the -1
;INV_4.1b.SCREEN_ITEM_LIST.ARRAY is not actually decremented by one permenantly becuase if we did that here it would happen as the player
;moved the item selector around. 
;It isn't practical to set quantity correctly to begin with in INV_4.CALCULATE.READIED_QTY.FIELDS 
;when it is called by INV_4.GET.NEXT_ITEM because a spell code from the item's master table record is needed to 
;determine if it is readied and during the inital screen rendering (.INIT.IDENTIFY_ITEMS) INV_4.1a.SCREEN_ITEM_LIST.DATA
;is not in memory yet, which is where the item master table records is stored. Doing it on the fly would require a disk load. 
;
;This is what leads to the final part of the cluge in INV_8.SELL_ITEM
;In order to ensure that a player can't sell a readied spellbook and that "Sell All" takes
;readied spell books into consideration, .SPELL_MENU.CHECK (.READIED_ITEM.CHECK) also checks to see if the spellbook
;is readied.	
;
;=============================================================================================================================

.INIT
	LDA COUT_CHAR_TYPE ;($00 = normal, $7F = inverse)
	STA COUT_CHAR_TYPE.SAVED


.PRINT.LINE1
@START
;SAVE REGISTERS
	TXA  ;this is a local save because X-REG contains the index for INV_4.1b.SCREEN_ITEM_LIST.ARRAY, which is needed in .PRINT.LINE2
	PHA
	
	;init indexes
	;(the reason X-REG isn't set to INV_4.1a.SCREEN_ITEM_LIST.DATA index at the top of the routine is because X-REG is sometimes expected to be the index to INV_4.1b.SCREEN_ITEM_LIST.ARRAY. FOr example, in print item QTY)
		TYA ;load item number to print
	JSR INV_4.1a.GET_INDEX.SCREEN_ITEM_LIST.DATA ;set INV_4.1a.SCREEN_ITEM_LIST.DATA index ($20 byte records)
		;RETURN: X-REG = INV_4.1a.SCREEN_ITEM_LIST.DATA index
		

		LDA INV_4.1b.SCREEN_LIST.ITEM_ALL.HTAB.LEFT
		STA HTAB
		;INC VTAB ;advance to 2nd text line of item
	JSR	UPDATE.CHAR.POS	

	
		;**OPT** Memory. Used this version of the "load item name for current item" once SBASM bug is fixed
		;which causes the LDAs below to use the wrong opcode (it uses immediate mode instead of immediate mode, indexed by X)
		; ;load item name for current item
		; LDA #INV_4.1a.SCREEN_ITEM_LIST.DATA+INV.ITEM_TABLE.ALL.NAME.OFFSET,X ;add the table base address and the offset to the item name field
		; STA STRING+$0
		; LDA /INV_4.1a.SCREEN_ITEM_LIST.DATA+INV.ITEM_TABLE.ALL.NAME.OFFSET,X
		; STA STRING+$1

				
		
		;load item name for current item
		TXA ;record index (for the onscreen item currently being processed)
		CLC
		ADC #INV_4.1a.SCREEN_ITEM_LIST.DATA+INV.ITEM_TABLE.ALL.NAME.OFFSET ;add the table base address and the offset to the item name field
		STA STRING+$0
		LDA /INV_4.1a.SCREEN_ITEM_LIST.DATA+INV.ITEM_TABLE.ALL.NAME.OFFSET
		ADC #$00 ;16-bit add
		STA STRING+$1		

				
		;restore COUT_CHAR_TYPE parm (might be set to $7F depending on the path taken to call this routine)
		LDA COUT_CHAR_TYPE.SAVED
		STA COUT_CHAR_TYPE ;($00 = normal, $7F = inverse)		
	JSR PRINT.STR.BACKGROUND

	
;RESTORE REGISTERS
	PLA
	TAX
			
.PRINT.LINE1.DONE
@END


	
.PRINT.LINE2
@START
		LDA INV_4.1b.SCREEN_LIST.ITEM_ALL.HTAB.LEFT
		STA HTAB
		;init here for convenience. above expected to be non-zero value
		STA INV_4.1b.SPELL_READIED.FLAG ;($00 = spell book item IS readied | !=$00 spell book item IS NOT readied)
		INC VTAB ;advance to 2nd text line of item
		
		;VTAB: use VTAB positon set by calling routine
	JSR	UPDATE.CHAR.POS	

		

;PRINT READIED INDICATOR
.PRINT.READIED_INDICATOR
@START
;(for non-spell menus, the readied indicator is * or ** to indicate the item is readied once or twice respectively by the active player
;for the spell menu, the readied indicator is *(x) where x is the spell hotkey number the spell is readied to.

.INV_4.MERCH_INTERFACE.BRANCH ;exit if call to this subroutine was from INV_8 (merchant transactions)
	LDA INV_4.1b.CALL_FROM	;($00 = main inventory interface active | $01 = merchant transactions interface active)
	BEQ .INV_4.MERCH_INTERFACE.BRANCH.DONE ;branch if no
	
	
		;X-REG (index to INV_4.1b.SCREEN_ITEM_LIST.ARRAY for item to print)
	JSR INV_8.PRINT.ITEM_LIST.PRICE
	
	
	JMP .PRINT.WT.DONE ;skip print for readied indicator, item quantity, and item weight
.INV_4.MERCH_INTERFACE.BRANCH.DONE

				
;BRANCH
	LDA INV.ACTIVE_MENU.CODE 
	CMP #INV.ACTIVE_MENU.SPELLS ;INV_4.1b.DISPLAY.MODE = $02 (misc items), but is spell menu active?
	BEQ	.SPELL_MENU.ACTIVE
	;**FALLS THROUGH**
.SPELL_MENU.NOT_ACTIVE
;VALIDATE ENTRANCE
	;is merchant transactions (MT) sub_module loaded in memory?
	LDA INV.SUB_MODULE.LOAD_FLAGS+$8 ;($00 = sub-module not loaded in memory | $01 = sub-module loaded in memory | high-bit set = initial launch complete)
	BEQ .VALIDATE.ENTRANCE.DONE ;branch if MT module is NOT loaded in memory.
	JSR INV_8.DISPLAY_READIED.INDICATOR ;MT mode displays one * per PC who has the item readied. This subroutine is located in INV_8 to reduce the memory required in this sub_module (INV_4)
	JMP .PRINT.READIED_INDICATOR.DONE
	
.VALIDATE.ENTRANCE.DONE

	
	;print readied indicator (* or **), if item is readied by active player
	LDA INV_4.1b.SCREEN_ITEM_LIST.ARRAY+$3,X ;load readied flag field of item list
	BEQ .PRINT.READIED_INDICATOR.DONE ;branch if item isn't readied at least once
		LDA #$AA 
	JSR INV_4.CALL.COUT.BACKGROUND		;print first *
	LDA INV_4.1b.SCREEN_ITEM_LIST.ARRAY+$3,X ;load readied flag field of item list
	CMP #$02 ;branch if item isn't readied twice
	BNE .PRINT.READIED_INDICATOR.DONE
		LDA #$AA 
	JSR INV_4.CALL.COUT.BACKGROUND		;print second *
	JMP .PRINT.READIED_INDICATOR.DONE

.SPELL_MENU.ACTIVE
@START
;=====================CODE-SECTION DOCUMENTATION==========================================================================
;
;Iterate PLAYER.HOTKEYS.SPELLS. If a hotkey value (spell code) matches the spell code of
;the current item, then it is readied and the loop index = the hot key number.
;
;===========================================================================================================================

	;save registers
	TXA
	PHA
	TYA
	PHA
	
	;init indexes/counters
	;(the reason X-REG isn't set to INV_4.1a.SCREEN_ITEM_LIST.DATA index at the top of the routine is because X-REG is sometimes expected to be the index to INV_4.1b.SCREEN_ITEM_LIST.ARRAY. FOr example, in print item QTY)
		TYA ;load item number to print
	JSR INV_4.1a.GET_INDEX.SCREEN_ITEM_LIST.DATA	;set INV_4.1a.SCREEN_ITEM_LIST.DATA index ($20 byte records)
		;RETURN: X-REG = INV_4.1a.SCREEN_ITEM_LIST.DATA index


.GET.SPELL_ITEM.READIED_STATUS		

	LDY #$00 ;init loop counter & PLAYER.HOTKEYS.SPELLS index
.LOOP.SPELL_MENU.IS.READIED	
	LDA PLAYER.HOTKEYS.SPELLS,Y
	CMP INV_4.1a.SCREEN_ITEM_LIST.DATA+INV.ITEM_TABLE.MI.SPELL_CODE.OFFSET,X ;load spell code field for current item
	BEQ .SPELL_MENU.PRINT.READIED_INDICATOR
	INY
	CPY #PLAYER.HOTKEYS.SPELLS.ARRAY_SIZE
	BNE .LOOP.SPELL_MENU.IS.READIED
	;if loop terminates without a match, then spell is not readied
	JMP .SPELL_MENU.PRINT.READIED_INDICATOR.DONE ;(skips printing readied indicator)
	
	
.SPELL_MENU.PRINT.READIED_INDICATOR			
	;print readied indicator: *(x), where X = the hotkey number the spell is readied to
	
	;Y-REG: the spell hotkey number to which the spell is readied
	TYA
	ORA #$B0 ;mask-in $B to the HO byte to convert the hot key number to the ASCII value that represents the hotkey number
	STA INV_4.TEXT_BLOCK.SPELL_READIED_INDICATOR+$2 ;save the hotkey ASCII code to the text block
	
		LDA #INV_4.TEXT_BLOCK.SPELL_READIED_INDICATOR
		STA STRING+$0
		LDA /INV_4.TEXT_BLOCK.SPELL_READIED_INDICATOR
		STA STRING+$1		
		;restore COUT_CHAR_TYPE parm (might be set to $7F depending on the path taken to call this routine)
		LDA COUT_CHAR_TYPE.SAVED
		STA COUT_CHAR_TYPE ;($00 = normal, $7F = inverse)		
	JSR PRINT.STR.BACKGROUND

	;set spell book item readied flag
	LDA #$00 ;there is a good reason that $00 is the set value.
	STA INV_4.1b.SPELL_READIED.FLAG ;($00 = spell book item IS readied | !=$00 spell book item IS NOT readied)
	
.SPELL_MENU.PRINT.READIED_INDICATOR.DONE

	;restore registers
	PLA
	TAY
	PLA
	TAX
	
.SPELL_MENU.ACTIVE.DONE
@END	
	
.PRINT.READIED_INDICATOR.DONE
@END	
	
;PRINT ITEM QTY
.PRINT.ITEM.QTY
@START

					
			; STA TEMP
			; ; LDA TROUBLESHOOTING.HOOK
			; ; CMP #$01
			; ; BNE .TEMP
			; lda INV_4.1b.DISPLAY.MODE ;($00 = display weapons | $01 = display armor | $02 = misc items)
			; cmp #$02
			; bne .temp
			; cpx #$04
			; bne .temp
			; stx $bf00
			; lda #$aa
			; ldx #INV_4.1b.SCREEN_ITEM_LIST.ARRAY
			; ldy /INV_4.1b.SCREEN_ITEM_LIST.ARRAY
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP
			
			
			
		LDA #INV_4.SCREEN_LIST.ITEM_ALL.QTY_HTAB
		STA HTAB
		;VTAB = keep current value
	JSR	UPDATE.CHAR.POS	
	
		LDA INV_4.1b.SCREEN_ITEM_LIST.ARRAY+$2,X ;load available quantity (total qty - total readied) for current item
	JSR CONVERT.HEX.8_TO_BCD.16
		;RETURN VALUE: BCD(2)
		
		;does QTY = $0?
		LDA BCD+$0
		CLC
		ADC BCD+$1
		BEQ .PRINT.DASH

.ALTERNATE.QTY.VALUE		
;ALTERNATE QTY VALUE
;if spell menu is active then deduct 1 from QTY if the spell is readied
				
				
;VALIDATE ENTRANCE
	;is spell menu active?
	LDA INV.ACTIVE_MENU.CODE 
	CMP #INV.ACTIVE_MENU.SPELLS ;INV_4.1b.DISPLAY.MODE = $02 (misc items), but is spell menu active?
	BNE	.ALTERNATE.QTY.VALUE.DONE
	; ;is this the first screen render for the INV_4 session?? 
	; LDA	INV_4.FIRST.SCREEN_RENDERING ;($00 = first screen render for the INV_4 session, i.e. when inventory items are first printed onscreen, >=$01 not first screen render for the INV_4 session, i.e. state loop has been reached at least once)
	; BNE .ALTERNATE.QTY.VALUE.DONE
	
;.SPELL_MENU.ACTIVE2
	;is spell book item readied? 
	LDA INV_4.1b.SPELL_READIED.FLAG ;($00 = spell book item IS readied | !=$00 spell book item IS NOT readied)
	BNE .ALTERNATE.QTY.VALUE.DONE
	
	;subtract 1 from QTY to exclude the spell book item that is readied
	DEC BCD+$0 ;subtract from the QTY that is about to print onscreen
	;DEC INV_4.1b.SCREEN_ITEM_LIST.ARRAY+$2,X ;load available quantity (total qty - total readied) for current item
	
	;**FALLS THROUGH**

.DEDUCT.QTY.DONE


	
		;does QTY = $0?
		LDA BCD+$0
		CLC
		ADC BCD+$1
		BEQ .PRINT.DASH
		
	
				; cpx #$04
				; bne .temp
				; lda #$aa
				; ldx bcd+$0
				; ldy bcd+$1
				; jsr prep.brk
				; brk
				
				
;.temp


.ALTERNATE.QTY.VALUE.DONE
	
		;**FALLS THROUGH**
		
		;PARM: BCD(2) was set as return value above

		;restore COUT_CHAR_TYPE parm (might be set to $7F depending on the path taken to call this routine)
		LDA COUT_CHAR_TYPE.SAVED
		STA COUT_CHAR_TYPE ;($00 = normal, $7F = inverse)		
		;COUT_CHAR_TYPE ($00 = normal, $7F = inverse) might be set to $7F depending on the path taken to call this routine
	JSR PRINT.BCD_PACKED.STR.BACKGROUND
		JMP .PRINT.QTY.DONE

		
.PRINT.DASH
		;Since QTY supresses zeros, the HTAB needs to be adjusted to print a - for the last digit)
		LDA #INV_4.SCREEN_LIST.ITEM_ALL.QTY_DASH_HTAB 
		STA HTAB
		;VTAB = keep current value
	JSR	UPDATE.CHAR.POS	
	
		LDA #$AD ;ASCII:'dash'
	JSR INV_4.CALL.COUT.BACKGROUND		;print second *

		;**FALLS THROUGH**

.PRINT.QTY.DONE
@END
	
;PRINT ITEM WEIGHT
.PRINT.ITEM.WEIGHT
@START
		LDA #INV_4.SCREEN_LIST.ITEM_ALL.WT_HTAB
		STA HTAB
		;VTAB = keep current value
	JSR	UPDATE.CHAR.POS	

	;init indexes
	;(the reason X-REG isn't set to INV_4.1a.SCREEN_ITEM_LIST.DATA index at the top of the routine is because X-REG is sometimes expected to be the index to INV_4.1b.SCREEN_ITEM_LIST.ARRAY. FOr example, in print item QTY)
		TYA ;load item number to print
	JSR INV_4.1a.GET_INDEX.SCREEN_ITEM_LIST.DATA ;set INV_4.1a.SCREEN_ITEM_LIST.DATA index ($20 byte records)
		;RETURN: X-REG = INV_4.1a.SCREEN_ITEM_LIST.DATA index
		
		;**OPT** Memory. turn this into a common subroutine (in inventory module), and replace all calls to PRINT.BCD_PACKED
		LDA INV_4.1a.SCREEN_ITEM_LIST.DATA+INV.ITEM_TABLE.ALL.STR_WEIGHT.OFFSET,X ;load item weight for current item
	JSR CONVERT.HEX.8_TO_BCD.16
		;RETURN VALUE: BCD(2)
	
		;PARM: BCD(2) was set as return value above
		
		;restore COUT_CHAR_TYPE parm (might be set to $7F depending on the path taken to call this routine)
		LDA COUT_CHAR_TYPE.SAVED
		STA COUT_CHAR_TYPE ;($00 = normal, $7F = inverse)		
	JSR PRINT.BCD_PACKED.STR.BACKGROUND
.PRINT.WT.DONE
	
.PRINT.LINE2.DONE
@END

@END

		
		

.EXIT
	RTS
	
@END
		
INV_4.1a.INIT.SCROLL_WINDOW
@START
;SAVE CURRENT TEXT WINDOW SETTINGS
;(if the inventory is launched from combat, we want to restore the combat text window upon exit). 

;copies TWF.LEFT_SBYTE, TWF.WIDTH, TWF.TOP_ROW, TWF.BOTTOM_ROW to corresponding .SAVED variables
	LDX #$00
.LOOP.COPY.TWF
	LDA TWF.LEFT_SBYTE,X
	STA TWF.LEFT_SBYTE.SAVED,X
	INX
	CPX #$04
	BNE .LOOP.COPY.TWF

	
;INIT SCROLL FUNCTION SECTION OF WINDOW	
		;INIT TEXT WINDOW BOUNDARIES (for the text itself, excluding borders)
		
.INV_4.MERCH_INTERFACE.BRANCH3 ;exit if merchant interface mode is active 	
	;is merchant transactions (MT) sub_module loaded in memory, and if so was the call to INV_4 from the .INIT_SCREEN code (initial launch)?
	LDA INV.SUB_MODULE.LOAD_FLAGS+$8 ;($00 = sub-module not loaded in memory | $01 = sub-module loaded in memory | high-bit set = initial launch complete)
	BNE .MERCH_INTERFACE.ACTIVE ;branch if MT module in memory		
		
		;**OPT** Memory. Instead of the above branch it would be more efficient if TWF.LEFT_SBYTE was set
		;by a variable, which was passed as a parm to INV_4 and set in the local init routine in INV_8.
		;the savings is the bytes to do the branch and the JMP below. 

		LDA #TWS.INVENTORY.SCROLL_WINDOW.LEFT_SBYTE	
		STA TWF.LEFT_SBYTE
		
		;SET TEXT WINDOW FUNCTION (TWF) CURSOR POSITION VARIABLES TO UPPER LEFT OF TEXT WINDOW
		LDA #TWS.INVENTORY.SCROLL_WINDOW.CURSOR_START_SBYTE
		STA TWF.HTAB.SAVED
		STA HTAB

		LDA #TWS.INVENTORY.SCROLL_WINDOW.WIDTH
		STA TWF.WIDTH
		
		JMP .SET.COMMON.DONE	
.MERCH_INTERFACE.ACTIVE
		JSR INV_8.DRAW_ERASE.SCROLL_WINDOW
		
		LDA #TWS.MERCHANT_TRANS.SCROLL_WINDOW.LEFT_SBYTE	
		STA TWF.LEFT_SBYTE	

		;SET TEXT WINDOW FUNCTION (TWF) CURSOR POSITION VARIABLES TO UPPER LEFT OF TEXT WINDOW
		LDA #TWS.MERCHANT_TRANS.SCROLL_WINDOW.CURSOR_START_SBYTE
		STA TWF.HTAB.SAVED
		STA HTAB

		LDA #TWS.MERCHANT_TRANS.SCROLL_WINDOW.WIDTH
		STA TWF.WIDTH
		
		;**FALLS THROUGH**
		
.SET.COMMON.DONE		
		

		
		LDA #TWS.INVENTORY.SCROLL_WINDOW.TOP_ROW
		STA TWF.TOP_ROW
		
		LDA #TWS.INVENTORY.SCROLL_WINDOW.BOTTOM_ROW+1
		STA TWF.BOTTOM_ROW

	

			
		LDA #TWS.INVENTORY.SCROLL_WINDOW.CURSOR_START_ROW
		STA TWF.VTAB.SAVED	
		STA VTAB
	JSR	UPDATE.CHAR.POS





	RTS
@END

INV_4.1a.RESTORE.TW.CURSOR
@START	
;SET CURSOR IN START POSITION	
		
		;RESTORE SCROLL TEXT WINDOW CURSOR POSITION
		LDA TWF.HTAB.SAVED
		STA HTAB
		
		LDA TWF.VTAB.SAVED		
		STA VTAB
	JSR	UPDATE.CHAR.POS
	
	RTS
@END
	
INV_4.1a.SAVE.TW.CURSOR
@START		
	;SAVE SCROLL TEXT WINDOW CURSOR POSITION
	LDA HTAB
	STA TWF.HTAB.SAVED
	
	LDA VTAB
	STA TWF.VTAB.SAVED		
		
	RTS
@END


;GRAPHICS
INV_4a.ERASE.ITEM_SELECTOR ;effectively erases the entire text for an item 
@START
;PARAMETERS: X-REG: item number to erase, COUT_CHAR_TYPE ;($00 = normal, $7F = inverse), INV_4.1b.SCREEN_LIST.ITEM_ALL.HTAB.LEFT, INV_4.1b.SCREEN_LIST.ITEM_ALL.HTAB.RIGHT
 
 
	LDA INV_4.1a.SCREEN_LIST_ITEM.LOOKUP_TABLE.VTAB,X
	STA VTAB ;set VTAB of item to print

	
;SET SCREEN POSITION OF SELECTOR
	;ACC = VTAB of selected item
	ASL ;X2
	ASL ;X4
	ASL ;X8 (8 lines per character)
	
	
	


			

			
	TAX ;set start line
	CLC
	ADC #$10 ;add 2 character depths
	STA DRAW.STOP_LINE
	
	LDY INV_4.1b.SCREEN_LIST.ITEM_ALL.HTAB.LEFT ;set start SBYTE

	
	
					;STX COW
				

				
;SET BYTE VALUE TO DRAW
	LDA COUT_CHAR_TYPE ;($00 = normal, $7F = inverse)
	BNE .SET.SELECTOR_ON
	LDA #$00 ;draws black
	JMP .SAVE.DRAW_BYTE
.SET.SELECTOR_ON	
	LDA #$7F ;draws white
	
	;**FALLS THROUGH**
	
.SAVE.DRAW_BYTE
	;ACC = draw byte value
	STA DRAW.BYTE_VALUE.HORIZONTAL+$0

.DRAW	
.LINE.LOOP
		;X=REG: start line
		LDA PAGE.BACKGROUND
	JSR GET.LINE.ADDRESS1
		;RETURN VALUE: LINE.BASE.ADDR1(2)
.PLOT.LOOP
	LDA DRAW.BYTE_VALUE.HORIZONTAL+$0
	STA (LINE.BASE.ADDR1), Y
	INY
	CPY INV_4.1b.SCREEN_LIST.ITEM_ALL.HTAB.RIGHT ;contains the .RIGHT value +1
	BNE .PLOT.LOOP
	LDY INV_4.1b.SCREEN_LIST.ITEM_ALL.HTAB.LEFT ;reset SBYTE to line start position
	INX	
	CPX DRAW.STOP_LINE
	BNE .LINE.LOOP

	
.EXIT


			

	RTS
	
@END


INV_4.1a.SYNC_PAGES.PLAYER_INV_WINDOW 
@START
	LDA #$01
	STA DRAW.START_BYTE

	LDA #$13
	STA DRAW.STOP_BYTE.ALT
	
	;**FALLS THROUGH** (to INV_4.1a.SYNC_PAGES)
@END
	
INV_4.1a.SYNC_PAGES ;Sync player or merchant inventory window, depending on the parms set by the calling routine. 
@START
;PARAMETERS: DRAW.START_BYTE, DRAW.STOP_BYTE


;=====================SUBROUTINE DOCUMENTATION====================================
;
;Copies the foreground page to the background page for the inventory text window only. 
;
;=================================================================================

;SAVE REGISTERS	
	TXA
	PHA
	TYA
	PHA

.INIT.SCREEN					;setup screen byte, line, depth	

	LDA #$20
	STA LINE.START	
	
	LDA #$B8
	STA LINE.STOP				

		LDX LINE.START			;hi-res line number	
.LINELOOP	
	LDY DRAW.START_BYTE			;reset screen byte index to the left edge of text window	
	
.GET.LINE.ADDRESS
		;X-REG: hi-res line number
		LDA PAGE.FOREGROUND
	JSR GET.LINE.ADDRESS1
		LDA PAGE.BACKGROUND
	JSR GET.LINE.ADDRESS2

	
.COPYLOOP
;COPY TEXT CHARACTER DATA
	LDA (LINE.BASE.ADDR1),Y		;COPY FROM: foreground page	
	STA (LINE.BASE.ADDR2),Y		;COPY TO: background page
	
	INY							;advance to next screen byte

	CPY DRAW.STOP_BYTE.ALT		;is line complete? (i.e. have all screen bytes in the line been scrolled?)
	BNE .COPYLOOP
	
	INX
	CPX LINE.STOP				;has end of inventory window been reached?
	BCC .LINELOOP				;if no, scroll next line
	
	;**FALLS THROUGH**

.SCROLL.COMPLETE


;RESTORE REGISTERS	
	PLA
	TAY
	PLA
	TAX
	
	RTS
	
@END


@END



;INV_4 LOCAL VARIABLES
@START

INV_4.1a.SCREEN_ITEM_LIST.DATA.RECORD_SIZE .EQ $20	;#CONSTANT
INV_4.1a.SCREEN_ITEM_LIST.DATA.ARRAY_SIZE 	.EQ $C0	;#CONSTANT
INV_4.1a.SCREEN_ITEM_LIST.DATA .BS INV_4.1a.SCREEN_ITEM_LIST.DATA.ARRAY_SIZE 
;datagram: (contains (6) $20 byte records, each of which has the datagram of the master item record loaded into it, i.e. weapons table, armor table, etc.)

INV_4.1a.SCREEN_LIST_ITEM.LOOKUP_TABLE.VTAB .HS 06.09.0C.0F.12.15


@END

