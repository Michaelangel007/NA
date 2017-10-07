;************************INCLUDE FILE*****************************
;(do not assemble stand-alone.  )


;=====================INCLUDE FILE DOCUMENTATION====================================
;
;Include file to SWAP.ROUTINES.INVENTORY.ASM
;
;=================================================================================


;=====================SUBROUTINE DOCUMENTATION====================================
;
;----OVERVIEW----
;This routine presents the data in PLAYER.INVENTORY.DATA to the player in a scrolling list
;with up to 6 items on the screen at a time.
;
;Filters are applied to the data in PLAYER.INVENTORY.DATA based on whether the Active PC is
;the weapons, armor, misc items, or spell menu is active and also based on whether the item
;is relevant to the player (i.e. are there units available for active PC to ready and does the
;active PC have the item readied......there is no reason to display an item which the player doesn't
;have readied and which there are no units available to ready because all units owned by the party are
;readied by players other than the Active PC)
;
;----SPELLS----
;The ready and unready scheme for spells is much different than for other items. 
;spells don't use the readied equipment table and readied status isn't stored in the
;PLAYER.INVENTORY.DATA readied flag fields. 
;
;Spells are really pretty simple. If a spell item is readied, it's spell code is stored as value in
;the PLAYER.HOTKEYS.SPELLS array. If a spell hotkey has the high-bit set then no spell is readied to it. 
;
;Instead of tracking the readied status in the inventory
;record for the item (PLAYER.INVENTORY.DATA), the readied status of an item is determined by iterating 
;PLAYER.HOTKEYS.SPELLS and comparing with the spell code field in the inventory record of a given item. 
;
;
;
;
;-----HI-RES Architecture----
;When the player moves the item selector off the screen, video memory screen scrolling is
;used to reposition the text on screen and the data for a new item is printed in the item
;slot at the top or bottom of the screen, depending on the direction of the scroll.
;
;The inventory window border and menu icons are draw on both pages because they are located
;partially of the tile grid. This is a consideration because COPY.SCREEN won't the hi-res 
;pages in the area off the tile grid. 
;
;All the text inside the inventory window is printed to the background page and then the 
;pages are flipped. 
;
;
;-Memory Management
;INV_8 (merchant transactions) is loaded into the memory occupied by INV_6 and above when
;the regular inventory interface is active. When the merchant transctive interface is 
;active, INV_4 is not loaded from disk until a keypress in the INV_8 state loop is processed 
;which passes control to INV_4
;
;INV_4 and INV_8 share some subroutines, which are divided into two groups:
;	INV_4.1a - subroutines needed by INV_8 to reach the state loop on first launch. 
;	INV_4.1b - subrotuines needed by INV_8 to process state loop commands. 
;
;The reason the routines are split into two groups is to minimize the disk load time. 
;INV_4.1a is loaded from disk during the INV_8 init. INV_4.1b is only loaded from disk 
;after a key is pressed in the state loop.
;
;INV_4 loads INV_4.1a and INV_4.1b during it's init as long as INV_8 isn't active.
;INV_4 also has some subroutines which are stored locally in INV_4 because they aren't needed by
;INV_8 (example: ready/unready weapons)
;
;INV_4 also has some local subroutines which aren't needed when the merchant interface is active. These are
;labeled as INV_4_GROUP2. The INV_4 local subroutines which are always needed (i.e. when the merchant interface
;or regular inventory interface is active, are located in a section called GROUP1 but the routine lables don't
;have this prefix, they are just INV_4, but they are the only routines whose prefix is just INV_4)
;
;INV_8 is loaded top of INV_4_GROUP2, which frees up almost an extra 2 pages of memory. To make this work,
;there is self-modifying code to modify the jump table used when sub modules are loaded 
;See INV.SUB_MODULE.FILE.PARAMETERS.INV_4 for more detailed documentation on this topic. 
;
;
;-Misc notes: 
;*The inventory text window is not setup for line wrapping. However, it is setup to
;scroll text up and down, from one item to the next within the layout. 
;
;*Once the initial 6 items are printed to the inventory window PLAYER_MERCH.INVENTORY.DATA.POINTER 
;is used when scrolling up/down to find the next relevant item. This works because the pointer
;is first set to the inventory record (PLAYER.INVENTORY.DATA) LO/HO memory address of the
;top or bottom item on-screen, depending on the direction of the scroll. 
;
;=================================================================================


;**OPT** Memory. I wonder if making spellbooks item_type = $03 would make sense. There is a whole bunch 
;of special code in INV_4 and INV_8 to identfy spell books when the spell menu is active. I really can't think
;of a benifit to having spell books lumped in with misc items. I may have done that originally because I was
;contemplating that the entire 256 record table for each item type would be used. However, once I decided that
;the player inventory would be limited to 256 record I effectively decided that would not be the case, and I 
;I don't it matters if the 256 items in the game are spread accross item_type 0-3 instead of 0-2. 


	
INV_4.DISPLAY_PLAYER_INVENTORY
@START
;PARAMETERS: INV_4.1b.DISPLAY.MODE= ($00 = display weapons | $01 = display armor | $02 = misc items), INV_4.ACTIVE_PLAYER
;			 INV.SUB_MODULE.LOAD_FLAGS+$8 ;($00 = standard mode | $01 = merchant transactions mode: init | high-bit set = merchant transactions mode: post-init)
	

		; LDA #$AA
		; JSR PREP.BRK
		; BRK
		

			
.INIT.READ.SHARED_SUBROUTINES ;(located here to share the merchant interface branch below with the scroll stuff)
@START
;(Merchant interface mode check not needed because INV_8.subroutines4.1all_FLAG is .EQ to INV_8.subroutines4.1b_FLAG, and
;the 4.1b flag is set before INV_4 is called by INV_8 and set after 4.1a is already in memory

	LDA INV_8.subroutines4.1all_FLAG		;($00 = not in memory| >=$01 = in memory).
	BNE .READ.SHARED_SUBROUTINES.DONE
	
			; LDA #$AA
			; JSR PREP.BRK
			; BRK
	
		;set destination memory address
		lda #INV_4.1all.ROUTINES
		sta parm.ldrlo
		lda /INV_4.1all.ROUTINES
		sta parm.ldrhi
		
		lda #INV_4.1all.SHARED_ROUTINES.SEEK_BYTES+$0
		sta PARM.SEEK_BYTES+$0	;seek length (LO byte).
		lda /INV_4.1all.SHARED_ROUTINES.SEEK_BYTES+$1
		sta PARM.SEEK_BYTES+$1	;seek length (HO byte).

		
		LDA #INV_4.1all.SHARED_ROUTINES.READ_BYTES
		sta PARM.READ_BYTES+$0		;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
		LDA /INV_4.1all.SHARED_ROUTINES.READ_BYTES
		sta PARM.READ_BYTES+$1		;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	JSR PRODOS.IO.READ_CURRENT_FILE
.READ.SHARED_SUBROUTINES.DONE

@END		
		

	

	;set call from = INV_4 (display player inventory)
	LDA #$00
	STA INV_4.1b.CALL_FROM	;($00 = subroutine called from INV_4, display player inventory | $01 = subroutine called from INV_8 merchant transactions)
	;STA INV_4.FIRST.SCREEN_RENDERING ;($00 = first screen render for the INV_4 session, i.e. when inventory items are first printed onscreen, >=$01 not first screen render for the INV_4 session, i.e. state loop has been reached at least once)

					; lda #$aa
					; jsr prep.brk
					; brk
					
;****INV DEBUG LOG*** 			;**OPT** Memory. Remove.
@START
.DEBUG.LABEL
		; ;PHA
		; LDA #.DEBUG.LABEL
		; STA INV.DEBUG.ADDRESS+$0
		; LDA /.DEBUG.LABEL
		; STA INV.DEBUG.ADDRESS+$1
	JSR INV.DEBUG.LOG.WRITE
		;;PLA
@END



			
.ERROR_TRAP
@START
;VALIDATE ENTRANCE: player inventory interface mode only
	LDA INV.SUB_MODULE.LOAD_FLAGS+$4 ;($00 = sub-module not loaded in memory | $01 = sub-module loaded in memory | high-bit set = initial launch complete)
	BNE .ERROR_TRAP.DONE 

	LDA INV_4.ACTIVE_PLAYER
	BEQ .ERROR.ACTIVE_PLAYER.OUT_OF_RANGE
	CMP #$07
	BCC .ERROR_TRAP.DONE
	
	;**FALLS THROUGH**
	
.ERROR.ACTIVE_PLAYER.OUT_OF_RANGE
;.ERROR_TRAP (INV_4.DISPLAY_PLAYER_INVENTORY) reports a value != 1-6 in INV_4.ACTIVE_PLAYER
	JSR PREP.BRK
	BRK



	
.ERROR_TRAP.DONE
@END

			
.INIT.INITIAL_LAUNCH_ONLY
@START
	LDA INV.SUB_MODULE.LOAD_FLAGS+$4 ;($00 = sub-module not loaded in memory | $01 = sub-module loaded in memory | high-bit set = initial launch complete)
	BMI .INIT.INITIAL_LAUNCH_ONLY.DONE
	
	;update flag /w initial launch complete
	;(so that above routine doesn't run again until the player exists the inventory module and reenters)
	ORA #$80 ;mask-in high-bit
	STA INV.SUB_MODULE.LOAD_FLAGS+$4 ;($00 = sub-module not loaded in memory | $01 = sub-module loaded in memory | high-bit set = initial launch complete)

	
	;;DEBUG: provides visual confirmation that this code section runs. 
;.DEBUG.PRINT_0 ;prints onscreen marker to track program flow. **OPT** Memory. Remove when resolved.  
@START
	; PHA ;SAVE ACC
	
		; LDA #$26
		; STA HTAB	
		; LDA #$01
		; STA VTAB
	; JSR	UPDATE.CHAR.POS
	
			; LDA #$B0
		; JSR COUT
		
	; PLA ;RESTORE ACC
@END
		
	
.INIT.INVENTORY.DATA
						
;****INV DEBUG LOG*** 			;**OPT** Memory. Remove.
@START
.DEBUG.LABEL2
		; ;PHA
		; LDA #.DEBUG.LABEL2
		; STA INV.DEBUG.ADDRESS+$0
		; LDA /.DEBUG.LABEL2
		; STA INV.DEBUG.ADDRESS+$1
	JSR INV.DEBUG.LOG.WRITE
		;;PLA
@END

						
						; lda #$aa
						; ldx #$aa
						; ldy #$aa
						; jsr prep.brk
						; brk
	

;READ PLAYER INVENTORY DATA FROM FILE
	JSR INV_4.1b.READ.PLAYER_INVENTORY.DATA
	

					
.INIT.INITIAL_LAUNCH_ONLY.DONE

	;;DEBUG: provides visual confirmation that the initial launch only code DIDN'T run. As expected. 
;.DEBUG.PRINT_1 ;prints onscreen marker to track program flow. **OPT** Memory. Remove when resolved.  
@START
	; PHA ;SAVE ACC
	
		; LDA #$26
		; STA HTAB	
		; LDA #$01
		; STA VTAB
	; JSR	UPDATE.CHAR.POS
	
			; LDA #$B1
		; JSR COUT
		
	; PLA ;RESTORE ACC
; .DEBUG.PRINT_1.DONE	
@END
	
	
@END
	

.INIT.STATS.SUB_MODULE
	JSR INV.REFRESH.ACTIVE_PLAYER.DATA ;reads character sheet record (from in-memory array) and readied equipment record (from in-memory array)
	


		
.INIT.SCREEN
@START
;CLEAR INVENTORY WINDOW TEXT SPACE
;(starting just below the border line below the menu icons.)
		LDA #TWS.LW.INVENTORY.TOP_LINE.TEXT_SPACE
		STA INV.ERASE.START_LINE
	JSR INV.ERASE.PLAYER_INV.TEXT_WINDOW
	JSR FLIP.PAGE
	JSR INV_4.1a.SYNC_PAGES.PLAYER_INV_WINDOW

@END
		
.INIT.SCREEN2
@START


.INIT.DRAW.SCROLL_TEXT_WINDOW
@START
;VALIDATE ENTRANCE
;(skip scroll window draw because dimensions are different when merchant transactions is active)
.INV_4.MERCH_INTERFACE.BRANCH  	
	;is merchant transactions (MT) sub_module loaded in memory, and if so was the call to INV_4 from the .INIT_SCREEN code (initial launch)?
	LDA INV.SUB_MODULE.LOAD_FLAGS+$8 ;($00 = sub-module not loaded in memory | $01 = sub-module loaded in memory | high-bit set = initial launch complete)
	BNE .INIT.DRAW.SCROLL_TEXT_WINDOW.DONE ;branch if MT module in memory (skip scroll window draw because dimensions are different when merchant transactions is active)




				
							
			; LDA #$AA
			; JSR PREP.BRK
			; BRK
			
;DRAW & ERASE SCROLL WINDOW (LOWER RIGHT)	
	JSR INV_4.DRAW_ERASE.SCROLL_WINDOW ;has validate entrance for merchant transaction mode

;INIT STATIC TEXT SECTION OF WINDOW
	JSR INV_4.GROUP2.PRINT.READIED_WEIGHT
	
	
;DRAW LINE: SEPERATOR BETWEEN SCROLL WINDOW AND STATIC TEXT (readied equip weight)
@START

	LDA #TWS.INVENTORY.SCROLL_WINDOW.SEPERATOR.LINE						;load line in x register	
	STA DRAW.START_LINE
	
	LDA #TWS.INVENTORY.SCROLL_WINDOW.LEFT_SBYTE
	STA DRAW.START_BYTE
	
	LDA #TWS.INVENTORY.SCROLL_WINDOW.RIGHT_SBYTE+1	
	STA DRAW.STOP_BYTE
		
	LDA #TWS.INVENTORY.SCROLL_WINDOW.SEPERATOR.LINE+1 ;(set to last line to draw + 1)
	STA DRAW.STOP_LINE
	
	LDA #$AA
	STA DRAW.BYTE_VALUE.HORIZONTAL+$0
	
	LDA #$D5
	STA DRAW.BYTE_VALUE.HORIZONTAL+$1
	;ACC = non-zero value expected
	;set 4.1b flag = in memory (**put here for convenience)
	STA INV_8.subroutines4.1b_FLAG		;($00 = not in memory| >=$01 = in memory).
	
.DRAW.LINE
;(when both hi-res pages need to stay in sync, instead of drawing to both pages, sometimes it is best to the pages individually
;and flip pages inbetween. This way the draw isn't as noticable to the user)
		
					; LDA #$01
					; STA TROUBLESHOOTING.HOOK
					
		LDA #$07 ;BIT0-2 set: use both hi-res pages. draw top line of rectangle. ignore all other bit flags.
	JSR DRAW.LINE



@END

.INIT.DRAW.SCROLL_TEXT_WINDOW.DONE
@END

.INIT_FUNCTION.SCROLL_TEXT_WINDOW
@START
;(merchant transactions mode uses the same scroll function dimensions, except for TWF.LEFT_SBYTE)



			
	JSR INV_4.1a.INIT.SCROLL_WINDOW
	

			
	
;PRINT INTRO TEXT (SCROLL WINDOW)
.PRINT.INTRO_TEXT
		LDA #INV_4.SCROLL_WINDOW.INTRO					
		STA TWF.STRING
		
		LDA /INV_4.SCROLL_WINDOW.INTRO
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW
.PRINT.INTRO_TEXT.DONE
	


			
	;**opt** Memory. Replace with JSR INV_4.1a.SAVE.TW.CURSOR
	;SAVE SCROLL TEXT WINDOW CURSOR POSITION
	; LDA HTAB
	; STA TWF.HTAB.SAVED
	
	; LDA VTAB
	; STA TWF.VTAB.SAVED	

	JSR INV_4.1a.SAVE.TW.CURSOR
		
@END

.INIT.SCREEN2.DONE


			
			
@END

.INIT.GENERAL
@START

	; LDA #$B7
	; STA INV_4.1b.HOTKEY_UPPER_PLUS1  ;#$B7 for main inventory mode, #$B8 for merchant transactions mode

	
	JSR INV_4.1a.INIT.SCREEN_LIST.ARRAYS ;init INV_4.1b.SCREEN_ITEM_LIST.ARRAY(x), INV_4.1b.SCREEN_ITEM_LIST.SELECTED.ITEM(x), and PLAYER_MERCH.INVENTORY.DATA.POINTER(2)
	


;get character sheet for active player
	;read PC character sheet data
		LDA INV_4.ACTIVE_PLAYER ;ACC = player sequential # (high-bit not set = read mode)
	JSR COMBAT.READ_WRITE.CHR_SHEET.PC
		;RETURN VALUE = CHR_SHEET.RECORD.READ




			
		
; ;TROUBLESHOOTING: PC CHARACTER SHEET READ
	; ;read PC character sheet data
		; LDA #$06
		; ;ACC = player sequential # (high-bit not set = read mode)
	; JSR COMBAT.READ_WRITE.CHR_SHEET.PC
		; ;RETURN VALUE = CHR_SHEET.RECORD.READ
; ;		
			; ;TROUBLESHOOTING LOOP
			; LDX #$00
; .TEST.LOOP6			
			; LDA CHR_SHEET.RECORD.READ,X
			; STA $BF00,X
			; INX
			; BNE .TEST.LOOP6
; ;
; ;
; ;
			; LDA #$EE
			; LDX #CHR_SHEET.PC.READIED_EQUIP
			; ldy /CHR_SHEET.PC.READIED_EQUIP
			; ;LDX FILE.OPEN.INVENTORY.DEBUG_COUNTER
			; JSR PREP.BRK
			; BRK


			
;###ok



					; lda #$ab
					; jsr prep.brk
					; brk
						
@END


.INIT.IDENTIFY_ITEMS
@START
;IF readied status for active player is true or QTY_AVAILABLE > 0
;	THEN add to INV_4.1b.SCREEN_ITEM_LIST.ARRAY and set byte 3 and 4 to QTY_AVAILABLE and readied status, set byte 5-6 to 16-bit inventory table record # address (i.e. save pointer)
	
	LDY #$00 ;init loop counter (on screen item counter)
	LDX #$00 ;init index to INV_4.1b.SCREEN_ITEM_LIST.ARRAY (4 byte records)
.LOOP.GET.ITEMS
				
				
				;STY COW ;DEBUG: save loop counter

					; LDA #$01
					; sta troubleshooting.hook
					
		LDA #$00 ;($00 = iterate down | $01 = iterate up)
	JSR INV_4.GET.NEXT_ITEM
		;RETURN VALUE: ACC ($00 = next item found, $01 = top or bottom of array reached), INV_4.1b.NEXT_ITEM.QTY_AVAILABLE, INV_4.NEXT_ITEM.READIED_STATUS, PLAYER_MERCH.INVENTORY.DATA.POINTER(2)
			
	;exit test
	CMP #$01 ;was top/bottom of array reached? (i.e. there is no next item)
	;BEQ .LOOP.GET.ITEMS.DONE.ALT ;branch if yes (the .ALT bypasses the DEY, which isn't needed because by exiting the loop here, the increment counter code at the end of each iteration isn't run)
	BEQ .LOOP.GET.ITEMS.DONE ;branch if yes
	
				; ; lda #INV_4.1b.SCREEN_ITEM_LIST.ARRAY
				; ; sta $be00
				; ; lda /INV_4.1b.SCREEN_ITEM_LIST.ARRAY
				; ; sta $be01
				; ; lda INV_4.NEXT_ITEM.READIED_STATUS
				; ; sta $be02
				; ; lda INV_4.1b.NEXT_ITEM.QTY_AVAILABLE
				; ; sta $be03
				; STA TEMP
				; CPY #$02
				; BNE .TEMP
				; ;LDA #$AA
				; ;LDX TEMP
				; LDA #$01
				; STA TROUBLESHOOTING.HOOK
				; ; LDX INV_4.1b.NEXT_ITEM.QTY_AVAILABLE ;if there are no units of the item available to be readied by the active player then there is no reason to display the item. 
				; ; JSR PREP.BRK
				; ; BRK
; .TEMP
				; LDA TEMP
	

.VALIDATE.ITEM
;should item be displayed onscreen?
	
	;is merchant transactions (MT) sub_module loaded in memory, and if so was the call to INV_4 from the .INIT_SCREEN code (initial launch)?
	LDA INV.SUB_MODULE.LOAD_FLAGS+$8 ;($00 = sub-module not loaded in memory | $01 = sub-module loaded in memory | high-bit set = initial launch complete)
	BNE .VALIDATE.ITEM.DONE ;branch if MT module is loaded in memory. automatically display all items
	
	LDA INV_4.NEXT_ITEM.READIED_STATUS ;($00 = not readied | $01 = readied by active player | $02 = readied twice by active player)
	BNE .VALIDATE.ITEM.DONE ;if item is readied by active player then item should be displayed
	LDA INV_4.1b.NEXT_ITEM.QTY_AVAILABLE ;if there are no units of the item available to be readied by the active player then there is no reason to display the item. 
	BEQ .LOOP.GET.ITEMS ;get next item. INV_4.GET.NEXT_ITEM will pick up at the position it left off at in the inventory table.  
.VALIDATE.ITEM.DONE

	;12 bytes _+ 6 = 18 (modify as is) * 3 = !54
	;subroutine = 18+6+3+1 = 28 + calling code = 3+3+3= 9*3 = 27. Total = 55
	
	
	;**FALLS THROUGH** 	;Since the party owns more of this item than is readied by the PCs, then the item should be displayed
	

		;save item
	
		;INV_4.1b.NEXT_ITEM.QTY_AVAILABLE: use existing value
		;INV_4.NEXT_ITEM.READIED_STATUS: use existing value
		;PLAYER_MERCH.INVENTORY.DATA.POINTER: use existing value
		;X-REG (INV_4.1b.SCREEN_ITEM_LIST.ARRAY index): use existing value 
	JSR INV_4.1a.SAVE_ITEM ;saves item if valid


	
	;**FALLS THROUGH**

.INCREMENT.COUNTERS_INDEXES
	INY ;increment loop counter (on screen item counter)
	TYA
	ASL ;X2
	ASL ;X4
	TAX ;save incremented index to INV_4.1b.SCREEN_ITEM_LIST.ARRAY (4 byte records)

	CPY #INV_4.MAX_ONSCREEN.ITEMS
	BNE .LOOP.GET.ITEMS
.LOOP.GET.ITEMS.DONE
	DEY ;reverse last loop counter increment so that INV_4.1b.SCREEN_LIST.TOTAL_RECORDS values will be 0-5
;.LOOP.GET.ITEMS.DONE.ALT
	STY INV_4.1b.SCREEN_LIST.TOTAL_RECORDS ;total items on screen. If > #INV_4.MAX_ONSCREEN.ITEMS that means less than #INV_4.MAX_ONSCREEN.ITEMS in the party inventory are relevant for display to the active player
	BPL .INIT.IDENTIFY_ITEMS.DONE ;branch if inventory isn't empty

;PLAYER INVENTORY IS EMPTY
;(refers to the player inventory containing no items with item_type = INV_4.1b.DISPLAY.MODE, the item_type filter)	
	
	;PRINT "NO ITEMS OWNED"
		LDA #$01
		STA HTAB
		LDA #$04
		STA VTAB
	JSR UPDATE.CHAR.POS
	
		LDA #INV_4.TEXT_BLOCKS.NO_ITEMS			
		STA STRING+$0
		
		LDA /INV_4.TEXT_BLOCKS.NO_ITEMS
		STA STRING+$1
	JSR PRINT.STR	
	
	JMP INV_4.STATE.LOOP.ALT.ENTRANCE ;(no need to print the item data on screen if there are no items)
.INIT.IDENTIFY_ITEMS.DONE

.INIT.SCREEN_ITEM_LIST

					
			; ; STA TEMP
			; ; LDA TROUBLESHOOTING.HOOK
			; ; CMP #$01
			; ; BNE .TEMP
			; lda INV_4.1b.DISPLAY.MODE ;($00 = display weapons | $01 = display armor | $02 = misc items)
			; cmp #$02
			; bne .temp
			; lda #$aa
			; ldx #INV_4.1b.SCREEN_ITEM_LIST.ARRAY
			; ldy /INV_4.1b.SCREEN_ITEM_LIST.ARRAY
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP
			
			
@END

 ;***OPT** Memory. Can be removed once bug is resolved. 
;.DEBUG_TRAP ;garabge data in inventory display (INV_4) 
@START
		; sta temp
		; lda INV.PLAYER.INVENTORY.DATA+$C
		; cmp #$80
		; bne .DEBUG_TRAP.REPORT_ERROR2
		; lda INV.PLAYER.INVENTORY.DATA+$D
		; cmp #$07
		; bne .DEBUG_TRAP.REPORT_ERROR2
		; lda INV.PLAYER.INVENTORY.DATA+$E
		; cmp #$01
		; bne .DEBUG_TRAP.REPORT_ERROR2
		; JMP .DEBUG_TRAP.NO_ERROR
		
; .DEBUG_TRAP.REPORT_ERROR2
; ;.DEBUG_TRAP (INV.FILE.READ.INVENTORY_DATA.ENTRANCE) reports that INV.PLAYER.INVENTORY.DATA does not contain
; ;the test bytes expected. This trap was created to track a bug causing garbage to be displyed in the inventory window. 
; ;based on some initial testing, when the bug occurs, the inventory data array ($BA00-$BFFF) contains all $00 values except for
; ;a portion of the $BD00 page. Thus, the theory is that when the bug occurs, the data in the file never makes it into memory for some reason.
	; LDA #$EE
	; LDX #$EE
	; LDY #$EE
	; JSR PREP.BRK
	; BRK
	
; .DEBUG_TRAP.NO_ERROR
		
		; lda temp
		
@END
;
;There is a different trap right after the file read. This trap is intended to detect a scenario where the data reads into memory from the file okay
;but gets corrputed later in the program flow. The location of this trap is somewhat aribtary. It's after the file read and before
;INV_4.STATE.LOOP.ALT.ENTRANCE. The bug is visible by INV_4.STATE.LOOP.ALT.ENTRANCE.



;####runaway path before this point			
			
.INIT.GET.ITEM.DATA
		;JMP DEBUG.HOOK
		LDA #INV_4.MAX_ONSCREEN.ITEMS
	JSR INV_4.1a.INIT.GET.ITEM.DATA


	
.INIT.MERCHANT_TRANSACTIONS_MODE_ONLY ;place holder in case such a section is needed.
@START
; .INV_4.MERCH_INTERFACE.BRANCH3  	
	; ;is merchant transactions (MT) sub_module loaded in memory, and if so was the call to INV_4 from the .INIT_SCREEN code (initial launch)?
	; LDA INV.SUB_MODULE.LOAD_FLAGS+$8 ;($00 = sub-module not loaded in memory | $01 = sub-module loaded in memory | high-bit set = initial launch complete)
	; BEQ .INIT.MERCHANT_TRANSACTIONS_MODE_ONLY.DONE ;branch if MT module in memory (skip scroll window draw because dimensions are different when merchant transactions is active)
	


	
.INIT.MERCHANT_TRANSACTIONS_MODE_ONLY.DONE
@END


			
	
			;JMP DEBUG.HOOK

;DEBUG.HOOK
@START
; ;DEBUG: move INV_4.1a.SCREEN_ITEM_LIST.DATA
				; LDX #$00
; .DEBUG.LOOP.INV_4.1b.SCREEN_ITEM_LIST.ARRAY
				; LDA INV_4.1b.SCREEN_ITEM_LIST.ARRAY,X
				; STA $BC00,X
				; INX
				; BNE .DEBUG.LOOP.INV_4.1b.SCREEN_ITEM_LIST.ARRAY


				; LDX #$00
; .DEBUG.LOOP.INV_4.1a.SCREEN_ITEM_LIST.DATA
				; LDA INV_4.1a.SCREEN_ITEM_LIST.DATA,X
				; STA $BD00,X
				; INX
				; BNE .DEBUG.LOOP.INV_4.1a.SCREEN_ITEM_LIST.DATA

				; LDX #$00
; .DEBUG.LOOP.FILE.ITEM_TABLE.RECORD.READ
				; LDA FILE.ITEM_TABLE.RECORD.READ,X
				; STA $BE00,X
				; INX
				; BNE .DEBUG.LOOP.FILE.ITEM_TABLE.RECORD.READ

				

				; LDA #INV_4.1b.SCREEN_ITEM_LIST.ARRAY
				; STA $BF01					
				; LDA /INV_4.1b.SCREEN_ITEM_LIST.ARRAY
				; STA $BF02					
				; LDA #INV_4.1a.SCREEN_ITEM_LIST.DATA
				; STA $BF03					
				; LDA /INV_4.1a.SCREEN_ITEM_LIST.DATA
				; STA $BF04	
				; LDA #FILE.ITEM_TABLE.RECORD.READ
				; STA $BF05
				; LDA /FILE.ITEM_TABLE.RECORD.READ
				; STA $BF06
				
				; LDA #$AB
				; LDX STRING+$0
				; LDY STRING+$1
				; JSR PREP.BRK
				; BRK
				
@END


.INIT.HEADERS

;QTY/WEIGHT COLUMN HEADERS
		LDA #INV_4.HEADER1.HTAB
		STA HTAB	
		LDA #INV_4.HEADER1.VTAB 
		STA VTAB
	JSR	UPDATE.CHAR.POS

		LDA #INV_4.TEXT_BLOCK.HEADER1
		STA STRING+$0
		LDA /INV_4.TEXT_BLOCK.HEADER1
		STA STRING+$1
	JSR PRINT.STR.BACKGROUND
	

			
INV_4.INIT.DISPLAY.ALL_ITEMS
	JSR INV_4.PRINT.ALL.ITEMS ;print all items visible on the video screen

	JSR FLIP.PAGE


	
		
.INIT.ACTIVE_ITEM_SELECTOR
	;LDA #$00
	LDA #$00
	STA INV_4.1b.SCREEN_ITEM_LIST.SELECTED.ITEM ;set item selector to item0 (first item)
	
@END

INV_4.STATE.LOOP
@START
;DISPLAY ITEM SELECTOR
		LDA #$7F ;set mode ($00 = item selector OFF | $7F = item selector ON) 
		;LDA #$00 ;set mode ($00 = item selector OFF | $7F = item selector ON)
		;INV_4.1b.SCREEN_ITEM_LIST.SELECTED.ITEM: use existing value
		SEC ;(CLC = don't sync hi-res pages | SEC = sync hi-res pages)
	JSR INV_4.PRINT.SELECTED.ITEM
	JSR FLIP.PAGE
				
.CHECK.DUAL_WIELDING.STATUS
	;load readied equipment record for active player
		LDA INV_4.ACTIVE_PLAYER ;load active PC number
	JSR INV.READ_WRITE_RECORD.CHAR_SHEET.READIED
		;RETURN VALUE: CHR_SHEET.PC.READIED_EQUIP.RECORD.READ($10)
	
	;parse readied equipment record
	LDA CHR_SHEET.READIED_EQUIP.TYPE.LHAND
	CMP #INV.ITEM_TYPE.NO_ATTACK_TURN
	BEQ .SINGLE_WIELDING
	LDA CHR_SHEET.READIED_EQUIP.TYPE.RHAND
	CMP #INV.ITEM_TYPE.NO_ATTACK_TURN
	BEQ .SINGLE_WIELDING
.DUAL_WIELDING
	LDA #$01
	STA INV_4.DUAL_WIELDING.FLAG ;($00 = single wielding | $01 = dual wielding)

	JMP .CHECK.DUAL_WIELDING.STATUS.DONE
.SINGLE_WIELDING
	LDA #$00
	STA INV_4.DUAL_WIELDING.FLAG ;($00 = single wielding | $01 = dual wielding)
.CHECK.DUAL_WIELDING.STATUS.DONE



	
;****INV DEBUG LOG*** 			;**OPT** Memory. Remove.
@START
.DEBUG.LABEL
		; ;PHA
		; LDA #.DEBUG.LABEL
		; STA INV.DEBUG.ADDRESS+$0
		; LDA /.DEBUG.LABEL
		; STA INV.DEBUG.ADDRESS+$1
	JSR INV.DEBUG.LOG.WRITE
		;;PLA
@END


			
	;**FALLS THROUGH**

INV_4.STATE.LOOP.ALT.ENTRANCE
@START

.KEYIN.LOOP
	LDA $C000
    BPL .KEYIN.LOOP
	STA $C010               ;CLR LAST KEY
		;ACC = keypress
	JSR CONVERT.ASCII.UCASE
		;RETURN VALUE: ACC = ASCII code (upper case, if the code is a letter)

.HOTKEY.CHECK ;test for keypress of 1-6 OR 2-5,7 (merchant transactions mode)
@START
.INV_4.MERCH_INTERFACE.BRANCH  	
	;is merchant transactions (MT) sub_module loaded in memory, and if so was the call to INV_4 from the .INIT_SCREEN code (initial launch)?
	PHA ;save keypress
	LDA INV.SUB_MODULE.LOAD_FLAGS+$8 ;($00 = sub-module not loaded in memory | $01 = sub-module loaded in memory | high-bit set = initial launch complete)
	BNE .MERCHANT_TRANSACTIONS.INTERFACE ;branch if MT module in memory (skip scroll window draw because dimensions are different when merchant transactions is active)
	
.MAIN_INVENOTRY.INTERFACE
;note: the ASCII codes refer to the key pressed not the menu code. The menu code = keypress -1		
	PLA ;load keypress
	CMP #$B1			
	BCC .HOTKEY.CHECK.DONE
	CMP #$B7 ;#$B7 for main inventory mode, #$B8 for merchant transactions mode
	BCC INV_4.EXIT_STEP ;exit, then goto the menu associated with the hotkey
	JMP .HOTKEY.CHECK.DONE

.MERCHANT_TRANSACTIONS.INTERFACE
;note: the ASCII codes refer to the key pressed not the menu code. The menu code = keypress -1	
	PLA ;load keypress
	CMP #$B6 ;in merchant trans interface mode, game settings isn't a valid menu
	BEQ .HOTKEY.CHECK.DONE
	CMP #$B2			
	BCC .HOTKEY.CHECK.DONE
	CMP #$B8 ;one greater than in .MAIN_INVENOTRY.INTERFACE mode to permit key 7 for the merchant inventory menu
	BCC INV_4.EXIT_STEP ;exit, then goto the menu associated with the hotkey

	;**FALLS THROUGH**
	
.HOTKEY.CHECK.DONE	
@END

	;ACC: keypress
	CMP #$89			;TAB (next menu)
	BEQ INV_4.EXIT_STEP

	CMP #$8B			;UP ARROW
	BEQ INV_4.COMMAND.CHANGE.ACTIVE_PLAYER.UP
	CMP #$8A			;DOWN ARROW
	BEQ INV_4.COMMAND.CHANGE.ACTIVE_PLAYER.DOWN	

	CMP #$9B			;ESC (EXIT COMBAT)
	BEQ	INV_4.EXIT_STEP
	
	;is inventory empty?
	LDX INV_4.1b.SCREEN_LIST.TOTAL_RECORDS ;(0-5 = item # of last item on screen | $FF = no inventory records are eligible for display)
	BMI INV_4.STATE.LOOP.ALT.ENTRANCE

	


	CMP #$88			;LEFT ARROW
	BEQ	INV_4.COMMAND.LEFT_ARROW
	CMP #$95			;RIGHT ARROW
	BEQ INV_4.COMMAND.RIGHT_ARROW
	CMP #$8D			;RETURN/ENTER (READY/UNREADY WEAPON)
	BEQ	INV_4.COMMAND.ENTER	;ready/unready item. Merchant interface active: sell item
	CMP #$C9
	BEQ INV_4.COMMAND.INFO	;(I)nfo on selected item
	CMP #$C2				;(B)uy item. Switches to merchant inventory window. **Merchant interfave active only**
	BEQ INV_4.BUY_ITEMS
	CMP #$D3				;(S)ell item   **Merchant interfave active only**
	BEQ INV_4.SELL_ITEMS	;(does the same thing as the enter key)
	
	
	;playtest keys
	CMP #$A8			;SHIFT+9 (SET HOOK)
	BEQ	INV_4.PLAYTEST.KEY_SHIFT9

	CMP #$D1			;(Q) QUIT
	BEQ	INV_4.COMMAND.QUIT	

	
	;default case
	JMP .KEYIN.LOOP ;do nothing, wait for next keypress


INV_4.SELL_ITEMS
.INV_4.MERCH_INTERFACE.BRANCH  	
	;is merchant transactions (MT) sub_module loaded in memory, and if so was the call to INV_4 from the .INIT_SCREEN code (initial launch)?
	LDA INV.SUB_MODULE.LOAD_FLAGS+$8 ;($00 = sub-module not loaded in memory | $01 = sub-module loaded in memory | high-bit set = initial launch complete)
	BEQ INV_4.STATE.LOOP.ALT.ENTRANCE ;branch if MT module is not in memory because (S)ell command is only valid when merchant interface is active
	
INV_4.COMMAND.ENTER
.INV_4.MERCH_INTERFACE.BRANCH  	
	;is merchant transactions (MT) sub_module loaded in memory, and if so was the call to INV_4 from the .INIT_SCREEN code (initial launch)?
	LDA INV.SUB_MODULE.LOAD_FLAGS+$8 ;($00 = sub-module not loaded in memory | $01 = sub-module loaded in memory | high-bit set = initial launch complete)
	BEQ .INV_4.MERCH_INTERFACE.BRANCH.DONE ;branch if MT module is not in memory because then enter key is ready/unready
	JSR INV_8.SELL_ITEM
	JMP INV_4.STATE.LOOP.ALT.ENTRANCE
.INV_4.MERCH_INTERFACE.BRANCH.DONE

	JMP INV_4_GROUP2.READY_UNREADY.REQUEST
	
INV_4.COMMAND.CHANGE.ACTIVE_PLAYER.UP
.INV_4.MERCH_INTERFACE.BRANCH  	
	;is merchant transactions (MT) sub_module loaded in memory, and if so was the call to INV_4 from the .INIT_SCREEN code (initial launch)?
	LDA INV.SUB_MODULE.LOAD_FLAGS+$8 ;($00 = sub-module not loaded in memory | $01 = sub-module loaded in memory | high-bit set = initial launch complete)
	BNE INV_4.STATE.LOOP.ALT.ENTRANCE ;branch if MT module is in memory because up/down arrow keys are only valid when merchant interface is active

	JSR CHARACTER.ROSTER.INCREMENT_UP
	JMP INV_4.DISPLAY_PLAYER_INVENTORY
	
INV_4.COMMAND.CHANGE.ACTIVE_PLAYER.DOWN
.INV_4.MERCH_INTERFACE.BRANCH  	
	;is merchant transactions (MT) sub_module loaded in memory, and if so was the call to INV_4 from the .INIT_SCREEN code (initial launch)?
	LDA INV.SUB_MODULE.LOAD_FLAGS+$8 ;($00 = sub-module not loaded in memory | $01 = sub-module loaded in memory | high-bit set = initial launch complete)
	BNE INV_4.STATE.LOOP.ALT.ENTRANCE_STEP ;branch if MT module is in memory because up/down arrow keys are only valid when merchant interface is active

	JSR CHARACTER.ROSTER.INCREMENT_DOWN
	JMP INV_4.DISPLAY_PLAYER_INVENTORY	

INV_4.COMMAND.LEFT_ARROW
	JMP INV_4.MOVE_SELECTOR_UP

INV_4.COMMAND.RIGHT_ARROW
	JMP INV_4.MOVE_SELECTOR_DOWN


INV_4.BUY_ITEMS
		LDA #$B7	;simulates press of hotkey 7 to affect a switch to the merchant inventory window
	
	;**FALLS THROUGH** (to INV_4.EXIT_STEP)
INV_4.EXIT_STEP
	JMP INV_4.EXIT
	
INV_4.COMMAND.INFO
	JSR INV_4.1b.ADDITIONAL_INFO
INV_4.STATE.LOOP.ALT.ENTRANCE_STEP
	JMP INV_4.STATE.LOOP.ALT.ENTRANCE


INV_4.PLAYTEST.KEY_SHIFT9	
	JSR PLAYTEST.KEY0 ;the JSR is to KEY_0 because the main game loop uses KEY_0 and this routine is resident in the main game loop source file.  
	JMP INV_4.STATE.LOOP.ALT.ENTRANCE

INV_4.COMMAND.QUIT
	JMP COMMAND.QUIT.PREP

	
@END
@END


	
;INV_4.EXIT

;****see INV_4.1b subroutines



;============================================================
@END


;LOCAL SUBROUTINES (GROUP 1) ***ALWAYS NEEDED**
@START
;MENU MANAGEMENT
INV_4.MOVE_SELECTOR_UP
@START
.ERASE.ITEM_SELECTOR

	JSR INV_4.ERASE.ITEM_SELECTOR


					
					; LDA #$ab
					; LDx INV_4.1b.SCREEN_ITEM_LIST.SELECTED.ITEM ;is the item selector set to the last item in the list?
					; ldy #INV_4.MAX_ONSCREEN.ITEMS
					; jsr prep.brk
					; brk
					
.SCROLL.DOWN ;scrolling down allows 1st item on screen to be replaced with new item
;VALIDATE ENTRANCE
	LDA INV_4.1b.SCREEN_ITEM_LIST.SELECTED.ITEM ;is the item selector set to the first item in the list?
	BNE .SCROLL.DOWN.DONE ;if item selector is not set to first item, then selector shouldn't move up into data not on screen. 
	; BEQ .COW
	; JMP .SCROLL.DOWN.DONE ;if item selector is not set to first item, then selector shouldn't move up into data not on screen. 
; .COW

;GET NEXT ITEM

					; lda #$00 
					; sta cow
					
	;set pointer to item0
	LDA INV_4.1b.SCREEN_ITEM_LIST.ARRAY+$0 ;load LO byte of inventory record address for item0
	STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
	LDA INV_4.1b.SCREEN_ITEM_LIST.ARRAY+$1 ;load HO byte of inventory record address for item0
	STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
					
.LOOP.GET.NEXT_ITEM
		LDA #$01 ;($00 = iterate down | $01 = iterate up)
	JSR INV_4.GET.NEXT_ITEM
		;RETURN VALUE: ACC ($00 = next item found, $01 = top or bottom of array reached), INV_4.1b.NEXT_ITEM.QTY_AVAILABLE, INV_4.NEXT_ITEM.READIED_STATUS, PLAYER_MERCH.INVENTORY.DATA.POINTER(2)
	
				; inc cow
				; sta temp
				; PHA ;save ACC
				; TXA
				; PHA
				
				; LDA #$24
				; STA VARIABLE1.HTAB
				; LDA #$07
				; STA VARIABLE1.VTAB
				; LDA #$26
				; STA VARIABLE2.HTAB
				; LDA #$08
				; STA VARIABLE2.VTAB								
				
				; ; LDA temp
				; ; LDX cow
				; lda 
			; JSR MONITOR.VARIABLE
			; JSR KEYIN ;pause optional
				; STA TEMP
			

; ; --------------------------------
; ; OPTIONAL: detect "Q" to break	
; ; note: place directly after template1. This way at the JSR KEYIN in template 1, if "Q" is pressed the code breaks to 
; ; the monitor so troubleshooting can be done.  
					; CMP #$D1
					; BNE .TEMP
					; LDA #$bb
					; ldx INV_4.1b.DISPLAY.MODE 
					; JSR PREP.BRK
					; BRK
; .TEMP
					; LDA TEMP
; ;--------------------------------

					; PLA
					; TAX
					; PLA ;restore ACC


		
	;exit test
	CMP #$01 ;was top/bottom of array reached? (i.e. there is no next item)
	BEQ .DRAW.ITEM_SELECTOR.ALT_ENTRANCE ;branch if yes. draw item selector (without item selection increment) because selector was aready erased above. 
	;BEQ .TEST ;branch if yes. draw item selector (without item selection increment) because selector was aready erased above. 

.VALIDATE.ITEM
;should item be displayed onscreen?
	
	;is merchant transactions (MT) sub_module loaded in memory, and if so was the call to INV_4 from the .INIT_SCREEN code (initial launch)?
	LDA INV.SUB_MODULE.LOAD_FLAGS+$8 ;($00 = sub-module not loaded in memory | $01 = sub-module loaded in memory | high-bit set = initial launch complete)
	BNE .VALIDATE.ITEM.DONE ;branch if MT module is loaded in memory. automatically display all items

	LDA INV_4.NEXT_ITEM.READIED_STATUS ;($00 = not readied | $01 = readied by active player | $02 = readied twice by active player)
	BNE .VALIDATE.ITEM.DONE ;if item is readied by active player then item should be displayed
	LDA INV_4.1b.NEXT_ITEM.QTY_AVAILABLE ;if there are no units of the item available to be readied by the active player then there is no reason to display the item. 
	BEQ .LOOP.GET.NEXT_ITEM ;get next item. INV_4.GET.NEXT_ITEM will pick up at the position it left off at in the inventory table.  
.VALIDATE.ITEM.DONE

			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
; ;=====DEBUG HOOK======						
		; LDX #$00
; .DEBUG.COPY1
		; LDA INV_4.1b.SCREEN_ITEM_LIST.ARRAY,X
		; STA $BD00,X
		; INX
		; BNE .DEBUG.COPY1
		
		
		; LDX #$00
; .DEBUG.COPY2
		; LDA INV_4.1a.SCREEN_ITEM_LIST.DATA,X
		; STA $BE00,X
		; INX
		; BNE .DEBUG.COPY2

			; LDA #$BB
			; LDX INV_4.1b.SCREEN_ITEM_LIST.SELECTED.ITEM
			; LDY TEMP
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP


			
	;**FALLS THROUGH** 	;Since the party owns more of this item than is readied by the PCs, then the item should be displayed
	
	JSR INV_4.1b.SCREEN_LIST.SCROLL.DOWN

.SAVE.ITEM
	
		;INV_4.1b.NEXT_ITEM.QTY_AVAILABLE: use existing value
		;INV_4.NEXT_ITEM.READIED_STATUS: use existing value
		;PLAYER_MERCH.INVENTORY.DATA.POINTER: use existing value
		LDX #$00 ;set INV_4.1b.SCREEN_ITEM_LIST.ARRAY index to the start of item 0 (of item0-item5)
	JSR INV_4.1a.SAVE_ITEM ;saves item if valid
 			
;SAVE MASTER ITEM RECORD
@START

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

		
		
			
			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			
			; LDX #$00
; .DEBUG_COPY1
			; LDA FILE.ITEM_TABLE.RECORD.READ,X
			; STA $BE00,X
			; INX
			; BNE .DEBUG_COPY1
			
			; PLA
			; TAY
			; LDA #$AA
			; LDX FILE.ITEM_TABLE.ID
			; ;LDY 
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP

			
;COPY DATA
	
	;**OPT** Memory. Speed. Selected item should always be $05. Hard code the index in X-REG
	
	;init indexes
		;ACC parm: preset to INV_4.1b.SCREEN_ITEM_LIST.SELECTED.ITEM  ;load selected item number
	JSR INV_4.1a.GET_INDEX.SCREEN_ITEM_LIST.DATA.ALT_ENTRANCE	;set INV_4.1a.SCREEN_ITEM_LIST.DATA index ($20 byte records)
		;RETURN: X-REG = INV_4.1a.SCREEN_ITEM_LIST.DATA index
					
	LDY #$00 ;init FILE.ITEM_TABLE.RECORD.READ index
.LOOP.COPY.DATA
	LDA FILE.ITEM_TABLE.RECORD.READ,Y
	
				;STA TEMP
				
	STA INV_4.1a.SCREEN_ITEM_LIST.DATA,X
				
					
	INX	;increment INV_4.1a.SCREEN_ITEM_LIST.DATA index
	INY ;increment FILE.ITEM_TABLE.RECORD.READ index
	CPY #FILE.ITEM_TABLE.RECORD.SIZE
	BNE .LOOP.COPY.DATA
@END



	
	JSR INV_4.DISPLAY_PLAYER_INVENTORY.SCROLL_DOWN
	
	
	JMP .DRAW.ITEM_SELECTOR.ALT_ENTRANCE ;draw item selector (without item selection increment) because selector was aready erased above.

	
	;JSR INV_4.GET.NEXT_ITEM_DOWN
;		***parse return value**...IF top/bottom of array code returned in ACC, THEN JMP INV_4.SUB_MODULE.STATE.LOOP
;	;save new item record to INV_4.1b.SCREEN_ITEM_LIST.ARRAY
;			scroll INV_4.1b.SCREEN_ITEM_LIST.ARRAY up
;			save new item record to item position 5 in the array
;
	;INV_4.1b.DISPLAY_PLAYER_INVENTORY.SCROLL_UP
;	DEC INV_4.1b.SCREEN_ITEM_LIST.SELECTED.ITEM ;offset the DEC in draw item selector since the selected item doesn't change

	
;****INSERT CODE FOR GET NEXT ITEM, WHEN MOVING DOWN IS OFFSCREEN

.SCROLL.DOWN.DONE
	
.DRAW.ITEM_SELECTOR ;(also prints new item if a scroll occured)
	
	DEC INV_4.1b.SCREEN_ITEM_LIST.SELECTED.ITEM ;advance selector to next item down on the video screen

.DRAW.ITEM_SELECTOR.ALT_ENTRANCE			
	JSR INV_4.DRAW.ITEM_SELECTOR


.EXIT
	JSR FLIP.PAGE

.EXIT.ALT
						;JSR KEYIN



	JMP INV_4.STATE.LOOP.ALT.ENTRANCE

; .TEST
		; LDA #$AA
		; LDX #$AA
		; LDY #$AA
		; JSR PREP.BRK
		; BRK
		

@END

INV_4.MOVE_SELECTOR_DOWN
@START

.DETECT.END_OF_SCREEN_LIST
;if there are less than 6 items displayed on the screen, that means less than 6 items in the
;parties inventory are relevant for display to the active PC. In this case, there is no
;reason to scroll and the item selector should stay where it is.
	LDA #INV_4.MAX_ONSCREEN.ITEMS-1 ;-1 because the constant is a quantity (starting with 1) and the variable in the CMP starts with 0. 
	CMP INV_4.1b.SCREEN_LIST.TOTAL_RECORDS ;are there less than 6 items on the screen? (item0-item5)
	BEQ .DETECT.END_OF_SCREEN_LIST.DONE
	LDA INV_4.1b.SCREEN_ITEM_LIST.SELECTED.ITEM ;is the item selector set to the last item in the list?
	CMP INV_4.1b.SCREEN_LIST.TOTAL_RECORDS ;(0-5 = item # of last item on screen | $FF = no inventory records are eligible for display)
	BEQ .EXIT.ALT ;exit without page flipping
	; BNE .COW
	; JMP .EXIT.ALT ;exit without page flipping
; .COW

.DETECT.END_OF_SCREEN_LIST.DONE	
	

.ERASE.ITEM_SELECTOR
	JSR INV_4.ERASE.ITEM_SELECTOR

					
					
					; LDA #$ab
					; LDx INV_4.1b.SCREEN_ITEM_LIST.SELECTED.ITEM ;is the item selector set to the last item in the list?
					; ldy #INV_4.MAX_ONSCREEN.ITEMS
					; jsr prep.brk
					; brk
					
.SCROLL.UP ;scrolling down allows last item on screen to be replaced with new item
;VALIDATE ENTRANCE
	LDA INV_4.1b.SCREEN_ITEM_LIST.SELECTED.ITEM ;is the item selector set to the last item in the list?
	CMP #INV_4.MAX_ONSCREEN.ITEMS-1 ;-1 because the constant is a quantity (starting with 1) and the array in the LDA starts with 0. 
	BCC .SCROLL.UP.DONE


	;set pointer to item5
	LDA INV_4.1b.SCREEN_ITEM_LIST.ARRAY+$14 ;load LO byte of inventory record address for item0
	STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
	LDA INV_4.1b.SCREEN_ITEM_LIST.ARRAY+$15 ;load HO byte of inventory record address for item0
	STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
		
;GET NEXT ITEM
.LOOP.GET.NEXT_ITEM

		LDA #$00 ;($00 = iterate down | $01 = iterate up)
	JSR INV_4.GET.NEXT_ITEM
		;RETURN VALUE: ACC ($00 = next item found, $01 = top or bottom of array reached), INV_4.1b.NEXT_ITEM.QTY_AVAILABLE, INV_4.NEXT_ITEM.READIED_STATUS, PLAYER_MERCH.INVENTORY.DATA.POINTER(2)
	
	
	;exit test
	CMP #$01 ;was top/bottom of array reached? (i.e. there is no next item)
	BEQ .DRAW.ITEM_SELECTOR.ALT_ENTRANCE ;branch if yes. draw item selector (without item selection increment) because selector was aready erased above. 
	;BEQ .TEST ;branch if yes. draw item selector (without item selection increment) because selector was aready erased above. 

.VALIDATE.ITEM
;should item be displayed onscreen?
	
	;is merchant transactions (MT) sub_module loaded in memory, and if so was the call to INV_4 from the .INIT_SCREEN code (initial launch)?
	LDA INV.SUB_MODULE.LOAD_FLAGS+$8 ;($00 = sub-module not loaded in memory | $01 = sub-module loaded in memory | high-bit set = initial launch complete)
	BNE .VALIDATE.ITEM.DONE ;branch if MT module is loaded in memory. automatically display all items
	
	LDA INV_4.NEXT_ITEM.READIED_STATUS ;($00 = not readied | $01 = readied by active player | $02 = readied twice by active player)
	BNE .VALIDATE.ITEM.DONE ;if item is readied by active player then item should be displayed
	LDA INV_4.1b.NEXT_ITEM.QTY_AVAILABLE ;if there are no units of the item available to be readied by the active player then there is no reason to display the item. 
	BEQ .LOOP.GET.NEXT_ITEM ;get next item. INV_4.GET.NEXT_ITEM will pick up at the position it left off at in the inventory table.  
.VALIDATE.ITEM.DONE
	
	;**FALLS THROUGH** 	;Since the party owns more of this item than is readied by the PCs, then the item should be displayed
	
	JSR INV_4.1b.SCREEN_LIST.SCROLL.UP


			
.SAVE.ITEM
	
		;INV_4.1b.NEXT_ITEM.QTY_AVAILABLE: use existing value
		;INV_4.NEXT_ITEM.READIED_STATUS: use existing value
		;PLAYER_MERCH.INVENTORY.DATA.POINTER: use existing value
		LDX #$14 ;set INV_4.1b.SCREEN_ITEM_LIST.ARRAY index to the start of item 5 (of item0-item5)
	JSR INV_4.1a.SAVE_ITEM ;saves item if valid
 


			
;SAVE MASTER ITEM RECORD
@START

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

		
		
			
			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			
			; LDX #$00
; .DEBUG_COPY1
			; LDA FILE.ITEM_TABLE.RECORD.READ,X
			; STA $BE00,X
			; INX
			; BNE .DEBUG_COPY1
			
			; PLA
			; TAY
			; LDA #$AA
			; LDX FILE.ITEM_TABLE.ID
			; ;LDY 
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP

			
;COPY DATA
	
	;**OPT** Memory. Speed. Selected item should always be $05. Hard code the index in X-REG
	
	;init indexes
		;ACC parm: preset to INV_4.1b.SCREEN_ITEM_LIST.SELECTED.ITEM  ;load selected item number
	JSR INV_4.1a.GET_INDEX.SCREEN_ITEM_LIST.DATA.ALT_ENTRANCE	;set INV_4.1a.SCREEN_ITEM_LIST.DATA index ($20 byte records)
		;RETURN: X-REG = INV_4.1a.SCREEN_ITEM_LIST.DATA index
			
	LDY #$00 ;init FILE.ITEM_TABLE.RECORD.READ index
.LOOP.COPY.DATA
	LDA FILE.ITEM_TABLE.RECORD.READ,Y
	
				;STA TEMP
				
	STA INV_4.1a.SCREEN_ITEM_LIST.DATA,X
				
					
	INX	;increment INV_4.1a.SCREEN_ITEM_LIST.DATA index
	INY ;increment FILE.ITEM_TABLE.RECORD.READ index
	CPY #FILE.ITEM_TABLE.RECORD.SIZE
	BNE .LOOP.COPY.DATA
@END



			
	
	JSR INV_4.DISPLAY_PLAYER_INVENTORY.SCROLL_UP
	JMP .DRAW.ITEM_SELECTOR.ALT_ENTRANCE ;draw item selector (without item selection increment) because selector was aready erased above.


	
;****INSERT CODE FOR GET NEXT ITEM, WHEN MOVING DOWN IS OFFSCREEN

.SCROLL.UP.DONE
	
.DRAW.ITEM_SELECTOR ;(also prints new item if a scroll occured)
	
	INC INV_4.1b.SCREEN_ITEM_LIST.SELECTED.ITEM ;advance selector to next item down on the video screen

.DRAW.ITEM_SELECTOR.ALT_ENTRANCE		
	JSR INV_4.DRAW.ITEM_SELECTOR


.EXIT
	JSR FLIP.PAGE

.EXIT.ALT

	JMP INV_4.STATE.LOOP.ALT.ENTRANCE

; .TEST
		; LDA #$AA
		; LDX #$AA
		; LDY #$AA
		; JSR PREP.BRK
		; BRK
		

@END

;data  
INV_4.GET.NEXT_ITEM
@START
;PARAMETERS: ACC ($00 = iterate down | $01 = iterate up), INV_4.1b.DISPLAY.MODE, PLAYER_MERCH.INVENTORY.DATA.POINTER(2)
;ENTRANCE: .INIT.SCREEN_ITEM_LIST, .MOVE_SELECTOR_DOWN, .MOVE_SELECTOR_UP
;RETURN: ACC ($00 = next item found, $01 = top or bottom of array reached), INV_4.1b.NEXT_ITEM.QTY_AVAILABLE, INV_4.NEXT_ITEM.READIED_STATUS, PLAYER_MERCH.INVENTORY.DATA.POINTER(2) (pointer to the record found)

;=====================SUBROUTINE DOCUMENTATION===========================================================================
;
;iterates INV.PLAYER.INVENTORY.DATA from the position of PLAYER_MERCH.INVENTORY.DATA.POINTER at the end of the last call.
;the loop runs until an item matching INV_4.1b.DISPLAY.MODE (weapon, armor, misc item) is found or until the top or bottom of the array
;is reached, depending on the iteration direction specified by the ACC parm. 
;
;The fists and skin items are filtered out. This avoids needing to manage their readied/unreadied status on the inventory screen,
;which has some complications. The fists and skin items are the first two records in INV.PLAYER.INVENTORY.DATA which is required due to
;some hard coded references, at a minimum the one in this routine where BEQ is used to detect zero intead of CMP #$00 
;
;=============================================================================================================================


.NEXT_RECORD
;The pointer is advance to the next record at the top of the routine, so that when it's called for scrolling,
;if the next record isn't within the array, that is detected right away. To accomidate this design,
;the init value of the pointer is set to one record before the start of PLAYER.INVENTORY.DATA, which
;works fine because screen init will call this routine with the interate down parm set. 


.SAVE.PARAMETERS
	;ACC ($00 = iterate down | $01 = iterate up)
	STA INV_4.1b.GET.NEXT_ITEM.PARM.MODE

;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA


.MODE_BRANCH	
	LDA INV_4.1b.GET.NEXT_ITEM.PARM.MODE ;($00 = iterate down | $01 = iterate up)
	BNE .NEXT_RECORD.ITERATE_UP
	
	;**FALLS THROUGH**

.LOOP.SEARCH.INVENTORY.DATA
	
.NEXT_RECORD.ITERATE_DOWN
	LDA PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
	CLC
	ADC #INV.PLAYER.INVENTORY.DATA.RECORD_SIZE
	STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
	LDA PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
	ADC /INV.PLAYER.INVENTORY.DATA.RECORD_SIZE ;16-bit add
	STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
.EXIT.TEST.DOWN
	CMP /INV.PLAYER.INVENTORY.DATA.PATCH_END
	BCC .TEST.RECORD_MATCH
	BEQ .TEST.RECORD_MATCH
 
	JMP .START_OR_END.OF_ARRAY_REACHED

.NEXT_RECORD.ITERATE_UP
	LDA PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
	SEC
	SBC #INV.PLAYER.INVENTORY.DATA.RECORD_SIZE
	STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
	LDA PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
	SBC /INV.PLAYER.INVENTORY.DATA.RECORD_SIZE ;16-bit add
	STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
.EXIT.TEST.UP
	CMP /INV.PLAYER.INVENTORY.DATA.PATCH_START
	; BCC .TEST.RECORD_MATCH
	; BEQ .TEST.RECORD_MATCH	
	BCS .TEST.RECORD_MATCH

	
	JMP .START_OR_END.OF_ARRAY_REACHED

	
	
.TEST.RECORD_MATCH
	LDY #$00 ;reset Y-REG on each iteration because array position is managed exclusively through PLAYER_MERCH.INVENTORY.DATA.POINTER 
	
	;Y-REG ;set to record byte $0 (item_level | item_type)
	LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y
	CMP #$FF ;check for open record value
	BEQ .MODE_BRANCH ;branch if record is open
	;Item_Type is in LO nibble.
	AND #$F ;mask-out HO nibble. 
;FILTER1: Item_Type
	CMP INV_4.1b.DISPLAY.MODE
	BNE .MODE_BRANCH ;next record
.FILTER2 ;this is an item_type specific filter
	CMP #INV.ITEM_TYPE.MISC ;this test only applies to item_type $00 (weapons) and $01 (armor)
	BCC .FILTER2C	 ;branch if item_type = $00 (weapons) and $01 (armor)
	INY ;advance to record byte $1 (item_ID)
	LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y
	PHA ;save item_ID
	LDA INV.ACTIVE_MENU.CODE 
	CMP #INV.ACTIVE_MENU.SPELLS ;INV_4.1b.DISPLAY.MODE = $02 (misc items), but is spell menu active?
	BEQ .FILTER2B ;branch, if yes
	;**FALLS THROUGH**	if, no. ;This means that the misc items menu is active.
.FILTER2A ;Misc Items menu active: filter out spells
	PLA ;restore item_ID
	CMP #INV_4.SPELL_RANGE.LTH
	BCC .MODE_BRANCH ;next record
	JMP .FILTER2.DONE
.FILTER2B ;Spells menu active: filter out non-spells
	PLA ;restore item_ID
	CMP #INV_4.SPELL_RANGE.LTH
	BCS .MODE_BRANCH ;next record
	JMP .FILTER2.DONE
.FILTER2C ;Weapons/Armor menu active: filter out fists and skin	
	INY ;advance to record byte $1 (item_ID)
	LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y
	;CMP #INV.ITEM_TYPE.WEAPON.FISTS ;fist or skin, they have the same item_ID
	BEQ .MODE_BRANCH ;;next record. branch if item_ID is fists or skin (can't tell which without an item_type test but it doesn't matter)
	;**FALLS THROUGH**
.FILTER2.DONE

.FILTER3 ;Filter out items with qty $0 (not available qty, this refers to the qty owned)
	LDY #$02 ;advane to record byte $3 (item_quantity)
	LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y
	BEQ .MODE_BRANCH ;next record
	
	;**FALLS THROUGH**	
.FILTERS.DONE

	
	;**FALLS THROUGH**
	
.CALCULATE.READIED_QTY.FIELDS
	JSR INV_4.CALCULATE.READIED_QTY.FIELDS
		;RETURN: INV_4.NEXT_ITEM.READIED_STATUS, INV_4.1b.NEXT_ITEM.QTY_AVAILABLE
	
			; LDY #$00
			; LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y ;load item_type
			; CMP #$02
			; BNE .TEMP
			; LDY #$01
			; LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y ;load item_type
			; CMP #$00
			; BNE .TEMP	
			; LDA INV_4.NEXT_ITEM.READIED_STATUS 
			; STA $BF00
			; LDA INV_4.1b.NEXT_ITEM.QTY_AVAILABLE
			; STA $BF01
			; LDA #$AA
			; JSR PREP.BRK
			; BRK
; .TEMP			
			
			
	;**FALLS THROUGH**

.ITEM_FOUND
	LDA #$00 ;set return value: ($00 = next item found, $01 = top or bottom of array reached)	
	JMP .SAVE.RETURN_VALUE
	
.START_OR_END.OF_ARRAY_REACHED
	LDA #$01 ;set return value: ($00 = next item found, $01 = top or bottom of array reached)

	;**FALLS THROUGH**
	
.SAVE.RETURN_VALUE	
	STA TEMP ;save return value

	;**FALLS THROUGH**

	
.EXIT
;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX
	
	;ACC = return value
	LDA TEMP ;restore return value
	
	RTS

	
	
@END

INV_4.CALCULATE.READIED_QTY.FIELDS
@START
;PARAMETERS: INV_4.ACTIVE_PLAYER, PLAYER_MERCH.INVENTORY.DATA.POINTER(2)
;ENTRANCE: direct
;RETURN: INV_4.NEXT_ITEM.READIED_STATUS, INV_4.1b.NEXT_ITEM.QTY_AVAILABLE


;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA
	
.INIT
	LDA #$00
	STA	INV_4.NEXT_ITEM.READIED_STATUS ;($00 = not readied | $01 = readied by active player | $02 = readied twice by active player)


	
	; LDA INV.ACTIVE_MENU.CODE 
	; CMP #INV.ACTIVE_MENU.SPELLS ;INV_4.1b.DISPLAY.MODE = $02 (misc items), but is spell menu active?

	
	
.INV_4.GET.READIED.STATUS 
@START
;Note: Check both readied flag fields, if active playered has item readied twice then set to $02, if once set to $01 	
	LDY #$04 ;set index to record byte $4 (Readied Equipment Flags #2)
	LDX INV_4.ACTIVE_PLAYER ;load index to INV.READIED_FLAGS.BIT_TEST_MASK	
	LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y ;load Readied Equipment Flags #2			
	AND INV.READIED_FLAGS.BIT_TEST_MASK,X ;test bit flag for active player	
	BEQ .READIED.FLAGS1.TEST ;branch if bit not set (active player doesn't have item readied via Readied Equipment Flags field #2)	
	INC INV_4.NEXT_ITEM.READIED_STATUS ;($00 = not readied | $01 = readied by active player | $02 = readied twice by active player)
	
.READIED.FLAGS1.TEST
	DEY ;advance to record byte $3 (Readied Equipment Flags #1)
	;X-REG: INV_4.ACTIVE_PLAYER (index to INV.READIED_FLAGS.BIT_TEST_MASK)
	LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y ;load Readied Equipment Flags #1
	AND INV.READIED_FLAGS.BIT_TEST_MASK,X ;test bit flag for active player	
	BEQ .READIED.FLAGS1.TEST.DONE ;branch if bit not set (active player doesn't have item readied via Readied Equipment Flags field #1)	
	INC INV_4.NEXT_ITEM.READIED_STATUS ;($00 = not readied | $01 = readied by active player | $02 = readied twice by active player)
.READIED.FLAGS1.TEST.DONE

			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP1
			; lda #$bb
			; JSR PREP.BRK
			; BRK
; .TEMP1
			; LDA TEMP
			
			
; ; ;DEBUG HOOK
			; ;validate entrance
			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP				
			; LDA PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
			; CMP #$7E
			; BNE .TEMP
			; LDA PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
			; CMP #$BA			
			; BNE .TEMP
			
			; ;save calculations
			; LDY #$04 ;set index to record byte $4 (Readied Equipment Flags #2)
			; LDX INV_4.ACTIVE_PLAYER ;load index to INV.READIED_FLAGS.BIT_TEST_MASK	
			; STX $BE00
			; LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y ;load Readied Equipment Flags #2		
			; STA $BE01
			; LDA INV.READIED_FLAGS.BIT_TEST_MASK,X ;test bit flag for active player	
			; STA $BE02
			
			; DEY ;advance to record byte $3 (Readied Equipment Flags #1)
			; ;X-REG: INV_4.ACTIVE_PLAYER (index to INV.READIED_FLAGS.BIT_TEST_MASK)
			; LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y ;load Readied Equipment Flags #1
			; STA $BE03
			
			; LDA INV.READIED_FLAGS.BIT_TEST_MASK,X ;test bit flag for active player	
			; STA $BE04

			; LDA INV_4.NEXT_ITEM.READIED_STATUS ;($00 = not readied | $01 = readied by active player | $02 = readied twice by active player)
			; STA $BE05

			; LDA #$BA 
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP
	
	
@END

			; STA TEMP
			; ; LDA COW
			; ; CMP #$02
			; LDA PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
			; CMP #$30
			; BNE .TEMP
			; LDA PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
			; CMP #$BA			
			; BNE .TEMP
			; LDA #$01
			; STA TROUBLESHOOTING.HOOK
			; ; LDA #$BA 
			; ; LDX TEMP
			; ; LDY INV.RE.READIED.QTY
			; ; JSR PREP.BRK
			; ; BRK
; .TEMP
			; LDA TEMP


.INV_4.GET.READIED.QTY
	JSR INV.GET.READIED.QTY 
		;RETURN VALUE: INV.RE.READIED.QTY (the number of times the ITEM-TO-READY is already readied by all PCs, including active PC)

		
.CALCULATE.AVAILABLE_QTY		
;*QTY_AVAILABLE = inventory record qty - # of times item is readied
	DEY ;advance to record byte $3 (Readied Equipment Flags #1)
	;X-REG: INV_4.ACTIVE_PLAYER (index to INV.READIED_FLAGS.BIT_TEST_MASK)
	LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y ;load QTY field

					; tax
					; lda #$ab
					; jsr prep.brk
					; ldy INV.RE.READIED.QTY
					; brk
	SEC
	SBC INV.RE.READIED.QTY
	BCC .ERROR.QTY.OVERFLOW
	STA INV_4.1b.NEXT_ITEM.QTY_AVAILABLE

	
	
; ;DEBUG HOOK	
			; sta temp
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP	
			; lda PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
			; sta $be00
			; lda PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
			; sta $be01			
			; LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y ;load QTY field
			; sta $be02
			; lda INV.RE.READIED.QTY
			; sta $be03			
			
			; lda #$aa
			; LDX TEMP
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP
			
	


					
.EXIT
;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX
	
	RTS
	
	
.ERROR.QTY.OVERFLOW
;.CALCULATE.AVAILABLE_QTY (INV_4.GET.NEXT_ITEM) reports that the number of times the current item
;has been ready is > the quantity of that item in the player's inventory.

	JSR PREP.BRK
	BRK
	
@END


;SCROLLING
INV_4.DISPLAY_PLAYER_INVENTORY.SCROLL_UP ;entrance only for player inventory window (INV_4)
@START
	LDA #$01
	STA DRAW.START_BYTE

	LDA #$13
	STA DRAW.STOP_BYTE.ALT
	
	LDA #$B8
	STA TWF.SCROLLING.STOP_LINE			;set the stop line, which is the last line of the last row of text characters to scroll +$08. 
	
	JMP INV_4.1b.DISPLAY_PLAYER_INVENTORY.SCROLL_UP

@END
	

INV_4.DISPLAY_PLAYER_INVENTORY.SCROLL_DOWN ;entrance only for player inventory window (INV_4)
@START
	LDA #$01
	STA DRAW.START_BYTE

	LDA #$13
	STA DRAW.STOP_BYTE.ALT

	LDA #$90 ;top line of the first row of text characters of the penultimate item
	STA LINE.START.COPY_FROM
	
	
	JMP INV_4.1b.DISPLAY_PLAYER_INVENTORY.SCROLL_DOWN

@END



;text
INV_4.DRAW.ITEM_SELECTOR
@START
		LDA #$7F ;set mode ($00 = item selector OFF | $7F = item selector ON) 
		;LDA #$00 ;set mode ($00 = item selector OFF | $7F = item selector ON) 
		CLC ;(CLC = don't sync hi-res pages | SEC = sync hi-res pages)
	JSR INV_4.PRINT.SELECTED.ITEM

	RTS
@END

INV_4.ERASE.ITEM_SELECTOR
@START
		;LDA #$7F ;set mode ($00 = item selector OFF | $7F = item selector ON) 
		LDA #$00 ;set mode ($00 = item selector OFF | $7F = item selector ON) 
		;INV_4.1b.SCREEN_ITEM_LIST.SELECTED.ITEM: use existing value
		SEC ;(CLC = don't sync hi-res pages | SEC = sync hi-res pages)
	JSR INV_4.PRINT.SELECTED.ITEM
	
	
	RTS
@END

INV_4.PRINT.ALL.ITEMS ;entrance only for player inventory window (INV_4)
@START
	LDA #INV_4.SCREEN_LIST.ITEM_ALL.HTAB.LEFT
	STA INV_4.1b.SCREEN_LIST.ITEM_ALL.HTAB.LEFT
	
	LDA #INV_4.MAX_ONSCREEN.ITEMS
	STA INV_4.1b.MAX_ONSCREEN.ITEMS

	JMP INV_4.1a.PRINT.ALL.ITEMS
	
@END
	
INV_4.PRINT.SELECTED.ITEM ;entrance only for player inventory window (INV_4)
@START
	PHA ;save ACC parm
	
	LDA #INV_4.SCREEN_LIST.ITEM_ALL.HTAB.LEFT
	STA INV_4.1b.SCREEN_LIST.ITEM_ALL.HTAB.LEFT
	LDA #INV_4.SCREEN_LIST.ITEM_ALL.HTAB.RIGHT+1
	STA INV_4.1b.SCREEN_LIST.ITEM_ALL.HTAB.RIGHT

	PLA ;restore  ACC parm
	
	JMP INV_4.1a.PRINT.SELECTED.ITEM

	
@END

INV_4.CALL.COUT.BACKGROUND
@START
;parameters: ACC (character to print)

		;ACC = parm: character to print
		PHA ;save character to print
		;restore COUT_CHAR_TYPE parm (might be set to $7F depending on the path taken to call this routine)
		LDA COUT_CHAR_TYPE.SAVED
		STA COUT_CHAR_TYPE ;($00 = normal, $7F = inverse)		
		PLA ;pull character to print
	JSR COUT.BACKGROUND		;print second *

	RTS
@END
	

;GRAPHICS
INV_4.DRAW_ERASE.SCROLL_WINDOW
@START	

			
	LDA #TWB.INVENTORY.SCROLL_WINDOW.TOP_LINE	
	STA DRAW.START_LINE
	
	LDA #TWB.INVENTORY.SCROLL_WINDOW.LEFT_SBYTE	
	STA DRAW.START_BYTE	
		
	LDA #TWB.INVENTORY.SCROLL_WINDOW.RIGHT_SBYTE+1
	STA DRAW.STOP_BYTE
		
	LDA #TWB.INVENTORY.SCROLL_WINDOW.BOTTOM_LINE+1
	STA DRAW.STOP_LINE

	LDA #$D5
	STA DRAW.BYTE_VALUE.HORIZONTAL+$0
	
	LDA #$AA
	STA DRAW.BYTE_VALUE.HORIZONTAL+$1
	
	LDA #$81	;#$90
	STA DRAW.BYTE_VALUE.VERTICLE+$0

	LDA #$A0
	STA DRAW.BYTE_VALUE.VERTICLE+$1

.DRAW.RECTANGLE
		LDA PAGE.BACKGROUND
		ORA #$7C ;set DRAW.LINE flags
	JSR DRAW.LINE
	
	JSR FLIP.PAGE
					
		LDA PAGE.BACKGROUND
		ORA #$7C ;set DRAW.LINE flags
	JSR DRAW.LINE


.EXIT
	
	RTS
@END
	

@END

;LOCAL SUBROUTINES (GROUP 2) ***NOT NEEDED WHEN MERCHANT INTERFACE IS ACTIVE***
INV_4.GROUP2.SUBROUTINES.START
@START

;DATA
INV_4_GROUP2.READY_UNREADY.REQUEST
@START
;PARAMETERS: none
;ENTRNACE: direct
;RETURN: updated INV.PLAYER.INVENTORY.DATA

;=====================SUBROUTINE DOCUMENTATION===========================================================================
;
;----NON- SPELLS----
;
;Also see code section documentation for .DETERMINE.READY_UNREADY.MODE below. 
;
;
;----SPELLS----
;The ready and unready scheme for spells is much different than for other items. 
;spells don't use the readied equipment table and readied status isn't stored in the
;PLAYER.INVENTORY.DATA readied flag fields. 
;
;Spells are really pretty simple. If a spell item is readied, it's spell code is stored as value in
;the PLAYER.HOTKEYS.SPELLS array. If a spell hotkey has the high-bit set then no spell is readied to it. 
;
;Instead of tracking the readied status in the inventory
;record for the item (PLAYER.INVENTORY.DATA), the readied status of an item is determined by iterating 
;PLAYER.HOTKEYS.SPELLS and comparing with the spell code field in the inventory record of a given item. 
;
;
;=============================================================================================================================


					; LDA #CHR_SHEET.PC.READIED_EQUIP.RECORD.READ
					; STA $BF00
					; LDA /CHR_SHEET.PC.READIED_EQUIP.RECORD.READ
					; STA $BF01
					; lda #$cc
					; ldx PARM.RE.UNREADY.ITEM_TYPE
					; ldy PARM.RE.UNREADY.ITEM_ID
					
					; JSR PREP.BRK
					; BRK
					
					
					
.INIT	

	JSR INV_4.1b.GET.SCREEN_LIST.ARRAY.INDEX
		;X-REG (index to SCREEN_ITEM_LIST.ARRAY for the item selected on screen)
		

				
	LDA INV_4.1b.SCREEN_ITEM_LIST.ARRAY+$0,X ;load LO byte of inventory record of selected item
	STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$0	
	LDA INV_4.1b.SCREEN_ITEM_LIST.ARRAY+$1,X ;load HO byte of inventory record of selected item
	STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
					
				;STX TEMP
				
				
;IS PC TRYING TO READY ARMOR IN COMBAT?
.COMBAT_ARMOR.CHECK
				;******for testing this section
				; lda #MAP.TYPE.COMBAT 
				; sta PLAYER.MAP.LOCATION_TYPE	;load map type of player's current map
				
	;is combat active?
	LDA PLAYER.MAP.LOCATION_TYPE	;load map type of player's current map
	CMP #MAP.TYPE.COMBAT 	
	BNE .COMBAT_ARMOR.CHECK.DONE
	;is selected item type = armor?
	; LDA INV_4.1b.SCREEN_ITEM_LIST.ARRAY+$0,X ;load level/item_type field of selected item
	; ;item_type is in LO nibble
	; AND #$0F ;mask out HO nibble
	
	LDY #$00 ;set index to byte $0 (level | item_type) of inventory record of selected item
	LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y ;load (level | item_type) field of selected item
	;item_type is in LO nibble
	AND #$0F ;mask out HO nibble						
	CMP #INV.ITEM_TYPE.ARMOR ;#CONSTANT.	
	BNE .COMBAT_ARMOR.CHECK.DONE
	
	;player is trying to ready armor in combat
		
		LDA #$06 ;($00 item readied | $01 = item not readied: item is not readiable | $02 don't use| $03 item not readied: STR gate check failed | $04 item not readied: DEX gate check failed | $05 item not readied: INT gate check failed | $06 can't ready armor during combat | $07 item unreadied | $08 prompt spell hotkey to ready)
	JSR INV_4.GROUP2.PRINT.MISC_TEXT ;plays sound for some text blocks

	JMP .EXIT
.COMBAT_ARMOR.CHECK.DONE

.SPELL_MENU.READY_UNREADY				
@START
;VALIDATE ENTRANCE
	LDA INV.ACTIVE_MENU.CODE 
	CMP #INV.ACTIVE_MENU.SPELLS ;INV_4.1b.DISPLAY.MODE = $02 (misc items), but is spell menu active?
	BNE	.SPELL_MENU.READY_UNREADY.DONE
		
		;**OPT** Memory. .LOOP.SPELL_MENU.IS.READIED and the wrapper around it can probably be merged .SPELL_MENU.ACTIVE (INV_4.1b.VIDEO_OUT.ITEM_LINE1_2), into a common subroutine
		
		
	JSR INV_4.1a.GET_INDEX.SCREEN_ITEM_LIST.DATA.ALT_ENTRANCE
		;RETURN: X-REG = INV_4.1a.SCREEN_ITEM_LIST.DATA index

	LDY #$00 ;init loop counter & PLAYER.HOTKEYS.SPELLS index
.LOOP.SPELL_MENU.IS.READIED	
	LDA PLAYER.HOTKEYS.SPELLS,Y
	CMP INV_4.1a.SCREEN_ITEM_LIST.DATA+INV.ITEM_TABLE.MI.SPELL_CODE.OFFSET,X ;load spell code for current item
	BEQ .SPELL_MENU.ITEM_READIED
	INY
	CPY #PLAYER.HOTKEYS.SPELLS.ARRAY_SIZE
	BNE .LOOP.SPELL_MENU.IS.READIED
	;if loop terminates without a match, then spell is not readied	
	
	
.SPELL_MENU.ITEM_NOT_READIED
@START

					
;PRINT SPELL HOTKEY PROMPT	
		LDA #$08 ;set parm print spell hotkey prompt
	JSR INV_4.GROUP2.PRINT.MISC_TEXT

.SPELL_MENU.KEYIN.LOOP
	; LDA $C000
	; BPL .SPELL_MENU.KEYIN.LOOP
	; STA $C010
	
	JSR KEYIN
	;ACC: keypress
	CMP #$9B			;ESC (EXIT READY SPELL)
	BEQ	.INV_4.STATE.LOOP.ALT.ENTRANCE_STEP

	;is keypress 0-9?
	CMP #$B0
	BCC .SPELL_MENU.KEYIN.LOOP ;keypress not valid, get next key
	CMP #$BA
	BCS .SPELL_MENU.KEYIN.LOOP ;keypress not valid, get next key
	
	;keypress was 0-9
	
	;ACC = keypress
	AND #$0F ;mask-out HO byte (i.e. if 0 was pressed, ACC will contain $B0 and if HO byte is masked out the ACC will contain $00)
	STA INV_4.SPELL_READY.HOTKEY_SELECTED
	LDA INV_4.1a.SCREEN_ITEM_LIST.DATA+INV.ITEM_TABLE.MI.SPELL_CODE.OFFSET,X	;load spell code for current item
	STA INV_4.SPELL_READY.SPELL_CODE
				
				; sta $be00
				; stx $be01
				; lda #INV.ITEM_TABLE.MI.SPELL_CODE.OFFSET
				; sta $be02
				; lda #$aa
				; ldx #INV_4.1a.SCREEN_ITEM_LIST.DATA
				; ldy /INV_4.1a.SCREEN_ITEM_LIST.DATA
				; jsr prep.brk
				; brk
				
	JMP .CALL.READY
	
.INV_4.STATE.LOOP.ALT.ENTRANCE_STEP
	JMP INV_4.STATE.LOOP.ALT.ENTRANCE
@END	

.SPELL_MENU.ITEM_READIED
		
	;Y-REG: iteration number of .LOOP.SPELL_MENU.IS.READIED when it detected the spell code of the selected item 
	STY INV_4.SPELL_READY.HOTKEY_SELECTED

	JMP .CALL.UNREADY

	
.SPELL_MENU.READY_UNREADY.DONE
	
@END


.DETERMINE.READY_UNREADY.MODE
;=====================CODE-SECTION DOCUMENTATION==========================================================================
;(The game decides whether to treat the player's "return/enter" key press as either
;a ready request or unready request. The determination depends on a variety of factors
;such as whether the player already has the item readied, how many units of the item are
;available to be readied, and whether the item is 1 or 2 handed). 
;
;PSEUDO CODE
; -Ready/Unready toggle key
;	; *if ACTIVE_PLAYER READIED_STATUS (INV_4.1b.SCREEN_ITEM_LIST.ARRAY record) is set ($02). ;$02 means item is readied twice
;			; THEN call UNREADY
;	; *if ACTIVE_PLAYER READIED_STATUS (INV_4.1b.SCREEN_ITEM_LIST.ARRAY record) is set ($01). ;$01 means item is readied once
;			; AND selected item is 2-handed THEN call UNREADY
;	; *if ACTIVE_PLAYER READIED_STATUS (INV_4.1b.SCREEN_ITEM_LIST.ARRAY record) is set ($01). ;$01 means item is readied once
;			; AND QTY_AVAILABLE = $0 THEN call UNREADY
;	; *if ACTIVE_PLAYER READIED_STATUS (INV_4.1b.SCREEN_ITEM_LIST.ARRAY record) is set ($01). ;$01 means item is readied once
;			; AND INV_4.DUAL_WIELDING.FLAG = yes ($01) THEN call UNREADY
;	; *ELSE
;		; READY	
;===========================================================================================================================




;;DEBUG HOOK
				; LDA INV_4.1b.SCREEN_ITEM_LIST.ARRAY+$0,X ;load LO byte of inventory record of selected item
				; STA $be00	
				; LDA INV_4.1b.SCREEN_ITEM_LIST.ARRAY+$1,X ;load HO byte of inventory record of selected item
				; STA $be01
				; LDY #$05 ;set index to byte $05 (equipment slot)
				; LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y ;load equipment slot field (high-bit set = 2 handed weapon)
				; sta $be02
				; LDA INV_4.1b.SCREEN_ITEM_LIST.ARRAY+$2,X ;load quantity available field of selected item
				; sta $be03
;
				; LDA #$ab
				; jsr prep.brk
				; brk
				


	LDY #$05 ;set index to byte $05 (equipment slot)
	LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y ;load equipment slot field (high-bit set = 2 handed weapon)
	AND #$7F ;mask-out high-bit
	;CMP #INV.RE.SLOT.UNREADIABLE_ITEM
					; sta $be00
					; lda #$aa
					; jsr prep.brk
					; brk
					
	LDA INV_4.1b.SCREEN_ITEM_LIST.ARRAY+$3,X ;load readied_status field of selected item
	CMP #$02 ;is item readied twice
	BEQ .CALL.UNREADY
	CMP #$00
	BEQ .CALL.READY	;item is not readied. if the item isn't readied at all, then the player can ready it because the available qty of any item on the screen is at least 1 unless the active player has it readied
	
	;imputed: readied_status = $01 (readied once)
	LDY #$00 ;set index to byte $01 (level | item type)
	LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y ;load equipment slot field (high-bit set = 2 handed weapon)
	;item_type is in LO byte
	AND #$0F ;mask-out HO byte
	BNE .CALL.UNREADY ;item_type != weapon. Dual wielding (the next section) is not relevant for armor and misc items. Thus, since the readied status = readied once, the only possible action is to unready the item. 
	
	;parse 1-2 handed status
	LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y ;load equipment slot field (high-bit set = 2 handed weapon)
	BMI .CALL.UNREADY ;branch if 2-handed 
	LDA INV_4.1b.SCREEN_ITEM_LIST.ARRAY+$2,X ;load quantity available field of selected item
	BEQ .CALL.UNREADY ;if item is readied once and qty available is zero, then treat request as unready
	LDA INV_4.DUAL_WIELDING.FLAG ;($00 = single wielding | $01 = dual wielding)
	BNE .CALL.UNREADY ;if player is dual wielding already, then the only availble action is to unready the item. 

	;**FALLS THROUGH**
	
; .ERROR
; ;.DETERMINE.READY_UNREADY.MODE (INV_4_GROUP2.READY_UNREADY.REQUEST) reports that ready/unready request scenario has occured which
; ;was not contemplated. 
	; JSR PREP.BRK
	; BRK
	

.CALL.READY

			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BCC .TEMP1
			; LDA #$CC
			; LDX INV_4.DUAL_WIELDING.FLAG
			; JSR PREP.BRK
			; BRK
; .TEMP1
			; LDA TEMP
			
				
		LDA #$00 ;ACC ($00 = ready equipment mode | $01 = unready equipment mode)

	JMP .EXECUTE.CALL

; .EXIT_STEP ;**OPT** Memory. Seems like this can be removed. It's not called by anything. 
	; JMP .EXIT
	
.CALL.UNREADY
	
			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BCC .TEMP2
			; LDA #$CD
			; LDX INV_4.DUAL_WIELDING.FLAG
			; JSR PREP.BRK
			; BRK
; .TEMP2
			; LDA TEMP
	
		LDA #$01 ;ACC ($00 = ready equipment mode | $01 = unready equipment mode)

	;**FALLS THROUGH**

.EXECUTE.CALL
		PHA ;save mode parm 

;READ READY_UNREADY SUB_MODULE FROM DISK
		LDA #$06 ;ready_unready sub_module code
	JSR INV.READ_LAUNCH.SUB_MODULE.WITH_RTS ;load is aborted if sub_module is already in memory

	
;DEBUG HOOK	
			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; lda PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
			; sta $be00
			; lda PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
			; sta $be01		
			; lda INV_4.1b.SCREEN_ITEM_LIST.SELECTED.ITEM
			; sta $be02
			; LDY #$00 ;set index to byte $00 (level|item_type)
			; LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y ;load byte $00
			; sta $be03
			; INY ;advance to byte $01 (item_ID)
			; LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y ;load byte $01
			; sta $be04
			; lda PARM.RE.READY.ITEM_TYPE
			; sta $be05
			; lda PARM.RE.READY.ITEM_ID
			; sta $be06
			; LDA TEMP
			; STA $be07
			; lda #$aa
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP

			
;CALL READY_UNREADY SUB_MODULE
		LDY #$00 ;set index to byte $00 (level|item_type)
		LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y ;load byte $00
		;item_type is in the LO byte
		AND #$0F ;mask-out HO byte
		STA PARM.RE.ITEM_TYPE
		INY ;advance to byte $01 (item_ID)
		LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y ;load byte $01
		STA PARM.RE.ITEM_ID	
		;RE.ACTIVE_PLAYER: use existing value
		PLA ;restore mode parm 
		;ACC ($00 = ready equipment mode | $01 = unready equipment mode)


;****INV DEBUG LOG*** 			;**OPT** Memory. Remove.
@START
.DEBUG.LABEL
		; PHA
		; LDA #.DEBUG.LABEL
		; STA INV.DEBUG.ADDRESS+$0
		; LDA /.DEBUG.LABEL
		; STA INV.DEBUG.ADDRESS+$1
	JSR INV.DEBUG.LOG.WRITE
		;PLA
@END

		
	JSR INV.READY_UNREADY.EQUIPMENT.ENTRANCE
		;RETURN VALUE: ACC = (($00 item readied | $01 = item not readied: item is not readiable | $02 don't use| $03 item not readied: STR gate check failed | $04 item not readied: DEX gate check failed | $05 item not readied: INT gate check failed | $06 can't ready armor during combat | $07 item unreadied | $08 prompt spell hotkey to ready)				

						; LDX #$AA
						; JSR PREP.BRK
						; BRK
						
		;ACC: use return value above as the text block to print code
	JSR INV_4.GROUP2.PRINT.MISC_TEXT ;plays sound for some text blocks


	;**FALLS THROUGH**


			; lda #$01
			; sta troubleshooting.hook	
			
			
.UPDATE.SCREEN_LIST

	LDX #$00 ;init INV_4.1b.SCREEN_ITEM_LIST.ARRAY index
	LDY #$00 ;init loop counter (item # 0-5)
.LOOP.UPDATE.SCREEN_LIST ;**OPT** Memory. A subroutine could be setup to init the pointer, a JSR to which could replace this pointer set and the one above 
		
			; cpy #$04
			; BNE .TEMP
			; lda #$01
			; sta troubleshooting.hook

						

; .TEMP
			
		LDA INV_4.1b.SCREEN_ITEM_LIST.ARRAY+$0,X ;load LO byte of inventory record of selected item
		STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$0	
		LDA INV_4.1b.SCREEN_ITEM_LIST.ARRAY+$1,X ;load HO byte of inventory record of selected item
		STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
		;INV_4.ACTIVE_PLAYER: use existing value	
	JSR INV_4.CALCULATE.READIED_QTY.FIELDS
		;RETURN: INV_4.NEXT_ITEM.READIED_STATUS, INV_4.1b.NEXT_ITEM.QTY_AVAILABLE


				
		;X-REG (INV_4.1b.SCREEN_ITEM_LIST.ARRAY index): use existing value
		;INV_4.1b.NEXT_ITEM.QTY_AVAILABLE: use existing value
		;INV_4.NEXT_ITEM.READIED_STATUS: use existing value
		;PLAYER_MERCH.INVENTORY.DATA.POINTER(2): use existing value
	JSR INV_4.1a.SAVE_ITEM

;EXIT TEST
	CPY INV_4.1b.SCREEN_LIST.TOTAL_RECORDS ;have all items on screen been updated?
	BCS .LOOP.UPDATE.SCREEN_LIST.DONE ;if yes, then exit loop
	
;INCREMENT INDEX
	INX
	INX
	INX
	INX ;increment INV_4.1b.SCREEN_ITEM_LIST.ARRAY index

	INY ;increment loop counter (item # 0-5)
	JMP .LOOP.UPDATE.SCREEN_LIST ;if no, continue loop
	
.LOOP.UPDATE.SCREEN_LIST.DONE
	
				; lda #$aa
				; ldx INV_4.1b.SCREEN_LIST.TOTAL_RECORDS
				; jsr prep.brk
				; brk
				
; ;=====DEBUG HOOK======
		; STX TEMP
		
		; LDX #$00
; .DEBUG.COPY2
		; LDA INV_4.1b.SCREEN_ITEM_LIST.ARRAY,X
		; STA $BD00,X
		; INX
		; BNE .DEBUG.COPY2

		; LDX #$00
; .DEBUG.COPY3
		; LDA INV_4.1a.SCREEN_ITEM_LIST.DATA,X
		; STA $BE00,X
		; INX
		; BNE .DEBUG.COPY3

		; LDA #$AA
		; LDX TEMP
		; ;yreg
		; JSR PREP.BRK
		; BRK
		
		LDA #TWS.LW.INVENTORY.TOP_LINE
		STA INV.ERASE.START_LINE
	JSR INV.ERASE.PLAYER_INV.TEXT_WINDOW



;PRINT UPDATED ITEM LIST
	JSR INV_4.GROUP2.PRINT.READIED_WEIGHT ;since readied items changed, the total weight of the readied items changed
	JSR INV_4.PRINT.ALL.ITEMS ;print all items visible on the video screen
	JSR FLIP.PAGE
	
				;INC troubleshooting.hook
				

	JMP INV_4.STATE.LOOP ;prints the item selector before falling through to the main state loop
					
	;JMP INV_4.INIT.DISPLAY.ALL_ITEMS ;prints all items, the item selector, then falls through to INV_4.STATE.LOOP.ALT.ENTRANCE
	
.EXIT

; *add code to a unready/ready submodule entrance in the main INV entrance that detects whether the sub-module is loaded or not (check a flag) and if not load the module from disk, and once loaded JSR it. 


	
	JMP INV_4.STATE.LOOP.ALT.ENTRANCE

	
@END

	
;PRINT TEXT	
INV_4.GROUP2.PRINT.READIED_WEIGHT
@START
;PARAMETERS: none
;ENTRANCE: direct from any INV_4 routine
;RETURN: updated video screen (static text portion of scroll window)

;**OPT** Memory. this section seems like it would be more efficient with screenbuilder

					; PLA
					; TAX
					; PLA
					; TAY
					; LDA #$BB
					; JSR PREP.BRK
					; BRK
					
					
;PRINT LABEL
		LDA #TWS.INVENTORY.SCROLL_WINDOW.LEFT_SBYTE
		STA HTAB
		LDA #TWS.INVENTORY.SCROLL_WINDOW.STATIC_TEXT.ROW
		STA VTAB
	JSR	UPDATE.CHAR.POS	
	
		LDA #INV_4.SCROLL_WINDOW.READIED_WEIGHT.LINE1
		STA STRING+$0
		LDA /INV_4.SCROLL_WINDOW.READIED_WEIGHT.LINE1
		STA STRING+$1
	JSR PRINT.STR

		LDA #TWS.INVENTORY.SCROLL_WINDOW.LEFT_SBYTE
		STA HTAB
		LDA #TWS.INVENTORY.SCROLL_WINDOW.STATIC_TEXT.ROW
		INC VTAB ;advance to next row
	JSR	UPDATE.CHAR.POS	

		LDA #INV_4.SCROLL_WINDOW.READIED_WEIGHT.LINE2
		STA STRING+$0
		LDA /INV_4.SCROLL_WINDOW.READIED_WEIGHT.LINE2
		STA STRING+$1
	JSR PRINT.STR



;PRINT VALUE


				; lda #$aa
				; LDx CHR_SHEET.PC.READIED_EQUIPMENT_WEIGHT ;load total readied equipment weight for active PC (unlike most other derived character sheet fields, this value is always up to date becuase the ready/unready routine modifyies it as readied equipment changes)
				; ldy INV_4.ACTIVE_PLAYER
				; jsr prep.brk
				; brk
				
		LDA CHR_SHEET.PC.READIED_EQUIPMENT_WEIGHT ;load total readied equipment weight for active PC (unlike most other derived character sheet fields, this value is always up to date becuase the ready/unready routine modifyies it as readied equipment changes)
		;LDA #$64
		; CLC ;(CLC = don't print CR | SEC = print CR to text window)
	JSR PRINT.HEX8.RJ

	
	; JSR CONVERT.HEX.8_TO_BCD.16
		; ;RETURN VALUE: BCD(2)
		
		; ;Parm BCD: value set above
	; JSR PRINT.BCD_PACKED

	
	
	RTS
	
@END
	
INV_4.GROUP2.PRINT.MISC_TEXT	
@START
;PARAMETERS: ACC = ($00 item readied | $01 = item not readied: item is not readiable | $02 don't use| $03 item not readied: STR gate check failed | $04 item not readied: DEX gate check failed | $05 item not readied: INT gate check failed | $06 can't ready armor during combat | $07 item unreadied | $08 prompt spell hotkey to ready)
;ENTRANCE: any INV_4 routine
;RETURN: scroll text output

;SAVE PARAMETERS
	
	;ACC: parm: text block code
	STA INV_4.GROUP2.PRINT.MISC_TEXT.MODE
	
; ;SAVE REGISTERS
	; TXA
	; PHA


.START	

	;TAX ;save parm text block to print
	JSR INV_4.1a.RESTORE.TW.CURSOR
	; PLA ;restore parm text block to print 
	;TXA ;restore parm text block to print 
	LDA INV_4.GROUP2.PRINT.MISC_TEXT.MODE		
	;TAX ;save parm text block to print
	CMP #$01
	BCC .BRANCH.TEXT_BLOCK
	CMP #$07
	BCS .BRANCH.TEXT_BLOCK

	;**FALLS THROUGH**
	
.PLAY_SOUND
	JSR PLAY.SOUND.DUMB_ASS
	;TXA ;restore parm text block to print 
	;LDA COW
	
	;**FALLS THROUGH**

.BRANCH.TEXT_BLOCK
	
	
				; STA TEMP
				; LDA #$AA
				; LDX TEMP
				; LDY $BE00
				; ; LDX TWF.STRING
				; ; LDY TWF.STRING+$1
				; JSR PREP.BRK
				; BRK

	
;**OPT** Memory. The text prints below could be handled by using ACC*2 as the index to a lookup table 
;which contains 2 byte records containing the memory address of each text block, which is used to set the TWF.STRING pointer.

				
	LDA INV_4.GROUP2.PRINT.MISC_TEXT.MODE	;($00 item readied | $01 = item not readied: item is not readiable | $02 don't use| $03 item not readied: STR gate check failed | $04 item not readied: DEX gate check failed | $05 item not readied: INT gate check failed | $06 can't ready armor during combat | $07 item unreadied | $08 prompt spell hotkey to ready)
	CMP #$00
	BEQ .ITEM_READIED
	CMP #$01
	BEQ .ITEM_NOT_READIABLE	
	CMP #$03 ;STR
	BEQ .STR_GATE_CHECK.FAILED
	CMP #$04 ;DEX
	BEQ .DEX_GATE_CHECK.FAILED
	CMP #$05 ;INT
	BEQ .INT_GATE_CHECK.FAILED
	CMP #$06
	BEQ .NO.ARMOR_CHANGE_IN_COMBAT	
	CMP #$07
	BEQ .ITEM_UNREADIED
	CMP #$08
	BEQ .SPELL_HOTKEY_READY_PROMPT

	;**FALLS THROUGH**


	
.ERROR
;Unexpected text block to print code in INV_4.GROUP2.PRINT.MISC_TEXT. This parm is sometimes set by the return value of
;INV.READY_UNREADY.EQUIPMENT.ENTRANCE, so the problem might be a garbage return value or a return value that isn't setup to be managed.
	JSR PREP.BRK
	BRK
	

	

	;**OPT** Memory. Instead of the branch above, and code sections below, a lot of memory could be saved (~50% I think) if the ACC parm
	;was used as a lookup table index. The lookup table would contain the HO/LO of the text block. Set the HO/LO of the text blocks to the lookup table using
	;SBASM directives. 
	
.ITEM_READIED
		LDA #INV_4.SCROLL_WINDOW.READIED			
		STA TWF.STRING
		
		LDA /INV_4.SCROLL_WINDOW.READIED
		STA TWF.STRING+$1						
	
	JMP .EXECUTE.PRINT
	
.ITEM_NOT_READIABLE
		LDA #INV_4.SCROLL_WINDOW.NOT_READIABLE			
		STA TWF.STRING
		
		LDA /INV_4.SCROLL_WINDOW.NOT_READIABLE
		STA TWF.STRING+$1						

	JMP .EXECUTE.PRINT

.STR_GATE_CHECK.FAILED
		LDA #INV_4.SCROLL_WINDOW.STR_GATE_CHECK.FAILED			
		STA TWF.STRING
		
		LDA /INV_4.SCROLL_WINDOW.STR_GATE_CHECK.FAILED
		STA TWF.STRING+$1						
	
	JMP .EXECUTE.PRINT

	
.DEX_GATE_CHECK.FAILED
		LDA #INV_4.SCROLL_WINDOW.DEX_GATE_CHECK.FAILED			
		STA TWF.STRING
		
		LDA /INV_4.SCROLL_WINDOW.DEX_GATE_CHECK.FAILED
		STA TWF.STRING+$1						
	
	JMP .EXECUTE.PRINT
	
.INT_GATE_CHECK.FAILED
		LDA #INV_4.SCROLL_WINDOW.INT_GATE_CHECK.FAILED			
		STA TWF.STRING
		
		LDA /INV_4.SCROLL_WINDOW.INT_GATE_CHECK.FAILED
		STA TWF.STRING+$1						

	JMP .EXECUTE.PRINT
	
.NO.ARMOR_CHANGE_IN_COMBAT
		LDA #INV_4.SCROLL_WINDOW.NO.ARMOR_CHANGE_IN_COMBAT			
		STA TWF.STRING
		
		LDA /INV_4.SCROLL_WINDOW.NO.ARMOR_CHANGE_IN_COMBAT
		STA TWF.STRING+$1						

	JMP .EXECUTE.PRINT
		
.ITEM_UNREADIED
		LDA #INV_4.SCROLL_WINDOW.UNREADIED			
		STA TWF.STRING
		
		LDA /INV_4.SCROLL_WINDOW.UNREADIED
		STA TWF.STRING+$1						
	JMP .EXECUTE.PRINT

		
.SPELL_HOTKEY_READY_PROMPT
		LDA #INV_4.SCROLL_WINDOW.HOTKEY_PROMPT				
		STA TWF.STRING
		
		LDA /INV_4.SCROLL_WINDOW.HOTKEY_PROMPT
		STA TWF.STRING+$1						


	;**FALLS THROUGH**

	
.EXECUTE.PRINT
	JSR PRINT.TEXT.WINDOW

	
.EXIT	
	
	JSR INV_4.1a.SAVE.TW.CURSOR

; ;RESTORE REGISTERS
	; PLA
	; TAX
	
	RTS
	
@END


@END


;INV_4.1 LOCAL TEXT BLOCKS (also see INV_4.1b LOCAL TEXT BLOCKS)
@START




@END


INV_4.LOCAL.SUBROUTINES.END
