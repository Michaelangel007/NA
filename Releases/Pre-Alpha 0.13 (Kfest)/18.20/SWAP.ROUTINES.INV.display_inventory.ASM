;************************INCLUDE FILE*****************************
;(do not assemble stand-alone.  )


;=====================INCLUDE FILE DOCUMENTATION====================================
;
;Include file to SWAP.ROUTINES.INVENTORY.ASM
;
;=================================================================================


INV_4.DISPLAY_INVENTORY
@START
;PARAMETERS: INV_4.DISPLAY.MODE= ($00 = display weapons | $01 = display armor | $02 = misc items), INV_4.ACTIVE_PLAYER

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
;
;-Misc notes: 
;*The inventory text window is not setup for line wrapping. However, it is setup to
;scroll text up and down, from one item to the next within the layout. 
;
;*Once the initial 6 items are printed to the inventory window PLAYER.INVENTORY.DATA.POINTER 
;is used when scrolling up/down to find the next relevant item. This works because the pointer
;is first set to the inventory record (PLAYER.INVENTORY.DATA) LO/HO memory address of the
;top or bottom item on-screen, depending on the direction of the scroll. 
;
;=================================================================================




;****INV DEBUG LOG*** 			;**OPT** Memory. Remove.
@START
.DEBUG.LABEL
		;PHA
		LDA #.DEBUG.LABEL
		STA INV.DEBUG.ADDRESS+$0
		LDA /.DEBUG.LABEL
		STA INV.DEBUG.ADDRESS+$1
	JSR INV.DEBUG.LOG.WRITE
		;PLA
@END
	
.ERROR_TRAP
@START
	LDA INV_4.ACTIVE_PLAYER
	BEQ .ERROR.ACTIVE_PLAYER.OUT_OF_RANGE
	CMP #$07
	BCC .ERROR_TRAP.DONE
	
	;**FALLS THROUGH**
	
.ERROR.ACTIVE_PLAYER.OUT_OF_RANGE
;.ERROR_TRAP (INV_4.DISPLAY_INVENTORY) reports a value != 1-6 in INV_4.ACTIVE_PLAYER
	JSR PREP.BRK
	BRK

.ERROR.INVENTORY_FILE.NOT_OPEN
;.INIT.INVENTORY.DATA reports that inventory file was not open. It should be at this point in the program flow and if it is not that
;indicates a bug or design flaw. Opening the file at this point in the program should be avoided because opening a file is a slow process. 	
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
		;PHA
		LDA #.DEBUG.LABEL2
		STA INV.DEBUG.ADDRESS+$0
		LDA /.DEBUG.LABEL2
		STA INV.DEBUG.ADDRESS+$1
	JSR INV.DEBUG.LOG.WRITE
		;PLA
@END

						
						; lda #$aa
						; ldx #$aa
						; ldy #$aa
						; jsr prep.brk
						; brk
						
;READ INVENTORY DATA FROM FILE
	JSR INV.FILE.READ.INVENTORY_DATA.ENTRANCE
		;RETURN VALUE: INV.FILE.READ.INVENTORY.OPEN_STATUS ;($00 = file was open. $01 = file was not open)
	CMP #$00
	BNE .ERROR.INVENTORY_FILE.NOT_OPEN

; ;SORT INVENTORY ARRAY BY LEVEL
; .SORT.INVENTORY_ARRAY
	; ;set inventory array length
	; LDA #INV.PLAYER.INVENTORY.TEST_DATA.BYTES.USED  
	; STA INV.PLAYER.INVENTORY.DATA-2
	; LDA /INV.PLAYER.INVENTORY.TEST_DATA.BYTES.USED
	; STA INV.PLAYER.INVENTORY.DATA-1
	
		; ;setup record size
		; LDA #$06
		; ;LDA #$06
		; STA array.record_size			
		; ;setup pointer
		; LDA #INV.PLAYER.INVENTORY.DATA-2	
			; ;LDA #$FE
		; STA sort.table.address
		; LDA /INV.PLAYER.INVENTORY.DATA-2	
			; ;LDA #$B9
		; STA sort.table.address+$1
	; JSR INSERTION_SORT
	
					;jmp .DEBUG.PRINT_1.DONE
					
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
	JSR INV.ERASE.TEXT_WINDOW
	JSR FLIP.PAGE
	JSR INV_4.SYNC_PAGES


;DRAW & ERASE SCROLL WINDOW (LOWER RIGHT)
	JSR INV_4.DRAW_ERASE.SCROLL_WINDOW

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
	
.DRAW.LINE
;(when both hi-res pages need to stay in sync, instead of drawing to both pages, sometimes it is best to the pages individually
;and flip pages inbetween. This way the draw isn't as noticable to the user)
		
					; LDA #$01
					; STA TROUBLESHOOTING.HOOK
					
		LDA #$07 ;BIT0-2 set: use both hi-res pages. draw top line of rectangle. ignore all other bit flags.
	JSR DRAW.LINE



@END


.INIT.SCROLL_TEXT_WINDOW
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

	



;INIT STATIC TEXT SECTION OF WINDOW
	JSR INV_4.PRINT.READIED_WEIGHT

;INIT SCROLL TEXT SECTION OF WINDOW	
		;INIT TEXT WINDOW BOUNDARIES (for the text itself, excluding borders)
		LDA #TWS.INVENTORY.SCROLL_WINDOW.LEFT_SBYTE	
		STA TWF.LEFT_SBYTE
		
		LDA #TWS.INVENTORY.SCROLL_WINDOW.WIDTH
		STA TWF.WIDTH
		
		LDA #TWS.INVENTORY.SCROLL_WINDOW.TOP_ROW
		STA TWF.TOP_ROW
		
		LDA #TWS.INVENTORY.SCROLL_WINDOW.BOTTOM_ROW
		STA TWF.BOTTOM_ROW

	
		;SET TEXT WINDOW FUNCTION (TWF) CURSOR POSITION VARIABLES TO UPPER LEFT OF TEXT WINDOW
		LDA #TWS.INVENTORY.SCROLL_WINDOW.CURSOR_START_SBYTE
		STA TWF.HTAB.SAVED
		STA HTAB
			
		LDA #TWS.INVENTORY.SCROLL_WINDOW.CURSOR_START_ROW
		STA TWF.VTAB.SAVED	
		STA VTAB
	JSR	UPDATE.CHAR.POS
	
	;PRINT INTRO TEXT		
		LDA #INV_4.SCROLL_WINDOW.INTRO					
		STA TWF.STRING
		
		LDA /INV_4.SCROLL_WINDOW.INTRO
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW
	
	
	;SAVE SCROLL TEXT WINDOW CURSOR POSITION
	LDA HTAB
	STA TWF.HTAB.SAVED
	
	LDA VTAB
	STA TWF.VTAB.SAVED	


		
@END

.INIT.GENERAL
@START

;init INV_4.SCREEN_ITEM_LIST.ARRAY
	LDX #$00
.INIT.LOOP
	LDA #$FF ;also used as array stop value (high-bit set)
	STA INV_4.SCREEN_ITEM_LIST.ARRAY,X
	INX
	CPX #INV_4.SCREEN_ITEM_LIST.ARRAY.ARRAY_SIZE
	BNE .INIT.LOOP

	LDA #$00
	STA INV_4.SCREEN_ITEM_LIST.SELECTED.ITEM
	;STA INV_4.NEXT_ITEM.READIED_STATUS
	
	LDA #INV.PLAYER.INVENTORY.DATA.LESS_ONE_RECORD
	STA PLAYER.INVENTORY.DATA.POINTER+$0
	LDA /INV.PLAYER.INVENTORY.DATA.LESS_ONE_RECORD
	STA PLAYER.INVENTORY.DATA.POINTER+$1

	
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



					; lda #$ab
					; jsr prep.brk
					; brk
						
@END

		
.INIT.IDENTIFY_ITEMS
@START
;IF readied status for active player is true or QTY_AVAILABLE > 0
;	THEN add to INV_4.SCREEN_ITEM_LIST.ARRAY and set byte 3 and 4 to QTY_AVAILABLE and readied status, set byte 5-6 to 16-bit inventory table record # address (i.e. save pointer)
	
	LDY #$00 ;init loop counter (on screen item counter)
	LDX #$00 ;init index to INV_4.SCREEN_ITEM_LIST.ARRAY (4 byte records)
.LOOP.GET.ITEMS
				
				
				;STY COW ;DEBUG: save loop counter

					; LDA #$01
					; sta troubleshooting.hook
					
		LDA #$00 ;($00 = iterate down | $01 = iterate up)
	JSR INV_4.GET.NEXT_ITEM
		;RETURN VALUE: ACC ($00 = next item found, $01 = top or bottom of array reached), INV_4.NEXT_ITEM.QTY_AVAILABLE, INV_4.NEXT_ITEM.READIED_STATUS, PLAYER.INVENTORY.DATA.POINTER(2)
	
		
	;exit test
	CMP #$01 ;was top/bottom of array reached? (i.e. there is no next item)
	;BEQ .LOOP.GET.ITEMS.DONE.ALT ;branch if yes (the .ALT bypasses the DEY, which isn't needed because by exiting the loop here, the increment counter code at the end of each iteration isn't run)
	BEQ .LOOP.GET.ITEMS.DONE ;branch if yes
	
				; lda #INV_4.SCREEN_ITEM_LIST.ARRAY
				; sta $be00
				; lda /INV_4.SCREEN_ITEM_LIST.ARRAY
				; sta $be01
				; lda INV_4.NEXT_ITEM.READIED_STATUS
				; sta $be02
				; lda INV_4.NEXT_ITEM.QTY_AVAILABLE
				; sta $be03
				; STA TEMP
				; CPY #$02
				; BNE .TEMP
				; LDA #$AA
				; ;LDX TEMP
				; JSR PREP.BRK
				; BRK
; .TEMP
				; LDA TEMP
	

.VALIDATE.ITEM
	;should item be displayed onscreen?
	LDA INV_4.NEXT_ITEM.READIED_STATUS ;($00 = not readied | $01 = readied by active player | $02 = readied twice by active player)
	BNE .VALIDATE.ITEM.DONE ;if item is readied by active player then item should be displayed
	LDA INV_4.NEXT_ITEM.QTY_AVAILABLE ;if there are no units of the item available to be readied by the active player then there is no reason to display the item. 
	BEQ .LOOP.GET.ITEMS ;get next item. INV_4.GET.NEXT_ITEM will pick up at the position it left off at in the inventory table.  
.VALIDATE.ITEM.DONE
	
	;**FALLS THROUGH** 	;Since the party owns more of this item than is readied by the PCs, then the item should be displayed
	

		;save item
	
		;INV_4.NEXT_ITEM.QTY_AVAILABLE: use existing value
		;INV_4.NEXT_ITEM.READIED_STATUS: use existing value
		;PLAYER.INVENTORY.DATA.POINTER: use existing value
		;X-REG (INV_4.SCREEN_ITEM_LIST.ARRAY index): use existing value 
	JSR INV_4.SAVE_ITEM ;saves item if valid


	
	;**FALLS THROUGH**

.INCREMENT.COUNTERS_INDEXES
	INY ;increment loop counter (on screen item counter)
	TYA
	ASL ;X2
	ASL ;X4
	TAX ;save incremented index to INV_4.SCREEN_ITEM_LIST.ARRAY (4 byte records)

	CPY #INV_4.MAX_ONSCREEN.ITEMS
	BNE .LOOP.GET.ITEMS
.LOOP.GET.ITEMS.DONE
	DEY ;reverse last loop counter increment so that INV_4.SCREEN_LIST.TOTAL_RECORDS values will be 0-5
;.LOOP.GET.ITEMS.DONE.ALT
	STY INV_4.SCREEN_LIST.TOTAL_RECORDS ;total items on screen. If > #INV_4.MAX_ONSCREEN.ITEMS that means less than #INV_4.MAX_ONSCREEN.ITEMS in the party inventory are relevant for display to the active player
	CPY #$FF ;if inventory screen empty? (if the loop counter, Y-REG, is $FF then the end of array was reached before any inventory records were found that match the mode parm for the current call to this routine. It's $FF not $00 because the DEY above will underflow Y-REG)
	;BEQ INV_4.STATE.LOOP.ALT.ENTRANCE ;branch if yes (no need to print the item data on screen if there are no items)
	BNE .COW
	JMP INV_4.STATE.LOOP.ALT.ENTRANCE ;branch if yes (no need to print the item data on screen if there are no items)
.COW
.INIT.SCREEN_ITEM_LIST

					; lda #$aa
					; jsr prep.brk
					; brk
					

			
			
@END


 ;***OPT** Memory. Can be removed once bug is resolved. 
.DEBUG_TRAP ;garabge data in inventory display (INV_4) 
@START
		sta temp
		lda INV.PLAYER.INVENTORY.DATA+$C
		cmp #$80
		bne .DEBUG_TRAP.REPORT_ERROR2
		lda INV.PLAYER.INVENTORY.DATA+$D
		cmp #$07
		bne .DEBUG_TRAP.REPORT_ERROR2
		lda INV.PLAYER.INVENTORY.DATA+$E
		cmp #$01
		bne .DEBUG_TRAP.REPORT_ERROR2
		JMP .DEBUG_TRAP.NO_ERROR
		
.DEBUG_TRAP.REPORT_ERROR2
;.DEBUG_TRAP (INV.FILE.READ.INVENTORY_DATA.ENTRANCE) reports that INV.PLAYER.INVENTORY.DATA does not contain
;the test bytes expected. This trap was created to track a bug causing garbage to be displyed in the inventory window. 
;based on some initial testing, when the bug occurs, the inventory data array ($BA00-$BFFF) contains all $00 values except for
;a portion of the $BD00 page. Thus, the theory is that when the bug occurs, the data in the file never makes it into memory for some reason.
	LDA #$EE
	LDX #$EE
	LDY #$EE
	JSR PREP.BRK
	BRK
	
.DEBUG_TRAP.NO_ERROR
		
		lda temp
		
@END
;
;There is a different trap right after the file read. This trap is intended to detect a scenario where the data reads into memory from the file okay
;but gets corrputed later in the program flow. The location of this trap is somewhat aribtary. It's after the file read and before
;INV_4.STATE.LOOP.ALT.ENTRANCE. The bug is visible by INV_4.STATE.LOOP.ALT.ENTRANCE.

		;JMP DEBUG.HOOK

		
.INIT.GET.ITEM.DATA
@START
;=====================CODE-SECTION DOCUMENTATION=======================================================
;
;This routine assembles all data needed for the first 6 items to be displayed on 
;the video screen, which is saved for future use when modifcations are made to onscreen 
;items that requires their data to be printed to the video screen again. 
;
;INV_4.SCREEN_ITEM_LIST.ARRAY is iterated, for each item the item record is read from the master item table, 
;and the data is saved to INV_4.SCREEN_ITEM_LIST.DATA
;
;=========================================================================================================


	LDY #$00 ;init loop counter (on screen item counter)
.OUTER_LOOP.GET.ITEMS

;SAVE COUNTER/INDEXES
		STY INV_4.GET_ITEMS.LOOP_COUNTER  ;SAVE loop counter
			
	;init indexes
	TYA
	ASL ;X2
	ASL ;X4
	TAX ;set INV_4.SCREEN_ITEM_LIST.ARRAY index ($4 byte records)
	
	;exit test (no more items)
	LDA INV_4.SCREEN_ITEM_LIST.ARRAY+$03,X ;load readied status field
	BMI .INIT.GET.ITEM.DATA.DONE ;branch if init value detected
	
	;init pointer to current item
	LDA INV_4.SCREEN_ITEM_LIST.ARRAY+$0,X ;load LO byte of inventory record
	STA PLAYER.INVENTORY.DATA.POINTER+$0
	LDA INV_4.SCREEN_ITEM_LIST.ARRAY+$1,X ;load HO byte of inventory record
	STA PLAYER.INVENTORY.DATA.POINTER+$1
	
	;read item record from master item file
		;(get parameters from the inventory record of current item)
		LDY #$01 ;set index to byte $1 (item_ID) of inventory record
		LDA (PLAYER.INVENTORY.DATA.POINTER),Y
		STA FILE.ITEM_TABLE.ID
		DEY ;backup to byte $0 (LEVEL | ITEM_TYPE) of inventory record
		LDA (PLAYER.INVENTORY.DATA.POINTER),Y
		;ITEM_TYPE is in LO nibble
		AND #$0F ;mask-out HO nibble. 
			;ACC = item table code ($00 = weapon | $01 armor | $02 misc item)
	JSR FILE.READ.ITEM_TABLES.ENTRANCE
		;RETURN: FILE.ITEM_TABLE.RECORD.READ($20)	


					; LDA #INV_4.SCREEN_ITEM_LIST.DATA
					; STA $BE00					
					; LDA /INV_4.SCREEN_ITEM_LIST.DATA
					; STA $BE01	
					; LDA #INV_4.SCREEN_ITEM_LIST.ARRAY
					; STA $BE02					
					; LDA /INV_4.SCREEN_ITEM_LIST.ARRAY
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
	LDA INV_4.GET_ITEMS.LOOP_COUNTER  ;load loop counter
	ASL ;X !2
	ASL ;X !4
	ASL ;X !8
	ASL ;X !16
	ASL ;X !32
	TAX ;set INV_4.SCREEN_ITEM_LIST.DATA index ($20 byte records)
		
					; LDA #$69
					; STA FILE.ITEM_TABLE.RECORD.READ+$0
					; LDA #$79
					; STA FILE.ITEM_TABLE.RECORD.READ+$1
					
	LDY #$00 ;init FILE.ITEM_TABLE.RECORD.READ index
.INNER_LOOP.COPY.DATA
	LDA FILE.ITEM_TABLE.RECORD.READ,Y
	
				;STA TEMP
				
	STA INV_4.SCREEN_ITEM_LIST.DATA,X
				
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
				; LDX #INV_4.SCREEN_ITEM_LIST.DATA
				; LDY /INV_4.SCREEN_ITEM_LIST.DATA
				; JSR PREP.BRK
				; BRK
				
	INX	;increment INV_4.SCREEN_ITEM_LIST.DATA index
	INY ;increment FILE.ITEM_TABLE.RECORD.READ index
	CPY #FILE.ITEM_TABLE.RECORD.SIZE
	BNE .INNER_LOOP.COPY.DATA


			
;RESTORE COUNTER/INDEXES	
	LDY INV_4.GET_ITEMS.LOOP_COUNTER  ;RESTORE loop counter

	

.EXIT_TEST ;(max items reached)
	INY ;increment loop counter
	CPY #INV_4.MAX_ONSCREEN.ITEMS
	BNE .OUTER_LOOP.GET.ITEMS
	
.INIT.GET.ITEM.DATA.DONE	
	
@END
		
			;JMP DEBUG.HOOK

;DEBUG.HOOK
@START
; ;DEBUG: move INV_4.SCREEN_ITEM_LIST.DATA
				; LDX #$00
; .DEBUG.LOOP.INV_4.SCREEN_ITEM_LIST.ARRAY
				; LDA INV_4.SCREEN_ITEM_LIST.ARRAY,X
				; STA $BC00,X
				; INX
				; BNE .DEBUG.LOOP.INV_4.SCREEN_ITEM_LIST.ARRAY


				; LDX #$00
; .DEBUG.LOOP.INV_4.SCREEN_ITEM_LIST.DATA
				; LDA INV_4.SCREEN_ITEM_LIST.DATA,X
				; STA $BD00,X
				; INX
				; BNE .DEBUG.LOOP.INV_4.SCREEN_ITEM_LIST.DATA

				; LDX #$00
; .DEBUG.LOOP.FILE.ITEM_TABLE.RECORD.READ
				; LDA FILE.ITEM_TABLE.RECORD.READ,X
				; STA $BE00,X
				; INX
				; BNE .DEBUG.LOOP.FILE.ITEM_TABLE.RECORD.READ

				

				; LDA #INV_4.SCREEN_ITEM_LIST.ARRAY
				; STA $BF01					
				; LDA /INV_4.SCREEN_ITEM_LIST.ARRAY
				; STA $BF02					
				; LDA #INV_4.SCREEN_ITEM_LIST.DATA
				; STA $BF03					
				; LDA /INV_4.SCREEN_ITEM_LIST.DATA
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

			
INV_4.INIT.DISPLAY.ALL_ITEMS
	JSR INV_4.PRINT.ALL.ITEMS ;print all items visible on the video screen
	JSR FLIP.PAGE
		

		
.INIT.ACTIVE_ITEM_SELECTOR
	;LDA #$00
	LDA #$00
	STA INV_4.SCREEN_ITEM_LIST.SELECTED.ITEM ;set item selector to item0 (first item)

@END

INV_4.STATE.LOOP
@START
;DISPLAY ITEM SELECTOR
		LDA #$7F ;set mode ($00 = item selector OFF | $7F = item selector ON) 
		;LDA #$00 ;set mode ($00 = item selector OFF | $7F = item selector ON)
		;INV_4.SCREEN_ITEM_LIST.SELECTED.ITEM: use existing value
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
		;PHA
		LDA #.DEBUG.LABEL
		STA INV.DEBUG.ADDRESS+$0
		LDA /.DEBUG.LABEL
		STA INV.DEBUG.ADDRESS+$1
	JSR INV.DEBUG.LOG.WRITE
		;PLA
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

.HOTKEY.CHECK ;test for keypress of 1-6
	CMP #$B1			
	BCC .HOTKEY.CHECK.DONE
	CMP #$B7
	BCC INV_4.EXIT ;exit, then goto the menu associated with the hotkey
.HOTKEY.CHECK.DONE	
	
	CMP #$89			;TAB (next menu)
	BEQ INV_4.EXIT

	CMP #$8B			;UP ARROW
	BEQ INV_4.COMMAND.CHANGE.ACTIVE_PLAYER.UP
	CMP #$8A			;DOWN ARROW
	BEQ INV_4.COMMAND.CHANGE.ACTIVE_PLAYER.DOWN	

	CMP #$9B			;ESC (EXIT COMBAT)
	BEQ	INV_4.EXIT
	
	;is inventory empty?
	LDX INV_4.SCREEN_LIST.TOTAL_RECORDS ;(0-5 = item # of last item on screen | $FF = no inventory records are eligible for display)
	BMI INV_4.STATE.LOOP.ALT.ENTRANCE

	

	CMP #$88			;LEFT ARROW
	BEQ	INV_4.COMMAND.LEFT_ARROW
	CMP #$95			;RIGHT ARROW
	BEQ INV_4.COMMAND.RIGHT_ARROW
	CMP #$8D			;RETURN/ENTER (READY/UNREADY WEAPON)
	BEQ	INV_4.COMMAND.ENTER	
	CMP #$C9
	BEQ INV_4.COMMAND.INFO
	


	;playtest keys
	CMP #$A8			;SHIFT+9 (SET HOOK)
	BEQ	INV_4.PLAYTEST.KEY_SHIFT9

	CMP #$D1			;(Q) QUIT
	BEQ	INV_4.COMMAND.QUIT	

	
	;default case
	JMP .KEYIN.LOOP ;do nothing, wait for next keypress

	
INV_4.COMMAND.CHANGE.ACTIVE_PLAYER.UP
	JSR CHARACTER.ROSTER.INCREMENT_UP
	JMP INV_4.DISPLAY_INVENTORY
	
INV_4.COMMAND.CHANGE.ACTIVE_PLAYER.DOWN
	JSR CHARACTER.ROSTER.INCREMENT_DOWN
	JMP INV_4.DISPLAY_INVENTORY	

	
INV_4.COMMAND.TAB  ;**OPT** Memory. This can be removed. 
	JMP INV_4.EXIT
	
INV_4.COMMAND.LEFT_ARROW
	JMP INV_4.MOVE_SELECTOR_UP

INV_4.COMMAND.RIGHT_ARROW
	JMP INV_4.MOVE_SELECTOR_DOWN
	
INV_4.COMMAND.ENTER
;****INV DEBUG LOG*** 			;**OPT** Memory. Remove.
@START
.DEBUG.LABEL
		;PHA
		LDA #.DEBUG.LABEL
		STA INV.DEBUG.ADDRESS+$0
		LDA /.DEBUG.LABEL
		STA INV.DEBUG.ADDRESS+$1
	JSR INV.DEBUG.LOG.WRITE
		;PLA
@END

	JMP INV_4.READY_UNREADY.REQUEST

INV_4.COMMAND.INFO
	JMP INV_4.ADDITIONAL_INFO


INV_4.PLAYTEST.KEY_SHIFT9	
	JSR PLAYTEST.KEY0 ;the JSR is to KEY_0 because the main game loop uses KEY_0 and this routine is resident in the main game loop source file.  
	JMP INV_4.STATE.LOOP.ALT.ENTRANCE

INV_4.COMMAND.QUIT
	JMP COMMAND.QUIT.PREP

@END
@END


INV_4.EXIT
@START
;PARAMETERS: ACC = ASCII code of keypress

	PHA ;save state_loop keypress
	
;RESTORE TEXT WINDOW SETTINGS TO PRE-LAUNCH
;(if the inventory is launched from combat, we want to restore the combat text window upon exit). 

;copies TWF.LEFT_SBYTE.SAVED, TWF.WIDTH.SAVED, TWF.TOP_ROW.SAVED, TWF.BOTTOM_ROW.SAVED to corresponding primary variables
	LDX #$00
.LOOP.COPY.TWF
	LDA TWF.LEFT_SBYTE.SAVED,X
	STA TWF.LEFT_SBYTE,X
	INX
	CPX #$04
	BNE .LOOP.COPY.TWF

	
.PARSE.PARAMETER
	PLA ;restore state_loop keypress
	;ACC = ASCII code of keypress
	CMP #$9B ;ESC
	BEQ .EXIT.INVENORY.MODULE
	CMP #$89 ;was tab pressed?
	BNE .PARSE.PARAMETERS.DONE ;if tab wasn't pressed, that means a hotkey was pressed and the hotkey ASCII code ($B1-$B6) is the parm to INV.NEXT.MENU
	LDA #$00 ; load "tab pressed" code for INV.NEXT.MENU
	
.PARSE.PARAMETERS.DONE
	PHA ;save parm to INV.NEXT.MENU


;DRAW & ERASE SCROLL WINDOW (LOWER RIGHT)
;(this is so that there isn't left over text when other sub_modules (such as stats) have control and so that
;if the inventory module was launched from combat that upon exit back to combat the scroll window doesn't have any
;left over text in it. It is easier to clear the window here than in combat because the combat text window setup routines are in combat setup)
	JSR INV_4.DRAW_ERASE.SCROLL_WINDOW
			

		PLA ;restore parm to INV.NEXT.MENU. ;(high-bit not set = tab pressed, increment menu | $B1-B6 = ASCII code to hotkey to specific menu) 
	JMP INV.NEXT.MENU 

	
.EXIT.INVENORY.MODULE

	;Before exiting module, check if any ready/unreadies occured and if so update combat stats
	JSR INV.RUN.CALCULATE.COMBAT.STATS.IF_CHANGES_OCUURED

	JMP INV.INVENTORY_MODULE.EXIT
	
@END


;============================================================
@END

;LOCAL SUBROUTINES
@START


;*******REMOVE INV_4.PRINT_MOCKUP******
;INV_4.PRINT_MOCKUP
@START

; ;PRINT HEADERS
; @START

; ; ;READIED WEIGHT
		; ; LDA #$01	;#CONSTANT. location to print the label preceeding the variable value. 
		; ; STA HTAB	
		; ; LDA #$04 ;VTAB of the centered space. 
		; ; STA VTAB
	; ; JSR	UPDATE.CHAR.POS

		; ; LDA #INV_4.TEXT_BLOCK.TEST_HEADER1
		; ; STA STRING+$0
		; ; LDA /INV_4.TEXT_BLOCK.TEST_HEADER1
		; ; STA STRING+$1
	; ; JSR PRINT.STR
	
; ;QTY/WEIGHT COLUMN HEADERS
		; LDA #$01	;#CONSTANT. location to print the label preceeding the variable value. 
		; STA HTAB	
		; LDA #$04 ;VTAB of the centered space. 
		; STA VTAB
	; JSR	UPDATE.CHAR.POS

		; LDA #INV_4.TEXT_BLOCK.TEST_HEADER2
		; STA STRING+$0
		; LDA /INV_4.TEXT_BLOCK.TEST_HEADER2
		; STA STRING+$1
	; JSR PRINT.STR




	
; @END

; ;PRINT 1ST ITEM LIST
; @START

; ;ITEM 1.1
		; LDA #$01	;#CONSTANT. location to print the label preceeding the variable value. 
		; STA HTAB	
		; LDA #$06 ;VTAB of the centered space. 
		; STA VTAB
	; JSR	UPDATE.CHAR.POS

		; LDA #INV_4.TEXT_BLOCK.TEST1.1
		; STA STRING+$0
		; LDA /INV_4.TEXT_BLOCK.TEST1.1
		; STA STRING+$1
	; JSR PRINT.STR.BACKGROUND
	
				; ;JSR KEYIN

		; LDA #$01	;#CONSTANT. location to print the label preceeding the variable value. 
		; STA HTAB	
		; INC VTAB
	; JSR	UPDATE.CHAR.POS
	
		; LDA #INV_4.TEXT_BLOCK.TEST1.2
		; STA STRING+$0
		; LDA /INV_4.TEXT_BLOCK.TEST1.2
		; STA STRING+$1
	; JSR PRINT.STR.BACKGROUND

				; ;JSR KEYIN
	
	
; ;ITEM 2.1
		; LDA #$01	;#CONSTANT. location to print the label preceeding the variable value. 
		; STA HTAB	
		; INC VTAB
		; INC VTAB
	; JSR	UPDATE.CHAR.POS

		; LDA #INV_4.TEXT_BLOCK.TEST2.1
		; STA STRING+$0
		; LDA /INV_4.TEXT_BLOCK.TEST2.1
		; STA STRING+$1
	; JSR PRINT.STR.BACKGROUND

		; LDA #$01	;#CONSTANT. location to print the label preceeding the variable value. 
		; STA HTAB	
		; INC VTAB
	; JSR	UPDATE.CHAR.POS
	
		; LDA #INV_4.TEXT_BLOCK.TEST2.2
		; STA STRING+$0
		; LDA /INV_4.TEXT_BLOCK.TEST2.2
		; STA STRING+$1
	; JSR PRINT.STR.BACKGROUND
	
; ;ITEM 3.1
		; LDA #$01	;#CONSTANT. location to print the label preceeding the variable value. 
		; STA HTAB	
		; INC VTAB
		; INC VTAB
	; JSR	UPDATE.CHAR.POS

		; LDA #INV_4.TEXT_BLOCK.TEST3.1
		; STA STRING+$0
		; LDA /INV_4.TEXT_BLOCK.TEST3.1
		; STA STRING+$1
	; JSR PRINT.STR.BACKGROUND

		; LDA #$01	;#CONSTANT. location to print the label preceeding the variable value. 
		; STA HTAB	
		; INC VTAB
	; JSR	UPDATE.CHAR.POS
	
		; LDA #INV_4.TEXT_BLOCK.TEST3.2
		; STA STRING+$0
		; LDA /INV_4.TEXT_BLOCK.TEST3.2
		; STA STRING+$1
	; JSR PRINT.STR.BACKGROUND	
	
; ;ITEM 4.1
		; LDA #$01	;#CONSTANT. location to print the label preceeding the variable value. 
		; STA HTAB	
		; INC VTAB
		; INC VTAB
	; JSR	UPDATE.CHAR.POS

		; LDA #INV_4.TEXT_BLOCK.TEST4.1
		; STA STRING+$0
		; LDA /INV_4.TEXT_BLOCK.TEST4.1
		; STA STRING+$1
	; JSR PRINT.STR.BACKGROUND

		; LDA #$01	;#CONSTANT. location to print the label preceeding the variable value. 
		; STA HTAB	
		; INC VTAB
	; JSR	UPDATE.CHAR.POS
	
		; LDA #INV_4.TEXT_BLOCK.TEST4.2
		; STA STRING+$0
		; LDA /INV_4.TEXT_BLOCK.TEST4.2
		; STA STRING+$1
	; JSR PRINT.STR.BACKGROUND	
	
; ;ITEM 5.1
		; LDA #$01	;#CONSTANT. location to print the label preceeding the variable value. 
		; STA HTAB	
		; INC VTAB
		; INC VTAB
	; JSR	UPDATE.CHAR.POS

		; LDA #INV_4.TEXT_BLOCK.TEST5.1
		; STA STRING+$0
		; LDA /INV_4.TEXT_BLOCK.TEST5.1
		; STA STRING+$1
	; JSR PRINT.STR.BACKGROUND

		; LDA #$01	;#CONSTANT. location to print the label preceeding the variable value. 
		; STA HTAB	
		; INC VTAB
	; JSR	UPDATE.CHAR.POS
	
		; LDA #INV_4.TEXT_BLOCK.TEST5.2
		; STA STRING+$0
		; LDA /INV_4.TEXT_BLOCK.TEST5.2
		; STA STRING+$1
	; JSR PRINT.STR.BACKGROUND	
	
; ;ITEM 6.1
		; LDA #$01	;#CONSTANT. location to print the label preceeding the variable value. 
		; STA HTAB	
		; INC VTAB
		; INC VTAB
	; JSR	UPDATE.CHAR.POS

		; LDA #INV_4.TEXT_BLOCK.TEST6.1
		; STA STRING+$0
		; LDA /INV_4.TEXT_BLOCK.TEST6.1
		; STA STRING+$1
	; JSR PRINT.STR.BACKGROUND

		; LDA #$01	;#CONSTANT. location to print the label preceeding the variable value. 
		; STA HTAB	
		; INC VTAB
	; JSR	UPDATE.CHAR.POS
	
		; LDA #INV_4.TEXT_BLOCK.TEST6.2
		; STA STRING+$0
		; LDA /INV_4.TEXT_BLOCK.TEST6.2
		; STA STRING+$1
	; JSR PRINT.STR.BACKGROUND
; @END

	; ;JSR KEYIN
	; JSR FLIP.PAGE
	; JSR KEYIN
	
	; JSR INV_4.SYNC_PAGES
	; ;JSR INV_4.DISPLAY_INVENTORY.SCROLL_UP
	; JSR INV_4.DISPLAY_INVENTORY.SCROLL_DOWN
	; JSR FLIP.PAGE
	; JSR KEYIN


	; lda #$aa
	; jsr prep.brk
	; brk

	
	
; .EXIT
	
	; RTS	
	
	
; ;INV_4.TEXT_BLOCK.TEST_HEADER1	.AZ -/Readied WT 056/,#$AF,/087/	
; INV_4.TEXT_BLOCK.TEST_HEADER2	.AZ -/ITEM       WT  QTY/
	
; INV_4.TEXT_BLOCK.TEST1.1		.AZ -/**         003 001/	
; INV_4.TEXT_BLOCK.TEST1.2		.AZ -/Dagger/	

; INV_4.TEXT_BLOCK.TEST2.1		.AZ -/           005 015/	
; INV_4.TEXT_BLOCK.TEST2.2		.AZ -/Long Sword/	

; INV_4.TEXT_BLOCK.TEST3.1		.AZ -/           001 020/	
; INV_4.TEXT_BLOCK.TEST3.2		.AZ -/Blunderbuss/	

; INV_4.TEXT_BLOCK.TEST4.1		.AZ -/           001 150/	
; INV_4.TEXT_BLOCK.TEST4.2		.AZ -/Cow-A-Pult/	

; INV_4.TEXT_BLOCK.TEST5.1		.AZ -/           199 016/	
; INV_4.TEXT_BLOCK.TEST5.2		.AZ -/Business Axe/	

; INV_4.TEXT_BLOCK.TEST6.1		.AZ -/           010 013/	
; INV_4.TEXT_BLOCK.TEST6.2		.AZ -/Death Sword/	

; INV_4.TEXT_BLOCK.TEST7.1		.AZ -/           015 009/	
; INV_4.TEXT_BLOCK.TEST7.2		.AZ -/New Item/	



; ; INV_4.TEXT_BLOCK.TEST_HEADER1	.AZ -/Readied WT 056/,#$AF,/087/	
; ; INV_4.TEXT_BLOCK.TEST_HEADER2	.AZ -/ITEM       QTY--WT/

	
; ; INV_4.TEXT_BLOCK.TEST1.1		.AZ -/**         003/,#$AF,/001/	
; ; INV_4.TEXT_BLOCK.TEST1.2		.AZ -/Dagger/	

; ; INV_4.TEXT_BLOCK.TEST2.1		.AZ -/           005/,#$AF,/015/	
; ; INV_4.TEXT_BLOCK.TEST2.2		.AZ -/Long Sword/	

; ; INV_4.TEXT_BLOCK.TEST3.1		.AZ -/           001/,#$AF,/020/	
; ; INV_4.TEXT_BLOCK.TEST3.2		.AZ -/Blunderbuss/	

; ; INV_4.TEXT_BLOCK.TEST4.1		.AZ -/           001/,#$AF,/150/	
; ; INV_4.TEXT_BLOCK.TEST4.2		.AZ -/Cow-A-Pult/	

; ; INV_4.TEXT_BLOCK.TEST5.1		.AZ -/           199/,#$AF,/016/	
; ; INV_4.TEXT_BLOCK.TEST5.2		.AZ -/Business Axe/	

; ; INV_4.TEXT_BLOCK.TEST6.1		.AZ -/           010/,#$AF,/013/	
; ; INV_4.TEXT_BLOCK.TEST6.2		.AZ -/Death Sword/	

; ; INV_4.TEXT_BLOCK.TEST7.1		.AZ -/           015/,#$AF,/009/	
; ; INV_4.TEXT_BLOCK.TEST7.2		.AZ -/New Item/	


; ; INV_4.TEXT_BLOCK.TEST1.1		.AZ -/         **003-001/	
; ; INV_4.TEXT_BLOCK.TEST1.2		.AZ -/Dagger/	

; ; INV_4.TEXT_BLOCK.TEST2.1		.AZ -/           005-015/	
; ; INV_4.TEXT_BLOCK.TEST2.2		.AZ -/Long Sword/	

; ; INV_4.TEXT_BLOCK.TEST3.1		.AZ -/           001-020/	
; ; INV_4.TEXT_BLOCK.TEST3.2		.AZ -/Blunderbuss/	

; ; INV_4.TEXT_BLOCK.TEST4.1		.AZ -/           001-150/	
; ; INV_4.TEXT_BLOCK.TEST4.2		.AZ -/Cow-A-Pult/	

; ; INV_4.TEXT_BLOCK.TEST5.1		.AZ -/           199-016/	
; ; INV_4.TEXT_BLOCK.TEST5.2		.AZ -/Business Axe/	

; ; INV_4.TEXT_BLOCK.TEST6.1		.AZ -/           010-013/	
; ; INV_4.TEXT_BLOCK.TEST6.2		.AZ -/Death Sword/	

; ; INV_4.TEXT_BLOCK.TEST7.1		.AZ -/           015-009/	
; ; INV_4.TEXT_BLOCK.TEST7.2		.AZ -/New Item/	


; ; INV_4.TEXT_BLOCK.TEST1.1		.AZ -/003**          001/	
; ; INV_4.TEXT_BLOCK.TEST1.2		.AZ -/Dagger/	

; ; INV_4.TEXT_BLOCK.TEST2.1		.AZ -/005            015/	
; ; INV_4.TEXT_BLOCK.TEST2.2		.AZ -/Long Sword/	

; ; INV_4.TEXT_BLOCK.TEST3.1		.AZ -/001            020/	
; ; INV_4.TEXT_BLOCK.TEST3.2		.AZ -/Blunderbuss/	

; ; INV_4.TEXT_BLOCK.TEST4.1		.AZ -/001            150/	
; ; INV_4.TEXT_BLOCK.TEST4.2		.AZ -/Cow-A-Pult/	

; ; INV_4.TEXT_BLOCK.TEST5.1		.AZ -/199            016/	
; ; INV_4.TEXT_BLOCK.TEST5.2		.AZ -/Business Axe/	

; ; INV_4.TEXT_BLOCK.TEST6.1		.AZ -/010            013/	
; ; INV_4.TEXT_BLOCK.TEST6.2		.AZ -/Death Sword/	

; ; INV_4.TEXT_BLOCK.TEST7.1		.AZ -/015            009/	
; ; INV_4.TEXT_BLOCK.TEST7.2		.AZ -/New Item/	


; ; INV_4.TEXT_BLOCK.TEST1.1		.AZ -/003(003)**   001/	
; ; INV_4.TEXT_BLOCK.TEST1.2		.AZ -/Dagger/	

; ; INV_4.TEXT_BLOCK.TEST2.1		.AZ -/005(004)     015/	
; ; INV_4.TEXT_BLOCK.TEST2.2		.AZ -/Long Sword/	

; ; INV_4.TEXT_BLOCK.TEST3.1		.AZ -/001(000)     020/	
; ; INV_4.TEXT_BLOCK.TEST3.2		.AZ -/Blunderbuss/	

; ; INV_4.TEXT_BLOCK.TEST4.1		.AZ -/001(001)     150/	
; ; INV_4.TEXT_BLOCK.TEST4.2		.AZ -/Cow-A-Pult/	

; ; INV_4.TEXT_BLOCK.TEST5.1		.AZ -/199(199)     016/	
; ; INV_4.TEXT_BLOCK.TEST5.2		.AZ -/Business Axe/	

; ; INV_4.TEXT_BLOCK.TEST6.1		.AZ -/010(009)     013/	
; ; INV_4.TEXT_BLOCK.TEST6.2		.AZ -/Death Sword/	

; ; INV_4.TEXT_BLOCK.TEST7.1		.AZ -/015(013)     009/	
; ; INV_4.TEXT_BLOCK.TEST7.2		.AZ -/New Item/	


; ;====original test text===========	
; ; INV_4.TEXT_BLOCK.TEST1.1		.AZ -/123456789/	
; ; INV_4.TEXT_BLOCK.TEST1.2		.AZ -/Dagger/	

; ; INV_4.TEXT_BLOCK.TEST2.1		.AZ -/948572018/	
; ; INV_4.TEXT_BLOCK.TEST2.2		.AZ -/Long Sword/	

; ; INV_4.TEXT_BLOCK.TEST3.1		.AZ -/858935627/	
; ; INV_4.TEXT_BLOCK.TEST3.2		.AZ -/Blunderbuss/	

; ; INV_4.TEXT_BLOCK.TEST4.1		.AZ -/676234090/	
; ; INV_4.TEXT_BLOCK.TEST4.2		.AZ -/Cow-A-Pult/	

; ; INV_4.TEXT_BLOCK.TEST5.1		.AZ -/893558102/	
; ; INV_4.TEXT_BLOCK.TEST5.2		.AZ -/Business Axe/	

; ; INV_4.TEXT_BLOCK.TEST6.1		.AZ -/435345235/	
; ; INV_4.TEXT_BLOCK.TEST6.2		.AZ -/Death Sword/	

; ; INV_4.TEXT_BLOCK.TEST7.1		.AZ -/123521934/	
; ; INV_4.TEXT_BLOCK.TEST7.2		.AZ -/New Item/	

	
@END


;INV_4.WAIT.LOOP
@START
; ;PARAMETERS: ACC (LOOP LENGTH IN SUPER-PAGES)

; .SET.PARAMETERS
	
	; STA TEMP
	; BEQ .EXIT.ALTERNATE.ENTRANCE ;don't restore registers since they were not saved
	
; .SAVE.REGISTERS
	; TXA
	; PHA
	; TYA
	; PHA
	
	
	; LDX #$00 ;init delay counter, lo byte
	; LDY #$00 ;init delay counter, ho byte
; .WAIT.LOOP1	
; .KB_BUFF.CHECK2.DONE
	; ; ;abort keypress check
	; ; LDA $C000
    ; ; BMI .EXIT ;if key has been pressed since last check, then exit loop

	; INX ;increment lo byte delay counter
	; BNE .WAIT.LOOP1
	; INY ;increment ho byte delay counter
	; CPY TEMP ;arbitrary value tuned to the visually observed animation speed
	; BNE .WAIT.LOOP1	
	; JMP .RESTORE.REGISTERS
	
; .EXIT

	
; .RESTORE.REGISTERS
	; PLA
	; TAY
	; PLA
	; TAX

; .EXIT.ALTERNATE.ENTRANCE	
	; LDA TEMP ;reset ACC to parameter value so loop can be called multiple times in a row without resting the parameter
	; RTS
@END


;MENU MANAGEMENT
INV_4.MOVE_SELECTOR_UP
@START
.ERASE.ITEM_SELECTOR
		;LDA #$7F ;set mode ($00 = item selector OFF | $7F = item selector ON) 
		LDA #$00 ;set mode ($00 = item selector OFF | $7F = item selector ON) 
		;INV_4.SCREEN_ITEM_LIST.SELECTED.ITEM: use existing value
		SEC ;(CLC = don't sync hi-res pages | SEC = sync hi-res pages)
	JSR INV_4.PRINT.SELECTED.ITEM
		
					
					; LDA #$ab
					; LDx INV_4.SCREEN_ITEM_LIST.SELECTED.ITEM ;is the item selector set to the last item in the list?
					; ldy #INV_4.MAX_ONSCREEN.ITEMS
					; jsr prep.brk
					; brk
					
.SCROLL.DOWN ;scrolling down allows 1st item on screen to be replaced with new item
;VALIDATE ENTRANCE
	LDA INV_4.SCREEN_ITEM_LIST.SELECTED.ITEM ;is the item selector set to the first item in the list?
	;BNE .SCROLL.DOWN.DONE ;if item selector is not set to first item, then selector shouldn't move up into data not on screen. 
	BEQ .COW
	JMP .SCROLL.DOWN.DONE ;if item selector is not set to first item, then selector shouldn't move up into data not on screen. 
.COW

;GET NEXT ITEM

					; lda #$00 
					; sta cow
					
	;set pointer to item0
	LDA INV_4.SCREEN_ITEM_LIST.ARRAY+$0 ;load LO byte of inventory record address for item0
	STA PLAYER.INVENTORY.DATA.POINTER+$0
	LDA INV_4.SCREEN_ITEM_LIST.ARRAY+$1 ;load HO byte of inventory record address for item0
	STA PLAYER.INVENTORY.DATA.POINTER+$1
					
.LOOP.GET.NEXT_ITEM
		LDA #$01 ;($00 = iterate down | $01 = iterate up)
	JSR INV_4.GET.NEXT_ITEM
		;RETURN VALUE: ACC ($00 = next item found, $01 = top or bottom of array reached), INV_4.NEXT_ITEM.QTY_AVAILABLE, INV_4.NEXT_ITEM.READIED_STATUS, PLAYER.INVENTORY.DATA.POINTER(2)
	
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
					; ldx INV_4.DISPLAY.MODE 
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
	LDA INV_4.NEXT_ITEM.READIED_STATUS ;($00 = not readied | $01 = readied by active player | $02 = readied twice by active player)
	BNE .VALIDATE.ITEM.DONE ;if item is readied by active player then item should be displayed
	LDA INV_4.NEXT_ITEM.QTY_AVAILABLE ;if there are no units of the item available to be readied by the active player then there is no reason to display the item. 
	BEQ .LOOP.GET.NEXT_ITEM ;get next item. INV_4.GET.NEXT_ITEM will pick up at the position it left off at in the inventory table.  
.VALIDATE.ITEM.DONE

			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
; ;=====DEBUG HOOK======						
		; LDX #$00
; .DEBUG.COPY1
		; LDA INV_4.SCREEN_ITEM_LIST.ARRAY,X
		; STA $BD00,X
		; INX
		; BNE .DEBUG.COPY1
		
		
		; LDX #$00
; .DEBUG.COPY2
		; LDA INV_4.SCREEN_ITEM_LIST.DATA,X
		; STA $BE00,X
		; INX
		; BNE .DEBUG.COPY2

			; LDA #$BB
			; LDX INV_4.SCREEN_ITEM_LIST.SELECTED.ITEM
			; LDY TEMP
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP


			
	;**FALLS THROUGH** 	;Since the party owns more of this item than is readied by the PCs, then the item should be displayed
	
	JSR INV_4.SCREEN_LIST.SCROLL.DOWN

.SAVE.ITEM
	
		;INV_4.NEXT_ITEM.QTY_AVAILABLE: use existing value
		;INV_4.NEXT_ITEM.READIED_STATUS: use existing value
		;PLAYER.INVENTORY.DATA.POINTER: use existing value
		LDX #$00 ;set INV_4.SCREEN_ITEM_LIST.ARRAY index to the start of item 0 (of item0-item5)
	JSR INV_4.SAVE_ITEM ;saves item if valid
 			
;SAVE MASTER ITEM RECORD
@START

	;read item record from master item file
		;(get parameters from the inventory record of current item)
		LDY #$01 ;set index to byte $1 (item_ID) of inventory record
		LDA (PLAYER.INVENTORY.DATA.POINTER),Y
		STA FILE.ITEM_TABLE.ID
		DEY ;backup to byte $0 (LEVEL | ITEM_TYPE) of inventory record
		LDA (PLAYER.INVENTORY.DATA.POINTER),Y
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
	LDA INV_4.SCREEN_ITEM_LIST.SELECTED.ITEM  ;load selected item number
	ASL ;X !2
	ASL ;X !4
	ASL ;X !8
	ASL ;X !16
	ASL ;X !32
	TAX ;set INV_4.SCREEN_ITEM_LIST.DATA index ($20 byte records)
		
					
	LDY #$00 ;init FILE.ITEM_TABLE.RECORD.READ index
.LOOP.COPY.DATA
	LDA FILE.ITEM_TABLE.RECORD.READ,Y
	
				;STA TEMP
				
	STA INV_4.SCREEN_ITEM_LIST.DATA,X
				
					
	INX	;increment INV_4.SCREEN_ITEM_LIST.DATA index
	INY ;increment FILE.ITEM_TABLE.RECORD.READ index
	CPY #FILE.ITEM_TABLE.RECORD.SIZE
	BNE .LOOP.COPY.DATA
@END



	
	JSR INV_4.DISPLAY_INVENTORY.SCROLL_DOWN
	
	
	JMP .DRAW.ITEM_SELECTOR.ALT_ENTRANCE ;draw item selector (without item selection increment) because selector was aready erased above.

	
	;JSR INV_4.GET.NEXT_ITEM_DOWN
;		***parse return value**...IF top/bottom of array code returned in ACC, THEN JMP INV_4.SUB_MODULE.STATE.LOOP
;	;save new item record to INV_4.SCREEN_ITEM_LIST.ARRAY
;			scroll INV_4.SCREEN_ITEM_LIST.ARRAY up
;			save new item record to item position 5 in the array
;
	;INV_4.DISPLAY_INVENTORY.SCROLL_UP
;	DEC INV_4.SCREEN_ITEM_LIST.SELECTED.ITEM ;offset the DEC in draw item selector since the selected item doesn't change

	
;****INSERT CODE FOR GET NEXT ITEM, WHEN MOVING DOWN IS OFFSCREEN

.SCROLL.DOWN.DONE
	
.DRAW.ITEM_SELECTOR ;(also prints new item if a scroll occured)
	
	DEC INV_4.SCREEN_ITEM_LIST.SELECTED.ITEM ;advance selector to next item down on the video screen

.DRAW.ITEM_SELECTOR.ALT_ENTRANCE

			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP2
; ;=====DEBUG HOOK======						
		; LDX #$00
; .DEBUG.COPY1.1
		; LDA INV_4.SCREEN_ITEM_LIST.ARRAY,X
		; STA $BD00,X
		; INX
		; BNE .DEBUG.COPY1.1
		
		
		; LDX #$00
; .DEBUG.COPY2.1
		; LDA INV_4.SCREEN_ITEM_LIST.DATA,X
		; STA $BE00,X
		; INX
		; BNE .DEBUG.COPY2.1

			; LDA #$BC
			; LDX INV_4.SCREEN_ITEM_LIST.SELECTED.ITEM
			; JSR PREP.BRK
			; BRK
; .TEMP2
			; LDA TEMP
			
		LDA #$7F ;set mode ($00 = item selector OFF | $7F = item selector ON) 
		;LDA #$00 ;set mode ($00 = item selector OFF | $7F = item selector ON) 
		CLC ;(CLC = don't sync hi-res pages | SEC = sync hi-res pages)
	JSR INV_4.PRINT.SELECTED.ITEM

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
	CMP INV_4.SCREEN_LIST.TOTAL_RECORDS ;are there less than 6 items on the screen? (item0-item5)
	BEQ .DETECT.END_OF_SCREEN_LIST.DONE
	LDA INV_4.SCREEN_ITEM_LIST.SELECTED.ITEM ;is the item selector set to the last item in the list?
	CMP INV_4.SCREEN_LIST.TOTAL_RECORDS ;(0-5 = item # of last item on screen | $FF = no inventory records are eligible for display)
	;BEQ .EXIT.ALT ;exit without page flipping
	BNE .COW
	JMP .EXIT.ALT ;exit without page flipping
.COW

.DETECT.END_OF_SCREEN_LIST.DONE	
	

.ERASE.ITEM_SELECTOR
		;LDA #$7F ;set mode ($00 = item selector OFF | $7F = item selector ON) 
		LDA #$00 ;set mode ($00 = item selector OFF | $7F = item selector ON) 
		;INV_4.SCREEN_ITEM_LIST.SELECTED.ITEM: use existing value
		SEC ;(CLC = don't sync hi-res pages | SEC = sync hi-res pages)
	JSR INV_4.PRINT.SELECTED.ITEM


					
					
					; LDA #$ab
					; LDx INV_4.SCREEN_ITEM_LIST.SELECTED.ITEM ;is the item selector set to the last item in the list?
					; ldy #INV_4.MAX_ONSCREEN.ITEMS
					; jsr prep.brk
					; brk
					
.SCROLL.UP ;scrolling down allows last item on screen to be replaced with new item
;VALIDATE ENTRANCE
	LDA INV_4.SCREEN_ITEM_LIST.SELECTED.ITEM ;is the item selector set to the last item in the list?
	CMP #INV_4.MAX_ONSCREEN.ITEMS-1 ;-1 because the constant is a quantity (starting with 1) and the array in the LDA starts with 0. 
	BCC .SCROLL.UP.DONE


	;set pointer to item5
	LDA INV_4.SCREEN_ITEM_LIST.ARRAY+$14 ;load LO byte of inventory record address for item0
	STA PLAYER.INVENTORY.DATA.POINTER+$0
	LDA INV_4.SCREEN_ITEM_LIST.ARRAY+$15 ;load HO byte of inventory record address for item0
	STA PLAYER.INVENTORY.DATA.POINTER+$1
		
;GET NEXT ITEM
.LOOP.GET.NEXT_ITEM

		LDA #$00 ;($00 = iterate down | $01 = iterate up)
	JSR INV_4.GET.NEXT_ITEM
		;RETURN VALUE: ACC ($00 = next item found, $01 = top or bottom of array reached), INV_4.NEXT_ITEM.QTY_AVAILABLE, INV_4.NEXT_ITEM.READIED_STATUS, PLAYER.INVENTORY.DATA.POINTER(2)
	
	
	;exit test
	CMP #$01 ;was top/bottom of array reached? (i.e. there is no next item)
	BEQ .DRAW.ITEM_SELECTOR.ALT_ENTRANCE ;branch if yes. draw item selector (without item selection increment) because selector was aready erased above. 
	;BEQ .TEST ;branch if yes. draw item selector (without item selection increment) because selector was aready erased above. 

.VALIDATE.ITEM
	;should item be displayed onscreen?
	LDA INV_4.NEXT_ITEM.READIED_STATUS ;($00 = not readied | $01 = readied by active player | $02 = readied twice by active player)
	BNE .VALIDATE.ITEM.DONE ;if item is readied by active player then item should be displayed
	LDA INV_4.NEXT_ITEM.QTY_AVAILABLE ;if there are no units of the item available to be readied by the active player then there is no reason to display the item. 
	BEQ .LOOP.GET.NEXT_ITEM ;get next item. INV_4.GET.NEXT_ITEM will pick up at the position it left off at in the inventory table.  
.VALIDATE.ITEM.DONE
	
	;**FALLS THROUGH** 	;Since the party owns more of this item than is readied by the PCs, then the item should be displayed
	
	JSR INV_4.SCREEN_LIST.SCROLL.UP


			
.SAVE.ITEM
	
		;INV_4.NEXT_ITEM.QTY_AVAILABLE: use existing value
		;INV_4.NEXT_ITEM.READIED_STATUS: use existing value
		;PLAYER.INVENTORY.DATA.POINTER: use existing value
		LDX #$14 ;set INV_4.SCREEN_ITEM_LIST.ARRAY index to the start of item 5 (of item0-item5)
	JSR INV_4.SAVE_ITEM ;saves item if valid
 


			
;SAVE MASTER ITEM RECORD
@START

	;read item record from master item file
		;(get parameters from the inventory record of current item)
		LDY #$01 ;set index to byte $1 (item_ID) of inventory record
		LDA (PLAYER.INVENTORY.DATA.POINTER),Y
		STA FILE.ITEM_TABLE.ID
		DEY ;backup to byte $0 (LEVEL | ITEM_TYPE) of inventory record
		LDA (PLAYER.INVENTORY.DATA.POINTER),Y
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
	LDA INV_4.SCREEN_ITEM_LIST.SELECTED.ITEM  ;load selected item number
	ASL ;X !2
	ASL ;X !4
	ASL ;X !8
	ASL ;X !16
	ASL ;X !32
	TAX ;set INV_4.SCREEN_ITEM_LIST.DATA index ($20 byte records)
		
					
	LDY #$00 ;init FILE.ITEM_TABLE.RECORD.READ index
.LOOP.COPY.DATA
	LDA FILE.ITEM_TABLE.RECORD.READ,Y
	
				;STA TEMP
				
	STA INV_4.SCREEN_ITEM_LIST.DATA,X
				
					
	INX	;increment INV_4.SCREEN_ITEM_LIST.DATA index
	INY ;increment FILE.ITEM_TABLE.RECORD.READ index
	CPY #FILE.ITEM_TABLE.RECORD.SIZE
	BNE .LOOP.COPY.DATA
@END



			
	
	JSR INV_4.DISPLAY_INVENTORY.SCROLL_UP
	JMP .DRAW.ITEM_SELECTOR.ALT_ENTRANCE ;draw item selector (without item selection increment) because selector was aready erased above.

	
	;JSR INV_4.GET.NEXT_ITEM_DOWN
;		***parse return value**...IF top/bottom of array code returned in ACC, THEN JMP INV_4.SUB_MODULE.STATE.LOOP
;	;save new item record to INV_4.SCREEN_ITEM_LIST.ARRAY
;			scroll INV_4.SCREEN_ITEM_LIST.ARRAY up
;			save new item record to item position 5 in the array
;
	;INV_4.DISPLAY_INVENTORY.SCROLL_UP
;	DEC INV_4.SCREEN_ITEM_LIST.SELECTED.ITEM ;offset the DEC in draw item selector since the selected item doesn't change

	
;****INSERT CODE FOR GET NEXT ITEM, WHEN MOVING DOWN IS OFFSCREEN

.SCROLL.UP.DONE
	
.DRAW.ITEM_SELECTOR ;(also prints new item if a scroll occured)
	
	INC INV_4.SCREEN_ITEM_LIST.SELECTED.ITEM ;advance selector to next item down on the video screen

.DRAW.ITEM_SELECTOR.ALT_ENTRANCE		
		LDA #$7F ;set mode ($00 = item selector OFF | $7F = item selector ON) 
		;LDA #$00 ;set mode ($00 = item selector OFF | $7F = item selector ON) 
		CLC ;(CLC = don't sync hi-res pages | SEC = sync hi-res pages)
	JSR INV_4.PRINT.SELECTED.ITEM

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
	

;DATA		
INV_4.GET.NEXT_ITEM
@START
;PARAMETERS: ACC ($00 = iterate down | $01 = iterate up), INV_4.DISPLAY.MODE, PLAYER.INVENTORY.DATA.POINTER(2)
;ENTRANCE: .INIT.SCREEN_ITEM_LIST, .MOVE_SELECTOR_DOWN, .MOVE_SELECTOR_UP
;RETURN: ACC ($00 = next item found, $01 = top or bottom of array reached), INV_4.NEXT_ITEM.QTY_AVAILABLE, INV_4.NEXT_ITEM.READIED_STATUS, PLAYER.INVENTORY.DATA.POINTER(2) (pointer to the record found)

;=====================SUBROUTINE DOCUMENTATION===========================================================================
;
;iterates INV.PLAYER.INVENTORY.DATA from the position of PLAYER.INVENTORY.DATA.POINTER at the end of the last call.
;the loop runs until an item matching INV_4.DISPLAY.MODE (weapon, armor, misc item) is found or until the top or bottom of the array
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
	STA INV_4.GET.NEXT_ITEM.PARM.MODE

;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA


.MODE_BRANCH	
	LDA INV_4.GET.NEXT_ITEM.PARM.MODE ;($00 = iterate down | $01 = iterate up)
	BNE .NEXT_RECORD.ITERATE_UP
	
	;**FALLS THROUGH**

.LOOP.SEARCH.INVENTORY.DATA
	
.NEXT_RECORD.ITERATE_DOWN
	LDA PLAYER.INVENTORY.DATA.POINTER+$0
	CLC
	ADC #INV.PLAYER.INVENTORY.DATA.RECORD_SIZE
	STA PLAYER.INVENTORY.DATA.POINTER+$0
	LDA PLAYER.INVENTORY.DATA.POINTER+$1
	ADC /INV.PLAYER.INVENTORY.DATA.RECORD_SIZE ;16-bit add
	STA PLAYER.INVENTORY.DATA.POINTER+$1
.EXIT.TEST.DOWN
	CMP /INV.PLAYER.INVENTORY.DATA.PATCH_END
	BCC .TEST.RECORD_MATCH
	BEQ .TEST.RECORD_MATCH
 
	JMP .START_OR_END.OF_ARRAY_REACHED

.NEXT_RECORD.ITERATE_UP
	LDA PLAYER.INVENTORY.DATA.POINTER+$0
	SEC
	SBC #INV.PLAYER.INVENTORY.DATA.RECORD_SIZE
	STA PLAYER.INVENTORY.DATA.POINTER+$0
	LDA PLAYER.INVENTORY.DATA.POINTER+$1
	SBC /INV.PLAYER.INVENTORY.DATA.RECORD_SIZE ;16-bit add
	STA PLAYER.INVENTORY.DATA.POINTER+$1
.EXIT.TEST.UP
	CMP /INV.PLAYER.INVENTORY.DATA.PATCH_START
	; BCC .TEST.RECORD_MATCH
	; BEQ .TEST.RECORD_MATCH	
	BCS .TEST.RECORD_MATCH

	
	JMP .START_OR_END.OF_ARRAY_REACHED

	
	
.TEST.RECORD_MATCH
	LDY #$00 ;reset Y-REG on each iteration because array position is managed exclusively through PLAYER.INVENTORY.DATA.POINTER 
	
	;Y-REG ;set to record byte $0 (item_level | item_type)
	LDA (PLAYER.INVENTORY.DATA.POINTER),Y
	CMP #$FF ;check for open record value
	BEQ .MODE_BRANCH ;branch if record is open
	;Item_Type is in LO nibble.
	AND #$F ;mask-out HO nibble. 
;FILTER1: Item_Type
	CMP INV_4.DISPLAY.MODE
	BNE .MODE_BRANCH
.FILTER2 ;this is an item_type specific filter
	CMP #INV.ITEM_TYPE.MISC ;this test only applies to item_type $00 (weapons) and $01 (armor)
	BCC .FILTER2C	 ;branch if item_type = $00 (weapons) and $01 (armor)
	INY ;advance to record byte $1 (item_ID)
	LDA (PLAYER.INVENTORY.DATA.POINTER),Y
	PHA ;save item_ID
	LDA INV.ACTIVE_MENU.CODE 
	CMP #INV.ACTIVE_MENU.SPELLS ;INV_4.DISPLAY.MODE = $02 (misc items), but is spell menu active?
	BEQ .FILTER2B ;branch, if yes
	;**FALLS THROUGH**	if, no. ;This means that the misc items menu is active.
.FILTER2A ;Misc Items menu active: filter out spells
	PLA ;restore item_ID
	CMP #INV_4.SPELL_RANGE.LTH
	BCC .MODE_BRANCH
	JMP .FILTERS.DONE
.FILTER2B ;Spells menu active: filter out non-spells
	PLA ;restore item_ID
	CMP #INV_4.SPELL_RANGE.LTH
	BCS .MODE_BRANCH
	JMP .FILTERS.DONE
.FILTER2C ;Weapons/Armor menu active: filter out fists and skin	
	INY ;advance to record byte $1 (item_ID)
	LDA (PLAYER.INVENTORY.DATA.POINTER),Y
	;CMP #INV.ITEM_TYPE.WEAPON.FISTS ;fist or skin, they have the same item_ID
	BEQ .MODE_BRANCH ;branch if item_ID is fists or skin (can't tell which without an item_type test but it doesn't matter)
.FILTERS.DONE	
	;**FALLS THROUGH**
	
.CALCULATE.READIED_QTY.FIELDS
	JSR INV_4.CALCULATE.READIED_QTY.FIELDS
	
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
;PARAMETERS: INV_4.ACTIVE_PLAYER, PLAYER.INVENTORY.DATA.POINTER(2)
;ENTRANCE: direct
;RETURN: INV_4.NEXT_ITEM.READIED_STATUS, INV_4.NEXT_ITEM.QTY_AVAILABLE


;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA
	
.INIT
	LDA #$00
	STA	INV_4.NEXT_ITEM.READIED_STATUS ;($00 = not readied | $01 = readied by active player | $02 = readied twice by active player)


.INV_4.GET.READIED.STATUS 
@START
;Note: Check both readied flag fields, if active playered has item readied twice then set to $02, if once set to $01 	
	LDY #$04 ;set index to record byte $4 (Readied Equipment Flags #2)
	LDX INV_4.ACTIVE_PLAYER ;load index to INV.READIED_FLAGS.BIT_TEST_MASK	
	LDA (PLAYER.INVENTORY.DATA.POINTER),Y ;load Readied Equipment Flags #2			
	AND INV.READIED_FLAGS.BIT_TEST_MASK,X ;test bit flag for active player	
	BEQ .READIED.FLAGS1.TEST ;branch if bit not set (active player doesn't have item readied via Readied Equipment Flags field #2)	
	INC INV_4.NEXT_ITEM.READIED_STATUS ;($00 = not readied | $01 = readied by active player | $02 = readied twice by active player)
	
.READIED.FLAGS1.TEST
	DEY ;advance to record byte $3 (Readied Equipment Flags #1)
	;X-REG: INV_4.ACTIVE_PLAYER (index to INV.READIED_FLAGS.BIT_TEST_MASK)
	LDA (PLAYER.INVENTORY.DATA.POINTER),Y ;load Readied Equipment Flags #1
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
			; LDA PLAYER.INVENTORY.DATA.POINTER+$0
			; CMP #$7E
			; BNE .TEMP
			; LDA PLAYER.INVENTORY.DATA.POINTER+$1
			; CMP #$BA			
			; BNE .TEMP
			
			; ;save calculations
			; LDY #$04 ;set index to record byte $4 (Readied Equipment Flags #2)
			; LDX INV_4.ACTIVE_PLAYER ;load index to INV.READIED_FLAGS.BIT_TEST_MASK	
			; STX $BE00
			; LDA (PLAYER.INVENTORY.DATA.POINTER),Y ;load Readied Equipment Flags #2		
			; STA $BE01
			; LDA INV.READIED_FLAGS.BIT_TEST_MASK,X ;test bit flag for active player	
			; STA $BE02
			
			; DEY ;advance to record byte $3 (Readied Equipment Flags #1)
			; ;X-REG: INV_4.ACTIVE_PLAYER (index to INV.READIED_FLAGS.BIT_TEST_MASK)
			; LDA (PLAYER.INVENTORY.DATA.POINTER),Y ;load Readied Equipment Flags #1
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
			; LDA PLAYER.INVENTORY.DATA.POINTER+$0
			; CMP #$30
			; BNE .TEMP
			; LDA PLAYER.INVENTORY.DATA.POINTER+$1
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
	LDA (PLAYER.INVENTORY.DATA.POINTER),Y ;load QTY field

					; tax
					; lda #$ab
					; jsr prep.brk
					; ldy INV.RE.READIED.QTY
					; brk
	SEC
	SBC INV.RE.READIED.QTY
	BCC .ERROR.QTY.OVERFLOW
	STA INV_4.NEXT_ITEM.QTY_AVAILABLE

	
	
; ;DEBUG HOOK	
			; sta temp
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP	
			; lda PLAYER.INVENTORY.DATA.POINTER+$0
			; sta $be00
			; lda PLAYER.INVENTORY.DATA.POINTER+$1
			; sta $be01			
			; LDA (PLAYER.INVENTORY.DATA.POINTER),Y ;load QTY field
			; sta $be02
			; lda INV.RE.READIED.QTY
			; sta $be03
			; STA TEMP
			;
			;
			; lda #$aa
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

INV_4.SAVE_ITEM 
@START
;PARAMETERS: X-REG (INV_4.SCREEN_ITEM_LIST.ARRAY index), INV_4.NEXT_ITEM.QTY_AVAILABLE, INV_4.NEXT_ITEM.READIED_STATUS, PLAYER.INVENTORY.DATA.POINTER(2)
;ENTRANCE: .INIT.IDENTIFY_ITEMS
;RETURN: updated INV_4.SCREEN_ITEM_LIST.ARRAY


.SAVE.ITEM
;SAVE ITEM DATA TO INV_4.SCREEN_ITEM_LIST.ARRAY 
;INV_4.SCREEN_ITEM_LIST.ARRAY (inventory table record # LO address, inventory table record # HO address, QTY_AVAILABLE, readied status)
	LDA PLAYER.INVENTORY.DATA.POINTER+$0
	STA INV_4.SCREEN_ITEM_LIST.ARRAY+$0,X

	LDA PLAYER.INVENTORY.DATA.POINTER+$1
	STA INV_4.SCREEN_ITEM_LIST.ARRAY+$1,X
	
	LDA INV_4.NEXT_ITEM.QTY_AVAILABLE
	STA INV_4.SCREEN_ITEM_LIST.ARRAY+$2,X
	
	LDA INV_4.NEXT_ITEM.READIED_STATUS
	STA INV_4.SCREEN_ITEM_LIST.ARRAY+$3,X

.EXIT

	RTS
@END

INV_4.READY_UNREADY.REQUEST
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


.INIT	
	;calculate index to INV_4.SCREEN_ITEM_LIST.ARRAY for selected item
	LDA INV_4.SCREEN_ITEM_LIST.SELECTED.ITEM ;load item number (0-5) currently selected
	ASL ;X2
	ASL ;X4 (4 byte record)
	TAX
				
	LDA INV_4.SCREEN_ITEM_LIST.ARRAY+$0,X ;load LO byte of inventory record of selected item
	STA PLAYER.INVENTORY.DATA.POINTER+$0	
	LDA INV_4.SCREEN_ITEM_LIST.ARRAY+$1,X ;load HO byte of inventory record of selected item
	STA PLAYER.INVENTORY.DATA.POINTER+$1
					
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
	; LDA INV_4.SCREEN_ITEM_LIST.ARRAY+$0,X ;load level/item_type field of selected item
	; ;item_type is in LO nibble
	; AND #$0F ;mask out HO nibble
	
	LDY #$00 ;set index to byte $0 (level | item_type) of inventory record of selected item
	LDA (PLAYER.INVENTORY.DATA.POINTER),Y ;load (level | item_type) field of selected item
	;item_type is in LO nibble
	AND #$0F ;mask out HO nibble						
	CMP #INV.ITEM_TYPE.ARMOR ;#CONSTANT.	
	BNE .COMBAT_ARMOR.CHECK.DONE
	
	;player is trying to ready armor in combat
		
		LDA #$06 ;($00 item readied | $01 = item not readied: item is not readiable | $02 don't use| $03 item not readied: STR gate check failed | $04 item not readied: DEX gate check failed | $05 item not readied: INT gate check failed | $06 can't ready armor during combat | $07 item unreadied | $08 prompt spell hotkey to ready)
	JSR INV_4.PRINT.MISC_TEXT ;plays sound for some text blocks

	JMP .EXIT
.COMBAT_ARMOR.CHECK.DONE

.SPELL_MENU.READY_UNREADY				
@START
;VALIDATE ENTRANCE
	LDA INV.ACTIVE_MENU.CODE 
	CMP #INV.ACTIVE_MENU.SPELLS ;INV_4.DISPLAY.MODE = $02 (misc items), but is spell menu active?
	BNE	.SPELL_MENU.READY_UNREADY.DONE

		;**OPT** Memory. Put the code for calculating the index to INV_4.SCREEN_ITEM_LIST.DATA into a subroutine. Several code blocks such as the one below can be replaced with a JSR
		
		;**OPT** Memory. .LOOP.SPELL_MENU.IS.READIED and the wrapper around it can probably me merged .SPELL_MENU.ACTIVE (INV_4.VIDEO_OUT.ITEM_LINE1_2), into a common subroutine
		
		
	;init indexes/counters
	LDA INV_4.SCREEN_ITEM_LIST.SELECTED.ITEM ;load item number (0-5) currently selected
	ASL ;X !2
	ASL ;X !4
	ASL ;X !8
	ASL ;X !16
	ASL ;X !32
	TAX ;set INV_4.SCREEN_ITEM_LIST.DATA index ($20 byte records)
	
	LDY #$00 ;init loop counter & PLAYER.HOTKEYS.SPELLS index
.LOOP.SPELL_MENU.IS.READIED	
	LDA PLAYER.HOTKEYS.SPELLS,Y
	CMP INV_4.SCREEN_ITEM_LIST.DATA+INV.ITEM_TABLE.MI.SPELL_CODE.OFFSET,X ;load spell code for current item
	BEQ .SPELL_MENU.ITEM_READIED
	INY
	CPY #PLAYER.HOTKEYS.SPELLS.ARRAY_SIZE
	BNE .LOOP.SPELL_MENU.IS.READIED
	;if loop terminates without a match, then spell is not readied	
	
	
.SPELL_MENU.ITEM_NOT_READIED
@START

					
;PRINT SPELL HOTKEY PROMPT	
		LDA #$08 ;set parm print spell hotkey prompt
	JSR INV_4.PRINT.MISC_TEXT

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
	LDA INV_4.SCREEN_ITEM_LIST.DATA+INV.ITEM_TABLE.MI.SPELL_CODE.OFFSET,X	;load spell code for current item
	STA INV_4.SPELL_READY.SPELL_CODE
				
				; sta $be00
				; stx $be01
				; lda #INV.ITEM_TABLE.MI.SPELL_CODE.OFFSET
				; sta $be02
				; lda #$aa
				; ldx #INV_4.SCREEN_ITEM_LIST.DATA
				; ldy /INV_4.SCREEN_ITEM_LIST.DATA
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
;	; *if ACTIVE_PLAYER READIED_STATUS (INV_4.SCREEN_ITEM_LIST.ARRAY record) is set ($02). ;$02 means item is readied twice
;			; THEN call UNREADY
;	; *if ACTIVE_PLAYER READIED_STATUS (INV_4.SCREEN_ITEM_LIST.ARRAY record) is set ($01). ;$01 means item is readied once
;			; AND selected item is 2-handed THEN call UNREADY
;	; *if ACTIVE_PLAYER READIED_STATUS (INV_4.SCREEN_ITEM_LIST.ARRAY record) is set ($01). ;$01 means item is readied once
;			; AND QTY_AVAILABLE = $0 THEN call UNREADY
;	; *if ACTIVE_PLAYER READIED_STATUS (INV_4.SCREEN_ITEM_LIST.ARRAY record) is set ($01). ;$01 means item is readied once
;			; AND INV_4.DUAL_WIELDING.FLAG = yes ($01) THEN call UNREADY
;	; *ELSE
;		; READY	
;===========================================================================================================================




;;DEBUG HOOK
				; LDA INV_4.SCREEN_ITEM_LIST.ARRAY+$0,X ;load LO byte of inventory record of selected item
				; STA $be00	
				; LDA INV_4.SCREEN_ITEM_LIST.ARRAY+$1,X ;load HO byte of inventory record of selected item
				; STA $be01
				; LDY #$05 ;set index to byte $05 (equipment slot)
				; LDA (PLAYER.INVENTORY.DATA.POINTER),Y ;load equipment slot field (high-bit set = 2 handed weapon)
				; sta $be02
				; LDA INV_4.SCREEN_ITEM_LIST.ARRAY+$2,X ;load quantity available field of selected item
				; sta $be03
;
				; LDA #$ab
				; jsr prep.brk
				; brk
				


	LDY #$05 ;set index to byte $05 (equipment slot)
	LDA (PLAYER.INVENTORY.DATA.POINTER),Y ;load equipment slot field (high-bit set = 2 handed weapon)
	AND #$7F ;mask-out high-bit
	;CMP #INV.RE.SLOT.UNREADIABLE_ITEM
					; sta $be00
					; lda #$aa
					; jsr prep.brk
					; brk
					
	;BEQ .EXIT_STEP ;branch if item is unreadiable	
	LDA INV_4.SCREEN_ITEM_LIST.ARRAY+$3,X ;load readied_status field of selected item
	CMP #$02 ;is item readied twice
	BEQ .CALL.UNREADY
	CMP #$00
	BEQ .CALL.READY	;item is not readied. if the item isn't readied at all, then the player can ready it because the available qty of any item on the screen is at least 1 unless the active player has it readied
	
	;imputed: readied_status = $01 (readied once)
	LDY #$00 ;set index to byte $01 (level | item type)
	LDA (PLAYER.INVENTORY.DATA.POINTER),Y ;load equipment slot field (high-bit set = 2 handed weapon)
	;item_type is in LO byte
	AND #$0F ;mask-out HO byte
	BNE .CALL.UNREADY ;item_type != weapon. Dual wielding (the next section) is not relevant for armor and misc items. Thus, since the readied status = readied once, the only possible action is to unready the item. 
	
	;parse 1-2 handed status
	LDA (PLAYER.INVENTORY.DATA.POINTER),Y ;load equipment slot field (high-bit set = 2 handed weapon)
	BMI .CALL.UNREADY ;branch if 2-handed 
	LDA INV_4.SCREEN_ITEM_LIST.ARRAY+$2,X ;load quantity available field of selected item
	BEQ .CALL.UNREADY ;if item is readied once and qty available is zero, then treat request as unready
	LDA INV_4.DUAL_WIELDING.FLAG ;($00 = single wielding | $01 = dual wielding)
	BNE .CALL.UNREADY ;if player is dual wielding already, then the only availble action is to unready the item. 

	;**FALLS THROUGH**
	
; .ERROR
; ;.DETERMINE.READY_UNREADY.MODE (INV_4.READY_UNREADY.REQUEST) reports that ready/unready request scenario has occured which
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

.EXIT_STEP ;**OPT** Memory. Seems like this can be removed. It's not called by anything. 
	JMP .EXIT
	
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
			; lda PLAYER.INVENTORY.DATA.POINTER+$0
			; sta $be00
			; lda PLAYER.INVENTORY.DATA.POINTER+$1
			; sta $be01		
			; lda INV_4.SCREEN_ITEM_LIST.SELECTED.ITEM
			; sta $be02
			; LDY #$00 ;set index to byte $00 (level|item_type)
			; LDA (PLAYER.INVENTORY.DATA.POINTER),Y ;load byte $00
			; sta $be03
			; INY ;advance to byte $01 (item_ID)
			; LDA (PLAYER.INVENTORY.DATA.POINTER),Y ;load byte $01
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
		LDA (PLAYER.INVENTORY.DATA.POINTER),Y ;load byte $00
		;item_type is in the LO byte
		AND #$0F ;mask-out HO byte
		STA PARM.RE.ITEM_TYPE
		INY ;advance to byte $01 (item_ID)
		LDA (PLAYER.INVENTORY.DATA.POINTER),Y ;load byte $01
		STA PARM.RE.ITEM_ID	
		;RE.ACTIVE_PLAYER: use existing value
		PLA ;restore mode parm 
		;ACC ($00 = ready equipment mode | $01 = unready equipment mode)


;****INV DEBUG LOG*** 			;**OPT** Memory. Remove.
@START
.DEBUG.LABEL
		PHA
		LDA #.DEBUG.LABEL
		STA INV.DEBUG.ADDRESS+$0
		LDA /.DEBUG.LABEL
		STA INV.DEBUG.ADDRESS+$1
	JSR INV.DEBUG.LOG.WRITE
		PLA
@END

		
	JSR INV.READY_UNREADY.EQUIPMENT.ENTRANCE
		;RETURN VALUE: ACC = (($00 item readied | $01 = item not readied: item is not readiable | $02 don't use| $03 item not readied: STR gate check failed | $04 item not readied: DEX gate check failed | $05 item not readied: INT gate check failed | $06 can't ready armor during combat | $07 item unreadied | $08 prompt spell hotkey to ready)				

						; LDX #$AA
						; JSR PREP.BRK
						; BRK
						
		;ACC: use return value above as the text block to print code
	JSR INV_4.PRINT.MISC_TEXT ;plays sound for some text blocks


	;**FALLS THROUGH**


			; lda #$01
			; sta troubleshooting.hook	
			
			
.UPDATE.SCREEN_LIST

	LDX #$00 ;init INV_4.SCREEN_ITEM_LIST.ARRAY index
	LDY #$00 ;init loop counter (item # 0-5)
.LOOP.UPDATE.SCREEN_LIST ;**OPT** Memory. A subroutine could be setup to init the pointer, a JSR to which could replace this pointer set and the one above 
		
			; cpy #$04
			; BNE .TEMP
			; lda #$01
			; sta troubleshooting.hook

						

; .TEMP
			
		LDA INV_4.SCREEN_ITEM_LIST.ARRAY+$0,X ;load LO byte of inventory record of selected item
		STA PLAYER.INVENTORY.DATA.POINTER+$0	
		LDA INV_4.SCREEN_ITEM_LIST.ARRAY+$1,X ;load HO byte of inventory record of selected item
		STA PLAYER.INVENTORY.DATA.POINTER+$1
		;INV_4.ACTIVE_PLAYER: use existing value	
	JSR INV_4.CALCULATE.READIED_QTY.FIELDS
		;RETURN: INV_4.NEXT_ITEM.READIED_STATUS, INV_4.NEXT_ITEM.QTY_AVAILABLE


				
		;X-REG (INV_4.SCREEN_ITEM_LIST.ARRAY index): use existing value
		;INV_4.NEXT_ITEM.QTY_AVAILABLE: use existing value
		;INV_4.NEXT_ITEM.READIED_STATUS: use existing value
		;PLAYER.INVENTORY.DATA.POINTER(2): use existing value
	JSR INV_4.SAVE_ITEM

;EXIT TEST
	CPY INV_4.SCREEN_LIST.TOTAL_RECORDS ;have all items on screen been updated?
	BCS .LOOP.UPDATE.SCREEN_LIST.DONE ;if yes, then exit loop
	
;INCREMENT INDEX
	INX
	INX
	INX
	INX ;increment INV_4.SCREEN_ITEM_LIST.ARRAY index

	INY ;increment loop counter (item # 0-5)
	JMP .LOOP.UPDATE.SCREEN_LIST ;if no, continue loop
	
.LOOP.UPDATE.SCREEN_LIST.DONE
	
				; lda #$aa
				; ldx INV_4.SCREEN_LIST.TOTAL_RECORDS
				; jsr prep.brk
				; brk
				
; ;=====DEBUG HOOK======
		; STX TEMP
		
		; LDX #$00
; .DEBUG.COPY2
		; LDA INV_4.SCREEN_ITEM_LIST.ARRAY,X
		; STA $BD00,X
		; INX
		; BNE .DEBUG.COPY2

		; LDX #$00
; .DEBUG.COPY3
		; LDA INV_4.SCREEN_ITEM_LIST.DATA,X
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
	JSR INV.ERASE.TEXT_WINDOW




;PRINT UPDATED ITEM LIST
	JSR INV_4.PRINT.READIED_WEIGHT ;since readied items changed, the total weight of the readied items changed
	JSR INV_4.PRINT.ALL.ITEMS ;print all items visible on the video screen
	JSR FLIP.PAGE
	
				;INC troubleshooting.hook
				

	JMP INV_4.STATE.LOOP ;prints the item selector before falling through to the main state loop
					
	;JMP INV_4.INIT.DISPLAY.ALL_ITEMS ;prints all items, the item selector, then falls through to INV_4.STATE.LOOP.ALT.ENTRANCE
	
.EXIT

; *add code to a unready/ready submodule entrance in the main INV entrance that detects whether the sub-module is loaded or not (check a flag) and if not load the module from disk, and once loaded JSR it. 


	
	JMP INV_4.STATE.LOOP.ALT.ENTRANCE

	
@END




;SCROLLING
INV_4.SCREEN_LIST.SCROLL.UP
@START
;PARAMETERS: INV_4.SCREEN_ITEM_LIST.ARRAY(x)
;ENTRANCE: INV_4.MOVE_SELECTOR_DOWN, INV_4.MOVE_SELECTOR_UP
;RETURN: updated INV_4.SCREEN_ITEM_LIST.ARRAY(x)



;=====DEBUG HOOK======						
		; LDX #$00
; .DEBUG.COPY1
		; LDA INV_4.SCREEN_ITEM_LIST.ARRAY,X
		; STA $BC00,X
		; INX
		; BNE .DEBUG.COPY1


;SCROLL INV_4.SCREEN_ITEM_LIST.ARRAY
	LDX #$04 ;set COPY FROM record index to item 1 (of item0 - item5)
	LDY #$00 ;init loop counter
.LOOP.LIST_ARRAY.SCROLL_UP
	LDA INV_4.SCREEN_ITEM_LIST.ARRAY,X
	STA INV_4.SCREEN_ITEM_LIST.ARRAY-$4,X ;-$4 = record size
		
	INX ;next record byte
	INY ;loop counter
	CPY #INV_4.SCREEN_ITEM_LIST.ARRAY.RECORD_SIZE 
	BNE .LOOP.LIST_ARRAY.SCROLL_UP
	
	LDY #$00 ;reset loop counter to $0
	;INX ;next record byte
	CPX #INV_4.SCREEN_ITEM_LIST.ARRAY.ARRAY_SIZE
	BCC .LOOP.LIST_ARRAY.SCROLL_UP

	
;SCROLL INV_4.SCREEN_ITEM_LIST.DATA
	LDX #$20 ;set COPY FROM record index to item 1 (of item0 - item5)
	LDY #$00 ;init loop counter
.LOOP.LIST_DATA.SCROLL_UP
	LDA INV_4.SCREEN_ITEM_LIST.DATA,X
	STA INV_4.SCREEN_ITEM_LIST.DATA-$20,X
		
	INX ;next record byte
	INY ;loop counter
	CPY #INV_4.SCREEN_ITEM_LIST.DATA.RECORD_SIZE 
	BNE .LOOP.LIST_DATA.SCROLL_UP
	

			
	LDY #$00 ;reset loop counter to $0
	;INX ;next record byte
	CPX #INV_4.SCREEN_ITEM_LIST.DATA.ARRAY_SIZE
	BCC .LOOP.LIST_DATA.SCROLL_UP




			
	
; ;=====DEBUG HOOK======
		; STX TEMP
		
		; LDX #$00
; .DEBUG.COPY2
		; LDA INV_4.SCREEN_ITEM_LIST.ARRAY,X
		; STA $BD00,X
		; INX
		; BNE .DEBUG.COPY2

		; LDX #$00
; .DEBUG.COPY3
		; LDA INV_4.SCREEN_ITEM_LIST.DATA,X
		; STA $BE00,X
		; INX
		; BNE .DEBUG.COPY3

		; LDA #$AA
		; LDX TEMP
		; ;yreg
		; JSR PREP.BRK
		; BRK
		
.EXIT
	
	RTS
@END
	
INV_4.SCREEN_LIST.SCROLL.DOWN
@START
;PARAMETERS: INV_4.SCREEN_ITEM_LIST.ARRAY(x)
;ENTRANCE: INV_4.MOVE_SELECTOR_DOWN, INV_4.MOVE_SELECTOR_UP
;RETURN: updated INV_4.SCREEN_ITEM_LIST.ARRAY(x)


				
; ;=====DEBUG HOOK======						
		; LDX #$00
; .DEBUG.COPY1
		; LDA INV_4.SCREEN_ITEM_LIST.ARRAY,X
		; STA $BC00,X
		; INX
		; BNE .DEBUG.COPY1
		

			; LDA #$05
			; STA COW
				
;SCROLL INV_4.SCREEN_ITEM_LIST.ARRAY
	LDX #$13 ;set COPY FROM record index to the last byte of item 4 (of item0 - item5)
	LDY #INV_4.SCREEN_ITEM_LIST.ARRAY.RECORD_SIZE ;init loop counter = record size
.LOOP.LIST_ARRAY.SCROLL_DOWN
	LDA INV_4.SCREEN_ITEM_LIST.ARRAY,X
	STA INV_4.SCREEN_ITEM_LIST.ARRAY+$4,X ;+$4 = record size
		
	DEX ;next record byte
	DEY ;loop counter
	BNE .LOOP.LIST_ARRAY.SCROLL_DOWN
	
				; DEC COW

				; LDA COW
				; CMP #$04
				; BNE .TEMP
			; LDA #$BB
			; JSR PREP.BRK
			; BRK
.TEMP
			
				
				
	LDY #INV_4.SCREEN_ITEM_LIST.ARRAY.RECORD_SIZE ;init loop counter = record size
	CPX #$FF
	BNE .LOOP.LIST_ARRAY.SCROLL_DOWN	
	
			; LDA #$BB
			; JSR PREP.BRK
			; BRK

			
			
;SCROLL INV_4.SCREEN_ITEM_LIST.DATA
	LDX #$9F ;set COPY FROM record index to the last byte of item 4 (of item0 - item5)
	LDY #INV_4.SCREEN_ITEM_LIST.DATA.RECORD_SIZE ;init loop counter = record size
.LOOP.LIST_DATA.SCROLL_DOWN
	LDA INV_4.SCREEN_ITEM_LIST.DATA,X
	STA INV_4.SCREEN_ITEM_LIST.DATA+$20,X ;+$20 = record size
		
	DEX ;next record byte
	DEY ;loop counter
	BNE .LOOP.LIST_DATA.SCROLL_DOWN
	
	LDY #INV_4.SCREEN_ITEM_LIST.DATA.RECORD_SIZE ;init loop counter = record size
	CPX #$FF
	BNE .LOOP.LIST_DATA.SCROLL_DOWN
	
	

			
			

			
; ;=====DEBUG HOOK======
		; STX TEMP
		
		; LDX #$00
; .DEBUG.COPY2
		; LDA INV_4.SCREEN_ITEM_LIST.ARRAY,X
		; STA $BD00,X
		; INX
		; BNE .DEBUG.COPY2

		; LDA #$AB
		; LDX TEMP
		; ;yreg
		; JSR PREP.BRK
		; BRK
		
		; STX TEMP
		
		
		; ;*******THIS COPY DOESN'T SEEM TO WORK
		; LDX #$00
; .DEBUG.COPY3
		; LDA INV_4.SCREEN_ITEM_LIST.DATA,X
		; STA $BE00,X
		; INX
		; BNE .DEBUG.COPY3

		; LDA #$AB
		; LDX TEMP
		; ;yreg
		; JSR PREP.BRK
		; BRK
		
		
.EXIT
	
	RTS
@END

INV_4.DISPLAY_INVENTORY.SCROLL_UP
@START
;PARAMETER: none
;ENTRANCE: direct
;RETURN: udpated background hi-res page data

;=====================SUBROUTINE DOCUMENTATION====================================
;
;This routines scrolls the item text up (within the inventory text window). 
;For example, item 0 dissapears, item 1 scolls to the position of item 0, item 5 scrolls
;to the postion of item4, and item 5 can be overwritten with a new item added to the onscreen list. 
;
;=================================================================================


; ;SYNC HI-RES PAGES
	; JSR INV_4.SYNC_PAGES

;SAVE REGISTERS	
	TXA
	PHA
	TYA
	PHA

.INIT.SCREEN					;setup screen byte, line, depth	

	
	LDA #$B8
	STA TWF.SCROLLING.STOP_LINE				;set the stop line, which is the last line of the last row of text characters to scroll +$08. 

;INIT SCROLL START
;(each "item" on the video screen contains two rows of text characters: first row has the item qty and other stats, and the second row has the item name)
;there is a blank line between each item
;Example
;
;Item 0 (row0) 8 pixels tall
;Item 0 (row1) 8 pixels tall
;<blank line>  8 pixels tall
;Item 1 (row0) 8 pixels tall
;Item 2 (row1) 8 pixels tall
;

	LDA #$30 ;top line of the first row of text characters of the first item
	STA LINE.START.COPY_TO
	CLC
	ADC #$18 ;top line of the first row of text characters of the second item
	STA LINE.START.COPY_FROM
	CLC
	ADC #$10 ;bottom line+1 of the second row of text characters of the second item		
	STA LINE.STOP
	
.LINELOOP	
	LDY #$01					;reset screen byte index to the left edge of text window	
	
.GET.LINE.ADDRESS
		;X-REG: hi-res line number to copy TO
		LDX LINE.START.COPY_TO
		LDA PAGE.BACKGROUND
	JSR GET.LINE.ADDRESS1


		;INX ;hi-res line number	to copy FROM
		LDX LINE.START.COPY_FROM
		LDA PAGE.BACKGROUND
	JSR GET.LINE.ADDRESS2
	
.COPYLOOP
;COPY TEXT CHARACTER DATA				
	LDA (LINE.BASE.ADDR2),Y		;TEXT CHAR DATA: COPY FROM	(background page)
	STA (LINE.BASE.ADDR1),Y		;TEXT CHAR DATA: COPY TO  (background page)	
	
	INY								;advance to next screen byte in current hi-res line

	CPY #$13						;is line complete? (i.e. have all screen bytes in the line been scrolled?)
	BNE .COPYLOOP					;if no, then continue loop
	
	;**FALLS THROUGH**				;if yes, then advance to next line within current item's text characters

.NEXT.ITEM.LINE
	INC LINE.START.COPY_TO
	INC LINE.START.COPY_FROM
	LDA LINE.START.COPY_FROM
	CMP LINE.STOP
	BCC .LINELOOP
	CMP TWF.SCROLLING.STOP_LINE					;has the end of scroll range been reached? 
	BCS .SCROLL.COMPLETE						;if yes, then exit all loops

	;**FALLS THROUGH**							;if no, then increment scroll postion for next item

.NEXT.ITEM	
	LDA LINE.START.COPY_TO
	CLC
	ADC #$8 ;since the scroll TO position is at the bottom of the 2nd row of the next item to copy to, an advance of 1 character (8 lines) moved the TO position to the top line of the first row of the next item 
	STA LINE.START.COPY_TO

	LDA LINE.START.COPY_FROM
	CLC
	ADC #$8 ;similar logic applies as with LINE.START.COPY_TO above. This sets the FROM to the top line of the first row of the next item to copy from. 
	STA LINE.START.COPY_FROM
	CLC
	ADC #$10 ;since each item has 2 rows of text characters, both rows together are $10 lines deep. 
	STA LINE.STOP
	JMP .LINELOOP

	;**FALLS THROUGH**
	
.SCROLL.COMPLETE
	
;RESTORE REGISTERS	
	PLA
	TAY
	PLA
	TAX
	
	RTS

@END

INV_4.DISPLAY_INVENTORY.SCROLL_DOWN
@START
;PARAMETER: none
;ENTRANCE: direct
;RETURN: udpated background hi-res page data

;=====================SUBROUTINE DOCUMENTATION====================================
;
;This routines scrolls the item text up (within the inventory text window). 
;For example, item 0 dissapears, item 1 scolls to the position of item 0, item 5 scrolls
;to the postion of item4, and item 5 can be overwritten with a new item added to the onscreen list. 
;
;=================================================================================



			
; ;SYNC HI-RES PAGES
	; JSR INV_4.SYNC_PAGES
			
			
;SAVE REGISTERS	
	TXA
	PHA
	TYA
	PHA

.INIT.SCREEN					;setup screen byte, line, depth	

	
	LDA #$30
	STA TWF.SCROLLING.STOP_LINE				;set the stop line, which is the last line of the last row of text characters to scroll +$08. 

;INIT SCROLL START
;(each "item" on the video screen contains two rows of text characters: first row has the item qty and other stats, and the second row has the item name)
;there is a blank line between each item
;Example
;
;Item 0 (row0) 8 pixels tall
;Item 0 (row1) 8 pixels tall
;<blank line>  8 pixels tall
;Item 1 (row0) 8 pixels tall
;Item 2 (row1) 8 pixels tall
;

	LDA #$90 ;top line of the first row of text characters of the penultimate item
	STA LINE.START.COPY_FROM
	CLC
	ADC #$18 ;top line of the first row of text characters of the last item
	STA LINE.START.COPY_TO
	CLC
	ADC #$10 ;bottom line+1 of the second row of text characters of the last item	
	STA LINE.STOP
	
.LINELOOP	
	LDY #$01					;reset screen byte index to the left edge of text window	
	
			
.GET.LINE.ADDRESS
		;INX ;hi-res line number	to copy FROM
		LDX LINE.START.COPY_FROM
		LDA PAGE.BACKGROUND
	JSR GET.LINE.ADDRESS2

		;X-REG: hi-res line number to copy TO
		LDX LINE.START.COPY_TO
		LDA PAGE.BACKGROUND
	JSR GET.LINE.ADDRESS1




			
.COPYLOOP
;COPY TEXT CHARACTER DATA				
	LDA (LINE.BASE.ADDR2),Y		;TEXT CHAR DATA: COPY FROM	(background page)
	STA (LINE.BASE.ADDR1),Y		;TEXT CHAR DATA: COPY TO  (background page)	
	
	INY								;advance to next screen byte in current hi-res line

	CPY #$13						;is line complete? (i.e. have all screen bytes in the line been scrolled?)
	BNE .COPYLOOP					;if no, then continue loop
	
	;**FALLS THROUGH**				;if yes, then advance to next line within current item's text characters

.NEXT.ITEM.LINE

	INC LINE.START.COPY_FROM
	INC LINE.START.COPY_TO
	LDA LINE.START.COPY_TO
	CMP LINE.STOP
	BCC .LINELOOP

	;**FALLS THROUGH**							;if no, then increment scroll postion for next item

.NEXT.ITEM			
	LDA LINE.START.COPY_FROM
	SEC
	SBC #$28 ;similar logic applies as with LINE.START.COPY_TO above. This sets the FROM to the top line of the first row of the next item to copy from. 
	STA LINE.START.COPY_FROM
	CMP TWF.SCROLLING.STOP_LINE					;has the end of the scroll range been reached? 
	BCC .SCROLL.COMPLETE						;if yes, then exit all loops
		
	LDA LINE.START.COPY_TO
	SEC
	SBC	#$28 ;since the scroll TO position is at the bottom of the 2nd row of the next item to copy to, an advance of 1 character (8 lines) moved the TO position to the top line of the first row of the next item 
	STA LINE.START.COPY_TO	
	CLC
	ADC #$10 ;since each item has 2 rows of text characters, both rows together are $10 lines deep. 
	STA LINE.STOP
	
					

					
	
	JMP .LINELOOP

	;**FALLS THROUGH**
	
.SCROLL.COMPLETE
	
;RESTORE REGISTERS	
	PLA
	TAY
	PLA
	TAX
	
	RTS

@END


		
;PRINT TEXT				
INV_4.PRINT.ALL.ITEMS
@START
;=====================SUBROUTINE DOCUMENTATION===========================================================================
;
;For each item on the video screen, prints both lines of item text to the background hi-res page. 
;The data to print is obtained by iterating through INV_4.SCREEN_ITEM_LIST.ARRAY and INV_4.SCREEN_ITEM_LIST.DATA
;
;=============================================================================================================================

.INIT.SCREEN_POSITION
		LDA #INV_4.SCREEN_LIST.ITEM0.VTAB
		STA VTAB
		
		LDA #INV_4.SCREEN_LIST.ITEM_ALL.HTAB.LEFT
		STA HTAB
	JSR	UPDATE.CHAR.POS	
	

	LDY #$00 ;init loop counter (on screen item counter)
.LOOP.PRINT_ITEMS

	;init indexes
	TYA
	ASL ;X2
	ASL ;X4
	TAX ;set INV_4.SCREEN_ITEM_LIST.ARRAY index ($4 byte records)
	
	;exit test (no more items)
	LDA INV_4.SCREEN_ITEM_LIST.ARRAY+$03,X ;load readied status field
	BPL .INIT_DONE ;branch if init value not detected
	JMP .LOOP.PRINT_ITEMS.DONE ;JMP if init value detected

.INIT_DONE


.PRINT.BOTH_LINES ;OF ITEM TEXT
	JSR INV_4.VIDEO_OUT.ITEM_LINE1_2
	

	;**OPT** Memory. It is possible that INV_4.SCREEN_ITEM_LIST.DATA, which contains the entire $20 byte item record for each onscreen item,
			;can be reduced in size to just contain the item name and weight for each record since those are the only
			;fields currently in use. However, if we output the item stats to the scroll window then the other fields would be needed. 
	



.NEXT_ITEM
		LDA #INV_4.SCREEN_LIST.ITEM_ALL.HTAB.LEFT
		STA HTAB
		INC VTAB ;insert blank line before next item
		INC VTAB ;advance to next item
	JSR	UPDATE.CHAR.POS	
	
.EXIT_TEST ;(max items reached)
	INY ;increment loop counter
	CPY #INV_4.MAX_ONSCREEN.ITEMS
	BEQ .LOOP.PRINT_ITEMS.DONE ;branch if max items already printed onscreen 
	JMP .LOOP.PRINT_ITEMS ;else continue loop
	
.LOOP.PRINT_ITEMS.DONE

.EXIT

			; LDA #INV_4.SCREEN_ITEM_LIST.DATA
			; STA $BF00
			; LDA /INV_4.SCREEN_ITEM_LIST.DATA
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

INV_4.PRINT.SELECTED.ITEM ;(prints both lines of item text)
@START
;PARAMETERS: ACC ($00 = item selector OFF | $01 = item selector ON), INV_4.SCREEN_ITEM_LIST.SELECTED.ITEM, carry flag (CLC = don't sync hi-res pages | SEC = sync hi-res pages)
;ENTRANCE:
;RETURN: video screen output

;Note: carry flag parm makes syncing pages optional so that mutiple calls can be done to this routine, presenting all changes to the background page at the same time. Otherwise a page flip
;would be needed after each call to this routine. 		
			
			
.SAVE.PARAMETERS
		;ACC ($00 = item selector OFF | $7F = item selector ON) 
		STA COUT_CHAR_TYPE ;($00 = normal, $7F = inverse)

	
.INIT
	;carry flag (CLC = don't sync hi-res pages | SEC = sync hi-res pages)
	BCC .SYNC.DONE

	JSR INV_4.SYNC_PAGES ;must come before INV_4.VIDEO_OUT.ITEM_LINE1_2
.SYNC.DONE
	
	;get vtab	
	LDX INV_4.SCREEN_ITEM_LIST.SELECTED.ITEM
	
	TXA ;save INV_4.SCREEN_ITEM_LIST.SELECTED.ITEM
	PHA	
	
	LDA INV_4.SCREEN_LIST_ITEM.LOOKUP_TABLE.VTAB,X
	STA VTAB ;set VTAB of item to print
	
	
		
.PRINT_ERASE.SELECTOR
@START


;SET SCREEN POSITION OF SELECTOR
	;ACC = VTAB of selected item
	ASL ;X2
	ASL ;X4
	ASL ;X8 (8 lines per character)
	TAX ;set start line
	CLC
	ADC #$10 ;add 2 character depths
	STA DRAW.STOP_LINE
	
	LDY #INV_4.SCREEN_LIST.ITEM_ALL.HTAB.LEFT ;set start SBYTE


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
	CPY #INV_4.SCREEN_LIST.ITEM_ALL.HTAB.RIGHT+1
	BNE .PLOT.LOOP
	LDY #INV_4.SCREEN_LIST.ITEM_ALL.HTAB.LEFT ;reset SBYTE to line start position
	INX
	CPX DRAW.STOP_LINE
	BNE .LINE.LOOP


@END


.PRINT.ITEM_TEXT		

		PLA ;restore INV_4.SCREEN_ITEM_LIST.SELECTED.ITEM
		TAY ;transfer INV_4.SCREEN_ITEM_LIST.SELECTED.ITEM
		;calculate INV_4.SCREEN_ITEM_LIST.ARRAY index for active player
		ASL ;X2
		ASL ;X4
		TAX ;set index to INV_4.SCREEN_ITEM_LIST.ARRAY for item to print
		;summary of parms set above for INV_4.VIDEO_OUT.ITEM_LINE1_2: X-REG, Y-REG, INV_4.SCREEN_ITEM_LIST.SELECTED.ITEM
	JSR INV_4.VIDEO_OUT.ITEM_LINE1_2

	
.EXIT

	RTS
	
@END
	
	

INV_4.VIDEO_OUT.ITEM_LINE1_2
@START
;PARAMETERS: VTAB, X-REG (index to INV_4.SCREEN_ITEM_LIST.ARRAY for item to print), Y-REG = item number to print, [COUT_CHAR_TYPE ;($00 = normal, $7F = inverse)]
;ENTRANCE: INV_4.PRINT.ALL.ITEMS, INV_4.PRINT.SELECTED.ITEM
;RETURN: video screen output


.INIT
	LDA COUT_CHAR_TYPE ;($00 = normal, $7F = inverse)
	STA COUT_CHAR_TYPE.SAVED
	
.PRINT.LINE1
@START
		LDA #INV_4.SCREEN_LIST.ITEM_ALL.READIED_HTAB
		STA HTAB
		;VTAB: use VTAB positon set by calling routine
	JSR	UPDATE.CHAR.POS	
	

;PRINT READIED INDICATOR
@START
;(for non-spell menus, the readied indicator is * or ** to indicate the item is readied once or twice respectively by the active player
;for the spell menu, the readied indicator is *(x) where x is the spell hotkey number the spell is readied to.

;BRANCH
	LDA INV.ACTIVE_MENU.CODE 
	CMP #INV.ACTIVE_MENU.SPELLS ;INV_4.DISPLAY.MODE = $02 (misc items), but is spell menu active?
	BEQ	.SPELL_MENU.ACTIVE
	;**FALLS THROUGH**
.SPELL_MENU.NOT_ACTIVE	
	;print readied indicator (* or **), if item is readied by active player
	LDA INV_4.SCREEN_ITEM_LIST.ARRAY+$3,X ;load readied flag field of item list
	BEQ .PRINT.READIED_INDICATOR.DONE ;branch if item isn't readied at least once
		LDA #$AA 
	JSR .CALL.COUT.BACKGROUND		;print first *
	LDA INV_4.SCREEN_ITEM_LIST.ARRAY+$3,X ;load readied flag field of item list
	CMP #$02 ;branch if item isn't readied twice
	BNE .PRINT.READIED_INDICATOR.DONE
		LDA #$AA 
	JSR .CALL.COUT.BACKGROUND		;print second *
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
	TYA ;load item number to print
	ASL ;X !2
	ASL ;X !4
	ASL ;X !8
	ASL ;X !16
	ASL ;X !32
	TAX ;set INV_4.SCREEN_ITEM_LIST.DATA index ($20 byte records)
	
	LDY #$00 ;init loop counter & PLAYER.HOTKEYS.SPELLS index
.LOOP.SPELL_MENU.IS.READIED	
	LDA PLAYER.HOTKEYS.SPELLS,Y
	CMP INV_4.SCREEN_ITEM_LIST.DATA+INV.ITEM_TABLE.MI.SPELL_CODE.OFFSET,X ;load item weight for current item
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
@START
		LDA #INV_4.SCREEN_LIST.ITEM_ALL.QTY_HTAB
		STA HTAB
		;VTAB = keep current value
	JSR	UPDATE.CHAR.POS	
	
		LDA INV_4.SCREEN_ITEM_LIST.ARRAY+$2,X ;load available quantity (total qty - total readied) for current item
	JSR CONVERT.HEX.8_TO_BCD.16
		;RETURN VALUE: BCD(2)
		
		;does QTY = $0?
		LDA BCD+$0
		CLC
		ADC BCD+$1
		BEQ .PRINT.DASH
		
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
	JSR .CALL.COUT.BACKGROUND		;print second *

		;**FALLS THROUGH**

.PRINT.QTY.DONE
@END
	
;PRINT ITEM WEIGHT
@START
		LDA #INV_4.SCREEN_LIST.ITEM_ALL.WT_HTAB
		STA HTAB
		;VTAB = keep current value
	JSR	UPDATE.CHAR.POS	

	;init indexes
	TYA ;load item number to print
	ASL ;X !2
	ASL ;X !4
	ASL ;X !8
	ASL ;X !16
	ASL ;X !32
	TAX ;set INV_4.SCREEN_ITEM_LIST.DATA index ($20 byte records)
	
		;**OPT** Memory. turn this into a common subroutine (in inventory module), and replace all calls to PRINT.BCD_PACKED
		LDA INV_4.SCREEN_ITEM_LIST.DATA+INV.ITEM_TABLE.ALL.STR_WEIGHT.OFFSET,X ;load item weight for current item
	JSR CONVERT.HEX.8_TO_BCD.16
		;RETURN VALUE: BCD(2)
	
		;PARM: BCD(2) was set as return value above
		
		;restore COUT_CHAR_TYPE parm (might be set to $7F depending on the path taken to call this routine)
		LDA COUT_CHAR_TYPE.SAVED
		STA COUT_CHAR_TYPE ;($00 = normal, $7F = inverse)		
	JSR PRINT.BCD_PACKED.STR.BACKGROUND
	
.PRINT.LINE1.DONE
@END

@END

.PRINT.LINE2
@START
		LDA #INV_4.SCREEN_LIST.ITEM_ALL.NAME_HTAB 
		STA HTAB
		INC VTAB ;advance to 2nd text line of item
	JSR	UPDATE.CHAR.POS	

	
		;**OPT** Memory. Used this version of the "load item name for current item" once SBASM bug is fixed
		;which causes the LDAs below to use the wrong opcode (it uses immediate mode instead of immediate mode, indexed by X)
		; ;load item name for current item
		; LDA #INV_4.SCREEN_ITEM_LIST.DATA+INV.ITEM_TABLE.ALL.NAME.OFFSET,X ;add the table base address and the offset to the item name field
		; STA STRING+$0
		; LDA /INV_4.SCREEN_ITEM_LIST.DATA+INV.ITEM_TABLE.ALL.NAME.OFFSET,X
		; STA STRING+$1

				
		
		;load item name for current item
		TXA ;record index (for the onscreen item currently being processed)
		CLC
		ADC #INV_4.SCREEN_ITEM_LIST.DATA+INV.ITEM_TABLE.ALL.NAME.OFFSET ;add the table base address and the offset to the item name field
		STA STRING+$0
		LDA /INV_4.SCREEN_ITEM_LIST.DATA+INV.ITEM_TABLE.ALL.NAME.OFFSET
		ADC #$00 ;16-bit add
		STA STRING+$1		
		
	
				
					;JMP DEBUG.HOOK
				;***REMOVE JMP DEBUG.HOOK ABOVE
				
		;restore COUT_CHAR_TYPE parm (might be set to $7F depending on the path taken to call this routine)
		LDA COUT_CHAR_TYPE.SAVED
		STA COUT_CHAR_TYPE ;($00 = normal, $7F = inverse)		
	JSR PRINT.STR.BACKGROUND

.PRINT.LINE2.DONE
@END

.EXIT
	RTS


.CALL.COUT.BACKGROUND
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
	
@END
		
INV_4.ADDITIONAL_INFO
@START
;PARAMETERS: none
;ENTRANCE: direct from any INV_4 routine
;RETURN: updated video screen (scroll window)

;NOTE: prints additional information on the selected item (from master item table) into the scroll text window in the lower right part of the screen. 

.INIT.CURSOR

	JSR INV_4.RESTORE.TW.CURSOR


	
.INIT.HEADER_TEXT
	JSR PRINT.TEXT.WINDOW.CR
		LDA #$AD ;ASCII = "-"
	JSR PRINT.TEXT.WINDOW.SINGLE.CHAR

		LDA #$AD ;ASCII = "-"
	JSR PRINT.TEXT.WINDOW.SINGLE.CHAR 			;**OPT** Memory. We could probably make do with a one dash header instead of two dashes.

	JSR PRINT.TEXT.WINDOW.CR

	
	
.INIT.INDEX	
	;Calculate index to INV_4.SCREEN_ITEM_LIST.DATA for selected item
	LDA INV_4.SCREEN_ITEM_LIST.SELECTED.ITEM
	ASL ;X2
	ASL ;X4
	; TAX ;save index to INV_4.SCREEN_ITEM_LIST.ARRAY (4 byte records)
	ASL ;X8
	ASL ;X!16
	ASL ;X!32
	TAY ;save index to INV_4.SCREEN_ITEM_LIST.DATA ($20 byte records)
	
	LDA INV_4.DISPLAY.MODE ;($00 = display weapons | $01 = display armor | $02 = misc items)
	
	; LDA INV_4.SCREEN_ITEM_LIST.ARRAY+$0,X ;load (item level | type) field of selected item
	; ;item type is in LO nibble
	; AND #$0F ;mask out HO nibble
	BNE .PRINT_INFO.ARMOR_MISC

	;**FALLS THROUGH**
	
.PRINT_INFO.WEAPON
@START

.BASE_DMG
;PRINT LABEL	
		LDA #INV_4.SCROLL_WINDOW.WP.BASE_DMG				
		STA TWF.STRING
		
		LDA /INV_4.SCROLL_WINDOW.WP.BASE_DMG
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW

;PRINT VALUE

		;load based damage for selected item		
		LDA INV_4.SCREEN_ITEM_LIST.DATA+INV.ITEM_TABLE.WP.DMG_PWR.OFFSET,Y ;load based damage for selected item
		CLC ;(CLC = don't print CR | SEC = print CR to text window)
	JSR PRINT.TEXT.WINDOW.HEX8.RJ


.HANDS_REQUIRED
		LDA INV_4.SCREEN_ITEM_LIST.DATA+INV.ITEM_TABLE.WP.SHAPE_ID_HANDS.OFFSET,Y ;load hands required for selected item (high-bit set = 2 handed | high-bit not set = 1 handed)
		BMI	.2_HANDED_WEAPON ;branch if high-bit set

.1_HANDED_WEAPON	
;PRINT LABEL	
		LDA #INV_4.SCROLL_WINDOW.WP.1_HANDED				
		STA TWF.STRING
		
		LDA /INV_4.SCROLL_WINDOW.WP.1_HANDED
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW
	
	JMP .HANDS_REQUIRED.DONE

.2_HANDED_WEAPON
		LDA #INV_4.SCROLL_WINDOW.WP.2_HANDED				
		STA TWF.STRING
		
		LDA /INV_4.SCROLL_WINDOW.WP.2_HANDED
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW
.HANDS_REQUIRED.DONE
	
.MAGIC.WEAPON
		LDA INV_4.SCREEN_ITEM_LIST.DATA+INV.ITEM_TABLE.WP.MAGIC_FLAG.OFFSET,Y ;load magic weapon flag for selected item (high-bit not set = no magic weapon | high-bit set = magic weapon)
		BMI	.IS_MAGIC ;branch if high-bit set

		
.ISNT_MAGIC	
;PRINT LABEL	
		LDA #INV_4.SCROLL_WINDOW.WP.ISNT_MAGIC				
		STA TWF.STRING
		
		LDA /INV_4.SCROLL_WINDOW.WP.ISNT_MAGIC
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW
	
	JMP .MAGIC.WEAPON.DONE

.IS_MAGIC
		LDA #INV_4.SCROLL_WINDOW.WP.IS_MAGIC				
		STA TWF.STRING
		
		LDA /INV_4.SCROLL_WINDOW.WP.IS_MAGIC
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW
.MAGIC.WEAPON.DONE


	JMP .EXIT
	
@END


.PRINT_INFO.ARMOR_MISC
@START

.ARMOR_RATING
;PRINT LABEL	
		LDA #INV_4.SCROLL_WINDOW.AR_MISC.ARMOR_RATING				
		STA TWF.STRING
		
		LDA /INV_4.SCROLL_WINDOW.AR_MISC.ARMOR_RATING
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW

;PRINT VALUE
		LDA INV_4.SCREEN_ITEM_LIST.DATA+INV.ITEM_TABLE.AR_MISC.DEFENSE_RATING.OFFSET,Y ;load based damage for selected item
		CLC ;(CLC = don't print CR | SEC = print CR to text window)
	JSR PRINT.TEXT.WINDOW.HEX8.RJ
	

.RESIST_MAGIC
;PRINT LABEL	
		LDA #INV_4.SCROLL_WINDOW.AR_MISC.RESIST_MAGIC				
		STA TWF.STRING
		
		LDA /INV_4.SCROLL_WINDOW.AR_MISC.RESIST_MAGIC
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW

;PRINT VALUE
		LDA INV_4.SCREEN_ITEM_LIST.DATA+INV.ITEM_TABLE.AR_MISC.MGK.RESIST.OFFSET,Y ;load based damage for selected item
		CLC ;(CLC = don't print CR | SEC = print CR to text window)
	JSR PRINT.TEXT.WINDOW.HEX8.RJ
	
	
		JSR PRINT.TEXT.WINDOW.CR

	
@END

	;**FALLS THROUGH**
		
.EXIT
		JSR PRINT.TEXT.WINDOW.CR

		JSR INV_4.SAVE.TW.CURSOR


		
	JMP INV_4.STATE.LOOP.ALT.ENTRANCE
@END

INV_4.PRINT.READIED_WEIGHT
@START
;PARAMETERS: none
;ENTRANCE: direct from any INV_4 routine
;RETURN: updated video screen (static text portion of scroll window)

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
		CLC ;(CLC = don't print CR | SEC = print CR to text window)
	JSR PRINT.TEXT.WINDOW.HEX8.RJ

	
	; JSR CONVERT.HEX.8_TO_BCD.16
		; ;RETURN VALUE: BCD(2)
		
		; ;Parm BCD: value set above
	; JSR PRINT.BCD_PACKED

	
	
	RTS
	
@END


INV_4.PRINT.MISC_TEXT	
@START
;PARAMETERS: ACC = ($00 item readied | $01 = item not readied: item is not readiable | $02 don't use| $03 item not readied: STR gate check failed | $04 item not readied: DEX gate check failed | $05 item not readied: INT gate check failed | $06 can't ready armor during combat | $07 item unreadied | $08 prompt spell hotkey to ready)
;ENTRANCE: any INV_4 routine
;RETURN: scroll text output

;SAVE PARAMETERS
	
	;ACC: parm: text block code
	STA INV_4.PRINT.MISC_TEXT.MODE
	
; ;SAVE REGISTERS
	; TXA
	; PHA


.START	

	;TAX ;save parm text block to print
	JSR INV_4.RESTORE.TW.CURSOR
	; PLA ;restore parm text block to print 
	;TXA ;restore parm text block to print 
	LDA INV_4.PRINT.MISC_TEXT.MODE		
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

				
	LDA INV_4.PRINT.MISC_TEXT.MODE	;($00 item readied | $01 = item not readied: item is not readiable | $02 don't use| $03 item not readied: STR gate check failed | $04 item not readied: DEX gate check failed | $05 item not readied: INT gate check failed | $06 can't ready armor during combat | $07 item unreadied | $08 prompt spell hotkey to ready)
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
;Unexpected text block to print code in INV_4.PRINT.MISC_TEXT. This parm is sometimes set by the return value of
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
	
	JSR INV_4.SAVE.TW.CURSOR

; ;RESTORE REGISTERS
	; PLA
	; TAX
	
	RTS
	
@END

INV_4.RESTORE.TW.CURSOR
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
	
INV_4.SAVE.TW.CURSOR
@START		
	;SAVE SCROLL TEXT WINDOW CURSOR POSITION
	LDA HTAB
	STA TWF.HTAB.SAVED
	
	LDA VTAB
	STA TWF.VTAB.SAVED		
		
	RTS
@END



;GRAPHICS
INV_4.SYNC_PAGES
@START
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
	STA LINE.START	;set starting line for the copy from part of the scroll operation
	
	LDA #$B8
	STA LINE.STOP				;set the stop line, which is the last line of the last row of text characters to scroll +$08. 

		LDX LINE.START	;hi-res line number	to copy FROM	
.LINELOOP	
	LDY #$01					;reset screen byte index to the left edge of text window	
	
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
	
	INY								;advance to next screen byte

	CPY #$13						;is line complete? (i.e. have all screen bytes in the line been scrolled?)
	BNE .COPYLOOP
	
	INX
	CPX LINE.STOP					;has end of inventory window been reached?
	BCC .LINELOOP					;if no, scroll next line
	
	;**FALLS THROUGH**

.SCROLL.COMPLETE


;RESTORE REGISTERS	
	PLA
	TAY
	PLA
	TAX
	
	RTS
	
@END

INV_4.DRAW_ERASE.SCROLL_WINDOW
@START	
	LDA #TWB.INVENTORY.SCROLL_WINDOW.TOP_LINE		;load line in x register	
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
	
	RTS
@END


@END

;LOCAL TEXT BLOCKS
@START
;headings
INV_4.TEXT_BLOCK.HEADER1			.AZ -/ITEM        WT QTY/

;misc: other
INV_4.TEXT_BLOCK.SPELL_READIED_INDICATOR	.AZ -/*(x)/

;scroll window: misc
INV_4.SCROLL_WINDOW.READIED			.AZ -#$8D,/--/,#$8D,/Item readied/
INV_4.SCROLL_WINDOW.UNREADIED		.AZ -#$8D,/--/,#$8D,/Item unreadied/
INV_4.SCROLL_WINDOW.NOT_READIABLE	.AZ -#$8D,/--/,#$8D,/Item can't be readied/	
INV_4.SCROLL_WINDOW.NO.ARMOR_CHANGE_IN_COMBAT .AZ -#$8D,/--/,#$8D,/Can't change armor while involved in a fracas/

INV_4.SCROLL_WINDOW.STR_GATE_CHECK.FAILED 	.AZ -#$8D,/--/,#$8D,/Strength too low to wield/
INV_4.SCROLL_WINDOW.DEX_GATE_CHECK.FAILED 	.AZ -#$8D,/--/,#$8D,/Dexterity too low to wield/
INV_4.SCROLL_WINDOW.INT_GATE_CHECK.FAILED 	.AZ -#$8D,/--/,#$8D,/Intelligence too low to wield/

INV_4.SCROLL_WINDOW.HOTKEY_PROMPT			.AZ	-/Ready to which spell slot? (0-9)/

;scroll window: additional information printout
INV_4.SCROLL_WINDOW.INTRO			.AZ -/Press "I" for more information on the selected item/

INV_4.SCROLL_WINDOW.READIED_WEIGHT.LINE1	.AZ -/Readied Equip/
INV_4.SCROLL_WINDOW.READIED_WEIGHT.LINE2	.AZ -/Weight:   /

INV_4.SCROLL_WINDOW.WP.BASE_DMG		.AZ -/Base DMG  /
INV_4.SCROLL_WINDOW.WP.1_HANDED		.AZ -/1-Handed/
INV_4.SCROLL_WINDOW.WP.2_HANDED		.AZ -/2-Handed/

INV_4.SCROLL_WINDOW.WP.ISNT_MAGIC	.AZ	-/Non-magic item/
INV_4.SCROLL_WINDOW.WP.IS_MAGIC		.AZ	-/Magic item/


INV_4.SCROLL_WINDOW.AR_MISC.ARMOR_RATING	.AZ	-/Armor RT  /
INV_4.SCROLL_WINDOW.AR_MISC.RESIST_MAGIC	.AZ	-/Resist Mgk/

@END


;LOCAL VARIABLES
@START

INV_4.SCREEN_ITEM_LIST.DATA.RECORD_SIZE .EQ $20	;#CONSTANT
INV_4.SCREEN_ITEM_LIST.DATA.ARRAY_SIZE 	.EQ $C0	;#CONSTANT
INV_4.SCREEN_ITEM_LIST.DATA .BS INV_4.SCREEN_ITEM_LIST.DATA.ARRAY_SIZE 

INV_4.SCREEN_LIST_ITEM.LOOKUP_TABLE.VTAB .HS 06.09.0C.0F.12.15


@END


;INV_4.TEST1		.AZ -/Text window print test. How did it go?/


	;.BS $D00