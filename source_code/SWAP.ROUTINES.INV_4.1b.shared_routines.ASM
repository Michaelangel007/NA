;************************INCLUDE FILE*****************************
;(do not assemble stand-alone.  )


;=====================INCLUDE FILE DOCUMENTATION====================================
;
;Include file to SWAP.ROUTINES.INVENTORY.ASM
;
;=================================================================================





;!!!TEMPORARY!!!!
INV_4.1b.ROUTINES
@START

;MENU MANAGEMENT
INV_4.EXIT ;main exit routine (exit's inventory module entirely)
@START
;PARAMETERS: ACC = ASCII code of keypress


				
	PHA ;save state_loop keypress
	
	
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


;ERASE SCROLL WINDOW (LOWER RIGHT) | OR ERASE PLAYER INVENTORY WINDOW
;
;-erase scroll window
;(this is so that there isn't left over text when other sub_modules (such as stats) have control and so that
;if the inventory module was launched from combat that upon exit back to combat the scroll window doesn't have any
;left over text in it. It is easier to clear the window here than in combat because the combat text window setup routines are in combat setup)
;
;-erase player inventory window
;(do this if in merchant interface mode so that when control is switched back
;to the merchant inventory window the player inventory window is blank. This is
;done so that when the player buys an item, the player inventory window doesn't
;need to be refreshed, which would also cause the merchant window to be refreshed
;because they both share the same data arrays. 
;
;VALIDATE ENTRANCE
;(validate entrance is needed because INV_4.DRAW_ERASE.SCROLL_WINDOW is called from two places)
.INV_4.MERCH_INTERFACE.BRANCH ;exit if call to this subroutine was from INV_8 (merchant transactions) 	
	;is merchant transactions (MT) sub_module loaded in memory, and if so was the call to INV_4 from the .INIT_SCREEN code (initial launch)?
	LDA INV.SUB_MODULE.LOAD_FLAGS+$8 ;($00 = sub-module not loaded in memory | $01 = sub-module loaded in memory | high-bit set = initial launch complete)
	BNE .MERCHANT_INTERFACE.MODE	 ;branch if MT module in memory (skip scroll window draw because dimensions are different when merchant transactions is active)

;REGULAR INTERFACE MODE	
	JSR INV_4.DRAW_ERASE.SCROLL_WINDOW
	JMP .INTERFACE_MODE.BRANCH.DONE
	
.MERCHANT_INTERFACE.MODE
	JSR INV.ERASE.INVENTORY_WINDOW.TEXT_SPACE ;foreground and background page /w flip

.INTERFACE_MODE.BRANCH.DONE
	
					; LDA #$01
					; STA TROUBLESHOOTING.HOOK
		;jsr keyin
	
		PLA ;restore parm to INV.NEXT.MENU. ;(high-bit not set = tab pressed, increment menu | $B1-B6 = ASCII code to hotkey to specific menu) 

	
	JMP INV.NEXT.MENU 

	
.EXIT.INVENORY.MODULE
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
	


			
	;Before exiting module, check if any ready/unreadies occured and if so update combat stats
	JSR INV.RUN.CALCULATE.COMBAT.STATS.IF_CHANGES_OCUURED

	JMP INV.INVENTORY_MODULE.EXIT
	
@END


;DATA
INV_4.1b.READ.PLAYER_INVENTORY.DATA
@START
;PARAMETERS: INV_4.1b.PLAYER_INVENTORY.FILE_DATA.FLAG ($00 = not in memory | >=$01 in memory)
;RETURN: INV.PLAYER.INVENTORY.DATA

;VALIDATE ENTRANCE
	LDA INV_4.1b.PLAYER_INVENTORY.FILE_DATA.FLAG ;($00 = not in memory | >=$01 in memory)
	BNE .EXIT ;branch if player inventory data is already in memory
	
;READ PLAYER INVENTORY DATA FROM FILE
	JSR INV.FILE.READ.INVENTORY_DATA.ENTRANCE
		;RETURN VALUE: INV.FILE.READ.INVENTORY.OPEN_STATUS ;($00 = file was open. $01 = file was not open)
	CMP #$00
	BNE .ERROR.INVENTORY_FILE.NOT_OPEN

	
;set player inventory flag  = in memory
	LDA #$01 ;**OPT** Memory. Once the sort routine below is enabled, embedd the STA below into it since it will have a non-zero value in the ACC on a few occasions
	STA INV_4.1b.PLAYER_INVENTORY.FILE_DATA.FLAG ;($00 = not in memory | >=$01 in memory)
	
	


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

	;***MEMORY RESERVATION FOR THE INVENTORY SORT CODE BELOW
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	
	
.EXIT
	RTS

.ERROR.INVENTORY_FILE.NOT_OPEN ;**OPT** Memory. This hasn't been a problem. Can probably remove this error trap any time. 
;.INIT.INVENTORY.DATA reports that inventory file was not open. It should be at this point in the program flow and if it is not that
;indicates a bug or design flaw. Opening the file at this point in the program should be avoided because opening a file is a slow process. 	
	JSR PREP.BRK
	BRK
	
@END

INV_4.1b.GET.SCREEN_LIST.ARRAY.INDEX
@START	
;PARAMETERS: INV_4.1b.SCREEN_ITEM_LIST.SELECTED.ITEM
;ENTRANCE: INV_4, INV_8 routines
;RETURN: X-REG (index to SCREEN_ITEM_LIST.ARRAY for the item selected on screen)

	;calculate index to INV_4.1b.SCREEN_ITEM_LIST.ARRAY for selected item
	LDA INV_4.1b.SCREEN_ITEM_LIST.SELECTED.ITEM ;load item number (0-5) currently selected
	ASL ;X2
	ASL ;X4 (4 byte record)
	TAX
	
	RTS
@END


;SCROLLING
INV_4.1b.SCREEN_LIST.SCROLL.UP
@START
;PARAMETERS: INV_4.1b.SCREEN_ITEM_LIST.ARRAY(x)
;ENTRANCE: INV_4.MOVE_SELECTOR_DOWN, INV_4.MOVE_SELECTOR_UP
;RETURN: updated INV_4.1b.SCREEN_ITEM_LIST.ARRAY(x)



;=====DEBUG HOOK======						
		; LDX #$00
; .DEBUG.COPY1
		; LDA INV_4.1b.SCREEN_ITEM_LIST.ARRAY,X
		; STA $BC00,X
		; INX
		; BNE .DEBUG.COPY1


;SCROLL INV_4.1b.SCREEN_ITEM_LIST.ARRAY
	LDX #$04 ;set COPY FROM record index to item 1 (of item0 - item5)
	LDY #$00 ;init loop counter
.LOOP.LIST_ARRAY.SCROLL_UP
	LDA INV_4.1b.SCREEN_ITEM_LIST.ARRAY,X
	STA INV_4.1b.SCREEN_ITEM_LIST.ARRAY-$4,X ;-$4 = record size
		
	INX ;next record byte
	INY ;loop counter
	CPY #INV_4.1b.SCREEN_ITEM_LIST.ARRAY.RECORD_SIZE 
	BNE .LOOP.LIST_ARRAY.SCROLL_UP
	
	LDY #$00 ;reset loop counter to $0
	;INX ;next record byte
	CPX #INV_4.1b.SCREEN_ITEM_LIST.ARRAY.ARRAY_SIZE
	BCC .LOOP.LIST_ARRAY.SCROLL_UP

	
;SCROLL INV_4.1a.SCREEN_ITEM_LIST.DATA
	LDX #$20 ;set COPY FROM record index to item 1 (of item0 - item5)
	LDY #$00 ;init loop counter
.LOOP.LIST_DATA.SCROLL_UP
	LDA INV_4.1a.SCREEN_ITEM_LIST.DATA,X
	STA INV_4.1a.SCREEN_ITEM_LIST.DATA-$20,X
		
	INX ;next record byte
	INY ;loop counter
	CPY #INV_4.1a.SCREEN_ITEM_LIST.DATA.RECORD_SIZE 
	BNE .LOOP.LIST_DATA.SCROLL_UP
	

			
	LDY #$00 ;reset loop counter to $0
	;INX ;next record byte
	CPX #INV_4.1a.SCREEN_ITEM_LIST.DATA.ARRAY_SIZE
	BCC .LOOP.LIST_DATA.SCROLL_UP




			
	
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
		
.EXIT
	
	RTS
@END
	
INV_4.1b.SCREEN_LIST.SCROLL.DOWN
@START
;PARAMETERS: INV_4.1b.SCREEN_ITEM_LIST.ARRAY(x)
;ENTRANCE: INV_4.MOVE_SELECTOR_DOWN, INV_4.MOVE_SELECTOR_UP
;RETURN: updated INV_4.1b.SCREEN_ITEM_LIST.ARRAY(x)


				
; ;=====DEBUG HOOK======						
		; LDX #$00
; .DEBUG.COPY1
		; LDA INV_4.1b.SCREEN_ITEM_LIST.ARRAY,X
		; STA $BC00,X
		; INX
		; BNE .DEBUG.COPY1
		

			; LDA #$05
			; STA COW
				
;SCROLL INV_4.1b.SCREEN_ITEM_LIST.ARRAY
	LDX #$13 ;set COPY FROM record index to the last byte of item 4 (of item0 - item5)
	LDY #INV_4.1b.SCREEN_ITEM_LIST.ARRAY.RECORD_SIZE ;init loop counter = record size
.LOOP.LIST_ARRAY.SCROLL_DOWN
	LDA INV_4.1b.SCREEN_ITEM_LIST.ARRAY,X
	STA INV_4.1b.SCREEN_ITEM_LIST.ARRAY+$4,X ;+$4 = record size
		
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
			
				
				
	LDY #INV_4.1b.SCREEN_ITEM_LIST.ARRAY.RECORD_SIZE ;init loop counter = record size
	CPX #$FF
	BNE .LOOP.LIST_ARRAY.SCROLL_DOWN	
	
			; LDA #$BB
			; JSR PREP.BRK
			; BRK

			
			
;SCROLL INV_4.1a.SCREEN_ITEM_LIST.DATA
	LDX #$9F ;set COPY FROM record index to the last byte of item 4 (of item0 - item5)
	LDY #INV_4.1a.SCREEN_ITEM_LIST.DATA.RECORD_SIZE ;init loop counter = record size
.LOOP.LIST_DATA.SCROLL_DOWN
	LDA INV_4.1a.SCREEN_ITEM_LIST.DATA,X
	STA INV_4.1a.SCREEN_ITEM_LIST.DATA+$20,X ;+$20 = record size
		
	DEX ;next record byte
	DEY ;loop counter
	BNE .LOOP.LIST_DATA.SCROLL_DOWN
	
	LDY #INV_4.1a.SCREEN_ITEM_LIST.DATA.RECORD_SIZE ;init loop counter = record size
	CPX #$FF
	BNE .LOOP.LIST_DATA.SCROLL_DOWN
	
	

			
			

			
; ;=====DEBUG HOOK======
		; STX TEMP
		
		; LDX #$00
; .DEBUG.COPY2
		; LDA INV_4.1b.SCREEN_ITEM_LIST.ARRAY,X
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
		; LDA INV_4.1a.SCREEN_ITEM_LIST.DATA,X
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


INV_4.1b.DISPLAY_PLAYER_INVENTORY.SCROLL_UP
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


;

;SAVE REGISTERS	
	TXA
	PHA
	TYA
	PHA

.INIT.SCREEN					;setup screen byte, line, depth	

	
	; LDA #$B8
	; STA TWF.SCROLLING.STOP_LINE				;set the stop line, which is the last line of the last row of text characters to scroll +$08. 

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
	LDY DRAW.START_BYTE					;reset screen byte index to the left edge of text window	
	
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
	
	INY							;advance to next screen byte in current hi-res line

	CPY DRAW.STOP_BYTE.ALT		;is line complete? (i.e. have all screen bytes in the line been scrolled?)
	BNE .COPYLOOP				;if no, then continue loop
	
	;**FALLS THROUGH**			;if yes, then advance to next line within current item's text characters

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

INV_4.1b.DISPLAY_PLAYER_INVENTORY.SCROLL_DOWN ;supports player and merchant inventory windows
@START
;PARAMETER: DRAW.START_BYTE, DRAW.STOP_BYTE, TWF.SCROLLING.STOP_LINE
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
	; JSR INV_4.1a.SYNC_PAGES
			
			
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


	LDA LINE.START.COPY_FROM
	CLC
	ADC #$18 ;top line of the first row of text characters of the last item
	STA LINE.START.COPY_TO
	CLC
	ADC #$10 ;bottom line+1 of the second row of text characters of the last item	
	STA LINE.STOP
	
.LINELOOP	
	LDY DRAW.START_BYTE					;reset screen byte index to the left edge of text window	
	
			
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
	
	INY							;advance to next screen byte in current hi-res line

	CPY DRAW.STOP_BYTE.ALT		;is line complete? (i.e. have all screen bytes in the line been scrolled?)
	BNE .COPYLOOP				;if no, then continue loop
	
	;**FALLS THROUGH**			;if yes, then advance to next line within current item's text characters

.NEXT.ITEM.LINE

	INC LINE.START.COPY_FROM
	INC LINE.START.COPY_TO
	LDA LINE.START.COPY_TO
	CMP LINE.STOP
	BCC .LINELOOP

	;**FALLS THROUGH**			;if no, then increment scroll postion for next item

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
INV_4.1b.ADDITIONAL_INFO
@START
;PARAMETERS: INV.ACTIVE_MENU.CODE
;ENTRANCE: direct from any INV_4 routine
;RETURN: updated video screen (scroll window)

;NOTE: prints additional information on the selected item (from master item table) into the scroll text window in the lower right part of the screen. 


;VALIDATE ENTRANCE
	JSR INV_4.1b.GET.SCREEN_LIST.ARRAY.INDEX
		;X-REG (index to SCREEN_ITEM_LIST.ARRAY for the item selected on screen)

	LDA INV_4.1b.SCREEN_ITEM_LIST.ARRAY+$3,X ;load (readied status | delete status flag). deleted record = $FF
	BPL .VALIDATE.ENTRANCE.DONE
	JMP .EXIT.ALT
.VALIDATE.ENTRANCE.DONE

		
		
.INIT.CURSOR

	JSR INV_4.1a.RESTORE.TW.CURSOR
	
	
	
						;JSR KEYIN

;**OPT** Memory. Convert to SCREEN_BUILDER. 
;There are some corner cases.
;indirect variable lookups like this: 		LDA INV_4.1a.SCREEN_ITEM_LIST.DATA+INV.ITEM_TABLE.ALL.STR_WEIGHT.OFFSET,Y ;load weight for selected item
;will need to be lookedup before calling screen builder and saved to a new variable (e.g. SCREEN_BUILDER.CUSTOMIZED.LOOKUP_TABLE.INDEX1)
;and then the screen builder command table will store the memory address of the new variable
;
;Some sections like .HANDS_REQUIRED print text conditioned on the high-bit status of a variable.
;A new ASSOC code and subroutine will need to be setup for this. It's a simular concept to 
;.ASCII_SOURCE.CORNER_CASE.BOOLEAN_FLAG. The main difference is that this routine makes it's decision
;based on =0/!=0 instead of high-bit status. 
;
	
.INIT.HEADER_TEXT
	JSR PRINT.TEXT.WINDOW.CR
		LDA #$AD ;ASCII = "-"
	JSR PRINT.TEXT.WINDOW.SINGLE.CHAR

	
							;JSR KEYIN

	
	
		; LDA #$AD ;ASCII = "-"
	; JSR PRINT.TEXT.WINDOW.SINGLE.CHAR 			;**OPT** Memory. We could probably make do with a one dash header instead of two dashes.

	JSR PRINT.TEXT.WINDOW.CR

	
			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; lda #$aa]
			; jsr keyin
				; LDA #$CD ;ASCII = "M"
			; JSR PRINT.TEXT.WINDOW.SINGLE.CHAR
			; jsr keyin
			; lda #$aa
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP
			
			
			
			
.INIT.INDEX	
	;Calculate index to INV_4.1a.SCREEN_ITEM_LIST.DATA for selected item
		;ACC parm: preset to INV_4.1b.SCREEN_ITEM_LIST.SELECTED.ITEM  ;load selected item number
	JSR INV_4.1a.GET_INDEX.SCREEN_ITEM_LIST.DATA.ALT_ENTRANCE	;set INV_4.1a.SCREEN_ITEM_LIST.DATA index ($20 byte records)
		;RETURN: X-REG = INV_4.1a.SCREEN_ITEM_LIST.DATA index
		
	TXA
	TAY ;transfer index to Y-REG


	
.PRINT_COMMMON1
@START
.PRINT.WEIGHT ;PRINT ITEM WEIGHT
;PRINT LABEL	
		LDA #INV_4.SCROLL_WINDOW.ALL.WEIGHT
		STA TWF.STRING+$0
		
		LDA /INV_4.SCROLL_WINDOW.ALL.WEIGHT
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW
	
	
							;JSR KEYIN

							

;PRINT VALUE

		LDA INV_4.1a.SCREEN_ITEM_LIST.DATA+INV.ITEM_TABLE.ALL.STR_WEIGHT.OFFSET,Y ;load weight for selected item
		; CLC ;(CLC = don't print CR | SEC = print CR to text window)
	JSR PRINT.HEX8.RJ

	
						;	JSR KEYIN


@END

	;**FALLS THROUGH**
	
	
.PRINT.MENU.SPECIFIC
;BRANCH	
	LDA INV.ACTIVE_MENU.CODE 
	CMP #INV.ACTIVE_MENU.SPELLS ;is spell menu active?
	BEQ .PRINT_INFO.SPELLS_STEP
	LDA INV_4.1b.DISPLAY.MODE ;($00 = display weapons | $01 = display armor | $02 = misc items)
	BNE .PRINT_INFO.ARMOR_MISC

	;**FALLS THROUGH**
	
.PRINT_INFO.WEAPON
@START

.BASE_DMG
;PRINT LABEL	
		LDA #INV_4.1b.SCROLL_WINDOW.WP.BASE_DMG				
		STA TWF.STRING
		
		LDA /INV_4.1b.SCROLL_WINDOW.WP.BASE_DMG
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW

;PRINT VALUE

		;load base damage for selected item		
		LDA INV_4.1a.SCREEN_ITEM_LIST.DATA+INV.ITEM_TABLE.WP.DMG_PWR.OFFSET,Y ;load based damage for selected item
		; CLC ;(CLC = don't print CR | SEC = print CR to text window)
	JSR PRINT.HEX8.RJ


.HANDS_REQUIRED
		LDA INV_4.1a.SCREEN_ITEM_LIST.DATA+INV.ITEM_TABLE.WP.SHAPE_ID_HANDS.OFFSET,Y ;load hands required for selected item (high-bit set = 2 handed | high-bit not set = 1 handed)
		BMI	.2_HANDED_WEAPON ;branch if high-bit set

.1_HANDED_WEAPON	
;PRINT LABEL	
		LDA #INV_4.1b.SCROLL_WINDOW.WP.1_HANDED				
		STA TWF.STRING
		
		LDA /INV_4.1b.SCROLL_WINDOW.WP.1_HANDED
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW
	
	JMP .HANDS_REQUIRED.DONE

.2_HANDED_WEAPON
		LDA #INV_4.1b.SCROLL_WINDOW.WP.2_HANDED				
		STA TWF.STRING
		
		LDA /INV_4.1b.SCROLL_WINDOW.WP.2_HANDED
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW
.HANDS_REQUIRED.DONE
	
.MAGIC.WEAPON

	JSR PRINT.TEXT.WINDOW.CR

	LDA INV_4.1a.SCREEN_ITEM_LIST.DATA+INV.ITEM_TABLE.WP.MAGIC_FLAG.OFFSET,Y ;load magic weapon flag for selected item (high-bit not set = no magic weapon | high-bit set = magic weapon)
	BMI	.IS_MAGIC ;branch if high-bit set

		
.ISNT_MAGIC	
;PRINT LABEL	
		LDA #INV_4.1b.SCROLL_WINDOW.WP.ISNT_MAGIC				
		STA TWF.STRING
		
		LDA /INV_4.1b.SCROLL_WINDOW.WP.ISNT_MAGIC
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW
	
	JMP .MAGIC.WEAPON.DONE

.IS_MAGIC
		LDA #INV_4.1b.SCROLL_WINDOW.WP.IS_MAGIC				
		STA TWF.STRING
		
		LDA /INV_4.1b.SCROLL_WINDOW.WP.IS_MAGIC
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW
.MAGIC.WEAPON.DONE


	JMP .PRINT_COMMMON2

.PRINT_INFO.SPELLS_STEP
	JMP .PRINT_INFO.SPELLS
	
@END


.PRINT_INFO.ARMOR_MISC
@START

.ARMOR_RATING
;PRINT LABEL	
		LDA #INV_4.1b.SCROLL_WINDOW.AR_MISC.ARMOR_RATING				
		STA TWF.STRING
		
		LDA /INV_4.1b.SCROLL_WINDOW.AR_MISC.ARMOR_RATING
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW

;PRINT VALUE
		LDA INV_4.1a.SCREEN_ITEM_LIST.DATA+INV.ITEM_TABLE.AR_MISC.DEFENSE_RATING.OFFSET,Y ;load based damage for selected item
		; CLC ;(CLC = don't print CR | SEC = print CR to text window)
	JSR PRINT.HEX8.RJ
	

.RESIST_MAGIC
;PRINT LABEL	
		LDA #INV_4.1b.SCROLL_WINDOW.AR_MISC.RESIST_MAGIC				
		STA TWF.STRING
		
		LDA /INV_4.1b.SCROLL_WINDOW.AR_MISC.RESIST_MAGIC
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW

;PRINT VALUE
		LDA INV_4.1a.SCREEN_ITEM_LIST.DATA+INV.ITEM_TABLE.AR_MISC.MGK.RESIST.OFFSET,Y ;load based damage for selected item
		; CLC ;(CLC = don't print CR | SEC = print CR to text window)
	JSR PRINT.HEX8.RJ
	
	
		JSR PRINT.TEXT.WINDOW.CR
	
	JMP .PRINT_COMMMON2
	
@END

.PRINT_INFO.SPELLS
@START

.LEVEL
;PRINT LABEL	
		LDA #INV_4.1b.SCROLL_WINDOW.SP.LEVEL				
		STA TWF.STRING
		
		LDA /INV_4.1b.SCROLL_WINDOW.SP.LEVEL
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW

;PRINT VALUE


		LDA INV_4.1a.SCREEN_ITEM_LIST.DATA+INV.ITEM_TABLE.AR_MISC.LEVEL.OFFSET,Y ;load level field for selected item
	JSR PRINT.HEX8.RJ
	
	
; .SPELL_DMG X2
; ;PRINT LABEL	
		; LDA #INV_4.1b.SCROLL_WINDOW.SP.BASE_DMG			
		; STA TWF.STRING
		
		; LDA /INV_4.1b.SCROLL_WINDOW.SP.BASE_DMG
		; STA TWF.STRING+$1						
	; JSR PRINT.TEXT.WINDOW

; ;PRINT VALUE
		
		; ;load spell damage for selected item		
		; LDA INV_4.1a.SCREEN_ITEM_LIST.DATA+INV.ITEM_TABLE.MI.SPELL_DMG_LO.OFFSET,Y ;load LO Byte
		; STA BIN+$0
		; LDA INV_4.1a.SCREEN_ITEM_LIST.DATA+INV.ITEM_TABLE.MI.SPELL_DMG_HO.OFFSET,Y ;load LO Byte
		; STA BIN+$1
		; CLC ;(SEC = print CR to text window | CLC don't print CR)		
	; JSR PRINT.HEX16.RJ


.SPELL_DMG ;not X2
;PRINT LABEL	
		LDA #INV_4.1b.SCROLL_WINDOW.SP.BASE_DMG			
		STA TWF.STRING
		
		LDA /INV_4.1b.SCROLL_WINDOW.SP.BASE_DMG
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW

;PRINT VALUE
		
		;load spell damage for selected item		
		LDA INV_4.1a.SCREEN_ITEM_LIST.DATA+INV.ITEM_TABLE.MI.SPELL_DMG.OFFSET,Y ;load LO Byte
	JSR PRINT.HEX8.RJ
	

	
	
			; LDA INV_4.1a.SCREEN_ITEM_LIST.DATA+INV.ITEM_TABLE.MI.OTHER_INFO.OFFSET ;(high-bit set = AOE)
			; STA $BF00
			; LDA #INV_4.1a.SCREEN_ITEM_LIST.DATA
			; STA $BF01
			; LDA /INV_4.1a.SCREEN_ITEM_LIST.DATA
			; STA $BF02
			; LDA #INV.ITEM_TABLE.MI.OTHER_INFO.OFFSET ;(high-bit set = AOE)
			; STA $BF03
			
			; LDA #$AA
			; JSR PREP.BRK
			; BRK
			
	
	
.SPELL_AOE ;(area-of-effect)	
;(print area-of-effect to text window if AOE flag is set)

	LDA INV_4.1a.SCREEN_ITEM_LIST.DATA+INV.ITEM_TABLE.MI.OTHER_INFO.OFFSET,Y ;(high-bit set = AOE)
	BPL .SPELL_AOE.NO

	;PRINT: area-of-effect 
		LDA #INV_4.1b.SCROLL_WINDOW.SP.AOE			
		STA TWF.STRING
		
		LDA /INV_4.1b.SCROLL_WINDOW.SP.AOE
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW
	JMP .SPELL_AOE.DONE

.SPELL_AOE.NO	;print blank line if spell isn't AOE to take the place of the AOE text, keeping the entire spell info block equal to the size of the scroll window, which looks good. 
	JSR PRINT.TEXT.WINDOW.CR


.SPELL_AOE.DONE	
				; LDA INV_4.1a.SCREEN_ITEM_LIST.DATA+INV.ITEM_TABLE.MI.OTHER_INFO ;(high-bit set = AOE)
				; STA $B
				; STY $BF00
				
				; LDA #$AA
				; LDX #INV_4.1a.SCREEN_ITEM_LIST.DATA
				; LDY /INV_4.1a.SCREEN_ITEM_LIST.DATA
				; JSR PREP.BRK
				; BRK
				
	;**FALLS THROUGH**

	
@END


.PRINT_COMMMON2

	JSR PRINT.TEXT.WINDOW.CR

	;**FALLS THROUGH**


.EXIT
		;JSR PRINT.TEXT.WINDOW.CR

		JSR INV_4.1a.SAVE.TW.CURSOR

.EXIT.ALT

	RTS ;returns to INV_4.COMMAND.INFO (JMPs to INV_4.STATE.LOOP.ALT.ENTRANCE) or  INV_8.COMMAND.INFO (JMPs to INV_8.STATE.LOOP.ALT.ENTRANCE)

@END



;GRAPHICS


@END


;***these will be needed by INV_4 in regular interface mode

;INV_4.1b LOCAL TEXT BLOCKS (also see INV_4 LOCAL TEXT BLOCKS)
@START
;headings
INV_4.TEXT_BLOCK.HEADER1			.AZ -/ITEM        WT QTY/

;misc: other
INV_4.TEXT_BLOCK.SPELL_READIED_INDICATOR	.AZ -/*(x)/
INV_4.TEXT_BLOCKS.NO_ITEMS					.AZ -/No items owned/


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

INV_4.1b.SCROLL_WINDOW.WP.BASE_DMG		.AZ -/Base DMG  /
INV_4.1b.SCROLL_WINDOW.WP.1_HANDED		.AZ -/1-Handed/
INV_4.1b.SCROLL_WINDOW.WP.2_HANDED		.AZ -/2-Handed/

INV_4.1b.SCROLL_WINDOW.WP.ISNT_MAGIC	.AZ	-/Non-magic item/
INV_4.1b.SCROLL_WINDOW.WP.IS_MAGIC		.AZ	-/Magic item/


INV_4.1b.SCROLL_WINDOW.AR_MISC.ARMOR_RATING	.AZ	-/Armor RT  /
INV_4.1b.SCROLL_WINDOW.AR_MISC.RESIST_MAGIC	.AZ	-/Resist Mgk/

INV_4.1b.SCROLL_WINDOW.SP.LEVEL		.AZ -/Level     /			
INV_4.1b.SCROLL_WINDOW.SP.BASE_DMG		.AZ -/Spell DMG /
INV_4.1b.SCROLL_WINDOW.SP.AOE			.AZ -/Area-of-effect/

INV_4.SCROLL_WINDOW.ALL.WEIGHT		.AZ -/Weight    /

@END




