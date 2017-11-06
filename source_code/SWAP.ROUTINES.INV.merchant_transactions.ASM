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

INV_8.MERCHANT_TRANSACTIONS
;=====================SUB_MODULE DOCUMENTATION===========================================================================
;
;
;---Overview---
;
;The player inventory window displays items differently when the merchant transactions interface is active.
;
;Note: for both interfaces, the QTY displayed is (total QTY - # of units of the item readied by all PCs)
;
;main interface active: 
;	a) the readied indicator (*) is only displayed when the active player has the item readied.*
;	b) items with no units available to be readied (I.E. QTY = 0) are NOT displayed*
;
;*Spells Menu: exception. the QTY is the total QTY, no deduction for readied items.
;
;merchant transactions interface active: 
;	a) the readied indicator (*) is displayed for each PC who has the item readied. 
;	b) items with no units available to be readied (I.E. QTY = 0) ARE displayed, because those item could be unreadied and sold. 
;		Thus, QTY is the number of "extra" units the party has, which aren't readied by anyone.*
;
;*Spells Menu: exception. the QTY is the total QTY -1 if the spell book is readied. 
;
;-Overview: technical notes
;Implementing the above required no changes to the calculation of QTY because the formula is the same.
; 
;An INV_8 subroutine was created to display the readied indicator differently (INV_8.DISPLAY_READIED.INDICATOR)
;
;In INV_4's init section, .VALIDATE.ITEM (.INIT.IDENTIFY_ITEMS) detects when merchant transactions interface mode is active
;and skips the checks which restrict which items are displayed on screen in main interface mode. In merchant transactions interface mode
;all items are displayed in the players inventory which match the display mode set (i.e weapons, armo, misc_items etc)
;
;In merchant mode, .PRINT.ITEM.QTY calls INV_8.SPELL_MENU.ALT_QTY_VALUE so that QTY -1 is displayed if the spell
;book is readied. The QTY value doesn't reflect a readied spell book because the readied flags on the player inventory record
;are not used to track the readied status. This is because spell books are readied at a party level, not at the PC level. 
;
;Merchant interface mode locally manages part of the scrolling of merchant inventory items. This is due in part
;because the merchant inventory items are stored in the merchant profile array (INV_8.MERCHANT.PROFILE.DATA), which has a different format than
;the player inventory array (PLAYER_MERCH.INVENTORY.DATA). To trace the path, start in the INV_8 state loop. The calls will
;bounce between local routines and INV_4.1 routines, which are compatible with merchant interface mode.
;
; 
;---Deeper Technical Notes---
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
;
;
;-Spell Items: readied indicator and quantity
;This is a cluge.
;See "-Spell Items: readied indicator and quantity" in INV_4.1a.VIDEO_OUT.ITEM_LINE1_2 for detailed documentation. 
;
;
;
;------------------TERMINOLOGY---------
;Player Inventory Window 	= the window on the left side of the screen. 
;Merchant Inventory Window 	= the window on the upper right side of the screen. 
;Scroll Text Window 		= the window on the lower right side of the screen. 
;
;-----------------------MODES-------------------
;There are 3 modes that INV_4 operates in when the merchant transactions sub_module is loaded
;
;---INV_4- Player Inventory (merchant trans init mode)---  
;Flag Variable: INV.SUB_MODULE.LOAD_FLAGS+$8 ;($00 = sub-module not loaded in memory | $01 = sub-module loaded in memory | high-bit set = initial launch complete)
;This mode is active when the flag is != $00 and the high-bit is set
;***not currently in use as of when I stopped displaying player inventory immediately on launch to 
;avoid the need to update the display when the player buys an item. 
;
;The mode is used during the merchant transactions module (INV_8) when it calls INV_4 to display player inventory data
;in the player inventory window (left side) for the first time.
;
;---INV_4- Player Inventory (merchant trans non-init mode)---
;Flag variable: INV.SUB_MODULE.LOAD_FLAGS+$8 ;($00 = sub-module not loaded in memory | $01 = sub-module loaded in memory | high-bit set = initial launch complete)
;This mode is active when the high-bit is set
;
;This mode is used when the player uses tab or the hotkeys (2-5) to access the player inventory menus (weapons, armor, misc items, spells)
;
;---INV_4- Called from INV_8- Merchant transactions---
;Flag variable: INV_4.1b.CALL_FROM	;($00 = subroutine called from INV_4, display player inventory | $01 = subroutine called from INV_8 merchant transactions)
;This mode is active when the flag != $00 
;
;This mode is used when the merchant transactions module makes calls a subroutine in INV_4, which does some things differently 
;when this mode is active. This way the merchant and player inventory windows can share a lot of the same code for things like printing item information to the screen. 
;
;
;
;
;--------------------MISC NOTES------
;*I started renaming all subroutines and variables used by both INV_4 and INV_8 to INV_4.1 but eventually stopped because
;		almost all are shared. The notable exceptions are the routines related to ready/unready and the few entrance routines
;		I added to set parameters INV_4 only parameters for shared subroutines. For the latter, I added a comment on the same line as the entrance routine label. 
;
;
;=============================================================================================================================


	;**OPT** Memory. If I'm really desperate for memory, consider locating some code or variables in the memory addressed used by INV_4 which aren't needed
					;in merchant interface mode, such as INV_4_GROUP2.READY_UNREADY.REQUEST. It may require reordering the INV_4 subroutines...like putting the non-shared ones at the end of the file or at least in a contiguous block


	; LDA #$AB
	; JSR PREP.BRK
	; BRK
	
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


;**OPT** Memory. I wonder if making spellbooks item_type = $03 would make sense. There is a whole bunch 
;of special code in INV_4 and INV_8 to identfy spell books when the spell menu is active. I really can't think
;of a benifit to having spell books lumped in with misc items. I may have done that originally because I was
;contemplating that the entire 256 record table for each item type would be used. However, once I decided that
;the player inventory would be limited to 256 record I effectively decided that would not be the case, and I 
;I don't it matters if the 256 items in the game are spread accross item_type 0-3 instead of 0-2. 


	
.INIT
@START


		

.INIT.GENERAL
@START
	LDA #INV_8.ACTIVE_MENU.CODE.MAX			;overrides default because when merchant transactions is active, there is an additional menu icon for the merchant inventory window. 
	STA INV.ACTIVE_MENU.CODE.MAX.VARIABLE	;tracks the code of the menu on the far right, so it can wrap around to the 1st on the left. 

	
@END
			
			
			
.INIT.INITAL_LAUNCH_ONLY
@START
;VALIDATE ENTRANCE
	LDA INV.SUB_MODULE.LOAD_FLAGS+$8 ;($00 = sub-module not loaded in memory | $01 = sub-module loaded in memory | high-bit set = initial launch complete)
	BPL .INIT.INITAL_LAUNCH_ONLY.START ;branch if initial launch NOT complete
	JMP INV_8.INIT.INITAL_LAUNCH_ONLY.DONE 

.INIT.INITAL_LAUNCH_ONLY.START
;INIT MERCHANT MENU
	LDA #$06 ;set active menu: merchant inventory window
	STA INV.ACTIVE_MENU.CODE

.INIT.INV_4.READ_SIZE
;(self-modifying code. See INV.SUB_MODULE.FILE.PARAMETERS.INV_4 for documentation on what this does) 
	LDA #INV_4.DISPLAY_PLAYER_INVENTORY.READ_BYTES.MERCH_MODE
	STA INV.SUB_MODULE.FILE.PARAMETERS.INV_4+$02
	LDA /INV_4.DISPLAY_PLAYER_INVENTORY.READ_BYTES.MERCH_MODE
	STA INV.SUB_MODULE.FILE.PARAMETERS.INV_4+$03

					
.INIT_SCREEN
@START

.INIT.DRAW_WINDOWS
	;draw merchant transaction window (right)
		;set parameters of INIT.TEXT_WINDOW.RIGHT
		LDA #TWB.LW.MERCH_INV.LEFT_SBYTE
		STA DRAW.START_BYTE
		LDA #$01 ;($00 = draw input window line | >=$01 = don't draw input window line)
	JSR INIT.TEXT_WINDOW.RIGHT 
	
	
	;draw player inventory window (left)
	JSR INV.DRAW_ERASE.INVENTORY_WINDOW
	
	
	;draw merchant menu icon
		LDA #INV.MENU_ICON7.SBYTE
		STA SCREEN.DRAW.CURRENT_BYTE
		
		LDA	#MENU_ICON7.MERCHANT_INV
		STA SHAPE+$0
		LDA /MENU_ICON7.MERCHANT_INV
		STA SHAPE+$1
	JSR INV.DRAW.MENU.ICON

	
	
;DRAW LINE: BELOW ICONS ;**OPT** Memory. Could be merged with the "DRAW LINE: BELOW ICONS" code section in INV.DRAW_ERASE.INVENTORY_WINDOW
@START

	LDA #TWB.LW.INVENTORY.SEPERATOR_LINE							;load line in x register	
	STA DRAW.START_LINE
	
	;LDA #TWB.LW.INVENTORY.LEFT_SBYTE+1
	LDA #TWB.LW.MERCH_INV.LEFT_SBYTE
	STA DRAW.START_BYTE
	
	;LDA #TWB.LW.INVENTORY.RIGHT_SBYTE-1
	LDA #TWB.LW.MERCH_INV.RIGHT_SBYTE
	STA DRAW.STOP_BYTE
		
	LDA #TWB.LW.MERCH_INV.SEPERATOR_LINE+1 ;(set to last line to draw + 1)
	STA DRAW.STOP_LINE
	
	;LDA #$AA
	LDA #$D5
	STA DRAW.BYTE_VALUE.HORIZONTAL+$0
	
	;LDA #$D5
	LDA #$AA
	STA DRAW.BYTE_VALUE.HORIZONTAL+$1
	
.DRAW.LINE
;(when both hi-res pages need to stay in sync, instead of drawing to both pages, sometimes it is best to the pages individually
;and flip pages inbetween. This way the draw isn't as noticable to the user)
		
					; LDA #$01
					; STA TROUBLESHOOTING.HOOK
					
		LDA #$07 ;BIT0-2 set: use both hi-res pages. draw top line of rectangle. ignore all other bit flags.
	JSR DRAW.LINE



@END

	
.INIT.MENU.SELECTOR ;DRAW MENU SELECTOR ON MENU ICON #1
;note: when main inventory interface mode is active the menu selector is init in the stats module, which
;isn't in memory when this routine is running, hence the need for it's own init routine. 
@START
	LDA #$84 ;set bit map shape value of 1st pillar
	STA DRAW.BYTE_VALUE.VERTICLE
	
	LDY #$1C ;set screen byte of pillar1
	LDX #TWB.LW.INVENTORY.TOP_LINE+1 ;set starting hi-res line
.LOOP.DRAW_ERASE
		LDA PAGE.BACKGROUND
	JSR GET.LINE.ADDRESS1

		LDA PAGE.FOREGROUND
	JSR GET.LINE.ADDRESS2

	LDA DRAW.BYTE_VALUE.VERTICLE	;bit mapped byte
	STA (LINE.BASE.ADDR1),Y			;save bit mapped byte to video screen memory (foreground page)  
	STA (LINE.BASE.ADDR2),Y			;save bit mapped byte to video screen memory (background page)

	INX ;next line (down)
	
	CPX #TWB.LW.INVENTORY.SEPERATOR_LINE
	BNE .LOOP.DRAW_ERASE
	CPY #$1C 
	BNE .LOOP.DRAW_ERASE.DONE
	LDA #$88 ;set bit map shape value of 2nd pillar
	STA DRAW.BYTE_VALUE.VERTICLE
	
	LDY #$1F ;set screen byte of pillar2
	LDX #TWB.LW.INVENTORY.TOP_LINE+1 ;reset starting hi-res line
	JMP .LOOP.DRAW_ERASE
.LOOP.DRAW_ERASE.DONE	
	
@END
	
@END

					
.INIT.DISPLAY.PLAYER.INVENTORY
@START	
	; ;display player inventory data
		; ; LDA #$00
		; ; STA INV_8_4.PLAYER_INV.REFRESH_FLAG	;($00 = refresh mode OFF | >=$01 = refresh mode on). Used when INV_8 wants INV_4 to refresh the player inventory window, without switching menus, after the initial launch. i.e. after an item is purchased. 
		; ;ACC =$00
		; STA INV_4.1b.DISPLAY.MODE
		; LDA #$01 
		; STA INV_4.ACTIVE_PLAYER	
		; LDA #$04	;inventory sub-module # to launch)
	; JMP INV.READ_LAUNCH.SUB_MODULE	
INV_8.INIT_RTS_FROM.INV_4	
	; ;****OPEN QUESTION: should .CHECK.DUAL_WIELDING.STATUS be skipped in INV_4 when MT trans is active? (post init, becuase INV_4 will be called when the player switches from the merchant window to the inventory window, and I think the INV_4 state loop would get used)
	; ;Update 8/31/2017; merchant trans interface is almost done and the above doesn't seem to have been hurting anything

	
	

@END

.INIT.READ.SHARED_ROUTINES ;read from disk the INV_4.1 subroutines, which are shared between INV_4 and INV_8
@START

		;set destination memory address
		lda #INV_4.1a.ROUTINES
		sta parm.ldrlo
		lda /INV_4.1a.ROUTINES
		sta parm.ldrhi
		
		lda #INV_4.1a.SHARED_ROUTINES.SEEK_BYTES+$0
		sta PARM.SEEK_BYTES+$0	;seek length (LO byte).
		lda /INV_4.1a.SHARED_ROUTINES.SEEK_BYTES+$1
		sta PARM.SEEK_BYTES+$1	;seek length (HO byte).

		
		LDA #INV_4.1a.SHARED_ROUTINES.READ_BYTES
		sta PARM.READ_BYTES+$0		;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
		LDA /INV_4.1a.SHARED_ROUTINES.READ_BYTES
		sta PARM.READ_BYTES+$1		;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	JSR PRODOS.IO.READ_CURRENT_FILE



	
@END


.INIT.READ.MASTER_PRICE_DATA
@START


		;set destination memory address
		lda #INV_8.1.MASTER_PRICE_TABLE.DATA
		sta parm.ldrlo
		lda /INV_8.1.MASTER_PRICE_TABLE.DATA
		sta parm.ldrhi
		
		lda #INV_8.1.DATA_STORAGE.SEEK_BYTES+$0
		sta PARM.SEEK_BYTES+$0	;seek length (LO byte).
		lda /INV_8.1.DATA_STORAGE.SEEK_BYTES+$1
		sta PARM.SEEK_BYTES+$1	;seek length (HO byte).

		
		LDA #INV_8.1.DATA_STORAGE.READ_BYTES
		sta PARM.READ_BYTES+$0		;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
		LDA /INV_8.1.DATA_STORAGE.READ_BYTES
		sta PARM.READ_BYTES+$1		;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	JSR PRODOS.IO.READ_CURRENT_FILE
	
@END

.INIT.READ.MERCHANT_PROFILE_DATA
@START
;=====================CODE-SECTION DOCUMENTATION==========================================================================
;
;Merchant ID refers to a record number in the INV.DATA.MERCHANT_PROFILE table. The first byte of each record contains the
;the number of bytes in that record. 
;
;The loop in this routine reads the record size byte, and if the current record is the record associated with the merchant the 
;player is talking to (= INV_8.MERCHANT_PROFILE_ID) then the file offset is in the correc position and a read from the file
;is done equal to the record size byte-1 (-1 because the record size byte was already read). If the current record is not the 
;required record then a seek is done equal to the record size byte-1 and the process is repeated. 
;
;
;===========================================================================================================================



; ;DRIVER 
; ;===============================

	;*****driver is setup in COMMAND.TALK
				
				
				;!!!!OLD!!!!!
				; LDA #$03
				; STA PLAYER.MAP.CODE
				; LDA #$08
				; STA INV_8.NPC_RECORD
; ;=============================

;.GET.MERCHANT_ID ;**deprecated. Merchant_ID is now detected during the seek process below. 
@START
; ;READ MERCHANT_ID TABLE

		; ;set destination memory address
		; lda #INV_8.MERCHANT_ID.TABLE		;***shares memory with INV_8.MERCHANT.PROFILE.DATA
		; sta parm.ldrlo
		; lda /INV_8.MERCHANT_ID.TABLE
		; sta parm.ldrhi
		
		; lda #INV_8.MERCHANT_ID_TABLE.SEEK_BYTES+$0
		; sta PARM.SEEK_BYTES+$0	;seek length (LO byte).
		; lda /INV_8.MERCHANT_ID_TABLE.SEEK_BYTES+$1
		; sta PARM.SEEK_BYTES+$1	;seek length (HO byte).

		
		; LDA #INV_8.MERCHANT_ID_TABLE.READ_BYTES
		; sta PARM.READ_BYTES+$0		;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
		; LDA /INV_8.MERCHANT_ID_TABLE.READ_BYTES
		; sta PARM.READ_BYTES+$1		;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	; JSR PRODOS.IO.READ_CURRENT_FILE

; ;SEARCH FOR NPC RECORD # IN MERCHANT_ID TABLE
; ;(the merchant_ID tables' records are listed in the sequential order of merchant_IDs. So the first record in the table is
; ;for merchant_ID $00, the 2nd record in the table is for merchant_ID $01, etc.)
 
	; LDX #$00 ;INV_8.MERCHANT_ID.TABLE index
	; LDY #$00 ;init loop counter
; .LOOP.SEARCH.MERCHANT_ID
	; ;check map code
	; LDA INV_8.MERCHANT_ID.TABLE+$0,X ;load map code field of current merchant_ID table record
	; CMP PLAYER.MAP.CODE ;does the map code of the player's current map match the map code field of the current merchant_ID table record?
	; BNE .NEXT_RECORD ;branch if no
	; ;check NPC record #
	; LDA INV_8.MERCHANT_ID.TABLE+$1,X ;load NPC Record # field of current merchant_ID table record
	; CMP INV_8.NPC_RECORD
	; ; LSR ;/2  ;convert NPC record # to a sequential #. i.e. NPC Record $00 = $00, Record # $08 = $01, Record # $10 = $02, etc. 
	; ; LSR ;/4
	; ; LSR ;/8
	; ; STA TEMP
	; ; CPY TEMP ;does the NPC Record # (converted to a sequential number) of the merchant match the merchant_ID for the current merchant_ID table record? 
	; BEQ .MERCHANT_ID.FOUND
	
; .NEXT_RECORD
	; INY ;increment loop counter
	; INX ;increment INV_8.MERCHANT_ID.TABLE index
	; INX ;record is $2 bytes
	; ;ACC = INV_8.MERCHANT_ID.TABLE+$1,X (NPC Record # field)
	; CMP #$FF ;is stop value found?
	; BNE .LOOP.SEARCH.MERCHANT_ID
	
	; ;** FALLS THROUGH**
	
; .ERROR_TRAP.MERCHANT_ID.NOT_FOUND
; ;.GET.MERCHANT_ID (INV_8.MERCHANT_TRANSACTIONS) reports that the NPC record # and Map code
; ;of the merchant the player is talking to is not found in the merchant_ID table. 
	; JSR PREP.BRK
	; BRK
	
; .MERCHANT_ID.FOUND
	; ;save merchant_ID
	; STY INV_8.MERCHANT_PROFILE_ID ;the value of the loop counter is the merchant_ID
	
	
@END


.SEEK.MERCHANT_PROFILE
@START

	;SEARCH LOOP INIT
	LDX #$00 ;merchant profile ID counter
	
	lda #INV.DATA.MERCHANT_PROFILE.OFFSET 
	sta INV_8.MERCHANT_PROFILE.SEEK_BYTES+$0	;seek length (LO byte).
	lda /INV.DATA.MERCHANT_PROFILE.OFFSET 
	sta INV_8.MERCHANT_PROFILE.SEEK_BYTES+$1	;seek length (HO byte).	
	
	JSR PRODOS.IO.RESET.FILE_OFFSET ;resets file offset so that seeks start at beginning of the file

INV_8.INIT.MERCHANT_PROFILE.SEARCH.LOOP
	
		;set destination memory address
		lda #INV_8.MERCHANT.PROFILE.RECORD_SIZE
		sta parm.ldrlo
		lda /INV_8.MERCHANT.PROFILE.RECORD_SIZE
		sta parm.ldrhi
		
		lda INV_8.MERCHANT_PROFILE.SEEK_BYTES+$0
		sta PARM.SEEK_BYTES+$0	;seek length (LO byte).
		lda INV_8.MERCHANT_PROFILE.SEEK_BYTES+$1
		sta PARM.SEEK_BYTES+$1	;seek length (HO byte).

		
		LDA #$03 ;read bytes $0-$2 of the merchant profile record
		sta PARM.READ_BYTES+$0		;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
		LDA #$00
		sta PARM.READ_BYTES+$1		;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	JSR PRODOS.IO.READ_CURRENT_FILE.NO_RESET


				
	;exit test
	;check map code
	LDA INV_8.MERCHANT.PROFILE.MAP_CODE	 ;load map code field of current merchant profile record
	CMP PLAYER.MAP.CODE ;does the map code of the player's current map match the map code field of the current merchant_ID table record?
	BNE .NEXT_RECORD ;branch if no
	;check NPC record #
	LDA INV_8.MERCHANT.PROFILE.NPC_RECORD ;load NPC Record # field of current merchant profile record
	CMP INV_8.NPC_RECORD
	BEQ .MERCHANT_PROFILE.FOUND

.NEXT_RECORD	
	;increment counter
	INX ;merchant profile ID counter
	BEQ .ERROR.MERCHANT.NOT.FOUND
	
	;seek to start of next merchant profile record
	LDA INV_8.MERCHANT.PROFILE.RECORD_SIZE ;load 8-bit size (bytes) of current profile record
	SEC
	SBC #$03 ;subract $3 bytes because 3 bytes were read at the top of the loop to get the profile record size byte, map_code, and NPC Record #
	STA INV_8.MERCHANT_PROFILE.SEEK_BYTES+$0
	LDA #$00 ;profile record size is 8-bit so set the HO seek size byte value to #$00. 
	STA INV_8.MERCHANT_PROFILE.SEEK_BYTES+$1
	JMP INV_8.INIT.MERCHANT_PROFILE.SEARCH.LOOP

.ERROR.MERCHANT.NOT.FOUND
;INV_8.INIT.MERCHANT_PROFILE.SEARCH.LOOP reports that the map code and NPC record # of the merchant the player talked to is not found
;in the merchant profile table. This error was triggered because X-REG flipped to $00, which is used as the merchant profile ID counter, 
;which increments as each sequential profile record is checked for a match.
	; LDA #$AA
	; LDX #$AA
	; LDY #$AA
	JSR PREP.BRK
	BRK
	
	
.MERCHANT_PROFILE.FOUND
@END

			; LDA INV_8.MERCHANT.PROFILE.MAP_CODE	 ;load map code field of current merchant profile record
			; sta $bf00
			; LDA INV_8.MERCHANT.PROFILE.NPC_RECORD ;load NPC Record # field of current merchant profile record
			; sta $bf01
			; lda PLAYER.MAP.CODE ;does the map code of the player's current map match the map code field of the current merchant_ID table record?
			; sta $bf02
			; lda INV_8.NPC_RECORD
			; sta $bf03
			; lda #$aa
			; jsr prep.brk
			; brk
			
					

.READ.MERCHANT_PROFILE
@START				
		;set destination memory address
		;(I'm not really sure why this variable set is needed but removing it causes problems)
		lda #INV_8.MERCHANT.PROFILE.DATA+$03 ;+$3 for the bytes already read at the top of the loop
		sta parm.ldrlo
		lda /INV_8.MERCHANT.PROFILE.DATA+$03 ;+$3 for the bytes already read at the top of the loop
		sta parm.ldrhi

		
		LDA INV_8.MERCHANT.PROFILE.RECORD_SIZE ;load 8-bit size (bytes) of current profile record
		SEC
		SBC #$03 ;subract $3 bytes because 3 bytes were read at the top of the loop to get the profile record size byte, map_code, and NPC Record #
		sta PARM.READ_BYTES+$0		;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
		LDA #$00 ;profile record size is 8-bit so set the HO read size byte value to #$00. 
		sta PARM.READ_BYTES+$1		;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	JSR PRODOS.IO.READ_CURRENT_FILE.READ_ONLY ;no file offeset reset. file offset is already in the correct position. 

@END
	
@END	

	

;!!!!WARNING: the intro text must be printed immediately after the profile data is read because
;I'm seeing a problem with INV.DATA.MERCHANT_PROFILE getting clobbered as of INV.DATA.MERCHANT_PROFILE+$70,
;which only affects the ASCII intro text (displayed once) stored in the profile
.INIT.INTRO_TEXT
@START

	JSR INV_4.1a.INIT.SCROLL_WINDOW

					;JSR KEYIN
					
		LDA #INV_8.MERCHANT.PROFILE.DATA
		CLC
		ADC INV_8.MERCHANT.PROFILE.NON_ASCII_SIZE
		STA TWF.STRING
		
		LDA /INV_8.MERCHANT.PROFILE.DATA
		ADC #$00 ;16-bit add
		STA TWF.STRING+$1
	JSR INV_8.PRINT.TEXT.WINDOW

@END	
	
INV_8.INIT.INITAL_LAUNCH_ONLY.DONE
@END


			
			

					;JSR KEYIN
			
			
	
.INIT.HEADERS ;**OPT** Memory. Use screen builder
@START
;ITEM/PRICE COLUMN HEADER
		LDA #INV_8.HEADER1.HTAB
		STA HTAB	
		LDA #INV_8.HEADER1.VTAB 
		STA VTAB
	JSR	UPDATE.CHAR.POS

		LDA #INV_8.TEXT_BLOCK.HEADER1
		STA STRING+$0
		LDA /INV_8.TEXT_BLOCK.HEADER1
		STA STRING+$1
	JSR PRINT.STR.BACKGROUND

;PRINT PLAYER GOLD LABEL
		LDA #INV_8.PLAYER_GOLD_LABEL.HTAB
		STA HTAB	
		LDA #INV_8.PLAYER_GOLD_LABEL.VTAB
		STA VTAB
	JSR	UPDATE.CHAR.POS
		
		LDA #INV_8.TEXT_BLOCK.GOLD					
		STA STRING+$0
		
		LDA /INV_8.TEXT_BLOCK.GOLD
		STA STRING+$1
	JSR PRINT.STR
	
	
;print (B)uy (S)ell text
		LDA #INV_8.TEXT_BLOCK.BUY_SELL.HTAB
		STA HTAB	
		LDA #INV_8.TEXT_BLOCK.BUY_SELL.VTAB
		STA VTAB
	JSR	UPDATE.CHAR.POS
		
		LDA #INV_8.TEXT_BLOCK.BUY_SELL					
		STA STRING+$0
		
		LDA /INV_8.TEXT_BLOCK.BUY_SELL
		STA STRING+$1
	JSR PRINT.STR
	
	
@END	

					;JSR KEYIN
		

.INIT_SESSION
	;update flag /w initial launch complete
	;(so that above routine doesn't run again until the player exists the inventory module and reenters)
	
	LDA INV.SUB_MODULE.LOAD_FLAGS+$8 ;($00 = sub-module not loaded in memory | $01 = sub-module loaded in memory | high-bit set = initial launch complete)
	ORA #$80 ;mask-in high-bit
	STA INV.SUB_MODULE.LOAD_FLAGS+$8 ;($00 = sub-module not loaded in memory | $01 = sub-module loaded in memory | high-bit set = initial launch complete)

	

	;INV.READIED_FLAGS.BIT_TEST_MASK
	
	

@END

INV_8.MERCHANT_INVENTORY.STATE_LOOP	;entrance from INV_4 menus
@START



			
			
;UPDATE STATE FLAGS

	;set call from = INV_8 (merchant transaction)
	LDA #$01
	STA INV_4.1b.CALL_FROM	;($00 = subroutine called from INV_4, display player inventory | $01 = subroutine called from INV_8 merchant transactions)


;REFRESH PLAYER GOLD AMOUNT	

	JSR INV_8.PRINT.PLAYER_GOLD_AMOUNT

							;JSR KEYIN

;REFRESH MERCHANT INVENTORY WINDOW			
	JSR INV_8.DISPLAY_MERCHANT.INVENTORY

							;JSR KEYIN
		
@END
						; LDA #$AA
						; JSR PREP.BRK
						; BRK
						

		

			
INV_8.STATE.LOOP.ALT.ENTRANCE
@START
	;**OPT** Memory. Can probably use INV_4.STATE.LOOP.ALT.ENTRANCE up through the tab key check, then have it JUMP
	;back here (on detect of merchant trans module loaded) to do the rest of the keypress parsing

	
					;LDA 
					; lda cow+$0
					; sta $bf00
					; lda cow+$1
					; sta $bf01
					; lda cow+$2
					; sta $bf02
					; lda cow+$3
					; sta $bf03
					; lda cow+$4
					; sta $bf04
					; lda cow+$5
					; sta $bf05
					; lda #$aa
					; LDx #INV_8.MERCHANT.PROFILE.DATA
					; ldy /INV_8.MERCHANT.PROFILE.DATA

					; jsr prep.brk
					; brk	




	
					
.KEYIN.LOOP
	
	LDA $C000
    BPL .KEYIN.LOOP
	STA $C010               ;CLR LAST KEY
		;ACC = keypress
	JSR CONVERT.ASCII.UCASE
		;RETURN VALUE: ACC = ASCII code (upper case, if the code is a letter)

;LOAD INV_4.1b sub_module (subroutines needed for state loop keypress commands)
@START
;SAVE REGISTERS
	PHA

	LDA INV_8.subroutines4.1b_FLAG		;($00 = not in memory| >=$01 = in memory).
	BNE .LOAD.INV_4.1b.DONE
	
		;set destination memory address
		lda #INV_4.1b.ROUTINES
		sta parm.ldrlo
		lda /INV_4.1b.ROUTINES
		sta parm.ldrhi
		
		lda #INV_4.1b.SHARED_ROUTINES.SEEK_BYTES+$0
		sta PARM.SEEK_BYTES+$0	;seek length (LO byte).
		lda /INV_4.1b.SHARED_ROUTINES.SEEK_BYTES+$1
		sta PARM.SEEK_BYTES+$1	;seek length (HO byte).

		
		LDA #INV_4.1b.SHARED_ROUTINES.READ_BYTES
		sta PARM.READ_BYTES+$0		;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
		LDA /INV_4.1b.SHARED_ROUTINES.READ_BYTES
		sta PARM.READ_BYTES+$1		;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	JSR PRODOS.IO.READ_CURRENT_FILE

.LOAD.INV_4.1b.DONE	
;RESTORE REGISTERS
	PLA
	;set INV_4.1b flag = in memory
	STA INV_8.subroutines4.1b_FLAG		;($00 = not in memory| >=$01 = in memory).

@END

	
.HOTKEY.CHECK ;test for keypress of 2-5, 7
	CMP #$B6 ;in merchant trans interface mode, game settings isn't a valid menu
	BEQ .HOTKEY.CHECK.DONE
	CMP #$B2			
	BCC .HOTKEY.CHECK.DONE
	CMP #$B8 ;one greater than in .MAIN_INVENOTRY.INTERFACE mode to permit key 7 for the merchant inventory menu
	BCC INV_8.EXIT_STEP ;exit, then goto the menu associated with the hotkey

.HOTKEY.CHECK.DONE	
	
	CMP #$89			;TAB (next menu)
	BEQ INV_8.EXIT_STEP

	; CMP #$8B			;UP ARROW
	; BEQ INV_4.COMMAND.CHANGE.ACTIVE_PLAYER.UP
	; CMP #$8A			;DOWN ARROW
	; BEQ INV_4.COMMAND.CHANGE.ACTIVE_PLAYER.DOWN	

	CMP #$9B			;ESC (EXIT COMBAT)
	BEQ	INV_8.EXIT_STEP
	
	;is inventory empty?
	LDX INV_4.1b.SCREEN_LIST.TOTAL_RECORDS ;(0-3 = item # of last item on screen | $FF = no inventory records are eligible for display)
	BMI INV_8.STATE.LOOP.ALT.ENTRANCE

	

	CMP #$88			;LEFT ARROW
	BEQ	INV_8.COMMAND.LEFT_ARROW
	
	CMP #$95			;RIGHT ARROW
	BEQ INV_8.COMMAND.RIGHT_ARROW

	
	CMP #$D3				;(S)ell item   (switches to player inventory window)
	BEQ INV_8.SELL_ITEMS
	
	CMP #$C2				;(B)uy item
	BEQ INV_8.BUY_ITEM_STEP
	
	CMP #$8D				;Enter key: Buy item
	BEQ	INV_8.BUY_ITEM_STEP	
		
	CMP #$C9				;(I)nfo on selected item
	BEQ INV_8.COMMAND.INFO


	;playtest keys
	CMP #$A8				;SHIFT+9 (SET HOOK)
	BEQ	INV_8.PLAYTEST.KEY_SHIFT9

	CMP #$D1				;(Q)uit
	BEQ	INV_8.COMMAND.QUIT	
	
	;default case
	JMP .KEYIN.LOOP ;do nothing, wait for next keypress

	
; INV_8.COMMAND.CHANGE.ACTIVE_PLAYER.UP
	; JSR CHARACTER.ROSTER.INCREMENT_UP
	; JMP INV_4.DISPLAY_PLAYER_INVENTORY
	
; INV_8.COMMAND.CHANGE.ACTIVE_PLAYER.DOWN
	; JSR CHARACTER.ROSTER.INCREMENT_DOWN
	; JMP INV_4.DISPLAY_PLAYER_INVENTORY	



	
INV_8.EXIT_STEP
	JMP INV_8.EXIT
	
INV_8.BUY_ITEM_STEP
	JMP INV_8.BUY_ITEM
	
INV_8.SELL_ITEMS
		LDA #$89	;simulates a tab key press to affect a switch to the player inventory window
	JMP INV_8.EXIT
	

	
INV_8.COMMAND.INFO
@START
;(prints additional information on the selected item to the scroll text window. The variables INV.ACTIVE_MENU.CODE and 
;INV_4.1b.DISPLAY.MODE are used by the subroutine INV_4.1b.ADDITIONAL_INFO to determine the format of the text printed, which is
;based on item_type and if misc whether the item_ID is in the spell item range)


	
	JSR INV_8.GET.SELECTED.ITEM.TYPE_ID
		;RETURN: INV_8.SELECTED.ITEM_TYPE, INV_8.SELECTED.ITEM_ID, X-REG (index to SCREEN_ITEM_LIST.ARRAY for the item selected on screen)
	

		;**FALLS THROUGH**
		
.SET.ITEM_TYPE
		LDA INV_8.SELECTED.ITEM_TYPE ;load item_type of the item selected onscreen
		STA INV_4.1b.DISPLAY.MODE ;($00 = display weapons | $01 = display armor | $02 = misc items)		
		CLC
		ADC #$01 ;menu code = item_type +1 because menu code starts with $01. 
		PHA ;save tenative value for INV.ACTIVE_MENU.CODE
		CMP #INV.ACTIVE_MENU.MISC_ITEMS ;is item_type miscellanous? (if yes then it might be a spell item depending on if the item_ID is in the spell item range)
		BNE .ITEM.NOT_SPELL	;branch, if no
		INY ;advance to byte $01 of item's profile date record
		LDA INV_8.SELECTED.ITEM_ID	;load item_ID of the item selected onscreen
		CMP #INV_4.SPELL_RANGE.LTH
		BCS .ITEM.NOT_SPELL
		
		;**FALLS THROUGH**


.ITEM_SPELL
		PLA ;restore tenative value for INV.ACTIVE_MENU.CODE
		CLC
		ADC #$01
		
		
		JMP .SAVE.MENU_CODE

.ITEM.NOT_SPELL	
		PLA ;restore tenative value for INV.ACTIVE_MENU.CODE
	

		;**FALLS THROUGH**
		
.SAVE.MENU_CODE
		;ACC = menu code
		STA INV.ACTIVE_MENU.CODE
	JSR INV_4.1b.ADDITIONAL_INFO
	
	;reset active menu code
	;(INV.ACTIVE_MENU.CODE was temporarily altered in the code above to fool INV_4.1b.ADDITIONAL_INFO because it uses INV.ACTIVE_MENU.CODE as a parameter to determine
	;which text format to use). 
	LDA #$06 ;set active menu: merchant inventory window
	STA INV.ACTIVE_MENU.CODE
	JMP INV_8.STATE.LOOP.ALT.ENTRANCE
@END
	
INV_8.COMMAND.LEFT_ARROW
	JMP INV_8.MOVE_SELECTOR_UP

INV_8.COMMAND.RIGHT_ARROW
	JMP INV_8.MOVE_SELECTOR_DOWN
	
; INV_8.COMMAND.ENTER
; ;****INV DEBUG LOG*** 			;**OPT** Memory. Remove.
; @START
; .DEBUG.LABEL
		; ; ;PHA
		; ; LDA #.DEBUG.LABEL
		; ; STA INV.DEBUG.ADDRESS+$0
		; ; LDA /.DEBUG.LABEL
		; ; STA INV.DEBUG.ADDRESS+$1
	; JSR INV.DEBUG.LOG.WRITE
		; ;;PLA
; @END

	; JMP INV_4_GROUP2.READY_UNREADY.REQUEST


INV_8.PLAYTEST.KEY_SHIFT9	
	JSR PLAYTEST.KEY0 ;the JSR is to KEY_0 because the main game loop uses KEY_0 and this routine is resident in the main game loop source file.  
	JMP INV_8.STATE.LOOP.ALT.ENTRANCE

INV_8.COMMAND.QUIT
	JMP COMMAND.QUIT.PREP

@END
@END

;INV_4.EXIT.MT.INIT
@START
	;see .INV_4.EXIT.MT.INIT above. 
	;this section is setup as a placeholder so that all exit routines are found at the bottom of the main 
	;routine of module. 

@END

INV_8.EXIT ;main exit routine (exit's inventory module entirely)
@START
;PARAMETERS: ACC = keypress from INV_8.STATE.LOOP

;SAVE REGISTERS
	PHA ;save keypress from INV_8.STATE.LOOP
	
;ERASE MENU ITEM SELECTOR (from merchant inventory window)				
		LDA #$00 ;set mode ($00 = item selector OFF | $7F = item selector ON) 
		SEC ;set sync mode (CLC = don't sync hi-res pages | SEC = sync hi-res pages)
	JSR INV_8.DRAW_ERASE.ITEM_SELECTOR	
	JSR FLIP.PAGE

	JSR INV_8.SYNC_PAGES.MERCHANT_INV_WINDOW ;sync merchant inventory window. menu selector is only erased on background page and INV_4 will flip the page.


.EXIT
;RESTORE REGISTERS
	PLA ;restore keypress from INV_8.STATE.LOOP

	JMP INV_4.EXIT
	
@END


	



;LOCAL SUBROUTINES
@START

;MENU MANAGEMENT
INV_8.MOVE_SELECTOR_UP
@START
.ERASE.ITEM_SELECTOR
		;LDA #$7F ;set mode ($00 = item selector OFF | $7F = item selector ON) 
		LDA #$00 ;set mode ($00 = item selector OFF | $7F = item selector ON) 
		;INV_4.1b.SCREEN_ITEM_LIST.SELECTED.ITEM: use existing value
		SEC ;(CLC = don't sync hi-res pages | SEC = sync hi-res pages)
	JSR INV_8.DRAW_ERASE.ITEM_SELECTOR		
					
					; LDA #$ab
					; LDx INV_4.1b.SCREEN_ITEM_LIST.SELECTED.ITEM ;is the item selector set to the last item in the list?
					; ldy #INV_4.MAX_ONSCREEN.ITEMS
					; jsr prep.brk
					; brk
					
.SCROLL.DOWN ;scrolling down allows 1st item on screen to be replaced with new item
	DEC INV_8.BYTE_COUNTER ;tracks the offset of the selected item's record in INV_8.MERCHANT.PROFILE.DATA. used to determine if INV_8.GET.NEXT_ITEM has reached top/bottom of merchant profile's item array. 
	DEC INV_8.BYTE_COUNTER

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
	JSR INV_8.GET.NEXT_ITEM
		;RETURN VALUE: ACC ($00 = next item found, $01 = top or bottom of array reached), INV_4.1b.NEXT_ITEM.QTY_AVAILABLE, INV_4.NEXT_ITEM.READIED_STATUS, PLAYER_MERCH.INVENTORY.DATA.POINTER(2)
	

		

		
	;exit test
	CMP #$01 ;was top/bottom of array reached? (i.e. there is no next item)
	BEQ .DRAW.ITEM_SELECTOR.ALT_ENTRANCE ;branch if yes. draw item selector (without item selection increment) because selector was aready erased above. 
	;BEQ .TEST ;branch if yes. draw item selector (without item selection increment) because selector was aready erased above. 

.VALIDATE.ITEM
;should item be displayed onscreen?

;should item be displayed onscreen?

;yes, always. In INV_4 this code section does some checks. 



			
			
	;**FALLS THROUGH** 	;Since the party owns more of this item than is readied by the PCs, then the item should be displayed
	
	JSR INV_4.1b.SCREEN_LIST.SCROLL.DOWN

	
	
			
			
.SAVE.ITEM
	
		;INV_4.1b.NEXT_ITEM.QTY_AVAILABLE: use existing value
		;INV_4.NEXT_ITEM.READIED_STATUS: use existing value
		;PLAYER_MERCH.INVENTORY.DATA.POINTER: use existing value
		LDX #$00 ;set INV_4.1b.SCREEN_ITEM_LIST.ARRAY index to the start of item 0 (of item0-item2)
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



			

		LDA #$15
		STA DRAW.START_BYTE

		LDA #$27
		STA DRAW.STOP_BYTE.ALT

		LDA #$48 ;top line of the first row of text characters of the penultimate item
		STA LINE.START.COPY_FROM
	JSR INV_4.1b.DISPLAY_PLAYER_INVENTORY.SCROLL_DOWN
	


			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; JSR FLIP.PAGE
			; JSR KEYIN
			; ;JSR FLIP.PAGE
			; LDA #$AA
			; LDX TEMP
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP



			
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

	
	
			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP2
; ;=====DEBUG HOOK======						
		; LDX #$00
; .DEBUG.COPY1.1
		; LDA INV_4.1b.SCREEN_ITEM_LIST.ARRAY,X
		; STA $BD00,X
		; INX
		; BNE .DEBUG.COPY1.1
		
		
		; LDX #$00
; .DEBUG.COPY2.1
		; LDA INV_4.1a.SCREEN_ITEM_LIST.DATA,X
		; STA $BE00,X
		; INX
		; BNE .DEBUG.COPY2.1

			; LDA #$BC
			; LDX INV_4.1b.SCREEN_ITEM_LIST.SELECTED.ITEM
			; JSR PREP.BRK
			; BRK
; .TEMP2
			; LDA TEMP
			
		LDA #$7F ;set mode ($00 = item selector OFF | $7F = item selector ON) 
		;LDA #$00 ;set mode ($00 = item selector OFF | $7F = item selector ON) 
		CLC ;(CLC = don't sync hi-res pages | SEC = sync hi-res pages)
	JSR INV_8.DRAW_ERASE.ITEM_SELECTOR
	
.EXIT
	JSR FLIP.PAGE

			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP2
			; ;JSR FLIP.PAGE
			; JSR KEYIN
			; ;JSR FLIP.PAGE
			; ; LDA #$AA
			; ; LDX TEMP
			; ; JSR PREP.BRK
			; ; BRK
; .TEMP2
			; LDA TEMP
			
	
.EXIT.ALT
						;JSR KEYIN



	JMP INV_8.STATE.LOOP.ALT.ENTRANCE

; .TEST
		; LDA #$AA
		; LDX #$AA
		; LDY #$AA
		; JSR PREP.BRK
		; BRK
		

@END


INV_8.MOVE_SELECTOR_DOWN ;**OPT** Memory (INV_4 / INV_8 trade off). INV_8.MOVE_SELECTOR_DOWN and INV_4.MOVE_SELECTOR_DOWN could be combined, but that would require  ;a handful of merchant mode checks in INV_4. I did them initially as separate routine because INV_4 is really right on memory and INV_8 seems to have plenty.;this concept also applies to INV_8.MOVE_SELECTOR_UP
@START

.DETECT.END_OF_SCREEN_LIST
;update 9/1/2017; This routine was copied over from the INV_4 version. this
;section from here to .DETECT.END_OF_SCREEN_LIST.DONE doesn't really make sense to me
;at this point. I understand not moving the item selector if it's already at the end of the list
;but that doesn't seem to be what this code is doing. 
;
;if there are less than 6 items displayed on the screen, that means less than 6 items in the
;parties inventory are relevant for display to the active PC. In this case, there is no
;reason to scroll and the item selector should stay where it is.
	LDA #INV_8.MAX_ONSCREEN.ITEMS-1 ;-1 because the constant is a quantity (starting with 1) and the variable in the CMP starts with 0. 
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
		
		;LDA #$7F ;set mode ($00 = item selector OFF | $7F = item selector ON) 
		LDA #$00 ;set mode ($00 = item selector OFF | $7F = item selector ON) 
		;INV_4.1b.SCREEN_ITEM_LIST.SELECTED.ITEM: use existing value
		SEC ;(CLC = don't sync hi-res pages | SEC = sync hi-res pages)
	JSR INV_8.DRAW_ERASE.ITEM_SELECTOR	


					; LDA #$AA
					; JSR PREP.BRK
					; BRK
					
					

					
					
					; LDA #$ab
					; LDx INV_4.1b.SCREEN_ITEM_LIST.SELECTED.ITEM ;is the item selector set to the last item in the list?
					; ldy #INV_4.MAX_ONSCREEN.ITEMS
					; jsr prep.brk
					; brk
					
.SCROLL.UP ;scrolling down allows last item on screen to be replaced with new item

	INC INV_8.BYTE_COUNTER ;tracks the offset of the selected item's record in INV_8.MERCHANT.PROFILE.DATA. used to determine if INV_8.GET.NEXT_ITEM has reached top/bottom of merchant profile's item array. 
	INC INV_8.BYTE_COUNTER

					
;VALIDATE ENTRANCE
	LDA INV_4.1b.SCREEN_ITEM_LIST.SELECTED.ITEM ;is the item selector set to the last item in the list?
	CMP #INV_8.MAX_ONSCREEN.ITEMS-1 ;-1 because the constant is a quantity (starting with 1) and the array in the LDA starts with 0. 
	BCC .SCROLL.UP.DONE


	;set pointer to item2 (of 0-2)
	LDA INV_4.1b.SCREEN_ITEM_LIST.ARRAY+$8 ;load LO byte of inventory record address for item0
	STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
	LDA INV_4.1b.SCREEN_ITEM_LIST.ARRAY+$9 ;load HO byte of inventory record address for item0
	STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$1

						
;GET NEXT ITEM
.LOOP.GET.NEXT_ITEM

		LDA #$00 ;($00 = iterate down | $01 = iterate up)
	JSR INV_8.GET.NEXT_ITEM
		;RETURN VALUE: ACC ($00 = next item found, $01 = top or bottom of array reached), INV_4.1b.NEXT_ITEM.QTY_AVAILABLE, INV_4.NEXT_ITEM.READIED_STATUS, PLAYER_MERCH.INVENTORY.DATA.POINTER(2)
	



						
	;exit test
	CMP #$01 ;was top/bottom of array reached? (i.e. there is no next item)
	BEQ .DRAW.ITEM_SELECTOR.ALT_ENTRANCE ;branch if yes. draw item selector (without item selection increment) because selector was aready erased above. 
	;BEQ .TEST ;branch if yes. draw item selector (without item selection increment) because selector was aready erased above. 

.VALIDATE.ITEM
;should item be displayed onscreen?

;yes, always. In INV_4 this code section does some checks. 

						
						


					;JSR .TROUBLESHOOTING.HOOK

			
	JSR INV_4.1b.SCREEN_LIST.SCROLL.UP


			
.SAVE.ITEM
	
		;INV_4.1b.NEXT_ITEM.QTY_AVAILABLE: use existing value
		;INV_4.NEXT_ITEM.READIED_STATUS: use existing value
		;PLAYER_MERCH.INVENTORY.DATA.POINTER: use existing value
		LDX #$8 ;set INV_4.1b.SCREEN_ITEM_LIST.ARRAY index to the start of item 2 (of item0-item2)
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



		;set INV_4.1b.DISPLAY_PLAYER_INVENTORY.SCROLL_UP parameteres	
		LDA #$15
		STA DRAW.START_BYTE

		LDA #$27
		STA DRAW.STOP_BYTE.ALT

		;LDA #$A0
		LDA #$67
		STA TWF.SCROLLING.STOP_LINE				;set the stop line, which is the last line of the last row of text characters to scroll +$08. 	
	JSR INV_4.1b.DISPLAY_PLAYER_INVENTORY.SCROLL_UP
	
	JMP .DRAW.ITEM_SELECTOR.ALT_ENTRANCE ;draw item selector (without item selection increment) because selector was aready erased above.


.SCROLL.UP.DONE
	
.DRAW.ITEM_SELECTOR ;(also prints new item if a scroll occured)
	
	INC INV_4.1b.SCREEN_ITEM_LIST.SELECTED.ITEM ;advance selector to next item down on the video screen


.DRAW.ITEM_SELECTOR.ALT_ENTRANCE

			
		LDA #$7F ;set mode ($00 = item selector OFF | $7F = item selector ON) 
		;LDA #$00 ;set mode ($00 = item selector OFF | $7F = item selector ON) 
		CLC ;(CLC = don't sync hi-res pages | SEC = sync hi-res pages)
	JSR INV_8.DRAW_ERASE.ITEM_SELECTOR	

.EXIT
	JSR FLIP.PAGE

.EXIT.ALT

	JMP INV_8.STATE.LOOP.ALT.ENTRANCE


	
	
; .TROUBLESHOOTING.HOOK						
			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP

						; ; LDA INV_4.1b.SCREEN_ITEM_LIST.SELECTED.ITEM
						; ; STA $BF00
						
						; ; LDA #INV_4.1b.SCREEN_ITEM_LIST.ARRAY
						; ; STA $BF01
						; ; LDA /INV_4.1b.SCREEN_ITEM_LIST.ARRAY
						; ; STA $BF02
						; ; LDA #INV_4.1a.SCREEN_ITEM_LIST.DATA
						; ; STA $BF03
						; ; LDA /INV_4.1a.SCREEN_ITEM_LIST.DATA
						; ; STA $BF04

						; ; LDA PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
						; ; STA $BF05
						; ; LDA PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
						; ; STA $BF06
						; ; LDA #INV_8.MERCHANT.PROFILE.DATA
						; ; STA $BF07
						; ; LDA /INV_8.MERCHANT.PROFILE.DATA
						; ; STA $BF08				


						; ; LDA INV_4.1b.SCREEN_ITEM_LIST.SELECTED.ITEM ;is the item selector set to the last item in the list?
						; ; STA $BF09
						; ; LDA #INV_8.MAX_ONSCREEN.ITEMS-1 ;-1 because the constant is a quantity (starting with 1) and the array in the LDA starts with 0. 
						; ; STA $BF0A
						
						; LDA #$AA
						; LDX TEMP
						; JSR PREP.BRK
						; BRK
; .TEMP
			; LDA TEMP

			; RTS
		

@END

;DATA
INV_8.GET.NEXT_ITEM
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
	ADC #INV_8.MERCHANT.PROFILE.ITEM_DATA.RECORD_SIZE
	STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
	LDA PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
	ADC /INV_8.MERCHANT.PROFILE.ITEM_DATA.RECORD_SIZE ;16-bit add
	STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
.INCREMENT.BYTE_COUNTER.DOWN
	; INC INV_8.BYTE_COUNTER
	; INC INV_8.BYTE_COUNTER
	LDA INV_8.BYTE_COUNTER ;tracks the offset of the selected item's record in INV_8.MERCHANT.PROFILE.DATA. used to determine if INV_8.GET.NEXT_ITEM has reached top/bottom of merchant profile's item array. 
.ARRAY_END_TEST.DOWN
	;ACC = INV_8.BYTE_COUNTER
	CMP INV_8.MERCHANT.PROFILE.NON_ASCII_SIZE
	BCC .NEXT_RECORD.DONE
	
	;rest byte counter to last record
	LDA INV_8.MERCHANT.PROFILE.NON_ASCII_SIZE
	SEC
	SBC #INV_8.MERCHANT.PROFILE.ITEM_DATA.RECORD_SIZE
	STA INV_8.BYTE_COUNTER ;tracks the offset of the selected item's record in INV_8.MERCHANT.PROFILE.DATA. used to determine if INV_8.GET.NEXT_ITEM has reached top/bottom of merchant profile's item array. 
	JMP .START_OR_END.OF_ARRAY_REACHED
	

.NEXT_RECORD.ITERATE_UP
	LDA PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
	SEC
	SBC #INV_8.MERCHANT.PROFILE.ITEM_DATA.RECORD_SIZE
	STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
	LDA PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
	SBC /INV_8.MERCHANT.PROFILE.ITEM_DATA.RECORD_SIZE ;16-bit add
	STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
.INCREMENT.BYTE_COUNTER.UP
	; DEC INV_8.BYTE_COUNTER
	; DEC INV_8.BYTE_COUNTER

.ARRAY_END_TEST.UP

	LDA	INV_8.BYTE_COUNTER ;tracks the offset of the selected item's record in INV_8.MERCHANT.PROFILE.DATA. used to determine if INV_8.GET.NEXT_ITEM has reached top/bottom of merchant profile's item array. 
	CMP #INV_8.MERCHANT.PROFILE.ITEM_RECORDS_START.OFFSET
	BCS .NEXT_RECORD.DONE
	
	;reset byte counter to first record
	LDA #INV_8.MERCHANT.PROFILE.ITEM_RECORDS_START.OFFSET
	STA INV_8.BYTE_COUNTER ;tracks the offset of the selected item's record in INV_8.MERCHANT.PROFILE.DATA. used to determine if INV_8.GET.NEXT_ITEM has reached top/bottom of merchant profile's item array. 
	
	JMP .START_OR_END.OF_ARRAY_REACHED
	
			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; lda #INV_8.MERCHANT.PROFILE.ITEM_RECORDS_START.OFFSET
			; sta $bf00
			; lda #$aa
			; ldx temp
			; ldy INV_8.BYTE_COUNTER
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP
			
			
	; ;ACC = INV_8.BYTE_COUNTER
	; CMP #INV_8.MERCHANT.PROFILE.ITEM_RECORDS_START.OFFSET
	; BEQ .START_OR_END.OF_ARRAY_REACHED

	;**FALLS THROUGH**

.NEXT_RECORD.DONE
	
;these variables should stay at their init values set in INV_8.DISPLAY_MERCHANT.INVENTORY	
; .CALCULATE.READIED_QTY.FIELDS
	; JSR INV_4.CALCULATE.READIED_QTY.FIELDS
		; ;RETURN: INV_4.NEXT_ITEM.READIED_STATUS, INV_4.1b.NEXT_ITEM.QTY_AVAILABLE
	
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


			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; lda #INV_8.MERCHANT.PROFILE.ITEM_RECORDS_START.OFFSET
			; sta $bf00
			; lda #$aa
			; ldx temp
			; ldy INV_8.BYTE_COUNTER
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP

			
			
;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX
	
	;ACC = return value
	LDA TEMP ;restore return value
	
	RTS

	
	
@END

INV_8.GET.ITEM_FINAL_PRICE
@START
;PARAMETERS: X-REG (index to INV_4.1b.SCREEN_ITEM_LIST.ARRAY for item), INV_8.GET.ITEM_FINAL_PRICE.PARM.PERCENT
;ENTRANCE: any INV_8 or INV_4.1 routine
;RETURN: RESULT(2)* INV_8.GET.ITEM_BASE_PRICE.PARM.ITEM_TYPE, INV_8.GET.ITEM_BASE_PRICE.PARM.ITEM_ID, XREG = (index to INV_4.1b.SCREEN_ITEM_LIST.ARRAY for item), INV_8.GET.ITEM_BASE_PRICE.RETURN.PRICE(2)
;
;*contains final price

;=====================CODE-SECTION DOCUMENTATION==========================================================================
;
;Uses the item_type and item_Id parms to get the base price for the item from the master price list and
;then use the buy or sell factor passed to this routine as INV_8.GET.ITEM_FINAL_PRICE.PARM.PERCENT to calculate
;the final price for the item and return it as RESULT(2)
;
;===========================================================================================================================



		;X-REG: passed to this routine as parm
	JSR INV_8.GET.ITEM.TYPE_ID
		;RETURN INV_8.GET.ITEM_BASE_PRICE.PARM.ITEM_TYPE, INV_8.GET.ITEM_BASE_PRICE.PARM.ITEM_ID
		
		

.GET.ITEM_BASE_PRICE
		;INV_8.GET.ITEM_BASE_PRICE.PARM.ITEM_TYPE ;set above
		;INV_8.GET.ITEM_BASE_PRICE.PARM.ITEM_ID ;set above
	JSR INV_8.GET.ITEM_BASE_PRICE
		;RETURN: INV_8.GET.ITEM_BASE_PRICE.RETURN.PRICE(2), XREG = (index to INV_4.1b.SCREEN_ITEM_LIST.ARRAY for item)


.CALCULATE.FINAL_PRICE
;apply the merchant's buy factor as a percentage. 
;(final price = base price * buy factor)
		
		LDA INV_8.GET.ITEM_BASE_PRICE.RETURN.PRICE+$0 ;load item base_price LO byte
		STA INV_8.APPLY_PERCENT.PARM_BASE+$0 ;variable borrowed from the combat routine
		LDA INV_8.GET.ITEM_BASE_PRICE.RETURN.PRICE+$1 ;load item base_price HO byte
		STA INV_8.APPLY_PERCENT.PARM_BASE+$1 ;variable borrowed from the combat routine
		LDA INV_8.GET.ITEM_FINAL_PRICE.PARM.PERCENT		
		STA INV_8.APPLY_PERCENT.PARM.PERCENT
	JSR INV_8.APPLY.PERCENTAGE.HEX_ONLY
		;RETURN VALUE: result(2)




			
					; lda INV_8.GET.ITEM_BASE_PRICE.RETURN.PRICE+$0
					; sta result+$0
					; lda INV_8.GET.ITEM_BASE_PRICE.RETURN.PRICE+$1
					; sta result+$1
.EXIT


	RTS
@END

INV_8.GET.TOTAL_PRICE
@START
;PARAMETERS: RESULT(2) = FINAL PRICE, INV_8.BUY_ITEM.QTY
;ENTRANCE: INV_8 and INV_4.1 routines
;RETURN VALUE: PROD(2) ;TOTAL PRICE	


		LDA INV_8.BUY_ITEM.QTY
		STA MULR+$0
		LDA #$00
		STA MULR+$1
		STA MULR+$2
		STA MULR+$3
		STA MULND+$2
		STA MULND+$3
;
		LDA RESULT+$0
		STA MULND+$0
		LDA RESULT+$1
		STA MULND+$1
	JSR MLP.32
		;RETURN VALUE: PROD(2) ;TOTAL PRICE
		;PROD(other digits are ignored)
	RTS
	
@END
	
INV_8.GET.SELECTED.ITEM.TYPE_ID
@START
;PARAMETERS: INV_4.1b.SCREEN_ITEM_LIST.SELECTED.ITEM, INV_4.1b.SCREEN_ITEM_LIST.ARRAY(x)
;ENTRANCE: any INV_8 or INV_4.1 routine
;RETURN: INV_8.SELECTED.ITEM_TYPE, INV_8.SELECTED.ITEM_ID, PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER, X-REG (index to SCREEN_ITEM_LIST.ARRAY for the item selected on screen)


	JSR INV_4.1b.GET.SCREEN_LIST.ARRAY.INDEX
		;X-REG (index to SCREEN_ITEM_LIST.ARRAY for the item selected on screen)
		

	;**FALLS THROUGH**
	
@END

INV_8.GET.ITEM.TYPE_ID
@START
;PARAMETERS: X-REG (index to INV_4.1b.SCREEN_ITEM_LIST.ARRAY for item), INV_4.1b.SCREEN_ITEM_LIST.ARRAY(x)
;ENTRANCE: any INV_8 or INV_4.1 routine
;RETURN: item price printed on the video screen, INV_8.GET.ITEM_BASE_PRICE.PARM.ITEM_TYPE*, INV_8.GET.ITEM_BASE_PRICE.PARM.ITEM_ID

;*Or, via .EQ, INV_8.SELECTED.ITEM_TYPE, INV_8.SELECTED.ITEM_ID

;SAVE REGISTERS
	TYA
	PHA
	
.GET.ITEM_TYPE	
		;identify the item_type and item_ID of the item to print
		LDA INV_4.1b.SCREEN_ITEM_LIST.ARRAY+$0,X ;load LO byte of memory address of selected item's record in INV_8.MERCHANT.PROFILE.DATA
		STA PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER+$0
		
		LDA INV_4.1b.SCREEN_ITEM_LIST.ARRAY+$1,X ;load HO byte of memory address of selected item's record in INV_8.MERCHANT.PROFILE.DATA
		STA PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER+$1
		
		LDY #$00
		LDA (PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER),Y ;load item_level | item_type field for selected item
		;ITEM_TYPE is in LO nibble
		AND #$0F ;mask-out HO nibble. 
		STA	INV_8.GET.ITEM_BASE_PRICE.PARM.ITEM_TYPE ;save item_type
		INY ;advance to byte $01
		LDA (PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER),Y ;load item_ID for selected item
		STA INV_8.GET.ITEM_BASE_PRICE.PARM.ITEM_ID ;save item_ID


		

		
							;***TROUBLESHOOTING HOOK**
							; LDA INV_4.1b.SCREEN_ITEM_LIST.ARRAY+$0,X ;load LO byte of memory address of selected item's record in INV_8.MERCHANT.PROFILE.DATA
							; STA $BF00
							;
							; LDA INV_4.1b.SCREEN_ITEM_LIST.ARRAY+$1,X ;load HO byte of memory address of selected item's record in INV_8.MERCHANT.PROFILE.DATA
							; STA $BF01
							;	
							; LDA	INV_8.GET.ITEM_BASE_PRICE.PARM.ITEM_TYPE		
							; STA $BF02
							;
							; LDA INV_8.GET.ITEM_BASE_PRICE.PARM.ITEM_ID
							; STA $BF03
							;
							; LDA #INV_4.1b.SCREEN_ITEM_LIST.ARRAY
							; STA $BF04
							;
							; LDA /INV_4.1b.SCREEN_ITEM_LIST.ARRAY
							; STA $BF05
							;
							; LDA #$AA
							; JSR PREP.BRK
							; BRK


.EXIT
;RESTORE REGISTERS
	PLA
	TAY
	
	RTS
	
@END

INV_8.GET.ITEM_BASE_PRICE
@START
;PARAMTERS: INV_8.GET.ITEM_BASE_PRICE.PARM.ITEM_TYPE, INV_8.GET.ITEM_BASE_PRICE.PARM.ITEM_ID
;ENTRANCE: any INV_8 or INV_4.1 routine
;RETURN: INV_8.GET.ITEM_BASE_PRICE.RETURN.PRICE(2)

;SAVE REGISTERS
	TXA
	PHA

.SET.INDEX ;INV_8.1.MASTER_PRICE_TABLE.DATA
	LDA INV_8.GET.ITEM_BASE_PRICE.PARM.ITEM_ID
	ASL ;X2 (two byte record)
	TAX

	
.EXECUTE.LOOKUP
	;branch based on item_type
	LDA INV_8.GET.ITEM_BASE_PRICE.PARM.ITEM_TYPE
	BEQ .WEAPONS_TABLE
	CMP #INV.ITEM_TYPE.ARMOR
	BEQ .ARMOR_TABLE
	CMP #INV.ITEM_TYPE.MISC
	BEQ .MISC_TABLE

.ERROR.TRAP
;INV_8.GET.ITEM_BASE_PRICE reports unexpected item_type code in INV_8.GET.ITEM_BASE_PRICE.PARM.ITEM_TYPE
	JSR PREP.BRK
	BRK

;**OPT** Memory. I may be able to save a few bytes by storing the  memory address to each table in an array, do
;a lookup to that array using item_type*2 as the index, then saving the address to a zpage pointer and doing an indirect lookup with Y-REG = item_ID*2	

.WEAPONS_TABLE
	LDA INV_8.MASTER_PRICE_TABLE.WEAPON.START+$0,X ;load price LO byte 
	STA INV_8.GET.ITEM_BASE_PRICE.RETURN.PRICE+$0 
	LDA INV_8.MASTER_PRICE_TABLE.WEAPON.START+$1,X ;load price LO byte 
	STA INV_8.GET.ITEM_BASE_PRICE.RETURN.PRICE+$1	
	JMP .EXECUTE.LOOKUP.DONE
	
.ARMOR_TABLE
	LDA INV_8.MASTER_PRICE_TABLE.ARMOR.START+$0,X ;load price LO byte 
	STA INV_8.GET.ITEM_BASE_PRICE.RETURN.PRICE+$0 
	LDA INV_8.MASTER_PRICE_TABLE.ARMOR.START+$1,X ;load price LO byte 
	STA INV_8.GET.ITEM_BASE_PRICE.RETURN.PRICE+$1

	JMP .EXECUTE.LOOKUP.DONE

.MISC_TABLE
	LDA INV_8.MASTER_PRICE_TABLE.MISC.START+$0,X ;load price LO byte 
	STA INV_8.GET.ITEM_BASE_PRICE.RETURN.PRICE+$0 
	LDA INV_8.MASTER_PRICE_TABLE.MISC.START+$1,X ;load price LO byte 
	STA INV_8.GET.ITEM_BASE_PRICE.RETURN.PRICE+$1
	
	;**FALLS THROUGH**
	
.EXECUTE.LOOKUP.DONE

	
.EXIT	
;SAVE REGISTERS
	PLA
	TAX
	
	RTS
@END

INV_8.ADD_ITEM.PLAYER_INVENTORY  ;for REMOVE_ITEM see .UPDATE.PLAYER_INVENTORY (INV_8.SELL_ITEM)
@START
;PARAMETERS: XREG = (index to INV_4.1b.SCREEN_ITEM_LIST.ARRAY for merchant's item), INV_8.BUY_ITEM.QTY, INV_8.SELECTED.ITEM_TYPE, INV_8.SELECTED.ITEM_ID
;ENTRANCE; INV_8 and INV_4.1 routines	
;RETURN: updated INV.PLAYER.INVENTORY.DATA, ACC = ($00 = add succeeded, $01 = add failed because it would result in qty > !255)

	
.GET.RECORD.POINTER
		JSR INV_8.INIT.PLAYER_INVENTORY.POINTER ;set INV.PLAYER.INVENTORY.DATA.POINTER to top of player inventory table (INV.PLAYER.INVENTORY.DATA)
	JSR INV_8.SEARCH.PLY_INV
		;RETURN: PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER(2)
		
.GET.SCREEN_LIST.DATA.INDEX
		;ACC parm: preset to INV_4.1b.SCREEN_ITEM_LIST.SELECTED.ITEM  ;load selected item number
	JSR INV_4.1a.GET_INDEX.SCREEN_ITEM_LIST.DATA.ALT_ENTRANCE	;set INV_4.1a.SCREEN_ITEM_LIST.DATA index ($20 byte records)
		;RETURN: X-REG = INV_4.1a.SCREEN_ITEM_LIST.DATA index
		
.EVALUATE.RECORD ;is record open or occupied?
;(if the player already owns 1 or more units of the item which the player purchased, then an inventory
;record will exist for that item_type, item_id. In that case the returned pointer is to that existing 
;record. If the player doesn't own any units of the item, then the returned pointer is to the first
;open record searching down from the top of the player inventory table)
	
	;is record open?
	LDY #$00
	LDA (PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER),Y
	CMP #$FF ;don't use BMI, this field has item_level in the HO byte which can have a value of $F, setting the high bit. 
	BNE .ADD_TO.EXISTING.RECORD ;branch if record is open
	
.ADD_TO.OPEN.RECORD


;SAVE RECORD DATA

	;LDY #$00 ;init index to PLAYER_MERCH.INVENTORY.DATA at byte $0
	
	;save item level | item_type field (byte $00)
	LDA INV_4.1a.SCREEN_ITEM_LIST.DATA+INV.ITEM_TABLE.ALL.LEVEL.OFFSET,X	;mask-in item level into HO nibble
			;STA $BF00

	ASL
	ASL
	ASL
	ASL
	ORA INV_8.GET.ITEM_BASE_PRICE.PARM.ITEM_TYPE ;load item_type into LO nibble
	STA (PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER),Y
	
			; STA $BF01
			; LDA INV_8.GET.ITEM_BASE_PRICE.PARM.ITEM_TYPE
			; STA $BF02
			
			; LDA #$AA
			; LDX PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER+$0
			; LDY PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER+$1
			; JSR PREP.BRK
			; BRK

			
	;save item_id field (byte $01)
	INY ;advance to (byte $01)
	LDA INV_8.GET.ITEM_BASE_PRICE.PARM.ITEM_ID ;load item_id
	STA (PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER),Y
	
	;save quantity field (byte $02)
	INY ;advance to (byte $02)
	LDA INV_8.BUY_ITEM.QTY ;load the quantity of the item the player purchased
	STA (PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER),Y

	;save readied flag #1 (byte $03)	
	INY ;advance to (byte $03)
	LDA #$00
	STA (PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER),Y
	
	;save readied flag #2 (byte $04)
	INY ;advance to (byte $04)
	STA (PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER),Y

	;save equipment field (byte $05)
	INY ;advance to (byte $05)
	LDA INV_4.1a.SCREEN_ITEM_LIST.DATA+INV.ITEM_TABLE.ALL.EQUIPMENT_SLOT.OFFSET,X	
	STA (PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER),Y
	
			; STY $BF00
			; LDA INV_4.1a.SCREEN_ITEM_LIST.DATA+INV.ITEM_TABLE.ALL.EQUIPMENT_SLOT.OFFSET,X	
			; STA $BF01
			; LDA INV_4.1a.SCREEN_ITEM_LIST.DATA
			; STA $BF02
 			; LDA INV.ITEM_TABLE.ALL.EQUIPMENT_SLOT.OFFSET
			; STA $BF03
			; STX $BF04
			; LDA #$AA
			; LDX PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER+$0
			; LDY PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER+$1
			; JSR PREP.BRK
			; BRK

			
	JMP .ADD_SUCCEEDS
	
.ADD_TO.EXISTING.RECORD

	
	;save quantity field (byte $02)
	LDY #$02
	LDA (PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER),Y ;load item quantity from existing inventory record
	CLC
	ADC INV_8.BUY_ITEM.QTY ;add $01 to item qty
	CMP (PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER),Y ;compare result of ADC to original value
	BCC .ADD_FAILS ;if the result of the ADC < original qty, then an 8-bit overflow has occured. 
	;ACC = item quantity owned after purchase quantity is added
	STA (PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER),Y ;save item adjusted quantity to existing inventory record
	

.ADD_SUCCEEDS
	LDA #$00 ;($00 = add succeeded, $01 = add failed because it would result in qty > !255)

	JMP .EXIT
	
.ADD_FAILS ;exceeds_capacity
	LDA #$01 ;($00 = add succeeded, $01 = add failed because it would result in qty > !255)
	
	
	;**FALLS THROUGH**
	
.EXIT

			; LDA INV_8.GET.ITEM_BASE_PRICE.PARM.ITEM_TYPE
			; STA $BF00
			; LDA INV_8.GET.ITEM_BASE_PRICE.PARM.ITEM_ID
			; STA $BF01
			; LDA #$AB
			; LDX PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER+$0
			; LDY PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER+$1
			; JSR PREP.BRK
			; BRK			
			
	RTS

			

@END

INV_8.SEARCH.PLY_INV ;returns a pointer to the item searched for or if not found returns a pointer to the first open record from the top of the table
@START
;PARAMETERS: INV_8.GET.ITEM_BASE_PRICE.PARM.ITEM_TYPE, INV_8.GET.ITEM_BASE_PRICE.PARM.ITEM_ID*, PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER(2)**
;ENTRANCE: direct
;RETURN: PLAYER_MERCH.INVENTORY.DATA.POINTER(2)***
;
;*INV__.SEARCH.PLY.INV.ITEM_ID is .EQ to INV_8.PARM.RE.SEARCH.ITEM_ID
;**Init to the top of the player inventory data array
;***PLAYER_MERCH.INVENTORY.DATA.POINTER(2) contains the memory address of the inventory record for the item the search was for -if it was found.


;Note: The above parameters and the parameters named in the .ASM file below share memory
;via .EQs


		.IN 	swap.routines.inv.search.ply_inv.asm



	
@END

INV_8.INIT.PLAYER_INVENTORY.POINTER
@START	
	LDA #INV.PLAYER.INVENTORY.DATA
	STA PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER+$0

	LDA /INV.PLAYER.INVENTORY.DATA
	STA PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER+$1

	RTS
@END

		
;INV_8.DEDUCT_ITEM.PLAYER_INVENTORY
@START

	;For inline version see .DEDUCT_ITEM.PLAYER_INVENTORY (INV_8.SELL_ITEM)
	;
	;I put the code for this inline because I'm not sure it will be needed anywhere else. 

@END


;BUY/SELL
INV_8.BUY_ITEM
@START

.INIT		
;READ PLAYER INVENTORY DATA FROM FILE (**only on first buy transaction during the session)
	JSR INV_4.1b.READ.PLAYER_INVENTORY.DATA




.GET.BUY_QTY
@START
;(ask player for QTY to buy: reject 0. valid range !1-99)
	
		LDA #INV_8.TEXT_BLOCK.BUY_QTY		
		STA TWF.STRING+$0
		
		LDA /INV_8.TEXT_BLOCK.BUY_QTY
		STA TWF.STRING+$1						
	JSR INV_8.PRINT.TEXT.WINDOW

;GET BCD INPUT
			LDA HTAB
			STA KEYIN.STRING.LEFT_EDGE ;backspace key not permitted to reduce HTAB below this value. 
			;INC KEYIN.STRING.LEFT_EDGE
			LDA #$02			;parameter: # of BCD input digits 
		JSR KEYIN.BCD
			;RETURN: RESULT(1), ACC = ($00 = input successful | >=$01 = input aborted via ESC key)
			CMP #$00
			BNE .ABORT_TRANSACTION		
		
;CONVERT BCD TO HEX (FINAL QTY VALUE)		
			LDA RESULT+$0
			STA BCD+$0
			LDA #$00
			STA BCD+$1	
		JSR CONVERT.BCD_TO_HEX
			;RETURN: BINARY, 
			
			;erase animated cursor
			JSR COUT.SPACE_CHAR	;prints a space character at the current HTAB/VTAB position

			
			LDA BIN+$0
			BNE .SAVE.BUY_QTY 
			JMP .ABORT_TRANSACTION
.SAVE.BUY_QTY
			STA INV_8.BUY_ITEM.QTY

@END


.GET.TOTAL_PRICE
@START			
.CALCULATE.ITEM_INDEX ;(index to INV_4.1b.SCREEN_ITEM_LIST.ARRAY for item)
	JSR INV_4.1b.GET.SCREEN_LIST.ARRAY.INDEX
		;X-REG (index to SCREEN_ITEM_LIST.ARRAY for the item selected on screen)
		
		
.GET.ITEM.FINAL_PRICE ;per item		
		LDA INV_8.MERCHANT.PROFILE.BUY_FACTOR
		STA INV_8.GET.ITEM_FINAL_PRICE.PARM.PERCENT
		;INV_8.BUY_ITEM.QTY: set above
		;X-REG: set above
	JSR INV_8.GET.ITEM_FINAL_PRICE
		;RETURN: RESULT(2) ;this is the final price for the item with the buy factor applied
		;	   : INV_8.GET.ITEM_BASE_PRICE.PARM.ITEM_TYPE, INV_8.GET.ITEM_BASE_PRICE.PARM.ITEM_ID
		;	   : XREG = (index to INV_4.1b.SCREEN_ITEM_LIST.ARRAY for item)
		
.CALCULATE.TOTAL_PRICE ;for the quantity of items the player wants to buy
		;RESULT(2) already set (FINAL PRICE)
		;INV_8.BUY_ITEM.QTY: already set
	JSR INV_8.GET.TOTAL_PRICE
		;RETURN VALUE: PROD(2) ;TOTAL PRICE


@END

		

.COMPLETE.TRANSACTION
@START
.PLAYER_GOLD.CHECK ;does player have enough gold to buy the item(s)?
;IS PLAYER GOLD >= TOTAL PRICE?
	
	;is total price so large it is a 3byte (24-bit) number? 
	LDA PROD+$2 ;OP2	(total price, byte3)
	BNE	.PLAYER_GOLD.INSUFFICIENT ;if PROD+$02 is !=0 then the total price > !65535
	
	;compare player gold and total price			
	;16-bit is-greater-or-equal
	;CLC/SEC: testing confirmed that neither are needed for 16-bit >+ test
	LDA PLAYER.GOLD+$0 ;OP1  
	CMP PROD+$0 ;OP2	(total price/LO)
	LDA PLAYER.GOLD+$1 ;OP1+1 (final price/HO)
	SBC PROD+$1 ;OP2+1 (total price/HO)
	BCS .PLAYER_GOLD.EXCEEDS.TOTAL_PRICE ;if carry set then OP1 >= OP2, which means the player has enough gold to buy the item 

.PLAYER_GOLD.INSUFFICIENT	;player doesn't have enough gold	
		LDA #INV_8.TEXT_BLOCK.GOLD_TOO_LOW
		STA TWF.STRING
		
		LDA /INV_8.TEXT_BLOCK.GOLD_TOO_LOW
		STA TWF.STRING+$1
	
	JMP .EXECUTE_PRINT	
.PLAYER_GOLD.EXCEEDS.TOTAL_PRICE ;player has enough gold


.ADD_ITEM.PLAYER_INVENTORY
;(add item to player inventory)

		;INV_8.GET.ITEM_BASE_PRICE.PARM.ITEM_TYPE: set above as return value
		;INV_8.GET.ITEM_BASE_PRICE.PARM.ITEM_ID: set above as return value
	JSR INV_8.ADD_ITEM.PLAYER_INVENTORY
		;RETURN: updated INV.PLAYER.INVENTORY.DATA, ACC = ($00 = add succeeded, $01 = add failed because it would result in qty > !255) 
	CMP #$00 ;did add succeed? 
	BEQ .DEDUCT.GOLD ;branch if yes
	
		LDA #INV_8.TEXT_BLOCK.ITEM_PURCHASE_FAILED
		STA TWF.STRING
		
		LDA /INV_8.TEXT_BLOCK.ITEM_PURCHASE_FAILED
		STA TWF.STRING+$1

	JMP .EXECUTE_PRINT

.ABORT_TRANSACTION
		LDA #INV_8.TEXT_BLOCK.ABORT_TRANSACTION
		STA TWF.STRING
		
		LDA /INV_8.TEXT_BLOCK.ABORT_TRANSACTION
		STA TWF.STRING+$1
		
	JMP .EXECUTE_PRINT

	
.DEDUCT.GOLD 
;DEDUCT TOTAL PRICE FROM PLAYER GOLD
		LDA PLAYER.GOLD+$0
		SEC
		SBC PROD+$0 ;TOTAL PRICE (LO)
		STA PLAYER.GOLD+$0
		LDA PLAYER.GOLD+$1
		SBC PROD+$1 ;16-bit subtract: TOTAL PRICE (HO)
		STA PLAYER.GOLD+$1
		

		LDA #INV_8.TEXT_BLOCK.ITEM_PURCHASED
		STA TWF.STRING
		
		LDA /INV_8.TEXT_BLOCK.ITEM_PURCHASED
		STA TWF.STRING+$1
		
.EXECUTE_PRINT
	JSR INV_8.PRINT.TEXT.WINDOW		

	;**FALLS THROUGH**

@END


.UPDATE.PLAYER_GOLD_ONSCREEN
	JSR INV_8.PRINT.PLAYER_GOLD_AMOUNT


.EXIT


;**LEFT OVER FROM WHEN I WAS TRYING TO REFRESH THE PLAYER INVENTORY WINDOW BEFORE 
;I DECIDED TO LEAVE IT BLANK WHEN THE MERCHANT WINDOW IS ACTIVE
	; LDA #$01 
	; STA INV_8_4.PLAYER_INV.REFRESH_FLAG	;($00 = refresh mode OFF | >=$01 = refresh mode on). Used when INV_8 wants INV_4 to refresh the player inventory window, without switching menus, after the initial launch. i.e. after an item is purchased. 
	; ;ACC = $01
	; STA INV.SUB_MODULE.LOAD_FLAGS+$8 ;($00 = sub-module not loaded in memory | $01 = sub-module loaded in memory | high-bit set = initial launch complete)
;		
;		
	; ;refresh player inventory data
		; LDA #$00
		; STA INV_4.1b.DISPLAY.MODE
		; LDA #$01 
		; STA INV_8_4.PLAYER_INV.REFRESH_FLAG	;($00 = refresh mode OFF | >=$01 = refresh mode on). Used when INV_8 wants INV_4 to refresh the player inventory window, without switching menus, after the initial launch. i.e. after an item is purchased. 
		; STA INV_4.ACTIVE_PLAYER	
		; LDA #$04	;inventory sub-module # to launch)
	; JMP INV.READ_LAUNCH.SUB_MODULE	
; INV_8.REFRESH_RTS_FROM.INV_4
;
;
	; JSR FLIP.PAGE
;
	; ;(the player and merchant inventory items are currently just on the background page)
	; JSR INV_4.1a.SYNC_PAGES ;sync player inventory window 



.EXIT.ALT		
	JMP INV_8.STATE.LOOP.ALT.ENTRANCE

	
	
@END

INV_8.SELL_ITEM   
@START						
;=====================SUBROUTINE DOCUMENTATION===========================================================================
;
;-Rules
;*selling items with price of $0 is not premitted (this is the mechanic to flag an item as unsellable)
;*selling readied items is not permitted
;*transactions are not permitted that would result in the player having more than !9999 gold
;
;---Technical---
;
;
;-Selling the last unit owned (removes item from the screen)
;This is done by having this routine call a draw routine to plot $00 to a rectangle encompassing both lines of text for the item) and then 
;prints the item text. 
;
;Additionally the readied status byte ($03) of the items INV_4.1b.SCREEN_ITEM_LIST.ARRAY record is set to the deleted record value ($FF)
;This deleted record value causes INV_4.1a.PRINT.SELECTED.ITEM to only draw or erase the item select (i.e but not print the item text) when it
;is called later to manage scrolling through the item list on screen. 
;
;-Selling Readied Items: Spell books
;The same rule applies but enforcing it is a corner case. See "-Spell Items: readied indicator and quantity" in INV_4.1a.VIDEO_OUT.ITEM_LINE1_2 for
;detailed documentation. 
;
;=============================================================================================================================


.INIT
	LDA #$00
	STA INV_8.SELL_ITEM.QTY
	
	JSR INV_8.GET.SELECTED.ITEM.TYPE_ID
		;RETURN: INV_8.SELECTED.ITEM_TYPE, INV_8.SELECTED.ITEM_ID, X-REG (index to SCREEN_ITEM_LIST.ARRAY for the item selected on screen), PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER

;VALIDATE ENTRANCE		
	LDA INV_4.1b.SCREEN_ITEM_LIST.ARRAY+$3,X ;load (readied status | delete status flag). deleted record = $FF
	BPL .VALIDATE.ENTRANCE.DONE
	JMP .EXIT
.VALIDATE.ENTRANCE.DONE

	
.DETECT.KNOWN_ITEM
@START	
;(This routine compares the item the player selected with the merchant's inventory to see if the merchant
;sells this item (/aka a "known" item. Merchants pay more for known items. Based on the outcome of this routine,
;either the known or unknown sell factor (on the merchants profile) will be used to calcualte the price the
;merchant will pay for the item)

	LDY #INV_8.MERCHANT.PROFILE.ITEM_RECORDS_START.OFFSET
.LOOP.SEARCH.MERCHANT_ITEM_RECORDS
	;Y-REG = record byte $0 item_type field
	LDA INV_8.MERCHANT.PROFILE.DATA,Y
	CMP INV_8.SELECTED.ITEM_TYPE
	BNE .NEXT_RECORD
	INY ;advance to record byte $1 item_ID field
	LDA INV_8.MERCHANT.PROFILE.DATA,Y
	CMP INV_8.SELECTED.ITEM_ID
	BEQ .RECORD_FOUND
.NEXT_RECORD
	INY ;record byte $0 item_type field   (record size is $2bytes)
	CPY #INV_8.MERCHANT.PROFILE.NON_ASCII_SIZE
	BNE .LOOP.SEARCH.MERCHANT_ITEM_RECORDS
.RECORD_NOT_FOUND	
	LDA #$00
	STA INV_8.SELL_ITEM.KNOWN_UNKNOWN.FLAG ;($00 = item is unknown | >=$01 = item is known)

	LDA INV_8.MERCHANT.PROFILE.UNKNOWN_SELL_FACTOR
	JMP .DETECT.KNOWN_ITEM.DONE
.RECORD_FOUND
	LDA INV_8.MERCHANT.PROFILE.KNOWN_SELL_FACTOR
	;ACC: expected >=$01
	STA INV_8.SELL_ITEM.KNOWN_UNKNOWN.FLAG ;($00 = item is unknown | >=$01 = item is known)
	
; .SET.KNOWN_UNKNOWN.FLAG
	; ;ACC = unknown/known status
	; STA INV_8.SELL_ITEM.KNOWN_UNKNOWN.FLAG	;($00 = item is unknown | >=$01 = item is known)
	
.DETECT.KNOWN_ITEM.DONE

@END

			; LDA INV_8.SELECTED.ITEM_TYPE
			; STA $BF01
			; LDA INV_8.SELECTED.ITEM_ID
			; STA $BF02
			; LDA #INV_8.MERCHANT.PROFILE.DATA
			; STA $BF03
			; LDA /INV_8.MERCHANT.PROFILE.DATA
			; STA $BF04
			; LDA #INV_8.MERCHANT.PROFILE.NON_ASCII_SIZE
			; STA $BF05
			; LDA #$AA
			; LDX INV_8.SELL_ITEM.KNOWN_UNKNOWN.FLAG ;($00 = item is unknown | >=$01 = item is known)
			; JSR PREP.BRK
			; BRK
			

.CALCULATE.ITEM_SELL_PRICE
@START
	
.GET.ITEM.FINAL_PRICE ;per item		
		;ACC: merchant sell factor (known or unknown) as set above
		STA INV_8.GET.ITEM_FINAL_PRICE.PARM.PERCENT
		;X-REG: set above
	JSR INV_8.GET.ITEM_FINAL_PRICE
		;RETURN: RESULT(2) ;this is the final price for the item with the sell factor applied
		;	   : INV_8.GET.ITEM_BASE_PRICE.PARM.ITEM_TYPE, INV_8.GET.ITEM_BASE_PRICE.PARM.ITEM_ID
		;	   : XREG = (index to INV_4.1b.SCREEN_ITEM_LIST.ARRAY for item)
		;	   : INV_8.GET.ITEM_BASE_PRICE.RETURN.PRICE+$0
	

		LDA RESULT+$0 	;FINAL PRICE (LO)
		STA INV_8.SELL_ITEM.FINAL_PRICE+$0
		LDA RESULT+$1 	;FINAL PRICE (HO)
		STA INV_8.SELL_ITEM.FINAL_PRICE+$1
		
				
@END


.VERIFY.ITEM_SALE.PERMITTED
@START
		LDA INV_8.GET.ITEM_BASE_PRICE.RETURN.PRICE+$0	;item base price (LO)					
		BNE .VERIFY.ITEM_SALE.PERMITTED.DONE
		LDA INV_8.GET.ITEM_BASE_PRICE.RETURN.PRICE+$1	;item base price (HO)					
		BNE .VERIFY.ITEM_SALE.PERMITTED.DONE
		;FINAL PRICE = $00 (item sale is not permitted)
		JMP .ITEM_SALE.NOT_PERMITTED
		

		
.VERIFY.ITEM_SALE.PERMITTED.DONE

@END


.DISPLAY_PRICE
@START
;CONNECT SCREENBUILDER TABLE
		LDA #INV_8.COMMAND_TABLE1.KNOWN_PRICE
		STA SCREEN_BUILDER.COMMAND_TABLE.POINTER+$0
		LDA /INV_8.COMMAND_TABLE1.KNOWN_PRICE
		STA SCREEN_BUILDER.COMMAND_TABLE.POINTER+$1	
		;STA SCREEN_BUILDER.HTAB.LEFT: (VTAB increment/HTAB reset not use, so this parm isn't needed)
		
	LDA INV_8.SELL_ITEM.KNOWN_UNKNOWN.FLAG ;($00 = item is unknown | >=$01 = item is known)
	BNE .DISPAY.KNOWN_PRICE
.DISPAY.UNKNOWN_PRICE
;self-modifying code
;(the only difference between the known and unkown price display is the 1st line of text. The code below modifies
;the address of the text block for the 1st line of text in the SCREENBUILDER command table. There is a small byte
;savings with this approach because a 2nd command table isn't needed as it would be without the self modifying code)
	LDA #INV_8.TEXT_BLOCK.UNKNOWN_SELL_PRICE1
	STA INV_8.DISPLAY_KNOWN_PRICE.TEXT_BLOCK1.LABEL+$0
	LDA /INV_8.TEXT_BLOCK.UNKNOWN_SELL_PRICE1
	STA INV_8.DISPLAY_KNOWN_PRICE.TEXT_BLOCK1.LABEL+$1


	JMP .EXECUTE.SCREENBUILDER
	
.DISPAY.KNOWN_PRICE
;self-modifying code
;(the only difference between the known and unkown price display is the 1st line of text. The code below modifies
;the address of the text block for the 1st line of text in the SCREENBUILDER command table. There is a small byte
;savings with this approach because a 2nd command table isn't needed as it would be without the self modifying code)
	LDA #INV_8.TEXT_BLOCK.KNOWN_SELL_PRICE1
	STA INV_8.DISPLAY_KNOWN_PRICE.TEXT_BLOCK1.LABEL+$0
	LDA /INV_8.TEXT_BLOCK.KNOWN_SELL_PRICE1
	STA INV_8.DISPLAY_KNOWN_PRICE.TEXT_BLOCK1.LABEL+$1

		
.EXECUTE.SCREENBUILDER

	JSR INV_4.1a.RESTORE.TW.CURSOR
	
	JSR SCREEN_BUILDER

	JSR INV_4.1a.SAVE.TW.CURSOR
	


	
@END
	
	

					; stx $bf00
					; lda INV_8.GET.ITEM_BASE_PRICE.PARM.ITEM_TYPE 
					; sta $bf01
					; lda INV_8.GET.ITEM_BASE_PRICE.PARM.ITEM_ID 
					; sta $bf02
					; lda INV_8.GET.ITEM_BASE_PRICE.RETURN.PRICE+$0
					; sta $bf03
					; lda INV_8.GET.ITEM_BASE_PRICE.RETURN.PRICE+$1
					; sta $bf04
					; lda INV_8.GET.ITEM_FINAL_PRICE.PARM.PERCENT
					; sta $bf05
					; LDA #INV_8.MASTER_PRICE_TABLE.ARMOR.START
					; sta $bf06
					; LDA /INV_8.MASTER_PRICE_TABLE.ARMOR.START
					; sta $bf07					
					; ldx prod+$0				
					; ldy prod+$1					

; .TEMP2
			; LDA TEMP
			
.GET.PLAYER.DECISION ;Does player accept mechant's offer? Does player sell all units of this item, or just 1?
@START
			
	JSR KEYIN
		;RETURN: ACC = ascii value of keypress
		
		;ACC: ascii value
	JSR CONVERT.ASCII.UCASE
	STA INV_8.SELL_ITEM.PLAYER.KEYPRESS
	CMP #$D9 ;"Y"es
	BEQ .SELL.SINGLE.ITEM
	CMP #$C1 ;"A"ll (sell all items)
	BEQ .GET.PLAYER.DECISION.DONE
	;default: No. reject merchants offer
	JMP .ABORT_TRANSACTION

	
.SELL.SINGLE.ITEM
	LDA #$01
	STA INV_8.SELL_ITEM.QTY
	
	;**FALLS THROUGH**   (go through the .FINALIZE.ITEM.QTY routine because it also verifies the transaction won't exceed players Max Gold
						;this is kind of shoe horned into the .FINALIZE.ITEM.QTY routine. Setting INV_8.SELL_ITEM.QTY to $01
						;now lets the finalize item qty routine know that we only want it to do the Max Gold check and not modify the sell quantity 
						;if Max Gold won't be exceeded by the transaction)
.GET.PLAYER.DECISION.DONE
@END




.COMPLETE.TRANSACTION
@START


.FINALIZE.ITEM.QTY ;and verify the transaction won't exceed the players max gold
@START
;
;Max Units = Player Gold Capacity / units of item owned 
;	Player Gold Capacity = (max gold - player gold)
;(A)ll quantity = Max Units OR units of item owned by the player, whichever is greater
;
;
;
;
.CALCULATE.GOLD.AVAILABLE
;Player Gold Capacity = (max gold - player gold)
		LDA	#PLAYER.GOLD.MAX
		SEC
		SBC PLAYER.GOLD+$0 
		STA INV_8.PLAYER_GOLD.CAPACITY+$0
		LDA /PLAYER.GOLD.MAX
		SBC PLAYER.GOLD+$1 ;16-bit subtract
		STA INV_8.PLAYER_GOLD.CAPACITY+$1


			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
				; LDA #PLAYER.GOLD.MAX
				; STA $BF00
				; LDA PLAYER.GOLD+$0
				; STA $BF01			
				; LDA INV_8.PLAYER_GOLD.CAPACITY+$0
				; STA $BF02
				
				; LDA /PLAYER.GOLD.MAX	
				; STA $BF03
				; LDA PLAYER.GOLD+$1
				; STA $BF04
				; LDA INV_8.PLAYER_GOLD.CAPACITY+$1
				; STA $BF05
			; lda #$aa 
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP


			
				; LDA #PLAYER.GOLD.MAX
				; STA $BF00
				; LDA PLAYER.GOLD+$0
				; STA $BF01			
				; LDA INV_8.PLAYER_GOLD.CAPACITY+$0
				; STA $BF02
				
				; LDA /PLAYER.GOLD.MAX	
				; STA $BF03
				; LDA PLAYER.GOLD+$1
				; STA $BF04
				; LDA INV_8.PLAYER_GOLD.CAPACITY+$1
				; STA $BF05
				
					; LDA #$AA
					; JSR PREP.BRK
					; BRK
					
					
					
					
.CALCUALTE.MAX_UNITS
@START
		LDA INV_8.PLAYER_GOLD.CAPACITY+$0
		STA DIVIDEND+$0			;number to be divided
		LDA INV_8.PLAYER_GOLD.CAPACITY+$1
		STA DIVIDEND+$1
		
		LDA INV_8.SELL_ITEM.FINAL_PRICE+$0
		STA DIVISOR+$0			;number to divide by
		
		LDA INV_8.SELL_ITEM.FINAL_PRICE+$1		
		STA DIVISOR+$1
	JSR DIV.16				;(dividend/divisor)					
		;RETURN VALUE: result+$0 (quotient LO), result+$1 (quotient HO), result+$2 (remainder LO), result+$3 (remainder HO)
		
		;Max Units = result+$0 (LO byte), result+$1 (LO byte)
		
		;LDY #$00 ;init index to player inventory record
	
		LDA RESULT+$1 ;Max Units (HO)
		BNE .FINAL_QTY.IS.UNITS_OWNED ;if the HO byte !=$0 then we know that Max Units is > units owned because units owned is an 8-bit number
		LDA RESULT+$0 ;Max Units (LO)
		BEQ .PLAYER.CANT_CARRY_MORE_GOLD ;If Max Units = $00 then player would exceed Max Gold if any units were sold
		CMP INV_4.1b.SCREEN_ITEM_LIST.ARRAY+$2,X ;load item available quantity			
		BCC .FINAL_QTY.IS.MAX_UNITS ;is Max Units less than Units Owned?
		
.FINAL_QTY.IS.UNITS_OWNED
		LDA INV_8.SELL_ITEM.QTY ;if sell qty is not $00 (the init value), then player has already decided to 
								;sell 1 unit of the item and the code was only allowed to reach this point to do the Max Gold check
		BNE .FINALIZE.ITEM.QTY.DONE
		
		LDA INV_4.1b.SCREEN_ITEM_LIST.ARRAY+$2,X ;load item available quantity
		STA	INV_8.SELL_ITEM.QTY
		JMP .FINALIZE.ITEM.QTY.DONE
		
.PLAYER.CANT_CARRY_MORE_GOLD
@START
		LDA #INV_8.TEXT_BLOCK.CANT_CARRY_MORE_GOLD
		STA TWF.STRING
		
		LDA /INV_8.TEXT_BLOCK.CANT_CARRY_MORE_GOLD
		STA TWF.STRING+$1
		
	JMP .EXECUTE_PRINT

@END


.ALL.UNITS.READIED
@START
		LDA #INV_8.TEXT_BLOCK.ALL_UNITS_READIED
		STA TWF.STRING
		
		LDA /INV_8.TEXT_BLOCK.ALL_UNITS_READIED
		STA TWF.STRING+$1
		
	JMP .EXECUTE_PRINT

@END
		

.FINAL_QTY.IS.MAX_UNITS
		LDA INV_8.SELL_ITEM.QTY ;if sell qty is not $00 (the init value), then player has already decided to 
								;sell 1 unit of the item and the code was only allowed to reach this point to do the Max Gold check
		BNE .FINALIZE.ITEM.QTY.DONE

		LDA RESULT+$0 ;Max Units (LO)
		STA	INV_8.SELL_ITEM.QTY
.FINALIZE.ITEM.QTY.DONE

		;**FALSS TROUGH**
		
@END
	
	
@END


.READIED_ITEM.CHECK 
@START
;*verify that enough unreadied units of the item are available to support the sell quantity
;i.e. player can't sell readied items.)


			
					
;STANDARD CHECK					
		LDA INV_4.1b.SCREEN_ITEM_LIST.ARRAY+$2,X ;load item available quantity
		
						; sta $bf00
						; lda #INV_4.1b.SCREEN_ITEM_LIST.ARRAY
						; sta $bf01
						; lda #INV_4.1b.SCREEN_ITEM_LIST.ARRAY
						; sta $bf02
						; lda #$aa
						; jsr prep.brk
						; brk
						
		BEQ .ALL.UNITS.READIED
		; CMP INV_8.SELL_ITEM.QTY ;is item available qty >= the sell qty?
		; BCS .READIED_ITEM.CHECK.DONE ;branch if yes
;player doesn't have enough units without selling readied items


.SPELL_MENU.CHECK
@START
;This is a cluge to enforce the "can't sell readied items rule"
;See "-Spell Items: readied indicator and quantity" in INV_4.1a.VIDEO_OUT.ITEM_LINE1_2 for detailed documentation. 



;**OPT** Memory. Could be merged with .GET.SPELL_ITEM.READIED_STATUS to create a common subroutine. Makes sense if INV_4 is less tight on memory than INV_8
;it may be possible to merge everything below .SPELL_MENU.ACTIVE


.VALIDATE.ENTRANCE	
	LDA INV.ACTIVE_MENU.CODE 
	CMP #INV.ACTIVE_MENU.SPELLS ;is spell menu active?
	BNE	.SPELL_MENU.CHECK.DONE
	
	
;SAVE REGISTERS
	TXA ;save INV_4.1b.SCREEN_ITEM_LIST.ARRAY index
	PHA 



.INIT.LOCAL
	;init INV_4.1a.SCREEN_ITEM_LIST.DATA index
		;ACC parm: preset to INV_4.1b.SCREEN_ITEM_LIST.SELECTED.ITEM  ;load selected item number
	JSR INV_4.1a.GET_INDEX.SCREEN_ITEM_LIST.DATA.ALT_ENTRANCE	;set INV_4.1a.SCREEN_ITEM_LIST.DATA index ($20 byte records)
		;RETURN: X-REG = INV_4.1a.SCREEN_ITEM_LIST.DATA index
		
;IS SPELLBOOK READIED?
	LDY #$00 ;init loop counter & PLAYER.HOTKEYS.SPELLS index
.LOOP.SPELL_MENU.IS.READIED	
	LDA PLAYER.HOTKEYS.SPELLS,Y
	CMP INV_4.1a.SCREEN_ITEM_LIST.DATA+INV.ITEM_TABLE.MI.SPELL_CODE.OFFSET,X ;load spell code field for current item
	BEQ .SPELL_BOOK.IS_READIED
	INY
	CPY #PLAYER.HOTKEYS.SPELLS.ARRAY_SIZE
	BNE .LOOP.SPELL_MENU.IS.READIED
	;if loop terminates without a match, then spell is not readied
	JMP .SPELL_BOOK.NOT_READIED

.SPELL_BOOK.IS_READIED
	PLA
	TAX ;restore INV_4.1b.SCREEN_ITEM_LIST.ARRAY

;CORNER CASE RULES: SPELL BOOK IS READIED
;(if the player only 1 unit, then abort the sale. If the player owns more than one and the 
;player selected "Sell All", deduct 1 from the "Sell All" quantity)	
	LDA INV_4.1b.SCREEN_ITEM_LIST.ARRAY+$2,X ;load item available quantity
	CMP #$01 ;does player only own 1 unit of this item? 
	BEQ .ALL.UNITS.READIED ;if yes, then all units are readied. 
	;is player trying to sell all available units of the item? 
	LDA INV_8.SELL_ITEM.QTY	
	CMP #$01
	BEQ .SPELL_BOOK.IS_READIED.DONE ;branch, if no
	;if yes (one units must be deducted to retain the readed spell book)
	DEC INV_8.SELL_ITEM.QTY

.SPELL_BOOK.IS_READIED.DONE
	TXA
	PHA ;put INV_4.1b.SCREEN_ITEM_LIST.ARRAY back on the stack to avoid a JMP over .SPELL_MENU.CHECK.EXIT
	
.SPELL_BOOK.NOT_READIED	

.SPELL_MENU.CHECK.EXIT	
;RESTORE REGISTERS
	PLA
	TAX ;restore INV_4.1b.SCREEN_ITEM_LIST.ARRAY index

.SPELL_MENU.CHECK.DONE
	
@END


.READIED_ITEM.CHECK.DONE

					

@END
	
.CALCULATE.TOTAL_PRICE ;for the quantity of items the player wants to sell 
@START

				

			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
				; LDA INV_8.PLAYER_GOLD.CAPACITY+$0
				; STA $BF00
				; LDA INV_8.PLAYER_GOLD.CAPACITY+$1
				; STA $BF01
				
				; LDA INV_8.SELL_ITEM.FINAL_PRICE+$0
				; STA $BF02
				
				; LDA INV_8.SELL_ITEM.FINAL_PRICE+$1		
				; STA $BF03
				; LDA RESULT+$0 ;Max Units (LO)
				; STA $BF04
				; LDA RESULT+$1 ;Max Units (HO)
				; STA $BF05
	

				; LDA INV_4.1b.SCREEN_ITEM_LIST.ARRAY+$2,X ;load item available quantity
				; STA $BF06
				; LDA INV_8.SELL_ITEM.QTY
				; STA $BF07	

			; lda #$aa 
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP
			
			

							; lda #$01
							; sta troubleshooting.hook
							
					
		LDA INV_8.SELL_ITEM.FINAL_PRICE+$0
		STA RESULT+$0 	;FINAL PRICE (LO)
		LDA INV_8.SELL_ITEM.FINAL_PRICE+$1
		STA RESULT+$1 	;FINAL PRICE (HO)
		;INV_8.SELL_ITEM.QTY: already set
	JSR INV_8.GET.TOTAL_PRICE
		;RETURN VALUE: PROD(2) ;TOTAL PRICE	

		
					; LDA INV_8.SELL_ITEM.FINAL_PRICE+$0
					; STA $BF00
					; LDA INV_8.SELL_ITEM.FINAL_PRICE+$1
					; STA $BF01
					; LDA INV_8.SELL_ITEM.QTY		
					; STA $BF02
					; LDA #$AA
					; JSR PREP.BRK
					; BRK
					
@END
	
.ADD.GOLD ;COMPLETE TRANSACTION
;ADD TOTAL PRICE TO PLAYER GOLD
		LDA PLAYER.GOLD+$0
		CLC
		ADC PROD+$0 ;TOTAL PRICE (LO)
		STA PLAYER.GOLD+$0
		LDA PLAYER.GOLD+$1
		ADC PROD+$1 ;16-bit add: TOTAL PRICE (HO)
		STA PLAYER.GOLD+$1
		
.DEDUCT.QTY.SOLD ;deduct from inventory and item screen list array
@START

.UPDATE.SCREEN_LIST
@START
;Deduct sell quantity from INV_4.1b.SCREEN_ITEM_LIST.ARRAY
	LDA INV_4.1b.SCREEN_ITEM_LIST.ARRAY+$2,X ;load item available quantity
	SEC
	SBC INV_8.SELL_ITEM.QTY
	STA INV_4.1b.SCREEN_ITEM_LIST.ARRAY+$2,X ;save item available quantity
				
;Update video screen
		LDA #$7F ;set mode ($00 = item selector OFF | $7F = item selector ON) 
		;LDA #$00 ;set mode ($00 = item selector OFF | $7F = item selector ON)
		;INV_4.1b.SCREEN_ITEM_LIST.SELECTED.ITEM: use existing value
		SEC ;(CLC = don't sync hi-res pages | SEC = sync hi-res pages)
	JSR INV_4.PRINT.SELECTED.ITEM

@END

.UPDATE.PLAYER_INVENTORY
@START
.GET.RECORD.POINTER
		JSR INV_8.INIT.PLAYER_INVENTORY.POINTER ;set INV.PLAYER.INVENTORY.DATA.POINTER to top of player inventory table (INV.PLAYER.INVENTORY.DATA)
	JSR INV_8.SEARCH.PLY_INV
		;RETURN: PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER(2)

;REMOVE SOLD ITEM(S) FROM PLAYER'S INVENTORY RECORD
		
	;save quantity field (byte $02)
	LDY #$02
	LDA (PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER),Y ;load item quantity from existing inventory record
	SEC
	SBC INV_8.SELL_ITEM.QTY ;load item_id
	;ACC = item quantity owned after sell quantity is subtracted
	STA (PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER),Y ;save item adjusted quantity to existing inventory record
	BNE .REMOVE_ITEM.DONE
	
.REMOVE_ITEM_FROM_SCREEN ;no more units owned	
;Removed the item from the video screen and sets a deleted flag to ensure it isn't printed again.
;If the player scrolls through the list enough the item will scroll out of INV_4.1b.SCREEN_ITEM_LIST.ARRAY 
;and the deleted record flag ensures the deleted item's text won't be displayed in the meantime. 


;SET DELETED RECORD VALUE
	LDA #$FF ;deleted record value
	STA INV_4.1b.SCREEN_ITEM_LIST.ARRAY+$3,X ;set readied flag to indicate delete record


;REMOVE ITEM FROM VIDEO SCREEN	
	JSR INV_4.1a.SYNC_PAGES.PLAYER_INV_WINDOW 				

		LDA #INV_4.SCREEN_LIST.ITEM_ALL.HTAB.LEFT
		STA INV_4.1b.SCREEN_LIST.ITEM_ALL.HTAB.LEFT
		LDA #INV_4.SCREEN_LIST.ITEM_ALL.HTAB.RIGHT+1
		STA INV_4.1b.SCREEN_LIST.ITEM_ALL.HTAB.RIGHT
		LDX INV_4.1b.SCREEN_ITEM_LIST.SELECTED.ITEM	
		LDA #$7F
		STA COUT_CHAR_TYPE ;($00 = normal, $7F = inverse)
	JSR INV_4a.ERASE.ITEM_SELECTOR
		LDA #$00 ;reset to default
		STA COUT_CHAR_TYPE ;($00 = normal, $7F = inverse)

	
.REMOVE_ITEM.DONE
	JSR FLIP.PAGE
	JSR INV_4.1a.SYNC_PAGES.PLAYER_INV_WINDOW 				
					

				
@END
@END

	
.PRINT.TRANSACTION_COMPLETION
		LDA #INV_8.TEXT_BLOCK.ITEM_SOLD
		STA TWF.STRING
		
		LDA /INV_8.TEXT_BLOCK.ITEM_SOLD
		STA TWF.STRING+$1
		
.EXECUTE_PRINT
	JSR INV_8.PRINT.TEXT.WINDOW		

	;**FALLS THROUGH**

@END


.UPDATE.PLAYER_GOLD_ONSCREEN
	JSR INV_8.PRINT.PLAYER_GOLD_AMOUNT


	
.EXIT
				; pla
				; tax			
				; pla
				; tay
				; lda #$aa
				; jsr prep.brk
				; brk


				
	RTS
	
	
.ABORT_TRANSACTION
		LDA #INV_8.TEXT_BLOCK.ABORT_TRANSACTION
		STA TWF.STRING
		
		LDA /INV_8.TEXT_BLOCK.ABORT_TRANSACTION
		STA TWF.STRING+$1
		
	JMP .EXECUTE_PRINT
	
		
.ITEM_SALE.NOT_PERMITTED
		LDA #INV_8.TEXT_BLOCK.ITEM_SALE.NOT_PERMITTED
		STA TWF.STRING
		
		LDA /INV_8.TEXT_BLOCK.ITEM_SALE.NOT_PERMITTED
		STA TWF.STRING+$1
		
	JMP .EXECUTE_PRINT


	
@END


;TEXT
INV_8.DISPLAY_MERCHANT.INVENTORY
@START

				
.INIT.GENERAL
@START
	JSR INV_4.1a.INIT.SCREEN_LIST.ARRAYS ;init INV_4.1b.SCREEN_ITEM_LIST.ARRAY(x), INV_4.1b.SCREEN_ITEM_LIST.SELECTED.ITEM(x), and PLAYER_MERCH.INVENTORY.DATA.POINTER(2)

	;set inventory pointer to the 1st item record
	LDA #INV_8.MERCHANT.PROFILE.DATA+INV_8.MERCHANT.PROFILE.ITEM_RECORDS_START.OFFSET
	STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
	
	LDA /INV_8.MERCHANT.PROFILE.DATA+INV_8.MERCHANT.PROFILE.ITEM_RECORDS_START.OFFSET
	STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
	
	LDA #INV_8.MERCHANT.PROFILE.ITEM_RECORDS_START.OFFSET-2 ;-2 because .EXIT_TEST1 is at the top of the loop and it's 1st increment will offset the -2
	STA INV_8.BYTE_COUNTER ;counts the byte position of PLAYER_MERCH.INVENTORY.DATA.POINTER in INV_8.MERCHANT.PROFILE.DATA
	
	LDA #$00 ;(must not be $FF as that is the init value, which is used for stop value detection)
	STA INV_4.NEXT_ITEM.READIED_STATUS ;only used in player inventory, but exepected by INV_4.1a.SAVE_ITEM, so set to $00. ($00 = not readied | $01 = readied by active player | $02 = readied twice by active player)
	STA INV_4.1b.NEXT_ITEM.QTY_AVAILABLE ;only used in player inventory, but exepected by INV_4.1a.SAVE_ITEM, so, set to $00. 
@END
	
					
.INIT.IDENTIFY_ITEMS
@START	
	LDY #$00 ;init loop counter (on screen item counter)
	LDX #$00 ;init index to INV_4.1b.SCREEN_ITEM_LIST.ARRAY (4 byte records)
.LOOP.GET.ITEMS

.EXIT_TEST1
	;has end of merchant profile record been reached?
	LDA INV_8.BYTE_COUNTER ;tracks the offset of the selected item's record in INV_8.MERCHANT.PROFILE.DATA. used to determine if INV_8.GET.NEXT_ITEM has reached top/bottom of merchant profile's item array. 
	CLC
	ADC #$02
	STA INV_8.BYTE_COUNTER ;tracks the offset of the selected item's record in INV_8.MERCHANT.PROFILE.DATA. used to determine if INV_8.GET.NEXT_ITEM has reached top/bottom of merchant profile's item array. 
;	CMP INV_8.MERCHANT.PROFILE.RECORD_SIZE
	CMP INV_8.MERCHANT.PROFILE.NON_ASCII_SIZE
	BCS .LOOP.GET.ITEMS.DONE
	
	

		;PLAYER_MERCH.INVENTORY.DATA.POINTER = already set
		;INV_4.NEXT_ITEM.READIED_STATUS = already set
		;INV_4.1b.NEXT_ITEM.QTY_AVAILABLE = already set
	JSR INV_4.1a.SAVE_ITEM ;saves item

	;**FALLS THROUGH**

.INCREMENT.COUNTERS_INDEXES
	;advance inventory pointer to next merchant item record
	LDA PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
	CLC
	ADC #INV_8.MERCHANT.PROFILE.ITEM_DATA.RECORD_SIZE
	STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
	
	LDA PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
	ADC #$00 ;16-bit add
	STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
	

	;increment INV_4.1b.SCREEN_ITEM_LIST.ARRAY index
	INY ;increment loop counter (on screen item counter)
	TYA
	ASL ;X2
	ASL ;X4
	TAX ;save incremented index to INV_4.1b.SCREEN_ITEM_LIST.ARRAY (4 byte records)

	

	
	;do we already have the max items that will fit onscreen?
	CPY #INV_8.MAX_ONSCREEN.ITEMS
	BNE .LOOP.GET.ITEMS
.LOOP.GET.ITEMS.DONE
	DEY ;reverse last loop counter increment so that INV_4.1b.SCREEN_LIST.TOTAL_RECORDS values will be 0-2
;.LOOP.GET.ITEMS.DONE.ALT
	STY INV_4.1b.SCREEN_LIST.TOTAL_RECORDS ;total items on screen. If > #INV_4.MAX_ONSCREEN.ITEMS that means less than #INV_4.MAX_ONSCREEN.ITEMS in the party inventory are relevant for display to the active player

	LDA #INV_8.MERCHANT.PROFILE.ITEM_RECORDS_START.OFFSET
	STA INV_8.BYTE_COUNTER ;counts the byte position of PLAYER_MERCH.INVENTORY.DATA.POINTER in INV_8.MERCHANT.PROFILE.DATA

					; lda #$aa
					; ; ldx #INV_4.1b.SCREEN_ITEM_LIST.ARRAY
					; ; ldy /INV_4.1b.SCREEN_ITEM_LIST.ARRAY
					; LDX INV_8.BYTE_COUNTER
					; jsr prep.brk
					; brk
					

					; JSR KEYIN
					; JSR FLIP.PAGE ;view background page
					; JSR KEYIN
					; JSR FLIP.PAGE ;restore foreground page
			
			
@END




.INIT.GET.ITEM.DATA

		LDA #INV_8.MAX_ONSCREEN.ITEMS ;variable, not a constant
	JSR INV_4.1a.INIT.GET.ITEM.DATA

	

					
INV_8.INIT.DISPLAY.ALL_ITEMS
		LDA #INV_8.SCREEN_LIST.ITEM_ALL.HTAB.LEFT
		STA INV_4.1b.SCREEN_LIST.ITEM_ALL.HTAB.LEFT
		
		LDA #INV_8.MAX_ONSCREEN.ITEMS
		STA INV_4.1b.MAX_ONSCREEN.ITEMS	
		
						
	JSR INV_4.1a.PRINT.ALL.ITEMS ;print all items visible on the video screen


	

					
.UPDATE.VIDEO_SCREEN
	
	
					

				
		LDA #$7F ;set mode ($00 = item selector OFF | $7F = item selector ON) 
		CLC ;set sync mode (CLC = don't sync hi-res pages | SEC = sync hi-res pages)
	JSR INV_8.DRAW_ERASE.ITEM_SELECTOR

	JSR FLIP.PAGE

	;(the player and merchant inventory items are currently just on the background page)
	JSR INV_4.1a.SYNC_PAGES.PLAYER_INV_WINDOW ;sync player inventory window 
	JSR INV_8.SYNC_PAGES.MERCHANT_INV_WINDOW ;sync merchant inventory window 

	

				

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
	
	RTS
	




@END

INV_8.DISPLAY_READIED.INDICATOR ;**OPT** Memory. This routine could be located in INV_4 if memory in INV_8 is tighter than in INV_4
@START


			
;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA
	
	
	
	LDA INV_4.1b.SCREEN_ITEM_LIST.ARRAY+$0,X ;load inventory record LO address
	STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
	
	LDA INV_4.1b.SCREEN_ITEM_LIST.ARRAY+$1,X ;load inventory record HO address
	STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$1

	
				;INC TROUBLESHOOTING.HOOK
		
;GET READIED QTY	
	JSR INV.GET.READIED.QTY 
		;RETURN VALUE: INV.RE.READIED.QTY (the number of times the ITEM-TO-READY is already readied by all PCs, including active PC)

	
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; ; LDA PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
			; ; CMP #$12
			; ; BNE .TEMP
			; ; LDA PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
			; ; CMP #$BA
			; ; BNE .TEMP
			; JSR FLIP.PAGE
			; JSR KEYIN
			; LDA #$AA
			; LDX INV.RE.READIED.QTY
			; JSR PREP.BRK
			; BRK
			
; .TEMP


			
;PRINT READIED INDICATOR
	LDX #$00

.LOOP.PRINT.READIED_INDICATOR
	CPX INV.RE.READIED.QTY
	BCS .LOOP.PRINT.READIED_INDICATOR.DONE
	
		LDA #$AA 
	JSR INV_4.CALL.COUT.BACKGROUND		;print first *
	
	INX
	JMP .LOOP.PRINT.READIED_INDICATOR
.LOOP.PRINT.READIED_INDICATOR.DONE




			
			; JSR FLIP.PAGE
			; JSR KEYIN
			; CMP #$CD
			; BNE .TEMP
			; LDA #$AA
			; LDX INV.RE.READIED.QTY
			; JSR PREP.BRK
			; BRK
; .TEMP
			
			
	;**FALLS THROUGH**
	
.EXIT
;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX
		
	RTS


@END

INV_8.DRAW_ERASE.SCROLL_WINDOW
@START	
;PARAMETERS: DRAW.START_LINE, DRAW.START_BYTE

	LDA #TWB.MERCHANT_TRANS.SCROLL_WINDOW.TOP_LINE	
	STA DRAW.START_LINE
	
	LDA #TWB.MERCHANT_TRANS.SCROLL_WINDOW.LEFT_SBYTE	
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

INV_8.DRAW_ERASE.ITEM_SELECTOR
@START	
;PARAMETERS: ACC = mode ($00 = item selector OFF | $7F = item selector ON), carry flag (CLC = don't sync hi-res pages | SEC = sync hi-res pages)

;SAVE PARAMETERS

	;ACC = parm: mode ($00 = item selector OFF | $7F = item selector ON)
	STA TEMP
		
.INIT
	;carry flag (CLC = don't sync hi-res pages | SEC = sync hi-res pages)
	BCC .SYNC.DONE

	JSR INV_8.SYNC_PAGES.MERCHANT_INV_WINDOW ;sync merchant inventory window
	
.SYNC.DONE


	
;DISPLAY ITEM SELECTOR
		LDA #INV_8.SCREEN_LIST.ITEM_ALL.HTAB.LEFT
		STA INV_4.1b.SCREEN_LIST.ITEM_ALL.HTAB.LEFT
		LDA #INV_8.SCREEN_LIST.ITEM_ALL.HTAB.RIGHT+1
		STA INV_4.1b.SCREEN_LIST.ITEM_ALL.HTAB.RIGHT
			
		LDA TEMP ;restore ACC parm: mode ($00 = item selector OFF | $7F = item selector ON)
		CLC ;parm for INV_4.1a.PRINT.SELECTED.ITEM: (CLC = don't sync hi-res pages | SEC = sync hi-res pages)
	JSR INV_4.1a.PRINT.SELECTED.ITEM		


	;JSR FLIP.PAGE
	
						

.EXIT
		RTS
@END					

INV_8.PRINT.ITEM_LIST.PRICE
@START
;PARAMETERS: X-REG (index to INV_4.1b.SCREEN_ITEM_LIST.ARRAY for item to print)
;ENTRANCE: any INV_8 or INV_4 routine
;RETURN: item price printed on the video screen, INV_8.GET.ITEM_BASE_PRICE.PARM.ITEM_TYPE, INV_8.GET.ITEM_BASE_PRICE.PARM.ITEM_ID



		LDA INV_8.MERCHANT.PROFILE.BUY_FACTOR
		STA INV_8.GET.ITEM_FINAL_PRICE.PARM.PERCENT
	JSR INV_8.GET.ITEM_FINAL_PRICE
		;RETURN: RESULT(2) ;this is the final price for the item with the buy factor applied
	
.PRINT.ITEM_PRICE	
;UPDATE CURSOR POSITION
		LDA #INV_8.SCREEN_LIST.ITEM_ALL.PRICE_HTAB
		STA HTAB	
	JSR	UPDATE.CHAR.POS
	
	
;PRINT	

		;set PRINT.HEX16.RJ parms
		LDA RESULT+$0
		STA BIN+$0
		LDA RESULT+$1
		STA BIN+$1
		
		LDA PAGE.BACKGROUND
		STA HRCG.PAGES	;($01 = hi-res page 1, $02 = hi-res page 2, $03 = both pages)
		
		;restore COUT_CHAR_TYPE parm (might be set to $7F depending on the path taken to call this routine)
		LDA COUT_CHAR_TYPE.SAVED
		STA COUT_CHAR_TYPE ;($00 = normal, $7F = inverse) 
		
		CLC ;(CLC = don't print CR | SEC = print CR to text window)
	JSR PRINT.HEX16.RJ	
	


		
.EXIT

	
	RTS
@END

INV_8.PRINT.PLAYER_GOLD_AMOUNT
@START	
		LDA #INV_8.PLAYER_GOLD_AMOUNT.HTAB
		STA HTAB	
		LDA #INV_8.PLAYER_GOLD_AMOUNT.VTAB
		STA	VTAB
	JSR	UPDATE.CHAR.POS
		
		
		;set PRINT.HEX16.RJ	parms
		LDA PLAYER.GOLD+$0
		STA BIN+$0
		LDA PLAYER.GOLD+$1
		STA BIN+$1
		
		; LDA #$03
		; STA HRCG.PAGES	;($01 = hi-res page 1, $02 = hi-res page 2, $03 = both pages)
		
		CLC ;(CLC = don't print CR | SEC = print CR to text window)
	JSR PRINT.HEX16.RJ	
	
	RTS
@END

INV_8.PRINT.TEXT.WINDOW
@START
;PARAMETERS: TWF.STRING(2)
;ENTRANCE: INV_8 and INV_4.1 routines
;RETURN: text printed to video screen in scroll window

	JSR INV_4.1a.RESTORE.TW.CURSOR
	
		;TWF.STRING(2): passed to this routine as parm
	JSR PRINT.TEXT.WINDOW

	
	JSR INV_4.1a.SAVE.TW.CURSOR
	
	RTS
@END

	

;GRAPHICS
INV_8.SYNC_PAGES.MERCHANT_INV_WINDOW
@START
		LDA #$15
		STA DRAW.START_BYTE

		LDA #$27
		STA DRAW.STOP_BYTE.ALT
	JSR INV_4.1a.SYNC_PAGES ;sync player inventory window
	
.EXIT
	RTS
	
@END



INV_8.ERASE.MERCHANT_INVENTORY.WINDOW.TEXT_SPACE
@START
;erases the merchant inventory items text (background hi-res page only)

		LDA #TWS.LW.MERCH_INV.TOP_LINE		
		STA DRAW.START_LINE
		
		LDA #TWS.LW.MERCH_INV.LEFT_SBYTE	
		STA DRAW.START_BYTE
		
		LDA #TWS.LW.MERCH_INV.RIGHT_SBYTE
		STA DRAW.STOP_BYTE
			
		LDA #TWS.LW.MERCH_INV.BOTTOM_LINE
		STA DRAW.STOP_LINE	
	JSR INV.GENERIC_BACKGROUND_ERASE_ROUTINE

	RTS
	
@END

;MATH
INV_8.APPLY.PERCENTAGE.HEX_ONLY ;supports large base numbers. Tested up to !9999
@START
;PARAMETERS: INV_8.APPLY_PERCENT.PARM.PERCENT, INV_8.APPLY_PERCENT.PARM_BASE(2)
;ENTRANCE: INV_8 and INV_4.1 routines
;RETURN: RESULT(2)

;=====================SUBROUTINE DOCUMENTATION====================================
;
;(to avoid using floating decimals, this routine applies the percent using the
;the formula: %(as integer) * number / 100, which yields the same result as number * percent (as decimal)
;So instead of .75 * 50, this routine would calculate 75*50/100
;
;
;This routine is different than COMBAT.STATS.APPLY.PERCENTAGE:
;	*COMBAT.STATS.APPLY.PERCENTAGE base numbers must be fairly small. The base number * M% cannot exceed a 16-bit number (!65535)
;	*This routine uses MLP.32 and DIV.32 to, in theory, support a 16-bit base number up to !65535*
;	*this routine doesn't support BCD and COMBAT.STATS.APPLY.PERCENTAGE because MLP.32 doesn't support BCD. 
;
;*I've only tested base numbers up to !9999. I tried !20000 and got a garbage result, I'm not sure why. 
;
;
;=================================================================================


;Formula: RESULT(2) = base number * M% / 100


		LDA INV_8.APPLY_PERCENT.PARM.PERCENT
		STA MULR+$0
		LDA #$00
		STA MULR+$1
		STA MULR+$2
		STA MULR+$3
		STA MULND+$2
		STA MULND+$3		
		;			
		LDA INV_8.APPLY_PERCENT.PARM_BASE+$0
		STA MULND+$0
		LDA INV_8.APPLY_PERCENT.PARM_BASE+$1
		;combat only: AND #$7F ;strip off high-bit (see .INIT.TO_HIT.TALLY for docs on the special use of the high-bit for this variable)
		STA MULND+$1		
		;MULND+$2: set above
		;MULND+$3: set above
		;SED (set decimal mode is done above)
		
		
			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; LDA INV_8.APPLY_PERCENT.PARM.PERCENT
			; sta $bf00
			; lda #MULR
			; sta $bf01
			; lda /MULR
			; sta $bf02
			; lda #MULND
			; sta $bf03
			; lda /MULND
			; sta $bf04
			; ;			
			; LDA INV_8.APPLY_PERCENT.PARM_BASE+$0
			; sta $bf05
			; LDA INV_8.APPLY_PERCENT.PARM_BASE+$1					
			; sta $bf06

; .TEMP
			; LDA TEMP
			
			
			
	JSR MLP.32 ;hex only
		;RETURN VALUE: PROD(8)
		;CLD (clear decimal mode is done below)
		LDA PROD+$0 ;load multiplication return value LO byte
		STA DIVIDEND+$0
		LDA PROD+$1 ;load multiplication return value HO byte
		STA DIVIDEND+$1	
		LDA PROD+$2 ;load multiplication return value HO byte
		STA DIVIDEND32+$0
 
 			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP2

			; LDA #PROD
			; sta $bf07
			; lda /prod
			; sta $bf08

		
					; ; stx $bf00
					; ; lda INV_8.GET.ITEM_BASE_PRICE.PARM.ITEM_TYPE 
					; ; sta $bf01
					; ; lda INV_8.GET.ITEM_BASE_PRICE.PARM.ITEM_ID 
					; ; sta $bf02
					; ; lda INV_8.GET.ITEM_BASE_PRICE.RETURN.PRICE+$0
					; ; sta $bf03
					; ; lda INV_8.GET.ITEM_BASE_PRICE.RETURN.PRICE+$1
					; ; sta $bf04
					; ; lda INV_8.GET.ITEM_FINAL_PRICE.PARM.PERCENT
					; ; sta $bf05
					; ; LDA #INV_8.MASTER_PRICE_TABLE.ARMOR.START
					; ; sta $bf06
					; ; LDA /INV_8.MASTER_PRICE_TABLE.ARMOR.START
					; ; sta $bf07					
					; ; ldx prod+$0				
					; ; ldy prod+$1					
					; lda #$aa
					; ldx RESULT
					; ldy RESULT

					; JSR PREP.BRK
					; BRK
; .TEMP2
			; LDA TEMP
			

			
 
 
 
		LDA #$64	;!100
		STA DIVISOR+$0
		STA DIV.16.PARM.MODE ;($00 = 16-bit dividend mode | >=$01 = 32-bit dividend mode)
		LDA #$00
		STA DIVISOR+$1
		STA DIVIDEND32+$1
		
		
			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			
			; lda #$32
			; sta DIVIDEND+$0
			; lda #$64
			; sta DIVIDEND+$1
			; lda #$00
			; STA DIVIDEND32+$0


			; LDA #$64	;!100
			; STA DIVISOR+$0
			; STA DIV.16.PARM.MODE ;($00 = 16-bit dividend mode | >=$01 = 32-bit dividend mode)
			; LDA #$00
			; STA DIVISOR+$1
			; STA DIVIDEND32+$1
		
			; ; lda #DIVIDEND
			; ; sta $bf00
			; ; lda /DIVIDEND
			; ; sta $bf01
			; ; lda #DIVIDEND32
			; ; sta $bf02
			; ; lda /DIVIDEND32
			; ; sta $bf03
			; ; ;			
			; ; LDA #DIVISOR
			; ; sta $bf04
			; ; LDA /DIVISOR				
			; ; sta $bf05
			; ; ;
			; ; LDA #PROD
			; ; sta $bf06
			; ; lda /prod
			; ; sta $bf07
			
					; ; lda #$aa
					; ; ldx RESULT+$0
					; ; ldy RESULT+$1

					; ; JSR PREP.BRK
					; ; BRK

; .TEMP
			; LDA TEMP
			
			
			
			
	JSR DIV.32 ;hex division routine
		;RETURN: RESULT(x)
		
	;**FALLS THROUGH**

	


			
.EXIT
			
	RTS
@END	


@END

	
;LOCAL TEXT BLOCKS
@START

;see ";text blocks" in INV_8.1


@END


;LOCAL VARIABLES
@START

;**OPT** Memory. If more variable space is needed, the ascii intro text isn't needed after initial launch. It should start no later than SHARED.VARIABLE_SPACE.BLOCK2+$30, which reflects 6 header bytes and !20 item records using 2 bytes each.
;however, keep in mind that INV_8.MERCHANT_ID.TABLE shares memory with INV_8.MERCHANT.PROFILE.DATA
INV_8.MERCHANT.PROFILE.DATA		.EQ SHARED.VARIABLE_SPACE.BLOCK2+$00	;I don't think I'd ever need more than $B, which would reflect a max # of merchant items of 20 and max length of intro text of !134. Unless I expanded it, the profile table is already setup for 20 items and it is unlikely that any intro text uses more than !134. 
;datagram: record size, map_code, NPC Record #, non-ascii size, merchant_type, buy_factor, known sell_factor, unknown sell_factor [item_type, item_code ..] (up to !20 item records)

						; ;==IN USE==								+$F1	;+F2 thru +FF is used by PRODOS.IO
						

INV_8.MERCHANT.PROFILE.ITEM_DATA.RECORD_SIZE		.EQ $02 ;#CONSTANT ;the size of each record that stores the item_type and item_id for a given item in the merchant's inventory. 
INV_8.MERCHANT.PROFILE.ITEM_RECORDS_START.OFFSET	.EQ	$08	;#CONSTANT


;!!!!WARNING: I'm seeing a problem with INV.DATA.MERCHANT_PROFILE getting clobbered 
;as of INV.DATA.MERCHANT_PROFILE+$70, which only affects the ASCII intro text (displayed once) 
;stored in the profile.


	
;***THESE ARE NOT CONSTANTS: don't use # prefix
INV_8.MERCHANT.PROFILE.RECORD_SIZE				.EQ INV_8.MERCHANT.PROFILE.DATA+$0
INV_8.MERCHANT.PROFILE.MAP_CODE					.EQ INV_8.MERCHANT.PROFILE.DATA+$1
INV_8.MERCHANT.PROFILE.NPC_RECORD				.EQ INV_8.MERCHANT.PROFILE.DATA+$2
INV_8.MERCHANT.PROFILE.NON_ASCII_SIZE			.EQ INV_8.MERCHANT.PROFILE.DATA+$3	;the size of the non-ascii portion of merchant's profile record, which contains the header fields and inventory item fields
INV_8.MERCHANT.PROFILE.MERCHANT_TYPE			.EQ INV_8.MERCHANT.PROFILE.DATA+$4
INV_8.MERCHANT.PROFILE.BUY_FACTOR				.EQ INV_8.MERCHANT.PROFILE.DATA+$5
INV_8.MERCHANT.PROFILE.KNOWN_SELL_FACTOR		.EQ INV_8.MERCHANT.PROFILE.DATA+$6
INV_8.MERCHANT.PROFILE.UNKNOWN_SELL_FACTOR		.EQ INV_8.MERCHANT.PROFILE.DATA+$7

;*****see INV_8.1
;INV_8.1.MASTER_PRICE_TABLE.DATA										
;datagram: (sell_price_lo, sell_price_ho)
								
;INV_8.MASTER_PRICE_TABLE.RECORD_SIZE	.EQ	$02 ;#CONSTANT


@END

;LOCAL SHAPE TABLES
@START

;violet
;MENU_ICON7.MERCHANT_INV			.HS	20.15.28.15.28.11.28.05.28.05.2C.0D.6C.0D.78.07.EC.8D.FC.8F.D8.87.F8.87.B0.86.F0.83.E0.83.C0.81	

;**OPT** Memory. Load the shape data into INV_8.MERCHANT.PROFILE.DATA in .INIT.READ.MERCHANT_PROFILE_DATA just before
;the merchant profile is read in. Then execute the draw and let the merchant profile clobber the shape table data since it is only needed once per session. 
;In fact, any code in the merchant transactions init only needed once per session (like drawing the menu selector) could be read from disk, used, then clobbered. 

MENU_ICON7.MERCHANT_INV			.HS	D0.AA.D0.AA.D4.A2.D4.8A.D4.8A.D4.8A.D4.8B.F8.87.EC.8D.FC.8F.D8.87.F8.87.B8.86.F8.87.F0.83.E0.81


@END


