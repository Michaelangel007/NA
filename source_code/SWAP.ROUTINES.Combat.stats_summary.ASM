; ============================================
; Copyright (C) 2016-2018. 6502 Workshop, LLC
; contact: mark@6502workshop.com
; ============================================



;************************INCLUDE FILE*****************************
;(do not assemble stand-alone.  )


;=====================SUBROUTINE DOCUMENTATION====================================
;
;This subroutine exists in both the combat module and the inventory module in order 
;to reduce main memory usage. This file is an include file in both modules so that this
;file is the source code for both, to avoid managing two code sets. The only implication of that is no parent labels can be
;defined within this routine. 
;
;The version in the inventory module is used when inventory is called when combat is not loaded. 
;
;
;=================================================================================


;****************OVERHAUL **OPT** PLAN USING CONTROL TABLE******
;**OPT** Memory
;
;The concept is to write an interpreter that would read a control table, which would contain the codes
;the interpreter needs to print text and variables to the screen.
;
;An outline of the control table and the print variable subroutines it would need to have wrappers for is below.
;The concept is that each control records could contain a series of print codes, each followed by the necessary fields to provide the
;the print wrappers within interpreter with the info they need. The wrapper would advance the pointer to the next print code and using the 
;control record size value, detect the end of the control record and initiate the processing of the next control record. 
;using control table size value to detect end of table and terminate the loop.  
;
;Note: 
;.RACE uses a lookup table method
;In theory, .HEALTH_STATUS, .GENDER, could be converted to a lookup table method. 
;
;If that was done maybe the control table could support a lookup table (might need an extra optional field for the lookup table address, but all records wouldn't need it becuase the control record
;pointer would get advanced in the print wrapper routines, which would know to expect the optional field before the next print command. 
;
;
;--MEMORY SAVINGS ESTIMATE
;
;If the lookup table method isn't supported the code that would be replaced is from .LEVEL to .RH.WEAPON.NAME,
;which is $114. If the lookup table method can be supported, then it would be $1B6.
;
;But this is the code that would be replaced. The size of the interpreter is Unknown.
;If only stats0 was converted, it seems borderline, especially without the lookup table support. 
;
;Note: any savings on stats0 reduces the size of the combat module in memory because a duplicate copy of stats0
;is included in the combat module during the build process. 
;
;Note: it looks like stats3 (skills summary) would benifit a lot from the interpreter. Also INV_4.1b.ADDITIONAL_INFO in INV_4
;The other stats screen might benifit a little bit. So, for purposes of freeing up memory in the inventory module when
;running in main interface mode, there should be a material gain. 
;
;----outline of control table
; control table size (bytes)  (1st byte in the table)
;
;-datagram of control record(s)
; control record size (bytes), HTAB, VTAB, print code*, [address of variable] [address of ascii string] [ascii string]
;
; *(x) print variable subroutine (one code for each routine)
; *(x) print ascii string in control record
; *(x) print ascii string from address stored in control record
; *high-bit not set = 8-bit variable
; *high-bit set = 16-bit variable
;
; -corner cases
; *health status   (text block based on variable value)
; *gender (text block based on variable value)
; *race (uses lookup table for text block)
; *.LH.WEAPON.NAME (setup constants in a table. have address of variable point to table. make the wrapper for PRINT.STR.APPEND_RIGHT will look there for it’s values
; *.FOOTER (modifies an ASCII hex table on the fly. May need to keep this hard coded)
;
; PRINT.HEX8.LJ
; PRINT.HEX8.RJ
; PRINT.HEX16.RJ
; PRINT.STR.APPEND_RIGHT
; PRINT.STR (used in screen2, combat stats)


.DISPLAY.STATS_SUMMARY	
@START
;PARAMETERS:
;ENTRANCE: COMBAT OR INVENTORY
;RETURN: 

						; lda #$bb
						; jsr prep.brk
						; brk
						

.INIT
@START
					
.INIT.INV.MODULE
;Note: I think it's okay if this runs when this screen is accessed after initial launch because
;this is menu 0, so the init value is still correct.
	LDA #$00
	STA INV.ACTIVE_MENU.CODE

	

		
		
		
			; ;TROUBLESHOOTING LOOP
			; LDX #$00
; .TEST.LOOP1			
			; LDA CHR_SHEET.RECORD.READ,X
			; STA $BA00,X
			; INX
			; BNE .TEST.LOOP1
			; ;
			; lda #$aa
			; jsr prep.brk
			; brk
			
.INIT.SCREEN
@START
;*****WARNING: doesn't display menu icons when called from combat. See **OPT** below for info on why.


			; jsr keyin
			; lda #$aa
			; jsr prep.brk
			; brk
			

.INIT.INVENTORY_WINDOW
@START
;VALIDATE ENTRANCE
;(skip this section if not first run as there is no need to redraw the inventory window
;and reinitializing the active player selector would reset the selector to player1)
	LDA INV_0.SCREEN0.FIRST_RUN.FLAG		;($00 =  first run of screen0 not complete | $01 first run screen0 complete)
	BEQ .VALIDATE.ENTRANCE.DONE
	;clear inventory text space
	;(after first run, the inventory window doesn't need to be redrawn, but the text space still needs to be cleared)
	JSR INV.ERASE.INVENTORY_WINDOW.TEXT_SPACE ;foreground and background page /w flip
	
	JMP .INIT.INVENTORY_WINDOW.DONE
.VALIDATE.ENTRANCE.DONE

	
	

.INV_0.DRAW_ERASE.INVENTORY_WINDOW ;also see duplicate routine "INV.DRAW_ERASE.INVENTORY_WINDOW"
;two routines are needed because combat keeps a duplicate of the stats screen in-memory so it can be launched without a disk load
@START
;DRAW BORDER: LEFT TEXT WINDOW
@START
	LDA #TWB.LW.INVENTORY.TOP_LINE		;load line in x register	
	STA DRAW.START_LINE
	
	LDA #TWB.LW.INVENTORY.LEFT_SBYTE
	STA DRAW.START_BYTE
	
	LDA #TWB.LW.INVENTORY.RIGHT_SBYTE
	STA DRAW.STOP_BYTE
		
	LDA #TWB.LW.INVENTORY.BOTTOM_LINE
	STA DRAW.STOP_LINE

	LDA #$D5
	STA DRAW.BYTE_VALUE.HORIZONTAL+$0
	
	LDA #$AA
	STA DRAW.BYTE_VALUE.HORIZONTAL+$1
	
	LDA #$81
	;LDA #$84
	STA DRAW.BYTE_VALUE.VERTICLE+$0

	LDA #$A0
	;LDA #$88
	STA DRAW.BYTE_VALUE.VERTICLE+$1

.DRAW.RECTANGLE
		LDA PAGE.BACKGROUND
		ORA #$7C ;set DRAW.LINE flags
	JSR DRAW.LINE
	
	JSR FLIP.PAGE
					
		LDA PAGE.BACKGROUND
		ORA #$7C ;set DRAW.LINE flags
	JSR DRAW.LINE
	


			
@END

;DRAW LINE: BELOW ICONS
@START

	LDA #TWB.LW.INVENTORY.SEPERATOR_LINE							;load line in x register	
	STA DRAW.START_LINE
	
	;LDA #TWB.LW.INVENTORY.LEFT_SBYTE+1
	LDA #TWB.LW.INVENTORY.LEFT_SBYTE
	STA DRAW.START_BYTE
	
	;LDA #TWB.LW.INVENTORY.RIGHT_SBYTE-1
	LDA #TWB.LW.INVENTORY.RIGHT_SBYTE
	STA DRAW.STOP_BYTE
		
	LDA #TWB.LW.INVENTORY.SEPERATOR_LINE+1 ;(set to last line to draw + 1)
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

;DRAW MENU ICONS
@START
.DRAW.MENU_ICONS


;****TEMPORARY: this validate entrance section exists to skip the menu icon draw during combat launch because
;the INV.DRAW.MENU.ICON isn't active in memory. I'm holding off on a solution because if combat will be
;permenatnly in memory then this section will be permenatnly in memory and can be accessed fro 
;screen0 regardless of combat or non-combat launch. In that event, the menu icon draw subroutine
;can be part of this subroutine, and any other code sections in this routine also duplicated in the
;common subroutines section of SWAP.ROUTINES.INV.entrance_exit.asm no longer need to be duplicated. 


;VALIDATE ENTRANCE
	;is combat active
	LDA PLAYER.MAP.LOCATION_TYPE	;load map type of player's current map
	CMP #MAP.TYPE.COMBAT
	BEQ .DRAW.MENU_ICONS.DONE

	

	
;DRAW MENU ICON_0
		LDA #INV.MENU_ICON0.SBYTE
		STA SCREEN.DRAW.CURRENT_BYTE
		
		LDA	#MENU_ICON0.STATS
		STA SHAPE+$0
		LDA /MENU_ICON0.STATS
		STA SHAPE+$1
	JSR INV.DRAW.MENU.ICON

;DRAW MENU ICON_1
		LDA #INV.MENU_ICON1.SBYTE
		STA SCREEN.DRAW.CURRENT_BYTE
		
		LDA	#MENU_ICON1.WEAPONS
		STA SHAPE+$0
		LDA /MENU_ICON1.WEAPONS
		STA SHAPE+$1
	JSR INV.DRAW.MENU.ICON

;DRAW MENU ICON_2
		LDA #INV.MENU_ICON2.SBYTE
		STA SCREEN.DRAW.CURRENT_BYTE
		
		LDA	#MENU_ICON2.ARMOR
		STA SHAPE+$0
		LDA /MENU_ICON2.ARMOR
		STA SHAPE+$1
	JSR INV.DRAW.MENU.ICON


;DRAW MENU ICON_3
		LDA #INV.MENU_ICON3.SBYTE
		STA SCREEN.DRAW.CURRENT_BYTE
		
		LDA	#MENU_ICON3.MISC_ITEMS
		STA SHAPE+$0
		LDA /MENU_ICON3.MISC_ITEMS
		STA SHAPE+$1
	JSR INV.DRAW.MENU.ICON	

;DRAW MENU ICON_4
		LDA #INV.MENU_ICON4.SBYTE
		STA SCREEN.DRAW.CURRENT_BYTE
		
		LDA	#MENU_ICON4.SPELLS
		STA SHAPE+$0
		LDA /MENU_ICON4.SPELLS
		STA SHAPE+$1
	JSR INV.DRAW.MENU.ICON	
	
;DRAW MENU ICON_5
		LDA #INV.MENU_ICON5.SBYTE
		STA SCREEN.DRAW.CURRENT_BYTE
		
		LDA	#MENU_ICON5.GAME_SETTINGS
		STA SHAPE+$0
		LDA /MENU_ICON5.GAME_SETTINGS
		STA SHAPE+$1
	JSR INV.DRAW.MENU.ICON	

.DRAW.MENU_ICONS.DONE	
@END

@END

.INIT.ACTIVE_PLAYER.SELECTOR
@START
	;is combat active
	LDA PLAYER.MAP.LOCATION_TYPE	;load map type of player's current map
	CMP #MAP.TYPE.COMBAT
	BNE .NOT.COMBAT.LAUNCH
		LDA #$7F
		STA COUT_CHAR_TYPE ;($00 = normal, $7F = inverse)	
		;INV.ACTIVE_PLAYER: already set
	JSR DRAW_ERASE.ACTIVE_PLAYER.SELECTOR
			
	JMP .INIT.ACTIVE_PLAYER.SELECTOR.DONE
	
.NOT.COMBAT.LAUNCH	
		LDA PARTY.TOTAL.PC ;initialy set active player to max value so that CHARACTER.ROSTER.INCREMENT_DOWN will flip it over to player 1
		STA INV.ACTIVE_PLAYER		
	JSR CHARACTER.ROSTER.INCREMENT_DOWN
.INIT.ACTIVE_PLAYER.SELECTOR.DONE	
	
@END	

.INIT.MENU.SELECTOR ;DRAW MENU SELECTOR ON MENU ICON #1
@START
	LDY #$03
	LDX #TWB.LW.INVENTORY.TOP_LINE+1
.LOOP.DRAW_ERASE
		LDA PAGE.BACKGROUND
	JSR GET.LINE.ADDRESS1

		LDA PAGE.FOREGROUND
	JSR GET.LINE.ADDRESS2

	LDA #$88					;bit mapped byte
	STA (LINE.BASE.ADDR1),Y		;save bit mapped byte to video screen memory (foreground page)  
	STA (LINE.BASE.ADDR2),Y		;save bit mapped byte to video screen memory (background page)

	INX ;next line (down)
	
	CPX #TWB.LW.INVENTORY.SEPERATOR_LINE
	BNE .LOOP.DRAW_ERASE
	
	
	
@END


.SET.ACTIVE.SCREEN ;(stats sub_module)
;****Connect combat to inventory note and **OPT** Memory. I'm not sure if this section should be here or in .INIT (INV.INVENTORY_MODULE.ENTRANCE) There is currently already some duplication

	LDA #$00
	STA INV.ACTIVE_STATS_SCREEN.CODE ;($00 = stats: screen0 - stats summary)

	
.INIT.INVENTORY_WINDOW.DONE

	;set run flag to complete so that the above init section doesn't run again if screen0 is accessed during the same inventory session
	LDA #$01
	STA INV_0.SCREEN0.FIRST_RUN.FLAG ;($00 =  first run of screen0 not complete | $01 first run screen0 complete)

	
@END



@END

			; jsr keyin
			; cmp #$D1
			; bne .temp
			; lda #$aa
			; ldx INV.ACTIVE_PLAYER
			; jsr prep.brk
			; brk
; .temp			

.RELAUNCH.RENTRY_POINT ;if combat is active and active player is toggled, this is the reentry point


.INIT.STATS.SUB_MODULE
;READ ACTIVE PC CHARACTER SHEET DATA
		LDA INV.ACTIVE_PLAYER ;parm: player sequential # (high-bit not set = read mode)
	JSR COMBAT.READ_WRITE.CHR_SHEET.PC
		;RETURN VALUE = CHR_SHEET.RECORD.READ
	
;READ READIED EQUIPMENT TABLE RECORD (for active player)
	;NOT NEEDED. This screen doesn't use data from this table. The other screens get the data on their own.
	;and this data isn't accessible from combat so if this routine tries to call the subroutines to get the
	;readied equipment table data, the program will hang unless the code is skipped if the combat map is detected. 

		
	
	;***remove this section****
;DRAW BORDER: LEFT TEXT WINDOW
@START

	; ;**OPT** Memory. Seems like this section could call a common routine to draw the inventory window and icons (right now it doesn't do the icons). 
	; ;and by doing that the validate entrance below and the "clear inventory text space" section from INV_0.DISPLAY.STATS_SUMMARY can be removed
	; ;UPDATE: using INV.DRAW_ERASE.INVENTORY_WINDOW in place of "clear inventory text space" causes a screen blip because it draws to both pages. As to the
	; ;routine below, ultimately INV.DRAW_ERASE.INVENTORY_WINDOW needs to be in main memory or duplicated in combat. The code below doesn't draw the menu icons and
	; ;it wouldn't make sense to make it do so (presumably via an inline copy of INV.DRAW_ERASE.INVENTORY_WINDOW) because then it takes up
	; ;combat module memory and inventory module memory. The decision between main memory or duplicating in combat module memory turns on whether combat will be
	; ;kept in memory permenantly.
	
; ;VALIDATE ENTRANCE
	; ;skip if combat is not active (text window border would already have been drawn in inventory entrance)
	; LDA PLAYER.MAP.LOCATION_TYPE		;load map code of player's current map
	; CMP #MAP.TYPE.COMBAT
	; BNE .DRAW.BORDER.DONE
	
	
; ;DRAW	
	; LDA #TWB.LW.INVENTORY.TOP_LINE		;load line in x register	
	; STA DRAW.START_LINE
	
	; LDA #TWB.LW.INVENTORY.LEFT_SBYTE
	; STA DRAW.START_BYTE
	
	; LDA #TWB.LW.INVENTORY.RIGHT_SBYTE
	; STA DRAW.STOP_BYTE
		
	; LDA #TWB.LW.INVENTORY.BOTTOM_LINE
	; STA DRAW.STOP_LINE

	; LDA #$D5
	; STA DRAW.BYTE_VALUE.HORIZONTAL+$0
	
	; LDA #$AA
	; STA DRAW.BYTE_VALUE.HORIZONTAL+$1
	
	; LDA #$81	;#$90
	; STA DRAW.BYTE_VALUE.VERTICLE+$0

	; LDA #$A0
	; STA DRAW.BYTE_VALUE.VERTICLE+$1

; .DRAW
		; LDA PAGE.BACKGROUND
		; ORA #$7C
	; JSR DRAW.LINE
	
	; JSR FLIP.PAGE
					
		; LDA PAGE.BACKGROUND
		; ORA #$7C	
	; JSR DRAW.LINE

; .DRAW.BORDER.DONE
@END

@END

	

		
.PRINT.SCREEN.TEXT
@START

;.CHARACTER_NAME
@START
; ;PRINT VARIABLE
		; LDA #INV.SCREEN.CHAR_NAME.HTAB
		; STA PRINT.STR.CENTERED.HTAB
		
		; LDA #INV.SCREEN.CHAR_NAME.VTAB
		; STA PRINT.STR.CENTERED.VTAB
		
		; LDA #INV.SCREEN.CHAR_NAME.SIZE
		; STA PRINT.STR.CENTERED.WINDOW_SIZE
		
		; LDA #CHR_SHEET.PC.NAME.CHARACTER 				
		; STA STRING+$0		
		; LDA /CHR_SHEET.PC.NAME.CHARACTER
		; STA STRING+$1
		; ;STRING(2): pointer to text string to print (already set)
	; JSR PRINT.STR.CENTERED

@END

.HEALTH_STATUS
@START

	;**OPT** Memory. If the health status codes were sequential, then SCREEN_BUILDER could be used to print
					;via the lookup table print_routine_code (PRC). See .CT.RACE for example. 
					;*Also see .PRINT.HEALTH_STATUS (DISPLAY.CHARACTER.ROSTER). Same issue. 

					
					
					
;PRINT LABEL
		LDA #INV.SCREEN.HEALTH.HTAB	;#CONSTANT. location to print the label preceeding the variable value. 
		STA HTAB	
		LDA #INV.SCREEN.HEALTH.VTAB 
		STA VTAB
	JSR	UPDATE.CHAR.POS

		LDA #.INV.TEXT_BLOCK.HEALTH
		STA STRING+$0
		LDA /.INV.TEXT_BLOCK.HEALTH
		STA STRING+$1
	JSR PRINT.STR
	
;PRINT VARIABLE

					
	LDA CHR_SHEET.PC.HEALTH_STATUS
	BEQ .HEALTH_STATUS.GOOD
	CMP #COMBAT.S_ENTITY.STATUS.POSSESSED
	BEQ .HEALTH_STATUS.POSSESSED
	CMP #COMBAT.S_ENTITY.STATUS.SLEEPING
	BEQ .HEALTH_STATUS.SLEEPING
	CMP #COMBAT.S_ENTITY.STATUS.DEAD
	BEQ .HEALTH_STATUS.DEAD
	
;ERROR: Unexpected health status code
;.HEALTH_STATUS(.DISPLAY.STATS_SUMMARY) reports unexpected health status code
	JSR PREP.BRK
	BRK

.HEALTH_STATUS.GOOD	
		LDA #.INV.TEXT_BLOCK.HEALTH_STATUS.GOOD
		STA STRING+$0
		LDA /.INV.TEXT_BLOCK.HEALTH_STATUS.GOOD
		STA STRING+$1
	JMP .HEALTH_STATUS.PRINT
	
.HEALTH_STATUS.POSSESSED
		LDA #.INV.TEXT_BLOCK.HEALTH_STATUS.POSSESSED
		STA STRING+$0
		LDA /.INV.TEXT_BLOCK.HEALTH_STATUS.POSSESSED
		STA STRING+$1
	JMP .HEALTH_STATUS.PRINT

.HEALTH_STATUS.SLEEPING
		LDA #.INV.TEXT_BLOCK.HEALTH_STATUS.SLEEPING
		STA STRING+$0
		LDA /.INV.TEXT_BLOCK.HEALTH_STATUS.SLEEPING
		STA STRING+$1
	JMP .HEALTH_STATUS.PRINT
	
.HEALTH_STATUS.DEAD
		LDA #.INV.TEXT_BLOCK.HEALTH_STATUS.DEAD
		STA STRING+$0
		LDA /.INV.TEXT_BLOCK.HEALTH_STATUS.DEAD
		STA STRING+$1
	;**FALLS THROUGH**
	
.HEALTH_STATUS.PRINT
	JSR PRINT.STR

.HEALTH_STATUS.DONE

@END

;###EXECUTE SCREEN BUILDER
@START
		;self modifying code
		;(the ascii text block which contains the footer is modified with the screen number by each screen subroutine )
		LDA #$B1							;set screen number on footer
		STA .INV.TEXT_BLOCK.FOOTER+$7



		;special parms (lookup table indexes that need custom bit manipulation)
		LDA CHR_SHEET.PC.RACE.MGK_FLG
		AND #$FF ;mask-out bit6-7, which are used for magic weapon flags
		STA SCREEN_BUILDER.CUSTOMIZED.LOOKUP_TABLE.INDEX1
		
		;standard parms
		LDA #$01 ;set HTAB of left margin of text space
		STA SCREEN_BUILDER.HTAB.LEFT
		LDA #.COMMAND_TABLE1
		STA SCREEN_BUILDER.COMMAND_TABLE.POINTER+$0
		LDA /.COMMAND_TABLE1
		STA SCREEN_BUILDER.COMMAND_TABLE.POINTER+$1		
	JSR SCREEN_BUILDER
	
@END
			
@END



.EXIT
			; lda #MAP.TYPE.COMBAT
			; sta PLAYER.MAP.LOCATION_TYPE		;load map code of player's current map
			
	LDA PLAYER.MAP.LOCATION_TYPE		;load map code of player's current map
	CMP #MAP.TYPE.COMBAT
	BEQ .COMBAT.STATS.STATE_LOOP ;branch if combat loaded	



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
	
	
	JMP INV.STATS.STATE_LOOP
	

.COMBAT.STATS.STATE_LOOP
@START

.KEYIN.LOOP
	LDA $C000
    BPL .KEYIN.LOOP
	STA $C010               ;CLR LAST KEY
	
		;;ACC = keypress
	;JSR CONVERT.ASCII.UCASE
		;;RETURN VALUE: ACC = ASCII code (upper case, if the code is a letter)

	;locally managed key commands	
	CMP #$8B			;UP ARROW
	BEQ .CHANGE.ACTIVE_PLAYER.UP
	CMP #$8A			;DOWN ARROW
	BEQ .CHANGE.ACTIVE_PLAYER.DOWN	

	;key commands that require inventory module to be loaded
.HOTKEY.CHECK ;test for keypress of 1-6
	CMP #$B1			
	BCC .HOTKEY.CHECK.DONE
	CMP #$B7
	BCC .LAUNCH.INVENTORY_MODULE.FROM_COMBAT ;exit, then goto the menu associated with the hotkey
.HOTKEY.CHECK.DONE	


	CMP #$88			;LEFT ARROW
	BEQ	.LAUNCH.INVENTORY_MODULE.FROM_COMBAT
	CMP #$95			;RIGHT ARROW
	BEQ .LAUNCH.INVENTORY_MODULE.FROM_COMBAT


	CMP #$9B			;ESC (EXIT COMBAT)
	BEQ	.EXIT_TO_COMBAT
	
	CMP #$89			;TAB (next menu)
	BEQ .LAUNCH.INVENTORY_MODULE.FROM_COMBAT

	JMP .KEYIN.LOOP


.CHANGE.ACTIVE_PLAYER.UP
	JSR CHARACTER.ROSTER.INCREMENT_UP
	JMP .RELAUNCH.SCREEN0
	
.CHANGE.ACTIVE_PLAYER.DOWN
	JSR CHARACTER.ROSTER.INCREMENT_DOWN

	;**FALLS THROUGH**
	
.RELAUNCH.SCREEN0	
; ;CLEAR INVENTORY TEXT SPACE
	; JSR INV.ERASE.INVENTORY_WINDOW.TEXT_SPACE ;foreground and background page /w flip

;****THE INVENTORY TEXT SPACE ISN'T CLEARED BECASE THE SUBROUTINE TO DO SO ISN'T ACCESSIBLE
;FROM THE COMBAT MODULE. 	
	
	JMP .RELAUNCH.RENTRY_POINT
	
.LAUNCH.INVENTORY_MODULE.FROM_COMBAT

	;****NOTE: I was going to seed the keypress using the code below but that actually doesn't work. And Peter pointed out that STA $C000 is a soft switch related to graphics. 
	; ;ACC = keypress
	; STA $C000 ;seed keypress for INV.STATS.STATE_LOOP

	;***FUTURE: to enable menu changes from combat, JSR LAUNCH.INVENTORY.MODULE.
	;INV.INVENTORY_MODULE.ENTRANCE will need some code to detect a call from the combat routine
	;and JMP INV.STATS.STATE_LOOP
	;
	;Note: the INV.INVENTORY_MODULE.ENTRANCE hasn't run
	;at this point in the program flow since combat has a local copy of screen0. 
	;it must run of things like reading the player inventory records from disk won't happen. 
	


	JSR PREP.BRK
	BRK
	
.EXIT_TO_COMBAT
	
	;**NOTE: from this point the program could just fall through back to COMBAT.DISPLAY.STATS_SUMMARY**
	;However, that won't work if the above state loop is used for stats screen0 access after
	;other sub_modules are accessed (i.e. items are readied/unreadied), as INV.INVENTORY_MODULE.EXIT may then need to run. 
	
	JSR PREP.BRK
	BRK
	
	
	
@END

	
@END
	
;LOCAL SUBROUTINES (!!!can't use parent labels!!!)

	
;LOCAL VARIABLES (!!!can't use parent labels!!!)
;(this is because this source file is an include to two parent source files)

;LOCAL TEXT BLOCKS (!!!can't use parent labels!!!)
@START
.INV.PLACE_HOLDER						.AZ -/AAA/	;***this text block is used for variables that aren't setup yet like HP MAX			

;.INV.PLACE_HOLDER						.AZ -/400/	;***this text block is used for variables that aren't setup yet like HP MAX			

;MISC
.INV.TEXT_BLOCK.FOOTER					.AZ -/<-MORE !/,#$AF,/4 MORE->/ ;screen number is at byte $7

;HEALTH
.INV.TEXT_BLOCK.HEALTH					.AZ -/Status:/
.INV.TEXT_BLOCK.HEALTH_STATUS.GOOD		.AZ -/good/
.INV.TEXT_BLOCK.HEALTH_STATUS.DEAD		.AZ -/dead/
.INV.TEXT_BLOCK.HEALTH_STATUS.SLEEPING	.AZ -/sleeping/
.INV.TEXT_BLOCK.HEALTH_STATUS.POSSESSED	.AZ -/possessed/

;
.INV.GENDER_TABLE ;(contains the memory address of the gender text blocks below. The gender code*2 stored in the character sheet is the index)
@START
	.DA #.INV.TEXT_BLOCK.GENDER.MALE, /.INV.TEXT_BLOCK.GENDER.MALE, #.INV.TEXT_BLOCK.GENDER.FEMALE, /.INV.TEXT_BLOCK.GENDER.FEMALE
.INV.TEXT_BLOCK.GENDER.MALE				.AZ -/male/
.INV.TEXT_BLOCK.GENDER.FEMALE			.AZ -/female/
@END

.INV.RACE_TABLE ;(contains the memory address of the race text blocks below. The race code*2 stored in the character sheet is the index)
@START
	.DA #.INV.TEXT_BLOCK.RACE.HUMAN, /.INV.TEXT_BLOCK.RACE.HUMAN, #.INV.TEXT_BLOCK.RACE.HALFLING, /.INV.TEXT_BLOCK.RACE.HALFLING, #.INV.TEXT_BLOCK.RACE.ELF, /.INV.TEXT_BLOCK.RACE.ELF, #.INV.TEXT_BLOCK.RACE.DARK_ELF, /.INV.TEXT_BLOCK.RACE.DARK_ELF, #.INV.TEXT_BLOCK.RACE.DWARF, /.INV.TEXT_BLOCK.RACE.DWARF

.INV.TEXT_BLOCK.RACE.HUMAN 		.AZ -/human/
.INV.TEXT_BLOCK.RACE.HALFLING 	.AZ -/halfling/
.INV.TEXT_BLOCK.RACE.ELF	 	.AZ -/elf/
.INV.TEXT_BLOCK.RACE.DARK_ELF 	.AZ -/dark elf/
.INV.TEXT_BLOCK.RACE.DWARF		.AZ -/dwarf/
@END


@END

;SCREEN_BUILDER COMMAND TABLES
@START

;*Note: Must use .DB directive when writing bytes to a file using a label expression (i.e. .COMMAND_TABLE1.RECORDS.END-.COMMAND_TABLE1.RECORDS.START)
 
 
.COMMAND_TABLE1
@START


;COMMAND TABLE SIZE (bytes)
	.DB .COMMAND_TABLE1.RECORDS.END-.COMMAND_TABLE1.RECORDS.START+1 ;+1 the byte quantity includes this byte that we are writing to the file with this directive. 
.COMMAND_TABLE1.RECORDS.START

		
.CT.GENDER
@START
;UPDATE SCREEN POSITION
	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$0E ;reset HTAB, VTAB+1

;LABEL

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$00 ;local ascii table, PRINT.STR

	;local ascii table
.INV.TEXT_BLOCK.GENDER					.AZ -/Gender:/
	
;DESCRIPTOR: GENDER

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$20 ;lookup table (external ascii table), PRINT.STR
	;command parameter fields (PRC)
	.DA #.INV.GENDER_TABLE		;lookup table address (LO)
	.DA /.INV.GENDER_TABLE		;lookup table address (HO)
	.DA #CHR_SHEET.PC.GENDER	;lookup table index address (LO)
	.DA /CHR_SHEET.PC.GENDER	;lookup table index address (HO)
	
	
	
@END	

.CT.RACE
@START
;RACE CODE LIST
;(for authoratative version see combat spreadsheet, PC character sheet datagram)
; $00 = human
; $01 = hafling
; $02 = elf
; $03 = dark elf   (this is the one I'd give the resist magic bonus in exchange for other stats being lower)
; $04 = dwarf



;UPDATE SCREEN POSITION
	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$0E ;reset HTAB, VTAB+1

;LABEL

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$00 ;local ascii table, PRINT.STR

	;local ascii table
.INV.TEXT_BLOCK.RACE	 		.AZ -/Race  :/
	
;DESCRIPTOR: RACE

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$20 ;lookup table (external ascii table), PRINT.STR
	;command parameter fields (PRC)
	.DA #.INV.RACE_TABLE		;lookup table address (LO)
	.DA /.INV.RACE_TABLE		;lookup table address (HO)
	.DA #SCREEN_BUILDER.CUSTOMIZED.LOOKUP_TABLE.INDEX1	;lookup table index address (LO)
	.DA /SCREEN_BUILDER.CUSTOMIZED.LOOKUP_TABLE.INDEX1	;lookup table index address (HO)
	
	
	
@END	

.CT.LEVEL
@START
;UPDATE SCREEN POSITION
	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$0E ;reset HTAB, VTAB+1
	
;LABEL

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$00 ;local ascii table, PRINT.STR

	;command parameter fields (PRC)
.INV.TEXT_BLOCK.LEVEL	 		.AZ -/Level :/
	
;VARIABLE

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$14 ;variable, PRINT.HEX8.LJ
	;command parameter fields (PRC)
	.DA #CHR_SHEET.PC_MOB.LEVEL
	.DA /CHR_SHEET.PC_MOB.LEVEL

@END
	
.CT.HP
@START
;UPDATE SCREEN POSITION
	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$0F ;reset HTAB, VTAB+2
	
;LABEL

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$00 ;local ascii table, PRINT.STR

	;command parameter fields (PRC)
	;local ascii table
.INV.TEXT_BLOCK.HP 				.AZ -/HP /,#$AD


;VARIABLE

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$18 ;variable, PRINT.HEX16.RJ
	;command parameter fields (PRC)
	.DA #CHR_SHEET.PC_MOB.HP_LO
	.DA /CHR_SHEET.PC_MOB.HP_LO	

;SEPERATOR
	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$00 ;local ascii table, PRINT.STR

	;local ascii table
.CT.HP.SEPERATOR	 	.AZ -/ /,#$AF,/ /	;prints " / "

;VARIABLE  ****place holder****

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$10 ;external ascii table, PRINT.STR
	;command parameter fields (PRC)
	.DA #.INV.PLACE_HOLDER
	.DA /.INV.PLACE_HOLDER
	
@END

.CT.XP
@START
;UPDATE SCREEN POSITION
	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$0E ;reset HTAB, VTAB+1
	
;LABEL

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$00 ;local ascii table, PRINT.STR

	;command parameter fields (PRC)
	;local ascii table
.INV.TEXT_BLOCK.XP 				.AZ -/XP /,#$AD


;VARIABLE

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$18 ;variable, PRINT.HEX16.RJ
	;command parameter fields (PRC)
	.DA #CHR_SHEET.PC_MOB.XP
	.DA /CHR_SHEET.PC_MOB.XP

;SEPERATOR
	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$00 ;local ascii table, PRINT.STR

	;local ascii table
.CT.XP.SEPERATOR	 	.AZ -/ /,#$AF,/ /	;prints " / "

;VARIABLE  ****place holder****

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$10 ;external ascii table, PRINT.STR
	;command parameter fields (PRC)
	.DA #.INV.PLACE_HOLDER
	.DA /.INV.PLACE_HOLDER
	
@END

.CT.MP
@START
;UPDATE SCREEN POSITION
	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$0E ;reset HTAB, VTAB+1
	
;LABEL

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$00 ;local ascii table, PRINT.STR

	;command parameter fields (PRC)
	;local ascii table
.INV.TEXT_BLOCK.MP 				.AZ -/MP /,#$AD


;VARIABLE

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$15 ;variable, PRINT.HEX8.RJ
	;command parameter fields (PRC)
	.DA #CHR_SHEET.PC_MOB.MP
	.DA /CHR_SHEET.PC_MOB.MP

;SEPERATOR
	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$00 ;local ascii table, PRINT.STR

	;local ascii table
.CT.MP.SEPERATOR	 	.AZ -/ /,#$AF,/ /	;prints " / "


;VARIABLE

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$14 ;variable, PRINT.HEX8.LJ
	;command parameter fields (PRC)
	.DA #CHR_SHEET.PC.ATTRIB.INT
	.DA /CHR_SHEET.PC.ATTRIB.INT

	
@END

.CT.STR
@START
;UPDATE SCREEN POSITION
	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$0F ;reset HTAB, VTAB+2
	
;LABEL

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$00 ;local ascii table, PRINT.STR

	;command parameter fields (PRC)
	;local ascii table
.INV.TEXT_BLOCK.STR 				.AZ -/STR/,#$AD

;VARIABLE

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$15 ;variable, PRINT.HEX8.RJ
	;command parameter fields (PRC)
	.DA #CHR_SHEET.PC.ATTRIB.STR
	.DA /CHR_SHEET.PC.ATTRIB.STR

;SEPERATOR
	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$00 ;local ascii table, PRINT.STR

	;local ascii table
.CT.STR.SEPERATOR	 	.AZ -/ *=/


;VARIABLE

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$14 ;variable, PRINT.HEX8.LJ
	;command parameter fields (PRC)
	.DA #CHR_SHEET.PC.READIED_EQUIPMENT_WEIGHT
	.DA /CHR_SHEET.PC.READIED_EQUIPMENT_WEIGHT

	
@END

.CT.DEX
@START
;UPDATE SCREEN POSITION
	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$0E ;reset HTAB, VTAB+1
	
;LABEL

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$00 ;local ascii table, PRINT.STR

	;command parameter fields (PRC)
	;local ascii table
.INV.TEXT_BLOCK.DEX 				.AZ -/DEX/,#$AD

;VARIABLE

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$15 ;variable, PRINT.HEX8.RJ
	;command parameter fields (PRC)
	.DA #CHR_SHEET.PC.ATTRIB.DEX
	.DA /CHR_SHEET.PC.ATTRIB.DEX

	
@END
		
.CT.INT
@START
;UPDATE SCREEN POSITION
	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$0E ;reset HTAB, VTAB+1
	
;LABEL

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$00 ;local ascii table, PRINT.STR

	;command parameter fields (PRC)
	;local ascii table
.INV.TEXT_BLOCK.INT 				.AZ -/INT/,#$AD

;VARIABLE

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$15 ;variable, PRINT.HEX8.RJ
	;command parameter fields (PRC)
	.DA #CHR_SHEET.PC.ATTRIB.INT
	.DA /CHR_SHEET.PC.ATTRIB.INT

	
@END
	
.CT.LH.WEAPON.NAME
@START
;UPDATE SCREEN POSITION
	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$0F ;reset HTAB, VTAB+2
	
;LABEL

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$00 ;local ascii table, PRINT.STR

	;command parameter fields (PRC)
	;local ascii table
.INV.TEXT_BLOCK.LH_WP_NAME			.AZ -/LH/,#$AD

;VARIABLE (***SPECIAL FORMAT FOR STRING APPEND***)

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$11 ;external ascii table, PRINT.STR.APPEND_RIGHT
	;command parameter fields (PRC)
	
	.DA #INV.SCREEN.LH_WP_NAME.APPEND_TEXT.HTAB		;PRINT.STR.APPEND_RIGHT.HTAB
		
	.DA #INV.SCREEN.LH_WP_NAME.APPEND_TEXT.VTAB		;PRINT.STR.APPEND_RIGHT.VTAB
		
	.DA #INV.SCREEN.LH_WP_NAME.APPEND_TEXT.SIZE		;PRINT.STR.APPEND_RIGHT.WINDOW_SIZE
		
		
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
	.DA #$00 ;local ascii table, PRINT.STR

	;command parameter fields (PRC)
	;local ascii table
.INV.TEXT_BLOCK.RH_WP_NAME			.AZ -/RH/,#$AD

;VARIABLE (***SPECIAL FORMAT FOR STRING APPEND***)

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$11 ;external ascii table, PRINT.STR.APPEND_RIGHT
	;command parameter fields (PRC)	
	.DA #INV.SCREEN.RH_WP_NAME.APPEND_TEXT.HTAB		;PRINT.STR.APPEND_RIGHT.HTAB
		
	.DA #INV.SCREEN.RH_WP_NAME.APPEND_TEXT.VTAB		;PRINT.STR.APPEND_RIGHT.VTAB
		
	.DA #INV.SCREEN.RH_WP_NAME.APPEND_TEXT.SIZE		;PRINT.STR.APPEND_RIGHT.WINDOW_SIZE
		
		
	.DA #CHR_SHEET.PC.NAME.READIED_WP_RHAND			;external ascii table
	.DA /CHR_SHEET.PC.NAME.READIED_WP_RHAND

	
@END

.CT.FOOTER
@START
;UPDATE SCREEN POSITION
	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$0D ;set HTAB, VTAB
	;command parameter fields (PRC)	
	.DA #INV.SCREEN.FOOTER.HTAB	
	.DA #INV.SCREEN.FOOTER.VTAB


;FOOTER TEXT

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$10 ;local ascii table, PRINT.STR

	;command parameter fields (PRC)
	.DA #.INV.TEXT_BLOCK.FOOTER						;external ascii table
	.DA /.INV.TEXT_BLOCK.FOOTER
	
	
@END


.COMMAND_TABLE1.RECORDS.END

.COMMAND_TABLE1.SIZE = .COMMAND_TABLE1.RECORDS.END-.COMMAND_TABLE1.RECORDS.START+1

;VERIFY COMMAND TABLE SIZE <= !256bytes
	.DO .COMMAND_TABLE1.SIZE>$100
	
	.ER	F,***.COMMAND_TABLE1 exceeds !256 bytes
	
	.FI ;END IF
@END


	
	
@END



;LOCAL SUBROUTINES (!!!can't use parent labels!!!)
@START


@END