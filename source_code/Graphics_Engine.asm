;************************INCLUDE FILE*****************************
;(do not assemble stand-alone.) /;

;=====================GRAPHICS_ENGINE.ASM DOCUMENTATION====================================
;This file contains the primary subroutines associated with graphic plotting with the screen's tile grid. 
;
;Occcasionally the code for one of the subroutines is included in another file, in-line. For example, Animation_Manager.ASM contains
;an in-line version of DRAW.TILE with a few customizations to remove components not required. 
;The purpose ofincluding such in-line code is to increase speed avoiding the costly JSR/RTS (12 clock cycles) required to make subroutine calls within inner loops. 
;
;Most of the subroutines in this file are designed for direct entrance.
;Notable exceptions are DRAW.ROW, DRAW.COLUMN and DRAW.TILE. 
;In general, to draw tiles, calls should typically be made to DRAW.SCREEN (redraw entire screen), DRAW.COLUMN.SINGLE, DRAW.ROW.SINGLE, or DRAW.TILE.SINGLE  
;=================================================================================



DRAW.SCREEN ;		;=============DRAWS SCREEN FROM TILE DATA, ANYWHERE ON THE MAP=========
@START
;PARAMETERS: GMAP, PAGE.FOREGROUND, PAGE.BACKGROUND
;RETURN: NONE

;=====================SUBROUTINE DOCUMENTATION====================================
;This is the top level subroutine used to draw the tiles on the screen from scratch. 
;This routine is called during game launch and at times during game operation when a complete update of all tiles on screen is required (example, player board flying transport and the dark tiles need to be updated)
;It is not used when the player moves because in that event the tiles are moved
;using by screen scrolling (see Graphics_Scrolling.ASM and calls to its routines from MOVE.NORTH/SOUTH/EAST/WEST in Movement_Manager.ASM)
;
;This subroutine makes calls to DRAW.ROW, which calls DRAW.TILE to handle the actual graphic plotting. 
;This subroutine is reposible for the following:
;		a) loading SCREEN.TILE.DATA with the TILE_IDs for each tile location on the screen based on the player's position on the regional map (stored in RMAP)
;		b) setting up the starting screen byte and line on the graphics screen associated with the row tiles are to be drawn
;		c) tracking and incrementing the row in which tiles are to be drawn
;		d) drawing any HCRG text for player stats, etc. that are permenant on the view screen (not yet implemented beyond an HRCG test)
;=================================================================================


			
.INIT
@START

						;**OPT** Speed. Memory. I think the above is a duplicate. there is also one below in this routine before call to MO.DRAW and DARKNESS.REVIEW
.INIT.MAP
		
	JSR TILE.LOOKUP.SCREEN		;Fills SCREEN.TILE.DATA with uncompressed tile data from RZONE.ARRAY. This is so that the drawing routines know which Tile_IDs are associated with each tile location on the view screen.


			
DRAW.SCREEN.ALTERNATE.ENTRANCE ;skips tile lookup; used for modules like combat which dictate the tiles onscreen
;.INIT.MAP (CONTINUED)
	
	LDA SMAP
	STA SMAP.CURRENT
	LDA SMAP+$1
	STA SMAP.CURRENT+$1

.INIT.SCREEN					;SETUP BYTE, LINE, DEPTH
	LDY #$00					;Y-REG IS THE INDEX FOR SCREEN.TILE.DATA, AND REFERS AND REFERS TO THE TILE #
	;ACC = $00 expected
	STY ANIMATION.FRAME_STATE	;START ANIMATION AT FRAME 0    ;**OPT** Memory. This init may not be needed since ANIMATION.FRAME_STATE is a .BS

	LDA	#SCREEN.DRAW.START_BYTE											;**OPT** THIS IS DUPLICATE
	STA SCREEN.DRAW.CURRENT_BYTE				;SET STARTING SCREEN BYTE			;**OPT** THIS IS DUPLICATE
	LDA #SCREEN.DRAW.START_LINE										
	STA TILE.LINE				;SET CURRENT LINE TO PLOT WITHIN TILE	
	STA TILE.LINE.START			;SET STARTING LINE FOR TILES IN ROW 0
	

.INIT.MISC
	LDA #$01					;$01 IS THE CODE FOR DRAW.SCREEN
	STA CALLED_BY.DRAW.SCREEN
		
		
;**OPT** when this is used for drawing the screen at game start or first entering a map, this is 
;probably fine. we shouldn't need to clear the entire screen for movement, but that will not flow
;through draw.screen so it will probably get setup correctly to begin with. This is a reminder
;note to thing about if screen clears are being handled most efficiently in general.
	
.CLEAR.SCREEN					;CLEAR BACKGROUND PAGE (WHERE DRAW.TILE WILL PLOT TO)
					
@END
		
.MAIN.DRAW
@START

.LOOP
	LDA #SCREEN.DRAW.START_BYTE		;RESET SCREEN.DRAW.CURRENT_BYTE TO THE FIRST TILE COLUMN
	STA SCREEN.DRAW.CURRENT_BYTE
	
	JSR DRAW.ROW

	LDA SMAP.CURRENT			;ADVANCE TO NEXT ROW ON THE SCREEN
	STA OP1
	LDA SMAP.CURRENT+$1
	STA OP1+$1
	LDA #OFFSET.DOWN
	STA OP2
	LDA #$00
	STA OP2+$1
	
	JSR ADC.16						;SMAP.CURRENT(2) + #OFFSET.DOWN(1)
	
	LDA RESULT
	STA SMAP.CURRENT
	LDA RESULT+$1
	STA SMAP.CURRENT+$1

	LDA TILE.LINE.START				;ADVANCE LINE TO NEXT ROW OF TILES
	CLC
	ADC #TILE.DEPTH.STANDARD
	STA TILE.LINE.START
	STA TILE.LINE
	CMP	#SCREEN.DRAW.STOP_LINE		;READY TO SWITCH TILE COLUMNS? (TILE.LINE should already be in ACC at end of DRAW.TILE) 
	BCS .DRAW.LOOP.COMPLETE			;IF YES, EXIT (BCS: is ACC >= CMP value)
		
	JMP .LOOP
.DRAW.LOOP.COMPLETE

			; JSR FLIP.PAGE
			; jsr keyin
			; JSR FLIP.PAGE

			
			
			
	LDA #$01					;THIS VALUE IS USED TO THAT CERTAIN ROUTINES (LIKE DRAW.MISC IN DARKNESS.REVIEW KNOW THAT THEY WERE ENTERED VIA DRAW.SCREEN AND NOT A MOVE ROUTINE)
	STA CALLED_BY.DRAW.SCREEN

	JSR MO.DRAW					;draw map objects (run before DARKNESS.REVIEW so that objects like doors are added to the screen arrays, which are how DARKNESS.REVIEW detects them)

			; JSR FLIP.PAGE
			; jsr keyin
			; JSR FLIP.PAGE
		
			
	JSR DARKNESS.REVIEW			;update the hidden (darkness) tiles on the screen based on the tile_type values in SCREEN.TILE.DATA 


			; JSR FLIP.PAGE
			; jsr keyin
			; JSR FLIP.PAGE

			
@END

.DRAW.MISC
@START


.DRAW.SCREEN_BORDER
@START

;TOP    LINE: $3000 PAGE1 (ADDR1), $5000 PAGE2 (ADDR2)
;BOTTOM LINE: $33D0 PAGE3 (ADDR3), $53D0 PAGE4 (ADDR4)

	LDY #$02			;init screen byte index

;INIT LINE BASE ADDRESSES

	;init LO bytes
	LDA #$00
	STA LINE.BASE.ADDR1
	STA LINE.BASE.ADDR2
	
	LDA #$D0
	STA LINE.BASE.ADDR3
	STA LINE.BASE.ADDR4
	
	;init HO bytes
	LDA #$30
	STA LINE.BASE.ADDR1+$1

	LDA #$50
	STA LINE.BASE.ADDR2+$1

	LDA #$33
	STA LINE.BASE.ADDR3+$1

	LDA #$53
	STA LINE.BASE.ADDR4+$1	

;DRAW TOP AND BOTTOM LINE OF BORDER ON BOTH HI-RES PAGES		
.SCREEN_BORDER.LOOP
	LDA #$D5
	STA (LINE.BASE.ADDR1),Y
	STA (LINE.BASE.ADDR2),Y
	STA (LINE.BASE.ADDR3),Y
	STA (LINE.BASE.ADDR4),Y
	
	INY ;move 1 screen byte right
	
	LDA #$AA
	STA (LINE.BASE.ADDR1),Y
	STA (LINE.BASE.ADDR2),Y
	STA (LINE.BASE.ADDR3),Y
	STA (LINE.BASE.ADDR4),Y
	
	INY ;move 1 screen byte right
	
	CPY #SCREEN.STOP_BYTE-$04
	BNE .SCREEN_BORDER.LOOP
@END



			
			
			

.DRAW.CLEAN_UP
@START
;CLEAN UP GRAPHICS DEBRIS LEFT BY RIGHT SIDE TEXT WINDOW
	
;**OPT** Memory. This section is a disaster and there is likely memory to be saved by totally
;redoing it. I'm thinking just clear the top/bottom/left/right of the map edges, then draw 
;what should be there, and either don't worry about optimzing it so only the parts needed are
;done or worry about that after I've got a simple baseline that works. I can at least probably
;have that entire section skipped for all paths except for when a module was exited (i.e. NPC Talk, Inventory)
;that would save the speed after commands like (B)oard, when it's most critical. 
	
.RIGHT_SIDE.WINDOW	
;=====================CODE-SECTION DOCUMENTATION====================================
;erase the right edge and first two bytes of the top/bottom lines of the text window border. 
;This is needed because it is the portion of the text window border that isn't normally onscreen, 
;It isn't taken care of before the routines using the text window exists because CLEAR.TEXT_WINDOW.RIGHT 
;only clears the text space, not the border. If it were to clear the border on both pages the foreground 
;erase would be very noticable.
;
;
;=================================================================================


;**OPT** Memory. This section may be able to be greatly reduced if it used the DRAW.LINE subroutine in my graphics libary instead of all of the inline code. But consider that setting the parameters for DRAW.LINE will take memory too. Consider how much savings there will really be. 
	LDA TW.RIGHT_WINDOW.CLEAN_UP.FLAG  	;is flag set? This cleanup is only needed the first time DRAW.SCREEN is run after the routine using the text window exits
	CMP #$01
	BEQ .CLEAR.TW.RIGHT_SMALL ;if yes
	JMP	.DRAW.RIGHT_WINDOW.TEXT	;if no
	
	;**FALLS THROUGH**

.CLEAR.TW.RIGHT_SMALL
		LDA #$03
	JSR CLEAR.TW.RIGHT_SMALL.MAIN_SCREEN ;clear the small text window before drawing the border. 
										 ;this isn't needed when returning from NPC.TALK because a the large right side window is cleared before exit. 
										 ;but this routine is also used to cleanup the screen after the exit of other routines like
										 ;COMMAND.DISPLAY_CHARACTER_ROSTER
	
;CLEAR TEXT WINDOW BORDER (RIGHT SIDE): TOP LINE										
.DRAW.TW.BORDER.TOP
		LDA PAGE.FOREGROUND
		LDX #TWB.RW.NPC_TALK.TOP_LINE
	JSR GET.LINE.ADDRESS1
	
	;set background page line address
		LDA PAGE.BACKGROUND
		LDX #TWB.RW.NPC_TALK.TOP_LINE
	JSR GET.LINE.ADDRESS2
	
.START.DRAW	
	LDY #$24
	LDA #$00	;draw empty byte
	STA (LINE.BASE.ADDR1),Y
	STA (LINE.BASE.ADDR2),Y
	INY ;move 1 screen byte right
	LDA #$00	;draw empty byte
	STA (LINE.BASE.ADDR1),Y
	STA (LINE.BASE.ADDR2),Y

	INY ;move 1 screen byte right
	LDA #$00	;draw empty byte
	STA (LINE.BASE.ADDR1),Y
	STA (LINE.BASE.ADDR2),Y
	INY ;move 1 screen byte right
	LDA #$00	;draw empty byte
	STA (LINE.BASE.ADDR1),Y
	STA (LINE.BASE.ADDR2),Y
	

;CLEAR TEXT WINDOW BORDER (LEFT SIDE): TOP LINE	
	LDX #TWB.RW.NPC_TALK.TOP_LINE+1
	LDY #$00	
.LOOP.TWB.LEFT.ERASE

		LDA PAGE.BACKGROUND
	JSR GET.LINE.ADDRESS1

		LDA PAGE.FOREGROUND
	JSR GET.LINE.ADDRESS2

	LDA #$00					;bit mapped byte
	STA (LINE.BASE.ADDR1),Y		;save bit mapped byte to video screen memory (foreground page)  
	STA (LINE.BASE.ADDR2),Y		;save bit mapped byte to video screen memory (background page)

	INY
	STA (LINE.BASE.ADDR1),Y		;save bit mapped byte to video screen memory (foreground page)  
	STA (LINE.BASE.ADDR2),Y		;save bit mapped byte to video screen memory (background page)
	CPY #$0C
	BNE .LOOP.TWB.LEFT.ERASE
	LDY #$00 ;reset screen byte
	INX ;next line (down)
	CPX #TWB.RW.NPC_TALK.TOP_LINE+4
	BNE .LOOP.TWB.LEFT.ERASE

	
;SETUP DRAW, TEXT WINDOW BORDER (RIGHT SIDE): RIGHT EDGE
	LDY #TWB.RW.NPC_TALK.RIGHT_SBYTE
	
		LDA PAGE.FOREGROUND
		LDX #TWB.RW.NPC_TALK.TOP_LINE+$1 ;line 5
	JSR GET.LINE.ADDRESS1

		LDA PAGE.BACKGROUND
		LDX #TWB.RW.NPC_TALK.TOP_LINE+$1 ;line 5
	JSR GET.LINE.ADDRESS2
	

;DRAW TEXT WINDOW BORDER (RIGHT SIDE): RIGHT EDGE
.DRAW.TW.BORDER.RIGHT
	LDA #$00					;draw empty byte
	STA (LINE.BASE.ADDR1),Y		;save bit mapped byte to video screen memory
	STA (LINE.BASE.ADDR2),Y

	INX ;next line, move down
		LDA PAGE.FOREGROUND
	JSR GET.LINE.ADDRESS1 ;**OPT*** Speed. Replace with in-line code if memory in NPC.TALK module is available

		LDA PAGE.BACKGROUND
	JSR GET.LINE.ADDRESS2
	
	CPX #TWB.RW.NPC_TALK.BOTTOM_LINE
	BNE .DRAW.TW.BORDER.RIGHT

;DRAW TEXT WINDOW BORDER (RIGHT SIDE): BOTTOM LINE
	LDY #TWB.RW.NPC_TALK.RIGHT_SBYTE
	LDA #$00					;draw empty byte
	STA (LINE.BASE.ADDR1),Y
	STA (LINE.BASE.ADDR2),Y
	DEY ;move 1 screen byte left
	LDA #$00					;draw empty byte
	STA (LINE.BASE.ADDR1),Y
	STA (LINE.BASE.ADDR2),Y

	DEY ;move 1 screen byte left
	LDA #$00					;draw empty byte
	STA (LINE.BASE.ADDR1),Y
	STA (LINE.BASE.ADDR2),Y
	DEY ;move 1 screen byte left
	LDA #$00					;draw empty byte
	STA (LINE.BASE.ADDR1),Y
	STA (LINE.BASE.ADDR2),Y

	STA TW.RIGHT_WINDOW.CLEAN_UP.FLAG  ;turn flag off so that the clean-up isn't done the next time DRAW.SCREEN runs

;CLEAR 2 EMPTY COLUMNS (1 TILE WIDTH) TO THE LEFT OF THE MAP EDGE
@START


		
	LDX #$00
.LOOP.ERASE
	LDY #$00

		LDA PAGE.BACKGROUND
	JSR GET.LINE.ADDRESS1

		LDA PAGE.FOREGROUND
	JSR GET.LINE.ADDRESS2

	LDA #$00					;bit mapped byte
	STA (LINE.BASE.ADDR1),Y		;save bit mapped byte to video screen memory (foreground page)  
	STA (LINE.BASE.ADDR2),Y		;save bit mapped byte to video screen memory (background page)

	INY
	STA (LINE.BASE.ADDR1),Y		;save bit mapped byte to video screen memory (foreground page)  
	STA (LINE.BASE.ADDR2),Y		;save bit mapped byte to video screen memory (background page)

	
	INX ;next line (down)
	
	CPX #$C0
	BNE .LOOP.ERASE

	
	
			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; JSR FLIP.PAGE

			; jsr keyin
			; LDA #$AA
			; jmp full.brk
			; BRK
; .TEMP
			; LDA TEMP		

		
@END
	
	;**FALLS THROUGH**
@END
		
			;**OPT** Memory. In the three sections below, put the text blocks in a common area and eliminate the JMPs 
.DRAW.RIGHT_WINDOW.TEXT ;/aka the small right side window (4 chars wide)
@START
	;JMP .SKIP

	JSR HCG.ON

;PRINT "GOLD"	
		LDA #PLAYER.GOLD.HTAB
		STA HTAB	
		LDA #PLAYER.GOLD.VTAB
		STA VTAB
	JSR	UPDATE.CHAR.POS
		
	JMP .TEXT1.PRINT

.TEXT1 .AZ -/Gold/			;ASCII text string
.TEXT1.PRINT
		LDA #.TEXT1 					
		STA STRING
		
		LDA /.TEXT1
		STA STRING+$1
				;LDA #$7F
	JSR PRINT.STR

			; LDA #$00
			; BEQ *
			; LDA #$AA
			; JSR PREP.BRK
			; BRK	

		
;PRINT "9999"	
	LDA #$24
	STA HTAB	
	LDA #$2
	STA VTAB
	JSR	UPDATE.CHAR.POS
	
		LDA PLAYER.GOLD+$0
		STA BIN+$0
		LDA PLAYER.GOLD+$1
		STA BIN+$1
		
		CLC ;(CLC = don't print CR | SEC = print CR to text window)
	JSR PRINT.HEX16.RJ
				
	JSR TIME.DISPLAY
	
@END

.DRAW.TOP_WINDOW.TEXT
@START
;PRINT "  nox archaist "	
		LDA #$B
		STA HTAB	
		LDA #$0
		STA VTAB
	JSR	UPDATE.CHAR.POS
		
	JMP .TEXT3.PRINT

;.TEXT4 .AZ -/               /			;ASCII text string
.TEXT3 .AZ -/  Nox Archaist /			;ASCII text string
.TEXT3.PRINT
		LDA #.TEXT3					
		STA STRING
		
		LDA /.TEXT3
		STA STRING+$1						
	JSR PRINT.STR

			; LDA #$7F
			; STA COUT_CHAR_TYPE ;($00 = normal, $7F = inverse)
			; LDA #$C1 ;ASCII 'A'
		; JSR COUT
	
@END

.DRAW.BOTTOM_WINDOW.TEXT
@START
;PRINT "               "	
		LDA #$B
		STA HTAB	
		LDA #$17
		STA VTAB
	JSR	UPDATE.CHAR.POS
		
		LDA #GLOBAL.TEXT_BLOCK.CLEAR_BOTTOM.TW		
		STA STRING
		
		LDA /GLOBAL.TEXT_BLOCK.CLEAR_BOTTOM.TW
		STA STRING+$1						
	JSR PRINT.STR


	
.EXIT.HRCG	

				
.SKIP
@END



	LDA #$00					;RESET TRACE
	STA CALLED_BY.DRAW.SCREEN
	

	RTS

@END
@END

DRAW.ROW.SINGLE ; 	;=============DRAWS A SINGLE TILE ROW AT SPECIFIED LOCATION==========
@START
;PARAMETERS: SMAP, X-REG (TILE ROW # TO DRAW)
;RETURN: NONE	
;ENTRANCE: DIRECT




;=====================SUBROUTINE DOCUMENTATION====================================
;This subroutine makes calls to DRAW.ROW (which calls DRAW.TILE) to do the graphics plotting for the tiles in the row specified by the parameterse passed to this routine.
;This routine is responsible for the following:
;			a) setup the starting graphics screen screen byte and line for the ROW to be drawn. 
;			b) update the Tile_ID data in SCREEN.TILE.DATA for the row to be drawn.
;				This step is performed because it is assumed this routine was designed to be called
;				After the graphic scrolling functions are performed after a player move. The scrolling functions scroll the graphics screen memory addresses and the Tile_ID data in SCREEN.TILE.DATA. 
;				This routine effectively inserts a new row of TilE_IDs in SCREEN.TILE.DATA to reflect the new terrain the player should see after the player's move is complete. 
;=================================================================================




;**OPT**. Speed. All the drawing routines except ERASE.TILE were written before the lookup tables
;were created for to obtain the starting screen byte and line using the screen tile location as the index.
;(SCREEN.INDEX.TILE_SBYTE & SCREEN.INDEX.TILE_LINE)
;it may be faster to use these lookup tables and increment a register than to use math to increment the
;graphics screen variables. 

.INIT.MAP

	LDA SMAP					;**OPT** speed memory. calling as a precaution until after most map development is done, I'm not sure if it will be needed. 
	STA SMAP.CURRENT
	LDA SMAP+$1
	STA SMAP.CURRENT+$1
				
.INIT.SCREEN					;SETUP BYTE, LINE, DEPTH

;DETERMINE ROW TO DRAW BASED ON X-REG VALUE	
	CPX #$0A
	BEQ .ROW0A

;DEFAULT TO ROW0
	LDY #$00					;INDEX FOR SCREEN.TILE.DATA USED IN DRAW.ROW
	LDX #$00					;INDEX FOR SCREEN.TILE.DATA USED IN TILE.LOOKUP.ROW

	
	LDA	#SCREEN.DRAW.START_LINE										
	STA TILE.LINE.START			;SET STARTING LINE TO DRAW ON
	STA TILE.LINE
	
	
	JMP .COMMON
	
.ROW0A
	LDY #SCREEN.ARRAY.LAST_ROW_START	;INDEX FOR SCREEN.TILE.DATA	USED IN DRAW.ROW
	LDX #SCREEN.ARRAY.LAST_ROW_START	;INDEX FOR SCREEN.TILE.DATA USED IN TILE.LOOKUP.ROW
	
	LDA #SCREEN.DRAW.STOP_LINE2		;THIS ALTERNATE VALUE IS +1 IS THE FIRST LINE IN THE LAST TILE ROW, WHICH IS WHERE WE WANT TO START TO DRAW TILES IN THE LAST ROW	
	CLC								;	I'M NOT SURE REALLY WHY IT'S +1, I DIDN'T CALCUALTE IT OUT, I JUST ADDED +1 TO SEE IF IT WOULD FIX A SLIGHT GLITCH WHEN MOVING SOUTH (SCROLLING UP), AND IT DID.
	ADC #$01	
	STA TILE.LINE.START				;SET STARTING LINE TO DRAW ON
	STA TILE.LINE

;ADJUST SMAP.CURRENT TO FIRST TILE IN LAST ROW
	LDA RMAP
	STA OP1
	LDA RMAP+$1
	STA OP1+$1
	
	LDA #OFFSET.SCREEN.LL
	STA OP2
	LDA #$00
	STA OP2+$1
	
	JSR ADC.16				;GMAP(2) + OFFSET.SCREEN.LL(1)
;**OPT** Speed. Put the code for 16-bit math functions, in graphics routinee, in-line, instead of JSR

	LDA RESULT
	STA SMAP.CURRENT
	LDA RESULT+$1
	STA SMAP.CURRENT+$1
			
	
.COMMON
	
	JSR TILE.LOOKUP.ROW
		
	LDA	#SCREEN.DRAW.START_BYTE									
	STA SCREEN.DRAW.CURRENT_BYTE				;SET STARTING SCREEN BYTE		
	
	JSR DRAW.ROW
		;**OPT** Speed. See if this routine can be allowed to fall through, eliminate the JSR above and the RTS below. 
	RTS

@END

DRAW.ROW ;			;=============DRAWS ROW FROM TILE DATA, AT SPECIFIED LOCATION=========
@START
;PARAMETERS: SMAP.CURRENT, #SCREEN.BYTE.START, SCREEN.DRAW.START_LINE, TILE_LINE_START, TILE_LINE
;	NOTE: SET TILE_LINE == TO TILE_LINE_START before calling this routine. This saves an LDA instruction versus doing an init of TILE_LINE in this routine. 
;RETURN: NONE
;ENTRANCE: DRAW.SCREEN, DRAW.ROW.SINGLE



;**OPT** Memory. Use 16 bit multiplication to calculate shape table address by tile type. Saves the 2 pages of memory used by TILE.SHAPES.HO/LO. Applies to DRAW.COLUMN, DRAW.ROW, DRAW.TILE.SINGLE, DRAW.TILE.FOREGROUND.SINGLE, ANIMATION.UPDATE

;**OPT** Speed. The tile shape table gets copied into the hopper. Maybe there are two hoppers. Current and last. Draw.tile continues to use a zero page variable (shape) and if current tile == last tile, then point (shape) to tile.hopper.last There is an extra cost in draw.tile of the indirect lookup to (shape) instead of a indexed lookup directly to a single hopper, but I bet the gains offset this. Avoiding copying a tile shape table is avoiding a 32 iteration loop * X clock cycles. Almost every region will probably have long strings of tiles....grass, forest, water, mountains, often appear in clusters. Applies to DRAW.COLUMN, DRAW.ROW, DRAW.TILE.SINGLE, DRAW.TILE.FOREGROUND.SINGLE, ANIMATION.UPDATE. Gains likely especially large in animation update, as water tile drawing is lots of repeat tiles. 

;**Opt** Speed. skip draw if animated tile ID or if MO present


;=====================SUBROUTINE DOCUMENTATION====================================
;This subroutine isn't designed for direct entrance. Management of some graphics screen variables
;is required by the calling routine.
;
;This subroutine is responsible for the following:
;			a) making calls to DRAW.TILE to do the graphics plotting for a specific tile at a specific screen tile location.
;			b) to make the calls, this routine increments the screen byte, which effectively increments the screen tile location. 
;				Line remains the same. The increments to line to draw a particular tile are handled by DRAW.TILE
;			d) iterating through the Tile_IDs in the row and copying the shape table data associated with the Tile_ID from
;			   auxiliary memory into a main memory shape table buffer, and setting a zero page pointer to this buffer used by DRAW.TILE
;			   This design was to enable a future speed improvment where the aux memory copy is skipped if the Tile_ID is the same as the last. 
;			a) Verifying whether a tile in each screen location should actually be drawn (i.e. is the tile hidden) and skip the draw if appropriate. 
;=================================================================================




.LOOP					

;LOAD TILE_TYPE OF THE NEXT TILE IN CURRENT ROW
	LDA SCREEN.TILE.DATA,Y			

; .CHECK.DARKNESS	
; ;IS TILE HIDDEN (DARKNESS)?	
	; LDX SCREEN.DARK.DATA,Y			
	; CPX #$01
	; BEQ .SKIPDRAW.STEP				;IF YES, DON'T DRAW TILE
	; JMP .CALC.SHAPE.TABLE
; .SKIPDRAW.STEP
	; JMP .SKIPDRAW
	
.CALC.SHAPE.TABLE
;CALCULATE SHAPE TABLE ADDRESS	
	TAX								
	LDA TILE.SHAPES.LO,X
	STA AUX_MOVE.START				;SAVE BASE ADDRESS AS START ADDRESS FOR AUX MEMORY MOVE

	CLC
	ADC #SHAPE.SIZE
	STA AUX_MOVE.END
	
	LDA TILE.SHAPES.HO,X
	STA AUX_MOVE.START+$1
	STA AUX_MOVE.END+$1

	CPX #ANIMATION.TILE_RANGE.START	;IS CURRENT TILE AN ANIMATION TILE?
	BCS	.ANIMATION.FRAME.OFFSET		;IF YES, CALCULATE AN OFFSET TO BASE ADDRESS VIA THE CURRENT ANIMATION FRAME
	JMP .NOT_ANIMATED
.ANIMATION.FRAME.OFFSET	
;ANIMATION.FRAME_STATE holds the current animation frame number for all animation tiles
;on the view screen. This variable can be used as an offset to the shape table base address
;to identify the address for the current animation frame for the shape table already loaded
;into SHAPE in the routine above.
;
;Each frame is $20 bytes (!32), so we can use ASL multiplication to calcualte the offset. 
	
	
	LDA ANIMATION.FRAME_STATE		;LOAD CURRENT ANIMATION FRAME
	
	ASL ;X2						
	ASL ;X4							
	ASL ;X8							
	ASL ;X16						
	ASL ;X32

;**OPT** if there were 4 or 8 animation frames, 8-bit addition could be used for the offset
;because page boundaries would never be crossed by the offset. 

	STA OP1							;SAVE RESULT OF MULTIPLCATION (ASLs) ABOVE				
	LDA #$00
	STA OP1+$1						
				
	LDA AUX_MOVE.START
	STA OP2
	LDA AUX_MOVE.START+$1
	STA OP2+$1
	
;=======INLINE CODE FOR ADC.16========	
;AUX_MOVE.START(2)+ ACC(1) [ANIMATION FRAME OFFSET]


; DO THE MATH
	CLD 
    CLC                          ;ALWAYS BEFORE ADD
    LDA OP1
    ADC OP2
    STA AUX_MOVE.START
		 
    LDA OP1+$1
    ADC OP2+$1					;carry flag not cleared via CLC intentionally, it's part of 16-bit adding. 
    STA AUX_MOVE.START+$1
	
;======================================
	
	LDA AUX_MOVE.END
	STA OP2
	LDA AUX_MOVE.END+$1
	STA OP2+$1
	
;=======INLINE CODE FOR ADC.16========	
;AUX_MOVE.END(2)+ ACC(1) [ANIMATION FRAME OFFSET]


; DO THE MATH 
	CLD 
    CLC                          ;ALWAYS BEFORE ADD
    LDA OP1
    ADC OP2
    STA AUX_MOVE.END
		 
    LDA OP1+$1
    ADC OP2+$1					;carry flag not cleared via CLC intentionally, it's part of 16-bit adding. 
    STA AUX_MOVE.END+$1
	
;======================================

.NOT_ANIMATED

	LDA #SHAPE.HOPPER0				;SAVE SHAPE.HOPPER0 AS THE DESTINATION ADDRESS FOR AUX MOVE
	STA AUX_MOVE.DEST
	STA SHAPE						;CONNECTS SHAPE.HOPPER0 TO SHAPE, USED BY DRAW.TILE
	LDA /SHAPE.HOPPER0
	STA AUX_MOVE.DEST+$1
	STA SHAPE+$1			

	CLC								;EXECUTE AUX MEMORY MOVE
	JSR AUX_MOVE
	
	JSR DRAW.TILE					;DRAW SHAPE
	
	
;ADJUST VARIABLES AND COUNTERS	
	LDA TILE.LINE.START
	STA TILE.LINE					;RESET CURRENT LINE IN TILE TO DRAW TO THE STARTING LINE

.SKIPDRAW
	INY								;NEXT TILE_ID 
	
	INC SCREEN.DRAW.CURRENT_BYTE	;NEXT SCREEN TILE LOCATION
    INC SCREEN.DRAW.CURRENT_BYTE	

	LDA #SCREEN.DRAW.STOP_BYTE			;AT RIGHT SIDE SCREEN EACH?
	CMP SCREEN.DRAW.CURRENT_BYTE
	BCS .LOOP.STEP						;IF NO, SWITCH OVER TO NEXT ROW (BCS: is ACC >= CMP value)
	JMP .EXIT
.LOOP.STEP
	JMP .LOOP

.EXIT	
	RTS
@END

DRAW.COLUMN.SINGLE 	;=============DRAWS A SINGLE TILE COLUMN AT SPECIFIED LOCATION==========
@START
;PARAMETERS: SMAP, X-REG (SCREEN COLUMN # TO DRAW)
;RETURN: NONE	
;ENTRANCE: DIRECT


;=====================SUBROUTINE DOCUMENTATION====================================
;This subroutine makes calls to DRAW.ROW (which calls DRAW.TILE) to do the graphics plotting for the tiles in the column specified by the parameterse passed to this routine.
;This routine is responsible for the following:
;			a) setup the starting graphics screen screen byte and line for the column to be drawn. 
;			b) update the Tile_ID data in SCREEN.TILE.DATA for the column to be drawn.
;				This step is performed because it is assumed this routine was designed to be called
;				After the graphic scrolling functions are performed after a player move. The scrolling functions scroll the graphics screen memory addresses and the Tile_ID data in SCREEN.TILE.DATA. 
;				This routine effectively inserts a new column of TilE_IDs in SCREEN.TILE.DATA to reflect the new terrain the player should see after the player's move is complete. 
;=================================================================================


.INIT.MAP
	
	LDA SMAP					;**OPT** speed memory. calling as a precaution until after most map development is done, I'm not sure if it will be needed. 
	STA SMAP.CURRENT
	LDA SMAP+$1
	STA SMAP.CURRENT+$1
	
.INIT.SCREEN					;SETUP BYTE, LINE, DEPTH

;DETERMINE COLUMN TO DRAW BASED ON X-REG VALUE	
	CPX #$11
	BEQ .COLUMN11 

;DEFAULT TO COLUMN0
	LDY #$00									;ARRAY ELEMENT OF FIRST TILE IN THE COLUMN, USED IN TILE.LOOKUP.COLUMN

	LDA	#SCREEN.DRAW.START_BYTE											
	STA SCREEN.DRAW.CURRENT_BYTE				;SET STARTING SCREEN BYTE			

;LOAD NEW COLUMN OF TILES	
				
	JSR TILE.LOOKUP.COLUMN						;LOAD NEW COLUMN OF TILES INTO SCREEN.TILE.DATA

	JMP .COMMON
	
.COLUMN11
	LDY #SCREEN.ARRAY.LAST_COLUMN_START			;ARRAY ELEMENT OF THE FIRST TILE IN THE LAST COLUMN, TILE.LOOKUP.COLUMN

	LDA	#SCREEN.DRAW.STOP_BYTE									
	STA SCREEN.DRAW.CURRENT_BYTE				;SET STARTING SCREEN BYTE		
	
	;LOAD NEW COLUMN OF TILES	
	LDA SMAP.CURRENT							;ADJUST SMAP.CURRENT TO THE 1ST TILE OF THE LAST COLUMN
	STA OP1
	LDA SMAP.CURRENT+$1
	STA OP1+$1
	LDA #OFFSET.SCREEN.UR						;CALCULATE TILE_ID OF TILE IN UPPER LEFT CORNER OF SCREEN
	STA OP2
	LDA #$00
	STA OP2+$1
				
	JSR ADC.16									;SMAP.CURRENT(2) - #SCREEN.ROW.SIZE(1)
			
	LDA RESULT									;SAVE TILE_ID OF UPPER RIGHT SCREEN TILE
	STA SMAP.CURRENT

	LDA RESULT+$1
	STA SMAP.CURRENT+$1
			
	JSR TILE.LOOKUP.COLUMN						;LOAD NEW COLUMN OF TILES INTO SCREEN.TILE.DATA

	

.COMMON


	LDA #SCREEN.DRAW.START_LINE										
	STA TILE.LINE				;SET CURRENT LINE TO PLOT WITHIN TILE	
	STA TILE.LINE.START			;SET STARTING LINE FOR TILES IN ROW 0
	

	JSR DRAW.COLUMN	
;**OPT** Speed. This JSR can probably be removed if this routine is redesgined
;to fall into draw.column. if draw.column relies on this routine for looping, just create
;another entry point for access via branch or JMP. 
;this may also be possible with draw.row.single and draw.row; though a branch or jmp
;isn't practical because draw.row is also called by draw.screen....but I don't think there
;is looping between them. 

	RTS
@END	

DRAW.COLUMN ; 		; ============DRAWS COLUMN FROM TILE DATA, AT SPECIFIED LOCATION=========
@START
;PARAMETERS: SMAP.CURRENT(2)
;RETURN: NONE	
;ENTRANCE: DRAW.COLUMN.SINGLE, (FUTURE)

;**Opt** Speed. skip draw if animated tile ID or if MO present


;=====================SUBROUTINE DOCUMENTATION====================================
;This subroutine isn't designed for direct entrance. Management of some graphics screen variables
;is required by the calling routine.
;
;This subroutine is responsible for the following:
;			a) making calls to DRAW.TILE to do the graphics plotting for a specific tile at a specific screen tile location.
;			b) to make the calls, this routine increments the screen byte, which effectively increments the screen tile location. 
;				Line remains the same. The increments to line to draw a particular tile are handled by DRAW.TILE
;			d) iterating through the Tile_IDs in the column and copying the shape table data associated with the Tile_ID from
;			   auxiliary memory into a main memory shape table buffer, and setting a zero page pointer to this buffer used by DRAW.TILE
;			   This design was to enable a future speed improvment where the aux memory copy is skipped if the Tile_ID is the same as the last. 
;			a) Verifying whether a tile in each screen location should actually be drawn (i.e. is the tile hidden) and skip the draw if appropriate. 
;=================================================================================


;FUTURE: if it were useful at some point to draw multiple columns in a row, I think this
;routine could be called from a loop which did two INCs to SCREEN.DRAW.CURRENT_BYTE after the call. HORSE MOVEMENT?
;In this scenario, TILE.LINE.START would need to be rest to the top of the screen. OR, don't increment tile.line.start, 
;at all in this routine and just increment TILE.LINE. I think if one line is added to TILE.LINE at each iteration of TIEL.DRAW, that will
;sync TILE.LINE up with the next tile.

	
;**OPT** MEMORY. MAYBE THIS ROUTINE COULD BE COMBINED WITH THE DRAW.ROW AND ROW OR COLULMN IS
;REQUESTED VIA THE CALLING ROUTINE BY A PARAMETER. 	

;**OPT** Speed. Change do draw.row/colum falls through to its own copy of draw.tile instead of JSR
		
	LDY #$00						;START WITH 1ST ELEMENT IN SCREEN.TILE.HOPPER ARRAY.
.LOOP					
;LOAD TILE_TYPE OF THE NEXT TILE IN CURRENT ROW
	LDA SCREEN.TILE.HOPPER,Y			

; ;IS TILE DEEP WATER?
; ;(note: this tally is used to determine whether to force animation to complete when entire screen is deep water tiles)
	; CMP #TILE_ID.DEEP_WATER
	; BNE .CHECK.DARKNESS
	; INC ANIMATION.DEEP_WATER.TALLY
	; ;**FALLS THROUGH**
	
; .CHECK.DARKNESS	
; ;IS TILE HIDDEN (DARKNESS)?	
	; LDX SCREEN.DARK.HOPPER,Y			
	; CPX #$01
	; BEQ .SKIPDRAW.STEP				;IF YES, DON'T DRAW TILE
	; JMP .CALC.SHAPE.TABLE
; .SKIPDRAW.STEP
	; JMP .SKIPDRAW
	
.CALC.SHAPE.TABLE
;CALCULATE SHAPE TABLE ADDRESS	
	TAX								
	LDA TILE.SHAPES.LO,X
	STA AUX_MOVE.START				;SAVE BASE ADDRESS AS START ADDRESS FOR AUX MEMORY MOVE
	CLC
	ADC #SHAPE.SIZE
	STA AUX_MOVE.END
	
	LDA TILE.SHAPES.HO,X
	STA AUX_MOVE.START+$1
	STA AUX_MOVE.END+$1

	CPX #ANIMATION.TILE_RANGE.START	;IS CURRENT TILE AN ANIMATION TILE?
	BCS	.ANIMATION.FRAME.OFFSET		;IF YES, CALCULATE AN OFFSET TO BASE ADDRESS VIA THE CURRENT ANIMATION FRAME
	JMP .NOT_ANIMATED
.ANIMATION.FRAME.OFFSET	
;ANIMATION.FRAME_STATE holds the current animation frame number for all animation tiles
;on the view screen. This variable can be used as an offset to the shape table base address
;to identify the address for the current animation frame for the shape table already loaded
;into SHAPE in the routine above.
;
;Each frame is $20 bytes (!32), so we can use ASL multiplication to calcualte the offset. 
	
	
	LDA ANIMATION.FRAME_STATE		;LOAD CURRENT ANIMATION FRAME
	
	ASL ;X2						
	ASL ;X4							
	ASL ;X8							
	ASL ;X16						
	ASL ;X32

;**OPT** if there were 4 or 8 animation frames, 8-bit addition could be used for the offset
;because page boundaries would never be crossed by the offset. 

	STA OP1							;SAVE RESULT OF MULTIPLCATION (ASLs) ABOVE				
	LDA #$00
	STA OP1+$1						
				
	LDA AUX_MOVE.START
	STA OP2
	LDA AUX_MOVE.START+$1
	STA OP2+$1
	
;=======INLINE CODE FOR ADC.16========	
;AUX_MOVE.START(2)+ ACC(1) [ANIMATION FRAME OFFSET]


; DO THE MATH
	CLD 
    CLC                          ;ALWAYS BEFORE ADD
    LDA OP1
    ADC OP2
    STA AUX_MOVE.START
		 
    LDA OP1+$1
    ADC OP2+$1					;carry flag not cleared via CLC intentionally, it's part of 16-bit adding. 
    STA AUX_MOVE.START+$1
	
;======================================
	
	LDA AUX_MOVE.END
	STA OP2
	LDA AUX_MOVE.END+$1
	STA OP2+$1
	
;=======INLINE CODE FOR ADC.16========	
;AUX_MOVE.END(2)+ ACC(1) [ANIMATION FRAME OFFSET]


; DO THE MATH 
	CLD 
    CLC                          ;ALWAYS BEFORE ADD
    LDA OP1
    ADC OP2
    STA AUX_MOVE.END
		 
    LDA OP1+$1
    ADC OP2+$1					;carry flag not cleared via CLC intentionally, it's part of 16-bit adding. 
    STA AUX_MOVE.END+$1
	
;======================================

.NOT_ANIMATED

	LDA #SHAPE.HOPPER0				;SAVE SHAPE.HOPPER0 AS THE DESTINATION ADDRESS FOR AUX MOVE
	STA AUX_MOVE.DEST
	STA SHAPE						;CONNECTS SHAPE.HOPPER0 TO SHAPE, USED BY DRAW.TILE
	LDA /SHAPE.HOPPER0
	STA AUX_MOVE.DEST+$1
	STA SHAPE+$1			

	CLC								;EXECUTE AUX MEMORY MOVE
	JSR AUX_MOVE
	
	JSR DRAW.TILE					;DRAW SHAPE
	

;**OPT** SPEED/MEMORY. TILE.LINE IS IN ACC AT END OF DRAW.TILE. I MAY BE ABLE TO INCREMENT IT A LINE TO REACH THE START OF THE NEXT TILE INSTEAD OF LOADING TILE.START LINE
;EACH TIME. GIVE EXTRA THOUGHT TO THINKING THIS THROUGH IF THIS ROUTINE ENDS UP BEING CALLED IN A LOOP TO DRAW MULTIPLE COLUMNS. HORSE MOVEMENT?
;

.SKIPDRAW
	INY								;INCREMENT COUNTER SO WE GET THE NEXT TILE_TYPE ON THE NEXT INTERATION OF .LOOP
	
	LDA TILE.LINE.START				;ADVANCE LINE TO NEXT ROW OF TILES
	CLC
	ADC #TILE.DEPTH.STANDARD
	STA TILE.LINE.START
	STA TILE.LINE		
	CMP	#SCREEN.DRAW.STOP_LINE			;READY TO SWITCH TILE COLUMNS?
	BCS .EXIT						;IF YES, EXIT (BCS: is ACC >= CMP value)

	JMP .LOOP

.EXIT
	RTS
@END

LOAD.PLAYER.WALKING.ICON ;========LOADS A FRESH COPY OF PLAYER'S WALKING ICON====
@START
;PARAMETERS:  NONE
;RETURN: NONE
;ENTRANCE: DIRECT

;=====================SUBROUTINE DOCUMENTATION====================================
;This subroutine updates the player icon buffer with a fresh copy of the shape table
;for the players active walking tile icon. Sometime the player icon buffer data is modified
;for special effects like sinking. This routine is how the game returns the player icon to "normal"
;=================================================================================


	

	;CALCULATE SHAPE TABLE ADDRESS	
	LDX PLAYER.WALKING.TILE			;THE TILE_ID IS THE INDEX TO LOOKUP THE AUX MEMORY ADDRESS THE SHAPE TABLE IS STORED AT
	
	LDA TILE.SHAPES.LO,X
	STA AUX_MOVE.START				;SAVE BASE ADDRESS AS START ADDRESS FOR AUX MEMORY MOVE
	CLC
	ADC #SHAPE.SIZE
	STA AUX_MOVE.END
	
	LDA TILE.SHAPES.HO,X
	STA AUX_MOVE.START+$1
	STA AUX_MOVE.END+$1

;LOAD SHAPE TABLE (ALWAYS 1ST FRAME OR BYTES WILL BE OUT OF SYNC)
	LDA #SHAPE.HOPPER0				;SAVE SHAPE.HOPPER0 AS THE DESTINATION ADDRESS FOR AUX MOVE
	STA AUX_MOVE.DEST
;	STA SHAPE						;CONNECTS SHAPE.HOPPER0 TO SHAPE, USED BY DRAW.TILE
	LDA /SHAPE.HOPPER0
	STA AUX_MOVE.DEST+$1
;	STA SHAPE+$1

	CLC								;EXECUTE AUX MEMORY MOVE
	JSR AUX_MOVE
		
	LDA #PLAYER.ICON.BUFFER							
	STA SHAPE										 
	LDA /PLAYER.ICON.BUFFER
	STA SHAPE+$1
	

;INIT INDEXES
	LDX #$00
	LDY #$00
	
.LOOP.COPY	;COPY AUX MEMORY SHAPE TABLE FOR PLAYER ICON INTO MAIN MEMORY PLAYER ICON BUFFER, FOR USE BY DRAW.TILE AND OTHER ROUTINES
	LDA SHAPE.HOPPER0,X				;COPY FROM
	STA (SHAPE),Y					;COPY TO (PLAYER.ICON.BUFFER)
	INX
	INY
	CPY #TILE.SHAPE.SIZE
	BNE .LOOP.COPY
	
	RTS
@END	


	
DRAW.TILE.PLAYER ;	;=============DRAWS PLAYER ICON===========================
@START
;PARAMETERS:  NONE
;RETURN: NONE
;ENTRANCE: DIRECT

	

				; LDA PLAYER.TILE.ACTIVE
				; LDX #PLAYER.TRANSPORT.MT.TILE_LOCATION0	
				; LDY SAVED_TILE_TYPE
				; BRK	

;=====================SUBROUTINE DOCUMENTATION====================================
;This subroutine draws a player icon in the location designed by various constants.
;The constants are fixed to the center of the screen (or aproximate center in the case of a multi-tile transport icon like a frigate)
;
;The shape table for the player icon is determined by the value in PLAYER.TILE.ACTIVE.
;This variables is modified when the player (B)oards (X)it's transport objects via MO.BOARD and MO.EXIT 
;
;This routine executes the draw via DRAW.TILE.SINGLE or DRAW.TILE. This 
;routine is responsible for:
;		a) determining the Tile_ID to draw
;		b) detecting if the player is standing in tall grass and if so modifing TILE.DEPTH to half the usual amount before calling DRAW.TILE
;		c) setting the start values for graphics screen variables in cases where DRAW.TILE is called.
;=================================================================================


.CHECK.STORM
	;is a storm MOB located at player position? (if yes, then always drawn animation even if it is the player location)
	
	LDY #SCREEN.ARRAY.PLAYER_LOCATION	;use player's screen location as the index to the sprite screen array
	LDA SCREEN.MO_SPRITE.DATA,Y
	CMP #$FF		;is a sprite located at the player's screen location?
	BEQ .NO.STORM	;if no, then no storm present 
	RTS				;return to either MOVE.PASS or MOVE.COMMON.ROUTINE 
	;***currently we don't check the TILE_ID of the sprite because there are no other sprites that are drawn over the player icon.
.NO.STORM
			

.LOOKUP.PLAYER.TRANSPORT.MODE			
	LDA PLAYER.TILE.ACTIVE
	CMP #TILE_ID.HORSE_C						;BRANCH BASED ON THE TYPE OF TRANSPORT 
	BEQ .HORSE
	CMP #TILE_ID.FRIGATE1.1
	BEQ .FRIGATE	
	CMP #TILE_ID.CARAVEL
	BEQ .CARAVEL
	CMP #TILE_ID.SKIFF
	BEQ .SKIFF
	CMP #TILE_ID.WYVERN
	BEQ .WYVERN

	JMP .STANDARD_ICON

.HORSE	
	LDA PLAYER.MOVE.CURRENT		;#$00=north, $01=south, $02=east, $03=west, $04=PASS
	
	CMP #$00
	BEQ	.HORSE.NORTH_EAST
	CMP #$01
	BEQ .MOVE.SOUTH_WEST
	CMP #$02
	BEQ	.HORSE.NORTH_EAST
	CMP #$03
	BEQ .MOVE.SOUTH_WEST
	CMP #$04
	BEQ .MOVE.SOUTH_WEST

.ERROR
;UNEXPECTED MOVE CODE IN PLAYER.MOVE.CURRENT IN DRAW.PLAYER.ICON
	BRK
	
.HORSE.NORTH_EAST
	LDA #TILE_ID.HORSE_B
	STA SAVED_TILE_TYPE		
	JMP .DRAW.TILE.SINGLE
	
.MOVE.SOUTH_WEST	
	LDA #TILE_ID.HORSE_A
	STA SAVED_TILE_TYPE		
	JMP .DRAW.TILE.SINGLE	

.FRIGATE			
	LDY #PLAYER.TRANSPORT.MT.TILE_LOCATION0		;LOAD NEXT SCREEN LOCATION
	LDA #TILE_ID.FRIGATE1.1						;LOAD FIRST TILE TYPE IN MULTI-TILE OBJECT
	STA SAVED_TILE_TYPE							;PARAMETER FOR DRAW.TILE.SINGLE (TILE_TYPE)
	JSR DRAW.TILE.SINGLE						;DRAW TILE


	LDY #PLAYER.TRANSPORT.MT.TILE_LOCATION1		;LOAD NEXT SCREEN LOCATION
	LDA #TILE_ID.FRIGATE1.2						;LOAD SECOND TILE TYPE IN MULTI-TILE OBJECT
	STA SAVED_TILE_TYPE							;PARAMETER FOR DRAW.TILE.SINGLE (TILE_TYPE)
	JSR DRAW.TILE.SINGLE						;DRAW TILE

	LDY #PLAYER.TRANSPORT.MT.TILE_LOCATION2		;LOAD NEXT SCREEN LOCATION
	LDA #TILE_ID.FRIGATE1.3						;LOAD THIRD TILE TYPE IN MULTI-TILE OBJECT
	STA SAVED_TILE_TYPE							;PARAMETER FOR DRAW.TILE.SINGLE (TILE_TYPE)
	JSR DRAW.TILE.SINGLE						;DRAW TILE

	LDY #PLAYER.TRANSPORT.MT.TILE_LOCATION3		;LOAD NEXT SCREEN LOCATION
	LDA #TILE_ID.FRIGATE1.4						;LOAD FOURTH TILE TYPE IN MULTI-TILE OBJECT
	STA SAVED_TILE_TYPE							;PARAMETER FOR DRAW.TILE.SINGLE (TILE_TYPE)
	JSR DRAW.TILE.SINGLE						;DRAW TILE

	
	RTS

.CARAVEL
	LDA #TILE_ID.CARAVEL
	STA SAVED_TILE_TYPE		
	JMP .DRAW.TILE.SINGLE	

.SKIFF		
	LDA #TILE_ID.SKIFF
	STA SAVED_TILE_TYPE		
	JMP .DRAW.TILE.SINGLE

.WYVERN
	LDA #TILE_ID.WYVERN
	STA SAVED_TILE_TYPE		
	JMP .DRAW.TILE.SINGLE	


;=======================	
.STANDARD_ICON
	LDY #SCREEN.ARRAY.PLAYER_LOCATION
						
	LDA #PLAYER.ICON.BUFFER				
	STA SHAPE
	LDA /PLAYER.ICON.BUFFER
	STA SHAPE+$1

.SET.DRAW.PARAMETERS	
	LDA SCREEN.INDEX.TILE_SBYTE,Y
	STA	SCREEN.DRAW.CURRENT_BYTE
	
	LDA SCREEN.INDEX.TILE_LINE,Y
	STA TILE.LINE.START
	STA TILE.LINE

	LDA SCREEN.TILE.DATA,Y
.BUILDING.CHECKS
@START
;VALIDATE ENTRANCE

	;is player in building? 
	LDX PLAYER.MAP.LOCATION_TYPE
	CPX #MAP.TYPE.BUILDING.GRE 	;is map type = building?
	BCC .BUILDING.CHECKS.DONE		;if no, skip rules in this section
	CPX #MAP.TYPE.BUILDING.LT	;is map type = building?
	BCS .BUILDING.CHECKS.DONE		;if no, skip rules in this section
	
	;**FALLS THROUGH**  (if yes)

.CHECK.PLAYER_ICON.SUPRESSED.TILES
@START
;(check to see if the terrain tile at the player's location is a tile that 
;covers the player and thus should not be drawn, such as an archway)

	;ACC: contains map terrain tile_ID of player screen location 
.CHECK.ARCHWAY	
	CMP #TILE_ID.BUILDING.ARCHWAY.GRE
	BCC .CHECK.ARCHWAY.DONE
	CMP #TILE_ID.BUILDING.ARCHWAY.LT
	BCS .CHECK.ARCHWAY.DONE
	JMP .EXIT ;don't draw player icon at all; exit
.CHECK.ARCHWAY.DONE

	
@END
	
.CHECK.UNOCCUPIED.TILE_SWAPS
	;ACC: contains map terrain tile_ID of player screen location 
	CMP #TILE_ID.UNOCCUPIED.TILE.SWAP.GRE1	
	BCC .CHECK.UNOCCUPIED.TILE_SWAPS.DONE
	CMP #TILE_ID.UNOCCUPIED.TILE.SWAP.LT1	
	BCS .CHECK.UNOCCUPIED.TILE_SWAPS.DONE
	;RTS
	CLC
	ADC #$01 ;swap in occupied tile
	STA SAVED_TILE_TYPE	
	JMP .DRAW.TILE.SINGLE
.CHECK.UNOCCUPIED.TILE_SWAPS.DONE
	
;IS MAP TERRAIN TILE A BED (LEFT SIDE)?
.CHECK.BED
	;ACC: contains map terrain tile_ID of player screen location 
	CMP #TILE_ID.BED_LEFT_UNOCCUPIED
	BNE .CHECK.COT
	LDA #TILE_ID.BED_LEFT_OCCUPIED
	STA SAVED_TILE_TYPE	
	JMP .DRAW.TILE.SINGLE
	
;IS MAP TERRAIN TILE A COT?
.CHECK.COT
	;ACC: contains map terrain tile_ID of player screen location 
	CMP #TILE_ID.COT_UNOCCUPIED
	BNE .CHECK.OUTHOUSE_HOLE
	LDA #TILE_ID.COT_OCCUPIED
	STA SAVED_TILE_TYPE	
	JMP .DRAW.TILE.SINGLE

;IS MAP TERRAIN TILE A OUTHOUSE HOLE?
.CHECK.OUTHOUSE_HOLE
	;ACC: contains map terrain tile_ID of player screen location 
	CMP #TILE_ID.OUTHOUSE_HOLE.UNOCCUPIED
	BNE .CHECK.OUTHOUSE_HOLE.DONE
	LDA #TILE_ID.OUTHOUSE_HOLE.OCCUPIED
	STA SAVED_TILE_TYPE	
	JMP .DRAW.TILE.SINGLE
.CHECK.OUTHOUSE_HOLE.DONE
.BUILDING.CHECKS.DONE	

@END

;IS MAP TILE TALL GRASS? 
;(ONLY DRAW UPPER HALF OF PLAYER, SO LOWER HALF IS HIDDEN BY GRASS)	

	;ACC: contains map terrain tile_ID of player screen location 
	CMP #TILE_ID.TALL_GRASS_A
	BEQ .TALL_GRASS	
	CMP #TILE_ID.TALL_GRASS_B
    BNE .NOT.TALL_GRASS
.TALL_GRASS
	LDA #TILE.DEPTH.HALF				;ADJUST TILE DEPTH SO ONLY UPPER HALF OF TILE IS DRAWN.	
	STA TILE.DEPTH

	JSR DRAW.TILE
	
	LDA #TILE.DEPTH.STANDARD			;RESET TILE DEPTH TO FULL TILE. 		
	STA TILE.DEPTH	
	JMP .EXIT
.NOT.TALL_GRASS
	
.DRAW.ENTRANCE
	JSR DRAW.TILE
	
.EXIT
	RTS



.DRAW.TILE.SINGLE
	LDY #SCREEN.ARRAY.PLAYER_LOCATION		;REQUIRED PARAMETER FOR DRAW.TILE.SINGLE	

	JMP DRAW.TILE.SINGLE
@END	

;CHARACTER ROSTER WINDOW ROUTINES
@START
DISPLAY.CHARACTER.ROSTER
@START		

;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA
	
			
.INIT.LOOP
	;save cursor position
	LDA HTAB	
	STA CURSOR.POSITION.SAVED+$0
	LDA VTAB
	STA CURSOR.POSITION.SAVED+$1

	;set starting text screen row
	LDA #$1
	STA VTAB

	;set hi-res line to character 1 status bar
	LDA #CHR_ROSTER.STATUS_BAR.START_LINE	
	STA CHR_ROSTER.STATUS_BAR.CURRENT_LINE		

	LDA #$08 ;set the size of the HP & MP status bars
	STA STATUS_BAR.MAX_SEGMENTS.PLUS_ONE
	
	LDX #$01 ;init loop counter and player #
.LOOP.DISPLAY.ROSTER



				
.INIT.INTERATION
@START
	;set cursor position
		LDA #CHR_ROSTER.NAME.START_SBYTE
		;LDA #$01
		STA HTAB	
	JSR	UPDATE.CHAR.POS
		
	;read PC character sheet data
		TXA
		;ACC = player sequential # (high-bit not set = read mode)
	JSR COMBAT.READ_WRITE.CHR_SHEET.PC
		;RETURN VALUE = CHR_SHEET.RECORD.READ


@END
	;**FALLS THROUGH**

	

	
.PRINT.CHARACTER.NAME
@START					
		LDA #CHR_SHEET.PC.NAME.CHARACTER			
		STA STRING
		
		LDA /CHR_SHEET.PC.NAME.CHARACTER
		STA STRING+$1	
		; LDA #$7F
		; STA COUT_CHAR_TYPE ;($00 = normal, $7F = inverse)			
	JSR PRINT.STR
			
@END


.DRAW.HP_MP.STATUS_BAR
@START

	;calculate HP status bar segments
		LDA CHR_SHEET.PC_MOB.HP_LO
		STA PERCENT.GET.PARM.LOW_NUMBER+$0	
		LDA CHR_SHEET.PC_MOB.HP_HO
		STA PERCENT.GET.PARM.LOW_NUMBER+$1	
		LDA CHR_SHEET.PC.HP_MAX.LO
		STA PERCENT.GET.PARM.HIGH_NUMBER+$0	
		LDA CHR_SHEET.PC.HP_MAX.HO
		STA PERCENT.GET.PARM.HIGH_NUMBER+$1
	JSR .GET.STATUS_BAR.SEGEMENTS
		;RETURN ACC = # of segements to draw				
		STA STATUS_BAR.SEGMENTS ;parm of STATUS_BAR.DRAW


			
.EXECUTE.DRAW
@START

;SAVE PARAMETERS
	TXA
	PHA	
	
		;set STATUS_BAR.DRAW parameters

		LDA #$55
		STA STATUS_BAR.BYTE0_VALUE	
			
		LDA #$2A
		STA STATUS_BAR.BYTE1_VALUE

		;STATUS_BAR.SEGMENTS = set above
	

;DRAW HP
			
;DRAW HP: 1ST LINE WIDTH
		LDX CHR_ROSTER.STATUS_BAR.CURRENT_LINE		
		INX	
		LDY #CHR_ROSTER.HP_STATUS_BAR.START_SBYTE	
		LDA #$00 ;set direction code ($00 = increases to left | >=$01 increases to right)
	JSR STATUS_BAR.DRAW
	
		;DRAW CENTER WEDGE PIECE
		;(the white dot that seperates the HP and MP status bars)
		;(only draw if no HP status bar segements
		;because normally one of the bits from the HP status bar joins with the
		;bit drawn above to form the white center wedge (a 2 pixel long white line))
		LDA STATUS_BAR.SEGMENTS
		BNE .DRAW.CENTER.DOT.DONE1.1
			LDY #CHR_ROSTER.HP_STATUS_BAR.START_SBYTE
			LDA #$40
		JSR DRAW.BYTE
.DRAW.CENTER.DOT.DONE1.1

				;JSR KEYIN

;DRAW HP: 2ND LINE WIDTH
		LDY #CHR_ROSTER.HP_STATUS_BAR.START_SBYTE	
		DEX
		;LDX CHR_ROSTER.STATUS_BAR.CURRENT_LINE		
		LDA #$00 ;set direction code ($00 = increases to left | >=$01 increases to right)
	JSR STATUS_BAR.DRAW

		;DRAW CENTER WEDGE PIECE
		;(the white dot that seperates the HP and MP status bars)
		;(only draw if no HP status bar segements
		;because normally one of the bits from the HP status bar joins with the
		;bit drawn above to form the white center wedge (a 2 pixel long white line))
		LDA STATUS_BAR.SEGMENTS
		BNE .DRAW.CENTER.DOT.DONE1.2
			LDY #CHR_ROSTER.HP_STATUS_BAR.START_SBYTE
			LDA #$40
		JSR DRAW.BYTE
.DRAW.CENTER.DOT.DONE1.2

.DRAW.HP.STATUS_BAR.DONE

				;JSR KEYIN
	
.DRAW.MP.VALIDATE ;validate entrance

	LDA STATUS_BAR.SEGMENTS
	BEQ .DRAW.MP.VALIDATE.DONE ;if HP = $00 then skip MP status bar segments calc so that it defaults to $00 and doesn't display, since the PC is dead.

			; STA TEMP
			; PLA
			; TAX
			; CPX #$05
			; BNE .TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; bne .temp
; ;;PC CHARACTER SHEET READ
	; ;read PC character sheet data
		; LDA #$05
		; ;ACC = player sequential # (high-bit not set = read mode)
	; JSR COMBAT.READ_WRITE.CHR_SHEET.PC
		; ;RETURN VALUE = CHR_SHEET.RECORD.READ
		
			; ;TROUBLESHOOTING LOOP
			; LDX #$00
; .TEST.LOOP			
			; LDA CHR_SHEET.RECORD.READ,X
			; STA $BE00,X
			; INX
			; BNE .TEST.LOOP
			
			; LDA #$AA			
			; ; LDx CHR_SHEET.PC_MOB.MP
			; ; LDy CHR_SHEET.PC.ATTRIB.INT	
			; JSR FULL.BRK
; .TEMP
			; TXA
			; PHA
			; LDA TEMP
			
		;calculate MP status bar segments
		LDA CHR_SHEET.PC_MOB.MP
		STA PERCENT.GET.PARM.LOW_NUMBER+$0	
		LDA #$00
		STA PERCENT.GET.PARM.LOW_NUMBER+$1	
		LDA CHR_SHEET.PC.ATTRIB.INT	
		STA PERCENT.GET.PARM.HIGH_NUMBER+$0	
		LDA #$00
		STA PERCENT.GET.PARM.HIGH_NUMBER+$1
	JSR .GET.STATUS_BAR.SEGEMENTS
		;RETURN ACC = # of segements to draw				
		STA STATUS_BAR.SEGMENTS ;parm of STATUS_BAR.DRAW

			; ;LDA #$AA			
			; LDx CHR_SHEET.PC_MOB.MP
			; LDy CHR_SHEET.PC.ATTRIB.INT	
			; JSR FULL.BRK
			
.DRAW.MP.VALIDATE.DONE
		
;DRAW MP: 1ST LINE WIDTH	
		LDA #$55
		STA STATUS_BAR.BYTE0_VALUE
		
		LDA #$2A
		STA STATUS_BAR.BYTE1_VALUE

		LDY #CHR_ROSTER.MP_STATUS_BAR.START_SBYTE	
		LDX CHR_ROSTER.STATUS_BAR.CURRENT_LINE		
		INX
		LDA #$01 ;set direction code ($00 = increases to left | >=$01 increases to right)
	JSR STATUS_BAR.DRAW

		;draw center wedge piece
		;(the white dot that seperates the hp and mp status bars)
		LDA STATUS_BAR.SEGMENTS
		BNE .DRAW.CENTER.DOT.DONE2.1
			LDY #CHR_ROSTER.MP_STATUS_BAR.START_SBYTE
			LDA #$01
		JSR DRAW.BYTE
.DRAW.CENTER.DOT.DONE2.1
		
;DRAW MP: 2ND LINE WIDTH	
		LDY #CHR_ROSTER.MP_STATUS_BAR.START_SBYTE	
		DEX
		LDA #$01 ;set direction code ($00 = increases to left | >=$01 increases to right)
	JSR STATUS_BAR.DRAW

		;draw center wedge piece
		;(the white dot that seperates the hp and mp status bars)
		LDA STATUS_BAR.SEGMENTS
		BNE .DRAW.CENTER.DOT.DONE2.2
			LDY #CHR_ROSTER.MP_STATUS_BAR.START_SBYTE
			LDA #$01
		JSR DRAW.BYTE	
.DRAW.CENTER.DOT.DONE2.2

.DRAW.HP_MP.STATUS_BAR.DONE
		
;RESTORE REGISTERS	
	PLA
	TAX	
@END

			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; CPX #$01 ;PC #1
			; BNE .TEMP
			; JSR KEYIN
			; LDA #$AA	
			; LDX STATUS_BAR.SEGMENTS
			; ; LDX BCD+$0
			; ; LDY BCD+$1
			; JSR FULL.BRK
; .TEMP
			; LDA TEMP

@END


.PRINT.HEALTH_STATUS
@START
	;set cursor position to print health status text
		LDA #CHR_ROSTER.HEALTH_STATUS.SBYTE-1 ;the -1 is because a blank space is printed before the health status code for cosmetics.  
		STA HTAB	
	JSR	UPDATE.CHAR.POS
	
;Select Case: Health Status Code	
	LDA CHR_SHEET.PC.HEALTH_STATUS
	CMP #COMBAT.S_ENTITY.STATUS.POSSESSED
	BEQ .IS.POSSESSED
	CMP #COMBAT.S_ENTITY.STATUS.SLEEPING
	BEQ .IS.SLEEPING	
	JMP .PRINT.HEALTH_STATUS.DONE
	

.IS.SLEEPING
	LDA #$D3 ;ASCII = "S"
	PHA ;push health status ASCII code to stack
	JMP .COMMON_CODE
	
.IS.POSSESSED
	LDA #$D0 ;ASCII = "P"
	PHA ;push health status ASCII code to stack
	
	;**FALLS THROUGH**
.COMMON_CODE
		LDA #$A0 ;ASCII = "space"
	JSR COUT

		LDA #$7F
		STA COUT_CHAR_TYPE ;($00 = normal, $7F = inverse)

		PLA ;pull health status ASCII code from stack
	JSR COUT
		

.PRINT.HEALTH_STATUS.DONE

	;**FALLS THROUGH**

		
@END

.INCREMENT.SCREEN_VARIABLES

	;advance cursor 2 lines, and reset to left edge of text window
		; LDA #$19
		; STA HTAB	
		INC VTAB
		INC VTAB

	;advance hi-res line to next status bar location 
	LDA CHR_ROSTER.STATUS_BAR.CURRENT_LINE		
	CLC
	ADC #CHR_ROSTER.STATUS_BAR.VDIST
	STA CHR_ROSTER.STATUS_BAR.CURRENT_LINE
	
	
			;jsr keyin
			
	;exit test
	CPX PARTY.TOTAL.PC
	BEQ .LOOP.DISPLAY.ROSTER.COMPLETE
	INX	;next PC	
	JMP .LOOP.DISPLAY.ROSTER
.LOOP.DISPLAY.ROSTER.COMPLETE



			
			; jsr keyin
			; lda #$ab
			; ;ldx #CHR_SHEET.RECORD.READ
			; ;ldy /CHR_SHEET.RECORD.READ
			; JSR FULL.BRK
			; brk
			


; ;TROUBLESHOOTING: PC CHARACTER SHEET READ
	; ;read PC character sheet data
		; LDA #$06
		; ;ACC = player sequential # (high-bit not set = read mode)
	; JSR COMBAT.READ_WRITE.CHR_SHEET.PC
		; ;RETURN VALUE = CHR_SHEET.RECORD.READ
;		
			; ;TROUBLESHOOTING LOOP
			; LDX #$00
; .TEST.LOOP			
			; LDA CHR_SHEET.RECORD.READ,X
			; STA $BF00,X
			; INX
			; BNE .TEST.LOOP
;
;
			; LDA #$EE
			; LDX CHR_SHEET.PC_MOB.ARMOR ;PC defense rating
			; ; LDX COMBAT.STATS.DAMAGE.FINAL+$0
			; ; LDY COMBAT.STATS.DAMAGE.FINAL+$1
			; JSR FULL.BRK
			; BRK
			
.EXIT
;RESTORE CURSOR POSITION
		LDA CURSOR.POSITION.SAVED+$0
		STA HTAB	
		LDA CURSOR.POSITION.SAVED+$1
		STA VTAB
	JSR	UPDATE.CHAR.POS	

;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX
	
			; lda #$01
			; sta troubleshooting.hook
			
	RTS

.GET.STATUS_BAR.SEGEMENTS
@START
;PARAMETERS: PERCENT.GET.PARM.LOW_NUMBER(2), PERCENT.GET.PARM.LOW_NUMBER(2)
;ENTRANCE: direct
;RETURN: # of segements to draw


		;PERCENT.GET.PARM.LOW_NUMBER(2) = set as parm to this routine
		;PERCENT.GET.PARM.HIGH_NUMBER(2) = set as parm to this routine
	JSR PERCENT.GET
		;RETURN: BCD(2) = 16-percentage (!0-!100)

	LDA BCD+$1		
	BNE .SET.7_SEGMENTS ;branch if return value >= !100%
	
	SED ;set decimal mode	
	LDA BCD+$0
	CMP #$86
	BCS .SET.7_SEGMENTS
	CMP #$72
	BCS .SET.6_SEGMENTS
	CMP #$58
	BCS .SET.5_SEGMENTS
	CMP #$45
	BCS .SET.4_SEGMENTS
	CMP #$30
	BCS .SET.3_SEGMENTS
	CMP #$15
	BCS .SET.2_SEGMENTS
	CMP #$01
	BCS .SET.1_SEGMENT
	
	;DEFAULT CASE: if none of the above are true, then BCD(2) = $00

	;**FALLS THROUGH**

.SET.0_SEGMENTS
	LDA #$00
	JMP .RETURN.SEGEMENTS
	
.SET.1_SEGMENT
	LDA #$01
	JMP .RETURN.SEGEMENTS

.SET.2_SEGMENTS
	LDA #$02
	JMP .RETURN.SEGEMENTS

.SET.3_SEGMENTS
	LDA #$03
	JMP .RETURN.SEGEMENTS

.SET.4_SEGMENTS
	LDA #$04
	JMP .RETURN.SEGEMENTS

.SET.5_SEGMENTS
	LDA #$05
	JMP .RETURN.SEGEMENTS

.SET.6_SEGMENTS
	LDA #$06
	JMP .RETURN.SEGEMENTS

.SET.7_SEGMENTS
	LDA #$07
	
	;**FALLS THROUGH**

	
.RETURN.SEGEMENTS
	CLD ;clear decimal mode
	
	;RETURN VALUE = ACC
			
			
	RTS
	
@END


;.ROLLING_CLEAR.ROSTER_WINDOW
@START
; ;PARAMETERS:  USE.PAGE*
; ;RETURN: NONE
; ;ENTRANCE: DIRECT
; ;*Specifies which hi-res page the text window should be erased on (BOTH = $03)

; ;=====================SUBROUTINE DOCUMENTATION====================================
; ;
; ;=================================================================================		
			
			
	; PHA ;save USE.PAGE parameter to ACC

; ;.INIT
	; LDA CLEAR_ROSTER.CURRENT.START_LINE	;load line in x register	
	; STA DRAW.START_LINE
	
	; LDA #TWB.ROSTER_WINDOW.LEFT_SBYTE+1
	; STA DRAW.START_BYTE

	; LDA #TWB.ROSTER_WINDOW.RIGHT_SBYTE
	; STA DRAW.STOP_BYTE
		
	; LDA CLEAR_ROSTER.CURRENT.START_LINE
	; CLC
	; ADC #$10
	; STA DRAW.STOP_LINE

; ;.DRAW
		; PLA ;restore USE.PAGE parameter to ACC
		; ORA #$80	;set high bit as a parameter to indicate we want the byte value for the draw to be $00
	; JSR DRAW.LINE

; ;INCREMENT 
	; LDA CLEAR_ROSTER.CURRENT.START_LINE
	; CLC
	; ADC #$10
	; STA CLEAR_ROSTER.CURRENT.START_LINE



	; RTS
@END

	
;.FULL_CLEAR.ROSTER_WINDOW
@START
; ;PARAMETERS:  USE.PAGE*
; ;RETURN: NONE
; ;ENTRANCE: DIRECT
; ;*Specifies which hi-res page the text window should be erased on (BOTH = $03)

; ;=====================SUBROUTINE DOCUMENTATION====================================
; ;
; ;=================================================================================

	; PHA ;save USE.PAGE parameter to ACC

; ;.INIT
	; LDA #TWB.ROSTER_WINDOW.TOP_LINE+1	;load line in x register	
	; STA DRAW.START_LINE
	
	; LDA #TWB.ROSTER_WINDOW.LEFT_SBYTE+1
	; STA DRAW.START_BYTE
	
	; LDA #TWB.ROSTER_WINDOW.RIGHT_SBYTE
	; STA DRAW.STOP_BYTE
		
	; LDA #TWB.ROSTER_WINDOW.BOTTOM_LINE-1
	; STA DRAW.STOP_LINE

; ;.DRAW
		; PLA ;restore USE.PAGE parameter to ACC
		; ORA #$80	;set high bit as a parameter to indicate we want the byte value for the draw to be $00
	; JSR DRAW.LINE


	; RTS

@END

	
@END


CHARACTER.ROSTER.INCREMENT_UP
@START
;PARAMETERS: INV_4.ACTIVE_PLAYER
;ENTRANCE: inventory module
;RETURN: updated video screen, incremented INV_4.ACTIVE_PLAYER


.ERASE.SELECTOR
		;INV_4.ACTIVE_PLAYER: contains current active player (before increment)
	JSR DRAW_ERASE.ACTIVE_PLAYER.SELECTOR


.INCREMENT.ACTIVE_PLAYER
@START
	LDA INV.ACTIVE_PLAYER ;load active player (sequential #)
	CMP #$01 ;is active player # the minimum #?
	BEQ .RESET.ACTIVE_PLAYER ;is yes, branch
	DEC INV.ACTIVE_PLAYER ;increment active player (sequential #)
	JMP .INCREMENT.ACTIVE_PLAYER.DONE
	
.RESET.ACTIVE_PLAYER
	LDA PARTY.TOTAL.PC ;reset active player # to the max player #
	STA INV.ACTIVE_PLAYER ;load active player (sequential #)

.INCREMENT.ACTIVE_PLAYER.DONE
@END


.DRAW.SELECTOR
		;INV_4.ACTIVE_PLAYER: contains incremented active player
		LDA #$7F
		STA COUT_CHAR_TYPE ;($00 = normal, $7F = inverse)			
	JSR DRAW_ERASE.ACTIVE_PLAYER.SELECTOR
		;**OPT** Memory. Combine with the same call in CHARACTER.ROSTER.INCREMENT_DOWN, as a JSR routine. It will save the bytes to set the parms less one byte for the RTS. (9 byte total saved)

.EXIT
	
	RTS


	RTS
@END

CHARACTER.ROSTER.INCREMENT_DOWN
@START
;PARAMETERS: INV_4.ACTIVE_PLAYER
;ENTRANCE: inventory module
;RETURN: updated video screen, incremented INV_4.ACTIVE_PLAYER


.ERASE.SELECTOR
		;INV_4.ACTIVE_PLAYER: contains current active player (before increment)
	JSR DRAW_ERASE.ACTIVE_PLAYER.SELECTOR


			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; JSR KEYIN
; .TEMP
			; LDA TEMP	


			
.INCREMENT.ACTIVE_PLAYER
@START
	LDA INV.ACTIVE_PLAYER ;load active player (sequential #)
	CMP PARTY.TOTAL.PC ;is active player # the maximum #?
	BEQ .RESET.ACTIVE_PLAYER ;is yes, branch
	INC INV.ACTIVE_PLAYER ;increment active player (sequential #)
	JMP .INCREMENT.ACTIVE_PLAYER.DONE
	
.RESET.ACTIVE_PLAYER
	LDA #$01 ;reset active player # to player1
	STA INV.ACTIVE_PLAYER ;load active player (sequential #)

.INCREMENT.ACTIVE_PLAYER.DONE
@END


.DRAW.SELECTOR
		;INV_4.ACTIVE_PLAYER: contains incremented active player
		LDA #$7F
		STA COUT_CHAR_TYPE ;($00 = normal, $7F = inverse)			
	JSR DRAW_ERASE.ACTIVE_PLAYER.SELECTOR




	
	
			
			
.EXIT
	
	RTS
	
	

@END

DRAW_ERASE.ACTIVE_PLAYER.SELECTOR ;current INV_4.ACTIVE_PLAYER
@START
;PARAMETERS: INV_4.ACTIVE_PLAYER, [COUT_CHAR_TYPE ;($00 = normal, $7F = inverse)]
;ENTRANCE: CHARACTER.ROSTER.INCREMENT_DOWN or CHARACTER.ROSTER.INCREMENT_DOWN 
;RETURN: updated video screen

;READ CHARACTER SHEET DATA
		LDA INV.ACTIVE_PLAYER ;load active player (sequential #)
		PHA ;save active player (sequential #)
		;ACC = player sequential # (high-bit not set = read mode)
	JSR COMBAT.READ_WRITE.CHR_SHEET.PC
		;RETURN VALUE = CHR_SHEET.RECORD.READ	

				; lda #$aa
				; ldx #CHR_SHEET.RECORD.READ
				; ldy /CHR_SHEET.RECORD.READ
				; jmp full.brk
				; brk
				
		
;SET CURSOR POSITION
		
		;vtab = INV_4.ACTIVE_PLAYER * 2 - 1
		PLA ;restore active player (sequential #)
		ASL ;X2
		SEC
		SBC #$01
		STA VTAB
	
		LDA #CHR_ROSTER.NAME.START_SBYTE
		STA HTAB	
	JSR	UPDATE.CHAR.POS


;PRINT NAME	
		LDA #CHR_SHEET.PC.NAME.CHARACTER			
		STA STRING+$0
		
		LDA /CHR_SHEET.PC.NAME.CHARACTER
		STA STRING+$1	
		; LDA #$7F
		; STA COUT_CHAR_TYPE ;($00 = normal, $7F = inverse)			
	JSR PRINT.STR
	
	
	RTS
			
@END



@END


	;**FALLS THROUGH**
	
;DRAW.TILE.SINGLE
;***see misc.main_memory.only.asm

;DRAW.TILE
;***see misc.main_memory.only.asm

;ERASE.TILE
;***see misc.main_memory.only.asm

	
;======INCLUDE FILES======
@START

;NONE

@END				
				
;======DEFINE VARIABLES======
@START




;GMAP.TILE.DATA 	.HS	05.34.05.36.05.00.02.34.02.36.05.88.05.8A.05.88.09.00.05.36.02.34.05.38.05.34.04.00.05.36.05.36.05.00.02.34.02.36.05.88.05.8A.05.8A.09.88.05.36.02.34.05.38.05.34.04.02.05.34.05.38.05.00.02.34.02.36.05.88.05.8A.05.8A.09.8A.05.88.02.88.05.34.05.36.04.00.05.38.05.38.05.00.02.34.02.36.05.00.05.88.05.8A.09.8A.05.8A.02.8A.05.88.05.38.04.00.05.38.05.00.05.00.02.34.02.36.05.00.05.88.05.8A.09.8A.05.89.02.8A.05.88.05.36.04.02.05.34.05.00.05.00.02.34.02.36.05.00.05.88.05.8A.09.8A.05.89.02.8A.05.88.05.38.04.00.05.36.05.36.05.00.02.34.02.36.05.8D.05.88.05.8A.09.8A.05.36.02.88.05.38.05.34.04.00.05.34.05.36.05.00.02.34.02.36.05.36.05.34.05.88.09.8A.05.36.02.88.05.38.05.34.04.00.05.00.05.36.05.00.02.34.02.36.05.36.05.00.05.88.09.8A.05.8A.02.88.05.38.05.38.04.01.05.34.05.36.05.00.02.34.02.34.05.34.05.88.05.88.09.8A.05.88.02.34.05.38.05.34.04.00.05.88.05.38.05.38.02.34.02.36.05.34.05.38.05.00.09.88.05.88.02.34.05.36.05.38.04.00.05.00.05.36.05.00.02.34.02.36.05.34.05.34.05.34.09.34.05.34.02.34.05.38.05.36.04.00.05.00.05.36.05.00.02.34.02.36.05.34.05.88.05.34.09.34.05.34.02.34.05.38.05.34.04.01.05.00.05.36.05.00.02.34.02.36.05.34.05.88.05.34.09.34.05.34.02.34.05.38.05.34.04.00.05.38.05.36.05.00.02.34.02.36.05.34.05.34.05.34.09.34.05.34.02.34.05.38.05.34.04.02.05.34.05.36.05.00.02.34.02.36.05.34.05.89.05.34.09.34.05.34.02.34.05.34.05.36.04.00.05.36.05.36.05.36.02.34.02.36.05.34.05.89.05.34.09.34.05.34.02.34.05.38.05.38.04.00.05.34.05.36.05.00.02.34.02.36.05.00.05.34.05.34.09.34.05.34.02.34.05.38.05.36.04.02.05.38.05.36.05.00.02.34.02.36.05.34.05.8A.05.34.09.34.05.34.02.34.05.36.05.38.04.00.05.34.05.36.05.00.02.34.02.36.05.34.05.8A.05.34.09.34.05.34.02.34.05.38.05.34.04.00.05.34.05.36.05.00.02.34.02.36.05.88.05.34.05.34.09.34.05.34.02.34.05.38.05.34.04.00.05.38.05.36.05.36.02.34.02.36.05.88.05.34.05.34.09.34.05.34.02.34.05.38.05.38.04.01.05.34.05.36.05.00.02.34.02.36.05.00.05.34.05.00.09.88.05.36.02.34.05.38.05.34.04.00.05.00.05.36.05.36.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.36.05.38.04.00.05.38.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.38.05.36.04.00.05.00.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.38.05.34.04.01.05.34.05.36.05.00.02.02.02.36.05.38.05.34.05.00.09.34.05.36.02.34.05.38.05.34.04.00.05.00.05.36.05.00.02.02.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.38.05.34.04.02.05.34.05.36.05.00.02.02.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.34.05.36.04.00.05.00.05.36.05.00.02.00.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.38.05.38.04.00.05.34.05.36.05.00.02.00.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.38.05.36.04.02.05.34.05.36.05.00.02.00.02.36.05.36.05.00.05.00.09.34.05.36.02.34.05.36.05.38.04.00.05.34.05.36.05.00.02.00.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.38.05.34.04.00.05.34.05.36.05.00.02.00.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.38.05.34.04.00.05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.38.05.38.04.01.05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.38.05.34.04.00.05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.36.05.38.04.00.05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.38.05.36.04.00.05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.38.05.34.04.01.05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.38.05.34.04.00.05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.38.05.34.04.02.05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.34.05.36.04.00.05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.38.05.02.04.00.05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.38.05.36.04.02.05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.36.05.38.04.00.05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.38.05.34.04.00.05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.38.05.34.04.00.05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.38.05.02.04.01.05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.38.05.34.04.00.05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.36.05.38.04.00.05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.38.05.36.04.00.05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.38.05.34.04.01.05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.38.05.34.04.00.05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.38.05.34.04.02.05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.34.05.36.04.00.05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.38.05.02.04.00.05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.38.05.36.04.02.05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.36.05.38.04.00.05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.38.05.34.04.00.05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.38.05.34.04.00.05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.38.05.02.04.01.05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.38.05.34.04.00.05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.36.05.38.04.00.05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.38.05.36.04.00
;array elements		00.01.02.03.04.05.06.07.08.09.0A.0B.0C.0D.0E.0F.10.11.12.13.14.15.16.17.18.19.1A.1B.1C.1D.1E.1F.20.21.22.23.24.25.26.27.28.29.2A.2B.2C.2D.2E.2F.30.31.32.33.34.35.36.37.38.39.3A.3B.3C.3D.3E.3F.40.41.42.43.44.45.46.47.48.49.4A.4B.4C.4D.4E.4F.50.51.52.53.54.55.56.57.58.59.5A.5B.5C.5D.5E.5F.60.61.62.63.64.65.66.67.68.69.6A.6B.6C.6D.6E.6F.70.71.72.73.74.75.76.77.78.79.7A.7B.7C.7D.7E.7F.80.81.82.83.84.85.86.87.88.89.8A.8B.8C.8D.8E.8F.90.91.92.93.94.95.96.97.98.99.9A.9B.9C.9D.9E.9F.A0.A1.A2.A3.A4.A5.A6.A7.A8.A9.AA.AB.AC.AD.AE.AF.B0.B1.B2.B3.B4.B5.B6.B7.B8.B9.BA.BB.BC.BD.BE.BF.C0.C1.C2.C3.C4.C5.C6.C7.C8.C9.CA.CB.CC.CD.CE.CF.D0.D1.D2.D3.D4.D5.D6.D7.D8.D9.DA.DB.DC.DD.DE.DF.E0.E1.E2.E3.E4.E5.E6.E7.E8.E9.EA.EB.EC.ED.EE.EF.F0.F1.F2.F3.F4.F5.F6.F7.F8.F9.FA.FB.FC.FD.FE.FF.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01
;	$AB is a tile group directly below player icon	
;Shape Table Variables


;TEST.ARRAY 		.HS			00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00
				
				
; RAW DATA FOR 64X64 TILE MAP (1 MAP ROW PER LINE X 64 LINES)

;	   !10					  !24	 !29   !34		   !48						!64
;	   $0A					  $18	 $1D   $22		   $30						$40	
; q		q	  q     Q     q     q	  q		Q	  q		q	  q		q	  q		q


;05.34.05.36.05.00.02.34.02.36.05.88.05.8A.05.88.09.00.05.36.02.34.05.02.05.34.04.00
;05.36.05.36.05.00.02.34.02.36.05.88.05.8A.05.8A.09.88.05.36.02.34.05.02.05.34.04.02
;05.34.05.02.05.00.02.34.02.36.05.88.05.8A.05.8A.09.8A.05.88.02.88.05.34.05.36.04.00
;05.02.05.02.05.00.02.34.02.36.05.00.05.88.05.8A.09.8A.05.8A.02.8A.05.88.05.02.04.00

;05.02.05.00.05.00.02.34.02.36.05.00.05.88.05.8A.09.8A.05.89.02.8A.05.88.05.36.04.02
;05.34.05.00.05.00.02.34.02.36.05.00.05.88.05.8A.09.8A.05.89.02.8A.05.88.05.02.04.00
;05.36.05.36.05.00.02.34.02.36.05.8D.05.88.05.8A.09.8A.05.36.02.88.05.02.05.34.04.00
;05.34.05.36.05.00.02.34.02.36.05.36.05.34.05.88.09.8A.05.36.02.88.05.02.05.34.04.00

;05.00.05.36.05.00.02.34.02.36.05.36.05.00.05.88.09.8A.05.8A.02.88.05.02.05.02.04.01
;05.34.05.36.05.00.02.34.02.34.05.8D.05.88.05.88.09.8A.05.88.02.34.05.02.05.34.04.00
;05.88.05.34.05.02.02.34.02.36.05.34.05.02.05.00.09.88.05.88.02.34.05.36.05.02.04.00
;END OF SCREEN 1

  
;05.00.05.36.05.00.02.34.02.36.05.34.05.34.05.34.09.34.05.34.02.34.05.02.05.36.04.00
;05.00.05.36.05.00.02.34.02.36.05.34.05.88.05.34.09.34.05.34.02.34.05.02.05.34.04.01
;05.00.05.36.05.00.02.34.02.36.05.34.05.88.05.34.09.34.05.34.02.34.05.02.05.34.04.00
;05.02.05.36.05.00.02.34.02.36.05.34.05.34.05.34.09.34.05.34.02.34.05.02.05.34.04.02

;05.34.05.36.05.00.02.34.02.36.05.34.05.89.05.34.09.34.05.34.02.34.05.34.05.36.04.00
;05.36.05.36.05.36.02.34.02.36.05.34.05.89.05.34.09.34.05.34.02.34.05.02.05.02.04.00
;05.34.05.36.05.00.02.34.02.36.05.00.05.34.05.34.09.34.05.34.02.34.05.02.05.36.04.02
;05.02.05.36.05.00.02.34.02.36.05.34.05.8A.05.34.09.34.05.34.02.34.05.36.05.02.04.00

;05.34.05.36.05.00.02.34.02.36.05.34.05.8A.05.34.09.34.05.34.02.34.05.02.05.34.04.00
;05.34.05.36.05.00.02.34.02.36.05.88.05.34.05.34.09.34.05.34.02.34.05.02.05.34.04.00
;05.02.05.36.05.36.02.34.02.36.05.88.05.34.05.34.09.34.05.34.02.34.05.02.05.02.04.01
;END OF SCREEN 2


;05.34.05.36.05.00.02.34.02.36.05.00.05.34.05.00.09.88.05.36.02.34.05.02.05.34.04.00
;05.00.05.36.05.36.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.36.05.02.04.00
;05.02.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.02.05.36.04.00
;05.00.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.02.05.34.04.01
;05.34.05.36.05.00.02.02.02.36.05.02.05.34.05.00.09.34.05.36.02.34.05.02.05.34.04.00
;05.00.05.36.05.00.02.02.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.02.05.34.04.02
;05.34.05.36.05.00.02.02.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.34.05.36.04.00
;05.00.05.36.05.00.02.00.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.02.05.02.04.00
;05.34.05.36.05.00.02.00.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.02.05.36.04.02
;05.34.05.36.05.00.02.00.02.36.05.36.05.00.05.00.09.34.05.36.02.34.05.36.05.02.04.00
;05.34.05.36.05.00.02.00.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.02.05.34.04.00
;05.34.05.36.05.00.02.00.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.02.05.34.04.00
;05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.02.05.02.04.01
;05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.02.05.34.04.00
;05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.36.05.02.04.00
;05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.02.05.36.04.00
;05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.02.05.34.04.01
;05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.02.05.34.04.00
;05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.02.05.34.04.02
;05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.34.05.36.04.00
;05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.02.05.02.04.00
;05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.02.05.36.04.02
;05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.36.05.02.04.00
;05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.02.05.34.04.00
;05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.02.05.34.04.00
;05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.02.05.02.04.01
;05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.02.05.34.04.00
;05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.36.05.02.04.00
;05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.02.05.36.04.00
;05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.02.05.34.04.01
;05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.02.05.34.04.00
;05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.02.05.34.04.02
;05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.34.05.36.04.00
;05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.02.05.02.04.00
;05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.02.05.36.04.02
;05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.36.05.02.04.00
;05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.02.05.34.04.00
;05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.02.05.34.04.00
;05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.02.05.02.04.01
;05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.02.05.34.04.00
;05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.36.05.02.04.00
;05.34.05.36.05.00.02.34.02.36.05.34.05.34.05.00.09.34.05.36.02.34.05.02.05.36.04.00

@END