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

		


.INIT.MISC
	LDA #$01					;$01 IS THE CODE FOR DRAW.SCREEN
	STA CALLED_BY.DRAW.SCREEN
								;**OPT** Speed. Memory. I think the above is a duplicate. there is also one below in this routine before call to MO.DRAW and DARKNESS.REVIEW
.INIT.MAP

	JSR TILE.LOOKUP.SCREEN		;Fills SCREEN.TILE.DATA with uncompressed tile data from RZONE.ARRAY. This is so that the drawing routines know which Tile_IDs are associated with each tile location on the view screen.
				
	LDA SMAP
	STA SMAP.CURRENT
	LDA SMAP+$1
	STA SMAP.CURRENT+$1

.INIT.SCREEN					;SETUP BYTE, LINE, DEPTH
	LDY #$00					;Y-REG IS THE INDEX FOR SCREEN.TILE.DATA, AND REFERS AND REFERS TO THE TILE #

	LDA	#SCREEN.DRAW.START_BYTE											;**OPT** THIS IS DUPLICATE
	STA SCREEN.DRAW.CURRENT_BYTE				;SET STARTING SCREEN BYTE			;**OPT** THIS IS DUPLICATE
	LDA #SCREEN.DRAW.START_LINE										
	STA TILE.LINE				;SET CURRENT LINE TO PLOT WITHIN TILE	
	STA TILE.LINE.START			;SET STARTING LINE FOR TILES IN ROW 0
	
	LDA #$00
	STA ANIMATION.FRAME_STATE	;START ANIMATION AT FRAME 0
	
;**OPT** when this is used for drawing the screen at game start or first entering a map, this is 
;probably fine. we shouldn't need to clear the entire screen for movement, but that will not flow
;through draw.screen so it will probably get setup correctly to begin with. This is a reminder
;note to thing about if screen clears are being handled most efficiently in general.
	
.CLEAR.SCREEN					;CLEAR BACKGROUND PAGE (WHERE DRAW.TILE WILL PLOT TO)
					
		

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
	BCS .DRAW.COMPLETE				;IF YES, EXIT (BCS: is ACC >= CMP value)

	JMP .LOOP

.DRAW.COMPLETE

	LDA #$01					;THIS VALUE IS USED TO THAT CERTAIN ROUTINES (LIKE DRAW.MISC IN DARKNESS.REVIEW KNOW THAT THEY WERE ENTERED VIA DRAW.SCREEN AND NOT A MOVE ROUTINE)
	STA CALLED_BY.DRAW.SCREEN
				
	JSR DARKNESS.REVIEW			;UPDATE THE HIDDEN (DARKNESS) TILES ON THE SCREEN BASED ON THE TILE_TYPE VALUES IN SCREEN.TILE.DATA 



	JSR MO.DRAW					;DRAW MAP OBJECTS
	
			
;DRAW HRCG TEXT
@START
	;JMP .SKIP

	JSR HCG.ON
	
;PRINT "GOLD"	
	LDA #$24
	STA HTAB	
	LDA #$1
	STA VTAB
	JSR	UPDATE.CHAR.POS
		
	JMP .TEXT1.PRINT

.TEXT1 .AZ -/Gold/			;ASCII text string
.TEXT1.PRINT
		LDA #.TEXT1 					
		STA STRING
		
		LDA /.TEXT1
		STA STRING+$1						
	JSR PRINT.STR



		
;PRINT "9999"	
	LDA #$24
	STA HTAB	
	LDA #$2
	STA VTAB
	JSR	UPDATE.CHAR.POS
	
		LDA #$99
		STA BCD
		LDA #$99
		STA BCD+$1
	JSR PRINT.BCD_PACKED
	

			
	JSR TIME.DISPLAY
	
.EXIT.HRCG	

				
.SKIP
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

	LDA #SHAPE.HOPPER1				;SAVE SHAPE.HOPPER1 AS THE DESTINATION ADDRESS FOR AUX MOVE
	STA AUX_MOVE.DEST
	STA SHAPE						;CONNECTS SHAPE.HOPPER1 TO SHAPE, USED BY DRAW.TILE
	LDA /SHAPE.HOPPER1
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

	LDA #SHAPE.HOPPER1				;SAVE SHAPE.HOPPER1 AS THE DESTINATION ADDRESS FOR AUX MOVE
	STA AUX_MOVE.DEST
	STA SHAPE						;CONNECTS SHAPE.HOPPER1 TO SHAPE, USED BY DRAW.TILE
	LDA /SHAPE.HOPPER1
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
	LDA #SHAPE.HOPPER1				;SAVE SHAPE.HOPPER1 AS THE DESTINATION ADDRESS FOR AUX MOVE
	STA AUX_MOVE.DEST
;	STA SHAPE						;CONNECTS SHAPE.HOPPER1 TO SHAPE, USED BY DRAW.TILE
	LDA /SHAPE.HOPPER1
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
	LDA SHAPE.HOPPER1,X				;COPY FROM
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
		
.STANDARD_ICON
	; ; LDA PLAYER.WALKING.TILE						;SPIRL TWIRLER
	; ; STA SAVED_TILE_TYPE	
	
	; LDA #PLAYER.ICON.BUFFER							;DRAW.TILE USES THE POINTER SHAPE TO LOCATE THE SHAPE TABLE.
	; STA SHAPE										;THIS CODE ATTACHES THE POINTER TO THE BUFFER USED TO STORE THE STANDARD PLAYER ICON. 
	; LDA /PLAYER.ICON.BUFFER
	; STA SHAPE+$1
	
		; LDA #$01										;LET'S DRAW.TILE.SINGLE KNOW WHERE IT'S BEING CALLED FROM SO IT COPIES THE SHAPE TABLE FROM PLAYER.ICON.BUFFER
		; STA CALLED_BY.DRAW.TILE.PLAYER.STANDARD_ICON
	; LDY #SCREEN.ARRAY.PLAYER_LOCATION
	; JSR DRAW.TILE.SINGLE	
		; LDA #$00
		; STA CALLED_BY.DRAW.TILE.PLAYER.STANDARD_ICON
	; RTS
	
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
	
;IS MAP TILE TALL GRASS? 
;(ONLY DRAW UPPER HALF OF PLAYER, SO LOWER HALF IS HIDDEN BY GRASS)	
	LDA SCREEN.TILE.DATA,Y
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
	JSR DRAW.TILE
	
.EXIT
	RTS



.DRAW.TILE.SINGLE
	LDY #SCREEN.ARRAY.PLAYER_LOCATION		;REQUIRED PARAMETER FOR DRAW.TILE.SINGLE	

	;FALLS THROUGH
@END	

DRAW.TILE.SINGLE ;	;=============DRAW A SINGLE TILE AT THE SPECIFIED SCREEN LOCATION=====
@START
;PARAMETERS: Y-REG (SCREEN POSITION AS MAPPED TO ELEMENT OF SCREEN.TILE.DATA), SAVED_TILE_TYPE (use if you want to draw a tile that isn't stored in SCREEN.TILE.DATA for the specified Y-REG location)
;RETURN: NONE
;ENTRANCE: DIRECT






;=====================SUBROUTINE DOCUMENTATION====================================
;This subroutine is designed for direct entrance. It setups up all the graphics screen variables required to draw a single tile and
;it copies the shape table for the Tile_ID (associated with the screen tile location passed via Y-REG) from auxiliary memory to a main memory buffer for use by DRAW.TILE
;
;there is a branch test in .START for an alternate entrance (.ALTERNATE_ENTRANCE) which is used if this routine was called for the purpose
;of drawing the player walking icon (/aka standard icon). In this event special treatment is needed because the shape table is copied from a special player icon buffer instead of from auxiliary memory.  
;=================================================================================



;VERIFY TILE IS ON THE VIEW SCREEN
	STY TEMP
	LDA #SCREEN.ARRAY.LAST_ELEMENT
	CMP TEMP
	BCS .START
	JMP ERROR

.START	
	LDA CALLED_BY.DRAW.TILE.PLAYER.STANDARD_ICON
	CMP #$01
	BNE .STANDARD.ENTRANCE
	JMP .ALTERNATE_ENTRANCE								;IF THIS ROUTINE WAS CALLED BY .STANDARD_ICON IN DRAW.TILE.PLAYER, THE SHAPE TABLE FOR PLAYER ICON IS COPIED FROM A BUFFER

.STANDARD.ENTRANCE	
	LDA SAVED_TILE_TYPE				;IF A TILE_TYPE IS PRESENT IN THIS VARIABLE, USE IT.
	CMP #$00
	BNE .REENTRY					;IF NOT, USE SCREEN.TILE.DATA AS THE SOURCE FOR TILE_TYPE


;LOAD TILE_TYPE OF THE NEXT TILE IN CURRENT ROW
	LDA SCREEN.TILE.DATA,Y			
	
.REENTRY
	STY SAVED.YREG.LOCAL					;SAVING YREG TO A VARIABLE ENABLES US TO AVOID USING X-REG IN THIS ROUTINE, WHICH ENABLES THIS ROUTINE TO FALL THROUGH TO DRAW.TILE INSTEAD SAVING X-REG TO THE STACK AND USING JSR TO DRAW.TILE 

;CALCULATE SHAPE TABLE ADDRESS	
	TAY								
	LDA TILE.SHAPES.LO,Y
	STA AUX_MOVE.START				;SAVE BASE ADDRESS AS START ADDRESS FOR AUX MEMORY MOVE
	CLC
	ADC #SHAPE.SIZE
	STA AUX_MOVE.END
	
	LDA TILE.SHAPES.HO,Y
	STA AUX_MOVE.START+$1
	STA AUX_MOVE.END+$1

	CPY #ANIMATION.TILE_RANGE.START	;IS CURRENT TILE AN ANIMATION TILE?
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

	LDA #SHAPE.HOPPER1				;SAVE SHAPE.HOPPER1 AS THE DESTINATION ADDRESS FOR AUX MOVE
	STA AUX_MOVE.DEST
	STA SHAPE						;CONNECTS SHAPE.HOPPER1 TO SHAPE, USED BY DRAW.TILE
	LDA /SHAPE.HOPPER1
	STA AUX_MOVE.DEST+$1
	STA SHAPE+$1			

	CLC								;EXECUTE AUX MEMORY MOVE
	JSR AUX_MOVE

	LDA #$00						
	STA SAVED_TILE_TYPE				;RESET TO $00 SO NEXT CALL TO THIS ROUTINE WILL USE DEFAULT TILE SOURCE (SCREEN.TILE.DATA) UNLESS SAVED.TILE_TYPE IS EXPRESSLY SET AGAIN. 
	LDY SAVED.YREG.LOCAL		

.ALTERNATE_ENTRANCE		
	LDA SCREEN.INDEX.TILE_SBYTE,Y
	STA	SCREEN.DRAW.CURRENT_BYTE
	
	LDA SCREEN.INDEX.TILE_LINE,Y
	STA TILE.LINE.START
	STA TILE.LINE


	;****FALLS THROUGH*****
@END
	
DRAW.TILE ;			;=============DRAW A SIGNLE TILE AT THE LOCATION SPECIFIED======
@START
;PARAMETERS:  TILE.LINE, TILE.LINE.START, TILE.DEPTH, SCREEN.DRAW.CURRENT_BYTE, [PAGE.FOREGROUND.OVERRIDE]
;RETURN: NONE
;ENTRANCE: DRAW.COLUMN, DRAW.ROW, DRAW.TILE.SINGLE


;=====================SUBROUTINE DOCUMENTATION====================================
;This subroutine is the lowest level routine associated with tile graphics plotting. 
;It is not designed for direct entrance. Management of all graphics screen variables
;is required by the calling routine. This doesn't setup any start values and it only increments
;screen byte and line in the context of a single tile (i.e. it assumes the calling routine will increment or reinitialize screen byte and line if another tile is to be plotted)
;
;This routie uses lookup tables to obtain the graphic screen memory address associated with
;each line of the tile to be drawn. This is done in a loop, in which both screen bytes of the tile are drawn
;on each iteration. Tiles are drawn top to bottom.  
;=================================================================================



;**OPT** Speed. Include an in-line code version of DRAW.TILE with DRAW.COLUMN and DRAW.ROW to avoid the costly JSR/RTS to call as a subroutine. 

.SAVE.REGISTERS
	TXA
	PHA
	TYA
	PHA

.START

	LDX TILE.LINE			;LOAD LINE IN X REGISTER	
	LDA #$00
	STA SHP.TBL.CNTR		;START DRAWING AT BEGINNING OF SHAPE TABLE


	LDA TILE.LINE.START		;THE STARTING LINE OF THE TILES IN THE CURRENT ROW
	CLC						;*OPT* I I THINK ALL CLS CAN BE REMOVED WHEN I'M NOT EXPECTING AN OVERFLOW
	ADC TILE.DEPTH			;ADD DEPTH OF SHAPE TO STARTING LINE IN HEX (# of lines, not the position of last line....so line positions $0-$F is $10 (!16) total lines)
	STA TILE.LINE.STOP		;SET THE LAST LINE TO DRAW BASED ON THE DEPTH OF THE TILE

.DRAW.LOOP
	
;==========INLINE CODE GET.LINE.ADDRESS1==================	
	LDA PAGE.FOREGROUND.OVERRIDE	;CHECK TO SEE IF CALLING ROUTINE WANTS ERASE DONE TO FOREGROUND INSTEAD OF BACKGROUND (DEFAULT)
	CMP #$01
	BEQ .FOREGROUND_OVERRIDE
	
	LDA PAGE.BACKGROUND		
	CMP #$02
	BEQ .LOOKUP.PAGE2
	JMP .LOOKUP.PAGE1
	
.FOREGROUND_OVERRIDE
	LDA PAGE.FOREGROUND		;SPECIFY PAGE TO GET ADDRESS FOR (SHOULD BE BACKGROUND PAGE)
	CMP #$02
	BEQ .LOOKUP.PAGE2
	

.LOOKUP.PAGE1	
	LDA LINE.HO.P1,X		;GET LINE ADDRESS
	STA LINE.BASE.ADDR1+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR1
	JMP .GETLINE_COMPLETE

.LOOKUP.PAGE2
	LDA LINE.HO.P2,X		;GET LINE ADDRESS
	STA LINE.BASE.ADDR1+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR1
.GETLINE_COMPLETE
;========================================================
	LDY SHP.TBL.CNTR		;LOAD Y WITH SHAPE TABLE COUNTER
	LDA (SHAPE),Y			;LOAD SHAPE BYTE (1st screen byte of the tile)

	LDY SCREEN.DRAW.CURRENT_BYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	STA (LINE.BASE.ADDR1),Y	;PLOT (1st screen byte)
	
	INC SHP.TBL.CNTR		;NEXT SHAPE BYTE, NEXT TILE LINE
	INC SCREEN.DRAW.CURRENT_BYTE			;SWITCH TO 2ND SCREEN BYTE IN THE TILE
	
	LDY SHP.TBL.CNTR		;LOAD Y WITH SHAPE TABLE COUNTER
	LDA (SHAPE),Y			;LOAD SHAPE BYTE (2nd screen byte of the tile)
	LDY SCREEN.DRAW.CURRENT_BYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	STA (LINE.BASE.ADDR1),Y				;PLOT (2st screen byte)
	
	DEC SCREEN.DRAW.CURRENT_BYTE			;SWITCH BACK TO 1ST SCREEN BYTE IN THE TILE	;**OPT** if the counters are updated after the CMP/branch it might save 1 INC SCREEN.DRAW.CURRENT_BYTE instruction in the main loop because SCREEN.DRAW.CURRENT_BYTE will be in 2nd position when this loop ends. 
	INX						;NEXT TILE LINE
	INC SHP.TBL.CNTR		;NEXT SHAPE BYTE
	
	CPX TILE.LINE.STOP		;IS TILE DONE?							
	BCC .DRAW.LOOP			;IF NO, DRAW NEXT LINE (BCC: is X-REG < CPX value)	

	LDA #$00
	STA PAGE.FOREGROUND.OVERRIDE
			
			
			
.RESTORE.REGISTERS
	PLA
	TAY
	PLA
	TAX
	
	RTS

ERROR
;ERROR REPORTED BY DRAW.TILE.SINGLE Y-REG VALUE > LAST ELEMENT OF SCREEN.TILE.DATA
;ACC CONTAINS THE VALUE OF CALLED_BY

	LDA TEXT
	LDA CALLED_BY
	BRK	
@END	

ERASE.TILE
@START
;PARAMETERS: X-REG (SCREEN POSITION TO ERASE, AS MAPPED TO ELEMENT OF SCREEN.TILE.DATA). [PAGE.FOREGROUND.OVERRIDE]
;RETURN: NONE
;ENTRANCE: DIRECT

;NOTE: IN THE CALLING ROUTINE (DARKNESS.CALCULATE), AT THE TIME THIS ROUTINE IS CALLED, Y-REG CONTAINS THE SCREEN POSITION OF THE OBSCURING TILE AND X-REG CONTAINS THE POSITION OF THE CURRENT TILE WHICH HAS BEEN FLAGGED AS DARK

;=====================SUBROUTINE DOCUMENTATION====================================
;Unlike it's counter parter, DRAW.TILE, this subroutine is desgined for direct entrance. 
;It sets up the graphics screen variables it needs for a specific screen tile location via lookup tables using the tile location as the index.
;
;This routine was designed to be used by darkness_manager.ASM to make tiles dark which already have pixels turne on.
;This routine turns off pixels one byte at a time by loading #$00 in to the associated memory address.
;This routine is capable of erasing a tile on the foreground or background page. 
;=================================================================================




;		**OPT** Speed. This code could be place in-line with each of the 7 "TYPE" subroutines in darkness_functions. so far that is the only place it's used. this would elimiate a JSR/RTS
;		and placed in-line in MO.PREP for erasing map objects. 

;			JMP .EXIT


;			LDA GMAP
;			CMP #$54
;			BNE .TEMP
;			CPX #$88			; SHOUD HAVE BEEN SET DARK BY $68 VIA TYPE2
;			BNE .TEMP
			;JSR FLIP.PAGE
			;JSR KEYIN
;			LDA TEXT
;			LDA PAGE1
;			BRK
;.TEMP

.SAVE.REGISTERS
	TXA
	PHA
	TYA
	PHA
	
	;TROUBLESHOOTING HOOK #1
		;CPX #$9D
		;BNE .TEMP
		;LDX #$AA
		;LDY #$AA
		;BRK

	STX TEMP
	LDA #SCREEN.ARRAY.LAST_ELEMENT
	CMP TEMP
	BCS .START
	JMP .ERROR
	
.START		
	LDA SCREEN.INDEX.TILE_SBYTE,X
	TAY
	LDA SCREEN.INDEX.TILE_LINE,X
	TAX

	
	TXA
	CLC						;*OPT* I I THINK ALL CLS CAN BE REMOVED WHEN I'M NOT EXPECTING AN OVERFLOW
	ADC #TILE.DEPTH.STANDARD			;ADD DEPTH OF SHAPE TO STARTING LINE IN HEX (# of lines, not the position of last line....so line positions $0-$F is $10 (!16) total lines)
	STA TILE.LINE.STOP		;SET THE LAST LINE TO DRAW BASED ON THE DEPTH OF THE TILE
	
.DRAW.LOOP

.GET.LINE.ADDRESS
	LDA PAGE.FOREGROUND.OVERRIDE	;CHECK TO SEE IF CALLING ROUTINE WANTS ERASE DONE TO FOREGROUND INSTEAD OF BACKGROUND (DEFAULT)
	CMP #$01
	BEQ .FOREGROUND_OVERRIDE
	
	LDA PAGE.BACKGROUND		;SPECIFY PAGE TO GET ADDRESS FOR (SHOULD BE BACKGROUND PAGE)
	CMP #$02
	BEQ .ADDR.LOOKUP.PAGE2
	JMP .ADDR.LOOKUP.PAGE1
	
.FOREGROUND_OVERRIDE
	LDA PAGE.FOREGROUND		;SPECIFY PAGE TO GET ADDRESS FOR (SHOULD BE BACKGROUND PAGE)
	CMP #$02
	BEQ .ADDR.LOOKUP.PAGE2

	
.ADDR.LOOKUP.PAGE1	
	LDA LINE.HO.P1,X			;GET LINE ADDRESS
	STA LINE.BASE.ADDR1+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR1
	JMP .LOOKUP_COMPLETE

.ADDR.LOOKUP.PAGE2
	LDA LINE.HO.P2,X			;GET LINE ADDRESS
	STA LINE.BASE.ADDR1+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR1	
.LOOKUP_COMPLETE

	LDA #$00
	STA (LINE.BASE.ADDR1),Y		;ERASE (1st screen byte)		
	INY
	STA (LINE.BASE.ADDR1),Y		;ERASE (2st screen byte)
	
	DEY							;SWITCH BACK TO 1ST SCREEN BYTE IN THE TILE	
	INX							;NEXT TILE LINE		

	CPX TILE.LINE.STOP		;IS TILE DONE?							
	BCC .DRAW.LOOP			;IF NO, DRAW NEXT LINE (BCC: is ACC < CMP value)	

	LDA #$00	
	STA PAGE.FOREGROUND.OVERRIDE	;TURN OFF OVERRIDE	
	
.RESTORE.REGISTERS
	PLA
	TAY
	PLA
	TAX

.EXIT					
	RTS	

.ERROR
;ERROR REPORTED BY ERASE.TILE. X-REG VALUE > LAST ELEMENT OF SCREEN.TILE.DATA, WHICH
;IN EFFECT REFERED TO A TILE NOT ON THE SCREEN VIEW. 
	BRK
@END
	
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