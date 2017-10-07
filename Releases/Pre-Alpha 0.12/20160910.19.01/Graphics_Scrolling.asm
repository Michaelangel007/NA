;************************INCLUDE FILE*****************************
;(do not assemble stand-alone. For a stand-alone version of this routine see examples/rwts2)

				
;=====================GRAPHICS_SCROLLING.ASM DOCUMENTATION====================================
;The graphics scrolling subroutines are conceptually part of the graphics engine, but
;were split off into a separate file for organization purposes.
;
;With the exception of COPY.SCREEN, the purposes of these subtroutines is
;to which is to scroll the graphics screen as part of the process of setting up a new
;graphics screen to reflect a player move. 
;
;COPY.SCREEN is used to prepare the screen for the animatin routines. 
;
;The following summarizes the entrances:
;		COPY.SCREEN: direct entrance
;		SCROLL.COLUMNS: direct entrance, it calls SCROLL.COLUMNS.DATA
;		SCROLL.ROWS: direct entrance, it calls SCROLL.ROWS.DATA
;=================================================================================




;NOTES ON **OPT** THIS TOPIC: IF THE .SCROLL ROUTINES LOADING THE BASE ADDRESSES BOTH LDX TILE.LINE AND TILE.LINE.CPY, THEN FURTHER UPSTREAM EITHER DIFFERENT OR THE SAME VALUES COULD BE PLACED DEPENDING ON WHETHER IT'S ROW OR COLUMN COPY.
;I INITIALLY GOT CONCERNED ABOUT DOING WHEN ROUTINE WHEN I SAW THAT THE ROW SCROLLING HAD INC SCREEN.DRAW.CURRENT_BYTE TWICE, THINKIN IT WAS MOVING OVER TWO TILES. BUT THAT ISN'T THE CASE, 2 SCREEN BYTES = 1 TILE. THE TILES ARE BEING COPIED TO THE BACKGROUND PAGE SO THAT IS WHY TILES DON'T GET OVERWRITTEN AS THE COPY IS BEING DONE. 


COPY.SCREEN				;=============SCROLLS ONSCREEN ROW TILES=========
@START
;PARAMTERS: PLAYER.MOVE.CURRENT
;RETURN: NONE

;=====================SUBROUTINE DOCUMENTATION====================================
;This subtroutines copies each byte on the foreground page to the same location on the backgroud page. 
;This is useful for routines such as animation which needs the foreground and background in sync before it does it's draws to the background.
;=================================================================================


;**OPT** Speed. Instead of iterating through each line in a tile, then moving to the next tile, this routine would be faster if it copied each screen byte in an entire line and then incremented to the next line. This would reduce the line lookups per tile to 16 instead 272. (16lines * 17 tiles)

			
.INIT.SCREEN					;SETUP BYTE, LINE, DEPTH

	LDA #SCREEN.DRAW.START_LINE										
	STA TILE.LINE.START			;SET STARTING LINE FOR TILES IN ROW 0

.OUTERLOOP
	LDX TILE.LINE.START		;PREPARE FOR NEW TILE: RESET CURRENT LINE IN TILE TO DRAW TO THE STARTING LINE
	
	LDA TILE.LINE.START
	CLC
	ADC #TILE.DEPTH.STANDARD			;ADD DEPTH OF SHAPE TO STARTING LINE IN HEX (# of lines, not the position of last line....so line positions $0-$F is $10 (!16) total lines)
	STA TILE.LINE.STOP		;SET THE LAST LINE TO DRAW BASED ON THE DEPTH OF THE TILE
				
.LINELOOP	
	LDY #SCREEN.DRAW.START_BYTE		;RESET SCREEN BYTE INDEX TO THE FIRST TILE COLUMN	
	
.GET.LINE.ADDRESS1A	
	LDA PAGE.FOREGROUND		;SPECIFY PAGE TO GET ADDRESS FOR	
	CMP #$02
	BEQ .ADDR1A.LOOKUP.PAGE2
	
.ADDR1A.LOOKUP.PAGE1	
	LDA LINE.HO.P1,X		;GET LINE ADDRESS
	STA LINE.BASE.ADDR1+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR1
	JMP .LOOKUP1A.COMPLETE

.ADDR1A.LOOKUP.PAGE2
	LDA LINE.HO.P2,X		;GET LINE ADDRESS
	STA LINE.BASE.ADDR1+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR1

.LOOKUP1A.COMPLETE
	
.GET.LINE.ADDRESS2A
	LDA PAGE.BACKGROUND		;SPECIFY PAGE TO GET ADDRESS FOR
	CMP #$02
	BEQ .ADDR2A.LOOKUP.PAGE2
	
.ADDR2A.LOOKUP.PAGE1	
	LDA LINE.HO.P1,X		;GET LINE ADDRESS
	STA LINE.BASE.ADDR2+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR2
	JMP .LOOKUP2A.COMPLETE

.ADDR2A.LOOKUP.PAGE2
	LDA LINE.HO.P2,X		;GET LINE ADDRESS
	STA LINE.BASE.ADDR2+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR2

.LOOKUP2A.COMPLETE
	
	
.COPYLOOP

	LDA (LINE.BASE.ADDR1),Y			;LOAD TILE DATA	
	STA (LINE.BASE.ADDR2),Y			;COPY TILE DATA

;IS LINE COMPLETE?
	CPY #SCREEN.DRAW.STOP_BYTE		;IS Y-REG = #SCREEN.DRAW.STOP_BYTE?
	BEQ .COPYLOOP_STEP				;IF YES, CONTINUE LOOP
	CPY #SCREEN.DRAW.STOP_BYTE		;IS Y-REG >= #SCREEN.DRAW.STOP_BYTE?
	BCS .LINE.COMPLETE				;IF YES, SWITCH OVER TO NEXT ROW (BCS: is Y-REG >= CPY value)
.COPYLOOP_STEP	
	INY		
	JMP .COPYLOOP					

.LINE.COMPLETE

	INX							;NEXT TILE LINE
		
	CPX TILE.LINE.STOP			;IS TILE DONE?	
	BCC .LINELOOP				;IF NO, DRAW NEXT LINE (BCC: is ACC < CMP value)

	
.ROW.COMPLETE						;IF YES, SETUP NEXT ROW
	LDA TILE.LINE.START				;ADVANCE LINE TO NEXT ROW OF TILES
	CLC
	ADC #TILE.DEPTH.STANDARD
	STA TILE.LINE.START
	CMP	#SCREEN.DRAW.STOP_LINE		;ARE ALL ROWS SCROLLED?
	BCS .EXIT						;IF YES, EXIT (BCS: is ACC >= CMP value)

	JMP .OUTERLOOP

.EXIT
		
	RTS
	
@END
	
SCROLL.COLUMNS			;=============SCROLLS ONSCREEN ROW TILES=========
@START
;PARAMETERS: PLAYER.MOVE.CURRENT
;RETURN: NONE

;=====================SUBROUTINE DOCUMENTATION====================================
;This subroutine shifts all tiles on screen over 1 tile to the left or right, which is
;is used by the player movement routines MOVE.EAST/WEST
;
;The result is one column of tiles is overwritten. For example, if the player move is east
;The tiles are shifted 1 tile to the left (opposite direction of player move). This causes the tiles originally in 
;screen column 0 to be overwitten (i.e. disappear from the screen). The right most column on the screen will have 
;a duplicate set of tiles, the same as in the 2nd from the right edge. This prepares DRAW.COLUMN.SINGLE to
;obtain updated tile data (net of the player move) and draw it in the right most column
;
;To execute the scrolling, each screen byte is copied from the foreground page to the background page, and the memory address
;on the background page is adjusted to reflect the new location (+/- 2 screen bytes)
;=================================================================================

	JSR SCROLL.COLUMN.DATA			;SCROLLS THE TILE_TYPE DATA IN SCREEN.TILE.DATA
		
.INIT.SCREEN					;SETUP BYTE, LINE, DEPTH

	; LDA ANIMATION.DEEP_WATER.TALLY	
	; SEC
	; SBC #SCREEN.COLUMN.SIZE			;SUBTRACT A COLUMNS WORTH OF TILES FROM THE DEEP WATER TALLY IN PREPERATION FOR A NEW COLUMN TO EVENTUALLY BE DRAWN. 
	; STA ANIMATION.DEEP_WATER.TALLY
	
	LDA #SCREEN.DRAW.START_LINE										
	STA TILE.LINE.START			;SET STARTING LINE FOR TILES IN ROW 0

	LDA PLAYER.MOVE.CURRENT		;#$00=north, $01=south, $02=east, $03=west
	CMP #$03
	BEQ	.MOVE.WEST.SCROLL.RIGHT
	CMP #$02
	BEQ	.MOVE.EAST.SCROLL.LEFT
	JMP .ERROR

.MOVE.WEST.SCROLL.RIGHT
@START
;NOTE: unlike data scrolling, multi-page graphics scolling can start the scroll at the left of the screen and iterate to the right, for west,
;		because the graphics data doesn't get overwritten as the copy progresses because it is copying from page1 to page2

	
.ROWLOOP.WEST

	LDX TILE.LINE.START		;PREPARE FOR NEW TILE: RESET CURRENT LINE IN TILE TO DRAW TO THE STARTING LINE
	
	LDA TILE.LINE.START
	CLC
	ADC #TILE.DEPTH.STANDARD			;ADD DEPTH OF SHAPE TO STARTING LINE IN HEX (# of lines, not the position of last line....so line positions $0-$F is $10 (!16) total lines)
	STA TILE.LINE.STOP		;SET THE LAST LINE TO DRAW BASED ON THE DEPTH OF THE TILE
				
.LINELOOP.WEST
	
	LDY #SCREEN.DRAW.START_BYTE		;RESET SCREEN BYTE INDEX TO THE FIRST TILE COLUMN	
	
.GET.LINE.ADDRESS1A	
	LDA PAGE.FOREGROUND		;SPECIFY PAGE TO GET ADDRESS FOR	
	CMP #$02
	BEQ .ADDR1A.LOOKUP.PAGE2
	
.ADDR1A.LOOKUP.PAGE1	
	LDA LINE.HO.P1,X		;GET LINE ADDRESS
	STA LINE.BASE.ADDR1+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR1
	JMP .LOOKUP1A.COMPLETE

.ADDR1A.LOOKUP.PAGE2
	LDA LINE.HO.P2,X		;GET LINE ADDRESS
	STA LINE.BASE.ADDR1+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR1

.LOOKUP1A.COMPLETE
	
.GET.LINE.ADDRESS2A
	LDA PAGE.BACKGROUND		;SPECIFY PAGE TO GET ADDRESS FOR
	CMP #$02
	BEQ .ADDR2A.LOOKUP.PAGE2
	
.ADDR2A.LOOKUP.PAGE1	
	LDA LINE.HO.P1,X		;GET LINE ADDRESS
	STA LINE.BASE.ADDR2+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR2
	JMP .LOOKUP2A.COMPLETE

.ADDR2A.LOOKUP.PAGE2
	LDA LINE.HO.P2,X		;GET LINE ADDRESS
	STA LINE.BASE.ADDR2+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR2

.LOOKUP2A.COMPLETE
	
	
.COPYLOOP.WEST
;COPY TILE DATA	

;**OPT** Speed. Unroll this loop. put in the code to copy each screen byte. this would eliminate the CPY, BCS and JMP instructions on each interation
;				Best to wait until the screen size is final

;**OPT** Speed. replace all the absolute mode references with immediate mode. For example. LDX #$02, instead of LDX #SCREEN.DRAW.START_BYTE. 
;				Best to wait until the screen size is final

	LDA (LINE.BASE.ADDR1),Y	;LOAD TILE DATA	
	INY						;MOVE AHEAD 1 COLUMN
	INY
	STA (LINE.BASE.ADDR2),Y	;COPY TILE DATA
	DEY						;MOVE BACK 1 COLUMN
	
	CPY #SCROLL.WEST.STOP_BYTE	;IS LINE COMPLETE?

	BCS .LINE.COMPLETE.WEST			;IF YES, SWITCH OVER TO NEXT ROW (BCS: is Y-REG >= CPY value)
		JMP .COPYLOOP.WEST

.LINE.COMPLETE.WEST

	INX							;NEXT TILE LINE
		
	CPX TILE.LINE.STOP			;IS TILE DONE?	
	BCC .LINELOOP.WEST			;IF NO, DRAW NEXT LINE (BCC: is ACC < CMP value)
;		JMP .ROW.COMPLETE.WEST	
	
.ROW.COMPLETE.WEST				;IF YES, SETUP NEXT ROW
	LDA TILE.LINE.START				;ADVANCE LINE TO NEXT ROW OF TILES
	CLC
	ADC #TILE.DEPTH.STANDARD
	STA TILE.LINE.START
	CMP	#SCREEN.DRAW.STOP_LINE		;ARE ALL ROWS SCROLLED?
	BCS .EXIT						;IF YES, EXIT (BCS: is ACC >= CMP value)

	JMP .ROWLOOP.WEST
@END

.MOVE.EAST.SCROLL.LEFT
@START
;**OPT** SPEED. Totally unroll the scrolling loops. do hard coded copies between the page 1 and page 2 memory addresses. See examples/graphics/get_line_formula.xls
;				for a layout of the base line addresses for the entire screen and the increments between them.
;				My conclusion was that I'd need to do an index by X/Y LDA/STA for each base line address, to handle each screen byte.
;				This would save 2 cycles because indexed more is 1 cycle less than indirect by Y mode. 
;				The LDA/STA addresses would have to be hard coded for each direction of movement. I estimated ~$750 (almost a super page) of memory required per direction. 
	
	
.ROWLOOP.EAST

	LDX TILE.LINE.START		;PREPARE FOR NEW TILE: RESET CURRENT LINE IN TILE TO DRAW TO THE STARTING LINE
	
	LDA TILE.LINE.START
	CLC
	ADC #TILE.DEPTH.STANDARD			;ADD DEPTH OF SHAPE TO STARTING LINE IN HEX (# of lines, not the position of last line....so line positions $0-$F is $10 (!16) total lines)
	STA TILE.LINE.STOP		;SET THE LAST LINE TO DRAW BASED ON THE DEPTH OF THE TILE
				
.LINELOOP.EAST
	
	LDY #SCREEN.DRAW.START_BYTE		;RESET SCREEN BYTE INDEX TO THE FIRST TILE COLUMN	

;GET LINE ADDRESSES FOR COPY TO/FROM
;(one address is obtained for PAGE1 and PAGE2)	
.GET.LINE.ADDRESS1B	
	LDA PAGE.FOREGROUND		;SPECIFY PAGE TO GET ADDRESS FOR	
	CMP #$02
	BEQ .ADDR1B.LOOKUP.PAGE2
	
.ADDR1B.LOOKUP.PAGE1	
	LDA LINE.HO.P1,X		;GET LINE ADDRESS
	STA LINE.BASE.ADDR1+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR1
	JMP .LOOKUP1B.COMPLETE

.ADDR1B.LOOKUP.PAGE2
	LDA LINE.HO.P2,X		;GET LINE ADDRESS
	STA LINE.BASE.ADDR1+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR1

.LOOKUP1B.COMPLETE
	
.GET.LINE.ADDRESS2B
	LDA PAGE.BACKGROUND		;SPECIFY PAGE TO GET ADDRESS FOR
	CMP #$02
	BEQ .ADDR2B.LOOKUP.PAGE2
	
.ADDR2B.LOOKUP.PAGE1	
	LDA LINE.HO.P1,X		;GET LINE ADDRESS
	STA LINE.BASE.ADDR2+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR2
	JMP .LOOKUP2B.COMPLETE

.ADDR2B.LOOKUP.PAGE2
	LDA LINE.HO.P2,X		;GET LINE ADDRESS
	STA LINE.BASE.ADDR2+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR2

.LOOKUP2B.COMPLETE
	
	
.COPYLOOP.EAST
;COPY TILE DATA	

;**OPT** Speed. Unroll this loop. put in the code to copy each screen byte. this would eliminate the CPY, BCS and JMP instructions on each interation
;				Best to wait until the screen size is final

;**OPT** Speed. replace all the absolute mode references with immediate mode. For example. LDX #$02, instead of LDX #SCREEN.DRAW.START_BYTE. 
;				Best to wait until the screen size is final

	INY						;MOVE AHEAD 1 TILE COLUMN
	INY
	LDA (LINE.BASE.ADDR1),Y	;LOAD TILE DATA	
	
	DEY						;MOVE BACK 1 TILE COLUMN
	DEY
	STA (LINE.BASE.ADDR2),Y	;COPY TILE DATA

	INY						;ADVANCE TO NEXT SCREEN BYTE
	
	CPY #SCROLL.STOP_BYTE		;IS ROW COMPLETE?
	BCS .LINE.COMPLETE.EAST		;IF YES, SWITCH OVER TO NEXT ROW (BCS: is Y-REG >= CPY value)
		JMP .COPYLOOP.EAST

.LINE.COMPLETE.EAST

	INX							;NEXT TILE LINE
		
	CPX TILE.LINE.STOP			;IS TILE DONE?	
	BCC .LINELOOP.EAST			;IF NO, DRAW NEXT LINE (BCC: is ACC < CMP value)
;		JMP .ROW.COMPLETE.EAST	
	
.ROW.COMPLETE.EAST				;IF YES, SETUP NEXT ROW
	LDA TILE.LINE.START			;ADVANCE LINE TO NEXT ROW OF TILES
	CLC
	ADC #TILE.DEPTH.STANDARD
	STA TILE.LINE.START
	CMP	#SCREEN.DRAW.STOP_LINE		;ARE ALL ROWS SCROLLED?
	BCS .EXIT						;IF YES, EXIT (BCS: is ACC >= CMP value)

	JMP .ROWLOOP.EAST
@END

.EXIT

	RTS

.ERROR
	;SCOLL.COLUMNS REPORTS INVALID VALUE IN PLAYER.MOVE.CURRENT 
;DISABLE.BS_RAM

	LDA #$A7
	JMP FULL.BRK
	
@END


SCROLL.COLUMN.DATA		;=============SCROLLS DATA IN SCREEN.TILE.DATA ARRAY=====
@START
;PARAMETERS: GMAP
;RETURN: SCREEN.TILE.DATA (UPDATED)
;ENTRANCE: SCROLL.COLUMNS, DIRECT

;=====================SUBROUTINE DOCUMENTATION====================================
;This subroutine serves the same purpose as SCROLL.COLUMNS, which is to scroll the graphics screen as part of 
;the process of setting up a new graphics screen to reflect a player move. 
;
;The difference is that this subroutine is scrolling the data in the screen arrays instead of
;the bytes stored in the graphics screen memory. The screen arrays contain the data which the subtroutines in graphics_engine.asm
;use to draw the tiles on screen. Accordingly, if only the bytes in graphics memory were scrolled, the screen arrays
;would think a different tiles existed on the screen than the tiles visible on the screen. This would cause a problem
;because many subroutines use the screen arrays to determine what the tile_ID or map object present at a specific screen tile location.  
;
;=================================================================================

	LDA PLAYER.MOVE.CURRENT		;#$00=north, $01=south, $02=east, $03=west
	CMP #$02
	BEQ	.MOVE.EAST.SCROLL.LEFT
	CMP #$03
	BEQ	.MOVE.WEST.SCROLL.RIGHT
	JMP .ERROR
	
.MOVE.EAST.SCROLL.LEFT
@START
.INIT.INDEX.COUNTERS1

	LDY #$00	;COPY TO
	LDX #$01 	;COPY FROM
	
.SCROLL.LOOP1
	LDA SCREEN.TILE.DATA, X					;;SCROLL TILE_TYPE DATA 
	STA SCREEN.TILE.DATA, Y
	
	LDA SCREEN.DARK.DATA,X				;SCROLL DARKESS DATA. 3 THINGS ARE HAPPENNING. SCROLL THE DARKESSS ON SCREEN LAST MOVE (SCREEN.DATA.DARK) AND SAVE THE SCROLLED VALUES INTO SCREEN.DARK.DATA_BEFORE. THEN INIT THE SCREEN.DARK.DATA ARRAY TO $00s SO IT'S READY FOR USE BY DARKNESS.REVIEW IN THE CURRENT MOVE BEING PROCESSED
	STA SCREEN.DARK.DATA_BEFORE,Y
	
	LDA SCREEN.MO_GENERAL.DATA, X		;SCROLL MAP OBJECT DATA 
	STA SCREEN.MO_GENERAL.DATA, Y		

	LDA SCREEN.MO_SPRITE.DATA, X			;SCROLL MAP OBJECT DATA
	STA SCREEN.MO_SPRITE.DATA, Y	

	LDA SCREEN.MO_SPRITE_TYPE.DATA, X			;SCROLL MAP OBJECT DATA
	STA SCREEN.MO_SPRITE_TYPE.DATA, Y
	
	LDA #$00
	STA SCREEN.DARK.DATA,X
	INX
	INY
	CPY #SCREEN.ARRAY.LAST_ELEMENT
	BCC .SCROLL.LOOP1
	
.CLEANUP1		
;INITS TO $00 THE FIRST COLUMN IN SCREEN.DARK.DATA, AND LAST COLUMN FOR SCREEN.DARK.DATA_BEFORE

;**OPT** Speed. Setup lookup table for the screen.tile.data values in the first and last column.
;That way INX/INY could be used in this routine instead of ADC. That would probably require either
;two loops, so either X or Y could be the index to the lookup table, or just unroll the loop...there are only 11 values and they could be specified as hard coded offset , i.e. +$00, $01etc.


	LDX #$00
	LDY #SCREEN.ARRAY.LAST_COLUMN_START
	
.CLEANUP.LOOP1
	
	LDA #$00
	STA SCREEN.DARK.DATA,X
	STA SCREEN.DARK.DATA_BEFORE,Y
	
	LDA #$FF
	STA SCREEN.MO_GENERAL.DATA, Y		
	STA SCREEN.MO_SPRITE.DATA, Y
	STA SCREEN.MO_SPRITE_TYPE.DATA, Y
	
	CPX #SCREEN.ARRAY.LAST_ROW_START
	BEQ .EXIT
	
	TXA
	CLC
	ADC	#SCREEN.ARRAY.OFFSET
	TAX
	
	TYA
	CLC
	ADC	#SCREEN.ARRAY.OFFSET
	TAY
	
	JMP .CLEANUP.LOOP1
@END

.MOVE.WEST.SCROLL.RIGHT
@START
	
.INIT.INDEX.COUNTERS2

	LDY #SCREEN.ARRAY.LAST_ELEMENT			;COPY TO
	TYA
	TAX
	DEX	;SET X-REG TO 1 LESS THAN Y-REG		;COPY FROM
.SCROLL.LOOP2
	LDA SCREEN.TILE.DATA, X					;SCROLL TILE_TYPE VALUES
	STA SCREEN.TILE.DATA, Y

	LDA SCREEN.MO_GENERAL.DATA, X		;SCROLL MAP OBJECT DATA 
	STA SCREEN.MO_GENERAL.DATA, Y		

	LDA SCREEN.MO_SPRITE.DATA, X			;SCROLL MAP OBJECT DATA
	STA SCREEN.MO_SPRITE.DATA, Y

	LDA SCREEN.MO_SPRITE_TYPE.DATA, X			;SCROLL MAP OBJECT DATA
	STA SCREEN.MO_SPRITE_TYPE.DATA, Y
	
	LDA SCREEN.DARK.DATA,X					;SCROLL DARKESS DATA. 3 THINGS ARE HAPPENNING. SCROLL THE DARKESSS ON SCREEN LAST MOVE (SCREEN.DATA.DARK) AND SAVE THE SCROLLED VALUES INTO SCREEN.DARK.DATA_BEFORE. THEN INIT THE SCREEN.DARK.DATA ARRAY TO $00s SO IT'S READY FOR USE BY DARKNESS.REVIEW IN THE CURRENT MOVE BEING PROCESSED
	STA SCREEN.DARK.DATA_BEFORE,Y

	LDA #$00
	STA SCREEN.DARK.DATA,X				
	
	CPX #$00
	BEQ .CLEANUP2
	DEX
	DEY
	JMP .SCROLL.LOOP2

.CLEANUP2	
;INITS TO $00 THE LAST COLUMN IN SCREEN.DARK.DATA, AND FIRST COLUMN FOR SCREEN.DARK.DATA_BEFORE

	LDX #SCREEN.ARRAY.LAST_COLUMN_START	
	LDY #$00
	
.CLEANUP.LOOP2
	
	LDA #$00
	STA SCREEN.DARK.DATA,X
	STA SCREEN.DARK.DATA_BEFORE,Y
	
	LDA #$FF
	STA SCREEN.MO_GENERAL.DATA, Y		
	STA SCREEN.MO_SPRITE.DATA, Y
	STA SCREEN.MO_SPRITE_TYPE.DATA, Y

	CPX #SCREEN.ARRAY.LAST_ELEMENT
	BEQ .EXIT

	TXA
	CLC
	ADC	#SCREEN.ARRAY.OFFSET
	TAX
	
	TYA
	CLC
	ADC	#SCREEN.ARRAY.OFFSET
	TAY
	
	
	JMP .CLEANUP.LOOP2	
@END
	
.EXIT

	RTS	

.ERROR
	;SCOLL.COLUMN.DATA REPORTS INVALID VALUE IN PLAYER.MOVE.CURRENT
	BRK	
@END
	
SCROLL.ROWS				;=============SCROLLS ONSCREEN COLUMN TILES=========
@START

;PARAMTERS: PLAYER.MOVE.CURRENT
;RETURN: NONE

;=====================SUBROUTINE DOCUMENTATION====================================
;This subroutine shifts all tiles on screen over 1 tile up or down, which is
;is used by the player movement routines MOVE.NORTH/SOUTH
;
;The result is one row of tiles is overwritten. For example, if the player move is south
;The tiles are shifted 1 tile up (opposite direction of player move). This causes the tiles originally in 
;screen row 0 to be overwitten (i.e. disappear from the screen). The bottom most row on the screen will have 
;a duplicate set of tiles, the same as in the 2nd row from the bottom. This prepares DRAW.ROW.SINGLE to
;obtain updated tile data (net of the player move) and draw it in the bottom row. 
;
;To execute the scrolling, each screen byte is copied from the foreground page to the background page, and the memory address
;on the background page is adjusted to reflect the new location (+/- 16 lines)
;=================================================================================

	
	JSR SCROLL.ROW.DATA			;SCROLLS THE TILE_TYPE DATA IN SCREEN.TILE.DATA
		
.INIT.SCREEN					;SETUP BYTE, LINE, DEPTH
	; LDA ANIMATION.DEEP_WATER.TALLY	
	; SEC
	; SBC #SCREEN.ROW.SIZE			;SUBTRACT A ROWS WORTH OF TILES FROM THE DEEP WATER TALLY IN PREPERATION FOR A NEW COLUMN TO EVENTUALLY BE DRAWN. 
	; STA ANIMATION.DEEP_WATER.TALLY
	
	LDA #SCREEN.DRAW.START_LINE										
	STA TILE.LINE.START			;SET STARTING LINE FOR TILES IN ROW 0
	CLC
	ADC #TILE.DEPTH.STANDARD
	STA TILE.LINE.COPY
	
	LDA PLAYER.MOVE.CURRENT		;#$00=north, $01=south, $02=east, $03=west
	CMP #$00
	BEQ	.MOVE.NORTH.SCROLL.DOWN
	CMP #$01
	BNE	*+$5
	JMP .MOVE.SOUTH.SCROLL.UP
	JMP .ERROR

.MOVE.NORTH.SCROLL.DOWN
@START

;NOTE: unlike data scrolling, multi-page graphics scolling can start the scroll at the top of the screen and iterate to the bottom, for north,
;		because the graphics data doesn't get overwritten as the copy progresses because it is copying from page1 to page2
	
.COLUMNLOOP.NORTH

	LDX TILE.LINE.START		;PREPARE FOR NEW COLUMN: RESET CURRENT LINE IN TILE TO DRAW TO THE STARTING LINE
	
	LDA TILE.LINE.START
	CLC
	ADC #TILE.DEPTH.STANDARD			;ADD DEPTH OF SHAPE TO STARTING LINE IN HEX (# of lines, not the position of last line....so line positions $0-$F is $10 (!16) total lines)
	STA TILE.LINE.STOP		;SET THE LAST LINE TO DRAW BASED ON THE DEPTH OF THE TILE	
	
.LINELOOP.NORTH
	
	LDY #SCREEN.DRAW.START_BYTE		;RESET SCREEN BYTE INDEX TO THE FIRST TILE COLUMN	
	
.GET.LINE.ADDRESS1A	
	LDA PAGE.FOREGROUND		;SPECIFY PAGE TO GET ADDRESS FOR	
	CMP #$02
	BEQ .ADDR1A.LOOKUP.PAGE2
	
.ADDR1A.LOOKUP.PAGE1	
	LDA LINE.HO.P1,X		;GET LINE ADDRESS
	STA LINE.BASE.ADDR1+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR1
	JMP .LOOKUP1A.COMPLETE

.ADDR1A.LOOKUP.PAGE2
	LDA LINE.HO.P2,X		;GET LINE ADDRESS
	STA LINE.BASE.ADDR1+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR1

.LOOKUP1A.COMPLETE
	
.GET.LINE.ADDRESS2A
	TXA						;X is the tile.line counter, and used by the get line address routine, so we need to save it to the stack
	PHA
	LDX TILE.LINE.COPY
	
	LDA PAGE.BACKGROUND		;SPECIFY PAGE TO GET ADDRESS FOR
	CMP #$02
	BEQ .ADDR2A.LOOKUP.PAGE2
	
.ADDR2A.LOOKUP.PAGE1	
	LDA LINE.HO.P1,X		;GET LINE ADDRESS
	STA LINE.BASE.ADDR2+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR2
	JMP .LOOKUP2A.COMPLETE

.ADDR2A.LOOKUP.PAGE2
	LDA LINE.HO.P2,X		;GET LINE ADDRESS
	STA LINE.BASE.ADDR2+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR2

.LOOKUP2A.COMPLETE
	PLA						;RESTORE X-REG FROM STACK. IT WAS TRANSFERED THERE AT THE START OF THE GET LINE ADDRESS2 ROUTINE
	TAX
	
.COPYLOOP.NORTH
;COPY TILE DATA	

;**OPT** Speed. Unroll this loop. put in the code to copy each screen byte. this would eliminate the CPY, BCS and JMP instructions on each interation
;				Best to wait until the screen size is final

;**OPT** Speed. replace all the absolute mode references with immediate mode. For example. LDX #$02, instead of LDX #SCREEN.DRAW.START_BYTE. UPDATE: THIS SHOULDN'T MATTER, THE ASSEMLBER REPLACES CONSTANCES WITH ABSOLUTE REFERENCES.
;				Best to wait until the screen size is final	

	LDA (LINE.BASE.ADDR1),Y				;TILE DATA: COPY FROM	
	STA (LINE.BASE.ADDR2),Y				;TILE DATA: COPY TO
	
	INY									;ADVANCE TO NEXT SCREEN BYTE

	CPY #SCROLL.STOP_BYTE				;IS LINE COMPLETE?
	BCS .LINE.COMPLETE.NORTH			;IF YES, SWITCH OVER TO NEXT ROW (BCS: is Y-REG >= CPY value)
	JMP .COPYLOOP.NORTH					;IF NO, COPY NEXT SCREEN BYTE

.LINE.COMPLETE.NORTH

	INX									;NEXT TILE LINE
	INC TILE.LINE.COPY
		
	CPX TILE.LINE.STOP					;IS TILE DONE?	
	BCC .LINELOOP.NORTH					;IF NO, DRAW NEXT LINE (BCC: is ACC < CMP value)

.ROW.COMPLETE.NORTH		
	LDA TILE.LINE.START					;ADVANCE LINE TO NEXT ROW OF TILES
	CLC
	ADC #TILE.DEPTH.STANDARD
	STA TILE.LINE.START
	CLC
	ADC #TILE.DEPTH.STANDARD
	STA TILE.LINE.COPY				;START LINE OF TILE 1 ROW DOWN	
	CMP	#SCREEN.DRAW.STOP_LINE		;ARE ALL ROWS SCROLLED?
	BCS .DONE						;IF YES, EXIT (BCS: is ACC >= CMP value)
	JMP .COLUMNLOOP.NORTH
.DONE
	;Branch Out of Range correction
	JMP .EXIT
@END
	
.MOVE.SOUTH.SCROLL.UP
@START
	
.COLUMNLOOP.SOUTH

	LDX TILE.LINE.START		;PREPARE FOR NEW COLUMN: RESET CURRENT LINE IN TILE TO DRAW TO THE STARTING LINE
	
	LDA TILE.LINE.START
	CLC
	ADC #TILE.DEPTH.STANDARD			;ADD DEPTH OF SHAPE TO STARTING LINE IN HEX (# of lines, not the position of last line....so line positions $0-$F is $10 (!16) total lines)
	STA TILE.LINE.STOP		;SET THE LAST LINE TO DRAW BASED ON THE DEPTH OF THE TILE	
	
.LINELOOP.SOUTH
	
	LDY #SCREEN.DRAW.START_BYTE		;RESET SCREEN BYTE INDEX TO THE FIRST TILE COLUMN	
	
.GET.LINE.ADDRESS1B	
	TXA						;X is the tile.line counter, and used by the get line address routine, so we need to save it to the stack
	PHA
	LDX TILE.LINE.COPY
		
	LDA PAGE.FOREGROUND		;SPECIFY PAGE TO GET ADDRESS FOR	
	CMP #$02
	BEQ .ADDR1B.LOOKUP.PAGE2
	
.ADDR1B.LOOKUP.PAGE1	
	LDA LINE.HO.P1,X		;GET LINE ADDRESS
	STA LINE.BASE.ADDR1+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR1
	JMP .LOOKUP1B.COMPLETE

.ADDR1B.LOOKUP.PAGE2
	LDA LINE.HO.P2,X		;GET LINE ADDRESS
	STA LINE.BASE.ADDR1+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR1

.LOOKUP1B.COMPLETE
	PLA						;RESTORE X-REG FROM STACK. IT WAS TRANSFERED THERE AT THE START OF THE GET LINE ADDRESS2 ROUTINE
	TAX
	
.GET.LINE.ADDRESS2B

	LDA PAGE.BACKGROUND		;SPECIFY PAGE TO GET ADDRESS FOR
	CMP #$02
	BEQ .ADDR2B.LOOKUP.PAGE2
	
.ADDR2B.LOOKUP.PAGE1	
	LDA LINE.HO.P1,X		;GET LINE ADDRESS
	STA LINE.BASE.ADDR2+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR2
	JMP .LOOKUP2B.COMPLETE

.ADDR2B.LOOKUP.PAGE2
	LDA LINE.HO.P2,X		;GET LINE ADDRESS
	STA LINE.BASE.ADDR2+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR2

.LOOKUP2B.COMPLETE
;	PLA						;RESTORE X-REG FROM STACK. IT WAS TRANSFERED THERE AT THE START OF THE GET LINE ADDRESS2 ROUTINE
;	TAX
	
.COPYLOOP.SOUTH
;COPY TILE DATA	

;**OPT** Speed. Unroll this loop. put in the code to copy each screen byte. this would eliminate the CPY, BCS and JMP instructions on each interation
;				Best to wait until the screen size is final

;**OPT** Speed. replace all the absolute mode references with immediate mode. For example. LDX #$02, instead of LDX #SCREEN.DRAW.START_BYTE. 
;				Best to wait until the screen size is final	

	LDA (LINE.BASE.ADDR1),Y				;TILE DATA: COPY FROM	
	STA (LINE.BASE.ADDR2),Y				;TILE DATA: COPY TO
	
	INY									;ADVANCE TO NEXT SCREEN BYTE

	CPY #SCROLL.STOP_BYTE				;IS LINE COMPLETE?
	BCS .LINE.COMPLETE.SOUTH			;IF YES, SWITCH OVER TO NEXT ROW (BCS: is Y-REG >= CPY value)
	JMP .COPYLOOP.SOUTH					;IF NO, COPY NEXT SCREEN BYTE

.LINE.COMPLETE.SOUTH

	INX									;NEXT TILE LINE
	INC TILE.LINE.COPY
		
	CPX TILE.LINE.STOP					;IS TILE DONE?	
	BCC .LINELOOP.SOUTH					;IF NO, DRAW NEXT LINE (BCC: is ACC < CMP value)

.ROW.COMPLETE.SOUTH	
	LDA TILE.LINE.START					;ADVANCE LINE TO NEXT ROW OF TILES
	CLC
	ADC #TILE.DEPTH.STANDARD
	STA TILE.LINE.START
	CLC
	ADC #TILE.DEPTH.STANDARD
	STA TILE.LINE.COPY				;START LINE OF TILE 1 ROW DOWN	
	CMP	#SCREEN.DRAW.STOP_LINE		;ARE ALL ROWS SCROLLED?
	BCS .EXIT						;IF YES, EXIT (BCS: is ACC >= CMP value)
	JMP .COLUMNLOOP.SOUTH
@END

.EXIT
		
	RTS	

.ERROR
	;SCOLL.ROWS REPORTS INVALID VALUE IN PLAYER.MOVE.CURRENT
	BRK	
@END

SCROLL.ROW.DATA		;======SCROLLS DATA IN SCREEN.TILE.DATA ARRAY=====
@START
;PARAMETERS: GMAP
;RETURN: SCREEN.TILE.DATA (UPDATED)
;ENTRANCE: SCROLL.ROW, DIRECT

;=====================SUBROUTINE DOCUMENTATION====================================
;This subroutine serves the same purpose as SCROLL.ROWS, which is to scroll the graphics screen as part of 
;the process of setting up a new graphics screen to reflect a player move. 
;
;The difference is that this subroutine is scrolling the data in the screen arrays instead of
;the bytes stored in the graphics screen memory. The screen arrays contain the data which the subtroutines in graphics_engine.asm
;use to draw the tiles on screen. Accordingly, if only the bytes in graphics memory were scrolled, the screen arrays
;would think a different tiles existed on the screen than the tiles visible on the screen. This would cause a problem
;because many subroutines use the screen arrays to determine what the tile_ID or map object present at a specific screen tile location.  
;
;=================================================================================


	LDA PLAYER.MOVE.CURRENT		;#$00=north, $01=south, $02=east, $03=west
	CMP #$00
	BEQ	.MOVE.NORTH.SCROLL.DOWN
	CMP #$01
	BEQ	.MOVE.SOUTH.SCROLL.UP
	JMP .ERROR
	
.MOVE.NORTH.SCROLL.DOWN
@START
	
.INIT.INDEX.COUNTERS1
	
	LDY #SCREEN.ARRAY.LAST_ELEMENT			;COPY TO
	
	TYA
	SEC
	SBC #SCREEN.ARRAY.OFFSET
	TAX	;SET X-REG TO NEXT ROW				;COPY FROM
		

.SCROLL.LOOP1
	LDA SCREEN.TILE.DATA, X					;SCROLL TILE_TYPE DATA 
	STA SCREEN.TILE.DATA, Y	

	LDA SCREEN.MO_GENERAL.DATA, X		;SCROLL MAP OBJECT DATA 
	STA SCREEN.MO_GENERAL.DATA, Y		

	LDA SCREEN.MO_SPRITE.DATA, X			;SCROLL MAP OBJECT DATA
	STA SCREEN.MO_SPRITE.DATA, Y

	LDA SCREEN.MO_SPRITE_TYPE.DATA, X			;SCROLL MAP OBJECT DATA
	STA SCREEN.MO_SPRITE_TYPE.DATA, Y
	
	LDA SCREEN.DARK.DATA,X					;SCROLL DARKESS DATA. 3 THINGS ARE HAPPENNING. SCROLL THE DARKESSS ON SCREEN LAST MOVE (SCREEN.DATA.DARK) AND SAVE THE SCROLLED VALUES INTO SCREEN.DARK.DATA_BEFORE. THEN INIT THE SCREEN.DARK.DATA ARRAY TO $00s SO IT'S READY FOR USE BY DARKNESS.REVIEW IN THE CURRENT MOVE BEING PROCESSED
	STA SCREEN.DARK.DATA_BEFORE,Y
	
	LDA #$00
	STA SCREEN.DARK.DATA,X	
	
	CPX #$00
	BEQ .CLEANUP1
	DEX
	DEY
	JMP .SCROLL.LOOP1

.CLEANUP1		;INITS TO $00 THE LAST ROW IN SCREEN.DARK.DATA AND FIRST ROW IN SCREEN.DARK.DATA_BEFORE (THE SCROLL LOOP ABOVE MISSES THESE LOCATIONS)
	LDX #SCREEN.ARRAY.LAST_ROW_START
	LDY #$00
	
.CLEANUP.LOOP1
	
	LDA #$00
	STA SCREEN.DARK.DATA,X					
	STA SCREEN.DARK.DATA_BEFORE,Y
	
	LDA #$FF
	STA SCREEN.MO_GENERAL.DATA, Y		
	STA SCREEN.MO_SPRITE.DATA, Y
	STA SCREEN.MO_SPRITE_TYPE.DATA, Y
	
	CPX #SCREEN.ARRAY.LAST_ELEMENT
	BEQ .EXIT
	INX
	INY
	JMP .CLEANUP.LOOP1

@END

.MOVE.SOUTH.SCROLL.UP
@START

.INIT.INDEX.COUNTERS2
	
	LDY #$00 						;COPY TO
	
	TYA
	CLC
	ADC #SCREEN.ARRAY.OFFSET
	TAX	;SET X-REG TO NEXT ROW		;COPY FROM

.SCROLL.LOOP2
	LDA SCREEN.TILE.DATA, X
	STA SCREEN.TILE.DATA, Y
	
	LDA SCREEN.MO_GENERAL.DATA, X		;SCROLL MAP OBJECT DATA 
	STA SCREEN.MO_GENERAL.DATA, Y		

	LDA SCREEN.MO_SPRITE.DATA, X			;SCROLL MAP OBJECT DATA
	STA SCREEN.MO_SPRITE.DATA, Y	

	LDA SCREEN.MO_SPRITE_TYPE.DATA, X			;SCROLL MAP OBJECT DATA
	STA SCREEN.MO_SPRITE_TYPE.DATA, Y
	
	LDA SCREEN.DARK.DATA,X					;SCROLL DARKESS DATA. 3 THINGS ARE HAPPENNING. SCROLL THE DARKESSS ON SCREEN LAST MOVE (SCREEN.DATA.DARK) AND SAVE THE SCROLLED VALUES INTO SCREEN.DARK.DATA_BEFORE. THEN INIT THE SCREEN.DARK.DATA ARRAY TO $00s SO IT'S READY FOR USE BY DARKNESS.REVIEW IN THE CURRENT MOVE BEING PROCESSED
	STA SCREEN.DARK.DATA_BEFORE,Y
	LDA #$00
	STA SCREEN.DARK.DATA,X	
	
	CPX #SCREEN.ARRAY.LAST_ELEMENT
	BEQ .CLEANUP2
	INX
	INY
	JMP .SCROLL.LOOP2


;**OPT** Speed. It's possible that the new/column row in screen.dark.data_before doens't need to be init to zero. It doesn't really matter that a new column/row of tiles will appear there.
;				if there was a dark tile scrolled into that position, then great, don't erase it. and if it's not flagged dark based on the new tile data, then the draw.row/tile routine will draw it. 	
.CLEANUP2		;INITS TO $00 THE FIRST ROW IN SCREEN.DARK.DATA AND LAST ROW IN SCREEN.DARK.DATA_BEFORE (THE SCROLL LOOP ABOVE MISSES THESE LOCATIONS)
	LDX #$00
	LDY #SCREEN.ARRAY.LAST_ROW_START
	
.CLEANUP.LOOP2
	
	LDA #$00
	STA SCREEN.DARK.DATA,X					
	STA SCREEN.DARK.DATA_BEFORE,Y
	
	LDA #$FF
	STA SCREEN.MO_GENERAL.DATA, Y		
	STA SCREEN.MO_SPRITE.DATA, Y
	STA SCREEN.MO_SPRITE_TYPE.DATA, Y
	
	CPY #SCREEN.ARRAY.LAST_ELEMENT
	BEQ .EXIT
	INX
	INY
	JMP .CLEANUP.LOOP2
@END	
	
.EXIT

	RTS	

.ERROR
	;SCOLL.ROW.DATA REPORTS INVALID VALUE IN PLAYER.MOVE.CURRENT
	BRK	
@END	