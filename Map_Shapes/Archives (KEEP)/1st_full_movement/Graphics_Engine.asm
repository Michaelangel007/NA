                .CR     6502            Use 6502 overlay
				.OR		$6000			**Always put before .TF directive and never use again in program
				.TF     TEST.BIN,BIN
				.EF		errors
				;.LI 	OFF				Switches the assembler machine code conversion listing off so errors are easier to see				
				.LF		C:\MY_CODE\LIST
				;				***For include files, look at end of program, before variable definitions. 

;
;
;EXAMPLE PURPOSE: 
;
;
;Hi-Res screen is !40 (!0-!39) ($0 - $27) screen bytes wide and !192 (!0-!192) ($0 - $BF) lines deep. 
;
;
;DRAW-DRAW vs. EOR (see HGUA p. 52-53 )
;							EOR		Draw-Draw	
;Speed						slower	faster
;Memory/Disk				same	same*		
;Flicker					some	none
;Draw over backgrounds		yes		no
;Collision detection		yes		difficult
;
;*unless the shape will move reappear at top of screen after reaching the bottom (or vice-versa). This is because and EOR erase routine is required to erase the shape at the screen edge.
;
;Conclusion: Draw-Draw seems the best choice for tiles graphics RPG animation. EOR (Draw-Ease) appears most useful in arcade game development. 
;
;DRAW-DRAW Concept
;
;By placing a border byte of #$00 at the top of the shape (if shape is moving down), the need for an special erase routine
;is eliminated. Because, after incementing the line, the shape bytes are redrawing over the top of the old shape.
;the only byte that wouldn't normally be replaced in this process is the top byte of the shape. Adding the border
;ensures that the top byte of the shape is erased by the redraw of the border shape byte.
;
;Exception: if a shape is moving looping from bottom of screen to top (& vice-versa), then an EOR erase is needed
;when the shape reaches the top/bottom of the screen. 
;
;The border is always place on the side (top/bottom) of the shape opposite of the diretion of movement. For example, if the shape is moving down, the border would be on the top of the shape.
;The border should be equal to the maximum number of lines the shape will move at a time. 
;
;
;
;GRAPHICS QUESTIONS (General)
;
;(EXPLAINED)*why does the upper part of the stick mans leg (2 dots) in the 2nd book example not appear on screen?
;			.The upper part of the legs were ommited in my code by mistake. However, when added, it just extends the torso with a green or violet
;			line. This is because for whatever reason, #$14 (two pixels on, with one off pixel between them) show up as 3 pixels on. 
;			The color appears to be determined based on what should plot (no adjacent ON pixels), which results in colored pixels, either
;			green or violet depending on whether the pixels are on odd or even columns. 
;			
;			Update: I observed with a different shape where two color dots were plotted in AppleWIN (full screen and regular, with 
;			a 1 dot HEX value), that only one dot was plotted on a physical Apple IIe. 
;			In looking at some games in an emaultor and physical Apple IIe, it looks like colors act really wierd in AppleWIN
;			Mountains look most purple, whereas they look white on the IIe, for example. There is a little "color bleed" on the 
;			physical IIe but it's much worse in the emualtor. It looks to me like games were just designed so that the bleed wasn't
;           a distraction. (I checked in the Virtual II Mac emulator and it seems to behave the same as AppleWIN as it relates
;			to color)


;=======PROGRAM START======

;	JMP DRIVER		;don't turn graphics mode on, so register output is visible from monitor prompt on BRK (testing)	

;Turn Hi-RES ON
	
PGM	NOP
	
;	LDA #$00		;SPECIFY BOTH PAGES FOR CLEAR SCREEN
;	JSR SCLEAR		;CLEAR SCREEN BEFORE TURNING ON GRAPHICS (AVOIDS UNSIGHTLY FLASH OF RANDOM DOTS) 
		
	LDA GRAPHICS	;TURN ON GRAPHICS MODE
	LDA HIRES		;SELECT HI-RES MODE
	LDA	PAGE1		;SELECT PAGE 1
	LDA MIXOFF		;SELECT FULL SCREEN GRAPHICS (PAGE 1)
	
	JMP DRIVER		;skip testing area

;===============TESTING AREA======

	LDA #$00		;SPECIFY BOTH PAGES FOR CLEAR SCREEN
	JSR SCLEAR		;CLEAR SCREEN BEFORE TURNING ON GRAPHICS (AVOIDS UNSIGHTLY FLASH OF RANDOM DOTS) 


	LDA	PAGE1
	
	
	
	
	LDX #$BF
	LDY #$00
	
	LDA #$01
	JSR GET.LINE.ADDRESS1	;Get Line Address	
	LDA #$7F
	STA (LINE.BASE.ADDR1),Y	;PLOT
	
	BRK
	
	
	LDX #$BF
	LDY #$00


	
	LDA LINE.HO.P1,X
	STA $77
	LDA LINE.LO,X
	STA $76
	LDA #$7F
	STA ($76),Y	;PLOT
	
	INY
		
	LDA LINE.HO.P1,X
	STA $77
	LDA LINE.LO,X
	STA $76
	LDA #$7F
	STA ($76),Y	;PLOT			
	
	


;	BRK
	
;====================MAIN PROGRAM============

DRIVER


;**OPT** THIS CAN BE REMOVED	
	LDA #$00		;SPECIFY BOTH PAGES FOR CLEAR SCREEN
	JSR SCLEAR		;CLEAR SCREEN BEFORE TURNING ON GRAPHICS (AVOIDS UNSIGHTLY FLASH OF RANDOM DOTS) 
	
	
;NOT SURE WHERE THIS SHOULD GO, PROBABLY AT VERY START OF GAME LAUNCH, PROBABLY FROM THERE IT IS ALL CALLS TO FLIP.PAGE
	LDA #$01						;
	STA PAGE.ACTIVE					;DRAW.TILE WILL DRAW THE OPPOSITE PAGE FROM THAT SPECIFED BY THIS VARIABLE
	LDA #$02
	STA PAGE.OPPOSITE	;**CLARITY*** rename to PAGE.FOREGROUND/PAGE.BACKGROUND
	LDA	PAGE1
	
;SETUP STARTING POSITION ON THE MAP
	LDA #$8A				;$48 is upper left corner. $2B is one row up, 5 rows left of the lower right corner. 
	STA GMAP				;$08D IS ANOTHER GOOD LOCATION
	LDA #$08				;$01 is upper left corner. $0D is one row up, 5 rows left of the lower right corner. 
	STA GMAP+$1
	
				;LDX #$00
				;JSR DRAW.ROW.SINGLE	
				;JSR FLIP.PAGE
				;BRK
	
	JSR DRAW.SCREEN
	JSR FLIP.PAGE
		
		
.KEYIN  

	LDA $C000
    BPL .KEYIN
    STA $C010               ;CLR LAST KEY
	
	CMP #$8B			;UP ARROW
	BEQ .NORTH
	CMP #$8A			;DOWN ARROW
	BEQ	.SOUTH
	CMP #$95			;RIGHT ARROW
	BEQ .EAST
	CMP #$88			;LEFT ARROW
	BEQ	.WEST

;ANY OTHER KEY PRESSED RESULTS IN NORTH	
.NORTH
	JSR MOVE.NORTH
	JMP .KEYIN
	
.SOUTH
	JSR MOVE.SOUTH
	JMP .KEYIN

.EAST
	JSR MOVE.EAST
	JMP .KEYIN

.WEST 
	JSR MOVE.WEST	
	JMP .KEYIN
	

	

DRAW.SCREEN		;=============DRAWS SCREEN FROM TILE DATA, ANYWHERE ON THE MAP=========
;PARAMTERS: GMAP, PAGE.ACTIVE, PAGE.OPPOSITE
;RETURN: NONE


.INIT.MAP
	;**OPT** MAYBE WE CAN JUST TRACK SMAP, AND DROP GMAP. ANY CALCULATION WE NEED TO DO WILL BE RELATIVE OT THE UPPER LEFT OF THE SCREEN. SEEMS LIKE THAT SHOULD WORK. 
	LDA GMAP					;LOAD TILE_ID OF CURRENT MAP POSITION
	STA OP1
	LDA GMAP+$1
	STA OP1+$1
	
	LDA #OFFSET.SCREEN			;CALCULATE TILE_ID OF TILE IN UPPER LEFT CORNER OF SCREEN
	STA OP2
	LDA /OFFSET.SCREEN
	STA OP2+$1
	
	JSR SBC.16					;GMAP(2) - SCREEN.OFFSET.LO/HO (2)
			
	LDA RESULT					;SAVE TILE_ID OF UPPER LEFT SCREEN TILE
	STA SMAP
	STA SMAP.CURRENT
	LDA RESULT+$1
	STA SMAP+$1
	STA SMAP.CURRENT+$1
		

.INIT.SCREEN					;SETUP BYTE, LINE, DEPTH
	LDA	#SCREEN.START.BYTE											;**OPT** THIS IS DUPLICATE
	STA SCREEN.BYTE				;SET STARTING SCREEN BYTE			;**OPT** THIS IS DUPLICATE
	LDA #SCREEN.START.LINE										
	STA TILE.LINE				;SET CURRENT LINE TO PLOT WITHIN TILE	
	STA TILE.LINE.START			;SET STARTING LINE FOR TILES IN ROW 0

	
;**OPT** when this is used for drawing the screen at game start or first entering a map, this is 
;probably fine. we shouldn't need to clear the entire screen for movement, but that will not flow
;through draw.screen so it will probably get setup correctly to begin with. This is a reminder
;note to thing about if screen clears are being handled most efficiently in general.
	
.CLEAR.SCREEN					;CLEAR BACKGROUND PAGE (WHERE DRAW.TILE WILL PLOT TO)
	LDA #$03
	JSR SCLEAR					
			
.LOOP
	LDA #SCREEN.START.BYTE		;RESET SCREEN.BYTE TO THE FIRST TILE COLUMN
	STA SCREEN.BYTE
	
	JSR DRAW.ROW	

	LDA SMAP.CURRENT			;ADVANCE TO NEXT ROW ON THE SCREEN
	STA OP1
	LDA SMAP.CURRENT+$1
	STA OP1+$1
	LDA #OFFSET.DOWN
	STA OP2
	LDA #$00
	STA OP2+$1
	
	JSR ADC.16					;SMAP.CURRENT(2) + #OFFSET.DOWN(1)
	
	LDA RESULT
	STA SMAP.CURRENT
	LDA RESULT+$1
	STA SMAP.CURRENT+$1

	LDA TILE.LINE.START				;ADVANCE LINE TO NEXT ROW OF TILES
	CLC
	ADC #TILE.DEPTH
	STA TILE.LINE.START
	STA TILE.LINE
	CMP	#SCREEN.STOP.LINE			;READY TO SWITCH TILE COLUMNS? (TILE.LINE should already be in ACC at end of DRAW.TILE) 
	BCS .EXIT						;IF YES, EXIT (BCS: is ACC >= CMP value)

	JMP .LOOP

.EXIT
	RTS

DRAW.ROW.SINGLE ;=========DRAWS A SINGLE TILE ROW AT SPECIFIED LOCATION==========
;PARAMETERS: GMAP, X-REG (TILE ROW # TO DRAW)
;RETURN: NONE	
;ENTRANCE: DIRECT


.INIT.MAP
	LDA GMAP					;LOAD TILE_ID OF CURRENT MAP POSITION
	STA OP1
	LDA GMAP+$1
	STA OP1+$1
				
	LDA #OFFSET.SCREEN			;CALCULATE TILE_ID OF TILE IN UPPER LEFT CORNER OF SCREEN
	STA OP2
	LDA /OFFSET.SCREEN
	STA OP2+$1
				
	JSR SBC.16					;GMAP(2) - SCREEN.OFFSET.LO/HO (2)
			
	LDA RESULT					;SAVE TILE_ID OF UPPER LEFT SCREEN TILE
	STA SMAP
	STA SMAP.CURRENT
	LDA RESULT+$1
	STA SMAP+$1
	STA SMAP.CURRENT+$1
				
.INIT.SCREEN					;SETUP BYTE, LINE, DEPTH

;DETERMINE ROW TO DRAW BASED ON X-REG VALUE	
	CPX #$0B
	BEQ .ROW0B

;DEFAULT TO ROW1
	LDA	#SCREEN.START.LINE										
	STA TILE.LINE.START			;SET STARTING LINE TO DRAW ON
	STA TILE.LINE
	JMP .COMMON
	
.ROW0B
	LDA #SCROLL.STOP.LINE		;THE SCOLL STOP LINE +1 IS THE FIRST LINE IN THE LAST TILE ROW, WHICH IS WHERE WE WANT TO START TO DRAW TILES IN THE LAST ROW	
	CLC							;	I'M NOT SURE REALLY WHY IT'S +1, I DIDN'T CALCUALTE IT OUT, I JUST ADDED +1 TO SEE IF IT WOULD FIX A SLIGHT GLITCH WHEN MOVING SOUTH (SCROLLING UP), AND IT DID.
	ADC #$01	
	STA TILE.LINE.START			;SET STARTING LINE TO DRAW ON
	STA TILE.LINE	

.COMMON

	LDA	#SCREEN.START.BYTE									
	STA SCREEN.BYTE				;SET STARTING SCREEN BYTE	
				
	JSR DRAW.ROW
	
	RTS

DRAW.ROW		;=============DRAWS ROW FROM TILE DATA, AT SPECIFIED LOCATION=========
;PARAMTERS: SMAP.CURRENT, #SCREEN.BYTE.START, SCREEN.START.LINE, TILE_LINE_START, TILE_LINE
;	NOTE: SET TILE_LINE == TO TILE_LINE_START before calling this routine. This saves an LDA instruction versus doing an init of TILE_LINE in this routine. 
;RETURN: NONE
;ENTRANCE: DRAW.SCREEN, DRAW.ROW.SINGLE

;LOAD TILES
	LDA SMAP.CURRENT				;THE TILE_ID OF THE FIRST TILE IN THE CURRENT ROW
	STA GMAP.LOOKUP
	LDA SMAP.CURRENT+$1
	STA GMAP.LOOKUP+$1
			
	JSR TILE.LOOKUP.INDEX			;LOOKUP THE INDEX (ELEMENT OF THE GMAP.TILE.DATA.ARRAY THAT CONTAINS THE TILE TYPE FOR THE FIRST TILE IN THE CURRENT ROW).
	JSR TILE.LOOKUP.ROW				;USING THE INDEX, LOAD THE TILE_TYPE FOR EACH TILE IN THE CURRENT ROW 
			
	LDX #$00						;INIT X COUNTER FOR 1ST TILE IN ROW	
;
.LOOP

					
	LDA SCREEN.TILE.ROW,X			;LOAD TILE_TYPE OF THE NEXT TILE IN CURRENT ROW AND CALCUALTE THE 
		

.TEMP
	INX								;INCREMENT COUNTER SO WE GET THE NEXT TILE_TYPE ON THE NEXT INTERATION OF .LOOP
	
	ASL ;X2							;	OFFSET TO THE SHAPE TABLE FOR THAT TILE. ALL TILE SHAPE TABLES
	ASL ;X4							;	 ARE 32byts, SO THE OFFSET IS (TILE_TYPE * 32)
	ASL ;X8							;	 THE OFFSET IS ADDED TO THE ADDRESS OF THE FIRST TILE SHAPE TABLE
	ASL ;X16						;	 TO CALCUALTE THE ADDRESS OF THE SHAPE TABLE FOR THE CURRENT TILE.
	ASL ;X32
				
	STA OP1							;SAVE RESULT OF MULTIPLCATION (ASLs) ABOVE				
	LDA #$00
	STA OP1+$1						
				
	LDA #GRASS						;GRASS IS THE FIRST SHAPE TABLE. 
	STA OP2
	LDA /GRASS
	STA OP2+$1
					
	JSR ADC.16						;CURRENT TILE SHAPE OFFSET + ADDRESS OF FIRST TILE SHAPE TABLE (GRASS)
	
	LDA RESULT
	STA SHAPE
	LDA RESULT+$1
	STA SHAPE+$1

	JSR DRAW.TILE					;DRAW SHAPE
	
	INC SCREEN.BYTE					;NEXT TILE
    INC SCREEN.BYTE	
	
	LDA TILE.LINE.START
	STA TILE.LINE					; RESET CURRENT LINE IN TILE TO DRAW TO THE STARTING LINE

	LDA #SCREEN.STOP.BYTE			;AT RIGHT SIDE SCREEN EACH?
	CMP SCREEN.BYTE
	BCS .LOOP						;IF NO, SWITCH OVER TO NEXT ROW (BCS: is ACC >= CMP value)
	
	RTS

DRAW.COLUMN.SINGLE ;=========DRAWS A SINGLE TILE COLUMN AT SPECIFIED LOCATION==========
;PARAMETERS: GMAP, X-REG (TILE COLUMN # TO DRAW)
;RETURN: NONE	
;ENTRANCE: DIRECT


.INIT.MAP
	LDA GMAP					;LOAD TILE_ID OF CURRENT MAP POSITION
	STA OP1
	LDA GMAP+$1
	STA OP1+$1
				
	LDA #OFFSET.SCREEN			;CALCULATE TILE_ID OF TILE IN UPPER LEFT CORNER OF SCREEN
	STA OP2
	LDA /OFFSET.SCREEN
	STA OP2+$1
				
	JSR SBC.16					;GMAP(2) - SCREEN.OFFSET.LO/HO (2)
			
	LDA RESULT					;SAVE TILE_ID OF UPPER LEFT SCREEN TILE
	STA SMAP
	STA SMAP.CURRENT
	LDA RESULT+$1
	STA SMAP+$1
	STA SMAP.CURRENT+$1
				
.INIT.SCREEN					;SETUP BYTE, LINE, DEPTH

;DETERMINE COLUMN TO DRAW BASED ON X-REG VALUE	
	CPX #$11
	BEQ .COLUMN17 

;DEFAULT TO COLUMN1	
	LDA	#SCREEN.START.BYTE											
	STA SCREEN.BYTE				;SET STARTING SCREEN BYTE			
	JMP .COMMON
	
.COLUMN17
	LDA	#SCREEN.STOP.BYTE									
	STA SCREEN.BYTE				;SET STARTING SCREEN BYTE	

.COMMON

	LDA #SCREEN.START.LINE										
	STA TILE.LINE				;SET CURRENT LINE TO PLOT WITHIN TILE	
	STA TILE.LINE.START			;SET STARTING LINE FOR TILES IN ROW 0

				
	JSR DRAW.COLUMN	
	
	RTS
	

DRAW.COLUMN		;=============DRAWS COLUMN FROM TILE DATA, AT SPECIFIED LOCATION=========
;PARAMETERS: SMAP.CURRENT(2)
;RETURN: NONE	
;ENTRANCE: DRAW.COLUMN.SINGLE, (FUTURE)

;FUTURE: if it were useful at some point to draw multiple columns in a row, I think this
;routine could be called from a loop which did two INCs to SCREEN.BYTE after the call. HORSE MOVEMENT?
;In this scenario, TILE.LINE.START would need to be rest to the top of the screen. OR, don't increment tile.line.start, 
;at all in this routine and just increment TILE.LINE. I think if one line is added to TILE.LINE at each iteration of TIEL.DRAW, that will
;sync TILE.LINE up with the next tile.

	
;**OPT** MEMORY. MAYBE THIS ROUTINE COULD BE COMBINED WITH THE DRAW.ROW AND ROW OR COLULMN IS
;REQUESTED VIA THE CALLING ROUTINE BY A PARAMETER. 	
	
	
;LOAD TILES
	LDA SMAP.CURRENT				;THE TILE_ID OF THE FIRST TILE IN THE CURRENT ROW
	STA GMAP.LOOKUP
	LDA SMAP.CURRENT+$1
	STA GMAP.LOOKUP+$1
	
	JSR TILE.LOOKUP.COLUMN		;LOAD THE TILE_TYPE FOR EACH TILE IN THE CURRENT ROW
			
	LDX #$00						;INIT X COUNTER FOR 1ST TILE IN ROW	
;
.LOOP

					
	LDA SCREEN.TILE.ROW,X			;LOAD TILE_TYPE OF THE NEXT TILE IN CURRENT COLUMN
		

.TEMP
	INX								;INCREMENT COUNTER SO WE GET THE NEXT TILE_TYPE ON THE NEXT INTERATION OF .LOOP
	
	ASL ;X2							;	OFFSET TO THE SHAPE TABLE FOR THAT TILE. ALL TILE SHAPE TABLES
	ASL ;X4							;	 ARE 32byts, SO THE OFFSET IS (TILE_TYPE * 32)
	ASL ;X8							;	 THE OFFSET IS ADDED TO THE ADDRESS OF THE FIRST TILE SHAPE TABLE
	ASL ;X16						;	 TO CALCUALTE THE ADDRESS OF THE SHAPE TABLE FOR THE CURRENT TILE.
	ASL ;X32
				
	STA OP1							;SAVE RESULT OF MULTIPLCATION (ASLs) ABOVE				
	LDA #$00
	STA OP1+$1						
				
	LDA #GRASS						;GRASS IS THE FIRST SHAPE TABLE. 
	STA OP2
	LDA /GRASS
	STA OP2+$1
					
	JSR ADC.16						;CURRENT TILE SHAPE OFFSET + ADDRESS OF FIRST TILE SHAPE TABLE (GRASS)
	
	LDA RESULT
	STA SHAPE
	LDA RESULT+$1
	STA SHAPE+$1

	JSR DRAW.TILE					;DRAW SHAPE
	
;**OPT** SPEED/MEMORY. TILE.LINE IS IN ACC AT END OF DRAW.TILE. I MAY BE ABLE TO INCREMENT IT A LINE TO REACH THE START OF THE NEXT TILE INSTEAD OF LOADING TILE.START LINE
;EACH TIME. GIVE EXTRA THOUGHT TO THINKING THIS THROUGH IF THIS ROUTINE ENDS UP BEING CALLED IN A LOOP TO DRAW MULTIPLE COLUMNS. HORSE MOVEMENT?
;
	LDA TILE.LINE.START				;ADVANCE LINE TO NEXT ROW OF TILES
	CLC
	ADC #TILE.DEPTH
	STA TILE.LINE.START
	STA TILE.LINE		
	CMP	#SCREEN.STOP.LINE			;READY TO SWITCH TILE COLUMNS?
	BCS .EXIT						;IF YES, EXIT (BCS: is ACC >= CMP value)

	JMP .LOOP

.EXIT
	RTS
	
	
DRAW.TILE

.SAVE.REGISTERS
	TXA
	PHA

.START
	
	LDA #$00
	STA SHP.TBL.CNTR		;START DRAWING AT BEGINNING OF SHAPE TABLE

	LDA TILE.LINE.START		;THE STARTING LINE OF THE TILES IN THE CURRENT ROW
	CLC						;*OPT* I I THINK ALL CLS CAN BE REMOVED WHEN I'M NOT EXPECTING AN OVERFLOW
	ADC #TILE.DEPTH			;ADD DEPTH OF SHAPE TO STARTING LINE IN HEX (# of lines, not the position of last line....so line positions $0-$F is $10 (!16) total lines)
	STA TILE.LINE.STOP		;SET THE LAST LINE TO DRAW BASED ON THE DEPTH OF THE TILE

.DRAW.LOOP

	LDX TILE.LINE			;LOAD LINE IN X REGISTER
	
	LDA PAGE.OPPOSITE		;SPECIFY PAGE TO GET ADDRESS FOR (SHOULD BE BACKGROUND PAGE)
	JSR GET.LINE.ADDRESS1	;GET LINE ADDRESS
							
	LDY SHP.TBL.CNTR		;LOAD Y WITH SHAPE TABLE COUNTER
	LDA (SHAPE),Y			;LOAD SHAPE BYTE (1st screen byte of the tile)

	LDY SCREEN.BYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	STA (LINE.BASE.ADDR1),Y	;PLOT (1st screen byte)
	
	INC SHP.TBL.CNTR		;NEXT SHAPE BYTE, NEXT TILE LINE
	INC SCREEN.BYTE			;SWITCH TO 2ND SCREEN BYTE IN THE TILE
	
	LDY SHP.TBL.CNTR		;LOAD Y WITH SHAPE TABLE COUNTER
	LDA (SHAPE),Y			;LOAD SHAPE BYTE (2nd screen byte of the tile)
	LDY SCREEN.BYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	STA (LINE.BASE.ADDR1),Y				;PLOT (2st screen byte)
	
	DEC SCREEN.BYTE			;SWITCH BACK TO 1ST SCREEN BYTE IN THE TILE	;**OPT** if the counters are updated after the CMP/branch it might save 1 INC SCREEN.BYTE instruction in the main loop because SCREEN.BYTE will be in 2nd position when this loop ends. 
	INC TILE.LINE			;NEXT TILE LINE
	INC SHP.TBL.CNTR		;NEXT SHAPE BYTE
	
	LDA TILE.LINE				
	CMP TILE.LINE.STOP		;IS TILE DONE?							
	BCC .DRAW.LOOP			;IF NO, DRAW NEXT LINE (BCC: is ACC < CMP value)	

	
.RESTORE.REGISTERS
	PLA
	TAX
	
	RTS						

	
	
;======INCLUDE FILES======

				.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_graphics
				.IN 	C:\MY_CODE\INCLUDES_LIBS\tile_functions
				.IN 	C:\MY_CODE\INCLUDES_LIBS\graphics_scrolling
				.IN 	C:\MY_CODE\INCLUDES_LIBS\movement_manager
				.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_math	
;				.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_common
				
;======DEFINE VARIBLES======
GRAPHICS 			.EQ	$C050
HIRES				.EQ	$C057
PAGE1				.EQ	$C054
MIXOFF				.EQ	$C052
PAGE2				.EQ	$C055

WAIT				.EQ	$FCA8
DELAY				.EQ	$41				;#CONSTANT


LINE.BASE.ADDR1		.EQ	$EA				;2byt
LINE.BASE.ADDR2		.EQ	$EC				;2byt

SHAPE				.EQ	$FA				;2byt			;Used by DRAW.TILE. Holds the pointer to the active shape table to be drawn. 	



;SHAPE	.HS	7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F																						


;TDATA	.HS																						
GRASS			.HS 08.40.02.01.00.00.00.04.00.00.00.10.20.00.00.00.02.41.00.00.20.04.00.00.00.41.02.00.08.10.00.00
TREES			.HS	00.00.20.01.28.05.2A.15.2A.15.2A.15.2A.15.28.05.20.01.18.03.18.03.18.03.18.03.18.03.1C.07.0E.0E																					
TREES2			.HS	00.00.20.01.28.05.2A.15.2A.15.2A.15.2A.15.28.05.20.01.18.03.18.03.18.03.18.03.18.03.1C.07.0E.0E																					
A				.HS	40.01.40.01.30.06.30.06.30.06.18.0C.18.0C.18.0C.0C.18.7C.1F.7C.1F.06.30.06.30.06.30.03.60.03.60

;GRASS			.HS	00.00.20.01.28.05.2A.15.2A.15.2A.15.2A.15.28.05.20.01.18.03.18.03.18.03.18.03.18.03.1C.07.0E.0E																					
;GRASS2			.HS	00.00.20.01.28.05.2A.15.2A.15.2A.15.2A.15.28.05.20.01.18.03.18.03.18.03.18.03.18.03.1C.07.0E.0E																					
;GRASS3			.HS	00.00.20.01.28.05.2A.15.2A.15.2A.15.2A.15.28.05.20.01.18.03.18.03.18.03.18.03.18.03.1C.07.0E.0E																					
;GRASS4			.HS	00.00.20.01.28.05.2A.15.2A.15.2A.15.2A.15.28.05.20.01.18.03.18.03.18.03.18.03.18.03.1C.07.0E.0E																					


;BLANK			.HS	00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00
;BLANK2			.HS	00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00

;GMAP.TILE.DATA	.HS 01.01.00.01.00.00.01.00.00.01.00.00.00.00.01.01.01.00.00.00.01.01.00.00.00.00.00.01.00.01.00.00.00.01.01.00.00.01.00.01.00.00.00.01.00.01.00.00.00.01.00.00.01.01.01.00.00.01.00.01.01.01.01.01.00.01.01.01.01.01.00.00.00.00.01.00.00.00.01.01.01.01.00.00.01.00.01.00.01.00.00.01.01.00.00.01.01.00.00.01.00.01.01.01.01.00.01.01.00.00.00.01.00.00.01.01.00.00.01.00.00.00.00.01.01.00.01.00.01.01.00.00.00.00.01.00.01.00.01.00.01.01.00.01.00.00.01.00.00.01.00.00.00.00.01.01.00.00.00.01.01.00.01.01.00.01.00.00.01.00.01.01.01.01.00.01.01.00.01.00.00.01.01.00.01.01.01.00.01.00.00.01.01.00.01.00.00.00.00.00.01.01.01.00.00.00.01.00.00.01.01.01.00.01.01.01.00.01.00.01.00.01.00.01.01.00.01.01.00.01.00.00.00.01.01.00.01.01.00.01.00.00.01.00.01.00.01.01.01.00.01.01.00.00.01.00.01.00.00.00.01.01.00.01.01.00.00.01.00.00.00.00.01.01.00.00.01.01.00.00.01.01.01.00.00.00.00.01.01.01.00.01.01.01.00.00.00.01.01.01.00.00.00.00.00.00.00.01.00.00.01.01.01.01.01.00.01.01.00.01.00.01.01.00.01.00.00.00.00.01.01.00.00.00.01.00.00.00.00.01.01.01.01.00.01.00.00.01.01.01.00.01.00.00.01.00.01.01.00.01.00.01.01.00.01.00.01.01.00.00.01.00.00.01.00.00.01.01.00.01.00.01.00.01.00.00.00.00.01.01.00.00.01.00.01.01.01.00.00.01.01.01.01.01.01.01.01.01.01.00.01.00.00.01.00.01.00.01.00.01.00.00.00.00.00.00.00.01.00.00.00.01.01.01.00.00.01.01.01.01.00

;test 64x64 array, doesn't match spreadsheet lookup tool. Had to do with the $0A values. 
GMAP.TILE.DATA 	.HS	05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.03.05.01.02.00.05.02.05.00.04.00.05.01.05.01.05.03.02.00.02.01.05.01.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.02.05.00.05.02.05.03.02.00.02.01.05.02.05.00.05.03.09.00.05.01.02.00.05.00.05.01.04.00.05.02.05.02.05.03.02.00.02.01.05.03.05.00.05.03.09.00.05.01.02.00.05.02.05.02.04.00.05.02.05.03.05.03.02.00.02.01.05.03.05.00.05.03.09.00.05.01.02.00.05.02.05.01.04.02.05.00.05.03.05.03.02.00.02.01.05.03.05.00.05.03.09.00.05.01.02.00.05.01.05.02.04.00.05.01.05.01.05.03.02.00.02.01.05.01.05.00.05.03.09.02.05.01.02.00.05.02.05.00.04.00.05.00.05.01.05.03.02.00.02.01.05.01.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.00.05.03.05.01.05.03.02.00.02.01.05.01.05.03.05.03.09.00.05.01.02.00.05.02.05.02.04.01.05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.00.05.02.05.01.05.02.02.00.02.01.05.00.05.00.05.03.09.02.05.01.02.00.05.01.05.02.04.00.05.03.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.01.04.00.05.03.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.01.05.03.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.03.05.01.02.00.05.02.05.00.04.00.05.02.05.01.05.03.02.00.02.01.05.00.05.03.05.03.09.00.05.01.02.00.05.02.05.00.04.02.05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.00.05.01.04.00.05.01.05.01.05.01.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.02.04.00.05.00.05.01.05.03.02.00.02.01.05.03.05.00.05.03.09.00.05.01.02.00.05.02.05.01.04.02.05.02.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.01.05.02.04.00.05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.00.05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.00.05.02.05.01.05.01.02.00.02.01.05.00.05.03.05.03.09.00.05.01.02.00.05.02.05.02.04.01.05.00.05.01.05.03.02.00.02.01.05.03.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.00.05.03.05.01.05.01.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.01.05.02.04.00.05.02.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.01.04.00.05.03.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.01.05.00.05.01.05.03.02.02.02.01.05.02.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.00.05.03.05.01.05.03.02.02.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.02.05.00.05.01.05.03.02.02.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.00.05.01.04.00.05.03.05.01.05.03.02.03.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.02.04.00.05.00.05.01.05.03.02.03.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.01.04.02.05.00.05.01.05.03.02.03.02.01.05.01.05.03.05.03.09.00.05.01.02.00.05.01.05.02.04.00.05.00.05.01.05.03.02.03.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.00.05.00.05.01.05.03.02.03.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.00.05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.02.04.01.05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.00.05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.01.05.02.04.00.05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.01.04.00.05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.01.05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.00.05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.02.05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.00.05.01.04.00.05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.02.04.00.05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.01.04.02.05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.01.05.02.04.00.05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.00.05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.00.05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.02.04.01.05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.00.05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.01.05.02.04.00.05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.01.04.00.05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.01.05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.00.05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.02.05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.00.05.01.04.00.05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.02.04.00.05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.01.04.02.05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.01.05.02.04.00.05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.00.05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.00.05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.02.04.01.05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.00.05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.01.05.02.04.00.05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.01.04.00
;array elements		00.01.02.03.04.05.06.07.08.09.0A.0B.0C.0D.0E.0F.10.11.12.13.14.15.16.17.18.19.1A.1B.1C.1D.1E.1F.20.21.22.23.24.25.26.27.28.29.2A.2B.2C.2D.2E.2F.30.31.32.33.34.35.36.37.38.39.3A.3B.3C.3D.3E.3F.40.41.42.43.44.45.46.47.48.49.4A.4B.4C.4D.4E.4F.50.51.52.53.54.55.56.57.58.59.5A.5B.5C.5D.5E.5F.60.61.62.63.64.65.66.67.68.69.6A.6B.6C.6D.6E.6F.70.71.72.73.74.75.76.77.78.79.7A.7B.7C.7D.7E.7F.80.81.82.83.84.85.86.87.88.89.8A.8B.8C.8D.8E.8F.90.91.92.93.94.95.96.97.98.99.9A.9B.9C.9D.9E.9F.A0.A1.A2.A3.A4.A5.A6.A7.A8.A9.AA.AB.AC.AD.AE.AF.B0.B1.B2.B3.B4.B5.B6.B7.B8.B9.BA.BB.BC.BD.BE.BF.C0.C1.C2.C3.C4.C5.C6.C7.C8.C9.CA.CB.CC.CD.CE.CF.D0.D1.D2.D3.D4.D5.D6.D7.D8.D9.DA.DB.DC.DD.DE.DF.E0.E1.E2.E3.E4.E5.E6.E7.E8.E9.EA.EB.EC.ED.EE.EF.F0.F1.F2.F3.F4.F5.F6.F7.F8.F9.FA.FB.FC.FD.FE.FF.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01


;Line Lookup Tables. LINE.LO is used for Hi-Res Page 1 and Page 2. 
LINE.HO.P2 		.HS 40.44.48.4C.50.54.58.5C.40.44.48.4C.50.54.58.5C.41.45.49.4D.51.55.59.5D.41.45.49.4D.51.55.59.5D.42.46.4A.4E.52.56.5A.5E.42.46.4A.4E.52.56.5A.5E.43.47.4B.4F.53.57.5B.5F.43.47.4B.4F.53.57.5B.5F.40.44.48.4C.50.54.58.5C.40.44.48.4C.50.54.58.5C.41.45.49.4D.51.55.59.5D.41.45.49.4D.51.55.59.5D.42.46.4A.4E.52.56.5A.5E.42.46.4A.4E.52.56.5A.5E.43.47.4B.4F.53.57.5B.5F.43.47.4B.4F.53.57.5B.5F.40.44.48.4C.50.54.58.5C.40.44.48.4C.50.54.58.5C.41.45.49.4D.51.55.59.5D.41.45.49.4D.51.55.59.5D.42.46.4A.4E.52.56.5A.5E.42.46.4A.4E.52.56.5A.5E.43.47.4B.4F.53.57.5B.5F.43.47.4B.4F.53.57.5B.5F
LINE.HO.P1 		.HS 20.24.28.2C.30.34.38.3C.20.24.28.2C.30.34.38.3C.21.25.29.2D.31.35.39.3D.21.25.29.2D.31.35.39.3D.22.26.2A.2E.32.36.3A.3E.22.26.2A.2E.32.36.3A.3E.23.27.2B.2F.33.37.3B.3F.23.27.2B.2F.33.37.3B.3F.20.24.28.2C.30.34.38.3C.20.24.28.2C.30.34.38.3C.21.25.29.2D.31.35.39.3D.21.25.29.2D.31.35.39.3D.22.26.2A.2E.32.36.3A.3E.22.26.2A.2E.32.36.3A.3E.23.27.2B.2F.33.37.3B.3F.23.27.2B.2F.33.37.3B.3F.20.24.28.2C.30.34.38.3C.20.24.28.2C.30.34.38.3C.21.25.29.2D.31.35.39.3D.21.25.29.2D.31.35.39.3D.22.26.2A.2E.32.36.3A.3E.22.26.2A.2E.32.36.3A.3E.23.27.2B.2F.33.37.3B.3F.23.27.2B.2F.33.37.3B.3F
LINE.LO			.HS 00.00.00.00.00.00.00.00.80.80.80.80.80.80.80.80.00.00.00.00.00.00.00.00.80.80.80.80.80.80.80.80.00.00.00.00.00.00.00.00.80.80.80.80.80.80.80.80.00.00.00.00.00.00.00.00.80.80.80.80.80.80.80.80.28.28.28.28.28.28.28.28.A8.A8.A8.A8.A8.A8.A8.A8.28.28.28.28.28.28.28.28.A8.A8.A8.A8.A8.A8.A8.A8.28.28.28.28.28.28.28.28.A8.A8.A8.A8.A8.A8.A8.A8.28.28.28.28.28.28.28.28.A8.A8.A8.A8.A8.A8.A8.A8.50.50.50.50.50.50.50.50.D0.D0.D0.D0.D0.D0.D0.D0.50.50.50.50.50.50.50.50.D0.D0.D0.D0.D0.D0.D0.D0.50.50.50.50.50.50.50.50.D0.D0.D0.D0.D0.D0.D0.D0.50.50.50.50.50.50.50.50.D0.D0.D0.D0.D0.D0.D0.D0





;Screen/Tile Draw Variables
SCREEN.START.BYTE		.EQ $02					;#CONSTANT		STARTING SCREEN BYTE OF FIRST TILE
SCREEN.START.LINE		.EQ	$08					;#CONSTANT		STARTING LINE OF FIRST TILE
SCREEN.STOP.BYTE		.EQ $22					;#CONSTANT		STOP SCREEN BYTE ON RIGHT SIDE
SCREEN.STOP.LINE		.EQ	$B7					;#CONSTANT		STOP LINE AT SCREEN BOTTOM
SCREEN.LAST.COLUMN		.EQ	$20					;#CONSTANT		STARTING SCREEN BYTE FOR DRAWN THE COLUMN ON THE RIGHT EDGE OF SCREEN
SCREEN.BYTE				.BS	$1					;1byt			KEEPS TRACK OF THE CURRENT SCREEN BYTE IN DRAW.TILE
TILE.LINE				.BS	$1					;1byt			KEEPS TRACK OF THE CURRENT LINE IN DRAW.TILE
TILE.DEPTH				.EQ $10					;#CONSTANT		# OF LINES IN 1 TILE
TILE.DEPTH.2			.EQ $20					;#CONSTANT		# OF LINES IN 2 TILE
TILE.LINE.START			.BS	$1					;1byt			START LINE OF CURRENT TILE TO BE DRAWN (DRAW.TILE)
TILE.LINE.STOP			.BS	$1					;1byt			STOP LINE OF CURRENT TILE TO BE DRAWN (DRAW.TILE)
TILE.LINE.COPY			.BS $1					;1byt			USED FOR COPYING TILES UP/DOWN IN SCROLL.SCREEN AND ITS' SUBROUTINES
SCROLL.STOP.BYTE		.EQ $21					;#CONSTANT		STOP SCREEN BYTE ON RIGHT SIDE WHEN SCREEN SCROLLING
SCROLL.STOP.LINE		.EQ $A7					;#CONSTANT		STOP LINE AT BOTTOM WHEN SCREEN SCROLLING
SHP.TBL.CNTR			.BS	$1					;1byt


;MAP POSITION VARIABLES
;
;TILE ID number							not a variable, but is term used to describe the value stored in GMAP, from which other variables such as SMAP, SMAP.CURRENT, and GMAP.LOOKUP are dervied. The tile ID is a unique reference number to a specific tile on the map (from a human perspective). To the computer it is meaningful because the value of Tile ID is also equal to the quantity of tiles on the map in GMAP.TILE.DATA up to and including the tile assocaited with Tile ID. 
GMAP					.BS $2			;Tracks the position on the map in computer terms. Specifically, it is the tile ID number of the tile at the center of the screen where the player stands. 
GMAP.X					.BS	$1			;Future use. compass tracking, longitude. 
GMAP.Y					.BS $1			;Future use. compass tracking, latitude. 
SMAP					.EQ $8200
SMAP.CURRENT			.EQ $8202


;MAP MOVEMENT OFFSETS
OFFSET.UP				.EQ $40		;#CONSTANT (negative offset)
OFFSET.DOWN				.EQ $40		;#CONSTANT
OFFSET.LEFT				.EQ $01		;#CONSTANT (negative offset)
OFFSET.RIGHT			.EQ $01		;#CONSTANT 
OFFSET.SCREEN			.EQ	$148		;#CONSTANT (negative offset)


; RAW DATA FOR 64X64 TILE MAP (1 MAP ROW PER LINE X 64 LINES)

;05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.03.05.01.02.00.05.02.05.00.04.00
;05.01.05.01.05.03.02.00.02.01.05.01.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.02
;05.00.05.02.05.03.02.00.02.01.05.02.05.00.05.03.09.00.05.01.02.00.05.00.05.01.04.00
;05.02.05.02.05.03.02.00.02.01.05.03.05.00.05.03.09.00.05.01.02.00.05.02.05.02.04.00
;05.02.05.03.05.03.02.00.02.01.05.03.05.00.05.03.09.00.05.01.02.00.05.02.05.01.04.02
;05.00.05.03.05.03.02.00.02.01.05.03.05.00.05.03.09.00.05.01.02.00.05.01.05.02.04.00
;05.01.05.01.05.03.02.00.02.01.05.01.05.00.05.03.09.02.05.01.02.00.05.02.05.00.04.00
;05.00.05.01.05.03.02.00.02.01.05.01.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.00
;05.03.05.01.05.03.02.00.02.01.05.01.05.03.05.03.09.00.05.01.02.00.05.02.05.02.04.01
;05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.00
;05.02.05.01.05.02.02.00.02.01.05.00.05.00.05.03.09.02.05.01.02.00.05.01.05.02.04.00
;05.03.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.01.04.00
;05.03.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.01
;05.03.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.03.05.01.02.00.05.02.05.00.04.00
;05.02.05.01.05.03.02.00.02.01.05.00.05.03.05.03.09.00.05.01.02.00.05.02.05.00.04.02
;05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.00.05.01.04.00
;05.01.05.01.05.01.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.02.04.00
;05.00.05.01.05.03.02.00.02.01.05.03.05.00.05.03.09.00.05.01.02.00.05.02.05.01.04.02
;05.02.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.01.05.02.04.00
;05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.00
;05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.00
;05.02.05.01.05.01.02.00.02.01.05.00.05.03.05.03.09.00.05.01.02.00.05.02.05.02.04.01
;05.00.05.01.05.03.02.00.02.01.05.03.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.00
;05.03.05.01.05.01.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.01.05.02.04.00
;05.02.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.01.04.00
;05.03.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.01
;05.00.05.01.05.03.02.02.02.01.05.02.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.00
;05.03.05.01.05.03.02.02.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.02
;05.00.05.01.05.03.02.02.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.00.05.01.04.00
;05.03.05.01.05.03.02.03.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.02.04.00
;05.00.05.01.05.03.02.03.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.01.04.02
;05.00.05.01.05.03.02.03.02.01.05.01.05.03.05.03.09.00.05.01.02.00.05.01.05.02.04.00
;05.00.05.01.05.03.02.03.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.00
;05.00.05.01.05.03.02.03.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.00
;05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.02.04.01
;05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.00
;05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.01.05.02.04.00
;05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.01.04.00
;05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.01
;05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.00
;05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.02
;05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.00.05.01.04.00
;05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.02.04.00
;05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.01.04.02
;05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.01.05.02.04.00
;05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.00
;05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.00
;05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.02.04.01
;05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.00
;05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.01.05.02.04.00
;05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.01.04.00
;05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.01
;05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.00
;05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.02
;05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.00.05.01.04.00
;05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.02.04.00
;05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.01.04.02
;05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.01.05.02.04.00
;05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.00
;05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.00
;05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.02.04.01
;05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.00.04.00
;05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.01.05.02.04.00
;05.00.05.01.05.03.02.00.02.01.05.00.05.00.05.03.09.00.05.01.02.00.05.02.05.01.04.00
