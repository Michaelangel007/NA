;************************INCLUDE FILE*****************************
;(do not assemble stand-alone.)

;=====================MISC.MAIN_MEMORY_ONLY.ASM DOCUMENTATION====================================
;
;These subroutines must be located in main memory to ensure they are accessible to the routines that call
;them. For example, if a routine making calls is in AUX_BSR then the called subroutine must be either in AUX BSR,
;zero page, or main memory.
;
;=================================================================================

;****;used by AUX_BSR routine DARKNESS.REVIEW.AUX_BSR.ENTRANCE**
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

;****;used by AUX_BSR routine DARKNESS.REVIEW.AUX_BSR.ENTRANCE**
DRAW.TILE.SINGLE ;	;=============DRAW A SINGLE TILE AT THE SPECIFIED SCREEN LOCATION=====
@START
;PARAMETERS: Y-REG (SCREEN POSITION AS MAPPED TO ELEMENT OF SCREEN.TILE.DATA), SAVED_TILE_TYPE (use if you want to draw a tile that isn't stored in SCREEN.TILE.DATA for the specified Y-REG location), [PAGE.FOREGROUND.OVERRIDE], [DRAW.TILE.SHAPE_TABLE.OVERRIDE*]
;RETURN: NONE
;ENTRANCE: DIRECT
;
;*DRAW.TILE.SHAPE_TABLE.OVERRIDE ($00 = load data into shape table from aux memory | $01 don't load data into shape table)

;=====================SUBROUTINE DOCUMENTATION====================================
;This subroutine is designed for direct entrance. It setups up all the graphics screen variables required to draw a single tile and
;it copies the shape table for the Tile_ID (associated with the screen tile location passed via Y-REG) from auxiliary memory to a main memory buffer for use by DRAW.TILE
;
;there is a branch test in .START for an alternate entrance (.ALTERNATE_ENTRANCE) which is used if this routine was called for the purpose
;of drawing the player walking icon (/aka standard icon). In this event special treatment is needed because the shape table is copied from a special player icon buffer instead of from auxiliary memory.  
;=================================================================================



			
			
;VERIFY TILE IS ON THE VIEW SCREEN
	STY SAVED.YREG.LOCAL ;save parm: screen index of tile location		
	
	CPY #SCREEN.ARRAY.LAST_ELEMENT2
	BCC .START
	JMP ERROR

.START	
	LDA CALLED_BY.DRAW.TILE.PLAYER.STANDARD_ICON
	CMP #$01
	BNE .STANDARD.ENTRANCE
	JMP .ALTERNATE_ENTRANCE								;IF THIS ROUTINE WAS CALLED BY .STANDARD_ICON IN DRAW.TILE.PLAYER, THE SHAPE TABLE FOR PLAYER ICON IS COPIED FROM A BUFFER

.STANDARD.ENTRANCE	
;PREPARE SHAPE TABLE

	;connect shape hopper
	LDA #SHAPE.HOPPER0				;SAVE SHAPE.HOPPER0 AS THE DESTINATION ADDRESS FOR AUX MOVE
	STA AUX_MOVE.DEST
	STA SHAPE						;CONNECTS SHAPE.HOPPER0 TO SHAPE, USED BY DRAW.TILE
	LDA /SHAPE.HOPPER0
	STA AUX_MOVE.DEST+$1
	STA SHAPE+$1

			
	;load data into shape table?
	;(this branch is used by callin routines which load data into the shape table hopper themselves)
	LDA DRAW.TILE.SHAPE_TABLE.OVERRIDE ;($00 = load data into shape table from aux memory | $01 don't load data into shape table)
	; BEQ .COW	
	; JMP .ALTERNATE_ENTRANCE2
	BNE .ALTERNATE_ENTRANCE2
	;JMP COW.TEST
;.COW
			
.CALCULATE.SHAPE_TABLE.ADDRESS	
	LDA SAVED_TILE_TYPE ;*OPT** Memory. Speed. Set this up as an ACC parameter, then save it to this variable as the 1st line of code in this routine. Then all the STA's to set this variable by calling routines can be removed. 				;IF A TILE_TYPE IS PRESENT IN THIS VARIABLE, USE IT.
	BNE .REENTRY					;IF NOT, USE SCREEN.TILE.DATA AS THE SOURCE FOR TILE_TYPE
	;load tile_type of the next tile in current row
	LDA SCREEN.TILE.DATA,Y					
.REENTRY
	STY SAVED.YREG.LOCAL					;SAVING YREG TO A VARIABLE ENABLES US TO AVOID USING X-REG IN THIS ROUTINE, WHICH ENABLES THIS ROUTINE TO FALL THROUGH TO DRAW.TILE INSTEAD SAVING X-REG TO THE STACK AND USING JSR TO DRAW.TILE 

		
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
.CONNECT.SHAPE.HOPPER

	; ====THIS SECTION MOVED ABOVE===
	; LDA #SHAPE.HOPPER0				;SAVE SHAPE.HOPPER0 AS THE DESTINATION ADDRESS FOR AUX MOVE
	; STA AUX_MOVE.DEST
	; STA SHAPE						;CONNECTS SHAPE.HOPPER0 TO SHAPE, USED BY DRAW.TILE
	; LDA /SHAPE.HOPPER0
	; STA AUX_MOVE.DEST+$1
	; STA SHAPE+$1			

	CLC								;EXECUTE AUX MEMORY MOVE
	JSR AUX_MOVE

			
		
.ALTERNATE_ENTRANCE2
	LDY SAVED.YREG.LOCAL		

.ALTERNATE_ENTRANCE		
	LDA #$00						
	STA SAVED_TILE_TYPE				;RESET TO $00 SO NEXT CALL TO THIS ROUTINE WILL USE DEFAULT TILE SOURCE (SCREEN.TILE.DATA) UNLESS SAVED.TILE_TYPE IS EXPRESSLY SET AGAIN. 

	LDA SCREEN.INDEX.TILE_SBYTE,Y
	STA	SCREEN.DRAW.CURRENT_BYTE
	
	LDA SCREEN.INDEX.TILE_LINE,Y
	STA TILE.LINE.START
	STA TILE.LINE


			
	;****FALLS THROUGH*****
@END

;**FALLS THROUGH**

;****;used by AUX_BSR routine DARKNESS.REVIEW.AUX_BSR.ENTRANCE**
DRAW.TILE ;			;=============DRAW A SINGLE TILE AT THE LOCATION SPECIFIED======
@START
;PARAMETERS:  TILE.LINE, TILE.LINE.START, *TILE.DEPTH, SCREEN.DRAW.CURRENT_BYTE, [PAGE.FOREGROUND.OVERRIDE]
;RETURN: NONE
;ENTRANCE: DRAW.COLUMN, DRAW.ROW, DRAW.TILE.SINGLE
;*Set to default value on game launch. When modified (i.e. tall grass), it is reset to default when done.

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

			;**OPT** Memory. Speed. It seems like TILE.LINE.START could be set from TILE.LINE, not passed as a parm but I haven't looked into it closely 
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
	STA DRAW.TILE.SHAPE_TABLE.OVERRIDE ;($00 = load data into shape table from aux memory | $01 don't load data into shape table)
			
			
			
.RESTORE.REGISTERS
	PLA
	TAY
	PLA
	TAX
	
	RTS

ERROR
;ERROR REPORTED BY DRAW.TILE.SINGLE Y-REG VALUE > LAST ELEMENT OF SCREEN.TILE.DATA
;ACC CONTAINS THE VALUE OF CALLED_BY


	LDA CALLED_BY
	JSR PREP.BRK
	BRK	
@END	


	