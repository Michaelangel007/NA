; ============================================
; Copyright (C) 2016-2018. 6502 Workshop, LLC
; contact: mark@6502workshop.com
; ============================================



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



;**OPT** Memory. See if there is a way to merge the shape table address/copy part of this routine with DRAW.TILE.TERRAIN_ENTRANCE, and
;then probably put DRAW.TILE.TERRAIN_ENTRANCE just above DRAW.TILE			
			
;VERIFY TILE IS ON THE VIEW SCREEN
	STY SAVED.YREG.LOCAL ;save parm: screen index of tile location		
	
	CPY #SCREEN.ARRAY.LAST_ELEMENT2
	BCC .START
	JMP ERROR

.START		
	;set DRAW.TILE parameters
	LDA SCREEN.INDEX.TILE_SBYTE,Y
	STA	SCREEN.DRAW.CURRENT_BYTE
	
	LDA SCREEN.INDEX.TILE_LINE,Y
	STA TILE.LINE.START
	STA TILE.LINE
		
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
	BNE .ALTERNATE_ENTRANCE2


			
.CALCULATE.SHAPE_TABLE.ADDRESS	
	LDA SAVED_TILE_TYPE 			;*OPT** Memory. Speed. Set this up as an ACC parameter, then save it to this variable as the 1st line of code in this routine. Then all the STA's to set this variable by calling routines can be removed. 				;IF A TILE_TYPE IS PRESENT IN THIS VARIABLE, USE IT.
	BNE .REENTRY					;IF NOT, USE SCREEN.TILE.DATA AS THE SOURCE FOR TILE_TYPE
	;load tile_type of the next tile in current row
	LDA SCREEN.TILE.DATA,Y					
.REENTRY	
	;set parms for DRAW.TILE.TERRAIN_ENTRANCE
		PHA ;save tile_type to draw
		LDA #$00						
		STA SAVED_TILE_TYPE				;RESET TO $00 SO NEXT CALL TO THIS ROUTINE WILL USE DEFAULT TILE SOURCE (SCREEN.TILE.DATA) UNLESS SAVED.TILE_TYPE IS EXPRESSLY SET AGAIN. 
		PLA ;restore tile_type to draw
						
		;ACC = TILE_ID
	JMP DRAW.TILE.TERRAIN_ENTRANCE

	
.ALTERNATE_ENTRANCE2
	LDY SAVED.YREG.LOCAL		

.ALTERNATE_ENTRANCE		
	LDA #$00						
	STA SAVED_TILE_TYPE				;RESET TO $00 SO NEXT CALL TO THIS ROUTINE WILL USE DEFAULT TILE SOURCE (SCREEN.TILE.DATA) UNLESS SAVED.TILE_TYPE IS EXPRESSLY SET AGAIN. 

			
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



			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			
			; INC HTAB
			; JSR UPDATE.CHAR.POS
			
				; LDA #$B0
			; JSR COUT
			
			; LDA #$00
			; STA TROUBLESHOOTING.HOOK
			
; .TEMP
			; LDA TEMP



			
	RTS

ERROR
;ERROR REPORTED BY DRAW.TILE.SINGLE Y-REG VALUE > LAST ELEMENT OF SCREEN.TILE.DATA
;ACC CONTAINS THE VALUE OF CALLED_BY


	LDA CALLED_BY
	JSR PREP.BRK
	BRK	
@END	



DRAW.TILE.TERRAIN_ENTRANCE
@START
;PARAMETERS: ACC = Tile_ID, TILE.LINE, TILE.LINE.START, SCREEN.DRAW.CURRENT_BYTE
;ENTRANCE: DRAW.ROW, DRAW.COLUMN
;note: this routine contains the common code to calculate and copy the shape table when drawing a terrain tile.


;**OPT** Memory. Use 16 bit multiplication to calculate shape table address by tile type. Saves the 2 pages of memory used by TILE.SHAPES.HO/LO. Applies to DRAW.COLUMN, DRAW.ROW, DRAW.TILE.SINGLE, DRAW.TILE.FOREGROUND.SINGLE, ANIMATION.UPDATE

;**OPT** Speed. The tile shape table gets copied into the hopper. Maybe there are two hoppers. Current and last. Draw.tile continues to use a zero page variable (shape) and if current tile == last tile, then point (shape) to tile.hopper.last There is an extra cost in draw.tile of the indirect lookup to (shape) instead of a indexed lookup directly to a single hopper, but I bet the gains offset this. Avoiding copying a tile shape table is avoiding a 32 iteration loop * X clock cycles. Almost every region will probably have long strings of tiles....grass, forest, water, mountains, often appear in clusters. Applies to DRAW.COLUMN, DRAW.ROW, DRAW.TILE.SINGLE, DRAW.TILE.FOREGROUND.SINGLE, ANIMATION.UPDATE. Gains likely especially large in animation update, as water tile drawing is lots of repeat tiles. 

;**Opt** Speed. skip draw if animated tile ID or if MO present


	;ACC = Tile_ID


.DAY_NIGHT.TILE.SWAP
;VALIDATE ENTRANCE

	PHA ;save Tile_ID
	
			
	
	
	;is a building map active
	;**OPT** Memory. Speed. Right now there is only one code for location type, which is used to determine the tile_set to use and for other location-type specific features. There are a lot of the later. It might be a savings if the 
					;there were a tile-set code separate from location type code, and there was a default tile-set picked using the location-type code if tile-set code was set to default. This might require adding another byte to some hex tables
					;but I suspect it would pay off. 
	LDA PLAYER.MAP.LOCATION_TYPE
	CMP #MAP.TYPE.BUILDING.GRE 	;is map type = building?
	BCC .VALIDATE_ENTRANCE.DONE		;if no
	CMP #MAP.TYPE.BUILDING.LT	;is map type = building?
	BCS .VALIDATE_ENTRANCE.DONE			;if no
	;if yes	

	;is it night? (the map data always has the day tile)
	LDA TIME.SUN.STATUS ;($00 = SUN RISING, $01 = DAY, $02 = SUN SETTING, $03 = NIGHT)
	CMP #$02
	BCC .VALIDATE_ENTRANCE.DONE

	
	;is Tile_ID is the day/night swap range	
	PLA ;restore Tile_ID
	CMP #TILE_ID.DAY_NIGHT.SWAP.EQ1
	BEQ .IS.SWAP_TILE	
	CMP #TILE_ID.DAY_NIGHT.SWAP.GRE1
	BCC .NOT.SWAP_TILE	
	CMP #TILE_ID.DAY_NIGHT.SWAP.LT1
	BCS .NOT.SWAP_TILE	
	
	;**FALLS THROUGH**
	
.IS.SWAP_TILE	
	CLC
	ADC #$01 ;tile_ID = tile_ID +1
	
	;**FALLS THROUGH**
	
.NOT.SWAP_TILE	
	PHA		;save Tile_ID to keep stack in sync. 
	
.VALIDATE_ENTRANCE.DONE	

	PLA ;restore Tile_ID


	
	

; .CHECK.DARKNESS	
; ;IS TILE HIDDEN (DARKNESS)?	
	; LDX SCREEN.DARK.DATA,Y			
	; CPX #$01
	; BEQ .SKIPDRAW.STEP				;IF YES, DON'T DRAW TILE
	; JMP .CALC.SHAPE.TABLE
; .SKIPDRAW.STEP
	; JMP .SKIPDRAW
	
.CALC.SHAPE.TABLE	;**OPT** Memory. The calculate and copy shape table code is probably the same as in DRAW.COLUMN. Maybe it could be setup as a common routine just above DRAW.TILE that falls through to it or does a JMP. 
;CALCULATE SHAPE TABLE ADDRESS

			STX COW

	
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
		
;=======INLINE CODE FOR ADC.16========		;**OPT** Memory. the section above and below can be replaced with a simple 16-bit add
;AUX_MOVE.START(2)+ ACC(1) [ANIMATION FRAME OFFSET]


; DO THE MATH
	CLD 
    CLC                          ;ALWAYS BEFORE ADD
    LDA OP1
    ADC OP2
    STA AUX_MOVE.START
		 
    LDA OP1+$1
    ADC OP2+$1
    STA AUX_MOVE.START+$1
	
;======================================
	
	LDA AUX_MOVE.END
	STA OP2
	LDA AUX_MOVE.END+$1
	STA OP2+$1
	
;=======INLINE CODE FOR ADC.16========	
;AUX_MOVE.END(2)+ ACC(1) [ANIMATION FRAME OFFSET]


; DO THE MATH 
	CLD 		;**OPT** Memory. CLD not needed here. Search for CLD in all sources files. I used to use it a lot more than needed. 
    CLC                          ;ALWAYS BEFORE ADD
    LDA OP1
    ADC OP2
    STA AUX_MOVE.END
		 
    LDA OP1+$1
    ADC OP2+$1	
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


			LDX COW
			
	JMP DRAW.TILE
@END
