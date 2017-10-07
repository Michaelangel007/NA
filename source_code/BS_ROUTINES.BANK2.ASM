; ============================================
; Copyright (C) 2016-2018. 6502 Workshop, LLC
; contact: mark@6502workshop.com
; ============================================


;************************INCLUDE FILE*****************************
;(do not assemble stand-alone.  )
				.TF     BS.ROUTINES.BANK2.bin,BIN
				.OR		$D000			**Always put before .TF directive and never use again in program
				;.TF     graphics_scrolling.bin,BIN
				.EB		OFF


	; STA $C009 ;enable aux zero-page & aux BSR 
	; STA $C008 ;enable main zero-page & main BSR 
	
	;STA $C003 ;read enable aux ($200 - $BFFF)

	;STA $C002 ;read enable main ($200 - $BFFF)
	
TILE_SWAP.SUN ;setup in BSR because it copies between AUX memory locations
@START
;=====================SUBROUTINE DOCUMENTATION====================================
;
;A collection of low level routines used to copy data between shape tables and shape table buffers (an aux-to-aux copy)
;to faciliate the Tile-Swap: sunrise/sunset event. See documentation at the top of EVENT_MANAGER.ASM for more details
;
;=================================================================================

;TILE_SWAP.SUN.INIT.BUFFERS (aux to aux copy)
@START
	; STA $C003 ;read enable aux ($200 - $BFFF)
	; STA $C005 ;write enable aux ($200 - $BFFF)

	; LDX #$00
; .LOOP
	; LDA SHAPE.FLOOR.BRICK.ORANGE,X
	; STA SHAPE_BUFFER.FLOOR.BRICK.ORANGE,X
	
	; LDA SHAPE.STREET.BLUE,X
	; STA SHAPE_BUFFER.FLOOR.BRICK.BLUE,X
	
	; INX
	; CPX #TILE.SHAPE.SIZE
	; BNE .LOOP

	; STA $C002 ;read enable main ($200 - $BFFF)
	; STA $C004 ;write enable main ($200 - $BFFF)

	; RTS
@END

TILE_SWAP.SUN.SET_DAY
@START

;DEBUG: screen goes crazy, town displays with streets in solid white and game hands. The page should not
;have fliped yet. The hook below doesn't triger

	
	STA $C003 ;read enable aux ($200 - $BFFF)
	STA $C005 ;write enable aux ($200 - $BFFF)

	LDX #$00
.LOOP
	LDA SHAPE_BUFFER.FLOOR.BRICK.ORANGE,X	
	STA SHAPE.FLOOR.BRICK,X
	STA SHAPE.PATH.FLOOR.BRICK,X

	LDA SHAPE_BUFFER.STREET.BRICK.ORANGE,X	
	STA SHAPE.STREET.BRICK,X

	LDA SHAPE_BUFFER.STREET.GRAVEL.ORANGE,X	
	STA SHAPE.STREET.GRAVEL,X

	INX
	CPX #TILE.SHAPE.SIZE
	BNE .LOOP

	STA $C002 ;read enable main ($200 - $BFFF)
	STA $C004 ;write enable main ($200 - $BFFF)

	RTS
@END
	
TILE_SWAP.SUN.SET_NIGHT
@START
	STA $C003 ;read enable aux ($200 - $BFFF)
	STA $C005 ;write enable aux ($200 - $BFFF)

	LDX #$00
.LOOP
	LDA SHAPE_BUFFER.FLOOR.BRICK.BLUE,X	
	STA SHAPE.FLOOR.BRICK,X
	STA SHAPE.PATH.FLOOR.BRICK,X

	LDA SHAPE_BUFFER.STREET.BRICK.BLUE,X	
	STA SHAPE.STREET.BRICK,X

	LDA SHAPE_BUFFER.STREET.GRAVEL.BLUE,X	
	STA SHAPE.STREET.GRAVEL,X
	
	INX
	CPX #TILE.SHAPE.SIZE
	BNE .LOOP
	
	STA $C002 ;read enable main ($200 - $BFFF)
	STA $C004 ;write enable main ($200 - $BFFF)

	RTS
@END
	

				
; ANIMATION.UPDATE
	; RTS 
@END

				
				.IN 	c:\my_code\na\source_code\map_object_management

				.IN 	c:\my_code\na\source_code\graphics_engine ;originally in lower main RTS, tried moving to BSR2 (had problem with darkness), then moved to main memory to troubleshoot it. 
				
				.IN 	c:\my_code\na\source_code\movement_manager
				.IN 	c:\my_code\na\source_code\tile_functions2
				
	.NO $FFFF,$AB	;FILLER TO THE UPPER LIMIT OF BANK SWITCHED MEMORY. 
					;This enables SBASM to generate an error if/when the code overshoots this memory address.
					;It also kees this target file at a fixed size so the track/sector stays the same on the disk image, which is needed to load the file via MY.RWTS
					