; ============================================
; Copyright (C) 2016-2018. 6502 Workshop, LLC
; contact: mark@6502workshop.com
; ============================================


;************************INCLUDE FILE*****************************
;(do not assemble stand-alone.  )


MOVE.PASS
@START

;=====================SUBROUTINE DOCUMENTATION====================================
;This subroutine primarily runs an update of map objects, the effect of which is to 
;process any mob moves, and then copies the foreground page to the background page (COPY.SCREEN)
;in preperation for the animation routine which will run after returning to the game loop.
;
;The player icon is also drawn. This becuase this routine is called after the player boards or exits
;a transport map object, both of which change the player icon. 
;
;The code to update the player map position may not be needed, since the player's position 
;doesn't change with a pass. It was left in to add confusion. Just kidding. It was left in 
;just in case there were circumstances where SMAP and SMAP.CURRENT were not in sync with RMAP, which 
;could be tough bugs to find. Most likely it's unnecessary though. 
;=================================================================================


		
		
	;**OPT** Speed memory. Maybe SMAP can get dervied from RMAP on boot. Then SMAP gets incremented along with RMAP in this routine, which for east and west is an INC/DEC rather than 16-BIT addition/subtraction. SMAP CURRENT would just get set to SMAP when it needed to be reset. 
.UPDATE.PLAYER.MAP_POSITION
	;SAVE PLAYER GMAP.X/Y BEFORE CURRENT MOVE
	LDA GMAP.X
	STA GMAP.X.LAST
	
	LDA GMAP.Y
	STA GMAP.Y.LAST
	
	
	LDA #$04
	STA PLAYER.MOVE.CURRENT			;PASS
	
	;MAKE SURE SMAP/SMAP.CURRENT ARE UP TO DATE 
	LDA RMAP					;LOAD TILE_ID OF CURRENT MAP POSITION (CENTER/PLAYER TILE)
	STA OP1
	LDA RMAP+$1
	STA OP1+$1
	
	LDA #OFFSET.SCREEN			;CALCULATE TILE_ID OF TILE IN UPPER LEFT CORNER OF SCREEN
	STA OP2
	LDA /OFFSET.SCREEN
	STA OP2+$1
	
	JSR SBC.16					;RMAP(2) - SCREEN.OFFSET.LO/HO (2)
			
	LDA RESULT					;SAVE TILE_ID OF UPPER LEFT SCREEN TILE
	STA SMAP
	STA SMAP.CURRENT
	LDA RESULT+$1
	STA SMAP+$1
	STA SMAP.CURRENT+$1
		
.NOFLIP

;INIT DARKNESS ARRAYS
	LDY #$00
.LOOP
	LDA SCREEN.DARK.DATA,Y
	STA SCREEN.DARK.DATA_BEFORE,Y
	LDA #$00
	STA SCREEN.DARK.DATA,Y
	
	CPY #$BA
	BEQ .DONE
	INY
	JMP .LOOP
.DONE

.UPDATE.VIDEO.SCREEN
@START
	;!!!!! BSR BANK1 !!!!
	JSR COPY.SCREEN.ENTRANCE ;**DON'T REMOVE** (or the screen goes chaotic sometimes when collisions occur with double mover mobs. MO.DRAW must need the graphics pages in sync for some reason). UDPATE: I think this is needed because sometimes, due to key press abort, no copy.screen was done after the last move, so the pages are out of syncing going into MOVE.PASS, which is triggered by non-movement commands like (3)ZAP, Push, Board, etc.


		
	JSR DARKNESS.REVIEW				;update the hidden (darkness) tiles on the screen based on the tile_type values in SCREEN.TILE.DATA 				

			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; ; LDA *
			; ; LDA *-2
			; ; LDX *-1			;PROGRAM COUNTER AT BRK+$2 WILL BE RETURNED IN X-REG/Y-REG
			; ; TAY
			; LDA #$AA
			; LDX SAVED_TILE_TYPE				
			; JSR FULL.BRK
; .TEMP
			; LDA TEMP

			
	JSR MO.DRAW						;should be after player icon on screen from last move is replaced

		
		LDA #$02					;set trace
		STA CALLED_BY
	JSR DRAW.TILE.PLAYER
		LDA #$00					;reset trace
		STA CALLED_BY

			; JSR FLIP.PAGE
			; JSR KEYIN
			; LDA #$AB
			; JSR FULL.BRK
			; BRK

		
	JSR FLIP.PAGE
	
	JSR TIME.UPDATE.MOVE			;increase game clock by 1 minute


@END

		
.EXIT
	LDA #$01
	STA ANIMATION.FORCED
	 
;SKIP ANIMATION ABORT CHECK?	
	LDY #SCREEN.ARRAY.PLAYER_LOCATION
	LDA SCREEN.TILE.DATA,Y
	CMP #TILE_ID.QUICKSAND				;IS PLAYER STANDING IN QUICKSAND?
	BNE .NOT_QUICKSAND					;IF NO, CONTINUE WITH WATER CHECK WHICH ALSO RESETS PLAYER HEIGHT TO DEFAULT
	LDA #$01
	STA ANIMATION.FORCED				;SET FORCED ANIMATION FLAG SO ANIMATION KEY PRESS ABORT CHECK WILL BE SKIPPED. 
.NOT_QUICKSAND	
	;**FALLS THROUGH

	LDA ANIMATION.FORCED.OVERRIDE
	CMP #$01						;IS ANIMATION FORCED OVERRIDE FLAG SET?
	BEQ .KEYCHECK					;IF YES, FORCE THE ABORT KEY PRESS CHECK
	LDA ANIMATION.FORCED
	CMP #$01						;IS ANIMATION FORCED FLAG SET?
	BEQ .EXIT.SKIP.KEYCHECK			;IF YES, DON'T ALLOW AN ABORT DUE TO KEY PRESS		
.KEYCHECK
	LDA KB_BUFFER					;LOAD NEXT KEY PRESS FROM BUFFER. WAS KEY PRESSED?
    BMI .EXIT.ALTERNATE				;IF YES, SKIP COPY.SCREEN WHICH IS PREP FOR ANIMATION. 
.EXIT.SKIP.KEYCHECK

	;**OPT. Memory. Speed. This copy screen may not be needed. There is a copy screen further up in MOVE.PASS
	
	;!!!!! BSR BANK1 !!!!
	JSR COPY.SCREEN.ENTRANCE ;**DON'T REMOVE** (or the screen goes chaotic sometimes when collisions occur with double mover mobs. MO.DRAW must need the graphics pages in sync for some reason). UDPATE: I think this is needed because sometimes, due to key press abort, no copy.screen was done after the last move, so the pages are out of syncing going into MOVE.PASS, which is triggered by non-movement commands like (3)ZAP, Push, Board, etc.

		
.EXIT.ALTERNATE
; ;DID MOB INITIATE COMBAT?	
	; LDA COMBAT_SE.MODE.PARM	;($00 = player initiated or init value in map objects manager | $01 = mob initiated | $FF = test mode)
	; BEQ .MOB.INITIATED.COMBAT.CHECK.DONE ;if yes, branch
	; JSR MOVE_MGR.INITIATE.COMBAT
; .MOB.INITIATED.COMBAT.CHECK.DONE
			
	RTS	
@END
		
MOVE.NORTH
@START	

;=====================SUBROUTINE DOCUMENTATION====================================
;This subtroutine is called by GAME.PRIMARY_LOOP when the player presses the key for move northward movement. 
;I bet this is a shocking surprise. You probably thought this subroutine was used for changing the players underarmor.
;I should have picked a clearer name for it. 
;
;Like it's south/east/west counterparts, this routine is responsible for:
;			a) collision checks (is a move in this direction blocked by an impassible tile?)
;			b) updating the player's map position if the move isn't blocked
;			c) determining if a zone transition is required (has the player moved 16 tiles from the center of the regional map?)
;			d) updating zone information (see below for details)
;			e) setup a new graphics screen on the background page to 
;			   present to the player by flipping the page to the 
;			   foreground once all updates are made. (see below for details)
;			f) Check to see if the player, after the current move, is 
;			   located on a Tile_ID with special characteristics (see MOVE.COMMON.ROUTINES for details)
;
;--Updating Zone Information (details)--
;In addition to things self explanatory in the code comments, there are
;some flags that need to be updated "MOB (SS) REGIONAL ZONE FLAGS" whose
;purpose may not be obvious. Here is the story on these flags:
;SS MOBS only move when offscreen if they are located on the regional 
;map. Since MOB's locations are tracked relative to the player, and the
;player's location on the regional map changes, the flags need to be updated
;each time the player moves. 
;
;--Setup new Graphics Screen (details)--
;Each time the player moves, a new graphics screen is setup by scrolling the tiles on the existing screen and drawing a new column or row of tiles
;to replace the tiles that scrolled off the screen. After this is done hidden tiles (darkness) is recalculated because even though previously dark tiles
;were scrolled, the change in the players location on the map could result in different tiles being visible and hidden than were so in the players previous location. 	
;Map objects must also be reviewed (i.e Mobs, Transport), which is done in MOVE.COMMON.ROUTINES which takes control at the end of all movement routines. 
;Objects that were previously at a location not on the view screen may be located on the view screen after the players move,
;and objects on the view screen prior to the playeres move might not be on the view screen after the players move. 
;Additionally, the game needs to consider if Mobs have moved (both on screen and offscreen) and update their x/y position and draw them in the correct screen tile location if they are on-screen.
;The old player icon erased (by drawing the underlying terrain in that location). This is because
;the player icon is scrolled by the graphics scolling routines and the player icon is always at the center of the screen. 
;
;=================================================================================


	LDA PLAYER.MOVE.FORCED			;SET TO $01 BY NON-KEYPRESS ROUTINES TO MOVE PLAYER.
	CMP #$01						;IS MOVE FORCED FLAG SET?
	BEQ .NOT_BLOCKED				;IF YES THEN SKIP COLLISION CHECKS.
	
;COLLISSION CHECK
	LDA #SCREEN.ARRAY.ADJACENT_NORTH	;NORTH 1 TILE	
	STA PLAYER.MOVE.CANDIDATE_TILE_LOC	
	LDA #SCREEN.ARRAY.ADJACENT_NORTH2	;NORTH 2 TILES
	STA PLAYER.MOVE.CANDIDATE_TILE_LOC2	
	JSR PLAYER.COLLISSION.CHECK
	CMP #$00							;RETURN VALUE OF $00 = MOVE PERMITTED, $01 = BLOCKED
	BEQ .NOT_BLOCKED
	JMP BLOCKED			
		
.NOT_BLOCKED
	LDA #$00
	STA PLAYER.MOVE.CURRENT				;NORTH
	

;WORLD MAP EDGE CHECK
	LDA PLAYER.MAP.LOCATION_TYPE		;building maps are except because a zone transition triggers exit back to prior map
	; CMP #MAP.TYPE.TOWN_VILLAGE
	; BEQ .UPDATE.ZONE.INFORMATION
	; CMP #MAP.TYPE.CASTLE
	; BEQ .UPDATE.ZONE.INFORMATION
	
	;is a building map active
	;**OPT** Memory. Speed. Right now there is only one code for location type, which is used to determine the tile_set to use and for other location-type specific features. There are a lot of the later. It might be a savings if the 
					;there were a tile-set code separate from location type code, and there was a default tile-set picked using the location-type code if tile-set code was set to default. This might require adding another byte to some hex tables
					;but I suspect it would pay off. 
	CMP #MAP.TYPE.BUILDING.GRE 	;is map type = building?
	BCC .MAP.TYPE_CHECK.DONE		;if no
	CMP #MAP.TYPE.BUILDING.LT	;is map type = building?
	BCS .MAP.TYPE_CHECK.DONE		;if no
	JMP .UPDATE.ZONE.INFORMATION	;if yes
.MAP.TYPE_CHECK.DONE
	
	LDA GMAP.Y
	CMP #WORLD.EDGE.NORTH
	BNE .UPDATE.ZONE.INFORMATION
	JMP BLOCKED							;for now walking off the map edge produces the same result as walking into a mountain or other impassible terrain.

.UPDATE.ZONE.INFORMATION
@START	

	
;MANAGE ZONE TRANSITIONS	
	DEC RMAP.Y
	LDA RMAP.Y
	CMP #RZONE.LOAD_THREASHOLD.NORTH	;HAS LOAD THREASHOLD BEEN REACHED?
	BNE .NO_LOAD

	JSR MAP.EXIT.CHECK					;IS PLAYER IN A BUILDING? IF SO, TRANSITION ZONES OR EXIT BUILDING?		

.ZONE.TRANSITION	
	JSR ZONE_TOOLS.TRANSITION.NORTH	
	
;CENTER RMAP IN REGIONAL MAP START POSITION 
;-------
;ADC 16-BIT 
	LDA RMAP
	CLC
	ADC #RZONE.LOAD.OFFSET.UP
	STA RMAP
	LDA RMAP+$1
	ADC /RZONE.LOAD.OFFSET.UP
	STA RMAP+$1
	;**OPT** Speed. Memory. Since RMAP is being reset to the same position each time, this could be done via constants instead of ADC. 
;-------
;INCREMENT PLAYER ZONE
	LDA PLAYER.WMAP.ZONE
	SEC
	SBC #WZONE.OFFSET
	STA PLAYER.WMAP.ZONE

;RESET PLAYER RMAP Y-AXIS
	LDA #RZONE.LOAD.Y.START
	STA RMAP.Y

;RESET MOB (SS) REGIONAL ZONE FLAGS	
	LDA #MAP_OBJECTS.SS.Y_FLAG.LOWER.START		;RESET FLAGS USED TO DETERMINE IF AN SS IS LOCATED ON THE REGIONAL MAP. IT CHANGES WITH EACH PLAYER MOVE BECAUSE MOB'S X,Y IS TRACKED RELATIVE TO THE PLAYER
	STA MAP_OBJECTS.SS.Y_FLAG.LOWER
	
	LDA #MAP_OBJECTS.SS.Y_FLAG.UPPER.START
	STA MAP_OBJECTS.SS.Y_FLAG.UPPER
	
	INC MAP_OBJECTS.SS.Y_FLAG.LOWER				;THIS IS TO OFFSET THE NORMAL MORE ADJUSTMENT, SO THAT THE FLAGS ARE IN THE START POSITION NET OF PLAYER MOVE
	DEC MAP_OBJECTS.SS.Y_FLAG.UPPER		

.NO_LOAD

@END	

.UPDATE.PLAYER.MAP_POSITION
@START
	;SAVE PLAYER GMAP.X/Y BEFORE CURRENT MOVE
	LDA GMAP.X
	STA GMAP.X.LAST
	
	LDA GMAP.Y
	STA GMAP.Y.LAST
	
	;ADJUST POSITION
	DEC GMAP.Y					;LATITUE -1 POSITION NORTH (UP)

	LDA RMAP					;UPDATE POSITION IN REGIONAL MAP ARRAY
	STA OP1
	LDA RMAP+$1
	STA OP1+$1
	LDA #OFFSET.UP
	STA OP2
	LDA #$00
	STA OP2+$1
	
	JSR SBC.16					;RMAP(2) - #OFFSET.UP(1)
	
	LDA RESULT
	STA RMAP
	LDA RESULT+$1
	STA RMAP+$1
			;**OPT** Memory. Speed. Eliminate the two LDAs of RMAP below by above saving RESULT TO OP1/+$1
	;UPDATE SMAP/SMAP.CURRENT
	LDA RMAP					;LOAD TILE_ID OF CURRENT MAP POSITION (CENTER/PLAYER TILE)
	STA OP1
	LDA RMAP+$1
	STA OP1+$1
	
	LDA #OFFSET.SCREEN			;CALCULATE TILE_ID OF TILE IN UPPER LEFT CORNER OF SCREEN
	STA OP2
	LDA /OFFSET.SCREEN
	STA OP2+$1
	
	JSR SBC.16					;RMAP(2) - SCREEN.OFFSET.LO/HO (2)
			
	LDA RESULT					;SAVE TILE_ID OF UPPER LEFT SCREEN TILE
	STA SMAP
	STA SMAP.CURRENT
	LDA RESULT+$1
	STA SMAP+$1
	STA SMAP.CURRENT+$1

;ADJUST MOB (SS) REGIONAL ZONE FLAGS
;**NOTE: increasing a lower flag removed 1 tile radius, decreasing a lower flag adds 1 tile radius		
	INC MAP_OBJECTS.SS.Y_FLAG.LOWER		;ADJUST FLAGS USED TO DETERMINE IF AN SS IS LOCATED ON THE REGIONAL MAP. IT CHANGES WITH EACH PLAYER MOVE BECAUSE MOB'S X,Y IS TRACKED RELATIVE TO THE PLAYER
	INC MAP_OBJECTS.SS.Y_FLAG.UPPER

	; DEC MAP_OBJECTS.SS.Y_FLAG.LOWER		;ADJUST FLAGS USED TO DETERMINE IF AN SS IS LOCATED ON THE REGIONAL MAP. IT CHANGES WITH EACH PLAYER MOVE BECAUSE MOB'S X,Y IS TRACKED RELATIVE TO THE PLAYER
	; INC MAP_OBJECTS.SS.Y_FLAG.UPPER
	
	;FUTURE: LOGIC FOR REACHING MAP EDGE...EITHER STOP PLAYER FROM DOING SO, TRIGGER AN EVENT OR IMPLEMENT 
	;LOGIC FOR A ROUND WORLD.
	;
	;IDEA: fill the edges of the map with at least 1/2 screen width of tiles of the same type as are on the
	;		opposite edge. then it doesn't really matter if the array calcualtions are skewed a bit.
	;		It seams like this is what U4/U5 did, but then again, it seems like seas creature and pirate ship
	;		movement would get thrown off.
	
.NOFLIP

;IF PLAYER TRANSPORT ACTIVE, UPDATE X,Y OF THE TRANSPORT MO RECORD 
;(this is so the tranport MO moves with the player)

	LDX PLAYER.TRANSPORT.ACTIVE					;LOAD TRANSPORT MO RECORD INDEX
	CPX #$FF									;IS PLAYER WALKING?
	BEQ .UPDATE.VIDEO_SCREEN					;IF YES, DON'T MODIFY ANY MO RECORDS, PROCEED TO UPDATE VIDEO SCREEN
	DEC MAP_OBJECTS.GENERAL+$1,X				;IF NO, ADJUST PLAYER TRANSPORT MO X-AXIS TO REFLECT PLAYER MOVE

	LDY #SCREEN.ARRAY.PLAYER_LOCATION			;LOAD PLAYER TILE AS INDEX	 (SCREEN ARRAYS HAVEN'T BEEN SCROLLED YET FOR CURRENT PLAYER MOVE)
	LDA #$FF									;THIS IS THE VALUE FOR NO MO RECORD 
	STA SCREEN.MO_GENERAL.DATA,Y				;ERASE TRANSPORT FROM POSITION, BEFORE CURRENT PLAYER MOVE, IN SCREEN ARRAY
@END
	
.UPDATE.VIDEO_SCREEN	
@START

		;ENABLE BANK-SWITCHED RAM ($D000 Bank 1)
		LDA $C08B					;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK1, 1ST)
		LDA $C08B	
	JSR SCROLL.ROWS	
		;ENABLE BANK-SWITCHED RAM ($D000 Bank 1)
		LDA $C083				;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK2 2nd)
		LDA $C083


		LDA #$03					;SET TRACE
		STA CALLED_BY	
		LDX #$00
	JSR DRAW.ROW.SINGLE
			
	JSR DARKNESS.REVIEW				;UPDATE THE HIDDEN (DARKNESS) TILES ON THE SCREEN BASED ON THE TILE_TYPE VALUES IN SCREEN.TILE.DATA 

;ERASE OLD PLAYER ICON 
	LDY #SCREEN.ARRAY.ADJACENT_SOUTH	;REMOVE PLAYER ICON FROM LAST MOVE, WHICH GOT SCROLLED
	JSR DRAW.TILE.SINGLE

	;DO ERASE FOR MULTI-TILE TRANSPORT FOOTPRINT?
	LDA PLAYER.TILE.ACTIVE
	CMP #TILE_ID.FRIGATE1.1				;IS FRIGATE THE ACTIVE TRANSPORT? (CURRENTLY THE ONLY MULTI-TILE TRANSPORT AVAILABLE)
	BNE .EXIT

	LDY #PLAYER.MT.ADJACENT_TILES.SOUTH0 ;REMOVE PLAYER ICON FROM LAST MOVE, WHICH GOT SCROLLED. #SCREEN.ARRAY.ADJACENT_NORTH is the same as the other location for a MT player icon. Note we only remove two tiles for a multi tile icon (4 tiles) because two of the tiles in the shape will be redraw automatically with the shape is drawn in it's new location (i.e for a move south, the bottom two tiles of the shape become the upper two tiles of the shape, net of the player move) 
	JSR DRAW.TILE.SINGLE	
	LDY #PLAYER.MT.ADJACENT_TILES.SOUTH1 ;REMOVE PLAYER ICON FROM LAST MOVE, WHICH GOT SCROLLED. #SCREEN.ARRAY.ADJACENT_NORTH is the same as the other location for a MT player icon. Note we only remove two tiles for a multi tile icon (4 tiles) because two of the tiles in the shape will be redraw automatically with the shape is drawn in it's new location (i.e for a move south, the bottom two tiles of the shape become the upper two tiles of the shape, net of the player move) 
	JSR DRAW.TILE.SINGLE	
		LDA #$00						;RESET TRACE
		STA CALLED_BY
		;**OPT** Memory. Remove all instances of CALLED_BY. Instead replace with a note that to
		;				 determine the calling routine pull the RTS address of the stack. 
@END


.EXIT
	JMP MOVE.COMMON.ROUTINE				;CODE THAT IS COMMON TO ALL MOVEVEMENT DIRECTIONS
@END
		
MOVE.SOUTH
@START			
			
;=====================SUBROUTINE DOCUMENTATION====================================
;See documentation in MOVE.NORTH
;=================================================================================

	LDA PLAYER.MOVE.FORCED			;SET TO $01 BY NON-KEYPRESS ROUTINES TO MOVE PLAYER.
	CMP #$01						;IS MOVE FORCED FLAG SET?
	BEQ .NOT_BLOCKED				;IF YES THEN SKIP COLLISION CHECKS.
	
;COLLISSION CHECK
	LDA #SCREEN.ARRAY.ADJACENT_SOUTH			;SOUTH 1 TILE
	STA PLAYER.MOVE.CANDIDATE_TILE_LOC			
	LDA #SCREEN.ARRAY.ADJACENT_SOUTH2			;SOUTH 2 TILES
	STA PLAYER.MOVE.CANDIDATE_TILE_LOC2		
			
	JSR PLAYER.COLLISSION.CHECK

			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; LDX #$AF
			; JMP FULL.BRK
			; LDA TEMP
			; BRK
; .TEMP
			; LDA TEMP
			

	CMP #$00										;RETURN VALUE OF $00 = MOVE PERMITTED, $01 = BLOCKED
	BEQ .NOT_BLOCKED
	JMP BLOCKED
	
.NOT_BLOCKED	
	LDA #$01
	STA PLAYER.MOVE.CURRENT				;SOUTH

;WORLD MAP EDGE CHECK
	LDA GMAP.Y
	CMP #WORLD.EDGE.SOUTH
	BNE .UPDATE.ZONE.INFORMATION
	JMP BLOCKED							;for now walking off the map edge produces the same result as walking into a mountain or other impassible terrain.

	
.UPDATE.ZONE.INFORMATION
@START
			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK2
			; CMP #$01
			; BNE .TEMP1
				; LDA #$00
				; STA HTAB	
				; LDA #$01
				; STA VTAB
				; jsr UPDATE.CHAR.POS
				; lda #$b0
				; jsr cout			
			
			
			; ;JSR KEYIN
; .TEMP1
			; LDA TEMP

			
;MANAGE ZONE TRANSITIONS	
	INC RMAP.Y
	LDA RMAP.Y
	CMP #RZONE.LOAD_THREASHOLD.SOUTH	;HAS LOAD THREASHOLD BEEN REACHED?
	BNE .NO_LOAD


	JSR MAP.EXIT.CHECK					;IS PLAYER IN A BUILDING? IF SO, TRANSITION ZONES OR EXIT BUILDING?		



			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK2
			; CMP #$01
			; BNE .TEMP2
				; lda #$b1
				; jsr cout			
			
			
			;JSR KEYIN
.TEMP2
			LDA TEMP
			
	JSR ZONE_TOOLS.TRANSITION.SOUTH



			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK2
			; CMP #$01
			; BNE .TEMP3
				; lda #$b2
				; jsr cout			
			
			
			; ;JSR KEYIN
; .TEMP3
			; LDA TEMP
			
			
			
;CENTER RMAP IN REGIONAL MAP START POSITION 
;-------
;SBC 16-BIT 
	LDA RMAP
	SEC
	SBC #RZONE.LOAD.OFFSET.DOWN
	STA RMAP
	LDA RMAP+$1
	SBC /RZONE.LOAD.OFFSET.DOWN
	STA RMAP+$1
;-------
;INCREMENT PLAYER ZONE
	LDA PLAYER.WMAP.ZONE
	CLC
	ADC #WZONE.OFFSET
	STA PLAYER.WMAP.ZONE
	
;RESET PLAYER RMAP Y-AXIS
	LDA #RZONE.LOAD.Y.START
	STA RMAP.Y
	
;RESET MOB (SS) REGIONAL ZONE FLAGS	
	LDA #MAP_OBJECTS.SS.Y_FLAG.LOWER.START		;UPDATE FLAGS USED TO DETERMINE IF AN SS IS LOCATED ON THE REGIONAL MAP. IT CHANGES WITH EACH PLAYER MOVE BECAUSE MOB'S X,Y IS TRACKED RELATIVE TO THE PLAYER
	STA MAP_OBJECTS.SS.Y_FLAG.LOWER
	
	LDA #MAP_OBJECTS.SS.Y_FLAG.UPPER.START
	STA MAP_OBJECTS.SS.Y_FLAG.UPPER	
	
	DEC MAP_OBJECTS.SS.Y_FLAG.LOWER				;THIS IS TO OFFSET THE NORMAL MORE ADJUSTMENT, SO THAT THE FLAGS ARE IN THE START POSITION NET OF PLAYER MOVE
	INC MAP_OBJECTS.SS.Y_FLAG.UPPER
	
.NO_LOAD
@END

			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK2
			; CMP #$01
			; BNE .TEMP4
				; lda #$b3
				; jsr cout			
			
			
			; ;JSR KEYIN
; .TEMP4
			; LDA TEMP
			
			

.UPDATE.PLAYER.MAP_POSITION
@START	
	;SAVE PLAYER GMAP.X/Y BEFORE CURRENT MOVE
	LDA GMAP.X
	STA GMAP.X.LAST
	
	LDA GMAP.Y
	STA GMAP.Y.LAST
	
	;ADJUST POSITION
	INC GMAP.Y					;LATITUE +1 POSITION SOUTH (DOWN)
	
	LDA RMAP					;UPDATE POSITION IN REGIONAL MAP ARRAY
	STA OP1
	LDA RMAP+$1
	STA OP1+$1
	LDA #OFFSET.DOWN
	STA OP2
	LDA #$00
	STA OP2+$1
	
	JSR ADC.16					;RMAP(2) + #OFFSET.DOWN(1)
	
	LDA RESULT
	STA RMAP
	LDA RESULT+$1
	STA RMAP+$1
	
	;UPDATE SMAP/SMAP.CURRENT
	LDA RMAP					;LOAD TILE_ID OF CURRENT MAP POSITION (CENTER/PLAYER TILE)
	STA OP1
	LDA RMAP+$1
	STA OP1+$1
	
	LDA #OFFSET.SCREEN			;CALCULATE TILE_ID OF TILE IN UPPER LEFT CORNER OF SCREEN
	STA OP2
	LDA /OFFSET.SCREEN
	STA OP2+$1
	
	JSR SBC.16					;RMAP(2) - SCREEN.OFFSET.LO/HO (2)
			
	LDA RESULT					;SAVE TILE_ID OF UPPER LEFT SCREEN TILE
	STA SMAP
	STA SMAP.CURRENT
	LDA RESULT+$1
	STA SMAP+$1
	STA SMAP.CURRENT+$1	
	
;ADJUST MOB (SS) REGIONAL MAP FLAGS	
;**NOTE: increasing a lower flag removed 1 tile radius, decreasing a lower flag adds 1 tile radius		
	DEC MAP_OBJECTS.SS.Y_FLAG.LOWER		;ADJUST FLAGS USED TO DETERMINE IF AN SS IS LOCATED ON THE REGIONAL MAP. IT CHANGES WITH EACH PLAYER MOVE BECAUSE MOB'S X,Y IS TRACKED RELATIVE TO THE PLAYER
	DEC MAP_OBJECTS.SS.Y_FLAG.UPPER

	; INC MAP_OBJECTS.SS.Y_FLAG.LOWER		;ADJUST FLAGS USED TO DETERMINE IF AN SS IS LOCATED ON THE REGIONAL MAP. IT CHANGES WITH EACH PLAYER MOVE BECAUSE MOB'S X,Y IS TRACKED RELATIVE TO THE PLAYER
	; DEC MAP_OBJECTS.SS.Y_FLAG.UPPER
	
	;FUTURE: LOGIC FOR REACHING MAP EDGE...EITHER STOP PLAYER FROM DOING SO, TRIGGER AN EVENT OR IMPLEMENT 
	;LOGIC FOR A ROUND WORLD.
	;
	;IDEA: fill the edges of the map with at least 1/2 screen width of tiles of the same type as are on the
	;		opposite edge. then it doesn't really matter if the array calcualtions are skewed a bit.
	;		It seams like this is what U4/U5 did, but then again, it seems like seas creature and pirate ship
	;		movement would get thrown off.
	
.NOFLIP
;**OPT** Speed. The code for flip.page and draw.tile.player could get put in-line instead of JSR.
;check to see where else these routines are called.

	LDX PLAYER.TRANSPORT.ACTIVE					;LOAD TRANSPORT MO RECORD INDEX
	CPX #$FF									;IS PLAYER WALKING?
	BEQ .UPDATE.VIDEO_SCREEN					;IF YES, DON'T MODIFY ANY MO RECORDS, PROCEED TO UPDATE VIDEO SCREEN
	INC MAP_OBJECTS.GENERAL+$1,X				;IF NO, ADJUST PLAYER TRANSPORT MO X-AXIS TO REFLECT PLAYER MOVE

	LDY #SCREEN.ARRAY.PLAYER_LOCATION			;LOAD PLAYER TILE AS INDEX	 (SCREEN ARRAYS HAVEN'T BEEN SCROLLED YET FOR CURRENT PLAYER MOVE)
	LDA #$FF									;THIS IS THE VALUE FOR NO MO RECORD 
	STA SCREEN.MO_GENERAL.DATA,Y				;ERASE TRANSPORT FROM POSITION, BEFORE CURRENT PLAYER MOVE, IN SCREEN ARRAY
				
@END
				
.UPDATE.VIDEO_SCREEN	
@START
		;ENABLE BANK-SWITCHED RAM ($D000 Bank 1)
		LDA $C08B					;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK1, 1ST)
		LDA $C08B	
	JSR SCROLL.ROWS	
		;ENABLE BANK-SWITCHED RAM ($D000 Bank 1)
		LDA $C083				;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK2 2nd)
		LDA $C083
			
	
		LDA #$04					;SET TRACE
		STA CALLED_BY		
		LDX #$0A
	JSR DRAW.ROW.SINGLE	
	
	JSR DARKNESS.REVIEW				;UPDATE THE HIDDEN (DARKNESS) TILES ON THE SCREEN BASED ON THE TILE_TYPE VALUES IN SCREEN.TILE.DATA 



;ERASE OLD PLAYER ICON?
		; LDA #$00
		; STA SAVED_TILE_TYPE
		
	LDY #SCREEN.ARRAY.ADJACENT_NORTH	;REMOVE PLAYER ICON FROM LAST MOVE, WHICH GOT SCROLLED
	JSR DRAW.TILE.SINGLE

	;DO ERASE FOR MULTI-TILE TRANSPORT FOOTPRINT?
	LDA PLAYER.TILE.ACTIVE
	CMP #TILE_ID.FRIGATE1.1				;IS FRIGATE THE ACTIVE TRANSPORT? (CURRENTLY THE ONLY MULTI-TILE TRANSPORT AVAILABLE)
	BNE .EXIT
	
	LDY #PLAYER.MT.ADJACENT_TILES.NORTH1 ;REMOVE PLAYER ICON FROM LAST MOVE, WHICH GOT SCROLLED. #SCREEN.ARRAY.ADJACENT_NORTH is the same as the other location for a MT player icon. Note we only remove two tiles for a multi tile icon (4 tiles) because two of the tiles in the shape will be redraw automatically with the shape is drawn in it's new location (i.e for a move south, the bottom two tiles of the shape become the upper two tiles of the shape, net of the player move) 
	JSR DRAW.TILE.SINGLE	
		LDA #$00						;RESET TRACE
		STA CALLED_BY	
@END
		
.EXIT	
			
			
	JMP MOVE.COMMON.ROUTINE				;CODE THAT IS COMMON TO ALL MOVEVEMENT DIRECTIONS
@END

MOVE.EAST
@START
;=====================SUBROUTINE DOCUMENTATION====================================
;See documentation in MOVE.NORTH
;=================================================================================

	LDA PLAYER.MOVE.FORCED			;SET TO $01 BY NON-KEYPRESS ROUTINES TO MOVE PLAYER.
	CMP #$01						;IS MOVE FORCED FLAG SET?
	BEQ .NOT_BLOCKED				;IF YES THEN SKIP COLLISION CHECKS.
	
;COLLISSION CHECK
	LDA #SCREEN.ARRAY.ADJACENT_EAST				;EAST 1 TILE
	STA PLAYER.MOVE.CANDIDATE_TILE_LOC			
	LDA #SCREEN.ARRAY.ADJACENT_EAST2			;EAST 2 TILES
	STA PLAYER.MOVE.CANDIDATE_TILE_LOC2		
	JSR PLAYER.COLLISSION.CHECK
	; STA TEMP
	; LDA #$FF
	; STA TEMPX
	; LDA TEMP
	CMP #$00							;RETURN VALUE OF $00 = MOVE PERMITTED, $01 = BLOCKED
	BEQ .NOT_BLOCKED
	JMP BLOCKED
	
.NOT_BLOCKED
	LDA #$02
	STA PLAYER.MOVE.CURRENT				;EAST

;WORLD MAP EDGE CHECK
	LDA GMAP.X
	CMP #WORLD.EDGE.EAST
	BNE .UPDATE.ZONE.INFORMATION
	JMP BLOCKED							;for now walking off the map edge produces the same result as walking into a mountain or other impassible terrain.
	
.UPDATE.ZONE.INFORMATION
@START	

	
;MANAGE ZONE TRANSITIONS	
	INC RMAP.X
	LDA RMAP.X
	CMP #RZONE.LOAD_THREASHOLD.EAST		;HAS LOAD THREASHOLD BEEN REACHED?
	BNE .NO_LOAD

	JSR MAP.EXIT.CHECK					;IS PLAYER IN A BUILDING? IF SO, TRANSITION ZONES OR EXIT BUILDING?		

		
	JSR ZONE_TOOLS.TRANSITION.EAST


			
		; PLA
		; TAX
		; PLA
		; TAY
		; LDA #$AC
		; JSR FULL.BRK
		
;CENTER RMAP IN REGIONAL MAP START POSITION 
;-------
;ADC 16-BIT 
	LDA RMAP
	SEC
	SBC #RZONE.LOAD.OFFSET.RIGHT
	STA RMAP
	LDA RMAP+$1
	SBC /RZONE.LOAD.OFFSET.RIGHT
	STA RMAP+$1
;-------
;INCREMENT PLAYER ZONE
	INC PLAYER.WMAP.ZONE
	
;RESET PLAYER RMAP X-AXIS
	LDA #RZONE.LOAD.X.START
	STA RMAP.X

;RESET MOB (SS) REGIONAL ZONE FLAGS	
	LDA #MAP_OBJECTS.SS.X_FLAG.LOWER.START		;RESET FLAGS USED TO DETERMINE IF AN SS IS LOCATED ON THE REGIONAL MAP. IT CHANGES WITH EACH PLAYER MOVE BECAUSE MOB'S X,Y IS TRACKED RELATIVE TO THE PLAYER
	STA MAP_OBJECTS.SS.X_FLAG.LOWER
	
	LDA #MAP_OBJECTS.SS.X_FLAG.UPPER.START
	STA MAP_OBJECTS.SS.X_FLAG.UPPER
	
	DEC MAP_OBJECTS.SS.X_FLAG.LOWER				;THIS IS TO OFFSET THE NORMAL MORE ADJUSTMENT, SO THAT THE FLAGS ARE IN THE START POSITION NET OF PLAYER MOVE
	INC MAP_OBJECTS.SS.X_FLAG.UPPER
	
.NO_LOAD
@END

.UPDATE.PLAYER.MAP_POSITION
@START
	;SAVE PLAYER GMAP.X/Y BEFORE CURRENT MOVE
	LDA GMAP.X
	STA GMAP.X.LAST
	
	LDA GMAP.Y
	STA GMAP.Y.LAST
	
	;ADJUST POSITION
	INC GMAP.X							;LONGITUTE +1 POSITION EAST (RIGHT)
	INC RMAP							;UPDATE POSITION IN REGIONAL MAP ARRAY
	BNE .NOFLIP
	INC RMAP+$1	
.NOFLIP	

;UPDATE SMAP/SMAP.CURRENT
	LDA RMAP					;LOAD TILE_ID OF CURRENT MAP POSITION (CENTER/PLAYER TILE)
	STA OP1
	LDA RMAP+$1
	STA OP1+$1
	
	LDA #OFFSET.SCREEN			;CALCULATE TILE_ID OF TILE IN UPPER LEFT CORNER OF SCREEN
	STA OP2
	LDA /OFFSET.SCREEN
	STA OP2+$1
	
	JSR SBC.16					;RMAP(2) - SCREEN.OFFSET.LO/HO (2)
			
	LDA RESULT					;SAVE TILE_ID OF UPPER LEFT SCREEN TILE
	STA SMAP
	STA SMAP.CURRENT
	LDA RESULT+$1
	STA SMAP+$1
	STA SMAP.CURRENT+$1
	
;ADJUST MOB (SS) REGIONAL MAP FLAGS	
;**NOTE: increasing a lower flag removed 1 tile radius, decreasing a lower flag adds 1 tile radius		
	DEC MAP_OBJECTS.SS.X_FLAG.LOWER		;ADJUST FLAGS USED TO DETERMINE IF AN SS IS LOCATED ON THE REGIONAL MAP. IT CHANGES WITH EACH PLAYER MOVE BECAUSE MOB'S X,Y IS TRACKED RELATIVE TO THE PLAYER
	DEC MAP_OBJECTS.SS.X_FLAG.UPPER

	; INC MAP_OBJECTS.SS.X_FLAG.LOWER		;ADJUST FLAGS USED TO DETERMINE IF AN SS IS LOCATED ON THE REGIONAL MAP. IT CHANGES WITH EACH PLAYER MOVE BECAUSE MOB'S X,Y IS TRACKED RELATIVE TO THE PLAYER
	; DEC MAP_OBJECTS.SS.X_FLAG.UPPER
	
	;FUTURE: LOGIC FOR REACHING MAP EDGE...EITHER STOP PLAYER FROM DOING SO, TRIGGER AN EVENT OR IMPLEMENT 
	;LOGIC FOR A ROUND WORLD.
	;
	;IDEA: fill the edges of the map with at least 1/2 screen width of tiles of the same type as are on the
	;		opposite edge. then it doesn't really matter if the array calcualtions are skewed a bit.
	;		It seams like this is what U4/U5 did, but then again, it seems like seas creature and pirate ship
	;		movement would get thrown off.

;IF PLAYER TRANSPORT ACTIVE, UPDATE X,Y OF THE TRANSPORT MO RECORD 
;(this is so the tranport MO moves with the player)

	LDX PLAYER.TRANSPORT.ACTIVE					;LOAD TRANSPORT MO RECORD INDEX
	CPX #$FF									;IS PLAYER WALKING?
	BEQ .UPDATE.VIDEO_SCREEN					;IF YES, DON'T MODIFY ANY MO RECORDS, PROCEED TO UPDATE VIDEO SCREEN
	INC MAP_OBJECTS.GENERAL,X					;IF NO, ADJUST PLAYER TRANSPORT MO X-AXIS TO REFLECT PLAYER MOVE

	LDY #SCREEN.ARRAY.PLAYER_LOCATION			;LOAD PLAYER TILES AS INDEX	 (SCREEN ARRAYS HAVEN'T BEEN SCROLLED YET FOR CURRENT PLAYER MOVE)
	LDA #$FF									;THIS IS THE VALUE FOR NO MO RECORD 
	STA SCREEN.MO_GENERAL.DATA,Y				;ERASE TRANSPORT FROM POSITION, BEFORE CURRENT PLAYER MOVE, IN SCREEN ARRAY	
@END

.UPDATE.VIDEO_SCREEN
@START

		;ENABLE BANK-SWITCHED RAM ($D000 Bank 1)
		LDA $C08B					;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK1, 1ST)
		LDA $C08B	
	JSR SCROLL.COLUMNS
		;ENABLE BANK-SWITCHED RAM ($D000 Bank 1)
		LDA $C083				;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK2 2nd)
		LDA $C083
		
		
		LDA #$05					;SET TRACE
		STA CALLED_BY	
		LDX #$11				;SPECIFY 1ST COLUMN FOR DRAW ROUTINE
	JSR DRAW.COLUMN.SINGLE
	
	JSR DARKNESS.REVIEW		;UPDATE THE HIDDEN (DARKNESS) TILES ON THE SCREEN BASED ON THE TILE_TYPE VALUES IN SCREEN.TILE.DATA 


;ERASE OLD PLAYER ICON	
	LDY #SCREEN.ARRAY.ADJACENT_WEST		;REMOVE PLAYER ICON FROM LAST MOVE, WHICH GOT SCROLLED
	JSR DRAW.TILE.SINGLE
	
	;DO ERASE FOR MULTI-TILE TRANSPORT FOOTPRINT?
	LDA PLAYER.TILE.ACTIVE
	CMP #TILE_ID.FRIGATE1.1				;IS FRIGATE THE ACTIVE TRANSPORT? (CURRENTLY THE ONLY MULTI-TILE TRANSPORT AVAILABLE)
	BNE .EXIT
	
	LDY #PLAYER.MT.ADJACENT_TILES.WEST1 ;REMOVE PLAYER ICON FROM LAST MOVE, WHICH GOT SCROLLED. #SCREEN.ARRAY.ADJACENT_NORTH is the same as the other location for a MT player icon. Note we only remove two tiles for a multi tile icon (4 tiles) because two of the tiles in the shape will be redraw automatically with the shape is drawn in it's new location (i.e for a move south, the bottom two tiles of the shape become the upper two tiles of the shape, net of the player move) 
	JSR DRAW.TILE.SINGLE	
		LDA #$00						;RESET TRACE
		STA CALLED_BY
@END

.EXIT
	JMP MOVE.COMMON.ROUTINE				;CODE THAT IS COMMON TO ALL MOVEVEMENT DIRECTIONS
	
@END
	
MOVE.WEST
@START
;=====================SUBROUTINE DOCUMENTATION====================================
;See documentation in MOVE.NORTH
;=================================================================================

	LDA PLAYER.MOVE.FORCED			;SET TO $01 BY NON-KEYPRESS ROUTINES TO MOVE PLAYER.
	CMP #$01						;IS MOVE FORCED FLAG SET?
	BEQ .NOT_BLOCKED				;IF YES THEN SKIP COLLISION CHECKS. 
	
;COLLISSION CHECK
	LDA #SCREEN.ARRAY.ADJACENT_WEST			;WEST 1 TILE
	STA PLAYER.MOVE.CANDIDATE_TILE_LOC				
	LDA #SCREEN.ARRAY.ADJACENT_WEST2		;WEST 2 TILES
	STA PLAYER.MOVE.CANDIDATE_TILE_LOC2		
	JSR PLAYER.COLLISSION.CHECK
	CMP #$00						;RETURN VALUE OF $00 = MOVE PERMITTED, $01 = BLOCKED
	BEQ .NOT_BLOCKED
	JMP BLOCKED
	
.NOT_BLOCKED	
	LDA #$03
	STA PLAYER.MOVE.CURRENT				;EAST

;WORLD MAP EDGE CHECK
	LDA PLAYER.MAP.LOCATION_TYPE		;building maps are except because a zone transition triggers exit back to prior map
	; CMP #MAP.TYPE.TOWN_VILLAGE
	; BEQ .UPDATE.ZONE.INFORMATION
	; CMP #MAP.TYPE.CASTLE
	; BEQ .UPDATE.ZONE.INFORMATION

	;is a building map active
	;**OPT** Memory. Speed. Right now there is only one code for location type, which is used to determine the tile_set to use and for other location-type specific features. There are a lot of the later. It might be a savings if the 
					;there were a tile-set code separate from location type code, and there was a default tile-set picked using the location-type code if tile-set code was set to default. This might require adding another byte to some hex tables
					;but I suspect it would pay off. 
	CMP #MAP.TYPE.BUILDING.GRE 	;is map type = building?
	BCC .MAP.TYPE_CHECK.DONE		;if no
	CMP #MAP.TYPE.BUILDING.LT	;is map type = building?
	BCS .MAP.TYPE_CHECK.DONE		;if no
	JMP .UPDATE.ZONE.INFORMATION	;if yes
.MAP.TYPE_CHECK.DONE
	
	LDA GMAP.X
	CMP #WORLD.EDGE.WEST
	BNE .UPDATE.ZONE.INFORMATION
	JMP BLOCKED							;for now walking off the map edge produces the same result as walking into a mountain or other impassible terrain.

	
.UPDATE.ZONE.INFORMATION
@START

	LDA #$03
	STA PLAYER.MOVE.CURRENT				;WEST

;========MANAGE ZONE TRANSITIONS=======	
	DEC RMAP.X							;RMAP.X/Y ARE THE PLAYERS X,Y WITHIN THE REGIONAL MAP, WHICH ARE USED TO TRIGGER ZONE TRANSITION. 
	LDA RMAP.X
	CMP #RZONE.LOAD_THREASHOLD.WEST		;HAS LOAD THREASHOLD BEEN REACHED?
	BNE .NO_LOAD

	JSR MAP.EXIT.CHECK					;IS PLAYER IN A BUILDING? IF SO, TRANSITION ZONES OR EXIT BUILDING?		

	JSR ZONE_TOOLS.TRANSITION.WEST
	
;CENTER RMAP IN REGIONAL MAP START POSITION 
;-------
;ADC 16-BIT 
	LDA RMAP
	CLC
	ADC #RZONE.LOAD.OFFSET.LEFT
	STA RMAP
	LDA RMAP+$1
	ADC /RZONE.LOAD.OFFSET.LEFT
	STA RMAP+$1
;-------
;INCREMENT PLAYER ZONE
	DEC PLAYER.WMAP.ZONE
	
;RESET PLAYER RMAP X-AXIS
	LDA #RZONE.LOAD.X.START
	STA RMAP.X

;RESET MOB (SS) REGIONAL ZONE FLAGS	
	LDA #MAP_OBJECTS.SS.X_FLAG.LOWER.START		;RESET FLAGS USED TO DETERMINE IF AN SS IS LOCATED ON THE REGIONAL MAP. IT CHANGES WITH EACH PLAYER MOVE BECAUSE MOB'S X,Y IS TRACKED RELATIVE TO THE PLAYER
	STA MAP_OBJECTS.SS.X_FLAG.LOWER
	
	LDA #MAP_OBJECTS.SS.X_FLAG.UPPER.START
	STA MAP_OBJECTS.SS.X_FLAG.UPPER

	INC MAP_OBJECTS.SS.X_FLAG.LOWER				;THIS IS TO OFFSET THE NORMAL MORE ADJUSTMENT, SO THAT THE FLAGS ARE IN THE START POSITION NET OF PLAYER MOVE
	DEC MAP_OBJECTS.SS.X_FLAG.UPPER

;============END MANAGE ZONE TRANSITIONS=====
	
.NO_LOAD
@END
	
.UPDATE.PLAYER.MAP_POSITION
@START
	;SAVE PLAYER GMAP.X/Y BEFORE CURRENT MOVE
	LDA GMAP.X
	STA GMAP.X.LAST
	
	LDA GMAP.Y
	STA GMAP.Y.LAST
	
	;ADJUST POSITION
	DEC GMAP.X					;LONGITUTE -1 POSITION WEST (LEFT)
	
;16-BIT SUBTRACT (REQUIRED, PROBLEMS WITH DEC WITH FLIP DETECT)
	LDA RMAP					;UPDATE POSITION IN REGIONAL MAP ARRAY
	SEC
	SBC #$01
	STA RMAP
	LDA RMAP+$1
	SBC #$00
	STA RMAP+$1
	
.NOFLIP
	
;UPDATE SMAP/SMAP.CURRENT
	LDA RMAP					;LOAD TILE_ID OF CURRENT MAP POSITION (CENTER/PLAYER TILE)
	STA OP1
	LDA RMAP+$1
	STA OP1+$1
	
	LDA #OFFSET.SCREEN			;CALCULATE TILE_ID OF TILE IN UPPER LEFT CORNER OF SCREEN
	STA OP2
	LDA /OFFSET.SCREEN
	STA OP2+$1
	
	JSR SBC.16					;RMAP(2) - SCREEN.OFFSET.LO/HO (2)
			
	LDA RESULT					;SAVE TILE_ID OF UPPER LEFT SCREEN TILE
	STA SMAP
	STA SMAP.CURRENT
	LDA RESULT+$1
	STA SMAP+$1
	STA SMAP.CURRENT+$1

;UPDATE MOB (SS) REGIONAL MAP FLAGS
;**NOTE: increasing a lower flag removed 1 tile radius, decreasing a lower flag adds 1 tile radius		
	INC MAP_OBJECTS.SS.X_FLAG.LOWER		;ADJUST FLAGS USED TO DETERMINE IF AN SS IS LOCATED ON THE REGIONAL MAP. IT CHANGES WITH EACH PLAYER MOVE BECAUSE MOB'S X,Y IS TRACKED RELATIVE TO THE PLAYER
	INC MAP_OBJECTS.SS.X_FLAG.UPPER

	; DEC MAP_OBJECTS.SS.X_FLAG.LOWER		;ADJUST FLAGS USED TO DETERMINE IF AN SS IS LOCATED ON THE REGIONAL MAP. IT CHANGES WITH EACH PLAYER MOVE BECAUSE MOB'S X,Y IS TRACKED RELATIVE TO THE PLAYER
	; INC MAP_OBJECTS.SS.X_FLAG.UPPER
	
	;FUTURE: LOGIC FOR REACHING MAP EDGE...EITHER STOP PLAYER FROM DOING SO, TRIGGER AN EVENT OR IMPLEMENT 
	;LOGIC FOR A ROUND WORLD.
	;
	;IDEA: fill the edges of the map with at least 1/2 screen width of tiles of the same type as are on the
	;		opposite edge. then it doesn't really matter if the array calcualtions are skewed a bit.
	;		It seams like this is what U4/U5 did, but then again, it seems like seas creature and pirate ship
	;		movement would get thrown off.

;IF PLAYER TRANSPORT ACTIVE, UPDATE X,Y OF THE TRANSPORT MO RECORD 
;(this is so the tranport MO moves with the player)

	LDX PLAYER.TRANSPORT.ACTIVE					;LOAD TRANSPORT MO RECORD INDEX
	CPX #$FF									;IS PLAYER WALKING?
	BEQ .UPDATE.VIDEO_SCREEN					;IF YES, DON'T MODIFY ANY MO RECORDS, PROCEED TO UPDATE VIDEO SCREEN
	DEC MAP_OBJECTS.GENERAL,X					;IF NO, ADJUST PLAYER TRANSPORT MO X-AXIS TO REFLECT PLAYER MOVE

	LDY #SCREEN.ARRAY.PLAYER_LOCATION			;LOAD PLAYER TILE AS INDEX	 (SCREEN ARRAYS HAVEN'T BEEN SCROLLED YET FOR CURRENT PLAYER MOVE)
	LDA #$FF									;THIS IS THE VALUE FOR NO MO RECORD 
	STA SCREEN.MO_GENERAL.DATA,Y				;ERASE TRANSPORT FROM POSITION, BEFORE CURRENT PLAYER MOVE, IN SCREEN ARRAY
	
@END

.UPDATE.VIDEO_SCREEN	
@START


		;ENABLE BANK-SWITCHED RAM ($D000 Bank 1)
		LDA $C08B					;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK1, 1ST)
		LDA $C08B	
	JSR SCROLL.COLUMNS
		;ENABLE BANK-SWITCHED RAM ($D000 Bank 1)
		LDA $C083				;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK2 2nd)
		LDA $C083
	
		; ;ENABLE.BS_RAM
		; LDA $C083				;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK 1ST)
		; LDA $C083	
	;JSR SCROLL.COLUMNS				
		; LDA $C082				;READ ENABLE ROM, DISABLE WRITE ON BSM (NORMAL STATE)
		; ;DISABLE.BS_RAM

		LDA #$06			;SET TRACE
		STA CALLED_BY		
		LDX #$00				;SPECIFY 1ST COLUMN FOR DRAW ROUTINE
	JSR DRAW.COLUMN.SINGLE
	
	JSR DARKNESS.REVIEW				;UPDATE THE HIDDEN (DARKNESS) TILES ON THE SCREEN BASED ON THE TILE_TYPE VALUES IN SCREEN.TILE.DATA 
	
;ERASE OLD PLAYER ICON?
	LDY #SCREEN.ARRAY.ADJACENT_EAST		;REMOVE PLAYER ICON FROM LAST MOVE, WHICH GOT SCROLLED
	JSR DRAW.TILE.SINGLE

	;DO ERASE FOR MULTI-TILE TRANSPORT FOOTPRINT?
	LDA PLAYER.TILE.ACTIVE
	CMP #TILE_ID.FRIGATE1.1				;IS FRIGATE THE ACTIVE TRANSPORT? (CURRENTLY THE ONLY MULTI-TILE TRANSPORT AVAILABLE)
	BNE .EXIT
	
	LDY #PLAYER.MT.ADJACENT_TILES.EAST0 ;REMOVE PLAYER ICON FROM LAST MOVE, WHICH GOT SCROLLED. #SCREEN.ARRAY.ADJACENT_NORTH is the same as the other location for a MT player icon. Note we only remove two tiles for a multi tile icon (4 tiles) because two of the tiles in the shape will be redraw automatically with the shape is drawn in it's new location (i.e for a move south, the bottom two tiles of the shape become the upper two tiles of the shape, net of the player move) 
	JSR DRAW.TILE.SINGLE	
	LDY #PLAYER.MT.ADJACENT_TILES.EAST1 ;REMOVE PLAYER ICON FROM LAST MOVE, WHICH GOT SCROLLED. #SCREEN.ARRAY.ADJACENT_NORTH is the same as the other location for a MT player icon. Note we only remove two tiles for a multi tile icon (4 tiles) because two of the tiles in the shape will be redraw automatically with the shape is drawn in it's new location (i.e for a move south, the bottom two tiles of the shape become the upper two tiles of the shape, net of the player move) 
	JSR DRAW.TILE.SINGLE	
		LDA #$00						;RESET TRACE
		STA CALLED_BY
@END
		
.EXIT		
	;**FALLS THROUGH
@END



MOVE.COMMON.ROUTINE ;==========CODE THAT IS COMMON TO ALL MOVEMENT DIRECTIONS (N/S/E/W)=====
@START

;=====================SUBROUTINE DOCUMENTATION====================================
;This subroutine takes over control (via JMP) at the end of all movement routines (MOVE.NORTH/SOUTH/EAST/WEST)
;to check to see if the player, after the current move, is located on a Tile_ID with special characteristics
;
;The following is a summary of the special tile types:
;
;*Quick Sand: player icon sinks (handled by animation) and this routine
;									applies slow progress based on several conditions to create the effect that the more tiles of quicksand the player crosses, 
;									the harder it is for the player to get out. If the player is not on a quicksand tile the player icon buffer is refreshed with 
;									the walking player icon shape table. This is because the tile scrolling done to create the sinking effect modifies the shape data in the player icon buffer. 
;*Water: The player icon is changed when the player enters surf water (waist deep), and changed again for all other water types (drowning) 
;*Deep Water: Normally when the player holds down the movement key, all animation stops because of a keypress animation abort trigger at the end of this routine.
;									When the view screen is all deep water, this trigger is bypassed thus forcing animation to be processed. This results a lower "tiles traversed per second ratio", which is normally ~20 per 6-7 seconds. 
;									However, since there is no frame of reference for movement when the screen is all deep water, this "slowness" is not detectable, it just makes the ocenas (and thus the world) seem larger than they actually are.
;									The exception is if a MOB or transport object is on the screen. In that event, animation is stopped just like it is when there are other non-deep water tiles on screen. 
;									These changes to animation status are driven by a tally of the number of deep water tiles on the view screen performed by this routine. This
;									count is taken based on the screen net of the player move and before the page is flipped which is the perfect time to decide whether animation will be skipped or not. 
;									Footnote: Boats have a pretty cool wake when in deep water, and there are also some distorations that look like waves which appear in all water types. These are
;									gliches than became features. I have no idea what is causing it. 
;
;
;=================================================================================
 			  
	JSR MO.DRAW						;SHOULD BE AFTER PLAYER ICON FROM LAST MOVE IS REPLACED


	;JSR NPC_BUILDING.DRAW



;DRAW PLAYER ICON
;(note: to add more tile types than abort animation check, add the new routine just before
;.check.water and change the JMP .draw.tile to goto the new check)
.CHECK.QUICKSAND
	LDY #SCREEN.ARRAY.PLAYER_LOCATION
	LDA SCREEN.TILE.DATA,Y
	
	CMP #TILE_ID.QUICKSAND				;IS PLAYER STANDING IN QUICKSAND?
	BNE MOVE.COMMON.CHECK.WATER			;IF NO, CONTINUE WITH WATER CHECK WHICH ALSO RESETS PLAYER HEIGHT TO DEFAULT
	LDA #$01
	STA ANIMATION.FORCED				;SET FORCED ANIMATION FLAG SO ANIMATION KEY PRESS ABORT CHECK WILL BE SKIPPED. 
	JMP MOVE.COMMON.DRAW.TILE			;IF YES, GO RIGHT TO DRAW TILE SO THAT THE PLAYER ICON BUFFER ISN'T UPDATED (KEEPING SUNK ICON INTACT) AND PLAYER HEIGHT ISN'T RESET. 
	
MOVE.COMMON.CHECK.WATER
	LDA PLAYER.WALKING.TILE.DEFAULT		;ENSURE PLAYER WALKING TILE IS SET TO DEFAULT AND ONLY CHANGE IT IF THE PLAYER IS STANDING IN WATER NET OF THE CURRENT MOVE. 
	STA PLAYER.WALKING.TILE
	
	LDY #SCREEN.ARRAY.PLAYER_LOCATION
	LDA SCREEN.TILE.DATA,Y

	;IS PLAYER STANDING IN WATER?
	CMP #ANIMATION.WATER_RANGE.START	;IS TILE_TYPE < START OF WATER TILE RANGE?
	BCC UPDATE.BUFFER_STEP						;IF YES, NEXT CHECK 
	
	CMP #ANIMATION.WATER_RANGE.END		;IS TILE_TYPE >= END OF WATER TILE RANGE+$1?
	BCS UPDATE.BUFFER_STEP						;IF YES, NEXT CHECK 
	
	;BRANCH BASED ON WATER TYPE
	CMP #TILE_ID.SURF					;IS PLAYER STANDING IN SURF
	BEQ MOVE.COMMON.SURF
	CMP #TILE_ID.DEEP_WATER				;IS PLAYER STANDING IN DEEP WATER
	BEQ MOVE.COMMON.DEEP_WATER
			
			; TAX
			; LDA TEXT
			; LDA #TILE_ID.SURF
			; BRK
	JMP MOVE.COMMON.OTHER_WATER
	
MOVE.COMMON.SURF
	LDY #SCREEN.ARRAY.PLAYER_LOCATION			;LOAD PLAYER TILE AS INDEX
	LDA SCREEN.MO_GENERAL.DATA,Y				;IS THERE A MO IN THE PLAYER'S LOCATION (NET OF CURRENT MOVE)	
	CMP #$FF									;THIS IS THE VALUE FOR NO MO RECORD 
	BEQ .SURF.NO_TRANSPORT						;IF YES, DON'T MODIFY PLAYER ICON

.SURF.TRANSPORT
	LDA PLAYER.WALKING.TILE.DEFAULT
	STA PLAYER.WALKING.TILE						;RESET PLAYER ICON TO DEFAULT IN CASE PLAYER WAS IN WATER AND JUST WALKED ONTO TRANSPORT
	JMP UPDATE.BUFFER	
	
.SURF.NO_TRANSPORT	
	LDA #TILE_ID.PLAYER_ICON.HALF_SUNK
	STA PLAYER.WALKING.TILE
	JMP UPDATE.BUFFER							

	
UPDATE.BUFFER_STEP
	JMP UPDATE.BUFFER	
	
MOVE.COMMON.DEEP_WATER
	LDY #SCREEN.ARRAY.PLAYER_LOCATION			;LOAD PLAYER TILE AS INDEX
	LDA SCREEN.MO_GENERAL.DATA,Y				;IS THERE A MO IN THE PLAYER'S LOCATION (NET OF CURRENT MOVE)	
	CMP #$FF									;THIS IS THE VALUE FOR NO MO RECORD 
	BEQ .DEEP_WATER.NO_TRANSPORT				;IF YES, DON'T MODIFY PLAYER ICON

.DEEP_WATER.TRANSPORT					
	LDA PLAYER.WALKING.TILE.DEFAULT
	STA PLAYER.WALKING.TILE						;RESET PLAYER ICON TO DEFAULT IN CASE PLAYER WAS IN WATER AND JUST WALKED ONTO TRANSPORT
	JMP .DEEP_WATER.ANIMATION.CHECKS
	
.DEEP_WATER.NO_TRANSPORT	
	LDA #TILE_ID.PLAYER_ICON.FULL_SUNK
	STA PLAYER.WALKING.TILE
	;**FALLS THROUGH
	
.DEEP_WATER.ANIMATION.CHECKS
;CALCULATE ANIMATION.DEEP_WATER.TALLY FOR USE ON NEXT MOVE
;Note: it is important to do this calculation here so that it's detected when the player moves from a screen all deep water to a screen without all deep water and vice versa. Doing this check in the animation routine has issues.
	LDY #$00	
	STY ANIMATION.DEEP_WATER.TALLY		;RESET TALLY
.LOOP.TALLY
	LDA SCREEN.TILE.DATA,Y				;ITERATE THROUGH SCREEN.TILE.DATA
	CMP #TILE_ID.DEEP_WATER
	BNE .NOT.DEEP
	INC ANIMATION.DEEP_WATER.TALLY		;INCREMENT TALLY FOR EACH INSTANCE OF A DEEP WATER TILE
.NOT.DEEP	
	INY
	CPY #SCREEN.ARRAY.LAST_ELEMENT2
	BCC .LOOP.TALLY

;DETERMINE IF SCREEN IS ALL DEEP WATER TILES	
	LDA ANIMATION.DEEP_WATER.TALLY
	CMP #SCREEN.ARRAY.LAST_ELEMENT		;IS SCREEN ALL DEEP WATER TILES?
	BCC .SCREEN.NOT.ALL.DEEP_WATER		;IF NO, THEN DON'T FORCE ANIMATION SO MOVEMENT DOESN'T APPEAR SLOW

.SCREEN.ALL.DEEP_WATER					;IF YES, THEN FORCE ANIMATION SO THAT WATER RIPPLES ARE VISIBLE. 
;DETERMINE IF ANY MAP OBJECTS (I.E. MOBS, TRANSPORT) ARE ON THE SCREEN (IF YES, DON'T FORCE ANIMATION)
	LDY #$00	
.LOOP.MO.DETECT
	LDA SCREEN.MO_SPRITE.DATA,Y			;ITERATE THROUGH SCREEN.TILE.DATA
	CMP #$FF							;IS A MOB MO PRESENT?
	BNE .EXIT							;IF YES, EXIT DEEP WATER ROUTINES WITHOUT FORCING ANIMATION
	LDA SCREEN.MO_GENERAL.DATA,Y		;ITERATE THROUGH SCREEN.TILE.DATA
	CMP #$FF							;IS A TRANSPORT MO PRESENT?
	BNE .EXIT							;IF YES, EXIT DEEP WATER ROUTINES WITHOUT FORCING ANIMATION
	INY
	CPY #SCREEN.ARRAY.LAST_ELEMENT2
	BCC .LOOP.MO.DETECT
.FORCE_ANIMATION
	LDA #$01
	STA ANIMATION.FORCED				;SET FORCED ANIMATION FLAG SO ANIMATION KEY PRESS ABORT CHECK WILL BE SKIPPED. 
.EXIT
	JMP UPDATE.BUFFER	
.SCREEN.NOT.ALL.DEEP_WATER
; ;CALCULATE ANIMATION.DEEP_WATER.TALLY FOR USE ON NEXT MOVE
; ;Note: it is important to do this calculation here so that it's detected when the player moves from a screen with not all deep water to a screen with all deep water 
	; LDY #$00	
	; STY ANIMATION.DEEP_WATER.TALLY		;RESET TALLY
; .LOOP.TALLY
	; LDA SCREEN.TILE.DATA,Y				;ITERATE THROUGH SCREEN.TILE.DATA
	; CMP #TILE_ID.DEEP_WATER
	; BNE .NOT.DEEP
	; INC ANIMATION.DEEP_WATER.TALLY		;INCREMENT TALLY FOR EACH INSTANCE OF A DEEP WATER TILE
; .NOT.DEEP	
	; INY
	; CPY #SCREEN.ARRAY.LAST_ELEMENT2
	; BCC .LOOP.TALLY
	
	JMP UPDATE.BUFFER					;EXIT DEEP WATER ROUTINE
	
MOVE.COMMON.OTHER_WATER
	LDY #SCREEN.ARRAY.PLAYER_LOCATION			;LOAD PLAYER TILE AS INDEX
	LDA SCREEN.MO_GENERAL.DATA,Y				;IS THERE A MO IN THE PLAYER'S LOCATION (NET OF CURRENT MOVE)	
	CMP #$FF									;THIS IS THE VALUE FOR NO MO RECORD 
	BEQ .OTHER_WATER.NO_TRANSPORT						;IF YES, DON'T MODIFY PLAYER ICON

.OTHER_WATER.TRANSPORT
	LDA PLAYER.WALKING.TILE.DEFAULT
	STA PLAYER.WALKING.TILE						;RESET PLAYER ICON TO DEFAULT IN CASE PLAYER WAS IN WATER AND JUST WALKED ONTO TRANSPORT
	JMP UPDATE.BUFFER	
	
.OTHER_WATER.NO_TRANSPORT	
	LDA #TILE_ID.PLAYER_ICON.FULL_SUNK
	STA PLAYER.WALKING.TILE
;	JMP UPDATE.BUFFER		
	;**FALLS THROUGH**
	
UPDATE.BUFFER
	LDA #PLAYER.HEIGHT.DEFAULT		;IF PLAYER IS ON QUICKSAND TILE THIS SECTION IS SKIPPED
	STA PLAYER.HEIGHT				;SO THIS IS A GOOD PLACE TO MAKE SURE PLAYER HEIGHT IS AT DEFAULT WHEN PLAYER IS NOT STANDING ON QUICKSAND
	
	JSR LOAD.PLAYER.WALKING.ICON	;COPY NEW PLAYER ICON TO THE PLAYER ICON BUFFER USED BY DRAW.TILE

MOVE.COMMON.DRAW.TILE		
		LDA #$04					;set trace
		STA CALLED_BY		
	JSR DRAW.TILE.PLAYER
		LDA #$00					;reset trace
		STA CALLED_BY	
		
	JSR FLIP.PAGE
	
	JSR TIME.UPDATE.MOVE			;increase game clock by 1 minute
	JSR MAP.ENTER.CHECK				;check for enterable location at the gmap x/y of player, net of current move
	
.EXIT
	LDA ANIMATION.FORCED.OVERRIDE
	CMP #$01						;IS ANIMATION FORCED OVERRIDE FLAG SET?
	BEQ .KEYCHECK					;IF YES, FORCE THE ABORT KEY PRESS CHECK	
	LDA ANIMATION.FORCED
	CMP #$01						;IS ANIMATION FORCED FLAG SET?
	BEQ .EXIT.SKIP.KEYCHECK			;IF YES, DON'T ALLOW AN ABORT DUE TO KEY PRESS		
	LDA PLAYER.MOVE.FORCED			;SET TO $01 BY NON-KEYPRESS ROUTINES TO MOVE PLAYER.
	CMP #$01						;IS MOVE FORCED FLAG SET?
	BEQ .EXIT.SKIP.KEYCHECK			;IF YES, DON'T ALLOW AN ABORT DUE TO KEY PRESS
.KEYCHECK
	LDA KB_BUFFER					;LOAD NEXT KEY PRESS FROM BUFFER. WAS KEY PRESSED?
    BMI .EXIT.ALTERNATE				;IF YES, SKIP COPY.SCREEN WHICH IS PREP FOR ANIMATION. 
.EXIT.SKIP.KEYCHECK

	;!!!!! BSR BANK1 !!!!
	JSR COPY.SCREEN.ENTRANCE ;**DON'T REMOVE** (or the screen goes chaotic sometimes when collisions occur with double mover mobs. MO.DRAW must need the graphics pages in sync for some reason). UDPATE: I think this is needed because sometimes, due to key press abort, no copy.screen was done after the last move, so the pages are out of syncing going into MOVE.PASS, which is triggered by non-movement commands like (3)ZAP, Push, Board, etc.

.EXIT.ALTERNATE

; ;DID MOB INITIATE COMBAT?	
	; LDA COMBAT_SE.MODE.PARM	;($00 = player initiated or init value in map objects manager | $01 = mob initiated | $FF = test mode)
	; BEQ .MOB.INITIATED.COMBAT.CHECK.DONE ;if yes, branch
	; JSR MOVE_MGR.INITIATE.COMBAT
; .MOB.INITIATED.COMBAT.CHECK.DONE


	RTS

@END

BLOCKED	;called when player makes a blocked move
@START
;Note: This routine is called by MOVE.NORTH/SOUTH/EAST/WEST if the player's move is blocked.
;The move routine makes that determination based on the value returned by PLAYER.COLLISION.CHECK


	;update bottom text window
		LDA #GLOBAL.TEXT_BLOCK.COMMAND_BLOCKED			
		STA STRING+$0		
		LDA /GLOBAL.TEXT_BLOCK.COMMAND_BLOCKED
		STA STRING+$1
	JSR PRINT.TW_BOTTOM
	
	JSR PLAY.SOUND.DUMB_ASS
	JSR MOVE.PASS			;RUN PASS ROUTINE SO THAT MO MOVEMENT OCCURS. 
	RTS						;RETURNS TO ROUTINE THAT CALLED MOVEMENT_MANAGER (GAME LOOP)
@END

PLAYER.COLLISSION.CHECK
@START
;PARAMETERS; PLAYER.MOVE.CANDIDATE_TILE_LOC, PLAYER.COMMAND.CURRENT
;RETURN VAUE; ACC ($00 = MOVE PERMITTED, $01 = MOVE BLOCKED)
;ENTRANCE: DIRECT

;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA

;IS PLAYER COLLISSION OVERRIDE ON?
	LDA PLAYER.COLLISSION_OVERRIDE
	CMP #$01
	BNE .FLYING.CHECK
	JMP PLAYER.MOVE_PREMITTED
	
.FLYING.CHECK
;NOTE: IF PLAYER IS RIDING A DRAGON I THINK THE BEST APPROACH IS FOR THE DRAGON TO FLIP THE
;PLAYER COLISSION OVERRIDE	

;IS PLAYER FLYING
	LDA PLAYER.TRANSPORT.STATUS
	CMP #$04
	BCS .CHECK_TRANSPORT		;IF YES, SKIP THE SLOW PROGRESS CHECK
				

.SLOW_PROGRESS.CHECK
@START	
;IS PLAYER ON SLOW PROGRESS TERRAIN?

; ;*****MANUAL OVERRIDE ON
				; JMP .CHECK_TRANSPORT
				
	LDY #SCREEN.ARRAY.PLAYER_LOCATION			;DON'T CHECK CANDIDATE TILE, CHECK CURRENT POSITION, BECAUSE THIS IS EFFECTIVELY A DECISION ON WHETHER TO ALLOW TO MOVE OFF OF IT'S CURRENT TILE (THE DISTINCTION ISN'T VERY RELEVANT FOR HILLS BUT IS VERY RELEVANT FOR QUICKSAND)
	LDA SCREEN.TILE.DATA,Y
	STA PLAYER.TILE.LAST						;TAKES THE OPPORTUNITY TO RECORD THE TILE TYPE THE PLAYER IS CURRENTLY STANDING ON. 


			
	CMP #TILE_ID.HILLS
	BEQ .SLOW_PROGRESS.HILLS
	CMP #TILE_ID.QUICKSAND
	BEQ .SLOW_PROGRESS.QUICKSAND
	
	JMP .CHECK_TRANSPORT


.SLOW_PROGRESS.HILLS	
	JSR RANDOM.8
	CMP #TILE.HILLS.PLAYER.SLOW_PROGRESS	;IF PLAYER TILE IS ON HILLS THEN 25% CHANCE OF SLOW PROGRESS
	BCC .CHECK_TRANSPORT	;NO SLOW PROGRESS


.SLOW_PROGRESS.GO			;SLOW PROGRESS HAS BEEN ENCOUNTERED, MAKE IT HAPPEN	
	JSR PLAY.SOUND.SLOW_PROGRESS
			
			
	;**FUTURE: REPLACE WITH ON SCREEN TEXT "SLOW PROGRESS"
.SLOW_PROGRESS.GO.ALT
;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX
	
	PLA
	PLA						;POP THE LAST RTS OFF THE STACK (THIS WOULD BE FROM THE JSR TO PLAYER.COLLISSION.CHECK FROM MOVE.N/S/E/W)
	JMP MOVE.PASS			;SLOW PROGRESS IS IMPLIMENTED IN THE FORM OF A PLAYER TURN PASS. THE RTS AT THE END OF MOVE PASS SHOULD RETURN TO GAME.LOOP)


.SLOW_PROGRESS.QUICKSAND
;CHECK IF PLAYER ACTIVE TRANSPORT IS EXEMPT
	LDA PLAYER.TILE.ACTIVE
	CMP #TILE_ID.WYVERN
	BEQ .CHECK_TRANSPORT

;IS SLOW PROGRESS AUTOMATIC?	
	LDA #TILE.QUICKSAND.SINK.AUTOMATIC
	CMP PLAYER.HEIGHT							;HAS PLAYER SUNK ALL THE WAY INTO QUICKSAND?
	BCC .SLOW_PROGRESS.ENCOUNTERED				;IF NO, THEN TREAT IT AUTOMATICALLY AS SLOW PROGRESS SO THE SINKING ANIMATION CONTINUES
	;**FALLS THROUGH**
	
	LDA PLAYER.HEIGHT									;NO CHANCE OF SLOW PROGRESS UNTIL PLAYER SINKS TO A CERTAIN HEIGHT. 
	CMP #TILE.QUICKSAND.PLAYER.SLOW_PROGRESS.HEIGHT		;THIS IS SO THAT THE PLAYER, IF HOLDING DOWN MOVEMENT KEY, CAN EASILY TRAVERSE A COUPLE QUICK STAND TILES WITH ONLY MINOR DEALY.
	BCS .CHECK_TRANSPORT
	
	JSR RANDOM.8
	CMP #TILE.QUICKSAND.PLAYER.SLOW_PROGRESS.RATIO	;IF PLAYER TILE IS ON HILLS THEN 25% CHANCE OF SLOW PROGRESS
	BCC .CHECK_TRANSPORT	;NO SLOW PROGRESS

.SLOW_PROGRESS.ENCOUNTERED	
	;NOTE: INTENTINALLY NOT HAVING IT MAKE A BEEP OR DISPLAY SLOW PROGRESS TEXT SO THE QUICKSAND IS MORE SUBTLE. 
	JMP .SLOW_PROGRESS.GO.ALT
@END

	
.CHECK_TRANSPORT	
;IS PLAYER WALKING OR ON TRANSPORT?	
			
	LDA PLAYER.TRANSPORT.ACTIVE
	CMP #$FF
	BEQ .CHECK_TRANSPORT.DONE
	JMP .TRANSPORT_ACTIVE
.CHECK_TRANSPORT.DONE

.WALKING_RULES	
@START	
;APPLY WALKING COLLISSION RULES			
	LDY PLAYER.MOVE.CANDIDATE_TILE_LOC			;***KEEP THIS HERE; .TRANSPORT.ACTIVE CHANGES THE Y-REG VALUE

	;check for map objects
	LDA SCREEN.MO_SPRITE.DATA,Y					;DOES DESTINATION TILE HAVE A MOB MO?	
	CMP #$FF
	BEQ .WALKING.MAP_OBJECT.CHECK.DONE
	JMP PLAYER.MOVE_BLOCKED
.WALKING.MAP_OBJECT.CHECK.DONE

			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; LDA TEMP
			; ;LDX SCREEN.MO_GENERAL.DATA,Y
			; JMP FULL.BRK
			; BRK
; .TEMP
			; LDA TEMP
			
		;****I don't recall why I originally put in the general MO check skip for the push command, but it may be important. 
	; LDA PLAYER.COMMAND.CURRENT
	; CMP #$D0 						;is command (P)ush?
	; BEQ .WALKING.TILE.TESTS			;if yes, ignore general map objects for collision purposes
	

		
	;CHECK FOR GENERAL MAP OBJECTS
	LDX SCREEN.MO_GENERAL.DATA,Y	;load general map object data for current tile location
	CPX #$FF						;is a general map object present?
	BEQ .MO.CHECKS.DONE			;if no, then continue with other checks
	
	LDA MAP_OBJECTS.GENERAL+$3,X	;load data byte of general map object record
	CMP #MO.DOOR.CLOSED.GRE			;is there a closed door on the candidate move location?	
	BCC .IS.MO.PORTCULLIS_LOWERED	;if no, then check to see if it is a lowered portcullis
	CMP #MO.DOOR.CLOSED.LT			;is there a closed door on the candidate move location?	
	BCS .IS.MO.PORTCULLIS_LOWERED	;if no, then check to see if it is a lowered portcullis	
	JMP .PLAYER.MOVE_BLOCKED_STEP			;if yes, then door is an obstacle, move is blocked. 	

.IS.MO.PORTCULLIS_LOWERED
	CMP #MO.PORTCULLIS.LOWERED		;is there a lowered portcullis on the candidate move location?	
	BEQ .PLAYER.MOVE_BLOCKED_STEP			;if yes, then move is blocked
	;**FALLS THROUGH
				;**OPT** Memory. Speed. The collision controls use byte $3 to determine the collission status of some objects. If all the obstacle values for byte $3 were less than or greater than a particular number, that might simplify this section. 
.IS.MO.TRANSPORT
	LDA PLAYER.COMMAND.CURRENT
	CMP #$D0 						;is command (P)ush?
	BNE .PLAYER.MOVE_PREMITTED_STEP		;if no, then move is permitted 

	;IS TRANSPORT OBJECT?
	LDA MAP_OBJECTS.GENERAL+$2,X ;load tile type of map object
	CMP #TILE_ID.HORSE_C						;BRANCH BASED ON THE TYPE OF TRASNPORT MO AT THE PLAYER LOCATION
	BEQ .PLAYER.MOVE_BLOCKED_STEP		;if yes, then push command cannot be executed because a pushable object cannot be pushed onto a transport object
	CMP #TILE_ID.FRIGATE1.1
	BEQ .PLAYER.MOVE_BLOCKED_STEP		;if yes, then push command cannot be executed because a pushable object cannot be pushed onto a transport object
	CMP #TILE_ID.CARAVEL
	BEQ .PLAYER.MOVE_BLOCKED_STEP		;if yes, then push command cannot be executed because a pushable object cannot be pushed onto a transport object
	CMP #TILE_ID.WYVERN
	BEQ .PLAYER.MOVE_BLOCKED_STEP		;if yes, then push command cannot be executed because a pushable object cannot be pushed onto a transport object
	CMP #TILE_ID.SKIFF
	BEQ .PLAYER.MOVE_BLOCKED_STEP		;if yes, then push command cannot be executed because a pushable object cannot be pushed onto a transport object
		;**OPT** Memory. Speed. This section could be shorter if transport objects could be identified by byte $3 (use $01-$03 for skiff values, with $01 being no skiffs, so boats are confuse with $00 as a default value), or if the TILE IDs for transport were in a range. 
	
	JMP .PLAYER.MOVE_PREMITTED_STEP	;if no, then the object is pushable. permitt move. 
.MO.CHECKS.DONE	

.WALKING_RULES.BUILDING ;building map type only
	;is player in building? 
	LDA PLAYER.MAP.LOCATION_TYPE
	; CMP #MAP.TYPE.TOWN_VILLAGE			;IS LOCATION TYPE = BUILDING?
	; BNE .WALKING_RULES.BUILDING.DONE	;if no, skip rules in this section

	;is a building map active
	;**OPT** Memory. Speed. Right now there is only one code for location type, which is used to determine the tile_set to use and for other location-type specific features. There are a lot of the later. It might be a savings if the 
					;there were a tile-set code separate from location type code, and there was a default tile-set picked using the location-type code if tile-set code was set to default. This might require adding another byte to some hex tables
					;but I suspect it would pay off. 
	CMP #MAP.TYPE.BUILDING.GRE 	;is map type = building?
	BCC .WALKING_RULES.BUILDING.DONE		;if no
	CMP #MAP.TYPE.BUILDING.LT	;is map type = building?
	BCS .WALKING_RULES.BUILDING.DONE		;if no
	;**FALLS THROUGH**			 			;if yes
	
	;obstacle test #2
	LDA SCREEN.TILE.DATA,Y			;load tile_type of prospective destination tile	
	CMP #COLLISION_FLAG.WALKING.GRE1.1
	BCC .OBSTACLE_2.TEST.DONE1
	CMP #COLLISION_FLAG.WALKING.LT1.1	
	BCC .PLAYER.MOVE_BLOCKED_STEP
	;**FALLS THROUGH**
.OBSTACLE_2.TEST.DONE1	
.WALKING_RULES.BUILDING.DONE

	;is player in undermap 
	LDA PLAYER.MAP.LOCATION_TYPE
	CMP #MAP.TYPE.UNDERMAP			;IS MAP TYPE = UNDERMAP?
	BNE .WALKING_RULES.UNDERMAP.DONE	;if no, skip rules in this section
	
	;obstacle test #2
	;range #1
	LDA SCREEN.TILE.DATA,Y			;load tile_type of prospective destination tile	
	CMP #COLLISION_FLAG.UNDERMAP.WALKING.GRE1
	BCC .UNDERMAP.OBSTACLE_2.TEST.RANGE1.DONE
	CMP #COLLISION_FLAG.UNDERMAP.WALKING.LT1	
	BCC .PLAYER.MOVE_BLOCKED_STEP
.UNDERMAP.OBSTACLE_2.TEST.RANGE1.DONE	

	;range #2
	CMP #COLLISION_FLAG.UNDERMAP.WALKING.GRE2
	BCC .UNDERMAP.OBSTACLE_2.TEST.RANGE2.DONE
	CMP #COLLISION_FLAG.UNDERMAP.WALKING.LT2	
	BCC .PLAYER.MOVE_BLOCKED_STEP
.UNDERMAP.OBSTACLE_2.TEST.RANGE2.DONE

	;range #3
	CMP #COLLISION_FLAG.UNDERMAP.WALKING.GRE3
	BCC .UNDERMAP.OBSTACLE_2.TEST.RANGE3.DONE
	CMP #COLLISION_FLAG.UNDERMAP.WALKING.LT3	
	BCC .PLAYER.MOVE_BLOCKED_STEP
.UNDERMAP.OBSTACLE_2.TEST.RANGE3.DONE

	;**FALLS THROUGH**
.WALKING_RULES.UNDERMAP.DONE

.WALKING_RULES.ALL	;all map types

	LDA SCREEN.TILE.DATA,Y			;load tile_type of prospective destination tile		
	
	;obstacle test #1
	CMP #COLLISION_FLAG.WALKING.LT1			;ABSOLUTE OBSTACLE?
	BCC .PLAYER.MOVE_BLOCKED_STEP

	
.WATER.TEST	
	;ACC = tile_type of prospective destination tile	
	CMP #COLLISION_FLAG.WALKING.GRE3		;RIVER?	
	BCS .WATER_TEST1.PASS
	JMP .NEXT_TEST
.WATER_TEST1.PASS
	CMP #COLLISION_FLAG.WALKING.LT3			;RIVER?	
	BCC .PLAYER.MOVE_BLOCKED_STEP

.PLAYER.MOVE_PREMITTED_STEP		
.NEXT_TEST	
;NO MORE TESTS FOR NOW
	JMP PLAYER.MOVE_PREMITTED

.PLAYER.MOVE_BLOCKED_STEP
	JMP PLAYER.MOVE_BLOCKED
@END
	
.TRANSPORT_ACTIVE
@START	
	LDY PLAYER.MOVE.CANDIDATE_TILE_LOC
;APPLY TRANSPORT COLLISSION RULES
	LDX PLAYER.TRANSPORT.ACTIVE					;LOAD MO RECORD INDEX OF ACTIVE TRANSPORT
	LDA MAP_OBJECTS.GENERAL+$2,X				;LOAD TILE TYPE OF ACTIVE TRANSPORT
	CMP #TILE_ID.HORSE_C						;IS PLAYER ON A HORSE?
	BEQ .HORSE_RULES							;IF YES, WALKING RULES APPLY
	CMP #TILE_ID.CARAVEL						
	BEQ .WATER_RULES.CARAVEL_STEP
	CMP #TILE_ID.FRIGATE1.1						
	BEQ .WATER_RULES.FRIGATE_STEP
	CMP #TILE_ID.SKIFF						
	BEQ .WATER_RULES.SKIFF_STEP
	CMP #TILE_ID.WYVERN						;IS PLAYER ON A HORSE?
	BEQ .WYVERN_RULES							;IF YES, WALKING RULES APPLY

;	JMP .ERROR									;IF NO EXPECTED TRANSPORT TYPE THEN ERROR OUT. 
.ERROR
;UNEXPECTED TRANSPORT TILE_TYPE IN PLAYER.COLLISSION.CHECK, .TRANSPORT.ACTIVE
;DISABLE.BS_RAM
	JSR PREP.BRK
	BRK
	
.WATER_RULES.SKIFF_STEP
	JMP .WATER_RULES.SKIFF

.WATER_RULES.CARAVEL_STEP
	JMP .WATER_RULES.CARAVEL

.WATER_RULES.FRIGATE_STEP
	JMP WATER_RULES.FRIGATE
@END
	
.WYVERN_RULES
@START	

	LDA PLAYER.TRANSPORT.STATUS					;IS WYVERN FLYING?
	BNE	.PLAYER.MOVE_PREMITTED_STEP				;IF YES, PERMIT MOVE

;WYVERN WALKING RULES
	LDA SCREEN.MO_SPRITE.DATA,Y					;DOES DESTINATION TILE HAVE A MOB MO?	
	CMP #$FF
	BNE .PLAYER.MOVE_BLOCKED_STEP

	LDX SCREEN.MO_GENERAL.DATA,Y	;load general map object data for current tile location
	CPX #$FF						;is a general map object present?
	BEQ .WYVERN.WALKING.TILE.TESTS	;if no, then continue with other checks
	
	LDA MAP_OBJECTS.GENERAL+$3,X	;load data byte of general map object record
	CMP #MO.PORTCULLIS.LOWERED		;is there a lowered portcullis on the candidate move location?	
	BEQ .PLAYER.MOVE_BLOCKED_STEP			;if yes, then move is blocked
	CMP #MO.DOOR.CLOSED.GRE			;is there a closed door on the candidate move location?	
	BCC .PLAYER.MOVE_PREMITTED_STEP				;if no, the map object is either an open door or an object that is not an obstacle (i.e. chair, boat, etc.). This fact overrides all other rules (i.e. if a boat is on a water tile, player can walk onto that tile)	
	CMP #MO.DOOR.CLOSED.LT			;is there a closed door on the candidate move location?	
	BCS .PLAYER.MOVE_PREMITTED_STEP			;if no, the map object is either an open door or an object that is not an obstacle (i.e. chair, boat, etc.). This fact overrides all other rules (i.e. if a boat is on a water tile, player can walk onto that tile)	
	JMP PLAYER.MOVE_BLOCKED				;if yes, then door is an obstacle, move is blocked. 	
	
.WYVERN.WALKING.TILE.TESTS	
	LDA SCREEN.TILE.DATA,Y						;LOAD TILE TYPE OF DESTINATION TILE
				
	CMP #COLLISION_FLAG.WYVERN.LT1			;ABSOLUTE OBSTACLE?
	BCC .PLAYER.MOVE_BLOCKED_STEP
	
	CMP #COLLISION_FLAG.WYVERN.GRE			;WATER?	
	BCS .WATER_TEST1.PASS2
	JMP .ALL.TESTS.PASSED
.WATER_TEST1.PASS2
	CMP #COLLISION_FLAG.WYVERN.LT2			;WATER?	
	BCC .IS_WATER
	JMP PLAYER.MOVE_PREMITTED
@END
	
.HORSE_RULES
@START	
	;**OPT** Memory. I think that there could be some consolidation between the collision routines. For example, Wyvern (walking) and horse rules are the same. 
	LDX #$00						;INIT .LOOP COUNTER
.LOOP
	LDA SCREEN.MO_SPRITE.DATA,Y		;DOES DESTINATION TILE HAVE A MOB MO?	
	CMP #$FF
	BNE .PLAYER.MOVE_BLOCKED_STEP2

	STX SAVED.XREG.LOCAL			;save loop counter
	LDX SCREEN.MO_GENERAL.DATA,Y	;load general map object data for current tile location
	CPX #$FF						;is a general map object present?
	BEQ .MO.CHECKS.DONE2		;if no, then continue with other checks
	
	LDA MAP_OBJECTS.GENERAL+$3,X	;load data byte of general map object record
	CMP #MO.PORTCULLIS.LOWERED		;is there a lowered portcullis on the candidate move location?	
	BEQ .PLAYER.MOVE_BLOCKED_STEP2	;if yes, then move is blocked
	CMP #MO.DOOR.CLOSED.GRE			;is there a closed door on the candidate move location?	
	BCC .PLAYER.MOVE_PREMITTED_STEP2	;if no, the map object is either an open door or an object that is not an obstacle (i.e. chair, boat, etc.). This fact overrides all other rules (i.e. if a boat is on a water tile, player can walk onto that tile)	
	CMP #MO.DOOR.CLOSED.LT			;is there a closed door on the candidate move location?	
	BCS .PLAYER.MOVE_PREMITTED_STEP2				;if no, the map object is either an open door or an object that is not an obstacle (i.e. chair, boat, etc.). This fact overrides all other rules (i.e. if a boat is on a water tile, player can walk onto that tile)	
.PLAYER.MOVE_BLOCKED_STEP2
	JMP PLAYER.MOVE_BLOCKED				;if yes, then door is an obstacle, move is blocked. 			
.MO.CHECKS.DONE2

.HORSE_RULES.BUILDING ;building map type only
	;is player in building?  
	LDA PLAYER.MAP.LOCATION_TYPE
	; CMP #MAP.TYPE.TOWN_VILLAGE			;IS LOCATION TYPE = BUILDING?
	; BNE .HORSE_RULES.BUILDING.DONE		;if no, skip rules in this section

	;is a building map active
	;**OPT** Memory. Speed. Right now there is only one code for location type, which is used to determine the tile_set to use and for other location-type specific features. There are a lot of the later. It might be a savings if the 
					;there were a tile-set code separate from location type code, and there was a default tile-set picked using the location-type code if tile-set code was set to default. This might require adding another byte to some hex tables
					;but I suspect it would pay off. 
	CMP #MAP.TYPE.BUILDING.GRE 	;is map type = building?
	BCC .HORSE_RULES.BUILDING.DONE		;if no
	CMP #MAP.TYPE.BUILDING.LT	;is map type = building?
	BCS .HORSE_RULES.BUILDING.DONE		;if no
	;**FALLS THROUGH**			 		;if yes

	
	LDA SCREEN.TILE.DATA,Y			;load tile_type of prospective destination tile	
	;obstacle test #2
	CMP #COLLISION_FLAG.HORSE.GRE1.1
	BCC .OBSTACLE_2.TEST.DONE2
	CMP #COLLISION_FLAG.HORSE.LT1.1	
	BCC .PLAYER.MOVE_BLOCKED_STEP2
.OBSTACLE_2.TEST.DONE2
.HORSE_RULES.BUILDING.DONE

.HORSE_RULES.ALL ;all map types

	LDX SAVED.XREG.LOCAL			;restore loop counter
	
	LDA SCREEN.TILE.DATA,Y			;load tile_type of prospective destination tile	
				
	;obstacle test #1
	CMP #COLLISION_FLAG.HORSE.LT1			;ABSOLUTE OBSTACLE?
	BCC .PLAYER.MOVE_BLOCKED_STEP2
	
	CMP #COLLISION_FLAG.HORSE.EQ1				;QUICK SAND?
	BEQ .PLAYER.MOVE_BLOCKED_STEP2
	
	CMP #COLLISION_FLAG.HORSE.GRE			;WATER?	
	BCS .WATER_TEST1.PASS3
	JMP .ALL.TESTS.PASSED
.WATER_TEST1.PASS3
	CMP #COLLISION_FLAG.HORSE.LT2			;WATER?	
	BCC .IS_WATER
	JMP PLAYER.MOVE_PREMITTED

.IS_WATER
			 
	LDA PLAYER.MOVE.COUNTER						;IS THIS THE 2ND PLAYER MOVE (FAST HORSE)
	BNE .PLAYER.MOVE_BLOCKED_STEP2							;IF YES, THEN MOVE IS BLOCKED (JUMP CAN ONLY TRAVERSE 1 WATER TILE SO WE DON'T WANT THE COLLISION CONTROLS TO RUN A CHECK ON THE PLAYER POSITION + 2 TILES VIA THE REST OF THE ROUTINE BELOW)
	
	CPX #$01									;IF IN 2ND ITERATION, THAT MEANS PLAYER JUMPED HIS HORSE AND WATER WAS FOUND IN THE PLAYER POSITION + 2 TILES
	BEQ .PLAYER.MOVE_BLOCKED_STEP2

	LDA PLAYER.MOVE.JUMP
	CMP #$01									;DID PLAYER TELL HIS HORSE TO JUMP?
	BNE .PLAYER.MOVE_BLOCKED_STEP2						;IF NO, MOVE BLOCKED

	
	LDY PLAYER.MOVE.CANDIDATE_TILE_LOC2			;LOAD THE TILE ID 2 TILES AWAY FROM PLAYER IN DIRECTION OF MOVE
	INX
				
	JMP .LOOP									;RUN COLLISSION CHECKS AGAIN FOR PLAYER POSIITON + 2 TILES
												;IF NO COLISSIONS ARE FOUND THE COLLISION CHECKS WILL AUTOMATICALLY JMP OUT OF THE LOOP
.PLAYER.MOVE_PREMITTED_STEP2	
.ALL.TESTS.PASSED	
	JMP PLAYER.MOVE_PREMITTED
	

@END	



.WATER_RULES.SKIFF
@START			
	LDA SCREEN.MO_SPRITE.DATA,Y					;DOES DESTINATION TILE HAVE A MOB MO?	
	CMP #$FF
	BNE .PLAYER.MOVE_BLOCKED_STEP2

	LDA SCREEN.MO_GENERAL.DATA,Y				;DOES DESTINATION TILE HAVE A TRANSPORT MO?	
	CMP #$FF
	BEQ .NEXT_TEST3								;IF NO, THEN NEXT TEST
	TAX
	LDA MAP_OBJECTS.GENERAL+$2,X				;LOAD TILE TYPE OF THE TRANSPORT OBJECT IN THE CANDIDATE MOVE LOCATION
	CMP #TILE_ID.FRIGATE1.1						;IS TILE TYPE A FRIGATE?
	BNE .PLAYER.MOVE_BLOCKED_STEP2							;ONLY FRIGATES CAN BE TRAVERSED BY SKIFFS (SO THEY CAN BOARD THE FRIGATE), IF NOT A FRIGATE, MOVE IS BLOCKED
	JMP PLAYER.MOVE_PREMITTED
	
.NEXT_TEST3		
	LDA SCREEN.TILE.DATA,Y						;LOAD TILE TYPE OF DESTINATION TILE
	
	CMP #COLLISION_FLAG.SKIFF.LT				;No land or absolute obstacles
	BCC .PLAYER.MOVE_BLOCKED_STEP2

	CMP #COLLISION_FLAG.SKIFF.EQ2				;No deep water
	BEQ .PLAYER.MOVE_BLOCKED_STEP2

	JMP PLAYER.MOVE_PREMITTED
	
.WATER_RULES.CARAVEL	
;	LDY PLAYER.MOVE.CANDIDATE_TILE_LOC			;***KEEP THIS HERE; .TRANSPORT.ACTIVE CHANGES THE Y-REG VALUE
				
	LDA SCREEN.MO_SPRITE.DATA,Y					;DOES DESTINATION TILE HAVE A MOB MO?	
	CMP #$FF
	BNE PLAYER.MOVE_BLOCKED

	LDA SCREEN.MO_GENERAL.DATA,Y				;DOES DESTINATION TILE HAVE A TRANSPORT MO?	
	CMP #$FF
	BNE PLAYER.MOVE_BLOCKED							;IF SO THAT OVERRIDES ALL OTHER RULES (I.E. IF A BOARD IS ON A WATER TILE, PLAYER CAN WALK ONTO THAT TILE)
		
	LDA SCREEN.TILE.DATA,Y						;LOAD TILE TYPE OF DESTINATION TILE
	
	CMP #COLLISION_FLAG.CARAVEL.LT				;No land, surf/shallow water, or absolute obstacles
	BCC PLAYER.MOVE_BLOCKED
	
	JMP PLAYER.MOVE_PREMITTED
@END


WATER_RULES.FRIGATE
@START	
;#PLAYER.MT.ADJACENT_TILES are constants that refer to the tile locations adjacent to a
;Frigate (multi-tile transport) when it is the active transport for the player. 
;
;The constants map to the screen array as follows, where 0-3 are the tiles #s of the multi-tile shape
; NN
;W01E
;W23E
; SS
;
;0-3 correspond to the screen array locations in the PLAYER.TRANSPORT.MT.TILE_LOCATION(0-3) set of constants



;DETERMINE PLAYER'S CANDIDATE MOVE	
	LDA PLAYER.MOVE.CANDIDATE_TILE_LOC		;THE MOVE.N/S/E/W ROUTINE LOADS THIS VARIABLE WITH THE DESTINATION TILE # BEFORE CALLING THE COLLISION CHECK ROUTINE
	CMP #SCREEN.ARRAY.ADJACENT_NORTH		;PLAYER'S CANDIDATE MOVE IS NORTH
	BEQ .FRIGATE.NORTH
	CMP #SCREEN.ARRAY.ADJACENT_SOUTH		;PLAYER'S CANDIDATE MOVE IS SOUTH
	BEQ .FRIGATE.SOUTH
	CMP #SCREEN.ARRAY.ADJACENT_EAST			;PLAYER'S CANDIDATE MOVE IS EAST
	BEQ .FRIGATE.EAST
	CMP #SCREEN.ARRAY.ADJACENT_WEST			;PLAYER'S CANDIDATE MOVE IS WEST
	BEQ .FRIGATE.WEST

.ERROR
;UNEXPECTD VALUE IN WATER_RULES.FRIGATE ROUTINE, PLAYER.MOVE.CANDIDATE_TILE_LOC VARIABLE
;DISABLE.BS_RAM
	JSR PREP.BRK
	BRK
	
.FRIGATE.NORTH	
	LDA #PLAYER.MT.ADJACENT_TILES.NORTH0
	STA TRANSPORT.MT.COLLISSION.HOPPER+$0
	LDA #PLAYER.MT.ADJACENT_TILES.NORTH1
	STA TRANSPORT.MT.COLLISSION.HOPPER+$1
	JMP .FRIGATE.COLLISSION.CHECK
	
.FRIGATE.SOUTH
	LDA #PLAYER.MT.ADJACENT_TILES.SOUTH0
	STA TRANSPORT.MT.COLLISSION.HOPPER+$0
	LDA #PLAYER.MT.ADJACENT_TILES.SOUTH1
	STA TRANSPORT.MT.COLLISSION.HOPPER+$1
	JMP .FRIGATE.COLLISSION.CHECK
	
.FRIGATE.EAST
	LDA #PLAYER.MT.ADJACENT_TILES.EAST0
	STA TRANSPORT.MT.COLLISSION.HOPPER+$0
	LDA #PLAYER.MT.ADJACENT_TILES.EAST1
	STA TRANSPORT.MT.COLLISSION.HOPPER+$1
	JMP .FRIGATE.COLLISSION.CHECK
	
.FRIGATE.WEST
	LDA #PLAYER.MT.ADJACENT_TILES.WEST0
	STA TRANSPORT.MT.COLLISSION.HOPPER+$0
	LDA #PLAYER.MT.ADJACENT_TILES.WEST1
	STA TRANSPORT.MT.COLLISSION.HOPPER+$1
	;**FALLS THROUGH**
	
.FRIGATE.COLLISSION.CHECK	
	LDX #$00	
	;INIT LOOP COUNTER & INDEX TO TRANSPORT.MT.COLLISSION.HOPPER
.LOOP


			
	LDY TRANSPORT.MT.COLLISSION.HOPPER,X
		
	LDA SCREEN.MO_SPRITE.DATA,Y					;DOES DESTINATION TILE HAVE A MOB MO?	
	CMP #$FF
	BNE PLAYER.MOVE_BLOCKED

			
	LDA SCREEN.MO_GENERAL.DATA,Y				;DOES DESTINATION TILE HAVE A TRANSPORT MO?	
	CMP #$FF
	BNE PLAYER.MOVE_BLOCKED							;IF SO THAT OVERRIDES ALL OTHER RULES (I.E. IF A BOARD IS ON A WATER TILE, PLAYER CAN WALK ONTO THAT TILE)
	
	LDA SCREEN.TILE.DATA,Y						;LOAD TILE TYPE OF DESTINATION TILE	
	CMP #COLLISION_FLAG.FRIGATE.LT				;No land or surf/shallow/medium water
	BCC PLAYER.MOVE_BLOCKED

			; CPX #$01
			; BNE .TEMP
			; LDY TRANSPORT.MT.COLLISSION.HOPPER
			; LDA SCREEN.TILE.DATA,Y
			; LDY TRANSPORT.MT.COLLISSION.HOPPER+$1
			; LDA SCREEN.TILE.DATA,Y
			; TAX
			
			; JMP FULL.BRK
			; BRK
; .TEMP

	INX
	CPX #$02
	BNE .LOOP
	
	JMP PLAYER.MOVE_PREMITTED

;	JMP PLAYER.MOVE_PREMITTED
;***OPT** Memory. Consolidate the move permitted/blocked routines below with the ones further up. 
	;***FALLS THROUGH

; PLAYER.MOVE_PREMITTED	
	; LDA #$00		;PERMITTS MOVE BY RETURNING $00 IN ACC

			; ; PLA
			; ; TAX
			; ; PLA
			; ; TAY
			; ; JMP FULL.BRK
			; ; BRK
	; RTS
	
; PLAYER.MOVE_BLOCKED
			; ; LDA #$BB
			; ; JMP FULL.BRK
			; ; BRK
			
	; LDA #$01		;BLOCKS MOVE BY RETURNING $01 IN ACC
	; RTS

@END


PLAYER.MOVE_BLOCKED
	
	LDA #$01		;BLOCKS MOVE BY RETURNING $01 IN ACC
	STA SAVED.ACC.LOCAL ;save return value
	JMP PLAYER.COLLISSION.EXIT
	
PLAYER.MOVE_PREMITTED	
	LDA #$00		;PERMITTS MOVE BY RETURNING $00 IN ACC
	STA SAVED.ACC.LOCAL ;save return value, because ACC will get clobbered when registeres are restored from the stack
	
	;**FALLS THROUGH**
	
PLAYER.COLLISSION.EXIT

;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX
	
	LDA SAVED.ACC.LOCAL ;restore return value
	
	RTS
	
@END

MAP.EXIT.CHECK
@START
;=====================SUBROUTINE DOCUMENTATION====================================
;
;See MAP.ENTER.CHECK for more detailed doc on the concept of enterable mapss.
;
;This subroutine is triggered when a zone transition is detected.
;Since buildings are all 9 zones in size (currently), a zone
;transition is effectively an exit from the building. 
;
;This subroutine simply returns the players positional variables
;back to their values before entering the map the player
;is now exitting from, which are stored in PLAYER.MAP.LOCATION.LAST(9)
;
;=================================================================================

;IS PLAYER IN A BUILDING? IF SO, TRANSITION ZONES OR EXIT BUILDING?			
;(this section was setup in contemplation for buildings larger than 9 loader zones
;which would require zone transitions to now always trigger an exit. currently
;there is no support for buildings larger than 9 zones)
;
	LDA PLAYER.MAP.LOCATION_TYPE
	CMP #MAP.TYPE.SURFACE
	BEQ .LOCATION_TYPE.SURFACE
	; CMP #MAP.TYPE.TOWN_VILLAGE				;TYPE 1 ONLY HAS 9 ZONES, SO EXIT ON 1ST ZONE TRANSITION
	; BEQ .LOCATION.EXIT				
	; CMP #MAP.TYPE.CASTLE			;NOT YET IMPLEMENTED
	; BEQ .LOCATION_TYPE.BUILDING2
	CMP #MAP.TYPE.UNDERMAP
	BEQ .LOCATION_TYPE.UNDERMAP
	
	CMP #MAP.TYPE.BUILDING.GRE 	;is map type = building?
	BCC .MAP.TYPE_CHECK.DONE		;if no
	CMP #MAP.TYPE.BUILDING.LT	;is map type = building?
	BCS .MAP.TYPE_CHECK.DONE		;if no
	JMP .LOCATION.EXIT			;if yes	(buildings only has 9 zones, so exit if zone transition triggered)
.MAP.TYPE_CHECK.DONE
	
.LOCATION.ERROR
;UNEXPECTED MAP CODE (PLAYER.MAP.LOCATION_TYPE) IN .UPDATE.ZONE.INFORMATION 
	JSR PREP.BRK
	BRK

.LOCATION_TYPE.SURFACE ; SURFACE, RETURN TO MOVE.NORTH/SOUTH/EAST/WEST
.LOCATION_TYPE.UNDERMAP	; UNDERMAP, ""
	RTS
	
.LOCATION_TYPE.BUILDING2
	;INSERT CHECK TO VERIFY EXIT
	
	;RTS IF NOT READY TO EXIT MAP
	RTS
	
.LOCATION.EXIT			
;UPDATE PLAYER MAP LOCATION VARIABLES WITH LAST LOCATION
;note: this data was recorded when the player entered the map now being exitted


		
	LDA PLAYER.MAP.LOCATION.LAST+$0
	STA PLAYER.MAP.LOCATION
	
	LDA PLAYER.MAP.LOCATION.LAST+$1
	STA PLAYER.MAP.LOCATION_TYPE

	LDA PLAYER.MAP.LOCATION.LAST+$2
	STA GMAP.X
	STA PARM.GMAP.X
	
	LDA PLAYER.MAP.LOCATION.LAST+$3
	STA GMAP.Y
	STA PARM.GMAP.Y	

	JSR MAP.UPDATE.POSITION ;set RMAP.X/Y, RMAP(2), PLAYER.WZONE and SS Flags
			
	; LDA PLAYER.MAP.LOCATION.LAST+$4
	; STA RMAP+$0

	; LDA PLAYER.MAP.LOCATION.LAST+$5
	; STA RMAP+$1

	; LDA PLAYER.MAP.LOCATION.LAST+$6
	; STA RMAP.X

	; LDA PLAYER.MAP.LOCATION.LAST+$7
	; STA RMAP.Y

	; LDA PLAYER.MAP.LOCATION.LAST+$8
	; STA PLAYER.WMAP.ZONE

	;SET GMAP.X/Y.LAST TO GMAP
	;(this is to avoid having values on two different maps)
	LDA GMAP.X
	STA GMAP.X.LAST
	LDA GMAP.Y
	STA GMAP.Y.LAST

	; ;SET SS_FLAGS BASED ON RMAP.X/Y
	; JSR MAP.CALCULATE.SS_FLAGS
	
	JSR LOAD.NEW.LOCATION
	
	PLA				;REMOVE RTS FROM THE STACK (THIS ROUTINE WAS JSR FROM MOVE.NORTH/SOUTH/EAST/WEST BUT WE ARE GOING TO RETURN TO THE GAME LOOP DIRECTLY)
	PLA
	
	JMP GAME.LAUNCH	;REDRAW ENTIRE SCREEN
	
	
	
	
@END

MAP.ENTER.CHECK			
@START
;=====================SUBROUTINE DOCUMENTATION====================================
;
;This is the primary documentation for enterable maps as a concept. 
;as well as the documentation for the MAP.ENTER.CHECK subtroutine. 
;
;-MAP.ENTER.CHECK subroutine docs
;
;This subroutine is called at by MOVE.ROUTINES.COMMON after the page flip
;so that the player sees their icon standing on the enterable map icon
;while the game is loading the enterable map, if one exists.
;
;No key press is required to enter a map (except in the future the game 
;may ask "you are about to enter a new area, proceed <Y/N>?")
;
;By having MOVE.ROUTINES.COMMON call this routine, the game
;automatically detects if a player has moved onto an enterable map.
;
;The first thing the routine does is it connects the 
;MAP.WORMHOLES.POINTER to an array which contains
;the x,y coordinates of the maps which are enterable from the player's
;current map code. For example map 0 (surface) has a list of enterable maps (buildings, for eexample).
;However, maps can be nested (see below) 
;
;Next, the subroutine seaches the array to see if the GMAP.X/Y
;of the player matches the GMAP.X/Y of an enterable map.
;
;If no, then this subroutines exits.
;
;If yes, then this subroutine makes the calls necessary
;to enter the map and sets the variables necessary
;so that the players's position in the current map is
;remembered, and the player's starting position in the new map 
;is setup. 
;
;-ENTERABLE MAPS (CONCEPT)
;
;Defined entrance/exits have a defined starting position in the destination map
;Defined entrance/exits are automatically triggered if the player walks on their GMAP.X/Y
;Default exits are automatically triggered if a zone transition is triggered (see map.EXIT.CHECK). 
;	The player is returned to the map they entered from as recorded in (PLAYER.MAP.LOCATION.LAST(9))
;Defined entrance/exits entrance/exits can be nested. 
;Maps can have multiple entrances/exits by using defined entrance/exits.
;Maps use a default starting position if no defined starting position exists
;	for the entrance/exit used. 
;Building maps have default entrance/exits enabled. 	
;The undermap has default entrance/exits disabled. This is because
; 	undermap exits can't occur via zone transition. Technically 
;	it could be setup that way, but it doesn't fit the "dungeon"
;	scenario where exists are via a specific tile (like a ladder), 
;	not by walking off the edge of the map, which fits in a 
;	building scenario.
;
;-What is Map Code & Map Type?
;They are both fields of each map record in MAP.LOCATIONS_xx 
;
;Map Code is a unique identifier for each enterable map. When
;a GMAP.X/Y match with the player occurs, the associated 
;map code tells LOAD.NEW.LOCATION which map to load. 
;
;The assocatiated map type code is use by LOAD.NEW.LOCATION
;to determine if new shape tables should be loaded.
;Map Type is a catagory which map codes are part of. 
;For example, surface has it's own map type, buildings is a map type.
;
;The reason some map codes (i.e. surface) have their own 
;map type is because map type is sometimes used
;by the game to understand generally what kind of map the player is
;on. For example, some commands behave differently when the player
;has the surface map type set vs. if the player has the building map type set. 
;
;
;-Nested Map Entrances
;For example map 0 (surface) has a list of enterable maps (buildings, for example).
;Perhaps the player enters map 1 from map 0. Map 1
;could have another set of enterable maps which the player can access from map 1, which may or may not
;be accessible from map 0.
;
;-Linked Maps (i.e to create large towns/castles)
;Maps can be linked. Essentially linking maps is just
;another way of describing nested maps.
;
;However, the concept behind linking maps is to create
;the illusion to the player that they are in one large map. 
;
;For example, a large town (perhaps the capital city) has four quarters
;surrounded by walls and a guard check point at the entance/exit to each quarter.
;This forces the player to walk on a nested map trigger setup
;at the entrance and exit of each quarter of the city. 
;
;Another example. A castle has a coutyard (1 map). 
;At the north end (or whichever end) of the courtyard has a gate leading to the keep.
;The gate is a nested map trigger. Once in the keep, perhaps there are several
;levels/floors, accessed by ladders/stairs which are also nested map triggers. 
;
;
;-Adding Maps
;See "ADDING MAPS DOCS" in the comment just below. 
;
;=================================================================================

;=====================ADDING MAPS DOCS====================================
;-ENABLE nested maps on the source maps if needed 
;	(for example, to add a 2nd floor to a building, the building must have nested maps enabled)
;	add .CURRENT_LOCATION(#) section to .IS.LOCATION.ENTERABLE below, to connect the pointer to the array storing the enterable maps which are enterable from
;	map #. 
;
;-ADD additional map
;	Add GMAP.X/Y coordinates to the MAP.LOCATIONS_xx array for the map from which
;	the new map is enterable. Also assign it a map code and map type. 
;	updated Map_Objects.XLS (my_code/rpg project/map) with the new map code and map type 
;	
;	Except for buildings (which use a default start position), add the starting postion of the new map to MAP.WORMHOLES.START.POSITION_DATA array
;
;
;	;----update 9/8/2016; identifying track & sector no longer necessary. This doc sectin needs updating to refelect the integration of the prodos bootloader and I/O driver
;	At a minimum each map needs it's own map. To set this up
;			a) create a new map data file (save an existing one as a new file name, change the target file name)
;			b) add an include statement for the new data file to the end of game_loop.ASM
;			c) add the new target file to go2p.bat & go2hd.bat
;			d) (not needed when using OpenDir) run go1.bat,then open the target file for the new map with CiderPress
;					Observe the start track/sector and number of sectors in ciderpress.
;			e) Plumb in the new map to LOAD.NEW.SHAPES & LOAD.NEW.MAP (MAP_TOOLS.ASM)
;			   this used to require setting up constants for the new data file in offloaded_variables.ASM back when we used an RWTS bootloader, but it is no longer required with the ProDOS bootloader.
; END OF PRIMRY ADD MAP DOCS, the next section summarizes the special case of linked maps.
;
;---ADD Linked Maps--- (i.e to create large towns/castles)
;	a) Setup each map as normal (see ADD additional map above)
;	b) ENABLE nested maps for each map as described above.
;	c) setup a nested map at each entrance/exit between the linked maps. 
;**See Linked maps in subroutine documentation above 
;for a more detailed explanation of linked maps including
;examples of their use. 
;
;
;-Maps with Multiple Entrance/Exists
;
;in .LOCATION.ENTERABLE, see comment titled ";FUTURE: SUPPORT FOR MAPS WITH MULTIPLE ENTRANCE/EXITS"
;
;
;=================================================================================




				

			
	;RTS	;**UNCOMMENT TO DISABLE MAP ENTER CHECK**
			
.IS.LOCATION.ENTERABLE			;(at the current gmap.x, gmap.y location on the map) 			
@START
	LDA PLAYER.MAP.LOCATION
	CMP #$00
	BEQ .CURRENT_LOCATION0
	
	CMP #$01
	BEQ .CURRENT_LOCATION1
	
	CMP #$02
	BEQ .CURRENT_LOCATION2
	
	CMP #$03
	BEQ .CURRENT_LOCATION3
	
	CMP #$04
	BEQ .CURRENT_LOCATION4

	CMP #$05
	BEQ .CURRENT_LOCATION5
	
	CMP #$40
	BEQ .CURRENT_LOCATION40
	
	JMP .ENTERABLE_LOCATION_FOUND.NO

.CURRENT_LOCATION0
	LDA #MAP.WORMHOLES_00
	STA MAP.WORMHOLES.POINTER
	LDA /MAP.WORMHOLES_00
	STA MAP.WORMHOLES.POINTER+$1

	JMP .SEARCH.ENTERABLE.LOCATIONS

.CURRENT_LOCATION1
	LDA #MAP.WORMHOLES_01
	STA MAP.WORMHOLES.POINTER
	LDA /MAP.WORMHOLES_01
	STA MAP.WORMHOLES.POINTER+$1

	JMP .SEARCH.ENTERABLE.LOCATIONS

.CURRENT_LOCATION2
	LDA #MAP.WORMHOLES_02
	STA MAP.WORMHOLES.POINTER
	LDA /MAP.WORMHOLES_02
	STA MAP.WORMHOLES.POINTER+$1
	
	JMP .SEARCH.ENTERABLE.LOCATIONS

.CURRENT_LOCATION3
	LDA #MAP.WORMHOLES_03
	STA MAP.WORMHOLES.POINTER
	LDA /MAP.WORMHOLES_03
	STA MAP.WORMHOLES.POINTER+$1
	
	JMP .SEARCH.ENTERABLE.LOCATIONS

.CURRENT_LOCATION4
	LDA #MAP.WORMHOLES_04
	STA MAP.WORMHOLES.POINTER
	LDA /MAP.WORMHOLES_04
	STA MAP.WORMHOLES.POINTER+$1
	
	JMP .SEARCH.ENTERABLE.LOCATIONS

.CURRENT_LOCATION5
	LDA #MAP.WORMHOLES_05
	STA MAP.WORMHOLES.POINTER
	LDA /MAP.WORMHOLES_05
	STA MAP.WORMHOLES.POINTER+$1
	
	JMP .SEARCH.ENTERABLE.LOCATIONS

	
.CURRENT_LOCATION40
	LDA #MAP.WORMHOLES_40
	STA MAP.WORMHOLES.POINTER
	LDA /MAP.WORMHOLES_40
	STA MAP.WORMHOLES.POINTER+$1
	
	JMP .SEARCH.ENTERABLE.LOCATIONS
	
.SEARCH.ENTERABLE.LOCATIONS
	LDY #$00
	LDX #$00

.LOOP.SEARCH
;.LOAD_RECORD
;(a record is loaded and then searched for a specific reason,
;which I don't remember offhand). I think it's because indirect index by Y doesn't allow hard coded offsets
;which are used to access the individual fields in the record once selected (for example, (MAP.WORMHOLES.RECORD+$1), Y doesn't work. 
	LDA (MAP.WORMHOLES.POINTER),Y
	STA MAP.WORMHOLES.RECORD,X
	INX										;INCREMENT LOAD COUNTER
	INY										;INCREMENT MAP.LOCATIONS_xx INDEX, RECORD LENGTH IS 4 BYTES, WHICH WILL BE THE NET INCREMENT UPON COMPLETION OF THIS LOAD LOOP. THE RECORD INDEX IS DOUBLEING AS FIELD INDEX

	CPX #MAP.WORMHOLES.RECORD.LENGTH
	BNE .LOOP.SEARCH
	
;.CHECK.RECORD
	;LDX #$00
	LDA MAP.WORMHOLES.RECORD+$0
	CMP GMAP.X
	BNE .NEXT.RECORD
	LDA MAP.WORMHOLES.RECORD+$1
	CMP GMAP.Y
	BEQ .LOCATION.ENTERABLE
.NEXT.RECORD
	LDA MAP.WORMHOLES.RECORD+$2
	CMP #$FF							;CHECK FOR STOP VALUE
	BEQ .ENTERABLE_LOCATION_FOUND.NO

	LDX #$00							;INIT LOAD COUNTER TO ZERO	

	JMP .LOOP.SEARCH	


.ENTERABLE_LOCATION_FOUND.NO
	RTS ;return to MOVE.COMMON

@END
	
.LOCATION.ENTERABLE


			
@START
;IS PLAYER ENTERING THE UNDERMAP?
	LDA MAP.WORMHOLES.RECORD+$3		;load map type of the map player is trying to enter
	CMP #MAP.TYPE.UNDERMAP		;is it the map type "undermap"?
	BCS .WALKING.CHECK				;if yes, proceed to verify the player has no transport active
	JMP .ENTRANCE.PERMITTED			;if no, then entrance is permited
	

.WALKING.CHECK	
;IS PLAYER WALKING?
	LDX PLAYER.TRANSPORT.ACTIVE		;load transport mo record index
	CPX #$FF						;is player walking?
	BEQ .ENTRANCE.PERMITTED			;if yes, then entrance is permitted
									;if no, then entrance is not permited, return to MOVE.COMMON
	JSR PLAY.SOUND.DUMB_ASS
	RTS ;return to MOVE.COMMON

@END

.ENTRANCE.PERMITTED
@START


				
				
			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK2
			; CMP #$03
			; BNE .TEMP1			
			; ; LDA PLAYER.MAP.LOCATION_TYPE
			; ; CMP #$02
			; ; BEQ .TEMP2
			; LDA GMAP.X
			; STA $BE00
			; LDA GMAP.Y
			; STA $BE01			
			; LDA #$AA
			; ldx PLAYER.MAP.LOCATION_TYPE			
			; JSR FULL.BRK
; .TEMP1
			; LDA TEMP

			
;RECORD DATA ON PLAYERS CURRENT LOCATION BEFORE ENTERING NEW MAP
;this data is recorded so that if player exits new map by walking off map
;(i.e. via a zone transition detection), then this data is used to return the player to their map (and GMAP.X/Y) prior to entering. 																	


	;**HRCG** ask player if they want to enter new area? Do we want players to be asked that if they are in an undermap type map? (probably not)
	;unless in underworld (check map type), then don't ask. 

;IS THIS A BUILDING-BUILDING ENTER/EXIT?  (i.e. an upper/lower floor of the same location?)
	LDA MAP.WORMHOLES.RECORD+$3 ;load destination map type
	; CMP #MAP.TYPE.TOWN_VILLAGE	;is player entering a building type map?
	; BNE .UPDATE.LAST.LOCATION.DATA
	
	;is a building map active
	;**OPT** Memory. Speed. Right now there is only one code for location type, which is used to determine the tile_set to use and for other location-type specific features. There are a lot of the later. It might be a savings if the 
					;there were a tile-set code separate from location type code, and there was a default tile-set picked using the location-type code if tile-set code was set to default. This might require adding another byte to some hex tables
					;but I suspect it would pay off. 
	CMP #MAP.TYPE.BUILDING.GRE 	;is map type = building?
	BCC .UPDATE.LAST.LOCATION.DATA		;if no
	CMP #MAP.TYPE.BUILDING.LT	;is map type = building?
	BCS .UPDATE.LAST.LOCATION.DATA		;if no
	;**FALLS THROUGH**			 		;if yes

	LDA PLAYER.MAP.LOCATION_TYPE	
	; CMP #MAP.TYPE.TOWN_VILLAGE	;is player entering a building type map from a building type map (i.e. an upper/lower floor of the same location?)
	; BEQ .BUILDING_TO_BUILDING	;if yes, then don't udpate the saved last map data because we want the player to be able to return to the entrance used from the surface map or undermap to enter the series of building type map. 

	;is a building map active
	;**OPT** Memory. Speed. Right now there is only one code for location type, which is used to determine the tile_set to use and for other location-type specific features. There are a lot of the later. It might be a savings if the 
					;there were a tile-set code separate from location type code, and there was a default tile-set picked using the location-type code if tile-set code was set to default. This might require adding another byte to some hex tables
					;but I suspect it would pay off. 
	CMP #MAP.TYPE.BUILDING.GRE 	;is map type = building?
	BCC .MAP_TYPE.CHECK.DONE		;if no
	CMP #MAP.TYPE.BUILDING.LT	;is map type = building?
	BCS .MAP_TYPE.CHECK.DONE		;if no
	JMP .BUILDING_TO_BUILDING	 		;if yes
.MAP_TYPE.CHECK.DONE
	;**FALLS THROUGH**
	
.UPDATE.LAST.LOCATION.DATA
;save player's GMAP, RMAP, WZONE which will be used the 
;next time the player exits a building type map via zone transition (LOCATION.EXIT.CHECK)	

			
	LDA PLAYER.MAP.LOCATION
	STA PLAYER.MAP.LOCATION.LAST+$0
	
	LDA PLAYER.MAP.LOCATION_TYPE
	STA PLAYER.MAP.LOCATION.LAST+$1
	
	LDA GMAP.X
	STA PLAYER.MAP.LOCATION.LAST+$2
	
	LDA GMAP.Y
	STA PLAYER.MAP.LOCATION.LAST+$3
	
	; LDA RMAP+$0
	; STA PLAYER.MAP.LOCATION.LAST+$4
	
	; LDA RMAP+$1
	; STA PLAYER.MAP.LOCATION.LAST+$5
	
	; LDA RMAP.X
	; STA PLAYER.MAP.LOCATION.LAST+$6
	
	; LDA RMAP.Y
	; STA PLAYER.MAP.LOCATION.LAST+$7
	
	; LDA PLAYER.WMAP.ZONE
	; STA PLAYER.MAP.LOCATION.LAST+$8

	;** FALLS THROUGH. this code section is invoked for non-building to building entrance/exists, but we process the .BUILDING_TO_BUILDING section anyway because PLAYER.MAP.LOCATION.LAST+$9 is expected to contain the location type by .USE.DEFAULT.POSITION, for all scenarios. 
	
.BUILDING_TO_BUILDING	
	LDA PLAYER.MAP.LOCATION_TYPE
	STA PLAYER.MAP.LOCATION.LAST+$9
	
.UPDATE.PLAYER.LOCATION	
;UPDATE PLAYER MAP VARIABLES WITH NEW MAP DATA
	LDA MAP.WORMHOLES.RECORD+$2
	STA PLAYER.MAP.LOCATION					;STORES THE MAP CODE OF THE PLAYER'S CURRENT MAP

	LDA MAP.WORMHOLES.RECORD+$3
	STA PLAYER.MAP.LOCATION_TYPE			;BUILDING (various sizes), DUNGEON WORLD ETC 


	JSR DETERMINE.SUNLIGHT.STATUS			;determine if sun is visible based on time of day and map type


		
FIND.STARTING.POSITION
@START



;=====================CODE-SECTION DOCUMENTATION====================================
;
;This code section determines the player's starting position
;on the new map by checking to see if the map
;has a defined starting position and if not the default position
;for the map_type is used. 
;
;-Check For Defined Starting Position 
;This is done by iterating the MAP.WORMHOLES.START.POSITION_DATA 
;array. A defined starting positon is found if there is 
;a match between the destination map code in the array and
;the map code of the entrance/exit trigger activated (MAP.WORMHOLES.RECORD)
;and if the source gmap.X/Y in the array match the current GMAP
;of the player. 
;
;-Default Starting Position
;If no map code and GMAP.X/Y match is found, then the 
;entrance/exit triggered doesn't have a defined starting position
;The default start position for the map_type of the map
;being entered is used. 
;
;based on the other fields in the record for that map code. 
	;some other variables values will be required to be calculated
	;see MAP OBJECTS.XLS for details on which fields must be calculated and
	;for a datagram on MAP.WORMHOLES.START.POSITION_DATA.
	;
	;if no map code match is found use the default start position below
	;which is determined by the source and destination map type.
	;
	;note: if an exit occurs from a map with multiple entrances,
	;nothing special needs to happen because .LOCATION.EXIT will
	;return the player to the last map and the last position at that map. 
	;**UPDATE: I don't this this note is correct. The result would be that the player 
	;would be returned to the same GMAP.X/Y on the prior map regardless of which exit was used.
	;**UPDATE2: I don't understand why I made the comment in UPDATE. I have multiple entrances to the test town from the surface map.
	;and when the player exits the town he appears at the entrance used on the surface map. The building exit function
	;returns the player to the coordinates used upon entrance, I know for sure this data is saved. 
;=================================================================================

.SEARCH.LOCATION.STARTING.POSITIONS
	LDY #$00
	LDX #$00

.LOOP.SEARCH
;.LOAD_RECORD
;(a record is loaded and then searched because that's how it was done
;in the enter maps loop above). See comments in that loop for more info.
;even though we don't use indirect index by Y, we might in the future if the number of map entrance/exists exceeds a page of memory.
			
	LDA MAP.WORMHOLES.START.POSITION_DATA,Y
	STA MAP.WORMHOLES.SP.RECORD,X
	INX										;INCREMENT LOAD COUNTER
	INY										;INCREMENT MAP.LOCATIONS_xx INDEX, RECORD LENGTH IS 4 BYTES, WHICH WILL BE THE NET INCREMENT UPON COMPLETION OF THIS LOAD LOOP. THE RECORD INDEX IS DOUBLEING AS FIELD INDEX

	CPX #MAP.WORMHOLES.SP.RECORD.LENGTH
	BNE .LOOP.SEARCH
	
;.CHECK.RECORD
			
	LDA MAP.WORMHOLES.SP.RECORD+$0
	CMP MAP.WORMHOLES.RECORD+$2
	BNE .NEXT.RECORD
	LDA MAP.WORMHOLES.SP.RECORD+$1
	CMP GMAP.X
	BNE .NEXT.RECORD
	LDA MAP.WORMHOLES.SP.RECORD+$2
	CMP GMAP.Y
	BEQ .START.POSITION.FOUND
.NEXT.RECORD
	LDA MAP.WORMHOLES.SP.RECORD+$0
	CMP #$FF							;check for stop value
	BEQ .USE.DEFAULT.POSITION			;if stop value is found then there was no starting position found for the destination location code which matches the GMAP.X/Y of the entrance/exit triggered by the player in the source location. 

	LDX #$00							;init load counter to zero	


	JMP .LOOP.SEARCH
	
.START.POSITION.FOUND
;CALCULATE RMAP.X/Y OF STARTING POSITION
		LDA MAP.WORMHOLES.SP.RECORD+$3	;contains GMAP.X on destination map
		STA PARM.GMAP.X
		STA GMAP.X
		LDA MAP.WORMHOLES.SP.RECORD+$4	;contains GMAP.Y on destination map
		STA PARM.GMAP.Y
		STA GMAP.Y

	JSR MAP.UPDATE.POSITION ;set RMAP.X/Y, RMAP(2), PLAYER.WZONE and SS Flags

	; JSR CONVERT.GMAP_XY.RMAP_XY
		; LDA RETURN.RMAP.X
		; STA RMAP.X
		; LDA RETURN.RMAP.Y
		; STA RMAP.Y		
			
; ;CALCULATE RMAP OF STARTING POSITION
		; LDA RMAP.X
		; STA PARM.RMAP.X
		; LDA RMAP.Y
		; STA PARM.RMAP.Y
	; JSR CONVERT.RMAP_XY.RMAP
		; LDA RETURN.RMAP
		; STA RMAP
		; LDA RETURN.RMAP+$1
		; STA RMAP+$1
		
; ;CALCULATE WZONE OF STARTING POSITION
		; LDA GMAP.X						;now contains the destination GMAP.X
		; STA PARM.GMAP.X
		; LDA GMAP.Y						;now contains the destination GMAP.Y
		; STA PARM.GMAP.Y
	; JSR CONVERT.GMAP_XY.WZONE
		; LDA RETURN.WZONE
		; STA PLAYER.WMAP.ZONE
		
	JMP .LOAD.NEW.LOCATION	
	
.USE.DEFAULT.POSITION
	LDA PLAYER.MAP.LOCATION_TYPE	
	; CMP #MAP.TYPE.TOWN_VILLAGE
	; BEQ .DESTINATION.LOCATION_TYPE.BUILDING	
	CMP #MAP.TYPE.UNDERMAP
	BEQ .DESTINATION.LOCATION_TYPE.UNDERMAP 
	CMP #MAP.TYPE.BUILDING.GRE 	;is map type = building?
	BCC .MAP.TYPE_CHECK.DONE		;if no
	CMP #MAP.TYPE.BUILDING.LT	;is map type = building?
	BCS .MAP.TYPE_CHECK.DONE		;if no
	JMP .DESTINATION.LOCATION_TYPE.BUILDING		;if yes
.MAP.TYPE_CHECK.DONE
	
.ERROR
;UNEXPECTED MAP_TYPE CODE IN PLAYER.MAP.LOCATION_TYPE
	JMP FULL.BRK
	BRK
	
.DESTINATION.LOCATION_TYPE.BUILDING
;USED DEFAULT STARTING POSITION IN NEW MAP
	LDA PLAYER.MAP.LOCATION.LAST+$9 ;load the map type of the source map (which player is exiting)
	; CMP #MAP.TYPE.TOWN_VILLAGE	;is player entering a building type map from a building type map (i.e. an upper/lower floor of the same location?)
	; BEQ .LOAD.NEW.LOCATION 	;if yes, start position is same as source position
	;is a building map active
	;**OPT** Memory. Speed. Right now there is only one code for location type, which is used to determine the tile_set to use and for other location-type specific features. There are a lot of the later. It might be a savings if the 
					;there were a tile-set code separate from location type code, and there was a default tile-set picked using the location-type code if tile-set code was set to default. This might require adding another byte to some hex tables
					;but I suspect it would pay off. 
	CMP #MAP.TYPE.BUILDING.GRE 	;is map type = building?
	BCC .MAP.TYPE_CHECK.DONE2		;if no
	CMP #MAP.TYPE.BUILDING.LT	;is map type = building?
	BCS .MAP.TYPE_CHECK.DONE2		;if no
	JMP .LOAD.NEW.LOCATION 			;if yes
.MAP.TYPE_CHECK.DONE2
	
.NON_BUILDING.TO.BUILDING	;if no, set start position to bottom/center of RZONE

		; LDA #$AA
		; JMP FULL.BRK
		; LDX PLAYER.MAP.LOCATION.LAST+$2
		; LDY PLAYER.MAP.LOCATION.LAST+$3
		; BRK	
		
	LDA #$18
	STA GMAP.X
	STA PARM.GMAP.X
	
	LDA #$26
	STA GMAP.Y
	STA PARM.GMAP.Y

	JSR MAP.UPDATE.POSITION ;set RMAP.X/Y, RMAP(2), PLAYER.WZONE and SS Flags


		
	; LDA #$38
	; STA RMAP+$0

	; LDA #$07
	; STA RMAP+$1

	; LDA #$18
	; STA RMAP.X

	; LDA #$26
	; STA RMAP.Y

	; LDA #$11
	; STA PLAYER.WMAP.ZONE
	
	JMP .LOAD.NEW.LOCATION

.DESTINATION.LOCATION_TYPE.UNDERMAP
;USED DEFAULT STARTING POSITION IN NEW MAP
	LDA #$38
	STA GMAP.X

	LDA #$47
	STA GMAP.Y


	JSR MAP.UPDATE.POSITION ;set RMAP.X/Y, RMAP(2), PLAYER.WZONE and SS Flags

	; LDA #$68
	; STA RMAP+$0

	; LDA #$04
	; STA RMAP+$1

	; LDA #$18
	; STA RMAP.X

	; LDA #$17
	; STA RMAP.Y

	; LDA #$23
	; STA PLAYER.WMAP.ZONE
	;**FALLS THROUGH**

@END
	
.LOAD.NEW.LOCATION


				
				
				
	;SET GMAP.X/Y.LAST TO GMAP
	;(this is to avoid having values on two different maps)
	LDA GMAP.X
	STA GMAP.X.LAST
	LDA GMAP.Y
	STA GMAP.Y.LAST
		
	;SET SS_FLAGS BASED ON RMAP.X/Y
	JSR MAP.CALCULATE.SS_FLAGS

	

				
				
				
	;RESET MOB GENERATION
	LDA #$00
	STA MOB.GEN.QUEUE

	LDA #$01							;SET TURN TO MOB
	STA GAME.MOB_GEN.CONTROL			
	STA GAME.TURN.CONTROL
	
	;JSR SAVE.SPR.DATA		;Save Sprite data for the current map
	JSR LOAD.NEW.LOCATION					;LOAD NEW SHAPE TABLES AND MAP




				
	LDA PLAYER.MAP.LOCATION_TYPE		
	; CMP #MAP.TYPE.TOWN_VILLAGE
	; BNE .EXIT
	
	;is a building map active
	;**OPT** Memory. Speed. Right now there is only one code for location type, which is used to determine the tile_set to use and for other location-type specific features. There are a lot of the later. It might be a savings if the 
					;there were a tile-set code separate from location type code, and there was a default tile-set picked using the location-type code if tile-set code was set to default. This might require adding another byte to some hex tables
					;but I suspect it would pay off. 
	CMP #MAP.TYPE.BUILDING.GRE 	;is map type = building?
	BCC .EXIT							;if no
	CMP #MAP.TYPE.BUILDING.LT	;is map type = building?
	BCS .EXIT							;if no
	;**FALLS THROUGH**			 		;if yes

	
;MAP_TYPE = BUILDING SPECIFIC INIT ROUTINES			
	JSR NPC.INIT							;set GMAP.X/Y and Active Anchor of NPCs, based on current time. 
			


				
.EXIT

;.DEBUG.PRINT_0 ;same as above but with auto-increment to ASCII code printed. **OPT** Memory. Remove when resolved.  
@START

				; LDA #$00
				; STA COW
				
			; PHA ;SAVE ACC
			
				; LDA #$26
				; STA HTAB	
				; LDA #$11	
				; STA VTAB
			; JSR	UPDATE.CHAR.POS
			
					; LDA COW
					; ORA #$B0
				; JSR COUT
				
			; PLA ;RESTORE ACC
			
			; INC COW
			

			
@END


			
			
			; LDA #$01
			; STA TROUBLESHOOTING.HOOK

.EXIT.CLEAN_UP
	;reset the combat mode flag
	;(this reset prevents a scenario where a mob is adjacent to a map wormhole icon when the player steps on the wormhole. This results in
	;the COMBAT_SE.MODE.PARM being set, which causes a hang after the map load returns to the game state loop. The game state loop check COMBAT_SE.MODE.PARM and initiates combat if set to $01.
	;I'm not exactly sure why the hang occured, but the combat is not desirable under those circumstances and this flag reset prevents it).
	
	LDA #$00
	STA COMBAT_SE.MODE.PARM	;($00 = player initiated or init value in map objects manager | $01 = mob initiated | $FF = test mode)
				
		
					;JSR KEYIN
					
					
							; LDA #$01
							; STA TROUBLESHOOTING.HOOK
							
							
						; LDA #$AA
						; JMP FULL.BRK
						; BRK
					
	PLA										;POP RTS FROM STACK, RETURN TO GAME LOOP DIRECTLY
	PLA
	JMP GAME.LAUNCH							;ALL NON-MOVEMENT COMMANDS MUST EXIT VIA JMP TO GAME.LAUNCH TO AVOID PROBLEMS WITH KEY PRESS ABORTS. SEE SUBROUTINE DOCUMENTATION FOR GAME.PRIMARY_LOOP FOR DETAILS.
	
@END

@END
	


;=================DEFINE VARIABLES===============

