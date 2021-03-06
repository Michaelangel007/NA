;************************INCLUDE FILE*****************************
;(do not assemble stand-alone.  )

;=====================ANIMATION_MANAGER.ASM DOCUMENTATION====================================
;Note: For a higher level discussion of the implementation of animation in the game
;See the "Animation" Section in Technical Overview.doc (/my_code/documentation). The "Animated Tiles" section
;of Graphics Details.doc talks in detail about how shape tables for animated tiles are setup. 
;
;See Chart 1.1 (my_code/documentation) for an illistration of the flow control
;of the subroutines in this file. 
; 
;The entrance to all animation routines is ANIMATION.UPDATE. 
;
;=================================================================================


ANIMATION.UPDATE 		;============INCREMENTS FRAME FOR ALL ANIMATION TILES ON-SCREEN=======
@START
;PARAMTERS: [ANIMATION.DELAY.OVERRIDE]
;ENTRANCE: DIRECT
;RETURN: ANIMATION.FRAME_STATE


;=====================SUBROUTINE DOCUMENTATION====================================
;
;This subroutine is the entrance to all animation routines. It is called
;by GAME.PRIMARY_LOOP while waiting for a keypress.
;The outerloop iterates through each tile in SCREEN.TILE.DATA looking for animation tiles.
;
;=================================================================================



;NOTES
;If a key is pressed, animation for water tiles is skipped but is processed for all other
;tile types. 
;
;A cycling the animation frames, a delay is inserted unless a key was process since the
;routine was called. The delay is needed to slow down animation with a small number of
;animated tiles on the screen. 
;
;The amount of the delay depends on the number of animated tiles on the screen.


							; lda #$aa
							; ; ldx COW+$0
							; ; ldy COW+$1
							; jmp full.brk
							; brk

			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; LDA #$AA			
			; JSR FULL.BRK
; .TEMP
			; LDA TEMP
			
			
;SAVE REGISTERS	
	PHA ;**opt** Memory. Speed. I don't think It isn't necessary to save ACC in this routine. 
	
	TXA
	PHA
	TYA
	PHA ;don't remove without adjusting the values popped off stack in NPC.TALK.INPUT (.EXIT) 



	
;INIT VARIABLES, COUNTERS, INDEXES
	LDA #$00
	STA ANIMATION.SCREEN.TALLY
	;STA ANIMATION.DEEP_WATER.TALLY
	
	LDX #$00							;INIT COUTER FOR LOOP
	LDA	#$FF							;SET ACC VALUE FOR USE IN LOOP
	TAY									;INIT Y-REG TO #$FF TO THAT INY AT START OF OUTERLOOP FLIPS IT TO #$00
LOOP
	STA	ANIMATION.MT.TRACKING,X			;INIT #$FF TO THIS ARRAY INSTEAD OF #$00 BECAUSE #$00 COULD BE A VALID MOB RECORD INDEX VALUE
	INX
	CPX #$10
	BNE LOOP
		
OUTERLOOP1
	INY									;NEXT TILE

			
		; CMP #$88
		; BEQ ANIMATION.SCROLL.WATER_STEP
		; CMP #$89
		; BEQ ANIMATION.SCROLL.WATER_STEP	
		; CMP #$8A
		; BEQ ANIMATION.SCROLL.WATER_STEP		
		; JMP ANIMATION.TILE.TESTS

; ANIMATION.SCROLL.WATER_STEP
	; JMP ANIMATION.SCROLL.WATER
	
ANIMATION.TILE.TESTS
@START
;=====================SUBROUTINE DOCUMENTATION====================================
;
;ANIMATION.TILE.TESTS (in ANIMATION.UPDATE) determines if there is a map object (i.e. transport, mob) located
;at the current screen tile location. If yes, then it subsitutes the Tile_ID for the map_object before 
;falling through to TEST.TILE_TYPE. In the case of Mobs, the determining the Tile_ID
;is complicated due to support for multi-tile mobs. See notes on DETERMINE.MOB.TILE_TYPE below.
;
;=================================================================================



		
.NPC.TALK.INPUT.KB_BUFF.CHECK1
	LDA ANIMATION.CALLED_BY
	BEQ .KB_BUFF.CHECK1.DONE
	JSR NPC.TALK.INPUT.CHECK.KB_BUFF
	;**FALLS THROUGH**
.KB_BUFF.CHECK1.DONE

	
;LOAD TILE_TYPE OF THE NEXT TILE IN CURRENT ROW
	LDA SCREEN.TILE.DATA,Y



			
; ;IS TILE DEEP WATER?
; ;(note: this tally is used to determine whether to force animation to complete when entire screen is deep water tiles)
	; CMP #TILE_ID.DEEP_WATER
	; BNE .CHECK.FOR.MOB
	; INC ANIMATION.DEEP_WATER.TALLY
	; ;**FALLS THROUGH**

;IS SCREEN TILE LOCATION ELIGIBLE FOR DRAW?
.CHECK.FOR.TEXT.WINDOW
;Note: if a text window is on screen, don't draw animation on top of it.	
;Note2: using x-reg to preserve acc (current tile type)

	LDX TW.RIGHT_WINDOW.STATUS.FLAG	;load the flag that tracks whether the right side text window is active
	CPX #$01						;is it active?
	BNE .CHECK.FOR.S_ENTITY			;if no, then next check
	
			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; ;cpy #$b5
			; cpy #$bb
			; bne .temp
			; ;CPY #SCREEN.ARRAY.LAST_ELEMENT	;AT LAST TILE?
			; ; BCS .debug.brk						;IF YES, EXIT	
			; ; jmp .TEMP
			; ; LDA *
			; ; LDA *-2
			; ; LDX *-1			;PROGRAM COUNTER AT BRK+$2 WILL BE RETURNED IN X-REG/Y-REG
			; ; TAY
; .debug.brk
			; LDA #$AB
			; LDX SCREEN.INDEX.COLUMN,Y		;load the column number of the current tile
			; ldy #TWB.RW.NPC_TALK.LEFT_TILE_COLUMN	;is the current tile column number >= the left edge of the text window?

			; JSR FULL.BRK
; .TEMP
			; LDA TEMP

	;EXIT TEST (there is also one in the routine EXIT_TEST)
	;(this text is needed because the right edge test below bypasses
	;the exit test in EXIT_TEST and once Y-REG = $BB the right edge test would
	;pass and Y-REG would continue to be incremented, eventually causing a crash)	
	CPY #SCREEN.ARRAY.LAST_ELEMENT2	;AT LAST TILE?
	BCC .EXIT_TEST.COMPLETE	
	JMP ANIMATION.EXIT				;IF YES, EXIT	
.EXIT_TEST.COMPLETE
	
	;check right edge
	LDX SCREEN.INDEX.COLUMN,Y		;load the column number of the current tile
	CPX #TWB.RW.NPC_TALK.LEFT_TILE_COLUMN	;is the current tile column number >= the left edge of the text window?
	BCS OUTERLOOP1					;if yes, then don't draw here, next tile	
	;**FALLS THROUGH**
	
;IS TILE SHAPE ANIMATED?


			
.CHECK.FOR.S_ENTITY
	LDX SCREEN.MO_SPRITE.DATA,Y			;DOES CURRENT TILE HAVE A MOB MO?	
	CPX #$FF
	BEQ .CHECK.FOR.GENERAL_MO			;IF NO, THEN CHECK FOR TRANSPORT

	JMP DETERMINE.S_ENTITY.TILE_TYPE			;RETURNS THE TILE_TYPE IN ACC. IF MOB IS MULTI-TILE, THE NECESSARY ADJUSTMENTS ARE MADE TO THE TILE_TYPE IN THE MO RECORD FOR THE MOB
	
.CHECK.FOR.GENERAL_MO ;(AND ANIMATED PLAYER WALKING TILES)
	CPY #SCREEN.ARRAY.PLAYER_LOCATION
	BNE .USE.SCREEN_ARRAY_INDEX			;IF THIS ITERATION IS PROCESSING THE PLAYER LOCATION THEN THE TRANSPORT INDEX, IF ANY IS ACTIVE, MUST BE RETREIVED FROM  PLAYER.TRANSPORT.ACTIVE

	;.is.combat_module.loaded	
	;note: if combat module is loaded, then we don't want to set the player map icon as the current animation tile, because in the combat map the player isn't anchored to the center of the screen.
	LDX PLAYER.MAP.LOCATION_TYPE	;load the code of the current module loaded. $00 = none. 
	CPX #MAP.TYPE.COMBAT		 	;is it equal to the combat module code?
	BEQ .USE.SCREEN_ARRAY_INDEX		;if yes, then skip code section below involving the player map icon
	
	LDX PLAYER.TRANSPORT.ACTIVE	
	CPX #$FF							;DOES PLAYER HAVE ACTIVE TRANSPORT?
	BEQ .NO.TRANSPORT.ACTIVE			;IF NO, THEN NO ANIMATION SHOULD BE DRAW, NEXT TILE OR EXIT
	LDA MAP_OBJECTS.GENERAL+$2,X		;LOAD TILE TYPE OF ACTIVE TRANSPORT	
	JMP TEST.TILE_TYPE
.NO.TRANSPORT.ACTIVE
	LDA PLAYER.WALKING.TILE				;PLAYER IS WALKING. LOAD TILE TYPE OF PLAYER WALKING ICON
	JMP TEST.TILE_TYPE
	
.USE.SCREEN_ARRAY_INDEX					;IF THIS ITERATION IS PROCESSING THE PLAYER LOCATION, CONTINUE TO USE SCREEN.MO_GENERAL.DATA,Y AS THE INDEX TO THE MAP OBJECT ARRAY.
	LDX SCREEN.MO_GENERAL.DATA,Y		;DOES CURRENT TILE HAVE A GENERAL MAP OBJECT?
	CPX #$FF
	BEQ TEST.TILE_TYPE					;IF NO, THEN SKIP AHEAD TO TEST TILE TYPE FOR ANIMATION	
	LDA MAP_OBJECTS.GENERAL+$2,X		;LOAD TILE TYPE OF GENERAL MAP OBJECT AT THE CURRENT SCREEN ARRAY LOCATION (Y-REG)

@END

TEST.TILE_TYPE		
@START
;=====================SUBROUTINE DOCUMENTATION====================================
;
;TEST.TILE_TYPE is a bit of spaghetti code. It is the gatekeeper to ANIMATION.TILE.FRAME_CYCLE,
;which does the actual graphics plotting. The first thing it does is branch to 
;EXIT_TEST if the Tile_ID it's passed in the ACC is not in the animated tile range.
;This makes it difficult to use ANIMATION.SCROLL.PLAYER on non-animated player icon tiles.
;
;If a Tile_Type is animated, it is not automatically refered to ANIMATION.TILE.FRAME_CYCLE for graphics
;plotting. There are exceptions where an animated tile should not be drawn
;and exceptions to those exceptions (you can probably see why this section of code got complicated)
;Just to make it more complex, TEST.TILE_TYPE also tests for Tile_IDs that have special
;animation features (currently just quicksand).
;
;Exceptions: When an Animated Tile Should not be Drawn
;tall grass, darkness, player location
;1) If the screen tile location is flagged as dark
;2) If the Tile_ID passed is for a Mob, but the underlying terrain tile is tall grass (Mobs go into stealth mode)
;3) If the current screen location in the ltop is the the player location
;				This requires checking to see if the player has active transport and if that active transport is a frigate (multi-tile object) because the player location is then 4 screen locations. 
;Exceptions to the Exceptions
;3) if the player has active transport and the transport tile is animated, then the tile should be drawn. 
;3) if the player icon is an animated Tile_ID. (This is detected in .CHECK.FOR.GENERAL_MO, which loads the animated player icon)
;
;----Special Animation Features---
;This subroutine doesn't have enough to do so we also tasked it with checking 
;to see if the Tile_ID passed has any special animation features. 
;
;1) Quicksand. If the Tile_ID is quicksand then a sinking effect is created by scrolling the player icon. This is done by a call to SCROLL.PLAYER.ICON.
;=================================================================================



;.MONITOR.VARIABLE  ;(print onscreen)
@START
; ;PARAMETERS: ACC (variable1 to monitor), X-REG (variable2 to monitor)
; ;			 VARIABLE1.HTAB, VARIABLE1.VTAB, VARIABLE2.HTAB, VARIABLE2.VTAB


				; STY TEMP
				; STA TEMP16
				
				
				; LDA TROUBLESHOOTING.HOOK
				; BEQ .TEMP
				
				; PHA ;save ACC
				; TXA
				; PHA
				
				; LDA #$26
				; STA VARIABLE1.HTAB
				; LDA #$01
				; STA VARIABLE1.VTAB
				; LDA #$26
				; STA VARIABLE2.HTAB
				; LDA #$02
				; STA VARIABLE2.VTAB								
				
				; LDA TEMP
				; LDX TEMP16
			; JSR MONITOR.VARIABLE
			; ;JSR KEYIN ;pause optional
			

				; PLA
				; TAX
				; PLA ;restore ACC
		
; .TEMP
				; LDA TEMP16
@END



			
	;ACC WILL HAVE TILE_TYPE FROM EITHER SCREEN.TILE.DATA (LOADED AT START OF .OUTTERLOOP), FROM .CHECK.FOR.MOB, OR FROM .CHECK.FOR.TRANSPORT
	STA ANIMATION.CURRENT_TILE_TYPE		;SAVED FOR USE IN .ANIMATION.TILE.FRAME_CYCLE. 	
	CMP #ANIMATION.TILE_RANGE.START		;IS CURRENT TILE A MAP ANIMATION TILE?
	BCS	.TEST.DARKNESS					;IF YES, PROCEED TO DARKNESS TEST
		;**OPT** use BMI to test whether it's an animation tile instead of CMP & BCS above
	JMP EXIT_TEST						;IF NO, NEXT TILE OR EXIT

.TEST.DARKNESS
;IS TILE HIDDEN (DARKNESS) 
	LDX SCREEN.DARK.DATA,Y			
	CPX #$01
	BNE .TEST.DARKNESS.DONE
.EXIT_TEST_STEP	
	JMP EXIT_TEST						;IF YES, NEXT TILE OR EXIT
.TEST.DARKNESS.DONE

	LDA SCREEN.TILE.DATA,Y
	
			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; CPY #$5C
			; BNE .TEMP
			; LDA TEMP
			; LDX #$AA
			; JSR FULL.BRK
			; BRK
; .TEMP
			; LDA TEMP
			
.TEST.TALL_GRASS
;IS TERRAIN TILE TALL GRASS? (MOBS ARE STEALTHY, NO ANIMATION)	
	CMP #TILE_ID.TALL_GRASS_A
	BEQ .EXIT_TEST_STEP					;IF YES, NEXT TILE OR EXIT
	CMP #TILE_ID.TALL_GRASS_B
    BEQ .EXIT_TEST_STEP					;IF YES, NEXT TILE OR EXIT
	;**FALLS THROUGH**
	
.BUILDING.CHECKS
;note: this routine assumes that all unoccupied tiles are setup as
;terrain tiles as the terrain tile type is in the ACC. To test for an unoccupied tile
;in a map object, this routine would need to test the value in ANIMATION.CURRENT_TILE_TYPE.

	;is player in building? 
	LDX PLAYER.MAP.LOCATION_TYPE
	; CPX #MAP.TYPE.TOWN_VILLAGE			;IS LOCATION TYPE = BUILDING?
	; BNE .BUILDING.CHECKS.DONE		;if no, skip rules in this section
	; ;**FALLS THROUGH**
	
	;is a building map active
	;**OPT** Memory. Speed. Right now there is only one code for location type, which is used to determine the tile_set to use and for other location-type specific features. There are a lot of the later. It might be a savings if the 
					;there were a tile-set code separate from location type code, and there was a default tile-set picked using the location-type code if tile-set code was set to default. This might require adding another byte to some hex tables
					;but I suspect it would pay off. 
	CPX #MAP.TYPE.BUILDING.GRE 	;is map type = building?
	BCC .BUILDING.CHECKS.DONE			;if no
	CPX #MAP.TYPE.BUILDING.LT	;is map type = building?
	BCS .BUILDING.CHECKS.DONE			;if no
	;**FALLS THROUGH**			 		;if yes

	
.CHECK.UNOCCUPIED.TILE_SWAPS
	;ACC: contains tile_ID of current screen location 
	CMP #TILE_ID.UNOCCUPIED.TILE.SWAP.GRE1	
	BCC .CHECK.UNOCCUPIED.TILE_SWAPS.DONE
	CMP #TILE_ID.UNOCCUPIED.TILE.SWAP.LT1	
	BCS .CHECK.UNOCCUPIED.TILE_SWAPS.DONE
	;is S_ENTITY present?
	LDX SCREEN.MO_SPRITE.DATA,Y				
	CPX #$FF	
	BEQ .CHECK.UNOCCUPIED.TILE_SWAPS.DONE ;if no, then occupied tile doesn't apply

	;is S_ENTITY passing through? If yes, then don't display occupied version of tile
	PHA	;save terrain tile_type

	;I commented this test out because in some cases I want the NPC's random movement at-anchor to display the occupied tiles (example, wizard puttering behind the counter in magic shop)
	; LDA MAP_OBJECTS.NPC+$7,X ;load at-anchor movement code
	; BNE .NPC.NOT.STATIONARY  ;if code != $00 (stantionary), then occupied tile doesn't apply because the NPC is just passing through.

	LDA MAP_OBJECTS.NPC+$6,X ;load transit code
	BNE .NPC.NOT.STATIONARY  ;if code != $00 (not in transit), then occupied tile doesn't apply because the NPC is just passing through.
	PLA ;restore terrain tile_type		
	CLC
	ADC #$01 ;switch to occupied version of tile
	STA ANIMATION.CURRENT_TILE_TYPE
	
		; JSR FULL.BRK
		; BRK
	JMP .BUILDING.CHECKS.DONE
	;JMP ANIMATION.TILE.FRAME_CYCLE
.NPC.NOT.STATIONARY
	PLA ;restore terrain tile_type
.CHECK.UNOCCUPIED.TILE_SWAPS.DONE

;****the following are occupied tile swap checks which haven't
;been setup in a range yet***


	;LDA SCREEN.TILE.DATA,Y
.TEST.BED
;IS TILE A BED (NO ANIMATION, SPRITE IS SLEEPING IF PRESENT)	
	;ACC: contains tile_ID of current screen location 
	CMP #TILE_ID.BED_LEFT_UNOCCUPIED
	BEQ .EXIT_TEST_STEP					;IF YES, NEXT TILE OR EXIT
	;***FALLS THROUGH***
.TEST.COT
;IS TILE A COT (NO ANIMATION, SPRITE IS SLEEPING IF PRESENT)	
	;ACC: contains terrain tile_ID of current screen location 
	CMP #TILE_ID.COT_UNOCCUPIED
	BEQ .EXIT_TEST_STEP					;IF YES, NEXT TILE OR EXIT
	;***FALLS THROUGH***
.TEST.OUTHOUSE_HOLE
;IS TILE A COT (NO ANIMATION, SPRITE IS SLEEPING IF PRESENT)	
	;ACC: contains terrain tile_ID of current screen location 
	CMP #TILE_ID.OUTHOUSE_HOLE.UNOCCUPIED
	BEQ .EXIT_TEST_STEP					;IF YES, NEXT TILE OR EXIT
.BUILDING.CHECKS.DONE
	;***FALLS THROUGH***
	LDA ANIMATION.CURRENT_TILE_TYPE
	
.NOTDARK
.NOT.TALL_GRASS

.TEST.PLAYER_LOCATION	
@START
		
;IS TILE AT PLAYER LOCATION?
;NOTE: This is needed because the player could be standing on an animated tile.  		
	CPY #SCREEN.ARRAY.PLAYER_LOCATION
	BNE .CHECK.MT.PLAYER.LOCATIONS	
	;ANIMATION.CURRENT_TILE_TYPE = animated player icon
	;(An animated player icon is detected in .CHECK.FOR.GENERAL_MO, which loads the animated player icon ANIMATION.CURRENT_TILE_TYPE)

.CHECK.STORM1
	;is tile a storm? (if yes, then always drawn animation even if it is the player location)
	
	;ACC = ANIMATION.CURRENT_TILE_TYPE
	CMP #TILE_ID.STORM.GRE
	BCC .NOT.STORM1
	CMP #TILE_ID.STORM.LT
	BCS .NOT.STORM1
	JMP .END.PLAYER_LOCATION.TESTS
.NOT.STORM1

	LDX PLAYER.TRANSPORT.ACTIVE			
	CPX #$FF							;DOES PLAYER HAVE ACTIVE TRANSPORT?
	BEQ .TEST.FOR.SPECIAL_TILE_FEATURES2	;IF NO, THEN NO ANIMATION SHOULD BE DRAW, NEXT TILE OR EXIT

.IS.COMBAT_MODULE.LOADED1
;we do this check so that if the combat module is loaded and the player entered combat on
;a Wyvern ally (i.e. active transport), that the rest of the transport active checks are ignore.  
;there is another combat module check further down for a different reason and both are needed.
			
	LDA PLAYER.MAP.LOCATION_TYPE		 ;load the code of the current module loaded. $00 = none. 
	CMP #MAP.TYPE.COMBAT				 ;is it equal to the combat module code?
	BEQ .TEST.FOR.SPECIAL_TILE_FEATURES2 ;if no, next check
.IS.COMBAT_MODULE.LOADED1.DONE
	
	LDA MAP_OBJECTS.GENERAL+$2,X		;LOAD TILE TYPE OF ACTIVE TRANSPORT
	CMP #ANIMATION.TILE_RANGE.START		;IS TRANSPORT TILE AN ANIMATION TILE? (E.G. WYVERN)
	BCS .ACTIVE.TRANSPORT.ANIMATED		;IF YES, DRAW ANIMATION
	JMP EXIT_TEST							;IF NO, DON'T DRAW ANIMATION. NEXT TILE OR EXIT

; .ACTIVE.TRANSPORT.ANIMATED_STEP
	; JMP .ACTIVE.TRANSPORT.ANIMATED
 
.TEST.FOR.SPECIAL_TILE_FEATURES2
;IS PLAYER STANDING IN QUICKSAND(PLAYER SINKS)
	LDA SCREEN.TILE.DATA,Y
	
	CMP #TILE_ID.QUICKSAND
	BNE .TEST.QUICKSAND.DONE						;IF NO, NEXT TEST
	JSR ANIMATION.SCROLL.PLAYER				;SKIP STANDARD ANIMATION AND SCROLL PLAYER TILE TO CREATE SINKING EFFECT
	JMP EXIT_TEST
.TEST.QUICKSAND.DONE

.BUILDING.CHECKS2
	;is player in building? 
	LDX PLAYER.MAP.LOCATION_TYPE
	; CPX #MAP.TYPE.TOWN_VILLAGE			;IS LOCATION TYPE = BUILDING?
	; BNE .BUILDING.CHECKS.DONE2		;if no, skip rules in this section

	;is a building map active
	;**OPT** Memory. Speed. Right now there is only one code for location type, which is used to determine the tile_set to use and for other location-type specific features. There are a lot of the later. It might be a savings if the 
					;there were a tile-set code separate from location type code, and there was a default tile-set picked using the location-type code if tile-set code was set to default. This might require adding another byte to some hex tables
					;but I suspect it would pay off. 
	CPX #MAP.TYPE.BUILDING.GRE 	;is map type = building?
	BCC .BUILDING.CHECKS.DONE2			;if no
	CPX #MAP.TYPE.BUILDING.LT	;is map type = building?
	BCS .BUILDING.CHECKS.DONE2			;if no
	;**FALLS THROUGH**			 		;if yes

.CHECK.PLAYER_ICON.SUPRESSED.TILES
@START
;(check to see if the terrain tile at the player's location is a tile that 
;covers the player and thus should not be drawn, such as an archway)


.CHECK.ARCHWAY	;is player standing under archway?
	;ACC: contains map terrain tile_ID of player screen location 
	CMP #TILE_ID.BUILDING.ARCHWAY.GRE   ;if no
	BCC .CHECK.ARCHWAY.DONE
	CMP #TILE_ID.BUILDING.ARCHWAY.LT	;if no
	BCS .CHECK.ARCHWAY.DONE
	JMP EXIT_TEST						;IF YES, don't draw player icon animation. NEXT TILE OR EXIT
.CHECK.ARCHWAY.DONE
@END

.CHECK.UNOCCUPIED.TILE_SWAPS2
	;ACC: contains map terrain tile_ID of player screen location 
	CMP #TILE_ID.UNOCCUPIED.TILE.SWAP.GRE1	
	BCC .CHECK.UNOCCUPIED.TILE_SWAPS.DONE2
	CMP #TILE_ID.UNOCCUPIED.TILE.SWAP.LT1	
	BCS .CHECK.UNOCCUPIED.TILE_SWAPS.DONE2
	CLC
	ADC #$01 ;switch to occupied version of tile
	STA ANIMATION.CURRENT_TILE_TYPE
	JMP .END.PLAYER_LOCATION.TESTS	
.CHECK.UNOCCUPIED.TILE_SWAPS.DONE2

.CHECK


.BUILDING.CHECKS.DONE2
	
	;MOTH-BALLED CODE
	; ; CMP #ANIMATION.WATER_RANGE.START	;IS TILE_TYPE < START OF WATER TILE RANGE?
	; ; BCC .IS.PLAYER.ANIMATED				;IF YES, NEXT CHECK 
	
	; ; CMP #ANIMATION.WATER_RANGE.END		;IS TILE_TYPE >= END OF WATER TILE RANGE+$1?
	; ; BCS .IS.PLAYER.ANIMATED				;IF YES, NEXT CHECK 
	
	; ; JSR ANIMATION.SCROLL.PLAYER			;PLAYER IS IN WATER. SKIP STANDARD ANIMATION AND SCROLL PLAYER TILE TO CREATE SINKING EFFECT
	; ; JMP EXIT_TEST

.IS.COMBAT_MODULE.LOADED2
;we do this check so that the .IS.PLAYER.ANIMATED code section below is skipped which checks to see if the player tile is animated. It doesn't matter. If combat module is loaded the player icon won't be the current animation tile. So we want the center screen tile to be treated just like any other. 	
;and by JMP .END.PLAYER_LOCATION.TESTS, the tile_ID in ANIMATION.CURRENT_TILE_TYPE as of the start of TEST.TILE_TYPE will be drawn. 
			
	LDA PLAYER.MAP.LOCATION_TYPE		 ;load the code of the current module loaded. $00 = none. 
	CMP #MAP.TYPE.COMBAT		 ;is it equal to the combat module code?
	BNE .IS.PLAYER.ANIMATED		;if no, next check
	JMP .END.PLAYER_LOCATION.TESTS
	
.IS.PLAYER.ANIMATED
	;ANIMATION.CURRENT_TILE_TYPE = animated player icon
	;(An animated player icon is detected in .CHECK.FOR.GENERAL_MO, which loads the animated player icon ANIMATION.CURRENT_TILE_TYPE)
		
;IS PLAYER TILE ANIMATED?
	LDA PLAYER.WALKING.TILE
	CMP #ANIMATION.TILE_RANGE.START		;IS CURRENT TILE A MAP ANIMATION TILE?
	BCS	.PLAYER.WALKING.ICON.ANIMATED	;IF YES, PROCESS ANIMATION FOR THIS TILE
	JMP EXIT_TEST						;IF NO, NEXT TILE OR EXIT



.CHECK.MT.PLAYER.LOCATIONS


;CHECK IF TRANSPORT IS ACTIVE FIRST BECAUSE ITS THE FASTER TEST. IN THE ABOVE ROUTINE WE NEED TO REST THE REGULAR PLAYER LOCATION WHETHER TRANSPORT IS ACTIVE OR NOT SO REGUALR PLAYER ICON DOESN'T GET DRAWN OVER WITH ANIMATION
	LDX PLAYER.TRANSPORT.ACTIVE			
	CPX #$FF									;DOES PLAYER HAVE ACTIVE TRANSPORT?
	BEQ .NO.MT.TRANSPORT.ACTIVE					;IF NO, THEN NO ANIMATION SHOULD BE DRAW, TEST EXIT.
	
	LDA MAP_OBJECTS.GENERAL+$2,X				;LOAD TILE TYPE OF ACTIVE TRANSPORT
	CMP #TILE_ID.FRIGATE1.1						;IS TRANSPORT A SINGLE TILE OBJECT? (FRIGATES ARE THE ONLY MULTI-TILE TRANSPORT IN THE GAME. IF MORE WERE ADDED, THEN EITHER TWO TILE_TYPE CHECKS ARE NEEDED OR A MULTI-TILE FLAG WOULD BE NEEDED
	BNE .NO.MT.TRANSPORT.ACTIVE					;IF YES, NO NEED TO CHECK THE LOCATIONS THE PLAYER ICON WOULD OCCUPY WHEN MULTI-TILE TRANSPORT IS ACTIVE. 
			
	CPY #PLAYER.TRANSPORT.MT.TILE_LOCATION1		;CHECK MULTI-TILE PLAYER TRANSPORT LOCATIONS
	BEQ .IS.PLAYER.MT.LOCATION						;IF PLAYER LOCATION, DON'T DRAW ANIMATION, TEST EXIT	
	CPY #PLAYER.TRANSPORT.MT.TILE_LOCATION2		;CHECK MULTI-TILE PLAYER TRANSPORT LOCATIONS
	BEQ .IS.PLAYER.MT.LOCATION						;IF PLAYER LOCATION, DON'T DRAW ANIMATION, TEST EXIT	
	CPY #PLAYER.TRANSPORT.MT.TILE_LOCATION3		;CHECK MULTI-TILE PLAYER TRANSPORT LOCATIONS
	BEQ .IS.PLAYER.MT.LOCATION						;IF PLAYER LOCATION, DON'T DRAW ANIMATION, TEST EXIT	
	JMP .NOTPLAYER_TILE
	
.IS.PLAYER.MT.LOCATION
.CHECK.STORM2
	;is tile a storm? (if yes, then always drawn animation even if it is the player location)
	LDA ANIMATION.CURRENT_TILE_TYPE
	CMP #TILE_ID.STORM.GRE
	BCC .NOT.STORM2
	CMP #TILE_ID.STORM.LT
	BCS .NOT.STORM2
	JMP .END.PLAYER_LOCATION.TESTS
.NOT.STORM2
	
	JMP EXIT_TEST								;IF YES, NEXT TILE OR EXIT

	
.ACTIVE.TRANSPORT.ANIMATED
	STA ANIMATION.CURRENT_TILE_TYPE		;UPDATED. THIS IS REQUIRED BECAUSE THE ACTIVE TRANSPORT INDEX IS NOT STORED IN THE TRANSPORT SCREEN ARRAY, SO THE ONLY WAY TO DETECT AN ACTIVE TRANSPORT OBJECT IS TO CHECK PLAYER.TRANSPORT.ACTIVE VARIABLE. 
.PLAYER.WALKING.ICON.ANIMATED
.NO.MT.TRANSPORT.ACTIVE
.NOTPLAYER_TILE
.END.PLAYER_LOCATION.TESTS
@END
	;**FALLS THROUGH**
	
	
;HAS PLAYER PRESSED A KEY SINCE THIS ROUTINE WAS RUN? IF SO, SKIP WATER TILE ANIMATION	
	; CMP #ANIMATION.WATER_RANGE.START	;IS TILE_TYPE < START OF WATER TILE RANGE?
	; BCC ANIMATION.TILE.FRAME_CYCLE		;IF YES, CONTINUE
	
	; CMP #ANIMATION.WATER_RANGE.END		;IS TILE_TYPE >= END OF WATER TILE RANGE+$1?
	; BCS ANIMATION.TILE.FRAME_CYCLE		;IF YES, CONTINUE
	
	; LDA KB_BUFFER						;LOAD NEXT KEY PRESS FROM BUFFER
    ; BPL ANIMATION.TILE.FRAME_CYCLE		;IF NO KEY PRESS, CONTINUE
	; JMP EXIT_TEST						;IF KEY PRESSED, LEAVE IT IN BUFFER AND RETURN TO GAME LOOP FOR KEYPRESS PROCESSING




			
@END

ANIMATION.TILE.FRAME_CYCLE ; ========DRAWS NEXT ANIMATION FRAME FOR ONE TILE======
@START

;=====================SUBROUTINE DOCUMENTATION====================================
;
;ANIMATION.TILE.FRAME_CYCLE is very straightforward. It is an in-line 
;version of DRAW.TILE.SINGLE & DRAW.TILE from Graphics_Engine.ASM. A few
;this occur worth mentioning before the in-line code starts at .CALC.SHAPE.TABLE
;
;1) A counter is incremented that tracks the numbe of animation tiles on 
;the screen, which is used in .EXIT to determine if a delay should be 
;inserted between the cycling of animation frames.
;
;2) Crocodile. Some checks are done on the Tile_ID that was passed and the Tile_ID 
;in SCREEN.TILE.DATA for the current screen location. If the checks conclude that
;there is a crocodile mob located in a surf water tile, then mob's Tile_ID is 
;changed because crocodiles have a different Tile_ID in water than they do on land. 
;
;=================================================================================

			
;DRAWS TO FOREGROUND
	
;PARAMTERS: Y-REG (SCREEN POSITION AS MAPPED TO ELEMENT OF SCREEN.TILE.DATA), ANIMATION.CURRENT_TILE_TYPE
;RETURN: NONE
;ENTRANCE: DIRECT

			; STA cow
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP


			
				; PHA ;save ACC
				; TXA
				; PHA
				
				; LDA #$24
				; STA VARIABLE1.HTAB
				; LDA #$07
				; STA VARIABLE1.VTAB
				; LDA #$26
				; STA VARIABLE2.HTAB
				; LDA #$08
				; STA VARIABLE2.VTAB								
				
				; tya
				; LDX #$aa
			; JSR MONITOR.VARIABLE
			; JSR KEYIN ;pause optional
				; STA TEMP
				; PLA
				; TAX
				; PLA ;restore ACC
; .temp
				; lda cow
		
	STY TEMP
	LDA #SCREEN.ARRAY.LAST_ELEMENT
	CMP TEMP
	BCS .START
	JMP ERROR2
	
.START	
		
	INC ANIMATION.SCREEN.TALLY		;KEEP TRACK OF THE # OF ANIMATED TILES ON THE VIEW SCREEN

	
;DETERMINE TILE_TYPE OF THE CURRENT ANIMATION TILE TO CYCLE
	LDA ANIMATION.CURRENT_TILE_TYPE
				




			
;IS MOB A CROCODILE?
	CMP #TILE_ID.CROC_A					;DOES MOB HAVE CROC PRIMARY TILE_ID?
	BNE .CALC.SHAPE.TABLE				;IF NO, CALCULATE SHAPE TABLE
			
	LDA SCREEN.TILE.DATA,Y				;LOAD MAP TILE MOB SITS ON
	CMP #TILE_ID.SURF					;IS CROC IN SURF WATER?
	BNE .CALC.SHAPE.TABLE				;IF NO, CALCULATE SHAPE TABLE
	
	LDX #TILE_ID.CROC_B			;used as index to the shape table base address lookup table				;IF YES, SUBSTITUTE ALTERNATE CROC TILE_ID (IN WATER ENVIRONMENT) 
	JMP .CALC.SHAPE.TABLE.ENTRANCE2	

;In-line: DRAW.TILE.SINGLE & DRAW.TILE (Graphics_Engine.ASM)
.CALC.SHAPE.TABLE
;CALCULATE SHAPE TABLE ADDRESS
	LDX ANIMATION.CURRENT_TILE_TYPE	;used as index to the shape table base address lookup table
;	STX ANIMATION.XREG.STORAGE		;save tile_type
				;**OPT** not sure the above is needed, the tile_type value in X-REG doesn't seem to be used in the rest of the routine. Also see the associated LDX below. 
				
.CALC.SHAPE.TABLE.ENTRANCE2									
	LDA TILE.SHAPES.LO,X			;LOAD SHAPE TABLE BASE ADDRESS INTO OP2
	STA OP2	
	LDA TILE.SHAPES.HO,X
	STA OP2+$1
@MIDDLE

;ANIMATION.FRAME_STATE holds the current animation frame number for all animation tiles
;on the view screen. This variable can be used as an offset to the shape table base address
;to identify the address for the current animation frame for the shape table already loaded
;into SHAPE in the routine above.
;
;Each frame is $20 bytes (!32), so we can use ASL multiplication to calcualte the offset. 

;**OPT** Speed. Now that there are 4 animation frames instead of 6, 8-bit addition could be used for the offset
;because page boundaries would never be crossed by the offset. If I add some 8 frame animation tiles I think
;8 bit addition will work there as well because each tile will take an entire page. Just increment the HO byte.
;Even though some delays are inserted, this will still help performance what there
;are lots of animation tiles on the screen (such as water) as no delay is inserted then.

	LDX ANIMATION.FRAME_STATE		;LOAD CURRENT ANIMATION FRAME

.CHECK.FOR.MOB
	LDA SCREEN.MO_SPRITE.DATA,Y			;load the sprite record # for the current tile from the sprite screen array		
	CMP #$FF							;does current tile have a mob mo?
	BEQ .CALCULATE.SHAPE.TABLE.ADDRESS	;if no then use the value in ACC for current animation frame when calculating shape table address

;ALTER FRAME ORDER TEST
;(NOTE: switches animation frame order based on bit test. The concept is that the storm objects are setup with different 
;flag byte values so that the bit test yields different results, causing the lightning bolts between some mobs to flash 
;at different times instead of being in sync.) 
		
	;alter frame order test	
	LDA #$40 ;the AND mask value needed for the BIT operation below. 
	BIT MAP_OBJECT.RECORD.READ+$3 ;test bit6 of MAP_OBJECT.RECORD.READ+$03
	BEQ .CALCULATE.SHAPE.TABLE.ADDRESS ;branch if bit6 is not set. use the value in XREG for current animation frame when calculating shape table address 
		;if BIT6 set, alter frame order to (2,3,0,1.....so real frame 0 = modified frame 2, real frame 1 = modified frame 3, real frame 2 = modified frame 0, real frame 3 = modified frame 1)

	;alter frame order
	CPX #$00 
	BEQ .FRAME00
	CPX #$01
	BEQ .FRAME01
	CPX #$02
	BEQ .FRAME02
	CPX #$03
	BEQ .FRAME03
.FRAME00
	LDX #$2	;modify current frame to frame 2
	JMP .CALCULATE.SHAPE.TABLE.ADDRESS	;now that the current frame is determine, proceed to calculate the shape table address
	
.FRAME01
	LDX #$3	;modify current frame to frame 3
	JMP .CALCULATE.SHAPE.TABLE.ADDRESS	;now that the current frame is determine, proceed to calculate the shape table address

.FRAME02
	LDX #$0	;modify current frame to frame 0
	JMP .CALCULATE.SHAPE.TABLE.ADDRESS	;now that the current frame is determine, proceed to calculate the shape table address

.FRAME03
	LDX #$1	;modify current frame to frame 1
	;**FALLS THROUGH**


	
.CALCULATE.SHAPE.TABLE.ADDRESS	
	;X-REG: animation frame number
	TXA
	ASL ;X2						
	ASL ;X4							
	ASL ;X8							
	ASL ;X16						
	ASL ;X32

	STA OP1							;LOAD SHAPE TABLE BASE OFFSET INTO OP1


	LDA #$00
	STA OP1+$1

;=======INLINE CODE FOR ADC.16========	
;TILE.SHAPES.HO/LO(2)+ ACC(1) [ANIMATION FRAME OFFSET]


; DO THE MATH
	CLD ;**opt** memory. speed. this can be removed. Also, optimize the 16-bit add code. no need to use parameters of the ADC-16 subroutine. 
    CLC                          ;ALWAYS BEFORE ADD
    LDA OP1
    ADC OP2
    STA AUX_MOVE.START
		 
    LDA OP1+$1
    ADC OP2+$1					;carry flag not cleared via CLC intentionally, it's part of 16-bit adding. 
    STA AUX_MOVE.START+$1
	
;======================================
	
	LDA #SHAPE.SIZE				;# OF BYTES IN SHAPE TABLE
	STA OP1
	LDA #$00
	STA OP1+$1

	LDA AUX_MOVE.START
	STA OP2
	LDA AUX_MOVE.START+$1
	STA OP2+$1
;=======INLINE CODE FOR ADC.16========	
;SHAPE.SIZE(1)+ AUX_MOVE.START(2)


; DO THE MATH
	CLD ;**opt** memory. speed. this can be removed. Also, optimize the 16-bit add code. no need to use parameters of the ADC-16 subroutine. 
    CLC                          ;ALWAYS BEFORE ADD
    LDA OP1
    ADC OP2
    STA AUX_MOVE.END
		 
    LDA OP1+$1
    ADC OP2+$1					;carry flag not cleared via CLC intentionally, it's part of 16-bit adding. 
    STA AUX_MOVE.END+$1
	
;======================================	

	LDA #ANIMATION.SHAPE.HOPPER		;SAVE ANIMATION.SHAPE.HOPPER AS THE DESTINATION ADDRESS FOR AUX MOVE
	STA AUX_MOVE.DEST
	STA SHAPE						;CONNECTS ANIMATION.SHAPE.HOPPER TO SHAPE, USED BY DRAW.TILE
	LDA /ANIMATION.SHAPE.HOPPER
	STA AUX_MOVE.DEST+$1
	STA SHAPE+$1			

	CLC								;EXECUTE AUX MEMORY MOVE
	JSR AUX_MOVE
				
	LDA SCREEN.INDEX.TILE_SBYTE,Y
	STA	TEMP
	STA SCREEN.DRAW.BYTE1
	INC TEMP
	LDA TEMP
	STA SCREEN.DRAW.BYTE2
	
	LDA SCREEN.INDEX.TILE_LINE,Y
	STA TILE.LINE.START
	STA TILE.LINE
	STY ANIMATION.YREG.STORAGE			;Y-REG IS USED IN .DRAW.LOOP, BUT IT IS ALSO NEEDED BY OUTERLOOP TO TRACK THE CURRENT TILE
	
	
	TAX						;LOAD LINE IN X REGISTER	
	LDA #$00
	STA SHP.TBL.CNTR		;START DRAWING AT BEGINNING OF SHAPE TABLE

	LDA TILE.LINE.START		;THE STARTING LINE OF THE TILES IN THE CURRENT ROW
	CLC						;*OPT* I I THINK ALL CLS CAN BE REMOVED WHEN I'M NOT EXPECTING AN OVERFLOW
	ADC #TILE.DEPTH.STANDARD			;ADD DEPTH OF SHAPE TO STARTING LINE IN HEX (# of lines, not the position of last line....so line positions $0-$F is $10 (!16) total lines)
	STA TILE.LINE.STOP		;SET THE LAST LINE TO DRAW BASED ON THE DEPTH OF THE TILE

@MIDDLE
	
.DRAW.LOOP
	
	LDA PAGE.BACKGROUND		;SPECIFY PAGE TO GET ADDRESS FOR
;==========INLINE CODE GET.LINE.ADDRESS1==================	
	;TILE.LINE IS IN X-REG
	CMP #$02
	BEQ .LOOKUP.PAGE2
	CMP #$01				;DOES ACC = PAGE1? IF, YES FALL THROUGH TO PAGE 1 LOOKUP
	
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

;MAIN DRAW CODE
;NOTE: SINCE ONLY TWO SCREEN BYTES ARE NEEDED AS THIS IS A SINGLE TILE DRAW, LOOKING UP BOTH BYTES
	LDY SHP.TBL.CNTR		;LOAD Y WITH SHAPE TABLE COUNTER
	LDA (SHAPE),Y			;LOAD SHAPE BYTE (1st screen byte of the tile)
	LDY SCREEN.DRAW.BYTE1	;SCREEN BYTE INDEX IN Y REGISTER	
	STA (LINE.BASE.ADDR1),Y	;PLOT (1st screen byte)
	
	INC SHP.TBL.CNTR		;NEXT SHAPE BYTE, NEXT TILE LINE
;	INC SCREEN.DRAW.CURRENT_BYTE			;SWITCH TO 2ND SCREEN BYTE IN THE TILE
	
	LDY SHP.TBL.CNTR		;LOAD Y WITH SHAPE TABLE COUNTER
	LDA (SHAPE),Y			;LOAD SHAPE BYTE (2nd screen byte of the tile)
	LDY SCREEN.DRAW.BYTE2	;SCREEN BYTE INDEX IN Y REGISTER	
	STA (LINE.BASE.ADDR1),Y	;PLOT (2st screen byte)
	
	;DEC SCREEN.DRAW.CURRENT_BYTE			;SWITCH BACK TO 1ST SCREEN BYTE IN THE TILE	;**OPT** if the counters are updated after the CMP/branch it might save 1 INC SCREEN.DRAW.CURRENT_BYTE instruction in the main loop because SCREEN.DRAW.CURRENT_BYTE will be in 2nd position when this loop ends. 
	INX						;NEXT TILE LINE
	INC SHP.TBL.CNTR		;NEXT SHAPE BYTE
	
	CPX TILE.LINE.STOP		;IS TILE DONE?							
	BCC .DRAW.LOOP			;IF NO, DRAW NEXT LINE (BCC: is X-REG < CPX value)	

	LDY ANIMATION.YREG.STORAGE			;RESTORE Y-REG
;	LDX ANIMATION.XREG.STORAGE			;RESTORE X-REG
								;**OPT** not sure the above is needed, the tile_type value in X-REG doesn't seem to be used in the rest of the routine. also see the associated STX above

@END
	
EXIT_TEST
@START


			
	CPY #SCREEN.ARRAY.LAST_ELEMENT	;AT LAST TILE?
	BCS ANIMATION.EXIT				;IF YES, EXIT	
	JMP OUTERLOOP1	

ANIMATION.EXIT
;INSERT DELAY
;Note: under most circumstances, the animation routine runs too fast. Inserting the delay
;helps achieve the desired visial frame flip rate. The delay is adjusted based on the number
;of animated tiles onscreen, currently only using one threadhold.
;Additionally, if animation was called by a string input routine, 
;then a different delay routine is used, which checks for a keypress each iteration of the delay loop.

; ;(this was for movement) INSERT DELAY, BUT SKIP IF KEYPRESS
	; LDA KB_BUFFER					;LOAD NEXT KEY PRESS FROM BUFFER. WAS KEY PRESSED?
    ; BMI .SKIPALL					;IF YES, SKIP DELAY, AND LEAVE KEY IN BUFFER, RETURN TO GAME LOOP FOR KEYPRESS PROCESSING



;DEBUG PRINTS HERE

			
	LDA ANIMATION.DELAY.OVERRIDE
	BNE .SKIPALL
				
	LDA ANIMATION.SCREEN.TALLY		
	CMP #ANIMATION.DELAY_TRIGGER1	;DETERMINE IF DELAY SHOULD BE SKIPPED BASE ON # OF ANIMATED TILES ON CURRENT VIEW SCREEN
	BCS .SKIP1

	LDX #$00 ;init delay counter, lo byte
	LDY #$00 ;init delay counter, ho byte
.WAIT.LOOP1	
	LDA ANIMATION.CALLED_BY			;get calling routine code, if available
	BEQ .KB_BUFF.CHECK2.DONE		;if no code is available, then assume that animation manager was not called by a string input routine and use the standard ROM wait routine to generate the delay.
	JSR NPC.TALK.INPUT.CHECK.KB_BUFF
.KB_BUFF.CHECK2.DONE
	INX ;increment lo byte delay counter
	BNE .WAIT.LOOP1
	INY ;increment ho byte delay counter
	CPY #$20 ;arbitrary value tuned to the visually observed animation speed
	BNE .WAIT.LOOP1	
	;**FALLS THROUGH**

; ;Standard ROM Wait Routine
	; LDA #$FF
	; JSR WAIT 				;IF NO, INSERT DELAY
	
.SKIP1
	LDA ANIMATION.SCREEN.TALLY		
	CMP #ANIMATION.DELAY_TRIGGER2	;DETERMINE IF DELAY SHOULD BE SKIPPED BASE ON # OF ANIMATED TILES ON CURRENT VIEW SCREEN
	BCS .DELAY_DONE	

	LDX #$00 ;init delay counter, lo byte
	LDY #$00 ;init delay counter, ho byte
.WAIT.LOOP2
	LDA ANIMATION.CALLED_BY			;get calling routine code, if available
	BEQ .KB_BUFF.CHECK3.DONE		;if no code is available, then assume that animation manager was not called by a string input routine and use the standard ROM wait routine to generate the delay.
	JSR NPC.TALK.INPUT.CHECK.KB_BUFF
.KB_BUFF.CHECK3.DONE
	INX ;increment lo byte delay counter
	BNE .WAIT.LOOP2
	INY ;increment ho byte delay counter
	CPY #$20 ;arbitrary value tuned to the visually observed animation speed
	BNE .WAIT.LOOP2		
	
	; LDA #$FF
	; JSR WAIT 				;IF NO, INSERT DELAY
	

.DELAY_DONE	
.SKIPALL


;DEBUG PRINTS HERE



;INCREMENT CURRENT ANIMATION FRAME	
			LDA ANIMATION.FRAME_STATE
			CMP #ANIMATION.TOTAL_FRAMES
			BNE .NOFLIP
		
			LDA #$00
			STA ANIMATION.FRAME_STATE
			STA ANIMATION.DELAY.OVERRIDE ;reset to default: off. 
			JMP .DONE
.NOFLIP
	INC ANIMATION.FRAME_STATE

.DONE
;INSERT DELAY, BUT SKIP IF KEYPRESS
	; LDA KB_BUFFER					;LOAD NEXT KEY PRESS FROM BUFFER. WAS KEY PRESSED?
    ; BMI .EXIT.ANIMATION_ABORT		;IF YES, SKIP DELAY, AND LEAVE KEY IN BUFFER, RETURN TO GAME LOOP FOR KEYPRESS PROCESSING



	
.EXIT.ANIMATION_COMPLETE
	JSR FLIP.PAGE
	

			
	;**OPT** Speed. Insert in-line code

	
; ;ENABLE.BS_RAM BEFORE EXITING
	; LDA $C083				;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK 1ST)
	; LDA $C083	


	
;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX	
	PLA

;.DEBUG.PRINT_0 ;same as above but with auto-increment to ASCII code printed. **OPT** Memory. Remove when resolved.  
@START
	
	; STA TEMP
	
	; LDA TROUBLESHOOTING.HOOK
	; BEQ .TEMP

			; ;get rts address
			; PLA
			; STA COW2+$0
			; sta cow2+$2
			; PLA
			; STA COW2+$1
			; sta cow2+$3
			; ;put rts address back
			; pha
			; ldA COW2+$0
			; pha
			
			
				; LDA #$00
				; STA COW
							
				; LDA #$26
				; STA HTAB	
				; LDA #$11	
				; STA VTAB
			; JSR	UPDATE.CHAR.POS
			
					; LDA COW
					; ORA #$B0
				; JSR COUT


		; TXA
		; PHA
		
		; LDA #$24
		; STA VARIABLE1.HTAB
		; LDA #$12
		; STA VARIABLE1.VTAB
		; LDA #$26
		; STA VARIABLE2.HTAB
		; LDA #$12
		; STA VARIABLE2.VTAB								
		
		; LDA COW2+$0
		; LDX COW2+$1
	; JSR MONITOR.VARIABLE

		; PLA
		; TAX
		

							; ; jsr keyin
							; ; cmp #$d1
							; ; bne .temp
							; ; lda #$aa
							; ; ldx COW2+$2
							; ; ldy COW2+$3
							; ; jmp full.brk
							; ; brk
			

		; INC COW
		
; .TEMP
			; LDA TEMP

			
@END


			
	RTS

NPC.TALK.INPUT.CHECK.KB_BUFF ;check fpr keypress
@START
;Note: periodically called during the run time of ANIMATION.UPDATE
;to check for a keypress on behalf of NPC.TALK.INPUT.KEYIN, a recusive subroutine.
;This check is only done if ANIMATION.UPDATE was called by NPC.TALK.INPUT.KEYIN
	LDA KB_BUFFER
    BPL .KB_BUFF.CHECK.DONE 	;branch if no value in lastkey buffer
	STA KEYIN.SAVED ;save captured keypress

			
		TXA ;save registers
		PHA
		TYA
		PHA
	JSR NPC.TALK.INPUT.KEYIN	;the subroutine which manages string input for the NPC.TALK module	
		PLA ;restore registers
		TAY
		PLA
		TAX

.KB_BUFF.CHECK.DONE	
	RTS ;return to the kb_buff check within ANIMATION.UPDATE that initiated the call
	
@END
	
ERROR2
;	ANIMATION.TILE.FRAME_CYLE reports request to draw a tile to a location not on the screen 
;	(i.e. > $BA)

	LDA #$A5
	JSR FULL.BRK
	
@END	
@END
	
DETERMINE.S_ENTITY.TILE_TYPE ;============FACTORS IN WHETHER MOB IS MULTI-TILE=====
@START
;PARAMETERS: X-REG (HOLDING THE MO INDEX OF THE CURRENT MOB)
;RETURN: ACC (MOB TILE TYPE)

;=====================SUBROUTINE DOCUMENTATION====================================
;This subroutine is called if a S_ENTITY map object is detected at the current screen tile location
;being processed. 
;
;This subroutine is responsible for returning the Tile_ID associated with the mob. 
;Doing so is straightforward if the mob is single tile; it is just a matter of returning byte 3 from the 
;MAP_OBJECTS.MOB array for the mob's record. 
;
;For multi-tile mobs, the challenge is that the Tile_ID is different 
;for each tile of the mob. However, the mob's record in MAP_OBJECTS.MOB
;only stores the Tile_ID of the upper left corner of the mob, which is also
;the first tile (upper left corner) that will be encoutered by the animation loop because
;the animation loop examines the screen arrays left to right, top to bottom. 
;
;The mob's screen array (SCREEN.MO_SPRITE.DATA) only stores the index to
;the MOB's record in MAP_OBJECTS.MOB, so this subroutine has to first figure out
;which of the multi-tile mob's tile locations is associated with the currente screen location. 
;
;Once that is known, returning the correct Tile_ID is straightforward because Multi-Tile Mobs 
;Tile_IDs are sequential (i.e the first Tile_ID is for the upper left tile of the mob, the 2nd)
;Tile ID is for the upper right tile of the mob, 3rd is for lower left, 4th is for lower right).
;So the Tile_IDs for a multi-tile mob might be $96 (upper left),$97 (upper right),$98 (lower left),$99 (lower right). 
;
;This subroutine determines which multi-tile mob tile location is assocaited with the
;current screen location being processed by keeping a tally of the number of times
;this routines has been called for each mob record. This works because
;the direction the screen arrays are iterated (described above) results in
;the loop encountering the multi-tile mob's tile locations sequentially. 
;=================================================================================

.INIT
	STY ANIMATION.YREG.STORAGE	;HOLDS OUTERLOOP COUNTER

;READ MAP OBJECT RECORD
;(for S_ENTITY at current screen tile location)

				; lDA #$00
				; STA MAP_OBJECT.ARRAY.S_ENTITY.NOT_FOUND	
				
			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; cpy #$49
			; bne .temp
			; LDA #$01
			; STA TROUBLESHOOTING.HOOK2
	
; .TEMP
			; LDA TEMP
	
					; lda #$01
					; sta troubleshooting.hook
					
		LDA #$08 ;read $8 bytes    ;**OPT** Memory. I think this routine could just default to an 8byte read and note that the last 4 bytes are garbage for S_ENTITIES with 4 byte records. The code that uses the return data already knows that. 
		;Y-REG (screen array index)
	JSR READ.MAP_OBJECT.ARRAY
		;RETURN VALUE = MAP_OBJECT.RECORD.READ(8)


			
;IS MOB MULTI-TILE?	
	LDA #$08 ;the AND mask value needed for the BIT operation below. 
	BIT MAP_OBJECT.RECORD.READ+$3 ;test bit3 of MAP_OBJECT.RECORD.READ+$03
	BNE .MT.SEARCH.ENTRANCE ;branch if bit3 is set (mob is multi-tile)
	JMP .EXIT2 ;jump if mob is not multi-tile
	
	;**FALLS THROUGH**
	

			
.MT.SEARCH.ENTRANCE	
@START
;=====================CODE-SECTION DOCUMENTATION====================================
;
;extract preliminary tile type from the mob's mo record and adjust based on the 
;mt tile# (0-3) of the current screen position.
;
;=================================================================================

 ;**OPT** Memory. Since this code only applies for MTT mobs, which there won't be mayn onscreen at once,
 ;maybe try JSR for CONVERT.GMAP.TO.PLAYER_RELATIVE.XY instead of in-line code. It's a subroutine already in map_tools.ASM 

.CONVERT.GMAP.TO.PLAYER_RELATIVE.XY
@START
;Formula: PLAYER GMAP.X/Y - NPC GMAP.X/Y + $80/$80 (ground zero, player location in relative grid) = NPC Player Relative X/Y

;Note: this routine converts the GMAP.X/Y values in MAP_OBJECT.RECORD.READ+$0-1 to 
;player relative X/Y values. When the combat module is loaded, player GMAP doesn't change
;as the combat PCs are treated as map objects just like mobs.  

.LOAD.PLAYER.GMAP

	
.DO.CONVERSION
	LDA GMAP.X						;player GMAP X-axis, before current move
	CMP MAP_OBJECT.RECORD.READ+$0	;sprite GMAP X-Axis
	BCC .MOB.MO_X_LESS
	
	SEC
	SBC MAP_OBJECT.RECORD.READ+$0		;sprite GMAP X-Axis	
	STA MAP_OBJECTS.X_ADJ				;==distance between player and sprite x-axis

	LDA #MAP_OBJECTS.PLAYER_LOCATION	;Subtract distance from Player Location on MO grid ($80)
	SBC	MAP_OBJECTS.X_ADJ
	STA MAP_OBJECT.RECORD.READ+$0		;==player relative X
	
	JMP .MOB.YTEST

.MOB.MO_X_LESS ;(Player X less than sprite X )
	LDA MAP_OBJECT.RECORD.READ+$0		;sprite GMAP X-Axis
	
	SEC
	SBC GMAP.X					;player GMAP X-axis, before current move
	STA MAP_OBJECTS.X_ADJ		;==distance between player and sprite x-axis
	
	LDA #MAP_OBJECTS.PLAYER_LOCATION	;Add distance to Player Location on MO grid ($80)
	CLC										
	ADC MAP_OBJECTS.X_ADJ								
	STA MAP_OBJECT.RECORD.READ+$0			;==player relative X
	
	;**FALLS THROUGH**
	
.MOB.YTEST	
	LDA GMAP.Y						;player GMAP Y-axis, before current move
	CMP MAP_OBJECT.RECORD.READ+$1	;sprite GMAP Y-Axis
	BCC .MOB.MO_Y_LESS
	
	SEC
	SBC MAP_OBJECT.RECORD.READ+$1		;sprite GMAP Y-Axis
	STA MAP_OBJECTS.Y_ADJ			;==distance between player and sprite y-axis

	LDA #MAP_OBJECTS.PLAYER_LOCATION	;Subtract distance from Player Location on MO grid ($80)
	SBC	MAP_OBJECTS.Y_ADJ
	STA MAP_OBJECT.RECORD.READ+$1		;==player relative Y
	
	JMP .CONVERSION.COMPLETE

.MOB.MO_Y_LESS ;(Player Y less than sprite Y )
	LDA MAP_OBJECT.RECORD.READ+$1	;sprite GMAP Y-Axis	
	SEC
	SBC GMAP.Y						;player GMAP Y-axis, before current move
	STA MAP_OBJECTS.Y_ADJ			;==distance between player and sprite y-axis
	
	LDA #MAP_OBJECTS.PLAYER_LOCATION	;Add distance to Player Location on MO grid ($80)
	CLC										
	ADC MAP_OBJECTS.Y_ADJ								
	STA MAP_OBJECT.RECORD.READ+$1		;==player relative Y
	;**FALLS THROUGH**	

.CONVERSION.COMPLETE			
@END
	
;DETERMINE MT TILE_TYPE								
	LDY #$00
.LOOP.MT.SEARCH							
	LDA ANIMATION.MT.TRACKING,Y			;LOAD NEXT MT INDEX
	STA ANIMATION.MT.INDEX				
	CMP #$FF							;#$FF IS THE ARRAY STOP VALUE. IF IT'S BEEN REACHED, THIS IS THE FIRST TILE FOR THIS MOB TO BE PROCESSED.
	BEQ .MT.INDEX.NOT_FOUND

	CPX ANIMATION.MT.INDEX				;is the current mob record index (X-REG) == current ANIMATION.MT.TRACKING record? (ANIMATION.MT.INDEX)
	BNE	.NEXT_RECORD					;if no, next ANIMATION.MT.TRACKING record
	LDA ANIMATION.MT.TRACKING+$1,Y		;LOAD MT TILE # (0-3)
	STA ANIMATION.MT.TILE_NUMBER

;ADJUST IF ON CERTAIN SCREEN EDGES (FOR OTHER EDGES SEE .MT.INDEX.NOT_FOUND)
	;LAST COLUMN
	LDA MAP_OBJECT.RECORD.READ+$0		;LOAD X-AXIS
	CMP #MAP_OBJECTS.X.LAST_COLUMN		
	BNE .NEXT_TEST1
	LDA ANIMATION.MT.TILE_NUMBER		;IF X = LAST COLUMN, AND MT TILE = #$01, THEN ADJUST MT TILE# TO #$02
	CMP #$01
	BNE .NEXT_TEST1
	INC ANIMATION.MT.TILE_NUMBER
.NEXT_TEST1

	;FIRST COLUMN (HANDLED WHEN RECORD IS FIRST RECORDED IN .MT.INDEX.NOT_FOUND)
	;LAST ROW (NOT NEEDED BECAUSE THE FIRST TWO TILES APPEARING ONSCREEN ARE MT TILE1, MT TILE2
	;FIRST ROW ((HANDLED WHEN RECORD IS FIRST RECORDED IN .MT.INDEX.NOT_FOUND)

.TESTS_DONE
	STY ANIMATION.YREG.STORAGE1				;SAVE LOOP COUNTER FOR .LOOP.MT.SEARCH
	LDY MAP_OBJECT.RECORD.READ+$2		;LOAD PRELIMINARY TILE TYPE
	STX ANIMATION.XREG.STORAGE				;SAVE MOB RECORD INDEX
	LDX #$01
.LOOP.MT.CALC.TILE_TYPE		
	INY									;Y-REG WILL HOLD THE FINAL TILE_TYPE WHEN THIS LOOP COMPLETES
	CPX ANIMATION.MT.TILE_NUMBER		;THE MT TILES #S ARE SEQUENTIAL, IN INCREMENTS OF 1, SO USING THE MT TILE NUMBER AS THE STOP VALUE IS PERFECT
	BEQ	.DONE3
	INX									;INCREMENT LOOP COUNTER
	JMP .LOOP.MT.CALC.TILE_TYPE
.DONE3
	STY ANIMATION.MT.FINAL_TILE_TYPE	;STORE VALUE TO FREE UP Y-REG AND ACC
	LDY ANIMATION.YREG.STORAGE1				;RESTORE LOOP COUNTER FOR .LOOP.MT.SEARCH, WHICH IS INDEX TO THE TRACKING ARRAY

;INCREMENT AND SAVE THE MT TILE NUMBER TO THE TRACKING ARRAY
	INC ANIMATION.MT.TILE_NUMBER				
	LDA ANIMATION.MT.TILE_NUMBER
	STA ANIMATION.MT.TRACKING+$1,Y	
			

	
;RESTORE REGISTERS, LOAD FINAL TILE_TYPE AND EXIT	
	LDX ANIMATION.XREG.STORAGE				;RESTORE MOB RECORD INDEX	
	LDY ANIMATION.YREG.STORAGE				;RESTORE OUTERLOOP COUNTER 
	LDA ANIMATION.MT.FINAL_TILE_TYPE	;RETURN FINAL TILE_TYPE IN ACC
	JMP TEST.TILE_TYPE
	
.NEXT_RECORD
	INY									;INCREMENT LOOP COUNTER TWICE
	INY
	CPY #$10							;IF THE MOB'S RECORD INDEX ISN'T FOUND BY THE END OF THE ARRAY'S MEMORY SIZE (#$10), THEN FALL THROUGH TO .MT.INDEX.NOT_FOUND
	BNE .LOOP.MT.SEARCH	
	LDY #$00							;SET Y-REG TO #$00 BECAUSE THIS IS WHERE THE MOB'S RECORD INDEX WILL GET RECORDED
	;***FALLS THROUGH
	
.MT.INDEX.NOT_FOUND
	TXA									;TRANSFER MOB RECORD INDEX TO THE ACC
	STA ANIMATION.MT.TRACKING,Y			;SAVE MOB'S RECORD INDEX TO THE TRACKING ARRAY

;ADJUST FOR CERTAIN SCREEN EDGES (SEE .LOOP.MT.SEARCH FOR OTHER EDGES)
	LDA MAP_OBJECT.RECORD.READ+$0		;LOAD X-AXIS
	CMP MAP_OBJECTS.X_APPROACH+$1		
	BNE .NOT_FIRST_COLUMN_APPROACH

	;FIRST COLUMN, APPROACH
	LDA #$03							;IF FIRST COLUMN, SET MT TILE NUMBER FOR NEXT ITERATION TO #$03. THIS ELIMINATES A NEED FOR AN ADJUSTMENT IN .LOOP.MT.SEARCH 
	STA ANIMATION.MT.TRACKING+$1,Y
	LDY MAP_OBJECT.RECORD.READ+$2		;LOAD MOB BASE TILE TYPE
	INY									;IN FIRST COLUMN, MT #$01 SHOULD BE DISPLAYED FIRST
	TYA									;RETURN TILE TYPE IN ACC
	JMP .EXIT.FINAL

.NOT_FIRST_COLUMN_APPROACH
	LDA MAP_OBJECT.RECORD.READ+$1		;LOAD Y-AXIS
	CMP MAP_OBJECTS.Y_APPROACH+$1		;IS MOB IN FIRST ROW -1?
	BNE .NO_EDGE_CASES
	;FIRST ROW, APPROACH
	LDA #$03							;IF FIRST COLUMN, SET MT TILE NUMBER FOR NEXT ITERATION TO #$03. THIS ELIMINATES A NEED FOR AN ADJUSTMENT IN .LOOP.MT.SEARCH 
	STA ANIMATION.MT.TRACKING+$1,Y
	LDY MAP_OBJECT.RECORD.READ+$2		;LOAD MOB BASE TILE TYPE
	INY
	INY									;IN FIRST COLUMN, MT #$02 SHOULD BE DISPLAYED FIRST
	TYA									;RETURN TILE TYPE IN ACC

	JMP .EXIT.FINAL

.NO_EDGE_CASES
	LDA #$01							;START MT TILE# AT #$01 SINCE TILE#0 WILL BE RETURNED THIS ITERATION
	STA ANIMATION.MT.TRACKING+$1,Y

	;***FALLS THROUGH
@END

.EXIT2

			
	LDY ANIMATION.YREG.STORAGE				;RESTORE OUTERLOOP COUNTER

;**OPT** Memory. Now that the map object read subroutine has been implemented at the top of this routine,
;I suspect that I can make some of this S_ENTITY type based code more efficient. 

	;does S_ENTITY share space with the MAP.OBJECTS.MOB array?
	LDA SCREEN.MO_SPRITE_TYPE.DATA,Y
	AND #$87 ;mask-out MTT flags (bits 3-6)
	CMP #S_ENTITY_TYPE.NC_MOB ;is type non-combat mob?
	BEQ .USES.MOB.ARRAY
	CMP #S_ENTITY_TYPE.PC ;is type combat player character?
	BEQ .USES.MOB.ARRAY

;Uses MAP_OBJECTS.NPC array (TYPE = NPC or combat MOB)
	LDA MAP_OBJECTS.NPC+$7,X			;load at-anchor movement flag
	CMP #$FF							;is no-draw flag set? (used when NPC records are parked at ladder anchors becuase they are scheduled to be on a different floor of a building)
	BNE .DEFAULT						;if no, return tile type of NPC record (like normal)
	LDA MAP_OBJECTS.NPC+$6,X			;load transit flag
	BNE .DEFAULT						;if NPC is in transit, return tile type of NPC record (like normal)
	LDA #ANIMATION.TILE_RANGE.START
	SEC
	SBC #$01							;return a tile type outside the animaiton range so that an animation frame is not drawn for this NPC.
	JMP .EXIT.FINAL
.DEFAULT	
	LDA MAP_OBJECTS.NPC+$2,X			;LOAD NPC TILE TYPE AND RETURN VIA ACC	
	JMP .EXIT.FINAL
	
.USES.MOB.ARRAY
	LDA MAP_OBJECTS.MOB+$2,X			;LOAD MOB TILE TYPE AND RETURN VIA ACC	
	;**FALLS THROUGH**


			
.EXIT.FINAL
	LDY ANIMATION.YREG.STORAGE				;RESTORE OUTERLOOP COUNTER
			
	JMP TEST.TILE_TYPE 
@END
	
ANIMATION.SCROLL.PLAYER	;SCROLLS PLAYER ICON TO CREATE SINKING EFFECT
@START
;PARAMTERS: ACC (TILE TYPE)
;ENTRANCE: DESIGNED FOR MAIN ANIMATION ROUTINE
;RETURN: NONE

;=====================SUBROUTINE DOCUMENTATION====================================
;This subroutine is called by TEST.TILE_TYPE (ANIMATION UPDATE) if the 
;Tile_ID being processed is a Tile_ID that requires this feature (i.e. quicksand)
;
;The principle of this routine is essentially the same as the routines in Graphics_Scrolling.ASM.
;The difference is that the scrolling is applies to a single tile instead of the entire tile grid.
;
;The scolling technique as applied to a single tile is as follows:
;	*Line 15 is copied to line 16. Line 14 is copied to line 15, and so on
;		until every line of the tile has been copied to the line below it. 
;
;The result of this is that the 2 screen bytes in the last line of the
;tile (last before scrolling) are overwritten and are no longer displayed
;on the graphics screen. 
;
;These changes are made to the player icon in the player icon buffer.
;Since the DRAW.TILE.PLAYER routine is setup use the data in the player icon buffer
;when drawing the walking player icon, the player's "sunk" appearance will
;persist, even if the player moves, unless the routines in Movement_Manager.ASM
;reset the player icon buffer by calling LOAD.PLAYER.WALKING.ICON, which 
;they do once the player walks onto a non-quicksand tile.  
;=================================================================================

		;*****DO NOT MODIFY CONTENTS OF ACC, IT'S A PARAMETER!!!!!
		


;SAVE REGISTERS
	STX ANIMATION.XREG.STORAGE
	STY ANIMATION.YREG.STORAGE			

;CONTINUE SCROLLING PLAYER ICON?
	CMP #TILE_ID.QUICKSAND				;IS PLAYER STANDING IN QUICKSAND
	BNE .OTHER.TERRAIN	
	LDA PLAYER.HEIGHT					;IF YES, HAS HE ALREADY SUNK TO MAX (ABOUT WAIST DEEP)
	CMP #TILE.QUICKSAND.SINK.HEIGHT
	BCS .START							;IF NO, THEN SCROLL 
	JMP .EXIT							;IF YES, THEN EXIT

.OTHER.TERRAIN	
;ERROR: UNEXPECTD TILE_ID WAS ALLOWED TO ENTER THIS ROUTINE
	LDA #$A6
	JSR FULL.BRK
	
	
.START	
;INCREMENT GLOBAL COUNTERS
	INC ANIMATION.SCREEN.TALLY
	DEC PLAYER.HEIGHT
	
;SETUP GRAPHICS SCREEN VARIABLES					
	LDA SCREEN.INDEX.TILE_SBYTE,Y
	STA	TEMP
	STA SCREEN.DRAW.BYTE1
	
	LDA SCREEN.INDEX.TILE_LINE,Y
	STA TILE.LINE.FIRST
	STA TILE.LINE.STOP
	DEC TILE.LINE.STOP
	CLC
	ADC #$F			;******NEEDS TO BE 1 LESS THAN TILE.DEPTH.STANDARD
	STA TILE.LINE.COPY_TO
	STA TILE.LINE.COPY_FROM
	DEC TILE.LINE.COPY_FROM

;INIT OTHER VARIABLES
	LDA #$1F
	STA ANIMATION.SCROLL.COUNTER
	
	

.LOOP.SCROLL
;=========SCROLL TILE=============
@START
	LDX	TILE.LINE.COPY_TO		
	LDY SCREEN.DRAW.BYTE1	;THIS IS REALLY TILE SCREEN BYTE0
	INY						;ADVANCE INDEX TO TILE SCREEN BYTE1
	
.GET.LINE.ADDRESS1A.2
	LDA PAGE.BACKGROUND		;SPECIFY PAGE TO GET ADDRESS FOR	
	CMP #$02
	BEQ .ADDR1A.LOOKUP.PAGE2.2
	
.ADDR1A.LOOKUP.PAGE1.2
	LDA LINE.HO.P1,X			;GET LINE ADDRESS
	STA LINE.BASE.ADDR1+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR1
	JMP .LOOKUP1A.COMPLETE.2

.ADDR1A.LOOKUP.PAGE2.2
	LDA LINE.HO.P2,X			;GET LINE ADDRESS
	STA LINE.BASE.ADDR1+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR1

.LOOKUP1A.COMPLETE.2
	
.GET.LINE.ADDRESS2A.2

	LDX TILE.LINE.COPY_FROM
	
	LDA PAGE.BACKGROUND			;SPECIFY PAGE TO GET ADDRESS FOR
	CMP #$02
	BEQ .ADDR2A.LOOKUP.PAGE2.2
	
.ADDR2A.LOOKUP.PAGE1.2
	LDA LINE.HO.P1,X		;GET LINE ADDRESS
	STA LINE.BASE.ADDR2+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR2
	JMP .LOOKUP2A.COMPLETE.2

.ADDR2A.LOOKUP.PAGE2.2
	LDA LINE.HO.P2,X		;GET LINE ADDRESS
	STA LINE.BASE.ADDR2+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR2
.LOOKUP2A.COMPLETE.2

@MIDDLE
	
;COPY TILE SCREEN BYTE1
	LDA (LINE.BASE.ADDR2),Y				;TILE DATA: COPY FROM	
	STA (LINE.BASE.ADDR1),Y				;TILE DATA: COPY TO
	
	STY ANIMATION.YREG.STORAGE1	
	LDY ANIMATION.SCROLL.COUNTER		;TRACKS BYTES READ BY THIS LOOP
	STA PLAYER.ICON.BUFFER,Y	
	DEC ANIMATION.SCROLL.COUNTER		;IT RUNS BACKWARDS BECAUSE THIS LOOP STARTS COPYING BYTES AT THE BOTTOM OF THE TILE
	LDY ANIMATION.YREG.STORAGE1
	
	DEY									;SWITCH TO SCREEN BYTE0

;COPY TILE SCREEN BYTE0
	LDA (LINE.BASE.ADDR2),Y				;TILE DATA: COPY FROM	
	STA (LINE.BASE.ADDR1),Y				;TILE DATA: COPY TO
	
	STY ANIMATION.YREG.STORAGE1	
	LDY ANIMATION.SCROLL.COUNTER		;TRACKS BYTES READ BY THIS LOOP
	STA PLAYER.ICON.BUFFER,Y	
	DEC ANIMATION.SCROLL.COUNTER		;IT RUNS BACKWARDS BECAUSE THIS LOOP STARTS COPYING BYTES AT THE BOTTOM OF THE TILE
;		LDY ANIMATION.YREG.STORAGE1
		
	DEC TILE.LINE.COPY_TO				;NEXT TILE LINE
	DEC TILE.LINE.COPY_FROM
	
	
	;NO NEED TO RESET TO SCREEN BYTE1, THAT IS DONE AT THE START OF THE LOOP
	
	DEX									;X-REG CONTAINS VALUE OF TILE.LINE.COPY_FROM BEFORE THE DEC ABOVE. SO DOING A DEX IS THE FUNCTIONALY THE SAME AS AND LDX TILE.LINE.COPY_FROM EXCEPT IT IS 2 CLOCK CYCLES FASTER. 
	CPX TILE.LINE.STOP					;IS TILE DONE?	
	BNE .LOOP.SCROLL					;IF NO, DRAW NEXT LINE (BCC: is ACC < CMP value)

@END
	
.EXIT
;RESTORE REGISTERS
	LDX ANIMATION.XREG.STORAGE
	LDY ANIMATION.YREG.STORAGE	
	
	RTS

@END

; ANIMATION.SCROLL.WATER
@START
; ;SAVE REGISTERS
	; STX ANIMATION.XREG.STORAGE
	; STY ANIMATION.YREG.STORAGE			


;=====================SUBROUTINE DOCUMENTATION====================================
;This subroutine was setup to scroll water tiles as the method of animating
;them instead of flipping frames. It works but it seemed choppier than 
;frame flipping for animating water so I didn't use it. The choppyness
;might be fixable but since frame flipping was working well once I started
;writing animation to the background page, I moved on.
;
;This code may be useful for future purposes. It was the starting point
;for the ANIMATION.SCOLL.PLAYER subroutine which creates the quicksand
;sinking effect. 
;=================================================================================
				
	; INC ANIMATION.SCREEN.TALLY
	
	; LDA SCREEN.INDEX.TILE_SBYTE,Y
	; STA	TEMP
	; STA SCREEN.DRAW.BYTE1
	
	; LDA SCREEN.INDEX.TILE_LINE,Y
	; STA TILE.LINE.FIRST
	; STA TILE.LINE.STOP
	; DEC TILE.LINE.STOP
	; CLC
	; ADC #$F			;******NEEDS TO BE 1 LESS THAN TILE.DEPTH.STANDARD
; ;#TILE.DEPTH.STANDARD			;ADD DEPTH OF SHAPE TO STARTING LINE IN HEX (# of lines, not the position of last line....so line positions $0-$F is $10 (!16) total lines)	
; ;	STA TILE.LINE.START
	; STA TILE.LINE.COPY_TO
	; STA TILE.LINE.COPY_FROM
	; DEC TILE.LINE.COPY_FROM
; ;	STA TILE.LINE.STOP					;SET THE LAST LINE TO DRAW BASED ON THE DEPTH OF THE TILE	
	
	
; ;=======COPY LAST LINE OF TILE TO BUFFER====


			
	; LDX TILE.LINE.COPY_TO
	; LDY SCREEN.DRAW.BYTE1

; ;GET LINE ADDRESS FOR 1ST LINE OF TILE
; .GET.LINE.ADDRESS1A.1
	; LDA PAGE.FOREGROUND		;SPECIFY PAGE TO GET ADDRESS FOR	
	; CMP #$02
	; BEQ .ADDR1A.LOOKUP.PAGE2.1
	
; .ADDR1A.LOOKUP.PAGE1.1	
	; LDA LINE.HO.P1,X		;GET LINE ADDRESS
	; STA LINE.BASE.ADDR1+$1
	; LDA LINE.LO,X
	; STA LINE.BASE.ADDR1
	; JMP .LOOKUP1A.COMPLETE.1

; .ADDR1A.LOOKUP.PAGE2.1
	; LDA LINE.HO.P2,X		;GET LINE ADDRESS
	; STA LINE.BASE.ADDR1+$1
	; LDA LINE.LO,X
	; STA LINE.BASE.ADDR1

; .LOOKUP1A.COMPLETE.1	
; ;COPY TILE SCREEN BYTE0
	; LDA (LINE.BASE.ADDR1),Y				;TILE DATA: COPY FROM	
	; STA ZONE_TOOLS.OUTPUT_BUFFER		;TILE DATA: COPY TO   (borrwing this buffer from the zone manager)

	; INY									;ADVANCE TO NEXT SCREEN BYTE

; ;COPY TILE SCREEN BYTE1
	; LDA (LINE.BASE.ADDR1),Y				;TILE DATA: COPY FROM	
	; STA ZONE_TOOLS.OUTPUT_BUFFER+$1		;TILE DATA: COPY TO (borrwing this buffer from the zone manager)




; ;=========SCROLL REMAINING LINES OF TILE=============

; .LOOP.SCROLL
	
	; LDX	TILE.LINE.COPY_TO		
	; LDY SCREEN.DRAW.BYTE1		
	
; .GET.LINE.ADDRESS1A.2
	; LDA PAGE.FOREGROUND		;SPECIFY PAGE TO GET ADDRESS FOR	
	; CMP #$02
	; BEQ .ADDR1A.LOOKUP.PAGE2.2
	
; .ADDR1A.LOOKUP.PAGE1.2
	; LDA LINE.HO.P1,X		;GET LINE ADDRESS
	; STA LINE.BASE.ADDR1+$1
	; LDA LINE.LO,X
	; STA LINE.BASE.ADDR1
	; JMP .LOOKUP1A.COMPLETE.2

; .ADDR1A.LOOKUP.PAGE2.2
	; LDA LINE.HO.P2,X		;GET LINE ADDRESS
	; STA LINE.BASE.ADDR1+$1
	; LDA LINE.LO,X
	; STA LINE.BASE.ADDR1

; .LOOKUP1A.COMPLETE.2
	
; .GET.LINE.ADDRESS2A.2

	; LDX TILE.LINE.COPY_FROM
	
	; LDA PAGE.FOREGROUND			;SPECIFY PAGE TO GET ADDRESS FOR
	; CMP #$02
	; BEQ .ADDR2A.LOOKUP.PAGE2.2
	
; .ADDR2A.LOOKUP.PAGE1.2
	; LDA LINE.HO.P1,X		;GET LINE ADDRESS
	; STA LINE.BASE.ADDR2+$1
	; LDA LINE.LO,X
	; STA LINE.BASE.ADDR2
	; JMP .LOOKUP2A.COMPLETE.2

; .ADDR2A.LOOKUP.PAGE2.2
	; LDA LINE.HO.P2,X		;GET LINE ADDRESS
	; STA LINE.BASE.ADDR2+$1
	; LDA LINE.LO,X
	; STA LINE.BASE.ADDR2
; .LOOKUP2A.COMPLETE.2

				
; ;COPY TILE SCREEN BYTE0
	; LDA (LINE.BASE.ADDR2),Y				;TILE DATA: COPY FROM	
	; STA (LINE.BASE.ADDR1),Y				;TILE DATA: COPY TO
	
	; INY									;ADVANCE TO NEXT SCREEN BYTE

; ;COPY TILE SCREEN BYTE1
	; LDA (LINE.BASE.ADDR2),Y				;TILE DATA: COPY FROM	
	; STA (LINE.BASE.ADDR1),Y				;TILE DATA: COPY TO
	
			
	; DEC TILE.LINE.COPY_TO				;NEXT TILE LINE
	; DEC TILE.LINE.COPY_FROM
	
	; DEY									;RESET SCREEN BYTE TO 0
	
	; DEX									;X-REG CONTAINS VALUE OF TILE.LINE.COPY_FROM BEFORE THE DEC ABOVE. SO DOING A DEX IS THE FUNCTIONALY THE SAME AS AND LDX TILE.LINE.COPY_FROM EXCEPT IT IS 2 CLOCK CYCLES FASTER. 
	; CPX TILE.LINE.STOP					;IS TILE DONE?	
	; BNE .LOOP.SCROLL					;IF NO, DRAW NEXT LINE (BCC: is ACC < CMP value)


				
		

; ;=======COPY BUFFER TO FIRST LINE====

	; LDX TILE.LINE.FIRST
	; LDY SCREEN.DRAW.BYTE1

; ;GET LINE ADDRESS FOR 1ST LINE OF TILE
; .GET.LINE.ADDRESS1A.3
	; LDA PAGE.FOREGROUND		;SPECIFY PAGE TO GET ADDRESS FOR	
	; CMP #$02
	; BEQ .ADDR1A.LOOKUP.PAGE2.3
	
; .ADDR1A.LOOKUP.PAGE1.3	
	; LDA LINE.HO.P1,X		;GET LINE ADDRESS
	; STA LINE.BASE.ADDR1+$1
	; LDA LINE.LO,X
	; STA LINE.BASE.ADDR1
	; JMP .LOOKUP1A.COMPLETE.3

; .ADDR1A.LOOKUP.PAGE2.3
	; LDA LINE.HO.P2,X		;GET LINE ADDRESS
	; STA LINE.BASE.ADDR1+$1
	; LDA LINE.LO,X
	; STA LINE.BASE.ADDR1
; .LOOKUP1A.COMPLETE.3

				
						
; ;COPY TILE SCREEN BYTE0
	; LDA ZONE_TOOLS.OUTPUT_BUFFER		;TILE DATA: COPY FROM
	; STA (LINE.BASE.ADDR1),Y				;TILE DATA: COPY TO
	
	; INY									;ADVANCE TO NEXT SCREEN BYTE

; ;COPY TILE SCREEN BYTE1
	; LDA ZONE_TOOLS.OUTPUT_BUFFER+$1		;TILE DATA: COPY TO	
	; STA (LINE.BASE.ADDR1),Y				;TILE DATA: COPY FROM	
	
			

; .EXIT
; ;RESTORE REGISTERS
	; LDX ANIMATION.XREG.STORAGE
	; LDY ANIMATION.YREG.STORAGE			
	; JMP EXIT_TEST	


@END