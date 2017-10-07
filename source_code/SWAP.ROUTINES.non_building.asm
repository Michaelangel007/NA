; ============================================
; Copyright (C) 2016-2018. 6502 Workshop, LLC
; contact: mark@6502workshop.com
; ============================================



;************************INCLUDE FILE*****************************
;(do not assemble stand-alone.  )
				.TF     swap.routines.non_building.bin,BIN
				.OR		$9600			**Always put before .TF directive and never use again in program
				.EB		OFF


;====SWAP.ROUTINES.NON_BUILDING.ASM DOCUMENTATION====================================
;
;These are routines which are needed when a non-building map is loaded, and are not needed
;when any of the modules are loaded which use the main memory swap are (i.e. NPC Talk, Combat, Inventory)
;
;As a result, these routines can be loaded into the main memory swap area and they will be
;swapped out to aux memory when one of the modules is loaded. 
;
;Since there are also routines which are only needed when a building map is loaded, memory is saved
;by only having the "building" or "non-building" routines in memory at any given time. 
;
;
;=================================================================================

MOB.GENERATION ;=========MANAGES THE RANDOM GENERATION OF MOBS ON THE MAP======
@START

;=====================*TOP LEVEL* SUBROUTINE DOCUMENTATION====================================
;
;Note: The tile_type for mobs is determined in .DECIDE.LAND_MOB and in .DECIDE.SEA_MOB
;
;This subroutine is called by GAME.PRIMARY_LOOP while waiting for a keypress.
;This subroutine does an abort-if-key-pressed check several times during it's run-time
;which is to avoid slowing down player movement. 
;
;Normally only one mob generation attempt per player turn is permitted. 
;However, a key feature is that aborts are tallied and this routine will
;run each time it is called from the game loop if there are unprocessed aborts.
;However, when processing aborted attempts, a probability check is still applied
;to determine if a mob should be generated. 
;
;=================================================================================


;IDEAS:
;	1) BALANCING SPEED WITH TIMELY GENERATION OF MOBS
;					Maybe instead of queuing all keypress aborts, we could pick a few generation zones on the map
;					few simple mob types it could create there. Of course the problem becomes mobs piling up there, unless they are SS. 

;====DETERMINE WHETHER TO ATTEMPT MOB GENERATION====
@START
;=====================CODE SECTION DOCUMENTATION====================================
;The determination is made based on whether a) it is the player's turn (only one mob 
;generation attempt per player move is permitted) and b) a random number probability check 
;=================================================================================

START
;IS IT MOBs turn or player's turn?
	LDA GAME.MOB_GEN.CONTROL
	CMP #$01
	BEQ .TURN.CHECK.COMPLETE
	JMP EXIT_FINAL
.TURN.CHECK.COMPLETE	
	
.GENERATE.DECIDE
;GENERATE MOB THIS TURN?

	; LDA MOB.GEN.QUEUE					;IF ABORTED RUNS OF MOB.GENERATION ARE IN THE QUEUE, BYPASS PROBABILITY CHECK.
	; BEQ .GENERATE.DECIDE.PROB
	; LDA #$01
	; STA MOB.GEN.QUEUE_LOCK				;LOCK THE QUEUE SO IT DOENSN'T INCREASE IF AN ABORT OCCURS WHEN PROCESSING THE QUEUE

	; JMP .GENERATE.START

.GENERATE.DECIDE.PROB
	;USE DEFAULT RANDOM # RANGE
	JSR RANDOM.8

	CMP MOB.GEN.PROBABILITY				;CHANCE THAT GAME WILL ATTEMPT TO GENERATE A MOB
	BCC .GENERATE.START	
	JMP EXIT_FINAL

@END
	
;=====DETERMINE LOCATION OF NEW MOB=====
@START
;=====================CODE SECTION DOCUMENTATION====================================
;If this code section is reached, the random number probability check for deciding
;whether to attempt mob generation passed. 
;
;The regional map location of new mob is determined by a two 8-bit random numbers, 
;the ranges of which are set so that they can be used as the HO/LO byte
;of RMAP. 
;
;Once the new mob's RMAP is calculated, collision checks are applied 
;to determine whether a new mob can be created at the RMAP location.
;
;Since mob locations are tracked using an x,y relative to the player position
;the mob's RMAP is converted to relative x,y. 
;=================================================================================

.GENERATE.START
;Random # range
;Random LO: 01-FF, HO: 00-0F

;INIT VARIABLES		
	LDA #$00
	STA MOB.GEN.SEA_FLAG				;RESET. #$01 TELLS .DECIDE.MOB_TYPE TO CHOOSE A SEA MOB TYPE				

;=====GENERATE RANDOM MAP POSITION (RMAP.X/Y)===========
@START	
.RANDOM.RMAP.X
	;Uses default range ($01-$FF) (RANDOM.8 resets the range to these values on return)

	LDA #$01
	STA RND.ABORT
	JSR RANDOM.8				;get random # ($01-$FF)
	AND #$3F					;mask out bit 7 & 8, resulting in a value $0 - $3F. The fact that the AND mask value is also #$3F is a coincidence. 
	CMP #RZONE.ARRAY.OFFSET		;is random number in RMAP range? 
	BCC .RANDOM.RMAP.X.IN_RANGE	;if yes, the keep the random number as is. 
	SEC							;if no, reduce the random # so that it is in range. And SBC of at least #$10 is required to ensure an in-range number. I used #$20. The significance is that it determines which area of the map will end up weighted more heavily for random mob appearances since there is double the chance of that range of values being used for RMAP 
	SBC #$20					;must be at least #$10
	;**FALLS THROUGH** (WITH ACC VALUE)
.RANDOM.RMAP.X.IN_RANGE
	;RANDOM # IS IN ACC
	STA MOB.CANDIDATE.MAP.X 	;RMAP. save random # as RMAP.X	

	LDA KB_BUFFER					;LOAD NEXT KEY PRESS FROM BUFFER
    BPL .RANDOM.RMAP.Y				;IF NO KEY PRESS, THEN CONTINUE
	JMP EXIT.ABORT					;IF KEY PRESSED, LEAVE IT IN BUFFER AND RETURN TO GAME LOOP FOR KEYPRESS PROCESSING

.RANDOM.RMAP.Y	
	LDA #$01
	STA RND.ABORT
	JSR RANDOM.8				;get random # ($01-$FF)
	AND #$3F					;mask out bit 7 & 8, resulting in a value $0 - $3F. The fact that the AND mask value is also #$3F is a coincidence. 
	CMP #RZONE.ARRAY.OFFSET		;is random number in RMAP range? 
	BCC .RANDOM.RMAP.Y.IN_RANGE	;if yes, the keep the random number as is. 
	SEC							;if no, reduce the random # so that it is in range. And SBC of at least #$10 is required to ensure an in-range number. I used #$20. The significance is that it determines which area of the map will end up weighted more heavily for random mob appearances since there is double the chance of that range of values being used for RMAP 
	SBC #$20					;must be at least #$10

	;**FALLS THROUGH** (WITH ACC VALUE)
.RANDOM.RMAP.Y.IN_RANGE
	;RANDOM # IS IN ACC
	STA MOB.CANDIDATE.MAP.X 	;RMAP. save random # as RMAP.Y

	LDA KB_BUFFER					;LOAD NEXT KEY PRESS FROM BUFFER
    BPL .CALCULATE.MOB.MAP			;IF NO KEY PRESS, THEN CONTINUE
	JMP EXIT.ABORT					;IF KEY PRESSED, LEAVE IT IN BUFFER AND RETURN TO GAME LOOP FOR KEYPRESS PROCESSING

@END

;======CALCULATE RMAP FROM RMAP.XY======
.CALCULATE.MOB.MAP			
@START

	LDY MOB.CANDIDATE.MAP.Y	;RMAP
	LDA RMAP.MULTIPLY_TABLE.HO,Y
	STA MOB.CANDIDATE.RMAP+$1
	LDA RMAP.MULTIPLY_TABLE.LO,Y
	STA MOB.CANDIDATE.RMAP

;16-BIT ADD
	CLC
	ADC MOB.CANDIDATE.MAP.X
	STA MOB.CANDIDATE.RMAP					
	LDA MOB.CANDIDATE.RMAP+$1
	ADC #$00
	STA MOB.CANDIDATE.RMAP+$1

.NO_FLIP
@END

;**note: MOB.CANDIDATE.MAP.X/Y CONTAIN RMAP BEFORE THIS POINT AND GMAP AFTER THIS POINT. SAME VARIABLES IS USED. 
.CONVERT.RMAP_XY.GMAP_XY ;CONVERTS A RMAP.XY VALUE TO GMAP.XY VALUE (uses player's GMAP as a reference point)
@START

	; LDA MOB.CANDIDATE.MAP.X
	; STA PARM.RMAP.X
	
	; LDA MOB.CANDIDATE.MAP.Y
	; STA PARM.RMAP.Y

.CONVERSION.START	
	LDA RMAP.X				;player RMAP X-axis
	CMP MOB.CANDIDATE.MAP.X			;RMAP-to-convert X-Axis
	BCC .MOB.MO_X_LESS
	
	SEC
	SBC MOB.CANDIDATE.MAP.X			;RMAP-to-convert X-Axis	
	STA MAP_OBJECTS.X_ADJ	;==distance between player and to-convert x-axis

	LDA GMAP.X				;load player GMAP x-axis
	SBC	MAP_OBJECTS.X_ADJ	;subtract distance
	STA MOB.CANDIDATE.MAP.X		;result is the GMAP.X of the to-convert RMAP.X

	JMP .MOB.YTEST

.MOB.MO_X_LESS ;(Player RMAP.X less than TO-CONVERT RMAP.X)
	LDA MOB.CANDIDATE.MAP.X			;RMAP-to-convert X-Axis
	
	SEC
	SBC RMAP.X				;player RMAP X-axis
	STA MAP_OBJECTS.X_ADJ	;==distance between player and to-convert x-axis
	
	LDA GMAP.X				;Add distance 
	CLC										
	ADC MAP_OBJECTS.X_ADJ								
	STA MOB.CANDIDATE.MAP.X		;result is the GMAP.X of the to-convert RMAP.X
	
	;**FALLS THROUGH**
	
.MOB.YTEST	
	LDA RMAP.Y				;player RMAP Y-axis
	CMP MOB.CANDIDATE.MAP.Y			;RMAP-to-convert Y-Axis
	BCC .MOB.MO_Y_LESS
	
	SEC
	SBC MOB.CANDIDATE.MAP.Y			;to-convert RMAP Y-Axis
	STA MAP_OBJECTS.Y_ADJ	;==distance between player and to covnert y-axis

	LDA GMAP.Y				;load player GMAP y-axis
	SBC	MAP_OBJECTS.Y_ADJ	;subtract distance
	STA MOB.CANDIDATE.MAP.Y		;result is the GMAP.Y of the to-convert RMAP.Y
	
	JMP .CONVERSION.COMPLETE

.MOB.MO_Y_LESS ;(Player RMAP.Y less than TO-CONVERT RMAP.Y )
	LDA MOB.CANDIDATE.MAP.Y			;RMAP-to-convert Y-axis	
	SEC
	SBC RMAP.Y				;player RMAP Y-axis
	STA MAP_OBJECTS.Y_ADJ	;==distance between player and to-convert y-axis
	
	LDA GMAP.Y				;load player GMAP y-axis
	CLC										
	ADC MAP_OBJECTS.Y_ADJ	;add distance							
	STA MOB.CANDIDATE.MAP.Y	;result is the GMAP.Y of the to-convert RMAP.Y
	;**FALLS THROUGH**	
.CONVERSION.COMPLETE
;RECORD FINAL VALUES	
	; LDA RETURN.GMAP.X
	; STA MOB.CANDIDATE.MAP.X
	
	; LDA RETURN.GMAP.Y
	; STA MOB.CANDIDATE.MAP.Y
	
@END

;======COLLISION CHECKS & DETERMINE LAND VS SEA MOB======
@START
;=======INLINE CODE FOR ADC.16========	
;RZONE.ARRAY(2) + RMAP (2)
	
	CLC                          			;ALWAYS BEFORE ADD
	LDA #RZONE.ARRAY						;OP1
	ADC MOB.CANDIDATE.RMAP					;OP2  
	STA RZONE.ARRAY.INDEX_ROW
				 
	LDA /RZONE.ARRAY						;OP1+$1
	ADC MOB.CANDIDATE.RMAP+$1				;OP2+$2 carry flag not cleared via CLC intentionally, it's part of 16-bit adding. 
	STA RZONE.ARRAY.INDEX_ROW+$1
;======================================	

	LDY #$00
	LDA (RZONE.ARRAY.INDEX_ROW),Y

	CMP #MOB.GEN_FLAG.GRE			;IS CANDIDATE LOCATION WATER?
	BCS .WATER_TEST1.PASS			;WATER TEST #1
	JMP .COLLISION.LAND_MOB
.WATER_TEST1.PASS
	CMP #MOB.GEN_FLAG.LT2			;WATER TEST #2	
	BCC .COLLISION.SEA_MOB
	;**FALLS THROUGH**
.COLLISION.LAND_MOB
	
	CMP #MOB.GEN_FLAG.LT1			;ABSOLUTE OBSTACLE?			
	BCC .KEY_CHECK					;IF YES, THEN CHECK FOR KEY PRESS BEFORE REGENERATING RANDOM LOCATION FOR CANDIDATE MOB
	JMP .LAND_MOB.NEXT_TEST
.KEY_CHECK
	LDA KB_BUFFER					;LOAD NEXT KEY PRESS FROM BUFFER
    BPL .GENERATE.START_STEP		;IF NO KEY PRESS, REGENERATE RANDOM LOCATION FOR CANDIDATE MOB
	JMP EXIT.ABORT					;IF KEY PRESSED, LEAVE IT IN BUFFER AND RETURN TO GAME LOOP FOR KEYPRESS PROCESSING
	
.GENERATE.START_STEP
	JMP .GENERATE.START

.LAND_MOB.NEXT_TEST

;OTHER CHECKS (NOT YET IMPLEMENTED)	
				;Player is in the tile, or adjacent to the tile..this is because there is a 1 turn lag between a mob being created and a mob appearing on the screen. the adjacent exclusion prevents a collision detection on the player for an object not drawn on the screen. 
				;A building is in the tile
	JMP .COLLISION.COMMON
	
.COLLISION.SEA_MOB	

	LDA #$01
	STA MOB.GEN.SEA_FLAG				;TELLS .DECIDE.MOB_TYPE TO CHOOSE A SEA MOB TYPE
;###DISABLE SEA MOB GENERATION BY PUTTING JMP GENERATE.START IN THIS LINE (NOT TESTED)
	;JMP GENERATE.START
	;****FALLS THROUGH...SHOULD IT?
@MIDDLE

.COLLISION.COMMON ;COLLISION CHECKS COMMON TO LAND & SEA

;(OLD)CALCULATE MAP X,Y OF PLAYER RMAP
@START
	; ;*******THIS MAY BE A GOOD PLACE FOR A KEY CHECK, BEFORE RESTARTING THE GENERATION	

	; ;**OPT** Memory. Speed. Once GMAP.X and GMAP.Y are in use this may not be necessary
	
	; LDA RMAP
	; STA DIVIDEND
	; LDA RMAP+$1
	; STA DIVIDEND+$1
	; LDA	#OFFSET.UP
	; STA DIVISOR
	; LDA #$00
	; STA DIVISOR+$1
	
	; JSR DIV.16
	
	; LDA RESULT 
	; STA PLAYER.MAP.Y								;QUOTIENT IS MAP Y-AXIS
	; LDA RESULT+$2
	; STA PLAYER.MAP.X								;REMAINDER IS MAP X-AXIS

; ;CALCULATE THE MOB'S RELATIVE X,Y
	; LDA MOB.CANDIDATE.MAP.X
	; CMP PLAYER.MAP.X
	; BCC .MOB.MO_X_LESS
	
	; SEC
	; SBC PLAYER.MAP.X
	; STA MAP_OBJECTS.X_ADJ
	; LDA #MAP_OBJECTS.PLAYER_LOCATION
	; CLC
	; ADC MAP_OBJECTS.X_ADJ
	; STA MOB.CANDIDATE.RELATIVE.X

	; JMP .MOB.YTEST

; .MOB.MO_X_LESS
	; LDA PLAYER.MAP.X
	; SEC
	; SBC MOB.CANDIDATE.MAP.X
	; STA MAP_OBJECTS.X_ADJ
	; LDA #MAP_OBJECTS.PLAYER_LOCATION
	; SEC
	; SBC MAP_OBJECTS.X_ADJ
	; STA MOB.CANDIDATE.RELATIVE.X

; .MOB.YTEST	

	; LDA MOB.CANDIDATE.MAP.Y
	; CMP PLAYER.MAP.Y
	; BCC .MOB.MO_Y_LESS
	
	; LDA MOB.CANDIDATE.MAP.Y
	; SEC
	; SBC PLAYER.MAP.Y
	; STA MAP_OBJECTS.Y_ADJ
	
	; LDA #MAP_OBJECTS.PLAYER_LOCATION
	; CLC
	; ADC MAP_OBJECTS.Y_ADJ
	; STA MOB.CANDIDATE.RELATIVE.Y
		
	; JMP .RELATIVE.X_Y.COMPLETE

; .MOB.MO_Y_LESS
	; LDA PLAYER.MAP.Y
	; SEC
	; SBC MOB.CANDIDATE.MAP.Y
	; STA MAP_OBJECTS.Y_ADJ
	
	; LDA #MAP_OBJECTS.PLAYER_LOCATION
	; SEC	
	; SBC MAP_OBJECTS.Y_ADJ
	; STA MOB.CANDIDATE.RELATIVE.Y

; .RELATIVE.X_Y.COMPLETE	
@END

;COMMON COLLISION CHECKS
	;Another Mob is in the tile
	;Transport is in the tile
@END
@END
	
;=========	
DECIDE.MOB_TYPE ;
@START
;=====================CODE-SECTION DOCUMENTATION====================================
;If a prospective new mob passes the collision checks, then the last step
;is to decide (via random numbers) the Tile_ID of the mob, it's flags,
;and setup a new map object record. 
;=================================================================================

	;(for now there is only one land mob tile, so skipping calcualtion)

;WAS SEA MOB OR LAND MOB ALREADY DETERMINED (BASED ON RMAP OF MOB)	
	LDA MOB.GEN.SEA_FLAG
	BNE .DECIDE.SEA_MOB		
.DECIDE.LAND_MOB
	LDA #$97
	STA MOB.CANDIDATE.TYPE	
	JMP DECIDE.MOB_FLAGS
.DECIDE.SEA_MOB
	LDA #$9F			;flag is $8C
	STA MOB.CANDIDATE.TYPE	
	;***FALLS THROUGH

DECIDE.MOB_FLAGS ;

;4 FLAG_OPTIONS: SPLIT POINTS $3F, $74, $BF
	JSR RANDOM.8
	CMP #$3F
	BCS .FLAG_OPTIONS.NEXT_TEST1
;.OPT0
	LDA #$00	
	JMP .FLAG.SET
.FLAG_OPTIONS.NEXT_TEST1
	CMP #$BF
	BCS .FLAG_OPTIONS.OPT3
;.FLAG_OPTIONS.NEXT_TEST2
	CMP #$74
	BCS .FLAG_OPTIONS.OPT2
;.OPT1
	LDA MOB.GEN.SS_QTY	
	CMP #MOB.GEN.SS_LIMIT
	BCS .SS_AT_MAX_LIMIT
	LDA #$03
	JMP .FLAG.SET
.SS_AT_MAX_LIMIT						;OPT1 IS EITHER FLAG #$03 (aggressive SS) or $00 (non-ss less aggresive), depending on whether the maximum SS in the current region has been reached
	LDA #$00
	JMP .FLAG.SET
.FLAG_OPTIONS.OPT2
	LDA #$01
	JMP .FLAG.SET
.FLAG_OPTIONS.OPT3
	LDA #$05
	JMP .FLAG.SET

.FLAG.SET
				
	STA MOB.CANDIDATE.FLAGS				;SET FLAG BASED ON ACC VALUE LOADED ABOVE

@MIDDLE

RECORD_ENTRANCE	
;RECORD NEW MOB RECORD

;=====================CODE-SECTION DOCUMENTATION====================================
;
;This code section searches the mob map object array for an open record.
;If no open record can be found an existing record is overwritten. However,
;a variable is used as an index to the record to overwrite. This variable
;is incremented each time there is no open records found because always
;overwriting the same record would potentially reduce the variety of
;mobs in the array. And, if the same record was overwritten and the mob
;was on the view screen, the player could observe the mob change it's type
;several time rapidly if there were aborts on the queue. 
;=================================================================================

	LDX #$00
.RECORD_LOOP
	LDA MAP_OBJECTS.MOB+$2,X
	CMP #$00								;#$00 IS THE ARRAY STOP VALUE
	BEQ .OPEN_RECORD_FOUND
	TXA
	CLC
	ADC #MAP_OBJECTS.RECORD_LENGTH
;	BEQ EXIT2								;IF COUNTER FLIPS TO #$00, THEN NO OPEN RECORDS AVAILABLE, EXIT
	BEQ .ARRAY_FULL							;IF COUNTER FLIPS TO #$00, THEN NO OPEN RECORDS AVAILABLE, EXIT

	TAX
	JMP .RECORD_LOOP

.ARRAY_FULL
	LDA MOB.GEN.ARRAY_FULL_COUNTER
	CLC
	ADC #MAP_OBJECTS.RECORD_LENGTH
	STA MOB.GEN.ARRAY_FULL_COUNTER
	TAX
	;**FALLS THROUGH
	
.OPEN_RECORD_FOUND
;	LDA MOB.CANDIDATE.RELATIVE.X
	LDA MOB.CANDIDATE.MAP.X		;GMAP
	STA MAP_OBJECTS.MOB+$0,X

;	LDA MOB.CANDIDATE.RELATIVE.Y
	LDA MOB.CANDIDATE.MAP.Y		;GMAP
	STA MAP_OBJECTS.MOB+$1,X

	LDA MOB.CANDIDATE.TYPE
	STA MAP_OBJECTS.MOB+$2,X

	LDA MOB.CANDIDATE.FLAGS
	STA MAP_OBJECTS.MOB+$3,X

@END	
EXIT2
@START
	LDA MOB.GEN.QUEUE
	CMP #$00							;**OPT** Memory. Speed. I think all CMP #$00s can be removed becuase BEQ/BNE trigger if the last operation results in 0, including an LDA. Also search for CMP #$01s because they could probably be convertd to test for $00. 
	BEQ .QUEUE_EMPTY							
	DEC	MOB.GEN.QUEUE					;IF THERE ARE ABORTED MOB GENERATIONS IN QUEUE, EXIT WITHOUT SETTING TURN BACK TO PLAYER
	JMP EXIT_FINAL

.QUEUE_EMPTY
	LDA #$00							;SET TURN TO PLAYER
	STA	GAME.MOB_GEN.CONTROL
	;**FALLS THROUGH

EXIT_FINAL

	RTS

EXIT.ABORT								;USED IF PLAYER PRESSES A KEY DURING THIS ROUTINE, KEEPS TURN SET TO MOB FOR GENERATION
	INC MOB.GEN.QUEUE	

	STA RND.HI	
	RTS
@END
@END


;INCLUDE FILES

	.IN 	c:\my_code\na\source_code\zone_functions

		;**OPT** Disk. Reduce the .NO to the actual amount of memory needed for the routines in this file. 
	.NO $A000 ;fill up to top of reserved memory block so that overflows are reported by SBASM
	
	
