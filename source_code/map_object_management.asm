; ============================================
; Copyright (C) 2016-2018. 6502 Workshop, LLC
; contact: mark@6502workshop.com
; ============================================


;************************INCLUDE FILE*****************************
;(do not assemble stand-alone.  )

;=====================MAP_OBJECT_MANAGEMENT.ASM DOCUMENTATION====================================
;
;See Chart 1.2 (my_code/documentation) for an illistration of the flow control
;of MO.DRAW. The other subroutines have very straightforward flow control.
;
;The subroutines in this file have the following functions:
;	MO.BOARD		player presses key to board a map object 
;	MO.XIT			player presses key to exit a map object 
;	MO.CREATE		create a new transport map object using parameters passed
;	MO.DRAW			draw map objects on the screen, handle mob moves
;  MOB.GENERATION	randomly generate mobs in a random location, random mob type. nothing predictable here...it's all random. 
;
;
;=================================================================================

MO.BOARD ; ========PLAYER ATTEMPTS BOARD MAP OBJECT======
@START
;PARAMETERS: NONE
;ENTRANCE: DIRECT
;RETURN: PLAYER.TILE.ACTIVE, PLAYER.TRANSPORT.ACTIVE

;=====================SUBROUTINE DOCUMENTATION====================================
;
;This subroutine is called by GAME.PRIMARY_LOOP when the player executes the command
;to (B)oard a transport map object. 
;
;The player's screen tile location is checked for a transport map object and if one exists
;the variables that track active transport and the player icon's tile are updated to reflect the 
;transport object that was boarded.
;
;Additional checks apply for Frigates as they can carry skiffs. If the Frigate
;already has the max number of skiffs, and the player's active transport is a skiff, then 
;the board attempt fails. 
;=================================================================================

;NOTE: An active transport map object is removed from the screen array.
				
;VERIFY A TRANSPORT MO IS AT PLAYER LOCATION
	LDY #SCREEN.ARRAY.PLAYER_LOCATION
	LDA SCREEN.MO_GENERAL.DATA,Y				;LOAD MO RECORD INDEX FROM SCREEN ARRAY FOR CURRENT PLAYER LOCATION
	CMP #$FF									;IS RECORD EMPTY? (I.E. NO MO AT THIS LOCATION)
	BEQ BOARD.NOTHING_TO_BOARD_STEP				;IF YES, EXIT
	TAX											;IF NO, USE INDEX TO LOOKUP TILE TYPE OF MO FROM MO RECORD.

;MAP OBJECT FOUND: IS IT A TRANSPORT MO?
	LDA MAP_OBJECTS.GENERAL+$2,X				;LOAD TILE TYPE OF TRANSPORT	
	CMP #TILE_ID.HORSE_C						;BRANCH BASED ON THE TYPE OF TRASNPORT MO AT THE PLAYER LOCATION
	BEQ BOARD.HORSE
	CMP #TILE_ID.FRIGATE1.1
	BEQ BOARD.FRIGATE
	CMP #TILE_ID.CARAVEL
	BEQ BOARD.CARAVEL
	CMP #TILE_ID.WYVERN
	BEQ BOARD.WYVERN
	CMP #TILE_ID.SKIFF
	BEQ BOARD.SKIFF

;NO TRANSPORT MO FOUND
	JMP BOARD.NOTHING_TO_BOARD_STEP
	
@MIDDLE
BOARD.NOTHING_TO_BOARD_STEP
	JMP BOARD.NOTHING_TO_BOARD
	
BOARD.WYVERN
	;***NOTHING ELSE TO DO, TRANSPORT MO INDEX ALREADY SAVED TO PLAYER.TRANSPORT.ACTIVE		
	STA PLAYER.TILE.ACTIVE					;STORES HORSE AS THE ACTIVE TRASNPORT TILE
	STX PLAYER.TRANSPORT.ACTIVE					;SAVE INDEX TO BOARDED OBJECT, TO LINK THE PLAYER TO THIS TRANSPORT MO RECORD
		
	JMP BOARD.EXIT
	
BOARD.HORSE
	;***NOTHING ELSE TO DO, TRANSPORT MO INDEX ALREADY SAVED TO PLAYER.TRANSPORT.ACTIVE		
	STA PLAYER.TILE.ACTIVE					;STORES HORSE AS THE ACTIVE TRASNPORT TILE
	STX PLAYER.TRANSPORT.ACTIVE					;SAVE INDEX TO BOARDED OBJECT, TO LINK THE PLAYER TO THIS TRANSPORT MO RECORD

	JMP BOARD.EXIT

BOARD.SKIFF				
	STA PLAYER.TILE.ACTIVE					;STORES SKIFF AS THE ACTIVE TRASNPORT TILE
	STX PLAYER.TRANSPORT.ACTIVE					;SAVE INDEX TO BOARDED OBJECT, TO LINK THE PLAYER TO THIS TRANSPORT MO RECORD
	JMP BOARD.EXIT

BOARD.CARAVEL
	STA PLAYER.TILE.ACTIVE					;STORES SKIFF AS THE ACTIVE TRASNPORT TILE
	STX PLAYER.TRANSPORT.ACTIVE					;SAVE INDEX TO BOARDED OBJECT, TO LINK THE PLAYER TO THIS TRANSPORT MO RECORD
	JMP BOARD.EXIT	
	

@MIDDLE	
BOARD.FRIGATE
	STA SAVED.ACC.LOCAL 		;save tile_type of mo being boarded

					
;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA
.EXTRACT_FLAGS
;EXTRACT MOB BOOLEAN FLAGS	
	LDA MAP_OBJECTS.GENERAL+$3,X
	LDY #$00	
.LOOP	
	LSR
	BCS .CARRY_IS_SET					
.CARRY_IS_CLEAR					;the carry is not Katie
	STA SAVED.ACC.LOCAL2
	LDA #$00
	JMP .WRITE_FLAG_VALUE
.CARRY_IS_SET							;THE CARRY IS KATIE
	STA SAVED.ACC.LOCAL2
	LDA #$01

.WRITE_FLAG_VALUE
	STA MAP_OBJECTS.GENERAL_FLAGS,Y
	CPY #$07
	BEQ .FLAGS_DONE
	INY
	LDA SAVED.ACC.LOCAL2
	JMP .LOOP
.FLAGS_DONE	

;RESTORE REGISTERS
	PLA
	TYA
	PLA
	TXA
	

;DID PLAYER HAVE A SKIFF AS ACTIVE TRANSPORT, PRIOR TO CURRENT (B)OARD COMMAND?
	LDY PLAYER.TRANSPORT.ACTIVE	
	LDA MAP_OBJECTS.GENERAL+$2,Y
	CMP #TILE_ID.SKIFF							
	BEQ .BOARDING.WITH.SKIFF

;NO SKIFF ACTIVE, BOARDING FRIGATE FROM WALKING MODE			
	LDA SAVED.ACC.LOCAL				;LOAD TILE TYPE OF OBJECT BEING BOARDED
	STA PLAYER.TILE.ACTIVE			;STORES FRIGATE AS THE ACTIVE TRASNPORT TILE
	STX PLAYER.TRANSPORT.ACTIVE		;SAVE INDEX TO BOARDED OBJECT, TO LINK THE PLAYER TO THIS TRANSPORT MO RECORD	

	JMP .CENTER_FRIGATE_ONSCREEN	

	
.TOO_MANY_SKIFFS						;WHICH IS NOT AS BIG A PROBLEM AS TOO MANY COOKS
	JSR PLAY.SOUND.DUMB_ASS
	RTS
	
.BOARDING.WITH.SKIFF
	LDA GENERAL.FLAG1							;IF FLAG IS SET TO $01 THEN FRIGATE HAS 2 SKIFFS
	BNE .TOO_MANY_SKIFFS	
		
;BOARDING SUCCESSFULL: UPDATE RECORDS

;INCREMENT SKIFFS ON FRIGATE
	LDA GENERAL.FLAG0							;IS FLAG SET TO #$01
	BNE .HAS.1SKIFF

.HAS.0SKIFFS
	LDA #$01 									;INCREASE SKIFFS ON FRIGATE TO 1. NOTE: THIS IS WRITING THE HEX VALUE OF THE FLAG COMBINATION TO REFLECT 1 SKIFF. SEE THE FLAG CHART IN MAP OBJECTS.XLS
	STA MAP_OBJECTS.GENERAL+$3,X
	JMP .UPDATE.ACTIVE.TRANSPORT
	
.HAS.1SKIFF
;EXIT WITH SKIFF
	LDA #$02 									;INCREASE SKIFFS ON FRIGATE TO 2. NOTE: THIS IS WRITING THE HEX VALUE OF THE FLAG COMBINATION TO REFLECT 2 SKIFF. SEE THE FLAG CHART IN MAP OBJECTS.XLS
	STA MAP_OBJECTS.GENERAL+$3,X
	;**FALLS THROUGH**
	
.UPDATE.ACTIVE.TRANSPORT	
;UPDATE ACTIVE TRANSPORT	
	STX PLAYER.TRANSPORT.ACTIVE					;SAVE INDEX TO BOARDED OBJECT, TO LINK THE PLAYER TO THIS TRANSPORT MO RECORD		
	LDA SAVED.ACC.LOCAL
	STA PLAYER.TILE.ACTIVE					;STORES FRIGATE AS THE ACTIVE TRASNPORT TILE	

;ERASE SKIFF FROM MAP OBJECTS ARRAY
	LDA #$00
	STA MAP_OBJECTS.GENERAL,Y					;ERASE MOB'S MAP OBJECT RECORD
	STA MAP_OBJECTS.GENERAL+$1,Y				;Y-REG CONTAINS THE INDEX TO THE MO RECORD OF THE SKIFF WHICH WAS THE ACTIVE TRANSPORT PRIOR TO BOARDING THE FRIGATE
	STA MAP_OBJECTS.GENERAL+$2,Y
	STA MAP_OBJECTS.GENERAL+$3,Y	
;	JMP BOARD.EXIT
	;**FALLS THROUGH
	
.CENTER_FRIGATE_ONSCREEN	
;CONVERT THE GMAP OF FRIGATE TO PLAYER RELATIVE X.Y

		LDA MAP_OBJECTS.GENERAL+$0,X	;load GMAP X-axis of frigate's upper left tile
		STA PARM.GMAP.X
		LDA MAP_OBJECTS.GENERAL+$1,X	;load GMAP Y-axis of frigate's upper left tile
		STA PARM.GMAP.Y
	JSR CONVERT.GMAP.TO.PLAYER_RELATIVE.XY
		;RETURN VALUES: RETURN.RELATIVE.X, RETURN.RELATIVE.Y
		

			
;IDENTIFY WHICH TILE OF THE SHIP THE PLAYER WAS LOCATED ON WHEN EXECUTING BOARD COMMAND	
	LDA RETURN.RELATIVE.X				;load player relative X-AXIS of frigate's upper left tile
	CMP #$7F
	BEQ .X7F
	CMP #$80
	BEQ .X80
	;**FALLS THROUGH
.ERROR2
;FRIGATE X-AXIS IN UNEXPECTED POSITION IN .CENTER_FRIGATE_ONSCREEN
	TAY
	;LDA #$AE
	JSR FULL.BRK
	

.X7F
	LDA RETURN.RELATIVE.Y				;load player relative X-AXIS of frigate's upper left tile
	CMP #$7F
	BEQ .X7F.Y7F
	CMP #$80
	BEQ .X7F.Y80

.ERROR3
;FRIGATE Y-AXIS IN UNEXPECTED POSITION IN .CENTER_FRIGATE_ONSCREEN
;
;DISABLE.BS_RAM
	JSR PREP.BRK
	BRK
	
.X80
	LDA RETURN.RELATIVE.Y				;load player relative X-AXIS of frigate's upper left tile
	CMP #$7F
	BEQ .X80.Y7F
	CMP #$80
	BEQ .X80.Y80						

.ERROR4
;FRIGATE Y-AXIS IN UNEXPECTED POSITION IN .CENTER_FRIGATE_ONSCREEN
	LDA TEXT
	BRK
	

.X80.Y80 ;NW: PLAYER IS LOCATED ON THE NORTHWEST TILE OF THE FRIGATE 
	JSR MOVE.PASS
	JMP .UPDATE.SCREEN.ARRAY
	
.X7F.Y80 ;NE: PLAYER IS LOCATED ON THE NORTHEAST TILE OF THE FRIGATE		
;UPDATE FRIGATE LOCATION
	
	;LDA #MAP_OBJECTS.PLAYER_LOCATION
	LDA GMAP.X
	STA MAP_OBJECTS.GENERAL,X
	LDA GMAP.Y
	STA MAP_OBJECTS.GENERAL+$1,X

;UPDATE PLAYER LOCATION	
	LDA #$01
	STA PLAYER.MOVE.FORCED						;SET FORCE MOVE FLAG SO MOVE.N/S/E/W WON'T DO COLLISION CHECKS OR CHECK FOR A KEYPRESS TO ABORT TO ALTERNATE GAME LOOP ENTRANCE (THIS NOT RETURNING VIA RTS TO THIS ROUTINE)
	
	JSR MOVE.WEST
	
	LDA #$00
	STA PLAYER.MOVE.FORCED						;RESET FLAG TO DEFAULT (OFF).
	
	JMP .UPDATE.SCREEN.ARRAY
	
.X80.Y7F ;SW: PLAYER IS LOCATED ON THE SOUTHWEST TILE OF THE FRIGATE

;UPDATE FRIGATE LOCATION
	; LDA #MAP_OBJECTS.PLAYER_LOCATION
	; STA MAP_OBJECTS.GENERAL,X
	; STA MAP_OBJECTS.GENERAL+$1,X

	INC MAP_OBJECTS.GENERAL+$1,X					;UPDATE FRIGATE X-AXIS LOCATION

;UPDATE PLAYER LOCATION	
	LDA #$01
	STA PLAYER.MOVE.FORCED						;SET FORCE MOVE FLAG SO MOVE.N/S/E/W WON'T DO COLLISION CHECKS OR CHECK FOR A KEYPRESS TO ABORT TO ALTERNATE GAME LOOP ENTRANCE (THIS NOT RETURNING VIA RTS TO THIS ROUTINE)
	
	JSR MOVE.NORTH
	
	LDA #$00
	STA PLAYER.MOVE.FORCED						;RESET FLAG TO DEFAULT (OFF).
	
	JMP .UPDATE.SCREEN.ARRAY
	
.X7F.Y7F ;SE: PLAYER IS LOCATED ON THE SOUTHEAST TILE OF THE FRIGATE	
;UPDATE PLAYER & FRIGATE LOCATION
				
	INC MAP_OBJECTS.GENERAL,X					;UPDATE FRIGATE X-AXIS LOCATION
	
	LDA #$01
	STA PLAYER.MOVE.FORCED						;SET FORCE MOVE FLAG SO MOVE.N/S/E/W WON'T DO COLLISION CHECKS OR CHECK FOR A KEYPRESS TO ABORT TO ALTERNATE GAME LOOP ENTRANCE (THIS NOT RETURNING VIA RTS TO THIS ROUTINE)
	
	TXA											
	PHA											;TRANSFER MO RECORD INDEX TO STACK
	JSR MOVE.WEST
	PLA											;RESTORE MO RECORD INDEX FROM STACK
	TAX

	INC MAP_OBJECTS.GENERAL+$1,X				;UPDATE FRIGATE Y-AXIS LOCATION

	JSR MOVE.NORTH

	
	LDA #$00
	STA PLAYER.MOVE.FORCED						;RESET FLAG TO DEFAULT (OFF).
	
;	JMP .UPDATE.SCREEN.ARRAY	
	
	;**FALLS THROUGH

.UPDATE.SCREEN.ARRAY		
;REMOVE FRIGATE FROM SCREEN ARRAY
;This is so that if a map object like a skiff moved onto another map object like a frigate, that the non-active transport object will be in the screen array so it can be detected for boarding purposes. 
;All transport objects are removed from screen array for consistency. The code is also setup to expect that active transport isn't in the screen array and I've seen problems like the tranport leaving a trail of itself when is left in screen array.
	LDY #PLAYER.TRANSPORT.MT.TILE_LOCATION0		;LOAD PLAYER TILES AS INDEX	 (SCREEN ARRAYS HAVEN'T BEEN SCROLLED YET FOR CURRENT PLAYER MOVE)
	LDA #$FF									;THIS IS THE VALUE FOR NO MO RECORD 
	STA SCREEN.MO_GENERAL.DATA,Y				;ERASE TRANSPORT FROM POSITION, BEFORE CURRENT PLAYER MOVE, IN SCREEN ARRAY

	LDY #PLAYER.TRANSPORT.MT.TILE_LOCATION1		;LOAD PLAYER TILES AS INDEX	 (SCREEN ARRAYS HAVEN'T BEEN SCROLLED YET FOR CURRENT PLAYER MOVE)
	LDA #$FF									;THIS IS THE VALUE FOR NO MO RECORD 
	STA SCREEN.MO_GENERAL.DATA,Y				;ERASE TRANSPORT FROM POSITION, BEFORE CURRENT PLAYER MOVE, IN SCREEN ARRAY
	
	LDY #PLAYER.TRANSPORT.MT.TILE_LOCATION2		;LOAD PLAYER TILES AS INDEX	 (SCREEN ARRAYS HAVEN'T BEEN SCROLLED YET FOR CURRENT PLAYER MOVE)
	LDA #$FF									;THIS IS THE VALUE FOR NO MO RECORD 
	STA SCREEN.MO_GENERAL.DATA,Y				;ERASE TRANSPORT FROM POSITION, BEFORE CURRENT PLAYER MOVE, IN SCREEN ARRAY
	
	LDY #PLAYER.TRANSPORT.MT.TILE_LOCATION3		;LOAD PLAYER TILES AS INDEX	 (SCREEN ARRAYS HAVEN'T BEEN SCROLLED YET FOR CURRENT PLAYER MOVE)
	LDA #$FF									;THIS IS THE VALUE FOR NO MO RECORD 
	STA SCREEN.MO_GENERAL.DATA,Y				;ERASE TRANSPORT FROM POSITION, BEFORE CURRENT PLAYER MOVE, IN SCREEN ARRAY

	RTS			

@MIDDLE
	
BOARD.NOTHING_TO_BOARD	
	JSR PLAY.SOUND.DUMB_ASS
	RTS

BOARD.EXIT
	; LDY #SCREEN.ARRAY.PLAYER_LOCATION			;LOAD PLAYER TILES AS INDEX	 (SCREEN ARRAYS HAVEN'T BEEN SCROLLED YET FOR CURRENT PLAYER MOVE)
	; LDA #$FF									;THIS IS THE VALUE FOR NO MO RECORD 
	; STA SCREEN.MO_GENERAL.DATA,Y				;ERASE TRANSPORT FROM POSITION, BEFORE CURRENT PLAYER MOVE, IN SCREEN ARRAY


			; LDA #$01
			; STA TROUBLESHOOTING.HOOK
			
	JSR MOVE.PASS
	
				
	RTS
@END
	
MO.XIT ; ========PLAYER ATTEMPS X.IT MAP OBJECT======
@START
;PARAMETERS: NONE
;ENTRANCE: DIRECT
;RETURN: PLAYER.TILE.ACTIVE, PLAYER.TRANSPORT.ACTIVE, PLAYER.WALKING.TILE, [PLAYER.TRANSPORT.SPEED], [PLAYER.TRANSPORT.SPEED]

;=====================SUBROUTINE DOCUMENTATION====================================
;
;This subroutine is called by GAME.PRIMARY_LOOP when the player executes the command
;to (X)it a transport map object. 
;
;There are no scenarios currently where an (X)it fails. The player is returned to walking mode immediately except
;if the player's active transport is a frigate of a skiff. 
;Frigate:  a check is done to see if the Frigate has any skiffs. If it
;		 	does, then the player're active transport is changed to a skiff.
;Skiff: a check is done to see if the player is standing on a frigate. 
;			If yes, then the skiff is added to the Frigate's skiff qty and 
;			the player is returned to walking mode.
;
;Variables are updated to return the player to walking mode and if the tranport
;object subject the (X)it has the ability to increase movement speed (i.e horse, Wyvern)
;then variables are updated to return the player to walking speed and not-flying status if
;appropriate. 
;=================================================================================

;VERIFY PLAYER HAS TRANSPORT ACTIVE
	LDX PLAYER.TRANSPORT.ACTIVE					;LOAD TRANSPORT MO RECORD INDEX
	STX SAVED.XREG.LOCAL						;SAVE INDEX FOR FUTURE USE 
	CPX #$FF									;IS PLAYER WALKING?
	BEQ XIT.NOTHING_TO_EXIT						;IF YES, DON'T MODIFY ANY MO RECORDS, PROCEED TO UPDATE VIDEO SCREEN

;EXIT THE TRANSPORT OBJECT	
	LDA #$FF									;SET TRANSPORT STATUS TO WALKING
	STA PLAYER.TRANSPORT.ACTIVE
	
	LDA MAP_OBJECTS.GENERAL+$2,X				;LOAD TILE TYPE OF ACTIVE TRANSPORT
	CMP #TILE_ID.HORSE_C						;BRANCH BASED ON THE TYPE OF TRASNPORT MO AT THE PLAYER LOCATION
	BEQ XIT.HORSE
	CMP #TILE_ID.FRIGATE1.1
	BEQ XIT.FRIGATE
	CMP #TILE_ID.CARAVEL
	BEQ XIT.CARAVEL
	CMP #TILE_ID.SKIFF
	BEQ .XIT.SKIFF_STEP
	CMP #TILE_ID.WYVERN
	BEQ XIT.WYVERN	

.ERROR											;IF TILE_TYPE NOT RECOGNIZED, ERROR OUT
;TILE_TYPE OF TRANSPORT PLAYER ATTEMPTED TO BOARD NOT RECOGNIZED IN MO.XIT
	BRK
@MIDDLE
.XIT.SKIFF_STEP
	JMP XIT.SKIFF

XIT.NOTHING_TO_EXIT
	JSR PLAY.SOUND.DUMB_ASS
	RTS

XIT.WYVERN		
	LDA #$00									
	STA PLAYER.TRANSPORT.SPEED					;RESET FAST TRANSPORT STATUS TO OFF
	STA PLAYER.TRANSPORT.STATUS					;""
	
	LDA PLAYER.WALKING.TILE.DEFAULT
	STA PLAYER.WALKING.TILE
	STA PLAYER.TILE.ACTIVE						;RESET TO WALKING PLAYER ICON
	
	LDA PLAYER.DARKNESS_OVERRIDE				;WAS PLAYER FLYING?
	BNE .PLAYER_WAS_FLYING						;IF YES, TURN OVER DARKNESS OVERRIDE

	JSR MOVE.PASS
	RTS

.PLAYER_WAS_FLYING
	LDA #$00
	STA PLAYER.DARKNESS_OVERRIDE				
	
	JSR MOVE.PASS
	RTS
	
;NOTE: the exit sequence below was necessary before XIT was setup to 
;automatically return to the game loop via GAME.LAUNCH. See GAME.PRIMARY_LOOP
;documentation for the reason why that got setup. 

	; PLA											;REMOVE RTS ADDED TO THE STACK WHEN MO.BOARD WAS CALLED
	; PLA

	; JMP GAME.LAUNCH								;DO A FRESH SCREEN DRAW TO FILL IN ANY DARK TILES

XIT.HORSE		
	LDA #$00									
	STA PLAYER.TRANSPORT.SPEED					;RESET FAST TRANSPORT STATUS TO OFF
	STA PLAYER.TRANSPORT.STATUS					;""

	JMP XIT.WALKING_MODE


XIT.CARAVEL
	JMP XIT.WALKING_MODE

@MIDDLE
	
XIT.FRIGATE
.EXTRACT_FLAGS
;EXTRACT MOB BOOLEAN FLAGS	
	LDA MAP_OBJECTS.GENERAL+$3,X
	LDY #$00	
.LOOP	
	LSR
	BCS .CARRY_IS_SET					
.CARRY_IS_CLEAR							;THE CARRY IS NOT KATIE
	STA SAVED.ACC.LOCAL
	LDA #$00
	JMP .WRITE_FLAG_VALUE
.CARRY_IS_SET							;THE CARRY IS KATIE
	STA SAVED.ACC.LOCAL
	LDA #$01

.WRITE_FLAG_VALUE
	STA MAP_OBJECTS.GENERAL_FLAGS,Y
	CPY #$07
	BEQ .FLAGS_DONE
	INY
	LDA SAVED.ACC.LOCAL
	JMP .LOOP
.FLAGS_DONE

;DOES THE FRIGATE HAVE ANY SKIFFS?
	LDA GENERAL.FLAG1							;IS FLAG SET TO #$01
	BNE .XIT.HAS.2SKIFFS	
	LDA GENERAL.FLAG0							;IS FLAG SET TO #$01
	BNE .XIT.HAS.1SKIFF
	
	JMP XIT.WALKING_MODE						;IF NO SKIFFS, EXIT TO WALKING MODE
	
.XIT.HAS.2SKIFFS
	LDA #$01 									;REDUCE SKIFFS ON FRIGATE TO 1. NOTE: THIS IS WRITING THE HEX VALUE OF THE FLAG COMBINATION TO REFLECT 1 SKIFF. SEE THE FLAG CHART IN MAP OBJECTS.XLS
	STA MAP_OBJECTS.GENERAL+$3,X	
	JMP .UPDATE.RECORDS
	
.XIT.HAS.1SKIFF
;EXIT WITH SKIFF
	LDA #$00 									;REDUCE SKIFFS ON FRIGATE TO 0. NOTE: THIS IS WRITING THE HEX VALUE OF THE FLAG COMBINATION TO REFLECT 1 SKIFF. SEE THE FLAG CHART IN MAP OBJECTS.XLS
	STA MAP_OBJECTS.GENERAL+$3,X
	;**FALLS THROUGH**
	
.UPDATE.RECORDS	
	LDA #TILE_ID.SKIFF
	STA PLAYER.TILE.ACTIVE					;STORES FRIGATE AS THE ACTIVE TRANSPORT TILE	

;CREATE NEW SKIFF RECORD IN MAP OBJECTS ARRAY

	;SET PARAMETERS FOR MO.TRANSPORT.CREATE 
	;LDA #MAP_OBJECTS.PLAYER_LOCATION
	LDA GMAP.X	;load player GMAP X-Axis
	STA MAP_OBJECTS.CREATE.TRANSPORT.X

	;LDA #MAP_OBJECTS.PLAYER_LOCATION				
	LDA GMAP.Y	;load player GMAP Y-Axis
	STA MAP_OBJECTS.CREATE.TRANSPORT.Y	
	
	LDA #TILE_ID.SKIFF		
	STA MAP_OBJECTS.CREATE.TRANSPORT.TILE_TYPE	

	LDA #$00
	STA MAP_OBJECTS.CREATE.TRANSPORT.SKIFFS			

	
	;CREATE MAP OBJECT FOR SKIFF
	JSR MO.TRANSPORT.CREATE						;RETURNS INDEX TO NEW MO RECORD IN X-REG

;SET NEW MO RECORD AS THE ACTIVE TRANSPORT FOR PLAYER
	STX PLAYER.TRANSPORT.ACTIVE

;EXIT
	JSR MOVE.PASS
	RTS

XIT.WALKING_MODE
	LDA PLAYER.WALKING.TILE.DEFAULT
	STA PLAYER.WALKING.TILE
	STA PLAYER.TILE.ACTIVE						;RESET TO WALKING PLAYER ICON
	
	JSR LOAD.PLAYER.WALKING.ICON
	
	JSR MOVE.PASS
	
	
	RTS

@MIDDLE
XIT.SKIFF

;IS PLAYER STANDING ON A FRIGATE?
	LDY #SCREEN.ARRAY.PLAYER_LOCATION
	LDA SCREEN.MO_GENERAL.DATA,Y				;LOAD TRANSPORT MO RECORD INDEX FROM SCREEN ARRAY FOR CURRENT PLAYER LOCATION
	CMP #$FF									;IS RECORD EMPTY? (I.E. NO MO AT THIS LOCATION)
	BEQ XIT.WALKING_MODE						;IF YES, XIT TO WALKING MODE
	TAX											;IF NO, USE INDEX TO LOOKUP TILE TYPE OF MO FROM MO RECORD.

;EXTRACT FLAGS SO WE CAN EXAMINE FRIGATE
;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA	
	
.EXTRACT_FLAGS
;EXTRACT MOB BOOLEAN FLAGS	
	LDA MAP_OBJECTS.GENERAL+$3,X
	LDY #$00	
.LOOP	
	LSR
	BCS .CARRY_IS_SET					
.CARRY_IS_CLEAR							;THE CARRY IS NOT KATIE
	STA SAVED.ACC.LOCAL
	LDA #$00
	JMP .WRITE_FLAG_VALUE
.CARRY_IS_SET							;THE CARRY IS KATIE
	STA SAVED.ACC.LOCAL
	LDA #$01

.WRITE_FLAG_VALUE
	STA MAP_OBJECTS.GENERAL_FLAGS,Y
	CPY #$07
	BEQ .FLAGS_DONE
	INY
	LDA SAVED.ACC.LOCAL
	JMP .LOOP
.FLAGS_DONE

;RESTORE REGISTERS
	PLA
	TYA
	PLA
	TXA		

	; LDA TROUBLESHOOTING.HOOK
	; CMP #$01
	; BNE .TEMP
	; LDA TEXT
			; LDA PLAYER.ACTIVE.TRANSPORT
			; ;LDA MAP_OBJECTS.GENERAL+$3,X
			; ;LDX GENERAL.FLAG0
			; ;LDY GENERAL.FLAG1	
	; BRK
; .TEMP

;OK, SO THE PLAYER IS STANDING ON A TRANSPORT MO (IN ADDITION TO ACTIVE SKIFF), IS IT A FRIGATE?
	LDA MAP_OBJECTS.GENERAL+$2,X				;LOAD TILE TYPE OF TRANSPORT	
	CMP #TILE_ID.FRIGATE1.1
	BEQ .LEAVE_SKIFF_ON_FRIGATE

	JMP XIT.WALKING_MODE						;XIT FRIGATE, RETURN TO WALKING MODE

	
.LEAVE_SKIFF_ON_FRIGATE
	LDA GENERAL.FLAG1							;DOES THE FRIGATE ALREADY HAVE THE MAX # OF SKIFFS ON IT? (CURRENTLY 2)
	BNE .TOO_MANY_SKIFFS						;ABORT THE BOARD ATTEMPT ON THE FRIGATE

	
;LEAVE SKIFF ON FRIGATE SUCCESSFULL: UPDATE RECORDS

;INCREMENT SKIFFS ON FRIGATE
	
	LDA GENERAL.FLAG0							;IS FLAG SET TO #$01
	BNE .HAS.1SKIFF

.HAS.0SKIFFS
	LDA #$01 									;INCREASE SKIFFS ON FRIGATE TO 1. NOTE: THIS IS WRITING THE HEX VALUE OF THE FLAG COMBINATION TO REFLECT 1 SKIFF. SEE THE FLAG CHART IN MAP OBJECTS.XLS
	STA MAP_OBJECTS.GENERAL+$3,X

	JMP .ERASE.SKIFF.MO
	
.HAS.1SKIFF
	LDA #$02 									;INCREASE SKIFFS ON FRIGATE TO 2. NOTE: THIS IS WRITING THE HEX VALUE OF THE FLAG COMBINATION TO REFLECT 2 SKIFF. SEE THE FLAG CHART IN MAP OBJECTS.XLS
	STA MAP_OBJECTS.GENERAL+$3,X
		

	;**FALLS THROUGH**
	
.ERASE.SKIFF.MO
;ERASE SKIFF FROM MAP OBJECTS ARRAY, SINCE IT'S NOW ON THE BOAT
	LDY SAVED.XREG.LOCAL						;XREG WAS SAVED IN THE BEGINNING OF XIT, WHICH CONTAINED THE INDEX OF THE ACTIVE TRANSPORT MO
	LDA #$00
	STA MAP_OBJECTS.GENERAL,Y					;ERASE MOB'S MAP OBJECT RECORD
	STA MAP_OBJECTS.GENERAL+$1,Y				;Y-REG CONTAINS THE INDEX TO THE MO RECORD OF THE SKIFF WHICH WAS THE ACTIVE TRANSPORT PRIOR TO BOARDING THE FRIGATE
	STA MAP_OBJECTS.GENERAL+$2,Y
	STA MAP_OBJECTS.GENERAL+$3,Y	
	JMP XIT.WALKING_MODE

.TOO_MANY_SKIFFS								;WHICH ISN'T AS BIG A PROBLEM AS TOO MANY COOKS
	LDX SAVED.XREG.LOCAL
	STX PLAYER.TRANSPORT.ACTIVE					;KEEP SKIFF AS ACTIVE TRANSPORT.
	JSR PLAY.SOUND.DUMB_ASS
	RTS
@END

MO.TRANSPORT.CREATE	;===========ADD A TRANSPORT OBJECT TO THE GENERATL MAP OBJECTS ARRAY======
@START
;PARAMETERS; MAP_OBJECTS.CREATE.TRANSPORT.X, MAP_OBJECTS.CREATE.TRANSPORT.Y, MAP_OBJECTS.CREATE.TRANSPORT.TILE_TYPE, MAP_OBJECTS.CREATE.TRANSPORT.SKIFFS
;ENTRANCE: MO.XIT, DIRECT
;RETURN: ;X-REG RETURNS THE INDEX TO THE MO RECORD CREATED

;=====================SUBROUTINE DOCUMENTATION====================================
;Used by MO.XIT to create a new skiff map object if the player (X)its a frigate carrying skiffs.
;=================================================================================


;FIND OPEN MO RECORD
	LDX #$00
.RECORD_LOOP
	LDA MAP_OBJECTS.GENERAL+$2,X
	CMP #$00								;#$00 IS THE ARRAY STOP VALUE
	BEQ .OPEN_RECORD_FOUND
	TXA
	CLC
	ADC #MAP_OBJECTS.RECORD_LENGTH
	BEQ .ARRAY_FULL							;IF COUNTER FLIPS TO #$00, THEN NO OPEN RECORDS AVAILABLE, EXIT

	TAX
	JMP .RECORD_LOOP

.ARRAY_FULL
	;IF THE TRANSPORT OBJECT ARRAY IS FULL, THE FIRST RECORD WILL ALWAYS BE OVERWRITTEN
	;**FALLS THROUGH
	
.OPEN_RECORD_FOUND
	LDA MAP_OBJECTS.CREATE.TRANSPORT.X	
	STA MAP_OBJECTS.GENERAL,X

	LDA MAP_OBJECTS.CREATE.TRANSPORT.Y
	STA MAP_OBJECTS.GENERAL+$1,X

	LDA MAP_OBJECTS.CREATE.TRANSPORT.TILE_TYPE
	STA MAP_OBJECTS.GENERAL+$2,X

	LDA MAP_OBJECTS.CREATE.TRANSPORT.SKIFFS
	STA MAP_OBJECTS.GENERAL+$3,X


;RETURN VALUE
	;X-REG RETURNS THE INDEX TO THE MO RECORD CREATED

	RTS
@END

MO.DRAW ;==========UPDATE LOCATION TO REFLECT PLAYER MOVE==============
@START
;=====================**TOP LEVEL** SUBROUTINE DOCUMENTATION====================================
;This subroutine is called by all movement routines (including MOVE.PASS) in movement_manager.ASM
;It is responsible for the following:
;				*Updating the x,y of map objects (i.e. transport and mobs) to reflect the player move because the x,y tracked for map object's is relative to the player's position
;				*Determining if mobs will move this turn, where they will move to, and if that move is permitted.
;				*After the above have been considered, drawing the mob in it's final location and erasing the mob in the prior location if needed. 
;
;
;=================================================================================


;PARAMETERES: INITIAL_SCREEN_DRAW (SET TO $01 IF CALLING FROM DRAW.SCREEN, SKIPS UPDATE TO MO X/Y THAT OCCURS AFTER PLAYER MOVE)
;RETURN: NONE
;ENTRANCE: DIRECT

;**OPT** Speed. This comment was originall written when doing the MO.ERASE routine, which erased the MOs and redrew terrain, until I realized erase wasn't needed. Not sure if this idea still applies.
;Since transport objects don't move unless boarded (which is handled through the player icon) it would probably be faster
;if the transport objects were allowed to scroll. I think this would mean modifying MO.DRAW so that transport objects are only drawn if called from draw.screen, and then
; column row/draw would need to handle drawing of transport objects if present. This would slow things down if no transport objects are present so
;it's hard to say which is better, short of a player building a boat bridge or somethign. 


;INITIAL VARIABLES, COUNTERS, INDEXES
@START
;Init Index and MO SCREEN ARRAYS to $FFs
;($FF is the flag for no map object in that tile location)

;CALLING ROUTINE CHECK: INITIAL SCREEN DRAW 

					;RTS
	LDA CALLED_BY.DRAW.SCREEN
	CMP #$01
	BNE .ENTRANCE2					;ONLY INIT MO SCREEN ARRAYS DURING INITIAL SCREEN DRAW.

	;**OPT** Speed.Memory. This init might not be needed sincea JSR to INIT.SCREEN.ARRAYS is done in GAME.LAUNCH, which is used as the entry point for post-launch routines that require a full screen draw.  
	LDY #$00
.INIT_LOOP
	LDA #$FF	
	STA SCREEN.MO_GENERAL.DATA,Y
	STA SCREEN.MO_SPRITE.DATA,Y
	STA SCREEN.MO_SPRITE_TYPE.DATA,Y
	CPY #$BA
	BEQ .DONE
	INY
	JMP .INIT_LOOP
.DONE

.ENTRANCE2

;INIT VARIABLES, INDEXES, COUNTERS
	LDX #$00						;index for map_object arrays
	STX SAVED.XREG.LOCAL			;save map object array index					
	STX MANAGE_OBJECTS.NPC_MOB.INDEX.FLAG
	STX MOB.GEN.SS_QTY				;tracks the qty of off screen ss in the current region

@END
	
PRIMARY_LOOP ;
@START
;=====================SUBROUTINE DOCUMENTATION====================================
;This subroutine iterates through the map object arrays. Once the stop value 
;for one of the arrays is reached, processing continues for the other array only, other
;than the stop value check at the top of TRANSPORT.ENTRACE and SPRITE.ENTRANCE
;
;There is a check done in several locations to detect whether this routine was called
;during an initial screen draw. If so, then all map objects on screen are drawn,
;whereas normally drawing might be skipped under some circumstances. For example,
;transport objects don't require drawing after the initial screen draw because they
;are only moving on screen because the player moved and thus screen scrolling takes care 
;of moving them to the correct location after a player move. Drawing is also 
;skipped if a mob passes for its move because as previously mentioned, it would be in the
;correct location due to graphics screen scrolling.
;
;
;The very first activity the loop does is to update the x,y position of
;the current record in the transport and mob map object arrays to reflect
;the player move. A player move is assumed because the top level subroutine 
;MO.DRAW is only* called from the movement routines in movement_mgr.ASM
;including for pass moves, though the player move adjustment to object location 
;is skipped in that event. 
;
;
;*other than the initial screen draw, in which case the x,y position adjustment is skipped) 
;=================================================================================


			
MANAGE.OBJECTS
@START
;(test each map object array for an empty record by checking $00 in the 3rd byte (tile_type) of the current record.
;If all map arrays have an empty record, then get next record)
;
;The map object array index is saved in SAVED.XREG.LOCAL,
;and X-REG is free to be used for other things during the loop.
;If the map object index is needed during the loop, load SAVED.XREG.LOCAL.

;debug: door ($14) is set to #$24 here when X= $0C
;debug: door ($14) is set to #$10 here when X= $00

;debug: door ($0C) is set to #$10 here when X= $10
;debug: door ($0C) is set to #$10 here when X= $30




					
					
	LDA MAP_OBJECTS.GENERAL+$2,X			;empty record?
	CMP #$00	
	BNE GENERAL.ENTRANCE
	LDA MAP_OBJECTS.MOB+$2,X				;empty record?
	CMP #$00	
	BNE .SPRITE_ENTRANCE_STEP	
	LDA MAP_OBJECTS.NPC+$2,X				;empty record?
	CMP #$00
	BNE .SPRITE_ENTRANCE_STEP
	JMP INCREMENT_INDEX
	
.SPRITE_ENTRANCE_STEP
	JMP SPRITE.ENTRANCE

	
;END-OF-ARRAYS CHECK	
GENERAL.ENTRANCE
@START
;=====================CODE-SECTION DOCUMENTATION====================================
;
;Transport objects are a type of general object. 
;This routine is setup to draw general objects during the
;inital screen draw, and after each player move. However, unless
;the general map object transitions from off-screen to on-screen, the
;drawing the map object isn't technically necessary becasue the screel
;scroll would have placed the map object in the correct location, since 
;general map objects don't move. 
;
;Since the general map objects stay in sync with player movement 
;via screen scrolling, there is no need to "erase" them.
;
;The first order of business is to determine if the current general object record
;has an x/y located on the view screen. If not, the rest of this routine 
;is skipped and the code falls through to SPRITE.ENTRANCE
;=================================================================================


;**OPT** Speed. Since scolling takes care of shifting transport on screen relative to the player,
;and transport doesn't make any 'moves' itself, I'm speculating there are only three times we need to draw transport on screen:
;1) initial screen draw
;2) when player walks on a transport and then walks off, without boarding it.
;3) the first time a transport appears on screen. 
;
;the first two cases are easy. For the third, maybe trigger a draw only if the transport's x,y
;is on the edge. Even better, for a given edge, only trigger the draw it the player was moving
;toward that edge, otherwise for all other edges the scrolling would handle proper placement
;
;this should make it so a boat bridge or harbor filled with ships doesn't slow things down. 


			
	
;	
;CALLING ROUTINE CHECK: INITIAL SCREEN DRAW 
	;Not needed because the conversion to player relative X/Y uses GMAP.X (reflects player move, or in the case of initial screen draw the actual player position)
	;SPRITE.ENTRANCE uses GMAP.X/Y.LAST because it has to evaluate the sprite position both before and after the player move. 
	
.LOAD.MAP_OBJECT.RECORD
@START
;debug: door ($14) is set to #$24 here when X= $0C

					
;The current general map object record is loaded into a holding array so that it can be manipulated.  
	LDA MAP_OBJECTS.GENERAL+$0,X	;load object GMAP.X
	STA GENERAL_MO.RECORD+$0		;save to current object record

	LDA MAP_OBJECTS.GENERAL+$1,X	;load object GMAP.Y
	STA GENERAL_MO.RECORD+$1		;save to current object record

	LDA MAP_OBJECTS.GENERAL+$2,X	;load obeject tile_type
	STA GENERAL_MO.RECORD+$2		;save to current object record
			
	LDA MAP_OBJECTS.GENERAL+$3,X	;load data byte
	STA GENERAL_MO.RECORD+$3		;save to current object record
					
@END	


			
.CONVERT.GMAP.TO.PLAYER_RELATIVE.XY ;**OPT** Memory. If I recall there is no reason not to change the routines that use player_relative XY, I just put this conversion in as a bandaid when converting to GMAP for map object records. 
@START
;Formula: PLAYER GMAP.X/Y - MO GMAP.X/Y + $80/$80 (ground zero, player location in relative grid) = MO Player Relative X/Y

;NOTE: Uses GMAP.X (reflects player move, or in the case of initial screen draw the actual player position)
;SPRITE.ENTRANCE uses GMAP.X/Y.LAST because it has to evaluate the sprite position both before and after the player move. 
	

	
	
;debug: door ($14) is set to #$24 here when X= $0C

	
	

					
					
					
.DO.CONVERSION
	LDA GMAP.X						;player GMAP X-axis
	CMP GENERAL_MO.RECORD+$0		;object GMAP X-Axis
	BCC .MOB.MO_X_LESS
	
	SEC
	SBC GENERAL_MO.RECORD+$0		;object GMAP X-Axis	
	STA MAP_OBJECTS.X_ADJ			;==distance between player and object x-axis

	LDA #MAP_OBJECTS.PLAYER_LOCATION	;Subtract distance from Player Location on MO grid ($80)
	SBC	MAP_OBJECTS.X_ADJ
	STA GENERAL_MO.RECORD+$0		;==player relative X
		
	JMP .MOB.YTEST

.MOB.MO_X_LESS ;(Player X less than object X )
	LDA GENERAL_MO.RECORD+$0		;object GMAP X-Axis
	
	SEC
	SBC GMAP.X						;player GMAP X-axis, before current move
	STA MAP_OBJECTS.X_ADJ			;==distance between player and object x-axis
	
	LDA #MAP_OBJECTS.PLAYER_LOCATION	;Add distance to Player Location on MO grid ($80)
	CLC										
	ADC MAP_OBJECTS.X_ADJ								
	STA GENERAL_MO.RECORD+$0		;==player relative X
	
	;**FALLS THROUGH**
	
.MOB.YTEST	
	LDA GMAP.Y						;player GMAP Y-axis, before current move
	CMP GENERAL_MO.RECORD+$1		;object GMAP Y-Axis
	BCC .MOB.MO_Y_LESS
	
	SEC
	SBC GENERAL_MO.RECORD+$1		;object GMAP Y-Axis
	STA MAP_OBJECTS.Y_ADJ			;==distance between player and object y-axis

	LDA #MAP_OBJECTS.PLAYER_LOCATION	;Subtract distance from Player Location on MO grid ($80)
	SBC	MAP_OBJECTS.Y_ADJ
	STA GENERAL_MO.RECORD+$1		;==player relative Y
	
	JMP .CONVERSION.COMPLETE

.MOB.MO_Y_LESS ;(Player Y less than object Y )
	LDA GENERAL_MO.RECORD+$1		;object GMAP Y-Axis	
	SEC
	SBC GMAP.Y						;player GMAP Y-axis, before current move
	STA MAP_OBJECTS.Y_ADJ			;==distance between player and object y-axis
	
	LDA #MAP_OBJECTS.PLAYER_LOCATION	;Add distance to Player Location on MO grid ($80)
	CLC										
	ADC MAP_OBJECTS.Y_ADJ								
	STA GENERAL_MO.RECORD+$1		;==player relative Y
	;**FALLS THROUGH**	
.CONVERSION.COMPLETE


			
@END
			
.ONSCREEN_CHECK ;
@START


;debug: door ($14) is set to #$24 here when X= $0C




			
;CHECK TO SEE IF CURRENT GENERAL  MO IS LOCATED ON THE VIEW SCREEN
	LDA GENERAL_MO.RECORD+$0
	CMP #MAP_OBJECTS.X_FLAG.LOWER				;IS OBJECT BEYOND LEFT EDGE OF SCREEN?
	BCC	.GENERAL.NOT_VISIBLE.OFFSCREEN_STEP	
	
	CMP #MAP_OBJECTS.X_FLAG.UPPER				;IS OBJECT BEYOND RIGHT EDGE OF SCREEN?
	BCS	.GENERAL.NOT_VISIBLE.OFFSCREEN_STEP
	
	LDA GENERAL_MO.RECORD+$1
	CMP #MAP_OBJECTS.Y_FLAG.LOWER				;IS OBJECT BEYOND LOWER EDGE OF SCREEN?
	BCC	.GENERAL.NOT_VISIBLE.OFFSCREEN_STEP	
	
	CMP #MAP_OBJECTS.Y_FLAG.UPPER				;IS OBJECT BEYOND UPPER EDGE OF SCREEN?
	BCS	.GENERAL.NOT_VISIBLE.OFFSCREEN_STEP

	JMP .GENERAL.IDENTIFY.TILE_LOCATION	

.GENERAL.NOT_VISIBLE.OFFSCREEN_STEP
	JMP .GENERAL.NOT_VISIBLE.OFFSCREEN
@END
	
.GENERAL.IDENTIFY.TILE_LOCATION				
@START
;=====================CODE-SECTION DOCUMENTATION====================================
;
;This code section is responsible for converting the general MO's relative x,y
;position into a screen tile location (the index to the screen arrays).
;The screen tile location is needed later to draw the tile since 
;DRAW.TILE.SINGLE needs this as a parameter.
;
;The mechanic of doing the conversion is essentially done by using the 
;difference between the player's x/y and the general MO's x/y as an adjustment to 
;the players screen tile position, using the appropriate offsets for the
;screen tile grid. For example, for a y axis differential, a multiplication table is used
;to calculate the offset to apply to the player's location. 
;
;Calculating the difference between player and general MO's x,y starts 
;determining whether the general MO's x,y is less than the player x,y.
;This is done so that negative numbers can be avoided. 
;=================================================================================

;debug door ($14), when X= $00 hook2 doesn't trigger here. X=$00 is an offsceen object (door).



					




					
;IDENTIFY SCREEN TILE #							;OBJECT IS ON SCREEN, WHERE DO WE DRAW IT?
				
	LDA GENERAL_MO.RECORD+$0
	CMP #MAP_OBJECTS.PLAYER_LOCATION
	BCC .GENERAL.MO_X_LESS
	
	LDA GENERAL_MO.RECORD+$0
	SEC
	SBC #MAP_OBJECTS.PLAYER_LOCATION
	STA MAP_OBJECTS.X_ADJ
	LDA #SCREEN.ARRAY.PLAYER_LOCATION
	CLC
	ADC MAP_OBJECTS.X_ADJ
	STA TEMP
	JMP .GENERAL.YTEST

.GENERAL.MO_X_LESS
	LDA #MAP_OBJECTS.PLAYER_LOCATION
	SEC
	SBC GENERAL_MO.RECORD+$0
	STA MAP_OBJECTS.X_ADJ
	LDA #SCREEN.ARRAY.PLAYER_LOCATION
	SEC
	SBC MAP_OBJECTS.X_ADJ
	STA TEMP


.GENERAL.YTEST	

	LDA GENERAL_MO.RECORD+$1
	CMP #MAP_OBJECTS.PLAYER_LOCATION
	BCC .GENERAL.MO_Y_LESS
	
	LDA GENERAL_MO.RECORD+$1
	SEC
	SBC #MAP_OBJECTS.PLAYER_LOCATION
	TAY
	LDA SCREEN.MULTIPLY_TABLE,Y
	STA MAP_OBJECTS.Y_ADJ
	
	LDA TEMP								;PARTIAL CALCULATION. CONTAINS #MAP_OBJECTS.PLAYER_LOCATION +/- MAP_OBJECTS.X_ADJ 
	CLC
	ADC MAP_OBJECTS.Y_ADJ
	STA MAP_OBJECTS.TILE_LOCATION	

	JMP .LOCATION.FOUND

.GENERAL.MO_Y_LESS
	LDA #MAP_OBJECTS.PLAYER_LOCATION
	SEC
	SBC GENERAL_MO.RECORD+$1		
	TAY
	LDA SCREEN.MULTIPLY_TABLE,Y
	STA MAP_OBJECTS.Y_ADJ
	
	LDA TEMP								;PARTIAL CALCULATION. CONTAINS #MAP_OBJECTS.PLAYER_LOCATION +/- MAP_OBJECTS.X_ADJ 
	SEC
	SBC MAP_OBJECTS.Y_ADJ
	STA MAP_OBJECTS.TILE_LOCATION
	;***FALLS THROUGH**

.LOCATION.FOUND
			
	LDA GENERAL_MO.RECORD+$2			;load tile type of active transport
	CMP #TILE_ID.FRIGATE1.1				;is transport a multi-tiletile object? (FRIGATES ARE THE ONLY MULTI-TILE TRANSPORT IN THE GAME. IF MORE WERE ADDED, THEN EITHER TWO TILE_TYPE CHECKS ARE NEEDED OR A MULTI-TILE FLAG WOULD BE NEEDED
	BNE .GENERAL.DRAWTILE				;if no, use regular drawtile routine
	JMP .TRANSPORT.DRAWTILE.MT			;if yes, use mt tile draw routine 
@END
	;**FALLS THROUGH**


.GENERAL.DRAWTILE	
@START

					
					
;debug: door ($14) is set to #$24 here when X= $0C
					
	LDY MAP_OBJECTS.TILE_LOCATION			;LOAD SCREEN ARRAY LOCATION OF THE CURRENT TRANSPORT MO					

				
;DOUBLE CHECK THAT WE REALLY WANT TO DRAW A MAP OBJECT TILE IN THIS LOCATION 	
;IS TILE HIDDEN (DARKNESS)?	
	LDA SCREEN.DARK.DATA,Y			
	CMP #$01
	BEQ .GENERAL.NOT_VISIBLE.DARK_STEP			;IF YES, DON'T DRAW TILE
	
;IS TILE AT PLAYER LOCATION?
.CHECK.PLAYER.LOCATION		
	CPY #SCREEN.ARRAY.PLAYER_LOCATION
	BNE .UPDATE.SCREEN_ARRAY2				;IF NOT PLAYER LOCATION, PROCEED TO UPDATE SCREEN ARRAY

	CPX PLAYER.TRANSPORT.ACTIVE				;IS MO RECORD INDEX A TRANSPORT OBJECT BOARDED BY PLAYER?
	BEQ .CHECK.EXCEPTIONS.BUILDING.LOCATION_TYPE						;IF YES, SKIP SCREEN ARRAY UPDATE AND DRAW TILE (THIS IS SO THAT IF THE PLAYER'S LOCATION ON TOP OF ANOTHER TRANSPORT OBJECT,, IT WILL BE DETECTED IN THE SCREEN ARRRAY VIA BOARD COMMAND. EXAMPLE, PLAYER IS BOARDED A SKIFF AND IS LOCATED ON A TILE WITH A FRIGATE)	

.UPDATE.SCREEN_ARRAY2	
	TXA										;ACC = MAP_OBJECTS.GENERAL INDEX		
	STA SCREEN.MO_GENERAL.DATA,Y			;SAVE MAP_OBJECTS.GENERAL INDEX TO SCREEN ARRAY TO MARK MOB'S PRESENCE FOR OTHER ROUTINES LIKE ANIMATION


			
.CHECK.EXCEPTIONS.BUILDING.LOCATION_TYPE
	;CHECK BUILDING LOCATION TYPE
	LDA PLAYER.MAP.LOCATION_TYPE
	; CMP #MAP.TYPE.TOWN_VILLAGE			;IS LOCATION TYPE = BUILDING?
	; BEQ .IN.BUILDING.OR.UNDERMAP
	; CMP #MAP.TYPE.UNDERMAP			
	; BNE .SAVE.TILE_TYPE
	
	CMP #MAP.TYPE.UNDERMAP			
	BEQ .IN.BUILDING.OR.UNDERMAP
	;is a building map active
	;**OPT** Memory. Speed. Right now there is only one code for location type, which is used to determine the tile_set to use and for other location-type specific features. There are a lot of the later. It might be a savings if the 
					;there were a tile-set code separate from location type code, and there was a default tile-set picked using the location-type code if tile-set code was set to default. This might require adding another byte to some hex tables
					;but I suspect it would pay off. 
	CMP #MAP.TYPE.BUILDING.GRE 	;is map type = building?
	BCC .SAVE.TILE_TYPE				;if no
	CMP #MAP.TYPE.BUILDING.LT	;is map type = building?
	BCS .SAVE.TILE_TYPE				;if no
	;**FALLS THROUGH**				;if yes
	
.IN.BUILDING.OR.UNDERMAP
	
.CHECK.SKIP.DRAW.FLAG
	LDA GENERAL_MO.RECORD+$3		;load data byte of general map object record
	CMP #MO.SKIP.DRAW.FLAG			;is tile_id = skip draw flag?
	BEQ .EXIT_STEP					;if yet, then skip draw for this tile
	;**FALLS THROUGH**
		
.CHECK.DOOR


;debug: door ($14) is set to #$24 here when X= $0C



;IS OBJECT AN OPEN DOOR?
;	LDA GENERAL_MO.RECORD+$3		;load data byte of general map object record
	;ACC = byte $03 of MO record
	CMP #MO.DOOR.OPEN.GRE			;is there an open door on current tile?
	BCC .SAVE.TILE_TYPE				;if no, then continue with other checks
	CMP #MO.DOOR.OPEN.LT			;is there an open door on current tile?	
	BCS .SAVE.TILE_TYPE				;if no, then continue with other checks

;OPEN DOOR PRESENT
	;IS DOOR PREVENTED FROM CLOSING?
		;Y-REG has screen tile location of current map object
	CPY #SCREEN.ARRAY.PLAYER_LOCATION	;is map object (the door) in the same tile location as the player?
	BEQ .DOOR.CANNOT.CLOSE.PLAYER				;if yes, then door can't close. Don't draw tile and reset counter to start (i.e the door won't start to close until the player is no longer in it)
	LDA SCREEN.MO_SPRITE.DATA,Y		;load screen array with sprite data for this tile location
	CMP #$FF						;is a sprite present in the same location as the current map object (door)?
	BNE .DOOR.CANNOT.CLOSE.SPRITE	;if yes, then door can't close. Don't draw tile and reset counter to start (i.e the door won't start to close until the sprite is no longer in it)
	DEC GENERAL_MO.RECORD+$3		;subtract 1 from the open door counter. When the value of the data byte reached $20, the door is closed. 
	LDA GENERAL_MO.RECORD+$3	
	CMP #MO.DOOR.CLOSE_TRIGGER		;is door closed as of next turn?
	BNE .EXIT_STEP					;if no, then exit (don't draw tile)
	LDA #MO.DOOR.CLOSED.UNLOCKED	;if yes, set door to closed in map object record, then draw tile
	STA GENERAL_MO.RECORD+$3
	;JMP .EXIT_STEP
	JMP .SAVE.TILE_TYPE

.DOOR.CANNOT.CLOSE.PLAYER
	;RESET OPEN DOOR COUNTER (i.e the door won't start to close until the sprite or player is no longer in it))
	LDA #MO.DOOR.OPEN.START		;set data byte to code for open door timer with 3 moves
	STA GENERAL_MO.RECORD+$3	
	JMP .EXIT_STEP

.GENERAL.NOT_VISIBLE.DARK_STEP
	JMP .GENERAL.NOT_VISIBLE.DARK
	
.DOOR.CANNOT.CLOSE.SPRITE
;NOTE: a separate routine is used for sprites, so that the open door counter can be set to a lower value. This is because general map objects like doors are processed before sprites so the sprite will appear to this subroutine like it is still in the doorway on the first turn the player sees the sprite move out of the doorway

	;RESET OPEN DOOR COUNTER (i.e the door won't start to close until the sprite or player is no longer in it))
	LDA #MO.DOOR.OPEN.START2	;set data byte to code for open door timer with 3 moves
	STA GENERAL_MO.RECORD+$3	
	JMP .EXIT_STEP
	
.DOOR.IS.OPEN ;**OPT** I don't think this is doing anything	
	DEC GENERAL_MO.RECORD+$3	;subtract 1 from the open door counter. When the value of the data byte reached $20, the door is closed. 
	;**FALLS THROUGH**


;TILE_ID.PORTCULLIS_LEVER.LEFT
	
.EXIT_STEP
	JMP .EXIT
	
.SAVE.TILE_TYPE


					
					
	LDA GENERAL_MO.RECORD+$2
	STA SAVED_TILE_TYPE
	
		; LDA #$07					;SET TRACE
		; STA CALLED_BY
	JSR DRAW.TILE.SINGLE
		; LDA #$07					;SET TRACE
		; STA CALLED_BY				;(****not needed, just trace RTS address from the stack)
	

			
	JMP .EXIT	
@END
	
.TRANSPORT.DRAWTILE.MT
@START
;IF MULTI-TILE MOB, RECORD THE SCREEN TILE # FOR EACH OF IT'S FOUR TILES		
	LDY MAP_OBJECTS.TILE_LOCATION			;LOAD SCREEN ARRAY LOCATION OF THE CURRENT TRANSPORT MO, CALCUALTED IN MAIN IDENTIFY TILE ROUTINE ABOVE
	STY TRANSPORT.MT.TILE_LOCATIONS			;SAVE AS TILE #0 (UPPER LEFT) OF MT TRANSPORT (BORROWING THE MOB MT ARRAY)

	INY
	STY TRANSPORT.MT.TILE_LOCATIONS+$1		;SAVE AS TILE #1 (UPPER RIGHT) OF MT TRANSPORT (BORROWING THE MOB MT ARRAY)
	TYA
	CLC
	ADC #SCREEN.ARRAY.OFFSET
	STA TRANSPORT.MT.TILE_LOCATIONS+$3		;SAVE AS TILE #3 (LOWER RIGHT) OF MT TRANSPORT (BORROWING THE MOB MT ARRAY)
	TAY
	DEY
	STY TRANSPORT.MT.TILE_LOCATIONS+$2		;SAVE AS TILE #2 (LOWER LEFT) OF MT TRANSPORT (BORROWING THE MOB MT ARRAY)

;CALCUALTE THE TILE TYPES FOR EACH TILE OF MT TRANSPORT
	LDY GENERAL_MO.RECORD+$2				;LOAD TILE TYPE OF MOB RECORD
	STY TRANSPORT.MT.TILE_TYPES				;SAVE TILE TYPE TO A HOLDING ARRAY (BORROWED FROM MOB ROUTINE)
	INY
	STY TRANSPORT.MT.TILE_TYPES+$1
	INY
	STY TRANSPORT.MT.TILE_TYPES+$2
	INY
	STY TRANSPORT.MT.TILE_TYPES+$3
	
	LDA GENERAL_MO.RECORD+$0				;LOAD X-AXIS (COLUMN) OF TILE0 OF MT MOB
	STA TRANSPORT.MT.POSITION.X				;SAVE FOR FUTURE USE, WHEN X/Y-REG ARE BOTH BUSY
	
	STX SAVED.XREG.LOCAL					;SAVE X-REG SO WE CAN USE IT AS LOOP COUNTER
	LDX #$00								;INIT LOOP COUNTER
.LOOP	
	LDY TRANSPORT.MT.TILE_LOCATIONS,X

;IS TILE ON THE VIEW SCREEN
;(normally this condition indicates an error, but with MT Mobs, it can happen legitimately if the mob is half on/off the screen. In that case, it's legit but the correct process is to not draw the offscreen tiles)	
	;IF TILE IS OFFSCREEN TO NORTH OR SOUTH, THE TILE # WILL EXCEED THE LAST ARRAY ELEMENT

	LDA #SCREEN.ARRAY.LAST_ELEMENT
	CMP TRANSPORT.MT.TILE_LOCATIONS,X	
	BCS .CHECK.EAST_WEST
	JMP .TILE.OFFSCREEN
.CHECK.EAST_WEST
	;OFFSCREEN EAST/WEST REQUIRES CHECKING THE COLUMN 
	LDA TRANSPORT.MT.POSITION.X				;LOAD X-AXIS (COLUMN) OF TILE0 OF MT MOB. LOADING FROM VARIABLE BECAUSE X/Y-REG ARE BOTH IN USE	

	CPX #$00								;IS TILE 0 THE CURRENT TILE??
	BEQ .TILE0.OFFSCREEN_CHECK
	CPX #$01								;IS TILE 1 THE CURRENT TILE??
	BEQ .TILE1.OFFSCREEN_CHECK
	CPX #$02								;IS TILE 2 THE CURRENT TILE??
	BEQ .TILE2.OFFSCREEN_CHECK
	CPX #$03								;IS TILE 3 THE CURRENT TILE??
	BEQ .TILE3.OFFSCREEN_CHECK

.TILE0.OFFSCREEN_CHECK
	CMP MAP_OBJECTS.X_APPROACH+$1
	BEQ .TILE.OFFSCREEN
	JMP .TILE_ONSCREEN
	
.TILE1.OFFSCREEN_CHECK
	CMP #MAP_OBJECTS.X.LAST_COLUMN
	BEQ .TILE.OFFSCREEN
	JMP .TILE_ONSCREEN

.TILE2.OFFSCREEN_CHECK
	CMP MAP_OBJECTS.X_APPROACH+$1
	BEQ .TILE.OFFSCREEN
	JMP .TILE_ONSCREEN

.TILE3.OFFSCREEN_CHECK

	CMP #MAP_OBJECTS.X.LAST_COLUMN
	BEQ .TILE.OFFSCREEN
	JMP .TILE_ONSCREEN
	
	
.TILE_ONSCREEN	
;IS TILE HIDDEN (DARKNESS)?	
	LDA SCREEN.DARK.DATA,Y			
	CMP #$01
	BEQ .TILE.NOTVISIBLE						;IF YES, DON'T DRAW TILE

;IS TILE PLAYER LOCATION? & CURRENT RECORD IS ACTIVE TRANSPORT? 	
	LDA SAVED.XREG.LOCAL						;LOAD MO RECORD INDEX
	CMP PLAYER.TRANSPORT.ACTIVE					;IS MO RECORD INDEX A TRANSPORT OBJECT BOARDED BY PLAYER?
	BNE .VALID.DRAW.LOCATION
	CPY #PLAYER.TRANSPORT.MT.TILE_LOCATION0		;CHECK MULTI-TILE PLAYER TRANSPORT LOCATIONS
	BEQ .EXIT_TEST								;IF PLAYER LOCATION, SKIP DRAW & SCREEN ARRAY UDPATE. (THIS IS SO THAT IF THE PLAYER IS LOCATION ON TOP OF ANOTHER TRANSPORT OBJECT,, IT WILL BE DETECTED IN THE SCREEN ARRRAY VIA BOARD COMMAND. EXAMPLE, PLAYER IS BOARDED A SKIFF AND IS LOCATED ON A TILE WITH A FRIGATE)	
	CPY #PLAYER.TRANSPORT.MT.TILE_LOCATION1		;CHECK MULTI-TILE PLAYER TRANSPORT LOCATIONS
	BEQ .EXIT_TEST								;IF PLAYER LOCATION, SKIP DRAW & SCREEN ARRAY UDPATE. (THIS IS SO THAT IF THE PLAYER IS LOCATION ON TOP OF ANOTHER TRANSPORT OBJECT,, IT WILL BE DETECTED IN THE SCREEN ARRRAY VIA BOARD COMMAND. EXAMPLE, PLAYER IS BOARDED A SKIFF AND IS LOCATED ON A TILE WITH A FRIGATE)	
	CPY #PLAYER.TRANSPORT.MT.TILE_LOCATION2		;CHECK MULTI-TILE PLAYER TRANSPORT LOCATIONS
	BEQ .EXIT_TEST								;IF PLAYER LOCATION, SKIP DRAW & SCREEN ARRAY UDPATE. (THIS IS SO THAT IF THE PLAYER IS LOCATION ON TOP OF ANOTHER TRANSPORT OBJECT,, IT WILL BE DETECTED IN THE SCREEN ARRRAY VIA BOARD COMMAND. EXAMPLE, PLAYER IS BOARDED A SKIFF AND IS LOCATED ON A TILE WITH A FRIGATE)	
	CPY #PLAYER.TRANSPORT.MT.TILE_LOCATION3		;CHECK MULTI-TILE PLAYER TRANSPORT LOCATIONS
	BEQ .EXIT_TEST								;IF PLAYER LOCATION, SKIP DRAW & SCREEN ARRAY UDPATE. (THIS IS SO THAT IF THE PLAYER IS LOCATION ON TOP OF ANOTHER TRANSPORT OBJECT,, IT WILL BE DETECTED IN THE SCREEN ARRRAY VIA BOARD COMMAND. EXAMPLE, PLAYER IS BOARDED A SKIFF AND IS LOCATED ON A TILE WITH A FRIGATE)	

.VALID.DRAW.LOCATION
	LDA MOB.MT.TILE_TYPES,X					;LOAD NEXT MT TILE_TYPE
	STA SAVED_TILE_TYPE	
	
				
		; LDA #$0F							;SET TRACE
		; STA CALLED_BY
	JSR DRAW.TILE.SINGLE
		; LDA #$00							;RESET TRACE
		; STA CALLED_BY					;(****not needed, just trace RTS address from the stack)

		
.UPDATE.SCREEN_ARRAY1
.TILE.NOTVISIBLE
	LDA SAVED.XREG.LOCAL					;load map object array index						
	STA SCREEN.MO_GENERAL.DATA,Y			;SAVE TO SCREEN ARRAY TO MARK MOB'S PRESENCE FOR OTHER ROUTINES LIKE ANIMATION

.TILE.OFFSCREEN
.EXIT_TEST
	INX
	CPX #$04
	BNE .LOOP
	LDX SAVED.XREG.LOCAL			;restore map objects array index	
	JMP .EXIT
@END
	
.GENERAL.NOT_VISIBLE.DARK	
;NOTE: UPDATE SCREEN ARRAY EVEN THGOUH TILE IS NOT VISIBLE (HIDDEN/DARK) SO THAT DARKNESS.REVIEW
;WILL DETECT IT ONCE THE TILE DOES BECOME VISIBLE...SINCE DARKNESS.REVIEW RUNS BEFORE MO.DRAW.

	TXA										;ACC = MAP_OBJECTS.GENERAL INDEX		
	STA SCREEN.MO_GENERAL.DATA,Y			;SAVE MAP_OBJECTS.GENERAL INDEX TO SCREEN ARRAY TO MARK MOB'S PRESENCE FOR OTHER ROUTINES LIKE ANIMATION
	;**FALLS THROUGH**

.GENERAL.NOT_VISIBLE.OFFSCREEN
.EXIT
.SAVE.MAP_OBJECT.RECORD


			
	; LDA MAP_OBJECTS.GENERAL+$0,X
	; STA GENERAL_MO.RECORD+$0

	; LDA MAP_OBJECTS.GENERAL+$1,X
	; STA GENERAL_MO.RECORD+$1
	
	LDA GENERAL_MO.RECORD+$2
	STA MAP_OBJECTS.GENERAL+$2,X

	LDA GENERAL_MO.RECORD+$3	
	STA MAP_OBJECTS.GENERAL+$3,X
	
	
;debug: door ($14) is set to #$10 here when X= $00
	


					
	;**FALLS THROUGH
@END
SPRITE.ENTRANCE ;	
@START

			
;======LOAD MOB/NPC RECORD, EMPTY RECORD CHECKS, EXTRACT FLAGS, INIT VARS=====
@START
;=====================CODE-SECTION DOCUMENTATION====================================
;
;S_ENTITY (sentient entity) refers to map objects which have intelligence and typically can move
;and the map and/or combat screen. Currently these include: MOBS, NPCs, and player characters in combat. 
;
;-MAP_OBJECT ARRAYS
;Some S_ENTITY types share map object array space.
;
;non-combat MOBs share with combat player characters
;NPCs share with combat mobs. 
;
;Note: A hostile NPC engaged in combat is considered a combat mob
;
;=================================================================================



				
;INIT VARIABLES (1 OF 2)

	LDY #$00
	STY MANAGE_OBJECTS.NPC_MOB.RECORD.FLAG	;$00=mob iteration, $01=npc iteration, $02=next map object record
	STY DESTINATION_REACHED_WHILE_SEEKING
	
	;INIT SPRITE RECORD
.LOOP									;since SPRITE.RECORD is larger than MAP.OBJECTS.MOB, we need to init SPRITE.RECORD on each iteration or left over data from MAP.OBJECTS.MOB could get leftover. 
	LDA #$00
	STA SPRITE.RECORD,Y
	INY
	CPY #SPRITE.RECORD.SIZE
	BNE .LOOP	

	
SWITCH.MOB_NPC	
	;INIT S_ENTITY FLAGS
	LDA #$FF
	LDY #$00
	STY COMBAT_SE.MODE.PARM	;($00 = player initiated or init value in map objects manager | $01 = mob initiated | $FF = test mode)
.LOOP2
	STA SPRITE.FLAGS_BYTE3,Y
	INY
	CPY #$08
	BNE .LOOP2

			
;IS CURRENT INDEX MOB ONLY OR NPC & MOB?
;since the MOB and NPC map object arrays are not the same size, this flag tracks whether the index points to a MOB record only or a MOB & NPC Record. $00 = Mob & NPC, $01 = Mob only, 	
	LDA MANAGE_OBJECTS.NPC_MOB.INDEX.FLAG	 ;$00 = Mob & NPC, $01 = Mob only, 
	CMP #$01								 ;is record MOB only?
	BEQ .LOAD.MOB.RECORD					 ;if yes, load mob record	;if no, determine if this is the mob or NPC iteration

;IS THIS MOB OR NPC RECORD? 
	LDA MANAGE_OBJECTS.NPC_MOB.RECORD.FLAG
	CMP #$01							;$00=mob record, $01=npc record, $02=next map object record
	BEQ .LOAD.NPC.RECORD
	CMP #$02
	BCS .INCREMENT_INDEX_STEP

	;**FALLS THROUGH**
	
.LOAD.MOB.RECORD	
;END-OF-ARRAY? (MOB) 	
	LDA MAP_OBJECTS.MOB+$02,X			;(test array for an empty record by checking $00 in the 3rd field (tile_type) of the current record.				
	CMP #$00							;empty record?
	BNE .CONTINUE1						;if no, process MOB record
;
	LDA MANAGE_OBJECTS.NPC_MOB.INDEX.FLAG	 ;$00 = Mob & NPC, $01 = Mob only, 
	CMP #$01							;is index mob only?
	BNE .FLIP.RECORD.FLAG				;if no, then flip record flag to NPC mode		
	JMP INCREMENT_INDEX					;if yes, increment index to get next record

.FLIP.RECORD.FLAG		
	JMP FLIP.NPC_MOB.RECORD.FLAG

.CONTINUE1 	
;LOAD CURRENT RECORD FROM MAP OBJECTS ARRAY
	LDA MAP_OBJECTS.MOB+$0,X	;load mob's GMAP.X 
	STA SPRITE.RECORD+$8		;save to sprite record GMAP field. 
	
	LDA MAP_OBJECTS.MOB+$1,X	;load mob's GMAP.Y
	STA SPRITE.RECORD+$9		;save to sprite record GMAP field. 
	
	LDA MAP_OBJECTS.MOB+$2,X
	STA SPRITE.RECORD+$2
	
	LDA MAP_OBJECTS.MOB+$3,X
	STA SPRITE.RECORD+$3



	;SET S_ENTITY TYPE
	
	;is combat active
	LDA PLAYER.MAP.LOCATION_TYPE	;load map type of players current location
	CMP #MAP.TYPE.COMBAT
	BNE .COMBAT.CHECK.DONE1 ;if combat is not active, then S_ENTITY type is non-combat MOB, but no need to set the value to $00 as that is already the init value of SPRITE.RECORD.	
;.COMBAT.ACTIVE
	LDA #S_ENTITY_TYPE.PC ;set S_ENTITY type to combat player character 
	STA SPRITE.RECORD+$A
	
	;set health status
	;(this is done because all S_ENTITIES except PCs have the health status in byte $7. This is because they all share 
	;space with MAP_OBJECTS.NPC, which has 8 byte records, whereas PCs share space with MAP_OBJECTS.MOB, which has 
	;4 byte records. 
	LDA COMBAT.MAP_OBJECTS.PC+$3,X ;contains either health status (if not active PC) or the movement direction code (if active PC)
	CPX COMBAT.PC.ACTIVE.RECORD	
	BNE .NOT.ACTIVE_PLAYER
.IS.ACTIVE_PLAYER
	;ACC = health status field (from map object record)
	LDY CALLED_BY.DRAW.SCREEN ;if this iteration is initial screen draw, get health status from map object record
	CPY #$01
	BEQ .IS.INITIAL.SCREEN.DRAW
	
	;ACC = health status field (from map object record)
	LDY COMBAT.TURN_STATUS ;$00 = PCs turn. $01 = Special(s) S_ENTITY, $02 = MOBs turn
	BNE .NOT.PC.TURN
	LDA COMBAT.PC.ACTIVE.HEALTH_STATUS 	;load health status for active PC. 
	;**FALLS THROUGH**					;For details see on why this is not loaded from the map object record,
										;see .UPDATE.VIDEO.SCREEN (COMBAT.MOVE.COMMON.ROUTINE).

			; STA SPRITE.RECORD+$7 ;save health status
			; JMP .COMBAT.CHECK.DONE1
		
.IS.INITIAL.SCREEN.DRAW
.NOT.PC.TURN
.NOT.ACTIVE_PLAYER																	
	STA SPRITE.RECORD+$7 ;save health status

		; CPX #$00
		; BNE .TEMP
		; LDA COMBAT.TURN_STATUS
		; BNE .TEMP
		
		; LDA #$DA
		; TAY
		; LDA COMBAT.MAP_OBJECTS.PC+$3,X
		; LDX COMBAT.PC.ACTIVE.RECORD
		
		; JSR FULL.BRK
		; ; BRK
; .TEMP

	
.COMBAT.CHECK.DONE1

	;SET NPC RECORD #
	STX SPRITE.RECORD+$B
	
	JMP .CONVERT.GMAP.TO.PLAYER_RELATIVE.XY
	
.INCREMENT_INDEX_STEP
	JMP INCREMENT_INDEX

	
.LOAD.NPC.RECORD
;(test array for an empty record by checking $00 in the 3rd field (tile_type) of the current record.	
	LDA MAP_OBJECTS.NPC+$02,X			;empty record?
	CMP #$00	
	BNE .CONTINUE2
	JMP INCREMENT_INDEX					;if yes, incremet map object record number and return to primary loop
.CONTINUE2
	;**OPT** Memory. Speed. This section seems like an unnecssary safety net.
	;maps which don't use the NPC array (i.e. surface), would contain all $00s on in the array
	;and thus the check of byte $02 of the NPC array in .LOAD.NPC.RECORD should not permitt entry to this point. 

;VALIDATE ENTRY	
;(it used to be that the NPC wasnt permitted to be used on the surface,
;but there was really no good reason for that, other than a slight speed advantage, so 
;eventually I removed the restriction)

	; LDA PLAYER.MAP.LOCATION_TYPE		;building maps are except because a zone transition triggers exit back to prior map
	; CMP #MAP.TYPE.TOWN_VILLAGE
	; BEQ .LOAD.NPC.RECORD.START
	; CMP #MAP.TYPE.UNDERMAP
	; BEQ .LOAD.NPC.RECORD.START
	; CMP #MAP.TYPE.COMBAT
	; BEQ .LOAD.NPC.RECORD.START
	; JMP .INCREMENT_INDEX_STEP			;**OPT** Speed. Memory. This location type check can be removed once the MAP_OBJECTS.NPC array is loaded from disk apon entering a building and flushed on exit. 

.LOAD.NPC.RECORD.START
	
;MAP_OBJECTS.MOB datagram
;Byte 0	Byte 1	Byte 2		Byte 3		Byte 4			Byte 5			Byte 6				Byte 7
;GMAP.X	GMAP.Y	Tile_type	Set to $03	Active Anchor	Path Index($00)	In Transit? ($00)	at-anchor move routine flag
;See Map Objects spreadsheet for authoratative version

;SPRITE.RECORD datagram	
;Byte 0						Byte 1						Byte 2		Byte 3			Byte 4			Byte 5		Byte 6		Byte 7						Byte 8			Byte 9			Byte $A
;player-relative.X of NPC	player-relative.Y of NPC	Tile_type	Set to $03 (SS)	Active Anchor	Path Index	In Transit?	at-anchor move routine flag	RMAP.X of NPC	RMAP.Y of NPC	Sprite_Type
;See Map Objects spreadsheet for authoratative version

					; lda #$cc
					; JSR FULL.BRK
					; brk
					
	LDA MAP_OBJECTS.NPC+$2,X
	STA SPRITE.RECORD+$2
	
	LDA MAP_OBJECTS.NPC+$3,X
	STA SPRITE.RECORD+$3	
	
	LDA MAP_OBJECTS.NPC+$4,X
	STA SPRITE.RECORD+$4
	
	LDA MAP_OBJECTS.NPC+$5,X
	STA SPRITE.RECORD+$5

	LDA MAP_OBJECTS.NPC+$6,X
	STA SPRITE.RECORD+$6

	LDA MAP_OBJECTS.NPC+$7,X
	STA SPRITE.RECORD+$7

;debug: door ($14) is set to #$10 here when X= $00 (NPC mode)

	


					
	;SET S_ENTITY TYPE
	
	;is combat active
	LDA PLAYER.MAP.LOCATION_TYPE	;load map type of players current location
	CMP #MAP.TYPE.COMBAT
	BEQ .COMBAT.ACTIVE2
	;**FALLS THROUGH
	
	LDA #S_ENTITY_TYPE.BLD_NPC ;set S_ENTITY type to Building NPC
	STA SPRITE.RECORD+$A
	JMP .COMBAT.CHECK.DONE2

	
.COMBAT.ACTIVE2
	;is current record in the combat MOB or the SPECIAL part of the array?
	CPX #COMBAT.MAP_OBJECTS.SPECIAL.START_RECORD
	BCC .SET.TYPE.C_MOB
.SET.TYPE.SPECIAL
	LDA #S_ENTITY_TYPE.SPECIAL ;set S_ENTITY type to combat MOB (includes hostile NPCs)
	STA SPRITE.RECORD+$A	
	JMP .COMBAT.CHECK.DONE2
	
.SET.TYPE.C_MOB	
	LDA #S_ENTITY_TYPE.C_MOB ;set S_ENTITY type to combat MOB (includes hostile NPCs)
	STA SPRITE.RECORD+$A
	;**FALLS THROUGH
.COMBAT.CHECK.DONE2
	
	;SET NPC RECORD #
	STX SPRITE.RECORD+$B

;SET SPRITE RECORD BYTE #$08, #$09
	LDA MAP_OBJECTS.NPC+$0,X
	STA SPRITE.RECORD+$8		;RMAP.X (SAME AS GMAP BECAUSE WE'RE IN UPPER LEFT OF WORLD MAP)
	
	LDA MAP_OBJECTS.NPC+$1,X
	STA SPRITE.RECORD+$9		;RMAP.Y (SAME AS GMAP BECAUSE WE'RE IN UPPER LEFT OF WORLD MAP)		
	
	;**FALLS THROUGH**

;MOB & NPC: CONVERT GMAP -> PLAYER RELATIVE X,Y	
;SET SPRITE RECORD BYTE $0,$1 
.CONVERT.GMAP.TO.PLAYER_RELATIVE.XY
@START
;Formula: PLAYER GMAP.X/Y - NPC GMAP.X/Y + $80/$80 (ground zero, player location in relative grid) = NPC Player Relative X/Y


.LOAD.PLAYER.GMAP
;NOTE: The reason that GMAP.X/Y.LAST is used is because sprites must be evaluated for onscreen/offscreen
;before and after the effect of the player move, and those routines both use player relative X/Y. 
;GMAP.X/Y.LAST contains the player GMAP before the effect the current player move.

	
.DO.CONVERSION
	LDA GMAP.X.LAST				;player GMAP X-axis, before current move
	CMP SPRITE.RECORD+$8		;sprite GMAP X-Axis
	BCC .MOB.MO_X_LESS
	
	SEC
	SBC SPRITE.RECORD+$8		;sprite GMAP X-Axis	
	STA MAP_OBJECTS.X_ADJ				;==distance between player and sprite x-axis

	LDA #MAP_OBJECTS.PLAYER_LOCATION	;Subtract distance from Player Location on MO grid ($80)
	SBC	MAP_OBJECTS.X_ADJ
	STA SPRITE.RECORD+$0				;==player relative X
	
	JMP .MOB.YTEST

.MOB.MO_X_LESS ;(Player X less than sprite X )
	LDA SPRITE.RECORD+$8		;sprite GMAP X-Axis
	
	SEC
	SBC GMAP.X.LAST				;player GMAP X-axis, before current move
	STA MAP_OBJECTS.X_ADJ		;==distance between player and sprite x-axis
	
	LDA #MAP_OBJECTS.PLAYER_LOCATION	;Add distance to Player Location on MO grid ($80)
	CLC										
	ADC MAP_OBJECTS.X_ADJ								
	STA SPRITE.RECORD+$0			;==player relative X
	
	;**FALLS THROUGH**
	
.MOB.YTEST	
	LDA GMAP.Y.LAST					;player GMAP Y-axis, before current move
	CMP SPRITE.RECORD+$9			;sprite GMAP Y-Axis
	BCC .MOB.MO_Y_LESS
	
	SEC
	SBC SPRITE.RECORD+$9			;sprite GMAP Y-Axis
	STA MAP_OBJECTS.Y_ADJ			;==distance between player and sprite y-axis

	LDA #MAP_OBJECTS.PLAYER_LOCATION	;Subtract distance from Player Location on MO grid ($80)
	SBC	MAP_OBJECTS.Y_ADJ
	STA SPRITE.RECORD+$1				;==player relative Y
	
	JMP .CONVERSION.COMPLETE

.MOB.MO_Y_LESS ;(Player Y less than sprite Y )
	LDA SPRITE.RECORD+$9			;sprite GMAP Y-Axis	
	SEC
	SBC GMAP.Y.LAST					;player GMAP Y-axis, before current move
	STA MAP_OBJECTS.Y_ADJ			;==distance between player and sprite y-axis
	
	LDA #MAP_OBJECTS.PLAYER_LOCATION	;Add distance to Player Location on MO grid ($80)
	CLC										
	ADC MAP_OBJECTS.Y_ADJ								
	STA SPRITE.RECORD+$1			;==player relative Y
	;**FALLS THROUGH**	


.CONVERSION.COMPLETE

			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; LDA #$AA
			; LDX GMAP.X.LAST
			; LDY GMAP.Y.LAST
			; JSR FULL.BRK
			; BRK
; .TEMP
			; LDA TEMP
			
			

				
@END
		
.EXTRACT_FLAGS
;EXTRACT MOB BOOLEAN BYTE3 FLAGS
	LDA SPRITE.RECORD+$A	;load sprite type
	;is S_ENTITY type mob (non-combat or combat) or special?
	CMP #S_ENTITY_TYPE.PC		
	BCS .INIT.VARIABLES		;if no, skip the extract flags routine
	;**FALLS THROUGH**
	
;START FLAG EXTRACTION	
	LDA SPRITE.RECORD+$3
	LDY #$00	
.LOOP	
	LSR
	BCS .CARRY_IS_SET					
.CARRY_IS_CLEAR							;THE CARRY IS NOT KATIE
	STA SAVED.ACC.LOCAL
	LDA #$00
	JMP .WRITE_FLAG_VALUE
.CARRY_IS_SET							;THE CARRY IS KATIE
	STA SAVED.ACC.LOCAL
	LDA #$01

.WRITE_FLAG_VALUE
	STA SPRITE.FLAGS_BYTE3,Y
	CPY #$07
	BEQ .FLAGS_DONE
	INY
	LDA SAVED.ACC.LOCAL
	JMP .LOOP
.FLAGS_DONE

				
.INIT.VARIABLES ;(2 OF 2)
	LDA #$00								;START COUNTER AT 1ST MOVE
	STA MOB.MOVE.COUNTER					;USED FOR TRACKING DOUBLE MOVER MOBS
	STA MOB.MOVE.ERASE_COUNTER				;START TILE ERASE COUNTER AT $00

	
;CALLING ROUTINE CHECK: INITIAL SCREEN DRAW 
	LDA CALLED_BY.DRAW.SCREEN
	CMP #$01
	BEQ .ONSCREEN_CHECK.NPM

@END
	
.ONSCREEN_CHECK.BEFORE.PLAYER.MOVE
@START
;BEFORE PLAYER MOVE, CHECK TO SEE IF CURRENT MOB MO IS LOCATED ON THE VIEW SCREEN
;(we need to know if it's onscreen before adjusting the mob's position net of player move
;so that the code can tell the difference between a mob on the screen edge that first appeared there as
;as a result of the player move (mob shouldn't move again to avoid the apperance of the mob starting in the 2nd row/column) vs. a mob which was located on the screen edge at 
;the start of the mobs's turn, net of player move, in which case the mob should move.

	LDA SPRITE.RECORD						;LOAD X POSITION OF CURRENT MOB
	CMP #MAP_OBJECTS.X_FLAG.LOWER				;IS OBJECT BEYOND LEFT EDGE OF SCREEN?
	BCC	.MOB.NOTONSCREEN.BPM
	
	CMP #MAP_OBJECTS.X_FLAG.UPPER				;IS OBJECT BEYOND RIGHT EDGE OF SCREEN?
	BCS	.MOB.NOTONSCREEN.BPM	
	
	LDA SPRITE.RECORD+$1
	CMP #MAP_OBJECTS.Y_FLAG.LOWER				;IS OBJECT BEYOND LOWER EDGE OF SCREEN?
	BCC	.MOB.NOTONSCREEN.BPM	
	
	CMP #MAP_OBJECTS.Y_FLAG.UPPER				;IS OBJECT BEYOND UPPER EDGE OF SCREEN?
	BCS	.MOB.NOTONSCREEN.BPM

	LDA #$00								
	STA MOB.SCREEN_STATUS.START					;SET TO $00 (ONSCREEN)
	
	JMP .APPLY.PLAYER_MOVE

.MOB.NOTONSCREEN.BPM
	LDA #$01								
	STA MOB.SCREEN_STATUS.START					;SET TO $01 (OFFSCREEN)

	;***FALLS THROUGH
@END

.APPLY.PLAYER_MOVE
@START	
;INCREMENT MOB X,Y POSITION	NET OF PLAYER'S CURRENT MOVE. 
	LDA PLAYER.MOVE.CURRENT		;#$00=north, $01=south, $02=east, $03=west, $04=PASS
					
	CMP #$00
	BEQ	.MOVE.NORTH
	CMP #$01
	BEQ .MOVE.SOUTH
	CMP #$02
	BEQ	.MOVE.EAST
	CMP #$03
	BEQ .MOVE.WEST
	CMP #$04	;PASS   (**OPT** Speed. Since there is no change in position for a pass move, I dont think an onscreen evaluation before and after applying player move is needed. Thus this line may be able to be changed to something after the net of player move on-screen evaluation)
	BEQ .ONSCREEN_CHECK.NPM
	JMP ERROR1
					
.MOVE.NORTH			;+1 TO Y POSITION
	INC SPRITE.RECORD+$1					;+1 TO Y POSITION
	JMP .ONSCREEN_CHECK.NPM
	
.MOVE.SOUTH			;-1 TO Y POSITION
	DEC SPRITE.RECORD+$1					;-1 TO Y POSITION
	JMP .ONSCREEN_CHECK.NPM	
	
.MOVE.EAST			;-1 TO X POSITION
	DEC SPRITE.RECORD						;-1 TO X POSITION
	JMP .ONSCREEN_CHECK.NPM

.MOVE.WEST			;+1 TO X POSITION
	INC SPRITE.RECORD						;+1 TO X POSITION
	;*****FALLS THROUGH*****				
@END

;TROUBLESHOOTING HOOK
;RETURNS THE LO/HO ADDRESS OF THE LOCTION THE 8 FLAGS ARE STORED
	; LDX #SPRITE.FLAGS_BYTE3
	; LDY /SPRITE.FLAGS_BYTE3
	
	; LDA TEXT
	; LDA PAGE1
	; ;LDA MOB.FLAG2
	; BRK

.ONSCREEN_CHECK.NPM	;
@START
;=====================CODE-SECTION DOCUMENTATION====================================
;The first order of business is to determine if the current mob record
;has an x/y located on the view screen. If not, the rest of this routine 
;is skipped and the code falls through to INCREMENT.INDEX
;
;Unless the mob's record has the SS flag set. Mob's with this flag set
;are permitted to move even when not located on the view screen, IF
;they are located within the regional map. 
;
;SS mobs are managed the same way as non-SS mobs when on the view screen,
;so this code section sets a flag (MOB.SCREEN_STATUS.SS) if the current
;record is an SS located off the view screen. Other routines will use this 
;flag to make flow control descision. 
;=================================================================================



;NET OF PLAYER MOVE (NPM), CHECK TO SEE IF CURRENT MOB MO IS LOCATED ON THE VIEW SCREEN
	LDA #$00								
	STA MOB.SCREEN_STATUS.NPM					;BY DEFAULT SET TO $00 (ONSCREEN)
	STA MOB.SCREEN_STATUS.SS					;BY DEFAULT SET TO $00 (NOT OFFSCREEN SS)
	
	LDA SPRITE.RECORD							;LOAD X POSITION OF CURRENT MOB
	CMP #MAP_OBJECTS.X_FLAG.LOWER				;IS OBJECT BEYOND LEFT EDGE OF SCREEN?
	BCC	.MOB.NOTONSCREEN_STEP	
	
	CMP #MAP_OBJECTS.X_FLAG.UPPER				;IS OBJECT BEYOND RIGHT EDGE OF SCREEN?
	BCS	.MOB.NOTONSCREEN_STEP	
	
	LDA SPRITE.RECORD+$1
	CMP #MAP_OBJECTS.Y_FLAG.LOWER				;IS OBJECT BEYOND LOWER EDGE OF SCREEN?
	BCC	.MOB.NOTONSCREEN_STEP	
	
	CMP #MAP_OBJECTS.Y_FLAG.UPPER				;IS OBJECT BEYOND UPPER EDGE OF SCREEN?
	BCS	.MOB.NOTONSCREEN_STEP


	
	JMP MOB.IDENTIFY.TILE_LOCATION	

.MOB.NOTONSCREEN_STEP

.STORM.CHECK ;is offscreen mob a storm?
	LDA SPRITE.RECORD+$2 ;load sprite tile type
	CMP #TILE_ID.STORM	;is sprite a storm?
	BNE .NOT.STORM		;if no, then proceed normally
	LDA #$01			;if yes, then proceed directly to movement routine. This saves a lot of clock cycles and is viable because storms ignore all obstacles. 
	STA MOB.SCREEN_STATUS.SS	;set SS flag to ON so that no tiles are drawn after the move is made. 
	JMP MOB.MOVE.NORTH_EAST
.NOT.STORM

	LDA #$01
	STA MOB.SCREEN_STATUS.NPM
	
	LDA CALLED_BY.DRAW.SCREEN
	CMP #$01
	BEQ .NOT.SS


			
	LDA MOB.FLAG1				;mob flag1: on=ss
	CMP #$01					;is current mob an ss?
	BEQ .SS						;if yes, set SS flag
	LDA SPRITE.RECORD+$A		;load sprite type
	CMP #S_ENTITY_TYPE.BLD_NPC				;is sprite a building NPC?	
	BEQ .SS						;if yes, set SS flag 
	;**FALLS THROUGH**			;if no, don't move or draw sprite
.NOT.SS				
	JMP MOB.NOTONSCREEN
	
.SS
	LDA #$01					;set offscreen ss flag
	STA MOB.SCREEN_STATUS.SS

;IS SPRITE IN A BUILDING?
	LDA PLAYER.MAP.LOCATION_TYPE		;load location type code
	; CMP #MAP.TYPE.TOWN_VILLAGE			;is location type = building?
	; BEQ	MOB.IDENTIFY.LOCATION			;if yes, do not do the SPRITE.SS_REGION.CHECK because buildings are exactly 1 regional zone in size. 
	; ;**FALLS THROUGH**

	;is a building map active
	;**OPT** Memory. Speed. Right now there is only one code for location type, which is used to determine the tile_set to use and for other location-type specific features. There are a lot of the later. It might be a savings if the 
					;there were a tile-set code separate from location type code, and there was a default tile-set picked using the location-type code if tile-set code was set to default. This might require adding another byte to some hex tables
					;but I suspect it would pay off. 
	CMP #MAP.TYPE.BUILDING.GRE 	;is map type = building?
	BCC .MAP.TYPE_CHECK.DONE		;if no
	CMP #MAP.TYPE.BUILDING.LT	;is map type = building?
	BCS .MAP.TYPE_CHECK.DONE		;if no
	JMP MOB.IDENTIFY.LOCATION 			;if yes
.MAP.TYPE_CHECK.DONE
	;**FALLS THROUGH**

	
@END

MOB.SS.REGI0N_CHECK	;======DETERMINES IF AN OFFSCREEN SS IS LOCATED ON THE REGIONAL MAP=========
@START
;=====================CODE-SECTION DOCUMENTATION====================================
;An offscreen SS mob is not permitted to move if it is not located on
;the regional map. The reason is because map data is needed for collision checks,
;and the regional map is only uncompressed tile data that exists in 
;memory when this routine is run. All other zone data is compressed
; and stored in auxiliary memory.
;
;Theoretically, a zone could be copied from auxiliary memory and uncompressed,
;to faciliate a few tile_ID lookups to do collision checks but I concluded
;this would be speed prohibitive. The player is waiting for the screen
;to be drawn after pressing a movement key, we've got to hurry it up!!
;
;=================================================================================
		
;THESE FLAGS ARE ADJUSTED ON EACH PLAYER MOVE (see movement_manager.asm) SINCE PLAYERS LOCATION ON REGIONAL MAP CHANGES. 	
	LDA SPRITE.RECORD+$0						;LOAD X POSITION OF CURRENT MOB
	CMP MAP_OBJECTS.SS.X_FLAG.LOWER				;IS SS MOB BEYOND LEFT EDGE OF REGIONAL MAP?
	BCC	.MOB.NOTINREGION	

	CMP MAP_OBJECTS.SS.X_FLAG.UPPER				;IS SS MOB BEYOND RIGHT EDGE OF REGIONAL MAP?
	BCS	.MOB.NOTINREGION	
	
	LDA SPRITE.RECORD+$1
	CMP MAP_OBJECTS.SS.Y_FLAG.LOWER				;IS SS MOB BEYOND TOP EDGE OF REGIONAL MAP?
	BCC	.MOB.NOTINREGION
	
	CMP MAP_OBJECTS.SS.Y_FLAG.UPPER				;IS SS MOB BEYOND BOTTOM EDGE OF REGIONAL MAP?
	BCS	.MOB.NOTINREGION
	
		
	INC MOB.GEN.SS_QTY							;INCREMENT THE TOM CANFIELD COUNTER.
												;ACTUALLY, THAT WON'T HELP, INCREMENT THE SS COUNTER
												;INSTEAD WHICH TRACKS THE NUMBER OF OFFSCREEN SS
												;IN THE CURRENT REGION.


	JMP MOB.IDENTIFY.LOCATION
	
.MOB.NOTINREGION		
	JMP MOB.NOTONSCREEN
@END
	
MOB.IDENTIFY.LOCATION ;				
@START
;debug: door ($14) is set to #$10 here when X= $00 (NPC mode)
					
	LDA MOB.SCREEN_STATUS.SS
	CMP #$01 									;IS MOB AN OFFSCREEN SS?
	BEQ MOB.IDENTIFY.MAP_LOCATION				;IF YES, FIND THE RMAP LOCATION FOR USE WITH COLLISION CHECKS ONLY
	JMP MOB.IDENTIFY.TILE_LOCATION				;IF NO, FIND THE SCREEN TILE LOCATION OF THE MOB	

MOB.IDENTIFY.MAP_LOCATION ;=============USED ONLY FOR MOBS WITH SS FLAG SET=======
@START

;=====================CODE-SECTION DOCUMENTATION====================================
;
;Converts the mob's relative x,y position to an RMAP position. This
;is needed for collision checks later since the mob is not on the view
;screen and thus the screen arrays won't have the applicable tile data. 
;=================================================================================
	
;INIT VARIABLES
	LDA #$00
	STA MOB.POSITION.X_GR
	STA MOB.POSITION.X_LT
	STA MOB.POSITION.Y_GR	
	STA MOB.POSITION.Y_LT


;IDENTIFY MAP LOCATION OF MOB SO WE CAN DETERMINE WHICH MAP LOCATIONS TO CHECK FOR COLLISSIONS
	LDA SPRITE.RECORD
	CMP #MAP_OBJECTS.PLAYER_LOCATION
	BCC .MOB.MO_X_LESS

;MO_X_GREATER	
	LDA SPRITE.RECORD
	SEC
	SBC #MAP_OBJECTS.PLAYER_LOCATION
	STA MAP_OBJECTS.X_ADJ
	
;======================================	
;RMAP(2)+ MAP_OBJECTS.X_ADJ(1)


; DO THE MATH
	CLD 
    CLC                       	 ;ALWAYS BEFORE ADD
    LDA RMAP
    ADC MAP_OBJECTS.X_ADJ
    STA TEMP16
		 
    LDA RMAP+$1
    ADC #$00					;carry flag not cleared via CLC intentionally, it's part of 16-bit adding. 
    STA TEMP16+$1
	
;======================================


;RECORD X-AXIS COMPARISON FOR MOB MOVEMENT 	
	LDA SPRITE.RECORD
	CMP #MAP_OBJECTS.PLAYER_LOCATION
	BEQ .X_AXIS.SAME
	
	STA MOB.POSITION.X_GR								;RECORDS THAT MOB'S X-AXIS IS GREATER THAN PLAYER'S. IT DOESN'T MATTER WHAT VALUE IS STORED, AS LONG AS IT IS != $00 

.X_AXIS.SAME	
	JMP .MOB.YTEST

.MOB.MO_X_LESS
	LDA #MAP_OBJECTS.PLAYER_LOCATION
	SEC
	SBC SPRITE.RECORD
	STA MAP_OBJECTS.X_ADJ
	
;======================================	
;RMAP(2)+ MAP_OBJECTS.X_ADJ(1)
	
	CLD 
    SEC                           ;ALWAYS BEFORE SUBTRACTION
    LDA RMAP
    SBC MAP_OBJECTS.X_ADJ
    STA TEMP16
    LDA RMAP+$1
    SBC #$00
    STA TEMP16+$1
;======================================
	
;RECORD X-AXIS COMPARISON FOR MOB MOVEMENT 	

	STA MOB.POSITION.X_LT								;RECORDS THAT MOB'S X-AXIS IS LESS THAN PLAYER'S. IT DOESN'T MATTER WHAT VALUE IS STORED, AS LONG AS IT IS != $00 

.MOB.YTEST	


	LDA SPRITE.RECORD+$1
	CMP #MAP_OBJECTS.PLAYER_LOCATION					;IS MOB Y AXIS < PLAYER Y AXIS?
	BCC .MOB.MO_Y_LESS
	
	LDA SPRITE.RECORD+$1
	SEC
	SBC #MAP_OBJECTS.PLAYER_LOCATION
	TAY
	
	LDA RMAP.MULTIPLY_TABLE.LO,Y
	STA MAP_OBJECTS.Y_ADJ
	LDA RMAP.MULTIPLY_TABLE.HO,Y
	STA MAP_OBJECTS.Y_ADJ+$1
	

	
;======================================	
;TEMP16(2)- MAP_OBJECTS.Y_ADJ(1)


; DO THE MATH
	CLD 								;**OPT** Memory. CLD isn't needed every time. to remove it though I need to know when I'll be using decimal mode (proabbly just with merchants) and then the map routine probably just needs to to a CLD once, so anytime somebody exits a merchant conversation it will get executed. 
    CLC                       			 ;ALWAYS BEFORE ADD
    LDA TEMP16							;PARTIAL CALCULATION. CONTAINS #MAP_OBJECTS.PLAYER_LOCATION +/- MAP_OBJECTS.X_ADJ 
    ADC MAP_OBJECTS.Y_ADJ
    STA MAP_OBJECTS.MAP_LOCATION
		 
    LDA TEMP16+$1
    ADC MAP_OBJECTS.Y_ADJ+$1			;carry flag not cleared via CLC intentionally, it's part of 16-bit adding. 
    STA MAP_OBJECTS.MAP_LOCATION+$1
	
;======================================



;RECORD Y-AXIS COMPARISON FOR MOB MOVEMENT 	
	LDA SPRITE.RECORD+$1
	CMP #MAP_OBJECTS.PLAYER_LOCATION
	BEQ .Y_AXIS.SAME
	
	STA MOB.POSITION.Y_GR								;RECORDS THAT MOB'S Y-AXIS IS GREATER THAN PLAYER'S. IT DOESN'T MATTER WHAT VALUE IS STORED, AS LONG AS IT IS != $00 
	
.Y_AXIS.SAME	
	JMP .EXIT

.MOB.MO_Y_LESS
	LDA #MAP_OBJECTS.PLAYER_LOCATION
	SEC
	SBC SPRITE.RECORD+$1		
	TAY
	LDA RMAP.MULTIPLY_TABLE.LO,Y
	STA MAP_OBJECTS.Y_ADJ
	LDA RMAP.MULTIPLY_TABLE.HO,Y
	STA MAP_OBJECTS.Y_ADJ+$1

;======================================	
;TEMP16(2)- MAP_OBJECTS.Y_ADJ(1)
	
	CLD 
    SEC                           ;ALWAYS BEFORE SUBTRACTION
    LDA TEMP16
    SBC MAP_OBJECTS.Y_ADJ
    STA MAP_OBJECTS.MAP_LOCATION
    LDA TEMP16+$1
    SBC MAP_OBJECTS.Y_ADJ+$1
    STA MAP_OBJECTS.MAP_LOCATION+$1
;======================================	
		
;RECORD Y-AXIS COMPARISON FOR MOB MOVEMENT 	
	STA MOB.POSITION.Y_LT								;RECORDS THAT MOB'S Y-AXIS IS LESS THAN PLAYER'S. IT DOESN'T MATTER WHAT VALUE IS STORED, AS LONG AS IT IS != $00 
	

.EXIT
;IS MOB ON A ROW OR COLUMN ON THE EDGE OF THE VIEW SCREEN (/AKA AN APPROACH ROW/COLUMN), AND A ROW OR COLUMN IS WITHIN THE VIEW SCREEN BOUNDARIES?
;IF YES, IDENTIFY THE TILE LOCATION JUST IN CASE THE MOB MOVES INTO THE VIEW SCREEN.
	
			
	LDY #$00	
.LOOP	
	LDA SPRITE.RECORD
	CMP MAP_OBJECTS.X_APPROACH,Y				;CHECK X-AXIS APPROACH VALUES	
	BEQ .CHECK.Y_AXIS							;IF ON X APPROACH COLUMN, CHECK Y-AXIS
	
	LDA SPRITE.RECORD+$1
	CMP MAP_OBJECTS.Y_APPROACH,Y				;CHECK Y-AXIS APPROACH VALUES
	BEQ .CHECK.X_AXIS							;IF ON Y APPROACH ROW, CHECK X-AXIS
	
	INY
	CPY #$04
	BNE .LOOP
	
.CHECK.X_AXIS
	LDA SPRITE.RECORD						;LOAD X POSITION OF CURRENT MOB
	CMP #MAP_OBJECTS.X_FLAG.LOWER				;IS OBJECT BEYOND LEFT EDGE OF SCREEN?
	BCC	.MOB_NOT_ON_EDGE	
	
	CMP #MAP_OBJECTS.X_FLAG.UPPER				;IS OBJECT BEYOND RIGHT EDGE OF SCREEN?
	BCS	.MOB_NOT_ON_EDGE	

	JMP MOB.IDENTIFY.TILE_LOCATION

.CHECK.Y_AXIS	
	LDA SPRITE.RECORD+$1
	CMP #MAP_OBJECTS.Y_FLAG.LOWER				;IS OBJECT BEYOND LOWER EDGE OF SCREEN?
	BCC	.MOB_NOT_ON_EDGE	
	
	CMP #MAP_OBJECTS.Y_FLAG.UPPER				;IS OBJECT BEYOND UPPER EDGE OF SCREEN?
	BCS	.MOB_NOT_ON_EDGE
	
	JMP MOB.IDENTIFY.TILE_LOCATION
	

.MOB_NOT_ON_EDGE
	JMP MOB.MOVEMENT
@END
	
MOB.IDENTIFY.TILE_LOCATION	
@START			
;=====================CODE-SECTION DOCUMENTATION====================================
;
;This code section (virtually identical to it's transport counterpart above,
;is responsible for converting the mob's relative x,y position into a 
;screen tile location (the index to the screen arrays).
;The screen tile location is later to used during the mob movement
;routines in activies such as collission checks. The screen tile location
;is also used to draw the tile since DRAW.TILE.SINGLE needs this as a parameter
;
;The mechanic of doing the conversion is essentially done by using the 
;difference between the player's x/y and the mob's x/y as an adjustment to 
;the players screen tile position, using the appropriate offsets for the
;screen tile grid. For example, for a y axis differential, a multiplication table is used
;to calculate the offset to apply to the player's location. 
;
;Calculating the difference between player and mob x,y starts 
;determining whether the mob x,y is less than the player x,y.
;This is done so that negative numbers can be avoided. 
;
;=================================================================================
	
	;**OPT** Memory. Mostly replace this routine with a JSR to CALCULATE.DISTANCE. The MTT code at the bottom would need to stay.
			
;INIT VARIABLES
	LDA #$00
	STA MOB.POSITION.X_GR
	STA MOB.POSITION.X_LT
	STA MOB.POSITION.Y_GR	
	STA MOB.POSITION.Y_LT

;IDENTIFY SCREEN TILE #							;OBJECT IS ON SCREEN, WHICH SCREEN TILE SHOULD WE DRAW IT IN?
	LDA SPRITE.RECORD
	CMP #MAP_OBJECTS.PLAYER_LOCATION
	BCC .MOB.MO_X_LESS
	
	;ACC = 
	SEC
	SBC #MAP_OBJECTS.PLAYER_LOCATION
	STA MAP_OBJECTS.X_ADJ
	LDA #SCREEN.ARRAY.PLAYER_LOCATION
	CLC
	ADC MAP_OBJECTS.X_ADJ
	STA TEMP

;RECORD X-AXIS COMPARISON FOR MOB MOVEMENT 	
	LDA SPRITE.RECORD
	CMP #MAP_OBJECTS.PLAYER_LOCATION
	BEQ .X_AXIS.SAME	
	STA MOB.POSITION.X_GR								;RECORDS THAT MOB'S X-AXIS IS GREATER THAN PLAYER'S. IT DOESN'T MATTER WHAT VALUE IS STORED, AS LONG AS IT IS != $00 

.X_AXIS.SAME	


	JMP .MOB.YTEST

.MOB.MO_X_LESS
	LDA #MAP_OBJECTS.PLAYER_LOCATION
	SEC
	SBC SPRITE.RECORD
	STA MAP_OBJECTS.X_ADJ
	LDA #SCREEN.ARRAY.PLAYER_LOCATION
	SEC
	SBC MAP_OBJECTS.X_ADJ
	STA TEMP

;RECORD X-AXIS COMPARISON FOR MOB MOVEMENT 	

	STA MOB.POSITION.X_LT								;RECORDS THAT MOB'S X-AXIS IS LESS THAN PLAYER'S. IT DOESN'T MATTER WHAT VALUE IS STORED, AS LONG AS IT IS != $00 


.MOB.YTEST	

	LDA SPRITE.RECORD+$1
	CMP #MAP_OBJECTS.PLAYER_LOCATION
	BCC .MOB.MO_Y_LESS
	
	SEC
	SBC #MAP_OBJECTS.PLAYER_LOCATION
	TAY
	LDA SCREEN.MULTIPLY_TABLE,Y
	STA MAP_OBJECTS.Y_ADJ
	
;Calculate screen array index	
	LDA TEMP ;the X-axis adjustment	;PARTIAL CALCULATION. CONTAINS #MAP_OBJECTS.PLAYER_LOCATION +/- MAP_OBJECTS.X_ADJ 								;PARTIAL CALCULATION. CONTAINS #MAP_OBJECTS.PLAYER_LOCATION +/- MAP_OBJECTS.X_ADJ 
	CLC
	ADC MAP_OBJECTS.Y_ADJ
	STA MAP_OBJECTS.TILE_LOCATION
		

;RECORD X-AXIS COMPARISON FOR MOB MOVEMENT 	
	LDA SPRITE.RECORD+$1
	CMP #MAP_OBJECTS.PLAYER_LOCATION
	BEQ .Y_AXIS.SAME
	
	STA MOB.POSITION.Y_GR								;RECORDS THAT MOB'S Y-AXIS IS GREATER THAN PLAYER'S. IT DOESN'T MATTER WHAT VALUE IS STORED, AS LONG AS IT IS != $00 
.Y_AXIS.SAME	

	JMP .FINAL_ROUTINE

.MOB.MO_Y_LESS
	LDA #MAP_OBJECTS.PLAYER_LOCATION
	SEC
	SBC SPRITE.RECORD+$1		
	TAY
	LDA SCREEN.MULTIPLY_TABLE,Y
	STA MAP_OBJECTS.Y_ADJ
	
;Calculate screen array index	
	LDA TEMP ;the X-axis adjustment	;PARTIAL CALCULATION. CONTAINS #MAP_OBJECTS.PLAYER_LOCATION +/- MAP_OBJECTS.X_ADJ 								;PARTIAL CALCULATION. CONTAINS #MAP_OBJECTS.PLAYER_LOCATION +/- MAP_OBJECTS.X_ADJ 
	SEC
	SBC MAP_OBJECTS.Y_ADJ
	STA MAP_OBJECTS.TILE_LOCATION	;screen array index of the current S_ENTITY									

;RECORD Y-AXIS COMPARISON FOR MOB MOVEMENT 	
	STA MOB.POSITION.Y_LT					;RECORDS THAT MOB'S Y-AXIS IS LESS THAN PLAYER'S. IT DOESN'T MATTER WHAT VALUE IS STORED, AS LONG AS IT IS != $00 



.FINAL_ROUTINE


;IF MULTI-TILE MOB, RECORD THE SCREEN TILE # FOR EACH OF IT'S FOUR TILES

	LDA MOB.FLAG3							
	CMP #$01								;IS CURENT MO RECORD FOR A MULTI-TILE MOB?
	BNE MOB.MOVEMENT
	LDY MAP_OBJECTS.TILE_LOCATION			;LOAD TILE LOCATION CALCUALTED IN MAIN ROUTINE ABOVE
	STY MOB.MT.TILE_LOCATIONS				;SAVE AS TILE #0 (UPPER LEFT) OF MT MOB

	INY
	STY MOB.MT.TILE_LOCATIONS+$1			;SAVE AS TILE #1 (UPPER RIGHT) OF MT MOB
	TYA
	CLC
	ADC #SCREEN.ARRAY.OFFSET
	STA MOB.MT.TILE_LOCATIONS+$3			;SAVE AS TILE #3 (LOWER RIGHT) OF MT MOB
	TAY
	DEY
	STY MOB.MT.TILE_LOCATIONS+$2			;SAVE AS TILE #2 (LOWER LEFT) OF MT MOB

	;******FALLS THROUGH
	
@END
@END
	
MOB.MOVEMENT				
@START
;debug: door ($0C) is set to #$10 here when X= $78 (NPC mode)


					



;=====================CODE-SECTION DOCUMENTATION====================================
;
;The following is an outline of the approach taken for mob movement:
;	*Is mob stopped by slow progress (terrain driven)? If yes, them move = pass (.SLOW.PROGRESS.CHECK)
;  *Collision check: examine the adjancent tile in all 4 directions and 
;		record whether each is blocked or permitted. (CHOOSE.COLLISION.SUBROUTINE)
;  *Based on the mob's position on the screen relative to the player, 
;		record primary and secondary moves based on the shortest path to 
;		the player. Movement profiles determined by flags aren't considered yet. (IDENTIFY.SCREEN_SECTION) 
;	*Use random numbers to choose between the various primary and secondary moves. 
;		The choice is based on which, if any, moves are blocked and the movement profile of the mob 
;		as indicated by the flag setting on the mob's record. (CHOOSE.MOB.MOVE)
;		Be careful if you have to take a look at the mob's record. It's worse than I thought (Bob the Orc's greatest polka hits).
;=================================================================================
			
;====TOP LEVEL FLOW CONTROL & INIT VARIABLES====
@START

;CALLING ROUTINE CHECK: INITIAL SCREEN DRAW 

	LDA CALLED_BY.DRAW.SCREEN
	CMP #$01
	BNE .NOT_INITIAL_DRAW
	JMP MOB.DRAWTILE.ENTRANCE2


.NOT_INITIAL_DRAW
;IS MOB APPEARING ON SCREEN FOR FIRST TIME? IF SO, SKIP MOVEMENT. 
;(IF SCREEN STATUS AT START WAS OFFSCREEN AND SCREEN STATUS AFTER PLAYER MOVE IS ONSCREEN, THEN SKIP)
;


	LDA MOB.SCREEN_STATUS.START			;CHECK SCREEN STATUS AT START
	CMP #$01							;$01 = OFFSCREEN
	BNE .MOB.MOVEMENT.START
	LDA MOB.SCREEN_STATUS.NPM			;CHECK SCREEN STATUS NET OF PLAYER MOVE (NPM)
	CMP #$00							;$00 = ONSCREEN
	BNE .MOB.MOVEMENT.START
	
	LDA #$10
	STA MOB.MOVE.COUNTER				;WHEN A DOUBLE MOVER FIRST APPEARS ON SCREEN IT FORFEITS IT'S 2ND MOVE
	JMP MOB.DRAWTILE
	
.MOB.MOVEMENT.START
;TESTHOOK1
;Reports the RMAP of the current MOB (only works if MOB has SS Flag set)
				;LDX MAP_OBJECTS.MAP_LOCATION
				;LDY MAP_OBJECTS.MAP_LOCATION+$1


;INIT VARIABLES
	LDY #$00
	STY MOB.MOVE.TOTAL_OPEN_PATHS
	LDA #$FF
.INIT_LOOP
	STA MOB.MOVE.OPTIONS_PRIMARY,Y							
	STA MOB.MOVE.OPTIONS_SECONDARY,Y	
	STA MOB.MOVE.OPEN_PATHS,Y
	LDA #$00
	STA MOB.MOVES.BLOCKED,Y	
	CPY #$04
	BEQ .DONE2
	INY
	JMP .INIT_LOOP
.DONE2	
@END

;====IS MOB FROZEN?=============================
;(code review 4/11/16; I forgot what this is for but the "frozen" status is based on
;the mobs FLAG0 and FLAG1 being set to non-aggressive and SS, an incompatible combination that should never occur
;so I apparently used that flag combination to signal this "frozen" condition.)
;(5/20/16; I reviewed this again and I think this was originally added for 
;playtesting and/or making videos, I probably wanted some mobs to
;stand still for some reason. The value for this flag combination is $02)
@START
.MOB.FROZEN.CHECK

	LDA MOB.FLAG0					;MOB FROZEN IF FLAG0 = $00 AND FLAG1 = $01
	CMP #$01
	BCS .SLOW.PROGRESS.CHECK
	LDA MOB.FLAG1
	CMP #$01
	BEQ .SPRITE.PASS.MOVE.ENTRANCE_STEP
	JMP .SLOW.PROGRESS.CHECK
	
	
.SPRITE.PASS.MOVE.ENTRANCE_STEP
	JMP SPRITE.PASS.MOVE.ENTRANCE	;SKIP COLLISION CHECKS AND REGULAR MOVE SELECTION ROUTINES

	
@END

;====IS MOB ON SLOW PROGRESS TERRAIN?===========
@START
.SLOW.PROGRESS.CHECK


	LDY MAP_OBJECTS.TILE_LOCATION

	LDA SCREEN.TILE.DATA,Y
	CMP #TILE_ID.HILLS
	BNE .CHECK.GAME.TURN
	
	JSR RANDOM.8	
	CMP #$C0								;IF MOB TILE IS ON HILLS THEN 25% CHANCE OF SLOW PROGRESS
	BCC .CHECK.GAME.TURN					;NO SLOW PROGRESS
	
	LDA #$01								;SET SLOW PROGRESS FLAG TO ON. THIS IS TO FORCE A DRAW OF THE TILE IF MOB IS A DOUBLE MOVER BECAUSE SLOW PROGRESS = PASS AND NORMALLY TILES AREN'T DRAWN ON A PASS BECAUSE THE MOB'S POSITION HASN'T CHANGED. HOWEVER, IF IT'S THE DOUBLE MOVER'S SECOND MOVE WHEN THE SLOW PROGRESS/PASS OCCURS, IT'S POSITION HAS CHANGED. 
	STA MOB.MOVE.SLOW.PROGRESS 
	;**FALLS THROUGH
	
.FORCE_PASS	
	JMP SPRITE.PASS.MOVE.ENTRANCE

.CHECK.GAME.TURN
	LDA GAME.TURN.CONTROL					;$00 FOR PLAYER TURN, HOLDS $01 FOR MOB/NPC TURN
	BNE .CHOOSE.COLLISION.SUBROUTINE		;IF MOBS TURN, CONTINUE 
	JMP .FORCE_PASS							;IF PLAYERS TURN, (THIS COULD HAPPEN IF PLAYER HAS BOARDED TRANSPORT THAT GIVES PLAYER MULTIPLE MOVES)

@END
	
.CHOOSE.COLLISION.SUBROUTINE			
@START
;CHOOSE COLLISION CHECK SUBROUTINE		

		
	; LDA MOB.FLAG1										;FLAG1: ON=SS										
	; CMP #$01											
	; BNE COLLISION_CHECK_ONSCREEN						;IF NOT SS, ALWAYS USE ONSCREEN ROUTINE

	
	LDA SPRITE.RECORD+$2
	CMP #TILE_ID.STORM
	BNE .NOT.STORM
	LDA #$05
	STA MOB.MOVE.CURRENT
	JMP MOB.MOVE.MAKE
	
.NOT.STORM
	
	LDA MOB.SCREEN_STATUS.SS
	CMP #$01						;$01=SS						
	BNE COLLISION_CHECK_ONSCREEN	;IF NOT SS, ALWAYS USE ONSCREEN ROUTINE
	
	LDA MOB.SCREEN_STATUS.NPM
	CMP #$00											;EVEN IF SS, USE ONSCREEN ROUTINE IF THE SS'S X/Y IS ON THE VIEW SCREEN, AS MAP POSITION IS NO LONGER NEEDED 
	BEQ COLLISION_CHECK_ONSCREEN
	
	JMP COLLISION_CHECK_OFFSCREEN						;ONLY IF AN SS, AND OFFSCREEN. USES RMAP POSITION FOR COLLISION CHECKS. 
	
	
COLLISION_CHECK_ONSCREEN_MTT_STEP
	JMP COLLISION_CHECK_ONSCREEN_MTT
	
COLLISION_CHECK_ONSCREEN
@START



;Collision check for all on-screen mobs (including SS type)	
	LDA MOB.FLAG3								;IS CURRENT MOB RECORD A MULTI-TILE MOB?
	CMP #$01
	BEQ COLLISION_CHECK_ONSCREEN_MTT_STEP		;IF YES, USE THE MULTI-TILE COLLISION CHECK

;===COLLISION CHECK

;STORE TILE #S ADJACENT TO MOB IN AN ARRAY SO WE CAN USE A LOOP TO APPLY COLLISION RULES

;SAVE TILE # NORTH OF MOB
.SET.NORTH.ADJACENT.TILE
	LDA SPRITE.RECORD+$1			;load y-axis
	CMP #MAP_OBJECTS.Y.FIRST_ROW 	;is tile in first row?
	BNE .NOT.FIRST.ROW				;if no, proceed to set value
	LDA #$FF
	STA MOB.ADJACENT_TILES+$0
	JMP .SET.SOUTH.ADJACENT.TILE
	
.NOT.FIRST.ROW	
	TYA
	SEC
	SBC #SCREEN.ARRAY.OFFSET
	STA MOB.ADJACENT_TILES+$0				;BORROWING SCREEN.TILE.HOPPER TO STORE THE TILE #S ADJACENT TO THE MOB
	;**FALLS THROUGH**
	
;SAVE TILE # SOUTH OF MOB
.SET.SOUTH.ADJACENT.TILE
	LDA SPRITE.RECORD+$1			;load y-axis
	CMP #MAP_OBJECTS.Y.LAST_ROW 	;is tile in last row?
	BNE .NOT.LAST.ROW				;if no, proceed to set value

			
	LDA #$FF
	STA MOB.ADJACENT_TILES+$1
	JMP .SET.EAST.ADJACENT.TILE
	
.NOT.LAST.ROW
	TYA
	CLC
	ADC #SCREEN.ARRAY.OFFSET
	STA MOB.ADJACENT_TILES+$1					;BORROWING SCREEN.TILE.HOPPER TO STORE THE TILE #S ADJACENT TO THE MOB
	;**FALLS THROUGH**
	
;SAVE TILE # EAST OF MOB
.SET.EAST.ADJACENT.TILE
	LDA SPRITE.RECORD+$0			;load x-axis
	CMP #MAP_OBJECTS.X.LAST_COLUMN  ;is tile in last row (non-combat screens)?
	BEQ .IN.LAST.COLUMN ;if yes, record direction as off screen
	CMP #MAP_OBJECTS.X.COMBAT.LAST_COLUMN ;is tile in last row (combat screens)
	BNE .NOT.LAST.COLUMN ;if no, then proceed to set value 
						;if yes, verify the S_ENTITY is in combat before recording direction as off screen
	;is S_ENTITY is in combat?
	;is combat active
	LDA PLAYER.MAP.LOCATION_TYPE	;load map type of players current location
	CMP #MAP.TYPE.COMBAT	;is the combat map loaded?
	BNE .NOT.LAST.COLUMN 	;if no, S_ENTITY is not in combat. Do not record direction as off-screen.		;**OPT** Memory. Is this check needed? seemed to end up at the same place either way.
	;**FALLS THROUGH	;if no, then S_ENITITY must be a combat mob, special (combat) or player character (combat). Record direction as offscreen
	
	; LDA SPRITE.RECORD+$A	;load S_ENTITY type
	; BEQ .NOT.LAST.COLUMN	;is S_ENTITY a non-combat mob? If yes, S_ENTITY is not in combat. Do not record direction as off-screen.
	; CMP #S_ENTITY_TYPE.BLD_NPC 	;is S_ENTITY a building or dungeon NPC?
	; BCS .NOT.LAST.COLUMN		;if yes, S_ENTITY is not in combat. Do not record direction as off-screen.
	; ;**FALLS THROUGH		;if no, then S_ENITITY mus be a combat mob, special (combat) or player character (combat). Record direction as offscreen 
	
.IN.LAST.COLUMN	
	LDA #$FF
	STA MOB.ADJACENT_TILES+$2
	JMP .SET.WEST.ADJACENT.TILE
	


.NOT.LAST.COLUMN
	INY
	STY MOB.ADJACENT_TILES+$2					;BORROWING SCREEN.TILE.HOPPER TO STORE THE TILE #S ADJACENT TO THE MOB
	;**FALLS THROUGH**
	
;SAVE TILE # WEST OF MOB
.SET.WEST.ADJACENT.TILE
	LDA SPRITE.RECORD+$0			 ;load x-axis
	CMP #MAP_OBJECTS.X.FIRST_COLUMN  ;is tile in last row?
	BNE .NOT.FIRST.COLUMN			 ;if no, proceed to set value
	LDA #$FF
	STA MOB.ADJACENT_TILES+$3
	JMP .CHECK.SPRITE.TYPE
	
.NOT.FIRST.COLUMN
	DEY
	DEY
	STY MOB.ADJACENT_TILES+$3					;BORROWING SCREEN.TILE.HOPPER TO STORE THE TILE #S ADJACENT TO THE MOB
	;**FALLS THROUGH**
	
.CHECK.SPRITE.TYPE
	LDA SPRITE.RECORD+$2
	CMP #COLLISION_FLAG.MOB_SEA.START			;FIRST TILE IN SEA MOB RANGE. 
	BCS .COLLISION.LOOP.MOB_SEA_STEP
	JMP .LOOP.MOB_LAND.ENTRANCE

.COLLISION.LOOP.MOB_SEA_STEP
			;Temporary rules, permitting all moves
			; LDA #$00
			; STA MOB.MOVES.BLOCKED+$0
			; STA MOB.MOVES.BLOCKED+$1
			; STA MOB.MOVES.BLOCKED+$2
			; LDA #$00
			; STA MOB.MOVES.BLOCKED+$3
			
			JMP COLLISION_TESTS_COMPLETE
		
	JMP .COLLISION.LOOP.MOB_SEA
	
.MOVE_BLOCKED.SCREEN_EDGE_STEP	
	JMP .MOVE_BLOCKED.SCREEN_EDGE
	
@MIDDLE	
.LOOP.MOB_LAND.ENTRANCE
	LDX #$00
.LOOP.MOB_LAND
	STX SAVED.XREG.LOCAL1			;save loop counter

	LDA MOB.ADJACENT_TILES,X					;LOAD NEXT ADJACENT TILE#
	TAY
			
;IS DIRECTION OFF SCREEN?
	CPY #$FF
	BEQ .MOVE_BLOCKED.SCREEN_EDGE_STEP
	
;IS SCREEN CENTER ADJACENT? IF SO, ATTACK (MOBS ONLY)
;(the screen center tile is the player, except for comabt mode)

;**OPT** Memory. Speed. I'm not sure why I set this up to check each mob type. It seem like 
;all that is needed is to check if combat is loaded and if not then skip the
;CPY #$5D check to determine if the center is adjacent. See .PLAYER.ADJACENT.CHECK in the MTT collision check routine
;for an example of this implementation.

	CPY #SCREEN.ARRAY.PLAYER_LOCATION	;is player adjacent to sprite?
	BEQ .CENTER.ADJACENT			;if yes, evaluate further
	JMP .CENTER.ADJACENT_CHECK.DONE	;if no, next test

.CENTER.ADJACENT
	;is s_enity a non-combat mob
	LDA SPRITE.RECORD+$A			;load sprite type
	CMP #S_ENTITY_TYPE.NC_MOB		;is sprite a non-combat MOB
		;**OPT** this CMP seem unneeded since the branch is on zero
	BEQ .CENTER.ADJACENT.TO.MOB
				;if no, treat as collision unless it's a hostile NPC, or ignore if S_ENTITY is a combat player character or a combat mob as the center has nothing to do with the location of opponents in combat mode. 


	;is s_entity a combat mob, combat special, or combat player character?				
	LDA SPRITE.RECORD+$A			;load sprite type
	CMP #S_ENTITY_TYPE.BLD_NPC		;is sprite a combat MOB, combat special, or a combat player character? 			
	BCC .CENTER.ADJACENT_CHECK.DONE
	
	;is s_enity a hostile npc? 
	LDA SPRITE.RECORD+$6			;load transit flag
	CMP #$FF						;is it set to hostile? (used to put guards in hostile mode toward player)
	BNE .NOT.HOSTILE				;if no, then the move in this direction is blocked
	JMP SPRITE.INITIATED.COMBAT					;if yes, then ATTACK! The red orc fights back! Message from Bilbo: if you try to kill a balrog you are a fool! If you know where this is from let me know! (mark@6502workshop.com)
.NOT.HOSTILE		
	JMP .MOVE_BLOCKED

.CENTER.ADJACENT.TO.MOB
	JMP SPRITE.INITIATED.COMBAT
.CENTER.ADJACENT_CHECK.DONE


	
.COMBAT_ONLY_CHECKS.COMPLETE
			
;COMBAT SCREEN EDGE
;(those checks are done COMBAT.MOVE_MGR)
	
.OVERRIDE_TEST	
;IS MOB COLLISION OVERRIDE ON?
	LDA MOB.COLLISION_OVERRIDE
	CMP #$01
	BEQ .MOVE_PERMITTED

;DOES MOB HAVE A SPECIAL COLISSION FLAG? 
;Note: a special colission flag specifies the tile types permitted instead of the standard rules which specify the tile tiles not permitted

	LDA MOB.FLAG5
	CMP #$01
	BEQ .SPECIAL.COLLISION.FLAG
	
.WALKING_RULES		
;APPLY MOB WALKING COLLISION RULES
	
	;CHECK FOR OTHER SPRITES
	LDA SCREEN.MO_SPRITE.DATA,Y					;DOES DESTINATION TILE HAVE A MOB MO?	
	CMP #$FF
	BNE .MOVE_BLOCKED

	;CHECK FOR GENERAL MAP OBJECTS
	LDX SCREEN.MO_GENERAL.DATA,Y	;load general map object data for current tile location
	CPX #$FF						;is a general map object present?
	BEQ .SPRITE.WALKING.TILE.TESTS	;if no, then continue with other checks

	;TRANSPORT OBJECTS NOT PERMITTED ;**OPT** Speed Memory. Put transport objects in a range. 
	LDA MAP_OBJECTS.GENERAL+$2,X	;load tile type of general map object record
	CMP #TILE_ID.HORSE_C			;move is blocked if a transport object is present
	BEQ .MOVE_BLOCKED
	CMP #TILE_ID.FRIGATE1.1
	BEQ .MOVE_BLOCKED
	CMP #TILE_ID.CARAVEL
	BEQ .MOVE_BLOCKED
	CMP #TILE_ID.WYVERN
	BEQ .MOVE_BLOCKED
	CMP #TILE_ID.SKIFF
	BEQ .MOVE_BLOCKED
	
	; ;CHECK FOR CLOSED DOORS
	; LDA MAP_OBJECTS.GENERAL+$3,X	;load data byte of general map object record
	; CMP #MO.DOOR.CLOSED.GRE			;is there a closed door on the candidate move location?	
	; BCC .MOVE_PERMITTED				;if no, the map object is either an open door or an object that is not an obstacle (i.e. open door, chair). This fact overrides all other rules (i.e. a chair can only be on traversable tiles)
	; CMP #MO.DOOR.CLOSED.LT			;is there a closed door on the candidate move location?	
	; BCS .MOVE_PERMITTED				;if no, the map object is either an open door or an object that is not an obstacle (i.e. open door, chair). This fact overrides all other rules (i.e. a chair can only be on traversable tiles)	
	; ;**FALLS THROUGH**				;if yes, then door is an obstacle, move is blocked. 	

	;CHECK FOR LOCKED DOORS
	LDA MAP_OBJECTS.GENERAL+$3,X	;load data byte of general map object record
	CMP #MO.DOOR.LOCKED.GRE			;is there a locked door on the candidate move location?	
	BCC .MOVE_PERMITTED				;if no, the map object is either an unlocked closed door, an open door or an non-door object that is not an obstacle (i.e. open door, chair). This fact overrides all other rules (i.e. a chair can only be on traversable tiles)
	CMP #MO.DOOR.LOCKED.LT			;is there a locked door on the candidate move location?	
	BCS .MOVE_PERMITTED				;if no, the map object is either an unlocked closed door, an open door or an non-door object that is not an obstacle (i.e. open door, chair). This fact overrides all other rules (i.e. a chair can only be on traversable tiles)
	;**FALLS THROUGH**

	
.MOVE_BLOCKED_STEP
	JMP .MOVE_BLOCKED
	
.SPRITE.WALKING.TILE.TESTS	

.SPRITE.WALKING.BUILDING ;building map only
	;is player in building? 
	LDA PLAYER.MAP.LOCATION_TYPE
	; CMP #MAP.TYPE.TOWN_VILLAGE			;IS LOCATION TYPE = BUILDING?
	; BNE .SPRITE.WALKING.BUILDING.DONE		;if no, skip rules in this section

	;is a building map active
	;**OPT** Memory. Speed. Right now there is only one code for location type, which is used to determine the tile_set to use and for other location-type specific features. There are a lot of the later. It might be a savings if the 
					;there were a tile-set code separate from location type code, and there was a default tile-set picked using the location-type code if tile-set code was set to default. This might require adding another byte to some hex tables
					;but I suspect it would pay off. 
	CMP #MAP.TYPE.BUILDING.GRE 	;is map type = building?
	BCC .SPRITE.WALKING.BUILDING.DONE		;if no, skip rules in this section
	CMP #MAP.TYPE.BUILDING.LT	;is map type = building?
	BCS .SPRITE.WALKING.BUILDING.DONE		;if no, skip rules in this section
	;**FALLS THROUGH						;if yes
	
	LDA SCREEN.TILE.DATA,Y						;LOAD TILE TYPE OF DESTINATION TILE
	
	;obstacle test #2
	CMP #COLLISION_FLAG.MOB_LAND.GRE1.1
	BCC .OBSTACLE_2.TEST.DONE
	CMP #COLLISION_FLAG.MOB_LAND.LT1.1	
	BCC .MOVE_BLOCKED
.OBSTACLE_2.TEST.DONE
.SPRITE.WALKING.BUILDING.DONE

.SPRITE.WALKING.ALL ;all map types
	LDA SCREEN.TILE.DATA,Y						;LOAD TILE TYPE OF DESTINATION TILE

	;obstacle test #1
	CMP #COLLISION_FLAG.MOB_LAND.LT1			;ABSOLUTE OBSTACLE?
	BCC .MOVE_BLOCKED
	
	CMP #COLLISION_FLAG.MOB_LAND.EQ1			;QUICKSAND?
	BEQ .MOVE_BLOCKED
	
	CMP #COLLISION_FLAG.MOB_LAND.GRE			;WATER?	
	BCS .WATER_TEST1.PASS
	JMP .NEXT_COLLISION_TEST
.WATER_TEST1.PASS
	CMP #COLLISION_FLAG.MOB_LAND.LT2			;WATER?	
	BCC .MOVE_BLOCKED
	
.NEXT_COLLISION_TEST	
;NO MORE TESTS FOR NOW
	;**FALLS THROUGH
	
	;JMP .MOVE_PERMITTED
@MIDDLE


.MOVE_PERMITTED
	LDX SAVED.XREG.LOCAL1			;restore loop counter

	LDA #$00		;PERMITTS MOVE BY RECORDING $00 
	STA MOB.MOVES.BLOCKED,X
	;**OPT** Memory. Speed. Moves are permitted by default ($00 is init to the MOB.MOVES.BLOCKED array at start of MOB.MOVEMENT.START). I think I can commment out the above two lines in this section and the other Collision check sections. 
	JMP .COLLISION_EXIT_TEST

.SPECIAL.COLLISION.FLAG
;Note: a special colission flag specifies the type types permitted instead of the standard rules which specify the tile tiles not permitted
	
	LDA MOB.FLAG6
	CMP #$01
	BEQ .SPECIAL_FLAG.COASTAL
	;ADD MORE CHECKS FOR FUTURE FLAGS
	
.SPECIAL_FLAG.COASTAL

	LDA SCREEN.TILE.DATA,Y						;LOAD TILE TYPE OF DESTINATION TILE
	CMP #COLLISION_FLAG.MOB_CROC.EQ2			;IS TILE SHALLOW WATER?
	BEQ .MOVE_PERMITTED							;IF YES, PERMIT MOVE IN THIS DIRECTION
	CMP #COLLISION_FLAG.MOB_CROC.EQ1			;IS TILE BEACH
	BEQ .MOVE_PERMITTED							;IF YES, PERMIT MOVE IN THIS DIRECTION
	CMP #COLLISION_FLAG.MOB_CROC.GRE1			;IS TILE_ID LESS THAN THE START OF LAND-WATER TILE RANGE?
	BCC .MOVE_BLOCKED							;IF YES, BLOCK MOVE
	CMP	#COLLISION_FLAG.MOB_CROC.LT1			;FROM THE LAST TEST WE KNOW THE TILE IS >= THE START OF THE LAND-TILE RANGE. IS TILE_ID LESS THAN THE END OF LAND-WATER TILE RANGE?
	BCC	.MOVE_PERMITTED							;IF YES, PREMITT MOVE
	;***FALLS THROUGH****

.MOVE_BLOCKED
	LDX SAVED.XREG.LOCAL1			;restore loop counter

	LDA #$01		;BLOCKS MOVE BY RECORDING $01
	STA MOB.MOVES.BLOCKED,X
	JMP .COLLISION_EXIT_TEST

.MOVE_BLOCKED.SCREEN_EDGE
	LDX SAVED.XREG.LOCAL1			;restore loop counter

	LDA #$02		;BLOCKS MOVE BY RECORDING $02. I think the blocked value due to the direction being offscreen is different than for an obstacle block becuase of something to do with NPCs, but I don't recall for sure. 
	STA MOB.MOVES.BLOCKED,X
	;**FALLS THROUGH**

.COLLISION_EXIT_TEST
	CPX #$03
	BEQ .COLLISION_TESTS_COMPLETE_STEP
	INX
	JMP .LOOP.MOB_LAND

.COLLISION.LOOP.MOB_SEA	
	;***<INSERT RULES FOR SEA MOBS>***
	;COPY THE SECTION ABOVE FOR LAND MOBS AS A STARTING POINT
	
	;****NOTE: make sure to check for player location using regular constant and also the constants for the MT frigate if frigate is active as transport

.COLLISION_TESTS_COMPLETE_STEP
		
	JMP COLLISION_TESTS_COMPLETE

	
@END
	
COLLISION_CHECK_ONSCREEN_MTT ;
@START
;Collision check for 4-tile MOBs


;STORE TILE #S ADJACENT TO MOB IN AN ARRAY SO WE CAN USE A LOOP TO APPLY COLLISION RULES
;
;The following diagram illustrates the MT MOB's Tiles ($0-$3) (stored in MOB.MT.TILE_LOCATIONS) and the adjacent tiles to be 
;checked for collision ($0-$7). If any tile in the directional group (i.e. $0-$1 is the north group)
;then the direction is marked as blocked in MOB.BLOCKED.MOVES	
;	  01
;    6014
;	 7235
;	  23
;
;The following routine uses the upper left tile of the MT MOB as the starting point and
;uses offsets to calculate the screen tile #s of each of the adjacent tiles. 

	LDX #$00
	;SEC	;set carry flag before a series of SBCs below
.LOOP.IDENTIFY_TILES
	;NORTH GROUP
	LDA SPRITE.RECORD+$1			 ;load y-axis
	CMP #MAP_OBJECTS.Y.FIRST_ROW 	;is tile in first row (player relative X/Y)
	BNE .NORTH.NOT.FIRST.ROW	;if no, proceed to set value
	LDA #$FF		;mark tile as offscreen
	STA MOB.MT.ADJACENT_TILES+$0 ;only one tile needs to be marked as off screen since the innerloop below will set a direction is blocked unless both tiles in the directional group are onscreen and passable
	;STA MOB.MT.ADJACENT_TILES+$1
	JMP .SET.EAST.GROUP
.NORTH.NOT.FIRST.ROW
	LDA	MOB.MT.TILE_LOCATIONS+$0	
	SEC ;do an SEC for each SBC in .LOOP.IDENTIFY_TILES because CMPs are doing between the SBCs, which is actually a subtraction operation and could result in an underflow, clearing the carry flag
	SBC #SCREEN.ARRAY.OFFSET
	TAY
	STY MOB.MT.ADJACENT_TILES+$0
	INY
	STY MOB.MT.ADJACENT_TILES+$1
		
.SET.EAST.GROUP
	;EAST GROUP
	LDA SPRITE.RECORD+$0			;load x-axis
	CMP #MAP_OBJECTS.X.LAST_COLUMN  ;is tile in last row (non-combat screens)? (player relative X/Y)
	BEQ .IN.LAST.COLUMN ;if yes, record direction as off screen
	CMP #MAP_OBJECTS.X.COMBAT.LAST_COLUMN-$1 ;is tile in last row (combat screens) (player relative X/Y). -1 is because the GMAP.X/Y of MT mobs (and thus the player relative screen position), is the upper left tile of the MT mob. 
	BNE .NOT.LAST.COLUMN ;if no, then proceed to set value 
						;if yes, verify the S_ENTITY is in combat before recording direction as off screen
	;is S_ENTITY is in combat?
	LDA PLAYER.MAP.LOCATION_TYPE	;load map type of players current location
	CMP #MAP.TYPE.COMBAT	;is the combat map loaded?
	BNE .NOT.LAST.COLUMN 	;if no, S_ENTITY is not in combat. Do not record direction as off-screen.
	;**FALLS THROUGH	;if no, then S_ENITITY must be a combat mob, special (combat) or player character (combat). Record direction as offscreen
	
		
.IN.LAST.COLUMN	
	LDA #$FF
	STA MOB.MT.ADJACENT_TILES+$5 ;only one tile needs to be marked as off screen since the innerloop below will set a direction is blocked unless both tiles in the directional group are onscreen and passable
	JMP .SET.WEST.GROUP

.NOT.LAST.COLUMN
	LDY MOB.MT.TILE_LOCATIONS+$3
	INY
	STY MOB.MT.ADJACENT_TILES+$5
	TYA
	SEC ;do an SEC for each SBC in .LOOP.IDENTIFY_TILES because CMPs are doing between the SBCs, which is actually a subtraction operation and could result in an underflow, clearing the carry flag
	SBC #SCREEN.ARRAY.OFFSET
	TAY
	STY MOB.MT.ADJACENT_TILES+$4
	
.SET.WEST.GROUP
	;WEST GROUP	
	LDA SPRITE.RECORD+$0			 ;load x-axis
	CMP #MAP_OBJECTS.X.FIRST_COLUMN 	;is tile in first column (player relative X/Y)
	BNE .WEST.NOT.FIRST.COLUMN		;if no, proceed to set value
	LDA #$FF	;mark tile as offscreen
	STA MOB.MT.ADJACENT_TILES+$7 ;only one tile needs to be marked as off screen since the innerloop below will set a direction is blocked unless both tiles in the directional group are onscreen and passable
	;STA MOB.MT.ADJACENT_TILES+$6
	JMP .SET.SOUTH.GROUP
.WEST.NOT.FIRST.COLUMN
	LDY	MOB.MT.TILE_LOCATIONS+$2
	DEY 
	STY MOB.MT.ADJACENT_TILES+$7
	TYA
	SEC ;do an SEC for each SBC in .LOOP.IDENTIFY_TILES because CMPs are doing between the SBCs, which is actually a subtraction operation and could result in an underflow, clearing the carry flag
	SBC #SCREEN.ARRAY.OFFSET
	TAY
	STY MOB.MT.ADJACENT_TILES+$6
	
.SET.SOUTH.GROUP
	;SOUTH GROUP					;SOUTH COMES LAST BECAUSE IT CLEARS THE CARRY FLAG. THIS WAY ONLY ONE SEC IS NEEDED ABOVE, SINCE NO UNDERFLOWS ARE EXPECTED
	LDA SPRITE.RECORD+$1			 ;load y-axis
	CMP #MAP_OBJECTS.Y.LAST_ROW-$1 	;is tile in last row (player relative X/Y). -1 is because the GMAP.X/Y of MT mobs (and thus the player relative screen position), is the upper left tile of the MT mob.
	BNE .SOUTH.NOT.LAST.ROW	;if no, proceed to set value
	LDA #$FF		;mark tile as offscreen
	STA MOB.MT.ADJACENT_TILES+$2 ;only one tile needs to be marked as off screen since the innerloop below will set a direction is blocked unless both tiles in the directional group are onscreen and passable
	;STA MOB.MT.ADJACENT_TILES+$1
	JMP .SET.DONE
.SOUTH.NOT.LAST.ROW
	LDA	MOB.MT.TILE_LOCATIONS+$2
	CLC
	ADC #SCREEN.ARRAY.OFFSET
	TAY
	STY MOB.MT.ADJACENT_TILES+$2
	INY
	STY MOB.MT.ADJACENT_TILES+$3
.SET.DONE
	
@MIDDLE
;MOB.MT.ADJACENT_TILES Diagram
;North Group: 0,1
;South Group: 2,3
;East Group : 4,5
;West Group : 6,7	

;SUMMARY OF INDEXES
;SAVED.XREG.LOCAL == MOB RECORD INDEX
;MOB.MT.GROUP_COUNTER == OUTERLOOP INDEX
;Y-REG == INNERLOOP INDEX & MISC USE


;===COLLISION CHECK

	LDX #$00
	STX MOB.MT.GROUP_COUNTER
.OUTERLOOP
	LDX MOB.MT.GROUP_COUNTER
	
	CPX #$00
	BEQ .SET_GROUP_NORTH
	CPX #$01
	BEQ .SET_GROUP_SOUTH
	CPX #$02
	BEQ .SET_GROUP_EAST
	CPX #$03
	BEQ .SET_GROUP_WEST
	JMP .ERROR
	
.SET_GROUP_NORTH
	LDA #MOB.MT.NORTH_GROUP
	STA MOB.MT.ADJACENT_TILES.PTR
	LDA /MOB.MT.NORTH_GROUP
	STA MOB.MT.ADJACENT_TILES.PTR+$1
	
	JMP .GROUP_SET_COMPLETE
	
.SET_GROUP_SOUTH
	LDA #MOB.MT.SOUTH_GROUP
	STA MOB.MT.ADJACENT_TILES.PTR
	LDA /MOB.MT.SOUTH_GROUP
	STA MOB.MT.ADJACENT_TILES.PTR+$1
	JMP .GROUP_SET_COMPLETE	

.SET_GROUP_EAST
	LDA #MOB.MT.EAST_GROUP
	STA MOB.MT.ADJACENT_TILES.PTR
	LDA /MOB.MT.EAST_GROUP
	STA MOB.MT.ADJACENT_TILES.PTR+$1
	
	JMP .GROUP_SET_COMPLETE

.SET_GROUP_WEST
	LDA #MOB.MT.WEST_GROUP
	STA MOB.MT.ADJACENT_TILES.PTR
	LDA /MOB.MT.WEST_GROUP
	STA MOB.MT.ADJACENT_TILES.PTR+$1
	
	JMP .GROUP_SET_COMPLETE
.GROUP_SET_COMPLETE	

.CHECK.SPRITE.TYPE
	LDA SPRITE.RECORD+$2
	CMP #COLLISION_FLAG.MOB_SEA.START			;FIRST TILE IN SEA MOB RANGE. 
	BCS .COLLISION.LOOP.MOB_SEA
	;;**FALLS THROUGH**

			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; LDA #$AA
			; LDX #MOB.MT.ADJACENT_TILES
			; LDY /MOB.MT.ADJACENT_TILES
			; JSR FULL.BRK
			; BRK
; .TEMP
			; LDA TEMP
			
@MIDDLE
	LDY #$00
.LOOP.DIRECTION_GROUP
	LDA (MOB.MT.ADJACENT_TILES.PTR),Y			;LOAD NEXT ADJACENT TILE#
	TAX
	
;IS DIRECTION OFF SCREEN?
	CPX #$FF
	BEQ .MOVE_BLOCKED.SCREEN_EDGE

.PLAYER.ADJACENT.CHECK	
;IS PLAYER ADJACENT? IF SO, ATTACK

	;is combat active?
	LDA PLAYER.MAP.LOCATION_TYPE	;load map type of players current location
	CMP #MAP.TYPE.COMBAT
	BEQ .PLAYER.ADJACENT.CHECK.COMPLETE ;if yes, branch

	CPX #$5D
	BNE .PLAYER.ADJACENT.CHECK.COMPLETE
	;LDX SAVED.XREG.LOCAL
	JMP SPRITE.INITIATED.COMBAT
.PLAYER.ADJACENT.CHECK.COMPLETE
	
.OVERRIDE_TEST
;IS MOB COLLISION OVERRIDE ON?
	LDA MOB.COLLISION_OVERRIDE
	CMP #$01
	BEQ .COLLISION_TESTS_COMPLETE_STEP			;THE PERMIT CODE IS INIT TO MOB.MOVES.BLOCKED BY DEFAULT, SO NO NEED TO WRITE IT HERE.
	
		
;APPLY MOB WALKING COLLISION RULES		
	LDA SCREEN.MO_SPRITE.DATA,X					;DOES DESTINATION TILE HAVE A MOB MO?	
	CMP #$FF
	BNE .MOVE_BLOCKED

	LDA SCREEN.MO_GENERAL.DATA,X				;DOES DESTINATION TILE HAVE A TRANSPORT MO?	
	CMP #$FF
	BNE .MOVE_BLOCKED						
	

.SPRITE.WALKING.BUILDING ;building map only
	;is player in building? 
	LDA PLAYER.MAP.LOCATION_TYPE
	; CMP #MAP.TYPE.TOWN_VILLAGE			;IS LOCATION TYPE = BUILDING?
	; BNE .SPRITE.WALKING.BUILDING.DONE		;if no, skip rules in this section

	;is a building map active
	;**OPT** Memory. Speed. Right now there is only one code for location type, which is used to determine the tile_set to use and for other location-type specific features. There are a lot of the later. It might be a savings if the 
					;there were a tile-set code separate from location type code, and there was a default tile-set picked using the location-type code if tile-set code was set to default. This might require adding another byte to some hex tables
					;but I suspect it would pay off. 
	CMP #MAP.TYPE.BUILDING.GRE 	;is map type = building?
	BCC .SPRITE.WALKING.BUILDING.DONE		;if no, skip rules in this section
	CMP #MAP.TYPE.BUILDING.LT	;is map type = building?
	BCS .SPRITE.WALKING.BUILDING.DONE		;if no, skip rules in this section
	;**FALLS THROUGH						;if yes

	LDA SCREEN.TILE.DATA,X						;LOAD TILE TYPE OF DESTINATION TILE
	
	;obstacle test #2
	CMP #COLLISION_FLAG.MOB_LAND.GRE1.1
	BCC .OBSTACLE_2.TEST.DONE
	CMP #COLLISION_FLAG.MOB_LAND.LT1.1	
	BCC .MOVE_BLOCKED
.OBSTACLE_2.TEST.DONE
.SPRITE.WALKING.BUILDING.DONE

.SPRITE.WALKING.ALL ;all map types	
	LDA SCREEN.TILE.DATA,X						;LOAD TILE TYPE OF DESTINATION TILE

	;obstacle test #1
	CMP #COLLISION_FLAG.MOB_LAND.LT1			;ABSOLUTE OBSTACLE?
	BCC .MOVE_BLOCKED

	CMP #COLLISION_FLAG.MOB_LAND.EQ1			;QUICKSAND?
	BEQ .MOVE_BLOCKED
	
	CMP #COLLISION_FLAG.MOB_LAND.GRE			;WATER?	
	BCS .WATER_TEST1.PASS
	JMP .NEXT_COLLISION_TEST
.WATER_TEST1.PASS
	CMP #COLLISION_FLAG.MOB_LAND.LT2			;WATER?	
	BCC .MOVE_BLOCKED
	
.NEXT_COLLISION_TEST	
;NO MORE TESTS FOR NOW
	JMP .COLLISION_EXIT_TEST

@MIDDLE
	
.COLLISION.LOOP.MOB_SEA	
	;***<INSERT RULES FOR SEA MOBS>***
	;COPY THE SECTION ABOVE FOR LAND MOBS AS A STARTING POINT

	;****NOTE: make sure to check for player location using regular constant and also the constants for the MT frigate if frigate is active as transport

			;Temporary rules, permitting all moves
			; LDA #$00
			; STA MOB.MOVES.BLOCKED+$0
			; STA MOB.MOVES.BLOCKED+$1
			; STA MOB.MOVES.BLOCKED+$2
			; LDA #$00
			; STA MOB.MOVES.BLOCKED+$3
			
			JMP COLLISION_TESTS_COMPLETE
		
		
.MOVE_BLOCKED
	LDX MOB.MT.GROUP_COUNTER		;restore outer loop counter / MOB.MOVES.BLOCKED index
	LDA #$01		;BLOCKS MOVE BY RECORDING $01
	STA MOB.MOVES.BLOCKED,X	
	LDY #$01							;FORCE TERMINATION OF INNER LOOP. ONLY ONE BLOCKED TILE PER DIRECTIONAL GROUP IS REQUIRED TO MARK THAT DIRECTION IS BLOCKED. 
	JMP .COLLISION_EXIT_TEST
	
.MOVE_BLOCKED.SCREEN_EDGE
	LDX MOB.MT.GROUP_COUNTER		;restore outer loop counter / MOB.MOVES.BLOCKED index

	LDA #$02		;BLOCKS MOVE BY RECORDING $02. I think the blocked value due to the direction being offscreen is different than for an obstacle block becuase of something to do with NPCs, but I don't recall for sure. 
	STA MOB.MOVES.BLOCKED,X
	LDY #$01		;FORCE TERMINATION OF INNER LOOP. ONLY ONE BLOCKED TILE PER DIRECTIONAL GROUP IS REQUIRED TO MARK THAT DIRECTION IS BLOCKED. 
	;**FALLS THROUGH**
	
.COLLISION_EXIT_TEST
	CPY #$01
	BEQ .INNERLOOP_COMPLETE
	INY
	JMP .LOOP.DIRECTION_GROUP

.INNERLOOP_COMPLETE	
	LDX MOB.MT.GROUP_COUNTER
	CPX #$03
	BEQ .COLLISION_TESTS_COMPLETE_STEP
	INX
	STX MOB.MT.GROUP_COUNTER
	JMP .OUTERLOOP


.COLLISION_TESTS_COMPLETE_STEP


			
	JMP COLLISION_TESTS_COMPLETE
.ERROR
;COLLISION_CHECK_ONSCREEN_MTT, .OUTERLOOP Reports unexpected value (!=0,4,8,$C) in MOB.MT.GROUP_COUNTER
	BRK
@END
 	
COLLISION_CHECK_OFFSCREEN
@START
;===COLLISION CHECK FOR MOBS WITH SS FLAG SET, WHICH ARE LOCATED OFF THE VIEW SCREEN====

;STORE TILE #S ADJACENT TO MOB IN AN ARRAY SO WE CAN USE A LOOP TO APPLY COLLISION RULES

	;STX SAVED.XREG.LOCAL						;COLLISION_CHECK_ONSCREEN USE AN X INDEX, SO IT SAVES X-REG, RESULTING IN A RESTORE X-REG UP AHEAD. SO THIS CODE NEEDS TO SAVE X-REG TO SO THAT THE CORRECT VALUE IS LOADED IN THE RESTORE. 
;SAVE TILE # NORTH OF MOB
;======================================	
;MAP_OBJECTS.MAP_LOCATION(2)- #OFFSET.UP(1)



				
	CLD 
    SEC                           ;ALWAYS BEFORE SUBTRACTION
    LDA MAP_OBJECTS.MAP_LOCATION
    SBC #OFFSET.UP
    STA RMAP.LOOKUP	
    LDA MAP_OBJECTS.MAP_LOCATION+$1
    SBC #$00
    STA RMAP.LOOKUP+$1	
	
;======================================

;LOOKUP TILE_TYPE
;======================================		
;RZONE.ARRAY(2) + RMAP.LOOKUP(2)
	
	CLC                          			;ALWAYS BEFORE ADD
	LDA #RZONE.ARRAY						;OP1
	ADC RMAP.LOOKUP							;OP2  
	STA RZONE.ARRAY.INDEX_ROW
				 
	LDA /RZONE.ARRAY						;OP1+$1
	ADC RMAP.LOOKUP+$1						;OP2+$2 carry flag not cleared via CLC intentionally, it's part of 16-bit adding. 
	STA RZONE.ARRAY.INDEX_ROW+$1
;======================================	

	LDY #$00
	LDA (RZONE.ARRAY.INDEX_ROW),Y	;LOOKUP TILE_TYPE FROM REGIONAL MAP ARRAY
	STA MOB.ADJACENT_TILES		 	;BORROWING SCREEN.TILE.HOPPER TO STORE THE TILE TYPE ADJACENT TO THE MOB
				
;
;SAVE TILE # WEST OF MOB
;======================================	
;MAP_OBJECTS.MAP_LOCATION(2) - $01 (ALWAYS THE OFFSET TO THE LEFT)
	
	CLD 
    SEC                         	  ;ALWAYS BEFORE SUBTRACTION
    LDA MAP_OBJECTS.MAP_LOCATION
    SBC #$01
    STA RMAP.LOOKUP	
    LDA MAP_OBJECTS.MAP_LOCATION+$1
    SBC #$00
    STA RMAP.LOOKUP+$1					;BORROWING SCREEN.TILE.HOPPER TO STORE THE RMAP OF THE TILE #S ADJACENT TO THE MOB
;======================================
;LOOKUP TILE_TYPE
;======================================		
;RZONE.ARRAY(2) + RMAP.LOOKUP(2)
	
	CLC                          			;ALWAYS BEFORE ADD
	LDA #RZONE.ARRAY						;OP1
	ADC RMAP.LOOKUP							;OP2  
	STA RZONE.ARRAY.INDEX_ROW
				 
	LDA /RZONE.ARRAY						;OP1+$1
	ADC RMAP.LOOKUP+$1						;OP2+$2 carry flag not cleared via CLC intentionally, it's part of 16-bit adding. 
	STA RZONE.ARRAY.INDEX_ROW+$1
;======================================	

	LDY #$00
	LDA (RZONE.ARRAY.INDEX_ROW),Y		;LOOKUP TILE_TYPE FROM REGIONAL MAP ARRAY
	STA MOB.ADJACENT_TILES+$3			;BORROWING SCREEN.TILE.HOPPER TO STORE THE TILE TYPE ADJACENT TO THE MOB	

;
;				
;SAVE TILE # EAST OF MOB
;======================================		
;MAP_OBJECTS.MAP_LOCATION(2)+ $01 (ALWAYS THE OFFSET TO THE RIGHT)


; DO THE MATH
	CLD 								
    CLC                       			 ;ALWAYS BEFORE ADD
    LDA MAP_OBJECTS.MAP_LOCATION							
    ADC #$01
    STA RMAP.LOOKUP
		 
    LDA MAP_OBJECTS.MAP_LOCATION+$1
    ADC #$00							;carry flag not cleared via CLC intentionally, it's part of 16-bit adding. 
    STA RMAP.LOOKUP+$1			
	
;======================================


;LOOKUP TILE_TYPE
;======================================		
;RZONE.ARRAY(2) + RMAP.LOOKUP(2)
	
	CLC                          			;ALWAYS BEFORE ADD
	LDA #RZONE.ARRAY						;OP1
	ADC RMAP.LOOKUP							;OP2  
	STA RZONE.ARRAY.INDEX_ROW
				 
	LDA /RZONE.ARRAY						;OP1+$1
	ADC RMAP.LOOKUP+$1						;OP2+$2 carry flag not cleared via CLC intentionally, it's part of 16-bit adding. 
	STA RZONE.ARRAY.INDEX_ROW+$1
;======================================	

	LDY #$00
	LDA (RZONE.ARRAY.INDEX_ROW),Y		;LOOKUP TILE_TYPE FROM REGIONAL MAP ARRAY
	STA MOB.ADJACENT_TILES+$2			;BORROWING SCREEN.TILE.HOPPER TO STORE THE TILE TYPE ADJACENT TO THE MOB		
				
;
;	
;SAVE TILE # SOUTH OF MOB
;======================================		
;MAP_OBJECTS.MAP_LOCATION(2)+ #OFFSET.DOWN(1)


; DO THE MATH
	CLD 								
    CLC                       			 ;ALWAYS BEFORE ADD
    LDA MAP_OBJECTS.MAP_LOCATION							
    ADC #OFFSET.DOWN
    STA RMAP.LOOKUP
		 
    LDA MAP_OBJECTS.MAP_LOCATION+$1
    ADC #$00							;carry flag not cleared via CLC intentionally, it's part of 16-bit adding. 
    STA RMAP.LOOKUP+$1					;BORROWING SCREEN.TILE.HOPPER TO STORE THE TILE #S ADJACENT TO THE MOB
	
;======================================

;LOOKUP TILE_TYPE
;======================================		
;RZONE.ARRAY(2) + RMAP.LOOKUP(2)
	
	CLC                          			;ALWAYS BEFORE ADD
	LDA #RZONE.ARRAY						;OP1
	ADC RMAP.LOOKUP							;OP2  
	STA RZONE.ARRAY.INDEX_ROW
				 
	LDA /RZONE.ARRAY						;OP1+$1
	ADC RMAP.LOOKUP+$1						;OP2+$2 carry flag not cleared via CLC intentionally, it's part of 16-bit adding. 
	STA RZONE.ARRAY.INDEX_ROW+$1
;======================================	

	LDY #$00
	LDA (RZONE.ARRAY.INDEX_ROW),Y			;LOOKUP TILE_TYPE FROM REGIONAL MAP ARRAY
	STA MOB.ADJACENT_TILES+$1				;BORROWING SCREEN.TILE.HOPPER TO STORE THE TILE TYPE ADJACENT TO THE MOB	

@MIDDLE

;MOB GROUP: DETERMINE WHETHER TO APPLY LAND OR SEA MOB COLLISION RULES	
	LDA SPRITE.RECORD+$2
	CMP #COLLISION_FLAG.MOB_SEA.START			;FIRST TILE IN SEA MOB RANGE. 
	BCS .COLLISION.LOOP.MOB_SEA


;APPLY MOB WALKING COLLISION RULES		
	LDY #$00
.LOOP.MOB_LAND
;IS MOB COLLISION OVERRIDE ON?
	LDA MOB.COLLISION_OVERRIDE
	CMP #$01
	BEQ .MOVE_PERMITTED

;BEGIN MAIN TESTS
.SPRITE.WALKING.BUILDING ;building map only
	;is player in building? 
	LDA PLAYER.MAP.LOCATION_TYPE
	; CMP #MAP.TYPE.TOWN_VILLAGE			;IS LOCATION TYPE = BUILDING?
	; BNE .SPRITE.WALKING.BUILDING.DONE		;if no, skip rules in this section

	;is a building map active
	;**OPT** Memory. Speed. Right now there is only one code for location type, which is used to determine the tile_set to use and for other location-type specific features. There are a lot of the later. It might be a savings if the 
					;there were a tile-set code separate from location type code, and there was a default tile-set picked using the location-type code if tile-set code was set to default. This might require adding another byte to some hex tables
					;but I suspect it would pay off. 
	CMP #MAP.TYPE.BUILDING.GRE 	;is map type = building?
	BCC .SPRITE.WALKING.BUILDING.DONE		;if no, skip rules in this section
	CMP #MAP.TYPE.BUILDING.LT	;is map type = building?
	BCS .SPRITE.WALKING.BUILDING.DONE		;if no, skip rules in this section
	;**FALLS THROUGH						;if yes

	LDA MOB.ADJACENT_TILES,Y					;LOAD NEXT ADJACENT TILE TYPE
	
	;obstacle test #2
	CMP #COLLISION_FLAG.MOB_LAND.GRE1.1
	BCC .OBSTACLE_2.TEST.DONE
	CMP #COLLISION_FLAG.MOB_LAND.LT1.1	
	BCC .MOVE_BLOCKED
.OBSTACLE_2.TEST.DONE
.SPRITE.WALKING.BUILDING.DONE

.SPRITE.WALKING.ALL ;all map types	
	LDA MOB.ADJACENT_TILES,Y					;LOAD NEXT ADJACENT TILE TYPE

	;obstacle test #1
	CMP #COLLISION_FLAG.MOB_LAND.LT1			;ABSOLUTE OBSTACLE?
	BCC .MOVE_BLOCKED

	CMP #COLLISION_FLAG.MOB_LAND.EQ1			;QUICKSAND?
	BEQ .MOVE_BLOCKED
	
	CMP #COLLISION_FLAG.MOB_LAND.GRE			;WATER?	
	BCS .WATER_TEST1.PASS
	JMP .NEXT_COLLISION_TEST
.WATER_TEST1.PASS
	CMP #COLLISION_FLAG.MOB_LAND.LT2			;WATER?	
	BCC .MOVE_BLOCKED
	
.NEXT_COLLISION_TEST	
;NO MORE TESTS FOR NOW
;	JMP .MOVE_PERMITTED

@MIDDLE
	
.MOVE_PERMITTED
	LDA #$00		;PERMITTS MOVE BY RECORDING $00 
	STA MOB.MOVES.BLOCKED,Y
	JMP .COLLISION_EXIT_TEST
	
.MOVE_BLOCKED
	LDA #$01		;BLOCKS MOVE BY RECORDING $01
	STA MOB.MOVES.BLOCKED,Y

.COLLISION_EXIT_TEST
		LDA #$00
		STA TEMP16
	CPY #$03
	BEQ COLLISION_TESTS_COMPLETE
	INY
	JMP .LOOP.MOB_LAND
	
.COLLISION.LOOP.MOB_SEA	
	;***<INSERT RULES FOR SEA MOBS>***
	;COPY THE SECTION ABOVE FOR LAND MOBS AS A STARTING POINT
 
	;*****FALLS THROUGH

COLLISION_TESTS_COMPLETE

@END
@END

;MOVE SELECTION
@START

COMBAT.MOVE.MANAGER.ENTRANCE
@START

; ;VALIDATE ENTRY
	; LDA PLAYER.MAP.LOCATION_TYPE		;load location type code
	; CMP #MAP.TYPE.COMBAT			;is location type = building?
	; BNE .EXIT
	
	; JSR COMBAT.MOVE_MANAGER

; .EXIT


			
@END

COMBAT.MOVE_MANAGER ;**OPT** Memory. Move this routine to the combat module. Should work just fine as long as we do a check to verify combat is loaded before the JSR to transfer control. 
@START
;=====================SUBROUTINE DOCUMENTATION====================================
;
;Mobs/specials mostly share the same routine for making attack/cast/move decisions. 

;-Combat rules
;*Mobs/specials with melee weapons chase the closest target until in melee range and then they initate a melee attack once adjacent. 
;*Mobs/specials with range weapons and spells attack the target farthest away.
;
;
;=================================================================================


;IS ENTRANCE PERMITTED TO THIS ROUTINE?
	; ;is combat active
	; LDA PLAYER.MAP.LOCATION_TYPE	;load map type of players current location
	; CMP #MAP.TYPE.COMBAT			;is player on combat map?
	; BNE COMBAT.MOVE_MANAGER.COMPLETE	;if no, then exit to next routine

;START COMBAT.MOVE_MANAGER	
	LDA SPRITE.RECORD+$A	;load the S_ENTITY type
	CMP #S_ENTITY_TYPE.PC	;is S_ENTITY a player character?
	BEQ .PLAYER.CHARACTERS_STEP  ;if yes, branch to the routine for player characters
	CMP #S_ENTITY_TYPE.C_MOB	;is S_ENTITY a combat MOB?
	BEQ .MOB			  	;if yes, branch to the routine for MOBs
	CMP #S_ENTITY_TYPE.SPECIAL	;is S_ENTITY a Special?
	BEQ .SPECIAL		  	;if yes, branch to the routine for MOBs
	JMP .COMBAT.MOVE_MANAGER.COMPLETE ;no other S_ENTITY types are permitted to enter this routine
	;JMP .ERROR.UNEXPECTED.S_ENTITY.TYPE

.PLAYER.CHARACTERS_STEP
	JMP .PLAYER.CHARACTERS
	
.SPECIAL ;front end (merges into mob routine)
@START				
	LDA COMBAT.TURN_STATUS ;load turn status. $00 = PCs turn. $01 = Special(s) S_ENTITY, $02 = MOBs turn
	CMP #COMBAT.TURN_STATUS.SPECIAL ;is it the SPECIALs turn?
	BEQ .IS.SPECIALS.TURN ;if yes, then branch to the subroutine that handles SPECIAL turns.
.S_ENTITY.NOT_ACTIVE_STEP
	JMP .S_ENTITY.NOT_ACTIVE	;if no, this SPECIAL record is not active, no move should be made.	

.IS.SPECIALS.TURN	
	LDA COMBAT.SPECIAL.ACTIVE.RECORD ;is the current map object = the SPECIAL S_ENTITY active? 
	CLC
	ADC #COMBAT.MAP_OBJECTS.SPECIAL.START_RECORD ;adjust the special record # to it's true value. The combat module use COMBAT.MAP_OBJECTS.SPECIAL, which starts at MAP_OBJECTS.NPC+$80. Since this routine (MO.DRAW) uses MAP_OBJECTS.NPC, we have to make this adjustment to make the record # indexes compatible. 
	STA COMBAT.ATTACKER.TRUE_RECORD
	CMP SPRITE.RECORD+$0B		;load map object record # of the current map object
	BNE .S_ENTITY.NOT_ACTIVE_STEP		;if no, then the current SPECIAL should not move. 

;**OPT** Memory. It is possible that the above portion of the special routine could be consolidate with the MOBs if
;COMBAT.ATTACKER.TRUE_RECORD was used to determine if the current special record belongs to the special whose turn it is

	
			JMP .MOB.ACQUIRE.TARGET
			
	
@END
	
.MOB
@START			
	LDA COMBAT.TURN_STATUS ;load turn status. $00 = PCs turn. $01 = Special(s) S_ENTITY, $02 = MOBs turn
	CMP #COMBAT.TURN_STATUS.MOB ;is it the MOBs turn?
	BEQ .IS.MOBS.TURN ;if yes, then branch to the subroutine that handles MOB turns.
	JMP .S_ENTITY.NOT_ACTIVE ;if no, this MOB record is not active, no move should be made.	
	
.IS.MOBS.TURN	
	LDA SPRITE.RECORD+$0B		;load map object record # of the current map object
	CMP COMBAT.MOB.ACTIVE.RECORD ;is the current map object = the player character active? 
	BNE .S_ENTITY.NOT_ACTIVE_STEP	;if no, then current MOB should not move. 
								;if yes, then the movement direction code should have been placed in byte $03 of the map object record by the calling routine
	;**FALLS THROUGH**
	
.MOB.NON_SPECIAL.INITS
;(INITs that must be seperate for MOBs and SPECIALs)

	;required due to MOBs and SPECIALs both sharing the same map objects array
	LDA COMBAT.ATTACKER.RECORD
	STA COMBAT.ATTACKER.TRUE_RECORD

	;**FALLS THROUGH**
	
.MOB.ACQUIRE.TARGET
;ACQUIRE TARGET & CALCULATE PRELIM POSITION DATA

	JSR COMBAT.ACQUIRE.TARGET
		;RETURN: COMBAT.CHASE.TARGET.X, COMBAT.CHASE.TARGET.Y, COMBAT.ACQUIRE.TARGET.FINAL.SINDEX (sindex), COMBAT.CHASE.TARGET.DISTANCE			
		;RETURN: MOB.POSITION.X_LT*, MOB.POSITION.X_GR*, MOB.POSITION.Y_LT*, MOB.POSITION.Y_GR*
			;*X_LT/X_GR = $00, then X-axis is equal. If either are set to $01 then true (i.e. if X_LT = $01, then X-AXIS is less than. if X_GR = $01, then X-AXIS is greater than) 
			;*Y_LT/Y_GR = $00, then Y-axis is equal. If either are set to $01 then true (i.e. if Y_LT = $01, then Y-AXIS is less than) 

						
	;**FALLS THROUGH**
			
.MOB.INIT_ATTACK ;melee/range/spell attacks are all one routine for MOBs
@START

;CALIBERATE ATTACKER/DEFENDER
;(assumes closest target will be used)
	LDY MAP_OBJECTS.TILE_LOCATION ;load screen tile location of the attacker
	STY COMBAT.ATTACKER.SINDEX ;set here for convenience, parameter of COMBAT.UPDATE.TEXT_WINDOW

	LDA COMBAT.ACQUIRE.TARGET.FINAL.SINDEX ;sindex of defender   ;**OPT** Memory. whereever it is set, can this variable be replaced with COMBAT.DEFENDER.SINDEX 
	STA COMBAT.DEFENDER.SINDEX


			; LDA #$01
			; STA TROUBLESHOOTING.HOOK
			
	;read attacker character sheet
		;YREG = COMBAT.ATTACKER.SINDEX
		LDA #$00 ;set read mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet
		;RETURN: CHR_SHEET.RECORD.READ($80)


			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP

			
; ;;SPECIAL CHARACTER SHEET READ
	; ; ;read SPECIAL character sheet data
		; ; LDA #$11 ;SPECIAL # + $10
		; ; ;ACC = player sequential # (high-bit not set = read mode)
	; ; JSR COMBAT.READ_WRITE.CHR_SHEET.MOB
		; ; ;RETURN VALUE = CHR_SHEET.RECORD.READ
		
			; ;TROUBLESHOOTING LOOP
			; LDX #$00
; .TEST.LOOP			
			; LDA CHR_SHEET.RECORD.READ,X
			; STA $BE00,X
			; INX
			; BNE .TEST.LOOP
			
			; LDA #$AA
			; ;LDX TEMP
			
			; ; LDA MULPLR
			; ; LDX RESULT+$0
			; ; LDY RESULT+$1
			; JSR FULL.BRK
			; BRK

; .TEMP
			; LDA TEMP
			
	;does attacker have a valid target?
	LDA COMBAT.ACQUIRE.TARGET.FINAL.SINDEX
	CMP #$FF ;was a target found?
	BEQ .MOB.MOVES ;branch if no target found
	; Bne .cow
	; jmp .MOB.MOVES ;branch if no target found

; .cow

	;set attacker x/y
	;(only needed for range attacks. used in COMBAT.ATTACK.RANGE.DISPLAY)
	LDX COMBAT.ATTACKER.TRUE_RECORD
	LDA COMBAT.MAP_OBJECTS.MOB+$0,X ;load active player GMAP.X
	STA FP.SOURCE.GMAP.X
	LDA COMBAT.MAP_OBJECTS.MOB+$1,X ;load active player GMAP.Y
	STA FP.SOURCE.GMAP.Y

				
;SET WEAPON VARIABLES

	;get shape_type
	LDA CHR_SHEET.MOB.INT.WP_SHAPE_TYPE	 ;load weapon shape_type
	BMI .NOT_ANGLED_SHAPE ;branch if high-bit is set (angled shape)
;.ANGLED_SHAPE
	LDA #$00
	JMP .SET.SHAPE_TYPE
	
.NOT_ANGLED_SHAPE 
	LDA #$02
	;**FALLS THROUGH**
.SET.SHAPE_TYPE
	STA COMBAT.ATTACK_COMMAND.WP_SHAPE_TYPE	

	;get shape_ID
	LDA CHR_SHEET.MOB.ENGAGED.WP_SHAPE_ID.SPELL_CODE ;load weapon shape_ID
	;left hand value is in HO nibble
	LSR ;move HO nibble to LO nibble
	LSR
	LSR
	LSR
	STA COMBAT.ATTACK_COMMAND.SHAPE_ID

	;get spell profile
	LDA CHR_SHEET.MOB.ENGAGED.WP_SHAPE_ID.SPELL_CODE ;load spell profile
	;spell profile value is in LO nibble
	AND #$F ;mask-out HO nibble
	;STA COMBAT.ATTACK_COMMAND.MOB_SPELL_PROFILE
	STA COMBAT.SPELL_CODE.CAST

@END
	;**FALLS THROUGH**

.MOB.DETERMINE.ATTACK_TYPE
@START
;=====================CODE-SECTION DOCUMENTATION====================================
;if mob has no magic points, use the character sheet info gathered by the routine above
;to determine if the mob has a range or melee weapon. 
;
;If the mob has magic points, a random # determines if the mob casts a spell. 
;If the mob casts a spell, the spell profile field on it's character sheet is fed into
;SPELL_FILE.ENTRANCE as the spell code parameter. 
;
;-Note
;range weapon radius as a concept doesn't apply to mobs. radius is enforced by the
;target selection routine which is only used by PCs.
;
;=================================================================================


	LDA CHR_SHEET.PC_MOB.MP ;load mob magic points
	BEQ .MOB.DETERMINE.MELEE_RANGE
	;does mob cast spell?
	JSR RANDOM.8
		;RETURN: 8-bit random #
		CMP #$80	;50% probability
		BCS .MOB.DETERMINE.MELEE_RANGE
	
	; **FALLS THROUGH**
	
.MOB.CAST_SPELL
	;if mob is not engaged in melee range combat, switch target from closest to furthest 
	JSR .COMBAT.MOB_SPECIAL.EVALUATE.TARGETS
		
;UPDATE.SCROLL_WINDOW.NAME
;(print MOB/SPECIAL name)
	
	JSR PRINT.TEXT.WINDOW.CR ;print carriage return to combat scroll window
	;JSR PRINT.TEXT.WINDOW.CR ;print carriage return to combat scroll window
	JSR COMBAT.PRINT.MOB_NAME

	;print mob-cast text
		LDA #COMBAT.TEXT_BLOCK.MOB_CAST					
		STA TWF.STRING+$0
		
		LDA /COMBAT.TEXT_BLOCK.MOB_CAST
		STA TWF.STRING+$1	
	JSR PRINT.TEXT.WINDOW

			
		;COMBAT.SPELL_CODE.CAST = already set
	JSR COMBAT.CAST.EXECUTE
	
	JMP .ATTACK.FINISH
	
.MOB.DETERMINE.MELEE_RANGE			
	LDA COMBAT.ATTACK_COMMAND.SHAPE_ID
	BEQ .MOB.DETERMINE.ATTACK_TYPE.DONE	; branch if mob has melee weapon

;MOB HAS RANGE WEAPON

	;if mob is not engaged in melee range combat, switch target from closest to furthest 
	JSR .COMBAT.MOB_SPECIAL.EVALUATE.TARGETS
		
	JMP .MOB.ATTACK.MELEE_RANGE
		;*even though melee and range is handled by the same routine, by jumping to it
		;now, it results in the mob attacking regardless of the distance from the target because
		;.EVALUATE.MELEE_ATTACK is skipped which only initiates an attack if the target is adjacent.
		
.MOB.DETERMINE.ATTACK_TYPE.DONE
@END
	;**FALLS THROUGH**

		
.EVALUATE.MELEE_ATTACK
;(determine whether the mob is in range for a melee attack. If not, then the mob will move)

	JSR COMBAT.MELEE.IN_RANGE.CHECK
		;RETURN VALUE: ACC = ($00 = in melee range, $01 = not in melee range)
	CMP #$00
	BEQ .MOB.ATTACK.MELEE_RANGE ;if yes, branch	

	;**FALLS THROUGH**
	
.MOB.MOVES	
	;clear the engaged field
	
		;**warning: attacker character sheet will be loaded on exit
		LDY COMBAT.ATTACKER.SINDEX ;**OPT** Memory. I think Y-REG is already set to this value. 
		LDA #COMBAT.STATS.CRTL_HIT.NOT_ENGAGED_VALUE ;set value to update.
	JSR COMBAT.STATS.UPDATE.ENGAGED
		
	;**FALLS THROUGH**
		
	JMP S_ENTITY.MAGNET_ALGORITHM

.MOB.ATTACK.MELEE_RANGE
;note: range weapon radius as a concept doesn't apply to mobs. radius is enforced by the
;target selection routine which is only used by PCs.



			
;UPDATE.SCROLL_WINDOW.NAME
;(print MOB/SPECIAL name)
	JSR PRINT.TEXT.WINDOW.CR ;print carriage return to combat scroll window
	;JSR PRINT.TEXT.WINDOW.CR ;print carriage return to combat scroll window
	JSR COMBAT.PRINT.MOB_NAME

	;print mob-attack text
		LDA #COMBAT.TEXT_BLOCK.MOB_ATTACK					
		STA TWF.STRING+$0
		
		LDA /COMBAT.TEXT_BLOCK.MOB_ATTACK
		STA TWF.STRING+$1	
	JSR PRINT.TEXT.WINDOW
	
	
	;set damage type 
	LDA #$00 ;melee (default, range weapons are detected by COMBAT.ATTACK.RANGE_MELEE)
	STA COMBAT.STATS.DAMAGE.TYPE ;($00 = melee | $01 range, $02 spell. High-bit not set = non-magic weapon (doesn't apply to type=spell or MOBs) | high-bit set = magic weapon (doesn't apply to type=spell or MOBs)


	
		; LDA #$01 ; ranged shape
		; STA COMBAT.ATTACK_COMMAND.SHAPE_ID
		; LDA #$02 ;non-angled shape (range weapon)
		; STA COMBAT.ATTACK_COMMAND.WP_SHAPE_TYPE			
	JSR COMBAT.ATTACK.RANGE_MELEE


			
	;**FALLS THROUGH**
	
.ATTACK.FINISH
@START
;.UPDATE.CHR_ROSTER
;(so any changes in PC HP/MP or health status is shown)


	JSR DISPLAY.CHARACTER.ROSTER


			
			
.S_ENTITY.NOT_ACTIVE

	LDA #$04			
	JMP .EXECUTE_MOVE ;force a pass (from the perspective of this subroutine, MO.DRAW)
@END
@END


.PLAYER.CHARACTERS ;PCs
@START
	LDA COMBAT.TURN_STATUS ;load turn status. $00 = PCs turn. $01 = Special(s) S_ENTITY, $02 = MOBs turn
	BEQ .IS.PCS.TURN ;is it the player character's turn?
	JMP .S_ENTITY.NOT_ACTIVE	;if no, this PC record is not active, no move should be made.	
	
.IS.PCS.TURN	
	LDA SPRITE.RECORD+$0B	;load map object record # of the current map object
	CMP COMBAT.PC.ACTIVE.RECORD ;is the current map object = the player character active? 
	BNE .S_ENTITY.NOT_ACTIVE			;if no, then current player character should not move. 
								;if yes, then the movement direction code should have been placed in byte $03 of the map object record by the calling routine

								
.PC.COLLISION.CHECK
;verify the move the player character selected is not blocked by an obstacle.

	LDX SPRITE.RECORD+$03		;load move direction code
	LDA MOB.MOVES.BLOCKED,X 	;is move direction blocked?
	STA COMBAT.PC.BLOCKED_MOVE.FLAG  ;($00 = not blocked | $01 = blocked)	
	BEQ .PC.MOVE.NOT_BLOCKED	;if no, then execute move
;PC MOVE BLOCKED	


	JSR PRINT.TEXT.WINDOW.CR ;print carriage return to combat scroll window
			
	;update scroll window	
		LDA #GLOBAL.TEXT_BLOCK.COMMAND_BLOCKED	
		STA TWF.STRING+$0
		
		LDA /GLOBAL.TEXT_BLOCK.COMMAND_BLOCKED	
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW

	
	JSR PLAY.SOUND.DUMB_ASS
	JMP EXIT				;exit because in combat, MO.DRAW only needs to process the move for 1 S_ENTITY, whoevers turn it is. 

	
.PC.MOVE.NOT_BLOCKED
	TXA ;transfer move direction code to ACC
	
.EXECUTE_MOVE
			
	;ACC = move direction code			
	STA MOB.MOVE.CURRENT	;save move diretion code as the move direction for the current S_ENTITY
	JMP MOB.MOVE.MAKE		;execute the move


; .ERROR.UNEXPECTED.S_ENTITY.TYPE
; ;COMBAT.MOVE_MANAGER reports unexpected S_ENTITY type code in SPRITE.RECORD+$0A	
	; LDA #$AF
	; JSR FULL.BRK
	;$AF = .ERROR.UNEXPECTED.S_ENTITY.TYPE (COMBAT.MOVE_MANAGER)
	
@END


;SUBROUTINES OF COMBAT.MOVE_MANAGER
@START

.COMBAT.MOB_SPECIAL.EVALUATE.TARGETS	
@START

	;is mob engaged in melee combat?
	;(if another S_ENTITY is attacking the mob within melee range then the mob is considered to be engaged in melee combat)
	JSR COMBAT.IS_ATTACKER.ENAGED
		;RETURN VALUE: ;($00 = attacker is NOT engaged in melee combat, >=$01 = attacker IS engaged in melee combat, the value is the SINDEX of the engaged S_ENTITY)
		CMP #$00
		BNE .EXIT ;branch if yes. don't switch targets (default to closest target) 

	;is there a furthest target on file?
	;(there won't be a furthest target if only one target is found. In that case the furthest target variables will remain set to their init values)
	LDA COMBAT.ACQUIRE.FURTHEST_TARGET.FINAL.SINDEX
	CMP #$FF ;init value
	BEQ .EXIT ;branch if furthest target is not on file. In this even the closest target is used because the code below to switch targets is not executed
	
	;range only: swap out closest target for furthest target
	LDA COMBAT.CHASE.FURTHEST_TARGET.DISTANCE
	STA COMBAT.CHASE.TARGET.DISTANCE ;**opt** memory. I don't think COMBAT.CHASE.TARGET.DISTANCE is used after COMBAT.MELEE.IN_RANGE.CHECK is called
	
	LDA COMBAT.FURTHEST_TARGET.X
	STA	COMBAT.CHASE.TARGET.X
	
	LDA COMBAT.FURTHEST_TARGET.Y
	STA COMBAT.CHASE.TARGET.Y
	
	LDA COMBAT.ACQUIRE.FURTHEST_TARGET.FINAL.SINDEX
	STA COMBAT.ACQUIRE.TARGET.FINAL.SINDEX
	STA COMBAT.DEFENDER.SINDEX
		
	;**FALLS THROUGH**
		
.EXIT
		
	RTS
@END

;.PRELIMINARY.POSITION.CALCULATIONS
@START
; ;CALCULATE APROXIMATE RELATIVE SCREEN LOCATION
; ;(Determines whether the S_ENTITY's X and Y A-XIS are greater than or less than that of the MOB's chase target. This information is used by S_ENTITY.MAGNET_ALGORITHM)

; ;PARAMETERS: COMBAT.MOB_SPECIAL.CHASE.TARGET.X, COMBAT.MOB_SPECIAL.CHASE.TARGET.Y, SPRITE.RECORD+$8, SPRITE.RECORD+$9
; ;ENTRANCE: DIRECT
; ;RETURN: MOB.POSITION.X_LT, MOB.POSITION.X_GR, MOB.POSITION.Y_LT, MOB.POSITION.Y_GR


; ;INIT VARIABLES
	; LDA #$00
	; STA MOB.POSITION.X_GR
	; STA MOB.POSITION.X_LT
	; STA MOB.POSITION.Y_GR	
	; STA MOB.POSITION.Y_LT

; .TEST.X_AXIS
	; LDA SPRITE.RECORD+$8 ;load GMAP.X of the current MOB
	; CMP COMBAT.CHASE.TARGET.X
	; BCC .X_LESS
	; BNE .X_GREATER
	; ;X-AXIS EQUAL	
	; JMP .TEST.Y_AXIS ;keep both MOB.POSITION.X_LT and MOB.POSITION.X_GR = $00. X-AXIS being equal will be imputed by S_ENTITY.MAGNET_ALGORITHM	
; .X_LESS	
	; LDA #$01
	; STA MOB.POSITION.X_LT
	; JMP .TEST.Y_AXIS
	
; .X_GREATER
	; LDA #$01
	; STA MOB.POSITION.X_GR
	; ;**FALLS THROUGH**
	
; .TEST.Y_AXIS
	; LDA SPRITE.RECORD+$9 ;load GMAP.Y of the current MOB
	; CMP COMBAT.CHASE.TARGET.Y
	; BCC .Y_LESS
	; BNE .Y_GREATER
	; ;Y-AXIS EQUAL	
	; JMP .AXIS.TESTS.DONE ;keep both MOB.POSITION.Y_LT and MOB.POSITION.Y_GR = $00. Y-AXIS being equal will be imputed by S_ENTITY.MAGNET_ALGORITHM	
; .Y_LESS	
	; LDA #$01
	; STA MOB.POSITION.Y_LT
	; JMP .AXIS.TESTS.DONE
	
; .Y_GREATER
	; LDA #$01
	; STA MOB.POSITION.Y_GR
	; ;**FALLS THROUGH**

; .AXIS.TESTS.DONE
	
	; RTS
@END

@END

.COMBAT.MOVE_MANAGER.COMPLETE
@END

		
NPC.MOVE_MANAGER	
@START
;debug: door ($0C) is set to #$10 here when X= $78 (NPC mode)

					
;VIABLE IDEAS NOT IMPLEMENTED
;*Ability to get lost. (probably a flag on byte 7). % chance that at any given point on the path, the NPC will stop using the path (set index to $00 maybe), and when index is $00 use a random movement algorithm
;*Ability to wander  (probably a flag on byte 7). % change on any given path move the the NPC will ignore it and instead use a random move (set flag on byte 5 indicating off path). Once off path % chance that the NPC make another random move or return to path as a flocking point. 


;IS ENTRANCE PERMITTED TO THIS ROUTINE?
	   
	;is sprite an building npc?
	LDA SPRITE.RECORD+$A
	CMP #S_ENTITY_TYPE.BLD_NPC
	BEQ .IS.NPC 
	JMP DECIDE.SPRITE.MOVE.ALGORITHM	;if no, proceed directly to the subroutine which decides what algorithm to use for mob movement
	
.NOT.IN.TRANSIT_STEP
	JMP NOT.IN.TRANSIT
	
.IS.NPC


 		
;IS NPC IN TRANSIT? 	
	LDA SPRITE.RECORD+$6 	;load transit flag
	CMP #$FF				;is it set to hostile?
	BEQ .NOT.IN.TRANSIT_STEP	;if yes, then NPC is not in transit
	CMP #$01				;is it set to a transit value?
	BCC	.NOT.IN.TRANSIT_STEP ;if no, then NPC will use an magnet algorithm to move instead of using a saved path from pathfinder. 
	;**FALLS THROUGH**		;if yes, use saved path generated by pathfinder after the NPC's first turn while in transit
	
.IN.TRANSIT		
;IS IT NPC'S LAST TRANSIT TURN?
;(if it is the NPC's first transit turn, we want the NPC to pass so that if NPC is transiting from a ladder tile the player
;is able to see the NPC on the ladder for 1 turn)

		
			
	LDA SPRITE.RECORD+$6 	;load transit flag
	CMP #$03				;is it set to first move?
	BNE	.NOT.FIRST.TRANSIT.TURN		;if no, use saved path generated by pathfinder
			
	LDA #$01
	STA SPRITE.RECORD+$6	;set NPC's transit flag to post-first turn, so than on its next turn the saved path will be used

	LDA MOB.SCREEN_STATUS.SS
	CMP #$01 				;is NPC an offscreen SS? 					;IS SPRITE AN OFFSCREEN SS?
	BEQ .PASS				;if yes, then don't set override as this NPC should not be drawn under any circumstances. 
	
	LDA #$01
	STA SPRITE.DRAWTILE.OVERRIDE	;set override so sprite tile will be drawn even move is a pass.
.PASS		
	JMP SPRITE.PASS.MOVE.ENTRANCE
.NOT.FIRST.TRANSIT.TURN
;DID NPC REACH DESTINATION ON PREVIOUS TRANSIT TURN?
	LDA SPRITE.RECORD+$6 	;load transit flag
	CMP #$04				;is it set to destination reached?
	BNE .DESTINATION.NOT.REACHED ;if no, then move via saved path

.DESTINATON.REACHED.LAST.MOVE
	LDA #$00				;if yes, set transit flag to off
	STA SPRITE.RECORD+$6			

	LDA SPRITE.RECORD+$7	;load anchor movement flag
	CMP #$FF				;is flag set to ladder anchor?
	BNE .PASS2
	
	LDA #$01
	STA SPRITE.ERASETILE.OVERRIDE ;set override flag so that sprite tile is erased (since it is now on another floor), which normally would not occur with a pass move
.PASS2
	JMP SPRITE.PASS.MOVE.ENTRANCE
	
.DESTINATION.NOT.REACHED	
.CHECK.PASS.FLAG
;DOES NPC HAVE A % FORCED PASS FLAG SET?
	LDA SPRITE.RECORD+$3	;load special flag
	CMP #$00				;is special flag set to off?
	BEQ .LOOKUP.SAVED_PATH
	CMP #$03				;is special flag > the range where the forced pass flags are setup?
	BCS .LOOKUP.SAVED_PATH	;if yes, then proceed with normal move and lookup saved path
							;if no, then see if the NPC will pass						
	
	CMP #$02
	BEQ .FLAG_02
;FLAG_01	
	LDA #NPC.SPECIAL_FLAG01.FORCED_PASS	;load the proability value for this flag
	JMP .SET.PROBABILITY
.FLAG_02	
	LDA #NPC.SPECIAL_FLAG02.FORCED_PASS	;load the proability value for this flag
	;**FALLS THROUGH
	
.SET.PROBABILITY
	STA PASS.PROBABILITY

			
.EVALUATE.PASSING
;DECIDE IF PASSIVE MOB WILL PASS	
			;INC TEMP.ARRAY
		LDA #$02
	JSR RANDOM.8	
	CMP PASS.PROBABILITY	;Does sprite pass?
			;LDX TEMP.ARRAY
			;STA TEMP.ARRAY+$1,X
	BCS .LOOKUP.SAVED_PATH				;if no, make normal move, load saved path for this NPC
	JMP SPRITE.PASS.MOVE.ENTRANCE		;If yes, jmp to pass routine to inject pass direction code
	
.LOOKUP.SAVED_PATH	
	LDX #$00				;SAVED.PATH.LOOKUP.TABLE index
.LOOP.LOOKUP.SAVED_PATH
;(path must be active and match the Anchor # and NPC Record # of the current Sprite Record)
	LDA SAVED.PATH.LOOKUP.TABLE+$1,X	;load NPC Record # of saved path
	CMP SPRITE.RECORD+$B				;does it match the current NPC record #?
	BNE .NEXT.PATH_RECORD				;if no, next saved path record
	LDA SAVED.PATH.LOOKUP.TABLE+$0,X	;load Anchor # of saved path
	CMP SPRITE.RECORD+$4				;does it match the current NPC's active Anchor #?
	BNE .NEXT.PATH_RECORD				;if no, next saved path record
	LDA SAVED.PATH.LOOKUP.TABLE+$2,X	;load status field of saved path record
	CMP #$01							;is saved path active?		
	BEQ .PATH_FOUND						;if yes, exit loop

.NEXT.PATH_RECORD
	INX									;if no, next saved path record
	INX
	INX
	INX									
	CPX #SAVED.PATH.LOOKUP.TABLE.SIZE
	BNE .LOOP.LOOKUP.SAVED_PATH
	
.ERROR.PATH_NOT_FOUND
;.LOOP.LOOKUP.SAVED_PATH (NPC.MOVE_MANAGER) reports that
;there is no saved path for an NPC in transition.

	; LDA SPRITE.RECORD+$B
	; CMP #$28
	; BEQ .TRAP
	
	; ;SET TRANSIT FLAG TO OFF
	; LDA #$00
	; STA SPRITE.RECORD+$6

	JSR APPLE_BELL
	;JSR APPLE_BELL
	;JSR APPLE_BELL
	; JSR APPLE_BELL
	; JSR APPLE_BELL
	
	LDA #$04
	STA MOB.MOVE.CURRENT 						;SET MOB'S FINAL MOVE DECISION TO PASS
	JMP MOB.MOVE.MAKE

; .TRAP	
			; LDX SAVED.XREG.LOCAL (contains the NPC record # of the NPC in transit who a saved path cannot be found for)
			; LDA #$A2 (ERROR CODE)
			; JSR FULL.BRK
	
.PATH_FOUND
@START
;=====================CODE-SECTION DOCUMENTATION====================================
;
;This code section processes the next ccordinate pair in the path. If the path is blocked,
;the NPC is put into "seek" mode to navigate around the obstacle. 
;
;If the path is open, this routine places the movement direction code into MOB.MOVE.CURRENT and does a JMP to 
;MOB.MOVE.MAKE. The path index is also decremented so that the next coordinate pair will get loaded
;by JSR LOAD.PATH.NEXT_MOVE on the NPC's next turn. 
;
;The path index is not decremented below $02 (the last coordinate pair, /aka the destination)
;until the destination is reached (i.e. NPC's GMAP.X/Y is equal to the last coordinate pair). By doing
;this the NPC will use the magnet algorithm to try to reach the destination if was thrown off of the path.
;In fact, it's worth noting that a magnet algorith (.MOVE.USING.PATH, .TEST.SCREEN_LOCATION) is
;use every time path coordinates are processed to determine the direction the NPC should move. See next section for details. 
;
;--How do NPCs use the path if the NPC is not at the source anchor x,y when the NPC is scheduled to transition?
;If an NPC has an at-anchor movement flag (sprite.record+$7)
;that does not keep the NPC at the exact anchor coordinates then the NPC will wander away from the path
;start location. However, as the path coorinates are processed, the direction they represent is based on the NPC's 
;current location relative to the path coordinates (i.e. the path coordinates are treated as a magnet to the NPC).
;The result is much more effective at navigating the NPC to it's destination than a regular magnet algoritm would be
;because feeding the path coordinates into algorithm means the magnet is usally very close to the NPC, and might even be
;only 1 tile away if the NPC is physically on the path. 
; 
;As long as the direction calculated by the magnet algorithm isn't blocked, the NPC will move in that direction. Considering NPC's don't usaully wander far
;from their anchors, this method works very well. In fact, keeping the path index at $02 was added long after the 
;initial implementation and up to that point NPCs were finding their destination anchor's just fine. Keeping the path index
;at $02 was to address some corner cases.   
;
;=================================================================================
	;X-REG = index to SAVED.PATH.LOOKUP.TABLE
	
	;GET NPC NEXT MOVE	
	JSR LOAD.PATH.INDEX		;load into TRANSIT.NEXT_MOVE.INDEX
				
	JSR LOAD.PATH.NEXT_MOVE	;load into TRANSIT.NEXT_MOVE.X/Y

	;IS NPC "SEEKING"?
	LDA SPRITE.RECORD+$06	;load transit flag
	CMP #$02				;is transit flag set to "SEEKING"?
	BNE .NPC.DESTINATION.CHECK 	;if no, then NPC will move using the X,Y coordinates loaded above 
			
	;NPC IS SEEKING: has it found the path again?
	LDA SPRITE.RECORD+$08	;load NPC x-axis
	CMP TRANSIT.NEXT_MOVE.X ;is NPC x-axis = path x-axis? 
	BNE .NPC.SEEKING.NOT_ON_PATH	;if no, then NPC has not found the path 
	LDA SPRITE.RECORD+$09		;load NPC y-axis
	CMP TRANSIT.NEXT_MOVE.Y 	;is NPC y-axis = path Y-axis? 
	BEQ .NPC.SEEKING.FOUND.PATH	;if yes, then NPC has found path
	;**FALLS THROUGH**			;if no, then NPC has not found the path 
	
.NPC.SEEKING.NOT_ON_PATH
;If player is standing on the seek path tile, then change seek path tile
;to the next tile in path
	LDA GMAP.X				;load player x-axis
	CMP TRANSIT.NEXT_MOVE.X ;is is the same as seek tile x-axis?
	BNE .USE.SAME.SEEK_TILE	;if no, then player is not standing on seek tile. Keep current seek tile for magnet algorithm
	LDA GMAP.Y				;load player y-axis
	CMP TRANSIT.NEXT_MOVE.Y ;is is the same as seek tile y-axis?
	BNE .USE.SAME.SEEK_TILE	;if no, then player is not standing on seek tile. Keep current seek tile for magnet algorithm
	
	;IS PLAYER STANDING ON NPC'S DESTINATION TILE (last tile in the path array)
	;(if yes, then don't increment the path index to avoid unpredicitible results
	;the NPC will just move around in the vicinity until the player moves of the destination tile)
	LDA TRANSIT.NEXT_MOVE.INDEX ;load path index
	CMP #$02					;is path index set to last path tile?
	BEQ .USE.SAME.SEEK_TILE		;if no, then player is not standing on NPC's destination tile. Keep current seek tile for magnet algorithm
;.CHANGE.SEEK_TILE	
	JMP NPC.PATH_SEEKING.MAGNET_ALGORITHM ;use magnet algorithm to try to find the path
	
.USE.SAME.SEEK_TILE			
	JMP NPC.PATH_SEEKING.MAGNET_ALGORITHM.START ;use magnet algorithm to try to find the path

.NPC.SEEKING.FOUND.PATH	
	;ADVANCE TO NEXT PATH TILE
	;decrement path index
	LDA TRANSIT.NEXT_MOVE.INDEX
	CMP #$03 ;don't decrement if the index is already $02 or $00. This is because the index should stay at $02 (the last coordinate pair) until 
			 ;the destination is reached. It is the destination that flips the index to $00 to mark the path as complete). 
	BCC .INDEX.INCREMENT.COMPLETE1
	SEC
	SBC #$02
	STA TRANSIT.NEXT_MOVE.INDEX
.INDEX.INCREMENT.COMPLETE1
	;no need to save index to aux memory now, it is saved later
	
	;GET NEXT PATH X,Y
	JSR LOAD.PATH.NEXT_MOVE	;load into TRANSIT.NEXT_MOVE.X/Y		;**OPT** I think the path is sometimes copied twice per iteration. if the was a page of main memory available temporarily maybe we could just copy it from aux memory once.I don't think the loader zone input/output buffers would work because they are used by path generaator which stays in memory as long as the player is in a building and map objects routine runs in buildings.
	
	;SET TRANSIT FLAG BACK TO NON-SEEKING
	LDA #$01
	STA SPRITE.RECORD+$06	;save value to transit flag

	;SEEKING DESTINATION CHECK: is destination of path reached?)
	;(note: there is a seperate destination check for seeking because
	;when the NPC GMAP.X/Y = the path X/Y that means that the NPC has found the path
	;again, not that the destination has been reached)
	LDA TRANSIT.NEXT_MOVE.X			 ;load x-axis of next move
	CMP SPRITE.RECORD+$08
	BNE .MOVE.USING.PATH	
	LDA TRANSIT.NEXT_MOVE.Y			;load Y-axis of next move
	CMP SPRITE.RECORD+$09			;Y-axis of NPC current position
	BNE .MOVE.USING.PATH

	LDA #$01						;if yes, then turn this flag on ($01). ($00 = off). This is so .DESTINATION.REACHED will know not to process a move from the saved path for this NPC.
	STA DESTINATION_REACHED_WHILE_SEEKING
	JMP .DESTINATION.REACHED	

	
.NPC.DESTINATION.CHECK	
;IS NPC DESTINATION REACHED?
;(note: there is a seperate destination check for seeking because
;when the NPC GMAP.X/Y = the path X/Y that means that the NPC has found the path
;again, not that the destination has been reached)

	LDA TRANSIT.NEXT_MOVE.X			 ;load x-axis of next move
	CMP SPRITE.RECORD+$08
	BNE .DESTINATION_REACHED.CHECK.COMPLETE	
	LDA TRANSIT.NEXT_MOVE.Y			;load Y-axis of next move
	CMP SPRITE.RECORD+$09			;Y-axis of NPC current position
	BNE .DESTINATION_REACHED.CHECK.COMPLETE
	;**FALLS THROUGH**
	
.DESTINATION.REACHED				
	LDA #$00
	STA TRANSIT.NEXT_MOVE.INDEX ;mark path index as completed

	LDA #$04
	STA SPRITE.RECORD+$6			;set transit flag to destination reached
	STA MOB.MOVE.CURRENT ;the ACC value of $04 sets the current move to pass. This variables is a parameter for MOB.MOVE.MAKE
	;note: this $04 transit flag setting was put in place when the destination check was
	;based on the path index being = $00, and when it was located at the end of the process near .UPDATE.PATH_INDEX.
	;the reason was that when the destination was reached, that fact was identified before the move was made to actually get there.
	;The destination check now just checks the current path.x/y against the NPC's gmap.X/Y and it does it up here at the top of the process. 
	;as a result, when destination reached is detected, the NPC is already at the destination. 
	;In the original method the $04 transit flag was set so that the NPC move manager would know this NPC was at the destination so that 
	;some special stuff (I think related to NPCs standing on ladders) could be done. We save $04 (pass move) to to MOB.MOVE.CURRENT so as to piggyback on that original setup. 
	;the special stuff is probaby still needed, I think it's just occuring 1 move later than it could. For example, currently when the destination
	;anchor is a ladder, the NPC is visible standing on it for 2 moves before dissappearing to show the player that the NPC climbed the ladder. 
	
	LDA #$FF
	STA SAVED.PATH.LOOKUP.TABLE+$2,X ;set path status to empty/completed/discard	

			
	LDA MOB.SCREEN_STATUS.SS
	CMP #$01 				;is NPC an offscreen SS? 					;IS SPRITE AN OFFSCREEN SS?
	BEQ .UPDATE.PATH.INDEX_STEP 	;if yes, then don't set override as this NPC should not be drawn under any circumstances. 
		
	LDA #$01
	STA SPRITE.DRAWTILE.OVERRIDE	;set override so sprite tile will be drawn even if the movement flag is set to a ladder anchor ($FF)
.UPDATE.PATH.INDEX_STEP
	JMP .UPDATE.PATH.INDEX
.DESTINATION_REACHED.CHECK.COMPLETE	
	;**FALLS THROUGH**
	
.MOVE.USING.PATH			
;DETERMINE DIRECTION CODE OF NEXT MOVE
;TRANSIT.NEXT_MOVE.X = X-AXIS
;TRANSIT.NEXT_MOVE.Y = Y-AXIS

				
	LDA TRANSIT.NEXT_MOVE.X			 ;load x-axis of next move
	CMP SPRITE.RECORD+$08
	BEQ .NORTH.OR.SOUTH
;.EAST.OR.WEST
	CMP SPRITE.RECORD+$08			;X-axis of NPC current position
	BCC .MOVE.WEST
;.MOVE.EAST
	LDY #$02						;load movement code for east in ACC as return value
	JMP .TEST.SCREEN_LOCATION
	
.MOVE.WEST
	LDY #$03						;load movement code for west in ACC as return value
	JMP .TEST.SCREEN_LOCATION
	
.NORTH.OR.SOUTH
	LDA TRANSIT.NEXT_MOVE.Y			;load Y-axis of next move
	CMP SPRITE.RECORD+$09			;Y-axis of NPC current position
	BCC .MOVE.NORTH
;.MOVE.SOUTH
	LDY #$01						;load movement code for south in ACC as return value
	JMP .TEST.SCREEN_LOCATION
	
.MOVE.NORTH
	LDY #$00						;load movement code for north in ACC as return value
	;**FALLS THROUGH**
	

	
.TEST.SCREEN_LOCATION		
;=====================CODE-SECTION DOCUMENTATION====================================
;
;If NPC is the edge of the screen for the direction
;of the path move, then skip collision checks because
;MOB.MOVES.BLOCKED may not contain a valid value for that
;direction. This is because the collision 
;testing that filled this array would have
;checked an offscreen location using the 
;onscreen collision routine. Doing so would
;result in a value from an unintended memory
;location being identified as the tile_type,
;effectively randomizing the result. 
;
;=================================================================================
	STX SAVED.XREG.LOCAL1			;save saved_path index
		;**OPT** Speed. Memory. It doesn't look like the STX above or the LDXs below are needed. Probably left over from an early, more complicated attempt at this section.		
	STY MOB.MOVE.CURRENT			;Y-REG contains the direction code associated with the saved path move
	
	LDA MOB.MOVES.BLOCKED,Y	;load open/blocked status of path move
	CMP #$02				;is status = blocked by screen edge?
	BEQ .FINALIZE.MOVE		;if yes, then proceed with path move as the open/blocked status should be ignored. 
	;**FALLS THROUGH** 		;if no, run the final collision check 
	
.FINAL.COLLISION.CHECK	
	LDA MOB.MOVES.BLOCKED,Y ;load the collision status for the candidate move direction code
	CMP #$00 				;is the path open?
	BEQ .FINALIZE.MOVE		;if yes, make the move from saved path
							;if no, then determine if path is blocked by an impassible obstacle or just a closed, unlocked, door. 
	;CHECK FOR GENERAL MAP OBJECTS
	; LDA MOB.ADJACENT_TILES,Y		;load adjacent screen tile location, in direction of saved path move, as index for map objects screen arrays
	; TAY
	; LDX SCREEN.MO_GENERAL.DATA,Y	;load general map object data for current tile location
	; CPX #$FF						;is a general map object present?
	; BEQ .NPC.PATH_BLOCKED			;if no, then path is impassable.
	
	; ;CHECK FOR CLOSED DOORS
	; LDA MAP_OBJECTS.GENERAL+$3,X	;load data byte of general map object record
	; CMP #$10						;is there a closed, unlocked, door on the path move location?	
	; BNE .NPC.PATH_BLOCKED			;if no, then path is blocked by an impassable object

	; ;NPC OPENS DOOR					;if yes
	; LDA #$24						;set data byte to code for open door timer with 3 moves
	; STA MAP_OBJECTS.GENERAL+$3,X

	; ; LDA #$00						;set open/blocked status = open, for the directon toward door
	; ; STA MOB.MOVES.BLOCKED,Y		
	; ;****DECIDED TO REMOVE BECAUSE Y-REG HAD TO GET SET TO THE SCREEN TILE LOCATION ABOVE. MOB.MOVES.BLOCKED doesn't appear to be used after this point for a saved path move though. 
	
	; JMP .FINALIZE.MOVE				;since door is now open, proceed to execute the move from saved path
		
.NPC.PATH_BLOCKED			
	LDX SAVED.XREG.LOCAL1			;restore saved_path index
	JMP NPC.PATH_SEEKING.MAGNET_ALGORITHM ;if no, attempt to navigate around the obstacle using a magnet algorithm


		
.FINALIZE.MOVE				
	LDX SAVED.XREG.LOCAL1			;restore saved_path index	
	;**FALLS THROUGH**
			

.UPDATE.PATH.INDEX
;SET PATH INDEX TO NEXT MOVE

	;decrement path index
	LDA TRANSIT.NEXT_MOVE.INDEX
	CMP #$03 ;don't decrement if the index is already $02 or $00. This is because the index should stay at $02 (the last coordinate pair) until 
			 ;the destination is reached. It is the destination that flips the index to $00 to mark the path as complete). 
	BCC .INDEX.INCREMENT.COMPLETE
	SEC
	SBC #$02
	STA TRANSIT.NEXT_MOVE.INDEX
.INDEX.INCREMENT.COMPLETE
		
	JSR SAVE.PATH.INDEX ;(decremented to next move)

	LDA DESTINATION_REACHED_WHILE_SEEKING
	BEQ .MAKE.MOVE
	JMP .DESTINATON.REACHED.LAST.MOVE
	
.MAKE.MOVE	
	LDA MOB.MOVE.CURRENT ;parameter for MOB.MOVE.MAKE
	JMP MOB.MOVE.MAKE	
@END	
	
NOT.IN.TRANSIT
	LDA SPRITE.RECORD+$6			;load transit flag
	CMP #$FF						;is it set to hostile? (used to put guards in hostile mode toward player)
	BEQ S_ENTITY.MAGNET_ALGORITHM_STEP ;if yes, use the player magnet algorithm to select the move for the NPC
	;BEQ DECIDE.SPRITE.MOVE.ALGORITHM
	
	LDA SPRITE.RECORD+$7			;load at-anchor movement flag
	CMP #$00						;pass flag
	BEQ SPRITE.PASS.MOVE.ENTRANCE_STEP
	CMP #$FF						;pass flag for NPC's on different floors than players. Needs it's own flag because it is also used to determine if the sprite's tile should be drawn.
	BEQ SPRITE.PASS.MOVE.ENTRANCE_STEP
	
	JMP SPRITE.RANDOM.MOVE.ENTRANCE

SPRITE.PASS.MOVE.ENTRANCE_STEP
	JMP SPRITE.PASS.MOVE.ENTRANCE
	
S_ENTITY.MAGNET_ALGORITHM_STEP
	JMP S_ENTITY.MAGNET_ALGORITHM
	
; ;GET MOVE BY RUNNING PATHFINDER FROM HERE		
	; LDA NPC.MOVE.COUNTER
	; CMP #$01
	; BCS .EXISTING_PATH
; ;
		; ;LOAD DATA FOR CURRENT SPRITE INTO THE PARAMETER ARRAY FOR NPC.PATHFINDER
		; LDX #$00
; .INIT.LOOP
		; LDA SPRITE.RECORD,X
		; STA PATHFINDER.SPRITE.RECORD,X		;PARAMETER
		; INX
		; CPX #SPRITE.RECORD.SIZE
		; BNE .INIT.LOOP	
; ;		
		; ;SET WEIGHT FOR NON-ROAD TILES
		; LDA #$10
		; STA PATHFINDER.STREET.PREFERENCE	
; ;		
		; ;SET PATHFINDER DESTINATION GMAP.X,Y
		; ; 26,26
		; LDA #$09
		; STA NPC.PATHFINDER.DESTINATION.TILE.X
		; LDA #$08
		; STA NPC.PATHFINDER.DESTINATION.TILE.Y	
; ;OTHER DESTINATIONS
		; ; LDA #$0B
		; ; STA NPC.PATHFINDER.DESTINATION.TILE.X
		; ; LDA #$1D
		; ; STA NPC.PATHFINDER.DESTINATION.TILE.Y			
	; ;	
		; ; 24,0C
		; ; LDA #$19
		; ; STA NPC.PATHFINDER.DESTINATION.TILE.X
		; ; LDA #$14
		; ; STA NPC.PATHFINDER.DESTINATION.TILE.Y
		; ; LDA #$0B
		; ; STA NPC.PATHFINDER.DESTINATION.TILE.X
		; ; LDA #$25
		; ; STA NPC.PATHFINDER.DESTINATION.TILE.Y		
		; ;		
		; ; 19,25
		; ; LDA #$12
		; ; STA NPC.PATHFINDER.DESTINATION.TILE.X
		; ; LDA #$08
		; ; STA NPC.PATHFINDER.DESTINATION.TILE.Y		
		; ;
		; ; 0B,19
		; ; LDA #$25
		; ; STA NPC.PATHFINDER.DESTINATION.TILE.X
		; ; LDA #$17
		; ; STA NPC.PATHFINDER.DESTINATION.TILE.Y	
		; ;
		; ;set auto-abort parameter
		; LDA #KEYPRESS.ABORT.ITERATIONS.DEFAULT	;set to KEYPRESS.ABORT.ITERATIONS.DEFAULT or set to $FF to disable auto-abort
		; ;LDA #$FF
		; STA KEYPRESS.ABORT.ITERATIONS		
	; JSR NPC.PATHFINDER ; returns movement code in ACC
; ;	
	; LDX NPC.PATHFINDER.FINAL.PATH+$0
	; STX FINAL.PATH.INDEX
; ;	
	; ;**FALLS THROUGH**
	; ;
; .EXISTING_PATH
; ;	
	; LDA FINAL.PATH.INDEX
	; SEC
	; SBC NPC.MOVE.COUNTER	
	; SBC NPC.MOVE.COUNTER	;2 SBCs because the final path record is 2 bytes
	; TAX
;	
; ;DETERMINE MOVEMENT RETURN CODE
	; LDA NPC.PATHFINDER.FINAL.PATH,X ;load x-axis of next move
	; CMP SPRITE.RECORD+$08
	; BEQ .NORTH.OR.SOUTH
; ;.EAST.OR.WEST
	; CMP SPRITE.RECORD+$08			;X-axis of NPC current position
	; BCC .MOVE.WEST
; ;.MOVE.EAST
	; LDA #$02						;load movement code for east in ACC as return value
	; JMP .MAKE.MOVE
; ;	
; .MOVE.WEST
	; LDA #$03						;load movement code for west in ACC as return value
	; JMP .MAKE.MOVE
; ;	
; .NORTH.OR.SOUTH
	; INX								;advance to byte $01 of final path array record
	; LDA NPC.PATHFINDER.FINAL.PATH,X ;load Y-axis of next move
; ;
	; CMP SPRITE.RECORD+$09			;Y-axis of NPC current position
	; BCC .MOVE.NORTH
; ;.MOVE.SOUTH
	; LDA #$01						;load movement code for south in ACC as return value
	; JMP .MAKE.MOVE
; ;	
; .MOVE.NORTH
	; LDA #$00						;load movement code for north in ACC as return value
	; JMP .MAKE.MOVE
; ;	
; .MAKE.MOVE
	; ;ACC = MOVEMENT DIRECTION CODE	
	; STA MOB.MOVE.CURRENT	;#$00=north, $01=south, $02=east, $03=west,  $04 = NO MOVE
; ;
	; INC NPC.MOVE.COUNTER
; ;
; ;
; ;			
; ;
	; JMP MOB.MOVE.MAKE	
		
@END

MOTH_BALLED.ALT.DECIDE.SPRITE.MOVE.ALGORITHM ;MOTH-BALLED
@START
	;****THIS SEECTION ISN'T NEEDED UNLESS ADHOC.SPRITE.PATHFINDER SUBROUTINE IS ENABLED.  

	
	;***NOTES ON NEXT STEPS IF THIS EVER GETS USED
; ;*In NPC.PATHFINDER, override the error trap for all paths blocked in adhoc mode is enabled. Instead return a direction code of pass ($04)
; ;*Add map object to collision checks. Make sure doors are excluded since doors can be opened.
; ;	
			; ;JMP S_ENTITY.MAGNET_ALGORITHM
			
; ;IS SPRITE AN OFFSCREEN SS?
	; LDA MOB.SCREEN_STATUS.SS
	; CMP #$01 						;is sprite an offscreen ss?
	; BEQ S_ENTITY.MAGNET_ALGORITHM_STEP	;if yes, then move using magnet algorithm as pathfinder is limited to a 32x32 grid, and adhoc pathfinder is designed for onscreen sprites. 

	
; ;IS SPRITE CLOSE ENOUGH TO PLAYER FOR ADHOC PATHFINDER?
		; LDA SPRITE.RECORD+$8 ;load sprite GMAP X-axis
		; STA PARM1.GMAP.X
		
		; LDA SPRITE.RECORD+$9 ;load sprite GMAP Y-axis
		; STA PARM1.GMAP.Y
	
		; LDA GMAP.X			;load player GMAP X-axis
		; STA PARM2.GMAP.X
		
		; LDA GMAP.Y			;load player GMAP Y-axis
		; STA PARM2.GMAP.Y	
	; JSR CALCULATE.DISTANCE
		; ;ACC =RETURN VALUE
	; CMP #SPRITE.USE.ADHOC.PATHFINDER			;is distance between player and sprite short enough to use adhoc pathfinder?
	; BCS S_ENTITY.MAGNET_ALGORITHM_STEP		;if no, use the magnet algorithm instead
	; ;**Falls Through**							;if yes, use adhoc pathfinder
@END
	;**Falls Through**
MOTH_BALLED.ADHOC.SPRITE.PATHFINDER ;MOTH-BALLED   ;Used by Mobs and Hostile NPCs when closer to the player
@START
;;*****THIS SUBROUTINE HAS BEEN MOTH-BALLED BECASUE IT RESULTS IN AN APROXIMATELY 50% REDUCITON IN SPEED BETWEEN PLAYER MOVES DUE TO THE OVERHEAD OF NPC.PATHFINDER

; .SET.RMAP.PARM
; ;IS SPRITE AN MOB
	; LDA SPRITE.RECORD+$A
	; CMP #S_ENTITY_TYPE.BLD_NPC
	; BCS .IS.NPC			;if no, we have the RMAP.X/Y for the sprite. Call pathfinder
	; ;**FALLS THROUGH

; ;CONVERT GMAP.X/Y OF MOB TO RMAP.X/Y
		; LDA SPRITE.RECORD+$8 ;load mob GMAP X-Axis
		; STA PARM.GMAP.X
		
		; LDA SPRITE.RECORD+$9 ;load mob GMAP Y-Axis
		; STA PARM.GMAP.Y

	; JSR CONVERT.GMAP_XY.RMAP_XY
		; ;SET RMAP X/Y OF SOURCE ANCHOR TO DESTINATION
		; LDA RETURN.RMAP.X
		; STA PATHFINDER.SPRITE.RECORD+$8

		; LDA RETURN.RMAP.Y
		; STA PATHFINDER.SPRITE.RECORD+$9

		; JMP .SET.REMAINING.PARMS
		
; .IS.NPC
		; ;SET RMAP X/Y OF SOURCE ANCHOR TO DESTINATION
		; LDA SPRITE.RECORD+$8	
		; STA PATHFINDER.SPRITE.RECORD+$8

		; LDA SPRITE.RECORD+$9
		; STA PATHFINDER.SPRITE.RECORD+$9
		; ;**FALLS THROUGH**


; .SET.REMAINING.PARMS	
			; ;SET RMAP.X/Y OF DESTINATION 
			; LDA RMAP.X	;load player RMAP.X axis
			; STA NPC.PATHFINDER.DESTINATION.TILE.X
			
			; LDA RMAP.Y	;load player RMAP.Y axis
			; STA NPC.PATHFINDER.DESTINATION.TILE.Y
			
			; ;SET MAP OBJECT RECORD #			
			; LDA SPRITE.RECORD+$B
			; STA PATHFINDER.SPRITE.RECORD+$B		;**OPT** Memory. Speed. May not be needed. Search for this variable in NPC.PATHFINDER

			; ;DISABLE AUTO-ABORT
			; LDA #$FF
			; STA KEYPRESS.ABORT.ITERATIONS
			; ;SET ADHOC FLAG
			; STA NPC.PATHFINDER.ADOC.FLAG	;set the flag that puts NPC.PATHFINDER in adhoc mode.
			
		; JSR NPC.PATHFINDER
			; ;ACC = RETURN VALUE (next move direction code)
			; STA MOB.MOVE.CURRENT 						;SET MOB'S FINAL MOVE DECISION TO PASS

		; JMP MOB.MOVE.MAKE
		
@END

SPRITE.RANDOM.MOVE.ENTRANCE ;BUILDING NPCs 
@START
;debug: door ($0C) is set to #$10 here when X= $78 (NPC mode)


					
					
	;Load direction codes into primary and secondary moves
	;no preference will be given to primary moves, the order is arbitrary.
	LDA #$00	
	STA MOB.MOVE.OPTIONS_PRIMARY
	LDA #$01
	STA MOB.MOVE.OPTIONS_PRIMARY+$1
	LDA #$02
	STA MOB.MOVE.OPTIONS_SECONDARY
	LDA #$03
	STA MOB.MOVE.OPTIONS_SECONDARY+$1

		
	LDA SPRITE.RECORD+$A	;load sprite type
	CMP #S_ENTITY_TYPE.BLD_NPC					;is sprite an NPC?
	BCS .IS.ANCHOR.FLOCKING.POINT	;if yes, check if NPC uses anchor as flocking point
	JMP CHOOSE.RANDOM.MOVE	;if no, proced to choose random move without further ado.
.IS.ANCHOR.FLOCKING.POINT		
	;Check for flocking point / tethered at-anchor movement code
	LDA SPRITE.RECORD+$7
	CMP #$01
	BEQ	.TETHERED_TO_ANCHOR.RADIUS1
	CMP #$02
	BEQ	.TETHERED_TO_ANCHOR.RADIUS2
	CMP #$03
	BEQ .TETHERED_TO_ANCHOR.RADIUS3
	CMP #$04
	BEQ .TETHERED_TO_ANCHOR.RADIUS4
	CMP #$10
	JMP CHOOSE.RANDOM.MOVE
	;**FALLS THROUGH
	
.ERROR.BAD.ANCHOR_MOVEMENT_FLAG
;NOT.IN.TRANSIT (NPC.MOVE.MANAGER) reports an unrecognized
;anchor movement flag in PATHFINDER.SPRITE.RECORD+$7.
;check the value in the "NPC Data" worksheet for the building the NPC is in.
	
	;LDA #$A3
	JSR FULL.BRK

.TETHERED_TO_ANCHOR.RADIUS1
	LDA #NPC.FLOCKING_POINT.RADIUS1
	STA NPC.FLOCKING_POINT.RADIUS
	JMP FLOCKING_POINT.CHECK
	
.TETHERED_TO_ANCHOR.RADIUS2
	LDA #NPC.FLOCKING_POINT.RADIUS2
	STA NPC.FLOCKING_POINT.RADIUS
	JMP FLOCKING_POINT.CHECK
	
.TETHERED_TO_ANCHOR.RADIUS3
	LDA #NPC.FLOCKING_POINT.RADIUS3
	STA NPC.FLOCKING_POINT.RADIUS
	JMP FLOCKING_POINT.CHECK	

.TETHERED_TO_ANCHOR.RADIUS4
	LDA #NPC.FLOCKING_POINT.RADIUS4
	STA NPC.FLOCKING_POINT.RADIUS
	JMP FLOCKING_POINT.CHECK
@END
	
	
SPRITE.PASS.MOVE.ENTRANCE
	LDA #$04
	STA MOB.MOVE.CURRENT 						;SET MOB'S FINAL MOVE DECISION TO PASS
	JMP MOB.MOVE.MAKE
	
			

NPC.PATH_SEEKING.MAGNET_ALGORITHM ;FOR TRANSITING NPC'S WITH BLOCKED PATHFINDER PATH
@START
;=====================CODE-SECTION DOCUMENTATION====================================
;
;This code-section, in conjunction with SPRITE.TYPE_01 constitute
;a magnet algorithm specifically desgined for NPCs who
;are transitioning between anchors, but the next move in the 
;pathfinder generated path is blocked.
;
;The next tile in the path after the blocked 
;path tile is used as the magnet.
;
;As with the MOB magnet alogrithm for player chasing,
;this algorithm prioritizes direction codes
;based on the direction the magnet is from
;the sprite. 
;
;
;=================================================================================



;HOUSEKEEPING
	;THE CURRENT PATH TILE IS BLOCKED, SO ADVANCE TO NEXT PATH TILE
	;decrement path index
	LDA TRANSIT.NEXT_MOVE.INDEX
	CMP #$03 ;don't decrement if the index is already $02 or $00. This is because the index should stay at $02 (the last coordinate pair) until 
			 ;the destination is reached. It is the destination that flips the index to $00 to mark the path as complete). 
	BCC .INDEX.INCREMENT.COMPLETE
	SEC
	SBC #$02
	STA TRANSIT.NEXT_MOVE.INDEX
	JSR SAVE.PATH.INDEX
.INDEX.INCREMENT.COMPLETE
	
	;GET NEXT PATH X,Y
	JSR LOAD.PATH.NEXT_MOVE	;load into TRANSIT.NEXT_MOVE.X/Y
			
			
	;SET NPC'S TRANSIT FLAG TO "SEEKING"
	;(seeking means that the NPC has moved off the path and is trying to get back to it)
	LDA #$02
	STA SPRITE.RECORD+$06	;save value to transit flag
	
	;STX SAVED.XREG.LOCAL	;SAVED.PATH.LOOKUP INDEX

NPC.PATH_SEEKING.MAGNET_ALGORITHM.START ;(alternate entrance)
;(skips the decrement to path index above, used
;for NPCs who are already seeking)

;PRIORITIZE PATHS	
	LDA #$FF
	STA NPC.ASSIGNED_PATHS+$0
	STA NPC.ASSIGNED_PATHS+$1
	STA NPC.ASSIGNED_PATHS+$2
	STA NPC.ASSIGNED_PATHS+$3



			
;EVALUATE.X_AXIS	
	LDA TRANSIT.NEXT_MOVE.X			;is path tile east or west of sprite?
	CMP SPRITE.RECORD+$8			;load sprite x-axis
	BEQ .X_AXIS.EQUAL				;if x-axis of path tile and sprite are the same, then neither
	BCC	.PATH_TILE.IS.WEST1			;if path tile x-axis is less than sprite, then west
;.PATH_TILE.IS.EAST
	LDA #$02 ;east
	STA MOB.MOVE.OPTIONS_PRIMARY+$0 ;save east as primary move option
	STA NPC.ASSIGNED_PATHS+$2		;record that east has been assigned
	JMP .EVALUATE.Y_AXIS
.PATH_TILE.IS.WEST1
	LDA #$03 ;west
	STA MOB.MOVE.OPTIONS_PRIMARY+$0 ;save west as primary move option
	STA NPC.ASSIGNED_PATHS+$3		;record that west has been assigned
	;**FALLS THROUGH**

.EVALUATE.Y_AXIS	
	LDA TRANSIT.NEXT_MOVE.Y			;is path tile north or south of sprite?
	CMP SPRITE.RECORD+$9			;load sprite Y-axis
	BEQ	.Y_AXIS.EQUAL				;if x-axis of path tile and sprite are the same, then neither
	BCC	.PATH_TILE.IS.NORTH			;if path tile y-axis is less than sprite, then north	
;.PATH_TILE.IS.SOUTH
	LDA #$01 ;south
	STA MOB.MOVE.OPTIONS_PRIMARY+$1 ;save south as primary move option
	STA NPC.ASSIGNED_PATHS+$1		;record that south has been assigned
	JMP .PRIORITIZE.SECONDARY
.PATH_TILE.IS.NORTH	
	LDA #$00 ;north
	STA MOB.MOVE.OPTIONS_PRIMARY+$1 ;save north as primary move option
	STA NPC.ASSIGNED_PATHS+$1		;record that north has been assigned
	JMP .PRIORITIZE.SECONDARY

	
.X_AXIS.EQUAL
	LDA TRANSIT.NEXT_MOVE.Y			;is path tile north or south of sprite?
	CMP SPRITE.RECORD+$9			;load sprite Y-axis
	BEQ .ERROR.BOTH.AXIS.EQUAL_STEP
	BCC .PATH_TILE.IS.NORTH2		;if path tile y-axis is less than sprite, then north	
;.PATH_TILE.IS.SOUTH
	LDA #$01 ;south
	STA MOB.MOVE.OPTIONS_PRIMARY+$0 ;save south as primary move option
	STA MOB.MOVE.OPTIONS_PRIMARY+$1 ;save south as primary move option
	STA NPC.ASSIGNED_PATHS+$1		;record that south has been assigned
	
	LDA #$00 ;north not available
	STA NPC.ASSIGNED_PATHS+$0		;record north as assigned, even though it wasn't, so that it is unavailable as a secondary move option
	JMP .PRIORITIZE.SECONDARY
.PATH_TILE.IS.NORTH2
	LDA #$00 ;north
	STA MOB.MOVE.OPTIONS_PRIMARY+$0	;save north as primary move option
	STA MOB.MOVE.OPTIONS_PRIMARY+$1	;save north as primary move option
	STA NPC.ASSIGNED_PATHS+$0		;record that north has been assigned
	
	LDA #$01 ;south not available
	STA NPC.ASSIGNED_PATHS+$1		;record south as assigned, even though it wasn't, so that it is unavailable as a secondary move option
	JMP .PRIORITIZE.SECONDARY

.ERROR.BOTH.AXIS.EQUAL_STEP
	JMP .ERROR.BOTH.AXIS.EQUAL
	
.Y_AXIS.EQUAL
	LDA TRANSIT.NEXT_MOVE.X			;is path tile east or west of sprite?
	CMP SPRITE.RECORD+$8			;load sprite X-axis
	BEQ .ERROR.BOTH.AXIS.EQUAL
	BCC .PATH_TILE.IS.WEST2			;if path tile y-axis is less than sprite, then west
;.PATH_TILE.IS.EAST
	LDA #$02 ;east
	STA MOB.MOVE.OPTIONS_PRIMARY+$0 ;save east as primary move option
	STA MOB.MOVE.OPTIONS_PRIMARY+$1 ;save east as primary move option
	STA NPC.ASSIGNED_PATHS+$2		;record that east has been assigned
	
	LDA #$03 ;west not available
	STA NPC.ASSIGNED_PATHS+$3		;record west as assigned, even though it wasn't, so that it is unavailable as a secondary move option
	JMP .PRIORITIZE.SECONDARY
.PATH_TILE.IS.WEST2
	LDA #$03 ;west
	STA MOB.MOVE.OPTIONS_PRIMARY+$0	;save north as primary move option
	STA MOB.MOVE.OPTIONS_PRIMARY+$1	;save north as primary move option
	STA NPC.ASSIGNED_PATHS+$3		;record that north has been assigned

	LDA #$02 ;east not available
	STA NPC.ASSIGNED_PATHS+$2		;record east as assigned, even though it wasn't, so that it is unavailable as a secondary move option
	;**FALLS THROUGH**
;	
.PRIORITIZE.SECONDARY
;upon entrance to this routine, 2 of the 4 direction codes
;have been assigned as primary moved. This routine assigneds
;the remaining direction codes as secondary moves, which it can
;identify because the codes already assigned were 
;were logged to NPC.ASSIGNED_PATHS .
;
;The elements of NPC.ASSIGNED_PATHS  corresponds
;with the direction codes: #$00=north, $01=south, $02=east, $03=west,
;
;For example, if north is assigned then $00 is saved to 
;NPC.ASSIGNED_PATHS +$0. If south is assigned then $01 is saved
;to NPC.ASSIGNED_PATHS +$1, and so on. Since the array is
;init to $FF, any element with $FF, upon entrance to this routine,
;is a direction available to be assigned. 

;INIT INDEXES
	LDY #$00				;init NPC.ASSIGNED_PATHS index
	LDX #$00				;init MOB.MOVE.OPTIONS_SECONDARY index
.LOOP.PRIORITIZE.SECONDARY
	LDA NPC.ASSIGNED_PATHS,Y	;load next element to see if the associated direction code # (#$00=north, $01=south, $02=east, $03=west) was assigned as a path
	CMP #$FF					;does element still contain the init value for this array?
	BNE .NEXT					;if no, then direction code was assigned, next element
	TYA
	STA MOB.MOVE.OPTIONS_SECONDARY,X	;if yes, then direction code was not assigned. save direction code as a secondary move option.

	CPX #$1								;have both secondary options been assigned?
	BEQ .EXIT					;if yes, then exit
	INX
.NEXT
	INY
	JMP .LOOP.PRIORITIZE.SECONDARY
	
.EXIT
			
	JMP SPRITE.TYPE_01

.ERROR.BOTH.AXIS.EQUAL
;NPC.PATH_SEEKING.MAGNET_ALGORITHM reports that the NPC's X&Y axis
;is the same as the X&Y axis of the next path tile.
;this is an error because this routine is only called if
;a transiting NPCs path was blocked. 
;
;Keep in mind that this routine advanced the path tile by 1
;tile so that this algorithm can lock onto a tile that hopefully
;isn't blocked. Regardless, there is no scenario I can think
;of where the next path tile should be the tile NPC was
;standing on when it detected a block. 
	
	;LDA #$A4
	JSR FULL.BRK
	
	
@END

DECIDE.SPRITE.MOVE.ALGORITHM ;I think this is only used as the entrance for mobs
@START

; .SHARK.CHECK ;*****TEMPORARY*** (eventally I need a way to have different % chance for move vs. pass for different mobs...probably a default and then the croc is the exception)
	; LDA SPRITE.RECORD+$02
	; CMP #TILE_ID.SHARK
	; BNE .PASSIVE.MOB.CHECK
	
	
.PASSIVE.MOB.CHECK
;SKIP IDENTIFY SCREEN SECTION IF MOB IS PASSIVE	
	LDA MOB.FLAG4		;is passive flag set on mob record?
	CMP #$01
	BEQ .EVALUATE.PASSING						;if yes, decide whether the passive mob will pass this turn
	JMP S_ENTITY.MAGNET_ALGORITHM					;if no, proceed to next section (flag4=0 is passive)
	;JMP DECIDE.SPRITE.MOVE.ALGORITHM
	
.EVALUATE.PASSING
;DECIDE IF PASSIVE MOB WILL PASS	
	JSR RANDOM.8								;90% chance of pass
	CMP #$19		;$19 for crocs, $A0 for sharks									;Does MOB pass?
	BCS .SPRITE.PASS.MOVE.ENTRANCE_STEP				;If yes, inject pass direction code
	JMP SPRITE.RANDOM.MOVE.ENTRANCE

.SPRITE.PASS.MOVE.ENTRANCE_STEP
	JMP SPRITE.PASS.MOVE.ENTRANCE
@END



S_ENTITY.MAGNET_ALGORITHM ;PLAYER IS MAGNET
@START
;=====================CODE-SECTION DOCUMENTATION====================================
;
;A magnet algorithm is a flocking algorithm that is setup so that the S_ENTITY moves towards the flocking
;point as though there was a magnet attraction to it.
;
;Prioritize move options based on location of player (or other flocking point), or more specifically, the screen section the flocking point is located in. 
;
;
;=========WHAT IS A SCREEN SECTION?========
;It refers to the region of the screen the mob is located in. 
;Screen sections are identical to those used to select the algorithm
;type in the darkness_manager.ASM routines. See /my_code/documentation/diagram1.xls
;
;The center of the chart is the player position (non-combat), or the target position (in combat)
;
;Example:
;
;Using the chart, a moving/attacking S_ENTITY is located in column 3, row 5 and the player or target is column 8, row 5.
;The following should be the variable position values. 
;
;X_GR: $00 (false)
;X_LT: $01 (true)
;Y_GR: $00 (false)
;Y_LT: $00 (false)
;
;The conclusion of this algorithm should be that the moving/attacking S_ENTITY is in section 1. 
;
;=============================
;
;The screen section the flocking point is in is determined by examining the following variables
;which are set by MOB.IDENTIFY.TILE_LOCATION or COMBAT.MOVE_MANAGER depending on whether the current S_ENTITY type is a combat mode S_ENTITY. 
;
;MOB.POSITION.X_LT ;(>=$01 = mob X-AXIS is less than flocking point X-AXIS, $00 = not less than)
;MOB.POSITION.X_GR ;(>=$01 = greater than, $00 = not greater than)
;MOB.POSITION.Y_LT ;(>=$01 = less than, $00 = not less than)
;MOB.POSITION.Y_GR ;(>=$01 = greater than, $00 = not greater than)
;
;=========DETERMINE CANDIDATE MOVE=========
;QUICK REFERENCE (MOVE DIRECTIONAL CODES): #$00=north, $01=south, $02=east, $03=west
; Here is a summary of the preferred move rules. Multiple rules can apply to a single position, the way that is handled is different for less-aggressive mobs vs. aggressive mobs. 
; if mob.x >  player.x then mob.move.preferred  = $03  (West)
; if mob.x <  player.x then mob.move.preferred  = $02  (East)
; if mob.y >  player.y then mob.move.preferred  = $00  (north)
; if mob.y <  player.y then mob.move.preferred  = $01  (south)
;=================================================================================

		

;**OPT** Speed. It may be possible to assign a value to each 1/8 screen section, and tally up the values based on the X/Y GR/LT hits int the 
;calucalte tile location section above. As long each the numbers assigned yield a unique result for each screen section, then the portion of the code below because 8 LDA/CMPs to assign the section number. Even 
;thay mabe can be skipped if the tally of the values is used as the section number in the code further down.


TEST1	;===========DETECT SECTION 0/2/1=================
@START
	LDA MOB.POSITION.X_LT ;(>=$01 = less than, $00 = not less than)
	BEQ .TEST2_STEP
	LDA MOB.POSITION.Y_LT ;(>=$01 = less than, $00 = not less than)
	BEQ SUBTEST1.1 ;branch if S_ENTITY's Y-AXIS less than the flocking point 
	JMP .SECTION0 ;else
.TEST2_STEP
	JMP TEST2
.SECTION0	
@START
	;SECTION 0
;	STA MOB.POSITION.SCREEN_SECTION
	;LDX SAVED.XREG.LOCAL
	LDA SPRITE.RECORD+$1	
	CMP #MAP_OBJECTS.Y.FIRST_ROW								;IF MOB IS IN FIRST ROW, ONLY PROVIDE SOUTH AS A PRIMARY MOVE OPTION
	BEQ .FIRST_ROW

	LDA SPRITE.RECORD	
	CMP #MAP_OBJECTS.X.FIRST_COLUMN								;IF MOB IS IN FIRST COLUMN, ONLY PROVIDE EAST AS A PRIMARY MOVE OPTION
	BEQ .FIRST_COLUMN

;MOB NOT IN FIRST ROW OR LAST COLUMN
	LDA #$02	;EAST										
	STA MOB.MOVE.OPTIONS_PRIMARY
	LDA #$01	;SOUTH
	STA MOB.MOVE.OPTIONS_PRIMARY+$1
	JMP .CONTINUE
.FIRST_COLUMN	
	LDA #$02	;EAST										
	STA MOB.MOVE.OPTIONS_PRIMARY
	LDA #$02	;EAST										
	STA MOB.MOVE.OPTIONS_PRIMARY+$1
	JMP .CONTINUE	
.FIRST_ROW
	LDA #$01	;SOUTH								
	STA MOB.MOVE.OPTIONS_PRIMARY
	LDA #$01	;SOUTH
	STA MOB.MOVE.OPTIONS_PRIMARY+$1	
	;****FALLS THROUGH
	
.CONTINUE	
	LDA #$00	;NORTH										
	STA MOB.MOVE.OPTIONS_SECONDARY								;S0: SET SECONDARY MOVE OPTIONS	
	LDA #$03	;WEST
	STA MOB.MOVE.OPTIONS_SECONDARY+$1
	
	JMP SCREEN_SECTION.DETERMINED
@END

SUBTEST1.1
@START
	LDA MOB.POSITION.Y_GR
	BEQ SECTION1 ;branch if S_ENTITY X-AXIS <= flocking point

	;**FALLS THROUGH** ;else

.SECTION2	
	LDA SPRITE.RECORD+$1	
	CMP #MAP_OBJECTS.Y.LAST_ROW									;IF MOB IS IN LAST ROW, ONLY PROVIDE NORTH AS A PRIMARY MOVE OPTION
	BEQ .LAST_ROW

	LDA SPRITE.RECORD	
	CMP #MAP_OBJECTS.X.FIRST_COLUMN								;IF MOB IS IN FIRST COLUMN, ONLY PROVIDE EAST AS A PRIMARY MOVE OPTION
	BEQ .FIRST_COLUMN

;MOB NOT IN LAST COLUMN OR LAST ROW
	LDA #$02	;EAST										
	STA MOB.MOVE.OPTIONS_PRIMARY
	LDA #$00	;NORTH
	STA MOB.MOVE.OPTIONS_PRIMARY+$1
	JMP .CONTINUE
.FIRST_COLUMN	
	LDA #$02	;EAST										
	STA MOB.MOVE.OPTIONS_PRIMARY
	LDA #$02	;EAST										
	STA MOB.MOVE.OPTIONS_PRIMARY+$1
	JMP .CONTINUE	
.LAST_ROW
	LDA #$00	;NORTH								
	STA MOB.MOVE.OPTIONS_PRIMARY
	LDA #$00	;NORTH
	STA MOB.MOVE.OPTIONS_PRIMARY+$1
	;****FALLS THROUGH
	
.CONTINUE
	LDA #$01	;SOUTH										
	STA MOB.MOVE.OPTIONS_SECONDARY								;S2: SET SECONDARY MOVE OPTIONS
	LDA #$03	;WEST
	STA MOB.MOVE.OPTIONS_SECONDARY+$1
	JMP SCREEN_SECTION.DETERMINED
@END

SECTION1
@START
	;X-AXIS < PLAYER, Y-AXIS = TO PLAYER (IMPUTED)

;SECTION 1													
	
	LDA #$02	;EAST										
	STA MOB.MOVE.OPTIONS_PRIMARY								;S1: SET PRIMARY MOVE OPTIONS
	LDA #$02	;EAST
	STA MOB.MOVE.OPTIONS_PRIMARY+$1
	LDA #$00	;NORTH										
	STA MOB.MOVE.OPTIONS_SECONDARY								;S1: SET SECONDARY MOVE OPTIONS		
	LDA #$01	;SOUTH
	STA MOB.MOVE.OPTIONS_SECONDARY+$1
	
	JMP SCREEN_SECTION.DETERMINED
@END

TEST3_STEP
	JMP TEST3
@END
	
TEST2	;===========DETECT SECTION 5/7/6=================
@START
	LDA MOB.POSITION.X_GR
	BEQ TEST3_STEP
	LDA MOB.POSITION.Y_LT
	BEQ SUBTEST2.1
	
.SECTION5
@START	
	;SECTION 5
;	LDA #$05
;	STA MOB.POSITION.SCREEN_SECTION

	;LDX SAVED.XREG.LOCAL
	LDA SPRITE.RECORD+$1	
	CMP #MAP_OBJECTS.Y.FIRST_ROW								;IF MOB IS IN FIRST ROW, ONLY PROVIDE SOUTH AS A PRIMARY MOVE OPTION
	BEQ .FIRST_ROW

	LDA SPRITE.RECORD	
	CMP #MAP_OBJECTS.X.LAST_COLUMN								;IF MOB IS IN LAST COLUMN, ONLY PROVIDE WEST AS A PRIMARY MOVE OPTION
	BEQ .LAST_COLUMN

;MOB NOT IN FIRST ROW OR LAST COLUMN
	LDA #$03	;WEST										
	STA MOB.MOVE.OPTIONS_PRIMARY
	LDA #$01	;SOUTH
	STA MOB.MOVE.OPTIONS_PRIMARY+$1
	JMP .CONTINUE
.LAST_COLUMN	
	LDA #$03	;WEST										
	STA MOB.MOVE.OPTIONS_PRIMARY
	LDA #$03	;WEST										
	STA MOB.MOVE.OPTIONS_PRIMARY+$1
	JMP .CONTINUE	
.FIRST_ROW
	LDA #$01	;SOUTH								
	STA MOB.MOVE.OPTIONS_PRIMARY
	LDA #$01	;SOUTH
	STA MOB.MOVE.OPTIONS_PRIMARY+$1
	;****FALLS THROUGH
	
.CONTINUE	
	LDA #$00	;NORTH										
	STA MOB.MOVE.OPTIONS_SECONDARY								;S5: SET SECONDARY MOVE OPTIONS
	LDA #$02	;EAST
	STA MOB.MOVE.OPTIONS_SECONDARY+$1	
	JMP SCREEN_SECTION.DETERMINED
SUBTEST2.1
	LDA MOB.POSITION.Y_GR
	CMP #$00
	BEQ SUBTEST2.2
@END

.SECTION7
@START	
	;SECTION 7	
;	LDA #$07
;	STA MOB.POSITION.SCREEN_SECTION

	;LDX SAVED.XREG.LOCAL
	LDA SPRITE.RECORD+$1	
	CMP #MAP_OBJECTS.Y.LAST_ROW									;IF MOB IS IN LAST ROW, ONLY PROVIDE NORTH AS A PRIMARY MOVE OPTION
	BEQ .LAST_ROW

	LDA SPRITE.RECORD	
	CMP #MAP_OBJECTS.X.LAST_COLUMN								;IF MOB IS IN LAST COLUMN, ONLY PROVIDE WEST AS A PRIMARY MOVE OPTION
	BEQ .LAST_COLUMN

;MOB NOT IN LAST COLUMN OR LAST ROW
	LDA #$03	;WEST										
	STA MOB.MOVE.OPTIONS_PRIMARY
	LDA #$00	;NORTH
	STA MOB.MOVE.OPTIONS_PRIMARY+$1
	JMP .CONTINUE
.LAST_COLUMN	
	LDA #$03	;WEST										
	STA MOB.MOVE.OPTIONS_PRIMARY
	LDA #$03	;WEST										
	STA MOB.MOVE.OPTIONS_PRIMARY+$1
	JMP .CONTINUE	
.LAST_ROW
	LDA #$00	;NORTH								
	STA MOB.MOVE.OPTIONS_PRIMARY
	LDA #$00	;NORTH
	STA MOB.MOVE.OPTIONS_PRIMARY+$1
	;****FALLS THROUGH
	
.CONTINUE
	LDA #$01	;SOUTH									
	STA MOB.MOVE.OPTIONS_SECONDARY							;S7: SET SECONDARY MOVE OPTIONS
	LDA #$02	;EAST
	STA MOB.MOVE.OPTIONS_SECONDARY+$1
	JMP SCREEN_SECTION.DETERMINED
SUBTEST2.2
	;X-AXIS > PLAYER, Y-AXIS = TO PLAYER (IMPUTED)
@END

.SECTION6
@START
	;SECTION 6
;	LDA #$06
;	STA MOB.POSITION.SCREEN_SECTION
;	LDA #$03	;WEST
;	STA MOB.MOVE.CANDIDATE
	LDA #$03	;WEST									
	STA MOB.MOVE.OPTIONS_PRIMARY
	LDA #$03	;WEST
	STA MOB.MOVE.OPTIONS_PRIMARY+$1
	LDA #$00	;NORTH										
	STA MOB.MOVE.OPTIONS_SECONDARY							
	LDA #$01	;SOUTH
	STA MOB.MOVE.OPTIONS_SECONDARY+$1
	
	JMP SCREEN_SECTION.DETERMINED
@END

TEST3	;===========DETECT SECTION 4/3===================
@START
	;X-AXIS = TO PLAYER (IMPUTED)
	LDA MOB.POSITION.Y_GR
	BEQ SUBTEST3.1
.SECTION4
@START	
	;SECTION 4
;	LDA #$00	;NORTH
;	STA MOB.MOVE.CANDIDATE
;	LDA #$04
;	STA MOB.POSITION.SCREEN_SECTION
	LDA #$00	;NORTH									
	STA MOB.MOVE.OPTIONS_PRIMARY				;S4: SET SECONDARY MOVE OPTIONS
	LDA #$00	;NORTH
	STA MOB.MOVE.OPTIONS_PRIMARY+$1
	LDA #$02	;EAST										
	STA MOB.MOVE.OPTIONS_SECONDARY				;S4: SET SECONDARY MOVE OPTIONS						
	LDA #$03	;WEST
	STA MOB.MOVE.OPTIONS_SECONDARY+$1
				
	JMP SCREEN_SECTION.DETERMINED
@END

SUBTEST3.1
@START
	;X-AXIS = TO PLAYER (IMPUTED), Y-AXIS < PLAYER (IMPUTED)
.SECTION3	
	;SECTION 3
;	LDA #$03
;	STA MOB.POSITION.SCREEN_SECTION
;	LDA #$01	;SOUTH
;	STA MOB.MOVE.CANDIDATE
	LDA #$01	;SOUTH								
	STA MOB.MOVE.OPTIONS_PRIMARY				;S3: SET PRIMARY MOVE OPTIONS
	LDA #$01	;SOUTH
	STA MOB.MOVE.OPTIONS_PRIMARY+$1
	LDA #$02	;EAST										
	STA MOB.MOVE.OPTIONS_SECONDARY				;S3: SET SECONDARY MOVE OPTIONS			
	LDA #$03	;WEST
	STA MOB.MOVE.OPTIONS_SECONDARY+$1
	JMP SCREEN_SECTION.DETERMINED
@END

SCREEN_SECTION.DETERMINED
@END



CHOOSE.MOB.MOVE ;
@START
	LDA MOB.FLAG0	;FLAG0: OFF=LESS AGGRESSIVE, ON=AGGRESSIVE
	CMP #$01
	BEQ SPRITE.TYPE_01
	JMP MOB.TYPE_00
	
;MOB-MOVEMENT TYPES
;less-aggressive mobs 
;		randomly pick one of moves stored in MOB.MOVE.OPTIONS_PRIMARY (2)
;		if that move is blocked, then it randomly chooses one of the open paths, or pass
;aggressive (bee-line) mobs
;		attempts moves in order (primary, primary+1,) until an unblocked move is found
;		randomly chooses between (secondary, secondary+1) 
;
;SS mobs
;		same as aggressive but they are allowed to move (and thus pursue) even when not
;		on the view screen
;
;Double Moves
;		Mob gets 2 moves to every mob turn. If player has a transport giving him/her 2 moves,
;		then on players turns, player moves two tile, and on mobs turn, mob moves two tiles. 



SPRITE.TYPE_01 ;=======AGGRESSIVE (BEE-LINE) MOBS======
@START

	
	
;used for aggressive (bee-line) MOBs and also as
;as the final part of the magnet algorithm
;for NPC's trying to navigate around a blocked
;path, the first part of which is in PRIORITIZE.NPC.MOVES

			;**OPT** Speed. Create a routine called by the game loop to generate random numbers (1 per game loop cycle) and store them in an array 1 page long, and increment a random number pointer.
			;			routines could them use the random number pointer to load a random # from the holding array, which is a lot faster than running the RANDOM.8 routine. 
;RANDOMIZE MOVES
	JSR RANDOM.8
	CMP #$80
	BCS .SCENARIO1
;SCENARIO0
	LDA MOB.MOVE.OPTIONS_PRIMARY
	STA TEMP
	LDA MOB.MOVE.OPTIONS_PRIMARY+$1
	STA MOB.MOVE.OPTIONS_PRIMARY
	LDA TEMP
	STA MOB.MOVE.OPTIONS_PRIMARY+$1

	LDA MOB.MOVE.OPTIONS_SECONDARY
	STA TEMP
	LDA MOB.MOVE.OPTIONS_SECONDARY+$1
	STA MOB.MOVE.OPTIONS_SECONDARY
	LDA TEMP
	STA MOB.MOVE.OPTIONS_SECONDARY+$1
	JMP .EVALUATE_PRIMARY
.SCENARIO1
	;LEAVE PRIMARY MOVES IN DEFAULT ORDER

.EVALUATE_PRIMARY
;CHOOSE WHICHEVER PRIMARY MOVE IS AN OPEN PATH	
	LDY MOB.MOVE.OPTIONS_PRIMARY
	LDA MOB.MOVES.BLOCKED,Y
	CMP #$00
	BEQ .PRIMARY0
	JSR PLAYER.HARASSMENT.CHECK	;if sprite is an NPC in transit, is player repeatidly blocking its path?
	LDY MOB.MOVE.OPTIONS_PRIMARY+$1
	LDA MOB.MOVES.BLOCKED,Y
	CMP #$00
	BNE .EVALUATE_SECONDARY
;.PRIMARY1
	LDA MOB.MOVE.OPTIONS_PRIMARY+$1
	STA MOB.MOVE.CURRENT
	JMP MOB.MOVE.MAKE
.PRIMARY0
	LDA MOB.MOVE.OPTIONS_PRIMARY
	STA MOB.MOVE.CURRENT
	JMP MOB.MOVE.MAKE

.EVALUATE_SECONDARY
;RANDOMIZE SECONDARY MOVES
	LDA MOB.MOVE.OPTIONS_PRIMARY
	CMP MOB.MOVE.OPTIONS_PRIMARY+$1		;are both primary moves the same? (this happens when an NPC is in seek move and the NPC is on the same X or Y axis as the path tile that it is seeking)
	BEQ .GET.RANDOM.NUMBER
	JSR PLAYER.HARASSMENT.CHECK	;if sprite is an NPC in transit, is player repeatidly blocking its path?
.GET.RANDOM.NUMBER
	JSR RANDOM.8
	CMP #$80
	BCS .SCENARIO1_SECONDARY
;SCENARIO0
	LDA MOB.MOVE.OPTIONS_SECONDARY
	STA TEMP
	LDA MOB.MOVE.OPTIONS_SECONDARY+$1
	STA MOB.MOVE.OPTIONS_SECONDARY
	LDA TEMP
	STA MOB.MOVE.OPTIONS_SECONDARY+$1
	JMP .EVALUATE_SECONDARY.START
.SCENARIO1_SECONDARY
	;LEAVE PRIMARY MOVES IN DEFAULT ORDER

.EVALUATE_SECONDARY.START
;CHOOSE WHICHEVER PRIMARY MOVE IS AN OPEN PATH	
	LDY MOB.MOVE.OPTIONS_SECONDARY
	LDA MOB.MOVES.BLOCKED,Y
	CMP #$00
	BEQ .SECONDARY0
	LDY MOB.MOVE.OPTIONS_SECONDARY+$1
	LDA MOB.MOVES.BLOCKED,Y
	CMP #$00
	BNE .PASS
;.SECONDARY1
	LDA MOB.MOVE.OPTIONS_SECONDARY+$1
	STA MOB.MOVE.CURRENT
	JMP MOB.MOVE.MAKE
.SECONDARY0
	LDA MOB.MOVE.OPTIONS_SECONDARY
	STA MOB.MOVE.CURRENT
	JMP MOB.MOVE.MAKE	
.PASS
;THIS SHOULD NEVER HAPPEN. IT'S LIKE THE EMERGENCY BRAKE. NEVER USE. 
	LDA #$04
	STA MOB.MOVE.CURRENT
	JMP MOB.MOVE.MAKE
	
@END
	
MOB.TYPE_00 ;=======LESS-AGGRESSIVE MOBS============
@START
;CHOOSE CANDIDATE MOVE
	JSR RANDOM.8
	CMP #$80
	BCS .OPTION1.CANDIDATE
;OPTION0 PREFERRED
	LDA MOB.MOVE.OPTIONS_PRIMARY
	STA MOB.MOVE.CANDIDATE
	JMP .CANDIDATE_MOVE.DETERMINED
.OPTION1.CANDIDATE
	LDA MOB.MOVE.OPTIONS_PRIMARY+$1
	STA MOB.MOVE.CANDIDATE
	
.CANDIDATE_MOVE.DETERMINED
;RE-INIT VARIABLES
	; LDA #$FF
	; LDA MOB.MOVE.OPTIONS_PRIMARY
	; LDA MOB.MOVE.OPTIONS_PRIMARY+$1
	; LDA MOB.MOVE.OPTIONS_SECONDARY
	; LDA MOB.MOVE.OPTIONS_SECONDARY+$1
	
;IS CANDIDATE MOVE BLOCKED?
	LDY MOB.MOVE.CANDIDATE
	LDA MOB.MOVES.BLOCKED,Y
	CMP #$01
	BCS .MOVE.CHOOSE_ALTERNATE

.MOVE.NOT_BLOCKED	
	LDA MOB.MOVE.CANDIDATE
	STA MOB.MOVE.CURRENT
	JMP MOB.MOVE.MAKE

.MOVE.CHOOSE_ALTERNATE	
	;STX SAVED.XREG.LOCAL
	
	LDA #$04											;ADD PASS AS A MOVE OPTION
	STA MOB.MOVE.OPTIONS_SECONDARY+$2
	JMP CHOOSE.RANDOM.MOVE
	
@END

FLOCKING_POINT.CHECK ; NPC ACTIVE ANCHOR IS FLOCKING POINT
@START
;INIT VARIABLES
	LDA #$00
	STA DISTANCE.NORTH.OF_FLOCKING_POINT
	STA DISTANCE.SOUTH.OF_FLOCKING_POINT
	STA DISTANCE.EAST.OF_FLOCKING_POINT
	STA DISTANCE.WEST.OF_FLOCKING_POINT
	
;GET GMAP.X/Y OF NPC'S ACTIVE ANCHOR
	LDX SPRITE.RECORD+$4
	LDA NPC.ANCHORS.X,X
	STA NPC.ACTIVE.ANCHOR.X
	
	LDA NPC.ANCHORS.Y,X
	STA NPC.ACTIVE.ANCHOR.Y

	




					
.CALCULATE.DISTANCE				;**OPT** Memory. there is a simular .CALCULATE.DISTANCE routine in NPC.PATHFINDER. (npc_building.ASM). Maybe they can merge into a single subroutine called by JSR		
@START
;CALCULATE DISTANCE FROM FLOCKING POINT
	LDA NPC.ACTIVE.ANCHOR.X
	CMP SPRITE.RECORD+$8
	BCC .MOB.MO_X_LESS
	;Flocking point is east of sprite
	SEC
	SBC SPRITE.RECORD+$8
	STA DISTANCE.WEST.OF_FLOCKING_POINT

	JMP .MOB.YTEST

.MOB.MO_X_LESS
;Flocking point is west of sprite
	LDA SPRITE.RECORD+$8
	SEC
	SBC NPC.ACTIVE.ANCHOR.X
	STA DISTANCE.EAST.OF_FLOCKING_POINT
	;**FALLS THROUGH**
	
.MOB.YTEST	
	LDA NPC.ACTIVE.ANCHOR.Y
	CMP SPRITE.RECORD+$9
	BCC .MOB.MO_Y_LESS
	;Flocking point is south of sprite
	SEC
	SBC SPRITE.RECORD+$9
	STA DISTANCE.NORTH.OF_FLOCKING_POINT	
	JMP .DISTANCE.CALC.COMPLETE

.MOB.MO_Y_LESS
	;Flocking point is north of sprite
	LDA SPRITE.RECORD+$9
	SEC
	SBC NPC.ACTIVE.ANCHOR.Y	
	STA DISTANCE.SOUTH.OF_FLOCKING_POINT
	;**FALLS THROUGH**
.DISTANCE.CALC.COMPLETE	
@END

;ADJUST MOVE DIRECTIONS AVAILABLE TO NPC, IF NEEDED
@START
.CHECK.NORTH
	LDA DISTANCE.NORTH.OF_FLOCKING_POINT	;load NPC distance from flocking point in noted direction. 
	CMP NPC.FLOCKING_POINT.RADIUS		;# of moves NPC is permitted to travel from flocking point in any direction. 
	BCC .CHECK.SOUTH	;if NPC at the maxium radius? If no, then no adjustments to the move direction available to the NPC are needed
				;if yes, then set this direction as blocked, if it wasn't already, in MOB.MOVES.BLOCKED
	LDA #$01
	STA MOB.MOVES.BLOCKED+$0
	;**FALLS THROUGH**
.CHECK.SOUTH 
	LDA DISTANCE.SOUTH.OF_FLOCKING_POINT	;load NPC distance from flocking point in noted direction. 
	CMP NPC.FLOCKING_POINT.RADIUS		;# of moves NPC is permitted to travel from flocking point in any direction. 
	BCC .CHECK.EAST 	;if NPC at the maxium radius? If no, then no adjustments to the move direction available to the NPC are needed
				;if yes, then set this direction as blocked, if it wasn't already, in MOB.MOVES.BLOCKED
	LDA #$01
	STA MOB.MOVES.BLOCKED+$1
	;**FALLS THROUGH**	
.CHECK.EAST 
	LDA DISTANCE.EAST.OF_FLOCKING_POINT	;load NPC distance from flocking point in noted direction. 
	CMP NPC.FLOCKING_POINT.RADIUS		;# of moves NPC is permitted to travel from flocking point in any direction. 
	BCC .CHECK.WEST 								;if NPC at the maxium radius? If no, then no adjustments to the move direction available to the NPC are needed
					;if yes, then set this direction as blocked, if it wasn't already, in MOB.MOVES.BLOCKED
	LDA #$01
	STA MOB.MOVES.BLOCKED+$2
	;**FALLS THROUGH**
.CHECK.WEST 
	LDA DISTANCE.WEST.OF_FLOCKING_POINT	;load NPC distance from flocking point in noted direction. 
	CMP NPC.FLOCKING_POINT.RADIUS		;# of moves NPC is permitted to travel from flocking point in any direction. 
	BCC .EXIT	;if NPC at the maxium radius? If no, then no adjustments to the move direction available to the NPC are needed
				;if yes, then set this direction as blocked, if it wasn't already, in MOB.MOVES.BLOCKED
	LDA #$01
	STA MOB.MOVES.BLOCKED+$3
	
.EXIT

			
@END
	;**FALLS THROUGH**
@END

;****BUG: I'M NOT SURE THE PASS MOVE OPTION IS HOOKED UP. THE ROUTINE BELOW ENDS WITH SECONDARY+$1
CHOOSE.RANDOM.MOVE
@START
;ENTRANCE: DIRECT, via MOB.TYPE_00
;PARAMETERS: MOB.MOVE.OPTIONS_PRIMARY(2), MOB.MOVE.OPTIONS_SECONDARY
;RETURN: MOB.MOVE.CURRENT


;=====================CODE-SECTION DOCUMENTATION====================================
;
;This code section is used by MOB.TYPE_00 (falls through)
;under some scenarios and is used by other routines
;which result in a random move for a sprite. 
;
;This routine compares MOB.MOVE.OPTIONS_PRIMARY(2) and
;MOB.MOVE.OPTIONS_SECONDARY(2) (which hold direction codes)
;against MOB.MOVES.BLOCKED(4), to determine the number of
;open paths (Y-REG) the direction codes for which are
;stored in MOB.MOVE.OPEN_PATHS(up to 5)
;
;Once the open paths are tallied and their direction
;code identified, one of them is randomly selected
;and loaded into MOB.MOVE.CURRENT, the parameter
;to MOB.MOVE.MAKE which makes movement happen.
;
;The purpose of doing a tally of the number of open
;paths is that it allows for a more efficient random
;selection process (faster).
;
;
;=================================================================================


;;DRIVER TEMPLATE
	; LDA #$00
	; STA MOB.MOVE.OPTIONS_PRIMARY
	; LDA #$01
	; STA MOB.MOVE.OPTIONS_PRIMARY+$1
	; LDA #$02
	; STA MOB.MOVE.OPTIONS_SECONDARY
	; LDA #$03
	; STA MOB.MOVE.OPTIONS_SECONDARY+$1
	; JMP RANDOM.MOVE.ENTRANCE
	

;DETERMINE OPEN PATHS AND STORE IN ARRAY
	LDX #$00
	LDY #$00
	
	LDA MOB.MOVE.OPTIONS_PRIMARY					
	TAX	
	LDA MOB.MOVES.BLOCKED,X								;EVALUATE THE PRIMARY AND SECONDARY MOVE OPTOINS TO SEE IF THEY ARE BLOCKED. 
	CMP #$01
	BCS .OPTION0.BLOCKED								;IF YES, DON'T RECORD AS AN OPEN PATH
	LDA MOB.MOVE.OPTIONS_PRIMARY						
	STA MOB.MOVE.OPEN_PATHS,Y							;IF NO, RECORD AS AN OPEN PATH
	INY													;INCREMENT TALLY OF OPEN PATHS, WHICH WILL BE USED TO BRANCH INTO A RANDOM NUMBER ROUTINE WITH THE NUMBER OF CHOICES EQUAL TO THE NUMBER OF OPEN PATHS 
.OPTION0.BLOCKED
	LDA MOB.MOVE.OPTIONS_PRIMARY+$1						;REPEAT ABOVE CONCEPT, UNTIL ALL MOVE OPTIONS ARE EVALUATED
	TAX
	LDA MOB.MOVES.BLOCKED,X
	CMP #$01
	BCS .OPTION1.BLOCKED
	LDA MOB.MOVE.OPTIONS_PRIMARY+$1
	STA MOB.MOVE.OPEN_PATHS,Y
	INY	
.OPTION1.BLOCKED
	LDA MOB.MOVE.OPTIONS_SECONDARY
	TAX
	LDA MOB.MOVES.BLOCKED,X
	CMP #$01
	BCS .OPTION2.BLOCKED
	LDA MOB.MOVE.OPTIONS_SECONDARY
	STA MOB.MOVE.OPEN_PATHS,Y
	INY	
.OPTION2.BLOCKED
	LDA MOB.MOVE.OPTIONS_SECONDARY+$1
	TAX
	LDA MOB.MOVES.BLOCKED,X
	CMP #$01
	BCS .OPTION3.BLOCKED
	LDA MOB.MOVE.OPTIONS_SECONDARY+$1
	STA MOB.MOVE.OPEN_PATHS,Y
	INY	
.OPTION3.BLOCKED
	LDA #$04
	STA MOB.MOVE.OPEN_PATHS,Y
	INY
	;Y-REG HOLDS A TALLY, THE NUMBER OF OPEN PATHS

;RANDOMLY CHOOSE AN OPEN PATH AS THE FINAL MOVE CHOICE	
	LDX #$00
	
	CPY #$01
	BEQ .PATHS_1
	CPY #$02
	BEQ .PATHS_2
	CPY #$03
	BEQ .PATHS_3
	CPY #$04
	BEQ .PATHS_4
	CPY #$05
	BEQ .PATHS_5
;			JMP .PATHS_4

.PATHS_ERROR
;UNEXPECTED NUMBER OF OPEN PATHS REPORTED IN .MOB.MOVE.CHOOSE_ALTERNATE	
	BRK
	
.PATHS_1 ;**OPT** Memory. In each section, do the STA MOB.MOVE.CURRENT after the JMP 
	LDA MOB.MOVE.OPEN_PATHS,X
	STA MOB.MOVE.CURRENT
	JMP MOB.MOVE.MAKE
.PATHS_2
	JSR RANDOM.8
	CMP #$80
	BCS .P2.OPTION1.CANDIDATE
;P2.OPTION0
	LDA MOB.MOVE.OPEN_PATHS
	STA MOB.MOVE.CURRENT
	JMP MOB.MOVE.MAKE
.P2.OPTION1.CANDIDATE
	LDA MOB.MOVE.OPEN_PATHS+$1
	STA MOB.MOVE.CURRENT
	JMP MOB.MOVE.MAKE

.PATHS_3 ;SPLIT POINTS $55, $AA
	JSR RANDOM.8
	CMP #$55
	BCS .PATHS_3.NEXT_TEST
;P3.OPTION0
	LDA MOB.MOVE.OPEN_PATHS
	STA MOB.MOVE.CURRENT
	JMP MOB.MOVE.MAKE
.PATHS_3.NEXT_TEST
	CMP #$AA
	BCC .P3.OPTION2.CANDIDATE
;P3.OPTION1
	LDA MOB.MOVE.OPEN_PATHS+$1
	STA MOB.MOVE.CURRENT
	JMP MOB.MOVE.MAKE
.P3.OPTION2.CANDIDATE
	LDA MOB.MOVE.OPEN_PATHS+$2
	STA MOB.MOVE.CURRENT
	JMP MOB.MOVE.MAKE

.PATHS_4 ;SPLIT POINTS $3F, $74, $BF
	JSR RANDOM.8
	CMP #$3F
	BCS .PATHS_4.NEXT_TEST1
;P4.OPTION0
	LDA MOB.MOVE.OPEN_PATHS
	STA MOB.MOVE.CURRENT
	JMP MOB.MOVE.MAKE
.PATHS_4.NEXT_TEST1
	CMP #$BF
	BCS .P4.OPTION3.CANDIDATE
;PATHS_4.NEXT_TEST2 
	CMP #$74
	BCS .P4.OPTION2.CANDIDATE
;P4.OPTION1	
	LDA MOB.MOVE.OPEN_PATHS+$1
	STA MOB.MOVE.CURRENT
	JMP MOB.MOVE.MAKE
.P4.OPTION2.CANDIDATE
	LDA MOB.MOVE.OPEN_PATHS+$2
	STA MOB.MOVE.CURRENT
	JMP MOB.MOVE.MAKE
.P4.OPTION3.CANDIDATE
	LDA MOB.MOVE.OPEN_PATHS+$3
	STA MOB.MOVE.CURRENT
	JMP MOB.MOVE.MAKE
	
	
.PATHS_5 ;SPLIT POINTS $33, $66, $99, $CC,
	JSR RANDOM.8
	CMP #$33
	BCS .PATHS_5.NEXT_TEST1
;P5.OPTION0
	LDA MOB.MOVE.OPEN_PATHS
	STA MOB.MOVE.CURRENT
	JMP MOB.MOVE.MAKE
.PATHS_5.NEXT_TEST1
	CMP #$CC
	BCS .P5.OPTION3.CANDIDATE
;PATHS_5.NEXT_TEST2 
	CMP #$99
	BCS .P5.OPTION2.CANDIDATE
;PATHS_5.NEXT_TEST3
	CMP #$66
	BCS .P5.OPTION4.CANDIDATE		
;P5.OPTION1	
	LDA MOB.MOVE.OPEN_PATHS+$1
	STA MOB.MOVE.CURRENT
	JMP MOB.MOVE.MAKE
.P5.OPTION2.CANDIDATE
	LDA MOB.MOVE.OPEN_PATHS+$2
	STA MOB.MOVE.CURRENT
	JMP MOB.MOVE.MAKE
.P5.OPTION3.CANDIDATE
	LDA MOB.MOVE.OPEN_PATHS+$3
	STA MOB.MOVE.CURRENT
	JMP MOB.MOVE.MAKE
.P5.OPTION4.CANDIDATE
	LDA MOB.MOVE.OPEN_PATHS+$4
	STA MOB.MOVE.CURRENT
;	JMP MOB.MOVE.MAKE
	
	;***FALLS THROUGH TO MOB.MOVE.MAKE
@END
@END
@END
@END

;MOVE EXECUTION
@START	
MOB.MOVE.MAKE ;				
@START	
;PARAMETERS: ACC (MOB.MOVE.CURRENT), SAVED.XREG.LOCAL (MOB RECORD INDEX), MAP_OBJECTS.TILE_LOCATION
;QUICK REFERENCE: #$00=north, $01=south, $02=east, $03=west, $04 = NO MOVE


;=====================SUBROUTINE DOCUMENTATION====================================
;This subroutine is passed the move code for the move the mob made via the ACC.
;Unless the mob is an offscreen SS, or unless the tile the mob is located in
;(prior to making its move) is hidden, the mob's tile is erased. 
;
;Then .MOB.MOVE.IMPLIMENT branches to the mob move routine associated with the move code
;in the ACC a the start of this routine. 
;
;The movement routines adjust the x,y position of the mob and also
;make a final decision on whether the mob's tile should be drawn. Things
;considered include whether the mob's move resulted in an offscreen SS moving onto the view screen
;or any kind of mob moving off the view screen. 
;
;If the players move resulted in the mob being located in the 1st column or row
;of the view screen, a move for the mob isn't permitted this turn to avoid
;the appearance that the mob appeared out of nowhere. This is handled in .NOT_INITIAL_DRAW (MOB.MOVEMENT)
;
;---Double Movers----
;In general mob's with the double mover flag set are given a 2nd move
;by MOB.MOVE.COMPLETE jumping back to MOB.MOVEMENT if the mob has the
;double mover flag set. The number of moves is tracked in MOB.MOVE.COUNTER.
;There is also an erase counter (MOB.MOVE.ERASE_COUNTER) so that the 
;double mover isn't erased more than once (I think).
;
;There was some kind of issue that occured with double movers hitting 
;slow progress. I don't recall what the issue was, but this fix has something
;to do with the code in MOB.MOVE.PASS. That section didn't exist unil the issue 
;occured (pass was handled differently, probably just a JMP to MOB.MOVE.COMPLETE)
;
;The double mover implementation is complicated, there were a variety of
;corner cases that came up, and it's pretty much shoe horned into the 
;code. There could be things I didn't remember when writing this. 
;=================================================================================

;debug: door ($14) is set to #$10 here when X= $00 (NPC mode)
;debug: door ($0C) is set to #$24 here when X= $78 (NPC mode)


					
					
			
	;ACC = MOB.MOVE.CURRENT (note, calling subroutines must save the move code to MOB.MOVE.CURRENT and have it in the ACC when making the call)
	STA MOB.MOVE.LAST
			
.CHECK.DOOR
;VALIDATE ENTRANCE

	;ACC = MOB.MOVE.CURRENT
	CMP #$04 	;is move = pass?
	BEQ .CHECK.DOOR.DONE			;If yes, then skip door check. Necessary because MOB.ADJACENT_TILES is set during the collision checks before the move direction is determined. 
	LDY MOB.SCREEN_STATUS.NPM		;($00=onsceen | $01 = offscreen). Is NPC onscreen?
	BNE	.CHECK.DOOR.DONE			;If no, then skip check. Offscreen map objects cannot be detected using the screen arrays such as SCREEN.MO_GENERAL.DATA.
	
;IS THERE A CLOSED, UNLOCKED DOOR IN THE DIRECTION OF MOVE?
	TAY ;transfer move direction code to Y-REG as index

					
	LDA MOB.ADJACENT_TILES,Y		;load adjacent screen tile location, in direction of saved path move, as index for map objects screen arrays
	TAY
	LDX SCREEN.MO_GENERAL.DATA,Y	;load general map object data for current tile location
	CPX #$FF						;is a general map object present?
	BEQ .CHECK.DOOR.DONE			;if no, then path is impassable.  (update 11/5/2017; I think this should say "if no, then path is passable" but I'm not 100% sure)

	
	;CHECK FOR CLOSED DOORS	
	LDA MAP_OBJECTS.GENERAL+$3,X	;load data byte of general map object record
	CMP #$10						;is there a closed, unlocked, door on the path move location?	
	BNE .CHECK.DOOR.DONE			;if no, then path is blocked by an impassable object

	;NPC OPENS DOOR					;if yes
	LDA #$24						;set data byte to code for open door timer with 3 moves
	STA MAP_OBJECTS.GENERAL+$3,X	

	
					; STA TEMP
					; TXA
					; PHA
					; LDA TROUBLESHOOTING.HOOK2
					; CMP #$01
					; BNE .TEMP
					; ;CPY #$58 ;is darkness algorithm looking a the door tile in question (triggers when player is on right edge tile of road, then press 0 and move)
					; ;CPY #$59 ;is darkness algorithm looking a the door tile in question (triggers when player is on middle tile of road, then press 0 and move)
					; ; CPX #$10 ;door MO $14, = $24 (top of loop, MANAGE.OBJECTS)
					; ; CPX #$0C ;door MO $14, = $24 (top of loop, MANAGE.OBJECTS)
					; ; CPX #$08 ;door MO $14, = $24 (top of loop, MANAGE.OBJECTS)
					; ; CPX #$04 ;door MO $14, = $24 (top of loop, MANAGE.OBJECTS)

					; LDX SAVED.XREG.LOCAL						;restore map object array index
					; CPX #$78
					; BNE .TEMP
					; LDA MANAGE_OBJECTS.NPC_MOB.RECORD.FLAG		;$00=mob iteration, $01=npc iteration, $02=next map object record
					; CMP #$01
					; BNE .TEMP
					
					
					STY $BF00 ;screen index of the destination tile, net of NPC's move
					LDX SCREEN.MO_GENERAL.DATA,Y	;load general map object data for current tile location
					STX $BF01
					LDA MAP_OBJECTS.GENERAL+$3,X	;load data byte of general map object record
					STA $BF02

					LDa MOB.SCREEN_STATUS.NPM		;($00=onsceen | $01 = offscreen). Is NPC onscreen?
					sta $bf03
					; LDA MAP_OBJECTS.GENERAL+$2,X			;empty record?
					; STA $BF00
					; LDA MAP_OBJECTS.MOB+$2,X				;empty record?
					; STA $BF01
					; LDA MAP_OBJECTS.NPC+$2,X				;empty record?
					; STA $BF02	
					lda #MOB.ADJACENT_TILES
					sta $bf04
					lda /MOB.ADJACENT_TILES
					sta $bf05
					
					LDA #$AA
					ldx SAVED.XREG.LOCAL						;restore map object array index
					LDY MOB.MOVE.LAST
					; ;LDY MANAGE_OBJECTS.NPC_MOB.RECORD.FLAG		;$00=mob iteration, $01=npc iteration, $02=next map object record
					; ; LDY PLAYER.WALKING.TILE.DEFAULT
					; ; ldx PLAYER.WALKING.TILE
					; ; LDX #SCREEN.MO_GENERAL.DATA
					; ; LDY /SCREEN.MO_GENERAL.DATA
					JSR FULL.BRK	;use stack to trace call
.TEMP
					PLA
					TAX
					LDA TEMP

					
	;**FALLS THROUGH**
	
.CHECK.DOOR.DONE

.BRANCH.TO.MOVE.ROUTINE	
;====BRANCH TO CORRECT MOVE ROUTINE=======
	LDY MAP_OBJECTS.TILE_LOCATION
						

				
;IF MOB IS AN OFFSCREEN SS TYPE SKIP ERASE TILE AND DARKNESS CHECK
	LDA MOB.SCREEN_STATUS.SS
	CMP #$01 									;IS MOB AN OFFSCREEN SS?
	BEQ .MOB.MOVE.IMPLIMENT_STEP				;IF YES, FIND THE MAP LOCATION FOR USE WITH COLLISION CHECKS ONLY
												;IF NO, FIND THE SCREEN LOCATION OF THE MOB
									
	LDA MOB.MOVE.CURRENT	
	CMP #$04					
	BNE .MOB.MOVE.ERASETILE
			
	LDA SPRITE.ERASETILE.OVERRIDE
	CMP #$01					;is override set? This happens when an NPC in transit reaches it's destination and the destination is a ladder (anchor movement flag = $FF)
	BEQ .MOB.MOVE.ERASETILE
	
	JMP MOB.MOVE.PASS						;IF NO MOB MOVE, NEXT TILE OR EXIT, UNLESS DOUBLE MOVER. 
												
.MOB.MOVE.IMPLIMENT_STEP
	JMP .MOB.MOVE.IMPLIMENT
	
.MOB.MOVE.MAKE.ONSCREEN ;
;TESTHOOK2
;(HARD CODE MOB MOB, OVERRIDE COMPUTER SELECTION)
;		LDA #$02
;		STA MOB.MOVE.CURRENT

.MOB.MOVE.ERASETILE 
@START
;=====================CODE-SECTION DOCUMENTATION====================================
;
;Under certain circumstances, the SPRITE tile is erased by drawing the underlying
;terrain tile and by setting the SPRITE screen array values to default value. 
;
;=================================================================================

	
	LDA MOB.FLAG2
	CMP #$01									;IS DOUBLE MOVER FLAG SET? 
	BNE .CONTINUE								;IF NO, CONTINUE WITH ERASE TILE
	LDA MOB.MOVE.ERASE_COUNTER				
	CMP #$01									;WAS TILE ALREADY ERASED THIS TURN? (I.E. ON THE MOB'S 1ST MOVE)
	BCS .MOB.MOVE.IMPLIMENT_STEP				;IF YES, SKIP ERASE TILE
												;IF NO, CONTINUE WITH ERASE TILE
.CONTINUE	
	LDA MOB.FLAG3								;MULT-TILE MOB?
	CMP #$01
	BEQ .MOB.MOVE.ERASETILE.MT					;IF YES, USE THE MULTI-TILE ERASE ROUTINE
	
;IS TILE HIDDEN (DARKNESS)?	
	LDA SCREEN.DARK.DATA,Y			
	CMP #$01
	BEQ .MOB.PREMOVE_LOCATION.NOTVISIBLE	;IF YES, DON'T ERASE SPRITE TILE (BY DRAWING TERRAIN TILE)
		
	LDA SCREEN.TILE.DATA,Y
	STA SAVED_TILE_TYPE
		;**OPT** Memory. Speed. I think the above two lines can be removed. SAVE_TILE_TYPE is only used when the tile type you want drawn is not stored in SCREEN.TILE.DATA, which in this case it is since we are "erasing".

		; LDA #$08							;SET TRACE
		; STA CALLED_BY						;(****not needed, just trace RTS address from the stack)
	JSR DRAW.TILE.SINGLE					;REPLACE MOB TILE WITH UNDERLYING MAP TILE
		LDA #$00							;RESET TRACE
		;STA CALLED_BY
		STA SPRITE.ERASETILE.OVERRIDE
		
	LDA #$01
	STA MOB.MOVE.ERASE_COUNTER				;RECORD THAT TILE WAS ERASED THIS TURN SO IT'S NOT ERASED ON MOB'S SECOND MOVE, IF ITS A DOUBEL MOVER
	
.MOB.PREMOVE_LOCATION.NOTVISIBLE	
	LDA #$FF
	STA SCREEN.MO_SPRITE.DATA,Y				;REMOVE MOB FROM MO SCREEN ARRAY
	STA SCREEN.MO_SPRITE_TYPE.DATA,Y		;REMOVE MOB FROM MO SCREEN ARRAY	
		;***OPT** Memory. Speed. it may be possible to remove .MOB.PREMOVE_LOCATION.NOTVISIBLE. SCREEN.MO_SPRITE.DATA is init to $FF at start of MO.DRAW. The location of the mob is recorded after the mob moves. Thus, if the pre-move location isn't visible, I'm not sure why we'd need to record that in the array. The erase routine is needed though as the graphic would have scrolled from the mob location prior to the player move. 
	
	JMP .MOB.MOVE.IMPLIMENT
@END	
.MOB.MOVE.ERASETILE.MT
@START	
	LDA SPRITE.RECORD					;LOAD X-AXIS (COLUMN) OF TILE0 OF MT MOB
	STA MOB.MT.POSITION.X					;SAVE FOR FUTURE USE, WHEN X/Y-REG ARE BOTH BUSY
	
	LDX #$00	

.MT.ERASELOOP	
	LDY MOB.MT.TILE_LOCATIONS,X				;LOAD NEXT TILE LOCATION



;IS TILE ON THE VIEW SCREEN
;(normally this condition indicates an error, but with MT Mobs, it can happen legitimately if the mob is half on/off the screen. In that case, it's legit but the correct process is to not draw the offscreen tiles)	
	;IF TILE IS OFFSCREEN TO NORTH OR SOUTH, THE TILE # WILL EXCEED THE LAST ARRAY ELEMENT
	LDA #SCREEN.ARRAY.LAST_ELEMENT
	CMP MOB.MT.TILE_LOCATIONS,X	
	BCS .CHECK.EAST_WEST
	JMP .TILE.OFFSCREEN
.CHECK.EAST_WEST
	;OFFSCREEN EAST/WEST REQUIRES CHECKING THE COLUMN 
	LDA MOB.MT.POSITION.X					;LOAD X-AXIS (COLUMN) OF TILE0 OF MT MOB. LOADING FROM VARIABLE BECAUSE X/Y-REG ARE BOTH IN USE	

	CPX #$00								;IS TILE 0 THE CURRENT TILE??
	BEQ .TILE0.OFFSCREEN_CHECK
	CPX #$01								;IS TILE 1 THE CURRENT TILE??
	BEQ .TILE1.OFFSCREEN_CHECK
	CPX #$02								;IS TILE 2 THE CURRENT TILE??
	BEQ .TILE2.OFFSCREEN_CHECK
	CPX #$03								;IS TILE 3 THE CURRENT TILE??
	BEQ .TILE3.OFFSCREEN_CHECK

.TILE0.OFFSCREEN_CHECK
	CMP MAP_OBJECTS.X_APPROACH+$1
	BEQ .TILE.OFFSCREEN
	JMP .TILE_ONSCREEN
	
.TILE1.OFFSCREEN_CHECK
	CMP #MAP_OBJECTS.X.LAST_COLUMN
	BEQ .TILE.OFFSCREEN
	JMP .TILE_ONSCREEN

.TILE2.OFFSCREEN_CHECK
	CMP MAP_OBJECTS.X_APPROACH+$1
	BEQ .TILE.OFFSCREEN
	JMP .TILE_ONSCREEN

.TILE3.OFFSCREEN_CHECK
	CMP #MAP_OBJECTS.X.LAST_COLUMN
	BEQ .TILE.OFFSCREEN
	JMP .TILE_ONSCREEN	
	
	
.TILE_ONSCREEN
;IS TILE HIDDEN (DARKNESS)?	
	LDA SCREEN.DARK.DATA,Y			
	CMP #$01
	BEQ .MOB.PREMOVE_LOCATION.NOTVISIBLE2	;IF YES, DON'T DRAW TILE

		; LDA #$09							;SET TRACE
		; STA CALLED_BY			;(****not needed, just trace RTS address from the stack)
	JSR DRAW.TILE.SINGLE					;REPLACE MOB TILE WITH UNDERLYING MAP TILE
		; LDA #$00							;SET TRACE
		; STA CALLED_BY
		
.MOB.PREMOVE_LOCATION.NOTVISIBLE2	
	LDA #$FF
	STA SCREEN.MO_SPRITE.DATA,Y				;REMOVE MOB FROM MO SCREEN ARRAY
	STA SCREEN.MO_SPRITE_TYPE.DATA,Y
		;***OPT** Memory. Speed. it may be possible to remove .MOB.PREMOVE_LOCATION.NOTVISIBLE. SCREEN.MO_SPRITE.DATA is init to $FF at start of MO.DRAW. The location of the mob is recorded after the mob moves. Thus, if the pre-move location isn't visible, I'm not sure why we'd need to record that in the array. The erase routine is needed though as the graphic would have scrolled from the mob location prior to the player move. 

.TILE.OFFSCREEN
.EXIT_TEST
	INX
	CPX #$04
	BNE .MT.ERASELOOP
	
	LDA #$01
	STA MOB.MOVE.ERASE_COUNTER				;RECORD THAT TILE WAS ERASED THIS TURN SO IT'S NOT ERASED ON MOB'S SECOND MOVE, IF ITS A DOUBEL MOVER
	
	LDY MOB.MT.TILE_LOCATIONS				;RESTORE TILE0 (UPPER LEFT) TO THE Y-REG
	;LDX SAVED.XREG.LOCAL					;CONTAINS MOB RECORD INDEX

	;****FALLS THROUGH***
	
@END
@END
	
.MOB.MOVE.IMPLIMENT
@START
			
	LDA MOB.MOVE.CURRENT					;#$00=north, $01=south, $02=east, $03=west,  $04 = NO MOVE

	CMP #$00
	BEQ	.MOB.MOVE.NORTH_STEP
	CMP #$01
	BEQ .MOB.MOVE.SOUTH_STEP
	CMP #$02
	BEQ	.MOB.MOVE.EAST_STEP
	CMP #$03
	BEQ .MOB.MOVE.WEST_STEP
	CMP #$04
	BEQ MOB.MOVE.PASS
	CMP #$05			;STORMS ONLY
	BEQ	MOB.MOVE.NORTH_EAST
	
	JMP ERROR1

.MOB.MOVE.COMPLETE_STEP
	JMP MOB.MOVE.COMPLETE

.MOB.MOVE.NORTH_STEP
	JMP MOB.MOVE.NORTH
	
.MOB.MOVE.SOUTH_STEP
	JMP MOB.MOVE.SOUTH
	
.MOB.MOVE.EAST_STEP
	JMP MOB.MOVE.EAST
 
.MOB.MOVE.WEST_STEP
	JMP MOB.MOVE.WEST

MOB.MOVE.COMPLETE_STEP			
	JMP MOB.MOVE.COMPLETE
@END

	
MOB.MOVE.PASS		
@START
;CHECK TO SEE IF TILE SHOULD BE DRAWN (NORMALLY IT ISN'T ON A PASS)
;					NOTE: THIS BECAUSE THE MOB'S POSITION HASN'T CHANGED. HOWEVER, IF IT'S A DOUBLE MOVER'S SECOND MOVE WHEN THE SLOW PROGRESS/PASS OCCURS, IT'S POSITION HAS CHANGED. 	
;

;debug: door ($14) is set to #$24 here when X= $00 (NPC mode)
;debug: door ($C0) is set to #$10 here when X= $00 (NPC mode)




			
	LDA MOB.FLAG2
	CMP #$01									;is double mover flag set? 
	BEQ .DOUBLE.MOVER							;if yes, branch to special code section for double movers
			

	; ;IS TILE HIDDEN (DARKNESS)?	
	; LDA SCREEN.DARK.DATA,Y			
	; CMP #$01
	; BEQ MOB.MOVE.COMPLETE_STEP					;if yes, don't draw sprite

	;IS SPRITE AN OFFSCREEN SS?
	LDA MOB.SCREEN_STATUS.SS
	CMP #$01 						;IS SPRITE AN OFFSCREEN SS?
	BEQ MOB.MOVE.COMPLETE_STEP		;IF YES, INCREMENT INDEX. CALL IT A DAY.

	; ;IS OVERRIDE SET?
	; LDA SPRITE.DRAWTILE.OVERRIDE 
	; CMP #$01				;is override flag set? ($01) (it is set on a tranisting NPC's first transit turn (which is always a pass) and last saved path move so that NPC appears ladder tile for 1 turn if the ladder anchor movement file is set ($FF)		
	; BEQ MOB.DRAWTILE_STEP	;if yes, automaticall draw tile. 

	
	; ;(6/28/16, this was put back****FOR SOME REASON HAVING THIS ENABLED WAS CAUSING
	; ;NPC'S TO BE DRAWN TWICE, IF THEY HAD
	; ;$00 (PASS) SET FOR AT MOVE ANCHOR
	
	; ;IS SPRITE A MULTI-TILE MOB	?				;If yes, then draw tile without considering whether tile was hidden last turn (which assumes that the mob is fully drawn unless the tiles were hidden). This is so that MTT mobs that just moved fully onto the screen will get drawn even if they pass.
	; LDA MOB.FLAG3
	; CMP #$01									;is double mover flag set? 
	; BEQ MOB.DRAWTILE_STEP

			
	; ;WAS TILE HIDDEN LAST PLAYER TURN?
	; LDA SCREEN.DARK.DATA_BEFORE,Y			
	; CMP #$01 ;($01 = dark/hidden)
	; BNE MOB.MOVE.COMPLETE_STEP		;if no, don't draw sprite is it should already be visible

			
	JMP	MOB.DRAWTILE_STEP						;if yes, draw sprite tile

	
.DOUBLE.MOVER	
	;***NOTE: it is possible that double movers should also
	;have the darkness now/before check as non-double movers do above.
	;Since double movers are typically configured as aggressive, which never 
	;pass, I decided not to address this for now. 
	
	LDA MOB.MOVE.SLOW.PROGRESS 
	CMP #$01									;is, slow progress flag on ($01)?
	BNE MOB.MOVE.COMPLETE_STEP					;if no, complete move


			; LDA #$Ac
			; LDy SUMMON.QTY
			; jmp full.brk
			; ;JSR PREP.BRK
			; BRK	
			
	JMP MOB.DRAWTILE_STEP		;if yes, draw sprite tile		
@END
	
MOB.MOVE.NORTH_EAST	;STORMS ONLY 
@START
;STORMS ARE MTT OBJECTS (so effectively this routine is only used by MTT objects)

;UPDATE MOB Y	(+1 NORTH)
	DEC SPRITE.RECORD+$1				;-1 to y position (moved mob x,y north)
	DEC SPRITE.RECORD+$9
;UPDATE MOB X	(+1 EAST)
	INC SPRITE.RECORD					;+1 TO X POSITION (MOVED MOB X,Y EAST)
	INC SPRITE.RECORD+$8	

;DID SS MOB MOVE ONTO VIEW SCREEN?	
	LDA SPRITE.RECORD+$1				
	CMP #MAP_OBJECTS.Y.LAST_ROW				;IS MOB'S Y AXIS == LAST ROW, NET OF ITS MOVE NORTH?
	BNE .CHECK_OFFSCREEN					;IF NO, PROCEED NORMALLY

	;MAKE SURE IT'S X-AXIS IS ON THE SCREEN
	LDA SPRITE.RECORD
	CMP #MAP_OBJECTS.X_FLAG.LOWER			;IS OBJECT BEYOND LEFT EDGE OF SCREEN?
	BCC	.CHECK_OFFSCREEN					;IF YES, SS DIDN'T MOVE ONTO SCREEN. PROCEED TO NEXT CHECK
	
	CMP #MAP_OBJECTS.X_FLAG.UPPER			;IS OBJECT BEYOND RIGHT EDGE OF SCREEN?
	BCS	.CHECK_OFFSCREEN					;IF YES, SS DIDN'T MOVE ONTO SCREEN. PROCEED TO NEXT CHECK

	;MAKE SURE ITS Y-AXIS IS ON THE SCREEN BEFORE TURNING OFF FLAG
	LDA SPRITE.RECORD+$1
	CMP #MAP_OBJECTS.Y_FLAG.LOWER			;IS OBJECT BEYOND LOWER EDGE OF SCREEN?
	BCC	.CHECK_OFFSCREEN					;IF YES, SS DIDN'T MOVE ONTO SCREEN. PROCEED TO NEXT CHECK

	CMP #MAP_OBJECTS.Y_FLAG.UPPER			;IS OBJECT BEYOND UPPER EDGE OF SCREEN?
	BCS	.CHECK_OFFSCREEN					;IF YES, SS DIDN'T MOVE ONTO SCREEN. PROCEED TO NEXT CHECK

	
	LDA #$00								;IF NO, SS IS ONSCREEN TURN OFF THE OFFSCREEN SS FLAG
	STA MOB.SCREEN_STATUS.SS
	
	LDA #$10								;IF SS/DOUBLE MOVER MOB MOVES ONTO VIEW SCREEN ON ITS FIRST MOVE, IT FORFEITS ITS 2ND MOVE.
	STA MOB.MOVE.COUNTER

	
.CHECK_OFFSCREEN	
;DID MOB MOVE OFF VIEW SCREEN?

	;check north
	LDA SPRITE.RECORD+$1
	CMP #MAP_OBJECTS.Y_FLAG.LOWER
	BCC MOB.NOTONSCREEN_STEP2

	;check east
	LDA SPRITE.RECORD+$0
	CMP #MAP_OBJECTS.X_FLAG.UPPER
	BCS MOB.NOTONSCREEN_STEP2

.INCREMENT_SCREEN_TILE	
	INY 			;MOVE EAST
	TYA
	SEC
	SBC #SCREEN.ARRAY.OFFSET	;MOVE NORTH
	STA MAP_OBJECTS.TILE_LOCATION

	;**OPT. This section may be able to be removed since MTT objects are never double movers
	
; .INCREMENT_MAP_TILE
	LDA MAP_OBJECTS.MAP_LOCATION				;DOUBLE MOVES NEED THIS VALUE UPDATED
;	SEC
	SBC #OFFSET.DOWN
	STA MAP_OBJECTS.MAP_LOCATION

;MAKE FINAL DECISION ON WHETHER TO DRAW MOB'S TILE	
	
	LDA MOB.SCREEN_STATUS.SS
	CMP #$01 									;IS MOB AN OFFSCREEN SS?
	BNE MOB.DRAWTILE_STEP						;IF NO, DRAWTILE
	JMP MOB.MOVE.COMPLETE
	
MOB.NOTONSCREEN_STEP2
	JMP MOB.NOTONSCREEN	
@END

MOB.MOVE.NORTH
@START
;UPDATE MOB X/Y	
	DEC SPRITE.RECORD+$1				;-1 to y position (moved mob x,y north)
	DEC SPRITE.RECORD+$9				;-1 to GMAP.Y, this field of sprite record only used for npcs

;DID SS MOB MOVE ONTO VIEW SCREEN?	
	LDA SPRITE.RECORD+$1				
	CMP #MAP_OBJECTS.Y.LAST_ROW				;IS MOB'S Y AXIS == LAST ROW, NET OF ITS MOVE NORTH?
	BNE .CHECK_OFFSCREEN					;IF NO, PROCEED NORMALLY

	;MAKE SURE IT'S X-AXIS IS ON THE SCREEN
	LDA SPRITE.RECORD
	CMP #MAP_OBJECTS.X_FLAG.LOWER			;IS OBJECT BEYOND LEFT EDGE OF SCREEN?
	BCC	.CHECK_OFFSCREEN					;IF YES, SS DIDN'T MOVE ONTO SCREEN. PROCEED TO NEXT CHECK
	
	CMP #MAP_OBJECTS.X_FLAG.UPPER			;IS OBJECT BEYOND RIGHT EDGE OF SCREEN?
	BCS	.CHECK_OFFSCREEN					;IF YES, SS DIDN'T MOVE ONTO SCREEN. PROCEED TO NEXT CHECK
	
	LDA #$00								;IF NO, SS IS ONSCREEN TURN OFF THE OFFSCREEN SS FLAG
	STA MOB.SCREEN_STATUS.SS
	
	LDA #$10								;IF SS/DOUBLE MOVER MOB MOVES ONTO VIEW SCREEN ON ITS FIRST MOVE, IT FORFEITS ITS 2ND MOVE.
	STA MOB.MOVE.COUNTER

	
.CHECK_OFFSCREEN	
;DID MOB MOVE OFF VIEW SCREEN?
	LDA SPRITE.RECORD+$1
	CMP #MAP_OBJECTS.Y_FLAG.LOWER
	BCC MOB.NOTONSCREEN_STEP


.INCREMENT_SCREEN_TILE	
	TYA
	SEC
	SBC #SCREEN.ARRAY.OFFSET					;MOVE NORTH
	STA MAP_OBJECTS.TILE_LOCATION

; .INCREMENT_MAP_TILE
	LDA MAP_OBJECTS.MAP_LOCATION				;DOUBLE MOVES NEED THIS VALUE UPDATED
;	SEC
	SBC #OFFSET.DOWN
	STA MAP_OBJECTS.MAP_LOCATION

;MAKE FINAL DECISION ON WHETHER TO DRAW MOB'S TILE	
	
	LDA MOB.SCREEN_STATUS.SS
	CMP #$01 									;IS MOB AN OFFSCREEN SS?
	BNE MOB.DRAWTILE_STEP						;IF NO, DRAWTILE
	JMP MOB.MOVE.COMPLETE							;IF YES, INCREMENT INDEX. CALL IT A DAY.

@END	

MOB.DRAWTILE_STEP			
	JMP MOB.DRAWTILE
	
MOB.MOVE.SOUTH		
@START
.CHECK_ONSCREEN
;UPDATE MOB X/Y	
	INC SPRITE.RECORD+$1				;+1 TO Y POSITION (MOVED MOB X,Y SOUTH)
	INC SPRITE.RECORD+$9				;+1 to GMAP.Y, this field of sprite record only used for npcs

;DID SS MOB MOVE ONTO VIEW SCREEN?	
	LDA SPRITE.RECORD+$1				
	CMP #MAP_OBJECTS.Y.FIRST_ROW			;IS MOB'S Y AXIS == FIRST ROW, NET OF ITS MOVE SOUTH?
	BNE .CHECK_OFFSCREEN					;IF NO, PROCEED NORMALLY

	
	
	;MAKE SURE IT'S X-AXIS IS ON THE SCREEN
	LDA SPRITE.RECORD
	CMP #MAP_OBJECTS.X_FLAG.LOWER			;IS OBJECT BEYOND LEFT EDGE OF SCREEN?
	BCC	.CHECK_OFFSCREEN					;IF YES, SS DIDN'T MOVE ONTO SCREEN. PROCEED TO NEXT CHECK
	
	CMP #MAP_OBJECTS.X_FLAG.UPPER			;IS OBJECT BEYOND RIGHT EDGE OF SCREEN?
	BCS	.CHECK_OFFSCREEN					;IF YES, SS DIDN'T MOVE ONTO SCREEN. PROCEED TO NEXT CHECK
		
	
	LDA #$00								;IF NO, SS IS ONSCREEN TURN OFF THE OFFSCREEN SS FLAG
	STA MOB.SCREEN_STATUS.SS
	
	LDA #$10								;IF SS/DOUBLE MOVER MOB MOVES ONTO VIEW SCREEN ON ITS FIRST MOVE, IT FORFEITS ITS 2ND MOVE.
	STA MOB.MOVE.COUNTER

	
.CHECK_OFFSCREEN	
;DID MOB MOVE OFF VIEW SCREEN?
	LDA SPRITE.RECORD+$1
	CMP #MAP_OBJECTS.Y_FLAG.UPPER
	BCS MOB.NOTONSCREEN_STEP


	
.INCREMENT_SCREEN_TILE
	TYA
	CLC
	ADC #SCREEN.ARRAY.OFFSET				;MOVE SOUTH
	STA MAP_OBJECTS.TILE_LOCATION

; .INCREMENT_MAP_TILE
	LDA MAP_OBJECTS.MAP_LOCATION			;DOUBLE MOVES NEED THIS VALUE UPDATED
	CLC
	ADC #OFFSET.UP
	STA MAP_OBJECTS.MAP_LOCATION

;MAKE FINAL DECISION ON WHETHER TO DRAW MOB'S TILE
	LDA MOB.SCREEN_STATUS.SS
	CMP #$01 									;IS MOB AN OFFSCREEN SS?
	BNE MOB.DRAWTILE_STEP						;IF NO, DRAWTILE		
	JMP MOB.MOVE.COMPLETE							;IF YES, INCREMENT INDEX. CALL IT A DAY.
MOB.NOTONSCREEN_STEP
	JMP MOB.NOTONSCREEN

@END
	
MOB.MOVE.EAST
@START
		
;UPDATE MOB X/Y	
	INC SPRITE.RECORD					;+1 TO X POSITION (MOVED MOB X,Y EAST)
	INC SPRITE.RECORD+$8				;+1 to GMAP.X, this field of sprite record only used for npcs
				
;DID SS MOB MOVE ONTO VIEW SCREEN?	
	LDA SPRITE.RECORD				
	CMP #MAP_OBJECTS.X.FIRST_COLUMN			;IS MOB'S X AXIS == FIRST COLUMN, NET OF ITS MOVE EAST?
	BNE .CHECK_OFFSCREEN					;IF NO, PROCEED NORMALLY
	
	;MAKE SURE ITS Y-AXIS IS ON THE SCREEN BEFORE TURNING OFF FLAG
	LDA SPRITE.RECORD+$1
	CMP #MAP_OBJECTS.Y_FLAG.LOWER			;IS OBJECT BEYOND LOWER EDGE OF SCREEN?
	BCC	.CHECK_OFFSCREEN					;IF YES, SS DIDN'T MOVE ONTO SCREEN. PROCEED TO NEXT CHECK

	CMP #MAP_OBJECTS.Y_FLAG.UPPER			;IS OBJECT BEYOND UPPER EDGE OF SCREEN?
	BCS	.CHECK_OFFSCREEN					;IF YES, SS DIDN'T MOVE ONTO SCREEN. PROCEED TO NEXT CHECK
	
	LDA #$10								;IF SS/DOUBLE MOVER MOB MOVES ONTO VIEW SCREEN ON ITS FIRST MOVE, IT FORFEITS ITS 2ND MOVE.
	STA MOB.MOVE.COUNTER

	LDA #$00								;IF NO, SS IS ONSCREEN TURN OFF THE OFFSCREEN SS FLAG
	STA MOB.SCREEN_STATUS.SS

.CHECK_OFFSCREEN	
;DID MOB MOVE OFF VIEW SCREEN?
		;**OPT** Memory and Speed. The onscreen check above does this same comparison, maybe if the tests in onscreen were reordered, and the X-FLAG upper check did MOB.NOTONSCREEN, then this section could be eliminated. 
	LDA SPRITE.RECORD
	CMP #MAP_OBJECTS.X_FLAG.UPPER
	BCS MOB.NOTONSCREEN_STEP

.INCREMENT_SCREEN_TILE	
	INY										;MOVE EAST (WE JUST INCREMENTED X AXIS, AND YES THIS IS THE Y-REG, BUT THE Y-REG HOLDS THE SCREEN TILE #)
	STY MAP_OBJECTS.TILE_LOCATION
	
; .INCREMENT_MAP_TILE
	INC MAP_OBJECTS.MAP_LOCATION			;DOUBLE MOVES NEED THIS VALUE UPDATED

;MAKE FINAL DECISION ON WHETHER TO DRAW MOB'S TILE
	LDA MOB.SCREEN_STATUS.SS
	CMP #$01 									;IS MOB AN OFFSCREEN SS?
	BNE MOB.DRAWTILE							;IF NO, DRAWTILE
	JMP MOB.MOVE.COMPLETE							;IF YES, INCREMENT INDEX. CALL IT A DAY.
@END		
		
MOB.MOVE.WEST
@START
;UPDATE MOB X/Y	
	DEC SPRITE.RECORD					;-1 TO X POSITION (MOVED MOB X,Y WEST)
	DEC SPRITE.RECORD+$8				;-1 to GMAP.X, this field of sprite record only used for npcs

;DID SS MOB MOVE ONTO VIEW SCREEN?	
	LDA SPRITE.RECORD				
	CMP #MAP_OBJECTS.X.LAST_COLUMN			;IS MOB IN LAST COLUMN, NET OF ITS MOVE WEST?
	BNE .CHECK_OFFSCREEN					;IF NO, PROCEED NORMALLY

	;MAKE SURE IT'S X-AXIS IS ON THE SCREEN BEFORE TURNING OFF FLAG
	LDA SPRITE.RECORD+$1
	CMP #MAP_OBJECTS.Y_FLAG.LOWER			;IS OBJECT BEYOND LOWER EDGE OF SCREEN?
	BCC	.CHECK_OFFSCREEN					;IF YES, SS DIDN'T MOVE ONTO SCREEN. PROCEED TO NEXT CHECK
	
	CMP #MAP_OBJECTS.Y_FLAG.UPPER			;IS OBJECT BEYOND UPPER EDGE OF SCREEN?
	BCS	.CHECK_OFFSCREEN					;IF YES, SS DIDN'T MOVE ONTO SCREEN. PROCEED TO NEXT CHECK
	
	LDA #$00								;IF NO, SS IS ONSCREEN TURN OFF THE OFFSCREEN SS FLAG
	STA MOB.SCREEN_STATUS.SS

	LDA #$10								;IF SS/DOUBLE MOVER MOB MOVES ONTO VIEW SCREEN ON ITS FIRST MOVE, IT FORFEITS ITS 2ND MOVE.
	STA MOB.MOVE.COUNTER

.CHECK_OFFSCREEN
;DID MOB MOVE OFF VIEW SCREEN?
	
	LDA SPRITE.RECORD
	CMP #MAP_OBJECTS.X_FLAG.LOWER
	BCC MOB.NOTONSCREEN_STEP
	
.INCREMENT_SCREEN_TILE	
	DEY											;MOVE WEST (WE JUST INCREMENTED X AXIS, AND YES THIS IS THE Y-REG, BUT THE Y-REG HOLDS THE SCREEN TILE #)			
	STY MAP_OBJECTS.TILE_LOCATION

; .INCREMENT_MAP_TILE
	DEC MAP_OBJECTS.MAP_LOCATION				;DOUBLE MOVES NEED THIS VALUE UPDATED

;MAKE FINAL DECISION ON WHETHER TO DRAW MOB'S TILE	
	LDA MOB.SCREEN_STATUS.SS
	CMP #$01 									;IS MOB AN OFFSCREEN SS?
	BNE MOB.DRAWTILE							;IF NO, DRAWTILE
	JMP MOB.MOVE.COMPLETE							;IF YES, INCREMENT INDEX. CALL IT A DAY.
@END
@END
@END
@END


SPRITE.INITIATED.COMBAT
@START
;EVENTUALLY JSR OR JMP TO THE COMBAT SUBROUTINE, WHICH MIGHT BE IN B/S MEMORY.
;PROBABLY JMP. RE-ENTRY WILL NEED TO CALL DRAW.SCREEN
	
	
; .DEBUG.CASTLE_HANG ;see if this section is accessed when it shouldn't be, causing COMBAT_SE.MODE.PARM to be set to $01 
			; LDA #$E2
			; LDX #$E2
			; LDY #$E2
			; JMP FULL.BRK
			; BRK
			
	
	LDA #$10
	STA MOB.MOVE.COUNTER
	;JSR APPLE_BELL
	
	;set combat mode = mob initiated
	LDA #$01
	STA COMBAT_SE.MODE.PARM	;($00 = player initiated or init value in map objects manager  | $01 = mob initiated | $FF = test mode)

	;save map object info for attacking mob
	LDA SPRITE.RECORD+$B ;load S_ENTITY map object index
	STA COMBAT_SE.S_ENTITY.MO_INDEX

	LDA SPRITE.RECORD+$A ;load S_ENTITY type
	STA COMBAT_SE.S_ENTITY.TYPE

	LDA SPRITE.RECORD+$02 ;load tile type of S_ENTITY
	STA COMBAT_SE.MOB_TILE_ID.PARM 		;The Tile_ID of the MOB on the main map which attacked the player or the player attacked
	

	
					; LDA #$01
					; STA TROUBLESHOOTING.HOOK
					

			
				

			
	;****FALLS THROUGH
@END
	
MOB.DRAWTILE ;
@START			

					
					
					
			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; JSR KEYIN
			; cmp #$D1
			; beq .break
			; cmp #$f1
			; beq .break
			; jmp .temp
; .break
			; LDA #$Aa
			; ldx MOB.FLAG0
			; ldy MOB.FLAG3
			; jmp full.brk
; .TEMP
			; LDA TEMP
			
			
;ERROR CHECKING
	LDY MAP_OBJECTS.TILE_LOCATION
	CPY #SCREEN.ARRAY.LAST_ELEMENT			;IS TILE TO DRAW OFF THE SCREEN?
	BCS .MOB.MOVE.COMPLETE_STEP				;IF YES, THEN COMPLETE MOVE.
											;THERE IS 1 KNOWN CASE WHERE THIS OCCURS: A DOUBLE MOVER SS IS ON HILLS, HE'S BLOCKED IN THE DIRECTION OF THE PLAYER AND HE HIT SLOW PROGRESS ON THE HILLS. 

.START
	LDA MOB.FLAG2
	CMP #$01						;IS IF DOUBLE MOVER FLAG SET? 
	BNE MOB.DRAWTILE.ENTRANCE2		;IF NO, PROCEED WITH TILE DRAW
	LDA MOB.MOVE.COUNTER					
	CMP #$01						;IS THIS THE MOB'S 2ND MOVE? 
	BCC .MOB.MOVE.COMPLETE_STEP		;IF NO, COMPLETE MOVE AND RESTART MOVE LOOP
	JMP	MOB.DRAWTILE.ENTRANCE2		;IF YES, DRAWTILE


.MOB.MOVE.COMPLETE_STEP
	JMP MOB.MOVE.COMPLETE

MOB.DRAWTILE.MT_STEP
	JMP MOB.DRAWTILE.MT
	
MOB.DRAWTILE.ENTRANCE2
;ENTRANCE FOR INITIAL SCREEN DRAW, SO MOB MOVE COUNT CHECK IS SKIPPED
	
	
	LDA MOB.FLAG0					;MULTI-TILE MOB?
	CMP #$01
	BNE .SINGLE.TILE
	
	LDA MOB.FLAG3					;MULTI-TILE MOB?
	CMP #$01
	BNE .SINGLE.TILE


			
	JMP MOB.DRAWTILE.MT_STEP					;IF YES, USE THE MULTI-TILE DRAW ROUTINE

.SINGLE.TILE
	LDY MAP_OBJECTS.TILE_LOCATION 

	
;IS SPRITE AN NPC ON A DIFFERENT BUILDING FLOOR THAN PLAYER?
;(yes, if the NPC is not in transit and has a movement flag of $FF or if an NPC in transit)
	LDA SPRITE.RECORD+$6	;load transit flag
	CMP #$01				;is NPC in transit?
	BCS .CHECK.DARKNESS		;if yes, then NPC is not on a differnet floor. Proceed to draw tile.
	LDA SPRITE.DRAWTILE.OVERRIDE 
	CMP #$01				;is override flag set? ($01) (it is set on a tranisting NPC's last saved path move so that NPC appears ladder tile for 1 turn if the ladder anchor movement file is set ($FF)		
	BEQ .CHECK.DARKNESS		;if yes, automaticall draw tile. The overrside is set on a transitting NPC's last save path move. We want the NPC's tile to be drawn on the last move.
	LDA SPRITE.RECORD+$7	;load sprite movement flag
	CMP #$FF				;is sprite an npc on a different floor?
	BNE .NOT.NPC.ON.DIFFERENT.FLOOR	;if no, then proceed to draw tile	
	JMP MOB.MOVE.COMPLETE	;if yes, then complete move without drawing tile	

	
.NOT.NPC.ON.DIFFERENT.FLOOR

.CHECK.DARKNESS			
;IS TILE HIDDEN (DARKNESS)?	
	LDA SCREEN.DARK.DATA,Y			
	CMP #$01
	BEQ .MOB.NOTVISIBLE_STEP					;IF YES, DON'T DRAW TILE

	LDA SCREEN.TILE.DATA,Y				;LOAD MAP TILE MOB SITS ON	
.CHECK.SWAP ;**OPT** Memory. Speed. Put all these tile ID swaps in a range. If conditions match and terrain tile_ID is in range, then -1 from terrain tile_ID. Setup the tile IDs so that the swap is always -1 from the primary.
;CHECK FOR SWAP TILE
	;**OPT** Speed. I think the check for hostile NPCs should be moved to the building only checks section just below. 
	
	;CHECK FOR HOSTILE NPCs
	;STA SAVED.ACC.LOCAL				;save tile_type of sprite's screen location
	LDX SPRITE.RECORD+$6			;load transit flag
	CPX #$FF						;is it set to hostile?
	BEQ .DRAW.TILE					;if yes, draw sprite tile (a hostile NPC should be drawn even if standing on a swap tile such as a bed or cot. don't use the "occupied" version of the bed/cot tile)
	;**Falls Through**
	
.BUILDING.CHECKS ;building map type only
	;is player in building? 
	LDX PLAYER.MAP.LOCATION_TYPE
	; CPX #MAP.TYPE.TOWN_VILLAGE			;IS LOCATION TYPE = BUILDING?
	; BNE .BUILDING.CHECKS.DONE		;if no, skip rules in this section
	
	;is a building map active
	;**OPT** Memory. Speed. Right now there is only one code for location type, which is used to determine the tile_set to use and for other location-type specific features. There are a lot of the later. It might be a savings if the 
					;there were a tile-set code separate from location type code, and there was a default tile-set picked using the location-type code if tile-set code was set to default. This might require adding another byte to some hex tables
					;but I suspect it would pay off. 
	CPX #MAP.TYPE.BUILDING.GRE 	;is map type = building?
	BCC .BUILDING.CHECKS.DONE		;if no, skip rules in this section
	CPX #MAP.TYPE.BUILDING.LT	;is map type = building?
	BCS .BUILDING.CHECKS.DONE		;if no, skip rules in this section
	;**FALLS THROUGH**				;if yes

.CHECK.UNOCCUPIED.TILE_SWAPS
	CMP #TILE_ID.UNOCCUPIED.TILE.SWAP.GRE1	
	BCC .CHECK.UNOCCUPIED.TILE_SWAPS.DONE
	CMP #TILE_ID.UNOCCUPIED.TILE.SWAP.LT1	
	BCS .CHECK.UNOCCUPIED.TILE_SWAPS.DONE
	
	;is S_ENTITY passing through? If yes, then don't display occupied version of tile
	LDX SAVED.XREG.LOCAL	;restore map object array index
	PHA	;save terrain tile_type

	;I commented this test out because in some cases I want the NPC's random movement at-anchor to display the occupied tiles (example, wizard puttering behind the counter in magic shop)
	; LDA MAP_OBJECTS.NPC+$7,X ;load at-anchor movement code
	; BNE .NPC.NOT.STATIONARY  ;if code != $00 (stantionary), then occupied tile doesn't apply because the NPC is just passing through.

	LDA MAP_OBJECTS.NPC+$6,X ;load transit code
	BNE .NPC.NOT.STATIONARY  ;if code != $00 (not in transit), then occupied tile doesn't apply because the NPC is just passing through.
	PLA ;restore terrain tile_type		
	CLC
	ADC #$01 ;switch to the occupied tile
	JMP .DRAW.TILE.ENTRANCE2	
.NPC.NOT.STATIONARY
	PLA ;restore terrain tile_type
.CHECK.UNOCCUPIED.TILE_SWAPS.DONE
	
;****the following are occupied tile swap checks which haven't
;been setup in a range yet***

;IS MAP TERRAIN TILE A BED (LEFT SIDE)?
.CHECK.BED  
	;ACC: contains map terrain tile_ID of player screen location 
	CMP #TILE_ID.BED_LEFT_UNOCCUPIED
	BNE .CHECK.COT
	LDA #TILE_ID.BED_LEFT_OCCUPIED	;if no, use "occupied" version of the bed/cot tile
	JMP .DRAW.TILE.ENTRANCE2

.MOB.NOTVISIBLE_STEP
	JMP .MOB.NOTVISIBLE

	
;IS MAP TERRAIN TILE A COT?
.CHECK.COT
	;ACC: contains map terrain tile_ID of player screen location 
	CMP #TILE_ID.COT_UNOCCUPIED
	BNE .CHECK.OUTHOUSE_HOLE
	LDA #TILE_ID.COT_OCCUPIED		;if no, use "occupied" version of the bed/cot tile
	JMP .DRAW.TILE.ENTRANCE2

.CHECK.OUTHOUSE_HOLE
	;ACC: contains map terrain tile_ID of player screen location 
	CMP #TILE_ID.OUTHOUSE_HOLE.UNOCCUPIED
	BNE .CHECK.CROCODILE
	LDA #TILE_ID.OUTHOUSE_HOLE.OCCUPIED		;if no, use "occupied" version of the bed/cot tile
	JMP .DRAW.TILE.ENTRANCE2
.BUILDING.CHECKS.DONE

.CHECK.SWAP.ALL ;all map types
.CHECK.CROCODILE
	;is mob a crocodile?
	;ACC: contains map terrain tile_ID of player screen location 
	LDX SPRITE.RECORD+$2				;LOAD MOB TILE TYPE
	CPX #TILE_ID.CROC_A					;DOES MOB HAVE CROC PRIMARY TILE_ID?
	BNE .CHECK.CROCODILE.COMPLETE					;IF NO, DRAW TILE
	LDX SCREEN.TILE.DATA,Y				;LOAD MAP TILE MOB SITS ON
	CPX #TILE_ID.SURF					;IS CROC IN SHALLOW WATER?
	BNE .CHECK.CROCODILE.COMPLETE						;IF NO, CALCULATE SHAPE TABLE
	LDA #TILE_ID.CROC_B					;IF YES, SUBSTITUTE ALTERNATE CROC TILE_ID (IN WATER ENVIRONMENT) 
	JMP .DRAW.TILE.ENTRANCE2
.CHECK.CROCODILE.COMPLETE

;IS MAP TERRAIN TILE TALL GRASS? 
;(ONLY DRAW UPPER HALF OF MOB, SO LOWER HALF IS HIDDEN BY GRASS)	

	;ACC: contains map terrain tile_ID of player screen location 
	CMP #TILE_ID.TALL_GRASS_A
	BEQ .TALL_GRASS	
	CMP #TILE_ID.TALL_GRASS_B
    BNE .NOT.TALL_GRASS
.TALL_GRASS
	LDA #TILE.DEPTH.HALF				;ADJUST TILE DEPTH SO ONLY UPPER HALF OF TILE IS DRAWN.	
	STA TILE.DEPTH	

	LDA SPRITE.RECORD+$2				;LOAD MOB TILE TYPE
	STA SAVED_TILE_TYPE
			
			
		; LDA #$0A							;SET TRACE
		; STA CALLED_BY					;(****not needed, just trace RTS address from the stack)
	JSR DRAW.TILE.SINGLE
		; LDA #$00							;RESET TRACE
		; STA CALLED_BY
		
	LDA #TILE.DEPTH.STANDARD			;RESET TILE DEPTH TO FULL TILE. 		
	STA TILE.DEPTH	
	JMP .UPDATE.SCREEN_ARRAY	
.NOT.TALL_GRASS


.DRAW.TILE
	LDA SPRITE.RECORD+$2			;LOAD MOB TILE TYPE
.DRAW.TILE.ENTRANCE2	
	STA SAVED_TILE_TYPE
			
		; LDA #$0B							;SET TRACE
		; STA CALLED_BY					;(****not needed, just trace RTS address from the stack)	
	JSR DRAW.TILE.SINGLE
		LDA #$00							;SET TRACE
		;STA CALLED_BY
		STA SPRITE.DRAWTILE.OVERRIDE 			;clear override


	;**FALLS THROUGH**
.UPDATE.SCREEN_ARRAY	
.MOB.NOTVISIBLE
	LDA SAVED.XREG.LOCAL					;restore map object array index
	STA SCREEN.MO_SPRITE.DATA,Y				;save the index to the map objects record to the sprite screen array.
			
	LDA SPRITE.RECORD+$A
	STA SCREEN.MO_SPRITE_TYPE.DATA,Y		;save the sprite type (MOB,NPC etc.) to the sprite type screen array.


	JMP MOB.MOVE.COMPLETE
@END	

MOB.DRAWTILE.MT ;
@START
;UPDATE ALL TILE LOCATIONS FOR THE MULTI-TILE MOB

	LDY MAP_OBJECTS.TILE_LOCATION			;LOAD TILE LOCATION CALCUALTED IN MAIN ROUTINE ABOVE
	STY MOB.MT.TILE_LOCATIONS				;SAVE AS TILE #0 (UPPER LEFT) OF MT MOB

	INY
	STY MOB.MT.TILE_LOCATIONS+$1			;SAVE AS TILE #1 (UPPER RIGHT) OF MT MOB
	TYA
	CLC
	ADC #SCREEN.ARRAY.OFFSET
	STA MOB.MT.TILE_LOCATIONS+$3			;SAVE AS TILE #3 (LOWER RIGHT) OF MT MOB
	TAY
	DEY
	STY MOB.MT.TILE_LOCATIONS+$2			;SAVE AS TILE #2 (LOWER LEFT) OF MT MOB

;CALCUALTE THE TILE TYPES FOR EACH TILE OF MT MOB
	LDY SPRITE.RECORD+$2				;LOAD TILE TYPE OF MOB RECORD
	STY MOB.MT.TILE_TYPES
	INY
	STY MOB.MT.TILE_TYPES+$1
	INY
	STY MOB.MT.TILE_TYPES+$2
	INY
	STY MOB.MT.TILE_TYPES+$3
	
	LDA SPRITE.RECORD+$0				;LOAD X-AXIS (COLUMN) OF TILE0 OF MT MOB
	STA MOB.MT.POSITION.X				;SAVE FOR FUTURE USE, WHEN X/Y-REG ARE BOTH BUSY
	
	
	;STX SAVED.XREG.LOCAL				;save map objects array index						
	LDX #$00							;INIT LOOP COUNTER
.LOOP	
	LDY MOB.MT.TILE_LOCATIONS,X

;IS TILE ON THE VIEW SCREEN
;(normally this condition indicates an error, but with MT Mobs, it can happen legitimately if the mob is half on/off the screen. In that case, it's legit but the correct process is to not draw the offscreen tiles)	

	;IF TILE IS OFFSCREEN TO NORTH OR SOUTH, THE SCREEN TILE # WILL EXCEED THE LAST ARRAY ELEMENT
	LDA #SCREEN.ARRAY.LAST_ELEMENT
	CMP MOB.MT.TILE_LOCATIONS,X	
	BCS .CHECK.EAST_WEST
	JMP .TILE.OFFSCREEN
.CHECK.EAST_WEST
	;OFFSCREEN EAST/WEST REQUIRES CHECKING THE COLUMN 
	LDA MOB.MT.POSITION.X					;LOAD X-AXIS (COLUMN) OF TILE0 OF MT MOB. LOADING FROM VARIABLE BECAUSE X/Y-REG ARE BOTH IN USE	

	CPX #$00								;IS TILE 0 THE CURRENT TILE??
	BEQ .TILE0.OFFSCREEN_CHECK
	CPX #$01								;IS TILE 1 THE CURRENT TILE??
	BEQ .TILE1.OFFSCREEN_CHECK
	CPX #$02								;IS TILE 2 THE CURRENT TILE??
	BEQ .TILE2.OFFSCREEN_CHECK
	CPX #$03								;IS TILE 3 THE CURRENT TILE??
	BEQ .TILE3.OFFSCREEN_CHECK

.TILE0.OFFSCREEN_CHECK
	CMP MAP_OBJECTS.X_APPROACH+$1
	BEQ .TILE.OFFSCREEN
	JMP .TILE_ONSCREEN
	
.TILE1.OFFSCREEN_CHECK			
	CMP #MAP_OBJECTS.X.LAST_COLUMN
	BEQ .TILE.OFFSCREEN
	JMP .TILE_ONSCREEN

.TILE2.OFFSCREEN_CHECK
	CMP MAP_OBJECTS.X_APPROACH+$1
	BEQ .TILE.OFFSCREEN
	JMP .TILE_ONSCREEN

.TILE3.OFFSCREEN_CHECK
	CMP #MAP_OBJECTS.X.LAST_COLUMN
	BEQ .TILE.OFFSCREEN
	JMP .TILE_ONSCREEN
	
	
.TILE_ONSCREEN	
;IS TILE HIDDEN (DARKNESS)?	
	LDA SCREEN.DARK.DATA,Y			
	CMP #$01
	BEQ .TILE.NOTVISIBLE					;IF YES, DON'T DRAW TILE

;IS MOB UNDEAD LORD
	LDA SPRITE.RECORD+$2		;load current mob tile_ID
	CMP #TILE_ID.UNDEAD_LORD	;if current mob an undead lord?
	BNE .CHECK.TALL.GRASS		;if no, do tall grass check
	LDA EVENT.FLAGS+$0			;is event flag $0 ON?
	BEQ .CHECK.TALL.GRASS		;if not, do tall grass check
		;if yes, then mob drops fire as it walks

.DOES.IT.DROP.FIRE
	JSR RANDOM.8
	CMP #EVENT.UNDEAD_LORD.FIRE_DROP.PROB
	BCS .NO_FIRE
;MOB DROPS FIRE
	LDA #TILE_ID.FIRE_A			;load tile ID for fire
	STA SCREEN.TILE.DATA,Y		;change terrain type to fire
	JMP .NOT.TALL_GRASS			;skip tall grass check since we don't want the terrain type changed twice. 
	
.NO_FIRE ;MOB DOESN'T DROP FILE
	;**FALLLS THROUGH**

;IS MOB ON TALL GRASS?
.CHECK.TALL.GRASS
	LDA SCREEN.TILE.DATA,Y
	CMP #TILE_ID.TALL_GRASS_A
	BEQ .TALL_GRASS	
	CMP #TILE_ID.TALL_GRASS_B
    BNE .NOT.TALL_GRASS	
	;***FALLS THROUGH***
.TALL_GRASS	
	LDA #TILE_ID.GRASS
	STA SCREEN.TILE.DATA,Y
.NOT.TALL_GRASS

.DRAW.DO	
	LDA MOB.MT.TILE_TYPES,X					;LOAD NEXT MT TILE_TYPE
	STA SAVED_TILE_TYPE	

		; LDA #$0C							;SET TRACE
		; STA CALLED_BY						;(****not needed, just trace RTS address from the stack)
	JSR DRAW.TILE.SINGLE
		; LDA #$00							;RESET TRACE
		; STA CALLED_BY

.TILE.NOTVISIBLE
	;this section applies to visile tiles as well as those not visible
	LDA SAVED.XREG.LOCAL				;restore map objects array index						
	STA SCREEN.MO_SPRITE.DATA,Y			;save to sprite screen array to mark sprite's presence for other routines like animation

	;save to sprite_type screen array
	;(so that other routines (like animation) know whether to use the sprite screen array value as an index to the mob or NPC screen array)
		LDA SPRITE.RECORD+$A
		;XREG = MTT tile # (0 = upper left, 1 = upper right, 2 = lower left, 3 = lower right)
	JSR MAP_OBJECT.ADD.MTT_FLAG_BITS
		;ACC = S_ENTITY_TYPE (with flag bits)
		STA SCREEN.MO_SPRITE_TYPE.DATA,Y	
	
.TILE.OFFSCREEN
.EXIT_TEST
	INX
	CPX #$04
	BEQ .MTT.EXIT
	JMP .LOOP
	
.MTT.EXIT
	; BEQ .COW.EXIT
	; JMP .LOOP
; .COW.EXIT
		
;****FALLS THROUGH
@END

MOB.MOVE.COMPLETE ;
@START


				
;DOUBLE MOVER MOB CHECKS	
	LDA MOB.FLAG2
	CMP #$01								;is if double mover flag set? 
	BNE SAVE.SPRITE.RECORD					;if yes, switch between mob & npc mode, restart sprite loop
	INC MOB.MOVE.COUNTER
	
	LDA MOB.MOVE.COUNTER					
	CMP #$02								;is this the mob's 2nd move? (we're using #$02 this time becuase we just incremented the counter)
	BCS SAVE.SPRITE.RECORD					;if yes, switch between mob & npc mode, restart sprite loop
	JMP MOB.MOVEMENT						;if no, let mob move again. 

MOB.NOTONSCREEN
	;****FALLS THROUGH****
@END
@END

SAVE.SPRITE.RECORD
@START
;SPRITE.RECORD datagram	
;Byte 0						Byte 1						Byte 2		Byte 3			Byte 4			Byte 5		Byte 6		Byte 7						Byte 8			Byte 9			Byte $A
;player-relative.X of NPC	player-relative.Y of NPC	Tile_type	Set to $03 (SS)	Active Anchor	Path Index	In Transit?	at-anchor move routine flag	RMAP.X of NPC	RMAP.Y of NPC	Sprite_Type
;See Map Objects spreadsheet for authoratative version

;debug door $14: hook2 doesn't trigger when X= $00, NPC mode. !!!!X-REG doesn't contain the map object index here


;SAVE CURRENT SPRITE RECORD UPDATES TO THE MAP OBJECTS ARRAY
	LDX SAVED.XREG.LOCAL						;restore map object array index

;debug: door ($14) is set to #$24 here when X= $00 (NPC mode)
	

					
					
	LDA MANAGE_OBJECTS.NPC_MOB.RECORD.FLAG		;$00=mob record, $01=npc record
	CMP #$01 
	BEQ .SAVE.SPRITE.TO_NPC
	

	LDA SPRITE.RECORD+$8		
	STA MAP_OBJECTS.MOB+$0,X	;update GMAP.X

	LDA SPRITE.RECORD+$9
	STA MAP_OBJECTS.MOB+$1,X	;update GMAP.Y
	
	; LDA SPRITE.RECORD+$2
	; STA MAP_OBJECTS.MOB+$2,X

	; LDA SPRITE.RECORD+$3
	; STA MAP_OBJECTS.MOB+$3,X
			
;IS INDEX TYPE NPC & MOB OR MOB ONLY?
	LDA MANAGE_OBJECTS.NPC_MOB.INDEX.FLAG	 ;$00 = Mob & NPC, $01 = Mob only, 
	CMP #$01							;is index mob only?
	BNE .FLIP.RECORD.FLAG				;if no, then flip record flag to NPC mode		
	JMP INCREMENT_INDEX					;if yes, increment index
	
.FLIP.RECORD.FLAG	
	JMP FLIP.NPC_MOB.RECORD.FLAG
	
.SAVE.SPRITE.TO_NPC
@START
;SPRITE.RECORD datagram	
;Byte 0						Byte 1						Byte 2		Byte 3			Byte 4			Byte 5		Byte 6		Byte 7						Byte 8			Byte 9			Byte $A
;player-relative.X of NPC	player-relative.Y of NPC	Tile_type	Set to $03 (SS)	Active Anchor	Path Index	In Transit?	at-anchor move routine flag	RMAP.X of NPC	RMAP.Y of NPC	Sprite_Type
;See Map Objects spreadsheet for authoratative version

;MAP_OBJECTS.NPC datagram
;Byte 0	Byte 1	Byte 2		Byte 3		Byte 4			Byte 5			Byte 6				Byte 7
;GMAP.X	GMAP.Y	Tile_type	Set to $03	Active Anchor	Path Index($00)	In Transit? ($00)	at-anchor move routine flag
;See Map Objects spreadsheet for authoratative version


			
	LDA SPRITE.RECORD+$8
	STA MAP_OBJECTS.NPC+$0,X

	LDA SPRITE.RECORD+$9
	STA MAP_OBJECTS.NPC+$1,X


	
	;**SKIP BYTE $02
	
	;**SKIP BYTE $03
	
	LDA SPRITE.RECORD+$4
	STA MAP_OBJECTS.NPC+$4,X

	LDA SPRITE.RECORD+$5
	STA MAP_OBJECTS.NPC+$5,X

	LDA SPRITE.RECORD+$6
	STA MAP_OBJECTS.NPC+$6,X


;debug: door ($14) is set to #$24 here when X= $00 (NPC mode)

					
	;**SKIP BYTE $07
@END
		
@END	
	;****FALLS THROUGH****	

FLIP.NPC_MOB.RECORD.FLAG			
@START
;debug: door ($14) is set to #$10 here when X= $00




					
	LDY #$00
.LOOP									;since SPRITE.RECORD is larger than MAP.OBJECTS.MOB, we need to init SPRITE.RECORD on each iteration or left over data from MAP.OBJECTS.MOB could get leftover. 
	LDA #$00
	STA SPRITE.RECORD,Y
	INY
	CPY #SPRITE.RECORD.SIZE
	BNE .LOOP


			
	INC MANAGE_OBJECTS.NPC_MOB.RECORD.FLAG		;$00=mob iteration, $01=npc iteration, $02=next map object record

			; STA TEMP
			; ; LDA TROUBLESHOOTING.HOOK
			; ; CMP #$01
			; ; BNE .TEMP
			; lda CALLED_BY.DRAW.SCREEN
			; cmp #$01
			; beq .temp
			; lda sprite.record+$a
			; cmp #$03
			; bne .temp
			; lda sprite.record+$b
			; cmp #$00
			; bne .temp			

			; LDX SPRITE.RECORD+$03		;load move direction code
			; LDA MOB.MOVES.BLOCKED,X 	;is move direction blocked?
			; tay
			; lda #$ab
			; ;ldx COMBAT.PC.ACTIVE ;starts with $01, not $00
			; jmp full.brk
			; ;jsr prep.brk
			; brk
; .TEMP
			; lda temp


;DID MOB INITIATE COMBAT?	
	LDA COMBAT_SE.MODE.PARM	;($00 = player initiated or init value in map objects manager | $01 = mob initiated | $FF = test mode)
	BNE EXIT ;if yes, branch

			
	JMP SWITCH.MOB_NPC
@END
	;**FALLS THROUGH** ;The NPC record is always processed after the MOB record, so when NPC record processing is complete, it is time to increment the index
	
INCREMENT_INDEX
@START

					
					

;DID MOB INITIATE COMBAT?	
	LDA COMBAT_SE.MODE.PARM	;($00 = player initiated or init value in map objects manager | $01 = mob initiated | $FF = test mode)
	BNE EXIT ;if yes, branch



			; STA TEMP
			; ; LDA TROUBLESHOOTING.HOOK
			; ; CMP #$01
			; ; BNE .TEMP
			; lda CALLED_BY.DRAW.SCREEN
			; cmp #$01
			; beq .temp
			; lda sprite.record+$a
			; cmp #$03
			; bne .temp
			; lda sprite.record+$b
			; cmp #$00
			; bne .temp			

			; LDX SPRITE.RECORD+$03		;load move direction code
			; LDA MOB.MOVES.BLOCKED,X 	;is move direction blocked?
			; tay
			; lda #$ac
			; ;ldx COMBAT.PC.ACTIVE ;starts with $01, not $00
			; jmp full.brk
			; ;jsr prep.brk
			; brk
; .TEMP
			; lda temp
	
	LDX SAVED.XREG.LOCAL	;restore map object array index
	
;INCREMENT INDEX TO ALL MAP OBJECT ARRAYS. 			
	TXA
	CLC
	ADC #MAP_OBJECTS.RECORD_LENGTH			;record length
	BEQ EXIT								;if increment results in $00 then index flipped over, meaning the array is full
	TAX
	STX SAVED.XREG.LOCAL					;save map object array index

;UPDATE INDEX TYPE FLAG: IS NEXT INDEX MOB ONLY OR NPC & MOB?
;since the MOB and NPC map object arrays are not the same size, this flag tracks whether the index points to a MOB record only or a MOB & NPC Record. $00 = Mob & NPC, $01 = Mob only, 

	LDA MANAGE_OBJECTS.NPC_MOB.INDEX.FLAG
	CMP #$01
	BNE .SET.TO.ONE
	LDA #$00
	STA MANAGE_OBJECTS.NPC_MOB.INDEX.FLAG
	JMP .RETURN.TO.PRIMARY_LOOP
.SET.TO.ONE
	LDA #$01
	STA MANAGE_OBJECTS.NPC_MOB.INDEX.FLAG
	;**FALLS THROUGH**
	
.RETURN.TO.PRIMARY_LOOP	
			
	JMP PRIMARY_LOOP	

EXIT
;debug: door ($0C) is set to #$24 here


					
					

				

			; STA TEMP
			; ; LDA TROUBLESHOOTING.HOOK
			; ; CMP #$01
			; ; BNE .TEMP
			; lda CALLED_BY.DRAW.SCREEN
			; cmp #$01
			; beq .temp1
			; ; lda sprite.record+$a
			; ; cmp #$03
			; ; bne .temp1
			; ; lda sprite.record+$b
			; ; cmp #$00
			; ; bne .temp1			

			; LDX SPRITE.RECORD+$03		;load move direction code
			; LDA MOB.MOVES.BLOCKED,X 	;is move direction blocked?
			; tay
			; lda #$aa
			; LDx MANAGE_OBJECTS.NPC_MOB.INDEX.FLAG	 ;$00 = Mob & NPC, $01 = Mob only, 
			; ;ldx COMBAT.PC.ACTIVE ;starts with $01, not $00
			; jmp full.brk
			; ;jsr prep.brk
			; brk
; .TEMP1
			; lda temp

	RTS ;return to routine that called MO.DRAW
@END

;SPRITE MOVEMENT SUBROUTINES
PLAYER.HARASSMENT.CHECK
@START
;PARAMETERS: Y-REG (sprite candidate move direction code)
	LDA SPRITE.RECORD+$6	;load transit flag (should never be set for a MOB sprite type)
	CMP #$01		;is sprite an NPC in transit?
	BCC .EXIT		;if no, then exit	
					;if yes, then check to see if player is adjacent to NPC
					
.IDENTIFY.ADJACENT.PLAYER.TILE
	CPY #$00
	BEQ	.BLOCKED_MOVE.IS.NORTH
	CPY #$01
	BEQ .BLOCKED_MOVE.IS.SOUTH
	CPY #$02
	BEQ	.BLOCKED_MOVE.IS.EAST
	CPY #$03
	BEQ .BLOCKED_MOVE.IS.WEST
	;if no valid value default to North. Probably a bad idea but what the hell. 
.BLOCKED_MOVE.IS.NORTH
	LDA #SCREEN.ARRAY.ADJACENT_SOUTH	 
	JMP .IS.PLAYER.ADJACENT

.BLOCKED_MOVE.IS.SOUTH
	LDA #SCREEN.ARRAY.ADJACENT_NORTH	
	JMP .IS.PLAYER.ADJACENT

.BLOCKED_MOVE.IS.EAST
	LDA #SCREEN.ARRAY.ADJACENT_WEST		
	JMP .IS.PLAYER.ADJACENT

.BLOCKED_MOVE.IS.WEST
	LDA #SCREEN.ARRAY.ADJACENT_EAST	
	JMP .IS.PLAYER.ADJACENT
	
.IS.PLAYER.ADJACENT
	;ACC = TILE ADJACENT TO PLAYER FOR THE DIRECTION CODE OF THE CANDIDATE MOVE
	CMP MAP_OBJECTS.TILE_LOCATION	;is player in the tile adjacent to NPC? (this variable contains the NPC screen tile location)
	BNE .EXIT						;if no, player is not blocking NPC
	
	LDA PLAYER.MOVE.CURRENT
	CMP #$04	;did player pass this turn
	BEQ .EXIT	;if yes, then don't count as player blocked NPC because it wasn't intentional. NPC just randomly picked the direction the player was in. 

	INC PLAYER.BLOCKED.NPC.COUNTER	;if yes, player is blocking NPC!
				
	LDA PLAYER.BLOCKED.NPC.COUNTER
	CMP #PLAYER.BLOCKED.NPC.THRESHOLD ;has player blocked NPC enough times for NPC to react verbally? (note: this counter applies to all NPCs, not individual NPCs, but usually the threshold will only be reached if the player is intentionally trying to block the path of a specific NPC)
	BCC .EXIT						;if no, exit
	JMP .NPC.TELLS.PLAYER.TO.STICK.IT ;if yes, well you see here this is going....
	

.EXIT
	RTS

.NPC.TELLS.PLAYER.TO.STICK.IT

	;RESET COUNTER
	LDA #$00
	STA PLAYER.BLOCKED.NPC.COUNTER

	
;PRINT <BLANK ROW>
	LDA #$7
	STA HTAB	
	LDA #$5
	STA VTAB
	JSR	UPDATE.CHAR.POS
		
	JMP .TEXT0.PRINT

.TEXT0 .AZ -/                          /			;ASCII text string
.TEXT0.PRINT
		LDA #.TEXT0					
		STA STRING
		
		LDA /.TEXT0
		STA STRING+$1						
	JSR PRINT.STR

	
;PRINT <BLANK ROW>
	LDA #$7
	STA HTAB	
	LDA #$6
	STA VTAB
	JSR	UPDATE.CHAR.POS
		
	JMP .TEXT1.PRINT

.TEXT1 .AZ -/                               /			;ASCII text string
.TEXT1.PRINT
		LDA #.TEXT1					
		STA STRING
		
		LDA /.TEXT1
		STA STRING+$1						
	JSR PRINT.STR

;PRINT <BLANK ROW>
	LDA #$7
	STA HTAB	
	LDA #$7
	STA VTAB
	JSR	UPDATE.CHAR.POS
		
	JMP .TEXT2.PRINT

.TEXT2 .AZ -/                               /			;ASCII text string
.TEXT2.PRINT
		LDA #.TEXT2					
		STA STRING
		
		LDA /.TEXT2
		STA STRING+$1						
	JSR PRINT.STR

;PRINT <BLANK ROW>
	LDA #$7
	STA HTAB	
	LDA #$8
	STA VTAB
	JSR	UPDATE.CHAR.POS
		
	JMP .TEXT2a.PRINT

.TEXT2a .AZ -/                               /			;ASCII text string
.TEXT2a.PRINT
		LDA #.TEXT2a					
		STA STRING
		
		LDA /.TEXT2a
		STA STRING+$1						
	JSR PRINT.STR

	
;PRINT "MOVE!!!"	
	LDA #$7
	STA HTAB	
	LDA #$6
	STA VTAB
	JSR	UPDATE.CHAR.POS
		
	JMP .TEXT3.PRINT


.TEXT3 .AZ -/ Beat it!!!/			;ASCII text string
.TEXT3.PRINT
		LDA #.TEXT3					
		STA STRING
		
		LDA /.TEXT3
		STA STRING+$1						
	JSR PRINT.STR

;PRINT "MOVE!!!"	
	LDA #$7
	STA HTAB	
	LDA #$7
	STA VTAB
	JSR	UPDATE.CHAR.POS
		
	JMP .TEXT4.PRINT
	
.TEXT4 .AZ -/ Or I'll call the brute squad!!/
			;ASCII text string
.TEXT4.PRINT
		LDA #.TEXT4					
		STA STRING
		
		LDA /.TEXT4
		STA STRING+$1						
	JSR PRINT.STR
	
	
;FORCE PASS MOVE SO PLAYER CAN BETTER SEE WHY NPC TOL
;HIM/HER TO STICK IT

	
	;REMOVE RTS FROM STACK ADDED FROM JSR PLAYER.HARASSMENT.CHECK
	PLA 
	PLA
	
	JMP SPRITE.PASS.MOVE.ENTRANCE
	
	;JMP .EXIT
	
@END




ERROR1
@START
;MO.DRAW DETECTS INVALID VALUE IN MOVE.COMMAND, or .MOB.MOVE.IMPLIMENT detects invalid
;value in MOB.MOVE.CURRENT,
;OR .APPLY.PLAYER_MOVE (GENERAL.ENTRANCE) deteced invalid value in PLAYER.MOVE
;OR .APPLY.PLAYER_MOVE (SPRITE.ENTRANCE) deteced invalid value in PLAYER.MOVE
	
	;LDA #$A1
	JSR FULL.BRK
	
@END
@END
@END


;TWO METHODS FOR HANDLING MULTI-TILE FRIGATES THAT WEREN'T USED	
@START

;=================================MAUALLY ADJUST RMAP METHOD==================

; ;IDENTIFY WHICH TILE OF THE SHIP THE PLAYER WAS LOCATED ON WHEN EXECUTING BOARD COMMAND	
; ;	LDA SAVED.ACC.LOCAL							;LOAD TILE TYPE OF OBJECT BEING BOARDED
	; LDA MAP_OBJECTS.GENERAL,X					;LOAD X-AXIS
	; CMP #$7F
	; BEQ .X7F
	; CMP #$80
	; BEQ .X80
	
; .ERROR2
; ;FRIGATE X-AXIS IN UNEXPECTED POSITION IN .CENTER_FRIGATE_ONSCREEN
	; TAY
	; LDA TEXT
	; BRK
	

; .X7F
	; LDA MAP_OBJECTS.GENERAL+$1,X					;LOAD Y-AXIS
	; CMP #$7F
	; BEQ .X7F.Y7F
	; CMP #$80
	; BEQ .X7F.Y80

; .ERROR3
; ;FRIGATE Y-AXIS IN UNEXPECTED POSITION IN .CENTER_FRIGATE_ONSCREEN
	; LDA TEXT
	; BRK
	
; .X80
	; LDA MAP_OBJECTS.GENERAL+$1,X					;LOAD Y-AXIS
	; CMP #$7F
	; BEQ .X80.Y7F
	; CMP #$80
	; BEQ .X80.Y80

; .ERROR4
; ;FRIGATE Y-AXIS IN UNEXPECTED POSITION IN .CENTER_FRIGATE_ONSCREEN
	; LDA TEXT
	; BRK
	

; .X80.Y80
; .NW ; PLAYER IS LOCATED ON THE NORTHWEST TILE OF THE FRIGATE
	; ;ALREADY CENTERED
	; JMP BOARD.EXIT
	
; .X7F.Y80	
; .NE ; PLAYER IS LOCATED ON THE NORTHEAST TILE OF THE FRIGATE
	 
	; DEC RMAP.X							;RMAP.X/Y ARE THE PLAYERS X,Y WITHIN THE REGIONAL MAP, WHICH ARE USED TO TRIGGER ZONE TRANSITION. 

; ;16-BIT SUBTRACT (REQUIRED, PROBLEMS WITH DEC WITH FLIP DETECT)
	; LDA RMAP					;UPDATE POSITION IN REGIONAL MAP ARRAY
	; SEC
	; SBC #$01
	; STA RMAP
	; LDA RMAP+$1
	; SBC #$00
	; STA RMAP+$1
	
; .NOFLIP

; ;=======THE FOLLOWING ARE UPDATES THAT OCCUR DURING A PLAYER MOVE======	
; ;UPDATE SMAP/SMAP.CURRENT
	; LDA RMAP					;LOAD TILE_ID OF CURRENT MAP POSITION (CENTER/PLAYER TILE)
	; STA OP1
	; LDA RMAP+$1
	; STA OP1+$1
	
	; LDA #OFFSET.SCREEN			;CALCULATE TILE_ID OF TILE IN UPPER LEFT CORNER OF SCREEN
	; STA OP2
	; LDA /OFFSET.SCREEN
	; STA OP2+$1
	
	; JSR SBC.16					;RMAP(2) - SCREEN.OFFSET.LO/HO (2)
			
	; LDA RESULT					;SAVE TILE_ID OF UPPER LEFT SCREEN TILE
	; STA SMAP
	; STA SMAP.CURRENT
	; LDA RESULT+$1
	; STA SMAP+$1
	; STA SMAP.CURRENT+$1

; ;UPDATE MOB (SS) REGIONAL MAP FLAGS
	; DEC MAP_OBJECTS.SS.X_FLAG.LOWER		;ADJUST FLAGS USED TO DETERMINE IF AN SS IS LOCATED ON THE REGIONAL MAP. IT CHANGES WITH EACH PLAYER MOVE BECAUSE MOB'S X,Y IS TRACKED RELATIVE TO THE PLAYER
	; INC MAP_OBJECTS.SS.X_FLAG.UPPER
; ;================================END STANDARD PLAYER MOVE UPDATES=========================


; ;UPDATE ALL MAP OBJECTS TO REFLECT ADJUSTMENT TO PLAYER LOCATION 

	; LDX #$00
; .LOOP.UPDATE.MAP_OBJECTS
	; INC MAP_OBJECTS.GENERAL,X				;+1 TO X POSITION
	; INC MAP_OBJECTS.MOB,X					;+1 TO X POSITION

; ;NEXT MAP OBJECT RECORD		
	; TXA
	; CLC
	; ADC #MAP_OBJECTS.RECORD_LENGTH			;RECORD LENGTH
	; BEQ .UPDATE.COMPLETE					;IF INCREMENT RESULTS IN $00 THEN INDEX FLIPPED OVER, MEANING THE ARRAY IS FULL
	; TAX	
											
; ;EXIT TEST
; ;(test each map array for $00 in the first field of the current record, if all arrays are at end, then exit)
; ;Note: it doesn't hurt for the x/y records to be updated in empty record (this will happen for the array that is less full), because it is the tile_id (3rd byte) that is used for the stop value
	; LDA MAP_OBJECTS.GENERAL+$2,X			;AT END OF ARRAY?
	; CMP #$00	
	; BNE .LOOP.UPDATE.MAP_OBJECTS			;IF NO, CONTINUE LOOP
	; LDA MAP_OBJECTS.MOB+$2,X				;AT END OF ARRAY?
	; CMP #$00	
	; BNE .LOOP.UPDATE.MAP_OBJECTS			;IF NO, CONTINUE LOOP

; .UPDATE.COMPLETE

	; PLA									;REMOVE RTS ADDED TO THE STACK WHEN MO.BOARD WAS CALLED
	; PLA

	; JMP GAME.LAUNCH						;REDRAW SCREEN WITH FRIGATE IN THE CENTER

	
; .X7F.Y7F	
; .SE ; PLAYER IS LOCATED ON THE SOUTHEAST TILE OF THE FRIGATE

	 
	; DEC RMAP.X							;RMAP.X/Y ARE THE PLAYERS X,Y WITHIN THE REGIONAL MAP, WHICH ARE USED TO TRIGGER ZONE TRANSITION. 
	; DEC RMAP.Y							;RMAP.X/Y ARE THE PLAYERS X,Y WITHIN THE REGIONAL MAP, WHICH ARE USED TO TRIGGER ZONE TRANSITION. 
	
; ;16-BIT SUBTRACT (REQUIRED, PROBLEMS WITH DEC WITH FLIP DETECT)
	; LDA RMAP					;UPDATE POSITION IN REGIONAL MAP ARRAY
	; SEC
	; SBC #$01
	; STA RMAP
	; LDA RMAP+$1
	; SBC #$00
	; STA RMAP+$1

	; LDA RMAP					;UPDATE POSITION IN REGIONAL MAP ARRAY
	; SEC
	; SBC #SCREEN.ARRAY.OFFSET
	; STA RMAP
	; LDA RMAP+$1
	; SBC #$00
	; STA RMAP+$1
	
; .NOFLIP

; ;=======THE FOLLOWING ARE UPDATES THAT OCCUR DURING A PLAYER MOVE======	
; ;UPDATE SMAP/SMAP.CURRENT
	; LDA RMAP					;LOAD TILE_ID OF CURRENT MAP POSITION (CENTER/PLAYER TILE)
	; STA OP1
	; LDA RMAP+$1
	; STA OP1+$1
	
	; LDA #OFFSET.SCREEN			;CALCULATE TILE_ID OF TILE IN UPPER LEFT CORNER OF SCREEN
	; STA OP2
	; LDA /OFFSET.SCREEN
	; STA OP2+$1
	
	; JSR SBC.16					;RMAP(2) - SCREEN.OFFSET.LO/HO (2)
			
	; LDA RESULT					;SAVE TILE_ID OF UPPER LEFT SCREEN TILE
	; STA SMAP
	; STA SMAP.CURRENT
	; LDA RESULT+$1
	; STA SMAP+$1
	; STA SMAP.CURRENT+$1

; ;UPDATE MOB (SS) REGIONAL MAP FLAGS
	; DEC MAP_OBJECTS.SS.X_FLAG.LOWER		;ADJUST FLAGS USED TO DETERMINE IF AN SS IS LOCATED ON THE REGIONAL MAP. IT CHANGES WITH EACH PLAYER MOVE BECAUSE MOB'S X,Y IS TRACKED RELATIVE TO THE PLAYER
	; INC MAP_OBJECTS.SS.X_FLAG.UPPER
	
	
; ;================================END STANDARD PLAYER MOVE UPDATES=========================


; ;UPDATE ALL MAP OBJECTS TO REFLECT ADJUSTMENT TO PLAYER LOCATION 

	; LDX #$00
; .LOOP.UPDATE.MAP_OBJECTS
	; INC MAP_OBJECTS.GENERAL,X				;+1 TO X POSITION
	; INC MAP_OBJECTS.MOB,X					;+1 TO X POSITION

; ;NEXT MAP OBJECT RECORD		
	; TXA
	; CLC
	; ADC #MAP_OBJECTS.RECORD_LENGTH			;RECORD LENGTH
	; BEQ .UPDATE.COMPLETE					;IF INCREMENT RESULTS IN $00 THEN INDEX FLIPPED OVER, MEANING THE ARRAY IS FULL
	; TAX	
											
; ;EXIT TEST
; ;(test each map array for $00 in the first field of the current record, if all arrays are at end, then exit)
; ;Note: it doesn't hurt for the x/y records to be updated in empty record (this will happen for the array that is less full), because it is the tile_id (3rd byte) that is used for the stop value
	; LDA MAP_OBJECTS.GENERAL+$2,X			;AT END OF ARRAY?
	; CMP #$00	
	; BNE .LOOP.UPDATE.MAP_OBJECTS			;IF NO, CONTINUE LOOP
	; LDA MAP_OBJECTS.MOB+$2,X				;AT END OF ARRAY?
	; CMP #$00	
	; BNE .LOOP.UPDATE.MAP_OBJECTS			;IF NO, CONTINUE LOOP

; .UPDATE.COMPLETE

	; PLA									;REMOVE RTS ADDED TO THE STACK WHEN MO.BOARD WAS CALLED
	; PLA

	; JMP GAME.LAUNCH						;REDRAW SCREEN WITH FRIGATE IN THE CENTER

	
; .X80.Y7F
; .SW ; PLAYER IS LOCATED ON THE SOUTHWEST TILE OF THE FRIGATE




;=========================TILE LOCATION IN VARIABLES METHOD===============
	
; .SET.MT.TILE_LOCATIONS
	; ;CALCULATE ALL 4 SCREEN LOCATIONS OF THE FRIGATE AND SAVE FOR USE BY OTHER ROUTINES LIKE DRAW.TILE.PLAYER
	; LDY #SCREEN.ARRAY.PLAYER_LOCATION
	; STY PLAYER.TRANSPORT.MT.TILE_LOCATIONS
	; INY
	; STY PLAYER.TRANSPORT.MT.TILE_LOCATIONS+$1
	; TYA
	; CLC
	; ADC #SCREEN.ARRAY.OFFSET
	; STA PLAYER.TRANSPORT.MT.TILE_LOCATIONS+$2
	; TAY
	; DEY
	; STA PLAYER.TRANSPORT.MT.TILE_LOCATIONS+$3

	
; ;STORE TILE #S ADJACENT TO FRIGATE IN AN ARRAY. USED BY MOVEMENT_MANAGER.ASM TO APPLY COLLISION RULES
; ;
; ;The following Diagram illustrates the MT OBJECTS Tiles ($0-$3) and the adjacent tiles to be 
; ;checked for collision ($0-$7), which are mappe to TRANSPORT.MT.ADJACENT_TILES by the routine below. If any tile in the directional group (i.e. $0-$1 is the north group)
; ;then the move is treated as blocked by MOVEMENT_MANAGER.ASM	
; ;	  01
; ;  6014
; ;	 7235
; ;	  23
; ;
; ;The following routine uses the upper left tile of the MT OBJECTS as the starting point and
; ;uses offsets to calculate the screen tile #s of each of the adjacent tiles. 

	; ;NORTH GROUP
	; LDA	TRANSPORT.MT.TILE_LOCATIONS+$0
	; SEC
	; SBC #SCREEN.ARRAY.OFFSET
	; TAY
	; STY TRANSPORT.MT.ADJACENT_TILES+$0
	; INY
	; STY TRANSPORT.MT.ADJACENT_TILES+$1
	; ;EAST GROUP
	; LDY MOB.MT.TILE_LOCATIONS+$3
	; INY
	; STY TRANSPORT.MT.ADJACENT_TILES+$5
	; TYA
	; SBC #SCREEN.ARRAY.OFFSET
	; TAY
	; STY TRANSPORT.MT.ADJACENT_TILES+$4
	; ;WEST GROUP	
	; LDY	MOB.MT.TILE_LOCATIONS+$2
	; DEY 
	; STY TRANSPORT.MT.ADJACENT_TILES+$7
	; TYA
	; SBC #SCREEN.ARRAY.OFFSET
	; TAY
	; STY TRANSPORT.MT.ADJACENT_TILES+$6	
	; ;SOUTH GROUP					;SOUTH COMES LAST BECAUSE IT CLEARS THE CARRY FLAG. THIS WAY ONLY ONE SEC IS NEEDED ABOVE, SINCE NO UNDERFLOWS ARE EXPECTED
	; LDA	MOB.MT.TILE_LOCATIONS+$2
	; CLC
	; ADC #SCREEN.ARRAY.OFFSET
	; TAY
	; STY MOB.MT.ADJACENT_TILES+$2
	; INY
	; STY MOB.MT.ADJACENT_TILES+$3
	
	; JMP BOARD.EXIT
	
@END
		
	
RUN.HOOK	
			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK2
			; CMP #$01
			; BNE .TEMP
			; ; LDA SPRITE.RECORD+$A	;load the S_ENTITY type
			; ; CMP #S_ENTITY_TYPE.PC	;is S_ENTITY a player character?
			; ; BNE .TEMP
			; ; LDA SPRITE.RECORD+$B	;load the S_ENTITY type
			; ; BNE .TEMP
			; jsr keyin
			; cmp #$d1
			; bne .temp
			
			; ; LDX #$00
; ; .DEBUG.LOOP			
			; ; LDA $EB00,X
			; ; STA $BE00,X
			; ; INX
			; ; BNE .DEBUG.LOOP

			; ; LDX #$00
; ; .DEBUG.LOOP2		
			; ; LDA $D500,X
			; ; STA $BF00,X
			; ; INX
			; ; BNE .DEBUG.LOOP2
			
			; LDA #$AA
			; LDx SPRITE.RECORD+$A	;load the S_ENTITY type
			; LDy SPRITE.RECORD+$b	;load the S_ENTITY type
			
			; JSR FULL.BRK
; .TEMP
			; LDA TEMP
	; rts
	