; ============================================
; Copyright (C) 2016-2018. 6502 Workshop, LLC
; contact: mark@6502workshop.com
; ============================================


;************************INCLUDE FILE*****************************
;(do not assemble stand-alone.  )

;=====================EVENT_MANAGER.ASM DOCUMENTATION====================================
;
;
;====EVENT LIST====
;
;*Sunrise/Sunset
;
;*TILE_SWAP: Sunrise/Sunset
;	The purpose of this event is to change the color of certain tiles (currently floor and street) by swapping the 
;	shape tables associated with specific tile IDs. The swap is initiated by CHECK.SUN.STATUS when sunrise or sunset 
;	first begins. No screen draw is done at first to force the new tile shapes onto the screen. As the sunrise/set 
;	phases play out, only a few tiles with the new shape table are drawn as screen scrolling handles most of the map
;	movement. This makes the introduction of the new tile shapes gradual. Then, in the last phase of the 
;	sunrise/sunset, a full screen draw is done via TILE_SWAP.SUNRISE_SUNSET.SCREEN_DRAW to complete the process.  
;	TILE_SWAP.INIT.SUNRISE_SUNSET needs to be run at certains times as well, see it's docs for more details. 
;
;=================================================================================

EVENT.MANAGER
@START

			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK2
			; CMP #$02
			; BNE .TEMP			
			; LDA PLAYER.MAP.LOCATION_TYPE
			; CMP #$02
			; BEQ .TEMP
			; LDA #$AA
			; ldx PLAYER.MAP.LOCATION_TYPE			
			; JSR FULL.BRK
; .TEMP
			; LDA TEMP

			
			

CHECK.SUN.STATUS
@START
.CHECK.FOR.LOCATION.EXEMPTION
;DOES CURRENT LOCATION TYPE HAVE SUNRISE/SUNET?
	LDA PLAYER.MAP.LOCATION_TYPE
	CMP #MAP.TYPE.UNDERMAP			;Undermap locations are excempt; if true then exit
	BEQ .NEXT.EVENT


.CHECK.SUNRISE
	LDA TIME.CURRENT.HOUR				;check if the 24th game clock == the constants that designate the time that sunrise occurs
	CMP #EVENT.SUNRISE.HOUR
	BNE .CHECK.SUNSET
	LDA TIME.CURRENT.MINUTE
	CMP #EVENT.SUNRISE.MINUTE
	BNE .CHECK.SUNSET
	
	;ACTIVATE SUNRISE
	LDA #$00
	STA TIME.SUN.STATUS
	STA TIME.SUN.COUNTER
	STA TIME.SUN.SUB_COUNTER

	;SET DAY TILES
	JSR TILE_SWAP.SUN.SET_DAY ;use day tiles (red)

	JMP .NEXT.EVENT
	
.CHECK.SUNSET
	LDA TIME.CURRENT.HOUR				;check if the 24th game clock == the constants that designate the time that sunset occurs
	CMP #EVENT.SUNSET.HOUR
	BNE .NEXT.EVENT
	LDA TIME.CURRENT.MINUTE
	CMP #EVENT.SUNSET.MINUTE
	BNE .NEXT.EVENT

	
	;ACTIVATE SUNSET
	LDA #$02
	STA TIME.SUN.STATUS	
	LDA #$04
	STA TIME.SUN.COUNTER
	LDA #$00
	STA TIME.SUN.SUB_COUNTER

	;SET NIGHT TILES	
	JSR TILE_SWAP.SUN.SET_NIGHT ;use night tiles (blue)

.NEXT.EVENT

			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK2
			; CMP #$02
			; BNE .TEMP			
			; LDA PLAYER.MAP.LOCATION_TYPE
			; CMP #$02
			; BEQ .TEMP2
			; LDA #$AB
			; ldx PLAYER.MAP.LOCATION_TYPE			
			; JSR FULL.BRK
; .TEMP2
			; LDA TEMP

			
@END
	;**FALLS THROUGH**
CHECK.NPC.SCHEDULER.INTERVAL
@START
;.CHECK.FOR.LOCATION.EXEMPTION
	LDA PLAYER.MAP.LOCATION_TYPE		
	; CMP #MAP.TYPE.TOWN_VILLAGE
	; BEQ .START
	;JMP .NEXT.EVENT
	
	;is a building map active
	LDA PLAYER.MAP.LOCATION_TYPE
	CMP #MAP.TYPE.BUILDING.GRE 	;is map type = building?
	BCC .NEXT.EVENT				;if no
	CMP #MAP.TYPE.BUILDING.LT	;is map type = building?
	BCS .NEXT.EVENT				;if no
	;**FALLS THROUGH**	 		;if yes
	


.START	
	;INTERVAL = EVERY HOUR
	LDA TIME.CURRENT.MINUTE		
	CMP #$00
	BNE .NEXT.EVENT
	
	JSR NPC.SCHEDULER
	
.NEXT.EVENT
	;**FALLS THROUGH**
	
			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK2
			; CMP #$02
			; BNE .TEMP			
			; LDA PLAYER.MAP.LOCATION_TYPE
			; CMP #$02
			; BEQ .TEMP3
			; LDA #$AC
			; ldx PLAYER.MAP.LOCATION_TYPE			
			; JSR FULL.BRK
; .TEMP3
			; LDA TEMP
			
NEXT.EVENT ;REPLACE WITH NAME OF NEXT EVENT******
	
@END
;**FALLS THROUGH**
EVENT.CHECKS.BUILDING_ONLY
@START
	LDA PLAYER.MAP.LOCATION_TYPE
	; CMP #MAP.TYPE.TOWN_VILLAGE
	; BEQ TILE_SWAP.SUNRISE_SUNSET.SCREEN_DRAW
	; JMP EVENT.MANAGER.EXIT
	
	;is a building map active
	;**OPT** Memory. Speed. Right now there is only one code for location type, which is used to determine the tile_set to use and for other location-type specific features. There are a lot of the later. It might be a savings if the 
					;there were a tile-set code separate from location type code, and there was a default tile-set picked using the location-type code if tile-set code was set to default. This might require adding another byte to some hex tables
					;but I suspect it would pay off. 
	CMP #MAP.TYPE.BUILDING.GRE 	;is map type = building?
	BCC TILE_SWAP.SUNRISE_SUNSET.SCREEN_DRAW		;if no
	CMP #MAP.TYPE.BUILDING.LT	;is map type = building?
	BCS TILE_SWAP.SUNRISE_SUNSET.SCREEN_DRAW		;if no
	JMP EVENT.MANAGER.EXIT			;if yes
	
	
TILE_SWAP.SUNRISE_SUNSET.SCREEN_DRAW
@START	

; ; .IS.NIGHT
	; ; LDA TIME.SUN.STATUS ;$00 = SUN RISING, $01 = DAY, $02 = SUN SETTING, $03 = NIGHT
	; ; CMP #$03
	; ; BNE .IS.SUNSET
	; ; JMP .SET.NIGHT.TILES	
.IS.SUNSET
	LDA TIME.SUN.STATUS ;$00 = SUN RISING, $01 = DAY, $02 = SUN SETTING, $03 = NIGHT
	CMP #$02
	BNE .IS.SUNRISE
	
	LDA TIME.SUN.COUNTER	;$00-$04: counts down from $04 for sunset, counts up from $00 for sunrise. each value is a phase of the sunset/sunrise
	CMP #$00				;if the counter is less than $02, it is almost fully night
	BNE .EVENT_CHECK.COMPELTE		;if the counter is $02 or greater, don't trigger the night tiles here because once the counter reaches $01 the night tiles will be triggered via the event manager
	LDA TIME.SUN.SUB_COUNTER
	CMP #$01
	BNE .EVENT_CHECK.COMPELTE		;if the counter is $02 or greater, don't trigger the night tiles here because once the counter reaches $01 the night tiles will be triggered via the event manager

	PLA ;pop 1 RTS off the stack
	PLA
	JMP GAME.LAUNCH
	;**FALLS THROUGH**	
; .SET.NIGHT.TILES
	; JSR TILE_SWAP.SUN.SET_NIGHT ;use night tiles (blue)
	; JMP .EVENT_CHECK.COMPELTE
	
; ; .IS.DAY
	; ; LDA TIME.SUN.STATUS ;$00 = SUN RISING, $01 = DAY, $02 = SUN SETTING, $03 = NIGHT
	; ; CMP #$01
	; ; BNE .IMPUTE.SUNRISE
	; ; JMP .SET.DAY.TILES	
.IS.SUNRISE 
	LDA TIME.SUN.STATUS ;$00 = SUN RISING, $01 = DAY, $02 = SUN SETTING, $03 = NIGHT
	CMP #$00
	BNE .EVENT_CHECK.COMPELTE
	
	LDA TIME.SUN.COUNTER	;$00-$04: counts down from $04 for sunset, counts up from $00 for sunrise. each value is a phase of the sunset/sunrise
	CMP #$04				;if the counter is greater than $02, it is almost fully day
	BNE .EVENT_CHECK.COMPELTE
	LDA TIME.SUN.SUB_COUNTER
	CMP #$01
	BNE .EVENT_CHECK.COMPELTE		;if the counter is $02 or greater, don't trigger the night tiles here because once the counter reaches $01 the night tiles will be triggered via the event manager

	PLA ;pop 1 RTS off the stack
	PLA
	JMP GAME.LAUNCH
	
	;**FALLS THROUGH**	
; .SET.DAY.TILES
	; JSR TILE_SWAP.SUN.SET_DAY ;use day tiles (red)
	; ;**FALLS THROUGH**
.EVENT_CHECK.COMPELTE

@END

NEXT.BUILDING.EVENT ;REPLACE WITH NAME OF NEXT EVENT******
@END
	;**FALLS THROUGH**
	
EVENT.MANAGER.EXIT
	RTS	;return to primary game loop
	
@END

;====EVENT RELATED SUBROUTINES====
DETERMINE.SUNLIGHT.STATUS
@START
;=====================SUBROUTINE DOCUMENTATION====================================
;
;determine if sun is visible based on time of day and location type
;
;=================================================================================

	LDA PLAYER.MAP.LOCATION_TYPE
	CMP #MAP.TYPE.SURFACE
	BEQ	.SUNLIGHT.NORMAL

	CMP #MAP.TYPE.UNDERMAP
	BEQ .SUNLIGHT.BLOCKED

	CMP #MAP.TYPE.BUILDING.GRE 	;is map type = building?
	BCC .MAP.TYPE_CHECK.DONE		;if no
	CMP #MAP.TYPE.BUILDING.LT	;is map type = building?
	BCS .MAP.TYPE_CHECK.DONE		;if no
	JMP .SUNLIGHT.NORMAL 			;if yes
.MAP.TYPE_CHECK.DONE
	
	;**FALLS THROUGH**  
		
.ERROR
;UNEXPECTED LOCATION TYPE DETECTED IN DETERMINE.SUNLIGHT.STATUS
;
;NOTE: if a new location was just added, 
;;*****MAKE SURE STOP VALUE IS IN CORRECT BYTE OF MAP.WORMHOLES_xx!!!!

	JSR PREP.BRK
	BRK

.SUNLIGHT.NORMAL
	JMP .TIME.DETERMINE.SUN.STATUS
	
.SUNLIGHT.BLOCKED
	LDA #$03							;sun status = night
	STA TIME.SUN.STATUS
	JMP .EXIT
	
.TIME.DETERMINE.SUN.STATUS
;PARAMETERS: TIME.CURRENT.HOUR, TIME.CURRENT.MINUTE
;RETURN: TIME.SUN.STATUS
;ENTRANCE: DIRECT


;IS IT DAY OR NIGHT?
.UPPER.TEST
	LDA TIME.CURRENT.HOUR				;check if current hour & minute is less than the hour at which sunset occurs
	CMP #EVENT.SUNSET.HOUR
	BCC	.LOWER.TEST
	CMP #EVENT.SUNSET.HOUR
	BEQ .IS.SUNSET.HOUR					
	JMP	.SET.NIGHT						;if current hour is not less than or equal to sunset hour, then it must be great. it is night
	
.IS.SUNSET.HOUR
	LDA TIME.CURRENT.MINUTE	
	CMP #EVENT.SUNSET.MINUTE			;if current hour = sunset hour, then the current minute must be compared to the sunset minute to determine if it is day or night. 
	BCC	.SET.DAY	
	JMP .SET.NIGHT
	
.LOWER.TEST	

			;TESTED;
	LDA TIME.CURRENT.HOUR				;check if current hour & minute is less than the hour at which sunrise occurs
	CMP #EVENT.SUNRISE.HOUR
	BCC	.SET.NIGHT						
	CMP #EVENT.SUNRISE.HOUR
	BEQ .IS.SUNRISE.HOUR					
	JMP	.SET.DAY						;if current hour is not less than or equal to sunrise hour, then it must be great. it is day
	
.IS.SUNRISE.HOUR
	LDA TIME.CURRENT.MINUTE	
	CMP #EVENT.SUNRISE.MINUTE			;if current hour = sunrise hour, then the current minute must be compared to the sunset minute to determine if it is day or night. 
	BCC	.SET.NIGHT	
	JMP .SET.DAY	

.SET.DAY
	LDA #$01							;sun status = day
	STA TIME.SUN.STATUS
	JMP .EXIT

.SET.NIGHT
	LDA #$03							;sun status = night
	STA TIME.SUN.STATUS
			; JSR PREP.BRK
			; LDA #$AA
			; BRK
	JMP .EXIT
	
.EXIT
	RTS
	
@END

TIME.DISPLAY ;=========DISPLAYS TIME ON VIDEO SCREEN======
@START
;PARAMETERS: NONE
;RETURN: NONE
;ENTRANCE: DIRECT


;PRINT "   "   (ERASE DISPLAY CLOCK)	
	LDA #$24
	STA HTAB	
	LDA #$4
	STA VTAB
	JSR	UPDATE.CHAR.POS

	JMP .TEXT1.PRINT ;**OPT** Memory. Move the text blocks to the global text block area at the end of game_loop.ASM and then remove the jumps. 

.TEXT1 .AZ -/    /			;ASCII text string
.TEXT1.PRINT
		LDA #.TEXT1 					
		STA STRING
		
		LDA /.TEXT1
		STA STRING+$1						
	JSR PRINT.STR

	
;PRINT $TIME.CURRENT.HOUR	
	LDA #$24
	STA HTAB	
	LDA #$4
	STA VTAB
	JSR	UPDATE.CHAR.POS
	
		LDA TIME.DISPLAY.MINUTE
		STA BCD
		LDA TIME.DISPLAY.HOUR
		STA BCD+$1
	JSR PRINT_FIXED.BCD_PACKED


	
;PRINT "AM" OR "PM"
	LDA #$25
	STA HTAB	
	LDA #$5
	STA VTAB
	JSR	UPDATE.CHAR.POS
	
	LDA TIME.DISPLAY.AM_PM
	BNE .IS.PM
;.IS.AM
	JMP .TEXT3.PRINT
.TEXT3 .AZ -/am/			;ASCII text string
.TEXT3.PRINT
		LDA #.TEXT3 					
		STA STRING
		
		LDA /.TEXT3
		STA STRING+$1

	JMP .PRINT.AM_PM

.IS.PM	
	JMP .TEXT4.PRINT		
.TEXT4 .AZ -/pm/			;ASCII text string
.TEXT4.PRINT
		LDA #.TEXT4					
		STA STRING
		
		LDA /.TEXT4
		STA STRING+$1						
		;**FALLS THROUGH**

.PRINT.AM_PM
	JSR PRINT.STR	

.EXIT
	
	RTS
	
@END


TILE_SWAP.INIT.SUNRISE_SUNSET
@START
;PARAMETERS: ACC ($00 = init buffers | >=$01 = don't init buffers)
;ENTRANCE: DIRECT
;RETURN: updated shape tables

;=====================SUBROUTINE DOCUMENTATION====================================
;
;This routine called upon building entrance, and by GAME.LAUNCH to evaluate the time
;of day and set the day or night floor/street tiles accordingly.
;
;(NO LONGER USED)-About Initializing the Buffers
;The first step in doing this is to init the shape table buffers for the day/night tiles.
;However, this only needs to be done if the building shape table has just been loaded. 
;
;the buffers need to be init because the building shape table is setup with a red floor tile and
;a blue street tile. These are copied to the red and blue buffers. 
;
;-After Buffers are Init, if applicable
;The buffers are init in .LOAD.MISC.SHAPES (LOADER.P.ASM) during game boot, so this process
;doen't need to be repeated. 
;Based on time of day, the buffers are copied into the floor 
;and street shape tables so that floor and street are the same
;shape and same color.  
;
;If it is sunrise/day, swap in the daytime tiles.
;If it is sunset/night, swap in the nightime tiles.  
;
;For more documentation on the overall implementation of this tile swap event, 
;see the Event_Manager.ASM documentation at the top of this file.
;
;=================================================================================

;**UPDATE: buffer is now init in LOADER.P.ASM
;an init each time building is entered is not needed.

	;ACC = parameter: ($00 = init buffers | >=$01 = don't init buffers)
	; CMP #$01 ;is ACC parm set to init buffer?
	; BCS .START ;if no, then skip init buffer
	;**FALLS THROUGH**

.INIT.BUFFER	
	;copy red & blue floor tiles to an aux memory buffer
	;JSR TILE_SWAP.SUN.INIT.BUFFERS ;this routine is in BSR: bank1

.START

;determine time of day
	LDA TIME.SUN.STATUS ;$00 = SUN RISING, $01 = DAY, $02 = SUN SETTING, $03 = NIGHT
	CMP #$00 ;sunrise?
	BEQ .SET.DAY.TILES
	CMP #$01 ;day
	BEQ .SET.DAY.TILES
	CMP #$02 ;sunset?
	BEQ .SET.NIGHT.TILES	
	;ACC assumed = #$03, night

.SET.NIGHT.TILES
	JSR TILE_SWAP.SUN.SET_NIGHT ;use night tiles (blue)
	JMP .TILE_SWAP.INIT.DONE
	
.SET.DAY.TILES

;DEBUG: screen looks okay



	; ;---------------------
	
			; ;
			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; jsr keyin

		; ;GET MAIN BSR MEMORY-STATUS UPON ENTRY
			; LDA $C012		;ROM/BSR soft-switch flag (bit7 = 1: BSR, bit7=0 ROM)
			; STA BSR.STATE
			; LDA $C011		;BANK1/BANK2 soft-switch flag (bit7 = 1: BANK2, bit7=0 BANK1)
			; STA BANK.STATE
			
			
			; lda #$aa
			; ldx BSR.STATE
			; ldy BANK.STATE
			; ; ldx #TILE_SWAP.SUN.SET_DAY
			; ; ldy /TILE_SWAP.SUN.SET_DAY
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP
	; ;---------------------	
	

	
	JSR TILE_SWAP.SUN.SET_DAY ;use day tiles (red)
	;**FALLS THROUGH**
	
	
.TILE_SWAP.INIT.DONE

	RTS
	
	
;****OLD METHOD TIED TO SUN STATUS AND SUN STATUS COUNTER
; .IS.NIGHT
	; LDA TIME.SUN.STATUS ;$00 = SUN RISING, $01 = DAY, $02 = SUN SETTING, $03 = NIGHT
	; CMP #$03
	; BNE .IS.SUNSET
	; JMP .SET.NIGHT.TILES	
; .IS.SUNSET
	; LDA TIME.SUN.STATUS ;$00 = SUN RISING, $01 = DAY, $02 = SUN SETTING, $03 = NIGHT
	; CMP #$02
	; BNE .IS.DAY	
	; LDA TIME.SUN.COUNTER	;$00-$04: counts down from $04 for sunset, counts up from $00 for sunrise. each value is a phase of the sunset/sunrise
	; CMP #$02				;if the counter is less than $02, it is almost fully night
	; BCS .IS.DAY			;if the counter is $02 or greater, don't trigger the night tiles here because once the counter reaches $01 the night tiles will be triggered via the event manager
; .SET.NIGHT.TILES
	; JSR TILE_SWAP.SUN.SET_NIGHT ;use night tiles (blue)
	; JMP .TILE_SWAP.INIT.DONE
	
; .IS.DAY
	; LDA TIME.SUN.STATUS ;$00 = SUN RISING, $01 = DAY, $02 = SUN SETTING, $03 = NIGHT
	; CMP #$01
	; BNE .IMPUTE.SUNRISE
	; JMP .SET.DAY.TILES	
; .IMPUTE.SUNRISE ;from check aboe, we know TIME.SUN.STATUS !=$03, $02, or $01. The only value left in the valid range for this variable is $00, which is sunrise. 
	; ; LDA TIME.SUN.STATUS ;$00 = SUN RISING, $01 = DAY, $02 = SUN SETTING, $03 = NIGHT
	; ; CMP #$00
	; ; BNE .IS.DAY	
	; LDA TIME.SUN.COUNTER	;$00-$04: counts down from $04 for sunset, counts up from $00 for sunrise. each value is a phase of the sunset/sunrise
	; CMP #$02				;if the counter is greater than $02, it is almost fully day
	; BCS .SET.DAY.TILES		;if the counter is less than $02, don't trigger the day tiles here because once the counter reaches $03 the day tiles will be triggered via the event manager
	; JMP .SET.NIGHT.TILES
; .SET.DAY.TILES
	; JSR TILE_SWAP.SUN.SET_DAY ;use day tiles (red)
	; ;**FALLS THROUGH**

@END
	
;==============KEEP THESE ROUTINES IN THIS ORDER====== (1st one fall through to 2nd one)
TIME.UPDATE.MOVE ;========UPDATES THE 24-HR GAME CLOCK AFTER PLAYER MOVE=====
@START
;PARAMETERS: TIME.CURRENT.HOUR, TIME.CURRENT.MINUTE
;RETURN: TIME.CURRENT.HOUR, TIME.CURRENT.MINUTE
;ENTRANCE: DIRECT

	SED							;set decimal (BCD) mode

	INC TIME.MOVES.COUNTER		;increment time move counter to reflect current move
	
;CHECK MOVE COUNTER
	LDA TIME.MOVES.COUNTER
	CMP #TIME.MOVES.PER_MINUTE	;are we ready for a clock increment? 
	BEQ .INCREMENT.CLOCK		;if yes, continue
	JMP TIME.UPDATE.EXIT		;if no, exit

.INCREMENT.CLOCK ;(24hr)
	LDA #$00
	STA TIME.MOVES.COUNTER
	
	LDA TIME.CURRENT.MINUTE
	CLC
	ADC #$01
	CMP #$60
	BNE .UPDATE.CLOCK
	LDA TIME.CURRENT.HOUR
	CLC
	ADC #$01
	CMP #$24
	BNE .RESET.MINUTE.COUNTER
.RESET.HOUR.COUNTER
	LDA #$00
	;STA TIME.CURRENT.HOUR
	;**FALLS THROUGH**
.RESET.MINUTE.COUNTER
	STA TIME.CURRENT.HOUR				;saves value in ACC from prior section
	LDA #$00
	;**FALLS THROUGH**
.UPDATE.CLOCK
	STA TIME.CURRENT.MINUTE				;saves value in ACC from prior section	
	;**FALLS THROUGH**
	
@END
	
TIME.UPDATE.DISPLAY	;=====CONVERTS THE 24-HR GAME CLOCK TO A 12-HOUR DISPLAY CLOCK=====			
@START
;PARAMETERS: TIME.CURRENT.HOUR, TIME.CURRENT.MINUTE
;RETURN: TIME.DISPLAY.HOUR, TIME.DISPLAY.MINUTE, TIME.DISPLAY.AM_PM
;ENTRANCE: DIRECT, OR VIA TIME.UDATE.MOVE

;CONVERT TO 12-HOUR CLOCK

	SED							;set decimal (BCD) mode

	
	LDA TIME.CURRENT.HOUR
	CMP #$13
	BCC .SET.DISPLAY.CLOCK
	SEC
	SBC #$12
	;**FALLS THROUGH
.SET.DISPLAY.CLOCK
	STA TIME.DISPLAY.HOUR				;saves value in ACC from prior section
	LDA TIME.CURRENT.MINUTE
	STA TIME.DISPLAY.MINUTE

;FIX 12am
	LDA TIME.DISPLAY.HOUR
	BNE .SET.AM_PM
	LDA #$12
	STA TIME.DISPLAY.HOUR
.SET.AM_PM	
	LDA TIME.CURRENT.HOUR
	CMP #$12
	BCS .IS.PM
;.IS.AM
	LDA #$00
	STA TIME.DISPLAY.AM_PM
	JMP .EXIT
.IS.PM
	LDA #$01
	STA TIME.DISPLAY.AM_PM
	JMP .EXIT
	
.EXIT
	;**FALLS THROUGH**
	
TIME.UPDATE.EXIT
	CLD							;clear decimal (BCD) mode, return to binary mode
	RTS
@END
;==============KEEP THESE ROUTINES IN THIS ORDER======

