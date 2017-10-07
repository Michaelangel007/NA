                .CR     6502            Use 6502 overlay
				.EF		errors
				;.LI 	OFF				Switches the assembler machine code conversion listing off so errors are easier to see				
				.LF		C:\MY_CODE\LIST
				.EB		OFF				;turn off error bell
				;				***For include files, look at end of program, before variable definitions. 


				
;===============GAME.BIN (game_loop.ASM)======================	
@START			
				.TF     GAME.BIN,BIN
				.OR		$6000			**Always put before .TF directive and never use again in program
				
;=====================SUBROUTINE DOCUMENTATION====================================
;
;For a description of the game launch process see:
;		Chart 0.1 (GAME LAUNCH) (/my_code/documentation)
;
;=================================================================================
				

;====================MAIN PROGRAM============


QUIT.GO
;=========PHYSICAL APPLE IIEs==========
;press control+reset when garbage text screen appears on BRK
;then do call -151 from ] prompt)
;==============================

	BRK
	
;ENABLE.BS_RAM BEFORE REENTRY
	LDA $C083				;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK 1ST)
	LDA $C083
	LDA SAVED.ACC.LOCAL
	JMP GAME.RENTRY

QUIT.PREP
@START
;=====================SUBROUTINE DOCUMENTATION====================================
;
;Exit to Apple machine language monitor. 
;This subroutine is located at the start of the program
;so that its memory address won't be changed by the
;fluctuations of the code size. This is important
;so that the rentry address to return to the game from the
;apple monitor remains the same. 
;
;After executing (Q), look for the break address. The reentry
;address is 1 byte before the break address reported by the 
;monitor. 
;
;NOTE: On the physical IIe, this routine will often result in a screen of 
;garbage and no access to the monitor prompt. The problem seems to
;occur after calling HCG.OFF. See notes in that routine for more
;information. A work around for troubleshooting something specific
;may be to turn off HRCG completely; skip HCG.ON and HCG.OFF
;
;Update: the garbage text screen appears to have nothing to do with HCG. Without turning on HCG
;I observed it on physical IIe and it also happens on AppleWIN in enhanced IIe mode. This is a bit odd
;since my physical IIe is unenhanced. 
;In any event, here is a work around I discovered on the physical IIe:
;	press control+reset when garbage text screen appears on BRK
;	then do call -151 from ] prompt. Monitor seems to function from there)
;
;Notes on troubleshooting: the problem doesn't seem to have anythinng to do with the way 
;the break is executed. The following break code will result in a proper break every time if place
;at the start of GAME.START.DRIVER:
;			LDA $C082				;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK 1ST)
;			LDA $C082		
;			LDA $C051
;
;			BRK
;
;However, this same code, when placed at the top of PREP.BRK will randomly result
;in garbage after (Q) is pressed. The next step in troubleshooting would be to use the above
;break code and gradualy move it deeper into the program, each time testing multiple times. 
;Out of 10 times pressing (Q), I observed 4 out of 10 resulted in the garbage screen. 
;
;***This troubleshooting can now be done in AppleWIN since 
;ApplWIN exhibits this behavior when in Apple IIe enhanced mode. 
;
;=================================================================================





;=========PHYSICAL APPLE IIEs==========
;press control+reset when garbage text screen appears on BRK
;then do call -151 from ] prompt)
;==============================

;COPY PART OF PATHFINDER SAVED.PATH.LOOKUP.TABLE 
		;AUX MEMORY -> MAIN MEMORY 	
			LDA #$00			;SET START ADDRESS
			STA AUX_MOVE.START
			LDA #$5C		
			STA AUX_MOVE.START+$1
			;
			LDA #$FF			;SET END ADDRESS
			STA AUX_MOVE.END
			LDA #$5C
			STA AUX_MOVE.END+$1
			;
			LDA #$00			;SET DESTINATION ADDRESS
			STA AUX_MOVE.DEST
			LDA #$20
			STA AUX_MOVE.DEST+$1
			CLC					;SET CARRY FLAG DESGINATD MOVE FROM AUX MEMORY -> MAIN
			JSR AUX_MOVE
			
					;NOP

		;AUX MEMORY -> MAIN MEMORY 	
			LDA #$00			;SET START ADDRESS
			STA AUX_MOVE.START
			LDA #$5D		
			STA AUX_MOVE.START+$1
			;
			LDA #$FF			;SET END ADDRESS
			STA AUX_MOVE.END
			LDA #$5D
			STA AUX_MOVE.END+$1
			;
			LDA #$00			;SET DESTINATION ADDRESS
			STA AUX_MOVE.DEST
			LDA #$21
			STA AUX_MOVE.DEST+$1
			CLC					;SET CARRY FLAG DESGINATD MOVE FROM AUX MEMORY -> MAIN
			JSR AUX_MOVE
			;			
		;AUX MEMORY -> MAIN MEMORY 	
			LDA #$00			;SET START ADDRESS
			STA AUX_MOVE.START
			LDA #$5E		
			STA AUX_MOVE.START+$1
			;
			LDA #$FF			;SET END ADDRESS
			STA AUX_MOVE.END
			LDA #$5E
			STA AUX_MOVE.END+$1
			;
			LDA #$00			;SET DESTINATION ADDRESS
			STA AUX_MOVE.DEST
			LDA #$22
			STA AUX_MOVE.DEST+$1
			CLC					;SET CARRY FLAG DESGINATD MOVE FROM AUX MEMORY -> MAIN
			JSR AUX_MOVE
			;						
		;AUX MEMORY -> MAIN MEMORY 	
			LDA #$00			;SET START ADDRESS
			STA AUX_MOVE.START
			LDA #$5F	
			STA AUX_MOVE.START+$1
			;
			LDA #$FF			;SET END ADDRESS
			STA AUX_MOVE.END
			LDA #$5F
			STA AUX_MOVE.END+$1
			;
			LDA #$00			;SET DESTINATION ADDRESS
			STA AUX_MOVE.DEST
			LDA #$23
			STA AUX_MOVE.DEST+$1
			CLC					;SET CARRY FLAG DESGINATD MOVE FROM AUX MEMORY -> MAIN
			JSR AUX_MOVE
			;	


				; LDA #$AF							;START ADDRESS OF CURRENT ZONE
				; STA AUX_MOVE.START
				; LDA #$04	
				; STA AUX_MOVE.START+$1
				
				; LDA #$B1							;END ADDRESS OF AUX MOVE WILL BE START ADDRESS OF NEXT ZONE
				; STA AUX_MOVE.END
				; LDA #$04					
				; STA AUX_MOVE.END+$1		
				
				; LDA #ZONE_TOOLS.INPUT_BUFFER						;SET DESTINATION ADDRESS
				; STA AUX_MOVE.DEST
				; LDA /ZONE_TOOLS.INPUT_BUFFER	
				; STA AUX_MOVE.DEST+$1
				; CLC		;AUX -> MAIN
				; JSR AUX_MOVE
				
	
	;JSR PREP.BRK
	

		
	LDA TEXT

; ;DISABLE.BS_RAM
	LDA $C082				;READ ENABLE ROM, DISABLE WRITE ON BSM (NORMAL STATE).
							; ;REQUIRED FOR BEFORE BRK BECAUSE APPLE MONITOR IS A ROM ROUTINE
	
	JSR CLEAR.TEXT.SCREEN
	JSR HCG.OFF				;disconnect HRCG driver, return COUT vector to normal value	

				
		
	;LDA PLAYER.HEIGHT
	; LDX #PLAYER.ICON.BUFFER
	; LDY /PLAYER.ICON.BUFFER
	;LDA ANIMATION.DEEP_WATER.TALLY
	; LDA PLAYER.MAP.LOCATION	
	; LDX PLAYER.MAP.LOCATION_TYPE
	
	;LDA GAME.MOB_GEN.CONTROL
	;LDA TIME.SUN.STATUS	
	; LDX TIME.SUN.COUNTER
	; LDY TIME.SUN.SUB_COUNTER

	; LDA COMPLETED.SCHEDULER.FLAG
	; LDX TIME.CURRENT.HOUR
	; LDY TIME.CURRENT.MINUTE

		
		
	JSR GENERATE.DEBUG.LOG

	LDA NTALK.VOICE_MODE
	;LDA PLAYER.BLOCKED.NPC.COUNTER
	; LDX #SCREEN.MO_SPRITE.DATA
	; LDY /SCREEN.MO_SPRITE.DATA


		; LDX #MAP.LOCATIONS_00
		; LDY /MAP.LOCATIONS_00
		;LDX #MAP_OBJECTS.MOB
		;LDY /MAP_OBJECTS.MOB
	
	STA SAVED.ACC.LOCAL
	JMP QUIT.GO
@END	

GENERATE.DEBUG.LOG
@START
;USED BEFORE A BRK TO LOG THE LOCATION AND/OR VALUE OF VARIOUS VARIABLES

;DEBUG.LOG = $3000

;PATHFINDER

	;$1$2, $2/$3 are open
	
	LDA #PATHFINDER.SPRITE.RECORD		;same format as SPRITE.RECORD, but only use by NPC.PATHFINDER
	STA DEBUG.LOG+$4
	LDA /PATHFINDER.SPRITE.RECORD
	STA DEBUG.LOG+$5	
	
	LDA #NPC.SCHEDULE.WORKSPACE
	STA DEBUG.LOG+$6
	LDA /NPC.SCHEDULE.WORKSPACE
	STA DEBUG.LOG+$7

	LDA #NPC.PATHGENERATOR.QUE
	STA DEBUG.LOG+$8
	LDA /NPC.PATHGENERATOR.QUE
	STA DEBUG.LOG+$9

	LDA #SAVED.PATH.LOOKUP.TABLE
	STA DEBUG.LOG+$A
	LDA /SAVED.PATH.LOOKUP.TABLE
	STA DEBUG.LOG+$B

	LDA #NPC.ANCHORS.X
	STA DEBUG.LOG+$C
	LDA /NPC.ANCHORS.X
	STA DEBUG.LOG+$D

	LDA #NPC.ANCHORS.Y
	STA DEBUG.LOG+$E
	LDA /NPC.ANCHORS.Y
	STA DEBUG.LOG+$F

;SCREEN ARRAYS
	LDA #SCREEN.TILE.DATA
	STA DEBUG.LOG+$20
	LDA /SCREEN.TILE.DATA
	STA DEBUG.LOG+$21

	LDA #SCREEN.DARK.DATA
	STA DEBUG.LOG+$22
	LDA /SCREEN.DARK.DATA
	STA DEBUG.LOG+$23

	LDA #SCREEN.DARK.DATA_BEFORE
	STA DEBUG.LOG+$24
	LDA /SCREEN.DARK.DATA_BEFORE
	STA DEBUG.LOG+$25

	LDA #SCREEN.MO_SPRITE.DATA
	STA DEBUG.LOG+$26
	LDA /SCREEN.MO_SPRITE.DATA
	STA DEBUG.LOG+$27	
	
	LDA #SCREEN.MO_SPRITE_TYPE.DATA
	STA DEBUG.LOG+$28
	LDA /SCREEN.MO_SPRITE_TYPE.DATA
	STA DEBUG.LOG+$29	

	LDA #SCREEN.MO_GENERAL.DATA
	STA DEBUG.LOG+$2A
	LDA /SCREEN.MO_GENERAL.DATA
	STA DEBUG.LOG+$2B
	
;Sprite Movement: choose paths
	;$40 & $41 open 
	
	LDA #MOB.MOVES.BLOCKED
	STA DEBUG.LOG+$42
	LDA /MOB.MOVES.BLOCKED
	STA DEBUG.LOG+$43	
	
	LDA #MOB.MOVE.OPTIONS_PRIMARY
	STA DEBUG.LOG+$44
	LDA /MOB.MOVE.OPTIONS_PRIMARY
	STA DEBUG.LOG+$45
	
	LDA #MOB.MOVE.OPTIONS_SECONDARY
	STA DEBUG.LOG+$46
	LDA /MOB.MOVE.OPTIONS_SECONDARY
	STA DEBUG.LOG+$47	
	
	LDA #NPC.ASSIGNED_PATHS
	STA DEBUG.LOG+$48
	LDA /NPC.ASSIGNED_PATHS
	STA DEBUG.LOG+$49	

	LDA #MOB.ADJACENT_TILES
	STA DEBUG.LOG+$4A
	LDA /MOB.ADJACENT_TILES
	STA DEBUG.LOG+$4B	
	
	
;Sprite Movement: General
	LDA #SPRITE.RECORD	;& GENERAL_MO.RECORD
	STA DEBUG.LOG+$50
	LDA /SPRITE.RECORD	;& GENERAL_MO.RECORD
	STA DEBUG.LOG+$51
	LDA #MAP_OBJECTS.TILE_LOCATION
	STA DEBUG.LOG+$52
	LDA /MAP_OBJECTS.TILE_LOCATION
	STA DEBUG.LOG+$53

	
;MAP/PLAYER VARIABLES
	LDA #ZONE.LOOKUP.LO
	STA DEBUG.LOG+$E0
	LDA /ZONE.LOOKUP.LO
	STA DEBUG.LOG+$E1

	LDA #ZONE.LOOKUP.HO
	STA DEBUG.LOG+$E2
	LDA /ZONE.LOOKUP.HO
	STA DEBUG.LOG+$E3
	
	LDA #RZONE.ARRAY
	STA DEBUG.LOG+$E4
	LDA /RZONE.ARRAY
	STA DEBUG.LOG+$E5

	LDA GMAP.X
	STA DEBUG.LOG+$E6
	LDA GMAP.Y
	STA DEBUG.LOG+$E7

	LDA RMAP.X
	STA DEBUG.LOG+$E8
	LDA RMAP.Y
	STA DEBUG.LOG+$E9

	LDA RMAP
	STA DEBUG.LOG+$EA
	LDA RMAP+$1
	STA DEBUG.LOG+$EB
	
	LDA PLAYER.WMAP.ZONE
	STA DEBUG.LOG+$EC
	
	LDA PLAYER.TRANSPORT.ACTIVE	
	STA DEBUG.LOG+$ED
	
;GENERAL 	
	LDA TIME.CURRENT.HOUR				;**OPT** Memory. These debug log commands can be remove. 
	STA DEBUG.LOG+$F0
	LDA TIME.CURRENT.MINUTE
	STA DEBUG.LOG+$F1	

	LDA #TROUBLESHOOTING.HOOK
	STA DEBUG.LOG+$F2
	LDA /TROUBLESHOOTING.HOOK
	STA DEBUG.LOG+$F3	
	;TROUBLESHOOTING.HOOK2
	;is the address of HOOK1 + $2
	LDA #EVENT.FLAGS
	STA DEBUG.LOG+$F4
	LDA /EVENT.FLAGS
	STA DEBUG.LOG+$F5		
	
	RTS

@END

;QUIT ISN'T RUN BY DEFAULT BECAUSE LOADER.BIN TRANSFERS
;CONTROL VIA A JMP TO GAME.LOADER2

GAME.START.DRIVER
@START	



			; LDA $C082				;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK 1ST)
			; LDA $C082		
			; LDA $C051
			
			; ;JSR $FCF8
			; BRK
			
  ;JMP QUIT.PREP
  
;=====================CODE-SECTION DOCUMENTATION====================================
;
;Setup variables that would normally be retreived from a saved game or 
;new game file. For example, the player's world map position (RMAP) and 
;the zone the player is locationed in (WZONE)
;
;see Chart 0.1 (GAME LAUNCH), in /my_code/documentation
;
;=================================================================================

;**OPT** Memory. Once testing is largely complete the init of vars/arrays should
;be able to be removed because .BS (block skip) automatically inits the arrays to $00 or
;whatever value is specified (so just modify the .BS to init the MO arrays to $FF for example)
;during testing I'm contiuing to do inits because if I change a var/array to a .EQ definition
;so i can monitor it's contents, then an init is needed.
@MIDDLE

	JSR RANDOM.8.INIT			;LOAD SEED AND SETS DEFAULT RETURN RESULT RANDOM # RANGE. 		

	
;============

;**TEMPORARY** (UNTIL SAVE/LOAD GAME FEATURE IS ADDED)
	
;SETUP STARTING POSITION ON THE MAP
	; LDA #$49				;$49 is upper left corner. $2B is one row up, 5 rows left of the lower right corner. 
	; STA GMAP				;$08,0D IS ANOTHER GOOD LOCATION
	; LDA #$01				;$01 is upper left corner. $0D is one row up, 5 rows left of the lower right corner. 
	; STA GMAP+$1


	
	;$09 is upper left corner, $19 is main island, $36 is lower right corner
	LDA #$19							;SET START WORLD ZONE ( SET WZONE )
	STA PLAYER.WMAP.ZONE				;
	
	;MAIN ISLAND: GMAP.X=$18, GMAP.Y=$37, ZONE $09: GMAP.X=$18, GMAP.Y=$17
		LDA #$18
		STA GMAP.X
		STA PARM.GMAP.X
;		STA GMAP.X.LAST. see GAME.LAUNCH
		
		LDA #$37
		STA GMAP.Y
		STA PARM.GMAP.Y
;		STA GMAP.Y.LAST. see GAME.LAUNCH
		
	JSR CONVERT.GMAP_XY.RMAP_XY
		LDA RETURN.RMAP.X					;SET PLAYER X,Y ON REGIONAL MAP. MUST ALIGN WITH RMAP. 
		STA RMAP.X
		STA PARM.RMAP.X
		LDA RETURN.RMAP.Y
		STA RMAP.Y
		STA PARM.RMAP.Y	

	JSR CONVERT.RMAP_XY.RMAP
		LDA RETURN.RMAP
		STA RMAP							;SET PLAYER LOCATION ON REGIONAL MAP. MUST ALIGN WITH RMAP.X/Y
		LDA RETURN.RMAP+$1
		STA RMAP+$1
	

	;****OLD METHOD, SETTING RMAP & RMAP.X/Y MANUALLY***
	; ;$0468 (default), $03AB (tall grass demo), 
	; ;GAMEPLAY VIDEO: RMAP $037C, ZONE $11, RMAP.X $1C, RMAP.Y $12
	; LDA #$68
	; STA RMAP							;SET PLAYER LOCATION ON REGIONAL MAP. MUST ALIGN WITH RMAP.X/Y
	; LDA #$04
	; STA RMAP+$1
	;
	;
	; ;DEFAULT X=$18, Y=$17
	; LDA #$18
	; STA RMAP.X							;SET PLAYER X,Y ON REGIONAL MAP. MUST ALIGN WITH RMAP. 
	; LDA #$17
	; STA RMAP.Y

	
	LDA #$00							;SET PLAYER LOCATION TO SURFACE
	STA PLAYER.MAP.LOCATION
	LDA #LOCATION.TYPE.SURFACE
	STA PLAYER.MAP.LOCATION_TYPE		;SET LOCATION TYPE TO SURFACE MAP


				; ;TROUBLESHOOTING HOOK
				; ;
				; LDA #$00							;START ADDRESS OF CURRENT ZONE
				; STA AUX_MOVE.START
				; LDA #$BF	
				; STA AUX_MOVE.START+$1
				; ;
				; LDA #$FF							;END ADDRESS OF AUX MOVE WILL BE START ADDRESS OF NEXT ZONE
				; STA AUX_MOVE.END
				; LDA #$BF					
				; STA AUX_MOVE.END+$1		
				; ;
				; LDA #$00							;SET DESTINATION ADDRESS
				; STA AUX_MOVE.DEST
				; LDA #$90
				; STA AUX_MOVE.DEST+$1
				; ;
				; CLC								;CLEAR CARRY FLAG DESGINATD MOVE FROM AUX -> MAIN MEMORY
				; JSR AUX_MOVE						
				; ;
				; LDA TEXT
				; LDX #ZONE_TOOLS.OUTPUT_BUFFER
				; LDY /ZONE_TOOLS.OUTPUT_BUFFER
				; ; LDX #ZONE.LOOKUP.HO
				; ; LDY /ZONE.LOOKUP.HO
				; LDA TEXT
				; LDA $C082
				; BRK

			
;INIT SS MAP REGIONAL FLAGS (KEEP EVEN AFTER LOAD/SAVE GAME IS IMPLEMENTED)
	JSR MAP.CALCULATE.SS_FLAGS

@MIDDLE

;SET ADULT MODE ON/OFF
	LDA #$01
	STA GAME.ADULT.MODE ;$00 = OFF, $01 = ON
	
;SET TRANSPORT MODE
	LDA #$FF
	STA PLAYER.TRANSPORT.ACTIVE			;BY DEFAULT SET PLAYER TO WALKING MODE

;SETUP PLAYER ICON
	LDA #$8D							;SPEAR TWIRLER
;	LDA #$99							;HALF SUNK	
;	LDA #$9A							;FULL SUNK	

	STA PLAYER.WALKING.TILE
	STA PLAYER.WALKING.TILE.DEFAULT
	
	LDA PLAYER.HEIGHT.DEFAULT			;SET TO FULL HEIGHT
	STA PLAYER.HEIGHT					;USED TO CONTROL SPECIAL EFFETS LIKE SINKING IN SHALLOW WATER

	JSR LOAD.PLAYER.WALKING.ICON		;COPY PLAYER ICON FROM AUX MEMORY SHAPE TABLE TO MAIN MEMORY BUFFER FOR USE BY DRAW.TILE AN OTHER ROUTINES
	
;SET WORLD
	;PLAYER.WMAP.ZONE ALREADY SET

	LDA #01								;SET SUN = DAYTIME
	STA TIME.SUN.STATUS	

			; ;TROUBLESHOOTING HOOK
				; ;
				; LDA #$A1							;START ADDRESS OF CURRENT ZONE
				; STA AUX_MOVE.START
				; LDA #$0C	
				; STA AUX_MOVE.START+$1
				; ;
				; LDA #$B1							;END ADDRESS OF AUX MOVE WILL BE START ADDRESS OF NEXT ZONE
				; STA AUX_MOVE.END
				; LDA #$0C					
				; STA AUX_MOVE.END+$1		
				; ;
				; LDA #ZONE_TOOLS.OUTPUT_BUFFER		;SET DESTINATION ADDRESS
				; STA AUX_MOVE.DEST
				; LDA /ZONE_TOOLS.OUTPUT_BUFFER
				; STA AUX_MOVE.DEST+$1
				; ;
				; CLC									;CLEAR CARRY FLAG DESGINATD MOVE FROM AUX -> MAIN MEMORY
				; JSR AUX_MOVE						
				; ;
				; LDA TEXT
				; LDX #ZONE_TOOLS.OUTPUT_BUFFER
				; LDY /ZONE_TOOLS.OUTPUT_BUFFER

		LDA #DATA.MAP.SURFACE.TOTAL.SECTORS
		STA TOTAL.SECTORS
		LDA #$01
		STA USE.COMPRESSION_FLAGS	;turn compression flags on. WZONE.COMPRESSION.FLAGS will be checked for each zone to determine if compression is on or off. 
	JSR ZONE_TOOLS.BUILD.WZONE_HEADERS		
	JSR REGION.UNCOMPRESS.ALL			;COPY COMPRESSED WORLD ZONE DATA INTO MAIN MEMORY AND UNCOMPRESS INTO REGIONAL MAP


		
		
				
		; ;TROUBLESHOOTING HOOK
		; LDA TEXT
		; LDA $C082
		; LDX #RZONE.ARRAY	
		; LDY /RZONE.ARRAY	
		; BRK

;SET CURRENT TIME (MILIARY) & 12-HOUR DISPLAY CLOCK
	LDA #$00					;$HEX$. $00=AM, $01=PM
	STA TIME.DISPLAY.AM_PM		;set start time to 12:00PM
	LDA #$10					;set start time to 12:00PM
	STA TIME.CURRENT.HOUR

	LDA #$00					;set start time to 12:00PM
	STA TIME.CURRENT.MINUTE
	STA TIME.DISPLAY.MINUTE

	LDA #$10
	STA TIME.DISPLAY.HOUR
	
				;***TEMP DELETE THIS**
				; LDA #$00
				; STA TEMP.ARRAY
	
	;;SET TEST TIME
	; LDA #$01					;$HEX$. $00=AM, $01=PM
	; STA TIME.DISPLAY.AM_PM		;set start time to 12:00PM
	; LDA #$19					;set start time to 12:00PM
	; STA TIME.CURRENT.HOUR
	; ;
	; LDA #$15					;set start time to 12:00PM
	; STA TIME.CURRENT.MINUTE
	; STA TIME.DISPLAY.MINUTE
	; ;
	; LDA #$7
	; STA TIME.DISPLAY.HOUR
	
	;SET SUN STATUS
	JSR DETERMINE.SUNLIGHT.STATUS			;determine if sun is visible based on time of day and location type

	
	
	
;INIT VARIABLES
			LDA #$00 ;**OPT** Memory. Can be removed once this variable is back to a .BS definition instead of .EQ
			STA MOB.GEN.QUEUE

			LDA #$FC
			STA MOB.GEN.ARRAY_FULL_COUNTER
			
			LDA #$00 ;**OPT** Memory. Can be removed once this variable is back to a .BS definition instead of .EQ	
			STA PLAYER.TRANSPORT.SPEED
			
			LDA #$00; PROBABLY NOT NEEDED, .BS SETS TO $00 ON LAUNCH
			STA ANIMATION.FORCED
			STA ANIMATION.DEEP_WATER.TALLY
			
;init miscellanous

	LDA #$00
	STA PLAYER.COLLISSION_OVERRIDE		;$00 = OFF, $01 = ON
	STA MOB.COLLISION_OVERRIDE			;$00 = OFF, $01 = ON


	LDA #$01							;SET TURN TO MOB
	STA GAME.MOB_GEN.CONTROL			
	STA GAME.TURN.CONTROL				;THIS IS WIERD BUT CORRECT. THE MOVEMENT ROUTINE DOESN'T CALL MO.DRAW UNTIL AFTER THE PLAYER MOVE IS PROCESSED, AND THE DRAW.SCREEN CALL TO MO.DRAW DOESN'T PROCESS MOB MOVES,
	;SO, MOBs DON'T GET TO MOVE UNTIL AFTER THE PLAYER HAS MOVED. PERIOD. ALL SETTING THE TURN TO MOB AT LAUNCH DOES IS LET MO.DRAW KNOW THAT IT'S THE MOB'S TURN, ONCE IT IS CALLED AFTER THE PLAYER MOVE. THIS SETUP IS SO THAT THE TURN CAN BE SET TO PLAYER BEFORE CALLING THE PLAYER MOVEMENT ROUTINE (MOVE.N/S/E/W) SO THAT MO.DRAW WON'T PROCESS A MOB MOVE IN SITUATIONS LIKE FAST TRANSPORT IS ENABLED. 

	
	;default $1A	
	LDA #$FF
	STA MOB.GEN.PROBABILITY				;SET DEFAULT PROBABILITY OF MOB GENERATION
	
	LDA #TILE.DEPTH.STANDARD			;ANY ROUTINE THAT CHANGES THIS VALUE IS RESPONSIBLE FOR RESETTING IT
	STA TILE.DEPTH

	;init arrays
	LDY #$00
.LOOP
	LDA #$00
	STA EVENT.FLAGS, Y
	INY
	BNE .LOOP
.DONE
	
	;SET TEST EVENT FLAGS
	LDA #$01
	STA EVENT.FLAGS+$0		;turn on fire drop for undead lord

	LDA #$01				;event contingency flag setting for NPC $08, keyword "RAIS". $01 = permitted
	STA EVENT.FLAGS+$6	
;============				
@END
				
GAME.RENTRY	;=======USED FOR GAME RENTRY VIA THE MONITOR AFTER A BRK VIA QUIT COMMAND		
@START

;=====================CODE-SECTION DOCUMENTATION====================================
;
;These comments apply to GAME.RENTRY and GAME.LAUNCH, the next code section.
;Turn on Hi-Res Graphics and setup screen arrays which track things like
;which tiles are hidden from view (darkness).
;
;Setup initial graphics screen. 
;
;see Chart 0.1 (GAME LAUNCH), in /my_code/documentation
;
;=================================================================================



	LDA #$00			;SPECIFY BOTH PAGES FOR CLEAR SCREEN
	JSR SCLEAR			;CLEAR SCREEN BEFORE TURNING ON GRAPHICS (AVOIDS UNSIGHTLY FLASH OF RANDOM DOTS) 

;	JMP SKIP.GRAPHICS		;don't turn graphics mode on, so register output is visible from monitor prompt on BRK (testing)	

	LDA GRAPHICS	;TURN ON GRAPHICS MODE
	LDA HIRES		;SELECT HI-RES MODE
	LDA	PAGE1		;SELECT PAGE 1
	LDA MIXOFF		;SELECT FULL SCREEN GRAPHICS (PAGE 1)
	
SKIP.GRAPHICS	
;NOT SURE WHERE THIS SHOULD GO, PROBABLY AT VERY START OF GAME LAUNCH, PROBABLY FROM THERE IT IS ALL CALLS TO FLIP.PAGE
	LDA #$01						;
	STA PAGE.FOREGROUND		;DRAW.TILE WILL DRAW THE OPPOSITE PAGE FROM THAT SPECIFED BY THIS VARIABLE
	LDA #$02
	STA PAGE.BACKGROUND	
	LDA	PAGE1
	
GAME.LAUNCH ;=====JMP HERE TO REDRAW SCREEN AT CURRENT GMAP POSITION
	
;USED BY
; ZAP KEY (3)
; WYVERN FLIGHT (AFTER (Y)ELL, GAME.LAUNCH IS CALLED TO REMOVE ANY DARKNESS THROUGH A FRESH DRAW.SCREEN)


;RESET CERTAIN PLAYER VARIABLES
	LDA GMAP.X
	STA GMAP.X.LAST			;when the player moves, these variables are out of sync but since the game starts without the player moving, effectively like a pass move, this code section makes sure that these variables are in sync. 
	
	LDA GMAP.Y
	STA GMAP.Y.LAST			;when the player moves, these variables are out of sync but since the game starts without the player moving, effectively like a pass move, this code section makes sure that these variables are in sync. 

	

				; STA TEMP
				; LDA TROUBLESHOOTING.HOOK
				; CMP #$01
				; BNE .TEMP
				; LDA SPRITE.RECORD+$B
				; CMP #$00
				; BNE .TEMP
				; ; LDA *
				; ; LDA *-2
				; ; LDX *-1			;PROGRAM COUNTER AT BRK+$2 WILL BE RETURNED IN X-REG/Y-REG
				; ; TAY
				; LDA #$AA
				; ; LDX SPRITE.RECORD+$0
				; ; LDY SPRITE.RECORD+$1	
				; LDX GMAP.X.LAST
				; LDY GMAP.Y.LAST
				; JSR FULL.BRK
				; BRK
; .TEMP
				; LDA TEMP

				
;INITIAL SCREEN	DRAW
	JSR INIT.SCREEN.ARRAYS	
	JSR DRAW.SCREEN	
	JSR DRAW.TILE.PLAYER
	JSR FLIP.PAGE
	
			
		;ENABLE BANK-SWITCHED RAM ($D000 Bank 1)
		LDA $C08B					;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK1, 1ST)
		LDA $C08B	
	JSR COPY.SCREEN	
		;RE-ENABLE BANK-SWITCHED RAM ($D000 Bank 2)
		LDA $C083				;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK2 2nd)
		LDA $C083
		
;FALLS DIRECTLY INTO GAME.LOOP
@END

GAME.PRIMARY_LOOP ;=======MANAGES GAME STATE, PROCESSES PLAYER KEYPRESS====== 
@START
;=====================SUBROUTINE DOCUMENTATION====================================
;
;This loop continously scans for a keypress and until a keypress is 
;deteted it continously executed several automatic subroutines which 
;update the game state. 
;
;Once a key is pressed, a subroutine associated with that key is executed
;
;======Special Requirements for Non-Movement Commands====
;
;The following code must be included
;
;1) Include at the very start of the routine
;IF JUMP IS ENABLED, DISABLE IT.  
;(this is to address a scenario where the player pressed (J)ump and then presses a non-movement key)
;	LDA PLAYER.MOVE.JUMP			
;	BEQ .CONTINUE					
;	LDA #$00
;	STA PLAYER.MOVE.JUMP
;.CONTINUE 	
;
;2) Include at the very end of the routine; this is the exit command
;	JMP GAME.LAUNCH							
;
;Reason: GAME.LAUNCH redraws the whole screen, whereas a JMP TO GAME.PRIMARY_LOOP
;does not. This is important because if a key press aborts occurs during movement, the
;JSR to COPY.SCREEN is skipped at the end of the MOVE.NORTH/SOUTH/EAST/WEST routines. That is
;fine if a movement key was pressed but if a non-movement key is pressed and the 
;non-movement subtroutines exits back to GAME.PRIMARY_LOOP then the result will be
;that the animation routine is run without the foreground and background being in sync
;because the JSR to COPY.SCREEN was skipped as previously noted. 
;=================================================================================

;======CONTINOUS ROUTINES UNTIL KEY PRESS====
			
		;ENABLE BANK-SWITCHED RAM ($D000 Bank 2)
		LDA $C08B
		LDA $C08B
	JSR ANIMATION.UPDATE
		;RE-ENABLE BANK-SWITCHED RAM ($D000 Bank 2)
		LDA $C083				;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK2 2nd)
		LDA $C083
			
	JSR MOB.GENERATION

		
GAME.PRIMARY_LOOP.ALTERNATE.ENTRANCE		;SKIPS ALL INBETWEEN MOVE ACTIVITES SUCH AS ANIMATION AND MOB GENERATION
	JSR TIME.DISPLAY		
	JSR EVENT.MANAGER			
	JSR NPC.PATHGENERATOR					;calculate the path to the next anchor of all Building NPCs with a schedule change in the next hour

	
	;INC GAME_LOOP.ITERATION.COUNTER
	



	
;======CONDITIONAL LOGIC====	
@START
@MIDDLE
	
		
	LDA $C000
    BPL GAME.PRIMARY_LOOP 
    STA $C010               ;CLR LAST KEY
		
	CMP #$8B			;UP ARROW
	BEQ .NORTH_STEP
	CMP #$8A			;DOWN ARROW
	BEQ .SOUTH_STEP
	CMP #$95			;RIGHT ARROW
	BEQ .EAST_STEP
	CMP #$88			;LEFT ARROW
	BEQ	.WEST_STEP
	CMP #$C2			;(B) BOARD
	BEQ	.BOARD_STEP
	CMP #$E2			;(b) BOARD
	BEQ	.BOARD_STEP
	; CMP #$C5			;(E) ENTER
	; BEQ	.ENTER_STEP
	; CMP #$E5			;(e) ENTER
	; BEQ	.ENTER_STEP	
	CMP #$C9			;(I) alternate key for up.
 	BEQ .NORTH_STEP
	CMP #$E9			;(i) alternate key for up
	BEQ .NORTH_STEP	
	CMP #$CA			;(J) JUMP
	BEQ	.JUMP_STEP
	CMP #$EA			;(j) JUMP
	BEQ	.JUMP_STEP
	CMP #$CF			;(O) OPEN
	BEQ	.OPEN.COMMAND_STEP
	CMP #$EF			;(o) OPEN / OPERATE
	BEQ	.OPEN.COMMAND_STEP	
	CMP #$D0			;(P) PUSH
	BEQ	.PUSH.COMMAND_STEP
	CMP #$F0			;(p) PUSH
	BEQ	.PUSH.COMMAND_STEP
	CMP #$D1			;(Q) QUIT
	BEQ	.QUIT_STEP
	CMP #$F1			;(q) QUIT
	BEQ	.QUIT_STEP
	CMP #$D4			;(T) Talk
	BEQ .TALK.COMMAND_STEP
	CMP #$F4			;(t) Talk
	BEQ .TALK.COMMAND_STEP	
	CMP #$D8			;(X) X-IT
	BEQ	.XIT_STEP
	CMP #$F8			;(x) X-IT
	BEQ	.XIT_STEP
	CMP #$D9			;(Y) YELL
	BEQ	.YELL_STEP
	CMP #$F9			;(Y) YELL
	BEQ	.YELL_STEP	
	CMP #$A0			;SPACE BAR
	BEQ	.PASS_STEP
	JMP .CHECK.PLAYTEST_KEYS
	
.INVALID.KEY.PRESS
	JSR PLAY.SOUND.DUMB_ASS
	JSR PASS
	JMP GAME.PRIMARY_LOOP


@MIDDLE
	
.NORTH_STEP
	JMP NORTH

.SOUTH_STEP
	JMP SOUTH

.EAST_STEP

	JMP EAST
	
.WEST_STEP
	JMP WEST

.PASS_STEP
	JMP PASS
	
.BOARD_STEP
	JMP BOARD

; .ENTER_STEP
	; JMP ENTER
	
.JUMP_STEP
	JMP JUMP

.OPEN.COMMAND_STEP
	JMP OPEN.COMMAND
	
.PUSH.COMMAND_STEP
	JMP PUSH.COMMAND
	
.QUIT_STEP
	JMP QUIT.PREP
	
.TALK.COMMAND_STEP	
	JMP TALK.COMMAND
	
.XIT_STEP
	JMP XIT
	
.YELL_STEP
	JMP YELL

.CHECK.PLAYTEST_KEYS
	CMP #$B0			;0	(SET HOOK)
	BEQ	.KEY0_STEP
	CMP #$A9			;SHIFT+0 (SET HOOK2)
	BEQ .KEY_SHIFT0_STEP 
	CMP #$B1			;1	(TOGGLE PLAYER COLLISION CONTROLS)
	BEQ	.KEY1_STEP
	CMP #$B2			;2	(TOGGLE MOB COLLISION CONTROLS)
	BEQ	.KEY2_STEP
	CMP #$B3			;3	(ZAP MOB)
	BEQ	.KEY3_STEP	
	CMP #$B4			;4	(INCREASE MOB GEN PROB BY !1/255)
	BEQ	.KEY4_STEP	
	CMP #$B5			;5	(DECREASE MOB GEN PROB BY !1/255)
	BEQ	.KEY5_STEP
	CMP #$B6			;6	(TOGGLE ANIMATION KEYPRESS ABORT)
	BEQ	.KEY6_STEP	
	CMP #$B7			;7	(TOGGLE TOD: DAYTIME/SUNRISE/SUNSET/NIGHT)	
	BEQ	.KEY7_STEP	
	CMP #$DE			;SHIFT+6 (TOGGLE DARKNESS OVERRIDE)
	BEQ .KEY_SHIFT6_STEP		
	CMP #$B8			;8	(TOGGLE PLS)
	BEQ	.KEY8_STEP	
	CMP #$B9			;9	(GAME CLOCK: +1 MINUTE)
	BEQ	.KEY9_STEP		
	CMP #$A8			;SHIFT+9 (GAME CLOCK: SET)
	BEQ	.KEY_SHIFT9_STEP
	JMP .INVALID.KEY.PRESS
	
.KEY0_STEP
	JMP KEY0
	
.KEY_SHIFT0_STEP
	JMP KEY_SHIFT0
	
.KEY1_STEP
	JMP KEY1
	
.KEY2_STEP
	JMP KEY2
	
.KEY3_STEP
	JMP KEY3
	
.KEY4_STEP
	JMP KEY4

.KEY5_STEP
	JMP KEY5
	
.KEY6_STEP
	JMP KEY6

.KEY_SHIFT6_STEP
	JMP KEY_SHIFT6
	
.KEY7_STEP	
	JMP KEY7

.KEY8_STEP	
	JMP KEY8

.KEY9_STEP
	JMP KEY9
.KEY_SHIFT9_STEP
	JMP KEY_SHIFT9	
	
@END

;=====KEY PRESS ROUTINES====
@START	
PASS
@START
	LDA #$01 						;SET TURN TO MOB & MOB GENERATION
	STA	GAME.MOB_GEN.CONTROL
	STA GAME.TURN.CONTROL
	JSR MOVE.PASS

	
.EXIT2
	LDA ANIMATION.FORCED.OVERRIDE
	CMP #$01						;IS ANIMATION FORCED OVERRIDE FLAG SET?
	BEQ .KEYCHECK					;IF YES, FORCE THE ABORT KEY PRESS CHECK
	LDA ANIMATION.FORCED
	CMP #$01						;IS ANIMATION FORCED FLAG SET?
	BEQ .EXIT.SKIP.KEYCHECK			;IF YES, DON'T ALLOW AN ABORT DUE TO KEY PRESS		
.KEYCHECK
	LDA KB_BUFFER					;LOAD NEXT KEY PRESS FROM BUFFER. WAS KEY PRESSED?
    BMI .EXIT.ALTERNATE				;IF YES, RETURN TO GAME LOOP VIA ALTERNATE ENTRANCE TO SKIP ANIMATION

.EXIT.SKIP.KEYCHECK
	LDA #$00
	STA ANIMATION.FORCED			;RESET TO OFF. IT'S ONLY TURNED ON IF PLAYER IS STANDING ON CERTAIN TERRAIN TYPE	
	JMP GAME.PRIMARY_LOOP 

.EXIT.ALTERNATE
	JMP GAME.PRIMARY_LOOP.ALTERNATE.ENTRANCE

@END
	
NORTH
@START
;=====================SUBROUTINE DOCUMENTATION====================================
;
;The NORTH/SOUTH/EAST/WEST subroutines are the entrance points to the 
;movement subroutines MOVE.NORTH/SOUTH/EAST/WEST
;
;They all have a simular structure. The purpose of .loop is to support
;commands like JUMP, and YELL (enable fast transport for horse, wyvern etc),
;Since the desired effect of those commands is for multiple player moves to
;occur, .loop is setup to call multiple player moves based on the value
;of PLAYER.TRANSPORT.SPEED
;
;PLAYER.TRANSPORT.SPEED was really designed with fast transport in mind, but
;JUMP uses it as a shoe horn to force two player moves to occur via .loop.
;
;In .EXIT there is a cleanup section that applies if this subroutine was 
;called by JUMP. In that event, PLAYER.TRANSPORT.SPEED needs to be returned
;to walking speed ($00), unless fast transport was enabled when the JUMP command
;was issue (i.e. a horse that is running (fast transport mode) should be able to jump
;at keep running)
;
;.EXIT2 has some logic which relates to the movement subroutines MOVE.NORTH/SOUTH/EAST/WEST
;When each of those routines ends, a keypress check is usually done so that animation can be
;skipped if the player is holding down the movement key. .EXIT2 make a 
;branch based on whether a key press has occured so that if a keypress has
;occured it returns to the game loop via the alternate entrace, which results in
;animation and mob generation being skipped. 
;
;Also note that the above is skipped entirely if ANIMATION.FORCED is set. This is
;because there are circumstances where animation is forced, no matter what.
;For example, if the screen is filled with all deep water, animation is forced
;because a statis screen doesn't look good under those circumstances. 
;
;=================================================================================

	LDA #$00
	STA PLAYER.MOVE.COUNTER			;INIT LOOP & MOVE COUNTER

.LOOP
;MULTIPLE MOVES	
	JSR MOVE.NORTH
	LDA #$00						;SET TURN TO PLAYER. THIS WAY MOBS ONLY GET TO MOVE ONCE, WHICH OCCURS AFTER THE PLAYERS 1ST MOVE AND BEFORE THE PLAYERS 2ND MOVE
	STA GAME.TURN.CONTROL
	INC PLAYER.MOVE.COUNTER			;INCREMENT LOOP & MOVE COUNTER
	LDA PLAYER.MOVE.COUNTER
	CMP PLAYER.TRANSPORT.SPEED
	BCS .EXIT
	JMP .LOOP

.EXIT
	
	LDA #$01 						;SET TURN TO MOB & MOB GENERATION
	STA	GAME.MOB_GEN.CONTROL
	STA GAME.TURN.CONTROL
	
	LDA #$8B						;SET TO UP ARROW (NORTH)
	STA PLAYER.COMMAND.LAST

;CLEAN UP (IF JUMP MOVE)	
	LDA PLAYER.MOVE.JUMP
	BEQ .EXIT2
	LDA #$00
	STA PLAYER.MOVE.JUMP
	
	LDX PLAYER.TRANSPORT.STATUS		;CHECK PLAYER TRANSPORT STATUS
	CPX #$01						;IS FAST TRANSPORT ENABLED?
	BCS .EXIT2						;IF YES, THEN EXIT
	STA PLAYER.TRANSPORT.SPEED		;IF NO, THEN TURN RETURN TRANSPORT SPEED TO $00 (1:1)

	
.EXIT2
	LDA ANIMATION.FORCED.OVERRIDE
	CMP #$01						;IS ANIMATION FORCED OVERRIDE FLAG SET?
	BEQ .KEYCHECK					;IF YES, FORCE THE ABORT KEY PRESS CHECK	

	LDA ANIMATION.FORCED
	CMP #$01						;IS ANIMATION FORCED FLAG SET?
	BEQ .EXIT.SKIP.KEYCHECK			;IF YES, DON'T ALLOW AN ABORT DUE TO KEY PRESS		

.KEYCHECK
	LDA KB_BUFFER					;LOAD NEXT KEY PRESS FROM BUFFER. WAS KEY PRESSED?
    BMI .EXIT.ALTERNATE				;IF YES, RETURN TO GAME LOOP VIA ALTERNATE ENTRANCE TO SKIP ANIMATION

.EXIT.SKIP.KEYCHECK
	LDA #$00
	STA ANIMATION.FORCED			;RESET TO OFF. IT'S ONLY TURNED ON IF PLAYER IS STANDING ON CERTAIN TERRAIN TYPE	
	JMP GAME.PRIMARY_LOOP 

.EXIT.ALTERNATE
	JMP GAME.PRIMARY_LOOP.ALTERNATE.ENTRANCE
	
@END
	
SOUTH
@START
;=====================SUBROUTINE DOCUMENTATION====================================
;
;See documentation above in subtroutine NORTH
;
;=================================================================================
	
	LDA #$00
	STA PLAYER.MOVE.COUNTER			;INIT LOOP & MOVE COUNTER

.LOOP
;MULTIPLE MOVES	
	JSR MOVE.SOUTH
	LDA #$00						;SET TURN TO PLAYER. THIS WAY MOBS ONLY GET TO MOVE ONCE, WHICH OCCURS AFTER THE PLAYERS 1ST MOVE AND BEFORE THE PLAYERS 2ND MOVE
	STA GAME.TURN.CONTROL
	INC PLAYER.MOVE.COUNTER			;INCREMENT LOOP & MOVE COUNTER
	LDA PLAYER.MOVE.COUNTER
	CMP PLAYER.TRANSPORT.SPEED
	BCS .EXIT
	JMP .LOOP
	
.EXIT	
	LDA #$01 						;SET TURN TO MOB & MOB GENERATION
	STA	GAME.MOB_GEN.CONTROL
	STA GAME.TURN.CONTROL	

	LDA #$8A						;SET TO DOWN ARROW (SOUTH)
	STA PLAYER.COMMAND.LAST
	
	LDA PLAYER.MOVE.JUMP
	BEQ .EXIT2
	LDA #$00
	STA PLAYER.MOVE.JUMP
	
	LDX PLAYER.TRANSPORT.STATUS		;CHECK PLAYER TRANSPORT STATUS
	CPX #$01						;IS FAST TRANSPORT ENABLED?
	BCS .EXIT2						;IF YES, THEN EXIT
	STA PLAYER.TRANSPORT.SPEED		;IF NO, THEN TURN RETURN TRANSPORT SPEED TO $00 (1:1)

	
.EXIT2
	LDA ANIMATION.FORCED.OVERRIDE
	CMP #$01						;IS ANIMATION FORCED OVERRIDE FLAG SET?
	BEQ .KEYCHECK					;IF YES, FORCE THE ABORT KEY PRESS CHECK	



	LDA ANIMATION.FORCED
	CMP #$01						;IS ANIMATION FORCED FLAG SET?
	BEQ .EXIT.SKIP.KEYCHECK			;IF YES, DON'T ALLOW AN ABORT DUE TO KEY PRESS		

.KEYCHECK
	LDA KB_BUFFER					;LOAD NEXT KEY PRESS FROM BUFFER. WAS KEY PRESSED?
    BMI .EXIT.ALTERNATE				;IF YES, RETURN TO GAME LOOP VIA ALTERNATE ENTRANCE TO SKIP ANIMATION

.EXIT.SKIP.KEYCHECK
	LDA #$00
	STA ANIMATION.FORCED			;RESET TO OFF. IT'S ONLY TURNED ON IF PLAYER IS STANDING ON CERTAIN TERRAIN TYPE	
	JMP GAME.PRIMARY_LOOP 

.EXIT.ALTERNATE
	JMP GAME.PRIMARY_LOOP.ALTERNATE.ENTRANCE

@END
	
EAST
@START
;=====================SUBROUTINE DOCUMENTATION====================================
;
;See documentation above in subtroutine NORTH
;
;=================================================================================

	LDA #$00
	STA PLAYER.MOVE.COUNTER			;INIT LOOP & MOVE COUNTER

.LOOP
;MULTIPLE MOVES	
	JSR MOVE.EAST
	LDA #$00						;SET TURN TO PLAYER. THIS WAY MOBS ONLY GET TO MOVE ONCE, WHICH OCCURS AFTER THE PLAYERS 1ST MOVE AND BEFORE THE PLAYERS 2ND MOVE
	STA GAME.TURN.CONTROL
	INC PLAYER.MOVE.COUNTER			;INCREMENT LOOP & MOVE COUNTER
	LDA PLAYER.MOVE.COUNTER
	CMP PLAYER.TRANSPORT.SPEED
	BCS .EXIT
	JMP .LOOP

.EXIT	
	LDA #$01 						;SET TURN TO MOB & MOB GENERATION
	STA	GAME.MOB_GEN.CONTROL
	STA GAME.TURN.CONTROL
	
	LDA #$95						;SET TO RIGHT ARROW (EAST)
	STA PLAYER.COMMAND.LAST
	
	LDA PLAYER.MOVE.JUMP
	BEQ .EXIT2
	LDA #$00
	STA PLAYER.MOVE.JUMP
	
	LDX PLAYER.TRANSPORT.STATUS		;CHECK PLAYER TRANSPORT STATUS
	CPX #$01						;IS FAST TRANSPORT ENABLED?
	BCS .EXIT2						;IF YES, THEN EXIT
	STA PLAYER.TRANSPORT.SPEED		;IF NO, THEN TURN RETURN TRANSPORT SPEED TO $00 (1:1)

	
.EXIT2
	LDA ANIMATION.FORCED.OVERRIDE
	CMP #$01						;IS ANIMATION FORCED OVERRIDE FLAG SET?
	BEQ .KEYCHECK					;IF YES, FORCE THE ABORT KEY PRESS CHECK
	LDA ANIMATION.FORCED
	CMP #$01						;IS ANIMATION FORCED FLAG SET?
	BEQ .EXIT.SKIP.KEYCHECK			;IF YES, DON'T ALLOW AN ABORT DUE TO KEY PRESS		
.KEYCHECK
	LDA KB_BUFFER					;LOAD NEXT KEY PRESS FROM BUFFER. WAS KEY PRESSED?
    BMI .EXIT.ALTERNATE				;IF YES, RETURN TO GAME LOOP VIA ALTERNATE ENTRANCE TO SKIP ANIMATION

.EXIT.SKIP.KEYCHECK
	LDA #$00
	STA ANIMATION.FORCED			;RESET TO OFF. IT'S ONLY TURNED ON IF PLAYER IS STANDING ON CERTAIN TERRAIN TYPE	
	JMP GAME.PRIMARY_LOOP 

.EXIT.ALTERNATE
	JMP GAME.PRIMARY_LOOP.ALTERNATE.ENTRANCE

@END

WEST 
@START	
;=====================SUBROUTINE DOCUMENTATION====================================
;
;See documentation above in subtroutine NORTH
;
;=================================================================================
	
	LDA #$00
	STA PLAYER.MOVE.COUNTER			;INIT LOOP & MOVE COUNTER

.LOOP
;MULTIPLE MOVES	
	JSR MOVE.WEST
	LDA #$00						;SET TURN TO PLAYER. THIS WAY MOBS ONLY GET TO MOVE ONCE, WHICH OCCURS AFTER THE PLAYERS 1ST MOVE AND BEFORE THE PLAYERS 2ND MOVE
	STA GAME.TURN.CONTROL
	INC PLAYER.MOVE.COUNTER			;INCREMENT LOOP & MOVE COUNTER
	LDA PLAYER.MOVE.COUNTER
	CMP PLAYER.TRANSPORT.SPEED
	BCS .EXIT
	JMP .LOOP
	
.EXIT		
	LDA #$01 						;SET TURN TO MOB & MOB GENERATION
	STA	GAME.MOB_GEN.CONTROL
	STA GAME.TURN.CONTROL
	
	LDA #$8B						;SET TO UP ARROW (NORTH)
	STA PLAYER.COMMAND.LAST
	
	LDA PLAYER.MOVE.JUMP
	BEQ .EXIT2
	LDA #$00
	STA PLAYER.MOVE.JUMP
	
	LDX PLAYER.TRANSPORT.STATUS		;CHECK PLAYER TRANSPORT STATUS
	CPX #$01						;IS FAST TRANSPORT ENABLED?
	BCS .EXIT2						;IF YES, THEN EXIT
	STA PLAYER.TRANSPORT.SPEED		;IF NO, THEN TURN RETURN TRANSPORT SPEED TO $00 (1:1)

	
.EXIT2
	LDA ANIMATION.FORCED.OVERRIDE
	CMP #$01						;IS ANIMATION FORCED OVERRIDE FLAG SET?
	BEQ .KEYCHECK					;IF YES, FORCE THE ABORT KEY PRESS CHECK
	LDA ANIMATION.FORCED
	CMP #$01						;IS ANIMATION FORCED FLAG SET?
	BEQ .EXIT.SKIP.KEYCHECK			;IF YES, DON'T ALLOW AN ABORT DUE TO KEY PRESS		
.KEYCHECK
	LDA KB_BUFFER					;LOAD NEXT KEY PRESS FROM BUFFER. WAS KEY PRESSED?
    BMI .EXIT.ALTERNATE				;IF YES, RETURN TO GAME LOOP VIA ALTERNATE ENTRANCE TO SKIP ANIMATION

.EXIT.SKIP.KEYCHECK
	LDA #$00
	STA ANIMATION.FORCED			;RESET TO OFF. IT'S ONLY TURNED ON IF PLAYER IS STANDING ON CERTAIN TERRAIN TYPE	
	JMP GAME.PRIMARY_LOOP 

.EXIT.ALTERNATE
	JMP GAME.PRIMARY_LOOP.ALTERNATE.ENTRANCE

@END

BOARD
@START
;IF JUMP IS ENABLED, DISABLE IT.  **include at start of all non-movement command subroutines**
;(this occurs if the player pressed (J)ump and then presses a non-movement key)

	LDA PLAYER.MOVE.JUMP			
	BEQ .CONTINUE					
	LDA #$00
	STA PLAYER.MOVE.JUMP

.CONTINUE 	
	JSR MO.BOARD
		JMP GAME.LAUNCH							;ALL NON-MOVEMENT COMMANDS MUST EXIT VIA JMP TO GAME.LAUNCH TO AVOID PROBLEMS WITH KEY PRESS ABORTS. SEE SUBROUTINE DOCUMENTATION FOR GAME.PRIMARY_LOOP FOR DETAILS.

@END

JUMP
@START

;VERIFY PLAYER IS ON A HORSE
	LDA PLAYER.TILE.ACTIVE
	CMP #TILE_ID.HORSE_C						;BRANCH BASED ON THE TYPE OF TRASNPORT MO AT THE PLAYER LOCATION
	BNE .NOT_ON_HORSE
	
.ALLOW.JUMP	
;ENABLE JUMP	
	LDA #$01								;LET THE COLLISSION CONTROLS (IN MOVEMENT_MANAGER.ASM) KNOW THAT HORSE IS JUMPING
	STA PLAYER.MOVE.JUMP
	
	LDA #$02								;SET SPEED TO 2:1
	STA PLAYER.TRANSPORT.SPEED	

	LDA #$CA								;SET LAST COMMAND TO 'J'
	STA PLAYER.COMMAND.LAST
	
;CHOOSE DIRECTION OF JUMP	
	;THIS IS DONE BY THE NEXT KEY PRESS FROM THE GAME LOOP. THE MOVEMENT COMMANDS WILL READ THE PLAYER.MOVE.JUMP VARIABLE TO DETECT THAT IT'S A JUMP MOVE. 
	JMP .EXIT
	
.NOT_ON_HORSE
	JSR PLAY.SOUND.DUMB_ASS
	
.EXIT
	JMP GAME.LAUNCH							;ALL NON-MOVEMENT COMMANDS MUST EXIT VIA JMP TO GAME.LAUNCH TO AVOID PROBLEMS WITH KEY PRESS ABORTS. SEE SUBROUTINE DOCUMENTATION FOR GAME.PRIMARY_LOOP FOR DETAILS.

	
@END

OPEN.COMMAND ;open/unlock doors, operate portcullis levers
@START
;SYNC BACKGROUND AND FOREGROUND GRAPHICS PAGES
;(prep for when KEYIN.ANIMATION is called, as animation requires the pages to be in sync)

			;**OPT** Memory. Speed. I think this can be removed if I remove the key press abort skipping of copy.screen at the end of the common move routine, which I will probably have to do to get animation for just mobs working even in a key press abort scenario. 
	;ENABLE BANK-SWITCHED RAM ($D000 Bank 1)
		LDA $C08B					;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK1, 1ST)
		LDA $C08B	
	JSR COPY.SCREEN		;**DON'T REMOVE** 
		;RE-ENABLE BANK-SWITCHED RAM ($D000 Bank 2)
		LDA $C083				;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK2 2nd)
		LDA $C083

;START 

	;GET DIRECTION
	JSR PLAYER.INPUT.COMMAND_DIRECTION
		;Y-REG = (not needed) screen tile location of adjacent screen tile in direction selected
		;ACC = screen tile location +2 screen tiles in direction selected
		;ACC = $FF if directon key wasn't proceed (invalid command)
	CMP #$FF	;was valid directon key entered by player?
	BNE .CHECK.OBJECT.TYPE	;if yes, the continue to process this command

	;** FALLS THROUGH** ;if no, then exit via invalid command routine
	
.INVALID.COMMAND
;command not valid because direction selected is not valid or because a closed door
;was not found adjacent to the player in the direction selected.
	JSR PLAY.SOUND.DUMB_ASS
	JMP GAME.LAUNCH							;ALL NON-MOVEMENT COMMANDS MUST EXIT VIA JMP TO GAME.LAUNCH TO AVOID PROBLEMS WITH KEY PRESS ABORTS. SEE SUBROUTINE DOCUMENTATION FOR GAME.PRIMARY_LOOP FOR DETAILS.

.CHECK.OBJECT.TYPE	
	LDX SCREEN.MO_GENERAL.DATA,Y	;load general map object data for current tile location
	CPX #$FF						;is a general map object present?
	BEQ .INVALID.COMMAND			;if no, then nothing to open. invalid command
	
	LDA MAP_OBJECTS.GENERAL+$3,X	;load data byte of general map object record
	CMP #$10						;is there a closed unlocked door at the location the player is trying to "open"
	BEQ .OPEN.CLOSED.UNLOCKED.DOOR	;if yes, then open it
	CMP #$11						;is there a closed locked door at the specified location?  	
	BEQ .OPEN.CLOSED.LOCKED.DOOR	;if yes, then unlock it
	CMP #MO.PORTCULLIS.LEFT			;is there a portcullis lever (set left) at the specified location?  	
	BEQ .SWITCH.PORTCULLIS.LEVER.RIGHT	;if yes, then switch it
	CMP #MO.PORTCULLIS.RIGHT		;is there a portcullis lever (set left) at the specified location?  	
	BEQ .SWITCH.PORTCULLIS.LEVER.LEFT	;if yes, then switch it
	
	JMP .INVALID.COMMAND
	
.OPEN.CLOSED.UNLOCKED.DOOR
	LDA #MO.DOOR.OPEN.START		;set data byte to code for open door timer with 4 moves
	STA MAP_OBJECTS.GENERAL+$3,X
	JMP GAME.LAUNCH
	
.OPEN.CLOSED.LOCKED.DOOR
	;SET DOOR TO UNLOCKED DOOR IN MAP OBJECT RECORD
	LDA #$10
	STA MAP_OBJECTS.GENERAL+$3,X

	;SWITCH DOOR TILE TO UNLOCKED
	DEC MAP_OBJECTS.GENERAL+$2,X
	
	JMP .EXIT
	
.SWITCH.PORTCULLIS.LEVER.RIGHT
	;SWITCH LEVER
	INC MAP_OBJECTS.GENERAL+$2,X ;add 1 to tile_ID of object
	INC MAP_OBJECTS.GENERAL+$3,X ;ADD 1 to data byte of object

	;CHANGE PORTCULIS TILE TO UP/RAISED (VIA SKIP DRAW FLAG)
	TXA
	SEC
	SBC #MAP_OBJECTS.RECORD_LENGTH	;move index to one record before the lever, which should be the record for the portcullis
	TAX
	LDA #MO.SKIP.DRAW.FLAG			;load skip draw flag to simulate raised/up portcullis 
	STA MAP_OBJECTS.GENERAL+$3,X	;save tile_id to the map object record of portcullis
	
	JMP .EXIT
	
.SWITCH.PORTCULLIS.LEVER.LEFT
	;SWITCH LEVER
	DEC MAP_OBJECTS.GENERAL+$2,X ;subtract 1 from tile_ID of object
	DEC MAP_OBJECTS.GENERAL+$3,X ;subtract 1 from data byte of object

	;CHANGE PORTCULIS TO DOWN/LOWERED
	TXA
	SEC
	SBC #MAP_OBJECTS.RECORD_LENGTH	;move index to one record before the lever, which should be the record for the portcullis
	TAX
	LDA #MO.PORTCULLIS.LOWERED
	STA MAP_OBJECTS.GENERAL+$3,X	;save flag status map object record of portcullis
	
	;**FALLS THROUGH**
	
.EXIT
	JMP GAME.LAUNCH

	
@END

PUSH.COMMAND ;push map objects to new location
@START

			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP3
			; LDA TEXT
			; LDA $C082				;READ ENABLE ROM, DISABLE WRITE ON BSM (NORMAL STATE).
									; ;REQUIRED FOR BEFORE BRK BECAUSE APPLE MONITOR IS A ROM ROUTINE
			; JSR CLEAR.TEXT.SCREEN
			; BRK
; .TEMP3
			; LDA TEMP
			
;SET CURRENT COMMAND
	LDA #$D0			;(P) PUSH
	STA PLAYER.COMMAND.CURRENT
	
;SYNC BACKGROUND AND FOREGROUND GRAPHICS PAGES
;(prep for when KEYIN.ANIMATION is called, as animation requires the pages to be in sync)

			;**OPT** Memory. Speed. I think this can be removed if I remove the key press abort skipping of copy.screen at the end of the common move routine, which I will probably have to do to get animation for just mobs working even in a key press abort scenario. 
	;ENABLE BANK-SWITCHED RAM ($D000 Bank 1)
		LDA $C08B					;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK1, 1ST)
		LDA $C08B	
	JSR COPY.SCREEN		;**DON'T REMOVE** 
		;RE-ENABLE BANK-SWITCHED RAM ($D000 Bank 2)
		LDA $C083				;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK2 2nd)
		LDA $C083

;START 		
	LDA PLAYER.TRANSPORT.ACTIVE	;load player transport status
	CMP #$FF					;does player have active transport?
	BNE .INVALID.COMMAND		;if yes, then push cannot be executed. 
	
	JSR PLAYER.INPUT.COMMAND_DIRECTION
		;Y-REG = screen tile location of adjacent screen tile in direction selected
		;ACC = screen tile location +2 screen tiles in direction selected
		;ACC = $FF if directon key wasn't proceed (invalid command)
	CMP #$FF	;was valid directon key entered by player?
	BNE .CHECK.OBJECT.TYPE	;if yes, the continue to process this command
	JMP .INVALID.COMMAND ;if no, then exit via invalid command routine
		
.CHECK.OBJECT.TYPE	
	;ACC = PLAYER SCREEN LOCATION +2 IN DIRECTION SELECTED
	STA PLAYER.MOVE.CANDIDATE_TILE_LOC
	
	LDX SCREEN.MO_GENERAL.DATA,Y ;load general map object record index from screen array, location = offset 1 tile from player location in the direction of the push command. 	
	CPX #$FF					;does adjacent tile contain a general map object?
	BEQ .INVALID.COMMAND		;if not, then push command is not valid

	;IS TRANSPORT OBJECT?
	LDA MAP_OBJECTS.GENERAL+$2,X ;load tile type of map object
	CMP #TILE_ID.HORSE_C						;BRANCH BASED ON THE TYPE OF TRASNPORT MO AT THE PLAYER LOCATION
	BEQ .INVALID.COMMAND		;if yes, then push command cannot be executed on that object type
	CMP #TILE_ID.FRIGATE1.1
	BEQ .INVALID.COMMAND		;if yes, then push command cannot be executed on that object type
	CMP #TILE_ID.CARAVEL
	BEQ .INVALID.COMMAND		;if yes, then push command cannot be executed on that object type
	CMP #TILE_ID.WYVERN
	BEQ .INVALID.COMMAND		;if yes, then push command cannot be executed on that object type
	CMP #TILE_ID.SKIFF
	BEQ .INVALID.COMMAND		;if yes, then push command cannot be executed on that object type	
		;**OPT** Memory. Speed. This section could be shorter if transport objects could be identified by byte $3 (use $01-$03 for skiff values, with $01 being no skiffs, so boats are confuse with $00 as a default value), or if the TILE IDs for transport were in a range. 

	LDA MAP_OBJECTS.GENERAL+$3,X	;load data byte of general map object record			
	CMP #MO.NOT_PUSHABLE.GRE		;is object pushable?
	BCS .INVALID.COMMAND			;if no, then push command cannot be executed on that object type	

	;**FALLS THROUGH**

.CHECK.OBJECT.DESTINATION
		;PARM: PLAYER.MOVE.CANDIDATE_TILE_LOC previously set 
		 
	JSR PLAYER.COLLISSION.CHECK	;use player collision check routine on the destination tile of the map object, net of the push		
	;return value in ACC ($00 = open, $01 = blocked)
	TAY ;save collision return code

;CLEAR COMMAND VARIABLES (they were only needed for PLAYER.COLLISION.CHECK)
	LDA #$D0			;(P) PUSH
	STA PLAYER.COMMAND.LAST
	LDA #$00			;(P) PUSH
	STA PLAYER.COMMAND.CURRENT
	
	CPY #$00 ;restore collision return code ($00 = open, $01 = blocked)
	BEQ	.CHANGE.OBJECT.LOCATION	;move is permitted, proceed to move map object
								;if move is note permitted, player and object swap locations

	;**FALLS THROUGH**

.SWAP.OBJECT.LOCATION
;swap object and player location
	LDA SAVED.ACC.GLOBAL1	;restore ASCII code of direction key pressed
	CMP #$8B			;UP ARROW
	BEQ .SWAP_NORTH
	CMP #$8A			;DOWN ARROW
	BEQ .SWAP_SOUTH
	CMP #$95			;RIGHT ARROW
	BEQ .SWAP_EAST
	CMP #$88			;LEFT ARROW
	BEQ	.SWAP_WEST 
	;**SHOULD NEVER FALL THROUGH**
	;(I don't think an error trap is needed as this section is exactly like the one above which does have an error trap in the form of report an invalid command to the player)

.SWAP_NORTH
	INC MAP_OBJECTS.GENERAL+$1,X			;+1 TO Y POSITION
		
	JMP NORTH ;execute player move in direction of push	


.INVALID.COMMAND
;SET LAST COMMAND
	LDA #$D0			;(P) PUSH
	STA PLAYER.COMMAND.LAST

;CLEAR CURRENT COMMAND ;**OPT** Memory. Speed. Need to clear current command until all command subroutines set the PLAYER.CURRENT.COMMAND variable. 
	LDA #$00			;(P) PUSH
	STA PLAYER.COMMAND.CURRENT
	
	JSR PLAY.SOUND.DUMB_ASS
	JMP GAME.LAUNCH							;ALL NON-MOVEMENT COMMANDS MUST EXIT VIA JMP TO GAME.LAUNCH TO AVOID PROBLEMS WITH KEY PRESS ABORTS. SEE SUBROUTINE DOCUMENTATION FOR GAME.PRIMARY_LOOP FOR DETAILS.

		
.SWAP_SOUTH
	DEC MAP_OBJECTS.GENERAL+$1,X			;-1 TO Y POSITION	
	JMP SOUTH ;execute player move in direction of push	

.SWAP_EAST
	DEC MAP_OBJECTS.GENERAL,X				;-1 TO X POSITION
	JMP EAST ;execute player move in direction of push	


.SWAP_WEST
	INC MAP_OBJECTS.GENERAL,X				;+1 TO X POSITION
	JMP WEST ;execute player move in direction of push	
		
	
.CHANGE.OBJECT.LOCATION
	LDA SAVED.ACC.GLOBAL1	;restore ASCII code of direction key pressed
	CMP #$8B			;UP ARROW
	BEQ .PUSH_NORTH
	CMP #$8A			;DOWN ARROW
	BEQ .PUSH_SOUTH
	CMP #$95			;RIGHT ARROW
	BEQ .PUSH_EAST
	CMP #$88			;LEFT ARROW
	BEQ	.PUSH_WEST 
	;**SHOULD NEVER FALL THROUGH**
	;(I don't think an error trap is needed as this section is exactly like the one above which does have an error trap in the form of report an invalid command to the player)

.PUSH_NORTH
	DEC MAP_OBJECTS.GENERAL+$1,X			;-1 TO Y POSITION	
		
	JMP NORTH ;execute player move in direction of push	
	
.PUSH_SOUTH
	INC MAP_OBJECTS.GENERAL+$1,X			;+1 TO Y POSITION
	JMP SOUTH ;execute player move in direction of push	

.PUSH_EAST
	INC MAP_OBJECTS.GENERAL,X				;+1 TO X POSITION
	JMP EAST ;execute player move in direction of push	


.PUSH_WEST
	DEC MAP_OBJECTS.GENERAL,X				;-1 TO X POSITION
		; JSR PREP.BRK
		; LDA MAP_OBJECTS.GENERAL,X
		; LDY #$AB
		; BRK
	JMP WEST ;execute player move in direction of push	
	
@END
	
;QUIT: SEE PROGRAM START

TALK.COMMAND
@START

			;**OPT** Memory. Speed. I think this can be removed if I remove the key press abort skipping of copy.screen at the end of the common move routine, which I will probably have to do to get animation for just mobs working even in a key press abort scenario. 
	;ENABLE BANK-SWITCHED RAM ($D000 Bank 1)
		LDA $C08B					;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK1, 1ST)
		LDA $C08B	
	JSR COPY.SCREEN		;**DON'T REMOVE** 
		;RE-ENABLE BANK-SWITCHED RAM ($D000 Bank 2)
		LDA $C083				;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK2 2nd)
		LDA $C083

		
	;GET DIRECTION
	JSR PLAYER.INPUT.COMMAND_DIRECTION
		;Y-REG = screen tile location of adjacent screen tile in direction selected
		;ACC = screen tile location +2 screen tiles in direction selected
		;ACC = $FF if directon key wasn't pressed (invalid command)
	CMP #$FF	;was valid directon key entered by player?
	BNE .CHECK.FOR_NPC	;if yes, the continue to process this command

	;** FALLS THROUGH** ;if no, then exit via invalid command routine
	
.INVALID.COMMAND
;command not valid because direction selected is not valid or because a closed door
;was not found adjacent to the player in the direction selected.
	JSR PLAY.SOUND.DUMB_ASS
	JMP GAME.LAUNCH ;**OPT** Memory. The .INVALID.COMMAND sections for each key command could probably be consolidated into INVALID.COMMAND, a parent label, and then just JMP to it from each command. The extra JMP is 3 bytes and the .INVALID.COMMAND sections are using 6 bytes, so there should be a 50% memory savings for each one consolidated.							;ALL NON-MOVEMENT COMMANDS MUST EXIT VIA JMP TO GAME.LAUNCH TO AVOID PROBLEMS WITH KEY PRESS ABORTS. SEE SUBROUTINE DOCUMENTATION FOR GAME.PRIMARY_LOOP FOR DETAILS.

.CHECK.FOR_NPC	
	LDX SCREEN.MO_SPRITE.DATA,Y	;load sprite map object data for current tile location
	CPX #$FF						;is a sprite map object present?
	BEQ .INVALID.COMMAND			;if no, then nothing nobody to talk to. exit via invalid command
	
	LDA SCREEN.MO_SPRITE_TYPE.DATA,Y	;load sprite type
	BEQ .INVALID.COMMAND			;if not an NPC, then nobody to talk to. exit via invalid command
	;STX NPC.TALK.RECORD				;if an NPC, save the MO record index and proceed to process the TALK command
	;**FALLS THROUGH**
	
			
.LOAD.SWAP.ROUTINE
;LOAD NPC.TALK FILE INTO THE MAIN MEMORY SWAP SPACE

	JSR SWAP.MAIN_MEMORY.OUT
	
;LOAD FILE "SRTN.NPC.TALK"
			
;set command type (READ | WRITE | SEEK)
; cmd_seek.current_drive 	.EQ $90
; cmd_read.current_drive  	.EQ $91
; cmd_write.current_drive 	.EQ $92
;			
; cmd_read.drive1			.EQ $1
; cmd_read.drive2		  	.EQ $81
; cmd_write.drive1			.EQ $2
; cmd_write.drive2			.EQ $82

	lda #cmd_read.drive2
	sta parm.reqcmd
	
;set destination memory address
	lda #SRTN.NPC.TALK.ADDRESS	
	sta parm.ldrlo
	lda /SRTN.NPC.TALK.ADDRESS		
	sta parm.ldrhi

;set read length (bytes)
	lda #$FF				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda #$FF				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi
	
;set filename to read from	
	lda #SRTN.NPC.TALK	;load LO address
	sta parm.namlo
	lda /SRTN.NPC.TALK	;load HO address
	sta parm.namhi 
	
		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO	

		;-XREG contains map object record # of the Active NPC
		;-YREG contains Active NPC screen tile location when NPC.TALK is called
	JSR NPC.TALK
		

.EXIT
	
	JSR SWAP.MAIN_MEMORY.IN
		
		; LDA EVENT.FLAGS+$02
		; CMP #$01
		; BNE .FLAG.NOT.SET		
		; LDA #$24
		; STA MAP_OBJECTS.GENERAL+$03

		; ; LDA #$AA
		; ; JSR PREP.BRK
		
		; ; BRK
; .FLAG.NOT.SET

			
			; JSR PREP.BRK
			; BRK
	
	JMP GAME.LAUNCH							;ALL NON-MOVEMENT COMMANDS MUST EXIT VIA JMP TO GAME.LAUNCH TO AVOID PROBLEMS WITH KEY PRESS ABORTS. SEE SUBROUTINE DOCUMENTATION FOR GAME.PRIMARY_LOOP FOR DETAILS.

	
	
@END

YELL ;ENABLED FAST TRANSPORT MODE, IF AVAILABLE
@START
;IF JUMP IS ENABLED, DISABLE IT.  **include at start of all non-movement command subroutines**
;(this occurs if the player pressed (J)ump and then presses a non-movement key)
	LDA PLAYER.MOVE.JUMP			
	BEQ .CONTINUE					
	LDA #$00
	STA PLAYER.MOVE.JUMP

.CONTINUE 
	LDA PLAYER.TILE.ACTIVE
	CMP #TILE_ID.HORSE_C
	BEQ .HORSE
	CMP #TILE_ID.WYVERN
	BEQ .WYVERN
	JMP .YELL_AT_WHAT


.HORSE	
	LDA PLAYER.TRANSPORT.SPEED
	BEQ .CURRENT_SPEED0.0
.CURRENT_SPEED1.0						;1 PLAYER MOVE PER 1 MOB MOVE
	LDA #$00
	STA PLAYER.TRANSPORT.SPEED
	STA PLAYER.TRANSPORT.STATUS	
	JMP .EXIT
	
.CURRENT_SPEED0.0						;2 PLAYER MOVE PER 1 MOB MOVE
	LDA #$02
	STA PLAYER.TRANSPORT.SPEED
	STA PLAYER.TRANSPORT.STATUS	
	JMP .EXIT

.WYVERN
	LDA PLAYER.TRANSPORT.SPEED
	BEQ .CURRENT_SPEED0.1
.CURRENT_SPEED1.1						;1 PLAYER MOVE PER 1 MOB MOVE
	LDA #$00
	STA PLAYER.TRANSPORT.SPEED
	STA PLAYER.TRANSPORT.STATUS	
	STA PLAYER.DARKNESS_OVERRIDE

	JMP GAME.LAUNCH						;DO A FRESH SCREEN DRAW TO CREATE ANY DARK TILES
	
.CURRENT_SPEED0.1						;4 PLAYER MOVE PER 1 MOB MOVE
	LDA #$04
	STA PLAYER.TRANSPORT.SPEED
	STA PLAYER.TRANSPORT.STATUS	
	STA PLAYER.DARKNESS_OVERRIDE
	JMP GAME.LAUNCH						;DO A FRESH SCREEN DRAW TO DRAW ANY DARK TILES. 
	
.YELL_AT_WHAT
	JSR PLAY.SOUND.DUMB_ASS				;FUTURE ON SCREEN TEXT "YELL AT WHAT? HORSE"

.EXIT
	JMP GAME.LAUNCH							;ALL NON-MOVEMENT COMMANDS MUST EXIT VIA JMP TO GAME.LAUNCH TO AVOID PROBLEMS WITH KEY PRESS ABORTS. SEE SUBROUTINE DOCUMENTATION FOR GAME.PRIMARY_LOOP FOR DETAILS.

@END
	
XIT
@START
;IF JUMP IS ENABLED, DISABLE IT.  **include at start of all non-movement command subroutines**
;(this occurs if the player pressed (J)ump and then presses a non-movement key)
	LDA PLAYER.MOVE.JUMP			
	BEQ .CONTINUE					
	LDA #$00
	STA PLAYER.MOVE.JUMP

.CONTINUE 
	JSR MO.XIT
	JMP GAME.LAUNCH							;ALL NON-MOVEMENT COMMANDS MUST EXIT VIA JMP TO GAME.LAUNCH TO AVOID PROBLEMS WITH KEY PRESS ABORTS. SEE SUBROUTINE DOCUMENTATION FOR GAME.PRIMARY_LOOP FOR DETAILS.


@END

;==========PLAY TEST KEYS=
@START
KEY0 ;=====SET TROUBLESHOOTING HOOK=======
@START
;CODE CAN BE INSERTED IN OTHER ROUTINES TO BREAK IF THE HOOK IS SET.
;SET TROUBLESHOOTING HOOK
	LDA TROUBLESHOOTING.HOOK
	CMP #$01
	BNE .NOT1
	LDA #$00	;$00 = HOOK CLEAR
	STA TROUBLESHOOTING.HOOK
	JMP GAME.PRIMARY_LOOP
.NOT1
	LDA #$01	;$01 = HOOK SET
	STA TROUBLESHOOTING.HOOK
	JMP GAME.PRIMARY_LOOP

KEY_SHIFT0 ;=====SET TROUBLESHOOTING HOOK2=======
@START
;CODE CAN BE INSERTED IN OTHER ROUTINES TO BREAK IF THE HOOK IS SET.
;SET TROUBLESHOOTING HOOK
	LDA TROUBLESHOOTING.HOOK2
	CMP #$01
	BNE .NOT1
	LDA #$00	;$00 = HOOK CLEAR
	STA TROUBLESHOOTING.HOOK2
	JMP GAME.PRIMARY_LOOP
.NOT1
	LDA #$01	;$01 = HOOK SET
	STA TROUBLESHOOTING.HOOK2
	JMP GAME.PRIMARY_LOOP
	
@END

KEY1 ;=====PLAYER COLLISSION OVERRIDE TOGGLE=======
@START
;IF OVERRIDE IS SET, PLAYER CAN TRAVERSE ANY TILE, INCLUDING MAP OBJECTS
	LDA PLAYER.COLLISSION_OVERRIDE
	CMP #$01
	BNE .NOT1
	LDA #$00
	STA PLAYER.COLLISSION_OVERRIDE
	JMP GAME.PRIMARY_LOOP
.NOT1
	LDA #$01
	STA PLAYER.COLLISSION_OVERRIDE
	JMP GAME.PRIMARY_LOOP

@END
	
KEY2 ;=====MOB COLLISSION OVERRIDE TOGGLE=======
@START
;IF OVERRIDE IS SET, MOB CAN TRAVERSE ANY TILE, INCLUDING MAP OBJECTS (NOT SURE WHAT EFFECT WOULD RESULT FROM THE LATTER)
	LDA MOB.COLLISION_OVERRIDE
	CMP #$01
	BNE .NOT1
	LDA #$00
	STA MOB.COLLISION_OVERRIDE
	JMP GAME.PRIMARY_LOOP
.NOT1
	LDA #$01
	STA MOB.COLLISION_OVERRIDE
	JMP GAME.PRIMARY_LOOP

@END

KEY3 ;=====ZAP SPRITE========
@START
			;**OPT** Memory. Speed. I think this can be removed if I remove the key press abort skipping of copy.screen at the end of the common move routine, which I will probably have to do to get animation for just mobs working even in a key press abort scenario. 
	;ENABLE BANK-SWITCHED RAM ($D000 Bank 1)
		LDA $C08B					;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK1, 1ST)
		LDA $C08B	
	JSR COPY.SCREEN		;**DON'T REMOVE** 
		;RE-ENABLE BANK-SWITCHED RAM ($D000 Bank 2)
		LDA $C083				;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK2 2nd)
		LDA $C083
		
	JSR KEYIN
	CMP #$8B			;UP ARROW
	BEQ .ZAP_NORTH
	CMP #$8A			;DOWN ARROW
	BEQ .ZAP_SOUTH
	CMP #$95			;RIGHT ARROW
	BEQ .ZAP_EAST
	CMP #$88			;LEFT ARROW
	BEQ	.ZAP_WEST

.ZAP_NORTH
	LDY #$4C
	JMP .SEARCH.ADJACENT_SPRITE
	
.ZAP_SOUTH	
	LDY #$6E
	JMP .SEARCH.ADJACENT_SPRITE
	
.ZAP_EAST
	LDY #$5E
	JMP .SEARCH.ADJACENT_SPRITE
 
.ZAP_WEST
	LDY #$5C
;****FALLS THROUGH

.SEARCH.ADJACENT_SPRITE

;IS THERE A SPRITE IN DIRECTION OF ATTACK?	
	LDA SCREEN.MO_SPRITE.DATA,Y					;is a mob located at target tile?
	CMP #$FF
	BEQ .NO_SPRITE
	TAX

;IS SPRITE MOB OR NPC?	
	LDA SCREEN.MO_SPRITE_TYPE.DATA,Y			;load sprite type flag from screen array
	CMP #$01	;is sprite an NPC?
	BEQ .PROCESS.NPC.KILL	;if yes, well, you get the idea
	;**FALLS THROUGH**		;if no, then SPRITE is a MOB. Delete MOB record.

.PROCESS.MOB.KILL	
;DELETE.MOB.RECORD	
	LDA #$00
	STA MAP_OBJECTS.MOB,X		;erase mob's map object record
	STA MAP_OBJECTS.MOB+$1,X
	STA MAP_OBJECTS.MOB+$2,X
	STA MAP_OBJECTS.MOB+$3,X
	LDA #$FF
	STA SCREEN.MO_SPRITE.DATA,Y	;erase mob from screen array
	STA SCREEN.MO_SPRITE_TYPE.DATA,Y

		; JSR PREP.BRK
		; BRK
		
	JMP GAME.LAUNCH		;redraw screen at current gmap position. mob's game turn is skipped 

.PROCESS.NPC.KILL	
;DELETE.NPC.RECORD
	LDA #$00
	STA MAP_OBJECTS.NPC+$0,X
	STA MAP_OBJECTS.NPC+$1,X
	STA MAP_OBJECTS.NPC+$2,X
	STA MAP_OBJECTS.NPC+$3,X
	STA MAP_OBJECTS.NPC+$4,X
	STA MAP_OBJECTS.NPC+$5,X
	STA MAP_OBJECTS.NPC+$6,X
	STA MAP_OBJECTS.NPC+$7,X

	LDA #$FF
	STA SCREEN.MO_SPRITE.DATA,Y	;erase mob from screen array
	STA SCREEN.MO_SPRITE_TYPE.DATA,Y
	
;WAS THERE A WITNESS TO THE KILL?
;(a witness is an NPC who is on the view screen and not located on a hidden/dark tile)
	LDY #$00		;set NPC record index
.LOOP.WITNESS.SEARCH
	LDA SCREEN.MO_SPRITE.DATA,Y	;load tile ID of NPC record
	CMP #$FF				;is a sprite found?
	BEQ .INCREMENT.INDEX1	;if no, increment index to get next screen array element
	LDA SCREEN.DARK.DATA,Y	;is the sprite on a tile hidden (dark)?
	BEQ .WITNESS.FOUND		;if no, then the sprite can see the player and is a witness to the act
	;**FALLS THROUGH** 		;if yes, the sprite cannot see the player and is not a witness to the act. Get next screen array element.
.INCREMENT.INDEX1
	INY	;next screen array element
	CPY #SCREEN.ARRAY.LAST_ELEMENT2
	BNE .LOOP.WITNESS.SEARCH	;if index hasn't flipped over to $00 then continue loop
;NO WITNESS FOUND
	JMP .EXIT_REDRAW
		
.WITNESS.FOUND
;SET GUARDS'S TO HOSTILE (because a witness was found)	
	LDX #$00		;set NPC record index
.SET.HOSTILE.LOOP
	LDA MAP_OBJECTS.NPC+$2,X	;load tile ID of NPC record
	CMP #TILE_ID.GUARD.GRE1 	;is NPC a guard?
	BCC .INCREMENT.INDEX		;if no, next record
	CMP #TILE_ID.GUARD.LT1		;is NPC a guard?
	BCS .INCREMENT.INDEX		;if no, next record
	;**FALLS THROUGH**
.GUARD.FOUND
	LDA #$FF			
	STA MAP_OBJECTS.NPC+$6,X	;set transit flag to hostile
			; JSR PREP.BRK
			; LDY MAP_OBJECTS.NPC+$6,X
			; BRK
	;**FALLS THROUGH**
.INCREMENT.INDEX	;next NPC Record
	TXA
	CLC
	ADC #NPC.RECORD.SIZE
	TAX
	BNE .SET.HOSTILE.LOOP	;if index hasn't flipped over to $00 then continue loop
	
.EXIT_REDRAW	
	JMP GAME.LAUNCH		;redraw screen at current gmap position. mob's game turn is skipped 

	
.NO_SPRITE	
	JMP GAME.PRIMARY_LOOP		
	

	
@END
	
KEY4 ;====INCREASE MOB GEN PROBABILITY BY $01=== 
@START
	INC MOB.GEN.PROBABILITY
	BNE .EXIT
	DEC MOB.GEN.PROBABILITY 
.EXIT
	JMP GAME.PRIMARY_LOOP

KEY5 ;====DECREASE MOB GEN PROBABILITY BY $01=== 
@START
	DEC MOB.GEN.PROBABILITY
	LDA MOB.GEN.PROBABILITY
	CMP #$FF
	BNE .EXIT
	INC MOB.GEN.PROBABILITY 
.EXIT
	JMP GAME.PRIMARY_LOOP
@END

@END
	
KEY6 ;=====TOGGLE ANIMATION KEYPRESS ABORT=======
@START
;IF OVERRIDE IS SET, PLAYER CAN TRAVERSE ANY TILE, INCLUDING MAP OBJECTS
	LDA ANIMATION.FORCED.OVERRIDE
	CMP #$01
	BNE .NOT1
	LDA #$00
	STA ANIMATION.FORCED.OVERRIDE
	JMP GAME.PRIMARY_LOOP
.NOT1
	LDA #$01
	STA ANIMATION.FORCED.OVERRIDE
	JMP GAME.PRIMARY_LOOP	
	
@END

KEY_SHIFT6 ;========TOGGLE DARKNESS OVERRIDE======
@START
				; LDA #$AA
				; JSR PREP.BRK
				; BRK
				
	LDA PLAYER.DARKNESS_OVERRIDE
	CMP #$04
	BNE .NOT4
	LDA #$00	;$00 = TURN OFF OVERRIDE
	STA PLAYER.DARKNESS_OVERRIDE
	JMP GAME.LAUNCH		;redraw screen at current gmap position. mob's game turn is skipped 
.NOT4
	LDA #$04	;$04 = OVERRIDE ON  (I think value just needs to be greater than $00, but $04 is what is used for Wyvern flight because it is combined with setting the transport speed to $04)
	STA PLAYER.DARKNESS_OVERRIDE
	JMP GAME.LAUNCH		;redraw screen at current gmap position. mob's game turn is skipped 
@END

KEY7 ;=====TOGGLE DAY/SUNRISE/SUNSET/NIGHT=======
@START
	INC TIME.SUN.STATUS		;$00 = SUN RISING, $01 = DAY, $02 = SUN SETTING, $03 = NIGHT
	LDA TIME.SUN.STATUS	
	CMP #$04
	BCC .NO_FLIP
	LDA #$00
	STA TIME.SUN.STATUS	
.NO_FLIP

	CMP #$00
	BEQ .SUN_RISING
	CMP #$02
	BEQ .SUN_SETTING
	JMP .EXIT
	
.SUN_RISING	
	LDA #$00
	STA TIME.SUN.COUNTER
	STA TIME.SUN.SUB_COUNTER
	JMP .EXIT
	
.SUN_SETTING
	LDA #$04
	STA TIME.SUN.COUNTER
	LDA #$00
	STA TIME.SUN.SUB_COUNTER
	;**FALLS THROUGH**
	
.EXIT
	JMP GAME.PRIMARY_LOOP	
	
@END


KEY8 ;=====PLS TOGGLE=======
@START
;TOGGLE PLAYER LIGHT SOURCE (PLS)
	LDA PLAYER.PLS_STATUS
	CMP #$01
	BNE .NOT1
	LDA #$00
	STA PLAYER.PLS_STATUS
	JMP GAME.PRIMARY_LOOP
.NOT1
	LDA #$01
	STA PLAYER.PLS_STATUS
	JMP GAME.PRIMARY_LOOP

@END

KEY9 ;=====GAME CLOCK: +1 MINUTE======
@START
	SED							;set decimal (BCD) mode

;INCREMENT CLOCK (24hr)
	LDA TIME.CURRENT.MINUTE
	CLC
	ADC #$01
	CMP #$60
	BNE .UPDATE.CLOCK
.RESET.HOUR.COUNTER
	LDA #$00
	;**FALLS THROUGH**
.UPDATE.CLOCK
	STA TIME.CURRENT.MINUTE

	
			
	JSR TIME.UPDATE.DISPLAY		;Convert 24hr clock values into 12hr clock values


.EXIT
	
	CLD							;clear decimal (BCD) mode, return to binary mode

.KEYCHECK
	LDA KB_BUFFER					;LOAD NEXT KEY PRESS FROM BUFFER. WAS KEY PRESSED?
    BMI KEY9						;IF YES, REPEAT THIS COMMAND
	
	
			; LDA #$01
			; STA TROUBLESHOOTING.HOOK
	
	JMP GAME.PRIMARY_LOOP
@END
	
KEY_SHIFT9 ;=====GAME CLOCK: SET======
@START


;GET NEW TIME (hhmm)
	LDA #$24
	STA HTAB	
	LDA #$09
	STA VTAB
	JSR	UPDATE.CHAR.POS
	
			; ;DEC HTAB
			; JSR PREP.BRK
			; LDA HTAB
			; BRK
			
	
		LDA #$24
		STA KEYIN.STRING.LEFT_EDGE ;backspace key not permitted to reduce HTAB below this value. 
		
		LDA #$04			;parameter: # of BCD input digits 
	JSR KEYIN.BCD
	
			; JSR PREP.BRK
			; LDX #RESULT
			; LDY /RESULT
			; BRK
			
;UPDATE 24hr CLOCK
	LDA RESULT
	STA TIME.CURRENT.HOUR
	
	LDA RESULT+$1
	STA TIME.CURRENT.MINUTE
	
	JSR TIME.UPDATE.DISPLAY

;PRINT "   "   (ERASE INPUT FROM SCREEN)	
	LDA #$24
	STA HTAB	
	LDA #$9
	STA VTAB
	JSR	UPDATE.CHAR.POS

	JMP .TEXT1.PRINT

.TEXT1 .AZ -/    /			;ASCII text string
.TEXT1.PRINT
		LDA #.TEXT1 					
		STA STRING
		
		LDA /.TEXT1
		STA STRING+$1						
	JSR PRINT.STR	


	
.UPDATE.GAME_STATE
;(update game state variables that depend on time)
;(these code section will become the basis for a Rest or Wait player 
;command)

;SET SUN STATUS
	JSR DETERMINE.SUNLIGHT.STATUS			;determine if sun is visible based on time of day and location type

		
;REINIT NPC'S (if in BUILDING)
;.CHECK.LOCATION.TYPE
	LDA PLAYER.MAP.LOCATION_TYPE		
	CMP #LOCATION.TYPE.BUILDING
	BNE .EXIT
	
	JSR NPC.INIT							;set GMAP.X/Y and Active Anchor of NPCs, based on current time. 
	JSR SCHEDULE.TABLE.IDENTIFY.NEXT_PATHS	;identify NPCs who will transit to a new anchor soon and add entries to the NPC.PATHGENERATOR.QUE, which are processed by NPC.PATHGENERATOR when called from the primary game loop.
		
;CLEAR ABORT FLAG
	LDA #$00					;$00 = No abort, $01 = aborted path is pending completion
	STA NPC.PATHFINDER.ABORT_FLAG	
	STA ITERATION.COUNTER		;reset as this counter tracks the auto-abort threashold.

			; LDA #$01
			; STA TROUBLESHOOTING.HOOK
			
			; LDA #$00
			; STA GAME_LOOP.ITERATION.COUNTER

			
			
.EXIT	
	JMP GAME.LAUNCH							;ALL NON-MOVEMENT COMMANDS MUST EXIT VIA JMP TO GAME.LAUNCH TO AVOID PROBLEMS WITH KEY PRESS ABORTS. SEE SUBROUTINE DOCUMENTATION FOR GAME.PRIMARY_LOOP FOR DETAILS.
	
	
@END
@END
@END
@END




@END
;======INCLUDE FILES;
@START
;======(SAME TARGET FILE)======
@START
				.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_graphics				
				.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_text
				.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_math
				.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_general
				.IN 	C:\MY_CODE\INCLUDES_LIBS\map_tools
				.IN 	C:\MY_CODE\INCLUDES_LIBS\npc_building.manager				
				.IN 	C:\MY_CODE\INCLUDES_LIBS\zone_functions
				.IN 	C:\MY_CODE\INCLUDES_LIBS\offloaded_variables2				
				.IN 	C:\MY_CODE\INCLUDES_LIBS\offloaded_variables
				.IN 	C:\MY_CODE\INCLUDES_LIBS\ptools.ASM

;recently moved back from BS memory

				
;TEMPORARY FOR TROUBLESHOOTING				
	;.IN 	C:\MY_CODE\INCLUDES_LIBS\zone_functions
	;.IN 	C:\MY_CODE\INCLUDES_LIBS\darkness_manager
	;.IN 	C:\MY_CODE\INCLUDES_LIBS\map_object_management
	;.IN 	C:\MY_CODE\INCLUDES_LIBS\zone_functions

@END
					
						

	.NO $A000,$EA	;FILLER TO THE UPPER LIMIT OF MAIN MEMORY. (RWTS OCCUPIES $B700 - $BFFF)
					;This enables SBASM to generate an error if/when the code overshoots this memory address.
					;It also kees this target file at a fixed size so the track/sector stays the same on the disk image, which is needed to load the file via MY.RWTS

					
;======INCLUDE FILES (SEPERATE TARGET FILE)======
@START

				;.EN  ;end current target file

;BOOT PROCESS 
				.IN 	C:\MY_CODE\INCLUDES_LIBS\noxarch.main.asm
				.IN 	C:\MY_CODE\INCLUDES_LIBS\loader.p.asm

;CONTROLLERS
				.IN 	C:\MY_CODE\INCLUDES_LIBS\controller.hrcg.aux.ASM
				
				
;				.IN 	C:\MY_CODE\INCLUDES_LIBS\Custom_Boot2

;DATA FILES: SHAPES			
				.IN 	C:\MY_CODE\INCLUDES_LIBS\DATA.SHAPES.BUILDING

;DATA FILES: SURFACE
				.IN 	C:\MY_CODE\INCLUDES_LIBS\COMPRESSED_DATA\compressed.DATA.MAP.SURFACE
				.IN 	C:\MY_CODE\INCLUDES_LIBS\DATA.SPR.SURFACE.ASM

;DATA FILES: LOCATION 1
				.IN 	C:\MY_CODE\INCLUDES_LIBS\DATA.TLK.L001.ASM
				;floor1 (map1)
				.IN 	C:\MY_CODE\INCLUDES_LIBS\DATA.MAP.L1.F1.M1.ASM
				.IN 	C:\MY_CODE\INCLUDES_LIBS\DATA.SPR.L1.F1.M1.ASM
				;floor2 (map2)
				.IN 	C:\MY_CODE\INCLUDES_LIBS\DATA.MAP.L1.F2.M2.ASM
				.IN 	C:\MY_CODE\INCLUDES_LIBS\DATA.SPR.L1.F2.M2.ASM

;DATA FILES: UNDERMAP
				.IN 	C:\MY_CODE\INCLUDES_LIBS\COMPRESSED_DATA\COMPRESSED.DATA.MAP.UNDERMAP_LV1.ASM
				.IN 	C:\MY_CODE\INCLUDES_LIBS\DATA.SPR.UNDERMAP_LV1.ASM

;MAIN GAME FILES				

				.IN 	C:\MY_CODE\INCLUDES_LIBS\SWAP.ROUTINES.NPC.TALK.ASM
				.IN 	C:\MY_CODE\INCLUDES_LIBS\lower_main.routines.ASM
				.IN 	C:\MY_CODE\INCLUDES_LIBS\BS_routines.bank1
				.IN 	C:\MY_CODE\INCLUDES_LIBS\BS_routines.bank2

@END
@END
