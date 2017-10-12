; ============================================
; Copyright (C) 2016-2018. 6502 Workshop, LLC
; contact: mark@6502workshop.com
;
; ============================================

;test from PC laptop


                .CR     6502            Use 6502 overlay
				.EF		errors
				;.LI 	OFF				Switches the assembler machine code conversion listing off so errors are easier to see				
				.LF		c:\my_code\na\build_folder\LIST
				.EB		OFF				;turn off error bell
				;				***For include files, look at end of program, before variable definitions. 


;GIT Test1
				
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


COMMAND.QUIT.GO
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

COMMAND.QUIT.PREP
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

; ;COPY PART OF PATHFINDER SAVED.PATH.LOOKUP.TABLE 
		; ;AUX MEMORY -> MAIN MEMORY 	
			; LDA #$00			;SET START ADDRESS
			; STA AUX_MOVE.START
			; LDA #$5C		
			; STA AUX_MOVE.START+$1
			; ;
			; LDA #$FF			;SET END ADDRESS
			; STA AUX_MOVE.END
			; LDA #$5C
			; STA AUX_MOVE.END+$1
			; ;
			; LDA #$00			;SET DESTINATION ADDRESS
			; STA AUX_MOVE.DEST
			; LDA #$20
			; STA AUX_MOVE.DEST+$1
			; CLC					;SET CARRY FLAG DESGINATD MOVE FROM AUX MEMORY -> MAIN
			; JSR AUX_MOVE
; ;			
					; ; ;NOP
; ;
		; ;AUX MEMORY -> MAIN MEMORY 	
			; LDA #$00			;SET START ADDRESS
			; STA AUX_MOVE.START
			; LDA #$5D		
			; STA AUX_MOVE.START+$1
			; ;
			; LDA #$FF			;SET END ADDRESS
			; STA AUX_MOVE.END
			; LDA #$5D
			; STA AUX_MOVE.END+$1
			; ;
			; LDA #$00			;SET DESTINATION ADDRESS
			; STA AUX_MOVE.DEST
			; LDA #$21
			; STA AUX_MOVE.DEST+$1
			; CLC					;SET CARRY FLAG DESGINATD MOVE FROM AUX MEMORY -> MAIN
			; JSR AUX_MOVE
			; ;			
		; ;AUX MEMORY -> MAIN MEMORY 	
			; LDA #$00			;SET START ADDRESS
			; STA AUX_MOVE.START
			; LDA #$5E		
			; STA AUX_MOVE.START+$1
			; ;
			; LDA #$FF			;SET END ADDRESS
			; STA AUX_MOVE.END
			; LDA #$5E
			; STA AUX_MOVE.END+$1
			; ;
			; LDA #$00			;SET DESTINATION ADDRESS
			; STA AUX_MOVE.DEST
			; LDA #$22
			; STA AUX_MOVE.DEST+$1
			; CLC					;SET CARRY FLAG DESGINATD MOVE FROM AUX MEMORY -> MAIN
			; JSR AUX_MOVE
			; ;						
		; ;AUX MEMORY -> MAIN MEMORY 	
			; LDA #$00			;SET START ADDRESS
			; STA AUX_MOVE.START
			; LDA #$5F	
			; STA AUX_MOVE.START+$1
			; ;
			; LDA #$FF			;SET END ADDRESS
			; STA AUX_MOVE.END
			; LDA #$5F
			; STA AUX_MOVE.END+$1
			; ;
			; LDA #$00			;SET DESTINATION ADDRESS
			; STA AUX_MOVE.DEST
			; LDA #$23
			; STA AUX_MOVE.DEST+$1
			; CLC					;SET CARRY FLAG DESGINATD MOVE FROM AUX MEMORY -> MAIN
			; JSR AUX_MOVE
			; ;	


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
				
	
; ;;PC CHARACTER SHEET READ
	; ;read PC character sheet data
		; LDA #$01
		; ;ACC = player sequential # (high-bit not set = read mode)
	; JSR COMBAT.READ_WRITE.CHR_SHEET.PC
		; ;RETURN VALUE = CHR_SHEET.RECORD.READ
		
			; ;TROUBLESHOOTING LOOP
			; LDX #$00
; .TEST.LOOP			
			; LDA CHR_SHEET.RECORD.READ,X
			; STA $BE00,X
			; INX
			; BNE .TEST.LOOP	
	
; ;;MOB CHARACTER SHEET READ
	; ;read PC character sheet data
		; LDA #$02
		; ;ACC = player sequential # (high-bit not set = read mode)
	; JSR COMBAT.READ_WRITE.CHR_SHEET.MOB
		; ;RETURN VALUE = CHR_SHEET.RECORD.READ
		
			; ;TROUBLESHOOTING LOOP
			; LDX #$00
; .TEST.LOOP			
			; LDA CHR_SHEET.RECORD.READ,X
			; STA $BE00,X
			; INX
			; BNE .TEST.LOOP
			
; ;TARGET DATABASE READ
			; ;TROUBLESHOOTING LOOP
			; LDX #$00
; .TEST.LOOP			
			; LDA COMBAT.TARGET_HIT.DB,X
			; STA $BE00,X
			; INX
			; BNE .TEST.LOOP	

			

	
	LDA TEXT

; ;DISABLE.BS_RAM
	LDA $C082				;READ ENABLE ROM, DISABLE WRITE ON BSM (NORMAL STATE).
							; ;REQUIRED FOR BEFORE BRK BECAUSE APPLE MONITOR IS A ROM ROUTINE
	
	JSR CLEAR.TEXT.SCREEN
	JSR HCG.OFF				;disconnect HRCG driver, return COUT vector to normal value	
		
	JSR GENERATE.DEBUG.LOG

	LDA #$AA
	ldx GMAP.X.LAST
	ldy GMAP.Y.LAST
	JMP COMMAND.QUIT.GO
	
@END	

GENERATE.DEBUG.LOG
@START
;USED BEFORE A BRK TO LOG THE LOCATION AND/OR VALUE OF VARIOUS VARIABLES

;DEBUG.LOG = $3000

; ;PATHFINDER

	; ;$1$2, $2/$3 are open
	
	; LDA #PATHFINDER.SPRITE.RECORD		;same format as SPRITE.RECORD, but only use by NPC.PATHFINDER
	; STA DEBUG.LOG+$4
	; LDA /PATHFINDER.SPRITE.RECORD
	; STA DEBUG.LOG+$5	
	
	; LDA #NPC.SCHEDULE.WORKSPACE
	; STA DEBUG.LOG+$6
	; LDA /NPC.SCHEDULE.WORKSPACE
	; STA DEBUG.LOG+$7

	; LDA #NPC.PATHGENERATOR.QUE
	; STA DEBUG.LOG+$8
	; LDA /NPC.PATHGENERATOR.QUE
	; STA DEBUG.LOG+$9

	; LDA #SAVED.PATH.LOOKUP.TABLE
	; STA DEBUG.LOG+$A
	; LDA /SAVED.PATH.LOOKUP.TABLE
	; STA DEBUG.LOG+$B

	; LDA #NPC.ANCHORS.X
	; STA DEBUG.LOG+$C
	; LDA /NPC.ANCHORS.X
	; STA DEBUG.LOG+$D

	; LDA #NPC.ANCHORS.Y
	; STA DEBUG.LOG+$E
	; LDA /NPC.ANCHORS.Y
	; STA DEBUG.LOG+$F

	; LDA TRANSIT.NEXT_MOVE.INDEX
	; STA DEBUG.LOG+$10
	; LDA TRANSIT.NEXT_MOVE.X
	; STA DEBUG.LOG+$11
	; LDA TRANSIT.NEXT_MOVE.Y
	; STA DEBUG.LOG+$12
	
; ;SCREEN ARRAYS
	; LDA #SCREEN.TILE.DATA
	; STA DEBUG.LOG+$20
	; LDA /SCREEN.TILE.DATA
	; STA DEBUG.LOG+$21

	; LDA #SCREEN.DARK.DATA ;$08BB - 0975
	; STA DEBUG.LOG+$22
	; LDA /SCREEN.DARK.DATA
	; STA DEBUG.LOG+$23

	; LDA #SCREEN.DARK.DATA_BEFORE
	; STA DEBUG.LOG+$24
	; LDA /SCREEN.DARK.DATA_BEFORE
	; STA DEBUG.LOG+$25

	; LDA #SCREEN.MO_SPRITE.DATA ;$0A31.$0AEB ($0A8E=player)
	; STA DEBUG.LOG+$26
	; LDA /SCREEN.MO_SPRITE.DATA
	; STA DEBUG.LOG+$27	
	
	; LDA #SCREEN.MO_SPRITE_TYPE.DATA ;$0AEC.$0BA6 ($0B49=player)
	; STA DEBUG.LOG+$28
	; LDA /SCREEN.MO_SPRITE_TYPE.DATA
	; STA DEBUG.LOG+$29	

	; LDA #SCREEN.MO_GENERAL.DATA
	; STA DEBUG.LOG+$2A
	; LDA /SCREEN.MO_GENERAL.DATA
	; STA DEBUG.LOG+$2B
	
;Sprite Movement: choose paths
	;$40 & $41 open 
	
	; LDA #MOB.MOVES.BLOCKED
	; STA DEBUG.LOG+$42
	; LDA /MOB.MOVES.BLOCKED
	; STA DEBUG.LOG+$43	
	
	; LDA #MOB.MOVE.OPTIONS_PRIMARY
	; STA DEBUG.LOG+$44
	; LDA /MOB.MOVE.OPTIONS_PRIMARY
	; STA DEBUG.LOG+$45
	
	; LDA #MOB.MOVE.OPTIONS_SECONDARY
	; STA DEBUG.LOG+$46
	; LDA /MOB.MOVE.OPTIONS_SECONDARY
	; STA DEBUG.LOG+$47	
	
	; LDA #NPC.ASSIGNED_PATHS
	; STA DEBUG.LOG+$48
	; LDA /NPC.ASSIGNED_PATHS
	; STA DEBUG.LOG+$49	

	; LDA #MOB.ADJACENT_TILES
	; STA DEBUG.LOG+$4A
	; LDA /MOB.ADJACENT_TILES
	; STA DEBUG.LOG+$4B	

	; LDA #MOB.MT.ADJACENT_TILES
	; STA DEBUG.LOG+$4C
	; LDA /MOB.MT.ADJACENT_TILES
	; STA DEBUG.LOG+$4D
	
	
	
; ;Sprite Movement: General
	; LDA #SPRITE.RECORD	;& GENERAL_MO.RECORD
	; STA DEBUG.LOG+$50
	; LDA /SPRITE.RECORD	;& GENERAL_MO.RECORD
	; STA DEBUG.LOG+$51
	; LDA #MAP_OBJECTS.TILE_LOCATION
	; STA DEBUG.LOG+$52
	; LDA /MAP_OBJECTS.TILE_LOCATION
	; STA DEBUG.LOG+$53

; ;Combat module
	; LDA COMBAT.PC.ACTIVE
	; STA DEBUG.LOG+$70
	; LDA COMBAT.PC.ACTIVE.RECORD
	; STA DEBUG.LOG+$71
		
	; LDA #COMBAT.TARGET.RECORD
	; STA DEBUG.LOG+$72
	; LDA /COMBAT.TARGET.RECORD
	; STA DEBUG.LOG+$73		


	; LDA COMBAT.CHASE.TARGET.X ;GMAP.X	
	; STA DEBUG.LOG+$74		
	
	; LDA COMBAT.CHASE.TARGET.Y ;GMAP.Y	
	; STA DEBUG.LOG+$75		
	
	; LDA COMBAT.CHASE.TARGET.DISTANCE
	; STA DEBUG.LOG+$76		

	; LDA COMBAT.CHASE.TARGET.HEALTH ;FUTURE
	; STA DEBUG.LOG+$77		

	; ;LDA PARM1.SHAPE.SBYTE ;screen byte # * 7 pixels
	; LDA SHAPE.SBYTE.START		;screen byte #
	; STA DEBUG.LOG+$78	
	
	; LDA SHAPE.LINE.START
	; STA DEBUG.LOG+$79

	; ;LDA PARM2.SHAPE.SBYTE ;screen byte # * 7 pixels
	; LDA SHAPE.SBYTE.TARGET ;screen byte #
	; STA DEBUG.LOG+$7A	
	
	; LDA SHAPE.LINE.TARGET
	; STA DEBUG.LOG+$7B

	; LDA SHAPE.PROJECTILE.SLOPE_RISE ;based on pixel or screen byte denominator (rise/run), whichever is used
	; STA DEBUG.LOG+$7C

	; LDA SHAPE.PROJECTILE.SLOPE_RISE.REMAINDER ;based on pixel or screen byte denominator (rise/run), whichever is used
	; STA DEBUG.LOG+$7D
	
	; LDA SHAPE.PROJECTILE.SLOPE_RISE.SBYTE
	; STA DEBUG.LOG+$7E

	; LDA SHAPE.PROJECTILE.SLOPE_RUN ;based on pixel or screen byte denominator (rise/run), whichever is used
	; STA DEBUG.LOG+$80
	
	; LDA	SHAPE.PROJECTILE.RISE		;(y2 - y1); not simplified.
	; STA DEBUG.LOG+$81
	
	; LDA	SHAPE.PROJECTILE.RUN.PIXEL	;(x2 - x1); not simplified. When calculating SHAPE.PROJECTILE.SLOPE_RISE, this value
									; ;is divided by the size of the horizontal bit/pixel increment.
									; ;***IF PIXEL MODE USED VAR WILL = $0E

	; STA DEBUG.LOG+$82	

	; LDA	SHAPE.PROJECTILE.RUN.SBYTE		;(x2 - x1); not simplified.
	; STA DEBUG.LOG+$83	
	
	; LDA BIT_SHIFT_SKIP.FLAG
	; STA DEBUG.LOG+$84
	
	; LDA #SHAPE.DRAW.HOPPER
	; STA DEBUG.LOG+$85
	
	; LDA /SHAPE.DRAW.HOPPER
	; STA DEBUG.LOG+$86

	
; ;MAP/PLAYER VARIABLES
	; LDA #ZONE.LOOKUP.LO
	; STA DEBUG.LOG+$E0
	; LDA /ZONE.LOOKUP.LO
	; STA DEBUG.LOG+$E1

	; LDA #ZONE.LOOKUP.HO
	; STA DEBUG.LOG+$E2
	; LDA /ZONE.LOOKUP.HO
	; STA DEBUG.LOG+$E3
	
	; LDA #RZONE.ARRAY
	; STA DEBUG.LOG+$E4
	; LDA /RZONE.ARRAY
	; STA DEBUG.LOG+$E5

	; LDA GMAP.X
	; STA DEBUG.LOG+$E6
	; LDA GMAP.Y
	; STA DEBUG.LOG+$E7

	; LDA RMAP.X
	; STA DEBUG.LOG+$E8
	; LDA RMAP.Y
	; STA DEBUG.LOG+$E9

	; LDA RMAP
	; STA DEBUG.LOG+$EA
	; LDA RMAP+$1
	; STA DEBUG.LOG+$EB
	
	; LDA PLAYER.WMAP.ZONE
	; STA DEBUG.LOG+$EC
	
	; LDA PLAYER.TRANSPORT.ACTIVE	
	; STA DEBUG.LOG+$ED
	
; ;GENERAL 	
	; LDA TIME.CURRENT.HOUR				;**OPT** Memory. These debug log commands can be remove. 
	; STA DEBUG.LOG+$F0
	; LDA TIME.CURRENT.MINUTE
	; STA DEBUG.LOG+$F1	

	; LDA TROUBLESHOOTING.HOOK
	; STA DEBUG.LOG+$F2
	; LDA TROUBLESHOOTING.HOOK2
	; STA DEBUG.LOG+$F3	
	; LDA #EVENT.FLAGS
	; STA DEBUG.LOG+$F4
	; LDA /EVENT.FLAGS
	; STA DEBUG.LOG+$F5		
	
	RTS

@END

;QUIT ISN'T RUN BY DEFAULT BECAUSE LOADER.BIN TRANSFERS
;CONTROL VIA A JMP TO GAME.LOADER2


GAME.SETUP ;one time things that don't need to be done ever again, even for a full screen draw
@START

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

;RESET FLOW CONTROL VARIABLES
	; LDA #$00
	; STA SRTN.MODULE.LOADED
				



.INIT.BUILDING
	LDA PLAYER.MAP.LOCATION_TYPE	;load map type of players current location
	; CMP #MAP.TYPE.TOWN_VILLAGE
	; BNE .INIT.BUILDING.COMPLETE ;branch if a building map is not loaded

	;is a building map active
	;**OPT** Memory. Speed. Right now there is only one code for location type, which is used to determine the tile_set to use and for other location-type specific features. There are a lot of the later. It might be a savings if the 
					;there were a tile-set code separate from location type code, and there was a default tile-set picked using the location-type code if tile-set code was set to default. This might require adding another byte to some hex tables
					;but I suspect it would pay off. 
	CMP #MAP.TYPE.BUILDING.GRE 	;is map type = building?
	BCC .INIT.BUILDING.COMPLETE		;if no
	CMP #MAP.TYPE.BUILDING.LT	;is map type = building?
	BCS .INIT.BUILDING.COMPLETE		;if no
	;**FALLS THROUGH**				;if yes
	
		;**BUILDING ONLY. Setup a special section if a lot of building only stuff gets added to this code section
		
		
		; ;LDA #$01 ;parameter: don't init shape buffers, since a shape table was just loaded
	; JSR TILE_SWAP.INIT.SUNRISE_SUNSET ;facilitates the swapping of daytile and nighttime tiles.

.INIT.BUILDING.COMPLETE


			
.INIT.SCREEN			
	;initial screen	draw	
	JSR INIT.SCREEN.ARRAYS	
			
	JSR DRAW.SCREEN	

			
	JSR INIT.TW_BOTTOM

	JSR DRAW.TILE.PLAYER
			
	JSR FLIP.PAGE

			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP1
			; JSR KEYIN
; .TEMP1
			; LDA TEMP

			
	;!!!!! BSR BANK1 !!!!
	JSR COPY.SCREEN.ENTRANCE ;**DON'T REMOVE** (or the screen goes chaotic sometimes when collisions occur with double mover mobs. MO.DRAW must need the graphics pages in sync for some reason). UDPATE: I think this is needed because sometimes, due to key press abort, no copy.screen was done after the last move, so the pages are out of syncing going into MOVE.PASS, which is triggered by non-movement commands like (3)ZAP, Push, Board, etc.

	
			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP2
			; JSR KEYIN
; .TEMP2
			; LDA TEMP

			
;FALLS DIRECTLY INTO GAME.LOOP
@END



			

			
			
GAME.PRIMARY_LOOP ;=======MANAGES GAME STATE, PROCESSES PLAYER KEYPRESS====== 
@START
;=====================SUBROUTINE DOCUMENTATION====================================
;
;This loop continously scans for a keypress and until a keypress is 
;deleted it continously executed several automatic subroutines which 
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

;======CONTINOUS LOOP UNTIL KEY PRESS====

	;INC RANDOM.NUMBER.SEED.COUNTER

	;!!!!! BSR:BANK1 !!!!
	JSR ANIMATION.UPDATE.ENTRANCE
	
GAME.PRIMARY_LOOP.ALTERNATE.ENTRANCE		;SKIPS ALL INBETWEEN MOVE ACTIVITES SUCH AS ANIMATION AND MOB GENERATION
			
;DID MOB INITIATE COMBAT?	
	LDA COMBAT_SE.MODE.PARM	;($00 = player initiated or init value in map objects manager | $01 = mob initiated | $FF = test mode)
	BEQ .MOB.INITIATED.COMBAT.CHECK.DONE ;if yes, branch
	JSR MOB.INITIATED.COMBAT.ENTRANCE
	JMP GAME.LAUNCH
.MOB.INITIATED.COMBAT.CHECK.DONE

	JSR TIME.DISPLAY
	JSR EVENT.MANAGER			

			
;IS PLAYER IN A BUILDING? IF NOT, EXIT
	LDA PLAYER.MAP.LOCATION_TYPE		
	; CMP #MAP.TYPE.TOWN_VILLAGE
	; BNE .NOT.IN.BUILDING

	;is a building map active
	;**OPT** Memory. Speed. Right now there is only one code for location type, which is used to determine the tile_set to use and for other location-type specific features. There are a lot of the later. It might be a savings if the 
					;there were a tile-set code separate from location type code, and there was a default tile-set picked using the location-type code if tile-set code was set to default. This might require adding another byte to some hex tables
					;but I suspect it would pay off. 
	CMP #MAP.TYPE.BUILDING.GRE 	;is map type = building?
	BCC .NOT.IN.BUILDING		;if no
	CMP #MAP.TYPE.BUILDING.LT	;is map type = building?
	BCS .NOT.IN.BUILDING		;if no
	;**FALLS THROUGH**			;if yes
	
	JSR NPC.PATHGENERATOR					;calculate the path to the next anchor of all Building NPCs with a schedule change in the next hour
	JMP .BUILDING.CHECK.COMPLETE
.NOT.IN.BUILDING	
	;JSR MOB.GENERATION
.BUILDING.CHECK.COMPLETE


		
;======CONDITIONAL LOGIC====	
@START
@MIDDLE
	
	;**OPT** Memory. Speed. Convert ACC value to upper or lowercase, then only do checks for one case
	
	LDA $C000
    BPL GAME.PRIMARY_LOOP 
    ; BMI .COW 
	; JMP GAME.PRIMARY_LOOP 
; .COW	
    STA $C010               ;CLR LAST KEY

	; PHA
	; LDA RANDOM.NUMBER.SEED.COUNTER
	; STA $4E
	; CLC
	; ADC #$63
	; STA $4F
	; PLA
	
	;**OPT** Memory. Convert ACC to UCASE and elimate the duplicate branches
	
	CMP #$8B			;UP ARROW
	BEQ .NORTH_STEP
	CMP #$8A			;DOWN ARROW
	BEQ .SOUTH_STEP
	CMP #$95			;RIGHT ARROW
	BEQ .EAST_STEP
	CMP #$88			;LEFT ARROW
	BEQ	.WEST_STEP
	CMP	#$C1			;(A) Attack
	BEQ .COMMAND.ATTACK_STEP
	CMP	#$E1			;(a) Attack
	BEQ .COMMAND.ATTACK_STEP
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
	BEQ	.COMMAND.QUIT_STEP
	CMP #$F1			;(q) QUIT
	BEQ	.COMMAND.QUIT_STEP
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
	CMP #$89			;TAB (display character roster)
	BEQ	.COMMAND.DISPLAY_CHARACTER_ROSTER_STEP
	JMP .CHECK.PLAYTEST_KEYS
	
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

.COMMAND.DISPLAY_CHARACTER_ROSTER_STEP
	JMP COMMAND.DISPLAY_CHARACTER_ROSTER
	
.COMMAND.ATTACK_STEP
	JMP COMMAND.ATTACK.MAIN_GAME_LOOP.ENTRANCE
	
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
	
.COMMAND.QUIT_STEP
	JMP COMMAND.QUIT.PREP
	
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
	BEQ	.COMMAND.ATTACK_STEP	
	CMP #$A3			;SHIFT+3 (TOGGLE LAUNCH COMBAT MODE)
	BEQ .KEY_SHIFT3_STEP
	CMP #$B4			;4	(INCREASE MOB GEN PROB BY !1/255)
	BEQ	.KEY4_STEP	
	CMP #$A4			;SHIFT+4 (LAUNCH MERCHANT TRANSACTIONS TEST MODE)
	BEQ .KEY_SHIFT4_STEP	
	CMP #$B5			;5	(DECREASE MOB GEN PROB BY !1/255)
	BEQ	.KEY5_STEP
	CMP #$B6			;6	(TOGGLE ANIMATION KEYPRESS ABORT)
	BEQ	.KEY6_STEP	
	CMP #$DE			;SHIFT+6 (TOGGLE DARKNESS OVERRIDE)
	BEQ .KEY_SHIFT6_STEP
	CMP #$B7			;7	(TOGGLE TOD: DAYTIME/SUNRISE/SUNSET/NIGHT)	
	BEQ	.KEY7_STEP	
		
	CMP #$B8			;8	(TOGGLE PLS)
	BEQ	.KEY8_STEP	
	CMP #$B9			;9	(GAME CLOCK: +1 MINUTE)
	BEQ	.KEY9_STEP		
	CMP #$A8			;SHIFT+9 (GAME CLOCK: SET)
	BEQ	.KEY_SHIFT9_STEP
	;JMP .INVALID.KEY.PRESS

	;**FALLS THROUGH**
	
.INVALID.KEY.PRESS
	;update bottom text window
		LDA #GLOBAL.TEXT_BLOCK.COMMAND_INVALID			
		STA STRING+$0		
		LDA /GLOBAL.TEXT_BLOCK.COMMAND_INVALID
		STA STRING+$1
	JSR PRINT.TW_BOTTOM
	
	JSR PLAY.SOUND.DUMB_ASS
	
	;set init code so that PASS doesn't trigger command text print
	LDA #$05
	STA TW.BOTTOM_WINDOW.INIT_CODE	;($00 = none | $01 = Ignite Torch | $02 = Open door | $03 = Unlock Door | $04 = Operate lever | $05 = Invalid Command)
	
	JSR PASS
	JMP GAME.PRIMARY_LOOP

	
.KEY0_STEP
	JMP KEY0
	
.KEY_SHIFT0_STEP
	JMP KEY_SHIFT0
	
.KEY1_STEP
	JMP KEY1
	
.KEY2_STEP
	JMP KEY2
	
; .KEY3_STEP
	; JMP KEY3

.KEY_SHIFT3_STEP
	JMP PLAYTEST.KEY_SHIFT3
	
.KEY4_STEP
	JMP KEY4
	
.KEY_SHIFT4_STEP
	JMP PLAYTEST.KEY_SHIFT4

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

	LDA TW.BOTTOM_WINDOW.INIT_CODE	;($00 = none | $01 = Ignite Torch | $02 = Open door | $03 = Unlock Door | $04 = Operate lever | $05 = Invalid Command)
	CMP #$05
	BEQ .UPDATE.BOTTOM.TEXT_WINDOW.DONE
	
	;.update.bottom.text_window
		LDA #GLOBAL.TEXT_BLOCK.COMMAND_PASS					
		STA STRING+$0		
		LDA /GLOBAL.TEXT_BLOCK.COMMAND_PASS
		STA STRING+$1
	JSR PRINT.TW_BOTTOM
.UPDATE.BOTTOM.TEXT_WINDOW.DONE
	
.EXIT2
	LDA #$00 ;reset init code
	STA TW.BOTTOM_WINDOW.INIT_CODE	;($00 = none | $01 = Ignite Torch | $02 = Open door | $03 = Unlock Door | $04 = Operate lever | $05 = Invalid Command)

		
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

.INIT
	;.update.bottom.text_window
		LDA #GLOBAL.TEXT_BLOCK.COMMAND_NORTH					
		STA STRING+$0		
		LDA /GLOBAL.TEXT_BLOCK.COMMAND_NORTH
		STA STRING+$1
	JSR PRINT.TW_BOTTOM
	
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


			
.INIT
	;.update.bottom.text_window
		LDA #GLOBAL.TEXT_BLOCK.COMMAND_SOUTH					
		STA STRING+$0		
		LDA /GLOBAL.TEXT_BLOCK.COMMAND_SOUTH
		STA STRING+$1
	JSR PRINT.TW_BOTTOM
	;---
	
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

				;JSR PLAY.SOUND.SLOW_PROGRESS ;TEST
				;JSR PLAY.SOUND.DUMB_ASS ;TEST

				


			
	JMP GAME.PRIMARY_LOOP 

.EXIT.ALTERNATE
				;JSR PLAY.SOUND.SLOW_PROGRESS ;TEST
				;JSR PLAY.SOUND.DUMB_ASS ;TEST
				
; SOUND.TEST				

	; LDA #$21				;SET FREQUENCY
	; STA HALFTIME			
	; ; LDA	#$1D				;SET DURATION
	; ; STA	LENGTH				
	; LDA	#$01				;SET DURATION
	; STA	LENGTH	
	
	
; .SAVE		;SAVE REGISTERES
	; ;PHA
	; TXA
	; PHA
	; TYA
	; PHA

		
; .NOTE
	; LDY	#$100				;!255

; .NOTE1
	; LDX	HALFTIME			;X contains the length of the note
	; LDA	SPEAKER				;Toggle the speaker
			
	; JMP .STALL1
; .STALL
	; NOP						;These NOPs compenstate for
	; NOP						;banches to NOTE1 from line 37
	; NOP						;They ensure that the overall loop
	; NOP						;times are the same so that they units
	; NOP						;of "length" don't vary with the frequency
	; NOP	
	; NOP
; .STALL1
	; DEY						;Loop time is 34 cycles
	; BNE	.STALL2				
	; DEC	LENGTH				;Reduce this evry 34*255 cycles
	; BEQ	.EXIT
	; BNE	.STALL3
; .STALL2
	; NOP						;These NOPs compensate even
	; NOP						;out the loop time when the code
	; NOP						;in lines 27-29 is not executed
	; NOP		
	; NOP
; .STALL3
	; DEX						;Loop time is 34 cycles
	; BNE	.STALL
	; BEQ	.NOTE1
; .EXIT

; @END

; .RESTORE	;RESTORE REGISTERES
	; PLA
	; TAY
	; PLA
	; TAX





			
	JMP GAME.PRIMARY_LOOP.ALTERNATE.ENTRANCE

@END
	
EAST
@START
;=====================SUBROUTINE DOCUMENTATION====================================
;
;See documentation above in subtroutine NORTH
;
;=================================================================================


			
.INIT
	;.update.bottom.text_window
		LDA #GLOBAL.TEXT_BLOCK.COMMAND_EAST					
		STA STRING+$0		
		LDA /GLOBAL.TEXT_BLOCK.COMMAND_EAST
		STA STRING+$1
	JSR PRINT.TW_BOTTOM
	;-----
	
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


.INIT
	;.update.bottom.text_window
		LDA #GLOBAL.TEXT_BLOCK.COMMAND_WEST			
		STA STRING+$0		
		LDA /GLOBAL.TEXT_BLOCK.COMMAND_WEST
		STA STRING+$1
	JSR PRINT.TW_BOTTOM
	;-----
	
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


COMMAND.ATTACK.MAIN_GAME_LOOP.ENTRANCE ;=====PRESS 3 TO ZAP S_ENTITY========
@START
;PARAMETERS: ACC = keypress captured in main game loop
;ENTRANCE: main game loop, do not use from combat

;SAVE PARAMETERES
	STA COMMAND.ATTACK.KEYPRESS.PARM


			
	; LDY #PLAYER.ADJACENT.NORTH	

		; ;YREG = screen location of S_ENTITY to delete (the one in the tile the mob is landing on)
	; JSR DELETE.S_ENTITY	
	; JMP GAME.LAUNCH		;redraw screen at current gmap position. mob's game turn is skipped 
	
	
		;set parameter: player adjacent screen locations
		LDA #PLAYER.ADJACENT.NORTH	
		STA PLAYER.ADJACENT.SCREEN_LOCATIONS+$0
		
			; ldx #$aa
			; jsr prep.brk
			; brk	
			
		LDA #PLAYER.ADJACENT.SOUTH	
		STA PLAYER.ADJACENT.SCREEN_LOCATIONS+$1

		LDA #PLAYER.ADJACENT.EAST
		STA PLAYER.ADJACENT.SCREEN_LOCATIONS+$2

		LDA #PLAYER.ADJACENT.WEST
		STA PLAYER.ADJACENT.SCREEN_LOCATIONS+$3
	JSR COMMAND.ATTACK
		;ACC = return value
		CMP #$00 ;is screen draw required? (S_ENTITY found/killed)
	BEQ .SCREEN.DRAW
	;assume return value is $01 (NPC not found, return to game state loop)		
	JMP GAME.PRIMARY_LOOP
	
.SCREEN.DRAW	
	JMP GAME.LAUNCH		;redraw screen at current gmap position. mob's game turn is skipped 
@END


COMMAND.ATTACK ;=====PRESS 3 TO ZAP SPRITE========
@START
;PARAMETERS: COMMAND.ATTACK.KEYPRESS.PARM, PLAYER.ADJACENT.SCREEN_LOCATIONS, COMMAND.ATTACK.KEYPRESS.PARM
;RETURN: Y-REG (screen array index of S_ENTITY killed), ACC ($00 = sprite found/killed, screen draw required | $01 = sprite note found, return to game state loop)



	
			;**OPT** Memory. Speed. I think this can be removed if I remove the key press abort skipping of copy.screen at the end of the common move routine, which I will probably have to do to get animation for just mobs working even in a key press abort scenario. 
	;!!!!! BSR BANK1 !!!!
	JSR COPY.SCREEN.ENTRANCE ;**DON'T REMOVE** (or the screen goes chaotic sometimes when collisions occur with double mover mobs. MO.DRAW must need the graphics pages in sync for some reason). UDPATE: I think this is needed because sometimes, due to key press abort, no copy.screen was done after the last move, so the pages are out of syncing going into MOVE.PASS, which is triggered by non-movement commands like (3)ZAP, Push, Board, etc.



			
	JSR KEYIN.ANIMATION.SINGLE
	CMP #$8B			;UP ARROW
	BEQ .ATTACK_NORTH
	CMP #$8A			;DOWN ARROW
	BEQ .ATTACK_SOUTH
	CMP #$95			;RIGHT ARROW
	BEQ .ATTACK_EAST
	CMP #$88			;LEFT ARROW
	BEQ	.ATTACK_WEST
	CMP #$9B			;ESC
	BEQ .EXIT_STEP
	;default case: play dumb ass sound then exit
			

			
	JMP .INVALID.KEYPRESS
	
.ATTACK_NORTH
	;**OPT** Memory. The only reason that the N/S/E/W code sections can't just load the constant for the tile adjacent to the player is that this
	;subroutine is setup to support a call from the combat module, at which time the adjacent tiles could be anywhere (they are tied to the screen location of the active PC)
	LDY PLAYER.ADJACENT.SCREEN_LOCATIONS+$0
	JMP .SEARCH.ADJACENT_SPRITE
	
.ATTACK_SOUTH	
	LDY PLAYER.ADJACENT.SCREEN_LOCATIONS+$1
	JMP .SEARCH.ADJACENT_SPRITE
	
.ATTACK_EAST
	LDY PLAYER.ADJACENT.SCREEN_LOCATIONS+$2
	JMP .SEARCH.ADJACENT_SPRITE
 
.ATTACK_WEST
	LDY PLAYER.ADJACENT.SCREEN_LOCATIONS+$3
	
;****FALLS THROUGH


.SEARCH.ADJACENT_SPRITE		
;IS THERE A SPRITE IN DIRECTION OF ATTACK?	
	LDA SCREEN.MO_SPRITE.DATA,Y					;is a mob located at target tile?


			
	CMP #$FF
	BEQ .NO_SPRITE
	; BNE .cow
	; jmp .NO_SPRITE
; .cow
	TAX
	
	;**FALLS THROUGH**
			
	LDA COMMAND.ATTACK.KEYPRESS.PARM ;contains keypress from main game loop which triggered this routine
	CMP #$B3	;ASCII = 3
	BEQ .S_ENTITY.KILLED

				

.INITIATE.COMBAT.SESSION
@START
	;save map object info for attacking mob
	TYA 
	PHA ;push screen index of mob player is attacking to stack

	LDA SCREEN.MO_SPRITE.DATA,Y					;is a mob located at target tile?
	STA COMBAT_SE.S_ENTITY.MO_INDEX ;S_ENTITY map object index of attacking mob

	LDA SCREEN.MO_SPRITE_TYPE.DATA,Y
	STA COMBAT_SE.S_ENTITY.TYPE  ;S_ENTITY type of attacking mob

	;get TILE_ID of S_ENTITY player is attacking

		;read $8 bytes
		LDA #$08 ;(high bit not set = lookup by screen index)
		;Y-REG (screen array index | map object record index)
	JSR READ.MAP_OBJECT.ARRAY
		;RETURN VALUE = MAP_OBJECT.RECORD.READ(8)
	LDA MAP_OBJECT.RECORD.READ+$2	;load TILE_ID of S_ENTITY
	STA COMBAT_SE.MOB_TILE_ID.PARM	;The Tile_ID of the MOB on the main map which attacked the player or the player attacked
		
;LAUNCH COMBAT SESSION
		
		;set combat mode = mob initiated
		LDA #$01
		STA COMBAT_SE.MODE.PARM	;($00 = player initiated or init value in map objects manager  | $01 = mob initiated | $FF = test mode)
		;COMBAT_SE.MOB_TILE_ID.PARM (already set above)		;The Tile_ID of the MOB on the main map which attacked the player or the player attacked
	JSR INITIATE.COMBAT
			
	;seed screen array data for attacking mob
	;(necessary because DELETE.S_ENTITY used the screen arrays to identify the map object to delete
	;and the screen arrays currently still contain data from the combat session. They won't be refreshed until
	;GAME.LAUNCH is called, after this subroutine completes)	
	PLA ;pull attacking MOB screen index to stack
	TAY ;MAP_OBJECTS.TILE_LOCATION
	LDA COMBAT_SE.S_ENTITY.TYPE  ;S_ENTITY type of attacking mob
	STA SCREEN.MO_SPRITE_TYPE.DATA,Y
	LDA COMBAT_SE.S_ENTITY.MO_INDEX ;S_ENTITY map object index of attacking mob
	STA SCREEN.MO_SPRITE.DATA,Y


@END
	
	
.S_ENTITY.KILLED	
		;YREG = screen location of S_ENTITY to delete (the one in the tile the mob is landing on)
	JSR DELETE.S_ENTITY	
	

;WAS THERE A WITNESS TO THE KILL?
;(a witness is an NPC who is on the view screen and not located on a hidden/dark tile)

	;is building map active? (if no, the witnesses don't apply)
	LDA PLAYER.MAP.LOCATION_TYPE	;load map type of players current location
	; CMP #MAP.TYPE.TOWN_VILLAGE
	; BEQ .WITNESS.SEARCH
	; CMP #MAP.TYPE.CASTLE
	; BEQ .WITNESS.SEARCH
	; JMP .EXIT_REDRAW
	
	;is a building map active
	;**OPT** Memory. Speed. Right now there is only one code for location type, which is used to determine the tile_set to use and for other location-type specific features. There are a lot of the later. It might be a savings if the 
					;there were a tile-set code separate from location type code, and there was a default tile-set picked using the location-type code if tile-set code was set to default. This might require adding another byte to some hex tables
					;but I suspect it would pay off. 
	CMP #MAP.TYPE.BUILDING.GRE 	;is map type = building?
	BCC .EXIT_REDRAW			;if no
	CMP #MAP.TYPE.BUILDING.LT	;is map type = building?
	BCS .EXIT_REDRAW			;if no
	JMP .WITNESS.SEARCH			;if yes
	
.EXIT_STEP
	JMP .EXIT
	
.WITNESS.SEARCH			
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

	;**FALLS THROUGH**
.INCREMENT.INDEX	;next NPC Record
	TXA
	CLC
	ADC #NPC.RECORD.SIZE
	TAX
	BNE .SET.HOSTILE.LOOP	;if index hasn't flipped over to $00 then continue loop
	;**FALLS THROUGH**
	
.EXIT_REDRAW	
	;load return value
	LDA #$00 ;S_ENTITY found/killed, screen draw required
	JMP .EXIT
	
.INVALID.KEYPRESS
.NO_SPRITE	
	JSR PLAY.SOUND.DUMB_ASS

	;load return value
	LDA #$00 ;S_ENTITY not found/killed, screen draw required
	;**FALLS THROUGH**
	
.EXIT			
	RTS

@END


COMMAND.CAST_SPELL ; (i.e. calls routines from the spell file)
@START
;=====================SUBROUTINE DOCUMENTATION====================================
;
;---FLOW CONTROL OVERVIEW----
;
;COMMAND.CAST_SPELL (main game engine)
;	*load SWAP.ROUTINES.Cast_spell.setup_exit.ASM from disk
;	*swap main combat module (non-persistent memory) in/out of aux memory
;	*restore weapon shape tables, which are clobbered by the above aux swap
;	*pass control to CAST_SPELL.ENTRANCE
;	*exists in main game engine memory to provide entance for non-combat spell casting
;
;CAST_SPELL.ENTRANCE (cast setup file, clobbers main combat module, except persistent memory)
;	*load spell file entrance routine. 
;	*pass control to spell file entrance routine
;	*exists in memory that clobbers non-persistent main combat module memory to reduce memory used in spell file and main combat module
; 
;<SPELL ENTRANCE ROUTINES> (spell file)
;	*select target (PCs)
;	*misc other functions
;	*load code block for spell cast
;	*pass control to spell code block
;
;<SPELL CODE BLOCK> (spell file)
;	*graphical effects for spell
;	*play spell sounds (future)
;	*record targets hit in database
;	*pass control to SPELL_FILE.AFTER_CAST
;		via RTS address seeded before the JMP from CAST_SPELL.ENTRANCE to the spell entrance routine 
;
;SPELL_FILE.AFTER_CAST (spell file)
;	*return control to COMMAND.CAST_SPELL
;
;COMMAND.CAST_SPELL
;	*finish cleaup, such as aux memory swap. The aux memory swap out
;		must be done in main game engine memory or otherwise the code doing the swap would get clobbered.  
;
;=================================================================================


			
			
	;copy main combat module to aux swap space2
	JSR SWAP2.MAIN_MEMORY.OUT
		
		;jmp .skip ;*****REMOVE THIS*** (debug)
		
.LOAD.CAST_SPELL ;setup/exit routine
@START

;filename = "SRTN.CAST_SPELL"
	
;------OPEN FILE------
.OPEN.FILE
	
	;set command type (READ | WRITE | SEEK)
; cmd_seek.current_drive 	.EQ $90
; cmd_read.current_drive  	.EQ $91
; cmd_write.current_drive 	.EQ $92
;			
; cmd_read.drive1			.EQ $1
; cmd_read.drive2		  	.EQ $81
; cmd_write.drive1			.EQ $2
; cmd_write.drive2			.EQ $82
	LDA #cmd_read.drive2
	sta parm.reqcmd
	
	
;set read length (bytes)
	LDA #$FF
	sta parm.sizelo	
	LDA #$FF
	sta parm.sizehi
	
;set destination memory address
	lda #SRTN.CAST_SPELL.FILE_ADDRESS
	sta parm.ldrlo
	lda /SRTN.CAST_SPELL.FILE_ADDRESS
	sta parm.ldrhi

;set filename to read from	
	lda #SRTN.CAST_SPELL.SETUP	;load LO address
	sta parm.namlo
	lda /SRTN.CAST_SPELL.SETUP	;load HO address
	sta parm.namhi
		LDA #$00 ;;PARM: $00=main, $01=aux
	JSR PRODOS.IO
@END

;.skip
	

			
	JSR CAST_SPELL.ENTRANCE



			
	;restore main combat module to main memory swap space
	JSR SWAP2.MAIN_MEMORY.IN


		
			
	;***this routine must be executed after SWAP2.MAIN_MEMORY.IN, not before, otherwise it will clobber the main COMBAT module code while it is swapped out to aux_bsr memory. 
	JSR LOAD.WEAPONS.SHAPE_TABLES

			
			
	RTS ;return to COMBAT.CAST.EXECUTE (both MOB and PC spell casts originate from that routine)
	
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
	;!!!!! BSR BANK1 !!!!
	JSR COPY.SCREEN.ENTRANCE ;**DON'T REMOVE** (or the screen goes chaotic sometimes when collisions occur with double mover mobs. MO.DRAW must need the graphics pages in sync for some reason). UDPATE: I think this is needed because sometimes, due to key press abort, no copy.screen was done after the last move, so the pages are out of syncing going into MOVE.PASS, which is triggered by non-movement commands like (3)ZAP, Push, Board, etc.


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
	
	;set init code to print command text
	LDA #$02
	STA TW.BOTTOM_WINDOW.INIT_CODE	;($00 = none | $01 = Ignite Torch | $02 = Open door | $03 = Unlock Door | $04 = Operate lever | $05 = Invalid Command)
	JMP GAME.LAUNCH
	
.OPEN.CLOSED.LOCKED.DOOR
	;SET DOOR TO UNLOCKED DOOR IN MAP OBJECT RECORD
	LDA #$10
	STA MAP_OBJECTS.GENERAL+$3,X

	;SWITCH DOOR TILE TO UNLOCKED
	DEC MAP_OBJECTS.GENERAL+$2,X
	
	;set init code to print command text
	LDA #$03
	STA TW.BOTTOM_WINDOW.INIT_CODE	;($00 = none | $01 = Ignite Torch | $02 = Open door | $03 = Unlock Door | $04 = Operate lever | $05 = Invalid Command)
	JMP .EXIT
	
.SWITCH.PORTCULLIS.LEVER.RIGHT
	;SWITCH LEVER
	INC MAP_OBJECTS.GENERAL+$2,X ;add 1 to tile_ID of object
	INC MAP_OBJECTS.GENERAL+$3,X ;ADD 1 to data byte of object

		; LDA MAP_OBJECTS.GENERAL+$2,X
		; TAY
		; LDA #$AA
		; JSR PREP.BRK
		; BRK
		
	
	;CHANGE PORTCULIS TO DOWN/LOWERED
	TXA
	SEC
	SBC #MAP_OBJECTS.RECORD_LENGTH	;move index to one record before the lever, which should be the record for the portcullis
	TAX
	LDA #MO.PORTCULLIS.LOWERED
	STA MAP_OBJECTS.GENERAL+$3,X	;save flag status map object record of portcullis
	
	JMP .SWITCH.COMMON
	
.SWITCH.PORTCULLIS.LEVER.LEFT
	;SWITCH LEVER
	DEC MAP_OBJECTS.GENERAL+$2,X ;subtract 1 from tile_ID of object
	DEC MAP_OBJECTS.GENERAL+$3,X ;subtract 1 from data byte of object

	;CHANGE PORTCULIS TILE TO UP/RAISED (VIA SKIP DRAW FLAG)
	TXA
	SEC
	SBC #MAP_OBJECTS.RECORD_LENGTH	;move index to one record before the lever, which should be the record for the portcullis
	TAX
	LDA #MO.SKIP.DRAW.FLAG			;load skip draw flag to simulate raised/up portcullis 
	STA MAP_OBJECTS.GENERAL+$3,X	;save tile_id to the map object record of portcullis
	
	;**FALLS THROUGH**

.SWITCH.COMMON
	;set init code to print command text
	LDA #$04
	STA TW.BOTTOM_WINDOW.INIT_CODE	;($00 = none | $01 = Ignite Torch | $02 = Open door | $03 = Unlock Door | $04 = Operate lever | $05 = Invalid Command)

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
	;!!!!! BSR BANK1 !!!!
	JSR COPY.SCREEN.ENTRANCE ;**DON'T REMOVE** (or the screen goes chaotic sometimes when collisions occur with double mover mobs. MO.DRAW must need the graphics pages in sync for some reason). UDPATE: I think this is needed because sometimes, due to key press abort, no copy.screen was done after the last move, so the pages are out of syncing going into MOVE.PASS, which is triggered by non-movement commands like (3)ZAP, Push, Board, etc.


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
	;!!!!! BSR BANK1 !!!!
	JSR COPY.SCREEN.ENTRANCE ;**DON'T REMOVE** (or the screen goes chaotic sometimes when collisions occur with double mover mobs. MO.DRAW must need the graphics pages in sync for some reason). UDPATE: I think this is needed because sometimes, due to key press abort, no copy.screen was done after the last move, so the pages are out of syncing going into MOVE.PASS, which is triggered by non-movement commands like (3)ZAP, Push, Board, etc.


		
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
;VALIDATE ENTRY	
	LDX SCREEN.MO_SPRITE.DATA,Y	;load sprite map object data for current tile location
	CPX #$FF						;is a sprite map object present?
	BEQ .INVALID.COMMAND			;if no, then nothing nobody to talk to. exit via invalid command
	
	LDA SCREEN.MO_SPRITE_TYPE.DATA,Y	;load sprite type
	AND #$87 ;mask-out MTT flags (bits 3-6)
	CMP #S_ENTITY_TYPE.BLD_NPC
	BCC .INVALID.COMMAND			;if not an NPC, then nobody to talk to. exit via invalid command
	;STX NPC.TALK.RECORD				;if an NPC, save the MO record index and proceed to process the TALK command

	;**FALLS THROUGH**

.CHECK.FOR_MERCHANT
;is the NPC a merchant?
	LDA MAP_OBJECTS.NPC+$2,X	;load sprite tile_ID
	CMP #TILE_ID.MERCHANT.EQ	
	BNE .LOAD.NPC.TALK ;branch, if no
		;X-REG: NPC RECORD # (already set)
	JMP COMMMAND.MERCHANT.TALK



.LOAD.NPC.TALK
	
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

		;-XREG contains map object record # of the Active NPC (/aka the NPC #)
		;-YREG contains Active NPC screen tile location when NPC.TALK is called
	

				
		;X-REG: NPC RECORD # (already set)
	JSR NPC.TALK
		

.EXIT
	
	JSR SWAP.MAIN_MEMORY.IN
		
	
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


COMMAND.DISPLAY_CHARACTER_ROSTER
@START
		LDA #$03
	;JSR CLEAR.TEXT_WINDOW.RIGHT
	JSR	CLEAR.TW.RIGHT.DISPLAY_CHARACTER_ROSTER
				;JSR KEYIN
	; JSR DRAW.TEXT_WINDOW.RIGHT
				; JSR KEYIN
				
	JSR DISPLAY.CHARACTER.ROSTER


.GET.KEYPRESS ;all keypresses abort, except TAB which launches the inventory module, stats sub_module: screen0
	JSR KEYIN
	CMP #$89			;TAB
	BNE	.EXIT_ALT
	
	
				;JMP PLAYTEST.KEY_SHIFT4
				

			
;LOAD INVENTORY FILE INTO THE MAIN MEMORY SWAP SPACE
	JSR SWAP.MAIN_MEMORY.OUT ;swap out anything currently occupying the swap space
	
						; lda #$aa
						; jsr prep.brk
						; brk
						
			LDA #$00 ;(PARM: ACC = inventory sub-module # to launch)
			STA INV.DEBUG.LOG.INDEX ;**OPT** Memory. Remove.   !!!!!TEMP!!!!!
	JSR LAUNCH.INVENTORY.MODULE
	;JSR INV_0.DISPLAY.STATS_SUMMARY

.EXIT
	JSR SWAP.MAIN_MEMORY.IN ;swap in whatever was in the swap space before the COMBAT file was loaded into it


.EXIT_ALT	
		LDA #$01
		STA TW.RIGHT_WINDOW.CLEAN_UP.FLAG
	JMP GAME.LAUNCH
@END

;==========PLAY TEST KEYS=
@START
KEY0 ;=====SET TROUBLESHOOTING HOOK=======
@START

	JSR PLAYTEST.KEY0
	JMP GAME.PRIMARY_LOOP
@END

KEY_SHIFT0 ;=====SET TROUBLESHOOTING HOOK2=======
@START

	JSR PLAYTEST.KEY_SHIFT0
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



PLAYTEST.KEY_SHIFT3 ;====LAUNCH COMBAT MODE===
@START

	;set combat mode = test
	LDA #$FF
	STA COMBAT_SE.MODE.PARM	;($00 = player initiated | $01 = mob initiated | $FF = test mode)
		
		;LDA #$E5 ;Bandit Warrior
		LDA #$97 ;Orc
		STA COMBAT_SE.MOB_TILE_ID.PARM 		;The Tile_ID of the MOB on the main map which attacked the player or the player attacked
	
	JSR INITIATE.COMBAT
	JMP GAME.LAUNCH
	
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

COMMMAND.MERCHANT.TALK
PLAYTEST.KEY_SHIFT4 ;====LAUNCH MERCHANT TRANSACTIONS TEST MODE===    ;**OPT** Memory. There is no reason to keep this hook active once the inventory module is done
@START
;PARAMETERS: X-REG: NPC Record # that player is talking to

.INIT
	;X-REG: NPC Record # that player is talking to
	STX INV_8.NPC_RECORD


;*****DRIVER
				LDA #$03
				STA PLAYER.MAP.CODE
				LDA #$08
				STA INV_8.NPC_RECORD
	
	
		
;LOAD INVENTORY FILE INTO THE MAIN MEMORY SWAP SPACE
	JSR SWAP.MAIN_MEMORY.OUT ;swap out anything currently occupying the swap space
	

		LDA #$08 ;(PARM: ACC = inventory sub-module # to launch)
	JSR LAUNCH.INVENTORY.MODULE
	JSR INV_8.MERCHANT_TRANSACTIONS


.EXIT
	JSR SWAP.MAIN_MEMORY.IN ;swap in whatever was in the swap space before the COMBAT file was loaded into it


	JMP GAME.LAUNCH
	
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

KEY8 ;=====PLS (i.e Torch) TOGGLE=======
@START
;TOGGLE PLAYER LIGHT SOURCE (PLS)
	LDA PLAYER.PLS_STATUS
	CMP #$01
	BNE .NOT1
	LDA #$00 ;douse torch
	STA PLAYER.PLS_STATUS
	JMP GAME.LAUNCH							;ALL NON-MOVEMENT COMMANDS MUST EXIT VIA JMP TO GAME.LAUNCH TO AVOID PROBLEMS WITH KEY PRESS ABORTS. SEE SUBROUTINE DOCUMENTATION FOR GAME.PRIMARY_LOOP FOR DETAILS.
	;JMP GAME.PRIMARY_LOOP
.NOT1
	LDA #$01 ;light torch
	STA TW.BOTTOM_WINDOW.INIT_CODE	;($00 = none | $01 = Ignite Torch | $02 = Open door | $03 = Unlock Door | $04 = Operate lever | $05 = Invalid Command)
	STA PLAYER.PLS_STATUS
	JMP GAME.LAUNCH							;ALL NON-MOVEMENT COMMANDS MUST EXIT VIA JMP TO GAME.LAUNCH TO AVOID PROBLEMS WITH KEY PRESS ABORTS. SEE SUBROUTINE DOCUMENTATION FOR GAME.PRIMARY_LOOP FOR DETAILS.
	;JMP GAME.PRIMARY_LOOP

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
	; CMP #MAP.TYPE.TOWN_VILLAGE
	; BNE .EXIT

	;is a building map active
	;**OPT** Memory. Speed. Right now there is only one code for location type, which is used to determine the tile_set to use and for other location-type specific features. There are a lot of the later. It might be a savings if the 
					;there were a tile-set code separate from location type code, and there was a default tile-set picked using the location-type code if tile-set code was set to default. This might require adding another byte to some hex tables
					;but I suspect it would pay off. 
	CMP #MAP.TYPE.BUILDING.GRE 	;is map type = building?
	BCC .EXIT						;if no
	CMP #MAP.TYPE.BUILDING.LT	;is map type = building?
	BCS .EXIT						;if no
	;**FALLS THROUGH**				;if yes
	
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


;PLAYTEST KEY SUBROUTINES
PLAYTEST.KEY0 ;=====SET TROUBLESHOOTING HOOK=======
@START
;CODE CAN BE INSERTED IN OTHER ROUTINES TO BREAK IF THE HOOK IS SET.
;SET TROUBLESHOOTING HOOK
	LDA TROUBLESHOOTING.HOOK
	CMP #$01
	BNE .NOT1
	LDA #$00	;$00 = HOOK CLEAR
	STA TROUBLESHOOTING.HOOK
	JMP .EXIT
.NOT1
	LDA #$01	;$01 = HOOK SET
	STA TROUBLESHOOTING.HOOK
.EXIT
	RTS
@END

PLAYTEST.KEY_SHIFT0 ;=====SET TROUBLESHOOTING HOOK2=======
@START
;CODE CAN BE INSERTED IN OTHER ROUTINES TO BREAK IF THE HOOK IS SET.
;SET TROUBLESHOOTING HOOK
	LDA TROUBLESHOOTING.HOOK2
	CMP #$01
	BNE .NOT1
	LDA #$00	;$00 = HOOK CLEAR
	STA TROUBLESHOOTING.HOOK2
	JMP .EXIT
.NOT1
	LDA #$01	;$01 = HOOK SET
	STA TROUBLESHOOTING.HOOK2
.EXIT	
	RTS
@END

@END
@END
@END




@END




;TEXT BLOCKS
@START
;===MOVEMENT==
GLOBAL.TEXT_BLOCK.COMMAND_PASS		.AZ -/Pass/
GLOBAL.TEXT_BLOCK.COMMAND_NORTH		.AZ -/North/
GLOBAL.TEXT_BLOCK.COMMAND_SOUTH		.AZ -/South/
GLOBAL.TEXT_BLOCK.COMMAND_EAST		.AZ -/East/
GLOBAL.TEXT_BLOCK.COMMAND_WEST		.AZ -/West/
GLOBAL.TEXT_BLOCK.COMMAND_BLOCKED	.AZ -/Blocked!/


;===OTHER===
GLOBAL.TEXT_BLOCK.COMMAND_IGNITE_TORCH 	.AZ -/Torch Ignited/
GLOBAL.TEXT_BLOCK.COMMAND_INVALID		.AZ -/Invalid Command/		
GLOBAL.TEXT_BLOCK.COMMAND_OPEN			.AZ -/Open Door/		
GLOBAL.TEXT_BLOCK.COMMAND_UNLOCK		.AZ -/Unlock Door/		
GLOBAL.TEXT_BLOCK.COMMAND_OPERATE		.AZ -/Operate Lever/		


;====NON-COMMAND====
GLOBAL.TEXT_BLOCK.CLEAR_BOTTOM.TW		 .AZ -/               /	;used to clear the bottom text window
	
@END



;======INCLUDE FILES;
@START
;======(SAME TARGET FILE)======
@START
				.IN 	c:\my_code\na\source_code\offloaded_variables.asm
				.IN 	c:\my_code\na\source_code\routines_text.asm
				.IN 	c:\my_code\na\source_code\ptools.asm
				.IN 	c:\my_code\na\source_code\map_tools.asm
				.IN 	c:\my_code\na\source_code\event_manager.asm	
				.IN 	c:\my_code\na\source_code\misc.main_memory_only.asm	
				.IN 	c:\my_code\na\source_code\data.txt.global.asm

				.IN 	c:\my_code\na\source_code\sound_manager.asm

				
;recently moved back from BS memory

				
;TEMPORARY FOR TROUBLESHOOTING				
	;.IN 	c:\my_code\na\source_code\zone_functions
	;.IN 	c:\my_code\na\source_code\darkness_manager
	;.IN 	c:\my_code\na\source_code\map_object_management
	;.IN 	c:\my_code\na\source_code\zone_functions

@END
					
						
	.NO $9000,$EA	;FILLER TO THE UPPER LIMIT OF THE MEMORY RESERVED FOR THE MAIN GAME ENGINE CODE
;	.NO $9600,$EA	;FILLER TO THE UPPER LIMIT OF THE MEMORY RESERVED FOR THE MAIN GAME ENGINE CODE
					;This enables SBASM to generate an error if/when the code overshoots this memory address.

					
;======INCLUDE FILES (SEPERATE TARGET FILE)======
@START

				;.EN  ;end current target file

;BOOT PROCESS 
				.IN 	c:\my_code\na\source_code\noxarch.main.asm
				.IN 	c:\my_code\na\source_code\noxarch.main.old.asm
				;.IN 	c:\my_code\na\source_code\loader.p.asm

;CONTROLLERS
				.IN 	c:\my_code\na\source_code\controller.hrcg.aux.ASM
				
				
;DATA FILES: SHAPES			
				.IN 	c:\my_code\na\source_code\data.shapes.building.asm
				.IN 	c:\my_code\na\source_code\data.shapes.castle_courtyard.asm
				.IN 	c:\my_code\na\source_code\data.shapes.undermap.asm
		;		.IN 	c:\my_code\na\source_code\data.shapes.undermap_town.asm

				
;DATA FILES: SURFACE
				.IN 	c:\my_code\na\source_code\compressed_data\compressed.data.map.surface.asm
				.IN 	c:\my_code\na\source_code\data.spr.surface.asm

;DATA FILES: LOCATION 1
				.IN 	c:\my_code\na\source_code\data.tlk.l001.asm
				;floor1 (map1)
				.IN 	c:\my_code\na\source_code\data.map.l1.f1.m1.asm
				.IN 	c:\my_code\na\source_code\data.spr.l1.f1.m1.asm
				;floor2 (map2)
				.IN 	c:\my_code\na\source_code\data.map.l1.f2.m2.asm
				.IN 	c:\my_code\na\source_code\data.spr.l1.f2.m2.asm
;DATA FILES: LOCATION 2
				;.IN 	c:\my_code\na\source_code\data.tlk.l001.asm
				;floor1.1 (map3)
				.IN 	c:\my_code\na\source_code\data.map.l2.f1.1.m3.asm
				.IN 	c:\my_code\na\source_code\data.spr.l2.f1.1.m3.asm
				;floor2.1 (map4)
				.IN 	c:\my_code\na\source_code\data.map.l2.f2.1.m4.asm
				.IN 	c:\my_code\na\source_code\data.spr.l2.f2.1.m4.asm
				;floor3.1 (map5)
				.IN 	c:\my_code\na\source_code\data.map.l2.f3.1.m5.asm
				.IN 	c:\my_code\na\source_code\data.spr.l2.f3.1.m5.asm
;DATA FILES: UNDERMAP
				.IN 	c:\my_code\na\source_code\compressed_data\compressed.data.map.undermap_lv1.asm
				.IN 	c:\my_code\na\source_code\data.spr.undermap_lv1.asm



;MAIN GAME FILES				

				.IN 	c:\my_code\na\source_code\swap.routines.inventory.asm
				;
				.IN 	c:\my_code\na\source_code\swap.routines.cast_spell.setup.asm
				.IN 	c:\my_code\na\source_code\swap.routines.spell_file.asm
				.IN 	c:\my_code\na\source_code\swap.routines.combat.asm
				.IN 	c:\my_code\na\source_code\swap.routines.combat.setup.asm
				.IN 	c:\my_code\na\source_code\swap.routines.combat.exit.asm
				;
				.IN 	c:\my_code\na\source_code\swap.routines.npc.talk.asm
				.IN 	c:\my_code\na\source_code\swap.routines.non_building.asm
				.IN 	c:\my_code\na\source_code\swap.routines.building.asm
				;
				;
				.IN 	c:\my_code\na\source_code\lower_main.routines.ASM
				.IN 	c:\my_code\na\source_code\bs_routines.bank1.asm
				.IN 	c:\my_code\na\source_code\bs_routines.bank2.asm
				.IN 	c:\my_code\na\source_code\bs_aux_routines.bank2.ASM

				
;DATA FILES: GAME & PLAYER
				.IN 	c:\my_code\na\source_code\data.ply.character_sheet.ASM
				;.IN 	c:\my_code\na\source_code\data.game.inventory.ASM
				;.IN 	c:\my_code\na\source_code\data.game.mob_tables.ASM

				
@END
@END

				;***TESTING****
				; .IN 	c:\my_code\na\testing\prodos_testing\dummy.file.ASM					
