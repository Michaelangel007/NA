;************************INCLUDE FILE*****************************
;(do not assemble stand-alone.)


;GENERAL NOTES
;
;THE DEV MODE PRE-LOADER COMPRESSION LOOP
;The loop that manages the compression of the tile data stored in hex tables needs to build two
;lookup tables with the HO/LO byte of the start of the compressed data for each zone. 
;
;The length of the output array (last record written, including stop value, +1) is the return value for ZONE.COMPRESS
;The loop should compress the zones sequentially and keep a tally of the length values. On each iteration use this value as an offset from a base memory address
;where the compressed zone data will be stored, and calculate the HO/LO starting address for the current
;and store in two lookup tables.
;
;
;
;FINAL GAME & DEV-MODE IN-GAME MAP EDITS 
;
;in the final game, the zones will be read in compressed from disk. when a change to the map is made
;via the in-game editor, rewrite the compressed data (using zone.compress on the active zone), and rebuild
;the zone lookup tables. The difference in starting location of the last zone before the modified zone
;and the first zone after the modified zone is the original size of the modified zone before it was modified. 
;the original size - new size (or other way around) is the adjustment to apply to the ho/lo address of each
;zone after the modified zone. Of course, data data in the zones after the modified zone has to get moved too. 
;
;this process is the same for handling in-game edits for final-game and dev mode, I think the difference
;is that technically saving the lookup tables to disk isn't needed because the disk zone files get exported
;to the spreadsheet and the loader will recalcualte the zone lookup tables. But, even in dev mode the lookup tables need to be rebuilt in memory 
;or the game won't function. 
;
;ALTERNATE IDEA FOR RECOMPRESSING AFTER IN-GAME MAP CHANGE: 

;=====================SUBROUTINE DOCUMENTATION====================================
;
;The subroutines in this file are used to populate/update the regional map
;(RZONE.DATA) with tile data from world zones stored in auxiliary memory.
;
;For a detailed discussion on loader zones see /my_code/documentation/loader zone details.doc
;
;=================================================================================


;==========================DRIVERS=========================
@START

;description: these were used in development to test each routine independently.
;they may need some modifiction to work now.

	
	;JMP DRIVER.UNCOMPRESS
	;JMP DRIVER.COMPRESS
	;JMP DRIVER.WORLD.COMPRESS
	;JMP DRIVER.REGION.UNCOMPRESS.ALL
	;JMP DRIVER.TILE.LOOKUP.SCREEN
	;JMP DRIVER.TILE.LOOKUP.COLUMN
	;JMP DRIVER.REGION.UNCOMPRESS.NORTH
	;JMP DRIVER.REGION.UNCOMPRESS.SOUTH
	;JMP DRIVER.REGION.UNCOMPRESS.EAST
	;JMP DRIVER.REGION.UNCOMPRESS.WEST

; DRIVER.TILE.LOOKUP.COLUMN
	; JSR WORLD.COMPRESS
	; LDA #$12								;SET START WORLD ZONE
	; STA PLAYER.WMAP.ZONE
	; JSR REGION.UNCOMPRESS.ALL
	
	; LDA #$71
	; STA SMAP.CURRENT
	; LDA #$03
	; STA SMAP.CURRENT+$1
	
; ;	LDX #$00								;SET COLUMN START TILE ON SCREEN ARRAY
	; JSR TILE.LOOKUP.COLUMN
	; BRK
	
; DRIVER.TILE.LOOKUP.SCREEN
	; JSR WORLD.COMPRESS
	; LDA #$12								;SET START WORLD ZONE
	; STA PLAYER.WMAP.ZONE
	; JSR REGION.UNCOMPRESS.ALL
	
	; LDA #$68
	; STA RMAP
	; LDA #$04
	; STA RMAP+$1
	
	; JSR TILE.LOOKUP.SCREEN
	; BRK
	
; DRIVER.REGION.UNCOMPRESS.NORTH ;
	; JSR WORLD.COMPRESS
	; LDA #$12								;SET START WORLD ZONE
	; STA PLAYER.WMAP.ZONE
	
	; JSR REGION.UNCOMPRESS.ALL
			
			
	; JSR REGION.UNCOMPRESS.NORTH
	
	; BRK
	
; DRIVER.REGION.UNCOMPRESS.SOUTH ;
	; JSR WORLD.COMPRESS
	; LDA #$12								;SET START WORLD ZONE
	; STA PLAYER.WMAP.ZONE
	
	; JSR REGION.UNCOMPRESS.ALL
			
			
	; JSR REGION.UNCOMPRESS.SOUTH
	
	; BRK
	
; DRIVER.REGION.UNCOMPRESS.EAST ;
	; JSR WORLD.COMPRESS
	; LDA #$12								;SET START WORLD ZONE
	; STA PLAYER.WMAP.ZONE
	
	; JSR REGION.UNCOMPRESS.ALL		
	; JSR REGION.UNCOMPRESS.EAST
	
	; BRK	
	
; DRIVER.REGION.UNCOMPRESS.WEST ;
	; JSR WORLD.COMPRESS
	; LDA #$12								;SET START WORLD ZONE
	; STA PLAYER.WMAP.ZONE
	
	; JSR REGION.UNCOMPRESS.ALL		
	; JSR REGION.UNCOMPRESS.WEST
	
	; BRK		
	
	
; DRIVER.WORLD.COMPRESS
	; JSR WORLD.COMPRESS
	
	; ;TEMPLATE
	; CLC					;SET CARRY FLAG DESGINATD MOVE FROM AUX MEMORY -> MAIN
	; LDA #$00			;SET START ADDRESS
	; STA AUX_MOVE.START
	; LDA #$02		
	; STA AUX_MOVE.START+$1
	
	; LDA #$40			;SET END ADDRESS
	; STA AUX_MOVE.END
	; LDA #$03
	; STA AUX_MOVE.END+$1
	
	; LDA #$00			;SET DESTINATION ADDRESS
	; STA AUX_MOVE.DEST
	; LDA #$70
	; STA AUX_MOVE.DEST+$1

	; JSR AUX_MOVE
	
	
	
			; ;LDX #WORLD.COMPRESS.ZONE_INPUT.ADDRESS
			; ;LDY /WORLD.COMPRESS.ZONE_INPUT.ADDRESS
	
			; ; LDX #WORLD.COMPRESS.ZONE_OUTPUT.ADDRESS
			; ; LDY /WORLD.COMPRESS.ZONE_OUTPUT.ADDRESS
			
			; LDX #ZONE.LOOKUP.HO
			; LDY /ZONE.LOOKUP.HO
			
			; LDX #ZONE.LOOKUP.LO
			; LDY /ZONE.LOOKUP.LO
			
	; BRK
	
; DRIVER.REGION.UNCOMPRESS.ALL
; ;ALPHA TEST CODE
	; ; JSR WORLD.COMPRESS


	; ; LDA #$3F								;SET WORLD ZONE	
	; ; STA WZONE.UNCOMPRESS.CURRENT
	; ; JSR ZONE_TOOLS.UNCOMPRESS.SINGLE
		
		
	; ; LDA #$08								;SET REGIONAL WORLD ZONE
	; ; STA RZONE.UNCOMPRESS.CURRENT
	; ; JSR ZONE_TOOLS.RCOPY
	
	; ; BRK
	

; ;THE BETA TEST CODE FOR THIS ROUTINE
	; JSR WORLD.COMPRESS
	; LDA #$12								;SET START WORLD ZONE
	; STA PLAYER.WMAP.ZONE
	
	; JSR REGION.UNCOMPRESS.ALL
	; BRK
	
; DRIVER.COMPRESS ;
	; LDA #WORLD.ZONE0
	; STA ZONE_TOOLS.INPUT
	; LDA /WORLD.ZONE0
	; STA ZONE_TOOLS.INPUT+$1
	

; ;***upgrade this to get the address out of a zone lookup table

	; LDA #$00
	; STA WORLD.COMPRESS.ZONE_OUTPUT.ADDRESS				;SET AUX MEMORY ADDRESS FOR COMPRESSED DATA
	; LDA #$02
	; STA WORLD.COMPRESS.ZONE_OUTPUT.ADDRESS+$1
	
	; JSR ZONE_TOOLS.COMPRESS.SINGLE

	; ;TEMPLATE
	; CLC					;SET CARRY FLAG DESGINATD MOVE FROM AUX MEMORY -> MAIN
	; LDA #$00			;SET START ADDRESS
	; STA AUX_MOVE.START
	; LDA #$02		
	; STA AUX_MOVE.START+$1
	
	; LDA #$FF			;SET END ADDRESS
	; STA AUX_MOVE.END
	; LDA #$02
	; STA AUX_MOVE.END+$1
	
	; LDA #$00			;SET DESTINATION ADDRESS
	; STA AUX_MOVE.DEST
	; LDA #$70
	; STA AUX_MOVE.DEST+$1

	; JSR AUX_MOVE
	

	; BRK

; DRIVER.UNCOMPRESS ;
	; JSR WORLD.COMPRESS
	
	; LDA #$00						;SET WORLD ZONE TO USE FOR UNCOMPRESS ROUTINE
	; STA WZONE.UNCOMPRESS.CURRENT
	
	; JSR ZONE_TOOLS.UNCOMPRESS.SINGLE

	; BRK

@END

REGION.UNCOMPRESS.ALL ;	===UNCOMPRESS MAP DATA INTO ALL REGIONAL ZONES===		
@START
;PARAMETERS: PLAYER.WMAP.ZONE (USED TO DERVISE START WORLD ZONE), USE.COMPRESSION_FLAGS

;=====================SUBROUTINE DOCUMENTATION====================================
;
;This subroutine copies comrpessed tile data from world zones stored in auxiliary memory into 
;main memory buffer then uncompress and copy into regional map zones (RZONE.ARRAY)
;
;This is done through a loop with calls to ZONE_TOOLS.UNCOMPRESS.SINGLE & ZONE_TOOLS.RCOPY
;
;This routine increments the current world zone and the current regional zone 
;and passes those values as parameters to the subroutines mentioned above. 
;=================================================================================
	
;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA

	
;CALCUALTE START WORLD ZONE	
	LDA PLAYER.WMAP.ZONE
	SEC
	SBC #WZONE.OFFSET
	TAX
	DEX	
	STX WZONE.UNCOMPRESS.CURRENT			;WORLD ZONE COUNTER
	STX WZONE.UNCOMPRESS.START
	STX	WZONE.UNCOMPRESS.ROW				;USED WHEN CHANGING ROWS, COLUMNS USE X-REG AS OFFSET
	

;INIT COUNTERS	
	LDX #$00								;COLUMN COUNTER
	LDY #$00								;ROW COUNTER 

;INIT REGIONAL START ZONE (ALWAYS ZONE0)
	STY RZONE.UNCOMPRESS.CURRENT			;REGIONAL ZONE COUNTER
	
.LOOP_ZONE	
			
	JSR ZONE_TOOLS.UNCOMPRESS.SINGLE		;UNCOMPRESS WORLD ZONE DATA FROM AUX MEMORY TO OUTPUT BUFFER

	JSR ZONE_TOOLS.RCOPY					;COPY DATA FROM OUTPUT BUFFER INTO REGIONAL ZONE ARRAY, INTO THE ARRAY ELEMENTS FOR THE CURRENT ZONE

	INC WZONE.UNCOMPRESS.CURRENT			;NEXT WORLD ZONE
	INC RZONE.UNCOMPRESS.CURRENT			;NEXT REGIONAL ZONE

;UPDATE RZONE GRID COUNTERS. EXIT CHECK	
	INX										;INCREMENT COLUMN COUNTER
	CPX #$03								;AT END OF ROW?
	BNE .LOOP_ZONE							;IF NO, CONTINUE LOOP

	INY										;NEXT ROW
	CPY #$03								;IS LAST ROW FINISHED? 
	BEQ .EXIT								;IF YES, EXIT

;UPDATE ZONE COUNTERS
	LDX #$00								;RESET COLUMN COUNTER
	LDA WZONE.UNCOMPRESS.ROW
	CLC
	ADC #WZONE.OFFSET						;MOVE ONE ROW DOWN
	STA WZONE.UNCOMPRESS.CURRENT			;SET CURRENT WORLD ZONE TO BEGINNING OF NEXT ROW	
	STA WZONE.UNCOMPRESS.ROW
	JMP .LOOP_ZONE

	
.EXIT


	
;RESTORE REGISTERS	
	PLA
	TAY
	PLA
	TAX
	
	RTS
@END

;==========================ZONE TRANSITION SUBROUTINES==============================
@START

;=====================SUBROUTINE DOCUMENTATION====================================
;
;This collection of subroutines do the following things:
;		a) scroll the regional zone data in RZONE.ARRAY in the opposite 
;				direction of the player move. (ZONE_TOOLS.TRANSITION.NORTH/SOUTH/EAST/WEST)
;		b) load new regional zone data on the edge facing the direction 
;				of the player move (REGION.UNCOMPRESS.NORTH/SOUTH/EAST/WEST).
;
;ZONE_TOOLS.TRANSITION.NORTH/SOUTH/EAST/WEST calculates the starting 
;memory address in RZONE.ARRAY to copy to/from, used by ZONE_TOOLS.SCROLL_ROW.COPY 
;
;REGION.UNCOMPRESS.NORTH/SOUTH/EAST/WEST increments the world and regional zone,
;used by ZONE_TOOLS.UNCOMPRESS.SINGLE and ZONE_TOOLS.RCOPY		
;	
;============================================================================================

ZONE_TOOLS.TRANSITION.NORTH ; ========NORTH ENTRANCE WHEN LOAD THREADSHOLD TRIGGERS======
@START
;PARAMETERS: PLAYER.WMAP.ZONE (USED TO DETERMINE WHICH NEW ZONES TO LOAD)
;RETURN: RZONE.ARRAY (UDPATED)
;ENTRANCE: DIRECT

;====================SCROLL ZONES========================

;INIT INDEXES	
	LDX #$01								;REGION ROW COUNTER
	

.LOOP_SCROLL
;SET RZONE BASE ADDRESS

;
;NOTE: RZONE.SOUTH.TABLE IS USED WITH THE TO/FROM REVERSED BECAUSE IT CONTAINS THE CORRECT VALUES FOR THE NORTH TRANSITION AS LONG AS THE X-REG COUNTER IS RUN BACKRWARDS. 
;
;=========COPY FROM========
;=======INLINE CODE FOR ADC.16========	
;RZONE.ARRAY(2) RZONE.LOOKUP.ARRAY_STOP.LO/HO(2)
	
	CLC                          				;ALWAYS BEFORE ADD
	LDA #RZONE.ARRAY							;OP1
	ADC RZONE.SOUTH.TABLE_TO.LO,X				;OP2  
	STA RZONE.ARRAY.INDEX.SCROLL_FROM
				 
	LDA /RZONE.ARRAY							;OP1+$1
	ADC RZONE.SOUTH.TABLE_TO.HO,X				;OP2+$2 carry flag not cleared via CLC intentionally, it's part of 16-bit adding. 
	STA RZONE.ARRAY.INDEX.SCROLL_FROM+$1
;======================================	

;=========COPY TO========
;=======INLINE CODE FOR ADC.16========	
;RZONE.ARRAY(2) RZONE.LOOKUP.ARRAY_STOP.LO/HO(2)
	
	CLC                          				;ALWAYS BEFORE ADD
	LDA #RZONE.ARRAY							;OP1
	ADC RZONE.SOUTH.TABLE_FROM.LO,X			;OP2  
	STA RZONE.ARRAY.INDEX.SCROLL_TO
				 
	LDA /RZONE.ARRAY							;OP1+$1
	ADC RZONE.SOUTH.TABLE_FROM.HO,X			;OP2+$2 carry flag not cleared via CLC intentionally, it's part of 16-bit adding. 
	STA RZONE.ARRAY.INDEX.SCROLL_TO+$1

;======================================	

	
	JSR ZONE_TOOLS.SCROLL_ROW.COPY 

	DEX
	CPX #$FF
	BNE .LOOP_SCROLL

	;***FALLS THROUGH
@END

REGION.UNCOMPRESS.NORTH ; ===LOADS 3 ZONES NORTH OF THE REGION MAP====
@START
;PARAMETERS: PLAYER.WMAP.ZONE (USED TO DERVISE START WORLD ZONE)
;ENTRANCE: ZONE_TOOLS.TRANSITION.NORTH
;RETURN: NONE

;=====================SUBROUTINE DOCUMENTATION====================================
;
;After loading the 3 zones to the north, the contents of RZONE 0,1,2 are
;replaced with the new tile data. This works in conjunction with zone scrolling to provide
;the tile functions for player movement.
;
;=================================================================================


;SAVE REGISTERS
	TXA
	PHA
	; TYA
	; PHA

;CALCULATE START WORLD ZONE	
	LDA PLAYER.WMAP.ZONE
	SEC
	SBC #WZONE.OFFSET2
	TAX
	DEX	
	STX WZONE.UNCOMPRESS.CURRENT			;WORLD ZONE COUNTER
	STX WZONE.UNCOMPRESS.START	
				
;INIT COUNTERS	
	LDX #$00								;COLUMN COUNTER 

;INIT REGIONAL START ZONE (ALWAYS ZONE0)
	STX RZONE.UNCOMPRESS.CURRENT			;REGIONAL ZONE COUNTER
	
	
.LOOP_ZONE


	JSR ZONE_TOOLS.UNCOMPRESS.SINGLE		;UNCOMPRESS WORLD ZONE DATA FROM AUX MEMORY TO OUTPUT BUFFER		
	JSR ZONE_TOOLS.RCOPY					;COPY DATA FROM OUTPUT BUFFER INTO REGIONAL ZONE ARRAY, INTO THE ARRAY ELEMENTS FOR THE CURRENT ZONE
			
	INC WZONE.UNCOMPRESS.CURRENT			;NEXT WORLD ZONE
	INC RZONE.UNCOMPRESS.CURRENT			;NEXT REGIONAL ZONE

;UPDATE RZONE GRID COUNTERS. EXIT CHECK	
	INX										;INCREMENT COLUMN COUNTER
	CPX #$03								;AT END OF ROW?
	BNE .LOOP_ZONE							;IF NO, CONTINUE LOOP

.EXIT
	
;RESTORE REGISTERS	
	; PLA
	; TAY
	PLA
	TAX
	
	RTS
@END
	
ZONE_TOOLS.TRANSITION.SOUTH ; ========SOUTH ENTRANCE WHEN LOAD THREADSHOLD TRIGGERS======
@START
;PARAMETERS: PLAYER.WMAP.ZONE (USED TO DETERMINE WHICH NEW ZONES TO LOAD)
;RETURN: RZONE.ARRAY (UDPATED)
;ENTRANCE: DIRECT

;====================SCROLL ZONES========================


;INIT INDEXES	
	LDX #$00								;REGION ROW COUNTER
	

.LOOP_SCROLL	
;SET RZONE BASE ADDRESS
;=======INLINE CODE FOR ADC.16========	
;RZONE.ARRAY(2) RZONE.LOOKUP.ARRAY_STOP.LO/HO(2)
	
	CLC                          				;ALWAYS BEFORE ADD
	LDA #RZONE.ARRAY							;OP1
	ADC RZONE.SOUTH.TABLE_FROM.LO,X		;OP2  
	STA RZONE.ARRAY.INDEX.SCROLL_FROM
				 
	LDA /RZONE.ARRAY							;OP1+$1
	ADC RZONE.SOUTH.TABLE_FROM.HO,X		;OP2+$2 carry flag not cleared via CLC intentionally, it's part of 16-bit adding. 
	STA RZONE.ARRAY.INDEX.SCROLL_FROM+$1

;======================================	
	
;=======INLINE CODE FOR ADC.16========	
;RZONE.ARRAY(2) RZONE.LOOKUP.ARRAY_STOP.LO/HO(2)
	
	CLC                          				;ALWAYS BEFORE ADD
	LDA #RZONE.ARRAY							;OP1
	ADC RZONE.SOUTH.TABLE_TO.LO,X				;OP2  
	STA RZONE.ARRAY.INDEX.SCROLL_TO
				 
	LDA /RZONE.ARRAY							;OP1+$1
	ADC RZONE.SOUTH.TABLE_TO.HO,X				;OP2+$2 carry flag not cleared via CLC intentionally, it's part of 16-bit adding. 
	STA RZONE.ARRAY.INDEX.SCROLL_TO+$1

;======================================	

	
	JSR ZONE_TOOLS.SCROLL_ROW.COPY 
	
	INX
	CPX #$02
	BNE .LOOP_SCROLL
	
	;***FALLS THROUGH
@END
	
REGION.UNCOMPRESS.SOUTH ; ===LOADS 3 ZONES SOUTH OF THE REGION MAP====
@START
;ENTRANCE: VIA ZONE_TOOLS.TRANSITION.SOUTH 
;ENTRANCE: ZONE_TOOLS.TRANSITION.SOUTH
;RETURN: NONE

;=====================SUBROUTINE DOCUMENTATION====================================
;
;After loading the 3 world zones to the south of the regional map, the contents of regional zone 6,7,8 are
;replaced with the new tile data. This works in conjunction with zone scrolling to provide
;the tile functions for player movement. 
;
;=================================================================================


;SAVE REGISTERS
	; TXA
	; PHA
	; TYA
	; PHA

;CALCULATE START WORLD ZONE	
	LDA PLAYER.WMAP.ZONE
	CLC
	ADC #WZONE.OFFSET2
	TAX
	DEX	
	STX WZONE.UNCOMPRESS.CURRENT			;WORLD ZONE COUNTER
	STX WZONE.UNCOMPRESS.START
	
				
;INIT COUNTERS	
	LDX #$00								;COLUMN COUNTER 

;INIT REGIONAL START ZONE (ALWAYS THE SAME)
	LDA #$06
	STA RZONE.UNCOMPRESS.CURRENT			;REGIONAL ZONE COUNTER
	
	
.LOOP_ZONE

	JSR ZONE_TOOLS.UNCOMPRESS.SINGLE		;UNCOMPRESS WORLD ZONE DATA FROM AUX MEMORY TO OUTPUT BUFFER		
				; LDA TROUBLESHOOTING.HOOK
				; CMP #$01
				; BNE .TEMP
				; LDA TEXT
				; LDA WZONE.UNCOMPRESS.CURRENT
				; LDX PLAYER.WMAP.ZONE
				; BRK
; .TEMP


	JSR ZONE_TOOLS.RCOPY					;COPY DATA FROM OUTPUT BUFFER INTO REGIONAL ZONE ARRAY, INTO THE ARRAY ELEMENTS FOR THE CURRENT ZONE
				
	INC WZONE.UNCOMPRESS.CURRENT			;NEXT WORLD ZONE
	INC RZONE.UNCOMPRESS.CURRENT			;NEXT REGIONAL ZONE

;UPDATE RZONE GRID COUNTERS. EXIT CHECK	
	INX										;INCREMENT COLUMN COUNTER
	CPX #$03								;AT END OF ROW?
	BNE .LOOP_ZONE							;IF NO, CONTINUE LOOP

.EXIT


	
;RESTORE REGISTERS	
	; PLA
	; TAY
	; PLA
	; TAX
	
	RTS	
@END

ZONE_TOOLS.TRANSITION.EAST ; ========EAST ENTRANCE WHEN LOAD THREADSHOLD TRIGGERS======
@START
;PARAMETERS: PLAYER.WMAP.ZONE (USED TO DETERMINE WHICH NEW ZONES TO LOAD)
;RETURN: RZONE.ARRAY (UDPATED)
;ENTRANCE: DIRECT

;====================SCROLL ZONES========================


;INIT INDEXES	
	LDX #$00								;REGION ZONE COUNTER



.LOOP_SCROLL
;SET RZONE BASE ADDRESS

;=========COPY FROM========
;=======INLINE CODE FOR ADC.16========	
;RZONE.ARRAY(2) RZONE.LOOKUP.ARRAY_STOP.LO/HO(2)
	
	CLC                          				;ALWAYS BEFORE ADD
	LDA #RZONE.ARRAY							;OP1
	ADC RZONE.EAST.TABLE_FROM.LO,X				;OP2  
	STA RZONE.ARRAY.INDEX.SCROLL_FROM
				 
	LDA /RZONE.ARRAY							;OP1+$1
	ADC RZONE.EAST_WEST.TABLE_FROM.HO,X				;OP2+$2 carry flag not cleared via CLC intentionally, it's part of 16-bit adding. 
	STA RZONE.ARRAY.INDEX.SCROLL_FROM+$1
;======================================	

;=========COPY TO========
;=======INLINE CODE FOR ADC.16========	
;RZONE.ARRAY(2) RZONE.LOOKUP.ARRAY_STOP.LO/HO(2)
	
	CLC                          				;ALWAYS BEFORE ADD
	LDA #RZONE.ARRAY							;OP1
	ADC RZONE.EAST.TABLE_TO.LO,X				;OP2  
	STA RZONE.ARRAY.INDEX.SCROLL_TO
				 
	LDA /RZONE.ARRAY							;OP1+$1
	ADC RZONE.EAST_WEST.TABLE_TO.HO,X			;OP2+$2 carry flag not cleared via CLC intentionally, it's part of 16-bit adding. 
	STA RZONE.ARRAY.INDEX.SCROLL_TO+$1

;======================================	

	JSR ZONE_TOOLS.SCROLL_COLUMN.COPY 

	INX
	CPX #$06									;LAST ZONE? (6 TOTAL)
	BNE .LOOP_SCROLL
	
	
	;***FALLS THROUGH
	

@END
	
REGION.UNCOMPRESS.EAST ; ===LOADS 3 ZONES EAST OF THE REGION MAP====
@START
;PARAMETERS: PLAYER.WMAP.ZONE (USED TO DERVISE START WORLD ZONE)
;ENTRANCE: ZONE_TOOLS.TRANSITION.EAST
;RETURN: NONE

;=====================SUBROUTINE DOCUMENTATION====================================
;
;After loading the 3 zones to the south, the contents of RZONE 2,5,8 are
;replaced with the new tile data. This works in conjunction with zone scrolling to provide
;the tile functions for player movement. 
;
;=================================================================================


			
;SAVE REGISTERS
	TXA
	PHA
	; TYA
	; PHA

;CALCULATE START WORLD ZONE	
	LDA PLAYER.WMAP.ZONE
	SEC
	SBC #WZONE.OFFSET
	TAX
	INX
	INX
	STX WZONE.UNCOMPRESS.CURRENT			;WORLD ZONE COUNTER
	STX WZONE.UNCOMPRESS.START


			
;INIT COUNTERS	
	LDX #$00								;COLUMN COUNTER 

;INIT REGIONAL START ZONE (ALWAYS THE SAME)
	LDA #$02
	STA RZONE.UNCOMPRESS.CURRENT			;REGIONAL ZONE COUNTER
	
	
.LOOP_ZONE
	
	JSR ZONE_TOOLS.UNCOMPRESS.SINGLE		;UNCOMPRESS WORLD ZONE DATA FROM AUX MEMORY TO OUTPUT BUFFER				

			
		;LDA $1000
		
	JSR ZONE_TOOLS.RCOPY					;COPY DATA FROM OUTPUT BUFFER INTO REGIONAL ZONE ARRAY, INTO THE ARRAY ELEMENTS FOR THE CURRENT ZONE

	
			; ;PLA
		; PLA
		; TAX
		; PLA
		; TAY
		; LDA #$AB
		; JMP FULL.BRK

			; LDA WZONE.UNCOMPRESS.CURRENT
			; ; LDX #WZONE.OFFSET
			; ; LDY #$EE
			; LDX #WZONE.UNCOMPRESS.CURRENT
			; LDY /WZONE.UNCOMPRESS.CURRENT
			; JMP FULL.BRK
			
	LDA WZONE.UNCOMPRESS.CURRENT			;NEXT WORLD ZONE
	CLC
	ADC #WZONE.OFFSET						;MOVE 1 ZONE DOWN
	STA WZONE.UNCOMPRESS.CURRENT


			
			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP2
			; LDA WZONE.UNCOMPRESS.CURRENT
			; ; LDX #WZONE.OFFSET
			; ; LDY #$EE
			; LDX #WZONE.UNCOMPRESS.CURRENT
			; LDY /WZONE.UNCOMPRESS.CURRENT
			; JMP FULL.BRK
			; BRK
; .TEMP2	
			; LDA TEMP
			
	
	LDA RZONE.UNCOMPRESS.CURRENT			;NEXT REGIONAL ZONE
	CLC
	ADC #RZONE.OFFSET						;MOVE 1 ZONE DOWN
	STA RZONE.UNCOMPRESS.CURRENT
	
;UPDATE RZONE GRID COUNTERS. EXIT CHECK	

		
	INX										;INCREMENT COLUMN COUNTER
	CPX #$03								;AT END OF ROW?
	BNE .LOOP_ZONE							;IF NO, CONTINUE LOOP	
	
	
.EXIT


			
			
;RESTORE REGISTERS	
	; PLA
	; TAY
	PLA
	TAX
	
	RTS	
@END

ZONE_TOOLS.TRANSITION.WEST ; ========WEST ENTRANCE WHEN LOAD THREADSHOLD TRIGGERS======
@START
;PARAMETERS: PLAYER.WMAP.ZONE (USED TO DETERMINE WHICH NEW ZONES TO LOAD)
;RETURN: RZONE.ARRAY (UDPATED)
;ENTRANCE: DIRECT

;====================SCROLL ZONES========================

;INIT INDEXES	
	LDX #$00								;REGION ZONE COUNTER

.LOOP_SCROLL
;SET RZONE BASE ADDRESS

;=========COPY FROM========
;=======INLINE CODE FOR ADC.16========	
;RZONE.ARRAY(2) RZONE.LOOKUP.ARRAY_STOP.LO/HO(2)
	
	CLC                          				;ALWAYS BEFORE ADD
	LDA #RZONE.ARRAY							;OP1
	ADC RZONE.WEST.TABLE_FROM.LO,X				;OP2  
	STA RZONE.ARRAY.INDEX.SCROLL_FROM
				 
	LDA /RZONE.ARRAY							;OP1+$1
	ADC RZONE.EAST_WEST.TABLE_FROM.HO,X			;OP2+$2 carry flag not cleared via CLC intentionally, it's part of 16-bit adding. 
	STA RZONE.ARRAY.INDEX.SCROLL_FROM+$1
;======================================	

;=========COPY TO========
;=======INLINE CODE FOR ADC.16========	
;RZONE.ARRAY(2) RZONE.LOOKUP.ARRAY_STOP.LO/HO(2)
	
	CLC                          				;ALWAYS BEFORE ADD
	LDA #RZONE.ARRAY							;OP1
	ADC RZONE.WEST.TABLE_TO.LO,X				;OP2  
	STA RZONE.ARRAY.INDEX.SCROLL_TO
				 
	LDA /RZONE.ARRAY							;OP1+$1
	ADC RZONE.EAST_WEST.TABLE_TO.HO,X			;OP2+$2 carry flag not cleared via CLC intentionally, it's part of 16-bit adding. 
	STA RZONE.ARRAY.INDEX.SCROLL_TO+$1

	
	JSR ZONE_TOOLS.SCROLL_COLUMN.COPY 


;======================================	

	INX
	CPX #$06									;LAST ZONE? (6 TOTAL)
	BNE .LOOP_SCROLL
		
	;***FALLS THROUGH
@END
	
REGION.UNCOMPRESS.WEST ; ===LOADS 3 ZONES WEST OF THE REGION MAP====
@START
;PARAMETERS: PLAYER.WMAP.ZONE (USED TO DERVISE START WORLD ZONE)
;ENTRANCE: ZONE_TOOLS.TRANSITION.WEST
;RETURN: NONE

;=====================SUBROUTINE DOCUMENTATION====================================
;
;After loading the 3 zones to the west, the contents of RZONE 0,3,6 are
;replaced with the new tile data. This works in conjunction with zone scrolling to provide
;the tile functions for player movement. 
;
;=================================================================================


;**OPT** Memory. I think the north/south and east/west region.uncompress routines could be combined
;with a branch based on MOVE.CURRENT to different routines for calculating the start world & region zone. 
;I think the loop can be shared. 


;SAVE REGISTERS
	TXA
	PHA
	; TYA
	; PHA

;CALCULATE START WORLD ZONE	
	LDA PLAYER.WMAP.ZONE
	SEC
	SBC #WZONE.OFFSET
	TAX
	DEX
	DEX
	STX WZONE.UNCOMPRESS.CURRENT			;WORLD ZONE COUNTER
	STX WZONE.UNCOMPRESS.START
	
				
;INIT COUNTERS	
	LDX #$00								;COLUMN COUNTER 

;INIT REGIONAL START ZONE (ALWAYS THE SAME)
	LDA #$00
	STA RZONE.UNCOMPRESS.CURRENT			;REGIONAL ZONE COUNTER
	
	
.LOOP_ZONE	
	JSR ZONE_TOOLS.UNCOMPRESS.SINGLE		;UNCOMPRESS WORLD ZONE DATA FROM AUX MEMORY TO OUTPUT BUFFER		
	JSR ZONE_TOOLS.RCOPY					;COPY DATA FROM OUTPUT BUFFER INTO REGIONAL ZONE ARRAY, INTO THE ARRAY ELEMENTS FOR THE CURRENT ZONE
				
	LDA WZONE.UNCOMPRESS.CURRENT			;NEXT WORLD ZONE
	CLC
	ADC #WZONE.OFFSET						;MOVE 1 ZONE DOWN
	STA WZONE.UNCOMPRESS.CURRENT
	
	
	LDA RZONE.UNCOMPRESS.CURRENT			;NEXT REGIONAL ZONE
	CLC
	ADC #RZONE.OFFSET						;MOVE 1 ZONE DOWN
	STA RZONE.UNCOMPRESS.CURRENT
	
;UPDATE RZONE GRID COUNTERS. EXIT CHECK	
	INX										;INCREMENT COLUMN COUNTER
	CPX #$03								;AT END OF ROW?
	BNE .LOOP_ZONE							;IF NO, CONTINUE LOOP	
	
	
.EXIT
	
;RESTORE REGISTERS	
	; PLA
	; TAY
	PLA
	TAX
	
	RTS	
@END

@END

;==========================ZONE TOOLS==============================
@START


ZONE_TOOLS.BUILD.WZONE_HEADERS
@START	
;PARAMETERS: TOTAL.SECTORS, USE.COMPRESSION_FLAGS
;ENTRANCE: DIRECT
;RETURN: ZONE.LOOKUP.LO/HO
;
;*Set equal to TOTAL.SECTORS from JSR DISK.READ.AUX used to read
;the compressed data from disk.
;The value is the number of pages of aux memory the compressed data resides in, which is also the
;the same as the number of sectors the data takes up on disk, except for the header sector
;if the compressed data was written to disk by AppleCommander.
 

;=====================SUBROUTINE DOCUMENTATION====================================
;
;-Overview
;This subroutine is designed to build the lookup table which
;contains the starting aux memory address of each world zone (ZONE.LOOKUP.HO/LO)
;
;It should be called right away after loading compressed map data
;from disk for a full sized world map (128x128 tiles, 64 zones)
;
;==DETAILED REVIEW==
;The basic approach of this routine is to copy two pages of zone data
;from aux memory to a main memory buffer, and then search for the
;$00 stop value at the end of each zone, setting the start address
;of the next zone equal to the address after a stop value is found.
;
;-The Search Pattern
;The search pattern is a little more complicated to take into consideration
;other uses of the value $00 in the tile_qty field (a compressed tile record is 2 bytes: tile qty, tile_type)
;
;The other uses are: $00 in tile_qty can also indicate !256 tiles,
;in which case candidate stop value +$2 is always $00 because it's the next stop value
;
;However, we can't conclude that candidate stop value +$2 isn't a real
;stop value because +$2 is = $00 because that pattern could result
;from a zone with !256 mountains. With it's stop value that zone's
;compressed data would be 00.00.00. Accordingly, the search pattern
;checks the next 3 bytes after any occurance of $00 before concluding
;it is a stop value. 
;
;See the first section in .INDEX.SEARCH_PASS1.LOOP & .INDEX.SEARCH_PASS2.LOOP
;for the code that implements the search pattern.
;
;-Multiple Pass Search
;The code is very complex because it is searching a data stream
;which spans multiple pages, and due to the search pattern required
;as described above, access to the first few bytes after the end of
;the current page is required. 
;
;As a result, the 8-bit index has to be reset during the search
;for a given page of data. To do this the search code was logically
;divided into PASS1 and PASS2, arbitrarily transitioning when the 
;counter reaches $80. Since the presense of stop values can alter whether
;the tile_qty is in an odd or even byte, the arbitrary break can
;cause the tile counter to become out of sync with tile_qty, since
;the tile counter starts at $00 in PASS2. To remedy this, COUNTER.ADJ
;is used to adjust the tile counter at the start of PASS2 if needed.
;
;-Zones with Compression Disabled
;The code for this is complicated. On the surface it seems 
;simple enough; there is a routine .ZONE.COMPRESSION.CHECK at
;the top of .OUTERLOOP which takes care of the lookup table updates
;for compression disabled zones since the the next zone's 
;start address is always 1 page ahead of the start address of 
;a compression disabled zone. 
; 
;One wrinkle is that if a compression disabled zone is found, the
;the stop value search in the next compression enabled zone must start
;at the last byte of the compresion disabled zone (which doesn't
;have a stop value, BTW). The issue is, normally the stop value
;search starts at X-REG = $00, the beginning of the first page
;of zone data in the buffer. But in the circumstances described, it
;needs to start somewhere in the middle...essentially the value of
;X-REG+1 when the stop value when the compression disabled zone 
;was found. This is managed through the variables X-REG.RESUME
;
;There is also code at the end of the PASS1/PASS2 stop value found
;routines (.STOP_VALUE_FOUND.PASS1/2) to check to see if the next
;zone has compression disabled because if so, the search routine 
;needs to terminate and let the .ZONE.COMPRESSION.CHECK routine 
;at the top of .OUTERLOOP take over. 
;
;There are just quite frankly a lot of values being passed around
;that have to be exactly correct at specific times in a code structure
;which is a bit intertwined. The value of ZONE.BASE.ADDRESS is another
;example. It gets affected by X-REG.RESUME if a compression disabled
;zone was found, but that means it isn't always affected. 
;
;If a compression disabled zone is found with a stop value at
;X-REG >= $80, then the next compression enabled zone needs
;to start the stop value search in PASS2.
;
;Lots of little quirks like this.
;
;Lesson learned: if possible avoid situations where a search
;must be done through multiple pages of data where not only the
;arrays don't have consistent lengths (the compressed zone data)
;but the records don't start/stop consistently on odd/even bytes,
;which introduces the problem of transitioning from one page of data 
;to the next. 
;
;=================================================================================

; ;======DRIVER TEMPLATE========
	;
	;****NOTE: IT IS ASSUMED THAT TOTAL.SECTORS WAS ALREADY INIT BY A CALL TO DISK.READ.AUX IMMEDIATELY PRIOR. 
	;	DEC TOTAL.SECTORS ;decrement to exclue header sector, because that wasn't loaded into aux memory)			
	;JSR ZONE_TOOLS.BUILD.WZONE_HEADERS2


			;TROUBLESHOOTING HOOK
			;(set zone number hear, uncomment the 3 other hooks below, and execution will stop when zone # is found)
			LDA #$FF			;SET ZONE # FOR TROUBLESHOOTING TRAP
			STA TEMP16
			
			;TROUBLESHOOTING HOOK
			;(erase existing zone table)
			LDX #$00
			
.TEMP.LOOP
			LDA #$AA
			STA ZONE.LOOKUP.LO,X
			INX
			CPX #$82
			BNE .TEMP.LOOP
			
		
;=======INIT==========
@START


;INDEX THE ZONE DATA

	;INIT COUNTERS	
	LDX #$00							;outerloop (page/sector) counter, index.search.loop (tile record) index/counter, which must stay in sync with the first field in the 2 byte compressed tile record (tile_qty, tile_type)
	LDY #$00							;ZONE.LOOKUP TABLE INDEX
	
	;SETUP AUX MOVE	
	LDA #WORLD.COMPRESS.AUX_MEMORY.START_LO				;start address of current zone
	STA ZONE.LOOKUP.LO,Y								;save first zone's address to lookup table, the loop will do the rest of the zones
	STA AUX.READ_ADDRESS.START
	
	LDA #WORLD.COMPRESS.AUX_MEMORY.START_HO				;save first zone's address to lookup table, the loop will do the rest of the zones
	STA ZONE.LOOKUP.HO,Y
	STA AUX.READ_ADDRESS.START+$1
	STA AUX.READ_ADDRESS.END			;this isn't a 2 byte variable, because the LO byte of end address is always #$FF, which is hard coded. 
	INC AUX.READ_ADDRESS.END			;we're reading in two pages of aux memory at a time so that the search has access to the first few values after the end of a page when identifying the stop value. 
	
	INY									;next zone table record

;SETUP ZONE START ADDRESSES USED IN LOOKUP TABLE BUILD	
	LDA AUX.READ_ADDRESS.START			;the aux addresses are copied to the zone addresses in several locations. They are all needed. It has to do with the intertwined nature of the code. This particular instance in INIT is needed because the zone address values are used in .ZONE.COMPRESSION.CHECK	at the top of .OUTERLOOP and the other instances need to be placed further downstream in case .ZONE.COMPRESSION.CHECK changes the aux_memory values which in turn requries the zone address values to be updated. 
	STA ZONE.START.BASE_ADDRESS

	LDA AUX.READ_ADDRESS.START+$1
	STA ZONE.START.BASE_ADDRESS+$1	
	
	
;SETUP OTHER STUFF
	LDA #$00
	STA COUNTER.ADJ						;Used to sync up the value of the compressed tile record index between search PASS1 & PASS2
	STA PRIOR.ZONE.INDEX				;Aways == ZONE.LOOKUP.TABLE index -1. This because the index is used to record the zone start address, and the a zone's start address is automatically calcualted as (prior zone start address + 1 page) if the prior zone had compression disabled. PRIOR.ZONE.INDEX holds the index to the prior zone. 
	STA XREG.RESUME						;Used to carryover the compressed tile record index to the first compression enabled zone after a compression disabled zone has been found. 


@END
	
.OUTERLOOP.ENTRANCE
@START

.OUTERLOOP
	STX SAVED.XREG.LOCAL				;save outerloop (page/sector) counter, index.search.loop (tile record) index/counter,


;UPDATE ZONE START ADDRESSES USED IN LOOKUP TABLE BUILD	
;(to reflect the increments made in .EXIT_TEST.OUTERLOOP)
	LDA AUX.READ_ADDRESS.START
	STA ZONE.START.BASE_ADDRESS

	LDA AUX.READ_ADDRESS.START+$1
	STA ZONE.START.BASE_ADDRESS+$1		

;RESET COMPRESSED TILE RECORD
	LDX #$00
	STX SAVED.XREG.LOCAL1				;save compressed tile record index

			
		
.ZONE.COMPRESSION.CHECK	
@START
;(no need to search for zone stop values until the first compressed zone starts)	

;USE ZONE COMPRESSION FLAGS?
	LDA USE.COMPRESSION_FLAGS
	BEQ .LOAD.ZONE.DATA			;are compression flags off ($00)? if yes, then zone is compressed. skip check of WZONE.COMPRESSION.FLAGS
								;if not, check WZONE.COMPRESSION.FLAGS to see if current zone is a zone that has compression ON or OFF

;WAS PRIOR ZONE COMPRESSION DISABLED?
;(if yes then the start address of the current zone is automatically +1page from the last zone start address)
	LDX PRIOR.ZONE.INDEX				;= YREG -1. 
	LDA WZONE.COMPRESSION.FLAGS, X
	BNE .LOAD.ZONE.DATA

	
;SET X-REG TO COMPRESSED TILE RECORD INDEX
	LDA #$00							;set x-reg = to xreg.resume, which is #$00 unless an uncompressed zone was found. 
	CLC
	ADC XREG.RESUME						;used to carryover the compressed tile record index if an uncompressed zone was found in the last iteration of SEARCH.INDEX.LOOP
	TAX
	STA ZONE.START.BASE_ADDRESS			;must be updated because we're about to use it to record a zone start address in this loop.
	STA SAVED.XREG.LOCAL1				;index was reset and saved above, so we need to override it so that when INDEX.SEARCH restores the index it will have the resume value, since a zone with compression disabled was found this iteration. 	
	LDX PRIOR.ZONE.INDEX				;restore needed because X-REG was modified a couple lines above

	LDA ZONE.START.BASE_ADDRESS			;record the start address of the current zone		
	STA ZONE.LOOKUP.LO,Y
		
	LDA ZONE.START.BASE_ADDRESS+$1
	STA ZONE.LOOKUP.HO,Y

			; ;TROUBLESHOOTING HOOK
			; CPY TEMP16
			; BNE .TEMP1
			; LDA TEXT
			; LDA $C082
			; ;LDY TOTAL.SECTORS
			; ;LDA AUX.READ_ADDRESS.START+$1
			; ;LDA NEW.MAP,X
			; ;LDA ZONE.LOOKUP.LO,Y
			; ;LDY TEMP
			; ;
			; LDA ZONE.START.BASE_ADDRESS
			; ;LDX PRIOR.ZONE.INDEX					;= YREG -1. 
			; ;LDA WZONE.COMPRESSION.FLAGS, X			
			; LDY ZONE.START.BASE_ADDRESS+$1
			; ;LDX AUX.READ_ADDRESS.START+$1
			; BRK
; .TEMP1 


	INC PRIOR.ZONE.INDEX				;= YREG -1. 
	INY									;increment zone lookup table index		
	INX									;increment WZONE.COMPREESSION.FLAGS index

	CPX #WZONE.TOTAL.PLUSONE				;Has last zone been processed?
	BNE .NEXT_ZONE						;if no, continue loop
	JMP .EXIT							;all zones are uncompressed, all zone indexes recorded. 
	
.NEXT_ZONE
;USE ZONE COMPRESSION FLAGS?
	LDA USE.COMPRESSION_FLAGS
	BEQ .LOAD.ZONE.DATA			;are compression flags off ($00)? if yes, then zone is compressed. skip check of WZONE.COMPRESSION.FLAGS
								;if not, check WZONE.COMPRESSION.FLAGS to see if current zone is a zone that has compression ON or OFF
;IS ZONE COMPRESSION DISABLED?
	LDA WZONE.COMPRESSION.FLAGS, X		;is next zone compression disabled?
	BNE .LOAD.ZONE.DATA					;if no, exit loop. No need to increment load counters because the next zone's stop value might be in the zone data already in memory. 

	
.INCREMENT.LOAD.COUNTERS
	INC AUX.READ_ADDRESS.START+$1		;increment aux move by 1 page/sector
	LDA AUX.READ_ADDRESS.START+$1
	STA ZONE.START.BASE_ADDRESS+$1		;need to be updated now because it is used right away at the top of the loop. The aux_end address won't be used until .LOAD.ZONE.DATA which takes care of the update. 

	INC AUX.READ_ADDRESS.END

	INC SAVED.XREG.LOCAL				;outerloop (page/sector) counter, index.search.loop (tile record) index/counter,
	JMP .ZONE.COMPRESSION.CHECK			;continue loop
	

@END

.LOAD.ZONE.DATA

;RESET ZONE START ADDRESSES USED IN LOOKUP TABLE BUILD
;(to reflect any increments made in .ZONE.COMPRESSION.CHECK)	
	
	LDA AUX.READ_ADDRESS.START
	STA ZONE.START.BASE_ADDRESS

	LDA AUX.READ_ADDRESS.START+$1
	STA ZONE.START.BASE_ADDRESS+$1			
		
	
;EXECUTE AUX MOVE (PAGE 1)
	LDA AUX.READ_ADDRESS.START			;set aux move start address
	STA AUX_MOVE.START
	
	LDA AUX.READ_ADDRESS.START+$1	
	STA AUX_MOVE.START+$1

			
	LDA #$FF							;set aux move end address
	STA AUX_MOVE.END					;this isn't a 2 byte variable, because the LO byte of end address is always #$FF, which is hard coded. 

	LDA AUX.READ_ADDRESS.END
	STA AUX_MOVE.END+$1			

	
	LDA #MAP.DATA.DEST_ADDR				;set destination address
	STA AUX_MOVE.DEST
	LDA /MAP.DATA.DEST_ADDR
	STA AUX_MOVE.DEST+$1

	
	CLC									;clear carry flag desginatd move from aux -> main memory
	JSR AUX_MOVE						;copy 2 pages of zone data from aux memory to main memory	


	
.INDEX.SEARCH.ENTRANCE
@START	
;CALCUALTE ZONE INDEXES IN CURRENT SECTOR

	LDX SAVED.XREG.LOCAL1				;restore compressed tile record index

			
	CPX #$80							;X-REG.RESUME was applied to the compressed tile record index, if appropriate, in .ZONE.COMPRESSION.CHECK. This CPX/branch is to determine whether the next search should start in PASS1 or PASS2. The later starts at byte $80 of the current page of zone data. 
	BCC	.INDEX.SEARCH_PASS1.LOOP.ENTRANCE
	
	;ADJUST X-REG COMRPESSED TILE INDEX
	TXA 								;PASS2 starts $80 bytes into the zone data in the current page, but x-reg is reset to $00. So, to resume x-reg at the position contemplated by XREG.RESUME, we have to subtract $80 first. 
	SEC
	SBC #$80
	TAX
	
	LDA ZONE.START.BASE_ADDRESS			;this adjustment makes PASS2 start $80 bytes in. This step is normally performed in the loops but we're dong it here since this is a shoe horn entrance to PASS2. 
	CLC
	ADC #$80 
	STA ZONE.START.BASE_ADDRESS
	
	LDA #$00							;**OPT** Memory. This reset may no longer me needed because after a stop value is found, this variable is now automatically set. But, if a stop value isn't found, maybe it being reset here (or somewhere) is still needed. 
	STA XREG.RESUME
	
	JMP .INDEX.SEARCH_PASS2.LOOP		;resume search at PASS2 because the compressed tile record index ix >= $80. PASS2 starts at byte $80 of the current page of zone data. 
	

.INDEX.SEARCH_PASS1.LOOP.ENTRANCE
;SYNC COMPRESSED TILE RECORD INDEX
;(if needed)	
	LDA COUNTER.ADJ							
	BEQ .INDEX.SEARCH_PASS1.LOOP
	INX

	LDA #$00
	STA COUNTER.ADJ
	STA XREG.RESUME						;**OPT** Memory. This reset may no longer me needed because after a stop value is found, this variable is now automatically set. But, if a stop value isn't found, maybe it being reset here (or somewhere) is still needed.


.INDEX.SEARCH_PASS1.LOOP
@START	
;SEARCH CURRENT SECTOR FOR ZONE DATA STOP VALUE ($00)
	LDA MAP.DATA.PASS1,X					;LOAD BYTE OF ZONE DATA FROM CURRENT SECTOR
	CMP #$00								;IS CURRENT BYTE == STOP VALUE?					
	BNE .NEXT_BYTE.PASS1_STEP
		LDA MAP.DATA.PASS1+$2,X				;check the next tile_qty byte, to make sure the current byte #$00 doesn't represent !256 tiles. 
		CMP #$00							;IS CURRENT BYTE == STOP VALUE?					
		BNE .STOP_VALUE_FOUND.PASS1			;$00.xx.xx (starting with current byte) pattern found, this is a stop value	
											;$00.xx.$00 (starting with current byte) pattern found, either a stop value or pseudo value for !256 tiles. If current byte is a stop value, then next tile pair is !256 mountains, which has the pattern $00.$00.$00...so if the current byte is a stop value, the current byte plus the next 3 bytes would be $00.$00.$00.$00. Accordingly, we need to test $x+1, $x+2 (already done) and $x+3
			LDA MAP.DATA.PASS1+$1,X			;CHECK THE NEXT (1) TILE_QTY BYTE.
			CMP #$00						;IS CURRENT BYTE == STOP VALUE?					
			BNE .NEXT_BYTE.PASS1_STEP		;since x+$1, +$2, +$3 are not all $00, then current byte is not a stop value.  
			LDA MAP.DATA.PASS1+$3,X			;CHECK THE NEXT (3) TILE_QTY BYTE.
			CMP #$00						;IS CURRENT BYTE == STOP VALUE?					
			BNE .NEXT_BYTE.PASS1_STEP		;since x+$1, +$2, +$3 are not all $00, then current byte is not a stop value.  
			JMP .STOP_VALUE_FOUND.PASS1		;$00.$00.$00.$00 (starting with current byte) pattern found, this is a stop value followed by !256 mountain tiles

.NEXT_BYTE.PASS1_STEP
	JMP .NEXT_BYTE.PASS1
	
.STOP_VALUE_FOUND.PASS1
;RECORD START ADDRESS OF NEXT ZONE
;(NEXT ZONE START = AUX.READ_ADDRESS.START + XREG +1.)
;(the +1 is because the XREG (stop value) of the last zone is 1 byte less than the start address of the next zone)
	LDA ZONE.START.BASE_ADDRESS				;=AUX.READ_ADDRESS.START		
	CLC
	INX										;+1	
	STX TEMP
	ADC TEMP								;+XREG
	BNE .NOFLIP								;A flip can occur if XREG is $FF. The +$1 is because the next zone starts 1 byte after the address of the stop value of the current zone. 
	INC ZONE.START.BASE_ADDRESS+$1			;in the even of a flip we need to temporarily increment the HO byte of the base address. 
.NOFLIP
	STA ZONE.LOOKUP.LO,Y					;record start address of current zone
	STA	XREG.RESUME							;used to carryover x-reg value to start of next search loop if the next sector(s) have compression disabled. that would result in a load of the next page(s) of zone data, and we can't start searching for stop values at the beginning of the an uncompressed zone would end up somewhere in the middle. if the next zone has compression enabled then xreg.resume is discarded.
	DEX										;reverse the +1 to restore register to its original value at start of this code section
			
	LDA ZONE.START.BASE_ADDRESS+$1			;record start address of current zone
	STA ZONE.LOOKUP.HO,Y
			
	LDA TEMP
	BNE .NO.FLIP.OCCURED
	DEC ZONE.START.BASE_ADDRESS+$1			;reversing increment above because .EXIT_TEST.OUTERLOOP automatically does an increment to the aux read address HO byte which will be saved to the zone address at the top of the outerloop.		
.NO.FLIP.OCCURED
			
			; ;TROUBLESHOOTING HOOK			
			; CPY TEMP16
			; BNE .TEMP2
			; LDA TEXT
			; LDA $C082
			; ;LDY TOTAL.SECTORS
			; ;LDA AUX.READ_ADDRESS.START+$1
			; ;LDA NEW.MAP,X
			; ;LDA ZONE.LOOKUP.LO,Y
			; ;LDY TEMP
			; BRK
; .TEMP2 
	
	INY										;NEXT ZONE TABLE RECORD
	INC PRIOR.ZONE.INDEX						;= YREG -1. 

	CPY #WZONE.TOTAL.PLUSONE				;even if there are more sectors to read, exit if all zones have been processed. 
	BCS .EXIT_STEP
	JMP .CHECK.NEXT.ZONE
	
.EXIT_STEP
	JMP .EXIT
	
	
.CHECK.NEXT.ZONE
	STY SAVED.YREG.LOCAL					;save ZONE.LOOKUP.LO/HO index

;USE ZONE COMPRESSION FLAGS?
	LDA USE.COMPRESSION_FLAGS
	BEQ .NOT_COMPRESSED1		;are compression flags off ($00)? if yes, then zone is compressed. skip check of WZONE.COMPRESSION.FLAGS
								;if not, check WZONE.COMPRESSION.FLAGS to see if current zone is a zone that has compression ON or OFF
;IS NEXT ZONE ENABLED FOR COMPRESSION?
;(if not, no need to search for a stop value for this zone)
	LDY PRIOR.ZONE.INDEX					;= YREG -1. 		
	LDA WZONE.COMPRESSION.FLAGS, Y			;is next zone compressed? 
	BNE .NOT_COMPRESSED1					;if no, use normal exit
		;**I think the above might be backwards. BNE seems like it would indicate the zone IS compressed.
		
	LDY SAVED.YREG.LOCAL					;restore ZONE.LOOKUP.LO/HO index
	JMP .EXIT_TEST.OUTERLOOP				;the intention is to pass control to .ZONE.COMPRESSION.CHECK at the top of outerloop but the code goes through the exit_test so that the aux counters and page/sector counter get updated. 

	
.NOT_COMPRESSED1 ;normal exit	
	LDY SAVED.YREG.LOCAL						;restore ZONE.LOOKUP.LO/HO index	
	JMP .NEXT_BYTE.PASS1.ALTERNATE.ENTRANCE		;if a stop value is found, the tile record index should only be incremented once to get baack in sync with the next record, because the stop value is added to the end of a record. 

		
.NEXT_BYTE.PASS1
;EXIT TEST INDEX.SEARCH.LOOP
	INX										;increment loop (tile record) counter
	CPX #$80								;exit test 1 of 2. the exit test is done in two stages because sometimes the loop counter is incremented once, sometimes twice, and either could result in a flip to $00. 
	BNE .NEXT_BYTE.PASS1.ALTERNATE.ENTRANCE	;loop until the counter flips to $00
	LDA #$01								
	STA COUNTER.ADJ							;used to sync the compressed tile record index. Since the search loop is terminating with only one INX, that means the added bytes of the stop values have caused the tile_qty field to start on an odd byte instead of an even byte, which means it is out of sync. Without this adjustment, PASS2 would start with the index at $00, which would mean it would be reading in the tile_type field instead of the tile_qty field. This would be very bad since the stop values are located in the tile_qty field.  
	JMP .NEXT.PASS							;exit this loop (index.search.loop), proceed to exit test for outer loop
.NEXT_BYTE.PASS1.ALTERNATE.ENTRANCE			;if a stop value is found, the tile record index should only be incremented once to get baack in sync with the next record, because the stop value is added to the end of a record. 
	INX										;record is 2 bytes, so two inx, except for right after finding a stop value.  
	CPX #$80								;exit test 2 of 2. the exit test is done in two stages because sometimes the loop counter is incremented once, sometimes twice, and either could result in a flip to $00. 
	BEQ .NEXT.PASS 	
	JMP .INDEX.SEARCH_PASS1.LOOP		;loop until the counter flips to $00

@END
	
.NEXT.PASS
;CLOSE OUT LOOP COUNTER, ADVANCING BASE ADDRESS FOR RESUMED SEARCH  
;(this is so we can include 1 page + 3 bytes in the search, which exceeds an 8-bit counter)
	TXA	;should contain #$80
	CLC
	ADC ZONE.START.BASE_ADDRESS
	STA ZONE.START.BASE_ADDRESS

;UPDATE COUNTERS AND INDEXES	
	LDX #$00							;rest loop counter (tile compressed record index)

	LDA COUNTER.ADJ						;sync the tile compression record index, if needed.
	BEQ .INDEX.SEARCH_PASS2.LOOP
	INX
	
	LDA #$00
	STA COUNTER.ADJ

.INDEX.SEARCH_PASS2.LOOP
@START

;SEARCH CURRENT SECTOR FOR ZONE DATA STOP VALUE ($00)
	LDA MAP.DATA.PASS2,X					;LOAD BYTE OF ZONE DATA FROM CURRENT SECTOR
	CMP #$00								;IS CURRENT BYTE == STOP VALUE?					
	BNE .NEXT_BYTE.PASS2
		LDA MAP.DATA.PASS2+$2,X				;CHECK THE NEXT TILE_QTY BYTE, TO MAKE SURE THE CURRENT BYTE #$00 DOESN'T REPRESENT !256 TILES. 
		CMP #$00							;IS CURRENT BYTE == STOP VALUE?					
		BNE .STOP_VALUE_FOUND.PASS2			;$00.xx.xx (starting with current byte) pattern found, this is a stop value	
											;$00.xx.$00 (starting with current byte) pattern found, either a stop value or pseudo value for !256 tiles. If current byte is a stop value, then next tile pair is !256 mountains, which has the pattern $00.$00.$00...so if the current byte is a stop value, the current byte plus the next 3 bytes would be $00.$00.$00.$00. Accordingly, we need to test $x+1, $x+2 (already done) and $x+3
			LDA MAP.DATA.PASS2+$1,X			;CHECK THE NEXT (1) TILE_QTY BYTE.
			CMP #$00						;IS CURRENT BYTE == STOP VALUE?					
			BNE .NEXT_BYTE.PASS2			;since x+$1, +$2, +$3 are not all $00, then current byte is not a stop value.  
			LDA MAP.DATA.PASS2+$3,X			;CHECK THE NEXT (3) TILE_QTY BYTE.
			CMP #$00						;IS CURRENT BYTE == STOP VALUE?					
			BNE .NEXT_BYTE.PASS2			;since x+$1, +$2, +$3 are not all $00, then current byte is not a stop value.  
			;**FALLS THROUGH**				;$00.$00.$00.$00 (starting with current byte) pattern found, this is a stop value followed by !256 mountain tiles
.STOP_VALUE_FOUND.PASS2
;RECORD START ADDRESS OF NEXT ZONE
;(NEXT ZONE START = AUX.READ_ADDRESS.START + XREG +1.)
;(the +1 is because the XREG (stop value) of the last zone is 1 byte less than the start address of the next zone)

	
	LDA ZONE.START.BASE_ADDRESS				;=AUX.READ_ADDRESS.START		
	CLC
	INX										;+1	
	STX TEMP
	ADC TEMP								;+XREG+$1
	BNE .NOFLIP2								;A flip can occur if XREG is $FF. The +$1 is because the next zone starts 1 byte after the address of the stop value of the current zone. 
	INC ZONE.START.BASE_ADDRESS+$1			;in the even of a flip we need to temporarily increment the HO byte of the base address. 
.NOFLIP2
	STA ZONE.LOOKUP.LO,Y					;record current zone start address
	STA	XREG.RESUME							;used to carryover x-reg value to start of next search loop if the next sector(s) have compression disabled. that would result in a load of the next page(s) of zone data, and we can't start searching for stop values at the beginning of the an uncompressed zone would end up somewhere in the middle. if the next zone has compression enabled then xreg.resume is discarded.
	DEX										;REVERSE THE +1 TO RESTORE REGISTER TO ITS ORIGINAL VALUE AT START OF THIS CODE SECTION

			
	LDA ZONE.START.BASE_ADDRESS+$1			;record current zone start address
	STA ZONE.LOOKUP.HO,Y					

	LDA TEMP
	BNE .NO.FLIP.OCCURED2
	DEC ZONE.START.BASE_ADDRESS+$1			;reversing increment above because .EXIT_TEST.OUTERLOOP automatically does an increment to the aux read address HO byte which will be saved to the zone address at the top of the outerloop.		
.NO.FLIP.OCCURED2

			; ;TROUBLESHOOTING HOOK
			; CPY TEMP16
			; BNE .TEMP3
			; LDA TEXT
			; LDA $C082
			; LDA ZONE.START.BASE_ADDRESS
			; LDY TEMP
			; ;LDY TOTAL.SECTORS
			; ;LDA AUX.READ_ADDRESS.START+$1
			; ;LDA NEW.MAP,X
			; ;LDA PRIOR.ZONE.INDEX
			; BRK
; .TEMP3


	INY										;NEXT ZONE TABLE RECORD
	CPY #WZONE.TOTAL.PLUSONE				;even if there are more sectors to read, exit if all zones have been processed. 
	BEQ .EXIT


	INC PRIOR.ZONE.INDEX					;= YREG -1. 
	
	STY SAVED.YREG.LOCAL					;save ZONE.LOOKUP.LO/HO index

;USE ZONE COMPRESSION FLAGS?
	LDA USE.COMPRESSION_FLAGS
	BEQ .NOT_COMPRESSED2		;are compression flags off ($00)? if yes, then zone is compressed. skip check of WZONE.COMPRESSION.FLAGS
								;if not, check WZONE.COMPRESSION.FLAGS to see if current zone is a zone that has compression ON or OFF
	
;IS NEXT ZONE ENABLED FOR COMPRESSION?
;(if not, no need to search for a stop value for this zone)								
	LDY PRIOR.ZONE.INDEX					;= YREG -1. 		
	LDA WZONE.COMPRESSION.FLAGS, Y			;is next zone compression disabled?
	BNE .NOT_COMPRESSED2					;if no, use normal exit
		;**I think the above might be backwards. BNE seems like it would indicate the zone IS compressed.
	
	LDY SAVED.YREG.LOCAL					;restore ZONE.LOOKUP.LO/HO index
	JMP .EXIT_TEST.OUTERLOOP				;the intention is to pass control to .ZONE.COMPRESSION.CHECK at the top of outerloop but the code goes through the exit_test so that the aux counters and page/sector counter get updated. 

	
.NOT_COMPRESSED2 ; normal exit
	LDY SAVED.YREG.LOCAL						;restore ZONE.LOOKUP.LO/HO index
	JMP .NEXT_BYTE.PASS2.ALTERNATE.ENTRANCE		;if a stop value is found, the tile record index should only be incremented once to get baack in sync with the next record, because the stop value is added to the end of a record. 

		
		
.NEXT_BYTE.PASS2
;EXIT TEST INDEX.SEARCH.LOOP
	INX										;increment loop (tile record) counter
	CPX #$80								;exit test 1 of 2. the exit test is done in two stages because sometimes the loop counter is incremented once, sometimes twice, and either could result in a flip to $00. 
	BNE .NEXT_BYTE.PASS2.ALTERNATE.ENTRANCE	;loop until the counter flips to $00
	LDA #$01
	STA COUNTER.ADJ							;see the line for COUNTER.ADJ in .NEXT_BYTE.PASS1. I don't recall why we need it here a second time as after PASS2 it should just get reset, but there may be a reason I don't recall. 
	JMP .EXIT_TEST.OUTERLOOP				;exit this loop (index.search.loop), proceed to exit test for outer loop
.NEXT_BYTE.PASS2.ALTERNATE.ENTRANCE			;if a stop value is found, the tile record index should only be incremented once to get baack in sync with the next record, because the stop value is added to the end of a record. 
	INX										;record is 2 bytes, so two inx, except for right after finding a stop value.  
	CPX #$80								;exit test 2 of 2. the exit test is done in two stages because sometimes the loop counter is incremented once, sometimes twice, and either could result in a flip to $00. 
	BEQ .EXIT_TEST.OUTERLOOP
	JMP .INDEX.SEARCH_PASS2.LOOP			;loop until the counter flips to $00
	
@END
@END
	
	
.EXIT_TEST.OUTERLOOP


		
	INC AUX.READ_ADDRESS.START+$1		;increment aux move by 1 page/sector
	INC AUX.READ_ADDRESS.END

	LDX SAVED.XREG.LOCAL				;load outerloop counter (page/sector)
	
	INX									;increment zone/page counter
	CPX TOTAL.SECTORS					;the # of sectors of map data read in == the number of pages of aux memory to read
	BCS .EXIT

	
	CPY #WZONE.TOTAL.PLUSONE			;even if there are more sectors to read, exit if all zones have been processed. 
	BCS .EXIT
	
	JMP .OUTERLOOP
@END

.EXIT
	CPY #WZONE.TOTAL.PLUSONE			;even if there are more sectors to read, exit if all zones have been processed. 
	BCC .ERROR


			; ;;TROUBLESHOOTING HOOK
			; LDA TEXT
			; LDA $C082
			; ;LDY TOTAL.SECTORS
			; ; LDX #ZONE.LOOKUP.LO
			; ; LDY /ZONE.LOOKUP.LO	
			; ;LDA #$AA			
			; BRK	
	RTS

.ERROR
;ERROR REPORTED BY ZONE_TOOLS.BUILD.WZONE_HEADERS.
;SUBROUTINES TERMINATED BECAUSE THE NUMBER OF SECTORS 
;SPECIFIED BY TOTAL.SECTORS WERE ALL PROCESSED, BUT 
;ALL ZONES WERE NOT PROCSSED. 
	LDA TEXT
	LDA $C082
	BRK
	
@END

ZONE_TOOLS.UNCOMPRESS.SINGLE ;
@START
;PARAMETERS: WZONE.UNCOMPRESS.CURRENT(1)*1, USE.COMPRESSION_FLAGS
;RETURN VALUE: NONE
;ENTRANCE: DIRECT
;*1: THE WORLD ZONE # TO UNCOMRPESS
			

;=====================SUBROUTINE DOCUMENTATION====================================
;
;This subroutine copies the tile data in a world zone (parameter designated)
;from auxiliary memory to a main memory input buffer. 
;
;The location of the world zone in aux memory is identified using the
;ZONE.LOOKUP.LO/HO lookup table, which is populated by WORLD.COMPRESS2
;during game boot. 
;
;WZONE.COMPRESSION.FLAGS is checked to see if the tile data for the 
;current zone is compressed. If not it is copied as-is from the input buffer
;to ZONE_TOOLS.OUTPUT_BUFFER. If the tile data is compressed, then it 
;is uncompressed during the copy process to ZONE_TOOLS.OUTPUT_BUFFER.
;=================================================================================

			
;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA
	
			; LDX #ZONE.LOOKUP.HO
			; LDY /ZONE.LOOKUP.HO
			; LDA TEXT
			; BRK	
			
	LDX WZONE.UNCOMPRESS.CURRENT					;X-REG IS THE ZONE LOOKUP INDEX
	
				; LDA #$B2
				; STA ZONE.LOOKUP.LO+$33
				
				
				
;COPY COMPRESSED ZONE DATA FROM AUX MEMORY TO A BUFFER IN MAIN MEMORY (STILL COMPRESSED)
	CLC              							  	;SET CARRY FLAG DESGINATD MOVE FROM AUX -> MAIN MEMORY
	LDA ZONE.LOOKUP.LO,X							;START ADDRESS OF CURRENT ZONE
	STA AUX_MOVE.START
	LDA ZONE.LOOKUP.HO,X	
	STA AUX_MOVE.START+$1
	
	LDA ZONE.LOOKUP.LO+$1,X							;END ADDRESS OF AUX MOVE WILL BE START ADDRESS OF NEXT ZONE
	STA AUX_MOVE.END
	LDA ZONE.LOOKUP.HO+$1,X						
	STA AUX_MOVE.END+$1
	
	LDA #ZONE_TOOLS.INPUT_BUFFER					;SET DESTINATION ADDRESS
	STA AUX_MOVE.DEST
	LDA /ZONE_TOOLS.INPUT_BUFFER
	STA AUX_MOVE.DEST+$1

	
	JSR AUX_MOVE

			; STA TEMP
			; LDA WZONE.UNCOMPRESS.CURRENT
			; CMP #$1B
			; BNE .TEMP2
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP2
			; ; LDA ZONE.LOOKUP.HO,X
			; ; TAY
			; ; LDA ZONE.LOOKUP.LO,X
			; ; TAX
			; ; LDX ZONE.LOOKUP.LO+$33			
			; ; LDY ZONE.LOOKUP.HO+$33
			; LDX #ZONE_TOOLS.INPUT_BUFFER
			; LDY /ZONE_TOOLS.INPUT_BUFFER
			
			; JMP FULL.BRK
			; BRK
; .TEMP2	


			; LDA ZONE.LOOKUP.HO,X
			; TAY
			; LDA ZONE.LOOKUP.LO,X
			; TAX

			; LDA TEXT
			; LDA #ZONE.LOOKUP.HO
			; LDY /ZONE.LOOKUP.HO
			; BRK
	
;USE ZONE COMPRESSION FLAGS?
	LDA USE.COMPRESSION_FLAGS
	BEQ .INIT					;are compression flags off ($00)? if yes, then proceed with uncompress routine skip check of WZONE.COMPRESSION.FLAGS
								;if not, check WZONE.COMPRESSION.FLAGS to see if WZONE.UNCOMPRESS.CURRENT is a zone that has compression ON or OFF
;IS ZONE DATA COMPRESSED?
	LDA WZONE.COMPRESSION.FLAGS,X
	CMP #$01										;#$01 = COMPRESSION ON
	BNE .ZONE.ALREADY.UNCOMPRESSED

.INIT
;INIT COUNTERS AND INDEXES
	LDY #$00									;INIT INPUT INDEX
	STY ZONE_TOOLS.OUTPUT.INDEX					;INIT OUTPUT INDEX
	
	LDX #$00									;INIT TILE_QTY COUNTER (FOR ADJACENT TILES OF THE SAME TYPE)
	

;MANUALLY (OUTSITE OF LOOP) EXAMINE FIRST TILE_PAIR TO CHECK FOR #$00
	LDA	ZONE_TOOLS.INPUT_BUFFER,Y				;LOAD NEXT TILE_QTY FROM ZONE DATA

	
			
			
	TAX
	BEQ .PSEUDO_VALUE.FOUND						;#$00 IN TILE_QTY IS EITHER A PSEUDO VALUE FOR !256 TILES (IF IN FIRST TILE PAIR, OR, OTHERWISE IT'S THE ARRAY STOP VALUE)
	INY											;NEXT FIELD
	LDA ZONE_TOOLS.INPUT_BUFFER,Y				;LOAD NEXT TILE_TYPE FROM ZONE DATA	
	JMP	.INITIAL_ENTRANCE						;ENTER LOOP VIA SPECIAL ENTRANCE POINT FOR FIRST ITERATION
	
.LOOP.LOAD.TILE_PAIR ;		
	LDA	ZONE_TOOLS.INPUT_BUFFER,Y				;LOAD NEXT TILE_QTY FROM ZONE DATA
	TAX

	BEQ .STOP_VALUE.FOUND						;#$00 IN TILE_QTY IS EITHER S PSEUDO VALUE FOR !256 TILES (IF IN FIRST TILE PAIR, OR, OTHERWISE IT'S THE ARRAY STOP VALUE)
	INY											;NEXT FIELD
	LDA ZONE_TOOLS.INPUT_BUFFER,Y				;LOAD NEXT TILE_TYPE FROM ZONE DATA	

.INITIAL_ENTRANCE	
	STY	ZONE_TOOLS.INPUT.INDEX
	LDY ZONE_TOOLS.OUTPUT.INDEX
	

.LOOP.WRITE_TILE ;	
	STA ZONE_TOOLS.OUTPUT_BUFFER,Y 
				
	INY											;INCREMENT OUTPUT INDEX

	DEX											;DECREMENT TILE_QTY COUNTER
	BEQ .NEXT_TILE_PAIR  						;IF TILE_QTY == #$00 THEN WE'RE READY FOR THE NEXT COMPRESSED TILE_PAIR
	JMP .LOOP.WRITE_TILE 						;ELSE, CONTINUE WRITE LOOP WITH THE SAME TILE_TYPE
	
.NEXT_TILE_PAIR ;
	STY ZONE_TOOLS.OUTPUT.INDEX
	LDY ZONE_TOOLS.INPUT.INDEX
	INY											;INCREMENT INPUT INDEX (NEXT TILE PAIR)
	JMP .LOOP.LOAD.TILE_PAIR
	
.PSEUDO_VALUE.FOUND	; 
;#$00 = !256 TILES 
;WRITE TILE 1/256 TO OUTPUT ARRAY
	INY
	LDA ZONE_TOOLS.INPUT_BUFFER,Y				;LOAD TILE_TYPE FROM CURRENT TILE_PAIR
	STY	ZONE_TOOLS.INPUT.INDEX					;SETUP INPUT INDEX FOR NEXT TILE PAIR RECORD, SINCE WE'LL NEED TO VERIFY THE STOP VALUE AFTER THE !256 TILES ARE WRITTEN
	LDY ZONE_TOOLS.OUTPUT.INDEX
	
	STA ZONE_TOOLS.OUTPUT_BUFFER,Y						
	INY											;INCREMENT OUTPUT INDEX

;WRITE TILES 2-256 TO OUTPUT ARRAY	
	LDX #$FF
	JMP .LOOP.WRITE_TILE
		
.STOP_VALUE.FOUND ;
	JMP .EXIT
	
	
.ZONE.ALREADY.UNCOMPRESSED
	LDY #$00

.LOOP	
	LDA ZONE_TOOLS.INPUT_BUFFER,Y				;THIS IS THE TEMPORARY LOCATION IN MAIN MEMORY WHERE ZONE DATA IN AUX MEMORY IS COPIED TO
	STA ZONE_TOOLS.OUTPUT_BUFFER,Y
	INY
	BNE .LOOP
	;**FALLS THROUGH
		
.EXIT


			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; LDA WZONE.UNCOMPRESS.CURRENT
			; CMP #$32	
			; BNE .TEMP
			; LDA TEXT
			; LDA $C082
			; LDX ZONE.LOOKUP.LO+$32
			; LDY ZONE.LOOKUP.HO+$32
			; BRK
; .TEMP
	

;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX
	
	RTS
	

.ERROR
;ZONE_TOOLS.UNCOMPRESS REPORTS THAT OUTPUT COUNTER FLIPPED TO #$00 IN EITHER .LOOP.WRITE_TILE OR .SWITCH_VALUE.FOUND
;
;THE UNCOMPRESSED OUTPUT DATA IS NEVER MORE THAN 1 PAGE LONG, SO IF THE OUTPUT COUNTER FLIPS TO #$00, SOMETHING IS WRONG. 
;
;DISABLE.BS_RAM
	LDA $C082				;READ ENABLE ROM, DISABLE WRITE ON BSM (NORMAL STATE).
							;REQUIRED FOR BEFORE BRK BECAUSE APPLE MONITOR IS A ROM ROUTINE

	LDA TEXT
	BRK
	
@END
	
ZONE_TOOLS.RCOPY ; ===COPY ZONE DATA TO REGIONAL MAP=====			
@START			
;PARAMETERS; RZONE.UNCOMPRESS.CURRENT (THE REGIONAL ZONE # TO COPY TO)
;ENTRANCE: REGION.UNCOMPRESS.ALL, REGION.UNCOMPRESS.NORTH/SOUTH/EAST/WEST
;RETURN: NONE

;=====================SUBROUTINE DOCUMENTATION====================================
;
;This subroutine copies the uncompressed world zone data from the output buffer into the regional
;zone array (RZONE.ARRAY)
;
;============================================================================================


		
;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA
	
;SET RZONE ARRAY START/STOP VALUES
	LDX RZONE.UNCOMPRESS.CURRENT

	
;INIT ROW INDEX TO THE SUM OF THE BASE ADDRESS OF THE RZONE ARRAY AND THE STARTING ELEMENT OF THE ARRAY FOR THE CURRENT ZONE	
;=======INLINE CODE FOR ADC.16========	
;RZONE.ARRAY(2) RZONE.LOOKUP.ARRAY_START.LO/HO(2)
	
	CLC                          			;ALWAYS BEFORE ADD
	LDA #RZONE.ARRAY						;OP1
	ADC RZONE.LOOKUP.ARRAY_START.LO,X		;OP2 
	STA RZONE.ARRAY.INDEX_ROW
				 
	LDA /RZONE.ARRAY						;OP1+$1
	ADC RZONE.LOOKUP.ARRAY_START.HO,X		;OP2+$2 carry flag not cleared via CLC intentionally, it's part of 16-bit adding. 
	STA RZONE.ARRAY.INDEX_ROW+$1

;======================================	
		

;=======INLINE CODE FOR ADC.16========	
;RZONE.ARRAY(2) RZONE.LOOKUP.ARRAY_STOP.LO/HO(2)
	
	CLC                          			;ALWAYS BEFORE ADD
	LDA #RZONE.ARRAY						;OP1
	ADC RZONE.LOOKUP.ARRAY_STOP.LO,X		;OP2  
	STA RZONE.ARRAY.STOP
				 
	LDA /RZONE.ARRAY						;OP1+$1
	ADC RZONE.LOOKUP.ARRAY_STOP.HO,X		;OP2+$2 carry flag not cleared via CLC intentionally, it's part of 16-bit adding. 
	STA RZONE.ARRAY.STOP+$1

;======================================	
			
;INIT INDEXES	
	LDX #$00								;OUTPUT BUFFER INDEX
	LDY #$00								;REGIONAL ZONE ARRAY COLUMN INDEX
		
.LOOP_ARRAY
	LDA ZONE_TOOLS.OUTPUT_BUFFER,X
	STA (RZONE.ARRAY.INDEX_ROW),Y
	INX 									;INCREMENT OUTPUT BUFFER INDEX
	INY 									;INCREMENT COLUMN INDEX
	CPY #$10 								;LAST COLUMN? 
	BNE .LOOP_ARRAY

;	
;LAST ROW?
		
		; CPX #$20
		; BNE .TEMP
		; LDX RZONE.ARRAY.INDEX_ROW
		; LDY RZONE.ARRAY.INDEX_ROW+$1
		; LDA TEXT
		; BRK
; .TEMP

;=================INLINE CODE FOR CMP.16================
;RZONE.ARRAY.INDEX_ROW(2), RZONE.ARRAY.STOP(2)
	
	LDA RZONE.ARRAY.INDEX_ROW				;OP1
	CMP RZONE.ARRAY.STOP					;OP2
	BNE .NE
	LDA RZONE.ARRAY.INDEX_ROW+$1			;OP1+$1
	CMP RZONE.ARRAY.STOP+$1					;OP2+$1
	BNE .NE
	JMP .EQUALS
.NE	;the 16-bit values are not equal
		;INCREMENT ROW INDEX
		;=======INLINE CODE FOR ADC.16========	
		;RZONE.ARRAY.INDEX_ROW(2) #RZONE.ARRAY.OFFSET(1)
		
			CLC                          			;ALWAYS BEFORE ADD
			LDA RZONE.ARRAY.INDEX_ROW				;OP1
			ADC #RZONE.ARRAY.OFFSET					;OP2  (# OF TILES BETWEEN ROWS IN RZONE ARRAY)
			STA RZONE.ARRAY.INDEX_ROW
				 
			LDA RZONE.ARRAY.INDEX_ROW+$1			;OP1+$1
			ADC #$00								;OP2+$2 carry flag not cleared via CLC intentionally, it's part of 16-bit adding. 
			STA RZONE.ARRAY.INDEX_ROW+$1

		;======================================	
		LDY #$00									;RESET COLUMN INDEX TO $00
		JMP .LOOP_ARRAY
.EQUALS
;========================================================
		
;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX

	
		; ;PLA
		; PLA
		; TAX
		; PLA
		; TAY
		; LDA #$AA
		; ;JMP FULL.BRK
		; JMP $E15E


			
	RTS
	
@END

ZONE_TOOLS.SCROLL_COLUMN.COPY  ;=====SCROLL ONE ZONE TO ANOTHER====
@START
;PARAMETERS: RZONE.ARRAY.INDEX.SCROLL_FROM, RZONE.ARRAY.INDEX.SCROLL_TO
;ENTRANCE: VIA ZONE_TOOLS.TRANSITION.NORTH/SOUTH/EAST/WEST
;RETURN: NONE

;=====================SUBROUTINE DOCUMENTATION====================================
;
;Scrolls a column of regional zone data. Same concept as SCROLL.COLUMNS
;in Graphics_Scrolling.ASM. See the subroutine docs for that routine for
;more details.
;
;=================================================================================


;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA
			
;INIT INDEXES	
	LDX #$00								;ZONE ELEMENT COUNTER
	LDY #$00								;REGIONAL ZONE ARRAY COLUMN INDEX
	
		
.LOOP_ARRAY
	LDA (RZONE.ARRAY.INDEX.SCROLL_FROM),Y
	STA (RZONE.ARRAY.INDEX.SCROLL_TO),Y
	INY 									;INCREMENT COLUMN INDEX
	INX										;INCREMENT ZONE ELEMENT INDEX
	CPY #$10 								;LAST COLUMN? 
	BNE .LOOP_ARRAY

;NEXT COLUMN

;
;INCREMENT INDEXES
;
	;INCREMENT FROM INDEX
	;=======INLINE CODE FOR ADC.16========	
	;RZONE.ARRAY.INDEX_ROW(2) #RZONE.ARRAY.OFFSET(1)
	
		CLC                          			;ALWAYS BEFORE ADD
		LDA RZONE.ARRAY.INDEX.SCROLL_FROM		;OP1
		ADC #RZONE.ARRAY.OFFSET					;OP2  (# OF TILES BETWEEN ROWS IN RZONE ARRAY)
		STA RZONE.ARRAY.INDEX.SCROLL_FROM
			 
		LDA RZONE.ARRAY.INDEX.SCROLL_FROM+$1	;OP1+$1
		ADC /RZONE.ARRAY.OFFSET					;OP2+$2 carry flag not cleared via CLC intentionally, it's part of 16-bit adding. 
		STA RZONE.ARRAY.INDEX.SCROLL_FROM+$1

	;======================================	
	;INCREMENT TO INDEX
	;=======INLINE CODE FOR ADC.16========	
	;RZONE.ARRAY.INDEX_ROW(2) #RZONE.ARRAY.OFFSET(1)
	
		CLC                          			;ALWAYS BEFORE ADD
		LDA RZONE.ARRAY.INDEX.SCROLL_TO			;OP1
		ADC #RZONE.ARRAY.OFFSET					;OP2  (# OF TILES BETWEEN ROWS IN RZONE ARRAY)
		STA RZONE.ARRAY.INDEX.SCROLL_TO
			 
		LDA RZONE.ARRAY.INDEX.SCROLL_TO+$1	;OP1+$1
		ADC /RZONE.ARRAY.OFFSET					;OP2+$2 carry flag not cleared via CLC intentionally, it's part of 16-bit adding. 
		STA RZONE.ARRAY.INDEX.SCROLL_TO+$1

	;======================================	


	LDY #$00								;RESET COLUMN INDEX TO $00 TO PREPARE FOR NEXT COLUMN 
	CPX #$00								;LAST ROW? WHEN THE X-REG COUNTER FLIPS OVER THE END OF THE ZONE IS REACHED
	BNE .LOOP_ARRAY							


	
		
;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX

	RTS
@END	

ZONE_TOOLS.SCROLL_ROW.COPY ; =====SCROLL ONE ZONE TO ANOTHER====
@START
;PARAMETERS: RZONE.ARRAY.INDEX.SCROLL_FROM, RZONE.ARRAY.INDEX.SCROLL_TO
;ENTRANCE: VIA ZONE_TOOLS.TRANSITION.NORTH/SOUTH/EAST/WEST

;=====================SUBROUTINE DOCUMENTATION====================================
;
;Scrolls a column of regional zone data. Same concept as SCROLL.ROWS
;in Graphics_Scrolling.ASM. See the subroutine docs for that routine for
;more details.
;
;=================================================================================


;SAVE REGISTERS
	TXA
	PHA 
	TYA
	PHA
		
;INIT INDEXES	
	LDX #$00								;ZONE COUNTER
	LDY #$00								;REGIONAL ZONE ARRAY INDEX

	
.LOOP_COPY
;COPY ZONE DATA		
	LDA (RZONE.ARRAY.INDEX.SCROLL_FROM),Y	
	STA (RZONE.ARRAY.INDEX.SCROLL_TO),Y


	
	INY 									;INCREMENT RZONE INDEX
	BNE .LOOP_COPY							;SINCE WE'RE COPYING 3 ZONES, THE RZONE INDEX WILL FLIP OVER BEFORE THE COPY IS DONE

	INC RZONE.ARRAY.INDEX.SCROLL_FROM+$1	;ON EACH FLIP OF THE INDEX, INCREMENT THE HO BYTE OF THE BASE ADDRESS
	INC RZONE.ARRAY.INDEX.SCROLL_TO+$1
	
	INX 									;INCREMENT ZONE COUNTER
	CPX #$03								;LAST ZONE?
	BNE .LOOP_COPY

;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX
	
	RTS
@END

@END

;==========================MOTH-BALLED=========================
@START
; WORLD.COMPRESS ;		======COMPRESS THE DATA FOR ALL ZONES AND STORE IN AUX MEMORY=====
@START

;=====================SUBROUTINE DOCUMENTATION====================================
;
;This subroutine is only used during game boot. See WORLD.COMPRESS2 in LOADER.BIN (GAME_LOOP.ASM)
;
;=================================================================================

; ;;SAVE REGISTERS
	; ;; TXA
	; ;; PHA

	; TYA
	; PHA
	
	
; ;;PSEUDO CODE
; ;;
; ;;load first input zone base address (from label, WORLD.ZONE0)
	; ;;save it to a ho/lo variable (input base address) WORLD.COMPRESS.ZONE_INPUT.ADDRESS
	; ;;save to zone tools input (zero page) ZONE_TOOLS.INPUT
; ;;load aux memory start address for world map zone data (from constant)	#WORLD.COMPRESS.AUX_MEMORY.START_LO/HO
	; ;;save it to a ho/lo variable (output base address)	WORLD.COMPRESS.ZONE_OUTPUT.ADDRESS
	; ;;save to lookup table ho/lo ZONE.LOOKUP.HO, ZONE.LOOKUP.LO	
	
; ;;loop
	; ;;jsr compress.single
	; ;;add return value in acc, add to output base address WORLD.COMPRESS.ZONE_OUTPUT.ADDRESS
		; ;;save to lookup table ho/lo ZONE.LOOKUP.HO, ZONE.LOOKUP.LO	
	; ;;increment input base address/HO (zones are on page boundaries)	WORLD.COMPRESS.ZONE_INPUT.ADDRESS
		; ;;;save to zone tools input (zero page)	ZONE_TOOLS.INPUT
	; ;do loop
		
		

; ;;INIT VARIABLES
	; LDY #$00											;ZONE LOOKUP TABLE INDEX

; ;;INPUT: load first input zone base address
	; LDA #WORLD.ZONE0
	; STA WORLD.COMPRESS.ZONE_INPUT.ADDRESS				;THIS WILL BE THE INPUT BASE ADDRESS COUNTER
	; STA ZONE_TOOLS.INPUT								;SAVE TO INPUT POINTER FOR COMPRESSION ROUTINE	

	; LDA /WORLD.ZONE0
	; STA WORLD.COMPRESS.ZONE_INPUT.ADDRESS+$1
	; STA ZONE_TOOLS.INPUT+$1


; ;;OUTPUT: load aux memory start address for world map zone data
	; LDA #WORLD.COMPRESS.AUX_MEMORY.START_LO
	; STA WORLD.COMPRESS.ZONE_OUTPUT.ADDRESS				;THIS WILL BE THE OUTPUT BASE ADDRESS COUNTER
	; STA ZONE.LOOKUP.LO,Y								;SAVE FIRST ZONE'S ADDRESS TO LOOKUP TABLE, THE LOOP WILL DO THE REST OF THE ZONES
	
	; LDA #WORLD.COMPRESS.AUX_MEMORY.START_HO
	; STA WORLD.COMPRESS.ZONE_OUTPUT.ADDRESS+$1	
	; STA ZONE.LOOKUP.HO,Y
	

; .LOOP.COMPRESS
	; JSR ZONE_TOOLS.COMPRESS.SINGLE		
	; INY													;INCREMENT LOOKUP TABLE INDEX
		
	; ;;INCREMENT OUTPUT BASE ADDRESS
; ;;(this will be the location on aux memory that the compressed data for the next zone will be written)
	; STA OP1												;ACC IS RETURN VALUE FROM JSR ZONE_TOOLS.COMPRESS.SINGLE. IT CONTAINS THE OUTPUT ARRAY SIZE (LAST ELEMENT DATA WAS WRITTNE TO)
	; LDA #$00
	; STA OP1+$1
	; LDA WORLD.COMPRESS.ZONE_OUTPUT.ADDRESS
	; STA OP2
	; LDA WORLD.COMPRESS.ZONE_OUTPUT.ADDRESS+$1
	; STA OP2+$1
	
	; JSR ADC.16
	
	; LDA RESULT
	; STA WORLD.COMPRESS.ZONE_OUTPUT.ADDRESS
	; STA ZONE.LOOKUP.LO,Y
	
	; LDA RESULT+$1
	; STA WORLD.COMPRESS.ZONE_OUTPUT.ADDRESS+$1
	; STA ZONE.LOOKUP.HO,Y

; ;;INCREMENT INPUT BASE ADDRESS
; ;;(This will be the address of the uncompressed data for the next zone)

	; INC WORLD.COMPRESS.ZONE_INPUT.ADDRESS+$1			;THE UNCOMPRESSED ZONE DATA BEINGS ON PAGE BOUNDARIES. 
	; LDA WORLD.COMPRESS.ZONE_INPUT.ADDRESS+$1
	; STA ZONE_TOOLS.INPUT+$1								;UPDATE THE INPUT POINTER FOR COMPRESSION ROUTINE
				
; ;;EXIT TEST
	; CPY #WZONE.TOTAL									;HAS LAST ZONE BEEN COMPRESSED?
	; BNE .LOOP.COMPRESS									;IF NO, CONTINUE LOOP
			
; ;;RESTORE REGISTERS
	; PLA
	; TAY
		
	; RTS													
@END
		
; ZONE_TOOLS.COMPRESS.SINGLE ; ===COMPRESS ZONE AND COPY TO AUX MEMORY===
@START
; ;;PARAMETERS: ZONE_TOOLS.INPUT(2)*1, WORLD.COMPRESS.ZONE_OUTPUT.ADDRESS(*2)
; ;;RETURN VALUE: ACC (the output array index to the last element written+1)
; ;;ENTRANCE: DIRECT

; ;;*1: THE HO/LO ADDRESS OF THE UNCOMPRESSED ZONE INPUT ARRAY
; ;;*2: THE AUX MEMORY HO/LO ADDRESS TO WRITE THE COMPRESSED DATA TO

;=====================SUBROUTINE DOCUMENTATION====================================
;
;This subroutine is only used during game boot. See ZONE_TOOLS.COMPRESS.SINGLE in LOADER.BIN (GAME_LOOP.ASM)
;
;
;The compressed record format is:
;Tile_Qty, Tile_Type
;
;If Tile_QTY = $00, that stands for !256 tiles. 
;=================================================================================


; ;;SAVE REGISTERS
	; TXA
	; PHA
	; TYA
	; PHA


; ;;INIT COUNTERS AND INDEXES
	; LDY #$00									;INIT INPUT INDEX
	; STY ZONE_TOOLS.OUTPUT.INDEX					;INIT OUTPUT INDEX

	; LDX #$00									;INIT TILE_QTY COUNTER (FOR ADJACENT TILES OF THE SAME TYPE)

	
; ;;LOAD FIRST TILE
	; LDA (ZONE_TOOLS.INPUT),Y
	; STA ZONE_TOOLS.TILE_LAST	
	; INY											;INCREMENT INPUT INDEX
	; INX											;INCREMENT TILE_QTY COUNTER
; .LOOP.LOAD_TILE	
	; LDA (ZONE_TOOLS.INPUT),Y					;LOAD NEXT TILE_TYPE FROM ZONE DATA	
	; CMP ZONE_TOOLS.TILE_LAST						;LAST TILE_TYPE TO BE LOADED FROM ZONE DATA
	; BNE .SAVE.TILE_PAIR

	; INX											;INCREMENT TILE_QTY COUNTER
	; INY											;INCREMENT INPUT INDEX	
	; BNE .LOOP.LOAD_TILE							;UNLESS INPUT INDEX HAS FLIPPED TO #$00, CONTINUE THE LOAD LOOP
	; ;;END OF INPUT ARRAY REACHED

	; ;;**FALLS THROUGH

; .SAVE.TILE_PAIR
	; STA SAVED.ACC.LOCAL							;SAVE CURRENT TILE_TYPE FOR FUTURE USE

; ;;SAVE AND LOAD INDEX
	; STY ZONE_TOOLS.INPUT.INDEX
	; LDY ZONE_TOOLS.OUTPUT.INDEX

; ;;SAVE TILE_PAIR TO COMRPESSED OUTPUT ARRAY
	; TXA	
	; STA ZONE_TOOLS.OUTPUT_BUFFER,Y				;SAVE TILE_QTY
	; INY											;INCREMENT OUTPUT COUNTER
	; LDA ZONE_TOOLS.TILE_LAST
	; STA ZONE_TOOLS.OUTPUT_BUFFER,Y				;SAVE TILE_TYPE
	; INY											;INCREMENT OUTPUT COUNTER FORWARD TO THE NEXT TILE PAIR	
; ;;	BEQ .ERROR									;IF OUTPUT COUNTER FLIPS TO $00, GENERATE ERROR
	; STY ZONE_TOOLS.OUTPUT.INDEX
	; LDY ZONE_TOOLS.INPUT.INDEX
	; BEQ .EXIT									;EXIT IF THIS WAS LAST TILE
	
; ;;RESTORE/INCREMENT COUNTERS AND CURRENT TILE	
	; LDA SAVED.ACC.LOCAL							;RESTORE CURRENT TILE_TYPE
	; STA ZONE_TOOLS.TILE_LAST
	; LDX #$01									;RESET TILE_QTY COUNTER, TO #$01 (REFLECTING CURRENT TILE IN ACC)

	; INY
	; BNE .LOOP.LOAD_TILE 						;AT END OF ZONE_TOOLS ARRAY? (IT'S $100, SO WE'RE CHECKING FOR A FLIP TO $00)
	; JMP .SAVE.TILE_PAIR
		
; .EXIT

; ;;WRITE STOP VALUE TO END OF OUTPUT ARRAY
	; LDY ZONE_TOOLS.OUTPUT.INDEX
	; LDA #$00
	; STA ZONE_TOOLS.OUTPUT_BUFFER,Y

					
				
	
; ;;COPY OUTPUT ARRAY TO AUX MEMORY

; ;;IDENTIFY OUTPUT BUFFER START/END ADDRESS
	; LDA #ZONE_TOOLS.OUTPUT_BUFFER
	; STA WORLD.COMPRESS.BUFFER_ADDRESS.START
	; STA OP1
	; LDA /ZONE_TOOLS.OUTPUT_BUFFER
	; STA WORLD.COMPRESS.BUFFER_ADDRESS.START+$1
	; STA OP1+$1

; ;;THE OUTPUT INDEX/COUNTER IS THE OFFSET TO CALC THE END ADDRESS	
	; LDA ZONE_TOOLS.OUTPUT.INDEX
	; STA OP2
	; LDA #$00
	; STA OP2+$1
	
	; JSR ADC.16										;WORLD.COMPRESS.BUFFER_ADDRESS(2) + ZONE_TOOLS.OUTPUT_BUFFER.INDEX(1) 

; ;;SAVE OUTPUT BUFFER END ADDRES
	; LDA RESULT
	; STA WORLD.COMPRESS.BUFFER_ADDRESS.END
	; LDA RESULT+$1
	; STA WORLD.COMPRESS.BUFFER_ADDRESS.END+$1

; ;;AUX MOVE
	; SEC               							  	;SET CARRY FLAG DESGINATD MOVE FROM MAIN MEMORY -> AUX
	; LDA WORLD.COMPRESS.BUFFER_ADDRESS.START			;SET START ADDRESS
	; STA AUX_MOVE.START
	; LDA WORLD.COMPRESS.BUFFER_ADDRESS.START+$1		
	; STA AUX_MOVE.START+$1
	
	; LDA WORLD.COMPRESS.BUFFER_ADDRESS.END			;SET END ADDRESS
	; STA AUX_MOVE.END
	; LDA WORLD.COMPRESS.BUFFER_ADDRESS.END+$1
	; STA AUX_MOVE.END+$1
	
	; LDA WORLD.COMPRESS.ZONE_OUTPUT.ADDRESS			;SET DESTINATION ADDRESS
	; STA AUX_MOVE.DEST
	; LDA WORLD.COMPRESS.ZONE_OUTPUT.ADDRESS+$1
	; STA AUX_MOVE.DEST+$1

	; JSR AUX_MOVE

; ;;RESTORE REGISTERS
	; PLA
	; TAY
	; PLA
	; TAX

; ;;SETUP RETURN VALUE IN ACC
	; INC ZONE_TOOLS.OUTPUT.INDEX						;INCREMENT SO THAT IT BECOMES THE LAST ARRAY ELEMENT WRITTEN +1
	; LDA ZONE_TOOLS.OUTPUT.INDEX						;THE INDEX IS THE LAST ARRAY ELEMENT WRITTEN+1
			
	; RTS

; .ERROR
; ; ;ZONE_TOOLS.COMPRESS REPORTS THAT OUTPUT COUNTER FLIPPED TO $00 IN .SAVE.TILE_PAIR. 
; ; ;
; ; ;THIS IMPLIES THAT THE TILES WERE SO DISSIMULAR (HORITZONALLY) THAT THE COMRPESSED DATA TOOK MORE 
; ; ;BYTES THAN THE UNCOMPRESSED DATA. IF THAT OCCURS, I HAVEN'T THROUGHT OF A WAY TO HANDLE IT OTHER 
; ; ;THAN TO MODIFY THE MAP DATA SO COMPRESSION TAKES LESS BYTES. 
	; BRK

@END	
@END