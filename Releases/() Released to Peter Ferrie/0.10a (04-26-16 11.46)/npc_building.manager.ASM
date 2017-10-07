;************************INCLUDE FILE*****************************
;(do not assemble stand-alone. For a stand-alone version of this routine see examples/rwts2)


; *NPC.PATHGENERATOR
            ; *called from game loop. at start of subroutine do a check for in building
            ; *setup remaining NPC arrays, including schedule, using dummy data
            ; *probably start by having routine generate a path for all entries in schedule, then
             ; phase in time controls, verification only one path is generated until next schedule change
           ; *path stored in memory should probably have a header of the hour/minute (24 hour) of schedule entry. keep in mind the index to last element is the 1st byte of the header

;*To-Do List
 


NPC.PATHGENERATOR.INIT
;SAVE REGISTERS
	TXA
	PHA
	
	LDX #$00
.LOOP.INIT1
	LDA #$00
	STA SAVED.PATH.LOOKUP.TABLE,X
	INX
	CPX #SAVED.PATH.LOOKUP.TABLE.SIZE
	BNE .LOOP.INIT1
	
	LDX #$00
.LOOP.INIT2
	LDA #$FF
	STA NPC.PATHGENERATOR.QUE,X
	INX
	CPX #NPC.PATHGENERATOR.QUE.SIZE
	BNE .LOOP.INIT2
	

	
	
	
;RESTORE REGISTERS
	PLA
	TAX
 	
	RTS
	
NPC.SCHEDULER
@START
;*at the change of each hour or other interval (i.e. 15 minutes),
;iterate NPC.SCHEDULE, for each match to current time set the
;Active Anchor on each NPC and set in-transit to $01, init path 
;index to $00


SCHEDULE.TABLE.IDENTIFY.NEXT_PATHS

;MAP_OBJECTS.NPC Datagram
;Byte 0	Byte 1	Byte 2		Byte 3		Byte 4			Byte 5			Byte 6				Byte 7
;GMAP.X	GMAP.Y	Tile_type	Set to $03	Active Anchor	Path Index($00)	In Transit? ($00)	at-anchor move routine flag
;see Map Objects spreadsheet for authoratative version

; NPC.SCHEDULE Datagram								
; Byte 0	Byte 1			Byte 2				Byte 3				Byte 4			Byte 5		Byte 6		Byte 7							X+$0
; Anchor #	NPC Record #	Start Time: Hour	Start Time: Minute	End Time: Hour	X ofAnchor	Y ofAnchor	at-anchor move routine flag		Stop value
;see Map Objects spreadsheet for authoratative version


;**I THINK IF THE AUTO-ABORT FLAG IS SET, THIS SECTION 
;SHOULD BE SKIPPED....UPDATE: I DON'T THINK THIS APPLIES BECAUSE
;THIS CODE IS SPLIT OFF FROM NPC.PATHGENERATOR.


	LDX #$00				;NPC.SCHEDULE index
	LDY #$00				;NPC.PATHGENERATOR.QUE index
LOOP.IDENTIFY.NEXT_PATHS
	LDA TIME.CURRENT.HOUR
	CMP #PATHGENERATOR.LOOK_AHEAD.DAY_TRANSITION.HOUR	;is hour late enough that the early hours after midnight fall within the looking ahead period? (i.e. if the look ahead period is 3 hours, and it is 23:00, then 01:00 is within the look ahead period even though 01 is less than 23. For this reason a special routine is required to evalute whether the scheduler entry is in the past or in the future)
	BCS .DAY.TRANSITION


	LDA NPC.SCHEDULE+$2,X
	CMP TIME.CURRENT.HOUR			;is schedule record for future or past?
	BCC NEXT.SCHEDULE.RECORD		;if past, next record
	BEQ .CHECK.MINUTE				;if not sure becase hours match, check minutes
	;SCHEDULE RECORD IS IN FUTURE
	

		
	LDA NPC.SCHEDULE+$2,X			;load hour of schedule record
	SEC
	SBC TIME.CURRENT.HOUR
	CMP #PATHGENERATOR.LOOK_AHEAD.HOURS ;is schedule record within the look ahead period for which NPC.PATHGENERATOR calculates paths in advance?
	BCC EXAMINE.SCHEDULE.RECORD			;if yes, then examine the schedule record to see if it's already in the scheduler que. If not, add it. 
	JMP NEXT.SCHEDULE.RECORD
	

.CHECK.MINUTE
	LDA TIME.CURRENT.MINUTE
	CMP NPC.SCHEDULE+$3,X				;is schedule record for future or past?
	BCC EXAMINE.SCHEDULE.RECORD			;if yes, then examine the schedule record to see if it's already in the scheduler que. If not, add it. 
	JMP NEXT.SCHEDULE.RECORD
	
	
.DAY.TRANSITION
	LDA NPC.SCHEDULE+$2,X
	CMP	TIME.CURRENT.HOUR				;do the hours match?
	BEQ .CHECK.MINUTE					;if yes, check minutes
	BCC	.ABSOLUTE.TIME.CONVERSION		;if scheduler hour is less than current hour, then the scheduler hour might be shortly after midnight, and within the look ahead period. To find out we need to convert to an absolute value of sorts.
	JMP EXAMINE.SCHEDULE.RECORD			;scheduler hour is in the future and within the look ahead period. We can conclude this because based on tests performed, the scheduler hour is between #PATHGENERATOR.LOOK_AHEAD.DAY_TRANSITION.HOUR and midnight
	
.ABSOLUTE.TIME.CONVERSION	
	CLC
	ADC #$24
	SEC
	SBC TIME.CURRENT.HOUR
	CMP #PATHGENERATOR.LOOK_AHEAD.HOURS ;is schedule record within the look ahead period for which NPC.PATHGENERATOR calculates paths in advance?
	BCC EXAMINE.SCHEDULE.RECORD			;if yes, then examine the schedule record to see if it's already in the scheduler que. If not, add it. 
	JMP NEXT.SCHEDULE.RECORD			;if no, then get next schedule record
	
NEXT.SCHEDULE.RECORD
;INCREMENT NPC.SCHEDULE INDEX
	TXA
	CLC
	ADC #$08			
	TAX
	
;CHECK STOP VALUE
	LDA NPC.SCHEDULE+$0,X
	CMP #$FF				;at end of array?
	BEQ NPC.SCHEDULER.EXIT	;if yes, exit
	
	JMP LOOP.IDENTIFY.NEXT_PATHS	;if no, continue loop


EXAMINE.SCHEDULE.RECORD	
;IS RECORD ALREADY IN QUE?

;NPC.PATHGENERATOR.QUE		
;Byte 0		Byte 1			Byte 2		Byte 3		Byte 4
;Anchor #	NPC Record #	X of Anchor	Y of Anchor	Status
;see map objects spreadsheet for authoritative version


.LOOP.SEARCH.QUE
	LDA NPC.PATHGENERATOR.QUE+$4,Y	;load status field
	CMP #$FF						;is record empty?
	BNE .CONTINUE					;if no, continue
	STY PATHGENERATOR.QUE.INDEX		;if yes, save que index (so that if we have to add a record, we've got the index to an empty record handy) 

.CONTINUE	
	LDA NPC.PATHGENERATOR.QUE+$0,Y	;load anchor # of current que record
	CMP NPC.SCHEDULE+$0,X			;load anchor # of current schedule record. 
									;do they match?
	BEQ .EXISTING.RECORD.FOUND		;if yes, then anchor is already in que, no need to continue searching
									;if no, check next que record								
	
	INY	;+1
	INY ;+2
	INY	;+3
	INY	;+4
	INY	;+5							;Y++5
	CPY #NPC.PATHGENERATOR.QUE.SIZE	;at end of array?
	BNE .LOOP.SEARCH.QUE			;if no, continue loop
									;if yes, record wasn't found. Add new record.

	;**FALLS THROUGH**

.ADD.NEW.RECORD
	;ADD RECORD
	LDY PATHGENERATOR.QUE.INDEX
	
	;SAVE ANCHOR #
	LDA NPC.SCHEDULE+$0,X
	STA NPC.PATHGENERATOR.QUE+$0,Y
	
	;SAVE NPC RECORD #
	LDA NPC.SCHEDULE+$1,X
	STA NPC.PATHGENERATOR.QUE+$1,Y	

	;SAVE ANCHOR X-AXIS
	LDA NPC.SCHEDULE+$5,X
	STA NPC.PATHGENERATOR.QUE+$2,Y	
	
	;SAVE ANCHOR Y-AXIS
	LDA NPC.SCHEDULE+$6,X
	STA NPC.PATHGENERATOR.QUE+$3,Y	

	;SET STATUS TO "NEEDS PROCESSING"
	LDA #$00
	STA NPC.PATHGENERATOR.QUE+$4,Y	
	
	;**FALLS THROUGH**(to borrow the JMP in the next section)

	
.EXISTING.RECORD.FOUND
;NO NEED TO ADD A NEW RECORD SINCE AN EXISTING UNPROCESSED RECORD
;ALREADY EXISTS FOR THIS ANCHOR
	JMP NEXT.SCHEDULE.RECORD	


	
	
NPC.SCHEDULER.EXIT

		LDA #$AA
		LDX #NPC.PATHGENERATOR.QUE
		LDY /NPC.PATHGENERATOR.QUE
		JSR PREP.BRK
		BRK
		
		
	RTS

	;**Consider possibility of prior abort
	;for each candidate see if the lookup table record
	;for the candidates anchor/MO record # exists and is
	;marked as save. If so, skip. 
	;When the first candidate with no path saved is identified
	;I think NPC.PATHFINDER can be called as normal as it 
	;will detect a prior abort and skip the init of variables

	

@END

	
NPC.PATHGENERATOR
@START
				
;IS PLAYER IN A BUILDING? IF NOT, EXIT
	LDA PLAYER.MAP.LOCATION_TYPE		
	CMP #LOCATION.TYPE.BUILDING
	BNE .NPC.PATHGENERATOR.EXIT_STEP
	JMP .START
	
.NPC.PATHGENERATOR.EXIT_STEP
 JMP NPC.PATHGENERATOR.EXIT
 
 
.START

			
.DRIVER
@START


		;SET WEIGHT FOR NON-ROAD TILES
		;LDA #$10
		LDA #$10
		STA PATHFINDER.STREET.PREFERENCE	
		
		;SET PATHFINDER DESTINATION GMAP.X,Y
		; 26,26
		; LDA #$09
		; STA NPC.PATHFINDER.DESTINATION.TILE.X
		; LDA #$08
		; STA NPC.PATHFINDER.DESTINATION.TILE.Y	
		;
		; LDA #$0B
		; STA NPC.PATHFINDER.DESTINATION.TILE.X
		; LDA #$1D
		; STA NPC.PATHFINDER.DESTINATION.TILE.Y	
		;
		; LDA #$09
		; STA NPC.PATHFINDER.DESTINATION.TILE.X
		; LDA #$22
		; STA NPC.PATHFINDER.DESTINATION.TILE.Y
		;
		; LDA #$21
		; STA NPC.PATHFINDER.DESTINATION.TILE.X
		; LDA #$09
		; STA NPC.PATHFINDER.DESTINATION.TILE.Y
		;
		; 24,0C
		; LDA #$19
		; STA NPC.PATHFINDER.DESTINATION.TILE.X
		; LDA #$14
		; STA NPC.PATHFINDER.DESTINATION.TILE.Y
		; ;
		; LDA #$0B
		; STA NPC.PATHFINDER.DESTINATION.TILE.X
		; LDA #$25
		; STA NPC.PATHFINDER.DESTINATION.TILE.Y		
		;		
		; 19,24
		LDA #$12
		STA NPC.PATHFINDER.DESTINATION.TILE.X
		LDA #$08
		STA NPC.PATHFINDER.DESTINATION.TILE.Y		
		;
		; 0B,19
		; LDA #$25
		; STA NPC.PATHFINDER.DESTINATION.TILE.X
		; LDA #$17
		; STA NPC.PATHFINDER.DESTINATION.TILE.Y	
		
		

		;LOAD DATA FOR CURRENT SPRITE INTO THE PARAMETER ARRAY FOR NPC.PATHFINDER
		; LDX #$00
; .INIT.LOOP
		; LDA SPRITE.RECORD,X
		; STA PATHFINDER.SPRITE.RECORD,X		;PARAMETER
		; INX
		; CPX #SPRITE.RECORD.SIZE
		; BNE .INIT.LOOP	

			
;MAP_OBJECTS.MOB datagram
;Byte 0	Byte 1	Byte 2		Byte 3		Byte 4			Byte 5			Byte 6				Byte 7
;GMAP.X	GMAP.Y	Tile_type	Set to $03	Active Anchor	Path Index($00)	In Transit? ($00)	at-anchor move routine flag
;See Map Objects spreadsheet for authoratative version

;SPRITE.RECORD datagram	
;Byte 0						Byte 1						Byte 2		Byte 3			Byte 4			Byte 5		Byte 6		Byte 7						Byte 8			Byte 9			Byte $A
;player-relative.X of NPC	player-relative.Y of NPC	Tile_type	Set to $03 (SS)	Active Anchor	Path Index	In Transit?	at-anchor move routine flag	RMAP.X of NPC	RMAP.Y of NPC	Sprite_Type
;See Map Objects spreadsheet for authoratative version


;26.26.97.03.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00


		;SET GMAP X/Y OF SPRITE CURRENT POSITION 
;*****(NEED TO COME FROM ANOTHER LOOP)
		;LDA MAP_OBJECTS.NPC+$0
		LDA MAP_OBJECTS.NPC+$0					;X-Axis of Sprite current position
		STA PATHFINDER.SPRITE.RECORD+$8
		;LDA MAP_OBJECTS.NPC+$1
		LDA MAP_OBJECTS.NPC+$1				;y-Axis of Sprite current position
		STA PATHFINDER.SPRITE.RECORD+$9
		
		LDA MAP_OBJECTS.NPC+$2
		STA PATHFINDER.SPRITE.RECORD+$2		
		LDA MAP_OBJECTS.NPC+$3
		STA PATHFINDER.SPRITE.RECORD+$3
		LDA MAP_OBJECTS.NPC+$4
		STA PATHFINDER.SPRITE.RECORD+$4
		
		;HARD CODED
		LDA #$00
		STA PATHFINDER.SPRITE.RECORD+$B	;Map Object Record #
		
		;THE FOLLOWING ARE CURRENTLY INIT TO $00 IN MO ARRAY
		LDA MAP_OBJECTS.NPC+$5
		STA PATHFINDER.SPRITE.RECORD+$5
		LDA MAP_OBJECTS.NPC+$6
		STA PATHFINDER.SPRITE.RECORD+$6
		LDA MAP_OBJECTS.NPC+$7
		STA PATHFINDER.SPRITE.RECORD+$7
@END



	
GET.PATH
@START
;PARAMETERS: NPC.PATHFINDER.DESTINATION.TILE.X, NPC.PATHFINDER.DESTINATION.TILE.Y, PATHFINDER.STREET.PREFERENCE, PATHFINDER.SPRITE.RECORD($B), 	
;ENTRANCE: NPC.PATHGENERATOR
;RETURN: NPC.PATHFINDER.FINAL.PATH

				LDA COMPLETED.FLAG ;TEMP TO PREVENT RECALCULATION 
				CMP #$01
				BEQ .EXIT.ABORT
				
	
	JSR NPC.PATHFINDER ; returns movement code in ACC
		LDA NPC.PATHFINDER.ABORT_FLAG
		BEQ .FIND.OPEN.RECORD
		JMP .EXIT.ABORT
			
	; LDX NPC.PATHFINDER.FINAL.PATH+$0
	; STX FINAL.PATH.INDEX
	
.FIND.OPEN.RECORD	
;FIND OPEN RECORD FOR SAVED PATH
	LDX #$00
.LOOP.FIND.OPEN_RECORD
	LDA SAVED.PATH.LOOKUP.TABLE+$2,X		;$01 = SAVE, $00 = DISCARD
	CMP #$00
	BEQ	.SAVE.RECORD
	INX
	CPX #SAVED.PATH.LOOKUP.TABLE.SIZE
	BNE .LOOP.FIND.OPEN_RECORD
;ERROR: NO OPEN RECORD FOUND
;.LOOP.FIND.OPEN_RECORD reports no open record found in NPC.PATHGENERATOR.SAVED_PATHS.AUX 
;This could be the result too many NPCs currently transitioning (tieing up records)
;and too many NPCs scheduled to transition soon (for which this subroutine is trying 
;to generate and save paths; requiring records.)
	LDA TEXT
	LDA $C082
	BRK
	
.SAVE.RECORD
		;X-REG = RECORD # OF NEXT OPEN RECORD	
	JSR SAVE.PATH.AUX_MEMORY
	

;UPDATE SAVED PATH RECORD	
;MARK RECORD STATUS AS "SAVE"
;SET STATUS FIELD TO "SAVE"
;(this prevents the record from being overwritten by new records)
	
	;Set Anchor #
	LDA PATHFINDER.SPRITE.RECORD+$4
	STA SAVED.PATH.LOOKUP.TABLE+$0,X
	
	;Set NPC Array Record #
	LDA PATHFINDER.SPRITE.RECORD+$B
	STA SAVED.PATH.LOOKUP.TABLE+$1,X
	
	LDA #$01							;$01 = SAVE, $00 = DISCARD
	STA SAVED.PATH.LOOKUP.TABLE+$2,X
		


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
			; LDA #$B6
			; STA AUX_MOVE.DEST+$1
			; CLC					;SET CARRY FLAG DESGINATD MOVE FROM AUX MEMORY -> MAIN
			; JSR AUX_MOVE
			; ;
			; LDX #SAVED.PATH.LOOKUP.TABLE
			; LDY /SAVED.PATH.LOOKUP.TABLE
			; ;
				LDA #$AA
				JSR PREP.BRK
				BRK
	
	JMP NPC.PATHGENERATOR.EXIT
	
.EXIT.ABORT
	
	;***DEPENDING ON WHETHER GET.PATH IS CALLED BY JSR,
	;THIS ROUTINE WILL EITHER RTS (FROM THE ORIGINAL CALL
	;TO NPC.PATHGENERATOR) OR IT WILL POP ONE OR MORE RTS OFF
	;THE STACK, THEN RTS
	
	
	; LDA #$AB	
	; JSR PREP.BRK
	; BRK	
	
	RTS
	



@END
	
	
SAVE.PATH.AUX_MEMORY
@START

;PARAMETERS: SAVED.PATH.HO_ADDRESS.OFFSET, X-REG (RECORD # OF NEXT OPEN RECORD)
;ENTRANCE: via NPC.PATHGENERATOR
;RETURN: NPC.PATHGENERATOR.SAVED_PATHS.AUX



;**OPT** Aux memory. Aux memory is reserved for 16 saved paths (1 page per path, 1 superpage total). Evaluate the max number of saved paths at any given time in the game and trim the memory reservation accordingly. 

; ;NOTE: going to track this in the saved path lookup table
; ;SET STATUS FIELD TO "SAVE"
; ;(this prevents the record from being overwritten by new records)
		; LDA #$01							;$01 = SAVE, $00 = DISCARD
		; STA NPC.PATHFINDER.FINAL.PATH+$1
		

;COPY PATH DATA (main memory -> aux memory)	

		;SET AUX START & END ADDRESS
		LDA #NPC.PATHFINDER.FINAL.PATH
		STA AUX_MOVE.START
		CLC
		ADC #$FF
		STA AUX_MOVE.END
		LDA /NPC.PATHFINDER.FINAL.PATH+$1
		ADC #$00				;16-BIT ADD
		STA AUX_MOVE.END+$1
		STA AUX_MOVE.START+$1
		
		;SET AUX DESTINATION ADDRESS
		LDA #NPC.PATHGENERATOR.SAVED_PATHS.AUX
		CLC
		ADC #$00
		STA AUX_MOVE.DEST
		TXA	;Contains the HO byte offset for the open record we're overwriting
		ADC /NPC.PATHGENERATOR.SAVED_PATHS.AUX		;16-BIT ADD
		STA AUX_MOVE.DEST+$1
		
		
		SEC                ;SET CARRY FLAG DESGINATD MOVE FROM MAIN MEMORY -> AUX
		JSR AUX_MOVE	
	
	RTS
	
@END
	

NPC.PATHGENERATOR.EXIT
	

	RTS
	
@END

	
NPC.PATHFINDER
@START
;PARAMETERS: NPC.PATHFINDER.DESTINATION.TILE.X, NPC.PATHFINDER.DESTINATION.TILE.Y, PATHFINDER.STREET.PREFERENCE, PATHFINDER.SPRITE.RECORD($B), MOB.MOVES.BLOCKED
;ENTRANCE:DIRECT
;RETURN: NPC.PATHFINDER.FINAL.PATH*
;*The first byte contains the index to record containing the x,y coordinates of the NPCs next move (which is the 2nd to last record in the final path, the last record contains the x,y coordinates of the NPC's current position)
;*The blocked/open status of each direction from the NPC's starting postion

;=====================SUBROUTINE DOCUMENTATION====================================
;
;
;====PREAMBLE====
;NPC.PATHFINDER is based on Greed's Best Path First algorithm, with some custom 
;enhancements to the priority que. 
;
;The best documentation I've seen on A* and Greedy's best path first algorithms
;http://www.redblobgames.com/pathfinding/a-star/introduction.html
;
;====OVERVIEW===
;The following as an overview of the implimentation in Nox Archaist.
; 
;The Pathfinder algorithm searches tiles for neighbors with open
;paths beginning at the current position of the sprite when 
;this subroutine was called.
;
;Open paths are added to the NPC.PATHFINDER.SEARCH.PATHS array and
;are added to a priority que (NPC.PATHFINDER.PRIORITY.QUE array) which determines the tile which will
;be searched next.
;
;The priority guides the search process on each iteratio by 
;serving up the Path Tile # with the shorest distance to the 
;destination. However, the priority que is not sorted each 
;iteration (more details below in Priority Que Operations).
;
;Eventually the algorithm searches enough tiles that the 
;the record at the top of the priority que has the X,Y coordinates
;of the destination and the algorithm exits. 
;
;====DETAILS====
;
;-Datagrams
;For Datagrams on the folllowing arrays, see map objects spreadsheet
;NPC.PATHFINDER.SEARCH.PATHS
;NPC.PATHFINDER.PRIORITY.QUE
;
;-Limitations / Error Checking
;The algorithm is very resource intensive, both for memory and
;CPU cycles. 
;
;Within a building map there are some paths that
;are too long for the algorithm to process without running out
;of memory for a paritular array or reaching the maxium $FF
;records in NPC.PATHFINDER.SEARCH.PATHS because Path Tile #
;in an 8-bit field. Keep in mind when considering how "long"
;a path is that the algorith will explore dead ends along the
;way to the destination. Efficient building design tips:
;keep the center of the building map open.  
;
;The following is a descrition of the error checking designed 
;to detect memory overflows and other issues. 
;
;The search path array, tile number tally, and final path array have overflow
;error traps. Priority que overflows result in the last record in the que being 
;overwritten. See comments at the top of .ADD_PRIORITY.QUE for more details.
;The priority que also has an underflow error trap by making sure
;there is at least one record in the que. This way detection occurs
;if a problem result in the algorithm burns through all recrords
;in the que without finding the destination tile.  
;
;
;-Misc Notes
;No Duplicate Neighbors: neighbors with the same X,Y are not 
;recorded. All instances after the first are discarded. This
;can result in a final path being less direct than it otherwise
;would be, but this saves a lot of memory in the search array
;and also CPU cycles. 
;
;GMAP and RMAP are the same because buildings are in the 9 zones
;located in the upper left corner of the map. 
;
;-Priority Que Operations
;
;When the que is sorted, a shell sort is used. I used a bubble
;sort initially, to quickly do a proof of concept, and the 
;algorithm processing time using shell sort was 20% of the 
;processing time using bubble sort. The speed was increased
;by a simular factor by adding shortest distance detection.
;
;
;Shortest distance detection: If a newly discovered neighbor is a shorter distance than the top record in the que, 
;then the top record is simply replaced and no sort is performed.
;However, if no neighbors discovered in a given iteration are a shorter
;distance than the top record in the que, the que is sorted. 
;
;When a top record swap occurs, the swapped record is added to
;the bottom of the que, unless it is a processed record, in which
;case it is discarded. The former occurs if more than one swap occurs 
;per iteration, which would happen if a neighbor tile has a shorter
;distance than the top record and then another neighbor tile is 
;discovered with a shorter distance than the first neighbor tile.
;
;
;-Summary of Que Index & Size Counter Adjustments
;Que Index = NPC.PATHFINDER.PRIORITY.QUE.RECORD_INDEX
;Que Size  = NPC.PATHFINDER.PRIORITY.QUE.RECORD_TALLY
;
;Top Record Replaced, no swap:	DEC Que Index
;								DEC Que Size
;
;Processed Record in Que: 		DEC Que Size 	;processed record in que flag is set when sort occurs because by definition that means that a processed record will end up at the bottom of the que, which we want to overwrite. 
;
;Always:	
;		  	INC Que Size
;			INC Que Index
;
;
;
;==Glossary==
;Building Path Grid = The building maps use the same GMAP grid as the surface map and undermap, but the pathfinder algorithm ignores the buffer tiles that surround the actual building. Building Map Grid refers to the coordinate set used for building tiles. 
;					  To convert from GMAP/RMAP X/Y: X-8, Y-7
;Closed Set		= Path Tiles which have already been searched for neighbor tiles. They are added to the closet set to prevent them from being searched again and to prevent new neighbor tiles from being added with the same X,Y coordinates.
;Neighbor Tile	= a tile which is adjacent on the X or Y axis to a Path Tile
;Open Path		= if the MOVES.BLOCKED array doesn't mark a particular direction as blocked, that direction is an open path
;Open Set		= All tiles which are not in the Closed Set
;Path Tile 		= a tile which has been or will be searched for neighbor tiles with open paths
;
;=================================================================================

;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA
	
.DRIVER
@START

;SET DESTINATION

	; LDA #$0B
	; STA NPC.PATHFINDER.DESTINATION.TILE.X
	; LDA #$1D
	; STA NPC.PATHFINDER.DESTINATION.TILE.Y
	
	; LDA #$26
	; STA NPC.PATHFINDER.DESTINATION.TILE.X
	; LDA #$23
	; STA NPC.PATHFINDER.DESTINATION.TILE.Y	

	; LDA #$15
	; STA NPC.PATHFINDER.DESTINATION.TILE.X
	; LDA #$1F
	; STA NPC.PATHFINDER.DESTINATION.TILE.Y	
	
	; LDA #$1F
	; STA NPC.PATHFINDER.DESTINATION.TILE.X
	; LDA #$20
	; STA NPC.PATHFINDER.DESTINATION.TILE.Y	
	
	
	; LDA #$0D
	; STA NPC.PATHFINDER.DESTINATION.TILE.X
	; LDA #$19
	; STA NPC.PATHFINDER.DESTINATION.TILE.Y	
	
	; LDA #$09
	; STA NPC.PATHFINDER.DESTINATION.TILE.X
	; LDA #$08
	; STA NPC.PATHFINDER.DESTINATION.TILE.Y	
	
	; LDA #$21
	; STA NPC.PATHFINDER.DESTINATION.TILE.X
	; LDA #$0A
	; STA NPC.PATHFINDER.DESTINATION.TILE.Y
	
	; LDA #$09
	; STA NPC.PATHFINDER.DESTINATION.TILE.X
	; LDA #$08
	; STA NPC.PATHFINDER.DESTINATION.TILE.Y

;SET STARTING POSITON
;TEMP UNTIL SPRITE RECORD PROBLEM IS FIXED	
			; LDA #$26
			; STA PATHFINDER.SPRITE.RECORD+$08
			; LDA #$26
			; STA PATHFINDER.SPRITE.RECORD+$09
			
			; LDA #$00					;NORTH
			; STA MOB.MOVES.BLOCKED+$0
			; LDA #$01					;SOUTH
			; STA MOB.MOVES.BLOCKED+$1
			; LDA #$00					;EAST
			; STA MOB.MOVES.BLOCKED+$2
			; LDA #$00					;WEST
			; STA MOB.MOVES.BLOCKED+$3		


			; LDA #$24
			; STA PATHFINDER.SPRITE.RECORD+$08
			; LDA #$0C
			; STA PATHFINDER.SPRITE.RECORD+$09

			; LDA #$01					;NORTH
			; STA MOB.MOVES.BLOCKED+$0
			; LDA #$00					;SOUTH
			; STA MOB.MOVES.BLOCKED+$1
			; LDA #$01					;EAST
			; STA MOB.MOVES.BLOCKED+$2
			; LDA #$01					;WEST
			; STA MOB.MOVES.BLOCKED+$3	
			
			
			; LDA #$1A
			; STA PATHFINDER.SPRITE.RECORD+$08
			; LDA #$26
			; STA PATHFINDER.SPRITE.RECORD+$09
			
			; LDA #$01					;NORTH
			; STA MOB.MOVES.BLOCKED+$0
			; LDA #$00					;SOUTH
			; STA MOB.MOVES.BLOCKED+$1
			; LDA #$01					;EAST
			; STA MOB.MOVES.BLOCKED+$2
			; LDA #$00					;WEST
			; STA MOB.MOVES.BLOCKED+$3	

			
			; LDA #$0B
			; STA PATHFINDER.SPRITE.RECORD+$08
			; LDA #$19
			; STA PATHFINDER.SPRITE.RECORD+$09
			
			; LDA #$01					;NORTH
			; STA MOB.MOVES.BLOCKED+$0
			; LDA #$00					;SOUTH
			; STA MOB.MOVES.BLOCKED+$1
			; LDA #$01					;EAST
			; STA MOB.MOVES.BLOCKED+$2
			; LDA #$00					;WEST
			; STA MOB.MOVES.BLOCKED+$3
			
;ALL POSITIONS	
			; LDA #$8D
			; STA PATHFINDER.SPRITE.RECORD+$2
@END
			

.ABORT.CHECK
	
	LDA NPC.PATHFINDER.ABORT_FLAG
	CMP #$01					;is abort flag set?
	BNE .INIT.STANDARD			;if no, do standard init variables. 
								;if yes, use re-entry init routine and clear abort flag

.INIT.REENTRY								
;CLEAR ABORT FLAG
	LDA #$00					;$00 = No abort, $01 = aborted path is pending completion
	STA NPC.PATHFINDER.ABORT_FLAG	
	STA ITERATION.COUNTER		;reset as this counter tracks the auto-abort threashold.

	
	
		; LDA #$01
		; STA TROUBLESHOOTING.HOOK
		
	LDY SEARCH.PATHS.LO_INDEX

	LDA	SEARCH.PATHS.POINTER.SAVED				;Setup pointer for the search path array so it can be accessed via indirect index by Y mode.
	STA NPC.PATHFINDER.SEARCH.PATHS.POINTER
	LDA	SEARCH.PATHS.POINTER.SAVED+$1
	STA NPC.PATHFINDER.SEARCH.PATHS.POINTER+$1
	

	
	JMP OUTER.LOOP
			
.INIT.STANDARD
@START
	;I THINK ALL THIS WILL BE IN FINAL
	;(the records dervied from sprite values are setup dynamically already, the others should be fixed values)

;INIT 16-BIT CLOSE_SET ARRAY TO $00s
		LDA #NPC.PATHFINDER.CLOSED_SET
		STA FILL.START
		LDA /NPC.PATHFINDER.CLOSED_SET
		STA FILL.START+$1
		
		LDA FILL.START
		CLC
		ADC #$FF
		STA FILL.END
		LDA FILL.START+$1
		ADC #$03	
		STA FILL.END+$1
		
		LDA #$00
		STA FILL.VALUE
		
	JSR MEMORY.FILL
				
				
	
;INIT 16-BIT SEARCH PATH ARRAY TO $00s
		LDA #NPC.PATHFINDER.SEARCH.PATHS
		STA FILL.START
		LDA /NPC.PATHFINDER.SEARCH.PATHS
		STA FILL.START+$1
		
		LDA FILL.START
		CLC
		ADC #$FF
		STA FILL.END
		LDA FILL.START+$1
		ADC #$0A	
		STA FILL.END+$1
		
		LDA #$AA
		STA FILL.VALUE
		
	JSR MEMORY.FILL
			

	
;INIT PRIORITY QUE ARRAY TO $00s
		LDA #NPC.PATHFINDER.PRIORITY.QUE
		STA FILL.START
		LDA /NPC.PATHFINDER.PRIORITY.QUE
		STA FILL.START+$1
		
		LDA FILL.START
		CLC
		ADC #$FF
		STA FILL.END
		LDA FILL.START+$1
		ADC #$00	
		STA FILL.END+$1
		
		LDA #$AA
		STA FILL.VALUE
		
	JSR MEMORY.FILL	

;INIT FINAL PATH ARRAY TO $00s
		LDA #NPC.PATHFINDER.FINAL.PATH
		STA FILL.START
		LDA /NPC.PATHFINDER.FINAL.PATH
		STA FILL.START+$1
		
		LDA FILL.START
		CLC
		ADC #$FF
		STA FILL.END
		LDA FILL.START+$1
		ADC #$00	
		STA FILL.END+$1
		
		LDA #$AA
		STA FILL.VALUE
		
	JSR MEMORY.FILL	
		

				
;ADD SPRITE STARTING POSITION TO CLOSED SET
	LDA PATHFINDER.SPRITE.RECORD+$8				;x-axis of sprite starting positon 
	SEC
	SBC #CLOSED_SET.X.ADJ
	STA CURRENT.TILE.ADJUSTED.X			;Convert X-axis to the Building Path Grid
	
	
	LDA PATHFINDER.SPRITE.RECORD+$9				;y-axis of sprite starting positon
	SEC
	SBC #CLOSED_SET.Y.ADJ
	STA CURRENT.TILE.ADJUSTED.Y			;Convert Y-axis to the Building Path Grid
	
	;get address of closed_set element for the X/Y coordinates
	LDA #NPC.PATHFINDER.CLOSED_SET			;get  base address of closed_set array
	STA NPC.PATHFINDER.CLOSED_SET.POINTER
	LDA /NPC.PATHFINDER.CLOSED_SET
	STA NPC.PATHFINDER.CLOSED_SET.POINTER+$1
	
	LDX CURRENT.TILE.ADJUSTED.Y				;get column adjustment to base address
	LDA NPC.PATHFINDER.CLOSED_SET.POINTER
	CLC
	ADC CLOSED_SET.MULTIPLY_TABLE.LO,X	
	STA NPC.PATHFINDER.CLOSED_SET.POINTER
	
	LDA NPC.PATHFINDER.CLOSED_SET.POINTER+$1	
	
	ADC CLOSED_SET.MULTIPLY_TABLE.HO,X
	STA NPC.PATHFINDER.CLOSED_SET.POINTER+$1
	
	;mark the closet_set element as searched
	LDY CURRENT.TILE.ADJUSTED.X
	LDA #$01
	STA (NPC.PATHFINDER.CLOSED_SET.POINTER),Y
	
	
;INIT FIRST RECORD IN NPC.PATHFINDER.SEARCH.PATHS
	LDA #$00								;set path tile#
	STA NPC.PATHFINDER.SEARCH.PATHS+$0
	STA NPC.PATHFINDER.SEARCH.PATHS+$4		;set X-axis (path tile #), for first record it is an unused place holder value. 
	STA NPC.PATHFINDER.SEARCH.PATHS+$5		;set Y-axis (path tile #), for first record it is an unused place holder value. 					
	STA NPC.PATHFINDER.SEARCH.PATHS+$1		;set source path tile #
	
	LDA PATHFINDER.SPRITE.RECORD+$08					;set X axis (source path tile #) using sprite RMAP.X at starting location
	STA NPC.PATHFINDER.SEARCH.PATHS+$2
	STA NPC.PATHFINDER.CURRENT.TILE.X		
	LDA PATHFINDER.SPRITE.RECORD+$09					;set Y axis (source path tile #) using sprite RMAP.X at starting location
	STA NPC.PATHFINDER.SEARCH.PATHS+$3
	STA NPC.PATHFINDER.CURRENT.TILE.Y	

;INIT MISC VARIABLESS
	LDA #$01								;start the tally at $01 because the first path tile # was already assigned above in this init section when the first record was init to the search path array.
	STA NPC.PATHFINDER.TILE_NUMBER.TALLY
	STA ITERATION.COUNTER ;TROUBLESHOOTING

	LDA #$FF
	STA PATH_TILE.SHORTEST.DISTANCE			;set the distance really high so it is easy to beat. The first neighbor tile discovered should surpass this value as the shortest distance. 			

	LDA #$00
	STA NPC.PATHFINDER.CURRENT.TILE	
	STA NPC.PATHFINDER.PRIORITY.QUE.SIZE+$1 ;the pathfinder que won't exceed an 8-bit record size value so we're doing an init of the 2nd byte here to make sure it's $00, the correct value for an 8-bit size stored in the 1st byte.
	STA NPC.PATHFINDER.PRIORITY.QUE.PROCESSED.RECORDS


	
;INIT COUNTERS, INDEXES, AND POINTERS
	LDA	#NPC.PATHFINDER.SEARCH.PATHS			;Setup pointer for the search path array so it can be accessed via indirect index by Y mode.
	STA NPC.PATHFINDER.SEARCH.PATHS.POINTER
	LDA	/NPC.PATHFINDER.SEARCH.PATHS
	STA NPC.PATHFINDER.SEARCH.PATHS.POINTER+$1
	
	LDA #$01									;# of 2byte records in the NPC.PATHFINDER.PRIORITY.QUE. Init value of $1 (2 bytes) used because the first two bytes in the array are reserved for the length of the data portion of the array, which is updated just before the array is sorted.  									
	STA NPC.PATHFINDER.PRIORITY.QUE.RECORD_TALLY
	STA NPC.PATHFINDER.PRIORITY.QUE.RECORD_INDEX
	
	LDY #SEARCH.PATHS.RECORD.SIZE				;#CONSTANT. NPC.PATHFINDER.SEARCH.PATHS index. 
	
	
	;First record already filled in init variables section, start with 2nd record

@END

				
OUTER.LOOP
	LDX #$00									;reset .LOOP.ACQUIRE.NEIGHBOR.TILES counter
	STX PATHFINDER.SHORTEST.DISTANCE.COUNTER	;reset counter. Tracks whether, in a given iteration, a neighor was found which had a shorter distance than any other tile in the que. 
	STX SEARCH.PATHS.NEIGHBORS_ADDED.COUNTER	;reset counter. Tracks the number of neigbors added in a given iteration.
	STX DIRECTION.TEST.COUNTER					;reset counter. Tracks which directions have been tested for open paths in LOOP.ACQUIRE.NEIGHBOR.TILES
	STX NPC.PATHFINDER.DIRECTIONS_CHECKED+$0
	STX NPC.PATHFINDER.DIRECTIONS_CHECKED+$1
	STX NPC.PATHFINDER.DIRECTIONS_CHECKED+$2
	STX NPC.PATHFINDER.DIRECTIONS_CHECKED+$3
	
;MARK CURRENT PRIORITY QUE RECORD AS PROCESSED
;(the reason we are marking the current record as processed before the processing has actually occured
;is because the current record, which is always at the top of the prioirty que at the start of outer loop, may
;get moved to the bottom of the que and float up several records from the bottom depending on the distances
;of the neighbor tiles of the current tile. Marking the current record as processed now saves having to track
;an index to its location. 
;
;Even though the current record is marked as process now, the processing
;will occur because the values from the current record have already been
;loaded into the variables used in the routine such as NPC.PATHFINDER.CURRENT.TILE.X, NPC.PATHFINDER.CURRENT.TILE.Y, NPC.PATHFINDER.CURRENT.TILE))
;
	LDA	NPC.PATHFINDER.PRIORITY.QUE+$02
	STA PATH_TILE.SHORTEST.DISTANCE			;save the actual distance for the current tile since the next commands clobber the value with #$FE to mark the record as processed. 
	LDA #$FE
	STA NPC.PATHFINDER.PRIORITY.QUE+$02		;by setting the distance on top record in the que to $FE. The sort algorithm doesn't seem to sort $FF values for unknown reasons, so $FE is used. 

IDENTIFY.OPEN.PATHS ;(run collision controls on tile just loaded)
@START

;SAVE REGISTERS
	STY SAVED.YREG.LOCAL					;general save, used STY instead of PHA
	
	
;CONVERT CURRENT TILE.XY TO CURRENT.TILE.RMAP.XY
	LDY NPC.PATHFINDER.CURRENT.TILE.Y
	LDA RMAP.MULTIPLY_TABLE.HO,Y
	STA NPC.PATHFINDER.NEIGHBOR.RMAP+$1
	LDA RMAP.MULTIPLY_TABLE.LO,Y
	STA NPC.PATHFINDER.NEIGHBOR.RMAP

	
	CLC
	ADC NPC.PATHFINDER.CURRENT.TILE.X
	STA NPC.PATHFINDER.NEIGHBOR.RMAP					
	LDA NPC.PATHFINDER.NEIGHBOR.RMAP+$1
	ADC #$00 ;16-BIT ADD
	STA NPC.PATHFINDER.NEIGHBOR.RMAP+$1

;===COLLISION CHECK FOR PATH TILES====
;(same code as COLLISION_CHECK_OFFSCREEN used during the MOB/NPC move selection routines)
;(some comments may not be updated yet as this code was a copy/paste
;from the previously mentioned routine.)

;STORE TILE #S ADJACENT TO NPC IN AN ARRAY SO WE CAN USE A LOOP TO APPLY COLLISION RULES

	;STX SAVED.XREG.LOCAL						;COLLISION_CHECK_ONSCREEN USE AN X INDEX, SO IT SAVES X-REG, RESULTING IN A RESTORE X-REG UP AHEAD. SO THIS CODE NEEDS TO SAVE X-REG TO SO THAT THE CORRECT VALUE IS LOADED IN THE RESTORE. 
;SAVE TILE # NORTH OF NPC
;=======INLINE CODE FOR SBC.16========	
;MAP_OBJECTS.MAP_LOCATION(2)- #OFFSET.UP(1)

			
	CLD 
    SEC                           ;ALWAYS BEFORE SUBTRACTION
    LDA NPC.PATHFINDER.NEIGHBOR.RMAP
    SBC #OFFSET.UP
    STA RMAP.LOOKUP	
    LDA NPC.PATHFINDER.NEIGHBOR.RMAP+$1
    SBC #$00
    STA RMAP.LOOKUP+$1	
	
;======================================

;LOOKUP TILE_TYPE
;=======INLINE CODE FOR ADC.16========	
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
	STA MOB.ADJACENT_TILES		 	;BORROWING SCREEN.TILE.HOPPER TO STORE THE TILE TYPE ADJACENT TO THE NPC
				
;
;SAVE TILE # WEST OF NPC
;=======INLINE CODE FOR SBC.16========	
;NPC.PATHFINDER.NEIGHBOR.RMAP(2) - $01 (ALWAYS THE OFFSET TO THE LEFT)
	
	CLD 
    SEC                         	  ;ALWAYS BEFORE SUBTRACTION
    LDA NPC.PATHFINDER.NEIGHBOR.RMAP
    SBC #$01
    STA RMAP.LOOKUP	
    LDA NPC.PATHFINDER.NEIGHBOR.RMAP+$1
    SBC #$00
    STA RMAP.LOOKUP+$1					;BORROWING SCREEN.TILE.HOPPER TO STORE THE RMAP OF THE TILE #S ADJACENT TO THE NPC
;======================================
;LOOKUP TILE_TYPE
;=======INLINE CODE FOR ADC.16========	
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
	STA MOB.ADJACENT_TILES+$3			;BORROWING SCREEN.TILE.HOPPER TO STORE THE TILE TYPE ADJACENT TO THE NPC	

;
;				
;SAVE TILE # EAST OF NPC
;=======INLINE CODE FOR ADC.16========	
;NPC.PATHFINDER.NEIGHBOR.RMAP(2)+ $01 (ALWAYS THE OFFSET TO THE RIGHT)


; DO THE MATH
	CLD 								
    CLC                       			 ;ALWAYS BEFORE ADD
    LDA NPC.PATHFINDER.NEIGHBOR.RMAP							
    ADC #$01
    STA RMAP.LOOKUP
		 
    LDA NPC.PATHFINDER.NEIGHBOR.RMAP+$1
    ADC #$00							;carry flag not cleared via CLC intentionally, it's part of 16-bit adding. 
    STA RMAP.LOOKUP+$1			
	
;======================================


;LOOKUP TILE_TYPE
;=======INLINE CODE FOR ADC.16========	
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
	STA MOB.ADJACENT_TILES+$2			;BORROWING SCREEN.TILE.HOPPER TO STORE THE TILE TYPE ADJACENT TO THE NPC		
				
;
;	
;SAVE TILE # SOUTH OF NPC
;=======INLINE CODE FOR ADC.16========	
;NPC.PATHFINDER.NEIGHBOR.RMAP(2)+ #OFFSET.DOWN(1)


; DO THE MATH
	CLD 								
    CLC                       			 ;ALWAYS BEFORE ADD
    LDA NPC.PATHFINDER.NEIGHBOR.RMAP							
    ADC #OFFSET.DOWN
    STA RMAP.LOOKUP
		 
    LDA NPC.PATHFINDER.NEIGHBOR.RMAP+$1
    ADC #$00							;carry flag not cleared via CLC intentionally, it's part of 16-bit adding. 
    STA RMAP.LOOKUP+$1					;BORROWING SCREEN.TILE.HOPPER TO STORE THE TILE #S ADJACENT TO THE NPC
	
;======================================

;LOOKUP TILE_TYPE
;=======INLINE CODE FOR ADC.16========	
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
	STA MOB.ADJACENT_TILES+$1				;BORROWING SCREEN.TILE.HOPPER TO STORE THE TILE TYPE ADJACENT TO THE NPC	

@MIDDLE

;NPC GROUP: DETERMINE WHETHER TO APPLY LAND OR SEA NPC COLLISION RULES	
	LDA PATHFINDER.SPRITE.RECORD+$2
	CMP #COLLISION_FLAG.MOB_SEA.START			;FIRST TILE IN SEA NPC RANGE. 
	BCS .COLLISION.LOOP.MOB_SEA


;APPLY MOB/NPC WALKING COLLISION RULES		
	LDY #$00
.LOOP.MOB_LAND
;IS NPC COLLISION OVERRIDE ON?
	LDA MOB.COLLISION_OVERRIDE
	CMP #$01
	BEQ .MOVE_PERMITTED

;BEGIN MAIN TESTS
	LDA MOB.ADJACENT_TILES,Y					;LOAD NEXT ADJACENT TILE TYPE
			
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
	BEQ .COLLISION_TESTS_COMPLETE
	INY
	JMP .LOOP.MOB_LAND
	
.COLLISION.LOOP.MOB_SEA	
	;***<INSERT RULES FOR SEA MOBS>***
	;COPY THE SECTION ABOVE FOR LAND MOBS AS A STARTING POINT
 
	;*****FALLS THROUGH

.COLLISION_TESTS_COMPLETE

;RESTORE REGISTERS
	LDY SAVED.YREG.LOCAL					;general restore, used LDY instead of PHA


@END


LOOP.ACQUIRE.NEIGHBOR.TILES
;
.PATHS_4 ;SPLIT POINTS $3F, $74, $BF
	JSR RANDOM.8
	CMP #$3F
	BCS .PATHS_4.NEXT_TEST1
;P4.OPTION0
	LDX #$00			;NORTH
	LDA NPC.PATHFINDER.DIRECTIONS_CHECKED,X
	CMP #$00							;has this direction already been checked?
	BNE LOOP.ACQUIRE.NEIGHBOR.TILES		;if no, pick new random number and try a different direction
	JMP .TEST.DIRECTION
.PATHS_4.NEXT_TEST1
	CMP #$BF
	BCS .P4.OPTION3
;PATHS_4.NEXT_TEST2 
	CMP #$74
	BCS .P4.OPTION2
;P4.OPTION1	
	LDX #$01			;SOUTH
	LDA NPC.PATHFINDER.DIRECTIONS_CHECKED,X
	CMP #$00							;has this direction already been checked?
	BNE LOOP.ACQUIRE.NEIGHBOR.TILES		;if no, pick new random number and try a different direction
	JMP .TEST.DIRECTION
.P4.OPTION2
	LDX #$02			;EAST
	LDA NPC.PATHFINDER.DIRECTIONS_CHECKED,X
	CMP #$00							;has this direction already been checked?
	BNE LOOP.ACQUIRE.NEIGHBOR.TILES		;if no, pick new random number and try a different direction
	JMP .TEST.DIRECTION
.P4.OPTION3
	LDX #$03			;WEST
	LDA NPC.PATHFINDER.DIRECTIONS_CHECKED,X
	CMP #$00							;has this direction already been checked?
	BNE LOOP.ACQUIRE.NEIGHBOR.TILES		;if no, pick new random number and try a different direction
	;
	;***FALLS THROUGH
	;
.TEST.DIRECTION
	INC DIRECTION.TEST.COUNTER
	LDA MOB.MOVES.BLOCKED,X					;load the collision status (west) for the current tile. $00 = open. $01 = blocked. ;X=$00=north, X=$01=south, X=$02=east, X=$03=west 
	CMP #$00								;is direction blocked?
	BNE .NEXT.DIRECTION						;if yes, then next direction test 
	JSR PATH.OPEN							;if no, then a candidate new neigbor has been found. Evaluate this open path.
	;
.NEXT.DIRECTION
	LDA #$01
	STA NPC.PATHFINDER.DIRECTIONS_CHECKED,X ;mark current direction as checked. 
	;
	LDA DIRECTION.TEST.COUNTER
	CMP #$04								;have all 4 directions been checked for open paths?
	BNE .NOT.DONE.YET 						;if no, generate a new random number and check another direction
	JMP NEXT.PATH.TILE						;if yes, then get next path tile			
	;
.NOT.DONE.YET
	LDA DIRECTION.TEST.COUNTER
	CMP #$03
	BCC .RETURN.TO.MAIN_LOOP
	LDX #$00
.LOOP.DEFAULT.SELECTION
	LDA NPC.PATHFINDER.DIRECTIONS_CHECKED,X
	CMP #$00
	BEQ .TEST.DIRECTION
	INX
	JMP .LOOP.DEFAULT.SELECTION
	;
.RETURN.TO.MAIN_LOOP
	JMP LOOP.ACQUIRE.NEIGHBOR.TILES
	
; LOOP.ACQUIRE.NEIGHBOR.TILES.PREFER_WEST
; ;#+$00=north, +$01=south, +$02=east, +$03=west
; ;.TEST.WEST
	; LDX #$03
	; LDA MOB.MOVES.BLOCKED+$3				;load the collision status (west) for the current tile. $00 = open. $01 = blocked.  
	; CMP #$00								;is direction blocked?
	; BNE .TEST_NORTH							;if yes, then next direction test 
	; JSR PATH.OPEN							;if no, then a candidate new neigbor has been found. Evaluate this open path.
	;;
; .TEST_NORTH
	; LDX #$00
	; LDA MOB.MOVES.BLOCKED+$0				;load the collision status (north) for the current tile. $00 = open. $01 = blocked.  
	; CMP #$00								;is direction blocked?
	; BNE .TEST_SOUTH							;if yes, then next direction test 
	; JSR PATH.OPEN							;if no, then a candidate new neigbor has been found. Evaluate this open path.
	;;
; .TEST_SOUTH
	; LDX #$01
	; LDA MOB.MOVES.BLOCKED+$1				;load the collision status (south) for the current tile. $00 = open. $01 = blocked.  
	; CMP #$00								;is direction blocked?
	; BNE .TEST_EAST							;if yes, then next direction test 
	; JSR PATH.OPEN							;if no, then a candidate new neigbor has been found. Evaluate this open path.
	;;
; .TEST_EAST
	; LDX #$02
	; LDA MOB.MOVES.BLOCKED+$2				;load the collision status (east) for the current tile. $00 = open. $01 = blocked.  
	; CMP #$00								;is direction blocked?
	; BNE .NEXT.PATH.TILE_STEP				;if yes, then next direction test 
	; JSR PATH.OPEN							;if no, then a candidate new neigbor has been found. Evaluate this open path.
	;
; .NEXT.PATH.TILE_STEP								
	; JMP NEXT.PATH.TILE						;if yes, then get next path tile			

	
; LOOP.ACQUIRE.NEIGHBOR.TILES.PREFER_NORTH
; ;#+$00=north, +$01=south, +$02=east, +$03=west
; ;
; ;.TEST.NORTH
	; LDX #$00
	; LDA MOB.MOVES.BLOCKED+$0				;load the collision status (north) for the current tile. $00 = open. $01 = blocked.  
	; CMP #$00								;is direction blocked?
	; BNE .TEST_SOUTH							;if yes, then next direction test 
	; JSR PATH.OPEN							;if no, then a candidate new neigbor has been found. Evaluate this open path.
	; ;
; .TEST_SOUTH
	; LDX #$01
	; LDA MOB.MOVES.BLOCKED+$1				;load the collision status (south) for the current tile. $00 = open. $01 = blocked.  
	; CMP #$00								;is direction blocked?
	; BNE .TEST_EAST							;if yes, then next direction test 
	; JSR PATH.OPEN							;if no, then a candidate new neigbor has been found. Evaluate this open path.
	; ;
; .TEST_EAST
	; LDX #$02
	; LDA MOB.MOVES.BLOCKED+$2				;load the collision status (east) for the current tile. $00 = open. $01 = blocked.  
	; CMP #$00								;is direction blocked?
	; BNE .TEST_WEST							;if yes, then next direction test 
	; JSR PATH.OPEN							;if no, then a candidate new neigbor has been found. Evaluate this open path.
	; ;
; .TEST_WEST
	; LDX #$03
	; LDA MOB.MOVES.BLOCKED+$3				;load the collision status (west) for the current tile. $00 = open. $01 = blocked.  
	; CMP #$00								;is direction blocked?
	; BNE .NEXT.PATH.TILE_STEP				;if yes, then next direction test 
	; JSR PATH.OPEN							;if no, then a candidate new neigbor has been found. Evaluate this open path.
	; ;
	; ;
; .NEXT.PATH.TILE_STEP								
	; JMP NEXT.PATH.TILE						;if yes, then get next path tile			

	
PATH.OPEN
@START				
.EVALUATE.ACQUIRED.NEIGHBOR
@START	


;CALCULATE THE RMAP.X/Y OF THE PROSPECTIVE NEIGHBOR TILE
;(the calculation is RMAP.X/Y of current tile +/- an adjustment based on the direction of the open path from the curren tile)
;(store result of calculation in NPC.PATHFINDER.NEIGHBOR.X/Y for use by the next section)

	;X-REG CONTAINS THE DIRECTION CODE FOR THE OPEN PATH: #$00=north, $01=south, $02=east, $03=west,  $04 = NO MOVE
	CPX #$00
	BEQ	.NEIGHBOR.NORTH
	CPX #$01
	BEQ .NEIGHBOR.SOUTH
	CPX #$02
	BEQ	.NEIGHBOR.EAST

;.NEIGHBOR.WEST $03 ASSUMED
.NEIGHBOR.WEST
	LDA NPC.PATHFINDER.CURRENT.TILE.X		
	SEC
	SBC #$01
	STA NPC.PATHFINDER.NEIGHBOR.X
	
	LDA NPC.PATHFINDER.CURRENT.TILE.Y	
	JMP .IS.NEIGHBOR.NEW

.NEIGHBOR.EAST	
	LDA NPC.PATHFINDER.CURRENT.TILE.X		
	CLC
	ADC #$01
	STA NPC.PATHFINDER.NEIGHBOR.X
	
	LDA NPC.PATHFINDER.CURRENT.TILE.Y	
	JMP .IS.NEIGHBOR.NEW
	
.NEIGHBOR.NORTH	
	LDA NPC.PATHFINDER.CURRENT.TILE.X		
	STA NPC.PATHFINDER.NEIGHBOR.X
	
	LDA NPC.PATHFINDER.CURRENT.TILE.Y
	SEC
	SBC #$01		
	JMP .IS.NEIGHBOR.NEW

.NEIGHBOR.SOUTH	
	LDA NPC.PATHFINDER.CURRENT.TILE.X		
	STA NPC.PATHFINDER.NEIGHBOR.X
	
	LDA NPC.PATHFINDER.CURRENT.TILE.Y
	CLC
	ADC #$01	
	;**FALLS THROUGH**	

	
.IS.NEIGHBOR.NEW
;Determine if neighbor is new. If the neighbor has already been 
;discovered, do not add the neighbor again.
	
	STA NPC.PATHFINDER.NEIGHBOR.Y			;Y-axis is already in ACC	

		;**OPT** Move closed set array to aux memory and/or convert it to using 1 bit per coordinate instead of a full byte.

		
;SAVE REGISTERS
	STX ACQUIRE.LOOP.COUNTER
	STY SEARCH.PATHS.LO_INDEX				;restores index

;CONVERT CANDIDATE NEIGHBOR RMAP.X/Y TO BUILDING MAP GRID	
	LDA NPC.PATHFINDER.NEIGHBOR.X			;load neighbor x-axis
	SEC
	SBC #CLOSED_SET.X.ADJ
	STA NEIGHBOR.ADJUSTED.X					;Convert to Building Map Grid
	
	
	LDA NPC.PATHFINDER.NEIGHBOR.Y			;load neighbor y-axis
	SEC
	SBC #CLOSED_SET.Y.ADJ
	STA NEIGHBOR.ADJUSTED.Y					;Convert to Building Map Grid
	

;GET ADDRESS OF CLOSED_SET ARRAY ELEMENT FOR CANDIDATE NEIGHBOR
	LDA #NPC.PATHFINDER.CLOSED_SET			;get closet_set array base address
	STA NPC.PATHFINDER.CLOSED_SET.POINTER
	LDA /NPC.PATHFINDER.CLOSED_SET
	STA NPC.PATHFINDER.CLOSED_SET.POINTER+$1

				
	LDX NEIGHBOR.ADJUSTED.Y					;get column adjustment
	LDA NPC.PATHFINDER.CLOSED_SET.POINTER
	CLC
	ADC CLOSED_SET.MULTIPLY_TABLE.LO,X	
	STA NPC.PATHFINDER.CLOSED_SET.POINTER
	
	LDA NPC.PATHFINDER.CLOSED_SET.POINTER+$1	
	ADC CLOSED_SET.MULTIPLY_TABLE.HO,X
	STA NPC.PATHFINDER.CLOSED_SET.POINTER+$1

;LOAD CLOSED_SET VALUE	
	LDY NEIGHBOR.ADJUSTED.X					
	LDA (NPC.PATHFINDER.CLOSED_SET.POINTER),Y	

; ;TROUBLESHOOTING HOOK
				; STA TEMP
				; STX SAVED.XREG.LOCAL
				; LDA ITERATION.COUNTER
				; CMP #$01
				; BNE .TEMP1
				; LDX ACQUIRE.LOOP.COUNTER
				; CPX #$01
				; BCC .TEMP1
				; JSR PREP.BRK
				; ;LDA FINAL.PATH.RECORD.COUNTER
				; LDA TEMP
				; LDX CURRENT.TILE.ADJUSTED.X
				; LDY CURRENT.TILE.ADJUSTED.Y
				; ;LDX NPC.PATHFINDER.CLOSED_SET.POINTER
				; ;LDY NPC.PATHFINDER.CLOSED_SET.POINTER+$1
				; ; LDX NPC.PATHFINDER.NEIGHBOR.RMAP
				; ; LDY NPC.PATHFINDER.NEIGHBOR.RMAP+$1
				; BRK
; .TEMP1
				; LDX SAVED.XREG.LOCAL
				; LDA TEMP
				

	CMP #$00										;is candidate neighbor marked as closed? (already searched)
	BEQ .NOT.IN.CLOSED_SET							;if no, add prospective neighbor to the search array and priority que 
;NEIGHBOR IN CLOSED SET
.IN.CLOSED.SET
	;RESTORE REGISTERS THEN TEST EXIT
	LDX ACQUIRE.LOOP.COUNTER
	LDY SEARCH.PATHS.LO_INDEX						;restores index
	JMP .EXIT_TEST									;if no, proceed to next prospecive neighbor 


.NOT.IN.CLOSED_SET
;CANDIDATE NEIGHBOR NOT IN CLOSED SET
;(therefore, candidate neighbor is approved as a new neighbor path tile)

.ADD.TILE.TO.CLOSED_SET
;To prevent tiles from being seached for neighbors twice, 
;which can cause endless loops, the current tile is added
;to the closed_set array. Also prevents neighbor tiles from being
;added if a path to the neighbor tile is already known, which
;increases efficiency. 
	

	LDA #$01
	STA (NPC.PATHFINDER.CLOSED_SET.POINTER),Y	;the pointer already has the address of the closed_set array element for the new neighbor. This STA writes $01 to the address, marking the neighbor as closed. 
	

	;RESTORE REGISTERS
	LDX ACQUIRE.LOOP.COUNTER
	LDY SEARCH.PATHS.LO_INDEX				;restores index
	
	;**FALLS THROUGH**	
	
.ADD.ACQUIRED.NEIGHBOR		
;ADD ACQUIRED NEIGHBOR TILE TO NPC.PATHFINDER.SEARCH.PATHS			


	LDA NPC.PATHFINDER.TILE_NUMBER.TALLY
	BEQ .ERROR.TILE_NUMBER.OVERFLOW			;If TILE_NUMBER.TALLY is $00, it is because it flipped over. It is init to $01 at the start of the this subroutine (NPC.PATHFINDER).
	STA (NPC.PATHFINDER.SEARCH.PATHS.POINTER),Y ;Byte0. assign path tile# to this acquired neigbor tile. 


			
	INY										;Increment search path index

	LDA NPC.PATHFINDER.CURRENT.TILE			
	STA (NPC.PATHFINDER.SEARCH.PATHS.POINTER),Y	;Byte1. set source path tile#	

	INY										;Increment search path index
	
	LDA NPC.PATHFINDER.CURRENT.TILE.X		
	STA (NPC.PATHFINDER.SEARCH.PATHS.POINTER),Y	;Byte2. set RMAP.X of source tile

	INY										;Increment search path index

	LDA NPC.PATHFINDER.CURRENT.TILE.Y		
	STA (NPC.PATHFINDER.SEARCH.PATHS.POINTER),Y	;Byte3. set RMAP.Y of source tile

	INY										;Increment search path index

;SET BYTES 4-5
;(calculate the rmap.x/y of the new neighbor tile)

	LDA NPC.PATHFINDER.NEIGHBOR.X
	STA (NPC.PATHFINDER.SEARCH.PATHS.POINTER),Y	;Byte 4. Neighbor tile RMAP.X

	INY											;Increment search path index

	LDA NPC.PATHFINDER.NEIGHBOR.Y
	STA (NPC.PATHFINDER.SEARCH.PATHS.POINTER),Y	;Byte 5. Neighbor tile RMAP.Y
	
	;**FALLS THROUGH**	


.NEXT.RECORD
	INC SEARCH.PATHS.NEIGHBORS_ADDED.COUNTER
	
	INY										;Increment search path index to next record
	INY										;Increment search path index to next record
	INY										;Increment search path index to next record
	BNE .CALCULATE.DISTANCE					;Did index flip to $00?
											;If yes then check for overflow and increment HO byte of base address index
;CHECK FOR SEARCH ARRAY OVERFLOWS
	LDA NPC.PATHFINDER.SEARCH.PATHS.POINTER+$1
	CMP /SEARCH.PATHS.HIMEM
	BEQ .ERROR.SEARCH_ARRAY.OVERFLOW
	
	INC NPC.PATHFINDER.SEARCH.PATHS.POINTER+$1		;increment HO Byte of base address index
	JMP .CALCULATE.DISTANCE
	
.ERROR.SEARCH_ARRAY.OVERFLOW
;Overflow error reported by .ADD.ACQUIRED.NEIGHBOR in NPC.PATHFINDER,
;due to the NPC.PATHFINDER.SEARCH.PATHS array is full likely because
;the destination is too far away from the sprite's starting position. 
;
;This is factor not just of the distance in tiles but also considering
;what dead ends the algorithm may get drawn into before reaching the final
;destination. Dead ends that require a lot of tiles to be searched before
;the algorithm identifies the dead end can really add a lot of neigbors to the search path array.
	LDA TEXT
	LDA $C082
	BRK

.ERROR.TILE_NUMBER.OVERFLOW
;Overflow error reported by .ADD.ACQUIRED.NEIGHBOR in NPC.PATHFINDER.	
;due to NPC.PATHFINDER.TILE_NUMBER.TALLY flipping over to $00.
;The causes for this are along the same line of what is described
;in	.ERROR.SEARCH_ARRAY.OVERFLOW
	LDA TEXT
	LDA $C082
	BRK
	
@END


.CALCULATE.DISTANCE			
@START
;CALCULATE DISTANCE FROM DESTINATION
	LDA NPC.PATHFINDER.NEIGHBOR.X
	CMP NPC.PATHFINDER.DESTINATION.TILE.X
	BCC .MOB.MO_X_LESS
	
	SEC
	SBC NPC.PATHFINDER.DESTINATION.TILE.X
	STA MAP_OBJECTS.X_ADJ

	JMP .MOB.YTEST

.MOB.MO_X_LESS
	LDA NPC.PATHFINDER.DESTINATION.TILE.X
	SEC
	SBC NPC.PATHFINDER.NEIGHBOR.X
	STA MAP_OBJECTS.X_ADJ
	;**FALLS THROUGH**
	
.MOB.YTEST	
	LDA NPC.PATHFINDER.NEIGHBOR.Y
	CMP NPC.PATHFINDER.DESTINATION.TILE.Y
	BCC .MOB.MO_Y_LESS
	
	SEC
	SBC NPC.PATHFINDER.DESTINATION.TILE.Y
	STA MAP_OBJECTS.Y_ADJ
		
	JMP .FINAL.DISTANCE.CALC

.MOB.MO_Y_LESS
	LDA NPC.PATHFINDER.DESTINATION.TILE.Y
	SEC
	SBC NPC.PATHFINDER.NEIGHBOR.Y	
	STA MAP_OBJECTS.Y_ADJ
	;**FALLS THROUGH**
	
.FINAL.DISTANCE.CALC
	LDA MAP_OBJECTS.X_ADJ					;finalize distance calculation
	CLC
	ADC MAP_OBJECTS.Y_ADJ
	STA NPC.PATHFINDER.CURRENT.DISTANCE	
	BEQ .FINAL.DISTANCE.COMPLETE			;if distance is $00 then the destination has been found, no need to add weight to the distance value. 
	
;WEIGHT DISTANCE FOR NON-FLOOR/ROAD TILES	
;(this is so NPCs will use road/floor tiles to get to their
;destination if possible)
.ROAD.TEST1
	LDA MOB.ADJACENT_TILES,X
	CMP #TILE_ID.ROAD.GRE1
	BCS .ROAD.TEST2
	JMP .FLOOR.TEST
	;JMP .NOT.ROAD
.ROAD.TEST2
	CMP #TILE_ID.ROAD.LT1
	BCC .FINAL.DISTANCE.COMPLETE  ;tile is road, the final distance value previously saves stands.
	;**FALLS THROUGH**
.FLOOR.TEST
	CMP #TILE_ID.FLOOR.GRE1
	BCS .FLOOR.TEST2
	JMP .NOT.ROAD_OR_FLOOR
.FLOOR.TEST2
	CMP #TILE_ID.FLOOR.LT1
	BCC .FINAL.DISTANCE.COMPLETE  ;tile is road, the final distance value previously saves stands.
	;**FALLS THROUGH**	
.NOT.ROAD_OR_FLOOR
	LDA NPC.PATHFINDER.CURRENT.DISTANCE
	CLC
	ADC PATHFINDER.STREET.PREFERENCE
	STA NPC.PATHFINDER.CURRENT.DISTANCE
		

				; LDA #$AA
				; JSR PREP.BRK
				; BRK
	
.FINAL.DISTANCE.COMPLETE
			
				; STA TEMP
				; LDA ITERATION.COUNTER
				; CMP #$02
				; BNE .TEMP
				; LDA #$AB
				; LDX #MOB.ADJACENT_TILES
				; LDY /MOB.ADJACENT_TILES
				; JSR PREP.BRK
				; BRK
; .TEMP
				; LDA TEMP
				
	;**FALLS THROUGH**

@END

.ADD_PRIORITY.QUE
@START
;-What happens if que overflows?
;if the priority que if full after a record add, the index isn't incremented which
;will result in the next record overwritting the last record in the priority que. 
;Since the que is sorted, it seems unlikely that any records at the bottom of a large que
;would ever be needed to find a path. 
;
;The safeguard for this is the verification at the top of NEXT.PATH.TILE
;to ensure that NPC.PATHFINDER.PRIORITY.QUE.RECORD_TALLY >= 1. If
;a critical record in the priority que was overwritten, the result
;should be that the algorithm tried (and deletes) all records in the
;priority que without finding the destination tile. 

			

				
	LDA NPC.PATHFINDER.CURRENT.DISTANCE
	CMP PATH_TILE.SHORTEST.DISTANCE			;is the current tile closer to the destination than the the top record in the que?
	BCS .ADD.TO.BOTTOM						;if no, then add the current tile to the bottom of the que
											;if yes, then replace the top record with the current tile
;COPY THE TOP RECORD TO A SWAP VARIABLE 	
	LDA NPC.PATHFINDER.PRIORITY.QUE+$2		;load top record distance field
	STA PATHFINDER.SWAP.DISTANCE			 
	
	LDA NPC.PATHFINDER.PRIORITY.QUE+$3		;load top record path tile number field
	STA PATHFINDER.SWAP.TILE_NUMBER	

	LDA NPC.PATHFINDER.CURRENT.DISTANCE		
	STA NPC.PATHFINDER.PRIORITY.QUE+$2		;save top record distance field
	STA PATH_TILE.SHORTEST.DISTANCE			;update the variable which holds the distance of the top que record. This is necessary because the distance field of the top que record gets clobbered at the top of outer loop when the record is marked as processed. The reason why the record is marked as processed before processing occurs is explained in the comments in the outer loop code section.  
	
	LDA NPC.PATHFINDER.TILE_NUMBER.TALLY	
	STA NPC.PATHFINDER.PRIORITY.QUE+$3		;save top record path tile number field
		
	INC PATHFINDER.SHORTEST.DISTANCE.COUNTER ;whenever the top record is overwritten by a new record with a shorter distance, this variable is incremented so that the sort routine can determine if it should run. 

	
;ERROR CHECK FOR QUE UNDERFLOW 	
	LDA NPC.PATHFINDER.PRIORITY.QUE.RECORD_TALLY	;verify que has at least 1 real record (in addition to the 2 byte length header, which counts as a record). otherwise the swap would overwrite itself, resulting in no change to the record at the top of the que
	CMP #$02
	BCS .IS.SWAP.PROCESSED
	JMP .INCREMENT.COUNTERS

.IS.SWAP.PROCESSED	
;IS THE TOP QUE RECORD A PROCESSED RECORD?
;(if multiple swaps occur in a given iteration, the top record might not be a processed record)
	LDA PATHFINDER.SWAP.DISTANCE					
	CMP #$FE										;is the swap tile a processed record?
	BNE .EXECUTE.ADD								;if no, add the swap record
	DEC NPC.PATHFINDER.PRIORITY.QUE.RECORD_INDEX	;if yes, don't add the swap record, it is no longer needed and adding it back in anyway causes problems. 
													;   and, decrement the que index so that the default increment later on will net out to zero, the proper result since the que didn't increase in size (1 record replace another)
	JMP .INCREMENT.COUNTERS							
	
.ADD.TO.BOTTOM
	LDA NPC.PATHFINDER.CURRENT.DISTANCE				;the swap variables are used as a hopper so that
	STA PATHFINDER.SWAP.DISTANCE					;.EXECUTE.ADD can be used whether a swap of the top record occurs or not. 
													; i.e. if a swap occurs, the swap variables contain the record to be added to the bottom of the que. If a swap doesn't occur, this section loadeds the swap variables with the new neighbor to be added.
	LDA NPC.PATHFINDER.TILE_NUMBER.TALLY
	STA PATHFINDER.SWAP.TILE_NUMBER
	;**FALLS THROUGH**
	
.EXECUTE.ADD
;ADD PATH TILE TO THE BOTTOM OF THE PRIORITY QUE
	;SETUP INDEX
	STX ACQUIRE.LOOP.COUNTER						;save acquire loop index
	LDA NPC.PATHFINDER.PRIORITY.QUE.RECORD_INDEX	;use the number of records in the que * 2 as the index to add the new record to the end of the que. The first two byte array header for the length of the data portion of the array is built into the record index value. The record index will either point to the last record in the que if a sort occured last iteration, so that a processed record is overwritten. If a sort didn't occur last iteration, then the index points to the the next byte after the last record in the que. 
	ASL ;X2									;do X2 because the record size is $2 bytes. 
	TAX										
	LDA PATHFINDER.SWAP.DISTANCE			;used as a hopper
	STA NPC.PATHFINDER.PRIORITY.QUE+$0,X	;Byte0. distance. [the distance from the current tile to the destination tile (Xc - Xd + Yc - Yd)]. 

	
	LDA PATHFINDER.SWAP.TILE_NUMBER			;used as a hopper
	STA NPC.PATHFINDER.PRIORITY.QUE+$1,X	;Byte1. path tile# of this acquired neigbor tile. 

.INCREMENT.COUNTERS			
;INCREMENT & RESTORE COUNTERS
	INC NPC.PATHFINDER.TILE_NUMBER.TALLY			;increment tally so the next time an open path is found this routine will assign the next path tile # in sequence. 		

	
	LDA NPC.PATHFINDER.PRIORITY.QUE.RECORD_TALLY
	CMP #$7F	;is priority que full? !127 (2bytes * 127 records, + 2 bytes for array data length, = 256, which would flip the tally to $00)
	BCS .RESTORE.LOOP.COUNTER ;if yes, skip increment to the que record tally which will result in the next record written to overwrite the record at the bottom of the priority que. 

	LDA NPC.PATHFINDER.PRIORITY.QUE.RECORD_TALLY
	CLC
	ADC #$01											;a record was added so increment the record tally by one. 
	SEC
	SBC NPC.PATHFINDER.PRIORITY.QUE.PROCESSED.RECORDS	;if a processed record was in the que, it was overwritten by the record added, so deduct one from the que record tally. 
	STA NPC.PATHFINDER.PRIORITY.QUE.RECORD_TALLY
	
	LDA NPC.PATHFINDER.PRIORITY.QUE.PROCESSED.RECORDS
	BEQ .INCREMENT.RECORD.INDEX							;if processed records counter = $00 then no need to decrement it. 
	SBC #$01											;there can be more than one processed record in the que. This line is to make sure the count stays accurate. 
	STA NPC.PATHFINDER.PRIORITY.QUE.PROCESSED.RECORDS
	;**FALLS THROUGH
	
.INCREMENT.RECORD.INDEX
	INC NPC.PATHFINDER.PRIORITY.QUE.RECORD_INDEX		;not always equal to the que record tally. it depends on whether the record will be written after the last record or overwriting the last record. This is controlled by .SORT.PRIORITY_QUE and the code just above it.
		
	
.RESTORE.LOOP.COUNTER		
	LDX ACQUIRE.LOOP.COUNTER							;restore acquire loop index
			
.EXIT_TEST
	

	
; ;TROUBLESHOOTING HOOK
				; STA TEMP
				; LDA ITERATION.COUNTER
				; CMP #$1F
				; BNE .TEMP1
				; CPX #$01
				; BNE .TEMP1
				; JSR PREP.BRK
				; LDA NPC.PATHFINDER.PRIORITY.QUE.RECORD_TALLY
				; LDY NPC.PATHFINDER.PRIORITY.QUE.RECORD_INDEX
				; ;LDY NPC.PATHFINDER.PRIORITY.QUE.PROCESSED.RECORDS
				; ;LDY NPC.PATHFINDER.CURRENT.TILE
				; ; LDX NPC.PATHFINDER.NEIGHBOR.RMAP
				; ; LDY NPC.PATHFINDER.NEIGHBOR.RMAP+$1
				; BRK
; .TEMP1		
				; LDA TEMP
								
	; INX				;increment counter for .LOOP.ACQUIRE.NEIGHBOR.TILES
	; CPX #$04
	; BEQ .EXIT
	RTS
	;JMP LOOP.ACQUIRE.NEIGHBOR.TILES
	
.EXIT

	
	;**FALLS THROUGH

	
@END	
@END

	
NEXT.PATH.TILE					
@START

			; LDA DIRECTION.TEST.COUNTER
			; CMP #$04
			; BNE .TEMP
			; JSR PREP.BRK
			; BRK
;.TEMP




; ; TROUBLESHOOTING HOOK
				; LDA ITERATION.COUNTER
				; CMP #$50
				; BNE .TEMP1
				; JSR PREP.BRK
				; LDA NPC.PATHFINDER.PRIORITY.QUE.RECORD_TALLY
				; LDX NPC.PATHFINDER.PRIORITY.QUE.RECORD_INDEX
				; LDY SEARCH.PATHS.NEIGHBORS_ADDED.COUNTER
				; ;LDY PATHFINDER.SHORTEST.DISTANCE.COUNTER 
				; ;LDY NPC.PATHFINDER.TILE_NUMBER.TALLY
				; ; LDA NPC.PATHFINDER.CURRENT.TILE
				; ; LDX NPC.PATHFINDER.CURRENT.TILE.X
				; ; LDY NPC.PATHFINDER.CURRENT.TILE.Y
				; BRK
; .TEMP1

;ERROR CHECK AND INDEX, COUNTER UPDATES	
@START
.ERROR.CHECK.QUE_SIZE
	LDA NPC.PATHFINDER.PRIORITY.QUE.RECORD_TALLY
	CMP #$02								;is there at least two records in the priority que? (two because the array length in the first two bytes counts as a record)
	BCS .SET.QUE.INDEX.DO_SORT				;if yes, then proceed
	JMP .ERROR.UNDERFLOW.QUE_SIZE			;if no, underflow error has occured
 
.UPDATE.COUNTERS ;AND
;DETERMINE IF SORT SHOULD BE PERFORMED
;(sort is skipped when at least one neighbor per iteration is 
;found which is closer to the destination than the record at the
;top of the que. Each occasion when such a neighbor is 
;found results in an INC to the counter PATHFINDER.SHORTEST.DISTANCE.COUNTER
;Accordingly, if the sort should be skipped, the counter should
;be greater than $00.

	LDA PATHFINDER.SHORTEST.DISTANCE.COUNTER	;was at least one neighbor found with a distance shorter than the top record in the que?
	BEQ	.SORT.PRIORITY_QUE						;if no, sort the que. 
	DEC NPC.PATHFINDER.PRIORITY.QUE.RECORD_TALLY	;a record was processed and deleted so decrement the record tally by one. Note: the current record is only deleted if it is replaced by a record of shorter distance, otherwise the current record is marked as processed (distance = $FE), but remains at the top of the que unti a sort occurs. 
	;**FALLS THROUGH**
	
.SET.QUE.INDEX.NO_SORT
;SET QUE INDEX TO 1 MORE THAN LAST RECORD IN QUE
;(this is because when the que is not sorted, there could be 
;unprocessed records at the bottom of the que and we don't want to overwrite those) 
	
	LDA NPC.PATHFINDER.PRIORITY.QUE.RECORD_TALLY 	;used to calculate que size
	STA	NPC.PATHFINDER.PRIORITY.QUE.RECORD_INDEX
	INC NPC.PATHFINDER.PRIORITY.QUE.RECORD_INDEX
	JMP .LOAD.NEXT.TILE	
	
	
.SET.QUE.INDEX.DO_SORT
;SET QUE INDEX TO LAST RECORD IN QUE
;(this is because when the que is sorted, processed records, which
;are marked by setting their distance to $FE, will be at the bottom 
;of the que and we want to overwrite them)	
	LDA NPC.PATHFINDER.PRIORITY.QUE.RECORD_TALLY	;used to calculate que size	
	STA	NPC.PATHFINDER.PRIORITY.QUE.RECORD_INDEX

	LDA #$01										
	STA NPC.PATHFINDER.PRIORITY.QUE.PROCESSED.RECORDS	;Set the processed record flag. The next time a record is added to the bottom of the que, this value will offset the increment to que size so that the record add is treated as a record overwrite. 
														;This variable used to be a counter in attempt to enable more than 1 processed record per sort to be overwritten, but that caused problems and the variable was converted to a flag, value either $00 or $01.  
	;**FALLS THROUGH**


@END

.SORT.PRIORITY_QUE
@START	
;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA		

;UPDATE QUE LENGTH HEADER	
	LDA NPC.PATHFINDER.PRIORITY.QUE.RECORD_TALLY	;use the number of records in the que * 2 as the index to add the new record to the end of the que. This works even though the 1st two bytes of the array are reserved for the size, in bytes, of the data portion of the array because when a record is erased the 1st byte of the record (distance) is set to $FF. This records gets sorted to the end of the que, so the index will point to this record, which is good because we want to overwrite it. 
	ASL ;X2											;do X2 because the record size is $2 bytes. 
	STA NPC.PATHFINDER.PRIORITY.QUE+$0		;the first two bytes of the array are reserved to store the array data length in bytes
			
			
 	;SETUP POINTER
	LDA #NPC.PATHFINDER.PRIORITY.QUE		
	LDX /NPC.PATHFINDER.PRIORITY.QUE
	
	JMP .INSERTION_SORT
	
.SHELL_SORT
;PARAMETERS: ACC (LO address of data array), X-REG (HO address of data array), array length*
;ENTRANCE: DIRECT
;RETURN: SORTED LIST in the same memory space used prior to the sort
;*the array length, in bytes, must be stored in the first byte of the data array
;        
				ldy #h_high - h_low - 1
                bne .SORT_MAIN         ; Always branch
				
				
.INSERTION_SORT
;PARAMETERS: ACC (LO address of data array), X-REG (HO address of data array), array length*
;ENTRANCE: DIRECT
;RETURN: SORTED LIST in the same memory space used prior to the sort
;*the array length, in bytes, must be stored in the first byte of the data array
;  
				ldy #$0

.SORT_MAIN
;INIT VALUES
;
;j(2) = input address.LO/HO
;in-address(2) = input address.LO/HO
;arr_start(2) = input address.LO/HO
;arr_length(2) = # of bytes in array 
;
;input address.LO/HO refers to the address of the data array being sorted,
;the HO/LO address of which is loaded into the ACC register and X-REG in 
;the DRIVER section above.
;      
			
				sty h_start_index
                cld
                sta J
                sta in_address


                clc
                adc #$2
                sta arr_start

                stx j+$1
                stx in_address+$1
												
                txa
                adc #$0
                sta arr_start+$1

                ldy #$0
                lda (j),y
                sta arr_length

                clc
                adc arr_start
                sta arr_end

                iny
                lda (j),y
                sta arr_length+$1

                adc arr_start+$1
                sta arr_end+$1
				
;original author comment:   for (h=1; h < length; h=3*h+1);

                ldx h_start_index     ; Start with highest value of h
.CHK_PREV_H  
;seems to test the length of the data array to be sorted and
;initiate an exit from the subroutine if no sort is needed
;because the data array is 0 or 1 bytes in length
;    
				lda h_low,x
                cmp arr_length
                lda h_high,x
                sbc arr_length+$1
                bcc .END_OF_INIT       ; If h < array_length, we've found the right h
                dex
                bpl .CHK_PREV_H
                rts                   ; array length is 0 or 1. No sorting needed.

.END_OF_INIT     
				inx
                stx h_index

;   while (h=(h-1)/3)

.H_LOOP          
				dec h_index
                bpl .GET_H
                JMP .SORT.COMPLETE     ; All done!

.GET_H 
;some values I traced on the first iteration of the loop
;h   = input address.LO + $2[from table]
;h+1 = $0 [from table]
;i	 = input address.LO + $2[from table]
;i+1 = input address.HO 

				ldy h_index
                lda h_low,y
                sta h
                clc
                adc in_address        ; ( in_address is arr_start - 2)
                sta i
                lda h_high,y
                sta h+$1
                adc in_address+$1
                sta i+$1

; for (i=h, j=i, v=arr[i]; i<=length; arr[j+h]=v, i++, j=i, v=arr[i])

.I_LOOP 
;This section is where the index to the data array, the 
;variable i(2), is incremented. Some data is loaded into
;the variable v(2) which is used later in .EXIT_J_LOOP
;
;I think that the amount of the increment controls the record 
;size. Right now it is a two byte record. To increase the record
;size I think the increment 
;would get increased to the number of bytes in the new record
;size. Additionally code would need to be adde to copy the additionl
;bytes in the new record size. 
;
;
;some values I traced on the first iteration of the loop
;i	 = input address.LO + $2[from table] +$2
;i+1 = input address.HO 
;j	 = input address.LO + $2[from table] +$2
;j+1 = input address.HO  
;v(2)= whe the input address was $7000, v = $7004, v+1 & v = $7005
;v_plus_1(2) just adds $01 to v(2) using a 16-bit add.


				lda i
                clc
                adc #$2			;array index increment
                sta i
                sta j
                lda i+$1
                adc #$0				
                sta i+$1
                sta j+$1

                ldx i
                cpx arr_end
                lda i+$1
                sbc arr_end+$1
                bcs .H_LOOP

                ldy #$0
                lda (j),y
                sta v
                clc
                adc #$1
                sta v_plus_1
                iny
                lda (j),y
                sta v+$1
                adc #0
                bcs .I_LOOP            ; v=$ffff, so no j-loop necessary
                sta v_plus_1+$1

                dey                   ; Set y=0

;         while((j-=h) >= 0 && arr[j] > v)

.J_LOOP 
;some values I traced on the first iteration of the loop
;h		= 2
;j 		= input address.LO +$2[from table]
;j+1 	= input address.HO
;j_plus_h = input address.HO
;x-reg  = j
;h + 1 = $00 [from table]
;j + 1 = input address.HO
;         
				lda j
                sta j_plus_h
                sec
                sbc h
                sta j
                tax
                lda j+$1
                sta j_plus_h+$1
                sbc h+$1
                sta j+$1

; Check if we've reached the bottom of the array

                bcc .EXIT_J_LOOP
                cpx arr_start
                sbc arr_start+$1
                bcc .EXIT_J_LOOp


.DO.COMPARISON
;Original author comment: Do the actual comparison:  arr[j] > v
;
;This is the code section where a comparison test is done on
;a pair of records to shape the order of records in the array.
;
;(j),y 		= record 0
;vplus_1(2) = record 1
;
;The 16-bit version of ths routine compares both bytes
;of record0 and record1 and branches to .EXIT_J_LOOP if
;record 1 is less.
;
;The 8-bit version of ths routine compares the LO byte
;of record0 and record1 and branches to .EXIT_J_LOOP if
;record 1 is less.
;
;The code below has notes on which lines can be commented/uncommented
;to enable/disable 16-bit. Note that enabling 16-bit in this
;way results in a 1 field record format as both bytes are used
;for one 16-bit field.
; 
;some values I traced on the first iteration of the loop:
;
;$7002 (the LO byte (j),y), $7003(the HO byte (j),y) was compared to v_plus_1 (value from $7004) and v_plus_1+1 (value from $9005) 
;
;
;

                lda (j),y
                tax
                iny                   ; Set y=1
                ; lda (j),y			  ;16-BIT/8-BIT: uncomment this line for 16-bit, comment it out for 8-bit

                cpx v_plus_1
                ; sbc v_plus_1+$1	  ;16-BIT/8-BIT: uncomment this line for 16-bit, comment it out for 8-bit	
                bcc .EXIT_J_LOOP		;branch if vplus is less than (j),y,


.MOVE.DATA
;original author comment: arr[j+h]=arr[j]
;
;some values I traced on the first iteration of the loop:
;
;Record0: $7002-7003
;Record1: $7004-7005
;
;Another way to summarize this section is, using the values traced
;on the first iteration:
;
;LDA $7003  (y=1)
;STA $7005  (y=1)
;
;LDA $7002  (y=0)
;STA $7004  (y=0)

						
                lda (j),y
                sta (j_plus_h),y
                dey                   ; Set y=0
                txa
                sta (j_plus_h),y
				

							
                bcs .J_LOOP            ; Always branch


							

.EXIT_J_LOOP     
;original author comment:       for (i=h, j=i, v=arr[i]; i<length; arr[j+h]=v, i++, j=i, v=arr[i])  ***  arr[j+h]=v part
;
;using values traced in the first iteration, this section
;can be summarized as
;
;LDA data previously loaded in .I_LOOP into the variable v(2)
;STA $7002
;STA $7003 
;
				lda v
                ldy #$0
                sta (j_plus_h),y
                iny
                lda v+$1
                sta (j_plus_h),y
 

			
				JMP .I_LOOP
				
.SORT.COMPLETE				
;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX
	
@END
		
.LOAD.NEXT.TILE	
@START
;LOAD NEXT TILE FROM PRIORITY QUE		

		
		; ; TROUBLESHOOTING HOOK
				; LDA ITERATION.COUNTER
				; CMP #$52
				; BNE .TEMP1
				; JSR PREP.BRK
				; LDA NPC.PATHFINDER.PRIORITY.QUE.RECORD_TALLY
				; LDX NPC.PATHFINDER.PRIORITY.QUE.RECORD_INDEX
				; LDY SEARCH.PATHS.NEIGHBORS_ADDED.COUNTER
				; ;LDY PATHFINDER.SHORTEST.DISTANCE.COUNTER 
				; ;LDY NPC.PATHFINDER.TILE_NUMBER.TALLY
				; ; LDA NPC.PATHFINDER.CURRENT.TILE
				; ; LDX NPC.PATHFINDER.CURRENT.TILE.X
				; ; LDY NPC.PATHFINDER.CURRENT.TILE.Y
				; BRK
; .TEMP1


			
	;*****MULTIPLICATION SUBROUTINE*******
	STY SEARCH.PATHS.LO_INDEX				;save index
		

	LDY NPC.PATHFINDER.PRIORITY.QUE+$2		;load distance field of the record at the top of the priority que
	CPY #$FE								;is a processed record at the top of the que? (when a record is processed the distance field is set to $FE. Procssed records are not always overwritten immediately, but should get sorted to the bottom of the que. If a processed record is at the top of the que when .LOAD.NEXT.TILE (this routine) is called then something went wrong with the que order. One possible cause is that a sort failed because the que size index got messed up. 
	BNE .CONTINUE							;if no, continue to load the next path tile
	JMP .ERROR.QUE.ORDER					;if yes, report error

.CONTINUE	
	LDA NPC.PATHFINDER.PRIORITY.QUE+$3		;load path tile #
	STA NPC.PATHFINDER.CURRENT.TILE
	
	;CALCULATE OFFSET TO BASE ADDRESS
	;(offset = path tile # * record size ($08))		
        STA MULPLR
        LDA #$00
        STA MULPLR+$1
		;
        LDA #SEARCH.PATHS.RECORD.SIZE
        STA MULCND
        LDA #$00
        STA MULCND+$01
		
        JSR MLP.16.NO_BCD
		
		LDA RESULT
		STA TEMP16
		LDA RESULT+$1
		STA TEMP16+$1

	;ADD BASE ADDRESS, OFFSET & ADJUSTMENT
	LDA	#NPC.PATHFINDER.SEARCH.PATHS
	CLC
	ADC TEMP16
	STA NPC.PATHFINDER.SEARCH.PATHS.POINTER2
	
	LDA	/NPC.PATHFINDER.SEARCH.PATHS
	ADC TEMP16+$1
	STA NPC.PATHFINDER.SEARCH.PATHS.POINTER2+$1	
	
	LDY #$04
	LDA (NPC.PATHFINDER.SEARCH.PATHS.POINTER2),Y
	STA NPC.PATHFINDER.CURRENT.TILE.X

	
	INY	;advance to byte $05
	
	LDA (NPC.PATHFINDER.SEARCH.PATHS.POINTER2),Y
	STA NPC.PATHFINDER.CURRENT.TILE.Y

	LDY SEARCH.PATHS.LO_INDEX				;restores index
	;no restore of x-reg needed because it will be reinit at start of
	;outer loop.  		
		
		
	; ;*******MULTIPLICATION TABLE VERSION*********			
	; STY SEARCH.PATHS.LO_INDEX				;save index
	;
	; LDY NPC.PATHFINDER.PRIORITY.QUE+$2		;load distance field of the record at the top of the priority que
	; CPY #$FE								;is a processed record at the top of the que? (when a record is processed the distance field is set to $FE. Procssed records are not always overwritten immediately, but should get sorted to the bottom of the que. If a processed record is at the top of the que when .LOAD.NEXT.TILE (this routine) is called then something went wrong with the que order. One possible cause is that a sort failed because the que size index got messed up. 
	; BEQ .ERROR.QUE.ORDER					;if yes, report error
	;										; ;if no, continue to load the next path tile
	;										
	; LDY NPC.PATHFINDER.PRIORITY.QUE+$3		;load path tile # field of the record at the top of the priority que
	; STY NPC.PATHFINDER.CURRENT.TILE			
	;
	;
	; LDA	#NPC.PATHFINDER.SEARCH.PATHS
	; CLC
	; ADC NPC.PATHFINDER.MULTIPLY_TABLE.LO,Y			;**OPT** Memory. Maybe this table could be stored in aux memory and copied down by this routine into the main memory swap space (where combat, inventory etc will be loaded). Would that be much faster than just using my multiply 16 routine? Also look at the multiply 16 routine I got from a book, that one might be much more efficent, it's limitatin was that it doesn't work with BCD.
; ;	ADC TEMP										;add lo byte adjustment, needed if if NPC.PATHFINDER.SEARCH.PATHS uses multiple pages ($4 bytes per page are skipped because the record size ($06) is not a page divisor that results in a whole number) 
	; STA NPC.PATHFINDER.SEARCH.PATHS.POINTER2
	;
	; LDA	/NPC.PATHFINDER.SEARCH.PATHS
	; ADC NPC.PATHFINDER.MULTIPLY_TABLE.HO,Y
	; STA NPC.PATHFINDER.SEARCH.PATHS.POINTER2+$1	
	;
	; LDY #$04
	; LDA (NPC.PATHFINDER.SEARCH.PATHS.POINTER2),Y
	; STA NPC.PATHFINDER.CURRENT.TILE.X
;
	;
	; INY	;advance to byte $05
	;
	; LDA (NPC.PATHFINDER.SEARCH.PATHS.POINTER2),Y
	; STA NPC.PATHFINDER.CURRENT.TILE.Y
;
;
	;			
; ;RESTORE REGISTERS
	; LDY SEARCH.PATHS.LO_INDEX				;restores index
	; ;no restore of x-reg needed because it will be reinit at start of
	; ;outer loop.  		
		
	INC ITERATION.COUNTER ;TROUBLESHOOTING

				
;IS CURRENT TILE THE DESTINATION TILE?
	LDA NPC.PATHFINDER.CURRENT.TILE.X
	CMP NPC.PATHFINDER.DESTINATION.TILE.X
	BNE .OUTER.LOOP_STEP					;if no, continue search
	
	LDA NPC.PATHFINDER.CURRENT.TILE.Y
	CMP NPC.PATHFINDER.DESTINATION.TILE.Y	
	BNE .OUTER.LOOP_STEP	

	JMP ASSEMBLE.FINAL.PATH					;if yes, exit search loop

	
.OUTER.LOOP_STEP

.AUTO_ABORT.CHECK	
	LDA ITERATION.COUNTER					
	CMP #KEYPRESS.ABORT.ITERATIONS		;has the auto-abort threashold (# of iterations since pathfinder was called) been reached?
	BCS .AUTO_ABORT						;if no, automatically continue loop
										;if yes, auto-abort
											
	; LDA KB_BUFFER					;LOAD NEXT KEY PRESS FROM BUFFER. WAS KEY PRESSED?
    ; BMI .KEYPRESS.ABORT				;IF YES, SKIP COPY.SCREEN WHICH IS PREP FOR ANIMATION. 

	JMP OUTER.LOOP

.AUTO_ABORT
;SAVE VARIABLES
	LDA	NPC.PATHFINDER.SEARCH.PATHS.POINTER
	STA SEARCH.PATHS.POINTER.SAVED
	LDA	NPC.PATHFINDER.SEARCH.PATHS.POINTER+$1
	STA SEARCH.PATHS.POINTER.SAVED+$1
	
;SET ABORT FLAG
	LDA #$01						;$00 = No abort, $01 = aborted path is pending completion
	STA NPC.PATHFINDER.ABORT_FLAG

;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX
	
	RTS
	
	
.ERROR.UNDERFLOW.QUE_SIZE
;NEXT.PATH.TILE reports a priority que size less than 1. 
;
;One possible scenario where this could happen is if the
;priority que fills up and starts overwriting records at the
;bottom of the que (which it does by design) and one of those
;records turns out to be critical to finding the path to the
;destination. 
;
;The result in that scenario should be that the algorithm tried (and deleted) 
;all records in the priority que without finding the destination tile,
;resulting in a que size of 0, and triggering this error.
	LDA TEXT	
	LDA $C082
	BRK
	
.ERROR.QUE.ORDER
;.LOAD.NEXT.TILE reports that a record with a distance value 
;of $FE is at the top of the que, which indicates a processed record.
;when a record is processed the distance field is set to $FE. 
;Procssed records are not always overwritten immediately, but 
;should get sorted to the bottom of the que. If a processed 
;record is at the top of the que when .LOAD.NEXT.TILE is called
;then something went wrong with the que order. One possible 
;cause is that a sort failed because the que size index got 
;messed up. 
	LDA TEXT	
	LDA $C082
	LDX #$AA
	LDY #$AA
	BRK
	
@END



		; STA TEMP
		; ; LDA CALLED_BY.DRAW.SCREEN
		; ; CMP #$01
		; ; BEQ .TEMP
		; LDA NPC.MOVE.COUNTER
		; BEQ .TEMP
		; LDA ITERATION.COUNTER
		; CMP #$0B
		; BCC .TEMP
		; LDA PATHFINDER.SPRITE.RECORD+$A
		; LDX #PATHFINDER.SPRITE.RECORD
		; LDY /PATHFINDER.SPRITE.RECORD

		; JSR PREP.BRK
		; BRK			
; .TEMP
		; LDA TEMP


	
ASSEMBLE.FINAL.PATH		
@START			

;NPC.PATHFINDER.SEARCH.PATHS.POINTER2 points to the same record
;it pointed to before control was passed to ASSEMBLE.FINAL.PATH,
;which is the record that contains a source path tile x,y = the destination x,y

		
;INIT VARIABLES
	LDA #$02
	STA FINAL.PATH.RECORD.COUNTER					;init to #$02 to reflect the 2 byte header, plus two path records ($2 bytes each) written before the loop starts
	
;WRITE DESTINATION TILE	AS THE 1ST RECORD IN FINAL PATH
;(doing this write before the loop because the data is accessible without an indexed lookup to the search path array)

	LDA NPC.PATHFINDER.DESTINATION.TILE.X
	STA NPC.PATHFINDER.FINAL.PATH+$2

	LDA NPC.PATHFINDER.DESTINATION.TILE.Y
	STA NPC.PATHFINDER.FINAL.PATH+$3	

;WRITE THE X,Y OF THE DESTINATION TILE'S SOURCE TILE AS THE 2ND RECORD IN FINAL PATH
;(doing this write before the loop because the data is accessible without recalculating the index to the search path array)
	LDY #$03
	LDA (NPC.PATHFINDER.SEARCH.PATHS.POINTER2),Y
	STA NPC.PATHFINDER.FINAL.PATH+$4					;write Y-AXIS
	
	DEY	;back up to byte $2 of the search path array record
	
	LDA (NPC.PATHFINDER.SEARCH.PATHS.POINTER2),Y
	STA NPC.PATHFINDER.FINAL.PATH+$5					;write X-AXIS
	
		
;SAVE THE SOURCE TILE # OF DESTINATION TILE
;(becomes the first index used by the loop below)
	DEY	;back up to byte $1 of the search path array record
	LDA (NPC.PATHFINDER.SEARCH.PATHS.POINTER2),Y
	STA NPC.PATHFINDER.NEXT.SOURCE_TILE

	LDX #$06	;init NPC.PATHFINDER.FINAL.PATH index
.LOOP.FINAL.PATH
;CALCULATE NPC.PATHFINDER.SEARCH.PATHS HO/LO INDEX

;*****MULTIPLICATION SUBROUTINE VERSION******
		;CALCULATE OFFSET TO BASE ADDRESS
		;(offset = path tile # * record size ($08))
		LDA NPC.PATHFINDER.NEXT.SOURCE_TILE		
        STA MULPLR
        LDA #$00
        STA MULPLR+$1
		;
        LDA #SEARCH.PATHS.RECORD.SIZE
        STA MULCND
        LDA #$00
        STA MULCND+$01
		;
        JSR MLP.16.NO_BCD
	;	
		LDA RESULT
		STA TEMP16
		LDA RESULT+$1
		STA TEMP16+$1
	;ADD BASE ADDRESS AND OFFSET
	LDA	#NPC.PATHFINDER.SEARCH.PATHS
	CLC
	ADC TEMP16
	STA NPC.PATHFINDER.SEARCH.PATHS.POINTER2
	;
	LDA	/NPC.PATHFINDER.SEARCH.PATHS
	ADC TEMP16+$1
	STA NPC.PATHFINDER.SEARCH.PATHS.POINTER2+$1
	

	
	
	; ;*******MULTIPLICATION TABLE VERSION*********
	; LDY NPC.PATHFINDER.NEXT.SOURCE_TILE				;load source path tile # of the last path tile in the final path to destination. 
	;
	; LDA	#NPC.PATHFINDER.SEARCH.PATHS
	; CLC
	; ADC NPC.PATHFINDER.MULTIPLY_TABLE.LO,Y			;**OPT** Memory. Maybe this table could be stored in aux memory and copied down by this routine into the main memory swap space (where combat, inventory etc will be loaded). Would that be much faster than just using my multiply 16 routine? Also look at the multiply 16 routine I got from a book, that one might be much more efficent, it's limitatin was that it doesn't work with BCD.
	; STA NPC.PATHFINDER.SEARCH.PATHS.POINTER2
	; LDA	/NPC.PATHFINDER.SEARCH.PATHS
	; ADC NPC.PATHFINDER.MULTIPLY_TABLE.HO,Y
	; STA NPC.PATHFINDER.SEARCH.PATHS.POINTER2+$1	
	
	
;GET NEXT SOURCE TILE IN PATH TO DESTINATION
	LDY #$01
	LDA (NPC.PATHFINDER.SEARCH.PATHS.POINTER2),Y
	STA NPC.PATHFINDER.NEXT.SOURCE_TILE

;WRITE THE NEXT SOURCE TILE X,Y AS NEXT FINAL PATH RECORD	
	INY	;advance to byte $02 of search path array record
	LDA (NPC.PATHFINDER.SEARCH.PATHS.POINTER2),Y
	STA NPC.PATHFINDER.FINAL.PATH,X
	
	INX	;advance to byte $01 of final path array record
	INY	;advance to byte $03 of search path array record
	
	LDA (NPC.PATHFINDER.SEARCH.PATHS.POINTER2),Y
	STA NPC.PATHFINDER.FINAL.PATH,X
	
;IS NEXT SOURCE TILE THE NPC'S CURRENT MAP POSITION?
	;ACC already has Y-axis of next source path tile
	CMP PATHFINDER.SPRITE.RECORD+$9		;Y-axis of the NPC's current map position
	BNE .NEXT.RECORD	
				
	DEY
	LDA (NPC.PATHFINDER.SEARCH.PATHS.POINTER2),Y ;X-axis of next source path tile
	CMP PATHFINDER.SPRITE.RECORD+$8		;X-axis of the NPC's current map position
	BNE .NEXT.RECORD			;If next source = NPC's current map position, then final path has been record, exit pathfinder

;WRITE INDEX OF LAST "FINAL PATH" RECORD TO 1ST BYTE
	LDA FINAL.PATH.RECORD.COUNTER
	ASL ;X2								;convert total records to total bytes of the records written. 
	; SEC
	; SBC #$01
	STA NPC.PATHFINDER.FINAL.PATH+$0	;write last record's index as first byte of the final path array. This was calculated just by taking total records *2, with no -1 to back up to the first byte of the last record, because the use of the first byte of the array for this index value puts a straight *2 calculation in alignment. The last record will be the next move the NPC makes to reach destination. 
	
	JMP NPC.PATHFINDER.EXIT	
	
.NEXT.RECORD

;;TROUBLESHOOTING HOOK	
				; STA SAVED.ACC.LOCAL
				; LDA FINAL.PATH.RECORD.COUNTER
				; ;CMP #$37
				; CMP #$20
				; BNE .TEMP1
				; JSR PREP.BRK
				; DEY
				; LDA (NPC.PATHFINDER.SEARCH.PATHS.POINTER2),Y
				; TAX
				; LDA PATHFINDER.SPRITE.RECORD+$9
				; LDY PATHFINDER.SPRITE.RECORD+$8
				; ; LDA FINAL.PATH.RECORD.COUNTER
				; ; LDX NPC.PATHFINDER.TILE_NUMBER.TALLY
				; ; LDY ITERATION.COUNTER
				; ; LDX NPC.PATHFINDER.NEIGHBOR.RMAP
				; ; LDY NPC.PATHFINDER.NEIGHBOR.RMAP+$1
				; BRK
				;
; .TEMP1				
				; LDA SAVED.ACC.LOCAL

			
	INC FINAL.PATH.RECORD.COUNTER		;increment record counter for NPC.PATHFINDER.FINAL.PATH 	
	INX 								;next final path array record
	CMP #FINAL.PATH.HIMEM
	BEQ .ERROR.OVERFLOW.FINAL.PATH
	JMP .LOOP.FINAL.PATH 

.ERROR.OVERFLOW.FINAL.PATH
;ERROR REPORTED BY .NEXT.RECORD (ASSEMBLE.FINAL.PATH). THE INDEX TO NPC.PATHFINDER.FINAL.PATH OVERFLOWED. 
	LDA TEXT
	LDA $C082
	BRK

@END

ERROR.OVERFLOW
;MULTIPLICATION TABLE OVERFLOW IN .ADD_PRIORITY.QUE
;likely caused by NPC.PATHFINDER.TILE_NUMBER.TALLY exceeding
;the last index in the multiplication table. 
;(NPC.PATHFINDER.MULTIPLY_TABLE.LO/HO)
	LDA TEXT
	LDA $C082
	BRK
	
NPC.PATHFINDER.EXIT	
;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX
	
;RETURN MOVEMENT CODE
; ;DETERMINE MOVEMENT RETURN CODE
	; LDY #$04						;byte $04 of final path array record
	;
	; LDA (NPC.PATHFINDER.SEARCH.PATHS.POINTER2),Y ;load x-axis of next move
	; CMP PATHFINDER.SPRITE.RECORD+$08
	; BEQ .NORTH.OR.SOUTH
; ;.EAST.OR.WEST
	; CMP PATHFINDER.SPRITE.RECORD+$08			;X-axis of NPC current position
	; BCC .MOVE.WEST
; ;.MOVE.EAST
	; LDA #$02						;load movement code for east in ACC as return value
	; JMP .EXIT
	;
; .MOVE.WEST
	; LDA #$03						;load movement code for west in ACC as return value
	; JMP .EXIT
	;
; .NORTH.OR.SOUTH
	; INY								;advance to byte $05 of final path array record
	; LDA (NPC.PATHFINDER.SEARCH.PATHS.POINTER2),Y ;load x-axis of next move
	;
	; CMP PATHFINDER.SPRITE.RECORD+$09			;X-axis of NPC current position
	; BCC .MOVE.NORTH
; ;.MOVE.SOUTH
	; LDA #$01						;load movement code for south in ACC as return value
	; JMP .EXIT
	;
; .MOVE.NORTH
	; LDA #$00						;load movement code for north in ACC as return value
	; JMP .EXIT
;.EXIT

;;TROUBLESHOOTING HOOK
				; LDA ITERATION.COUNTER
				; CMP #$02
				; BNE .TEMP1
				; JSR PREP.BRK
				; ; LDA FINAL.PATH.RECORD.COUNTER
				; ; LDX NPC.PATHFINDER.TILE_NUMBER.TALLY
				; ; LDY ITERATION.COUNTER
				; LDA NPC.PATHFINDER.PRIORITY.QUE.RECORD_TALLY
				; LDX NPC.PATHFINDER.PRIORITY.QUE.RECORD_INDEX
				; LDY PATHFINDER.SHORTEST.DISTANCE.COUNTER
				; ; LDY NPC.PATHFINDER.NEIGHBOR.RMAP+$1
				; BRK
				; ;
; ;.TEMP1
				

				
	LDA #$01
	STA COMPLETED.FLAG 

	RTS
	
@END

;MULTIPLICATION BY 6 
;used to calculate the offset to the base address of the 16-bit array NPC.PATHFINDER.SEARCH.PATHS
; NPC.PATHFINDER.MULTIPLY_TABLE.LO	.HS 00.06.0C.12.18.1E.24.2A.30.36.3C.42.48.4E.54.5A.60.66.6C.72.78.7E.84.8A.90.96.9C.A2.A8.AE.B4.BA.C0.C6.CC.D2.D8.DE.E4.EA.F0.F6.FC.02.08.0E.14.1A.20.26.2C.32.38.3E.44.4A.50.56.5C.62.68.6E.74.7A.80
; NPC.PATHFINDER.MULTIPLY_TABLE.HO	.HS 00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01

;MULTIPLICATION BY 8
; NPC.PATHFINDER.MULTIPLY_TABLE.LO	.HS	00.08.10.18.20.28.30.38.40.48.50.58.60.68.70.78.80.88.90.98.A0.A8.B0.B8.C0.C8.D0.D8.E0.E8.F0.F8.00.08.10.18.20.28.30.38.40.48.50.58.60.68.70.78.80.88.90.98.A0.A8.B0.B8.C0.C8.D0.D8.E0.E8.F0.F8.00.08.10.18.20.28.30.38.40.48.50.58.60.68.70.78.80.88.90.98.A0.A8.B0.B8.C0.C8.D0.D8.E0.E8.F0.F8.00.08.10.18.20.28.30.38.40.48.50.58.60.68.70.78.80.88.90.98.A0.A8.B0.B8.C0.C8.D0.D8.E0.E8.F0.F8.00.08.10.18.20.28.30.38.40.48.50.58.60.68.70.78.80.88.90.98.A0.A8.B0.B8.C0.C8.D0.D8.E0.E8.F0.F8.00.08.10.18.20.28.30.38.40.48.50.58.60.68.70.78.80.88.90.98.A0.A8.B0.B8.C0.C8.D0.D8.E0.E8.F0.F8.00.08.10.18.20.28.30.38.40.48.50.58.60.68.70.78.80.88.90.98.A0.A8.B0.B8.C0.C8.D0.D8.E0.E8.F0.F8.00.08.10.18.20.28.30.38.40.48.50.58.60.68.70.78.80.88.90.98.A0.A8.B0.B8.C0.C8.D0.D8.E0.E8.F0.F8
; NPC.PATHFINDER.MULTIPLY_TABLE.HO	.HS	00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07

SCREEN.INDEX.TILE_SBYTE		.HS 02.04.06.08.0A.0C.0E.10.12.14.16.18.1A.1C.1E.20.22.02.04.06.08.0A.0C.0E.10.12.14.16.18.1A.1C.1E.20.22.02.04.06.08.0A.0C.0E.10.12.14.16.18.1A.1C.1E.20.22.02.04.06.08.0A.0C.0E.10.12.14.16.18.1A.1C.1E.20.22.02.04.06.08.0A.0C.0E.10.12.14.16.18.1A.1C.1E.20.22.02.04.06.08.0A.0C.0E.10.12.14.16.18.1A.1C.1E.20.22.02.04.06.08.0A.0C.0E.10.12.14.16.18.1A.1C.1E.20.22.02.04.06.08.0A.0C.0E.10.12.14.16.18.1A.1C.1E.20.22.02.04.06.08.0A.0C.0E.10.12.14.16.18.1A.1C.1E.20.22.02.04.06.08.0A.0C.0E.10.12.14.16.18.1A.1C.1E.20.22.02.04.06.08.0A.0C.0E.10.12.14.16.18.1A.1C.1E.20.22
SCREEN.INDEX.TILE_LINE		.HS 08.08.08.08.08.08.08.08.08.08.08.08.08.08.08.08.08.18.18.18.18.18.18.18.18.18.18.18.18.18.18.18.18.18.28.28.28.28.28.28.28.28.28.28.28.28.28.28.28.28.28.38.38.38.38.38.38.38.38.38.38.38.38.38.38.38.38.38.48.48.48.48.48.48.48.48.48.48.48.48.48.48.48.48.48.58.58.58.58.58.58.58.58.58.58.58.58.58.58.58.58.58.68.68.68.68.68.68.68.68.68.68.68.68.68.68.68.68.68.78.78.78.78.78.78.78.78.78.78.78.78.78.78.78.78.78.88.88.88.88.88.88.88.88.88.88.88.88.88.88.88.88.88.98.98.98.98.98.98.98.98.98.98.98.98.98.98.98.98.98.A8.A8.A8.A8.A8.A8.A8.A8.A8.A8.A8.A8.A8.A8.A8.A8.A8

