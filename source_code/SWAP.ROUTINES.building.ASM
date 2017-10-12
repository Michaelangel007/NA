; ============================================
; Copyright (C) 2016-2018. 6502 Workshop, LLC
; contact: mark@6502workshop.com
; ============================================


;************************INCLUDE FILE*****************************
;(do not assemble stand-alone.  )
				.TF     swap.routines.building.bin,BIN
				.OR		$9600			**Always put before .TF directive and never use again in program
				.EB		OFF


;=====SWAP.ROUTINES.BUILDING.ASM DOCUMENTATION====================================
;
;These are routines which are needed when a building map is loaded, and are not needed
;when any of the modules are loaded which use the main memory swap are (i.e. NPC Talk, Combat, Inventory)
;
;As a result, these routines can be loaded into the main memory swap area and they will be
;swapped out to aux memory when one of the modules is loaded. 
;
;Since there are also routines which are only needed when a non-building map is loaded, memory is saved
;by only having the "building" or "non-building" routines in memory at any given time. 
;
;=================================================================================


NPC_BUILDING.MANAGER
@START

;=====DOCUMENTATION====================================
;
;==OVERVIEW===
;This file contains the subroutines for managing NPC 
;schedules and the primary subroutines which make up 
;"PathFinder". There is some code in Movement_Manager.ASM (NPC.MOVE_MANAGER) 
;which uses pathfinder to determine the next move for NPCs in transition.
;
;NPC's hang out at various "anchor locations" and 
;periodically switch (transition) anchor locations as
;determine by their schedule. The purpose of pathfinder 
;is identify the RMAP.X/Y coordinates between NPC's 
;current anchor location and a destination anchor location.  
;
;How NPC's move while at their anchor locations is 
;controlled by NPC.MOVE_MANAGER (Movement_Manager.ASM).
;
;See Chart 1.3 (PathFinder) for information on how 
;each subroutines integrates into the rest of the game 
;code and the high level bullet points on the functionality 
;of each. 
;
;See Map Objects spreadhseet NPC & NPC Pathfinder for 
;datagrams on all the arrays used in the subroutines 
;in this file, and also a lot of details on the logical
;structure of NPC schedules.
;
;
;
;=====A FEW MISC DETAILS====
;
;NPC.PATHGENERATOR runs continously, processing records
;in the que. 
;NPC.SCHEDULER only runs at inverals, which detects 
;NPCs ready to transit to another anchor. 
;SCHEDULE.TABLE.IDENTIFY.NEXT_PATHS runs continously, which adds records to the schedule que if needed, unless the abort flag is set, which means a path is currently being calculated. 
;
;
;return values:
;	NPC.INIT: **called from MAP.ENTER.CHECK**
;			updated MAP_OBJECTS.NPC (RMAP.X/Y, transit flag OFF)
;
;	NPC.SCHEDULE: **called from CHECK.NPC.SCHEDULER.INTERVAL (EVENT.MANAGER)**
;			SCHEDULE.TABLE.IDENTIFY.TRANSIT:
;				updated MAP_OBJECTS.NPC (some transit flags ON, "at move anchor flag" set)
;			SCHEDULE.TABLE.IDENTIFY.NEXT_PATHS
;				NPC.PATHGENERATOR.QUE (contains upcoming paths needed)
;
;	NPC.PATHGENERATOR **called from GAME.PRIMARY_LOOP**
;			PATHFINDER.SPRITE.RECORD (loaded from a specific record of NPC.PATHGENERATOR.QUE)
;			SAVED.PATH.LOOKUP.TABLE	(lookup table for accessing paths stored in AUX memory)
;			NPC.PATHGENERATOR.SAVED_PATHS.AUX (path data, only accessible via AUX memory)
;=================================================================================

NPC.INIT ;**OPT** Memory. This routine could probably be loaded from disk when a building is entered. Not needed for undermap as this array only deals with schedules/paths
@START
;=====================SUBROUTINE DOCUMENTATION====================================
;
;Called upon entering a building, changing the game clock. Expected to be called for any sort of "wait" or "rest" function. 
;
;Clears all arrays related to NPC SCheduing
;
;*Sets the active anchor location for each NPC record based on the current time of day.
;
;=================================================================================


;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA

;INIT MISC. VARIABLES	
	LDX #$00
	STX PLAYER.BLOCKED.NPC.COUNTER
	STX NPC.PATHFINDER.ABORT_FLAG
	
;INIT ARRAYS
.LOOP.INIT1
	LDA #$FF
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

	LDX #$00
.LOOP.INIT3
	LDA #$FF
	STA NPC.SCHEDULE.WORKSPACE,X
	CPX #NPC.SCHEDULE.WORKSPACE.SIZE
	BEQ .LOOP.INIT3.COMPLETE
	INX
	JMP .LOOP.INIT3
.LOOP.INIT3.COMPLETE	
	
	
	
; ;*****TEMP FOR TESTING**** ;**OPT** considering removing, but see notes below. 
; ;be careful, I tried removing this once and
; ;there were some issue, though they may have existed
; ;before removing this code. The two issues, which didn't occur consistently
; ;was a NPC on floor2 of test town transitioning to the ladder but remaining visible.
; ;and on one occasion when the transition worked correctly, I exited the town
; ;and there was a skiff on the town tile, so something got message up with surface map object locations. 
; ;but when I tried this I wasn't 100% confident I was on a stable version, having
; ;just implemented ladder transitions. 
	; LDX #$00
; .LOOP.INIT4
	; LDA #$AA
	; STA NPC.ANCHORS.X,X
	; STA NPC.ANCHORS.Y,X
	; INX
	; CPX #$A0
	; BEQ .LOOP.INIT4.COMPLETE
	; JMP .LOOP.INIT4
; .LOOP.INIT4.COMPLETE

	

;INIT NPC RECORDS & BUILD ANCHOR LOOKUP TABLE
@START	
;=====================CODE-SECTION DOCUMENTATION====================================
;
;Compares the current hour to the hour in each schedule record
;and builds an array (NPC.SCHEDULE.WORKSPACE) which contains
;the schedule record HO/LO address, indexed by NPC Record #
;for fast access. 
;
;This array can be index by NPC Record # (which are in 
;increments of $8 bytes) by wasting a lot of space. But,
;It borrows space from the ZONE_TOOLS.INPUT_BUFFER so it's
;space that was being used anyway. 
;
;Once NPC.SCHEDULE.WORKSPACE array has been built, 
;NPC.INIT.NPC_RECORDS uses it to set various fields on 
;each NPC Record, such as the Active Anchor.
;
;In general this routine looks for the schedule record
;with a time that is less than the current time and closer
;to the current time than any other record. 
;
;This routines also has to deal with scenarios such as this:
;it is 02:00am and there is a schedule record at 22:00pm and
;there are no NPC Schedule between it and 02:00am. The 
;problem is that since a day boundary is crossed, simple
;math can't be used to determine that the 22:00 record 
;comes before the current time of 2:00am
;
;The solution used was this. If a record is found greaer than 
;the current time and no records less than the current time 
;were found, then the algorith changes approach (see .IS.GREATER.APPLIES) by 
;treating records greater than the current time as being 
;BEFORE the current time. The more greater the record is,
;the closer to the current time it is. (i.e. if it is 2am, a record at 23:00 is closer 
;to the current time than a record at 22:00)
;
;=================================================================================



	;INIT VARIABLES, INDEXES, COUNTERS
	LDY #$00						;NPC.SCHEDULE index (16-bit array)
	LDA #NPC.SCHEDULE
	STA NPC.SCHEDULE.POINTER
	LDA /NPC.SCHEDULE
	STA NPC.SCHEDULE.POINTER+$1
LOOP.INIT.NPC
	INY		;advance to schedule record byte $01
	INY		;advance to schedule record byte $02 (start hour)

	LDA (NPC.SCHEDULE.POINTER),Y
	CMP TIME.CURRENT.HOUR			;is schedule record for an hour after of before the current hours position on the clock (ignoring day)
	BCC INIT.EXAMINE.SCHEDULE.RECORD	;if before, then examine the schedule record to see if it is closer to the current time than the next best record
	BEQ .CHECK.MINUTE				;if not sure because hours match, check minutes	
	;SCHEDULE RECORD GREATER THAN CURRENT HOUR
.AFTER.CURRENT_HOUR	
	;Set NPC.SCHEDULE.WORKSPACE index
	DEY		;back up to schedule record byte $1 (NPC record #)
	LDA (NPC.SCHEDULE.POINTER),Y		;NPC Record # is the index to
	TAX

	LDA NPC.SCHEDULE.WORKSPACE+$5,X		
	CMP #$00							;Is greater than flag set? If yes, that means that INIT.NEXT.SCHEDULE.RECORD will treat hour values as being closer to the current hour, the more greater the numerical hour value is. For example, if the current time is 03:00, then a schedule entry of 23:00 (the day before) is before the current time and it is closer to the current time than 22:00. Thus we can tell 23:00 is closer because it is greater than 22:00
	BEQ .IS.GREATER.APPLIES

	LDA NPC.SCHEDULE.WORKSPACE+$1,X		;load hour
	CMP #$FF							;have any schedule records been found already for this NPC record? (if so they would have been for hours less than the current time's posiiton on the clock)? The presence of $FF (init value) indicates the answer is no as all schedule records occur in the array in time sequence. 
	BNE .NEXT.RECORD
.IS.GREATER.APPLIES
	LDA #$00
	STA NPC.SCHEDULE.WORKSPACE+$5,X		;flip the greater than flag to greater. This means that INIT.NEXT.SCHEDULE.RECORD will treat hour values as being closer to the current hour, the more greater the numerical hour value is. For example, if the current time is 03:00, then a schedule entry of 23:00 (the day before) is before the current time and it is closer to the current time than 22:00. Thus we can tell 23:00 is closer because it is greater than 22:00
	INY		;advance to schedule record byte $2 (hour). This is to keep Y-REG in sync with it's value before all other calls to INIT.NEXT.SCHEDULE.RECORD

	JMP INIT.EXAMINE.SCHEDULE.RECORD
	
.NEXT.RECORD	
	INY		;advance to schedule record byte $2 (hour). This is to keep Y-REG in sync with it's value before all other calls to NEXT.SCHEDULE.RECORD
	
	JMP INIT.NEXT.SCHEDULE.RECORD
	

.CHECK.MINUTE
	INY		;advance to schedule record byte $03 (start minute)

	LDA TIME.CURRENT.MINUTE	
	CMP (NPC.SCHEDULE.POINTER),Y		;is schedule record before or exactly at current time?
	DEY		;back up to schedule record byte $2. This is to keep Y-REG in sync with it's value before all other calls to INIT.EXAMINE.SCHEDULE.RECORD
	BCS INIT.EXAMINE.SCHEDULE.RECORD	;if yes, then examine the schedule record to see if it's already in the scheduler que. If not, add it. 	
	JMP .AFTER.CURRENT_HOUR

INIT.NEXT.SCHEDULE.RECORD
	;Y-REG = schedule record byte $02 (start hour)

	DEY
	DEY ;backup to schedule record byte $00 (Anchor #)
	
;WRITE CURRENT RECORD'S ANCHOR X,Y-AXIS TO LOOKUP TABLE
;get anchor # of current schedule record
	LDA (NPC.SCHEDULE.POINTER),Y		;load byte #$00 (Anchor #)
	TAX									;load anchor # as index to NPC.ANCHORS.X/Y
	

;GET GMAP.X of ANCHOR LOCATION	
	INY
	INY
	INY
	INY
	INY ;advance to schedule record byte $05 (GMAP.X)
	
	LDA (NPC.SCHEDULE.POINTER),Y		;load byte $05 (GMAP.X)
	STA	NPC.ANCHORS.X, X				;save to lookup table

;GET GMAP.Y of ANCHOR LOCATION		
	INY ;advance to schedule record byte $06 (GMAP.Y)
	LDA (NPC.SCHEDULE.POINTER),Y		;load byte $06 (GMAP.Y)
	STA	NPC.ANCHORS.Y, X				;save to lookup table
	
	
;INCREMENT NPC.SCHEDULE INDEX
	;Y-REG = schedule record byte $02 (start hour)

	INY ;+5
	INY ;+6 (advance to next record....+2 and Y-REG start of $06 = a total increment of +8)
	
	BNE .CHECK.STOP.VALUE 			;did index flip to #$00? 
	INC NPC.SCHEDULE.POINTER+$1		;if yes, increment HO byte of pointer
				
.CHECK.STOP.VALUE	
	;Y-REG = schedule record byte $00 (anchor # & stop value)
	LDA (NPC.SCHEDULE.POINTER),Y 	;load byte $00
	CMP #$FF						;at end of array?	
	BNE LOOP.INIT.NPC			;if no, continue loop
	JMP NPC.INIT.NPC_RECORDS			;if yes, exit loop


INIT.EXAMINE.SCHEDULE.RECORD	
; NPC.SCHEDULE Datagram								
; Byte 0	Byte 1			Byte 2				Byte 3				Byte 4			Byte 5		Byte 6		Byte 7							X+$0
; Anchor #	NPC Record #	Start Time: Hour	Start Time: Minute	End Time: Hour	X ofAnchor	Y ofAnchor	at-anchor move routine flag		Stop value
;see Map Objects spreadsheet for authoratative version

	
;NPC.SCHEDULE.WORKSPACE Datagram				
;Byte 0			Byte 1				Byte 2					Byte 3					Byte 4
;NPC Record #	Closest Hour Value	Closest Minute Value	Schedule Record LO Byte	Schedule Record HO Byte
;See Map Objects spreadsheet (NPC Pathfinder section) for the authoritative version

	
;INIT VARIABLES, COUNTERS, INDEXES	
	;Y-REG = schedule record byte $2 (hour)

	;Set NPC.SCHEDULE.WORKSPACE index
	DEY		;back up to schedule record byte $1 (NPC record #)
	LDA (NPC.SCHEDULE.POINTER),Y		;NPC Record # is the index to NPC.SCHEDULE.WORKSPACE. This works because the records in both arrays have the same number of bytes
	TAX
	
	INY		;advance to schedule record byte $2 (hour)

	LDA NPC.SCHEDULE.WORKSPACE+$1,X
	CMP #$FF						;is array init value in workspace array for this NPC Record?
	BEQ .SET.VALUE					;if yes, then automatically save this scheduler record's hour/minute value to the workspace array
	CMP (NPC.SCHEDULE.POINTER),Y 	 
	BCC .SET.VALUE					;schedule record hour is greater than the hour saved in workspace for this NPC, so replace the workspace value with the current schedule record value 	
	BEQ .CHECK.MINUTE				;since the hours are equal, the minute values must be compared 
	JMP	INIT.NEXT.SCHEDULE.RECORD	;schedule record is not closer to current time than the record saved in workspace for this NPC. Get next schedule record.
	
	;**FALLS THROUGH**
.SET.VALUE
	;Y-REG = schedule record byte $2 (hour)

	;SAVE SCHEDULE RECORD HO/LO ADDRESS
	DEY		;back up to schedule record byte $1
	DEY		;back up to schedule record byte $0	
	STY TEMP
	LDA NPC.SCHEDULE.POINTER
	CLC
	ADC TEMP
	STA NPC.SCHEDULE.WORKSPACE+$3,X
	LDA NPC.SCHEDULE.POINTER+$1
	ADC #$00	; 16-BIT
	STA NPC.SCHEDULE.WORKSPACE+$4,X
	
	;SAVE NPC Record #
	INY		;advance to schedule record byte $1 (NPC Record#)
	LDA (NPC.SCHEDULE.POINTER),Y	;load NPC Record #
	STA NPC.SCHEDULE.WORKSPACE+$0,X	
	
	;SAVE HOUR
	INY		;advance to schedule record byte $2 (hour)	
	LDA (NPC.SCHEDULE.POINTER),Y	;load hour
	STA NPC.SCHEDULE.WORKSPACE+$1,X
	
	;SAVE MINUTE
	INY		;advance to schedule record byte $3 (minute)	
	LDA (NPC.SCHEDULE.POINTER),Y	;load minute
	STA NPC.SCHEDULE.WORKSPACE+$2,X
	
	
	DEY		;back up to schedule record byte $2 (hour). This is to keep the y-reg value in sync for all calls to INIT.NEXT.SCHEDULE.RECORD		
	JMP INIT.NEXT.SCHEDULE.RECORD
	
.CHECK.MINUTE
	INY		;advance to schedule record byte $03 (start minute)

	
			
	LDA NPC.SCHEDULE.WORKSPACE+$2,X		;load minute
	CMP (NPC.SCHEDULE.POINTER),Y		
	DEY		;back up to schedule record byte $2. This is to keep Y-REG in sync with it's value before all other calls to INIT.NEXT.SCHEDULE.RECORD
	BCC .SET.VALUE	;schedule record is either equal to or closer to the current time than the hour/minute values in saves workspace, so replace the workspace value with the current schedule record value 

	JMP INIT.NEXT.SCHEDULE.RECORD	;schedule record is not closer to current time than the record saved in workspace for this NPC. Get next schedule record.
	

NPC.INIT.NPC_RECORDS
			; LDA #$AA
			; JSR PREP.BRK
			; LDX #NPC.SCHEDULE.WORKSPACE
			; LDY /NPC.SCHEDULE.WORKSPACE
			; BRK
			
			
	LDX #$F8				;MAP_OBJECTS.NPC index. Init to $F8, because the index increment is at the top of the loop. By init $F8 the index flips over to $00 on first iteration. 

.LOOP.NPC_RECORDS

;INCREMENT/DECREMENT INDEXES AND TEST EXIT	
	LDY #$00 ;NPC.SCHEDULE index. Always start at byte $00 (anchor #)

.LOOP.INCREMENT	
	TXA
	CLC
	ADC #$08		;increment NPC record #
	TAX			

	;test for active NPC Record
	LDA NPC.SCHEDULE.WORKSPACE+$2,X 	;is this NPC Record # in use? If not the $FF init value for NPC.SCHEDULE.WORKSPACE will be found. 
	CMP #$FF	;is init value found?
	BNE .PROCESS.RECORD.START ;if no, then this NPC Record # is in use. Process the workspace data for it. 
	
	;exit test
	;(if the current NPC Record is inactive, then check to see if the current NPC Record # is the max value possible. If it is, then we're done; exit the loop)
	CPX #MAXIMUM.NPC.RECORD_NUMBER ;is current NPC Record # the max value possibe?
	BEQ NPC.INIT.EXIT ;if yes, then exit the loop
	JMP .LOOP.INCREMENT ;if no, then get the next NPC Record # because NPC Record #s not in use should be skipped. Otherwise the MAP_OBJECTS.NPC array will end up with bad data in that NPC Record slot and wierd graphical things and/or crashes could happen. 
			
.PROCESS.RECORD.START			
;SETUP SCHEDULE POINTER
	LDA NPC.SCHEDULE.WORKSPACE+$3,X		;load LO address of schedule record for current NPC record
	STA NPC.SCHEDULE.POINTER			
	
	LDA NPC.SCHEDULE.WORKSPACE+$4,X		;load HO address of schedule record for current NPC record
	STA NPC.SCHEDULE.POINTER+$1			

	
	;Y-REG = byte $00 (anchor #)
		
;INIT ANCHOR #
	LDA (NPC.SCHEDULE.POINTER),Y		;load Anchor # (schedule record byte $0)
	STA MAP_OBJECTS.NPC+$4,X	


;INIT NPC GMAP.X
	LDY #$05 ;advance to byte $5 (Anchor X-Axis)
	LDA (NPC.SCHEDULE.POINTER),Y		;load Anchor X-Axis (schedule record byte $5)
	STA MAP_OBJECTS.NPC+$0,X
			
	INY	;advance to byte $6 (Anchor Y-Axis)

;INIT NPC GMAP.Y
	LDA (NPC.SCHEDULE.POINTER),Y		;load Anchor Y-Axis (schedule record byte $6)
	STA MAP_OBJECTS.NPC+$1,X
	
	INY	;advance to byte $7 (at-anchor move routine flag)

;INIT NPC "AT-ANCHOR MOVE FLAG"	
	LDA (NPC.SCHEDULE.POINTER),Y		;load "at-anchor move routine flag" (schedule record byte $7)
	STA MAP_OBJECTS.NPC+$7,X

;INIT TRANSIT FLAG: OFF
	LDA #$00
	STA MAP_OBJECTS.NPC+$6,X			;**OPT** Speed. Memory. Can be removed once NPC array is loaded from disk when entering building

	;INC NPC.INIT.ITERATION_COUNTER

	CPX #MAXIMUM.NPC.RECORD_NUMBER ;a 2nd exit check is need because the other exit check only triggers if the NPC Record is empty. If the maxium NPC Record # is in use, then we need this exit check to make sure we exit the loop before X-REG (NPC Record #) flips over to $00
	BEQ NPC.INIT.EXIT

	JMP .LOOP.NPC_RECORDS
	

			
@END

NPC.INIT.EXIT	

			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; ;CPX #$20
			; BNE .TEMP
			; JSR PREP.BRK
			; lda #$aa
			; LDX #NPC.SCHEDULE.WORKSPACE
			; LDY /NPC.SCHEDULE.WORKSPACE
			; BRK
; .TEMP
			; LDA TEMP
			
			; LDA #$AB
			; JSR PREP.BRK
			; LDX #NPC.SCHEDULE.WORKSPACE
			; LDY /NPC.SCHEDULE.WORKSPACE
			; BRK
			
;RESTORE REGISTERS		
	PLA
	TAY
	PLA
	TAX
			; LDA #$AA
			; LDX #NPC.SCHEDULE.WORKSPACE
			; LDY /NPC.SCHEDULE.WORKSPACE
			; JSR PREP.BRK
			; BRK

			
	RTS
@END

	
NPC.SCHEDULER
@START
;=====================SUBROUTINE DOCUMENTATION====================================
;
;==OVERVIEW==
;*Runs periodically, as specified by CHECK.NPC.SCHEDULER.INTERVAL 
;in EVENT_MGR.ASM. Currently runs once per hour, but support is built in for using minute-based intervals. 
;
;*Identifies NPCs that are schedule to transit at the 
;current time.  
;
;*Identifies paths that will need to be calculated soon by 
;looking ahead in schedule. These paths are added to a que.
;
;==SCHEDULE.TABLE.IDENTIFY.NEXT_PATHS==
;The purpose of identifying paths that will be needed soon
;is so that NPC.PATHFINDER can run in the "background", which 
;is facilitated via an auto-abort function.
;	
;This variable that controls the number of hours ahead
;into the schedle to look is PATHGENERATOR.LOOK_AHEAD.HOURS	
;Note that the minimum value is $02, which means 1 hour, 59 minutes. 
;
;
;=================================================================================

;VERIFY SCHEDULER SHOULD RUN RIGHT NOW
	LDA PLAYER.MAP.LOCATION_TYPE		
	; CMP #MAP.TYPE.TOWN_VILLAGE
	; BEQ .IN.BUILDING
	CMP #MAP.TYPE.BUILDING.GRE 	;is map type = building?
	BCC .MAP.TYPE_CHECK.DONE		;if no
	CMP #MAP.TYPE.BUILDING.LT	;is map type = building?
	BCS .MAP.TYPE_CHECK.DONE		;if no
	JMP .IN.BUILDING			;if yes	
.MAP.TYPE_CHECK.DONE
	
	RTS
	
.IN.BUILDING	
		
			; LDA	COMPLETED.SCHEDULER.FLAG 	;to prevent multiple runs during testing
			; CMP #$01
			; BNE .START2
			; JMP NPC.SCHEDULER.EXIT
; .START2

SCHEDULE.TABLE.IDENTIFY.TRANSIT
@START		
	;INIT VARIABLES, INDEXES, COUNTERS
	LDY #$00						;NPC.SCHEDULE index (16-bit array)
	LDA #NPC.SCHEDULE
	STA NPC.SCHEDULE.POINTER
	LDA /NPC.SCHEDULE
	STA NPC.SCHEDULE.POINTER+$1
.LOOP.TRANSIT_SEARCH
	INY		;advance to schedule record byte $01
	INY		;advance to schedule record byte $02 (start hour)

	LDA (NPC.SCHEDULE.POINTER),Y
	CMP TIME.CURRENT.HOUR			;is schedule record for the current hour
	BEQ .CHECK.MINUTE				;if not sure becase hours match, check minutes	

	JMP .NEXT.SCHEDULE.RECORD	

.CHECK.MINUTE
	INY		;advance to schedule record byte $03 (start minute)
	
	LDA (NPC.SCHEDULE.POINTER),Y		;does schedule record match current hour and minute?
	DEY		;back up to schedule record byte $2. This is to keep Y-REG in sync with it's value before all other calls to NEXT.SCHEDULE.RECORD
	CMP TIME.CURRENT.MINUTE
	BNE .NEXT.SCHEDULE.RECORD
	;**FALLS THROUGH**

			
.CHECK.ACTIVE.ANCHOR
;(if NPC is already at the anchor for this schedule record, then do not put NPC in transit mode)
;(this can happen if the player enters a building at the exact time of a schedule record)
	;INIT MAP_OBJECTS.NPC Index	
			
	DEY		;back up to schedule record byte $1 (NPC Record #)
	LDA (NPC.SCHEDULE.POINTER),Y		;Load NPC Record (schedule record byte $01)
	TAX		
	
	DEY		;back up to schedule record byte $0 (Anchor #)
	LDA (NPC.SCHEDULE.POINTER),Y		;Load Anchor # (schedule record byte $00)
	CMP MAP_OBJECTS.NPC+$4,X			;is the NPC's active anchor the same as the schedule record's anchor? 
	BNE .PROCESS.TRANSIT.RECORD			;if yes, then this record triggers an NPC to transit to another anchor	
										;if no, then next schedule record
	INY		
	INY		;advance to schedule record byte $02 (start hour)
	;**FALLS THROUGH**					
	
	
.NEXT.SCHEDULE.RECORD
;INCREMENT NPC.SCHEDULE INDEX

	;Y-REG = schedule record byte $02 (start hour)
	
	INY ;+1
	INY ;+2
	INY ;+3
	INY ;+4
	INY ;+5
	INY ;+6 (advance to next record....+6 and Y-REG start of $02 = a total increment of +8)
	
	;INC SCHEDULER.ITERATION.COUNTER

			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; LDA GAME_LOOP.ITERATION.COUNTER
			; CMP #$05
			; BNE .TEMP
			; LDA SCHEDULER.ITERATION.COUNTER
			; CMP #$24
			; BNE .TEMP
			; JSR PREP.BRK
			; LDA (NPC.SCHEDULE.POINTER),Y
			; BRK
; .TEMP
			; LDA TEMP
			; TYA
			
	BNE .CHECK.STOP.VALUE 			;did index flip to #$00? 
	INC NPC.SCHEDULE.POINTER+$1		;if yes, increment HO byte of pointer

.CHECK.STOP.VALUE				
	;Y-REG = schedule record byte $00 (anchor # & stop value)
	LDA (NPC.SCHEDULE.POINTER),Y 	;load byte $00
	CMP #$FF						;at end of array?	
	BNE .LOOP.TRANSIT_SEARCH		;if no, continue loop
	JMP .EXIT						;if yes, exit loop


.PROCESS.TRANSIT.RECORD			
; NPC.SCHEDULE Datagram								
; Byte 0	Byte 1			Byte 2				Byte 3				Byte 4			Byte 5		Byte 6		Byte 7							X+$0
; Anchor #	NPC Record #	Start Time: Hour	Start Time: Minute	End Time: Hour	X ofAnchor	Y ofAnchor	at-anchor move routine flag		Stop value
;see Map Objects spreadsheet for authoratative version

	;Y-REG = schedule record byte $0 (Anchor #)

	;SET NPC Active Anchor
	LDA (NPC.SCHEDULE.POINTER),Y		;Load Anchor # (schedule record byte $00)
	STA MAP_OBJECTS.NPC+$4,X
	

	;SET TRANSIT FLAG: ON/FIRST MOVE	
	LDA #$03
	STA MAP_OBJECTS.NPC+$6,X			;**OPT** Speed. Memory. Can be removed once NPC array is loaded from disk when entering building

	
	;Set "At-Anchor move flag"
	TYA
	CLC
	ADC #$07	;advance to schedule record byte $7 (At-Anchor move flag)	
	TAY
	
	LDA (NPC.SCHEDULE.POINTER),Y		;Load flag, schedule record byte $07
	STA MAP_OBJECTS.NPC+$7,X
	
	DEY
	DEY
	DEY
	DEY
	DEY		;advance to schedule record byte $02 (start hour)	;keep Y-REG in sync with expected value
	
	JMP .NEXT.SCHEDULE.RECORD			;continue loop, test exit
	
	
.EXIT
	;**FALLS THROUGH
		
@END
	
			
SCHEDULE.TABLE.IDENTIFY.NEXT_PATHS
@START

	
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

;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA

;EXIT IF A PATH IS BEING PROCESSED ALREADY
	LDA NPC.PATHFINDER.ABORT_FLAG		;is flag set? ($00 = not set, $01 = set)
	BEQ .NO_ABORT_SET					;if yes, continue						
	JMP NPC.SCHEDULER.EXIT
.NO_ABORT_SET

;INIT VARIABLES, INDEXES, COUNTERS
	LDA #NPC.SCHEDULE
	STA NPC.SCHEDULE.POINTER
	LDA /NPC.SCHEDULE
	STA NPC.SCHEDULE.POINTER+$1
	
	LDY #$00						;NPC.SCHEDULE index (16-bit array)
LOOP.IDENTIFY.NEXT_PATHS
	INY		;advance to schedule record byte $01
	INY		;advance to schedule record byte $02 (start hour)

	LDA TIME.CURRENT.HOUR
	CMP #PATHGENERATOR.LOOK_AHEAD.DAY_TRANSITION.HOUR	;is hour late enough that the early hours after midnight fall within the looking ahead period? (i.e. if the look ahead period is 3 hours, and it is 23:00, then 01:00 is within the look ahead period even though 01 is less than 23. For this reason a special routine is required to evalute whether the scheduler entry is in the past or in the future)
	BCS .DAY.TRANSITION


		
	LDA (NPC.SCHEDULE.POINTER),Y
	CMP TIME.CURRENT.HOUR			;is schedule record for future or past?
	BCC NEXT.SCHEDULE.RECORD		;if past, next record
	BEQ .CHECK.MINUTE				;if not sure because hours match, check minutes
	;SCHEDULE RECORD IS IN FUTURE



.CHECK.LOOK_AHEAD.HOURS						
	LDA (NPC.SCHEDULE.POINTER),Y			;load hour of schedule record
	SED				;set BCD Mode
	SEC
	SBC TIME.CURRENT.HOUR
	CLD				;clear BCD mode (return to binary mode)
	CMP #PATHGENERATOR.LOOK_AHEAD.HOURS ;is schedule record within the look ahead period for which NPC.PATHGENERATOR calculates paths in advance?
	BCC EXAMINE.SCHEDULE.RECORD			;if yes, then examine the schedule record to see if it's already in the scheduler que. If not, add it. 
	JMP NEXT.SCHEDULE.RECORD


.CHECK.MINUTE
	INY		;advance to schedule record byte $03 (start minute)
	
	LDA TIME.CURRENT.MINUTE
	CMP (NPC.SCHEDULE.POINTER),Y		;is schedule record for future or past?
	DEY		;back up to schedule record byte $2. This is to keep Y-REG in sync with it's value before all other calls to EXAMINE.SCHEDULE.RECORD
	BCC EXAMINE.SCHEDULE.RECORD			;if yes, then examine the schedule record to see if it's already in the scheduler que. If not, add it. 
	JMP NEXT.SCHEDULE.RECORD
	
.DAY.TRANSITION
	;Y-REG = schedule record byte $02 (start hour)
	
	LDA (NPC.SCHEDULE.POINTER),Y		;load record byte $02 (start hour)
	CMP	TIME.CURRENT.HOUR				;do the hours match?
	BEQ .CHECK.MINUTE					;if yes, check minutes
	BCC	.ABSOLUTE.TIME.CONVERSION		;if scheduler hour is less than current hour, then the scheduler hour might be shortly after midnight, and within the look ahead period. To find out we need to convert to an absolute value of sorts.
	JMP .CHECK.LOOK_AHEAD.HOURS
	;JMP EXAMINE.SCHEDULE.RECORD			;scheduler hour is in the future and within the look ahead period. We can conclude this because based on tests performed, the scheduler hour is between #PATHGENERATOR.LOOK_AHEAD.DAY_TRANSITION.HOUR and midnight
	
.ABSOLUTE.TIME.CONVERSION	
	SED				;set BCD Mode
	CLC
	ADC #$24
	SEC
	SBC TIME.CURRENT.HOUR
	CLD				;clear BCD mode (return to binary mode)
	CMP #PATHGENERATOR.LOOK_AHEAD.HOURS ;is schedule record within the look ahead period for which NPC.PATHGENERATOR calculates paths in advance?
	BCC EXAMINE.SCHEDULE.RECORD			;if yes, then examine the schedule record to see if it's already in the scheduler que. If not, add it. 
	;**FALLS THROUGH**					;if no, then get next schedule record
	

NEXT.SCHEDULE.RECORD
;INCREMENT NPC.SCHEDULE INDEX

	;Y-REG = schedule record byte $02 (start hour)
	INY ;+1
	INY ;+2
	INY ;+3
	INY ;+4
	INY ;+5
	INY ;+6 (advance to next record....+6 and Y-REG start of $02 = a total increment of +8)
	
	BNE .CHECK.STOP.VALUE 			;did index flip to #$00? 
	INC NPC.SCHEDULE.POINTER+$1		;if yes, increment HO byte of pointer
		;SCHEDULE.LO_INDEX


				
.CHECK.STOP.VALUE			
;CHECK STOP VALUE
	;Y-REG = schedule record byte $00 (anchor # & stop value)
	LDA (NPC.SCHEDULE.POINTER),Y 	;load byte $00
	CMP #$FF						;at end of array?	
	BNE LOOP.IDENTIFY.NEXT_PATHS	;if no, continue loop
									;if yes, exit
	JMP NPC.SCHEDULER.EXIT				

	
EXAMINE.SCHEDULE.RECORD	
;IS A RECORD ALREADY IN QUE, FOR THIS NPC RECORD #?
;(two paths for the same NPC record number can't be generated
;at the same time becuase only one of those paths will reflect
;the NPC's current postion. The other path will be for a future
;anchor transition and the NPC's start postion will be different.
;It could be done if the schedule record and que record had a source
;anchor and destination anchor but that would require a lot more memory)

;NPC.PATHGENERATOR.QUE		
;Byte 0		Byte 1			Byte 2		Byte 3		Byte 4
;Anchor #	NPC Record #	X of Anchor	Y of Anchor	Status
;see map objects spreadsheet for authoritative version


			

;INIT VARIABLES, COUNTERS, INDEXES	
	LDX #NPC.PATHGENERATOR.QUE.LAST_RECORD_START	;NPC.PATHGENERATOR.QUE index, set to the last byte (For a $3C byte array, the last record start is $36, not $37, because $0 is the first byte)
	;Y-REG = schedule record byte $2
	DEY		;back up to schedule record byte $1 (NPC record #)

	LDA #$FF						;set a default value so we can tell if the que is full
	STA PATHGENERATOR.QUE.INDEX
	
.LOOP.SEARCH.QUE
	;Y-REG = schedule record byte byte $1 (NPC record #)
	LDA NPC.PATHGENERATOR.QUE+$5,X	;load status field
	CMP #$FF						;is record empty/completed?
	BNE .CONTINUE					;if no, continue
	STX PATHGENERATOR.QUE.INDEX		;if yes, save que index (so that if we have to add a record, we've got the index to an empty record handy) 
	
.CONTINUE	
;IS THERE ALREADY AN ACTIVE RECORD IN QUE FOR THIS NPC?
;(i.e. not marked as complete/empty?)
	LDA NPC.PATHGENERATOR.QUE+$1,X	;load NPC Record # of current que record
	CMP (NPC.SCHEDULE.POINTER),Y 	;load record byte $01 (NPC Record #) of current schedule record. 
	BNE .NEXT.QUE_RECORD			;if no match, then next que record
	LDA NPC.PATHGENERATOR.QUE+$5,X	;load status field
	CMP #$FF						;is record empty/completed?	
	BNE .EXISTING.RECORD.FOUND		;if yes, then NPC Record # is already active in que, no need to continue searching
									;if no match check next que record								
.NEXT.QUE_RECORD
;END-OF-ARRAY CHECK									
	CPX #$00						;at end of array?
	BEQ .ADD.NEW.RECORD				;if yes, then existing record found
	

	DEX	;+1
	DEX ;+2
	DEX	;+3
	DEX	;+4
	DEX	;+5		
	DEX	;+6
	
	JMP .LOOP.SEARCH.QUE			;if no, continue loop
							

.ADD.NEW.RECORD
	;Y-REG = schedule record byte $1 (NPC #)

;ERROR CHECK: QUE FULL?
	LDA PATHGENERATOR.QUE.INDEX
	CMP #$FF						;is the que full?
	BEQ .ERROR.SCHEDULE.QUE.FULL	;if yes, report error
	
;ADD RECORD
	LDX PATHGENERATOR.QUE.INDEX		;load index to last open record found

	;SAVE ANCHOR #
	DEY		;back up to schedule record byte $0 (ANCHOR #)
	LDA (NPC.SCHEDULE.POINTER),Y 	;load byte $00 
	
	INY 
	INY 
	INY 
	INY	;advance to schedule record byte $04 (source anchor)
	CMP (NPC.SCHEDULE.POINTER),Y 	;is the destination anchor and source anchor the same?
	BNE .CONTINUE2					;if no, proceed to create que record and save destination anchor # to que
	DEY
	DEY	;back up to byte $02 (hour), so y-reg is in sync with other calls to NEXT.SCHEDULE.RECORD
	JMP	NEXT.SCHEDULE.RECORD 		;if yes, then next record
.CONTINUE2
	STA NPC.PATHGENERATOR.QUE+$0,X	
	
	;SAVE NPC RECORD #
	DEY
	DEY
	DEY ;back up to schedule record byte $01
	LDA (NPC.SCHEDULE.POINTER),Y 	;load byte $01
	STA NPC.PATHGENERATOR.QUE+$1,X	

			
	;SAVE SOURCE ANCHOR #
	INY ;advance to schedule record byte $02
	INY ;advance to schedule record byte $03
	INY ;advance to schedule record byte $04
	LDA (NPC.SCHEDULE.POINTER),Y 	;load byte $04
	STA NPC.PATHGENERATOR.QUE+$4,X	
	
	
	;SAVE ANCHOR X-AXIS
	INY ;advance to schedule record byte $05
	LDA (NPC.SCHEDULE.POINTER),Y 	;load byte $05
	STA NPC.PATHGENERATOR.QUE+$2,X	
	
	;SAVE ANCHOR Y-AXIS
	INY ;advance to schedule record byte $06
	LDA (NPC.SCHEDULE.POINTER),Y 	;load byte $06
	STA NPC.PATHGENERATOR.QUE+$3,X	


	;SET STATUS TO "NEEDS PROCESSING"
	LDA #$00
	STA NPC.PATHGENERATOR.QUE+$5,X	
	
		
	DEY		;back up to schedule record byte $5
	DEY		;back up to schedule record byte $4
	DEY		;back up to schedule record byte $3
	DEY		;back up to schedule record byte $2. This is to keep Y-REG in sync with it's value before all other calls to NEXT.SCHEDULE.RECORD
		
	JMP NEXT.SCHEDULE.RECORD
	
.EXISTING.RECORD.FOUND
;NO NEED TO ADD A NEW RECORD SINCE AN EXISTING UNPROCESSED RECORD
;ALREADY EXISTS FOR THIS ANCHOR

			
	INY		;advance to schedule record byte $02 (start hour). This is to keep Y-REG in sync with it's value before all other calls to NEXT.SCHEDULE.RECORD
	JMP NEXT.SCHEDULE.RECORD	

.ERROR.SCHEDULE.QUE.FULL
;.ADD.NEW.RECORD (NPC.SCHEDULER) reports NPC.PATHGENERATOR.QUE
;is full, as indicated by all records having a value of $00 in
;byte $05, indicating the record needs processing. 
	JSR PREP.BRK
	BRK

@END
NPC.SCHEDULER.EXIT		
;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX	




			; LDA TIME.CURRENT.HOUR
			; CMP #$15
			; BNE .TEMP
			; LDA #$AA
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP
			
			; STA TEMP
			; LDA TIME.CURRENT.HOUR
			; CMP #$15
			; BNE .TEMP20
			; INC TROUBLESHOOTING.HOOK

; .TEMP20
			; LDA TEMP
			
	RTS	

@END

	
NPC.PATHGENERATOR
;PARAMETERS: NPC.PATHGENERATOR.QUE
@START	
;=====================SUBROUTINE DOCUMENTATION====================================
;
;==OVERVIEW==
;*Calls NPC.PATHFINDER to calculate the path for each entry 
;in the pathgenerator que. 
;
;*When a path is completed, save it to aux memory. 
;Movement_Manager.ASM (NPC.MOVE_MANAGER) uses the saved path data to determine next move for NPCs in transition. 
;
;*NPC.PATHFINDER periodic aborts exit back to game loop 
;from here. 
;
;PROCESS.SCHEDULE_QUE.RECORDS is the main routine. It reads
;a record from the que, loads it into pathfinder.sprite.record, 
;then calls NPC.PATHFINDER. Upon return, this routine does an
;an auto-abort flag check and uses an alternate exit if it is set. 
;
;=================================================================================


;ADD PATHS TO THE SCHEDULE QUE
	JSR SCHEDULE.TABLE.IDENTIFY.NEXT_PATHS	;identify NPCs who will transit to a new anchor soon and add entries to the NPC.PATHGENERATOR.QUE, which are processed by NPC.PATHGENERATOR when called from the primary game loop.

	
			; STA TEMP
			; LDA TIME.CURRENT.HOUR
			; CMP #$15
			; BNE .TEMP20
			; INC TROUBLESHOOTING.HOOK

; .TEMP20
			; LDA TEMP

			
			; ; STA TEMP
			; ; LDA TROUBLESHOOTING.HOOK
			; ; CMP #$01
			; ; BCC .TEMP
			; ; BNE .TEMP
			; ;LDA PATHFINDER.SPRITE.RECORD+$04
			; ;LDA PATHFINDER.SPRITE.RECORD+$0B
			; ;CMP #$18
			; ;CMP #$08
			; LDA TIME.CURRENT.HOUR
			; CMP #$15
			; BCC .TEMP
			; LDA NPC.PATHGENERATOR.QUE
			; CMP #$08
			; BNE .TEMP
			; LDA #$AA
			; LDX TROUBLESHOOTING.HOOK
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP
			
;****(but not with hour triggered trap. wrong destination anchor at this point
			
PROCESS.SCHEDULE_QUE.RECORDS
@START


			
			
;MAP_OBJECTS.MOB datagram
;Byte 0	Byte 1	Byte 2		Byte 3		Byte 4			Byte 5			Byte 6				Byte 7
;GMAP.X	GMAP.Y	Tile_type	Set to $03	Active Anchor	Path Index($00)	In Transit? ($00)	at-anchor move routine flag
;See Map Objects spreadsheet for authoratative version

;NPC.PATHGENERATOR.QUE	(!10 records allocated)				
;Byte 0		Byte 1			Byte 2		Byte 3		Byte 4		Byte 5
;Anchor #	NPC Record #	X ofAnchor	Y of Anchor	Non-Street 	Weight	status
;See Map Objects spreadsheet for authoratative version


				
	LDX #$00					;NPC.PATHGENERATOR.QUE index
.LOOP.SCHEDULE.QUE
	LDA NPC.PATHGENERATOR.QUE+$5,X	;load status field
	CMP #$00						;is record flagged for processing?
	BNE .NEXT_RECORD				;if no, next record
	
;SETUP PATHFINDER PARAMETERS
	LDY NPC.PATHGENERATOR.QUE+$1,X	;load NPC record field for use as index to Map Objects Array
	
	; ;SET WEIGHT FOR NON-ROAD TILES
	; ;LDA #$10
	; LDA NPC.PATHGENERATOR.QUE+4,X
	; STA PATHFINDER.STREET.PREFERENCE	
	

				
	;SET RMAP X/Y OF DESTINATION 
	LDA NPC.PATHGENERATOR.QUE+$2,X
	STA NPC.PATHFINDER.DESTINATION.TILE.X
	
	LDA NPC.PATHGENERATOR.QUE+$3,X
	STA NPC.PATHFINDER.DESTINATION.TILE.Y
	
	;SET RMAP X/Y OF SOURCE ANCHOR TO DESTINATION
	STX SAVED.XREG.LOCAL			;save NPC.PATHGENERATOR.QUE index
	LDA NPC.PATHGENERATOR.QUE+$4,X	;load source anchor #
	TAX 							;load source anchor # as inxex to NPC.ANCHORS.X/Y

	LDA NPC.ANCHORS.X,X				;load source anchor X-axis
	STA PATHFINDER.SPRITE.RECORD+$8

	LDA NPC.ANCHORS.Y,X				;load source anchor Y-axis
	STA PATHFINDER.SPRITE.RECORD+$9	
	LDX	SAVED.XREG.LOCAL			;restore NPC.PATHGENERATOR.QUE index
	
			; STA TEMP
			; ; LDA TROUBLESHOOTING.HOOK
			; ; CMP #$01
			; ; BNE .TEMP
			; LDA NPC.ANCHORS.Y,X
			; TAY
			; LDA NPC.ANCHORS.X,X
			; TAX
			
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP
	
;THE FOLLOWING MAY NOT BE NEEED (**OPT** Memory)
	;SET TILE_TYPE OF SPRITE
	LDA MAP_OBJECTS.NPC+$2,Y
	STA PATHFINDER.SPRITE.RECORD+$2		
				
	;SET MOB TYPE MOVEMENT FLAG OF SPRITE
	LDA MAP_OBJECTS.NPC+$3,Y
	STA PATHFINDER.SPRITE.RECORD+$3
	
	;SET ANCHOR # OF PATH (USE DESTINATION ANCHOR #)
	LDA NPC.PATHGENERATOR.QUE+$0,X
	STA PATHFINDER.SPRITE.RECORD+$4

	;SET SPRITE TYPE TO BUILDING NPC			
	LDA #$03						
	STA PATHFINDER.SPRITE.RECORD+$A
			
	;SET MAP OBJECT RECORD #			;***THIS ONE PROBABLY IS NEEDED
	STY PATHFINDER.SPRITE.RECORD+$B	
			;**THE ABOVE IS NEEDED
			
	;SET PATH INDEX	(THIS FIELD MIGHT GET USED FOR SOMETHING ELSE)
	LDA MAP_OBJECTS.NPC+$5,Y
	STA PATHFINDER.SPRITE.RECORD+$5	
	
	;SET IN TRANSIT
	LDA MAP_OBJECTS.NPC+$6,Y
	STA PATHFINDER.SPRITE.RECORD+$6
	
	;SET "AT-ANCHOR" MOVE ROUTINE FLAG
	LDA MAP_OBJECTS.NPC+$7,Y
	STA PATHFINDER.SPRITE.RECORD+$7
				

		STX PATHGENERATOR.QUE.INDEX
	JSR GET.PATH
		LDX PATHGENERATOR.QUE.INDEX

.NEXT_RECORD
	INX	;+1
	INX ;+2
	INX	;+3
	INX	;+4
	INX	;+5
	INX ;+6
	CPX #NPC.PATHGENERATOR.QUE.SIZE	;at end of array?
	BNE .LOOP.SCHEDULE.QUE			;if no, continue loop
									;if yes, then exit as there are no more records to process
								
	;**FALLS THROUGH**
.EXIT


				
			; LDA #$01					;TO PREVENT MULTIPLE RUNS DURING TESTING
			; STA COMPLETED.FLAG 
	
	JMP NPC.PATHGENERATOR.EXIT
	
	
@END

	
GET.PATH
@START
;PARAMETERS: NPC.PATHFINDER.DESTINATION.TILE.X, NPC.PATHFINDER.DESTINATION.TILE.Y, PATHFINDER.SPRITE.RECORD($B), 	
;ENTRANCE: NPC.PATHGENERATOR
;RETURN: NPC.PATHFINDER.FINAL.PATH

		;set auto-abort parameter
		LDA #KEYPRESS.ABORT.ITERATIONS.DEFAULT	;set to KEYPRESS.ABORT.ITERATIONS.DEFAULT or set to $FF to disable auto-abort
		;LDA #$FF
		STA KEYPRESS.ABORT.ITERATIONS


			
	JSR NPC.PATHFINDER
		LDA NPC.PATHFINDER.ABORT_FLAG
		BEQ .FIND.OPEN.RECORD
		JMP .EXIT.ABORT

	; .FIND.OPEN.RECORD	
	; ;FIND OPEN RECORD FOR SAVED PATH
		; LDX #$00
		; STX SAVED.PATH.NUMBER					;Counts iterations of the SAVED.PATH array. Used to set SAVED.PATH.NUMBER
	; .LOOP.FIND.OPEN_RECORD
		; LDA SAVED.PATH.LOOKUP.TABLE+$2,X		;status field. $01 = SAVE, $00 = DISCARD
		; CMP #$00
		; BEQ	.SAVE.RECORD
		; INC SAVED.PATH.NUMBER
		; INX
		; INX	
		; INX
		; INX ;advance to next record
		; CPX #SAVED.PATH.LOOKUP.TABLE.SIZE
		; BNE .LOOP.FIND.OPEN_RECORD

.FIND.OPEN.RECORD		
;FIND OPEN RECORD IN SAVED PATH TABLE	
	;INIT VARIABLES, COUNTERS, INDEXES	
	LDX #SAVED.PATH.LOOKUP.TABLE.LAST_RECORD_START	;NPC.PATHGENERATOR.QUE index, set to the last byte (For a $3C byte array, the last record start is $36, not $37, because $0 is the first byte)
	;Y-REG = schedule record byte $2
	LDA #SAVED.PATH.MAXIMUM
	STA SAVED.PATH.COUNTER			;Decrements as the records of SAVED.PATH.LOOKUP.TABLE are iterated. Used as the HO Byte index to the saves paths in aux memory. 

	LDA #$FF						;set a default value so we can tell if the que is full
	STA SAVED.PATH.LOOKUP.TABLE.INDEX
	
.LOOP.FIND.OPEN_RECORD ;and verify path to be saved is not a duplicate
	LDA SAVED.PATH.LOOKUP.TABLE+$2,X		;status field. $01 = SAVE, $FF = DISCARD
	CMP #$FF						;is record marked for discard?
	BNE .CONTINUE					;if no, continue
	STX SAVED.PATH.LOOKUP.TABLE.INDEX		;if yes, save que index (so that if we have to add a record, we've got the index to an empty record handy) 
	LDA SAVED.PATH.COUNTER	
	STA SAVED.PATH.NUMBER			;this makes it so the path number used will tie back to the last empty record found
	
.CONTINUE	
;IS THERE ALREADY AN ACTIVE RECORD IN LOOKUP TABLE FOR THIS NPC?
;(i.e. not marked as complete/empty?)
	LDA SAVED.PATH.LOOKUP.TABLE+$0,X	;load Anchor # of current table record
	CMP PATHFINDER.SPRITE.RECORD+$4 	;load Anchor # of path to be saved
	BNE .NEXT.QUE_RECORD			;if no match, then next table record
	LDA SAVED.PATH.LOOKUP.TABLE+$1,X	;load NPC Record # of current table record
	CMP PATHFINDER.SPRITE.RECORD+$B 	;load NPC Record # of path to be saved
	BNE .NEXT.QUE_RECORD			;if no match, then next table record
	LDA SAVED.PATH.LOOKUP.TABLE+$2,X	;load status field
	CMP #$FF						;is record empty/completed?	
	BNE .EXISTING.RECORD.FOUND		;if yes, the no need to save the path as it has the same Anchor # and NPC Record # of an active path record in the table.
									;if no match check next table record								
.NEXT.QUE_RECORD
;END-OF-ARRAY CHECK									
	DEC SAVED.PATH.COUNTER

	CPX #$00						;at end of array?
	BEQ .SAVE.RECORD				;if yes, no existing record found
		
	DEX	;+1
	DEX ;+2
	DEX	;+3
	DEX	;+4
	
	JMP .LOOP.FIND.OPEN_RECORD		;if no, continue loop

.EXISTING.RECORD.FOUND
;An active record matching the Anchor # and NPC Record # of the path
;to be saved was found. This can happen if the player stays
;idle long enough that all paths in the que are generated
	
	;**Discarding path without saving***
	JMP .EXIT.STANDARD
	
.SAVE.RECORD
;UPDATE SAVED PATH RECORD	
;MARK RECORD STATUS AS "SAVE"
;SET STATUS FIELD TO "SAVE"
;(this prevents the record from being overwritten by new records)

;ERROR CHECK: QUE FULL?


				
	LDA SAVED.PATH.LOOKUP.TABLE.INDEX
	CMP #$FF							;is the que full?
	BEQ .ERROR.SAVED.PATH.TABLE.FULL	;if yes, report error
	
	LDX SAVED.PATH.LOOKUP.TABLE.INDEX	;load index to last open record found
	
	;Set Anchor #
	LDA PATHFINDER.SPRITE.RECORD+$4
	STA SAVED.PATH.LOOKUP.TABLE+$0,X
	
	;Set NPC Array Record #
	LDA PATHFINDER.SPRITE.RECORD+$B
	STA SAVED.PATH.LOOKUP.TABLE+$1,X
	
	;Set Path Status
	LDA #$01							;$01 = SAVE, $FF = DISCARD
	STA SAVED.PATH.LOOKUP.TABLE+$2,X
	
	;Set Path #
	LDA	SAVED.PATH.NUMBER
	STA SAVED.PATH.LOOKUP.TABLE+$3,X

				
		;SAVED.PATH.NUMBER already set ;parameter
	JSR SAVE.PATH.AUX_MEMORY

;TROUBLESHOOTING HOOK			
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
				; LDA #$AA
				; JSR PREP.BRK
				; BRK
	
	;**FALLS THROUGH**


.EXIT.STANDARD
	RTS		;return to PROCESS.SCHEDULE_QUE.RECORDS
	
	
.EXIT.ABORT
	
	;***DEPENDING ON WHETHER GET.PATH IS CALLED BY JSR,
	;THIS ROUTINE WILL EITHER RTS (FROM THE ORIGINAL CALL
	;TO NPC.PATHGENERATOR) OR IT WILL POP ONE OR MORE RTS OFF
	;THE STACK, THEN RTS
	
	
	; LDA #$AB	
	; JSR PREP.BRK
	; BRK	
	
	PLA
	PLA		;POP 1 RTS OFF THE STACK

	RTS		;return to the JSR NPC.PATHGENERATOR in GAME.PRIMARY_LOOP
	


.ERROR.SAVED.PATH.TABLE.FULL
;ERROR: NO OPEN RECORD FOUND
;.LOOP.FIND.OPEN_RECORD reports no open record found in NPC.PATHGENERATOR.SAVED_PATHS.AUX 
;This could be the result too many NPCs currently transitioning (tieing up records)
;and too many NPCs scheduled to transition soon (for which this subroutine is trying 
;to generate and save paths; requiring records.)
			; LDA #$AA
			; ; LDA NPC.SCHEDULE.WORKSPACE+$2,X
			; ; TAX
			; ; LDA (NPC.SCHEDULE.POINTER),Y
			;
			; LDX #NPC.SCHEDULE.WORKSPACE
			; LDY /NPC.SCHEDULE.WORKSPACE
			; JSR PREP.BRK
			; BRK 
			

				; LDX #SAVED.PATH.LOOKUP.TABLE
				; LDY /SAVED.PATH.LOOKUP.TABLE
				; JSR PREP.BRK

	JSR PREP.BRK
	BRK

@END
	
	
SAVE.PATH.AUX_MEMORY
@START

;PARAMETERS: SAVED.PATH.HO_ADDRESS.OFFSET, SAVED.PATH.NUMBER
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
	LDA SAVED.PATH.NUMBER 						;Used as the HO byte offset for the open record we're overwriting
	ADC /NPC.PATHGENERATOR.SAVED_PATHS.AUX		;16-BIT ADD
	STA AUX_MOVE.DEST+$1
	
	
	SEC                ;SET CARRY FLAG DESGINATD MOVE FROM MAIN MEMORY -> AUX
	JSR AUX_MOVE	

	RTS	;return to GET.PATH
	
@END
	

NPC.PATHGENERATOR.EXIT
	

	RTS
	
@END

	
NPC.PATHFINDER			
@START			
;PARAMETERS: NPC.PATHFINDER.DESTINATION.TILE.X, NPC.PATHFINDER.DESTINATION.TILE.Y, PATHFINDER.SPRITE.RECORD($B), KEYPRESS.ABORT.ITERATIONS, NPC.PATHFINDER.ADOC.FLAG
;ENTRANCE:DIRECT
;RETURN: NPC.PATHFINDER.FINAL.PATH*, ACC = NEXT MOVE DIRECTION CODE (Only if KEYPRESS.ABORT.ITERATIONS = $FF)
;*The first byte contains the index to record containing the x,y coordinates of the NPCs next move (which is the 2nd to last record in the final path, the last record contains the x,y coordinates of the NPC's current position)
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

;=====================SUBROUTINE DOCUMENTATION====================================
;
;==OVERVIEW==
;*Calculates a specific path based on the information 
;provided in PATHFINDER.SPRITE.RECORD, which is populated
;by NPC.PATHGENERATOR.
;
;*Auto-aborts after 3 iterations of it's primary to give 
;automation and player movement a chance to occur.
;
;
;==BACKGROUND==
;This routines uses a stripped down, modified version of
;the A* algorithm. Since distance isn't really considered,
;it is most simular to the Greedy's Best Path First algorithm
;which is effectively a component of A*.
;
;A graph based approach is used rather than the entire 
;map grid.
;
;For more information on A*, Greedy's Best Path First 
;and the concepts of graph and grid see:
;http://www.redblobgames.com/pathfinding/a-star/introduction.html
;
;==WALKTHROUGH==
;
;Current Tile refers to the tile which the pathfinder
;algorithm is currently evaluating. The sprite's starting
;X,Y coordinates are always the first Current Tile.
;
;The adjacent tile types to the current Tile are tested 
;to see if they are valid Graph Points. (special tile_type IDs
;are used to draw an invisible path on the floor, road, grass and
;in the future other tiles which make up the Graph Points).
;
;If an adjacent tile is a valid Graph Point then it is treated
;as an Open Path because it is a valid direction for the 
;NPC to move. 
;
;Each Open Path tile is evaluated to see if it qualifies to be
;treated as a new "Neighbor". The only criteria is that the Open Path
;tile cannot have already been added as a new neighbor. This is
;determines by checking the NPC.PATHFINDER.CLOSED_SET array, which
;is a stencil of the building map (just the 33tile X 33tile area at
;the center which Sprites can reside in)
;  
;All new Neigbors are added to the NPC.PATHFINDER.SEARCH.PATHS array.
;The distance from each Neighbor to the destination anchor
;is calculated and used to add each Neighbor to the 
;NPC.PATHFINDER.PRIORITY.QUE array, which is what determines
;the order in which neighbors are evaluted for open paths.
;
;For each Current Tile, a variable is used to track 
;whether any of the Neighbors have a distance to the destination
;anchor which is less than the the Current Tile.
;
;Any Neigbor which passes this test is put at the first
;position in the priority que. Otherwise, Neighbors are
;added to the bottom. If no Neighbor passes this check
;then the que is sorted. 
;
;The next tile is loaded using the priority que as a guide
;and the next tile becomes the new Current Tile.
;If the next tile is the destination tile then the algorithm
;main loop is terminated and control is transfered to
;ASSEMBLE.FINAL.PATH. 
;
;ASSEMBLE.FINAL.PATH uses the NPC.PATHFINDER.SEARCH.PATHS
;array to assemble the RMAP.X/Y coordinates of each tile
;in the path from the sprite's current position to the
;destination anchor. 
;=================================================================================

			
;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA

				; STA TEMP
				; LDA PATHFINDER.SPRITE.RECORD+$0B
				; CMP #$01
				; BNE .TEMP
				; ; LDA PATHFINDER.STREET.PREFERENCE
				; ; LDX NPC.PATHFINDER.DESTINATION.TILE.X
				; ; LDY NPC.PATHFINDER.DESTINATION.TILE.Y	
				; ;LDX #NPC.PATHGENERATOR.QUE
				; ;LDY /NPC.PATHGENERATOR.QUE
				; LDX #PATHFINDER.SPRITE.RECORD
				; LDY /PATHFINDER.SPRITE.RECORD
				; JSR PREP.BRK
				; BRK					
; .TEMP
				; LDA TEMP
				
.DRIVER
@START

;SET DESTINATION
;
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

	LDY SEARCH.PATHS.LO_INDEX

	LDA	SEARCH.PATHS.POINTER.SAVED				;Setup pointer for the search path array so it can be accessed via indirect index by Y mode.
	STA NPC.PATHFINDER.SEARCH.PATHS.POINTER
	LDA	SEARCH.PATHS.POINTER.SAVED+$1
	STA NPC.PATHFINDER.SEARCH.PATHS.POINTER+$1
	

	
	JMP OUTER.LOOP
			
.INIT.STANDARD
@START
			;JSR APPLE_BELL
;ERROR CHECK: ARE THE NPC'S CURRENT AND DESTINATION COORDINATES THE SAME?
	LDA NPC.PATHFINDER.DESTINATION.TILE.X
	CMP PATHFINDER.SPRITE.RECORD+$8
	BNE .START.INIT

	LDA NPC.PATHFINDER.DESTINATION.TILE.Y
	CMP PATHFINDER.SPRITE.RECORD+$9
	BNE .START.INIT
	
.ERROR.SAME.COORDINATES
;NPC.PATHFINDER REPORTS THAT THE START AND DESTINATION 
;COORDINATES PROVIDED AS PARAMETERS ARE THE SAME. 
				
	JSR PREP.BRK
	BRK
	

.START.INIT

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
	STA ITERATION.COUNTER 					;tracks timing of auto-abort. Also helpful for troubleshooting

	LDA #$FF
	STA PATH_TILE.SHORTEST.DISTANCE			;set the distance really high so it is easy to beat. The first neighbor tile discovered should surpass this value as the shortest distance. 			

	LDA #$00
	STA NPC.PATHFINDER.CURRENT.TILE	
	STA NPC.PATHFINDER.PRIORITY.QUE.SIZE+$1 ;the pathfinder que won't exceed an 8-bit record size value so we're doing an init of the 2nd byte here to make sure it's $00, the correct value for an 8-bit size stored in the 1st byte.
	STA NPC.PATHFINDER.PRIORITY.QUE.PROCESSED.RECORDS

	; LDA #$10
	; STA PATHFINDER.STREET.PREFERENCE		;set weight for non-road tiles

	
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
	
	
;CONVERT CURRENT TILE.XY TO CURRENT.TILE.RMAP
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
;======================================	
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
	STA MOB.ADJACENT_TILES+$3			;BORROWING SCREEN.TILE.HOPPER TO STORE THE TILE TYPE ADJACENT TO THE NPC	

;
;				
;SAVE TILE # EAST OF NPC
;======================================		
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
	STA MOB.ADJACENT_TILES+$2			;BORROWING SCREEN.TILE.HOPPER TO STORE THE TILE TYPE ADJACENT TO THE NPC		
				
;
;	
;SAVE TILE # SOUTH OF NPC
;======================================	
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
	STA MOB.ADJACENT_TILES+$1				;BORROWING SCREEN.TILE.HOPPER TO STORE THE TILE TYPE ADJACENT TO THE NPC	

@MIDDLE

;NPC GROUP: DETERMINE WHETHER TO APPLY LAND OR SEA NPC COLLISION RULES	
	LDA PATHFINDER.SPRITE.RECORD+$2
	CMP #COLLISION_FLAG.MOB_SEA.START			;FIRST TILE IN SEA NPC RANGE. 
	BCS .COLLISION.LOOP.MOB_SEA


;APPLY MOB/NPC WALKING COLLISION RULES		
	LDY #$00
.LOOP.MOB_LAND

.CHECK.ADHOC.FLAG ;MOTH-BALLED
@START
	;****THIS SECTION ISN'T NEEDED UNLESS ADHOC.SPRITE.PATHFINDER SUBROUTINE IS ENABLED.  

	LDA NPC.PATHFINDER.ADOC.FLAG
	CMP #$FF						;is the adhoc flag set?
	BEQ .STANDARD.COLLISION.TESTS	;if yes, then use standard collision tests rather than the grid tile tests. This is because when in adhoc mode pathfinder needs to look at the actual terrain types to determine whether a path is open.
@END
	
	
	LDA MOB.ADJACENT_TILES,Y					;LOAD NEXT ADJACENT TILE TYPE


;MARK PATHFINDER GRID TILES, SUCH AS NON-FLOOR-PATH/ROAD TILE, AS BLOCKED
;(floor-path & road tiles are automatically marked as permitted, skipping the rest of the
;normal collision tests. All other tiles are automatically marked as blocked.
;This is so NPCs will use road/floor-path tiles to get to their
;destination, and the algorithm will not record any other tiles
;as neigbors, increasing efficiency. 


			; STA TEMP
			; ; LDA TROUBLESHOOTING.HOOK
			; ; CMP #$01
			; ; BNE .TEMP
			; LDY #$00
			; BNE .TEMP
			; JSR PREP.BRK
			; LDA #$AA
			; LDX MOB.MOVES.BLOCKED,Y
			; BRK
; .TEMP
			; LDA TEMP


.CHECK.FOR.PATHFINDER_GRID.TILES
			
.ROAD.TEST
	CMP #TILE_ID.ROAD.GRE1
	BCC .ROAD.TEST.COMPLETE
	CMP #TILE_ID.ROAD.LT1
	BCC .MOVE_PERMITTED  				;tile is road
	;**FALLS THROUGH**
.ROAD.TEST.COMPLETE
	;**FALLS THROUGH**
	
.GROUP_A.TEST
	CMP #TILE_ID.NOXA_GRID.GROUP_A.GRE1
	BCC .GROUP_A.TEST_COMPLETE
	CMP #TILE_ID.NOXA_GRID.GROUP_A.LT1
	BCC .MOVE_PERMITTED			 		;tile is floor
.GROUP_A.TEST_COMPLETE
	;**FALLS THROUGH**
	
.GROUP_B.TEST
	CMP #TILE_ID.NOXA_GRID.GROUP_B.GRE1
	BCC .GROUP_B.TEST_COMPLETE			 		;tile might be ladder
	CMP #TILE_ID.NOXA_GRID.GROUP_B.LT1
	BCC	.MOVE_PERMITTED			 		;tile is ladder
.GROUP_B.TEST_COMPLETE
	;**FALLS THROUGH**	

	;default JMP after last test
	JMP .MOVE_BLOCKED	
	
	
; ;LAST TEST
; .BED.TEST1
	; CMP #TILE_ID.BED_COT.GRE1
	; BCS .BED.TEST2				 		;tile might be a bed or cot
	; JMP .MOVE_BLOCKED					;this is last test, so tile is not a grid tile, so path is not open. 
; .BED.TEST2
	; CMP #TILE_ID.BED_COT.LT1
	; BCC .MOVE_PERMITTED			 		;tile is a bed or cot
	;JMP .MOVE_BLOCKED					;this is last test, so tile is not a grid tile, so path is not open. 


.STANDARD.COLLISION.TESTS ;MOTH-BALLED
@START

	;****THIS SECTION ISN'T NEEDED UNLESS ADHOC.SPRITE.PATHFINDER SUBROUTINE IS ENABLED.  

	;****ALSO ENABLED .CHECK.ADHOC.FLAG ABOVE
	
; ;IS NPC COLLISION OVERRIDE ON?
	; LDA MOB.COLLISION_OVERRIDE
	; CMP #$01
	; BEQ .MOVE_PERMITTED

; ;BEGIN MAIN TESTS
	; LDA MOB.ADJACENT_TILES,Y			;LOAD NEXT ADJACENT TILE TYPE

	; CMP #COLLISION_FLAG.MOB_LAND.LT1	;ABSOLUTE OBSTACLE?
	; BCC .MOVE_BLOCKED

	; CMP #COLLISION_FLAG.MOB_LAND.EQ1	;QUICKSAND?
	; BEQ .MOVE_BLOCKED
	
	; CMP #COLLISION_FLAG.MOB_LAND.GRE	;WATER?	
	; BCS .WATER_TEST1.PASS
	; JMP .NEXT_COLLISION_TEST
; .WATER_TEST1.PASS
	; CMP #COLLISION_FLAG.MOB_LAND.LT2	;WATER?	
	; BCC .MOVE_BLOCKED
	
; .NEXT_COLLISION_TEST	
; ;NO MORE TESTS FOR NOW
	; ;JMP .MOVE_PERMITTED
	; ;**FALLS THROUGH**
	
@END

@MIDDLE

	;**FALLS THROUGH**
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
;=====================CODE-SECTION DOCUMENTATION====================================
;
;This routine determines the order in which Open Paths 
;from the current tile are evaluated as candidate neighbors. 
;The order is relevant because if two Neighbors are the 
;same distance from the destination, the order which they
;are added to the priority que can affect which path the
;sprite takes. Basically, the order breaks a tie in distance. 
;
;This routines does this by using random number to pick
;an element of MOB.MOVES.BLOCKED, which stores the open ($00)
;/ blocked ($01) status of each direction/path from the Current Tile.
;
;It keeps track of the direction/paths checked via the
;NPC.PATHFINDER.DIRECTIONS_CHECKED array. If a direction
;has been checked it gets a new random number and repeats
;the process until all directions have been checked. The
;random numbers result in Sprites sometimes taking different
;paths between the same start and destination coordinates. 
;
;Ultimately all of this rigamarole sets the value in X-REG
;which is a parameter for direction/path when the JSR PATH.OPEN
;is executed. 
;
;==ATERNATE ROUTINE==
;
;Sometimes it is desireable in troubleshooting to remove
;the randomoness described above. If that is the case
;comment out this section LOOP.ACQUIRE.NEIGHBOR.TILES and
;uncomment LOOP.ACQUIRE.NEIGHBOR.TILES.PREFER_WEST, which 
;will use the order: west, north, south, east. 
;
;
;=================================================================================


		
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
	LDA MOB.MOVES.BLOCKED+$0
	CLC
	ADC MOB.MOVES.BLOCKED+$1
	ADC MOB.MOVES.BLOCKED+$2
	ADC MOB.MOVES.BLOCKED+$3
	CMP #$04
	BEQ .ERROR.ALL.PATHS.BLOCKED
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
	
.ERROR.ALL.PATHS.BLOCKED
;LOOP.ACQUIRE.NEIGHBOR.TILES reports that all paths from the
;current tile are blocked (MOB.MOVES.BLOCKED+$0-$3 = $01)
;
	
	JSR PREP.BRK
	BRK

;;********KEEP THIS SECTION******
; LOOP.ACQUIRE.NEIGHBOR.TILES.PREFER_WEST
;
;;=====================CODE-SECTION DOCUMENTATION====================================
;;See documentation above in LOOP.ACQUIRE.NEIGHBOR.TILES
;;=================================================================================
;;
; ;#+$00=north, +$01=south, +$02=east, +$03=west
; ;.TEST.WEST
	; LDX #$03
	; LDA MOB.MOVES.BLOCKED+$3				;load the collision status (west) for the current tile. $00 = open. $01 = blocked.  
	; CMP #$00								;is direction blocked?
	; BNE .TEST_NORTH							;if yes, then next direction test 
	; JSR PATH.OPEN							;if no, then a candidate new neigbor has been found. Evaluate this open path.
	; ;
; .TEST_NORTH
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
	; BNE .NEXT.PATH.TILE_STEP				;if yes, then next direction test 
	; JSR PATH.OPEN							;if no, then a candidate new neigbor has been found. Evaluate this open path.
	
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
	
	;**OPT** Memory. If the memory occupied by the close set array remains in the main memory buffer, we could increase the size of the closet set array to eliminate the need to converty the RMAP value to the building grid. This would save memory and increase speed, and the extra memory in the buffer is clobbered anyway. Unless the pathfinder arrays are copied to aux memory when other modules are loaded into the buffer such as NPC conversation. Then maybe it should stay as-is, unless aux memory will likely be plentiful after the whole game is done. 
	LDA NPC.PATHFINDER.NEIGHBOR.X			;load neighbor x-axis
	SEC
	SBC #CLOSED_SET.X.ADJ
	STA NEIGHBOR.ADJUSTED.X					;Convert to Building Map Grid
		;**OPT** Speed. Memory. It may be possible to do the ADJUSTED.X/Y calculation in the code section above. 
	
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
	LDX ACQUIRE.LOOP.COUNTER  ;**OPT** Speed. Memory. This may not be needed because the direction codes aren't fed into PATH.OPEN by a loop with an iteration based exit anymore since it the direction code is now selected using random numbers.  
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
	JSR PREP.BRK
	BRK

.ERROR.TILE_NUMBER.OVERFLOW
;Overflow error reported by .ADD.ACQUIRED.NEIGHBOR in NPC.PATHFINDER.	
;due to NPC.PATHFINDER.TILE_NUMBER.TALLY flipping over to $00.
;The causes for this are along the same line of what is described
;in	.ERROR.SEARCH_ARRAY.OVERFLOW


			JSR GENERATE.DEBUG.LOG
			
			LDA PATHFINDER.SPRITE.RECORD+$8
			STA DEBUG.LOG+$100
			LDA PATHFINDER.SPRITE.RECORD+$9
			STA DEBUG.LOG+$101	
			LDA NPC.PATHFINDER.DESTINATION.TILE.X
			STA DEBUG.LOG+$102
			LDA NPC.PATHFINDER.DESTINATION.TILE.Y
			STA DEBUG.LOG+$103
				
				JSR PREP.BRK
				BRK
	
@END

.CALCULATE.DISTANCE			
@START

		LDA NPC.PATHFINDER.NEIGHBOR.X
		STA PARM1.GMAP.X
		
		LDA NPC.PATHFINDER.DESTINATION.TILE.X
		STA PARM2.GMAP.X
		
		LDA NPC.PATHFINDER.NEIGHBOR.Y
		STA PARM1.GMAP.Y
		
		LDA NPC.PATHFINDER.DESTINATION.TILE.Y
		STA PARM2.GMAP.Y
			
	JSR CALCULATE.DISTANCE
		;ACC = RETURN VALUE
		STA NPC.PATHFINDER.CURRENT.DISTANCE


			
			
; ;WEIGHT DISTANCE FOR NON-FLOOR/ROAD TILES	
; ;(this is so NPCs will use road/floor tiles to get to their
; ;destination if possible)
; .ROAD.TEST1
	; LDA MOB.ADJACENT_TILES,X
	; CMP #TILE_ID.ROAD.GRE1
	; BCS .ROAD.TEST2
	; JMP .FLOOR.TEST
	; ;JMP .NOT.ROAD
; .ROAD.TEST2
	; CMP #TILE_ID.ROAD.LT1
	; BCC .FINAL.DISTANCE.COMPLETE  ;tile is road, the final distance value previously saves stands.
	; ;**FALLS THROUGH**
; .FLOOR.TEST
	; CMP #TILE_ID.FLOOR_PATH.GRE1
	; BCS .FLOOR.TEST2
	; JMP .NOT.ROAD.OR.FLOOR_PATH
; .FLOOR.TEST2
	; CMP #TILE_ID.FLOOR_PATH.LT1
	; BCC .FINAL.DISTANCE.COMPLETE  ;tile is road, the final distance value previously saves stands.
	; ;**FALLS THROUGH**	
; .NOT.ROAD.OR.FLOOR_PATH
	; LDA NPC.PATHFINDER.CURRENT.DISTANCE
	; CLC
	; ADC PATHFINDER.STREET.PREFERENCE
	; STA NPC.PATHFINDER.CURRENT.DISTANCE

	
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
;=====================CODE-SECTION DOCUMENTATION====================================
;
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
;
;=================================================================================
		
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
;NOTE:
;Exit test no longer needed here. This subroutine (PATH.OPEN) is
;now called by JSR from LOOP.ACQUIRE.NEIGHBOR.TILES.PREFER_WEST
;or, if enabled, LOOP.ACQUIRE.NEIGHBOR.TILES. Both of these
;calling routines do their own exit cehck. 
			
		
.EXIT	
	
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


;OLD EXIT CHECK CODE				
	; INX				;increment counter for .LOOP.ACQUIRE.NEIGHBOR.TILES
	; CPX #$04
	; BEQ .EXIT
	;JMP LOOP.ACQUIRE.NEIGHBOR.TILES	
	
	RTS
	
	;**USED TO FALL THROUGH

	
@END	
@END

	
NEXT.PATH.TILE					
@START

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
	

;UPDATE QUE LENGTH HEADER	
	LDA NPC.PATHFINDER.PRIORITY.QUE.RECORD_TALLY	;use the number of records in the que * 2 as the index to add the new record to the end of the que. This works even though the 1st two bytes of the array are reserved for the size, in bytes, of the data portion of the array because when a record is erased the 1st byte of the record (distance) is set to $FF. This records gets sorted to the end of the que, so the index will point to this record, which is good because we want to overwrite it. 
	ASL ;X2											;do X2 because the record size is $2 bytes. 
	STA NPC.PATHFINDER.PRIORITY.QUE+$0		;the first two bytes of the array are reserved to store the array data length in bytes
			

		;setup record size
		LDA #$02
		STA array.record_size			
		;setup pointer
		LDA #NPC.PATHFINDER.PRIORITY.QUE		
		STA sort.table.address
		LDX /NPC.PATHFINDER.PRIORITY.QUE	
		STX sort.table.address+$1
	JSR INSERTION_SORT
		;**OPT**. Memory. reduce the variable definition sizes for unsorted_record.values and unsorted_record.values_plus1
		;down to the largest record size used in an insert sort in Nox Archaist. It currently takes up !16bytes * 2 to 
		;support a record size up to $10, but that may be way more than needed. 
		;Additionaly, unsorted_record.values_plus1's only purpose is to faciliate the comparison of record keys. Unless
		;Nox Archaist has a sort with multiple record keys, then the code in the insertion sort could be modified so that
		;only the record key is stored in unsorted_record.values_plus1
		
.SORT.COMPLETE	
	
@END
		
.LOAD.NEXT.TILE	;LOAD NEXT TILE FROM PRIORITY QUE	
@START	
;=====================CODE-SECTION DOCUMENTATION====================================
;
;Code exists in this section for a multiplication
;calculation method and a multiplication table method
;for calculating the address of a given record in 
;NPC.PATHFINDER.SEARCH.PATHS. The table method is of
;course faster but takes a lot of memory. After doing
;some speed test it appeared that using the table method
;didn't increase the speed by a meaningful amount so
;I went with the calculation method. By meaningful, I mean
;either way, fOr NPCs, the pathfinder would have to run 
;in the background and save paths for future use rather 
;than run ad hoc when an NPC moves. 
;
;ASSEMBLE.FINAL.PATH also simular multiplication code,
;both methods. 
;
;=================================================================================


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
	CMP KEYPRESS.ABORT.ITERATIONS		;has the auto-abort threashold (# of iterations since pathfinder was called) been reached?
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
	JSR PREP.BRK
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
	JSR PREP.BRK
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

@END

	
ASSEMBLE.FINAL.PATH		
@START			
;=====================CODE-SECTION DOCUMENTATION====================================
;
;Code exists in this section for a multiplication
;calculation method and a multiplication table method
;for calculating the address of a given record in 
;NPC.PATHFINDER.SEARCH.PATHS. The table method is of
;course faster but takes a lot of memory. After doing
;some speed test it appeared that using the table method
;didn't increase the speed by a meaningful amount so
;I went with the calculation method. By meaningful, I mean
;either way, fOr NPCs, the pathfinder would have to run 
;in the background and save paths for future use rather 
;than run ad hoc when an NPC moves. 
;
;.LOAD.NEXT.METHOD also simular multiplication code,
;both methods. 
;
;=================================================================================


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
	STA NPC.PATHFINDER.FINAL.PATH+$5					;write Y-AXIS
	
	DEY	;back up to byte $2 of the search path array record
	
	LDA (NPC.PATHFINDER.SEARCH.PATHS.POINTER2),Y
	STA NPC.PATHFINDER.FINAL.PATH+$4					;write X-AXIS
	
		
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
	JSR PREP.BRK
	BRK

@END

ERROR.OVERFLOW
;MULTIPLICATION TABLE OVERFLOW IN .ADD_PRIORITY.QUE
;likely caused by NPC.PATHFINDER.TILE_NUMBER.TALLY exceeding
;the last index in the multiplication table. 
;(NPC.PATHFINDER.MULTIPLY_TABLE.LO/HO)
	JSR PREP.BRK
	BRK
	
NPC.PATHFINDER.EXIT
;MARK THIS PATH AS COMPLETE. 
	LDX PATHGENERATOR.QUE.INDEX
	LDA #$FF
	STA NPC.PATHGENERATOR.QUE+$5,X	;set status field in the scheduler que to complete. 

	
	
			; STA TEMP
			; ; LDA TROUBLESHOOTING.HOOK
			; ; CMP #$01
			; ; BNE .TEMP
			; ;LDA PATHFINDER.SPRITE.RECORD+$04
			; LDA PATHFINDER.SPRITE.RECORD+$0B
			; CMP #$18
			; ;CMP #$08
			; BNE .TEMP
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP
			
			
	; LDA NPC.PATHFINDER.ADOC.FLAG
	; CMP #$FF						;is the adhoc flag set?
	; BNE .EXIT						;if no, then exit
.RETURN.NEXT_MOVE.DIRECTION_CODE	;MOTH-BALLED  ;If yes, then the calling routine is expecting a direction code as the return value. Only the adhoc pathfinder routine turns the adhoc flag on and the adhoc pathfinder is setup to determine the next move for the sprite based on the returned direction code. 
@START
	;****THIS SEECTION ISN'T NEEDED UNLESS ADHOC.SPRITE.PATHFINDER SUBROUTINE IS ENABLED.  
	; LDA #$00
	; STA NPC.PATHFINDER.ADOC.FLAG	;clear the flag that puts NPC.PATHFINDER in adhoc mode.

	
	
; ;CONVERT SPRITE RMAP 	
	
	; LDY #$04						;byte $04 of final path array record
	
	; LDA (NPC.PATHFINDER.SEARCH.PATHS.POINTER2),Y ;load x-axis of next move
	; CMP PATHFINDER.SPRITE.RECORD+$08



	; BEQ .NORTH.OR.SOUTH
; ;.EAST.OR.WEST
	; CMP PATHFINDER.SPRITE.RECORD+$08			;X-axis of NPC current position
	; BCC .MOVE.WEST
; ;.MOVE.EAST
	; LDA #$02						;load movement code for east in ACC as return value
	; JMP .EXIT
	
; .MOVE.WEST
	; LDA #$03						;load movement code for west in ACC as return value
	; JMP .EXIT
	
; .NORTH.OR.SOUTH
	; INY								;advance to byte $05 of final path array record
	; LDA (NPC.PATHFINDER.SEARCH.PATHS.POINTER2),Y ;load Y-axis of next move
	
	; CMP PATHFINDER.SPRITE.RECORD+$09			;Y-axis of NPC current position
	; BCC .MOVE.NORTH
; ;.MOVE.SOUTH
	; LDA #$01						;load movement code for south in ACC as return value
	; JMP .EXIT
	
; .MOVE.NORTH
	; LDA #$00						;load movement code for north in ACC as return value
	; ;**FALLS THROUGH**
@END

.EXIT	
;RESTORE REGISTERS
	
	STA SAVED.ACC.LOCAL		;save movement code, if one was generated
	
	PLA
	TAY
	PLA
	TAX
	
	LDA SAVED.ACC.LOCAL		;restore movement code, if one was generated

	
				;JSR APPLE_BELL

	RTS
	
@END	


LOAD.PATH.INDEX
@START
;PARAMETERS: X-REG (index to SAVED.PATH.LOOKUP.TABLE)
;RETURN: TRANSIT.NEXT_MOVE.INDEX
;ENTRANCE: direct

;=====================SUBROUTINE DOCUMENTATION====================================
;
;Returns the path index saved in the first byte of the saved path number 
;specified by X-REG. 
;
;Since the saves paths have 1 page of memory reserved each,
;the exact aux address is calculated by using
;the path # (X-REG) as an offset to the base address of the
;saves paths in aux memory. 
;
;=================================================================================


	;COPY PATH INDEX (aux memory -> main memory)	

	;SET AUX START & END ADDRESS
	LDA #NPC.PATHGENERATOR.SAVED_PATHS.AUX
	STA AUX_MOVE.START
	CLC
	ADC #$01
	STA AUX_MOVE.END
	LDA SAVED.PATH.LOOKUP.TABLE+$3,X 			;Use path # in the saved path table for the HO byte offset
	ADC /NPC.PATHGENERATOR.SAVED_PATHS.AUX		;16-BIT ADD
	STA AUX_MOVE.END+$1
	STA AUX_MOVE.START+$1
	
	;SET MAIN MEMORY DESTINATION ADDRESS
	LDA #TRANSIT.NEXT_MOVE.INDEX
	STA AUX_MOVE.DEST
	LDA /TRANSIT.NEXT_MOVE.INDEX 				
	STA AUX_MOVE.DEST+$1
	
	
	CLC											 ;SET CARRY FLAG DESGINATD MOVE FROM AUX -> MAIN MEMORY
	JSR AUX_MOVE	
	
	
	RTS
	
@END

SAVE.PATH.INDEX
@START
;PARAMETERS: X-REG (index to SAVED.PATH.LOOKUP.TABLE)
;RETURN: none
;ENTRANCE: direct

;=====================SUBROUTINE DOCUMENTATION====================================
;
;Saves the path index (TRANSIT.NEXT_MOVE.INDEX) 
;to the first byte of the saved path number 
;specified by X-REG. 
;
;Since the saves paths have 1 page of memory reserved each,
;the exact aux address is calculated by using
;the path # (X-REG) as an offset to the base address of the
;saves paths in aux memory. 
;
;=================================================================================



;COPY PATH INDEX (aux memory -> main memory)		
	;SET AUX START & END ADDRESS
	LDA #TRANSIT.NEXT_MOVE.INDEX
	STA AUX_MOVE.START
	STA AUX_MOVE.END
	LDA /TRANSIT.NEXT_MOVE.INDEX
	STA AUX_MOVE.END+$1
	STA AUX_MOVE.START+$1
	
	;SET AUX DESTINATION ADDRESS
	LDA #NPC.PATHGENERATOR.SAVED_PATHS.AUX
	STA AUX_MOVE.DEST
	LDA SAVED.PATH.LOOKUP.TABLE+$3,X 			;Use path # in the saved path table for the HO byte offset
	CLC
	ADC /NPC.PATHGENERATOR.SAVED_PATHS.AUX					
	STA AUX_MOVE.DEST+$1
	
	
	SEC										 ;SET CARRY FLAG DESGINATD MOVE FROM MAIN MEMORY -> AUX
	JSR AUX_MOVE

	RTS
@END


LOAD.PATH.NEXT_MOVE
@START
;PARAMETERS: X-REG (index to SAVED.PATH.LOOKUP.TABLE), TRANSIT.NEXT_MOVE.INDEX
;RETURN: TRANSIT.NEXT_MOVE.X, TRANSIT.NEXT_MOVE.Y
;ENTRANCE: direct

;=====================SUBROUTINE DOCUMENTATION====================================
;
;Returns the X,Y coordintes of the next move
;in the path specified by the saved path number 
;specified by X-REG. 
;
;Since the saves paths have 1 page of memory reserved each,
;the exact aux address of the start of the path
;is calculated by using the path # (X-REG) as an offset to the base address of the
;saves paths in aux memory. The path index (1st byte of the path)
;is used to located the x,y coordiantes within
;the saved path with are for the next move. 
;
;=================================================================================



	;COPY NEXT MOVE X/Y-AXIS (aux memory -> main memory)	

	;SET AUX START & END ADDRESS
	LDA #NPC.PATHGENERATOR.SAVED_PATHS.AUX
	CLC
	ADC TRANSIT.NEXT_MOVE.INDEX
	STA AUX_MOVE.START
	CLC
	ADC #$01
	STA AUX_MOVE.END
	LDA SAVED.PATH.LOOKUP.TABLE+$3,X 			;Use path # in the saved path table for the HO byte offset
	ADC /NPC.PATHGENERATOR.SAVED_PATHS.AUX		;16-BIT ADD
	STA AUX_MOVE.END+$1
	STA AUX_MOVE.START+$1
	
	;SET MAIN MEMORY DESTINATION ADDRESS
	LDA #TRANSIT.NEXT_MOVE.X	;this copy will populate X and Y because the copy is for 2 bytes
	STA AUX_MOVE.DEST
	LDA /TRANSIT.NEXT_MOVE.X	 				
	STA AUX_MOVE.DEST+$1
	
	
	CLC											 ;SET CARRY FLAG DESGINATD MOVE FROM AUX -> MAIN MEMORY
	JSR AUX_MOVE
	
	RTS

@END

NPC_BUILDING.MANAGER.END

				


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
				



;MULTIPLICATION BY 6 
;used to calculate the offset to the base address of the 16-bit array NPC.PATHFINDER.SEARCH.PATHS
; NPC.PATHFINDER.MULTIPLY_TABLE.LO	.HS 00.06.0C.12.18.1E.24.2A.30.36.3C.42.48.4E.54.5A.60.66.6C.72.78.7E.84.8A.90.96.9C.A2.A8.AE.B4.BA.C0.C6.CC.D2.D8.DE.E4.EA.F0.F6.FC.02.08.0E.14.1A.20.26.2C.32.38.3E.44.4A.50.56.5C.62.68.6E.74.7A.80
; NPC.PATHFINDER.MULTIPLY_TABLE.HO	.HS 00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01

;MULTIPLICATION BY 8
; NPC.PATHFINDER.MULTIPLY_TABLE.LO	.HS	00.08.10.18.20.28.30.38.40.48.50.58.60.68.70.78.80.88.90.98.A0.A8.B0.B8.C0.C8.D0.D8.E0.E8.F0.F8.00.08.10.18.20.28.30.38.40.48.50.58.60.68.70.78.80.88.90.98.A0.A8.B0.B8.C0.C8.D0.D8.E0.E8.F0.F8.00.08.10.18.20.28.30.38.40.48.50.58.60.68.70.78.80.88.90.98.A0.A8.B0.B8.C0.C8.D0.D8.E0.E8.F0.F8.00.08.10.18.20.28.30.38.40.48.50.58.60.68.70.78.80.88.90.98.A0.A8.B0.B8.C0.C8.D0.D8.E0.E8.F0.F8.00.08.10.18.20.28.30.38.40.48.50.58.60.68.70.78.80.88.90.98.A0.A8.B0.B8.C0.C8.D0.D8.E0.E8.F0.F8.00.08.10.18.20.28.30.38.40.48.50.58.60.68.70.78.80.88.90.98.A0.A8.B0.B8.C0.C8.D0.D8.E0.E8.F0.F8.00.08.10.18.20.28.30.38.40.48.50.58.60.68.70.78.80.88.90.98.A0.A8.B0.B8.C0.C8.D0.D8.E0.E8.F0.F8.00.08.10.18.20.28.30.38.40.48.50.58.60.68.70.78.80.88.90.98.A0.A8.B0.B8.C0.C8.D0.D8.E0.E8.F0.F8
; NPC.PATHFINDER.MULTIPLY_TABLE.HO	.HS	00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07
@END

		;**OPT** Disk. Reduce the .NO to the actual amount of memory needed for the routines in this file. 
	.NO $A000 ;fill up to top of reserved memory block so that overflows are reported by SBASM
