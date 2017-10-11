; ============================================
; Copyright (C) 2016-2018. 6502 Workshop, LLC
; contact: mark@6502workshop.com
; ============================================



;************************INCLUDE FILE*****************************
;(do not assemble stand-alone.  )


LOAD.NEW.LOCATION	; ==========LOADS NEW SHAPE TABLES FROM DISK INTO AUX=====
@START
;PARAMETERS: PLAYER.MAP.LOCATION_CODE, PLAYER.MAP.LOCATION_TYPE, CURRENT.MAP_LOCATION.SPR_DATA
;RETURN: new location shape tables and map data in memory, updated SPR data file on disk for current location, SPR data for new location in memory
;ENTRANCE: DIRECT

;=====================SUBROUTINE DOCUMENTATION====================================
;
;*write sprite data to the sprite data file for the active map. 
;*load non-building routines into swap space so the zone tools are available for PARSE.RZONE.DATA, if needed.
;*load shape table for new map, based on the new map type
;*load new map and sprite data.
;*unpack zone map data (PARSE.RZONE.DATA) for surface and undermap. not needed for building since building maps are only one regional map in size.  
;*if new map is type building; load building routines into swap space
;=================================================================================



		
SAVE.SPR.DATA
@START

;ERASE ACTIVE TRANSPORT RECORD, UNLESS PLAYER IS WALKING
	;IS PLAYER WALKING?
	LDX PLAYER.TRANSPORT.ACTIVE		;load transport mo record index
	CPX #$FF						;is player walking?
	BEQ .SAVE.SPRITE.DATA			;if yes, then proceed to save sprite data
									;if no, then erase the active transport record

;COPY ACTIVE TRANSPORT RECORD TO NEW LOCATION TRANSPORT RECORD
	LDA MAP_OBJECTS.GENERAL+$0,X
	STA FORWARD.TRANSPORT.BUFFER+$0

	LDA MAP_OBJECTS.GENERAL+$1,X
	STA FORWARD.TRANSPORT.BUFFER+$1

	LDA MAP_OBJECTS.GENERAL+$2,X
	STA FORWARD.TRANSPORT.BUFFER+$2

	LDA MAP_OBJECTS.GENERAL+$3,X
	STA FORWARD.TRANSPORT.BUFFER+$3
	
;ERASE ACTIVE TRANSPORT RECORD
	LDA #$00
	STA MAP_OBJECTS.GENERAL+$0,X
	STA MAP_OBJECTS.GENERAL+$1,X
	STA MAP_OBJECTS.GENERAL+$2,X
	STA MAP_OBJECTS.GENERAL+$3,X
	
.SAVE.SPRITE.DATA


;WRITE FILE "DATA.SPR.xXX"
	
;READ SUBROUTINES FROM DISK TO MAIN MEMORY (LOWER 48K)	

;set command type (READ | WRITE | SEEK)
; cmd_seek.current_drive 	.EQ $90
; cmd_read.current_drive  	.EQ $91
; cmd_write.current_drive 	.EQ $92
;			
; cmd_read.drive1			.EQ $1
; cmd_read.drive2		  	.EQ $81
; cmd_write.drive1			.EQ $2
; cmd_write.drive2			.EQ $82

	lda #cmd_write.drive2
	sta parm.reqcmd

;set write data size (# of 512 byte blocks to write from memory)
	lda #$00 ;always #$00
	sta parm.sizelo

	;lda #$04 ;number of pages to write (must be in multiples of 2. i.e. 2, 4, 6, 8 $0A, etc)
	lda #$06 ;number of pages to write (must be in multiples of 2. i.e. 2, 4, 6, 8 $0A, etc)
	sta parm.sizehi
	
;set source memory address for write
	lda #DATA.SPR.WRITE.ADDRESS
	sta parm.ldrlo
	lda /DATA.SPR.WRITE.ADDRESS
	sta parm.ldrhi

	
;set filename to write to	
	lda CURRENT.MAP_LOCATION.SPR_DATA	;load LO address
	sta parm.namlo	
	lda CURRENT.MAP_LOCATION.SPR_DATA+$1 ;load HO address
	sta parm.namhi	
		
		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO	

			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; ; CPX #$04
			; ; BNE .TEMP
			; JSR KEYIN
			; LDX $C012
			; LDY $C011
			; JSR PREP.BRK
			; lda #$aa

			; BRK
; .TEMP
			; LDA TEMP
			
			; JSR PREP.BRK
			; LDA STATUS
			; LDA #$AB
			; LDX CURRENT.MAP_LOCATION.SPR_DATA
			; LDY CURRENT.MAP_LOCATION.SPR_DATA+$1
			; ;LDX #MAP_OBJECTS.MOB
			; ;LDY /MAP_OBJECTS.MOB
			; BRK
	
	
	;**FALLS THROUGH**
@END

LOAD.NEW.SHAPES ;and swap routines specific to map type
@START
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
	
	JSR LOAD.SRTN.NON_BUILDING ;load non-building routines so that zone tools will be available when PARSE.RZONE.DATA is run. 

	
	LDA PLAYER.MAP.LOCATION_TYPE
	CMP #MAP.TYPE.SURFACE
	BEQ .MAP_TYPE.SURFACE	
	CMP #MAP.TYPE.TOWN_VILLAGE 	;tile_set check, needs to check town/castle individually instead of just checking for building. 
	BEQ .MAP_TYPE.TOWN_VILLAGE 
	CMP #MAP.TYPE.CASTLE		;tile_set check, needs to check town/castle individually instead of just checking for building. 
	BEQ .MAP_TYPE.CASTLE_STEP
	CMP #MAP.TYPE.UNDERMAP
	BEQ .MAP_TYPE.UNDERMAP_STEP	

.ERROR
;UNEXPECTED LOCATION TYPE DETECTED IN LOAD.NEW.SHAPES
	JSR PREP.BRK
	BRK

.MAP_TYPE.CASTLE_STEP
	JMP .MAP_TYPE.CASTLE
	
.MAP_TYPE.UNDERMAP_STEP
	JMP .MAP_TYPE.UNDERMAP
	
.MAP_TYPE.SURFACE
@START
;LOAD FILE "DATA.SHP.SURF"

;set command type (READ | WRITE)
	;already set at top of LOAD.NEW.SHAPES
	
;set destination memory address
	lda #SHP.TBL.START.ADDRESS
	sta parm.ldrlo
	lda /SHP.TBL.START.ADDRESS
	sta parm.ldrhi

;set read length (bytes)
	lda #$FF				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda #$FF				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi
	
;set filename to read from	
	lda #DATA.SHP.SURF	;load LO address
	sta parm.namlo
	lda /DATA.SHP.SURF	;load HO address
	sta parm.namhi

		LDA #$01	;PARM: $00=main, $01=aux
	JSR PRODOS.IO	
	
	;JSR LOAD.SRTN.NON_BUILDING
	
	JMP LOAD.NEW.MAP

@END
	
.MAP_TYPE.TOWN_VILLAGE
@START
;LOAD FILE "DATA.SHP.BLD"

			; lda #$01
			; sta troubleshooting.hook
			
;set command type (READ | WRITE)
	;already set at top of LOAD.NEW.SHAPES

;set destination memory address
	lda #SHP.TBL.START.ADDRESS
	sta parm.ldrlo
	lda /SHP.TBL.START.ADDRESS
	sta parm.ldrhi
	;**OPT** set this part before the branch based on map type above

;set read length (bytes)
	lda #$FF				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda #$FF				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi
	;**OPT** set this part before the branch based on map type above
	
;set filename to read from	
	lda #DATA.SHP.BLD	;load LO address
	sta parm.namlo
	lda /DATA.SHP.BLD	;load HO address
	sta parm.namhi

		LDA #$01	;PARM: $00=main, $01=aux
	JSR PRODOS.IO	

	;don't call LOAD.SRTN.BUILDING, because that swaps out the zone tools will be needed to load the new map. 

;DEBUG: screen looks ok

	
.OTHER
		; ;LDA #$00 ;parameter: init shape buffers, since a shape table was just loaded
	; JSR TILE_SWAP.INIT.SUNRISE_SUNSET ;facilitates the swapping of daytile and nighttime tiles.


;DEBUG: screen not okay

	
.SHAPE.LOAD.DONE

	JMP LOAD.NEW.MAP
	
@END


.MAP_TYPE.CASTLE
@START
;LOAD FILE "DATA.SHP.CASTLE_COURTYARD"

			; lda #$01
			; sta troubleshooting.hook
			
;set command type (READ | WRITE)
	;already set at top of LOAD.NEW.SHAPES

;set destination memory address
	lda #SHP.TBL.START.ADDRESS
	sta parm.ldrlo
	lda /SHP.TBL.START.ADDRESS
	sta parm.ldrhi
	;**OPT** set this part before the branch based on map type above

;set read length (bytes)
	lda #$FF				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda #$FF				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi
	;**OPT** set this part before the branch based on map type above
	
;set filename to read from	
	lda #DATA.SHP.CASTLE_COURTYARD	;load LO address
	sta parm.namlo
	lda /DATA.SHP.CASTLE_COURTYARD	;load HO address
	sta parm.namhi

		LDA #$01	;PARM: $00=main, $01=aux
	JSR PRODOS.IO	

	;don't call LOAD.SRTN.BUILDING, because that swaps out the zone tools will be needed to load the new map. 


; .OTHER3
		; ;LDA #$00 ;parameter: init shape buffers, since a shape table was just loaded
	; JSR TILE_SWAP.INIT.SUNRISE_SUNSET ;facilitates the swapping of daytile and nighttime tiles.

.SHAPE.LOAD.DONE3

	JMP LOAD.NEW.MAP
	
@END

.MAP_TYPE.UNDERMAP
@START
;LOAD FILE "DATA.SHP.UM"

;set command type (READ | WRITE)
	;already set at top of LOAD.NEW.SHAPES

;set destination memory address
	lda #SHP.TBL.START.ADDRESS
	sta parm.ldrlo
	lda /SHP.TBL.START.ADDRESS
	sta parm.ldrhi
	;**OPT** set this part before the branch based on map type above

;set read length (bytes)
	lda #$FF				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda #$FF				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi
	;**OPT** set this part before the branch based on map type above
	
;set filename to read from	
	lda #DATA.SHP.UM	;load LO address
	sta parm.namlo
	lda /DATA.SHP.UM	;load HO address
	sta parm.namhi
				
		LDA #$01	;PARM: $00=main, $01=aux
	JSR PRODOS.IO	

	;JSR LOAD.SRTN.NON_BUILDING
	
.OTHER2
		; ;LDA #$00 ;parameter: init shape buffers, since a shape table was just loaded
	; JSR TILE_SWAP.INIT.SUNRISE_SUNSET ;facilitates the swapping of daytile and nighttime tiles.

.SHAPE.LOAD.DONE2

	JMP LOAD.NEW.MAP
	
@END
		
		;**FALLS THROUGH**
		

			
@END

LOAD.NEW.MAP ;& SPRITE DATA
@START


				
	LDA PLAYER.MAP.LOCATION
	CMP #$00
	BEQ LOCATION_CODE.00 ;surface
	CMP #$01
	BEQ .LOCATION_CODE.01_STEP ;Town: test, floor1
	CMP #$02			 
	BEQ .LOCATION_CODE.02_STEP ;Town: test, floor2
	CMP #$03			 
	BEQ .LOCATION_CODE.03_STEP ;Castle: test, floor1.1
	CMP #$04			 
	BEQ .LOCATION_CODE.04_STEP ;Castle: test, floor2.1
	CMP #$05			 
	BEQ .LOCATION_CODE.05_STEP ;Castle: test, floor3.1
	CMP #$40			
	BEQ .LOCATION_CODE.40_STEP ;Undermap, level1
	
.ERROR
;UNEXPECTED LOCATION TYPE DETECTED IN LOAD.NEW.SHAPES
	JSR PREP.BRK
	BRK

.LOCATION_CODE.01_STEP
	JMP LOCATION_CODE.01
	
.LOCATION_CODE.02_STEP
	JMP LOCATION_CODE.02

.LOCATION_CODE.03_STEP
	JMP LOCATION_CODE.03
	
.LOCATION_CODE.04_STEP
	JMP LOCATION_CODE.04	

.LOCATION_CODE.05_STEP
	JMP LOCATION_CODE.05
	
.LOCATION_CODE.40_STEP
	JMP LOCATION_CODE.40
	
LOCATION_CODE.00 ;surface
@START
;===LOAD MAP DATA===

;LOAD FILE "DATA.MAP.SURF"

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
	lda #WORLD.COMPRESS.AUX_MEMORY.START_LO
	sta parm.ldrlo
	lda #WORLD.COMPRESS.AUX_MEMORY.START_HO
	sta parm.ldrhi

;set read length (bytes)
	lda #$FF				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda #$FF				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi
	
;set filename to read from	
	lda #DATA.MAP.SURF	;load LO address
	sta parm.namlo
	lda /DATA.MAP.SURF	;load HO address
	sta parm.namhi

		LDA #$01	;PARM: $00=main, $01=aux
	JSR PRODOS.IO
	
			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK2
			; CMP #$01
			; BNE .TEMP2
			; LDA TEXT
			; LDA $C082				;READ ENABLE ROM, DISABLE WRITE ON BSM (NORMAL STATE).
									; ;REQUIRED FOR BEFORE BRK BECAUSE APPLE MONITOR IS A ROM ROUTINE
			; JSR CLEAR.TEXT.SCREEN
			; BRK
; .TEMP2
			; LDA TEMP
			
		LDA #DATA.MAP.SURFACE.TOTAL.SECTORS
		STA TOTAL.SECTORS
		LDA #$01
		STA USE.COMPRESSION_FLAGS	;turn compression flags on. WZONE.COMPRESSION.FLAGS will be checked for each zone to determine if compression is on or off. 
	JSR ZONE_TOOLS.BUILD.WZONE_HEADERS
	
	JSR REGION.UNCOMPRESS.ALL				;Load surface map, it is stored in auxiliary memory.


;===LOAD SPRITE DATA===

;LOAD FILE "DATA.SPR.SURF"

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
	lda #DATA.LOAD.ADDRESS.SURFACE	
	sta parm.ldrlo
	lda /DATA.LOAD.ADDRESS.SURFACE		
	sta parm.ldrhi

;set read length (bytes)
	lda #$FF				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda #$FF				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi
	
;set filename to read from	
	lda #DATA.SPR.SURF	;load LO address
	sta parm.namlo
	sta CURRENT.MAP_LOCATION.SPR_DATA ;save pointer to this filename so other routines will know which sprite data file applies to the current location
	lda /DATA.SPR.SURF	;load HO address
	sta parm.namhi
	sta CURRENT.MAP_LOCATION.SPR_DATA+$1 ;save pointer to this filename so other routines will know which sprite data file applies to the current location
	
		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO

;SET TALK FILE POINTER FOR THIS LOCATION
;(note: it doesn't need to be loaded for each map; just each location which has an entrance to it from outside the location.
; For example, a 2 floor town. Set this parameter when the map for floor 1 is loaded. If the player enters floor2, then this parameter will still be set)
	LDA #DATA.TLK.L001				;load LO address
	STA CURRENT.MAP_LOCATION.TLK_DATA

	LDA /DATA.TLK.L001				;load HO address
	STA CURRENT.MAP_LOCATION.TLK_DATA+$1
	
	JMP FORWARD.PLAYER.TRANSPORT
	
@END
	
LOCATION_CODE.01 ;Town: test, floor1
@START
;LOAD FILE "DATA.MAP.L1"
	
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
	lda #RZONE.ARRAY.TEMP_BUFFER	
	sta parm.ldrlo
	lda /RZONE.ARRAY.TEMP_BUFFER	
	sta parm.ldrhi

;set read length (bytes)
	lda #$FF				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda #$FF				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi
	
;set filename to read from	
	lda #DATA.MAP.M1	;load LO address
	sta parm.namlo
	lda /DATA.MAP.M1	;load HO address
	sta parm.namhi
	
		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO

;===LOAD SPRITE DATA===

;LOAD FILE "DATA.SPR.M1"
	
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
	lda #DATA.LOAD.ADDRESS.BUILDINGS	
	sta parm.ldrlo
	lda /DATA.LOAD.ADDRESS.BUILDINGS		
	sta parm.ldrhi

;set read length (bytes)
	lda #$FF				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda #$FF				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi
	
;set filename to read from	
	lda #DATA.SPR.M1	;load LO address
	sta parm.namlo
	sta CURRENT.MAP_LOCATION.SPR_DATA	;save pointer to this filename so other routines will know which sprite data file applies to the current location
	lda /DATA.SPR.M1	;load HO address
	sta parm.namhi 
	sta CURRENT.MAP_LOCATION.SPR_DATA+$1 ;save pointer to this filename so other routines will know which sprite data file applies to the current location
	
		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO


;SET TALK FILE POINTER FOR THIS LOCATION
;(note: it doesn't need to be loaded for each map; just each location which has an entrance to it from outside the location.
; For example, a 2 floor town. Set this parameter when the map for floor 1 is loaded. If the player enters floor2, then this parameter will still be set)
	LDA #DATA.TLK.L001				;load LO address
	STA CURRENT.MAP_LOCATION.TLK_DATA

	LDA /DATA.TLK.L001				;load HO address
	STA CURRENT.MAP_LOCATION.TLK_DATA+$1
	
	


			
	JMP PARSE.RZONE.DATA
@END


LOCATION_CODE.02 ;Town: test, floor2
@START
;LOAD FILE "DATA.MAP.M2"

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
	lda #RZONE.ARRAY.TEMP_BUFFER	
	sta parm.ldrlo
	lda /RZONE.ARRAY.TEMP_BUFFER	
	sta parm.ldrhi

;set read length (bytes)
	lda #$FF				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda #$FF				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi
	
;set filename to read from	
	lda #DATA.MAP.M2	;load LO address
	sta parm.namlo
	lda /DATA.MAP.M2	;load HO address
	sta parm.namhi
	
		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO


			
;===LOAD SPRITE DATA===

;LOAD FILE "DATA.SPR.M2"
	
;READ SUBROUTINES FROM DISK TO MAIN MEMORY (LOWER 48K)	

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
	lda #DATA.LOAD.ADDRESS.BUILDINGS	
	sta parm.ldrlo
	lda /DATA.LOAD.ADDRESS.BUILDINGS		
	sta parm.ldrhi

;set read length (bytes)
	lda #$FF				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda #$FF				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi
	
;set filename to read from	
	lda #DATA.SPR.M2	;load LO address
	sta parm.namlo
	sta CURRENT.MAP_LOCATION.SPR_DATA	;save pointer to this filename so other routines will know which sprite data file applies to the current location
	lda /DATA.SPR.M2	;load HO address
	sta parm.namhi 
	sta CURRENT.MAP_LOCATION.SPR_DATA+$1 ;save pointer to this filename so other routines will know which sprite data file applies to the current location
	
		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO

			
	JMP PARSE.RZONE.DATA
@END

LOCATION_CODE.03 ;Castle: test, floor1.1
@START
;LOAD FILE "DATA.MAP.M3"

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
	lda #RZONE.ARRAY.TEMP_BUFFER	
	sta parm.ldrlo
	lda /RZONE.ARRAY.TEMP_BUFFER	
	sta parm.ldrhi
	;**OPT** Memory. Set this part in a common code section
	;Or maybe set via zero page variable, and all routines have common section for file load
	
;set read length (bytes)
	lda #$FF				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda #$FF				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi
	;**OPT** Memory. Set this part in a common code section
	;Or maybe set via zero page variable, and all routines have common section for file load

;set filename to read from	
	lda #DATA.MAP.M3	;load LO address
	sta parm.namlo
	lda /DATA.MAP.M3	;load HO address
	sta parm.namhi
	
		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO


			
;===LOAD SPRITE DATA===

;LOAD FILE "DATA.SPR.M2"
	
;READ SUBROUTINES FROM DISK TO MAIN MEMORY (LOWER 48K)	

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
	lda #DATA.LOAD.ADDRESS.BUILDINGS	
	sta parm.ldrlo
	lda /DATA.LOAD.ADDRESS.BUILDINGS		
	sta parm.ldrhi
	;**OPT** Memory. Set this part in a common code section
	;Or maybe set via zero page variable, and all routines have common section for file load

;set read length (bytes)
	lda #$FF				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda #$FF				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi
	;**OPT** Memory. Set this part in a common code section
	;Or maybe set via zero page variable, and all routines have common section for file load
	
;set filename to read from	
	lda #DATA.SPR.M3	;load LO address
	sta parm.namlo
	sta CURRENT.MAP_LOCATION.SPR_DATA	;save pointer to this filename so other routines will know which sprite data file applies to the current location
	lda /DATA.SPR.M3	;load HO address
	sta parm.namhi 
	sta CURRENT.MAP_LOCATION.SPR_DATA+$1 ;save pointer to this filename so other routines will know which sprite data file applies to the current location
	
		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO

			
	JMP PARSE.RZONE.DATA
@END

LOCATION_CODE.04 ;Castle: test, floor2.1
@START
;LOAD FILE "DATA.MAP.M4"

;set command type (READ | WRITE | SEEK)
; cmd_seek.current_drive 	.EQ $90
; cmd_read.current_drive  	.EQ $91
; cmd_write.current_drive 	.EQ $92
;			
; cmd_read.drive1			.EQ $1
; cmd_read.drive2		  	.EQ $81
; cmd_write.drive1			.EQ $2
; cmd_write.drive2			.EQ $82




			; lda #$02
			; sta TROUBLESHOOTING.HOOK2
			
			
	lda #cmd_read.drive2
	sta parm.reqcmd
	
;set destination memory address
	lda #RZONE.ARRAY.TEMP_BUFFER	
	sta parm.ldrlo
	lda /RZONE.ARRAY.TEMP_BUFFER	
	sta parm.ldrhi
	;**OPT** Memory. Set this part in a common code section
	;Or maybe set via zero page variable, and all routines have common section for file load
	
;set read length (bytes)
	lda #$FF				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda #$FF				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi
	;**OPT** Memory. Set this part in a common code section
	;Or maybe set via zero page variable, and all routines have common section for file load

;set filename to read from	
	lda #DATA.MAP.M4	;load LO address
	sta parm.namlo
	lda /DATA.MAP.M4	;load HO address
	sta parm.namhi
	
		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO


			
;===LOAD SPRITE DATA===

;LOAD FILE "DATA.SPR.M4"
	
;READ SUBROUTINES FROM DISK TO MAIN MEMORY (LOWER 48K)	

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
	lda #DATA.LOAD.ADDRESS.BUILDINGS	
	sta parm.ldrlo
	lda /DATA.LOAD.ADDRESS.BUILDINGS		
	sta parm.ldrhi
	;**OPT** Memory. Set this part in a common code section
	;Or maybe set via zero page variable, and all routines have common section for file load

;set read length (bytes)
	lda #$FF				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda #$FF				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi
	;**OPT** Memory. Set this part in a common code section
	;Or maybe set via zero page variable, and all routines have common section for file load
	
;set filename to read from	
	lda #DATA.SPR.M4	;load LO address
	sta parm.namlo
	sta CURRENT.MAP_LOCATION.SPR_DATA	;save pointer to this filename so other routines will know which sprite data file applies to the current location
	lda /DATA.SPR.M4	;load HO address
	sta parm.namhi 
	sta CURRENT.MAP_LOCATION.SPR_DATA+$1 ;save pointer to this filename so other routines will know which sprite data file applies to the current location
	
		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO

			
	JMP PARSE.RZONE.DATA
@END

LOCATION_CODE.05 ;Castle: test, floor3.1
@START
;LOAD FILE "DATA.MAP.M5"

;set command type (READ | WRITE | SEEK)
; cmd_seek.current_drive 	.EQ $90
; cmd_read.current_drive  	.EQ $91
; cmd_write.current_drive 	.EQ $92
;			
; cmd_read.drive1			.EQ $1
; cmd_read.drive2		  	.EQ $81
; cmd_write.drive1			.EQ $2
; cmd_write.drive2			.EQ $82




			; lda #$02
			; sta TROUBLESHOOTING.HOOK2
			
			
	lda #cmd_read.drive2
	sta parm.reqcmd
	
;set destination memory address
	lda #RZONE.ARRAY.TEMP_BUFFER	
	sta parm.ldrlo
	lda /RZONE.ARRAY.TEMP_BUFFER	
	sta parm.ldrhi
	;**OPT** Memory. Set this part in a common code section
	;Or maybe set via zero page variable, and all routines have common section for file load
	
;set read length (bytes)
	lda #$FF				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda #$FF				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi
	;**OPT** Memory. Set this part in a common code section
	;Or maybe set via zero page variable, and all routines have common section for file load

;set filename to read from	
	lda #DATA.MAP.M5	;load LO address
	sta parm.namlo
	lda /DATA.MAP.M5	;load HO address
	sta parm.namhi
	
		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO


			
;===LOAD SPRITE DATA===

;LOAD FILE "DATA.SPR.M5"
	
;READ SUBROUTINES FROM DISK TO MAIN MEMORY (LOWER 48K)	

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
	lda #DATA.LOAD.ADDRESS.BUILDINGS	
	sta parm.ldrlo
	lda /DATA.LOAD.ADDRESS.BUILDINGS		
	sta parm.ldrhi
	;**OPT** Memory. Set this part in a common code section
	;Or maybe set via zero page variable, and all routines have common section for file load

;set read length (bytes)
	lda #$FF				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda #$FF				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi
	;**OPT** Memory. Set this part in a common code section
	;Or maybe set via zero page variable, and all routines have common section for file load
	
;set filename to read from	
	lda #DATA.SPR.M5	;load LO address
	sta parm.namlo
	sta CURRENT.MAP_LOCATION.SPR_DATA	;save pointer to this filename so other routines will know which sprite data file applies to the current location
	lda /DATA.SPR.M5	;load HO address
	sta parm.namhi 
	sta CURRENT.MAP_LOCATION.SPR_DATA+$1 ;save pointer to this filename so other routines will know which sprite data file applies to the current location
	
		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO

			
	JMP PARSE.RZONE.DATA
@END

LOCATION_CODE.40 ;Undermap, level1
@START

				
				
;===LOAD MAP DATA===

;LOAD FILE "DATA.UMAP.L1"

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
	lda #WORLD.COMPRESS.AUX_MEMORY.START_LO
	sta parm.ldrlo
	lda #WORLD.COMPRESS.AUX_MEMORY.START_HO
	sta parm.ldrhi

;set read length (bytes)
	lda #$FF				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda #$FF				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi
	
;set filename to read from	
	lda #DATA.MAP.ULV1	;load LO address
	sta parm.namlo
	lda /DATA.MAP.ULV1	;load HO address
	sta parm.namhi
		
		LDA #$01	;PARM: $00=main, $01=aux
	JSR PRODOS.IO		

	

				
				
				
				
			LDA #DATA.MAP.ULV1.TOTAL.SECTORS
			STA TOTAL.SECTORS	
			LDA #$01 ;turn ON zone comprssion flags.
			STA USE.COMPRESSION_FLAGS	 ($00=OFF | $01 = ON). If OFF, then all zones are compressed. If on, then WZONE.COMPRESSION.FLAGS determines whether a zone is compressed or not.
		JSR ZONE_TOOLS.BUILD.WZONE_HEADERS




				
	JSR REGION.UNCOMPRESS.ALL				;Load surface map, it is stored in auxiliary memory.



;(SPECIAL TEST) TEST.ENTRANCE

			
;===LOAD SPRITE DATA===

;LOAD FILE "DATA.SPR.ULV1"

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
	lda #DATA.LOAD.ADDRESS.UNDERMAP	
	sta parm.ldrlo
	lda /DATA.LOAD.ADDRESS.UNDERMAP		
	sta parm.ldrhi

;set read length (bytes)
	lda #$FF				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda #$FF				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi
	
;set filename to read from	
	lda #DATA.SPR.ULV1	;load LO address
	;LDA #DATA.SPR.SURF
	sta parm.namlo
	sta CURRENT.MAP_LOCATION.SPR_DATA ;save pointer to this filename so other routines will know which sprite data file applies to the current location	
	lda /DATA.SPR.ULV1	;load HO address
	;LDA /DATA.SPR.SURF	
	sta parm.namhi
	sta CURRENT.MAP_LOCATION.SPR_DATA+$1 ;save pointer to this filename so other routines will know which sprite data file applies to the current location


			
		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO

	

				
				
				
			;(SPECIAL TEST)  LDA #$AD
			; LDX #$EE
			; JSR PREP.BRK
			; BRK
			

;SET TALK FILE POINTER FOR THIS LOCATION
;(note: it doesn't need to be loaded for each map; just each location which has an entrance to it from outside the location.
; For example, a 2 floor town. Set this parameter when the map for floor 1 is loaded. If the player enters floor2, then this parameter will still be set)
	LDA #DATA.TLK.L001				;load LO address
	STA CURRENT.MAP_LOCATION.TLK_DATA

	LDA /DATA.TLK.L001				;load HO address
	STA CURRENT.MAP_LOCATION.TLK_DATA+$1

	
	JMP FORWARD.PLAYER.TRANSPORT
@END


;LOCATION_CODE.02	
;LOCATION_CODE.03
;ETC


	
PARSE.RZONE.DATA ;USED FOR UNCOMPRESSED MAPS (BUILDINGS), NOT FOR COMPRESSED MAPS (SURFACE, UNDERMAP LEVELS)
@START


			
;INIT VARIABLES
	LDX #$00	;init load loop counter
	
	LDA #RZONE.ARRAY.TEMP_BUFFER
	STA SWAP_SPACE.MAIN_MEMORY.POINTER

	LDA /RZONE.ARRAY.TEMP_BUFFER
	STA SWAP_SPACE.MAIN_MEMORY.POINTER+$1

	LDA #$00					;SET STARTING REGIONAL ZONE
	STA RZONE.UNCOMPRESS.CURRENT
			
.LOOP.LOAD
	LDY #$00	;init copy loop index
			
.LOOP.COPY
	LDA (SWAP_SPACE.MAIN_MEMORY.POINTER),Y	;read byte from swap space, which contains the map data read from disk
	STA NEW.MAP,Y							;save byte into the input buffer used by ZONE_TOOLS.RCOPY
	INY	;increment copy index
	BNE .LOOP.COPY


			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; lda #$bb
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP
			
	JSR ZONE_TOOLS.RCOPY					;copy data from buffer (NEW.MAP) into regional zone array, into the array elements for the current zone

;INCREMENT RZONE INDEX & COUNTER
	INC SWAP_SPACE.MAIN_MEMORY.POINTER+$1	;increment ho byte of swap space index
	INC RZONE.UNCOMPRESS.CURRENT	;increment zone counter

			; CPX #$00
			; BNE .TEMP
			; LDA RZONE.UNCOMPRESS.CURRENT
			; ; LDX SWAP_SPACE.MAIN_MEMORY.POINTER
			; ; LDY SWAP_SPACE.MAIN_MEMORY.POINTER+$1
			; ;LDX #NEW.MAP
			; ;LDY /NEW.MAP
			; JSR PREP.BRK
			; BRK
; .TEMP	
		
	INX 							;INCREMENT TOTAL SECTORS READ	
	CPX #RZONE.TOTAL
	BEQ .EXIT
	JMP .LOOP.LOAD

.EXIT



				
	;**FALLS THROUGH**
@END
	
FORWARD.PLAYER.TRANSPORT
@START

			
;VERIFY PLAYER HAS TRANSPORT ACTIVE
	LDX PLAYER.TRANSPORT.ACTIVE					;LOAD TRANSPORT MO RECORD INDEX
	;STX SAVED.XREG.LOCAL						;SAVE INDEX FOR FUTURE USE 
	CPX #$FF									;IS PLAYER WALKING?
	BEQ .EXIT
		
;CREATE TRANSPORT RECORD AT NEW LOCATION
	JSR MO.TRANSPORT.CREATE
	STX PLAYER.TRANSPORT.ACTIVE ;set player's active transport index to the new record created

;COPY PRIOR LOCATION TRANSPORT RECORD TO NEW LOCATION TRANSPORT RECORD
	; LDA FORWARD.TRANSPORT.BUFFER+$0
	; STA MAP_OBJECTS.GENERAL+$0,X

	; LDA FORWARD.TRANSPORT.BUFFER+$1
	; STA MAP_OBJECTS.GENERAL+$1,X	
	
	LDA GMAP.X
	STA MAP_OBJECTS.GENERAL+$0,X

	LDA GMAP.Y
	STA MAP_OBJECTS.GENERAL+$1,X

	LDA FORWARD.TRANSPORT.BUFFER+$2
	STA MAP_OBJECTS.GENERAL+$2,X	

	LDA FORWARD.TRANSPORT.BUFFER+$3
	STA MAP_OBJECTS.GENERAL+$3,X	

	;**FALLS THROUGH**

.EXIT


				
				
	;**FALLS THROUGH**
@END
	
@END
	
LOAD.MAP_TYPE_SPECIFIC.MODULES ;serves as exit to top level routine LOAD.NEW.LOCATION
@START


				
;note: this routine exists so that the building specific routines aren't loaded until
;the new map has been loaded. This is because the zone tools needed to process new map data aren't included
;in the building specific routines file.  

	LDA PLAYER.MAP.LOCATION_TYPE
	; CMP #MAP.TYPE.TOWN_VILLAGE
	; BEQ .IN_BUILDING
	; CMP #MAP.TYPE.CASTLE
	; BEQ .IN_BUILDING
	; JMP .EXIT
	CMP #MAP.TYPE.BUILDING.GRE 	;is map type = building?
	BCC .EXIT						;if no
	CMP #MAP.TYPE.BUILDING.LT	;is map type = building?
	BCS .EXIT						;if no
	JMP .IN_BUILDING			;if yes	

.IN_BUILDING
	JSR LOAD.SRTN.BUILDING	
	
.EXIT ;(exit for LOAD.NEW.LOCATION)
	RTS ;return to routine that called LOAD.NEW.LOCATION
@END


;LOAD.NEW.LOCATION SUBROUTINES

LOAD.SRTN.NON_BUILDING
@START
;PARAMETERS: NONE

;LOAD FILE "SRTN.NON.BLD"

;set command type (READ | WRITE)
	;already set at top of LOAD.NEW.SHAPES

;set destination memory address
	lda #SRTN.NON.BLD.ADDRESS
	sta parm.ldrlo
	lda /SRTN.NON.BLD.ADDRESS
	sta parm.ldrhi

;set read length (bytes)
	lda #$FF				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda #$FF				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi
	;**OPT** This parm needs to be set after each call to ProRWTS. Maybe I can store it in the wrapper, and use the ACC parm (hi bit on/off) to convery to the wrapper whether I want to use the last value or set it.  

;set filename to read from	
	lda #SRTN.NON.BLD	;load LO address
	sta parm.namlo
	lda /SRTN.NON.BLD	;load HO address
	sta parm.namhi

		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO
	
	RTS ;return to the JSR in LOAD.NEW.MAP that called this routine
@END
	
LOAD.SRTN.BUILDING
@START


				
				
;PARAMETERS: none

;LOAD FILE "SRTN.BLD"

;set command type (READ | WRITE)
	;already set at top of LOAD.NEW.SHAPES

;set destination memory address
	lda #SRTN.BLD.ADDRESS
	sta parm.ldrlo
	lda /SRTN.BLD.ADDRESS
	sta parm.ldrhi

;set read length (bytes)
	lda #$FF				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda #$FF				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi
	
;set filename to read from	
;(!!!WARNING: player disk must be in one of the drives as it contains this file)
	lda #SRTN.BLD	;load LO address
	sta parm.namlo
	lda /SRTN.BLD	;load HO address
	sta parm.namhi

		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO

	RTS ;return to the JSR in LOAD.MAP_TYPE_SPECIFIC.MODULES
@END
@END

;****for the general purpose subroutines that used to be at
;the end of this file, see GENERAL_SUBROUTINES.ASM***