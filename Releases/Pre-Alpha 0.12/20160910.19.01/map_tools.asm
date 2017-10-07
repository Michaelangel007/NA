;************************INCLUDE FILE*****************************
;(do not assemble stand-alone. For a stand-alone version of this routine see examples/rwts2)


LOAD.NEW.LOCATION	; ==========LOADS NEW SHAPE TABLES FROM DISK INTO AUX=====
@START
;PARAMETERS: PLAYER.MAP.LOCATION_CODE, PLAYER.MAP.LOCATION_TYPE, CURRENT.LOCATION.SPR_DATA
;RETURN: new location shape tables and map data in memory, updated SPR data file on disk for current location, SPR data for new location in memory
;ENTRANCE: DIRECT

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

	lda #$04 ;number of pages to write (must be in multiples of 2. i.e. 2, 4, 6, 8 $0A, etc)
	sta parm.sizehi
	
;set source memory address
	lda #MAP_OBJECTS.MOB
	sta parm.ldrlo
	lda /MAP_OBJECTS.MOB
	sta parm.ldrhi

	
;set filename to read from	
	lda CURRENT.LOCATION.SPR_DATA	;load LO address
	sta parm.namlo	
	lda CURRENT.LOCATION.SPR_DATA+$1 ;load HO address
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
			; LDX CURRENT.LOCATION.SPR_DATA
			; LDY CURRENT.LOCATION.SPR_DATA+$1
			; ;LDX #MAP_OBJECTS.MOB
			; ;LDY /MAP_OBJECTS.MOB
			; BRK
	
	
	;**FALLS THROUGH**
@END

LOAD.NEW.SHAPES
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
	
	LDA PLAYER.MAP.LOCATION_TYPE
	CMP #LOCATION.TYPE.SURFACE
	BEQ .LOCATION_TYPE.SURFACE	
	CMP #LOCATION.TYPE.BUILDING
	BEQ .LOCATION_TYPE.BUILDING	
	CMP #LOCATION.TYPE.UNDERMAP
	BEQ .LOCATION_TYPE.BUILDING ;**TEMPORARILY DUNGEONS USE THE BUILDING TILE SET. 
	
	
.ERROR
;UNEXPECTED LOCATION TYPE DETECTED IN LOAD.NEW.SHAPES
	LDA TEXT
	LDA $C082
	BRK

	
.LOCATION_TYPE.SURFACE
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
		
	JMP .CALL.LOADER
@END
	
.LOCATION_TYPE.BUILDING
@START
;LOAD FILE "DATA.SHP.BLD"

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
	lda #DATA.SHP.BLD	;load LO address
	sta parm.namlo
	lda /DATA.SHP.BLD	;load HO address
	sta parm.namhi

	JMP .CALL.LOADER
@END


.LOCATION_TYPE.DUNGEON


;LOCATION_TYPE.02
;LOCATION_TYPE.03
;ETC	
	
.CALL.LOADER
		LDA #$01	;PARM: $00=main, $01=aux
	JSR PRODOS.IO

			
			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; LDA TEXT			
			; LDA $C082				;READ ENABLE ROM, DISABLE WRITE ON BSM (NORMAL STATE).
									; ;REQUIRED FOR BEFORE BRK BECAUSE APPLE MONITOR IS A ROM ROUTINE
			; JSR CLEAR.TEXT.SCREEN
			; BRK
; .TEMP
			; LDA TEMP
			
			; LDA #$01		;skipping $01 header sector because the data to be read from disk was written to disk by Apple Commander which adds a header. 
		; JSR DISK.READ.AUX
		
		;**FALLS THROUGH**
		

			
@END

LOAD.NEW.MAP ;& SPRITE DATA
@START

	LDA PLAYER.MAP.LOCATION
	CMP #$00
	BEQ LOCATION_CODE.00 ;DATA.MAP.SURF
	CMP #$01
	BEQ LOCATION_CODE.01 ;DATA.MAP.M1        (L1: FLOOR1)
	CMP #$02			 
	BEQ .LOCATION_CODE.02_STEP ;DATA.MAP.M2  (L1: FLOOR2)
	CMP #$40			 ;DATA.MAP.ULV1
	BEQ .LOCATION_CODE.40_STEP
	
.ERROR
;UNEXPECTED LOCATION TYPE DETECTED IN LOAD.NEW.SHAPES
	LDA TEXT
	LDA $C082
	BRK

.LOCATION_CODE.02_STEP
	JMP LOCATION_CODE.02

.LOCATION_CODE.40_STEP
	JMP LOCATION_CODE.40	
	
LOCATION_CODE.00 ;DATA.MAP.SURF
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
	lda #MAP_OBJECTS.MOB	
	sta parm.ldrlo
	lda /MAP_OBJECTS.MOB		
	sta parm.ldrhi

;set read length (bytes)
	lda #$FF				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda #$FF				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi
	
;set filename to read from	
	lda #DATA.SPR.SURF	;load LO address
	sta parm.namlo
	sta CURRENT.LOCATION.SPR_DATA ;save pointer to this filename so other routines will know which sprite data file applies to the current location
	lda /DATA.SPR.SURF	;load HO address
	sta parm.namhi
	sta CURRENT.LOCATION.SPR_DATA+$1 ;save pointer to this filename so other routines will know which sprite data file applies to the current location
	
		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO
	
	JMP FORWARD.PLAYER.TRANSPORT
	
@END
	
LOCATION_CODE.01 ;DATA.MAP.M1
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
	lda #MAP_OBJECTS.MOB	
	sta parm.ldrlo
	lda /MAP_OBJECTS.MOB		
	sta parm.ldrhi

;set read length (bytes)
	lda #$FF				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda #$FF				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi
	
;set filename to read from	
	lda #DATA.SPR.M1	;load LO address
	sta parm.namlo
	sta CURRENT.LOCATION.SPR_DATA	;save pointer to this filename so other routines will know which sprite data file applies to the current location
	lda /DATA.SPR.M1	;load HO address
	sta parm.namhi 
	sta CURRENT.LOCATION.SPR_DATA+$1 ;save pointer to this filename so other routines will know which sprite data file applies to the current location
	
		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO


;SET TALK FILE POINTER FOR THIS LOCATION
;(note: it doesn't need to be loaded for each map; just each location which has an entrance to it from outside the location.
; For example, a 2 floor town. Set this parameter when the map for floor 1 is loaded. If the player enters floor2, then this parameter will still be set)
	LDA #DATA.TLK.L001				;load LO address
	STA CURRENT.LOCATION.TLK_DATA

	LDA /DATA.TLK.L001				;load HO address
	STA CURRENT.LOCATION.TLK_DATA+$1
	
	JMP PARSE.RZONE.DATA
@END

		; LDA #$01		;skipping $01 sector because the data to be read from disk was written to disk by Apple Commander which adds a header. 
		; JSR DISK.READ.AUX
LOCATION_CODE.02 ;DATA.MAP.M2  (Loc 01, Floor2)
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
	lda #MAP_OBJECTS.MOB	
	sta parm.ldrlo
	lda /MAP_OBJECTS.MOB		
	sta parm.ldrhi

;set read length (bytes)
	lda #$FF				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda #$FF				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi
	
;set filename to read from	
	lda #DATA.SPR.M2	;load LO address
	sta parm.namlo
	sta CURRENT.LOCATION.SPR_DATA	;save pointer to this filename so other routines will know which sprite data file applies to the current location
	lda /DATA.SPR.M2	;load HO address
	sta parm.namhi 
	sta CURRENT.LOCATION.SPR_DATA+$1 ;save pointer to this filename so other routines will know which sprite data file applies to the current location
	
		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO
	
	JMP PARSE.RZONE.DATA
@END

LOCATION_CODE.40 ;DATA.MAP.ULV1
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
		LDA #$00
		STA USE.COMPRESSION_FLAGS	;turn OFF zone comprssion flags. All zones will be compressed. 

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
	lda #MAP_OBJECTS.MOB	
	sta parm.ldrlo
	lda /MAP_OBJECTS.MOB		
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
	sta CURRENT.LOCATION.SPR_DATA ;save pointer to this filename so other routines will know which sprite data file applies to the current location	
	lda /DATA.SPR.ULV1	;load HO address
	;LDA /DATA.SPR.SURF	
	sta parm.namhi
	sta CURRENT.LOCATION.SPR_DATA+$1 ;save pointer to this filename so other routines will know which sprite data file applies to the current location


			
		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO

			;(SPECIAL TEST)  LDA #$AD
			; LDX #$EE
			; JSR PREP.BRK
			; BRK
			
			
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

	
	
.EXIT
	RTS
@END
	
@END
@END	
@END

;===============MEMORY TOOLS==========

SWAP.MAIN_MEMORY.OUT
@START

	;MAIN MEMORY -> AUX MEMORY	
	LDA #SWAP_SPACE.MAIN_MEMORY			;SET START ADDRESS
	STA AUX_MOVE.START
	LDA /SWAP_SPACE.MAIN_MEMORY
	STA AUX_MOVE.START+$1
	
	LDA #SWAP_SPACE.MAIN_MEMORY.END		;SET END ADDRESS
	STA AUX_MOVE.END
	LDA /SWAP_SPACE.MAIN_MEMORY.END
	STA AUX_MOVE.END+$1
	
	LDA #SWAP_SPACE.AUX_MEMORY			;SET DESTINATION ADDRESS
	STA AUX_MOVE.DEST
	LDA /SWAP_SPACE.AUX_MEMORY
	STA AUX_MOVE.DEST+$1
	SEC                ;SET CARRY FLAG DESGINATD MOVE FROM MAIN MEMORY -> AUX
	JSR AUX_MOVE
		

	RTS

@END

SWAP.MAIN_MEMORY.IN
@START

	;AUX MEMORY -> MAIN MEMORY 	
	LDA #SWAP_SPACE.AUX_MEMORY		;SET START ADDRESS
	STA AUX_MOVE.START
	LDA /SWAP_SPACE.AUX_MEMORY		
	STA AUX_MOVE.START+$1
	
	LDA #SWAP_SPACE.AUX_MEMORY.END		;SET END ADDRESS
	STA AUX_MOVE.END
	LDA /SWAP_SPACE.AUX_MEMORY.END
	STA AUX_MOVE.END+$1
	
	LDA #SWAP_SPACE.MAIN_MEMORY			;SET DESTINATION ADDRESS
	STA AUX_MOVE.DEST
	LDA /SWAP_SPACE.MAIN_MEMORY
	STA AUX_MOVE.DEST+$1
	CLC					;SET CARRY FLAG DESGINATD MOVE FROM AUX MEMORY -> MAIN
	JSR AUX_MOVE
	
	RTS
	
@END

;===============COMMAND TOOLS=========
PLAYER.INPUT.COMMAND_DIRECTION
@START

	JSR KEYIN.ANIMATION.SINGLE
	STA SAVED.ACC.GLOBAL1	;save direction key for future use
	CMP #$8B			;UP ARROW
	BEQ .CHECK_NORTH
	CMP #$8A			;DOWN ARROW
	BEQ .CHECK_SOUTH
	CMP #$95			;RIGHT ARROW
	BEQ .CHECK_EAST
	CMP #$88			;LEFT ARROW
	BEQ	.CHECK_WEST 
;INVALID DIRECTION SELECTED
		;**FALLS THROUGH**

.INVALID.COMMAND
;command not valid because direction selected is not valid or because a pushable general object does not exist 1 tile from player location in the direction selected
;or because player tried to use the push command with transport active



;LOAD PLAYER ADJACENT SCREEN LOCATION # (as map object screen array index)
.CHECK_NORTH	
	LDY #SCREEN.ARRAY.ADJACENT_NORTH
	LDA #SCREEN.ARRAY.ADJACENT_NORTH2
	JMP .EXIT

.CHECK_SOUTH	
	LDY #SCREEN.ARRAY.ADJACENT_SOUTH
	LDA #SCREEN.ARRAY.ADJACENT_SOUTH2
	JMP .EXIT

.CHECK_EAST
	LDY #SCREEN.ARRAY.ADJACENT_EAST
	LDA #SCREEN.ARRAY.ADJACENT_EAST2
	JMP .EXIT
	
.CHECK_WEST
	LDY #SCREEN.ARRAY.ADJACENT_WEST
	LDA #SCREEN.ARRAY.ADJACENT_WEST2
	;**FALLS THROUGH**

.EXIT
	
	RTS
@END

;===============MISC TOOLS========

MAP.CALCULATE.SS_FLAGS ;CALCUALTE SS MAP REGIONAL FLAGS BASED ON RMAP.X/Y 
@START
;PARAMETERS: RMAP.X, RMAP.Y
;RETURN: MAP_OBJECTS.SS.X_FLAG.UPPER, MAP_OBJECTS.SS.Y_FLAG.UPPER, MAP_OBJECTS.SS.X_FLAG.LOWER, MAP_OBJECTS.SS.Y_FLAG.LOWER
;ENTRANCE: DIRECT

;Note: the principle in play is that flags to detect when a mob x,y value (relative to the player) is off the 
;regional map needs to float depending on the player position withing the region. 
	
	LDA RMAP.X												;PLAYER'S REGIONAL MAP X-AXIS POSITION
	CMP #RZONE.LOAD.X.START									;X-AXIS VALUE WHEN PLAYER IS CENTERED IN REGI0NAL MAP
	BCC .XLESS
	
	LDA RMAP.X		
	SEC
	SBC #RZONE.LOAD.X.START
	STA MAP_OBJECTS.X_ADJ
	
	LDA #MAP_OBJECTS.SS.X_FLAG.LOWER.START					;LEFT EDGE OF REGION WHEN PLAYER IS CENTERED IN REGIONAL MAP
	SEC
	SBC MAP_OBJECTS.X_ADJ
	STA MAP_OBJECTS.SS.X_FLAG.LOWER

	LDA #MAP_OBJECTS.SS.X_FLAG.UPPER.START					;RIGHT EDGE OF REGION WHEN PLAYER IS CENTERED IN REGIONAL MAP
	SEC
	SBC MAP_OBJECTS.X_ADJ
	STA MAP_OBJECTS.SS.X_FLAG.UPPER
	
	
	JMP .YTEST
	
.XLESS	
	LDA #RZONE.LOAD.X.START									;X-AXIS VALUE WHEN PLAYER IS CENTERED IN REGI0NAL MAP
	SEC
	SBC RMAP.X	
	STA MAP_OBJECTS.X_ADJ
	
	LDA #MAP_OBJECTS.SS.X_FLAG.LOWER.START					;LEFT EDGE OF REGION WHEN PLAYER IS CENTERED IN REGIONAL MAP
	CLC
	ADC MAP_OBJECTS.X_ADJ
	STA MAP_OBJECTS.SS.X_FLAG.LOWER
	
	LDA #MAP_OBJECTS.SS.X_FLAG.UPPER.START					;RIGHT EDGE OF REGION WHEN PLAYER IS CENTERED IN REGIONAL MAP
	CLC
	ADC MAP_OBJECTS.X_ADJ
	STA MAP_OBJECTS.SS.X_FLAG.UPPER						
	;**FALLS THROUGH**
	
.YTEST
	LDA RMAP.Y												;PLAYER'S REGIONAL MAP X-AXIS POSITION
	CMP #RZONE.LOAD.Y.START									;X-AXIS VALUE WHEN PLAYER IS CENTERED IN REGI0NAL MAP
	BCC .YLESS
	
	LDA RMAP.Y		
	SEC
	SBC #RZONE.LOAD.Y.START
	STA MAP_OBJECTS.Y_ADJ
	
	LDA #MAP_OBJECTS.SS.Y_FLAG.LOWER.START					;BOTTOM EDGE OF REGION WHEN PLAYER IS CENTERED IN REGIONAL MAP
	SEC
	SBC MAP_OBJECTS.Y_ADJ
	STA MAP_OBJECTS.SS.Y_FLAG.LOWER

	LDA #MAP_OBJECTS.SS.Y_FLAG.UPPER.START					;TOP EDGE OF REGION WHEN PLAYER IS CENTERED IN REGIONAL MAP
	SEC
	SBC MAP_OBJECTS.Y_ADJ
	STA MAP_OBJECTS.SS.Y_FLAG.UPPER
	
	
	JMP .DONE1
	
.YLESS	
	LDA #RZONE.LOAD.Y.START									;Y-AXIS VALUE WHEN PLAYER IS CENTERED IN REGI0NAL MAP
	SEC
	SBC RMAP.Y	
	STA MAP_OBJECTS.Y_ADJ
	
	LDA #MAP_OBJECTS.SS.Y_FLAG.LOWER.START					;BOTTOM EDGE OF REGION WHEN PLAYER IS CENTERED IN REGIONAL MAP
	CLC
	ADC MAP_OBJECTS.Y_ADJ
	STA MAP_OBJECTS.SS.Y_FLAG.LOWER
	
	LDA #MAP_OBJECTS.SS.Y_FLAG.UPPER.START					;TOP EDGE OF REGION WHEN PLAYER IS CENTERED IN REGIONAL MAP
	CLC
	ADC MAP_OBJECTS.Y_ADJ
	STA MAP_OBJECTS.SS.Y_FLAG.UPPER	
.DONE1
	RTS
@END


;CALCULATE DISTANCE BETWEEN X,Y COORDINATES
;see .CALCULATE.DISTANCE (NPC.PATHFINDER)
;(designed for RMAP/GMAP but may work for player relative X,Y too)

CALCULATE.DISTANCE ;Calculate distance between two GMAP.X/Y or GMAP.X/Y positions
@START
;PARAMETERS: PARM1.GMAP.X, PARM1.GMAP.Y, PARM2.GMAP.X, PARM2.GMAP.Y
;RETURN: ACC (DISTANCE IN TILE MOVES, IGNORING OBSTACLES)
;ENTRANCE: DIRECT

			
;CALCULATE DISTANCE BETWEEN PARM1 AND PARM2
	LDA PARM1.GMAP.X
	CMP PARM2.GMAP.X
	BCC .MOB.MO_X_LESS
	
	SEC
	SBC PARM2.GMAP.X
	STA MAP_OBJECTS.X_ADJ

	JMP .MOB.YTEST

.MOB.MO_X_LESS
	LDA PARM2.GMAP.X
	SEC
	SBC PARM1.GMAP.X
	STA MAP_OBJECTS.X_ADJ
	;**FALLS THROUGH**
	
.MOB.YTEST	
	LDA PARM1.GMAP.Y
	CMP PARM2.GMAP.Y
	BCC .MOB.MO_Y_LESS
	
	SEC
	SBC PARM2.GMAP.Y
	STA MAP_OBJECTS.Y_ADJ
		
	JMP .FINAL.DISTANCE.CALC

.MOB.MO_Y_LESS
	LDA PARM2.GMAP.Y
	SEC
	SBC PARM1.GMAP.Y	
	STA MAP_OBJECTS.Y_ADJ
	;**FALLS THROUGH**
	
.FINAL.DISTANCE.CALC
	LDA MAP_OBJECTS.X_ADJ					;finalize distance calculation
	CLC
	ADC MAP_OBJECTS.Y_ADJ
	;STA RETURN.DISTANCE	
	;BEQ .FINAL.DISTANCE.COMPLETE			;if distance is $00 then the destination has been found, no need to add weight to the distance value. 
	;**FALLS THROUGH**
	
.FINAL.DISTANCE.COMPLETE
	;RETURN ACC: final distance
	RTS
	
@END


;===============MAP CONVERSION TOOLS========


;***WARNING: this routine might only work in towns, where the wzones are in the upper left corner of map. Double check if used for any other purpose!!!!
CONVERT.GMAP_XY.RMAP_XY ;CONVERTS A PLAYER START GMAP.XY VALUE INTO AN RMAP.XY 
@START
;PARAMETERS: PARM.GMAP.X, PARM.GMAP.Y
;RETURN: RETURN.RMAP.X, RETURN.RMAP.Y
;ENTRANCE: DIRECT

;FORUMLA: RMAP.XY = $10 + REMAINDER(GMAP.XY/#WZONE.WIDTH)
;Note: $10 is the starting x/y value in the upper left corner of the center RZONE.
;The player will start in the center RZONE once arriving at the destination location so we can make this assumption. 


;***WARNING: this routine might only work in towns, where the wzones are in the upper left corner of map. Double check if used for any other purpose!!!!

	
	LDA PARM.GMAP.X
	STA DIVIDEND

	LDA #$00
	STA DIVIDEND+$1
	LDA	#WZONE.WIDTH
	STA DIVISOR
	LDA #$00
	STA DIVISOR+$1
	STA DIVIDEND32
	STA DIVIDEND32+$1
		
	JSR DIV.16						;**OPT** Speed. Replace with in-line code
	
	LDA RESULT+$2					;REMAINDER
	STA TEMPX							
	
	LDA TEMPX
	CLC
	ADC #WZONE.WIDTH
	STA RETURN.RMAP.X
	
	
	LDA PARM.GMAP.Y
	STA DIVIDEND
	LDA #$00
	STA DIVIDEND+$1
	LDA	#WZONE.WIDTH
	STA DIVISOR
	LDA #$00
	STA DIVISOR+$1
	
	JSR DIV.16						;**OPT** Speed. Replace with in-line code
	
	LDA RESULT+$2					;REMAINDER 
	STA TEMPY						;**OPT** Speed. Memory. TEMPY isn't really needed just use RESULT for next calculation	
	
	LDA TEMPY	
	CLC
	ADC #WZONE.WIDTH
	STA RETURN.RMAP.Y

	RTS
@END


;.CONVERT.RMAP_XY.GMAP_XY ;CONVERTS A RMAP.XY VALUE TO GMAP.XY VALUE (uses player's GMAP as a reference point)
@START
; ;PARAMETERS: PARM.RMAP.X, PARM.RMAP.Y
; ;RETURN: RETURN.GMAP.X, RETURN.GMAP.Y
; ;ENTRANCE: DIRECT

; ;FORMULA: 	RETURN.GMAP.X = PARM.RMAP.X - RMAP.X + GMAP.X
			; ;RETURN.GMAP.Y = PARM.RMAP.Y - RMAP.Y + GMAP.Y

; ;ARCHIVE: A forumla that doesn't require player's GMAP, but much less efficient. I didn't do the X-axix calc but should be doable. 
; ;FORMULA:	GMAP.Y = (QUOTIENT[WZONE.ROW.NUMBER / RZONE.OFFSET] * RMAP.SIZE + (REMAINDER * WZONE.WIDTH)) + PARM.RMAP.Y
							; ;WZONE.ROW.NUMBER = QUOTIENT [WZONE.ROW / WZONE.OFFSET]
							; ;WZONE.ROW = QUOTIENT [PARM.WZONE / WZONE.OFFSET], ignore remainder

							
; ; ;TEMPATE	
	; ; LDA MOB.CANDIDATE.MAP.X
	; ; STA PARM.RMAP.X
	
	; ; LDA MOB.CANDIDATE.MAP.Y
	; ; STA PARM.RMAP.Y
	
; .START	
	; LDA RMAP.X				;player RMAP X-axis
	; CMP PARM.RMAP.X			;RMAP-to-convert X-Axis
	; BCC .MOB.MO_X_LESS
	
	; SEC
	; SBC PARM.RMAP.X			;RMAP-to-convert X-Axis	
	; STA MAP_OBJECTS.X_ADJ	;==distance between player and to-convert x-axis

	; LDA GMAP.X				;load player GMAP x-axis
	; SBC	MAP_OBJECTS.X_ADJ	;subtract distance
	; STA RETURN.GMAP.X		;result is the GMAP.X of the to-convert RMAP.X

	; JMP .MOB.YTEST

; .MOB.MO_X_LESS ;(Player RMAP.X less than TO-CONVERT RMAP.X)
	; LDA PARM.RMAP.X			;RMAP-to-convert X-Axis
	
	; SEC
	; SBC RMAP.X				;player RMAP X-axis
	; STA MAP_OBJECTS.X_ADJ	;==distance between player and to-convert x-axis
	
	; LDA GMAP.X				;Add distance 
	; CLC										
	; ADC MAP_OBJECTS.X_ADJ								
	; STA RETURN.GMAP.X		;result is the GMAP.X of the to-convert RMAP.X
	
	; ;**FALLS THROUGH**
	
; .MOB.YTEST	
	; LDA RMAP.Y				;player RMAP Y-axis
	; CMP PARM.RMAP.Y			;RMAP-to-convert Y-Axis
	; BCC .MOB.MO_Y_LESS
	
	; SEC
	; SBC PARM.RMAP.Y			;to-convert RMAP Y-Axis
	; STA MAP_OBJECTS.Y_ADJ	;==distance between player and to covnert y-axis

	; LDA GMAP.Y				;load player GMAP y-axis
	; SBC	MAP_OBJECTS.Y_ADJ	;subtract distance
	; STA RETURN.GMAP.Y		;result is the GMAP.Y of the to-convert RMAP.Y
	
	; JMP .CONVERSION.COMPLETE

; .MOB.MO_Y_LESS ;(Player RMAP.Y less than TO-CONVERT RMAP.Y )
	; LDA PARM.RMAP.Y			;RMAP-to-convert Y-axis	
	; SEC
	; SBC RMAP.Y				;player RMAP Y-axis
	; STA MAP_OBJECTS.Y_ADJ	;==distance between player and to-convert y-axis
	
	; LDA GMAP.Y				;load player GMAP y-axis
	; CLC										
	; ADC MAP_OBJECTS.Y_ADJ	;add distance							
	; STA RETURN.GMAP.Y		;result is the GMAP.Y of the to-convert RMAP.Y
	; ;**FALLS THROUGH**	


; .CONVERSION.COMPLETE
@END
	

CONVERT.RMAP_XY.RMAP
@START
;PARAMETERS: PARM.RMAP.X, PARM.RMAP.Y
;RETURN: RETURN.RMAP(2)
;ENTRANCE: DIRECT

	LDY PARM.RMAP.Y
	LDA RMAP.MULTIPLY_TABLE.HO,Y
	STA RETURN.RMAP+$1
	LDA RMAP.MULTIPLY_TABLE.LO,Y
	STA RETURN.RMAP

;16-BIT ADD
	CLC
	ADC PARM.RMAP.X
	STA RETURN.RMAP					
	LDA RETURN.RMAP+$1
	ADC #$00
	STA RETURN.RMAP+$1
	
	RTS
	
@END

CONVERT.GMAP_XY.WZONE
@START
;PARAMETERS: PARM.GMAP.X, PARM.GMAP.Y
;RETURN: RETURN.WZONE
;ENTRANCE: DIRECT

;FORMULA: WZONE = (GMAP.Y/#WZONE.WIDTH * #WZONE OFFSET)+(GMAP.X/#WZONE.WIDTH)
;	Remainder form division is discarded, only use quotient. 


	LDA PARM.GMAP.Y
	STA DIVIDEND
	LDA #$00
	STA DIVIDEND+$1
	LDA	#WZONE.WIDTH
	STA DIVISOR
	LDA #$00
	STA DIVISOR+$1
	
	JSR DIV.16						;**OPT** Speed. Replace with in-line code
	
	LDA RESULT						;QUOTIENT
	ASL	;X2
	ASL ;X4
	ASL ;X8
	STA TEMPY
	
	LDA PARM.GMAP.X
	STA DIVIDEND

	LDA #$00
	STA DIVIDEND+$1
	LDA	#WZONE.WIDTH
	STA DIVISOR
	LDA #$00
	STA DIVISOR+$1
	STA DIVIDEND32
	STA DIVIDEND32+$1
		
	JSR DIV.16						;**OPT** Speed. Replace with in-line code
	
	LDA RESULT						;QUOTIENT
	CLC
	ADC TEMPY
	STA RETURN.WZONE
	
	RTS
	

@END


; also see .CONVERT.GMAP.TO.PLAYER_RELATIVE.XY (MAP_OBJECTS.MANAGEMENT), WHICH ARE SETUP AS IN-LINE CODE FOR SPEED
CONVERT.GMAP.TO.PLAYER_RELATIVE.XY
@START
;PARAMETERS: PARM.GMAP.X, PARM.GMAP.Y
;RETURN: RETURN.RELATIVE.X, RETURN.RELATIVE.Y
;ENTRANCE: DIRECT

;Formula: PLAYER GMAP.X/Y - MO GMAP.X/Y + $80/$80 (ground zero, player location in relative grid) = MO Player Relative X/Y

.DO.CONVERSION
	LDA GMAP.X					;player GMAP X-axis
	CMP PARM.GMAP.X				;object GMAP X-Axis
	BCC .MOB.MO_X_LESS
	
	SEC
	SBC PARM.GMAP.X				;object GMAP X-Axis	
	STA MAP_OBJECTS.X_ADJ		;==distance between player and object x-axis

	LDA #MAP_OBJECTS.PLAYER_LOCATION	;Subtract distance from Player Location on MO grid ($80)
	SBC	MAP_OBJECTS.X_ADJ
	STA RETURN.RELATIVE.X			;==player relative X
		
	JMP .MOB.YTEST

.MOB.MO_X_LESS ;(Player X less than object X )
	LDA PARM.GMAP.X					;object GMAP X-Axis
	
	SEC
	SBC GMAP.X						;player GMAP X-axis, before current move
	STA MAP_OBJECTS.X_ADJ			;==distance between player and object x-axis
	
	LDA #MAP_OBJECTS.PLAYER_LOCATION	;Add distance to Player Location on MO grid ($80)
	CLC										
	ADC MAP_OBJECTS.X_ADJ								
	STA RETURN.RELATIVE.X			;==player relative X
	
	;**FALLS THROUGH**
	
.MOB.YTEST	
	LDA GMAP.Y						;player GMAP Y-axis, before current move
	CMP PARM.GMAP.Y					;object GMAP Y-Axis
	BCC .MOB.MO_Y_LESS
	
	SEC
	SBC PARM.GMAP.Y					;object GMAP Y-Axis
	STA MAP_OBJECTS.Y_ADJ			;==distance between player and object y-axis

	LDA #MAP_OBJECTS.PLAYER_LOCATION	;Subtract distance from Player Location on MO grid ($80)
	SBC	MAP_OBJECTS.Y_ADJ
	STA RETURN.RELATIVE.Y			;==player relative Y
	
	JMP .CONVERSION.COMPLETE

.MOB.MO_Y_LESS ;(Player Y less than object Y )
	LDA PARM.GMAP.Y					;object GMAP Y-Axis	
	SEC
	SBC GMAP.Y						;player GMAP Y-axis, before current move
	STA MAP_OBJECTS.Y_ADJ			;==distance between player and object y-axis
	
	LDA #MAP_OBJECTS.PLAYER_LOCATION	;Add distance to Player Location on MO grid ($80)
	CLC										
	ADC MAP_OBJECTS.Y_ADJ								
	STA RETURN.RELATIVE.Y				;==player relative Y
	;**FALLS THROUGH**	
	
.CONVERSION.COMPLETE	
	RTS
	
@END

;also see .MOB.IDENTIFY.TILE_LOCATION (MAP_OBJECTS.MANAGEMENT), the in-line code version used for speed
CONVERT.PLAYER_RELATIVE.XY.TO.SCREEN_ARRAY_INDEX 
@START
;PARAMETERS: PARM.PARM.RELATIVE.X, PARM.RELATIVE.Y
;RETURN: RETURN.SCREEN_ARRAY_INDEX(1)
;ENTRANCE: DIRECT
			
;=====================CODE-SECTION DOCUMENTATION====================================
;
;This code section (virtually identical to it's transport counterpart above,
;is responsible for converting a map objects relative x,y position into a 
;screen tile location (the index to the screen arrays).
;The screen tile location is useful in may routines such as
;DRAW.TILE.SINGLE, which needs the screen array index as a parameter.
;
;The mechanic of doing the conversion is essentially done by using the 
;difference between the player's x/y and the map objects x/y as an adjustment to 
;the players screen tile position, using the appropriate offsets for the
;screen tile grid. For example, for a y axis differential, a multiplication table is used
;to calculate the offset to apply to the player's location. 
;
;Calculating the difference between player and map object's x/y starts 
;determining whether the map object x,y is less than the player x,y.
;This is done so that negative numbers can be avoided. 
;
;===USING THIS ROUTINE IN PLACE OF .MOB.IDENTIFY.TILE_LOCATION (MAP_OBJECTS.MANAGEMENT.ASM) INLINE CODE===
;The code in this routine that is commend out would need to be uncommented. This code
;prepares some values used later in map objects management but that aren't needed for just
;a simple relative x/y to screen array index conversion, so I commented the code out to save memory. 
;
;=================================================================================
				
;INIT VARIABLES

	; LDA #$00
	; STA MOB.POSITION.X_GR
	; STA MOB.POSITION.X_LT
	; STA MOB.POSITION.Y_GR	
	; STA MOB.POSITION.Y_LT

;IDENTIFY SCREEN TILE #							;OBJECT IS ON SCREEN, WHICH SCREEN TILE SHOULD WE DRAW IT IN?
	LDA PARM.RELATIVE.X
	CMP #MAP_OBJECTS.PLAYER_LOCATION
	BCC .MOB.MO_X_LESS
	
	LDA PARM.RELATIVE.X ;**OPT** Speed. Memory. This line can probably be deleted as that value is already in ACC
	SEC
	SBC #MAP_OBJECTS.PLAYER_LOCATION
	STA MAP_OBJECTS.X_ADJ
	LDA #SCREEN.ARRAY.PLAYER_LOCATION
	CLC
	ADC MAP_OBJECTS.X_ADJ
	STA TEMP

; ;RECORD X-AXIS COMPARISON FOR MOB MOVEMENT 	
	; LDA PARM.PARM.RELATIVE.X
	; CMP #MAP_OBJECTS.PLAYER_LOCATION
	; BEQ .X_AXIS.SAME	
	; STA MOB.POSITION.X_GR								;RECORDS THAT MOB'S X-AXIS IS GREATER THAN PLAYER'S. IT DOESN'T MATTER WHAT VALUE IS STORED, AS LONG AS IT IS != $00 

.X_AXIS.SAME	


	JMP .MOB.YTEST

.MOB.MO_X_LESS
	LDA #MAP_OBJECTS.PLAYER_LOCATION
	SEC
	SBC PARM.RELATIVE.X
	STA MAP_OBJECTS.X_ADJ
	LDA #SCREEN.ARRAY.PLAYER_LOCATION
	SEC
	SBC MAP_OBJECTS.X_ADJ
	STA TEMP

; ;RECORD X-AXIS COMPARISON FOR MOB MOVEMENT 	
	
	; STA MOB.POSITION.X_LT								;RECORDS THAT MOB'S X-AXIS IS LESS THAN PLAYER'S. IT DOESN'T MATTER WHAT VALUE IS STORED, AS LONG AS IT IS != $00 


.MOB.YTEST	

	LDA PARM.RELATIVE.Y
	CMP #MAP_OBJECTS.PLAYER_LOCATION
	BCC .MOB.MO_Y_LESS
	
	LDA PARM.RELATIVE.Y				;**OPT** Speed. Memory. This line can probably be deleted as that value is already in ACC
	SEC
	SBC #MAP_OBJECTS.PLAYER_LOCATION
	TAY
	LDA SCREEN.MULTIPLY_TABLE,Y
	STA MAP_OBJECTS.Y_ADJ

;Calculate screen array index	
	LDA TEMP ;the X-axis adjustment	;PARTIAL CALCULATION. CONTAINS #MAP_OBJECTS.PLAYER_LOCATION +/- MAP_OBJECTS.X_ADJ 								;PARTIAL CALCULATION. CONTAINS #MAP_OBJECTS.PLAYER_LOCATION +/- MAP_OBJECTS.X_ADJ 
	CLC
	ADC MAP_OBJECTS.Y_ADJ
	STA RETURN.SCREEN_ARRAY_INDEX
		

; ;RECORD X-AXIS COMPARISON FOR MOB MOVEMENT 	
	; LDA PARM.PARM.RELATIVE.Y
	; CMP #MAP_OBJECTS.PLAYER_LOCATION
	; BEQ .Y_AXIS.SAME
	
	; STA MOB.POSITION.Y_GR								;RECORDS THAT MOB'S Y-AXIS IS GREATER THAN PLAYER'S. IT DOESN'T MATTER WHAT VALUE IS STORED, AS LONG AS IT IS != $00 
.Y_AXIS.SAME	

	JMP .EXIT

.MOB.MO_Y_LESS
	LDA #MAP_OBJECTS.PLAYER_LOCATION
	SEC
	SBC PARM.RELATIVE.Y		
	TAY
	LDA SCREEN.MULTIPLY_TABLE,Y
	STA MAP_OBJECTS.Y_ADJ
	
;Calculate screen array index	
	LDA TEMP ;the X-axis adjustment	;PARTIAL CALCULATION. CONTAINS #MAP_OBJECTS.PLAYER_LOCATION +/- MAP_OBJECTS.X_ADJ 								;PARTIAL CALCULATION. CONTAINS #MAP_OBJECTS.PLAYER_LOCATION +/- MAP_OBJECTS.X_ADJ 
	SEC
	SBC MAP_OBJECTS.Y_ADJ
	STA RETURN.SCREEN_ARRAY_INDEX										

; ;RECORD Y-AXIS COMPARISON FOR MOB MOVEMENT 	
	
	; STA MOB.POSITION.Y_LT					;RECORDS THAT MOB'S Y-AXIS IS LESS THAN PLAYER'S. IT DOESN'T MATTER WHAT VALUE IS STORED, AS LONG AS IT IS != $00 


.EXIT
; .FINAL_ROUTINE

; ;IF MULTI-TILE MOB, RECORD THE SCREEN TILE # FOR EACH OF IT'S FOUR TILES

	; LDA MOB.FLAG3							
	; CMP #$01								;IS CURENT MO RECORD FOR A MULTI-TILE MOB?
	; BNE MOB.MOVEMENT
	; LDY MAP_OBJECTS.TILE_LOCATION			;LOAD TILE LOCATION CALCUALTED IN MAIN ROUTINE ABOVE
	; STY MOB.MT.TILE_LOCATIONS				;SAVE AS TILE #0 (UPPER LEFT) OF MT MOB

	; INY
	; STY MOB.MT.TILE_LOCATIONS+$1			;SAVE AS TILE #1 (UPPER RIGHT) OF MT MOB
	; TYA
	; CLC
	; ADC #SCREEN.ARRAY.OFFSET
	; STA MOB.MT.TILE_LOCATIONS+$3			;SAVE AS TILE #3 (LOWER RIGHT) OF MT MOB
	; TAY
	; DEY
	; STY MOB.MT.TILE_LOCATIONS+$2			;SAVE AS TILE #2 (LOWER LEFT) OF MT MOB

	RTS
	
@END
	
;CONVERT COLUMN,ROW TO SCREEN ARRAY INDEX 
;(I think this is already done via a lookup table, no subroutine needed)

INIT.SCREEN.ARRAYS
@START
;PARAMETERS: NONE
;ENTRANCE: DIRECT
;RETURN: SCREEN ARRAYS SET TO DEFAULT VALUES

;=====================SUBROUTINE DOCUMENTATION====================================
;
;The screen arrays are stencils to the SCREEN.TILE.DATA array, used to track map objects
;and the visible/hidden (dark) status of ech tile. 
;The data in them is set by DARKESS.REVIEW and MO.DRAW. They must be initialized at game launch and
;also whenever the map in memory changes because the init values are used as default and are only changed by the
;previously mentioned routines as needed. 
;
;
;=================================================================================


;SAVE REGISTERS
	TAY
	PHA
	
	LDY #$00
.LOOP
	LDA #$00
	STA SCREEN.DARK.DATA, Y
	STA SCREEN.DARK.DATA_BEFORE, Y	
	
	LDA #$FF
	STA SCREEN.MO_GENERAL.DATA,Y
	STA SCREEN.MO_SPRITE.DATA,Y
	STA SCREEN.MO_SPRITE_TYPE.DATA,Y
	
	CPY #$BA
	BEQ .DONE
	INY
	JMP .LOOP
.DONE

;RESTORE REGISTERS
	PLA
	TAY
	
	RTS
@END
