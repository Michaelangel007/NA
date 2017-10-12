; ============================================
; Copyright (C) 2016-2018. 6502 Workshop, LLC
; contact: mark@6502workshop.com
; ============================================


;************************INCLUDE FILE*****************************
;(do not assemble stand-alone.  )



;==============TEMP============




;===============MEMORY TOOLS==========

SWAP.MAIN_MEMORY.OUT
@START
;note: swaps out the main memory swap area into aux swap space #1


	;MAIN MEMORY -> AUX MEMORY	
	LDA #SWAP_SPACE.MAIN_MEMORY.ALT.ADDR			;SET START ADDRESS
	STA AUX_MOVE.START
	LDA /SWAP_SPACE.MAIN_MEMORY.ALT.ADDR
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
;note: swaps aux swap space #1 contents into the main memory swap space

	;AUX MEMORY -> MAIN MEMORY 	
	LDA #SWAP_SPACE.AUX_MEMORY		;SET START ADDRESS
	STA AUX_MOVE.START
	LDA /SWAP_SPACE.AUX_MEMORY		
	STA AUX_MOVE.START+$1
	
	LDA #SWAP_SPACE.AUX_MEMORY.END		;SET END ADDRESS
	STA AUX_MOVE.END
	LDA /SWAP_SPACE.AUX_MEMORY.END
	STA AUX_MOVE.END+$1
	
	LDA #SWAP_SPACE.MAIN_MEMORY.ALT.ADDR			;SET DESTINATION ADDRESS
	STA AUX_MOVE.DEST
	LDA /SWAP_SPACE.MAIN_MEMORY.ALT.ADDR
	STA AUX_MOVE.DEST+$1
	CLC					;SET CARRY FLAG DESGINATD MOVE FROM AUX MEMORY -> MAIN
	JSR AUX_MOVE
	
	RTS
	
@END


SWAP2.MAIN_MEMORY.IN
@START
;note: swaps aux swap space #2 contents into the main memory swap space

;**OPT** Disk Speed. Reduce size of SWAP_SPACE2.AUX_MEMORY to the actual amount
;needed.*
		;*the himem or lomem of AUX_BSR swap space2 can be decrease/increased. 
		;Currently the entire combat module is copied but really only the portion clobbered by the spell file needs to be copied (this is the portion below COMBAT.PERSISTANT.MEMORY.START. This currently is about 8 pages of memory).
		;see routines


		
		
; ;---DEBUG BREAK---
	; LDA TEXT
	
; ;DISABLE.BS_RAM
	; LDA $C082				;READ ENABLE ROM, DISABLE WRITE ON BSM (NORMAL STATE).
							; ;REQUIRED FOR BEFORE BRK BECAUSE APPLE MONITOR IS A ROM ROUTINE

	; JSR CLEAR.TEXT.SCREEN   ;this JSR is to a ROM address
	
; ;JSR HCR.OFF
; ;RETURN OUTPUT HOOK TO NORMAL SETTINGS	
	; LDA #$F0		;PRODUCES LOW BYTE
	; STA CSW
	; LDA #$FD		;PRODUCES HIGH BYTES
	; STA CSW+$1
	
	; LDA #$EE
	; LDX #$EE
	; LDY #$EE
	; BRK
	


	
		TSX			;transfer stack pointer to X-REG
		STX TEMP	;save stack pointer
	STA $C009 ;enable aux zero-page & aux BSR 

	;execute copy
		LDA #SWAP_SPACE2.AUX_MEMORY
		STA COPY.FROM_START
		LDA /SWAP_SPACE2.AUX_MEMORY
		STA COPY.FROM_START+$1

		LDA #SWAP_SPACE2.AUX_MEMORY.MOB.START-1
		STA COPY.FROM_END
		LDA /SWAP_SPACE2.AUX_MEMORY.MOB.START-1
		STA COPY.FROM_END+$1
			
		LDA #SRTN.COMBAT.ADDRESS
		STA COPY.TO
		LDA /SRTN.COMBAT.ADDRESS
		STA COPY.TO+$1
	
	JSR MEMORY.COPY	

	
	STA $C008 ;enable main zero-page & main BSR 
		LDX TEMP	;restore stack pointer to X-REG
		TXS ;transfer X-REG to stack pointer



			; lda #$ee
			; ldx #$ee
			; ldy #$ee
			; jsr prep.brk
			; brk

			
	; ;AUX MEMORY -> MAIN MEMORY 	
	; LDA #SWAP_SPACE2.AUX_MEMORY		;SET START ADDRESS
	; STA AUX_MOVE.START
	; LDA /SWAP_SPACE2.AUX_MEMORY		
	; STA AUX_MOVE.START+$1
	
	; LDA #SWAP_SPACE2.AUX_MEMORY.END		;SET END ADDRESS
	; STA AUX_MOVE.END
	; LDA /SWAP_SPACE2.AUX_MEMORY.END
	; STA AUX_MOVE.END+$1
	
	; LDA #SWAP_SPACE.MAIN_MEMORY			;SET DESTINATION ADDRESS
	; STA AUX_MOVE.DEST
	; LDA /SWAP_SPACE.MAIN_MEMORY
	; STA AUX_MOVE.DEST+$1
	; CLC					;SET CARRY FLAG DESGINATD MOVE FROM AUX MEMORY -> MAIN
	; JSR AUX_MOVE
	
	RTS
	
@END


;===============COMMAND TOOLS=========
PLAYER.INPUT.COMMAND_DIRECTION
@START
;PARAMETERS: NONE
;ENTRANCE: DIRECT
;RETURN: Y-REG = screen tile location of adjacent screen tile in direction selected, ACC = (screen tile location +2 screen tiles in direction selected | ACC = $FF if directon key wasn't pressed (invalid command)), 
;		 SAVED.ACC.GLOBAL1 = ascii code of keypress
		
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
	LDA #$FF ;set return value
	JMP .EXIT


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

;===============ENTRANCE/WRAPPER ROUTINES======
@START

INITIATE.COMBAT
@START
;PARAMETERS: COMBAT_SE.MODE.PARM, COMBAT_SE.MOB_TILE_ID.PARM 
;ENTRANCE: direct

			;**OPT** Memory. Speed. I think this can be removed if I remove the key press abort skipping of copy.screen at the end of the common move routine, which I will probably have to do to get animation for just mobs working even in a key press abort scenario. 
	;!!!!! BSR BANK1 !!!!
	JSR COPY.SCREEN.ENTRANCE ;**DON'T REMOVE** (or the screen goes chaotic sometimes when collisions occur with double mover mobs. MO.DRAW must need the graphics pages in sync for some reason). UDPATE: I think this is needed because sometimes, due to key press abort, no copy.screen was done after the last move, so the pages are out of syncing going into MOVE.PASS, which is triggered by non-movement commands like (3)ZAP, Push, Board, etc.



;*****FUTURE EXPANSION NOTE: ;**OPT**	
;when I'm ready to have an (A)ttack command
;integrate it with the talk command as they both involved a directional arrow key and
;loading a module. In the current talk logic, after direction key press, check if command key was attack
;and if so set a different filename, but memory swap out is the same. Do another command key check before the JSR to NPC.TALK to instead do a JSR to NPC.COMBAT. More details to work out but that is the general idea. 



.LOAD.SWAP.ROUTINE
;LOAD COMBAT FILE INTO THE MAIN MEMORY SWAP SPACE



			
	JSR SWAP.MAIN_MEMORY.OUT ;swap out anything currently occupying the swap space

;LOAD COMBAT SETUP
@START
;PARAMETERS: parm.namlo, parm.namhi

;LOAD FILE "SRTN.COMBAT.S"
			
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
	lda #SRTN.COMBAT.ADDRESS	
	sta parm.ldrlo
	lda /SRTN.COMBAT.ADDRESS		
	sta parm.ldrhi

;set read length (bytes)
	lda #COMBAT_SE.SETUP.MAIN.READ_BYTES				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda /COMBAT_SE.SETUP.MAIN.READ_BYTES				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi
	
;set filename to read from 
	lda #SRTN.COMBAT.SETUP	;load LO address
	sta parm.namlo
	lda /SRTN.COMBAT.SETUP	;load HO address
	sta parm.namhi 
	
		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO

@END


;CALL COMBAT MODULE		
		LDA SCREEN.TILE.DATA+$5D ;screen center (player location)
		STA COMBAT_SE.TERRAIN_TILE_ID.PARM ;The Tile_ID of the terrain the player was standing on when combat was initiated. 
	;COMBAT_SE.MOB_TILE_ID.PARM (already set by calling routine) 		;The Tile_ID of the MOB on the main map which attacked the player or the player attacked
	JSR COMBAT.ENTRANCE	
			
;LOAD COMBAT MAIN
@START
;LOAD FILE "SRTN.COMBAT"
			
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
	lda #SRTN.COMBAT.ADDRESS	
	sta parm.ldrlo
	lda /SRTN.COMBAT.ADDRESS		
	sta parm.ldrhi

;set read length (bytes)
	lda #$FF				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda #$FF				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi
	
;set filename to read from	
	lda #SRTN.COMBAT	;load LO address
	sta parm.namlo
	lda /SRTN.COMBAT	;load HO address
	sta parm.namhi 
	
		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO	
@END
	

			
			
;CALL MAIN COMBAT ROUTINE
		;-XREG contains map object record # of the Active NPC
		;-YREG contains Active NPC screen tile location when NPC.TALK is called
	
	;JSR COMBAT.ENTRANCE	
	JSR COMBAT.MAIN ;**

	
;LOAD COMBAT EXIT
@START
;PARAMETERS: parm.namlo, parm.namhi

;LOAD FILE "SRTN.COMBAT.S"
			
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
	lda #SWAP_SPACE.MAIN_MEMORY	
	sta parm.ldrlo
	lda /SWAP_SPACE.MAIN_MEMORY		
	sta parm.ldrhi

;set read length (bytes)
	lda #$FF				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda #$FF				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi
	
;set filename to read from 
	lda #SRTN.COMBAT.EXIT	;load LO address
	sta parm.namlo
	lda /SRTN.COMBAT.EXIT	;load HO address
	sta parm.namhi 
	
		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO
	
@END


;CALL COMBAT EXIT
	JSR COMBAT.EXIT
	
	
			
	JMP .EXIT		;**OPT** Memory. Seems like this JMP can be removed. 
	

		;**OPT** Memory. Consolidate the above two calls
		
.EXIT
	
	JSR SWAP.MAIN_MEMORY.IN ;swap in whatever was in the swap space before the COMBAT file was loaded into it

	
	RTS
@END

MOB.INITIATED.COMBAT.ENTRANCE
@START
;PARAMETERS: COMBAT_SE.MODE.PARM	;($00 = player initiated or init value in map objects manager | $01 = mob initiated | $FF = test mode), COMBAT_SE.S_ENTITY.TYPE, COMBAT_SE.S_ENTITY.MO_INDEX
		 
	;load sindex for attacking mob
	LDA	MAP_OBJECTS.TILE_LOCATION
	PHA ;push attacking MOB screen index to stack

	;launch combat session
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
	 

		;Y-REG: ;MAP_OBJECTS.TILE_LOCATION (sindex of attacking MOB)
	JSR DELETE.S_ENTITY
	
	RTS
	
@END

LAUNCH.INVENTORY.MODULE
@START
;PARAMETERS: ACC = inventory sub-module # to launch
;ENTRANCE: direct
;RETURN: none

;SAVE PARAMETERS
	PHA ;push parm: inventory sub-module code to stack	(PARM: ACC = inventory sub-module # to launch)

		
;LOAD FILE "SRTN.INVENTORY" (only loads part of the file)

;------OPEN FILE----       ;**OPT** Memory. Replace with a call to PRODOS.IO.OPEN_FILE
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

    lda #cmd_read.drive1
	sta parm.reqcmd
	
;set destination memory address
	lda #INV.ENTRANCE.PATCH_START
	sta parm.ldrlo
	lda /INV.ENTRANCE.PATCH_START
	sta parm.ldrhi

	
;set read length (bytes)
	lda #$01				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda #$00				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi
	
;set filename to read from	
	lda #SRTN.INVENTORY	;load LO address
	sta parm.namlo
	lda /SRTN.INVENTORY	;load HO address
	sta parm.namhi 

		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO

	;reset file offset
	;(the file offset was incremented by reading in the one byte above. Other routines will assume the fileset is set to the beginning of the file)
	JSR PRODOS.IO.RESET.FILE_OFFSET

@END

;------SEEK FILE------
@START

.SEEK.FILE
	lda #cmd_seek.current_drive
	sta parm.reqcmd

;set seek length (bytes)
	lda #INV.ENTRANCE.SEEK_BYTES			;seek length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda /INV.ENTRANCE.SEEK_BYTES			;seek length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi
	
		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO
			
	;**FALLS THROUGH**

@END

;-------READ--------
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

	lda #cmd_read.current_drive
	sta parm.reqcmd
	

;set read length (bytes)
	lda #INV.ENTRANCE.READ_BYTES				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda /INV.ENTRANCE.READ_BYTES				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi
			
		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO	
@END


;PASS CONTROL TO INVENTORY MODULE
		PLA ;pull parm: inventory sub-module # to launch from stack	
	JSR INV.INVENTORY_MODULE.ENTRANCE

.EXIT

	RTS
@END

;graphics
COPY.SCREEN.ENTRANCE
@START	
		;ENABLE BANK-SWITCHED RAM ($D000 Bank 1)
		LDA $C08B	 	;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK1, 1ST)
		LDA $C08B
	JSR COPY.SCREEN	
		;RE-ENABLE BANK-SWITCHED RAM ($D000 Bank 2)
		LDA $C083				;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK2 2nd)
		LDA $C083
		
	RTS
@END
	
ANIMATION.UPDATE.ENTRANCE
@START	
		;ENABLE BANK-SWITCHED RAM ($D000 Bank 1)
		LDA $C08B 
		LDA $C08B
	JSR ANIMATION.UPDATE
		;RE-ENABLE BANK-SWITCHED RAM ($D000 Bank 2)
		LDA $C083				;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK2 2nd)
		LDA $C083

	RTS
@END
	
DARKNESS.REVIEW
@START
;(Note: this wrapper is used to save memory so that each call doesn't need
;to enable/disable AUX_BSR)

;VALIDATE ENTRY
;(if COMBAT module is loaded don't permit entry because a) a hang will occur due to COMBAT clobbering AUX BSR (where the darkness routines are located)
;and b) the COMBAT module doesn't need the darkness routines

	LDA PLAYER.MAP.LOCATION_TYPE		;load map type code
	CMP #MAP.TYPE.COMBAT			;is map type = combat?
	BEQ .EXIT ;branch if yes

.EXECUTE.CALL
			
		TSX			;transfer stack pointer to X-REG
		STX STACK_POINTER.SAVED	;save stack pointer
	STA $C009 ;enable aux zero-page & aux BSR 

	
	JSR DARKNESS.REVIEW.AUX_BSR.ENTRANCE


			
	STA $C008 ;enable main zero-page & main BSR 
		LDX STACK_POINTER.SAVED	;restore stack pointer to X-REG
		TXS ;transfer X-REG to stack pointer

.EXIT
		
	RTS
	
	
@END
	

SCROLL.ROWS.ENTRANCE
@START
	
		;ENABLE BANK-SWITCHED RAM ($D000 Bank 1)
		LDA $C08B					;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK1, 1ST)
		LDA $C08B	
	JSR SCROLL.ROWS	
		;ENABLE BANK-SWITCHED RAM ($D000 Bank 1)
		LDA $C083				;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK2 2nd)
		LDA $C083
	
	RTS

@END



SCROLL.COLUMNS.ENTRANCE
@START
	
		;ENABLE BANK-SWITCHED RAM ($D000 Bank 1)
		LDA $C08B					;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK1, 1ST)
		LDA $C08B	
	JSR SCROLL.COLUMNS
		;ENABLE BANK-SWITCHED RAM ($D000 Bank 1)
		LDA $C083				;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK2 2nd)
		LDA $C083
	
	RTS

@END



		
@END

	
STATUS_BAR.DRAW
@START
;PARAMETERS: ACC = direction code ($00 = increases to left | >=$01 increases to right), X-REG = start line, Y-REG = start sbyte, STATUS_BAR.SEGMENTS, STATUS_BAR.MAX_SEGMENTS.PLUS_ONE
;ENTRANCE: direct
;RETURN: 5 segment status bar drawn on video screen

;SAVE PARAMETERS
	STA STATUS_BAR.DIRECTION_CODE ;($00 = increases to left | >=$01 increases to right)

; ;SAVE REGISTERS
	; TXA
	; PHA
	
			
.INIT
	LDA #$01
	STA STATUS_BAR.LOOP_COUNTER

	STY STATUS_BAR.CURRENT_SBYTE
	STX STATUS_BAR.CURRENT_LINE
	

.LOOP.STATUS_BAR.DRAW

.DRAW.BYTE0
		LDY STATUS_BAR.CURRENT_SBYTE
		LDX STATUS_BAR.CURRENT_LINE		

		LDA STATUS_BAR.SEGMENTS
		CMP STATUS_BAR.LOOP_COUNTER
		BCC .DRAW.EMPTY_SEGMENT0
;.DRAW.FULL_SEGMENT0		
		LDA STATUS_BAR.BYTE0_VALUE
		JMP .EXECUTE.DRAW0
.DRAW.EMPTY_SEGMENT0
		LDA #$00 ;set all pixels to off (erase)
.EXECUTE.DRAW0
	JSR DRAW.BYTE

			
.NEXT.STATUS_BAR.SEGEMENT.BYTE0	
	JSR .NEXT.STATUS_BAR.SEGEMENT
	
.STATUS_BAR.EXIT_TEST.BYTE0	
	LDA STATUS_BAR.LOOP_COUNTER
	CMP STATUS_BAR.MAX_SEGMENTS.PLUS_ONE
	BEQ .LOOP.STATUS_BAR.DRAW.COMPLETE

	
.DRAW.BYTE1

		LDY STATUS_BAR.CURRENT_SBYTE
		LDA STATUS_BAR.SEGMENTS
		CMP STATUS_BAR.LOOP_COUNTER
		BCC .DRAW.EMPTY_SEGMENT1
;.DRAW.FULL_SEGMENT1		
		LDA STATUS_BAR.BYTE1_VALUE
		JMP .EXECUTE.DRAW1
.DRAW.EMPTY_SEGMENT1
		LDA #$00 ;set all pixels to off (erase)
.EXECUTE.DRAW1
	JSR DRAW.BYTE

			
.NEXT.STATUS_BAR.SEGEMENT.BYTE1				
	JSR .NEXT.STATUS_BAR.SEGEMENT
	

.STATUS_BAR.EXIT_TEST.BYTE1	
	LDA STATUS_BAR.LOOP_COUNTER
	CMP STATUS_BAR.MAX_SEGMENTS.PLUS_ONE
	BNE .LOOP.STATUS_BAR.DRAW

.LOOP.STATUS_BAR.DRAW.COMPLETE
	
	
; ;RESTORE REGISTERS
	; PLA
	; TAX
	
.EXIT

	RTS

.NEXT.STATUS_BAR.SEGEMENT
@START
	INC STATUS_BAR.LOOP_COUNTER

	LDA STATUS_BAR.DIRECTION_CODE ;($00 = increases to left | >=$01 increases to right)
	BNE .INCREASE_RIGHT
;.INCREASE_LEFT
	DEC STATUS_BAR.CURRENT_SBYTE
	JMP .NEXT.STATUS_BAR.SEGEMENT.BYTE1.DONE
	
.INCREASE_RIGHT
	INC STATUS_BAR.CURRENT_SBYTE
.NEXT.STATUS_BAR.SEGEMENT.BYTE1.DONE	
	RTS
@END

@END

CURSOR.DRAW
@START
;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA
	

	LDY HTAB	;set screen byte of cursor shape
	LDA VTAB
	ASL ;X2
	ASL ;X4
	ASL ;X8
	TAX			;set top hi-res line of cursor shape
	CLC
	ADC #$08
	STA TILE.LINE.STOP ;set bottom hi-res line cursor shape. Used as stop value
	
	LDA #$00
	STA SHP.TBL.CNTR


		
.LOOP.CURSOR.DRAW
;KEYPRESS CHECK
	LDA KB_BUFFER
    BPL .KEYPRESS_CHECK.DONE ;branch if no keypress
	;if keypress then exit loop
    ;STA KB_BUFFER_ACK               ;CLR LAST KEY
	JMP .EXIT
.KEYPRESS_CHECK.DONE

		LDA PAGE.BACKGROUND
	JSR GET.LINE.ADDRESS1

		LDA PAGE.FOREGROUND
	JSR GET.LINE.ADDRESS2

	TXA
	PHA
	LDA ANIMATION.FRAME_STATE
	ASL ;X2  calculate index to shape table for current animation frame
	ASL ;X4
	ASL ;X8
	CLC
	ADC SHP.TBL.CNTR
	TAX

	LDA .CURSOR.SHAPE_TABLE,X
	;LDA #$7F
	
		; ;JSR KEYIN
		; LDY #$AA
		; JSR PREP.BRK
		; BRK
		
	STA (LINE.BASE.ADDR1),Y		;save bit mapped byte to video screen memory (foreground page)  
	STA (LINE.BASE.ADDR2),Y		;save bit mapped byte to video screen memory (background page)
	PLA
	TAX


	INC SHP.TBL.CNTR ;next shape table byte
	INX ;next cursor line
	CPX TILE.LINE.STOP	
	BNE .LOOP.CURSOR.DRAW
	

.EXIT
;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX

	RTS

.CURSOR.SHAPE_TABLE
.CURSOR.SHAPE_TABLE1.1			.HS	00.1E.00.3C.00.1E.00.3C
.CURSOR.SHAPE_TABLE1.2			.HS	3C.00.1E.00.3C.00.1E.00
.CURSOR.SHAPE_TABLE1.3			.HS	00.3C.00.1E.00.3C.00.1E
.CURSOR.SHAPE_TABLE1.4			.HS	1E.00.3C.00.1E.00.3C.00

@END

;===============MISC TOOLS========

MAP.CALCULATE.SS_FLAGS ;CALCUALTE SS MAP REGIONAL FLAGS BASED ON RMAP.X/Y 
@START
;PARAMETERS: RMAP.X, RMAP.Y
;RETURN: MAP_OBJECTS.SS.X_FLAG.UPPER, MAP_OBJECTS.SS.Y_FLAG.UPPER, MAP_OBJECTS.SS.X_FLAG.LOWER, MAP_OBJECTS.SS.Y_FLAG.LOWER
;ENTRANCE: DIRECT

;Note: the principle in play is that flags, to detect when a mob x,y value (relative to the player) is off the 
;regional map, need to float depending on the player position withing the region. 
	
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

;===============DATA TOOLS========

;MAP OBJECT ARRAYS
@START
LOOKUP.MAP_OBJECT.ARRAY
@START
;PARAMETERS: Y-REG (screen array index), [MAP_OBJECT.ARRAY.ERROR_TRAP_OVERRIDE]
;RETURN: MAP_OBJECT.ARRAY.POINTER(2), MAP_OBJECT.ARRAY.CODE*, MAP_OBJECT.ARRAY.S_ENTITY.TYPE, [(DISABLED) MAP_OBJECT.ARRAY.S_ENTITY.NOT_FOUND]
;ENTRANCE: DIRECT, READ.MAP_OBJECT.ARRAY, WRITE.MAP_OBJECT.ARRAY, CLEAR.MAP_OBJECT.WRITE.BUFFER, 
;
;*MAP_OBJECT.ARRAY.CODE ;identifies the array associated with the S_ENTITY specified. ($FE = MAP.OBJECTS.MOB | $FF = MAP.OBJECTS.NPC)


;=====================SUBROUTINE DOCUMENTATION====================================
;This subroutine is used to lookup the map object array associated with a specific S_ENTITY,
;which is identified via the S_ENTITY type code in the SCREEN.MO_SPRITE_TYPE.DATA array. 
;
;The calling routine can then do an indirect lookup by using MAP_OBJECT.ARRAY.POINTER to access
;the desired map object array.
;
;The calling routine should handle the save/restore of the Y-REG register unless Y-REG can be clobbered.
;This is because Y-REG will need to contain the map object index when doing lookup using MAP_OBJECT.ARRAY.POINTER
;
;*NOTE: Specials
;
;This routine returns the true index for specials. i.e. index to the record in MAP_OBJECTS.NPC rather than
;the index to the COMBAT.MAP_OBJECTS.SPECIAL array, which is just a pointer to MAP_OBJECTS.NPC+$80
;=================================================================================

;BPL returns true if the high bit (bit7) is not set
;BMI returns true if the high bit (bit7) is set 

			
	;ACC: lookup mode (high bit set | high bit not set), and if high bit is set, then ACC is S_ENTITY type code. 
	BPL .LOOKUP_MODE.SINDEX	
	;ACC = S_ENTITY type + high bit set		
	JMP .LOOKUP_MODE.S_ENTITY_TYPE
	
.LOOKUP_MODE.SINDEX
	LDA SCREEN.MO_SPRITE_TYPE.DATA,Y	;load s_entity type
	;mask-out of MTT bit flags occurs below
	
	;branch based on the type of S_ENTITY found
	
	;**FALLS THROUGH**
	
.LOOKUP_MODE.S_ENTITY_TYPE
	;ACC = S_ENTITY type
	AND #$07 ;mask-out high bit and MTT flags (bits 3-6)
	STA MAP_OBJECT.ARRAY.S_ENTITY.TYPE
	CMP #S_ENTITY_TYPE.NC_MOB
	BEQ	.MOB_ARRAY
	CMP #S_ENTITY_TYPE.C_MOB
	BEQ	.NPC_ARRAY
	CMP #S_ENTITY_TYPE.SPECIAL
	BEQ .NPC_ARRAY
	CMP #S_ENTITY_TYPE.PC
	BEQ .MOB_ARRAY
	CMP #S_ENTITY_TYPE.BLD_NPC
	BEQ .NPC_ARRAY
	CMP #S_ENTITY_TYPE.DNG_NPC
	BEQ .NPC_ARRAY
	
	;**FALLS THROUGH**
	
.S_ENTITY_TYPE.ERROR
;.DETERMINE.S_ENTITY.TYPE (CL.S_ENTITY.FOUND) reports unexpected S_ENTITY type value.
		; LDA #SELECT_TARGET.LAST_TARGET
		; LDX /SELECT_TARGET.LAST_TARGET

	; LDA MAP_OBJECT.ARRAY.ERROR_TRAP_OVERRIDE		
	; CMP #$FF
	; BEQ .ERROR_TRAP_OVERRIDE
	

	JSR PREP.BRK
	BRK
	
; .ERROR_TRAP_OVERRIDE
	; ;set S_ENTITY not found return value
	; LDA #$01
	; STA MAP_OBJECT.ARRAY.S_ENTITY.NOT_FOUND ;($00 = S_ENTITY found | $01 = S_ENTITY not found)
	
	; JMP .EXIT
	
.NPC_ARRAY		
	;set array pointer
	LDA #MAP_OBJECTS.NPC
	STA MAP_OBJECT.ARRAY.POINTER+$0

	LDA /MAP_OBJECTS.NPC
	STA MAP_OBJECT.ARRAY.POINTER+$1
	
	;set array type
	LDA #$FF
	STA MAP_OBJECT.ARRAY.CODE
	
			
	JMP .EXIT

.MOB_ARRAY
	;set array pointer
	LDA #MAP_OBJECTS.MOB
	STA MAP_OBJECT.ARRAY.POINTER+$0

	LDA /MAP_OBJECTS.MOB
	STA MAP_OBJECT.ARRAY.POINTER+$1

	;set array type
	LDA #$FE
	STA MAP_OBJECT.ARRAY.CODE

	
	;**FALLS THROUGH**


.EXIT
	RTS

@END

READ.MAP_OBJECT.ARRAY
@START
;PARAMETERS: *ACC (#of bytes to read or lookup mode (high bit not set = lookup by screen index, high bit set = lookup by map object record index & S_ENITY type), Y-REG (screen array index | map object record index), [(DISABLED) MAP_OBJECT.ARRAY.ERROR_TRAP_OVERRIDE]
;RETURN: MAP_OBJECT.RECORD.READ(8), MAP_OBJECT.ARRAY.S_ENTITY.TYPE, MAP_OBJECT.ARRAY.MO_INDEX, MAP_OBJECT.ARRAY.CODE**, [(DISABLED) MAP_OBJECT.ARRAY.S_ENTITY.NOT_FOUND]
;ENTRANCE: DIRECT
;
;*if lookup mode is S_ENTITY type, then # of bytes to read defaults to (8), and S_ENTITY type is stored in ACC
;			;**OPT** memory. Change lookup mode parm to carry flag status, default to an 8 byte read, then remove the LDA #$08 from all the calls to this routine. 
;**MAP_OBJECT.ARRAY.CODE ;identifies the array associated with the S_ENTITY specified. ($FE = MAP.OBJECTS.MOB | $FF = MAP.OBJECTS.NPC)

;=====================SUBROUTINE DOCUMENTATION====================================
;
;*reads in part or all of a record for a specific S_ENTITY.
;*use ACC to specify the # of bytes to read
;
;===================================================================

;BPL returns true if the high bit (bit7) is not set
;BMI returns true if the high bit (bit7) is set 
;


;;****TEMPLATE*****
		; ;read $8 bytes
		; LDA #$08 ;(high bit not set = lookup by screen index)
		; Y-REG (screen array index | map object record index)
	; JSR READ.MAP_OBJECT.ARRAY
		; ;RETURN VALUE = MAP_OBJECT.RECORD.READ(8)
		


	
;SAVE PARAMETERS
	;ACC = parm: # of bytes per read or S_ENTITY type code, depending on mode
	STA SAVED.ACC.LOCAL ;save parm: # of bytes to read

;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA
	
	LDA SAVED.ACC.LOCAL ;restore parm: # of bytes to read
	BMI .LOOKUP.BY_TYPE ;if high bit set then lookup mode is Lookup by Type and map object record index

.LOOKUP.BY_SINDEX	
		
	;get pointer to map object array for the S_ENTITY specified
	;in the Y-REG parameter
	
		;ACC = high bit not set
	JSR LOOKUP.MAP_OBJECT.ARRAY



			
	LDA SCREEN.MO_SPRITE.DATA,Y ;load map object record index for specified S_ENTITY
	TAY ;transfer map object record index
	JMP .READ.ENTRANCE
	
.LOOKUP.BY_TYPE
			
	;get pointer to map object array for the S_ENTITY specified
	;in the SAVED.ACC.LOCAL (S_ENTITY_TYPE) parameter
	
		;ACC = S_ENTITY type + high bit set
	JSR LOOKUP.MAP_OBJECT.ARRAY		
	LDA #$08 ;default # of bytes to read to 8
	STA SAVED.ACC.LOCAL
			
	;YREG: map object record index (was parameter to this routine)	

	;**FALLS THROUGH**
	
.READ.ENTRANCE


			
			; LDA #$AA
			; LDX SELECT_TARGET.RADIUS
			; LDY COMBAT.ACQUIRE.TARGET.FINAL.SINDEX
			; JSR PREP.BRK
			; BRK

			
	STY MAP_OBJECT.ARRAY.MO_INDEX ;save map object index
	LDX #$00
.READ.LOOP


	
	
	LDA (MAP_OBJECT.ARRAY.POINTER),Y
	STA MAP_OBJECT.RECORD.READ,X
	INY
	INX
	CPX SAVED.ACC.LOCAL ;have specified # of bytes been read?

		
	BNE .READ.LOOP
	


			
.EXIT
	;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX
	
	RTS
	
@END

WRITE.MAP_OBJECT.ARRAY
@START
;PARAMETERS: *ACC (#of bytes to write) Y-REG (screen array index)
;RETURN: updated map object record
;ENTRANCE: DIRECT

;*ACC: $FF = write all bytes. 
	  ;$FE = write the first four bytes
	  ;$00-$07 = write only that specific byte.

;=====================SUBROUTINE DOCUMENTATION====================================
;
;*writes part or all of a buffer to map object record for a specific S_ENTITY.
;*use ACC to specify the # of bytes to read
;
;*How to avoid setting all bytes when doing a write:
;since MAP_OBJECT.RECORD.READ and MAP_OBJECT.RECORD.WRITE share the same memory space,
;one approach to changing a single byte is 
; *call READ.MAP_OBJECT.ARRAY
; *change the byte or bytes you want to modify
; *call WRITE.MAP_OBJECT.ARRAY
; 
;===================================================================

;SAVE PARAMETERS
	STA SAVED.ACC.LOCAL ;save parm: # of bytes to read
	
;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA

.INIT
	LDX #$00 ;loop counter and MAP_OBJECT.RECORD.READ index
	
	;get pointer to map object array for the S_ENTITY specified
	;in the Y-REG parameter
	JSR LOOKUP.MAP_OBJECT.ARRAY
				
	LDA SCREEN.MO_SPRITE.DATA,Y ;load map object record index for specified S_ENTITY
	TAY ;transfer map object record index


			
	LDA SAVED.ACC.LOCAL ;load parm: # of bytes to write
	CMP #$08 ;is parm set to all bytes?
	BCC .WRITE.SPECIFIC_BYTE
	CMP #$FF
	BEQ .WRITE.ALL_BYTES

	;**FALLS THROUGH**
	
.WRITE.FIRST_FOUR_BYTES	
	LDA #$04 ;set # of bytes to write to 8
	STA SAVED.ACC.LOCAL
	JMP .WRITE.LOOP
.WRITE.ALL_BYTES	
	LDA #$08 ;set # of bytes to write to 8
	STA SAVED.ACC.LOCAL
				
	;**FALLS THROUGH**
.WRITE.LOOP
	LDA MAP_OBJECT.RECORD.WRITE,X
	STA (MAP_OBJECT.ARRAY.POINTER),Y
	INY
	INX
	CPX SAVED.ACC.LOCAL ;have specified # of bytes been read?
	BNE .WRITE.LOOP


			
	JMP .EXIT
	
.WRITE.SPECIFIC_BYTE
	;TAX ;transfer byte # to write to X-REG

.SET.BYTE.NUMBER
	INY ;advance index 1 byte
	INX
	CPX SAVED.ACC.LOCAL
	BNE .SET.BYTE.NUMBER

.WRITE.BYTE	
	LDX SAVED.ACC.LOCAL ;set index to MAP_OBJECT.RECORD.WRITE to the byte # to write
	LDA MAP_OBJECT.RECORD.WRITE,X
	STA (MAP_OBJECT.ARRAY.POINTER),Y

	;**FALLS THROUGH**
	
		; lda #$AA
		; jsr prep.brk
		; brk
	
.EXIT


	;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX
	
	RTS
	
@END

CLEAR.MAP_OBJECT.WRITE.BUFFER
@START
;PARAMETERS: none
;RETURN: updated MAP_OBJECT.RECORD.WRITE, MAP_OBJECT.ARRAY.CODE*
;ENTRANCE: DIRECT

;*MAP_OBJECT.ARRAY.CODE ;identifies the array associated with the S_ENTITY specified. ($FE = MAP.OBJECTS.MOB | $FF = MAP.OBJECTS.NPC). This is set via the call to LOOKUP.MAP_OBJECT.ARRAY

	
;SAVE REGISTERS
	TXA
	PHA

.INIT
	LDX #$00 ;loop counter and MAP_OBJECT.RECORD.READ index
		
.WRITE.LOOP
	LDA #$00
	STA MAP_OBJECT.RECORD.READ,X
	INX
	CPX #$08 ;have specified # of bytes been read?
	BNE .WRITE.LOOP

	;not technically needed but this call sets the
	;MAP_OBJECT.ARRAY.CODE return value
	JSR LOOKUP.MAP_OBJECT.ARRAY
	
	;**FALLS THROUGH**
	
.EXIT
	;RESTORE REGISTERS
	PLA
	TAX
	
	RTS
@END

MAP_OBJECT.ADD.MTT_FLAG_BITS
@START
;PARAMETERS: ACC = S_ENTITY TYPE (without the MTT flag bits set), X-REG = MTT tile # (0-3)
;ENTRANCE: direct
;RETURN: ACC = S_ENTITY_TYPE (with flag bits)


;===BIT FLAG MATRIX===
;bit0-2: holds the S_ENTITY type value without the MTT bit flags. 
;		;currently values are $0-$5. Max value for bit 0-2 = $08
;bit3 set = tile 3. ORA mask to set = $08
;bit4 set = tile 2. ORA mask to set = $10
;bit5 set = tile 1. ORA mask to set = $20
;bit6 set = tile 0*. ORA mask to set = $40
;bit7: the init / no S_ENTITY found value is $FF which results in bit7 being set
;
;AND value to mask-out MTT bit flags: $87
;
;Note: The numberical order of the tiles is the reverse of the order of the bits so that
;tile0 (the index tile) is assigned to bit6, which an be tested via the overflow flag (BVS/BVC) 
;
;*tile0 is the index tile for MTT S_ENTITIES. I'm not sure if this tile has any special significance anymore. It used to be imporant
;to know the screen index of that tile so that the screen index of the 3 tiles of the MTT object could be calculated. Each of the 4 tiles screen indexes
;reference back to the same map object index.
;
;**OPT** Memory. Considering that $FF value for no S_ENTITY sets bit7, and the other legit value don't use bit 7, a lot of memory could be saved by
;changing the CMP/branch code to BMI in any case where $FF needs to be tested

	;ACC = S_ENTITY TYPE (without MTT bit flags)
	CPX #$00
	BEQ .TILE0 ;upper left
	CPX #$01
	BEQ .TILE1 ;upper right
	CPX #$02
	BEQ .TILE2 ;lower left
	CPX #$03
	BEQ .TILE3 ;lower right
	;***warning: no error trap for unexpected
	;tile value in X-REG
	;
	
.TILE0 ;upper left
;bit6 set = tile 3. ORA mask to set = $80

	;ACC = S_ENTITY TYPE (without MTT bit flags)
	ORA #$40
	JMP .EXIT

.TILE1 ;upper right
;bit5 set = tile 2. ORA mask to set = $40

	;ACC = S_ENTITY TYPE (without MTT bit flags)
	ORA #$20
	JMP .EXIT

.TILE2 ;lower left
;bit4 set = tile 1. ORA mask to set = $20

	;ACC = S_ENTITY TYPE (without MTT bit flags)
	ORA #$10

	JMP .EXIT

.TILE3 ;lower right	
;bit3 set = tile 0. ORA mask to set = $10

	;ACC = S_ENTITY TYPE (without MTT bit flags)
	ORA #$08


	;**FALLS THROUGH**
	
.EXIT
	
	;RETURN VALUE: ACC = S_ENTITY TYPE (with MTT bit flags)
	
	RTS
@END

;MAP_OJECTS.FIND.MTT.KEY ;***I never finished this routine. The intention was for it to return the sindex of the upper left tile of the MTT S_ENTITY associated with the Y-REG parm.
@START
;PARAMETERS: ;Y-REG = screen tile index of MTT S_ENTITY (any of the four tiles of the object)
;ENTRANCE: direct
;RETURN:

	; LDA SCREEN.MO_SPRITE.DATA,Y ;load map object record index of the killed S_ENTITY 
	; STA SAVED.ACC.LOCAL
	; LDA SCREEN.MO_SPRITE_TYPE.DATA,Y ;load type code of the killed S_ENTITY 

	;RTS
@END

DELETE.S_ENTITY
@START

;**OPT** Memory. If the zap mob playtest function is removed from the main game loop then DELETE.S_ENTITY can probably be moved to the combat stats file. 
;At that point it probably makes sense for the this routine to delete the mob from the graphics screen (currently handled elsewhere) in addition to deleting 
;from the screen arrays.

;PARAMETERS: Y-REG (screen array index)
;RETURN: updated map object record, ACC: ($00 = deleted S_ENTITY was single tile, $01 = deleted S_ENTITY was multi-tile)
;ENTRANCE: DIRECT

;SAVE REGISTERS
	TYA
	PHA
	
.DELETE.COMMON


			
		;**OPT** Memory. The detection of MTT could be moved into .TARGET.IS.KILLED (COMBAT.DAMAGE.ROLL), and the code to provide the single/MTT as a return value could be removed.
		;instead, single/MTT could be provided to this routine as a parameter via the ACC
		
		;Y-REG: screen index of map oject to read
		LDA #$08 ;parm for READ.MAP_OBJECT.ARRAY and coincidentally the AND mask value needed for the BIT operation below. 
		STA TEMP ;set bit3 for future BIT test.
	JSR READ.MAP_OBJECT.ARRAY
		;RETURN VALUE: MAP_OBJECT.RECORD.READ(8)	
		LDA MAP_OBJECT.RECORD.READ+$03 ;read flag byte
		PHA ;push save killed S_ENTITY flag byte to stack
		


				
	;set all values in buffer to $00
	JSR CLEAR.MAP_OBJECT.WRITE.BUFFER
		;return value: MAP_OBJECT.ARRAY.CODE ($FE = MAP.OBJECTS.MOB | $FF = MAP.OBJECTS.NPC)
	
		;redraw screen at current gmap position. mob's game turn is skipped 

		;set health status = "dead"
		LDA #COMBAT.S_ENTITY.STATUS.DEAD	
		STA MAP_OBJECT.RECORD.WRITE+$3 ;sets the "dead" health status
		STA MAP_OBJECT.RECORD.WRITE+$7 ;sets the "dead" health status

		;write either $4 or $8 bytes (all) depending on the array type associated with the S_ENTITY
		LDA MAP_OBJECT.ARRAY.CODE ;return value of CLEAR.MAP_OBJECT.WRITE.BUFFER. ($FE = MAP.OBJECTS.MOB | $FF = MAP.OBJECTS.NPC)	
		;Y-REG (screen array index)
	JSR WRITE.MAP_OBJECT.ARRAY

			; pla ;pop rts
			; pla
			; JMP GAME.LAUNCH

.DETERMINE.MOB.TYPE ;(single tile or multi-tile)				
		; LDA #$08 ;parm for READ.MAP_OBJECT.ARRAY and coincidentally the AND mask value needed for the BIT operation below. 
		; STA TEMP ;set bit3 for future BIT test.
		PLA ;pull save killed S_ENTITY flag byte from stack
		BIT TEMP ;test bit3 of MAP_OBJECT.RECORD.READ+$03
		BNE .DELETE.MULTI_TILE_MOB ;branch if bit3 is set (indicates a multi-tile mob)
		
		;**FALLS THROUGH**
	
.DELETE.SINGLE_TILE_MOB		
	;delete S_ENTITY from screen arrays
	LDA #$FF ;set init value
	STA SCREEN.MO_SPRITE.DATA,Y
	STA SCREEN.MO_SPRITE_TYPE.DATA,Y

	;set return value
	LDA #$00 ;set return = single tile S_ENTITY
	STA SAVED.ACC.LOCAL
	;**FALLS THROUGH**
		
.EXIT
;RESTORE REGISTERS
	PLA
	TAY
	
	;restore return value
	LDA SAVED.ACC.LOCAL

				; pla
				; tax
				; pla
				; tay
				; lda #$ab
				; jsr prep.brk
				; brk	
				
	RTS

.DELETE.MULTI_TILE_MOB			
	;delete S_ENTITY from screen arrays

	;start in the upper left corner of the MTT mob (which Y-REG is already set to)
	LDA #$FF ;set init value
	STA SCREEN.MO_SPRITE.DATA,Y
	STA SCREEN.MO_SPRITE_TYPE.DATA,Y
	
	INY ;move to the upper right tile of the MTT MOB
	
	STA SCREEN.MO_SPRITE.DATA,Y
	STA SCREEN.MO_SPRITE_TYPE.DATA,Y

	TYA
	CLC
	ADC #SCREEN.ARRAY.OFFSET  ;move to the lower right tile of the MTT MOB
	TAY

	LDA #$FF ;set init value
	STA SCREEN.MO_SPRITE.DATA,Y
	STA SCREEN.MO_SPRITE_TYPE.DATA,Y
	
	DEY ;move to the lower left tile of the MTT MOB

	STA SCREEN.MO_SPRITE.DATA,Y
	STA SCREEN.MO_SPRITE_TYPE.DATA,Y
	
	
	;set return value
	LDA #$01 ;set return = single tile S_ENTITY
	STA SAVED.ACC.LOCAL	
	
	
				
	JMP .EXIT
	

	
@END

@END

;CHARACTER SHEETS (**see ENTRANCE warning**)
@START
;ENTRANCE: ***Only from routines which don't clobber the SHAPE.HOPPER0 because the variables in this routine share space with 
;that array. See ;CHARACTER SHEET RELATED in offloaded_variables.asm for details
 

READ_WRITE.CHR_SHEET.POINTER
@START
;PARAMETERS: Y-REG: record or field index*, CHR_SHEET.POINTER(2), READ_WRITE.CHR_SHEET.RECORD.STOP_LENGTH, READ_WRITE.CHR_SHEET.MODE
;ENTRANCE: COMBAT.READ_WRITE.CHR_SHEET, READ_WRITE.CHR_SHEET.PC
;RETURN: CHR_SHEET.RECORD.READ

;*for the 8-bit routines for MOBs & SPECIAL, Y-REG holds the index to the record
	;for the 16-bt PC routine, Y-REG holds the field index since the record index is added to the pointer. 
;UPDATE (3/12/17): The MOB & SPECIAL routines have now been updated to 16-bit
		
;SET MEMORY STATE
	
	;ENABLE BANK-SWITCHED RAM ($D000 Bank 1)
	LDA $C08B 
	LDA $C08B
						
		; TSX			;transfer stack pointer to X-REG
		; STX TEMP	;save stack pointer
	STA $C009 ;enable aux zero-page & aux BSR 

	
;SET PARAMETERS (passed via registers)		
		
	LDX #$00
.READ_WRITE.CHR_SHEET.LOOP
	LDA READ_WRITE.CHR_SHEET.MODE ;($00 = read | $01 = write)
	BNE .WRITE
	;**FALLS THROUGH**
;.READ	
	LDA (CHR_SHEET.POINTER),Y
	STA CHR_SHEET.RECORD.READ,X
	JMP .INCREMENT.INDEXES
.WRITE
	LDA CHR_SHEET.RECORD.READ,X
	STA (CHR_SHEET.POINTER),Y
.INCREMENT.INDEXES
	INY
	INX
	CPX CHR_SHEET.RECORD.STOP_LENGTH
	BNE .READ_WRITE.CHR_SHEET.LOOP
	
;RESTORE MEMORY STATE
	STA $C008 ;enable main zero-page & main BSR 
		; LDX TEMP	;restore stack pointer to X-REG
		; TXS ;transfer X-REG to stack pointer

	;RE-ENABLE BANK-SWITCHED RAM ($D000 Bank 2)
	LDA $C083				;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK2 2nd)
	LDA $C083

	RTS
	
@END	

;COMBAT CHARACTERS SHEET READ/WRITE ROUTINES
;(these routines used to be in the combat module but were moved
;to the main toolbox to support access from other modules like inventory)
;**OPT** Memory. To free up main program memory, move these routines back to the COMBAT module and
;create duplicates for any other modules that need them, like the INVENTORY module
;Another option is to put these routines at the very start of the combat module, and tie the end address to a constant. 
;In the inventory module use the constant to reserve memory at the start of the module so that the character sheet read/write routines
;can be read from the combat file on disk into the reserved memory when the inventory module is loaded.
;I think this is the way to go.
;update: I'm not sure how it is possible to move these routines out of main game engine since skills like lockpick and pilfer would
;need to be accessed. But, maybe those values can get seeded into main game engine variables (i.e. update those values upon launch and levelup)
;
@START
COMBAT.READ_WRITE.CHR_SHEET.PC ;(READ/WRITE ENABLED)
@START
;PARAMETERS: *ACC (sequential player #) - (high-bit not set = read mode | high-bit set = write mode)
;ENTRANCE: direct
;RETURN: CHR_SHEET.RECORD.READ($80), CHR_SHEET.POINTER

;*ACC parameter examples:
;	player 1, read mode: set to $01  (high-bit not set)
;	player 1, wrote mode: set to $81 (high-bit set)

;;*****TEMPLATE****
	; ;read PC character sheet data
		; TXA
		; ;ACC = player sequential # (high-bit not set = read mode)
	; JSR COMBAT.READ_WRITE.CHR_SHEET.PC
		; ;RETURN VALUE = CHR_SHEET.RECORD.READ
		;
;
			; STA TEMP
			; ; LDA TROUBLESHOOTING.HOOK
			; ; CMP #$01
			; CPX #$02
			; BNE .TEMP
				; ;TROUBLESHOOTING LOOP
				; LDX #$00	
; .TEST.LOOP
				; LDA CHR_SHEET.RECORD.READ,X
				; STA $BF00,X
				; INX
				; BNE .TEST.LOOP	
			; LDA #$BB
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP
			
			



			
;SAVE PARAMETERS

.SET.MODE
	PHA ;push parameter for use later
	;ACC = parm: read/write mode
	BMI .SET.WRITE_MODE ;branch if high-bit is set (write mode)
;.SET.READ_MODE
	LDA #$00 ;set read mode value
	JMP .SET.MODE.COMPLETE
.SET.WRITE_MODE
	LDA #$01 ;set write mode value
	
	;**FALLS THROUGH**
	
.SET.MODE.COMPLETE
	;ACC = read/write mode value
	STA READ_WRITE.CHR_SHEET.MODE
	
.SET.PC.NUMBER
	PLA ;pull mode/player# parameter
	AND #$7F ;mask-out high-bit 
	;ACC = parm: PC sequential number (i.e. player 1-6)
	SEC
	SBC #$01 ;subtract one because the first character sheet index starts with $00 (so for player 1, we need the calculation to be 0*$20 = $00)
	STA CHR_SHEET.S_ENTITY.NUMBER
	
;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA

			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; lda #$ab
				; ; ; LDX PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
				; ; ; LDY PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
				; ; ; ldx #CHR_SHEET.PC.READIED_EQUIP.RECORD.READ
				; ; ; ldy /CHR_SHEET.PC.READIED_EQUIP.RECORD.READ
				; ; LDX #INV.RE.CONFLICT_RECORD1
				; ; LDY /INV.RE.CONFLICT_RECORD1
				; ldx #INV.SAVED.ITEM_TABLE.RECORD
				; ldy /INV.SAVED.ITEM_TABLE.RECORD
				; JSR PREP.BRK
				; BRK	
; .TEMP
			; LDA TEMP
			
		;set parameters
		
		;calculate character sheet index
		;(see docs for this routine (.READ_WRITE.CHR_SHEET.PC) above for formula overview)

		LDA CHR_SHEET.S_ENTITY.NUMBER ;ACC = player number
		STA MULPLR
		LDA #$00
		STA MULPLR+$1
		;
		LDA #CHR_SHEET.PC.RECORD_SIZE
		STA MULCND
		LDA #$00
		STA MULCND+$01
	JSR MLP.16.NO_BCD
		;RETURN VALUE: result+$0 (product LO), result+$1 (product HO)

	

			
		;set parameters for READ_WRITE.CHR_SHEET.POINTER
		;(note: aux zero-page needs to be enabled before setting the zero page parameters that will be used in READ_WRITE.CHR_SHEET.POINTER after aux memory is enabled)
		STA $C009 ;enable aux zero-page & aux BSR 
						
		;connect character sheet pointer
		LDA #CHR_SHEET.PC.AUX_MEMORY.START
		CLC
		ADC RESULT+$0 ;PC character sheet record index, LO byte
		STA CHR_SHEET.POINTER+$0

		LDA /CHR_SHEET.PC.AUX_MEMORY.START
		ADC RESULT+$1 ;16-bit add. PC character sheet record index, LO byte
		STA CHR_SHEET.POINTER+$1

		STA $C008 ;enable main zero-page & main BSR 
		
		;set stop value
		LDA #CHR_SHEET.PC.RECORD_SIZE
		STA CHR_SHEET.RECORD.STOP_LENGTH

		LDY #$00 ;set field index
.PC.EXECUTE.READ		
	JSR READ_WRITE.CHR_SHEET.POINTER
	

.EXIT
;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX
	
	RTS
	
@END

;COMBAT.READ_WRITE.CHR_SHEET.SPECIAL
@START

;**use COMBAT.READ_WRITE.CHR_SHEET.MOB, just add $10 to the sequential SPECIAL #. 
;by adding $10 that forces the offset to the start of the SPECIAL array (since there are $10 MOB records). The value
;It isn't necessary to worry about SPECIAL map object records starting at $80 instead of $00 because 
;COMBAT.READ_WRITE.CHR_SHEET.MOB doesn't use the MO index in the calculation. 

@END

COMBAT.READ_WRITE.CHR_SHEET.MOB
@START
;PARAMETERS: *ACC (sequential mob #) - (high-bit not set = read mode | high-bit set = write mode)
;ENTRANCE: direct
;RETURN: CHR_SHEET.RECORD.READ($80), CHR_SHEET.POINTER

	
;SAVE PARAMETERS

.SET.MODE
	PHA ;push parameter for use later
	;ACC = parm: read/write mode
	BMI .SET.WRITE_MODE ;branch if high-bit is set (write mode)
;.SET.READ_MODE
	LDA #$00 ;set read mode value
	JMP .SET.MODE.COMPLETE
.SET.WRITE_MODE
	LDA #$01 ;set write mode value
	
	;**FALLS THROUGH**
	
.SET.MODE.COMPLETE
	;ACC = read/write mode value
	STA READ_WRITE.CHR_SHEET.MODE
	
.SET.MOB.NUMBER
	PLA ;pull mode/player# parameter
	AND #$7F ;mask-out high-bit 
	;ACC = parm: PC sequential number (i.e. player 1-6)
	SEC
	SBC #$01 ;subtract one because the first character sheet index starts with $00 (so for player 1, we need the calculation to be 0*$20 = $00)
	STA CHR_SHEET.S_ENTITY.NUMBER

;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA


		;set parameters
		
		;calculate character sheet index
		;(see docs for this routine (.READ_WRITE.CHR_SHEET.PC) above for formula overview)

		;**OPT** Memory. Replace with in-line 16-bit power of 2 multiplication using ASL & ROL
		;(see ";MLP.16 using powers of 2" in routines_math.ASM)
		LDA CHR_SHEET.S_ENTITY.NUMBER
		STA MULPLR
		LDA #$00
		STA MULPLR+$1
		;
		LDA #CHR_SHEET.MOB.RECORD_SIZE
		STA MULCND
		LDA #$00
		STA MULCND+$01
	JSR MLP.16.NO_BCD
		;RETURN VALUE: result+$0 (product LO), result+$1 (product HO)

		;(note: aux zero-page needs to be enabled before setting the zero page parameters that will be used in READ_WRITE.CHR_SHEET.POINTER after aux memory is enabled)
		STA $C009 ;enable aux zero-page & aux BSR 
				
		;connect character sheet pointer
		LDA #CHR_SHEET.MOB.AUX_MEMORY.START
		CLC
		ADC RESULT+$0 ;MOB character sheet record index, LO byte
		STA CHR_SHEET.POINTER+$0

		LDA /CHR_SHEET.MOB.AUX_MEMORY.START
		ADC RESULT+$1 ;16-bit add. MOB character sheet record index, LO byte
		STA CHR_SHEET.POINTER+$1
		
		STA $C008 ;enable main zero-page & main BSR 
		
		;set stop value
		LDA #CHR_SHEET.MOB.RECORD_SIZE
		STA CHR_SHEET.RECORD.STOP_LENGTH
		
		LDY #$00 ;set field index
;.MOB.EXECUTE.READ		
	JSR READ_WRITE.CHR_SHEET.POINTER

.EXIT	
;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX
	
	RTS
	
@END

COMBAT.READ_WRITE.CHR_SHEET
@START
;PARAMETERS: Y-REG (screen array index of S_ENTITY whose character sheet will be read), ACC = mode ($00 = read | $01 = write)
;ENTRANCE: direct
;RETURN: CHR_SHEET.RECORD.READ($80), CHR_SHEET.POINTER

;=====================SUBROUTINE DOCUMENTATION====================================
;
;This routine is the entrance in the combat module for all character sheet 
;reads, for all S_ENTITY types,
;
;The S_ENTITY type is used to determine the start address of the character sheet array
;in aux memory so that READ_WRITE.CHR_SHEET.POINTER, the general purpose character sheet array read routine, can be used.  
;
;This routine also converts the map object index of the S_ENTITY to the character sheet index. 
;
;=================================================================================

	
.SAVE.PARAMETERS
	;ACC = parm: ($00 = read | $01 = write)
	STA READ_WRITE.CHR_SHEET.MODE
	
			; LDA CHR_SHEET.RECORD.READ+$0C
			; CMP #$39
			; BNE .SAVE.REGISTERS
			; ;TROUBLESHOOTING LOOP
			; LDX #$00
; .TEST.LOOP			
			; LDA CHR_SHEET.RECORD.READ,X
			; STA $BE00,X
			; INX
			; BNE .TEST.LOOP			
			; JSR PREP.BRK
			; BRK
			
.SAVE.REGISTERS
	TXA
	PHA
	TYA
	PHA


.GET.MAP_OJECT.INDEX
	LDA SCREEN.MO_SPRITE.DATA,Y	;load s_entity map object index
	TAX ;tranfer map object index

.IDENTIFY.CHR_SHEET
	LDA SCREEN.MO_SPRITE_TYPE.DATA,Y	;load s_entity type
	AND #$87 ;mask-out MTT flags (bits 3-6)
	;branch based on the type of S_ENTITY found
		
	;ACC = S_ENTITY type	
	; CMP #S_ENTITY_TYPE.NC_MOB
	; BEQ	.MOB_ARRAY
	CMP #S_ENTITY_TYPE.C_MOB
	BEQ	.READ_WRITE.CHR_SHEET.MOB
	CMP #S_ENTITY_TYPE.SPECIAL
	BEQ .READ_WRITE.CHR_SHEET.SPECIAL
	CMP #S_ENTITY_TYPE.PC
	
;*****REMOVE THE COW CODE ***	
	;BEQ .READ_WRITE.CHR_SHEET.PC
	BNE .COW
	JMP .READ_WRITE.CHR_SHEET.PC

.COW

	; CMP #S_ENTITY_TYPE.BLD_NPC
	; BEQ .NPC_ARRAY
	; CMP #S_ENTITY_TYPE.DNG_NPC
	; BEQ .NPC_ARRAY
	
	;**FALLS THROUGH**
	
.S_ENTITY_TYPE.ERROR
;.DETERMINE.S_ENTITY.TYPE_AA (CL.S_ENTITY.FOUND) reports unexpected S_ENTITY type value.
		; LDA #SELECT_TARGET.LAST_TARGET
		; LDX /SELECT_TARGET.LAST_TARGET

			; pla ;pop saved registers
			; pla ;""
			; pla	;pull RTS
			; tax
			; pla ;""
			; tay

	ldA COMBAT.PC.ACTIVE.SINDEX		;**OPT** Memory. This LDX is for troubleshooting only and can be removed. 
	STA $BE00
	lda #$ee
		;LDX COMBAT.STATS.HIT_MISS_KILL.FLAG ;($00 = hit | $01 = miss | $02 = kill)

	LDX COW+$0
	LDY COW+$1
	JSR PREP.BRK
	BRK


.READ_WRITE.CHR_SHEET.MOB
@START
;note: the MOB map object record size is $8 and the mob character sheet record size is $20
;so MOB char_sheet index = mob MO index *4
;example: MO index = $08 (start of 2nd MOB MO record) * $4 = $20 (start of 2nd character sheet record)
			
		;set parameters
		
		;calculate character sheet index
		;(see docs for this routine (.READ_WRITE.CHR_SHEET.PC) above for formula overview)

		;**OPT** Memory. Replace with in-line 16-bit power of 2 multiplication using ASL & ROL
		;(see ";MLP.16 using powers of 2" in routines_math.ASM)
		TXA ;transfer map object index
		STA MULPLR
		LDA #$00
		STA MULPLR+$1
		;
		LDA #$04
		STA MULCND
		LDA #$00
		STA MULCND+$01
	JSR MLP.16.NO_BCD
		;RETURN VALUE: result+$0 (product LO), result+$1 (product HO)

		;(note: aux zero-page needs to be enabled before setting the zero page parameters that will be used in READ_WRITE.CHR_SHEET.POINTER after aux memory is enabled)
		STA $C009 ;enable aux zero-page & aux BSR 
				
		;connect character sheet pointer
		LDA #CHR_SHEET.MOB.AUX_MEMORY.START
		CLC
		ADC RESULT+$0 ;MOB character sheet record index, LO byte
		STA CHR_SHEET.POINTER+$0

		LDA /CHR_SHEET.MOB.AUX_MEMORY.START
		ADC RESULT+$1 ;16-bit add. MOB character sheet record index, LO byte
		STA CHR_SHEET.POINTER+$1
		
		STA $C008 ;enable main zero-page & main BSR 
		
		;set stop value
		LDA #CHR_SHEET.MOB.RECORD_SIZE
		STA CHR_SHEET.RECORD.STOP_LENGTH


			
		LDY #$00 ;set field index
	JSR READ_WRITE.CHR_SHEET.POINTER

	JMP .EXIT
	

	
@END

.READ_WRITE.CHR_SHEET.SPECIAL
@START
;note: the SPECIAL map object record size is $8 and the SPECIAL character sheet record size is $20
;so SPECIAL char_sheet index = mob MO index *4
;example: MO index = $08 (start of 2nd SPECIAL MO record) * $4 = $20 (start of 2nd character sheet record)

		;set parameters
		
		;calculate character sheet index
		;(see docs for this routine (.READ_WRITE.CHR_SHEET.PC) above for formula overview)

		;**OPT** Memory. Replace with in-line 16-bit power of 2 multiplication using ASL & ROL
		;(see ";MLP.16 using powers of 2" in routines_math.ASM)
		
		TXA ;transfer SPECIAL map object index
		SEC ;adjust SPECIAL MO index to a $00 start (normally they start at $80). This is necessary because we are using the MO index to 
			;represent the record number of the SPECIAL within the SPECIAL character sheet array. 
		SBC #COMBAT.MAP_OBJECTS.SPECIAL.START_RECORD
			
		STA MULPLR
		LDA #$00
		STA MULPLR+$1
		;
		LDA #$04
		STA MULCND
		LDA #$00
		STA MULCND+$01
	JSR MLP.16.NO_BCD
		;RETURN VALUE: result+$0 (product LO), result+$1 (product HO)

		;(note: aux zero-page needs to be enabled before setting the zero page parameters that will be used in READ_WRITE.CHR_SHEET.POINTER after aux memory is enabled)
		STA $C009 ;enable aux zero-page & aux BSR 
				
		;connect character sheet pointer
		LDA #CHR_SHEET.SPECIAL.AUX_MEMORY.START
		CLC
		ADC RESULT+$0 ;SPECIAL character sheet record index, LO byte
		STA CHR_SHEET.POINTER+$0

		LDA /CHR_SHEET.SPECIAL.AUX_MEMORY.START
		ADC RESULT+$1 ;16-bit add. SPECIAL character sheet record index, LO byte
		STA CHR_SHEET.POINTER+$1
		
		STA $C008 ;enable main zero-page & main BSR 
		
		;set stop value
		LDA #CHR_SHEET.SPECIAL.RECORD_SIZE
		STA CHR_SHEET.RECORD.STOP_LENGTH

		LDY #$00 ;set field index
	JSR READ_WRITE.CHR_SHEET.POINTER

	JMP .EXIT		

@END
	
.READ_WRITE.CHR_SHEET.PC
@START
;note: the PC map object record size is $4 and the PC character sheet record size is $80
;so PC char_sheet index = MO index * $20 
;example: MO index = $04 (start of 2nd PC MO record) * $20 = $80 (start of 2nd character sheet record)

			
		;set parameters
		
		;calculate character sheet index
		;(see docs for this routine (.READ_WRITE.CHR_SHEET.PC) above for formula overview)

		;**OPT** Memory. Replace with in-line 16-bit power of 2 multiplication using ASL & ROL
		;(see ";MLP.16 using powers of 2" in routines_math.ASM)

		TXA ;transfer PC map object index
		STA MULPLR
		LDA #$00
		STA MULPLR+$1
		;
		LDA #$20
		STA MULCND
		LDA #$00
		STA MULCND+$01
	JSR MLP.16.NO_BCD
		;RETURN VALUE: result+$0 (product LO), result+$1 (product HO)

		;(note: aux zero-page needs to be enabled before setting the zero page parameters that will be used in READ_WRITE.CHR_SHEET.POINTER after aux memory is enabled)
		STA $C009 ;enable aux zero-page & aux BSR 
				
		;connect character sheet pointer
		LDA #CHR_SHEET.PC.AUX_MEMORY.START
		CLC
		ADC RESULT+$0 ;PC character sheet record index, LO byte
		STA CHR_SHEET.POINTER+$0

		LDA /CHR_SHEET.PC.AUX_MEMORY.START
		ADC RESULT+$1 ;16-bit add. PC character sheet record index, LO byte
		STA CHR_SHEET.POINTER+$1
		
		STA $C008 ;enable main zero-page & main BSR 
		
		;set stop value
		LDA #CHR_SHEET.PC.RECORD_SIZE
		STA CHR_SHEET.RECORD.STOP_LENGTH
		
		LDY #$00 ;set field index
	JSR READ_WRITE.CHR_SHEET.POINTER

@END
	
	;**FALLS THROUGH**

.EXIT
;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX
	
	RTS
@END

;SKILLS
@START

SKILL.PROGRESS.UPDATE
@START
;PARAMETERS: ACC = char sheet field offset (see offloaded_variables.ASM for constants for each character sheet field, use the .INDEX version), Y-REG = sindex of S_ENTITY whose characters sheet needs updating
;ENTRANCE: direct
;RETURN: updated skill (character sheet saved to aux memory)

;save parameters
	STA TEMP ;parm: char sheet field offset
	
;SAVE REGISTERS
	TXA
	PHA

.READ.CHAR_SHEET
		;Y-REG = COMBAT.ATTACKER.SINDEX or COMBAT.DEFENDER.SINDEX
		LDA #$00 ;set read mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet

	
	LDX TEMP ;parm: char sheet field offset	
	INC CHR_SHEET.RECORD.READ,X
	;overflow check
	LDA CHR_SHEET.RECORD.READ,X
	BNE .INC_DONE	
	LDA #$FF
	STA CHR_SHEET.RECORD.READ,X
.INC_DONE	

.WRITE.CHAR_SHEET	
;(this write is need to save the increment to the PCs skills)
		;Y-REG = COMBAT.ATTACKER.SINDEX or COMBAT.DEFENDER.SINDEX
		LDA #$01 ;set write mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;write character sheet

;RESTORE REGISTERS
	PLA
	TAX
	
	RTS
@END

@END

@END

@END

;ADD_REMOVE.INVENTORY.ITEM ;***NOT YET WRITTEN, CODE IS A STARTING POINT ONLY**
@START
;*******NOTES ON CREATING THIS ROUTINE*****
	; *note: I had conidered that when records are added to the inventory table that the
	; level field wouldn't be populated so the inventory file I/O routines could stay local to the 
	; inventory module. However, the treasure and merchant routines will need access
	; to the item names so therefore the inventory file access routines will need to be in main game engine memroy anyway
;
;
;**WARNING: the one/two handed weapon status is stored in the "hands required & Shape_ID" field in the item tables but is stored in the equipment slot field (bit6 I think, check datagram)
;in the player inventory table. This is imporant because the add item routine will need to set the equipment slot field value in the inventory table using data from the item table.
;
;**WARNING: player inv record byte $0 (item level | item type) is nibble based, and since item level in the item tables is stored as 1-!16, make sure
;to write it to the player inv record as level=level-1 so the value range will fit into a nibble ($0-$F). This won't cause a problem because the level sub-field is only used for sorting the table. 
;
;
;Only being able to write 2 pages at a time is key design consideration.
;The two routines that for sure need to write inventory data are merchant transactions
;and treasury management (presumably to be added to COMBAT.EXIT), a 3rd possibility
;would be opening chests from the undermap and building maps.
;
;The simplest solution to code would be to read the entire inventory data 
;into memory ($600), modify it, then write the entire $600 bytes to disk. 
;Treasury mgt for sure I'd think would have enough memory to support this locally. Merchant
;transactions is less certain. For opening boxes on the map, the only way to support that would
;be for open box to trigger a disk load (i.e open box is a module, maybe connceted into the end-of-combat treasure
;mgr routines). That might be necessary no matter what considering that there might not be enough
;main memory to parse the map object flag field to determine box contents and it's really
;doubtful there would be two pages of main memory to spare for a buffer to only write part of the file,
;which is the only alternative to writing $600 bytes...it's either write $600 or write $200, there is 
;no scenario where less than $200 can be written.
;
;As to a $200 byte buffer, the shape hopper could be used but that is only $100, and 
;it's a .EQ to the memory address $200 (kb buffer) so it's not practical to locate one of
;of the shared memory blocks adjacent to it even if the shared memory blocks were usable, which
;they certainly wouldn't be inside a building. 
;
;	
;GOLD. It might be easiest to make gold an in-game variable that is only written to disk
;when the game is saved. i.e. don't bother writing it to the working inventory file.	
;maybe gold gets saved along with the game state data too so it's not an odd 2 byte
;sitting between the regular inventory data and something else. 	
;
;
;***WRITING THE ROUTINE****
;
;Review the inventory datagram. There is an open record value (level field high bit)
;and a table stop value (readied field, high-bit). The example code below from the create special
;routine only uses a stop value. 
;
;Also consider how/where to incorporate a routine to optmize the inventory table, sort by level
;so the open records filter to the bottom, then reset the stop value.
;Actually as I think about it, the play was to write the table length to the first two bytes, 
;which would be needed for sorting anyway. Then use the length instead of
;the stop value. but then it doesn't fit in 6 page. Aaarrg!!
;
;Buckle it. the main benifit of the optimization was in a scenario
;where the routine did small reads until a record was found, but that
;doesn't matter since the write minimum is $200.
;So, we don't really need a stop value, just an open record value,
;and that is better off bein the readied flag field than the level field for 
;future nibble splitting considerations.

	
; ;RETURN VALUE: ACC = ($00 = special record added | $01 special character sheet array full)

; ;SAVE REGISTERS
	; TXA
	; PHA
	; TYA
	; PHA

				; ; JSR TEST.GET.SPECIAL.DATA
				; ; LDA #$AA
				; ; JSR PREP.BRK
				; ; BRK
				
; .INIT.MEMORY.STATE
; @START
	; ;ENABLE BANK-SWITCHED RAM ($D000 Bank 1)
	; LDA $C08B 
	; LDA $C08B
	
	; LDX SPECIAL.DATA.POINTER+$0
	; LDY SPECIAL.DATA.POINTER+$1
	

				
		; ;TSX			;transfer stack pointer to X-REG
		; ;STX TEMP	;save stack pointer
	; STA $C009 ;enable aux zero-page & aux BSR 

	; STX SPECIAL.DATA.POINTER+$0
	; STY SPECIAL.DATA.POINTER+$1

		

; .FIND.NEXT.OPEN.RECORD
; @START
	; LDY #$00 ;init array index
	; LDX #$00 ;init CHR_SHEET_EXTENDED.SPECIAL.HP_MAX index (2 byte records)
		
	; LDA #CHR_SHEET.SPECIAL.AUX_MEMORY.START
	; STA CHR_SHEET.POINTER+$0
	
	; LDA /CHR_SHEET.SPECIAL.AUX_MEMORY.START
	; STA CHR_SHEET.POINTER+$1
	
; .FIND.RECORD.LOOP
	; LDA (CHR_SHEET.POINTER),Y ;read 1st field in record of SPECIAL character sheet
	; CMP #CHR_SHEET.SPECIAL.STOP_VALUE
	; BEQ .FIND.RECORD.LOOP.DONE
	
	; ;increment index: next record
	; INX ;increment CHR_SHEET_EXTENDED.SPECIAL.HP_MAX index (2 byte records)
	; INX ;""
	
	; LDA CHR_SHEET.POINTER+$0 ;increment array pointer
	; CLC
	; ADC #CHR_SHEET.SPECIAL.RECORD_SIZE
	; STA CHR_SHEET.POINTER+$0	
	; BNE .FIND.RECORD.LOOP

	; INC CHR_SHEET.POINTER+$1

	; ;END OF ARRAY TEST
	; ;if pointer HO byte is <= the end of the special aux memory block
	; ;then continue loop
	; LDA CHR_SHEET.POINTER+$1	
	; CMP /CHR_SHEET.SPECIAL.AUX_MEMORY.END
	; BCC .FIND.RECORD.LOOP
	; BEQ .FIND.RECORD.LOOP				
	; JMP .SPECIAL_ARRAY.FULL
	
; .FIND.RECORD.LOOP.DONE
; @END

; .CREATE.NEW.SPECIAL.RECORD	
; @START
	; ;copy SPECIAL data from stats table to special character sheet
	
	; ;LDY #$00
; .INIT.SPECIAL.CHR_SHEET.LOOP
	; LDA (SPECIAL.DATA.POINTER),Y ;read SPECIAL character sheet data from stats table	
	; STA (CHR_SHEET.POINTER),Y ;write to next open record in SPECIAL character sheet
	; INY
	; CPY #CHR_SHEET.SPECIAL.RECORD_SIZE
	; BNE .INIT.SPECIAL.CHR_SHEET.LOOP
	
	; ;set Max HP LO/HO of SPECIAL
	; LDY #CHR_SHEET.PC_MOB.HP_LO.OFFSET
	; LDA (SPECIAL.DATA.POINTER),Y ;read SPECIAL character sheet data from stats table	
	; STA CHR_SHEET_EXTENDED.SPECIAL.HP_MAX.AUX+$0,X
	; INY ;#CHR_SHEET.PC_MOB.HP_HO.OFFSET
	; LDA (SPECIAL.DATA.POINTER),Y ;read SPECIAL character sheet data from stats table	
	; STA CHR_SHEET_EXTENDED.SPECIAL.HP_MAX.AUX+$1,X


	
	; ; ;move stop value to start of next record
	; ; LDA #CHR_SHEET.SPECIAL.STOP_VALUE
	; ; STA (CHR_SHEET.POINTER),Y
; @END

; .SPECIAL_RECORD.ADDED
	; LDA #$00 ;set return value ($00 = special record added | $01 special character sheet array full)
	; STA TEMP
	; JMP .EXIT

; .SPECIAL_ARRAY.FULL
	; LDA #$01 ;set return value ($00 = special record added | $01 special character sheet array full)
	; STA TEMP

	; ;**FALLS THROUGH**
	
; .EXIT
		
; ;RESTORE MEMORY STATE
	; STA $C008 ;enable main zero-page & main BSR 
		; ; LDX TEMP	;restore stack pointer to X-REG
		; ; TXS ;transfer X-REG to stack pointer

	; ;RE-ENABLE BANK-SWITCHED RAM ($D000 Bank 2)
	; LDA $C083				;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK2 2nd)
	; LDA $C083

	

	
; ;RESTORE REGISTERS
	; PLA
	; TAY
	; PLA
	; TAX

	; LDA TEMP ;load return value ($00 = special record added | $01 special character sheet array full)
	

			
	; RTS
	
; ;LOCAL VARIABLES
	; ;CHR_SHEET.SAVED.YREG.INDEX .BS $1
	
	
; ;TEST.GET.SPECIAL.DATA ;copy SPECIAL char sheet from aux to main memory
; @START
			
; ; ;SPECIAL

	; ; ;ENABLE BANK-SWITCHED RAM ($D000 Bank 1)
	; ; LDA $C08B 
	; ; LDA $C08B
						
		; ; TSX			;transfer stack pointer to X-REG
		; ; STX TEMP	;save stack pointer
	; ; STA $C009 ;enable aux zero-page & aux BSR 



	; ; ; LDA #CHR_SHEET.SPECIAL.STOP_VALUE
	; ; ; STA CHR_SHEET.SPECIAL.AUX_MEMORY.START
	
			; ; LDA #CHR_SHEET.SPECIAL.AUX_MEMORY.START
			; ; STA COPY.FROM_START
			; ; LDA /CHR_SHEET.SPECIAL.AUX_MEMORY.START
			; ; STA COPY.FROM_START+$1
			; ; LDA #CHR_SHEET.SPECIAL.AUX_MEMORY.END
			; ; STA COPY.FROM_END
			; ; LDA /CHR_SHEET.SPECIAL.AUX_MEMORY.END
			; ; STA COPY.FROM_END+$1
				
			; ; LDA #$00
			; ; STA COPY.TO
			; ; LDA #$BD
			; ; STA COPY.TO+$1
		
		; ; JSR MEMORY.COPY
				
	
; ; ;RESTORE MEMORY STATE
	; ; STA $C008 ;enable main zero-page & main BSR 
		; ; LDX TEMP	;restore stack pointer to X-REG
		; ; TXS ;transfer X-REG to stack pointer

	; ; ;RE-ENABLE BANK-SWITCHED RAM ($D000 Bank 2)
	; ; LDA $C083				;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK2 2nd)
	; ; LDA $C083

	; ; RTS
	
; @END


@END


;====FILE I/O===

;INVENTORY FILE
@START
FILE.OPEN.INVENTORY
@START
;PARAMETERS: PARM.SEEK_BYTES
;RETURN: FILE.ITEM_TABLE.RECORD.READ
;ENTRANCE: FILE.READ.ITEM_TABLES.ENTRANCE

		lda #DATA.GME.INVENTORY	;load LO address
		sta parm.namlo
		lda /DATA.GME.INVENTORY	;load HO address
		sta parm.namhi
	JSR PRODOS.IO.OPEN_FILE

	
.EXIT
	INC FILE.OPEN.INVENTORY.DEBUG_COUNTER ;tracks the number of times the file is opened, which we want to minimize to avoid long disk load times. 
	
	RTS

@END

;**OPT** Memory. It may be possible to move this routine to SWAP.ROUTINES.INV.entrance_exit.asm. 
;I think I put it here because it is called from SWAP.ROUTINES.INV.readied_equipment.asm perhaps thining
;the entrance sub_module wouldn't be loaded in memory if the launch occured from combat. 
;However, it is only stats summary (INV_0) that is shared with the combat module. The entrance sub_module 
;would be needed to load the readied equipment screen from the stats summary screen. 

FILE.READ.ITEM_TABLES.ENTRANCE ;master item tables
@START
;PARAMETERS: ACC = item_type ($00 = weapon | $01 armor | $02 misc item), FILE.ITEM_TABLE.ID
;ENTRANCE: ****FILE.OPEN.INVENTORY must be called first
;RETURN: FILE.ITEM_TABLE.RECORD.READ($20)

;=====================SUBROUTINE DOCUMENTATION====================================
;
;This routine uses its paramaters to identify the specific item table
;the read is for, the record requested, and then returns the data for that record. 
;
;The records are read using the sequence file_offset_reset/seek/read.
;
;The file_offset_reset (/aka seek reset) allows the seek to start from the beginning
;of the file, which greatly simplies the calculation of the number of seek bytes.
;
;The seek calculations are done using the appropriate ";ITEM FILE OFFSETS" constants at the end
;of the DATA.GAME.INVENTORY.ASM file. (seeks to the start of the desired table) + the offset to the specific record 
;of the table to read.
;
;The reason the file offset reset method is used instead of opening the file every time is because opening
;files is very slow due on floppy due to ProRWTS needing to search for the location of the file on the floppy disk. 
;
;
;=================================================================================
	


; ;*****DRIVER******
	; ;read item record from master item file
		; ;(get parameters from the inventory record of current item)
		; LDY #$01 ;set index to byte $1 (item_ID) of inventory record
		; LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y
		; STA FILE.ITEM_TABLE.ID
		; DEY ;backup to byte $0 (LEVEL | ITEM_TYPE) of inventory record
		; LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y
		; ;ITEM_TYPE is in LO nibble
		; AND #$0F ;mask-out HO nibble. 
			; ;ACC = item table code ($00 = weapon | $01 armor | $02 misc item)
	; JSR FILE.READ.ITEM_TABLES.ENTRANCE
		; ;RETURN: FILE.ITEM_TABLE.RECORD.READ($20)	

		
;SAVE PARAMETERS

	;ACC = item table code ($00 = weapon | $01 armor | $02 misc item)
	STA FILE.ITEM_TABLE.TYPE_CODE
	

.IS.FILE.OPEN
		LDA #DATA.GME.INVENTORY
		STA FILENAME.CHECK_LO
		LDA /DATA.GME.INVENTORY
		STA FILENAME.CHECK_HI
	JSR PRODOS.IO.CHECK_FILE_STATUS
		;RETURN VALUE: ACC ($00 = file open | $01 = file closed)
	CMP #$00 ;is file open?
	BEQ .FILE_OPEN ;branch if yes
	
	;**FALLS THROUGH**
	
.FILE_NOT_OPEN	
	JSR FILE.OPEN.INVENTORY

	;**FALLS THROUGH**
	
.FILE_OPEN	

	
	;**FALLS THROUGH**

			
.SET.READ.PARAMETERS
@START
	
.ADD.TABLE_OFFSET		;**OPT** Memory. Convert this branch-case section to load the offset values from a lookup table built from .DA directives
	LDA FILE.ITEM_TABLE.TYPE_CODE
	BEQ .ADD.WEAPON_TABLE.OFFSET
	CMP #$01
	BEQ .ADD.ARMOR_TABLE.OFFSET
	;default case: #$02 (item table)
	
	;**FALLS THROUGH**
	
.ADD.MISC_ITEM_TABLE.OFFSET	;(number of bytes from the start of the file to the start of the table specified as a parameter by the calling routine)
	LDA #INV.MISC_ITEM_TABLE.OFFSET
	STA PARM.SEEK_BYTES+$0

	LDA /INV.MISC_ITEM_TABLE.OFFSET
	STA PARM.SEEK_BYTES+$1	
	JMP .ADD.ITEM_ID.OFFSET
	
.ADD.WEAPON_TABLE.OFFSET ;(number of bytes from the start of the file to the start of the table specified as a parameter by the calling routine)
	LDA #INV.WEAPON_TABLE.OFFSET
	STA PARM.SEEK_BYTES+$0

	LDA /INV.WEAPON_TABLE.OFFSET
	STA PARM.SEEK_BYTES+$1
	

			
	JMP .ADD.ITEM_ID.OFFSET

.ADD.ARMOR_TABLE.OFFSET ;(number of bytes from the start of the file to the start of the table specified as a parameter by the calling routine)
	LDA #INV.ARMOR_TABLE.OFFSET
	STA PARM.SEEK_BYTES+$0

	LDA /INV.ARMOR_TABLE.OFFSET
	STA PARM.SEEK_BYTES+$1

			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; LDA #$CB
			; LDX INV.ITEM_TABLE.WP.MAGIC_FLAG5
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP

			
	JMP .ADD.ITEM_ID.OFFSET
	
	;**FALLS THROUGH**

.ADD.ITEM_ID.OFFSET ;(number of bytes from the start of the table to the start of the record associated with the item_id)
	LDA FILE.ITEM_TABLE.ID
	BEQ .EXECUTE.FILE_READ ;if ITEM_ID is $00 then no offset from start of table is neede because the table starts with the record for item_id $00
	
		;ACC = FILE.ITEM_TABLE.ID
		STA MULPLR+$0
		LDA #$00
		STA MULPLR+$1
		;
		LDA #FILE.ITEM_TABLE.RECORD.SIZE
		STA MULCND+$0
		LDA /FILE.ITEM_TABLE.RECORD.SIZE
		STA MULCND+$01			
	JSR MLP.16.NO_BCD
		;RETURN: RESULT(2) = ITEM_ID OFFSET 

	;**FALLS THROUGH**

			
.EXECUTE.ADD			
	LDA RESULT+$0
	CLC
	ADC PARM.SEEK_BYTES+$0
	STA PARM.SEEK_BYTES+$0
	LDA RESULT+$1
	ADC PARM.SEEK_BYTES+$1 ;16-bit add
	STA PARM.SEEK_BYTES+$1

.EXECUTE.FILE_READ					
		;set PARM.READ_BYTES
		lda #FILE.ITEM_TABLE.RECORD.SIZE		;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
		STA PARM.READ_BYTES+$0
		
		lda /FILE.ITEM_TABLE.RECORD.SIZE	;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
		STA PARM.READ_BYTES+$1				

		;set destination memory address
		lda #FILE.ITEM_TABLE.RECORD.READ
		sta parm.ldrlo
		lda /FILE.ITEM_TABLE.RECORD.READ
		sta parm.ldrhi	
	JSR PRODOS.IO.READ_CURRENT_FILE


	
	
				; LDA #$BC
				; ;LDX #$BB
				; ;LDX FILE.OPEN.INVENTORY.DEBUG_COUNTER
				; ;LDY #$BB
				; ldx parm.namlo
				; ldy parm.namhi
				; JSR PREP.BRK
				; BRK
				

			
.EXIT


	
	RTS
	
@END
@END


@END


;==============MAP GENERAL TOOLS=========
@START

;****WARNING*** must update RZONE data after MAP.UPDATE.POSITION is used 
MAP.UPDATE.POSITION ;updates RMAP, RMAP.X/Y, PLAYER.WMAP.ZONE and it calls  ;CALCUALTE SS MAP REGIONAL FLAGS BASED ON RMAP.X/Y. Must update RZONE data if this routine is used 
@START
;PARMETERS: GMAP.X, GMAP.Y, PARM.GMAP.X, PARM.GMAP.Y (it's more efficient for these to be set by the calling routine, see template below)
;ENTRANCE: direct
;RETURN: PLAYER.WMAP.ZONE, RMAP.X/Y, RMAP(2), MAP_OBJECTS.SS.X_FLAG.UPPER, MAP_OBJECTS.SS.Y_FLAG.UPPER, MAP_OBJECTS.SS.X_FLAG.LOWER, MAP_OBJECTS.SS.Y_FLAG.LOWER

	JSR CONVERT.GMAP_XY.WZONE
		LDA RETURN.WZONE
		STA PLAYER.WMAP.ZONE
		
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
	
	

			
;INIT SS MAP REGIONAL FLAGS (KEEP EVEN AFTER LOAD/SAVE GAME IS IMPLEMENTED)
	JSR MAP.CALCULATE.SS_FLAGS

	RTS
@END

;CALCULATE DISTANCE BETWEEN X,Y COORDINATES
;see .CALCULATE.DISTANCE (NPC.PATHFINDER)
;(designed for RMAP/GMAP but may work for player relative X,Y too)


CALCULATE.RISE_RUN
;PARAMETERS: PARM1.SHAPE.SBYTE, PARM1.SHAPE.LINE, PARM2.SHAPE.SBYTE, PARM2.SHAPE.LINE
;RETURN: ACC (DISTANCE), RETURN.SHAPE.RISE, RETURN.SHAPE.RUN 

CALCULATE.DISTANCE ;Calculate distance between two GMAP.X/Y or GMAP.X/Y positions
@START
;PARAMETERS: PARM1.GMAP.X, PARM1.GMAP.Y, PARM2.GMAP.X, PARM2.GMAP.Y
;RETURN: ACC (DISTANCE IN TILE MOVES, IGNORING OBSTACLES), MOB.POSITION.X_LT*, MOB.POSITION.X_GR*, MOB.POSITION.Y_LT*, MOB.POSITION.Y_GR*
;ENTRANCE: DIRECT
;
;*X_LT/X_GR = $00, then X-axis is equal. If either are set to $01 then true (i.e. if X_LT = $01, then X-AXIS is less than. if X_GR = $01, then X-AXIS is greater than) 
;*Y_LT/Y_GR = $00, then Y-axis is equal. If either are set to $01 then true (i.e. if Y_LT = $01, then Y-AXIS is less than) 



;INIT
	LDA #$00
	STA MOB.POSITION.X_LT
	STA MOB.POSITION.X_GR
	STA MOB.POSITION.Y_LT
	STA MOB.POSITION.Y_GR
	
;CALCULATE DISTANCE BETWEEN PARM1 AND PARM2
	LDA PARM1.GMAP.X
	CMP PARM2.GMAP.X
	BCC .MOB.MO_X_LESS
	
	SEC
	SBC PARM2.GMAP.X
	STA MAP_OBJECTS.X_ADJ
	;BEQ .MOB.MO_X.EQUAL
	; LDA #$00	;**OPT** Memory. Could be eliminated if the $00 was false for this flag. Then just save whatever value 
				; ;is in the ACC to set to true. But do it using the LDA before the addition/subtraction in case the
				; ;the result is $00. But keep in mind that these flags are used in COMBAT.MOVE.MANAGER to identify adjacent tiles for mob attacks				
	STA MOB.POSITION.X_GR ;set to true	
	JMP .MOB.YTEST

.MOB.MO_X_LESS
	LDA PARM2.GMAP.X
	SEC
	SBC PARM1.GMAP.X
	STA MAP_OBJECTS.X_ADJ
	; LDA #$00	;**OPT** Memory. Could be eliminated if the $00 was false for this flag. Then just save whatever value 
				; ;is in the ACC to set to true. But do it using the LDA before the addition/subtraction in case the
				; ;the result is $00. But keep in mind that these flags are used in COMBAT.MOVE.MANAGER to identify adjacent tiles for mob attacks				
	STA MOB.POSITION.X_LT ;set to true

	;**FALLS THROUGH**
	
.MOB.MO_X.EQUAL
	;**FALLS THROUGH**			
.MOB.YTEST	
	LDA PARM1.GMAP.Y
	CMP PARM2.GMAP.Y
	BCC .MOB.MO_Y_LESS
	
	SEC
	SBC PARM2.GMAP.Y
	STA MAP_OBJECTS.Y_ADJ
	;BEQ .MOB.MO_Y.EQUAL
	; LDA #$00	;**OPT** Memory. Could be eliminated if the $00 was false for this flag. Then just save whatever value 
				; ;is in the ACC to set to true. But do it using the LDA before the addition/subtraction in case the
				; ;the result is $00. But keep in mind that these flags are used in COMBAT.MOVE.MANAGER to identify adjacent tiles for mob attacks								
	STA MOB.POSITION.Y_GR	;set to true	
	JMP .FINAL.DISTANCE.CALC

.MOB.MO_Y_LESS
	LDA PARM2.GMAP.Y
	SEC
	SBC PARM1.GMAP.Y	
	STA MAP_OBJECTS.Y_ADJ
	; LDA #$00	;**OPT** Memory. Could be eliminated if the $00 was false for this flag. Then just save whatever value 
				; ;is in the ACC to set to true. But do it using the LDA before the addition/subtraction in case the
				; ;the result is $00. But keep in mind that these flags are used in COMBAT.MOVE.MANAGER to identify adjacent tiles for mob attacks								
	STA MOB.POSITION.Y_LT ;set to true
	;**FALLS THROUGH**

.MOB.MO_Y.EQUAL
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

@END

;===============MAP CONVERSION TOOLS========
@START

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
;(9/13/2016; using this now for player start position on surface and it seems to work)


	;is gmap.x/y in region 0?
	;(region 0 are the 6 zones in the upper left corner of the map. GMAP.X/Y and RMAP.X/Y are the same in that region because it contains the 0,0 intersection of the X and Y GMAP axis.)
	LDA PARM.GMAP.X
	CMP #WZONE.WIDTH*3 ;even though WZONE.WIDTH is a constant, don't use # because it's an expression
	BCS .NOT_IN_REGION0
	STA RETURN.RMAP.X ;this value will get overwritten if .NOT_IN_REGION0 is called. Doing this STA here, rather than below after the next CMP to save memory.
	LDA PARM.GMAP.Y 	;set RMAP.X = GMAP.X
	CMP #WZONE.WIDTH*3
	BCS .NOT_IN_REGION0			
	STA RETURN.RMAP.Y ;set RMAP.Y = GMAP.Y
	JMP .EXIT ;RMAP.X/Y has been set equal to GMAP.X/Y, we're done. Exit. 
	
.NOT_IN_REGION0	
	LDA PARM.GMAP.X
	STA DIVIDEND

	LDA #$00
	STA DIVIDEND+$1
	LDA	#WZONE.WIDTH
	STA DIVISOR
	LDA #$00
	STA DIVISOR+$1
	;STA DIVIDEND32
	;STA DIVIDEND32+$1
		
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

.EXIT
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
;	Remainder from division is discarded, only use quotient. 
	
	LDA PARM.GMAP.Y
		;LDA #$30
	STA DIVIDEND
	LDA #$00
	STA DIVIDEND+$1
	LDA	#WZONE.WIDTH
		;LDA #$10
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
	;STA DIVIDEND32
	;STA DIVIDEND32+$1
		
	JSR DIV.16						;**OPT** Speed. Replace with in-line code
	
	LDA RESULT						;QUOTIENT
	CLC
	ADC TEMPY
	STA RETURN.WZONE ;**OPT**; return value could be via ACC.


			
	RTS
	

@END

CONVERT.COLUMN_ROW.TO.SINDEX ;**OPT** Memory. Is this duplicate to CONVERT.COLUMN_ROW.TO.SCREEN_ARRAY_INDEX?
@START
;PARAMETERS: PARM.GMAP.X, PARM.GMAP.Y
;RETURN: Y-REG (screen array index of tile location associated with GMAP X.Y)
;ENTRANCE: DIRECT

	JSR CONVERT.GMAP.TO.PLAYER_RELATIVE.XY
		LDA RETURN.RELATIVE.X
		STA PARM.RELATIVE.X
		LDA RETURN.RELATIVE.Y
		STA PARM.RELATIVE.Y	
	JSR CONVERT.PLAYER_RELATIVE.XY.TO.SCREEN_ARRAY_INDEX
		LDY RETURN.SCREEN_ARRAY_INDEX ;used below

		RTS
	
@END

;**OPT** Memory. All the conversion routines which return a screen index via the variable RETURN.SCREEN_ARRAY_INDEX can probably be changed to return the value via the ACC. 
	
; also see .CONVERT.GMAP.TO.PLAYER_RELATIVE.XY (MAP_OBJECTS.MANAGEMENT.ASM and ANIMATION_MANAGER.ASM), WHICH ARE SETUP AS IN-LINE CODE FOR SPEED
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

CONVERT.COLUMN_ROW.TO.SCREEN_ARRAY_INDEX ;The screen array index is also /aka the screen tile location, used for drawing graphics. 
@START
;PARAMETERS: PARM.COLUMN, PARM.ROW
;RETURN: ACC: screen index
;ENTRANCE: DIRECT

;FORMULA: PARM.ROW * tiles per row + PARM.COLUMN

	LDA PARM.ROW
	STA MULPLR
	LDA #$00
	STA MULPLR+$1
	;
	LDA #SCREEN.ARRAY.OFFSET ;number of tiles in each row
	STA MULCND
	LDA #$00
	STA MULCND+$01
	
	JSR MLP.16.NO_BCD
	
	LDA RESULT
	CLC
	ADC PARM.COLUMN		;add the column # to (PARM.ROW * tiles per row) as per formula described above.  
	STA RETURN.SCREEN_ARRAY_INDEX ;save return value

	;return value = ACC: screen index
	
	RTS
	
@END
	
;CONVERT SCREEN ARRAY INDEX TO COLUMN,ROW 
;(this is already done via a lookup table, no subroutine needed. See SCREEN TABLES in the variables file)
;See SCREEN.INDEX.COLUMN and SCREEN.INDEX.ROW
@END


;===============TEXT ROUTINES=======




