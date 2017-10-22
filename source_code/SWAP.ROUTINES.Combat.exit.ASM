; ============================================
; Copyright (C) 2016-2018. 6502 Workshop, LLC
; contact: mark@6502workshop.com
; ============================================



;************************INCLUDE FILE*****************************
;(do not assemble stand-alone.  )
				.TF     swap.routines.combat.exit.bin,BIN
				;.OR		SRTN.COMBAT.ADDRESS			**Always put before .TF directive and never use again in program
				.OR		SWAP_SPACE.MAIN_MEMORY			**Always put before .TF directive and never use again in program
				.EB		OFF

;=====================SETUP/EXIT ROUTINES DOCUMENTATION====================================
;
;This file is loaded just before and just after COMBAT.MAIN is called. This enabled 
;certain routines to be offloaded from the main combat module into this file, which frees
;up memory in the main combat module. 
;
;
;=================================================================================

COMBAT.EXIT.START


COMBAT.EXIT
@START

				

;**OPT** Disk. This routine is a bit fat because memory wasn't a constraint. But if disk becomes a constraint, there
;are some gains that can be made in here, such as passing the parms to CHECK.SKILL.LEVELUP via registers insead of variables. 

;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA

COMBAT.SESSION_CLEANUP
@START
	
.SYNC.MAP_OBJECTS.WITH.CHAR_SHEET
@START
;(copies health status field on the PC map object array to character sheet so that if a character died
;in battle (for example) that result is proprated into gameplay after the combat module is exited)

	LDX #$01 ;init loop counter and player #
.LOOP.INIT.MAP_OBJECTS.PC
			
	;read PC character sheet data
		TXA
		;ACC = player sequential # (high-bit not set = read mode)
	JSR COMBAT.READ_WRITE.CHR_SHEET.PC
		;RETURN VALUE = CHR_SHEET.RECORD.READ
	
	;copy health status code from character sheet to map object array

	;calculate map object array index
	DEX ;subtract 1 from player # (which starts with 1) to align with map object array index (which starts with 0)
	TXA
	ASL ;X2
	ASL ;X4
	TAY ;index calculated, transfer to Y-REG
	INX ;add 1 to player # to offset subtract (via DEX) above
	
	LDA COMBAT.MAP_OBJECTS.PC+$3,Y
	STA CHR_SHEET.PC.HEALTH_STATUS	
	
	;exit test
	CPX PARTY.TOTAL.PC
	BEQ .LOOP.INIT.MAP_OBJECTS.PC.DONE
	INX	
	JMP .LOOP.INIT.MAP_OBJECTS.PC
.LOOP.INIT.MAP_OBJECTS.PC.DONE


@END
			
			
.SAVE.PLAYER.CHR_SHEET
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

	lda #cmd_write.drive2
	sta parm.reqcmd

;set write data size (# of 512 byte blocks to write from memory)
	lda #DATA.PLY.CHR_SHEET.FILE_SIZE ;always #$00
	sta parm.sizelo

	lda /DATA.PLY.CHR_SHEET.FILE_SIZE ;number of pages to write (must be in multiples of 2. i.e. 2, 4, 6, 8 $0A, etc)
	sta parm.sizehi
	
;set source memory address for write
	lda #CHR_SHEET.PC.AUX_MEMORY.START
	sta parm.ldrlo
	lda /CHR_SHEET.PC.AUX_MEMORY.START
	sta parm.ldrhi
	
;set filename to write to	
	lda #DATA.PLY.CHR_SHEET ;load LO address
	sta parm.namlo	
	lda /DATA.PLY.CHR_SHEET ;load HO address
	sta parm.namhi	
		
		LDA #$01	;PARM: $00=main, $01=aux
	JSR PRODOS.IO


@END

	
.RESTORE.GAME_VARIABLES ;**OPT** Memory. This can't be combined with the save position variables routine in location.maps.enter (movement.mgr.ASM) because the player varaibles saved when entering a building need to be restored on exit, and combat could happen in a building. but, if there are other instances where save/restore game location variables comes up then maybe those routines and this one could be combined and accessed via JSR. 
@START


	LDX #$00
.RESTORE.LOOP
;this loop does the equivilent of the following: 
	; LDA COMBAT.GMAP.X.SAVED
	; STA GMAP.X
	;
	; LDA COMBAT.GMAP.Y.SAVED
	; STA GMAP.Y
	;		
	; LDA COMBAT.GMAP.X.LAST.SAVED
	; STA GMAP.X.LAST
	;
	; LDA COMBAT.GMAP.Y.LAST.SAVED
	; STA GMAP.Y.LAST
	;
	; LDA COMBAT.RMAP.X.SAVED
	; STA RMAP.X
	;
	; LDA COMBAT.RMAP.Y.SAVED
	; STA RMAP.Y
	;
	; LDA COMBAT.RMAP.SAVED+$0
	; STA RMAP+$0
	; LDA COMBAT.RMAP.SAVED+$1
	; STA RMAP+$1
	;
	; LDA COMBAT.WZONE.SAVED
	; STA PLAYER.WMAP.ZONE
	
	LDA COMBAT.GMAP.X.SAVED,X
	STA GMAP.X,X
	INX
	CPX #$9
	BNE .RESTORE.LOOP


;RESTORE MISC GAME STATE & MAP VARIABLES
	LDA COMBAT.TIME.SUN.STATUS.SAVED
	STA TIME.SUN.STATUS
	
	JSR MAP.CALCULATE.SS_FLAGS

				
	;restore player's location type before entering combat
	LDA COMBAT.PLAYER.MAP.LOCATION_TYPE.SAVED
	STA PLAYER.MAP.LOCATION_TYPE

@END

;**OPT** Memory. This section can probably be moved to COMBAT.EXIT
		
;RESET SCREEN
		; LDA #$03 ;clear both pages
	; JSR CLEAR.TEXT_WINDOW.RIGHT
	
	;restore verticle blue text window border where horizontal scroll window top border line integrated
		LDY #$18
		LDX #TWB.COMBAT.SCROLL_WINDOW.TOP_LINE
		LDA #$84		
	JSR DRAW.BYTE

		LDY #$27
		LDX #TWB.COMBAT.SCROLL_WINDOW.TOP_LINE
		LDA #$A0		
	JSR DRAW.BYTE		
	



@END

	

	; LDA #$AA
	; LDX #$AA
	; LDY #$AA
	; JSR PREP.BRK
	; BRK

COMBAT.CLEANUP
@START
;=====================SUBROUTINE DOCUMENTATION====================================
;
;The AUX BSR routines are clobbered by weapon projectile shape data during combat setup.
;This subroutines restores them. 
;
;The data is read from the file into a buffer first. 
;This is needed because the AUX BSR routines need to be loaded to bank2
;and since the ProRWTS controller is in bank1, ProRWTS can't read data directly into AUR BSR/bank2)
;
;
;=================================================================================

.RESTORE.AUX_BSR.ROUTINES
@START

;OPEN FILE "BS_AUX.RTN.BK2"
@START
.OPEN.FILE2
	
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
	LDA #$00  	;$01
	sta parm.sizelo	
	LDA #$01	;$00
	sta parm.sizehi
	
;set destination memory address
	lda #SHAPE.HOPPER0
	sta parm.ldrlo
	lda /SHAPE.HOPPER0
	sta parm.ldrhi

;set filename to read from	
	lda #BS_AUX.ROUTINES.BK2	;load LO address
	sta parm.namlo
	lda /BS_AUX.ROUTINES.BK2	;load HO address
	sta parm.namhi
					
		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO
@END


	;**FALLS THROUGH**
	
					
.INIT.LOOP.VARIABLES
@START
	;init aux zero page variables 
	;(note: for the memory copy. when aux BSR is enabled, aux zero page is also enabled)
	STA $C009 ;enable aux zero-page & aux BSR 

	LDA #BS_AUX_ROUTINES.BANK2.COPY_TO.START
	STA COPY.TO
	LDA /BS_AUX_ROUTINES.BANK2.COPY_TO.START
	STA COPY.TO+$1

	STA $C008 ;enable main zero-page & main BSR 
	
	LDA /BS_AUX_ROUTINES.BANK2.FILE_SIZE	
	TAY ;set loop counter (only the HO byte is needed for the counter because the weapons shape tables always end on a page boundry, they are $400bytes each)
@END
	

.RESTORE.LOOP
@START
.COPY.CHUNK.TO.AUX_BSR	
	;enable AUX memory (BSR: bank2 is already enabled)						
		TSX			;transfer stack pointer to X-REG
		STX STACK_POINTER.SAVED	;save stack pointer
	STA $C009 ;enable aux zero-page & aux BSR 

	;execute copy	
		LDA #SHAPE.HOPPER0
		STA COPY.FROM_START+$0
		LDA /SHAPE.HOPPER0
		STA COPY.FROM_START+$1

		LDA #SHAPE.HOPPER.END
		STA COPY.FROM_END+$0
		LDA /SHAPE.HOPPER.END
		STA COPY.FROM_END+$1
	
		;COPY.TO+$1 is init before loop starts and it is incremented below
	JSR MEMORY.COPY	
		

.INCREMENT.COUNTER
	INC COPY.TO+$1

	STA $C008 ;enable main zero-page & main BSR 
		LDX STACK_POINTER.SAVED	;restore stack pointer to X-REG
		TXS ;transfer X-REG to stack pointer

		
.READ.CHUNK
;----READ FILE-----
;(read file in $100 byte chunks until the remaining bytes to 
;read are < $100)
	
	lda #cmd_read.current_drive
	sta parm.reqcmd

;set destination memory address
	lda #SHAPE.HOPPER0
	sta parm.ldrlo
	lda /SHAPE.HOPPER0
	sta parm.ldrhi
	
;set read length (bytes)
	lda #$00				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda #$01		;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi

; ;set filename to read from	
	; lda #BS_AUX.ROUTINES.BK2	;load LO address
	; sta parm.namlo
	; lda /BS_AUX.ROUTINES.BK2	;load HO address
	; sta parm.namhi
	
		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO
	
			; cpy #$01 ;***remember Y-REG is a decement
			; bne .temp
			; lda #$bb
			; jsr prep.brk
			; brk
.TEMP
	
.DECREMENT.LOOP_COUNTER	
	DEY
	BNE .RESTORE.LOOP
	
@END


.RESTORE.DONE

		; lda #$01
		; sta troubleshooting.hook
	


; ;==========troubleshooting

	; ;enable AUX memory (BSR: bank2 is already enabled)						
		; TSX			;transfer stack pointer to X-REG
		; STX STACK_POINTER.SAVED	;save stack pointer
	; STA $C009 ;enable aux zero-page & aux BSR 

	; ;execute copy	
		; LDA #BS_AUX_ROUTINES.BANK2.COPY_TO.START
		; ;lda #$00
		; STA COPY.FROM_START+$0
		; LDA /BS_AUX_ROUTINES.BANK2.COPY_TO.START
		; ;lda #$E0
		; STA COPY.FROM_START+$1

		; LDA #$FF
		; STA COPY.FROM_END+$0
		; LDA #$E0
		; STA COPY.FROM_END+$1

		; LDA #$00
		; STA COPY.TO
		; LDA #$A0
		; STA COPY.TO+$1
		
		; ;COPY.TO+$1 is init before loop starts and it is incremented below
	; JSR MEMORY.COPY	


	; STA $C008 ;enable main zero-page & main BSR 
		; LDX STACK_POINTER.SAVED	;restore stack pointer to X-REG
		; TXS ;transfer X-REG to stack pointer
	
		; lda #$aB
		; jsr prep.brk
		; brk
		
; ;=========troubeshooting=====


		
@END

	; LDA #$01
	; STA COMBAT_SE.MODE.PARM	;($00 = player initiated or init value in map objects manager | $01 = mob initiated | $FF = test mode)

@END

	;**FALLS THROUGH**

COMBAT.PROCESS.LEVELUP
@START
	LDX #$01 ;init loop counter and player #
.LOOP.PROCESS.LEVELUP
			
.INIT
@START
	;read PC character sheet data
		TXA
		;ACC = player sequential # (high-bit not set = read mode)
	JSR COMBAT.READ_WRITE.CHR_SHEET.PC
		;RETURN VALUE = CHR_SHEET.RECORD.READ

	LDA #$01 ;default to no levelup
	STA COMBAT.LEVELUP.FLAG	;$1byte ($00 = levelup, $01 = no levelup)


@END
	;**FALLS THROUGH**
	
.CHECK.LEVEL.THREASHOLD
@START
	;validate entry
	LDA CHR_SHEET.PC_MOB.LEVEL
	CMP #COMBAT.LEVELUP.MAX_LEVEL
	BCS .CHECK.LEVEL.THREASHOLD.DONE ;branch if PC level is >= max level
	;calculate table index
	;(each table record has two fields and the table is indexed by level, so the index = level *2 		
	ASL ;X2
	TAY
	
	; LDA LEVEL.XP.THREASHOLD+$0,Y
	; STA COMBAT.LEVELUP.NEXT_LEVEL.XP+$0

	; LDA LEVEL.XP.THREASHOLD+$1,Y
	; STA COMBAT.LEVELUP.NEXT_LEVEL.XP+$1
	
	
	;CLC/SEC: testing confirmed that neither are needed for 16-bit >+ test.
	LDA CHR_SHEET.PC_MOB.XP+$0 ;OP1
    CMP LEVEL.XP.THREASHOLD+$0,Y ;OP2
    LDA CHR_SHEET.PC_MOB.XP+$1 ;OP1+1
    SBC LEVEL.XP.THREASHOLD+$1,Y ;OP2+1
    BCC .CHECK.LEVEL.THREASHOLD.DONE  ;if carry not set then OP1 < OP2, which means the player does not have enough experience to level up
	
;.PC.LEVELS_UP
	
	;increment PC level
	INC CHR_SHEET.PC_MOB.LEVEL
	
	;increment level-based index
	INY
	INY ;two bytes because the lookup tables using this index have two byte records
	
	;allocate attribute upgrade points
	;(the number of points is retreived from a lookup table with an index of PC level, net of the levelup)
	LDA LEVELUP.ATTRIBUTE_UPGRADE.POINTS,Y
	STA CHR_SHEET.PC.ATTRIB.UPGRADE_POINTS
	
	;set levelup flag
	;(purpose of flag not yet determined)
	LDA #$00
	STA COMBAT.LEVELUP.FLAG	;$1byte ($00 = levelup, $01 = no levelup)

	;update HP MAX fields
	
	;<****INSERT CODE>****
	
	;*note: the code below was designed to get HP max from a table but
	;HP max is not the same for all PCs at each level due to my plan to incorporate strength as a modifier to the HP levelup increase.  
	;HP max should be set here and when characters are created. It isn't needed in CALCULATE.COMBAT.STATS.GENERAL_ENTRANCE
	
	
	; ;(this code section is in CALCULATE.COMBAT.STATS.GENERAL_ENTRANCE and COMBAT.PROCESS.LEVELUP because the former is called during game launch to init character sheet values and the later 
	; ;is where the player's level gets changed during gameplay)
	; LDA CHR_SHEET.PC_MOB.LEVEL	
	; ;calculate table index
	; ;(each table record has two fields and the table is indexed by level, so the index = level *2 		
	; ASL ;X2	;**OPT** Memory. index is already calculated above so this setup shouldn't be necessary. 
	; TAY
	; DEY ;subtract 2 (1 record) from index because table starts with level1 not level0
	; DEY ;""
	; LDA HP_MAX.TABLE+$0,Y ;load HP MAX LO byte for PCs current level
	; STA CHR_SHEET.PC.HP_MAX.LO
	; LDA HP_MAX.TABLE+$1,Y ;load HP MAX HO byte for PCs current level
	; STA CHR_SHEET.PC.HP_MAX.HO

.CHECK.LEVEL.THREASHOLD.DONE

		
@END

	;**FALLS THROUGH**

.CHECK.SKILL.THREASHOLDS
@START
;(source data for ALL_SKILLS.SKILL.THREASHOLD and MELEE_RANGE_DODGE.PROGRESS.BUCKET_SIZE
;is the STA spreadsheet (top level metrics tab). It also includes a schematic of the table). 

;CONNECT SKILL LOOKUP TABLE POINTERS
	LDA #ALL_SKILLS.SKILL.THREASHOLD
	STA	COMBAT.LEVELUP.SKILL_THREASHOLD.POINTER+$0	
	LDA /ALL_SKILLS.SKILL.THREASHOLD
	STA	COMBAT.LEVELUP.SKILL_THREASHOLD.POINTER+$1

	LDA #MELEE_RANGE_DODGE.PROGRESS.BUCKET_SIZE
	STA	COMBAT.LEVELUP.SKILL_PROG_SIZE.POINTER+$0	
	LDA /MELEE_RANGE_DODGE.PROGRESS.BUCKET_SIZE
	STA	COMBAT.LEVELUP.SKILL_PROG_SIZE.POINTER+$1
	
	
;MELEE SKILL
;**OPT** Pass the offset to the skill parm via ACC, then transfer
;to X-REG and access the character sheet read buffer directly. Access the progress
;bucket field via offset+$1. This of course requires that the offset field is always 
;the field immediately after the associated skill field. Currently that is the case, except for
;critical hit. 
		
		LDA CHR_SHEET.PC.SKILL.MELEE
		STA COMBAT.LEVELUP.SKILL.PARM	
		LDA CHR_SHEET.PC.SKILL.MELEE.PROG
		STA COMBAT.LEVELUP.SKILL_PROG.PARM
	JSR CHECK.SKILL.LEVELUP
		LDA COMBAT.LEVELUP.SKILL.PARM
		STA CHR_SHEET.PC.SKILL.MELEE
		LDA COMBAT.LEVELUP.SKILL_PROG.PARM
		STA CHR_SHEET.PC.SKILL.MELEE.PROG
	
	
;RANGE SKILL
		LDA CHR_SHEET.PC.SKILL.RANGE
		STA COMBAT.LEVELUP.SKILL.PARM
		LDA CHR_SHEET.PC.SKILL.RANGE.PROG
		STA COMBAT.LEVELUP.SKILL_PROG.PARM
	JSR CHECK.SKILL.LEVELUP
		LDA COMBAT.LEVELUP.SKILL.PARM
		STA CHR_SHEET.PC.SKILL.RANGE
		LDA COMBAT.LEVELUP.SKILL_PROG.PARM
		STA CHR_SHEET.PC.SKILL.RANGE.PROG
	
;DODGE SKILL
		LDA CHR_SHEET.PC.SKILL.DODGE
		STA COMBAT.LEVELUP.SKILL.PARM
		LDA CHR_SHEET.PC.SKILL.DODGE.PROG
		STA COMBAT.LEVELUP.SKILL_PROG.PARM
	JSR CHECK.SKILL.LEVELUP
		LDA COMBAT.LEVELUP.SKILL.PARM
		STA CHR_SHEET.PC.SKILL.DODGE
		LDA COMBAT.LEVELUP.SKILL_PROG.PARM
		STA CHR_SHEET.PC.SKILL.DODGE.PROG


;CRITICAL HIT SKILL

	;connect lookup table for critical hit progress bucket
	;(no need to connect a different threashold lookup table because critical hit shares
	;the same one with the prior skills)
	LDA #CH.PROGRESS.BUCKET_SIZE	
	STA	COMBAT.LEVELUP.SKILL_PROG_SIZE.POINTER+$0	
	LDA /CH.PROGRESS.BUCKET_SIZE
	STA	COMBAT.LEVELUP.SKILL_PROG_SIZE.POINTER+$1

	
		LDA CHR_SHEET.PC.SKILL.CRITICAL_HIT
		STA COMBAT.LEVELUP.SKILL.PARM
		LDA CHR_SHEET.PC.SKILL.CRITICAL_HIT.PROG
		STA COMBAT.LEVELUP.SKILL_PROG.PARM
	JSR CHECK.SKILL.LEVELUP
		LDA COMBAT.LEVELUP.SKILL.PARM
		STA CHR_SHEET.PC.SKILL.CRITICAL_HIT
		LDA COMBAT.LEVELUP.SKILL_PROG.PARM
		STA CHR_SHEET.PC.SKILL.CRITICAL_HIT.PROG

		
;LOCKPICKING SKILL

	;connect lookup table for critical hit progress bucket
	;(no need to connect a different threashold lookup table because critical hit shares
	;the same one with the prior skills)
	LDA #LOCKPICKING.PROGRESS.BUCKET_SIZE	
	STA	COMBAT.LEVELUP.SKILL_PROG_SIZE.POINTER+$0	
	LDA /LOCKPICKING.PROGRESS.BUCKET_SIZE
	STA	COMBAT.LEVELUP.SKILL_PROG_SIZE.POINTER+$1

	
		LDA CHR_SHEET.PC.SKILL.LOCKPICKING 
		STA COMBAT.LEVELUP.SKILL.PARM
		LDA CHR_SHEET.PC.SKILL.LOCKPICKING.PROG
		STA COMBAT.LEVELUP.SKILL_PROG.PARM
	JSR CHECK.SKILL.LEVELUP
		LDA COMBAT.LEVELUP.SKILL.PARM
		STA CHR_SHEET.PC.SKILL.LOCKPICKING 
		LDA COMBAT.LEVELUP.SKILL_PROG.PARM
		STA CHR_SHEET.PC.SKILL.LOCKPICKING.PROG

		
;PILFERY SKILL

	;connect lookup table for critical hit progress bucket
	;(no need to connect a different threashold lookup table because critical hit shares
	;the same one with the prior skills)
	LDA #PILFERY.PROGRESS.BUCKET_SIZE	
	STA	COMBAT.LEVELUP.SKILL_PROG_SIZE.POINTER+$0	
	LDA /PILFERY.PROGRESS.BUCKET_SIZE
	STA	COMBAT.LEVELUP.SKILL_PROG_SIZE.POINTER+$1

	
		LDA CHR_SHEET.PC.SKILL.PILFER
		STA COMBAT.LEVELUP.SKILL.PARM
		LDA CHR_SHEET.PC.SKILL.PILFER.PROG
		STA COMBAT.LEVELUP.SKILL_PROG.PARM
	JSR CHECK.SKILL.LEVELUP
		LDA COMBAT.LEVELUP.SKILL.PARM
		STA CHR_SHEET.PC.SKILL.PILFER
		LDA COMBAT.LEVELUP.SKILL_PROG.PARM
		STA CHR_SHEET.PC.SKILL.PILFER.PROG
		
@END

			; ;TROUBLESHOOTING LOOP
			; LDX #$00
; .TEST.LOOP2			
			; LDA CHR_SHEET.RECORD.READ,X
			; STA $BF00,X
			; INX
			; BNE .TEST.LOOP2

			
			; LDA #$AB			
			; ; LDX CHR_SHEET.PC.SKILL.MELEE
			; ; LDY CHR_SHEET.PC.SKILL.MELEE.PROG	
			; LDX  CHR_SHEET.PC.SKILL.CRITICAL_HIT
			; LDY  CHR_SHEET.PC.SKILL.CRITICAL_HIT.PROG			
			; ;LDX COMBAT.LEVELUP.FLAG	;$1byte ($00 = levelup, $01 = no levelup)
			; JSR PREP.BRK
			; BRK
			
			
	
.WRITE.CHAR_SHEET
	;read character sheet
		TXA
		ORA #$80 ;set write mode by masking in high-bit
		;ACC = player sequential # + (high-bit not set = read mode | high-bit set = write mode)
	JSR COMBAT.READ_WRITE.CHR_SHEET.PC	



			

	;exit test
	CPX PARTY.TOTAL.PC
	BEQ .LOOP.PROCESS.LEVELUP.DONE
	INX		
	JMP .LOOP.PROCESS.LEVELUP
.LOOP.PROCESS.LEVELUP.DONE
			; lda #$ab
			; ;ldx #CHR_SHEET.RECORD.READ
			; ;ldy /CHR_SHEET.RECORD.READ
			; jsr prep.brk
			; brk
			
	; ;read PC character sheet data
		; LDA #$02
		; ;ACC = player sequential # (high-bit not set = read mode)
	; JSR COMBAT.READ_WRITE.CHR_SHEET.PC
		; ;RETURN VALUE = CHR_SHEET.RECORD.READ

			; ;TROUBLESHOOTING LOOP
			; LDX #$00
; .TEST.LOOP2			
			; LDA CHR_SHEET.RECORD.READ,X
			; STA $BF00,X
			; INX
			; BNE .TEST.LOOP2

			
			; LDA #$AC			
			; ; LDX CHR_SHEET.PC.SKILL.MELEE
			; ; LDY CHR_SHEET.PC.SKILL.MELEE.PROG	
			; LDX  CHR_SHEET.PC.SKILL.CRITICAL_HIT
			; LDY  CHR_SHEET.PC.SKILL.CRITICAL_HIT.PROG			
			; ;LDX COMBAT.LEVELUP.FLAG	;$1byte ($00 = levelup, $01 = no levelup)
			; JSR PREP.BRK
			; BRK
			
		
.SAVE.PC_CHR_SHEET
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

	lda #cmd_write.drive2
	sta parm.reqcmd

;set write data size (# of 512 byte blocks to write from memory)
	lda #DATA.PLY.CHR_SHEET.FILE_SIZE ;always #$00
	sta parm.sizelo

	lda /DATA.PLY.CHR_SHEET.FILE_SIZE ;number of pages to write (must be in multiples of 2. i.e. 2, 4, 6, 8 $0A, etc)
	sta parm.sizehi
	
;set source memory address for write
	lda #CHR_SHEET.PC.AUX_MEMORY.START
	sta parm.ldrlo
	lda /CHR_SHEET.PC.AUX_MEMORY.START
	sta parm.ldrhi
	
;set filename to write to	
	lda #DATA.PLY.CHR_SHEET ;load LO address
	sta parm.namlo	
	lda /DATA.PLY.CHR_SHEET ;load HO address
	sta parm.namhi	
		
		LDA #$01	;PARM: $00=main, $01=aux
	JSR PRODOS.IO
@END

@END

.TREASURE_DROP
	JSR COMBAT.TREASURE_MGT.ENTRANCE

	
.EXIT


;SAVE REGISTERS
	PLA
	TAY
	PLA
	TAX
	
	RTS ;I think this is the exit from COMBAT.EXIT
	
CHECK.SKILL.LEVELUP
@START
	
	;get index for MELEE_RANGE_DODGE.PROGRESS.BUCKET_SIZE and ALL_SKILLS.SKILL.THREASHOLD
	LDY #$00
.GET.PROGRESS_BUCKET.INDEX
;(match up the PCs skill points to the threashold in ALL_SKILLS.SKILL.THREASHOLD.
;the skill threadhold is what determines the bucket size. The bucket size is indexed to the order that the 
;threadholds are listed in. Thus, once the PCs skill is matched up to the threashold, the index is known
;for MELEE_RANGE_DODGE.PROGRESS.BUCKET_SIZE)

			
	LDA (COMBAT.LEVELUP.SKILL_THREASHOLD.POINTER),Y
	CMP COMBAT.LEVELUP.SKILL.PARM
	BEQ .GET.PROGRESS_BUCKET.INDEX.DONE ;if equal take different branch so that index is not decremented
	BCS .FINALIZE.INDEX
	INY	
	INY
	CPY #MELEE_RANGE_DODGE.SKILL.TABLE.SIZE
	BEQ .FINALIZE.INDEX	
	JMP .GET.PROGRESS_BUCKET.INDEX

			; LDA (COMBAT.LEVELUP.SKILL_THREASHOLD.POINTER),Y
			; ;LDX #$AC
			; ldX COMBAT.LEVELUP.SKILL.PARM
			; JSR PREP.BRK
			; BRK
.FINALIZE.INDEX
	;finalize index
	;(decrement index because once threashold is greater than skill that means the threahold just before the current is the one that applies.
	;For example if the skill is 51 and the threadholds are 25, 75 then the threadhold index to get the correct bucket size is 25....that bucket size woud
	;apply to advance skill points from 25 to 75)
	;(also, if the skill points = threashold then that is the threahold that applies, so no need to decrement the index. This is the purpose of the
	;the BEQ .GET.PROGRESS_BUCKET.INDEX.DONE code in the loop above.)
	
	DEY ;decrement index
	DEY ;decrement another byte because each record has 2 fields

.GET.PROGRESS_BUCKET.INDEX.DONE

			; LDA (COMBAT.LEVELUP.SKILL_THREASHOLD.POINTER),Y
			; ldX COMBAT.LEVELUP.SKILL.PARM
			; ;LDX #$AC
			; ;LDY COMBAT.LEVELUP.SKILL_PROG.PARM 
			; ; LDX CHR_SHEET.PC.SKILL.MELEE
			; ; LDY CHR_SHEET.PC.SKILL.MELEE.PROG	
			; ;LDX COMBAT.LEVELUP.FLAG	;$1byte ($00 = levelup, $01 = no levelup)
			; JSR PREP.BRK
			; BRK
	
			; LDA (COMBAT.LEVELUP.SKILL_PROG_SIZE.POINTER),Y
			; ldX COMBAT.LEVELUP.SKILL_PROG.PARM
			; ;LDX #$AC
			; ;LDY COMBAT.LEVELUP.SKILL_PROG.PARM 
			; ; LDX CHR_SHEET.PC.SKILL.MELEE
			; ; LDY CHR_SHEET.PC.SKILL.MELEE.PROG	
			; ;LDX COMBAT.LEVELUP.FLAG	;$1byte ($00 = levelup, $01 = no levelup)
			; JSR PREP.BRK
			; BRK
			
	;does PC have enough progress points to levelup this skill?
	LDA COMBAT.LEVELUP.SKILL_PROG.PARM
	CMP (COMBAT.LEVELUP.SKILL_PROG_SIZE.POINTER),Y
	BCC .LEVELUP.CHECK.DONE


			
	;update skill points and progress bucket
	;(whatever skill progress points weren't used )
	;XREG = index for MELEE_RANGE_DODGE.PROGRESS.BUCKET_SIZE and ALL_SKILLS.SKILL.THREASHOLD
	LDA COMBAT.LEVELUP.SKILL_PROG.PARM
	SEC
	SBC (COMBAT.LEVELUP.SKILL_PROG_SIZE.POINTER),Y
	BCC .ERROR.UNDERFLOW 
	STA COMBAT.LEVELUP.SKILL_PROG.PARM

			; lda (COMBAT.LEVELUP.SKILL_PROG_SIZE.POINTER),Y
			; ;LDx #$AB			
			; LDx COMBAT.LEVELUP.SKILL_PROG.PARM
			; ;LDX COMBAT.LEVELUP.FLAG	;$1byte ($00 = levelup, $01 = no levelup)
			; JSR PREP.BRK
			; BRK

.INCREMENT.SKILL_POINTS			
	;increment skill points
	LDA COMBAT.LEVELUP.SKILL.PARM
	CLC
	ADC #$01
	;overflow check
	BCC .INCREMENT.SKILL_POINTS.DONE
	LDA #COMBAT.SKILL.MAX
	;**FALLS THROUGH**
.INCREMENT.SKILL_POINTS.DONE	
	;ACC = updated skill points
	STA COMBAT.LEVELUP.SKILL.PARM
.LEVELUP.CHECK.DONE

.EXIT

	RTS
	
	
	
.ERROR.UNDERFLOW
;.GET.PROGRESS_BUCKET.INDEX.DONE reports underflow, which
;shouldn't occur because if .GET.PROGRESS_BUCKET.INDEX worked correctly
;and the lookup table values are setup correctly, then
;MELEE_RANGE_DODGE.PROGRESS.BUCKET_SIZE should be <= COMBAT.LEVELUP.SKILL.PARM.PROG

	JSR PREP.BRK
	BRK
	
	
@END

@END

;**this routine will probably need
;the text window functions so that it can
;display some levelup alerts



;SUPPORTING SUBROUTINES: General
@START
COMBAT.TREASURE_MGT.ENTRANCE
@START
.KEYPRESS_ABORT_CHECK
	LDA KB_BUFFER
	CMP #$9B
	BNE .KEYPRESS_ABORT_CHECK.DONE  ;ESC key was not pressed
	;ESC key pressed: skip treasure drop routine
	STA	KB_BUFFER_ACK               ;CLR LAST KEY
	JMP .EXIT
.KEYPRESS_ABORT_CHECK.DONE					
					
.INIT.SCREEN
;draw item drop window (right)
		;set parameters of INIT.TEXT_WINDOW.RIGHT
		LDA #TWB.TREASURE_DROP.LEFT_SBYTE
		STA DRAW.START_BYTE
		;LDA #$00 ;($00 = draw input window line | >=$01 = don't draw input window line)
		LDA #$01 ;($00 = draw input window line | >=$01 = don't draw input window line)
	JSR INIT.TEXT_WINDOW.RIGHT 
	;JSR COMBAT.ERASE.TREASURE_DROP.WINDOW.TEXT_SPACE
	
	
;init scroll window top line
@START
	LDA #TWS.TREASURE_DROP.SCROLL_WINDOW.TOP_LINE	
	STA DRAW.START_LINE
	
	LDA #TWS.TREASURE_DROP.SCROLL_WINDOW.TOP_LINE+1						 ;(set to last line to draw + 1)
	STA DRAW.STOP_LINE

	LDA #TWS.TREASURE_DROP.SCROLL_WINDOW.LEFT_SBYTE-1
	STA DRAW.START_BYTE
	
	;LDA #TWS.TREASURE_DROP.SCROLL_WINDOW.RIGHT_SBYTE+1
	LDA #TWB.TREASURE_DROP.RIGHT_SBYTE+1
	STA DRAW.STOP_BYTE
		
	
	LDA #$D5
	STA DRAW.BYTE_VALUE.HORIZONTAL+$0
	
	LDA #$AA
	STA DRAW.BYTE_VALUE.HORIZONTAL+$1
	
.DRAW
;(when both hi-res pages need to stay in sync, instead of drawing to both pages, sometimes it is best to the pages individually
;and flip pages inbetween. This way the draw isn't as noticable to the user)
		
		LDA #$07 ;BIT0-2 set: use both hi-res pages. draw top line of rectangle. ignore all other bit flags. See subroutine docs for full bit flag list.
	JSR DRAW.LINE

@END
	
	
	
					
					;jsr keyin

	
	
	;#####search for usable equipment?
	
		;jsr keyin
		
	JSR COMBAT.LAUNCH.TREASURE_MGT
	
.EXIT

	RTS
	
	
TREASURE.TEST		.AZ -/Testing/
	
@END


COMBAT.LAUNCH.TREASURE_MGT
@START


	
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
	lda #TM_1.TREASURE_MGT.PATCH_START
	sta parm.ldrlo
	lda /TM_1.TREASURE_MGT.PATCH_START
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
	lda #TM_1.TREASURE_MGT.SEEK_BYTES			;seek length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda /TM_1.TREASURE_MGT.SEEK_BYTES			;seek length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
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
	lda #TM_1.TREASURE_MGT.READ_BYTES				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda /TM_1.TREASURE_MGT.READ_BYTES				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi
			
		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO
	
	JSR TM_1.TREASURE_MGT
	
	RTS
	



@END


@END

;COMBAT.ERASE.TREASURE_DROP.WINDOW.TEXT_SPACE   ;!!this routine didn't work for some reason. So I buckled it. 
@START
; ;erases the treasure drop items text (bOth hi-res pages)

		; LDA #TWS.LW.TREASURE_DROP.TOP_LINE		
		; STA DRAW.START_LINE
		
		; LDA #TWS.LW.TREASURE_DROP.LEFT_SBYTE	
		; STA DRAW.START_BYTE
		
		; LDA #TWS.LW.TREASURE_DROP.RIGHT_SBYTE
		; STA DRAW.STOP_BYTE
			
		; LDA #TWS.LW.TREASURE_DROP.BOTTOM_LINE

			
; .DRAW
; ;(when both hi-res pages need to stay in sync, instead of drawing to both pages, sometimes it is best to the pages individually
; ;and flip pages inbetween. This way the draw isn't as noticable to the user)
		
		; LDA PAGE.BACKGROUND ;set bit 0 & 1 with the desired hi-res page value
		; ORA #$7C 			;sets bits 2-5 so all border edges are drawn. set bit6 to enable draw/erase mode
	; JSR DRAW.LINE
	
	; JSR FLIP.PAGE
	
						; ;JSR KEYIN
						
					
		; LDA PAGE.BACKGROUND
		; ORA #$7C	
	; JSR DRAW.LINE

	
; .EXIT

	; RTS
	
@END

;LOCAL VARIABLES
@START

;GENERAL
@START
;.DA directive documentation
;the .DA directive in SBASM converts each comma deliminated value to a 16-bit hex value and inserts it
;in the target binary file at the location of the program counter. 
;if no sign (such as $,%) is specified for a value, then the value is assumed to be decimal.
;
;Example;
;
;program counter = $6000
;.DA 100,256
;
;result
;$6000: 64.00.00.01
;

;(source data forthe tables below is the STA spreadsheet (top level metrics tab). It also includes a schematic of the table). 
LEVEL.XP.THREASHOLD				.DA	0,50,115,199,308,449,632,869,1177,1577,2097,2773,3651,4792,6275,8202
;LEVEL.XP.THREASHOLD						.DA	0,100,250,500,800,1600,3200,6400,12800,25600
LEVELUP.ATTRIBUTE_UPGRADE.POINTS				.DA	0,0,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12
;LEVELUP.ATTRIBUTE_UPGRADE.POINTS		.DA	0,0,4,4,4,4,8,8,12,16,20

ALL_SKILLS.SKILL.THREASHOLD				.DA	20,32,44,56,68,80,92,104,116,128,140,152,164,176,188
MELEE_RANGE_DODGE.PROGRESS.BUCKET_SIZE	.DA	16,18,20,22,26,30,32,36,41,45,51,56,61,66,72
CH.PROGRESS.BUCKET_SIZE					.DA	1,1,1,2,3,5,6,8,10,13,16,19,23,27,32
LOCKPICKING.PROGRESS.BUCKET_SIZE		.DA	12,12,12,12,12,12,12,12,12,12,12,12,12,12,12
PILFERY.PROGRESS.BUCKET_SIZE			.DA	12,12,12,12,12,12,12,12,12,12,12,12,12,12,12
										
MELEE_RANGE_DODGE.SKILL.TABLE.SIZE		.EQ	$1E

; ALL_SKILLS.SKILL.THREASHOLD				.DA	12,16,20,24,32,40,52,68,88
; MELEE_RANGE_DODGE.PROGRESS.BUCKET_SIZE	.DA	20,31,57,64,88,160,165,172,255
; CH.PROGRESS.BUCKET_SIZE					.DA	1,1,2,3,5,12,16,22,33
; LOCKPICKING.PROGRESS.BUCKET_SIZE		.DA	4,4,4,4,8,8,12,16,24
; PILFERY.PROGRESS.BUCKET_SIZE			.DA	4,4,4,4,8,8,12,16,24

; MELEE_RANGE_DODGE.SKILL.TABLE.SIZE		.EQ	$12	;the qty of fields in the lookup tables above *2. Each decimal number in the table is one field, which is comprised of 2 bytes (LO/HO)

@END


@END


@END
			
			
COMBAT.EXIT.END

				
;COMBAT.EXIT.SIZE_OVERFLOW_CHECK
@START
;(the reason for this check is because COMBAT.EXIT and TM_1.TREASURE_MGT.PATCH_START need to 
;exist in the main memory swap space at the same time. COMBAT.EXIT starts at the beginning of the 
;swap space and TM_1.TREASURE_MGT.PATCH_START is positioned higher up in the swap space)

;COMBAT.EXIT.SIZE = COMBAT.EXIT.END-INV_8.1.COMBAT.EXIT.START

	
;SIZE OVERFLOW CHECK
	.DO COMBAT.EXIT.END>TM_1.TREASURE_MGT.PATCH_START
	
	.ER	F,***COMBAT.EXIT.SIZE_OVERFLOW_CHECK reports size overflow
	
	
	.FI ;END IF				
				
				
				
				
@END
				
				