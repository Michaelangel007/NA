; ============================================
; Copyright (C) 2016-2018. 6502 Workshop, LLC
; contact: mark@6502workshop.com
; ============================================


;************************INCLUDE FILE*****************************
;(do not assemble stand-alone.  )
				.TF     swap.routines.cast_spell.setup.bin,BIN
				.OR		SRTN.CAST_SPELL.FILE_ADDRESS			**Always put before .TF directive and never use again in program
						;$9F00
				.EB		OFF

;=====================SETUP/EXIT ROUTINES DOCUMENTATION====================================
;
;---FUNCTION---
;
;This file handles all preparatory routines for spell casting which do not need
;to be located in a particular memory area, such as the main game engine. 
;
;---MEMORY LOCATION---
;This file is loaded in memory just below COMBAT.PERSISTANT.MEMORY. This
;memory space is clobbered by the spell file when loaded.  
;This file could be loaded anywhere in the memory range of the main combat module.
;The reason it is loaded at the end, just before COMBAT.PERSISTANT.MEMORY is so that
;a .NO to COMBAT.PERSISTANT.MEMORY at the end of this file uses less memory. The .NO is there
;just to avoid overflowing into the persistent memory range.  
;
;Placing the routines in this file results in less memory used in combat module, spell file,
;and main game engine. Anything that can be put here should be as it is the least taxed area of memory
;involved in the spell casting processs. 
;
;=================================================================================

CAST_SPELL.ENTRANCE ;(called from COMMAND.CAST_SPELL)
@START
.INIT
			
	;PC/MOB TURN BRANCH
	LDA COMBAT.TURN_STATUS   ;($00 = PCs turn. $01 = Special(s) S_ENTITY (example Wyvern Ally, Summoned Demon), $02 = MOBs turn)
	BEQ .SET.PRINT_STRING.MODE.DONE ;if PCs turn, then branch

	LDA #$01
	STA PRINT.STR.MODE	;($00 = normal | >=$01 = wait/pause functionality enabled)
.SET.PRINT_STRING.MODE.DONE

	
	;read spell file graphic effects entrance code into main memory swap space
.LOAD.SPELL_FILE
@START
		
;filename = "SRTN.SPELL_FILE"
	
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
	LDA #SPELL_FILE.GRAPHICAL.EFFECTS.READ_BYTES
	sta parm.sizelo	
	LDA /SPELL_FILE.GRAPHICAL.EFFECTS.READ_BYTES
	sta parm.sizehi
	
;set destination memory address
	lda #SPELL.FILE.ENTRANCE.BUFFER
	sta parm.ldrlo
	lda /SPELL.FILE.ENTRANCE.BUFFER
	sta parm.ldrhi

;set filename to read from	
	lda #SRTN.SPELL_FILE	;load LO address
	sta parm.namlo
	lda /SRTN.SPELL_FILE	;load HO address
	sta parm.namhi
		LDA #$00 ;;PARM: $00=main, $01=aux
	JSR PRODOS.IO
@END

@END

	;**FALLS THROUGH**

CAST_SPELL.SELECT_SPELL
@START
;=====================SUBROUTINE DOCUMENTATION====================================
;
;This routine is loaded from disk by COMBAT.COMMAND.CAST. This routine parses spell code stored in the player 
;keypress and then branches to:
;		*print the spell name to the scroll window
;		*JMP to the entrance routine for the associated spell. 
;
;The spell entrance routine (in the spell file) handles loading the associated spell code block from disk.
;
;=================================================================================


.INIT
@START
	;init target database
	;(needs to be done so that when the DB is checked for duplicates it doesn't get mixed up by old values)
	;(the init value can be any value that is not a valid screen index ($00 -$BA)
	
	LDA #COMBAT.TARGET_HIT.DB.STOP_VALUE ;set stop value in first byte in case there are no targets
	STA COMBAT.TARGET_HIT.DB+$0
	LDX #$01 ;start init at 2nd byte for the reason noted above
	STX COMBAT.STATS.SPELL.KILL_FLAG ;set here for convenience. death magic attack: ($00 = succeeded | $01 = not attempted | $03 = failed). If set then all targets hit by the spell are automatically killed. Sometimes saving thows may be available. 
	STX COMBAT.STATS.SPELL.KILL_FLAG_MASTER ;set here for convenience. death magic attack: ($00 = succeeded | $01 = not attempted | $03 = failed). If set then all targets hit by the spell are automatically killed. Sometimes saving thows may be available. 
.TARGET.DB.INIT.LOOP
	LDA #$EA
	STA COMBAT.TARGET_HIT.DB,X
	INX
	CPX #COMBAT.TARGET_HIT.DB.SIZE
	BNE .TARGET.DB.INIT.LOOP
@END

	;**FALLS THROUGH**

;GET SPELL INFO, VALIDATE MP, VALIDATE SPELL READIED
@START
	
;PC/MOB TURN BRANCH
;(the branch is because for mobs and specials the spell code was set by .MOB.CAST_SPELL before COMBAT.CAST.EXECUTE was called.
	LDA COMBAT.TURN_STATUS   ;($00 = PCs turn. $01 = Special(s) S_ENTITY (example Wyvern Ally, Summoned Demon), $02 = MOBs turn)
	BEQ .GET.SPELL_INFO		 ;if PCs turn, then branch
	LDA COMBAT.SPELL_CODE.CAST
	JMP .GET.SPELL_INFO.MOB_SPECIAL

	;**FALLS THROUGH**
	
.GET.SPELL_INFO	
	LDA COMBAT.CAST.KEYPRESS.ASCII ;player keypress response (cast)
	AND #$0F ;convert keypress from ASCII to hex (masks out the HO nibble)
	TAX ;transfer keypress to X-REG as index
	
	LDA PLAYER.HOTKEYS.SPELLS,X
	STA COMBAT.SPELL_CODE.CAST
	;is there a spell readied to this hotkey?
	BPL .GET.SPELL_INFO.DONE	;if high-bit is not set then a spell is readied. 
;NO SPELL READIED
		LDA #COMBAT.TEXT_BLOCK.NO_SPELL_READIED
		STA TWF.STRING+$0
		
		LDA /COMBAT.TEXT_BLOCK.NO_SPELL_READIED
		STA TWF.STRING+$1
	JMP .NO.SPELL.READIED

.GET.SPELL_INFO.DONE
	
.GET.SPELL_INFO.MOB_SPECIAL ;ALTERNATE ENTRANCE FROM BRANCH ABOVE
	;ACC = COMBAT.SPELL_CODE.CAST
	
			; lda #$12 ;override spell code for mob/special spell testing
			; STA COMBAT.SPELL_CODE.CAST
	
	;calculate spell table index
	ASL ;X2 because the spell table has 2 byte records
	TAX ;transfer spell code to X-REG as index

	;load spell damage
	LDA COMBAT.STATS.SPELL_TABLE+$0,X
	STA COMBAT.STATS.SPELL.DAMAGE
	;load magic point cost for spell
	LDA COMBAT.STATS.SPELL_TABLE+$1,X
	STA COMBAT.STATS.SPELL.MP_COST

			; ;LDA PLAYER.HOTKEYS.SPELLS
			; LDA #$AA
			; LDX COMBAT.STATS.SPELL.DAMAGE
			; LDY COMBAT.STATS.SPELL.MP_COST
			; JSR PREP.BRK
			; BRK
			
	;**FALLS THROUGH**
	
.VALIDATE.MP
	;read attacker character sheet
		LDY COMBAT.ATTACKER.SINDEX
		LDA #$00 ;set read mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet
		;RETURN: CHR_SHEET.RECORD.READ($80)

			; LDA #$AA
			; LDX CHR_SHEET.PC_MOB.MP	
			; LDY COMBAT.STATS.SPELL.MP_COST
			; JSR PREP.BRK
			; BRK
			
	;does attacker have enough MP to cast the spell?
	LDA CHR_SHEET.PC_MOB.MP	
	CMP COMBAT.STATS.SPELL.MP_COST
	BCS .VALIDATE.MP.DONE ;branch if attacker has enough MP to cast spell
.MP.NOT_SUFFICENT

	;pc/mob turn branch
	;(the branch is because for mobs and specials, the MP check was done by the calling routine. The MOB/Special MP check
	;is just to confirm the mob's MP is >0, and the intention is that their MP will be a multiple of the MP cost of the spell they are designed to cast.
	;thus, if the standard MP check in this routine fails, then there is a problem, likely that the MOB/SPECIALs MP was not entered as a multiple of its spell cost in the 
	;MOB/SPECIAL data table.)
	LDA COMBAT.TURN_STATUS   ;($00 = PCs turn. $01 = Special(s) S_ENTITY (example Wyvern Ally, Summoned Demon), $02 = MOBs turn)
	BNE .ERROR.MOB_SPECIAL.MP_CHECK.FAIL	 ;if not PCs turn, then branch

		LDA #COMBAT.TEXT_BLOCK.MP_TOO_LOW
		STA TWF.STRING+$0
		
		LDA /COMBAT.TEXT_BLOCK.MP_TOO_LOW
		STA TWF.STRING+$1
		;**FALLS THROUGH**
.NO.SPELL.READIED ;(to the hotkey selected). Branch take here from .GET.SPELL_INFO	
	;PCs turn: MP not sufficient to cast spell
	JSR PLAY.SOUND.DUMB_ASS
	LDA #$01 ;set flag
	STA COMBAT.ABORT.FLAG		;($00 = not set, $01 = set)
	
	
	;update scroll window	
	
		;STRING(2): already set
	JSR PRINT.TEXT.WINDOW	

				
	JMP CAST_SPELL.EXIT ;(located in main combat module, persistent memory.)

	
	
.ERROR.MOB_SPECIAL.MP_CHECK.FAIL
;(for mobs and specials, the MP check was done by the calling routine. The MOB/Special MP check
;is just to confirm the mob's MP is >0, and the intention is that their MP will be a multiple of the MP cost of the spell they are designed to cast.
;thus, if the standard MP check in this routine fails (triggering this error trap), then there is a problem, likely that the MOB/SPECIALs MP was not entered as a multiple of its spell cost in the 
;MOB/SPECIAL data table.)
	
	JSR PREP.BRK
	BRK
	
.VALIDATE.MP.DONE
@END

	;**FALLS THROUGH**

;CALL ENTRANCE FOR SPELL CAST
@START

.INIT2
	;;seed RTS memory address for use by the spell entrance routines. (i.e. GE.SPELL.MAGIC_MISSLE.ENTRANCE, etc)
	LDA /SPELL_FILE.AFTER_CAST-1
	PHA
	LDA #SPELL_FILE.AFTER_CAST-1
	PHA

	; LDA #SPELL_FILE.AFTER_CAST
	; SEC
	; SBC #$01
	; STA TEMP16+$0
	; LDA /SPELL_FILE.AFTER_CAST
	; SBC #$00
	; STA TEMP16+$1
	
	; LDA TEMP16+$1
	; PHA
	; LDA TEMP16+$0
	; PHA
	
	
.PARSE.SPELL_CODE
;BRANCH TO SPELL ENTRANCE

	;erase cursor from select spell prompt
	JSR PRINT.TEXT.WINDOW.SPACE

	LDA COMBAT.SPELL_CODE.CAST
	CMP #$00
	BEQ .SPELL_CODE.00_STEP
	CMP #$01
	BEQ .SPELL_CODE.01_STEP
	CMP #$02
	BEQ .SPELL_CODE.02_STEP
	CMP #$03
	BEQ .SPELL_CODE.03_STEP
	CMP #$04
	BEQ .SPELL_CODE.04_STEP
	CMP #$05
	BEQ .SPELL_CODE.05_STEP
	CMP #$06
	BEQ .SPELL_CODE.06_STEP
	CMP #$07
	BEQ .SPELL_CODE.07_STEP
	CMP #$08
	BEQ .SPELL_CODE.08_STEP
	CMP #$09
	BEQ .SPELL_CODE.09_STEP
	CMP #$0A
	BEQ .SPELL_CODE.0A_STEP	
	CMP #$0B
	BEQ .SPELL_CODE.0B_STEP	;**FALLS THROUGH**
	CMP #$0C
	BEQ .SPELL_CODE.0C_STEP	
	CMP #$0E
	BEQ .SPELL_CODE.0E_STEP	
	CMP #$12
	BEQ .SPELL_CODE.12_STEP
	CMP #$13
	BEQ .SPELL_CODE.13_STEP
	CMP #$14
	BEQ .SPELL_CODE.14_STEP	
	CMP #$15
	BEQ .SPELL_CODE.15_STEP		
	CMP #$16
	BEQ .SPELL_CODE.16_STEP	
	CMP #$17
	BEQ .SPELL_CODE.17_STEP	
	CMP #$18
	BEQ .SPELL_CODE.18_STEP	
	CMP #$19
	BEQ .SPELL_CODE.19_STEP	
	CMP #$1A
	BEQ .SPELL_CODE.1A_STEP	
	CMP #$1B
	BEQ .SPELL_CODE.1B_STEP	
	CMP #$1C
	BEQ .SPELL_CODE.1C_STEP			
	CMP #$1D
	BEQ .SPELL_CODE.1D_STEP	
	
	;**insert $1C-$2F**
	
	CMP #$30
	BEQ .SPELL_CODE.30_STEP	
	
	;**FALLS THROUGH**
	
.ERROR.INVALID.SPELL_CODE
;.GET.SPELL_CODE (COMBAT.COMMAND.CAST) reports invalid spell code in PLAYER.HOTKEYS.SPELL,X
	JSR PREP.BRK
	BRK

.SPELL_CODE.00_STEP	;MAGIC MISSLE
	JMP .SPELL_CODE.00	;MAGIC MISSLE

.SPELL_CODE.01_STEP ;FLAME ARROW
	JMP .SPELL_CODE.01 ;FLAME ARROW

.SPELL_CODE.02_STEP ;FIREBALL (small)
	JMP .SPELL_CODE.02 ;FIREBALL (small)

.SPELL_CODE.03_STEP ;FIREBALL (medium)
	JMP .SPELL_CODE.03 ;FIREBALL (medium)

.SPELL_CODE.04_STEP ;FIREBALL (large)
	JMP .SPELL_CODE.04 ;FIREBALL (large)

.SPELL_CODE.05_STEP ;INFERNAL BLAST
	JMP .SPELL_CODE.05 ;INFERNAL BLAST

.SPELL_CODE.06_STEP ;LIGHTNING BOLT
	JMP .SPELL_CODE.06 ;LIGHTNING BOLT

.SPELL_CODE.07_STEP	;LIGHTNING
	JMP .SPELL_CODE.07
	
.SPELL_CODE.08_STEP ;LIGHTNING MEGA_BLAST
	JMP .SPELL_CODE.08
		
.SPELL_CODE.09_STEP	;CHAIN_LIGHTNING (medium)
	JMP .SPELL_CODE.09
	
.SPELL_CODE.0A_STEP	;CHAIN_LIGHTNING (large)
	JMP .SPELL_CODE.0A
		
.SPELL_CODE.0B_STEP ;DIE
	JMP .SPELL_CODE.0B

.SPELL_CODE.0C_STEP	;MASS_DEATH
	JMP .SPELL_CODE.0C

.SPELL_CODE.0E_STEP	;SUMMON: LESSER UNDEAD
	JMP .SPELL_CODE.0E

.SPELL_CODE.12_STEP ;SUMMON: ARMGDN.COW	
	JMP .SPELL_CODE.12
	
.SPELL_CODE.13_STEP ;SUMMON.DEMON_LORD
	JMP .SPELL_CODE.13

.SPELL_CODE.14_STEP ;SMITE: SWORD
	JMP .SPELL_CODE.14
	
.SPELL_CODE.15_STEP ;MASS SMITE: SWORD
	JMP .SPELL_CODE.15
	
.SPELL_CODE.16_STEP ;SMITE: AXE
	JMP .SPELL_CODE.16

.SPELL_CODE.17_STEP ;ED'S AXE OF ANGUISH
	JMP .SPELL_CODE.17

.SPELL_CODE.18_STEP ;SMITE: BOULDER
	JMP .SPELL_CODE.18
	
.SPELL_CODE.19_STEP ;MASS SMITE: BOULDER
	JMP .SPELL_CODE.19

.SPELL_CODE.1A_STEP	;SHOCK SPHERE
	JMP .SPELL_CODE.1A
	
.SPELL_CODE.1B_STEP	;MASS SHOCK SPHERE (small)
	JMP .SPELL_CODE.1B

.SPELL_CODE.1C_STEP	;MASS SHOCK SPHERE (medium)
	JMP .SPELL_CODE.1C

.SPELL_CODE.1D_STEP	;MASS SHOCK SPHERE (large)
	JMP .SPELL_CODE.1D

.SPELL_CODE.30_STEP
	JMP .SPELL_CODE.30
	
.SPELL_CODE.00	;MAGIC MISSLE
		
	;print spell name (code $0)	
		LDA #COMBAT.TEXT_BLOCK.SPELL_NAME.0	
		STA TWF.STRING
		
		LDA /COMBAT.TEXT_BLOCK.SPELL_NAME.0
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW

	
	JMP GE.SPELL.MAGIC_MISSLE.ENTRANCE

		;**OPT** Memory. Disk Speed. Replace the TWF.STRING(2) set in each section with common code that loads the memory address from a lookup table (use spell code as index), which is seeded by SBASM directives
.SPELL_CODE.01 ;FLAME ARROW
	;print spell name (code $1)	
		LDA #COMBAT.TEXT_BLOCK.SPELL_NAME.1
		STA TWF.STRING
		
		LDA /COMBAT.TEXT_BLOCK.SPELL_NAME.1
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW
	
	JMP GE.SPELL.FLAME_ARROW.ENTRANCE

.SPELL_CODE.02 ;FIREBALL (small)
	;print spell name (code $2)	
		LDA #COMBAT.TEXT_BLOCK.SPELL_NAME.2
		STA TWF.STRING
		
		LDA /COMBAT.TEXT_BLOCK.SPELL_NAME.2
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW
	
		LDA #$01 ;parameter: radius 
	JMP GE.SPELL.FIREBALL.ENTRANCE

.SPELL_CODE.03 ;FIREBALL (medium)
	;print spell name (code $3)	
		LDA #COMBAT.TEXT_BLOCK.SPELL_NAME.3	
		STA TWF.STRING
		
		LDA /COMBAT.TEXT_BLOCK.SPELL_NAME.3
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW
	
		LDA #$02 ;parameter: radius 
	JMP GE.SPELL.FIREBALL.ENTRANCE
	
.SPELL_CODE.04 ;FIREBALL (large)
	;print spell name (code $4)	
		LDA #COMBAT.TEXT_BLOCK.SPELL_NAME.4	
		STA TWF.STRING
		
		LDA /COMBAT.TEXT_BLOCK.SPELL_NAME.4
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW
	
		LDA #$03 ;parameter: radius 
	JMP GE.SPELL.FIREBALL.ENTRANCE

.SPELL_CODE.05 ;INFERNAL BLAST
	;print spell name (code $5)	
		LDA #COMBAT.TEXT_BLOCK.SPELL_NAME.5
		STA TWF.STRING
		
		LDA /COMBAT.TEXT_BLOCK.SPELL_NAME.5
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW
	
	JMP GE.SPELL.LIGHTNING.ENTRANCE

.SPELL_CODE.06 ;LIGHTNING BOLT
	;print spell name (code $6)	
		LDA #COMBAT.TEXT_BLOCK.SPELL_NAME.6	
		STA TWF.STRING
		
		LDA /COMBAT.TEXT_BLOCK.SPELL_NAME.6
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW
	
	JMP GE.SPELL.LIGHTNING.ENTRANCE

.SPELL_CODE.07	;LIGHTNING
	;print spell name (code $7)	
		LDA #COMBAT.TEXT_BLOCK.SPELL_NAME.7
		STA TWF.STRING
		
		LDA /COMBAT.TEXT_BLOCK.SPELL_NAME.7
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW
	
	JMP GE.SPELL.LIGHTNING.ENTRANCE

.SPELL_CODE.08 ;LIGHTNING MEGA_BLAST	;print spell name (code $8)	
		LDA #COMBAT.TEXT_BLOCK.SPELL_NAME.8	
		STA TWF.STRING
		
		LDA /COMBAT.TEXT_BLOCK.SPELL_NAME.8
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW
	
	JMP GE.SPELL.LIGHTNING.ENTRANCE
		;**OPT** Memory. for all spells, use a greater than / less than test to route all spell codes to the same section for calling the lightning entrance routine.

.SPELL_CODE.09	;CHAIN_LIGHTNING (medium)
	;print spell name (code $9)	
		LDA #COMBAT.TEXT_BLOCK.SPELL_NAME.9
		STA TWF.STRING
		
		LDA /COMBAT.TEXT_BLOCK.SPELL_NAME.9
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW
	
	JMP GE.SPELL.LIGHTNING.ENTRANCE

.SPELL_CODE.0A	;CHAIN_LIGHTNING (large)
	;print spell name (code $A)	
		LDA #COMBAT.TEXT_BLOCK.SPELL_NAME.A
		STA TWF.STRING
		
		LDA /COMBAT.TEXT_BLOCK.SPELL_NAME.A
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW
	
	JMP GE.SPELL.LIGHTNING.ENTRANCE

.SPELL_CODE.0B ;DIE
	;print spell name (code $B)	
		LDA #COMBAT.TEXT_BLOCK.SPELL_NAME.B
		STA TWF.STRING
		
		LDA /COMBAT.TEXT_BLOCK.SPELL_NAME.B
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW
	
	JMP GE.SPELL.DIE.ENTRANCE

.SPELL_CODE.0C	;MASS_DEATH
	;print spell name (code $C)	
		LDA #COMBAT.TEXT_BLOCK.SPELL_NAME.C	
		STA TWF.STRING
		
		LDA /COMBAT.TEXT_BLOCK.SPELL_NAME.C
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW
	
	JMP GE.SPELL.MASS_DEATH.ENTRANCE

.SPELL_CODE.0E ;SUMMON: LESSER UNDEAD
	;print spell name (code $0E)	
		LDA #COMBAT.TEXT_BLOCK.SPELL_NAME.E	
		STA TWF.STRING
		
		LDA /COMBAT.TEXT_BLOCK.SPELL_NAME.E
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW
	
		LDA #$01 ;($00 = armagedon by the cows | $01 = summon standard routine)
	JMP GE.SPELL.SUMMON.ENTRANCE
	
.SPELL_CODE.12 ;ARMGDN.COW	
	;print spell name (code $12)	
		LDA #COMBAT.TEXT_BLOCK.SPELL_NAME.12
		STA TWF.STRING
		
		LDA /COMBAT.TEXT_BLOCK.SPELL_NAME.12
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW
	
		LDA #$00 ;($00 = armagedon by the cows | $01 = summon standard routine)
	JMP GE.SPELL.SUMMON.ENTRANCE
	
.SPELL_CODE.13 ;SUMMON.DEMON_LORD
	;print spell name (code $13)	
		LDA #COMBAT.TEXT_BLOCK.SPELL_NAME.13	
		STA TWF.STRING
		
		LDA /COMBAT.TEXT_BLOCK.SPELL_NAME.13
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW
	
		LDA #$01 ;($00 = armagedon by the cows | $01 = summon standard routine)
	JMP GE.SPELL.SUMMON.ENTRANCE

.SPELL_CODE.14 ;SMITE: SWORD
	;set single-target
	LDA #$00
	STA SPELL.SMITE.MULTI_TARGET.FLAG	;($00 = single target | $01 = multi-target)

	;print spell name (code $14)	
		LDA #COMBAT.TEXT_BLOCK.SPELL_NAME.14	
		STA TWF.STRING
		
		LDA /COMBAT.TEXT_BLOCK.SPELL_NAME.14
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW
	
	JMP GE.SPELL.SMITE.ENTRANCE

.SPELL_CODE.15 ;MASS SMITE: SWORD
	;set multi-target
	LDA #$01
	STA SPELL.SMITE.MULTI_TARGET.FLAG	;($00 = single target | $01 = multi-target)

	
	;print spell name (code $15)	
		LDA #COMBAT.TEXT_BLOCK.SPELL_NAME.15	
		STA TWF.STRING
		
		LDA /COMBAT.TEXT_BLOCK.SPELL_NAME.15
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW
	
	JMP GE.SPELL.SMITE.ENTRANCE
	
.SPELL_CODE.16 ;SMITE: AXE
	;set single-target
	LDA #$00
	STA SPELL.SMITE.MULTI_TARGET.FLAG	;($00 = single target | $01 = multi-target)

	;print spell name (code $16)	
		LDA #COMBAT.TEXT_BLOCK.SPELL_NAME.16	
		STA TWF.STRING
		
		LDA /COMBAT.TEXT_BLOCK.SPELL_NAME.16
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW
	
	JMP GE.SPELL.SMITE.ENTRANCE
	
.SPELL_CODE.17  ;ED'S AXE OF ANGUISH
	;set multi-target
	LDA #$01
	STA SPELL.SMITE.MULTI_TARGET.FLAG	;($00 = single target | $01 = multi-target)

	;print spell name (code $17)	
		LDA #COMBAT.TEXT_BLOCK.SPELL_NAME.17	
		STA TWF.STRING
		
		LDA /COMBAT.TEXT_BLOCK.SPELL_NAME.17
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW
	
	JMP GE.SPELL.SMITE.ENTRANCE	
	
.SPELL_CODE.18 ;SMITE: BOULDER
	;set single-target
	LDA #$00
	STA SPELL.SMITE.MULTI_TARGET.FLAG	;($00 = single target | $01 = multi-target)

	;print spell name (code $15)	
		LDA #COMBAT.TEXT_BLOCK.SPELL_NAME.18	
		STA TWF.STRING
		
		LDA /COMBAT.TEXT_BLOCK.SPELL_NAME.18
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW
	
	JMP GE.SPELL.SMITE.ENTRANCE
	
.SPELL_CODE.19 ;MASS SMITE: BOULDER
	;set multi-target
	LDA #$01
	STA SPELL.SMITE.MULTI_TARGET.FLAG	;($00 = single target | $01 = multi-target)

	;print spell name (code $19)	
		LDA #COMBAT.TEXT_BLOCK.SPELL_NAME.19
		STA TWF.STRING
		
		LDA /COMBAT.TEXT_BLOCK.SPELL_NAME.19
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW
	
	JMP GE.SPELL.SMITE.ENTRANCE

.SPELL_CODE.1A	;SHOCK SPHERE
	;print spell name (code $1A)	
		LDA #COMBAT.TEXT_BLOCK.SPELL_NAME.1A
		STA TWF.STRING
		
		LDA /COMBAT.TEXT_BLOCK.SPELL_NAME.1A
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW
	
	JMP GE.SPELL.SHOCK_SPHERE.ENTRANCE	

.SPELL_CODE.1B	;MASS SHOCK SPHERE (small)
	;print spell name (code $1B)	
		LDA #COMBAT.TEXT_BLOCK.SPELL_NAME.1B
		STA TWF.STRING
		
		LDA /COMBAT.TEXT_BLOCK.SPELL_NAME.1B
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW
	
		LDA #$01 ;parameter: radius 
	JMP GE.SPELL.MASS_SHOCK_SPHERE.ENTRANCE

.SPELL_CODE.1C	;MASS SHOCK SPHERE (medium)
	;print spell name (code $1C)	
		LDA #COMBAT.TEXT_BLOCK.SPELL_NAME.1C
		STA TWF.STRING
		
		LDA /COMBAT.TEXT_BLOCK.SPELL_NAME.1C
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW
	
		LDA #$02 ;parameter: radius 
	JMP GE.SPELL.MASS_SHOCK_SPHERE.ENTRANCE

	
	
.SPELL_CODE.1D	;MASS SHOCK SPHERE (large)
	;print spell name (code $1D)	
		LDA #COMBAT.TEXT_BLOCK.SPELL_NAME.1D
		STA TWF.STRING
		
		LDA /COMBAT.TEXT_BLOCK.SPELL_NAME.1D
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW
	
		LDA #$03 ;parameter: radius 
	JMP GE.SPELL.MASS_SHOCK_SPHERE.ENTRANCE
	

	
.SPELL_CODE.30
	;print spell name (code $30)	
		LDA #COMBAT.TEXT_BLOCK.SPELL_NAME.30
		STA TWF.STRING
		
		LDA /COMBAT.TEXT_BLOCK.SPELL_NAME.30
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW
	
	JMP GE.SPELL.COMBAT_TELEPORT_PARTY.ENTRANCE


	
@END

@END
	
;CAST_SPELL.EXIT
	;**see SWAP.ROUTINES.Spell_File.ASM** (it resides in spell file entrance routine)

;TEXT BLOCKS
@START	


; ;=====SPELL NAMES=====
COMBAT.TEXT_BLOCK.SPELL_NAME.0		.AZ	-#$8D,/Magica Arundinis/,#$8D
COMBAT.TEXT_BLOCK.SPELL_NAME.1		.AZ	-#$8D,/Flamma Arundinis/,#$8D
COMBAT.TEXT_BLOCK.SPELL_NAME.2		.AZ	-#$8D,/Simpuli Pluralitas Flamma/,#$8D
COMBAT.TEXT_BLOCK.SPELL_NAME.3		.AZ	-#$8D,/Intermedia Pluralitas Flamma/,#$8D
COMBAT.TEXT_BLOCK.SPELL_NAME.4		.AZ	-#$8D,/Laxatus Pluralitas Flamma/,#$8D
COMBAT.TEXT_BLOCK.SPELL_NAME.5		.AZ	-#$8D,/Flamma Bucinum/,#$8D
COMBAT.TEXT_BLOCK.SPELL_NAME.6		.AZ	-#$8D,/Simpuli Electrium/,#$8D
COMBAT.TEXT_BLOCK.SPELL_NAME.7		.AZ	-#$8D,/Intermedia Electrium/,#$8D
COMBAT.TEXT_BLOCK.SPELL_NAME.8		.AZ	-#$8D,/Laxatus Electrium/,#$8D
COMBAT.TEXT_BLOCK.SPELL_NAME.9		.AZ	-#$8D,/Intermedia Pluralitas Electrium/,#$8D
COMBAT.TEXT_BLOCK.SPELL_NAME.A		.AZ	-#$8D,/Laxatus Pluralitas Electrium/,#$8D
COMBAT.TEXT_BLOCK.SPELL_NAME.B		.AZ	-#$8D,/Necatus/,#$8D
COMBAT.TEXT_BLOCK.SPELL_NAME.C		.AZ	-#$8D,/Pluralitas Necatus/,#$8D
COMBAT.TEXT_BLOCK.SPELL_NAME.E		.AZ	-#$8D,/Summonitus Simpuli Mortuus/,#$8D
COMBAT.TEXT_BLOCK.SPELL_NAME.12		.AZ	-#$8D,/Summonitus La Vache/,#$8D
COMBAT.TEXT_BLOCK.SPELL_NAME.13		.AZ	-#$8D,/Summonitus Damonis/,#$8D
COMBAT.TEXT_BLOCK.SPELL_NAME.14		.AZ	-#$8D,/NOT NAMED YET/,#$8D
COMBAT.TEXT_BLOCK.SPELL_NAME.15		.AZ	-#$8D,/Pluralitas Ictus Ferrum/,#$8D
COMBAT.TEXT_BLOCK.SPELL_NAME.16		.AZ	-#$8D,/NOT NAMED YET/,#$8D
COMBAT.TEXT_BLOCK.SPELL_NAME.17		.AZ	-#$8D,/NOT NAMED YET/,#$8D
COMBAT.TEXT_BLOCK.SPELL_NAME.18		.AZ	-#$8D,/NOT NAMED YET/,#$8D
COMBAT.TEXT_BLOCK.SPELL_NAME.19		.AZ	-#$8D,/NOT NAMED YET/,#$8D
COMBAT.TEXT_BLOCK.SPELL_NAME.1A		.AZ	-#$8D,/NOT NAMED YET/,#$8D
COMBAT.TEXT_BLOCK.SPELL_NAME.1B		.AZ	-#$8D,/Simpuli Pluralitas Electrium/,#$8D
COMBAT.TEXT_BLOCK.SPELL_NAME.1C		.AZ	-#$8D,/Intermedia Pluralitas Electrium/,#$8D
COMBAT.TEXT_BLOCK.SPELL_NAME.1D		.AZ	-#$8D,/Laxatus Pluralitas Electrium/,#$8D
COMBAT.TEXT_BLOCK.SPELL_NAME.30		.AZ	-#$8D,/Simpuli Traversia/,#$8D



	; LDA COMBAT.STATS.SPELL.KILL_FLAG ;death magic attack: ($00 = succeeded | $01 = not attempted | $03 = failed).
	; BEQ .PRINT.DEATH_MAGIC.SUCCESS
	; CMP #$03
	; BNE .PRINT.DEATH_MAGIC.FAIL

	
;COMBAT_TEXT_BLOCK.ANY_KEY1	.AZ -#$8D, #$8D,/<Press any key>/ ;ASCII text string	

@END

;LOCAL VARIABLES
@START

;demo
;COMBAT.STATS.SPELL_TABLE		.HS	0A.03.1E.0C.0F.13.19.21.2D.3E.50.24.32.16.5A.29.6E.33.23.30.37.4C.7F.3C.7F.69.00.00.00.00.00.00.00.00.00.00.00.5B.00.62

;normal
;COMBAT.STATS.SPELL_TABLE		.HS	0A.03.1E.0C.0F.13.19.21.2D.3E.50.24.32.16.5A.29.6E.33.23.30.37.4C.7F.3C.7F.69.00.00.00.0A.00.00.00.00.00.00.00.5B.00.62.00.01.7F.50.00.01.00.01.00.01.00.01.50.24.0F.13.19.21.2D.3E.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.14

;kfest demo
COMBAT.STATS.SPELL_TABLE	.HS	0A.03.1E.0C.0F.13.19.21.2D.3E.50.24.32.16.5A.29.6E.33.23.30.37.4C.7F.3C.7F.69.00.00.00.0A.00.00.00.00.00.00.00.5B.00.62.00.01.E8.50.00.01.00.01.00.01.00.01.50.24.0F.13.19.21.2D.3E.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.14

@END



			;**OPT** Memory. Disk Speed. lower this .NO to the minimum needed.
	.NO CAST_SPELL.ENTRANCE+$600 ;detect end of memory reserved for main program code 
			;***WARNING: before increasing this .NO, first you must move SRTN.CAST_SPELL.FILE_ADDRESS lower in memory by the same amount.
			; ;**increase by one block (2 pages) at a time to minimize disk space
			
	;.NO $BD00 ;maximum value for above
				