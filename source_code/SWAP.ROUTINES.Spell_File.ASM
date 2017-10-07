; ============================================
; Copyright (C) 2016-2018. 6502 Workshop, LLC
; contact: mark@6502workshop.com
; ============================================


;************************INCLUDE FILE*****************************
;(do not assemble stand-alone.  )
				.TF     swap.routines.spell_file.bin,BIN
				.OR		SRTN.SPELL_FILE.ADDRESS		**Always put before .TF directive and never use again in program
				.EB		OFF


;=====SWAP.ROUTINES.COMBAT.SPELL.ASM DOCUMENTATION====================================
;
;CAST SPELL FLOW CONTROL (**see COMMAND.CAST_SPELL subroutine docs**)
;
;-Shape Data
;*Non-angle shapes (animated):
;	Shape data is stored in the code block for the spell in this file. It is loaded in to the shape hopper within the code block. DRAW.TILE.SINGLE is called.
;	with the parameter set that prevents shape hopper from being clobbered by loading shape data from aux memory based on tile_ID
;
;*Angled shapes (non-animated)
;	Shape data is currently loaded into aux memory by LOADER.P.ASM and SELECT.SHAPE.TABLE (COMBAT.FIRE.PROJECTILE) reads the shape data from aux memory
;	and populates the shape hopper. 
;	NOTE: this may change in the future so that the shape data is stored in the code block for this spell in this file.
;
;-Drawing Projectile on Targets
;	*see .DRAW.PROJECTILE.ON.TARGET in the main combat file	
;
;-Target hits
;
;*Each code block in this spell file populates COMBAT.TARGET_HIT.DB with the screen index (screen tile location) 
;of any targets hit, using $FF as the stop value. The method in which this array is populated varies. Some use their 
;own copy of the subroutine .TARGET.HIT.CHECK and some integrate the target hit database update into other routines unique
;to that spell. See documentation at the top of each spell code block for more details on the method used for that particular spell. 
;
;
;A fixed number of bytes is allocated for SPELL.FILE.CODE_BLOCK.BUFFER.START. There is   
;a size test in SPELL_FILE.LOAD.CODE_BLOCK that triggers an error trap if the spell code block 
;being loaded is larger than the spell code bloack buffer. 
;
;Spell file offsets: see end of this file
;
;-Layout of this file
;start/end markers are outside the patch sections so that their addresses will
;be sequential. Thus, subtraction can be used to calculate the number of bytes between
;the start of the spell file and the start of a particular spell. This will be the seek value
;for ProRWTS. 
;
;And, subtraction can be used to calculate the number of bytes between the
;start of a spell code block and the end of that spell code block. This will be the read length for ProRWTS
;
;
;-Increase/Decrease size of Spell File buffer
;
;*COMBAT.MAIN.MODULE.END: modifies the size of the spell code block buffer.
;*SPELL.FILE.ENTRANCE.BUFFER.END: modifies the size of the spell file entrance buffer and has the opposite effect on the spell code block buffer. 
;
;These are the only two constants that need to be changed, and only one of them needs to be changed depending on which buffer
;you want to change the size of. All other constants and assembler directives related to the buffer sizes are derived from these constants. 
;there are a few code comments that record the memory address associated with various related constants for refernece, so I usually update those as well. 
;Search for ";PARAMETERS FOR FILE MANAGEMENT" in offloaded_variables.ASM, in the combat/spell file section for most of them. 
;
;
;-Spell Damage / MP Cost
;Spell damage amount and MP cost is retreived from lookup table (COMBAT.STATS.SPELL_TABLE*) with spell code index in CAST_SPELL.SELECT_SPELL. This routine
;also verifies player has enough MP to cast the spell before passing control to the spell's entrance routine. 
;Spell damage is applied in .APPLY.SPELL_DAMAGE	(SPELL_FILE.AFTER_CAST)
;*COMBAT.STATS.SPELL_TABLE is stored in this file.
;
;MP cost is deducted (with abort if not enough) in .DEDUCT.MP.COST (CAST_SPELL.SELECT_SPELL), 
;which is called by the entrance routine for each spell. 
;
;
;-Spell Text (scroll window)
;
;For hit/damage text see .APPLY.SPELL_DAMAGE (SPELL_FILE.AFTER_CAST)
;For spell name text see CAST_SPELL.SELECT_SPELL
;
;-Flags
;Critical hit ENGAGED index: in each spell code block
;Auto-kill flag (for death spells): in each spell code block
;
;
;-----TARGET SELECTION ROUTINE--- ****OLD. N/A******(**special memory management**)
@START
;****OLD. N/A. 
;(I decided this was too much hassle and remove the spell file version of the target select routine. Instead I moved
;the attack target select routine into the portion of memory in the combat module that isn't clobbered by the spell file. 
;Spells and atack now use the same target select routine)
;
;There is a separate target selection routine for
;the (A)ttack and the (C)ast command. The attack version is in the SWAP.ROUTINES.Combat.ASM source file
;and the cast version is in the SWAP.ROUTINES.Combat.spells.ASM source file. 
;
;They are identical
;but keeping them separate results in a potentially larger spell code buffer because
;the code for the attack version is stored in the main combat program, which is clobbered
;by the spell file.
;
;The spell file buffer is comprised of the entrance buffer and the spell code buffer.
;The cast version of the target select routine is loaded into the entrace buffer along with other
;code which determins which spell code block is needed. The taret selection routine is loaded at the end
;of the entrance buffer. Accordingly, the spell code block could be expanded in side to clobber the part
;of the entrance buffer which contains the target selection code. This is currently not being done, it is 
;an option for future expansion if needed. 
;
;
;----UPDATING THE TARGET SELECT ROUTINE----
;
;In general the procedure is to replace COMBAT.SELECT.ATTACK_TARGET with
;COMBAT.SELECT.SPELL_ATTACK (assuming the attack version was updated),
;but there is a little more to it. 
;
;Here are the specific steps:
;
;*Before doing the replacement
;	*open the code block for COMBAT.SELECT.ATTACK_TARGET
;   and copy the COMBAT.SPELL.ACQUIRE.TARGET code block to a temp file. 
;  *copy .DRAW.SINGLE.OFF_GRID to a temp file
;
;*delete COMBAT.SELECT.ATTACK_TARGET
;*replace with COMBAT.SELECT.ATTACK_TARGET
;*rename code block back to COMBAT.SELECT.ATTACK_TARGET
;*In COMBAT.SELECT.ATTACK_TARGET, change JSR COMBAT.ACQUIRE.TARGET to JSR COMBAT.SPELL.ACQUIRE.TARGET
;*Copy COMBAT.SPELL.ACQUIRE.TARGET from the temp file back into the end of the COMBAT.SELECT.ATTACK_TARGET code block (it must be the very end because it starts with a parent label)
;*copy .DRAW.SINGLE.OFF_GRID from the temp file back into COMBAT.SELECT.ATTACK_TARGET code block
;*replace JSR COMBAT.DRAW.SINGLE.OFF_GRID with JSR DRAW.SINGLE.OFF_GRID
;*(NEW) Make sure .EXIT in the top level routine in COMBAT.SELECT.ATTACK_TARGET contains the following: (i'm not sure how to best integrate it into the steps above)
;		;JSR SPELL_FILE.COMMON.ENTRANCE_CODE
;*(NEW) .WEAPON.RADIUS.CHECK added to COMBAT.SELECT.ATTACK_TARGET. it shouldn't hurt to leave it in COMBAT.SELECT.ATTACK_TARGET but if there are problems, this may be a place to look.   
;		;this routine was to double check the radius before a last target entry is used since the spell routine could set a last target that exceeds the player's weapon radius
;	
@END
;
;=================================================================================

	;must come before SPELL_FILE.START
	NOP	;this is a filler byte read in to open the file, before the seek to advance to the position in the file 
			;where the code block for the desired spell starts. 	

SPELL_FILE.START


SPELL_FILE.ENTRANCE_ROUTINE			
@START
	
SPELL_FILE.AFTER_CAST
@START
;=====================SUBROUTINE DOCUMENTATION====================================
;
;This routine applies spell damage to the targets hit. This is done by iterating
;through the target database and feeding the screen index of each target into
;COMBAT.DAMAGE.ROLL. The scroll window text assciated with hit/damage is also printed
;in that loop
;
;=================================================================================



.SPELL.GRAPHICS.COMPLETE				
	; ;make sure defender name and "select target" text are on different lines
	JSR PRINT.TEXT.WINDOW.CR ;print carriage return to combat scroll window


		
				; LDA #$AA
				; LDX #COW.TEXT
				; LDY /COW.TEXT
				; JSR PREP.BRK
				; BRK
				
		; ;LDA #COMBAT.TEXT_BLOCK.SELECT_TARGET
		; LDA #COW.TEXT		
		; STA TWF.STRING+$0
		
		; ;LDA /COMBAT.TEXT_BLOCK.SELECT_TARGET
		; LDA /COW.TEXT
		; STA TWF.STRING+$1
	; JSR PRINT.TEXT.WINDOW


			;jsr keyin

	

.DRAW.PROJECTILE.ON.TARGET
@START
;VALIDATE ENTRANCE	
	;skip if spell cast was aborted
	LDA COMBAT.ABORT.FLAG		;($00 = not set, $01 = set)
	BNE .DRAW.PROJECTILE.ON.TARGET.DONE	;if flag set then don't cast spell

	;skip if smite spell is being cast
	;(SMITE spell has the projectile fire integrated with .APPLY.SPELL_DAMAGE below because displaying the target hit is part of the graphics effects for the spell.
	;For some reason letting the code below run for the smite spell causes tile sized graphics artifacts. I think it is because the parms for COMBAT.DRAW.SHAPE are
	;not setup for the last target as they would be with a single-target spell. However, that implies the code below should be skipped for other multi-target spells such as
	;chain lightning and I'm not sure how that's managed. maybe the target from the perspective of this routine is mob selected with the target selector icon only)	
	LDA SPELL.SMITE.ACTIVE.FLAG	 ;($00 = SMITE not active | >=$01 = SMITE active)
	BNE .DRAW.PROJECTILE.ON.TARGET.DONE	;if flag set then don't cast spell

;START	
	LDA COMBAT.TARGET_HIT.DB+$0 ;read 1st byte of target database
	CMP #COMBAT.TARGET_HIT.DB.STOP_VALUE ;if 1st byte is set to stop value then no targets were hit. this occurs with spells like summoning. 
	BEQ .DRAW.PROJECTILE.ON.TARGET.DONE ;branch if no targets were hit by the spell that was cast
	
	;refresh shape table to make sure its are in the correct color positions. No way of knowing the position it was left in after the last draw in COMBAT.SHAPE.MOVE 
	JSR CONVERT.MASTER.SHAPE_TABLE

		DEC SHAPE.SBYTE.TARGET.ORIGINAL
		LDA SHAPE.SBYTE.TARGET.ORIGINAL
		STA SHAPE.DRAW.CURRENT.SBYTE
		
		LDA SHAPE.LINE.TARGET
		STA SHAPE.DRAW.LINE_ROW.NUMBER
		;STA TILE.LINE.TARGET

		;foreground: draw shape in current position
		LDA #$01
		STA PAGE.FOREGROUND.OVERRIDE
		LDA #$00 ;set draw mode
	JSR COMBAT.DRAW.SHAPE
	
		; ;JSR KEYIN	
		; LDA #$20
		; JSR WAIT.LOOP

; ;ERASE THE PROJECTILE FROM TARGET

		; ;foreground: draw shape in current position
		; LDA #$01
		; STA PAGE.FOREGROUND.OVERRIDE
		; LDA #$01 ;set draw mode
	; JSR COMBAT.DRAW.SHAPE

.DRAW.PROJECTILE.ON.TARGET.DONE

@END
	

			
.APPLY.SPELL_DAMAGE	
@START	
;APPLY SPELL DAMAGE TO TARGETS HIT

;**OPT** Memory. If the damage roll routine gets moved to aux memory, move
;this routine to damage roll

			
	;skip non-damage spells (i.e. summon, healing, etc)
	;and spells where no targets where hit (i.e. mass death if it is cast in a direction with no targets)

	LDA COMBAT.TARGET_HIT.DB+$0
	CMP #COMBAT.TARGET_HIT.DB.STOP_VALUE					
	;BEQ .APPLY.SPELL_DAMAGE.LOOP.DONE ;if no targets hit, skip damage roll and stats display update
	BNE .COW
	JMP .APPLY.SPELL_DAMAGE.LOOP.DONE ;if no targets hit, skip damage roll and stats display update
.COW
		
	LDA #$02
	STA COMBAT.STATS.DAMAGE.TYPE ;($00 = melee | $01 range, $02 spell. High-bit not set = non-magic weapon (doesn't apply to type=spell or MOBs) | high-bit set = magic weapon (doesn't apply to type=spell or MOBs)	
	LDX #$00
.APPLY.SPELL_DAMAGE.LOOP
;DISPLAY HIT

		;were there targets hit?		
		LDA COMBAT.TARGET_HIT.DB,X
		CMP #COMBAT.TARGET_HIT.DB.STOP_VALUE					
		BEQ .APPLY.SPELL_DAMAGE.LOOP.DONE ;if no targets left then exit damage loop
		PHA ;push next target sindex to stack
		STA COMBAT.DEFENDER.SINDEX

		;update scroll window
		CPX #$00
		BEQ .PRINT.CR.DONE ;skip blank line because there was already one inserted after select target or after "casts spell!" for mobs (though I'm not sure how)
		
		;print blank line between last cast spell outcome and next defender name
		JSR PRINT.TEXT.WINDOW.CR ;print carriage return to combat scroll window
.PRINT.CR.DONE

			CLC ;set parm = use defender SINDEX (COMBAT.PRINT.DEFENDER.NAME) 
		JSR COMBAT.PRINT.DEFENDER.NAME

		;fire projectile for certain spells
		;(e.g. SMITE spell has the projectile fire integrated with the apply damage routine because displaying the target hit is part of the graphics effects for the spell)
		LDA SPELL.SMITE.ACTIVE.FLAG	 ;($00 = SMITE not active | >=$01 = SMITE active)
		BEQ .CALL.DAMAGE.ROLL
		
				; LDA #$AA
				; LDX #COMBAT.TARGET_HIT.DB
				; LDY /COMBAT.TARGET_HIT.DB
				; JSR PREP.BRK
				; BRK
				
		JSR SPELL.SMITE.FIRE_PROJECTILE
		
.CALL.DAMAGE.ROLL
				; LDA #$AB
				; LDX #COMBAT.TARGET_HIT.DB
				; LDY /COMBAT.TARGET_HIT.DB
				; JSR PREP.BRK
				; BRK
						
		LDY COMBAT.ATTACKER.SINDEX ;load attacker sindex				
	JSR COMBAT.DAMAGE.ROLL
	
.UPDATE.SCROLL_WINDOW
	; ;make sure defender name and cast spell outcome are on different lines
	; JSR PRINT.TEXT.WINDOW.CR ;print carriage return to combat scroll window

	;select text block to print
	LDA COMBAT.STATS.SPELL.KILL_FLAG ;death magic attack: ($00 = succeeded | $01 = not attempted | $03 = failed).
	BEQ .PRINT.DEATH_MAGIC.SUCCEEDED
	CMP #$03
	BEQ .PRINT.DEATH_MAGIC.FAILED
	LDA COMBAT.STATS.HIT_MISS_KILL.FLAG ;($00 = hit | $01 = miss | $02 = kill)
	CMP #$02
	BEQ .PRINT.KILLED	
	;DEFAULT CASE: HIT
	
;.PRINT_HIT	
	; ;make sure hit prints on line after defender name	
	; JSR PRINT.TEXT.WINDOW.CR ;print carriage return to combat scroll window
	JSR COMBAT.PRINT.HIT
	JSR DISPLAY.MOB.HP_STATUS_BAR	
	JMP .DISPLAY.COMBAT.MATH

.PRINT.KILLED
		LDA #COMBAT.TEXT_BLOCK.KILLED					
		STA TWF.STRING+$0
		
		LDA /COMBAT.TEXT_BLOCK.KILLED
		STA TWF.STRING+$1		
	JMP .EXECUTE.PRINT
	
.PRINT.DEATH_MAGIC.SUCCEEDED
		LDA #COMBAT.TEXT_BLOCK.DEATH_MAGIC.SUCCEEDED					
		STA TWF.STRING+$0
		
		LDA /COMBAT.TEXT_BLOCK.DEATH_MAGIC.SUCCEEDED
		STA TWF.STRING+$1	
	JMP .EXECUTE.PRINT

.PRINT.DEATH_MAGIC.FAILED
		LDA #COMBAT.TEXT_BLOCK.DEATH_MAGIC.FAILED					
		STA TWF.STRING+$0
		
		LDA /COMBAT.TEXT_BLOCK.DEATH_MAGIC.FAILED
		STA TWF.STRING+$1
	
	;**FALLS THROUGH**
	
.EXECUTE.PRINT	
	JSR PRINT.TEXT.WINDOW

.DISPLAY.COMBAT.MATH
	;JSR PRINT.TEXT.WINDOW.CR ;print carriage return to combat scroll window

		;JSR KEYIN
	JSR COMBAT.PRINT.MATH
		;JSR KEYIN
			
.UPDATE.SCROLL_WINDOW.DONE

	;**FALLS THROUGH**
	
	PLA ;pull next target sindex from stack
	LDY COMBAT.STATS.SPELL.KILL_FLAG ;death magic attack: ($00 = succeeded | $01 = not attempted | $03 = failed).
	CPY #$03
	BEQ .DEATH_MAGIC.FAILED
	;$01 (not attempted) should fall through to .STANDARD because not attempted would be the status for a non-death_magic spell

.STANDARD	
		TAY ;transfer defender sindex to Y-REG				
	JSR COMBAT.AUDIO_VISUAL.HIT	
				
	JMP .INCREMENT.INDEX
	
.DEATH_MAGIC.FAILED
	JSR PLAY.COMBAT.ATTACK_MISS.SOUND
	
	;**FALLS THROUGH**
	
.INCREMENT.INDEX
		INX
		JMP .APPLY.SPELL_DAMAGE.LOOP
.APPLY.SPELL_DAMAGE.LOOP.DONE	
@END



.EXIT
	
	JMP CAST_SPELL.EXIT ;(located in spell file entrance routine)
	;RTS ;return to COMBAT.COMMAND.CAST
	
@END


				
;***SPELL ENTRANCE ROUTINES****
@START
SPELL_FILE.LOAD.CODE_BLOCK.ENTRANCE

GE.SPELL.MAGIC_MISSLE.ENTRANCE
@START
;PARAMETERS: none
;ENTRANCE: direct
;RETURN: *targets hit (COMBAT.TARGET_HIT.DB(x)), graphics effects for spell 
;

;LOAD SPELL
		;set parameters
		LDA #GE.SPELL.MAGIC_MISSLE.SEEK_BYTES
		STA PARM.SEEK_BYTES+$0

		LDA /GE.SPELL.MAGIC_MISSLE.SEEK_BYTES
		STA PARM.SEEK_BYTES+$1

		LDA #GE.SPELL.MAGIC_MISSLE.READ_BYTES
		STA PARM.READ_BYTES+$0

		LDA /GE.SPELL.MAGIC_MISSLE.READ_BYTES
		STA PARM.READ_BYTES+$1
	JSR SPELL_FILE.LOAD.CODE_BLOCK

		
;SELECT TARGET
		LDA #$FF ;parm: set radius
		;LDA #$02 ;parm: set radius
	JSR COMBAT.SELECT.ATTACK_TARGET
		LDA COMBAT.ABORT.FLAG		;($00 = not set, $01 = set)
		BNE .EXIT	;if flag set then don't cast spell

;CALL COMMON ENTRANCE CODE
	JSR SPELL_FILE.COMMON.ENTRANCE_CODE
		
;CALL SPELL ROUTINE

		;set target location: as selected by player
		LDA SELECT_TARGET.CURRENT_TARGET.X
		STA FP.TARGET.GMAP.X
		LDA SELECT_TARGET.CURRENT_TARGET.Y
		STA FP.TARGET.GMAP.Y
	JSR GE.SPELL.MAGIC_MISSLE

.EXIT
				
			; lda #$01
			; sta troubleshooting.hook

	
	;jmp SPELL_FILE.AFTER_CAST
	
	RTS
@END

GE.SPELL.FLAME_ARROW.ENTRANCE
@START
;PARAMETERS: none
;ENTRANCE: direct
;RETURN: *targets hit (COMBAT.TARGET_HIT.DB(x)), graphics effects for spell 
;
;* = chain lightning only

	
;LOAD SPELL
		;set parameters
		LDA #GE.SPELL.FLAME_ARROW.SEEK_BYTES
		STA PARM.SEEK_BYTES+$0

		LDA /GE.SPELL.FLAME_ARROW.SEEK_BYTES
		STA PARM.SEEK_BYTES+$1

		LDA #GE.SPELL.FLAME_ARROW.READ_BYTES
		STA PARM.READ_BYTES+$0

		LDA /GE.SPELL.FLAME_ARROW.READ_BYTES
		STA PARM.READ_BYTES+$1
	JSR SPELL_FILE.LOAD.CODE_BLOCK

;SELECT TARGET
		LDA #$FF ;parm: set radius
		;LDA #$02 ;parm: set radius
	JSR COMBAT.SELECT.ATTACK_TARGET
		LDA COMBAT.ABORT.FLAG		;($00 = not set, $01 = set)
		BNE .EXIT	;if flag set then don't casst spell

;CALL COMMON ENTRANCE CODE
	JSR SPELL_FILE.COMMON.ENTRANCE_CODE
		
;CALL SPELL ROUTINE

		;set target location: as selected by player
		LDA SELECT_TARGET.CURRENT_TARGET.X
		STA FP.TARGET.GMAP.X
		LDA SELECT_TARGET.CURRENT_TARGET.Y
		STA FP.TARGET.GMAP.Y
		
		LDA #FLAME_ARROW.SHAPE.START
		STA COMBAT.SHAPE_TABLE.POINTER+$0
		LDA /FLAME_ARROW.SHAPE.START
		STA COMBAT.SHAPE_TABLE.POINTER+$1			
	JSR GE.SPELL.FLAME_ARROW

	;**FALLS THROUGH**

.EXIT	
	RTS
@END

GE.SPELL.FIREBALL.ENTRANCE
@START
;PARAMETERS: ACC (select spell routine)
;ENTRANCE: direct
;RETURN: *targets hit (COMBAT.TARGET_HIT.DB(x)), graphics effects for spell 
;

			
;SAVE PARAMETERS

	;ACC = parm: radius
	;PHA ;save spell radius parameter to stack
	STA SAVED.ACC.LOCAL2 ;save radius parameter
	
;LOAD SPELL
		;set parameters
		LDA #GE.SPELL.FIREBALL.SEEK_BYTES
		STA PARM.SEEK_BYTES+$0

		LDA /GE.SPELL.FIREBALL.SEEK_BYTES
		STA PARM.SEEK_BYTES+$1

		LDA #GE.SPELL.FIREBALL.READ_BYTES
		STA PARM.READ_BYTES+$0

		LDA /GE.SPELL.FIREBALL.READ_BYTES
		STA PARM.READ_BYTES+$1
	
	JSR SPELL_FILE.LOAD.CODE_BLOCK

;SELECT TARGET
		LDA #$FF ;parm: set radius
		;LDA #$02 ;parm: set radius
	JSR COMBAT.SELECT.ATTACK_TARGET
		LDA COMBAT.ABORT.FLAG		;($00 = not set, $01 = set)
		BNE .EXIT	;if flag set then don't casst spell

;CALL COMMON ENTRANCE CODE
	JSR SPELL_FILE.COMMON.ENTRANCE_CODE
		
;CALL SPELL ROUTINE
		;set target location: as selected by player
		LDA SELECT_TARGET.CURRENT_TARGET.X
		STA FP.TARGET.GMAP.X
		LDA SELECT_TARGET.CURRENT_TARGET.Y
		STA FP.TARGET.GMAP.Y	
		
		LDA SAVED.ACC.LOCAL2 ;restore radius parameter
		;PLA ;restore spell radius parameter to stack				
	JSR GE.SPELL.FIREBALL

.EXIT				
		
	RTS
@END

GE.SPELL.LIGHTNING.ENTRANCE
@START
;PARAMETERS: ACC (select spell routine)
;ENTRANCE: direct
;RETURN: *targets hit (COMBAT.TARGET_HIT.DB(x)), graphics effects for spell 
;
;* = chain lightning only


			
;LOAD SPELL
		;set parameters
		LDA #GE.SPELL.LIGHTNING.SEEK_BYTES
		STA PARM.SEEK_BYTES+$0

		LDA /GE.SPELL.LIGHTNING.SEEK_BYTES
		STA PARM.SEEK_BYTES+$1

		LDA #GE.SPELL.LIGHTNING.READ_BYTES
		STA PARM.READ_BYTES+$0

		LDA /GE.SPELL.LIGHTNING.READ_BYTES
		STA PARM.READ_BYTES+$1
	JSR SPELL_FILE.LOAD.CODE_BLOCK


			
;CALL SPELL ROUTINE

;SELECT SPELL ROUTINE #1

	LDA COMBAT.SPELL_CODE.CAST
	CMP #SPELL_CODE.INFERNAL.BLAST
	BEQ .INFERNAL_BLAST 
	
		;if != #SPELL_CODE.INFERNAL.BLAST, then all other possibilities use the regular lightning bolt shape

.COMMON ;regular lightning and chain lightning		
	LDA #LIGHTNING_BOLT.SHAPE.START
	STA COMBAT.SHAPE_TABLE.POINTER+$0

	LDA /LIGHTNING_BOLT.SHAPE.START
	STA COMBAT.SHAPE_TABLE.POINTER+$1

	
;SELECT SPELL ROUTINE #2
	LDA COMBAT.SPELL_CODE.CAST
		
	;#SPELL_CODE.INFERNAL.BLAST branch already occured above in SELECT SPELL ROUTINE #1
	CMP #SPELL_CODE.CHAIN_LIGHTNING.MEDIUM	 
	BEQ .CHAIN_LIGHTNING.MEDIUM
	CMP #SPELL_CODE.CHAIN_LIGHTNING.LARGE
	BEQ .CHAIN_LIGHTNING.LARGE
	; CMP #SPELL_CODE.LIGHTNING.MEGA_BLAST
	; BEQ .LIGHTNING.MEGA_BLAST
	;default
	;#SPELL_CODE.LIGHTNING.BOLT
	;#SPELL_CODE.LIGHTNING.BLAST
	;#SPELL_CODE.LIGHTNING.MEGA_BLAST


	
	;**FALLS THROUGH**
	
.LIGHTNING.BLAST_BOLT 
;note: there is another branch in GE.SPELL.LIGHTNING to distinguish between bolt and blast. 


			
	JSR .COMMON.LIGHTNING.CODE ;SELECT TARGET, ;CALL COMMON ENTRANCE CODE


;CALL SPELL ROUTINE

		;set target location: as selected by player
		LDA SELECT_TARGET.CURRENT_TARGET.X
		STA FP.TARGET.GMAP.X
		LDA SELECT_TARGET.CURRENT_TARGET.Y
		STA FP.TARGET.GMAP.Y		
	JSR GE.SPELL.LIGHTNING

	JMP .EXIT

; .LIGHTNING.MEGA_BLAST 
	; ;JSR GE.SPELL.LIGHTNING.MEGA_BLAST

	; JMP .EXIT
	
.INFERNAL_BLAST
	JSR .COMMON.LIGHTNING.CODE ;SELECT TARGET, ;CALL COMMON ENTRANCE CODE
		
;CALL SPELL ROUTINE

		;set target location: as selected by player
		LDA SELECT_TARGET.CURRENT_TARGET.X
		STA FP.TARGET.GMAP.X
		LDA SELECT_TARGET.CURRENT_TARGET.Y
		STA FP.TARGET.GMAP.Y
		
		LDA #FLAME_LIGHTNING_BOLT.SHAPE.START
		STA COMBAT.SHAPE_TABLE.POINTER+$0

		LDA /FLAME_LIGHTNING_BOLT.SHAPE.START
		STA COMBAT.SHAPE_TABLE.POINTER+$1
	
	JSR GE.SPELL.LIGHTNING
			
	JMP .EXIT
	
.CHAIN_LIGHTNING.MEDIUM	
	JSR .COMMON.LIGHTNING.CODE ;SELECT TARGET, ;CALL COMMON ENTRANCE CODE
		
;CALL SPELL ROUTINE

		;set target location: as selected by player
		LDA SELECT_TARGET.CURRENT_TARGET.X
		STA FP.TARGET.GMAP.X
		LDA SELECT_TARGET.CURRENT_TARGET.Y
		STA FP.TARGET.GMAP.Y
		
		LDA #$02 ;set radius 
	JSR GE.SPELL.CHAIN_LIGHTNING

	JMP .EXIT
	
.CHAIN_LIGHTNING.LARGE
	JSR .COMMON.LIGHTNING.CODE ;SELECT TARGET, ;CALL COMMON ENTRANCE CODE
		
		
;CALL SPELL ROUTINE

		;set target location: as selected by player
		LDA SELECT_TARGET.CURRENT_TARGET.X
		STA FP.TARGET.GMAP.X
		LDA SELECT_TARGET.CURRENT_TARGET.Y
		STA FP.TARGET.GMAP.Y
		
		LDA #$04 ;set radius 
	JSR GE.SPELL.CHAIN_LIGHTNING

	;**FALLS THROUGH**

.EXIT
			
	RTS
	
	
.COMMON.LIGHTNING.CODE
@START


			
;SELECT TARGET
		LDA #$FF ;parm: set radius
		;LDA #$02 ;parm: set radius
	JSR COMBAT.SELECT.ATTACK_TARGET

				; lda #$aa
				; LDx COMBAT.ABORT.FLAG		;($00 = not set, $01 = set)
				; STy COMBAT.ACQUIRE.TARGET.FINAL.SINDEX 
				; jsr prep.brk
				; brk
				
		LDA COMBAT.ABORT.FLAG		;($00 = not set, $01 = set)
		BEQ .SELECT.TARGET.DONE ;branch if abort flag is not set
		PLA ;pop RTS off the stack and do an early exit
		PLA
		JMP .EXIT ;if flag set then don't cast spell
.SELECT.TARGET.DONE



			
;CALL COMMON ENTRANCE CODE
	JSR SPELL_FILE.COMMON.ENTRANCE_CODE


	RTS
@END

@END

GE.SPELL.SUMMON.ENTRANCE
@START
;PARAMETERS: ACC (select spell routine code)
;ENTRANCE: direct
;RETURN: graphics effects for spell 

;=====================SUBROUTINE DOCUMENTATION====================================
;
;*future ideas
;
;It would be nice to collate together two 4 frame animation sets. 
;For exampe, cycle through the 4 frames for the wing flapping. Then ever X iterations,
;cycle through the 4 fire frames.
;
;This could be done if the second set was setup as the next 4 tiles IDs after the first set.
;then use the flag byte to increment the tile ID to the second set based on a counter, or
;instead of the flag byte, identify via the tile_ID or tile_ID range.  
;
;=================================================================================

	;ACC = parm: select spell routine code ;($00 = armagedon by the cows | $01 = summon standard routine)
	STA SAVED.ACC.LOCAL ;save spell selection parameter (;($00 = armagedon by the cows | $01 = summon standard routine))

;LOAD SPELL
		;set parameters
		LDA #GE.SPELL.SUMMON.SEEK_BYTES
		STA PARM.SEEK_BYTES+$0

		LDA /GE.SPELL.SUMMON.SEEK_BYTES
		STA PARM.SEEK_BYTES+$1

		LDA #GE.SPELL.SUMMON.READ_BYTES
		STA PARM.READ_BYTES+$0

		LDA /GE.SPELL.SUMMON.READ_BYTES
		STA PARM.READ_BYTES+$1
	JSR SPELL_FILE.LOAD.CODE_BLOCK

;CALL COMMON ENTRANCE CODE
	JSR SPELL_FILE.COMMON.ENTRANCE_CODE
		
;CALL SPELL ROUTINE

;SELECT SPELL ROUTINE #2
	LDA SAVED.ACC.LOCAL ;restore spell selection parameter (;($00 = armagedon by the cows | $01 = summon standard routine))
	BNE  .SUMMON.COMMON 
	
	; CMP #$01	;($00 = armagedon by the cows | $01 = summon standard routine)
	; BEQ .SUMMON.DEMON_LORD

	; CMP #$02	;($00 = armagedon by the cows | $01 = summon standard routine)
	; BEQ .SUMMON.COMMON

	
	;**FALLS THROUGH**
	
.ARMGDN_COW
;CALL SPELL CODE BLOCK

		LDA #$08 ;set # of cows to summon
	JSR GE.SPELL.ARMGDN_COW
	JMP .EXIT

.SUMMON.COMMON
;CALL SPELL CODE BLOCK

	JSR GE.SPELL.SUMMON.STANDARD.ENTRANCE

	;**FALLS THROUGH**
	
.EXIT	
	RTS
@END

GE.SPELL.DIE.ENTRANCE
@START
;PARAMETERS: none
;ENTRANCE: direct
;RETURN: *targets hit (COMBAT.TARGET_HIT.DB(x)), graphics effects for spell 
;


;LOAD SPELL
		;set parameters
		LDA #GE.SPELL.DIE.SEEK_BYTES
		STA PARM.SEEK_BYTES+$0

		LDA /GE.SPELL.DIE.SEEK_BYTES
		STA PARM.SEEK_BYTES+$1

		LDA #GE.SPELL.DIE.READ_BYTES
		STA PARM.READ_BYTES+$0

		LDA /GE.SPELL.DIE.READ_BYTES
		STA PARM.READ_BYTES+$1
	JSR SPELL_FILE.LOAD.CODE_BLOCK

		
;SELECT TARGET
		LDA #$FF ;parm: set radius
		;LDA #$02 ;parm: set radius
	JSR COMBAT.SELECT.ATTACK_TARGET
		LDA COMBAT.ABORT.FLAG		;($00 = not set, $01 = set)
		BNE .EXIT	;if flag set then don't casst spell

;CALL COMMON ENTRANCE CODE
	JSR SPELL_FILE.COMMON.ENTRANCE_CODE

;CALL SPELL CODE
		;set target location: as selected by player
		LDA SELECT_TARGET.CURRENT_TARGET.X
		STA FP.TARGET.GMAP.X
		LDA SELECT_TARGET.CURRENT_TARGET.Y
		STA FP.TARGET.GMAP.Y
	JSR GE.SPELL.DIE

.EXIT	
	RTS
@END

GE.SPELL.MASS_DEATH.ENTRANCE
@START
;PARAMETERS: none
;ENTRANCE: direct
;RETURN: *targets hit (COMBAT.TARGET_HIT.DB(x)), graphics effects for spell 

	
;LOAD SPELL
		;set parameters
		LDA #GE.SPELL.MASS_DEATH.SEEK_BYTES
		STA PARM.SEEK_BYTES+$0

		LDA /GE.SPELL.MASS_DEATH.SEEK_BYTES
		STA PARM.SEEK_BYTES+$1

		LDA #GE.SPELL.MASS_DEATH.READ_BYTES
		STA PARM.READ_BYTES+$0

		LDA /GE.SPELL.MASS_DEATH.READ_BYTES
		STA PARM.READ_BYTES+$1
	JSR SPELL_FILE.LOAD.CODE_BLOCK

		
;CALL SPELL ROUTINE

		;set shape color
		LDA #$00 ;set to black
		;LDA #$01 ;set to white
		STA SHAPE.ORA.BIT_VALUE
		LDA #$00 ;set to black
		;LDA #$7F ;set to white
		STA SHAPE.NON_EDGE.BYTE.VALUE

		LDA #$01 ;set to off
		STA SHAPE.MOVE.ERASE.TOGGLE	;($00 = shape erase on | >=$01 = shape erase off)
		
	JSR GE.SPELL.MASS_DEATH
			; pla
			; tax
			; pla
			; tay
			; lda #$aa			
			; jsr prep.brk
			; brk
			
.EXIT

				
	RTS
	
@END

GE.SPELL.SMITE.ENTRANCE
@START
;PARAMETERS: none
;ENTRANCE: direct
;RETURN: *targets hit (COMBAT.TARGET_HIT.DB(x)), graphics effects for spell 
;

;LOAD SPELL
		;set parameters
		LDA #GE.SPELL.SMITE.SEEK_BYTES
		STA PARM.SEEK_BYTES+$0

		LDA /GE.SPELL.SMITE.SEEK_BYTES
		STA PARM.SEEK_BYTES+$1

		LDA #GE.SPELL.SMITE.READ_BYTES
		STA PARM.READ_BYTES+$0

		LDA /GE.SPELL.SMITE.READ_BYTES
		STA PARM.READ_BYTES+$1
	JSR SPELL_FILE.LOAD.CODE_BLOCK

	
;;SELECT TARGET
;
	; ;validate entrance
	; LDA SPELL.SMITE.MULTI_TARGET.FLAG	;($00 = single target | $01 = multi-target)
	; BNE .SELECT_TARGET.DONE
		; LDA #$FF ;parm: set radius
		; ;LDA #$02 ;parm: set radius
	; JSR COMBAT.SELECT.ATTACK_TARGET
		; LDA COMBAT.ABORT.FLAG		;($00 = not set, $01 = set)
		; BNE .EXIT	;if flag set then don't cast spell
; .SELECT_TARGET.DONE

;CALL COMMON ENTRANCE CODE
	JSR SPELL_FILE.COMMON.ENTRANCE_CODE
		
;CALL SPELL ROUTINE

		;set target location: as selected by player
		LDA SELECT_TARGET.CURRENT_TARGET.X
		STA FP.TARGET.GMAP.X
		LDA SELECT_TARGET.CURRENT_TARGET.Y
		STA FP.TARGET.GMAP.Y
	JSR GE.SPELL.SMITE


	
.EXIT

			; lda #$01
			; sta troubleshooting.hook

	
	;jmp SPELL_FILE.AFTER_CAST
	
	RTS
@END

GE.SPELL.SHOCK_SPHERE.ENTRANCE
@START
;PARAMETERS: none
;ENTRANCE: direct
;RETURN: *targets hit (COMBAT.TARGET_HIT.DB(x)), graphics effects for spell 


;LOAD SPELL
		;set parameters
		LDA #GE.SPELL.SHOCK_SPHERE.SEEK_BYTES
		STA PARM.SEEK_BYTES+$0

		LDA /GE.SPELL.SHOCK_SPHERE.SEEK_BYTES
		STA PARM.SEEK_BYTES+$1

		LDA #GE.SPELL.SHOCK_SPHERE.READ_BYTES
		STA PARM.READ_BYTES+$0

		LDA /GE.SPELL.SHOCK_SPHERE.READ_BYTES
		STA PARM.READ_BYTES+$1
	JSR SPELL_FILE.LOAD.CODE_BLOCK

		
;SELECT TARGET
		LDA #$FF ;parm: set radius
		;LDA #$02 ;parm: set radius
	JSR COMBAT.SELECT.ATTACK_TARGET
		LDA COMBAT.ABORT.FLAG		;($00 = not set, $01 = set)
		BNE .EXIT	;if flag set then don't cast spell

;CALL COMMON ENTRANCE CODE
	JSR SPELL_FILE.COMMON.ENTRANCE_CODE
		
;CALL SPELL ROUTINE

		;set target location: as selected by player
		LDA SELECT_TARGET.CURRENT_TARGET.X
		STA FP.TARGET.GMAP.X
		LDA SELECT_TARGET.CURRENT_TARGET.Y
		STA FP.TARGET.GMAP.Y
	JSR GE.SPELL.SHOCK_SPHERE


		
.EXIT

	
	RTS

@END

GE.SPELL.MASS_SHOCK_SPHERE.ENTRANCE
@START
;PARAMETERS: ACC (select spell routine)
;ENTRANCE: direct
;RETURN: *targets hit (COMBAT.TARGET_HIT.DB(x)), graphics effects for spell 
;

			
;SAVE PARAMETERS

	;ACC = parm: radius
	;PHA ;save spell radius parameter to stack
	STA SAVED.ACC.LOCAL2 ;save radius parameter
	
;LOAD SPELL
		;set parameters
		LDA #GE.SPELL.MASS_SHOCK_SPHERE.SEEK_BYTES
		STA PARM.SEEK_BYTES+$0

		LDA /GE.SPELL.MASS_SHOCK_SPHERE.SEEK_BYTES
		STA PARM.SEEK_BYTES+$1

		LDA #GE.SPELL.MASS_SHOCK_SPHERE.READ_BYTES
		STA PARM.READ_BYTES+$0

		LDA /GE.SPELL.MASS_SHOCK_SPHERE.READ_BYTES
		STA PARM.READ_BYTES+$1
	
	JSR SPELL_FILE.LOAD.CODE_BLOCK

;SELECT TARGET
		LDA #$FF ;parm: set radius
		;LDA #$02 ;parm: set radius
	JSR COMBAT.SELECT.ATTACK_TARGET
		LDA COMBAT.ABORT.FLAG		;($00 = not set, $01 = set)
		BNE .EXIT	;if flag set then don't casst spell

;CALL COMMON ENTRANCE CODE
	JSR SPELL_FILE.COMMON.ENTRANCE_CODE
	
	;*I put the sound here because if I add anything to mass shock sphere it blows up in errors because it is a mirror image of fireball
	;and the lables are in exactly the same place. there is a parent label GE.EXPLOSION that would need to be changed to a new name in mass shock shere to resolve this. 
	JSR COMBAT.PLAY.SOUND.ELECTRICITY
	
;CALL SPELL ROUTINE
		;set target location: as selected by player
		LDA SELECT_TARGET.CURRENT_TARGET.X
		STA FP.TARGET.GMAP.X
		LDA SELECT_TARGET.CURRENT_TARGET.Y
		STA FP.TARGET.GMAP.Y	
		
		LDA SAVED.ACC.LOCAL2 ;restore radius parameter
		;PLA ;restore spell radius parameter to stack				
	JSR GE.SPELL.MASS_SHOCK_SPHERE

.EXIT				
		; LDA #$AA
		; LDX COMBAT.EXPLOSION.PROB ;set probability for first explosion "ring"	
		; LDY COMBAT.EXPLOSION.PROB.DEC ;set decrement amount per explosion "ring"	
		; JSR PREP.BRK
		; BRK		
	RTS
@END

GE.SPELL.COMBAT_TELEPORT_PARTY.ENTRANCE
@START
;PARAMETERS: none
;ENTRANCE: direct
;RETURN: updated PC GMAP X/Y and updated video screen
;

;LOAD SPELL
		;set parameters
		LDA #GE.SPELL.COMBAT_TELEPORT_PARTY.SEEK_BYTES
		STA PARM.SEEK_BYTES+$0

		LDA /GE.SPELL.COMBAT_TELEPORT_PARTY.SEEK_BYTES
		STA PARM.SEEK_BYTES+$1

		LDA #GE.SPELL.COMBAT_TELEPORT_PARTY.READ_BYTES
		STA PARM.READ_BYTES+$0

		LDA /GE.SPELL.COMBAT_TELEPORT_PARTY.READ_BYTES
		STA PARM.READ_BYTES+$1
	JSR SPELL_FILE.LOAD.CODE_BLOCK

		
;CALL COMMON ENTRANCE CODE
	JSR SPELL_FILE.COMMON.ENTRANCE_CODE
		
;CALL SPELL ROUTINE

	JSR GE.SPELL.COMBAT_TELEPORT_PARTY

.EXIT
				
	
	RTS
@END


;------------------END ENTRANCE ROUTINES----
SPELL_FILE.LOAD.CODE_BLOCK
@START
;PARAMETERS: PARM.SEEK_BYTES, PARM.READ_BYTES
;RETURN: load spell code block into spell file buffer
;ENTRANCE: direct


			
;=====LOAD SPELL FILE======
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
	LDA #$01
	sta parm.sizelo	
	LDA #$00
	sta parm.sizehi
	
;set destination memory address
	lda #SPELL.FILE.CODE_BLOCK.BUFFER.START
	sta parm.ldrlo
	lda /SPELL.FILE.CODE_BLOCK.BUFFER.START
	sta parm.ldrhi

;set filename to read from	
	lda #SRTN.SPELL_FILE	;load LO address
	sta parm.namlo
	lda /SRTN.SPELL_FILE	;load HO address
	sta parm.namhi
					
		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO
			
	;**FALLS THROUGH**
	
;------SEEK FILE------
.SEEK.FILE
	lda #cmd_seek.current_drive
	sta parm.reqcmd

;set seek length (bytes)
	lda PARM.SEEK_BYTES+$0	;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda PARM.SEEK_BYTES+$1	;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi

			; LDA #$AA
			; ;LDX #SPELL.FILE.CODE_BLOCK.BUFFER.START
			; ;LDY /SPELL.FILE.CODE_BLOCK.BUFFER.START
			; LDX parm.sizelo
			; LDY parm.sizehi
			; JSR PREP.BRK
			; BRK
			
		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO
			
	;**FALLS THROUGH**
	
;----READ FILE-----
.READ.FILE
	
	lda #cmd_read.current_drive
	sta parm.reqcmd

; ;set destination memory address
	; lda #SPELL.FILE.CODE_BLOCK.BUFFER.START
	; sta parm.ldrlo
	; lda /SPELL.FILE.CODE_BLOCK.BUFFER.START
	; sta parm.ldrhi
	
;set read length (bytes)
	lda PARM.READ_BYTES+$0		;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda PARM.READ_BYTES+$1		;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi
	cmp /SPELL.FILE.CODE_BLOCK.BUFFER.SIZE ;load HO byte of buffer size
	bcs .ERROR.OVERFLOW ;if spell file to load is less than the spell file buffer size, then load file, otherwise report error

	;**falls through**
	
.load.file	
		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO		
	
	RTS

.ERROR.OVERFLOW
;SPELL_FILE.LOAD.CODE_BLOCK reports that spell file routine is >= SPELL.FILE.CODE_BLOCK.BUFFER.START.SIZE 
	JSR PREP.BRK
	BRK
	
@END
@END

SPELL_FILE.COMMON.ENTRANCE_CODE
@START
;this code is placed here so that it doesn't have to be placed
;in the entrance routine for each spell. 

	
	
	
;DEDUCT MP
;(or abort if attacker doesn't have enough MP to cast the spell)
	JSR COMBAT.SPELL.DEDUCT_MP
		; LDA COMBAT.ABORT.FLAG		;($00 = not set, $01 = set)
		; BNE .EXIT	;if flag set then don't cast spell
	
	RTS
@END

COMBAT.PLAY.SOUND.ELECTRICITY
@START

		;SET PARAMETERS
		
		;connect sound_data pointer to the sound array you want to play
		LDA #SOUND_DATA.COMBAT.REPEAT_TEST2
		STA SOUND_DATA.POINTER+$0
		LDA /SOUND_DATA.COMBAT.REPEAT_TEST2
		STA SOUND_DATA.POINTER+$1
		
		LDA #$14  ;$14 is full electricity sound length
		STA SOUND_DATA.LENGTH ;the number of bytes in the sound array (including the repeate byte)
		;LDA #$04 ;parameter: mode ($00 = freq/duration data table only | $01 also use wait data table | $03 no wait table, enable repeats after every pair | $04 no wait table, enable repeats after every two pairs)
	JSR PLAY.SOUND.REPEAT2

	
	RTS
	
;HEX ARRAYS	

;electricity: actual data ($14 bytes in length)
SOUND_DATA.COMBAT.REPEAT_TEST2 .HS F0.01.F8.01.04.E0.01.D2.01.06.C0.01.C6.01.15.FF.01.F8.01.0E

;electricity: test
;SOUND_DATA.COMBAT.REPEAT_TEST2 .HS F0.01.F8.01.04
;.E0.01.D2.01.06.C0.01.C6.01.15.FF.01.F8.01.0E


@END

COMBAT.SPELL.DEDUCT_MP
@START
;PARAMETERS: COMBAT.ATTACKER.SINDEX, COMBAT.STATS.SPELL.MP_COST, CHR_SHEET.PC_MOB.MP
;ENTRANCE: SPELL_FILE.SELECT.SPELL
;RETURN: CHR_SHEET.PC_MOB.MP

		
.DEDUCT.MP.COST
	;read attacker character sheet
		LDY COMBAT.ATTACKER.SINDEX
		LDA #$00 ;set read mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet
		;RETURN: CHR_SHEET.RECORD.READ($80)

	LDA CHR_SHEET.PC_MOB.MP
	SEC
	SBC COMBAT.STATS.SPELL.MP_COST
	STA CHR_SHEET.PC_MOB.MP

		;write attacker character sheet
		LDY COMBAT.ATTACKER.SINDEX
		LDA #$01 ;set write mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet
		;RETURN: CHR_SHEET.RECORD.READ($80)

	;**FALLS THROUGH**
	
.EXIT

	RTS
	
@END

COMBAT.SPELL.CREATE.SPECIAL
@START
;RETURN VALUE: ACC = ($00 = special record added | $01 special character sheet array full)

;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA

				; JSR TEST.GET.SPECIAL.DATA
				; LDA #$AA
				; JSR PREP.BRK
				; BRK
				
.INIT.MEMORY.STATE
@START
	;ENABLE BANK-SWITCHED RAM ($D000 Bank 1)
	LDA $C08B 
	LDA $C08B
	
	LDX SPECIAL.DATA.POINTER+$0
	LDY SPECIAL.DATA.POINTER+$1
	

				
		;TSX			;transfer stack pointer to X-REG
		;STX TEMP	;save stack pointer
	STA $C009 ;enable aux zero-page & aux BSR 

	STX SPECIAL.DATA.POINTER+$0
	STY SPECIAL.DATA.POINTER+$1

@END

		

.FIND.NEXT.OPEN.RECORD
@START
	LDY #$00 ;init array index
	LDX #$00 ;init CHR_SHEET_EXTENDED.SPECIAL.HP_MAX index (2 byte records)
		
	LDA #CHR_SHEET.SPECIAL.AUX_MEMORY.START
	STA CHR_SHEET.POINTER+$0
	
	LDA /CHR_SHEET.SPECIAL.AUX_MEMORY.START
	STA CHR_SHEET.POINTER+$1
	
.FIND.RECORD.LOOP
	LDA (CHR_SHEET.POINTER),Y ;read 1st field in record of SPECIAL character sheet
	CMP #CHR_SHEET.SPECIAL.STOP_VALUE
	BEQ .FIND.RECORD.LOOP.DONE
	
	;increment index: next record
	INX ;increment CHR_SHEET_EXTENDED.SPECIAL.HP_MAX index (2 byte records)
	INX ;""
	
	LDA CHR_SHEET.POINTER+$0 ;increment array pointer
	CLC
	ADC #CHR_SHEET.SPECIAL.RECORD_SIZE
	STA CHR_SHEET.POINTER+$0	
	BNE .FIND.RECORD.LOOP

	INC CHR_SHEET.POINTER+$1

	;END OF ARRAY TEST
	;if pointer HO byte is <= the end of the special aux memory block
	;then continue loop
	LDA CHR_SHEET.POINTER+$1	
	CMP /CHR_SHEET.SPECIAL.AUX_MEMORY.END
	BCC .FIND.RECORD.LOOP
	BEQ .FIND.RECORD.LOOP				
	JMP .SPECIAL_ARRAY.FULL
	
.FIND.RECORD.LOOP.DONE
@END

.CREATE.NEW.SPECIAL.RECORD	
@START
	;copy SPECIAL data from stats table to special character sheet
	
	;LDY #$00
.INIT.SPECIAL.CHR_SHEET.LOOP
	LDA (SPECIAL.DATA.POINTER),Y ;read SPECIAL character sheet data from stats table	
	STA (CHR_SHEET.POINTER),Y ;write to next open record in SPECIAL character sheet
	INY
	CPY #CHR_SHEET.SPECIAL.RECORD_SIZE
	BNE .INIT.SPECIAL.CHR_SHEET.LOOP
	
	;set Max HP LO/HO of SPECIAL
	LDY #CHR_SHEET.PC_MOB.HP_LO.OFFSET
	LDA (SPECIAL.DATA.POINTER),Y ;read SPECIAL character sheet data from stats table	
	STA CHR_SHEET_EXTENDED.SPECIAL.HP_MAX.AUX+$0,X
	INY ;#CHR_SHEET.PC_MOB.HP_HO.OFFSET
	LDA (SPECIAL.DATA.POINTER),Y ;read SPECIAL character sheet data from stats table	
	STA CHR_SHEET_EXTENDED.SPECIAL.HP_MAX.AUX+$1,X


	
	; ;move stop value to start of next record
	; LDA #CHR_SHEET.SPECIAL.STOP_VALUE
	; STA (CHR_SHEET.POINTER),Y
@END

.SPECIAL_RECORD.ADDED
	LDA #$00 ;set return value ($00 = special record added | $01 special character sheet array full)
	STA TEMP
	JMP .EXIT

.SPECIAL_ARRAY.FULL
	LDA #$01 ;set return value ($00 = special record added | $01 special character sheet array full)
	STA TEMP

	;**FALLS THROUGH**
	
.EXIT
		
;RESTORE MEMORY STATE
	STA $C008 ;enable main zero-page & main BSR 
		; LDX TEMP	;restore stack pointer to X-REG
		; TXS ;transfer X-REG to stack pointer

	;RE-ENABLE BANK-SWITCHED RAM ($D000 Bank 2)
	LDA $C083				;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK2 2nd)
	LDA $C083

	

	
;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX

	LDA TEMP ;load return value ($00 = special record added | $01 special character sheet array full)
	

			
	RTS
	
;LOCAL VARIABLES
	;CHR_SHEET.SAVED.YREG.INDEX .BS $1
	
	
;TEST.GET.SPECIAL.DATA ;copy SPECIAL char sheet from aux to main memory
@START
			
; ;SPECIAL

	; ;ENABLE BANK-SWITCHED RAM ($D000 Bank 1)
	; LDA $C08B 
	; LDA $C08B
						
		; TSX			;transfer stack pointer to X-REG
		; STX TEMP	;save stack pointer
	; STA $C009 ;enable aux zero-page & aux BSR 



	; ; LDA #CHR_SHEET.SPECIAL.STOP_VALUE
	; ; STA CHR_SHEET.SPECIAL.AUX_MEMORY.START
	
			; LDA #CHR_SHEET.SPECIAL.AUX_MEMORY.START
			; STA COPY.FROM_START
			; LDA /CHR_SHEET.SPECIAL.AUX_MEMORY.START
			; STA COPY.FROM_START+$1
			; LDA #CHR_SHEET.SPECIAL.AUX_MEMORY.END
			; STA COPY.FROM_END
			; LDA /CHR_SHEET.SPECIAL.AUX_MEMORY.END
			; STA COPY.FROM_END+$1
				
			; LDA #$00
			; STA COPY.TO
			; LDA #$BD
			; STA COPY.TO+$1
		
		; JSR MEMORY.COPY
				
	
; ;RESTORE MEMORY STATE
	; STA $C008 ;enable main zero-page & main BSR 
		; LDX TEMP	;restore stack pointer to X-REG
		; TXS ;transfer X-REG to stack pointer

	; ;RE-ENABLE BANK-SWITCHED RAM ($D000 Bank 2)
	; LDA $C083				;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK2 2nd)
	; LDA $C083

	; RTS
	
@END


@END

CAST_SPELL.EXIT 
@START
;=====================SUBROUTINE DOCUMENTATION====================================
;
;Accessed via JMP from SPELL_FILE.AFTER_CAST.
;
;The purpose of this subroutine is to handle various post-spell cleanup before returning control to 
;COMMAND.CAST_SPELL in the main game engine. This structure avoids consuing main game engine memory for these cleanup 
;routines. 
;
;=================================================================================

			
	; ;display graphics effects
		
	;reset print string mode flag
	LDA #$00
	STA PRINT.STR.MODE	;($00 = normal | >=$01 = wait/pause functionality enabled)




	LDA COMBAT.ABORT.FLAG		;($00 = not set, $01 = set)
	BNE	.EXIT ;if player aborted the target selection, don't cast spell


;ERASE SHAPE?
.ERASE.SHAPE.CHECK
@START
;(if incremental erase is disabled, then we need to clean up the screen before exiting)

	;is incremental erase enabled?
	;(note: if incremental erase is enabled, then an erase isn't needed at this point because the erase was done each time the shape moved)
	;(If an erase wasn't done each time the shape moved then the shape leaves a trail, which needs to be erased now)

	LDA SHAPE.MOVE.ERASE.TOGGLE	;($00 = shape erase on | >=$01 = shape erase off)
	BEQ .ERASE.SHAPE.CHECK.COMPLETE
		
		; LDA #$AB
		; JSR PREP.BRK
		; BRK


		
	LDA #$00
	STA SHAPE.MOVE.ERASE.TOGGLE	;flip toggle to off (default) 

	JSR FLIP.PAGE ;effectively erases the shape because the shape was only drawn to the foreground
	
	;SYNC HI-RES PAGES
	;Note: needed because the pages need to be in sync for animation to work properly. 
	;ENABLE BANK-SWITCHED RAM ($D000 Bank 1)
	;!!!!! BSR BANK1 !!!!
	JSR COPY.SCREEN.ENTRANCE ;**DON'T REMOVE** (or the screen goes chaotic sometimes when collisions occur with double mover mobs. MO.DRAW must need the graphics pages in sync for some reason). UDPATE: I think this is needed because sometimes, due to key press abort, no copy.screen was done after the last move, so the pages are out of syncing going into MOVE.PASS, which is triggered by non-movement commands like (3)ZAP, Push, Board, etc.


			; jsr keyin
			; lda #$aa
			; jsr prep.brk
			; brk		

.ERASE.SHAPE.CHECK.COMPLETE
@END
	;**FALLS THROUGH**

.EXIT

			
	RTS ;return to COMMAND.CAST_SPELL
@END
	


	.NO SPELL.FILE.ENTRANCE.BUFFER.END+$1 ;fill rest of reserved memory so that SBASM will generated an error if overflow occurs. 

SPELL_FILE.GRAPHICAL.EFFECTS.END
@END


	;**OPT** Disk. Certain spells could be combined into a single spell code block. For example, all spells that use an angle shape (i.e. flame arrow, lightning bolt). And all spells that launch an animated projectile with a standard single target impact (i.e. magic missle and DIE).
				   ;in the combine routine use the spell code to branch to a section that connects the pointer to the proper shape table
				   
GE.SPELL.MAGIC_MISSLE.START ;marks the start address of the code block for this spell
;--------
@START
	.PH SPELL.FILE.CODE_BLOCK.BUFFER.START ;start patch 

GE.SPELL.MAGIC_MISSLE
@START
;PARAMETERS: FP.SOURCE.GMAP.X, FP.SOURCE.GMAP.Y, FP.TARGET.GMAP.X, FP.TARGET.GMAP.Y
;ENTRANCE: DIRECT		
;RETURN: targets hit (COMBAT.TARGET_HIT.DB)

;=====================SUBROUTINE DOCUMENTATION====================================
;
;-target hits
;COMBAT.TARGET_HIT.DB is populated by a call to .TARGET.HIT.CHECK.
;
;
;=================================================================================


.INIT
	;ACC = parameter
	;STA COMBAT.EXPLOSION.RADIUS

	
	JSR .LOAD.SHAPE.TABLE

		
;FIRE PROJECTILE

		;LDA #$00  ;disable (support was added for this)
		LDA #COMBAT.PROJECTILE.DELAY	;(time target = 1.6 seconds when there are 7 empty tiles between source and target)
		STA SHAPE.MOVE.DELAY_LENGTH ;The length of the delay between the shape draw and shape erase.

		LDA #$00 ;set of on
		STA SHAPE.MOVE.SCREEN_ANIMATION.TOGGLE ;($00 = screen animation on | >=$01 = screen animation off)
		STA SHAPE.MOVE.ERASE.TOGGLE	;($00 = shape erase on | >=$01 = shape erase off)		
		LDA #$01	;set mode to angled shape ;($00 = angled static shape | >=$01 = non-angled, animated shape)
			;STA SHAPE.MOVE.SCREEN_ANIMATION.TOGGLE ;($00 = screen animation on | >=$01 = screen animation off)
	JSR COMBAT.FIRE.PROJECTILE

	;draw shape on target
	;(note: this is done because sometimes the COMABT.SHAPE.MOVE doesn't draw the shape directly on the target. Sometimes it does.)
	JSR .DRAW.SHAPE.ON.TARGET
	
	;**FALLS THROUGH**
	
.UPDATE.TARGET_HIT.DATABASE	

;get screen location of prospective GMAP.X/Y of target
		LDA FP.TARGET.GMAP.X ;load new special S_ENTITYY GMAP.X
		STA PARM.GMAP.X
		LDA FP.TARGET.GMAP.Y ;load new special S_ENTITY GMAP.Y
		STA PARM.GMAP.Y				
	JSR CONVERT.COLUMN_ROW.TO.SINDEX
		LDY RETURN.SCREEN_ARRAY_INDEX ;used below
	JSR .TARGET.HIT.CHECK

.SET.CRITICAL_HIT.FLAGS
@START
;(engaged s_entity screen index)

		;**warning: attacker character sheet will be loaded on exit
		LDY COMBAT.ATTACKER.SINDEX
		LDA COMBAT.TARGET_HIT.DB+$0 ;set value to update (sindex of the target)	
.UPDATE.CHR_SHEET.ENGAGED.INDEX ;**OPT** Memory. This routine was written as as subroutine, but is being used inline here and it could be made more efficient. 
;PARAMETERS: YREG = screen index of attacker, ACC = value to write to CHR_SHEET.PC_MOB.ENGAGED.SINDEX
;ENTRANCE: direct
;RETURN: saved attacker character sheet with updated CHR_SHEET.PC_MOB.ENGAGED.SINDEX field

.SAVE.PARAMETERS.ENAGED
	;ACC = value to write to CHR_SHEET.PC_MOB.ENGAGED.SINDEX
	STA COMBAT.ENGAGED_SINDEX.UPDATE.PARM
	
	;read attacker (MOB) character sheet
		
		;YREG = COMBAT.ATTACKER.SINDEX
		LDA #$00 ;set read mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet

			
	;set the engaged field to the updated value
	;(expected to be the screen index of the defender or $FF to indicate no engagement)
	LDA COMBAT.ENGAGED_SINDEX.UPDATE.PARM
	STA CHR_SHEET.PC_MOB.ENGAGED.SINDEX

			
		;YREG = COMBAT.ATTACKER.SINDEX
		LDA #$01 ;set write mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet

@END


.EXIT
	RTS


@END

.LOAD.SHAPE.TABLE
@START
;PARAMETERS: none
;ENTRANCE: direct

;load the shape into the hoppers
	LDX #$00
.PROJECTILE.LOAD.SHAPE.LOOP0
	LDA SHAPE_TABLE.MAGIC_MISSLE.START,X	
	STA SHAPE.HOPPER0,X
	INX
	BNE .PROJECTILE.LOAD.SHAPE.LOOP0

	RTS
@END

.DRAW.SHAPE.ON.TARGET
@START
;PARAMETERES: none
;ENTRANCE: direct

	;refresh shape table
	;(note: this needs to be done because on sharp angles COMBAT.FIRE.PROJECTILE or its subroutines modify the contents
	;of SHAPE_HOPPER0+$0-SHAPE_HOPPER0+$1F for some reason. I'm not sure why, but refreshing the shape tables resolve the issue for this routine.)
	JSR .LOAD.SHAPE.TABLE
	
	
		;set parameters
		LDY SHAPE.SINDEX.TARGET	
		LDA SCREEN.INDEX.TILE_SBYTE,Y
		STA	SHAPE.DRAW.CURRENT.SBYTE
		
		LDA SCREEN.INDEX.TILE_LINE,Y
		STA SHAPE.DRAW.LINE_ROW.NUMBER

		;Y-REG = screen tile lcation of target
		LDA #$01 ;set foreground page mode
		STA PAGE.FOREGROUND.OVERRIDE
		LDA #$00 ;set draw mode
	JSR .DRAW.TILE.AND_ORA
	
		LDA #$A0		
	JSR WAIT.LOOP

	RTS


@END

.DRAW.TILE.AND_ORA ;draw single tile using AND + ORA mask
@START
;PARAMETERS:  	;ACC = ($00 = draw mode | $01 = erase mode), SHAPE.DRAW.LINE_ROW.NUMBER, SHAPE.DRAW.CURRENT.SBYTE, [PAGE.FOREGROUND.OVERRIDE]
;RETURN: NONE
;ENTRANCE: .DRAW.SHAPE.ON.TARGET
;*Set to default value on game launch. When modified (i.e. tall grass), it is reset to default when done.

;=====================SUBROUTINE DOCUMENTATION====================================
;This subroutine, COMBAT.SHAPE.DRAW and DRAW.TILE are the lowest level routines associated with tile graphics plotting. 
;See DRAW.TILE for general documentation on the methodology used to plot to the graphics screen.
;
;This subroutine's unique purpose is to be able to draw a single tile using an AND+ORA mask. 
;
;This subroutine is specifically designed to draw shapes which are objects drawn on top of a background. The underlying
;terrain can be supplied by terrain tiles or other object tiles. However, this routine can draw over any 14x16 section
;the hi-res screen whether it conforms to the tile grid or not.
;
;To blend shapes into the background, two shape tables are used. One containing and AND mask and the other containing
;an ORA mask.
;
;here is a quick summary of the concept:
;(also see the first two posts from Michael Pohoreski in the following
;post https://groups.google.com/forum/#!topic/comp.sys.apple2.programmer/cgxZOBIL728)
;
; byte1: 1111110
; byte2: 0000010
;
; Using AND on byte 1 and byte 2 results in:
;
; result: 0000010
;
; (i.e. each bit is assigned the value 1 only if both byte1 AND byte2 have a 1 in that bit position.
;
;
; OR works similarly except that a 1 is assigned to each bit position in the result if either byte1 OR byte2 has a 1 in that bit position. 
;
;
; The technique works like this:
;
; before drawing the shape copy the background, and apply an AND value to each byte. The AND values has "0" values where the shape will be. The result is a shape table that has the background and a small "hole" carved out where the shape will go.
;
; Then apply an OR value to each byte. the OR values have "1" values where the shape is. The result is shape table with 1s where either the background or the shape is. This is different from simply drawing the shape on top of the background because when the "hole" is carved out, a two pixel buffer is put around the shape so that the background pixels and the shape pixels don't interact to create unwanted lines and colors. 
;=================================================================================
	
	;ACC = $00 = draw mode | $01 = erase mode
	STA DRAW.SHAPE.MODE

;**OPT** Speed. Include an in-line code version of DRAW.TILE with DRAW.COLUMN and DRAW.ROW to avoid the costly JSR/RTS to call as a subroutine. 

.SAVE.REGISTERS
	TXA
	PHA
	TYA
	PHA

.START	
	LDX SHAPE.DRAW.LINE_ROW.NUMBER			;LOAD LINE IN X REGISTER	
	TXA ;transfer SHAPE.DRAW.LINE_ROW.NUMBER to ACC
	STA TILE.LINE.CNTR 
	STA TILE.LINE.START
	CLC						;*OPT* I I THINK ALL CLS CAN BE REMOVED WHEN I'M NOT EXPECTING AN OVERFLOW
	ADC #TILE.DEPTH.STANDARD				;ADD DEPTH OF SHAPE TO STARTING LINE IN HEX (# of lines, not the position of last line....so line positions $0-$F is $10 (!16) total lines)
	STA TILE.LINE.STOP		;SET THE LAST LINE TO DRAW BASED ON THE DEPTH OF THE TILE

	LDA #$00
	STA SHP.TBL.CNTR		;START DRAWING AT BEGINNING OF SHAPE TABLE
	
	
.DRAW.LOOP

;GET LINE ADDRESS
@START

;==========INLINE CODE GET.LINE.ADDRESS1==================	
	LDA PAGE.FOREGROUND.OVERRIDE	;CHECK TO SEE IF CALLING ROUTINE WANTS ERASE DONE TO FOREGROUND INSTEAD OF BACKGROUND (DEFAULT)
	CMP #$01
	BEQ .FOREGROUND_OVERRIDE
	
	LDA PAGE.BACKGROUND		
	CMP #$02
	BEQ .LOOKUP.PAGE2
	JMP .LOOKUP.PAGE1
	
.FOREGROUND_OVERRIDE
	LDA PAGE.FOREGROUND		;SPECIFY PAGE TO GET ADDRESS FOR (SHOULD BE BACKGROUND PAGE)
	CMP #$02
	BEQ .LOOKUP.PAGE2
	

.LOOKUP.PAGE1	
	LDA LINE.HO.P1,X		;GET LINE ADDRESS
	STA LINE.BASE.ADDR1+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR1
	JMP .GETLINE_COMPLETE

.LOOKUP.PAGE2
	LDA LINE.HO.P2,X		;GET LINE ADDRESS
	STA LINE.BASE.ADDR1+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR1
.GETLINE_COMPLETE
;========================================================
@END
	
.DRAW_ERASE.ENTRANCE
	LDA DRAW.SHAPE.MODE ;ACC = $00 = draw mode | $01 = erase mode
	;BNE .ERASE.MODE
	BEQ .DRAW.MODE
	JMP .ERASE.MODE
	;**FALLS THROUGH**
	
.DRAW.MODE	
;screen byte 0
	LDY SHP.TBL.CNTR		;LOAD Y WITH SHAPE TABLE COUNTER	
	;init counters/indexes
	TYA ;init X-REG to $00 as the SHAPE.EXISTING_DATA.BUFFER index
	TAX ;""
	

	;save existing graphics data
	LDY SHAPE.DRAW.CURRENT.SBYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	LDA (LINE.BASE.ADDR1),Y	;load existing data from video memory
	STA SHAPE.EXISTING_DATA.BUFFER,X
			;STA TEMP
	;load new shape data
	;ACC = existing graphics data
	LDY SHP.TBL.CNTR		;LOAD Y WITH SHAPE TABLE COUNTER	
	AND (SHAPE.BACKGROUND.AND_MASK),Y
	ORA (SHAPE.COMPOSITE.ORA_MASK),Y
		
	;write new shape data
	LDY SHAPE.DRAW.CURRENT.SBYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	STA (LINE.BASE.ADDR1),Y	;PLOT (1st screen byte)
	
;next screen byte
	INC SHP.TBL.CNTR		;NEXT SHAPE BYTE
	INX	;next existing data buffer byte
	INC SHAPE.DRAW.CURRENT.SBYTE			;SWITCH TO 2ND SCREEN BYTE IN THE TILE	
	
;screen byte 1
	
	;save existing graphics data
	LDY SHAPE.DRAW.CURRENT.SBYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	LDA (LINE.BASE.ADDR1),Y	;load existing data from video memory
	STA SHAPE.EXISTING_DATA.BUFFER,X
		
	;load new shape data
	;ACC = existing graphics data
	LDY SHP.TBL.CNTR		;LOAD Y WITH SHAPE TABLE COUNTER	
	AND (SHAPE.BACKGROUND.AND_MASK),Y
	ORA (SHAPE.COMPOSITE.ORA_MASK),Y
		
	;write new shape data
	LDY SHAPE.DRAW.CURRENT.SBYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	STA (LINE.BASE.ADDR1),Y	;PLOT (1st screen byte)
	JMP .NEXT.LINE
	
.ERASE.MODE
;screen byte0
	LDY SHP.TBL.CNTR		;LOAD Y WITH SHAPE TABLE COUNTER
	LDA SHAPE.EXISTING_DATA.BUFFER,Y			;LOAD SHAPE BYTE (1st screen byte of the tile)

	LDY SHAPE.DRAW.CURRENT.SBYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	STA (LINE.BASE.ADDR1),Y	;PLOT (1st screen byte)

;next screen byte	
	INC SHP.TBL.CNTR		;NEXT SHAPE BYTE, NEXT TILE LINE
	INC SHAPE.DRAW.CURRENT.SBYTE			;SWITCH TO 2ND SCREEN BYTE IN THE TILE

;screen byte1	
	LDY SHP.TBL.CNTR		;LOAD Y WITH SHAPE TABLE COUNTER
	LDA SHAPE.EXISTING_DATA.BUFFER,Y			;LOAD SHAPE BYTE (2nd screen byte of the tile)
	LDY SHAPE.DRAW.CURRENT.SBYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	STA (LINE.BASE.ADDR1),Y	
	;**FALLS THROUGH**
	
.NEXT.LINE
;next line, reset screen byte	
	DEC SHAPE.DRAW.CURRENT.SBYTE			;SWITCH BACK TO 1ST SCREEN BYTE IN THE TILE	;**OPT** if the counters are updated after the CMP/branch it might save 1 INC SHAPE.DRAW.CURRENT.SBYTE instruction in the main loop because SHAPE.DRAW.CURRENT.SBYTE will be in 2nd position when this loop ends. 
	INC SHAPE.DRAW.LINE_ROW.NUMBER
	
	INC SHP.TBL.CNTR		;NEXT SHAPE BYTE
	INC TILE.LINE.CNTR		;NEXT TILE LINE
	LDX TILE.LINE.CNTR 		;restore tile line number to X-REG

;exit test	
	CPX TILE.LINE.STOP		;IS TILE DONE?							
	BCS .DONE				
	JMP .DRAW.LOOP
.DONE	
	LDA #$00
	STA PAGE.FOREGROUND.OVERRIDE
				
.RESTORE.REGISTERS
	PLA
	TAY
	PLA
	TAX
	
	RTS

@END	

.TARGET.HIT.CHECK
@START
;PARAMETERS: YREG (screen index), COMBAT.EXPLOSION.PROB

;SAVE PARAMETERS
	TXA
	PHA
	
;WAS A TARGET PRESENT?				
	;YREG = screen index
	LDA SCREEN.MO_SPRITE.DATA,Y
	CMP #$FF 
	BEQ .CHECK.TARGET_HIT.EXIT ;if no S_ENTITY is present at this screen location then exit, don't record a target hit


;IS THE HIT TILE THE SPELLCASTER?
	CPY COMBAT.ATTACKER.SINDEX
	BEQ .CHECK.TARGET_HIT.EXIT
	
;IS THIS A DUPLICATE HIT?
;(note: this routine is needed because the iteration patter of some spells results in tiles being examined more than once, which could result
;in this routine being called more than once for the same tile)

	LDX #$00
	TYA ;transfer screen tile location to ACC
.DUPLICATE.CHECK.LOOP
	CMP COMBAT.TARGET_HIT.DB,X ;if S_ENTITY in current screen location was already recorded as a hit, then exit
	BEQ	.CHECK.TARGET_HIT.EXIT
	INX
;	CPX #COMBAT.TARGET_HIT.DB.STOP_VALUE
	CPX #COMBAT.TARGET_HIT.DB.SIZE
	BNE .DUPLICATE.CHECK.LOOP
		
	
;RECORD NEW TARGET HIT	
	;YREG = screen index
	TYA
	LDX COMBAT.TARGET_HIT.DB.INDEX
	STA COMBAT.TARGET_HIT.DB,X
	;write stop value
	;(note: each time a new target hit is recorded the stop value is advanced 1 byte in the array)
	LDA #COMBAT.TARGET_HIT.DB.STOP_VALUE
	STA COMBAT.TARGET_HIT.DB+$1,X

			; ;JSR KEYIN
			; STA TEMP
			; CMP #$D1
			; BNE .TEMP
			; ;LDA #$AA
			; LDA COMBAT.TARGET_HIT.DB,X
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP

	INC COMBAT.TARGET_HIT.DB.INDEX	;next target_hit record

.CHECK.TARGET_HIT.EXIT

;RESTORE PARAMETERS
	PLA
	TAX
			
	RTS	
@END

;SHAPE TABLE: MAGIC MISSLE
@START
SHAPE_TABLE.MAGIC_MISSLE.START
.SPELL.MAGIC_MISSLE.SHAPE_0.AND_MASK			.HS	FF.FF.FF.FF.AF.FF.83.FC.83.FC.AF.FF.FF.FF.FF.D7.FF.81.EB.81.80.D7.80.FF.EB.F5.BF.C0.BF.C0.FF.F5
.SPELL.MAGIC_MISSLE.SHAPE_0.ORA_MASK			.HS	00.00.00.00.00.00.50.00.50.00.00.00.00.00.00.00.00.28.00.28.14.00.14.00.00.00.00.0A.00.0A.00.00
				
.SPELL.MAGIC_MISSLE.SHAPE_1.AND_MASK			.HS	7F.7F.7F.75.3F.40.3F.40.6B.75.00.57.00.01.6B.01.7F.57.7F.7F.7F.7F.7F.7F.2F.7F.03.7C.03.7C.2F.7F
.SPELL.MAGIC_MISSLE.SHAPE_1.ORA_MASK			.HS	00.00.00.00.00.0A.00.0A.00.00.14.00.14.28.00.28.00.00.00.00.00.00.00.00.00.00.50.00.50.00.00.00
				
.SPELL.MAGIC_MISSLE.SHAPE_2.AND_MASK			.HS	FF.FF.FF.F5.BF.C0.BF.C0.FF.F5.AF.FF.83.FC.83.FC.AB.FF.80.FF.80.F5.AB.C0.BF.C0.FF.F5.FF.FF.FF.FF
.SPELL.MAGIC_MISSLE.SHAPE_2.ORA_MASK			.HS	00.00.00.00.00.0A.00.0A.00.00.00.00.50.00.50.00.00.00.14.00.14.00.00.0A.00.0A.00.00.00.00.00.00
				
.SPELL.MAGIC_MISSLE.SHAPE_3.AND_MASK			.HS	7F.7F.2F.7F.03.7C.03.7C.2B.7F.00.75.00.40.2B.40.7F.75.7F.7F.7F.7F.7F.7F.2F.7F.03.7C.03.7C.2F.7F
.SPELL.MAGIC_MISSLE.SHAPE_3.ORA_MASK			.HS	00.00.00.00.50.00.50.00.00.00.14.00.14.0A.00.0A.00.00.00.00.00.00.00.00.00.00.50.00.50.00.00.00

@END

	.EP ;end patch mode
;--------
GE.SPELL.MAGIC_MISSLE.END ;marks the end address of the code block for this spell
@END
;=======================

GE.SPELL.FLAME_ARROW.START ;marks the start address of the code block for this spell
;--------
@START
	.PH SPELL.FILE.CODE_BLOCK.BUFFER.START ;start patch 

GE.SPELL.FLAME_ARROW
@START
;PARAMETERS: FP.SOURCE.GMAP.X, FP.SOURCE.GMAP.Y, FP.TARGET.GMAP.X, FP.TARGET.GMAP.Y
;ENTRANCE: DIRECT

;=====================SUBROUTINE DOCUMENTATION====================================
;
;-target hits
;COMBAT.TARGET_HIT.DB is populated by a call to .TARGET.HIT.CHECK from this routine
;
;=================================================================================


		;LDA #$00  ;disable (support was added for this)
		;LDA #$10  ;lightning speed	
		LDA #COMBAT.PROJECTILE.DELAY	;(time target = 1.6 seconds when there are 7 empty tiles between source and target)
		STA SHAPE.MOVE.DELAY_LENGTH ;The length of the delay between the shape draw and shape erase.

		LDA #$01 ;set of off
		STA SHAPE.MOVE.SCREEN_ANIMATION.TOGGLE ;($00 = screen animation on | >=$01 = screen animation off)
		
		LDA #$00
		STA SHAPE.MOVE.ERASE.TOGGLE	;($00 = shape erase on | >=$01 = shape erase off)
		
		LDA #$00	;set mode to angled shape ;($00 = angled static shape | >=$01 = non-angled, animated shape)
	JSR COMBAT.FIRE.PROJECTILE	
		
	;**FALLS THROUGH**
	
.UPDATE.TARGET_HIT.DATABASE	

;get screen location of prospective GMAP.X/Y of target
		LDA FP.TARGET.GMAP.X ;load new special S_ENTITYY GMAP.X
		STA PARM.GMAP.X
		LDA FP.TARGET.GMAP.Y ;load new special S_ENTITY GMAP.Y
		STA PARM.GMAP.Y				
	JSR CONVERT.COLUMN_ROW.TO.SINDEX
		LDY RETURN.SCREEN_ARRAY_INDEX ;used below
	JSR .TARGET.HIT.CHECK

.SET.CRITICAL_HIT.FLAGS
@START
;(engaged s_entity screen index)

		;**warning: attacker character sheet will be loaded on exit
		LDY COMBAT.ATTACKER.SINDEX
		LDA COMBAT.TARGET_HIT.DB+$0 ;set value to update (sindex of the target)	
.UPDATE.CHR_SHEET.ENGAGED.INDEX ;**OPT** Memory. This routine was written as as subroutine, but is being used inline here and it could be made more efficient. 
;PARAMETERS: YREG = screen index of attacker, ACC = value to write to CHR_SHEET.PC_MOB.ENGAGED.SINDEX
;ENTRANCE: direct
;RETURN: saved attacker character sheet with updated CHR_SHEET.PC_MOB.ENGAGED.SINDEX field

.SAVE.PARAMETERS.ENAGED
	;ACC = value to write to CHR_SHEET.PC_MOB.ENGAGED.SINDEX
	STA COMBAT.ENGAGED_SINDEX.UPDATE.PARM
	
	;read attacker (MOB) character sheet
		
		;YREG = COMBAT.ATTACKER.SINDEX
		LDA #$00 ;set read mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet

			
	;set the engaged field to the updated value
	;(expected to be the screen index of the defender or $FF to indicate no engagement)
	LDA COMBAT.ENGAGED_SINDEX.UPDATE.PARM
	STA CHR_SHEET.PC_MOB.ENGAGED.SINDEX

			
		;YREG = COMBAT.ATTACKER.SINDEX
		LDA #$01 ;set write mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet

@END

.EXIT	
	RTS
	
@END

.TARGET.HIT.CHECK
@START
;PARAMETERS: YREG (screen index), COMBAT.EXPLOSION.PROB

;SAVE PARAMETERS
	TXA
	PHA
	
;WAS A TARGET PRESENT?				
	;YREG = screen index
	LDA SCREEN.MO_SPRITE.DATA,Y
	CMP #$FF 
	BEQ .CHECK.TARGET_HIT.EXIT ;if no S_ENTITY is present at this screen location then exit, don't record a target hit

;IS THE HIT TILE THE SPELLCASTER?
	CPY COMBAT.ATTACKER.SINDEX
	BEQ .CHECK.TARGET_HIT.EXIT
	
;IS THIS A DUPLICATE HIT?
;(note: this routine is needed because the iteration patter of some spells results in tiles being examined more than once, which could result
;in this routine being called more than once for the same tile)

	LDX #$00
	TYA ;transfer screen tile location to ACC
.DUPLICATE.CHECK.LOOP
	CMP COMBAT.TARGET_HIT.DB,X ;if S_ENTITY in current screen location was already recorded as a hit, then exit
	BEQ	.CHECK.TARGET_HIT.EXIT
	INX
;	CPX #COMBAT.TARGET_HIT.DB.STOP_VALUE
	CPX #COMBAT.TARGET_HIT.DB.SIZE
	BNE .DUPLICATE.CHECK.LOOP
		
	
;RECORD NEW TARGET HIT	
	;YREG = screen index
	TYA
	LDX COMBAT.TARGET_HIT.DB.INDEX
	STA COMBAT.TARGET_HIT.DB,X
	;write stop value
	;(note: each time a new target hit is recorded the stop value is advanced 1 byte in the array)
	LDA #COMBAT.TARGET_HIT.DB.STOP_VALUE
	STA COMBAT.TARGET_HIT.DB+$1,X

			; ;JSR KEYIN
			; STA TEMP
			; CMP #$D1
			; BNE .TEMP
			; ;LDA #$AA
			; LDA COMBAT.TARGET_HIT.DB,X
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP

	INC COMBAT.TARGET_HIT.DB.INDEX	;next target_hit record

.CHECK.TARGET_HIT.EXIT

;RESTORE PARAMETERS
	PLA
	TAX
			
	RTS	
@END

;SHAPE TABLE: FLAME ARROW
@START
FLAME_ARROW.SHAPE.START
;----------
SPELL.ANGULAR.FLAME_ARROW.SHAPE_0.AND_MASK			.HS	FF.FF.FF.FB.FF.E0.FF.E0.FF.E0.FF.E0.FF.E0.FF.E0.FF.E0.FF.E0.FF.E0.FF.E0.FF.E0.FF.E0.FF.FB.FF.FF
SPELL.ANGULAR.FLAME_ARROW.SHAPE_0.ORA_MASK			.HS	00.00.00.00.00.81.00.81.00.81.00.81.00.81.00.81.00.81.00.81.00.81.00.81.00.81.00.81.00.00.00.00
				
SPELL.ANGULAR.FLAME_ARROW.SHAPE_1.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.D7.EA.81.80.D7.EA.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF
SPELL.ANGULAR.FLAME_ARROW.SHAPE_1.ORA_MASK			.HS	00.00.00.00.00.00.00.00.00.00.00.00.00.00.A8.95.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00
				
SPELL.ANGULAR.FLAME_ARROW.SHAPE_2.AND_MASK			.HS	FF.FF.FF.FE.9F.F8.9F.F8.9F.F8.9F.F8.9F.F8.9F.F8.9F.F8.9F.F8.9F.F8.9F.F8.9F.F8.9F.F8.FF.FE.FF.FF
SPELL.ANGULAR.FLAME_ARROW.SHAPE_2.ORA_MASK			.HS	00.00.00.00.00.81.00.81.00.81.00.81.00.81.00.81.00.81.00.81.00.81.00.81.00.81.00.81.00.00.00.00
				
SPELL.ANGULAR.FLAME_ARROW.SHAPE_3.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.D7.EA.81.80.D7.EA.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF
SPELL.ANGULAR.FLAME_ARROW.SHAPE_3.ORA_MASK			.HS	00.00.00.00.00.00.00.00.00.00.00.00.00.00.A8.95.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00
				
				
;OCTET 4				
SPELL.ANGULAR.FLAME_ARROW.SHAPE_NE0.AND_MASK			.HS	FF.FF.FF.FB.FF.E0.FF.E0.FF.FA.9F.F8.9F.F8.DF.FE.87.FE.87.FE.D7.FF.C1.FF.C1.FF.F7.FF.FF.FF.FF.FF
SPELL.ANGULAR.FLAME_ARROW.SHAPE_NE0.ORA_MASK			.HS	00.00.00.00.00.84.00.84.00.00.00.81.00.81.00.00.A0.80.A0.80.00.00.88.80.88.80.00.00.00.00.00.00
				
SPELL.ANGULAR.FLAME_ARROW.SHAPE_NE1.AND_MASK			.HS	FF.FF.FF.FF.FF.EF.FF.83.FF.EB.FF.E0.FF.FA.9F.F8.DF.FE.87.FE.D7.FF.C1.FF.F7.FF.FF.FF.FF.FF.FF.FF
SPELL.ANGULAR.FLAME_ARROW.SHAPE_NE1.ORA_MASK			.HS	00.00.00.00.00.00.00.90.00.00.00.84.00.00.00.81.00.00.A0.00.00.00.88.00.00.00.00.00.00.00.00.00
				
SPELL.ANGULAR.FLAME_ARROW.SHAPE_NE2.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.FF.FF.FF.EF.FF.83.FF.E0.9F.F8.87.FE.C1.FF.F7.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF
SPELL.ANGULAR.FLAME_ARROW.SHAPE_NE2.ORA_MASK			.HS	00.00.00.00.00.00.00.00.00.00.00.90.00.84.00.81.A0.00.88.00.00.00.00.00.00.00.00.00.00.00.00.00
				
				
;OCTET 5				
SPELL.ANGULAR.FLAME_ARROW.SHAPE_SE0.AND_MASK			.HS	FF.FF.FF.FF.F7.FF.C1.FF.C1.FF.D7.FF.87.FE.87.FE.DF.FE.9F.F8.9F.F8.FF.FA.FF.E0.FF.E0.FF.FB.FF.FF
SPELL.ANGULAR.FLAME_ARROW.SHAPE_SE0.ORA_MASK			.HS	00.00.00.00.00.00.88.80.88.80.00.00.A0.80.A0.80.00.00.00.81.00.81.00.00.00.84.00.84.00.00.00.00
				
SPELL.ANGULAR.FLAME_ARROW.SHAPE_SE1.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.F7.FF.C1.FF.D7.FF.87.FE.DF.FE.9F.F8.FF.FA.FF.E0.FF.EB.FF.83.FF.EF.FF.FF.FF.FF
SPELL.ANGULAR.FLAME_ARROW.SHAPE_SE1.ORA_MASK			.HS	00.00.00.00.00.00.00.00.88.00.00.00.A0.00.00.00.00.81.00.00.00.84.00.00.00.90.00.00.00.00.00.00
				
SPELL.ANGULAR.FLAME_ARROW.SHAPE_SE2.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.F7.FF.C1.FF.C1.FF.87.FE.9F.F8.FF.E0.FF.83.FF.EF.FF.FF.FF.FF.FF.FF
SPELL.ANGULAR.FLAME_ARROW.SHAPE_SE2.ORA_MASK			.HS	00.00.00.00.00.00.00.00.00.00.00.00.88.00.88.00.A0.00.00.81.00.84.00.90.00.00.00.00.00.00.00.00
				
				
;OCTET 6				
SPELL.ANGULAR.FLAME_ARROW.SHAPE_SW0.AND_MASK			.HS	FF.FF.FF.FF.FF.EF.FF.83.FF.83.FF.EB.FF.E0.FF.E0.FB.FA.80.F8.80.F8.DB.FE.87.FE.87.FE.DF.FF.FF.FF
SPELL.ANGULAR.FLAME_ARROW.SHAPE_SW0.ORA_MASK			.HS	00.00.00.00.00.00.80.90.80.90.00.00.80.84.80.84.00.00.84.81.84.81.00.00.A0.80.A0.80.00.00.00.00
				
SPELL.ANGULAR.FLAME_ARROW.SHAPE_SW1.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.FF.EF.FF.83.FF.EB.FF.E0.FF.FA.9F.F8.DF.FE.87.FE.D7.FF.C1.FF.F7.FF.FF.FF.FF.FF
SPELL.ANGULAR.FLAME_ARROW.SHAPE_SW1.ORA_MASK			.HS	00.00.00.00.00.00.00.00.00.90.00.00.00.84.00.00.00.81.00.00.A0.00.00.00.88.00.00.00.00.00.00.00
				
SPELL.ANGULAR.FLAME_ARROW.SHAPE_SW2.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.EF.FF.83.FF.E0.9F.F8.87.FE.C1.FF.F7.FF.FF.FF.FF.FF.FF.FF.FF.FF
SPELL.ANGULAR.FLAME_ARROW.SHAPE_SW2.ORA_MASK			.HS	00.00.00.00.00.00.00.00.00.00.00.00.00.90.00.84.00.81.A0.00.88.00.00.00.00.00.00.00.00.00.00.00
				
				
;OCTET 7				
SPELL.ANGULAR.FLAME_ARROW.SHAPE_NW0.AND_MASK			.HS	FF.FF.DF.FF.87.FE.87.FE.DF.FE.9F.F8.9F.F8.FF.FA.FF.E0.FF.E0.FF.EB.FF.83.FF.83.FF.EF.FF.FF.FF.FF
SPELL.ANGULAR.FLAME_ARROW.SHAPE_NW0.ORA_MASK			.HS	00.00.00.00.A0.00.A0.00.00.00.80.01.80.01.00.00.80.84.80.84.00.00.80.90.80.90.00.00.00.00.00.00
				
SPELL.ANGULAR.FLAME_ARROW.SHAPE_NW1.AND_MASK			.HS	FF.FF.FF.FF.F7.FF.C1.FF.D7.FF.87.FE.DF.FE.9F.F8.FF.FA.FF.E0.FF.EB.FF.83.FF.EF.FF.FF.FF.FF.FF.FF
SPELL.ANGULAR.FLAME_ARROW.SHAPE_NW1.ORA_MASK			.HS	00.00.00.00.00.00.88.00.00.00.A0.00.00.00.80.01.00.00.00.84.00.00.00.90.00.00.00.00.00.00.00.00
				
SPELL.ANGULAR.FLAME_ARROW.SHAPE_NW2.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.FF.FF.F7.FF.C1.FF.87.FE.9F.F8.FF.E0.FF.83.FF.EF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF
SPELL.ANGULAR.FLAME_ARROW.SHAPE_NW2.ORA_MASK			.HS	00.00.00.00.00.00.00.00.00.00.88.00.A0.00.80.01.00.84.00.90.00.00.00.00.00.00.00.00.00.00.00.00
@END

	.EP ;end patch mode
;--------
GE.SPELL.FLAME_ARROW.END ;marks the end address of the code block for this spell
@END
;=======================
		
GE.SPELL.FIREBALL.START ;marks the start address of the code block for this spell
;--------
@START
	.PH SPELL.FILE.CODE_BLOCK.BUFFER.START ;start patch 

GE.SPELL.FIREBALL ;requires GE.EXPLOSION
@START
;PARAMETERS: FP.SOURCE.GMAP.X, FP.SOURCE.GMAP.Y, FP.TARGET.GMAP.X, FP.TARGET.GMAP.Y
;ENTRANCE: DIRECT		
;RETURN: targets hit (COMBAT.TARGET_HIT.DB)

;=====================SUBROUTINE DOCUMENTATION====================================
;
;-target hits
;COMBAT.TARGET_HIT.DB is populated by a call to .TARGET.HIT.CHECK from GE.EXPLOSION
;
;-Medium Fireball
; LDA #$E6 ;90%  ($E6 = 90%)
; STA COMBAT.EXPLOSION.PROB ;set probability for first explosion "ring"	
;
; LDA #$80 ;-50%   ($40 = 25%, $80 = 50%)
; STA COMBAT.EXPLOSION.PROB.DEC ;set decrement amount per explosion "ring"	
;
; LDA #TILE_ID.FIRE_A ;parameter: tile_type of explosion
; STA COMBAT.EXPLOSION.TILE_TYPE
;
; LDA #$02 ;radius
;JSR .GE.EXPLOSION
;
;-Large Fireball
; LDA #$E6 ;90%  ($E6 = 90%)
; STA COMBAT.EXPLOSION.PROB ;set probability for first explosion "ring"	
;
; LDA #$80 ;-50%   ($40 = 25%, $80 = 50%)
; STA COMBAT.EXPLOSION.PROB.DEC ;set decrement amount per explosion "ring"	
;
; LDA #TILE_ID.FIRE_A ;parameter: tile_type of explosion
; STA COMBAT.EXPLOSION.TILE_TYPE
;
; LDA #$03 ;radius
;JSR .GE.EXPLOSION
;=================================================================================

		
.INIT
	;ACC = parameter
	STA COMBAT.EXPLOSION.RADIUS

	JSR .LOAD.SHAPE.TABLE
	
;FIRE PROJECTILE
			
		;LDA #$00  ;disable (support was added for this)
		LDA #COMBAT.PROJECTILE.DELAY	;(time target = 1.6 seconds when there are 7 empty tiles between source and target)
		STA SHAPE.MOVE.DELAY_LENGTH ;The length of the delay between the shape draw and shape erase.

		LDA #$00 ;set off
		STA SHAPE.MOVE.SCREEN_ANIMATION.TOGGLE ;($00 = screen animation on | >=$01 = screen animation off)
		STA SHAPE.MOVE.ERASE.TOGGLE	;($00 = shape erase on | >=$01 = shape erase off)		
		LDA #$01	;set mode to angled shape ;($00 = angled static shape | >=$01 = non-angled, animated shape)
			;STA SHAPE.MOVE.SCREEN_ANIMATION.TOGGLE ;($00 = screen animation on | >=$01 = screen animation off)
	JSR COMBAT.FIRE.PROJECTILE


	
;DRAW EXPLOSION

	;refresh shape table
	;(note: this needs to be done because on sharp angles COMBAT.FIRE.PROJECTILE or its subroutines modify the contents
	;of SHAPE_HOPPER0+$0-SHAPE_HOPPER0+$1F for some reason. I'm not sure why, but refreshing the shape tables resolve the issue for this routine.)
	;JSR .LOAD.SHAPE.TABLE
	JSR .LOAD.SHAPE.TABLE

		LDA #$C0 ;($E6 = 90%, $C0 = 75%)
		STA COMBAT.EXPLOSION.PROB ;set probability for first explosion "ring"	

		LDA #$40 ;($40 = -25%, $80 = -50%)
		STA COMBAT.EXPLOSION.PROB.DEC ;set decrement amount per explosion "ring"	

		LDA #TILE_ID.FIRE_A ;parameter: tile_type of explosion
		;LDA #$9F ;COW
		STA COMBAT.EXPLOSION.TILE_TYPE

		LDA COMBAT.EXPLOSION.RADIUS
	JSR .GE.EXPLOSION

	
.SET.CRITICAL_HIT.FLAGS
@START
;(engaged s_entity screen index)

		;**warning: active player character sheet will be loaded on exit
		LDY COMBAT.ATTACKER.SINDEX
		LDA #COMBAT.STATS.CRTL_HIT.NOT_ENGAGED_VALUE ;set value to update.
.UPDATE.CHR_SHEET.ENGAGED.INDEX ;**OPT** Memory. This routine was written as as subroutine, but is being used inline here and it could be made more efficient. 
;PARAMETERS: YREG = screen index of attacker, ACC = value to write to CHR_SHEET.PC_MOB.ENGAGED.SINDEX
;ENTRANCE: direct
;RETURN: saved attacker character sheet with updated CHR_SHEET.PC_MOB.ENGAGED.SINDEX field

.SAVE.PARAMETERS.ENAGED
	;ACC = value to write to CHR_SHEET.PC_MOB.ENGAGED.SINDEX
	STA COMBAT.ENGAGED_SINDEX.UPDATE.PARM
	
	;read attacker (MOB) character sheet
		
		;YREG = COMBAT.ATTACKER.SINDEX
		LDA #$00 ;set read mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet

			
	;set the engaged field to the updated value
	;(expected to be the screen index of the defender or $FF to indicate no engagement)
	LDA COMBAT.ENGAGED_SINDEX.UPDATE.PARM
	STA CHR_SHEET.PC_MOB.ENGAGED.SINDEX

			
		;YREG = COMBAT.ATTACKER.SINDEX
		LDA #$01 ;set write mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet

@END


.EXIT

	RTS


@END

.LOAD.SHAPE.TABLE
@START
;PARAMETERS: none
;ENTRANCE: direct

;load the shape into the hoppers
	LDX #$00
.LOAD.SHAPE_DATA.LOOP0
	LDA .SHAPE_TABLE.FIREBALL.START,X	
	STA SHAPE.HOPPER0,X
	INX
	BNE .LOAD.SHAPE_DATA.LOOP0

	RTS
@END

;SHAPE TABLE: FIREBALL
@START

;****VIOLET/BLUE BACKGROUND MAY BE A PROBLEM***
;(if this spell is cast when the battle field background has blue, there could be unwanted blue pixels
;that appear inside the fire. This is because the AND mask for the fireball spell blends in the background. The reason the 
;violet pixels turn blue is because the ORA mask for the fireball shape sets the high bit and violet and blue share the even numbered columns.
;
;This issue is not as bad with the fireball projectile because for that shape the background isn't blended so only it only occurs with a few pixles 
;that are outside the shape boundaries but in screen bytes shared with the shape a result of horizontal shape movement that is less than
;one full screen byte.
;
;Thus, the issue would not be as bad with the fireball spell if the backgroun wasn't blended but I've deferred that decision becuase blending the background
;over the targets hit looks cool, and it helps to conceptualize that those targets were hit before seeing the hit shape.  

.SHAPE_TABLE.FIREBALL.START
.SPELL.FIREBALL.SHAPE_0.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.F7.FF.C1.FF.F0.FF.DD.FE.87.B8.C1.8E.D7.83.87.AE.85.BC.90.CB.80.A0.81.80.80.80
.SPELL.FIREBALL.SHAPE_0.ORA_MASK			.HS	80.80.80.80.80.80.80.80.88.80.82.80.80.80.A0.81.88.C0.80.D0.A0.80.E0.80.82.C0.AA.84.A0.D1.8E.C5
				
.SPELL.FIREBALL.SHAPE_1.AND_MASK			.HS	7F.7F.7F.7F.7F.7F.7F.7F.7D.7F.50.7F.05.3A.07.40.1D.03.00.6E.40.3F.79.4F.57.3B.01.60.10.00.00.00
.SPELL.FIREBALL.SHAPE_1.ORA_MASK			.HS	80.80.80.80.80.80.80.80.80.80.82.80.A0.80.E0.C4.80.90.A2.80.86.80.80.C0.80.80.A8.84.82.91.AA.97
				
.SPELL.FIREBALL.SHAPE_2.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.F7.FF.C1.FF.E0.FF.F9.EF.FD.82.90.E8.F5.BE.81.C8.97.80.FF.E8.F5.E0.80.80.85.E0
.SPELL.FIREBALL.SHAPE_2.ORA_MASK			.HS	80.80.80.80.80.80.80.80.88.80.86.80.80.80.80.90.82.81.80.80.88.C1.80.93.80.80.80.84.8A.95.A0.84
				
.SPELL.FIREBALL.SHAPE_3.AND_MASK			.HS	7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.77.7F.01.7B.00.60.1D.00.05.6A.50.60.5D.7B.07.7A.41.60.15.00.00.48
.SPELL.FIREBALL.SHAPE_3.ORA_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.88.80.E2.84.80.91.A0.80.82.84.80.80.A0.80.88.84.80.95.8A.C1
@END

.GE.EXPLOSION
@START
;PARAMETERS: ACC (radius of explosion), COMBAT.EXPLOSION.TILE_TYPE,  COMBAT.EXPLOSION.PROB,  COMBAT.EXPLOSION.PROB.DEC
;ENTRANCE: DIRECT
;RETURN: 

.INIT.EXPLOSION
	;ACC = parameter
	STA COMBAT.EXPLOSION.RADIUS

	; ;init target_hit database
	; LDX #$00 ;init COMBAT.TARGET_HIT.DB index
	; STX COMBAT.TARGET_HIT.DB.INDEX
; .INIT.LOOP
	; LDA #$FF ;use init value that isn't ever used as a screen array index (screen tile location)
	; STA COMBAT.TARGET_HIT.DB,X
	; INX
	; CPX #COMBAT.TARGET_HIT.DB.SIZE
	; BNE .INIT.LOOP
		
	LDY SHAPE.SINDEX.TARGET
	
	STY COMBAT.EXPLOSION.CENTER
	;STY COMBAT.EXPLOSION.RUN.START_TILE	
	
	LDA #$00
	STA COMBAT.EXPLOSION.RADIUS.COUNTER
	;STA COMBAT.EXPLOSION.SCREEN_EDGE.COLLISION
	;STA COMBAT.EXPLOSION.SCREEN_EDGE.RIGHT
	
	LDA SCREEN.INDEX.COLUMN,Y ;load tile column # of target 
	STA COMBAT.EXPLOSION.START.COLUMN

	LDA SCREEN.INDEX.ROW,Y ;load tile column # of current algorithm tile
	STA COMBAT.EXPLOSION.START.ROW

			
.DRAW.TARGET_TILE ;draws an explosion tile at the explosion center 
	
		LDA COMBAT.EXPLOSION.PROB
		;STA SAVED.ACC.LOCAL ;save probability 
		PHA ;save probability 

			; lda #$aa
			; ldx COMBAT.EXPLOSION.PROB
			; jsr prep.brk
			; brk
			
		;temporarily adjust probability so that the explosion center tile always draws
		LDA #$FF ;set probability to 100% (there is an override that treats $FF as 100% probability)
		STA COMBAT.EXPLOSION.PROB
	JSR .DRAW.EXPLOSION.ENTRANCE

	PLA ;restore probability
	;LDA SAVED.ACC.LOCAL ;restore probability
	STA COMBAT.EXPLOSION.PROB
			

			
	;check for zero radius setting
	LDA COMBAT.EXPLOSION.RADIUS
	BNE .EXPLOSION.LOOP
	JMP .ERASE.EXPLOSION ;if zero radius then exit after drawin explosion on the target tile
	
.EXPLOSION.LOOP

			

.INCREMENT.START_TILE
;LOOP EXIT CHECK
	LDA COMBAT.EXPLOSION.RADIUS.COUNTER
	CMP COMBAT.EXPLOSION.RADIUS ;is COMBAT.EXPLOSION.RADIUS.COUNTER >= COMBAT.EXPLOSION.RADIUS?
	BCS .EXPLOSION.DRAW.COMPLETE_STEP ;if yes, then the explosion draw is complete. exit. 	
	;**FALLS THROUGH** ;if no, then draw the next "ring" of the explosion. 

;INCREMENT COUNTERS & INDEXES	
	INC COMBAT.EXPLOSION.RADIUS.COUNTER ;increment each tile the start tile is moved up one position, and that wides the radius of the current run 

	LDA COMBAT.EXPLOSION.START.COLUMN
	CLC
	ADC COMBAT.EXPLOSION.RADIUS.COUNTER
	STA COMBAT.EXPLOSION.RUN_RIGHT.STOP_VALUE

	LDA COMBAT.EXPLOSION.START.ROW
	SEC
	SBC COMBAT.EXPLOSION.RADIUS.COUNTER
	STA COMBAT.EXPLOSION.RUN_UP.STOP_VALUE ;set to the start row + COMBAT.EXPLOSION.RADIUS.COUNTER

	LDA COMBAT.EXPLOSION.START.ROW
	CLC
	ADC COMBAT.EXPLOSION.RADIUS.COUNTER
	STA COMBAT.EXPLOSION.RUN_DOWN.STOP_VALUE ;set to the start row + COMBAT.EXPLOSION.RADIUS.COUNTER

	LDA COMBAT.EXPLOSION.START.COLUMN
	SEC
	SBC COMBAT.EXPLOSION.RADIUS.COUNTER
	STA COMBAT.EXPLOSION.RUN_LEFT.STOP_VALUE ;set to the start column - COMBAT.EXPLOSION.RADIUS.COUNTER
	
	
	
	;screen edge exit test
	LDA SCREEN.INDEX.ROW,Y ;load tile row # of algorithm current tile location
	BEQ .START_TILE.INCREMENT.COMPLETE ;if in first row, then keep start tile in the same position
			
	;increment algorithm start tile location
	TYA ;transfer algorithm current tile
	SEC
	SBC #SCREEN.ARRAY.OFFSET
	TAY ;transfer current algorithm to Y-REG as screen index parameter
	JMP .DRAW.EXPLOSION.START

.EXPLOSION.DRAW.COMPLETE_STEP
	JMP .EXPLOSION.DRAW.COMPLETE
	
.START_TILE.INCREMENT.COMPLETE
	;JSR EXPLOSION.RADIUS.ADJUSTMENT
		

	; ;increment algorithm start tile location
	; TYA ;transfer algorithm current tile
	; CLC
	; ADC #SCREEN.ARRAY.OFFSET	
	; ;ACC = incremented start tile value
	
	;**FALLS THROUGH**

.DRAW.EXPLOSION.START			
;DRAW EXPLOSION IN ALGORITHM START TILE

		;Y-REG = incremented start tile value
	JSR .DRAW.EXPLOSION.ENTRANCE 
			
	;**FALLS THROUGH**
	
.RING.START ;each explosion ring has 5 runs. It's 5 instead of 4 because the top of the ring is split into two runs,
			;because the alogrithm start tile is in the center of the top of the ring. 
	
.RUN.LEFT
;INIT
	LDX #$00 ;init COMBAT.EXPLOSION.RUN.COUNTER
.RUN.LEFT.LOOP ;the run that is left of the start tile

	;screen edge exit check
	LDA SCREEN.INDEX.COLUMN,Y ;load tile column # of algorithm current tile location
	;BNE .RUN.LEFT.DRAW ;if not in first column, then continue this run
	;JSR EXPLOSION.RADIUS.ADJUSTMENT ;shorten radius to blast isn't expanded in direction opposite the screen edge
	BEQ .RUN.DOWN ;if in first column, then end this run
	;JMP .RUN.DOWN ;if in first column, then end this run

.RUN.LEFT.DRAW			
	;draw explosion
		DEY ;left increment: current algorithm tile
	JSR .DRAW.EXPLOSION.ENTRANCE 

	INX ;increment for each tile move in the current run. COMBAT.EXPLOSION.RUN.COUNTER 
	
	;run exit check
	LDA SCREEN.INDEX.COLUMN,Y ;load tile column # of current algorithm tile
	CMP COMBAT.EXPLOSION.RUN_LEFT.STOP_VALUE ;set to the start column - COMBAT.EXPLOSION.RADIUS.COUNTER	
	;CPX COMBAT.EXPLOSION.RADIUS.COUNTER ;is COMBAT.EXPLOSION.RUN.COUNTER (x-reg) = COMBAT.EXPLOSION.RADIUS.COUNTER
	BNE .RUN.LEFT.LOOP ;if yes, continue drawing in this run
				  ;if no, next run
				  
	;**FALLS THROUGH**


	
.RUN.DOWN
;INIT
	LDX #$00 ;reset COMBAT.EXPLOSION.RUN.COUNTER

.RUN.DOWN.LOOP

;CRASHES
	
			
	;screen edge exit check
	LDA SCREEN.INDEX.ROW,Y ;load tile row # of algorithm current tile location
	CMP #COMBAT.SCREEN.LAST_ROW
	;BNE .RUN.DOWN.DRAW ;if not in last row, then continue this run
	;JSR EXPLOSION.RADIUS.ADJUSTMENT ;shorten radius to blast isn't expanded in direction opposite the screen edge
	BEQ .RUN.RIGHT ;if in last row, then end this run
	;JMP .RUN.RIGHT ;if in last row, then end this run
	

.RUN.DOWN.DRAW
	;draw explosion
	
		;down increment: current algorithm tile
		TYA
		CLC
		ADC #SCREEN.ARRAY.OFFSET
		TAY ;transfer incremented screen array index back to Y-REG		
	JSR .DRAW.EXPLOSION.ENTRANCE 
		
	INX ;increment for each tile move in the current run. COMBAT.EXPLOSION.RUN.COUNTER 
	
	;run exit check
	LDA SCREEN.INDEX.ROW,Y ;load tile column # of current algorithm tile
	CMP COMBAT.EXPLOSION.RUN_DOWN.STOP_VALUE ;set to the start row + COMBAT.EXPLOSION.RADIUS.COUNTER
	;CPX COMBAT.EXPLOSION.RADIUS.COUNTER_X2 ;is COMBAT.EXPLOSION.RUN.COUNTER (x-reg) = COMBAT.EXPLOSION.RADIUS_X2
	BNE .RUN.DOWN.LOOP ;if no, continue drawing in this run
				  ;if yes, next run

	;**FALLS THROUGH**
				  
.RUN.RIGHT
;INIT
	LDX #$00 ;reset COMBAT.EXPLOSION.RUN.COUNTER

;CRASHES
			
.RUN.RIGHT.LOOP

	LDA SCREEN.INDEX.COLUMN,Y ;load tile column # of algorithm current tile location
	CMP #COMBAT.SCREEN.LAST_COLUMN
	;BNE .RUN.RIGHT.DRAW ;if not in last column, then continue this run
	; JSR EXPLOSION.RADIUS.ADJUSTMENT ;shorten radius to blast isn't expanded in direction opposite the screen edge
	; LDA #$01
	; STA COMBAT.EXPLOSION.SCREEN_EDGE.RIGHT
	BEQ .RUN.UP ;if in last column, then end this run

.RUN.RIGHT.DRAW			
	;draw explosion
		INY ;right increment: current algorithm tile
	JSR .DRAW.EXPLOSION.ENTRANCE 
		
	INX ;increment for each tile move in the current run. COMBAT.EXPLOSION.RUN.COUNTER 
	
	;run exit check
	LDA SCREEN.INDEX.COLUMN,Y ;load tile column # of current algorithm tile
	CMP COMBAT.EXPLOSION.RUN_RIGHT.STOP_VALUE ;set to the start column + COMBAT.EXPLOSION.RADIUS.COUNTER
	
	;CPX COMBAT.EXPLOSION.RADIUS.COUNTER_X2 ;is COMBAT.EXPLOSION.RUN.COUNTER (x-reg) = COMBAT.EXPLOSION.RADIUS_X2
	BNE .RUN.RIGHT.LOOP ;if no, continue drawing in this run
				  ;if yes, next run

	;**FALLS THROUGH**

.RUN.UP
;INIT
	LDX #$00 ;reset COMBAT.EXPLOSION.RUN.COUNTER

.RUN.UP.LOOP

	
	;screen edge exit test
	LDA SCREEN.INDEX.ROW,Y ;load tile row # of algorithm current tile location
	;BNE .RUN.UP.DRAW ;if not in first row, then continue this run
	;JSR EXPLOSION.RADIUS.ADJUSTMENT ;shorten radius to blast isn't expanded in direction opposite the screen edge	
	BEQ .RUN.LEFT2 ;if in first row, then end this run
	;JMP .RUN.LEFT2 ;if in first row, then end this run


.RUN.UP.DRAW	
	;draw explosion
	
		;down increment: curren algorithm tile
		TYA
		SEC
		SBC #SCREEN.ARRAY.OFFSET
		TAY ;transfer incremented screen array index back to Y-REG		
	JSR .DRAW.EXPLOSION.ENTRANCE 
		
	INX ;increment for each tile move in the current run. COMBAT.EXPLOSION.RUN.COUNTER 
	
	;run exit check
	LDA SCREEN.INDEX.ROW,Y ;load tile column # of current algorithm tile
	CMP COMBAT.EXPLOSION.RUN_UP.STOP_VALUE ;set to the start row - COMBAT.EXPLOSION.RADIUS.COUNTER
	;CPX COMBAT.EXPLOSION.RADIUS.COUNTER_X2 ;is COMBAT.EXPLOSION.RUN.COUNTER (x-reg) = COMBAT.EXPLOSION.RADIUS_X2
	BNE .RUN.UP.LOOP ;if no, continue drawing in this run
				  ;if yes, next run

	;**FALLS THROUGH**

.RUN.LEFT2
;INIT
	;LDX #$00 ;reset COMBAT.EXPLOSION.RUN.COUNTER

.RUN.LEFT2.LOOP ;the run that is right of the start tile



	;INX ;increment for each tile move in the current run. COMBAT.EXPLOSION.RUN.COUNTER 

			
	;run exit check
	LDA SCREEN.INDEX.COLUMN,Y ;load tile row # of algorithm current tile location
	CMP COMBAT.EXPLOSION.START.COLUMN ;is current algorithm tile in or beyond the start tile column
	
	;CPX COMBAT.EXPLOSION.RADIUS.COUNTER ;is COMBAT.EXPLOSION.RUN.COUNTER (x-reg) = COMBAT.EXPLOSION.RADIUS.COUNTER	
	BEQ .RING.END ;if no, continue drawing in this run
	;BCC .RING.END ;if no, continue drawing in this run
				  ;if yes, next run

		DEY ;left increment: current algorithm tile
					  
		;YREG =
	JSR .DRAW.EXPLOSION.ENTRANCE 

	JMP .RUN.LEFT2.LOOP
	
	;**FALLS THROUGH**

; .RING.END.ENTRANCE1
	; DEY ;move current algorithm tile back to start position. 
		
;.RING.END.ENTRANCE2	
.RING.END	
	;JSR FLIP.PAGE ;display the explosion tiles drawn in the last iteration, representing a "ring" of the explosion

	;decrease probability for next explosion "ring"
	;(that an explosion will be drawn for any given tile)
	LDA COMBAT.EXPLOSION.PROB
	SEC
	SBC COMBAT.EXPLOSION.PROB.DEC
	STA COMBAT.EXPLOSION.PROB
	BCC .PROBABILITY.UNDERFLOW
	JMP .WAIT
	
.PROBABILITY.UNDERFLOW	
	LDA #$81 ;set probability to a minimum of 20%
	STA COMBAT.EXPLOSION.PROB
	
.WAIT	
		;LDA #$06
		LDA #$A0		
	JSR WAIT.LOOP		

;CONTINUE LOOP
	JMP .EXPLOSION.LOOP
	
.EXPLOSION.DRAW.COMPLETE

	
.ERASE.EXPLOSION
	
			;JSR KEYIN
		
		LDA #$FF		
	JSR WAIT.LOOP
	
	JSR FLIP.PAGE
	
	;!!!!! BSR BANK1 !!!!
	JSR COPY.SCREEN.ENTRANCE 
	
	;**FALLS THROUGH**
	
.GE.EXPLOSION.EXIT

	RTS

;EXPLOSION.RADIUS.ADJUSTMENT
@START

	; LDA COMBAT.EXPLOSION.SCREEN_EDGE.COLLISION ;has the algorithm already encountered a screen edge?
	; BNE .EXIT ;if yes exit. Only 1 adjustment is needed
	
	; ;adjust radius
	; ;(the father into the algorithm the screen edge was detected, the smaller the adjustmet to the radius is)
	; LDA COMBAT.EXPLOSION.RADIUS
	; SEC
	; SBC COMBAT.EXPLOSION.RADIUS.COUNTER	
	; STA TEMP
	; LDA COMBAT.EXPLOSION.RADIUS
	; SBC TEMP
	; STA COMBAT.EXPLOSION.RADIUS
	
			; ; LDA #$AA
			; ; LDX TEMP
			; ; ;LDX COMBAT.EXPLOSION.SCREEN_EDGE.COLLISION
			; ; LDY COMBAT.EXPLOSION.RADIUS
			; ; JSR PREP.BRK
			; ; BRK
			
	; ;set flag so this adjustment isn't repeated
	; LDA #$01
	; STA COMBAT.EXPLOSION.SCREEN_EDGE.COLLISION
	
; .EXIT
	; RTS
@END

.DRAW.EXPLOSION.ENTRANCE
@START
;PARAMETERS: YREG (screen index), COMBAT.EXPLOSION.PROB
;ENTRANCE: DIRECT
;RETURN: explosion graphics effect, updated COMBAT.TARGET_HIT.DB


;DRAW EXPLOSION ON THIS TILE?
;(some tiles are randomly skipped to vary the explosion pattern)

	;if probability is $FF, treat as 100% probability
	LDA COMBAT.EXPLOSION.PROB
	CMP #$FF
	BEQ .DRAW.CURRENT_TILE.START
	
		;JMP .DRAW.CURRENT_TILE.START
		
.PATHS_2
	JSR RANDOM.8
	CMP COMBAT.EXPLOSION.PROB		
	BCS .DRAW.EXPLOSION.EXIT	;don't draw explosion on this tile
	;**FALLS THROUGH**
	
;DRAW EXPLOSION ON CURRENT TILE
.DRAW.CURRENT_TILE.START
		; ;YREG = screen index
		; LDA #$01
		; STA PAGE.FOREGROUND.OVERRIDE
		; ;STA DRAW.TILE.SHAPE_TABLE.OVERRIDE ;($00 = load data into shape table from aux memory | $01 don't load data into shape table)
		; LDA COMBAT.EXPLOSION.TILE_TYPE
		; STA SAVED_TILE_TYPE 			
	; JSR DRAW.TILE.SINGLE

	;JSR .DRAW.TILE.AND_ORA
	;JSR .DRAW.SHAPE.ON.TARGET
	JSR .DRAW.EXPLOSION.SHAPE
	
	JSR .TARGET.HIT.CHECK
	
.DRAW.EXPLOSION.EXIT

	RTS
	
@END

@END

.DRAW.EXPLOSION.SHAPE
@START
;PARAMETERES: Y-REG (screen index of tile location)
;ENTRANCE: direct

		
		;set parameters
			
		;YREG = screen index for tile location	
		LDA SCREEN.INDEX.TILE_SBYTE,Y
		STA SHAPE.DRAW.CURRENT.SBYTE
		
		LDA SCREEN.INDEX.TILE_LINE,Y
		STA SHAPE.DRAW.LINE_ROW.NUMBER
		
		;Y-REG = screen tile lcation of target
		LDA #$01 ;set foreground page mode
		STA PAGE.FOREGROUND.OVERRIDE
		LDA #$00 ;set draw mode
	JSR .DRAW.TILE.AND_ORA
	
	RTS


@END

.DRAW.TILE.AND_ORA ;draw single tile using AND + ORA mask
@START
;PARAMETERS:  	;ACC = ($00 = draw mode | $01 = erase mode), SHAPE.DRAW.LINE_ROW.NUMBER, SHAPE.DRAW.CURRENT.SBYTE, [PAGE.FOREGROUND.OVERRIDE]
;RETURN: NONE
;ENTRANCE: .DRAW.SHAPE.ON.TARGET
;*Set to default value on game launch. When modified (i.e. tall grass), it is reset to default when done.

;=====================SUBROUTINE DOCUMENTATION====================================
;This subroutine, COMBAT.SHAPE.DRAW and DRAW.TILE are the lowest level routines associated with tile graphics plotting. 
;See DRAW.TILE for general documentation on the methodology used to plot to the graphics screen.
;
;This subroutine's unique purpose is to be able to draw a single tile using an AND+ORA mask. 
;
;This subroutine is specifically designed to draw shapes which are objects drawn on top of a background. The underlying
;terrain can be supplied by terrain tiles or other object tiles. However, this routine can draw over any 14x16 section
;the hi-res screen whether it conforms to the tile grid or not.
;
;To blend shapes into the background, two shape tables are used. One containing and AND mask and the other containing
;an ORA mask.
;
;here is a quick summary of the concept:
;(also see the first two posts from Michael Pohoreski in the following
;post https://groups.google.com/forum/#!topic/comp.sys.apple2.programmer/cgxZOBIL728)
;
; byte1: 1111110
; byte2: 0000010
;
; Using AND on byte 1 and byte 2 results in:
;
; result: 0000010
;
; (i.e. each bit is assigned the value 1 only if both byte1 AND byte2 have a 1 in that bit position.
;
;
; OR works similarly except that a 1 is assigned to each bit position in the result if either byte1 OR byte2 has a 1 in that bit position. 
;
;
; The technique works like this:
;
; before drawing the shape copy the background, and apply an AND value to each byte. The AND values has "0" values where the shape will be. The result is a shape table that has the background and a small "hole" carved out where the shape will go.
;
; Then apply an OR value to each byte. the OR values have "1" values where the shape is. The result is shape table with 1s where either the background or the shape is. This is different from simply drawing the shape on top of the background because when the "hole" is carved out, a two pixel buffer is put around the shape so that the background pixels and the shape pixels don't interact to create unwanted lines and colors. 
;=================================================================================
	
	;ACC = $00 = draw mode | $01 = erase mode
	STA DRAW.SHAPE.MODE

;**OPT** Speed. Include an in-line code version of DRAW.TILE with DRAW.COLUMN and DRAW.ROW to avoid the costly JSR/RTS to call as a subroutine. 

.SAVE.REGISTERS
	TXA
	PHA
	TYA
	PHA

.START	
	LDX SHAPE.DRAW.LINE_ROW.NUMBER			;LOAD LINE IN X REGISTER	
	TXA ;transfer SHAPE.DRAW.LINE_ROW.NUMBER to ACC
	STA TILE.LINE.CNTR 
	STA TILE.LINE.START
	CLC						;*OPT* I I THINK ALL CLS CAN BE REMOVED WHEN I'M NOT EXPECTING AN OVERFLOW
	ADC #TILE.DEPTH.STANDARD				;ADD DEPTH OF SHAPE TO STARTING LINE IN HEX (# of lines, not the position of last line....so line positions $0-$F is $10 (!16) total lines)
	STA TILE.LINE.STOP		;SET THE LAST LINE TO DRAW BASED ON THE DEPTH OF THE TILE

	LDA #$00
	STA SHP.TBL.CNTR		;START DRAWING AT BEGINNING OF SHAPE TABLE
	
	
.DRAW.LOOP

;GET LINE ADDRESS
@START

;==========INLINE CODE GET.LINE.ADDRESS1==================	
	LDA PAGE.FOREGROUND.OVERRIDE	;CHECK TO SEE IF CALLING ROUTINE WANTS ERASE DONE TO FOREGROUND INSTEAD OF BACKGROUND (DEFAULT)
	CMP #$01
	BEQ .FOREGROUND_OVERRIDE
	
	LDA PAGE.BACKGROUND		
	CMP #$02
	BEQ .LOOKUP.PAGE2
	JMP .LOOKUP.PAGE1
	
.FOREGROUND_OVERRIDE
	LDA PAGE.FOREGROUND		;SPECIFY PAGE TO GET ADDRESS FOR (SHOULD BE BACKGROUND PAGE)
	CMP #$02
	BEQ .LOOKUP.PAGE2
	

.LOOKUP.PAGE1	
	LDA LINE.HO.P1,X		;GET LINE ADDRESS
	STA LINE.BASE.ADDR1+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR1
	JMP .GETLINE_COMPLETE

.LOOKUP.PAGE2
	LDA LINE.HO.P2,X		;GET LINE ADDRESS
	STA LINE.BASE.ADDR1+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR1
.GETLINE_COMPLETE
;========================================================
@END
	
.DRAW_ERASE.ENTRANCE
	LDA DRAW.SHAPE.MODE ;ACC = $00 = draw mode | $01 = erase mode
	;BNE .ERASE.MODE
	BEQ .DRAW.MODE
	JMP .ERASE.MODE
	;**FALLS THROUGH**
	
.DRAW.MODE	
;screen byte 0
	LDY SHP.TBL.CNTR		;LOAD Y WITH SHAPE TABLE COUNTER	
	;init counters/indexes
	TYA ;init X-REG to $00 as the SHAPE.EXISTING_DATA.BUFFER index
	TAX ;""
	

	;save existing graphics data
	LDY SHAPE.DRAW.CURRENT.SBYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	LDA (LINE.BASE.ADDR1),Y	;load existing data from video memory
	STA SHAPE.EXISTING_DATA.BUFFER,X
			;STA TEMP
	;load new shape data
	;ACC = existing graphics data
	LDY SHP.TBL.CNTR		;LOAD Y WITH SHAPE TABLE COUNTER	
	AND (SHAPE.BACKGROUND.AND_MASK),Y
	ORA (SHAPE.COMPOSITE.ORA_MASK),Y

			; STA TEMP
			; LDA TEST.ITERATION.COUNTER
			; CMP #$02
			; BNE .TEMP
			; LDA #$AA
			; LDX SHAPE.DRAW.LINE_ROW.NUMBER
			; LDY SHAPE.DRAW.CURRENT.SBYTE
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP
			
	;write new shape data
	LDY SHAPE.DRAW.CURRENT.SBYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	STA (LINE.BASE.ADDR1),Y	;PLOT (1st screen byte)
	
;next screen byte
	INC SHP.TBL.CNTR		;NEXT SHAPE BYTE
	INX	;next existing data buffer byte
	INC SHAPE.DRAW.CURRENT.SBYTE			;SWITCH TO 2ND SCREEN BYTE IN THE TILE	
	
;screen byte 1
	
	;save existing graphics data
	LDY SHAPE.DRAW.CURRENT.SBYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	LDA (LINE.BASE.ADDR1),Y	;load existing data from video memory
	STA SHAPE.EXISTING_DATA.BUFFER,X
		
	;load new shape data
	;ACC = existing graphics data
	LDY SHP.TBL.CNTR		;LOAD Y WITH SHAPE TABLE COUNTER	
	AND (SHAPE.BACKGROUND.AND_MASK),Y
	ORA (SHAPE.COMPOSITE.ORA_MASK),Y
		
	;write new shape data
	LDY SHAPE.DRAW.CURRENT.SBYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	STA (LINE.BASE.ADDR1),Y	;PLOT (1st screen byte)
	JMP .NEXT.LINE
	
.ERASE.MODE
;screen byte0
	LDY SHP.TBL.CNTR		;LOAD Y WITH SHAPE TABLE COUNTER
	LDA SHAPE.EXISTING_DATA.BUFFER,Y			;LOAD SHAPE BYTE (1st screen byte of the tile)

	LDY SHAPE.DRAW.CURRENT.SBYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	STA (LINE.BASE.ADDR1),Y	;PLOT (1st screen byte)

;next screen byte	
	INC SHP.TBL.CNTR		;NEXT SHAPE BYTE, NEXT TILE LINE
	INC SHAPE.DRAW.CURRENT.SBYTE			;SWITCH TO 2ND SCREEN BYTE IN THE TILE

;screen byte1	
	LDY SHP.TBL.CNTR		;LOAD Y WITH SHAPE TABLE COUNTER
	LDA SHAPE.EXISTING_DATA.BUFFER,Y			;LOAD SHAPE BYTE (2nd screen byte of the tile)
	LDY SHAPE.DRAW.CURRENT.SBYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	STA (LINE.BASE.ADDR1),Y	
	;**FALLS THROUGH**
	
.NEXT.LINE
;next line, reset screen byte	
	DEC SHAPE.DRAW.CURRENT.SBYTE			;SWITCH BACK TO 1ST SCREEN BYTE IN THE TILE	;**OPT** if the counters are updated after the CMP/branch it might save 1 INC SHAPE.DRAW.CURRENT.SBYTE instruction in the main loop because SHAPE.DRAW.CURRENT.SBYTE will be in 2nd position when this loop ends. 
	INC SHAPE.DRAW.LINE_ROW.NUMBER
	
	INC SHP.TBL.CNTR		;NEXT SHAPE BYTE
	INC TILE.LINE.CNTR		;NEXT TILE LINE
	LDX TILE.LINE.CNTR 		;restore tile line number to X-REG

;exit test	
	CPX TILE.LINE.STOP		;IS TILE DONE?							
	BCS .DONE				
	JMP .DRAW.LOOP
.DONE	
	LDA #$00
	STA PAGE.FOREGROUND.OVERRIDE
				
.RESTORE.REGISTERS
	PLA
	TAY
	PLA
	TAX
	
	RTS

@END	

.TARGET.HIT.CHECK
@START
;PARAMETERS: YREG (screen index), COMBAT.EXPLOSION.PROB

;SAVE PARAMETERS
	TXA
	PHA
	
;WAS A TARGET PRESENT?				
	;YREG = screen index
	LDA SCREEN.MO_SPRITE.DATA,Y
	CMP #$FF 
	BEQ .CHECK.TARGET_HIT.EXIT ;if no S_ENTITY is present at this screen location then exit, don't record a target hit

	
; ;MTT CHECK

		; ;is S_ENTITY a tile of a multi-tile mob other than the index tile (upper left tile)?
		; ;(the upper left tile is the only tile of a multi-tile mob that is a valid character sheet index)
		; ;Y-REG: search loop index
	; JSR COMBAT.TARGET.SEARCH.MTT.CHECK
		; ; ;RETURN VALUE: ACC = ($00 =  MTT index tile found | $01 = MTT not found at this screen location | $02 = MTT tile found other than index tile)
		; CMP #$02 ;i S_ENTITY found a non-index tile of an MTT S_ENTTY? (i.e. a tile other than the upper left of the MTT)
		; BEQ	.CHECK.TARGET_HIT.EXIT	;skip this S_ENTITY
	
;IS THE HIT TILE THE SPELLCASTER?
	CPY COMBAT.ATTACKER.SINDEX
	BEQ .CHECK.TARGET_HIT.EXIT
	
;IS THIS A DUPLICATE HIT?
;(note: this routine is needed because the iteration patter of some spells results in tiles being examined more than once, which could result
;in this routine being called more than once for the same tile)

	LDX #$00
	TYA ;transfer screen tile location to ACC
.DUPLICATE.CHECK.LOOP
	CMP COMBAT.TARGET_HIT.DB,X ;if S_ENTITY in current screen location was already recorded as a hit, then exit
	BEQ	.CHECK.TARGET_HIT.EXIT
	INX
;	CPX #COMBAT.TARGET_HIT.DB.STOP_VALUE
	CPX #COMBAT.TARGET_HIT.DB.SIZE
	BNE .DUPLICATE.CHECK.LOOP
		
	
;RECORD NEW TARGET HIT	
	;YREG = screen index
	TYA
	LDX COMBAT.TARGET_HIT.DB.INDEX
	STA COMBAT.TARGET_HIT.DB,X
	;write stop value
	;(note: each time a new target hit is recorded the stop value is advanced 1 byte in the array)
	LDA #COMBAT.TARGET_HIT.DB.STOP_VALUE
	STA COMBAT.TARGET_HIT.DB+$1,X

			; ;JSR KEYIN
			; STA TEMP
			; CMP #$D1
			; BNE .TEMP
			; ;LDA #$AA
			; LDA COMBAT.TARGET_HIT.DB,X
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP

	INC COMBAT.TARGET_HIT.DB.INDEX	;next target_hit record

.CHECK.TARGET_HIT.EXIT

;RESTORE PARAMETERS
	PLA
	TAX
			
	RTS	
@END
	
	.EP ;end patch mode
;--------
GE.SPELL.FIREBALL.END ;marks the end address of the code block for this spell
@END
;=======================

GE.SPELL.LIGHTNING.START ;marks the start address of the code block for this spell
;--------
@START
	.PH SPELL.FILE.CODE_BLOCK.BUFFER.START ;start patch 

GE.SPELL.CHAIN_LIGHTNING ;requires GE.BUILD.DISTANCE.DB, GE.SPELL.LIGHTNING
@START
;PARAMETERS: COMBAT.SPELL_CASTER.GMAP.X, COMBAT.SPELL_CASTER.GMAP.Y, COMBAT.PLAYER_SELECTED.TARGET.GMAP.X, COMBAT.PLAYER_SELECTED.TARGET.GMAP.Y, COMBAT.EXPLOSION.RADIUS
;ENTRANCE: DIRECT
;RETURN: targets hit (COMBAT.TARGET_HIT.DB)
	
;=====================SUBROUTINE DOCUMENTATION====================================
;
;-target hits
;COMBAT.TARGET_HIT.DB is populated by GE.SPELL.LIGHTNING which is called by this routine.
;
;=================================================================================

.INIT
	;ACC = parameter
	STA COMBAT.EXPLOSION.RADIUS

	JSR COMBAT.PLAY.SOUND.ELECTRICITY

	JSR GE.BUILD.DISTANCE.DB

		; LDA #$EA
		; JSR PREP.BRK
		; BRK
		
			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; LDA #$EA			
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP
			
	;PARMS FOR CALL TO LIGHTNING
	;POINT THE PARMS FOR THIS ROUTINE TO THESE VARS SO NO NEED TO COPY?
	;FP.SOURCE.GMAP.X, FP.SOURCE.GMAP.Y, FP.TARGET.GMAP.X, FP.TARGET.GMAP.Y, 

	LDA COMBAT.PLAYER_SELECTED.TARGET.GMAP.X
	;LDA FP.TARGET.GMAP.X
	STA CL.TARGET_LAST.GMAP.X

	LDA COMBAT.PLAYER_SELECTED.TARGET.GMAP.Y	
	;LDA FP.TARGET.GMAP.Y
	STA CL.TARGET_LAST.GMAP.Y

;GENERATE BOLT FROM SPELL CASTER TO INITIAL TARGET

	JSR GE.SPELL.LIGHTNING.ALTERNATE.ENTRANCE
	
	JSR .INBETWEEN.DRAWS.ROUTINE

	;if there are no records in database, exit
	;(note: this happens when there are no S_ENTITIES within the radius specified by the calling routine)
	LDA COMBAT.S_ENTITY.DISTANCE.LENGTH
	BEQ .EXIT
	

		
			; LDA #$AA
			; ; LDX #COMBAT.S_ENTITY.DISTANCE.DB
			; ; LDY /COMBAT.S_ENTITY.DISTANCE.DB
			; ;LDA SCREEN.MO_SPRITE_TYPE.DATA,Y	;load s_entity type
			; LDX COMBAT.S_ENTITY.FOUND.GMAP.X
			; LDY COMBAT.S_ENTITY.FOUND.GMAP.Y
			; JSR PREP.BRK
			; BRK
			
			
	LDX #$00
.GENERATE.LIGHTNING.LOOP
		;SET PARAMETERS
		LDA CL.TARGET_LAST.GMAP.X
		STA FP.SOURCE.GMAP.X

		LDA CL.TARGET_LAST.GMAP.Y
		STA FP.SOURCE.GMAP.Y
		
		LDA COMBAT.S_ENTITY.DISTANCE.DB+$1,X  ;load GMAP X-AXIS of next S_ENTITY in database
		STA FP.TARGET.GMAP.X

		LDA COMBAT.S_ENTITY.DISTANCE.DB+$2,X  ;load GMAP Y-AXIS of next S_ENTITY in database
		STA FP.TARGET.GMAP.Y

		
			
		;save registers
		TXA 
		PHA 
	JSR GE.SPELL.LIGHTNING.ALTERNATE.ENTRANCE
		;restore registers
		PLA
		TAX
		
			; JSR KEYIN
			; STA TEMP
			; CMP #$D1
			; BNE .TEMP
			; LDA #$AA
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP
			
	JSR .INBETWEEN.DRAWS.ROUTINE
			
	;update next source S_ENTITY
	;(note: the target from this iteration will be the source in the next iteration)
	LDA FP.TARGET.GMAP.X
	STA CL.TARGET_LAST.GMAP.X

	LDA FP.TARGET.GMAP.Y
	STA CL.TARGET_LAST.GMAP.Y
		
	;increment COMBAT.S_ENTITY.DISTANCE.DB index
	TXA
	CLC
	ADC #$03 ;next distance database record
	TAX
	CMP COMBAT.S_ENTITY.DISTANCE.LENGTH
	BCC .GENERATE.LIGHTNING.LOOP
		; BCS .EXIT
		; JMP .GENERATE.LIGHTNING.LOOP

		;**FALLS THROUGH**

	
	
		; LDA #$AB
		; LDX #COMBAT.S_ENTITY.DISTANCE.DB
		; LDY /COMBAT.S_ENTITY.DISTANCE.DB
		; JSR PREP.BRK
		; BRK
.EXIT

.SET.CRITICAL_HIT.FLAGS
@START
;(engaged s_entity screen index)

		;**warning: active player character sheet will be loaded on exit
		LDY COMBAT.ATTACKER.SINDEX
		LDA #COMBAT.STATS.CRTL_HIT.NOT_ENGAGED_VALUE ;set value to update.
.UPDATE.CHR_SHEET.ENGAGED.INDEX ;**OPT** Memory. This routine was written as as subroutine, but is being used inline here and it could be made more efficient. 
;PARAMETERS: YREG = screen index of attacker, ACC = value to write to CHR_SHEET.PC_MOB.ENGAGED.SINDEX
;ENTRANCE: direct
;RETURN: saved attacker character sheet with updated CHR_SHEET.PC_MOB.ENGAGED.SINDEX field

.SAVE.PARAMETERS.ENAGED
	;ACC = value to write to CHR_SHEET.PC_MOB.ENGAGED.SINDEX
	STA COMBAT.ENGAGED_SINDEX.UPDATE.PARM
	
	;read attacker (MOB) character sheet
		
		;YREG = COMBAT.ATTACKER.SINDEX
		LDA #$00 ;set read mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet

			
	;set the engaged field to the updated value
	;(expected to be the screen index of the defender or $FF to indicate no engagement)
	LDA COMBAT.ENGAGED_SINDEX.UPDATE.PARM
	STA CHR_SHEET.PC_MOB.ENGAGED.SINDEX

			
		;YREG = COMBAT.ATTACKER.SINDEX
		LDA #$01 ;set write mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet

@END
	
	RTS

	
.INBETWEEN.DRAWS.ROUTINE
;ERASE.LIGHTNING
	JSR FLIP.PAGE ;effectively erases the shape because the shape was only drawn to the foreground
	
	;!!!!! BSR BANK1 !!!!
	JSR COPY.SCREEN.ENTRANCE ;**DON'T REMOVE** (or the screen goes chaotic sometimes when collisions occur with double mover mobs. MO.DRAW must need the graphics pages in sync for some reason). UDPATE: I think this is needed because sometimes, due to key press abort, no copy.screen was done after the last move, so the pages are out of syncing going into MOVE.PASS, which is triggered by non-movement commands like (3)ZAP, Push, Board, etc.

		
		
	RTS
	
@END

GE.BUILD.DISTANCE.DB 
@START
;PARAMETERS: COMBAT.SPELL_CASTER.GMAP.X, COMBAT.SPELL_CASTER.GMAP.Y, COMBAT.PLAYER_SELECTED.TARGET.GMAP.X, COMBAT.PLAYER_SELECTED.TARGET.GMAP.Y, COMBAT.EXPLOSION.RADIUS

;DOCUMENTATION

;can only be used with angled shapes because shape.hopper+$40 is used
;to store the distance database. angled shapes only use up to +$3F, but animated shapes
;use all of shape hopper. 

;
;COMBAT.S_ENTITY.DISTANCE.DB datagram
;field 0 (distance)
;Byte 0		Byte 1			Byte 2					
;distance	GMAP.X (target)	GMAP.Y (target)	


;***temp
		; LDX #$00
; .INIT.LOOP
		; LDA #$00
		; STA COMBAT.S_ENTITY.DISTANCE.DB,X
		; INX
		; CPX #$72
		; BNE .INIT.LOOP
		
.INIT.VARIABLES
	LDX #$00 ;COMBAT.S_ENTITY.DISTANCE.DB index
	LDY #$00 ;screen array index
	;STY COMBAT.S_ENTITY.DISTANCE.LENGTH+$0
	STY COMBAT.S_ENTITY.DISTANCE.LENGTH+$1

		
CL.SCREEN.ARRAY.LOOP ;main loop	


	LDA SCREEN.MO_SPRITE.DATA,Y ;load S_ENTITY map object index, if one is present at this screen location
	CMP #$FF ;is there a map object at this screen array location?
	BNE CL.S_ENTITY.FOUND
	;**FALLS THROUGH**	

CL.INCREMENT.SCREEN_ARRAY.INDEX	
	INY ;increment screen array index
	CPY #SCREEN.ARRAY.LAST_ELEMENT2 ;at end of screen array?
	BNE CL.SCREEN.ARRAY.LOOP
	;**FALLS THROUGH**	


.WRITE.DATABASE.LENGTH

		; LDA #$AB
		; ; LDX #COMBAT.S_ENTITY.DISTANCE.DB
		; ; LDY /COMBAT.S_ENTITY.DISTANCE.DB
		; JSR PREP.BRK
		; BRK

	STX COMBAT.S_ENTITY.DISTANCE.LENGTH+$0 ;save index as length LO byte. X is init with 0 but was incremented after the last record write, so X = byte qty. 
	;HO byte was init to #$00. The array will always be > $100 bytes in length, so it can stay at init value. 
	
.EXIT

		
	RTS	
	
CL.S_ENTITY.FOUND ;an S_ENTITY was found at at the screen locatio in the current iteration of the loop
	
	
.DETERMINE.S_ENTITY.TYPE
	TXA
	PHA ;push distance database index to stack

	LDA SCREEN.MO_SPRITE.DATA,Y	;load s_entity index
	TAX ;transfer screen array index to X-REG
	
	LDA SCREEN.MO_SPRITE_TYPE.DATA,Y	;load s_entity type
	AND #$87 ;mask-out MTT flags (bits 3-6)
	;branch based on the type of S_ENTITY found
	CMP #S_ENTITY_TYPE.C_MOB
	BEQ	.S_ENTITY.IS.C_MOB  
	CMP #S_ENTITY_TYPE.SPECIAL
	BEQ .S_ENTITY.IS.SPECIAL
	CMP #S_ENTITY_TYPE.PC
	BEQ .S_ENTITY.IS.PC
	
	;**FALLS THROUGH**
	
.S_ENTITY_TYPE.ERROR
;.DETERMINE.S_ENTITY.TYPE (CL.S_ENTITY.FOUND) reports unexpected S_ENTITY type value.
	JSR PREP.BRK
	BRK
	
.S_ENTITY.IS.C_MOB 
	;load s_entity map object record data
	LDA COMBAT.MAP_OBJECTS.MOB+$0,X	;load sprite GMAP X-axis
	STA COMBAT.S_ENTITY.FOUND.GMAP.X	;save

	LDA COMBAT.MAP_OBJECTS.MOB+$1,X	;load sprite GMAP Y-axis
	STA COMBAT.S_ENTITY.FOUND.GMAP.Y	

		; TYA
		; ;LDA #$AB
		; LDX COMBAT.S_ENTITY.FOUND.GMAP.X
		; LDY COMBAT.S_ENTITY.FOUND.GMAP.Y
		; JSR PREP.BRK
		; BRK
		
	JMP .RESTORE.DB.INDEX


.S_ENTITY.IS.SPECIAL
	;load s_entity map object record data	
	LDA COMBAT.MAP_OBJECTS.SPECIAL.TRUE_INDEX+$0,X	;load sprite GMAP X-axis
	STA COMBAT.S_ENTITY.FOUND.GMAP.X	;save

	LDA COMBAT.MAP_OBJECTS.SPECIAL.TRUE_INDEX+$1,X	;load sprite GMAP Y-axis
	STA COMBAT.S_ENTITY.FOUND.GMAP.Y	
	JMP .RESTORE.DB.INDEX

.S_ENTITY.IS.PC
	LDA COMBAT.MAP_OBJECTS.PC+$0,X	;load sprite GMAP X-axis
	STA COMBAT.S_ENTITY.FOUND.GMAP.X	;save

	LDA COMBAT.MAP_OBJECTS.PC+$1,X	;load sprite GMAP Y-axis
	STA COMBAT.S_ENTITY.FOUND.GMAP.Y	

	;**FALLS THROUGH**	

.RESTORE.DB.INDEX
	PLA ;pull distance database index from stack
	TAX
	
.CALCULATE.TARGET.DISTANCE
	;is the current S_ENTITY the target?
	LDA COMBAT.S_ENTITY.FOUND.GMAP.X
	CMP COMBAT.PLAYER_SELECTED.TARGET.GMAP.X 
	BNE .TARGET.CHECK.COMPLETE
	LDA COMBAT.S_ENTITY.FOUND.GMAP.Y
	CMP COMBAT.PLAYER_SELECTED.TARGET.GMAP.Y
	BEQ	.EXIT ;skip this S_ENTITY, do not increment database
.TARGET.CHECK.COMPLETE
	
		;SET PARAMETERS
		LDA COMBAT.S_ENTITY.FOUND.GMAP.X
		STA PARM2.GMAP.X

		LDA COMBAT.PLAYER_SELECTED.TARGET.GMAP.X ;load target GMAP.X
		STA PARM1.GMAP.X

		LDA COMBAT.S_ENTITY.FOUND.GMAP.Y
		STA PARM2.GMAP.Y	

		LDA COMBAT.PLAYER_SELECTED.TARGET.GMAP.Y ;load target GMAP.Y
		STA PARM1.GMAP.Y
	JSR CALCULATE.DISTANCE		
		;ACC = DISTANCE
		;RETURN MAP_OBJECTS.X_ADJ = X-AXIS differential
		;RETURN MAP_OBJECTS.Y_ADJ = Y-AXIS differential
		STA SAVED.ACC.LOCAL ;save distance
	
	LDA MAP_OBJECTS.X_ADJ
	CMP COMBAT.EXPLOSION.RADIUS
	;don't increment index if distance is greater than radius
	BEQ .Y_AXIS.TEST ;if distance = radius
	BCS .EXIT ;implicit greater than test (if distance is greater than radius)
	;**FALLS THROUGH**	
.Y_AXIS.TEST
	LDA MAP_OBJECTS.Y_ADJ
	CMP COMBAT.EXPLOSION.RADIUS
	;don't increment index if distance is greater than radius
	BEQ .WRITE.DATABASE.RECORD ;if distance = radius
	BCS .EXIT ;implicit greater than test (if distance is greater than radius)
	
	;**FALLS THROUGH**	

.WRITE.DATABASE.RECORD
	LDA SAVED.ACC.LOCAL ;restore distance

	;ACC = DISTANCE
	STA COMBAT.S_ENTITY.DISTANCE.DB+$0,X
	LDA COMBAT.S_ENTITY.FOUND.GMAP.X
	STA COMBAT.S_ENTITY.DISTANCE.DB+$01,X ;this is a convenient location to save the target X-axis to the distance database

	LDA COMBAT.S_ENTITY.FOUND.GMAP.Y
	STA COMBAT.S_ENTITY.DISTANCE.DB+$02,X ;this is a convenient location to save the target Y-axis to the distance database		

		;**FALLS THROUGH** (if distance is less than radius)
		
.INCREMENT.DATABASE.INDEX
	;increment COMBAT.S_ENTITY.DISTANCE.DB index
	TXA
	CLC
	ADC #$03 ;next database record
	TAX
	CMP #$72 ;this is based on up to 16 mobs, 16 specials, and 6 player characters, using 3 byte records.
			 ;in the distance database.
	BCS .EXIT.SUBROUTINE
	;**FALLS THROUGH**
	
.EXIT


			
		; CPX #$03
		; BNE .TEMP
		; ;LDA #$AA
		; ; LDX #COMBAT.S_ENTITY.DISTANCE.DB
		; ; LDY /COMBAT.S_ENTITY.DISTANCE.DB
		; LDA SCREEN.MO_SPRITE_TYPE.DATA,Y	;load s_entity type
		; LDX COMBAT.S_ENTITY.FOUND.GMAP.X
		; LDY COMBAT.S_ENTITY.FOUND.GMAP.Y
		; JSR PREP.BRK
		; BRK
;.TEMP

		
	JMP CL.INCREMENT.SCREEN_ARRAY.INDEX

.EXIT.SUBROUTINE
	LDY #SCREEN.ARRAY.LAST_ELEMENT2 ;force exit of screen array loop by setting index to the exit value
	JMP CL.INCREMENT.SCREEN_ARRAY.INDEX	
	

	
@END

GE.SPELL.LIGHTNING
@START
;PARAMETERS: FP.SOURCE.GMAP.X, FP.SOURCE.GMAP.Y, FP.TARGET.GMAP.X, FP.TARGET.GMAP.Y
;ENTRANCE: DIRECT

;=====================SUBROUTINE DOCUMENTATION====================================
;
;-target hits
;COMBAT.TARGET_HIT.DB is populated by a call to .TARGET.HIT.CHECK from this routine
;
;=================================================================================

	JSR COMBAT.PLAY.SOUND.ELECTRICITY

GE.SPELL.LIGHTNING.ALTERNATE.ENTRANCE			
;SELECT SPELL ROUTINE #1
	LDA COMBAT.SPELL_CODE.CAST
	CMP #SPELL_CODE.LIGHTNING.BOLT
	BEQ .BOLT.LEAVES.NO_TRAIL
	;default
	;#SPELL_CODE.INFERNAL.BLAST branch already occured above in SELECT SPELL ROUTINE #1
	;#SPELL_CODE.LIGHTNING.BLAST
	;#SPELL_CODE.CHAIN_LIGHTNING.MEDIUM
	;#SPELL_CODE.CHAIN_LIGHTNING.LARGE
	;#SPELL_CODE.LIGHTNING.MEGA_BLAST
	
	;**FALLS THROUGH**
	
.BOLT.LEAVES.TRAIL
;SELECT SPELL ROUTINE
	LDA COMBAT.SPELL_CODE.CAST
	CMP #SPELL_CODE.LIGHTNING.MEGA_BLAST
	BEQ .LIGHTNING.MEGA_BLAST

	;**FALLS THROUGH**
	
		;LDA #$00  ;disable (support was added for this)
		LDA #COMBAT.PROJECTILE.DELAY.LIGHTNING	;(time target = 1.6 seconds when there are 7 empty tiles between source and target)
		STA SHAPE.MOVE.DELAY_LENGTH ;The length of the delay between the shape draw and shape erase.

		LDA #$01 ;set of off
		STA SHAPE.MOVE.SCREEN_ANIMATION.TOGGLE ;($00 = screen animation on | >=$01 = screen animation off)
		STA SHAPE.MOVE.ERASE.TOGGLE	;($00 = shape erase on | >=$01 = shape erase off)
	
		LDA #$00	;set mode to angled shape ;($00 = angled static shape | >=$01 = non-angled, animated shape)
	JSR COMBAT.FIRE.PROJECTILE
			
		
	JMP .UPDATE.TARGET_HIT.DATABASE	

.LIGHTNING.MEGA_BLAST

	LDX #$00
.MULTIPLE_BLASTS.LOOP
		;LDA #$00  ;disable (support was added for this)
		LDA #COMBAT.PROJECTILE.DELAY.LIGHTNING	;(time target = 1.6 seconds when there are 7 empty tiles between source and target)
		STA SHAPE.MOVE.DELAY_LENGTH ;The length of the delay between the shape draw and shape erase.

		LDA #$01 ;set of off
		STA SHAPE.MOVE.SCREEN_ANIMATION.TOGGLE ;($00 = screen animation on | >=$01 = screen animation off)
		STA SHAPE.MOVE.ERASE.TOGGLE	;($00 = shape erase on | >=$01 = shape erase off)
	
		LDA #$00	;set mode to angled shape ;($00 = angled static shape | >=$01 = non-angled, animated shape)
	JSR COMBAT.FIRE.PROJECTILE

;ERASE PROJECTILE
@START
	LDA #$00
	STA SHAPE.MOVE.ERASE.TOGGLE	;flip toggle to off (default) 

	JSR FLIP.PAGE ;effectively erases the shape because the shape was only drawn to the foreground
	
	;SYNC HI-RES PAGES
	;Note: needed because the pages need to be in sync for animation to work properly. 
	;!!!!! BSR BANK1 !!!!
	JSR COPY.SCREEN.ENTRANCE ;**DON'T REMOVE** (or the screen goes chaotic sometimes when collisions occur with double mover mobs. MO.DRAW must need the graphics pages in sync for some reason). UDPATE: I think this is needed because sometimes, due to key press abort, no copy.screen was done after the last move, so the pages are out of syncing going into MOVE.PASS, which is triggered by non-movement commands like (3)ZAP, Push, Board, etc.

		
; .DRAW.PROJECTILE.ON.TARGET

	; ;refresh shape table to make sure its are in the correct color positions. No way of knowing the position it was left in after the last draw in COMBAT.SHAPE.MOVE 
	; JSR CONVERT.MASTER.SHAPE_TABLE

		; DEC SHAPE.SBYTE.TARGET.ORIGINAL
		; LDA SHAPE.SBYTE.TARGET.ORIGINAL
		; STA SHAPE.DRAW.CURRENT.SBYTE
		
		; LDA SHAPE.LINE.TARGET
		; STA SHAPE.DRAW.LINE_ROW.NUMBER
		; ;STA TILE.LINE.TARGET

		; ;foreground: draw shape in current position
		; LDA #$01
		; STA PAGE.FOREGROUND.OVERRIDE
		; LDA #$00 ;set draw mode
	; JSR COMBAT.DRAW.SHAPE
	
		; LDA #$80
		; JSR WAIT.LOOP

; ;ERASE THE PROJECTILE FROM TARGET

		; ;foreground: draw shape in current position
		; LDA #$01
		; STA PAGE.FOREGROUND.OVERRIDE
		; LDA #$01 ;set draw mode
	; JSR COMBAT.DRAW.SHAPE
@END

	
	;increment and exit test
	INX
	CPX #$05
	BNE .MULTIPLE_BLASTS.LOOP
	
	JMP .UPDATE.TARGET_HIT.DATABASE	
	
	
.BOLT.LEAVES.NO_TRAIL	;infernal blast, lightning blast, chain lightning
		;LDA #$00  ;disable (support was added for this)
		LDA #COMBAT.PROJECTILE.DELAY.LIGHTNING	;(time target = 1.6 seconds when there are 7 empty tiles between source and target)
		;LDA #$80 ;arrow speed
		STA SHAPE.MOVE.DELAY_LENGTH ;The length of the delay between the shape draw and shape erase.

		LDA #$01 ;set of off
		STA SHAPE.MOVE.SCREEN_ANIMATION.TOGGLE ;($00 = screen animation on | >=$01 = screen animation off)
		
		LDA #$00
		STA SHAPE.MOVE.ERASE.TOGGLE	;($00 = shape erase on | >=$01 = shape erase off)
		
		LDA #$00	;set mode to angled shape ;($00 = angled static shape | >=$01 = non-angled, animated shape)
	JSR COMBAT.FIRE.PROJECTILE	

		; JSR KEYIN
		; JSR PREP.BRK
		; BRK
		
	;**FALLS THROUGH**
	
.UPDATE.TARGET_HIT.DATABASE	

;get screen location of prospective GMAP.X/Y of target
		LDA FP.TARGET.GMAP.X ;load new special S_ENTITYY GMAP.X
		STA PARM.GMAP.X
		LDA FP.TARGET.GMAP.Y ;load new special S_ENTITY GMAP.Y
		STA PARM.GMAP.Y				
	JSR CONVERT.COLUMN_ROW.TO.SINDEX
		LDY RETURN.SCREEN_ARRAY_INDEX ;used below


				
	JSR .TARGET.HIT.CHECK
				
.SET.CRITICAL_HIT.FLAGS
@START
;(engaged s_entity screen index)


				
		;**warning: attacker character sheet will be loaded on exit
		LDY COMBAT.ATTACKER.SINDEX
		LDA COMBAT.TARGET_HIT.DB+$0 ;set value to update (sindex of the target)	
.UPDATE.CHR_SHEET.ENGAGED.INDEX ;**OPT** Memory. This routine was written as as subroutine, but is being used inline here and it could be made more efficient. 
;PARAMETERS: YREG = screen index of attacker, ACC = value to write to CHR_SHEET.PC_MOB.ENGAGED.SINDEX
;ENTRANCE: direct
;RETURN: saved attacker character sheet with updated CHR_SHEET.PC_MOB.ENGAGED.SINDEX field

.SAVE.PARAMETERS.ENAGED
	;ACC = value to write to CHR_SHEET.PC_MOB.ENGAGED.SINDEX
	STA COMBAT.ENGAGED_SINDEX.UPDATE.PARM
	
	;read attacker (MOB) character sheet
		
		;YREG = COMBAT.ATTACKER.SINDEX
		LDA #$00 ;set read mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet

			
	;set the engaged field to the updated value
	;(expected to be the screen index of the defender or $FF to indicate no engagement)
	LDA COMBAT.ENGAGED_SINDEX.UPDATE.PARM
	STA CHR_SHEET.PC_MOB.ENGAGED.SINDEX

			
		;YREG = COMBAT.ATTACKER.SINDEX
		LDA #$01 ;set write mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet
			
@END

.EXIT	

	RTS
	
@END

.TARGET.HIT.CHECK
@START
;PARAMETERS: YREG (screen index), COMBAT.EXPLOSION.PROB


			
;SAVE PARAMETERS
	TXA
	PHA
	
;WAS A TARGET PRESENT?				
	;YREG = screen index
	LDA SCREEN.MO_SPRITE.DATA,Y
	CMP #$FF 
	BEQ .CHECK.TARGET_HIT.EXIT ;if no S_ENTITY is present at this screen location then exit, don't record a target hit

	
;;MTT CHECK

		; ;is S_ENTITY a tile of a multi-tile mob other than the index tile (upper left tile)?
		; ;(the upper left tile is the only tile of a multi-tile mob that is a valid character sheet index)
		; ;Y-REG: search loop index
	; JSR COMBAT.TARGET.SEARCH.MTT.CHECK
			; ;RETURN VALUE: ACC = ($00 =  MTT index tile found | $01 = MTT not found at this screen location | $02 = MTT tile found other than index tile)
		; CMP #$02 ;i S_ENTITY found a non-index tile of an MTT S_ENTTY? (i.e. a tile other than the upper left of the MTT)
		; BEQ	.CHECK.TARGET_HIT.EXIT	;skip this S_ENTITY


				; lda #$aa
				; ldx #COMBAT.TARGET_HIT.DB
				; ldy /COMBAT.TARGET_HIT.DB
				; ;LDx COMBAT.ABORT.FLAG		;($00 = not set, $01 = set)
				; ;LDy SELECT_TARGET.ACTIVE.SELECTION.SINDEX ;load defender's (target) sindex
				; jsr prep.brk
				; brk
				
;IS THE HIT TILE THE SPELLCASTER?
	CPY COMBAT.ATTACKER.SINDEX
	BEQ .CHECK.TARGET_HIT.EXIT
		
;IS THIS A DUPLICATE HIT?
;(note: this routine is needed because the iteration patter of some spells results in tiles being examined more than once, which could result
;in this routine being called more than once for the same tile)

	LDX #$00
	TYA ;transfer screen tile location to ACC
.DUPLICATE.CHECK.LOOP
	CMP COMBAT.TARGET_HIT.DB,X ;if S_ENTITY in current screen location was already recorded as a hit, then exit
	BEQ	.CHECK.TARGET_HIT.EXIT
	INX
	CPX #COMBAT.TARGET_HIT.DB.SIZE
	BNE .DUPLICATE.CHECK.LOOP
		
			; LDA #$AA
			; LDX COMBAT.TARGET_HIT.DB
			; LDY COMBAT.TARGET_HIT.DB+$1
			; JSR PREP.BRK
			; BRK
			
;RECORD NEW TARGET HIT	
	;YREG = screen index
	TYA
	LDX COMBAT.TARGET_HIT.DB.INDEX
	STA COMBAT.TARGET_HIT.DB,X
	;write stop value
	;(note: each time a new target hit is recorded the stop value is advanced 1 byte in the array)
	LDA #COMBAT.TARGET_HIT.DB.STOP_VALUE
	STA COMBAT.TARGET_HIT.DB+$1,X

	INC COMBAT.TARGET_HIT.DB.INDEX	;next target_hit record

.CHECK.TARGET_HIT.EXIT
;TARGET DATABASE READ
			; ;TROUBLESHOOTING LOOP
			; LDX #$00
; .TEST.LOOP			
			; LDA COMBAT.TARGET_HIT.DB,X
			; STA $BF00,X
			; INX
			; BNE .TEST.LOOP	
			
			; LDA #$AB
			; JSR PREP.BRK
			; BRK
			

;RESTORE PARAMETERS
	PLA
	TAX
			
	RTS	
@END

;SUBROUTINES
;.TARGET.SEARCH.MTT.CHECK
@START
; ;PARAMETERS: MAP_OBJECT.RECORD.READ(8) = data for S_ENTITY found at current screen location
; ;ENTRANCE: various identify target routines
; ;RETURN: ACC = $00 (MTT not found at this screen location or MTT index tile found | $01 = MTT tile found other than index tile)

; ;SAVE REGISTERS
	; ; TXA
	; ; PHA
	; TYA
	; PHA

; ;INIT

	; ;read map object data at screen location of S_ENTITY found
		; LDA #$08 ;read $8 bytes    ;**OPT** Memory. I think this routine could just default to an 8byte read and note that the last 4 bytes are garbage for S_ENTITIES with 4 byte records. The code that uses the return data already knows that. 
		; ;Y-REG (screen array index)
	; JSR READ.MAP_OBJECT.ARRAY
		; ;RETURN VALUE: MAP_OBJECT.RECORD.READ(8), MAP_OBJECT.ARRAY.S_ENTITY.TYPE, MAP_OBJECT.ARRAY.MO_INDEX

	
; ;IS MOB MULTI-TILE (MTT)?	
	; LDA #$08 ;the AND mask value needed for the BIT operation below. 
	; BIT MAP_OBJECT.RECORD.READ+$3 ;test bit3 of MAP_OBJECT.RECORD.READ+$03
	; BEQ .MTT_CHECK.MTT_NOT_FOUND ;branch if bit3 is not set (mob is not multi-tile)

	
; ;IS CURRENT SCREEN LOCATION THE MTT INDEX TILE?
; ;(index tile = upper left tile)

	; ;screen edge check
	; LDA MAP_OBJECT.RECORD.READ+$0 ;load X-AXIS
	; CMP #COMBAT.SCREEN.COLUMN.LAST
	; BEQ .MTT_CHECK.MTT_INDEX_TILE_NOT_FOUND

	; LDA MAP_OBJECT.RECORD.READ+$1 ;load Y-AXIS
	; CMP #COMBAT.SCREEN.ROW.LAST
	; BEQ .MTT_CHECK.MTT_INDEX_TILE_NOT_FOUND

	
	; ;save data for MTT index tile
	; LDA	MAP_OBJECT.ARRAY.S_ENTITY.TYPE
	; STA .MTT_CHECK.S_ENTITY.TYPE
	; LDA	MAP_OBJECT.ARRAY.MO_INDEX
	; STA .MTT_CHECK.MO_INDEX	
		


; ;SCREEN TILE LOCATION +1 right

	; INY ;screen location +1 right	
	
	; ;get MO data for screen location +1 right
		; LDA #$00 ;init value. defaults to found, and then the value is only change if the S_ENTITY is not found. 
		; STA MAP_OBJECT.ARRAY.S_ENTITY.NOT_FOUND
		; LDA #$FF
		; STA MAP_OBJECT.ARRAY.ERROR_TRAP_OVERRIDE		
		; LDA #$08 ;read $8 bytes    ;**OPT** Memory. I think this routine could just default to an 8byte read and note that the last 4 bytes are garbage for S_ENTITIES with 4 byte records. The code that uses the return data already knows that. 
		; ;Y-REG (screen array index)
	; JSR READ.MAP_OBJECT.ARRAY
		; ;RETURN VALUE: MAP_OBJECT.RECORD.READ(8), MAP_OBJECT.ARRAY.S_ENTITY.TYPE, MAP_OBJECT.ARRAY.MO_INDEX, MAP_OBJECT.ARRAY.S_ENTITY.NOT_FOUND
		
		; ;was S_ENTITY found at this screen tile location?
		; ;(if no, then an MTT index tile was not found at original screen location)
		; LDA MAP_OBJECT.ARRAY.S_ENTITY.NOT_FOUND ;($00 = S_ENTITY found | $01 = S_ENTITY not found)
		; BNE .MTT_CHECK.MTT_INDEX_TILE_NOT_FOUND ;branch if no

	; ;does tile contain an S_ENTITY that is part of the original MTT S_ENTITY found?
	; ;(if no, then the MTT tile found is not the index tile)
	; LDA MAP_OBJECT.ARRAY.S_ENTITY.TYPE
	; CMP .MTT_CHECK.S_ENTITY.TYPE ;S_ENTITY type found at original screen location
	; BNE .MTT_CHECK.MTT_INDEX_TILE_NOT_FOUND ;branch if no
	; LDA MAP_OBJECT.ARRAY.MO_INDEX
	; CMP .MTT_CHECK.MO_INDEX	;MO index found at original screen location
	; BNE .MTT_CHECK.MTT_INDEX_TILE_NOT_FOUND ;branch if no
	
					; ; lda #$ab
					; ; LDx MAP_OBJECT.ARRAY.S_ENTITY.NOT_FOUND ;($00 = S_ENTITY found | $01 = S_ENTITY not found)
					; ; jsr prep.brk
					; ; brk
					
; ;SCREEN TILE LOCATION +1 down

	; ;change screen tile location
	; DEY ;return to original screen location of found S_ENTITY
	; TYA
	; CLC
	; ADC #SCREEN.ARRAY.OFFSET ;move 1 tile up from screen location
	; TAY
	
	; ;get MO data for screen location +1 up
		; LDA #$00 ;init value. defaults to found, and then the value is only change if the S_ENTITY is not found. 
		; STA MAP_OBJECT.ARRAY.S_ENTITY.NOT_FOUND
		; LDA #$FF
		; STA MAP_OBJECT.ARRAY.ERROR_TRAP_OVERRIDE		
		; LDA #$08 ;read $8 bytes    ;**OPT** Memory. I think this routine could just default to an 8byte read and note that the last 4 bytes are garbage for S_ENTITIES with 4 byte records. The code that uses the return data already knows that. 
		; ;Y-REG (screen array index)
	; JSR READ.MAP_OBJECT.ARRAY
		; ;RETURN VALUE: MAP_OBJECT.RECORD.READ(8), MAP_OBJECT.ARRAY.S_ENTITY.TYPE, MAP_OBJECT.ARRAY.MO_INDEX		

					
		; ;was S_ENTITY found at this screen tile location?
		; ;(if no, then an MTT index tile was not found at original screen location)
		; LDA MAP_OBJECT.ARRAY.S_ENTITY.NOT_FOUND ;($00 = S_ENTITY found | $01 = S_ENTITY not found)
		; BNE .MTT_CHECK.MTT_INDEX_TILE_NOT_FOUND ;branch if no

	; ;does tile contain an S_ENTITY that is part of the original MTT S_ENTITY found?
	; ;(if yes, then the MTT tile found is the index tile)
	; LDA MAP_OBJECT.ARRAY.S_ENTITY.TYPE
	; CMP .MTT_CHECK.S_ENTITY.TYPE ;S_ENTITY type found at original screen location
	; BNE .MTT_CHECK.MTT_INDEX_TILE_NOT_FOUND ;branch if no
	; LDA MAP_OBJECT.ARRAY.MO_INDEX
	; CMP .MTT_CHECK.MO_INDEX ;MO index found at original screen location
	; BNE .MTT_CHECK.MTT_INDEX_TILE_NOT_FOUND
	; ;index tile found
	; JMP .MTT_CHECK.MTT_INDEX_TILE_FOUND
	
; .MTT_CHECK.MTT_INDEX_TILE_NOT_FOUND
	; LDA #$01    ;$00 (MTT not found at this screen location or MTT index tile found | $01 = MTT tile found other than index tile)
	; STA .MTT_CHECK.RETURN_VALUE	
	; JMP .TARGET.SEARCH.MTT.CHECK.EXIT
	
; .MTT_CHECK.MTT_INDEX_TILE_FOUND	
; .MTT_CHECK.MTT_NOT_FOUND	
	; LDA #$00    ;$00 (MTT not found at this screen location or MTT index tile found | $01 = MTT tile found other than index tile)
	; STA .MTT_CHECK.RETURN_VALUE
	
	; ;**FALLS THROUGH**
	
; .TARGET.SEARCH.MTT.CHECK.EXIT

; ;RESTORE REGISTERS
	; PLA
	; TAY
	; ; PLA
	; ; TAX
	
	; LDA .MTT_CHECK.RETURN_VALUE ;load return value. ;$00 (MTT not found at this screen location or MTT index tile found | $01 = MTT tile found other than index tile)
	
	; RTS
	
; ;--------

; ;LOCAL VARIABLES
; @START
; .MTT_CHECK.S_ENTITY.TYPE	.BS $1
; .MTT_CHECK.MO_INDEX 		.BS $1	
; .MTT_CHECK.RETURN_VALUE		.BS $1

; @END
	
@END

;SHAPE TABLE: LIGHTNING BOLT
@START
LIGHTNING_BOLT.SHAPE.START
;----------
SPELL.ANGULAR.LIGHTNING_BOLT.SHAPE_0.AND_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80
SPELL.ANGULAR.LIGHTNING_BOLT.SHAPE_0.ORA_MASK			.HS	98.80.B0.86.B0.83.E0.81.C4.A9.D4.A1.90.A3.90.AF.9C.87.D8.81.F0.A9.E4.88.B0.8C.EC.86.F8.A3.80.80
				
SPELL.ANGULAR.LIGHTNING_BOLT.SHAPE_1.AND_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80
SPELL.ANGULAR.LIGHTNING_BOLT.SHAPE_1.ORA_MASK			.HS	80.80.80.80.84.A0.90.80.D8.82.8C.B8.FC.8C.E7.A7.81.8E.83.BA.DE.82.C6.8A.80.A0.80.80.80.80.80.80
				
SPELL.ANGULAR.LIGHTNING_BOLT.SHAPE_2.AND_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80
SPELL.ANGULAR.LIGHTNING_BOLT.SHAPE_2.ORA_MASK			.HS	80.80.F8.A3.EC.86.B0.8C.E4.88.F0.A9.D8.81.9C.87.90.AF.90.A3.D4.A1.C4.A9.E0.81.B0.83.B0.86.98.80
				
SPELL.ANGULAR.LIGHTNING_BOLT.SHAPE_3.AND_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80
SPELL.ANGULAR.LIGHTNING_BOLT.SHAPE_3.ORA_MASK			.HS	80.80.80.80.84.A0.80.88.C0.9A.9C.B0.B0.BE.E4.E7.F0.80.DC.C0.C0.FA.D0.E2.84.80.80.80.80.80.80.80
				
				
;OCTET 4				
SPELL.ANGULAR.LIGHTNING_BOLT.SHAPE_NE0.AND_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80
SPELL.ANGULAR.LIGHTNING_BOLT.SHAPE_NE0.ORA_MASK			.HS	E0.80.C4.99.C0.8D.90.87.D0.A1.D4.A1.E4.A0.E4.AC.E7.87.B6.80.BC.A8.98.8A.98.82.99.87.FC.A1.80.80
				
SPELL.ANGULAR.LIGHTNING_BOLT.SHAPE_NE1.AND_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80
SPELL.ANGULAR.LIGHTNING_BOLT.SHAPE_NE1.ORA_MASK			.HS	80.80.C0.89.84.B3.80.9B.80.8E.90.A3.90.83.94.83.C4.A9.E6.8A.BC.80.B0.80.98.88.CD.83.F8.A0.80.80
				
SPELL.ANGULAR.LIGHTNING_BOLT.SHAPE_NE2.AND_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80
SPELL.ANGULAR.LIGHTNING_BOLT.SHAPE_NE2.ORA_MASK			.HS	80.88.C1.80.D0.B0.90.98.98.8C.CC.A7.E6.8E.BC.BA.EC.82.C7.8A.81.A0.83.80.DE.A0.C6.8A.80.80.80.80
				
				
;OCTET 5				
SPELL.ANGULAR.LIGHTNING_BOLT.SHAPE_SE0.AND_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80
SPELL.ANGULAR.LIGHTNING_BOLT.SHAPE_SE0.ORA_MASK			.HS	80.80.FC.A1.99.87.98.82.98.8A.BC.A8.B6.80.E7.87.E4.AC.E4.A0.D4.A1.D0.A1.90.87.C0.8D.C4.99.E0.80
				
SPELL.ANGULAR.LIGHTNING_BOLT.SHAPE_SE1.AND_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80
SPELL.ANGULAR.LIGHTNING_BOLT.SHAPE_SE1.ORA_MASK			.HS	80.80.F8.A0.CD.83.98.88.B0.80.BC.80.E6.8A.C4.A9.94.83.90.83.90.A3.80.8E.80.9B.84.B3.C0.89.80.80
				
SPELL.ANGULAR.LIGHTNING_BOLT.SHAPE_SE2.AND_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80
SPELL.ANGULAR.LIGHTNING_BOLT.SHAPE_SE2.ORA_MASK			.HS	80.80.80.80.C6.8A.DE.A0.83.80.81.A0.C7.8A.C7.8A.EC.82.BC.BA.E6.8E.CC.A7.98.8C.90.98.D0.B0.C1.80
				
				
;OCTET 6				
SPELL.ANGULAR.LIGHTNING_BOLT.SHAPE_SW0.AND_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80
SPELL.ANGULAR.LIGHTNING_BOLT.SHAPE_SW0.ORA_MASK			.HS	80.80.84.BF.E0.99.C0.98.D0.98.94.BC.80.EC.E0.E7.B4.A6.80.A6.80.AB.84.8B.E0.89.B0.83.98.A3.80.86
				
SPELL.ANGULAR.LIGHTNING_BOLT.SHAPE_SW1.AND_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80
SPELL.ANGULAR.LIGHTNING_BOLT.SHAPE_SW1.ORA_MASK			.HS	80.80.84.9E.C0.B3.90.98.80.8C.80.BC.D0.E6.94.A3.C0.A9.C0.89.C4.89.F0.80.D8.81.CC.A1.90.83.80.80
				
SPELL.ANGULAR.LIGHTNING_BOLT.SHAPE_SW2.AND_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80
SPELL.ANGULAR.LIGHTNING_BOLT.SHAPE_SW2.ORA_MASK			.HS	80.80.80.80.D0.E2.84.FA.80.C0.84.80.D0.E2.C0.B6.DC.BC.F0.E6.E0.B3.B0.98.98.88.8C.8A.80.82.90.80
				
				
;OCTET 7				
SPELL.ANGULAR.LIGHTNING_BOLT.SHAPE_NW0.AND_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80
SPELL.ANGULAR.LIGHTNING_BOLT.SHAPE_NW0.ORA_MASK			.HS	80.86.98.A3.B0.83.E0.89.84.8B.84.AB.84.A6.B4.A6.E0.E7.80.EC.94.BC.D0.98.C0.98.E0.99.84.BF.80.80
				
SPELL.ANGULAR.LIGHTNING_BOLT.SHAPE_NW1.AND_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80
SPELL.ANGULAR.LIGHTNING_BOLT.SHAPE_NW1.ORA_MASK			.HS	80.80.90.83.CC.A1.D8.81.F0.80.C4.89.C0.89.C0.A9.94.A3.D0.E6.80.BC.80.8C.90.98.C0.B3.84.9E.80.80
				
SPELL.ANGULAR.LIGHTNING_BOLT.SHAPE_NW2.AND_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80
SPELL.ANGULAR.LIGHTNING_BOLT.SHAPE_NW2.ORA_MASK			.HS	90.80.80.82.8C.8A.98.88.B0.98.E4.B3.F0.E6.DC.BC.C0.B6.D0.E2.84.80.80.C0.84.FA.D0.E2.80.80.80.80

@END

;SHAPE TABLE: FLAME LIGHTNING BOLT
@START
FLAME_LIGHTNING_BOLT.SHAPE.START
;----------

SPELL.ANGULAR.FLAME_LIGHTNING.SHAPE_0.AND_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80
SPELL.ANGULAR.FLAME_LIGHTNING.SHAPE_0.ORA_MASK			.HS	8C.80.98.83.D8.81.F0.80.E2.94.EA.90.C8.91.C8.97.CE.83.EC.80.F8.94.B2.84.98.86.B6.83.FC.91.80.80
				
SPELL.ANGULAR.FLAME_LIGHTNING.SHAPE_1.AND_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80
SPELL.ANGULAR.FLAME_LIGHTNING.SHAPE_1.ORA_MASK			.HS	80.80.80.80.88.C0.A0.80.B0.85.98.F0.F8.99.CE.CF.82.9C.86.F4.BC.85.8C.95.80.C0.80.80.80.80.80.80
				
SPELL.ANGULAR.FLAME_LIGHTNING.SHAPE_2.AND_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80
SPELL.ANGULAR.FLAME_LIGHTNING.SHAPE_2.ORA_MASK			.HS	80.80.FC.91.B6.83.98.86.B2.84.F8.94.EC.80.CE.83.C8.97.C8.91.EA.90.E2.94.F0.80.D8.81.98.83.8C.80
				
SPELL.ANGULAR.FLAME_LIGHTNING.SHAPE_3.AND_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80
SPELL.ANGULAR.FLAME_LIGHTNING.SHAPE_3.ORA_MASK			.HS	80.80.80.80.82.90.80.84.A0.8D.8E.98.98.9F.F2.F3.B8.C0.AE.E0.A0.BD.A8.B1.82.80.80.80.80.80.80.80
				
				
;OCTET 4				
SPELL.ANGULAR.FLAME_LIGHTNING.SHAPE_NE0.AND_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80
SPELL.ANGULAR.FLAME_LIGHTNING.SHAPE_NE0.ORA_MASK			.HS	C0.81.88.B3.80.9B.A0.8E.A0.C3.A8.C3.C8.C1.C8.D9.CE.8F.EC.80.F8.D0.B0.94.B0.84.B2.8E.F8.C3.80.80
				
SPELL.ANGULAR.FLAME_LIGHTNING.SHAPE_NE1.AND_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80
SPELL.ANGULAR.FLAME_LIGHTNING.SHAPE_NE1.ORA_MASK			.HS	80.80.80.93.88.E6.80.B6.80.9C.A0.C6.A0.86.A8.86.88.D3.CE.95.F8.80.E0.80.B0.90.9A.87.F0.C1.80.80
				
SPELL.ANGULAR.FLAME_LIGHTNING.SHAPE_NE2.AND_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80
SPELL.ANGULAR.FLAME_LIGHTNING.SHAPE_NE2.ORA_MASK			.HS	80.90.82.81.A0.E1.A0.B0.B0.98.98.CF.CC.9D.F8.F4.D8.85.8E.95.82.C0.86.80.BC.C1.8C.95.80.80.80.80
				
				
;OCTET 5				
SPELL.ANGULAR.FLAME_LIGHTNING.SHAPE_SE0.AND_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80
SPELL.ANGULAR.FLAME_LIGHTNING.SHAPE_SE0.ORA_MASK			.HS	80.80.F8.C3.B2.8E.B0.84.B0.94.F8.D0.EC.80.CE.8F.C8.D9.C8.C1.A8.C3.A0.C3.A0.8E.80.9B.88.B3.C0.81
				
SPELL.ANGULAR.FLAME_LIGHTNING.SHAPE_SE1.AND_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80
SPELL.ANGULAR.FLAME_LIGHTNING.SHAPE_SE1.ORA_MASK			.HS	80.80.F0.C1.9A.87.B0.90.E0.80.F8.80.CE.95.88.D3.A8.86.A0.86.A0.C6.80.9C.80.B6.88.E6.80.93.80.80
				
SPELL.ANGULAR.FLAME_LIGHTNING.SHAPE_SE2.AND_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80
SPELL.ANGULAR.FLAME_LIGHTNING.SHAPE_SE2.ORA_MASK			.HS	80.80.80.80.8C.95.BC.C1.86.80.82.C0.8E.95.8E.95.D8.85.F8.F4.CC.9D.98.CF.B0.98.A0.B0.A0.E1.82.81
				
				
;OCTET 6				
SPELL.ANGULAR.FLAME_LIGHTNING.SHAPE_SW0.AND_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80
SPELL.ANGULAR.FLAME_LIGHTNING.SHAPE_SW0.ORA_MASK			.HS	80.80.C2.9F.F0.CC.A0.8C.A8.8C.8A.9E.80.B6.F0.F3.9A.93.82.93.C2.95.C2.85.F0.84.D8.81.CC.91.80.83
				
SPELL.ANGULAR.FLAME_LIGHTNING.SHAPE_SW1.AND_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80
SPELL.ANGULAR.FLAME_LIGHTNING.SHAPE_SW1.ORA_MASK			.HS	80.80.82.8F.E0.D9.88.8C.80.86.80.9E.A8.F3.CA.91.E0.84.E0.84.E2.84.B8.80.EC.80.E6.90.C8.81.80.80
				
SPELL.ANGULAR.FLAME_LIGHTNING.SHAPE_SW2.AND_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80
SPELL.ANGULAR.FLAME_LIGHTNING.SHAPE_SW2.ORA_MASK			.HS	80.80.80.80.A8.B1.82.BD.80.E0.82.C0.A8.F1.A0.9B.AE.9E.B8.B3.F2.99.98.8C.8C.84.86.85.80.C1.88.80
				
				
;OCTET 7				
SPELL.ANGULAR.FLAME_LIGHTNING.SHAPE_NW0.AND_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80
SPELL.ANGULAR.FLAME_LIGHTNING.SHAPE_NW0.ORA_MASK			.HS	80.83.CC.91.D8.81.F0.84.C2.85.C2.95.82.93.9A.93.F0.F3.80.B6.8A.9E.A8.8C.A0.8C.F0.CC.C2.9F.80.80
				
SPELL.ANGULAR.FLAME_LIGHTNING.SHAPE_NW1.AND_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80
SPELL.ANGULAR.FLAME_LIGHTNING.SHAPE_NW1.ORA_MASK			.HS	80.80.C8.81.E6.90.EC.80.B8.80.E2.84.E0.84.E0.94.CA.91.A8.F3.80.9E.80.86.88.8C.E0.D9.82.8F.80.80
				
SPELL.ANGULAR.FLAME_LIGHTNING.SHAPE_NW2.AND_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80
SPELL.ANGULAR.FLAME_LIGHTNING.SHAPE_NW2.ORA_MASK			.HS	88.80.80.C1.86.85.8C.84.98.8C.F2.99.B8.B3.AE.9E.A0.9B.A8.F1.82.C0.80.E0.82.BD.A8.B1.80.80.80.80

@END

	.EP ;end patch mode
;--------
GE.SPELL.LIGHTNING.END ;marks the end address of the code block for this spell
@END
;=======================

GE.SPELL.SUMMON.START ;marks the start address of the code block for this spell
;--------
@START
	.PH SPELL.FILE.CODE_BLOCK.BUFFER.START ;start patch 

GE.SPELL.ARMGDN_COW ;requires GE.SUMMON
@START
;PARAMETERS: ARMGDN.COW.QTY
;ENTRANCE: direct
;RETURN: targets hit (COMBAT.TARGET_HIT.DB)

.SAVE.PARAMETERS
	;ACC = quantity of cows to summon
	STA ARMGDN.COW.QTY	
	

			
.INIT
	; LDA #$FF ;init to OFF
	; STA ARMGDN.CLOBBERED.MOB.MO_INDEX ;($FF = disabled | any other value is a map object index)
	; LDA #$00 ;set to OFF
	; STA SUMMON.COLLISION.OVERIDE ;($00 off | $01 = ON)
	
	LDX #$00
.SUMMON.LOOP

	
			;STA COMBAT.PLAYER_SELECTED.TARGET.GMAP.Y

		LDA #COMBAT.ALIGNMENT.PC
		;LDA #$01 ;mob aligned
		;LDA #$03 ;pc aligned
		STA SUMMON.PARM.ALIGNMENT

		LDA #COMBAT.ENEMY_TYPE.MOB
		;LDA #$01 ;mob targets
		;LDA #$03 ;pc targets
		STA SUMMON.PARM.ENEMY_TYPE
	
		LDY #MAP_OBJECT.FLAG.AGGR	;set flags: set aggressive
		STY SUMMON.PARM.FLAGS
		JSR .GET.PROSPECTIVE.COORDINATES	
		LDA #$01 ;($00 = off, $01 = on)
		STA SUMMON.COLLISION.OVERIDE ;override the GE.SUMMON collision check because it's already been run in a 
									;modified way that allows cows to land on non-player character S_ENTITIES
		LDA #TILE_ID.COW	;set tile_type

	JSR GE.SUMMON

	;setup character sheet record for summoned S_ENTITY
		LDA #.SPECIAL.TABLE.SPECIAL.ID_2
		STA SPECIAL.DATA.POINTER+$0
		LDA /.SPECIAL.TABLE.SPECIAL.ID_2
		STA SPECIAL.DATA.POINTER+$1		
	JSR COMBAT.SPELL.CREATE.SPECIAL
			
	LDA #$FF ;reset to init value
	STA ARMGDN.CLOBBERED.MOB.MO_INDEX ;($FF = disabled | any other value is a map object index)

		; PHA ;save ACC
		; TXA
		; PHA
		
		; LDA #$24
		; STA VARIABLE1.HTAB
		; LDA #$07
		; STA VARIABLE1.VTAB
		; LDA #$26
		; STA VARIABLE2.HTAB
		; LDA #$08
		; STA VARIABLE2.VTAB								
		
		; LDA #$AA
		; LDX VARIABLE2
	; JSR MONITOR.VARIABLE
	; JSR KEYIN ;pause optional
		; STA TEMP
		; PLA
		; TAX
		; PLA ;restore ACC
		
	INX ;increment loop counter
	CPX ARMGDN.COW.QTY
	BNE .SUMMON.LOOP
	
			; lda #$aa
			; jsr prep.brk
			; brk
			
	
	;**FALLS THROUGH**

.EXIT
	LDA #$00 ;rest to default (off)
	STA SUMMON.COLLISION.OVERIDE ;($00 = off, $01 = on)
	


			
.SET.CRITICAL_HIT.FLAGS
@START
;(engaged s_entity screen index)

		;**warning: active player character sheet will be loaded on exit
		LDY COMBAT.ATTACKER.SINDEX
		LDA #COMBAT.STATS.CRTL_HIT.NOT_ENGAGED_VALUE ;set value to update.
.UPDATE.CHR_SHEET.ENGAGED.INDEX ;**OPT** Memory. This routine was written as as subroutine, but is being used inline here and it could be made more efficient. 
;PARAMETERS: YREG = screen index of attacker, ACC = value to write to CHR_SHEET.PC_MOB.ENGAGED.SINDEX
;ENTRANCE: direct
;RETURN: saved attacker character sheet with updated CHR_SHEET.PC_MOB.ENGAGED.SINDEX field

.SAVE.PARAMETERS.ENAGED
	;ACC = value to write to CHR_SHEET.PC_MOB.ENGAGED.SINDEX
	STA COMBAT.ENGAGED_SINDEX.UPDATE.PARM
	
	;read attacker (MOB) character sheet
		
		;YREG = COMBAT.ATTACKER.SINDEX
		LDA #$00 ;set read mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet

			
	;set the engaged field to the updated value
	;(expected to be the screen index of the defender or $FF to indicate no engagement)
	LDA COMBAT.ENGAGED_SINDEX.UPDATE.PARM
	STA CHR_SHEET.PC_MOB.ENGAGED.SINDEX

			
		;YREG = COMBAT.ATTACKER.SINDEX
		LDA #$01 ;set write mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet

@END



			
	RTS
	
	;SPECIAL: MAD COW
.SPECIAL.TABLE.SPECIAL.ID_2	.HS	00.01.32.00.00.00.00.0A.50.0A.00.00.FF.00.06.00.00
.SPECIAL.TABLE.SPECIAL.ID_2.NAME.START	.AZ	-/Mad cow/
.SPECIAL.TABLE.SPECIAL.ID_2.NAME.END		
.SPECIAL.TABLE.SPECIAL.ID_2.NAME.SIZE	.EQ	.SPECIAL.TABLE.SPECIAL.ID_2.NAME.END-.SPECIAL.TABLE.SPECIAL.ID_2.NAME.START
	.BS 	CHR_SHEET.SPECIAL.NAME.MAX_SIZE-.SPECIAL.TABLE.SPECIAL.ID_2.NAME.SIZE+1,$AA

	
;SUBROUTINES	
.GET.PROSPECTIVE.COORDINATES
@START

;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA

.GET.LOOP	
		;set custom random number range
		;(it resets to default after each call to RANDOM.8)
		LDA #$01
		STA RND.LO		;see note above before using a different return value. 
		LDA #$0A
		STA RND.HI
	JSR RANDOM.8
		;ACC = random number
		STA COMBAT.PLAYER_SELECTED.TARGET.GMAP.X

		;set custom random number range
		;(it resets to default after each call to RANDOM.8)
		LDA #$01
		STA RND.LO		;see note above before using a different return value. 
		LDA #$0A
		STA RND.HI
	JSR RANDOM.8
		;ACC = random number
		STA COMBAT.PLAYER_SELECTED.TARGET.GMAP.Y
			
	;get screen location of prospective GMAP.X/Y of next cow
		LDA COMBAT.PLAYER_SELECTED.TARGET.GMAP.X ;load new special S_ENTITYY GMAP.X
		STA PARM.GMAP.X
		LDA COMBAT.PLAYER_SELECTED.TARGET.GMAP.Y ;load new special S_ENTITY GMAP.Y
		STA PARM.GMAP.Y				
	JSR CONVERT.GMAP.TO.PLAYER_RELATIVE.XY
		LDA RETURN.RELATIVE.X
		STA PARM.RELATIVE.X
		LDA RETURN.RELATIVE.Y
		STA PARM.RELATIVE.Y	
	JSR CONVERT.PLAYER_RELATIVE.XY.TO.SCREEN_ARRAY_INDEX
		LDY RETURN.SCREEN_ARRAY_INDEX ;used below
		STY SUMMON.TARGET.SINDEX

;COLLISION CHECK
		STY SAVED.YREG.LOCAL ;save screen index of prospective coordinates	
			;**OPT** Memory. The STY above is probably not needed. 

		;check for other s_entities
		;(note: cows may squish all S_ENTITIES types except for player characters)
		LDA SCREEN.MO_SPRITE_TYPE.DATA,Y	;load S_ENTITY map object type	
		; AND #$87 ;mask-out MTT flags (bits 3-6)
		; CMP #$FF 			
		BMI .S_ENTITY.CHECK.COMPLETE ;does destination tile have a mob mo?
		CMP #S_ENTITY_TYPE.PC ;is S_ENTITY found a player character?
		BEQ .S_ENTITY.CHECK.COMPLETE
		;at this point we know there is a non-PC S_ENTITY at the prospective coordinates.
		;check for multi-tile mob (except from squish)
			LDA #$08 ;parameter and AND mask
		STA TEMP ;save AND mask
			;Y-REG: screen index
		JSR READ.MAP_OBJECT.ARRAY
		LDA MAP_OBJECT.RECORD.READ+$3 ;load flag byte
		BIT TEMP ;is flag bit3 set (multi-tile)?  (MOB.FLAG3 is the label that normally stores the flag value of this bit, when the flag bits are fully extracted)
		BNE .S_ENTITY.CHECK.COMPLETE ;if bit is set then multi-tile mob is present. Don't accept prospective coordiantes since multi-tile mobs are exempt from squish. 
		
		; LDA SCREEN.MO_SPRITE.DATA,Y	;load S_ENTITY map object index		
		; TAX
		
		; INY ;check tile that would be multi-tile mobs upper right tile
		; LDA SCREEN.MO_SPRITE_TYPE.DATA,Y	;load S_ENTITY map object index		
		; CMP #$FF ;does destination tile have a mob mo?			
		; BEQ .S_ENTITY.CHECK.COMPLETE
		
		JMP .COW.LANDS_ON.MOB ;an S_ENTITY is found but it is not a player character. 					
				
.S_ENTITY.CHECK.COMPLETE		
		LDY SAVED.YREG.LOCAL ;restore screen index of prospective coordinates	
			;**OPT** Memory. The LDY above is probably not needed. 
			
		;parm SUMMON.PARM.FLAGS
		;parm SUMMON.TARGET.SINDEX is set above
	JSR SUMMON.COLLISION.CHECK		
			
		;ACC = ($00 = open | $01 = blocked)
		CMP #$00
		BEQ .GET.COORDINATES.EXIT
		
	;location blocked. get another pair of coordinates	
	INX ;loop counter (used to detect a run away loop, in case all location coordinates are blocked, for example)
	BNE .GET.LOOP
	JMP .GET.COORDINATES.EXIT
	
	
	
.COW.LANDS_ON.MOB
	LDA SCREEN.MO_SPRITE.DATA,Y	;load S_ENTITY map object index		
	STA ARMGDN.CLOBBERED.MOB.MO_INDEX
	
;DOES COW SQUISH MOB?
	
	;is mob level exempt from squish attacks?

	;*****<INSERT CODE HERE>
	;note: my intentionion is for all MTT mobs to be level exempt from squishing.
		;but GE.SUMMON doesn't check whether the prospective clobbered is MTT. The final decision
		;is made here. Thus, the level exemptions need to be setup so that any MTT mob is a high enough level that it ends up exempt from squishing. 

.DELETE.EXISTING.S_ENTITY

	JSR PLAY.SOUND.SQUISH

		;YREG = screen location of S_ENTITY to delete (the one in the tile the mob is landing on)
	JSR DELETE.S_ENTITY

.DELETE.COMPLETE

	;**FALLS THROUGH**

			
.GET.COORDINATES.EXIT

;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX
	
	RTS
@END
	
@END


GE.SPELL.SUMMON.STANDARD.ENTRANCE
@START

;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA

.SELECT.SPELL.ROUTINE
	LDA COMBAT.SPELL_CODE.CAST
	CMP #SPELL_CODE.SUMMON.DEMON_LORD
	BEQ .SUMMON.DEMON_LORD
	;default case: summon lesser_undead
	
	;**FALLS THROUGH**
	
.SUMMON.LESSER_UNDEAD
	;set tile_type of summoned S_ENTITY
	LDA #TILE_ID.SKELETON			 ;set tile_type
	STA SUMMON.PARM.TILE_TYPE

	LDA #MAP_OBJECT.FLAG.AGGR	;set flags: set aggressive
	STA SUMMON.PARM.FLAGS

	;set COMBAT.SPELL.CREATE.SPECIAL parms		
	LDA #.SPECIAL.TABLE.SPECIAL.ID_3
	STA SPECIAL.DATA.POINTER+$0
	LDA /.SPECIAL.TABLE.SPECIAL.ID_3
	STA SPECIAL.DATA.POINTER+$1	

	;set qty to summon
	LDA #$FF ;set high-bit, base qty on spellcaster level	;($00-$7F = qty to summon | high-bit set = qty to summon is based on spellcasters level)		
	JMP .SELECT.SPELL.ROUTINE.DONE
	
.SUMMON.DEMON_LORD
	;set tile_type of summoned S_ENTITY
	LDA #TILE_ID.DEMON_LORD ;set tile_type		
	;LDA #TILE_ID.SKELETON			 ;set tile_type
	STA SUMMON.PARM.TILE_TYPE

	;LDA #MAP_OBJECT.FLAG.AGGR	;set flags: set aggressive
	LDA #MAP_OBJECT.FLAG.MTT_AGGR	;set flags: set aggressive
	STA SUMMON.PARM.FLAGS

	;set SPECIAL char sheet data pointer (parms for COMBAT.SPELL.CREATE.SPECIAL)	
	LDA #.SPECIAL.TABLE.SPECIAL.ID_1
	STA SPECIAL.DATA.POINTER+$0
	LDA /.SPECIAL.TABLE.SPECIAL.ID_1
	STA SPECIAL.DATA.POINTER+$1	
		
	;set qty to summon
	LDA #$01 ;set fixed value ;($00-$7F = qty to summon | high-bit set = qty to summon is based on spellcasters level)
.SELECT.SPELL.ROUTINE.DONE
	
	;**FALLS THROUGH**
	
.SUMMON.LOOP.ENTRANCE
@START
;PARAMETERS: ACC (summon qty), SUMMON.PARM.TILE_TYPE, SUMMON.PARM.FLAGS

	
;SAVE PARAMETERS
	;ACC: ;($00-$7F = qty to summon | high-bit set = qty to summon is based on spellcasters level)
	STA SUMMON.QTY ;($00-$7F = qty to summon | high-bit set = qty to summon is based on spellcasters level)
	

	
.INIT
	;set summon QTY
	LDA SUMMON.QTY ;($00-$7F = qty to summon | high-bit set = qty to summon is based on spellcasters level)
	BPL	.SAVE.QTY ;branch if fixed qty is specified (high-bit note set)
		
	;set # of S_ENTITIES to summon based on spellcaster's level
	;(formula = ROUNDUP(level/2))
	LDA CHR_SHEET.PC_MOB.LEVEL
	LSR ;/2
	BNE .SAVE.QTY
	;set minimum summon qty
	LDA #$01
.SAVE.QTY	
	STA SUMMON.QTY
	
	LDX #$00
.SUMMON.LOOP
@START
	
.SET.S_ENTITY.CHARACTERISTICS
;(set alignment/enemy type & destination tile)

	;read attacker character sheet
		LDY COMBAT.ATTACKER.SINDEX
		LDA #$00 ;set read mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet
		;RETURN: CHR_SHEET.RECORD.READ($80)
	
	

;PC/MOB TURN BRANCH
	LDA COMBAT.TURN_STATUS   ;($00 = PCs turn. $01 = Special(s) S_ENTITY (example Wyvern Ally, Summoned Demon), $02 = MOBs turn)
	BEQ .PC_ATTACKER ;if not PCs turn, then branch

	;**FALLS THROUGH**
	
.NON_PC.ATTACKER
;(attacker is either a SPECIAL or MOB. Set the summoned S_ENTITIES alignment and enemy type to the same
;as the attacker)

	LDA SPRITE.RECORD+$4
	STA SUMMON.PARM.ALIGNMENT
	
	LDA SPRITE.RECORD+$5
	STA SUMMON.PARM.ENEMY_TYPE
			
	JMP .SET.ALIGNMENT_ENEMY_TYPE.DONE


.PC_ATTACKER
	LDA #COMBAT.ALIGNMENT.PC
	;LDA #COMBAT.ALIGNMENT.MOB
	;LDA #$01 ;mob aligned
	;LDA #$03 ;pc aligned
	STA SUMMON.PARM.ALIGNMENT

	LDA #COMBAT.ENEMY_TYPE.MOB
	;LDA #COMBAT.ENEMY_TYPE.PC
	;LDA #$01 ;mob targets
	;LDA #$03 ;pc targets
	STA SUMMON.PARM.ENEMY_TYPE
		
	;**FALLS THROUGH**
	
.SET.ALIGNMENT_ENEMY_TYPE.DONE

	;get destination tile coordinates for the summoned S_ENTITY
	JSR SUMMON.GET.COORDINATES

.EXECUTE.SUMMON		

;CREATE SPECIAL CHARACTER SHEET RECORD
		
	
	JSR COMBAT.SPELL.CREATE.SPECIAL
		;RETURN VALUE: ACC = ($00 = special record added | $01 special character sheet array full)
		CMP #$01 ;is special character sheet array full?
		BEQ .EXIT_TEST ;branch if yes and skip the creation of the special map object
		
		;----set TILE_ID and flags	
		LDA SUMMON.PARM.TILE_TYPE		 ;set tile_type		
		LDY SUMMON.PARM.FLAGS		 	;set flags: set aggressive
	JSR GE.SUMMON


.EXIT_TEST

	INX
	CPX SUMMON.QTY
	BCC .SUMMON.LOOP

	
	
@END
@END
	
	;**FALLS THROUGH**
	
	
.SET.CRITICAL_HIT.FLAGS
@START
;(engaged s_entity screen index)

		;**warning: active player character sheet will be loaded on exit
		LDY COMBAT.ATTACKER.SINDEX
		LDA #COMBAT.STATS.CRTL_HIT.NOT_ENGAGED_VALUE ;set value to update.
.UPDATE.CHR_SHEET.ENGAGED.INDEX ;**OPT** Memory. This routine was written as as subroutine, but is being used inline here and it could be made more efficient. 
;PARAMETERS: YREG = screen index of attacker, ACC = value to write to CHR_SHEET.PC_MOB.ENGAGED.SINDEX
;ENTRANCE: direct
;RETURN: saved attacker character sheet with updated CHR_SHEET.PC_MOB.ENGAGED.SINDEX field

.SAVE.PARAMETERS.ENAGED
	;ACC = value to write to CHR_SHEET.PC_MOB.ENGAGED.SINDEX
	STA COMBAT.ENGAGED_SINDEX.UPDATE.PARM
	
	;read attacker (MOB) character sheet
		
		;YREG = COMBAT.ATTACKER.SINDEX
		LDA #$00 ;set read mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet
		;RETURN: CHR_SHEET.RECORD.READ($80)

			
	;set the engaged field to the updated value
	;(expected to be the screen index of the defender or $FF to indicate no engagement)
	LDA COMBAT.ENGAGED_SINDEX.UPDATE.PARM
	STA CHR_SHEET.PC_MOB.ENGAGED.SINDEX

			
		;YREG = COMBAT.ATTACKER.SINDEX
		LDA #$01 ;set write mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet

@END


.EXIT

;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX


	
			; LDA #$01
			; STA TROUBLESHOOTING.HOOK
			
	RTS

;SPECIAL SHAPE TABLES

	;SPECIAL: DEMON LORD
.SPECIAL.TABLE.SPECIAL.ID_1	.HS	00.10.20.03.00.00.80.A0.50.A0.00.00.FF.00.06.C0.00
.SPECIAL.TABLE.SPECIAL.ID_1.NAME.START	.AZ	-/Demon Lord/
.SPECIAL.TABLE.SPECIAL.ID_1.NAME.END		
.SPECIAL.TABLE.SPECIAL.ID_1.NAME.SIZE	.EQ	.SPECIAL.TABLE.SPECIAL.ID_1.NAME.END-.SPECIAL.TABLE.SPECIAL.ID_1.NAME.START
	.BS 	CHR_SHEET.SPECIAL.NAME.MAX_SIZE-.SPECIAL.TABLE.SPECIAL.ID_1.NAME.SIZE+1,$AA

	;SPECIAL: SKELETON
.SPECIAL.TABLE.SPECIAL.ID_3	.HS	00.03.96.00.00.00.00.1E.00.1E.00.00.FF.00.00.00.00
;dungeon video; .SPECIAL.TABLE.SPECIAL.ID_3	.HS	00.08.19.00.00.00.00.02.00.1E.00.00.FF.00.00.00.00
.SPECIAL.TABLE.SPECIAL.ID_3.NAME.START	.AZ	-/Skeleton/
.SPECIAL.TABLE.SPECIAL.ID_3.NAME.END		
.SPECIAL.TABLE.SPECIAL.ID_3.NAME.SIZE	.EQ	.SPECIAL.TABLE.SPECIAL.ID_3.NAME.END-.SPECIAL.TABLE.SPECIAL.ID_3.NAME.START
	.BS 	CHR_SHEET.SPECIAL.NAME.MAX_SIZE-.SPECIAL.TABLE.SPECIAL.ID_3.NAME.SIZE+1,$AA


@END

GE.SUMMON ;called by wrappers that stay in memory and also spells loaded from disk like GE.SPELL.ARMGDN.COW
@START
;PARAMETERS: ACC (tile_type), Y-REG (flag value), [SUMMON.COLLISION.OVERIDE]*, 
;ENTRANCE: DIRECT, GE.ARMGDN.COWS

;=====================SUBROUTINE DOCUMENTATION====================================
;
;*Future ideas:
;
;**also see notes in the GE.SPELL.SUMMON.DEMON_LORD subroutine
;
;At the moment a big limitation of summon is that the shape tables for all summonable S_ENTITIES
;has to be in all tile sets. The following are my thoughts on possible ways to change that.
;
;Store all summonable S_ENTITIY shape tables in the spell loaded from disk.
;once an S_ENTITY is summoned, copy it into a buffer in aux memory. The buffer would have
;one slot or maybe multiple slots, but definately less slots than the total number of summonable S_ENTITIES or
;it would be easier to just load them into aux memory on game boot. 
;
;So essentially, the player can't summon every type of summonable S_ENTITY at the same time. But, we could have a game
;rule that says a spellcaster cannot maintain the concentration necesssary to control more than one at a time. Since that seems to
;work from a game play standpoint, I'm tempted to just have one slot. In that case it might be even easier to just designate
;the top 4 tiles in every tile set as a special slot. 
;
;Use the flag byte ($3) to tell the animation manager that the S_ENTITY was summoned and to check the slot(s)
;If there are multiple slots, then a lookup table using tile ID, slot # pairs would be needed for
;animation manager to use the correct slot
;
;DRAW.TILE.SINLGE is already setup to use the shape data loaded into the shape hopper by the calling routine.  
;
;=================================================================================


;*SUMMON.COLLISION.OVERIDE: if set then GE.SUMMON does not do any collision checks, which instead are done by the calling routine. 
			;(DISABLED) if set, then ARMGDN.CLOBBERED.MOB.MO_INDEX should also be set if the intention is for the summoned S_ENTITY to clobber 
			;an existing S_ENTITY map object record
			;[ARMGDN.CLOBBERED.MOB.MO_INDEX]
			
	;Init Tile_TYPE
	STA SUMMON.PARM.TILE_TYPE
		
	;Init Flags (default $00)
	STY SUMMON.PARM.FLAGS

;SAVE REGISTERS
	TXA
	PHA
	
;COLLISION CHECKS
		
	;get screen location of target tile
		LDA COMBAT.PLAYER_SELECTED.TARGET.GMAP.X ;load new special S_ENTITYY GMAP.X
		STA PARM.GMAP.X
		LDA COMBAT.PLAYER_SELECTED.TARGET.GMAP.Y ;load new special S_ENTITY GMAP.Y
		STA PARM.GMAP.Y				
	JSR CONVERT.COLUMN_ROW.TO.SINDEX
		LDY RETURN.SCREEN_ARRAY_INDEX ;used below
		STY SUMMON.TARGET.SINDEX
	
	LDA SUMMON.COLLISION.OVERIDE ;($00 = off, $01 = on)
	BNE .CREATE.SPECIAL.S_ENTITY
	
	;**OPT** Memory. Speed. this JSR to check collisions is probably now duplicate, but I'm not sure if armagedon by cows still relies on it. 
	
		;parm SUMMON.PARM.FLAGS
		;parm SUMMON.TARGET.SINDEX is set above
	JSR SUMMON.COLLISION.CHECK	;verify the target screen tile location doesn't already contain a map object or an obstacle
		;ACC = ($00 = OPEN | $01 = BLOCKED)
		CMP #$00
		BEQ .CREATE.SPECIAL.S_ENTITY
		JMP .TARGET_LOCATION.INVALID

.CREATE.SPECIAL.S_ENTITY
			
;CREATE SPECIAL S_ENTITY RECORD
	JSR GET.SPECIAL.RECORD ;get index to special map object array
		;return value: XREG = index to next open map object record (special array)

	;Init Tile_TYPE
	LDA SUMMON.PARM.TILE_TYPE
	;STA COMBAT.MAP_OBJECTS.SPECIAL+$2,X
	STA COMBAT.MAP_OBJECTS.SPECIAL.TRUE_INDEX+$2,X
	STA SAVED_TILE_TYPE
		
	;Init Flags (default $00)
	LDA SUMMON.PARM.FLAGS
	STA COMBAT.MAP_OBJECTS.SPECIAL.TRUE_INDEX+$3,X

	;Init GMAP.X/Y
	LDA COMBAT.PLAYER_SELECTED.TARGET.GMAP.X
	STA COMBAT.MAP_OBJECTS.SPECIAL.TRUE_INDEX+$0,X
	LDA COMBAT.PLAYER_SELECTED.TARGET.GMAP.Y
	STA COMBAT.MAP_OBJECTS.SPECIAL.TRUE_INDEX+$1,X
	
;multi-tile mob
	; LDA #$09 ;flags
	; STA COMBAT.MAP_OBJECTS.SPECIAL+$3,X

	;set health
	LDA #$00 ;good
	;LDA #$FF ;dead
	STA COMBAT.MAP_OBJECTS.SPECIAL.TRUE_INDEX+$7,X
		
		
;ALIGNMENT

	;Init Alignment	
	LDA SUMMON.PARM.ALIGNMENT
	STA COMBAT.MAP_OBJECTS.SPECIAL.TRUE_INDEX+$4,X

	;Init Enemy Type
	LDA SUMMON.PARM.ENEMY_TYPE
	STA COMBAT.MAP_OBJECTS.SPECIAL.TRUE_INDEX+$5,X

	;Increment Total Specials
	INC COMBAT.SPECIAL.TOTAL ;the number of specials at the start of combat	

.DRAW.S_ENTITY
	
	;JSR SUMMON.LOAD.SHAPE_TABLE ;load shape table data stored in this routine into SHAPE.HOPPER0 for use with DRAW.TILE.SINGLE 

	LDA COMBAT.MAP_OBJECTS.SPECIAL.TRUE_INDEX+$3,X ;load flag byte of new special S_ENTITY
	CMP #MAP_OBJECT.FLAG.MTT_AGGR	;is flag set to multi-tile?
	BEQ .DRAW.MULTI_TILE.S_ENTITIES ;if yes, branch to multi-tile draw section


;========SINGLE TILE=====
	
	
.DRAW.WARP_SINGLE.SEQUENCE					
		;connect shape table pointer
		LDA #WARP_SINGLE.SHAPE.START.FRAME0
		STA SHAPE.BACKGROUND.AND_MASK+$0

		LDA /WARP_SINGLE.SHAPE.START.FRAME0
		STA SHAPE.BACKGROUND.AND_MASK+$1
		;Y-REG = SUMMON.TARGET.SINDEX
	JSR .DRAW.SINGLE.OFF_GRID

.SUMMON.DRAW.SINGLE.TILE
		LDA SAVED_TILE_TYPE
		PHA ;push saved tile_type
		;YREG = SCREEN_ARRAY_INDEX
		;SAVED_TILE_TYPE set above
	JSR DRAW.TILE.SINGLE ;background draw					
		PLA ;restore tile_type
		STA SAVED_TILE_TYPE
		LDA #$01
		STA PAGE.FOREGROUND.OVERRIDE
	JSR DRAW.TILE.SINGLE ;foreground draw				

	JSR FLIP.PAGE

	;JSR .SUMMON.BLINK.S_ENTITY

.UPDATE.SCREEN_ARRAYS
	TXA ;**OPT** Memory. Speed. This line and the next seem like they could be consolidated if the next line was changed to STX.
	STA SCREEN.MO_SPRITE.DATA,Y
	LDA #S_ENTITY_TYPE.SPECIAL
	STA SCREEN.MO_SPRITE_TYPE.DATA,Y
	
	JMP .EXIT
	
;========MULTI-TILE=====
		
.DRAW.MULTI_TILE.S_ENTITIES

.DRAW.WARP_MTT.SEQUENCE					
		;connect shape table pointer
		LDA #WARP_MTT.SHAPE.START.FRAME0
		STA SHAPE.BACKGROUND.AND_MASK+$0

		LDA /WARP_MTT.SHAPE.START.FRAME0
		STA SHAPE.BACKGROUND.AND_MASK+$1
		;Y-REG = SUMMON.TARGET.SINDEX
	JSR .DRAW.MTT.OFF_GRID
	
;CALCUALTE THE TILE TYPES FOR EACH TILE OF MT MOB
	STX SUMMON.MO_INDEX ;save map object index

	LDA COMBAT.MAP_OBJECTS.SPECIAL.TRUE_INDEX+$2,X
	TAX
	STX SUMMON.MT.TILE_TYPES
	INX
	STX SUMMON.MT.TILE_TYPES+$1
	INX
	STX SUMMON.MT.TILE_TYPES+$2
	INX
	STX SUMMON.MT.TILE_TYPES+$3
	
	JSR .SUMMON.DRAW.MTT ;draw summoned S_ENTITY on tile grid
	
		
	LDX #$00
.MTT.UPDATE.SCREEN.ARRAYS.LOOP
	LDY SUMMON.MT.TILE_LOCATIONS,X
		
	LDA SUMMON.MO_INDEX ;restore map objects index				;save to sprite_type screen array so that other routines (like animation) know whether to use the sprite screen array value as an index to the mob or NPC screen array
	; SEC
	; SBC #COMBAT.MAP_OBJECTS.SPECIAL.START_RECORD ;	;convert map object index (X-REG) to standardized value. (SPECIAL array starts at the NPC array + $80)
	STA SCREEN.MO_SPRITE.DATA,Y			;save to sprite screen array to mark sprite's presence for other routines like animation
	
		LDA #S_ENTITY_TYPE.SPECIAL
		;XREG = MTT tile # (0 = upper left, 1 = upper right, 2 = lower left, 3 = lower right)
	JSR MAP_OBJECT.ADD.MTT_FLAG_BITS
		;ACC = S_ENTITY_TYPE (with flag bits)
		STA SCREEN.MO_SPRITE_TYPE.DATA,Y	
	
	INX
	CPX #$04
	BNE .MTT.UPDATE.SCREEN.ARRAYS.LOOP


	JMP .EXIT

.TARGET_LOCATION.INVALID
;GE.SUMMON reports that destination location of summoned S_ENTITY is blocked

	;JSR PLAY.SOUND.DUMB_ASS
	
	JSR PREP.BRK
	BRK
	
	
.EXIT	
;RESTORE REGISTERS
	PLA
	TAX
		
	RTS ;return to the routine that called GE.SUMMON


;SUBROUTINES
.SUMMON.DRAW.MTT ;on tile grid
@START
	LDX #$00 ;init SUMMON.MT.TILE_TYPES index and loop counter
.DRAW.MTT.LOOP
		LDY SUMMON.MT.TILE_LOCATIONS,X
		;YREG = SCREEN_ARRAY_INDEX
		LDA SUMMON.MT.TILE_TYPES,X				;LOAD NEXT MT TILE_TYPE
		STA SAVED_TILE_TYPE
		PHA ;save tile_type
	JSR DRAW.TILE.SINGLE ;background draw
		PLA ;restore tile_type
		STA SAVED_TILE_TYPE
		LDA #$01
		STA PAGE.FOREGROUND.OVERRIDE
	JSR DRAW.TILE.SINGLE ;foreground draw				

	;increment & exit test
	INX 
	CPX #$04
	BNE .DRAW.MTT.LOOP

	;JSR FLIP.PAGE
		;JSR KEYIN
	
	RTS
@END

.DRAW.MTT.OFF_GRID ;off tile grid (animated, multi-tile)
@START
;PARAMETERS: Y-REG (screen tile location of target), SHAPE.BACKGROUND.AND_MASK+$0(2), SUMM
;ENTRANCE: direct
;RETURN: Multi-tile hi-res shape

;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA
	
.INIT.MTT_TARGET.LOCATION	
	;identify MTT mob screen tile locations
	;LDY SUMMON.TARGET.SINDEX
	STY SUMMON.MT.TILE_LOCATIONS+$0 ;set screen location of upper left tile

	STY SUMMON.MT.TILE_LOCATIONS+$1 ;set screen location of upper right tile
	INC SUMMON.MT.TILE_LOCATIONS+$1 ;+1 tile right

	TYA
	CLC
	ADC #SCREEN.ARRAY.OFFSET ;+1 tile down	
	STA SUMMON.MT.TILE_LOCATIONS+$2 ;set screen location of lower left tile

	TYA
	CLC
	ADC #SCREEN.ARRAY.OFFSET+1 ;+1 tile down, +1 tile right	
	STA SUMMON.MT.TILE_LOCATIONS+$3 ;set screen location of lower right tile
	
	LDY #$00 ;init outer loop (animation frame) counter
.DRAW.WARP.OUTTER_LOOP
;SAVE REGISTERS
	TYA
	PHA
	
	LDX #$00 ;init inner loop (MTT shape tile #) counter
.DRAW.WARP.INNER_LOOP ;draw one animation frame (multi-tile)


;SETUP SHAPE TABLE
	;LDY SCREEN.INDEX.TILE_SBYTE
	
		;execute copy
		LDA SHAPE.BACKGROUND.AND_MASK+$0
		STA COPY.FROM_START+$0
		LDA SHAPE.BACKGROUND.AND_MASK+$1
		STA COPY.FROM_START+$1

		LDA COPY.FROM_START+$0
		CLC
		ADC #$3F
		STA COPY.FROM_END+$0
		LDA COPY.FROM_START+$1
		ADC #$00 ;16-bit add
		STA COPY.FROM_END+$1
			
		LDA #SHAPE.HOPPER0
		STA COPY.TO+$0
		LDA /SHAPE.HOPPER0
		STA COPY.TO+$1
			
	JSR MEMORY.COPY

	;convert shape table so it can be used with COMBAT.DRAW.SHAPE
	JSR CONVERT.MASTER.SHAPE_TABLE
	;JSR SHAPE.ALIGN.COLORS
	
		;set COMBAT.DRAW.SHAPE parameters
		LDA SUMMON.MT.TILE_LOCATIONS,X
		TAY
		LDA SCREEN.INDEX.TILE_SBYTE,Y
		STA SHAPE.DRAW.CURRENT.SBYTE
		DEC SHAPE.DRAW.CURRENT.SBYTE ;since CONVERT.MASTER.SHAPE_TABLE inserts one screen byte to the left and right of the shape table (AND mask only)
									 ;we need to shift staring screen byte to the left to stay aligned with the tile grid. the insertion previously mentioned
									 ;is built into the COMBAT.SHAPE.DRAW routine for shape movement, which though movement isn't needed for this routine
		
		LDA SCREEN.INDEX.TILE_LINE,Y
		STA SHAPE.DRAW.LINE_ROW.NUMBER

		;foreground: draw shape in current position
		LDA #$01
		STA PAGE.FOREGROUND.OVERRIDE
		LDA #$00 ;set draw mode
	JSR COMBAT.DRAW.SHAPE
			

;INCREMENT SHAPE TABLE
	LDA SHAPE.BACKGROUND.AND_MASK+$0
	CLC
	ADC #$40
	STA SHAPE.BACKGROUND.AND_MASK+$0
	LDA SHAPE.BACKGROUND.AND_MASK+$1
	ADC #$00 ;16-bit add
	STA SHAPE.BACKGROUND.AND_MASK+$1

;INCREMENT MULTI-TILE #
	INX ;next tile in MTT shape
	CPX #$04
	BNE .DRAW.WARP.INNER_LOOP

;RESTORE REGISTERS
	PLA
	TAY

	
;OUTER LOOP: INCREMENT ANIMATION FRAME

	;update screen animation (serves as a delay between animation frames)		
	;!!!!! BSR:BANK1 !!!!
	JSR ANIMATION.UPDATE.ENTRANCE
		

		; LDA #$FF
	; JSR WAIT.LOOP
	
	;exit test
	INY ;next animation frame
	CPY #$04
	BNE .DRAW.WARP.OUTTER_LOOP
			;JSR KEYIN
			; LDA #$AA
			; JSR PREP.BRK
			; BRK

;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX
	
	RTS
@END

.DRAW.SINGLE.OFF_GRID ;off tile grid (animated, single-tile)
@START
;PARAMETERS: Y-REG (screen tile location of target), SHAPE.BACKGROUND.AND_MASK(2)
;ENTRANCE: direct
;RETURN: Hi-res shape

;SAVE REGISTERS
	TXA
	PHA

	
.INIT.SINGLE_TARGET.LOCATION		
		LDA SCREEN.INDEX.TILE_SBYTE,Y
		STA SHAPE.DRAW.CURRENT.SBYTE
		DEC SHAPE.DRAW.CURRENT.SBYTE ;since CONVERT.MASTER.SHAPE_TABLE inserts one screen byte to the left and right of the shape table (AND mask only)
									 ;we need to shift staring screen byte to the left to stay aligned with the tile grid. the insertion previously mentioned
									 ;is built into the COMBAT.SHAPE.DRAW routine for shape movement, which though movement isn't needed for this routine
		
		LDA SCREEN.INDEX.TILE_LINE,Y
		STA SHAPE.DRAW.LINE_ROW.NUMBER

	LDX #$00 ;init inner loop (MTT shape tile #) counter
.DRAW.SINGLE.LOOP ;draw one animation frame (multi-tile)


;SETUP SHAPE TABLE
	;LDY SCREEN.INDEX.TILE_SBYTE
	
		;execute copy
		LDA SHAPE.BACKGROUND.AND_MASK+$0
		STA COPY.FROM_START+$0
		LDA SHAPE.BACKGROUND.AND_MASK+$1
		STA COPY.FROM_START+$1

		LDA COPY.FROM_START+$0
		CLC
		ADC #$3F
		STA COPY.FROM_END+$0
		LDA COPY.FROM_START+$1
		ADC #$00 ;16-bit add
		STA COPY.FROM_END+$1
			
		LDA #SHAPE.HOPPER0
		STA COPY.TO+$0
		LDA /SHAPE.HOPPER0
		STA COPY.TO+$1
			
	JSR MEMORY.COPY

	;convert shape table so it can be used with COMBAT.DRAW.SHAPE
	JSR CONVERT.MASTER.SHAPE_TABLE
	
		;set COMBAT.DRAW.SHAPE parameters

		;foreground: draw shape in current position
		LDA #$01
		STA PAGE.FOREGROUND.OVERRIDE
		LDA #$00 ;set draw mode
	JSR COMBAT.DRAW.SHAPE
		
;UPDATE SCREEN ANIMATION (serves as a delay)		
		; LDA #$FF
	; JSR WAIT.LOOP
		
		
	;!!!!! BSR:BANK1 !!!!
	JSR ANIMATION.UPDATE.ENTRANCE

		
;INCREMENT SHAPE TABLE
	LDA SHAPE.BACKGROUND.AND_MASK+$0
	CLC
	ADC #$40
	STA SHAPE.BACKGROUND.AND_MASK+$0
	LDA SHAPE.BACKGROUND.AND_MASK+$1
	ADC #$00 ;16-bit add
	STA SHAPE.BACKGROUND.AND_MASK+$1

;INCREMENT MULTI-TILE #
	INX ;next animation frame
	CPX #$04
	BNE .DRAW.SINGLE.LOOP

;RESTORE REGISTERS
	PLA
	TAX
	
	RTS
@END


.SUMMON.BLINK.S_ENTITY ;DISABLED
@START
	; TXA
	; PHA


; ;IS SUMMONED S_ENTITY CLOBBERING AN EXISTING S_ENTITY?
; ;(if yes, then don't show blinking effect)
	; LDA ARMGDN.CLOBBERED.MOB.MO_INDEX
	; CMP #$FF
	; BNE .BLINK_EXIT
	
; ;DO THE BLINK 	
	; LDX #$00
; .BLINK.LOOP
		; ;ENABLE BANK-SWITCHED RAM ($D000 Bank 1)
		; LDA $C08B 
		; LDA $C08B ;**OPT** Memory. ANIMATION.UPDATE is called a lot. put it in a subroutine. That way the BSR swithing only takes up memory once. 
	; JSR ANIMATION.UPDATE
		; ;RE-ENABLE BANK-SWITCHED RAM ($D000 Bank 2)
		; LDA $C083				;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK2 2nd)
		; LDA $C083
		
				; ;JSR KEYIN
				
		; LDA #$40
		; JSR WAIT.LOOP
		
	; INX
	; CPX #$04
	; BNE .BLINK.LOOP

; .BLINK_EXIT	
	; PLA
	; TAX
	
	; RTS
@END

SUMMON.GET.COORDINATES
@START
;RETURN: COMBAT.PLAYER_SELECTED.TARGET.GMAP.X, COMBAT.PLAYER_SELECTED.TARGET.GMAP.Y


;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA

	
;SET DESTINATION TILE FOR THE SUMMON
	LDX #$00
.SET.DEST.LOOP
		
;LOAD PROSPECTIVE DESTINATION TILE SINDEX
;(the prospective destination tile is different depending on the alignment of the attacker (spellcaster). This is
;because two different destination tile position arrays are setup so that mob aligned spellcasters summon their S_ENTITIES
;closer to the mob starting front line and vice-versa for PC attacker (spellcasters).

	;PC/MOB TURN BRANCH
	LDA COMBAT.TURN_STATUS   ;($00 = PCs turn. $01 = Special(s) S_ENTITY (example Wyvern Ally, Summoned Demon), $02 = MOBs turn)
	BEQ .PC_ATTACKER ;if not PCs turn, then branch
	LDA SPRITE.RECORD+$4 ;load MOB/SPECIAL alignment
	CMP #COMBAT.ALIGNMENT.PC
	BEQ	.PC_ATTACKER
	LDA .DESTINATION.TILES.MOB_SPECIAL,X
	JMP .CHECK.STOP_VALUE
.PC_ATTACKER
	LDA .DESTINATION.TILES.PC,X
	;**FALLS THROUGH**
.CHECK.STOP_VALUE	
	CMP #$FF ;has stop value been found?
	BEQ .TARGET_ZONE.FULL
	INX ;increment loop counter
	TAY ;contains screen index of prospective destination tile
		;parm SUMMON.PARM.FLAGS is already set
		STY SUMMON.TARGET.SINDEX 
	JSR SUMMON.COLLISION.CHECK	
		;RETURN VALUE: ACC = ($00 = open | $01 = blocked)
		CMP #$00
	BNE .SET.DEST.LOOP
.SET.DEST.LOOP.DONE
;OPEN DESTINATION TILE FOUND
	LDA SCREEN.INDEX.COLUMN,Y
	STA COMBAT.PLAYER_SELECTED.TARGET.GMAP.X
	LDA SCREEN.INDEX.ROW,Y
	STA COMBAT.PLAYER_SELECTED.TARGET.GMAP.Y
	JMP .EXIT
	
.TARGET_ZONE.FULL ;randomly selects destination tile from entire screen
@START

;(the target zone for the summoned S_ENTITY is full, so 
;this routine randomly chooses a tile anywhere on the screen. 
		
	;JSR KEYIN
	
	; LDA #.DESTINATION.TILES.MOB_SPECIAL
	; STA $BE00
	
	; LDA /.DESTINATION.TILES.MOB_SPECIAL
	; STA $BE01

	; LDA .DESTINATION.TILES.MOB_SPECIAL,X
	; JSR PREP.BRK
	; BRK
	
	
	
	LDX #$00
.GET.LOOP	
		;set custom random number range
		;(it resets to default after each call to RANDOM.8)
		LDA #$01
		STA RND.LO		;see note above before using a different return value. 
		LDA #$0A
		STA RND.HI
	JSR RANDOM.8
		;ACC = random number
		STA COMBAT.PLAYER_SELECTED.TARGET.GMAP.X

		;set custom random number range
		;(it resets to default after each call to RANDOM.8)
		LDA #$01
		STA RND.LO		;see note above before using a different return value. 
		LDA #$0A
		STA RND.HI
	JSR RANDOM.8
		;ACC = random number
		STA COMBAT.PLAYER_SELECTED.TARGET.GMAP.Y
			
	;get screen location of prospective GMAP.X/Y of next cow
		LDA COMBAT.PLAYER_SELECTED.TARGET.GMAP.X ;load new special S_ENTITYY GMAP.X
		STA PARM.GMAP.X
		LDA COMBAT.PLAYER_SELECTED.TARGET.GMAP.Y ;load new special S_ENTITY GMAP.Y
		STA PARM.GMAP.Y				
	JSR CONVERT.GMAP.TO.PLAYER_RELATIVE.XY
		LDA RETURN.RELATIVE.X
		STA PARM.RELATIVE.X
		LDA RETURN.RELATIVE.Y
		STA PARM.RELATIVE.Y	
	JSR CONVERT.PLAYER_RELATIVE.XY.TO.SCREEN_ARRAY_INDEX
		LDY RETURN.SCREEN_ARRAY_INDEX ;used below
		STY SUMMON.TARGET.SINDEX

;COLLISION CHECK
		
			;parm SUMMON.PARM.FLAGS
			;parm SUMMON.TARGET.SINDEX is set above
		JSR SUMMON.COLLISION.CHECK	
		;RETURN VALUE: ACC = ($00 = open | $01 = blocked)
		CMP #$00
		BEQ .EXIT
		
	;location blocked. get another pair of coordinates	
	INX ;loop counter (used to detect a run away loop, in case all location coordinates are blocked, for example)
	BNE .GET.LOOP
@END
					
	;**FALLS THROUGH**
	
.EXIT

;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX
	
	RTS

.DESTINATION.TILES.MOB_SPECIAL	.HS 5A.59.5B.58.5C.6B.6A.6C.69.6D.7C.7B.7D.7A.7E.57.68.79.5D.6E.7F.56.67.78.5E.6F.80.55.66.77.FF
		;DATAGRAM: byte0 = screen index of prospective destination tile  (1 byte record)
				;stop value = $FF

.DESTINATION.TILES.PC			.HS 7C.7B.7D.7A.7E.6B.6A.6C.69.6D.5A.59.5B.58.5C.79.68.57.7F.6E.5D.78.67.56.80.6F.5E.77.66.55.FF
		;DATAGRAM: byte0 = screen index of prospective destination tile  (1 byte record)
				;stop value = $FF

SUMMON.COLLISION.CHECK
@START
;PARAMETERS: SUMMON.TARGET.SINDEX, SUMMON.PARM.FLAGS
;RETURN: ACC ($00 = screen location open, $01 = screen location blocked)
;ENTRANCE: DIRECT, GE.SUMMON, GE.SPELL.ARMGDN.COW

;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA

		
;COLLISION CHECK ON TARGET LOCATION
		
.SELECT.COLLISION.ROUTINES ;(MTT or non-MTT)
	LDA SUMMON.PARM.FLAGS
	CMP #MAP_OBJECT.FLAG.MTT_AGGR	;is flag set to multi-tile?
	BEQ SUMMON.COLLISION_CHECK.MTT ;if yes, branch to multi-tile draw section
	
	;**FALLS THROUGH**
	
	
SUMMON.COLLISION_CHECK.SINGLE
@START


.ENTRANCE
	LDY SUMMON.TARGET.SINDEX	;LOAD NEXT ADJACENT TILE#
	
.WALKING_RULES		
;APPLY MOB WALKING COLLISION RULES

.MAP_OBJECT.CHECKS	
		
	;CHECK FOR OTHER SPRITES
	LDA SCREEN.MO_SPRITE.DATA,Y					;DOES DESTINATION TILE HAVE A MOB MO?	
	CMP #$FF			
	BNE .MOVE_BLOCKED


			
	;CHECK FOR GENERAL MAP OBJECTS
	LDA SCREEN.MO_GENERAL.DATA,Y	;load general map object data for current tile location
	CMP #$FF						;is a general map object present?
	;BEQ .SPRITE.WALKING.TILE.TESTS	;if no, then continue with other checks
	BNE .MOVE_BLOCKED

.TERRAIN.OBSTACLE.CHECKS
	LDA SCREEN.TILE.DATA,Y						;LOAD TILE TYPE OF DESTINATION TILE

	;obstacle test #1
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
	;**FALLS THROUGH
	
	;JMP .MOVE_PERMITTED

.MOVE_PERMITTED
	LDA #$00		;PERMITTS MOVE BY RECORDING $00 
	STA SUMMON.COLLISION.RETURN_VALUE
	JMP SUMMON.COLLISION.EXIT

.MOVE_BLOCKED
	LDA #$01		;BLOCKS MOVE BY RECORDING $01	LDA #$00		;PERMITTS MOVE BY RECORDING $00 
	STA SUMMON.COLLISION.RETURN_VALUE
	JMP SUMMON.COLLISION.EXIT


; .COLLISION.LOOP.MOB_SEA	
	; ;***<INSERT RULES FOR SEA MOBS>***
	; ;COPY THE SECTION ABOVE FOR LAND MOBS AS A STARTING POINT
	
	; ;****NOTE: make sure to check for player location using regular constant and also the constants for the MT frigate if frigate is active as transport

@END
	
SUMMON.COLLISION_CHECK.MTT  ;
@START

.IDENTIFY.MTT.TILE.SCREEN_LOCATIONS
	;set Multi-tile S_ENTITY tile screen locations
	LDY SUMMON.TARGET.SINDEX
	STY SUMMON.MT.TILE_LOCATIONS+$0 ;set screen location of upper left tile

	STY SUMMON.MT.TILE_LOCATIONS+$1 ;set screen location of upper right tile
	INC SUMMON.MT.TILE_LOCATIONS+$1 ;+1 tile right

	TYA
	CLC
	ADC #SCREEN.ARRAY.OFFSET ;+1 tile down	
	STA SUMMON.MT.TILE_LOCATIONS+$2 ;set screen location of lower left tile

	TYA
	CLC
	ADC #SCREEN.ARRAY.OFFSET+1 ;+1 tile down, +1 tile right	
	STA SUMMON.MT.TILE_LOCATIONS+$3 ;set screen location of lower right tile
		
	LDX #$00
.MTT.COLLISION.CHECKS.LOOP

	LDY SUMMON.MT.TILE_LOCATIONS,X	;LOAD NEXT ADJACENT TILE#
	
.WALKING_RULES		
;APPLY MOB WALKING COLLISION RULES

.MAP_OBJECT.CHECKS	
	;CHECK FOR OTHER SPRITES
	LDA SCREEN.MO_SPRITE.DATA,Y					;DOES DESTINATION TILE HAVE A MOB MO?	
	CMP #$FF
	BNE .MOVE_BLOCKED

	;CHECK FOR GENERAL MAP OBJECTS
	LDA SCREEN.MO_GENERAL.DATA,Y	;load general map object data for current tile location
	CMP #$FF						;is a general map object present?
	;BEQ .SPRITE.WALKING.TILE.TESTS	;if no, then continue with other checks
	BNE .MOVE_BLOCKED

.TERRAIN.OBSTACLE.CHECKS
	LDA SCREEN.TILE.DATA,Y						;LOAD TILE TYPE OF DESTINATION TILE

	;obstacle test #1
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
	;**FALLS THROUGH
	
	;JMP .MOVE_PERMITTED

.MOVE_PERMITTED
	LDA #$00		;PERMITTS MOVE BY RECORDING $00 
	STA SUMMON.COLLISION.RETURN_VALUE
	JMP .EXIT.TEST

.MOVE_BLOCKED
	LDA #$01		;BLOCKS MOVE BY RECORDING $01	LDA #$00		;PERMITTS MOVE BY RECORDING $00 
	STA SUMMON.COLLISION.RETURN_VALUE
	JMP SUMMON.COLLISION.EXIT

.EXIT.TEST
	INX ;next tile screen location
	CPX #$04
	BNE .MTT.COLLISION.CHECKS.LOOP
	
; .COLLISION.LOOP.MOB_SEA	
	; ;***<INSERT RULES FOR SEA MOBS>***
	; ;COPY THE SECTION ABOVE FOR LAND MOBS AS A STARTING POINT
	
	; ;****NOTE: make sure to check for player location using regular constant and also the constants for the MT frigate if frigate is active as transport

	;**FALLS THROUGH**
@END

	;**FALLS THROUGH**
	
SUMMON.COLLISION.EXIT

;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX
	
	LDA SUMMON.COLLISION.RETURN_VALUE
			
	RTS
	
@END
	
GET.SPECIAL.RECORD
@START
;PARAMETERS; none
;ENTRANCE: DIRECT
;RETURN: ;X-REG RETURNS THE INDEX TO THE MO RECORD CREATED

;=====================SUBROUTINE DOCUMENTATION====================================
;Returns index to the next open record in the SPECIAL combat array, 
;
;(DISABLED) or if SUMMON.COLLISION.OVERIDE is set (= $01)
;then return the index to the record of the existing S_ENTITY which the new S_ENTITY collided with. 
;(DISABLED) [SUMMON.COLLISION.OVERIDE], [ARMGDN.CLOBBERED.MOB.MO_INDEX]
;=================================================================================


;SAVE REGISTERS
	PHA
	TYA
	PHA
	
	
; ;DETERMINE WHICH ROUTINE TO USE	
; ;(note: ARMGDN.CLOBBERED.MOB.MO_INDEX will be set to its init value unless the summoned S_ENTITY is
; ;clobbering an existing S_ENTITTY)
	
	; LDA ARMGDN.CLOBBERED.MOB.MO_INDEX ;($FF = disabled | any other value is a map object index)
	; CMP #$FF
	; BEQ .FIND.NEXT.OPEN.RECORD

	
	; ;**FALLS THROUGH**
	
; .CLOBBER.EXISTING.RECORD
	; LDA ARMGDN.CLOBBERED.MOB.MO_INDEX ;load return value. The routine that calls GE.SUMMON is responsible for setting this parameter. 
	
	; TAX
	; JMP .EXIT
	
.FIND.NEXT.OPEN.RECORD	
;FIND OPEN MO RECORD
	LDX #$00
.RECORD_LOOP
	LDA COMBAT.MAP_OBJECTS.SPECIAL+$2,X
	CMP #$00								;#$00 IS THE ARRAY STOP VALUE
	BEQ .OPEN_RECORD_FOUND
	TXA
	CLC
	ADC #COMBAT.MAP_OBJECTS.SPECIAL.RECORD_SIZE
	BEQ .ARRAY_FULL							;IF COUNTER FLIPS TO #$00, THEN NO OPEN RECORDS AVAILABLE, EXIT

	TAX
	JMP .RECORD_LOOP

.ARRAY_FULL
	;IF THE MAP OBJECT ARRAY IS FULL, THE FIRST RECORD WILL ALWAYS BE OVERWRITTEN
	;**FALLS THROUGH

.OPEN_RECORD_FOUND
	;convert map object index (X-REG) to standardized value. (SPECIAL array starts at the NPC array + $80)
	TXA
	SEC
	SBC #COMBAT.MAP_OBJECTS.SPECIAL.START_RECORD
	TAX		

.EXIT
	
;RESTORE REGISTERS
	PLA
	TAY
	PLA
	
;RETURN VALUE
	;X-REG RETURNS THE INDEX TO THE MO RECORD CREATED

	RTS
@END


@END

;SHAPE TABLE: SUMMONING WARP (MTT)
@START
WARP_MTT.SHAPE.START.FRAME0				
.SPELL.SUMMON.WARP_MTT.FRAME0.SHAPE_0.AND_MASK			.HS	7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F
.SPELL.SUMMON.WARP_MTT.FRAME0.SHAPE_0.ORA_MASK			.HS	00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00
				
.SPELL.SUMMON.WARP_MTT.FRAME0.SHAPE_1.AND_MASK			.HS	7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F
.SPELL.SUMMON.WARP_MTT.FRAME0.SHAPE_1.ORA_MASK			.HS	00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00
				
.SPELL.SUMMON.WARP_MTT.FRAME0.SHAPE_2.AND_MASK			.HS	7F.7F.7F.0F.7F.0F.7F.0F.7F.0F.7F.0F.7F.0F.7F.0F.7F.0F.7F.03.3F.00.0F.40.03.40.03.20.07.00.1F.00
.SPELL.SUMMON.WARP_MTT.FRAME0.SHAPE_2.ORA_MASK			.HS	00.00.00.40.00.40.00.40.00.40.00.40.00.40.00.40.00.40.00.70.00.3C.40.07.70.0C.30.06.60.4F.00.78
				
.SPELL.SUMMON.WARP_MTT.FRAME0.SHAPE_3.AND_MASK			.HS	7F.7F.78.7F.78.7F.78.7F.78.7F.78.7F.78.7F.60.7F.00.7F.00.7E.00.7C.00.70.00.60.00.70.00.7C.06.7F
.SPELL.SUMMON.WARP_MTT.FRAME0.SHAPE_3.ORA_MASK			.HS	00.00.01.00.01.00.01.00.01.00.01.00.01.00.01.00.1F.00.33.00.66.00.66.03.3C.06.4F.03.79.00.00.00
				
WARP_MTT.SHAPE.START.FRAME1				
.SPELL.SUMMON.WARP_MTT.FRAME1.SHAPE_0.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.FF.FF.FF.BF.FF.8F.FF.8F.FF.8F.FF.8F.FF.8F.FF.8F.FF.83.FF.80.BF.80.FF.80.FF.83
.SPELL.SUMMON.WARP_MTT.FRAME1.SHAPE_0.ORA_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.C0.80.C0.80.C0.80.C0.80.C0.80.C0.80.E0.80.BC.80.AE.80.BC.80.B0
				
.SPELL.SUMMON.WARP_MTT.FRAME1.SHAPE_1.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.FF.FF.FE.FF.F8.FF.F8.FF.F8.FF.F8.FF.F8.FF.F8.FF.F0.FF.C0.FF.80.FE.80.FE.80.FF
.SPELL.SUMMON.WARP_MTT.FRAME1.SHAPE_1.ORA_MASK			.HS	80.80.80.80.80.80.80.80.80.80.81.80.81.80.81.80.81.80.81.80.81.80.83.80.87.80.BD.80.B5.80.9F.80
				
.SPELL.SUMMON.WARP_MTT.FRAME1.SHAPE_2.AND_MASK			.HS	FF.83.FF.87.FF.87.FF.87.FF.87.FF.87.FF.81.BF.80.8F.80.87.80.81.80.80.80.80.C0.80.80.80.80.83.80
.SPELL.SUMMON.WARP_MTT.FRAME1.SHAPE_2.ORA_MASK			.HS	80.B0.80.E0.80.E0.80.E0.80.E0.80.E0.80.F8.80.FE.C0.E1.E0.F0.F8.98.8C.9B.CE.8F.E6.8C.9C.E3.F0.9C
				
.SPELL.SUMMON.WARP_MTT.FRAME1.SHAPE_3.AND_MASK			.HS	F0.FF.F0.FF.F0.FF.F0.FF.F0.FF.C0.FF.80.FE.80.F8.90.F0.80.C0.80.80.80.80.81.C0.80.C0.80.E0.BC.F8
.SPELL.SUMMON.WARP_MTT.FRAME1.SHAPE_3.ORA_MASK			.HS	83.80.83.80.83.80.83.80.83.80.83.80.B3.80.CF.81.83.83.87.8F.CE.99.8C.98.B8.8C.E6.8C.C3.87.80.80
				
WARP_MTT.SHAPE.START.FRAME2				
.SPELL.SUMMON.WARP_MTT.FRAME2.SHAPE_0.AND_MASK			.HS	FF.8F.FF.8F.FF.8F.FF.8F.FF.87.FF.87.FF.81.FF.80.FF.80.FF.80.BF.80.9F.80.8F.80.83.80.8F.80.9F.80
.SPELL.SUMMON.WARP_MTT.FRAME2.SHAPE_0.ORA_MASK			.HS	80.C0.80.C0.80.C0.80.C0.80.E0.80.E0.80.F8.80.BC.80.AC.80.AC.80.AE.80.AF.C0.AB.F0.AA.C0.AB.80.AF
				
.SPELL.SUMMON.WARP_MTT.FRAME2.SHAPE_1.AND_MASK			.HS	F8.FF.F8.FF.F8.FF.F0.FF.E0.FF.C0.FF.C0.FF.C0.FF.80.FF.80.FE.80.FE.80.FC.80.F0.80.E0.80.F0.80.FC
.SPELL.SUMMON.WARP_MTT.FRAME2.SHAPE_1.ORA_MASK			.HS	81.80.81.80.81.80.83.80.87.80.8F.80.8D.80.8D.80.9D.80.B5.80.B5.80.F5.80.D4.83.D4.86.D5.83.F4.80
				
.SPELL.SUMMON.WARP_MTT.FRAME2.SHAPE_2.AND_MASK			.HS	FF.80.BF.80.8F.80.83.80.80.80.80.90.80.F8.80.F8.80.C0.90.80.B0.80.80.90.80.F8.80.FC.80.80.80.80
.SPELL.SUMMON.WARP_MTT.FRAME2.SHAPE_2.ORA_MASK			.HS	80.AC.80.AE.C0.BF.F0.BC.9C.E6.8E.83.FE.81.C3.81.C3.81.83.BB.83.EE.C6.83.C6.81.E6.80.E7.80.FC.FF
				
.SPELL.SUMMON.WARP_MTT.FRAME2.SHAPE_3.AND_MASK			.HS	80.FE.80.FC.80.E0.80.80.80.80.C0.83.E0.80.80.80.88.80.80.80.80.80.80.86.81.87.83.82.80.80.80.E0
.SPELL.SUMMON.WARP_MTT.FRAME2.SHAPE_3.ORA_MASK			.HS	B5.80.F5.80.D5.81.FD.9F.9D.B0.8F.E0.87.FC.87.E7.C0.E1.C0.E1.CF.E1.8E.E0.98.E0.B0.B0.B0.98.FF.87
				
WARP_MTT.SHAPE.START.FRAME3				
.SPELL.SUMMON.WARP_MTT.FRAME3.SHAPE_0.AND_MASK			.HS	FF.80.87.80.83.80.87.80.83.80.81.80.80.80.80.80.80.80.81.80.87.80.83.80.80.D0.80.F0.80.C0.83.F0
.SPELL.SUMMON.WARP_MTT.FRAME3.SHAPE_0.ORA_MASK			.HS	80.BC.E0.EF.F0.EA.E0.FB.F0.FB.F8.BA.DC.BE.FE.BA.C6.AB.E0.AA.E0.A2.F0.A2.DC.82.D7.82.DC.8A.F0.82
				
.SPELL.SUMMON.WARP_MTT.FRAME3.SHAPE_1.AND_MASK			.HS	FF.FF.F8.C0.98.80.80.80.80.80.80.80.80.80.80.C0.80.F0.80.F0.80.E0.82.C0.80.80.82.80.80.80.80.C0
.SPELL.SUMMON.WARP_MTT.FRAME3.SHAPE_1.ORA_MASK			.HS	80.80.81.80.81.BF.C3.EB.E7.BA.E7.9A.FD.9A.DD.8E.D5.83.D4.83.D4.86.D0.8E.D5.9A.D0.EA.D1.9A.D4.8E
				
.SPELL.SUMMON.WARP_MTT.FRAME3.SHAPE_2.AND_MASK			.HS	81.C0.81.C0.80.D0.80.80.80.80.89.80.9F.80.BF.80.FF.80.93.90.80.E0.80.F0.80.BC.80.CE.80.83.E1.81
.SPELL.SUMMON.WARP_MTT.FRAME3.SHAPE_2.ORA_MASK			.HS	D8.8A.F0.8A.E6.82.CF.AB.E6.AA.C0.AB.80.AB.80.BE.80.E0.80.83.CC.87.9F.83.E3.80.B3.C0.9E.B0.80.98
				
.SPELL.SUMMON.WARP_MTT.FRAME3.SHAPE_3.AND_MASK			.HS	83.E0.80.C0.88.C0.80.C0.80.E0.80.F0.80.F8.80.F8.80.E4.80.81.B8.C0.9F.80.8C.80.90.C0.E0.F0.E0.FF
.SPELL.SUMMON.WARP_MTT.FRAME3.SHAPE_3.ORA_MASK			.HS	D0.86.D4.8E.C1.8A.C4.8E.D5.86.D5.83.DD.81.FD.81.FD.80.87.98.80.8E.80.B3.C0.B1.83.8F.86.80.86.80

@END

;SHAPE TABLE: SUMMONING WARP (single)
@START
WARP_SINGLE.SHAPE.START.FRAME0				
.SPELL.SUMMON.WARP.SINGLE.SHAPE_0.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.BF.FE.8F.F8.8F.F8.8F.F8.8F.F8.8F.F8.83.F0.80.C0.80.C0.81.C0
.SPELL.SUMMON.WARP.SINGLE.SHAPE_0.ORA_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.80.80.C0.81.C0.81.C0.81.C0.81.C0.81.F0.81.9C.8F.CC.89.F8.8F
				
.SPELL.SUMMON.WARP.SINGLE.SHAPE_1.AND_MASK			.HS	7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.4F.7F.03.7E.00.78.00.78.00.78.00.60.00.60.01.70.00.40.00.40.01.40
.SPELL.SUMMON.WARP.SINGLE.SHAPE_1.ORA_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.B0.80.FC.81.D6.81.D6.81.D6.86.D6.86.F8.83.9C.8F.CC.89.F8.8F
				
.SPELL.SUMMON.WARP.SINGLE.SHAPE_2.AND_MASK			.HS	FF.FF.83.F2.81.C0.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.C0.80.F0
.SPELL.SUMMON.WARP.SINGLE.SHAPE_2.ORA_MASK			.HS	80.80.B0.80.B8.8C.D4.9F.D4.EA.DE.EA.D6.9A.D6.9A.D6.9E.D4.9E.D4.BA.D4.BA.D4.9A.DE.9E.D6.8F.BE.82
				
.SPELL.SUMMON.WARP.SINGLE.SHAPE_3.AND_MASK			.HS	01.78.00.60.00.40.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.01.00.07.00
.SPELL.SUMMON.WARP.SINGLE.SHAPE_3.ORA_MASK			.HS	F8.81.DC.86.DC.86.D6.BE.D6.EA.D6.EA.D7.EA.D7.BA.D7.BA.D6.9A.D6.9A.D7.9A.D6.9A.DC.BA.F8.EB.80.BE

@END


;SPELL.SUMMON.LOCAL.VARIABLES
@START
;(shares memory with combat explosion and the existing data buffer used by COMBAT.SHAPE.DRAW. Summon doesn't call those
;routines so there isn't a conflict.)


SPELL.SUMMON.LOCAL_VAR.BLOCK .BS $08


; SUMMON.PARM.TILE_TYPE	.EQ SHAPE.EXISTING_DATA.BUFFER+$0 ;$1 byte
; SUMMON.PARM.FLAGS		.EQ SHAPE.EXISTING_DATA.BUFFER+$1 ;$1 byte
; SUMMON.PARM.ALIGNMENT	.EQ SHAPE.EXISTING_DATA.BUFFER+$2 ;$1 byte
; SUMMON.PARM.ENEMY_TYPE	.EQ SHAPE.EXISTING_DATA.BUFFER+$3 ;$1 byte
; SUMMON.TARGET.SINDEX	.EQ SHAPE.EXISTING_DATA.BUFFER+$4 ;$1 byte

SUMMON.PARM.TILE_TYPE	.EQ SPELL.SUMMON.LOCAL_VAR.BLOCK+$0 ;$1 byte
SUMMON.PARM.FLAGS		.EQ SPELL.SUMMON.LOCAL_VAR.BLOCK+$1 ;$1 byte
SUMMON.PARM.ALIGNMENT	.EQ SPELL.SUMMON.LOCAL_VAR.BLOCK+$2 ;$1 byte
SUMMON.PARM.ENEMY_TYPE	.EQ SPELL.SUMMON.LOCAL_VAR.BLOCK+$3 ;$1 byte
SUMMON.TARGET.SINDEX	.EQ SPELL.SUMMON.LOCAL_VAR.BLOCK+$4 ;$1 byte


SUMMON.COLLISION.RETURN_VALUE	.EQ SPELL.SUMMON.LOCAL_VAR.BLOCK+$5 ;$1 byte
SUMMON.COLLISION.OVERIDE 	.BS $01 ;(.BS for init purpose). ($00 = off, $01 = on)
SUMMON.MT.TILE_LOCATIONS	.EQ COMBAT.EXPLOSION.CENTER+$0 ;$4 bytes
SUMMON.MT.TILE_TYPES		.EQ COMBAT.EXPLOSION.CENTER+$4 ;$4 bytes

SUMMON.QTY					.EQ SPELL.SUMMON.LOCAL_VAR.BLOCK+$6 ;$1 byte
SUMMON.MO_INDEX				.EQ SPELL.SUMMON.LOCAL_VAR.BLOCK+$7 ;$1 byte
@END

@END

	.EP ;end patch mode
;--------
GE.SPELL.SUMMON.END ;marks the end address of the code block for this spell
@END
;=======================

GE.SPELL.DIE.START ;marks the start address of the code block for this spell
;--------
@START
	.PH SPELL.FILE.CODE_BLOCK.BUFFER.START ;start patch 

GE.SPELL.DIE 
@START
;PARAMETERS: FP.SOURCE.GMAP.X, FP.SOURCE.GMAP.Y, FP.TARGET.GMAP.X, FP.TARGET.GMAP.Y
;ENTRANCE: DIRECT		
;RETURN: targets hit (COMBAT.TARGET_HIT.DB)

;=====================SUBROUTINE DOCUMENTATION====================================
;
;-target hits
;COMBAT.TARGET_HIT.DB is populated by a call to .TARGET.HIT.CHECK from .GE.EXPLOSION
;
;
;=================================================================================


.INIT
	
	JSR .LOAD.SHAPE.TABLE

		
;FIRE PROJECTILE

		;LDA #$00  ;disable (support was added for this)
		LDA #COMBAT.PROJECTILE.DELAY	;(time target = 1.6 seconds when there are 7 empty tiles between source and target)
		STA SHAPE.MOVE.DELAY_LENGTH ;The length of the delay between the shape draw and shape erase.

		LDA #$00 ;set of on
		STA SHAPE.MOVE.SCREEN_ANIMATION.TOGGLE ;($00 = screen animation on | >=$01 = screen animation off)
		STA SHAPE.MOVE.ERASE.TOGGLE	;($00 = shape erase on | >=$01 = shape erase off)		
		LDA #$01	;set mode to angled shape ;($00 = angled static shape | >=$01 = non-angled, animated shape)
			;STA SHAPE.MOVE.SCREEN_ANIMATION.TOGGLE ;($00 = screen animation on | >=$01 = screen animation off)
	JSR COMBAT.FIRE.PROJECTILE

	;draw shape on target
	;(note: this is done because sometimes the COMABT.SHAPE.MOVE doesn't draw the shape directly on the target. Sometimes it does.)
	JSR .DRAW.SHAPE.ON.TARGET
	
	;**FALLS THROUGH**
	
.UPDATE.TARGET_HIT.DATABASE	
;get screen location of prospective GMAP.X/Y of target
		LDA FP.TARGET.GMAP.X ;load new special S_ENTITYY GMAP.X
		STA PARM.GMAP.X
		LDA FP.TARGET.GMAP.Y ;load new special S_ENTITY GMAP.Y
		STA PARM.GMAP.Y				
	JSR CONVERT.COLUMN_ROW.TO.SINDEX
		LDY RETURN.SCREEN_ARRAY_INDEX ;used below
	JSR .TARGET.HIT.CHECK

.SPELL_DAMAGE

	LDA #$00 ;set kill flag = success
    STA COMBAT.STATS.SPELL.KILL_FLAG_MASTER ;($00 = succeeded | $01 = not attempted | $03 = failed). If set then all targets hit by the spell are automatically killed. Sometimes saving thows may be available. 
	
.SET.CRITICAL_HIT.FLAGS
@START
;(engaged s_entity screen index)

		;**warning: attacker character sheet will be loaded on exit
		LDY COMBAT.ATTACKER.SINDEX
		LDA COMBAT.TARGET_HIT.DB+$0 ;set value to update (sindex of the target)	
.UPDATE.CHR_SHEET.ENGAGED.INDEX ;**OPT** Memory. This routine was written as as subroutine, but is being used inline here and it could be made more efficient. 
;PARAMETERS: YREG = screen index of attacker, ACC = value to write to CHR_SHEET.PC_MOB.ENGAGED.SINDEX
;ENTRANCE: direct
;RETURN: saved attacker character sheet with updated CHR_SHEET.PC_MOB.ENGAGED.SINDEX field

.SAVE.PARAMETERS.ENAGED
	;ACC = value to write to CHR_SHEET.PC_MOB.ENGAGED.SINDEX
	STA COMBAT.ENGAGED_SINDEX.UPDATE.PARM
	
	;read attacker (MOB) character sheet
		
		;YREG = COMBAT.ATTACKER.SINDEX
		LDA #$00 ;set read mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet

			
	;set the engaged field to the updated value
	;(expected to be the screen index of the defender or $FF to indicate no engagement)
	LDA COMBAT.ENGAGED_SINDEX.UPDATE.PARM
	STA CHR_SHEET.PC_MOB.ENGAGED.SINDEX

			
		;YREG = COMBAT.ATTACKER.SINDEX
		LDA #$01 ;set write mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet

@END

.EXIT
	RTS


@END

.LOAD.SHAPE.TABLE
@START
;PARAMETERS: none
;ENTRANCE: direct

;load the shape into the hoppers
	LDX #$00
.PROJECTILE.LOAD.SHAPE.LOOP0
	LDA SHAPE_TABLE.DIE.START,X	
	STA SHAPE.HOPPER0,X
	INX
	BNE .PROJECTILE.LOAD.SHAPE.LOOP0

	RTS
@END

.DRAW.SHAPE.ON.TARGET
@START
;PARAMETERES: none
;ENTRANCE: direct

	;refresh shape table
	;(note: this needs to be done because on sharp angles COMBAT.FIRE.PROJECTILE or its subroutines modify the contents
	;of SHAPE_HOPPER0+$0-SHAPE_HOPPER0+$1F for some reason. I'm not sure why, but refreshing the shape tables resolve the issue for this routine.)
	JSR .LOAD.SHAPE.TABLE
	
	
		;set parameters
		LDY SHAPE.SINDEX.TARGET	
		LDA SCREEN.INDEX.TILE_SBYTE,Y
		STA	SHAPE.DRAW.CURRENT.SBYTE
		
		LDA SCREEN.INDEX.TILE_LINE,Y
		STA SHAPE.DRAW.LINE_ROW.NUMBER

		;Y-REG = screen tile lcation of target
		LDA #$01 ;set foreground page mode
		STA PAGE.FOREGROUND.OVERRIDE
		LDA #$00 ;set draw mode
	JSR .DRAW.TILE.AND_ORA
	
		LDA #$A0		
	JSR WAIT.LOOP

	RTS


@END

.DRAW.TILE.AND_ORA ;draw single tile using AND + ORA mask
@START
;PARAMETERS:  	;ACC = ($00 = draw mode | $01 = erase mode), SHAPE.DRAW.LINE_ROW.NUMBER, SHAPE.DRAW.CURRENT.SBYTE, [PAGE.FOREGROUND.OVERRIDE]
;RETURN: NONE
;ENTRANCE: .DRAW.SHAPE.ON.TARGET
;*Set to default value on game launch. When modified (i.e. tall grass), it is reset to default when done.

;=====================SUBROUTINE DOCUMENTATION====================================
;This subroutine, COMBAT.SHAPE.DRAW and DRAW.TILE are the lowest level routines associated with tile graphics plotting. 
;See DRAW.TILE for general documentation on the methodology used to plot to the graphics screen.
;
;This subroutine's unique purpose is to be able to draw a single tile using an AND+ORA mask. 
;
;This subroutine is specifically designed to draw shapes which are objects drawn on top of a background. The underlying
;terrain can be supplied by terrain tiles or other object tiles. However, this routine can draw over any 14x16 section
;the hi-res screen whether it conforms to the tile grid or not.
;
;To blend shapes into the background, two shape tables are used. One containing and AND mask and the other containing
;an ORA mask.
;
;here is a quick summary of the concept:
;(also see the first two posts from Michael Pohoreski in the following
;post https://groups.google.com/forum/#!topic/comp.sys.apple2.programmer/cgxZOBIL728)
;
; byte1: 1111110
; byte2: 0000010
;
; Using AND on byte 1 and byte 2 results in:
;
; result: 0000010
;
; (i.e. each bit is assigned the value 1 only if both byte1 AND byte2 have a 1 in that bit position.
;
;
; OR works similarly except that a 1 is assigned to each bit position in the result if either byte1 OR byte2 has a 1 in that bit position. 
;
;
; The technique works like this:
;
; before drawing the shape copy the background, and apply an AND value to each byte. The AND values has "0" values where the shape will be. The result is a shape table that has the background and a small "hole" carved out where the shape will go.
;
; Then apply an OR value to each byte. the OR values have "1" values where the shape is. The result is shape table with 1s where either the background or the shape is. This is different from simply drawing the shape on top of the background because when the "hole" is carved out, a two pixel buffer is put around the shape so that the background pixels and the shape pixels don't interact to create unwanted lines and colors. 
;=================================================================================
	
	;ACC = $00 = draw mode | $01 = erase mode
	STA DRAW.SHAPE.MODE

;**OPT** Speed. Include an in-line code version of DRAW.TILE with DRAW.COLUMN and DRAW.ROW to avoid the costly JSR/RTS to call as a subroutine. 

.SAVE.REGISTERS
	TXA
	PHA
	TYA
	PHA

.START	
	LDX SHAPE.DRAW.LINE_ROW.NUMBER			;LOAD LINE IN X REGISTER	
	TXA ;transfer SHAPE.DRAW.LINE_ROW.NUMBER to ACC
	STA TILE.LINE.CNTR 
	STA TILE.LINE.START
	CLC						;*OPT* I I THINK ALL CLS CAN BE REMOVED WHEN I'M NOT EXPECTING AN OVERFLOW
	ADC #TILE.DEPTH.STANDARD				;ADD DEPTH OF SHAPE TO STARTING LINE IN HEX (# of lines, not the position of last line....so line positions $0-$F is $10 (!16) total lines)
	STA TILE.LINE.STOP		;SET THE LAST LINE TO DRAW BASED ON THE DEPTH OF THE TILE

	LDA #$00
	STA SHP.TBL.CNTR		;START DRAWING AT BEGINNING OF SHAPE TABLE
	
	
.DRAW.LOOP

;GET LINE ADDRESS
@START

;==========INLINE CODE GET.LINE.ADDRESS1==================	
	LDA PAGE.FOREGROUND.OVERRIDE	;CHECK TO SEE IF CALLING ROUTINE WANTS ERASE DONE TO FOREGROUND INSTEAD OF BACKGROUND (DEFAULT)
	CMP #$01
	BEQ .FOREGROUND_OVERRIDE
	
	LDA PAGE.BACKGROUND		
	CMP #$02
	BEQ .LOOKUP.PAGE2
	JMP .LOOKUP.PAGE1
	
.FOREGROUND_OVERRIDE
	LDA PAGE.FOREGROUND		;SPECIFY PAGE TO GET ADDRESS FOR (SHOULD BE BACKGROUND PAGE)
	CMP #$02
	BEQ .LOOKUP.PAGE2
	

.LOOKUP.PAGE1	
	LDA LINE.HO.P1,X		;GET LINE ADDRESS
	STA LINE.BASE.ADDR1+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR1
	JMP .GETLINE_COMPLETE

.LOOKUP.PAGE2
	LDA LINE.HO.P2,X		;GET LINE ADDRESS
	STA LINE.BASE.ADDR1+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR1
.GETLINE_COMPLETE
;========================================================
@END
	
.DRAW_ERASE.ENTRANCE
	LDA DRAW.SHAPE.MODE ;ACC = $00 = draw mode | $01 = erase mode
	;BNE .ERASE.MODE
	BEQ .DRAW.MODE
	JMP .ERASE.MODE
	;**FALLS THROUGH**
	
.DRAW.MODE	
;screen byte 0
	LDY SHP.TBL.CNTR		;LOAD Y WITH SHAPE TABLE COUNTER	
	;init counters/indexes
	TYA ;init X-REG to $00 as the SHAPE.EXISTING_DATA.BUFFER index
	TAX ;""
	

	;save existing graphics data
	LDY SHAPE.DRAW.CURRENT.SBYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	LDA (LINE.BASE.ADDR1),Y	;load existing data from video memory
	STA SHAPE.EXISTING_DATA.BUFFER,X
			;STA TEMP
	;load new shape data
	;ACC = existing graphics data
	LDY SHP.TBL.CNTR		;LOAD Y WITH SHAPE TABLE COUNTER	
	AND (SHAPE.BACKGROUND.AND_MASK),Y
	ORA (SHAPE.COMPOSITE.ORA_MASK),Y
		
	;write new shape data
	LDY SHAPE.DRAW.CURRENT.SBYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	STA (LINE.BASE.ADDR1),Y	;PLOT (1st screen byte)
	
;next screen byte
	INC SHP.TBL.CNTR		;NEXT SHAPE BYTE
	INX	;next existing data buffer byte
	INC SHAPE.DRAW.CURRENT.SBYTE			;SWITCH TO 2ND SCREEN BYTE IN THE TILE	
	
;screen byte 1
	
	;save existing graphics data
	LDY SHAPE.DRAW.CURRENT.SBYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	LDA (LINE.BASE.ADDR1),Y	;load existing data from video memory
	STA SHAPE.EXISTING_DATA.BUFFER,X
		
	;load new shape data
	;ACC = existing graphics data
	LDY SHP.TBL.CNTR		;LOAD Y WITH SHAPE TABLE COUNTER	
	AND (SHAPE.BACKGROUND.AND_MASK),Y
	ORA (SHAPE.COMPOSITE.ORA_MASK),Y
		
	;write new shape data
	LDY SHAPE.DRAW.CURRENT.SBYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	STA (LINE.BASE.ADDR1),Y	;PLOT (1st screen byte)
	JMP .NEXT.LINE
	
.ERASE.MODE
;screen byte0
	LDY SHP.TBL.CNTR		;LOAD Y WITH SHAPE TABLE COUNTER
	LDA SHAPE.EXISTING_DATA.BUFFER,Y			;LOAD SHAPE BYTE (1st screen byte of the tile)

	LDY SHAPE.DRAW.CURRENT.SBYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	STA (LINE.BASE.ADDR1),Y	;PLOT (1st screen byte)

;next screen byte	
	INC SHP.TBL.CNTR		;NEXT SHAPE BYTE, NEXT TILE LINE
	INC SHAPE.DRAW.CURRENT.SBYTE			;SWITCH TO 2ND SCREEN BYTE IN THE TILE

;screen byte1	
	LDY SHP.TBL.CNTR		;LOAD Y WITH SHAPE TABLE COUNTER
	LDA SHAPE.EXISTING_DATA.BUFFER,Y			;LOAD SHAPE BYTE (2nd screen byte of the tile)
	LDY SHAPE.DRAW.CURRENT.SBYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	STA (LINE.BASE.ADDR1),Y	
	;**FALLS THROUGH**
	
.NEXT.LINE
;next line, reset screen byte	
	DEC SHAPE.DRAW.CURRENT.SBYTE			;SWITCH BACK TO 1ST SCREEN BYTE IN THE TILE	;**OPT** if the counters are updated after the CMP/branch it might save 1 INC SHAPE.DRAW.CURRENT.SBYTE instruction in the main loop because SHAPE.DRAW.CURRENT.SBYTE will be in 2nd position when this loop ends. 
	INC SHAPE.DRAW.LINE_ROW.NUMBER
	
	INC SHP.TBL.CNTR		;NEXT SHAPE BYTE
	INC TILE.LINE.CNTR		;NEXT TILE LINE
	LDX TILE.LINE.CNTR 		;restore tile line number to X-REG

;exit test	
	CPX TILE.LINE.STOP		;IS TILE DONE?							
	BCS .DONE				
	JMP .DRAW.LOOP
.DONE	
	LDA #$00
	STA PAGE.FOREGROUND.OVERRIDE
				
.RESTORE.REGISTERS
	PLA
	TAY
	PLA
	TAX
	
	RTS

@END	

.TARGET.HIT.CHECK
@START
;PARAMETERS: YREG (screen index), COMBAT.EXPLOSION.PROB

;SAVE PARAMETERS
	TXA
	PHA
	
;WAS A TARGET PRESENT?				
	;YREG = screen index
	LDA SCREEN.MO_SPRITE.DATA,Y
	CMP #$FF 
	BEQ .CHECK.TARGET_HIT.EXIT ;if no S_ENTITY is present at this screen location then exit, don't record a target hit

;IS THE HIT TILE THE SPELLCASTER?
	CPY COMBAT.ATTACKER.SINDEX
	BEQ .CHECK.TARGET_HIT.EXIT
	
;IS THIS A DUPLICATE HIT?
;(note: this routine is needed because the iteration patter of some spells results in tiles being examined more than once, which could result
;in this routine being called more than once for the same tile)

	LDX #$00
	TYA ;transfer screen tile location to ACC
.DUPLICATE.CHECK.LOOP
	CMP COMBAT.TARGET_HIT.DB,X ;if S_ENTITY in current screen location was already recorded as a hit, then exit
	BEQ	.CHECK.TARGET_HIT.EXIT
	INX
;	CPX #COMBAT.TARGET_HIT.DB.STOP_VALUE
	CPX #COMBAT.TARGET_HIT.DB.SIZE
	BNE .DUPLICATE.CHECK.LOOP
		
	
;RECORD NEW TARGET HIT	
	;YREG = screen index
	TYA
	LDX COMBAT.TARGET_HIT.DB.INDEX
	STA COMBAT.TARGET_HIT.DB,X
	;write stop value
	;(note: each time a new target hit is recorded the stop value is advanced 1 byte in the array)
	LDA #COMBAT.TARGET_HIT.DB.STOP_VALUE
	STA COMBAT.TARGET_HIT.DB+$1,X

			; ;JSR KEYIN
			; STA TEMP
			; CMP #$D1
			; BNE .TEMP
			; ;LDA #$AA
			; LDA COMBAT.TARGET_HIT.DB,X
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP

	INC COMBAT.TARGET_HIT.DB.INDEX	;next target_hit record

.CHECK.TARGET_HIT.EXIT

			
;RESTORE PARAMETERS
	PLA
	TAX
			
	RTS	
@END

;SHAPE TABLE: DIE
@START
SHAPE_TABLE.DIE.START
.SPELL.DIE.SHAPE_0.AND_MASK			.HS	FF.FF.FF.FF.8F.F8.87.F0.87.F0.83.E0.83.E0.83.E0.83.E0.83.E0.83.E0.87.F0.87.F0.8F.F8.FF.FF.FF.FF
.SPELL.DIE.SHAPE_0.ORA_MASK			.HS	00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00
				
.SPELL.DIE.SHAPE_1.AND_MASK			.HS	FF.FF.FF.FF.8F.F8.87.F0.87.F0.83.E0.83.E0.83.E0.83.E0.83.E0.83.E0.87.F0.87.F0.8F.F8.FF.FF.FF.FF
.SPELL.DIE.SHAPE_1.ORA_MASK			.HS	00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00
				
.SPELL.DIE.SHAPE_2.AND_MASK			.HS	FF.FF.FF.FF.8F.F8.87.F0.87.F0.83.E0.83.E0.83.E0.83.E0.83.E0.83.E0.87.F0.87.F0.8F.F8.FF.FF.FF.FF
.SPELL.DIE.SHAPE_2.ORA_MASK			.HS	00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00
				
.SPELL.DIE.SHAPE_3.AND_MASK			.HS	FF.FF.FF.FF.8F.F8.87.F0.87.F0.83.E0.83.E0.83.E0.83.E0.83.E0.83.E0.87.F0.87.F0.8F.F8.FF.FF.FF.FF
.SPELL.DIE.SHAPE_3.ORA_MASK			.HS	00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00
@END

	.EP ;end patch mode
;--------
GE.SPELL.DIE.END ;marks the end address of the code block for this spell
@END
;=======================

GE.SPELL.MASS_DEATH.START ;marks the start address of the code block for this spell
;--------
@START
	.PH SPELL.FILE.CODE_BLOCK.BUFFER.START ;start patch 
	
GE.SPELL.MASS_DEATH
@START
;PARAMETERS: COMBAT.SPELL_CASTER.GMAP.X, COMBAT.SPELL_CASTER.GMAP.Y, SHAPE.ORA.BIT_VALUE, SHAPE.NON_EDGE.BYTE.VALUE

;=====================SUBROUTINE DOCUMENTATION====================================
;
;-target hits
;COMBAT.TARGET_HIT.DB is populated by MD.IDENTIFY.TARGETS
;
;This routine uses custom graphics graphics plotting routines to draw a black or white triangle shape up/down/left/right from the spell caster
;as determined by player input. 
;
;.WRAPPER is the front end of the routine, use the make multiple calls to the draw routine with delays inserted in order to create
;a flickering effect on screen. The flickering gets slower (delays increased) until the shape eventually remains solid onscreen for several seconds.
;
;
;===DETAILS===
;The biggest complication in doing this is drawing the edges of the triangle where the portions of the shape take up less than a full screen byte.
;In these sections AND masks are used to draw the shape byte setting all the bits of the shape to $00 except for the portion of the byte which is the background.
;This works for black since black is drawn using the value $00. For white, an ORA mask is applied to change the bits of the shape to $01. 
;
;The AND/ORA masks are stored in lookup tables:
;
; MD.SPELL.AND_MASK.TABLE.LEFT
; MD.SPELL.AND_MASK.TABLE.RIGHT
;
; MD.SPELL.ORA_MASK.TABLE.LEFT
; MD.SPELL.ORA_MASK.TABLE.RIGHT
;
;SHAPE.BIT.CURRENT is the index to the tables. The index is increased or decreased
;as the bytes of the shape are drawn. 
;
;
;
;-North/South Routines
;
;See "MD Spell (Diagram A) in the my_code/documentation folder"
;
;The routine for drawing north/south are more straight forward than those
;for drawing east/west.
;
;This routine draws one line of the shape at a time. It does so by drawing the left edge byte, the center, and then the right edge byte. 
;The values for the edge bytes are calculated using the AND/ORA masks. The center byte is always $00 for black and $7F for white. 
;
;For North/South, the lookup tables are used as labled. i.e. the table labeled "left" is used for west and the table labeled
;"right" is used for east. 
;
;The SHAPE.BIT.CURRENT is incremented/decremented in the opposite direction
;for each casting direction. (see "MD Spell (Diagram 1")
;
;
;-East/West Routines
;
;See "MD Spell (Diagram A) in the my_code/documentation folder"
;
;Note: The AND/ORA lookup tables are used in the opposite way they are labeled. See "MD Spell (Diagram A)"
;
;The pattern in which the shape is drawn is much more complicated than for north/south.
;The diagram is color coded to show the sections drawn by different loops. 
;
;Yellow = MD.DRAW.LINE.UPPER.EDGE
;Blue = MD.DRAW.LINE.LOWER.EDGE
;Green = .DRAW.LINE.CENTER_LOOP
;
;The routine starts by drawing the upper edge, then the lower edge, as show in the diagram
;The the screen byte is incremented/decremented. The upper edge is drawn again, the center is drawn, 
;then the lower edge is drawn again.
;
;The upper/center/lower sequence is continued until the shape is fully drawn.
;There are some corner cases. For example, since the draw sequence starts with the upper edge
;things get complicated when the starting line is decremented below the top of the tile screen edge.
;Preventing it from drawing offsreen isn't that hard but the upper edge ends up partially offscreen so part of the
;upper edge has to be drawn. The draw has to start with the correct AND/ORA mask, which is out of sequence from the rest
;of the draws. 
;
;
;-All Directions
;
;*If the spellcaster is in the edge columns/rows of the tile grid and casts the spell in an offscreen direction, the
;cast fails. This check is done in .PLAYER.INPUT.DIRECTION. off tile-grid screen edge checks are also done in the drawing routines. 
;
;
;=================================================================================


.WRAPPER
@START

.INIT.ALL ;applies to all casting directions

	LDA #$01
	STA MD.FLICKER.WAIT_VALUE
	STA IS.SPELL.MASS_DEATH.FLAG	;($00 = no, >$01 = yes)

	LDX #$00
	STX MD.CAST.DIRECTION

	
;CALL DRAW SHAPE 
@START

;LOOP #1	
.LOOP

;DRAW SHAPE

	JSR MD.SPELL.DRAW_SHAPE
		
		; LDA #$AA
		; JSR PREP.BRK
		; BRK
		
;DELAY
		
		LDA MD.FLICKER.WAIT_VALUE
	JSR WAIT.LOOP
		
;ERASE SHAPE
	JSR FLIP.PAGE ;effectively erases the shape because the shape was only drawn to the foreground
	
	;SYNC HI-RES PAGES
	;Note: needed because the pages need to be in sync for animation to work properly. 
	;!!!!! BSR BANK1 !!!!
	JSR COPY.SCREEN.ENTRANCE ;**DON'T REMOVE** (or the screen goes chaotic sometimes when collisions occur with double mover mobs. MO.DRAW must need the graphics pages in sync for some reason). UDPATE: I think this is needed because sometimes, due to key press abort, no copy.screen was done after the last move, so the pages are out of syncing going into MOVE.PASS, which is triggered by non-movement commands like (3)ZAP, Push, Board, etc.


;CHECK FOR KEYPRESS ABORT
	LDA KB_BUFFER
    BPL .KEYPRESS.CHECK.COMPLETE1		;branch if no keypress detected in $C000
	STA KB_BUFFER_ACK       ;clr last key
	CMP #$9B				;ESC KEY
	;BEQ	.FINAL.SHAPE.DRAW
	BEQ .DRAW.DONE
.KEYPRESS.CHECK.COMPLETE1

	
;INCREMENT COUNTERS	
	;INC MD.FLICKER.WAIT_VALUE
	LDA MD.FLICKER.WAIT_VALUE
	CLC
	ADC #$14
	STA MD.FLICKER.WAIT_VALUE
	
	INX
	CPX #$0C ;19
	;LDA MD.FLICKER.WAIT_VALUE
	BNE .LOOP


			
;LOOP #2	
	LDA #$FF
	STA MD.FLICKER.WAIT_VALUE
	
	LDX #$00
	;STX MD.FLICKER.WAIT_VALUE
		
.LOOP2

;DRAW SHAPE
	JSR MD.SPELL.DRAW_SHAPE

;DELAY

			
		LDA MD.FLICKER.WAIT_VALUE
	JSR WAIT.LOOP
	JSR WAIT.LOOP
		
;ERASE SHAPE
	JSR FLIP.PAGE ;effectively erases the shape because the shape was only drawn to the foreground
	
	;SYNC HI-RES PAGES
	;Note: needed because the pages need to be in sync for animation to work properly. 
	;!!!!! BSR BANK1 !!!!
	JSR COPY.SCREEN.ENTRANCE ;**DON'T REMOVE** (or the screen goes chaotic sometimes when collisions occur with double mover mobs. MO.DRAW must need the graphics pages in sync for some reason). UDPATE: I think this is needed because sometimes, due to key press abort, no copy.screen was done after the last move, so the pages are out of syncing going into MOVE.PASS, which is triggered by non-movement commands like (3)ZAP, Push, Board, etc.



;CHECK FOR KEYPRESS ABORT
	LDA KB_BUFFER
    BPL .KEYPRESS.CHECK.COMPLETE2		;branch if no keypress detected in $C000
	STA KB_BUFFER_ACK       ;clr last key
	CMP #$9B				;ESC KEY
	;BEQ	.FINAL.SHAPE.DRAW
	BEQ .DRAW.DONE
.KEYPRESS.CHECK.COMPLETE2

;INCREMENT COUNTERS	
	INX
	CPX #$03 
	;LDA MD.FLICKER.WAIT_VALUE
	BNE .LOOP2		
	
			
;LOOP #3
	LDX #$00
.LOOP3


;DRAW SHAPE
	JSR MD.SPELL.DRAW_SHAPE
	
	
;INSERT DELAY
		LDA MD.FLICKER.WAIT_VALUE
	JSR WAIT.LOOP

;ERASE SHAPE
	JSR FLIP.PAGE ;effectively erases the shape because the shape was only drawn to the foreground
		
	;SYNC HI-RES PAGES
	;Note: needed because the pages need to be in sync for animation to work properly. 
	;!!!!! BSR BANK1 !!!!
	JSR COPY.SCREEN.ENTRANCE ;**DON'T REMOVE** (or the screen goes chaotic sometimes when collisions occur with double mover mobs. MO.DRAW must need the graphics pages in sync for some reason). UDPATE: I think this is needed because sometimes, due to key press abort, no copy.screen was done after the last move, so the pages are out of syncing going into MOVE.PASS, which is triggered by non-movement commands like (3)ZAP, Push, Board, etc.


;CHECK FOR KEYPRESS ABORT
	LDA KB_BUFFER
    BPL .KEYPRESS.CHECK.COMPLETE3		;branch if no keypress detected in $C000
	STA KB_BUFFER_ACK       ;clr last key
	CMP #$9B				;ESC KEY
	;BEQ	.FINAL.SHAPE.DRAW
	BEQ .DRAW.DONE
.KEYPRESS.CHECK.COMPLETE3
		
	INX
	;CPX #$10
	CPX #$08
	BNE .LOOP3
	
	;**falls through**

.FINAL.SHAPE.DRAW
	;final shape draw
	JSR MD.SPELL.DRAW_SHAPE

		
@END
		; LDA COMBAT.ABORT.FLAG	;($00 = not set | $01 = target not found | $02 = ESC pressed to exit attack command | $03 = TAB pressed to toggle left/right hand active). Usually ESC pressed is detected via the value $01 but this use of the abort flag is a special situation where there are 3 cases to detect. 

.DRAW.DONE
			; LDA #$01
			; STA TROUBLESHOOTING.HOOK
			
	JSR MD.IDENTIFY.TARGETS
			

			
GE.SPELL.MASS_DEATH.EXIT

;DEDUCT MP
	JSR COMBAT.SPELL.DEDUCT_MP

	RTS ;return to routine that called GE.SPELL.MASS_DEATH
@END	
	
MD.SPELL.DRAW_SHAPE

;SAVE REGISTERS
	TXA
	PHA
	
	
.CALCULATE.SPELL_CASTER.HIRES.POSITION
@START
		LDA COMBAT.SPELL_CASTER.GMAP.X
		STA PARM.GMAP.X
		LDA COMBAT.SPELL_CASTER.GMAP.Y
		STA PARM.GMAP.Y				
	JSR CONVERT.GMAP.TO.PLAYER_RELATIVE.XY
		LDA RETURN.RELATIVE.X
		STA PARM.RELATIVE.X
		LDA RETURN.RELATIVE.Y
		STA PARM.RELATIVE.Y	
	JSR CONVERT.PLAYER_RELATIVE.XY.TO.SCREEN_ARRAY_INDEX
		LDY RETURN.SCREEN_ARRAY_INDEX ;used below

	LDA SCREEN.INDEX.TILE_SBYTE,Y
	STA SHAPE.SBYTE.START
	STA SHAPE.SBYTE.END
	
	LDA SCREEN.INDEX.TILE_LINE,Y
	STA SHAPE.DRAW.LINE_ROW.NUMBER
	STA SHAPE.LINE.START
@END

.PLAYER.INPUT.DIRECTION
@START
	
	LDA MD.CAST.DIRECTION
	BNE .PARSE.DIRECTION_CODE
	JSR KEYIN.ANIMATION.SINGLE
	STA MD.CAST.DIRECTION	;save direction key for future use
.PARSE.DIRECTION_CODE
	CMP #$8B			;UP ARROW
	BEQ .NORTH
	CMP #$8A			;DOWN ARROW
	BEQ .SOUTH
	CMP #$95			;RIGHT ARROW
	BEQ .EAST
	CMP #$88			;LEFT ARROW
	BEQ	.WEST
	JMP .INVALID.COMMAND

.MD.COMMON.CODE ;PLAY SOUND
@START	
; ;TEMP: PLAY SOUND FOR ALL SPELLS

	; JSR COMBAT.PLAY.SOUND.ELECTRICITY
	

	RTS
@END
 	
;CHECK FOR SCREEN EDGE
;(note: this is because it is a directional spell. No point in letting the player cast the spell off the screen)

.NORTH
	JSR .MD.COMMON.CODE ;PLAY SOUND, DEDUCT MP

	LDA COMBAT.SPELL_CASTER.GMAP.Y ;load spellcaster GMAP y-axis
	BEQ .INVALID.COMMAND
 	JMP MD.CAST.NORTH_SOUTH
	
.SOUTH
	JSR .MD.COMMON.CODE ;PLAY SOUND, DEDUCT MP

	LDA COMBAT.SPELL_CASTER.GMAP.Y ;load spellcaster GMAP y-axis
	CMP #SCREEN.ROW.LAST
	BEQ .INVALID.COMMAND
	JMP MD.CAST.SOUTH

.EAST
	JSR .MD.COMMON.CODE ;PLAY SOUND, DEDUCT MP

	LDA COMBAT.SPELL_CASTER.GMAP.X ;load spellcaster GMAP x-axis
	CMP #COMBAT.SCREEN.COLUMN.LAST
	BEQ .INVALID.COMMAND
	JMP MD.CAST.EAST_STEP
	
.WEST
	JSR .MD.COMMON.CODE ;PLAY SOUND, DEDUCT MP

	LDA COMBAT.SPELL_CASTER.GMAP.X ;load spellcaster GMAP x-axis
	BNE MD.CAST.WEST_STEP
		
	;**FALLS THROUGH**

.INVALID.COMMAND
;the key pressed was not a directional (arrow) key.
	JSR PLAY.SOUND.DUMB_ASS
	
	PLA ;restore registers which were saved upon entrance. The value is no longer needed but it needs to be cleared off the stack.
	
	PLA ;pop RTS off the stack from the JSR to MD.SPELL.DRAW_SHAPE
	PLA 
	
	JMP GE.SPELL.MASS_DEATH.EXIT
	;JMP MD.DRAW_SHAPE.EXIT

MD.CAST.EAST_STEP
	JMP MD.CAST.EAST

MD.CAST.WEST_STEP
	JMP MD.CAST.WEST
@END
	
MD.CAST.SOUTH
;setup as a separate entrance from NORTH so that the start line can be moved down, so that the spell caster isn't erased
	LDA SHAPE.DRAW.LINE_ROW.NUMBER
	CLC
	ADC #TILE.DEPTH.STANDARD+1	;The +2 is to offset the two DECs in MD.CAST.NORTH_SOUTH
	STA SHAPE.DRAW.LINE_ROW.NUMBER
	;**FALLS THROUGH**
MD.CAST.NORTH_SOUTH
@START	
.INIT.LINE_DRAW
	;first line of death shape is one line above the spell caster
	;DEC SHAPE.DRAW.LINE_ROW.NUMBER
	DEC SHAPE.DRAW.LINE_ROW.NUMBER

	INC SHAPE.SBYTE.END ;the first line drawn will span 2 screen bytes, so end = start +1

	LDA #$6
	STA SHAPE.BIT.SCREEN_LEFT.CURRENT ;the first line starts in bit6 of the start screen byte.
	
	LDA #$00
	STA SHAPE.BIT.SCREEN_RIGHT.CURRENT ;the first line ends in bit0 of the end screen byte.

	LDX SHAPE.DRAW.LINE_ROW.NUMBER
.DRAW.OUTER_LOOP
@START
.GET.LINE.ADDRESS ;(FOREGROUND PAGE)
@START

;==========INLINE CODE GET.LINE.ADDRESS1==================	
	; LDA PAGE.FOREGROUND.OVERRIDE	;CHECK TO SEE IF CALLING ROUTINE WANTS ERASE DONE TO FOREGROUND INSTEAD OF BACKGROUND (DEFAULT)
	; CMP #$01
	; BEQ .FOREGROUND_OVERRIDE
	
	; LDA PAGE.BACKGROUND		
	; CMP #$02
	; BEQ .LOOKUP.PAGE2
	; JMP .LOOKUP.PAGE1
	
; .FOREGROUND_OVERRIDE
	LDA PAGE.FOREGROUND		;SPECIFY PAGE TO GET ADDRESS FOR (SHOULD BE BACKGROUND PAGE)
	CMP #$02
	BEQ .LOOKUP.PAGE2
	

.LOOKUP.PAGE1	
	LDA LINE.HO.P1,X		;GET LINE ADDRESS
	STA LINE.BASE.ADDR1+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR1
	JMP .GETLINE_COMPLETE

	
.LOOKUP.PAGE2
	LDA LINE.HO.P2,X		;GET LINE ADDRESS
	STA LINE.BASE.ADDR1+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR1
.GETLINE_COMPLETE
;========================================================
@END


.SET.EDGE.BYTES

.SET.SCREEN_LEFT.BYTE
	LDY SHAPE.SBYTE.START
	LDA (LINE.BASE.ADDR1),Y	;load existing data from video memory
	LDY SHAPE.BIT.SCREEN_LEFT.CURRENT
	AND MD.SPELL.AND_MASK.TABLE.LEFT,Y
	STA SHAPE.VALUE.SCREEN_LEFT
	
	LDA SHAPE.ORA.BIT_VALUE
	BEQ .SET.SCREEN_RIGHT.BYTE
	
	;line is white
	LDA SHAPE.VALUE.SCREEN_LEFT
	ORA MD.SPELL.ORA_MASK.TABLE.LEFT,Y
	STA SHAPE.VALUE.SCREEN_LEFT
	;**FALLS THROUGH**
.SET.SCREEN_RIGHT.BYTE
	LDY SHAPE.SBYTE.END
	LDA (LINE.BASE.ADDR1),Y	;load existing data from video memory
	LDY SHAPE.BIT.SCREEN_RIGHT.CURRENT ;only the bit value for the screen left byte is tracked. the lookup tables for the screen right value take this into account. 
	AND MD.SPELL.AND_MASK.TABLE.RIGHT,Y
	STA SHAPE.VALUE.SCREEN_RIGHT
	
	LDA SHAPE.ORA.BIT_VALUE
	BEQ .SET.EDGE.BYTES.COMPLETE
	
	;line is white
	LDA SHAPE.VALUE.SCREEN_RIGHT
	ORA MD.SPELL.ORA_MASK.TABLE.RIGHT,Y
	STA SHAPE.VALUE.SCREEN_RIGHT
	;**FALLS THROUGH**
.SET.EDGE.BYTES.COMPLETE

.DRAW.LINE.LOOP



			
			
;.DRAW.LINE.EDGE.LEFT
	LDY SHAPE.SBYTE.START
	LDA SHAPE.VALUE.SCREEN_LEFT
	STA (LINE.BASE.ADDR1),Y	;save byte value to video memory
	
.DRAW.LINE.INNER_LOOP
	INY ;next screen byte
	CPY SHAPE.SBYTE.END	;has last screen byte (right edge) been reached?
	BEQ .DRAW.LINE.EDGE.RIGHT
	
	LDA SHAPE.NON_EDGE.BYTE.VALUE
	STA (LINE.BASE.ADDR1),Y	;save byte value to video memory
	JMP .DRAW.LINE.INNER_LOOP ;continue loop until last byte on right edge of line is reached.
	
.DRAW.LINE.EDGE.RIGHT
	;YREG = SHAPE.SBYTE.END
	LDA SHAPE.VALUE.SCREEN_RIGHT
	STA (LINE.BASE.ADDR1),Y	;save byte value to video memory
	
.NEXT_LINE
	
		; LDA #$05
	; JSR WAIT.LOOP
	
	LDA MD.CAST.DIRECTION
	CMP #$8B			;UP ARROW
	BEQ .INCREMENT.LINE.NORTH		
	;**FALLS THROUGH**
	
.INCREMENT.LINE.SOUTH	
	INX ;move 1 line down on the video screen
	
.EXIT.TEST.SOUTH
	CPX #SCREEN.DRAW.STOP_LINE
	BEQ .INCREMENT.LEFT_SIDE ;only exit if greater than. This BEQ carves out the equal. 
	BCS .DRAW.COMPLETE 
	
	JMP .INCREMENT.LEFT_SIDE
	
.INCREMENT.LINE.NORTH
	DEX ;move 1 line up on the video screen
	
.EXIT.TEST.NORTH
	CPX #SCREEN.DRAW.START_LINE
	BCC .DRAW.COMPLETE
	;*FALLS THROUGH**
	
.INCREMENT.LEFT_SIDE
	LDA SHAPE.BIT.SCREEN_LEFT.CURRENT
	BNE .INCREMENT.BIT_LEFT
	;exit test
	LDA SHAPE.SBYTE.START
	CMP #COMBAT.SCREEN.DRAW.START_BYTE
	BEQ .INCREMENT.RIGHT_SIDE
	;increment screen start byte +1 left
	DEC SHAPE.SBYTE.START
	LDA #$06 ;reset bit counter for new screen byte
	STA SHAPE.BIT.SCREEN_LEFT.CURRENT
	JMP .INCREMENT.RIGHT_SIDE

.INCREMENT.BIT_LEFT
	DEC SHAPE.BIT.SCREEN_LEFT.CURRENT
	;**FALLS THROUGH**
	
.INCREMENT.RIGHT_SIDE
	LDA SHAPE.BIT.SCREEN_RIGHT.CURRENT
	CMP #$06
	BNE .INCREMENT.BIT_RIGHT
	;exit test
	LDA SHAPE.SBYTE.END
	CMP #COMBAT.SCREEN.DRAW.STOP_BYTE
	BEQ .INCREMENT.BITS.COMPLETE
	;increment screen end byte +1 right
	INC SHAPE.SBYTE.END
	LDA #$00 ;reset bit counter for new screen byte
	STA SHAPE.BIT.SCREEN_RIGHT.CURRENT
	JMP .INCREMENT.BITS.COMPLETE

.INCREMENT.BIT_RIGHT
	INC SHAPE.BIT.SCREEN_RIGHT.CURRENT
	;**FALLS THROUGH**	
	
.INCREMENT.BITS.COMPLETE		
	JMP .DRAW.OUTER_LOOP
	
.DRAW.COMPLETE
	JMP MD.DRAW_SHAPE.EXIT
@END

@END

;MD.CAST.EAST_WEST
@START

MD.CAST.EAST

.INIT.LINE_DRAW
;adjust draw start position to avoid erasing the spell caster
	INC SHAPE.SBYTE.CURRENT
	INC SHAPE.SBYTE.CURRENT

	LDA #$06
	STA SHAPE.BIT.CURRENT ;the bit counter runs the opposite from the north/south routines

	LDA #MD.SPELL.AND_MASK.TABLE.LEFT
	STA MD.SPELL.AND_MASK.TABLE+$0
	LDA /MD.SPELL.AND_MASK.TABLE.LEFT
	STA MD.SPELL.AND_MASK.TABLE+$1

	LDA #MD.SPELL.ORA_MASK.TABLE.LEFT
	STA MD.SPELL.ORA_MASK.TABLE+$0
	LDA /MD.SPELL.ORA_MASK.TABLE.LEFT
	STA MD.SPELL.ORA_MASK.TABLE+$1

	JMP MD.CAST.EAST_WEST
	
MD.CAST.WEST

.INIT.LINE_DRAW
;adjust draw start position to avoid erasing the spell caster
	DEC SHAPE.SBYTE.CURRENT
	
	LDA #$00
	STA SHAPE.BIT.CURRENT ;the bit counter runs the opposite from the north/south routines
	
	LDA #MD.SPELL.AND_MASK.TABLE.RIGHT
	STA MD.SPELL.AND_MASK.TABLE+$0
	LDA /MD.SPELL.AND_MASK.TABLE.RIGHT
	STA MD.SPELL.AND_MASK.TABLE+$1

	LDA #MD.SPELL.ORA_MASK.TABLE.RIGHT
	STA MD.SPELL.ORA_MASK.TABLE+$0
	LDA /MD.SPELL.ORA_MASK.TABLE.RIGHT
	STA MD.SPELL.ORA_MASK.TABLE+$1

	;**FALLS THROUGH**
	
MD.CAST.EAST_WEST

.INIT.COMMON ;applies to both east and west casting directions

;adjust draw start position to avoid erasing the spell caster	
	;move down 1/2 tile 
	LDA SHAPE.LINE.START
	CLC
	ADC #TILE.DEPTH.HALF
	STA SHAPE.LINE.START
	;LDA SHAPE.LINE.START
	SEC
	SBC #$06 ;move start line 6 hi-res lines up
	STA SHAPE.LINE.START
	;LDA SHAPE.LINE.START
		;**OPT** can the ADC and SBC be done without the LDA and STA in between?
	CLC
	ADC #$0C ;set end line =  start line + !12
	STA SHAPE.LINE.END
	;LDA SHAPE.LINE.END
	SEC
	SBC #$06
	STA SHAPE.LINE.LOWER_EDGE.START
	
	LDA #$00
	STA MD.SCREEN_EDGE.TOP.FLAG
	STA MD.EAST_WEST.ORA.FINAL_VALUE ;set to black by default. This value is overridden if the white color parameter is set

.DRAW.OUTER_LOOP
@START

.SET.EDGE.LINES

.SET.BOTTOM_LINE
		LDX SHAPE.LINE.END
	JSR MD.GET.LINE.ADDRESS  ;lookup the hi-res line address of the current line number
	
	LDY SHAPE.SBYTE.CURRENT
	LDA (LINE.BASE.ADDR1),Y	;load existing data from video memory
	LDY SHAPE.BIT.CURRENT
	AND (MD.SPELL.AND_MASK.TABLE),Y
	STA SHAPE.VALUE.BOTTOM_LINE
	
	LDA SHAPE.ORA.BIT_VALUE
	BEQ .SET.TOP_LINE
	
	;line is white
	LDA SHAPE.VALUE.BOTTOM_LINE
	ORA (MD.SPELL.ORA_MASK.TABLE),Y
	STA SHAPE.VALUE.BOTTOM_LINE
	;**FALLS THROUGH**
	
.SET.TOP_LINE
		LDX SHAPE.LINE.START
	JSR MD.GET.LINE.ADDRESS  ;lookup the hi-res line address of the current line number

	
	LDY SHAPE.SBYTE.CURRENT
	LDA (LINE.BASE.ADDR1),Y	;load existing data from video memory
	LDY SHAPE.BIT.CURRENT
	AND (MD.SPELL.AND_MASK.TABLE),Y
	STA SHAPE.VALUE.TOP_LINE
	
	LDA SHAPE.ORA.BIT_VALUE
	BEQ .SET.EDGE.LINES.COMPLETE

	
	;line is white
	LDA SHAPE.VALUE.TOP_LINE
	ORA (MD.SPELL.ORA_MASK.TABLE),Y
	STA SHAPE.VALUE.TOP_LINE
		;**FALLS THROUGH**

.SET.EDGE.LINES.COMPLETE	
		
.DRAW.LINE.LOOP

	JSR MD.DRAW.LINE.UPPER.EDGE
		
.DRAW.LINE.CENTER_LOOP
		
	INX ;next hi-res line

;SCREEN EDGE TEST
	CPX #SCREEN.DRAW.STOP_LINE ;only exit if greater than. This BEQ carves out the equal.
	BEQ .GET.ADDRESS 		;carve out equal to. 
	BCS .DRAW.LOWER_EDGE 	;if greater than only. if past screen edge branch to draw lower edge because it inits SHAPE.BIT.CURRENT for the next column.

;SCREEN EDGE TEST
	CPX #SCREEN.DRAW.START_LINE
	BCS .GET.ADDRESS ;branch if not above top screen edge
					
	;if above screen edge continue to increment hi-res line number until below top screen edge	
	JMP .DRAW.LINE.CENTER_LOOP
	
.GET.ADDRESS
	JSR MD.GET.LINE.ADDRESS ;lookup the hi-res line address of the current line number
	CPX SHAPE.LINE.LOWER_EDGE.START
	BCS .DRAW.LOWER_EDGE
		
	LDA SHAPE.NON_EDGE.BYTE.VALUE
	STA (LINE.BASE.ADDR1),Y	;save byte value to video memory

.CONTINUE.LOOP			
	JMP .DRAW.LINE.CENTER_LOOP ;continue loop until last byte on right edge of line is reached.
	
.DRAW.LOWER_EDGE	
	JSR MD.DRAW.LINE.LOWER.EDGE
		
.NEXT.PIXEL_COLUMN

		; LDA #$05
	; JSR WAIT.LOOP
	
;.INCREMENT.TOP_LINE
	LDA SHAPE.LINE.START
	CMP #SCREEN.DRAW.START_LINE
	BCC .INCREMENT.BOTTOM_LINE
	LDA SHAPE.LINE.START
	SEC
	SBC #$07
	STA SHAPE.LINE.START
	
	;**FALLS THROUGH**
.INCREMENT.BOTTOM_LINE
	LDA SHAPE.LINE.END
	CMP #SCREEN.DRAW.STOP_LINE ;only exit if greater than. This BEQ carves out the equal.
	;**OPT** Speed. Memory. Flip the operators. do a less than or equal to test.
	BEQ .EQUAL
	;BCS .INCREMENT.SCREEN_BYTE
	BCS .OVER.BOTTOM.LINE
.EQUAL
	LDA SHAPE.LINE.END
	STA SHAPE.LINE.LOWER_EDGE.START
	CLC
	ADC #$07
	STA SHAPE.LINE.END
	JMP .INCREMENT.SCREEN_BYTE
.OVER.BOTTOM.LINE		
	LDA #SCREEN.DRAW.STOP_LINE
	STA SHAPE.LINE.LOWER_EDGE.START
	;**FALLS THROUGH**
	
.INCREMENT.SCREEN_BYTE
	LDA MD.CAST.DIRECTION
	CMP #$95			;RIGHT ARROW
	BEQ .INCREMENT.EAST
	;**FALLS THROUGH**
	
.INCREMENT.WEST
	LDA SHAPE.SBYTE.CURRENT
	CMP #COMBAT.SCREEN.DRAW.START_BYTE
	BEQ .DRAW.COMPLETE
	;increment screen byte +1 left
	DEC SHAPE.SBYTE.CURRENT
	JMP .INCREMENT.BITS.COMPLETE

.INCREMENT.EAST
	;exit test
	LDA SHAPE.SBYTE.CURRENT
	CMP #COMBAT.SCREEN.DRAW.STOP_BYTE
	BEQ .DRAW.COMPLETE
	;increment screen byte +1 right
	INC SHAPE.SBYTE.CURRENT
	
.INCREMENT.BIT
	;**FALLS THROUGH**

.INCREMENT.BITS.COMPLETE		
	JMP .DRAW.OUTER_LOOP
	
.DRAW.COMPLETE
	JMP MD.DRAW_SHAPE.EXIT

					
;SUBROUTINES
MD.DRAW.LINE.UPPER.EDGE
@START
	;X-REG = SHAPE.LINE.START

	LDY SHAPE.SBYTE.CURRENT ;need here in case the screen edge check below triggers early exit because the next routine (draws the center) doesn't load the screen byte into Y-REG.
	
	;screen edge check
	;(note: if top screen edge has been reached once before, then this routine should not run again since the upper edge of the shape will be flush against the upper screen edge
	LDA MD.SCREEN_EDGE.TOP.FLAG ;$00 = not set, $01 = set
	BNE .EXIT.ALTERNATE_ENTRANCE
	
.DRAW.LOOP.UPPER_EDGE	
	LDY SHAPE.SBYTE.CURRENT
	
;SCREEN EDGE TEST
	CPX #SCREEN.DRAW.START_LINE ;only exit if greater than. This BEQ carves out the equal.
	BCS .PLOT ;branch if curren line is not above screen edge
	LDA #$01
	STA MD.SCREEN_EDGE.TOP.FLAG ;$00 = not set, $01 = set
	JMP .NEXT.LINE
	
.PLOT
	;is color parameter set to white?
	
	LDA SHAPE.ORA.BIT_VALUE
	BEQ .COLOR.CHECK.COMPLETE
	
	;line is white
	LDY SHAPE.BIT.CURRENT
	LDA (MD.SPELL.ORA_MASK.TABLE),Y
	STA MD.EAST_WEST.ORA.FINAL_VALUE
	;**FALLS THROUGH**
.COLOR.CHECK.COMPLETE	
	
	LDA (LINE.BASE.ADDR1),Y	;load existing data from video memory
	LDY SHAPE.BIT.CURRENT
	AND (MD.SPELL.AND_MASK.TABLE),Y
	ORA MD.EAST_WEST.ORA.FINAL_VALUE
	LDY SHAPE.SBYTE.CURRENT
	STA (LINE.BASE.ADDR1),Y	;save byte value to video memory

.NEXT.LINE
	INX ;next hi-res line

.GET.ADDRESS
	JSR MD.GET.LINE.ADDRESS ;lookup the hi-res line address of the current line number

;NEXT BIT

	;do exit test based on the direction of casting.
	;(note: the direction of casting is needed because the AND mask table (MD.SPELL.AND_MASK.TABLE) must be iterated in the opposite direction for east vs. west)
	LDA MD.CAST.DIRECTION
	CMP #$95			;RIGHT ARROW
	BEQ .INCREMENT.EAST
	;**FALLS THROUGH**

.INCREMENT.WEST	
	;exit test
	LDA SHAPE.BIT.CURRENT
	CMP #$06
	BEQ .EXIT
	
	INC SHAPE.BIT.CURRENT
	
	JMP .DRAW.LOOP.UPPER_EDGE

.INCREMENT.EAST
	;exit test
	LDA SHAPE.BIT.CURRENT
	CMP #$00
	BEQ .EXIT
	
	DEC SHAPE.BIT.CURRENT
	
	JMP .DRAW.LOOP.UPPER_EDGE


.EXIT
	DEX ;undo last INX because after RTS, the next routine does an INX before the draw.
	
.EXIT.ALTERNATE_ENTRANCE
	;init SHAPE.BIT.CURRENT based on the direction of casting.
	;(note: the direction of casting is needed because the AND mask table (MD.SPELL.AND_MASK.TABLE) must be iterated in the opposite direction for east vs. west)
	LDA MD.CAST.DIRECTION
	CMP #$95			;RIGHT ARROW
	BEQ .INIT.EAST
	
	LDA #$06
	STA SHAPE.BIT.CURRENT ;value should already be $06 except for when screen edge branch (at the top of this routine) triggers 
	JMP .RETURN
	
.INIT.EAST
	LDA #$00
	STA SHAPE.BIT.CURRENT ;value should already be $00 except for when screen edge branch (at the top of this routine) triggers 

.RETURN		
	RTS

	
@END

MD.DRAW.LINE.LOWER.EDGE
@START
	
.DRAW.LOOP.LOWER_EDGE
		
;SCREEN EDGE TEST
	CPX #SCREEN.DRAW.STOP_LINE ;only exit if greater than. This BEQ carves out the equal.
	BEQ .PLOT
	BCS .EXIT

.PLOT

	;is color parameter set to white?
	LDA SHAPE.ORA.BIT_VALUE
	BEQ .COLOR.CHECK.COMPLETE
	
	;line is white
	LDY SHAPE.BIT.CURRENT
	LDA (MD.SPELL.ORA_MASK.TABLE),Y
	STA MD.EAST_WEST.ORA.FINAL_VALUE
	;**FALLS THROUGH**
.COLOR.CHECK.COMPLETE	

	
	LDY SHAPE.SBYTE.CURRENT
	LDA (LINE.BASE.ADDR1),Y	;load existing data from video memory
	LDY SHAPE.BIT.CURRENT
	AND (MD.SPELL.AND_MASK.TABLE),Y
	ORA MD.EAST_WEST.ORA.FINAL_VALUE
	LDY SHAPE.SBYTE.CURRENT
	STA (LINE.BASE.ADDR1),Y	;save byte value to video memory

;.NEXT.LINE
	INX ;next hi-res line
	JSR MD.GET.LINE.ADDRESS ;lookup the hi-res line address of the current line number
	
;NEXT BIT

	;do exit test based on the direction of casting.
	;(note: the direction of casting is needed because the AND mask table (MD.SPELL.AND_MASK.TABLE) must be iterated in the opposite direction for east vs. west)
	LDA MD.CAST.DIRECTION
	CMP #$95			;RIGHT ARROW
	BEQ .INCREMENT.EAST
	;**FALLS THROUGH**

.INCREMENT.WEST	
	;exit test
	LDA SHAPE.BIT.CURRENT
	CMP #$00
	BEQ .EXIT
	
	DEC SHAPE.BIT.CURRENT
	
	JMP .DRAW.LOOP.LOWER_EDGE

.INCREMENT.EAST
	;exit test
	LDA SHAPE.BIT.CURRENT
	CMP #$06
	BEQ .EXIT
	
	INC SHAPE.BIT.CURRENT
	
	JMP .DRAW.LOOP.LOWER_EDGE

.EXIT

	;init SHAPE.BIT.CURRENT based on the direction of casting.
	;(note: the direction of casting is needed because the AND mask table (MD.SPELL.AND_MASK.TABLE) must be iterated in the opposite direction for east vs. west)
	LDA MD.CAST.DIRECTION
	CMP #$95			;RIGHT ARROW
	BEQ .INIT.EAST
	
	LDA #$00
	STA SHAPE.BIT.CURRENT ;value should already be $06 except for when screen edge branch (at the top of this routine) triggers 
	JMP .RETURN
	
.INIT.EAST
	LDA #$06
	STA SHAPE.BIT.CURRENT ;value should already be $00 except for when screen edge branch (at the top of this routine) triggers 

.RETURN
	RTS
@END
	

@END
@END
	
;SUBROUTINES
MD.GET.LINE.ADDRESS ;(FOREGROUND PAGE)
@START
;PARAMETERS: X-REG (hi-res line number)
;ENTRANCE: direct
;RETURN: LINE.BASE.ADDR1(2)

;==========INLINE CODE GET.LINE.ADDRESS1==================	
	; LDA PAGE.FOREGROUND.OVERRIDE	;CHECK TO SEE IF CALLING ROUTINE WANTS ERASE DONE TO FOREGROUND INSTEAD OF BACKGROUND (DEFAULT)
	; CMP #$01
	; BEQ .FOREGROUND_OVERRIDE
	
	; LDA PAGE.BACKGROUND		
	; CMP #$02
	; BEQ .LOOKUP.PAGE2
	; JMP .LOOKUP.PAGE1
	
; .FOREGROUND_OVERRIDE
	LDA PAGE.FOREGROUND		;SPECIFY PAGE TO GET ADDRESS FOR (SHOULD BE BACKGROUND PAGE)
	CMP #$02
	BEQ .LOOKUP.PAGE2
	

.LOOKUP.PAGE1	
	LDA LINE.HO.P1,X		;GET LINE ADDRESS
	STA LINE.BASE.ADDR1+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR1
	JMP .GETLINE_COMPLETE

	
.LOOKUP.PAGE2
	LDA LINE.HO.P2,X		;GET LINE ADDRESS
	STA LINE.BASE.ADDR1+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR1
.GETLINE_COMPLETE
	RTS
;========================================================
@END
	
MD.IDENTIFY.TARGETS
@START
;PARAMETERS: COMBAT.S_ENTITY.FOUND.GMAP.X, COMBAT.S_ENTITY.FOUND.GMAP.Y

;COMBAT.S_ENTITY.DISTANCE.DB datagram
;field 0 (distance)
;Byte 0		Byte 1			Byte 2					
;distance	GMAP.X (target)	GMAP.Y (target)	


;***temp
		LDX #$00
.INIT.LOOP
		LDA #$00
		STA COMBAT.S_ENTITY.DISTANCE.DB,X
		INX
		CPX #$72
		BNE .INIT.LOOP
		
.INIT.VARIABLES
	LDX #$00 ;COMBAT.S_ENTITY.DISTANCE.DB index
	LDY #$00 ;screen array index
	STY COMBAT.S_ENTITY.DISTANCE.LENGTH+$1

MD.SCREEN.ARRAY.LOOP ;main loop	

	LDA SCREEN.MO_SPRITE.DATA,Y ;load S_ENTITY map object index, if one is present at this screen location
	CMP #$FF ;is there a map object at this screen array location?
	BNE MD.S_ENTITY.FOUND
	;**FALLS THROUGH**	

MD.INCREMENT.SCREEN_ARRAY.INDEX	
	INY ;increment screen array index
	CPY #SCREEN.ARRAY.LAST_ELEMENT2 ;at end of screen array?
	BNE MD.SCREEN.ARRAY.LOOP
	;**FALLS THROUGH**	


.WRITE.DATABASE.LENGTH

		; LDA #$AB
		; ; LDX #COMBAT.S_ENTITY.DISTANCE.DB
		; ; LDY /COMBAT.S_ENTITY.DISTANCE.DB
		; JSR PREP.BRK
		; BRK

	STX COMBAT.S_ENTITY.DISTANCE.LENGTH+$0 ;save index as length LO byte. X is init with 0 but was incremented after the last record write, so X = byte qty. 
	;HO byte was init to #$00. The array will always be > $100 bytes in length, so it can stay at init value. 
	
.EXIT
	
	RTS	
	
MD.S_ENTITY.FOUND ;an S_ENTITY was found at at the screen location in the current iteration of the loop

;LOAD S_ENTITY GMAP	

		LDA #$08 ;read $8 bytes    ;**OPT** Memory. I think this routine could just default to an 8byte read and note that the last 4 bytes are garbage for S_ENTITIES with 4 byte records. The code that uses the return data already knows that. 
		;Y-REG (screen array index)
	JSR READ.MAP_OBJECT.ARRAY
		;RETURN VALUE: MAP_OBJECT.RECORD.READ(8), MAP_OBJECT.ARRAY.S_ENTITY.TYPE, MAP_OBJECT.ARRAY.MO_INDEX
		
		LDA MAP_OBJECT.RECORD.READ+$0
		STA COMBAT.S_ENTITY.FOUND.GMAP.X
		LDA MAP_OBJECT.RECORD.READ+$1
		STA COMBAT.S_ENTITY.FOUND.GMAP.Y

;IS S_ENTITY A VALID TARGET?	

	;is the current S_ENTITY the spellcaster?
	LDA COMBAT.S_ENTITY.FOUND.GMAP.X
	CMP COMBAT.SPELL_CASTER.GMAP.X 
	BNE .SPELLCASTER.CHECK.COMPLETE
	LDA COMBAT.S_ENTITY.FOUND.GMAP.Y
	CMP COMBAT.SPELL_CASTER.GMAP.Y
	BEQ	.EXIT_STEP ;skip this S_ENTITY, do not increment database
.SPELLCASTER.CHECK.COMPLETE
				
		;is S_ENTITY a tile of a multi-tile mob other than the index tile (upper left tile)?
		;(the upper left tile is the only tile of a multi-tile mob that is a valid character sheet index)
		;Y-REG: search loop index
	JSR COMBAT.TARGET.SEARCH.MTT.CHECK
			;RETURN VALUE: ACC = $00 ($00 =  MTT index tile found | $01 = MTT not found at this screen location | $02 = MTT tile found other than index tile)
		CMP #$02 ;is S_ENTITY found a non-index tile of an MTT S_ENTTY? (i.e. a tile other than the upper left of the MTT)
		BEQ	.EXIT_STEP	;skip this S_ENTITY
		
.VALIDATE.TARGET.CHECK.COMPLETE	
	
		;set parameters
		LDA COMBAT.S_ENTITY.FOUND.GMAP.X
		STA PARM2.GMAP.X

		LDA COMBAT.SPELL_CASTER.GMAP.X ;load target GMAP.X
		STA PARM1.GMAP.X

		LDA COMBAT.S_ENTITY.FOUND.GMAP.Y
		STA PARM2.GMAP.Y	

		LDA COMBAT.SPELL_CASTER.GMAP.Y ;load target GMAP.Y
		STA PARM1.GMAP.Y
	JSR CALCULATE.DISTANCE		
		;ACC = DISTANCE
		;RETURN MAP_OBJECTS.X_ADJ = X-AXIS differential
		;RETURN MAP_OBJECTS.Y_ADJ = Y-AXIS differential
		STA SAVED.ACC.LOCAL ;save distance
	
	;use direction code to apply rules for target validation
	LDA MD.CAST.DIRECTION
	CMP #$8B			;UP ARROW
	BEQ .VALIDATE.NORTH
	CMP #$8A			;DOWN ARROW
	BEQ .VALIDATE.SOUTH
	CMP #$95			;RIGHT ARROW
	BEQ .VALIDATE.EAST
	CMP #$88			;LEFT ARROW
	BEQ	.VALIDATE.WEST

.ERROR.INVALID.DIRECTION.CODE			
;MD.IDENTIFY.TARGETS reports invalid direction code in MD.CAST.DIRECTION

	JSR PREP.BRK
	BRK

.EXIT_STEP
	JMP .EXIT
	
.VALIDATE.NORTH ;casting direction
	;found S_ENTITY must be above the spellcaster GMAP -AXIS
	LDA COMBAT.S_ENTITY.FOUND.GMAP.Y
	CMP COMBAT.SPELL_CASTER.GMAP.Y
	BCS .EXIT ;S_SENTITY is not valid target

	JMP .NORTH_SOUTH.COMMON
	
.VALIDATE.SOUTH ;casting direction
	;found S_ENTITY must be below the spellcaster GMAP -AXIS
	LDA COMBAT.S_ENTITY.FOUND.GMAP.Y
	CMP COMBAT.SPELL_CASTER.GMAP.Y
	BCC .EXIT ;S_SENTITY is not valid target

	;**FALLS THROUGH**
	
.NORTH_SOUTH.COMMON	
	;found S_ENTITY run must be <= rise to be a valid target
	;run > rise = not valid target
	LDA MAP_OBJECTS.X_ADJ 
	CMP MAP_OBJECTS.Y_ADJ
	;don't increment index if run is greater than rise
	BEQ .WRITE.DATABASE.RECORD ;;S_SENTITY is valid target. if run = rise
	BCS .EXIT ;S_SENTITY is not valid target. implicit greater than test (if run is greater than rise)
	JMP .WRITE.DATABASE.RECORD
		;**OPT** setup as less than or equal to
		
.VALIDATE.EAST ;casting direction
	;found S_ENTITY must be to the left of the spellcaster GMAP X-AXIS
	LDA COMBAT.S_ENTITY.FOUND.GMAP.X
	CMP COMBAT.SPELL_CASTER.GMAP.X
	BCC .EXIT ;S_SENTITY is not valid target

	JMP .EAST_WEST.COMMON
	
.VALIDATE.WEST ;casting direction
	
	;found S_ENTITY must be to the right of the spellcaster GMAP X-AXIS
	LDA COMBAT.S_ENTITY.FOUND.GMAP.X
	CMP COMBAT.SPELL_CASTER.GMAP.X
	BCS .EXIT ;S_SENTITY is not valid target

	;**FALLS THROUGH**
	
.EAST_WEST.COMMON
	;found S_ENTITY rise must be < run to be a valid target
	;rise >= run = not valid target
	LDA MAP_OBJECTS.Y_ADJ
	CMP MAP_OBJECTS.X_ADJ 
	;don't increment index if run is greater than rise
	;BEQ .WRITE.DATABASE.RECORD ;;S_SENTITY is valid target. if run = rise
	BCS .EXIT ;S_SENTITY is not valid target. implicit greater than test (if run is greater than rise)

	;**FALLS THROUGH**	

.WRITE.DATABASE.RECORD

	LDA SAVED.ACC.LOCAL ;restore distance

	;ACC = DISTANCE
	STA COMBAT.S_ENTITY.DISTANCE.DB+$0,X
	LDA COMBAT.S_ENTITY.FOUND.GMAP.X
	STA COMBAT.S_ENTITY.DISTANCE.DB+$1,X ;this is a convenient location to save the target X-axis to the distance database

	LDA COMBAT.S_ENTITY.FOUND.GMAP.Y
	STA COMBAT.S_ENTITY.DISTANCE.DB+$2,X ;this is a convenient location to save the target Y-axis to the distance database		

	;**FALLS THROUGH** (if distance is less than radius)

.RECORD.TARGET.HIT
;SAVE REGISTERS
	TXA
	PHA
	
	;YREG = screen index
	TYA
	LDX COMBAT.TARGET_HIT.DB.INDEX
	STA COMBAT.TARGET_HIT.DB,X
	;write stop value
	;(note: each time a new target hit is recorded the stop value is advanced 1 byte in the array)
	LDA #COMBAT.TARGET_HIT.DB.STOP_VALUE
	STA COMBAT.TARGET_HIT.DB+$1,X

	INC COMBAT.TARGET_HIT.DB.INDEX	;next target_hit record
	
;RESTORE REGISTERS	
	PLA
	TAX
	
	;**FALLS THROUGH** (if distance is less than radius)
		
		
.INCREMENT.DATABASE.INDEX
	;increment COMBAT.S_ENTITY.DISTANCE.DB index
	TXA
	CLC
	ADC #$03 ;next database record
	TAX
	CMP #$72 ;this is based on up to 16 mobs, 16 specials, and 6 player characters, using 3 byte records.
			 ;in the distance database.
	BCS .EXIT.SUBROUTINE
	;**FALLS THROUGH**
	
.EXIT
	
	JMP MD.INCREMENT.SCREEN_ARRAY.INDEX

.EXIT.SUBROUTINE
	LDY #SCREEN.ARRAY.LAST_ELEMENT2 ;force exit of screen array loop by setting index to the exit value
	JMP MD.INCREMENT.SCREEN_ARRAY.INDEX	


;SUBROUTINES
;.TARGET.SEARCH.MTT.CHECK
@START
; ;PARAMETERS: MAP_OBJECT.RECORD.READ(8) = data for S_ENTITY found at current screen location
; ;ENTRANCE: various identify target routines
; ;RETURN: ACC = $00 (MTT not found at this screen location or MTT index tile found | $01 = MTT tile found other than index tile)

; ;SAVE REGISTERS
	; ; TXA
	; ; PHA
	; TYA
	; PHA

; ;INIT
	
	; ;read map object data at screen location of S_ENTITY found
		; LDA #$08 ;read $8 bytes    ;**OPT** Memory. I think this routine could just default to an 8byte read and note that the last 4 bytes are garbage for S_ENTITIES with 4 byte records. The code that uses the return data already knows that. 
		; ;Y-REG (screen array index)
	; JSR READ.MAP_OBJECT.ARRAY
		; ;RETURN VALUE: MAP_OBJECT.RECORD.READ(8), MAP_OBJECT.ARRAY.S_ENTITY.TYPE, MAP_OBJECT.ARRAY.MO_INDEX
	
; ;IS MOB MULTI-TILE (MTT)?	
	; LDA #$08 ;the AND mask value needed for the BIT operation below. 
	; BIT MAP_OBJECT.RECORD.READ+$3 ;test bit3 of MAP_OBJECT.RECORD.READ+$03
	; BEQ .MTT_CHECK.MTT_NOT_FOUND ;branch if bit3 is not set (mob is not multi-tile)

	
; ;IS CURRENT SCREEN LOCATION THE MTT INDEX TILE?
; ;(index tile = upper left tile)

	; ;screen edge check
	; LDA MAP_OBJECT.RECORD.READ+$0 ;load X-AXIS
	; CMP #COMBAT.SCREEN.COLUMN.LAST
	; BEQ .MTT_CHECK.MTT_INDEX_TILE_NOT_FOUND

	; LDA MAP_OBJECT.RECORD.READ+$1 ;load Y-AXIS
	; CMP #COMBAT.SCREEN.ROW.LAST
	; BEQ .MTT_CHECK.MTT_INDEX_TILE_NOT_FOUND

	
	; ;save data for MTT index tile
	; LDA	MAP_OBJECT.ARRAY.S_ENTITY.TYPE
	; STA .MTT_CHECK.S_ENTITY.TYPE
	; LDA	MAP_OBJECT.ARRAY.MO_INDEX
	; STA .MTT_CHECK.MO_INDEX	
		


; ;SCREEN TILE LOCATION +1 right

	; INY ;screen location +1 right	
	
	; ;get MO data for screen location +1 right
		; LDA #$00 ;init value. defaults to found, and then the value is only change if the S_ENTITY is not found. 
		; STA MAP_OBJECT.ARRAY.S_ENTITY.NOT_FOUND
		; LDA #$FF
		; STA MAP_OBJECT.ARRAY.ERROR_TRAP_OVERRIDE		
		; LDA #$08 ;read $8 bytes    ;**OPT** Memory. I think this routine could just default to an 8byte read and note that the last 4 bytes are garbage for S_ENTITIES with 4 byte records. The code that uses the return data already knows that. 
		; ;Y-REG (screen array index)
	; JSR READ.MAP_OBJECT.ARRAY
		; ;RETURN VALUE: MAP_OBJECT.RECORD.READ(8), MAP_OBJECT.ARRAY.S_ENTITY.TYPE, MAP_OBJECT.ARRAY.MO_INDEX, MAP_OBJECT.ARRAY.S_ENTITY.NOT_FOUND
		
		; ;was S_ENTITY found at this screen tile location?
		; ;(if no, then an MTT index tile was not found at original screen location)
		; LDA MAP_OBJECT.ARRAY.S_ENTITY.NOT_FOUND ;($00 = S_ENTITY found | $01 = S_ENTITY not found)
		; BNE .MTT_CHECK.MTT_INDEX_TILE_NOT_FOUND ;branch if no

	; ;does tile contain an S_ENTITY that is part of the original MTT S_ENTITY found?
	; ;(if no, then the MTT tile found is not the index tile)
	; LDA MAP_OBJECT.ARRAY.S_ENTITY.TYPE
	; CMP .MTT_CHECK.S_ENTITY.TYPE ;S_ENTITY type found at original screen location
	; BNE .MTT_CHECK.MTT_INDEX_TILE_NOT_FOUND ;branch if no
	; LDA MAP_OBJECT.ARRAY.MO_INDEX
	; CMP .MTT_CHECK.MO_INDEX	;MO index found at original screen location
	; BNE .MTT_CHECK.MTT_INDEX_TILE_NOT_FOUND ;branch if no
	
					; ; lda #$ab
					; ; LDx MAP_OBJECT.ARRAY.S_ENTITY.NOT_FOUND ;($00 = S_ENTITY found | $01 = S_ENTITY not found)
					; ; jsr prep.brk
					; ; brk
					
; ;SCREEN TILE LOCATION +1 down

	; ;change screen tile location
	; DEY ;return to original screen location of found S_ENTITY
	; TYA
	; CLC
	; ADC #SCREEN.ARRAY.OFFSET ;move 1 tile up from screen location
	; TAY
	
	; ;get MO data for screen location +1 up
		; LDA #$00 ;init value. defaults to found, and then the value is only change if the S_ENTITY is not found. 
		; STA MAP_OBJECT.ARRAY.S_ENTITY.NOT_FOUND
		; LDA #$FF
		; STA MAP_OBJECT.ARRAY.ERROR_TRAP_OVERRIDE		
		; LDA #$08 ;read $8 bytes    ;**OPT** Memory. I think this routine could just default to an 8byte read and note that the last 4 bytes are garbage for S_ENTITIES with 4 byte records. The code that uses the return data already knows that. 
		; ;Y-REG (screen array index)
	; JSR READ.MAP_OBJECT.ARRAY
		; ;RETURN VALUE: MAP_OBJECT.RECORD.READ(8), MAP_OBJECT.ARRAY.S_ENTITY.TYPE, MAP_OBJECT.ARRAY.MO_INDEX		

					
		; ;was S_ENTITY found at this screen tile location?
		; ;(if no, then an MTT index tile was not found at original screen location)
		; LDA MAP_OBJECT.ARRAY.S_ENTITY.NOT_FOUND ;($00 = S_ENTITY found | $01 = S_ENTITY not found)
		; BNE .MTT_CHECK.MTT_INDEX_TILE_NOT_FOUND ;branch if no

	; ;does tile contain an S_ENTITY that is part of the original MTT S_ENTITY found?
	; ;(if yes, then the MTT tile found is the index tile)
	; LDA MAP_OBJECT.ARRAY.S_ENTITY.TYPE
	; CMP .MTT_CHECK.S_ENTITY.TYPE ;S_ENTITY type found at original screen location
	; BNE .MTT_CHECK.MTT_INDEX_TILE_NOT_FOUND ;branch if no
	; LDA MAP_OBJECT.ARRAY.MO_INDEX
	; CMP .MTT_CHECK.MO_INDEX ;MO index found at original screen location
	; BNE .MTT_CHECK.MTT_INDEX_TILE_NOT_FOUND
	; ;index tile found
	; JMP .MTT_CHECK.MTT_INDEX_TILE_FOUND
	
; .MTT_CHECK.MTT_INDEX_TILE_NOT_FOUND
	; LDA #$01    ;$00 (MTT not found at this screen location or MTT index tile found | $01 = MTT tile found other than index tile)
	; STA .MTT_CHECK.RETURN_VALUE	
	; JMP .TARGET.SEARCH.MTT.CHECK.EXIT
	
; .MTT_CHECK.MTT_INDEX_TILE_FOUND	
; .MTT_CHECK.MTT_NOT_FOUND	
	; LDA #$00    ;$00 (MTT not found at this screen location or MTT index tile found | $01 = MTT tile found other than index tile)
	; STA .MTT_CHECK.RETURN_VALUE
	
	; ;**FALLS THROUGH**
	
; .TARGET.SEARCH.MTT.CHECK.EXIT

; ;RESTORE REGISTERS
	; PLA
	; TAY
	; ; PLA
	; ; TAX
	
	; LDA .MTT_CHECK.RETURN_VALUE ;load return value. ;$00 (MTT not found at this screen location or MTT index tile found | $01 = MTT tile found other than index tile)
	
	; RTS
	
; ;--------

; ;LOCAL VARIABLES
; @START
; ; .EXCLUSION_LIST.RECORD.SIZE		.EQ $5 ;#CONSTANT. byte $00 = MTT S_ENTITY type code, byte $01 = MTT MO index, $02-$04 = sindex of 3 exclusion tiles (upper right, lower left, and lower right tiles of the MTT)
; ; .EXCLUSION_LIST.ARRAY.SIZE		.EQ $50

; ; .TARGET.SEARCH.MTT_EXCLUSION_LIST	.BS .EXCLUSION_LIST.ARRAY.SIZE ;supports !16 MTT mobs

; ;TARGET.SEARCH.MTT_EXCLUSION_LIST.INDEX	.BS $1

; .MTT_CHECK.S_ENTITY.TYPE	.BS $1
; .MTT_CHECK.MO_INDEX 		.BS $1	
; .MTT_CHECK.RETURN_VALUE		.BS $1

@END
	
@END

;.TARGET.SEARCH.MTT.CHECK ;**uses the exclusion list method. Not finished, on first run all MTT mob tiles were still showing up in target db
@START
; ;SAVE REGISTERS
	; TXA
	; PHA
	; TYA
	; PHA

; ;INIT
	
; ;IS MOB MULTI-TILE (MTT)?	
	; LDA #$08 ;the AND mask value needed for the BIT operation below. 
	; BIT MAP_OBJECT.RECORD.READ+$3 ;test bit3 of MAP_OBJECT.RECORD.READ+$03
	; BEQ .TARGET.SEARCH.MTT.CHECK.EXIT ;branch if bit3 is not set (mob is multi-tile)

; ;HAS THIS MTT OBJECT ALREADY BEEN FOUND ON THE SCREEN?
	; LDX #$00
; .ALREADY.FOUND.MTT.LOOP
; ;(if the S_ENTITY type code and MO index of the MTT found is in the exclusion list database
; ;then it has already been found and no additional exclusions need to be recorded for it)
; ;
; ;.EXCLUSION_LIST.RECORD.SIZE
; ; datagram: byte $00 = MTT S_ENTITY type code, byte $01 = MTT MO index, $02-$04 = sindex of 3 exclusion tiles (upper right, lower left, and lower right tiles of the MTT)


	; LDA .TARGET.SEARCH.MTT_EXCLUSION_LIST,X
	; CMP MAP_OBJECT.ARRAY.S_ENTITY.TYPE
	; BNE .INCREMENT.INDEX
; .MATCH.S_ENTITY.TYPE
	; INX ;advance to byte $01 in exclusion record
	; LDA .TARGET.SEARCH.MTT_EXCLUSION_LIST,X
	; CMP MAP_OBJECT.ARRAY.MO_INDEX
	; BNE .INCREMENT.INDEX
	; JMP .TARGET.SEARCH.MTT.CHECK.EXIT.ALT ;MTT S_ENTITY has already been found on screen. no additional exclusions need to be recorded. 

; .INCREMENT.INDEX
	; TXA
	; CLC
	; ADC #.EXCLUSION_LIST.RECORD.SIZE
	; TAX
	; CPX #.EXCLUSION_LIST.ARRAY.SIZE
	; BNE .ALREADY.FOUND.MTT.LOOP
; .ALREADY.FOUND.MTT.LOOP.DONE	
	; ;if loop ends on it's own, then the MTT found on screen is not already in the exclusion list. 

	; ;**FALLS THROUGH**
	
; .TARGET.SEARCH.RECORD.MTT.EXCLUSION	
	; LDX TARGET.SEARCH.MTT_EXCLUSION_LIST.INDEX ;load index to next open record

; ;RECORD EXCLUSION RECORD KEY (byte $0-$1)	
	; LDA MAP_OBJECT.ARRAY.S_ENTITY.TYPE
	; STA .TARGET.SEARCH.MTT_EXCLUSION_LIST,X
	; INX ;advance to byte $01 in exclusion record
	; LDA MAP_OBJECT.ARRAY.MO_INDEX
	; STA .TARGET.SEARCH.MTT_EXCLUSION_LIST,X
	; INX ;next byte in exclusion record
	

; ;RECORD EXCLUSION: upper right tile
	; INY
	; TYA
	; STA .TARGET.SEARCH.MTT_EXCLUSION_LIST,X
	; INX ;next byte in exclusion record
	
; ;RECORD EXCLUSION: lower right tile
	; TYA
	; SEC
	; SBC #SCREEN.ARRAY.OFFSET		
	; STA .TARGET.SEARCH.MTT_EXCLUSION_LIST,X
	; TAY
	; INX ;next byte in exclusion record

; ;RECORD EXCLUSION: lower left tile
	; DEY
	; TYA
	; STA .TARGET.SEARCH.MTT_EXCLUSION_LIST,X
	; INX ;next byte in exclusion record
	
; .TARGET.SEARCH.MTT.CHECK.EXIT

; ;SAVE EXCLUSION LIST INDEX
	; STX TARGET.SEARCH.MTT_EXCLUSION_LIST.INDEX

; .TARGET.SEARCH.MTT.CHECK.EXIT.ALT
; ;RESTORE REGISTERS
	; PLA
	; TAY
	; PLA
	; TAX
	
	
	; RTS

;LOCAL VARIABLES
@START
; .EXCLUSION_LIST.RECORD.SIZE		.EQ $5 ;#CONSTANT. byte $00 = MTT S_ENTITY type code, byte $01 = MTT MO index, $02-$04 = sindex of 3 exclusion tiles (upper right, lower left, and lower right tiles of the MTT)
; .EXCLUSION_LIST.ARRAY.SIZE		.EQ $50

; .TARGET.SEARCH.MTT_EXCLUSION_LIST	.BS .EXCLUSION_LIST.ARRAY.SIZE ;supports !16 MTT mobs


; TARGET.SEARCH.MTT_EXCLUSION_LIST.INDEX	.BS $1


@END
	
@END

@END

	
MD.DRAW_SHAPE.EXIT ; return to routine that called MD.SPELL.DRAW_SHAPE

.SET.SPELL_DAMAGE
	LDA #$00 ;set kill flag = success
    STA COMBAT.STATS.SPELL.KILL_FLAG_MASTER ;death magic attack: ($00 = succeeded | $01 = not attempted | $03 = failed). If set then all targets hit by the spell are automatically killed. Sometimes saving thows may be available. 

.SET.CRITICAL_HIT.FLAGS
@START
;(engaged s_entity screen index)

		;**warning: active player character sheet will be loaded on exit
		LDY COMBAT.ATTACKER.SINDEX
		LDA #COMBAT.STATS.CRTL_HIT.NOT_ENGAGED_VALUE ;set value to update.
.UPDATE.CHR_SHEET.ENGAGED.INDEX ;**OPT** Memory. This routine was written as as subroutine, but is being used inline here and it could be made more efficient. 
;PARAMETERS: YREG = screen index of attacker, ACC = value to write to CHR_SHEET.PC_MOB.ENGAGED.SINDEX
;ENTRANCE: direct
;RETURN: saved attacker character sheet with updated CHR_SHEET.PC_MOB.ENGAGED.SINDEX field

.SAVE.PARAMETERS.ENAGED
	;ACC = value to write to CHR_SHEET.PC_MOB.ENGAGED.SINDEX
	STA COMBAT.ENGAGED_SINDEX.UPDATE.PARM
	
	;read attacker (MOB) character sheet
		
		;YREG = COMBAT.ATTACKER.SINDEX
		LDA #$00 ;set read mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet

			
	;set the engaged field to the updated value
	;(expected to be the screen index of the defender or $FF to indicate no engagement)
	LDA COMBAT.ENGAGED_SINDEX.UPDATE.PARM
	STA CHR_SHEET.PC_MOB.ENGAGED.SINDEX

			
		;YREG = COMBAT.ATTACKER.SINDEX
		LDA #$01 ;set write mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet

@END
	
;RESTORE REGISTERS
	PLA
	TAX
				
	RTS

;LOOKUP TABLES

MD.SPELL.AND_MASK.TABLE.LEFT .HS 80.81.83.87.8F.9F.BF
MD.SPELL.ORA_MASK.TABLE.LEFT .HS 7F.7E.7C.78.70.60.40

MD.SPELL.AND_MASK.TABLE.RIGHT	.HS FE.FC.F8.F0.E0.C0.80
MD.SPELL.ORA_MASK.TABLE.RIGHT	.HS 01.03.07.0F.1F.3F.7F


@END

	.EP ;end patch mode
;--------
GE.SPELL.MASS_DEATH.END ;marks the end address of the code block for this spell
@END

GE.SPELL.SMITE.START ;marks the start address of the code block for this spell
;--------
@START
	.PH SPELL.FILE.CODE_BLOCK.BUFFER.START ;start patch 

GE.SPELL.SMITE
@START
;PARAMETERS: FP.SOURCE.GMAP.X, FP.SOURCE.GMAP.Y, FP.TARGET.GMAP.X, FP.TARGET.GMAP.Y
;ENTRANCE: DIRECT		
;RETURN: targets hit (COMBAT.TARGET_HIT.DB)

;=====================SUBROUTINE DOCUMENTATION====================================
;
;-target hits
;COMBAT.TARGET_HIT.DB is populated by a call to .TARGET.HIT.CHECK.
;
;
;=================================================================================

;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA
	
.INIT
	LDA #$01
	STA SPELL.SMITE.ACTIVE.FLAG	 ;($00 = SMITE not active | >=$01 = SMITE active)


.SELECT.SPELL.ROUTINE
	LDA COMBAT.SPELL_CODE.CAST
	CMP #SPELL_CODE.SMITE_SWORD
	BEQ .SMITE_SWORD
	CMP #SPELL_CODE.MASS_SMITE_SWORD
	BEQ .MASS_SMITE_SWORD
	CMP #SPELL_CODE.SMITE_AXE
	BEQ .SMITE_AXE
	CMP #SPELL_CODE.EDS_AXE	
	BEQ .EDS_AXE
	CMP #SPELL_CODE.SMITE_BOULDER
	BEQ .SMITE_BOULDER
	CMP #SPELL_CODE.MASS_SMITE_BOULDER
	BEQ .MASS_SMITE_BOULDER
	
.ERROR.SPELL_CODE.NOT_FOUND
;.SELECT.SPELL.ROUTINE (GE.SPELL.SMITE) reports an unexpected 
;spell code value in COMBAT.SPELL_CODE.CAST

	JSR PREP.BRK
	BRK

.SMITE_SWORD
	
.MASS_SMITE_SWORD
	;connect shape table
	LDA #SHAPE_TABLE.SWORD.START
	STA SMITE.SHAPE_TABLE.POINTER+$0
	
	LDA /SHAPE_TABLE.SWORD.START
	STA SMITE.SHAPE_TABLE.POINTER+$1
	JMP .SELECT.SPELL.ROUTINE.DONE
	
.SMITE_AXE
.EDS_AXE	
	;connect shape table
	LDA #SHAPE_TABLE.AXE.START
	STA SMITE.SHAPE_TABLE.POINTER+$0
	
	LDA /SHAPE_TABLE.AXE.START
	STA SMITE.SHAPE_TABLE.POINTER+$1
	JMP .SELECT.SPELL.ROUTINE.DONE
	

.SMITE_BOULDER	
.MASS_SMITE_BOULDER
	;connect shape table
	LDA #SHAPE_TABLE.BOULDER.START
	STA SMITE.SHAPE_TABLE.POINTER+$0
	
	LDA /SHAPE_TABLE.BOULDER.START
	STA SMITE.SHAPE_TABLE.POINTER+$1

	;**FALLS THROUGH**
	
.SELECT.SPELL.ROUTINE.DONE
		

		
.ACQUIRE.TARGETS
@START
;(only acquire targets if this particular smite spell is multi-target. Single target 
;smite spells have their targets selected before entrance to GE.SPELL.SMITE)
;
;Ed's Axe of Anguish: all enemy S_ENTITIES are targets hit
;
	LDA SPELL.SMITE.MULTI_TARGET.FLAG	;($00 = single target | $01 = multi-target)
	BEQ .UPDATE.TARGET_HIT.DATABASE	

	;IF PC'S TURN, THEN SET SPRITE.RECORD TO ACTIVE PC DATA
	;(this is because COMBAT.ACQUIRE.TARGET assumes that the attacker's data is loaded into SPRITE.RECORD since it was originally
	;desgined to be called from map_object_management.ASM)
	;PC/MOB TURN BRANCH
	LDA COMBAT.TURN_STATUS   ;($00 = PCs turn. $01 = Special(s) S_ENTITY (example Wyvern Ally, Summoned Demon), $02 = MOBs turn)
	BNE .TURN.CHECK.DONE ;if not PCs turn, then branch

	JSR .SET.SPRITE.RECORD
.TURN.CHECK.DONE
		
		LDA #$01
		STA COMBAT.ACQUIRE.RETURN_VALUE.PARM ;($00 = standard return values | >=$01 = COMBAT.TARGET_HIT.DB(x))
	JSR COMBAT.ACQUIRE.TARGET ;acquires multiple targets
	JMP .ACQUIRE.TARGETS.DONE

.UPDATE.TARGET_HIT.DATABASE	;adds single target
;get screen location of prospective GMAP.X/Y of target
		LDA FP.TARGET.GMAP.X ;load new special S_ENTITYY GMAP.X
		STA PARM.GMAP.X
		LDA FP.TARGET.GMAP.Y ;load new special S_ENTITY GMAP.Y
		STA PARM.GMAP.Y				
	JSR CONVERT.COLUMN_ROW.TO.SINDEX
		LDY RETURN.SCREEN_ARRAY_INDEX ;used below
	JSR .TARGET.HIT.CHECK

.ACQUIRE.TARGETS.DONE
	
@END
	
	
	;**FALLS THROUGH**


.SET.CRITICAL_HIT.FLAGS
@START
;(engaged s_entity screen index)

		;**warning: attacker character sheet will be loaded on exit
		LDY COMBAT.ATTACKER.SINDEX
		LDA COMBAT.TARGET_HIT.DB+$0 ;set value to update (sindex of the target)	
.UPDATE.CHR_SHEET.ENGAGED.INDEX ;**OPT** Memory. This routine was written as as subroutine, but is being used inline here and it could be made more efficient. 
;PARAMETERS: YREG = screen index of attacker, ACC = value to write to CHR_SHEET.PC_MOB.ENGAGED.SINDEX
;ENTRANCE: direct
;RETURN: saved attacker character sheet with updated CHR_SHEET.PC_MOB.ENGAGED.SINDEX field

.SAVE.PARAMETERS.ENAGED
	;ACC = value to write to CHR_SHEET.PC_MOB.ENGAGED.SINDEX
	STA COMBAT.ENGAGED_SINDEX.UPDATE.PARM
	
	;read attacker (MOB) character sheet
		
		;YREG = COMBAT.ATTACKER.SINDEX
		LDA #$00 ;set read mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet

			
	;set the engaged field to the updated value
	;(expected to be the screen index of the defender or $FF to indicate no engagement)
	LDA COMBAT.ENGAGED_SINDEX.UPDATE.PARM
	STA CHR_SHEET.PC_MOB.ENGAGED.SINDEX

			
		;YREG = COMBAT.ATTACKER.SINDEX
		LDA #$01 ;set write mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet

@END

		
.EXIT


		
;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX
	
	RTS
@END

.SET.SPRITE.RECORD
@START

	;SET SPRITE.RECORD TO ACTIVE PC
	LDX COMBAT.PC.ACTIVE.RECORD
	
	LDA COMBAT.MAP_OBJECTS.PC+$0,X ;load active player GMAP.X
	STA SPRITE.RECORD+$8
	LDA COMBAT.MAP_OBJECTS.PC+$1,X ;load active player GMAP.Y
	STA SPRITE.RECORD+$9
		
		
	LDA #$00
	STA SPRITE.RECORD+$2 		 ;set flag byte (this will make sure that the PC isn't accidentally treated as an MTT object)

	LDA #COMBAT.ENEMY_TYPE.MOB		
	STA SPRITE.RECORD+$5		 ;set enemy type. Since its PCs turn, the enemy type is mob, which includes mob specials. 

	STX SPRITE.RECORD+$B ;save active PC map object record index
	
	LDA #S_ENTITY_TYPE.PC
	STA SPRITE.RECORD+$A ;set S_ENTITY type to PC				

	RTS
	
@END
	
.TARGET.HIT.CHECK
@START
;PARAMETERS: YREG (screen index), COMBAT.EXPLOSION.PROB

;SAVE PARAMETERS
	TXA
	PHA
	
;WAS A TARGET PRESENT?				
	;YREG = screen index
	LDA SCREEN.MO_SPRITE.DATA,Y
	CMP #$FF 
	BEQ .CHECK.TARGET_HIT.EXIT ;if no S_ENTITY is present at this screen location then exit, don't record a target hit


;IS THE HIT TILE THE SPELLCASTER?
	CPY COMBAT.ATTACKER.SINDEX
	BEQ .CHECK.TARGET_HIT.EXIT
	
;IS THIS A DUPLICATE HIT?
;(note: this routine is needed because the iteration patter of some spells results in tiles being examined more than once, which could result
;in this routine being called more than once for the same tile)

	LDX #$00
	TYA ;transfer screen tile location to ACC
.DUPLICATE.CHECK.LOOP
	CMP COMBAT.TARGET_HIT.DB,X ;if S_ENTITY in current screen location was already recorded as a hit, then exit
	BEQ	.CHECK.TARGET_HIT.EXIT
	INX
;	CPX #COMBAT.TARGET_HIT.DB.STOP_VALUE
	CPX #COMBAT.TARGET_HIT.DB.SIZE
	BNE .DUPLICATE.CHECK.LOOP
		
	
;RECORD NEW TARGET HIT	
	;YREG = screen index
	TYA
	LDX COMBAT.TARGET_HIT.DB.INDEX
	STA COMBAT.TARGET_HIT.DB,X
	;write stop value
	;(note: each time a new target hit is recorded the stop value is advanced 1 byte in the array)
	LDA #COMBAT.TARGET_HIT.DB.STOP_VALUE
	STA COMBAT.TARGET_HIT.DB+$1,X

			; ;JSR KEYIN
			; STA TEMP
			; CMP #$D1
			; BNE .TEMP
			; ;LDA #$AA
			; LDA COMBAT.TARGET_HIT.DB,X
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP

	INC COMBAT.TARGET_HIT.DB.INDEX	;next target_hit record

.CHECK.TARGET_HIT.EXIT

;RESTORE PARAMETERS
	PLA
	TAX
			
	RTS	
@END

SPELL.SMITE.FIRE_PROJECTILE
@START		
;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA

.INIT
				;JSR KEYIN
				
	JSR .LOAD.SHAPE.TABLE

	;set projectile source
	JSR .ROLL.PROJECTILE.SOURCE

	
				
	;set projectile target
	;(if this particular smite spell is single-target then the target was selected before entrance to GE.SPELL.SMITE)
	LDA SPELL.SMITE.MULTI_TARGET.FLAG	;($00 = single target | $01 = multi-target)
	BEQ .SET.TARGETS.DONE
	
	LDY COMBAT.DEFENDER.SINDEX
	LDA SCREEN.INDEX.COLUMN,Y
	STA FP.TARGET.GMAP.X
	LDA SCREEN.INDEX.ROW,Y
	STA FP.TARGET.GMAP.Y	
.SET.TARGETS.DONE

;FIRE PROJECTILE

		;LDA #$00  ;disable (support was added for this)
		LDA #COMBAT.PROJECTILE.DELAY	;(time target = 1.6 seconds when there are 7 empty tiles between source and target)
		STA SHAPE.MOVE.DELAY_LENGTH ;The length of the delay between the shape draw and shape erase.

		;FP.TARGET.GMAP.X/Y: already set
		;FP.SOURCE.GMAP.X/Y: already set

		LDA #$00 ;set of on
		STA SHAPE.MOVE.SCREEN_ANIMATION.TOGGLE ;($00 = screen animation on | >=$01 = screen animation off)
		STA SHAPE.MOVE.ERASE.TOGGLE	;($00 = shape erase on | >=$01 = shape erase off)		
		LDA #$01	;set mode to angled shape ;($00 = angled static shape | >=$01 = non-angled, animated shape)
			;STA SHAPE.MOVE.SCREEN_ANIMATION.TOGGLE ;($00 = screen animation on | >=$01 = screen animation off)		
	JSR COMBAT.FIRE.PROJECTILE

				;JSR KEYIN

		
	;draw shape on target
	;(note: this is done because sometimes the COMABT.SHAPE.MOVE doesn't draw the shape directly on the target. Sometimes it does.)
	JSR .DRAW.SHAPE.ON.TARGET

				;JSR KEYIN
		
;RESTORE REGISTERS
	PLA 
	TAY
	PLA
	TAX
	
	RTS
	
	
.ROLL.PROJECTILE.SOURCE
@START
;PARAMETERS: NONE
;RETURN: FP.SOURCE.GMAP.X, FP.SOURCE.GMAP.Y
	
	;choose the position on screen edge that projectile will be fired from
		LDA #$00
		STA RND.LO
		LDA #$05
		STA RND.HI
	JSR RANDOM.8
		ASL ;X2 (converts random # to index for .SMITE.PROJECTILE.POSITIONS, which has 2 byte records)
		TAY ;transfer position to Y-REG
	;choose screen edge that projectile will be fired from
	JSR RANDOM.8
	;ACC: 8-bit random number
	CMP #$40
	BCC .NORTH	
	CMP #$80
	BCC .SOUTH
	CMP #$C0
	BCC .EAST
	
	;**FALLS THROUGH**
	
.WEST
	LDA .SMITE.PROJECTILE.POSITIONS.WEST+$0,Y
	STA FP.SOURCE.GMAP.X

	LDA .SMITE.PROJECTILE.POSITIONS.WEST+$1,Y
	STA FP.SOURCE.GMAP.Y
	JMP .ROLL.PROJECTILE.SOURCE.EXIT
	
.EAST	
	LDA .SMITE.PROJECTILE.POSITIONS.EAST+$0,Y
	STA FP.SOURCE.GMAP.X

	LDA .SMITE.PROJECTILE.POSITIONS.EAST+$1,Y
	STA FP.SOURCE.GMAP.Y
	JMP .ROLL.PROJECTILE.SOURCE.EXIT
	
.SOUTH	
	LDA .SMITE.PROJECTILE.POSITIONS.SOUTH+$0,Y
	STA FP.SOURCE.GMAP.X

	LDA .SMITE.PROJECTILE.POSITIONS.SOUTH+$1,Y
	STA FP.SOURCE.GMAP.Y
	JMP .ROLL.PROJECTILE.SOURCE.EXIT

.NORTH
	LDA .SMITE.PROJECTILE.POSITIONS.NORTH+$0,Y
	STA FP.SOURCE.GMAP.X

	LDA .SMITE.PROJECTILE.POSITIONS.NORTH+$1,Y
	STA FP.SOURCE.GMAP.Y

	;**FALLS THROUGH**
	
.ROLL.PROJECTILE.SOURCE.EXIT

	RTS
	
.SMITE.PROJECTILE.POSITIONS.NORTH	.HS 00.00.02.00.04.00.06.00.08.00.0A.00
	;datagram: byte$0 = GMAP.X, byte$01 = GMAP.Y   (refers to the starting position of the projectile)

.SMITE.PROJECTILE.POSITIONS.SOUTH	.HS 00.0A.02.0A.04.0A.06.0A.08.0A.0A.0A
	;datagram: byte$0 = GMAP.X, byte$01 = GMAP.Y   (refers to the starting position of the projectile)

.SMITE.PROJECTILE.POSITIONS.EAST	.HS 0A.00.0A.02.0A.04.0A.06.0A.08.0A.0A
	;datagram: byte$0 = GMAP.X, byte$01 = GMAP.Y   (refers to the starting position of the projectile)

.SMITE.PROJECTILE.POSITIONS.WEST	.HS 00.00.00.02.00.04.00.06.00.08.00.0A
	;datagram: byte$0 = GMAP.X, byte$01 = GMAP.Y   (refers to the starting position of the projectile)
	
@END

.DRAW.SHAPE.ON.TARGET
@START
;PARAMETERES: none
;ENTRANCE: direct

	;refresh shape table
	;(note: this needs to be done because on sharp angles COMBAT.FIRE.PROJECTILE or its subroutines modify the contents
	;of SHAPE_HOPPER0+$0-SHAPE_HOPPER0+$1F for some reason. I'm not sure why, but refreshing the shape tables resolve the issue for this routine.)
	JSR .LOAD.SHAPE.TABLE
	
	
		;set parameters
		LDY SHAPE.SINDEX.TARGET	
		LDA SCREEN.INDEX.TILE_SBYTE,Y
		STA	SHAPE.DRAW.CURRENT.SBYTE
		
		LDA SCREEN.INDEX.TILE_LINE,Y
		STA SHAPE.DRAW.LINE_ROW.NUMBER

		;Y-REG = screen tile lcation of target
		LDA #$01 ;set foreground page mode
		STA PAGE.FOREGROUND.OVERRIDE
		LDA #$00 ;set draw mode
	JSR .DRAW.TILE.AND_ORA
	
		LDA #$A0		
	JSR WAIT.LOOP

	RTS


@END

.DRAW.TILE.AND_ORA ;draw single tile using AND + ORA mask
@START
;PARAMETERS:  	;ACC = ($00 = draw mode | $01 = erase mode), SHAPE.DRAW.LINE_ROW.NUMBER, SHAPE.DRAW.CURRENT.SBYTE, [PAGE.FOREGROUND.OVERRIDE]
;RETURN: NONE
;ENTRANCE: .DRAW.SHAPE.ON.TARGET
;*Set to default value on game launch. When modified (i.e. tall grass), it is reset to default when done.

;=====================SUBROUTINE DOCUMENTATION====================================
;This subroutine, COMBAT.SHAPE.DRAW and DRAW.TILE are the lowest level routines associated with tile graphics plotting. 
;See DRAW.TILE for general documentation on the methodology used to plot to the graphics screen.
;
;This subroutine's unique purpose is to be able to draw a single tile using an AND+ORA mask. 
;
;This subroutine is specifically designed to draw shapes which are objects drawn on top of a background. The underlying
;terrain can be supplied by terrain tiles or other object tiles. However, this routine can draw over any 14x16 section
;the hi-res screen whether it conforms to the tile grid or not.
;
;To blend shapes into the background, two shape tables are used. One containing and AND mask and the other containing
;an ORA mask.
;
;here is a quick summary of the concept:
;(also see the first two posts from Michael Pohoreski in the following
;post https://groups.google.com/forum/#!topic/comp.sys.apple2.programmer/cgxZOBIL728)
;
; byte1: 1111110
; byte2: 0000010
;
; Using AND on byte 1 and byte 2 results in:
;
; result: 0000010
;
; (i.e. each bit is assigned the value 1 only if both byte1 AND byte2 have a 1 in that bit position.
;
;
; OR works similarly except that a 1 is assigned to each bit position in the result if either byte1 OR byte2 has a 1 in that bit position. 
;
;
; The technique works like this:
;
; before drawing the shape copy the background, and apply an AND value to each byte. The AND values has "0" values where the shape will be. The result is a shape table that has the background and a small "hole" carved out where the shape will go.
;
; Then apply an OR value to each byte. the OR values have "1" values where the shape is. The result is shape table with 1s where either the background or the shape is. This is different from simply drawing the shape on top of the background because when the "hole" is carved out, a two pixel buffer is put around the shape so that the background pixels and the shape pixels don't interact to create unwanted lines and colors. 
;=================================================================================
	
	;ACC = $00 = draw mode | $01 = erase mode
	STA DRAW.SHAPE.MODE

;**OPT** Speed. Include an in-line code version of DRAW.TILE with DRAW.COLUMN and DRAW.ROW to avoid the costly JSR/RTS to call as a subroutine. 

.SAVE.REGISTERS
	TXA
	PHA
	TYA
	PHA

.START	
	LDX SHAPE.DRAW.LINE_ROW.NUMBER			;LOAD LINE IN X REGISTER	
	TXA ;transfer SHAPE.DRAW.LINE_ROW.NUMBER to ACC
	STA TILE.LINE.CNTR 
	STA TILE.LINE.START
	CLC						;*OPT* I I THINK ALL CLS CAN BE REMOVED WHEN I'M NOT EXPECTING AN OVERFLOW
	ADC #TILE.DEPTH.STANDARD				;ADD DEPTH OF SHAPE TO STARTING LINE IN HEX (# of lines, not the position of last line....so line positions $0-$F is $10 (!16) total lines)
	STA TILE.LINE.STOP		;SET THE LAST LINE TO DRAW BASED ON THE DEPTH OF THE TILE

	LDA #$00
	STA SHP.TBL.CNTR		;START DRAWING AT BEGINNING OF SHAPE TABLE
	
	
.DRAW.LOOP

;GET LINE ADDRESS
@START

;==========INLINE CODE GET.LINE.ADDRESS1==================	
	LDA PAGE.FOREGROUND.OVERRIDE	;CHECK TO SEE IF CALLING ROUTINE WANTS ERASE DONE TO FOREGROUND INSTEAD OF BACKGROUND (DEFAULT)
	CMP #$01
	BEQ .FOREGROUND_OVERRIDE
	
	LDA PAGE.BACKGROUND		
	CMP #$02
	BEQ .LOOKUP.PAGE2
	JMP .LOOKUP.PAGE1
	
.FOREGROUND_OVERRIDE
	LDA PAGE.FOREGROUND		;SPECIFY PAGE TO GET ADDRESS FOR (SHOULD BE BACKGROUND PAGE)
	CMP #$02
	BEQ .LOOKUP.PAGE2
	

.LOOKUP.PAGE1	
	LDA LINE.HO.P1,X		;GET LINE ADDRESS
	STA LINE.BASE.ADDR1+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR1
	JMP .GETLINE_COMPLETE

.LOOKUP.PAGE2
	LDA LINE.HO.P2,X		;GET LINE ADDRESS
	STA LINE.BASE.ADDR1+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR1
.GETLINE_COMPLETE
;========================================================
@END
	
.DRAW_ERASE.ENTRANCE
	LDA DRAW.SHAPE.MODE ;ACC = $00 = draw mode | $01 = erase mode
	;BNE .ERASE.MODE
	BEQ .DRAW.MODE
	JMP .ERASE.MODE
	;**FALLS THROUGH**
	
.DRAW.MODE	
;screen byte 0
	LDY SHP.TBL.CNTR		;LOAD Y WITH SHAPE TABLE COUNTER	
	;init counters/indexes
	TYA ;init X-REG to $00 as the SHAPE.EXISTING_DATA.BUFFER index
	TAX ;""
	

	;save existing graphics data
	LDY SHAPE.DRAW.CURRENT.SBYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	LDA (LINE.BASE.ADDR1),Y	;load existing data from video memory
	STA SHAPE.EXISTING_DATA.BUFFER,X
			;STA TEMP
	;load new shape data
	;ACC = existing graphics data
	LDY SHP.TBL.CNTR		;LOAD Y WITH SHAPE TABLE COUNTER	
	AND (SHAPE.BACKGROUND.AND_MASK),Y
	ORA (SHAPE.COMPOSITE.ORA_MASK),Y
		
	;write new shape data
	LDY SHAPE.DRAW.CURRENT.SBYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	STA (LINE.BASE.ADDR1),Y	;PLOT (1st screen byte)
	
;next screen byte
	INC SHP.TBL.CNTR		;NEXT SHAPE BYTE
	INX	;next existing data buffer byte
	INC SHAPE.DRAW.CURRENT.SBYTE			;SWITCH TO 2ND SCREEN BYTE IN THE TILE	
	
;screen byte 1
	
	;save existing graphics data
	LDY SHAPE.DRAW.CURRENT.SBYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	LDA (LINE.BASE.ADDR1),Y	;load existing data from video memory
	STA SHAPE.EXISTING_DATA.BUFFER,X
		
	;load new shape data
	;ACC = existing graphics data
	LDY SHP.TBL.CNTR		;LOAD Y WITH SHAPE TABLE COUNTER	
	AND (SHAPE.BACKGROUND.AND_MASK),Y
	ORA (SHAPE.COMPOSITE.ORA_MASK),Y
		
	;write new shape data
	LDY SHAPE.DRAW.CURRENT.SBYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	STA (LINE.BASE.ADDR1),Y	;PLOT (1st screen byte)
	JMP .NEXT.LINE
	
.ERASE.MODE
;screen byte0
	LDY SHP.TBL.CNTR		;LOAD Y WITH SHAPE TABLE COUNTER
	LDA SHAPE.EXISTING_DATA.BUFFER,Y			;LOAD SHAPE BYTE (1st screen byte of the tile)

	LDY SHAPE.DRAW.CURRENT.SBYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	STA (LINE.BASE.ADDR1),Y	;PLOT (1st screen byte)

;next screen byte	
	INC SHP.TBL.CNTR		;NEXT SHAPE BYTE, NEXT TILE LINE
	INC SHAPE.DRAW.CURRENT.SBYTE			;SWITCH TO 2ND SCREEN BYTE IN THE TILE

;screen byte1	
	LDY SHP.TBL.CNTR		;LOAD Y WITH SHAPE TABLE COUNTER
	LDA SHAPE.EXISTING_DATA.BUFFER,Y			;LOAD SHAPE BYTE (2nd screen byte of the tile)
	LDY SHAPE.DRAW.CURRENT.SBYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	STA (LINE.BASE.ADDR1),Y	
	;**FALLS THROUGH**
	
.NEXT.LINE
;next line, reset screen byte	
	DEC SHAPE.DRAW.CURRENT.SBYTE			;SWITCH BACK TO 1ST SCREEN BYTE IN THE TILE	;**OPT** if the counters are updated after the CMP/branch it might save 1 INC SHAPE.DRAW.CURRENT.SBYTE instruction in the main loop because SHAPE.DRAW.CURRENT.SBYTE will be in 2nd position when this loop ends. 
	INC SHAPE.DRAW.LINE_ROW.NUMBER
	
	INC SHP.TBL.CNTR		;NEXT SHAPE BYTE
	INC TILE.LINE.CNTR		;NEXT TILE LINE
	LDX TILE.LINE.CNTR 		;restore tile line number to X-REG

;exit test	
	CPX TILE.LINE.STOP		;IS TILE DONE?							
	BCS .DONE				
	JMP .DRAW.LOOP
.DONE	
	LDA #$00
	STA PAGE.FOREGROUND.OVERRIDE
				
.RESTORE.REGISTERS
	PLA
	TAY
	PLA
	TAX
	
	RTS

@END	

.LOAD.SHAPE.TABLE
@START
;PARAMETERS: none
;ENTRANCE: direct

;load the shape into the hoppers
	LDX #$00
	LDY #$00
.PROJECTILE.LOAD.SHAPE.LOOP0
	LDA (SMITE.SHAPE_TABLE.POINTER),Y
	STA SHAPE.HOPPER0,X
	INY
	INX
	BNE .PROJECTILE.LOAD.SHAPE.LOOP0
		
	RTS
@END

@END


;SHAPE TABLE: SWORD
@START
SHAPE_TABLE.SWORD.START
.WEAPON.THROWING_KNIFE.SHAPE_0.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.FF.F3.FF.C0.A7.C0.81.E0.81.F0.87.F8.87.FC.81.F8.81.F8.A7.FE.FF.FF.FF.FF.FF.FF
.WEAPON.THROWING_KNIFE.SHAPE_0.ORA_MASK			.HS	80.80.80.80.80.80.80.80.80.8C.80.8E.98.87.D8.83.E0.81.E0.80.D8.81.D8.81.80.80.80.80.80.80.80.80
				
.WEAPON.THROWING_KNIFE.SHAPE_1.AND_MASK			.HS	7F.7F.7F.7F.7F.7F.27.7E.01.78.01.78.07.7C.01.7C.01.78.07.70.1F.60.3F.40.7F.40.7F.73.7F.7F.7F.7F
.WEAPON.THROWING_KNIFE.SHAPE_1.ORA_MASK			.HS	80.80.80.80.80.80.80.80.D8.81.D8.81.E0.80.F8.80.D8.81.C0.83.80.87.80.8E.80.8C.80.80.80.80.80.80
				
.WEAPON.THROWING_KNIFE.SHAPE_2.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.BF.F2.8F.C0.8F.C0.9F.F0.8F.C0.87.C0.83.F0.81.FE.81.FF.E7.FF.FF.FF.FF.FF.FF.FF
.WEAPON.THROWING_KNIFE.SHAPE_2.ORA_MASK			.HS	80.80.80.80.80.80.80.80.C0.8D.C0.8D.80.83.C0.8F.E0.8D.F0.80.B8.80.98.80.80.80.80.80.80.80.80.80
				
.WEAPON.THROWING_KNIFE.SHAPE_3.AND_MASK			.HS	7F.7F.7F.7F.7F.7F.67.7F.01.7F.01.7E.03.70.07.40.0F.40.1F.70.0F.40.0F.40.3F.72.7F.7F.7F.7F.7F.7F
.WEAPON.THROWING_KNIFE.SHAPE_3.ORA_MASK			.HS	80.80.80.80.80.80.80.80.98.80.B8.80.F0.80.E0.8D.C0.8F.80.83.C0.8D.C0.8D.80.80.80.80.80.80.80.80

@END

;SHAPE TABLE: AXE
@START
SHAPE_TABLE.AXE.START
;----------
.WEAPON.THROWING_STAR.QTY3.SHAPE_0.AND_MASK			.HS	FF.FF.BF.FF.8F.FC.83.FC.87.F8.87.FE.DF.F7.F7.C1.81.C0.81.80.80.E0.83.FB.EF.FF.FF.FF.FF.FF.FF.FF
.WEAPON.THROWING_STAR.QTY3.SHAPE_0.ORA_MASK			.HS	80.80.80.80.C0.80.F0.80.E0.81.A0.80.80.80.80.88.88.8E.B8.9C.9C.84.90.80.80.80.80.80.80.80.80.80
				
.WEAPON.THROWING_STAR.QTY3.SHAPE_1.AND_MASK			.HS	7F.7F.5F.7F.07.7E.07.78.03.7C.0F.7C.3F.7B.6F.60.03.00.00.40.01.40.41.77.77.7F.7F.7F.7F.7F.7F.7F
.WEAPON.THROWING_STAR.QTY3.SHAPE_1.ORA_MASK			.HS	80.80.80.80.A0.80.E0.81.F0.80.C0.80.80.80.80.84.90.9C.9C.8E.B8.88.88.80.80.80.80.80.80.80.80.80
				
.WEAPON.THROWING_STAR.QTY3.SHAPE_2.AND_MASK			.HS	FF.FF.BF.FF.8F.FC.83.FC.87.F8.87.FE.DF.F7.F7.C1.81.C0.81.80.80.E0.83.FB.EF.FF.FF.FF.FF.FF.FF.FF
.WEAPON.THROWING_STAR.QTY3.SHAPE_2.ORA_MASK			.HS	80.80.80.80.C0.80.F0.80.E0.81.A0.80.80.80.80.88.88.8E.B8.9C.9C.84.90.80.80.80.80.80.80.80.80.80
				
.WEAPON.THROWING_STAR.QTY3.SHAPE_3.AND_MASK			.HS	7F.7F.5F.7F.07.7E.07.78.03.7C.0F.7C.3F.7B.6F.60.03.00.00.40.01.40.41.77.77.7F.7F.7F.7F.7F.7F.7F
.WEAPON.THROWING_STAR.QTY3.SHAPE_3.ORA_MASK			.HS	80.80.80.80.A0.80.E0.81.F0.80.C0.80.80.80.80.84.90.9C.9C.8E.B8.88.88.80.80.80.80.80.80.80.80.80


@END

;SHAPE TABLE: BOULDER
@START
SHAPE_TABLE.BOULDER.START
;----------
.PROJECTILE.BOULDER.SHAPE_0.AND_MASK			.HS	FF.FF.FF.FF.DF.FF.87.FC.87.F0.83.E0.81.80.81.C0.81.C0.81.C0.80.C0.80.E0.83.F0.9F.E0.FF.F9.FF.FF
.PROJECTILE.BOULDER.SHAPE_0.ORA_MASK			.HS	00.00.00.00.00.00.20.00.60.03.70.07.78.1D.58.0F.78.0F.78.0F.7C.0E.74.07.60.03.00.06.00.00.00.00
				
.PROJECTILE.BOULDER.SHAPE_1.AND_MASK			.HS	7F.7F.7F.7F.5F.7F.07.7C.07.70.03.60.01.00.01.40.01.40.01.40.00.40.00.60.03.70.1F.60.7F.79.7F.7F
.PROJECTILE.BOULDER.SHAPE_1.ORA_MASK			.HS	00.00.00.00.00.00.20.00.60.03.70.07.78.1D.58.0F.78.0F.78.0F.7C.0E.74.07.60.03.00.06.00.00.00.00
				
.PROJECTILE.BOULDER.SHAPE_2.AND_MASK			.HS	FF.FF.FF.FF.DF.FF.87.FC.87.F0.83.E0.81.80.81.C0.81.C0.81.C0.80.C0.80.E0.83.F0.9F.E0.FF.F9.FF.FF
.PROJECTILE.BOULDER.SHAPE_2.ORA_MASK			.HS	00.00.00.00.00.00.20.00.60.03.70.07.78.1D.58.0F.78.0F.78.0F.7C.0E.74.07.60.03.00.06.00.00.00.00
				
.PROJECTILE.BOULDER.SHAPE_3.AND_MASK			.HS	7F.7F.7F.7F.5F.7F.07.7C.07.70.03.60.01.00.01.40.01.40.01.40.00.40.00.60.03.70.1F.60.7F.79.7F.7F
.PROJECTILE.BOULDER.SHAPE_3.ORA_MASK			.HS	00.00.00.00.00.00.20.00.60.03.70.07.78.1D.58.0F.78.0F.78.0F.7C.0E.74.07.60.03.00.06.00.00.00.00

@END


.LOCAL.VARIABLES
@START


@END

@END
	.EP ;end patch mode
;--------
GE.SPELL.SMITE.END

GE.SPELL.SHOCK_SPHERE.START ;marks the start address of the code block for this spell
;--------
@START
	.PH SPELL.FILE.CODE_BLOCK.BUFFER.START ;start patch 

GE.SPELL.SHOCK_SPHERE
@START
;PARAMETERS: FP.SOURCE.GMAP.X, FP.SOURCE.GMAP.Y, FP.TARGET.GMAP.X, FP.TARGET.GMAP.Y
;ENTRANCE: DIRECT		
;RETURN: targets hit (COMBAT.TARGET_HIT.DB)

;=====================SUBROUTINE DOCUMENTATION====================================
;
;-target hits
;COMBAT.TARGET_HIT.DB is populated by a call to .TARGET.HIT.CHECK.
;
;
;=================================================================================


.INIT

	JSR COMBAT.PLAY.SOUND.ELECTRICITY
	
	JSR .LOAD.SHAPE.TABLE

		
;FIRE PROJECTILE

		;LDA #$00  ;disable (support was added for this)
		LDA #COMBAT.PROJECTILE.DELAY	;(time target = 1.6 seconds when there are 7 empty tiles between source and target)
		STA SHAPE.MOVE.DELAY_LENGTH ;The length of the delay between the shape draw and shape erase.

		LDA #$00 ;set of on
		STA SHAPE.MOVE.SCREEN_ANIMATION.TOGGLE ;($00 = screen animation on | >=$01 = screen animation off)
		STA SHAPE.MOVE.ERASE.TOGGLE	;($00 = shape erase on | >=$01 = shape erase off)		
		LDA #$01	;set mode to angled shape ;($00 = angled static shape | >=$01 = non-angled, animated shape)
			;STA SHAPE.MOVE.SCREEN_ANIMATION.TOGGLE ;($00 = screen animation on | >=$01 = screen animation off)
	JSR COMBAT.FIRE.PROJECTILE

	;draw shape on target
	;(note: this is done because sometimes the COMABT.SHAPE.MOVE doesn't draw the shape directly on the target. Sometimes it does.)
	JSR .DRAW.SHAPE.ON.TARGET
	
	;**FALLS THROUGH**
	
.UPDATE.TARGET_HIT.DATABASE	

;get screen location of prospective GMAP.X/Y of target
		LDA FP.TARGET.GMAP.X ;load new special S_ENTITYY GMAP.X
		STA PARM.GMAP.X
		LDA FP.TARGET.GMAP.Y ;load new special S_ENTITY GMAP.Y
		STA PARM.GMAP.Y				
	JSR CONVERT.COLUMN_ROW.TO.SINDEX
		LDY RETURN.SCREEN_ARRAY_INDEX ;used below
	JSR .TARGET.HIT.CHECK

.SET.CRITICAL_HIT.FLAGS
@START
;(engaged s_entity screen index)

		;**warning: attacker character sheet will be loaded on exit
		LDY COMBAT.ATTACKER.SINDEX
		LDA COMBAT.TARGET_HIT.DB+$0 ;set value to update (sindex of the target)	
.UPDATE.CHR_SHEET.ENGAGED.INDEX ;**OPT** Memory. This routine was written as as subroutine, but is being used inline here and it could be made more efficient. 
;PARAMETERS: YREG = screen index of attacker, ACC = value to write to CHR_SHEET.PC_MOB.ENGAGED.SINDEX
;ENTRANCE: direct
;RETURN: saved attacker character sheet with updated CHR_SHEET.PC_MOB.ENGAGED.SINDEX field

.SAVE.PARAMETERS.ENAGED
	;ACC = value to write to CHR_SHEET.PC_MOB.ENGAGED.SINDEX
	STA COMBAT.ENGAGED_SINDEX.UPDATE.PARM
	
	;read attacker (MOB) character sheet
		
		;YREG = COMBAT.ATTACKER.SINDEX
		LDA #$00 ;set read mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet

			
	;set the engaged field to the updated value
	;(expected to be the screen index of the defender or $FF to indicate no engagement)
	LDA COMBAT.ENGAGED_SINDEX.UPDATE.PARM
	STA CHR_SHEET.PC_MOB.ENGAGED.SINDEX

			
		;YREG = COMBAT.ATTACKER.SINDEX
		LDA #$01 ;set write mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet

@END


.EXIT

	RTS

@END

.LOAD.SHAPE.TABLE
@START
;PARAMETERS: none
;ENTRANCE: direct

;load the shape into the hoppers
	LDX #$00
.PROJECTILE.LOAD.SHAPE.LOOP0
	LDA .SHAPE_TABLE.SHOCK_SPHERE.START,X	
	STA SHAPE.HOPPER0,X
	INX
	BNE .PROJECTILE.LOAD.SHAPE.LOOP0

	RTS
@END

.DRAW.SHAPE.ON.TARGET
@START
;PARAMETERES: none
;ENTRANCE: direct

	;refresh shape table
	;(note: this needs to be done because on sharp angles COMBAT.FIRE.PROJECTILE or its subroutines modify the contents
	;of SHAPE_HOPPER0+$0-SHAPE_HOPPER0+$1F for some reason. I'm not sure why, but refreshing the shape tables resolve the issue for this routine.)
	JSR .LOAD.SHAPE.TABLE
	
	
		;set parameters
		LDY SHAPE.SINDEX.TARGET	
		LDA SCREEN.INDEX.TILE_SBYTE,Y
		STA	SHAPE.DRAW.CURRENT.SBYTE
		
		LDA SCREEN.INDEX.TILE_LINE,Y
		STA SHAPE.DRAW.LINE_ROW.NUMBER

		;Y-REG = screen tile lcation of target
		LDA #$01 ;set foreground page mode
		STA PAGE.FOREGROUND.OVERRIDE
		LDA #$00 ;set draw mode
	JSR .DRAW.TILE.AND_ORA
	
		LDA #$A0		
	JSR WAIT.LOOP

	RTS


@END

.DRAW.TILE.AND_ORA ;draw single tile using AND + ORA mask
@START
;PARAMETERS:  	;ACC = ($00 = draw mode | $01 = erase mode), SHAPE.DRAW.LINE_ROW.NUMBER, SHAPE.DRAW.CURRENT.SBYTE, [PAGE.FOREGROUND.OVERRIDE]
;RETURN: NONE
;ENTRANCE: .DRAW.SHAPE.ON.TARGET
;*Set to default value on game launch. When modified (i.e. tall grass), it is reset to default when done.

;=====================SUBROUTINE DOCUMENTATION====================================
;This subroutine, COMBAT.SHAPE.DRAW and DRAW.TILE are the lowest level routines associated with tile graphics plotting. 
;See DRAW.TILE for general documentation on the methodology used to plot to the graphics screen.
;
;This subroutine's unique purpose is to be able to draw a single tile using an AND+ORA mask. 
;
;This subroutine is specifically designed to draw shapes which are objects drawn on top of a background. The underlying
;terrain can be supplied by terrain tiles or other object tiles. However, this routine can draw over any 14x16 section
;the hi-res screen whether it conforms to the tile grid or not.
;
;To blend shapes into the background, two shape tables are used. One containing and AND mask and the other containing
;an ORA mask.
;
;here is a quick summary of the concept:
;(also see the first two posts from Michael Pohoreski in the following
;post https://groups.google.com/forum/#!topic/comp.sys.apple2.programmer/cgxZOBIL728)
;
; byte1: 1111110
; byte2: 0000010
;
; Using AND on byte 1 and byte 2 results in:
;
; result: 0000010
;
; (i.e. each bit is assigned the value 1 only if both byte1 AND byte2 have a 1 in that bit position.
;
;
; OR works similarly except that a 1 is assigned to each bit position in the result if either byte1 OR byte2 has a 1 in that bit position. 
;
;
; The technique works like this:
;
; before drawing the shape copy the background, and apply an AND value to each byte. The AND values has "0" values where the shape will be. The result is a shape table that has the background and a small "hole" carved out where the shape will go.
;
; Then apply an OR value to each byte. the OR values have "1" values where the shape is. The result is shape table with 1s where either the background or the shape is. This is different from simply drawing the shape on top of the background because when the "hole" is carved out, a two pixel buffer is put around the shape so that the background pixels and the shape pixels don't interact to create unwanted lines and colors. 
;=================================================================================
	
	;ACC = $00 = draw mode | $01 = erase mode
	STA DRAW.SHAPE.MODE

;**OPT** Speed. Include an in-line code version of DRAW.TILE with DRAW.COLUMN and DRAW.ROW to avoid the costly JSR/RTS to call as a subroutine. 

.SAVE.REGISTERS
	TXA
	PHA
	TYA
	PHA

.START	
	LDX SHAPE.DRAW.LINE_ROW.NUMBER			;LOAD LINE IN X REGISTER	
	TXA ;transfer SHAPE.DRAW.LINE_ROW.NUMBER to ACC
	STA TILE.LINE.CNTR 
	STA TILE.LINE.START
	CLC						;*OPT* I I THINK ALL CLS CAN BE REMOVED WHEN I'M NOT EXPECTING AN OVERFLOW
	ADC #TILE.DEPTH.STANDARD				;ADD DEPTH OF SHAPE TO STARTING LINE IN HEX (# of lines, not the position of last line....so line positions $0-$F is $10 (!16) total lines)
	STA TILE.LINE.STOP		;SET THE LAST LINE TO DRAW BASED ON THE DEPTH OF THE TILE

	LDA #$00
	STA SHP.TBL.CNTR		;START DRAWING AT BEGINNING OF SHAPE TABLE
	
	
.DRAW.LOOP

;GET LINE ADDRESS
@START

;==========INLINE CODE GET.LINE.ADDRESS1==================	
	LDA PAGE.FOREGROUND.OVERRIDE	;CHECK TO SEE IF CALLING ROUTINE WANTS ERASE DONE TO FOREGROUND INSTEAD OF BACKGROUND (DEFAULT)
	CMP #$01
	BEQ .FOREGROUND_OVERRIDE
	
	LDA PAGE.BACKGROUND		
	CMP #$02
	BEQ .LOOKUP.PAGE2
	JMP .LOOKUP.PAGE1
	
.FOREGROUND_OVERRIDE
	LDA PAGE.FOREGROUND		;SPECIFY PAGE TO GET ADDRESS FOR (SHOULD BE BACKGROUND PAGE)
	CMP #$02
	BEQ .LOOKUP.PAGE2
	

.LOOKUP.PAGE1	
	LDA LINE.HO.P1,X		;GET LINE ADDRESS
	STA LINE.BASE.ADDR1+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR1
	JMP .GETLINE_COMPLETE

.LOOKUP.PAGE2
	LDA LINE.HO.P2,X		;GET LINE ADDRESS
	STA LINE.BASE.ADDR1+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR1
.GETLINE_COMPLETE
;========================================================
@END
	
.DRAW_ERASE.ENTRANCE
	LDA DRAW.SHAPE.MODE ;ACC = $00 = draw mode | $01 = erase mode
	;BNE .ERASE.MODE
	BEQ .DRAW.MODE
	JMP .ERASE.MODE
	;**FALLS THROUGH**
	
.DRAW.MODE	
;screen byte 0
	LDY SHP.TBL.CNTR		;LOAD Y WITH SHAPE TABLE COUNTER	
	;init counters/indexes
	TYA ;init X-REG to $00 as the SHAPE.EXISTING_DATA.BUFFER index
	TAX ;""
	

	;save existing graphics data
	LDY SHAPE.DRAW.CURRENT.SBYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	LDA (LINE.BASE.ADDR1),Y	;load existing data from video memory
	STA SHAPE.EXISTING_DATA.BUFFER,X
			;STA TEMP
	;load new shape data
	;ACC = existing graphics data
	LDY SHP.TBL.CNTR		;LOAD Y WITH SHAPE TABLE COUNTER	
	AND (SHAPE.BACKGROUND.AND_MASK),Y
	ORA (SHAPE.COMPOSITE.ORA_MASK),Y
		
	;write new shape data
	LDY SHAPE.DRAW.CURRENT.SBYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	STA (LINE.BASE.ADDR1),Y	;PLOT (1st screen byte)
	
;next screen byte
	INC SHP.TBL.CNTR		;NEXT SHAPE BYTE
	INX	;next existing data buffer byte
	INC SHAPE.DRAW.CURRENT.SBYTE			;SWITCH TO 2ND SCREEN BYTE IN THE TILE	
	
;screen byte 1
	
	;save existing graphics data
	LDY SHAPE.DRAW.CURRENT.SBYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	LDA (LINE.BASE.ADDR1),Y	;load existing data from video memory
	STA SHAPE.EXISTING_DATA.BUFFER,X
		
	;load new shape data
	;ACC = existing graphics data
	LDY SHP.TBL.CNTR		;LOAD Y WITH SHAPE TABLE COUNTER	
	AND (SHAPE.BACKGROUND.AND_MASK),Y
	ORA (SHAPE.COMPOSITE.ORA_MASK),Y
		
	;write new shape data
	LDY SHAPE.DRAW.CURRENT.SBYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	STA (LINE.BASE.ADDR1),Y	;PLOT (1st screen byte)
	JMP .NEXT.LINE
	
.ERASE.MODE
;screen byte0
	LDY SHP.TBL.CNTR		;LOAD Y WITH SHAPE TABLE COUNTER
	LDA SHAPE.EXISTING_DATA.BUFFER,Y			;LOAD SHAPE BYTE (1st screen byte of the tile)

	LDY SHAPE.DRAW.CURRENT.SBYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	STA (LINE.BASE.ADDR1),Y	;PLOT (1st screen byte)

;next screen byte	
	INC SHP.TBL.CNTR		;NEXT SHAPE BYTE, NEXT TILE LINE
	INC SHAPE.DRAW.CURRENT.SBYTE			;SWITCH TO 2ND SCREEN BYTE IN THE TILE

;screen byte1	
	LDY SHP.TBL.CNTR		;LOAD Y WITH SHAPE TABLE COUNTER
	LDA SHAPE.EXISTING_DATA.BUFFER,Y			;LOAD SHAPE BYTE (2nd screen byte of the tile)
	LDY SHAPE.DRAW.CURRENT.SBYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	STA (LINE.BASE.ADDR1),Y	
	;**FALLS THROUGH**
	
.NEXT.LINE
;next line, reset screen byte	
	DEC SHAPE.DRAW.CURRENT.SBYTE			;SWITCH BACK TO 1ST SCREEN BYTE IN THE TILE	;**OPT** if the counters are updated after the CMP/branch it might save 1 INC SHAPE.DRAW.CURRENT.SBYTE instruction in the main loop because SHAPE.DRAW.CURRENT.SBYTE will be in 2nd position when this loop ends. 
	INC SHAPE.DRAW.LINE_ROW.NUMBER
	
	INC SHP.TBL.CNTR		;NEXT SHAPE BYTE
	INC TILE.LINE.CNTR		;NEXT TILE LINE
	LDX TILE.LINE.CNTR 		;restore tile line number to X-REG

;exit test	
	CPX TILE.LINE.STOP		;IS TILE DONE?							
	BCS .DONE				
	JMP .DRAW.LOOP
.DONE	
	LDA #$00
	STA PAGE.FOREGROUND.OVERRIDE
				
.RESTORE.REGISTERS
	PLA
	TAY
	PLA
	TAX
	
	RTS

@END	

.TARGET.HIT.CHECK
@START
;PARAMETERS: YREG (screen index), COMBAT.EXPLOSION.PROB

;SAVE PARAMETERS
	TXA
	PHA
	
;WAS A TARGET PRESENT?				
	;YREG = screen index
	LDA SCREEN.MO_SPRITE.DATA,Y
	CMP #$FF 
	BEQ .CHECK.TARGET_HIT.EXIT ;if no S_ENTITY is present at this screen location then exit, don't record a target hit


;IS THE HIT TILE THE SPELLCASTER?
	CPY COMBAT.ATTACKER.SINDEX
	BEQ .CHECK.TARGET_HIT.EXIT
	
;IS THIS A DUPLICATE HIT?
;(note: this routine is needed because the iteration patter of some spells results in tiles being examined more than once, which could result
;in this routine being called more than once for the same tile)

	LDX #$00
	TYA ;transfer screen tile location to ACC
.DUPLICATE.CHECK.LOOP
	CMP COMBAT.TARGET_HIT.DB,X ;if S_ENTITY in current screen location was already recorded as a hit, then exit
	BEQ	.CHECK.TARGET_HIT.EXIT
	INX
;	CPX #COMBAT.TARGET_HIT.DB.STOP_VALUE
	CPX #COMBAT.TARGET_HIT.DB.SIZE
	BNE .DUPLICATE.CHECK.LOOP
		
	
;RECORD NEW TARGET HIT	
	;YREG = screen index
	TYA
	LDX COMBAT.TARGET_HIT.DB.INDEX
	STA COMBAT.TARGET_HIT.DB,X
	;write stop value
	;(note: each time a new target hit is recorded the stop value is advanced 1 byte in the array)
	LDA #COMBAT.TARGET_HIT.DB.STOP_VALUE
	STA COMBAT.TARGET_HIT.DB+$1,X

			; ;JSR KEYIN
			; STA TEMP
			; CMP #$D1
			; BNE .TEMP
			; ;LDA #$AA
			; LDA COMBAT.TARGET_HIT.DB,X
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP

	INC COMBAT.TARGET_HIT.DB.INDEX	;next target_hit record

.CHECK.TARGET_HIT.EXIT

;RESTORE PARAMETERS
	PLA
	TAX
			
	RTS	
@END

;SHAPE TABLE: SHOCK_SPHERE
@START
.SHAPE_TABLE.SHOCK_SPHERE.START
.SPELL.SHOCK_SPHERE.SHAPE_0.AND_MASK			.HS	80.F0.80.F0.80.C0.8C.80.80.C0.80.80.83.80.80.C0.80.C0.80.80.83.D0.80.C0.80.80.82.C0.80.C0.82.F4
.SPELL.SHOCK_SPHERE.SHAPE_0.ORA_MASK			.HS	C4.82.D0.83.F3.8A.C0.9B.91.8E.DC.BA.F0.AE.F4.8B.DC.8E.F4.A2.D0.83.F4.8E.9C.BA.C0.8E.CD.8B.90.80
				
.SPELL.SHOCK_SPHERE.SHAPE_1.AND_MASK			.HS	7F.7F.00.00.00.40.00.00.01.00.00.00.00.40.00.00.01.00.00.40.00.60.08.00.00.40.03.00.00.00.00.47
.SPELL.SHOCK_SPHERE.SHAPE_1.ORA_MASK			.HS	80.80.DC.B0.F4.8E.DC.8B.D8.E6.90.8C.C3.8F.F4.BA.D8.AE.D6.8B.F3.82.C0.9B.E4.8E.B0.AE.F0.B8.9B.80
				
.SPELL.SHOCK_SPHERE.SHAPE_2.AND_MASK			.HS	8F.84.A0.95.80.C0.80.80.80.D0.80.C0.89.80.80.C0.81.80.80.80.83.90.8F.80.83.C0.80.80.83.80.80.80
.SPELL.SHOCK_SPHERE.SHAPE_2.ORA_MASK			.HS	C0.E0.86.80.94.8A.DC.AE.F3.83.D0.8B.C0.AE.C6.8B.D0.A3.F4.E2.D0.83.C0.AE.D0.8B.D4.A2.D0.A8.8C.B3
				
.SPELL.SHOCK_SPHERE.SHAPE_3.AND_MASK			.HS	00.7F.03.60.00.00.00.40.00.00.03.00.0B.40.00.50.03.00.00.50.00.40.00.00.00.01.00.40.0B.50.3F.01
.SPELL.SHOCK_SPHERE.SHAPE_3.ORA_MASK			.HS	94.80.D0.82.C4.9A.D6.8B.F4.BA.E0.AE.C0.8B.F4.83.D0.AF.FC.82.C6.8B.E3.AE.90.98.F4.8C.C0.80.80.A8

@END

	.EP ;end patch mode
;--------
GE.SPELL.SHOCK_SPHERE.END ;marks the end address of the code block for this spell
@END
;=======================

;**OPT** Disk Space. Mass shock sphere and fireball could be consolidated. The only difference is the shape table used which could be setup as a pointer parm. Additionally,
;**update** Mass shock sphere now doesn't draw the corneres of the square pattern to make it look more round. To consolidate mass shock sphere and fireball this functionality would need to be 
;tied to a parameter. 
; 
;note: spells like magic missle and regular shock sphere could be combined too; the situation is the same.

GE.SPELL.MASS_SHOCK_SPHERE.START ;marks the start address of the code block for this spell
;--------
@START
	.PH SPELL.FILE.CODE_BLOCK.BUFFER.START ;start patch 

GE.SPELL.MASS_SHOCK_SPHERE ;requires GE.EXPLOSION
@START
;PARAMETERS: FP.SOURCE.GMAP.X, FP.SOURCE.GMAP.Y, FP.TARGET.GMAP.X, FP.TARGET.GMAP.Y
;ENTRANCE: DIRECT		
;RETURN: targets hit (COMBAT.TARGET_HIT.DB)

;=====================SUBROUTINE DOCUMENTATION====================================
;
;-target hits
;COMBAT.TARGET_HIT.DB is populated by a call to .TARGET.HIT.CHECK from GE.EXPLOSION
;
;-Medium Fireball
; LDA #$E6 ;90%  ($E6 = 90%)
; STA COMBAT.EXPLOSION.PROB ;set probability for first explosion "ring"	
;
; LDA #$80 ;-50%   ($40 = 25%, $80 = 50%)
; STA COMBAT.EXPLOSION.PROB.DEC ;set decrement amount per explosion "ring"	
;
; LDA #TILE_ID.FIRE_A ;parameter: tile_type of explosion
; STA COMBAT.EXPLOSION.TILE_TYPE
;
; LDA #$02 ;radius
;JSR .GE.EXPLOSION
;
;-Large Fireball
; LDA #$E6 ;90%  ($E6 = 90%)
; STA COMBAT.EXPLOSION.PROB ;set probability for first explosion "ring"	
;
; LDA #$80 ;-50%   ($40 = 25%, $80 = 50%)
; STA COMBAT.EXPLOSION.PROB.DEC ;set decrement amount per explosion "ring"	
;
; LDA #TILE_ID.FIRE_A ;parameter: tile_type of explosion
; STA COMBAT.EXPLOSION.TILE_TYPE
;
; LDA #$03 ;radius
;JSR .GE.EXPLOSION
;=================================================================================

		
.INIT
	;ACC = parameter
	STA COMBAT.EXPLOSION.RADIUS

	JSR .LOAD.SHAPE.TABLE
	
;FIRE PROJECTILE
			
		;LDA #$00  ;disable (support was added for this)
		LDA #COMBAT.PROJECTILE.DELAY	;(time target = 1.6 seconds when there are 7 empty tiles between source and target)
		STA SHAPE.MOVE.DELAY_LENGTH ;The length of the delay between the shape draw and shape erase.

		LDA #$00 ;set off
		STA SHAPE.MOVE.SCREEN_ANIMATION.TOGGLE ;($00 = screen animation on | >=$01 = screen animation off)
		STA SHAPE.MOVE.ERASE.TOGGLE	;($00 = shape erase on | >=$01 = shape erase off)		
		LDA #$01	;set mode to angled shape ;($00 = angled static shape | >=$01 = non-angled, animated shape)
			;STA SHAPE.MOVE.SCREEN_ANIMATION.TOGGLE ;($00 = screen animation on | >=$01 = screen animation off)
	JSR COMBAT.FIRE.PROJECTILE


	
;DRAW EXPLOSION

	;refresh shape table
	;(note: this needs to be done because on sharp angles COMBAT.FIRE.PROJECTILE or its subroutines modify the contents
	;of SHAPE_HOPPER0+$0-SHAPE_HOPPER0+$1F for some reason. I'm not sure why, but refreshing the shape tables resolve the issue for this routine.)
	;JSR .LOAD.SHAPE.TABLE
	JSR .LOAD.SHAPE.TABLE

		LDA #$FF ;($E6 = 90%, $C0 = 75%)
		STA COMBAT.EXPLOSION.PROB ;set probability for first explosion "ring"	

		LDA #$00 ;($40 = -25%, $80 = -50%)
		STA COMBAT.EXPLOSION.PROB.DEC ;set decrement amount per explosion "ring"	

		LDA #TILE_ID.FIRE_A ;parameter: tile_type of explosion
		;LDA #$9F ;COW
		STA COMBAT.EXPLOSION.TILE_TYPE

		LDA COMBAT.EXPLOSION.RADIUS
	JSR .GE.EXPLOSION

	
.SET.CRITICAL_HIT.FLAGS
@START
;(engaged s_entity screen index)

		;**warning: active player character sheet will be loaded on exit
		LDY COMBAT.ATTACKER.SINDEX
		LDA #COMBAT.STATS.CRTL_HIT.NOT_ENGAGED_VALUE ;set value to update.
.UPDATE.CHR_SHEET.ENGAGED.INDEX ;**OPT** Memory. This routine was written as as subroutine, but is being used inline here and it could be made more efficient. 
;PARAMETERS: YREG = screen index of attacker, ACC = value to write to CHR_SHEET.PC_MOB.ENGAGED.SINDEX
;ENTRANCE: direct
;RETURN: saved attacker character sheet with updated CHR_SHEET.PC_MOB.ENGAGED.SINDEX field

.SAVE.PARAMETERS.ENAGED
	;ACC = value to write to CHR_SHEET.PC_MOB.ENGAGED.SINDEX
	STA COMBAT.ENGAGED_SINDEX.UPDATE.PARM
	
	;read attacker (MOB) character sheet
		
		;YREG = COMBAT.ATTACKER.SINDEX
		LDA #$00 ;set read mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet

			
	;set the engaged field to the updated value
	;(expected to be the screen index of the defender or $FF to indicate no engagement)
	LDA COMBAT.ENGAGED_SINDEX.UPDATE.PARM
	STA CHR_SHEET.PC_MOB.ENGAGED.SINDEX

			
		;YREG = COMBAT.ATTACKER.SINDEX
		LDA #$01 ;set write mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet

@END


.EXIT
		
		
	RTS


@END

.LOAD.SHAPE.TABLE
@START
;PARAMETERS: none
;ENTRANCE: direct

;load the shape into the hoppers
	LDX #$00
.LOAD.SHAPE_DATA.LOOP0
	LDA .SHAPE_TABLE.SHOCK_SPHERE.START,X	
	STA SHAPE.HOPPER0,X
	INX
	BNE .LOAD.SHAPE_DATA.LOOP0

	RTS
@END

;SHAPE TABLE: SHOCK_SPHERE
@START
.SHAPE_TABLE.SHOCK_SPHERE.START
.SPELL.SHOCK_SPHERE.SHAPE_0.AND_MASK			.HS	80.F0.80.F0.80.C0.8C.80.80.C0.80.80.83.80.80.C0.80.C0.80.80.83.D0.80.C0.80.80.82.C0.80.C0.82.F4
.SPELL.SHOCK_SPHERE.SHAPE_0.ORA_MASK			.HS	C4.82.D0.83.F3.8A.C0.9B.91.8E.DC.BA.F0.AE.F4.8B.DC.8E.F4.A2.D0.83.F4.8E.9C.BA.C0.8E.CD.8B.90.80
				
.SPELL.SHOCK_SPHERE.SHAPE_1.AND_MASK			.HS	7F.7F.00.00.00.40.00.00.01.00.00.00.00.40.00.00.01.00.00.40.00.60.08.00.00.40.03.00.00.00.00.47
.SPELL.SHOCK_SPHERE.SHAPE_1.ORA_MASK			.HS	80.80.DC.B0.F4.8E.DC.8B.D8.E6.90.8C.C3.8F.F4.BA.D8.AE.D6.8B.F3.82.C0.9B.E4.8E.B0.AE.F0.B8.9B.80
				
.SPELL.SHOCK_SPHERE.SHAPE_2.AND_MASK			.HS	8F.84.A0.95.80.C0.80.80.80.D0.80.C0.89.80.80.C0.81.80.80.80.83.90.8F.80.83.C0.80.80.83.80.80.80
.SPELL.SHOCK_SPHERE.SHAPE_2.ORA_MASK			.HS	C0.E0.86.80.94.8A.DC.AE.F3.83.D0.8B.C0.AE.C6.8B.D0.A3.F4.E2.D0.83.C0.AE.D0.8B.D4.A2.D0.A8.8C.B3
				
.SPELL.SHOCK_SPHERE.SHAPE_3.AND_MASK			.HS	00.7F.03.60.00.00.00.40.00.00.03.00.0B.40.00.50.03.00.00.50.00.40.00.00.00.01.00.40.0B.50.3F.01
.SPELL.SHOCK_SPHERE.SHAPE_3.ORA_MASK			.HS	94.80.D0.82.C4.9A.D6.8B.F4.BA.E0.AE.C0.8B.F4.83.D0.AF.FC.82.C6.8B.E3.AE.90.98.F4.8C.C0.80.80.A8

@END


.GE.EXPLOSION
@START
;PARAMETERS: ACC (radius of explosion), COMBAT.EXPLOSION.TILE_TYPE,  COMBAT.EXPLOSION.PROB,  COMBAT.EXPLOSION.PROB.DEC
;ENTRANCE: DIRECT
;RETURN: 

.INIT.EXPLOSION
	;ACC = parameter
	STA COMBAT.EXPLOSION.RADIUS

	; ;init target_hit database
	; LDX #$00 ;init COMBAT.TARGET_HIT.DB index
	; STX COMBAT.TARGET_HIT.DB.INDEX
; .INIT.LOOP
	; LDA #$FF ;use init value that isn't ever used as a screen array index (screen tile location)
	; STA COMBAT.TARGET_HIT.DB,X
	; INX
	; CPX #COMBAT.TARGET_HIT.DB.SIZE
	; BNE .INIT.LOOP
		
	LDY SHAPE.SINDEX.TARGET
	
	STY COMBAT.EXPLOSION.CENTER
	;STY COMBAT.EXPLOSION.RUN.START_TILE	
	
	LDA #$00
	STA COMBAT.EXPLOSION.RADIUS.COUNTER
	;STA COMBAT.EXPLOSION.SCREEN_EDGE.COLLISION
	;STA COMBAT.EXPLOSION.SCREEN_EDGE.RIGHT
	
	LDA SCREEN.INDEX.COLUMN,Y ;load tile column # of target 
	STA COMBAT.EXPLOSION.START.COLUMN

	LDA SCREEN.INDEX.ROW,Y ;load tile column # of current algorithm tile
	STA COMBAT.EXPLOSION.START.ROW

			
.DRAW.TARGET_TILE ;draws an explosion tile at the explosion center 
	
		LDA COMBAT.EXPLOSION.PROB
		;STA SAVED.ACC.LOCAL ;save probability 
		PHA ;save probability 

			; lda #$aa
			; ldx COMBAT.EXPLOSION.PROB
			; jsr prep.brk
			; brk
			
		;temporarily adjust probability so that the explosion center tile always draws
		LDA #$FF ;set probability to 100% (there is an override that treats $FF as 100% probability)
		STA COMBAT.EXPLOSION.PROB
	JSR .DRAW.EXPLOSION.ENTRANCE

	PLA ;restore probability
	;LDA SAVED.ACC.LOCAL ;restore probability
	STA COMBAT.EXPLOSION.PROB
			

			
	;check for zero radius setting
	LDA COMBAT.EXPLOSION.RADIUS
	BNE .EXPLOSION.LOOP
	JMP .ERASE.EXPLOSION ;if zero radius then exit after drawin explosion on the target tile
	
.EXPLOSION.LOOP

			

.INCREMENT.START_TILE
;LOOP EXIT CHECK
	LDA COMBAT.EXPLOSION.RADIUS.COUNTER
	CMP COMBAT.EXPLOSION.RADIUS ;is COMBAT.EXPLOSION.RADIUS.COUNTER >= COMBAT.EXPLOSION.RADIUS?
	BCS .EXPLOSION.DRAW.COMPLETE_STEP ;if yes, then the explosion draw is complete. exit. 	
	;**FALLS THROUGH** ;if no, then draw the next "ring" of the explosion. 

;INCREMENT COUNTERS & INDEXES	
	INC COMBAT.EXPLOSION.RADIUS.COUNTER ;increment each time the start tile is moved up one position, and that wides the radius of the current run 

	LDA COMBAT.EXPLOSION.START.COLUMN
	CLC
	ADC COMBAT.EXPLOSION.RADIUS.COUNTER
	STA COMBAT.EXPLOSION.RUN_RIGHT.STOP_VALUE

	LDA COMBAT.EXPLOSION.START.ROW
	SEC
	SBC COMBAT.EXPLOSION.RADIUS.COUNTER
	STA COMBAT.EXPLOSION.RUN_UP.STOP_VALUE ;set to the start row + COMBAT.EXPLOSION.RADIUS.COUNTER

	LDA COMBAT.EXPLOSION.START.ROW
	CLC
	ADC COMBAT.EXPLOSION.RADIUS.COUNTER
	STA COMBAT.EXPLOSION.RUN_DOWN.STOP_VALUE ;set to the start row + COMBAT.EXPLOSION.RADIUS.COUNTER
	
	LDA COMBAT.EXPLOSION.START.COLUMN
	SEC
	SBC COMBAT.EXPLOSION.RADIUS.COUNTER
	STA COMBAT.EXPLOSION.RUN_LEFT.STOP_VALUE ;set to the start column - COMBAT.EXPLOSION.RADIUS.COUNTER

	;set exclude stop value (only used on outter ring)
	;(the ring position at which the explosion tiles will not be drawn for the rest of the run)
	;
	;(note: my vision for this was that the number of tiles excluded would increase with the radius, but
	;it didn't quite work right. By setting the exclusion stop value equal to the run stop value the result is that
	;just the 4 corner tiles of the explosion square are excluded.)
	;
	;**OPT** Memory. Disk space. Given the above note, this section isn't really  needed
	;the code below for each run could be modified so that the exclusion occurs if the
	;run stop value is reached. I left this code in place in case I decide to try to troubleshoot it later and
	;achieve the original effect envisioned. 
	
	LDA COMBAT.EXPLOSION.RUN_RIGHT.STOP_VALUE
	; SEC
	; SBC COMBAT.EXPLOSION.RADIUS ;deduct radius so that the larger the radius the more tiles are excluded in the outter ring of the explosion
	; CLC
	; ADC #$01 ;converts radius to radius-1
	STA COMBAT.EXPLOSION.RUN_RIGHT.EXCLUDE.STOP_VALUE

	LDA COMBAT.EXPLOSION.RUN_UP.STOP_VALUE 
	; CLC
	; ADC COMBAT.EXPLOSION.RADIUS ;deduct radius so that the larger the radius the more tiles are excluded in the outter ring of the explosion
	; SEC
	; SBC #$01 ;converts radius to radius-1	
	STA COMBAT.EXPLOSION.RUN_UP.EXCLUDE.STOP_VALUE ;set to the start row + COMBAT.EXPLOSION.RADIUS.COUNTER


	LDA COMBAT.EXPLOSION.RUN_DOWN.STOP_VALUE
	; SEC
	; SBC COMBAT.EXPLOSION.RADIUS ;deduct radius so that the larger the radius the more tiles are excluded in the outter ring of the explosion
	; CLC
	; ADC #$01 ;converts radius to radius-1	
	STA COMBAT.EXPLOSION.RUN_DOWN.EXCLUDE.STOP_VALUE


	LDA COMBAT.EXPLOSION.RUN_LEFT.STOP_VALUE ;set to the start column - COMBAT.EXPLOSION.RADIUS.COUNTER
	; CLC
	; ADC COMBAT.EXPLOSION.RADIUS ;deduct radius so that the larger the radius the more tiles are excluded in the outter ring of the explosion
	; SEC
	; SBC #$01 ;converts radius to radius-1
	STA COMBAT.EXPLOSION.RUN_LEFT.EXCLUDE.STOP_VALUE ;set to the start column - COMBAT.EXPLOSION.RADIUS.COUNTER	
.LAST_RING.CHECK.DONE	
	
	;screen edge exit test
	LDA SCREEN.INDEX.ROW,Y ;load tile row # of algorithm current tile location
	BEQ .START_TILE.INCREMENT.COMPLETE ;if in first row, then keep start tile in the same position
			
	;increment algorithm start tile location
	TYA ;transfer algorithm current tile
	SEC
	SBC #SCREEN.ARRAY.OFFSET
	TAY ;transfer current algorithm to Y-REG as screen index parameter
	JMP .DRAW.EXPLOSION.START

.EXPLOSION.DRAW.COMPLETE_STEP
	JMP .EXPLOSION.DRAW.COMPLETE
	
.START_TILE.INCREMENT.COMPLETE
	;JSR EXPLOSION.RADIUS.ADJUSTMENT
		

	; ;increment algorithm start tile location
	; TYA ;transfer algorithm current tile
	; CLC
	; ADC #SCREEN.ARRAY.OFFSET	
	; ;ACC = incremented start tile value
	
	;**FALLS THROUGH**

.DRAW.EXPLOSION.START			
;DRAW EXPLOSION IN ALGORITHM START TILE

		;Y-REG = incremented start tile value
	JSR .DRAW.EXPLOSION.ENTRANCE 
			
	;**FALLS THROUGH**
	
.RING.START ;each explosion ring has 5 runs. It's 5 instead of 4 because the top of the ring is split into two runs,
			;because the alogrithm start tile is in the center of the top of the ring. 
	
.RUN.LEFT
;INIT
	LDX #$00 ;init COMBAT.EXPLOSION.RUN.COUNTER
.RUN.LEFT.LOOP ;the run that is left of the start tile

	;screen edge exit check
	LDA SCREEN.INDEX.COLUMN,Y ;load tile column # of algorithm current tile location
	;BNE .RUN.LEFT.DRAW ;if not in first column, then continue this run
	;JSR EXPLOSION.RADIUS.ADJUSTMENT ;shorten radius to blast isn't expanded in direction opposite the screen edge
	BEQ .RUN.DOWN ;if in first column, then end this run
	;JMP .RUN.DOWN ;if in first column, then end this run

.RUN.LEFT.DRAW	
	
		
;DRAW EXPLOSION
		DEY ;left increment: current algorithm tile

	;is this iteration the outter ring?
	LDA COMBAT.EXPLOSION.RADIUS
	CMP COMBAT.EXPLOSION.RADIUS.COUNTER
	BNE .EXECUTE.DRAW.RUN_LEFT
	
	;is the current tile position at the exclusion stop value?
	;(the ring position at which the explosion tiles will not be drawn for the rest of the run)
	LDA COMBAT.EXPLOSION.RUN_LEFT.EXCLUDE.STOP_VALUE 
	CMP SCREEN.INDEX.COLUMN,Y ;load tile column # of current algorithm tile
	BCS .DRAW.EXPLOSION.RUN.LEFT.DONE
			
			; JSR KEYIN
			; LDA SCREEN.INDEX.COLUMN,Y ;load tile column # of current algorithm tile
			; LDX COMBAT.EXPLOSION.RUN_LEFT.EXCLUDE.STOP_VALUE 
			; JSR PREP.BRK
			; BRK
			
.EXECUTE.DRAW.RUN_LEFT			
	JSR .DRAW.EXPLOSION.ENTRANCE 
.DRAW.EXPLOSION.RUN.LEFT.DONE

	INX ;increment for each tile move in the current run. COMBAT.EXPLOSION.RUN.COUNTER 

	
	;run exit check
	LDA SCREEN.INDEX.COLUMN,Y ;load tile column # of current algorithm tile
	CMP COMBAT.EXPLOSION.RUN_LEFT.STOP_VALUE ;set to the start column - COMBAT.EXPLOSION.RADIUS.COUNTER	
	;CPX COMBAT.EXPLOSION.RADIUS.COUNTER ;is COMBAT.EXPLOSION.RUN.COUNTER (x-reg) = COMBAT.EXPLOSION.RADIUS.COUNTER
	BNE .RUN.LEFT.LOOP ;if yes, continue drawing in this run
				  ;if no, next run
				  
	;**FALLS THROUGH**


	
.RUN.DOWN
;INIT
	LDX #$00 ;reset COMBAT.EXPLOSION.RUN.COUNTER

.RUN.DOWN.LOOP

;CRASHES
	
			
	;screen edge exit check
	LDA SCREEN.INDEX.ROW,Y ;load tile row # of algorithm current tile location
	CMP #COMBAT.SCREEN.LAST_ROW
	;BNE .RUN.DOWN.DRAW ;if not in last row, then continue this run
	;JSR EXPLOSION.RADIUS.ADJUSTMENT ;shorten radius to blast isn't expanded in direction opposite the screen edge
	BEQ .RUN.RIGHT ;if in last row, then end this run
	;JMP .RUN.RIGHT ;if in last row, then end this run
	

.RUN.DOWN.DRAW
;DRAW EXPLOSION

		;down increment: current algorithm tile
		TYA
		CLC
		ADC #SCREEN.ARRAY.OFFSET
		TAY ;transfer incremented screen array index back to Y-REG		
	
	;is this iteration the outter ring?
	LDA COMBAT.EXPLOSION.RADIUS
	CMP COMBAT.EXPLOSION.RADIUS.COUNTER
	BNE .EXECUTE.DRAW.RUN_DOWN
	
	;is the current tile position at the exclusion stop value?
	;(the ring position at which the explosion tiles will not be drawn for the rest of the run)
	LDA SCREEN.INDEX.ROW,Y ;load tile column # of current algorithm tile
	CMP COMBAT.EXPLOSION.RUN_DOWN.STOP_VALUE ;set to the start row + COMBAT.EXPLOSION.RADIUS.COUNTER
	BCS .DRAW.EXPLOSION.RUN.DOWN.DONE
.EXECUTE.DRAW.RUN_DOWN	
	JSR .DRAW.EXPLOSION.ENTRANCE 
.DRAW.EXPLOSION.RUN.DOWN.DONE
		
	INX ;increment for each tile move in the current run. COMBAT.EXPLOSION.RUN.COUNTER 
	
	;run exit check
	LDA SCREEN.INDEX.ROW,Y ;load tile column # of current algorithm tile
	CMP COMBAT.EXPLOSION.RUN_DOWN.STOP_VALUE ;set to the start row + COMBAT.EXPLOSION.RADIUS.COUNTER
	;CPX COMBAT.EXPLOSION.RADIUS.COUNTER_X2 ;is COMBAT.EXPLOSION.RUN.COUNTER (x-reg) = COMBAT.EXPLOSION.RADIUS_X2
	BNE .RUN.DOWN.LOOP ;if no, continue drawing in this run
				  ;if yes, next run

	;**FALLS THROUGH**
				  
.RUN.RIGHT
;INIT
	LDX #$00 ;reset COMBAT.EXPLOSION.RUN.COUNTER

;CRASHES
			
.RUN.RIGHT.LOOP

	LDA SCREEN.INDEX.COLUMN,Y ;load tile column # of algorithm current tile location
	CMP #COMBAT.SCREEN.LAST_COLUMN
	;BNE .RUN.RIGHT.DRAW ;if not in last column, then continue this run
	; JSR EXPLOSION.RADIUS.ADJUSTMENT ;shorten radius to blast isn't expanded in direction opposite the screen edge
	; LDA #$01
	; STA COMBAT.EXPLOSION.SCREEN_EDGE.RIGHT
	BEQ .RUN.UP ;if in last column, then end this run

.RUN.RIGHT.DRAW			
;DRAW EXPLOSION

		INY ;right increment: current algorithm tile

	;is this iteration the outter ring?
	LDA COMBAT.EXPLOSION.RADIUS
	CMP COMBAT.EXPLOSION.RADIUS.COUNTER
	BNE .EXECUTE.DRAW.RUN_RIGHT
	
	;is the current tile position at the exclusion stop value?
	;(the ring position at which the explosion tiles will not be drawn for the rest of the run)
	LDA SCREEN.INDEX.COLUMN,Y ;load tile column # of current algorithm tile
	CMP COMBAT.EXPLOSION.RUN_RIGHT.STOP_VALUE ;set to the start column + COMBAT.EXPLOSION.RADIUS.COUNTER
	BCS .DRAW.EXPLOSION.RUN.RIGHT.DONE
.EXECUTE.DRAW.RUN_RIGHT	
	JSR .DRAW.EXPLOSION.ENTRANCE 
.DRAW.EXPLOSION.RUN.RIGHT.DONE

		
	INX ;increment for each tile move in the current run. COMBAT.EXPLOSION.RUN.COUNTER 
	
	;run exit check
	LDA SCREEN.INDEX.COLUMN,Y ;load tile column # of current algorithm tile
	CMP COMBAT.EXPLOSION.RUN_RIGHT.STOP_VALUE ;set to the start column + COMBAT.EXPLOSION.RADIUS.COUNTER
	
	;CPX COMBAT.EXPLOSION.RADIUS.COUNTER_X2 ;is COMBAT.EXPLOSION.RUN.COUNTER (x-reg) = COMBAT.EXPLOSION.RADIUS_X2
	BNE .RUN.RIGHT.LOOP ;if no, continue drawing in this run
				  ;if yes, next run

	;**FALLS THROUGH**

.RUN.UP
;INIT
	LDX #$00 ;reset COMBAT.EXPLOSION.RUN.COUNTER

.RUN.UP.LOOP

	
	;screen edge exit test
	LDA SCREEN.INDEX.ROW,Y ;load tile row # of algorithm current tile location
	;BNE .RUN.UP.DRAW ;if not in first row, then continue this run
	;JSR EXPLOSION.RADIUS.ADJUSTMENT ;shorten radius to blast isn't expanded in direction opposite the screen edge	
	BEQ .RUN.LEFT2 ;if in first row, then end this run
	;JMP .RUN.LEFT2 ;if in first row, then end this run


.RUN.UP.DRAW	
;DRAW EXPLOSION
	
		;down increment: curren algorithm tile
		TYA
		SEC
		SBC #SCREEN.ARRAY.OFFSET
		TAY ;transfer incremented screen array index back to Y-REG		

	;is this iteration the outter ring?
	LDA COMBAT.EXPLOSION.RADIUS
	CMP COMBAT.EXPLOSION.RADIUS.COUNTER
	BNE .EXECUTE.DRAW.RUN_UP
	
	;is the current tile position at the exclusion stop value?
	;(the ring position at which the explosion tiles will not be drawn for the rest of the run)
	LDA COMBAT.EXPLOSION.RUN_UP.STOP_VALUE ;set to the start row - COMBAT.EXPLOSION.RADIUS.COUNTER
	CMP SCREEN.INDEX.ROW,Y ;load tile column # of current algorithm tile
	BCS .DRAW.EXPLOSION.RUN.UP.DONE
.EXECUTE.DRAW.RUN_UP	
	JSR .DRAW.EXPLOSION.ENTRANCE 
.DRAW.EXPLOSION.RUN.UP.DONE
	
	INX ;increment for each tile move in the current run. COMBAT.EXPLOSION.RUN.COUNTER 
	
	;run exit check
	LDA SCREEN.INDEX.ROW,Y ;load tile column # of current algorithm tile
	CMP COMBAT.EXPLOSION.RUN_UP.STOP_VALUE ;set to the start row - COMBAT.EXPLOSION.RADIUS.COUNTER
	;CPX COMBAT.EXPLOSION.RADIUS.COUNTER_X2 ;is COMBAT.EXPLOSION.RUN.COUNTER (x-reg) = COMBAT.EXPLOSION.RADIUS_X2
	BNE .RUN.UP.LOOP ;if no, continue drawing in this run
				  ;if yes, next run

	;**FALLS THROUGH**

.RUN.LEFT2
;INIT
	;LDX #$00 ;reset COMBAT.EXPLOSION.RUN.COUNTER

.RUN.LEFT2.LOOP ;the run that is right of the start tile



	;INX ;increment for each tile move in the current run. COMBAT.EXPLOSION.RUN.COUNTER 

			
	;run exit check
	LDA SCREEN.INDEX.COLUMN,Y ;load tile row # of algorithm current tile location
	CMP COMBAT.EXPLOSION.START.COLUMN ;is current algorithm tile in or beyond the start tile column
	
	;CPX COMBAT.EXPLOSION.RADIUS.COUNTER ;is COMBAT.EXPLOSION.RUN.COUNTER (x-reg) = COMBAT.EXPLOSION.RADIUS.COUNTER	
	BEQ .RING.END ;if no, continue drawing in this run
	;BCC .RING.END ;if no, continue drawing in this run
				  ;if yes, next run

		DEY ;left increment: current algorithm tile
					  
		;YREG =
	JSR .DRAW.EXPLOSION.ENTRANCE 

	JMP .RUN.LEFT2.LOOP
	
	;**FALLS THROUGH**

; .RING.END.ENTRANCE1
	; DEY ;move current algorithm tile back to start position. 
		
;.RING.END.ENTRANCE2	
.RING.END	
	;JSR FLIP.PAGE ;display the explosion tiles drawn in the last iteration, representing a "ring" of the explosion

	;decrease probability for next explosion "ring"
	;(that an explosion will be drawn for any given tile)
	LDA COMBAT.EXPLOSION.PROB
	SEC
	SBC COMBAT.EXPLOSION.PROB.DEC
	STA COMBAT.EXPLOSION.PROB
	BCC .PROBABILITY.UNDERFLOW
	JMP .WAIT
	
.PROBABILITY.UNDERFLOW

		; LDA #$AA
		; LDX COMBAT.EXPLOSION.PROB ;set probability for first explosion "ring"	
		; LDY COMBAT.EXPLOSION.PROB.DEC ;set decrement amount per explosion "ring"	
		; JSR PREP.BRK
		; BRK	
		
	LDA #$81 ;set probability to a minimum of 20%
	STA COMBAT.EXPLOSION.PROB
	
.WAIT	
		;LDA #$06
		LDA #$A0		
	JSR WAIT.LOOP		

;CONTINUE LOOP
	JMP .EXPLOSION.LOOP
	
.EXPLOSION.DRAW.COMPLETE

	
.ERASE.EXPLOSION
	
			;JSR KEYIN
		
		LDA #$FF		
	JSR WAIT.LOOP
	
	JSR FLIP.PAGE
	
	;!!!!! BSR BANK1 !!!!
	JSR COPY.SCREEN.ENTRANCE ;**DON'T REMOVE** (or the screen goes chaotic sometimes when collisions occur with double mover mobs. MO.DRAW must need the graphics pages in sync for some reason). UDPATE: I think this is needed because sometimes, due to key press abort, no copy.screen was done after the last move, so the pages are out of syncing going into MOVE.PASS, which is triggered by non-movement commands like (3)ZAP, Push, Board, etc.

	
	;**FALLS THROUGH**
	
.GE.EXPLOSION.EXIT

	RTS

;EXPLOSION.RADIUS.ADJUSTMENT
@START

	; LDA COMBAT.EXPLOSION.SCREEN_EDGE.COLLISION ;has the algorithm already encountered a screen edge?
	; BNE .EXIT ;if yes exit. Only 1 adjustment is needed
	
	; ;adjust radius
	; ;(the father into the algorithm the screen edge was detected, the smaller the adjustmet to the radius is)
	; LDA COMBAT.EXPLOSION.RADIUS
	; SEC
	; SBC COMBAT.EXPLOSION.RADIUS.COUNTER	
	; STA TEMP
	; LDA COMBAT.EXPLOSION.RADIUS
	; SBC TEMP
	; STA COMBAT.EXPLOSION.RADIUS
	
			; ; LDA #$AA
			; ; LDX TEMP
			; ; ;LDX COMBAT.EXPLOSION.SCREEN_EDGE.COLLISION
			; ; LDY COMBAT.EXPLOSION.RADIUS
			; ; JSR PREP.BRK
			; ; BRK
			
	; ;set flag so this adjustment isn't repeated
	; LDA #$01
	; STA COMBAT.EXPLOSION.SCREEN_EDGE.COLLISION
	
; .EXIT
	; RTS
@END

.DRAW.EXPLOSION.ENTRANCE
@START
;PARAMETERS: YREG (screen index), COMBAT.EXPLOSION.PROB
;ENTRANCE: DIRECT
;RETURN: explosion graphics effect, updated COMBAT.TARGET_HIT.DB


;DRAW EXPLOSION ON THIS TILE?
;(some tiles are randomly skipped to vary the explosion pattern)

	;if probability is $FF, treat as 100% probability
	LDA COMBAT.EXPLOSION.PROB
	CMP #$FF
	BEQ .DRAW.CURRENT_TILE.START
	
		;JMP .DRAW.CURRENT_TILE.START
		
.PATHS_2
	JSR RANDOM.8
	CMP COMBAT.EXPLOSION.PROB		
	BCS .DRAW.EXPLOSION.EXIT	;don't draw explosion on this tile
	;**FALLS THROUGH**
	
;DRAW EXPLOSION ON CURRENT TILE
.DRAW.CURRENT_TILE.START
		; ;YREG = screen index
		; LDA #$01
		; STA PAGE.FOREGROUND.OVERRIDE
		; ;STA DRAW.TILE.SHAPE_TABLE.OVERRIDE ;($00 = load data into shape table from aux memory | $01 don't load data into shape table)
		; LDA COMBAT.EXPLOSION.TILE_TYPE
		; STA SAVED_TILE_TYPE 			
	; JSR DRAW.TILE.SINGLE

	;JSR .DRAW.TILE.AND_ORA
	;JSR .DRAW.SHAPE.ON.TARGET
	JSR .DRAW.EXPLOSION.SHAPE
	
	JSR .TARGET.HIT.CHECK
	
.DRAW.EXPLOSION.EXIT

	RTS
	
@END

@END

.DRAW.EXPLOSION.SHAPE
@START
;PARAMETERES: Y-REG (screen index of tile location)
;ENTRANCE: direct

		
		;set parameters
			
		;YREG = screen index for tile location	
		LDA SCREEN.INDEX.TILE_SBYTE,Y
		STA SHAPE.DRAW.CURRENT.SBYTE
		
		LDA SCREEN.INDEX.TILE_LINE,Y
		STA SHAPE.DRAW.LINE_ROW.NUMBER
		
		;Y-REG = screen tile lcation of target
		LDA #$01 ;set foreground page mode
		STA PAGE.FOREGROUND.OVERRIDE
		LDA #$00 ;set draw mode
	JSR .DRAW.TILE.AND_ORA
	
	RTS


@END

.DRAW.TILE.AND_ORA ;draw single tile using AND + ORA mask
@START
;PARAMETERS:  	;ACC = ($00 = draw mode | $01 = erase mode), SHAPE.DRAW.LINE_ROW.NUMBER, SHAPE.DRAW.CURRENT.SBYTE, [PAGE.FOREGROUND.OVERRIDE]
;RETURN: NONE
;ENTRANCE: .DRAW.SHAPE.ON.TARGET
;*Set to default value on game launch. When modified (i.e. tall grass), it is reset to default when done.

;=====================SUBROUTINE DOCUMENTATION====================================
;This subroutine, COMBAT.SHAPE.DRAW and DRAW.TILE are the lowest level routines associated with tile graphics plotting. 
;See DRAW.TILE for general documentation on the methodology used to plot to the graphics screen.
;
;This subroutine's unique purpose is to be able to draw a single tile using an AND+ORA mask. 
;
;This subroutine is specifically designed to draw shapes which are objects drawn on top of a background. The underlying
;terrain can be supplied by terrain tiles or other object tiles. However, this routine can draw over any 14x16 section
;the hi-res screen whether it conforms to the tile grid or not.
;
;To blend shapes into the background, two shape tables are used. One containing and AND mask and the other containing
;an ORA mask.
;
;here is a quick summary of the concept:
;(also see the first two posts from Michael Pohoreski in the following
;post https://groups.google.com/forum/#!topic/comp.sys.apple2.programmer/cgxZOBIL728)
;
; byte1: 1111110
; byte2: 0000010
;
; Using AND on byte 1 and byte 2 results in:
;
; result: 0000010
;
; (i.e. each bit is assigned the value 1 only if both byte1 AND byte2 have a 1 in that bit position.
;
;
; OR works similarly except that a 1 is assigned to each bit position in the result if either byte1 OR byte2 has a 1 in that bit position. 
;
;
; The technique works like this:
;
; before drawing the shape copy the background, and apply an AND value to each byte. The AND values has "0" values where the shape will be. The result is a shape table that has the background and a small "hole" carved out where the shape will go.
;
; Then apply an OR value to each byte. the OR values have "1" values where the shape is. The result is shape table with 1s where either the background or the shape is. This is different from simply drawing the shape on top of the background because when the "hole" is carved out, a two pixel buffer is put around the shape so that the background pixels and the shape pixels don't interact to create unwanted lines and colors. 
;=================================================================================
	
	;ACC = $00 = draw mode | $01 = erase mode
	STA DRAW.SHAPE.MODE

;**OPT** Speed. Include an in-line code version of DRAW.TILE with DRAW.COLUMN and DRAW.ROW to avoid the costly JSR/RTS to call as a subroutine. 

.SAVE.REGISTERS
	TXA
	PHA
	TYA
	PHA

.START	
	LDX SHAPE.DRAW.LINE_ROW.NUMBER			;LOAD LINE IN X REGISTER	
	TXA ;transfer SHAPE.DRAW.LINE_ROW.NUMBER to ACC
	STA TILE.LINE.CNTR 
	STA TILE.LINE.START
	CLC						;*OPT* I I THINK ALL CLS CAN BE REMOVED WHEN I'M NOT EXPECTING AN OVERFLOW
	ADC #TILE.DEPTH.STANDARD				;ADD DEPTH OF SHAPE TO STARTING LINE IN HEX (# of lines, not the position of last line....so line positions $0-$F is $10 (!16) total lines)
	STA TILE.LINE.STOP		;SET THE LAST LINE TO DRAW BASED ON THE DEPTH OF THE TILE

	LDA #$00
	STA SHP.TBL.CNTR		;START DRAWING AT BEGINNING OF SHAPE TABLE
	
	
.DRAW.LOOP

;GET LINE ADDRESS
@START

;==========INLINE CODE GET.LINE.ADDRESS1==================	
	LDA PAGE.FOREGROUND.OVERRIDE	;CHECK TO SEE IF CALLING ROUTINE WANTS ERASE DONE TO FOREGROUND INSTEAD OF BACKGROUND (DEFAULT)
	CMP #$01
	BEQ .FOREGROUND_OVERRIDE
	
	LDA PAGE.BACKGROUND		
	CMP #$02
	BEQ .LOOKUP.PAGE2
	JMP .LOOKUP.PAGE1
	
.FOREGROUND_OVERRIDE
	LDA PAGE.FOREGROUND		;SPECIFY PAGE TO GET ADDRESS FOR (SHOULD BE BACKGROUND PAGE)
	CMP #$02
	BEQ .LOOKUP.PAGE2
	

.LOOKUP.PAGE1	
	LDA LINE.HO.P1,X		;GET LINE ADDRESS
	STA LINE.BASE.ADDR1+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR1
	JMP .GETLINE_COMPLETE

.LOOKUP.PAGE2
	LDA LINE.HO.P2,X		;GET LINE ADDRESS
	STA LINE.BASE.ADDR1+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR1
.GETLINE_COMPLETE
;========================================================
@END
	
.DRAW_ERASE.ENTRANCE
	LDA DRAW.SHAPE.MODE ;ACC = $00 = draw mode | $01 = erase mode
	;BNE .ERASE.MODE
	BEQ .DRAW.MODE
	JMP .ERASE.MODE
	;**FALLS THROUGH**
	
.DRAW.MODE	
;screen byte 0
	LDY SHP.TBL.CNTR		;LOAD Y WITH SHAPE TABLE COUNTER	
	;init counters/indexes
	TYA ;init X-REG to $00 as the SHAPE.EXISTING_DATA.BUFFER index
	TAX ;""
	

	;save existing graphics data
	LDY SHAPE.DRAW.CURRENT.SBYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	LDA (LINE.BASE.ADDR1),Y	;load existing data from video memory
	STA SHAPE.EXISTING_DATA.BUFFER,X
			;STA TEMP
	;load new shape data
	;ACC = existing graphics data
	LDY SHP.TBL.CNTR		;LOAD Y WITH SHAPE TABLE COUNTER	
	AND (SHAPE.BACKGROUND.AND_MASK),Y
	ORA (SHAPE.COMPOSITE.ORA_MASK),Y

			; STA TEMP
			; LDA TEST.ITERATION.COUNTER
			; CMP #$02
			; BNE .TEMP
			; LDA #$AA
			; LDX SHAPE.DRAW.LINE_ROW.NUMBER
			; LDY SHAPE.DRAW.CURRENT.SBYTE
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP
			
	;write new shape data
	LDY SHAPE.DRAW.CURRENT.SBYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	STA (LINE.BASE.ADDR1),Y	;PLOT (1st screen byte)
	
;next screen byte
	INC SHP.TBL.CNTR		;NEXT SHAPE BYTE
	INX	;next existing data buffer byte
	INC SHAPE.DRAW.CURRENT.SBYTE			;SWITCH TO 2ND SCREEN BYTE IN THE TILE	
	
;screen byte 1
	
	;save existing graphics data
	LDY SHAPE.DRAW.CURRENT.SBYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	LDA (LINE.BASE.ADDR1),Y	;load existing data from video memory
	STA SHAPE.EXISTING_DATA.BUFFER,X
		
	;load new shape data
	;ACC = existing graphics data
	LDY SHP.TBL.CNTR		;LOAD Y WITH SHAPE TABLE COUNTER	
	AND (SHAPE.BACKGROUND.AND_MASK),Y
	ORA (SHAPE.COMPOSITE.ORA_MASK),Y
		
	;write new shape data
	LDY SHAPE.DRAW.CURRENT.SBYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	STA (LINE.BASE.ADDR1),Y	;PLOT (1st screen byte)
	JMP .NEXT.LINE
	
.ERASE.MODE
;screen byte0
	LDY SHP.TBL.CNTR		;LOAD Y WITH SHAPE TABLE COUNTER
	LDA SHAPE.EXISTING_DATA.BUFFER,Y			;LOAD SHAPE BYTE (1st screen byte of the tile)

	LDY SHAPE.DRAW.CURRENT.SBYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	STA (LINE.BASE.ADDR1),Y	;PLOT (1st screen byte)

;next screen byte	
	INC SHP.TBL.CNTR		;NEXT SHAPE BYTE, NEXT TILE LINE
	INC SHAPE.DRAW.CURRENT.SBYTE			;SWITCH TO 2ND SCREEN BYTE IN THE TILE

;screen byte1	
	LDY SHP.TBL.CNTR		;LOAD Y WITH SHAPE TABLE COUNTER
	LDA SHAPE.EXISTING_DATA.BUFFER,Y			;LOAD SHAPE BYTE (2nd screen byte of the tile)
	LDY SHAPE.DRAW.CURRENT.SBYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	STA (LINE.BASE.ADDR1),Y	
	;**FALLS THROUGH**
	
.NEXT.LINE
;next line, reset screen byte	
	DEC SHAPE.DRAW.CURRENT.SBYTE			;SWITCH BACK TO 1ST SCREEN BYTE IN THE TILE	;**OPT** if the counters are updated after the CMP/branch it might save 1 INC SHAPE.DRAW.CURRENT.SBYTE instruction in the main loop because SHAPE.DRAW.CURRENT.SBYTE will be in 2nd position when this loop ends. 
	INC SHAPE.DRAW.LINE_ROW.NUMBER
	
	INC SHP.TBL.CNTR		;NEXT SHAPE BYTE
	INC TILE.LINE.CNTR		;NEXT TILE LINE
	LDX TILE.LINE.CNTR 		;restore tile line number to X-REG

;exit test	
	CPX TILE.LINE.STOP		;IS TILE DONE?							
	BCS .DONE				
	JMP .DRAW.LOOP
.DONE	
	LDA #$00
	STA PAGE.FOREGROUND.OVERRIDE
				
.RESTORE.REGISTERS
	PLA
	TAY
	PLA
	TAX
	
	RTS

@END	

.TARGET.HIT.CHECK
@START
;PARAMETERS: YREG (screen index), COMBAT.EXPLOSION.PROB

;SAVE PARAMETERS
	TXA
	PHA
	
;WAS A TARGET PRESENT?				
	;YREG = screen index
	LDA SCREEN.MO_SPRITE.DATA,Y
	CMP #$FF 
	BEQ .CHECK.TARGET_HIT.EXIT ;if no S_ENTITY is present at this screen location then exit, don't record a target hit

	
; ;MTT CHECK

		; ;is S_ENTITY a tile of a multi-tile mob other than the index tile (upper left tile)?
		; ;(the upper left tile is the only tile of a multi-tile mob that is a valid character sheet index)
		; ;Y-REG: search loop index
	; JSR COMBAT.TARGET.SEARCH.MTT.CHECK
		; ; ;RETURN VALUE: ACC = ($00 =  MTT index tile found | $01 = MTT not found at this screen location | $02 = MTT tile found other than index tile)
		; CMP #$02 ;i S_ENTITY found a non-index tile of an MTT S_ENTTY? (i.e. a tile other than the upper left of the MTT)
		; BEQ	.CHECK.TARGET_HIT.EXIT	;skip this S_ENTITY
	
;IS THE HIT TILE THE SPELLCASTER?
	CPY COMBAT.ATTACKER.SINDEX
	BEQ .CHECK.TARGET_HIT.EXIT
	
;IS THIS A DUPLICATE HIT?
;(note: this routine is needed because the iteration patter of some spells results in tiles being examined more than once, which could result
;in this routine being called more than once for the same tile)

	LDX #$00
	TYA ;transfer screen tile location to ACC
.DUPLICATE.CHECK.LOOP
	CMP COMBAT.TARGET_HIT.DB,X ;if S_ENTITY in current screen location was already recorded as a hit, then exit
	BEQ	.CHECK.TARGET_HIT.EXIT
	INX
;	CPX #COMBAT.TARGET_HIT.DB.STOP_VALUE
	CPX #COMBAT.TARGET_HIT.DB.SIZE
	BNE .DUPLICATE.CHECK.LOOP
		
	
;RECORD NEW TARGET HIT	
	;YREG = screen index
	TYA
	LDX COMBAT.TARGET_HIT.DB.INDEX
	STA COMBAT.TARGET_HIT.DB,X
	;write stop value
	;(note: each time a new target hit is recorded the stop value is advanced 1 byte in the array)
	LDA #COMBAT.TARGET_HIT.DB.STOP_VALUE
	STA COMBAT.TARGET_HIT.DB+$1,X

			; ;JSR KEYIN
			; STA TEMP
			; CMP #$D1
			; BNE .TEMP
			; ;LDA #$AA
			; LDA COMBAT.TARGET_HIT.DB,X
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP

	INC COMBAT.TARGET_HIT.DB.INDEX	;next target_hit record

.CHECK.TARGET_HIT.EXIT

;RESTORE PARAMETERS
	PLA
	TAX
			
	RTS	
@END

;LOCAL VARIABLES
@START
COMBAT.EXPLOSION.RUN_RIGHT.EXCLUDE.STOP_VALUE	.BS $1
COMBAT.EXPLOSION.RUN_UP.EXCLUDE.STOP_VALUE 		.BS $1
COMBAT.EXPLOSION.RUN_DOWN.EXCLUDE.STOP_VALUE	.BS $1
COMBAT.EXPLOSION.RUN_LEFT.EXCLUDE.STOP_VALUE	.BS $1

@END


	.EP ;end patch mode
;--------
GE.SPELL.MASS_SHOCK_SPHERE.END ;marks the end address of the code block for this spell
@END
;=======================
;----------END DAMAGE/SUMMON SPELLS----

GE.SPELL.COMBAT_TELEPORT_PARTY.START ;marks the start address of the code block for this spell
;--------
@START
	.PH SPELL.FILE.CODE_BLOCK.BUFFER.START ;start patch 

GE.SPELL.COMBAT_TELEPORT_PARTY
@START
;PARAMETERS: none
;ENTRANCE: DIRECT		
;RETURN: updated PC GMAP X/Y and updated video screen
; 

;=====================SUBROUTINE DOCUMENTATION====================================
;
;no targets
;=================================================================================


			
;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA
	
.INIT
	LDA #$00
	STA TELEPORT.ACTIVE_PC.MOVE.FLAG ;($00 = not moved this iteration | moved this iteration)


.TELEPORT_MOVES_MAX	.EQ $03	;#CONSTANT. Specifies the max number of moves north that this spell will teleport the PCs


	LDY #$00 ;init loop counter
.MOVE.LOOP
;SAVE REGISTERS
	TYA
	PHA
	
	LDX #$00
	STX TELEPORT.LOOP.COUNTER
.TELEPORT.LOOP

	
	;get current PC's X/Y axis position
	LDA COMBAT.MAP_OBJECTS.PC+$0,X ;Load COLUMN (X-AXIS) for current PC record
	STA	PARM.GMAP.X
	LDA COMBAT.MAP_OBJECTS.PC+$1,X ;Load ROW (Y-AXIS) for current PC record
	STA PARM.GMAP.Y
	
			; STA TEMP
			; ; LDA TROUBLESHOOTING.HOOK
			; ; CMP #$01
			; CPX #$04
			; BNE .TEMP
			; LDA #$AA
			; LDY TEMP
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP
			
.TEST.MOVE.NORTH
	;ACC: current PC Y-AXIS (current position, before move north)
	CMP #$00 ;is PC in 1st row (upper/northern screen edge)
	;BEQ .NEXT.PC ;branch if yes
	bne .cow
	jmp	.NEXT.PC ;branch if yes
.cow
	
	DEC PARM.GMAP.Y ;adjust current PC's X/Y axis to 1 move north of current position
	LDA PARM.GMAP.Y
	STA TELEPORT.SINDEX.ADJ ;save Y-AXIS (adjusted) because PARM.GMAP.Y gets modified by CONVERT.COLUMN_ROW.TO.SINDEX
	
		;parm: PARM.GMAP.X (set above)
		;parm: PARM.GMAP.Y (set above)
	JSR CONVERT.COLUMN_ROW.TO.SINDEX
		STY SUMMON.TARGET.SINDEX ;variable borrowed from summon routine
		LDA #MAP_OBJECT.FLAG.AGGR ;this flag value will trigger the collision routine to check the footprint of a single tile object
		STA SUMMON.PARM.FLAGS
	JSR .COLLISION.CHECK
		LDA .COLLISION.RETURN_VALUE ;($00 = screen location open, $01 = screen location blocked)
		CMP #$01 ;is move blocked?
 		BEQ .NEXT.PC 
			
.EXECUTE.MOVE
	;save updated Y-AXIS value
	LDA TELEPORT.SINDEX.ADJ ;restore Y-AXIS (adjusted)	
	STA COMBAT.MAP_OBJECTS.PC+$1,X ;Load ROW (Y-AXIS) for current PC record


.UPDATE.VIDEO_SCREEN
		;Y-REG: screen tile location of the target S_ENTITY (after move North)

;DRAW PC
;(position after current move)	
		LDA COMBAT.MAP_OBJECTS.PC+$2,X ;Load TILE_ID for current PC record
		STA SAVED_TILE_TYPE
		PHA ;push current PC's tile_id to the stack
		LDA #$01 ;set foreground override
		STA PAGE.FOREGROUND.OVERRIDE
		;Y-REG: screen tile location of the target S_ENTITY (after move North)
	JSR DRAW.TILE.SINGLE
		;background set by default
		PLA ;pull current PC's tile_id from stack
		;Y-REG: screen tile location of the target S_ENTITY (after move North)
	JSR DRAW.TILE.SINGLE
			
	;update screen arrays
	TXA
	STA SCREEN.MO_SPRITE.DATA,Y ;save map object index of current PC
	LDA #S_ENTITY_TYPE.PC
	STA SCREEN.MO_SPRITE_TYPE.DATA,Y ;save S_ENTITY type as PC
			
;ERASE PC'S TILE 
;(position before current move)
		
		;move sindex down 1 tile (the position before the current move)
		TYA
		CLC
		ADC #SCREEN.ARRAY.OFFSET
		TAY
		
		
		LDA #$01 ;set foreground override
		STA PAGE.FOREGROUND.OVERRIDE
		;Y-REG: screen tile location of the target S_ENTITY before move North)
	JSR DRAW.TILE.SINGLE
		;background set by default
		;Y-REG: screen tile location of the target S_ENTITY before move North)
	JSR DRAW.TILE.SINGLE


	;erase from screen arrays
	lda #$FF ;screen array init value
	STA SCREEN.MO_SPRITE.DATA,Y 
	STA SCREEN.MO_SPRITE_TYPE.DATA,Y

	
			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; cpy #$01
			; lda TELEPORT.LOOP.COUNTER
			; cmp #$05
			; bne .temp
			; LDA #$AA
			; ldx TELEPORT.ACTIVE_PC.MOVE.FLAG	;($00 = not moved this iteration | moved this iteration)
			; ;LDy COMBAT.ATTACKER.SINDEX
			; ldy COMBAT.PC.ACTIVE
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP
			
	;is current PC = active PC?
	;(is the PC being processed in current loop iteration the same as the PC whose turn it is?)
	;(If yes then some other variables used to track the active PC need to be updated)
	LDA TELEPORT.LOOP.COUNTER
	CLC
	ADC #$01 ;add $1 becuase counter was init to $00 and COMBAT.PC.ACTIVE = $01 for the 1st PC 
	CMP COMBAT.PC.ACTIVE
	BNE .PC.ACTIVE_CHECK.DONE
	;ACC: expected to be value >$00
	STA TELEPORT.ACTIVE_PC.MOVE.FLAG	;($00 = not moved this iteration | moved this iteration)

		
	
.PC.ACTIVE_CHECK.DONE	
	
			;JSR KEYIN

			
			;JSR KEYIN
			
			; LDA #$AA
			; JSR PREP.BRK
			; BRK
			
.NEXT.PC
	;increment PC map object index
	TXA
	CLC
	ADC #COMBAT.MAP_OBJECTS.PC.RECORD_SIZE
	TAX
		
	INC TELEPORT.LOOP.COUNTER ;increment loop counter
	LDA TELEPORT.LOOP.COUNTER 
	CMP PARTY.TOTAL.PC ;(PC sequential #)
	BEQ .TELEPORT.LOOP.DONE
	JMP .TELEPORT.LOOP
	
.TELEPORT.LOOP.DONE
	
;RESTORE REGISTERS
	PLA
	TAY
	
	

			
.INCREMENT.ACTIVE_PC.SINDEX	
	LDA TELEPORT.ACTIVE_PC.MOVE.FLAG	;($00 = not moved this iteration | moved this iteration)
	BEQ .INCREMENT.ACTIVE_PC.SINDEX.DONE	
			
	LDA COMBAT.PC.ACTIVE.SINDEX
	SEC
	SBC #SCREEN.ARRAY.OFFSET ;move the index 1 tile north
	STA COMBAT.PC.ACTIVE.SINDEX
	STA COMBAT.ATTACKER.SINDEX


			
	LDA #$00 ;reset flag
	STA TELEPORT.ACTIVE_PC.MOVE.FLAG	;($00 = not moved this iteration | moved this iteration)
.INCREMENT.ACTIVE_PC.SINDEX.DONE
	


			
	INY ;increment move loop counter (# of tiles moved)
	
	

	
	CPY #.TELEPORT_MOVES_MAX
	BEQ .MOVE.LOOP.DONE	
	JMP .MOVE.LOOP
.MOVE.LOOP.DONE


			
		; JSR KEYIN
		; LDA #$AA
		; LDX TELEPORT.LOOP.COUNTER
		; JSR PREP.BRK
		; BRK
		
.SET.CRITICAL_HIT.FLAGS
@START
;(engaged s_entity screen index)

		;**warning: attacker character sheet will be loaded on exit
		LDY COMBAT.ATTACKER.SINDEX
		LDA COMBAT.TARGET_HIT.DB+$0 ;set value to update (sindex of the target)	
.UPDATE.CHR_SHEET.ENGAGED.INDEX ;**OPT** Memory. This routine was written as as subroutine, but is being used inline here and it could be made more efficient. 
;PARAMETERS: YREG = screen index of attacker, ACC = value to write to CHR_SHEET.PC_MOB.ENGAGED.SINDEX
;ENTRANCE: direct
;RETURN: saved attacker character sheet with updated CHR_SHEET.PC_MOB.ENGAGED.SINDEX field

.SAVE.PARAMETERS.ENAGED
	;ACC = value to write to CHR_SHEET.PC_MOB.ENGAGED.SINDEX
	STA COMBAT.ENGAGED_SINDEX.UPDATE.PARM
	
	;read attacker (MOB) character sheet
		
		;YREG = COMBAT.ATTACKER.SINDEX
		LDA #$00 ;set read mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet

			
	;set the engaged field to the updated value
	;(expected to be the screen index of the defender or $FF to indicate no engagement)
	LDA COMBAT.ENGAGED_SINDEX.UPDATE.PARM
	STA CHR_SHEET.PC_MOB.ENGAGED.SINDEX

			
		;YREG = COMBAT.ATTACKER.SINDEX
		LDA #$01 ;set write mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet

@END


.EXIT
;RESTORE REGISTERS
	PLA 
	TAY
	PLA
	TAX
	
			; LDA #$01
			; STA TROUBLESHOOTING.HOOK

			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; LDA #$AA
			; LDX COMBAT.PC.ACTIVE.SINDEX
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP
			
	RTS

@END

.COLLISION.CHECK ;**BORROWED FROM SUMMON SPELL CODE BLOCK**
@START
;PARAMETERS: SUMMON.TARGET.SINDEX, SUMMON.PARM.FLAGS
;ENTRANCE: DIRECT, GE.SUMMON, GE.SPELL.ARMGDN.COW
;RETURN: ACC ($00 = screen location open, $01 = screen location blocked)

;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA

		
;COLLISION CHECK ON TARGET LOCATION
		
.SELECT.COLLISION.ROUTINES ;(MTT or non-MTT)
	LDA .PARM.FLAGS
	CMP #MAP_OBJECT.FLAG.MTT_AGGR	;is flag set to multi-tile?
	BEQ .COLLISION_CHECK.MTT ;if yes, branch to multi-tile draw section
	
	;**FALLS THROUGH**
	
	
.COLLISION_CHECK.SINGLE
@START


.ENTRANCE
	LDY .TARGET.SINDEX	;LOAD NEXT ADJACENT TILE#
	
.WALKING_RULES		
;APPLY MOB WALKING COLLISION RULES

.MAP_OBJECT.CHECKS	
		
	;CHECK FOR OTHER SPRITES
	LDA SCREEN.MO_SPRITE.DATA,Y					;DOES DESTINATION TILE HAVE A MOB MO?	
	CMP #$FF			
	BNE .MOVE_BLOCKED


			
	;CHECK FOR GENERAL MAP OBJECTS
	LDA SCREEN.MO_GENERAL.DATA,Y	;load general map object data for current tile location
	CMP #$FF						;is a general map object present?
	;BEQ .SPRITE.WALKING.TILE.TESTS	;if no, then continue with other checks
	BNE .MOVE_BLOCKED

.TERRAIN.OBSTACLE.CHECKS
	LDA SCREEN.TILE.DATA,Y						;LOAD TILE TYPE OF DESTINATION TILE

	;obstacle test #1
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
	;**FALLS THROUGH
	
	;JMP .MOVE_PERMITTED

.MOVE_PERMITTED
	LDA #$00		;PERMITTS MOVE BY RECORDING $00 
	STA .COLLISION.RETURN_VALUE
	JMP .COLLISION.EXIT

.MOVE_BLOCKED
	LDA #$01		;BLOCKS MOVE BY RECORDING $01	LDA #$00		;PERMITTS MOVE BY RECORDING $00 
	STA .COLLISION.RETURN_VALUE
	JMP .COLLISION.EXIT


; .COLLISION.LOOP.MOB_SEA	
	; ;***<INSERT RULES FOR SEA MOBS>***
	; ;COPY THE SECTION ABOVE FOR LAND MOBS AS A STARTING POINT
	
	; ;****NOTE: make sure to check for player location using regular constant and also the constants for the MT frigate if frigate is active as transport

@END
	
.COLLISION_CHECK.MTT  ;
@START

.IDENTIFY.MTT.TILE.SCREEN_LOCATIONS
	;set Multi-tile S_ENTITY tile screen locations
	LDY .TARGET.SINDEX
	STY .MT.TILE_LOCATIONS+$0 ;set screen location of upper left tile

	STY .MT.TILE_LOCATIONS+$1 ;set screen location of upper right tile
	INC .MT.TILE_LOCATIONS+$1 ;+1 tile right

	TYA
	CLC
	ADC #SCREEN.ARRAY.OFFSET ;+1 tile down	
	STA .MT.TILE_LOCATIONS+$2 ;set screen location of lower left tile

	TYA
	CLC
	ADC #SCREEN.ARRAY.OFFSET+1 ;+1 tile down, +1 tile right	
	STA .MT.TILE_LOCATIONS+$3 ;set screen location of lower right tile
		
	LDX #$00
.MTT.COLLISION.CHECKS.LOOP

	LDY .MT.TILE_LOCATIONS,X	;LOAD NEXT ADJACENT TILE#
	
.WALKING_RULES_MTT		
;APPLY MOB WALKING COLLISION RULES

.MAP_OBJECT.CHECKS_MTT	
	;CHECK FOR OTHER SPRITES
	LDA SCREEN.MO_SPRITE.DATA,Y					;DOES DESTINATION TILE HAVE A MOB MO?	
	CMP #$FF
	BNE .MOVE_BLOCKED_MTT

	;CHECK FOR GENERAL MAP OBJECTS
	LDA SCREEN.MO_GENERAL.DATA,Y	;load general map object data for current tile location
	CMP #$FF						;is a general map object present?
	;BEQ .SPRITE.WALKING.TILE.TESTS	;if no, then continue with other checks
	BNE .MOVE_BLOCKED_MTT

.TERRAIN.OBSTACLE.CHECKS.MTT
	LDA SCREEN.TILE.DATA,Y						;LOAD TILE TYPE OF DESTINATION TILE

	;obstacle test #1
	CMP #COLLISION_FLAG.MOB_LAND.LT1			;ABSOLUTE OBSTACLE?
	BCC .MOVE_BLOCKED_MTT
	
	CMP #COLLISION_FLAG.MOB_LAND.EQ1			;QUICKSAND?
	BEQ .MOVE_BLOCKED_MTT
	
	CMP #COLLISION_FLAG.MOB_LAND.GRE			;WATER?	
	BCS .WATER_TEST1.PASS.MTT
	JMP .NEXT_COLLISION_TEST_MTT
.WATER_TEST1.PASS.MTT
	CMP #COLLISION_FLAG.MOB_LAND.LT2			;WATER?	
	BCC .MOVE_BLOCKED_MTT
	
.NEXT_COLLISION_TEST_MTT	
;NO MORE TESTS FOR NOW

	;**FALLS THROUGH
	
	;JMP .MOVE_PERMITTED

.MOVE_PERMITTED_MTT
	LDA #$00		;PERMITTS MOVE BY RECORDING $00 
	STA .COLLISION.RETURN_VALUE
	JMP .EXIT.TEST_MTT

.MOVE_BLOCKED_MTT
	LDA #$01		;BLOCKS MOVE BY RECORDING $01	LDA #$00		;PERMITTS MOVE BY RECORDING $00 
	STA .COLLISION.RETURN_VALUE
	JMP .COLLISION.EXIT

.EXIT.TEST_MTT
	INX ;next tile screen location
	CPX #$04
	BNE .MTT.COLLISION.CHECKS.LOOP
	
; .COLLISION.LOOP.MOB_SEA	
	; ;***<INSERT RULES FOR SEA MOBS>***
	; ;COPY THE SECTION ABOVE FOR LAND MOBS AS A STARTING POINT
	
	; ;****NOTE: make sure to check for player location using regular constant and also the constants for the MT frigate if frigate is active as transport

	;**FALLS THROUGH**
@END

	;**FALLS THROUGH**
	
.COLLISION.EXIT

;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX
	
	LDA .COLLISION.RETURN_VALUE
			
	RTS
	
@END

;LOCAL VARIABLES
@START

;.COLLISSION.CHECK VARAIBLES
.TARGET.SINDEX	.EQ SPELL.SUMMON.LOCAL_VAR.BLOCK+$4 ;$1 byte
.PARM.FLAGS		.EQ SPELL.SUMMON.LOCAL_VAR.BLOCK+$1 ;$1 byte

.COLLISION.RETURN_VALUE	.EQ SPELL.SUMMON.LOCAL_VAR.BLOCK+$5 ;$1 byte
.COLLISION.OVERIDE 	.BS $01 ;(.BS for init purpose). ($00 = off, $01 = on)
.MT.TILE_LOCATIONS	.EQ COMBAT.EXPLOSION.CENTER+$0 ;$4 bytes
.MT.TILE_TYPES		.EQ COMBAT.EXPLOSION.CENTER+$4 ;$4 bytes

;OTHER VARIABLES
TELEPORT.LOOP.COUNTER			.BS $01
TELEPORT.SINDEX.ADJ				.BS $01
TELEPORT.ACTIVE_PC.MOVE.FLAG 	.BS $01 ;($00 = not moved this iteration | moved this iteration)


@END

	.EP ;end patch mode
;--------
GE.SPELL.COMBAT_TELEPORT_PARTY.END ;marks the end address of the code block for this spell
@END


;=======================

	
;=======================

;========================================
;===========WEAPON SHAPES================
;========================================
;
WEAPON.SHAPE_TABLES.FILE_START_ADDRESS 

	.PH $D000 ;start patch mode
			  ;this is so that the labels in the patch section will be assigned memory addresses that correspond with
			  ;the location in BSR this data is loaded into. 

WEAPON.SHAPE_TABLES.AUX_ANGLED_START_ADDRESS 
@START
;Uses $400 bytes per shape. 

;CROSSBOW BOLT
@START
CROSSBOW_BOLT.SHAPE_0.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.FF.FF.FF.F9.BF.E0.BF.E0.BF.E0.BF.E0.BF.E0.BF.E0.FF.F9.FF.FF.FF.FF.FF.FF.FF.FF
CROSSBOW_BOLT.SHAPE_0.ORA_MASK			.HS	00.00.00.00.00.00.00.00.00.00.40.01.40.01.40.01.40.01.40.01.40.01.00.00.00.00.00.00.00.00.00.00
				
CROSSBOW_BOLT.SHAPE_1.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.8F.F8.83.E0.8F.F8.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF
CROSSBOW_BOLT.SHAPE_1.ORA_MASK			.HS	00.00.00.00.00.00.00.00.00.00.00.00.00.00.70.07.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00
				
CROSSBOW_BOLT.SHAPE_2.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.FF.FF.BF.FE.8F.F8.8F.F8.8F.F8.8F.F8.8F.F8.8F.F8.BF.FE.FF.FF.FF.FF.FF.FF.FF.FF
CROSSBOW_BOLT.SHAPE_2.ORA_MASK			.HS	00.00.00.00.00.00.00.00.00.00.40.01.40.01.40.01.40.01.40.01.40.01.00.00.00.00.00.00.00.00.00.00
				
CROSSBOW_BOLT.SHAPE_3.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.8F.F8.83.E0.8F.F8.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF
CROSSBOW_BOLT.SHAPE_3.ORA_MASK			.HS	00.00.00.00.00.00.00.00.00.00.00.00.00.00.70.07.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00
				
				
;OCTET 4				
CROSSBOW_BOLT.SHAPE_NE0.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.FF.FD.BF.F0.BF.F0.9F.F0.9F.F8.8F.F8.8F.FC.87.FC.87.FE.87.FE.DF.FF.FF.FF.FF.FF
CROSSBOW_BOLT.SHAPE_NE0.ORA_MASK			.HS	00.00.00.00.00.00.00.00.00.02.00.02.00.03.00.01.40.01.40.00.60.00.20.00.20.00.00.00.00.00.00.00
				
CROSSBOW_BOLT.SHAPE_NE1.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.FF.FF.FF.F3.FF.C0.BF.E0.9F.F0.8F.F8.87.FC.83.FE.CF.FF.FF.FF.FF.FF.FF.FF.FF.FF
CROSSBOW_BOLT.SHAPE_NE1.ORA_MASK			.HS	00.00.00.00.00.00.00.00.00.00.00.0C.00.06.00.03.40.01.60.00.30.00.00.00.00.00.00.00.00.00.00.00
				
CROSSBOW_BOLT.SHAPE_NE2.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.F3.FF.C0.9F.E0.87.F8.83.FE.CF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF
CROSSBOW_BOLT.SHAPE_NE2.ORA_MASK			.HS	00.00.00.00.00.00.00.00.00.00.00.00.00.0C.00.07.60.01.30.00.00.00.00.00.00.00.00.00.00.00.00.00
				
				
;OCTET 5				
CROSSBOW_BOLT.SHAPE_SE0.AND_MASK			.HS	FF.FF.DF.FF.87.FE.87.FE.87.FC.8F.FC.8F.F8.9F.F8.9F.F0.BF.F0.BF.F0.FF.FD.FF.FF.FF.FF.FF.FF.FF.FF
CROSSBOW_BOLT.SHAPE_SE0.ORA_MASK			.HS	00.00.00.00.20.00.20.00.60.00.40.00.40.01.00.01.00.03.00.02.00.02.00.00.00.00.00.00.00.00.00.00
				
CROSSBOW_BOLT.SHAPE_SE1.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.E7.FF.81.FF.83.FE.87.FC.8F.F8.9F.F0.BF.E0.FF.F9.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF
CROSSBOW_BOLT.SHAPE_SE1.ORA_MASK			.HS	00.00.00.00.00.00.00.00.18.00.30.00.60.00.40.01.00.03.00.06.00.00.00.00.00.00.00.00.00.00.00.00
				
CROSSBOW_BOLT.SHAPE_SE2.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.CF.FF.83.FE.87.F8.9F.E0.FF.C0.FF.F3.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF
CROSSBOW_BOLT.SHAPE_SE2.ORA_MASK			.HS	00.00.00.00.00.00.00.00.00.00.00.00.30.00.60.01.00.07.00.0C.00.00.00.00.00.00.00.00.00.00.00.00
				
				
;OCTET 6				
CROSSBOW_BOLT.SHAPE_SW0.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.FF.FD.BF.F0.BF.F0.9F.F0.9F.F8.8F.F8.8F.FC.87.FC.87.FE.87.FE.DF.FF.FF.FF.FF.FF
CROSSBOW_BOLT.SHAPE_SW0.ORA_MASK			.HS	00.00.00.00.00.00.00.00.00.02.00.02.00.03.00.01.40.01.40.00.60.00.20.00.20.00.00.00.00.00.00.00
				
CROSSBOW_BOLT.SHAPE_SW1.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.FF.FF.FF.F3.FF.C0.BF.E0.9F.F0.8F.F8.87.FC.83.FE.CF.FF.FF.FF.FF.FF.FF.FF.FF.FF
CROSSBOW_BOLT.SHAPE_SW1.ORA_MASK			.HS	00.00.00.00.00.00.00.00.00.00.00.0C.00.06.00.03.40.01.60.00.30.00.00.00.00.00.00.00.00.00.00.00
				
CROSSBOW_BOLT.SHAPE_SW2.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.F3.FF.C0.9F.E0.87.F8.83.FE.CF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF
CROSSBOW_BOLT.SHAPE_SW2.ORA_MASK			.HS	00.00.00.00.00.00.00.00.00.00.00.00.00.0C.00.07.60.01.30.00.00.00.00.00.00.00.00.00.00.00.00.00
				
				
;OCTET 7				
CROSSBOW_BOLT.SHAPE_NW0.AND_MASK			.HS	FF.FF.DF.FF.87.FE.87.FE.87.FC.8F.FC.8F.F8.9F.F8.9F.F0.BF.F0.BF.F0.FF.FD.FF.FF.FF.FF.FF.FF.FF.FF
CROSSBOW_BOLT.SHAPE_NW0.ORA_MASK			.HS	00.00.00.00.20.00.20.00.60.00.40.00.40.01.00.01.00.03.00.02.00.02.00.00.00.00.00.00.00.00.00.00
				
CROSSBOW_BOLT.SHAPE_NW1.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.E7.FF.81.FF.83.FE.87.FC.8F.F8.9F.F0.BF.E0.FF.F9.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF
CROSSBOW_BOLT.SHAPE_NW1.ORA_MASK			.HS	00.00.00.00.00.00.00.00.18.00.30.00.60.00.40.01.00.03.00.06.00.00.00.00.00.00.00.00.00.00.00.00
				
CROSSBOW_BOLT.SHAPE_NW2.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.CF.FF.83.FE.87.F8.9F.E0.FF.C0.FF.F3.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF
CROSSBOW_BOLT.SHAPE_NW2.ORA_MASK			.HS	00.00.00.00.00.00.00.00.00.00.00.00.30.00.60.01.00.07.00.0C.00.00.00.00.00.00.00.00.00.00.00.00

; CROSSBOW_BOLT.SHAPE.START
; ;----------
; CROSSBOW_BOLT.SHAPE_0.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.FF.FF.FF.F9.BF.E0.BF.E0.BF.E0.BF.E0.BF.E0.BF.E0.FF.F9.FF.FF.FF.FF.FF.FF.FF.FF
; CROSSBOW_BOLT.SHAPE_0.ORA_MASK			.HS	00.00.00.00.00.00.00.00.00.00.40.01.40.01.40.01.40.01.40.01.40.01.00.00.00.00.00.00.00.00.00.00
				
; CROSSBOW_BOLT.SHAPE_1.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.8F.F8.83.E0.8F.F8.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF
; CROSSBOW_BOLT.SHAPE_1.ORA_MASK			.HS	00.00.00.00.00.00.00.00.00.00.00.00.00.00.70.07.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00

; ; EXCLUDE_ALL.AND_MASK .HS 00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00
; ; ;SPELL.FIREBALL.SHAPE_0.ORA_MASK			.HS	80.80.80.80.80.80.80.80.88.80.82.80.80.80.A0.81.88.C0.80.D0.A0.80.E0.80.82.C0.AA.84.A0.D1.8E.C5
; ; EXCLUDE_ALL.AND_MASK 	.HS 00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00
; ; SOLID.ORANGE			.HS	FC.FF.AC.F5.AC.F5.AC.F5.AC.F5.AC.F5.AC.F5.AC.F5.AC.F5.AC.F5.AC.F5.AC.F5.AC.F5.AC.F5.AC.F5.FC.FF	
	
	
; CROSSBOW_BOLT.SHAPE_2.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.FF.FF.BF.FE.8F.F8.8F.F8.8F.F8.8F.F8.8F.F8.8F.F8.BF.FE.FF.FF.FF.FF.FF.FF.FF.FF
; CROSSBOW_BOLT.SHAPE_2.ORA_MASK			.HS	00.00.00.00.00.00.00.00.00.00.40.01.40.01.40.01.40.01.40.01.40.01.00.00.00.00.00.00.00.00.00.00
				
; CROSSBOW_BOLT.SHAPE_3.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.8F.F8.83.E0.8F.F8.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF
; CROSSBOW_BOLT.SHAPE_3.ORA_MASK			.HS	00.00.00.00.00.00.00.00.00.00.00.00.00.00.70.07.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00

; ; ;SPELL.FIREBALL.SHAPE_0.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.F7.FF.C1.FF.F0.FF.DD.FE.87.B8.C1.8E.D7.83.87.AE.85.BC.90.CB.80.A0.81.80.80.80
; ; ;SPELL.FIREBALL.SHAPE_0.ORA_MASK			.HS	80.80.80.80.80.80.80.80.88.80.82.80.80.80.A0.81.88.C0.80.D0.A0.80.E0.80.82.C0.AA.84.A0.D1.8E.C5
; ; EXCLUDE_ALL.AND_MASK3 .HS 00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00
; ; SOLID.ORANGE3			.HS	FC.FF.AC.F5.AC.F5.AC.F5.AC.F5.AC.F5.AC.F5.AC.F5.AC.F5.AC.F5.AC.F5.AC.F5.AC.F5.AC.F5.AC.F5.FC.FF	
				
				
; ;OCTET 4				
; CROSSBOW_BOLT.SHAPE_NE0.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.FF.FD.BF.F0.BF.F0.9F.F0.9F.F8.8F.F8.8F.FC.87.FC.87.FE.87.FE.DF.FF.FF.FF.FF.FF
; CROSSBOW_BOLT.SHAPE_NE0.ORA_MASK			.HS	00.00.00.00.00.00.00.00.00.02.00.02.00.03.00.01.40.01.40.00.60.00.20.00.20.00.00.00.00.00.00.00
				
; CROSSBOW_BOLT.SHAPE_NE1.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.FF.FF.FF.F3.FF.C0.BF.E0.9F.F0.8F.F8.87.FC.83.FE.CF.FF.FF.FF.FF.FF.FF.FF.FF.FF
; CROSSBOW_BOLT.SHAPE_NE1.ORA_MASK			.HS	00.00.00.00.00.00.00.00.00.00.00.0C.00.06.00.03.40.01.60.00.30.00.00.00.00.00.00.00.00.00.00.00
				
; CROSSBOW_BOLT.SHAPE_NE2.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.F3.FF.C0.9F.E0.87.F8.83.FE.CF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF
; CROSSBOW_BOLT.SHAPE_NE2.ORA_MASK			.HS	00.00.00.00.00.00.00.00.00.00.00.00.00.0C.00.07.60.01.30.00.00.00.00.00.00.00.00.00.00.00.00.00
				
				
; ;OCTET 5				
; CROSSBOW_BOLT.SHAPE_SE0.AND_MASK			.HS	FF.FF.DF.FF.87.FE.87.FE.87.FC.8F.FC.8F.F8.9F.F8.9F.F0.BF.F0.BF.F0.FF.FD.FF.FF.FF.FF.FF.FF.FF.FF
; CROSSBOW_BOLT.SHAPE_SE0.ORA_MASK			.HS	00.00.00.00.20.00.20.00.60.00.40.00.40.01.00.01.00.03.00.02.00.02.00.00.00.00.00.00.00.00.00.00
				
; CROSSBOW_BOLT.SHAPE_SE1.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.E7.FF.81.FF.83.FE.87.FC.8F.F8.9F.F0.BF.E0.FF.F9.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF
; CROSSBOW_BOLT.SHAPE_SE1.ORA_MASK			.HS	00.00.00.00.00.00.00.00.18.00.30.00.60.00.40.01.00.03.00.06.00.00.00.00.00.00.00.00.00.00.00.00
				
; CROSSBOW_BOLT.SHAPE_SE2.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.CF.FF.83.FE.87.F8.9F.E0.FF.C0.FF.F3.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF
; CROSSBOW_BOLT.SHAPE_SE2.ORA_MASK			.HS	00.00.00.00.00.00.00.00.00.00.00.00.30.00.60.01.00.07.00.0C.00.00.00.00.00.00.00.00.00.00.00.00
				
				
; ;OCTET 6				
; CROSSBOW_BOLT.SHAPE_SW0.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.FF.FD.BF.F0.BF.F0.9F.F0.9F.F8.8F.F8.8F.FC.87.FC.87.FE.87.FE.DF.FF.FF.FF.FF.FF
; CROSSBOW_BOLT.SHAPE_SW0.ORA_MASK			.HS	00.00.00.00.00.00.00.00.00.02.00.02.00.03.00.01.40.01.40.00.60.00.20.00.20.00.00.00.00.00.00.00
				
; CROSSBOW_BOLT.SHAPE_SW1.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.FF.FF.FF.F3.FF.C0.BF.E0.9F.F0.8F.F8.87.FC.83.FE.CF.FF.FF.FF.FF.FF.FF.FF.FF.FF
; CROSSBOW_BOLT.SHAPE_SW1.ORA_MASK			.HS	00.00.00.00.00.00.00.00.00.00.00.0C.00.06.00.03.40.01.60.00.30.00.00.00.00.00.00.00.00.00.00.00

; ; EXCLUDE_ALL.AND_MASK .HS 00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00
; ; SOLID.ORANGE			.HS	FC.FF.AC.F5.AC.F5.AC.F5.AC.F5.AC.F5.AC.F5.AC.F5.AC.F5.AC.F5.AC.F5.AC.F5.AC.F5.AC.F5.AC.F5.FC.FF	

				
; CROSSBOW_BOLT.SHAPE_SW2.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.F3.FF.C0.9F.E0.87.F8.83.FE.CF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF
; CROSSBOW_BOLT.SHAPE_SW2.ORA_MASK			.HS	00.00.00.00.00.00.00.00.00.00.00.00.00.0C.00.07.60.01.30.00.00.00.00.00.00.00.00.00.00.00.00.00
				
				
; ;OCTET 7				
; CROSSBOW_BOLT.SHAPE_NW0.AND_MASK			.HS	FF.FF.DF.FF.87.FE.87.FE.87.FC.8F.FC.8F.F8.9F.F8.9F.F0.BF.F0.BF.F0.FF.FD.FF.FF.FF.FF.FF.FF.FF.FF
; CROSSBOW_BOLT.SHAPE_NW0.ORA_MASK			.HS	00.00.00.00.20.00.20.00.60.00.40.00.40.01.00.01.00.03.00.02.00.02.00.00.00.00.00.00.00.00.00.00
				
; CROSSBOW_BOLT.SHAPE_NW1.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.E7.FF.81.FF.83.FE.87.FC.8F.F8.9F.F0.BF.E0.FF.F9.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF
; CROSSBOW_BOLT.SHAPE_NW1.ORA_MASK			.HS	00.00.00.00.00.00.00.00.18.00.30.00.60.00.40.01.00.03.00.06.00.00.00.00.00.00.00.00.00.00.00.00
				
; CROSSBOW_BOLT.SHAPE_NW2.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.CF.FF.83.FE.87.F8.9F.E0.FF.C0.FF.F3.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF
; CROSSBOW_BOLT.SHAPE_NW2.ORA_MASK			.HS	00.00.00.00.00.00.00.00.00.00.00.00.30.00.60.01.00.07.00.0C.00.00.00.00.00.00.00.00.00.00.00.00

@END

;ARROW
@START
ARROW.SHAPE.START
;----------
ARROW.SHAPE_0.AND_MASK			.HS	FF.FF.FF.FF.FF.F9.BF.E0.BF.E0.BF.E0.BF.E0.BF.E0.BF.E0.BF.E0.BF.E0.9F.C0.9F.C0.8F.80.BF.E6.FF.FF
ARROW.SHAPE_0.ORA_MASK			.HS	00.00.00.00.00.00.40.01.40.01.40.01.40.01.40.01.40.01.40.01.40.01.60.03.60.03.30.06.00.00.00.00
				
ARROW.SHAPE_1.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.FF.FF.E3.FF.80.F8.81.E0.83.C0.81.E0.80.F8.E3.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF
ARROW.SHAPE_1.ORA_MASK			.HS	00.00.00.00.00.00.00.00.00.00.1C.00.78.07.70.0F.78.07.1C.00.00.00.00.00.00.00.00.00.00.00.00.00
				
ARROW.SHAPE_2.AND_MASK			.HS	FF.FF.CF.F9.83.E0.87.F0.87.F0.8F.F8.8F.F8.8F.F8.8F.F8.8F.F8.8F.F8.8F.F8.8F.F8.BF.FE.FF.FF.FF.FF
ARROW.SHAPE_2.ORA_MASK			.HS	00.00.00.00.30.06.60.03.60.03.40.01.40.01.40.01.40.01.40.01.40.01.40.01.40.01.00.00.00.00.00.00
				
ARROW.SHAPE_3.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.FF.FF.FF.E3.8F.80.83.C0.81.E0.83.C0.8F.80.FF.E3.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF
ARROW.SHAPE_3.ORA_MASK			.HS	00.00.00.00.00.00.00.00.00.00.00.1C.70.0F.78.07.70.0F.00.1C.00.00.00.00.00.00.00.00.00.00.00.00
				
				
;OCTET 4				
ARROW.SHAPE_NE0.AND_MASK			.HS	FF.FF.FF.FC.9F.F0.9F.F0.8F.F8.8F.F8.87.F8.87.FC.83.FC.81.FC.81.FC.87.FC.87.FC.9F.FF.FF.FF.FF.FF
ARROW.SHAPE_NE0.ORA_MASK			.HS	00.00.00.00.00.03.00.03.40.01.40.01.60.01.60.00.70.00.78.00.78.00.60.00.60.00.00.00.00.00.00.00
				
ARROW.SHAPE_NE1.AND_MASK			.HS	FF.FF.FF.FF.FF.E7.FF.81.FF.80.BF.C0.8F.E0.83.F0.81.F8.81.FC.87.FC.83.FC.83.FE.CF.FF.FF.FF.FF.FF
ARROW.SHAPE_NE1.ORA_MASK			.HS	00.00.00.00.00.00.00.18.00.1C.00.0E.00.07.70.03.78.01.78.00.60.00.70.00.30.00.00.00.00.00.00.00
				
ARROW.SHAPE_NE2.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.FF.FF.FF.F1.BF.C0.81.C0.80.E0.80.F8.83.FE.81.FF.C0.FF.F3.FF.FF.FF.FF.FF.FF.FF
ARROW.SHAPE_NE2.ORA_MASK			.HS	00.00.00.00.00.00.00.00.00.00.00.0E.40.0F.7E.07.7C.01.30.00.18.00.0C.00.00.00.00.00.00.00.00.00
				
				
;OCTET 5				
ARROW.SHAPE_SE0.AND_MASK			.HS	FF.FF.FF.FF.9F.FF.87.FC.87.FC.81.FC.81.FC.83.FC.87.F8.8F.F8.8F.F8.8F.F0.9F.F0.9F.F0.FF.FC.FF.FF
ARROW.SHAPE_SE0.ORA_MASK			.HS	00.00.00.00.00.00.60.00.60.00.78.00.78.00.70.00.60.01.40.01.40.01.40.03.00.03.00.03.00.00.00.00
				
ARROW.SHAPE_SE1.AND_MASK			.HS	FF.FF.FF.FF.CF.FF.83.FE.83.FC.87.FC.81.FC.81.F8.83.F0.8F.E0.BF.C0.FF.80.FF.81.FF.E7.FF.FF.FF.FF
ARROW.SHAPE_SE1.ORA_MASK			.HS	00.00.00.00.00.00.30.00.70.00.60.00.78.00.78.01.70.03.00.07.00.0E.00.1C.00.18.00.00.00.00.00.00
				
ARROW.SHAPE_SE2.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.F3.FF.C0.FF.81.FF.81.FE.80.F8.80.E0.83.C0.BF.C0.FF.F1.FF.FF.FF.FF.FF.FF.FF.FF
ARROW.SHAPE_SE2.ORA_MASK			.HS	00.00.00.00.00.00.00.00.0C.00.18.00.30.00.7E.01.7C.07.40.0F.00.0E.00.00.00.00.00.00.00.00.00.00
				
				
;OCTET 6				
ARROW.SHAPE_SW0.AND_MASK			.HS	FF.FF.FF.FF.FF.FC.9F.F0.9F.F0.9F.C0.9F.C0.9F.E0.8F.F0.8F.F8.8F.F8.87.F8.87.FC.87.FC.9F.FF.FF.FF
ARROW.SHAPE_SW0.ORA_MASK			.HS	00.00.00.00.00.00.00.03.00.03.00.0F.00.0F.00.07.40.03.40.01.40.01.60.01.60.00.60.00.00.00.00.00
				
ARROW.SHAPE_SW1.AND_MASK			.HS	FF.FF.FF.FF.FF.F9.BF.E0.9F.E0.9F.F0.9F.C0.8F.C0.87.E0.83.F8.81.FE.80.FF.C0.FF.F3.FF.FF.FF.FF.FF
ARROW.SHAPE_SW1.ORA_MASK			.HS	00.00.00.00.00.00.00.06.00.07.00.03.00.0F.40.0F.60.07.70.00.38.00.1C.00.0C.00.00.00.00.00.00.00
				
ARROW.SHAPE_SW2.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.FF.E7.FF.81.FF.C0.BF.E0.8F.80.83.80.81.C0.81.FE.C7.FF.FF.FF.FF.FF.FF.FF.FF.FF
ARROW.SHAPE_SW2.ORA_MASK			.HS	00.00.00.00.00.00.00.00.00.18.00.0C.00.06.40.1F.70.3F.78.01.38.00.00.00.00.00.00.00.00.00.00.00
				
				
;OCTET 7				
ARROW.SHAPE_NW0.AND_MASK			.HS	FF.FF.9F.FF.87.FC.87.FC.8F.F8.8F.F8.8F.F0.9F.F0.9F.E0.9F.C0.9F.C0.9F.F0.9F.F0.FF.FC.FF.FF.FF.FF
ARROW.SHAPE_NW0.ORA_MASK			.HS	00.00.00.00.60.00.60.00.40.01.40.01.40.03.00.03.00.07.00.0F.00.0F.00.03.00.03.00.00.00.00.00.00
				
ARROW.SHAPE_NW1.AND_MASK			.HS	FF.FF.FF.FF.F3.FF.C0.FF.80.FF.81.FE.83.F8.87.E0.8F.C0.9F.C0.9F.F0.9F.E0.BF.E0.FF.F9.FF.FF.FF.FF
ARROW.SHAPE_NW1.ORA_MASK			.HS	00.00.00.00.00.00.0C.00.1C.00.38.00.70.00.60.07.40.0F.00.0F.00.03.00.07.00.06.00.00.00.00.00.00
				
ARROW.SHAPE_NW2.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.FF.FF.C7.FF.81.FE.81.C0.83.80.8F.80.BF.E0.FF.C0.FF.81.FF.E7.FF.FF.FF.FF.FF.FF
ARROW.SHAPE_NW2.ORA_MASK			.HS	00.00.00.00.00.00.00.00.00.00.38.00.78.01.70.3F.40.1F.00.06.00.0C.00.18.00.00.00.00.00.00.00.00
@END

 .BS $400 ;**empty angled shape slot**
 
WEAPON.SHAPE_TABLES.AUX_NON_ANGLED_START_ADDRESS
;Uses $100 bytes per shape. 

;THROWING STAR (qty 1)
@START
THROWING_STAR_1.SHAPE.START
;----------
WEAPON.THROWING_STAR.QTY1.SHAPE_0.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FD.BF.F0.8F.F0.9F.E0.9F.F8.FF.FE.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF
WEAPON.THROWING_STAR.QTY1.SHAPE_0.ORA_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.80.82.C0.83.80.87.80.81.80.80.80.80.80.80.80.80.80.80.80.80
				
WEAPON.THROWING_STAR.QTY1.SHAPE_1.AND_MASK			.HS	7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7E.1F.78.1F.60.0F.70.3F.70.7F.7D.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F
WEAPON.THROWING_STAR.QTY1.SHAPE_1.ORA_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.80.81.80.87.C0.83.80.82.80.80.80.80.80.80.80.80.80.80.80.80
				
WEAPON.THROWING_STAR.QTY1.SHAPE_2.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FD.BF.F0.8F.F0.9F.E0.9F.F8.FF.FE.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF
WEAPON.THROWING_STAR.QTY1.SHAPE_2.ORA_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.80.82.C0.83.80.87.80.81.80.80.80.80.80.80.80.80.80.80.80.80
				
WEAPON.THROWING_STAR.QTY1.SHAPE_3.AND_MASK			.HS	7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7E.1F.78.1F.60.0F.70.3F.70.7F.7D.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F
WEAPON.THROWING_STAR.QTY1.SHAPE_3.ORA_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.80.81.80.87.C0.83.80.82.80.80.80.80.80.80.80.80.80.80.80.80
@END

;THROWING STAR (qty 2)
@START
THROWING_STAR_2.SHAPE.START
;----------
WEAPON.THROWING_STAR.QTY2.SHAPE_0.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.BF.FF.8F.FC.83.FC.87.F8.87.FE.DF.F7.FF.C1.BF.C0.FF.80.FF.E0.FF.FB.FF.FF.FF.FF
WEAPON.THROWING_STAR.QTY2.SHAPE_0.ORA_MASK			.HS	80.80.80.80.80.80.80.80.C0.80.F0.80.E0.81.A0.80.80.80.80.88.80.8E.80.9C.80.84.80.80.80.80.80.80
				
WEAPON.THROWING_STAR.QTY2.SHAPE_1.AND_MASK			.HS	7F.7F.7F.7F.7F.7F.5F.7F.07.7E.07.78.03.7C.0F.7C.3F.7B.7F.60.7F.00.3F.40.7F.41.7F.77.7F.7F.7F.7F
WEAPON.THROWING_STAR.QTY2.SHAPE_1.ORA_MASK			.HS	80.80.80.80.80.80.80.80.A0.80.E0.81.F0.80.C0.80.80.80.80.84.80.9C.80.8E.80.88.80.80.80.80.80.80
				
WEAPON.THROWING_STAR.QTY2.SHAPE_2.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.BF.FF.8F.FC.83.FC.87.F8.87.FE.DF.F7.FF.C1.BF.C0.FF.80.FF.E0.FF.FB.FF.FF.FF.FF
WEAPON.THROWING_STAR.QTY2.SHAPE_2.ORA_MASK			.HS	80.80.80.80.80.80.80.80.C0.80.F0.80.E0.81.A0.80.80.80.80.88.80.8E.80.9C.80.84.80.80.80.80.80.80
				
WEAPON.THROWING_STAR.QTY2.SHAPE_3.AND_MASK			.HS	7F.7F.7F.7F.7F.7F.5F.7F.07.7E.07.78.03.7C.0F.7C.3F.7B.7F.60.7F.00.3F.40.7F.41.7F.77.7F.7F.7F.7F
WEAPON.THROWING_STAR.QTY2.SHAPE_3.ORA_MASK			.HS	80.80.80.80.80.80.80.80.A0.80.E0.81.F0.80.C0.80.80.80.80.84.80.9C.80.8E.80.88.80.80.80.80.80.80


@END

;THROWING STAR (qty 3)
@START
THROWING_STAR_3.SHAPE.START
;----------
WEAPON.THROWING_STAR.QTY3.SHAPE_0.AND_MASK			.HS	FF.FF.BF.FF.8F.FC.83.FC.87.F8.87.FE.DF.F7.F7.C1.81.C0.81.80.80.E0.83.FB.EF.FF.FF.FF.FF.FF.FF.FF
WEAPON.THROWING_STAR.QTY3.SHAPE_0.ORA_MASK			.HS	80.80.80.80.C0.80.F0.80.E0.81.A0.80.80.80.80.88.88.8E.B8.9C.9C.84.90.80.80.80.80.80.80.80.80.80
				
WEAPON.THROWING_STAR.QTY3.SHAPE_1.AND_MASK			.HS	7F.7F.5F.7F.07.7E.07.78.03.7C.0F.7C.3F.7B.6F.60.03.00.00.40.01.40.41.77.77.7F.7F.7F.7F.7F.7F.7F
WEAPON.THROWING_STAR.QTY3.SHAPE_1.ORA_MASK			.HS	80.80.80.80.A0.80.E0.81.F0.80.C0.80.80.80.80.84.90.9C.9C.8E.B8.88.88.80.80.80.80.80.80.80.80.80
				
WEAPON.THROWING_STAR.QTY3.SHAPE_2.AND_MASK			.HS	FF.FF.BF.FF.8F.FC.83.FC.87.F8.87.FE.DF.F7.F7.C1.81.C0.81.80.80.E0.83.FB.EF.FF.FF.FF.FF.FF.FF.FF
WEAPON.THROWING_STAR.QTY3.SHAPE_2.ORA_MASK			.HS	80.80.80.80.C0.80.F0.80.E0.81.A0.80.80.80.80.88.88.8E.B8.9C.9C.84.90.80.80.80.80.80.80.80.80.80
				
WEAPON.THROWING_STAR.QTY3.SHAPE_3.AND_MASK			.HS	7F.7F.5F.7F.07.7E.07.78.03.7C.0F.7C.3F.7B.6F.60.03.00.00.40.01.40.41.77.77.7F.7F.7F.7F.7F.7F.7F
WEAPON.THROWING_STAR.QTY3.SHAPE_3.ORA_MASK			.HS	80.80.80.80.A0.80.E0.81.F0.80.C0.80.80.80.80.84.90.9C.9C.8E.B8.88.88.80.80.80.80.80.80.80.80.80


@END

;THROWING KNIFE (small)
@START
THROWING_KNIFE_SMALL.SHAPE.START
;;----------
WEAPON.THROWING_KNIFE.SMALL.SHAPE_0.AND_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80
WEAPON.THROWING_KNIFE.SMALL.SHAPE_0.ORA_MASK			.HS	80.80.80.80.80.80.80.80.80.88.80.84.A0.82.C0.81.C0.81.A0.82.80.80.80.80.80.80.80.80.80.80.80.80
				
WEAPON.THROWING_KNIFE.SMALL.SHAPE_1.AND_MASK			.HS	00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00
WEAPON.THROWING_KNIFE.SMALL.SHAPE_1.ORA_MASK			.HS	80.80.80.80.80.80.80.80.80.80.A0.82.C0.81.C0.81.A0.82.80.84.80.88.80.80.80.80.80.80.80.80.80.80
				
WEAPON.THROWING_KNIFE.SMALL.SHAPE_2.AND_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80
WEAPON.THROWING_KNIFE.SMALL.SHAPE_2.ORA_MASK			.HS	80.80.80.80.80.80.80.80.80.80.C0.84.80.83.80.83.C0.84.A0.80.90.80.80.80.80.80.80.80.80.80.80.80
				
WEAPON.THROWING_KNIFE.SMALL.SHAPE_3.AND_MASK			.HS	00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00
WEAPON.THROWING_KNIFE.SMALL.SHAPE_3.ORA_MASK			.HS	80.80.80.80.80.80.80.80.90.80.A0.80.C0.84.80.83.80.83.C0.84.80.80.80.80.80.80.80.80.80.80.80.80



@END

;THROWING KNIFE (large)
@START
THROWING_KNIFE_LARGE.SHAPE.START
;----------
WEAPON.THROWING_KNIFE.SHAPE_0.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.FF.F3.FF.C0.A7.C0.81.E0.81.F0.87.F8.87.FC.81.F8.81.F8.A7.FE.FF.FF.FF.FF.FF.FF
WEAPON.THROWING_KNIFE.SHAPE_0.ORA_MASK			.HS	80.80.80.80.80.80.80.80.80.8C.80.8E.98.87.D8.83.E0.81.E0.80.D8.81.D8.81.80.80.80.80.80.80.80.80
				
WEAPON.THROWING_KNIFE.SHAPE_1.AND_MASK			.HS	7F.7F.7F.7F.7F.7F.27.7E.01.78.01.78.07.7C.01.7C.01.78.07.70.1F.60.3F.40.7F.40.7F.73.7F.7F.7F.7F
WEAPON.THROWING_KNIFE.SHAPE_1.ORA_MASK			.HS	80.80.80.80.80.80.80.80.D8.81.D8.81.E0.80.F8.80.D8.81.C0.83.80.87.80.8E.80.8C.80.80.80.80.80.80
				
WEAPON.THROWING_KNIFE.SHAPE_2.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.BF.F2.8F.C0.8F.C0.9F.F0.8F.C0.87.C0.83.F0.81.FE.81.FF.E7.FF.FF.FF.FF.FF.FF.FF
WEAPON.THROWING_KNIFE.SHAPE_2.ORA_MASK			.HS	80.80.80.80.80.80.80.80.C0.8D.C0.8D.80.83.C0.8F.E0.8D.F0.80.B8.80.98.80.80.80.80.80.80.80.80.80
				
WEAPON.THROWING_KNIFE.SHAPE_3.AND_MASK			.HS	7F.7F.7F.7F.7F.7F.67.7F.01.7F.01.7E.03.70.07.40.0F.40.1F.70.0F.40.0F.40.3F.72.7F.7F.7F.7F.7F.7F
WEAPON.THROWING_KNIFE.SHAPE_3.ORA_MASK			.HS	80.80.80.80.80.80.80.80.98.80.B8.80.F0.80.E0.8D.C0.8F.80.83.C0.8D.C0.8D.80.80.80.80.80.80.80.80
@END

;COW
@START
MISC.COW.SHAPE_0.AND_MASK			.HS	FF.FF.E0.80.C0.80.E0.81.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80
MISC.COW.SHAPE_0.ORA_MASK			.HS	80.80.86.C4.89.EC.84.B8.9E.BC.FE.BC.CE.BF.C6.BD.E6.9C.FE.9C.F2.B3.F2.B0.FC.BF.D4.BF.DB.BC.89.E6
				
MISC.COW.SHAPE_1.AND_MASK			.HS	7F.00.7F.00.7F.01.1C.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.03.70.6F.70.7F
MISC.COW.SHAPE_1.ORA_MASK			.HS	80.C4.80.EC.80.B8.80.BE.E3.BD.FC.BC.E6.9C.E6.93.F2.B1.D2.BF.D6.BF.D6.B8.C6.A4.8E.90.83.80.83.80
				
MISC.COW.SHAPE_2.AND_MASK			.HS	FF.FF.E0.80.C0.80.E0.81.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80
MISC.COW.SHAPE_2.ORA_MASK			.HS	80.80.86.C4.89.EC.84.B8.9E.BC.FE.BC.CE.BF.C6.BD.E6.9C.FE.9C.F2.B3.F2.B0.FC.BF.D4.BF.DB.BC.89.E6
				
MISC.COW.SHAPE_3.AND_MASK			.HS	7F.00.7F.00.7F.01.1C.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.03.70.6F.70.7F
MISC.COW.SHAPE_3.ORA_MASK			.HS	80.C4.80.EC.80.B8.80.BE.E3.BD.FC.BC.E6.9C.E6.93.F2.B1.D2.BF.D6.BF.D6.B8.C6.A4.8E.90.83.80.83.80
@END

;FIREBALL (range weapon only, spells hav their own shape tables)
@START
PROJECTLE.FIREBALL.SHAPE_0.AND_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80
PROJECTLE.FIREBALL.SHAPE_0.ORA_MASK			.HS	80.80.80.80.80.80.80.80.88.80.82.80.80.80.A0.81.88.C0.80.D0.A0.80.E0.80.82.C0.AA.84.A0.D1.8E.C5
				
PROJECTLE.FIREBALL.SHAPE_1.AND_MASK			.HS	00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00
PROJECTLE.FIREBALL.SHAPE_1.ORA_MASK			.HS	80.80.80.80.80.80.80.80.80.80.82.80.A0.80.E0.C4.80.90.A2.80.86.80.80.C0.80.80.A8.84.82.91.AA.97
				
PROJECTLE.FIREBALL.SHAPE_2.AND_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80
PROJECTLE.FIREBALL.SHAPE_2.ORA_MASK			.HS	80.80.80.80.80.80.80.80.88.80.86.80.80.80.80.90.82.81.80.80.88.C1.80.93.80.80.80.84.8A.95.A0.84
				
PROJECTLE.FIREBALL.SHAPE_3.AND_MASK			.HS	00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00
PROJECTLE.FIREBALL.SHAPE_3.ORA_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.88.80.E2.84.80.91.A0.80.82.84.80.80.A0.80.88.84.80.95.8A.C1

; PROJECTLE.FIREBALL.SHAPE_0.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.F7.FF.C1.FF.F0.FF.DD.FE.87.B8.C1.8E.D7.83.87.AE.85.BC.90.CB.80.A0.81.80.80.80
; PROJECTLE.FIREBALL.SHAPE_0.ORA_MASK			.HS	80.80.80.80.80.80.80.80.88.80.82.80.80.80.A0.81.88.C0.80.D0.A0.80.E0.80.82.C0.AA.84.A0.D1.8E.C5
				
; PROJECTLE.FIREBALL.SHAPE_1.AND_MASK			.HS	7F.7F.7F.7F.7F.7F.7F.7F.7D.7F.50.7F.05.3A.07.40.1D.03.00.6E.40.3F.79.4F.57.3B.01.60.10.00.00.00
; PROJECTLE.FIREBALL.SHAPE_1.ORA_MASK			.HS	80.80.80.80.80.80.80.80.80.80.82.80.A0.80.E0.C4.80.90.A2.80.86.80.80.C0.80.80.A8.84.82.91.AA.97
				
; PROJECTLE.FIREBALL.SHAPE_2.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.F7.FF.C1.FF.E0.FF.F9.EF.FD.82.90.E8.F5.BE.81.C8.97.80.FF.E8.F5.E0.80.80.85.E0
; PROJECTLE.FIREBALL.SHAPE_2.ORA_MASK			.HS	80.80.80.80.80.80.80.80.88.80.86.80.80.80.80.90.82.81.80.80.88.C1.80.93.80.80.80.84.8A.95.A0.84
				
; PROJECTLE.FIREBALL.SHAPE_3.AND_MASK			.HS	7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.77.7F.01.7B.00.60.1D.00.05.6A.50.60.5D.7B.07.7A.41.60.15.00.00.48
; PROJECTLE.FIREBALL.SHAPE_3.ORA_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.88.80.E2.84.80.91.A0.80.82.84.80.80.A0.80.88.84.80.95.8A.C1
@END

;ICEBALL (range weapon only, spells hav their own shape tables)
@START

PROJECTLE.ICEBALL.SHAPE_0.AND_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80
PROJECTLE.ICEBALL.SHAPE_0.ORA_MASK			.HS	80.80.80.80.80.80.80.80.84.80.81.80.80.80.D0.80.84.A0.80.A8.90.80.B0.80.81.A0.95.82.D0.A8.C7.A2
				
PROJECTLE.ICEBALL.SHAPE_1.AND_MASK			.HS	00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00
PROJECTLE.ICEBALL.SHAPE_1.ORA_MASK			.HS	80.80.80.80.80.80.80.80.80.80.81.80.90.80.B0.A2.80.88.91.80.83.80.80.A0.80.80.94.82.C1.88.D5.8B
				
PROJECTLE.ICEBALL.SHAPE_2.AND_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80
PROJECTLE.ICEBALL.SHAPE_2.ORA_MASK			.HS	80.80.80.80.80.80.80.80.84.80.83.80.80.80.80.88.C1.80.80.80.C4.A0.C0.89.80.80.80.82.C5.8A.90.82
				
PROJECTLE.ICEBALL.SHAPE_3.AND_MASK			.HS	00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00
PROJECTLE.ICEBALL.SHAPE_3.ORA_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.84.80.B1.82.C0.88.90.80.81.82.80.80.90.80.84.82.C0.8A.C5.A0


; EMPTY.AND_MASK0 .HS	80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80
; ;PROJECTLE.ICEBALL.SHAPE_0.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.FB.FF.E0.FF.F8.FF.AE.FF.83.DC.A0.87.EB.81.83.D7.82.DE.C8.85.80.D0.80.80.80.80
; PROJECTLE.ICEBALL.SHAPE_0.ORA_MASK			.HS	80.80.80.80.80.80.80.80.84.80.81.80.80.80.D0.80.84.A0.80.A8.90.80.B0.80.81.A0.95.82.D0.A8.C7.A2
				
; EMPTY.AND_MASK1 .HS	80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80
; ;PROJECTLE.ICEBALL.SHAPE_1.AND_MASK			.HS	7F.7F.7F.7F.7F.7F.7F.7F.7E.7F.68.7F.02.5D.03.00.4E.41.00.77.60.5F.7C.07.6B.5D.00.70.08.40.00.40
; PROJECTLE.ICEBALL.SHAPE_1.ORA_MASK			.HS	80.80.80.80.80.80.80.80.80.80.81.80.90.80.B0.A2.80.88.91.80.83.80.80.A0.80.80.94.82.C1.88.D5.8B
				
; EMPTY.AND_MASK2 .HS	80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80
; ;PROJECTLE.ICEBALL.SHAPE_2.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.FB.FF.E0.FF.F0.FF.FC.F7.BE.C1.88.F4.BA.DF.80.84.8B.C0.BF.F4.BA.F0.80.C0.82.F0
; PROJECTLE.ICEBALL.SHAPE_2.ORA_MASK			.HS	80.80.80.80.80.80.80.80.84.80.83.80.80.80.80.88.C1.80.80.80.C4.A0.C0.89.80.80.80.82.C5.8A.90.82
				
; EMPTY.AND_MASK3 .HS	80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80
; ;PROJECTLE.ICEBALL.SHAPE_3.AND_MASK			.HS	7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7B.7F.40.7D.00.70.0E.40.02.75.28.70.6E.7D.03.7D.20.70.0A.40.00.04
; PROJECTLE.ICEBALL.SHAPE_3.ORA_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.84.80.B1.82.C0.88.90.80.81.82.80.80.90.80.84.82.C0.8A.C5.A0
@END

;BOULDER
@START
PROJECTILE.BOULDER.SHAPE_0.AND_MASK			.HS	FF.FF.FF.FF.DF.FF.87.FC.87.F0.83.E0.81.80.81.C0.81.C0.81.C0.80.C0.80.E0.83.F0.9F.E0.FF.F9.FF.FF
PROJECTILE.BOULDER.SHAPE_0.ORA_MASK			.HS	00.00.00.00.00.00.20.00.60.03.70.07.78.1D.58.0F.78.0F.78.0F.7C.0E.74.07.60.03.00.06.00.00.00.00
				
PROJECTILE.BOULDER.SHAPE_1.AND_MASK			.HS	7F.7F.7F.7F.5F.7F.07.7C.07.70.03.60.01.00.01.40.01.40.01.40.00.40.00.60.03.70.1F.60.7F.79.7F.7F
PROJECTILE.BOULDER.SHAPE_1.ORA_MASK			.HS	00.00.00.00.00.00.20.00.60.03.70.07.78.1D.58.0F.78.0F.78.0F.7C.0E.74.07.60.03.00.06.00.00.00.00
				
PROJECTILE.BOULDER.SHAPE_2.AND_MASK			.HS	FF.FF.FF.FF.DF.FF.87.FC.87.F0.83.E0.81.80.81.C0.81.C0.81.C0.80.C0.80.E0.83.F0.9F.E0.FF.F9.FF.FF
PROJECTILE.BOULDER.SHAPE_2.ORA_MASK			.HS	00.00.00.00.00.00.20.00.60.03.70.07.78.1D.58.0F.78.0F.78.0F.7C.0E.74.07.60.03.00.06.00.00.00.00
				
PROJECTILE.BOULDER.SHAPE_3.AND_MASK			.HS	7F.7F.7F.7F.5F.7F.07.7C.07.70.03.60.01.00.01.40.01.40.01.40.00.40.00.60.03.70.1F.60.7F.79.7F.7F
PROJECTILE.BOULDER.SHAPE_3.ORA_MASK			.HS	00.00.00.00.00.00.20.00.60.03.70.07.78.1D.58.0F.78.0F.78.0F.7C.0E.74.07.60.03.00.06.00.00.00.00
@END

;(disabled) ;UPSIDE DOWN COW
@START
; PROJECTILE.COW.UPSIDE_DOWN.SHAPE_0.AND_MASK			.HS	FF.FF.FF.BB.F7.80.C1.80.81.81.83.80.81.80.80.80.80.80.80.80.80.80.80.80.80.80.80.CE.80.FE.DD.FF
; PROJECTILE.COW.UPSIDE_DOWN.SHAPE_0.ORA_MASK			.HS	80.80.80.80.80.C4.88.EC.18.38.B0.3A.70.3B.F2.1B.7E.33.34.31.1C.39.7C.7F.1C.30.36.00.22.00.00.00
				
; PROJECTILE.COW.UPSIDE_DOWN.SHAPE_1.AND_MASK			.HS	7F.7F.77.0F.41.03.01.00.01.00.00.00.00.00.01.00.03.00.00.00.00.00.00.00.00.00.00.0E.00.7E.5D.7F
; PROJECTILE.COW.UPSIDE_DOWN.SHAPE_1.ORA_MASK			.HS	80.80.80.C0.88.E0.98.E4.30.3C.B2.3A.7E.3B.F8.1B.70.33.34.31.1C.39.7C.3F.1C.70.36.00.22.00.00.00
				
; PROJECTILE.COW.UPSIDE_DOWN.SHAPE_2.AND_MASK			.HS	FF.FF.FF.BB.F7.80.C1.80.81.81.80.80.80.80.81.80.83.80.80.80.80.80.80.80.80.80.80.86.80.9E.DD.FF
; PROJECTILE.COW.UPSIDE_DOWN.SHAPE_2.ORA_MASK			.HS	80.80.80.80.80.C4.88.EC.98.38.B2.3A.7E.3B.F8.1B.70.33.34.31.1C.39.7C.3F.1C.30.36.60.22.00.00.00
				
; PROJECTILE.COW.UPSIDE_DOWN.SHAPE_3.AND_MASK			.HS	7F.7F.7F.60.7F.00.77.00.41.01.01.00.00.00.00.00.01.00.00.00.00.00.00.00.00.00.00.0E.00.7E.5D.7F
; PROJECTILE.COW.UPSIDE_DOWN.SHAPE_3.ORA_MASK			.HS	80.80.80.84.80.8C.80.CC.88.78.B8.3A.F2.3B.FE.1B.70.33.34.31.1C.39.7C.3F.1C.70.36.00.22.00.00.00

@END

;POISON BALL (VIOLET)
@START
; PROJECTLE.POISON_BALL.SHAPE_0.AND_MASK			.HS	00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00
; PROJECTLE.POISON_BALL.SHAPE_0.ORA_MASK			.HS	00.00.00.00.00.00.00.00.04.00.01.00.00.00.50.00.04.20.00.28.10.00.30.00.01.20.15.02.50.28.47.22
				
; PROJECTLE.POISON_BALL.SHAPE_1.AND_MASK			.HS	00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00
; PROJECTLE.POISON_BALL.SHAPE_1.ORA_MASK			.HS	00.00.00.00.00.00.00.00.00.00.01.00.10.00.30.22.00.08.11.00.03.00.00.20.00.00.14.02.41.08.55.0B
				
; PROJECTLE.POISON_BALL.SHAPE_2.AND_MASK			.HS	00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00
; PROJECTLE.POISON_BALL.SHAPE_2.ORA_MASK			.HS	00.00.00.00.00.00.00.00.04.00.03.00.00.00.00.08.41.00.00.00.44.20.40.09.00.00.00.02.45.0A.10.02
				
; PROJECTLE.POISON_BALL.SHAPE_3.AND_MASK			.HS	00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00
; PROJECTLE.POISON_BALL.SHAPE_3.ORA_MASK			.HS	00.00.00.00.00.00.00.00.00.00.00.00.04.00.31.02.40.08.10.00.01.02.00.00.10.00.04.02.40.0A.45.20
@END

;POISON BALL (GREEN)
@START
PROJECTLE.POISONBALL.GREEN.SHAPE_0.AND_MASK			.HS	00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00
PROJECTLE.POISONBALL.GREEN.SHAPE_0.ORA_MASK			.HS	00.05.00.05.18.11.28.18.28.00.20.03.60.05.60.05.60.05.32.05.28.05.02.01.00.00.00.00.00.00.00.00
				
PROJECTLE.POISONBALL.GREEN.SHAPE_1.AND_MASK			.HS	00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00
PROJECTLE.POISONBALL.GREEN.SHAPE_1.ORA_MASK			.HS	06.30.0A.00.0A.00.00.00.20.00.20.01.20.01.30.15.30.15.60.05.60.05.30.05.28.15.28.01.00.00.00.00
				
PROJECTLE.POISONBALL.GREEN.SHAPE_2.AND_MASK			.HS	00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00
PROJECTLE.POISONBALL.GREEN.SHAPE_2.ORA_MASK			.HS	00.00.20.00.00.00.00.00.00.00.20.04.28.05.28.55.28.47.20.47.60.0D.20.01.00.00.08.10.42.41.20.01
				
PROJECTLE.POISONBALL.GREEN.SHAPE_3.AND_MASK			.HS	00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00
PROJECTLE.POISONBALL.GREEN.SHAPE_3.ORA_MASK			.HS	00.00.00.00.00.00.00.05.00.45.00.01.20.07.2A.07.2A.4D.22.05.20.61.20.03.20.00.00.00.00.00.00.00
@END

@END

	.EP ;end patch mode
	
WEAPON.SHAPE_TABLES.END_ADDRESS ;this label is used by the game loader to find the start of the code to copy to aux BSR

;==========================================


;=====SPELL FILE OFFSETS====
@START

;SPELL_FILE.GRAPHICAL.EFFECTS.SEEK_BYTES = SPELL_FILE.ENTRANCE-SPELL_FILE.START
SPELL_FILE.GRAPHICAL.EFFECTS.READ_BYTES = SPELL_FILE.GRAPHICAL.EFFECTS.END-SPELL_FILE.START

GE.SPELL.MAGIC_MISSLE.SEEK_BYTES = GE.SPELL.MAGIC_MISSLE.START-SPELL_FILE.START
GE.SPELL.MAGIC_MISSLE.READ_BYTES = GE.SPELL.MAGIC_MISSLE.END-GE.SPELL.MAGIC_MISSLE.START

GE.SPELL.FLAME_ARROW.SEEK_BYTES = GE.SPELL.FLAME_ARROW.START-SPELL_FILE.START
GE.SPELL.FLAME_ARROW.READ_BYTES = GE.SPELL.FLAME_ARROW.END-GE.SPELL.FLAME_ARROW.START

GE.SPELL.FIREBALL.SEEK_BYTES = GE.SPELL.FIREBALL.START-SPELL_FILE.START
GE.SPELL.FIREBALL.READ_BYTES = GE.SPELL.FIREBALL.END-GE.SPELL.FIREBALL.START
	
GE.SPELL.LIGHTNING.SEEK_BYTES = GE.SPELL.LIGHTNING.START-SPELL_FILE.START
GE.SPELL.LIGHTNING.READ_BYTES = GE.SPELL.LIGHTNING.END-GE.SPELL.LIGHTNING.START
	
GE.SPELL.SUMMON.SEEK_BYTES = GE.SPELL.SUMMON.START-SPELL_FILE.START
GE.SPELL.SUMMON.READ_BYTES = GE.SPELL.SUMMON.END-GE.SPELL.SUMMON.START

GE.SPELL.DIE.SEEK_BYTES = GE.SPELL.DIE.START-SPELL_FILE.START
GE.SPELL.DIE.READ_BYTES = GE.SPELL.DIE.END-GE.SPELL.DIE.START

GE.SPELL.MASS_DEATH.SEEK_BYTES = GE.SPELL.MASS_DEATH.START-SPELL_FILE.START
GE.SPELL.MASS_DEATH.READ_BYTES = GE.SPELL.MASS_DEATH.END-GE.SPELL.MASS_DEATH.START
	
GE.SPELL.SMITE.SEEK_BYTES = GE.SPELL.SMITE.START-SPELL_FILE.START
GE.SPELL.SMITE.READ_BYTES = GE.SPELL.SMITE.END-GE.SPELL.SMITE.START

GE.SPELL.SHOCK_SPHERE.SEEK_BYTES = GE.SPELL.SHOCK_SPHERE.START-SPELL_FILE.START
GE.SPELL.SHOCK_SPHERE.READ_BYTES = GE.SPELL.SHOCK_SPHERE.END-GE.SPELL.SHOCK_SPHERE.START

GE.SPELL.MASS_SHOCK_SPHERE.SEEK_BYTES = GE.SPELL.MASS_SHOCK_SPHERE.START-SPELL_FILE.START
GE.SPELL.MASS_SHOCK_SPHERE.READ_BYTES = GE.SPELL.MASS_SHOCK_SPHERE.END-GE.SPELL.MASS_SHOCK_SPHERE.START

GE.SPELL.COMBAT_TELEPORT_PARTY.SEEK_BYTES = GE.SPELL.COMBAT_TELEPORT_PARTY.START-SPELL_FILE.START
GE.SPELL.COMBAT_TELEPORT_PARTY.READ_BYTES = GE.SPELL.COMBAT_TELEPORT_PARTY.END-GE.SPELL.COMBAT_TELEPORT_PARTY.START



;---weapons

WEAPON.SHAPE_TABLES.SEEK_BYTES = WEAPON.SHAPE_TABLES.FILE_START_ADDRESS-SPELL_FILE.START
WEAPON.SHAPE_TABLES.READ_BYTES = WEAPON.SHAPE_TABLES.END_ADDRESS-WEAPON.SHAPE_TABLES.FILE_START_ADDRESS
		

@END

