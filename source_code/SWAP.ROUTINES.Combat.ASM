; ============================================
; Copyright (C) 2016-2018. 6502 Workshop, LLC
; contact: mark@6502workshop.com
; ============================================



;************************INCLUDE FILE*****************************
;(do not assemble stand-alone.  )
				.TF     swap.routines.combat.bin,BIN
				.OR		SRTN.COMBAT.ADDRESS		;**Always put before .TF directive and never use again in program
				.EB		OFF


;=====SWAP.ROUTINES.COMBAT.ASM DOCUMENTATION====================================
;
;COMBAT is the main entrance to the combat module; JSR from any other map to initiate combat.
;
;A lot of memory swapping is integrated into this module.
;See memory map below for more details. 
;
;
;==Casting Spells==
;
;CAST SPELL FLOW CONTROL (**see COMMAND.CAST_SPELL subroutine docs**)
; 
;The graphics effects and target hit identification code is stored in a spell file (SWAP.ROUTINES.combat.spells.ASM). When a spell is cast,
;a keypress is collected from the player. As long as it is a spell hotkey, the main combat progam is swapped out to aux memory, then the 
;graphics entrance routine is loaded into the spell file entrance buffer.
;
;The code loaded into the entrance buffer identifies the code block associated with the spell and sets most or all parameteres required. In some cases there are a few 
;parameters set within the code block, in cases where multiple spells share the same block. 
;
;After the code block is indentified, the code block is loaded into the code block buffer, and control is passed to the code block. 
;Once the spell code block completes, the main combat program is swapped back in. 
;
;The map object arrays are not swapped in or out. They are fixed in memory because the spell code blocks sometimes modify them (i.e. summoning spells).
;Other routines that stay fixed in memory include the graphics subroutines (called by the spell code blocks, and also by the attack command), and the code that handles the swap before/after the spell file is loaded. 
;
;If more memory is needed for the spell code block buffer,
;if could probably clobber part of the spell entrance buffer because the
;last routine loaded in the entrace buffer is COMBAT.SELECT.ATTACK_TARGET. As long
;as the code to load the code_block buffer is lower in memory than the select spell target routine, it seems like
;the code_block buffer start could just be lowered to the start of the select spell target routine and it should work fine.
;
;
;==Attack (melee/ranged)==
;
;The angled projectile shapes for the attack command are stored in
;aux memory ($D000-$FFFF). They are initally loaded from disk (the spell file) into aux memory
;in LOADER.P.ASM. This aux memory range is also used for AUX SWAP2,
;which is where the main combat module (except for CAST_SPELL.ENTRANCE) is swapped out to when a spell list cast,
;triggered a load of the spell file (see casting spell section above). 
;
;Thus, the spell file clobbers the weapon angled projectiles.
;The weapons angles shapes are loaded from disk back into aux memory in 
;.RESTORE.WEAPONS.SHAPE_TABLES (CAST_SPELL.ENTRANCE)
;
;*non-angled animated shapes (weapons)
;they are also stored in the spell file in the weapons shape section (see WEAPON.SHAPE_TABLES.FILE_START_ADDRESS)
;
;weapons animated shapes are handled differently from spells, which have their shape table is loaded into SHAPE.HOPPER0 via in the spell file code block.
;Tor weapon animated shapes, the shape table needs to be loaded from aux memory. This is done
;in the SELECT.SHAPE_TABLE routine based on the SHAPE.MOVE.MODE parameter. 
;
;
;==Inventory Manager (tenative)==
;When the player launches the inventory manager, the entire combat module is swapped out to aux memory
;and when the player exists the inventory manager, the combat module is swapped back in. 
;
;
;===MODULE MEMORY MAP===
;$9600-$BCFF	main combat program (swaps out to SWAP_SPACE2.AUX_MEMORY when spell file swaps in), (clobbers NPC schedule array; it's not needed)
	;$B5C0-$BCFF graphics subroutines, and spell swap routines. Remains in memory when spell file is swapped in.
;$9600-$AEFF	spell file buffer (swaps in)
	;$9600-$9AFF entrance buffer (separate because it is used to parse player input to select the code block for the spell)
	;$9B00-$AEFF spell code block buffer. available for spell graphic shapes and associated code
;$BA00-BFFF*	Map Object Arrays (npc schedule, mob, general, and npc) ; not all required for COMBAT
	;$BD00-BFFF*	Mob, General and NPC map object arrays (required for COMBAT, but note that in combat MOBs use the NPC array and PCs use the MOB array)
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
;===IDEAS TO FREE UP MEMORY===
;
;COMBAT.MAP_OBJECT.MOB only uses $4bytes per record * $6 player characters. One record after that needs to have $00 in byte $02 since that is the 
;stop value used in map_objects.manager.ASM. But, the rest of the array ($100 bytes total size) could be used by the combat module for something else. 
;
;=================================================================================

;TESTING.S_ENTITY.DATA
@START

;RESERVED AREA FOR COMBAT SPECIFIC VARIABLES/TABLES

;COMBAT.MAP_OBJECTS.PC.INIT .HS	04.08.00.00.06.08.00.00.03.09.00.00.07.09.00.00.02.0A.00.00.08.0A.00.00

;projectile animation testing

;center;(player 1 was 2,8)
;COMBAT.MAP_OBJECTS.PC.INIT .HS	08.06.00.00.05.05.00.00.05.0A.00.00.07.09.00.00.02.0A.00.00.08.0A.00.00

;upper right
;COMBAT.MAP_OBJECTS.PC.INIT .HS	00.02.00.00.0A.00.00.00.03.09.00.00.07.09.00.00.02.0A.00.00.08.0A.00.00


;1mob
;COMBAT.TEMP.MOB.INIT 	  .HS	08.04.97.01.01.03.00.00.00.00.00.00.00.00.00.00

;2mob
;COMBAT.TEMP.MOB.INIT 	  .HS	03.04.97.01.01.03.00.00.05.04.97.01.01.03.00.00

;4mob
;COMBAT.TEMP.MOB.INIT 	  .HS	02.05.B7.01.01.03.00.00.01.01.8D.01.01.03.00.00.06.01.B9.01.01.03.00.00.09.02.BA.01.01.03.00.00
;COMBAT.TEMP.MOB.INIT 	  .HS	04.04.B7.01.01.03.00.00.05.04.8D.01.01.03.00.00.07.04.B9.01.01.03.00.00.04.03.BA.01.01.03.00.00

;5mob
 ;cows testing
;COMBAT.TEMP.MOB.INIT 	  .HS	09.02.B7.01.01.03.00.00.07.03.8D.01.01.03.00.00.05.04.B9.01.01.03.00.00.02.01.BA.01.01.03.00.00.0A.09.B7.01.01.03.00.00
 
 
 ;MD cast east target validation
;COMBAT.TEMP.MOB.INIT 	  .HS	09.02.B7.01.01.03.00.00.07.03.8D.01.01.03.00.00.05.04.B9.01.01.03.00.00.08.07.BA.01.01.03.00.00.0A.09.B7.01.01.03.00.00
 ;MD cast west target validation
;COMBAT.TEMP.MOB.INIT 	  .HS	02.01.B7.01.01.03.00.00.02.02.8D.01.01.03.00.00.02.03.B9.01.01.03.00.00.04.04.BA.01.01.03.00.00.01.07.B7.01.01.03.00.00
 ;MD cast south target validation
;COMBAT.TEMP.MOB.INIT 	  .HS	07.06.B7.01.01.03.00.00.03.07.8D.01.01.03.00.00.06.07.B9.01.01.03.00.00.08.09.BA.01.01.03.00.00.01.0A.B7.01.01.03.00.00
 ;MD cast north target validation
;COMBAT.TEMP.MOB.INIT 	  .HS	02.03.B7.01.01.03.00.00.03.03.8D.01.01.03.00.00.04.04.B9.01.01.03.00.00.06.04.BA.01.01.03.00.00.0A.00.B7.01.01.03.00.00
;COMBAT.TEMP.MOB.INIT 	  .HS	02.05.B7.01.01.03.00.00.01.01.8D.01.01.03.00.00.06.01.B9.01.01.03.00.00.09.02.BA.01.01.03.00.00.05.04.B7.01.01.03.00.00

;all orcs
;COMBAT.TEMP.MOB.INIT 	  .HS	04.04.97.01.01.03.00.00.05.04.97.01.01.03.00.00.07.04.97.01.01.03.00.00.04.03.97.01.01.03.00.00

;8mobs
;COMBAT.TEMP.MOB.INIT 	  .HS	03.04.97.01.01.03.00.00.05.04.97.01.01.03.00.00.07.04.97.01.01.03.00.00.04.03.97.01.01.03.00.00.06.03.97.01.01.03.00.00.03.02.97.01.01.03.00.00.05.02.97.01.01.03.00.00.07.02.97.01.01.03.00.00

;**OPT** Memory. Figure out away to put these hex table in throw away memory.
@END


COMBAT.MAIN ;INIT AND MAIN LOOP
@START				
;=====================SUBROUTINE DOCUMENTATION====================================
;
;Graphics Summary
;*All Map Objects are drawn after each player character move, and after each mob. **OPT** Speed. Insert code at the top of the MANAGE.OBJECTS (map_objects.manager.ASM) where the decision is made whether the next array record is empty for the sprite and general object arrays. Also Make that decision based on whether X-REG is = the active NPC or Mob. If it is > then exit, no need to process more records. 
;*COPY.SCREEN is executed after each move. If this isn't done, then the erase of the S_ENTITY who moved will only occur on one page. **OPT** Speed. This should go faster if the erase was just done on both pages. 
;
;=================================================================================


				

			
.INIT.MISC

.LOAD.WEAPONS.SHAPE_TABLES
@START
;=====================SUBROUTINE DOCUMENTATION====================================
;
;This routine loads the shape tables for weapon projectiles. This will clobber the
;AUX BSR routines. This is okay because they are not needed by the COMBAT module.
;they are restored in the COMBAT.EXIT routines. 
;
;
;=================================================================================


;---hang doesn't occur here


			
	;must be called from the main combat module. 
	;(can't be called from  the combat setup file because it is a routine in the main combat module)
	JSR LOAD.WEAPONS.SHAPE_TABLES


@END


;-debug hang occurs here

			
COMBAT.INIT.MOB.HP_MAX
@START

	
	LDX #$01 ;init loop counter and player #
.LOOP.COPY.MOB.HP_MAX
	;read MOB character sheet data
		TXA
		;ACC = MOB sequential # (high-bit not set = read mode)
	JSR COMBAT.READ_WRITE.CHR_SHEET.MOB
		;RETURN VALUE = CHR_SHEET.RECORD.READ

	;calculate CHR_SHEET_EXTENDED index
	TXA
	SEC
	SBC #$01 ;subtract 1 because X-REG loop counter starts at #$01 and array starts at $00
	ASL ;X2 (array record size is 2 byte)
	TAY ;transfer to Y-REG for use as CHR_SHEET_EXTENDED index
	
	;copy MOB HP to MOB HP MAX
	;(since combat hasn't started yet, MOB HP is set to the max)

	LDA CHR_SHEET.PC_MOB.HP_LO	
	
		;;DEMO.MOD.MOB_HP_MAX
		; LDA #$A8	
	STA CHR_SHEET_EXTENDED.MOB.HP_MAX,Y ;save LO byte
	
	INY ;advance to HO byte field in record
	
	LDA CHR_SHEET.PC_MOB.HP_HO

		; ;DEMO.MOD.MOB_HP_MAX
		; LDA #$00
	STA CHR_SHEET_EXTENDED.MOB.HP_MAX,Y ;save HO byte
		
		;exit test
	CPX #$20 ;!16 mobs + !16 specials
	BEQ .LOOP.COPY.MOB.HP_MAX.COMPLETE
	INX	;next mob/special
	JMP .LOOP.COPY.MOB.HP_MAX
.LOOP.COPY.MOB.HP_MAX.COMPLETE

		; lda #$aa
		; LDX #CHR_SHEET_EXTENDED.MOB.HP_MAX
		; LDY /CHR_SHEET_EXTENDED.MOB.HP_MAX
		; jsr prep.brk
		; brk
			
@END

	
	
COMBAT.GAME_STATE.LOOP.INIT
@START
COMBAT.REVIEW.TURN_STATUS ;**OPT** Memory. Consolidate turn management into one loop. Load any S_ENTITY data needed into a holding record (like sprite.record). That should help. COMBAT.TARGET.RECORD setup for this already, but probably can't share memory with it. 

			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; LDA COMBAT.TURN_STATUS ;set combat turn to PCs. $00 = PCs turn. $01 = Special(s) S_ENTITY, $02 = MOBs turn
			; BNE .TEMP
			; LDA COMBAT.PC.ACTIVE
			; CMP #$01
			; BNE .TEMP
			; JSR KEYIN
			; LDA #$BB
			; ;LDX COMBAT.ATTACK_COMMAND.ITERATION_COUNTER	;($00 = left hand | $01 = right hand)	
			; LDX COMBAT.TURN_STATUS ;load turn status. $00 = PCs turn. $01 = Special(s) S_ENTITY, $02 = MOBs turn
			; LDY COMBAT.PC.ACTIVE.LOCK	;did game player lock active combat player to a specific player #? 
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP


			
			
	LDA COMBAT.TURN_STATUS ;load turn status. $00 = PCs turn. $01 = Special(s) S_ENTITY, $02 = MOBs turn
	BEQ .COMBAT.PC.TURN ;is it the Player Character's turn? If yes, branch to the combat game loop
	CMP #COMBAT.TURN_STATUS.SPECIAL ;is it the MOBs turn?
	BEQ .COMBAT.SPECIAL.TURN_STEP ;if yes, then branch to the subroutine that handles MOB turns.
	CMP #COMBAT.TURN_STATUS.MOB ;is it the MOBs turn?
	BEQ .COMBAT.MOBS.TURN_STEP ;if yes, then branch to the subroutine that handles MOB turns.

	;**defaults to players turn, add branch for specials and an error trap.

.COMBAT.SPECIAL.TURN_STEP
	JMP COMBAT.SPECIAL.TURN

.COMBAT.MOBS.TURN_STEP
	JMP COMBAT.MOBS.TURN

.COMBAT.PC.TURN
COMBAT.GAME_STATE.LOOP.ENTRANCE
	
.INIT
	; ;init COMMAND.ATTACK variables which need to stay constant during each individual PCs turn
	; ;(otherwise the PCs can attack multiple times with each hand by using the ESC abort key to exit COMMAND.ATTACK
	; ;and then reentering it)
	; LDA #$00
	; STA COMBAT.ATTACK_COMMAND.RH_STATUS.FLAG ;($00 = right hand attack not complete | $01 = right hand attack completed  | $02 target not in range OR no weapon readied in right hand)
	; STA COMBAT.ATTACK_COMMAND.LH_STATUS.FLAG ;($00 = left hand attack not complete | $01 = left hand attack complete | $02 target not in range OR no weapon readied in left hand)
	
	;check for active player lock
	LDA COMBAT.PC.ACTIVE.LOCK	;did game player lock active combat player to a specific player #? 
	BEQ .LOCK.CHECK.COMPLETE    ;if no, then proceed normaly. 
		;ACC = COMBAT.PC.ACTIVE.LOCK 
	JSR COMBAT.GET.ACTIVE_PLAYER.HEALTH_STATUS
		;RETURN VALUE: ACC = active player health status code
	CMP #COMBAT.S_ENTITY.STATUS.GOOD ;is locked active player's health status "good"? Did he/she eat the salmon?
	BEQ .LOCKED.PC.HEALTH_GOOD
	;locked active player can't move (i.e. dead/sleeping)
	LDA #$00
	STA COMBAT.PC.ACTIVE.LOCK ;clear active player lock
	JMP .LOCK.CHECK.COMPLETE

	
.LOCKED.PC.HEALTH_GOOD
	LDA COMBAT.PC.ACTIVE.LOCK ;load locked active player #
	STA COMBAT.PC.ACTIVE ;if yes, then set active player variables to the locked player
	SEC
	SBC #$01
	; BNE	.STANDARD.INCREMENT	 ;is player 1 active? If no, do a standard increment
	; LDA #$00 ;if yes, hard code the record value because otherwise the standard increment procedure would result in a multiplication of zero. 
	; STA COMBAT.PC.ACTIVE.RECORD
	; JMP .LOCK.CHECK.COMPLETE
.STANDARD.INCREMENT
	ASL ;X2
	ASL ;X4
	STA COMBAT.PC.ACTIVE.RECORD 
		
	;**FALLS THROUGH**

.LOCK.CHECK.COMPLETE

;DEBUG: aux BSR write-enabled test = pass

	;recalculate screen index in case locked active player was active
	JSR COMBAT.GET.ACTIVE_PLAYER.SCREEN.LOCATION		
		STY COMBAT.ACTIVE_PLAYER.SINDEX ;not used in this routine; save for future use
		STY COMBAT.ATTACKER.SINDEX 
		;**OPT** Memory. could COMBAT.ACTIVE_PLAYER.SINDEX just be a pointer to COMBAT.ATTACKER.SINDEX? as long as the 
		;instances were it is used are PC specific then the generic variables COMBAT.ATTACKER.SINDEX should have the correct value. 
		
;<X-REG IS OK> 		
	
COMBAT.GAME_STATE.LOOP.ALT_ENTRANCE ;used when commands are aborted. It is needed because it calculates the screen tile location of the active PC and loads it into YREG
	;init COMMAND.ATTACK variables which need to stay constant during each individual PCs turn
	;(otherwise the PCs can attack multiple times with each hand by using the ESC abort key to exit COMMAND.ATTACK
	;and then reentering it)
	LDA #$00
	STA COMBAT.ATTACK_COMMAND.RH_STATUS.FLAG ;($00 = right hand attack not complete | $01 = right hand attack completed  | $02 target not in range OR no weapon readied in right hand)
	STA COMBAT.ATTACK_COMMAND.LH_STATUS.FLAG ;($00 = left hand attack not complete | $01 = left hand attack complete | $02 target not in range OR no weapon readied in left hand)
	
	
	LDX COMBAT.PC.ACTIVE.RECORD ;**OPT** Memory. I think X-REG is already set to this variable via the COMBAT.GET.ACTIVE_PLAYER.SCREEN.LOCATION variable
				;**OPT** Memory. It looks like this routine is duplicate to the one just below .LOCK.CHECK.COMPLETE
		LDA COMBAT.MAP_OBJECTS.PC+$0,X ;load active player GMAP.X
		STA PARM.GMAP.X
		LDA COMBAT.MAP_OBJECTS.PC+$1,X ;load active player GMAP.Y
		STA PARM.GMAP.Y			
	JSR CONVERT.COLUMN_ROW.TO.SINDEX
		LDA RETURN.SCREEN_ARRAY_INDEX ;used below
		STA COMBAT.PC.ACTIVE.SINDEX
			;**OPT** Memory. Speed. The COMBAT.PC.ACTIVE.SINDEX was added late. Look for other occasions where this value is calculated from scratch and replace by loading this variable. 



			
				; ;TEMP ERROR TRAP1
				; CPY #$00
				; BNE .TEMP2
				; INC TROUBLESHOOTING.HOOK
				; LDA COMBAT.PC.ACTIVE
				; STA COW
				
; .TEMP2

			
;VALIDATE ENTRANCE
		
;Skip PCs Turn? 

	;Is PC Dead or Sleeping?
	LDA COMBAT.MAP_OBJECTS.PC+$3,X
	CMP #COMBAT.S_ENTITY.STATUS.SLEEPING			
	BCC .PC.STATUS.CHECK.COMPLETE ;if no	
	JMP INCREMENT.ACTIVE_PC.ALTERNATE.ENTRANCE ;if yes
.PC.STATUS.CHECK.COMPLETE

				; ;TEMP ERROR TRAP2
				; STY TEMP
				; LDY COMBAT.ACTIVE_PLAYER.SINDEX
				; CPY #$00
				; BNE .TEMP
				; LDA COMBAT.MAP_OBJECTS.PC+$3,X
				; STA $BE00
				; LDA COMBAT.PC.ACTIVE
				; STA $BE01
				; LDA COW
				; STA $BE02
				; LDA TROUBLESHOOTING.HOOK
				; STA $BE03
				; ; LDX COMBAT.PC.ACTIVE.RECORD
				; ; LDA COMBAT.MAP_OBJECTS.PC+$0,X ;load active player GMAP.X
				; ; STA SELECT_TARGET.CURRENT_TARGET.X
				; ; STA $BE00
				; ; LDA COMBAT.MAP_OBJECTS.PC+$1,X ;load active player GMAP.Y
				; ; STA SELECT_TARGET.CURRENT_TARGET.Y
				; ; STA $BE01
				; LDA #$DD
				; JSR PREP.BRK
				; BRK
				
; .TEMP
				; LDY TEMP
	

;DEBUG: aux BSR write-enabled test = pass
	

	
COMBAT.UPDATE.COMMAND_PROMPT
@START

				
				; lda #$01
				; sta troubleshooting.hook

			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; lda #$aa
			; ldx COMBAT.FIRST_PC_TURN.FLAG
			; ldy COMBAT.ABORT.FLAG
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP

			
	;don't print blank lines if first iteration
	;(because text window is empty and it doesn't look good)
	LDA COMBAT.FIRST_PC_TURN.FLAG	;($00 = first PC turn | >=$01 not first PC turn)
	BEQ .FIRST_TURN
	
	JSR PRINT.TEXT.WINDOW.CR ;print carriage return to combat scroll window

	JMP .ABORT.FLAG.CHECK	
	
.FIRST_TURN


;DEBUG: aux BSR write-enabled test = PASS
				
				
	;.update.chr_roster
	;(so any changes in PC HP/MP or health status is shown)
	JSR DISPLAY.CHARACTER.ROSTER

	
;DEBUG: aux BSR write-enabled test = FAIL


	
	;**FALLS THROUGH**
	
.ABORT.FLAG.CHECK		
	;abort flag check
	;(if player used ESC to abort a command, print carriage return
	LDA COMBAT.ABORT.FLAG		;($00 = not set, $01 = set)
	BEQ .ABORT.FLAG.CHECK.DONE

	JSR PRINT.TEXT.WINDOW.CR ;print carriage return to combat scroll window
		
	LDA #$00 ;clear abort flag
	STA COMBAT.ABORT.FLAG		;($00 = not set, $01 = set)
.ABORT.FLAG.CHECK.DONE

			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; lda #$aa
			; ldy COMBAT.PC.ACTIVE.SINDEX
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP
			
			

	;read character sheet for active player
		LDY COMBAT.PC.ACTIVE.SINDEX ;**OPT** Memory. This variable and COMBAT.ACTIVE_PLAYER.SINDEX seem to serve the same purpose. 
		LDA #$00 ;set read mode ;($00 = read | $01 = write)			
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet for COMBAT.ACTIVE_PLAYER.SINDEX
		;RETURN: CHR_SHEET.RECORD.READ($80)

		
	;print active player name		
	JSR COMBAT.PRINT.PC_NAME


	;print command prompt	
		LDA #COMBAT.TEXT_BLOCK.COMMAND_PROMPT					
		STA TWF.STRING
		
		LDA /COMBAT.TEXT_BLOCK.COMMAND_PROMPT
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW

@END

	;lda #$01
		
		
@END

	;**FALLS THROUGH**
	
COMBAT.GAME_STATE.LOOP ;entrance is above section, COMBAT.GAME_STATE.LOOP.INIT
@START
;======CONTINOUS LOOP UNTIL KEY PRESS====

	
.GET.PLAYER.KEYPRESS.LOOP	
	JSR COMBAT.KEYIN
	STA COMBAT.FIRST_PC_TURN.FLAG ;set here to non-zero value because it is convenient. ;($00 = first PC turn | >=$01 not first PC turn)

		;ACC = ascii value of keypress

			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; JSR KEYIN
			; LDA #$AA
			; LDY COMBAT.PC.ACTIVE.SINDEX ;load screen tile location of active player
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP
			
;convert keypress to upper case

	JSR CONVERT.ASCII.UCASE
		;RETURN VALUE: ACC = ASCII code (upper case, if the code is a letter)
		

	;**OPT** Memory. Consolidate the key checks that comabt game loop and the primary game loop have in common and setup via JSR.
		
	CMP #$8B			;UP ARROW
	BEQ .COMBAT.COMMAND.NORTH_STEP
	CMP #$8A			;DOWN ARROW
	BEQ .COMBAT.COMMAND.SOUTH_STEP
	CMP #$95			;RIGHT ARROW
	BEQ .COMBAT.COMMAND.EAST_STEP
	CMP #$88			;LEFT ARROW
	BEQ	.COMBAT.COMMAND.WEST_STEP
	CMP #$A0			;SPACE BAR
	BEQ	.COMBAT.COMMAND.PASS_STEP
	CMP #$C1			;(A) ATTACK
	BEQ	.COMBAT.COMMAND.ATTACK_STEP
	CMP #$C3			;(C) CAST
	BEQ	.COMBAT.COMMAND.CAST_STEP
	CMP #$D0			;(P) toggle PC targets (valid/invalid)
	BEQ	.COMMAND.PC_TARGET_VALID.TOGGLE_STEP
	CMP #$D1			;(Q) QUIT
	BEQ	.COMMAND.QUIT_STEP	
	CMP #$AB			;(+) INCREASE SCROLL SPEED
	BEQ .COMMAND.INCREASE.SCROLL_SPEED_STEP	
	CMP #$BD			;(=) INCREASE SCROLL SPEED
	BEQ .COMMAND.INCREASE.SCROLL_SPEED_STEP
	CMP #$AD			;(-) INCREASE SCROLL SPEED
	BEQ .COMMAND.DECREASE.SCROLL_SPEED_STEP
	CMP #$DF			;(_) INCREASE SCROLL SPEED
	BEQ .COMMAND.DECREASE.SCROLL_SPEED_STEP
	CMP #$89			;TAB (display character roster)
	BEQ .COMMAND.DISPLAY.STATS_SUMMARY_STEP


	;set active player keys
	CMP #$A9			;SHIFT+0: clear active player lock
	BEQ .COMBAT.COMMAND.KEY_SHIFT0_STEP	
	CMP #$A1			;SHIFT+1: lock active player 1
	BEQ .COMBAT.COMMAND.KEY_SHIFT1_STEP	
	CMP #$C0			;SHIFT+2: lock active player 2
	BEQ .COMBAT.COMMAND.KEY_SHIFT2_STEP
	CMP #$A3			;SHIFT+3: lock active player 3
	BEQ .COMBAT.COMMAND.KEY_SHIFT3_STEP
	CMP #$A4			;SHIFT+4: lock active player 4
	BEQ .COMBAT.COMMAND.KEY_SHIFT4_STEP
	CMP #$A5			;SHIFT+5: lock active player 5
	BEQ .COMBAT.COMMAND.KEY_SHIFT5_STEP
	CMP #$DE			;SHIFT+6: lock active player 6
	BEQ .COMBAT.COMMAND.KEY_SHIFT6_STEP
	
	;UNUSED NUMBER KEYS
	;saved from when the number keys were used to set active player	
	; CMP #$B0			;1: clear active player lock
	; BEQ .COMBAT.COMMAND.KEY_0_STEP
	; CMP #$B1			;1: set active player 1
	; BEQ .COMBAT.COMMAND.KEY_1_STEP
	; CMP #$B2			;2: set active player 2
	; BEQ .COMBAT.COMMAND.KEY_2_STEP
	; CMP #$B3			;3: set active player 3
	; BEQ .COMBAT.COMMAND.KEY_3_STEP
	; CMP #$B4			;4: set active player 4
	; BEQ .COMBAT.COMMAND.KEY_4_STEP
	; CMP #$B5			;5: set active player 5
	; BEQ .COMBAT.COMMAND.KEY_5_STEP
	; CMP #$B6			;6: set active player 6
	; BEQ .COMBAT.COMMAND.KEY_6_STEP
	
.CHECK.PLAYTEST_KEYS

	CMP #$A9			;SHIFT+0 (SET HOOK2)
	BEQ .COMBAT.PLAYTEST.KEY_SHIFT0_STEP
	CMP #$B3			;3	(ZAP MOB)
	BEQ	.COMBAT.PLAYTEST.KEY3_STEP	
	CMP #$9B			;ESC (EXIT COMBAT)
	BEQ	.EXIT.COMBAT_SESSION
	CMP #$AA			;SHIFT+8 (TOGGLE COMBAT MATH DISPLAY)
	BEQ .COMBAT.PLAYTEST.KEY_SHIFT8_STEP
	CMP #$A8			;SHIFT+9 (SET HOOK)
	BEQ	.COMBAT.PLAYTEST.KEY_SHIFT9_STEP

	;default case: invalid key press
	;**FALLS THROUGH**
	
	JMP .INVALID.KEY.PRESS
		
.COMBAT.COMMAND.PASS_STEP
	JMP COMBAT.COMMAND.PASS
	
.COMBAT.COMMAND.NORTH_STEP
	JMP COMBAT.COMMAND.NORTH

.COMBAT.COMMAND.SOUTH_STEP
	JMP COMBAT.COMMAND.SOUTH

.COMBAT.COMMAND.EAST_STEP
	JMP COMBAT.COMMAND.EAST

.COMBAT.COMMAND.WEST_STEP
	JMP COMBAT.COMMAND.WEST

.COMBAT.COMMAND.ATTACK_STEP
	JMP COMBAT.COMMAND.ATTACK
	
.COMBAT.COMMAND.CAST_STEP
	JMP COMBAT.COMMAND.CAST
	
.COMMAND.PC_TARGET_VALID.TOGGLE_STEP
	JMP COMMAND.PC_TARGET_VALID.TOGGLE

.COMMAND.QUIT_STEP
	JMP COMMAND.QUIT.PREP

.COMMAND.INCREASE.SCROLL_SPEED_STEP
	;JSR to call the routine so that the routine can exit with RTS,
	;enabling it to be called by other routines such as PRINT.STR
	JSR COMMAND.INCREASE.SCROLL_SPEED
	JMP COMBAT.GAME_STATE.LOOP
	
.COMMAND.DECREASE.SCROLL_SPEED_STEP
	;JSR to call the routine so that the routine can exit with RTS,
	;enabling it to be called by other routines such as PRINT.STR
	JSR COMMAND.DECREASE.SCROLL_SPEED
	JMP COMBAT.GAME_STATE.LOOP

.COMMAND.DISPLAY.STATS_SUMMARY_STEP
	JSR COMBAT.DISPLAY.STATS_SUMMARY
	
		JSR KEYIN
		
	JMP COMBAT.GAME_STATE.LOOP
	
;set active player commands
.COMBAT.COMMAND.KEY_SHIFT0_STEP
	JMP COMBAT.COMMAND.KEY_SHIFT0
	
.COMBAT.COMMAND.KEY_SHIFT1_STEP
	JMP COMBAT.COMMAND.KEY_SHIFT1

.COMBAT.COMMAND.KEY_SHIFT2_STEP
	JMP COMBAT.COMMAND.KEY_SHIFT2

.COMBAT.COMMAND.KEY_SHIFT3_STEP
	JMP COMBAT.COMMAND.KEY_SHIFT3

.COMBAT.COMMAND.KEY_SHIFT4_STEP
	JMP COMBAT.COMMAND.KEY_SHIFT4

.COMBAT.COMMAND.KEY_SHIFT5_STEP
	JMP COMBAT.COMMAND.KEY_SHIFT5

.COMBAT.COMMAND.KEY_SHIFT6_STEP
	JMP COMBAT.COMMAND.KEY_SHIFT6

;UNUSED NUMBER KEYS
; .COMBAT.COMMAND.KEY_0_STEP
	; JMP COMBAT.COMMAND.KEY_1
;	
; .COMBAT.COMMAND.KEY_1_STEP	
	; JMP COMBAT.COMMAND.KEY_1
;	
; .COMBAT.COMMAND.KEY_2_STEP	
	; JMP COMBAT.COMMAND.KEY_2
;	
; .COMBAT.COMMAND.KEY_3_STEP	
	; JMP COMBAT.COMMAND.KEY_3	
;	
; .COMBAT.COMMAND.KEY_4_STEP	
	; JMP COMBAT.COMMAND.KEY_4
;	
; .COMBAT.COMMAND.KEY_5_STEP	
	; JMP COMBAT.COMMAND.KEY_5
;	
; .COMBAT.COMMAND.KEY_6_STEP	
	; JMP COMBAT.COMMAND.KEY_6
	
	
;play test keys
.COMBAT.PLAYTEST.KEY_SHIFT0_STEP
	JMP COMBAT.PLAYTEST.KEY_SHIFT0

.COMBAT.PLAYTEST.KEY3_STEP
	JMP COMBAT.PLAYTEST.KEY3
	
	
.COMBAT.PLAYTEST.KEY_SHIFT8_STEP
	JMP COMBAT.PLAYTEST.KEY_SHIFT8

.COMBAT.PLAYTEST.KEY_SHIFT9_STEP
	JMP COMBAT.PLAYTEST.KEY_SHIFT9
	
	
.INVALID.KEY.PRESS
@START	
	JSR PRINT.TEXT.WINDOW.SPACE
	
	; JSR PRINT.TEXT.WINDOW.CR ;print carriage return to combat scroll window
	; JSR PRINT.TEXT.WINDOW.CR ;print carriage return to combat scroll window

	;update scroll window	
		LDA #GLOBAL.TEXT_BLOCK.INVALID_COMMAND
		STA TWF.STRING+$0
		
		LDA /GLOBAL.TEXT_BLOCK.INVALID_COMMAND
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW

	JSR PRINT.TEXT.WINDOW.CR ;print carriage return to combat scroll window

	JSR PLAY.SOUND.DUMB_ASS
	JMP COMBAT.UPDATE.COMMAND_PROMPT
	;JMP .GET.PLAYER.KEYPRESS.LOOP	
@END


@END
	
.EXIT.COMBAT_SESSION
@START
	
; .SYNC.MAP_OBJECTS.WITH.CHAR_SHEET
; @START
; ;(copies health status field on the PC map object array to character sheet so that if a character died
; ;in battle (for example) that result is proprated into gameplay after the combat module is exited)

	; LDX #$01 ;init loop counter and player #
; .LOOP.INIT.MAP_OBJECTS.PC
			
	; ;read PC character sheet data
		; TXA
		; ;ACC = player sequential # (high-bit not set = read mode)
	; JSR COMBAT.READ_WRITE.CHR_SHEET.PC
		; ;RETURN VALUE = CHR_SHEET.RECORD.READ
	
	; ;copy health status code from character sheet to map object array

	; ;calculate map object array index
	; DEX ;subtract 1 from player # (which starts with 1) to align with map object array index (which starts with 0)
	; TXA
	; ASL ;X2
	; ASL ;X4
	; TAY ;index calculated, transfer to Y-REG
	; INX ;add 1 to player # to offset subtract (via DEX) above
	
	; LDA COMBAT.MAP_OBJECTS.PC+$3,Y
	; STA CHR_SHEET.PC.HEALTH_STATUS	
	
	; ;exit test
	; CPX PARTY.TOTAL.PC
	; BEQ .LOOP.INIT.MAP_OBJECTS.PC.DONE
	; INX	
	; JMP .LOOP.INIT.MAP_OBJECTS.PC
; .LOOP.INIT.MAP_OBJECTS.PC.DONE


; @END
			
			
; .SAVE.PLAYER.CHR_SHEET
; @START
; ;set command type (READ | WRITE | SEEK)
; ; cmd_seek.current_drive 	.EQ $90
; ; cmd_read.current_drive  	.EQ $91
; ; cmd_write.current_drive 	.EQ $92
; ;			
; ; cmd_read.drive1			.EQ $1
; ; cmd_read.drive2		  	.EQ $81
; ; cmd_write.drive1			.EQ $2
; ; cmd_write.drive2			.EQ $82

	; lda #cmd_write.drive2
	; sta parm.reqcmd

; ;set write data size (# of 512 byte blocks to write from memory)
	; lda #DATA.PLY.CHR_SHEET.FILE_SIZE ;always #$00
	; sta parm.sizelo

	; lda /DATA.PLY.CHR_SHEET.FILE_SIZE ;number of pages to write (must be in multiples of 2. i.e. 2, 4, 6, 8 $0A, etc)
	; sta parm.sizehi
	
; ;set source memory address for write
	; lda #CHR_SHEET.PC.AUX_MEMORY.START
	; sta parm.ldrlo
	; lda /CHR_SHEET.PC.AUX_MEMORY.START
	; sta parm.ldrhi
	
; ;set filename to write to	
	; lda #DATA.PLY.CHR_SHEET ;load LO address
	; sta parm.namlo	
	; lda /DATA.PLY.CHR_SHEET ;load HO address
	; sta parm.namhi	
		
		; LDA #$01	;PARM: $00=main, $01=aux
	; JSR PRODOS.IO


; @END

	
; .RESTORE.GAME_VARIABLES ;**OPT** Memory. This can't be combined with the save position variables routine in location.maps.enter (movement.mgr.ASM) because the player varaibles saved when entering a building need to be restored on exit, and combat could happen in a building. but, if there are other instances where save/restore game location variables comes up then maybe those routines and this one could be combined and accessed via JSR. 
; @START


	; LDX #$00
; .RESTORE.LOOP
; ;this loop does the equivilent of the following: 
	; ; LDA COMBAT.GMAP.X.SAVED
	; ; STA GMAP.X
	; ;
	; ; LDA COMBAT.GMAP.Y.SAVED
	; ; STA GMAP.Y
	; ;		
	; ; LDA COMBAT.GMAP.X.LAST.SAVED
	; ; STA GMAP.X.LAST
	; ;
	; ; LDA COMBAT.GMAP.Y.LAST.SAVED
	; ; STA GMAP.Y.LAST
	; ;
	; ; LDA COMBAT.RMAP.X.SAVED
	; ; STA RMAP.X
	; ;
	; ; LDA COMBAT.RMAP.Y.SAVED
	; ; STA RMAP.Y
	; ;
	; ; LDA COMBAT.RMAP.SAVED+$0
	; ; STA RMAP+$0
	; ; LDA COMBAT.RMAP.SAVED+$1
	; ; STA RMAP+$1
	; ;
	; ; LDA COMBAT.WZONE.SAVED
	; ; STA PLAYER.WMAP.ZONE
	
	; LDA COMBAT.GMAP.X.SAVED,X
	; STA GMAP.X,X
	; INX
	; CPX #$9
	; BNE .RESTORE.LOOP


; ;RESTORE MISC GAME STATE & MAP VARIABLES
	; LDA COMBAT.TIME.SUN.STATUS.SAVED
	; STA TIME.SUN.STATUS
	
	; JSR MAP.CALCULATE.SS_FLAGS

				
	; ;restore player's location type before entering combat
	; LDA COMBAT.PLAYER.MAP.LOCATION_TYPE.SAVED
	; STA PLAYER.MAP.LOCATION_TYPE

; @END

; ;**OPT** Memory. This section can probably be moved to COMBAT.EXIT
		
; ;RESET SCREEN
		; ; LDA #$03 ;clear both pages
	; ; JSR CLEAR.TEXT_WINDOW.RIGHT
	
	; ;restore verticle blue text window border where horizontal scroll window top border line integrated
		; LDY #$18
		; LDX #TWB.COMBAT.SCROLL_WINDOW.TOP_LINE
		; LDA #$84		
	; JSR DRAW.BYTE

		; LDY #$27
		; LDX #TWB.COMBAT.SCROLL_WINDOW.TOP_LINE
		; LDA #$A0		
	; JSR DRAW.BYTE		
	

	
	RTS ;returns to INITIATE.COMBAT
@END

@END


	
COMBAT.COMMANDS
@START
COMBAT.GAME_STATE.LOOP.ALT_ENTRANCE_STEP
		; LDA #$01
		; STA TROUBLESHOOTING.HOOK
	JMP COMBAT.GAME_STATE.LOOP.ALT_ENTRANCE


;MOVEMENT COMMANDS
@START
COMBAT.COMMAND.PASS
	
	;update scroll window	
		LDA #GLOBAL.TEXT_BLOCK.COMMAND_PASS					
		STA TWF.STRING+$0
		
		LDA /GLOBAL.TEXT_BLOCK.COMMAND_PASS
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW
	
	JMP INCREMENT.ACTIVE_PC

	
COMBAT.COMMAND.NORTH
			
	;update scroll window	
		LDA #GLOBAL.TEXT_BLOCK.COMMAND_NORTH	
		STA TWF.STRING+$0
		
		LDA /GLOBAL.TEXT_BLOCK.COMMAND_NORTH	
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW

	
	LDA #$00 ;load code for "north"	
	JMP COMBAT.MOVE.COMMON.ROUTINE


COMBAT.COMMAND.SOUTH
	;update scroll window	
		LDA #GLOBAL.TEXT_BLOCK.COMMAND_SOUTH	
		STA TWF.STRING+$0
		
		LDA /GLOBAL.TEXT_BLOCK.COMMAND_SOUTH	
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW

	LDA #$01 ;load code for "south"	
	JMP COMBAT.MOVE.COMMON.ROUTINE

	
COMBAT.COMMAND.EAST
	;update scroll window	
		LDA #GLOBAL.TEXT_BLOCK.COMMAND_EAST	
		STA TWF.STRING+$0
		
		LDA /GLOBAL.TEXT_BLOCK.COMMAND_EAST	
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW

	LDA #$02 ;load code for "east"	
	JMP COMBAT.MOVE.COMMON.ROUTINE

	
COMBAT.COMMAND.WEST
	;update scroll window	
		LDA #GLOBAL.TEXT_BLOCK.COMMAND_WEST
		STA TWF.STRING+$0
		
		LDA /GLOBAL.TEXT_BLOCK.COMMAND_WEST	
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW

	LDA #$03 ;load code for "west"	


	;**FALLS THROUGH**

COMBAT.MOVE.COMMON.ROUTINE
@START
	;ACC = player move directional code
	;YREG = COMBAT.ACTIVE_PLAYER.SINDEX (but it really isn't needed at this point, it gets clobbered below)

		PHA

	JSR PRINT.TEXT.WINDOW.CR ;print carriage return to combat scroll window
		
		;**warning: active player character sheet will be loaded on exit
		LDY COMBAT.PC.ACTIVE.SINDEX
		LDA #COMBAT.STATS.CRTL_HIT.NOT_ENGAGED_VALUE ;set value to update.
	JSR COMBAT.STATS.UPDATE.ENGAGED	
		
		PLA
		
.UPDATE.VIDEO.SCREEN
;Note: the move direction code is saved to byte $03 of the map object record as a means to communicate the code to
;MO.DRAW. However, byte $03 is also the health status field for PCs. To manage this we save swap out the health status
;field temporary to a variable (COMBAT.PC.ACTIVE.HEALTH_STATUS), and restore the health status field after control 
;returns from MO.DRAW (called via the JSR COMBAT.UPDATE.VIDEO.SCREEN). Additionally, COMBAT.PC.ACTIVE.HEALTH_STATUS is
;used by MO.DRAW to set SPRITE.RECORD+$7 which is the universal health_status byte for all combat S_ENTIIES.  
;
;This only applies to the active PC, and only if it is the PC's turn to move. From the perspective of MO.DRAW, all other PCs have the health status field stored
;in COMBAT.MAP_OBJECTS.PC+$3
 
	;ACC =move direction code ACC. is set on byte $03 of player character's map object record upon entrance to this routine
	TAY ;transfer move direction to Y-REG

	LDA COMBAT.MAP_OBJECTS.PC+$3,X ;load active PC's health status
	STA COMBAT.PC.ACTIVE.HEALTH_STATUS ;save it for future use

	TYA ;transfer move direction to back to ACC		
	STA COMBAT.MAP_OBJECTS.PC+$3,X
		
					
	JSR COMBAT.UPDATE.VIDEO.SCREEN

			; STA TEMP
			; ; LDA COMBAT.TURN_STATUS ;load turn status. $00 = PCs turn. $01 = Special(s) S_ENTITY, $02 = MOBs turn
			; ; BNE .TEMP ;branch if not PC turn
			
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; LDA #$00
			; STA TROUBLESHOOTING.HOOK2
			; STA TROUBLESHOOTING.HOOK
; .TEMP
			; LDA TEMP
						
		
	LDA COMBAT.PC.ACTIVE.HEALTH_STATUS ;load active PC's health status
	STA COMBAT.MAP_OBJECTS.PC+$3,X ;save it to active PC's map object record


			
			
INCREMENT.ACTIVE_PC



			
	LDA COMBAT.PC.ACTIVE.LOCK	;did game player lock active combat player to a specific player #? 
	BEQ .LOCK.CHECK.COMPLETE    ;if no, then proceed normally. 
	LDA PARTY.TOTAL.PC ;if yes, then set active player to the last in the party so that the increment routine will switch the combat turn to Special S_ENTITY(s)
	STA COMBAT.PC.ACTIVE
	;**FALLS THROUGH**
.LOCK.CHECK.COMPLETE

INCREMENT.ACTIVE_PC.ALTERNATE.ENTRANCE
.UPDATE.CHR_ROSTER
;(so any changes in PC HP/MP or health status is shown)
	JSR DISPLAY.CHARACTER.ROSTER

	LDA COMBAT.PC.ACTIVE ;starts with $01, not $00
	CMP PARTY.TOTAL.PC ;starts with $01, not $00
	BEQ .FLIP.ACTIVE.PC
	INC COMBAT.PC.ACTIVE ;starts with $01, not $00
	JMP .INCREMENT.RECORD
.FLIP.ACTIVE.PC
	LDA #$01 ;reset active PC to player 1
	STA COMBAT.PC.ACTIVE ;starts with $01, not $00

;COMBAT TURN = SPECIALS OR MOBS
	LDA COMBAT.SPECIAL.TOTAL ;load total # of active special S_ENTITIES as of the start of combat
	;LDA COMBAT.MAP_OBJECTS.SPECIAL+$02 ;load tile_type of 1st special slot
	BNE .SET.SPECIAL.ACTIVE	
;SET.MOBS.ACTIVE

	LDA #$02
	STA COMBAT.TURN_STATUS ;set combat turn to MOBs. $00 = PCs turn. $01 = Special(s) S_ENTITY, $02 = MOBs turn

	LDA #$01
	STA COMBAT.MOB.ACTIVE ;set MOB 1 (note: doesn't start with $00 for the 1st MOB)
	
	LDA #$00 
	STA COMBAT.MOB.ACTIVE.RECORD


			
	JMP .INCREMENT.RECORD
	
.SET.SPECIAL.ACTIVE
	INC COMBAT.TURN_STATUS ;set combat turn to Special S_ENTITY(s). $00 = PCs turn. $01 = Special(s) S_ENTITY, $02 = MOBs turn

	LDA #$01
	STA COMBAT.SPECIAL.ACTIVE ;set SPECIAL 1 (note: doesn't start with $00 for the 1st MOB)
	
	LDA #$00 
	STA COMBAT.SPECIAL.ACTIVE.RECORD
	
	;**FALLS THROUGH**
.INCREMENT.RECORD
	LDA COMBAT.PC.ACTIVE ;starts with $01, not $00
	SEC
	SBC #$01 ;subtract $01 because COMBAT.PC.ACTIVE starts with $01, not $00
	BNE	.STANDARD.INCREMENT ;if player 1 active? If no, do a standard increment
	;LDA #COMBAT.MAP_OBJECTS.PC.RECORD_SIZE	;if yes, hard code the increment because otherwise the standard increment procedure would result in a multiplication of zero. 
	LDA #$00 ;if yes, hard code the record value because otherwise the standard increment procedure would result in a multiplication of zero. 
	STA COMBAT.PC.ACTIVE.RECORD
	JMP .EXIT
.STANDARD.INCREMENT
	ASL ;X2
	ASL ;X4
	STA COMBAT.PC.ACTIVE.RECORD
	;TAX ;set MAP_OBJECTS.PC array index to active player character


			
.EXIT

	JMP COMBAT.REVIEW.TURN_STATUS
@END
@END	
	
;PLAYTEST KEYS
@START

COMBAT.PLAYTEST.KEY_SHIFT0 ;SET HOOK2. Turn off active player lock (not a playtest key, these two functions are piggybacked until the playtest keys are removed)
@START
	JSR PLAYTEST.KEY_SHIFT0
	LDA #$00 ;turn off active player lock
	STA COMBAT.PC.ACTIVE.LOCK
	;reset active player to player 1
	STA COMBAT.PC.ACTIVE.RECORD 	
	LDA #$01
	STA COMBAT.PC.ACTIVE 
	JMP COMBAT.GAME_STATE.LOOP.ENTRANCE	
@END

COMBAT.PLAYTEST.KEY3 ;=====ZAP S_ENTITY========
@START

		;Y-REG: screen array index of active player

		;set parameter: player adjacent screen locations
		
		;set north
		TYA
		SEC
		SBC #SCREEN.ARRAY.OFFSET
		STA PLAYER.ADJACENT.SCREEN_LOCATIONS+$0

		;set south
		TYA
		CLC
		ADC #SCREEN.ARRAY.OFFSET
		STA PLAYER.ADJACENT.SCREEN_LOCATIONS+$1

		;set east
		INY
		STY PLAYER.ADJACENT.SCREEN_LOCATIONS+$2
	
		;set west
		DEY
		DEY
		STY PLAYER.ADJACENT.SCREEN_LOCATIONS+$3
		LDA #$B3 ;set parm to trigger playtest zap mode
		STA COMMAND.ATTACK.KEYPRESS.PARM ;(simulates a keypress of 3 coming from the main game loop)
	JSR COMMAND.ATTACK
		;ACC = return value (whether an S_ENTITY was killed)
		;Y-REG = return value: screen array index of killed S_ENTITY
		CMP #$00 ;is screen draw required? (S_ENTITY found/killed)
	BEQ .SCREEN.DRAW
	;assume return value is $01 (NPC not found, return to game state loop)		
	JMP COMBAT.GAME_STATE.LOOP
	
.SCREEN.DRAW				
;ERASE S_ENTITY FROM BOTH HI-RES PAGES
			
		;YREG = SCREEN_ARRAY_INDEX
		LDA #$01
		STA PAGE.FOREGROUND.OVERRIDE
	JSR DRAW.TILE.SINGLE	

		;will draw to background page by default (erase S_ENTITY from both pages it will reappear when 
		;animiation flips the page)
	JSR DRAW.TILE.SINGLE 
	JMP INCREMENT.ACTIVE_PC

@END


COMBAT.PLAYTEST.KEY_SHIFT8 ;TOGGLE COMBAT MATH
@START
	LDA GAME.COMBAT_MATH_DISPLAY.MODE	;($00 = OFF | $01 >= ON)
	CMP #$01	
	BNE .NOT1
	LDA #$00	;set OFF
	STA GAME.COMBAT_MATH_DISPLAY.MODE	;($00 = OFF | $01 >= ON)
	JMP .EXIT
.NOT1
	LDA #$01	;set ON
	STA GAME.COMBAT_MATH_DISPLAY.MODE
	
	;**FALLS THROUGH**
	
.EXIT
	JMP COMBAT.GAME_STATE.LOOP
	
@END


COMBAT.PLAYTEST.KEY_SHIFT9  ;SET HOOK1
	JSR PLAYTEST.KEY0 ;the JSR is to KEY_0 because the main game loop uses KEY_0 and this routine is resident in the main game loop source file.  
	JMP COMBAT.GAME_STATE.LOOP	

@END

	
COMBAT.COMMAND.ATTACK
@START			
.INIT
	LDA #$00
	STA COMBAT.STATS.DAMAGE.TYPE 						;($00 = melee | $01 range, $02 spell. High-bit not set = non-magic weapon (doesn't apply to type=spell or MOBs) | high-bit set = magic weapon (doesn't apply to type=spell or MOBs). 
	STA COMBAT.ATTACK_COMMAND.ATTACKS_MADE				;$1byte ($00 = none | $01 = one | $02 = two ). # of range/melee attacks made by the active player this turn. 
	STA COMBAT.ATTACK_COMMAND.DUAL_WIELDING.SPACE.FLAG	;($00 = not set | $01 = set)
	
	;print command name (attack)	
		LDA #COMBAT.TEXT_BLOCK.COMMAND_ATTACK					
		STA TWF.STRING
		
		LDA /COMBAT.TEXT_BLOCK.COMMAND_ATTACK
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW
	
.LEFT_HAND_ENTRANCE	

.LH.INIT
	LDA #$00
	STA COMBAT.ABORT.FLAG		;($00 = not set, $01 = set)
								;can't be set in  COMBAT.INIT.STATS.VARIABLES because the abort flag is needed before this routine is called. 
	
	;set active hand = left
	STA COMBAT.ATTACK_COMMAND.ACTIVE_HAND ;(high bit not set = left hand active | high bit set = right hand active)
	
	;read character sheet for active player
		LDY COMBAT.ACTIVE_PLAYER.SINDEX
		LDA #$00 ;set read mode ;($00 = read | $01 = write)			
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet for COMBAT.ACTIVE_PLAYER.SINDEX



.GET.WEAPON.ATTRIBUTES.LEFT_HAND
@START
	
	;get shape_ID
	LDA CHR_SHEET.PC.SHAPE_ID ;load weapon shape_ID (left hand) 
	;left hand value is in HO nibble
	LSR ;move HO nibble to LO nibble
	LSR
	LSR
	LSR
	STA COMBAT.ATTACK_COMMAND.SHAPE_ID
		
	;get radius
	LDA CHR_SHEET.PC.WP_RADIUS ;load weapon radius (left hand) 
	;left hand value is in HO nibble
	LSR ;move HO nibble to LO nibble
	LSR
	LSR
	LSR
	STA COMBAT.ATTACK_COMMAND.WEAPON.RADIUS

	
	;get shape_type
	LDA CHR_SHEET.PC.WP.SHAPE_TYPE ;load weapon shape_type (left hand) 
	AND #$7F ;mask out high-bit (high-bit status contains right hand shape_type)	
	STA COMBAT.ATTACK_COMMAND.WP_SHAPE_TYPE
	
.CHECK.LEFT_HAND.WEAPON				
	;has left hand already attacked?
	LDA COMBAT.ATTACK_COMMAND.LH_STATUS.FLAG ;($00 = left hand attack not complete | $01 = left hand attack complete | $02 target not in range OR no weapon readied in left hand)
	BNE .CHECK.RIGHT_HAND.STEP ;branch if yes
					
	;weapon in left hand?
	LDA COMBAT.ATTACK_COMMAND.SHAPE_ID
	CMP #COMBAT.ATTACK_COMMAND.NO_READIED_WEAPON				
	BNE .CHECK.LEFT_HAND.WEAPON.DONE ;branch if left hand has weapon
	;left hand has no weapon: set left hand status to completed
	LDA #$02
	STA COMBAT.ATTACK_COMMAND.LH_STATUS.FLAG ;($00 = left hand attack not complete | $01 = left hand attack complete | $02 target not in range OR no weapon readied in left hand)
.CHECK.RIGHT_HAND.STEP	
	JMP .CHECK.RIGHT_HAND ;skip left hand attack, check for weapon in right hand
.CHECK.LEFT_HAND.WEAPON.DONE

.CHECK.MAGIC_WEAPON.LEFT_HAND
	LDA CHR_SHEET.PC.RACE.MGK_FLG ;load race/magic weapon flag field 
	BPL .CHECK.MAGIC_WEAPON.LEFT_HAND.DONE ;branch if left hand does not have magic weapon (high-bit is not set)	
	;left hand has magic weapon
	LDA #$80 ;set high-bit (for now assume melee weapon, melee/range test comes later)
	STA COMBAT.STATS.DAMAGE.TYPE ;($00 = melee | $01 range, $02 spell. High-bit not set = non-magic weapon (doesn't apply to type=spell or MOBs) | high-bit set = magic weapon (doesn't apply to type=spell or MOBs)
.CHECK.MAGIC_WEAPON.LEFT_HAND.DONE	

	;print left hand text	
		LDA #COMBAT.TEXT_BLOCK.LEFT_HAND					
		STA TWF.STRING
		
		LDA /COMBAT.TEXT_BLOCK.LEFT_HAND
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW

	JSR .PRINT.WEAPON_TYPE
	
		; LDA HTAB	
		; SEC
		; SBC #$0E
		; STA HTAB
		LDA #TWS.COMBAT.SCROLL_WINDOW.LEFT_SBYTE
		STA HTAB
	JSR	UPDATE.CHAR.POS	;backup to start of row so that right hand text will overwrite left hand text. 
	
@END	
	;**FALLS THROUGH**
	
.ATTACK.LOOP

			
.SELECT.TARGET
		;set parameters
		
			;**OPT** Memory. Make abort flag the ACC return value of COMBAT.SELECT.ATTACK_TARGET. Since there are multiple calls it should pay off even though the return value will have to be saved inside the routine and loaded into ACC at end (because the register restores clobber the ACC)
		LDA COMBAT.ATTACK_COMMAND.WEAPON.RADIUS ;load radius radius for weapon in current hand						
		;LDA #$FF ;parm: set radius
		;LDA #$01 ;parm: set radius
	JSR COMBAT.SELECT.ATTACK_TARGET
		;RETURN VALUE: COMBAT.ABORT.FLAG	;($00 = not set | $01 = target not found | $02 = ESC pressed to exit attack command | $03 = TAB pressed to toggle left/right hand active). Usually ESC pressed is detected via the value $01 but this use of the abort flag is a special situation where there are 3 cases to detect. 

				
		;PROCESS ABORT_TOGGLE FLAG
@START
		LDA COMBAT.ABORT.FLAG	;($00 = not set | $01 = target not found | $02 = ESC pressed to exit attack command | $03 = TAB pressed to toggle left/right hand active). Usually ESC pressed is detected via the value $01 but this use of the abort flag is a special situation where there are 3 cases to detect. 
		BEQ .CALIBERATE.DEFENDER ;branch if abort flag not set
		CMP #$02 ;was ESC pressed? 
		BNE .CONTINUE ;branch if no
		;if player has already attacked with one hand, don't permit an abort of the attack command
		LDA COMBAT.ATTACK_COMMAND.ATTACKS_MADE	 ;$1byte ($00 = none | $01 = one | $02 = two ). # of range/melee attacks made by the active player this turn. 
		BEQ .EXECUTE.ABORT
		; LDA #$00 ;clear abort flag before rentering select target routine
		; STA COMBAT.ABORT.FLAG	;($00 = not set | $01 = target not found | $02 = ESC pressed to exit attack command | $03 = TAB pressed to toggle left/right hand active). Usually ESC pressed is detected via the value $01 but this use of the abort flag is a special situation where there are 3 cases to detect. 
		JMP .EXIT
		
.EXECUTE.ABORT	
				; lda #$01
				; sta troubleshooting.hook
				
			; LDA #$03 ;set parm: erase both hi-res pages
		; JSR COMBAT.CLEAR.SCROLL_WINDOW
		JMP COMBAT.GAME_STATE.LOOP.ALT_ENTRANCE_STEP2
		;default: $01 (target not found) or $03 (player command: toggle weapon)

.CONTINUE
		DEC VTAB ;back up 2 rows so that when weapon hand toggles, the text for the new hand overwrites the text from the last hand
		DEC VTAB
	JSR	UPDATE.CHAR.POS

					
			
		LDA COMBAT.ATTACK_COMMAND.ACTIVE_HAND ;(high bit not set = left hand active | high bit set = right hand active)
		BPL .TOGGLE.TO.RIGHT_HAND
;.TOGGLE.TO.LEFT_HAND
	
		;if toggle was player command, don't play sound
		LDA COMBAT.ABORT.FLAG	;($00 = not set | $01 = target not found | $02 = ESC pressed to exit attack command | $03 = TAB pressed to toggle left/right hand active). Usually ESC pressed is detected via the value $01 but this use of the abort flag is a special situation where there are 3 cases to detect. 
		CMP #$03
		BEQ .LEFT_HAND_ENTRANCE_STEP
		;BEQ .test.toggle.left
		
		;no target found
		;(this occurs when 
		;			a) player there are no targets within the radius of the weapon in the active hand (can occur with melee weapons and range weapons with radius that doesn't reach the entire screen)
		;			b) OR all mobs & specials are dead and there is either only one PC left or the flag is set to PCs are not valid targets)
		;
		;(set active hand to completed so that it won't be eligible for an attack again)
		LDA #$02
		STA COMBAT.ATTACK_COMMAND.RH_STATUS.FLAG ;($00 = right hand attack not complete | $01 = right hand attack completed  | $02 target not in range OR no weapon readied in right hand)

		;print no target text
		JSR COMBAT.PRINT.NO_TARGET

.LEFT_HAND_ENTRANCE_STEP		
		JMP .LEFT_HAND_ENTRANCE
		
.TOGGLE.TO.RIGHT_HAND
		;no need to increment COMBAT.ATTACK_COMMAND.ITERATION_COUNTER because that is done automatically by the right hand routine. 

		;if toggle was player command, don't play sound
		LDA COMBAT.ABORT.FLAG	;($00 = not set | $01 = target not found | $02 = ESC pressed to exit attack command | $03 = TAB pressed to toggle left/right hand active). Usually ESC pressed is detected via the value $01 but this use of the abort flag is a special situation where there are 3 cases to detect. 
		CMP #$03
		;BEQ .test.tab.toggle.right
		BEQ .RIGHT_HAND_ENTRANCE

		;no target found
		;(this occurs when 
		;			a) player there are no targets within the radius of the weapon in the active hand (can occur with melee weapons and range weapons with radius that doesn't reach the entire screen)
		;			b) OR all mobs & specials are dead and there is either only one PC left or the flag is set to PCs are not valid targets)
		;
		;(set active hand to completed so that it won't be eligible for an attack again)
		LDA #$02
		STA COMBAT.ATTACK_COMMAND.LH_STATUS.FLAG ;($00 = left hand attack not complete | $01 = left hand attack complete | $02 target not in range OR no weapon readied in left hand)
		
	JSR COMBAT.PRINT.NO_TARGET ;print no target text

		JMP .RIGHT_HAND_ENTRANCE


@END

			
.CALIBERATE.DEFENDER
		;COMBAT.ATTACKER.SINDEX already set
		LDA SELECT_TARGET.ACTIVE.SELECTION.SINDEX ;load defender's (target) sindex
		STA COMBAT.DEFENDER.SINDEX

	;SET ATTACKER X/Y
	;(used in COMBAT.ATTACK.RANGE.DISPLAY)
	LDX COMBAT.ATTACKER.RECORD
	LDA COMBAT.MAP_OBJECTS.PC+$0,X ;load active player GMAP.X
	STA FP.SOURCE.GMAP.X
	LDA COMBAT.MAP_OBJECTS.PC+$1,X ;load active player GMAP.Y
	STA FP.SOURCE.GMAP.Y
	
		;**FALLS THROUGH**
		
.EXECUTE.ATTACK
	;mark active hand as having completed attack
	;(this is so that the player can't toggle to a weapon/hand that has already attacked on this turn)
	LDA COMBAT.ATTACK_COMMAND.ACTIVE_HAND ;(high bit not set = left hand active | high bit set = right hand active)
	BMI .SET.RIGHT_HAND.ATTACK_COMPLETE
;.SET.LEFT_HAND.ATTACK_COMPLETE
	LDA #$01
	STA COMBAT.ATTACK_COMMAND.LH_STATUS.FLAG ;($00 = left hand attack not complete | $01 = left hand attack complete | $02 target not in range OR no weapon readied in left hand)
		
	JMP .CALL.ATTACK.ROUTINE
.SET.RIGHT_HAND.ATTACK_COMPLETE
	LDA #$01
	STA COMBAT.ATTACK_COMMAND.RH_STATUS.FLAG ;($00 = right hand attack not complete | $01 = right hand attack completed  | $02 target not in range OR no weapon readied in right hand)

	;**FALLS THROUGH**
	
.CALL.ATTACK.ROUTINE
	;make sure defender name and "select target" text are on different lines
	JSR PRINT.TEXT.WINDOW.CR ;print carriage return to combat scroll window

	JSR COMBAT.ATTACK.RANGE_MELEE

	;JSR PRINT.TEXT.WINDOW.CR ;print carriage return to combat scroll window

	INC COMBAT.ATTACK_COMMAND.ATTACKS_MADE	 ;$1byte ($00 = none | $01 = one | $02 = two ). # of range/melee attacks made by the active player this turn. 

			; lda #$01
			; sta troubleshooting.hook
			
	;**FALLS THROUGH**

			; ; STA TEMP
			; ; LDA TROUBLESHOOTING.HOOK
			; ; CMP #$01
			; ; BNE .TEMP
			; LDA #$AA
			; ;LDX COMBAT.ATTACK_COMMAND.RH_STATUS.FLAG
			; LDY COMBAT.ACTIVE_PLAYER.SINDEX
			; JSR PREP.BRK
			; BRK
; ;.TEMP
			; ;LDA TEMP
			
.RIGHT_HAND_ENTRANCE
.CHECK.RIGHT_HAND
		;**OPT** Memory. Dual wielding may not be necessary to track on the character sheet, since the shape ID for both hands inludes a status for no readied weapon $01.	
	
	;read character sheet for active player
		LDY COMBAT.ACTIVE_PLAYER.SINDEX
		LDA #$00 ;set read mode ;($00 = read | $01 = write)			
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet for COMBAT.ACTIVE_PLAYER.SINDEX
	
	;get shape_ID
	LDA CHR_SHEET.PC.SHAPE_ID ;load weapon shape_ID (right hand) 
	;right hand value is in LO nibble
	AND #$F ;mask-out HO nibble
	STA COMBAT.ATTACK_COMMAND.SHAPE_ID


	;has right hand already attacked?
	LDA COMBAT.ATTACK_COMMAND.RH_STATUS.FLAG ;($00 = right hand attack not complete | $01 = right hand attack completed  | $02 target not in range OR no weapon readied in right hand)
	BEQ .RH.READIED_WP.CHECK ;branch if no
			
	;**FALLS THROUGH**
	

.LOOP.EXIT_TEST
;SWITCH BACK TO LEFT HAND OR EXIT?
	
		;if player executed weapon toggle command, switch to left hand
		LDA COMBAT.ABORT.FLAG	;($00 = not set | $01 = target not found | $02 = ESC pressed to exit attack command | $03 = TAB pressed to toggle left/right hand active). Usually ESC pressed is detected via the value $01 but this use of the abort flag is a special situation where there are 3 cases to detect. 
		CMP #$03
		BEQ .LEFT_HAND_ENTRANCE.STEP
		
		;no toggle command: is left hand eligible to attack? (flag for left hand only is set to complete if no weapon readied in left hand)
		;(if right hand attack completed on this iteration, the left hand may not have attacked yet if the player toggled over to the right hand weapon)
		LDA COMBAT.ATTACK_COMMAND.LH_STATUS.FLAG ;($00 = left hand attack not complete | $01 = left hand attack complete | $02 target not in range OR no weapon readied in left hand)
		BEQ .LEFT_HAND_ENTRANCE.STEP
		
		LDA COMBAT.ATTACK_COMMAND.LH_STATUS.FLAG ;($00 = left hand attack not complete | $01 = left hand attack complete | $02 target not in range OR no weapon readied in left hand)
		CMP #$02
		BNE .EXIT
		LDA COMBAT.ATTACK_COMMAND.RH_STATUS.FLAG ;($00 = left hand attack not complete | $01 = left hand attack complete | $02 target not in range OR no weapon readied in left hand)
		CMP #$02
		BNE .EXIT
		
		;**FALLS THROUGH**	
		
.COMBAT.GAME_STATE.LOOP.ALT_ENTRANCE_STEP
		JMP	COMBAT.GAME_STATE.LOOP.ALT_ENTRANCE_STEP ;branch if yes
		
.LEFT_HAND_ENTRANCE.STEP		
		JMP .LEFT_HAND_ENTRANCE

.RH.READIED_WP.CHECK
			
	;weapon in right hand?
	LDA COMBAT.ATTACK_COMMAND.SHAPE_ID ;load shape ID in active hand
	CMP #COMBAT.ATTACK_COMMAND.NO_READIED_WEAPON
	BNE .CHECK.RIGHT_HAND.DONE ;branch if right hand has weapon readied
	LDA #$02 
	STA COMBAT.ATTACK_COMMAND.RH_STATUS.FLAG ;($00 = right hand attack not complete | $01 = right hand attack completed  | $02 target not in range OR no weapon readied in right hand)
	JMP .LOOP.EXIT_TEST		
.CHECK.RIGHT_HAND.DONE
			
	;**FALLS THROUGH**
	
.GET.WEAPON.ATTRIBUTES.RIGHT_HAND	

.RH.INIT
	LDA #$00
	STA COMBAT.ABORT.FLAG		;($00 = not set, $01 = set)
								;can't be set in  COMBAT.INIT.STATS.VARIABLES because the abort flag is needed before this routine is called. 
	;set active hand = right
	LDA #$80 ;set high-bit
	STA COMBAT.ATTACK_COMMAND.ACTIVE_HAND ;(high bit not set = left hand active | high bit set = right hand active)


	
;PRINT RIGHT HAND TEXT

	;insert <CR>?
	;(do so if PC is dual wielding, and other hand has attacked
	;this is so that a blank like appears between the attack outcome of the other hand and the right hand text)
	LDA COMBAT.ATTACK_COMMAND.ATTACKS_MADE ;$1byte ($00 = none | $01 = one | $02 = two ). # of range/melee attacks made by the active player this turn. 
	BEQ .INSERT_CR.CHECK.DONE ;if an attack hasn't been made then don't print <CR> because the PC might have toggled to this hand
	LDA COMBAT.ATTACK_COMMAND.DUAL_WIELDING.SPACE.FLAG	;($00 = not set | $01 = set)
	BNE .INSERT_CR.CHECK.DONE ;I couldn't figure out how to make this work without this stupid byte hogging flag. The problem is, we only want the <CR> 
							  ;immediately after the attack outcome from the other hand is printed. If the PC tries to toggle hands (even though other hand has attacked) 
							  ;we don't want another <CR>. This logic can't be setup where the attack outcome is printed because that subroutine has no awareness of whether 
							  ;the PC is dual wielding or whether the other hand has attacked. That would be a duplication of the logic in this subroutine. Stupid extra flag.
							  ;.BS $alot. 					
			
	LDA #$01
	STA COMBAT.ATTACK_COMMAND.DUAL_WIELDING.SPACE.FLAG	;($00 = not set | $01 = set)
	JSR PRINT.TEXT.WINDOW.CR ;print carriage return to combat scroll window	
.INSERT_CR.CHECK.DONE
	
		LDA #COMBAT.TEXT_BLOCK.RIGHT_HAND					
		STA TWF.STRING
		
		LDA /COMBAT.TEXT_BLOCK.RIGHT_HAND
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW

	JSR .PRINT.WEAPON_TYPE

		
		LDA #TWS.COMBAT.SCROLL_WINDOW.LEFT_SBYTE
		STA HTAB
	JSR	UPDATE.CHAR.POS ;backup to start of row so that left hand text will overwrite right hand text. 

			;jsr keyin

			
.CHECK.MAGIC_WEAPON.RIGHT_HAND
	BIT CHR_SHEET.PC.RACE.MGK_FLG ;do bit test on bit6 of race/magic weapon flag field 
										;(high bit 7 not set = left hand does not have a magic weapon | high bit 7 set = left hand has a magic weapon)
										;(high bit 6 not set = right hand does not have a magic weapon | high bit 6 set = right hand has a magic weapon)

	BVC .CHECK.MAGIC_WEAPON.RIGHT_HAND.DONE ;branch if right hand does not have magic weapon (bit6 is not set)	
	;right hand has magic weapon
	LDA #$80 ;set high-bit (for now assume melee weapon, melee/range test comes later)
	STA COMBAT.STATS.DAMAGE.TYPE ;($00 = melee | $01 range, $02 spell. High-bit not set = non-magic weapon (doesn't apply to type=spell or MOBs) | high-bit set = magic weapon (doesn't apply to type=spell or MOBs)
.CHECK.MAGIC_WEAPON.RIGHT_HAND.DONE	


	;get radius
	LDA CHR_SHEET.PC.WP_RADIUS ;load weapon radius (right hand) 
	;right hand value is in LO nibble
	AND #$F ;mask-out HO nibble
	STA COMBAT.ATTACK_COMMAND.WEAPON.RADIUS

	;get shape_type
	LDA CHR_SHEET.PC.WP.SHAPE_TYPE ;load weapon shape_type (right hand) 
	BMI .RIGHT_HAND.NOT_ANGLED_SHAPE ;branch if high-bit is set (angled shape)
;.RIGHT_HAND.ANGLED_SHAPE
	LDA #$00
	JMP .RIGHT_HAND.SET.SHAPE_TYPE
	
.RIGHT_HAND.NOT_ANGLED_SHAPE 
	LDA #$02
	;**FALLS THROUGH**
.RIGHT_HAND.SET.SHAPE_TYPE
	STA COMBAT.ATTACK_COMMAND.WP_SHAPE_TYPE
	;**FALLS THROUGH**
.GET.WEAPON.ATTRIBUTES.RIGHT_HAND.COMPLETE

	JMP .ATTACK.LOOP ;run loop again for right hand weapon

	
.EXIT

			
	JMP INCREMENT.ACTIVE_PC
	
.PRINT.WEAPON_TYPE
@START
	;detect weapon type
	LDA COMBAT.ATTACK_COMMAND.SHAPE_ID ;load shape_ID for weapon in current hand ($00 = melee | <=$01 = ranged)
	BEQ .RHAND.MELEE	;branch if melee weapon
;.RHAND.RANGE
	LDA #COMBAT.TEXT_BLOCK.RANGE					
	STA TWF.STRING+$0
	
	LDA /COMBAT.TEXT_BLOCK.RANGE
	STA TWF.STRING+$1
	JMP .COMMON
.RHAND.MELEE
	LDA #COMBAT.TEXT_BLOCK.MELEE					
	STA TWF.STRING+$0
	
	LDA /COMBAT.TEXT_BLOCK.MELEE
	STA TWF.STRING+$1
	
	;**FALLS THROUGH**
	
.COMMON
	JSR PRINT.TEXT.WINDOW

	RTS
	
@END


@END

	
; COMBAT.GAME_STATE.LOOP.ALT_ENTRANCE_STEP4
	; JMP COMBAT.GAME_STATE.LOOP.ALT_ENTRANCE
 
COMBAT.COMMAND.CAST
@START			
.INIT.PROJECTILE	
		;set source location: active PC
		LDX COMBAT.PC.ACTIVE.RECORD
		LDA COMBAT.MAP_OBJECTS.PC+$0,X ;load active player GMAP.X
		STA FP.SOURCE.GMAP.X
		LDA COMBAT.MAP_OBJECTS.PC+$1,X ;load active player GMAP.Y
		STA FP.SOURCE.GMAP.Y

	; ; ;***temp: use player screen index
		; LDX #$04 ;set player2
		; LDA COMBAT.MAP_OBJECTS.PC+$0,X ;load player2 GMAP.X
		; STA FP.TARGET.GMAP.X
		; LDA COMBAT.MAP_OBJECTS.PC+$1,X ;load player2 GMAP.Y
		; STA FP.TARGET.GMAP.Y
		
	;**FALLS THROUGH**


	;print command name (cast spell)	
		LDA #COMBAT.TEXT_BLOCK.COMMAND_CAST					
		STA TWF.STRING
		
		LDA /COMBAT.TEXT_BLOCK.COMMAND_CAST
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW
	
.GET.SPELL_CODE ;get player input
	JSR COMBAT.KEYIN
		;ACC = ascii value of keypress
	CMP #$9B			;ESC KEY
	BEQ	COMBAT.GAME_STATE.LOOP.ALT_ENTRANCE_STEP2 ;abort cast spell
	; BNE .COW
	; JMP COMBAT.GAME_STATE.LOOP.ALT_ENTRANCE_STEP2 ;abort cast spell
; .COW
	CMP #$B0
	BCC .GET.SPELL_CODE ;keypress not valid
	CMP #$BA
	BCS .GET.SPELL_CODE ;keypress not valid
	;valid keypress
	STA COMBAT.CAST.KEYPRESS.ASCII ;player keypress response (cast) 
			
	JSR COMBAT.CAST.EXECUTE
	
.EXIT


		
	JMP INCREMENT.ACTIVE_PC

@END	

COMMAND.PC_TARGET_VALID.TOGGLE
@START
	LDA SELECT_TARGET.PC_VALID.FLAG
	CMP #$01
	BNE .NOT1
	LDA #$00 ;($00 = off, PCs not valid targets | $01 = on, PCs are valid targets)
	STA SELECT_TARGET.PC_VALID.FLAG
	JMP .EXIT
.NOT1	
	LDA #$01 ;($00 = off, PCs not valid targets | $01 = on, PCs are valid targets)
	STA SELECT_TARGET.PC_VALID.FLAG
.EXIT
	JMP COMBAT.GAME_STATE.LOOP.ENTRANCE	

@END
	
;SET PLAYER ACTIVE COMMANDS
@START
COMBAT.COMMAND.KEY_SHIFT0
	LDA #$00
	JMP COMBAT.COMMAND.SET.ACTIVE_PLAYER.COMMON

COMBAT.COMMAND.KEY_SHIFT1
	LDA #$01
	JMP COMBAT.COMMAND.SET.ACTIVE_PLAYER.COMMON
	
COMBAT.COMMAND.KEY_SHIFT2
	LDA #$02
	JMP COMBAT.COMMAND.SET.ACTIVE_PLAYER.COMMON

COMBAT.COMMAND.KEY_SHIFT3
	LDA #$03
	JMP COMBAT.COMMAND.SET.ACTIVE_PLAYER.COMMON

COMBAT.COMMAND.KEY_SHIFT4
	LDA #$04
	JMP COMBAT.COMMAND.SET.ACTIVE_PLAYER.COMMON

COMBAT.COMMAND.KEY_SHIFT5
	LDA #$05
	JMP COMBAT.COMMAND.SET.ACTIVE_PLAYER.COMMON

COMBAT.COMMAND.KEY_SHIFT6
	LDA #$06
	JMP COMBAT.COMMAND.SET.ACTIVE_PLAYER.COMMON

	
COMBAT.COMMAND.SET.ACTIVE_PLAYER.COMMON
	PHA ;push active player number to stack

	JSR PRINT.TEXT.WINDOW.SPACE

	;print blank line between last cast spell outcome and next defender name
	JSR PRINT.TEXT.WINDOW.CR ;print carriage return to combat scroll window

	PLA ;pull active player number to stack

.VALIDATE.PLAYER.NUMBER
	;ACC = requested active player to lock
	CMP PARTY.TOTAL.PC ;is requested player number in the party? (i.e. if there are two players in the party then a request to lock player 3 active will fail)
	BEQ .CHECK.HEALTH_STATUS ;branch if requested player number is in party. 
	BCS .SET.LOCKED.PLAYER.FAILED.ALT_ENTRANCE ;implicit greater than test. ;branch if requested player number is in party. 
	
	;**FALLS THROUGH**
	
.CHECK.HEALTH_STATUS
	PHA ;save requested active player to lock
		;ACC = requested active player to lock
	JSR COMBAT.GET.ACTIVE_PLAYER.HEALTH_STATUS
		;RETURN VALUE: ACC = active player health status code
	CMP #COMBAT.S_ENTITY.STATUS.GOOD ;is requested locked active player's health status "good"? Did he/she eat the salmon?
	BEQ .REQUESTED.PC.HEALTH_GOOD

.SET.LOCKED.PLAYER.FAILED	
	;lock active player failed 
	PLA ;pop requested active player to lock
.SET.LOCKED.PLAYER.FAILED.ALT_ENTRANCE
	JSR PLAY.SOUND.DUMB_ASS
	JMP COMBAT.GAME_STATE.LOOP.ALT_ENTRANCE_STEP
	
.REQUESTED.PC.HEALTH_GOOD
	PLA ;restore requested active player to lock
	STA COMBAT.PC.ACTIVE.LOCK
	JMP INCREMENT.ACTIVE_PC ;setting active player lock takes up a turn for the PCs
@END

COMBAT.GAME_STATE.LOOP.ALT_ENTRANCE_STEP2
	JMP COMBAT.GAME_STATE.LOOP.ALT_ENTRANCE
	
@END
@END

;TURN MANAGEMENT
@START
COMBAT.SPECIAL.TURN ;you want to hear about our specials?
@START		
;Skip Next Specials's Turn? 
	LDX COMBAT.SPECIAL.ACTIVE.RECORD ;load MAP_OBJECTS.MOB array index

	;Is Special Dead or sleeping?
	LDA COMBAT.MAP_OBJECTS.SPECIAL+$7,X
	CMP #COMBAT.S_ENTITY.STATUS.SLEEPING
			
	BCS .INCREMENT.ACTIVE_SPECIAL	;if yes
	;**FALLS THROUGH**
	
.UPDATE.VIDEO.SCREEN
	JSR COMBAT.UPDATE.VIDEO.SCREEN
		
.INCREMENT.ACTIVE_SPECIAL
	LDA COMBAT.SPECIAL.ACTIVE ;starts with $01, not $00
	CMP COMBAT.SPECIAL.TOTAL ;starts with $01, not $00
	BEQ .FLIP.ACTIVE.SPECIAL
	INC COMBAT.SPECIAL.ACTIVE ;starts with $01, not $00	

	JMP .INCREMENT.RECORD
.FLIP.ACTIVE.SPECIAL

			
;COMBAT TURN = MOBs
	LDA #$01 
	STA COMBAT.SPECIAL.ACTIVE ;reset MOB #1 to prepare for next MOB turn. starts with $01, not $00
	STA COMBAT.MOB.ACTIVE ;set player 1 (note: doesn't start with $00 for the 1st MOB)
	
	INC COMBAT.TURN_STATUS ;set combat turn to MOBs $00 = PCs turn. $01 = Special(s) S_ENTITY, $02 = MOBs turn
	LDA #$00
	STA COMBAT.MOB.ACTIVE.RECORD	
	;**FALLS THROUGH**
	
.INCREMENT.RECORD
	LDA COMBAT.SPECIAL.ACTIVE ;starts with $01, not $00
	SEC
	SBC #$01 ;subtract $01 because COMBAT.SPECIAL.ACTIVE starts with $01, not $00
	BNE	.STANDARD.INCREMENT ;is SPECIAL #1 active? If no, do a standard increment
	LDA #$00 ;if yes, hard code the record value because otherwise the standard increment procedure would result in a multiplication of zero. 
	STA COMBAT.SPECIAL.ACTIVE.RECORD
	JMP .EXIT
.STANDARD.INCREMENT
	ASL ;X2
	ASL ;X4
	ASL ;X8
	STA COMBAT.SPECIAL.ACTIVE.RECORD
	
.EXIT

	JMP COMBAT.REVIEW.TURN_STATUS
@END

;PCs TURN (see COMBAT.COMMANDS above)

COMBAT.MOBS.TURN
@START


	
;Skip Next Mob's Turn? 
	LDX COMBAT.MOB.ACTIVE.RECORD ;load MAP_OBJECTS.MOB array index

	;Is MOB Dead or sleeping?
	LDA COMBAT.MAP_OBJECTS.MOB+$7,X
	CMP #COMBAT.S_ENTITY.STATUS.SLEEPING
			
	BCS .INCREMENT.ACTIVE_MOB	;if yes
	;**FALLS THROUGH**
	
			;**OPT** Speed. Even though dead mobs's turns are skipped via this section, map_objects_management still processes a pass move for them in .S_ENTITY.NOT_ACTIVE. Speed
			;should increase if instead of a pass move, .S_ENTITY.NOT_ACTIVE just does a JMP to the bottom of the loop so the next record can be processed. 

.UPDATE.VIDEO.SCREEN
	JSR COMBAT.UPDATE.VIDEO.SCREEN

	
.INCREMENT.ACTIVE_MOB
	LDA COMBAT.MOB.ACTIVE ;starts with $01, not $00
	CMP COMBAT.MOB.START.TOTAL ;starts with $01, not $00
	BEQ .FLIP.ACTIVE.MOB
	INC COMBAT.MOB.ACTIVE ;starts with $01, not $00
	JMP .INCREMENT.RECORD
.FLIP.ACTIVE.MOB

;COMBAT TURN = PCs
	LDA #$01 
	STA COMBAT.MOB.ACTIVE ;reset MOB #1 to prepare for next MOB turn. starts with $01, not $00
	STA COMBAT.PC.ACTIVE ;set player 1 (note: doesn't start with $00 for the 1st MOB)
	
		;STA TROUBLESHOOTING.HOOK
		;INC TROUBLESHOOTING.HOOK
		
	LDA #$00
	STA COMBAT.TURN_STATUS ;set combat turn to PCs. $00 = PCs turn. $01 = Special(s) S_ENTITY, $02 = MOBs turn
	STA COMBAT.PC.ACTIVE.RECORD	
	;**FALLS THROUGH**
.INCREMENT.RECORD
	LDA COMBAT.MOB.ACTIVE ;starts with $01, not $00
	SEC
	SBC #$01 ;subtract $01 because COMBAT.MOB.ACTIVE starts with $01, not $00
	BNE	.STANDARD.INCREMENT ;is MOB #1 active? If no, do a standard increment
;	LDA #COMBAT.MAP_OBJECTS.MOB.RECORD_SIZE ;if yes, hard code the increment because otherwise the standard increment procedure would result in a multiplication of zero. 
	LDA #$00 ;if yes, hard code the record value because otherwise the standard increment procedure would result in a multiplication of zero. 
	STA COMBAT.MOB.ACTIVE.RECORD
	JMP .EXIT
.STANDARD.INCREMENT
	ASL ;X2
	ASL ;X4
	ASL ;X8
	STA COMBAT.MOB.ACTIVE.RECORD
	;TAX ;set MAP_OBJECTS.MOB array index to active MOB's record

		
	
.EXIT
		
				
	JMP COMBAT.REVIEW.TURN_STATUS
@END


COMBAT.UPDATE.VIDEO.SCREEN
@START
;PARAMETERS: none
;ENTRANCE: direct
;RETURN: updated S_ENTITY graphic positions



	; LDA #$01
	; STA TROUBLESHOOTING.HOOK

	; ;ENABLE BSR:B2 ($D000 Bank 2)
		; LDA $C083				;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK2 2nd)
		; LDA $C083


			; STA TEMP
			; ;LDA TROUBLESHOOTING.HOOK
			; ;CMP #$01
			; ;BNE .TEMP
			; LDX #$00
; .TEMP.LOOP	
			; LDA MO.DRAW,X
			; STA $2000,X
			; INX
			; CPX #$10
			; BNE .TEMP.LOOP
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP

			
			
		TXA
		PHA
	JSR MO.DRAW
		PLA
		TAX		
				
	JSR FLIP.PAGE


	;!!!!! BSR BANK1 !!!!
	JSR COPY.SCREEN.ENTRANCE ;**DON'T REMOVE** (or the screen goes chaotic sometimes when collisions occur with double mover mobs. MO.DRAW must need the graphics pages in sync for some reason). UDPATE: I think this is needed because sometimes, due to key press abort, no copy.screen was done after the last move, so the pages are out of syncing going into MOVE.PASS, which is triggered by non-movement commands like (3)ZAP, Push, Board, etc.

	;!!!!! BSR:BANK1 !!!!
	JSR ANIMATION.UPDATE.ENTRANCE

			
;WAS MOVE BLOCKED? (PC's only)
;(if yes, then allow PC to continue its turn)

	;PC/MOB TURN BRANCH
	LDA COMBAT.TURN_STATUS   ;($00 = PCs turn. $01 = Special(s) S_ENTITY (example Wyvern Ally, Summoned Demon), $02 = MOBs turn)
	BNE .EXIT
	
	;was move blocked?
	; STX SAVED.XREG.LOCAL
	; LDX SPRITE.RECORD+$03		;load move direction code		
	; LDA MOB.MOVES.BLOCKED,X 	;is move direction blocked?
			
	LDA COMBAT.PC.BLOCKED_MOVE.FLAG  ;($00 = not blocked | $01 = blocked)	
	BEQ .EXIT.ALT

		; lda #$aa
		; ; LDX #MOB.MOVES.BLOCKED
		; ; LDY /MOB.MOVES.BLOCKED
		; LDX SPRITE.RECORD+$03		;load move direction code
		; jsr prep.brk
		; brk
	
	LDX COMBAT.PC.ACTIVE.RECORD
	LDA COMBAT.PC.ACTIVE.HEALTH_STATUS ;load active PC's health status
	STA COMBAT.MAP_OBJECTS.PC+$3,X ;save it to active PC's map object record

		; lda #$01
		; sta troubleshooting.hook
		
	PLA ;pop RTS off stack
	PLA
	JMP COMBAT.GAME_STATE.LOOP.ALT_ENTRANCE

.EXIT.ALT
	;LDX SAVED.XREG.LOCAL
		
.EXIT
			
	RTS
	
@END

@END

;ATTACK/CAST RELATED
@START	
COMBAT.ATTACK.RANGE_MELEE
@START
;PARAMETERS: COMBAT.ATTACKER.RECORD, FP.SOURCE.GMAP.X, FP.SOURCE.GMAP.Y, SELECT_TARGET.CURRENT_TARGET.X, SELECT_TARGET.CURRENT_TARGET.Y, COMBAT.ATTACK_COMMAND.SHAPE_ID, COMBAT.ATTACK_COMMAND.WP_SHAPE_TYPE, COMBAT.STATS.DAMAGE.TYPE*
;ENTRANCE: COMBAT.COMMAND.ATTACK, .MOB.ATTACK.MELEE_RANGE (COMBAT.MOVE_MANAGER)
;RETURN: graphical effects on video screen (if applicable), and updated character sheet (i.e record any damage to defender)

;*COMBAT.STATS.DAMAGE.TYPE parm must be set to melee (with or without high-bit set for magic weapon flag), upon entrance to this routine, 

.INIT
	
	;set spell_code variable to "not set"
	;(used by supporting routines to determine whether an attack command or cast spell command is in process)
	;(also used in SELECT.SHAPE_TABLE to determine whether to load the shape table from aux memory vs. assume it was loaded via the spell code block)
	;(can't be done in COMBAT.INIT.STATS.VARIABLES because when casting a spell, that routine is called after the spell code is set)
	LDA #$FF 	
	STA COMBAT.SPELL_CODE.CAST

	; LDA #$00
	; STA COMBAT.TARGET_HIT.DB.INDEX

		LDA #$00 ;set parm: ($00 = init all variables | $01 = skip flags and TO_HIT varibles)
	JSR COMBAT.INIT.STATS.VARIABLES	

	;set print string mode
	;(if not PCs turn, enabled the delay/pause features so the player can slow down/pause the scroll text if desired)

	;PC/MOB TURN BRANCH
	LDA COMBAT.TURN_STATUS   ;($00 = PCs turn. $01 = Special(s) S_ENTITY (example Wyvern Ally, Summoned Demon), $02 = MOBs turn)
	BEQ .SET.PRINT_STRING.MODE.DONE ;if PCs turn, then branch

	LDA #$01
	STA PRINT.STR.MODE	;($00 = normal | >=$01 = wait/pause functionality enabled)
.SET.PRINT_STRING.MODE.DONE
		
.EXECUTE.ATTACK
@START

.DETECT.WEAPON_TYPE
	;COMBAT.STATS.DAMAGE.TYPE = $00 or $80
	LDA COMBAT.ATTACK_COMMAND.SHAPE_ID ;load shape_ID for weapon in current hand ($00 = melee | <=$01 = ranged)
	BEQ .DETECT.WEAPON_TYPE.DONE	;branch if melee weapon

	;set damage type = range
	LDA COMBAT.STATS.DAMAGE.TYPE ;($00 = melee | $01 range, $02 spell. High-bit not set = non-magic weapon (doesn't apply to type=spell or MOBs) | high-bit set = magic weapon (doesn't apply to type=spell or MOBs)
	ORA #$01 ;mask-in range value
	STA COMBAT.STATS.DAMAGE.TYPE ;($00 = melee | $01 range, $02 spell. High-bit not set = non-magic weapon (doesn't apply to type=spell or MOBs) | high-bit set = magic weapon (doesn't apply to type=spell or MOBs)
.DETECT.WEAPON_TYPE.DONE



		
	;**FALLS THROUGH**

;TROUBLESHOOTING.HOOK.VALIDATE_SINDEX
@START
; ;(designed to trace unexpected breaks
; ;occuring in COMBAT.READ_WRITE.CHR_SHEET caused by an invalid attacker SINDEX,
; ;which causes and invalid sprite type, which triggeres the error trap replicated below)
; ;the hook returns the RTS address of the routine that called COMBAT.HIT_MISS.ROLL or COMBAT.DAMAGE.ROLL
		; ;

		
		; LDY COMBAT.ATTACKER.SINDEX
		; CPY #$b5
		; bcc .CHECK.TYPE
	
		; LDA SCREEN.INDEX.COLUMN,Y
		; CMP #$B
		; BCC .CHECK.TYPE

		; LDA #$00
		; STA $9600
		; STY $9602
		; JMP .S_ENTITY_TYPE.ERROR
		
; .CHECK.TYPE	
		
		; LDA SCREEN.MO_SPRITE_TYPE.DATA,Y	;load s_entity type
		; ;branch based on the type of S_ENTITY found
		; ;	
		; ;ACC = S_ENTITY type	
		; ; CMP #S_ENTITY_TYPE.NC_MOB
		; ; BEQ	.MOB_ARRAY
		; CMP #S_ENTITY_TYPE.C_MOB
		; BEQ	.TEMP
		; CMP #S_ENTITY_TYPE.SPECIAL
		; BEQ .TEMP
		; CMP #S_ENTITY_TYPE.PC
		; BEQ .TEMP
		; ; CMP #S_ENTITY_TYPE.BLD_NPC
		; ; BEQ .NPC_ARRAY
		; ; CMP #S_ENTITY_TYPE.DNG_NPC
		; ; BEQ .NPC_ARRAY
		; ;
		; ;**FALLS THROUGH*
		; STA $9601
		; LDA #$00
		; STA $9600
		; STY $9602
		
; .S_ENTITY_TYPE.ERROR
; ;.DETERMINE.S_ENTITY.TYPE (CL.S_ENTITY.FOUND) reports unexpected S_ENTITY type value.
		; ; LDA #SELECT_TARGET.LAST_TARGET
		; ; LDX /SELECT_TARGET.LAST_TARGET
; ;
		; ;		 
; ;
			; pla ;pop RTS from to combat.range_Melee
			; tax
			; pla
			; tay
	; lda #$dd
	; ;ldx COMBAT.PC.ACTIVE.SINDEX
	; JSR PREP.BRK
	; BRK
	
	
		; LDY COMBAT.DEFENDER.SINDEX
		; CPY #$b5
		; bcc .CHECK.TYPE2
	
		; LDA SCREEN.INDEX.COLUMN,Y
		; CMP #$B
		; BCC .CHECK.TYPE2

		; LDA #$01
		; STA $9600
		; STY $9602
		; JMP .S_ENTITY_TYPE.ERROR
		
; .CHECK.TYPE2		
		; LDA SCREEN.MO_SPRITE_TYPE.DATA,Y	;load s_entity type
		; ;branch based on the type of S_ENTITY found
		; ;	
		; ;ACC = S_ENTITY type	
		; ; CMP #S_ENTITY_TYPE.NC_MOB
		; ; BEQ	.MOB_ARRAY
		; CMP #S_ENTITY_TYPE.C_MOB
		; BEQ	.TEMP
		; CMP #S_ENTITY_TYPE.SPECIAL
		; BEQ .TEMP
		; CMP #S_ENTITY_TYPE.PC
		; BEQ .TEMP
		; ; CMP #S_ENTITY_TYPE.BLD_NPC
		; ; BEQ .NPC_ARRAY
		; ; CMP #S_ENTITY_TYPE.DNG_NPC
		; ; BEQ .NPC_ARRAY
		; ;
		; ;**FALLS THROUGH**
		; ;
		; STA $9601
		; LDA #$01
		; STA $9600
		; STY $9602
		; jmp .S_ENTITY_TYPE.ERROR
	
; .TEMP
@END

			
		LDY COMBAT.ATTACKER.SINDEX
		;COMBAT.DEFENDER.SINDEX already set
	JSR COMBAT.HIT_MISS.ROLL ;note: this call needs to be before the call to fire projectile so that a miss can affect the trajectory
		;return value = COMBAT.STATS.HIT_MISS_KILL.FLAG ;($00 = hit | $01 = miss | $02 = kill)
			
	;**FALLS THROUGH**


	
.MELEE_RANGE.BRANCH
;(branch based on melee / range flag)
	LDA COMBAT.ATTACK_COMMAND.SHAPE_ID ;load shape_ID for weapon in current hand ($00 = melee | <=$01 = ranged)
	BEQ .DRAW_PROJECTILE.WEAPON.TYPE.MELEE	;if high bit not set, then branch

	;**FALLS THROUGH**
	
.DRAW_PROJECTILE.WEAPON.TYPE.RANGE
			
	JSR COMBAT.ATTACK.RANGE.DISPLAY
		
	;JMP .DRAW_PROJECTILE.TYPE.END
		
		
.DRAW_PROJECTILE.WEAPON.TYPE.MELEE	
	;currently no shape draw needs to be done for a melee attack because they 
	;aren't visible 1 tile away. 

	
	; **FALLS THROUGH**

.DRAW_PROJECTILE.TYPE.END

	; **FALLS THROUGH**
			
			
.UPDATE.SCROLL.WINDOW.DEFENDER_NAME
	CLC ;set parm = use defender SINDEX (COMBAT.PRINT.DEFENDER.NAME) 

	LDA COMBAT.STATS.RANGE_INTERFERENCE.FLAG ;($00 = inteference successful | $01 = interference not attempted | $03 = inteference failed)
	BNE .EXECUTE.PRINT.DEFENDER.NAME

	LDY COMBAT.STATS.ATTACKER.ENAGED.FLAG ;(at this point in the code, this variable should = SINDEX of the engaged S_ENTITY engaged in melee combat with the attacker)
	SEC ;set parm = override defender SINDEX (COMBAT.PRINT.DEFENDER.NAME) 
.EXECUTE.PRINT.DEFENDER.NAME			
	JSR COMBAT.PRINT.DEFENDER.NAME

	;**FALLS THROUGH**
@END
	
	;**FALLS THROUGH**
	
.PROCESS.HIT_MISS			
		LDA COMBAT.STATS.HIT_MISS_KILL.FLAG ;($00 = hit | $01 = miss | $02 = kill)
		BNE .TARGET.MISS
			
		;**FALLS THROUGH**
		
.TARGET.HIT	;play/display hit sound and shape
@START

			
;DISPLAY HIT
		LDY COMBAT.DEFENDER.SINDEX
	JSR COMBAT.AUDIO_VISUAL.HIT

				; LDA #test.cow				
				; STA TWF.STRING+$0
				
				; LDA /test.cow
				; STA TWF.STRING+$1
			; JSR PRINT.TEXT.WINDOW
	
;ROLL DAMAGE
		LDY COMBAT.ATTACKER.SINDEX ;**OPT** Memory. have COMBAT.DAMAGE.ROLL and COMBAT.HIT_MISS assume that COMBAT.ATTACKER/DEFENDER.SINDEX is set and drop the Y-REG parameter. 
		;COMBAT.DEFENDER.SINDEX = SELECT_TARGET.ACTIVE.SELECTION.SINDEX 
	JSR COMBAT.DAMAGE.ROLL
	

.UPDATE.SCROLL.WINDOW.HIT_OUTCOME ;i.e. missed, dodged etc.	
@START	
	;select text block to print
	LDA COMBAT.STATS.CRTL_HIT_FLAG ;($00 = critical hit succeeded, $01 = critical hit not attempted, $03 = critical hit blocked by defender)
	BEQ .PRINT.CRITICAL_HIT
	LDA COMBAT.STATS.HIT_MISS_KILL.FLAG ;($00 = hit | $01 = miss | $02 = kill)
	CMP #$02
	BEQ .PRINT.KILLED
	
		;default case: print hit!
	
;**OPT** Memory. The text prints below could be handled via a COMBAT.PRINT.MISC routine which has the ACC as the
;parm specifying the text block. ACC*2 = index to a lookup table which contains 2 byte records containing the memory address of
;each text block, which is used to set the TWF.STRING pointer.
	
.PRINT.HIT
	;JSR COMBAT.PRINT.HIT

	;print "hit" text block
		LDA #$00 ;($00 = hit text block, $01 = killed text block, $02 = critial_hit text block, $03 = critial_hit_foiled text block)
	JSR COMBAT.PRINT.HIT.OTHER
	
	;print other text if applicable to the hit outcome
	LDA COMBAT.STATS.CRTL_HIT_FLAG ;($00 = critical hit succeeded, $01 = critical hit not attempted, $03 = critical hit blocked by defender)
	CMP #$03
	BNE .PRINT.HIT.DONE

		; LDA #COMBAT.TEXT_BLOCK.CRITICAL_HIT.FOILED			
		; STA TWF.STRING+$0
		
		; LDA /COMBAT.TEXT_BLOCK.CRITICAL_HIT.FOILED
		; STA TWF.STRING+$1
	; JSR PRINT.TEXT.WINDOW

		LDA #$03 ;($00 = hit text block, $01 = killed text block, $02 = critial_hit text block, $03 = critial_hit_foiled text block)
	JSR COMBAT.PRINT.HIT.OTHER
	
	
		; LDA #COMBAT.TEXT_BLOCK.CRITICAL_HIT.FOILED2				
		; STA TWF.STRING+$0
		
		; LDA /COMBAT.TEXT_BLOCK.CRITICAL_HIT.FOILED2
		; STA TWF.STRING+$1
	; JSR PRINT.TEXT.WINDOW
	
	JMP .PRINT.HIT.DONE

.PRINT.KILLED
		; LDA #COMBAT.TEXT_BLOCK.KILLED					
		; STA TWF.STRING+$0
		
		; LDA /COMBAT.TEXT_BLOCK.KILLED
		; STA TWF.STRING+$1
	; JSR PRINT.TEXT.WINDOW

		LDA #$01 ;($00 = hit text block, $01 = killed text block, $02 = critial_hit text block, $03 = critial_hit_foiled text block)
	JSR COMBAT.PRINT.HIT.OTHER
	
	JMP .DISPLAY.STATUS_BAR.DONE
	
.PRINT.CRITICAL_HIT
		; LDA #COMBAT.TEXT_BLOCK.CRITICAL_HIT					
		; STA TWF.STRING+$0
		
		; LDA /COMBAT.TEXT_BLOCK.CRITICAL_HIT
		; STA TWF.STRING+$1	
	; JSR PRINT.TEXT.WINDOW

		LDA #$02 ;($00 = hit text block, $01 = killed text block, $02 = critial_hit text block, $03 = critial_hit_foiled text block)
	JSR COMBAT.PRINT.HIT.OTHER
	
	LDA COMBAT.STATS.HIT_MISS_KILL.FLAG ;($00 = hit | $01 = miss | $02 = kill)
	CMP #$02
	BEQ .PRINT.KILLED
		
	;**FALLS THROUGH**

.PRINT.HIT.DONE

	JSR DISPLAY.MOB.HP_STATUS_BAR
.DISPLAY.STATUS_BAR.DONE

;DISPLAY COMBAT MATH

		;JSR PRINT.TEXT.WINDOW.CR ;print carriage return to combat scroll window
	
			;JSR KEYIN
		JSR COMBAT.PRINT.MATH
			;JSR KEYIN
			
	
.UPDATE.SCROLL.WINDOW.HIT_OUTCOME.DONE	
@END

	JMP .TARGET.HIT_MISS.COMPLETE
@END


	
.TARGET.MISS
@START			
	;set stats variables to $00 for stats display
		LDA #$01 ;set parm: ($00 = init all variables | $01 = skip flags and TO_HIT varibles)
	JSR COMBAT.INIT.STATS.VARIABLES	
	
	JSR PLAY.COMBAT.ATTACK_MISS.SOUND	


.UPDATE.SCROLL.WINDOW.MISS_OUTCOME ;i.e. missed, dodged etc.	
	;select text block to print
	LDA COMBAT.STATS.DODGE_FLAG	;($00 = dodge succeeded, $01 dodge not attempted, $03 dodge failed)
	BEQ .PRINT.DODGED_PARRIED
	LDA COMBAT.STATS.RANGE_INTERFERENCE.FLAG ;($00 = inteference successful | $01 = interference not attempted | $03 = inteference failed)
	BEQ .PRINT.FOILED
	;default case: print missed

.PRINT.MISSED
		LDA #COMBAT.TEXT_BLOCK.MISSED					
		STA TWF.STRING+$0
		
		LDA /COMBAT.TEXT_BLOCK.MISSED
		STA TWF.STRING+$1	
	JMP .PRINT.MISS_EXECUTE	

.PRINT.FOILED
		LDA #COMBAT.TEXT_BLOCK.FOILED					
		STA TWF.STRING+$0
		
		LDA /COMBAT.TEXT_BLOCK.FOILED
		STA TWF.STRING+$1
	JMP .PRINT.MISS_EXECUTE	
	
.PRINT.DODGED_PARRIED
	LDA COMBAT.STATS.DAMAGE.TYPE ;($00 = melee | $01 range, $02 spell. High-bit not set = non-magic weapon (doesn't apply to type=spell or MOBs) | high-bit set = magic weapon (doesn't apply to type=spell or MOBs))
	AND #$7F ;mask-out high bit
	BNE .SET_DODGED
		
	JSR RANDOM.8
	CMP #$80
	BCS .SET_DODGED

		
	;**FALLS THROUGH**
	
.SET_PARRIED
		LDA #COMBAT.TEXT_BLOCK.PARRIED					
		STA TWF.STRING+$0
		
		LDA /COMBAT.TEXT_BLOCK.PARRIED
		STA TWF.STRING+$1
	JMP .PRINT.MISS_EXECUTE	
		
.SET_DODGED
		LDA #COMBAT.TEXT_BLOCK.DODGED					
		STA TWF.STRING+$0
		
		LDA /COMBAT.TEXT_BLOCK.DODGED
		STA TWF.STRING+$1

	;**FALLS THROUGH**
	
.PRINT.MISS_EXECUTE	
		;TWF.STRING$+0 already set above
		;TWF.STRING$+1 already set above
	JSR PRINT.TEXT.WINDOW

;DISPLAY COMBAT MATH

		;JSR PRINT.TEXT.WINDOW.CR ;print carriage return to combat scroll window
	
			;JSR KEYIN
		JSR COMBAT.PRINT.MATH
			;JSR KEYIN
			
	;**FALLS THROUGH**
@END
.TARGET.HIT_MISS.COMPLETE

	;**FALLS THROUGH**
	
;SET CRITICAL HIT RELATED FLAGS
;(engaged s_entity screen index)

		;**warning: attacker character sheet will be loaded on exit
		LDA COMBAT.DEFENDER.SINDEX ;set value to update.
		LDY COMBAT.ATTACKER.SINDEX
	JSR COMBAT.STATS.UPDATE.ENGAGED

	

.EXIT

	;reset print string mode flag
	LDA #$00
	STA PRINT.STR.MODE	;($00 = normal | >=$01 = wait/pause functionality enabled)

	
		; LDA #$01
		; STA TROUBLESHOOTING.HOOK
		
	RTS
@END

COMBAT.GAME_STATE.LOOP.ALT_ENTRANCE_STEP3
	JMP COMBAT.GAME_STATE.LOOP.ALT_ENTRANCE
	
COMBAT.MELEE.IN_RANGE.CHECK
@START
;PARAMETERS: COMBAT.CHASE.TARGET.X_LT, COMBAT.CHASE.TARGET.X_GR, COMBAT.CHASE.TARGET.Y_LT, COMBAT.CHASE.TARGET.Y_GR, COMBAT.CHASE.TARGET.DISTANCE
;ENTRANCE: .EVALUATE.MELEE_ATTACK (COMBAT.MOVE_MANAGER), .RANGE_WEAPON.INTERFERENCE.ROLL (COMBAT.STATS.TO_HIT.ROLL) 
;RETURN: ACC = ($00 = in melee range, $01 = not in melee range)
			
;NOTE: this routine was originally designed for mobs. This is why the
;"chase target" variables are used. I considered changing these variables to point to
;the ones returned by CALCULATE.DISTANCE. But, that won't work because the return variables from that routine are
;shared with MOB.IDENTIFY.TILE_LOCATION, which are used in the mob movement algorithms.  

		
	;is mob adjacent to target?
	;(first step, determine the distance value that equates to adjacent. 
	;(i.e. if the target is 1 tile away on a diagonal then the distance = 2. If the 
	;target is 1 tile away on the X or Y axis, then the distance = 1)
	
	;determine if attacker and target are on same X-AXIS or Y-AXIS
	LDA COMBAT.CHASE.TARGET.X_LT
	BNE .CHECK.Y_AXIS
	LDA COMBAT.CHASE.TARGET.X_GR ;both need to be checked because LT is set to $00 if less than, GR is set to $00 is greater than. Both are left as $01 if neither; i.e. equal.
	BEQ .ADJACENT.DISTANCE.IS.1
	;**FALLS THROUGH**	
.CHECK.Y_AXIS			
	LDA COMBAT.CHASE.TARGET.Y_LT
	BNE .ADJACENT.DISTANCE.IS.2
	LDA COMBAT.CHASE.TARGET.Y_GR ;both need to be checked because LT is set to $00 if less than, GR is set to $00 is greater than. Both are left as $01 if neither; i.e. equal.
	BNE .ADJACENT.DISTANCE.IS.2
	;**FALLS THROUGH**
.ADJACENT.DISTANCE.IS.1
	LDA #$01
	JMP .IS.ADJACENT
.ADJACENT.DISTANCE.IS.2			
	LDA #$02
	;**FALLS THROUGH**	
.IS.ADJACENT			
	CMP COMBAT.CHASE.TARGET.DISTANCE ;load screen tile location of the attacker
	BCS .IN.MELEE_RANGE
;.NOT_IN.MELEE_RANGE
	LDA #$01 ;set return value: not in melee range
	JMP .EXIT	
.IN.MELEE_RANGE
	LDA #$00 ;set return value: in melee range
	
.EXIT

	RTS

@END
	
COMBAT.CAST.EXECUTE
@START
;PARAMETERS: COMBAT.ATTACKER.SINDEX, COMBAT.DEFENER.SINDEX, FP.SOURCE.GMAP.X, FP.SOURCE.GMAP.Y, COMBAT.CAST.KEYPRESS.ASCII or COMBAT.SPELL_CODE.CAST
;ENTRANCE: COMBAT.COMMAND.CAST, .MOB.CAST_SPELL
;RETURN: graphical spell effectes and damage




.INIT

		LDA #$00 ;set parm: ($00 = init all variables | $01 = skip flags and TO_HIT varibles)
	JSR COMBAT.INIT.STATS.VARIABLES


.DISPLAY.GRAPHICAL.EFFECTS
@START

;call spell graphics
	JSR COMMAND.CAST_SPELL
		LDA COMBAT.ABORT.FLAG		;($00 = not set, $01 = set)
		BEQ .DISPLAY.GRAPHICAL.EFFECTS.DONE
		PLA ;pop RTS off stack from JSR to call this routine (COMBAT.CAST.EXECUTE)
		PLA
		JMP COMBAT.GAME_STATE.LOOP.ALT_ENTRANCE_STEP3 ;if player aborted during the target selection, spell casting was skipped, return to game loop without incrementing active player

.DISPLAY.GRAPHICAL.EFFECTS.DONE


		
		
@END	

	;**FALLS THROUGH**
	
;.UPDATE.STATS.DISPLAY
@START
	;skip non-damage spells
	;(i.e. summon, healing, etc)
	;**actually, probably not needed because the spell file sets the stop value in byte $0 of the target DB, and
	;the damage loop above checks for it and skips this routine (stats display) if it is found in the first byte. 
	
	; LDA COMBAT.STATS.SPELL.DAMAGE
	; BEQ .UPDATE.STATS.DISPLAY.DONE
	

		;COMBAT.ATTACKER.SINDEX already set
		;COMBAT.DEFENDER.SINDEX already set		
	;JSR COMBAT.UPDATE.TEXT_WINDOW
	;note: for area of effect spells, this currently just displays the damage roll for the last target hit.
			;in order to show for each target, this call would have to be moved up into
			;.APPLY.SPELL_DAMAGE.LOOP. It could make sense to do that if I implement a scroll speed parameter.
.UPDATE.STATS.DISPLAY.DONE
@END

	;**FALLS THROUGH**

.EXIT

			; LDA #$AA
			; ldx #$aa
			; ldy #$aa
			; ; LDX COMBAT.CAST.KEYPRESS.ASCII ;player keypress response (cast)
			; ; LDY PLAYER.HOTKEYS.SPELLS,X
			; JSR PREP.BRK
			; BRK



			
	RTS
	
@END

COMBAT.ATTACK.RANGE.DISPLAY
@START
;PARAMETERS: COMBAT.ATTACKER.RECORD, FP.SOURCE.GMAP.X, FP.SOURCE.GMAP.Y, SELECT_TARGET.CURRENT_TARGET.X, SELECT_TARGET.CURRENT_TARGET.Y, COMBAT.ATTACK_COMMAND.WP_SHAPE_TYPE
;ENTRANCE: COMBAT.ATTACK.RANGE_MELEE
;RETURN: range weapon graphical effects on video screen

	
;SET PARAMETERS FOR (COMBAT.FIRE.PROJECTILE)
		
		;connect pointer to projectile shape table 

		;set pointer LO Byte
		;(LO Byte is alwas $00 for both angled and non-angled shapes because the shape tables are $400 and $100 bytes in size respectively
		;and thus always start on a page boundary)
		LDA #$00
		STA COMBAT.SHAPE_TABLE.POINTER+$0 ;set pointer LO byte

		
		;set pointer HO Byte
		
		;branch: shape_type
		LDA COMBAT.ATTACK_COMMAND.SHAPE_ID
		CMP #COMBAT.NON_ANGLED_PROJECTILE.SHAPE_ID.START
		BCS .SET_POINTER.NON_ANGLED_SHAPE
		
.SET_POINTER.ANGLED_SHAPE ;set pointer HO byte
		LDA COMBAT.ATTACK_COMMAND.SHAPE_ID
		SEC
		SBC #COMBAT.ANGLED_PROJECTILE.SHAPE_ID.START ;adjust the SHAPE_ID (the first few shape IDs are not used for projectile shapes)	
		ASL ;X2
		ASL ;X4
		CLC
		ADC /WEAPON.SHAPE_TABLES.AUX_ANGLED_START_ADDRESS
		STA COMBAT.SHAPE_TABLE.POINTER+$1 ;set pointer HO byte			
		JMP .SET_POINTER.DONE
		
.SET_POINTER.NON_ANGLED_SHAPE ;set pointer HO byte
		LDA COMBAT.ATTACK_COMMAND.SHAPE_ID
		SEC
		SBC #COMBAT.NON_ANGLED_PROJECTILE.SHAPE_ID.START ;adjust the SHAPE_ID (the first few shape IDs are not used for projectile shapes)	
		CLC
		ADC /WEAPON.SHAPE_TABLES.AUX_NON_ANGLED_START_ADDRESS
		STA COMBAT.SHAPE_TABLE.POINTER+$1 ;set pointer HO byte

.SET_POINTER.DONE
		
		; lda #CROSSBOW_BOLT.SHAPE.START
		; STA COMBAT.SHAPE_TABLE.POINTER+$0 ;set pointer LO byte

		; lda /CROSSBOW_BOLT.SHAPE.START
		; STA COMBAT.SHAPE_TABLE.POINTER+$1 ;set pointer HO byte

		
		;set source location

		;FP.SOURCE.GMAP.X: set via parm
		;FP.SOURCE.GMAP.Y: set via parm
				
				; lda #$aa
				; ldx FP.SOURCE.GMAP.X ; set via parm
				; ldy FP.SOURCE.GMAP.Y ; set via parm
				; JSR FULL.BRK
				; brk
				
		;set target location: as selected by player
		LDA SELECT_TARGET.CURRENT_TARGET.X ;valid for PCs, MOBs, and SPECIALS. .EQ COMBAT.CHASE.TARGET.X
		STA FP.TARGET.GMAP.X
		LDA SELECT_TARGET.CURRENT_TARGET.Y ;valid for PCs, MOBs, and SPECIALS. .EQ COMBAT.CHASE.TARGET.Y
		STA FP.TARGET.GMAP.Y
	
		;DEMO.MOD.DELAY
		;LDA #$00  ;disable (support was added for this)
		LDA #COMBAT.PROJECTILE.DELAY	;(time target = 1.6 seconds when there are 7 empty tiles between source and target)
		;LDA #$C0 ;demo speed
		STA SHAPE.MOVE.DELAY_LENGTH ;The length of the delay between the shape draw and shape erase.

		LDA #$00 ;set on
		STA SHAPE.MOVE.SCREEN_ANIMATION.TOGGLE ;($00 = screen animation on | >=$01 = screen animation off)
		STA SHAPE.MOVE.ERASE.TOGGLE	;($00 = shape erase on | >=$01 = shape erase off)
	
		;LDA #$00	;set mode to angled shape ;($00 = angled static shape | $01 = non-angled, animated shape (spell), $02 = non-angled, animated shape (weapon))
		;LDA #$01	;set mode to spell non-angled shape ;($00 = angled static shape | $01 = non-angled, animated shape (spell), $02 = non-angled, animated shape (weapon))
		;LDA #$02	;set mode to weapon non-angled shape ;($00 = angled static shape | $01 = non-angled, animated shape (spell), $02 = non-angled, animated shape (weapon))
		LDA COMBAT.ATTACK_COMMAND.WP_SHAPE_TYPE ;load shape type for weapon in current hand 	

				; lda FP.SOURCE.GMAP.X ; set via parm
				; sta $bf00
				; lda FP.SOURCE.GMAP.Y ; set via parm
				; sta $bf01
				; LDa SELECT_TARGET.CURRENT_TARGET.X ;valid for PCs, MOBs, and SPECIALS
				; sta $bf02
				; LDa SELECT_TARGET.CURRENT_TARGET.Y ;valid for PCs, MOBs, and SPECIALS
				; sta $bf03
				; lda COMBAT.ATTACK_COMMAND.WP_SHAPE_TYPE
				; sta $bf04
				; lda #$aa
				; JSR FULL.BRK
				; brk
				
	JSR COMBAT.FIRE.PROJECTILE	

	
.EXIT

	RTS
@END

@END


;COMBAT STATISTICS ROUTINES1
@START

	.IN 	swap.routines.combat.stats_routines2.asm
			

@END

@END

;SOUND ROUTINES
@START

;COMBAT.PLAY.SOUND.ELECTRICITY
;(moved to spell file)

@END

;PRINT TEXT SUBROUTINES
@START

COMBAT.PRINT.NO_TARGET
@START
;PARAMETERS: CHR_SHEET.RECORD.READ (8)
;ENTRANCE: direct
;RETURN: printed text

		;print no target text
		LDA #COMBAT.TEXT_BLOCK.NO_TARGET					
		STA TWF.STRING
		
		LDA /COMBAT.TEXT_BLOCK.NO_TARGET
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW

			
	RTS
	
@END
	
	
@END

;TEXT BLOCKS1 (also see TEXT BLOCKS2)
@START

;===GENERAL===
;COMBAT.TEXT_BLOCK.COMMAND_PROMPT	.AZ -#$8D,/]/
COMBAT.TEXT_BLOCK.COMMAND_PROMPT	.HS 8D.00


;===MOVEMENT== 

;Converted to GLOBAL versions of these labels, which are accessible from anywhere. 

; COMBAT.TEXT_BLOCK.COMMAND_PASS		.AZ -/Pass/,#$8D
; COMBAT.TEXT_BLOCK.COMMAND_NORTH		.AZ -/North/,#$8D
; COMBAT.TEXT_BLOCK.COMMAND_SOUTH		.AZ -/South/,#$8D
; COMBAT.TEXT_BLOCK.COMMAND_EAST		.AZ -/East/,#$8D
; COMBAT.TEXT_BLOCK.COMMAND_WEST		.AZ -/West/,#$8D
; COMBAT.TEXT_BLOCK.COMMAND_BLOCKED	.AZ -#$8D,/blocked!/,#$8D

;===ATTACK==
COMBAT.TEXT_BLOCK.COMMAND_ATTACK	.AZ	-/Attack/,#$8D
COMBAT.TEXT_BLOCK.LEFT_HAND			.AZ	-/Lft hand:/
COMBAT.TEXT_BLOCK.RIGHT_HAND		.AZ	-/Rt  hand:/
COMBAT.TEXT_BLOCK.MELEE				.AZ	-/melee/
COMBAT.TEXT_BLOCK.RANGE				.AZ	-/range/

COMBAT.TEXT_BLOCK.NO_TARGET			.AZ	-#$8D,#$8D,#$8D,/no target in/,#$8D,/range!/,#$8D    ;two of the #$8D codes at the start of the array are to offset the two DEC VTAB in .CONTINUE (.ATTACK)

;miss
COMBAT.TEXT_BLOCK.MISSED			.AZ	-/missed!/,#$8D
COMBAT.TEXT_BLOCK.DODGED			.AZ	-/dodged!/,#$8D
COMBAT.TEXT_BLOCK.PARRIED			.AZ	-/parried!/,#$8D
COMBAT.TEXT_BLOCK.FOILED			.AZ	-/interferes with range attack!/,#$8D
;hit
COMBAT.TEXT_BLOCK.CRITICAL_HIT			.AZ	-/critical hit!/,#$8D
COMBAT.TEXT_BLOCK.CRITICAL_HIT.FOILED	.AZ	-/critical hit/,#$8D,/foiled!/,#$8D
;COMBAT.TEXT_BLOCK.CRITICAL_HIT.FOILED2 .AZ	-/foiled!/,#$8D

;COMBAT.TEXT_BLOCK.KILLED			.AZ	-/killed!/,#$8D
;COMBAT.TEXT_BLOCK.HIT				.AZ	-/hit!/,#$8D
	;(see persistant memory)

;MOB
COMBAT.TEXT_BLOCK.MOB_ATTACK		.AZ	-#$8D,/attacks!/,#$8D
	
;=====CAST=====
COMBAT.TEXT_BLOCK.COMMAND_CAST		.AZ	-/Cast Spell/,#$8D

;MOB
COMBAT.TEXT_BLOCK.MOB_CAST		.AZ	-#$8D,/casts spell!/,#$8D



	
	; LDA COMBAT.STATS.SPELL.KILL_FLAG ;death magic attack: ($00 = succeeded | $01 = not attempted | $03 = failed).
	; BEQ .PRINT.DEATH_MAGIC.SUCCESS
	; CMP #$03
	; BNE .PRINT.DEATH_MAGIC.FAIL

	
;COMBAT_TEXT_BLOCK.ANY_KEY1	.AZ -#$8D, #$8D,/<Press any key>/ ;ASCII text string	

@END


;SUPPORTING SUBROUTINES
@START

COMBAT.INIT.STATS.VARIABLES
@START
;PARAMETERS: ACC: ($00 = init all variables | $01 = skip flags and TO_HIT varibles)
;ENTRANCE: direct
;RETURN: the variables below init to $00

.PARSE.PARAMETERS
	;ACC ($00 = init all variables | $01 = skip flags and TO_HIT varibles)
	BNE .ALWAYS.CLEAR
	
	;**FALLS THROUGH**
	
;DON'T CLEAR THESE FLAGS ON A MISS			
	LDA #$1 ;init to "critical hit/dodge not attempted". Init is needed because critical hit and dodge saving throw routines don't run if the TO-HIT roll is a miss
	STA COMBAT.STATS.CRTL_HIT_FLAG 			;($00 = critical hit succeeded, $01 = critical hit not attempted, $03 = critical hit blocked by defender)
	STA COMBAT.STATS.DODGE_FLAG				;($00 = dodge succeeded, $01 dodge not attempted, $03 dodge failed)
	STA COMBAT.STATS.RANGE_INTERFERENCE.FLAG ;($00 = inteference successful | $01 = interference not attempted | $03 = inteference failed)
    STA COMBAT.STATS.SPELL.KILL_FLAG ;death magic attack: ($00 = succeeded | $01 = not attempted | $03 = failed). If set then all targets hit by the spell are automatically killed. Sometimes saving thows may be available. 

	LDA #$00
	STA COMBAT.STATS.MODIFIED_TO_HIT	
	STA COMBAT.STATS.TO_HIT.ROLL
	STA COMBAT.STATS.HIT_MISS_KILL.FLAG ;($00 = hit | $01 = miss | $02 = kill). Set to hit so that COMBAT.FIRE.PROJECTILE (used by spells and range attacks) doesn't modify the target tile sindex to illustrate a miss. 	
	STA COMBAT.STATS.DAMAGE.TYPE 	;($00 = melee | $01 range, $02 spell. High-bit not set = non-magic weapon (doesn't apply to type=spell or MOBs) | high-bit set = magic weapon (doesn't apply to type=spell or MOBs)

.ALWAYS.CLEAR	
	;set target_hit DB stop value
	;(note: by setting the stop value on the first byte, if an attack misses, the miss is reflected by the fact that there are not target hits recorded. The stop value will be the first value read in)
	LDA #COMBAT.TARGET_HIT.DB.STOP_VALUE 
	STA COMBAT.TARGET_HIT.DB+$0
	
	;(***see SPELL_FILE.ENTRANCE)
	;STA COMBAT.STATS.SPELL.KILL_FLAG 
	
	LDA #$00 
	STA COMBAT.TARGET_HIT.DB.INDEX
	STA COMBAT.ABORT.FLAG		;($00 = not set, $01 = set)

	STA COMBAT.STATS.BASE_DAMAGE.ROLL_X2+$0
	STA COMBAT.STATS.BASE_DAMAGE.ROLL_X2+$1
	STA COMBAT.STATS.REGULAR.ARMOR.ROLL
	STA COMBAT.STATS.DAMAGE.FINAL+$0
	STA COMBAT.STATS.DAMAGE.FINAL+$1
	STA COMBAT.STATS.CRTL_HIT_DAMAGE+$0
	STA COMBAT.STATS.CRTL_HIT_DAMAGE+$1
	STA COMBAT.STATS.RESIST_MAGIC.DEFENSE_RATING ;(ho byte ignored)
	STA IS.SPELL.MASS_DEATH.FLAG	;($00 = no, >$01 = yes)
	STA SPELL.SMITE.ACTIVE.FLAG		;($00 = SMITE not active | >=$01 = SMITE active)
	STA SHAPE.MOVE.ERASE.TOGGLE	;($00 = shape erase on | >=$01 = shape erase off)
	
.EXIT
	
	RTS
	
@END
	
;COMBAT.UPDATE.TEXT_WINDOW
@START
; ;PARAMETERS: COMBAT.ATTACKER.SINDEX, COMBAT.TARGET.SINDEX
; ;ENTRANCE: direct
; ;RETURN: none

; ;TEXT WINDOW IDEAS
; ;fixed text window dimension notes
; ;first printable row = $1	 (!22 printable rows)	
; ;last printable row = $16
; ;first printable column = $19
; ;last printable column = $26  (!13 printable characters total)
; ;
; ;area for player names, HP, MP
; ;	row $1 - $D
; ;	allows for two text lines per player *6
; ;			player name is one one line and HP/MP is on the other line
; ;				*the hp/mp could be numerical or a violet horizontal line for health and blue line for magic
; ;   no seperator between player's text block.
; ;	seperator between last player line and the start of the scrolling text window
; ;
; ;tenative scrolling text window (streaming combat/command outpout, maybe stats calcs)
; ;		row $0E - $16. (total available rows = $9
; ;
; ;Player <X> attacks <MOB NAME> | <MOB NAME> attacks player 1   (make sure to consider player on player, mob on mob and special attacks)
; ;to-hit%
; ;to-hit roll
; ;base damage roll
; ;critical hit damage
; ;armor roll (ideally for mobs it displays the "resist" stat associated with the type of attack that occured)
; ;
; ;final damage
; ;flags: range interference, dodge, critical hit, kill (death magic)


; .SAVE.REGISTERS
	; TXA
	; PHA
	; TYA
	; PHA

		; LDA #$03 ;clear both hi-res pages
	; JSR CLEAR.TEXT_WINDOW.RIGHT
	
			; ;JSR KEYIN
		
; .PRINT.MOB_PC.HIT_POINTS
; @START

; ;PRINT "pc"	
		; LDA #$19
		; STA HTAB	
		; LDA #$1
		; STA VTAB
	; JSR	UPDATE.CHAR.POS
		
	; JMP .TEXT1.PRINT

; .TEXT1 .AZ -/pc/			;ASCII text string
; .TEXT1.PRINT
		; LDA #.TEXT1				
		; STA STRING
		
		; LDA /.TEXT1
		; STA STRING+$1						
	; JSR PRINT.STR
		

	
; ;=======PC HP==========

; ;PRINT "hp:"	
		; LDA #$19
		; STA HTAB	
		; LDA #$2
		; STA VTAB
	; JSR	UPDATE.CHAR.POS
		
	; JMP .TEXT2.PRINT

; .TEXT2 .AZ -/hp:/			;ASCII text string
; .TEXT2.PRINT
		; LDA #.TEXT2			
		; STA STRING
		
		; LDA /.TEXT2
		; STA STRING+$1						
	; JSR PRINT.STR

; ;print player1 HP (BCD)	
		; LDA #$01 ;set player1
	; JSR COMBAT.PRINT.PLAYER.HIT_POINTS		

; ;print player2 HP (BCD)
		; LDA #$21
		; STA HTAB	
		; LDA #$2
		; STA VTAB
	; JSR	UPDATE.CHAR.POS
	
		; LDA #$02 ;set player2
	; JSR COMBAT.PRINT.PLAYER.HIT_POINTS		
	
; ;print player3-5 HP (BCD)
		; LDA #$19
		; STA HTAB	
		; LDA #$3
		; STA VTAB
	; JSR	UPDATE.CHAR.POS
	
		; LDA #$03 ;set player3
	; JSR COMBAT.PRINT.PLAYER.HIT_POINTS	

		; LDA #$A0 ;ASII = space
	; JSR COUT	
	
		; LDA #$04 ;set player4
	; JSR COMBAT.PRINT.PLAYER.HIT_POINTS
	
		; LDA #$A0 ;ASII = space
	; JSR COUT
	
		; LDA #$05 ;set player5
	; JSR COMBAT.PRINT.PLAYER.HIT_POINTS

		; LDA #$A0 ;ASII = space
	; JSR COUT
	
; ;print player6 HP (BCD)
		; LDA #$19
		; STA HTAB	
		; LDA #$4
		; STA VTAB
	; JSR	UPDATE.CHAR.POS
	
		; LDA #$06 ;set player6
	; JSR COMBAT.PRINT.PLAYER.HIT_POINTS
	
	
; ;==========PRINT "mobs"==========
		; LDA #$19
		; STA HTAB	
		; LDA #$05	;$9
		; STA VTAB
	; JSR	UPDATE.CHAR.POS
		
	; JMP .TEXT3.PRINT

; .TEXT3 .AZ -/mobs/			;ASCII text string
; .TEXT3.PRINT
		; LDA #.TEXT3			
		; STA STRING
		
		; LDA /.TEXT3
		; STA STRING+$1						
	; JSR PRINT.STR

; ;=======MOB HP: <player1>====

; ;PRINT "hp:"	
		; LDA #$19
		; STA HTAB	
		; LDA #$6
		; STA VTAB
	; JSR	UPDATE.CHAR.POS
		
		; LDA #.TEXT2			
		; STA STRING
		
		; LDA /.TEXT2
		; STA STRING+$1						
	; JSR PRINT.STR
		
; ;print mob1 HP (BCD)	
		; LDA #$01 ;set mob1
	; JSR COMBAT.READ_WRITE.CHR_SHEET.MOB	
		; LDA CHR_SHEET.PC_MOB.HP_LO ;16-bit hex # to convert (LO byte)
		; STA BIN+$0
		; LDA CHR_SHEET.PC_MOB.HP_HO ;16-bit hex # to convert (HO byte)
		; STA BIN+$1
	; JSR CONVERT.HEX.16_TO_BCD.24			
		; ;RETURN: BCD(2)
		
			
	; JSR PRINT.BCD_PACKED

; ;=======MOB HP: <player2>====

		; LDA #$21
		; STA HTAB	
		; LDA #$06
		; STA VTAB
	; JSR	UPDATE.CHAR.POS

; ;print mob2 HP (BCD)	
		; LDA #$02 ;set mob2
	; JSR COMBAT.READ_WRITE.CHR_SHEET.MOB				
		; LDA CHR_SHEET.PC_MOB.HP_LO ;16-bit hex # to convert (LO byte)
		; STA BIN+$0
		; LDA CHR_SHEET.PC_MOB.HP_HO ;16-bit hex # to convert (HO byte)
		; STA BIN+$1
	; JSR CONVERT.HEX.16_TO_BCD.24
		; ;RETURN: BCD(2)
	; JSR PRINT.BCD_PACKED	
; @END

; .PRINT.TO_HIT.PERCENT
; @START
; ;PRINT "to hit% !"	
		; LDA #$19
		; STA HTAB	
		; LDA #$E
		; STA VTAB
	; JSR	UPDATE.CHAR.POS
		
	; JMP .TEXT4.PRINT

; .TEXT4 .AZ -/to hit% !/			;ASCII text string
; .TEXT4.PRINT
		; LDA #.TEXT4				
		; STA STRING
		
		; LDA /.TEXT4
		; STA STRING+$1						
	; JSR PRINT.STR
	
; ;PRINT "<to hit% hex value>"	
		; LDA #$25
		; STA HTAB	
		; LDA #$E
		; STA VTAB
	; JSR	UPDATE.CHAR.POS	
		; LDA COMBAT.STATS.MODIFIED_TO_HIT
	; JSR CONVERT.HEX_TO_ASCII	
	
		; LDA RESULT+$1
	; JSR COUT
		; LDA RESULT+$0
	; JSR COUT

; ; ;PRINT: clear damage roll value
; ; ;PRINT "<damage roll hex value>"	
		; ; LDA #$25
		; ; STA HTAB	
		; ; LDA #$15
		; ; STA VTAB
	; ; JSR	UPDATE.CHAR.POS	
	
		; ; LDA #$B0
	; ; JSR COUT
		; ; LDA #$B0
	; ; JSR COUT
	
; @END

; .PRINT.TO_HIT.ROLL
; @START
	
	
; ;PRINT "to hit roll!"	
		; LDA #$19
		; STA HTAB	
		; LDA #$F
		; STA VTAB
	; JSR	UPDATE.CHAR.POS
		
	; JMP .TEXT5.PRINT

; .TEXT5 .AZ -/to hit roll!/			;ASCII text string
; .TEXT5.PRINT
		; LDA #.TEXT5				
		; STA STRING
		
		; LDA /.TEXT5
		; STA STRING+$1						
	; JSR PRINT.STR
	
; ;PRINT "<to hit roll hex value>"	
		; LDA #$25
		; STA HTAB	
		; LDA #$F
		; STA VTAB
	; JSR	UPDATE.CHAR.POS	
	
			; ;***this value is BCD format, HEX-> ASCII is the same as BCD->ASCII
		; LDA COMBAT.STATS.TO_HIT.ROLL
		; ;LDA COMBAT.ACTIVE_PLAYER.SINDEX
	; JSR CONVERT.HEX_TO_ASCII	
	
		; LDA RESULT+$1
	; JSR COUT
		; LDA RESULT+$0
	; JSR COUT
; @END

; .PRINT.BASE_DAMAGE.ROLL
; @START	
; ;PRINT "bs dmg roll $"	
		; LDA #$19
		; STA HTAB	
		; LDA #$10 ;$15
		; STA VTAB
	; JSR	UPDATE.CHAR.POS
		
	; JMP .TEXT6.PRINT

; .TEXT6 .AZ -/bs dmg   $/			;ASCII text string
; .TEXT6.PRINT
		; LDA #.TEXT6		
		; STA STRING
		
		; LDA /.TEXT6
		; STA STRING+$1						
	; JSR PRINT.STR
	
; ;PRINT "<base damage roll hex value>"	
		; LDA #$23
		; STA HTAB	;**OPT** Memory. The update.char.pos before the hex/BCD values are printed (in this section and all sections in this routine), 
		; LDA #$10	;shouldn't be neede because the description print puts the cursor in the correct position.
		; STA VTAB
	; JSR	UPDATE.CHAR.POS	
	
		; LDA COMBAT.STATS.BASE_DAMAGE.ROLL_X2+$1
	; JSR CONVERT.HEX_TO_ASCII	
	
		; LDA RESULT+$1
	; JSR COUT
		; LDA RESULT+$0
	; JSR COUT

		; LDA COMBAT.STATS.BASE_DAMAGE.ROLL_X2+$0
	; JSR CONVERT.HEX_TO_ASCII	
	
		; LDA RESULT+$1
	; JSR COUT
		; LDA RESULT+$0
	; JSR COUT	
; @END

; .PRINT.CRTL_HIT_DAMAGE
; @START

					
; ;PRINT "cr hit dmg  $"	
		; LDA #$19
		; STA HTAB	
		; LDA #$11
		; STA VTAB
	; JSR	UPDATE.CHAR.POS
		
	; JMP .TEXT7.PRINT
					   ; ;0000
; .TEXT7 .AZ -/cr dmg   $/			;ASCII text string
; .TEXT7.PRINT
		; LDA #.TEXT7		
		; STA STRING
		
		; LDA /.TEXT7
		; STA STRING+$1						
	; JSR PRINT.STR
	
; ;PRINT "<critial hit hex value, HO BYTE>"	
		; LDA #$23
		; STA HTAB	
		; LDA #$11
		; STA VTAB
	; JSR	UPDATE.CHAR.POS	
	
		; LDA COMBAT.STATS.CRTL_HIT_DAMAGE+$1
	; JSR CONVERT.HEX_TO_ASCII	
	
		; LDA RESULT+$1
	; JSR COUT
		; LDA RESULT+$0
	; JSR COUT

; ;PRINT "<critial hit hex value, LO BYTE>"		
		; LDA COMBAT.STATS.CRTL_HIT_DAMAGE+$0
	; JSR CONVERT.HEX_TO_ASCII	
	
		; LDA RESULT+$1
	; JSR COUT
		; LDA RESULT+$0
	; JSR COUT
	
; ;**OPT** Memory. I think some subroutines could be written to
; ;optimize alot of the code for writing text on th screen. Like
; ;a routine that converts the ACC to ascii and prints it to the screen,
; ;like the code above. But, ultimately the on screen values should be all BCD
; ;anyway and it will need to be interlaced into the print to text window function
; ;so the current code is really all temporary.
	
; @END

; .PRINT.REGULAR.ARMOR.ROLL
; @START
	
; ;PRINT "armor roll $"	
		; LDA #$19
		; STA HTAB	
		; LDA #$12
		; STA VTAB
	; JSR	UPDATE.CHAR.POS
		
	; JMP .TEXT8.PRINT

; .TEXT8 .AZ -/armor roll $/			;ASCII text string
; .TEXT8.PRINT
		; LDA #.TEXT8		
		; STA STRING
		
		; LDA /.TEXT8
		; STA STRING+$1						
	; JSR PRINT.STR
	
; ;PRINT "<armor roll hex value>"	
		; LDA #$25
		; STA HTAB	
		; LDA #$12
		; STA VTAB
	; JSR	UPDATE.CHAR.POS	
	
		; ;lda COMBAT.STATS.CRTL_HIT.TO_HIT.ROLL
		; LDA COMBAT.STATS.REGULAR.ARMOR.ROLL
	; JSR CONVERT.HEX_TO_ASCII	
	
		; LDA RESULT+$1
	; JSR COUT
		; LDA RESULT+$0
	; JSR COUT
	
; @END

; .PRINT.MAGIC_DEFENSE.ROLL
; @START
; ;PRINT "magic defense $"	
		; LDA #$19
		; STA HTAB	
		; LDA #$13
		; STA VTAB
	; JSR	UPDATE.CHAR.POS
		
	; JMP .TEXT9.PRINT

; .TEXT9 .AZ -/magic def. $/			;ASCII text string
; .TEXT9.PRINT
		; LDA #.TEXT9		
		; STA STRING
		
		; LDA /.TEXT9
		; STA STRING+$1						
	; JSR PRINT.STR
	
; ;PRINT "<final damage roll hex value>"	
		; LDA #$25
		; STA HTAB	
		; LDA #$13
		; STA VTAB
	; JSR	UPDATE.CHAR.POS	
	
		; LDA COMBAT.STATS.RESIST_MAGIC.DEFENSE_RATING ;(ho byte ignored)
	; JSR CONVERT.HEX_TO_ASCII	
	
		; LDA RESULT+$1
	; JSR COUT
		; LDA RESULT+$0
	; JSR COUT
	
; @END

; .PRINT.DAMAGE_FINAL.ROLL
; @START

	
; ;PRINT "final dmg$"	
		; LDA #$19
		; STA HTAB	
		; LDA #$14
		; STA VTAB
	; JSR	UPDATE.CHAR.POS
		
	; JMP .TEXT10.PRINT

; .TEXT10 .AZ -/final dmg$/			;ASCII text string
; .TEXT10.PRINT
		; LDA #.TEXT10	
		; STA STRING
		
		; LDA /.TEXT10
		; STA STRING+$1						
	; JSR PRINT.STR
	
; ;PRINT "<final damage 16-bit hex value>"	
		; ; LDA #$25
		; ; STA HTAB	
		; ; LDA #$14
		; ; STA VTAB
	; ; JSR	UPDATE.CHAR.POS	
	
		; LDA COMBAT.STATS.DAMAGE.FINAL+$1
	; JSR CONVERT.HEX_TO_ASCII	
	
		; LDA RESULT+$1
	; JSR COUT
		; LDA RESULT+$0
	; JSR COUT

		; LDA COMBAT.STATS.DAMAGE.FINAL+$0
	; JSR CONVERT.HEX_TO_ASCII	
	
		; LDA RESULT+$1
	; JSR COUT
		; LDA RESULT+$0
	; JSR COUT
; @END

; .PRINT.FLAGS.ROLL
; @START
; ;dodge flag, critical hit flag

		; LDA #$19
		; STA HTAB	
		; LDA #$15
		; STA VTAB
	; JSR	UPDATE.CHAR.POS

		; LDA COMBAT.STATS.RANGE_INTERFERENCE.FLAG ;($00 = inteference successful | $01 = interference not attempted | $03 = inteference failed)
	; JSR CONVERT.HEX_TO_ASCII	
		; LDA RESULT+$0
	; JSR COUT

		; LDA COMBAT.STATS.DODGE_FLAG	;($00 = dodge succeeded, $01 dodge not attempted, $03 dodge failed)
	; JSR CONVERT.HEX_TO_ASCII	
		; LDA RESULT+$0
	; JSR COUT
	
		; LDA COMBAT.STATS.CRTL_HIT_FLAG ;($00 = critical hit succeeded, $01 = critical hit not attempted, $03 = critical hit blocked by defender)
	; JSR CONVERT.HEX_TO_ASCII		
		; LDA RESULT+$0
	; JSR COUT

		; LDA COMBAT.STATS.SPELL.KILL_FLAG ;death magic attack: ($00 = succeeded | $01 = not attempted | $03 = failed).
	; JSR CONVERT.HEX_TO_ASCII		
		; LDA RESULT+$0
	; JSR COUT	
; @END


; .PRINT.HP.TARGET.FINAL
; @START
; ;PRINT "final HP: !"	
		; LDA #$19
		; STA HTAB	
		; LDA #$16
		; STA VTAB
	; JSR	UPDATE.CHAR.POS
		
	; JMP .TEXT11.PRINT

; .TEXT11 .AZ -/final hp: !/			;ASCII text string
; .TEXT11.PRINT
		; LDA #.TEXT11		
		; STA STRING
		
		; LDA /.TEXT11
		; STA STRING+$1						
	; JSR PRINT.STR
	
; ;PRINT "<final hp BCD value>"	
		; LDA #$23
		; STA HTAB	
		; LDA #$16
		; STA VTAB
	; JSR	UPDATE.CHAR.POS	
	
; ;print target HP (BCD)		

	; ;is target dead?
	; LDA COMBAT.STATS.HIT_MISS_KILL.FLAG ;($00 = hit | $01 = miss | $02 = kill)
	; CMP #$02 
	; BEQ .TARGET.DEAD
	; ;**FALLS THROUGH**
; .TARGET.ALIVE	
		; LDA #$00 ;set read mode ($00 = read | $01 = write))	
		; LDY COMBAT.TARGET.SINDEX
	; JSR COMBAT.READ_WRITE.CHR_SHEET
		; LDA CHR_SHEET.PC_MOB.HP_LO ;16-bit hex # to convert (LO byte)
		; STA BIN+$0
		; LDA CHR_SHEET.PC_MOB.HP_HO ;16-bit hex # to convert (HO byte)
		; STA BIN+$1
	; JSR CONVERT.HEX.16_TO_BCD.24
		; ;RETURN: BCD(3)
	; JSR PRINT.BCD_PACKED
	
	; JMP .PRINT.HP.TARGET.FINAL.COMPLETE
	
; .TARGET.DEAD
			; ; STA TEMP
			; ; LDA TROUBLESHOOTING.HOOK
			; ; CMP #$01
			; ; BNE .TEMP
				; ; lda CHR_SHEET.PC_MOB.HP_LO
				; ; sta $9600
				; ; lda CHR_SHEET.PC_MOB.HP_HO
				; ; sta $9601
				; ; lda COMBAT.STATS.DAMAGE.FINAL+$0
				; ; sta $9602
				; ; lda COMBAT.STATS.DAMAGE.FINAL+$1
				; ; sta $9603
			; ; lda #$aa
			; ; ;lda CHR_SHEET.PC_MOB.HP_LO
			; ; ;ldx CHR_SHEET.PC_MOB.HP_HO
			; ; ldx COMBAT.STATS.HIT_MISS_KILL.FLAG
			; ; ldy COMBAT.TARGET.SINDEX
			; ; JSR PREP.BRK
			; ; BRK
; ; .TEMP
			; ; LDA TEMP

			
; ;print final HP as four zeros
		; LDA #$B0 ;ascii code for $0
	; JSR COUT
		; LDA #$B0 ;ascii code for $0
	; JSR COUT
		; LDA #$B0 ;ascii code for $0
	; JSR COUT
		; LDA #$B0 ;ascii code for $0
	; JSR COUT
	
	; ;**FALLS THROUGH**
	
; @END
; .PRINT.HP.TARGET.FINAL.COMPLETE

	; ;**OPT** Memory. move the text blocks (.AZ tables) in this routine to a common location in this .ASM file. Then remove the JMPs that jump over the .AZ tables. 
; .EXIT

			; ;JSR KEYIN

; ;RESTORE REGISTERS
	; PLA
	; TAY
	; PLA
	; TAX
	
	; RTS
@END

;COMBAT.PRINT.PLAYER.HIT_POINTS
@START
; ;PARAMETERS: ACC = player sequential #
; ;ENTRANCE: direct
; ;RETURN: BCD printed to video screen, CHR_SHEET.RECORD.READ($20)

		; ;ACC = player sequential #
	; JSR COMBAT.READ_WRITE.CHR_SHEET.PC
		; LDA CHR_SHEET.PC_MOB.HP_LO ;16-bit hex # to convert (LO byte)
		; STA BIN+$0
		; LDA CHR_SHEET.PC_MOB.HP_HO ;16-bit hex # to convert (HO byte)
		; STA BIN+$1
	; JSR CONVERT.HEX.16_TO_BCD.24
		; ;RETURN: BCD(2)
	; JSR PRINT.BCD_PACKED
	
	; RTS

@END

COMBAT.GET.ACTIVE_PLAYER.HEALTH_STATUS
@START
;PARAMETERS: ACC = COMBAT.PC.ACTIVE.LOCK
;ENTRANCE: direct
;RETURN: ACC = active player health status code

	SEC
	SBC #$01
	;ACC: locked active player #
	ASL ;X2 
	ASL ;X4 ;calculate map object index
	TAX
	
	;set return value
	LDA COMBAT.MAP_OBJECTS.PC+$3,X ;load health status of locked active player			

	RTS
	
@END

	
;COMBAT.CLEAR.SCROLL_WINDOW
@START
; ;PARAMETERS:  USE.PAGE*
; ;RETURN: NONE
; ;ENTRANCE: NPC.TALK, DIRECT
; ;*Specifies which hi-res page the text window should be erased on (BOTH = $03)

; ;=====================SUBROUTINE DOCUMENTATION====================================
; ;
; ;=================================================================================

	; PHA ;save USE.PAGE parameter to ACC

; .INIT
	; LDA #TWS.COMBAT.SCROLL_WINDOW.TOP_LINE		;load line in x register	
	; STA DRAW.START_LINE
	
	; LDA #TWS.COMBAT.SCROLL_WINDOW.LEFT_SBYTE
	; STA DRAW.START_BYTE
	
	; LDA #TWS.COMBAT.SCROLL_WINDOW.RIGHT_SBYTE+$1
	; STA DRAW.STOP_BYTE
		
	; LDA #TWS.COMBAT.SCROLL_WINDOW.BOTTOM_LINE
	; STA DRAW.STOP_LINE

; .DRAW
		; PLA ;restore USE.PAGE parameter to ACC
		; ORA #$80	;set high bit as a parameter to indicate we want the byte value for the draw to be $00
	; JSR DRAW.LINE

; .EXIT
	; RTS

@END

COMBAT.DISPLAY.STATS_SUMMARY
@START
;******FINISHING INVENTORY CONNECTION TO COMBAT****
;
;-Currently screen0 doesn't fully work correctly
;	*if active player is toggled the screen isn't cleared
;	*switching menus or stats screens isn't hooked up. (breaks to monitor, intentionally, if attempted)
;	*ESC isn't hooked up (breaks to monitor, intentionally, if attempted)
;
;-.COMBAT.STATS.STATE_LOOP is setup in screen0 to handle the operation of screen0 when run locally 
;and to manage the transition to the disk loaded module. 
;.LAUNCH.INVENTORY_MODULE.FROM_COMBAT has notes on making the call to the disk loaded module.
;
;-if combat stays in memory permenatnly, screen0 will be in memory permenantly. That has changes
;some things. Comments on this are in "temporary" note in .DRAW.MENU_ICONS (screen0)

		;set inventory active player
		LDA COMBAT.PC.ACTIVE
		STA INV.ACTIVE_PLAYER

	.IN 	swap.routines.combat.stats_summary.asm

@END

@END


		;$A200
	.NO COMBAT.MAIN.MODULE.END 	;**OPT** Memory. There might be some space above this point because the entire space reserved for the peristant memory range might not be needed. 
				;the spell file code buffer end address is set to $B3FF so if
				;this .NO is changed, change the spell file code buffer end address
				;to reflect

COMBAT.PERSISTANT.MEMORY
@START
;=====================SUBROUTINE DOCUMENTATION====================================
;
;THE ROUTINES IN THIS SECTION ARE ALWAYS IN MEMORY
;(i.e. they are above the upper address of the spell file buffer so they are kept in memory when
;a spell is loaded from disk. They are swapped out along with the rest of the combat module if the inventory module is
;loaded from combat)
;	
;=================================================================================

;COMBAT GRAPHICS_SUBROUTINES (low level)
@START

COMBAT.FIRE.PROJECTILE
@START
;OTHER PARMS: FP.TARGET.GMAP.X, FP.TARGET.GMAP.Y, FP.SOURCE.GMAP.X, FP.SOURCE.GMAP.Y
;PARAMETERS: COMBAT.SHAPE_TABLE.POINTER(2), ACC (SHAPE.MOVE.MODE), SHAPE.MOVE.DELAY_LENGTH, SHAPE.MOVE.SCREEN_ANIMATION, 
;		     SHAPE.MOVE.ERASE.TOGGLE 

;=====================SUBROUTINE DOCUMENTATION====================================
;
;-Overview
;This subroutine is the entrance for firing projectiles off the tile grid such as arrows, crossbow bolts, 
;lightning bolts, fireballs, etc. 
;
;-Blending Projectile into Background
;Each shape has two shape tables (AND mask and ORA mask) so taht it will be blended into the background which it is 
;drawn over. See COMBAT.DRAW.SHAPE for more details. 
;
;-Logistics of Lines
;This routine sets the parameters for the start S_ENTITY, target S_ENTITY and the geometric line between them on the
;hi-res screen, which are needed for calling SHAPE.MOVE. 
;
;The general approach is to increment the screen byte (or bits) and line in accordance with the slope of the geometric line
;and draw the shape after each horizontal increment. Sometimes a draw is done sooner if the slope value is 
;high (steep line) to avoid choppy movement. 
;
;Run increments are usually 1 screen byte, but for steep slopes are 2 pixels/bits.
;
;The remainder value in the slope calculation is ignored except when it is large, and then an increment rise sub-counter
;is used to do an extra hi-res line increment every few screen bytes. 
;
;-Slope of Line formula
;rise = (x2 - x1)
;run = (y2 - y1)
;slope = rise/run
;
;slope is calculated via a call to DIV.16 a real division subroutine which returns an integer and remainder value.
;
;
;Note: After writing this module I learned the integer subtractin method (instead of division) of calculating the pixel
;positions of a line (thank you Michael Pohoreski), such as is used in Bresenham's algorithm. However, I decided not to redo this routine for two
;reasons: 1) it is fast enough as-is 2) the inprecision of the division method cause the projectile to hit the target in
;at slightly different points depending on the slope of the line. For, example, some slopes result in a hit on the upper left
;part of the target, some in the middle, and some in the lower right. This adds some realism in that even a master marksman is
;not going to hit the exact same point on a target every time. 
;
;
;-Octets (8 screen sections)
;
;The logic of this routine and SHAPE.MOVE is based on the screen having 8 distinct sections (octets) in which lines
;are drawing using slightly different code. The octets are the same as with S_ENTITY flocking algorithms for 
;calculating moves on the tile grid and also the same as are used with the darkness algorithm.
;
;The reason for the octets is twofold:
;
;1) faciliate the calculation of rise and run without negative numbers. CALCULATE.RISE_RUN does this by detecting which 
;X2/X1 and Y2/Y1 value is larger and uses that for the minuend (1st number in subtraction equation) 
;
;2) to plot the trajectory of the line, either an increment or decrement is needed based on the octet the 
;target S_ENTITY resides in. This is handled in the .INCREMENT.RISE and .INCREMENT.RUN routines based on the value of
;SHAPE.POSITION.X_LT, SHAPE.POSITION.Y_LT, which are set in CALCULATE.RISE_RUN.
;
;Horizonatal (left/right) and verticle (up/down) directions are 4 of the 8 octets. Detection of these octets was 
;somewhat shoe horned in. See .DETECT.HORIZONTAL_VERTICLE.OCTET (.CALCULATE.LINE)
;
;-Horizontal Increments Less than 1 Screen Byte
;
;It turned out that 1 screen byte increments produced choppy/wobbly movement on steep slopes, such
;as when the start and target locations are in adjacent tile columns. 
;
;To avoid the need for a lot of shape tables, bit shifting (ASL/LSR) is used to move shapes horizontally 2 pixels/bits
;at a time. Instead of a shape table with 2 bytes per line X 16 lines, we instead have a shape 
;table 4 bytes per line X 16 lines. Thus, the shape moves within a bubble consisting of 4 screen bytes. When the shape 
;moves horizontally it moves within that bubble, and when a full screen byte of motion has occurs then the starting
;screen byte of the bubble moves and the shape resets to the starting position in the bubble. 
;
;The 2 byte X 16 line shape table for each shape is converted into the 4 byte X 16 line version via
;the routine CONVERT.MASTER.SHAPE_TABLE.
;
;The actual bit shifting is performed by:
;
;BIT_SHIFT.SHAPE_RIGHT.ENTRANCE
;BIT_SHIFT.SHAPE_LEFT.ENTRANCE
;
;-Color Issues
;
;*Odd/Even Screen Bytes 
;There are color issue when a screen byte is incremented regarldess of whether the horizontal increment is being done 
;in 1 screen byte increments (screen byte mode), or if it occurs when the bubble (see bit shifting section above) is 
;incremented 1 screen byte.
;
;This particular color issue is that the odd/even column color assignments repeat every other screen byte due to each 
;screen byte having 7 pixels; an odd number. Since the pattern repeats every other screen byte, the bits must be shifted
;by 1 bit on even numbered screen bytes in order to keep the colors aligned. This alignment is performed by the 
;subroutine SHAPE.ALIGN.COLORS.
;
;The color alignment bit shift needs to occur in odd numbered screen bytes because the first screen byte on the tile
;grid is even numbered and the orientation of the column colors in all tile designs assumes that the tile is comprised
;of an odd numbered screen byte followed by an even numbered screen byte. 
;
;*Background Color Issues
;Since the color bit (high bit) of each byte of shape data must be set to either 0 or 1, the background and shape must
;use the same color bit setting. The bit shifting routines are setup so that the color bit of the shape data bytes 
;overrides the color bit of the background. The result is that shapes will disturb the colors of the background for one
;byte adjacent (left & right) to the shape. 
;
;Exception: when horizontal increments are in SHAPE.MOVE routine are in screen byte mode, then the shape will only 
;override the color bit setting for the two bytes that it occupies. The color bit setting of the adjacent bytes won't
;be modified. Since the horizontal increments are only used for very steep angles, this background color issue only
;happens in a relatively small number of start/target screen locations.
;
;
;
;-Useful Variables
;
;SHAPE.PROJECTILE.RUN.SBYTE: contains the unsimplified run value. (X2-X1), as measured in screen bytes
;SHAPE.PROJECTILE.RUN.PIXEL: contains the unsimplified run value. (X2-X1), as measured in pixels (sbytes *7)
;SHAPE.PROJECTILE.RISE: contains the unsimplified rise value. (Y2-Y1)
;
;
;Ok, here is where terminology gets a bit goofy. After calculating slope via rise/run, in the math world (if I recall)
;slope should equal a whole number or equal rise over run, reduced to it's simplest form. 
;If slope is a whole number, then technically the whole number is over 1 (i.e. if the slope is 5 it is also 5/1).
;SHAPE.MOVE uses both value (5 and 1 in the example), so I named them
;SHAPE.PROJECTILE.SLOPE_RISE and SHAPE.PROJECTILE.SLOPE_RUN, with the later always being $01 unlesss the line is 
;verticle. Also, since DIV.16 returns a remainder value, we don't always end up with rise/run reduced to it's simplest
;form. If there is a remainder, we end up with a integer and remainder over 1. 
;Anyway, the point here was to try to make some sense out of the variables names for anyone trying to relate them to 
;precise math terms.  
;
;SHAPE.PROJECTILE.SLOPE_RISE (numerator): will equal $00 for horizontal lines
;SHAPE.PROJECTILE.SLOPE_RUN (denominator): will equal $00 for verticle lines
;
;
;=================================================================================


		
	;ACC = parameter
	STA SHAPE.MOVE.MODE		;($00 = angled static shape | >=$01 = non-angled, animated shape)

;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA
	
;init
	LDA #$00 ;set all to false
	STA SHAPE.POSITION.X_GR ;.EQ MOB.POSITION.X_GR
	STA SHAPE.POSITION.X_LT ;.EQ MOB.POSITION.X_LT
	STA SHAPE.POSITION.Y_GR ;.EQ MOB.POSITION.Y_GR
	STA SHAPE.POSITION.Y_LT ;.EQ MOB.POSITION.Y_LT
	;ACC = $00
	STA SHAPE.PROJECTILE.HORIZONAL_INC.MODE ;$00 = screen byte, >= $01 = pixels (bit shifting)
	; STA SHAPE.MOVE.SCREEN_ANIMATION.TOGGLE ;default animation on 
	; STA SHAPE.MOVE.ERASE.TOGGLE ;detault erase on
	

		
	JSR SET.SHAPE_TABLE.POINTER


		
.IDENTIFY.START_TARGET
@START	
;=====================CODE-SECTION DOCUMENTATION====================================
;
;Obtains the screen tile location (screen array index) of the S_ENTITY which is firing the projetile and
;the target S_ENTITY. Using the screen array index, the screen byte and hi-res line of each S_ENTITY is obtained.
;These values will be used to calculate the rise/run of the geometric line between the S_ENTITIES. 
;
;The start (firing) S_ENTITY is set to the active PC
;The target S_SENTITY is currently hard coded as PC 2 but eventually the target will be set
;based on a parmeter supplied via a target selection routine. The parameter should contain the
;screen index of the target, which can then be used to lookup everything else.  
;
;=================================================================================

;set start (firing) S_ENTITY		
		LDA FP.SOURCE.GMAP.X	;**OPT** Memory. This conversion probably is no longer necessary since COMBAT.ATTACKER.SINDEX and COMBAT.DEFENDER.SINDEX are always known before this routine is called. FP.SOURCE.GMAP.X can probably then be eliminated. 
		STA PARM.GMAP.X
		LDA FP.SOURCE.GMAP.Y
		STA PARM.GMAP.Y				
	JSR CONVERT.COLUMN_ROW.TO.SINDEX
		LDY RETURN.SCREEN_ARRAY_INDEX ;used below

	LDA SCREEN.INDEX.TILE_SBYTE,Y
	STA SHAPE.SBYTE.START
	STA	SHAPE.DRAW.CURRENT.SBYTE
	
	LDA SCREEN.INDEX.TILE_LINE,Y
	STA TILE.LINE.START
	STA SHAPE.DRAW.LINE_ROW.NUMBER
	STA SHAPE.LINE.START
	STA PARM1.SHAPE.LINE
	
;set target S_ENTITY
		LDA FP.TARGET.GMAP.X ;**OPT** Memory. This conversion probably is no longer necessary since COMBAT.ATTACKER.SINDEX and COMBAT.DEFENDER.SINDEX are always known before this routine is called. 
		STA PARM.GMAP.X
		LDA FP.TARGET.GMAP.Y
		STA PARM.GMAP.Y				
	JSR CONVERT.COLUMN_ROW.TO.SINDEX
		;RETURN VALUE: Y-REG (screen array index of tile location associated with GMAP X.Y)

;DETECT MISS
;(if miss, then shift target tile sindex to graphically display the miss)		
	LDA COMBAT.STATS.HIT_MISS_KILL.FLAG ;($00 = hit | $01 = miss | $02 = kill)
	CMP #$01
	BNE .DETECT.MISS_DONE ;if not miss, then branch
	
	;is target in first or last column?
	LDA FP.TARGET.GMAP.X
	BEQ .SHIFT.TARGET.RIGHT
	CMP #COMBAT.SCREEN.COLUMN.LAST
	BEQ .SHIFT.TARGET.LEFT
	
	;DEMO.MISS.SHIFT.DISABLED
	;target is not on screen edge, decide shift direction with random number. 
	JSR RANDOM.8
		;RETURN VALUE: 8-BIT random number
	CMP #$80 ;50% chance
	BCC .SHIFT.TARGET.RIGHT
	
	;**FALLS THROUGH**
	
.SHIFT.TARGET.LEFT
	;Y-REG (screen array index of tile location associated with GMAP X.Y)

	DEY ;shift target tile sindex, 1 tile left
	
	JMP .DETECT.MISS_DONE
	
.SHIFT.TARGET.RIGHT
	;Y-REG (screen array index of tile location associated with GMAP X.Y)
	INY ;shift target tile sindex, 1 tile right

	;**FALLS THROUGH**
	
.DETECT.MISS_DONE			
		STY SHAPE.SINDEX.TARGET
		
	LDA SCREEN.INDEX.TILE_SBYTE,Y
	STA SHAPE.SBYTE.TARGET
	STA PARM2.SHAPE.SBYTE
	STA SHAPE.SBYTE.TARGET.ORIGINAL
	
	LDA SCREEN.INDEX.TILE_LINE,Y
	STA SHAPE.LINE.TARGET
	STA PARM2.SHAPE.LINE
@END


		
.CALCULATE.LINE
@START

.CALCULATE.RISE_RUN
		LDA	SHAPE.SBYTE.START
		STA PARM1.SHAPE.SBYTE
		LDA SHAPE.DRAW.LINE_ROW.NUMBER
		STA PARM1.SHAPE.LINE			
	JSR CALCULATE.RISE_RUN
		;RETURN: ACC (DISTANCE IN TILE MOVES, IGNORING OBSTACLES), SHAPE.POSITION.X_LT, SHAPE.POSITION.X_GR, SHAPE.POSITION.Y_LT, SHAPE.POSITION.Y_GR

		;ACC = distance (not pixel adjusted). as far as I can tell this is a garbage value. Not useful for anything. 
		LDA RETURN.SHAPE.RISE
		STA SHAPE.PROJECTILE.RISE

		LDA RETURN.SHAPE.RUN
		STA SHAPE.PROJECTILE.RUN.SBYTE
					
.COMPARE.RISE_RUN		
	LDA SHAPE.PROJECTILE.RISE
	CMP SHAPE.PROJECTILE.RUN.SBYTE
	BCS .CALCULATE.SLOPE	
	LDA #$01
	;ACC = #$01
	STA RESULT+$2 ;since .CALCULATE.SLOPE is skipped, init the remainder return value for JSR DIV.16 since that value
				;can affect .SHAPE.MOVEMENT.LOOP
	JMP .SET.RISE
	;**FALLS THROUGH**
	
.CALCULATE.SLOPE	
;(reduce rise/run to an interger over 1 or an integer and remainder over 1)
		LDA SHAPE.PROJECTILE.RISE
		STA DIVIDEND
		LDA #$00
		STA SHAPE.MOVE.EXTRA_RISE.COUNTER ;placed here for convenince of $00 in ACC		
		STA DIVIDEND+$1
		STA DIVISOR+$1
		LDA	SHAPE.PROJECTILE.RUN.SBYTE
		STA DIVISOR
	JSR DIV.16

.LOAD.RISE
		LDA RESULT 	;SLOPE: QUOTIENT	
.SET.RISE	
		;if JMP from .COMPARE.RISE_RUN, then ACC = $01
		STA SHAPE.PROJECTILE.SLOPE_RISE ;this is the slope numerator
		STA SHAPE.PROJECTILE.SLOPE_RISE.SBYTE ;the slope numerator when screen bytes are used for the run (denominator)
		
		LDA RESULT+$2 ;SLOPE: REMAINDER
		STA SHAPE.PROJECTILE.SLOPE_RISE.REMAINDER
		
	LDA #$01
	STA SHAPE.PROJECTILE.SLOPE_RUN ; this is the slope denominator
	STA SHAPE.MOVE.RECURSIVE.CALL ;any value will work for init except for the value of #SHAPE.PROJECTILE.DRAW_THREASHOLD
	STA BIT_SHIFT_SKIP.FLAG ;$01 = set, $00 = not set. If set then in SHAPE.MOVE, then a screen byte increment is performed instead of a bit shift when incrementing run 

	;**FALLS THROUGH**

DETECT.CORNER.CASES
@START
;Note: The SHAPE.PROJECTILE.SLOPE_RISE and SHAPE.PROJECTILE.SLOPE_RUN values are not final until this routine has
;run. 
;	
.DETECT.HORIZONTAL_VERTICLE.OCTET
	;is line horizontal? 
	LDA SHAPE.PROJECTILE.RISE	
	BNE .CHECK.VERT
	LDA #$00
	STA SHAPE.PROJECTILE.SLOPE_RISE ;if yes, overrise slope_rise value because the slope calculation would have divided 0 by another number, resulting in a garbage result. 
	
	LDA #$01 ;set to one because horizontal lines use a 1 screen byte run increment
	STA SHAPE.PROJECTILE.SLOPE_RUN		
	JMP .DETECT.CORNER_CASES.DONE

.CHECK.VERT	
	;is line verticle? 
	LDA SHAPE.PROJECTILE.RUN.SBYTE
	BNE .DETECT_OCTET.DONE
	LDA SHAPE.PROJECTILE.RISE ;set slope_rise as the total rise because the slope calculation would have divided by 0 since the line is verticle, resulting in a garbage result. 
	STA SHAPE.PROJECTILE.SLOPE_RISE ;if yes, overrise slope_run value
	LDA #$00
	STA SHAPE.PROJECTILE.SLOPE_RUN ;override default value of $01
	JMP .DETECT.CORNER_CASES.DONE
.DETECT_OCTET.DONE

.DETECT.STEEP_SLOPE
;Note: For steep slopes, use +/- 2 bit/pixel horizontal increment instead of 1 screen byte. 

	LDA SHAPE.PROJECTILE.SLOPE_RISE
	CMP #SHAPE.PROJECTILE.STEEP_SLOPE
	BCC .DETECT.SLOPE.DONE

	;set shape horizontal inc mode to pixels
	LDA #$01
	STA SHAPE.PROJECTILE.HORIZONAL_INC.MODE	;$00 = screen byte, >= $01 = pixels (bit shifting)
	
.CONVERT_RUN_TO_PIXELS
;(RETURN.SHAPE.RUN, return value from above JSR CALCULATE.RISE_RUN contains the number of screen bytes in the run, which contain 7 pixels each)
    
		LDA RETURN.SHAPE.RUN
        STA MULPLR
        LDA #$00
        STA MULPLR+$1
		;
        LDA #$07
        STA MULCND
        LDA #$00
        STA MULCND+$01
		
        JSR MLP.16.NO_BCD
			LDA RESULT
			STA SHAPE.PROJECTILE.RUN.PIXEL	

		;divide by 2 since the horizontal increment will be 2 pixels/bits
		LDA SHAPE.PROJECTILE.RUN.PIXEL
		LSR ;/2
		
.CALCULATE.SLOPE	
;(reduce rise/run to an interger over 1 or an integer and remainder over 1)
		;ACC = SHAPE.PROJECTILE.RUN.PIXEL /2
		STA DIVISOR
		LDA SHAPE.PROJECTILE.RISE
		STA DIVIDEND
		LDA #$00
		STA DIVIDEND+$1
		STA DIVISOR+$1

	JSR DIV.16
		LDA RESULT 	;SLOPE: QUOTIENT	
		STA SHAPE.PROJECTILE.SLOPE_RISE ;this is the slope numerator
		
		LDA RESULT+$2 ;SLOPE: REMAINDER
		STA SHAPE.PROJECTILE.SLOPE_RISE.REMAINDER

	LDA #$02 ;2 bits/pixels
	STA SHAPE.PROJECTILE.SLOPE_RUN ; this is the slope denominator

		LDA #$00
	STA BIT_SHIFT_SKIP.FLAG ;$01 = set, $00 = not set. If set then in SHAPE.MOVE, then a screen byte increment is performed instead of a bit shift when incrementing run 

.DETECT.SLOPE.DONE
	
.DETECT.CORNER_CASES.DONE
	;**FALLS THROUGH**

.BEGIN.PROJECTILE.LAUNCH
	JSR SELECT.SHAPE_TABLE
	JSR COMBAT.SHAPE.MOVE
	
.EXIT	
;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX

	RTS
@END
@END
@END

	;**FALLS THROUGH**	

		
SELECT.SHAPE_TABLE
@START
;PARAMETERS: SHAPE.MOVE.MODE

;=====================CODE-SECTION DOCUMENTATION====================================
;
;===OVERVIEW===
;
;-non-angled, animated shape tables
;load all shape table data for  (i.e. not just one animation frame)
;
;-angled, static shape tables
;load the shape table data for the octet associated with the slope of the line
;
;
;
;-----SHAPE/LINE OCTET MATRIX----
;
;T7		  T0		T4
;
;
;T3   	  S			T1
;
;				
;T6		  T2		T5
;
;S = position of start S_ENTITY
;T = position of target S_ENTITY relative to the start S_ENTITY
;T(n) = the octet the geometric line resides in. (the geometric line between the start and target) 
;
;Also see angular.shape.template (LAYOUT DOCS worksheet) for more documentation on the shape table octets.
;(see c:\my_code\na\Map_Shapes\Shapes\Tiles\templates).
;
;T0 = SHAPE.POSITION.Y_GR is true
;T1 = SHAPE.POSITION.X_LT
;T2 = SHAPE.POSITION.Y_LT
;T3 = SHAPE.POSITION.X_GR
;T4 = SHAPE.POSITION.X_LT, SHAPE.POSITION.Y_GR 
;T5 = SHAPE.POSITION.X_LT, SHAPE.POSITION.Y_LT
;T6 = SHAPE.POSITION.X_GR, SHAPE.POSITION.Y_LT
;T7 = SHAPE.POSITION.X_GR, SHAPE.POSITION.Y_GR
;
;*I found the following diagram in my notes and
;added it after the fact. It looks like it described the position
;of player1 relative to player2 in a circumstance where player 1 was the source
;of a projectile and player2 was the target. The X_LT, Y_GR presumably refers
;which of the SHAPE.POSITION variables above would be true in each position. 
;
;P2							P2
;X_GR						X_LT
;Y_GR						Y_GR
;
;			P1
;
;P2
;X_GR						X_LT
;Y_LT						Y_LT
;
;------------------------------------
;
;-Memory Layout of Shapes
;The octets are numbered the order which the associated shape tables are stored in aux memory. The upper octets (4-7), for
;shape versions at an angle, contain 3 shape pairs each. A shape pair consists of an AND Mask and ORA mask for 
;the shape. 
;
;-Memory Notes (general)
;
;angled shapes use first $40 of shape hopper, non-angled animated used the whole $100
;Animation manager (called by .DRAW.SHAPE in COMBAT.SHAPE.MOVE) uses different a shape hopper so there isnt a conflict.
;
;
;-Other Notes
;SHAPE.PROJECTILE.ANGLE.TYPE(n) refers to the 3 versions of each shape
;which correspond to the steepness of the slope. 
;Type 0 is the most steep, Type 2 is the least steep, and type 0 is a 45 degree angle. 
;The angle types correspond with the lables used in angular.shape.template (LAYOUT DOCS worksheet). For example "Shape NW2"
;is a type2 angle, the shape version with the steepest version pointing to the northwest.
;
;=================================================================================

	LDA SHAPE.MOVE.MODE		;($00 = angled static shape | $01 = non-angled, animated shape (spell), $02 = non-angled, animated shape (weapon))
	BEQ .ANGLED.STATIC.SHAPES
	;**FALLS THROUGH**
	
.NON_ANGLED.ANIMATED.SHAPES
;note: this routine branches based on the two animated shape modes because
;the shape table is loaded into SHAPE.HOPPER0 for spells in the spell file code block, but for weapon animated shapes,
;the shape table needs to be loaded from aux memory. 

	;ACC = shape mode
	CMP #$02 ;is mode set to "non-angled shape spell"?
	BNE	.MODE.CHECK.COMPLETE ;if yes, then skip aux load because each spell code block takes care of the shape load directly into the shape hopper (I think)
	
		;set shape table size
		;(no HO byte is used because .COPY.SHAPE_TABLE only copies shape tables $100 bytes or less. $100 byte is $00 - $FF)
		LDA #$FF
		STA SHAPE_TABLE.SIZE+$0
		LDX #$00 ;set to #$00 because X-REG and Y-REG are added in the calculation in .SET.START.ADDRESS 
		LDY #$00 ;set to #$00 because X-REG and Y-REG are added in the calculation in .SET.START.ADDRESS 
	JMP .SET.START.ADDRESS
	
.MODE.CHECK.COMPLETE
	JMP .CONVERT.SHAPE_TABLE
	
.ANGLED.STATIC.SHAPES
@START	
			
	;detect verticle line
	LDA SHAPE.PROJECTILE.SLOPE_RUN
	BEQ .OCTET0_OR_2
	
	;detect horizontal line
	LDA SHAPE.PROJECTILE.SLOPE_RISE
	BEQ .OCTET1_OR_3

	LDA SHAPE.PROJECTILE.SLOPE_RISE.SBYTE  ;.SBYTE used because if a steep slope angle was detected in the DETECT.CORNER.CASES routine above then SHAPE.PROJECTILE.SLOPE_RISE will get recalculated, and the angel values in this section are based off or rise/run where run is a screen byte value. 
	CMP #SHAPE.PROJECTILE.ANGLE.TYPE2
	BCC .ANGLE.TYPE2
	
	LDA SHAPE.PROJECTILE.SLOPE_RISE.SBYTE
	CMP #SHAPE.PROJECTILE.ANGLE.TYPE1
	BCC .ANGLE.TYPE1
	;DEFAULT TO TYPE0 (all other possibilities eliminated)
	
	;**FALLS THROUGH**

.ANGLE.TYPE0		
	LDA #$00 ;multiplyer for shape table offset
	JMP .ANGLE.TYPE.COMMON.CODE	
	
.ANGLE.TYPE1
	LDA #$01 ;multiplyer for shape table offset
	JMP .ANGLE.TYPE.COMMON.CODE	

.ANGLE.TYPE2
	LDA #$02 ;multiplyer for shape table offset
	JMP .ANGLE.TYPE.COMMON.CODE				
	;**FALLS THROUGH**
	
.ANGLE.TYPE.COMMON.CODE	
	;ACC = ANGLE TYPE
	STA SHAPE.PROJECTILE.ANGLE.TYPE
		
	LDA SHAPE.POSITION.X_LT 
	BNE .OCTET4_OR_5 ;branch if true
	
	;DEFAULT TO OCTET6_OR_7 (all other possibilities eliminated)
	JMP .OCTET6_OR_7


	
;verticle lines	
.OCTET0_OR_2

	LDA SHAPE.POSITION.Y_LT
	BNE .OCTET2 ;branch if true
	;**FALLS THROUGH**
.OCTET0	
	LDX #$00 ;no offset, use first shape pair 
	LDY #$00
	JMP .OCTET.CALCULATIONS.COMPLETE

.OCTET2
	LDX #SHAPE_TABLE.ANGULAR.OCTET2.OFFSET
	LDY /SHAPE_TABLE.ANGULAR.OCTET2.OFFSET
	JMP .OCTET.CALCULATIONS.COMPLETE

;horitzontal lines	
.OCTET1_OR_3
	LDA SHAPE.POSITION.X_GR
	BNE .OCTET3 ;branch if true
	;DEFAULT TO OCTET1 (all other possibilities eliminated)
	
	;**FALLS THROUGH**
	
.OCTET1
	LDX #SHAPE_TABLE.ANGULAR.OCTET1.OFFSET
	LDY /SHAPE_TABLE.ANGULAR.OCTET1.OFFSET
	JMP .OCTET.CALCULATIONS.COMPLETE
	
	
.OCTET3
	LDX #SHAPE_TABLE.ANGULAR.OCTET3.OFFSET
	LDY /SHAPE_TABLE.ANGULAR.OCTET3.OFFSET
	JMP .OCTET.CALCULATIONS.COMPLETE

.OCTET4_OR_5	
	LDA SHAPE.POSITION.Y_LT
	BNE .OCTET5 ;branch if true
	;DEFAULT TO OCTET4 (all other possibilities eliminated)
	
.OCTET4
	LDA SHAPE.PROJECTILE.ANGLE.TYPE
	BEQ .OCTET4.ANGLE_TYPE0
	CMP #$02
	BCC .OCTET4.ANGLE_TYPE1
	;DEFAULT TO ANGLE_TYPE2 (all other possibilities eliminated)

	;**FALLS THROUGH

.OCTET4.ANGLE_TYPE2
	LDX #SHAPE_TABLE.ANGULAR2.OCTET4.OFFSET
	LDY /SHAPE_TABLE.ANGULAR2.OCTET4.OFFSET
	JMP .OCTET.CALCULATIONS.COMPLETE
	
.OCTET4.ANGLE_TYPE0
	LDX #SHAPE_TABLE.ANGULAR0.OCTET4.OFFSET
	LDY /SHAPE_TABLE.ANGULAR0.OCTET4.OFFSET
	JMP .OCTET.CALCULATIONS.COMPLETE

.OCTET4.ANGLE_TYPE1
	LDX #SHAPE_TABLE.ANGULAR1.OCTET4.OFFSET
	LDY /SHAPE_TABLE.ANGULAR1.OCTET4.OFFSET
	JMP .OCTET.CALCULATIONS.COMPLETE

	
.OCTET5
	LDA SHAPE.PROJECTILE.ANGLE.TYPE
	BEQ .OCTET5.ANGLE_TYPE0
	CMP #$02
	BCC .OCTET5.ANGLE_TYPE1
	;DEFAULT TO ANGLE_TYPE2 (all other possibilities eliminated)

	;**FALLS THROUGH

.OCTET5.ANGLE_TYPE2
	LDX #SHAPE_TABLE.ANGULAR2.OCTET5.OFFSET
	LDY /SHAPE_TABLE.ANGULAR2.OCTET5.OFFSET
	JMP .OCTET.CALCULATIONS.COMPLETE
	
.OCTET5.ANGLE_TYPE0		
	LDX #SHAPE_TABLE.ANGULAR0.OCTET5.OFFSET
	LDY /SHAPE_TABLE.ANGULAR0.OCTET5.OFFSET
	JMP .OCTET.CALCULATIONS.COMPLETE

.OCTET5.ANGLE_TYPE1
	LDX #SHAPE_TABLE.ANGULAR1.OCTET5.OFFSET
	LDY /SHAPE_TABLE.ANGULAR1.OCTET5.OFFSET
	JMP .OCTET.CALCULATIONS.COMPLETE

.OCTET6_OR_7
	LDA SHAPE.POSITION.Y_GR
	BNE .OCTET7 ;branch if true
	;DEFAULT TO OCTET6 (all other possibilities eliminated)

.OCTET6
	LDA SHAPE.PROJECTILE.ANGLE.TYPE
	BEQ .OCTET6.ANGLE_TYPE0
	CMP #$02
	BCC .OCTET6.ANGLE_TYPE1
	;DEFAULT TO ANGLE_TYPE2 (all other possibilities eliminated)

	;**FALLS THROUGH

.OCTET6.ANGLE_TYPE2
	LDX #SHAPE_TABLE.ANGULAR2.OCTET6.OFFSET
	LDY /SHAPE_TABLE.ANGULAR2.OCTET6.OFFSET
	JMP .OCTET.CALCULATIONS.COMPLETE
	
.OCTET6.ANGLE_TYPE0
	LDX #SHAPE_TABLE.ANGULAR0.OCTET6.OFFSET
	LDY /SHAPE_TABLE.ANGULAR0.OCTET6.OFFSET
	JMP .OCTET.CALCULATIONS.COMPLETE

.OCTET6.ANGLE_TYPE1
	LDX #SHAPE_TABLE.ANGULAR1.OCTET6.OFFSET
	LDY /SHAPE_TABLE.ANGULAR1.OCTET6.OFFSET
	JMP .OCTET.CALCULATIONS.COMPLETE

.OCTET7
	LDA SHAPE.PROJECTILE.ANGLE.TYPE
	BEQ .OCTET7.ANGLE_TYPE0
	CMP #$02
	BCC .OCTET7.ANGLE_TYPE1
	;DEFAULT TO ANGLE_TYPE2 (all other possibilities eliminated)

	;**FALLS THROUGH

.OCTET7.ANGLE_TYPE2
	LDX #SHAPE_TABLE.ANGULAR2.OCTET7.OFFSET
	LDY /SHAPE_TABLE.ANGULAR2.OCTET7.OFFSET
	JMP .OCTET.CALCULATIONS.COMPLETE
	
.OCTET7.ANGLE_TYPE0
	LDX #SHAPE_TABLE.ANGULAR0.OCTET7.OFFSET
	LDY /SHAPE_TABLE.ANGULAR0.OCTET7.OFFSET
	JMP .OCTET.CALCULATIONS.COMPLETE

.OCTET7.ANGLE_TYPE1
	LDX #SHAPE_TABLE.ANGULAR1.OCTET7.OFFSET
	LDY /SHAPE_TABLE.ANGULAR1.OCTET7.OFFSET
	;**FALLS THROUGH**
.OCTET.CALCULATIONS.COMPLETE
	;**FALLS THROUGH**

.INIT.SHAPE_TABLE.SIZE
	LDA #SHAPE_TABLE.ANGULAR.SHAPE_PAIR.SIZE-1
	STA SHAPE_TABLE.SIZE+$0

	;**FALLS THROUGH**
	
.SET.START.ADDRESS	
	TXA ;restore lo byte of offset (angled shapes only)
	CLC
	ADC COMBAT.SHAPE_TABLE.POINTER+$0
	STA COMBAT.SHAPE_TABLE.START_ADDRESS+$0
	TYA ;restore ho byte of offset (angled shapes only)
	ADC COMBAT.SHAPE_TABLE.POINTER+$1 ;16-bit add
	STA COMBAT.SHAPE_TABLE.START_ADDRESS+$1
	

			
	;**FALLS THROUGH**
	
.COPY.SHAPE_TABLE
;COPY SHAPE TABLES FROM AUX BSR:BANK2 TO MAIN MEMORY BUFFER	
			
		;BSR:bank2 already enabled
			
	LDA COMBAT.SPELL_CODE.CAST
	CMP #$FF ;is a (C)ast Spell command in process?
	BNE .EXECUTE.COPY ;if yes, then don't enable aux memory since the shape data is stored in the spell file
			
		TSX			;transfer stack pointer to X-REG
		STX TEMP	;save stack pointer
	STA $C009 ;enable aux zero-page & aux BSR 

.EXECUTE.COPY
		;execute copy
		LDA COMBAT.SHAPE_TABLE.START_ADDRESS+$0
		STA COPY.FROM_START+$0
		LDA COMBAT.SHAPE_TABLE.START_ADDRESS+$1
		STA COPY.FROM_START+$1

		LDA COMBAT.SHAPE_TABLE.START_ADDRESS+$0
		CLC
		ADC SHAPE_TABLE.SIZE+$0
		STA COPY.FROM_END+$0
		LDA COMBAT.SHAPE_TABLE.START_ADDRESS+$1
		ADC #$00 ;16-bit add
		STA COPY.FROM_END+$1
			
		LDA #SHAPE.HOPPER0
		STA COPY.TO+$0
		LDA /SHAPE.HOPPER0
		STA COPY.TO+$1
				
	JSR MEMORY.COPY	
			
	LDA COMBAT.SPELL_CODE.CAST
	CMP #$FF ;is a (C)ast Spell command in process?
	BNE .EXECUTE.COPY.COMPLETE ;if yes, then don't modify stack pointer since aux memory was not enabled above, due to the shape data being stored in the spell file

	STA $C008 ;enable main zero-page & main BSR 
		LDX TEMP	;restore stack pointer to X-REG
		TXS ;transfer X-REG to stack pointer

.EXECUTE.COPY.COMPLETE
	;**FALLS THROUGH**
	
			; ldx #$00
; .temp.loop
			; ;lda SHAPE.DRAW.HOPPER,X
			; lda SHAPE.HOPPER0,X
			; sta $BE00,X
			; inx
			; bne .temp.loop
			; lda #$aa
			; JSR PREP.BRK
			; BRK
	
.CONVERT.SHAPE_TABLE
	JSR CONVERT.MASTER.SHAPE_TABLE			
			
	;**FALLS THROUGH**

	

			
.EXIT
	RTS ;return to COMBAT.FIRE.PROJECTILE or other calling routine 
	
@END

@END
	
	;**FALLS THROUGH**	
		
COMBAT.SHAPE.MOVE
@START
;PARAMETERS: SHAPE.PROJECTILE.SLOPE_RISE, SHAPE.PROJECTILE.SLOPE_RUN, SHAPE.POSITION.X_LT, SHAPE.POSITION.Y_LT,	
;			SHAPE.DRAW.LINE_ROW.NUMBER, SHAPE.LINE.TARGET, SHAPE.LINE.SBYTE, SHAPE.DRAW.CURRENT.SBYTE, 
;			SHAPE.MOVE.DELAY_LENGTH, SHAPE.MOVE.SCREEN_ANIMATION, SHAPE.MOVE.ERASE.TOGGLE 

;ENTRANCE: DIRECT, COMBAT.FIRE.PROJECTILE
;RETURN: HI-RES SHAPE MOVEMENT


;=====================CODE-SECTION DOCUMENTATION====================================
;
;This routine manages off-tile grid movement of shapes. It has a lot of parameters which
;are currently set by COMBAT.FIRE.PROJECTILE. See COMBAT.FIRE.PROJECTILE for documentation on the concept of
;off tile grid shape movement.  
;
;This routine increments the screen byte and hi-res line of the shape using the parameters and then
;calls COMBAT.DRAW.SHAPE to do the actual plotting. 
;
;
;-Memory Notes (general)
;
;angled shapes use first $40 of shape hopper, non-angled animated used the whole $100
;Animation manager (called by .DRAW.SHAPE in COMBAT.SHAPE.MOVE) uses different a shape hopper so there isnt a conflict.
;
;=================================================================================


;INIT VARIABLES
	LDA SHAPE.SBYTE.START ;**OPT** Speed. Memory. Since we have this here we can probably remove the init of SHAPE.DRAW.CURRENT.SBYTE above when selecting the start S_ENTITY
	STA	SHAPE.DRAW.CURRENT.SBYTE 

	;adjust start/target screen byte


	LDA SHAPE.POSITION.X_LT ;is SHAPE.SBYTE.START < SHAPE.SBYTE.TARGET (meaning target is in Octet 1,4 or 5)
	BNE .SHAPE.MOVING_RIGHT ;branch if true
;.SHAPE.MOVING_LEFT
	DEC SHAPE.DRAW.CURRENT.SBYTE ;start draw 1 screen byte to the left or right because the shape data starts in the 2nd byte of SHAPE.DRAW.HOPPER
	DEC SHAPE.SBYTE.TARGET ;also adjust target screen byte or the SHAPE.MOVE loop will terminate too soon
	JMP .NEXT.INIT
.SHAPE.MOVING_RIGHT
	DEC SHAPE.DRAW.CURRENT.SBYTE ;start draw 1 screen byte to the left or right because the shape data starts in the 2nd byte of SHAPE.DRAW.HOPPER
	;DEC to target not needed because in this case the DEC to screen byte creates more distance between start and target, whereas in the above case, the DEC to screen byte results in less distance. 
	
	;**FALLS THROUGH**
.NEXT.INIT	
	
	LDA #$00
	STA SHAPE.MOVE.INTRA_SBYTE.COUNTER
	STA SHAPE_TABLE.RESET.FLAG ;$01 = set, $00 = not set. If set then in SHAPE.MOVE, the shape tables are refreshed from the master after the screen draw is complete.
	STA SHAPE.PROJECTILE.ANIMATION_FRAME
	STA SHAPE.MOVE.ITERATION_COUNTER
	
.SHAPE.MOVE.LOOP



.DRAW.SHAPE
@START
;=====================CODE-SECTION DOCUMENTATION====================================
;
;This routine draws the shape, then updates screen animation (with it's delay overrided), then waits, then erases the 
;shape by drawing the original background
;on top of the shape.
;
;-Screen Animation Integration Notes
;
;The shape is drawn to both hi-res pages then draw-draw erased from both pages. This is because the screen animation
;update will flip the page, so we need both pages to have the shape data.
;
;
;=================================================================================

;VALIDARTE ENTRANCE

	LDA SHAPE.MOVE.ITERATION_COUNTER ;only permit entrance after 1st interation to prevent the projectile from totally covering the source tile. 
	BEQ .DRAW.SHAPE.COMPLETE
	; CMP #$02
	; BCC .DRAW.SHAPE.COMPLETE
	
	
;DRAW
			
	;foreground: draw shape in current position
		LDA #$01
		STA PAGE.FOREGROUND.OVERRIDE
		LDA #$00 ;set draw mode
	STA SHAPE.MOVE.DRAW_THREASHOLD.COUNTER ;init counter. unrelated to DRAW.SHAPE call, just a convienient location  
	JSR COMBAT.DRAW.SHAPE

	
	;is incremental erase enabled?
	;(if no, then don't draw shape to background page because when the 
	; projectile reaches it's target the projectile trail will be erased via a page flip and screen copy)
	
	LDA SHAPE.MOVE.ERASE.TOGGLE	;($00 = shape erase on | >=$01 = shape erase off)
	BNE .UPDATE.SCREEN.ANIMATION
			
	;background: draw shape in current position
		;LDA #$01
		;STA PAGE.FOREGROUND.OVERRIDE
		LDA #$00 ;set draw mode
	JSR COMBAT.DRAW.SHAPE

		LDA SHAPE.MOVE.DELAY_LENGTH ;The length of the delay between the shape draw and shape erase.
	JSR WAIT.LOOP
	
			
				
.UPDATE.SCREEN.ANIMATION
; ;UPDATE ANIMATION FRAMES ON ENTIRE SCREEN	
; ;Note: SHAPE.MOVE.SCREEN_ANIMATION.COUNTER was originally intended to cause the screen animation update to only run 
; ;every few shape draws. However, it seemed to work out better for the screen animation update to occur every shape draw
; ;and so the code for the counter is disabled. 
	
	;is screen animation enabled?
	LDA SHAPE.MOVE.SCREEN_ANIMATION.TOGGLE ;($00 = screen animation on | >=$01 = screen animation off)
	BNE .SCREEN.ANIMATION_COMPLETE	


			

	; LDA SHAPE.MOVE.SCREEN_ANIMATION.COUNTER
	; CMP #SHAPE.MOVE.SCREEN_ANIMATION.THREASHOLD
	; BCC .ANIMATION.COMPLETE	
	

		LDA #$01
		STA ANIMATION.DELAY.OVERRIDE	
	JSR ANIMATION.UPDATE.ENTRANCE
	
	
		; ;ENABLE BANK-SWITCHED RAM ($D000 Bank 1)
		; LDA $C08B
		; LDA $C08B
		; LDA #$01
		; STA ANIMATION.DELAY.OVERRIDE
	; JSR ANIMATION.UPDATE
		; ;RE-ENABLE BANK-SWITCHED RAM ($D000 Bank 2)
		; LDA $C083				;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK2 2nd)
		; LDA $C083 ;**OPT** Memory. ANIMATION.UPDATE is called a lot. put it in a subroutine. That way the BSR swithing only takes up memory once. 


			; ldx #$00
; .temp.loop
			; ;lda SHAPE.DRAW.HOPPER,X
			; LDA #$00
			; STA SHAPE.HOPPER0,X
			; inx
			; CPX #$20
			; bne .temp.loop

			
	; ;reset animation counter
	; LDA #$00
	; STA SHAPE.MOVE.SCREEN_ANIMATION.COUNTER
; .ANIMATION.COMPLETE
; INC SHAPE.MOVE.SCREEN_ANIMATION.COUNTER
	
.SCREEN.ANIMATION_COMPLETE

				;jsr keyin
				; lda #$aa
				; jsr prep.brk
				; brk

				
;TROUBLESHOOTING HOOK (pause between draws)					
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BEQ .TEMP
			
			; TXA
			; PHA
			; ;	
			; LDA #$24
			; STA VARIABLE1.HTAB
			; LDA #$07
			; STA VARIABLE1.VTAB
			; LDA #$26
			; STA VARIABLE2.HTAB
			; LDA #$08
			; STA VARIABLE2.VTAB								
			; ;
			; ;LDA SHAPE.MOVE.INTRA_SBYTE.COUNTER
			; LDA SHAPE.DRAW.CURRENT.SBYTE
			; LDX SHAPE.DRAW.LINE_ROW.NUMBER
			; ;LDX SHAPE.DRAW.CURRENT.SBYTE
			; ;LDX #$AA
			; ;LDX SHAPE.PROJECTILE.ANIMATION_FRAME
			; ;
		; JSR MONITOR.VARIABLE
			; PLA
			; TAX		
		; JSR KEYIN ;pause optional

			; ;
			; STA TEMP
			; CMP #$D1
			; BNE .TEMP
			; LDA #$01
			; STA TROUBLESHOOTING.HOOK
			; ; LDA #$AA
			; ; JSR PREP.BRK
			; ; BRK
; .TEMP
			; LDA TEMP
	

;ERASE
			;JMP .DRAW.SHAPE.COMPLETE
			
	;is incremental erase enabled?
	LDA SHAPE.MOVE.ERASE.TOGGLE	;($00 = shape erase on | >=$01 = shape erase off)
	BNE .DRAW.SHAPE.COMPLETE
					
	;foreground: draw background in current position (removing shape)
		LDA #$01
		STA PAGE.FOREGROUND.OVERRIDE
		;ACC =$01 ;set erase mode parameter
	JSR COMBAT.DRAW.SHAPE
	
	;background: draw background in current position (removing shape)
		;LDA #$01
		;STA PAGE.FOREGROUND.OVERRIDE
		LDA #$01 ;set erase mode
	JSR COMBAT.DRAW.SHAPE	

.DRAW.SHAPE.COMPLETE
	INC SHAPE.MOVE.ITERATION_COUNTER
	
	;**FALLS THROUGH**		
@END
	
.INCREMENT.SHAPE
@START

.INIT.COUNTERS_INDEXES	
	LDY #$00 ;init shape rise counter
	LDX #$00 ;init shape run counter

	;**FALLS THROUGH**
	
.INCREMENT.ANIMATION_FRAME	
@START
	;is shape animated?
	LDA SHAPE.MOVE.MODE		;($00 = angled static shape | >=$01 = non-angled, animated shape)
	BEQ .REFRESH.SHAPE.TABLES
			
	;next animation frame
	INC SHAPE.PROJECTILE.ANIMATION_FRAME
	LDA SHAPE.PROJECTILE.ANIMATION_FRAME
	CMP #$04
	BCC .NEXT.FRAME.DONE
;FLIP COUNTER
	LDA #$00
	STA SHAPE.PROJECTILE.ANIMATION_FRAME
	
	;reset shape table pointer
	JSR SET.SHAPE_TABLE.POINTER
	
	JMP .CONVERT.SHAPE.TABLE ;pointer is already set correctly for frame so .ADVANCE.POINTER can be skipped 
	;**FALLS THROUGH**
.NEXT.FRAME.DONE


.ADVANCE.POINTER ;advance shape table pointer to next animation frame

			; STA TEMP
			; ;LDA TROUBLESHOOTING.HOOK
			; lda SHAPE.PROJECTILE.ANIMATION_FRAME
			; ldx #SHAPE.HOPPER0
			; ldy /SHAPE.HOPPER0
			; CMP #$02
			; BNE .TEMP
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP
			
	;calculate AND mask address (blend in hi-res background)
	LDA #$40 ;offset between shape table pairs (AND mask table size = $20, ORA mask table size = $20)
	CLC
	ADC SHAPE.BACKGROUND.AND_MASK+$0	
	STA SHAPE.BACKGROUND.AND_MASK+$0						
	LDA	SHAPE.BACKGROUND.AND_MASK+$1
	ADC #$00 ;16-bit
	STA SHAPE.BACKGROUND.AND_MASK+$1

	;calculate ORA mask address (shape data)
	LDA #$20 ;size of the AND mask table
	CLC
	ADC SHAPE.BACKGROUND.AND_MASK+$0	
	STA SHAPE.COMPOSITE.ORA_MASK+$0						
	LDA	SHAPE.BACKGROUND.AND_MASK+$1
	ADC #$00 ;16-bit
	STA SHAPE.COMPOSITE.ORA_MASK+$1
		
		
.CONVERT.SHAPE.TABLE			
	;convert shape table of current animation frame
	JSR CONVERT.MASTER.SHAPE_TABLE

;BIT SHIFT NEW SHAPE TABLE TO LAST SHAPE POSITION (THIS ITERATION OF .INCREMENT.RISE WILL SHIFT IT AGAIN)
	;the total bits to shift is determine by SHAPE.MOVE.INTRA_SBYTE.COUNTER, which is incremented for each bit shifted,
	;and also by whether the current screen byte is add or even as that determines whether a bit adjustment is needed 
	;to align colors

	JSR SHAPE.ALIGN.COLORS			
			
	LDA SHAPE.MOVE.INTRA_SBYTE.COUNTER
	BEQ .BIT.SHIFT.COMPLETE
	;**FALLS THROUGH**
.DETERMINE.DIRECTION
	PHA ;push to stack: # of bits to shift
	LDA SHAPE.POSITION.X_LT ;is SHAPE.SBYTE.START < SHAPE.SBYTE.TARGET. (meaning target is in Octet 1,4 or 5)
	BNE .SHIFT_SHAPE.RIGHT ;branch if true
;.SHIFT_SHAPE.LEFT
	PLA ;pull from stack: # of bits to shift
	JSR BIT_SHIFT.SHAPE_LEFT.ENTRANCE
	JMP .BIT.SHIFT.COMPLETE
.SHIFT_SHAPE.RIGHT
	PLA ;pull from stack: # of bits to shift
	JSR BIT_SHIFT.SHAPE_RIGHT.ENTRANCE
.BIT.SHIFT.COMPLETE
		
	;**FALLS THROUGH**
@END
	;**FALLS THROUGH**
	
.REFRESH.SHAPE.TABLES
@START
;AFTER EACH SCREEN BYTE INCREMENT, REFRESH SHAPE TABLES
;(if doing a 2 bit increment, then after +6 bits the shape will be in the same position 
;as after the screen byte increment following the shape table refresh. Actually this is only true if the refreshed shape table is adjusted 1 bit left/right
;to aligh color columns, otherwise after +6 bits the shape will be 1 bit position different than after the screen byte increment
;following the shape table refresh). 
;This is because screen bytes only have 7 pixels. So +6 bits, almost moves the shape entirely into the next screen byte. It's only one pixel
;different. And if the refreshed shape table is adjusted by 1 bit (oppoiste the direction of shape movement), then that one pixel difference is gone. 

	LDA SHAPE_TABLE.RESET.FLAG ;$01 = set, $00 = not set. If set then in SHAPE.MOVE, the shape tables are refreshed from the master after the screen draw is complete.
	BEQ .INCREMENT.RISE ;if flag not set, then don't refresh shape tables. 

	;is shape animated?
	LDA SHAPE.MOVE.MODE		;($00 = angled static shape | >=$01 = non-angled, animated shape)
	BNE .SBYTE.INCREMENT ;if yes, skip the shape table conversion because it was already done in .INCREMENT.ANIMATION_FRAME above
	
				
	JSR CONVERT.MASTER.SHAPE_TABLE

			
.SBYTE.INCREMENT
	LDA SHAPE.POSITION.X_LT ;is SHAPE.SBYTE.START < SHAPE.SBYTE.TARGET. (meaning target is in Octet 1,4 or 5)
	BNE .SBYTE.INCREMENT.RIGHT ;branch if true
;.HORIZONTAL.SBYTE.LEFT
	DEC SHAPE.DRAW.CURRENT.SBYTE
	JMP .SBYTE.INCREMENT.DONE
.SBYTE.INCREMENT.RIGHT
	INC SHAPE.DRAW.CURRENT.SBYTE
.SBYTE.INCREMENT.DONE
		
;ALIGN BITS WITH COLOR COLUMNS
;(the odd/even color rules follow a patter that repeats every 2 screen bytes. Accordingly, every other screen byte increment
;also requires a single bit shift in the opposite direction of the shape motion on the video screen.  
;This repeating pattern every other screen byte happens because there are only 7 
;pixels were screen byte due to the high bit being the color bit instead of a pixel)		

	JSR SHAPE.ALIGN.COLORS
	
.CLEAR.FLAGS	
	LDA #$00 ;clear flag
	STA SHAPE_TABLE.RESET.FLAG ;$01 = set, $00 = not set. If set then in SHAPE.MOVE, the shape tables are refreshed from the master after the screen draw is complete.
	;**FALLS THROUGH**

@END
	
	;**FALLS THROUGH**

.INCREMENT.RISE
@START


			
	;is line horizontal?
	LDA SHAPE.PROJECTILE.SLOPE_RISE
	BEQ .INCREMENT.RUN ;if yes, then don't increment rise, proceed to increment run 

	;increment the starting hi-res line of the shape
	LDA SHAPE.POSITION.Y_LT ;is SHAPE.LINE.START < SHAPE.LINE.TARGET. (meaning target is in octet 2,5 or 6)
	BNE .Y_LT ;branch if true

.Y_GR		
	DEC SHAPE.DRAW.LINE_ROW.NUMBER

	;exit test (line)
	LDA SHAPE.DRAW.LINE_ROW.NUMBER
	CMP SHAPE.LINE.TARGET
	BCC .EXIT_STEP	
	JMP .INCREMENTS.COMPLETE

	
.Y_LT
		
	INC SHAPE.DRAW.LINE_ROW.NUMBER

	;exit test (line)
	LDA SHAPE.DRAW.LINE_ROW.NUMBER
	CMP SHAPE.LINE.TARGET
	BCS .EXIT_STEP
	;**FALLS THROUGH**
.INCREMENTS.COMPLETE


.INCREMENTAL.DRAW	
	;(VERTICLE ONLY) do incremental draw? 
	;NOTE: this is needed for verticle lines because normally the shape.draw is triggered from .INCREMENT.RUN, but that 
	;routine is skipped for verticle lines. 
	
	;is line verticle?
	LDA SHAPE.PROJECTILE.SLOPE_RUN
	BNE .EXIT.TEST ;if no, then skip this section
	
	INC SHAPE.MOVE.DRAW_THREASHOLD.COUNTER
	LDA SHAPE.MOVE.DRAW_THREASHOLD.COUNTER
	CMP #SHAPE.PROJECTILE.DRAW_THREASHOLD ;threashold for doing a draw even though run hasn't been incremented
	BCC .EXIT.TEST	
	STA SHAPE.MOVE.RECURSIVE.CALL
	JMP .SHAPE.MOVE.LOOP
	
.EXIT.TEST ;(rise counter)			
	STA SHAPE.MOVE.RECURSIVE.CALL ;RESET

	INY ;inc run counter  

	CPY SHAPE.PROJECTILE.SLOPE_RISE
	BCC .INCREMENT.RISE
		
	;INCREMENT EXTRA LINE? 
	;(for larger remainder values, to help make sure target is hit)
	LDA SHAPE.PROJECTILE.SLOPE_RISE.REMAINDER ;SLOPE: REMAINDER
	CMP #SHAPE.MOVE.EXTRA_RISE.SLOPE_THREASHOLD
	BCC .INCREMENT.RUN
	LDA SHAPE.MOVE.EXTRA_RISE.COUNTER
	CMP #SHAPE.MOVE.EXTRA_RISE.COUNTER_THREASHOLD
	BNE .REMAINDER.CHECK.COMPLETE
	LDA #$00
	STA SHAPE.MOVE.EXTRA_RISE.COUNTER ;reset counter

	JMP .INCREMENT.RISE	

.EXIT_STEP			
	JMP .EXIT.MOVE.LOOP
	
.REMAINDER.CHECK.COMPLETE
	INC SHAPE.MOVE.EXTRA_RISE.COUNTER
	;**FALLS THROUGH**
@END

.INCREMENT.RUN
@START
	;is line verticle?
	LDA SHAPE.PROJECTILE.SLOPE_RUN
	BEQ .INCREMENT.RISE ;if yes, then don't increment run, proceed to increment rise 
	;BEQ .INCREMENT.RISE_STEP ;if yes, then don't increment run, proceed to increment rise 

	;increment hi-res x-axis where shape is drawn
	LDA SHAPE.POSITION.X_LT ;is SHAPE.SBYTE.START < SHAPE.SBYTE.TARGET. (meaning target is in Octet 1,4 or 5)
	BNE .X_LT ;branch if true

	;** FALLS THROUGH
	
.X_GR
@START
	LDA BIT_SHIFT_SKIP.FLAG  ;$01 = set, $00 = not set. If set then in SHAPE.MOVE, then a bit shift isn't performed when incrementing rise. This occurs when the screen byte is incremented, making a bit shift unnecessary
	BNE .SKIP.BIT_SHIFT1 ;skip bit shift routine if the flag is set

		LDA #$01 ;# of bits to shift
	JSR BIT_SHIFT.SHAPE_LEFT.ENTRANCE
	INC SHAPE.MOVE.INTRA_SBYTE.COUNTER ;inc counter because 1 more bit has been shifted
		;****WARNING: the above INC has to change if the # of bit shifts per run increment changes
		
		
.NEXT.SCREEN.BYTE ;?	
	LDA SHAPE.MOVE.INTRA_SBYTE.COUNTER
	CMP #$06  ;the value is $06, because $08 is actually $00 of the next screen byte
	BCC .RUN.EXIT.TEST1

	;**FALLS THROUGH**
			
.RESET.SHAPE_TABLES1
	LDA #$01
	STA SHAPE_TABLE.RESET.FLAG ;$01 = set, $00 = not set. If set then in SHAPE.MOVE, the shape tables are refreshed from the master after the screen draw is complete.
	JMP .RUN.EXIT.TEST1

.SKIP.BIT_SHIFT1
	DEC SHAPE.DRAW.CURRENT.SBYTE

	; ;is shape animated?
	; LDA SHAPE.MOVE.MODE		;($00 = angled static shape | >=$01 = non-angled, animated shape)
	; BEQ .SKIP.BIT_SHIFT1.DONE ;if no, then skip color alignment because it gets done in .INCREMENT.ANIMATION_FRAME	
	
	JSR SHAPE.ALIGN.COLORS
.SKIP.BIT_SHIFT1.DONE
	
	;**FALLS THROUGH**	
	
.RUN.EXIT.TEST1

	;exit check
	LDA SHAPE.DRAW.CURRENT.SBYTE
	CMP SHAPE.SBYTE.TARGET
	BEQ .EXIT.MOVE.LOOP ;EXIT IF <=
	BCC .EXIT.MOVE.LOOP
.EXIT_TEST.COMPLETE1	
	JMP .INCREMENTS.COMPLETE1
@END

.X_LT
@START
	LDA BIT_SHIFT_SKIP.FLAG  ;$01 = set, $00 = not set. If set then in SHAPE.MOVE, then a bit shift isn't performed when incrementing rise. This occurs when the screen byte is incremented, making a bit shift unnecessary
	BNE .SKIP.BIT_SHIFT2 ;skip bit shift routine if the flag is set
	
		LDA #$01 ;# of bits to shift
	JSR BIT_SHIFT.SHAPE_RIGHT.ENTRANCE
	INC SHAPE.MOVE.INTRA_SBYTE.COUNTER ;inc counter because 1 more bit has been shifted
		;****WARNING: the above INC has to change if the # of bit shifts per run increment changes

		
;TROUBLESHOOTING.HOOK (pause, and monitor variables)
		; TXA
		; PHA
		; TYA
		; PHA
			; ;LDA SHAPE.DRAW.CURRENT.SBYTE
			; ; TYA
			; ; TAX
			; ;LDA SHAPE.DRAW.LINE_ROW.NUMBER
			; ;LDX #$AA
			; ;LDX SHAPE.DRAW.LINE_ROW.NUMBER
			; ;LDA SHAPE.DRAW.CURRENT.SBYTE
				; ;LDX SHAPE.DRAW.LINE_ROW.NUMBER
				; ;LDX SHAPE.PROJECTILE.ANIMATION_FRAME
			; LDA SHAPE.MOVE.INTRA_SBYTE.COUNTER
		; JSR MONITOR.VARIABLE
		; JSR KEYIN 
		; PLA
		; TAY
		; PLA
		; TAX
		
		
.NEXT.SCREEN.BYTE2 ;?	
	LDA SHAPE.MOVE.INTRA_SBYTE.COUNTER
	CMP #$06  ;the value is $06, because $08 is actually $00 of the next screen byte
	BCC .RUN.EXIT.TEST2
	
	;**FALLS THROUGH**
			
.RESET.SHAPE_TABLES2
	LDA #$01
	STA SHAPE_TABLE.RESET.FLAG ;$01 = set, $00 = not set. If set then in SHAPE.MOVE, the shape tables are refreshed from the master after the screen draw is complete.
	JMP .RUN.EXIT.TEST2
	
	;**FALLS THROUGH

.SKIP.BIT_SHIFT2
	INC SHAPE.DRAW.CURRENT.SBYTE
;	INC SHAPE.DRAW.CURRENT.SBYTE

	; ;is shape animated?
	; LDA SHAPE.MOVE.MODE		;($00 = angled static shape | >=$01 = non-angled, animated shape)
	; BEQ .SKIP.BIT_SHIFT2.DONE ;if no, then skip color alignment because it gets done in .INCREMENT.ANIMATION_FRAME	
		
	JSR SHAPE.ALIGN.COLORS
.SKIP.BIT_SHIFT2.DONE

	;**FALLS THROUGH
	
.RUN.EXIT.TEST2	
	;exit check
	LDA SHAPE.DRAW.CURRENT.SBYTE
	CMP SHAPE.SBYTE.TARGET
	BCS .EXIT.MOVE.LOOP
.EXIT_TEST.COMPLETE2
	;**FALLS THROUGH**
@END
	;**FALLS THROUGH**
	
.INCREMENTS.COMPLETE1	

	INX ;inc run counter		
	CPX SHAPE.PROJECTILE.SLOPE_RUN
	BEQ	.INCREMENT.RUN.COMPLETE
	JMP .INCREMENT.RUN

.INCREMENT.RUN.COMPLETE	
	JMP .SHAPE.MOVE.LOOP

@END
@END	


;.EXIT.TESTING
		
.EXIT.MOVE.LOOP

; .DRAW.PROJECTILE.ON.TARGET
		; LDA SHAPE.SBYTE.TARGET
		; STASHAPE.DRAW.CURRENT.SBYTE
		
		; LDA SHAPE.LINE.TARGET
		; STA SHAPE.DRAW.LINE_ROW.NUMBER
		; ;STA TILE.LINE.TARGET

		; ;foreground: draw shape in current position
		; LDA #$01
		; STA PAGE.FOREGROUND.OVERRIDE
		; LDA #$00 ;set draw mode
	; JSR COMBAT.DRAW.SHAPE
	
		; JSR KEYIN

		
		
; ;INSERT DELAY IF SOURCE AND TARGET ARE ADJACENT
; ;(helps the projectile be onscreen long enough to be visible)
		; LDA FP.TARGET.GMAP.X
		; STA PARM1.GMAP.X
		; LDA FP.TARGET.GMAP.Y
		; STA PARM1.GMAP.Y	
	
		; LDA FP.SOURCE.GMAP.X
		; STA PARM2.GMAP.X
		; LDA FP.SOURCE.GMAP.Y
		; STA PARM2.GMAP.Y	
	; JSR CALCULATE.DISTANCE
		; CMP #$02
		; BCS .EXIT
	
		LDA #$20
		JSR WAIT.LOOP
.EXIT		

			; ldx #$00
; .temp.loop
			; ;lda SHAPE.DRAW.HOPPER,X
			; lda SHAPE.HOPPER0,X
			; sta $BE00,X
			; inx
			; bne .temp.loop
			; lda #$aa
			; JSR PREP.BRK
			; BRK
			
			; jsr keyin
			; LDA #$AC
			; JSR PREP.BRK
			; BRK		
						
			; lda #$01
			; sta troubleshooting.hook



			
	RTS ;return to COMBAT.FIRE.PROJECTILE, which will immediately exit to the routine which called it. 
		;at the point of this RTS, the projectile has reached its target

SET.SHAPE_TABLE.POINTER
@START
;=====================CODE-SECTION DOCUMENTATION====================================
;
;DRAW.SHAPE uses the zero page variables SHAPE.BACKGROUND.AND_MASK+$0 and SHAPE.COMPOSITE.ORA_MASK+$1
;right now I'm connecting them to shape hopper because that is where I copied the shape
;tables into. But, if the shape tables are stored in the combat module then the could be
;connected directly and we should skip loading them into shape hopper.
; 
;If the shapes get stored in aux memory then I'd probaby copy them from aux memory
;into the shape hoppers, in which case the code below could get moved to SHAPE.DRAW or SHAPE.MOVE
;
;=================================================================================

	LDA #SHAPE.HOPPER0				;SAVE SHAPE.HOPPER0 AS THE DESTINATION ADDRESS FOR AUX MOVE
	STA SHAPE.BACKGROUND.AND_MASK+$0						;CONNECTS SHAPE.HOPPER0 TO SHAPE, USED BY DRAW.TILE
	LDA /SHAPE.HOPPER0
	STA SHAPE.BACKGROUND.AND_MASK+$1

		
	LDA #SHAPE.HOPPER1				;SAVE SHAPE.HOPPER0 AS THE DESTINATION ADDRESS FOR AUX MOVE
	STA SHAPE.COMPOSITE.ORA_MASK+$0						;CONNECTS SHAPE.HOPPER0 TO SHAPE, USED BY DRAW.TILE
	LDA /SHAPE.HOPPER1
	STA SHAPE.COMPOSITE.ORA_MASK+$1

	RTS
@END

CONVERT.MASTER.SHAPE_TABLE
@START
;PARAMETERS: SHAPE.HOPPER0, SHAPE.HOPPER1
;ENTRANCE: DIRECT
;RETURN SHAPE.DRAW.HOPPER


;=====================SUBROUTINE DOCUMENTATION====================================
;
;prepends and appends each byte pair in the master shape table with $00, so that each hi-res line contains 
;4 bytes in the shape table. This is so that horizontal shape movement can be done via bit shifting. 
;
;The master shape table is whatever (SHAPE.BACKGROUND.AND_MASK+$0) is pointed to, which is usually SHAPE.HOPPER0,
;a $100 byte buffer that can hold a 4 frame animation shape with AND/ORA masks for each frame. 
;=================================================================================
			
;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA
		
	LDX #$00 ;SHAPE.DRAW.HOPPER index
	LDY #$00 ;(SHAPE.BACKGROUND.AND_MASK+$0) index
.CONVERT.MASTER.SHAPE_TABLE.LOOP ;INIT SECTION OF SHAPE.MOVE
	
	CPX #$40 ;is current record an AND mask or ORA mask?
	BCS .SET.EXTRA_BYTES.ORA_MASK
;.SET.EXTRA_BYTES.AND_MASK
	LDA #$FF  ;mask in all bits from background
	JMP .SAVE.EXTRA_BYTES
.SET.EXTRA_BYTES.ORA_MASK
	LDA #$00  ;setting $00 results in no shape pixels drawn in this byte
	;**FALLS THROUGH**
.SAVE.EXTRA_BYTES
	STA SHAPE.DRAW.HOPPER+0,X ;prepend $00
	STA SHAPE.DRAW.HOPPER+3,X ;append $00

	;copy the bytes containing graphics data
	LDA (SHAPE.BACKGROUND.AND_MASK+$0),Y
	STA SHAPE.DRAW.HOPPER+$1,X
	INY ;next master shape table byte
	LDA (SHAPE.BACKGROUND.AND_MASK+$0),Y
	STA SHAPE.DRAW.HOPPER+$2,X

	;increment indexes
	INY ;next master shape table byte

	INX ;next draw.hopper record
	INX ;next draw.hopper record
	INX ;next draw.hopper record
	INX ;next draw.hopper record
	CPX #$80
	BNE .CONVERT.MASTER.SHAPE_TABLE.LOOP

	
;RESET INTRA SCREEN BYTE COUNTER
;(except if shape is animated, because in that case this routine is called after every screen draw as part of incrementing the animation frame.
;(for non-animated shapes, this routine is only called after enough bit shifts have been done to equal a full screen byte)

	LDA SHAPE.MOVE.INTRA_SBYTE.COUNTER
	CMP #$06
	BCC .EXIT
	
	LDA #$00 ;reset counter. This is needed when the shape tables are reset after a screen byte increment. 
	STA SHAPE.MOVE.INTRA_SBYTE.COUNTER

.EXIT
	

		
;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX
	
	RTS
@END

BIT_SHIFT.SHAPE_RIGHT.ENTRANCE
@START
;=====================SUBROUTINE DOCUMENTATION====================================
;
;See mycode/documentation/misc/SWAP.ROUTINES.Combat.txt
;
;For some reason the SBASM cross assembler crashes when the documentation text
;is in the source file, so I moved it to the text file for now. 
;
;=================================================================================

	;ACC = number of bits/bixels to shift, as specified above
	STA SHAPE.MOVE.BIT_SHIFT.QTY

;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA

	LDY #$00 ;init bit shift loop counter
.BIT_SHIFT.SHAPE_RIGHT.OUTER_LOOP ;shift shape right 1 pixel
	TYA
	PHA ;push loop counter to stack

	LDX #$00 ;init shape table record index
.BIT_SHIFT.SHAPE_RIGHT.INNER_LOOP ;shift current shape table record
	
;===BYTE1===
.BYTE1.SHAPE_SHIFT_RIGHT.START
	LDA SHAPE.DRAW.HOPPER+$1,X	
	ASL ;shift bits left (shift shape right)
		;bit7(color setting) shifted to carry flag, bit6 shifted to bit7, zero placed in bit0,  

.BYTE1.GET.TRANSFER_OUT.VALUE1
	TAY ;save shape byte1	

;test bit7: get transfer value to byte2, bit0	
	BMI .BYTE1.TRANSFER_OUT_VALUE.SET1 ;returns true if the high bit (bit7) is set
;.TRANSFER_VALUE.NOT_SET
	LDA #$00
	JMP .BYTE1.SAVE.TRANSFER_OUT_VALUE1	
	
.BYTE1.TRANSFER_OUT_VALUE.SET1	
	LDA #$01
.BYTE1.SAVE.TRANSFER_OUT_VALUE1	
	STA SHAPE.DRAW.BYTE.TRANSFER_VALUE
	;**FALLS THROUGH**
	
;test carry flag: get color bit value	
.BYTE1.COLOR.TEST1
	TYA ;restore shape byte1
	BCS .BYTE1.COLOR.BIT.SET1	
;.BYTE1.COLOR.BIT.NOT_SET1
	AND #$7F ;mask out bit7

		; LDY #$EE
		; JSR PREP.BRK
		; BRK
		
	JMP .BYTE1.SET.BIT0
	
.BYTE1.COLOR.BIT.SET1
	ORA #$80 ;mask in bit7
	;**FALLS THROUGH**
	
.BYTE1.SET.BIT0
;SET BIT0 VALUE (IF AND MASK).
;(a zero was set in bit0 as a result of ASL above)
	CPX #$40 ;is current record in a shape table AND mask or ORA mask?
	BCS .BYTE1.SAVE1 ;branch if in ORA mask
;.BYTE1.SET.BIT0
	ORA #$01 ;mask-in bit0
	;**FALLS THROUGH**

	
.BYTE1.SAVE1	
	STA SHAPE.DRAW.HOPPER+$1,X	
	;**FALLS THROUGH**

;===BYTE2===
.BYTE2.SHAPE_SHIFT_RIGHT.START
	LDA SHAPE.DRAW.HOPPER+$2,X	
	ASL ;shift bits left (shift shape right)
		;bit7(color setting) shifted to carry flag, bit6 shifted to bit7, zero placed in bit0,  

;mask in transfer value from byte1	
	LDY SHAPE.DRAW.BYTE.TRANSFER_VALUE
	BNE .BYTE2.TRANSFER_IN_VALUE.SET1
;.BYTE2.TRANSFER_IN_VALUE.NOT_SET1
	AND #$FE ;mask out bit0
	JMP .BYTE2.GET.TRANSFER_OUT_VALUE1
.BYTE2.TRANSFER_IN_VALUE.SET1
	ORA #$01 ;mask in bit 0
	;**FALLS THROUGH**
	
.BYTE2.GET.TRANSFER_OUT_VALUE1
	TAY ;save shape byte2	

	;test bit7: get transfer value to byte3, bit0	
	BMI .BYTE2.TRANSFER_OUT_VALUE.SET1 ;returns true if the high bit (bit7) is set
;.TRANSFER_VALUE.NOT_SET
	LDA #$00
	JMP .BYTE2.SAVE.TRANSFER_OUT_VALUE1	
.BYTE2.TRANSFER_OUT_VALUE.SET1
	LDA #$02
.BYTE2.SAVE.TRANSFER_OUT_VALUE1	
	STA SHAPE.DRAW.BYTE.TRANSFER_VALUE
	;**FALLS THROUGH**
	
;test carry flag: get color bit value	
.BYTE2.COLOR.TEST1
	TYA ;restore shape byte2
	BCS .BYTE2.COLOR.BIT.SET1	
;.BYTE2.COLOR.BIT.NOT_SET1
	AND #$7F ;mask out bit7
	JMP .BYTE2.SAVE1
	
.BYTE2.COLOR.BIT.SET1
	ORA #$80 ;mask in bit7
	
.BYTE2.SAVE1	
	PHA ;push to stack: shape byte for byte3 color test
	STA SHAPE.DRAW.HOPPER+$2,X	
	;**FALLS THROUGH**

;===BYTE3===

	;is screem byte mode enabled for horizontal increments? 
	LDA SHAPE.PROJECTILE.HORIZONAL_INC.MODE		; $00 = screen byte, >= $01 = pixels (bit shifting)
	BNE .BYTE3.SHAPE_SHIFT_RIGHT.START ;if no, then proceed to shift byte3 
					;if yes, then skip byte $03 shift	
	PLA ;clean up stack from PHA in .BYTE2.SAVE1 above
	JMP .BYTE3.DONE 
	
.BYTE3.SHAPE_SHIFT_RIGHT.START
	LDA SHAPE.DRAW.HOPPER+$3,X	
	ASL ;shift bits left (shift shape right)
		;bit7(color setting) shifted to carry flag, bit6 shifted to bit7, zero placed in bit0,  
	
;mask in color bit from byte2
	TAY ;save byte3

	;test bit7 of byte2
	PLA ;pull from stack: shape byte2. This operation seeds the BMI test as the BMI is based on the bit7 result of the last operation performed. 
	BMI .BYTE3.COLOR.TRANSFER_IN_VALUE.SET1 ;returns true if the high bit (bit7) is set	
;.BYTE3.COLOR.TRANSFER_IN_VALUE.NOT_SET1
	TYA ;restore byte3
	AND #$7F ;mask out bit7
	JMP .BYTE3.TRANSFER_IN.VALUE
.BYTE3.COLOR.TRANSFER_IN_VALUE.SET1
	TYA
	ORA #$80 ;mask in bit7

	
.BYTE3.TRANSFER_IN.VALUE	
;mask in transfer value from byte1	
	LDY SHAPE.DRAW.BYTE.TRANSFER_VALUE
	BNE .BYTE3.TRANSFER_IN_VALUE.SET1
;.BYTE3.TRANSFER_IN_VALUE.NOT_SET1
	AND #$FE ;mask out bit0
	JMP .BYTE3.SAVE1
.BYTE3.TRANSFER_IN_VALUE.SET1
	ORA #$01 ;mask in bit 0
	;**FALLS THROUGH**
	
.BYTE3.SAVE1	
	STA SHAPE.DRAW.HOPPER+$3,X	
	;**FALLS THROUGH**

.BYTE3.DONE

	;next shape table record. each byte pair corresponds with one hi-res line of the shape
	INX 
	INX
	INX
	INX
	;inner loop exit test: at last shape table record?
	CPX #$80
	BNE .BIT_SHIFT.SHAPE_RIGHT.INNER_LOOP
	; BEQ .LOOP.DONE
	; JMP .BIT_SHIFT.SHAPE_RIGHT.INNER_LOOP
; .LOOP.DONE

	;outer loop exit test: has shape been moved the total number of pixels requested by calling routine?

	PLA ;restore loop counter
	TAY

	INY ;increment loop counter
	CPY SHAPE.MOVE.BIT_SHIFT.QTY
	BEQ .EXIT.BIT_SHIFT.LOOP
	JMP .BIT_SHIFT.SHAPE_RIGHT.OUTER_LOOP

.EXIT.BIT_SHIFT.LOOP	
;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX
	
	RTS
@END
	
BIT_SHIFT.SHAPE_LEFT.ENTRANCE
@START
;PARAMETERS: ACC (number of bits/bixels to shift)
;RETURN: SHAPE.DRAW.HOPPER
;ENTRANCE: SHAPE.MOVE

;=====================SUBROUTINE DOCUMENTATION====================================
;
;See BIT_SHIFT.SHAPE_RIGHT.ENTRANCE for documentation. 
;
;
;=================================================================================



	;ACC = number of bits/bixels to shift, as specified above
	STA SHAPE.MOVE.BIT_SHIFT.QTY
	
;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA

	LDY #$00 ;init bit shift loop counter
.BIT_SHIFT.SHAPE_LEFT.OUTER_LOOP ;shift shape right 1 pixel
	TYA
	PHA ;push loop counter to stack

	LDX #$00 ;init shape table record index
.BIT_SHIFT.SHAPE_LEFT.INNER_LOOP ;shift current shape table record
	

			
;===BYTE2===
.BYTE2.SHAPE_SHIFT_LEFT.START
	LDA SHAPE.DRAW.HOPPER+$2,X	
	LSR ;shift bits right (shift shape left)
		;zero placed in bit7, bit7(color setting) shifted to bit6, bit0 shifted to carry flag

.BYTE2.GET.TRANSFER_OUT_VALUE
	TAY ;save shape byte2	

	;get transfer value to byte1, bit
	BCS .BYTE2.TRANSFER_OUT_VALUE.SET2
;.TRANSFER_VALUE.NOT_SET
	LDA #$00
	JMP .BYTE2.SAVE.TRANSFER_OUT_VALUE2	
	
.BYTE2.TRANSFER_OUT_VALUE.SET2	
	LDA #$01
.BYTE2.SAVE.TRANSFER_OUT_VALUE2	
	STA SHAPE.DRAW.BYTE.TRANSFER_VALUE
	;**FALLS THROUGH**
	
;test overflow flag: get color bit value	
.BYTE2.COLOR.TEST2
	TYA ;restore shape byte1
	STA TEMP ;transfer shape byte1 to memory address for BIT test
	BIT TEMP ;BIT moves bit 6 of value in TEMP to overflow flag
	BVS .BYTE2.COLOR.BIT.SET2	
;.BYTE2.COLOR.BIT.NOT_SET2
	AND #$7F ;mask out bit7

;SET BIT6 VALUE (IF AND MASK).
;(bit7 was moved into bit6 as a result of ASL above, the value of which could be $00 or $01, but we know it's $00 based on the BVS test above)
;(If the current byte record is for an AND mask shape table then we need bit6 to be $01)
	CPX #$40 ;is current record in a shape table AND mask or ORA mask?
	BCC .BYTE2.SET.BIT6 ;branch if an AND mask record
 	JMP .BYTE2.SAVE2
	
.BYTE2.COLOR.BIT.SET2
	ORA #$80 ;mask in bit7

;SET BIT6 VALUE (IF AND MASK).
;(bit7 was moved into bit6 as a result of ASL above, the value of which could be $00 or $01, but we know it's $01 based on the BVS test above)
;(If the current byte record is for an ORA mask shape table then we need bit6 to be $00)
	CPX #$40 ;is current record in a shape table AND mask or ORA mask?
	BCC .BYTE2.SAVE2 ;branch if an AND mask record

	;**FALLS THROUGH** ;if an ORA MASK record

.BYTE2.NOT_SET.BIT6 ;for ORA mask records
	AND #$BF ;mask-out bit6	
	JMP .BYTE2.SAVE2	
		;**left off troubleshooting

	
.BYTE2.SET.BIT6 ;for AND mask records
	ORA #$40 ;mask-in bit6
	;**FALLS THROUGH**
		
.BYTE2.SAVE2	
	STA SHAPE.DRAW.HOPPER+$2,X	
	;**FALLS THROUGH**


			
;===BYTE1===	
.BYTE1.SHAPE_SHIFT_LEFT.START
	LDA SHAPE.DRAW.HOPPER+$1,X	
	
		;STA TEMP16

	LSR ;shift bits right (shift shape left)
		;zero placed in bit7, bit7(color setting) shifted to bit6, bit0 shifted to carry flag

		
;test overflow flag: get color bit value	
.BYTE1.COLOR.TEST2
	STA TEMP ;transfer shape byte1 to memory address for BIT test
	BIT TEMP ;BIT moves bit 6 of value in TEMP to overflow flag
	BVS .BYTE1.COLOR.BIT.SET2	
;.BYTE1.COLOR.BIT.NOT_SET2
	AND #$7F ;mask out bit7
	JMP .BYTE1.PUT.TRANSFER_IN_VALUE
			;**ABOVE JUMP WAS WHERE IT WENT OFF TRACK
			
.BYTE1.COLOR.BIT.SET2
	ORA #$80 ;mask in bit7
	;**FALLS THROUGH**

.BYTE1.PUT.TRANSFER_IN_VALUE
	;mask in transfer value from byte2, to byte1 bit6
	LDY SHAPE.DRAW.BYTE.TRANSFER_VALUE
	BNE .BYTE1.TRANSFER_IN_VALUE.SET2
;.BYTE1.TRANSFER_IN_VALUE.NOT_SET2
	AND #$BF ;mask out bit6
	JMP .BYTE1.GET.TRANSFER_OUT_VALUE2
.BYTE1.TRANSFER_IN_VALUE.SET2
	ORA #$40 ;mask in bit6
	;**FALLS THROUGH**
	
.BYTE1.GET.TRANSFER_OUT_VALUE2
	TAY ;save shape byte1	

	;get transfer value to byte0, bit6
	BCS .BYTE1.TRANSFER_OUT_VALUE.SET2
;.TRANSFER_VALUE.NOT_SET
	LDA #$00
	JMP .BYTE1.SAVE.TRANSFER_OUT_VALUE2
	
.BYTE1.TRANSFER_OUT_VALUE.SET2	
	LDA #$01
.BYTE1.SAVE.TRANSFER_OUT_VALUE2	
	STA SHAPE.DRAW.BYTE.TRANSFER_VALUE
	;**FALLS THROUGH**
	
.BYTE1.SAVE
	TYA ;restore shape byte1
	PHA ;push to stack: shape byte for byte0 color test
	STA SHAPE.DRAW.HOPPER+$1,X	
	;**FALLS THROUGH**
	
;===BYTE0===
	;is screem byte mode enabled for horizontal increments? 
	LDA SHAPE.PROJECTILE.HORIZONAL_INC.MODE		; $00 = screen byte, >= $01 = pixels (bit shifting)
	BNE .BYTE0.SHAPE_SHIFT_LEFT.START	;if no, then proceed to shift byte3 
					;if yes, then skip byte $03 shift	
	PLA ;clean up stack from PHA in .BYTE1.SAVE1 above
	JMP .BYTE0.DONE 
	
.BYTE0.SHAPE_SHIFT_LEFT.START
	LDA SHAPE.DRAW.HOPPER+$0,X	
	LSR ;shift bits right (shift shape left)
		;zero placed in bit7, bit7(color setting) shifted to bit6, bit0 shifted to carry flag

;mask in color bit from byte1
	TAY ;save shape byte0
	
	;test bit7 of byte1
	PLA ;pull from stack: shape byte1. This operation seeds the BMI test as the BMI is based on the bit7 result of the last operation performed. 
	BMI .BYTE0.COLOR.TRANSFER_IN_VALUE.SET2 ;returns true if the high bit (bit7) is set	
;.BYTE2.COLOR.TRANSFER_IN_VALUE.NOT_SET2
	TYA ;restore shape byte0
	AND #$7F ;mask out bit7
	JMP .BYTE0.TRANSFER_IN.VALUE
		
.BYTE0.COLOR.TRANSFER_IN_VALUE.SET2
	TYA ;restore shape byte0
	ORA #$80 ;mask in bit7
	;**FALLS THROUGH**
	
.BYTE0.TRANSFER_IN.VALUE	
;mask in transfer value from byte1, to bit6	
	LDY SHAPE.DRAW.BYTE.TRANSFER_VALUE
	BNE .BYTE0.TRANSFER_IN_VALUE.SET2
;.BYTE0.TRANSFER_IN_VALUE.NOT_SET0
	AND #$BF ;mask out bit6
	JMP .BYTE0.SAVE1
.BYTE0.TRANSFER_IN_VALUE.SET2
	ORA #$40 ;mask in bit6
	;**FALLS THROUGH**

.BYTE0.SAVE1	
	STA SHAPE.DRAW.HOPPER+$0,X	
.BYTE0.DONE

	;**FALLS THROUGH**
			
	;next shape table record. each byte pair corresponds with one hi-res line of the shape
	INX 
	INX
	INX
	INX
	;inner loop exit test: at last shape table record?
	CPX #$80
	;BNE .BIT_SHIFT.SHAPE_LEFT.INNER_LOOP
	BEQ .SHAPE_LEFT.LOOP.DONE
	JMP .BIT_SHIFT.SHAPE_LEFT.INNER_LOOP
.SHAPE_LEFT.LOOP.DONE		
	;outer loop exit test: has shape been moved the total number of pixels requested by calling routine?

	PLA ;restore loop counter
	TAY

	INY ;increment loop counter
	CPY SHAPE.MOVE.BIT_SHIFT.QTY
	BEQ .EXIT.BIT_SHIFT.LOOP2
	JMP .BIT_SHIFT.SHAPE_LEFT.OUTER_LOOP

.EXIT.BIT_SHIFT.LOOP2	
;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX
	
	RTS	
@END

SHAPE.ALIGN.COLORS
@START
;PARAMETERS: SHAPE.DRAW.CURRENT.SBYTE
;ENTRANCE: DIRECT
;RETURN: SHAPE.DRAW.HOPPER

;=====================SUBROUTINE DOCUMENTATION====================================
;ALIGN BITS WITH COLOR COLUMNS
;(the odd/even color rules follow a patter that repeats every 2 screen bytes. Accordingly, every other screen byte increment
;also requires a single bit shift in the opposite direction of the shape motion on the video screen.  
;
;This repeating pattern every other screen byte happens because there are only 7 
;pixels were screen byte due to the high bit being the color bit instead of a pixel)		
;
;=================================================================================

	
					
		LDA #$01 ;set AND mask
	BIT SHAPE.DRAW.CURRENT.SBYTE ;test bit0 of memory address. BIT ANDs the memory address value with the ACC and sets the zero flag based on the result. 
	BNE .ODD.SCREEN_BYTES ;if screen byte is an odd number, shape data is in correct color columns. Skip bit shift

	;**FALLS THROUGH**
	
;.ODD.SCREEN_BYTES	
	LDA SHAPE.POSITION.X_LT ;is SHAPE.SBYTE.START < SHAPE.SBYTE.TARGET. (meaning target is in Octet 1,4 or 5)
	BNE .SHAPE.MOVING.RIGHT ;branch if true
;.SHAPE.MOVING.LEFT
		LDA #$01 ;do 1 bit shift
	JSR BIT_SHIFT.SHAPE_LEFT.ENTRANCE
	JMP .EXIT
.SHAPE.MOVING.RIGHT
		LDA #$01 ;do 1 bit shift
	JSR BIT_SHIFT.SHAPE_RIGHT.ENTRANCE
	JMP .EXIT

.ODD.SCREEN_BYTES
	JSR CONVERT.MASTER.SHAPE_TABLE	
	;**FALLS THROUGH**

.EXIT
	RTS
	
@END
	
@END
	
;COMBAT.DRAW.SHAPE.ORIGINAL ;has draw/erase function for a single tile drawn with an AND + ORA mask			
@START
; ;PARAMETERS:  	;ACC = ($00 = draw mode | $01 = erase mode), SHAPE.DRAW.LINE_ROW.NUMBER, TILE.LINE.START, *TILE.DEPTH, SHAPE.DRAW.CURRENT.SBYTE, [PAGE.FOREGROUND.OVERRIDE]
; ;RETURN: NONE
; ;ENTRANCE: COMBAT.SHAPE.MOVE
; ;*Set to default value on game launch. When modified (i.e. tall grass), it is reset to default when done.

; ;=====================SUBROUTINE DOCUMENTATION====================================
; ;This subroutine and DRAW.TILE are the lowest level routine associated with tile graphics plotting. 
; ;See DRAW.TILE for general documentation on the methodology used to plot to the graphics screen.
; ;
; ;This subroutine is specifically designed to draw shapes which are objects drawn on top of a background. The underlying
; ;terrain can be supplied by terrain tiles or other object tiles. However, this routine can draw over any 14x16 section
; ;the hi-res screen whether it conforms to the tile grid or not.
; ;
; ;To blend shapes into the background, two shape tables are used. One containing and AND mask and the other containing
; ;an ORA mask.
; ;
; ;here is a quick summary of the concept:
; ;(also see the first two posts from Michael Pohoreski in the following
; ;post https://groups.google.com/forum/#!topic/comp.sys.apple2.programmer/cgxZOBIL728)
; ;
; ; byte1: 1111110
; ; byte2: 0000010
; ;
; ; Using AND on byte 1 and byte 2 results in:
; ;
; ; result: 0000010
; ;
; ; (i.e. each bit is assigned the value 1 only if both byte1 AND byte2 have a 1 in that bit position.
; ;
; ;
; ; OR works similarly except that a 1 is assigned to each bit position in the result if either byte1 OR byte2 has a 1 in that bit position. 
; ;
; ;
; ; The technique works like this:
; ;
; ; before drawing the shape copy the background, and apply an AND value to each byte. The AND values has "0" values where the shape will be. The result is a shape table that has the background and a small "hole" carved out where the shape will go.
; ;
; ; Then apply an OR value to each byte. the OR values have "1" values where the shape is. The result is shape table with 1s where either the background or the shape is. This is different from simply drawing the shape on top of the background because when the "hole" is carved out, a two pixel buffer is put around the shape so that the background pixels and the shape pixels don't interact to create unwanted lines and colors. 
; ;=================================================================================
	
	; ;ACC = $00 = draw mode | $01 = erase mode
	; STA DRAW.SHAPE.MODE

; ;**OPT** Speed. Include an in-line code version of DRAW.TILE with DRAW.COLUMN and DRAW.ROW to avoid the costly JSR/RTS to call as a subroutine. 

; .SAVE.REGISTERS
	; TXA
	; PHA
	; TYA
	; PHA

; .START

	; LDX SHAPE.DRAW.LINE_ROW.NUMBER			;LOAD LINE IN X REGISTER	
	; TXA ;transfer SHAPE.DRAW.LINE_ROW.NUMBER to ACC
	; STA TILE.LINE.CNTR 
	; STA TILE.LINE.START
	; CLC						;*OPT* I I THINK ALL CLS CAN BE REMOVED WHEN I'M NOT EXPECTING AN OVERFLOW
	; ADC TILE.DEPTH			;ADD DEPTH OF SHAPE TO STARTING LINE IN HEX (# of lines, not the position of last line....so line positions $0-$F is $10 (!16) total lines)
	; STA TILE.LINE.STOP		;SET THE LAST LINE TO DRAW BASED ON THE DEPTH OF THE TILE

	; LDA #$00
	; STA SHP.TBL.CNTR		;START DRAWING AT BEGINNING OF SHAPE TABLE
	
	
; .DRAW.LOOP

; ;GET LINE ADDRESS
; @START

; ;==========INLINE CODE GET.LINE.ADDRESS1==================	
	; LDA PAGE.FOREGROUND.OVERRIDE	;CHECK TO SEE IF CALLING ROUTINE WANTS ERASE DONE TO FOREGROUND INSTEAD OF BACKGROUND (DEFAULT)
	; CMP #$01
	; BEQ .FOREGROUND_OVERRIDE
	
	; LDA PAGE.BACKGROUND		
	; CMP #$02
	; BEQ .LOOKUP.PAGE2
	; JMP .LOOKUP.PAGE1
	
; .FOREGROUND_OVERRIDE
	; LDA PAGE.FOREGROUND		;SPECIFY PAGE TO GET ADDRESS FOR (SHOULD BE BACKGROUND PAGE)
	; CMP #$02
	; BEQ .LOOKUP.PAGE2
	

; .LOOKUP.PAGE1	
	; LDA LINE.HO.P1,X		;GET LINE ADDRESS
	; STA LINE.BASE.ADDR1+$1
	; LDA LINE.LO,X
	; STA LINE.BASE.ADDR1
	; JMP .GETLINE_COMPLETE

; .LOOKUP.PAGE2
	; LDA LINE.HO.P2,X		;GET LINE ADDRESS
	; STA LINE.BASE.ADDR1+$1
	; LDA LINE.LO,X
	; STA LINE.BASE.ADDR1
; .GETLINE_COMPLETE
; ;========================================================
; @END
	
; .DRAW_ERASE.ENTRANCE
	; LDA DRAW.SHAPE.MODE ;ACC = $00 = draw mode | $01 = erase mode
	; BNE .ERASE.MODE
	; ;**FALLS THROUGH**
	
; .DRAW.MODE	
; ;screen byte 0
	; LDY SHP.TBL.CNTR		;LOAD Y WITH SHAPE TABLE COUNTER	
	; ;init counters/indexes
	; TYA ;init X-REG to $00 as the SHAPE.EXISTING_DATA.BUFFER index
	; TAX ;""
	

	; ;save existing graphics data
	; LDY SHAPE.DRAW.CURRENT.SBYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	; LDA (LINE.BASE.ADDR1),Y	;load existing data from video memory
	; STA SHAPE.EXISTING_DATA.BUFFER,X
			; ;STA TEMP
	; ;load new shape data
	; ;ACC = existing graphics data
	; LDY SHP.TBL.CNTR		;LOAD Y WITH SHAPE TABLE COUNTER	
	; AND (SHAPE.BACKGROUND.AND_MASK),Y
	; ORA (SHAPE.COMPOSITE.ORA_MASK),Y


	; ;write new shape data
	; LDY SHAPE.DRAW.CURRENT.SBYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	; STA (LINE.BASE.ADDR1),Y	;PLOT (1st screen byte)
	
; ;next screen byte
	; INC SHP.TBL.CNTR		;NEXT SHAPE BYTE
	; INX	;next existing data buffer byte
	; INC SHAPE.DRAW.CURRENT.SBYTE			;SWITCH TO 2ND SCREEN BYTE IN THE TILE	
	
; ;screen byte 1
	
	; ;save existing graphics data
	; LDY SHAPE.DRAW.CURRENT.SBYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	; LDA (LINE.BASE.ADDR1),Y	;load existing data from video memory
	; STA SHAPE.EXISTING_DATA.BUFFER,X
		
	; ;load new shape data
	; ;ACC = existing graphics data
	; LDY SHP.TBL.CNTR		;LOAD Y WITH SHAPE TABLE COUNTER	
	; AND (SHAPE.BACKGROUND.AND_MASK),Y
	; ORA (SHAPE.COMPOSITE.ORA_MASK),Y

	; ;write new shape data
	; LDY SHAPE.DRAW.CURRENT.SBYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	; STA (LINE.BASE.ADDR1),Y	;PLOT (1st screen byte)
	; JMP .NEXT.LINE
	
; .ERASE.MODE
; ;screen byte0
	; LDY SHP.TBL.CNTR		;LOAD Y WITH SHAPE TABLE COUNTER
	; LDA SHAPE.EXISTING_DATA.BUFFER,Y			;LOAD SHAPE BYTE (1st screen byte of the tile)

	; LDY SHAPE.DRAW.CURRENT.SBYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	; STA (LINE.BASE.ADDR1),Y	;PLOT (1st screen byte)

; ;next screen byte	
	; INC SHP.TBL.CNTR		;NEXT SHAPE BYTE, NEXT TILE LINE
	; INC SHAPE.DRAW.CURRENT.SBYTE			;SWITCH TO 2ND SCREEN BYTE IN THE TILE

; ;screen byte1	
	; LDY SHP.TBL.CNTR		;LOAD Y WITH SHAPE TABLE COUNTER
	; LDA SHAPE.EXISTING_DATA.BUFFER,Y			;LOAD SHAPE BYTE (2nd screen byte of the tile)
	; LDY SHAPE.DRAW.CURRENT.SBYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	; STA (LINE.BASE.ADDR1),Y	
	; ;**FALLS THROUGH**
	
; .NEXT.LINE
; ;next line, reset screen byte	
	; DEC SHAPE.DRAW.CURRENT.SBYTE			;SWITCH BACK TO 1ST SCREEN BYTE IN THE TILE	;**OPT** if the counters are updated after the CMP/branch it might save 1 INC SHAPE.DRAW.CURRENT.SBYTE instruction in the main loop because SHAPE.DRAW.CURRENT.SBYTE will be in 2nd position when this loop ends. 
	
	; INC SHP.TBL.CNTR		;NEXT SHAPE BYTE
	; INC TILE.LINE.CNTR		;NEXT TILE LINE
	; LDX TILE.LINE.CNTR 		;restore tile line number to X-REG

; ;exit test	
	; CPX TILE.LINE.STOP		;IS TILE DONE?							
	; BCS .DONE				
	; JMP .DRAW.LOOP
; .DONE	
	; LDA #$00
	; STA PAGE.FOREGROUND.OVERRIDE
				
; .RESTORE.REGISTERS
	; PLA
	; TAY
	; PLA
	; TAX
	
	; RTS

@END	

COMBAT.DRAW.SHAPE ;			
@START
;PARAMETERS:  	;ACC = ($00 = draw mode | $01 = erase mode), SHAPE.DRAW.LINE_ROW.NUMBER, SHAPE.DRAW.CURRENT.SBYTE, [PAGE.FOREGROUND.OVERRIDE]
;RETURN: NONE
;ENTRANCE: COMBAT.SHAPE.MOVE
;*Set to default value on game launch. When modified (i.e. tall grass), it is reset to default when done.

;=====================SUBROUTINE DOCUMENTATION====================================
;
;See COMBAT.FIRE.PROJECTILE for documentation on the concept of
;off tile grid shape movement.  
;
;-Summary (extremely condensed)
;
;*Receive start screen byte and start line as parameters 
;*plot 4 screen bytes from shape table per hi-res line.
;*Increment line until end of shape table reached.
;*Line addresses are obtained via lookup tables. 
;
;-Details
;This subroutine and DRAW.TILE are the lowest level routine associated with tile graphics plotting. 
;See DRAW.TILE for general documentation on the methodology used to plot to the graphics screen.
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
;
;
;
;=================================================================================
	
	;ACC = $00 = draw mode | $01 = erase mode
	STA DRAW.SHAPE.MODE

.SAVE.REGISTERS
	TXA
	PHA
	TYA
	PHA

.START
	LDA SHAPE.DRAW.CURRENT.SBYTE
	STA SHAPE.DRAW.START.SBYTE
	CLC
	ADC #$04
	STA SHAPE.DRAW.STOP.SBYTE 
	
	LDX SHAPE.DRAW.LINE_ROW.NUMBER			;LOAD LINE IN X REGISTER	
	TXA ;transfer SHAPE.DRAW.LINE_ROW.NUMBER to ACC
	STA TILE.LINE.CNTR 
		
	CLC						;*OPT* I I THINK ALL CLS CAN BE REMOVED WHEN I'M NOT EXPECTING AN OVERFLOW
	ADC TILE.DEPTH			;ADD DEPTH OF SHAPE TO STARTING LINE IN HEX (# of lines, not the position of last line....so line positions $0-$F is $10 (!16) total lines)
	STA TILE.LINE.STOP		;SET THE LAST LINE TO DRAW BASED ON THE DEPTH OF THE TILE

	LDA #$00
	STA SHP.TBL.CNTR		;START DRAWING AT BEGINNING OF SHAPE TABLE
	STA SHAPE.DRAW.SCREEN_BYTE.COUNTER ;4 screen bytes are plotted per hi-res line. This counter tracks the number of screen bytes plotted on the current line

			
.DRAW.LOOP

;GET LINE ADDRESS
@START

;==========INLINE CODE GET.LINE.ADDRESS1==================	
	LDA PAGE.FOREGROUND.OVERRIDE	;CHECK TO SEE IF CALLING ROUTINE WANTS ERASE DONE TO FOREGROUND INSTEAD OF BACKGROUND (DEFAULT)
	BNE .FOREGROUND_OVERRIDE
	
	LDA PAGE.BACKGROUND
	JMP .GET.LINE.ADDRESS

.FOREGROUND_OVERRIDE
	LDA PAGE.FOREGROUND		;SPECIFY PAGE TO GET ADDRESS FOR (SHOULD BE BACKGROUND PAGE)

	;**FALLS THROUGH**

.GET.LINE.ADDRESS	
	JSR GET.LINE.ADDRESS1
	
@END
	
	LDX SHP.TBL.CNTR
	
.DRAW_ERASE.ENTRANCE
	LDA DRAW.SHAPE.MODE ;ACC = $00 = draw mode | $01 = erase mode
	BNE .ERASE.MODE.LOOP
	;**FALLS THROUGH**
	
.DRAW.MODE.LOOP	
;screen byte 0-3
	;save existing graphics data
	LDY SHAPE.DRAW.CURRENT.SBYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	LDA (LINE.BASE.ADDR1),Y	;load existing data from video memory
	STA SHAPE.EXISTING_DATA.BUFFER,X

	;load new shape data
	
	;ACC = existing graphics data
	AND SHAPE.DRAW.HOPPER.AND_MASK,X
	ORA SHAPE.DRAW.HOPPER.ORA_MASK,X

	;write new shape data	
	STA (LINE.BASE.ADDR1),Y	;PLOT 
	
;next screen byte		
	INX	;next shape table counter
	INC SHAPE.DRAW.CURRENT.SBYTE ;next screen byte in current line			

;next line?
	LDA SHAPE.DRAW.CURRENT.SBYTE
	CMP SHAPE.DRAW.STOP.SBYTE ;have 4 screen bytes been plotted for the current hi-res line?
	BNE .DRAW.MODE.LOOP ;if no, continue plotting on the current line
	JMP .NEXT.LINE ;yes, then next line
	
.ERASE.MODE.LOOP
;screen byte0-3
	LDA SHAPE.EXISTING_DATA.BUFFER,X	;LOAD SHAPE BYTE

	LDY SHAPE.DRAW.CURRENT.SBYTE		;SCREEN BYTE INDEX IN Y REGISTER	
	STA (LINE.BASE.ADDR1),Y	;PLOT 

;next screen byte	
	INX	;next shape byte
	INC SHAPE.DRAW.CURRENT.SBYTE ;next screen byte in current line		

;next line?
	LDA SHAPE.DRAW.CURRENT.SBYTE
	CMP SHAPE.DRAW.STOP.SBYTE ;have 4 screen bytes been plotted for the current hi-res line?
	BNE .ERASE.MODE.LOOP ;if no, continue plotting on the current line	

	;**FALLS THROUGH**
	
.NEXT.LINE


;****TROUBLESHOOTING HOOK****
			; TXA
			; PHA
			;
			; LDA #$24
			; STA VARIABLE1.HTAB
			; LDA #$07
			; STA VARIABLE1.VTAB
			; LDA #$26
			; STA VARIABLE2.HTAB
			; LDA #$08
			; STA VARIABLE2.VTAB								
			;
			; ;LDX SHAPE.DRAW.LINE_ROW.NUMBER
			; LDA SHAPE.MOVE.INTRA_SBYTE.COUNTER
			; ;LDX SHAPE.DRAW.CURRENT.SBYTE
			; ;LDX #$AA
			; LDX SHAPE.PROJECTILE.ANIMATION_FRAME
			;
		; JSR MONITOR.VARIABLE
		; JSR KEYIN ;pause optional
			; PLA
			; TAX
			;
			; STA TEMP
			; CMP #$D1
			; BNE .TEMP
			; LDA #$AA
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP

			
			
	LDA SHAPE.DRAW.START.SBYTE 
	STA SHAPE.DRAW.CURRENT.SBYTE ;reset screen byte
	
	STX SHP.TBL.CNTR  	;X-REG was tracking the shape table counter in .DRAW.MODE.LOOP. Update SHP.TBL.CNTR because X-REG is needed for TILE.LINE.CNTR, to get the next line address.  
	INC TILE.LINE.CNTR	;next tile line
	LDX TILE.LINE.CNTR 	;restore tile line number to X-REG

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

	

;COMBAT.DRAW.ACTIVE_PLAYER
@START
	; LDA COMBAT.ACTIVE_PC.BLINK.COUNTER
	; BNE .EXIT
	; ;.calculate.active.player.screen.index	

	
		; LDX COMBAT.PC.ACTIVE.RECORD
		; LDA COMBAT.MAP_OBJECTS.PC+$0,X ;load active player GMAP.X
		; STA PARM.GMAP.X
		; LDA COMBAT.MAP_OBJECTS.PC+$1,X ;load active player GMAP.Y
		; STA PARM.GMAP.Y				
	; JSR CONVERT.COLUMN_ROW.TO.SINDEX
		; LDY RETURN.SCREEN_ARRAY_INDEX ;used below

		; ;YREG = SCREEN_ARRAY_INDEX
		; LDA COMBAT.MAP_OBJECTS.PC+$1,X ;load active player tile_type
		; STA SAVED_TILE_TYPE
		; LDA #$01
		; STA PAGE.FOREGROUND.OVERRIDE
	; JSR DRAW.TILE.SINGLE					;REPLACE MOB TILE WITH UNDERLYING MAP TILE

; .EXIT
	; RTS
	
@END

COMBAT.DRAW.ACTIVE_S_ENTITY.INDICATOR
@START
;PARAMETERS: ACC: S_ENTITY type, Y-REG: map object record index of S_ENTITY
;ENTRANCE: direct
;RETURN: hi-res output

;SAVE PARAMETERS
	;STA SAVED.ACC.LOCAL
	
			; pla
			; tax
			; pla
			; tay
			; lda #$aa
			; jsr prep.brk
			; brk
			
;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA
	
;GET ACTIVE PLAYER HI-RES LOCATION 
	LDX SCREEN.INDEX.TILE_LINE,Y
		
	LDA SCREEN.INDEX.TILE_SBYTE,Y
	TAY	

;------
		
		
		;Y-REG = screen byte (active player)
		;X-REG = line address (active player)
		LDA PAGE.FOREGROUND ;set page # parameter
	JSR GET.LINE.ADDRESS1

		;**OPT** Memory. Speed. If we stick with just one line on the bottom for the active player selector then 
		;this coule be made more efficient
		
	LDA #$7F
;	STA (LINE.BASE.ADDR1),Y
	INY
	;STA (LINE.BASE.ADDR1),Y
			
	TXA
	CLC
	ADC #$0F
	TAX

		LDA PAGE.FOREGROUND ;set page # parameter
	JSR GET.LINE.ADDRESS1
	LDA #$7F
	STA (LINE.BASE.ADDR1),Y
	DEY 
	STA (LINE.BASE.ADDR1),Y


;SAVE REGISTERS
	PLA
	TAY
	PLA
	TAX
	
	RTS
	
@END
	
DISPLAY.MOB.HP_STATUS_BAR
@START
;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA
	
;VALIDATE.ENTRANCE

		LDY COMBAT.DEFENDER.SINDEX
		
		;if PC defender, don't draw status bar
		;(it's redundant since the combat roster already has a status bar
		;and it would be difficult to implement because MOB MAX_HP is stored 
		;in a local array)
		LDA SCREEN.MO_SPRITE_TYPE.DATA,Y
		CMP #S_ENTITY_TYPE.PC
		BNE .VALIDATE.ENTRANCE.DONE
		JMP .EXIT
.VALIDATE.ENTRANCE.DONE
		
; ;PC/MOB TURN BRANCH
	; LDA COMBAT.TURN_STATUS   ;($00 = PCs turn. $01 = Special(s) S_ENTITY (example Wyvern Ally, Summoned Demon), $02 = MOBs turn)
	; BEQ .TURN_BRANCH.CHECK.DONE ;if PCs turn, then branch
	; JMP .EXIT 
; .TURN_BRANCH.CHECK.DONE
			
.INIT
	;set hi-res line to character 1 status bar
	LDA VTAB
	ASL ;X2
	ASL ;X4
	ASL ;X8 ;hi-res line number = VTAB * $8	
	;LDA #CHR_ROSTER.STATUS_BAR.START_LINE	
	STA CHR_ROSTER.STATUS_BAR.CURRENT_LINE		
	
	
	LDA #$08 ;set the size of the HP & MP status bars
	STA STATUS_BAR.MAX_SEGMENTS.PLUS_ONE

	


	;read defender character sheet

		LDA #$00 ;set read mode ;($00 = read | $01 = write)
		;YREG = COMBAT.DEFENDER.SINDEX
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet
		;RETURN: CHR_SHEET.RECORD.READ($80)
		
;CALCULATE CHR_SHEET_EXTENDED INDEX
	
		;read $8 bytes
		LDA #$08 ;(high bit not set = lookup by screen index)
		;Y-REG = COMBAT.DEFENDER.SINDEX 
	JSR READ.MAP_OBJECT.ARRAY
		;RETURN VALUE = MAP_OBJECT.RECORD.READ(8)

	;convert to MOB sequential # -1
	;(it will be -1 becuase MO_INDEX starts with $00, but that's the format we need it in anyway)
	LDA MAP_OBJECT.ARRAY.MO_INDEX
	LSR ;X2
	LSR ;X4
	LSR ;X8
	
	;calculate CHR_SHEET_EXTENDED INDEX
	ASL ;X2 (array record size is 2 byte)
	
	LDX SCREEN.MO_SPRITE_TYPE.DATA,Y	;load s_entity map object index
	CPX #S_ENTITY_TYPE.SPECIAL
	BNE .CALCULATE.CHR_SHEET.EXTENDED.INDEX.DONE	
	;redo conversion because specials true MO INDEX starts at $80.
	LDA MAP_OBJECT.ARRAY.MO_INDEX
	SEC
	SBC #$80
	;convert to MOB sequential # -1
	;(it will be -1 becuase MO_INDEX starts with $00, but that's the format we need it in anyway)
	LSR ;X2
	LSR ;X4
	LSR ;X8

	;calculate CHR_SHEET_EXTENDED INDEX
	ASL ;X2 (array record size is 2 byte)
	
	CLC
	ADC #$20 ;size of MOB section of CHR_SHEET_EXTENDED
.CALCULATE.CHR_SHEET.EXTENDED.INDEX.DONE	
	TAY ;transfer to Y-REG for use as CHR_SHEET_EXTENDED index

	
	; LDX #CHR_SHEET_EXTENDED.MOB.HP_MAX
	; LDY /CHR_SHEET_EXTENDED.MOB.HP_MAX
	; JSR PREP.BRK
	; BRK
	
	
	;**FALLS THROUGH**

.DRAW.HP_MP.STATUS_BAR
@START


	;calculate HP status bar segments
		LDA CHR_SHEET.PC_MOB.HP_LO
		STA PERCENT.GET.PARM.LOW_NUMBER+$0	
		LDA CHR_SHEET.PC_MOB.HP_HO
		STA PERCENT.GET.PARM.LOW_NUMBER+$1	
		LDA CHR_SHEET_EXTENDED.MOB.HP_MAX,Y ;load LO byte
		;LDA #$08
		STA PERCENT.GET.PARM.HIGH_NUMBER+$0	
		INY ;advance to HO byte field in CHR_SHEET_EXTENDED.MOB.HP_MAX
		LDA CHR_SHEET_EXTENDED.MOB.HP_MAX,Y ;load HO byte
		;LDA #$01
		STA PERCENT.GET.PARM.HIGH_NUMBER+$1

			; lda #$aa
			; LDx PERCENT.GET.PARM.HIGH_NUMBER+$0
			; LDy PERCENT.GET.PARM.HIGH_NUMBER+$1
			; jsr prep.brk
			; brk
			
	JSR .GET.STATUS_BAR.SEGEMENTS
		;RETURN ACC = # of segements to draw				
		STA STATUS_BAR.SEGMENTS ;parm of STATUS_BAR.DRAW
			
.EXECUTE.DRAW
@START
	
		;set STATUS_BAR.DRAW parameters

		LDA #$55
		STA STATUS_BAR.BYTE0_VALUE	
			
		LDA #$2A
		STA STATUS_BAR.BYTE1_VALUE

		;STATUS_BAR.SEGMENTS = set above
	

;DRAW HP
			
;DRAW HP: 1ST LINE WIDTH	
		LDX CHR_ROSTER.STATUS_BAR.CURRENT_LINE		
		INX
		INX
		INX
		INX
		
		;draw center wedge piece
		;(the white dot that seperates the hp and mp status bars)
		;(this draw is for the MP STATUS bar portion of the wedge piece because the MP status bar isn't drawn 
		;in this routine, which is just for the scroll window)	
		LDY #CHR_ROSTER.MP_STATUS_BAR.START_SBYTE	
			LDA #$01
		JSR DRAW.BYTE
		
		LDY #CHR_ROSTER.HP_STATUS_BAR.START_SBYTE		
		LDA #$00 ;set direction code ($00 = increases to left | >=$01 increases to right)
	JSR STATUS_BAR.DRAW

		;DRAW CENTER WEDGE PIECE
		;(the white dot that seperates the HP and MP status bars)
		;only draw if no HP status bar segements
		;because normally one of the bits from the HP status bar joins with the
		;bit drawn above to form the white center wedge (a 2 pixel long white line)
		LDA STATUS_BAR.SEGMENTS
		BNE .DRAW.CENTER.DOT.DONE1.1
			LDY #CHR_ROSTER.HP_STATUS_BAR.START_SBYTE
			LDA #$40
		JSR DRAW.BYTE
.DRAW.CENTER.DOT.DONE1.1

;DRAW HP: 2ND LINE WIDTH	
		DEX
		
		;draw center wedge piece
		;(the white dot that seperates the hp and mp status bars)		
			LDY #CHR_ROSTER.MP_STATUS_BAR.START_SBYTE	
			LDA #$01
		JSR DRAW.BYTE
		
		LDY #CHR_ROSTER.HP_STATUS_BAR.START_SBYTE			
		LDA #$00 ;set direction code ($00 = increases to left | >=$01 increases to right)
	JSR STATUS_BAR.DRAW
	
		;DRAW CENTER WEDGE PIECE
		;(the white dot that seperates the HP and MP status bars)
		;(only draw if no HP status bar segements
		;because normally one of the bits from the HP status bar joins with the
		;bit drawn above to form the white center wedge (a 2 pixel long white line))
		LDA STATUS_BAR.SEGMENTS
		BNE .DRAW.CENTER.DOT.DONE1.2
			LDY #CHR_ROSTER.HP_STATUS_BAR.START_SBYTE
			LDA #$40
		JSR DRAW.BYTE
.DRAW.CENTER.DOT.DONE1.2

.DRAW.HP.STATUS_BAR.DONE
	

.DRAW.HP_MP.STATUS_BAR.DONE
@END
@END

.CLEAN_UP
;**OPT** Memory. I'm not sure why this read attacker is needed but
;I had some problems without it. But, those problems may have been
;related to something else. As far as I can tell this routine is called at the end of the 
;attack sequence so it shouldn't matter what character sheet is in memory when this routine 
;completes. Worth trying to remove it and seeing what happens (watch in particular for breaks and combat scroll text problems)

	;read attacker character sheet
		LDY COMBAT.ATTACKER.SINDEX
		LDA #$00 ;set read mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet
		;RETURN: CHR_SHEET.RECORD.READ($80)
	
	;print "Health" label after H.P. status bar
		LDA #$21
		STA HTAB
		;VTAB: keep current value
	JSR UPDATE.CHAR.POS
	
					; LDA #$AA
					; LDX HTAB
					; LDY VTAB
					; JSR PREP.BRK
					; BRK
					
		LDA #COMBAT.TEXT_BLOCK.STATUS_BAR_LABEL					
		STA TWF.STRING+$0
		
		LDA /COMBAT.TEXT_BLOCK.STATUS_BAR_LABEL
		STA TWF.STRING+$1	
	JSR PRINT.TEXT.WINDOW
			;**NOTE: Only 1 byte of SCREEN_BUILDER savings to replace the above, and it won't even
			;work because SCREEN_BUILDER requires HTAB and VTAB to be set and the above keeps the 
			;VTAB value. 

	
.EXIT



;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX
	
	
	RTS

.GET.STATUS_BAR.SEGEMENTS
@START
;PARAMETERS: PERCENT.GET.PARM.LOW_NUMBER(2), PERCENT.GET.PARM.LOW_NUMBER(2)
;ENTRANCE: direct
;RETURN: # of segements to draw


		;PERCENT.GET.PARM.LOW_NUMBER(2) = set as parm to this routine
		;PERCENT.GET.PARM.HIGH_NUMBER(2) = set as parm to this routine
	JSR PERCENT.GET
		;RETURN: BCD(2) = 16-percentage (!0-!100)

					

					
	LDA BCD+$1		
	BNE .SET.7_SEGMENTS
	
	SED ;set decimal mode	
	LDA BCD+$0
	CMP #$86
	BCS .SET.7_SEGMENTS
	CMP #$72
	BCS .SET.6_SEGMENTS
	CMP #$58
	BCS .SET.5_SEGMENTS
	CMP #$45
	BCS .SET.4_SEGMENTS
	CMP #$30
	BCS .SET.3_SEGMENTS
	CMP #$15
	BCS .SET.2_SEGMENTS
	CMP #$01
	BCS .SET.1_SEGMENT
	
	;DEFAULT CASE: if none of the above are true, then BCD(2) = $00

	;**FALLS THROUGH**

.SET.0_SEGMENTS
	LDA #$00
	JMP .RETURN.SEGEMENTS
	
.SET.1_SEGMENT
	LDA #$01
	JMP .RETURN.SEGEMENTS

.SET.2_SEGMENTS
	LDA #$02
	JMP .RETURN.SEGEMENTS

.SET.3_SEGMENTS
	LDA #$03
	JMP .RETURN.SEGEMENTS

.SET.4_SEGMENTS
	LDA #$04
	JMP .RETURN.SEGEMENTS

.SET.5_SEGMENTS
	LDA #$05
	JMP .RETURN.SEGEMENTS

.SET.6_SEGMENTS
	LDA #$06
	JMP .RETURN.SEGEMENTS

.SET.7_SEGMENTS
	LDA #$07
	
	;**FALLS THROUGH**

	
.RETURN.SEGEMENTS
	CLD ;clear decimal mode
	
	;RETURN VALUE = ACC
			
			
	RTS
	
@END

	
@END

COMBAT.DRAW.SINGLE.OFF_GRID ;off tile grid (animated, single-tile, foreground)
@START
;PARAMETERS: ACC: draw mode ($00 = not animated | $01 = animated); Y-REG (screen tile location of target), SHAPE.BACKGROUND.AND_MASK(2)
;ENTRANCE: direct
;RETURN: Hi-res shape

;SAVE PARAMETERS

			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; jsr keyin
			; lda #$bc
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP
			
	;ACC = draw mode parameter
	STA COMBAT.DRAW.SINGLE.OFF_GRID.ANIMATION_FLAG ;($00 = not animated | $01 = animated)

	
;SAVE REGISTERS
	TXA
	PHA
	TYA
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


			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; LDA #$AA
			; LDX COMBAT.DRAW.SINGLE.OFF_GRID.WAIT_VALUE ;Parameter value for WAIT.LOOP between drawing the shape and screen animation update.	
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP
			
		LDA COMBAT.DRAW.SINGLE.OFF_GRID.WAIT_VALUE ;Parameter value for WAIT.LOOP between drawing the shape and screen animation update.	
	JSR WAIT.LOOP


	;is shape animated? 
	;(if not animated shape, skip the screen animation update or problems can be caused in calling routines in which the hi-res page status is critical. 
	;if animated shape, do a screen animation update because drawing all 4 frames of the animated shape without doing a screen animation update would look out of sync)
	LDA COMBAT.DRAW.SINGLE.OFF_GRID.ANIMATION_FLAG ;($00 = not animated | $01 = animated)
	BEQ .ANIMATION.UPDATE.COMPLETE ;if no, branch


			
	;draw active player selector
		LDY COMBAT.PC.ACTIVE.SINDEX ;load screen tile location of active player
	JSR COMBAT.DRAW.ACTIVE_S_ENTITY.INDICATOR
		

;UPDATE SCREEN ANIMATION (serves as additional delay)		
			
	;!!!!! BSR:BANK1 !!!!
	JSR ANIMATION.UPDATE.ENTRANCE
	
.ANIMATION.UPDATE.COMPLETE
		
;INCREMENT SHAPE TABLE
	LDA SHAPE.BACKGROUND.AND_MASK+$0
	CLC
	ADC #$40
	STA SHAPE.BACKGROUND.AND_MASK+$0
	LDA SHAPE.BACKGROUND.AND_MASK+$1
	ADC #$00 ;16-bit add
	STA SHAPE.BACKGROUND.AND_MASK+$1

	LDA $C000
    BMI .DRAW.SINGLE.OFF_GRID.EXIT ;if key has been pressed since last check, then exit loop

	;is shape animated? 
	LDA COMBAT.DRAW.SINGLE.OFF_GRID.ANIMATION_FLAG ;($00 = not animated | $01 = animated)
	BEQ .DRAW.SINGLE.OFF_GRID.EXIT

			
;INCREMENT MULTI-TILE #
	INX ;next animation frame
	CPX #$04
	BNE .DRAW.SINGLE.LOOP


.DRAW.SINGLE.OFF_GRID.EXIT
;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX

	
	RTS
@END

	
@END

;COMBAT STATISTICS ROUTINES2
@START
	.IN 	swap.routines.combat.stats_routines.asm

@END

;MEMORY MGT SUBROUTINES
@START
SWAP2.MAIN_MEMORY.OUT
@START
;note: swaps out the main memory swap area into aux swap space #2

			; LDA $C012		;ROM/BSR soft-switch flag (bit7 = 1: BSR, bit7=0 ROM)
			; sta $be00
			; LDA $C011		;BANK1/BANK2 soft-switch flag (bit7 = 1: BANK2, bit7=0 BANK1)
			; sta $be01
			; lda #$dd
			; jsr prep.brk
			; brk



			
			
		; LDA $C083 ;write enable BSR
		; LDA $C083
	;------------------


	
		TSX			;transfer stack pointer to X-REG
		STX TEMP	;save stack pointer
	STA $C009 ;enable aux zero-page & aux BSR 

	;execute copy
		LDA #SRTN.COMBAT.ADDRESS
		STA COPY.FROM_START
		LDA /SRTN.COMBAT.ADDRESS
		STA COPY.FROM_START+$1

		;**OPT** Disk speed. If inventory doesn't need to use memory above COMBAT.MAIN.MODULE.END we don't need to swap in/out the persistent memory range which exists above that. i.e change the LDAs below to use COMBAT.PERSISTANT.MEMORY 		
		LDA #SRTN.COMBAT.ADDRESS.END
		;LDA #$FF
		STA COPY.FROM_END
		LDA /SRTN.COMBAT.ADDRESS.END
		;LDA #$B8
		STA COPY.FROM_END+$1
			
		LDA #SWAP_SPACE2.AUX_MEMORY
		;LDA #$00
		STA COPY.TO
		LDA /SWAP_SPACE2.AUX_MEMORY
		;LDA #$D0
		STA COPY.TO+$1
	
	JSR MEMORY.COPY	

	
	STA $C008 ;enable main zero-page & main BSR 
		LDX TEMP	;restore stack pointer to X-REG
		TXS ;transfer X-REG to stack pointer

	RTS

@END

;SWAP2.MAIN_MEMORY.IN
;moved to main memory, otherwise the copy will clobber
;this memory location. 



			

			
			
	;***this routine must be executed after SWAP2.MAIN_MEMORY.IN, not before, otherwise it will clobber the main COMBAT module code while it is swapped out to aux_bsr memory. 
LOAD.WEAPONS.SHAPE_TABLES ;restore. They were clobbered by SWAP2.MAIN_MEMORY.OUT 
@START				
; ;=====LOAD SPELL FILE======
;filename = "SRTN.SPELL_FILE"
	
;------OPEN FILE------
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
	LDA #$01  	;$01
	sta parm.sizelo	
	LDA #$00	;$00
	sta parm.sizehi
	
;set destination memory address
	lda #SHAPE.HOPPER0
	sta parm.ldrlo
	lda /SHAPE.HOPPER0
	sta parm.ldrhi

;set filename to read from	
	lda #SRTN.SPELL_FILE	;load LO address
	sta parm.namlo
	lda /SRTN.SPELL_FILE	;load HO address
	sta parm.namhi
					
		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO

;------SEEK FILE------
;(seek to the start of the weapon shape tables)

.SEEK.FILE
	lda #cmd_seek.current_drive
	sta parm.reqcmd

;set seek length (bytes)
	lda #WEAPON.SHAPE_TABLES.SEEK_BYTES	;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda /WEAPON.SHAPE_TABLES.SEEK_BYTES	;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi
			
		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO	
	
	;**FALLS THROUGH**
	
					
.INIT.LOOP.VARIABLES

	;init aux zero page variables 
	;(note: for the memory copy. when aux BSR is enabled, aux zero page is also enabled)
	STA $C009 ;enable aux zero-page & aux BSR 

	LDA #SPELL.FILE.WEAPONS.SHAPE_TABLE.AUX.START
	STA COPY.TO
	LDA /SPELL.FILE.WEAPONS.SHAPE_TABLE.AUX.START
	STA COPY.TO+$1

	STA $C008 ;enable main zero-page & main BSR 
	
	LDA /WEAPON.SHAPE_TABLES.READ_BYTES	
	TAY ;set loop counter (only the HO byte is needed for the counter because the weapons shape tables always end on a page boundry, they are $400bytes each)
	

	

;----debug: hang doesn't occur here

			; lda #$aa
			; jsr prep.brk
			; brk	
			
.RESTORE.LOOP

.READ.CHUNK
;----READ FILE-----
;(read file in $100 byte chunks until the remaining bytes to 
;read are < $100)
	
	lda #cmd_read.current_drive
	sta parm.reqcmd

;set destination memory address
	lda #SHAPE.HOPPER0
	;LDA #$00
	sta parm.ldrlo
	lda /SHAPE.HOPPER0
	;LDA #$02  ;$06 WORKS
	sta parm.ldrhi
	
;set read length (bytes)
	lda #$00				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda #$01		;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi

		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO
	
.COPY.CHUNK.TO.AUX_BSR	
	;enable AUX memory (BSR: bank2 is already enabled)						
		TSX			;transfer stack pointer to X-REG
		STX TEMP	;save stack pointer
		;STX SAVED.STACK.POINTER
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
		LDX TEMP	;restore stack pointer to X-REG
		;LDX SAVED.STACK.POINTER
		TXS ;transfer X-REG to stack pointer

	DEY
	BNE .RESTORE.LOOP


.RESTORE.DONE


;---debug: hang occurs her

			
	RTS
	
@END

@END

;SOUND SUBROUTINES2
@START
COMBAT.AUDIO_VISUAL.HIT
@START
;PARAMETERS: Y-REG = screen index of tile to draw hit shape

		;jsr keyin
		
;SAVE REGISTERS
	TXA
	PHA
	
;TROUBLESHOOTING NOTE: hit shape doesn't display if player
;holds down on a movement key because WAIT.LOOP has a keypress abort.
		;connect shape table pointer
		LDA #ATTACK_HIT.SHAPE.SHAPE_0.AND_MASK
		STA SHAPE.BACKGROUND.AND_MASK+$0

		LDA /ATTACK_HIT.SHAPE.SHAPE_0.AND_MASK
		STA SHAPE.BACKGROUND.AND_MASK+$1

		;LDA #$60
		LDA #$FF
		STA COMBAT.DRAW.SINGLE.OFF_GRID.WAIT_VALUE ;Parameter value for WAIT.LOOP between drawing the shape and screen animation update.	

		LDA #$00 ;set draw mode: not animated. ($00 = not animated | $01 = animated)		
	JSR COMBAT.DRAW.SINGLE.OFF_GRID
		

;ERASE HIT SHAPE

.GET.S_ENTITY.TILE_ID
	;was S_ENTITY killed?
	;(if S_ENTITY was killed then skip the map object tile lookup because the map object record was already deleted
	;in .TARGET.IS.KILLED (COMBAT.DAMAGE.ROLL). And, that routine also erases the S_ENTITY from the screen so
	;the erase-draw below can be skipped too).
	
	LDA #$00
	STA TEMP16+$0
	
	LDA COMBAT.STATS.HIT_MISS_KILL.FLAG ;($00 = hit | $01 = miss | $02 = kill)
	CMP #$02
	BEQ .DRAW.S_ENTITY ;branch if kill flag set to succeeded

		LDA #$08 ;read $8 bytes    ;**OPT** Memory. I think this routine could just default to an 8byte read and note that the last 4 bytes are garbage for S_ENTITIES with 4 byte records. The code that uses the return data already knows that. 
		;Y-REG (screen array index)	
	JSR READ.MAP_OBJECT.ARRAY
		;RETURN VALUE = MAP_OBJECT.RECORD.READ(8)
		LDA MAP_OBJECT.RECORD.READ+$2
		STA SAVED_TILE_TYPE
		STA TEMP16+$0
.GET.S_ENTITY.TILE_ID.DONE

.DRAW.S_ENTITY
		
		;Y-REG: screen tile location of the target S_ENTITY
		LDA #$01 ;set foreground & shape table override
		STA PAGE.FOREGROUND.OVERRIDE

		;set override if spell cast is mass death
		;(this is so that the black can be inserted into the shape table. otherwise
		;the terrain tile will be drawn which doesn't match the black cone on the screen at the time)		
		LDA IS.SPELL.MASS_DEATH.FLAG	;($00 = no, >$01 = yes)
		BEQ .SET.OVERRIDE.FLAGS.DONE
		LDA #$01
		STA DRAW.TILE.SHAPE_TABLE.OVERRIDE ;($00 = load data into shape table from aux memory | $01 don't load data into shape table)
		LDX #$00
.LOOP.OVERRIDE.SHAPE_TABLE
		LDA #$00
		STA SHAPE.HOPPER0,X
		INX
		CPX #$20
		BNE .LOOP.OVERRIDE.SHAPE_TABLE
		;**FALLS THROUGH**
.SET.OVERRIDE.FLAGS.DONE
	JSR DRAW.TILE.SINGLE
		;Y-REG: screen tile location of the target S_ENTITY		
		LDA TEMP16+$0
		STA SAVED_TILE_TYPE
		
		;set override if spell cast is mass death
		;(this is so that the black can be inserted into the shape table. otherwise
		;the terrain tile will be drawn which doesn't match the black cone on the screen at the time)		
		LDA IS.SPELL.MASS_DEATH.FLAG	;($00 = no, >$01 = yes)
		BEQ .SET.OVERRIDE.FLAGS.DONE2
		LDA #$01
		STA DRAW.TILE.SHAPE_TABLE.OVERRIDE ;($00 = load data into shape table from aux memory | $01 don't load data into shape table)
		;shape table data already set in .LOOP.OVERRIDE.SHAPE_TABLE
		
		;**FALLS THROUGH**
.SET.OVERRIDE.FLAGS.DONE2		
		
		;background page set by default
	JSR DRAW.TILE.SINGLE
.DRAW.S_ENTITY.DONE
	
;PLAY HIT SOUND
	JSR PLAY.COMBAT.ATTACK_HIT.SOUND

.EXIT
;RESTORE REGISTERS
	PLA
	TAX

		
	RTS
	
@END

PLAY.COMBAT.ATTACK_HIT.SOUND
@START
		;SET PARAMETERS
		
		;connect sound_data pointer
		LDA #SOUND_DATA.COMBAT.ATTACK_HIT
		STA SOUND_DATA.POINTER+$0
		LDA /SOUND_DATA.COMBAT.ATTACK_HIT
		STA SOUND_DATA.POINTER+$1
		
		LDA #$1A
		STA SOUND_DATA.LENGTH
	JSR PLAY.SOUND
	
	 RTS
	
;HEX TABLES	
SOUND_DATA.COMBAT.ATTACK_HIT .HS 06.02.20.02.28.02.3C.02.50.02.64.02.02.02.8C.02.A0.02.B4.02.C8.02.DC.02.F0.02


@END

PLAY.COMBAT.ATTACK_MISS.SOUND	;**temp**
@START
;SAVE REGISTERS
	TXA
	PHA
	

; ;PRINT CHARACTER TO SCREEN THAT DENOTES START OF PROGRAM 
	; LDA $CD
	; JSR $FDED


;PLAY SOUND
    LDX #coun1a
.decrement
    ;PLAY SOUND
    LDA #tone1a               ;SET FREQUENCY   
    STA HALFTIME           
    LDA    #dur2a                ;SET DURATION
    STA    LENGTH               
    JSR SOUND.MAKE            ;MAKE THE SOUND AT THE SPECIFIE LENGTH AND FREQUENCY
   
;PLAY SOUND
    LDA #tone2a                ;SET FREQUENCY   
    STA HALFTIME           
    LDA    #dur1a                ;SET DURATION
    STA    LENGTH               
    JSR SOUND.MAKE            ;MAKE THE SOUND AT THE SPECIFIE LENGTH AND FREQUENCY
  DEX
  CPX #$03
  BNE .decrement


;RESTORE REGISTERS
	PLA
	TAX
	
	RTS
	

tone1a .EQ $60
tone2a .EQ $4a

dur1a .EQ $1
dur2a .EQ $2	

coun1a .EQ $10
@END

@END

;PRINT TEXT SUBROUTINES
@START

COMBAT.PRINT.MATH ;**OPT** Memory. Optimize with SCREEN_BUILDER interpreter
@START
;PARAMETERS: COMBAT.ATTACKER.SINDEX, COMBAT.TARGET.SINDEX
;ENTRANCE: direct
;RETURN: none

;COMBAT TEXT WINDOW NOTES
;(refers to the entire right side window, inclusive of the area dedicated for the charactere roster and the scroll window area)
;fixed text window dimension notes
;first printable row = $1	 (!22 printable rows)	
;last printable row = $16
;first printable column = $19
;last printable column = $26  (!13 printable characters total)
;

;ORIGINAL PLANNING NOTES
@START
;area for player names, HP, MP
;	row $1 - $D
;	allows for two text lines per player *6
;			player name is one one line and HP/MP is on the other line
;				*the hp/mp could be numerical or a violet horizontal line for health and blue line for magic
;   no seperator between player's text block.
;	seperator between last player line and the start of the scrolling text window
;
;tenative scrolling text window (streaming combat/command outpout, maybe stats calcs)
;		row $0E - $16. (total available rows = $9
;
;Player <X> attacks <MOB NAME> | <MOB NAME> attacks player 1   (make sure to consider player on player, mob on mob and special attacks)
;to-hit%
;to-hit roll
;base damage roll
;critical hit damage
;armor roll (ideally for mobs it displays the "resist" stat associated with the type of attack that occured)
;
;final damage
;flags: range interference, dodge, critical hit, kill (death magic)
@END

		
.VALIDATE.ENTRANCE

	LDA GAME.COMBAT_MATH_DISPLAY.MODE	;($00 = OFF | $01 >= ON)
	BNE .SAVE.REGISTERS
	JMP .EXIT.ALT
		
		
.SAVE.REGISTERS
	TXA
	PHA
	TYA
	PHA

		
		;JSR KEYIN
.INIT
	LDA #$01
	STA PRINT.STR.MODE	;($00 = normal | >=$01 = wait/pause functionality enabled)

	;ensure there is a space between the hit/miss/spell outcome text and the combat math text
	JSR PRINT.TEXT.WINDOW.CR ;print carriage return to combat scroll window
		;**OPT** Memory. $4bytes can be saved by replacing this, and the other PRINT.TEXT.WINDOW.CR in this
		;subroutine, by adding a print_routine_code to SCREEN_BUILDER for PRINT.TEXT.WINDOW.CR. This
		;would cost some general game engine memory but free up some combat module memory, which may or 
		;may not be helpful.
		;To add this support, the branch for PRINT.TEXT.WINDOW.CR would be added in the same section as
		;the ones for updating the screen position. After PRINT.TEXT.WINDOW.CR is executed, exit SCREEN_BUILDER,
		;don't execute the .PROCESS.PRC section which has the indirect jump table since the purpose of that is 
		;to execute a print routine. 
			
.PRINT.TO_HIT.PERCENT
@START
;VALIDATE ENTRANCE
;(if damage type = spell then skip the TO_HIT stats because spells always hit)

	LDA COMBAT.STATS.DAMAGE.TYPE ;($00 = melee | $01 range, $02 spell. High-bit not set = non-magic weapon (doesn't apply to type=spell or MOBs) | high-bit set = magic weapon (doesn't apply to type=spell or MOBs)
	AND #$7F ;mask-out high-bit
	CMP #$02 ;is damage type spell?
	BEQ .PRINT.BASE_DAMAGE.ROLL ;branch if yes
		

;###EXECUTE SCREEN BUILDER: COMBAT.SB_COMMAND_TABLE1
@START

		;special parms (lookup table indexes that need custom bit manipulation)
		;<NONE>
		
		;standard parms
		LDA #COMBAT.SB_COMMAND_TABLE1
		STA SCREEN_BUILDER.COMMAND_TABLE.POINTER+$0
		LDA /COMBAT.SB_COMMAND_TABLE1
		STA SCREEN_BUILDER.COMMAND_TABLE.POINTER+$1		
	JSR SCREEN_BUILDER
	
@END

	
@END






.PRINT.TO_HIT.ROLL
@START

;HANDLED IN COMBAT.SB_COMMAND_TABLE1, executed in .PRINT.TO_HIT.PERCENT
	
@END

.PRINT.BASE_DAMAGE.ROLL ;DAMAGE X2
@START
;VALIDATE ENTRANCE
;(if flag is set to miss then skip damage and armor related items since no damage was done)

	LDA COMBAT.STATS.HIT_MISS_KILL.FLAG ;($00 = hit | $01 = miss | $02 = kill)
	CMP #$01
	BNE .FLAG.CHECK.COMPLETE ;branch if miss
	JMP .PRINT.FLAGS.ROLL
.FLAG.CHECK.COMPLETE		

	
;###EXECUTE SCREEN BUILDER: COMBAT.SB_COMMAND_TABLE2 ;toggle text window <CR> parm: ON
@START

		;special parms (lookup table indexes that need custom bit manipulation)
		;<NONE>
		
		;standard parms
		LDA #COMBAT.SB_COMMAND_TABLE2
		STA SCREEN_BUILDER.COMMAND_TABLE.POINTER+$0 
		LDA /COMBAT.SB_COMMAND_TABLE2
		STA SCREEN_BUILDER.COMMAND_TABLE.POINTER+$1		
	JSR SCREEN_BUILDER
	
@END



@END

.PRINT.CRTL_HIT_DAMAGE
@START

	LDA COMBAT.STATS.CRTL_HIT_FLAG ;($00 = critical hit succeeded, $01 = critical hit not attempted, $03 = critical hit blocked by defender)
	BNE .PRINT.CRTL_HIT_DAMAGE.DONE
			

;###EXECUTE SCREEN BUILDER: COMBAT.SB_COMMAND_TABLE3
@START

		;special parms (lookup table indexes that need custom bit manipulation)
		;<NONE>
		
		;standard parms
		LDA #COMBAT.SB_COMMAND_TABLE3
		STA SCREEN_BUILDER.COMMAND_TABLE.POINTER+$0
		LDA /COMBAT.SB_COMMAND_TABLE3
		STA SCREEN_BUILDER.COMMAND_TABLE.POINTER+$1		
	JSR SCREEN_BUILDER
	
@END


.PRINT.CRTL_HIT_DAMAGE.DONE	
@END

.PRINT.REGULAR.ARMOR.ROLL
@START

;###EXECUTE SCREEN BUILDER: COMBAT.SB_COMMAND_TABLE4
@START

		;special parms (lookup table indexes that need custom bit manipulation)
		;<NONE>
		
		;standard parms
		LDA #COMBAT.SB_COMMAND_TABLE4
		STA SCREEN_BUILDER.COMMAND_TABLE.POINTER+$0
		LDA /COMBAT.SB_COMMAND_TABLE4
		STA SCREEN_BUILDER.COMMAND_TABLE.POINTER+$1		
	JSR SCREEN_BUILDER
	
@END

@END

.PRINT.MAGIC_DEFENSE.ROLL
@START
;VALIDATE ENTRANCE
;(don't print if value = $00)
	LDA COMBAT.STATS.RESIST_MAGIC.DEFENSE_RATING ;(ho byte ignored)
	BEQ .PRINT.MAGIC_DEFENSE.ROLL.DONE

	

;###EXECUTE SCREEN BUILDER: COMBAT.SB_COMMAND_TABLE5
@START

		;special parms (lookup table indexes that need custom bit manipulation)
		;<NONE>
		
		;standard parms
		LDA #COMBAT.SB_COMMAND_TABLE5
		STA SCREEN_BUILDER.COMMAND_TABLE.POINTER+$0
		LDA /COMBAT.SB_COMMAND_TABLE5
		STA SCREEN_BUILDER.COMMAND_TABLE.POINTER+$1		
	JSR SCREEN_BUILDER
	
@END

.PRINT.MAGIC_DEFENSE.ROLL.DONE


@END


.PRINT.DAMAGE_FINAL.ROLL
@START


;###EXECUTE SCREEN BUILDER: COMBAT.SB_COMMAND_TABLE6
@START

		;special parms (lookup table indexes that need custom bit manipulation)
		;<NONE>
		
		;standard parms
		LDA #COMBAT.SB_COMMAND_TABLE6
		STA SCREEN_BUILDER.COMMAND_TABLE.POINTER+$0
		LDA /COMBAT.SB_COMMAND_TABLE6
		STA SCREEN_BUILDER.COMMAND_TABLE.POINTER+$1		
	JSR SCREEN_BUILDER
	
@END	

@END

.PRINT.HP.TARGET.FINAL
@START
;print target HP (BCD)		

	;is target dead?
	LDA COMBAT.STATS.HIT_MISS_KILL.FLAG ;($00 = hit | $01 = miss | $02 = kill)
	CMP #$02 
	BEQ .TARGET.DEAD
	;**FALLS THROUGH**
.TARGET.ALIVE	
	;read defender (target) character sheet 
		LDY COMBAT.DEFENDER.SINDEX ;load defender (target) sindex
		LDA #$00 ;set read mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet
		;RETURN: CHR_SHEET.RECORD.READ($80)

		
;**Note: SCREEN_BUILDER wouldn't result in any savings for the code block below.
;SB has two set a pointer, do a JSR and also costs a byte for the table size + 3 bytes to print something		
		LDA CHR_SHEET.PC_MOB.HP_LO ;16-bit hex # to convert (LO byte)
		STA BIN+$0
		LDA CHR_SHEET.PC_MOB.HP_HO ;16-bit hex # to convert (HO byte)
		STA BIN+$1
		SEC ;(SEC = print CR to text window | CLC don't print CR)
	JSR PRINT.HEX16.RJ ;leading zeros suppressed. right justified		
		

	;**FALLS THROUGH**	
.TARGET.DEAD
	
	;**FALLS THROUGH**
	
@END
.PRINT.HP.TARGET.FINAL.COMPLETE


.PRINT.FLAGS.ROLL ;**OPT** Memory. Instead of printing single characters, maybe just use an underlying pring hex to ascii routine
@START

;###EXECUTE SCREEN BUILDER: COMBAT.SB_COMMAND_TABLE7
@START

		;special parms (lookup table indexes that need custom bit manipulation)
		;<NONE>
		
		;standard parms
		LDA #COMBAT.SB_COMMAND_TABLE7
		STA SCREEN_BUILDER.COMMAND_TABLE.POINTER+$0
		LDA /COMBAT.SB_COMMAND_TABLE7
		STA SCREEN_BUILDER.COMMAND_TABLE.POINTER+$1		
	JSR SCREEN_BUILDER
	
@END


@END

	;**FALLS THROUGH**

.COMMON.ROUTINE
	;ensure there is a blank line at the end of the combat math display
	JSR PRINT.TEXT.WINDOW.CR

	LDA #$00 ;reset parameters
	STA PRINT.STR.MODE	;($00 = normal | >=$01 = wait/pause functionality enabled)		
			
			;JSR KEYIN
			
	;**FALLS THROUGH**

.EXIT
;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX

.EXIT.ALT
	

				
	RTS
@END


COMBAT.PRINT.HIT.OTHER ;prints a variety of text blocks depending on the value of COMBAT.STATS.HIT_MISS_KILL.FLAG ;($00 = hit | $01 = miss | $02 = kill)
@START
;PARAMETERS: ACC ;($00 = killed text block, $01 = critial_hit text block, $02 = critial_hit_foiled text block)
;ENTRANCE: COMBAT module routines
;RETURN: text block printed to combat scroll window


	;ACC = parm: text block code
	ASL ;X2
	TAX
	
		LDA .OTHER.HIT.TEXT_BLOCK.LOOKUP_TABLE+$0,X
		STA TWF.STRING+$0
		LDA .OTHER.HIT.TEXT_BLOCK.LOOKUP_TABLE+$1,X
		STA TWF.STRING+$1
	JSR PRINT.TEXT.WINDOW
	
	RTS
	
	

.OTHER.HIT.TEXT_BLOCK.LOOKUP_TABLE
	;.BS $2
	.DA #COMBAT.TEXT_BLOCK.HIT
	.DA /COMBAT.TEXT_BLOCK.HIT
	
	.DA #COMBAT.TEXT_BLOCK.KILLED
	.DA /COMBAT.TEXT_BLOCK.KILLED
	
	.DA #COMBAT.TEXT_BLOCK.CRITICAL_HIT
	.DA /COMBAT.TEXT_BLOCK.CRITICAL_HIT
	
	.DA #COMBAT.TEXT_BLOCK.CRITICAL_HIT.FOILED
	.DA /COMBAT.TEXT_BLOCK.CRITICAL_HIT.FOILED	
	
	

@END



;COMBAT.PRINT.HIT
@START
; ;PARAMETERS: CHR_SHEET.RECORD.READ (8)
; ;ENTRANCE: direct
; ;RETURN: printed text


		; LDA #COMBAT.TEXT_BLOCK.HIT					
		; STA TWF.STRING+$0
		
		; LDA /COMBAT.TEXT_BLOCK.HIT
		; STA TWF.STRING+$1	
	; JSR PRINT.TEXT.WINDOW
	
	; RTS
	
@END

COMBAT.PRINT.DEFENDER.NAME
@START
;PARAMETERS: COMBAT.DEFENDER.SINDEX, carry flag (SEC = override defender SINDEX | CLC = use defender SINDEX)
;ENTRANCE: direct
;RETURN: printed text

	;parm = carry flag (SEC = override defender SINDEX | CLC = use defender SINDEX)
	BCS .ALTERNATE_ENTRANCE
	
	;read character sheet for active player
		LDY COMBAT.DEFENDER.SINDEX
.ALTERNATE_ENTRANCE
		LDA #$00 ;set read mode ;($00 = read | $01 = write)			
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet for COMBAT.ACTIVE_PLAYER.SINDEX

	;JSR PRINT.TEXT.WINDOW.CR ;print carriage return to combat scroll window
		
	;SELECT CASE: defender S_ENTITY type	
	LDA SCREEN.MO_SPRITE_TYPE.DATA,Y
	CMP #S_ENTITY_TYPE.PC
	BEQ .PRINT.PC.NAME
	
	;print defender name (MOB/SPECIAL)		
	JSR COMBAT.PRINT.MOB_NAME					
	JMP .UPDATE.SCROLL.WINDOW.S_ENTITY_NAME.DONE

.PRINT.PC.NAME
	;print defender name (PC)		
	JSR COMBAT.PRINT.PC_NAME

.UPDATE.SCROLL.WINDOW.S_ENTITY_NAME.DONE	
	;make sure attack outcome is on line after defender name and 
	JSR PRINT.TEXT.WINDOW.CR ;print carriage return to combat scroll window

	
	RTS
	
@END
		
COMBAT.PRINT.MOB_NAME ;prints MOB/SPECIAL name to the combat scroll window
@START
;PARAMETERS: CHR_SHEET.RECORD.READ (8)
;ENTRANCE: direct
;RETURN: printed text

		LDA #CHR_SHEET.MOB.NAME					
		STA TWF.STRING
		
		LDA /CHR_SHEET.MOB.NAME
		STA TWF.STRING+$1	
	JSR PRINT.TEXT.WINDOW
	
	RTS
	
@END

COMBAT.PRINT.PC_NAME ;prints PC name to the combat scroll window
@START
;PARAMETERS: CHR_SHEET.RECORD.READ (8)
;ENTRANCE: direct
;RETURN: printed text

		LDA #CHR_SHEET.PC.NAME.CHARACTER					
		STA TWF.STRING+$0
		
		LDA /CHR_SHEET.PC.NAME.CHARACTER
		STA TWF.STRING+$1		
	JSR PRINT.TEXT.WINDOW
	
	RTS
	
@END

	
@END

;TEXT BLOCKS2 (also see TEXT BLOCKS1)
@START

;=====COMBAT MATH====
;COMBAT.TEXT_BLOCK.MATH.TO_HIT 				.AZ -/Hit%        /			;ASCII text string
;COMBAT.TEXT_BLOCK.MATH.TO_HIT.ROLL			.AZ -/ roll       /			;ASCII text string
;COMBAT.TEXT_BLOCK.MATH.BASE_DMG				.AZ -/Dmg       /			;ASCII text string
;COMBAT.TEXT_BLOCK.MATH.CRTL_DMG				.AZ -/CR dmg    /			;ASCII text string
;COMBAT.TEXT_BLOCK.MATH.ARMOR.ROLL			.AZ -/Armor       /			;ASCII text string
;COMBAT.TEXT_BLOCK.MATH.ARMOR.ROLL			.AZ -/Armor     /			;ASCII text string
;COMBAT.TEXT_BLOCK.MATH.MAGIC_DEFENSE.ROLL	.AZ -/Magic def /			;ASCII text string
;COMBAT.TEXT_BLOCK.MATH.TOTAL_DMG			.AZ -/Total dmg /			;ASCII text string
;COMBAT.TEXT_BLOCK.MATH.DEFENDER.FINAL_HP	.AZ -/H.P.      /			;ASCII text string
;COMBAT.TEXT_BLOCK.MATH.FLAGS				.AZ -/Flags     /			;ASCII text string

;=====ATTACK/CAST====
COMBAT.TEXT_BLOCK.HIT				.AZ	-/hit!/,#$8D
COMBAT.TEXT_BLOCK.KILLED			.AZ	-/killed!/,#$8D

COMBAT.TEXT_BLOCK.STATUS_BAR_LABEL	.AZ	-/ H.P./,#$8D
COMBAT.TEXT_BLOCK.SELECT_TARGET		.AZ	-#$8D,/Select target!/,#$8D
;COW.TEXT		.AZ	-#$8D

COMBAT.TEXT_BLOCK.DEATH_MAGIC.SUCCEEDED		.AZ	-/death magic succeeded!/,#$8D
COMBAT.TEXT_BLOCK.DEATH_MAGIC.FAILED		.AZ	-/death magic failed!/,#$8D
COMBAT.TEXT_BLOCK.MP_TOO_LOW				.AZ	-#$8D,/M.P. too low!/
COMBAT.TEXT_BLOCK.NO_SPELL_READIED			.az -/No spell readied!/
@END

;SCREEN_BUILDER COMMAND TABLES
@START

;*Note: Must use .DB directive when writing bytes to a file using a label expression (i.e. .COMMAND_TABLE1.RECORDS.END-.COMMAND_TABLE1.RECORDS.START)
 
 
COMBAT.SB_COMMAND_TABLE1 ;TO_HIT, TO_HIT ROLL
@START


;COMMAND TABLE SIZE (bytes)
	.DB COMBAT.SB_COMMAND_TABLE1.RECORDS.END-COMBAT.SB_COMMAND_TABLE1.RECORDS.START+1 ;+1 the byte quantity includes this byte that we are writing to the file with this directive. 
COMBAT.SB_COMMAND_TABLE1.RECORDS.START
	
.CT.TO_HIT
@START
	
;LABEL

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$02 ;local ascii table, PRINT.TEXT.WINDOW

	;command parameter fields (PRC)
	;local ascii table
COMBAT.TEXT_BLOCK.MATH.TO_HIT 				.AZ -/Hit%        /			;ASCII text string

;VARIABLE

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$16 ;variable, PRINT.TEXT.WINDOW.BCD8
	;command parameter fields (PRC)
	.DA #COMBAT.STATS.MODIFIED_TO_HIT
	.DA /COMBAT.STATS.MODIFIED_TO_HIT

	
@END


.CT.TO_HIT.ROLL
@START
	
;LABEL

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$02 ;local ascii table, PRINT.TEXT.WINDOW

	;command parameter fields (PRC)
	;local ascii table
COMBAT.TEXT_BLOCK.MATH.TO_HIT.ROLL			.AZ -/ roll       /			;ASCII text string

;VARIABLE

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$16 ;variable, PRINT.TEXT.WINDOW.BCD8
	;command parameter fields (PRC)
	.DA #COMBAT.STATS.TO_HIT.ROLL	;***this value is BCD format
	.DA /COMBAT.STATS.TO_HIT.ROLL	;***this value is BCD format

	
@END




COMBAT.SB_COMMAND_TABLE1.RECORDS.END

COMBAT.SB_COMMAND_TABLE1.SIZE = COMBAT.SB_COMMAND_TABLE1.RECORDS.END-COMBAT.SB_COMMAND_TABLE1.RECORDS.START+1

;VERIFY COMMAND TABLE SIZE <= !256bytes
	.DO COMBAT.SB_COMMAND_TABLE1.SIZE>$100
	
	.ER	F,***COMBAT.SB_COMMAND_TABLE1 exceeds !256 bytes
	
	.FI ;END IF
@END

COMBAT.SB_COMMAND_TABLE2 ;DAMAGE ROLL
@START


;COMMAND TABLE SIZE (bytes)
	.DB COMBAT.SB_COMMAND_TABLE2.RECORDS.END-COMBAT.SB_COMMAND_TABLE2.RECORDS.START+1 ;+1 the byte quantity includes this byte that we are writing to the file with this directive. 
COMBAT.SB_COMMAND_TABLE2.RECORDS.START
	
	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$0C ;toggle text window <CR> parm: ON

	
.CT.BASE_DAMAGE.ROLL
@START
;=========ORIGINAL NOTES BEFORE CONVERSION TO SCREEN_BUILDER==================================
;CONVERT BASE DAMAGE ROLL TO BCD THEN PRINT
;(the base damage will be a max of 3 unpacked BCD digits. Since the conversion
;routine outputs 3 packed BCD digits (6 unpacked digits), the lowest 3 digits will
;be the LO nibble in BCD+$1 and both nibbles of BCD+$0. CONVERT.HEX_TO_ASCII takes care
;of unpacking the nibbles as well as converting to ASCII)
;=============================================================================================

;LABEL

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$02 ;local ascii table, PRINT.TEXT.WINDOW

	;command parameter fields (PRC)
	;local ascii table
COMBAT.TEXT_BLOCK.MATH.BASE_DMG				.AZ -/Dmg       /			;ASCII text string

;VARIABLE

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$18 ;variable, PRINT.HEX16.RJ
	;command parameter fields (PRC)
	.DA #COMBAT.STATS.BASE_DAMAGE.ROLL_X2		;16-bit
	.DA /COMBAT.STATS.BASE_DAMAGE.ROLL_X2

	
@END





COMBAT.SB_COMMAND_TABLE2.RECORDS.END

COMBAT.SB_COMMAND_TABLE2.SIZE = COMBAT.SB_COMMAND_TABLE2.RECORDS.END-COMBAT.SB_COMMAND_TABLE2.RECORDS.START+1

;VERIFY COMMAND TABLE SIZE <= !256bytes
	.DO COMBAT.SB_COMMAND_TABLE2.SIZE>$100
	
	.ER	F,***COMBAT.SB_COMMAND_TABLE2 exceeds !256 bytes
	
	.FI ;END IF
@END

COMBAT.SB_COMMAND_TABLE3 ;CRITICAL HIT DAMAGE
@START


;COMMAND TABLE SIZE (bytes)
	.DB COMBAT.SB_COMMAND_TABLE3.RECORDS.END-COMBAT.SB_COMMAND_TABLE3.RECORDS.START+1 ;+1 the byte quantity includes this byte that we are writing to the file with this directive. 
COMBAT.SB_COMMAND_TABLE3.RECORDS.START
	
	
.CT.CRTL_HIT_DAMAGE
@START
;=========ORIGINAL NOTES BEFORE CONVERSION TO SCREEN_BUILDER==================================
;CONVERT BASE DAMAGE ROLL TO BCD THEN PRINT
;(the base damage will be a max of 3 unpacked BCD digits. Since the conversion
;routine outputs 3 packed BCD digits (6 unpacked digits), the lowest 3 digits will
;be the LO nibble in BCD+$1 and both nibbles of BCD+$0. CONVERT.HEX_TO_ASCII takes care
;of unpacking the nibbles as well as converting to ASCII)
;=============================================================================================

;LABEL

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$02 ;local ascii table, PRINT.TEXT.WINDOW

	;command parameter fields (PRC)
	;local ascii table
COMBAT.TEXT_BLOCK.MATH.CRTL_DMG				.AZ -/CR dmg    /			;ASCII text string

;VARIABLE

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$18 ;variable, PRINT.HEX16.RJ
	;command parameter fields (PRC)
	.DA #COMBAT.STATS.CRTL_HIT_DAMAGE		;16-bit
	.DA /COMBAT.STATS.CRTL_HIT_DAMAGE

	
@END



COMBAT.SB_COMMAND_TABLE3.RECORDS.END

COMBAT.SB_COMMAND_TABLE3.SIZE = COMBAT.SB_COMMAND_TABLE3.RECORDS.END-COMBAT.SB_COMMAND_TABLE3.RECORDS.START+1

;VERIFY COMMAND TABLE SIZE <= !256bytes
	.DO COMBAT.SB_COMMAND_TABLE3.SIZE>$100
	
	.ER	F,***COMBAT.SB_COMMAND_TABLE3 exceeds !256 bytes
	
	.FI ;END IF
@END

COMBAT.SB_COMMAND_TABLE4 ;ARMOR ROLL
@START


;COMMAND TABLE SIZE (bytes)
	.DB COMBAT.SB_COMMAND_TABLE4.RECORDS.END-COMBAT.SB_COMMAND_TABLE4.RECORDS.START+1 ;+1 the byte quantity includes this byte that we are writing to the file with this directive. 
COMBAT.SB_COMMAND_TABLE4.RECORDS.START
	

.CT.REGULAR.ARMOR.ROLL
@START

;LABEL

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$02 ;local ascii table, PRINT.TEXT.WINDOW

	;command parameter fields (PRC)
	;local ascii table
COMBAT.TEXT_BLOCK.MATH.ARMOR.ROLL			.AZ -/Armor     /			;ASCII text string

;VARIABLE

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$15 ;variable, PRINT.TEXT.WINDOW.HEX8RJ
	;command parameter fields (PRC)
	.DA #COMBAT.STATS.REGULAR.ARMOR.ROLL		;16-bit
	.DA /COMBAT.STATS.REGULAR.ARMOR.ROLL

	
@END



COMBAT.SB_COMMAND_TABLE4.RECORDS.END

COMBAT.SB_COMMAND_TABLE4.SIZE = COMBAT.SB_COMMAND_TABLE4.RECORDS.END-COMBAT.SB_COMMAND_TABLE4.RECORDS.START+1

;VERIFY COMMAND TABLE SIZE <= !256bytes
	.DO COMBAT.SB_COMMAND_TABLE4.SIZE>$100
	
	.ER	F,***COMBAT.SB_COMMAND_TABLE4 exceeds !256 bytes
	
	.FI ;END IF
@END
	
COMBAT.SB_COMMAND_TABLE5 ;MAGIC DEFENSE ROLL
@START


;COMMAND TABLE SIZE (bytes)
	.DB COMBAT.SB_COMMAND_TABLE5.RECORDS.END-COMBAT.SB_COMMAND_TABLE5.RECORDS.START+1 ;+1 the byte quantity includes this byte that we are writing to the file with this directive. 
COMBAT.SB_COMMAND_TABLE5.RECORDS.START
	
	
.CT.MAGIC_DEFENSE.ROLL
@START

;LABEL

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$02 ;local ascii table, PRINT.TEXT.WINDOW

	;command parameter fields (PRC)
	;local ascii table
COMBAT.TEXT_BLOCK.MATH.MAGIC_DEFENSE.ROLL	.AZ -/Magic def /			;ASCII text string

;VARIABLE

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$18 ;variable, PRINT.TEXT.WINDOW.BCD8
	;command parameter fields (PRC)
	.DA #COMBAT.STATS.RESIST_MAGIC.DEFENSE_RATING
	.DA /COMBAT.STATS.RESIST_MAGIC.DEFENSE_RATING

	
@END


COMBAT.SB_COMMAND_TABLE5.RECORDS.END

COMBAT.SB_COMMAND_TABLE5.SIZE = COMBAT.SB_COMMAND_TABLE5.RECORDS.END-COMBAT.SB_COMMAND_TABLE5.RECORDS.START+1

;VERIFY COMMAND TABLE SIZE <= !256bytes
	.DO COMBAT.SB_COMMAND_TABLE5.SIZE>$100
	
	.ER	F,***COMBAT.SB_COMMAND_TABLE5 exceeds !256 bytes
	
	.FI ;END IF
@END

COMBAT.SB_COMMAND_TABLE6 ;TOTAL DAMAGE
@START


;COMMAND TABLE SIZE (bytes)
	.DB COMBAT.SB_COMMAND_TABLE6.RECORDS.END-COMBAT.SB_COMMAND_TABLE6.RECORDS.START+1 ;+1 the byte quantity includes this byte that we are writing to the file with this directive. 
COMBAT.SB_COMMAND_TABLE6.RECORDS.START
	

.CT.DAMAGE_FINAL.ROLL
@START
;=========ORIGINAL NOTES BEFORE CONVERSION TO SCREEN_BUILDER==================================
;
;(the base damage will be a max of 4 unpacked BCD digits. Since the conversion
;routine outputs 3 packed BCD digits (6 unpacked digits), the lowest 4 digits will
;be both nibbles of BCD+$1 and BCD+$0. CONVERT.HEX_TO_ASCII takes care
;of unpacking the nibbles as well as converting to ASCII)
;		
;=============================================================================================

;LABEL

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$02 ;local ascii table, PRINT.TEXT.WINDOW

	;command parameter fields (PRC)
	;local ascii table
COMBAT.TEXT_BLOCK.MATH.TOTAL_DMG			.AZ -/Final dmg /			;ASCII text string

;VARIABLE

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$18 ;variable, PRINT.HEX16.RJ
	;command parameter fields (PRC)
	.DA #COMBAT.STATS.DAMAGE.FINAL		;16-bit
	.DA /COMBAT.STATS.DAMAGE.FINAL

	
@END


.CT.HP.TARGET.FINAL ;***LABEL ONLY***
@START
;LABEL

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$02 ;local ascii table, PRINT.TEXT.WINDOW

	;command parameter fields (PRC)
	;local ascii table
COMBAT.TEXT_BLOCK.MATH.DEFENDER.FINAL_HP	.AZ -/H.P.      /			;ASCII text string


	
@END




COMBAT.SB_COMMAND_TABLE6.RECORDS.END

COMBAT.SB_COMMAND_TABLE6.SIZE = COMBAT.SB_COMMAND_TABLE6.RECORDS.END-COMBAT.SB_COMMAND_TABLE6.RECORDS.START+1

;VERIFY COMMAND TABLE SIZE <= !256bytes
	.DO COMBAT.SB_COMMAND_TABLE6.SIZE>$100
	
	.ER	F,***COMBAT.SB_COMMAND_TABLE6 exceeds !256 bytes
	
	.FI ;END IF
@END

COMBAT.SB_COMMAND_TABLE7 ;FLAGS
@START


;COMMAND TABLE SIZE (bytes)
	.DB COMBAT.SB_COMMAND_TABLE7.RECORDS.END-COMBAT.SB_COMMAND_TABLE7.RECORDS.START+1 ;+1 the byte quantity includes this byte that we are writing to the file with this directive. 
COMBAT.SB_COMMAND_TABLE7.RECORDS.START
	

.CT.FLAGS.ROLL
@START

;LABEL

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$02 ;local ascii table, PRINT.TEXT.WINDOW

	;command parameter fields (PRC)
	;local ascii table
COMBAT.TEXT_BLOCK.MATH.FLAGS				.AZ -/Flags     /			;ASCII text string

;VARIABLE

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$13 ;variable, PRINT.TEXT.WINDOW.HEX8
	;command parameter fields (PRC)
	.DA #COMBAT.STATS.RANGE_INTERFERENCE.FLAG ;($00 = inteference successful | $01 = interference not attempted | $03 = inteference failed)
	.DA /COMBAT.STATS.RANGE_INTERFERENCE.FLAG

;VARIABLE

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$13 ;variable, PRINT.TEXT.WINDOW.HEX8
	;command parameter fields (PRC)
	.DA #COMBAT.STATS.DODGE_FLAG	;($00 = dodge succeeded, $01 dodge not attempted, $03 dodge failed)
	.DA /COMBAT.STATS.DODGE_FLAG

;VARIABLE

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$13 ;variable, PRINT.TEXT.WINDOW.HEX8
	;command parameter fields (PRC)
	.DA #COMBAT.STATS.CRTL_HIT_FLAG ;($00 = critical hit succeeded, $01 = critical hit not attempted, $03 = critical hit blocked by defender)
	.DA /COMBAT.STATS.CRTL_HIT_FLAG

;VARIABLE

	;set command code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
	.DA #$13 ;variable, PRINT.TEXT.WINDOW.HEX8
	;command parameter fields (PRC)
	.DA #COMBAT.STATS.SPELL.KILL_FLAG ;death magic attack: ($00 = succeeded | $01 = not attempted | $03 = failed). ;($00 = critical hit succeeded, $01 = critical hit not attempted, $03 = critical hit blocked by defender)
	.DA /COMBAT.STATS.SPELL.KILL_FLAG	
	
@END




COMBAT.SB_COMMAND_TABLE7.RECORDS.END

COMBAT.SB_COMMAND_TABLE7.SIZE = COMBAT.SB_COMMAND_TABLE7.RECORDS.END-COMBAT.SB_COMMAND_TABLE7.RECORDS.START+1

;VERIFY COMMAND TABLE SIZE <= !256bytes
	.DO COMBAT.SB_COMMAND_TABLE7.SIZE>$100
	
	.ER	F,***COMBAT.SB_COMMAND_TABLE7 exceeds !256 bytes
	
	.FI ;END IF
@END

	
@END


;OTHER SUBROUTINES
@START
COMBAT.SELECT.ATTACK_TARGET ;(PC)
@START
;PARAMETERS: COMBAT.PC.ACTIVE.RECORD, SELECT_TARGET.RADIUS
;ENTRANCE: direct
;RETURN: SELECT_TARGET.ACTIVE.SELECTION.SINDEX, COMBAT.PLAYER_SELECTED.TARGET.GMAP.X, COMBAT.PLAYER_SELECTED.TARGET.GMAP.Y ;load new special S_ENTITY GMAP.Y (**i don't recall what this means, it might have been tacked onto this line by mistake)

				; ldX #$aA
				; ;ldx SELECT_TARGET.MTT.PRESENT ;($00 = MTT s_entity not present at current search position | $01 = MTT s_entity not present at current search position)
				; jsr prep.brk
				; brk
	
.MAIN.ROUTINE
@START

	
;SAVE PARAMETERS
	;ACC = parm: radius
	STA SELECT_TARGET.RADIUS

			
;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA


;VALIDATE ENTRY
;(this routine is called by some spells, and if those spells are cast by MOBs or SPECIALs this routine should be skipped because it is for PCs only. MOBs and SPECIALS have their own 
;target selection routine)

;PC/MOB TURN BRANCH
	LDA COMBAT.TURN_STATUS   ;($00 = PCs turn. $01 = Special(s) S_ENTITY (example Wyvern Ally, Summoned Demon), $02 = MOBs turn)
	BEQ .INIT
	JMP .EXIT

	
.INIT	
	LDA #$00
	STA SELECT_TARGET.ITERATION.COUNTER ;if there is a last target on file, used to force target database after arrow keys are used 
	STA SELECT_TARGET.LAST_TARGET.FLAG ;($00 = not set, >= $01 = set). Set when a last target is on file for the active player
	
;****IS THE LAST KEY INIT STILL NEEDED SINCE WE AREN'T USING THE DB ANYMORE?	
	LDA #$95 ;ASCII = right arrow. 
	STA SELECT_TARGET.LAST_KEY ;used to determine which direction in the database to iterate if a PC record is found and PCs are not valid targets. 
	
; .NO_TARGETS.CHECK	
	; LDA COMBAT.S_ENTITY.DISTANCE.LENGTH ;if length = 0 then no targets are in range
	; BNE .DRAW.TARGET.SELECTOR ;if there are targets in range, then draw the target selector
	; ;set abort flag so that active player can continue its turn
	; LDA #$01
	; STA COMBAT.ABORT.FLAG ;($00 = not set, $01 = set)	
	; JMP .EXIT
					
.DRAW.TARGET.SELECTOR
			
	LDX #$00	; init COMBAT.S_ENTITY.DISTANCE.DB index to 1st record
.DRAW.TARGET_SELECTOR.LOOP
;GET TARGET LOCATION

;LOAD LAST TARGET FOR ACTIVE PLAYER
;(if there is one on file)

	LDA SELECT_TARGET.CURRENT_TARGET.X
	STA PARM.GMAP.X 
	
	LDA SELECT_TARGET.CURRENT_TARGET.Y
	STA PARM.GMAP.Y 
			
	LDA SELECT_TARGET.ITERATION.COUNTER ;if there is a last target on file, used to force target database after arrow keys are used 
	BNE .LOOKUP.SBYTE.STEP ;if not first iteration, then don't check for a target on file because the player is controlling the target selection with the arrow keys

	;calculate SELECT_TARGET.LAST_TARGET.DB index
	LDA COMBAT.PC.ACTIVE ;load the number of the active player (i.e. 1,2,3,4,5,6)
	ASL ;X2
	SEC
	SBC #$02
	TAY ;set index for SELECT_TARGET.LAST_TARGET.DB
	STY SELECT_TARGET.LAST_TARGET.INDEX

			
	LDA SELECT_TARGET.LAST_TARGET.DB+$0,Y ;load S_ENTITY type of last target
	CMP #SELECT_TARGET.LAST_TARGET.INIT_VALUE
	BEQ .LAST_TARGET.CHECK.COMPLETE ;branch if no "last target" entry is stored for the active PC.



			
		PHA ;save S_ENTITY type
		LDA SELECT_TARGET.LAST_TARGET.DB+$1,Y ;load map object index of last target
		TAY ;map object index
		PLA ;restore S_ENTITY type
		ORA #$80 ;mask-in high bit to set lookup mode: S_ENTITY TYPE & MAP OBJECT INDEX	
		STA SELECT_TARGET.LAST_TARGET.FLAG ;($00 = not set, >= $01 = set). Set when a last target is on file for the active player. This is just a convenient place to set the flag because there is a value >0 in the ACC
	JSR READ.MAP_OBJECT.ARRAY
		;is last target dead?
		LDA SELECT_TARGET.LAST_TARGET.FLAG ;set a few lines above to the S_ENTITY type with the high bit set
		AND #$7F ;mask-out high bit ;**OPT** Memory. Speed. This AND can be eliminated if constant is created containg the value of S_ENTITY_TYPE.PC with the high bit set. Then check for that in the CMP below
		;ACC = S_ENTITY type
		CMP #S_ENTITY_TYPE.PC
		BNE .MOB_SPECIAL.DEAD.CHECK
;.PC.DEAD.CHECK
		LDA MAP_OBJECT.RECORD.READ+$3 ;load health status
		CMP #COMBAT.S_ENTITY.STATUS.DEAD 
		BEQ .LAST_TARGET.DEAD ;if yes, branch
		JMP .DEAD.CHECKS.DONE

.MOB_SPECIAL.DEAD.CHECK
		LDA MAP_OBJECT.RECORD.READ+$7 ;load health status
		CMP #COMBAT.S_ENTITY.STATUS.DEAD 
		BEQ .LAST_TARGET.DEAD ;if yes, branch
.DEAD.CHECKS.DONE
		
.WEAPON.RADIUS.CHECK
;(double check weapon radius because the spell file version of select target (COMBAT.SELECT.ATTACK_TARGET) may
;have set a last target outside players weapon's radius because spells usually don't have a radius limit)

		LDA MAP_OBJECT.RECORD.READ+$0 ;load last target GMAP X axis
		STA SELECT_TARGET.SEARCH_POSITION.X ;will be loaded into PARM2.GMAP.X
		STA PARM.GMAP.X
		LDA MAP_OBJECT.RECORD.READ+$1 ;load last target GMAP Y axis
		STA SELECT_TARGET.SEARCH_POSITION.Y ;will be loaded into PARM2.GMAP.Y
		STA PARM.GMAP.Y

	;lookup screen index of last target 
			;PARM.GMAP.X = set above
			;PARM.GMAP.Y = set above
		JSR CONVERT.COLUMN_ROW.TO.SINDEX
			LDY RETURN.SCREEN_ARRAY_INDEX ;used below
		;Y-REG = sindex of last target	
	JSR .RADIUS_CHECK.ALTERNATE_ENTRANCE
				; ;LDA SELECT_TARGET.RADIUS
				; LDX SELECT_TARGET.SEARCH_POSITION.X
				; LDY SELECT_TARGET.SEARCH_POSITION.Y
				; JSR PREP.BRK
				; BRK
				
		;RETURN VALUE: ACC = ($00 = target distance exceeds radius, $01 = target distance is within radius)
		CMP #$00
		BEQ .LAST_TARGET.CHECK.COMPLETE ;find new target. the target set as last target exceeds the player's weapon radius
			;else: use "last target" on file if target distance is within radius

.WEAPON.RADIUS.DONE

		;**FALLS THROUGH
		
.SETUP.LAST.TARGET
		;set GMAP.X/Y of last target
		LDA MAP_OBJECT.RECORD.READ+$0 ;load last target GMAP X axis
		STA SELECT_TARGET.CURRENT_TARGET.X	
		STA PARM.GMAP.X
		LDA MAP_OBJECT.RECORD.READ+$1 ;load last target GMAP Y axis
		STA SELECT_TARGET.CURRENT_TARGET.Y	
		STA PARM.GMAP.Y
.LOOKUP.SBYTE.STEP
	JMP .LOOKUP.SBYTE

.EXIT_STEP
	JMP .EXIT
	
.LAST_TARGET.DEAD
	;clear last target flag
	LDA #$00
	STA SELECT_TARGET.LAST_TARGET.FLAG ;($00 = not set, >= $01 = set). Set when a last target is on file for the active player. This is just a convenient place to set the flag because there is a value >0 in the ACC
	
	;**FALLS THROUGH** (select new target)
	
.LAST_TARGET.CHECK.COMPLETE
		;**OPT** convert this routine and COMBAT.S_ENTITY.DISTANCE.DB to use screen index 
		;instead of GMAP.X/Y. I think I'm converting them back and forth. Then again, GMAP.X/Y
		;is required to calculate distance so closer examination would be required to see if there are material savings. 

		
.NEW_TARGET ;start with target closest to active player	
			

			
	JSR .SELECT_TARGET.GET.CLOSEST
				
		;if no target found then exit
		LDA COMBAT.ABORT.FLAG ;($00 = not set, $01 = set)	
		BNE .EXIT_STEP
		
		LDA SELECT_TARGET.CLOSEST_TARGET.X	;points to same memory as SELECT_TARGET.CURRENT_TARGET.X
		STA PARM.GMAP.X
		STA SELECT_TARGET.CURRENT_TARGET.X ;this variable is set here because it is used to specify the target when attack (probably also cast) calls the fire projectile routine
		LDA SELECT_TARGET.CLOSEST_TARGET.Y  ;points to same memory as SELECT_TARGET.CURRENT_TARGET.Y
		STA PARM.GMAP.Y
		STA SELECT_TARGET.CURRENT_TARGET.Y ;this variable is set here because it is used to specify the target when attack (probably also cast) calls the fire projectile routine
		
.LOOKUP.SBYTE

			
	LDA SELECT_TARGET.ITERATION.COUNTER ;if there is a last target on file, used to force target database after arrow keys are used 
	BNE .UPDATE.SCROLL.WINDOW.DONE ;if not first iteration, then don't check for a target on file because the player is controlling the target selection with the arrow keys
;.UPDATE.SCROLL.WINDOW.DONE
		LDA #COMBAT.TEXT_BLOCK.SELECT_TARGET					
		STA TWF.STRING+$0
		
		LDA /COMBAT.TEXT_BLOCK.SELECT_TARGET
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW
.UPDATE.SCROLL.WINDOW.DONE

	
			; jsr keyin
			; LDA #$AA
			; LDX COMBAT.SPELL_CODE.CAST
			; JSR PREP.BRK
			; BRK	

			
	JSR CONVERT.COLUMN_ROW.TO.SINDEX
		LDY RETURN.SCREEN_ARRAY_INDEX ;used below
		STY SELECT_TARGET.ACTIVE.SELECTION.SINDEX

;******IS THIS APPROACH STILL VALID IF PC TARGET WAS SELECTED?

	; ;is target a PC?
	; LDA SCREEN.MO_SPRITE_TYPE.DATA,Y
	; CMP #S_ENTITY_TYPE.PC
	; BNE .GET.TARGET.LOCATION.COMPLETE ;if no, branch
	; ;are PC's valid targets? (check flag)
	; LDA SELECT_TARGET.PC_VALID.FLAG ;($00 = off, PCs not valid targets | $01 = on, PCs are valid targets)
	; BNE .GET.TARGET.LOCATION.COMPLETE ;if yes, branch
	
	; ;skip PC record: determine which direction in database to iterate
	; LDA SELECT_TARGET.LAST_KEY ;the key pressed on the last iteration
	; CMP #$95			;RIGHT ARROW
	; BEQ .KEYPRESS.RIGHT_ARROW
	; CMP #$88			;LEFT ARROW
	; BEQ	.KEYPRESS.LEFT_ARROW
	
	; ;**FALLS THROUGH**
	
; .ERROR.UNEXPECTED.LAST_KEY
; ;unexpected value in variable SELECT_TARGET.LAST_KEY reported by .NEW_TARGET
	; JSR PREP.BRK
	; BRK
		
.GET.TARGET.LOCATION.COMPLETE
	
	
			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; LDA #$AA
			; ; LDX #SELECT_TARGET.LAST_TARGET.DB
			; ; LDY /SELECT_TARGET.LAST_TARGET.DB
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP

.KEYIN_LOOP			
	;connect shape table pointer
	LDA #SELECT_TARGET.STANDARD.SHAPE_0.AND_MASK
	STA SHAPE.BACKGROUND.AND_MASK+$0

	LDA /SELECT_TARGET.STANDARD.SHAPE_0.AND_MASK
	STA SHAPE.BACKGROUND.AND_MASK+$1

		;Y-REG: screen tile location of prospective target
		LDA #$01 ;set draw mode: animation. ($00 = not animated | $01 = animated)	
		;LDA #$60 ;set draw mode: animation. ($00 = not animated | $01 = animated)
		STA COMBAT.DRAW.SINGLE.OFF_GRID.WAIT_VALUE ;Parameter value for WAIT.LOOP between drawing the shape and screen animation update. We set it to $01 here to save doing an LDA to set it to $00. $01 is short enough. 	
	JSR COMBAT.DRAW.SINGLE.OFF_GRID ;draw animated shape
		
	
	LDA KB_BUFFER
    ;BPL .DRAW.TARGET_SELECTOR.LOOP_STEP	;loop until key is pressed, depositing its ascii value in the keyboard buffer
    BPL .KEYIN_LOOP	;loop until key is pressed, depositing its ascii value in the keyboard buffer
	STA KB_BUFFER_ACK       ;clr last key

.PARSE.KEYPRESS
@START
	;ACC = last keypress
	JSR CONVERT.ASCII.UCASE
		;RETURN VALUE: ACC = ASCII code (upper case, if the code is a letter)
	
	STA SELECT_TARGET.LAST_KEY
	CMP #$95			;RIGHT ARROW
	BEQ .FIND.NEXT_TARGET
	CMP #$88			;LEFT ARROW
	BEQ	.FIND.NEXT_TARGET	
	CMP #$8A			;DOWN ARROW
	BEQ	.FIND.NEXT_TARGET		
	CMP #$8B			;UP ARROW
	BEQ	.FIND.NEXT_TARGET	
	CMP #$89			;TAB KEY
	BEQ	.KEYPRESS.TAB
	CMP #$9B			;ESC KEY
	BEQ	.KEYPRESS.ESC
	
	;target selecton keys
	CMP #$C1			;'A' (since the player used the (A)ttack command to get to this routine, it is convenient as a target selection key)
	BEQ	.TARGET.SELECTED
	CMP #$8D			;RETURN/ENTER KEY
	BEQ	.TARGET.SELECTED
	CMP #$A0			;SPACE BAR
	BEQ	.TARGET.SELECTED
	;default
	;keypress not valid, continue keyin loop

.DRAW.TARGET_SELECTOR.LOOP_STEP
	JMP .DRAW.TARGET_SELECTOR.LOOP

.FIND.NEXT_TARGET
	JSR .SELECT_TARGET.FIND.NEXT_TARGET
	
	JMP .DRAW.TARGET_SELECTOR.LOOP


; .KEYPRESS.RIGHT_ARROW
	; LDA SELECT_TARGET.LAST_TARGET.FLAG ;($00 = not set, >= $01 = set). Set when a last target is on file for the active player
	; BEQ .NEXT.TARGET.RIGHT ;if last target isn't on file for active player, then incremet target database record index. 
	; LDA SELECT_TARGET.ITERATION.COUNTER ;if there is a last target on file, used to force target database after arrow keys are used 
	; BNE .NEXT.TARGET.RIGHT ;if first iteration, and last target was used, skip first database increment so that the selector starts on the closest target
	; ;is last target also the 1st target in databse?
	; LDA SELECT_TARGET.CURRENT_TARGET.X
	; CMP COMBAT.S_ENTITY.DISTANCE.DB+$1,X ;is GMAP.X of last target (currently selected) and the GMAP.X of the 1st target in the database the same?
	; BNE .DB.CHECK.COMPLETE
	; LDA SELECT_TARGET.CURRENT_TARGET.Y
	; CMP COMBAT.S_ENTITY.DISTANCE.DB+$2,X ;is GMAP.Y of last target (currently selected) and the GMAP.Y of the 1st target in the database the same?
	; BEQ .NEXT.TARGET.RIGHT	;if last target is the same as first in the db then increment db index so that the same target isn't displayed twice
; .DB.CHECK.COMPLETE
	; JMP .KEYPRESS.RIGHT_ARROW.EXIT
	
; .NEXT.TARGET.RIGHT
	; ;next target
	; INX	;advance index (COMBAT.S_ENTITY.DISTANCE.DB)
	; INX
	; INX
	
	; CPX COMBAT.S_ENTITY.DISTANCE.LENGTH ;was the last target displayed the last in the database?
	; BCC .KEYPRESS.RIGHT_ARROW.EXIT ;if no, then the index should now be pointed to the next target
	; ;if yes, then flip index to 1st database record
	; LDX #$00

; .KEYPRESS.RIGHT_ARROW.EXIT
	; INC SELECT_TARGET.ITERATION.COUNTER ;if there is a last target on file, used to force target database after arrow keys are used 
	; JMP .DRAW.TARGET_SELECTOR.LOOP

; .KEYPRESS.LEFT_ARROW
	; LDA SELECT_TARGET.LAST_TARGET.FLAG ;($00 = not set, >= $01 = set). Set when a last target is on file for the active player
	; BEQ .NEXT.TARGET.LEFT ;if last target isn't on file for active player, then incremet target database record index. 
	; LDA SELECT_TARGET.ITERATION.COUNTER ;if there is a last target on file, used to force target database after arrow keys are used 
	; BNE .NEXT.TARGET.LEFT ;if first iteration, and last target was used, skip first database increment so that the selector starts on the closest target
	; ;is last target also the 1st target in databse?
	; LDA SELECT_TARGET.CURRENT_TARGET.X
	; CMP COMBAT.S_ENTITY.DISTANCE.DB+$1,X ;is GMAP.X of last target (currently selected) and the GMAP.X of the 1st target in the database the same?
	; BNE .DB.CHECK.COMPLETE2
	; LDA SELECT_TARGET.CURRENT_TARGET.Y
	; CMP COMBAT.S_ENTITY.DISTANCE.DB+$2,X ;is GMAP.Y of last target (currently selected) and the GMAP.Y of the 1st target in the database the same?
	; BEQ .NEXT.TARGET.LEFT	;if last target is the same as first in the db then increment db index so that the same target isn't displayed twice
; .DB.CHECK.COMPLETE2
	; JMP .KEYPRESS.LEFT_ARROW.EXIT

; .NEXT.TARGET.LEFT
	; CPX #$00 ;was the last target displayed the first in the database?
	; BEQ .FLIP.INDEX 	;if yes, then flip index to last database record

	; ;if no, then increment index to the next target
	; DEX	;advance index (COMBAT.S_ENTITY.DISTANCE.DB)
	; DEX
	; DEX
	
	; JMP .KEYPRESS.LEFT_ARROW.EXIT

; .FLIP.INDEX	
	; LDX COMBAT.S_ENTITY.DISTANCE.LENGTH ;always equal to the 1st byte after the end of the last record. 
	; DEX	;advance index (COMBAT.S_ENTITY.DISTANCE.DB)
	; DEX
	; DEX
	
	; ;**FALLS THROUGH**

; .KEYPRESS.LEFT_ARROW.EXIT	
	; INC SELECT_TARGET.ITERATION.COUNTER ;if there is a last target on file, used to force target database after arrow keys are used 
	; JMP .DRAW.TARGET_SELECTOR.LOOP
		

.KEYPRESS.TAB
	LDA #$03 ;set flag
	JMP .SET.ABORT.FLAG
	
.KEYPRESS.ESC
	LDA #$02 ;set flag

	;**FALLS THROUGH**
.SET.ABORT.FLAG	
	STA COMBAT.ABORT.FLAG	;($00 = not set | $01 = target not found | $02 = ESC pressed to exit attack command | $03 = TAB pressed to toggle left/right hand active)		
	JMP .EXIT
	
@END

.TARGET.SELECTED
	;SELECT_TARGET.CURRENT_TARGET.X is set in loop above
	;SELECT_TARGET.CURRENT_TARGET.Y is set in loop above

	;save selected target as the last target for the active player
	;(so that on the next turn for the active player, target selectin will begin with the target selected this turn)

	LDX SELECT_TARGET.LAST_TARGET.INDEX
	LDA SCREEN.MO_SPRITE_TYPE.DATA,Y ;lookup S_ENTITY type of selected target		
	STA SELECT_TARGET.LAST_TARGET.DB+$0,X
	LDA SCREEN.MO_SPRITE.DATA,Y ;lookup map object record index of selected target		
	STA SELECT_TARGET.LAST_TARGET.DB+$1,X

			;INC TROUBLESHOOTING.HOOK
		
;**FALLS THROUGH**
	
.EXIT

				
			; lda #$aa
			; ldx #SELECT_TARGET.LAST_TARGET.DB
			; ldy /SELECT_TARGET.LAST_TARGET.DB
			; jsr prep.brk
			; brk
		
;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX
	
	RTS
@END

.SELECT_TARGET.FIND.NEXT_TARGET
@START

				
.INIT2
	INC SELECT_TARGET.ITERATION.COUNTER ;if there is a last target on file, used to prevent using last target after arrow keys have been pressed so that player is in control of the target selection
	
	LDA SELECT_TARGET.CURRENT_TARGET.X
	STA SELECT_TARGET.SEARCH_POSITION.X

	LDA SELECT_TARGET.CURRENT_TARGET.Y
	STA SELECT_TARGET.SEARCH_POSITION.Y
	
	
.TARGET.SEARCH_LOOP

.PARSE.KEY_PRESS
	LDA SELECT_TARGET.LAST_KEY ;the current player keypress being processed	
	CMP #$95			;RIGHT ARROW
	BEQ .ITERATE.LEFT_RIGHT
	CMP #$88			;LEFT ARROW
	BEQ	.ITERATE.LEFT_RIGHT	
	;default (assume up/down arrow was pressed)
	JMP .ITERATE.UP_DOWN
	
	; CMP #$8A			;DOWN ARROW
	; BEQ	.ITERATE.UP_DOWN_STEP		
	; CMP #$8B			;UP ARROW
	; BEQ	.ITERATE.UP_DOWN_STEP	

.ITERATE.LEFT_RIGHT

.INCREMENT.SEARCH_POSITION
;SCREEN EDGE CHECKS

	;ACC = SELECT_TARGET.LAST_KEY ;the current player keypress being processed
	CMP #$95			;RIGHT ARROW
	BEQ .CHECK.RIGHT_EDGE
	;if last key wasn't right arrow, assume it was left arrow
	
	;**FALLS THROUGH**
	
.CHECK.LEFT_EDGE
	LDA SELECT_TARGET.SEARCH_POSITION.X
	BNE .INCREMENT.LEFT ;branch if current target isn't in 1st column

	;in first column
	LDA SELECT_TARGET.SEARCH_POSITION.Y
	BEQ .INCREMENT.LEFT.IN.FIRST.ROW ;branch if current target is in 1st row
	
	LDA #COMBAT.SCREEN.COLUMN.LAST
	STA SELECT_TARGET.SEARCH_POSITION.X ;move search position to right edge of row
	DEC SELECT_TARGET.SEARCH_POSITION.Y ;move search position up one row
	JMP .INCREMENT.SEARCH_POSITION.COMPLETE
	
.INCREMENT.LEFT.IN.FIRST.ROW ;and in first column
;search position is in upper left corner of the screen. move it to lower right corner of screen
	LDA #COMBAT.SCREEN.COLUMN.LAST
	STA SELECT_TARGET.SEARCH_POSITION.X ;move search position to right edge of row
	LDA #COMBAT.SCREEN.ROW.LAST
	STA SELECT_TARGET.SEARCH_POSITION.Y ;move search position to last row
	JMP .INCREMENT.SEARCH_POSITION.COMPLETE

	
;.CHECK.LEFT_EDGE.COMPLETE
.INCREMENT.LEFT
	DEC SELECT_TARGET.SEARCH_POSITION.X
	JMP .INCREMENT.SEARCH_POSITION.COMPLETE

.CHECK.RIGHT_EDGE
	;check right edge
	LDA SELECT_TARGET.SEARCH_POSITION.X
	CMP #COMBAT.SCREEN.COLUMN.LAST
	BNE .INCREMENT.RIGHT ;branch if current target isn't in last column

	;in last column
	LDA SELECT_TARGET.SEARCH_POSITION.Y
	CMP #COMBAT.SCREEN.ROW.LAST
	BEQ .INCREMENT.RIGHT.IN.LAST.ROW ;branch if current target is in last row
	
	LDA #$00
	STA SELECT_TARGET.SEARCH_POSITION.X ;move search position to left edge of row
	INC SELECT_TARGET.SEARCH_POSITION.Y ;move search position down one row
	JMP .INCREMENT.SEARCH_POSITION.COMPLETE
	
.INCREMENT.RIGHT.IN.LAST.ROW ;and in last column
;search position is in lower right corner of the screen. move it to upper left corner of screen
	LDA #$00
	STA SELECT_TARGET.SEARCH_POSITION.X ;move search position to left edge of row
	;ACC = $00
	STA SELECT_TARGET.SEARCH_POSITION.Y ;move search position to first row
	JMP .INCREMENT.SEARCH_POSITION.COMPLETE

;.CHECK.RIGHT_EDGE.COMPLETE
.INCREMENT.RIGHT	
	INC SELECT_TARGET.SEARCH_POSITION.X
	
			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; LDA #$AC
			; LDX SELECT_TARGET.SEARCH_POSITION.X
			; ;LDX SELECT_TARGET.CURRENT_TARGET.X
			
			; LDY SELECT_TARGET.SEARCH_POSITION.Y
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP

	;INC TROUBLESHOOTING.HOOK

	
	;**FALLS THROUGH**
	
.INCREMENT.SEARCH_POSITION.COMPLETE

	JSR .EXAMINE.SEARCH_POSITION
		;ACC = return value ($00 = not found, $01 = found)

	CMP #$01 ;was an S_ENTITY found at the search position?
	BNE .TARGET.SEARCH_LOOP ;if no, continue the search loop. Note: don't branch directly to .ITERATE.LEFT_RIGHT because this routine expects the ACC to be set by the keypress parse code just before it.
	JMP .NEXT_TARGET.FOUND ;if yes, then exit and return target
	
	
.ITERATE.UP_DOWN
;Note: the general approach is to move up/down 1 row, then iterate left until the left edge is reached, then iterate right until the right edge is reached
;once the right edge is reached, move up/down another row.
;if multiple S_ENTITIES are found in the same row then return the one closest to the last target in tile distance. 

;INIT


.NEXT.ROW
;MOVE UP/DOWN 1 ROW

	LDA #$00 ;set initial iteration direction to left
	STA SELECT_TARGET.EXAMINE.ITERATION.DIRECTION ;($00 = iterate left, $01 = right)
	;ACC = #$00
	STA SELECT_TARGET.EXAMINE.S_ENTITY.FOUND ;($00 = not found, $01 = found)

	LDA #$FF ;init to #$FF so that any target will be closer
	STA SELECT_TARGET.PROSPECTIVE.S_ENTITY.DISTANCE
	
	LDA SELECT_TARGET.LAST_KEY ;the current player keypress being processed
	CMP #$8B			;UP ARROW
	BEQ .MOVE_UP.ROW
	;if last key wasn't up arrow, assume it was down arrow

.MOVE_DOWN.ROW 
	LDA SELECT_TARGET.SEARCH_POSITION.Y
	CMP #COMBAT.SCREEN.ROW.LAST
	BEQ .MOVE_DOWN.IN.LAST.ROW ;branch if current position is in last row

	INC SELECT_TARGET.SEARCH_POSITION.Y
	JMP .UP_DOWN.SEARCH_LOOP

.MOVE_DOWN.IN.LAST.ROW
	LDA #$00
	STA SELECT_TARGET.SEARCH_POSITION.Y ;move search position to first row
	JMP .UP_DOWN.SEARCH_LOOP

	
.MOVE_UP.ROW
	LDA SELECT_TARGET.SEARCH_POSITION.Y
	BEQ .MOVE_UP.IN.FIRST.ROW ;branch if current position is in 1st row

	DEC SELECT_TARGET.SEARCH_POSITION.Y
	JMP .UP_DOWN.SEARCH_LOOP
	
.MOVE_UP.IN.FIRST.ROW
	LDA #COMBAT.SCREEN.ROW.LAST
	STA SELECT_TARGET.SEARCH_POSITION.Y ;move search position to last row
	
	;**FALLS THROUGH**

.UP_DOWN.SEARCH_LOOP
	
.EXAMINE.CURRENT.POSITION
;examine the first position in the current row

	JSR .EXAMINE.SEARCH_POSITION
		;ACC = return value ($00 = not found, $01 = found)

	CMP #$01 ;was an S_ENTITY found at the search position?
	BNE .CHECK.ITERATION.DIRECTION ;if no, continue the search
			;if yes, is distance less than next best prospective target in current row

	;GET DISTANCE TO PROSPECTIVE TARGET			
		LDA SELECT_TARGET.CURRENT_TARGET.X ;load last target GMAP.X
		STA PARM1.GMAP.X
		LDA SELECT_TARGET.CURRENT_TARGET.Y ;load last target GMAP.Y
		STA PARM1.GMAP.Y

		LDA SELECT_TARGET.SEARCH_POSITION.X ;load current search position GMAP.X
		STA PARM2.GMAP.X
		LDA SELECT_TARGET.SEARCH_POSITION.Y ;load current search position GMAP.Y
		STA PARM2.GMAP.Y	
	JSR CALCULATE.DISTANCE
		;ACC = tile distance, ignoring obstacles
		CMP SELECT_TARGET.PROSPECTIVE.S_ENTITY.DISTANCE ;is distance of new target found less than distance of best prospective target found so far in the current row?
		BCS .CHECK.ITERATION.DIRECTION ;if no, continue the search
		
		;save new target found as the best prospective target so far
		STA SELECT_TARGET.PROSPECTIVE.S_ENTITY.DISTANCE
		
		LDA SELECT_TARGET.SEARCH_POSITION.X ;load current search position GMAP.X
		STA SELECT_TARGET.PROSPECTIVE.S_ENTITY.X
		LDA SELECT_TARGET.SEARCH_POSITION.Y ;load current search position GMAP.Y
		STA SELECT_TARGET.PROSPECTIVE.S_ENTITY.Y

		;set flag indicating an S_ENTITY has been found in the current row
		LDA #$01
		STA SELECT_TARGET.EXAMINE.S_ENTITY.FOUND

		;**FALLS THROUGH**
	
.CHECK.ITERATION.DIRECTION

	LDA SELECT_TARGET.EXAMINE.ITERATION.DIRECTION ;($00 = iterate left, $01 = iterate right)
	BNE .ITERATE.CURRENT_ROW.RIGHT ;branch if iteration direction is right
	
	;**FALLS THROUGH**
	
.ITERATE.CURRENT_ROW.LEFT
	LDA SELECT_TARGET.SEARCH_POSITION.X
	BEQ .ITERATE.CURRENT_ROW.RIGHT ;branch if current search position is in first column

	DEC SELECT_TARGET.SEARCH_POSITION.X
	JMP .UP_DOWN.SEARCH_LOOP
	
.ITERATE.CURRENT_ROW.RIGHT
	LDA #$01 ;change interation direction to right. it only needs to be changed once but once it is set it doesn't hurt to reset the flag, which takes less memory. 
	STA SELECT_TARGET.EXAMINE.ITERATION.DIRECTION ;($00 = iterate left, $01 = iterate right)

	;check right edge (note: stop search once right edge is reached)
	LDA SELECT_TARGET.SEARCH_POSITION.X
	CMP #COMBAT.SCREEN.COLUMN.LAST
	BEQ .UP_DOWN.EXIT_TEST ;branch if current search position is in last column

	INC SELECT_TARGET.SEARCH_POSITION.X
	JMP .UP_DOWN.SEARCH_LOOP

.NEXT.ROW_STEP
	JMP .NEXT.ROW
	
.UP_DOWN.EXIT_TEST
	;was an S_ENTITY found in the current row?
	

			
	LDA SELECT_TARGET.EXAMINE.S_ENTITY.FOUND
	BEQ .NEXT.ROW_STEP ;branch if no S_ENTITY has been found yet.
	
	;NEXT TARGET FOUND
	LDA SELECT_TARGET.PROSPECTIVE.S_ENTITY.X
	STA SELECT_TARGET.SEARCH_POSITION.X
	
	LDA SELECT_TARGET.PROSPECTIVE.S_ENTITY.Y
	STA SELECT_TARGET.SEARCH_POSITION.Y


			; ;lda #$aa
			; LDA SELECT_TARGET.EXAMINE.S_ENTITY.FOUND
			; LDx SELECT_TARGET.PROSPECTIVE.S_ENTITY.X

			; LDy SELECT_TARGET.PROSPECTIVE.S_ENTITY.Y
			; jsr prep.brk
			; brk			
			
	;**FALLS THROUGH**

;------


.NEXT_TARGET.FOUND
;***THIS SHOULD GO SOMEWHERE
	LDA SELECT_TARGET.SEARCH_POSITION.X
	STA SELECT_TARGET.CURRENT_TARGET.X
	;STA PARM.GMAP.X ;prepares for .LOOKUP.SBYTE
	
	LDA SELECT_TARGET.SEARCH_POSITION.Y
	STA SELECT_TARGET.CURRENT_TARGET.Y
	;STA PARM.GMAP.Y ;prepares for .LOOKUP.SBYTE
	
		
.FIND.NEXT_TARGET.EXIT

		; LDA #$AC
		; ;LDX SELECT_TARGET.SEARCH_POSITION.X
		; LDX SELECT_TARGET.CURRENT_TARGET.X
		
		; ;LDY SELECT_TARGET.SEARCH_POSITION.Y
		; LDY SELECT_TARGET.CURRENT_TARGET.Y
		; JSR PREP.BRK
		; BRK
	
	RTS
	
@END

.EXAMINE.SEARCH_POSITION ;search for target
@START	
;PARAMETERS: SELECT_TARGET.SEARCH_POSITION.X, SELECT_TARGET.SEARCH_POSITION.Y
;ENTRANCE: direct
;RETURN: ACC = ($00 = not found, $01 = found)


	;lookup screen index of search position
		LDA SELECT_TARGET.SEARCH_POSITION.X	
		STA PARM.GMAP.X
		LDA SELECT_TARGET.SEARCH_POSITION.Y 
		STA PARM.GMAP.Y
	JSR CONVERT.COLUMN_ROW.TO.SINDEX
		LDY RETURN.SCREEN_ARRAY_INDEX ;used below

	;is an S_ENTITY location at the search position?
	LDA SCREEN.MO_SPRITE_TYPE.DATA,Y
	;mask-out of MTT flag bits not necessary. 
	CMP #$FF
	BNE .PROSPECTIVE.TARGET.FOUND ;if yes, continue to search for an S_ENTITY
	;**FALLS THROUGH**
.TARGET.NOT_FOUND	
	LDA #$00 ;set return value ($00 = not found, $01 = found)
	JMP .EXAMINE.EXIT ;if no, continue to search for an S_ENTITY

.PROSPECTIVE.TARGET.FOUND 		
		; sta temp
		; cpy #$48
		; bne .temp
		; LDA SCREEN.MO_SPRITE_TYPE.DATA,Y
		; ldx MAP_OBJECT.RECORD.READ+$3
		; ldy #$ac
		; jsr prep.brk
		; brk
; .temp
		; lda temp
		
		
		
	;filter out PC targets unless PC's are valid target's
	LDA SCREEN.MO_SPRITE_TYPE.DATA,Y
	AND #$87 ;mask-out MTT flags (bits 3-6)
	CMP #S_ENTITY_TYPE.PC
	BNE .RADIUS_CHECK.ALTERNATE_ENTRANCE ;branch if prospective target is not a PC
	
	;filter out the PC attacker
	CPY COMBAT.PC.ACTIVE.SINDEX
	BEQ .TARGET.NOT_FOUND ;branch if current search postion is the attacker PC
	
	
	LDA SELECT_TARGET.PC_VALID.FLAG ;($00 = off, PCs not valid targets | $01 = on, PCs are valid targets)
	BEQ .TARGET.NOT_FOUND ;branch if pc's are not valid targets
	
	;**FALLS THROUGH**
	
.RADIUS_CHECK.ALTERNATE_ENTRANCE ;enter here to just check weapon radius against target distance	
;(verify distance to prospective target doesn't exceed the radius of the attacker's weapon)
;*to use this entrance Y-REG must be set to the sindex of prospective target

	;is S_ENTITY a tile of a multi-tile mob other than the index tile (upper left tile)?
	;(the upper left tile is the only tile of a multi-tile mob that is a valid character sheet index)
		;YREG = screen index of current search position
	JSR COMBAT.TARGET.SEARCH.MTT.CHECK
		;RETURN VALUE: ACC = ($00 =  MTT index tile found | $01 = MTT not found at this screen location | $02 = MTT tile found other than index tile)				
		STA SELECT_TARGET.MTT.PRESENT ;($00 =  MTT index tile found | $01 = MTT not found at this screen location | $02 = MTT tile found other than index tile)
		CMP #$02 ;S_ENTITY found a non-index tile of an MTT S_ENTTY? (i.e. a tile other than the upper left of the MTT)
		BEQ	.TARGET.NOT_FOUND ;skip this S_ENTITY

	;GET DISTANCE BETWEEN ARRACKTER AND PROSPECTIVE TARGET			
		LDX COMBAT.PC.ACTIVE.RECORD
		
		LDA COMBAT.MAP_OBJECTS.PC+$0,X ;load active player GMAP.X
		STA PARM1.GMAP.X
		LDA COMBAT.MAP_OBJECTS.PC+$1,X ;load active player GMAP.Y
		STA PARM1.GMAP.Y

		LDA SELECT_TARGET.SEARCH_POSITION.X ;load current search position GMAP.X
		STA PARM2.GMAP.X
		LDA SELECT_TARGET.SEARCH_POSITION.Y ;load current search position GMAP.Y
		STA PARM2.GMAP.Y	
	JSR CALCULATE.DISTANCE
		;ACC = tile distance, ignoring obstacles
		;RETURN MAP_OBJECTS.X_ADJ = X-AXIS differential
		;RETURN MAP_OBJECTS.Y_ADJ = Y-AXIS differential

		;the following parameters were all set above
		;PARM1.GMAP.X, PARM2.GMAP.X, PARM1.GMAP.Y, PARM2.GMAP.Y, MAP_OBJECTS.X_ADJ, MAP_OBJECTS.Y_ADJ, SELECT_TARGET.RADIUS
	JSR COMBAT.CHECK.WEAPON.RADIUS
		;RETURN VALUE: ($00 = target distance exceeds radius, $01 = target distance <= weapon radius)
		CMP #$00 ;does target distance exceed radius?
		BEQ .DISTANCE.EXCEEDS.RADIUS ;branch if yes
		
	;**FALLS THROUGH**

.DISTANCE.WITHIN.RADIUS
	LDA #$01 ;set return value ($00 = target distance exceeds radius, $01 = target distance <= weapon radius)
	JMP .EXAMINE.EXIT
	
.DISTANCE.EXCEEDS.RADIUS
	LDA #$00 ;set return value ($00 = target distance exceeds radius, $01 = target distance <= weapon radius)

.EXAMINE.EXIT	
	;ACC = return value ($00 = not found, $01 = found)

	RTS
	
; .TARGET.NOT_FOUND.STEP
	; JMP .TARGET.NOT_FOUND
	
@END
	
.SELECT_TARGET.GET.CLOSEST
@START

			
;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA
	
;INIT
	LDA #$FF
	STA COMBAT.CHASE.TARGET.X	
	STA COMBAT.CHASE.TARGET.Y
	
			;***RENAME TO SOMETHING ELSE, THIS IS THE SINDEX OF THE TARGET
	STA COMBAT.ACQUIRE.TARGET.FINAL.SINDEX ;**OPT** Memory. Might not be needed since this variable is init at the start of COMBAT.ACQUIRE.TARGET

	
.GET.CLOSEST.TARGET
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
	JSR COMBAT.ACQUIRE.TARGET
		;return: COMBAT.ACQUIRE.TARGET.FINAL.SINDEX

			
	;was a mob aligned (including specials) target found?
	LDA COMBAT.ACQUIRE.TARGET.FINAL.SINDEX		
	CMP #$FF ;is init value still set?
	BNE .GET.CLOSEST.TARGET.COMPLETE ;if no, then target was found
	
	;no target found, unless PC's are valid target's
	LDA SELECT_TARGET.PC_VALID.FLAG ;($00 = off, PCs not valid targets | $01 = on, PCs are valid targets)
	BEQ .NO_TARGET_FOUND ;branch if pc's are not valid targets
	

	;check for PC targets
	;(PCs are allowed to attack other PCs)
		LDA #COMBAT.ENEMY_TYPE.ALL
		STA SPRITE.RECORD+$5 ;set enemy type to "ALL" so that PC 
	JSR COMBAT.ACQUIRE.TARGET
		;return: COMBAT.ACQUIRE.TARGET.FINAL.SINDEX

			; JSR KEYIN
			; LDA #$AA
			; LDX COMBAT.ACQUIRE.TARGET.FINAL.SINDEX
			; JSR PREP.BRK
			; BRK
			
	
	
	;was any target found?
	LDA COMBAT.ACQUIRE.TARGET.FINAL.SINDEX
	CMP #$FF ;is init value still set?
	BNE .GET.CLOSEST.TARGET.COMPLETE ;if no, then target was found

	;**FALLS THROUGH**
	
.NO_TARGET_FOUND
;.GET.CLOSEST.TARGET reports that no S_ENTIES are left on the battlefield except the 
;active player.

;future: if the PCs decide to battle it out, this could happen. 
;Thus, instead of an error trap this condition should probaby
;set the target to the active player or just not allow a target to be set. 
	
;in theory something like the following should prevent a target from being set and return control to the player
;this code was what I used in the original target selection routine when no targets were found. 
;
	LDA #$01
	STA COMBAT.ABORT.FLAG ;($00 = not set, $01 = set)	
	;JMP .EXIT

	
	; ;LDA #$EE
	; JSR PREP.BRK
	; BRK
	
.GET.CLOSEST.TARGET.COMPLETE


.GET.CLOSEST.EXIT
;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX
	
	RTS
	
@END


COMBAT.TARGET.SEARCH.MTT.CHECK
@START
;PARAMETERS:  Y-REG: screen index of location to do MTT evaluation
;ENTRANCE: various identify target routines
;RETURN: ACC = ($00 =  MTT index tile found | $01 = MTT not found at this screen location | $02 = MTT tile found other than index tile)


;=====================SUBROUTINE DOCUMENTATION====================================
;
;For MTT S_ENTITY, the S_ENTITY type code stored in SCREEN.MO_SPRITE_TYPE.DATA
;contains a bit flag (bits 3-6) which identifies the position of the tile within the
;MTT S_ENTIT (i.e. upper left, upper right etc). 
;
;See ";===BIT FLAG MATRIX===" in MAP_OBJECT.ADD.MTT_FLAG_BITS in the subroutine documentation
;for more details on these bit flags. 
;
;=================================================================================


	; LDA #$01
	; STA TROUBLESHOOTING.HOOK


			
;IS MOB MULTI-TILE (MTT)?

	;read map object record of S_ENTITY at current search position
		;read $8 bytes
		LDA #$08 ;(high bit not set = lookup by screen index)
		;Y-REG (screen array index | map object record index)
	JSR READ.MAP_OBJECT.ARRAY
		;RETURN VALUE = MAP_OBJECT.RECORD.READ(8)

	
			
	LDA #$08 ;the AND mask value needed for the BIT operation below. 
	BIT MAP_OBJECT.RECORD.READ+$3 ;test bit3 of MAP_OBJECT.RECORD.READ+$03
	BEQ .MTT.NOT_FOUND ;branch if bit3 is not set (mob is not multi-tile)

	;does the current search position contain the index tile (upper left) of the MTT S_ENTITY?
	LDA SCREEN.MO_SPRITE_TYPE.DATA,Y
	STA TEMP
	BIT TEMP
	BVC .MTT.INDEX_TILE.NOT_FOUND ;branch if no (bit6 not set). MTT index tile not found
	;else: index tile found
.MTT_CHECK.DONE

	;**FALLS THROUGH**
	
.MTT.INDEX_TILE.FOUND
	LDA #$00 ;set return value: ($00 =  MTT index tile found | $01 = MTT not found at this screen location | $02 = MTT tile found other than index tile)
	JMP .EXIT

.MTT.NOT_FOUND
	LDA #$01 ;set return value: ($00 =  MTT index tile found | $01 = MTT not found at this screen location | $02 = MTT tile found other than index tile)
	JMP .EXIT
	
.MTT.INDEX_TILE.NOT_FOUND
	LDA #$02 ;set return value: ($00 =  MTT index tile found | $01 = MTT not found at this screen location | $02 = MTT tile found other than index tile)


.EXIT
	;ACC = RETURN VALUE: ($00 =  MTT index tile found | $01 = MTT not found at this screen location | $02 = MTT tile found other than index tile)

	RTS
@END

COMBAT.CHECK.WEAPON.RADIUS
@START
;PARAMETERS: PARM1.GMAP.X, PARM2.GMAP.X, PARM1.GMAP.Y, PARM2.GMAP.Y, MAP_OBJECTS.X_ADJ, MAP_OBJECTS.Y_ADJ, SELECT_TARGET.RADIUS, SELECT_TARGET.MTT.PRESENT
;ENTRANCE: DIRECT
;RETURN: ACC = ;set return value ($00 = not found, $01 = found)

;=====================CODE-SECTION DOCUMENTATION====================================
;
;Test the weapon radius against the relative position of the PC attacker and S_ENTITY 
;defender which is located in the current search position.
;
;If the X-AXIS AND Y-AXIS differential in the relative positions are less or equal to the 
;weapon radius then the prospective target is within the weapon's radius.
;
;The only exception is in the case of a multi-tile S_ENTITY in the current search position. 
;Because only the index tile (upper left) of the MTT is treated as a valid target, there are some
;relative positions where a distance of 2 is acceptable for a weapon with a radius of 1 because on 
;the video screen one of more of the non-index tiles of the multi-tile mob appears 1 tile away from 
;the player. 
;
;The 2nd test for X-AXIS and Y-AXIS is
;designed to treat a distance of 2 as being with a melee radius (1):
;
;M = MTT S_ENTITY, non index tiles
;I = MTT S_ENTITY, index tiles
;2,1 = distance from index tile
;
;1112
;1IM2
;1MM2
;2222
;
;
;
;=================================================================================

.X_AXIS.TEST
		
		LDA MAP_OBJECTS.X_ADJ ;**OPT** Memory. Speed. Setup with <= test
		CMP SELECT_TARGET.RADIUS
		;not valid target if distance is greater than radius
		BEQ .Y_AXIS.TEST ;branch if test passed. if distance = radius
		BCC .X_AXIS.TEST.DONE ;branch if test passed. implicit less than test (if distance is less than radius).
		
		;1st X-AXIS test fails
		;(if MTT present that apply different rules)
		LDA SELECT_TARGET.MTT.PRESENT ;($00 =  MTT index tile found | $01 = MTT not found at this screen location | $02 = MTT tile found other than index tile)
		BNE .DISTANCE.EXCEEDS.RADIUS ;branch if no MTT present. In this case the failure of the 1st X-AXIS test is final
		
		LDA PARM2.GMAP.X ;load X-AXIS of current search position. 
		STA TEMP
		DEC TEMP ;X-AXIS -1 (of search position)
		

			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; CPY #$6B
			; BNE .TEMP
			; LDA #$AB
			; LDx SELECT_TARGET.MTT.PRESENT ;($00 =  MTT index tile found | $01 = MTT not found at this screen location | $02 = MTT tile found other than index tile)
			; LDY TEMP
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP
			
				; lda #$ab
				; ldx SELECT_TARGET.MTT.PRESENT ;($00 = MTT s_entity not present at current search position | $01 = MTT s_entity not present at current search position)
				; jsr prep.brk
				; brk
				
		LDA PARM1.GMAP.X ;load PC X-AXIS
		CMP TEMP ;load prospective target X-AXIS-1
		BCC .DISTANCE.EXCEEDS.RADIUS ;In this case the failure of the 1st X-AXIS test is final
		;2nd X-AXIS test. check radius again, with +1 added to account for multi-tile mob's non-index tiles
		LDA SELECT_TARGET.RADIUS
		STA TEMP
		INC TEMP
		LDA MAP_OBJECTS.X_ADJ ;**OPT** Memory. Speed. Setup with <= test
		CMP TEMP
		BEQ .Y_AXIS.TEST ;branch if test passed. if distance = radius
		BCS .DISTANCE.EXCEEDS.RADIUS ;branch if test fails. implicit greater than test (if distance is greater than radius).
		;else: distance < radius
		;**OPT** Memory. Consolidate 1st and 2nd X/Y-AXIS test with weapon radius as the parameters and a pass/fail return value. 
.X_AXIS.TEST.DONE
		
	;**FALLS THROUGH**	

.Y_AXIS.TEST
		LDA MAP_OBJECTS.Y_ADJ
		CMP SELECT_TARGET.RADIUS
		;not valid target if distance is greater than radius
		BEQ .DISTANCE.WITHIN.RADIUS ;if distance = radius
		BCC .Y_AXIS.TEST.DONE ;branch if test passed. implicit less than test (if distance is less than radius).
		
		;1st Y-AXIS test fails
		;(if MTT present that apply different rules)
		LDA SELECT_TARGET.MTT.PRESENT ;($00 =  MTT index tile found | $01 = MTT not found at this screen location | $02 = MTT tile found other than index tile)
		BNE .DISTANCE.EXCEEDS.RADIUS ;branch if no MTT present. In this case the failure of the 1st Y-AXIS test is final

		LDA PARM2.GMAP.Y ;load Y-AXIS of current search position. 
		STA TEMP
		DEC TEMP ;Y-AXIS -1 (of search position)



			; STA TEMP
			; ; LDA TROUBLESHOOTING.HOOK
			; ; CMP #$01
			; CPY #$6B
			; BNE .TEMP2
			; LDA #$AC
			; ;LDA SELECT_TARGET.SEARCH_POSITION.Y
			; LDx SELECT_TARGET.MTT.PRESENT ;($00 =  MTT index tile found | $01 = MTT not found at this screen location | $02 = MTT tile found other than index tile)
			; LDY TEMP
			; JSR PREP.BRK
			; BRK
; .TEMP2
			; LDA TEMP
			
				; lda #$ac
				; LDx SELECT_TARGET.MTT.PRESENT ;($00 =  MTT index tile found | $01 = MTT not found at this screen location | $02 = MTT tile found other than index tile)
				; jsr prep.brk
				; brk
				
		LDA PARM1.GMAP.Y ;load PC Y-AXIS
		CMP TEMP ;load prospective target Y-AXIS-1
		BCC .DISTANCE.EXCEEDS.RADIUS ;In this case the failure of the 1st Y-AXIS test is final

		;2nd Y-AXIS test. check radius again, with +1 added to account for multi-tile mob's non-index tiles
		LDA SELECT_TARGET.RADIUS
		STA TEMP
		INC TEMP
		LDA MAP_OBJECTS.Y_ADJ ;**OPT** Memory. Speed. Setup with <= test
		CMP TEMP
		BEQ .Y_AXIS.TEST.DONE ;branch if test passed. if distance = radius
		BCS .DISTANCE.EXCEEDS.RADIUS ;branch if test fails. implicit greater than test (if distance is greater than radius).
		;else: distance < radius
		;**OPT** Memory. Consolidate 1st and 2nd X/Y-AXIS test with weapon radius as the parameters and a pass/fail return value. 
	
.Y_AXIS.TEST.DONE

.DISTANCE.WITHIN.RADIUS
	LDA #$01 ;set return value ($00 = target distance exceeds radius, $01 = target distance <= weapon radius)
	JMP .EXIT
	
.DISTANCE.EXCEEDS.RADIUS
	LDA #$00 ;set return value ($00 = target distance exceeds radius, $01 = target distance <= weapon radius)

.EXIT

	RTS
	
@END

;COMBAT.TARGET.SEARCH.MTT.CHECK
@START
; ;PARAMETERS:  Y-REG: screen index of location to do MTT evaluation, MAP_OBJECT.RECORD.READ(8) = data for S_ENTITY found at current screen location
; ;ENTRANCE: various identify target routines
; ;RETURN: ACC = ($00 =  MTT index tile found | $01 = MTT not found at this screen location | $02 = MTT tile found other than index tile)

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


			; ; LDA #$01
			; ; STA TROUBLESHOOTING.HOOK
			
			
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
	; LDA #$01    ($00 =  MTT index tile found | $01 = MTT not found at this screen location | $02 = MTT tile found other than index tile)
	; STA .MTT_CHECK.RETURN_VALUE	
	; JMP .TARGET.SEARCH.MTT.CHECK.EXIT
	
; .MTT_CHECK.MTT_INDEX_TILE_FOUND	
; .MTT_CHECK.MTT_NOT_FOUND	
	; LDA #$00    ($00 =  MTT index tile found | $01 = MTT not found at this screen location | $02 = MTT tile found other than index tile)
	; STA .MTT_CHECK.RETURN_VALUE
	
	; ;**FALLS THROUGH**
	
; .TARGET.SEARCH.MTT.CHECK.EXIT

; ;RESTORE REGISTERS
	; PLA
	; TAY
	; ; PLA
	; ; TAX
	

	; LDA .MTT_CHECK.RETURN_VALUE ;load return value. ($00 =  MTT index tile found | $01 = MTT not found at this screen location | $02 = MTT tile found other than index tile)
	
	; RTS
	
;--------

;LOCAL VARIABLES
@START
; .EXCLUSION_LIST.RECORD.SIZE		.EQ $5 ;#CONSTANT. byte $00 = MTT S_ENTITY type code, byte $01 = MTT MO index, $02-$04 = sindex of 3 exclusion tiles (upper right, lower left, and lower right tiles of the MTT)
; .EXCLUSION_LIST.ARRAY.SIZE		.EQ $50

; .TARGET.SEARCH.MTT_EXCLUSION_LIST	.BS .EXCLUSION_LIST.ARRAY.SIZE ;supports !16 MTT mobs

;TARGET.SEARCH.MTT_EXCLUSION_LIST.INDEX	.BS $1

; .MTT_CHECK.S_ENTITY.TYPE	.BS $1
; .MTT_CHECK.MO_INDEX 		.BS $1	
; .MTT_CHECK.RETURN_VALUE		.BS $1

@END
	
@END

@END

COMBAT.ACQUIRE.TARGET  ;(MOB/SPECIAL chase targets). Must always be in memory during combat because it is called by COMBAT.SELECT.ATTACK_TARGET
@START
;PARAMETERS: *SPRITE.RECORD+$5, *SPRITE.RECORD+$8, *SPRITE.RECORD+$9, *SPRITE.RECORD+$A, *SPRITE.RECORD+$B, [COMBAT.ACQUIRE.RETURN_VALUE.PARM]
;RETURN: COMBAT.CHASE.TARGET.X, COMBAT.CHASE.TARGET.Y, COMBAT.ACQUIRE.TARGET.FINAL.SINDEX (sindex), COMBAT.CHASE.TARGET.DISTANCE			
;RETURN: MOB.POSITION.X_LT*, MOB.POSITION.X_GR*, MOB.POSITION.Y_LT*, MOB.POSITION.Y_GR*
;RETURN: COMBAT.ACQUIRE.FURTHEST_TARGET.FINAL.SINDEX**, COMBAT.FURTHEST_TARGET.X, COMBAT.FURTHEST_TARGET.Y, COMBAT.CHASE.FURTHEST_TARGET.DISTANCE
;RETURN: COMBAT.TARGET_HIT.DB***
;
;*X_LT/X_GR = $00, then X-axis is equal. If either are set to $01 then true (i.e. if X_LT = $01, then X-AXIS is less than. if X_GR = $01, then X-AXIS is greater than) 
;*Y_LT/Y_GR = $00, then Y-axis is equal. If either are set to $01 then true (i.e. if Y_LT = $01, then Y-AXIS is less than) 
;
;*set with the attacker's map object record data for these fields
;**set to $FF if only 1 target is found. In that even, the target data will be in the variables for the closest target (the first line of return variables listed above)
;***this array is populated with all S_ENTITIES on the screen which are not aligned with the attacker, but only if COMBAT.ACQUIRE.RETURN_VALUE.PARM is set.  
;


;=====================SUBROUTINE DOCUMENTATION====================================
;Note: this routine must always be in memory during combat because it is called by COMBAT.SELECT.ATTACK_TARGET
;
;This subroutine is used to identify a target for MOBS and SPECIALS (melee/range/spell attacks). It is called from .MOB.ACQUIRE.TARGET (COMBAT.MOVE.MANAGER)
;and, on PCs turn, also to identify the closest target to the PC for purpose of initializing the target selector routine (COMBAT.SELECT.ATTACK_TARGET). After the
;PC target selector routine is initialized it no longer needs to call this routine because it cycles through the targets in response to the player pressing the arrow keys
;(it just finds the next closest target in the direction of the arrow key and it has its own code for doing that)
;
;
;----Terms Used (in code comments)
;Active S_ENTITY 	= the S_ENTITY for whom we are acquring a target (the attacker). SPRITE.RECORD contains the map_object data for this S_ENTITY. 
;Found S_ENTITY / Enemy S_ENTITY Found 	= the S_ENTITY at the current screen location being examined by the screen array loop which matches the attacker's enemy type code
;Valid Target S_ENTITY	= a Found S_ENTITY which the algorithm has concluded is a valid target for the Active S_ENTITY
;Current Valid Target S_ENTITY	= a Valid Target identified in the current iteration of the main loop (SCREEN.ARRAY.LOOP)
;Prospective Target = the best target found in all iterations of the main loop performed so far. If a better target is found in a subsequent iteration, the Prospective Target is replaced.
;Final Target		= the Prospective Target when the loop terminates, with it's data stored in COMBAT.CHASE.TARGET.<all>
;
;=================================================================================




;----QUICK REFERENCE INFO---
;
; S_ENTITY_TYPE.NC_MOB	.EQ $00	;#CONSTANT: non-combat mob code for use with SCREEN.MO_SPRITE_TYPE.DATA
; S_ENTITY_TYPE.C_MOB		.EQ $01	;#CONSTANT: combat mob code for use with SCREEN.MO_SPRITE_TYPE.DATA
; S_ENTITY_TYPE.PC		.EQ $02	;#CONSTANT: player characer code for use with SCREEN.MO_SPRITE_TYPE.DATA
; S_ENTITY_TYPE.SPECIAL	.EQ $03	;#CONSTANT: combat "special" S_ENTITY, for use with SCREEN.MO_SPRITE_TYPE.DATA
; S_ENTITY_TYPE.BLD_NPC	.EQ $04	;#CONSTANT: building NPC code for use with SCREEN.MO_SPRITE_TYPE.DATA
; S_ENTITY_TYPE.DNG_NPC	.EQ $05	;#CONSTANT: Dungeon NPC code for use with SCREEN.MO_SPRITE_TYPE.DATA
;
;
; COMBAT.ENEMY_TYPE.MOB		.EQ $01	S_ENTITY_TYPE.C_MOB ;#CONSTANT. Used to set the type of enemy a given S_ENTITY will pursue & attack 
; COMBAT.ENEMY_TYPE.PC		.EQ $02 S_ENTITY_TYPE.PC ;#CONSTANT. Used to set the type of enemy a given S_ENTITY will pursue & attack 
; COMBAT.ENEMY_TYPE.ALL_SPECIAL	.EQ $03 S_ENTITY_TYPE.SPECIAL ;#CONSTANT. Used to set the type of enemy a given S_ENTITY will pursue & attack 
; COMBAT.ENEMY_TYPE.MOB_SPECIAL	.EQ $F1 S_ENTITY_TYPE.SPECIAL ;#CONSTANT. Used to set the type of enemy a given S_ENTITY will pursue & attack 
; COMBAT.ENEMY_TYPE.PC_SPECIAL	.EQ $F2 S_ENTITY_TYPE.SPECIAL ;#CONSTANT. Used to set the type of enemy a given S_ENTITY will pursue & attack 
; COMBAT.ENEMY_TYPE.ALL		.EQ $FF ;#CONSTANT. Used to set the type of enemy a given S_ENTITY will pursue & attack 


; COMBAT.S_ENTITY.STATUS.GOOD			.EQ $00 ;#CONSTANT. 
; COMBAT.S_ENTITY.STATUS.SLEEPING		.EQ $01 ;#CONSTANT. 
; COMBAT.S_ENTITY.STATUS.POSSESSED	.EQ $02 ;#CONSTANT. 
; COMBAT.S_ENTITY.STATUS.DEAD			.EQ $FF ;#CONSTANT. 

	
		
.INIT.VARIABLES
	;init target selection variables
	LDY #$FF ;(init to $FF so that any S_ENTITY found will have a distance lower than the init value) 
	STY COMBAT.CHASE.TARGET.DISTANCE
	STY COMBAT.CHASE.TARGET.HEALTH
	STY COMBAT.ACQUIRE.TARGET.FINAL.SINDEX
	STY COMBAT.ACQUIRE.FURTHEST_TARGET.FINAL.SINDEX
	;init screen array index counter 
	INY ;flip Y-REG to $00 for less clock cyles and memory than LDA.
	;COMBAT.TARGET.X/Y aren't needed as they are reflected in the distance variable.



			
SCREEN.ARRAY.LOOP ;main loop				
	LDA SCREEN.MO_SPRITE_TYPE.DATA,Y ;load type of S_ENTITY map object, if one is present at this screen location
	AND #$87 ;mask-out MTT flags (bits 3-6)
	;mask-out of MTT bits not necessary
	BMI INCREMENT.SCREEN_ARRAY.INDEX  ;test for the value $FF by checking for high-bit set. branch if there is no map object at this screen array location.			
	CMP #S_ENTITY_TYPE.SPECIAL ;is the Found S_ENTITY a SPECIAL?
	BEQ ENEMY.S_ENTITY.FOUND ;alignment/enemy codes need to be compared, but that happens later on because the Found.S_ENTITY map object record data has to be loaded first. 	
	CMP SPRITE.RECORD+$05 ;does the type code of the Found S_ENTITY match the enemy code of the active S_ENITY?
		
		; ; lda COMBAT.TARGET_HIT.DB.INDEX
		; ; sta $be00
		; ;lda #$aa
		; ldx SPRITE.RECORD+$05 
		; ; ldx #COMBAT.TARGET_HIT.DB
		; ; ldy /COMBAT.TARGET_HIT.DB
		; jsr prep.brk
		; brk	
		
	BNE .ENEMY.TYPE_CODE.NO_MATCH
	JMP ENEMY.S_ENTITY.FOUND
.ENEMY.TYPE_CODE.NO_MATCH
	LDA SPRITE.RECORD+$05 ;load active S_ENTITY enemy code
	CMP #COMBAT.ENEMY_TYPE.ALL ;is this S_ENTITY's enemy code set to all?
	BEQ ENEMY.S_ENTITY.FOUND

	;**FALLS THROUGH**
	
INCREMENT.SCREEN_ARRAY.INDEX	

			
			
	; ;reset furthest target branch flag
	; LDA #$00
	; STA COMBAT.ACQUIRE.FURTHEST_TARGET.FLAG ;($00 = furthest target branch not used this iteration | >$01 furthest target branch used on this iteration)

	INY ;increment index
	CPY #SCREEN.ARRAY.LAST_ELEMENT2 ;at end of screen array?
	BNE SCREEN.ARRAY.LOOP
	
	;**FALLS THROUGH**	
	
.SET.FINAL.TARGET
;(all final target variables except for the GR/LT position flags 
;are set in .SAVE.PROSPECTIVE.TARGET. They will automatically reflect the final target values
;because they are not clobbered on each iteration of the target searh loops. The GR/LT position
;variables are clobbered on each iteration because they are set when distance is calculated which must
;be done for each S_ENTITY found). 

				



			
	LDY #$00
	;reset parm to default value. done here for convenience
	STY COMBAT.ACQUIRE.RETURN_VALUE.PARM ;($00 = standard return values | >=$01 = COMBAT.TARGET_HIT.DB(x))

.SET.FINAL.TARGET.LOOP
;This loop does the following:
	; LDA COMBAT.CHASE.TARGET.X_LT
	; STA MOB.POSITION.X_LT
	;
	; LDA COMBAT.CHASE.TARGET.X_GR
	; STA MOB.POSITION.X_GR
	;
	; LDA COMBAT.CHASE.TARGET.Y_LT
	; STA MOB.POSITION.Y_LT
	;
	; LDA COMBAT.CHASE.TARGET.Y_GR
	; STA MOB.POSITION.Y_GR
	
	LDA COMBAT.CHASE.TARGET.X_GR,Y
	STA MOB.POSITION.X_GR,Y
	INY
	CPY #$04
	BNE .SET.FINAL.TARGET.LOOP
	

			
.EXIT


				
	RTS	
	
ENEMY.S_ENTITY.FOUND	


.MTT.FILTER	;filter out non-index tiles of MTT S_ENTITIES (PCs only)
@START
;(Note: technially it is okay for all attackers to attack non-index MTT tiles because the underlying map object index is the same for
;all MTT tiles. The purpose of this filter is cosmetic, so that the PCs don't have to press the arrow key multiple times to get
;the next target)

;PC/MOB TURN BRANCH
	LDA COMBAT.TURN_STATUS   ;($00 = PCs turn. $01 = Special(s) S_ENTITY (example Wyvern Ally, Summoned Demon), $02 = MOBs turn)
	BNE .MTT.FILTER.DONE ;if not PCs turn, then branch

			
		;Y-REG: search loop index
	JSR COMBAT.TARGET.SEARCH.MTT.CHECK
			;RETURN VALUE: ACC = ($00 =  MTT index tile found | $01 = MTT not found at this screen location | $02 = MTT tile found other than index tile)
		STA SELECT_TARGET.MTT.PRESENT ;($00 =  MTT index tile found | $01 = MTT not found at this screen location | $02 = MTT tile found other than index tile)
		CMP #$02 ;is S_ENTITY found a non-index tile of an MTT S_ENTTY? (i.e. a tile other than the upper left of the MTT)
		BEQ	INCREMENT.SCREEN_ARRAY.INDEX	;skip this S_ENTITY

			; STA TEMP
			; ; LDA TROUBLESHOOTING.HOOK
			; ; CMP #$01
			; CPY #$7D
			; BNE .TEMP
			; lda #$ac
			; LDX TEMP
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP
			
.MTT.FILTER.DONE
@END

	
		;**OPT** replace this section with a call to LOOKUP.MAP_OBJECT.ARRAY. Note: there are several instances like this which were not labeled with **OPT**
	LDA SCREEN.MO_SPRITE_TYPE.DATA,Y ;load type of Found S_ENTITY 
	AND #$87 ;mask-out MTT flags (bits 3-6)
	STA COMBAT.TARGET.S_ENTITY_TYPE	 ;we'll need this value several times		
	;branch based on the type of S_ENTITY found
	CMP #S_ENTITY_TYPE.C_MOB
	BEQ	.FOUND.C_MOB 
	CMP #S_ENTITY_TYPE.PC
	BEQ .FOUND.PC
	CMP #S_ENTITY_TYPE.SPECIAL
	BEQ .FOUND.SPECIAL
		
.ERROR.BAD.S_ENTITY_TYPE.CODE
;Invalid S_ENTITY type code found in SCREEN.MO_SPRITE_TYPE.DATA in
;reported by ENEMY.S_ENTITY.FOUND (COMBAT)
		;LDA #$EE
		;LDX #$EE
	JSR PREP.BRK
	BRK

.FOUND.C_MOB ;set pointer to COMBAT.MAP_OBJECTS.MOB
	LDA #COMBAT.MAP_OBJECTS.MOB
	STA COMBAT.MAP_OBJECT.ARRAY.POINTER+$0

	LDA /COMBAT.MAP_OBJECTS.MOB
	STA COMBAT.MAP_OBJECT.ARRAY.POINTER+$1
	
	JMP .EXAMINE.MATCH
	
.FOUND.PC
	LDA #COMBAT.MAP_OBJECTS.PC
	STA COMBAT.MAP_OBJECT.ARRAY.POINTER+$0

	LDA /COMBAT.MAP_OBJECTS.PC
	STA COMBAT.MAP_OBJECT.ARRAY.POINTER+$1
	
	JMP .EXAMINE.MATCH
	
.FOUND.SPECIAL
	LDA #COMBAT.MAP_OBJECTS.SPECIAL.TRUE_INDEX
	STA COMBAT.MAP_OBJECT.ARRAY.POINTER+$0

	LDA /COMBAT.MAP_OBJECTS.SPECIAL.TRUE_INDEX
	STA COMBAT.MAP_OBJECT.ARRAY.POINTER+$1		
	;*FALLS THROUGH**
	;JMP .EXAMINE.MATCH
	

		
.EXAMINE.MATCH
;LOAD RECORD
	;load Found S_ENTITY's map object record data
	STY COMBAT.ACQUIRE.TARGET.SCREEN_ARRAY.INDEX ;save screen array index of Found S_ENTITY
	LDA SCREEN.MO_SPRITE.DATA,Y ;load record index of Found S_ENTITY's map object record index
		
	TAY ;set Y-REG = S_ENTITY record index
	LDX #$00 ;init X-REG for COMBAT.TARGET.RECORD index
		
.LOAD.FOUND_S_ENTITY.LOOP
	LDA (COMBAT.MAP_OBJECT.ARRAY.POINTER),Y ;load next field in Found S_ENTITY's map object record
	STA COMBAT.TARGET.RECORD,X
	INY ;increment index
	INX ;increment index
	CPX #COMBAT.MAP_OBJECTS.MOB.RECORD_SIZE	;at end of array? (note that the special array is the same size. The PC array is smaller so it will end up with random values in the used fields)
	BNE .LOAD.FOUND_S_ENTITY.LOOP

	LDY COMBAT.ACQUIRE.TARGET.SCREEN_ARRAY.INDEX ;restore screen array index
		
	;if Found S_ENTITY is type PC, copy byte $03 to byte $07 in the target record
	;(note: this is because the health status field for the PC record is byte $03 but for all other S_ENTITY types, the health status field is byte $07)
	LDA COMBAT.TARGET.S_ENTITY_TYPE ;load the type of the Found S_ENTITY
	CMP #S_ENTITY_TYPE.PC
	BNE .EXAMINE.MATCH.START
	LDA COMBAT.TARGET.RECORD+$03
	STA COMBAT.TARGET.RECORD+$07
	;**FALLS THROUGH**
	
.EXAMINE.MATCH.START

		; LDA #$DA
		; JSR PREP.BRK
		; BRK
		

.CHECK.ALIGNMENT.IF.SPECIAL
;If the Found S_ENTITY is a special, only target if it is aligned with the enemy target code of the Active S_ENTITY
	LDA COMBAT.TARGET.S_ENTITY_TYPE 	
	CMP #S_ENTITY_TYPE.SPECIAL ;is the Found S_ENTITY a SPECIAL?
	BNE .CHECK.NO.SELF_ATTACKS	
	
	;**FALLS THROUGH**

.SPECIAL.FOUND
	LDA SPRITE.RECORD+$05 ;load active S_ENTITY enemy code
	CMP COMBAT.TARGET.RECORD+$04 ;does it match Found S_ENTITY alignment code
	BNE .INCREMENT.SCREEN_ARRAY.INDEX_STEP ;if no, then next S_ENTITY
	
	;**FALLS THROUGH**
	
.CHECK.NO.SELF_ATTACKS	
	;if Found S_ENTITY is the active S_ENTITY, skip. S_ENTITIES don't attack themselves. Get next S_ENTITY
	;(note: this can happen if the Active S_ENTITY enemy type code (byte $05) is set to all. 	
	LDA SCREEN.MO_SPRITE.DATA,Y ;load record index of Found S_ENTITY's map object record index
	CMP SPRITE.RECORD+$B ;compare to Active S_ENTITY map object record index
	BNE .CHECK.NO.SELF_ATTACKS.COMPLETE ;Found S_ENTITY is not the Active S_ENTITY
	LDA SCREEN.MO_SPRITE_TYPE.DATA,Y ;load S_ENTITY type of Found S_ENTITY
	AND #$87 ;mask-out MTT flags (bits 3-6)
	CMP SPRITE.RECORD+$A ;compare to Active S_ENTITY type
	BNE .CHECK.NO.SELF_ATTACKS.COMPLETE ;Found S_ENTITY is not the Active S_ENTITY
		
	JMP INCREMENT.SCREEN_ARRAY.INDEX ;Found S_ENTITY is the Active S_ENTITY. Get next S_ENTITY

.INCREMENT.SCREEN_ARRAY.INDEX_STEP
	JMP INCREMENT.SCREEN_ARRAY.INDEX	
.CHECK.NO.SELF_ATTACKS.COMPLETE
	
.CHECK.HEALTH.STATUS
;CHECK FOR DEAD STATUS	
	;if Found S_ENTITY is dead, it is not a valid target, return to loop, look for more S_ENTITIES
	LDA COMBAT.TARGET.RECORD+$7 ;load S_ENTITY health status flag
	CMP #COMBAT.S_ENTITY.STATUS.DEAD
	BEQ .INCREMENT.SCREEN_ARRAY.INDEX_STEP
			;Found S_ENTITY is not dead, so it's now the Prospective Valid Valid Target S_ENTITY 

.VALID_TARGET.ACQUIRED
;GET DISTANCE TO VALID TARGET
	

;RECORD NEW TARGET HIT 
;(for calling routines which want all enemy S_ENTITIES returned, regardless of distance. e.g. smite spells)
@START	
;VALIDATE ENTRANCE
	LDA COMBAT.ACQUIRE.RETURN_VALUE.PARM ;($00 = standard return values | >=$01 = COMBAT.TARGET_HIT.DB(x))
	BEQ .RECORD.NEW_TARGET.DONE

	;YREG = screen index
	TYA
	LDX COMBAT.TARGET_HIT.DB.INDEX
	STA COMBAT.TARGET_HIT.DB,X
	;write stop value
	;(note: each time a new target hit is recorded the stop value is advanced 1 byte in the array)
	LDA #COMBAT.TARGET_HIT.DB.STOP_VALUE
	STA COMBAT.TARGET_HIT.DB+$1,X

	INC COMBAT.TARGET_HIT.DB.INDEX	;next target_hit record

.RECORD.NEW_TARGET.DONE
		
@END
	
.INIT.EXAMINE.DISTANCE.LOOP

			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; cpy #$9e
			; bne .temp
			
			; jsr keyin
			
			; lda #$ac
			; Ldx COMBAT.ACQUIRE.TARGET.FINAL.SINDEX		
			; ;LDy COMBAT.ABORT.FLAG	;($00 = not set | $01 = target not found | $02 = ESC pressed to exit attack command | $03 = TAB pressed to toggle left/right hand active). Usually ESC pressed is detected via the value $01 but this use of the abort flag is a special situation where there are 3 cases to detect. 
			; ;ldy COMBAT.ACQUIRE.LOOP.INDEX
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP

			
			
		;convert screen index of valid target to the screen column/row # and override the GMAP.X/Y in the 
		;valid S_ENTITY'S map object record
		;(this is done because MTT S_ENTITIES have a GMAP.X/Y of the upper left tile of the object. By calculating the 
		;column/row via the screen index we are able to calculate the distance to the specific tile of the MTT that 
		;is currently being examined.)
		LDA SCREEN.INDEX.COLUMN,Y
		STA COMBAT.TARGET.RECORD+$0
		LDA SCREEN.INDEX.ROW,Y
		STA COMBAT.TARGET.RECORD+$1
		
		STY COMBAT.ACQUIRE.LOOP.INDEX ;save SCREEN.ARRAY.LOOP counter/index

		
		LDA COMBAT.CHASE.TARGET.DISTANCE
		STA COMBAT.CHASE.PROSPECTIVE_TARGET.DISTANCE

			;init screen index of Active S_ENTITY / attacker tile using GMAP.X/Y
			;**OPT** Memory. This section could be removed if COMBAT.ATTACKER.SINDEX was set in COMBAT.MOVE.MANGER before this routie was called. That would require reworking the initial code
							;for specials and mobs turn, which isn't the same at first. We can't just use MAP_OBJECTS.TILE_LOCATION because this routine is also called by the PC target selection routine
							;which doesn't set that variable Maybe that variable could be set .EQ to combat.attacker.sindex as an alterantive. 
			LDA SPRITE.RECORD+$8 ;load Active S_ENTITY / attacker GMAP.X
			STA PARM.COLUMN
			LDA SPRITE.RECORD+$9 ;load Active S_ENTITY / attacker GMAP.Y
			STA PARM.ROW
		JSR CONVERT.COLUMN_ROW.TO.SCREEN_ARRAY_INDEX
			;RETURN VALUE = screen index
			TAY ;screen index of Active S_ENTITY
			;STY COMBAT.ACQUIRE.ATTACKER.SINDEX ;save for future use
			
		LDX #$00 ;init loop counter
		STX COMBAT.CHASE.VALID_TARGET.CLOSER ;init flag that determines, after the loop completes, if prospective target is closer to attacker
.EXAMINE.DISTANCE.LOOP
;(This loop only runs for multiple iterations if the Active S_ENTITY (the attacker) is a
;MTT S_ENTITY. The purpose of the loop in that event is to check the distance between the Current Valid Target and
;each tile of the MTT S_ENTITY. The shortest distance (of the MTT tiles) and the associated X/Y position flags will be saved as the Prospective
;Target if the distance is less than the Prospective Target.).



		;convert screen index of Active S_ENTITY / attacker to the screen column/row # 
		;(this makes it so with a MTT S_ENTITY, the distance is calculated from the tile of the MTT that is currently being examined)
		LDA SCREEN.INDEX.COLUMN,Y
		STA PARM1.GMAP.X
		LDA SCREEN.INDEX.ROW,Y
		STA PARM1.GMAP.Y
		
		LDA COMBAT.TARGET.RECORD+$0 ;load Valid Target S_ENTITY GMAP.X
		STA PARM2.GMAP.X
		LDA COMBAT.TARGET.RECORD+$1 ;load Valid  Target S_ENTITY GMAP.Y
		STA PARM2.GMAP.Y	
	JSR CALCULATE.DISTANCE				
		;ACC = tile distance, ignoring obstacles
		;RETURN MAP_OBJECTS.X_ADJ = X-AXIS differential
		;RETURN MAP_OBJECTS.Y_ADJ = Y-AXIS differential	
		;RETURN: MOB.POSITION.X_LT*, MOB.POSITION.X_GR*, MOB.POSITION.Y_LT*, MOB.POSITION.Y_GR*
			;*X_LT/X_GR = $00, then X-axis is equal. If either are set to $01 then true (i.e. if X_LT = $01, then X-AXIS is less than. if X_GR = $01, then X-AXIS is greater than) 
			;*Y_LT/Y_GR = $00, then Y-axis is equal. If either are set to $01 then true (i.e. if Y_LT = $01, then Y-AXIS is less than) 


			
		CMP COMBAT.CHASE.PROSPECTIVE_TARGET.DISTANCE ;is prospective target < the distance of the next closest target?
		;BEQ .VALID.TARGET.IS.CLOSER ;if yes
		BCS .VALID_TARET.IS.EQUAL_OR_FURTHER ;if no
		;FALLS THROUGH      ;if yes
		

.VALID.TARGET.IS.CLOSER			
; ;CHECK VALID TARGT MP (magic points)
;**WARNING: I never tested this code, which was for mobs to seek spellcaster. It isn't finished. It doesn't
;restrict the code from PCs (who use this routine for targte selection) and
;it is not tied to the mob map object record for a trigger. If it were implemented
;I had always figured only some mobs would have this behavior. 
		;
		;
		;PHA ;push tile distance (Active S_ENTITY) to stack
		; ;YREG = screen index of Active S_ENTITY
		; LDA #$00 ;set read mode ;($00 = read | $01 = write)
	; JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet
		; ;RETURN: CHR_SHEET.RECORD.READ($80)
		; LDA CHR_SHEET.PC_MOB.MP
		; BNE .MP.NOT.ZERO
		; PLA ;pop tile distance off the stack
		; JMP .EXAMINE.DISTANCE.LOOP.EXIT_TEST
		;
; .MP.NOT.ZERO		
		; PLA ;pull tile distance (Active S_ENTITY) to stack
@START		
		
;TEMP_SAVE TARGET POSITION
;(this is necessary because if the S_ENTITY is multi-tile, then the
;.EXAMINE.DISTANCE.LOOP will run for 4 iterations and the MOB.POSITION variables
;will get calculated again on each iteration)

		;ACC = tile distance, ignoring obstacles
		STA COMBAT.CHASE.PROSPECTIVE_TARGET.DISTANCE ;save valid target distance as prospective target distance. 
		;STY COMBAT.CHASE.MTT_ATTACKER.CLOSEST.TILE
		STA COMBAT.CHASE.VALID_TARGET.CLOSER ;($00 = valid target is not closer to the attacker | >=$01 prospective target is closer to attacker, considing all attacker tiles if MTT)

	
	TYA
	PHA
	LDY #$00
.SAVE.TARGET.POSITION.LOOP
;This loop does the following:
	; LDA COMBAT.CHASE.TARGET.X_LT
	; STA COMBAT.CHASE.VALID_TARGET.X_LT
	;
	; LDA COMBAT.CHASE.TARGET.X_GR
	; STA COMBAT.CHASE.VALID_TARGET.X_GR
	;
	; LDA COMBAT.CHASE.TARGET.Y_LT
	; STA COMBAT.CHASE.VALID_TARGET.Y_LT
	;
	; LDA COMBAT.CHASE.TARGET.Y_GR
	; STA COMBAT.CHASE.VALID_TARGET.Y_GR
	
	LDA MOB.POSITION.X_GR,Y
	STA COMBAT.CHASE.VALID_TARGET.X_GR,Y
	INY
	CPY #$04
	BNE .SAVE.TARGET.POSITION.LOOP
	PLA
	TAY
	
	JMP .EXAMINE.DISTANCE.LOOP.EXIT_TEST	
@END

.VALID_TARET.IS.EQUAL_OR_FURTHER
@START
;NOTE: since this section doesn't change the COMBAT.CHASE.VALID_TARGET.X_GR variables,
;they will contain the data from the last target deemed the "closest" when .SAVE.PROSPECTIVE.TARGET runs below. 
;This is okay because the X/Y position flags aren't needed for the furthest target since it isn't used
;for movement (range weapon and spell target use only).


	;ACC = tile distance, ignoring obstacles
	STA COMBAT.CHASE.FURTHEST_TARGET.DISTANCE
	;STA COMBAT.ACQUIRE.FURTHEST_TARGET.FLAG ;($00 = furthest target branch not used this iteration | >$01 furthest target branch used on this iteration)
	
	LDA COMBAT.TARGET.RECORD+$0 ;load Current Valid Target GMAP.X
	STA COMBAT.FURTHEST_TARGET.X	
	LDA COMBAT.TARGET.RECORD+$1 ;load Current Valid Target GMAP.Y
	STA COMBAT.FURTHEST_TARGET.Y
	
	LDA COMBAT.ACQUIRE.TARGET.SCREEN_ARRAY.INDEX
	STA COMBAT.ACQUIRE.FURTHEST_TARGET.FINAL.SINDEX 

	
@END

		;**FALLS THROUGH**
	
.EXAMINE.DISTANCE.LOOP.EXIT_TEST	
	;IS ATTACKER A MULTI-TILE MOB?
	LDA #$08 
	STA TEMP ;set bit3 for BIT test.
	LDA SPRITE.RECORD+$3 ;load flag byte from attacker's map object record
	BIT TEMP ;test bit3 of MAP_OBJECT.RECORD.READ+$03
	BEQ .EXAMINE.DISTANCE.LOOP.COMPLETE ;branch if bit3 is not set (indicates single-tile mob)

			
	INX ;increment loop index
	CPX #$01
	BEQ .EXAMINE.UPPER_RIGHT.TILE
	CPX #$02
	BEQ .EXAMINE.LOWER_RIGHT.TILE
	CPX #$03
	BEQ .EXAMINE.LOWER_LEFT.TILE
	JMP .EXAMINE.DISTANCE.LOOP.COMPLETE

.EXAMINE.UPPER_RIGHT.TILE
	INY ;move 1 tile right
	JMP .EXAMINE.DISTANCE.LOOP

.EXAMINE.LOWER_RIGHT.TILE
	TYA
	CLC
	ADC #SCREEN.ARRAY.OFFSET	 ;move 1 tile down
	TAY
	JMP .EXAMINE.DISTANCE.LOOP

.EXAMINE.LOWER_LEFT.TILE
	DEY ;move 1 tile left
	JMP .EXAMINE.DISTANCE.LOOP
.EXAMINE.DISTANCE.LOOP.COMPLETE	

			
	LDY COMBAT.ACQUIRE.LOOP.INDEX ;restore SCREEN.ARRAY.LOOP counter/index

	;is valid target closer to attacker? (in the case of an MTT attacker, closer to any of the MTT's tiles)
	LDA COMBAT.CHASE.VALID_TARGET.CLOSER ;($00 = prospective target is not closer to the attacker | >=$01 prospective target is closer to attacker, considing all attacker tiles if MTT)
	BNE .CHECK.WEAPON_RADIUS
	JMP .RETURN.TO.LOOP	
	
.CHECK.WEAPON_RADIUS ;(only applies if it is PC's turn and this routine is being used for the target selection routine)		
@START
		; ;ACC = tile distance, ignoring obstacles
		; STA COMBAT.CHASE.PROSPECTIVE_TARGET.DISTANCE ;save target distance
		
		;if PCs turn is active, then verify distance is within the attacker's weapon radius
		;(the only calling routine when PC's turn is active is COMBAT.SELECT.ATTACK_TARGET)
		LDA COMBAT.TURN_STATUS	;($00 = PCs turn. $01 = Special(s) S_ENTITY (example Wyvern Ally, Summoned Demon), $02 = MOBs turn)
		BNE .SAVE.PROSPECTIVE.TARGET

		;the following parameters were all set above
		;PARM1.GMAP.X, PARM2.GMAP.X, PARM1.GMAP.Y, PARM2.GMAP.Y, MAP_OBJECTS.X_ADJ, MAP_OBJECTS.Y_ADJ, SELECT_TARGET.RADIUS
	JSR COMBAT.CHECK.WEAPON.RADIUS
		;RETURN VALUE: ($00 = target distance exceeds radius, $01 = target distance <= weapon radius)
		CMP #$00 ;does distance exceed radius?
		BEQ .DISTANCE.EXCEEDS.RADIUS ;branch if yes
	
				; LDA #$01
				; STA TROUBLESHOOTING.HOOK

			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; LDA #$AD
			; ;LDX COMBAT.ACQUIRE.TARGET.FINAL.SINDEX 
			; LDY TEMP
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP

			
	;**FALLS THROUGH**
	
		; LDA MAP_OBJECTS.X_ADJ
		; CMP SELECT_TARGET.RADIUS
		; ;don't save target if distance is greater than radius
		; BEQ .Y_AXIS.TEST ;if distance = radius
		; BCS .DISTANCE.EXCEEDS.RADIUS ;implicit greater than test (if distance is greater than radius)
			; ;**OPT** setup as less than or equal to
		
		; ;X-AXIS distance is <= radius
		
	; ;**FALLS THROUGH**	

; .Y_AXIS.TEST
		; LDA MAP_OBJECTS.Y_ADJ
		; CMP SELECT_TARGET.RADIUS
		; ;don't save target if distance is greater than radius
		; BEQ .SAVE.PROSPECTIVE.TARGET ;if distance = radius
		; BCS .DISTANCE.EXCEEDS.RADIUS ;implicit greater than test (if distance is greater than radius)
			; ;**OPT** setup as less than or equal to

		; ;Y-AXIS distance is <= radius
			
		JMP .SAVE.PROSPECTIVE.TARGET	

.DISTANCE.EXCEEDS.RADIUS	
		JMP .RETURN.TO.LOOP ;else don't save target: it is out of range of the attacker's weapon

@END
		
.SAVE.PROSPECTIVE.TARGET ;saved the Current Valid Target as the Prospective Target
;(NOTE: these variables are also the final target variables since they will contain the best target when the loop
;terminates. This is the case because they are not clobbered. Some variables which are clobbered in each loop iteration are
;saved in .SET.FINAL.TARGET)
	
	
	;save Current Valid Target as Prospective Target

	LDX #$00
.SAVE.PROSEPECTIVE.TARGET.LOOP
;This loop does the following:
	; LDA COMBAT.CHASE.VALID_TARGET.X_GR
	; STA COMBAT.CHASE.TARGET.X_GR
	;
	; LDA COMBAT.CHASE.VALID_TARGET.X_LT
	; STA COMBAT.CHASE.TARGET.X_LT
	;
	; LDA COMBAT.CHASE.VALID_TARGET.Y_GR
	; STA COMBAT.CHASE.TARGET.Y_GR
	;
	; LDA COMBAT.CHASE.VALID_TARGET.Y_LT
	; STA COMBAT.CHASE.TARGET.Y_LT

	LDA COMBAT.CHASE.VALID_TARGET.X_GR,X
	STA COMBAT.CHASE.TARGET.X_GR,X
	INX
	CPX #$04
	BNE .SAVE.PROSEPECTIVE.TARGET.LOOP

	
	LDA COMBAT.CHASE.PROSPECTIVE_TARGET.DISTANCE ;restore target distance
	STA COMBAT.CHASE.TARGET.DISTANCE


	
	;**FUTURE: SEE IF HEALTH OF VALID TARGET IS LESS THAN THREADHOLD AND IF LESS, SEE IF DISTANCE IS UNDER THREAHOLD FOR PRIORITIZING HEALTH AS SELCTION CRITIERIA. OR DO THE CHECKS THE OTHER WAY AROUND, WHICHEVER MAKES MORE SENSE. 

	LDA COMBAT.TARGET.RECORD+$0 ;load Current Valid Target GMAP.X
	STA COMBAT.CHASE.TARGET.X	
	LDA COMBAT.TARGET.RECORD+$1 ;load Current Valid Target GMAP.Y
	STA COMBAT.CHASE.TARGET.Y
	
	LDA COMBAT.ACQUIRE.TARGET.SCREEN_ARRAY.INDEX
	STA COMBAT.ACQUIRE.TARGET.FINAL.SINDEX 




			
.SAVE.PROSPECTIVE.TARGET.DONE

	;**FALLS THROUGH**

			
.RETURN.TO.LOOP

	LDY COMBAT.ACQUIRE.LOOP.INDEX ;restore SCREEN.ARRAY.LOOP counter/index

	
			
	JMP .INCREMENT.SCREEN_ARRAY.INDEX_STEP

	
@END

COMBAT.KEYIN
@START
;PARAMETERS: none
;ENTRNACE: direct
;RETURN: ACC (ascii value of keypress)


;SAVE REGISTERS

;not needed. all the routines currently called
;do their own register save/restores:
;		COMBAT.DRAW.ACTIVE_S_ENTITY.INDICATOR
;		ANIMATION.UPDATE
;		WAIT.LOOP
;and neither X-REG or Y-REG are used in this routine.

.KEYIN.LOOP

		TYA
		PHA
		LDY COMBAT.PC.ACTIVE.SINDEX ;load screen tile location of active player
;		LDA #S_ENTITY_TYPE.PC ;set parm S_ENTITY_TYPE = PC
	JSR COMBAT.DRAW.ACTIVE_S_ENTITY.INDICATOR
		PLA
		TAY

	;update screen animation
	
	;!!!!! BSR:BANK1 !!!!
	JSR ANIMATION.UPDATE.ENTRANCE

		LDA #$80
	JSR WAIT.LOOP
	
	;update cursor animation
	JSR CURSOR.DRAW

		
; .ACTIVE_PC.BLINKING
; ;Note: the blink function works by erasing the active player icon from the foreground every X animation cycles. 
		
	; LDA COMBAT.ACTIVE_PC.BLINK.COUNTER
	; CMP #$03 ;set to $4 (an the CMP below) to neutralize animation.
	; BNE .BLINK_OFF

		; ;YREG = SCREEN_ARRAY_INDEX
		; LDA #$01
		; STA PAGE.FOREGROUND.OVERRIDE
	; JSR DRAW.TILE.SINGLE					;REPLACE MOB TILE WITH UNDERLYING MAP TILE

; .BLINK_OFF
	; LDA COMBAT.ACTIVE_PC.BLINK.COUNTER
	; CMP #$3 ;set to $4 (an the CMP above) to neutralize animation.
	; BNE .INCREMENT.BLINK.COUNTER
	; LDA #$00
	; STA COMBAT.ACTIVE_PC.BLINK.COUNTER
	
; .INCREMENT.BLINK.COUNTER	
	; INC COMBAT.ACTIVE_PC.BLINK.COUNTER

	
.KEYPRESS_CHECK	
	LDA $C000
    BPL .KEYIN.LOOP
	STA $C010               ;CLR LAST KEY
	
	;return value = ACC (keypress)	
	RTS
	
@END

COMBAT.GET.ACTIVE_PLAYER.SCREEN.LOCATION		
@START
;PARAMETERS: PC.ACTIVE.RECORD, COMBAT.MAP_OBJECTS.PC(x)
;ENTRANCE: direct
;RETURN: Y-REG: screen tile location

		;.calculate.active.player.screen.index	
		LDX COMBAT.PC.ACTIVE.RECORD
		LDA COMBAT.MAP_OBJECTS.PC+$0,X ;load active player GMAP.X
		STA SELECT_TARGET.CURRENT_TARGET.X
		STA PARM.GMAP.X
		LDA COMBAT.MAP_OBJECTS.PC+$1,X ;load active player GMAP.Y
		STA SELECT_TARGET.CURRENT_TARGET.Y
		STA PARM.GMAP.Y
	JSR CONVERT.COLUMN_ROW.TO.SINDEX
		LDY RETURN.SCREEN_ARRAY_INDEX ;used below
		;STY SELECT_TARGET.ACTIVE.SELECTION.SINDEX

	RTS
	
@END

COMMAND.INCREASE.SCROLL_SPEED ;====INCREASE SCROLL SPEED=== 
@START
	LDA GAME.SCROLL_SPEED
	SEC
	SBC #SCROLL_SPEED.INCREMENT
	BCC .EXIT ;if counter underflows, exit without saving scroll speed
.SAVE.SCROLL_SPEED
	STA GAME.SCROLL_SPEED
	
.EXIT
	RTS
@END

COMMAND.DECREASE.SCROLL_SPEED ;====DECREASE SCROLL SPEED=== 
@START
	LDA GAME.SCROLL_SPEED
	CLC
	ADC #SCROLL_SPEED.INCREMENT
	CMP #SCROLL_SPEED.MAX
	BCS .EXIT ;if counter overflows, exit without saving scroll speed
.SAVE.SCROLL_SPEED
	STA GAME.SCROLL_SPEED
	
.EXIT
	RTS
@END

@END

;SHAPE TABLES: 
@START

;target selector (standard)
@START
SELECT_TARGET.STANDARD.SHAPE_0.AND_MASK			.HS	FF.FF.83.C0.81.80.C0.83.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.C0.83.81.80.83.C0
SELECT_TARGET.STANDARD.SHAPE_0.ORA_MASK			.HS	80.80.F0.8F.98.98.8C.B0.86.E0.E6.E7.E6.E7.B6.ED.B6.ED.B6.ED.E6.E7.E6.E7.86.E0.8C.B0.98.98.F0.8F
				
SELECT_TARGET.STANDARD.SHAPE_1.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.FF.FF.9F.F8.87.E0.87.E0.83.C0.83.C0.83.C0.87.E0.87.E0.9F.F8.FF.FF.FF.FF.FF.FF
SELECT_TARGET.STANDARD.SHAPE_1.ORA_MASK			.HS	80.80.80.80.80.80.80.80.80.80.E0.87.E0.87.B0.8D.B0.8D.B0.8D.E0.87.E0.87.80.80.80.80.80.80.80.80
				
SELECT_TARGET.STANDARD.SHAPE_2.AND_MASK			.HS	8F.80.FF.BF.FF.FF.BF.FC.FF.FF.FF.FF.DF.FE.87.F8.87.F8.87.F8.DF.FE.FF.FF.FF.FF.BF.FC.FF.FF.FF.BF
SELECT_TARGET.STANDARD.SHAPE_2.ORA_MASK			.HS	80.80.80.80.80.80.80.80.80.80.80.80.80.80.A0.81.A0.81.A0.81.80.80.80.80.80.80.80.80.80.80.80.80
				
SELECT_TARGET.STANDARD.SHAPE_3.AND_MASK			.HS	FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF.FF
SELECT_TARGET.STANDARD.SHAPE_3.ORA_MASK			.HS	00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00
@END

;attack hit shape
ATTACK_HIT.SHAPE.SHAPE_0.AND_MASK			.HS	87.FC.89.C8.80.80.80.80.80.C0.81.C0.80.80.80.80.80.80.80.80.81.80.81.80.80.80.80.80.80.C0.81.C0
ATTACK_HIT.SHAPE.SHAPE_0.ORA_MASK			.HS	E0.80.C0.81.E6.B3.CC.99.FC.8F.F8.8F.BC.9F.AE.FD.AF.8D.AC.BD.B8.E7.F8.8F.FC.9F.CC.B3.E6.87.B0.8C

; ATTACK_HIT.SHAPE.SHAPE_0.AND_MASK			.HS	87.FC.89.C8.80.80.80.80.80.C0.81.C0.80.80.80.80.80.80.80.80.81.80.81.80.80.80.80.80.80.C0.80.80
; ATTACK_HIT.SHAPE.SHAPE_0.ORA_MASK			.HS	E0.80.C0.81.E6.B3.CC.99.FC.8F.F8.8F.BC.9F.AE.FD.AF.8D.AC.BD.B8.E7.F8.8F.FC.9F.CC.B3.E6.87.D5.AA

@END
	
;LOCAL VARIABLES
@START


CHR_SHEET_EXTENDED.MOB.HP_MAX 	  .BS $20 
CHR_SHEET_EXTENDED.SPECIAL.HP_MAX .BS $20 
	;datagram: (byte0: MOB#1 HP_MAX LO BYTE, byte1: MOB#1 HP_MAX HO BYTE); [byte2: MOB#2 HP_MAX LO BYTE, byte3: MOB#2 HP_MAX HO BYTE], etc.

CHR_SHEET_EXTENDED.SPECIAL.HP_MAX.AUX .EQ	SWAP_SPACE2.AUX_MEMORY+CHR_SHEET_EXTENDED.SPECIAL.HP_MAX-SRTN.COMBAT.ADDRESS 


@END

@END

	.NO $BD00 ;detect end of memory reserved for main program code 
				
