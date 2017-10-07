; ============================================
; Copyright (C) 2016-2018. 6502 Workshop, LLC
; contact: mark@6502workshop.com
; ============================================



;************************INCLUDE FILE*****************************
;(do not assemble stand-alone.  )

;=====================STATS ROUTINES DOCUMENTATION====================================
;
;=SUMMARY=
;The two primary routines are COMBAT.HIT_MISS.ROLL and
;COMBAT.DAMAGE.ROLL. Both used by PCs, MOBs, and SPECIAL S_ENTITIES
;which results in a S_ENTITY TYPE agnostic design.
;
;Both routines use a series of modifiers before making the final TO-HIT/BASE DAMAGE rolls.
;the TO-HIT routine has some routines that only run if a hit occurs, such as saving throws (i.e. dodge)
;and critical hit. 
;
;====S_ENTITY TYPE AGNOSTIC DESIGN===
;To make this design possible the COMBAT.ATTACKER.SINDEX and COMBAT.DEFENDER.SINDEX
;variables are passed as parameters or via Y-REG. These variables are used for 
;character sheet read/writes. The character sheet read/write routines figure out whether
;it needs to access a mob, pc, or special character sheet.
;
;This S_ENTITY agnostic design is able to work because most of the key character sheet fields
;are the same offset on all character sheets. There are a few exceptions and the routines
;do have some branches based on the attacker/defender S_ENTITY type. Such branches also occur
;to handle a few cases where the mobs/PCs/specials do not have the same formula for each S_ENTITY type.  
;
;There are no graphics routines used be either so most of the variables are .EQ pointers
;to SHAPE.HOPPER0. Some variables that need to be persistant (such as for the onscreen stats display) 
;are pointers to space in SHARED.VARIABLE_SPACE.BLOCK2.
;
;
;====MISC====
;
;-Death Magic Saving Throw.
;		See .ROLL.DEATH_MAGIC.SAVING_THROW which is embedded in the .LOAD.DAMAGE.FIELD routine in COMBAT.DAMAGE.ROLL
;		It can't be in COMBAT.TO_HIT.ROLL because spells don't call that routine (they always hit)
;
;=================================================================================


;COMBAT.HIT_MISS.ROLL
;(see SWAP.ROUTINES.Combat.ASM)


COMBAT.DAMAGE.ROLL
@START
;PARAMETERS: COMBAT.TURN_STATUS, *Y-REG: sindex of the attacker, COMBAT.DEFENDER.SINDEX, COMBAT.STATS.DAMAGE.TYPE
;ENTRANCE: direct
;RETURN: none

;=====================SUBROUTINE DOCUMENTATION====================================
;-OVERVIEW
;
;calculates net damage to target, reduces the targets hit points, if the net damage is >= targets hitpoints, then
;a target killed routine is executed which modifies the targets map object record and remove the target from the video screen.
;
;-FORMULA
;formula: base damage + critical hit damage - armor* = final damage
;
;modifiers are applied to base damage and armor could have modifiers applies but doesn't currently.
;
;*includes regular armor and resist magic defense
;
;-BASE DAMAGE: upgrading to 16-bit
;
;COMBAT.STATS.BASE_DAMAGE.MOD_TALLY(2) is 16-bit and so is COMBAT.STATS.DAMAGE.FINAL(2),
;so base damage could be a 16-bit number, but the overflow check in .CHECK.UNDERFLOW_OVERFLOW
;currently limits it to 8-bit. This is because the random number generator RANDOM.8 is setup for
;8-bit. I looked at my notes in RANDOM.8 and it looks like it is based off of code for a 16-bit random
;number generator, but my notes say that the HO byte was always $00. If I were to try to acquire
;16-bit random number capability I would first do some tests to verify that the HO byte always returns zero
;(like have it generate a few pages of 16-bit numbers. my original testing probably wasn't that comprensive),
;and if it does always return $00 in the HO byte I may be able to get some help from the online forums.
;But, consider that a 16-bit random number would be from $0000-$FFFF and I'm likely only interested in values
;from $0000-$002FF or so, most of the time. as a result, there could be a speed problem
;as many interations may be needed to find one that fits the range. On the other hand, maybe the HO byte can be
;scaled down using LSRs. 
;
;
;=================================================================================

;**OPT** Memory. change this parameter to COMBAT.STATS.ATTACKER.SINDEX. I think all of the calling routines have this 
;variables set at the point this routine. some might have it set after and the set could probably be moved up. the savings 
;is not doing a special LDY just before the call whtn the value is already in a variable.  
	

			
;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA
	

;CALCULATE DAMAGE
@START
.INIT
@START
	LDA #$00 
	STA COMBAT.STATS.CRTL_HIT_DAMAGE+$0 ;this init is required because if the critical hit doesn't occur these variables will still have garabge values when they are added to the damage subtotal.
	STA COMBAT.STATS.CRTL_HIT_DAMAGE+$1 ;""
	STA COMBAT.STATS.HIT_MISS_KILL.FLAG ;($00 = hit | $01 = miss | $02 = kill)

			; LDA #$01
			; STA TROUBLESHOOTING.HOOK
		
	JSR GET.ATTACKER_DEFENDER.CHR_SHEET.DATA



			
			; LDA #$AA
			; LDX COMBAT.TARGET_HIT.DB+$0
			; LDY COMBAT.ACTIVE_PLAYER.SINDEX
			; JSR PREP.BRK
			; BRK
			
.LOAD.DAMAGE.FIELDS
@START

;DETERMINE WHICH DAMAGE FIELDS TO LOAD FROM CHARACTER SHEET
	LDA COMBAT.STATS.DAMAGE.TYPE ;($00 = melee | $01 range, $02 spell. High-bit not set = non-magic weapon (doesn't apply to type=spell or MOBs) | high-bit set = magic weapon (doesn't apply to type=spell or MOBs))
	CMP #$02 ;**OPT** Memory. Changing spell value to $00, and have melee/range as $01/$02 should free up some memory. Verify that BNE will work for all situations that are not spell.
	BEQ .LOAD.ALL.DAMAGE.SPELL ;branch if damage type is spell	

	LDA COMBAT.TURN_STATUS   ;($00 = PCs turn. $01 = Special(s) S_ENTITY (example Wyvern Ally, Summoned Demon), $02 = MOBs turn)
	BEQ .LOAD.PC.DAMAGE ;if PCs turn, then branch
;.LOAD.MOB_SPECIAL.DAMAGE
	LDA CHR_SHEET.MOB.DMG ;load mob damage 
	JMP .APPLY.DAMAGE.MODIFIERS
.LOAD.PC.DAMAGE	

			; LDA COMBAT.ATTACK_COMMAND.CURRENT_HAND ;($00 = left hand | $01 = right hand)
			; LDX #$AA
			; JSR PREP.BRK
			; BRK	
			
	;determine hand used for attack
	LDA COMBAT.ATTACK_COMMAND.ACTIVE_HAND	;(high bit not set = left hand active | high bit set = right hand active)
	;LDA COMBAT.ATTACK_COMMAND.CURRENT_HAND			;($00 = left hand | $01 = right hand)
	BMI .LOAD.PC.DAMAGE.RIGHT_HAND
;.LOAD.PC.DAMAGE.LEFT_HAND	
	LDA CHR_SHEET.PC.DMG.LHAND ;load damage field (left hand)			
	JMP .APPLY.DAMAGE.MODIFIERS
.LOAD.PC.DAMAGE.RIGHT_HAND
	LDA CHR_SHEET.PC.DMG.RHAND ;load damage field (right hand)
	JMP .LOAD.DAMAGE.FIELDS.DONE

.LOAD.ALL.DAMAGE.SPELL

	;is target automatically killed? (i.e. death magic)
	LDA COMBAT.STATS.SPELL.KILL_FLAG_MASTER ;($00 = succeeded | $01 = not attempted | $03 = failed). If succeeded then all targets hit by the spell are automatically killed. Sometimes saving thows may be available. 
	BNE .NO_AUTO_KILL
	
.ROLL.DEATH_MAGIC.SAVING_THROW
@START 
;Formula: death magic saving throw = (INTELLGIENCE) CMP random #
	
;intelligence skill = 0
;it should result in death magic succeeding because the random number is !1-!99 and thus INT = 0 will
;never result in the random number being lower than INT)

;ROLL SAVING THROW

		;get random #
	JSR COMBAT.RANDOM.8.BCD
		;RETURN VALUE = BCD random # !1-!99
		STA COMBAT.STATS.DEATH_MAGIC_ST.ROLL ;**OPT** Memory. Only needed for debugging

	;convert intelligence to BCD
		LDA COMBAT.STATS.DEFENDER.INT ;load defender intelligence
	JSR CONVERT.HEX.8_TO_BCD.16	
		;**OPT** Memory. These are only needed for debugging
		LDA BCD+$0 ;BCD conversion of COMBAT.STATS.DEFENDER.INT
		STA COMBAT.STATS.DEATH_MAGIC_ST.PROB

		
;=========================TROUBLESHOOTING
@START
		; PHA ;save ACC
		; TXA
		; PHA
		
		; LDA #$19
		; STA VARIABLE1.HTAB
		; LDA #$0C
		; STA VARIABLE1.VTAB
		; LDA #$1C
		; STA VARIABLE2.HTAB
		; LDA #$0C
		; STA VARIABLE2.VTAB								
		
		; LDA COMBAT.STATS.DEATH_MAGIC_ST.ROLL
		; LDX COMBAT.STATS.DEATH_MAGIC_ST.PROB
	; JSR MONITOR.VARIABLE
	; JSR KEYIN ;pause optional
		; STA TEMP
		; PLA
		; TAX
		; PLA ;restore ACC
; ; ;=========================
@END

		
		SED ;set decimal mode
		CMP COMBAT.STATS.DEATH_MAGIC_ST.ROLL
		CLD ;clear decimal mode
		BCC .DEATH_MAGIC.SUCCEEDS	
;.DEATH_MAGIC.FAILS
	LDA #$03 ;set failed flag value
	STA COMBAT.STATS.SPELL.KILL_FLAG ;death magic attack: ($00 = succeeded | $01 = not attempted | $03 = failed). If set then all targets hit by the spell are automatically killed. Sometimes saving thows may be available. 

	;set $0000 damage
	LDA #$00 ;set damage to $0000
	STA COMBAT.STATS.DAMAGE.FINAL+$0
	LDA #$00 ;set damage to $0000
	STA COMBAT.STATS.DAMAGE.FINAL+$1
	

			
	JMP .EXIT

.DEATH_MAGIC.SUCCEEDS
	LDA #$00 ;set success flag value
	STA COMBAT.STATS.SPELL.KILL_FLAG ;death magic attack: ($00 = succeeded | $01 = not attempted | $03 = failed). If set then all targets hit by the spell are automatically killed. Sometimes saving thows may be available. 
.ROLL.DEATH_MAGIC.SAVING_THROW.DONE

	
@END


	;**FALLS THROUGH**
	
	;target is auto-killed, set final damage
	LDA #$B8 ;set damage to !3000
	STA COMBAT.STATS.DAMAGE.FINAL+$0
	LDA #$0B ;set damage to !3000
	STA COMBAT.STATS.DAMAGE.FINAL+$1
	
	JMP .TARGET.IS.KILLED
	
.NO_AUTO_KILL
				; LDA #$AA
				; LDY COMBAT.STATS.SPELL.DAMAGE
				; JSR PREP.BRK
				; BRK
				
	LDA COMBAT.STATS.SPELL.DAMAGE ;contains median damage for spell (not X2) 

	;**FALLS THROUGH**
		
.LOAD.DAMAGE.FIELDS.DONE
@END
		
		
@END

;**FALLS THROUGH**

;CALCULATE BASE DAMAGE
@START

.APPLY.DAMAGE.MODIFIERS			
@START
	STA COMBAT.STATS.DAMAGE_MEDIAN
				
.INIT.DAMAGE.TALLY
@START
;***NOTE: the tally is init to $8000 so that all modified can be added/subtracted without a 16-bit underflow occuring, and in a way 
;where an 8-bit underflow/overflow in the LO byte during the tally can be detected when the tally is done so that the minimum or maximum DAMAGE can be used. 
;after all modifiers are added/subtracted to/from base DAMAGE (note some modifiers are added in CALCULATE.COMBAT.STATS.GENERAL_ENTRANCE before this subroutine is called), if high-bit of HO byte is not set then 8-bit underflow has occured. 
;If no underflow has occurred, then mask out high-bit of HO byte. If HO byte != $00 then 8-bit overflow has occured 

	;init tally and add the damage value from character sheet
	
	;ACC =  COMBAT.STATS.DAMAGE_MEDIAN ;attacker damage field
	STA COMBAT.STATS.BASE_DAMAGE.MOD_TALLY+$0
	LDA #$80 
	STA COMBAT.STATS.BASE_DAMAGE.MOD_TALLY+$1
			
	;**FALLS THROUGH**
@END


;PC/MOB TURN BRANCH
	LDA COMBAT.TURN_STATUS   ;($00 = PCs turn. $01 = Special(s) S_ENTITY (example Wyvern Ally, Summoned Demon), $02 = MOBs turn)
	BEQ .APPLY.PC_DAMAGE_MODIFIERS ;if PCs turn, then branch
	JMP .DAMAGE.MODIFIERS.COMPLETE

.APPLY.PC_DAMAGE_MODIFIERS ;PC attacker
	;CHR_SHEET.MOB.RESIST_MAGIC
	
.APPLY.SIZE.DMG.MODIFIER
@START
;VALIDATE ENTRANCE
	LDA COMBAT.STATS.DAMAGE.TYPE ;($00 = melee | $01 range, $02 spell. High-bit not set = non-magic weapon (doesn't apply to type=spell or MOBs) | high-bit set = magic weapon (doesn't apply to type=spell or MOBs)	
	AND #$7F ;mask-out high-bit		
	CMP #$00
	BEQ .APPLY.SIZE.DMG.MODIFIER.COMPLETE ;branch if readied weapon is melee (whether it is magical or not)	
	CMP #$02
	BEQ .APPLY.SIZE.DMG.MODIFIER.COMPLETE ;branch if damage type is spell	

	;is mob char sheet size/dodge multi-use field in use a size field?
	LDA COMBAT.STATS.DEFENDER.DODGE_SKILL
	BNE .APPLY.SIZE.DMG.MODIFIER.COMPLETE ;if dodge skill > $00 then it has been set, which means the field is not in use for size

	
	JSR COMBAT.STATS.CALC.SIZE.MODIFIER.PERCENT
		;RETURN VALUE: COMBAT.STATS.SIZE_PERCENT, COMBAT.STATS.SIZE.MODIFIER.RETURN_FLAG ;($00 = small mob | $01 larger mob | $FF = medium mob -no modifier)
		BMI .APPLY.SIZE.DMG.MODIFIER.COMPLETE ;branch if mob in the size range where the size modifier is $00. 
	

			
.APPY.PERCENTAGE

;APPLY TO DAMAGE TALLY
		
		;set parms for COMBAT.STATS.APPLY.PERCENTAGE.BCD
		LDA COMBAT.STATS.BASE_DAMAGE.MOD_TALLY+$0
		STA COMBAT.STATS.APPLY_PERCENT.PARM_BASE+$0
		LDA COMBAT.STATS.BASE_DAMAGE.MOD_TALLY+$1
		AND #$7F ;strip off high-bit (see .INIT.TO_HIT.TALLY for docs on the special use of the high-bit for this variable)
		STA COMBAT.STATS.APPLY_PERCENT.PARM_BASE+$1		
		;COMBAT.STATS.SIZE_PERCENT already set
		LDA #$00 ;set hex mode ($00 = hex mode | $01 = BCD mode)		
	JSR COMBAT.STATS.APPLY.PERCENTAGE
		;RETURN VALUE: RESULT(2)
		LDA RESULT+$0
		STA COMBAT.STATS.SIZE.MODIFIER+$0
		LDA RESULT+$1
		STA COMBAT.STATS.SIZE.MODIFIER+$1

	LDA COMBAT.STATS.SIZE.MODIFIER.RETURN_FLAG ;($00 = small mob | $01 larger mob | $FF = medium mob -no modifier)
	CMP #$01 ;is modifier an increase to base stat? (for small MOBs, PC range damage increases)
	BEQ .APPLY.PERCENT.DMG.LARGE_MOB
	
;SMALL MOB

	;add size modifier to DAMAGE TALLY
	LDA COMBAT.STATS.BASE_DAMAGE.MOD_TALLY+$0
	CLC
	ADC COMBAT.STATS.SIZE.MODIFIER+$0
	STA COMBAT.STATS.BASE_DAMAGE.MOD_TALLY+$0		
	LDA COMBAT.STATS.BASE_DAMAGE.MOD_TALLY+$1
	ADC COMBAT.STATS.SIZE.MODIFIER+$1 ;16-bit subtract
	STA COMBAT.STATS.BASE_DAMAGE.MOD_TALLY+$1

	JMP .APPLY.PERCENT.DMG.DONE
	
.APPLY.PERCENT.DMG.LARGE_MOB
	;add size modifier to DAMAGE tally
	LDA COMBAT.STATS.BASE_DAMAGE.MOD_TALLY+$0
	SEC
	SBC COMBAT.STATS.SIZE.MODIFIER+$0
	STA COMBAT.STATS.BASE_DAMAGE.MOD_TALLY+$0		
	LDA COMBAT.STATS.BASE_DAMAGE.MOD_TALLY+$1
	SBC COMBAT.STATS.SIZE.MODIFIER+$1 ;16-bit subtract
	STA COMBAT.STATS.BASE_DAMAGE.MOD_TALLY+$1

	;**FALLS THROUGH**
.APPLY.PERCENT.DMG.DONE

.APPLY.SIZE.DMG.MODIFIER.COMPLETE

			; LDA #$AA

			; LDX COMBAT.STATS.SIZE_PERCENT	
			; LDY COMBAT.STATS.SIZE.MODIFIER
			; LDX COMBAT.STATS.BASE_DAMAGE.MOD_TALLY+$0
			; LDY COMBAT.STATS.BASE_DAMAGE.MOD_TALLY+$1
			; JSR PREP.BRK
			; BRK
			
@END




.CHECK.UNDERFLOW_OVERFLOW
;***NOTE: the tally is init to $8000 above in (.INIT.DAMAGE.TALLY) so that all modified can be added/subtracted without a 16-bit underflow occuring, and in a way 
;where an 8-bit underflow/overflow in the LO byte during the tally can be detected when the tally is done so that the minimum or maximum DAMAGE can be used. 
;after all modifiers are added/subtracted to/from base DAMAGE (note some modifiers are added in CALCULATE.COMBAT.STATS.GENERAL_ENTRANCE before this subroutine is called), if high-bit of HO byte is not set then 8-bit underflow has occured. 
;If no underflow has occurred, then mask out high-bit of HO byte. If HO byte != $00 then 8-bit overflow has occured 

	LDA COMBAT.STATS.BASE_DAMAGE.MOD_TALLY+$01 ;load HO byte of modifier tally
	BPL .SET.MIN.DAMAGE ;branch if high-bit is not set (16-bit underflow occured during the tally)
	;**FALLS THROUGH**	
.CHECK.OVERFLOW
	AND #$7F ;mask-out high-bit
	CMP #$00 ;if the HO byte of the tally is $00 after the high-bit is masked out, then an 8-bit overflow never occured in the LO byte during the tally
	BNE .SET.MAX.DAMAGE ;8-bit overflow occured in the LO byte during the tally
	;**FALLS THROUGH**	
.CHECK.UNDERFLOW_OVERFLOW.COMPLETE

	;**FALLS THROUGH**
	
.CHECK.DAMAGE.MIN_MAX		
;the modifiers might collectively result in a DAMAGE value lower than the minimum or greater than the maximum. 
;If that happens, this routine overrides and sets the minimum or maximum as applies.

	LDA COMBAT.STATS.BASE_DAMAGE.MOD_TALLY+$00 ;load LO byte of modifier tally
	CMP #COMBAT.STATS.BASE_DAMAGE.MIN
	BCC .SET.MIN.DAMAGE
	CMP #COMBAT.STATS.BASE_DAMAGE.MAX
	BCS .SET.MAX.DAMAGE
	JMP .DAMAGE.MODIFIERS.COMPLETE
	
.SET.MIN.DAMAGE
	LDA #COMBAT.STATS.BASE_DAMAGE.MIN	
	STA	COMBAT.STATS.BASE_DAMAGE.MOD_TALLY+$0
	JMP .DAMAGE.MODIFIERS.COMPLETE

.SET.MAX.DAMAGE	
	LDA #COMBAT.STATS.BASE_DAMAGE.MAX	
	STA	COMBAT.STATS.BASE_DAMAGE.MOD_TALLY+$0
	
	;**FALLS THROUGH**

.DAMAGE.MODIFIERS.COMPLETE
@END

				
.CALC.DMG.RANGE
		LDA COMBAT.STATS.BASE_DAMAGE.MOD_TALLY+$0 ;set parm: range median
	JSR COMBAT.STATS.CALC.RANGE ;8-BIT SUPPORT ONLY
		;RETURN VALUES: RND.LO, RND.HI
		LDA RND.LO ;load damage LO field 	
		STA COMBAT.STATS.BASE_DAMAGE_LO ;**OPT** Memory. Only needed for debugging
		LDA RND.HI ;load damage LO field 	
		STA COMBAT.STATS.BASE_DAMAGE_HI ;**OPT** Memory. Only needed for debugging
		
	;**FALLS THROUGH**
	
.ROLL.DMG
@START	
		;set parms for RANDOM.8
		;RND.LO: set in .CALC.DMG.RANGE
		;RND.HI: set in .CALC.DMG.RANGE
.EXECUTE.DMG.ROLL
			;**TEST; DELETE
			; LDA #$00
			; STA RND.LO
			;STA RND.HI
	JSR RANDOM.8
			;lda #$ff
		STA COMBAT.STATS.BASE_DAMAGE.ROLL

;BASE DAMAGE *2
;(used to double the impact of the 8-bit base damage roll instead of generating a 16-bit random #)

		;ACC = COMBAT.STATS.BASE_DAMAGE.ROLL
		STA MULPLR
		LDA #$0
		STA MULPLR+$1
		;
		LDA #$02
		STA MULCND
		LDA #$00
		STA MULCND+$01
	JSR MLP.16.NO_BCD
		;RETURN VALUE: result+$0 (product LO), result+$1 (product HO)		
		LDA RESULT+$0
		STA COMBAT.STATS.BASE_DAMAGE.ROLL_X2+$0
		LDA RESULT+$1
		STA COMBAT.STATS.BASE_DAMAGE.ROLL_X2+$1
					
	;**FALLS THROUGH**


				
@END
@END
	
	;**FALLS THROUGH**

;.ROLL.DEATH_MAGIC.SAVING_THROW ;***see top of this subroutine

.CALCULATE.CRITICAL_HIT.DAMAGE
@START
;formula: critical hit damage = (ch skill * 4)
			
.VALIDATE.ENTRY			
; ;PC/MOB TURN BRANCH
	; LDA COMBAT.TURN_STATUS   ;($00 = PCs turn. $01 = Special(s) S_ENTITY (example Wyvern Ally, Summoned Demon), $02 = MOBs turn)
	; BNE .CALC.CRITIAL_HIT.DONE ;if not PCs turn, then branch

						; lda #$aa
						; ldx COMBAT.STATS.CRTL_HIT_FLAG
						; ; ldx COMBAT.STATS.DEFENDER.ENGAGED.MO_INDEX
						; ; ldy COMBAT.ATTACKER.SINDEX
						; jsr prep.brk
						; brk
	
;is attack spell damage?
;(critical hit is N/A for spells)
	LDA COMBAT.STATS.DAMAGE.TYPE ;($00 = melee | $01 range, $02 spell. High-bit not set = non-magic weapon (doesn't apply to type=spell or MOBs) | high-bit set = magic weapon (doesn't apply to type=spell or MOBs))
	CMP #$02
	BEQ .CALC.CRITIAL_HIT.DONE ;branch if damage type is spell	

	
;did attacker have a successful critical hit?
	LDA COMBAT.STATS.CRTL_HIT_FLAG 	;($00 = critical hit succeeded, $01 = critical hit not attempted, $03 = critical hit blocked by defender)
	BNE .CALC.CRITIAL_HIT.DONE
	
	;**FALLS THROUGH**
	
.CRITICAL_HIT.START
	
;(ch skill * 4)		
		LDA CHR_SHEET.PC_MOB.SKILL.CRITICAL_HIT
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
		LDA RESULT+$0
		STA COMBAT.STATS.CRTL_HIT_DAMAGE+$0
		LDA RESULT+$1
		STA COMBAT.STATS.CRTL_HIT_DAMAGE+$1


.RANGE.WEAPON.CHECK
;REDUCE RANGE WEAPON CRITICAL HIT DMG TO 25%
				
		LDA COMBAT.STATS.DAMAGE.TYPE ;($00 = melee | $01 range, $02 spell. High-bit not set = non-magic weapon (doesn't apply to type=spell or MOBs) | high-bit set = magic weapon (doesn't apply to type=spell or MOBs)
		BEQ .CALC.CRITIAL_HIT.DONE
		;the spell damage type fall through but the parent routine COMBAT.DAMAGE.ROLL isn't used by spells 
		;so if the damage type isn't melee it should be same to assume damage is range. 
		
		LDA COMBAT.STATS.CRTL_HIT_DAMAGE+$0
		STA DIVIDEND+$0	;number to be divided
		LDA COMBAT.STATS.CRTL_HIT_DAMAGE+$1
		STA DIVIDEND+$1
		LDA #$04
		STA DIVISOR+$0     ;number to divide by
		LDA #$00
		STA DIVISOR+$1
	JSR DIV.16			;(dividend/divisor)			
		;RETURN VALUE: result+$0 (quotient LO), result+$1 (quotient HO)
		LDA RESULT+$0
		STA COMBAT.STATS.CRTL_HIT_DAMAGE+$0
		LDA RESULT+$1
		STA COMBAT.STATS.CRTL_HIT_DAMAGE+$1

		
.CALC.CRITIAL_HIT.DONE
	
@END
@END

;(OLD) CALCULATE CRITICAL HIT DAMAGE (used real division, mob & PC had seperate formulas)
@START
; ;formula: critical hit damage = (base damage roll_X2 + ((ch skill * ch skill)/5))
			
; .VALIDATE.ENTRY			
; ; ;PC/MOB TURN BRANCH
	; ; LDA COMBAT.TURN_STATUS   ;($00 = PCs turn. $01 = Special(s) S_ENTITY (example Wyvern Ally, Summoned Demon), $02 = MOBs turn)
	; ; BNE .CALC.CRITIAL_HIT.DONE ;if not PCs turn, then branch

						; ; lda #$aa
						; ; ldx COMBAT.STATS.CRTL_HIT_FLAG
						; ; ; ldx COMBAT.STATS.DEFENDER.ENGAGED.MO_INDEX
						; ; ; ldy COMBAT.ATTACKER.SINDEX
						; ; jsr prep.brk
						; ; brk
	
; ;is attack spell damage?
; ;(critical hit is N/A for spells)
	; LDA COMBAT.STATS.DAMAGE.TYPE ;($00 = melee | $01 range, $02 spell. High-bit not set = non-magic weapon (doesn't apply to type=spell or MOBs) | high-bit set = magic weapon (doesn't apply to type=spell or MOBs))
	; CMP #$02
	; BEQ .CALC.CRITIAL_HIT.DONE_STEP ;branch if damage type is spell	

	
; ;did attacker have a successful critical hit?
	; LDA COMBAT.STATS.CRTL_HIT_FLAG 	;($00 = critical hit succeeded, $01 = critical hit not attempted, $03 = critical hit blocked by defender)
	; BEQ .CRITICAL_HIT.START

; .CALC.CRITIAL_HIT.DONE_STEP
	; JMP .CALC.CRITIAL_HIT.DONE
	
; .CRITICAL_HIT.START
; ;PC/MOB TURN BRANCH
	; LDX COMBAT.TURN_STATUS   ;($00 = PCs turn. $01 = Special(s) S_ENTITY (example Wyvern Ally, Summoned Demon), $02 = MOBs turn)
	; BNE .CRITICAL_HIT.NON_PC_FORMULA ;if not PCs turn, then branch

	
; .CRITICAL_HIT.PC_FORMULA				
; ;(ch skill * ch skill)
		
		; LDA CHR_SHEET.PC_MOB.SKILL.CRITICAL_HIT
		; STA MULPLR
		; LDA #$00
		; STA MULPLR+$1
		; ;
		; LDA CHR_SHEET.PC_MOB.SKILL.CRITICAL_HIT ;**OPT** Memory. Skip this LDA by doing the STA for MULCND right after STA to MULPLR
		; STA MULCND
		; LDA #$00
		; STA MULCND+$01
	; JSR MLP.16.NO_BCD
		; ;RETURN VALUE: result+$0 (product LO), result+$1 (product HO)
		
; ;(/5)		
		; LDA RESULT+$0 ;(ch skill * ch skill), LO
		; STA DIVIDEND+$0	;number to be divided
		; LDA RESULT+$1 ;(ch skill * ch skill), HO
		; STA DIVIDEND+$1
		; LDA #$05	;!5
		; STA DIVISOR+$0     ;number to divide by
		; LDA #$00
		; STA DIVISOR+$1
	; JSR DIV.16			;(dividend/divisor)			
		; ;RETURN VALUE: result+$0 (quotient LO), result+$1 (quotient HO)

; ;(+base damage roll)
		; LDA RESULT+$0 ;((ch skill * ch skill)/10), LO byte
		; CLC
		; ADC COMBAT.STATS.BASE_DAMAGE.ROLL_X2+$0  ;used to double the impact of the 8-bit base damage roll instead of generating a 16-bit random #
		; STA COMBAT.STATS.CRTL_HIT_DAMAGE+$0
		; LDA RESULT+$1 ;((ch skill * ch skill)/10), HO byte
		; ADC COMBAT.STATS.BASE_DAMAGE.ROLL_X2+$1  ;16-bit add. Used to double the impact of the 8-bit base damage roll instead of generating a 16-bit random #
		; STA COMBAT.STATS.CRTL_HIT_DAMAGE+$1

		; JMP .RANGE.WEAPON.CHECK
		

; .CRITICAL_HIT.NON_PC_FORMULA
		; LDA COMBAT.STATS.BASE_DAMAGE.ROLL_X2+$0  ;used to double the impact of the 8-bit base damage roll instead of generating a 16-bit random #
		; STA DIVIDEND+$0	;number to be divided
		; LDA COMBAT.STATS.BASE_DAMAGE.ROLL_X2+$1  ;used to double the impact of the 8-bit base damage roll instead of generating a 16-bit random #
		; STA DIVIDEND+$1
		; LDA #$02
		; STA DIVISOR+$0     ;number to divide by
		; LDA #$00
		; STA DIVISOR+$1
	; JSR DIV.16			;(dividend/divisor)			
		; ;RETURN VALUE: result+$0 (quotient LO), result+$1 (quotient HO)
		; LDA RESULT+$0
		; STA COMBAT.STATS.CRTL_HIT_DAMAGE+$0
		; LDA RESULT+$1
		; STA COMBAT.STATS.CRTL_HIT_DAMAGE+$1

		; ;**FALLS THROUGH**
; .RANGE.WEAPON.CHECK
; ;REDUCE RANGE WEAPON CRITICAL HIT DMG TO 25%
				
		; LDA COMBAT.STATS.DAMAGE.TYPE ;($00 = melee | $01 range, $02 spell. High-bit not set = non-magic weapon (doesn't apply to type=spell or MOBs) | high-bit set = magic weapon (doesn't apply to type=spell or MOBs)
		; BEQ .CALC.CRITIAL_HIT.DONE
		; ;the spell damage type fall through but the parent routine COMBAT.DAMAGE.ROLL isn't used by spells 
		; ;so if the damage type isn't melee it should be same to assume damage is range. 
		
		; LDA COMBAT.STATS.CRTL_HIT_DAMAGE+$0
		; STA DIVIDEND+$0	;number to be divided
		; LDA COMBAT.STATS.CRTL_HIT_DAMAGE+$1
		; STA DIVIDEND+$1
		; LDA #$04
		; STA DIVISOR+$0     ;number to divide by
		; LDA #$00
		; STA DIVISOR+$1
	; JSR DIV.16			;(dividend/divisor)			
		; ;RETURN VALUE: result+$0 (quotient LO), result+$1 (quotient HO)
		; LDA RESULT+$0
		; STA COMBAT.STATS.CRTL_HIT_DAMAGE+$0
		; LDA RESULT+$1
		; STA COMBAT.STATS.CRTL_HIT_DAMAGE+$1
		
		
; .CALC.CRITIAL_HIT.DONE
	
@END

;**FALLS THROUGH**
			
.CALCULATE.ARMOR
@START
.ARMOR.INIT			
	LDA #$00
	STA COMBAT.STATS.RESIST_MAGIC.DEFENSE_RATING+$0
	STA COMBAT.STATS.RESIST_MAGIC.DEFENSE_RATING+$1
	STA COMBAT.STATS.DAMAGE.ARMOR_TALLY+$0
	STA COMBAT.STATS.DAMAGE.ARMOR_TALLY+$1
	STA COMBAT.STATS.REGULAR.ARMOR.ROLL

				
.REGULAR.ARMOR.DEDUCTION
@START 
			
.REGULAR.ARMOR.ROLL.NON_PC.ATTACKER	
		LDY COMBAT.DEFENDER.SINDEX ;load defender's (target) SINDEX
		LDA #$00 ;set read mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet			

.CALC.REGULAR.START	
.CALC.REGULAR.ARMOR_RANGE
		
		LDA CHR_SHEET.PC_MOB.ARMOR ;set parm: range median
		;note: #COMBAT.STATS.REGULAR.ARMOR.RANGE.PERCENT is not used in COMBAT.STATS.CALC.RANGE because it is .EQ to #COMBAT.STATS.DAMAGE.RANGE.PERCENT, which is used in the routine. 
	JSR COMBAT.STATS.CALC.RANGE ;8-BIT SUPPORT ONLY
		;RETURN VALUES: RND.LO, RND.HI	

.EXECUTE.REGULAR.ARMOR.ROLL		
	JSR RANDOM.8
			sta temp
		
		;RETURN VALUE: 8-bit random number
.REGULAR.ARMOR.ROLL.DONE

.CHECK.DAMAGE_TYPE
;regular armor is only 25% effective against magical damage			
	LDX COMBAT.STATS.DAMAGE.TYPE ;($00 = melee | $01 range, $02 spell. High-bit not set = non-magic weapon (doesn't apply to type=spell or MOBs) | high-bit set = magic weapon (doesn't apply to type=spell or MOBs)
	BMI .MAGIC_WP.SPELL.DMG ;branch if magic weapon damage
	;not magic weapon damage, check if spell damage
	CPX #$02
	BNE .CHECK.DAMAGE_TYPE.DONE ;branch if not spell damage
	;**FALLS THROUGH**
.MAGIC_WP.SPELL.DMG
	;ACC = 8-bit random number
	;(reduce ACC to 25% of random number)
	LSR ;/2
	LSR ;/4
				
	;**FALLS THROUGH**
.CHECK.DAMAGE_TYPE.DONE
	;ACC = 8-bit random number
	STA COMBAT.STATS.REGULAR.ARMOR.ROLL

;ADD REGULAR ARMOR DEDUCTION TO TALLY	
	LDA COMBAT.STATS.DAMAGE.ARMOR_TALLY+$0	
	CLC
	ADC COMBAT.STATS.REGULAR.ARMOR.ROLL
	STA COMBAT.STATS.DAMAGE.ARMOR_TALLY+$0
	LDA COMBAT.STATS.DAMAGE.ARMOR_TALLY+$1
	ADC #$00
	STA COMBAT.STATS.DAMAGE.ARMOR_TALLY+$1

	;**FALLS THROUGH**

.REGULAR.ARMOR.DEDUCTION.DONE
	;**FALLS THROUGH**

@END
		
.RESIST_MAGIC.DEFENSE_RATING
@START		
	LDA COMBAT.STATS.DAMAGE.TYPE ;($00 = melee | $01 range, $02 spell. High-bit not set = non-magic weapon (doesn't apply to type=spell or MOBs) | high-bit set = magic weapon (doesn't apply to type=spell or MOBs)
	BMI .RESIST_MAGIC.START ;branch if magic weapon damage
	CMP #$02
	BEQ .RESIST_MAGIC.START ;branch if spell damage
	JMP .APPLY.RESIST_MAGIC.DEFENSE_RATING.DONE
	
.RESIST_MAGIC.START	

	JSR COMBAT.STATS.CALC.RESIST_SPELL_MAGIC.DEFENSE_RATING
	
	;add resist modifier to ARMOR TALLY
	LDA COMBAT.STATS.DAMAGE.ARMOR_TALLY+$0
	CLC
	ADC COMBAT.STATS.RESIST_MAGIC.DEFENSE_RATING+$0
	STA COMBAT.STATS.DAMAGE.ARMOR_TALLY+$0		
	LDA COMBAT.STATS.DAMAGE.ARMOR_TALLY+$1
	ADC COMBAT.STATS.RESIST_MAGIC.DEFENSE_RATING+$1 ;16-bit add
	STA COMBAT.STATS.DAMAGE.ARMOR_TALLY+$1	
	
.APPLY.RESIST_MAGIC.DEFENSE_RATING.DONE
	
@END
	JMP .ARMOR_TALLY.OVERFLOW.CHECK

.ARMOR_TALLY.EXCEEDS.DAMAGE.ROLL		
	LDA #$00
	STA COMBAT.STATS.DAMAGE.FINAL+$0
	STA COMBAT.STATS.DAMAGE.FINAL+$1
	JMP .FINAL.DAMAGE.CALCULATION.DONE

.ARMOR_TALLY.OVERFLOW.CHECK
@START
	LDA COMBAT.STATS.DAMAGE.ARMOR_TALLY+$1
	BEQ .ARMOR_TALLY.OVERFLOW.CHECK.DONE			
	LDA #$FF ;set max 8-bit armor tally value since the actual value overflowed
	STA COMBAT.STATS.DAMAGE.ARMOR_TALLY+$0
.ARMOR_TALLY.OVERFLOW.CHECK.DONE

	;**FALLS THROUGH**
@END


@END

;**FALLS THROUGH**


.DAMAGE.SUBTOTAL.CALCULATION
@START

		; LDA RESULT+$1
		; STA COMBAT.STATS.BASE_DAMAGE.ROLL_X2+$1

		
	LDA COMBAT.STATS.BASE_DAMAGE.ROLL_X2+$0
	CLC
	ADC COMBAT.STATS.CRTL_HIT_DAMAGE+$0
	STA COMBAT.STATS.DAMAGE.SUBTOTAL+$0
	LDA COMBAT.STATS.BASE_DAMAGE.ROLL_X2+$1
	ADC COMBAT.STATS.CRTL_HIT_DAMAGE+$1 ;16-bit addition
	STA COMBAT.STATS.DAMAGE.SUBTOTAL+$1
;**FALLS THROUGH**	
@END

;**FALLS THROUGH**


				
.FINAL.NET_DAMAGE.CALCULATION
@START
;(final damage = damage roll - armor tally)		
	
		; ;driver
		; LDA #$90
		; STA COMBAT.STATS.DAMAGE.ARMOR_TALLY+$0
		; LDA #$89
		; STA COMBAT.STATS.DAMAGE.ARMOR_TALLY+$1
		
	
;is armor tally >= damage subtotal? (if yes, final damage = 0)

	;16-bit is-greater-or-equal
	;CLC/SEC: testing confirmed that neither are needed for 16-bit >+ test
	LDA COMBAT.STATS.DAMAGE.ARMOR_TALLY+$0 ;OP1
	CMP COMBAT.STATS.DAMAGE.SUBTOTAL+$0 ;OP2
	LDA COMBAT.STATS.DAMAGE.ARMOR_TALLY+$1 ;OP1+1
	SBC COMBAT.STATS.DAMAGE.SUBTOTAL+$1 ;OP2+1
	BCS .ARMOR_TALLY.EXCEEDS.DAMAGE.ROLL ;if carry set then OP1 >= OP2, which means the target is killed 

;calculate final damage
	LDA COMBAT.STATS.DAMAGE.SUBTOTAL+$0
	SEC
	SBC COMBAT.STATS.DAMAGE.ARMOR_TALLY+$0 ;underflow check not needed because it is effectively done by the code block just above	
				;LDA #$39 ;force final damange value (LO byte)
	STA COMBAT.STATS.DAMAGE.FINAL+$0
	LDA COMBAT.STATS.DAMAGE.SUBTOTAL+$1
	SBC COMBAT.STATS.DAMAGE.ARMOR_TALLY+$1 ;underflow check not needed because it is effectively done by the code block just above	
				;LDA #$01 ;force final damange value (HO byte)
	STA COMBAT.STATS.DAMAGE.FINAL+$1
					
.FINAL.DAMAGE.CALCULATION.DONE

;**FALLS THROUGH**
	
@END
	
;**FALLS THROUGH**

			
		; ;TROUBLESHOOTING.HOOK (DAMAGE)
		; ;
		; ;
				; STA TEMP
				; LDA TROUBLESHOOTING.HOOK
				; CMP #$01
				; BEQ .HOOK.START
				; JMP .TEMP
; .HOOK.START
			; ;
			; LDA COMBAT.STATS.DAMAGE_MEDIAN
			; STA $9600			
			; ;
			; LDA COMBAT.STATS.DEFENDER.SIZE_ATTRIB
			; STA $9601
			; LDA COMBAT.STATS.SIZE_PERCENT
			; STA $9602
			; LDA COMBAT.STATS.SIZE.MODIFIER
			; STA	$9603
			; ;
			; LDA COMBAT.STATS.BASE_DAMAGE.MOD_TALLY+$0
			; STA $9604
			; LDA COMBAT.STATS.BASE_DAMAGE.MOD_TALLY+$1
			; STA $9605
			; ;
			; LDA COMBAT.STATS.BASE_DAMAGE_LO ;actual random.8 range/lo. doesn't reflect *2 to double damage without 16-bit random # 	
			; STA $9606
			; LDA COMBAT.STATS.BASE_DAMAGE_HI ;""	
			; STA $9607
			; LDA COMBAT.STATS.BASE_DAMAGE.ROLL
			; STA $9608
			; ;
			; LDA COMBAT.STATS.CRTL_HIT_DAMAGE+$0
			; STA $9609
			; LDA COMBAT.STATS.CRTL_HIT_DAMAGE+$1
			; STA $960A	
			; ;
			; ;
			; ;
			; LDA COMBAT.STATS.REGULAR.ARMOR.ROLL
			; STA $960B
			; LDA COMBAT.STATS.DEFENDER.RESIST.MAGIC
			; STA	$960C
			; LDA COMBAT.STATS.RESIST_MAGIC.PERCENT
			; STA	$960D		
			; LDA COMBAT.STATS.RESIST_MAGIC.DEFENSE_RATING+$0
			; STA	$960E
			; LDA COMBAT.STATS.DAMAGE.ARMOR_TALLY+$0
			; STA	$960F
			; LDA COMBAT.STATS.DAMAGE.ARMOR_TALLY+$1
			; STA	$9610
			; LDA RND.LO ;for armor roll
			; STA $9611
			; LDA RND.HI ;for armor roll
			; STA $9612			
			; ;
			; ;
			; ;
			; LDA COMBAT.STATS.DAMAGE.TYPE
			; STA $9613
			; LDA COMBAT.STATS.DAMAGE.SUBTOTAL+$0	 ;(base damage + critical hit)
			; STA $9614
			; LDA COMBAT.STATS.DAMAGE.SUBTOTAL+$1  ;(base damage + critical hit)
			; STA $9615
			; ;
			; LDA COMBAT.STATS.DAMAGE.FINAL+$0	;(damage subtotal - armor tally)
			; STA $9616
			; LDA COMBAT.STATS.DAMAGE.FINAL+$1	;(damage subtotal - armor tally)
			; STA $9617
			; ;	
			; LDA #$EE
			; ;LDX CHR_SHEET.PC_MOB.ARMOR ;PC defense rating
			; LDX COMBAT.STATS.DAMAGE.FINAL+$0
			; LDY COMBAT.STATS.DAMAGE.FINAL+$1
			; JSR PREP.BRK
			; BRK
			; ;
; .TEMP
			; LDA TEMP
			
			
.RECORD.DAMAGE
@START

	
			;**OPT** Memory. Speed. It may not be needed to read the character sheet again, I think the target's character sheet will already be in memory 
			;after the armor deduction routine above
			
	;read defender's (target) character sheet 
		LDY COMBAT.DEFENDER.SINDEX ;load target screen tile location
		LDA #$00 ;set read mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet
		;RETURN = CHR_SHEET.RECORD.READ(80)

.KILL_CHECK
			;======troubleshooting: hard code damage=====
			; lda #$00
			; sta COMBAT.STATS.DAMAGE.FINAL+$0
			; lda #$10
			; sta COMBAT.STATS.DAMAGE.FINAL+$1
			
	;16-bit is-greater-or-equal
 	;CLC/SEC: testing confirmed that neither are needed for 16-bit >+ test
	LDA COMBAT.STATS.DAMAGE.FINAL+$0 ;OP1
    CMP CHR_SHEET.PC_MOB.HP_LO ;OP2
    LDA COMBAT.STATS.DAMAGE.FINAL+$1 ;OP1+1
    SBC CHR_SHEET.PC_MOB.HP_HO ;OP2+1
    BCS .TARGET.IS.KILLED  ;branch if final damage >= target hit points, which means the target is killed 
						   ;(if carry set then OP1 >= OP2)
	;**FALLS THROUGH**


.APPLY.DAMAGE			
	;(target HP - damage roll)	
	LDA CHR_SHEET.PC_MOB.HP_LO ;load target hit points LO field
	SEC
	SBC COMBAT.STATS.DAMAGE.FINAL+$0
	STA CHR_SHEET.PC_MOB.HP_LO ;save target hit points LO field
	LDA CHR_SHEET.PC_MOB.HP_HO ;load target hit points HO field
	SBC COMBAT.STATS.DAMAGE.FINAL+$1 ;16-bit subtract
	STA CHR_SHEET.PC_MOB.HP_HO ;save target hit points HO field

			
	;**FALLS THROUGH**

.WRITE.TARGET.CHAR_SHEET	
		;YREG: COMBAT.DEFENDER.SINDEX
		LDA #$01 ;set write mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;write character sheet
		;RETURN = CHR_SHEET.RECORD.READ(80)

@END
		
.EXIT		
;RESTORE REGISTERS


			
			
			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
				; lda CHR_SHEET.PC_MOB.HP_LO
				; sta $9600
				; lda CHR_SHEET.PC_MOB.HP_HO
				; sta $9601
				; lda COMBAT.STATS.DAMAGE.FINAL+$0
				; sta $9602
				; lda COMBAT.STATS.DAMAGE.FINAL+$1
				; sta $9603
			; lda #$aa
			; ldy COMBAT.TARGET.SINDEX
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP

			
	PLA
	TAY
	PLA
	TAX
				
	RTS
	
.TARGET.IS.KILLED
@START
;CHR_SHEET.RECORD.READ(80) = defender character sheet
	
.RECORD.XP ;PC ONLY
@START
;PC/MOB TURN BRANCH
	LDA COMBAT.TURN_STATUS   ;($00 = PCs turn. $01 = Special(s) S_ENTITY (example Wyvern Ally, Summoned Demon), $02 = MOBs turn)
	BEQ  .CALCULATE.LEVEL.MODIFIER ;if PCs turn, then branch
	JMP .RECORD.XP.DONE 
	
	;**FALLS THROUGH**
	
.CALCULATE.LEVEL.MODIFIER
@START
;IS ATTACKER OR DEFENDER HIGHER LEVEL?
	LDA COMBAT.STATS.ATTACKER_LEVEL	
	CMP COMBAT.STATS.DEFENDER_LEVEL
	BCC .ATTACKER.LOWER.LEVEL
	
	;**FALLS THROUGH**
	
.ATTACKER.HIGHER_EQUAL.LEVEL
	;ACC =  COMBAT.STATS.ATTACKER_LEVEL
	SEC 
	SBC COMBAT.STATS.DEFENDER_LEVEL
	CMP #COMBAT.STATS.LEVEL_XP_MODIFIER.MAX.PLUS_ONE ;The max level difference between attacker and defender for which the modifier increases. 
	BCC .LTE_MAX
	;since level difference is >= MAX, reduce difference MAX for purposes of the modifier calculation
	LDA #COMBAT.STATS.LEVEL_XP_MODIFIER.MAX
	
	;**FALLS THROUGH**
	
.LTE_MAX	
	TAY
	LDA COMBAT.LEVEL_XP_MODIFIER.TABLE,Y ;add leveling modifier for (attacker level-defender level)		
	STA COMBAT.STATS.LEVEL_XP_MODIFIER

	LDA #$00
	STA COMBAT.STATS.LEVEL_XP_MODIFIER.FLAG ;($00 = attacker is higher level, or level is equal | $01 = defender is higher level)
	JMP .CALCULATE.LEVEL_MODIFIER.COMPLETE ;if no overflow 
	
.ATTACKER.LOWER.LEVEL
	LDA COMBAT.STATS.DEFENDER_LEVEL
	SEC 
	SBC COMBAT.STATS.ATTACKER_LEVEL
	CMP #COMBAT.STATS.LEVEL_XP_MODIFIER.MAX.PLUS_ONE ;The max level difference between attacker and defender for which the modifier increases. 
	BCC .LTE_MAX2
	;since level difference is >= MAX, reduce difference MAX for purposes of the modifier calculation
	LDA #COMBAT.STATS.LEVEL_XP_MODIFIER.MAX
	;** FALLS THROUGH**
.LTE_MAX2	
	TAY
	LDA COMBAT.LEVEL_XP_MODIFIER.TABLE,Y ;save leveling modifier for (defender level-attacker level)		
	STA COMBAT.STATS.LEVEL_XP_MODIFIER
	
	LDA #$01
	STA COMBAT.STATS.LEVEL_XP_MODIFIER.FLAG ;($00 = attacker is higher level, or level is equal | $01 = defender is higher level)
.CALCULATE.LEVEL_MODIFIER.COMPLETE
@END

;APPLY LEVEL MODIFIER AS %
@START
;Formula: XP +/- (XP * leveling modifier%)


	;formula: (XP * leveling modifier%)
		LDA CHR_SHEET.PC_MOB.XP ;defender XP 
		STA COMBAT.STATS.DEFENDER.XP ;save for use in the final calculation
		STA COMBAT.STATS.APPLY_PERCENT.PARM_BASE+$0
		LDA #$00
		STA COMBAT.STATS.APPLY_PERCENT.PARM_BASE+$1
		LDA COMBAT.STATS.LEVEL_XP_MODIFIER
		STA COMBAT.STATS.PARM.PERCENT
		LDA #$00	;set hex mode ($00 = hex mode | $01 = BCD mode)				
	JSR COMBAT.STATS.APPLY.PERCENTAGE
		;RETURN VALUE: result(2)
		;result+$1 ignored

		LDA RESULT+$0 ;(XP * leveling modifier%)
		STA COMBAT.STATS.XP.X.MOD

			; LDA RESULT+$0
			; LDX COMBAT.STATS.PARM.PERCENT
			; LDY COMBAT.STATS.APPLY_PERCENT.PARM_BASE+$0
			; JSR PREP.BRK
			; BRK	
			
;XP +/- 

	;read attacker character sheet
		LDY COMBAT.ATTACKER.SINDEX
		LDA #$00 ;set read mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet
		;RETURN = CHR_SHEET.RECORD.READ(80)
		
			; LDA #$AA
			; LDX CHR_SHEET.PC_MOB.XP+$0
			; LDY CHR_SHEET.PC_MOB.XP+$1
			; JSR PREP.BRK
			; BRK	
			
	;calculate XP award for the kill
	LDX COMBAT.STATS.LEVEL_XP_MODIFIER.FLAG ;($00 = attacker is higher level, or level is equal | $01 = defender is higher level)
	BNE	.DEFENDER.LEVEL.HIGHER
;.ATTACKER.LEVEL.HIGHER_EQAL
	LDA COMBAT.STATS.DEFENDER.XP ;defender XP
	SEC
	SBC	COMBAT.STATS.XP.X.MOD ;(XP * LEVEL MODIFIER%)
	STA COMBAT.STATS.XP.AWARD
	

			; LDA COMBAT.STATS.LEVEL_XP_MODIFIER
			; STA $9600
			; LDA COMBAT.STATS.DEFENDER.XP
			; STA $9601 
			; LDA COMBAT.STATS.XP.X.MOD
			; STA $9602
			

			; LDA COMBAT.STATS.DEFENDER.XP
			; LDX COMBAT.STATS.XP.X.MOD
			; LDY COMBAT.STATS.XP.AWARD
			; JSR PREP.BRK
			; BRK	
			
	JMP .ADD.XP.AWARD
	
.DEFENDER.LEVEL.HIGHER
	LDA COMBAT.STATS.DEFENDER.XP ;defender XP
	CLC
	ADC	COMBAT.STATS.XP.X.MOD ;(XP * LEVEL MODIFIER%)
	STA COMBAT.STATS.XP.AWARD

			; LDA COMBAT.STATS.DEFENDER.XP
			; LDX COMBAT.STATS.XP.X.MOD
			; LDY COMBAT.STATS.XP.AWARD
			; JSR PREP.BRK
			; BRK
			
.APPLY.LEVEL.MOD.DONE

@END

	;**FALLS THROUGH**
	
.ADD.XP.AWARD
	LDA CHR_SHEET.PC_MOB.XP+$0 ;attacker XP
	CLC
	ADC COMBAT.STATS.XP.AWARD
	STA CHR_SHEET.PC_MOB.XP+$0 ;attacker XP
	LDA CHR_SHEET.PC_MOB.XP+$1 ;attacker XP
	ADC #$00 ;16-BIT subtract
	STA CHR_SHEET.PC_MOB.XP+$1 ;attacker XP

	

.SAVE.ATTACKER.CHR_SHEET
		LDY COMBAT.ATTACKER.SINDEX
		LDA #$01 ;set write mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;write character sheet
		;RETURN = CHR_SHEET.RECORD.READ(80)


.RECORD.XP.DONE
@END

	
	;set hit/miss flag to "kill"
	;(so that other routines can easily detect that a kill, and associated map objec record delete, has occured which can help avoid accessing an empty map object record for something)
	LDA #$02
	STA COMBAT.STATS.HIT_MISS_KILL.FLAG ;($00 = hit | $01 = miss | $02 = kill)

	
;UPDATE DEFENDER CHR_SHEET FIELDS 

	;read defender's (target) character sheet 
		LDY COMBAT.DEFENDER.SINDEX ;load target screen tile location
		LDA #$00 ;set read mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet
		;RETURN = CHR_SHEET.RECORD.READ(80)

	;set hit points to $00 ;**OPT** Memory. This code is only needed if mob hit points are displayed, which isn't planned for the production version
	LDA #$00 
	STA CHR_SHEET.PC_MOB.HP_LO ;save hit points LO byte
	STA CHR_SHEET.PC_MOB.HP_HO ;save hit points HO byte
	;set health status to dead
	LDA #COMBAT.S_ENTITY.STATUS.DEAD
	STA CHR_SHEET.PC.HEALTH_STATUS
	
		LDY COMBAT.DEFENDER.SINDEX
		LDA #$01 ;set write mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;write character sheet
		;RETURN = CHR_SHEET.RECORD.READ(80)

		; ;Y-REG = COMBAT.DEFENDER.SINDEX
	; JSR MAP_OJECTS.FIND.MTT.KEY
		
;Identify MTT index tile (this routine can be run for single-tile S_ENTITIES without any harm)
@start

	;(if S_ENTITY is a multi-tile mob, Y-REG might be the screen index to a tile of the MTT other than the upper left corner)
	;(the routines below require Y-REG to be set to the upper left corner tile of a MTT)
	LDA SCREEN.MO_SPRITE.DATA,Y ;load map object record index of the killed S_ENTITY 
	STA SAVED.ACC.LOCAL
	LDA SCREEN.MO_SPRITE_TYPE.DATA,Y ;load type code of the killed S_ENTITY 
	AND #$87 ;mask-out MTT flags (bits 3-6)
	STA SAVED.ACC.LOCAL2
		;**OPT** Memory. I think this true-up section can be removed now that the MTT flag bits are used, or at least rewritten more efficiently. It should be able
					;to be removed if MTT mobs attacking each other are not allowed to select a target other than their MTT index keys (see combat.acquire.target)
		
		
			; ;lda #$aa
			; LDA SAVED.ACC.LOCAL
			; LDX SAVED.ACC.LOCAL2
			; jsr prep.brk
			; brk
			
	LDY #$00 ;init screen index counter
.TRUE_UP.LOOP
	;(search sprite screen array until the map object record index of the killed S_ENTITY is found. The first instance found of the MO index
	;is the upper left tile if S_ENTITY is an MTT. If S_ENTITY is single tile then it is still the one we want because there is only one MO index
	;for a single tile S_ENTITY)
	LDA SCREEN.MO_SPRITE.DATA,Y
	CMP SAVED.ACC.LOCAL
	BNE .INCREMENT.INDEX
	LDA SCREEN.MO_SPRITE_TYPE.DATA,Y
	AND #$87 ;mask-out MTT flags (bits 3-6)
	CMP SAVED.ACC.LOCAL2
	BEQ .TRUE_UP.LOOP.COMPLETE
	;**FALLS THROUGH**
.INCREMENT.INDEX	
	INY ;next screen tile location
	;DEY ;reverse last increment to report the screen tile location that triggered the CMP
	JMP .TRUE_UP.LOOP
.TRUE_UP.LOOP.COMPLETE	
@end


				; lda #$ab
				; jsr prep.brk
				; brk				

	;erase map object record, and set health status to dead
		;Y-REG: screen tile location of the target S_ENTITY
	JSR DELETE.S_ENTITY
		;return value: ACC = ($00 = deleted S_ENTITY was single tile, $01 = deleted S_ENTITY was multi-tile)


				
;DRAW TERRAIN TILE UNDER S_ENTITY
	;(this is needed because map_objects.manager won't draw anything in that tile since the S_ENTITY is dead)

	;;ACC = ($00 = deleted S_ENTITY was single tile, $01 = deleted S_ENTITY was multi-tile)
	CMP #$01
	BEQ .DRAW_ERASE.MTT ;branch if S_ENTITY is multi-tile
;.DRAW_ERASE.SINGLE_TILE
	JSR .DRAW.TILE.SINGLE ;local routine which does the draw to the background and foreground pages		

	
		
	JMP .EXIT
	
.DRAW_ERASE.MTT	
	;start in the upper left corner of the MTT mob (which Y-REG is already set to)
	JSR .DRAW.TILE.SINGLE ;local routine which does the draw to the background and foreground pages

	INY ;move to the upper right tile of the MTT MOB
	
	JSR .DRAW.TILE.SINGLE ;local routine which does the draw to the background and foreground pages

	TYA
	CLC
	ADC #SCREEN.ARRAY.OFFSET  ;move to the lower right tile of the MTT MOB
	TAY

		; lda #$aa
		; jsr prep.brk
		; brk
		
	JSR .DRAW.TILE.SINGLE ;local routine which does the draw to the background and foreground pages
	
	DEY ;move to the lower left tile of the MTT MOB

	JSR .DRAW.TILE.SINGLE ;local routine which does the draw to the background and foreground pages

			
	JMP .EXIT

.DRAW.TILE.SINGLE ;draw terrain tile to both hi-res pages
;parameters: YREG (screen tile location to erase)

;**OPT** Memory. Are there other occasions where a background and foreground draw occurs by making two calls? If so it may
;make sense to put this routine in the general toolbox. Also,
;even for routines that only do a foreground draw, 6 bytes could be saved by having
;just a foreground draw routine because the LDA and STA to set the parameters would be saved. In areas where speed isn't an issue
;this could make sense too. 

		;Y-REG: screen tile location of the target S_ENTITY
		LDA #$01 ;set foreground override
		STA PAGE.FOREGROUND.OVERRIDE
	JSR DRAW.TILE.SINGLE
		;background set by default
	JSR DRAW.TILE.SINGLE
	
	RTS
@END
@END
	

;SUPPORTING SUBROUTINES
GET.ATTACKER_DEFENDER.CHR_SHEET.DATA
@START

			
;PARAMETERS: Y-REG: sindex of attacker, COMBAT.DEFENDER.SINDEX
;ENTRANCE: COMBAT.HIT_MISS.ROLL
;RETURN: CHR_SHEET.RECORD.READ

;SAVE PARAMETERS
	;ACC = Y-REG parm (sindex of attacker)
	STY COMBAT.ATTACKER.SINDEX

	
.INIT
	LDA #$00
	STA COMBAT.STATS.DEFENDER.DODGE_SKILL ;needed since the variable might not get set if this routine was called for mobs and the mob is using that field for size instead (I think, writing this note after the fact)
	
	
;LOAD CHARACTERS SHEET DATA

	;read defender (target) character sheet 
		LDY COMBAT.DEFENDER.SINDEX ;load defender (target) sindex
		LDA #$00 ;set read mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet
		;RETURN: CHR_SHEET.RECORD.READ($80)

;.SET.DEFENDER.MOB_PC.FIELDS
	
		;set PC/MOB common fields
		LDA CHR_SHEET.PC_MOB.LEVEL ;load defender level (only used if PC is defender, but mob character sheet also has this field)
		STA COMBAT.STATS.DEFENDER_LEVEL
		
		LDA CHR_SHEET.PC_MOB.RESIST_MAGIC ;load defender resist magic
		STA COMBAT.STATS.DEFENDER.RESIST.MAGIC


			; ;TROUBLESHOOTING LOOP
			; LDX #$00
; .TEST.LOOP			
			; LDA CHR_SHEET.RECORD.READ,X
			; STA $BF00,X
			; INX
			; BNE .TEST.LOOP
			
			; LDA #$AA
			; ;LDX #$AA
			; LDX CHR_SHEET.PC_MOB.ARMOR ;set parm: range median
			; LDY COMBAT.DEFENDER.SINDEX
			; JSR PREP.BRK
			; BRK
			
; ;;PC CHARACTER SHEET READ
	; ;read PC character sheet data
		; LDA #$01
		; ;ACC = player sequential # (high-bit not set = read mode)
	; JSR COMBAT.READ_WRITE.CHR_SHEET.PC
		; ;RETURN VALUE = CHR_SHEET.RECORD.READ
		
			; ;TROUBLESHOOTING LOOP
			; LDX #$00
; .TEST.LOOP			
			; LDA CHR_SHEET.RECORD.READ,X
			; STA $BF00,X
			; INX
			; BNE .TEST.LOOP
		
			; LDA #$AA
			; JSR PREP.BRK
			; BRK
			
			
		LDA CHR_SHEET.PC_MOB.ENGAGED.SINDEX
		STA COMBAT.STATS.DEFENDER.ENGAGED.MO_INDEX

				
		;pc/mob turn branch
		LDA COMBAT.TURN_STATUS   ;($00 = PCs turn. $01 = Special(s) S_ENTITY (example Wyvern Ally, Summoned Demon), $02 = MOBs turn)
		BEQ .SET.DEFENDER.MOB.FIELDS ;if PCs turn, then branch
.SET.DEFENDER.PC.FIELDS	
		;set PC fields which are not the same byte # in the PC and MOB character sheet datagram
		LDA CHR_SHEET.PC.ATTRIB.INT
		STA COMBAT.STATS.DEFENDER.INT

		LDA CHR_SHEET.PC.READIED_EQUIPMENT_WEIGHT
		STA COMBAT.STATS.DEFENDER.READIED_EQUIPMENT_WEIGHT
		
		LDA CHR_SHEET.PC.SKILL.DODGE ;load dodge/parry skill
		STA COMBAT.STATS.DEFENDER.DODGE_SKILL

		LDA CHR_SHEET.PC.RESIST_CRITICAL_HIT
		STA COMBAT.STATS.DEFENDER.RESIST_CRITCAL_HIT

		JMP .SET.DEFENDER.MOB_PC.FIELDS.DONE
		
.SET.DEFENDER.MOB.FIELDS
		;set MOB fields which are not the same byte # in the PC and MOB character sheet datagram
		LDA CHR_SHEET.MOB.INT.WP_SHAPE_TYPE
		AND #$7F ;mask-out high-bit
		STA COMBAT.STATS.DEFENDER.INT

		LDA CHR_SHEET.MOB.RESIST_CRITICAL_HIT
		STA COMBAT.STATS.DEFENDER.RESIST_CRITCAL_HIT
	
		LDA CHR_SHEET.MOB.SIZE_DODGE ;load size/dodge-parry skill		
		STA COMBAT.STATS.DEFENDER.SIZE_ATTRIB ;**note** even though the size variable is set when the field is used for dodge, entrance to .APPLY.SIZE.TO_HIT.MODIFIER is prevented by checking to see if dodge skill >0. 
											  ;.ROLL.DODGE.SAVING_THROW prevents entrance when dodge is not in use by checking for high-bit.
		BPL .SET.DEFENDER.MOB_PC.FIELDS.DONE ;branch if field is in use for size instead of dodge skill (high bit not set)
		AND #$7F ;mask-out high-bit
		STA COMBAT.STATS.DEFENDER.DODGE_SKILL	


		;**FALLS THROUGH**
			
		;TROUBLESHOOTING HOOK
		;(also enable the one below, then observe 
		;which one triggers, testing the branch above)
		;
		; lda #$aa
		; LDX CHR_SHEET.MOB.SIZE_DODGE ;load size/dodge-parry skill
		; LDY COMBAT.STATS.DEFENDER.DODGE_SKILL	
		; jsr prep.brk
		; brk
		;
		; lda #$aa
		; LDX COMBAT.STATS.DEFENDER.SIZE_ATTRIB
		; LDY COMBAT.STATS.DEFENDER.DODGE_SKILL	
		; jsr prep.brk
		; brk
		
.SET.DEFENDER.MOB_PC.FIELDS.DONE

			
		; ;TROUBLESHOOTING HOOK
		; ;(also enable the one above, then observe 
		; ;which one triggers, testing the branch above)
		;
		; lda #$ab
		; LDX CHR_SHEET.MOB.SIZE_DODGE ;load size/dodge-parry skill
		; LDY COMBAT.STATS.DEFENDER.DODGE_SKILL	
		; jsr prep.brk
		; brk
		;
		; lda #$ab
		; LDX COMBAT.STATS.DEFENDER.SIZE_ATTRIB
		; LDY COMBAT.STATS.DEFENDER.DODGE_SKILL	
		; jsr prep.brk
		; brk


; ;TROUBLESHOOTING HOOK
; ;(designed to trace unexpected breaks
; ;occuring in COMBAT.READ_WRITE.CHR_SHEET caused by an invalid attacker SINDEX,
; ;which causes and invalid sprite type, which triggeres the error trap replicated below)
; ;the hook returns the RTS address of the routine that called COMBAT.HIT_MISS.ROLL or COMBAT.DAMAGE.ROLL
		; ;
		; LDY COMBAT.ATTACKER.SINDEX
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
; .S_ENTITY_TYPE.ERROR
; ;.DETERMINE.S_ENTITY.TYPE (CL.S_ENTITY.FOUND) reports unexpected S_ENTITY type value.
		; ; LDA #SELECT_TARGET.LAST_TARGET
		; ; LDX /SELECT_TARGET.LAST_TARGET
; ;
		; ;		 
; ;
			; pla ;pop RTS from GET.ATTACKER_DEFENDER.CHR_SHEET.DATA,
			; pla
			; ;
			; pla ;pop save registers for combat damage roll or combat to hit roll
			; pla
			; ;
			; pla ;pop RTS from to combat to-hit/damage roll
			; tax
			; pla
			; tay
	; lda #$ee
	; ;ldx COMBAT.PC.ACTIVE.SINDEX
	; JSR PREP.BRK
	; BRK
; .TEMP
	
		;**FALLS THROUGH**
			
	;read attacker character sheet
		LDY COMBAT.ATTACKER.SINDEX
		LDA #$00 ;set read mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet
		;RETURN: CHR_SHEET.RECORD.READ($80)
		LDA CHR_SHEET.PC_MOB.LEVEL ;load attacker level
		STA COMBAT.STATS.ATTACKER_LEVEL	

		
	RTS
@END

;**this routine also exists multiple times in the spell file, search for .SET.CRITICAL_HIT.FLAGS
COMBAT.STATS.UPDATE.ENGAGED ;**OPT** If there are any other occasions where just one character sheet field is updated, consider modifying this routine so it can modify any char sheet field, passing the field # as a parm in variable that is LDX
@START
;PARAMETERS: YREG = screen index of attacker, ACC = value to write to CHR_SHEET.PC_MOB.ENGAGED.SINDEX
;ENTRANCE: direct
;RETURN: saved attacker character sheet with updated CHR_SHEET.PC_MOB.ENGAGED.SINDEX field

.SAVE.PARAMETERES
	;ACC = value to write to CHR_SHEET.PC_MOB.ENGAGED.SINDEX
	STA COMBAT.ENGAGED_SINDEX.UPDATE.PARM
	
	;read attacker (MOB) character sheet
		
		;YREG = COMBAT.ATTACKER.SINDEX / COMBAT.PC.ACTIVE.SINDEX
		LDA #$00 ;set read mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet

			
	;set the engaged field to the updated value
	;(expected to be the screen index of the defender or $FF to indicate no engagement)
	LDA COMBAT.ENGAGED_SINDEX.UPDATE.PARM
	STA CHR_SHEET.PC_MOB.ENGAGED.SINDEX

			
		;YREG = COMBAT.ATTACKER.SINDEX
		LDA #$01 ;set write mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet

	RTS
@END

COMBAT.STATS.CALC.RANGE ;uses +/10% spread
@START
;PARAMETERS: ACC =  median value range is based on
;ENTRANCE: direct
;RETURN: RND.LO, RND.HI

	;SAVE PARAMETERES
	STA COMBAT.STATS.CALC.RANGE.MEDIAN.PARM+$0

	
		;ACC = COMBAT.STATS.CALC.RANGE.MEDIAN.PARM+$0
		STA COMBAT.STATS.APPLY_PERCENT.PARM_BASE+$0
		LDA #$00
		STA COMBAT.STATS.APPLY_PERCENT.PARM_BASE+$1
		LDA #COMBAT.STATS.DAMAGE.RANGE.PERCENT ;!10%
		STA COMBAT.STATS.PARM.PERCENT
		LDA #$00	;set hex mode ($00 = hex mode | $01 = BCD mode)				
	JSR COMBAT.STATS.APPLY.PERCENTAGE
		;RETURN VALUE: result(2)
		;result+$1 ignored
	
	LDA COMBAT.STATS.CALC.RANGE.MEDIAN.PARM
	CMP RESULT+$0 ;(median range value * 25%)	
    BCS .SET.RANGE_LO.STANDARD ;branch if median base damage is >= result, which is about to be subtracted from it
						   ;and thus no underflow should occur
	
	;set range lo to zero to avoid underflow
	LDA #$00
	STA RND.LO ;save range LO field 	
	JMP .SET.RANGE_HI.STANDARD
	
.SET.RANGE_LO.STANDARD
	LDA COMBAT.STATS.CALC.RANGE.MEDIAN.PARM
	SEC
	SBC RESULT+$0 ;(median range value * 25%)
	STA RND.LO ;save range LO field 	

	;**FALLS THROUGH**
	
.SET.RANGE_HI.STANDARD

	LDA RESULT+$0 ;(median range value * 25%)
	ASL ;2X (converts value to equal the full size of the range, from the LO value to the HI value)
	CLC
	ADC RND.LO ;save range LO field
	STA RND.HI ;save damage HI field 	
	
	;**FALLS THROUGH**
	
.EXIT
				; LDA #$AA
				; ; LDX COMBAT.STATS.BASE_DAMAGE.MOD_TALLY+$0
				; ; LDY COMBAT.STATS.BASE_DAMAGE.MOD_TALLY+$1
				; LDX RND.LO
				; LDY RND.HI
				; ;LDY RESULT+$0
				; JSR PREP.BRK
				; BRK
				
	RTS
@END

COMBAT.RANDOM.8.BCD
@START
;PARAMTERS: none
;ENTRANCE: direct
;RETURN: BCD random number !1-!100

;get random number !1-!199, then convert it to !1-!99
		;LDA #$64	;=!100
		LDA #$C7	;=!200
		STA RND.HI
		LDA #$01 
		STA RND.LO
	JSR RANDOM.8
		;ACC = random # (HEX)
	CMP #$64
	BCC .LESS
	SEC
	SBC #$64
	;LSR ;/2
.LESS	
	JSR CONVERT.HEX.8_TO_BCD.16
	LDA BCD+$0 ;load converted value

	RTS
@END

COMBAT.STATS.CALC.SIZE.MODIFIER.PERCENT
@START
;PARAMETERS: *COMBAT.STATS.DEFENDER.SIZE_ATTRIB
;ENTRANCE: COMBAT.DAMAGE.ROLL
;RETURN: COMBAT.STATS.SIZE_PERCENT, COMBAT.STATS.SIZE.MODIFIER.RETURN_FLAG

;***WARNING** all parameters are expected as HEX, not BCD

	LDA COMBAT.STATS.DEFENDER.SIZE_ATTRIB
	CMP #COMBAT.STATS.SIZE.LT_SMALL
	BCC .CALC.PERCENT.SMALL_MOB
	CMP #COMBAT.STATS.SIZE.GRE_LARGE
	BCS .CALC.PERCENT.LARGE_MOB
	;default: modifier = $00
	; LDA #$00
	; STA COMBAT.STATS.SIZE.MODIFIER+$0
	; LDA #$00
	; STA COMBAT.STATS.SIZE.MODIFIER+$1	
	LDA #$FF ;set return value (indicates size modifier doesn't apply)
	JMP .EXIT.ALTERNATE	

.CALC.PERCENT.SMALL_MOB
;Formula1: M% = !80 ($50) / size

		;calculate formula1: M%
		LDA #$00 ;load return value 
		STA COMBAT.STATS.SIZE.MODIFIER.RETURN_FLAG ;($00 = small mob | $01 larger mob | $FF = medium mob -no modifier)
	
		LDA COMBAT.STATS.DEFENDER.SIZE_ATTRIB
		STA DIVISOR
		LDA #$00
		STA DIVISOR+$1
		LDA	#COMBAT.STATS.SIZE_MOD.LT_DEC.DIVIDEND
		STA DIVIDEND
		LDA #$00
		STA DIVIDEND+$1
	JSR DIV.16						
		LDA RESULT ;(QUOTIENT of the division)
	JMP .CALCULATE.SIZE_PERCENT.COMPLETE


.CALC.PERCENT.LARGE_MOB
;Formula1: M% = size * 4

	;calculate formula1: M%
	LDA #$01 ;load return value
	STA COMBAT.STATS.SIZE.MODIFIER.RETURN_FLAG ;($00 = small mob | $01 larger mob | $FF = medium mob -no modifier)

	LDA COMBAT.STATS.DEFENDER.SIZE_ATTRIB
	ASL ;X2
	ASL ;X4
.CALCULATE.SIZE_PERCENT.COMPLETE
	;ACC = COMBAT.STATS.SIZE_PERCENT
	STA COMBAT.STATS.SIZE_PERCENT ;**OPT** Memory. only needed for debugging (search for other instances of this variable and disable)
	STA COMBAT.STATS.PARM.PERCENT

	
.EXIT
	LDA COMBAT.STATS.SIZE.MODIFIER.RETURN_FLAG ;($00 = small mob | $01 larger mob | $FF = medium mob -no modifier)

	;LDA #$AA
	;ldx COMBAT.STATS.SIZE.MODIFIER.RETURN_FLAG
	;lda COMBAT.STATS.SIZE.MODIFIER
	;LDX COMBAT.STATS.TO_HIT.MOD_TALLY+$0	
	;LDY COMBAT.STATS.TO_HIT.MOD_TALLY+$1
	;ldy COMBAT.STATS.DEFENDER.SIZE_ATTRIB
	; pla
	; tax
	; pla
	; tay
	; lda #$ee
	; JSR PREP.BRK
	; BRK
	
.EXIT.ALTERNATE			
	RTS
@END

COMBAT.STATS.APPLY.PERCENTAGE	;cannot be combined with INV_8.APPLY.PERCENTAGE.HEX_ONLY becuase the INV_8 routine has no BCD support because it uses MLP.32 and DIV.32 to support large base numbers. 
@START
;PARAMETERS: ;ACC: ($00 = hex mode | $01 = BCD mode)*, COMBAT.STATS.PERCENT.PARM, COMBAT.STATS.APPLY_PERCENT.PARM_BASE(2)
;ENTRANCE: COMBAT.DAMAGE.ROLL
;RETURN: RESULT(2)

;=====================SUBROUTINE DOCUMENTATION====================================
;
;(to avoid using floating decimals, this routine applies the percent using the
;the formula: %(as integer) * number / 100, which yields the same result as number * percent (as decimal)
;So instead of .75 * 50, this routine would calculate 75*50/100
;
;=================================================================================


;Formula: RESULT(2) = base number * M% / 100

;**WARNING**: this routine is setup for the some parms to be submitted as hex and some 
;as BCD when using BCD mode
;	COMBAT.STATS.PERCENT.PARM: submit as hex
;	COMBAT.STATS.APPLY_PERCENT.PARM_BASE(2): submit as BCD
;

		
		;ACC = parm: mode ($00 = hex mode | $01 = BCD mode)
		STA  COMBAT.STATS.APPLY.PERCENT.MODE
		BEQ .HEX.ENTRANCE ;branch if BCD mode

			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP1
			; ; LDA MULCND.COUNTER
			; ; CMP #$01
			; ; BNE .TEMP
			
			; ;LDA #$AA
			; LDX #$DD
			; JSR PREP.BRK
			; BRK
; .TEMP1
			; LDA TEMP
				
			LDA COMBAT.STATS.PARM.PERCENT
		JSR CONVERT.HEX.8_TO_BCD.16
			;RETURN VALUE: BCD+$0
			LDA BCD+$0
			STA COMBAT.STATS.PARM.PERCENT
		SED ;set decimal mode
		;ACC: size modifier multiplier
.HEX.ENTRANCE
		LDA COMBAT.STATS.PARM.PERCENT
		STA MULPLR+$0
		LDA #$00
		STA MULPLR+$1
		;			
		LDA COMBAT.STATS.APPLY_PERCENT.PARM_BASE+$0
		STA MULCND+$0
		LDA COMBAT.STATS.APPLY_PERCENT.PARM_BASE+$1
		AND #$7F ;strip off high-bit (see .INIT.TO_HIT.TALLY for docs on the special use of the high-bit for this variable)
		STA MULCND+$01			
		;SED (set decimal mode is done above)
	JSR MLP.16	;supports HEX and BCD
		;CLD (clear decimal mode is done below)
		LDA RESULT+$0 ;load multiplication return value LO byte
		STA DIVIDEND+$0
		LDA RESULT+$1 ;load multiplication return value HO byte
		STA DIVIDEND+$1	


			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
				; ; LDA COMBAT.STATS.DEFENDER.RESIST.MAGIC
				; ;LDA COMBAT.STATS.PARM.PERCENT
				; LDA MULPLR+$1
				; LDX RESULT+$0
				; LDY RESULT+$1
				; ; LDX COMBAT.STATS.BASE_DAMAGE.MOD_TALLY+$0
				; ; LDY COMBAT.STATS.BASE_DAMAGE.MOD_TALLY+$1				STA COMBAT.STATS.APPLY_PERCENT.PARM_BASE+$1
				; ; LDX COMBAT.STATS.RESIST_MAGIC.DEFENSE_RATING
			; ; LDX COMBAT.STATS.APPLY_PERCENT.PARM_BASE+$0
			; ; LDY COMBAT.STATS.APPLY_PERCENT.PARM_BASE+$1
			; ; LDX MULCND+$0
			; ; LDY MULCND+$1
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP

			
		;
	LDA COMBAT.STATS.APPLY.PERCENT.MODE	;($00 = hex mode | $01 = BCD mode)	
	BNE .BCD.MODE 
		LDA #$64	;!100
		STA DIVISOR+$0
		LDA #$00
		STA DIVISOR+$1
	JSR DIV.16 ;hex division routine
		JMP .EXIT
.BCD.MODE		
		LDA #$00
		STA DIVISOR+$0
		LDA #$01	;$64
		STA DIVISOR+$1
	JSR DIV.16.BCD
		CLD ;clear decimal mode

		
.EXIT
			
	RTS
@END	
	
COMBAT.STATS.CALC.RESIST_SPELL_MAGIC.DEFENSE_RATING
@START
;PARAMTERS: none
;ENTRANCE: COMBAT.DAMAGE.ROLL
;RETURN: COMBAT.STATS.RESIST_MAGIC.DEFENSE_RATING

;=====================SUBROUTINE DOCUMENTATION====================================
;
;(to avoid using floating decimals, this routine applies the percent using the
;the formula: %(as integer) * number / 100, which yields the same result as number * percent (as decimal)
;So instead of .75 * 50, this routine would calculate 75*50/100
;
;=================================================================================


;formula1: mod = damage median * percent
;formula2: percent = (resist*resist)/(resist*2)


			; ;***TESTING. REMOVE
			; LDA #$0E
			; STA COMBAT.STATS.BASE_DAMAGE.ROLL
			
;check floor value
		LDA COMBAT.STATS.DEFENDER.RESIST.MAGIC
		CMP #COMBAT.STATS.CALC.RESIST_SPELL_MAGIC.DEFENSE_RATING.FLOOR
		BCC .SET.ZERO.MODIFIER

;(resist*resist)
		
		;ACC = COMBAT.STATS.DEFENDER.RESIST.MAGIC
		STA MULPLR
		LDA #$00
		STA MULPLR+$1
		;
		LDA COMBAT.STATS.DEFENDER.RESIST.MAGIC
		STA MULCND
		LDA #$00
		STA MULCND+$01
	JSR MLP.16.NO_BCD
		;RETURN VALUE: result+$0 (product LO), result+$1 (product HO)
		LDA RESULT+$0 ;(resist * resist)
		STA COMBAT.STATS.RESIST_MAGIC.R_X_R+$0
		LDA RESULT+$1
		STA COMBAT.STATS.RESIST_MAGIC.R_X_R+$1

;(resist*4)		
		LDA COMBAT.STATS.DEFENDER.RESIST.MAGIC
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
		LDA RESULT+$0 ;(resist * 2)
		STA DIVISOR+$0     ;number to divide by
		LDA RESULT+$1
		STA DIVISOR+$1

;(resist*resist)/(resist*2)
		LDA COMBAT.STATS.RESIST_MAGIC.R_X_R+$0
		STA DIVIDEND+$0	;number to be divided
		LDA COMBAT.STATS.RESIST_MAGIC.R_X_R+$1
		STA DIVIDEND+$1
	JSR DIV.16			;(dividend/divisor)			
		LDA RESULT+$1 ;load quotient HO byte
		BEQ .8_BIT.RESULT	;branch if quotient is an 8-bit result
		;quotient is 16-bit
		;(ignore HO byte of result and set the return value as the 8-bit max)
		LDA #$FF
		JMP .SAVE.PERCENT
.SET.ZERO.MODIFIER
;this routine is related to ";check floor value" at the start of this routine
;not the JSR DIV.16 results immediately above
		LDA #$00
		STA COMBAT.STATS.RESIST_MAGIC.DEFENSE_RATING
				
		JMP .EXIT
		
.8_BIT.RESULT
		LDA RESULT+$0 ;(QUOTIENT of the division)
.SAVE.PERCENT
		STA COMBAT.STATS.RESIST_MAGIC.PERCENT ;**OPT** Memory. only needed for debugging. (search for other instances of this variable and disable)
		STA COMBAT.STATS.PARM.PERCENT
		;**FALLS THROUGH**
.APPLY.PERCENT				
		LDA COMBAT.STATS.BASE_DAMAGE.ROLL_X2+$0
		STA COMBAT.STATS.APPLY_PERCENT.PARM_BASE+$0
		LDA COMBAT.STATS.BASE_DAMAGE.ROLL_X2+$1
		STA COMBAT.STATS.APPLY_PERCENT.PARM_BASE+$1


				
			; LDA #$01
			; STA TROUBLESHOOTING.HOOK
			
		;COMBAT.STATS.PERCENT.PARM already set
		LDA #$00	;set hex mode ($00 = hex mode | $01 = BCD mode)				
	JSR COMBAT.STATS.APPLY.PERCENTAGE
		LDA RESULT+$0
		STA COMBAT.STATS.RESIST_MAGIC.DEFENSE_RATING+$0
		LDA RESULT+$1
		STA COMBAT.STATS.RESIST_MAGIC.DEFENSE_RATING+$1

		
			; LDA #$CD
			; LDX COMBAT.STATS.RESIST_MAGIC.DEFENSE_RATING
			; LDY COMBAT.STATS.BASE_DAMAGE.ROLL
			; ; LDX RESULT+$0
			; ; LDY rESULT+$1
			; ; LDX COMBAT.STATS.PARM.PERCENT
			; ; LDY MULPLR+$0
			; ; LDX MULPLR.TALLY+$0
			; ; LDY MULPLR.TALLY+$1
			; JSR PREP.BRK
			; BRK
			
.EXIT


		
		
	RTS
@END

;COMBAT CHARACTERS SHEET READ/WRITE ROUTINES
@START
;(these routines used to be in the combat module but were moved
;to the main toolbox (MAP_TOOLS.ASM for now but will eventually be split off) to support access from other modules like inventory)
;
;**OPT** Memory. To free up main program memory, move these routines back to the COMBAT module and
;create duplicates for any other modules that need them, like the INVENTORY module
;Another option is to put these routines at the very start of the combat module, and tied the end address to a constant. 
;In the inventory module use the constant to reserve memory at the start of the module so that the character sheet read/write routines
;can be read from the combat file on disk into the reserved memory when the inventory module is loaded.
;I think this is the way to go. 


@END

;LOCAL VARIABLES
@START


;BCD version
COMBAT.LEVEL_XP_MODIFIER.TABLE		.HS 00.04.10.24.40.64

@END
