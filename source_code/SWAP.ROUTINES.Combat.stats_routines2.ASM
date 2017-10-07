;************************INCLUDE FILE*****************************
;(do not assemble stand-alone.  )


COMBAT.HIT_MISS.ROLL
@START
;PARAMETERS: Y-REG: sindex of attacker, COMBAT.DEFENDER.SINDEX, COMBAT.STATS.DAMAGE.TYPE
;ENTRANCE: direct
;RETURN:  ACC & COMBAT.STATS.HIT_MISS_KILL.FLAG = ($00 = hit | $01 = miss)
;RETURN:  because it is convenient, COMBAT.READ_WRITE.CHR_SHEET = character sheet data for COMBAT.ACTIVE_PLAYER.SINDEX. Used by the calling routine for unrelated functions for efficiency. 

;=====================SUBROUTINE DOCUMENTATION====================================
;
;TO-HIT calculations are done in BCD. This is so that a random number can be drawn (!1-!100) can be drawn
;in alignment with the % scale. Otherwise we'd be getting a random # $0-$FF which would have to be converted to be aligned
;with the decimal % scale. That would take a lot of memory, whereas forcing the random numbers into the !1-!100 range takes more speed.
;
;=================================================================================
			
;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA

	
.temp.test.random.numbers
@START
; ;generates a page of BCD random numbers ($1-$99)) and
; ;tallies the number that fall into the ranges
; ;specified below. The tallies are stored in $BF00-$BF03

	; LDA #$00	;init tally memory locations
	; STA $BF00
	; STA $BF01
	; STA $BF02
	; STA $BF03
	; TAX ;init loop counter
; .temp.test.random.numbers.loop

; ;get random number !1-!199, then convert it to !1-!99
	; JSR COMBAT.RANDOM.8.BCD
		; ;RETURN VALUE = BCD random # !1-!99

	; cmp #$25
	; bcc .LT.25
	; cmp #$50
	; bcc .GRE.25.LT.50
	; cmp #$75
	; bcc .GRE.50.LT.75

; ;GRE.76.LTE.99
	; INC $BF03
	; JMP .INC.LOOP.COUNTER
; .LT.25
	; INC $BF00
	; JMP .INC.LOOP.COUNTER
; .GRE.25.LT.50
	; INC $BF01
	; JMP .INC.LOOP.COUNTER
; .GRE.50.LT.75
	; INC $BF02

; .INC.LOOP.COUNTER
	; INX
	; CPX #$14
	; BNE .temp.test.random.numbers.loop
	
	
	; LDA #$AA
	; JSR PREP.BRK
	; BRK
; .RANDOM.NUMBER.TEST.END
	
@END


.INIT

	JSR GET.ATTACKER_DEFENDER.CHR_SHEET.DATA
	
.CALCULATE.TO_HIT.PERCENTAGE
	
.INIT.TO_HIT.TALLY
@START
;***NOTE: the tally is init to $8000 so that all modified can be added/subtracted without a 16-bit underflow occuring, and in a way 
;where an 8-bit underflow/overflow in the LO byte during the tally can be detected when the tally is done so that the minimum or maximum TO-HOT can be used. 
;after all modifiers are added/subtracted to/from base TO-HIT, if high-bit of HO byte is not set then 8-bit underflow has occured. 
;If no underflow has occurred, then mask out high-bit of HO byte. If HO byte != $00 then 8-bit overflow has occured 

	;init tally and add the base TO-HIT value from character sheet
	LDA CHR_SHEET.PC_MOB.TO_HIT ;attacker to-hit field
	STA COMBAT.STATS.TO_HIT.MOD_TALLY+$0
	LDA #$80
	STA COMBAT.STATS.TO_HIT.MOD_TALLY+$1
		
	;**FALLS THROUGH**
@END
	;**FALLS THROUGH**
	
.APPLY.MODIFIERS
@START

.APPLY.COMMON_MODIFIERS
@START

;APPLY LEVEL MODIFIER
	LDA COMBAT.STATS.ATTACKER_LEVEL	
	CMP COMBAT.STATS.DEFENDER_LEVEL
	BCC .ATTACKER.LOWER.LEVEL
	;**FALLS THROUGH**
	

.ATTACKER.HIGHER_EQUAL.LEVEL
	;ACC =  COMBAT.STATS.ATTACKER_LEVEL
	SEC 
	SBC COMBAT.STATS.DEFENDER_LEVEL
	CMP #COMBAT.STATS.LEVEL_TO_HIT_MODIFIER.MAX.PLUS_ONE ;The max level difference between attacker and defender for which the modifier increases. 
	BCC .LTE_MAX
	;since level difference is >= MAX, reduce difference MAX for purposes of the modifier calculation
	LDA #COMBAT.STATS.LEVEL_TO_HIT_MODIFIER.MAX
	
	;**FALLS THROUGH**
	
.LTE_MAX	
	TAY
	LDA COMBAT.LEVEL_TO_HIT_MODIFIER.TABLE,Y ;add leveling modifier for (attacker level-defender level)		
	STA COMBAT.STATS.LEVEL_TO_HIT_MODIFIER
	SED ;set decimal mode
	CLC
	ADC CHR_SHEET.PC_MOB.TO_HIT ;add attacker to-hit field
	STA COMBAT.STATS.TO_HIT.MOD_TALLY+$0
	LDA COMBAT.STATS.TO_HIT.MOD_TALLY+$1
	ADC #$00 ;16-bit add
	STA COMBAT.STATS.TO_HIT.MOD_TALLY+$1
	CLD ;clear decimal mode

	JMP .APPLY.LEVEL_MODIFIER.COMPLETE ;if no overflow 
	
.ATTACKER.LOWER.LEVEL
	LDA COMBAT.STATS.DEFENDER_LEVEL
	SEC 
	SBC COMBAT.STATS.ATTACKER_LEVEL
	CMP #COMBAT.STATS.LEVEL_TO_HIT_MODIFIER.MAX.PLUS_ONE ;The max level difference between attacker and defender for which the modifier increases. 
	BCC .LTE_MAX2
	;since level difference is >= MAX, reduce difference MAX for purposes of the modifier calculation
	LDA #COMBAT.STATS.LEVEL_TO_HIT_MODIFIER.MAX
	;** FALLS THROUGH**
.LTE_MAX2	
	TAY

	LDA COMBAT.LEVEL_TO_HIT_MODIFIER.TABLE,Y ;save leveling modifier for (defender level-attacker level)		
	STA COMBAT.STATS.LEVEL_TO_HIT_MODIFIER

	LDA CHR_SHEET.PC_MOB.TO_HIT ;attacker to-hit field	
	SED ;set decimal mode
	SEC
	SBC COMBAT.LEVEL_TO_HIT_MODIFIER.TABLE,Y ;subtract leveling modifier for (defender level-attacker level)		
	STA COMBAT.STATS.TO_HIT.MOD_TALLY+$0
	LDA COMBAT.STATS.TO_HIT.MOD_TALLY+$1 
	SBC #$00 ;16-bit subtract
	STA COMBAT.STATS.TO_HIT.MOD_TALLY+$1
	CLD ;clear decimal mode
.APPLY.LEVEL_MODIFIER.COMPLETE
		

		
	;**FALLS THROUGH**

	
; ;=======ORIGINAL FORMULA====
; ;APPLY.DODGE_PARRY.MODIFIER	
; ;PC DODGE/PARRY Modifier = (dodge skill - (PC level * 6)
		; LDA COMBAT.STATS.DEFENDER_LEVEL	
		; STA MULPLR
		; LDA #$00
		; STA MULPLR+$1
		; ;
		; LDA #$06
		; STA MULCND
		; LDA #$00
		; STA MULCND+$01
	; JSR MLP.16.NO_BCD
		;	
		; ;return value: RESULT+$0
		; LDA RESULT+$0 ;(RESULT = PC level *6)			
		; SED ;set decimal mode	
		; CMP COMBAT.STATS.DEFENDER.DODGE_SKILL
		; CLD ;clear decimal mode	
		; BCS .APPLY.COMMON_MODIFIERS.COMPLETE ;if dodge deduction is >= dodge skill then skip dodge modifier as it equals $00. 
	;;
			; ;;TROUBLESHOOTING HOOK
			; ;;
			; ; lda #$aa
			; ; ldx RESULT+$0
			; ; ldy COMBAT.STATS.DEFENDER.DODGE_SKILL
			; ; jsr prep.brk
			; ; brk
		;	
	; JSR CONVERT.HEX.8_TO_BCD.16 
		; ;return value: BCD(2)
		; LDA BCD+$0 ;(BCD=PC level *6)		
	; STA COMBAT.STATS.DEFENDER.DODGE_SKILL.DEDUCTION.BCD	
;
		;
		; LDA COMBAT.STATS.DEFENDER.DODGE_SKILL
	; JSR CONVERT.HEX.8_TO_BCD.16 
		; ;return value: BCD(2)
	; LDA BCD+$0 ;(BCD version of COMBAT.STATS.DEFENDER.DODGE_SKILL)
	; STA COMBAT.STATS.DEFENDER.DODGE.PROB ;**OPT** Memory. Only used for debugging
	; SED ;set decimal mode	
	; SEC
	; SBC COMBAT.STATS.DEFENDER.DODGE_SKILL.DEDUCTION.BCD ;subtract: (PC level *6)
	; STA COMBAT.STATS.DODGE_SKILL.MODIFIER		
	; LDA COMBAT.STATS.TO_HIT.MOD_TALLY+$0 ;attacker base to-hit + dex modifier (pc onmly) + level modifier	
	; SEC 
	; SBC COMBAT.STATS.DODGE_SKILL.MODIFIER
	; STA COMBAT.STATS.TO_HIT.MOD_TALLY+$0
	; LDA COMBAT.STATS.TO_HIT.MOD_TALLY+$1
	; SBC #$00 ;16-bit subtract
	; STA COMBAT.STATS.TO_HIT.MOD_TALLY+$1
	; CLD ;clear decimal mode	
;
		; ; ;TROUBLESHOOTING HOOK
		; ; ; ACC + X-REG = Y-REG 
		; ; ; (yreg is the final modifier total)
		;
		; ; LDA COMBAT.STATS.DEFENDER.DODGE.PROB ;(BCD version of COMBAT.STATS.DEFENDER.DODGE_SKILL)
		; ; ldx COMBAT.STATS.DEFENDER.DODGE_SKILL.DEDUCTION.BCD ;subtract: (PC level *6)
		; ; ldy COMBAT.STATS.DODGE_SKILL.MODIFIER		
		; ; jsr prep.brk
		; ; brk
		;
	; ;**FALLS THROUGH**
	
.APPLY.COMMON_MODIFIERS.COMPLETE
@END

	;**FALLS THROUGH**


		; ;TROUBLESHOOTING HOOK
		; ;ACC + X-REG = Y-REG 
		; ;(yreg is the final modifier total)
		
		; LDA BCD+$0 ;(BCD version of COMBAT.STATS.DEFENDER.DODGE_SKILL)
		; ldx COMBAT.STATS.DEFENDER.DODGE_SKILL.DEDUCTION.BCD ;subtract: (PC level *6)
		; ldy COMBAT.STATS.DODGE_SKILL.MODIFIER		
		; jsr prep.brk
		; brk		


	
;APPLY PC/MOB SPECIFIC MODIFIERS
	
;PC/MOB TURN BRANCH
	LDA COMBAT.TURN_STATUS   ;($00 = PCs turn. $01 = Special(s) S_ENTITY (example Wyvern Ally, Summoned Demon), $02 = MOBs turn)
	BEQ .APPLY.PC_TO_HIT_MODIFIERS ;if PCs turn, then branch
	JMP .CHECK.UNDERFLOW_OVERFLOW
		
.APPLY.PC_TO_HIT_MODIFIERS ;PC attacker	
	
		; lda #$bb
		; LDX COMBAT.STATS.DEFENDER.SIZE_ATTRIB
		; LDY COMBAT.STATS.DEFENDER.DODGE_SKILL	
		; jsr prep.brk
		; brk
	
.APPLY.SIZE.TO_HIT.MODIFIER
@START
;Formula1: M% = !80 ($50) / size
;Formula2: modifier = TO-HIT * M% / 100

	LDA COMBAT.STATS.DAMAGE.TYPE ;($00 = melee | $01 range, $02 spell. High-bit not set = non-magic weapon (doesn't apply to type=spell or MOBs) | high-bit set = magic weapon (doesn't apply to type=spell or MOBs)	
	AND #$7F ;mask-out high-bit
	CMP #$00
	BEQ .APPLY.SIZE.TO_HIT.MODIFIER.COMPLETE ;branch if readied weapon is melee (whether it is magical or not)	
		
	;is mob char sheet size/dodge multi-use field in use a size field?
	LDA COMBAT.STATS.DEFENDER.DODGE_SKILL
	BNE .APPLY.SIZE.TO_HIT.MODIFIER.COMPLETE ;if dodge skill > $00 then it has been set, which means the field is not in use for size

	
	JSR COMBAT.STATS.CALC.SIZE.MODIFIER.PERCENT
	;RETURN VALUE: COMBAT.STATS.SIZE_PERCENT, COMBAT.STATS.SIZE.MODIFIER.RETURN_FLAG ;($00 = small mob | $01 larger mob | $FF = medium mob -no modifier)
		BMI .APPLY.SIZE.TO_HIT.MODIFIER.COMPLETE ;branch if mob in the size range where the size modifier is $00. 


			
.APPLY.PERCENTAGE		
		;set parms for COMBAT.STATS.APPLY.PERCENTAGE.BCD
		LDA COMBAT.STATS.TO_HIT.MOD_TALLY+$0
		STA COMBAT.STATS.APPLY_PERCENT.PARM_BASE+$0
		LDA COMBAT.STATS.TO_HIT.MOD_TALLY+$1
		AND #$7F ;strip off high-bit (see .INIT.TO_HIT.TALLY for docs on the special use of the high-bit for this variable)
		STA COMBAT.STATS.APPLY_PERCENT.PARM_BASE+$1		
		;COMBAT.STATS.SIZE_PERCENT already set
		LDA #$01 ;set mode parm ($00 = hex mode | $01 = BCD mode)
	JSR COMBAT.STATS.APPLY.PERCENTAGE
		;RETURN VALUE: RESULT(2)
		LDA RESULT+$0
		STA COMBAT.STATS.SIZE.MODIFIER+$0
		LDA RESULT+$1
		STA COMBAT.STATS.SIZE.MODIFIER+$1
	
	LDA COMBAT.STATS.SIZE.MODIFIER.RETURN_FLAG ;($00 = small mob | $01 larger mob | $FF = medium mob -no modifier)
	CMP #$01 ;is modifier an increase to base stat? (for large MOBs, PC TO-HIT increases)
	BEQ .APPLY.SIZE_MOD.LARGE_MOB
	
;.APPLY.SIZE_MOD.SMALL_MOB	
	LDA COMBAT.STATS.TO_HIT.MOD_TALLY+$0
	SED ;set decimal mode	
	SEC
	SBC COMBAT.STATS.SIZE.MODIFIER+$0
	STA COMBAT.STATS.TO_HIT.MOD_TALLY+$0		
	LDA COMBAT.STATS.TO_HIT.MOD_TALLY+$1
	SBC COMBAT.STATS.SIZE.MODIFIER+$1 ;16-bit subtract
	STA COMBAT.STATS.TO_HIT.MOD_TALLY+$1
	CLD ;clear decimal mode

	JMP .APPLY.SIZE.TO_HIT.MODIFIER.COMPLETE
	
.APPLY.SIZE_MOD.LARGE_MOB
	LDA COMBAT.STATS.TO_HIT.MOD_TALLY+$0
	SED ;set decimal mode	
	CLC
	ADC COMBAT.STATS.SIZE.MODIFIER+$0
	STA COMBAT.STATS.TO_HIT.MOD_TALLY+$0		
	LDA COMBAT.STATS.TO_HIT.MOD_TALLY+$1
	ADC COMBAT.STATS.SIZE.MODIFIER+$1 ;16-bit subtract
	STA COMBAT.STATS.TO_HIT.MOD_TALLY+$1
	CLD ;clear decimal mode	

	;**FALLS THROUGH**
	
.APPLY.SIZE.TO_HIT.MODIFIER.COMPLETE
@END
			
	;**FALLS THROUGH**
	
.CHECK.UNDERFLOW_OVERFLOW
;***NOTE: the tally is init to $8000 above in (.INIT.TO_HIT.TALLY) so that all modified can be added/subtracted without a 16-bit underflow occuring, and in a way 
;where an 8-bit underflow/overflow in the LO byte during the tally can be detected when the tally is done so that the minimum or maximum TO-HOT can be used. 
;after all modifiers are added/subtracted to/from base TO-HIT, if high-bit of HO byte is not set then 8-bit underflow has occured. 
;If no underflow has occurred, then mask out high-bit of HO byte. If HO byte != $00 then 8-bit overflow has occured 
		
	LDA COMBAT.STATS.TO_HIT.MOD_TALLY+$01 ;load HO byte of modifier tally (includes base TO-HIT)
	BPL .SET.MIN.TO_HIT ;branch if high-bit is not set (16-bit underflow occured during the tally)
	;**FALLS THROUGH**	
.CHECK.OVERFLOW
	AND #$7F ;mask-out high-bit
	CMP #$00 ;if the HO byte of the tally is $00 after the high-bit is masked out, then an 8-bit overflow never occured in the LO byte during the tally
	BNE .SET.MAX.TO_HIT ;8-bit overflow occured in the LO byte during the tally
	;**FALLS THROUGH**	
.CHECK.UNDERFLOW_OVERFLOW.COMPLETE

	;**FALLS THROUGH**
	
.CHECK.TO_HIT.MIN_MAX		
;the modifiers might collectively result in a TO-HIT value lower than the minimum or greater than the maximum. 
;If that happens, this routine overrides and sets the minimum or maximum as applies.

	LDA COMBAT.STATS.TO_HIT.MOD_TALLY+$00 ;load LO byte of modifier tally (includes base TO-HIT)
	CMP #COMBAT.STATS.TO_HIT.MIN
	BCC .SET.MIN.TO_HIT
	CMP #COMBAT.STATS.TO_HIT.MAX
	BCS .SET.MAX.TO_HIT
	JMP .TO_HIT.MODIFIERS.TALLY.COMPLETE
	
.SET.MIN.TO_HIT

	LDA #COMBAT.STATS.TO_HIT.MIN	
	STA	COMBAT.STATS.TO_HIT.MOD_TALLY+$0
	JMP .TO_HIT.MODIFIERS.TALLY.COMPLETE

.SET.MAX.TO_HIT	
	LDA #COMBAT.STATS.TO_HIT.MAX	
	STA	COMBAT.STATS.TO_HIT.MOD_TALLY+$0

	;**FALLS THROUGH**
	
.TO_HIT.MODIFIERS.TALLY.COMPLETE
	;set final TO-HIT value
	LDA COMBAT.STATS.TO_HIT.MOD_TALLY+$00 ;load LO byte of modifier tally (includes base TO-HIT)
	STA COMBAT.STATS.MODIFIED_TO_HIT

	
	
	
@END
	;**FALLS THROUGH
	
.ROLLS
@START

.ROLL.TO_HIT
@START
;TO-HIT calculations are done in BCD. This is so that a random number can be drawn (!1-!100) can be drawn
;in alignment with the % scale. Otherwise we'd be getting a random # $0-$FF which would have to be converted to be aligned
;with the decimal % scale. That would take a lot of memory, whereas forcing the random numbers into the !1-!100 range takes more speed.
;
;Also worth noting, the random number range starts out as
;$1-$200 ($1-$C7), and if the number is > !100 it is divided by 2 to convert it back to the desired range. This reduces the speed required
;as less random numbers have to be generated until finding one that is acceptable. 

;get random number !1-!199, then convert it to !1-!99
	JSR COMBAT.RANDOM.8.BCD
		;RETURN VALUE = BCD random # !1-!99
			
					;LDA #$01 ;TO-HIT roll override  (force hit)
					;LDA #$99 ;TO-HIT roll override (force miss)
					
	STA COMBAT.STATS.TO_HIT.ROLL
	SED ;set decimal mode
	CMP COMBAT.STATS.MODIFIED_TO_HIT
	CLD ;clear decimal mode	
	BCC .HIT
	BEQ .HIT
	
	;**FALLS THROUGH**
	
.MISS
	LDA #$01 ;set return value = miss
	JMP .EXIT
	
.HIT
@END

	;**FALLS THROUGH**

.ROLL.DODGE.SAVING_THROW
@START 
;Formula: DODGE-PARRY saving throw = (DODGE-PARRY skill/2) CMP random #
	
;dodge skill = 0
;(this can happen with mobs because the size field and dodge field is shared. If size is in use then dodge = 0. 
;it should result in dodge fails because the random number is !1-!99 and thus dodge skill = 0 will
;always be lower)


;VALIDATE ENTRANCE

	;is mob char sheet size/dodge multi-use field in use as size field?
	LDA COMBAT.STATS.DEFENDER.SIZE_ATTRIB
	BPL .ROLL.DODGE.SAVING_THROW.DONE ;branch if field not in use for dodge skill (all dodge values have high-bit is set)


;CHECK WEIGHT LIMIT
;(if the defender is a PC then his/her readied equipment weight might prevent the dodge-parry saving throw)

	;pc/mob turn branch
	LDA COMBAT.TURN_STATUS   ;($00 = PCs turn. $01 = Special(s) S_ENTITY (example Wyvern Ally, Summoned Demon), $02 = MOBs turn)
	BEQ .CHECK.WEIGHT_LIMIT.DONE	;if PCs turn, then branch. (if it's PC's turn, then mob's are defending, and mob's 
									;aren't subject to the weight limit because if we don't want them to have a dodge 
									;saving throw we just don't set that value on their character sheet)
	
	;does PC exceed the weight limit 
	LDA CHR_SHEET.PC.READIED_EQUIPMENT_WEIGHT
				
				; LDX #$AA
				; LDY #COMBAT.STATS.DODGE.WEIGHT.LIMIT
				; JSR PREP.BRK
				; BRK

	
	CMP #COMBAT.STATS.DODGE.WEIGHT.LIMIT
	BCS	.ROLL.DODGE.SAVING_THROW.DONE
.CHECK.WEIGHT_LIMIT.DONE

	;**FALLS THROUGH**
				

				
;ROLL SAVING THROW
	
		;get random #
	JSR COMBAT.RANDOM.8.BCD
		;RETURN VALUE = BCD random # !1-!99
		STA COMBAT.STATS.DODGE_SKILL.ROLL ;**OPT** Memory. Only needed for debugging

	;convert dodge skill to BCD
		LDA COMBAT.STATS.DEFENDER.DODGE_SKILL ;load defender dodge-parry skill

	;don't /2 on dodge skill if MOB/SPECIALS are defenders
	;(the reason is because dodge skill for MOB/SPECIALs is a shared field with size, and the high-bit is masked out to get the
	;dodge value, which results in a max value of !128. If this value is /2 the result is a dodge saving throw chance that is too low for gameplay purposes
	;i.e. it would max out at a 64% chance)

		;pc/mob turn branch
		LDX COMBAT.TURN_STATUS   ;($00 = PCs turn. $01 = Special(s) S_ENTITY (example Wyvern Ally, Summoned Demon), $02 = MOBs turn)
		BEQ .CONVERT.DODGE_SKILL	;if PCs turn (MOB/SPECIALS are defenders), then branch.
		;pc defenders only
		LSR ;/2
.CONVERT.DODGE_SKILL ;entrance for MOB/SPECIAL defenders
	JSR CONVERT.HEX.8_TO_BCD.16	
		;**OPT** Memory. These are only needed for debugging
		;checkfor 8-bit BCD overflow
		LDA BCD+$1
		BEQ .OVERFLOW.CHECK.DONE
		LDA #COMBAT.STATS.TO_HIT.MAX
		;overflow detected, set max value
		STA BCD+$0 ;BCD conversion of COMBAT.STATS.DEFENDER.DODGE_SKILL
		
.OVERFLOW.CHECK.DONE
		LDA BCD+$0 ;BCD conversion of COMBAT.STATS.DEFENDER.DODGE_SKILL
		STA COMBAT.STATS.DEFENDER.DODGE.PROB
		
		SED ;set decimal mode
		CMP COMBAT.STATS.DODGE_SKILL.ROLL ;BCD
		CLD ;clear decimal mode
		BCC .DODGE_FAILS	
;.DODGE_SUCCEEDS			
	LDA #$00 ;set failed flag value
	STA COMBAT.STATS.DODGE_FLAG	;$1byte ($00 = dodge succeeded, $01 dodge not attempted, $03 dodge failed)
		
	JMP .MISS ;since the dodge succedded, the TO-HIT flag needs to be set to miss. 

.DODGE_FAILS
	LDA #$03 ;set failed flag value
	STA COMBAT.STATS.DODGE_FLAG	;$1byte ($00 = dodge succeeded, $01 dodge not attempted, $03 dodge failed)
	;JMP .ROLL.DODGE.SAVING_THROW.DONE

	
.ROLL.DODGE.SAVING_THROW.DONE

	
@END

.ROLL.DEATH_MAGIC.SAVING_THROW
;located in the COMBAT.DAMAGE.ROLL routine because spells don't run this subrotuine (DAMAGE.TO-HIT.ROLL).

	;**FALLS THROUGH**

	
.CRITICAL_HIT.ATTEMPT_BLOCK.ROLLS
@START
; formula1: critical hit skill = % chance that critical hit opportunity occurs for an attacker
; formula1: critical hit resist = % chance that a defender blocks a critical attempt 
; Note: thus, two level 10 assassins (with !90 for skill and resist) would end up rarely scoring a critical hit on eachother. Each would have a 90% chance of identifying an opportunity, but each would have a 90% chance of blocking the attempt
;
;TO-HIT calculations are done in BCD. See .ROLL.TO_HIT for documentation on the reason why and implementation notes.

.CALCUALTE.CH_ATTEMPT.PROBABILITY
;(the probability that a critical hit attempt is made by the attacker.
;Critical hit attempt probability has 3 tiers: defender engaged with PC (lowest), defender not engaged (middle), defender distracted: engaged with a defender that is not the PC (highest)
;See STA spreadsheet for a probability chart)

.SET.DEX
	;pc/mob turn branch
	LDX COMBAT.TURN_STATUS   ;($00 = PCs turn. $01 = Special(s) S_ENTITY (example Wyvern Ally, Summoned Demon), $02 = MOBs turn)
	BNE .MOB_SPECIALS		 ;if not PCs turn, then branch 
	LDA CHR_SHEET.PC.ATTRIB.DEX
	STA COMBAT.STATS.CRTL_HIT.DEX
	JMP .SET.DEX.DONE
	
.MOB_SPECIALS
;if it is MOB or SPECIALS turn, use their critical hit skill for the DEX value because MOBs and SPECIALS don't have a DEX attribute
;but the formula will drasticly understate the proability to hit if no DEX value is used.
 
	LDA CHR_SHEET.PC_MOB.SKILL.CRITICAL_HIT
	STA COMBAT.STATS.CRTL_HIT.DEX		
.SET.DEX.DONE

	;**FALLS THROUGH**

	;calculate 1st part of formula
	;(critical hit + DEX)
	LDA CHR_SHEET.PC_MOB.SKILL.CRITICAL_HIT
	CLC
	ADC COMBAT.STATS.CRTL_HIT.DEX
	STA COMBAT.STATS.CRTL_HIT.PROB+$0
	LDA #$00
	ADC #$00 ;16-bit add
	STA COMBAT.STATS.CRTL_HIT.PROB+$1
	
	;**FALLS THROUGH**
	
	; BCS .PROB.OVERFLOW ;check for 8-bit hex overflow
	; JMP .SCENARO.BRANCH
	
; .PROB.OVERFLOW
	; LDA #$FF ;set to max 8-bit hex value instead of max BCD value because formula depends on it's interior calculations being allowed to exceed !99. See critical hit skill on STA Chart (TO-HIT tab) for more details.

.SCENARO.BRANCH	
;are attacker and defender already engaged in combat?
;("engaged" means the defender initiated an attack against the current attacker on its last turn)
			
	LDX COMBAT.STATS.DEFENDER.ENGAGED.MO_INDEX
	CPX #COMBAT.STATS.CRTL_HIT.NOT_ENGAGED_VALUE
	BEQ .DEFENDER.NOT_ENGAGED
	CPX COMBAT.ATTACKER.SINDEX
	BNE .DEFENDER.DISTRACTED ;.DEFENDER.DISTRACTED (engaged with another attacker). ;formula: critical hit probability = (target skill + DEX)
	;default: engaged combatants (attacker and defender engaged with each other)
	
	;**FALLS THROUGH**
	
.ENGAGED.COMBATANTS
;formula: critical hit probability = (target skill+DEX)/8

	; LSR ;/2
	; LSR ;/4
	; LSR ;/8
	LDA #$08 ;set critical hit formula divisor	 	
	JMP .CALC.PROB

.DEFENDER.NOT_ENGAGED
;formula: critical hit probability = (target skill+DEX)/4

	; LSR ;/2
	; LSR ;/4	
	LDA #$04 ;set critical hit formula divisor	
	JMP .CALC.PROB
	
.DEFENDER.DISTRACTED
;formula: critical hit probability = (target skill+DEX)/2

	;LSR ;/2
	LDA #$02 ;set critical hit formula divisor
	
	;**FALLS THROUGH**

.CALC.PROB
		;ACC = critical hit formula divisor
		STA DIVISOR+$0			;number to divide by
		LDA #$00
		STA DIVISOR+$1
		
		
		LDA COMBAT.STATS.CRTL_HIT.PROB+$0
		STA DIVIDEND+$0			;number to be divided
		LDA COMBAT.STATS.CRTL_HIT.PROB+$1
		STA DIVIDEND+$1
	JSR DIV.16				;(dividend/divisor)					
		;RETURN VALUE: result+$0 (quotient LO), result+$1 (quotient HO), result+$2 (remainder LO), result+$3 (remainder HO)		

	LDA RESULT+$1 		 	;QUOTIENT
	BNE .PROB.OVERFLOW ;check for 8-bit hex overflow
	LDA RESULT+$0 		 	;QUOTIENT
	CMP #$64 ;!100	
	BCS .PROB.OVERFLOW ;check for BCD overflow
	JMP .SAVE.PROB

	
.PROB.OVERFLOW
	LDA #COMBAT.STATS.CRTL_HIT.PROB.MAX ;$5F = !99 (BCD)

	;**FALLS THROUGH**
.SAVE.PROB		
	;ACC = (critical hit skill + DEX) / ?
		
	;**FALLS THROUGH**
			
;.ADD.LEVEL_MOD
		;ACC: probability calculation (HEX)
	JSR CONVERT.HEX.8_TO_BCD.16
	LDA BCD+$0 ;load converted value
	;***LEVEL MOD DISABLED
	;(I disabed it for several reasons. 1. it wasn't setup correctly; the level mod isn't always an add to probability,
	;it is a subtract if the defender level is higher. 2. It may dimish the coolness of the assasain track
	;if warriors, mages, and everybodies brother is critical hitting lower level mobs. 3. critical hit is already level adjusted in the sense that
	;a hit has to occur for there to be a chance of a critical hit, and the base TO-HIT probability is level adjusted)
	;
	; SED ;set decimal mode
	; CLC
	; ADC COMBAT.STATS.LEVEL_TO_HIT_MODIFIER
	; CLD ;clear decimal mode	
	; BCS .PROB.OVERFLOW
	STA COMBAT.STATS.CRTL_HIT.PROB			
.CALCUALTE.CH_ATTEMPT.PROB.DONE

		; LDA #$AA
		; LDX COMBAT.STATS.CRTL_HIT.PROB			
		; JSR PREP.BRK
		; BRK
		
	
	;**FALLS THROUGH**
	

.IS.CRITICAL_HIT.ATTEMPTED
;get random number !1-!200, then convert it to !1-!99
	JSR COMBAT.RANDOM.8.BCD
		;RETURN VALUE = BCD random # !1-!99
		;LDA #$01					
		STA COMBAT.STATS.CRTL_HIT.TO_HIT.ROLL
			
	LDA COMBAT.STATS.CRTL_HIT.PROB
	SED ;set decimal mode
	CMP COMBAT.STATS.CRTL_HIT.TO_HIT.ROLL
	CLD ;clear decimal mode	
	BCC .CRITICAL_HIT.NOT_ATTEMPTED	
	
.CRITICAL_HIT.ATTEMPTED	
;IS CRITCAL HIT BLOCKED BY DEFENDER?
	JSR COMBAT.RANDOM.8.BCD
		;RETURN VALUE = BCD random # !1-!99
		;LDA #$01					
		STA COMBAT.STATS.CRTL_HIT_BLOCK.ROLL

	LDA COMBAT.STATS.DEFENDER.RESIST_CRITCAL_HIT
	LSR ;/2
	CMP #$64 ;is resist skill >= !100? If so there is no need to roll the savings throw
	BCS .CRITICAL_HIT.BLOCKED
	JSR CONVERT.HEX.8_TO_BCD.16
		
	LDA BCD+$0 ;load converted value
	SED ;set decimal mode
	CMP COMBAT.STATS.CRTL_HIT_BLOCK.ROLL
	CLD ;clear decimal mode	
	BCC .CRITICAL_HIT.SUCCEEDED
.CRITICAL_HIT.BLOCKED			
	LDA #$03 ;critial hit blocked
	STA COMBAT.STATS.CRTL_HIT_FLAG 	;($00 = critical hit succeeded, $01 = critical hit not attempted, $03 = critical hit blocked by defender)
	JMP .CRITICAL_HIT.ROLL.DONE	
	
.CRITICAL_HIT.SUCCEEDED
	LDA #$00 ;critial hit succeeded
	STA COMBAT.STATS.CRTL_HIT_FLAG 	;($00 = critical hit succeeded, $01 = critical hit not attempted, $03 = critical hit blocked by defender)
	JMP .CRITICAL_HIT.ROLL.DONE
	
.CRITICAL_HIT.NOT_ATTEMPTED
	LDA #$01 ;critical hit not attempted
	STA COMBAT.STATS.CRTL_HIT_FLAG 	;($00 = critical hit succeeded, $01 = critical hit not attempted, $03 = critical hit blocked by defender)
	
	;**FALLS THROUGH**
	
.CRITICAL_HIT.ROLL.DONE


			
@END
	
	;**FALLS THROUGH**
.RANGE_WEAPON.INTERFERENCE.ROLL
@START
;if the attacker has a range weapon, there is a chance the defender will interfere with it's operation.
;if the defender is successful, then the result is automatically a miss for the attacker, regardless of the outcome of 
;any of the rolls up to this point


;IS ATTACKER USING A RANGE WEAPON?
	LDA COMBAT.STATS.DAMAGE.TYPE ;($00 = melee | $01 range, $02 spell. High-bit not set = non-magic weapon (doesn't apply to type=spell or MOBs) | high-bit set = magic weapon (doesn't apply to type=spell or MOBs))
	CMP #$01
	BNE .RANGE_WEAPON.INTERFERENCE.ROLL.DONE ;branch if damage type is not range	
			
	JSR COMBAT.IS_ATTACKER.ENAGED			
		;ACC = ;($00 = attacker is NOT engaged in melee combat, >=$01 = attacker IS engaged in melee combat, the value is the SINDEX of the engaged S_ENTITY)
		CMP #$00 ;is attacker engaged in melee combat
		BEQ .RANGE_WEAPON.INTERFERENCE.ROLL.DONE ;branch if no
		
	
;ROLL RANGE INTERFERENCE
	JSR RANDOM.8
		;RETURN VALUE: 8-BIT random number
	CMP #COMBAT.STATS.RANGE_INTERFERENCE.PROB ;50% chance
	BCS .NO.INTERFERENCE
.INTERFERENCE
	LDA #$00 ;outcome code = success
	STA COMBAT.STATS.RANGE_INTERFERENCE.FLAG ;($00 = inteference successful | $01 = interference not attempted | $03 = inteference failed)
	JMP .MISS
.NO.INTERFERENCE
	LDA #$03 ;outcome code = failed
	STA COMBAT.STATS.RANGE_INTERFERENCE.FLAG ;($00 = inteference successful | $01 = interference not attempted | $03 = inteference failed)
.RANGE_WEAPON.INTERFERENCE.ROLL.DONE

@END
	
	LDA #$00 ;set return value = hit

	;**FALLS THROUGH**

.EXIT

			
.SAVE.RETURN_VALUE
	;ACC = return value for this routine
	STA COMBAT.STATS.HIT_MISS_KILL.FLAG ;($00 = hit | $01 = miss | $02 = kill)

	
	; ;;TROUBLESHOOTING.HOOK (TO-HIT)
	; ;
			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
	; ;		
	; LDA CHR_SHEET.PC_MOB.TO_HIT ;attacker to-hit field
	; STA $9600
	; ;
	; LDA COMBAT.STATS.LEVEL_TO_HIT_MODIFIER
	; STA $9601
	; ;
	; LDA COMBAT.STATS.DEFENDER.DODGE.PROB ;controls branch into apply size modifier
	; STA $9602	
	; LDA COMBAT.STATS.DODGE_SKILL.ROLL
	; STA $9603
	; LDA COMBAT.STATS.DODGE_FLAG	;$1byte ($00 = dodge succeeded, $01 dodge not attempted, $03 dodge failed)
	; STA $9604	
	; ;
	; LDA COMBAT.STATS.DEFENDER.SIZE_ATTRIB
	; STA $9605
	; LDA COMBAT.STATS.SIZE_PERCENT
	; STA $9606
	; LDA COMBAT.STATS.SIZE.MODIFIER
	; STA	$9607
	; ;
	; LDA COMBAT.STATS.TO_HIT.MOD_TALLY+$0	
	; STA	$9608
	; LDA COMBAT.STATS.TO_HIT.MOD_TALLY+$1
	; STA	$9609
	; ;
	; LDA CHR_SHEET.PC_MOB.SKILL.CRITICAL_HIT
	; STA	$960A
	; LDA COMBAT.STATS.CRTL_HIT.PROB
	; STA	$960B	
	; LDA COMBAT.STATS.CRTL_HIT.TO_HIT.ROLL
	; STA	$960C
	; LDA COMBAT.STATS.DEFENDER.RESIST_CRITCAL_HIT
	; STA	$960D
	; LDA COMBAT.STATS.CRTL_HIT_BLOCK.ROLL
	; STA	$960E
	; LDA COMBAT.STATS.CRTL_HIT_FLAG 	;($00 = critical hit succeeded, $01 = critical hit not attempted, $03 = critical hit blocked by defender)
	; STA	$960F
	; ;
	; LDA COMBAT.STATS.HIT_MISS_KILL.FLAG ;($00 = hit | $01 = miss | $02 = kill)
	; STA $9610
	
	; ;LDA #$EE
	; LDA COMBAT.STATS.CRTL_HIT.PROB
	; ;LDA COMBAT.STATS.CRTL_HIT_FLAG 	;($00 = critical hit succeeded, $01 = critical hit not attempted, $03 = critical hit blocked by defender)
	; LDX COMBAT.STATS.CRTL_HIT.TO_HIT.ROLL	
	; LDY COMBAT.STATS.CRTL_HIT_BLOCK.ROLL

	; ; LDA COMBAT.STATS.DEFENDER.DODGE.PROB ;controls branch into apply size modifier
	; ; LDX COMBAT.STATS.DODGE_SKILL.ROLL
	; ; LDY COMBAT.STATS.DODGE_FLAG	;$1byte ($00 = dodge succeeded, $01 dodge not attempted, $03 dodge failed)

	; JSR PREP.BRK
	; BRK
	; ;
; .TEMP
			; LDA TEMP
	; ;
	


	;**FALLS THROUGH**
	
.SKILL_PROGRESS.UPDATE
@START		




			; LDA #$01
			; STA COMBAT.STATS.CRTL_HIT_FLAG
			
;PC/MOB TURN BRANCH
	LDA COMBAT.TURN_STATUS   ;($00 = PCs turn. $01 = Special(s) S_ENTITY (example Wyvern Ally, Summoned Demon), $02 = MOBs turn)
	BNE .SKILL_PROGRESS.UPDATE.MOB_SPECIAL ;if not PCs turn, then branch

;PC ATTACKER
	LDY COMBAT.ATTACKER.SINDEX ;set parm for SKILL.PROGRESS.UPDATE

	;CRITICAL HIT SKILL
	LDA COMBAT.STATS.CRTL_HIT_FLAG	;($00 = critical hit succeeded, $01 = critical hit not attempted, $03 = critical hit blocked by defender)
	CMP #$01
	BEQ .INC.CRITICAL_HIT.DONE
		LDA #CHR_SHEET.PC.SKILL.CRITICAL_HIT.PROG.INDEX
		;YREG = COMBAT.ATTACKER.SINDEX
	JSR SKILL.PROGRESS.UPDATE	
.INC.CRITICAL_HIT.DONE	
	;**FALLS THROUGH**
	
	;MELEE/RANGED SKILL
	;(only if hit occured)
	LDA COMBAT.STATS.HIT_MISS_KILL.FLAG	;$1byte ;($00 = hit | $01 = miss | $02 = kill)
	BNE .INC.RANGE_MELEE.DONE
	
	;which skill to update?
    LDA COMBAT.STATS.DAMAGE.TYPE ;($00 = melee | $01 range, $02 spell. High-bit not set = non-magic weapon (doesn't apply to type=spell or MOBs) | high-bit set = magic weapon (doesn't apply to type=spell or MOBs)
	AND #$7F ;mask-out high-bit
	BNE .INC.RANGE_SKILL
;.INC.MELEE_SKILL
		LDA #CHR_SHEET.PC.SKILL.MELEE.PROG.INDEX
		;YREG = COMBAT.ATTACKER.SINDEX
	JSR SKILL.PROGRESS.UPDATE
	JMP .INC.RANGE_MELEE.DONE
	
.INC.RANGE_SKILL
		LDA #CHR_SHEET.PC.SKILL.RANGE.PROG.INDEX
		;YREG = COMBAT.ATTACKER.SINDEX
	JSR SKILL.PROGRESS.UPDATE 
.INC.RANGE_MELEE.DONE
	
	JMP .SKILL_PROGRESS.UPDATE.DONE		
	
.SKILL_PROGRESS.UPDATE.MOB_SPECIAL
	;is defender a PC?
	;(if not, then a mob is attacking a special or vice-versa, so the PCs skill progress shouldn't be incremented)
	LDY COMBAT.DEFENDER.SINDEX
	LDA SCREEN.MO_SPRITE_TYPE.DATA,Y ;load S_ENTITY type of defender		
	AND #$87 ;mask-out MTT flags (bits 3-6)
	CMP #S_ENTITY_TYPE.PC
	BNE .SKILL_PROGRESS.UPDATE.MOB.DONE ;branch if defender is not a PC

	;***<INSERT CHECK FOR READIED EQUIPMENT WEIGHT, DON'T UPDATE SKILL PROGRESS IF TOO HEAVY>
	;(and TO-HIT routine, don't apply dodge/parry mod if readied equipment weight is too heavy)

		; ldx #$aa
		; jsr prep.brk
		; brk
		
	;was dodge attempted?
	;(if not then don't increment progress
	;the only time a dodge isn't attempted is if the TO-HIT roll results in miss,
	;but we can't check the TO-HIT flag because if the dodge succeeds the TO-HIT flag is
	;changed to hit)
	
	LDA COMBAT.STATS.DODGE_FLAG ;$1byte ($00 = dodge succeeded, $01 dodge not attempted, $03 dodge failed)
	CMP #$01
	BEQ .SKILL_PROGRESS.UPDATE.MOB.DONE
	
		;YREG = COMBAT.DEFENDER.SINDEX ;set parm for SKILL.PROGRESS.UPDATE
		LDA #CHR_SHEET.PC.SKILL.DODGE.PROG.INDEX
	JSR SKILL.PROGRESS.UPDATE 	
.SKILL_PROGRESS.UPDATE.MOB.DONE

.SKILL_PROGRESS.UPDATE.DONE
	
				; ;JSR KEYIN
				; ;read PC character sheet data
					; LDA #$02
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

			
			; lda #$ab
			; ldx COMBAT.STATS.CRTL_HIT_FLAG
			; JSR FULL.BRK
			; brk
			
@END

	;**FALLS THROUGH**

;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX

	;load return value
	LDA COMBAT.STATS.HIT_MISS_KILL.FLAG ;($00 = hit | $01 = miss | $02 = kill)


	RTS


COMBAT.IS_ATTACKER.ENAGED
@START
;PARAMETERS: COMBAT.ATTACKER.SINDEX
;ENTRANCE: combat
;RETURN: ACC = ;($00 = attacker is engaged in melee combat, $01 = attacker is not engaged in melee combat)

.INIT
	;see CALCULATE.DISTANCE parms that are the same each iteration
	LDY COMBAT.ATTACKER.SINDEX
	LDA SCREEN.INDEX.COLUMN,Y
	STA PARM1.GMAP.X
	LDA SCREEN.INDEX.ROW,Y
	STA PARM1.GMAP.Y
		
	LDX #$00 ;init loop screen index
.SEARCH.SCREEN_ARRAY.LOOP	
	LDA SCREEN.MO_SPRITE.DATA,X ;check screen location for an S_ENTITY
	CMP #$FF ;is S_ENTITY found?
	BEQ .INCREMENT.INDEX ;branch if no
	
	;determine if found S_ENTITY is in melee range of attacker	

		;XREG = screen index of current screen location being examined
		LDA SCREEN.INDEX.COLUMN,X
		STA PARM2.GMAP.X
		LDA SCREEN.INDEX.ROW,X
		STA PARM2.GMAP.Y		
	JSR CALCULATE.DISTANCE
		;RETURN VALUE: tile distance between X/Y coordinates
		STA COMBAT.CHASE.TARGET.DISTANCE ; parm of COMBAT.MELEE.IN_RANGE.CHECK
			
		LDA MOB.POSITION.X_LT
		STA COMBAT.CHASE.TARGET.X_LT
		LDA MOB.POSITION.X_GR
		STA COMBAT.CHASE.TARGET.X_GR
		LDA MOB.POSITION.Y_LT
		STA COMBAT.CHASE.TARGET.Y_LT
		LDA MOB.POSITION.Y_GR
		STA COMBAT.CHASE.TARGET.Y_GR	
	JSR COMBAT.MELEE.IN_RANGE.CHECK
		;RETURN VALUE: ACC = ($00 = in melee range, $01 = not in melee range)		
	CMP #$01
	BEQ .INCREMENT.INDEX ;branch if defender is not in melee range	

	;**FALLS THROUGH**
	
.EXAMINE.FOUND.S_ENITY
;(Determine if the found S_ENTITY is engaged in combat with the attacker)

	;read melee range found S_ENTITY character sheet

		;XREG = SINDEX of melee range S_ENTITY found
		TXA
		TAY ;set SINDEX parm
		LDA #$00 ;set read mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet
		;RETURN: CHR_SHEET.RECORD.READ($80)
	
	;is melee range S_ENTITY enaged in melee combat with attacker
	LDY COMBAT.ATTACKER.SINDEX
	CPY CHR_SHEET.PC_MOB.ENGAGED.SINDEX
	BNE .INCREMENT.INDEX ;branch, if no
	
	;set return value
	;X-REG = sindex of S_ENTITY found at current search position
	STX COMBAT.STATS.ATTACKER.ENAGED.FLAG ;($00 = attacker is NOT engaged in melee combat, >=$01 = attacker IS engaged in melee combat, the value is the SINDEX of the engaged S_ENTITY)


	JMP .RESTORE.ATTACKER.CHR_SHEET	

.INCREMENT.INDEX						
	INX
	CPX #SCREEN.ARRAY.LAST_ELEMENT2 ;at end of screen array?
	BNE .SEARCH.SCREEN_ARRAY.LOOP
	;BNE .SEARCH.SCREEN_ARRAY.LOOP_STEP

	;No S_ENTITY found engaged in melee combat with attacker 
	LDA #$00 ;set return value
	STA COMBAT.STATS.ATTACKER.ENAGED.FLAG ;($00 = attacker is NOT engaged in melee combat, >=$01 = attacker IS engaged in melee combat, the value is the SINDEX of the engaged S_ENTITY)
	
	
.RESTORE.ATTACKER.CHR_SHEET
;(this is assumed to be the character sheet loaded when this routine was called)

	;read attacker character sheet
		LDY COMBAT.ATTACKER.SINDEX
		LDA #$00 ;set read mode ;($00 = read | $01 = write)
	JSR COMBAT.READ_WRITE.CHR_SHEET ;read character sheet
		;RETURN: CHR_SHEET.RECORD.READ($80)

	;load return value
	LDA COMBAT.STATS.ATTACKER.ENAGED.FLAG ;($00 = attacker is NOT engaged in melee combat, >=$01 = attacker IS engaged in melee combat, the value is the SINDEX of the engaged S_ENTITY)

	RTS

@END



@END
@END

;LOCAL VARIABLES
@START


;BCD version
COMBAT.LEVEL_TO_HIT_MODIFIER.TABLE	.HS 00.02.08.18.32.50

;hex version
;COMBAT.LEVEL_TO_HIT_MODIFIER.TABLE	.HS 00.02.08.12.20.32

@END

