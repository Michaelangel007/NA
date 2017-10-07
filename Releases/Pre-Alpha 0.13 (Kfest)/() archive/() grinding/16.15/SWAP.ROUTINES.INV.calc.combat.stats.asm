;************************INCLUDE FILE*****************************
;(do not assemble stand-alone.  )


;=====================INCLUDE FILE DOCUMENTATION====================================
;
;Include file to SWAP.ROUTINES.INVENTORY.ASM
;
;=================================================================================
	
	

CALCULATE.COMBAT.STATS
@START
;=====================SUBROUTINE DOCUMENTATION====================================
;
;The CALCULATE.COMBAT.STATS subroutine parses the players readied equipment data
;and calculates the derived stats on the in-memory player character sheets. 
;
;Readied equipment is stored in slots: left hand, right hand, head, torso, etc.
;(see combat stats.xls for an authoratative list)
;
;==ENTRANCES==
;There are 3 entrances to this subroutine:
;
;CALCULATE.COMBAT.STATS.PROCESS_ALL_ENTRANCE 
;		*all equipment slots are processed
;		*used by the character creation module (future)
;
;CALCULATE.COMBAT.STATS.LEVELUP_ENTRANCE
;		*only DAMAGE related equipment slots are processed (i.e. right/left hand), which incorporate a strength modifier. The strength value might change on levelup. 
;		*the dex modifier is always calculated since it doesn't do any file I/O but it's worth nothing here that the DEX modifier also needs to be calculated for the same reason as strength. 
;		*used by the levelup routine (future). If base attributes are increased by trainers then the levelup entrance call would be triggered by the trainers.
;
;CALCULATE.COMBAT.STATS.GENERAL_ENTRANCE
;		*only processes the DAMAGE or DEFENSE related equipment slots whose status has changed during the current inventory module session. 
;			For example, if the player changes the readied weapon in the PC #1's left hand, then all DAMAGE related equipment slots would be processed (i.e. left/right hand) 
;		*which equipment slots are processed is controlled by INV.RE.UPDATE_DAMAGE.FLAG, INV.RE.UPDATE_DEFENSE.FLAG
;		*Corner case: shields. INV.RE.UPDATE_DEFENSE.FLAG is set in INV.READY_UNREADY.EQUIPMENT.ENTRANCE based on the item_type of the item which is being unreadied or readied. 
;			As a result, if a shield (item_type $01) is readied or unreadied then the DEFENSE flag will get set causing the left/right hand slots to get processed in .CALCULATE.ARMOR.STATS but not in .CALCULATE.WEAPON.STATS
;		*Used by the ready equipment routine in the inventory module
;	
;
;-Disk Speed Improvement Ideas **OPT** Disk Speed
;
;Currently the equipment slots are grouped into DAMAGE and DEFENSE with a flag setting for
;each. It's all or nothing. If one player readies a weapon then the DAMAGE flag is set and the left/right
;hand equipment slots for all players is processed. Another approach would be to DAMAGE and DEFENSE be an array of flags
;with 6 byte records so that each player has a DEFENSE and DAMAGE flag. This should drastically cut down on the disk loads
;this routine. 
; 
;=================================================================================

CALCULATE.COMBAT.STATS.PROCESS_ALL_ENTRANCE
@START
;=====================CODE-SECTION DOCUMENTATION====================================
;
;All equipment slots are processed. This entrance is used by the character creation module.  
;
;=================================================================================

	LDA #$01
	STA INV.RE.UPDATE_DAMAGE.FLAG 	;($00 = no changes made that effect DAMAGE on the character sheet. $01 changes were made that affect the same)
	STA INV.RE.UPDATE_DEFENSE.FLAG 	;($00 = no changes made that effect DAMAGE on the character sheet. $01 changes were made that affect the same)
	LDA #$02
	STA INV.CALCULATE.STATS.MODE ;($00 = general mode. only modified equip slots are processed | $01 = levelup mode. Only lhand/rhand and TO-HIT are processed because they use STR/DEX. | $02 = process all mode. Used for character creation)

	
	; ;read files
	; JSR FILE.OPEN.INVENTORY
	; JSR INV.READ_FILE.CHAR_SHEET.READIED
	
	
		;JMP CALCULATE.COMBAT.STATS.LEVELUP_ENTRANCE

		

		; LDA #$01 ;set flags to "process"
	; JSR INV.INIT.PROCESS_FLAGS

	

	JMP CALCULATE.COMBAT.STATS.MAIN
	
@END

CALCULATE.COMBAT.STATS.LEVELUP_ENTRANCE
@START
;=====================CODE-SECTION DOCUMENTATION====================================
;
;Note: Levelup needs DEX to process and left/right hand to process since STR is part of the damage calculation and it is
;addressed in the left/right hand slot code. DEX is always processed, regardless of flag settings, because there is no file I/O.
;
;=================================================================================


	LDA #$01
	STA INV.CALCULATE.STATS.MODE	 ;($00 = general mode. only modified equip slots are processed | $01 = levelup mode. Only lhand/rhand and TO-HIT are processed because they use STR/DEX. | $02 = process all mode. Used for character creation)
	STA INV.RE.UPDATE_DAMAGE.FLAG 	;($00 = no changes made that effect DAMAGE on the character sheet. $01 changes were made that affect the same)
	
	LDA #$00 ;**OPT** Memory. This LDA/STA might not be needed if INV.INVENTORY_MODULE.ENTRANCE is run (which inits the flags to $00) before this routine is called
	STA INV.RE.UPDATE_DEFENSE.FLAG 	;($00 = no changes made that effect DAMAGE on the character sheet. $01 changes were made that affect the same)

	
	; ;read files
	; JSR FILE.OPEN.INVENTORY
	; JSR INV.READ_FILE.CHAR_SHEET.READIED
	
	
	; LDA #$01
	; STA INV.CALCULATE.STATS.MODE ;($00 = general mode. only modified equip slots are processed | $01 = levelup mode. Only lhand/rhand and TO-HIT are processed because they use STR/DEX. | $02 = process all mode. Used for character creation)

	; ;***TEMP: this routine is in a temp location, move it to the entrance(s) to the inventory module. 
	; ;i.e. this needs to be the first thing done so that the ready equipment routine (for example) can modify it's values
		; LDA #$00 ;set flags to "don't process"
	; JSR INV.INIT.PROCESS_FLAGS
	

	JMP CALCULATE.COMBAT.STATS.MAIN

@END

CALCULATE.COMBAT.STATS.GENERAL_ENTRANCE
@START

;=====================CODE-SECTION DOCUMENTATION====================================
;
;NOTE: INV.RE.UPDATE_DAMAGE.FLAG and INV.RE.UPDATE_DEFENSE.FLAG are init in INV.INVENTORY_MODULE.ENTRANCE.
;by leaving the values untouched via this entrance, the only way those flags would be set is if they were
;set by INV.READY_UNREADY.EQUIPMENT.ENTRANCE. Accordingly, this routine is used when processing
;of equipment slots is desired only if they were modified by the current inventory module session.
;
;=================================================================================

	LDA #$00
	STA INV.CALCULATE.STATS.MODE ;($00 = general mode. only modified equip slots are processed | $01 = levelup mode. Only lhand/rhand and TO-HIT are processed because they use STR/DEX. | $02 = process all mode. Used for character creation)

			
	;**FALLS THROUGH**
@END

	;**FALLS THROUGH**
	
CALCULATE.COMBAT.STATS.MAIN
@START
;PARAMETERS: CHR_SHEET.PC*
;ENTRANCE:
;RETURN:

;*Must be loaded to the location reserved for it in aux memory. 

;=====================SUBROUTINE DOCUMENTATION====================================
;
;This subroutine calculates the derived stats in CHR_SHEET.PC.
;This is done by examining each readied item and calculating things such as the median damage, armor defense rating and magic resistance. 
;
;---FORMULAS-----
;PC Damage = weapon power rating + weapon skill + strength adjustment + size modifier%
;Strength Adjustment = player strength - 15 (floor)
;
;
;-Equipment Weight (Total Readied)
;When this routine is called in process-all mode, as each readied item is examined when this routine is run the items weight is tallied on the PC character sheet. 
;The code for this is in the left and right hand weapon routines and in .UPDATE.ARMOR.DEFENSE_RATING.TALLY, which is called by
;the armor routine. INV.READY_UNREADY.EQUIPMENT.ENTRANCE adds/subtracts to the Total Readied Equipment Weight as items are
;readied or unreadied so that this routine doesn't need to calculate the total weight from scratch, which would require all equipment
;slots to be processed, which is disk load intensive. 
;
;-Single-Dual Wielding / Shields 
; for left hand, if doesn't have type weapon then shape_ID = $01
; for right hand, if 2HD weapon in right or right hand doesn't have type weapon then set shape_ID = $01
; shape_ID $01 means no weapon readied and hand is skipped in the attack command
;
;An Item_Type of $03 in the left or right hand indicates that the other hand has a 2HD weapon readied. Since the weapon takes
;both hands, the $03 code prevents a weapon_ID from being placed in the fields for the other hand. This is enfored by the inventory module.
;
;
;		
;
;==FILE I/O OVERVIEW==
;The routine is structued with a large loop with the PC sequential number as the counter. 
;Thus, the status for all equipment slots are processed one PC at time. 
;
;Before the loop starts a few things of note happen:
;
;*the inventory file is opened. 
;*the readied equipment data for all players is read into memory
;*the destination address is changed to the item table record.
;
;In the loop, the readied equipment data for the current PC is copied from is memory location
;into a record buffer.
;
;As the equipment slots are processed, the readied equipment data is used to 
;drive reads to the game item tables (stored in the inventory file) which contain the details
;on the readied items. These reads are actually file_offset_reset/seek/reads. 
;
;The seek calculations are done using the appropriate ";ITEM FILE OFFSETS" constants at the end
;of the file (seeks to the start of the desired table) + the offset to the specific record 
;of the table to read.
;
;The reason the file offset reset method is used instead of opening the file every time is because opening
;files is very slow due on floppy due to ProRWTS needing to search for the location of the file on the floppy disk. 
;
;
;=================================================================================


			; LDA #$EE
			; LDX #CHR_SHEET.PC.READIED_EQUIP
			; ldy /CHR_SHEET.PC.READIED_EQUIP
			; ;LDX FILE.OPEN.INVENTORY.DEBUG_COUNTER
			; JSR PREP.BRK
			; BRK
			
.INIT
@START

	;load inventory and readied equipment data from disk
	;(unless this routine was called in general mode, as the data should then already exist in memory)
	LDA INV.CALCULATE.STATS.MODE ;($00 = general mode. only modified equip slots are processed | $01 = levelup mode. Only lhand/rhand and TO-HIT are processed because they use STR/DEX. | $02 = process all mode. Used for character creation)
	BEQ .MODE_SPECIFIC.INIT.DONE
	PHA ;push to stack: INV.CALCULATE.STATS.MODE ;($00 = general mode. only modified equip slots are processed | $01 = levelup mode. Only lhand/rhand and TO-HIT are processed because they use STR/DEX. | $02 = process all mode. Used for character creation)
	
	
	;init only for process-all mode
	PLA ;pull from stack: INV.CALCULATE.STATS.MODE ;($00 = general mode. only modified equip slots are processed | $01 = levelup mode. Only lhand/rhand and TO-HIT are processed because they use STR/DEX. | $02 = process all mode. Used for character creation)
	CMP #$02
	BNE .MODE_SPECIFIC.INIT.DONE
	LDA #$00
	STA CHR_SHEET.PC.READIED_EQUIPMENT_WEIGHT
.MODE_SPECIFIC.INIT.DONE
	
	;LDY #$FF ;init CHR_SHEET.PC.PROCESS_FLAGS index (set to $FF so that increment can be placed at top of loop; it will flip to $00 at start of first iteration)
	LDX #$01 ;init loop counter and player #
.LOOP.CALC.DERIVED_STATS
			

	;read PC character sheet data
		TXA
		;ACC = player sequential # (high-bit not set = read mode)
	JSR COMBAT.READ_WRITE.CHR_SHEET.PC
		;RETURN VALUE = CHR_SHEET.RECORD.READ
		
		TXA
		;ACC = player sequential # (high-bit not set = read mode)
	JSR INV.READ_WRITE_RECORD.CHAR_SHEET.READIED
		;RETURN VALUE: CHR_SHEET.PC.READIED_EQUIP.RECORD.READ($10)

		
		
							


	

@END
	;**FALLS THROUGH**
	
.CALCUALTE.WEAPON.STATS
@START


			
.VALIDATE.WEAPON.ENTRANCE
;(only process left/right hand equipment slot if DAMAGE flag is set which means either the left/right hand equipent slot contents has changed since the last run of this routine)	
	LDA INV.RE.UPDATE_DAMAGE.FLAG 	;($00 = no changes made that effect DAMAGE on the character sheet. $01 changes were made that affect the same)
	BNE .VALIDATE.WEAPON.ENTRANCE.DONE
	JMP .BOTH_HANDS.COMPLETE 
.VALIDATE.WEAPON.ENTRANCE.DONE

.NO_WEAPON.INIT
	;init character sheet values
	;(this needs to be done because if there is no weapon in the left hand, the set left hand code will get skipped
	;and there would be old values in these variables when the right hand code masks-in a value to the LO nibble)
	;update 6/26: if this section is needed it seems like it could be moved to .LEFT_HAND.NO_WEAPON below. 
	

	;**OPT** Memory. It is possible this could be skipped. i.e. if the combat code ignores the nibble with the garbage data because there is no 
	;weapon in that hand then it shouldn't hurt anything. But for testing doing the init seems helpful.

	LDA #$00
	STA CHR_SHEET.PC.SHAPE_ID
	STA CHR_SHEET.PC.WP_RADIUS
	STA CHR_SHEET.PC.WP.SHAPE_TYPE
	STA INV_COMBAT.STR_MODIFIER

.LEFT_HAND.ENTRANCE		
	; ;process this equipment slot?
	; INY ;advance index to current equipment slot ($0)
	; LDA INV.CALCULATE.STATS.MODE ;($00 = general mode. only modified equip slots are processed | $01 = levelup mode. Only lhand/rhand and TO-HIT are processed because they use STR/DEX. | $02 = process all mode. Used for character creation)
	; BNE .LEFT_HAND.START
	; LDA CHR_SHEET.PC.PROCESS_FLAGS,Y
	; STA CHR_SHEET.PC.PROCESS_FLAGS.LHAND ;hands setup as dedicated variables because left/right hand have a weapon and an armor process routine. 
	; BEQ .LEFT_HAND.COMPLETE
	
	;is there an item of type weapon in this hand?
	LDA CHR_SHEET.READIED_EQUIP.TYPE.LHAND ;load readied item_type (left hand)
	;BNE .LEFT_HAND.NO_WEAPON ;($00 = weapon | $01 armor | $02 misc item | $03 empty hand or in use for 2HD in other hand)
	BEQ .LEFT_HAND.WEAPON_CHECK.DONE
	JMP .LEFT_HAND.NO_WEAPON ;($00 = weapon | $01 armor | $02 misc item | $03 empty hand or in use for 2HD in other hand)	
.LEFT_HAND.WEAPON_CHECK.DONE

.READ.LEFT_HAND.ITEM_RECORD
	;READ WEAPON TABLE RECORD
		LDA CHR_SHEET.READIED_EQUIP.ID.LHAND ;load readied weapon_id (left hand)
		STA FILE.ITEM_TABLE.ID
		;set weapon table mode (ACC = $00 = weapon | $01 armor | $02 misc item | $03 empty hand or in use for 2HD in other hand)
		LDA CHR_SHEET.READIED_EQUIP.TYPE.LHAND ;load readied item_type (left hand)				
	JSR FILE.READ.ITEM_TABLES.ENTRANCE
		;RETURN VALUE: FILE.ITEM_TABLE.RECORD.READ(x)

		
.SET.WEAPON_NAME.LEFT_HAND
	JSR INV_7.COPY.ITEM_NAME.LHAND	
	
			; cpx #$03
			; bne .temp
			; LDA #$EE
			; LDX INV.ITEM_TABLE.WP.RADIUS
			; ;LDX CHR_SHEET.PC.WP_RADIUS
			; ;LDX CHR_SHEET.PC_MOB.ARMOR ;PC defense rating
			; ; LDX COMBAT.STATS.DAMAGE.FINAL+$0
			; ; LDY COMBAT.STATS.DAMAGE.FINAL+$1
			; JSR PREP.BRK
			; BRK
; .temp
.SET.WEAPON_FLAGS.LEFT_HAND	
	;update total readied equipment weight
		LDA INV.ITEM_TABLE.ALL.STR_WEIGHT
	JSR INV.TALLY_ITEM_WEIGHT
	
	;SET.MAGIC_WEAPON.FLAGS
		; CHR_SHEET.READIED_EQUIP.ID.LHANDB
		; high-bit not set = no magic weapon
		; high-bit set = magic weapon
	LDA INV.ITEM_TABLE.WP.MAGIC_FLAG ;load magic weapon flag / resist magic bonus field
	BPL .SET.MAGIC_WEAPON.FLAGS.LEFT_HAND.DONE ;branch if high-bit is not set	(no magic weapon)	
	;left hand has magic weapon
	LDA CHR_SHEET.PC.RACE.MGK_FLG ;load race/magic weapon flag field 	
	ORA #$80 ;mask in high-bit
	STA CHR_SHEET.PC.RACE.MGK_FLG ;save race/magic weapon flag field 	
		;CHR_SHEET.PC.RACE.MGK_FLG
		;high bit 7 not set 	= left hand does not have a magic weapon
		;high bit 7 set      	= left hand has a magic weapon
		;high bit 6 not set 	= right hand does not have a magic weapon
		;high bit 6 set       	= right hand has a magic weapon
	JMP .SET.MAGIC_WEAPON.FLAGS.LEFT_HAND.DONE
	
.LEFT_HAND.NO_MAGIC_WEAPON
	LDA CHR_SHEET.PC.RACE.MGK_FLG ;load race/magic weapon flag field 	
	AND #$7F ;mask out high-bit
	STA CHR_SHEET.PC.RACE.MGK_FLG ;save race/magic weapon flag field 	
.SET.MAGIC_WEAPON.FLAGS.LEFT_HAND.DONE

	;;**disabled**
	;;(This is because in order for all items types to have resist magic attribute, there couldn't' be a defense/damage flag separation of the calculations.)
	;ADD WEAPON'S MAGIC RESIST BUFF TO PLAYER CHAR SHEET
	; JSR INV.COMBAT.TALLY.RESIST.MAGIC

	;.SET.WEAPON.SHAPE_ID (on character sheet)
	LDA INV.ITEM_TABLE.WP.SHAPE_ID_HANDS ;load weapon projectile shape_ID (left hand)
	AND #$7F ;mask-out high bit (used to designate a dual handed weapon, but that info isn't needed outside of the inventory module)
	;char sheet: HO nibble = (left hand)
	PHA ;push weapon projectile shape_ID (left hand) to stack; will save us from spending bytes on another LDA + AND later. 
	ASL ;move LO nibble to HO nibble (to align with format of CHR_SHEET.PC.SHAPE_ID)
	ASL
	ASL
	ASL
	STA CHR_SHEET.PC.SHAPE_ID ;save to character sheet: weapon projectile shape_ID field
		
	;.SET.WEAPON_RADIUS (on character sheet)
	LDA INV.ITEM_TABLE.WP.RADIUS ;load weapon radius 
	;char sheet: HO nibble = (left hand)
	ASL ;move LO nibble to HO nibble (to align with format of CHR_SHEET.PC.WP_RADIUS)
	ASL
	ASL
	ASL
	STA CHR_SHEET.PC.WP_RADIUS ;save to character sheet

	;.SET.WEAPON_SHAPE_TYPE (on character sheet)
	LDA INV.ITEM_TABLE.WP.SHAPE_TYPE ;load weapon shape type (left hand)
	STA CHR_SHEET.PC.WP.SHAPE_TYPE ;save to character sheet
	

.SET.TOTAL_DAMAGE.LEFT_HAND
;formula: median damage = weapon power rating + skill damage (min/max appied) + strength adjustment
	
		PLA ;pull weapon projectile shape_ID (left hand) to stack; saves us from spending bytes on another AND.
	JSR INV_COMBAT.CALC.SKILL.DAMAGE ;it's the same code for both hands

	JSR INV_COMBAT.CALCULATE.STR.MOD
		;RETURN VALUE: acc = strength modifier
	
	;ACC = INV_COMBAT.STR_MODIFIER	
	CLC
	ADC INV.ITEM_TABLE.WP.DMG_PWR ;add: weapon damage power rating
	BCS .SET.MAX.DAMANGE.LEFT_HAND
	ADC INV_COMBAT.PLAYER.SKILL.DAMAGE ;add: weapon skill based damage
	BCS .SET.MAX.DAMANGE.LEFT_HAND
	STA CHR_SHEET.PC.DMG.LHAND ;save to left hand damage LO field on character sheet

;****NOTE: this sections turns out not to be needed becuase fists are setup in the master items table with a max/min skill damage of $00,
;which forces the skill modified to $00, leaving the STR mod the only value used for the modified damage total.  
; .TOTAL_DAMAGE.LEFT_HAND.OVERRIDE ;if weapon = fists/hands
; ;(This routine nerfs fists. if readied weapon is hands then limit the modified damage to strength so that 
; ;the benifit of non-fists weapons is magnified. )
	; LDA CHR_SHEET.READIED_EQUIP.ID.LHAND ;load readied weapon_id (left hand)
	; BNE .TOTAL_DAMAGE.LEFT_HAND.OVERRIDE.DONE

	; JSR INV_COMBAT.CALCULATE.STR.MOD
		; ;RETURN VALUE: acc = strength modifier
	; STA CHR_SHEET.PC.DMG.LHAND ;save to left hand damage LO field on character sheet
; .TOTAL_DAMAGE.LEFT_HAND.OVERRIDE.DONE
			
	JMP .LEFT_HAND.COMPLETE
	
.SET.MAX.DAMANGE.LEFT_HAND
	LDA #INV_COMBAT.DAMAGE.MAX	
	STA CHR_SHEET.PC.DMG.LHAND ;save to left hand damage LO field on character sheet

	;**FALLS THROUGH**
	
.LEFT_HAND.COMPLETE
	JMP .RIGHT_HAND.ENTRANCE ;**OPT** Memory. Maybe the JMP to .LEFT_HAND.COMPLETE could just be a JMP to .RIGHT_HAND.ENTRANCE
	
						

.LEFT_HAND.NO_WEAPON
	;.SET.WEAPON.SHAPE_ID (on character sheet)
	LDA #COMBAT.ATTACK_COMMAND.NO_READIED_WEAPON ;used when this hand has shield or other hand has 2hd weapon
	;char sheet: HO nibble = (left hand)
	ASL ;move LO nibble to HO nibble
	ASL
	ASL
	ASL
	STA CHR_SHEET.PC.SHAPE_ID ;save to character sheet: weapon projectile shape_ID field

	;SET WEAPON NAME: ---
	JSR INV_7.SET_NULL.ITEM_NAME.LHAND	

	;SET BASE DAMAGE TO $00
	;(this hand will be skipped in combat because it doesn't have a weapon, but the base damage still needs to be set to $00 for the benifit of the UI stats screens)
	LDA #$00
	STA CHR_SHEET.PC.DMG.RHAND ;save to right hand damage LO field on character sheet			

	
	;**FALLS THROUGH**
	
.RIGHT_HAND.ENTRANCE
	; ;process this equipment slot?
	; INY ;advance index to current equipment slot ($1)
	; LDA INV.CALCULATE.STATS.MODE ;($00 = general mode. only modified equip slots are processed | $01 = levelup mode. Only lhand/rhand and TO-HIT are processed because they use STR/DEX. | $02 = process all mode. Used for character creation)		
	; BNE .RIGHT_HAND.START			
	; LDA CHR_SHEET.PC.PROCESS_FLAGS,Y
	; STA CHR_SHEET.PC.PROCESS_FLAGS.RHAND ;hands setup as dedicated variables because left/right hand have a weapon and an armor process routine. 
	; BNE .PROCESS_FLAG.CHECK.DONE 
	; JMP .RIGHT_HAND.COMPLETE
; .PROCESS_FLAG.CHECK.DONE

.RIGHT_HAND.START
	;is there an item of type weapon in this hand?
	LDA CHR_SHEET.READIED_EQUIP.TYPE.RHAND ;load readied item_type (right hand)
	beq .RIGHT_HAND.WEAPON_CHECK.DONE
	jmp .RIGHT_HAND.NO_WEAPON ;($00 = weapon | $01 armor | $02 misc item | $03 empty hand or in use for 2HD in other hand)
.RIGHT_HAND.WEAPON_CHECK.DONE
	
	
			
.READ.RIGHT_HAND.ITEM_RECORD
	;READ RECORD FROM WEAPON TABLE
		LDA CHR_SHEET.READIED_EQUIP.ID.RHAND ;load readied weapon_id (right hand)
		STA FILE.ITEM_TABLE.ID
		;set weapon table mode (ACC = $00 = weapon | $01 armor | $02 misc item | $03 empty hand or in use for 2HD in other hand)
		LDA CHR_SHEET.READIED_EQUIP.TYPE.RHAND ;load readied item_type (left hand)				
	JSR FILE.READ.ITEM_TABLES.ENTRANCE
		;RETURN VALUE: FILE.ITEM_TABLE.RECORD.READ(x)
		
.SET.WEAPON_NAME.RIGHT_HAND

	JSR INV_7.COPY.ITEM_NAME.RHAND
	
.SET.WEAPON_FLAGS.RIGHT_HAND		
	;update total readied equipment weight
		LDA INV.ITEM_TABLE.ALL.STR_WEIGHT
	JSR INV.TALLY_ITEM_WEIGHT

	
	;SET.MAGIC_WEAPON.FLAGS
		; CHR_SHEET.READIED_EQUIP.ID.LHANDB
		; high-bit not set = no magic weapon
		; high-bit set = magic weapon
	LDA INV.ITEM_TABLE.WP.MAGIC_FLAG ;load magic weapon flag / resist magic bonus field
	BPL .RIGHT_HAND.NO_MAGIC_WEAPON ;branch if high-bit is not set	(no magic weapon)	
	;right hand has magic weapon
	LDA CHR_SHEET.PC.RACE.MGK_FLG ;load race/magic weapon flag field 	
	ORA #$40 ;mask in bit6
	STA CHR_SHEET.PC.RACE.MGK_FLG ;save race/magic weapon flag field 	
		;CHR_SHEET.PC.RACE.MGK_FLG
		;high bit 7 not set 	= left hand does not have a magic weapon
		;high bit 7 set      	= left hand has a magic weapon
		;high bit 6 not set 	= right hand does not have a magic weapon
		;high bit 6 set       	= right hand has a magic weapon
	JMP .SET.MAGIC_WEAPON.FLAGS.RIGHT_HAND.DONE

.RIGHT_HAND.NO_MAGIC_WEAPON
	LDA CHR_SHEET.PC.RACE.MGK_FLG ;load race/magic weapon flag field 	
	AND #$BF ;mask out bit6
	STA CHR_SHEET.PC.RACE.MGK_FLG ;save race/magic weapon flag field 	
.SET.MAGIC_WEAPON.FLAGS.RIGHT_HAND.DONE
	
	;;**disabled**
	;;(This is because in order for all items types to have resist magic attribute, there couldn't' be a defense/damage flag separation of the calculations.)
	; ;ADD WEAPON'S MAGIC RESIST BUFF TO PLAYER CHAR SHEET
	; JSR INV.COMBAT.TALLY.RESIST.MAGIC
	
	;.SET.WEAPON.SHAPE_ID (on character sheet)
	LDA INV.ITEM_TABLE.WP.SHAPE_ID_HANDS ;load weapon projectile shape_ID (right hand)
	AND #$7F ;mask-out high bit (used to designate a dual handed weapon, but that info isn't needed outside of the inventory module)
	PHA ;push weapon projectile shape_ID (left hand) to stack; will save us from spending bytes on another LDA + AND later. 
	;char sheet: LO nibble = (right hand)
	ORA CHR_SHEET.PC.SHAPE_ID ;mask-in weapon projectile shape_ID (right hand) to LO nibble
	STA CHR_SHEET.PC.SHAPE_ID ;save to character sheet: weapon projectile shape_ID field
		
	;.SET.WEAPON_RADIUS (on character sheet)
	LDA CHR_SHEET.PC.WP_RADIUS ;load weapon radius from character sheet (left hand value in HO nibble, LO nibble empty)
	;char sheet: LO nibble = (right hand)
	ORA INV.ITEM_TABLE.WP.RADIUS ;mask-in weapon radius (right hand) to LO nibble
	STA CHR_SHEET.PC.WP_RADIUS ;save to character sheet: radius field

.SET.WEAPON_SHAPE_TYPE ;(on character sheet)
	LDA INV.ITEM_TABLE.WP.SHAPE_TYPE ;load weapon shape type (right hand)
	BNE .RIGHT_HAND.NOT.ANGLED_SHAPE	
;.RIGHT_HAND.ANGLED_SHAPE
	;ANGLED SHAPE = high bit not set. no action required
	STA CHR_SHEET.PC.WP.SHAPE_TYPE ;save to character sheet: weapon shape type flag field
	JMP .SET.WEAPON_SHAPE.TYPE.COMPLETE
	
.RIGHT_HAND.NOT.ANGLED_SHAPE
	ORA #$80 ;mask in high-bit
	STA CHR_SHEET.PC.WP.SHAPE_TYPE ;save to character sheet
	;**FALLS THROUGH**
.SET.WEAPON_SHAPE.TYPE.COMPLETE

	;**FALLS THROUGH**
.SET.WEAPON.FLAGS.COMPLETE	


.SET.TOTAL_DAMAGE.RIGHT_HAND			
		PLA ;pull weapon projectile shape_ID (right hand) to stack; saves us from spending bytes on another AND.
	JSR INV_COMBAT.CALC.SKILL.DAMAGE ;it's the same code for both hands

	JSR INV_COMBAT.CALCULATE.STR.MOD
		;RETURN VALUE: acc = strength modifier
	
	;ACC = INV_COMBAT.STR_MODIFIER
	CLC
	ADC INV.ITEM_TABLE.WP.DMG_PWR ;add: weapon damage power rating
	BCS .SET.MAX.DAMANGE.RIGHT_HAND
	ADC INV_COMBAT.PLAYER.SKILL.DAMAGE ;add: weapon skill based damage
	BCS .SET.MAX.DAMANGE.RIGHT_HAND
	STA CHR_SHEET.PC.DMG.RHAND ;save to right hand damage LO field on character sheet			
	JMP .RIGHT_HAND.COMPLETE
	
.SET.MAX.DAMANGE.RIGHT_HAND
	LDA #INV_COMBAT.DAMAGE.MAX	
	STA CHR_SHEET.PC.DMG.LHAND ;save to left hand damage LO field on character sheet
	
	;**FALLS THROUGH**
	
.RIGHT_HAND.COMPLETE

		
	JMP .BOTH_HANDS.COMPLETE
	
.RIGHT_HAND.NO_WEAPON
	;.SET.WEAPON.SHAPE_ID (on character sheet)
	LDA #COMBAT.ATTACK_COMMAND.NO_READIED_WEAPON ;used when this hand has shield or other hand has 2hd weapon
	;char sheet: LO nibble = (right hand)
	ORA CHR_SHEET.PC.SHAPE_ID ;mask-in weapon projectile shape_ID (right hand) to LO nibble
	STA CHR_SHEET.PC.SHAPE_ID ;save to character sheet: weapon projectile shape_ID field

	;SET WEAPON NAME: ---
	JSR INV_7.SET_NULL.ITEM_NAME.RHAND
	
	;SET BASE DAMAGE TO $00
	;(this hand will be skipped in combat because it doesn't have a weapon, but the base damage still needs to be set to $00 for the benifit of the UI stats screens)
	LDA #$00
	STA CHR_SHEET.PC.DMG.RHAND ;save to right hand damage LO field on character sheet			
	
	
	;JMP .SINGLE_WIELDING 

	;**FALLS THROUGH**

.BOTH_HANDS.COMPLETE 


			
			
	
		; lda #$ae
		; ldx #CHR_SHEET.PC.PROCESS_FLAGS
		; ldy /CHR_SHEET.PC.PROCESS_FLAGS
		; jsr prep.brk
		; brk

			
; .SET.DUAL_SINGLE.WIELDING
@START
	; ;.SET.WEAPON_QTY (on character sheet)
	; LDA CHR_SHEET.READIED_EQUIP.ID.LHAND ;read readied weapon_ID (left hand)
	; CMP #INV_COMBAT.NO_READIED_WEAPON
	; BCS .SINGLE_WIELDING
	; LDA CHR_SHEET.READIED_EQUIP.ID.RHAND ;read readied weapon_ID (right hand)
	; CMP #INV_COMBAT.NO_READIED_WEAPON
	; BCS .SINGLE_WIELDING
	
	; ;**FALLS THROUGH**
	
; .DUAL_WIELDING
	; LDA CHR_SHEET.PC.RACE.MGK_FLG ;load race / weapon qty
	; ORA #$80 ;mask in high-bit
	; STA CHR_SHEET.PC.RACE.MGK_FLG ;save to character sheet with weapon qty set
	; JMP .SET.DUAL_SINGLE.WIELDING.COMPLETE	
	
; .SINGLE_WIELDING
	; LDA CHR_SHEET.PC.RACE.MGK_FLG ;load race / weapon qty
	; AND #$7F ;mask out high-bit
	; STA CHR_SHEET.PC.RACE.MGK_FLG ;save to character sheet with weapon qty set
	; ;**FALLS THROUGH**
; .SET.DUAL_SINGLE.WIELDING.COMPLETE	

		; lda CHR_SHEET.PC.ATTRIB.STR
		;; lda #$aa
		; ldx INV.ITEM_TABLE.WP.DMG_PWR ;add: weapon damage power rating
		; ldy INV_COMBAT.PLAYER.SKILL.DAMAGE ;add: weapon skill based damage
@END

		
;.CALCULATE.DAMAGE_RANGE
@START	
; .CALC.DMG_RNG.LEFT	
	; ;is there an item of type weapon in this hand?
	; ;[hands (fist) is the default weapon assigned in the main inventory module, but hands can also contain items that 
	; ;are type armor or type misc item, neither of which get an attack turn and thus do not need the stats set in this section)

	; LDA CHR_SHEET.READIED_EQUIP.TYPE.LHAND ;load readied item_type (left hand)
	; BNE .CALC.DMG_RNG.RIGHT  ;($00 = weapon | $01 armor | $02 misc item | $03 empty hand or in use for 2HD in other hand)
	
	; ;set damage left hand
	; LDA CHR_SHEET.PC.DMG.LHAND ;load damage LO field, which currently contains the median damage value
	; SEC
	; SBC #INV_COMBAT.DAMAGE_RANGE.HALF_SIZE
	; STA CHR_SHEET.PC.DMG.LHAND ;save damage LO field (now it really is the LO value)	
	; BCC .UNDERFLOW_ERROR_STEP
	; CLC
	; ADC #INV_COMBAT.DAMAGE_RANGE.SIZE
	; STA CHR_SHEET.PC.DMG_HI.LHAND ;save damage HO field (now it really is the HO value)
	
	; ;**FALLS THROUGH**

; .CALC.DMG_RNG.RIGHT		
	; ;is there an item of type weapon in this hand?
	; ;[hands (fist) is the default weapon assigned in the main inventory module, but hands can also contain items that 
	; ;are type armor or type misc item, neither of which get an attack turn and thus do not need the stats set in this section)

	; LDA CHR_SHEET.READIED_EQUIP.TYPE.RHAND ;load readied item_type (right hand)
	; BNE .CALC.DMG_RNG.RIGHT_COMPLETE ;($00 = weapon | $01 armor | $02 misc item | $03 empty hand or in use for 2HD in other hand)

	; ;set damage right hand
	; LDA CHR_SHEET.PC.DMG.RHAND ;load damage LO field, which currently contains the median damage value
	; SEC
	; SBC #INV_COMBAT.DAMAGE_RANGE.HALF_SIZE
	; STA CHR_SHEET.PC.DMG.RHAND ;save damage LO field (now it really is the LO value)
	; BCC .UNDERFLOW_ERROR_STEP
	; CLC
	; ADC #INV_COMBAT.DAMAGE_RANGE.SIZE
	; STA CHR_SHEET.PC.DMG_HI.RHAND ;save damage HO field (now it really is the HO value)
	; ;**FALLS THROUGH**
; .CALC.DMG_RNG.RIGHT_COMPLETE

@END

.CALCULATE.WEAPON.STATS.COMPLETE
@END


			
.CALCULATE.ARMOR.STATS
@START

			
.VALIDATE.ARMOR.ENTRANCE
	;(only process left/right hand equipment slot if DAMAGE flag is set which means either the left/right hand equipent slot contents has changed since the last run of this routine)	
	LDA INV.RE.UPDATE_DEFENSE.FLAG 	;($00 = no changes made that effect DEFENSE on the character sheet. $01 changes were made that affect the same)
	BNE .VALIDATE.ARMOR.ENTRANCE.DONE
	JMP .CALCULATE.ARMOR.STATS.COMPLETE	
.VALIDATE.ARMOR.ENTRANCE.DONE

			
.ARMOR.INIT
	LDA #$00
	STA CHR_SHEET.PC_MOB.ARMOR 
	STA CHR_SHEET.PC_MOB.RESIST_MAGIC
	
	
;SAVE REGISTERS
;(needed because .UPDATE.ARMOR.DEFENSE_RATING.TALLY uses Y-REG)
	TXA
	PHA
	
.LEFT_HAND.ARMOR
	; ;process this equipment slot?
	; LDA CHR_SHEET.PC.PROCESS_FLAGS.LHAND ;hands setup as dedicated variables because left/right hand have a weapon and an armor process routine. 
	; BEQ .LEFT_HAND.ARMOR.COMPLETE
	
	;is there an item of type armor in this hand?
	;(when weapon/armor is unreadied, set body part 
	;to empty hands/skin. fists should be in the players 
	;inventory automatically (weapon_ID $00) which has to be readied. 
	;Skin is item_ID $00 in the armor table. Fists get an attack turn, empty hands (item_type $03) do not.) 
	;shields are readed to the same fields as weapons and are identified by the item_type = $01 (armor)
	
	LDA CHR_SHEET.READIED_EQUIP.TYPE.LHAND ;load readied item_type (left hand)
	CMP #$01
	BNE .LEFT_HAND.NO_ARMOR ;($00 = weapon | $01 armor | $02 misc item | $03 empty hand or in use for 2HD in other hand)
	

.SET.ARMOR_FLAGS.LEFT_HAND

		;**OPT** Memory. Combine left hand and right hand code into a single routine with the armor_ID as the parameter
		
	;read record from armor table
		LDX CHR_SHEET.READIED_EQUIP.ID.LHAND ;load readied armor_id (left hand)
		LDA CHR_SHEET.READIED_EQUIP.TYPE.LHAND ;load readied armor_id (left hand)
	JSR .UPDATE.ARMOR.DEFENSE_RATING.TALLY
		;RETURN VALUE: CHR_SHEET.PC_MOB.ARMOR, CHR_SHEET.PC.READIED_EQUIPMENT_WEIGHT, CHR_SHEET.PC_MOB.RESIST_MAGIC, FILE.ITEM_TABLE.RECORD.READ(x)

.SET.ITEM_NAME.LEFT_HAND
	JSR INV_7.COPY.ITEM_NAME.LHAND
	
	;**FALLS THROUGH**
	
.LEFT_HAND.NO_ARMOR
.LEFT_HAND.ARMOR.COMPLETE

.RIGHT_HAND.ARMOR
	; ;process this equipment slot?
	; LDA CHR_SHEET.PC.PROCESS_FLAGS.LHAND ;hands setup as dedicated variables because left/right hand have a weapon and an armor process routine. 
	; BEQ .RIGHT_HAND.ARMOR.COMPLETE
	
	;is there an item of type armor in this hand?
	;(when weapon/armor is unreadied, set body part 
	;to empty hands/skin. fists should be in the players 
	;inventory automatically (weapon_ID $00) which has to be readied. 
	;Skin is item_ID $00 in the armor table. Fists get an attack turn, empty hands (item_type $03) do not.) 
	;shields are readed to the same fields as weapons and are identified by the item_type = $01 (armor)
	
	LDA CHR_SHEET.READIED_EQUIP.TYPE.RHAND ;load readied item_type (left hand)
	CMP #$01
	BNE .RIGHT_HAND.NO_ARMOR ;($00 = weapon | $01 armor | $02 misc item | $03 empty hand or in use for 2HD in other hand)
	

.SET.ARMOR_FLAGS.RIGHT_HAND
	;read record from armor table
		LDX CHR_SHEET.READIED_EQUIP.ID.RHAND ;load readied armor_id (left hand)
		LDA CHR_SHEET.READIED_EQUIP.TYPE.RHAND ;load readied armor_id (left hand)
	JSR .UPDATE.ARMOR.DEFENSE_RATING.TALLY
		;RETURN VALUE: CHR_SHEET.PC_MOB.ARMOR, CHR_SHEET.PC.READIED_EQUIPMENT_WEIGHT, CHR_SHEET.PC_MOB.RESIST_MAGIC, FILE.ITEM_TABLE.RECORD.READ(x)

.SET.ITEM_NAME.RIGHT_HAND
	JSR INV_7.COPY.ITEM_NAME.RHAND

	
	;**FALLS THROUGH**
	
.RIGHT_HAND.NO_ARMOR
.RIGHT_HAND.ARMOR.COMPLETE

	;**FALLS THROUGH**

.HEAD	
	; ;process this equipment slot?
	; INY ;advance index to current equipment slot ($2)
	; LDA CHR_SHEET.PC.PROCESS_FLAGS,Y
	; BEQ .HEAD.ARMOR.COMPLETE
		
		LDX CHR_SHEET.READIED_EQUIP.ID.HEAD ;load readied armor_id (head)
		LDA CHR_SHEET.READIED_EQUIP.TYPE.HEAD ;load readied armor_id (head)
	JSR .UPDATE.ARMOR.DEFENSE_RATING.TALLY
.HEAD.ARMOR.COMPLETE
	
	;**FALLS THROUGH**

.TORSO
	; ;process this equipment slot?
	; INY ;advance index to current equipment slot ($3)
	; LDA CHR_SHEET.PC.PROCESS_FLAGS,Y
	; BEQ .TORSO.ARMOR.COMPLETE
	
			; LDA #$01
			; STA	troubleshooting.hook
			
		LDX CHR_SHEET.READIED_EQUIP.ID.TORSO ;load readied armor_id (torso)
		LDA CHR_SHEET.READIED_EQUIP.TYPE.TORSO ;load readied armor_id (torso)
	JSR .UPDATE.ARMOR.DEFENSE_RATING.TALLY
		;RETURN VALUE: CHR_SHEET.PC_MOB.ARMOR, CHR_SHEET.PC.READIED_EQUIPMENT_WEIGHT, CHR_SHEET.PC_MOB.RESIST_MAGIC, FILE.ITEM_TABLE.RECORD.READ(x)
	
				; LDA CHR_SHEET.PC_MOB.ARMOR
				; STA $BE00
				; LDA #$BA
				; LDX #FILE.ITEM_TABLE.RECORD.READ
				; LDY /FILE.ITEM_TABLE.RECORD.READ
				; JSR PREP.BRK
				; BRK
				
.TORSO.ARMOR.COMPLETE

	;**FALLS THROUGH**
	
.FEET
	
	; ;process this equipment slot?
	; INY ;advance index to current equipment slot ($4)
	; LDA CHR_SHEET.PC.PROCESS_FLAGS,Y
	; BEQ .FEET.ARMOR.COMPLETE
	
		LDX CHR_SHEET.READIED_EQUIP.ID.FEET ;load readied armor_id (feet)
		LDA CHR_SHEET.READIED_EQUIP.TYPE.FEET ;load readied armor_id (feet)
	JSR .UPDATE.ARMOR.DEFENSE_RATING.TALLY
.FEET.ARMOR.COMPLETE
	
	;**FALLS THROUGH**

.HAND_COVERING

	; ;process this equipment slot?
	; INY ;advance index to current equipment slot ($5)
	; LDA CHR_SHEET.PC.PROCESS_FLAGS,Y
	; BEQ .HAND_COVERING.ARMOR.COMPLETE
	
		LDX CHR_SHEET.READIED_EQUIP.ID.HAND_COVERING ;load readied armor_id (hand covering)
		LDA CHR_SHEET.READIED_EQUIP.TYPE.HAND_COVERING ;load readied armor_id (hand covering)
	JSR .UPDATE.ARMOR.DEFENSE_RATING.TALLY
.HAND_COVERING.ARMOR.COMPLETE

	;**FALLS THROUGH**
	
.FINGER





	; ;process this equipment slot?
	; INY ;advance index to current equipment slot ($6)
	; LDA CHR_SHEET.PC.PROCESS_FLAGS,Y
	; BEQ .FINGER.ARMOR.COMPLETE
	
		LDX CHR_SHEET.READIED_EQUIP.ID.FINGER ;load readied armor_id (hand covering)
		LDA CHR_SHEET.READIED_EQUIP.TYPE.FINGER
	JSR .UPDATE.ARMOR.DEFENSE_RATING.TALLY
.FINGER.ARMOR.COMPLETE

	;**FALLS THROUGH**

.NECK
	; ;process this equipment slot?
	; INY ;advance index to current equipment slot ($7)
	; LDA CHR_SHEET.PC.PROCESS_FLAGS,Y
	; BEQ .NECK.ARMOR.COMPLETE
	
		LDX CHR_SHEET.READIED_EQUIP.ID.NECK ;load readied armor_id (hand covering)
		LDA CHR_SHEET.READIED_EQUIP.TYPE.NECK ;load readied armor_id (hand covering)
	JSR .UPDATE.ARMOR.DEFENSE_RATING.TALLY
.NECK.ARMOR.COMPLETE

	;**FALLS THROUGH**

.CALCULATE.ARMOR.STATS.EXIT		
;RESTORE REGISTERS
	PLA
	TAX
	
	JMP .CALCULATE.ARMOR.STATS.COMPLETE	

; .UNDERFLOW_ERROR_STEP
	; JMP .UNDERFLOW_ERROR
	
.UPDATE.ARMOR.DEFENSE_RATING.TALLY
@START
;PARAMETERS: ACC = ITEM_TYPE, Y-REG = ITEM_ID
;ENTRANCE: various routines in ;CALCUALTE ARMOR STATS
;RETURN: CHR_SHEET.PC_MOB.ARMOR, CHR_SHEET.PC.READIED_EQUIPMENT_WEIGHT, CHR_SHEET.PC_MOB.RESIST_MAGIC, FILE.ITEM_TABLE.RECORD.READ(x)

	;read record from armor table



						
		STX FILE.ITEM_TABLE.ID		
		;ACC = item_type_ID from CHR_SHEET.PC.READIED_EQUIP.RECORD.READ
		;ACC ;parm: item table mode (ACC = $00 = weapon | $01 armor | $02 misc item | $03 empty hand or in use for 2HD in other hand)			
	JSR FILE.READ.ITEM_TABLES.ENTRANCE
		;RETURN VALUE: FILE.ITEM_TABLE.RECORD.READ


		
	;update armor stats tally
	LDA CHR_SHEET.PC_MOB.ARMOR ;load armor rating field
	CLC
	ADC INV.ITEM_TABLE.AR_MISC.DEFENSE_RATING ;load defense rating or armor item
	STA CHR_SHEET.PC_MOB.ARMOR ;save armor rating field 

	

			; STA TEMP
			; LDA troubleshooting.hook
			; CMP #$01
			; BNE .TEMP
			; cpx #$06
			; bne .temp
			; ;LDA #$AA
			; LDA FILE.ITEM_TABLE.ID
			; LDX CHR_SHEET.PC_MOB.ARMOR ;PC defense rating
			; LDY INV.ITEM_TABLE.AR_MISC.DEFENSE_RATING ;load defense rating of armor item
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP


	;update total readied equipment weight
		LDA INV.ITEM_TABLE.ALL.STR_WEIGHT
	JSR INV.TALLY_ITEM_WEIGHT

	; lda INV.ITEM_TABLE.ALL.STR_WEIGHT
	; CMP #$01
	; bne .error
	
	 ;**FALLS THROUGH**

	;add weapon's magic resist buff to player char sheet
	JSR INV.COMBAT.TALLY.RESIST.MAGIC
	
	RTS
	
; .error
	; ldx #$aa
	; jsr prep.brk
	; brk
	
@END


.CALCULATE.ARMOR.STATS.COMPLETE	

			
			
@END


.CALCULATE.DEXTERITY.MODIFIER ;always processed because there is no file I/O
@START
;Formula: DEX / 3

		LDA CHR_SHEET.PC.ATTRIB.DEX ;load player dexterity
	JSR CONVERT.HEX.8_TO_BCD.16
		LDA BCD+$0 ;load converted value	
		STA DIVIDEND+$0 ;number to be divided
		LDA BCD+$1 ;load converted value	
		STA DIVIDEND+$1
		LDA #$03
		STA DIVISOR+$0 ;number to divide by
		LDA #$00
		STA DIVISOR+$1
	JSR DIV.16.BCD
		;RETURN VALUE: result+$0 (quotient LO), result+$1 (quotient HO), result+$2 (remainder LO), result+$3 (remainder HO)
		LDA RESULT+$0 ;(dex /3)

				
	SED ;set decimal mode
	CLC
	ADC #INV_COMBAT.TO_HIT.BASE
	BCS .SET.MAX.TO_HIT
	STA CHR_SHEET.PC_MOB.TO_HIT ;save to TO-HIT % field
	;CLD ;clear decimal mode is done at end of routine below
	
	;**FALLS THROUGH**
	
.CHECK.TO_HIT.MAX
;the modifiers might collectively result in a TO-HIT value lower than the minimum or greater than the maximum. 
;If that happens, this routine overrides and sets the minimum or maximum as applies.

	LDA CHR_SHEET.PC_MOB.TO_HIT ;load TO-HIT % field	
	CMP #COMBAT.STATS.TO_HIT.MAX
	BCS .SET.MAX.TO_HIT

	JMP .CALCULATE.DEXTERITY.MODIFIER.COMPLETE
	

.SET.MAX.TO_HIT	
	LDA #COMBAT.STATS.TO_HIT.MAX	
	STA	CHR_SHEET.PC_MOB.TO_HIT ;load TO-HIT % field

	;**FALLS THROUGH**
	
.CALCULATE.DEXTERITY.MODIFIER.COMPLETE
	CLD ;clear decimal mode

	 ;**FALLS THROUGH**
	
;===========ORIGINAL FORMULA=====
;
;Formula: DEX Modifier = (dex - (PC level * 6)
		;
		; LDA #$06 ;parm: multiplier
	; JSR INV_COMBAT.CALCULATE.LEVEL_MULTIPLE			
		; ;return value: RESULT+$0
		; LDA RESULT+$0 ;(RESULT = PC level *6)			
		; CMP CHR_SHEET.PC.ATTRIB.DEX ;load player dexterity
		; BCS .CALCULATE.DEXTERITY.MODIFIER.COMPLETE ;if dodge deduction is >= dodge skill then skip dodge modifier as it equals $00. 		
	; LDA CHR_SHEET.PC.ATTRIB.DEX ;load player dexterity
	; SEC
	; SBC RESULT+$0 ;subtract: (PC level *6)
		; ;ACC is parameter
	; JSR CONVERT.HEX.8_TO_BCD.16
	; LDA BCD+$0 ;load converted value	
	; SED ;set decimal mode
	; CLC
	; ADC #INV_COMBAT.TO_HIT.BASE
	; BCS .SET.MAX.TO_HIT
	; STA CHR_SHEET.PC_MOB.TO_HIT ;save to TO-HIT % field
	; CLD ;clear decimal mode
	;
	; ;**FALLS THROUGH**
	;
; .CHECK.TO_HIT.MAX
; ;the modifiers might collectively result in a TO-HIT value lower than the minimum or greater than the maximum. 
; ;If that happens, this routine overrides and sets the minimum or maximum as applies.
;
	; LDA CHR_SHEET.PC_MOB.TO_HIT ;load TO-HIT % field	
	; CMP #COMBAT.STATS.TO_HIT.MAX
	; BCS .SET.MAX.TO_HIT
;
	; JMP .CALCULATE.DEXTERITY.MODIFIER.COMPLETE
	;
;
; .SET.MAX.TO_HIT	
	; LDA #COMBAT.STATS.TO_HIT.MAX	
	; STA	CHR_SHEET.PC_MOB.TO_HIT ;load TO-HIT % field
;
	; ;**FALLS THROUGH**
	;
; .CALCULATE.DEXTERITY.MODIFIER.COMPLETE
	
		
@END






			
			
.WRITE.CHAR_SHEET
		TXA
		ORA #$80 ;set write mode by masking in high-bit
		;ACC = player sequential # + (high-bit not set = read mode | high-bit set = write mode)
	JSR COMBAT.READ_WRITE.CHR_SHEET.PC	


		; lda #$aa
		; ldx CHR_SHEET.PC.DMG.LHAND
		; ldy INV_COMBAT.STR_MODIFIER		 ;save strength modifier
		; ;ldy CHR_SHEET.PC.DMG.RHAND
		; jsr prep.brk
		; brk
	
			; cpx #$02
			; bne .temp
			; LDA #$AB
			; LDX CHR_SHEET.PC.WP_RADIUS
			; ;LDX CHR_SHEET.PC_MOB.ARMOR ;PC defense rating
			; ; LDX COMBAT.STATS.DAMAGE.FINAL+$0
			; ; LDY COMBAT.STATS.DAMAGE.FINAL+$1
			; JSR PREP.BRK
			; BRK
; .temp




			
	;exit test
	CPX PARTY.TOTAL.PC
	BEQ .LOOP.CALC.DERIVED_STATS.COMPLETE
	INX
	
	
			
	JMP .LOOP.CALC.DERIVED_STATS
.LOOP.CALC.DERIVED_STATS.COMPLETE
		
			
					
	;JMP .EXIT

	;TROUBLESHOOTING: RUN CALC.STATS TWICE
		;; INC TROUBLESHOOTING.HOOK
		;; LDA TROUBLESHOOTING.HOOK
		;; CMP #$02
		;; BEQ .TEMP2
		;; JMP CALCULATE.COMBAT.STATS.LEVELUP_ENTRANCE
;; .TEMP2
	
;TROUBLESHOOTING: CHECK PC CHAR_SHEET DATA
@START



; ;;TROUBLESHOOTING: PC CHARACTER SHEET READ
	; ;;read PC character sheet data
		; LDA #$01
		; ;ACC = player sequential # (high-bit not set = read mode)
	; JSR COMBAT.READ_WRITE.CHR_SHEET.PC
		; ;RETURN VALUE = CHR_SHEET.RECORD.READ
		; ;
			; ;TROUBLESHOOTING LOOP
			; LDX #$00
; .TEST.LOOP1			
			; LDA CHR_SHEET.RECORD.READ,X
			; STA $BA00,X
			; INX
			; BNE .TEST.LOOP1
			; ;
; ;
; ;TROUBLESHOOTING: PC CHARACTER SHEET READ
	; ;read PC character sheet data
			; LDA #$02
			; ;ACC = player sequential # (high-bit not set = read mode)
		; JSR COMBAT.READ_WRITE.CHR_SHEET.PC
			; ;RETURN VALUE = CHR_SHEET.RECORD.READ
			; ;
			; ;TROUBLESHOOTING LOOP
			; LDX #$00
; .TEST.LOOP2			
			; LDA CHR_SHEET.RECORD.READ,X
			; STA $BB00,X
			; INX
			; BNE .TEST.LOOP2
; ;
; ;TROUBLESHOOTING: PC CHARACTER SHEET READ
	; ;read PC character sheet data
		; LDA #$03
		; ;ACC = player sequential # (high-bit not set = read mode)
	; JSR COMBAT.READ_WRITE.CHR_SHEET.PC
		; ;RETURN VALUE = CHR_SHEET.RECORD.READ
		; ;
			; ;TROUBLESHOOTING LOOP
			; LDX #$00
; .TEST.LOOP3			
			; LDA CHR_SHEET.RECORD.READ,X
			; STA $BC00,X
			; INX
			; BNE .TEST.LOOP3
; ;
; ;TROUBLESHOOTING: PC CHARACTER SHEET READ
	; ;read PC character sheet data
		; LDA #$04
		; ;ACC = player sequential # (high-bit not set = read mode)
	; JSR COMBAT.READ_WRITE.CHR_SHEET.PC
		; ;RETURN VALUE = CHR_SHEET.RECORD.READ
		; ;
			; ;TROUBLESHOOTING LOOP
			; LDX #$00
; .TEST.LOOP4			
			; LDA CHR_SHEET.RECORD.READ,X
			; STA $BD00,X
			; INX
			; BNE .TEST.LOOP4
; ;
			; ;
; ;TROUBLESHOOTING: PC CHARACTER SHEET READ
	; ;read PC character sheet data
		; LDA #$05
		; ;ACC = player sequential # (high-bit not set = read mode)
	; JSR COMBAT.READ_WRITE.CHR_SHEET.PC
		; ;RETURN VALUE = CHR_SHEET.RECORD.READ
	; ;	
			; ;TROUBLESHOOTING LOOP
			; LDX #$00
; .TEST.LOOP5			
			; LDA CHR_SHEET.RECORD.READ,X
			; STA $BE00,X
			; INX
			; BNE .TEST.LOOP5
; ;
; ;
; ;
; ;TROUBLESHOOTING: PC CHARACTER SHEET READ
	; ;read PC character sheet data
		; LDA #$06
		; ;ACC = player sequential # (high-bit not set = read mode)
	; JSR COMBAT.READ_WRITE.CHR_SHEET.PC
		; ;RETURN VALUE = CHR_SHEET.RECORD.READ
; ;		
			; ;TROUBLESHOOTING LOOP
			; LDX #$00
; .TEST.LOOP6			
			; LDA CHR_SHEET.RECORD.READ,X
			; STA $BF00,X
			; INX
			; BNE .TEST.LOOP6
; ;
; ;
; ;
			; LDA #$EE
			; LDX #CHR_SHEET.PC.READIED_EQUIP
			; ldy /CHR_SHEET.PC.READIED_EQUIP
			; ;LDX FILE.OPEN.INVENTORY.DEBUG_COUNTER
			; JSR PREP.BRK
			; BRK
; ;
; ;					
; ;
; ;-------END TROUBLESHOOTING HOOK
			




			
.EXIT


			
	RTS
	



; DEBUG.HOOK.WRONG_DAMAGE_VALUES	

			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
; ;;TROUBLESHOOTING: PC CHARACTER SHEET READ
	; ;;read PC character sheet data
		; LDA #$01
		; ;ACC = player sequential # (high-bit not set = read mode)
	; JSR COMBAT.READ_WRITE.CHR_SHEET.PC
		; ;RETURN VALUE = CHR_SHEET.RECORD.READ
		; ;
			; ;TROUBLESHOOTING LOOP
			; LDX #$00
; .TEST.LOOP1			
			; LDA CHR_SHEET.RECORD.READ,X
			; STA $BA00,X
			; INX
			; BNE .TEST.LOOP1
			; ;			
			
			; lda INV.READY_UNREADY.RUN_FLAG		
			; sta $be00
			; lda INV.RE.UPDATE_DAMAGE.FLAG
			; sta $be01
			; lda INV.RE.UPDATE_DEFENSE.FLAG
			; sta $be02
			; lda #$aa	
			; LDX #CHR_SHEET.PC.READIED_EQUIP.RECORD.READ
			; LDY /CHR_SHEET.PC.READIED_EQUIP.RECORD.READ
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP
			
	; RTS
	
	
	

@END

; .UNDERFLOW_ERROR
; ;.CALCULATE.DAMAGE_RANGE reports an underflow error..
; ;
; ;***WARNING: this could be cause if weapons or armor are added to the game which 
; ;have a damage/refense rating smaller than #INV_COMBAT.DAMAGE_RANGE.HALF_SIZE or #INV_COMBAT.ARMOR_RANGE.HALF_SIZE
; ;Long term: when an underflow occurs should just result in the LO range being set to $00. 
; ;
; ;more specifically, currently this error occurs when the median damage of defense value calculated by the below
; ;
; ;damage related labels
; ;.SET.TOTAL_DAMAGE.LEFT_HAND or .SET.TOTAL_DAMAGE.RIGHT_HAND
; ;is less than #INV_COMBAT.DAMAGE_RANGE.HALF_SIZE, which is not expected.
; ;
; ;defense related lables
; ;(I didn't record, search for underflow_error to find them)
; ;
; ;
; ;

INV_COMBAT.CALCULATE.STR.MOD
@START
;PARAMETERS: none
;ENTRANCE: CALCULATE.COMBAT.STATS.GENERAL_ENTRANCE
;RETURN: INV_COMBAT.STR_MODIFIER

;formula: mod = strength / 4*
;*melee weapons only



	LDA INV.ITEM_TABLE.WP.SHAPE_ID_HANDS ;load weapon projectile shape_ID (left hand)		
	;LDA CHR_SHEET.PC.WP.SHAPE_TYPE ;($00 = melee weapon | >=$01 = range weapon shape ID)
	BEQ .CALC.MOD ;branch if melee weapon
	LDA #$00
	JMP .SAVE.MOD.VALUE
.CALC.MOD	
	LDA CHR_SHEET.PC.ATTRIB.STR ;load player strength
	LSR ;/2
	LSR ;/4
.SAVE.MOD.VALUE
	STA INV_COMBAT.STR_MODIFIER		 ;save strength modifier

.EXIT
		
			; LDA #$AA
			; LDX INV_COMBAT.STR_MODIFIER
			; ; LDY CHR_SHEET.PC.DMG.LHAND ;save to left hand damage LO field on character sheet
			; LDY CHR_SHEET.PC.WP.SHAPE_TYPE ;($00 = melee weapon | >=$01 = range weapon shape ID)
			; JSR PREP.BRK
			; BRK
			
	RTS
	
; .ERROR.LEVEL_FIELD.OVERFLOW
; ;INV_COMBAT.CALCULATE.STR.MOD reports that 
; ;multiplication on the PC level field resultsed in an 8-bit overflow
	; JSR PREP.BRK
	; BRK
	
;=====ORIGINAL FORMULA=====	
; ;Formula: strength Modifier = (str - (PC level * 3)
		;
		; LDA #$03 ;parm: multiplier
	; JSR INV_COMBAT.CALCULATE.LEVEL_MULTIPLE
		; ;RETURN VALUE: acc = (PC level * 3)
	; JSR MLP.16.NO_BCD
		; ;return value: RESULT+$0
		; LDA RESULT+$0 ;(RESULT = PC level *6)			
		; CMP CHR_SHEET.PC.ATTRIB.STR ;load player strength
		; BCS .CALCULATE.STRENGTH.MODIFIER.COMPLETE ;if dodge deduction is >= dodge skill then skip dodge modifier as it equals $00. 		
	; LDA CHR_SHEET.PC.ATTRIB.STR ;load player strength
	; SEC
	; SBC RESULT+$0 ;subtract: (PC level *3)
	; STA INV_COMBAT.STR_MODIFIER		 ;save strength modifier
	;
; .CALCULATE.STRENGTH.MODIFIER.COMPLETE
; .EXIT
	; RTS
	
@END
	
INV_COMBAT.CALCULATE.LEVEL_MULTIPLE
@START
; ;PARAMETERS: ACC (multiplier)
; ;ENTRANCE: DIRECT
; ;RETURN: RESULT(2)*

; ;*contains the result of the calculation: level * multiplier

		; ;ACC = parm: multiplier
		; STA MULPLR
		; LDA #$00
		; STA MULPLR+$1
		; ;
		; LDA CHR_SHEET.PC_MOB.LEVEL ;load PC level
		; STA MULCND
		; LDA #$00
		; STA MULCND+$01
	; JSR MLP.16.NO_BCD

	; RTS
@END
	
INV_COMBAT.CALC.SKILL.DAMAGE
@START
;PARAMETERS: CHR_SHEET.PC_MOB.LEVELA or $1B (weapon projectile shape_ID left/right hand)
;RETURN: INV_COMBAT.PLAYER.SKILL.DAMAGE

;Formula: Weapon skill dmg mod = weapon skill / 4 * 3
;Formula: Weapon skill mod = weapon skill / 3

;calculate skill based portion of weapon damage

;**OPT** Memory. The range and melee min/max routines can probably be consolidated into a routine where the player skill (melee or ranged) is a parm to the routine. 

;SAVE PARAMETERS
	;STA INV_COMBAT.PLAYER.SKILL.TYPE

	
		
			
.DETECT.WEAPON.TYPE	
	;ACC = parameter: CHR_SHEET.PC_MOB.LEVELA or $1B ;weapon projectile shape_ID (left/right hand)
	CMP #$00 ;$00 = no shape (melee)
	BNE .USE.RANGED_SKILL
;.USE.MELEE_SKILL
	LDA CHR_SHEET.PC.SKILL.MELEE ;PC melee weapon skill
			
	JMP .DETECT.WEAPON.TYPE.DONE
	
.USE.RANGED_SKILL
	LDA CHR_SHEET.PC.SKILL.RANGE ;PC range weapon skill
.DETECT.WEAPON.TYPE.DONE
	;**FALLS THROUGH**
	
;APPLY WEAPON SKILL FORMULA

;formula: (weapon skill / 4)
	LSR ;/2
	LSR ;/4
	STA INV_COMBAT.PLAYER.SKILL.DAMAGE 
			
			STA COW
			
			
; ;formula: (weapon skill / 3)
; ;
		; ;ACC = melee or range weapon skill 
		; STA DIVIDEND+$0		;number to be divided
		; LDA #$00
		; STA DIVIDEND+$1
		; LDA	#$03
		; STA DIVISOR+$0		;number to divide by
		; LDA #$00
		; STA DIVISOR+$1
	; JSR DIV.16				;(dividend/divisor)					
		; ;RETURN VALUE: result+$0 (quotient LO), result+$1 (quotient HO), result+$2 (remainder LO), result+$3 (remainder HO)		
		; LDA RESULT+$0 		;QUOTIENT
		; STA INV_COMBAT.PLAYER.SKILL.DAMAGE 
		; ;LDA RESULT+$1 ;ignored
.CALC.SKILL.DAMAGE.COMPLETE

			; LDX #$AA
			; LDY CHR_SHEET.PC.SKILL.RANGE ;PC range weapon skill
			; ;LDA INV.ITEM_TABLE.WP.DMG_PWR ;add: weapon damage power rating
			; ;LDX INV_COMBAT.PLAYER.SKILL.DAMAGE
			; ;LDX INV_COMBAT.STR_MODIFIER
			; ;LDY CHR_SHEET.PC.DMG.LHAND ;save to left hand damage LO field on character sheet
			; JSR PREP.BRK
			; BRK
			
.CHECK.SKILL.MAX
	LDA INV.ITEM_TABLE.WP.SKILLED_MAX ;weapon damage skill max
	CMP INV_COMBAT.PLAYER.SKILL.DAMAGE
	BCS .CHECK.MIN
	;apply skill maximum damage value
	LDA INV.ITEM_TABLE.WP.SKILLED_MAX ;weapon damage skill max
	STA INV_COMBAT.PLAYER.SKILL.DAMAGE
	JMP .CALC.SKILL.MIN_MAX.COMPLETE

.CHECK.MIN
	LDA INV_COMBAT.PLAYER.SKILL.DAMAGE
	CMP INV.ITEM_TABLE.WP.UNSKILLED_MIN ;weapon damage skill min
	BCS .CALC.SKILL.MIN_MAX.COMPLETE
	;apply skill minimum damage value
	LDA INV.ITEM_TABLE.WP.UNSKILLED_MIN ;weapon damage skill min
	STA INV_COMBAT.PLAYER.SKILL.DAMAGE
.CALC.SKILL.MIN_MAX.COMPLETE

	;**FALLS THROUGH**
	
.EXIT

	
	RTS
@END

INV.COMBAT.TALLY.RESIST.MAGIC
@START
;PARAMETERS: CHR_SHEET.PC_MOB.RESIST_MAGIC, INV.ITEM_TABLE.ALL.RESIST.MAGIC
;ENTRANCE: CALCULATE.COMBAT.STATS.GENERAL_ENTRANCE
;RETURN: updated CHR_SHEET.PC_MOB.RESIST_MAGIC

	LDA CHR_SHEET.PC_MOB.RESIST_MAGIC
	AND #$7F ;mask-out high-bit. Required because the weapon table shares the same field for resist magic and the magic weapon flag
	CLC
	ADC INV.ITEM_TABLE.ALL.RESIST.MAGIC	
	STA	CHR_SHEET.PC_MOB.RESIST_MAGIC

			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; LDA	CHR_SHEET.PC_MOB.RESIST_MAGIC
			; STA $BE00
			; LDA #$AA
			; LDX #FILE.ITEM_TABLE.RECORD.READ
			; LDY /FILE.ITEM_TABLE.RECORD.READ
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP
			
	RTS
@END

INV.TALLY_ITEM_WEIGHT
@START

;SAVE PARAMETERS
	STA TEMP ;save parm: item weight
	
;VALIDATE ENTRANCE (only run in process-all mode)
;(this is because calculating total readied equipment weight would require processing all equipment slots. It isn't necesssary because the ready/unready routines update the total readied equipment weight field for each ready/unready executed)
	LDA INV.CALCULATE.STATS.MODE ;($00 = general mode. only modified equip slots are processed | $01 = levelup mode. Only lhand/rhand and TO-HIT are processed because they use STR/DEX. | $02 = process all mode. Used for character creation)
	CMP #$02
	BNE .EXIT
	
	;TALLY ITEM WEIGHT
	LDA CHR_SHEET.PC.READIED_EQUIPMENT_WEIGHT
	CLC
	ADC TEMP ;item weight
	;overflow check
	BCC .SAVE.ITEM.WEIGHT.TALLY1
	LDA #$FF ;set max weight value
.SAVE.ITEM.WEIGHT.TALLY1
	STA CHR_SHEET.PC.READIED_EQUIPMENT_WEIGHT
	
.EXIT
	
	RTS
@END


@END

@END



;LOCAL SUBROUTINES (CALCULATE.COMBAT.STATS)
@START

;INV_7.INIT.PROCESS_FLAGS
@START
; ;PARAMETERS: ACC (flag setting)
; ;ENTRANCE: direct
; ;RETURN: CHR_SHEET.PC.PROCESS_FLAGS(x)

; ;SAVE PARAMETERS
	; PHA ;push flag setting to stack
	
	; LDX #$00
; .INIT.LOOP
	; PLA ;pull flag setting from stack
	; STA CHR_SHEET.PC.PROCESS_FLAGS,X ;($00 = don't process | $01 process)
	; PHA ;push flag setting to stack
	; INX
	; CPX #CHR_SHEET.PC.PROCESS_FLAGS.SIZE
	; BNE .INIT.LOOP
	; PLA ;pop flag setting from stack

; .EXIT	
	; RTS
@END

INV_7.COPY.ITEM_NAME.LHAND
@START
;PARAMETER: none
;ENTRANCE: direct
;RETURN: CHR_SHEET.PC.NAME.READIED_WP_LHAND or CHR_SHEET.PC.NAME.READIED_WP_RHAND

;SAVE REGISTERS
	TXA
	PHA

	LDX #$00
.LOOP.COPY.ITEM_NAME
	LDA INV.ITEM_TABLE.ALL.NAME,X
	STA CHR_SHEET.PC.NAME.READIED_WP_LHAND,X
	INX
	CPX #INV.ITEM_TABLE.ALL.NAME.MAX_SIZE
	BNE .LOOP.COPY.ITEM_NAME

;RESTORE REGISTERS
	PLA
	TAX	

	RTS
	
@END

INV_7.COPY.ITEM_NAME.RHAND
@START
;PARAMETER: none
;ENTRANCE: direct
;RETURN: CHR_SHEET.PC.NAME.READIED_WP_LHAND or CHR_SHEET.PC.NAME.READIED_WP_RHAND



			
;SAVE REGISTERS
	TXA
	PHA

	LDX #$00
.LOOP.COPY.ITEM_NAME
	LDA INV.ITEM_TABLE.ALL.NAME,X
	STA CHR_SHEET.PC.NAME.READIED_WP_RHAND,X
	INX
	CPX #INV.ITEM_TABLE.ALL.NAME.MAX_SIZE
	BNE .LOOP.COPY.ITEM_NAME


			
;RESTORE REGISTERS
	PLA
	TAX

	
	RTS
	
@END

INV_7.SET_NULL.ITEM_NAME.LHAND
@START
;PARAMETER: none
;ENTRANCE: direct
;RETURN: CHR_SHEET.PC.NAME.READIED_WP_LHAND or CHR_SHEET.PC.NAME.READIED_WP_RHAND
		
;SAVE REGISTERS
	TXA
	PHA

	LDX #$00
.LOOP.COPY.ITEM_NAME
	LDA #$AD ;ASCII "-"
	STA CHR_SHEET.PC.NAME.READIED_WP_LHAND,X
	INX
	CPX #INV.ITEM_TABLE.ALL.NAME.MAX_SIZE-1
	BNE .LOOP.COPY.ITEM_NAME
	;save stop value
	LDA #$00 
	STA CHR_SHEET.PC.NAME.READIED_WP_LHAND,X

			
;RESTORE REGISTERS
	PLA
	TAX

	
	RTS
	

@END

INV_7.SET_NULL.ITEM_NAME.RHAND
@START
;PARAMETER: none
;ENTRANCE: direct
;RETURN: CHR_SHEET.PC.NAME.READIED_WP_LHAND or CHR_SHEET.PC.NAME.READIED_WP_RHAND
		
;SAVE REGISTERS
	TXA
	PHA

	LDX #$00
.LOOP.COPY.ITEM_NAME
	LDA #$AD ;ASCII "-"
	STA CHR_SHEET.PC.NAME.READIED_WP_RHAND,X
	INX
	CPX #INV.ITEM_TABLE.ALL.NAME.MAX_SIZE-1
	BNE .LOOP.COPY.ITEM_NAME
	;save stop value
	LDA #$00 
	STA CHR_SHEET.PC.NAME.READIED_WP_RHAND,X

			
;RESTORE REGISTERS
	PLA
	TAX

	
	RTS
	

@END




@END

;LOCAL.VARIABLES (CALCULATE.COMBAT.STATS) ;**OPT** 
@START
;HP_MAX.TABLE				.DA	20,40,60,80,100,120,140,160,180,200
HP_MAX.TABLE				.DA	50,100,150,200,250,300,350,400,450,500,550,600,650,700,750,800


; CHR_SHEET.PC.PROCESS_FLAGS		.BS DATA.PLY.CHR_SHEET.READIED.SIZE/2		;($00 = don't process | $01 process). ;each equipment slot, including each hand, has it's own flag, 1 byte per flag. The first flag starts at $00
; CHR_SHEET.PC.PROCESS_FLAGS.SIZE	.EQ DATA.PLY.CHR_SHEET.READIED.SIZE/2
; CHR_SHEET.PC.PROCESS_FLAGS.LHAND .BS $1 ;hands setup as dedicated variables because left/right hand have a weapon and an armor process routine. 
; CHR_SHEET.PC.PROCESS_FLAGS.RHAND .BS $1 ;""

;INV.CALCULATE.STATS.MODE		.BS $1 ;($00 = general mode. only modified equip slots are processed | $01 = levelup mode. Only lhand/rhand and TO-HIT are processed because they use STR/DEX. | $02 = process all mode. Used for character creation)

;TEXT BLOCKS

; INV.TEXT_BLOCK.NULL_NAME	.AZ -/---/



@END



;split out any subroutines above into a common subroutine patch (like the items file read) if they will be accessed by other inventory sub-modules
