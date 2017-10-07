; ============================================
; Copyright (C) 2016-2018. 6502 Workshop, LLC
; contact: mark@6502workshop.com
; ============================================



;************************INCLUDE FILE*****************************
;(do not assemble stand-alone.  )


;=====================INCLUDE FILE DOCUMENTATION====================================
;
;Include file to SWAP.ROUTINES.INVENTORY.ASM
;
;=================================================================================
	
	

;INV.TEST.READY_UNREADY
@START
	
	; ;***TEMP: this routine is in a temp location, move it to the entrance(s) to the inventory module. 
	; ;i.e. this needs to be the first thing done so that the ready equipment routine (for example) can modify it's values
		; LDA #$00 ;set flags to "don't process"
	; JSR INV.INIT.PROCESS_FLAGS
	

; ;UNREADY EQUIPMENT DRIVER

; ;***TEMP: read item table record. This data is expected to be in memory 
; ;at the time a ready request is made by the UI
	; ;READ WEAPON TABLE RECORD
		; LDA #$01
		; ;LDA #$03
		; STA PARM.RE.ITEM_ID
		; STA FILE.ITEM_TABLE.ID
		; ;set weapon table mode (ACC = $00 = weapon | $01 armor | $02 misc item | $03 empty hand or in use for 2HD in other hand)
		; LDA #$02
		; ;LDA #$00
		; STA PARM.RE.ITEM_TYPE
	; JSR FILE.READ.ITEM_TABLES.ENTRANCE
		; ;RETURN VALUE: FILE.ITEM_TABLE.RECORD.READ(x)


						
		; ; LDA #$00
		; ; STA PARM.RE.ITEM_TYPE
		; ; LDA #$02
		; ; STA PARM.RE.ITEM_ID
		; LDA #$03
		; STA RE.ACTIVE_PLAYER			
		; ; ;PARM.RE.ITEM_TYPE: already set
		; ; ;PARM.RE.ITEM_ID: already set
		; LDA #$01 ;set unready mode
	; JSR INV.READY_UNREADY.EQUIPMENT.ENTRANCE

	
	
; ; ;TEST: read ready equipment table for active PC
	
			; ;TROUBLESHOOTING LOOP
			; LDX #$00
; .TEST.LOOP1			
			; LDA CHR_SHEET.PC.READIED_EQUIP.RECORD.READ,X
			; STA $BE00,X
			; INX
			; CPX #$60
			; BNE .TEST.LOOP1
			
				; LDA #$BA
				; LDX TEMP
				; ; LDX PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
				; ; LDY PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
				; ; ldx #CHR_SHEET.PC.READIED_EQUIP.RECORD.READ
				; ; ldy /CHR_SHEET.PC.READIED_EQUIP.RECORD.READ
				; JSR PREP.BRK
				; BRK
				
;READY EQUIPMENT DRIVER

; ;***TEMP: read item table record. This data is expected to be in memory 
; ;at the time a ready request is made by the UI

	; ;READ WEAPON TABLE RECORD
		; ;LDA #$04
		; ;LDA #$07
		; LDA #$08
		; STA PARM.RE.ITEM_ID
		; STA FILE.ITEM_TABLE.ID
		; ;set weapon table mode (ACC = $00 = weapon | $01 armor | $02 misc item | $03 empty hand or in use for 2HD in other hand)
		; ;LDA #$01
		; LDA #$00
		; STA PARM.RE.ITEM_TYPE
	; JSR FILE.READ.ITEM_TABLES.ENTRANCE
		; ;RETURN VALUE: FILE.ITEM_TABLE.RECORD.READ(x)

	
		; LDA #$03
		; STA RE.ACTIVE_PLAYER
		; ;PARM.RE.ITEM_TYPE: already set
		; ;PARM.RE.ITEM_ID: already set
		; LDA #$00 ;set ready item mode
	; JSR INV.READY_UNREADY.EQUIPMENT.ENTRANCE		
		; ;RETURN VALUE: ACC = ($00 item readied | $01 = item not readied: item is not readiable | $02 don't use| $03 item not readied: STR gate check failed | $04 item not readied: DEX gate check failed | $05 item not readied: INT gate check failed | $06 can't ready armor during combat | $07 item unreadied | $08 prompt spell hotkey to ready)				

	
; ; ;TEST: read ready equipment table for active PC
			; ; STA TEMP
			
			; ; ;TROUBLESHOOTING LOOP
			; ; LDX #$00
; ; .TEST.LOOP1			
			; ; LDA CHR_SHEET.PC.READIED_EQUIP.RECORD.READ,X
			; ; STA $BE00,X
			; ; INX
			; ; CPX #$60
			; ; BNE .TEST.LOOP1
			
				; ; LDA #$AA
				; ; LDX TEMP
				; ; ; LDX PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
				; ; ; LDY PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
				; ; ; ldx #CHR_SHEET.PC.READIED_EQUIP.RECORD.READ
				; ; ; ldy /CHR_SHEET.PC.READIED_EQUIP.RECORD.READ
				; ; JSR PREP.BRK
				; ; BRK
				

; ;===========================


	; ;**ADD: code to save the readied equipment table to disk
	; ;in INV.INVENTORY_MODULE.EXIT. 
	
	;;JSR INV.INVENTORY_MODULE.EXIT
 

			
	;;RTS
	
@END




INV.READY_UNREADY.EQUIPMENT.ENTRANCE
@START 
;PARAMETERS: ACC ($00 = ready equipment mode | $01 = unready equipment mode), PARM.RE.ITEM_TYPE, PARM.RE.ITEM_ID, RE.ACTIVE_PLAYER*
;ENTRANCE:
;RETURN: updated INV.PLAYER.INVENTORY.DATA, ACC = $00 item readied | $01 = item not readied: item is not readiable | $02 = item not readied: QTY not sufficient | $03 item not readied: STR gate check failed | $04 item not readied: DEX gate check failed | $05 item not readied: INT gate check failed | $06 not used | $07 item unreadied)				
;
;
;*RE.ACTIVE_PLAYER: set to $00 to unready for all players (not tested). 
;

;=====================SUBROUTINE DOCUMENTATION====================================
;
;For unready request this routine assumes that the player has the item readied. For ready requests,
;this routine assumes that the item exists in the player inventory table (i.e it exists in the game) but if the player
;doesn't own a sufficient quantity or any of the gate checks fail, an error code is returned. 
;
;
;==SUMMARY OF OUTCOMES===
;
;READY REQUEST			ALREADY READIED (HANDS)		OUTCOME
;(1) 1-handed weapon	nothing						(1) 1-handed weapon readied
;(1) 1-handed weaponA	(1) 1-handed weaponB		(2) 1-handed weapon readiedAB
;(1) 1-handed weaponA	(2) 1-handed weaponBC		(1) 1-handed weapon readiedA  (both weapons readied before this routine was called become unreadied)
;(1) 1-handed weaponA	(1) 2-handed weapon			(1) 1-handed weapon readied
;(1) 2-handed weapon	(1) or (2) 1-handed weapon	(1) 2-handed weapon
;
;The equipment slots other than hands are straight forward. If the slots is occupied, the conflicting item is unreadied and the item-to-be-readied is then readied to that slot. 				
;
;
;Byte $04 of the INV.PLAYER.INVENTORY.DATA records contains the readied flags 
;which indicate what player(s) have the item readied. 
;
; bit2 = PC #1
; bit3 = PC #2
; bit4 = PC #3
; bit5 = PC #4
; bit6 = PC #5
; bit7 = PC #6
;
;          AND / ORA
; bit2 = $FB / $04           player 1
; bit3 = $F7 / $08           player 2
; bit4 = $EF / $10           player 3
; bit5 = $DF / $20           player 4
; bit6 = $BF / $40           player 5
; bit7 = $7F / $80           player 6
;
;
;Use the ORA mask to set the flag for a specific player. When observing the value of this field in memory, the value will reflect 
;the sum of the ORA mask values for each player who has the item readied. For example, if player 1 and player 3 have the same item
;readied the value of this field will be $14 = $10 + $4. Of course, the number of players that can ready an unique Item_Type/ID is
;limited to the quantity value of the quantity field. 
;
;Use the AND mask to unset the flag for a specific player. Use $00 to unready the item for all players. 
;
;
;;(SAVING IN CASE NEEDED1) = code that record the number of times the active player has a conflicting item readied. It records it to byte $05 of the conflict record.
;I originally thought this might be needed for verifying sufficient QTY of the item is owned before permitting the READY request but then decided it isn't needed, but saved it
;in case I change my mind again. 
;
;
;
;=================================================================================

;*****TEMPLATE*******
		; LDA #$03
		; STA RE.ACTIVE_PLAYER
		; ;PARM.RE.ITEM_TYPE: already set
		; ;PARM.RE.ITEM_ID: already set
		; LDA #$00 ;set ready item mode  (SEC = Ready Mode | CLC = Unready Mode)
	; JSR INV.READY_UNREADY.EQUIPMENT.ENTRANCE		
		; ;RETURN VALUE: ACC = ($00 item readied | $01 = item not readied: item is not readiable | $02 don't use| $03 item not readied: STR gate check failed | $04 item not readied: DEX gate check failed | $05 item not readied: INT gate check failed | $06 can't ready armor during combat | $07 item unreadied | $08 prompt spell hotkey to ready)				


		
;SAVE PARAMETERS
	
	STA READY_EQUIPMENT.MODE ;($00 = ready equipment mode | $01 = unready equipment mode)
	
	
		;****INV DEBUG LOG*** 			;**OPT** Memory. Remove.
@START
.DEBUG.LABEL
		;;PHA
		; LDA #.DEBUG.LABEL
		; STA INV.DEBUG.ADDRESS+$0
		; LDA /.DEBUG.LABEL
		; STA INV.DEBUG.ADDRESS+$1
	JSR INV.DEBUG.LOG.WRITE
		;;PLA
@END

	
; ;DEBUG HOOK	
			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; lda PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
			; sta $be00
			; lda PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
			; sta $be01		
			; lda READY_EQUIPMENT.MODE
			; sta $be02
			; lda PARM.RE.READY.ITEM_TYPE
			; sta $be03
			; lda PARM.RE.READY.ITEM_ID
			; sta $be04
			; lda #$ab
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP

			
;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA


				
				; LDA #$CD
				; JSR COUT
				; JSR KEYIN
				

.INIT
	;inventory pointer
	LDA #INV.PLAYER.INVENTORY.DATA
	STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
	LDA /INV.PLAYER.INVENTORY.DATA
	STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$1

	;read master item table record for item to ready/unready
		LDA PARM.RE.ITEM_ID
		STA FILE.ITEM_TABLE.ID
		LDA PARM.RE.ITEM_TYPE ;set weapon table mode 
		;ACC: (weapon | $01 armor | $02 misc item | $03 empty hand or in use for 2HD in other hand)
	JSR FILE.READ.ITEM_TABLES.ENTRANCE
		;RETURN VALUE: FILE.ITEM_TABLE.RECORD.READ(x)

		
	;read PC character sheet data
		LDA RE.ACTIVE_PLAYER ;ACC = player sequential # (high-bit not set = read mode)
	JSR COMBAT.READ_WRITE.CHR_SHEET.PC
		;RETURN VALUE = CHR_SHEET.RECORD.READ

		
;--DATAGRAM--
;INV.RE.CONFLICT_RECORD1 	($00 = conflicting inventory record LO byte | $01 = conflicting inventory record HO byte | $02 # of times active player has readied item conflicting record is for)
;INV.RE.CONFLICT_RECORD1 	""
.INIT.UNREADY.CONFLICTS
	LDX #$00 ;loop counter
	;convenient place for init of the following
	STX PARM.RE.UNREADY.CLEAR_FLAG	;($00 = clear 1st readied flags field only | $01 clear both readied flags field)
	STX INV.UNREADY.ITERATION_COUNTER
	
	LDA #$FF ;init value
.LOOP.INIT.UNREADY.CONFLICTS
	STA INV.RE.CONFLICT_RECORD1,X
	INX
	CPX #INV.RE.CONFLICT.TABLE.SIZE
	BNE .LOOP.INIT.UNREADY.CONFLICTS
	
			
;READ READIED EQUIPMENT TABLE RECORD (for active player)
	;***WARNING: Assumes file is open. add file-open check to this routine
		LDA RE.ACTIVE_PLAYER ;ACC = player sequential # (high-bit not set = read mode)
	JSR INV.READ_WRITE_RECORD.CHAR_SHEET.READIED
		;RETURN VALUE: CHR_SHEET.PC.READIED_EQUIP.RECORD.READ($10)

		
.SELECT_MODE
;branch to ready or unready routine

			

	LDA READY_EQUIPMENT.MODE ;($00 = ready equipment mode | $01 = unready equipment mode)
		;PARM.RE.READY.ITEM_TYPE: already set. It is .EQ PARM.RE.ITEM_TYPE
		;PARM.RE.READY.ITEM_ID: already set. It is .EQ PARM.RE.ITEM_TYPE
	BEQ INV.READY_EQUIPMENT
		LDA PARM.RE.ITEM_TYPE
		LDX	PARM.RE.ITEM_ID
	JMP INV.UNREADY_EQUIPMENT
	
INV.READY_EQUIPMENT
@START
;PARAMETERS: PARM.RE.READY.ITEM_TYPE, PARM.RE.READY.ITEM_ID, PARM.RE.ACTIVE_PLAYER, PLAYER_MERCH.INVENTORY.DATA.POINTER(2)
;ENTRANCE: direct
;RETURN: updated CHR_SHEET.PC.READIED_EQUIP and INV.PLAYER.INVENTORY.DATA

	
;=====================CODE-SECTION DOCUMENTATION====================================
;
;		;Note: No need to verify player owns the item
;		;	There is no need to search the player inventory to verify that an item requested to be readied exists because only items on the onscreen list can be requested to be readied
;		;	and the onscreen list is drawn from the inventory table)
;			;
;		;CHECK IF ITEM IS READIABLE
;			;if not, set return value $02. 
;			;RETURN VALUE: ACC = ($00 item readied | $01 = item not readied: item is not readiable | $02 don't use| $03 item not readied: STR gate check failed | $04 item not readied: DEX gate check failed | $05 item not readied: INT gate check failed | $06 can't ready armor during combat | $07 item unreadied | $08 prompt spell hotkey to ready)				
;		
;		;UNREADY ANY CONFLICTS
;			;init both conflict records to $FF (datagram: LO/HO byte of inventory record that conflicts)
;			;IF item table equipment slot field = hands goto HANDS
;			;NOT HANDS
;			;(record parse loop) IF item table equipment slot field = inventory table equipment slot field AND 1st readied flags field = active player THEN 
;				;save record pointer to conflict record #1. goto EXECUTE UNREADY
;			;HANDS
;			;count # of items active player has readied to hands
;					(record parse loop) IF inventory table equipmet slot = hands, increment in-record hands counter and in-table hands counter for each readied flags field with active player. Do two increments to the in-table hands counter if the conflicting item is 2-handed per the inventory table equipment slot;												;if either readied flags field has active player set then save record to conflict record #1 (or #2 if #1 is already set), and set byte 3 of the conflict record with the number of times the item is readied as per the two readied flag fields											
;												;IF in-record hands counter = 2 (indicates dual wielding of the same item) then copy conflict record 1 to conflict record 2 so that the unready routine is called twice for this time.
;												;error if both conflict records are already set. 
;			;
;			;CHECK GATES
;				;STR
;					;calculate ADJ Total Weight (adjusted total readied equipment weight)
;						;for each conflict record, get the item weight (item table) * # of times player has that item readied (readied flag fields).  
;						;subtract from EQUIP_WEIGHT (char sheet) to get ADJ Total Weight
;					;if STR field (char sheet) >= ADJ Total Weight + STR field (items table) THEN gate check = pass. goto PROCESS.CONFLICT.RECORDS 
;				;DEX
;					;if DEX field (char sheet) >= DEX field (items table) THEN gate check = pass. goto PROCESS.CONFLICT.RECORDS 
;				;INT
;					;if INT field (char sheet) >= INT field (items table) THEN gate check = pass. goto PROCESS.CONFLICT.RECORDS
;			
;			;		
;			;PROCESS.CONFLICT.RECORDS
;			;IF item table "hands required & Shape_ID" field = 1-handed AND hands counter > 2 THEN goto READY ITEM
;			;IF item table "hands required & Shape_ID" field = 2-handed THEN fall through
;			;IF item table "hands required & Shape_ID" field = 1-handed AND hands counter >= 2 THEN fall through
;
;				;if item table "hands required & Shape_ID" field = 2-handed then set parm (PARM.UNREADY.CLEAR_FLAG) to clear both readied flag fields (just in case the player has two single handed weapons readied with the same Item_Type/ID)
;				;call .UNREADY_EQUIPMENT for any conflict records
;		;		
;		;	
;		;EXECUTE READY ITEM
;			;Use INV.SEARCH.PLY_INV to find the inventory record for the item
;			;set the readied flag for the player on the item's inventory record. If 1st readied flag is readied for active player then set the 2nd readied flag for active player
;			;Use active player # and the equipment slot value in the item table to set the item to readied the readied equipment table.  
;					;if hands: if left hand Item_Type != $03 then ready item to right hand. If right hand item_type also != $03 then error. 
;							;note: if a shield is readied, that will result in an Item_Type of $01 readied to a hand but that's fine. in CALCUATE.STATS, any Item_Type value in hands other than $00 (weapon) results in the shape ID being set to $01 which COMMAND.ATTACK treats to mean "skip that hand"
;			;RETURN VALUE: ACC = ($00 item readied | $01 = item not readied: item is not readiable | $02 don't use| $03 item not readied: STR gate check failed | $04 item not readied: DEX gate check failed | $05 item not readied: INT gate check failed | $06 can't ready armor during combat | $07 item unreadied | $08 prompt spell hotkey to ready)				
;						
;		
;
;
;=================================================================================


;****INV DEBUG LOG*** 			;**OPT** Memory. Remove.
@START
.DEBUG.LABEL
		;; PHA
		; LDA #.DEBUG.LABEL
		; STA INV.DEBUG.ADDRESS+$0
		; LDA /.DEBUG.LABEL
		; STA INV.DEBUG.ADDRESS+$1
	JSR INV.DEBUG.LOG.WRITE
		;; PLA
@END


.SPELL_MENU.READY
@START		
;=====================CODE-SECTION DOCUMENTATION==========================================================================
;
;The ready and unready scheme for spells is much different than for other items. 
;spells don't use the readied equipment table and readied status isn't stored in the
;PLAYER.INVENTORY.DATA readied flag fields. 
;
;Spells are really pretty simple. If a spell item is readied, it's spell code is stored as value in
;the PLAYER.HOTKEYS.SPELLS array. If a spell hotkey has the high-bit set then no spell is readied to it. 
;
;
;
;===========================================================================================================================

	
;VALIDATE ENTRANCE
	LDA INV.ACTIVE_MENU.CODE 
	CMP #INV.ACTIVE_MENU.SPELLS ;INV_4.1b.DISPLAY.MODE = $02 (misc items), but is spell menu active?
	BNE	.SPELL_MENU.READY.DONE

	LDY INV_4.SPELL_READY.HOTKEY_SELECTED	
	LDA INV_4.SPELL_READY.SPELL_CODE
	STA	PLAYER.HOTKEYS.SPELLS,Y
	
		; STA $BE00
		; STY $BE01
		; LDA #$AA
		; LDX #PLAYER.HOTKEYS.SPELLS
		; LDY /PLAYER.HOTKEYS.SPELLS
		; JSR PREP.BRK
		; BRK
	
	JMP .RETURN.ITEM_READIED
.SPELL_MENU.READY.DONE	
@END
			
.CHECK.READIABLE
@START
;CHECK IF ITEM IS READIABLE
	;if not, set return value $02. 
	;RETURN VALUE: ACC = ($00 item readied | $01 = item not readied: item is not readiable | $02 don't use| $03 item not readied: STR gate check failed | $04 item not readied: DEX gate check failed | $05 item not readied: INT gate check failed | $06 can't ready armor during combat | $07 item unreadied | $08 prompt spell hotkey to ready)				
	
	LDA INV.ITEM_TABLE.ALL.EQUIPMENT_SLOT
	CMP #INV.RE.SLOT.UNREADIABLE_ITEM
	BNE .CHECK.READIABLE.DONE
	
.RETURN.ITEM_NOT_READIABLE
;RETURN VALUE: ACC = ($00 item readied | $01 = item not readied: item is not readiable | $02 don't use| $03 item not readied: STR gate check failed | $04 item not readied: DEX gate check failed | $05 item not readied: INT gate check failed | $06 can't ready armor during combat | $07 item unreadied | $08 prompt spell hotkey to ready)				
	LDA #$01
	JMP .RETURN_VALUE.SET
	
.CHECK.READIABLE.DONE
@END

.IDENTIFY.CONFLICTING.ITEMS
@START
;--DATAGRAM--
;INV.RE.CONFLICT_RECORD1 	($00 = conflicting inventory record LO byte | $01 = conflicting inventory record HO byte | $02 # of times active player has readied item conflicting record is for)
;INV.RE.CONFLICT_RECORD1 	""
;These arrays are init in the init section of the top level routine (INV.READY_UNREADY.EQUIPMENT.ENTRANCE)



;BRANCH: HANDS/NOT_HANDS
;(branch based on whether the item to unready uses the "hands" equipment slot)	
	LDA INV.ITEM_TABLE.ALL.EQUIPMENT_SLOT
	CMP #INV.RE.SLOT.HANDS
	BEQ .ITEM.USES.HANDS	
.ITEM.USES.NOT_HANDS
		;PLAYER_MERCH.INVENTORY.DATA.POINTER(2): already set
		;PARM.RE.ITEM_TYPE: already set
		;PARM.RE.ITEM_ID: already set
	JSR INV.SEARCH.PLY_INV.CONFLICTS.NOT_HANDS
		;RETURN VALUE: INV.RE.CONFLICT_RECORD1(3)
	JMP .IDENTIFY.CONFLICTING.ITEMS.DONE
	
.ITEM.USES.HANDS
;HANDS
;	count # of items active player has readied to hands
;			;(record parse loop) IF inventory table equipmet slot = hands, increment hands counter for each readied flags field with active player. Do two increments to the hands counter if the conflicting item is 2-handed per the inventory table equipment slot;			;if either readied flags field has active player set then save record to conflict record #1 (or #2 if #1 is already set), and set byte 3 of the conflict record with the number of times the item is readied as per the two readied flag fields											
;			;error if both conflict records are already set. 
;

	
				
		;PLAYER_MERCH.INVENTORY.DATA.POINTER(2): already set
		;PARM.RE.READY.ITEM_TYPE: already set
		;PARM.RE.READY.ITEM_ID: already set
	JSR INV.SEARCH.PLY_INV.CONFLICTS.HANDS
		;RETURN VALUE: INV.RE.CONFLICT_RECORD1(3), [INV.RE.CONFLICT_RECORD2(3)], PLAYER_MERCH.INVENTORY.DATA.POINTER(2)*, IN.RE.ITEMS_READIED_IN_HANDS.TOTAL
	
.IDENTIFY.CONFLICTING.ITEMS.DONE
@END

			
.CHECK.GATES
@START
;=====================CODE-SECTION DOCUMENTATION====================================
;CHECK GATES
;	STR
;		calculate ADJ Total Weight (adjusted total readied equipment weight)
;			for each conflict record, get the item weight (item table) * # of times player has that item readied (readied flag fields).  
;			subtract from EQUIP_WEIGHT (char sheet) to get ADJ Total Weight
;		if STR field (char sheet) >= ADJ Total Weight + STR field (items table) THEN gate check = pass. goto PROCESS.CONFLICT.RECORDS 
;	DEX
;		if DEX field (char sheet) >= DEX field (items table) THEN gate check = pass. goto PROCESS.CONFLICT.RECORDS 
;	INT
;		if INT field (char sheet) >= INT field (items table) THEN gate check = pass. goto PROCESS.CONFLICT.RECORDS
;
;=================================================================================

				; lda #$01
				; sta troubleshooting.hook
				
.INIT.GATES					
	LDA #$00
	STA INV.GATE_CHECK.WEIGHT.TALLY 
	STA INV.GATE_CHECK.ADJ_TOTAL_WEIGHT
	
.GATE.STR
@START

;	STR
;		calculate ADJ Total Weight (adjusted total readied equipment weight)
;			for each conflict record, get the item weight (item table) * # of times player has that item readied (readied flag fields).  
;			subtract from EQUIP_WEIGHT (char sheet) to get ADJ Total Weight
;		if STR field (char sheet) >= ADJ Total Weight + STR field (items table) THEN gate check = pass. goto PROCESS.CONFLICT.RECORDS 

.CALCULATE.EQUIP_WEIGHT.NET_OF_UNREADY
@START

				
.VALIDATE.ENTRY
;PERMIT DUAL WIELDING
.BRANCH.HANDS 
;BRANCH: HANDS/NOT_HANDS
;(branch based on whether the item-to-be-readied uses the "hands" equipment slot)
;(if slot is not hands, then processing can start. If slot is hands additional checks need to be
;done because of the potential for dual wielding)
	LDA INV.ITEM_TABLE.ALL.EQUIPMENT_SLOT
	CMP #INV.RE.SLOT.HANDS
	BNE .CALCULATE.START ;branch if not hands slot
	
	LDA INV.ITEM_TABLE.WP.SHAPE_ID_HANDS ;load item-to-readied's item_table field that contains the 1-2handed status. We can assume the item is a weapon because only weapon's use the "hands" equipment slot, which this routine is for. 
	STA INV.ITEM_TABLE.WP.SHAPE_ID_HANDS.SAVED ;save INV.ITEM_TABLE.WP.SHAPE_ID_HANDS (item-to-readied) to stack
	BMI .CALCULATE.START ;branch if item-to-readied is 2-handed because in that event all conflicting items will be unreadied and their weight should be counted in the tally so it's excluded from the gate calculation.
	LDA IN.RE.ITEMS_READIED_IN_HANDS.TOTAL
	CMP #$02		
	BCC .TALLY.WEIGHT.DONE ;branch if hands counter > 2 (this means that the player is trying to ready a single handed weapon and doesn't have two single handed weapons readied)
	; BCS .COW 
	; JMP .TALLY.WEIGHT.DONE ;branch if hands counter > 2 (this means that the player is trying to ready a single handed weapon and doesn't have two single handed weapons readied)
; .COW

.CALCULATE.START
	;save current item_table record
	LDX #$00
.LOOP.SAVE
	LDA FILE.ITEM_TABLE.RECORD.READ,X
	STA INV.SAVED.ITEM_TABLE.RECORD,X
	INX
	CPX #FILE.ITEM_TABLE.RECORD.SIZE
	BNE .LOOP.SAVE


				
				
.BRANCH.CONFLICT_RECORD.CHECK	
;BRANCH: which conflicts records, if any, are in-use?			
	LDA INV.RE.CONFLICT_RECORD1+$2
	BPL .TALLY.WEIGHT.OF.RECORD1
	LDA INV.RE.CONFLICT_RECORD2+$2
	BMI .NO_CONFLICTS ;branch if both conflict records are empty
	JMP .TALLY.WEIGHT.OF.RECORD2

.NO_CONFLICTS ;if there are not conflicting items to unready, the equiment weight total on the PC character sheet is correct for the purposes of the STR gate check
;ADJ Total Weight =  EQUIP_WEIGHT (char sheet)
	LDA CHR_SHEET.PC.READIED_EQUIPMENT_WEIGHT ;load weight of active player's readied equipment
	JMP .SAVE.ADJ.TOTAL_WEIGHT

.OVERFLOW_ERROR
;.CALCULATE.EQUIP_WEIGHT.NET_OF_UNREADY reports 8-bit addition overflow. This
;means that the sum of weight values of the items to unready due to conflicts is 
;>!256, which it shouldn't be. 
	JSR PREP.BRK
	BRK



	
.TALLY.WEIGHT.OF.RECORD1	
@START
	;set pointer to conflict record #1
	LDA INV.RE.CONFLICT_RECORD1+$0 ;load LO byte of inventory record address
	STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
	LDA INV.RE.CONFLICT_RECORD1+$1 ;load HO byte of inventory record address
	STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$1

	LDY #$01 ;set index Item_ID field
	LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y
	STA FILE.ITEM_TABLE.ID
	DEY ;backup to to LEVEL | ITEM_TYPE field 
	LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y
	;ITEM_TYPE is in LO nibble
	AND #$0F ;mask-out HO nibble. 
		;ACC = item table code ($00 = weapon | $01 armor | $02 misc item)
	JSR FILE.READ.ITEM_TABLES.ENTRANCE
		;RETURN: FILE.ITEM_TABLE.RECORD.READ($20)
	
	LDA INV.GATE_CHECK.WEIGHT.TALLY 		
	CLC
	ADC INV.ITEM_TABLE.ALL.STR_WEIGHT
	STA INV.GATE_CHECK.WEIGHT.TALLY  
	BCS .OVERFLOW_ERROR

;BRANCH: is conflict record #2 in-use?
	LDA INV.RE.CONFLICT_RECORD2+$2
	BPL .TALLY.WEIGHT.OF.RECORD2 ;**ADD: this means both conflict records are empty. ADJ Total Weight =  EQUIP_WEIGHT (char sheet)
	JMP .TALLY.WEIGHT.DONE
@END

.TALLY.WEIGHT.OF.RECORD2 
@START
	;set pointer to conflit record #1
	LDA INV.RE.CONFLICT_RECORD2+$0 ;load LO byte of inventory record address
	STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
	LDA INV.RE.CONFLICT_RECORD2+$1 ;load HO byte of inventory record address
	STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$1

	;**OPT** memory. After pointer is set above, jump back to .TALLY.WEIGHT.OF.RECORD1 and eliminate the 
	;code below. Would need to set an interation counter for the code above so that it jumps over
	;this code. 
	
	LDY #$01 ;set index Item_ID field
	LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y
	STA FILE.ITEM_TABLE.ID
	DEY ;backup to to LEVEL | ITEM_TYPE field 
	LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y
	;ITEM_TYPE is in LO nibble
	AND #$0F ;mask-out HO nibble. 
		;ACC = item table code ($00 = weapon | $01 armor | $02 misc item)
	JSR FILE.READ.ITEM_TABLES.ENTRANCE
		;RETURN: FILE.ITEM_TABLE.RECORD.READ($20)
		
	LDA INV.GATE_CHECK.WEIGHT.TALLY 		
	CLC
	ADC INV.ITEM_TABLE.ALL.STR_WEIGHT
	STA INV.GATE_CHECK.WEIGHT.TALLY  
	BCS .OVERFLOW_ERROR
	;**FALLS THROUGH**
	
@END
	;**FALLS THROUGH**
.TALLY.WEIGHT.DONE


			
;FINALIZE ADJ WEIGHT CALCULATION
	LDA CHR_SHEET.PC.READIED_EQUIPMENT_WEIGHT ;load weight of active player's readied equipment
	SEC
	SBC INV.GATE_CHECK.WEIGHT.TALLY ;subtract the weight of the equipment that would be unreadied due to equipment slot conflicts with the ready request

.SAVE.ADJ.TOTAL_WEIGHT
	STA INV.GATE_CHECK.ADJ_TOTAL_WEIGHT 

				
.RESTORE.ITEM.RECORD
	;verify item record was previously saved
	;(the save doesn't occur if the item-to-ready uses the hands slot and there are not 2 conflicting items already readied)
	LDA INV.ITEM_TABLE.WP.SHAPE_ID_HANDS.SAVED ;restore INV.ITEM_TABLE.WP.SHAPE_ID_HANDS (item-to-readied) to stack. This stack pull is needed because the item-to-ready is not currently in the item table read buffer

	BMI .LOOP.RESTORE.ENTRANCE ;branch if item-to-readied is 2-handed because record was previous saved in that event. 
	LDA IN.RE.ITEMS_READIED_IN_HANDS.TOTAL
	CMP #$02		
	BCC .LOOP.RESTORE.DONE ;branch if hands counter > 2 (this means that the player is trying to ready a single handed weapon and doesn't have two single handed weapons readied)

.LOOP.RESTORE.ENTRANCE	
	;restore original item_table record
	LDX #$00
.LOOP.RESTORE
	LDA INV.SAVED.ITEM_TABLE.RECORD,X
	STA FILE.ITEM_TABLE.RECORD.READ,X
	INX
	CPX #FILE.ITEM_TABLE.RECORD.SIZE
	BNE .LOOP.RESTORE
.LOOP.RESTORE.DONE	
	


@END

.EXECUTE.STR.GATE_CHECK	

						
	;is {ADJ Total Weight) + STR field (items table)} <= STR field (char sheet)?
	LDA INV.GATE_CHECK.ADJ_TOTAL_WEIGHT ;weight of players readied items, if the conflicting items were unreadied
	CLC
	ADC INV.ITEM_TABLE.ALL.STR_WEIGHT ;item-to-ready
	CMP CHR_SHEET.PC.ATTRIB.STR
	BCC .GATES.STR.DONE ;branch if yes
	BEQ .GATES.STR.DONE ;branch if yes
	;set return value
	LDA #$03
	JMP .RETURN_VALUE.SET	;RETURN VALUE: ACC = ($00 item readied | $01 = item not readied: item is not readiable | $02 don't use| $03 item not readied: STR gate check failed | $04 item not readied: DEX gate check failed | $05 item not readied: INT gate check failed | $06 can't ready armor during combat | $07 item unreadied | $08 prompt spell hotkey to ready)				
.GATES.STR.DONE


; ;========TROUBLESHOOTING HOOK============
				; LDA CHR_SHEET.PC.READIED_EQUIPMENT_WEIGHT ;load weight of active player's readied equipment
				; sta $be00
				; lda INV.GATE_CHECK.WEIGHT.TALLY 
				; sta $be01
				; LDA INV.GATE_CHECK.ADJ_TOTAL_WEIGHT
				; sta $be02
				; LDA INV.ITEM_TABLE.ALL.STR_WEIGHT ;item-to-ready
				; sta $be03
				; LDA CHR_SHEET.PC.ATTRIB.STR
				; sta $be04
				; LDA #$AB
				; ; ; LDX PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
				; ; ; LDY PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
				; ; ; ldx #CHR_SHEET.PC.READIED_EQUIP.RECORD.READ
				; ; ; ldy /CHR_SHEET.PC.READIED_EQUIP.RECORD.READ
				; LDX #INV.RE.CONFLICT_RECORD1
				; LDY /INV.RE.CONFLICT_RECORD1
				; JSR PREP.BRK
				; BRK	
				
				
				
@END


			
.GATE.DEX
	;is DEX field (char sheet) >= DEX field (items table)?
	LDA CHR_SHEET.PC.ATTRIB.DEX
	CMP INV.ITEM_TABLE.ALL.DEX
	BCS .GATES.DEX.DONE ;branch if yes
	;set return value
	LDA #$04
	JMP .RETURN_VALUE.SET	;RETURN VALUE: ACC = ($00 item readied | $01 = item not readied: item is not readiable | $02 don't use| $03 item not readied: STR gate check failed | $04 item not readied: DEX gate check failed | $05 item not readied: INT gate check failed | $06 can't ready armor during combat | $07 item unreadied | $08 prompt spell hotkey to ready)				
.GATES.DEX.DONE

.GATE.INT
	;is INT field (char sheet) >= INT field (items table)?
	LDA CHR_SHEET.PC.ATTRIB.INT
	CMP INV.ITEM_TABLE.ALL.INT
	BCS .GATES.INT.DONE ;branch if yes
	;set return value
	LDA #$05
	JMP .RETURN_VALUE.SET	;RETURN VALUE: ACC = ($00 item readied | $01 = item not readied: item is not readiable | $02 don't use| $03 item not readied: STR gate check failed | $04 item not readied: DEX gate check failed | $05 item not readied: INT gate check failed | $06 can't ready armor during combat | $07 item unreadied | $08 prompt spell hotkey to ready)				
.GATES.INT.DONE
	
@END



.CHECK.QTY
@START
.CHECK.QTY.INIT

;GET POINTER TO INVENTORY RECORD OF ITEM-TO-READY
		;reset pointer to start of array
		LDA #INV.PLAYER.INVENTORY.DATA
		STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
		LDA /INV.PLAYER.INVENTORY.DATA
		STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
		
		;PARM.RE.ITEM_TYPE: already set
		;PARM.RE.ITEM_ID: already set
		LDA PARM.RE.READY.ITEM_TYPE
		LDX PARM.RE.READY.ITEM_ID	
	JSR INV.SEARCH.PLY_INV
		;RETURN VALUE: PLAYER_MERCH.INVENTORY.DATA.POINTER(2)=(target inventory record)
	
		
;GET # OF READIED INSTANCES		
	JSR INV.GET.READIED.QTY
		;RETURN VALUE: INV.RE.READIED.QTY


				
;DO THE CHECK		
	LDA INV.RE.READIED.QTY ;load the number of times the ITEM-TO-READY is already readied by all PCs, including active PC
	LDY #$02 ;set index to QTY field of inventory record of ITEM-TO-READY
	CMP (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y ;compare to: QTY owned of ITEM-TO-READY


				
	BCC .CHECK.QTY.DONE		;branch if the player owns more of the item than is already readied	
	
	;**FALLS THROUGH**
	
.RETURN.ITEM_QTY_INSUFFICIENT
;RETURN VALUE: ACC = ($00 item readied | $01 = item not readied: item is not readiable | $02 don't use| $03 item not readied: STR gate check failed | $04 item not readied: DEX gate check failed | $05 item not readied: INT gate check failed | $06 can't ready armor during combat | $07 item unreadied | $08 prompt spell hotkey to ready)				
	LDA #$02
	JMP .RETURN_VALUE.SET

.CHECK.QTY.DONE


;==============TROUBLESHOOTING HOOK=======
				; LDA INV.RE.READIED.QTY ;load the number of times the ITEM-TO-READY is already readied by all PCs, including active PC
				; sta $be00
				; LDY #$02 ;set index to QTY field of inventory record of ITEM-TO-READY
				; LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y ;compare to: QTY owned of ITEM-TO-READY
				; sta $be01

				; LDA #$AB
				; ; ; LDX PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
				; ; ; LDY PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
				; ; ; ldx #CHR_SHEET.PC.READIED_EQUIP.RECORD.READ
				; ; ; ldy /CHR_SHEET.PC.READIED_EQUIP.RECORD.READ
				; LDX #INV.RE.CONFLICT_RECORD1
				; LDY /INV.RE.CONFLICT_RECORD1
				; JSR PREP.BRK
				; BRK	
@END





			
		; LDA #$B0
		; JSR COUT
		; LDA #$8D
		; JSR COUT
		
				; LDA #$AB
				; ; ; LDX PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
				; ; ; LDY PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
				; ; ; ldx #CHR_SHEET.PC.READIED_EQUIP.RECORD.READ
				; ; ; ldy /CHR_SHEET.PC.READIED_EQUIP.RECORD.READ
				; LDX #INV.RE.CONFLICT_RECORD1
				; LDY /INV.RE.CONFLICT_RECORD1
				; JSR PREP.BRK
				; BRK

			
					; LDA #$01
					; STA TROUBLESHOOTING.HOOK


			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP




			
.PROCESS.CONFLICT.RECORDS
@START
;=====================CODE-SECTION DOCUMENTATION====================================
;
; PROCESS.CONFLICT.RECORDS
; IF item table "hands required & Shape_ID" field = 1-handed AND hands counter > 2 THEN goto READY ITEM
; IF item table "hands required & Shape_ID" field = 2-handed THEN fall through
; IF item table "hands required & Shape_ID" field = 1-handed AND hands counter >= 2 THEN fall through
;
;	if item table "hands required & Shape_ID" field = 2-handed then set parm (PARM.UNREADY.CLEAR_FLAG) to clear both readied flag fields (just in case the player has two single handed weapons readied with the same Item_Type/ID)
;	call .UNREADY_EQUIPMENT for any conflict records
;=================================================================================

.PROCESS.INIT
	;save pointer for use in .EXECUTE.READY because this routine needs to change the pointer value
	LDA PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
	STA PLAYER_MERCH.INVENTORY.DATA.POINTER.SAVED+$0
	LDA PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
	STA PLAYER_MERCH.INVENTORY.DATA.POINTER.SAVED+$1

			
.BRANCH  
;BRANCH: HANDS/NOT_HANDS
;(branch based on whether the item-to-be-readied uses the "hands" equipment slot)
;(if slot is not hands, then processing can start. If slot is hands additional checks need to be
;done because of the potential for dual wielding and 2-handed weapons)
	LDA INV.ITEM_TABLE.ALL.EQUIPMENT_SLOT
	CMP #INV.RE.SLOT.HANDS
	BNE .PROCESS.START ;branch if not hands slot


;.ITEM_TO_BE_READIED.USES.HANDS	
	LDA INV.ITEM_TABLE.WP.SHAPE_ID_HANDS ;load item-to-be-readied's item_table field that contains the 1-2handed status. We can assume the item is a weapon because only weapon's use the "hands" equipment slot, which this routine is for. 
	BMI .PROCESS.START.TWO_HANDED.ENTRANCE ;branch if 2-handed
	LDA IN.RE.ITEMS_READIED_IN_HANDS.TOTAL
	CMP #$02		
	BCC .EXECUTE.READY ;branch if hands counter > 2 (this means that the player is trying to ready a single handed weapon and doesn't have two single handed weapons readied)
	JMP .PROCESS.START
	
.PROCESS.START.TWO_HANDED.ENTRANCE

					; LDA #$AA
					; JSR PREP.BRK
					; BRK
					
	LDA #$01 ;since item-to-ready is two handed, both readied flags need to be cleared as both hands must be free
	STA PARM.RE.UNREADY.CLEAR_FLAG	 ;($00 = clear 1st readied flags field only | $01 clear both readied flags field)

.PROCESS.START
;(process conflict records)	
.CONFLICT.RECORD1

		; LDA #$B1
		; JSR COUT
		; LDA #$8D
		; JSR COUT
		; JSR KEYIN
		
		
	LDA INV.RE.CONFLICT_RECORD1+$2
	BMI .CONFLICT.RECORD2 ;if record still has init values, then next record
	LDA INV.RE.CONFLICT_RECORD1+$0
	STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
	LDA INV.RE.CONFLICT_RECORD1+$1
	STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$1	
	
		LDA INV.RE.CONFLICT_RECORD1+$3 ;item_type of conflict record
		LDX INV.RE.CONFLICT_RECORD1+$4 ;item_id  of conflict record
	JSR INV.UNREADY_EQUIPMENT
	
.CONFLICT.RECORD2
	LDA INV.RE.CONFLICT_RECORD2+$2
	BMI .PROCESS.CONFLICT.RECORDS.DONE	;if record still has init values, then no records to process	
	LDA INV.RE.CONFLICT_RECORD2+$0
	STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
	LDA INV.RE.CONFLICT_RECORD2+$1
	STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
		LDA INV.RE.CONFLICT_RECORD2+$3 ;item_type of conflict record
		LDX INV.RE.CONFLICT_RECORD2+$4 ;item_id  of conflict record
	JSR INV.UNREADY_EQUIPMENT



.PROCESS.CONFLICT.RECORDS.DONE	
@END

		; LDA #$B2
		; JSR COUT
		; LDA #$8D
		; JSR COUT
		
.EXECUTE.READY
@START
;=====================CODE-SECTION DOCUMENTATION====================================
;EXECUTE READY ITEM
;	Use INV.SEARCH.PLY_INV to find the inventory record for the item
;	set the readied flag for the player on the item's inventory record. If 1st readied flag is readied for active player then set the 2nd readied flag for active player
;	Use active player # and the equipment slot value in the item table to set the item to readied the readied equipment table.  
;			if hands: if left hand Item_Type != $03 then ready item to right hand. If right hand item_type also != $03 then error. 
;					note: if a shield is readied, that will result in an Item_Type of $01 readied to a hand but that's fine. in CALCUATE.STATS, any Item_Type value in hands other than $00 (weapon) results in the shape ID being set to $01 which COMMAND.ATTACK treats to mean "skip that hand"
;
;=================================================================================



				
.INIT.EXECUTE.READY
	;set run flag so that INV.INVENTORY_MODULE.EXIT will know that equiment slots changed, triggering it to call CALCULATE.COMBAT.STATS to recalculate stats derived from what equipment is readied. 
	LDA #$01 ;
	STA INV.READY_UNREADY.RUN_FLAG	;($00 = ready_unready not called | $01 ready_unready called. ready_unready refers to INV.READY_UNREADY.EQUIPMENT.ENTRANCE)

	
	;add weight of item-to-ready
	LDA CHR_SHEET.PC.READIED_EQUIPMENT_WEIGHT ;load weight of active player's readied equipment
	CLC
	ADC INV.ITEM_TABLE.ALL.STR_WEIGHT
	STA CHR_SHEET.PC.READIED_EQUIPMENT_WEIGHT ;load weight of active player's readied equipment

	
	;**OPT** Memory. It might be possible to eliminate the next 4 opcodes, and just use PLAYER_MERCH.INVENTORY.DATA.POINTER.SAVED as a zpage pointer in the code below.
					 ;the wrinkle is if the code below in the rest of this routine calls shared subroutines which expect the pointer to be in PLAYER_MERCH.INVENTORY.DATA.POINTER
	LDA PLAYER_MERCH.INVENTORY.DATA.POINTER.SAVED+$0
	STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
	LDA PLAYER_MERCH.INVENTORY.DATA.POINTER.SAVED+$1
	STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
		
	LDY #$03 ;set index to record byte #$03 (1st Readied Flags Field)		
	LDX RE.ACTIVE_PLAYER ;load index to INV.READIED_FLAGS.AND_MASK	
.SET.READIED_FLAGS
	LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y ;load Readied Equipment Flags #1
	PHA ;push Readied Flag value to stack
	AND INV.READIED_FLAGS.BIT_TEST_MASK,X ;test bit flag for active player
	BNE .USE.READIED.FLAGS.FIELD2 ;branch if active player already has item readied via this field (i.e if the player is trying to ready a 2nd single handed weapon of the same type)
	PLA ;pull Readied Flag value from stack
	ORA INV.READIED_FLAGS.ORA_MASK,X ;mask-in bit flag for active player (this readies the item)
	STA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y ;save Readied Equipment Flags #1
	JMP .SET.READIED_FLAGS.DONE
	
.ERROR.READY_OVERFLOW
;.EXECUTE.READY reports that both Readied Flag Fields are set for the active player.
;That means both hands are in use already so another item requiring a "hands" equipment slot cannot be readied. 
;This means something went wrong in .IDENTIFY.CONFLICTING.ITEMS, .PROCESS.CONFLICT.RECORDS, or INV.UNREADY_EQUIPMENT which
;handle unreadying items that conflict with the item-to-be-readied. 
	JSR PREP.BRK
	BRK

.USE.READIED.FLAGS.FIELD2
;increment index then loop
	PLA ;pop Readied Flag value from stack
	CPY #$04 ;if Y-REG is already #$04 (the 2nd Readied Flags field), that means this routine is being run for a 2nd time, which means both Readied Flag Fields are set for the active player.
	BEQ .ERROR.READY_OVERFLOW
	INY ;advance index to record byte #$04 (2nd Readied Flags Field)
	JMP .SET.READIED_FLAGS

.SET.READIED_FLAGS.DONE

;	Use active player # and the equipment slot value in the item table to set the item to readied the readied equipment table.  
;			if hands: if left hand Item_Type != $03 then ready item to right hand. If right hand item_type also != $03 then error. 
;					note: if a shield is readied, that will result in an Item_Type of $01 readied to a hand but that's fine. in CALCUATE.STATS, any Item_Type value in hands other than $00 (weapon) results in the shape ID being set to $01 which COMMAND.ATTACK treats to mean "skip that hand"

.UPDATE.CHR_SHEET.PC.READIED_EQUIP ;**OPT** Memory. This version is very close to the version in the unready routine. The main difference is .ITEM.IS.1_HANDED. It might be possible to consolidate the common parts by passing parms for the item-to-be-readied or fists/skin if an unready. 
;(update readied equipment slots on PC character sheet)
@START
;COMMON CODE


				
		LDA PARM.RE.READY.ITEM_TYPE
	JSR INV.RE.UPDATE.PROCESS_FLAGS
		;RETURN VALUE: INV.RE.UPDATE_DAMAGE.FLAG 	;($00 = no changes made that effect DAMAGE on the character sheet. $01 changes were made that affect the same), INV.RE.UPDATE_DEFENSE.FLAG 	;($00 = no changes made that effect DAMAGE on the character sheet. $01 changes were made that affect the same)


				
;BRANCH: HANDS/NOT_HANDS ;**OPT** Memory. There are several code blocks that do a hands slot check on byte #$05 of the current inventory record. Setup as JSR with parameter return. Search for LDY #$05 to find most of them. ;OR, this test could be done by LDA INV.ITEM_TABLE.ALL.EQUIPMENT_SLOT and CMP #INV.RE.SLOT.HANDS. The inventory record can only be tested for the item-to-be-readied because a search as already done. the item table would be the normal place to find it anyway. 
;(branch based on whether the item-to-be-readied uses the "hands" equipment slot)
	LDY #$05 ;set record byte $05 (inventory equipment slot field)
	LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y ;load field from target inventory record
	AND #$3F ;mask-out bit7 and bit6 (used for the stop value and 1/2handed weapon flags)
	CMP #INV.RE.SLOT.HANDS
	BEQ .ITEM_TO_BE_READIED.USES.HANDS
.ITEM_TO_BE_READIED.USES.NOT_HANDS ;the item-to-be-readied doesn't use the hands equipment slot
;	*if inventory equipment slot field != hands
;		set equipmenent slot for active player to the item-to-be-readied

	;calculate index to equipment slot in CHR_SHEET.PC.READIED_EQUIP
	ASL ;X2 (index =equipment slot # * 2)
	TAX
	;set equipment slot to the item-to-be-readied 
	LDA PARM.RE.READY.ITEM_TYPE
	STA CHR_SHEET.PC.READIED_EQUIP.RECORD.READ,X
	LDA PARM.RE.READY.ITEM_ID
	STA CHR_SHEET.PC.READIED_EQUIP.RECORD.READ+$1,X
 
	JMP .UPDATE.CHR_SHEET.PC.READIED_EQUIP.DONE

		
.ITEM_TO_BE_READIED.USES.HANDS ;the item-to-be-readied uses the hands equipment slot
;BRANCH: IS WEAPON 1-HANDED OR 2-HANDED?			
	LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y ;load field from target inventory record
	BPL .ITEM.IS.1_HANDED ;if bit7 is NOT set then 1handed weapon
@START	
	
;.ITEM.IS.2_HANDED 
;	*if inventory equipment slot field = 2 handed weapon
		;*set left hand to item-to-be-readied 
		;*set right hand Item_Type $03, Item_ID $00 (hand-in use). This prevents player from attacking twice. 
	
	;set left hand readied item to hands (fists)
	LDA PARM.RE.READY.ITEM_TYPE
	STA CHR_SHEET.READIED_EQUIP.TYPE.LHAND
	LDA PARM.RE.READY.ITEM_ID
	STA CHR_SHEET.READIED_EQUIP.ID.LHAND

	;set right hand readied item to HAND IN USE (prevents fists from getting 2 attacks per turn)
	LDA #INV.ITEM_TYPE.NO_ATTACK_TURN
	STA CHR_SHEET.READIED_EQUIP.TYPE.RHAND
	JMP .UPDATE.CHR_SHEET.PC.READIED_EQUIP.DONE
	
.ITEM.IS.1_HANDED	
;	*if inventory equipment slot field = hands
;		*if inventory equipment slot field = 1 handed weapon
	;		*check left hand: if available then set left hand to item-to-be-readied
	;		*check right hand: if available then set left hand to item-to-be-readied
	;			;if neither hand is available then report error
	;Available = hand set to in-use or fists. In this context, the in-use code means the hand is empty and available.
	;Basically the constant #INV.ITEM_TYPE.NO_ATTACK_TURN is used when a hand should not get an attack turn. It got named the in-use
	;code because it was originally created to occupy hand slots when the other hand slot had a 2-handed weapon. 
	;Even though the constant has been renamed correctly there are tons of comments in the source code the mention
	;"in use" or somethig along that line. Sorry!!!
	
.CHECK.LHAND
	;is left hand in use? 
	LDA CHR_SHEET.READIED_EQUIP.TYPE.LHAND
	CMP #INV.ITEM_TYPE.NO_ATTACK_TURN ;in this case, the in_use code means the hand is available because when an item is unreadied that hand is set to in-use so it doesn't get an attack turn. basically, in-use means empty in the context of the unready routine. 
	BEQ .LHAND.AVAILABLE ;branch if hand isn't in use
;CHECK FOR FISTS
	CMP #INV.ITEM_TYPE.WEAPON.FISTS
	BNE .CHECK.LHAND.DONE ;branch if fists aren't readied to left hand
	LDA CHR_SHEET.READIED_EQUIP.ID.LHAND
	CMP #INV.ITEM_ID.WEAPON.FISTS
	BNE .CHECK.LHAND.DONE ;branch if fists aren't readied to left hand
	
.LHAND.AVAILABLE	
	;item match
	;set left hand readied item to item-to-be-readied
	LDA PARM.RE.READY.ITEM_TYPE
	STA CHR_SHEET.READIED_EQUIP.TYPE.LHAND
	LDA PARM.RE.READY.ITEM_ID
	STA CHR_SHEET.READIED_EQUIP.ID.LHAND	
	JMP .UPDATE.CHR_SHEET.PC.READIED_EQUIP.DONE
.CHECK.LHAND.DONE	

.CHECK.RHAND				
	;is right hand in use? 
	LDA CHR_SHEET.READIED_EQUIP.TYPE.RHAND
	CMP #INV.ITEM_TYPE.NO_ATTACK_TURN ;in this case, the in_use code means the hand is available because when an item is unreadied that hand is set to in-use so it doesn't get an attack turn. basically, in-use means empty in the context of the unready routine. 
	BEQ .RHAND.AVAILABLE ;branch if hand isn't in use
;CHECK FOR FISTS
	CMP #INV.ITEM_TYPE.WEAPON.FISTS
	BNE .ERROR.NEITHER_HAND_AVAILABLE ;branch if fists aren't readied to right hand
	LDA CHR_SHEET.READIED_EQUIP.ID.RHAND
	CMP #INV.ITEM_ID.WEAPON.FISTS
	BNE .ERROR.NEITHER_HAND_AVAILABLE ;branch if fists aren't readied to right hand
	
.RHAND.AVAILABLE		
	;item match
	;set right hand readied item to item-to-be-readied
	LDA PARM.RE.READY.ITEM_TYPE
	STA CHR_SHEET.READIED_EQUIP.TYPE.RHAND
	LDA PARM.RE.READY.ITEM_ID
	STA CHR_SHEET.READIED_EQUIP.ID.RHAND		
	JMP .UPDATE.CHR_SHEET.PC.READIED_EQUIP.DONE	
	
	;**FALLS THROUGH**

.ERROR.NEITHER_HAND_AVAILABLE
;.UPDATE.CHR_SHEET.PC.READIED_EQUIP (.EXECUTE.READY) reports that
;neither hand is available in CHR_SHEET.READIED_EQUIP. Available = hand set to in-use or fists.
;In this context, the in-use code means the hand is empty and available.  

	JSR PREP.BRK
	BRK
	
@END

.UPDATE.CHR_SHEET.PC.READIED_EQUIP.DONE
	;**FALLS THROUGH**
@END		

	;**FALLS THROUGH**

@END


		; LDA #$B3
		; JSR COUT
		; LDA #$8D
		; JSR COUT
		
		
	;**FALLS THROUGH**

; .UNREADY.FISTS
	; LDX RE.ACTIVE_PLAYER ;load index to INV.READIED_FLAGS.AND_MASK	

	; ;validate entrance
	; LDA PARM.RE.READY.ITEM_TYPE
	; BNE .UNREADY.DEFAULT_ITEMS.DONE ;branch if item to ready is not type weapon
	

	; ;set readied flag for fists item
	; ;record byte #$03 (1st Readied Flags Field). Skin it always the first record in INV.PLAYER.INVENTORY.DATA		
	; LDX RE.ACTIVE_PLAYER ;load index to INV.READIED_FLAGS.AND_MASK	
	; LDA INV.PLAYER.INVENTORY.DATA+$3 ;load Readied Equipment Flags #1
	; AND INV.READIED_FLAGS.AND_MASK,X ;mask-out bit flag for active player (this unreadies the item)
	; STA INV.PLAYER.INVENTORY.DATA+$3 ;save Readied Equipment Flags #1
.UNREADY.DEFAULT_ITEMS.DONE
	

.RETURN.ITEM_READIED
;RETURN VALUE: ACC = ($00 item readied | $01 = item not readied: item is not readiable | $02 don't use| $03 item not readied: STR gate check failed | $04 item not readied: DEX gate check failed | $05 item not readied: INT gate check failed | $06 can't ready armor during combat | $07 item unreadied | $08 prompt spell hotkey to ready)				
	LDA #$00

.RETURN_VALUE.SET


				
	;ACC = return value
	STA INV.RE.RETURN_VALUE	;ACC = ($00 item readied | $01 = item not readied: item is not readiable | $02 don't use| $03 item not readied: STR gate check failed | $04 item not readied: DEX gate check failed | $05 item not readied: INT gate check failed | $06 can't ready armor during combat | $07 item unreadied | $08 prompt spell hotkey to ready)				
	
.EXIT

	
;****INV DEBUG LOG*** 			;**OPT** Memory. Remove.
@START
.DEBUG.LABEL2
		;;PHA
		; LDA #.DEBUG.LABEL2
		; STA INV.DEBUG.ADDRESS+$0
		; LDA /.DEBUG.LABEL2
		; STA INV.DEBUG.ADDRESS+$1
	JSR INV.DEBUG.LOG.WRITE
		;;PLA
@END

			
	JMP INV.RE.EXIT

@END

INV.UNREADY_EQUIPMENT
@START
;PARAMETERS: ACC = item-to-unready Item_Type, X-REG = item-to-unready Item_ID, PARM.RE.ACTIVE_PLAYER, PARM.RE.UNREADY.CLEAR_FLAG, [PLAYER_MERCH.INVENTORY.DATA.POINTER(2) OR INV.RE.CONFLICT_RECORD1, INV.RE.CONFLICT_RECORD2]
;ENTRANCE: direct, INV.READY_EQUIPMENT
;RETURN: updated CHR_SHEET.PC.READIED_EQUIP and INV.PLAYER.INVENTORY.DATA
 				
;=====================CODE-SECTION DOCUMENTATION====================================
;
;
;PSEUDO CODE
;skip inventory record search IF either conflict record is set, which means that the call is from the ready routine and the inventory record pointer is set as a parameter
;	
;clearing the readied flags on the inventory record: clear one of the two readied flags but not both, unless PARM.UNREADY.CLEAR_FLAG set, then clear both. 
;
;	
;*updated readied equipment table
;	*if inventory equipment slot field != hands
;		set equipmenent slot for active player to Item_Type $01, Item_ID $00 (skin)
;	*if inventory equipment slot field = hands
;		*if inventory equipment slot field = 1 handed weapon
;	;		*check left hand: if it's the item requested to be unreadied then set left hand to Item_Type $03 (not in use)
;	;		*check right hand: if it's the item requested to be unreadied then set right hand to Item_Type $03 (not in use)
;	;			;if neither hand has the item to unready then report error
;			*if after the above operations both left/right hand slots have Item_Type $03 (no weapons readied) then set left hand to hands(fists)
;	*if inventory equipment slot field = 2 handed weapon
;		;*set left hand to Item_Type $00, Item_ID $00 (hands) 
;		;*set right hand Item_Type $03, Item_ID $00 (hand-in use). This prevents player from attacking twice. 
;	
;
;=================================================================================





			
;SAVE PARAMETERS
	;ACC = item-to-unready Item_Type
	STA PARM.RE.UNREADY.ITEM_TYPE
	;X-REG item-to-unready Item_ID
	STX PARM.RE.UNREADY.ITEM_ID


	
;****INV DEBUG LOG*** 			;**OPT** Memory. Remove.
@START
.DEBUG.LABEL
		; ;PHA
		; LDA #.DEBUG.LABEL
		; STA INV.DEBUG.ADDRESS+$0
		; LDA /.DEBUG.LABEL
		; STA INV.DEBUG.ADDRESS+$1
	JSR INV.DEBUG.LOG.WRITE
		; ;PLA
@END

	
			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; lda #$ab
			; ldx PARM.RE.UNREADY.ITEM_TYPE
			; ldy PARM.RE.UNREADY.ITEM_ID
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP
			
						;inc troubleshooting.hook
			
	
.INIT
	;set run flag so that INV.INVENTORY_MODULE.EXIT will know that equiment slots changed, triggering it to call CALCULATE.COMBAT.STATS to recalculate stats derived from what equipment is readied. 
	LDA #$01 ;
	STA INV.READY_UNREADY.RUN_FLAG	;($00 = ready_unready not called | $01 ready_unready called. ready_unready refers to INV.READY_UNREADY.EQUIPMENT.ENTRANCE)
		



.SPELL_MENU.UNREADY
@START		
;=====================CODE-SECTION DOCUMENTATION==========================================================================
;
;The ready and unready scheme for spells is much different than for other items. 
;spells don't use the readied equipment table and readied status isn't stored in the
;PLAYER.INVENTORY.DATA readied flag fields. 
;
;Spells are really pretty simple. If a spell item is readied, it's spell code is stored as value in
;the PLAYER.HOTKEYS.SPELLS array. If a spell hotkey has the high-bit set then no spell is readied to it. 
;
;
;
;===========================================================================================================================

	
;VALIDATE ENTRANCE
	LDA INV.ACTIVE_MENU.CODE 
	CMP #INV.ACTIVE_MENU.SPELLS ;INV_4.1b.DISPLAY.MODE = $02 (misc items), but is spell menu active?
	BNE	.SPELL_MENU.READY.DONE
	LDY INV_4.SPELL_READY.HOTKEY_SELECTED	
	LDA #$FF ;high-bit set is the "no spell readied" code
	STA	PLAYER.HOTKEYS.SPELLS,Y
	
	
	;set return value
	LDA #$07
	STA INV.RE.RETURN_VALUE	;ACC = ($00 item readied | $01 = item not readied: item is not readiable | $02 don't use| $03 item not readied: STR gate check failed | $04 item not readied: DEX gate check failed | $05 item not readied: INT gate check failed | $06 can't ready armor during combat | $07 item unreadied | $08 prompt spell hotkey to ready)

	JMP INV.RE.EXIT.ALT_ENTRANCE	
.SPELL_MENU.READY.DONE	
@END
		

		
;IS INVENTORY RECORD ALREADY PROVIDED BY CALLING ROUTINE?
;(INV.READY_EQUIPMENT calls this routine to unready items whose equipment slots conflict with the item-to-be-readied. In
;that case PLAYER_MERCH.INVENTORY.DATA.POINTER(2) is already set to the inventory record of the item-to-unready
;We can detect a call from INV.READY_EQUIPMENT because it only calls this routine if conflicts were record in INV.RE.CONFLICT_RECORD1 and INV.RE.CONFLICT_RECORD2
	LDA INV.RE.CONFLICT_RECORD1+$2
	BMI .GET.INVENTORY.RECORD ;init value is $FF so if high-bit set then the call is not from INV.READY_EQUIPMENT
	;no need to check the 2nd conflict record as it is only used if the 1st conflict record is in-use.

.CALL.FROM.READY_EQUIMENT_ROUTINE
;subtract the weight of the conflicting items that will be unreadied by this routine

	;validate entrance (only run once per call to the ready routine)
	LDA INV.UNREADY.ITERATION_COUNTER
	BNE .GET.INVENTORY.RECORD.DONE
	
	LDA CHR_SHEET.PC.READIED_EQUIPMENT_WEIGHT ;load weight of active player's readied equipment
	SEC
	SBC INV.GATE_CHECK.WEIGHT.TALLY
	STA CHR_SHEET.PC.READIED_EQUIPMENT_WEIGHT ;load weight of active player's readied equipment

	INC INV.UNREADY.ITERATION_COUNTER
	
	JMP .GET.INVENTORY.RECORD.DONE
	
.GET.INVENTORY.RECORD
		;PLAYER_MERCH.INVENTORY.DATA.POINTER(2): already set
		LDA PARM.RE.UNREADY.ITEM_TYPE
		LDX PARM.RE.UNREADY.ITEM_ID		
	JSR INV.SEARCH.PLY_INV
		;RETURN VALUE:PLAYER_MERCH.INVENTORY.DATA.POINTER(2)=(target inventory record)

	
;subtract weight of item-to-unready
	LDA CHR_SHEET.PC.READIED_EQUIPMENT_WEIGHT ;load weight of active player's readied equipment
	SEC
	SBC INV.ITEM_TABLE.ALL.STR_WEIGHT
	STA CHR_SHEET.PC.READIED_EQUIPMENT_WEIGHT ;load weight of active player's readied equipment
.GET.INVENTORY.RECORD.DONE

			
.EXECUTE.UNREADY		
	;PLAYER_MERCH.INVENTORY.DATA.POINTER(2) = pointer to target inventory record

;CLEAR 1ST READIED FLAG FIELD
.CLEAR.READIED_FLAGS_FIELD1
	LDY #$03 ;set record byte $03 (readied flags field #1)
	LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y ;load readied flags of target inventory record
	LDX RE.ACTIVE_PLAYER ;load index to INV.READIED_FLAGS.AND_MASK
	PHA ;push readied flag field #1 to stack
	AND INV.READIED_FLAGS.AND_MASK,X ;unset ready flag for inventory active player
	STA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y ;save readied flags to target inventory record
	PLA ;pull readied flag field #1 from stack
	;if active player didn't have item readied in 1st field, clear 2nd field
	;(this action assumes the item is readied via the 2nd field but that isn't verified)
	AND INV.READIED_FLAGS.BIT_TEST_MASK,X ;unset ready flag for inventory active player
	BEQ	.CLEAR.READIED_FLAGS_FIELD2

;CLEAR BOTH READIED FLAG FIELDS?
;(if the the unready request is made by the ready routine and the item ready request was for a 2-handed weapon then clear both
;readied flag fields becuase if the player had two units of this item readied both readied flag fields would be set)
	LDA PARM.RE.UNREADY.CLEAR_FLAG ;($00 = clear 1st readied flags field only | $01 clear both readied flags field)
	BEQ .CLEAR.READIED_FIELDS.DONE
.CLEAR.READIED_FLAGS_FIELD2
	INY ;set record byte $04 (readied flags field #2)
	LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y ;load readied flags of target inventory record
	LDX RE.ACTIVE_PLAYER ;load index to INV.READIED_FLAGS.AND_MASK
	AND INV.READIED_FLAGS.AND_MASK,X ;unset ready flag for inventory active player
	STA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y ;save readied flags to target inventory record
.CLEAR.READIED_FIELDS.DONE



			
			
			
.UPDATE.CHR_SHEET.PC.READIED_EQUIP
;(update readied equipment slots on PC character sheet)
@START
;COMMON CODE
		LDA PARM.RE.UNREADY.ITEM_TYPE
	JSR INV.RE.UPDATE.PROCESS_FLAGS
		;RETURN VALUE: INV.RE.UPDATE_DAMAGE.FLAG 	;($00 = no changes made that effect DAMAGE on the character sheet. $01 changes were made that affect the same), INV.RE.UPDATE_DEFENSE.FLAG 	;($00 = no changes made that effect DAMAGE on the character sheet. $01 changes were made that affect the same)
	
;BRANCH: HANDS/NOT_HANDS
;(branch based on whether the item to unready uses the "hands" equipment slot)			
	LDY #$05 ;set record byte $05 (inventory equipment slot field)
	LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y ;load field from target inventory record
	AND #$3F ;mask-out bit7 and bit6 (used for the stop value and 1/2handed weapon flags)
	CMP #INV.RE.SLOT.HANDS
	BEQ .ITEM.USES.HANDS
.ITEM.USES.NOT_HANDS ;the item to be unreadied doesn't use the hands equipment slot
;	*if inventory equipment slot field != hands
;		set equipmenent slot for active player to Item_Type $01, Item_ID $00 (skin)
	
	; ;set process flag for equipment slot associated with item-to-unready
	; ;(setting the flag means CALCULATE.STATS will process this slot)
	; TAX
	; STA CHR_SHEET.PC.PROCESS_FLAGS,X

	;calculate index to equipment slot in CHR_SHEET.PC.READIED_EQUIP
	ASL ;X2 (index =equipment slot # * 2)
	TAX
	;set readied item to skin
	LDA #INV.ITEM_TYPE.ARMOR.SKIN
	STA CHR_SHEET.PC.READIED_EQUIP.RECORD.READ,X
	LDA #INV.ITEM_ID.ARMOR.SKIN
	STA CHR_SHEET.PC.READIED_EQUIP.RECORD.READ+$1,X
	JMP .UPDATE.CHR_SHEET.PC.READIED_EQUIP.DONE

		
.ITEM.USES.HANDS ;the item to be unreadied uses the hands equipment slot
@START		
;BRANCH: IS WEAPON 1-HANDED OR 2-HANDED?		
	LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y ;load field from target inventory record
	BPL .ITEM.IS.1_HANDED ;if bit7 is NOT set then 1handed weapon

					
;.ITEM.IS.2_HANDED 
;	*if inventory equipment slot field = 2 handed weapon
		;*set left hand to Item_Type $00, Item_ID $00 (hands) 
		;*set right hand Item_Type $03, Item_ID $00 (hand-in use). This prevents player from attacking twice. 
	

	;set right hand readied item to HAND IN USE (prevents fists from getting 2 attacks per turn)
	LDA #INV.ITEM_TYPE.NO_ATTACK_TURN
	STA CHR_SHEET.READIED_EQUIP.TYPE.RHAND
	
	JMP .READY.FISTS ;set left hand readied item to hands (fists)
	

	
.ITEM.IS.1_HANDED	
;	*if inventory equipment slot field = hands
;		*if inventory equipment slot field = 1 handed weapon
	;		*check left hand: if it's the item requested to be unreadied then set left hand to Item_Type $00, Item_ID $00 (hands)
	;		*check right hand: if it's the item requested to be unreadied then set right hand to Item_Type $00, Item_ID $00 (hands)
	;			;if neither hand has the item to unready then report error
	;		*if after the above operations both left/right hand slots have Item_Type/ID $00 then set the right hand to Item_Type $03 (to prevent two "hands" weapons from being readied)


.CHECK.LHAND
	;compare readied item in left hand to the item unready request
	LDA CHR_SHEET.READIED_EQUIP.TYPE.LHAND
	CMP PARM.RE.UNREADY.ITEM_TYPE
	BNE .CHECK.LHAND.DONE
	LDA CHR_SHEET.READIED_EQUIP.ID.LHAND
	CMP PARM.RE.UNREADY.ITEM_ID
	BNE .CHECK.LHAND.DONE
	;item match
	;set left hand readied item to NOT-IN-USE
	LDA #INV.ITEM_TYPE.NO_ATTACK_TURN
	STA CHR_SHEET.READIED_EQUIP.TYPE.LHAND
	; LDA #INV.ITEM_TYPE.NO_ATTACK_TURN
	; STA CHR_SHEET.READIED_EQUIP.ID.LHAND	
	JMP .CHECK.NO_WEAPONS.READIED
.CHECK.LHAND.DONE	

.CHECK.RHAND			
	;compare readied item in right hand to the item unready request
	LDA CHR_SHEET.READIED_EQUIP.TYPE.RHAND
	CMP PARM.RE.UNREADY.ITEM_TYPE
	BNE .ERROR.ITEM.NOT_FOUND
	LDA CHR_SHEET.READIED_EQUIP.ID.RHAND
	CMP PARM.RE.UNREADY.ITEM_ID
	BNE .ERROR.ITEM.NOT_FOUND
	;item match
	;set right hand readied item to NOT-IN-USE
	LDA #INV.ITEM_TYPE.NO_ATTACK_TURN
	STA CHR_SHEET.READIED_EQUIP.TYPE.RHAND
	; LDA #INV.ITEM_TYPE.NO_ATTACK_TURN
	; STA CHR_SHEET.READIED_EQUIP.ID.RHAND	
	
	

	;**FALLS THROUGH**

.CHECK.NO_WEAPONS.READIED
;		*if after the above operations both left/right hand slots have Item_Type $03 (no weapons readied) then set left hand to hands(fists)

	LDA CHR_SHEET.READIED_EQUIP.TYPE.LHAND
	CMP #INV.ITEM_TYPE.NO_ATTACK_TURN
	BNE .UPDATE.CHR_SHEET.PC.READIED_EQUIP.DONE
	LDA CHR_SHEET.READIED_EQUIP.TYPE.RHAND
	CMP #INV.ITEM_TYPE.NO_ATTACK_TURN	
	BNE .UPDATE.CHR_SHEET.PC.READIED_EQUIP.DONE

	;check failed. No weapons are readied. Set left hand to hands(fists.)
.READY.FISTS
	LDA #INV.ITEM_TYPE.WEAPON.FISTS
	STA CHR_SHEET.READIED_EQUIP.TYPE.LHAND
	LDA #INV.ITEM_ID.WEAPON.FISTS
	STA CHR_SHEET.READIED_EQUIP.ID.LHAND

	; ;set readied flag for fists item
	; ;LDY #$03 ;set index to record byte #$03 (1st Readied Flags Field)		
	; LDX RE.ACTIVE_PLAYER ;load index to INV.READIED_FLAGS.AND_MASK	
; .SET.READIED_FLAGS
	; LDA INV.PLAYER.INVENTORY.DATA+$3 ;load Readied Equipment Flags #1
	; ;PHA ;push Readied Flag value to stack
	; ;AND INV.READIED_FLAGS.BIT_TEST_MASK,X ;test bit flag for active player
	; ;BNE .USE.READIED.FLAGS.FIELD2 ;branch if active player already has item readied via this field (i.e if the player is trying to ready a 2nd single handed weapon of the same type)
	; ;PLA ;pull Readied Flag value from stack
	; ORA INV.READIED_FLAGS.ORA_MASK,X ;mask-in bit flag for active player (this readies the item)
	; STA INV.PLAYER.INVENTORY.DATA+$3 ;save Readied Equipment Flags #1
	; ;JMP .SET.READIED_FLAGS.DONE
	

	JMP .UPDATE.CHR_SHEET.PC.READIED_EQUIP.DONE

.ERROR.ITEM.NOT_FOUND
;.UNREADY_EQUIPMENT (INV.READY_UNREADY.EQUIPMENT.ENTRANCE) reports that the Item_Type/ID 
;of the unready request (passed via parameters), was not found in CHR_SHEET.READIED_EQUIP 

					; lda troubleshooting.hook
					; sta $be00
					; LDA #CHR_SHEET.PC.READIED_EQUIP.RECORD.READ
					; STA $BF00
					; LDA /CHR_SHEET.PC.READIED_EQUIP.RECORD.READ
					; STA $BF01
					; lda #$cc
					; ldx PARM.RE.UNREADY.ITEM_TYPE
					; ldy PARM.RE.UNREADY.ITEM_ID
					
					; JSR PREP.BRK
					; BRK
					
	JSR PREP.BRK
	BRK
	
@END
.UPDATE.CHR_SHEET.PC.READIED_EQUIP.DONE
;****INV DEBUG LOG*** 			;**OPT** Memory. Remove.
@START
.DEBUG.LABEL2
		; ;PHA
		; LDA #.DEBUG.LABEL2
		; STA INV.DEBUG.ADDRESS+$0
		; LDA /.DEBUG.LABEL2
		; STA INV.DEBUG.ADDRESS+$1
	JSR INV.DEBUG.LOG.WRITE
		;;PLA
@END

			

;SET RETURN VALUE
	LDA #$07 ;set item unreadied return code
	STA INV.RE.RETURN_VALUE	;($00 item readied | $01 = item not readied: item is not readiable | $02 = item not readied: QTY not sufficient | $03 item not readied: STR gate check failed | $04 item not readied: DEX gate check failed | $05 item not readied: INT gate check failed | $06 not used | $07 item unreadied)				


;IS INVENTORY RECORD ALREADY PROVIDED BY CALLING ROUTINE? (if no, then exit, othewise return to INV.READY_EQUIPMENT)
;(INV.READY_EQUIPMENT calls this routine to unready items whose equipment slots conflict with the item-to-be-readied. In
;that case PLAYER_MERCH.INVENTORY.DATA.POINTER(2) is already set to the inventory record of the item-to-unready
;We can detect a call from INV.READY_EQUIPMENT because it only calls this routine if conflicts were record in INV.RE.CONFLICT_RECORD1 and INV.RE.CONFLICT_RECORD2

	LDA INV.RE.CONFLICT_RECORD1+$2
	BMI INV.RE.EXIT ;init value is $FF so if high-bit set then the call is not from INV.READY_EQUIPMENT
	;no need to check the 2nd conflict record as it is only used if the 1st conflict record is in-use.

	
	RTS ;return to INV.READY_EQUIPMENT
	
	
				; LDA PARM.RE.UNREADY.ITEM_TYPE
				; sta $be00
				; LDA PARM.RE.UNREADY.ITEM_ID
				; sta $be01
				; pla
				; tax
				; pla
				; tay
				; LDA #$AB
				; ; ; LDX PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
				; ; ; LDY PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
				; ; ; ldx #CHR_SHEET.PC.READIED_EQUIP.RECORD.READ
				; ; ; ldy /CHR_SHEET.PC.READIED_EQUIP.RECORD.READ
				; ; LDX #INV.RE.CONFLICT_RECORD1
				; ; LDY /INV.RE.CONFLICT_RECORD1
				; JSR PREP.BRK
				; BRK	
				
@END		

@END

			
		; LDA #$B4
		; JSR COUT
		; LDA #$8D
		; JSR COUT
		; JSR KEYIN
				
				; PHA
				; LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y ;load readied flags of target inventory record
				; STA $BE00
				; STX $BE01
				; LDA INV.READIED_FLAGS.AND_MASK,X ;unset ready flag for inventory active player
				; STA $BE02
				; PLA
				; STA $BE03
				; LDA #$BB
				; LDX PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
				; LDY PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
				; JSR PREP.BRK
				; BRK
				
	;PLAYER_MERCH.INVENTORY.DATA.POINTER(2)*

INV.RE.EXIT
@START
;WRITE CHANGES TO READIED EQUIPMENT TABLE
;(this writes the changes from the read/write buffer to the in-memory table). The changes are written to the inventory file
;on exit of the inventory module (see INV.INVENTORY_MODULE.EXIT)


;****INV DEBUG LOG*** 			;**OPT** Memory. Remove.
@START
.DEBUG.LABEL
		; ;PHA
		; LDA #.DEBUG.LABEL
		; STA INV.DEBUG.ADDRESS+$0
		; LDA /.DEBUG.LABEL
		; STA INV.DEBUG.ADDRESS+$1
	JSR INV.DEBUG.LOG.WRITE
		;;PLA
@END


		

				; ldx #$00
; .test.loopaa
				; lda CHR_SHEET.PC.READIED_EQUIP.RECORD.READ,x
				; sta $bf00,x
				; inx
				; cpx #$10
				; bne .test.loopaa
				
				
		LDA RE.ACTIVE_PLAYER ;ACC = player sequential # (high-bit not set = read mode)
		PHA ;push sequential active player # to stack
		ORA #$80 ;mask-in hit-bit to set write mode
	JSR INV.READ_WRITE_RECORD.CHAR_SHEET.READIED
		;RETURN VALUE: CHR_SHEET.PC.READIED_EQUIP.RECORD.READ($10)

			
;WRITE CHANGES TO ACTIVE PC CHARACTER SHEET
;(this writes the changes from the read/write buffer to the in-memory table). The PC character sheet data
;is always in memory and is only saved to a file during the save-game process 

		PLA ;pull sequential active player # from stack
		ORA #$80 ;set write mode by masking in high-bit
		;ACC = player sequential # + (high-bit not set = read mode | high-bit set = write mode)
	JSR COMBAT.READ_WRITE.CHR_SHEET.PC	

INV.RE.EXIT.ALT_ENTRANCE		
;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX


		; LDA #$B3
		; JSR COUT
		; LDA #$8D
		; JSR COUT

	;load return value
	LDA INV.RE.RETURN_VALUE	;ACC = ($00 item readied | $01 = item not readied: item is not readiable | $02 don't use| $03 item not readied: STR gate check failed | $04 item not readied: DEX gate check failed | $05 item not readied: INT gate check failed | $06 can't ready armor during combat | $07 item unreadied | $08 prompt spell hotkey to ready)				

	



			
	RTS ;return to routine that called INV.READY_UNREADY.EQUIPMENT.ENTRANCE
@END


@END


;LOCAL SUBROUTINES	(INV.READY_UNREADY.EQUIPMENT.ENTRANCE)
@START
INV.SEARCH.PLY_INV
@START
;PARAMETERS: ACC = search-item Item_Type, X-REG = search-item Item_ID, PLAYER_MERCH.INVENTORY.DATA.POINTER(2), PARM.RE.SEARCH.ITEM_TYPE, PARM.RE.SEARCH.ITEM_ID
;ENTRANCE: direct
;RETURN: PLAYER_MERCH.INVENTORY.DATA.POINTER(2)*
;
;*PLAYER_MERCH.INVENTORY.DATA.POINTER(2) contains the memory address of the inventory record for the item the search was for -if it was found.

;SAVE PARAMETERS
	;ACC = search-item  Item_Type
	STA PARM.RE.SEARCH.ITEM_TYPE
	;X-REG search-item  Item_ID
	STX PARM.RE.SEARCH.ITEM_ID


		
	;LDX #$00
.LOOP.SEARCH.INVENTORY.DATA

	;open record check
	;(implicitly done by TEST1 below because Item_Type will never be equal to the open record value of $FF)

	LDY #$00 ;reset Y-REG on each iteration because array position is managed exclusively through PLAYER_MERCH.INVENTORY.DATA.POINTER 
	
	;Y-REG ;set to record byte $0 (item_level | item_type)
	LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y
	;Item_Type is in LO nibble.
	AND #$F ;mask-out HO nibble. 
;TEST1: Item_Type
	CMP PARM.RE.SEARCH.ITEM_TYPE 
	BNE .NEXT_RECORD
	
	INY ;advance to record byte $1 (item_code)
.TEST2 ;item_code
	LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y
	CMP PARM.RE.SEARCH.ITEM_ID
	BEQ .ITEM.FOUND
	
.NEXT_RECORD
	;INX
	LDA PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
	CLC
	ADC #INV.PLAYER.INVENTORY.DATA.RECORD_SIZE
	STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
	LDA PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
	ADC /INV.PLAYER.INVENTORY.DATA.RECORD_SIZE ;16-bit add
	STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
.EXIT.TEST	
	CMP /INV.PLAYER.INVENTORY.DATA.PATCH_END
	BCC .LOOP.SEARCH.INVENTORY.DATA
	BEQ .LOOP.SEARCH.INVENTORY.DATA

	;**FALLS THROUGH**
	
.ITEM.NOT_FOUND
;INV.SEARCH.PLY_INV reports inventory item not found. This shouldn't ever occur because calls to 
;the top level routine (INV.READY_UNREADY.EQUIPMENT.ENTRANCE) should only be permitted by the UI if the
;inventory item is on screen and the on screen data is sourced from the player inventory array which this routine
;searches.  
	JSR PREP.BRK
	BRK
	
.ITEM.FOUND


				; LDX #$BC
				; LDX PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
				; LDY PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
				; ;LDX FILE.OPEN.INVENTORY.DEBUG_COUNTER
				; JSR PREP.BRK
				; BRK
				
				
.EXIT

	RTS
	
@END

INV.SEARCH.PLY_INV.CONFLICTS.NOT_HANDS
@START
;PARAMETERS: PLAYER_MERCH.INVENTORY.DATA.POINTER(2), PARM.RE.ACTIVE.PLAYER
;ENTRANCE: direct
;RETURN: INV.RE.CONFLICT_RECORD1(3), PLAYER_MERCH.INVENTORY.DATA.POINTER(2)*
;
;*PLAYER_MERCH.INVENTORY.DATA.POINTER(2) contains the memory address of the inventory record for the item that is readied to the same equipment slot as the item-to-be-readied
;*INV.RE.CONFLICT_RECORD1(3) "" and in the 3rd byte, the number of times item is readied. This array seems unnecessary but it is used for consistency with
;INV.SEARCH.PLY_INV.CONFLICTS.HANDS so that the return value parse for both can look at the same arrays.

;=====================SUB-ROUTINE DOCUMENTATION====================================
;
;Returns a pointer to any inventory records that are readied to equipment slots which
;conflict with the item requested to be readied.
;
;Only used for items requested to be readied that use equipment slots other than "hands"
;
;--Pseduo Code--
;(record parse loop) IF item table equipment slot field = inventory table equipment slot field AND 1st readied flags field = active player THEN 
;		save record pointer to conflict record #1. goto EXECUTE UNREADY
;
;=================================================================================

				
	;LDX #$00
.LOOP.SEARCH.INVENTORY.DATA

	;open record check
	;(skip record if not open)
	LDY #$00 ;reset Y-REG on each iteration because array position is managed exclusively through PLAYER_MERCH.INVENTORY.DATA.POINTER 
	LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y ;load Item_Type of current inventory record
	CMP #$FF
	BEQ .NEXT_RECORD
	
	LDY #$05 ;reset Y-REG on each iteration because array position is managed exclusively through PLAYER_MERCH.INVENTORY.DATA.POINTER 	
	;Y-REG ;set to record byte $5 (equimpment slot)
	LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y ;load inventory record equipment slot field
	AND #$3F ;mask-out bit6 and bit7
;TEST1: Item-to-be-readied Equipment Slot = inventory record equipment slot?
	CMP INV.ITEM_TABLE.ALL.EQUIPMENT_SLOT ;load item-to-be-readied equipment slot field
	BNE .NEXT_RECORD
	
	DEY ;back up to record byte $4
	DEY ;back up to record byte $3 (Readied Equipment Flags #1)
.TEST2 ;does active player have item readied?
	LDX RE.ACTIVE_PLAYER ;load index to INV.READIED_FLAGS.AND_MASK	
	LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y ;load Readied Equipment Flags #1
	AND INV.READIED_FLAGS.BIT_TEST_MASK,X ;test bit flag for active player
	BEQ .NEXT_RECORD ;branch if bit not set (active player does't have item readied)	
	;active player has item readied: save inventory record as a conflict
	LDA PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
	STA INV.RE.CONFLICT_RECORD1+$0
	LDA PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
	STA INV.RE.CONFLICT_RECORD1+$1
	LDA #$01
	STA INV.RE.CONFLICT_RECORD1+$2	;save # of times the player has this item readied. We know it's $1 because there is no need to check the 
									;2nd readied flags field for items which don't use the "hands" equipment slot.
	
	DEY ;back up to record byte $2
	DEY ;back up to record byte $1 (Item_ID)
	LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y ;load Item_ID of current inventory record
	STA INV.RE.CONFLICT_RECORD1+$4
	
	DEY ;back up to record byte $0 (Level | Item_Type)
	LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y ;load Item_Type of current inventory record
	;Item Item is in LO Nibble
	AND #$0F ;mask out HO Nibble
	STA INV.RE.CONFLICT_RECORD1+$3
	
	; (SAVING IN CASE NEEDED1) LDA #$01 ;always $01 because only weapons can have the same item readied more than once if sufficient qty permits.  
	; (SAVING IN CASE NEEDED1) STA INV.RE.CONFLICT_RECORD1+$5	;save qty of the current inventory record item that player has readied. 
	JMP .EXIT
	
.NEXT_RECORD
	;INX
	LDA PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
	CLC
	ADC #INV.PLAYER.INVENTORY.DATA.RECORD_SIZE
	STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
	LDA PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
	ADC /INV.PLAYER.INVENTORY.DATA.RECORD_SIZE ;16-bit add
	STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
.EXIT.TEST	
	CMP /INV.PLAYER.INVENTORY.DATA.PATCH_END
	BCC .LOOP.SEARCH.INVENTORY.DATA ;continue loop if end of array not reached
	BEQ .LOOP.SEARCH.INVENTORY.DATA ;continue loop if end of array not reached
	;NO CONFLICTS FOUND
	
	;**FALLS THROUGH**



.EXIT

				
	RTS
	
@END

INV.SEARCH.PLY_INV.CONFLICTS.HANDS
@START
;PARAMETERS: PLAYER_MERCH.INVENTORY.DATA.POINTER(2)
;ENTRANCE: direct
;RETURN: INV.RE.CONFLICT_RECORD1(3), [INV.RE.CONFLICT_RECORD2(3)], PLAYER_MERCH.INVENTORY.DATA.POINTER(2)*, IN.RE.ITEMS_READIED_IN_HANDS.TOTAL
;
;*PLAYER_MERCH.INVENTORY.DATA.POINTER(2) contains the memory address of the inventory record for the item that is readied to the same equipment slot as the item-to-be-readied
;*INV.RE.CONFLICT_RECORD1(3) "" and in the 3rd byte, the number of times item is readied. This array seems unnecessary but it is used for consistency with
;INV.SEARCH.PLY_INV.CONFLICTS.HANDS so that the return value parse for both can look at the same arrays.

;=====================SUB-ROUTINE DOCUMENTATION====================================
;
;Returns a pointer to any inventory records that are readied to equipment slots which
;conflict with the item requested to be readied, and with that pointer return the number
;of times the player has that item type/ID
;
;Only used for items requested to be readied that use the equipment slot "hands"
;
;--Pseduo Code--
;
;	count # of items active player has readied to hands
;		(record parse loop) IF inventory table equipmet slot = hands, increment in-record hands counter and in-table hands counter for each readied flags field with active player. Do two increments to the in-table hands counter if the conflicting item is 2-handed per the inventory table equipment slot;												;if either readied flags field has active player set then save record to conflict record #1 (or #2 if #1 is already set), and set byte 3 of the conflict record with the number of times the item is readied as per the two readied flag fields											
;									IF in-record hands counter = 2 (indicates dual wielding of the same item) then copy conflict record 1 to conflict record 2 so that the unready routine is called twice for this time.
;									error if both conflict records are already set. 
;
;
;=================================================================================


.INIT
	LDA #$00
	STA IN.RE.ITEMS_READIED_IN_HANDS.THIS_RECORD
	STA IN.RE.ITEMS_READIED_IN_HANDS.TOTAL
	STA IN.RE.DETECT.READIED_INSTANCE.FLAG ;($00 = no active readied PC flag found active on inventory record this iteration | >=$01 = active PC readied flag found active on inventory record this iteration) 
	
	;LDX #$00
.LOOP.SEARCH.INVENTORY.DATA
	;open record check
	;(skip record if not open)
	LDY #$00 ;reset Y-REG on each iteration because array position is managed exclusively through PLAYER_MERCH.INVENTORY.DATA.POINTER 
	LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y ;load Item_Type of current inventory record
	CMP #$FF
	BNE .OPEN_RECORD.CHECK.DONE 
	JMP .NEXT_RECORD
.OPEN_RECORD.CHECK.DONE
	
	LDY #$05 ;reset Y-REG on each iteration because array position is managed exclusively through PLAYER_MERCH.INVENTORY.DATA.POINTER 

;TESTS
@START	
	;Y-REG ;set to record byte $5 (equimpment slot)
	LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y ;load inventory record equipment slot field
	AND #$3F ;mask-out bit6 and bit7
;VALIDATE RECORD TEST1: is inventory record equipment slot = "hands"?
	CMP #INV.RE.SLOT.HANDS 
	BEQ .VALIDATE.RECORD.DONE ;if yes
	JMP .NEXT_RECORD ;if no
.VALIDATE.RECORD.DONE
	
;READIED FLAGS #2 TEST: ;does active player have item readied?
	DEY ;back up to record byte $4 (Readied Equipment Flags #2)
	LDX RE.ACTIVE_PLAYER ;load index to INV.READIED_FLAGS.BIT_TEST_MASK	
	LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y ;load Readied Equipment Flags #1
	AND INV.READIED_FLAGS.BIT_TEST_MASK,X ;test bit flag for active player
	BEQ .READIED.FLAGS2.TEST ;branch if bit not set (active player doesn't have item readied via Readied Equipment Flags field #2)	
	INC IN.RE.ITEMS_READIED_IN_HANDS.THIS_RECORD	
	INC IN.RE.ITEMS_READIED_IN_HANDS.TOTAL
	STA IN.RE.DETECT.READIED_INSTANCE.FLAG ;($00 = no active readied PC flag found active on inventory record this iteration | >=$01 = active PC readied flag found active on inventory record this iteration) 

.READIED.FLAGS2.TEST
	DEY ;back up to record byte $3 (Readied Equipment Flags #1)
	;X-REG: index to INV.READIED_FLAGS.BIT_TEST_MASK	
	LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y ;load Readied Equipment Flags #1
	AND INV.READIED_FLAGS.BIT_TEST_MASK,X ;test bit flag for active player
	BEQ .READIED.FLAGS.TESTS.DONE ;branch if bit not set (active player doesn't have item readied via Readied Equipment Flags field #2)	
	INC IN.RE.ITEMS_READIED_IN_HANDS.THIS_RECORD	
	INC IN.RE.ITEMS_READIED_IN_HANDS.TOTAL
	STA IN.RE.DETECT.READIED_INSTANCE.FLAG ;($00 = no active readied PC flag found active on inventory record this iteration | >=$01 = active PC readied flag found active on inventory record this iteration) 
.READIED.FLAGS.TESTS.DONE
	;if no readied flags for active PC detected, then next record
	LDA IN.RE.DETECT.READIED_INSTANCE.FLAG ;($00 = no active readied PC flag found active on inventory record this iteration | >=$01 = active PC readied flag found active on inventory record this iteration) 
	BEQ .NEXT_RECORD
	
.TWO_HANDED.TEST
	;(SAVING IN CASE NEEDED1) save the qty of the item in current inventory record before the 2 handed test distortes it
	;(SAVING IN CASE NEEDED1) LDA IN.RE.ITEMS_READIED_IN_HANDS.THIS_RECORD
	;(SAVING IN CASE NEEDED1) STA IN.RE.ITEMS_READIED_IN_HANDS.THIS_RECORD.QTY
	LDY #$05 ;set index to record byte $5 (equimpment slot) of inventory record
	LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y ;load inventory record equipment slot field
	BPL .TWO_HANDED.TEST.DONE
	;conflicting item is 2-handed
	;(the assumption behind this code is that if a two handed weapon is readied, is only readied once via the readied flags,
	;which means that the two counters below should = $1 at this point, where as we want them to = $2 to reflect that both hands slots are in-use with the 2-handed weapon)
	;INC IN.RE.ITEMS_READIED_IN_HANDS.THIS_RECORD	
	INC IN.RE.ITEMS_READIED_IN_HANDS.TOTAL		
	
.TWO_HANDED.TEST.DONE
	
	;**FALLS THROUGH**
@END
	;**FALLS THROUGH**

.SAVE.CONFLICT.RECORD
@START					
	;**OPT** memory. This routine and .USE.RECORD2 can probably be merged if a loop was used instead, with an index by X to the conflict array..ie. after conflict1, index = index+$5 so that it points at conflict record2
	LDA INV.RE.CONFLICT_RECORD1+$2
	BPL .USE.RECORD2
	;save inventory record as a conflict
	LDA PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
	STA INV.RE.CONFLICT_RECORD1+$0
	LDA PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
	STA INV.RE.CONFLICT_RECORD1+$1
	LDA IN.RE.ITEMS_READIED_IN_HANDS.THIS_RECORD
	STA INV.RE.CONFLICT_RECORD1+$2	;save # of times the player has this item record readied 	
	
	LDY #$00 ;set record byte $0 (Level | Item_Type)
	LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y ;load Item_Type of current inventory record
	;Item Item is in LO Nibble
	AND #$0F ;mask out HO Nibble
	STA INV.RE.CONFLICT_RECORD1+$3
	INY ;advance to record byte $1 (Item_ID)
	LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y ;load Item_ID of current inventory record
	STA INV.RE.CONFLICT_RECORD1+$4
	
	;(SAVING IN CASE NEEDED1) LDA IN.RE.ITEMS_READIED_IN_HANDS.THIS_RECORD.QTY
	;(SAVING IN CASE NEEDED1) STA INV.RE.CONFLICT_RECORD1+$5	;save qty of the current inventory record item that player has readied. 

	;dual wielding record check
	;(if item in current record is readied twice then we want the item report in both conflict records)
	LDA IN.RE.ITEMS_READIED_IN_HANDS.THIS_RECORD	
	CMP #$02
	BCS .USE.RECORD2.DUAL_WIELD.ENTRANCE
	;**FALLS THROUGH**
	JMP .NEXT_RECORD
	
.USE.RECORD2	
	LDA INV.RE.CONFLICT_RECORD2+$2
	BPL .ERROR.CONFLICT.OVERFLOW	
.USE.RECORD2.DUAL_WIELD.ENTRANCE
	;save inventory record as a conflict
	LDA PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
	STA INV.RE.CONFLICT_RECORD2+$0
	LDA PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
	STA INV.RE.CONFLICT_RECORD2+$1
	LDA IN.RE.ITEMS_READIED_IN_HANDS.THIS_RECORD
	STA INV.RE.CONFLICT_RECORD2+$2	;save # of times the player has this item record readied 

	LDY #$00 ;set record byte $0 (Level | Item_Type)
	LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y ;load Item_Type of current inventory record
	;Item Item is in LO Nibble
	AND #$0F ;mask out HO Nibble
	STA INV.RE.CONFLICT_RECORD2+$3
	INY ;advance to record byte $1 (Item_ID)
	LDA (PLAYER_MERCH.INVENTORY.DATA.POINTER),Y ;load Item_ID of current inventory record
	STA INV.RE.CONFLICT_RECORD2+$4
	
	;(SAVING IN CASE NEEDED1) LDA IN.RE.ITEMS_READIED_IN_HANDS.THIS_RECORD.QTY
	;(SAVING IN CASE NEEDED1) STA INV.RE.CONFLICT_RECORD1+$5	;save qty of the current inventory record item that player has readied. 


				
	JMP .NEXT_RECORD
@END	

.NEXT_RECORD
@START
	;reset iteration flags
	LDA #$00
	STA IN.RE.DETECT.READIED_INSTANCE.FLAG ;($00 = no active readied PC flag found active on inventory record this iteration | >=$01 = active PC readied flag found active on inventory record this iteration) 
	STA IN.RE.ITEMS_READIED_IN_HANDS.THIS_RECORD
	LDA PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
	CLC
	ADC #INV.PLAYER.INVENTORY.DATA.RECORD_SIZE
	STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
	LDA PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
	ADC /INV.PLAYER.INVENTORY.DATA.RECORD_SIZE ;16-bit add
	STA PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
.EXIT.TEST	
	CMP /INV.PLAYER.INVENTORY.DATA.PATCH_END
	BCC .LOOP.SEARCH.INVENTORY.DATA_STEP ;continue loop if end of array not reached
	BEQ .LOOP.SEARCH.INVENTORY.DATA_STEP ;continue loop if end of array not reached
	;NO CONFLICTS FOUND

	

	
	;**FALLS THROUGH**
@END	
	;**FALLS THROUGH**

.EXIT
			
	RTS
	
.LOOP.SEARCH.INVENTORY.DATA_STEP
	JMP .LOOP.SEARCH.INVENTORY.DATA

.ERROR.CONFLICT.OVERFLOW
;.SAVE.CONFLICT.RECORD (INV.SEARCH.PLY_INV.CONFLICTS.NOT_HANDS) reports both conflict records full. There should never
;be more than two record conflict records set. If there are, then that means more than 2 items are readied to "hands" equipment slots.


				
	JSR PREP.BRK
	BRK
	
@END

INV.RE.UPDATE.PROCESS_FLAGS
@START
;PARAMETERS: ITEM_TYPE
;RETURN VALUE: INV.RE.UPDATE_DAMAGE.FLAG 	;($00 = no changes made that effect DAMAGE on the character sheet. $01 changes were made that affect the same), INV.RE.UPDATE_DEFENSE.FLAG 	;($00 = no changes made that effect DAMAGE on the character sheet. $01 changes were made that affect the same)
 
;Note: this routine updates the flags which CALCULATE.STATS uses to determine whether weapon and/or armor related equipment slots have been updated
;since it was last run. This routine's Item_Type parm refers to the item-to-ready or item-to-unready

	;ACC = PARM: ITEM_TYPE
	CMP #INV.ITEM_TYPE.WEAPON
	BNE .SET.DEFENSE.FLAG
.SET.DAMAGE.FLAG	
	LDA #$01
	STA INV.RE.UPDATE_DAMAGE.FLAG 	;($00 = no changes made that effect DAMAGE on the character sheet. $01 changes were made that affect the same)
	JMP .EXIT
	
.SET.DEFENSE.FLAG
	LDA #$01
	STA INV.RE.UPDATE_DEFENSE.FLAG 	;($00 = no changes made that effect DAMAGE on the character sheet. $01 changes were made that affect the same)

	;**FALLS THROUGH**
	
.EXIT	
	RTS

@END
				
@END				
								
;LOCAL VARIABLES (INV.READY_UNREADY.EQUIPMENT.ENTRANCE)
@START

@END

