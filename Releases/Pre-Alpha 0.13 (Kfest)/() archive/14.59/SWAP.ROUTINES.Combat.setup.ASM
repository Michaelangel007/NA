;************************INCLUDE FILE*****************************
;(do not assemble stand-alone.  )
				.TF     swap.routines.combat.setup.bin,BIN
				.OR		SRTN.COMBAT.ADDRESS			**Always put before .TF directive and never use again in program
				.EB		OFF

;=====================SETUP/EXIT ROUTINES DOCUMENTATION====================================
;
;This file is loaded just before and just after COMBAT.MAIN is called. This enabled 
;certain routines to be offloaded from the main combat module into this file, which frees
;up memory in the main combat module. 
;
;
;=================================================================================

COMBAT_SE.SETUP.MAIN.START

COMBAT.ENTRANCE ;roll mob stats and copy to aux memory
COMBAT.SETUP
@START
;PARAMETERS: COMBAT_SE.MODE.PARM ($00 = player initiated | $01 = mob initiated | $FF = test mode), COMBAT_SE.MOB_TILE_ID.PARM, COMBAT_SE.TERRAIN_TILE_ID.PARM
;ENTRANCE: direct



.INIT
	;LDA #COMBAT_SE.MOB.GROUP_ID.LOOKUP.START
	LDA #$00
	STA MOB.GROUP_ID.LOOKUP.POINTER+$0
	;LDA /COMBAT_SE.MOB.GROUP_ID.LOOKUP.START
	STA MOB.GROUP_ID.LOOKUP.POINTER+$1
	
	STA COMBAT_SE.MOB_IDS.THIS_ENCOUNTER+$0 ;init first byte to $00, because the seek size is calculated like this: (current value in this array * record size) - (previous value in array * record size)
.INIT.MO_ARRAYS ;**TEMP** (I'm no longer sure why I marked this is temp. It seems to be permenantly needed)
@START
	;reset map object arrays (mob, general and NPC)
	LDA #MAP_OBJECTS.MOB
	STA FILL.START
	LDA /MAP_OBJECTS.MOB
	STA FILL.START+$1
	LDA #MAP_OBJECTS.NPC
	CLC
	ADC #$FF
	STA FILL.END
	LDA /MAP_OBJECTS.NPC
	ADC #$00	;16-bit add
	STA FILL.END+$1
	
	LDA #$00
	STA FILL.VALUE

	JSR MEMORY.FILL
@END	


	;****ENABLE THIS SECTION ONCE REGULAR COMBAT MODE IS WORKING**
	;(regular combat mode is when combat is initiated by the player attacking a mob or vice versa)
; ;MODE BRANCH

	LDA COMBAT_SE.MODE.PARM	;($00 = player initiated | $01 = mob initiated | $FF = test mode)
	CMP #$FF
	BNE .PLAYER_MOB.INITIATED_COMBAT
	
	;**FALLS THROUGH**

		;JMP .PLAYER_MOB.INITIATED_COMBAT
				
.LOAD.TEST_MODE.DATA
	JSR COMBAT_SE.TEST.MODE
	JMP COMBAT.SETUP.SESSION
	
.PLAYER_MOB.INITIATED_COMBAT
@START
			
.DETERMINE.MOB_ENCOUNTER_GROUP
	LDX #$00
.LOOP.FIND.MAP_TYPE.START
;(find the memory address that the records for the active map type start at)
	LDA COMBAT_SE.MOB.GROUP_ID.LOOKUP.START,X	
	CMP PLAYER.MAP.LOCATION_TYPE
	BEQ .LOOP.FIND.MAP_TYPE.START.DONE
	TXA
	CLC 
	ADC #COMBAT_SE.MOB.GROUP_ID.RECORD.SIZE
	TAX
	BNE .LOOP.FIND.MAP_TYPE.START
	INC MOB.GROUP_ID.LOOKUP.POINTER+$1
	JMP .LOOP.FIND.MAP_TYPE.START	
.LOOP.FIND.MAP_TYPE.START.DONE
	;close out LO byte counter to the pointer
	STX MOB.GROUP_ID.LOOKUP.POINTER+$0
	
	;add base address and pointer value
	LDA #COMBAT_SE.MOB.GROUP_ID.LOOKUP.START
	CLC
	ADC MOB.GROUP_ID.LOOKUP.POINTER+$0
	STA MOB.GROUP_ID.LOOKUP.POINTER+$0
	LDA /COMBAT_SE.MOB.GROUP_ID.LOOKUP.START
	ADC MOB.GROUP_ID.LOOKUP.POINTER+$1	;16-bit add
	STA MOB.GROUP_ID.LOOKUP.POINTER+$1	
	
	
		; LDA COMBAT_SE.MOB_TILE_ID.PARM
		; CMP #$97
		; BEQ .TEMP
		; LDA #$BB
		; JSR PREP.BRK
		; BRK
; .TEMP

			
	LDY #$00 ;init record index
.LOOP.FIND.GROUP_ID.RECORD
;***WARNING: if more than 64 mobs (64*4bytes per record) are in the same shape table
;then this routine needs to advance records via a 16-bit
;add to the pointer instead of incrementing Y-REG.
	INY ;advance index to byte $01 (mob tile_ID )
	LDA (MOB.GROUP_ID.LOOKUP.POINTER),Y
	CMP COMBAT_SE.MOB_TILE_ID.PARM
	BEQ .LOOP.FIND.GROUP_ID.RECORD.DONE
	;next record
	INY ;advance index to byte $02
	INY ;advance index to byte $03
	INY ;advance index to byte $00 of next record
	JMP .LOOP.FIND.GROUP_ID.RECORD
.LOOP.FIND.GROUP_ID.RECORD.DONE

	INY ;advance index to byte $02 (mob group_ID)
	
	LDA (MOB.GROUP_ID.LOOKUP.POINTER),Y
	STA COMBAT_SE.MOB_GROUP_TABLE.RECORD_ID


		; LDA #$01
		; sta COMBAT_SE.MOB_GROUP_TABLE.RECORD_ID
	
			; lda #$aa
			; ldx COMBAT_SE.MOB_GROUP_TABLE.RECORD_ID
			; jsr prep.brk
			; brk
			
.READ.MOB_ENCOUNTER_GROUP.RECORD
@START
	;calculate mob group table record offset
	;(also seek bytes since right now the ProRWTS seek pointer = COMBAT_SE.MOB_TABLES.START
		LDA COMBAT_SE.MOB_GROUP_TABLE.RECORD_ID
		STA MULPLR+$0
		LDA #$00
		STA MULPLR+$1
		;
		LDA #COMBAT_SE.MOB_GROUP_TABLE.RECORD.SIZE
		STA MULCND+$0
		LDA /COMBAT_SE.MOB_GROUP_TABLE.RECORD.SIZE
		STA MULCND+$1			
	JSR MLP.16.NO_BCD
		;RETURN: RESULT(2) = RECORD_ID OFFSET 

; ;*****TEMP		
	; LDA RESULT+$0
	; STA PARM.SEEK_BYTES+$0
	; LDA RESULT+$1
	; STA PARM.SEEK_BYTES+$1

	;save seek byte
	;(this value will be used in COMBAT.SE.INIT.MAP_OBJECTS.MOB.PHASE2 to calculate the location of the ProRWTS seek pointer.
	;It is saved to a different varaible because PARM.SEEK_BYTES(2) is .EQ to shape hopper and there might be code added 
	;to COMBAT.SETUP later that involves graphics. It is setup as a safety net )
	LDA RESULT+$0
	STA COMBAT_SE.GROUP_TABLE.SEEK_BYTES+$0
	LDA RESULT+$1
	STA COMBAT_SE.GROUP_TABLE.SEEK_BYTES+$1
	
	;**FALLS THROUGH**
		
;------SEEK FILE------
.SEEK.FILE
	lda #cmd_seek.current_drive
	sta parm.reqcmd

;set seek length (bytes)
	lda RESULT+$0			;seek length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda RESULT+$1			;seek length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi

		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO
			
	;**FALLS THROUGH**
	
;----READ FILE-----
.READ.FILE
	
	lda #cmd_read.current_drive
	sta parm.reqcmd

;set destination memory address
	lda #COMBAT_SE.MOB_GROUP_TABLE.RECORD.READ	
	sta parm.ldrlo
	lda /COMBAT_SE.MOB_GROUP_TABLE.RECORD.READ		
	sta parm.ldrhi
	
;set read length (bytes)
	lda #COMBAT_SE.MOB_GROUP_TABLE.RECORD.SIZE		;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda /COMBAT_SE.MOB_GROUP_TABLE.RECORD.SIZE	;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi

	;**falls through**

		
.load.file	
		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO		
		
				
		; LDA #$AA
		; JSR PREP.BRK
		; BRK

@END

			
;;TROUBLESHOOTING HOOK
;
;
			; LDX #$00
; .DEBUG.LOOP1
			; LDA COMBAT_SE.MOB_GROUP_TABLE.RECORD.READ,X
			; STA $BE00,X
			; INX
			; BNE .DEBUG.LOOP1	
			;
			;
			; LDX #$00
; .DEBUG.LOOP2
			; LDA COMBAT_SE.MOB_GROUP_TABLE.SLOT.DATA,X
			; STA $BF00,X
			; INX
			; BNE .DEBUG.LOOP2
			;
			;
			; LDA #$AA
			; JSR PREP.BRK
			; BRK

			
		
.CALCULATE.MOB.QTY
@START			
;formula: mob qty = max mobs - randomized variance
;variance = max mobs * fluctuation % (ROUND DOWN)
;randomized variance: RND.LO = $0, RND.HI = variance


		;set parms for COMBAT_SE.STATS.APPLY.PERCENTAGE.BCD
		LDA COMBAT_SE.MOB_GROUP_TABLE.RECORD.READ+$0 ;read max qty 
		STA COMBAT_SE.STATS.APPLY_PERCENT.PARM_BASE+$0
		LDA #$00
		STA COMBAT_SE.STATS.APPLY_PERCENT.PARM_BASE+$1		
		LDA COMBAT_SE.MOB_GROUP_TABLE.RECORD.READ+$1 ;read fluctuation %
		STA COMBAT_SE.STATS.PERCENT.PARM
		LDA #$01 ;set hex mode ($00 = hex mode | $01 = BCD mode)		
	JSR COMBAT_SE.STATS.APPLY.PERCENTAGE
		;RETURN VALUE: RESULT(2)	***BCD***
		;ignore HO byte RESULT+$1)
		LDA RESULT+$0 ;max possible variance	
		BEQ .NO.VARIANCE		
		;**WARNING** COMBAT_SE.RANDOMIZE.VARIANCE expects a hex value in ACC so if return value from
		;COMBAT_SE.STATS.APPLY.PERCENTAGE is > 9 then a conversion needs to be done. 
	JSR COMBAT_SE.RANDOMIZE.VARIANCE
		;ACC = randomized variance
		STA TEMP ;randomized variance
		LDA COMBAT_SE.MOB_GROUP_TABLE.RECORD.READ+$0 ;read max qty 
		SEC
		SBC TEMP ;randomized variance
		
				; LDX #$AA
				; LDY TEMP
				; ; LDX RESULT+$00
				; ; LDY RESULT+$01
				; JSR PREP.BRK
				; BRK
				
	JMP .SAVE.MOB.TOTAL
	
.NO.VARIANCE
	;set total mobs = max mob qty
	LDA COMBAT_SE.MOB_GROUP_TABLE.RECORD.READ+$0 ;read max qty 
	;**FALLS THROUGH

.SAVE.MOB.TOTAL
	STA COMBAT.MOB.START.TOTAL	
	
.CALCULATE.MOB.QTY.DONE

@END
			; LDA #$AB
			; ;LDX TEMP
			; ldy COMBAT.MOB.START.TOTAL
			; JSR PREP.BRK
			; BRK		
	

			
					; LDA #$06
					; STA COMBAT.MOB.START.TOTAL

	LDX #$01 ;init start mob number (starts with $01) 
	LDY #$00 ;init mob slot #
	
;--mob slot datagam--
;byte0: mob character sheet record ID (i.e. identifies the mob that will occupy this slot)
;byte1: screen zone #
;byte2: map object flag value

COMBAT.SE.INIT.MAP_OBJECTS.MOB
@START	
	
.INIT.MAP_OBJECTS.MOB.LOOP.PHASE1
@START		
	;calculate map object record index
	TXA ;current mob number (starts with $1)
	SEC
	SBC #$01 ;needed because map object index starts with $00
	ASL ;X2
	ASL ;X4
	ASL ;X8
	STA COMBAT_SE.MAP_OBJECT.INDEX
				
	; ;read mob group table record
		; LDA COMBAT_SE.MOB_GROUP_TABLE.RECORD_ID
		; STA COMBAT_SE.MOB_TABLE.RECORD_ID
		; LDA #$00 ;($00 = mob group | >=$01 mob character sheet)
	; JSR COMBAT_SE.READ.MOB_TABLE.FILE.ENTRANCE
		; ;RETURN: COMBAT_SE.MOB_GROUP_TABLE.RECORD.READ ($42)

			
.SET.MAP_OBJECT.FIELDS
@START	

;=====================CODE-SECTION DOCUMENTATION====================================
;
;-SCREEN ZONES
;The screen zone field in each mob slot determines the region of the screen the mob in that slot
;will be located in. 
;
;Screen Zone 0: X-axis = 1-9 | Y-axis = 3-4 
;Screen Zone 1: X-axis = 1-9 | Y-axis = 1-2
;Screen Zone 2: X-axis = 1-9 | Y-axis = 0
;
;(see 1Screen Array and Tile Documentation.XLS for visual illustration of screen zones)
;
;=================================================================================

.SET.MOB.X_Y.AXIS		
	DEX ;convert sequential MOB # (starts with $01) to table index (starts with $00)
	LDA COMBAT_SE.MOB_GROUP_TABLE.SLOT.DATA+$2,Y ;read screen zone field for this mob slot. (high bit not set = determine mob location based on zones | high bit set = determine mob location using fixed config)
	BPL .PARSE.SCREEN_ZONE ;branch
	CMP #$80
	BEQ .FIXED.CONFIG0

.ERROR.FIXED.CONFIG.VALUE
;.SET.MAP_OBJECT.FIELDS reports unexepected value in COMBAT_SE.MOB_GROUP_TABLE.SLOT.DATA zone field	


; ;TROUBLESHOOTING HOOK

			; STX TEMP
			
			; LDX #$00
; .DEBUG.LOOP1
			; LDA COMBAT_SE.MOB_GROUP_TABLE.RECORD.READ,X
			; STA $BE00,X
			; INX
			; BNE .DEBUG.LOOP1	
			
			
			; LDX #$00
; .DEBUG.LOOP2
			; LDA COMBAT_SE.MOB_GROUP_TABLE.SLOT.DATA,X
			; STA $BF00,X
			; INX
			; BNE .DEBUG.LOOP2
			
			
			; LDA #$AA
			; LDX TEMP
			; JSR PREP.BRK
			; BRK
			
	JSR PREP.BRK
	BRK
	
.FIXED.CONFIG0
	LDA COMBAT_SE.MOB_LOCATION.FIXED.CONFIG0.X,X 
	STA COMBAT_SE.MOB.X_AXIS 
	LDA COMBAT_SE.MOB_LOCATION.FIXED.CONFIG0.Y,X 
	STA COMBAT_SE.MOB.Y_AXIS

			
	INX ;undo above index conversion
	JMP .SET.MOB.X_Y.AXIS.DONE

.PARSE.SCREEN_ZONE		
@START
	INX ;undo above index conversion

.PARSE.SCREEN_ZONE.Y_AXIS
	LDA COMBAT_SE.MOB_GROUP_TABLE.SLOT.DATA+$2,Y ;get screen zone assigned to this mob slot. 
	AND #$7F ;mask-out high bit, which is used to designate a fixed config for mob locations
	BEQ .SLOT0
	CMP #$01
	BEQ .SLOT1
	;default case: #$02 = .SLOT2
	
	;**FALLS THROUGH**
	
.SLOT2
	LDA #$00
	STA COMBAT_SE.MOB.Y_AXIS 

	JMP .PARSE.SCREEN_ZONE.Y_AXIS.DONE

.SLOT1		
	JSR RANDOM.8
		;RETURN VALUE: ACC = 8-bit random number
	CMP #$80
	BCS .Y_AXIS.EQ.1
	;default case: ACC > #$80
	LDA #$02 ;load Y-AXIS value
	JMP .SLOT1.SAVE.Y_AXIS
.Y_AXIS.EQ.1
	LDA #$01 ;load Y-AXIS value
	
	;**FALLS THROUGH**
	
.SLOT1.SAVE.Y_AXIS	
	STA COMBAT_SE.MOB.Y_AXIS 

	JMP .PARSE.SCREEN_ZONE.Y_AXIS.DONE
	
.SLOT0
	JSR RANDOM.8
		;RETURN VALUE: ACC = 8-bit random number
	
	CMP #$80
	BCS .Y_AXIS.EQ.3
	;default case: ACC > #$80
	LDA #$04 ;load Y-AXIS value
	JMP .SLOT0.SAVE.Y_AXIS
.Y_AXIS.EQ.3
	LDA #$03 ;load Y-AXIS value
	
	;**FALLS THROUGH**
	
.SLOT0.SAVE.Y_AXIS	
	STA COMBAT_SE.MOB.Y_AXIS 

	;**FALLS THROUGH**
	
.PARSE.SCREEN_ZONE.Y_AXIS.DONE

.PARSE.SCREEN_ZONE.X_AXIS
;NOTE: This code produces a random number 1-8 without using RND.LO / RND.HI parms to RANDOM.8 which results
;in much faster execution. The logic is that we just scale down the 8-bit random number with the LSRs. 
;* !255 ($FF) /8 = 7.9, which means 7 will be returned since LSR effectively rounds down.
;* !32 ($10) or below will return a value less than 1. So at this point we have a random number 0-7
;* By adding 1 to the accumulator after the division is done that pushes up the random number to 1-8
	
	JSR RANDOM.8
		;RETURN VALUE: ACC = 8-bit random number
	LSR ;/2
	LSR ;/4
	LSR ;/8
	LSR ;/!16
	LSR ;/!32
	CLC
	ADC #$01
	STA COMBAT_SE.MOB.X_AXIS 
.PARSE.SCREEN_ZONE.X_AXIS.DONE
.SET.MOB.X_Y.AXIS.DONE	
@END

.GET.SLOT.FLAG_VALUE
	LDA COMBAT_SE.MOB_GROUP_TABLE.SLOT.DATA+$3,Y ;get flag value assigned to this mob slot. 
	STA COMBAT_SE.MOB.FLAG_VALUE		


			
.GET.SLOT.MOB_ID
;(each mob slot has two possible MOB_ID values. This routine chooses which one to use based on a random #)

;**OPT** Speed. test the 2nd MOB_ID at the start of this section (here) to see if high-bit is set. If it is, then skip random number and
;automatically use 1st mob ID. The table data will need to be setup to conform with this. The reason to use the 2nd MOB_ID as the one with the high-bit
;but use the 1st mOB ID as the default is to further reduce clock cycles because the 1st MOB ID won't need to have the high-bit masked-out.  

	JSR RANDOM.8
		;RETURN VALUE: ACC = 8-bit random number
	
	CMP #$80
	BCS .USE.MOB_ID.1
	;default case: ACC > #$80
	LDA COMBAT_SE.MOB_GROUP_TABLE.SLOT.DATA+$0,Y ;get 1st mob character sheet record # that is assigned to this mob slot	
	JMP .SAVE.RECORD_ID
.USE.MOB_ID.1
	LDA COMBAT_SE.MOB_GROUP_TABLE.SLOT.DATA+$1,Y ;get 2nd mob character sheet record # that is assigned to this mob slot	
	
	;**FALLS THROUGH**
	
.SAVE.RECORD_ID
	;STA COMBAT_SE.MOB_TABLE.RECORD_ID
	STA COMBAT_SE.MOB_IDS.THIS_ENCOUNTER,X ;save the MOB_ID for the current mob to the holding array for further processing.

				
		TXA
		PHA	 ;push mob seqential # counter to stack
;set map object data: flag value
	LDX COMBAT_SE.MAP_OBJECT.INDEX
	LDA COMBAT_SE.MOB.FLAG_VALUE ;load mob flag value from mob group record table (holding variable)
	STA COMBAT.MAP_OBJECTS.MOB+$3,X ;save mob flag value to map object record

;set map object data: alignment, enemy type code, health status
	LDA #COMBAT.ALIGNMENT.MOB
	STA COMBAT.MAP_OBJECTS.MOB+$4,X ;save alignment value to map object record

	LDA #COMBAT.ENEMY_TYPE.PC
	STA COMBAT.MAP_OBJECTS.MOB+$5,X ;save enemy type value to map object record
	
	LDA #COMBAT.S_ENTITY.STATUS.GOOD
	STA COMBAT.MAP_OBJECTS.MOB+$7,X ;save health status type value to map object record
	

;set map object data: X/Y-AXIS
	LDA COMBAT_SE.MOB.X_AXIS
	STA COMBAT.MAP_OBJECTS.MOB+$0,X 
	LDA COMBAT_SE.MOB.Y_AXIS
	STA COMBAT.MAP_OBJECTS.MOB+$1,X 

	PLA ;pull mob seqential # counter from stack
	TAX
@END
	
				
	;exit test
	CPX COMBAT.MOB.START.TOTAL
	BEQ .INIT.MAP_OBJECTS.MOB.LOOP.PHASE1.COMPLETE
	INX	;next mob	
	
	;next mob slot
	TYA
	CLC
	ADC #COMBAT_SE.MOB_SLOT.RECORD.SIZE ;increment mob slot index
	TAY

	
			; LDA #$AA
			; ;LDX COMBAT_SE.MOB_TILE_ID.PARM
			; JSR PREP.BRK
			; BRK
			

			; STA TEMP
			; ; LDA TROUBLESHOOTING.HOOK
			; ; CMP #$01
			; CPX #$02
			; BNE .TEMP
			; LDA #$AA
			; ;LDX COMBAT_SE.MOB_TILE_ID.PARM
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP

		
		; lda #$01
		; sta TROUBLESHOOTING.HOOK

	JMP .INIT.MAP_OBJECTS.MOB.LOOP.PHASE1
	
	
.INIT.MAP_OBJECTS.MOB.LOOP.PHASE1.COMPLETE	
	
	
	
.INIT.MAP_OBJECTS.MOB.LOOP.COMPLETE

			; LDA #$AA
			; LDX #COMBAT_SE.MOB_IDS.THIS_ENCOUNTER
			; LDY /COMBAT_SE.MOB_IDS.THIS_ENCOUNTER
			; JSR PREP.BRK
			; BRK
			
			
		; LDA #$AC
		; JSR PREP.BRK
		; BRK
		
@END


COMBAT.SE.INIT.MAP_OBJECTS.MOB.PHASE2
@START
;=====================CODE-SECTION DOCUMENTATION====================================
;
;This routine (phase2) does a series of seek/reads to the mob tables file (DATA.GME.MOB_TABLES)
;in order to load the character sheet information for each mob previous created in phase1. 
;The file (DATA.GME.MOB_TABLES) was opened in phase1 so to save significant disk load time, phase2
;will just do seek/reads for each MOB_ID stored in the COMBAT_SE.MOB_IDS.THIS_ENCOUNTER array.
;
;Doing this requires taking into consideration the location of the seek pointer as of the
;end of phase1.
;
;=================================================================================

.INIT

	;SORT MOB_ID LIST
	
	;expand record size to $2 bytes and insert the MOB sequential # as the 2nd byte in the record. 
	LDX #$01 ;start with byte $01 because the index is the mob seqential #. Byte $00 is filler.  
	LDY #$02 ;start with byte $02 because the first two bytes of the sorted array are reserved for the 16-bit array length (required by INSERT.SORT)
.LOOP.EXPAND.RECORD_SIZE
	;**OPT** Memory. Speed. This loop wouldn't be needed if phase 1 (above) wrote 2 byte records to COMBAT_SE.MOB_IDS.THIS_ENCOUNTER with the 2nd byte being the mob sequential number. 
	LDA COMBAT_SE.MOB_IDS.THIS_ENCOUNTER,X
	STA COMBAT_SE.MOB_IDS.THIS_ENCOUNTER.SORTED,Y
	INY ;next sorted array byte
	TXA ;set value for 2nd byte in record (mob sequential #)
	STA COMBAT_SE.MOB_IDS.THIS_ENCOUNTER.SORTED,Y
	CPX COMBAT.MOB.START.TOTAL
	BEQ .LOOP.EXPAND.RECORD_SIZE.DONE
	INX ;next encounter array byte
	INY ;next sorted array byte
	JMP .LOOP.EXPAND.RECORD_SIZE
.LOOP.EXPAND.RECORD_SIZE.DONE
	;save size to the first byte of array
	;(required by INSERTION_SORT)
	LDA COMBAT.MOB.START.TOTAL
	ASL ;X2
	STA COMBAT_SE.MOB_IDS.THIS_ENCOUNTER.SORTED+$0
	LDA #$00
	STA COMBAT_SE.MOB_IDS.THIS_ENCOUNTER.SORTED+$1

	
		;setup record size
		LDA #$02
		STA array.record_size			
		;setup pointer
		LDA #COMBAT_SE.MOB_IDS.THIS_ENCOUNTER.SORTED		
		STA sort.table.address
		LDX /COMBAT_SE.MOB_IDS.THIS_ENCOUNTER.SORTED	
		STX sort.table.address+$1
	JSR INSERTION_SORT
	
	
	LDA #$00
	STA COMBAT_SE.MOB_IDS.THIS_ENCOUNTER.SORTED+$0

	
		; LDX #$00
; .TEST_LOOP
		; LDA COMBAT_SE.MOB_IDS.THIS_ENCOUNTER.SORTED,X
		; STA $BE00,X
		; INX
		; BNE .TEST_LOOP
		
		
			; LDA #$AA
			; LDX #COMBAT_SE.MOB_IDS.THIS_ENCOUNTER
			; LDY /COMBAT_SE.MOB_IDS.THIS_ENCOUNTER
			; JSR PREP.BRK
			; BRK
			
			

	;CALCUALTE ONE-TIME ADJUSTMENT TO ACCOUNT FOR SEEK POINTER AS OF END OF PHASE1
	;(formula: table offset - group table seek pointer - group table record size)
	;(the reason for subtracting the group table record size is because the prorwts seek pointer
	;will be at the byte after the last byte read, but the group table seek pointer will not reflect 
	;the last read.)
	LDA #COMBAT_SE.MOB_CHAR_SHEET_TABLE.OFFSET
	SEC
	SBC COMBAT_SE.GROUP_TABLE.SEEK_BYTES+$0
	STA COMBAT_SE.ONE_TIME_ADJUSTMENT.SEEK_BYTES+$0
	LDA /COMBAT_SE.MOB_CHAR_SHEET_TABLE.OFFSET
	SBC COMBAT_SE.GROUP_TABLE.SEEK_BYTES+$1 ;16-bit subtract
	STA COMBAT_SE.ONE_TIME_ADJUSTMENT.SEEK_BYTES+$1

	;subtract group table record size because last read isn't reflected in seek bytes total	
	LDA COMBAT_SE.ONE_TIME_ADJUSTMENT.SEEK_BYTES+$0
	SEC
	SBC #COMBAT_SE.MOB_GROUP_TABLE.RECORD.SIZE
	STA COMBAT_SE.ONE_TIME_ADJUSTMENT.SEEK_BYTES+$0
	LDA COMBAT_SE.ONE_TIME_ADJUSTMENT.SEEK_BYTES+$1
	SBC /COMBAT_SE.MOB_GROUP_TABLE.RECORD.SIZE ;16-bit subtract
	STA COMBAT_SE.ONE_TIME_ADJUSTMENT.SEEK_BYTES+$1


;!!!WARNING: don't try to consolidate the 16-bit subtracts above. In any order of operations there is a potential for
;an underflow on the first SBC.
	
;TROUBLESHOOTING HOOK: CHECK CALCULATION	
			; LDA #COMBAT_SE.MOB_CHAR_SHEET_TABLE.OFFSET
			; STA $BE00
			; LDA /COMBAT_SE.MOB_CHAR_SHEET_TABLE.OFFSET
			; STA $BE01
			; LDA COMBAT_SE.GROUP_TABLE.SEEK_BYTES+$0
			; STA $BE02
			; LDA COMBAT_SE.GROUP_TABLE.SEEK_BYTES+$1
			; STA $BE03
			; LDA #COMBAT_SE.MOB_GROUP_TABLE.RECORD.SIZE
			; STA $BE04
			; LDA /COMBAT_SE.MOB_GROUP_TABLE.RECORD.SIZE
			; STA $BE05
			; LDA COMBAT_SE.ONE_TIME_ADJUSTMENT.SEEK_BYTES+$0
			; STA $BE06
			; LDA COMBAT_SE.ONE_TIME_ADJUSTMENT.SEEK_BYTES+$1
			; STA $BE07
			
			; LDA #$AA
			; LDX COMBAT_SE.GROUP_TABLE.SEEK_BYTES+$0
			; LDY COMBAT_SE.GROUP_TABLE.SEEK_BYTES+$1
			; JSR PREP.BRK
			; BRK
			

			
			
	;**FALLS THROUGH**
	
	LDX #$02 ;init COMBAT_SE.MOB_IDS.THIS_ENCOUNTER index 
.COMBAT.SE.INIT.MAP_OBJECTS.MOB.LOOP.PHASE2
		
					
.SET.MAP_OBJECT.FIELDS
@START	

.CALCULATE.SEEK_BYTES 
@START
;(from last MOB character sheet record to next MOB character sheet record)
;(seek bytes formula: (current MOD_ID * record size) - (previous MOD_ID * record size))

			; STA TEMP
			; ; LDA TROUBLESHOOTING.HOOK
			; ; CMP #$01
			; CPX #$02
			; BNE .TEMP
			; LDA COMBAT_SE.MOB_IDS.THIS_ENCOUNTER,X ;load MOB_ID for the current MOB
			; LDY #$AA
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP
			


			
	;calculate map object record index
	LDA COMBAT_SE.MOB_IDS.THIS_ENCOUNTER.SORTED+$1,X ;load mob sequential # from current record
	PHA ;push MOB sequential # to stack

	SEC
	SBC #$01 ;needed because map object index starts with $00
	ASL ;X2
	ASL ;X4
	ASL ;X8
	STA COMBAT_SE.MAP_OBJECT.INDEX

	;calculate offset to next mob character sheet record
		SEC ;set for both SBCs below. Set here so only one opcode is needed. 
		LDA COMBAT_SE.MOB_IDS.THIS_ENCOUNTER.SORTED,X ;load MOB_ID for the current MOB
		CPX #$02	
		BEQ .CORNER_CASE.FIRST_MOB_RECORD_PROCESSED ;branch if yes
		;is current mob_ID the same as prior mob_ID?
		STA TEMP ;save ACC
		SBC COMBAT_SE.MOB_IDS.THIS_ENCOUNTER.SORTED-$2,X ;load MOB_ID for the previous MOB
		BEQ .LOAD.CHR_SHEET.DATA.DONE ;branch if current mob_ID is the same as prior mob_ID. This results in the file load being skipped, leaving the character sheet data for the prior mob_ID in the read buffer
		LDA TEMP ;restore ACC
		SBC #$1 ;if MOB_ID != $00, subtract $01 so that next record seek bytes will be reduced by the amount of the record size.
				 ;This is because the ProRWTS seek pointer will already have advanced by that amount from doing the last read. 
				 ;The reason for the corner case instead of just ommitting the deduction of the record size in the .INIT calculations
				 ;and deducting $1 from COMBAT_SE.MOB_IDS.THIS_ENCOUNTER,X always is because if the first MOB ID is $00 
				 ;then there are no bytes to implicitly deduct by reducing COMBAT_SE.MOB_IDS.THIS_ENCOUNTER,X by $1; i.e. an underflow would result. 
.CORNER_CASE.FIRST_MOB_RECORD_PROCESSED		
		SBC COMBAT_SE.MOB_IDS.THIS_ENCOUNTER.SORTED-$2,X ;load MOB_ID for the previous MOB
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
	
			
	;calculate next seek bytes value
	LDA RESULT+$0 ;load next record seek bytes (LO)

	CLC
	ADC COMBAT_SE.ONE_TIME_ADJUSTMENT.SEEK_BYTES+$0
	STA COMBAT_SE.CHAR_SHEET.NEXT_RECORD.SEEK_BYTES+$0
	LDA RESULT+$1 ;load next record seek bytes (HO)
	ADC COMBAT_SE.ONE_TIME_ADJUSTMENT.SEEK_BYTES+$1 ;16-bit add
	STA COMBAT_SE.CHAR_SHEET.NEXT_RECORD.SEEK_BYTES+$1		


;TROUBLESHOOTING HOOK: CHECK CALCULATION	
			; LDA COMBAT_SE.MOB_IDS.THIS_ENCOUNTER.SORTED,X ;load MOB_ID for the current MOB
			; STA $BE00
			; LDA RESULT+$0 ;load current seek bytes (LO)
			; STA $BE01
			; LDA RESULT+$1 ;load current seek bytes (LO)
			; STA $BE02
			; LDA COMBAT_SE.ONE_TIME_ADJUSTMENT.SEEK_BYTES+$0
			; STA $BE03
			; LDA COMBAT_SE.ONE_TIME_ADJUSTMENT.SEEK_BYTES+$1
			; STA $BE04					
			; LDA COMBAT_SE.CHAR_SHEET.NEXT_RECORD.SEEK_BYTES+$0
			; STA $BE05
			; LDA COMBAT_SE.CHAR_SHEET.NEXT_RECORD.SEEK_BYTES+$1
			; STA $BE06
			; LDA COMBAT_SE.MOB_GROUP_TABLE.RECORD_ID
			; STA $BE07
			;
			; LDA #$AA
			; ; LDX COMBAT_SE.CHAR_SHEET.NEXT_RECORD.SEEK_BYTES+$0
			; ; LDY COMBAT_SE.CHAR_SHEET.NEXT_RECORD.SEEK_BYTES+$1
			; JSR PREP.BRK
			; BRK
			

	;set to $00 for future iterations 
	LDA #$00
	STA COMBAT_SE.ONE_TIME_ADJUSTMENT.SEEK_BYTES+$0
	STA COMBAT_SE.ONE_TIME_ADJUSTMENT.SEEK_BYTES+$1
			
@END
		

.LOAD.CHR_SHEET.DATA ;(load character sheet data from file)
@START
;------SEEK FILE------
.SEEK.FILE
	lda #cmd_seek.current_drive
	sta parm.reqcmd

;set seek length (bytes)
	lda COMBAT_SE.CHAR_SHEET.NEXT_RECORD.SEEK_BYTES+$0	;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda COMBAT_SE.CHAR_SHEET.NEXT_RECORD.SEEK_BYTES+$1	;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi
	
		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO
			
	;**FALLS THROUGH**
	
;----READ FILE-----
.READ.FILE
	
	lda #cmd_read.current_drive
	sta parm.reqcmd
	
;set destination memory address
	lda #CHR_SHEET.RECORD.READ
	sta parm.ldrlo
	lda /CHR_SHEET.RECORD.READ
	sta parm.ldrhi
	
;set read length (bytes)
	lda #COMBAT_SE.MOB_CHAR_SHEET_TABLE.RECORD.SIZE	;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda /COMBAT_SE.MOB_CHAR_SHEET_TABLE.RECORD.SIZE	;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi

	;**falls through**	
			
.load.file	
		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO	

.LOAD.CHR_SHEET.DATA.DONE
@END
			
;set map object data: tile_id
	LDA CHR_SHEET.MOB.TILE_ID ;load mob tile_ID from master mob character sheet table
	LDY COMBAT_SE.MAP_OBJECT.INDEX
	STA COMBAT.MAP_OBJECTS.MOB+$2,Y ;save mob tile_id to map object record

@END
	
.WRITE.MOB.CHAR_SHEET
	;write MOB character sheet data	
		PLA ;parm: set ACC to MOB sequential #	
		ORA #$80 ;set high-bit to turn on write mode
		;CHR_SHEET.RECORD.READ($80): already set above
		;ACC = MOB sequential # (high-bit not set = read mode, high-bit set = write mode)
	JSR COMBAT.READ_WRITE.CHR_SHEET.MOB
		
	;exit test
	TXA 
	LSR ;/2 (converts loop counter to mob sequential #)
	CMP COMBAT.MOB.START.TOTAL
	BEQ .INIT.MAP_OBJECTS.MOB.LOOP.PHASE2.COMPLETE
	INX	;next current encounter record
	INX	;"" (2 byte record: mob_ID, mob sequential #)

	JMP .COMBAT.SE.INIT.MAP_OBJECTS.MOB.LOOP.PHASE2
	
	
.INIT.MAP_OBJECTS.MOB.LOOP.PHASE2.COMPLETE	
	
.INIT.MAP_OBJECTS.MOB.LOOP.COMPLETE

		
@END
@END

	;**FALLS THROUGH**		
@END
	;**FALLS THROUGH**		
	
;TROUBLESHOOTING HOOK: check character sheet data in memory, written by routines above
	;;read MOB character sheet data
		;
		; LDA #$01 ;MOB sequential # (high-bit not set = read mode, high-bit set = write mode)
	; JSR COMBAT.READ_WRITE.CHR_SHEET.MOB
	;
			; STX TEMP
			; LDX #$00
; .TEST.LOOP
			; LDA CHR_SHEET.RECORD.READ,X
			; STA $BE00,X
			; INX
			; BNE .TEST.LOOP
		;	
		; LDA #$AC
		; ; LDX #COMBAT_SE.MOB_IDS.THIS_ENCOUNTER ;load MOB_ID for the current MOB
		; ; LDY /COMBAT_SE.MOB_IDS.THIS_ENCOUNTER ;load MOB_ID for the current MOB
		; LDX TEMP
		; LDY COMBAT.MOB.START.TOTAL
		; JSR PREP.BRK
		; BRK
	;
	; JSR TEST.ENTRANCE
	; JSR COMBAT_SE.TEST.MODE
	
COMBAT.SETUP.SESSION
@START
;SAVE REGISTERES
	TXA
	PHA
	TYA
	PHA
		

		
.MISC
@START


				; LDA #$00
				; STA BCD+$0
				; STA BCD+$1
				; LDA #$88
				; STA TROUBLESHOOTING.HOOK
			; JSR PRINT.BCD_PACKED
			
.LOAD.WEAPON_PROJECTILE.SHAPE_TABLES
@START

;***see .LOAD.WEAPONS.SHAPE_TABLES (COMBAT.MAIN)
			
;it had to be moved because it is a subroutine in the 
;main combat module so it's not loaded into memory yet.
@END
		

.INIT.MAP_OBJECTS.PC
@START	
;TEMP: LOAD PC MAP OBJECT DATA INTO MAP OBECT ARRAY
;(production: determine starting PC locations, load the tile_type
;from the character sheet (always in aux memory) and load them into the PC array)

		LDA #COMBAT_LOADER.MAP_OBJECT.PC.START
		STA COPY.FROM_START
		LDA /COMBAT_LOADER.MAP_OBJECT.PC.START
		STA COPY.FROM_START+$1
		LDA #COMBAT_LOADER.MAP_OBJECT.PC.END
		STA COPY.FROM_END
		LDA /COMBAT_LOADER.MAP_OBJECT.PC.END
		STA COPY.FROM_END+$1
			
		LDA #COMBAT.MAP_OBJECTS.PC
		STA COPY.TO
		LDA /COMBAT.MAP_OBJECTS.PC
		STA COPY.TO+$1
		
		JSR MEMORY.COPY	

; ;**moved to .INIT.PARTY.SIZE (LOADER.P) so that CALCULATE.COMBAT.STATS.GENERAL_ENTRANCE can be called to setup the player character sheets	
; ;tally the number of PCs active
; ;(this routine iterates through the COMBAT.MAP_OBJECTS.PC, until the stop value ($00) is found in byte $02 (tile_type))
;
	; LDX #$00
	; LDY #$00
; .COUNT.PC.LOOP
	; LDA COMBAT.MAP_OBJECTS.PC+$2,X
	; BEQ .COUNT.PC.LOOP.COMPLETE
	; INY ;increment mob tally
;	
	; ;increment map object record index
	; TXA
	; CLC
	; ADC #COMBAT.MAP_OBJECTS.PC.RECORD_SIZE
	; TAX
	; ;exit test
	; BNE .COUNT.PC.LOOP
; .COUNT.PC.LOOP.COMPLETE
	; STY PARTY.TOTAL.PC
	

;
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
	
	LDA CHR_SHEET.PC.HEALTH_STATUS	
	STA COMBAT.MAP_OBJECTS.PC+$3,Y
	
	;exit test
	CPX PARTY.TOTAL.PC
	BEQ .LOOP.INIT.MAP_OBJECTS.PC.DONE
	INX	
	JMP .LOOP.INIT.MAP_OBJECTS.PC
.LOOP.INIT.MAP_OBJECTS.PC.DONE

@END

.SET.CHR_SHEET.SPECIAL.STOP_VALUE
@START
;(set stop value into 1st byte of special character sheet
;located in aux bsr memory. This is done so that the 1st empty record can be found)

			
;SPECIAL

	;ENABLE BANK-SWITCHED RAM ($D000 Bank 1)
	LDA $C08B 
	LDA $C08B
						
		TSX			;transfer stack pointer to X-REG
		STX TEMP	;save stack pointer
	STA $C009 ;enable aux zero-page & aux BSR 


			LDA #CHR_SHEET.SPECIAL.AUX_MEMORY.START
			STA FILL.START
			LDA /CHR_SHEET.SPECIAL.AUX_MEMORY.START
			STA FILL.START+$1
			LDA #CHR_SHEET.SPECIAL.AUX_MEMORY.END
			STA FILL.END
			LDA /CHR_SHEET.SPECIAL.AUX_MEMORY.END
			STA FILL.END+$1
			
			LDA #CHR_SHEET.SPECIAL.STOP_VALUE
			STA FILL.VALUE
		
		JSR MEMORY.FILL

	; LDA #CHR_SHEET.SPECIAL.STOP_VALUE
	; STA CHR_SHEET.SPECIAL.AUX_MEMORY.START
	
		
	
;RESTORE MEMORY STATE
	STA $C008 ;enable main zero-page & main BSR 
		LDX TEMP	;restore stack pointer to X-REG
		TXS ;transfer X-REG to stack pointer

	;RE-ENABLE BANK-SWITCHED RAM ($D000 Bank 2)
	LDA $C083				;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK2 2nd)
	LDA $C083
	
@END	
	
@END

	;**FALLS THROUGH**
			
.INIT.COMBAT.SESSION
@START

		
.INIT.MODULE
	; LDA #SRTN.MODULE_CODE.COMBAT	;save combat module code to SRTN.MODULE.LOADED 
	; STA SRTN.MODULE.LOADED 			;lets other subroutines know that the combat module is loaded

.INIT.COMBAT_SESSION
	;init stats display
	LDA #$00
	STA COMBAT.FIRST_PC_TURN.FLAG ;set here to non-zero value because it is convenient. ;($00 = first PC turn | >=$01 not first PC turn)
	STA COMBAT.ABORT.FLAG		;($00 = not set, $01 = set)

	LDA #$01
	STA SELECT_TARGET.PC_VALID.FLAG ;($00 = off, PCs not valid targets | $01 = on, PCs are valid targets)
	
.INIT.SCREEN
@START
.CONNECT.POINTER
;MAP TYPE/CODE BRANCH
	LDA COMBAT_SE.TERRAIN_TILE_ID.PARM ;load TILE_ID of the terrain tile player was standing on when combat was initiated
	LDX PLAYER.MAP.LOCATION_TYPE	;load map type of player's current map
	CPX #MAP.TYPE.SURFACE
	BEQ .SURFACE.ACTIVE
	CPX #MAP.TYPE.UNDERMAP
	BEQ .UNDERMAP.ACTIVE
	; CPX #MAP.TYPE.TOWN_VILLAGE
	; BEQ .BUILDING.ACTIVE
	
	CPX #MAP.TYPE.BUILDING.GRE 	;is map type = building?
	BCC .MAP.TYPE_CHECK.DONE		;if no
	CPX #MAP.TYPE.BUILDING.LT	;is map type = building?
	BCS .MAP.TYPE_CHECK.DONE		;if no
	JMP .BUILDING.ACTIVE			;if yes	
.MAP.TYPE_CHECK.DONE	
	
.ERROR.MAP_TYPE
;.INIT.SCREEN (COMBAT.SETUP.MISC) reports unexpected value in PLAYER.MAP.LOCATION_TYPE 
	JSR PREP.BRK
	BRK
	
.SURFACE.ACTIVE
	;ACC = COMBAT_SE.TERRAIN_TILE_ID.PARM
	CMP #TILE_ID.HILLS ;TILE_ID of the terrain tile player was standing on when combat was initiated
	BEQ .BATTLEFIELD_0.HILLS
	CMP #TILE_ID.TREES.GRE
	BCC .TEST2
	CMP #TILE_ID.TREES.LT
	BCC .BATTLEFIELD_3.FOREST
.TEST2
	CMP #TILE_ID.SHORELINE.GRE
	BCC .DEFAULT
	CMP #TILE_ID.SHORELINE.LT
	BCC .BATTLEFIELD_5.SHORELINE	
.DEFAULT

;USE DEFAULT SURFACE COMBAT MAP LAYOUT
	LDA #BATTLEFIELD_4.SURFACE_DEFAULT
	STA COMBAT.BATTLEFILED.POINTER+$0
	LDA /BATTLEFIELD_4.SURFACE_DEFAULT
	STA COMBAT.BATTLEFILED.POINTER+$1
	JMP .CONNECT.POINTER.DONE
	
.BATTLEFIELD_0.HILLS
	LDA #BATTLEFIELD_0.HILLS
	STA COMBAT.BATTLEFILED.POINTER+$0
	LDA /BATTLEFIELD_0.HILLS
	STA COMBAT.BATTLEFILED.POINTER+$1
	JMP .CONNECT.POINTER.DONE
	
.BATTLEFIELD_3.FOREST
	LDA #BATTLEFIELD_3.FOREST
	STA COMBAT.BATTLEFILED.POINTER+$0
	LDA /BATTLEFIELD_3.FOREST
	STA COMBAT.BATTLEFILED.POINTER+$1
	JMP .CONNECT.POINTER.DONE
	
.BATTLEFIELD_5.SHORELINE
	LDA #BATTLEFIELD_5.SHORELINE
	STA COMBAT.BATTLEFILED.POINTER+$0
	LDA /BATTLEFIELD_5.SHORELINE
	STA COMBAT.BATTLEFILED.POINTER+$1
	JMP .CONNECT.POINTER.DONE
	
.UNDERMAP.ACTIVE
	;ACC = COMBAT_SE.TERRAIN_TILE_ID.PARM
	CMP #TILE_ID.SPIDER_WEB ;TILE_ID of the terrain tile player was standing on when combat was initiated
	BEQ .BATTLEFIELD_1.SPIDER_WEBS

;USE DEFAULT UNDERMAP COMBAT MAP LAYOUT
	LDA #BATTLEFIELD_2.UNDERMAP_DEFAULT
	STA COMBAT.BATTLEFILED.POINTER+$0
	LDA /BATTLEFIELD_2.UNDERMAP_DEFAULT
	STA COMBAT.BATTLEFILED.POINTER+$1
	JMP .CONNECT.POINTER.DONE

.BATTLEFIELD_1.SPIDER_WEBS 
	LDA #BATTLEFIELD_1.SPIDER_WEBS 
	STA COMBAT.BATTLEFILED.POINTER+$0
	LDA /BATTLEFIELD_1.SPIDER_WEBS 
	STA COMBAT.BATTLEFILED.POINTER+$1
	JMP .CONNECT.POINTER.DONE
	
	
.BUILDING.ACTIVE	
	; LDA COMBAT_SE.TERRAIN_TILE_ID.PARM ;load TILE_ID of the terrain tile player was standing on when combat was initiated?
	; CMP 

	
;***TEMP** USE DEFAULT SURFACE COMBAT MAP LAYOUT
	LDA #BATTLEFIELD_4.SURFACE_DEFAULT
	STA COMBAT.BATTLEFILED.POINTER+$0
	LDA /BATTLEFIELD_4.SURFACE_DEFAULT
	STA COMBAT.BATTLEFILED.POINTER+$1
	
	;**FALLS THROUGH**
	
.CONNECT.POINTER.DONE

.SET.BATTLEFIELD
;init combat screen to specified array
	LDY #$00
.LOOP.SET.BATTLEFIELD
	LDA (COMBAT.BATTLEFILED.POINTER),Y
	STA SCREEN.TILE.DATA,Y
	CPY #SCREEN.ARRAY.LAST_ELEMENT
	BEQ .LOOP.SET.BATTLEFIELD.DONE
	INY
	JMP .LOOP.SET.BATTLEFIELD
.LOOP.SET.BATTLEFIELD.DONE
	
@END

	
.INIT.SAVE.GAME_VARIABLES
	;note: this is done so that the map variables can be modified to trick the game engine
	;into thinking that the player is in the upper left corner of the world map (with different tile data for the combat screen.
	;As a result, that postion, column/row is the same as GMAP.X/Y

	LDX #$00
.RESTORE.LOOP
;this loop does the equivilent of the following: 
	; LDA GMAP.X
	; STA COMBAT.GMAP.X.SAVED
	;
	; LDA GMAP.Y
	; STA COMBAT.GMAP.Y.SAVED
	;		
	; LDA GMAP.X.LAST
	; STA COMBAT.GMAP.X.LAST.SAVED
	;
	; LDA GMAP.Y.LAST
	; STA COMBAT.GMAP.Y.LAST.SAVED
	;
	; LDA RMAP.X
	; STA COMBAT.RMAP.X.SAVED
	;
	; LDA RMAP.Y
	; STA COMBAT.RMAP.Y.SAVED
	;
	; LDA RMAP+$0
	; STA COMBAT.RMAP.SAVED+$0
	; LDA RMAP+$1
	; STA COMBAT.RMAP.SAVED+$1
	;
	; LDA PLAYER.WMAP.ZONE
	; STA COMBAT.WZONE.SAVED
	
	LDA GMAP.X,X
	STA COMBAT.GMAP.X.SAVED,X
	INX
	CPX #$9
	BNE .RESTORE.LOOP
	
	
	;save player's location type before entering combat
	LDA PLAYER.MAP.LOCATION_TYPE
	STA COMBAT.PLAYER.MAP.LOCATION_TYPE.SAVED
		;STA COW2+$4

	;save sunlight status
	LDA TIME.SUN.STATUS
	STA COMBAT.TIME.SUN.STATUS.SAVED
		;STA COW2+$5
	
.INIT.SET.GAME_VARIABLES
	;set map type to combat
	;(so other routines can deterine that the combat module is running)
	LDA #MAP.TYPE.COMBAT
	STA PLAYER.MAP.LOCATION_TYPE
	
	;set GMAP to align with combat screen
	;note: tricks game engine routines like map_objects.manager.asm into thinking that the combat screen is a regular map screen in the upper left corner of the map. 	
	LDA #SCREEN.COLUMN_CENTER
	STA GMAP.X
	STA GMAP.X.LAST
	STA PARM.GMAP.X
		
	LDA #SCREEN.ROW_CENTER
	STA GMAP.Y
	STA GMAP.Y.LAST
	STA PARM.GMAP.Y
	
	;set sunlight status to daytime
	LDA #$01
	STA TIME.SUN.STATUS
		
	;calculate RMAP
	;note: this is always done when changing the gmap.x/y of the player. the combat screen is a special case and it may not be needed since the screen is fixed on a specific map location; the map isn't moving.
	;**opt** speed. Memory
	JSR MAP.UPDATE.POSITION ;set RMAP.X/Y, RMAP(2) and SS Flags
	
	
.INIT.S_ENTITIES

; ;TEMP: set 
	; LDA #COMBAT.MAX.PC
	; STA COMBAT.PC.MOVE.ELIGIBLE
; ;---

	;other S_ENTITY varibles
	LDA #$01 
	STA COMBAT.PC.ACTIVE	;set player #1 (note: doesn't start with $00 for the 1st PC)
	STA COMBAT.MOB.ACTIVE ;set MOB #1 (note: doesn't start with $00 for the 1st MOB)
	STA COMBAT.SPECIAL.ACTIVE ;set SPECIAL #1 (note: doesn't start with $00 for the 1st SPECIAL S_ENTITY)
	
	LDA #COMBAT.TURN_STATUS.PC	;PCs get first turn
	STA COMBAT.TURN_STATUS ;$00 = PCs turn. $01 = Special(s) S_ENTITY, $02 = MOBs turn
	;ACC = $00
	STA COMBAT.MOB.ACTIVE.RECORD
	STA COMBAT.PC.ACTIVE.RECORD ;set record # to player 1 (note: unlike COMBAT.PC.ACTIVE, this variable does start with $00, then incremented by the number of bytes in the record)
	STA COMBAT.PC.ACTIVE.LOCK ;set active player lock = off
	STA COMBAT.SPECIAL.ACTIVE.RECORD
	STA COMBAT.SPECIAL.START.TOTAL
	
	LDA #$04 ;pass
	STA PLAYER.MOVE.CURRENT ;set player move to pass, because in combat mode we communicate the player character moves via byte $03 on their array record. Otherwise MO.DRAW would modify the position of all other map_objects based on the value in PLAYER.MOVE.CURRENT

;--

.INIT.PC.S_ENTITIES
		
	;init last target array for select target subroutine
	LDX #$00
.LOOP4	
	LDA #SELECT_TARGET.LAST_TARGET.INIT_VALUE
	STA SELECT_TARGET.LAST_TARGET.DB,X
			
	INX
	CPX #SELECT_TARGET.LAST_TARGET.SIZE
	BNE .LOOP4
	


				
.INIT.SPECIAL.S_ENTITIES	
	;Is player riding a Wyvern Ally?
	LDA PLAYER.TILE.ACTIVE
	CMP #TILE_ID.WYVERN
	BNE COMBAT.INIT.DRAW.SCREEN


					; PLA
					; TAX
					; PLA
					; TAY
					; LDA #$AA
				; JSR PREP.BRK
				; BRK
				
.INIT.SPECIAL.DATA
@START	
	;Init Tile_TYPE
	
	;ACC = PLAYER.TILE.ACTIVE (always set to #TILE_ID.WYVERN if the branch above was taken)
	STA COMBAT.MAP_OBJECTS.SPECIAL+$2
	
	;Init Total Specials
	LDA #$01
	STA COMBAT.SPECIAL.START.TOTAL ;the number of specials at the start of combat	

	;Init GMAP.X/Y
	LDA #$00
	STA COMBAT.MAP_OBJECTS.SPECIAL+$0
	LDA #$04
	STA COMBAT.MAP_OBJECTS.SPECIAL+$1
	
	;closest to mob #1
	; LDA #$04
	; STA COMBAT.MAP_OBJECTS.SPECIAL+$0
	; LDA #$07
	; STA COMBAT.MAP_OBJECTS.SPECIAL+$1
	
	;standard location
	; LDA #$05
	; STA COMBAT.MAP_OBJECTS.SPECIAL+$0
	; LDA #$09
	; STA COMBAT.MAP_OBJECTS.SPECIAL+$1
	
	;Init Flags (default $00)
	LDA #$01 ;flags ;set aggressive flag
	STA COMBAT.MAP_OBJECTS.SPECIAL+$3
	; LDA #$09 ;flags
	; STA COMBAT.MAP_OBJECTS.SPECIAL+$3
	
	;Init Alignment	
		;LDA #$01 ;mob aligned
	LDA #$03 ;pc aligned
	STA COMBAT.MAP_OBJECTS.SPECIAL+$4
	
	;Init Enemy Type
	LDA #$01 ;mob targets
		;LDA #$03 ;pc targets
	STA COMBAT.MAP_OBJECTS.SPECIAL+$5


.INIT.SPECIAL.CHR_SHEET
	;ENABLE BANK-SWITCHED RAM ($D000 Bank 1)
	LDA $C08B 
	LDA $C08B
						
		TSX			;transfer stack pointer to X-REG
		STX TEMP	;save stack pointer
	STA $C009 ;enable aux zero-page & aux BSR 

	
	;copy SPECIAL data from stats table to character sheet (1st record)
	LDX #$00
.INIT.SPECIAL.CHR_SHEET.LOOP
	LDA .SPECIAL.TABLE.SPECIAL.ID_0,X ;read SPECIAL character sheet data
	STA CHR_SHEET.SPECIAL.AUX_MEMORY.START,X ;write to 1st record in SPECIAL character sheet
	INX
	CPX #CHR_SHEET.SPECIAL.RECORD_SIZE
	BNE .INIT.SPECIAL.CHR_SHEET.LOOP
	
	;move stop value to 2nd record
	LDA #CHR_SHEET.SPECIAL.STOP_VALUE
	STA CHR_SHEET.SPECIAL.AUX_MEMORY.START,X
		
;RESTORE MEMORY STATE
	STA $C008 ;enable main zero-page & main BSR 
		LDX TEMP	;restore stack pointer to X-REG
		TXS ;transfer X-REG to stack pointer

	;RE-ENABLE BANK-SWITCHED RAM ($D000 Bank 2)
	LDA $C083				;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK2 2nd)
	LDA $C083

	
	
	;set health
	;(N/A. it defaults to $00 = good)

	JMP .INIT.SPECIAL.DATA.DONE
	
		;SPECIAL: WYVERN
.SPECIAL.TABLE.SPECIAL.ID_0	.HS	00.0A.58.02.00.00.80.78.50.78.00.00.FF.00.06.D0.00
.SPECIAL.TABLE.SPECIAL.ID_0.NAME.START	.AZ	-/Wyvern/
.SPECIAL.TABLE.SPECIAL.ID_0.NAME.END		
.SPECIAL.TABLE.SPECIAL.ID_0.NAME.SIZE	.EQ	.SPECIAL.TABLE.SPECIAL.ID_0.NAME.END-.SPECIAL.TABLE.SPECIAL.ID_0.NAME.START
	.BS 	CHR_SHEET.SPECIAL.NAME.MAX_SIZE-.SPECIAL.TABLE.SPECIAL.ID_0.NAME.SIZE+1,$AA

.INIT.SPECIAL.DATA.DONE

@END

		; LDA #$FF ;dead
		; STA COMBAT.MAP_OBJECTS.SPECIAL+$7,X

			
; ;TEMP: hard code special #2
			; LDX #$08
			; ;Init Tile_TYPE
			; LDA #$96
			; STA COMBAT.MAP_OBJECTS.SPECIAL+$2,X
			
			; ;Init Total Specials
			; LDA #$02
			; STA COMBAT.SPECIAL.START.TOTAL ;the number of specials at the start of combat	

			; ;Init GMAP.X/Y
			; LDA #$08
			; STA COMBAT.MAP_OBJECTS.SPECIAL+$0,X
			; LDA #$03
			; STA COMBAT.MAP_OBJECTS.SPECIAL+$1,X
			; ;$02,$01
			
			; ;closest to mob #1
			; ; LDA #$04
			; ; STA COMBAT.MAP_OBJECTS.SPECIAL+$0,X
			; ; LDA #$07
			; ; STA COMBAT.MAP_OBJECTS.SPECIAL+$1,X
			
			; ;standard location
			; ; LDA #$05
			; ; STA COMBAT.MAP_OBJECTS.SPECIAL+$0,X
			; ; LDA #$09
			; ; STA COMBAT.MAP_OBJECTS.SPECIAL+$1,X
			
			; ;Init Flags (default $00)
			; LDA #$01 ;flags ;set aggressive flag
			; STA COMBAT.MAP_OBJECTS.SPECIAL+$3,X
			; ; LDA #$09 ;flags
			; ; STA COMBAT.MAP_OBJECTS.SPECIAL+$3,X
			
			; ;Init Alignment	
				; LDA #$01 ;mob aligned
			; ;LDA #$03 ;pc aligned
			; STA COMBAT.MAP_OBJECTS.SPECIAL+$4,X
			
			; ;Init Enemy Type
			; ;LDA #$01 ;mob targets
				; LDA #$03 ;pc targets
			; STA COMBAT.MAP_OBJECTS.SPECIAL+$5,X
		
		
			; ;set health
			; LDA #$00 ;good
			; ;LDA #$FF ;dead
			; STA COMBAT.MAP_OBJECTS.SPECIAL+$7,X
			
	
;---
		
COMBAT.INIT.DRAW.SCREEN			
	;reset screen arrays
	JSR INIT.SCREEN.ARRAYS

	JSR DRAW.SCREEN.ALTERNATE.ENTRANCE ;alternate entrace skips the call to TILE.LOOKUP.SCREEN, since this module controls the tiles onscreen. 								
	JSR COMBAT_SETUP.INIT.TEXT_WINDOW.RIGHT		
	JSR FLIP.PAGE


			
;SYNC HI-RES PAGES
;Note: needed because the pages need to be in sync for animation to work properly. 
	;ENABLE BANK-SWITCHED RAM ($D000 Bank 1)
		LDA $C08B					;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK1, 1ST)
		LDA $C08B	
	JSR COPY.SCREEN		;**DON'T REMOVE** 
		;RE-ENABLE BANK-SWITCHED RAM ($D000 Bank 2)
		LDA $C083				;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK2 2nd)
		LDA $C083

.EXIT

		
;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX


			
				
	RTS ;return to routine that called combat.setup (INITIATE.COMBAT)
		
@END

COMBAT_SE.TEST.MODE
@START
.INIT.MAP_OBJECTS.MOB
@START	
;TEMP: LOAD MOB MAP OBJECT DATA INTO MAP OBECT ARRAY
;(production: load the values as they are rolled direcly into the mob MO array)
		LDA #COMBAT_LOADER.MAP_OBJECT.MOB.START
		STA COPY.FROM_START
		LDA /COMBAT_LOADER.MAP_OBJECT.MOB.START
		STA COPY.FROM_START+$1
		LDA #COMBAT_LOADER.MAP_OBJECT.MOB.END
		STA COPY.FROM_END
		LDA /COMBAT_LOADER.MAP_OBJECT.MOB.END
		STA COPY.FROM_END+$1
			
		LDA #COMBAT.MAP_OBJECTS.MOB
		STA COPY.TO
		LDA /COMBAT.MAP_OBJECTS.MOB
		STA COPY.TO+$1
		
		JSR MEMORY.COPY	
		
;tally the number of mobs active
;(this routine iterates through the COMBAT.MAP_OBJECTS.MOB, until the stop value ($00) is found in byte $02 (tile_type))
	LDX #$00
	LDY #$00
.COUNT.MOBS.LOOP
	LDA COMBAT.MAP_OBJECTS.MOB+$2,X
	BEQ .COUNT.MOBS.LOOP.COMPLETE
	INY ;increment mob tally
	
	;increment map object record index
	TXA
	CLC
	ADC #COMBAT.MAP_OBJECTS.MOB.RECORD_SIZE
	TAX
	
	
	;exit test
	BNE .COUNT.MOBS.LOOP
.COUNT.MOBS.LOOP.COMPLETE
	STY COMBAT.MOB.START.TOTAL

		
@END


.LOAD.CHR_SHEET.MOB
@START
;LOAD CHARACTER SHEET DATA 
;(into aux bsr memory)

	;ENABLE BANK-SWITCHED RAM ($D000 Bank 1)
	LDA $C08B 
	LDA $C08B
						
		TSX			;transfer stack pointer to X-REG
		STX TEMP	;save stack pointer
	STA $C009 ;enable aux zero-page & aux BSR 

		;execute copy
		LDA #TEST.COMBAT_SE.MOB_CHAR_SHEET_TABLE.START
		STA COPY.FROM_START+$0
		LDA /TEST.COMBAT_SE.MOB_CHAR_SHEET_TABLE.START
		STA COPY.FROM_START+$1

		LDA #TEST.COMBAT_SE.MOB_CHAR_SHEET_TABLE.END
		STA COPY.FROM_END+$0
		LDA /TEST.COMBAT_SE.MOB_CHAR_SHEET_TABLE.END
		STA COPY.FROM_END+$1
	
		LDA #CHR_SHEET.MOB.AUX_MEMORY.START
		STA COPY.TO
		LDA /CHR_SHEET.MOB.AUX_MEMORY.START
		STA COPY.TO+$1
	
	JSR MEMORY.COPY	
			
	
;RESTORE MEMORY STATE
	STA $C008 ;enable main zero-page & main BSR 
		LDX TEMP	;restore stack pointer to X-REG
		TXS ;transfer X-REG to stack pointer

	;RE-ENABLE BANK-SWITCHED RAM ($D000 Bank 2)
	LDA $C083				;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK2 2nd)
	LDA $C083
	
;COPY HP MAX LO/HO 
;(From character sheet to extended character sheet.)
;(The extended character sheet variables are stored locally in the main combat module
;at the very end of the memory range reserved for it.)

	RTS
	
@END
@END

;**OPT** Memory. Disk. Remove combat test mode
COMBAT_LOADER.MO.DATA ;(for test mode)
;(load PC and MOB map object arrays)
	.IN 	c:\my_code\includes_libs\swap.routines.combat.test_data.asm
	.IN 	c:\my_code\includes_libs\swap.routines.combat.test_data2.asm


@END
@END


;SUPPORTING SUBROUTINES: General
@START

				;**OPT** Memory. Disk Speed. convert as much of the routines below as possible to use DRAW.LINE, which can draw a rectangle and erase what is inside, and draw a single line. 
				
COMBAT_SETUP.INIT.TEXT_WINDOW.RIGHT
@START
	
		;LDA PAGE.BACKGROUND	
		LDA #$03
	JSR COMBAT_SETUP_EXIT.CLEAR.TEXT_WINDOW.RIGHT
				;JSR KEYIN
	JSR COMBAT_SETUP_EXIT.CLEAR.TEXT_WINDOW.TOP	
	JSR COMBAT_SETUP_EXIT.CLEAR.TEXT_WINDOW.BOTTOM
				;JSR KEYIN
	JSR COMBAT_SETUP_EXIT.DRAW.TEXT_WINDOW.RIGHT
				;JSR KEYIN
				
	;JSR DISPLAY.CHARACTER.ROSTER
	
		;JSR KEYIN
		

;INIT TEXT WINDOW

		;INIT TEXT WINDOW BOUNDARIES (for the text itself, excluding borders)
		LDA #TWS.COMBAT.SCROLL_WINDOW.LEFT_SBYTE	
		STA TWF.LEFT_SBYTE
		
		LDA #TWS.COMBAT.SCROLL_WINDOW.WIDTH
		STA TWF.WIDTH
		
		LDA #TWS.COMBAT.SCROLL_WINDOW.TOP_ROW
		STA TWF.TOP_ROW
		
		LDA #TWS.COMBAT.SCROLL_WINDOW.BOTTOM_ROW
		STA TWF.BOTTOM_ROW

	
; ;SET CURSOR IN START POSITION	
		
			;SET CURSOR POSITION TO UPPER LEFT OF TEXT WINDOW
			LDA #TWS.COMBAT.SCROLL_WINDOW.CURSOR_START_SBYTE
			STA HTAB	
			LDA #TWS.COMBAT.SCROLL_WINDOW.CURSOR_START_ROW
			STA VTAB
		JSR	UPDATE.CHAR.POS



	
	RTS
	
@END

COMBAT_SETUP_EXIT.DRAW.TEXT_WINDOW.RIGHT
@START

;INIT VARIABLES
	LDA #$00
	STA SCREEN_BYTE.COUNTER

	LDA #$01
	STA TW.RIGHT_WINDOW.CLEAN_UP.FLAG   ;turn flag on so that DRAW.SCREEN will erase the right edge and first two bytes of the top/bottom lines of the text window border. 
										;This is needed because this is the portion of the text window border that isn't normally onscreen, AND CLEAR.TEXT_WINDOW.RIGHT only clears the text space, not the border. If it were to clear the border on both pages the foreground erase would be very noticable.
										;Also used via TW.RIGHT_WINDOW.STATUS.FLAG (.EQ) to tell the animation manager not to draw on top of the text window
	
.DRAW.SCREEN_BORDER
;Note: this is to erase any contents of the 1 line text windows at the top and bottom of the general screen

;TOP    LINE: $3000 PAGE1 (ADDR1), $5000 PAGE2 (ADDR2)
;BOTTOM LINE: $33D0 PAGE3 (ADDR3), $53D0 PAGE4 (ADDR4)

	LDY #$02			;init screen byte index

;INIT LINE BASE ADDRESSES

	;init LO bytes
	LDA #$00
	STA LINE.BASE.ADDR1
	STA LINE.BASE.ADDR2
	
	LDA #$D0
	STA LINE.BASE.ADDR3
	STA LINE.BASE.ADDR4
	
	;init HO bytes
	LDA #$30
	STA LINE.BASE.ADDR1+$1

	LDA #$50
	STA LINE.BASE.ADDR2+$1

	LDA #$33
	STA LINE.BASE.ADDR3+$1

	LDA #$53
	STA LINE.BASE.ADDR4+$1	
	
;DRAW TOP AND BOTTOM LINE OF BORDER ON BOTH HI-RES PAGES	
.SCREEN_BORDER.LOOP
	LDA #$D5
	STA (LINE.BASE.ADDR1),Y
	STA (LINE.BASE.ADDR2),Y
	STA (LINE.BASE.ADDR3),Y
	STA (LINE.BASE.ADDR4),Y
	
	INY ;move 1 screen byte right
	
	LDA #$AA
	STA (LINE.BASE.ADDR1),Y
	STA (LINE.BASE.ADDR2),Y
	STA (LINE.BASE.ADDR3),Y
	STA (LINE.BASE.ADDR4),Y
	
	INY ;move 1 screen byte right
	
	CPY #SCREEN.STOP_BYTE-$04
	BNE .SCREEN_BORDER.LOOP

	
.DRAW.TW.BORDER.TOP
		LDA #TWB.RW.NPC_TALK.TOP_LINE
		STA DRAW.START_LINE
		
		LDA #$24
		STA DRAW.START_BYTE
		
		LDA #$28
		STA DRAW.STOP_BYTE
			
		LDA #TWB.RW.NPC_TALK.TOP_LINE+$1
		STA DRAW.STOP_LINE

		LDA #$D5	
		STA DRAW.BYTE_VALUE.HORIZONTAL+$0
		
		LDA #$AA	
		STA DRAW.BYTE_VALUE.HORIZONTAL+$1
		LDA #$07		;set draw to both pages, and set the top edge flag. 
			;we set high bit as a parameter to indicate we want the byte value for the draw to be $00
	JSR DRAW.LINE
	
	
;SETUP DRAW, TEXT WINDOW BORDER (RIGHT SIDE): RIGHT EDGE
	LDY #TWB.RW.NPC_TALK.RIGHT_SBYTE
	
	LDA PAGE.BACKGROUND
	LDX #TWB.RW.NPC_TALK.TOP_LINE+$1 ;line 5
	JSR GET.LINE.ADDRESS1

	LDA PAGE.FOREGROUND
	JSR GET.LINE.ADDRESS2
	
;DRAW TEXT WINDOW BORDER (RIGHT SIDE): RIGHT EDGE
.DRAW.TW.BORDER.RIGHT
	LDA #$A0					;bit mapped byte
	STA (LINE.BASE.ADDR1),Y		;save bit mapped byte to video screen memory (foreground page)
	STA (LINE.BASE.ADDR2),Y		;save bit mapped byte to video screen memory (background page)

	INX ;next line, move down
		LDA PAGE.BACKGROUND
	JSR GET.LINE.ADDRESS1 

	LDA PAGE.FOREGROUND
	JSR GET.LINE.ADDRESS2
	
	CPX #TWB.RW.NPC_TALK.BOTTOM_LINE
	BNE .DRAW.TW.BORDER.RIGHT

	
	
;DRAW TEXT WINDOW BORDER (RIGHT SIDE): BOTTOM LINE
	LDY #TWB.RW.NPC_TALK.RIGHT_SBYTE
	LDA #$AA
	STA (LINE.BASE.ADDR1),Y		;save bit mapped byte to video screen memory (foreground page) 
	STA (LINE.BASE.ADDR2),Y		;save bit mapped byte to video screen memory (background page)
	DEY ;move 1 screen byte left
	LDA #$D5
	STA (LINE.BASE.ADDR1),Y		;save bit mapped byte to video screen memory (foreground page) 
	STA (LINE.BASE.ADDR2),Y		;save bit mapped byte to video screen memory (background page)

	DEY ;move 1 screen byte left
	LDA #$AA
	STA (LINE.BASE.ADDR1),Y		;save bit mapped byte to video screen memory (foreground page) 
	STA (LINE.BASE.ADDR2),Y		;save bit mapped byte to video screen memory (background page)
	DEY ;move 1 screen byte left
	LDA #$D5
	STA (LINE.BASE.ADDR1),Y		;save bit mapped byte to video screen memory (foreground page)  
	STA (LINE.BASE.ADDR2),Y		;save bit mapped byte to video screen memory (background page)

;SETUP DRAW, TEXT WINDOW BORDER (RIGHT SIDE): LEFT EDGE

	;Move up 1 line
	LDA LINE.BASE.ADDR1+$1
	SEC
	SBC #$04				;Subtracts $400 from the line base address.
	STA LINE.BASE.ADDR1+$1

	;Move up 1 line
	LDA LINE.BASE.ADDR2+$1
	SEC
	SBC #$04				;Subtracts $400 from the line base address.
	STA LINE.BASE.ADDR2+$1
	
	LDY #TWB.RW.NPC_TALK.LEFT_SBYTE
.DRAW.TW.BORDER.LEFT
	LDA #$84					;bit mapped byte
	STA (LINE.BASE.ADDR1),Y		;save bit mapped byte to video screen memory (foreground page)  
	STA (LINE.BASE.ADDR2),Y		;save bit mapped byte to video screen memory (background page)

	DEX ;next line, move up
		LDA PAGE.BACKGROUND
	JSR GET.LINE.ADDRESS1

		LDA PAGE.FOREGROUND
	JSR GET.LINE.ADDRESS2
	
	CPX #TWB.RW.NPC_TALK.TOP_LINE
	BNE .DRAW.TW.BORDER.LEFT
	
			;JMP .EXIT
	

;=====COMBAT SCROLL WINDOW=====
@START
;DRAW COMBAT SCROLL WINDOW BORDER: TOP LINE
			
.DRAW.TW.INPUT.BORDER.TOP
	LDA PAGE.BACKGROUND
	LDX #TWB.COMBAT.SCROLL_WINDOW.TOP_LINE
	JSR GET.LINE.ADDRESS1

	LDA PAGE.FOREGROUND
	JSR GET.LINE.ADDRESS2
	
	LDY #TWB.COMBAT.SCROLL_WINDOW.LEFT_SBYTE+1 ;+1 is because the byte to integrate with the verticle border line is draw below. 
	LDA #$D5
.LOOP.TW.INPUT.BORDER.TOP
	LDA #$AA	
	STA (LINE.BASE.ADDR1),Y	;save to background page
	STA (LINE.BASE.ADDR2),Y ;save to foreground page
	INY ;move 1 screen byte right
	CPY #TWB.COMBAT.SCROLL_WINDOW.RIGHT_SBYTE
	BEQ .LOOP.TW.INPUT.BORDER.TOP.DONE
	LDA #$D5
	STA (LINE.BASE.ADDR1),Y ;save to background page
	STA (LINE.BASE.ADDR2),Y ;save to foreground page

	INY ;move 1 screen byte right
	CPY #TWB.COMBAT.SCROLL_WINDOW.RIGHT_SBYTE
	BNE .LOOP.TW.INPUT.BORDER.TOP
.LOOP.TW.INPUT.BORDER.TOP.DONE


;draw edge bytes of horizontal line to integrate with text window border verticle line
		LDY #$18
		;X-REG = line address from loop above
		LDA #$D4		
	JSR DRAW.BYTE

		LDY #$27
		;X-REG = line address from loop above
		LDA #$AA		
	JSR DRAW.BYTE


	
;PRINT COMBAT SCROLL WINDOW BORDER (TOP) FANCY EDGES
	; ;set cursor positione
		; LDA #$19
		; STA HTAB
		; LDA #$0D
		; STA VTAB
	; JSR	UPDATE.CHAR.POS
	;
			; LDA #$BE ;ASCII = ">"
		; JSR COUT
;
	; ;set cursor positione
		; LDA #$26
		; STA HTAB
		; LDA #$0D
		; STA VTAB
	; JSR	UPDATE.CHAR.POS
	;
			; LDA #$BC ;ASCII = "<"
		; JSR COUT
		
@END
		


; ;SETUP DRAW, SCREEN BORDER: LEFT EDGE
; ;(we decided to close off the left edge of the screen when the combat window is open
; ;becuase otherwise the screen has border on all edges except one)
	;
	;****I CHANGED MY MIND BEFORE ADJUSTING THIS CODE TO DRAW THE LINE ON THE LEFT EDGE. IT IS SETUP FOR A DIFFERENT LINE NOW AS I PASTED IT IN FROM SOMWHERE ELSE. 
	;
	; ;Move up 1 line
	; LDA LINE.BASE.ADDR1+$1
	; SEC
	; SBC #$04				;Subtracts $400 from the line base address.
	; STA LINE.BASE.ADDR1+$1

	; ;Move up 1 line
	; LDA LINE.BASE.ADDR2+$1
	; SEC
	; SBC #$04				;Subtracts $400 from the line base address.
	; STA LINE.BASE.ADDR2+$1
	
	; LDY #TWB.RW.NPC_TALK.LEFT_SBYTE
; .DRAW.TW.BORDER.LEFT
	; LDA #$84					;bit mapped byte
	; STA (LINE.BASE.ADDR1),Y		;save bit mapped byte to video screen memory (foreground page)  
	; STA (LINE.BASE.ADDR2),Y		;save bit mapped byte to video screen memory (background page)

	; DEX ;next line, move up
		; LDA PAGE.BACKGROUND
	; JSR GET.LINE.ADDRESS1

		; LDA PAGE.FOREGROUND
	; JSR GET.LINE.ADDRESS2
	
	; CPX #TWB.RW.NPC_TALK.TOP_LINE
	; BNE .DRAW.TW.BORDER.LEFT

	
.EXIT
	RTS
	

@END	

COMBAT_SETUP_EXIT.CLEAR.TEXT_WINDOW.RIGHT
@START
;PARAMETERS:  USE.PAGE*
;RETURN: NONE
;ENTRANCE: NPC.TALK, DIRECT
;*Specifies which hi-res page the text window should be erased on (BOTH = $03)

;=====================SUBROUTINE DOCUMENTATION====================================
;
;=================================================================================

	PHA ;save USE.PAGE parameter to ACC

.INIT
	LDA #TWS.RW.NPC_TALK.TOP_ROW		;load line in x register	
	STA DRAW.START_LINE
	
	LDA #TWS.RW.NPC_TALK.LEFT_SBYTE
	STA DRAW.START_BYTE
	
	LDA #TWS.RW.NPC_TALK.RIGHT_SBYTE+$1
	STA DRAW.STOP_BYTE
		
	LDA #TWS.RW.NPC_TALK.BOTTOM_ROW+$1
	STA DRAW.STOP_LINE

.DRAW
		PLA ;restore USE.PAGE parameter to ACC
		ORA #$80	;set high bit as a parameter to indicate we want the byte value for the draw to be $00
	JSR DRAW.LINE

.EXIT
	RTS

@END

COMBAT_SETUP_EXIT.CLEAR.TEXT_WINDOW.TOP
@START
;PARAMETERES: NONE
;ENTRANCE: DIRECT
;RETURN: NONE


.INIT
	LDA #TWB.TOP_WINDOW.TOP_LINE		;load line in x register	
	STA DRAW.START_LINE
	
	LDA #TWB.TOP_WINDOW.LEFT_SBYTE
	STA DRAW.START_BYTE
	
	LDA #TWB.TOP_WINDOW.RIGHT_SBYTE+$1
	STA DRAW.STOP_BYTE
		
	LDA #TWB.TOP_WINDOW.BOTTOM_LINE+$1
	STA DRAW.STOP_LINE

.DRAW	
		LDA #$83		;set draw to both pages
			;we set high bit as a parameter to indicate we want the byte value for the draw to be $00
		;STA USE.PAGE
	JSR DRAW.LINE
	
	RTS
	


@END

COMBAT_SETUP_EXIT.CLEAR.TEXT_WINDOW.BOTTOM
@START
;PARAMETERES: NONE
;ENTRANCE: DIRECT
;RETURN: NONE

.INIT
	LDA #TWB.BOTTOM_WINDOW.TOP_LINE		;load line in x register	
	STA DRAW.START_LINE
	
	LDA #TWB.BOTTOM_WINDOW.LEFT_SBYTE
	STA DRAW.START_BYTE
	
	LDA #TWB.BOTTOM_WINDOW.RIGHT_SBYTE+$1
	STA DRAW.STOP_BYTE
		
	LDA #TWB.BOTTOM_WINDOW.BOTTOM_LINE+$1
	STA DRAW.STOP_LINE

	
.DRAW	
		LDA #$83		;set draw to both pages
			;we set high bit as a parameter to indicate we want the byte value for the draw to be $00
		;STA USE.PAGE
	JSR DRAW.LINE
	

.EXIT
	
	RTS
@END

COMBAT_SE.STATS.APPLY.PERCENTAGE
@START
;PARAMETERS: ;ACC: ($00 = hex mode | $01 = BCD mode)*, COMBAT_SE.STATS.PERCENT.PARM, COMBAT_SE.STATS.APPLY_PERCENT.PARM_BASE(2)
;ENTRANCE: COMBAT.SETUP
;RETURN: RESULT(2)

;=====================SUBROUTINE DOCUMENTATION====================================
;
;(to avoid using floating decimals, this routine applies the percent using the
;the formula: %(as integer) * number / 100, which yields the same result as number * percent (as decimal)
;So instead of .75 * 50, this routine would calculate 75*50/100
;
;=================================================================================


;Formula: RESULT(2) = base number * M% / 100

;*WARNING: make sure that all parameters are submitted as BCD values if this mode is used 

	
		;ACC = parm: mode ($00 = hex mode | $01 = BCD mode)
		STA  COMBAT_SE.STATS.APPLY.PERCENT.MODE
		BEQ .HEX.ENTRANCE ;branch if BCD mode

		SED ;set decimal mode
		
				
		;ACC: size modifier multiplier
.HEX.ENTRANCE
		LDA COMBAT_SE.STATS.PERCENT.PARM
		;LDA #$20
		STA MULPLR+$0
		LDA #$00
		STA MULPLR+$1
		;			
		LDA COMBAT_SE.STATS.APPLY_PERCENT.PARM_BASE+$0
		;LDA #$06
		STA MULCND+$0
		LDA COMBAT_SE.STATS.APPLY_PERCENT.PARM_BASE+$1
		;LDA #$00
		STA MULCND+$01			
		;SED (set decimal mode is done above)

				; lda #$01 
				; sta troubleshooting.hook
				
	JSR MLP.16	;supports HEX and BCD
		;CLD (clear decimal mode is done below)
		LDA RESULT+$0 ;load multiplication return value LO byte
		STA DIVIDEND+$0
		LDA RESULT+$1 ;load multiplication return value HO byte
		STA DIVIDEND+$1				
		;


				; LDA #$AA
				; ; LDA RESULT+$0
				; ; STA $BE00
				; ; LDA RESULT+$1
				; ; STA $BE01
				; ;LDX COMBAT_SE.STATS.PERCENT.PARM
				; LDX RESULT+$0
				; LDY RESULT+$1
				; ; LDA COMBAT_SE.STATS.PERCENT.PARM
				; ; LDX COMBAT_SE.STATS.APPLY_PERCENT.PARM_BASE+$0
				; ; LDY COMBAT_SE.STATS.APPLY_PERCENT.PARM_BASE+$1
				; JSR PREP.BRK
				; BRK

				
	LDA COMBAT_SE.STATS.APPLY.PERCENT.MODE	;($00 = hex mode | $01 = BCD mode)	
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

COMBAT_SE.RANDOMIZE.VARIANCE
@START
;PARAMETERS: ACC = max possible variance
;ENTRANCE: COMBAT.SETUP
;RETURN: ACC = variance

	;brance if max possible variance > ?? $10?
	
	TAX ;use max possible variance as loop counter
	INX ;for purposes of the random number tier calculations, add one so that $00 is a possible variance. i.e 
		;if the max possible variance is 1, then the variance could be 0 or 1. 
		;essentially, to calculate the tiers we need X-REG to be the quantity of tiers. 
		
		LDA #$FF
		STA DIVIDEND+$0 ;number to be divided
		LDA #$00
		STA DIVIDEND+$1
		;X-REG = max possible variance
		STX DIVISOR+$0 ;number to divide by
		LDA #$00
		STA DIVISOR+$1
	JSR DIV.16
		;RETURN VALUE: result+$0 (quotient LO), result+$1 (quotient HO), result+$2 (remainder LO), result+$3 (remainder HO)
		LDA RESULT+$0
		;ignore HO byte (LDA RESULT+$1)
		STA COMBAT_SE.RANDOM_NUMBER.TIERS
			
	JSR RANDOM.8
		;RETURN VALUE: ACC = 8-bit random number
		STA COMBAT_SE.MOB_QTY.RANDOM_NUMBER

	DEX ;undo the INX above, which converted X-REG to the quantity of tiers.  


	;XREG: = max possible variance. Use as loop counter
	LDA #$FF
.LOOP.RANDOMIZE
	SEC
	SBC COMBAT_SE.RANDOM_NUMBER.TIERS
	CMP COMBAT_SE.MOB_QTY.RANDOM_NUMBER
	BCC .LOOP.RANDOMIZE.DONE
	DEX ;decrement loop counter
	BNE .LOOP.RANDOMIZE
.LOOP.RANDOMIZE.DONE
	;X-REG = randomized variance
	
	
.SET.RETURN_VALUE
	TXA ;transfer randomized variance to ACC
	
			; LDA COMBAT_SE.RANDOM_NUMBER.TIERS
			; LDY COMBAT_SE.MOB_QTY.RANDOM_NUMBER
			; ;XREG
			; JSR PREP.BRK
			; BRK
			
	RTS
@END
	
@END

;**OPT** Disk. Disk speed. Memory. There might be savings in all of these if the routines that call COMBAT_SE.READ.MOB_TABLE.FILE.ENTRANCE just have in-line code



;LOCAL VARIABLES
@START

;GENERAL
@START

@END

;BATTLE SCREENS
@START

;**OPT** Speed. Disk speed. store battle maps in separate file and only load from disk the one that is needed for the current combat instance.

;SURFACE
BATTLEFIELD_0.HILLS				.HS	45.45.45.45.45.45.45.45.45.45.45.00.00.00.00.00.00.45.45.34.34.34.34.34.45.45.34.45.00.00.00.00.00.00.45.45.34.34.34.45.45.34.34.34.45.00.00.00.00.00.00.45.45.45.34.34.45.34.34.34.34.45.00.00.00.00.00.00.45.34.45.45.34.34.34.34.34.34.45.00.00.00.00.00.00.45.34.34.34.34.34.34.34.45.34.45.00.00.00.00.00.00.45.34.34.34.34.34.34.34.45.45.45.00.00.00.00.00.00.45.34.34.45.45.34.34.34.34.45.45.00.00.00.00.00.00.45.34.45.45.34.34.34.34.34.34.45.00.00.00.00.00.00.45.45.45.45.34.34.34.45.45.34.45.00.00.00.00.00.00.45.45.45.45.45.45.45.45.45.45.45.00.00.00.00.00.00
BATTLEFIELD_3.FOREST			.HS	34.34.34.34.34.34.34.1A.34.34.34.00.00.00.00.00.00.34.34.34.34.34.34.34.34.34.34.34.00.00.00.00.00.00.34.1A.1A.34.34.34.34.34.34.34.34.00.00.00.00.00.00.34.1A.34.34.34.34.34.34.1B.34.34.00.00.00.00.00.00.34.34.34.34.34.34.34.34.34.34.34.00.00.00.00.00.00.34.34.34.34.34.34.34.34.34.34.34.00.00.00.00.00.00.34.34.34.34.34.34.34.34.34.1C.1C.00.00.00.00.00.00.34.34.1B.34.34.34.34.34.34.34.1C.00.00.00.00.00.00.34.34.34.34.34.34.34.34.34.34.34.00.00.00.00.00.00.1C.34.34.34.34.34.34.34.34.34.34.00.00.00.00.00.00.34.34.34.34.34.34.34.34.34.34.34.00.00.00.00.00.00																				
BATTLEFIELD_4.SURFACE_DEFAULT	.HS	34.34.34.34.34.34.34.34.34.34.34.00.00.00.00.00.00.34.34.34.34.34.34.34.34.34.34.34.00.00.00.00.00.00.34.34.20.34.34.34.34.34.21.34.34.00.00.00.00.00.00.34.34.34.34.34.34.34.34.34.34.34.00.00.00.00.00.00.34.34.34.34.34.20.34.34.34.34.20.00.00.00.00.00.00.34.34.34.21.34.34.34.34.22.34.34.00.00.00.00.00.00.34.34.34.34.34.34.34.34.34.34.34.00.00.00.00.00.00.34.34.34.34.34.34.34.34.34.34.34.00.00.00.00.00.00.34.22.34.34.34.34.34.20.34.34.34.00.00.00.00.00.00.34.34.34.34.34.34.34.34.34.22.34.00.00.00.00.00.00.34.34.34.34.34.34.34.34.34.34.34.00.00.00.00.00.00
BATTLEFIELD_5.SHORELINE			.HS	88.88.88.88.88.88.88.88.88.88.88.00.00.00.00.00.00.88.88.88.88.88.88.88.88.88.88.88.00.00.00.00.00.00.88.88.88.88.88.88.88.88.88.88.88.00.00.00.00.00.00.88.88.88.88.88.88.88.88.88.88.88.00.00.00.00.00.00.67.71.88.88.88.88.88.88.88.70.67.00.00.00.00.00.00.41.76.71.88.88.88.88.88.70.75.41.00.00.00.00.00.00.34.41.76.67.71.77.77.70.75.41.34.00.00.00.00.00.00.34.41.41.41.41.41.41.41.41.41.34.00.00.00.00.00.00.34.34.41.41.34.41.41.41.34.34.34.00.00.00.00.00.00.34.34.34.34.34.34.34.34.34.34.34.00.00.00.00.00.00.34.34.34.34.34.34.34.34.34.34.34.00.00.00.00.00.00	


;UNDERMAP
BATTLEFIELD_1.SPIDER_WEBS		.HS	7C.7C.7C.7C.7C.7C.7C.7C.6A.7C.7C.00.00.00.00.00.00.6A.6A.7C.6A.6A.7C.7C.7C.6A.6A.7C.00.00.00.00.00.00.7C.6A.6A.6A.7C.7C.6A.6A.6A.6A.7C.00.00.00.00.00.00.7C.6A.6A.6A.7C.7C.7C.6A.6A.6A.7C.00.00.00.00.00.00.6A.6A.6A.6A.6A.7C.7C.6A.6A.6A.6A.00.00.00.00.00.00.6A.7C.7C.6A.7C.7C.7C.7C.6A.7C.6A.00.00.00.00.00.00.7C.7C.7C.7C.7C.7C.7C.7C.7C.7C.7C.00.00.00.00.00.00.7C.7C.7C.7C.7C.7C.7C.7C.7C.7C.7C.00.00.00.00.00.00.7C.7C.7C.7C.7C.7C.7C.7C.7C.7C.7C.00.00.00.00.00.00.7C.7C.7C.7C.7C.7C.7C.7C.7C.7C.7C.00.00.00.00.00.00.7C.7C.7C.7C.7C.7C.7C.7C.7C.7C.7C.00.00.00.00.00.00	
BATTLEFIELD_2.UNDERMAP_DEFAULT	.HS	7C.7C.7C.7C.7C.7C.7C.7C.7C.7C.7C.00.00.00.00.00.00.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.00.00.00.00.00.00.7C.7C.7C.7C.7C.7C.7C.7C.7C.7C.7C.00.00.00.00.00.00.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.00.00.00.00.00.00.7C.7C.7C.7C.7C.7C.7C.7C.7C.7C.7C.00.00.00.00.00.00.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.00.00.00.00.00.00.7C.7C.7C.7C.7C.7C.7C.7C.7C.7C.7C.00.00.00.00.00.00.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.00.00.00.00.00.00.7C.7C.7C.7C.7C.7C.7C.7C.7C.7C.7C.00.00.00.00.00.00.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.00.00.00.00.00.00.7C.7C.7C.7C.7C.7C.7C.7C.7C.7C.7C.00.00.00.00.00.00

;;TWO COWS
;BATTLEFIELD_0.HILLS		.HS	45.45.45.45.45.45.45.45.45.45.45.00.00.00.00.00.00.45.45.34.34.34.34.34.45.45.34.45.00.00.00.00.00.00.45.45.34.34.34.45.45.34.34.34.45.00.00.00.00.00.00.45.45.45.34.34.45.34.34.34.34.45.00.00.00.00.00.00.45.34.45.45.34.34.34.34.34.34.45.00.00.00.00.00.00.45.34.34.34.34.34.34.34.45.34.45.00.00.00.00.00.00.45.34.34.34.34.34.34.34.45.45.45.00.00.00.00.00.00.45.34.9F.45.45.34.34.34.9F.45.45.00.00.00.00.00.00.45.34.45.45.34.34.34.34.34.34.45.00.00.00.00.00.00.45.45.45.45.34.34.34.45.45.34.45.00.00.00.00.00.00.45.45.45.45.45.45.45.45.45.45.45.00.00.00.00.00.00

; ;3 COWS
; BATTLEFIELD_0.HILLS			.HS	45.45.45.45.E8.45.45.45.45.45.45.00.00.00.00.00.00.45.45.34.34.34.34.34.45.45.34.45.00.00.00.00.00.00.45.45.34.34.34.45.45.34.34.34.45.00.00.00.00.00.00.45.45.45.34.34.45.34.34.34.34.45.00.00.00.00.00.00.45.34.45.45.34.34.34.34.34.34.45.00.00.00.00.00.00.45.34.34.34.34.34.34.34.45.34.45.00.00.00.00.00.00.45.34.34.34.34.34.34.34.45.45.45.00.00.00.00.00.00.45.34.9F.45.45.34.34.34.9F.45.45.00.00.00.00.00.00.45.34.45.45.34.34.34.34.34.34.45.00.00.00.00.00.00.45.45.45.45.34.34.34.45.45.34.45.00.00.00.00.00.00.45.45.45.45.45.45.45.45.45.45.45.00.00.00.00.00.00

;all cows
;BATTLEFIELD_0.HILLS			.HS	9F.9F.9F.9F.9F.9F.9F.9F.9F.9F.9F.00.00.00.00.00.00.9F.9F.9F.9F.9F.9F.9F.9F.9F.9F.9F.00.00.00.00.00.00.9F.9F.9F.9F.9F.9F.9F.9F.9F.9F.9F.00.00.00.00.00.00.9F.9F.9F.9F.9F.9F.9F.9F.9F.9F.9F.00.00.00.00.00.00.9F.9F.9F.9F.9F.9F.9F.9F.9F.9F.9F.00.00.00.00.00.00.9F.9F.9F.9F.9F.9F.9F.9F.9F.9F.9F.00.00.00.00.00.00.9F.9F.9F.9F.9F.9F.9F.9F.9F.9F.9F.00.00.00.00.00.00.9F.9F.9F.9F.9F.9F.9F.9F.9F.9F.9F.00.00.00.00.00.00.9F.9F.9F.9F.9F.9F.9F.9F.9F.9F.9F.00.00.00.00.00.00.9F.9F.9F.9F.9F.9F.9F.9F.9F.9F.9F.00.00.00.00.00.00.9F.9F.9F.9F.9F.9F.9F.9F.9F.9F.9F.00.00.00.00.00.00

;COMBAT.BATTLEFILED.ACTIVE .EQ BATTLEFIELD_1.SPIDER_WEBS
;COMBAT.BATTLEFILED.ACTIVE	.EQ BATTLEFIELD_2.UNDERMAP_DEFAULT
@END

;MOB TABLES: GROUP_ID LOOKUP
@START	
; COMBAT_SE.MOB.GROUP_ID.LOOKUP.START		
; COMBAT_SE.MOB.GROUP_ID.LOOKUP.REC_0	.HS	01.97.00.00
; COMBAT_SE.MOB.GROUP_ID.LOOKUP.REC_1	.HS	01.98.05.00
; COMBAT_SE.MOB.GROUP_ID.LOOKUP.REC_2	.HS	01.00.00.00
; COMBAT_SE.MOB.GROUP_ID.LOOKUP.REC_3	.HS	00.00.00.00
; COMBAT_SE.MOB.GROUP_ID.LOOKUP.REC_4	.HS	00.97.02.00
; COMBAT_SE.MOB.GROUP_ID.LOOKUP.REC_5	.HS	00.98.05.00
; COMBAT_SE.MOB.GROUP_ID.LOOKUP.END
		
COMBAT_SE.MOB.GROUP_ID.LOOKUP.START		
COMBAT_SE.MOB.GROUP_ID.LOOKUP.REC_0	.HS	03.E5.05.00
COMBAT_SE.MOB.GROUP_ID.LOOKUP.REC_1	.HS	01.97.00.00
COMBAT_SE.MOB.GROUP_ID.LOOKUP.REC_2	.HS	01.98.05.00
COMBAT_SE.MOB.GROUP_ID.LOOKUP.REC_3	.HS	01.00.00.00
COMBAT_SE.MOB.GROUP_ID.LOOKUP.REC_4	.HS	00.00.00.00
COMBAT_SE.MOB.GROUP_ID.LOOKUP.REC_5	.HS	00.97.04.00
COMBAT_SE.MOB.GROUP_ID.LOOKUP.REC_6	.HS	00.98.05.00
COMBAT_SE.MOB.GROUP_ID.LOOKUP.END		

	


@END


;MOB LOCATION FIXED CONFIG TABLES (set manually, not using spreadsheet template)
@START
;FIXED.CONFIG1: 6-8 MOB wedge battle formation
COMBAT_SE.MOB_LOCATION.FIXED.CONFIG0.X	.HS 05.04.06.03.05.07.02.08
COMBAT_SE.MOB_LOCATION.FIXED.CONFIG0.Y 	.HS	04.03.03.02.02.02.01.01

@END

@END


COMBAT_SE.SETUP.MAIN.END
;MOB TABLES
@START

				.IN 	c:\my_code\includes_libs\data.game.mob_tables.ASM

@END


;FILE OFFSETS
@START
COMBAT_SE.SETUP.MAIN.READ_BYTES	.EQ COMBAT_SE.SETUP.MAIN.END-COMBAT_SE.SETUP.MAIN.START
COMBAT_SE.MOB_TABLES.READ_BYTES	.EQ COMBAT_SE.MOB_TABLES.START-COMBAT_SE.MOB_TABLES.END

@END



	;.NO $A300
			;detect end of memory reserved for main program code 
	

	;.NO $BD00 ;maximum value for above
				