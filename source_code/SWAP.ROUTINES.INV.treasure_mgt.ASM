;************************INCLUDE FILE*****************************
;(do not assemble stand-alone.  )


;=====================INCLUDE FILE DOCUMENTATION====================================
;
;Include file to SWAP.ROUTINES.INVENTORY.ASM
;
;=================================================================================


;=====================SUB_MODULE DOCUMENTATION===========================================================================
;
;
;
;=============================================================================================================================

TM_1.TREASURE_MGT



;********open chest could get to be long code considering lockpicking, traps etc. 
;probably best for treasure mgt to have an entrance routine that always loads and then based
;on an ACC passed parm, either load combat treasure drop or open chest code. 
;
;Consider that there are some shared routine between open chest and combat treasure drop. If the shared
;routines are put in the entrance code, that might make the combat treasure drop exclusive code pretty thin.
;if that is the case it may make more sense to have a treasure mgt module that loads and if ACC = open box then
;ALSO load the open box code. 


TM_1.COMBAT.ITEM_DROP
@START

.INIT.SCREEN
;init text window function	
	LDA #TWS.TREASURE_DROP.SCROLL_WINDOW.TOP_ROW
	STA TWF.TOP_ROW
	
	LDA #TWS.TREASURE_DROP.SCROLL_WINDOW.BOTTOM_ROW+1
	STA TWF.BOTTOM_ROW
	
	LDA #TWS.TREASURE_DROP.SCROLL_WINDOW.LEFT_SBYTE	
	STA TWF.LEFT_SBYTE	

	LDA #TWS.TREASURE_DROP.SCROLL_WINDOW.WIDTH
	STA TWF.WIDTH

	;SET TEXT WINDOW FUNCTION (TWF) CURSOR POSITION VARIABLES TO UPPER LEFT OF TEXT WINDOW
		LDA #TWS.TREASURE_DROP.SCROLL_WINDOW.CURSOR_START_SBYTE	
		STA HTAB

		LDA #TWS.TREASURE_DROP.SCROLL_WINDOW.CURSOR_START_ROW
		STA VTAB
	JSR UPDATE.CHAR.POS

.DRIVER ;if this converted to a subroutine, this section would set the parms

		LDA #$15
		STA IMAGE_DRAW.START_BYTE
		
		LDA #$26 
		STA IMAGE_DRAW.STOP_BYTE	;last screen byte to draw
		
		LDA #$08
		STA IMAGE_DRAW.LINE
	
		LDA #$68
		STA IMAGE_DRAW.LINE.STOP	;last line to draw +1
		
		LDA #SHAPE_TABLE.TREASURE_PILE
		STA SHAPE+$0
		LDA /SHAPE_TABLE.TREASURE_PILE
		STA SHAPE+$1
	JSR TM_1.DRAW.IMAGE
	JSR FLIP.PAGE
	


.INIT.GENERAL
;set player inventory flag  = in memory
	LDA #$00
	STA TM_1.PLAYER_INVENTORY.FILE_DATA.FLAG ;($00 = not in memory | >=$01 in memory)
	STA TM_1.PICKUP_ALL.FLAG	;($00 = not set, >=$01 = set)
	STA TM_1.GOLD_DROP.TALLY+$0
	STA TM_1.GOLD_DROP.TALLY+$1

	
.DOES.MOB.DROP ;***just mobs, not sure I'm going to add specials as their appearance is harder to quantify in formulas for STA economy projections
;CHR_SHEET.MOB.TREASURE_CODE	

	;####insert code to determine if this mob drops treasure


	LDX #$00 ;init mob sequential number, which will incremet by $1 at the top of the loop
.LOOP.ITEM_DROP

.NEXT_MOB
	;increment sequential mob number
	INX

						; LDA #$AA
						; STA CHR_SHEET.RECORD.READ+$0

.READ.MOB.DATA						
	;read mob/special character sheet
		TXA ;(sequential mob #) - (high-bit not set = read mode | high-bit set = write mode)
	JSR COMBAT.READ_WRITE.CHR_SHEET.MOB
		;RETURN: CHR_SHEET.RECORD.READ($80)
		

.GOLD_DROP.ENTRANCE
@START
.GET.GOLD_DROP.AMOUNT
@START
;(this code section gets the baseline gold drop for a mob of this level, and gets the % of baseline parameters in the mob's
;treasure code field, which is then adjusted +/- !10 to randomize the actual gold drop. Since the +/- !10 is applied to the
;treasure code value, the call to it as a % to the baseline gold drop is at the end. 



;GET BASELINE GOLD DROP	
		;calculate TM_1.APPLY_PERCENT.PARM.PERCENT (% of baseline gold drop) 
		LDA CHR_SHEET.MOB.TREASURE_CODE
		STA BCD+$0
		LDA #$00		;this byte represents the 3rd and 4th BCD digit in the order 4th/3rd. This is because it's in the format of HO/LO. 
		STA BCD+$1
	JSR CONVERT.BCD_TO_HEX
		;RETURN: BIN(4)
		LDA BIN+$0
		STA TM_1.APPLY_PERCENT.PARM.PERCENT
		

		
;RANDOMIZE.GOLD_DROP_AMOUNT
@START

;GET ADJUSTMENT AMOUNT (this amount will either be added or subtracted from treasure code value)
@START
;get random number !1-!20, then convert it to !1-!10
		LDA #$14	;=!20
		STA RND.HI
		LDA #$01 
		STA RND.LO
	JSR RANDOM.8
		;ACC = random # (HEX)
		
			
	CMP #$0B
	BCC .RND.IS.LESS
	SEC
	SBC #$0A
.RND.IS.LESS	
	STA TM_1.GOLD_DROP.PERCENT.ADJ
	
 
						
;ADD OR SUBTRACT ADJUSTMENT?
@START
	JSR RANDOM.8
		;ACC = random # (HEX)
	
	;SED ;set decimal mode
	CMP #$80
	BCC .SUBTRACT
	
	
;.ADD
	LDA TM_1.GOLD_DROP.PERCENT.ADJ
	CLC
	ADC TM_1.APPLY_PERCENT.PARM.PERCENT
	STA TM_1.APPLY_PERCENT.PARM.PERCENT
	JMP .RANDOMIZE.GOLD_DROP_AMOUNT.DONE
.SUBTRACT
	LDA TM_1.APPLY_PERCENT.PARM.PERCENT
	SEC
	SBC TM_1.GOLD_DROP.PERCENT.ADJ
	STA TM_1.APPLY_PERCENT.PARM.PERCENT

	;**FALLS THROUGH**

.RANDOMIZE.GOLD_DROP_AMOUNT.DONE
	;CLD ;clear decimal mode


@END


@END

					

					
;APPLY PERCENTAGE TO BASELINE
		
		;calcualte TM_1.GOLD_DROP.BASELINE index (baseline gold drop for the current mob's level)
		LDA CHR_SHEET.MOB.LEVEL
		SEC
		SBC #$01 ;subtract 1 becaue CHR_SHEET.MOB.LEVEL starts with $1 and TM_1.GOLD_DROP.BASELINE is a lookup table that starts with record $0
		ASL ;X2 (2 byte records)
		TAY ;YREG = TM_1.GOLD_DROP.BASELINE index
		
		LDA TM_1.GOLD_DROP.BASELINE+$0,Y
		STA TM_1.APPLY_PERCENT.PARM_BASE+$0
		LDA TM_1.GOLD_DROP.BASELINE+$1,Y
		STA TM_1.APPLY_PERCENT.PARM_BASE+$1				
		;TM_1.APPLY_PERCENT.PARM.PERCENT: set above		
	JSR TM_1.APPLY.PERCENTAGE.HEX_ONLY
		;RETURN: RESULT(2)	
				
		
.GET.GOLD_DROP.AMOUNT.DONE
			
@END

.PRINT.GOLD_DROPPED
@START


;!!!!! To renable this section, must also uncomment both PLAs in .ADD.GOLD below, and comment out the two LDA RESULT(2) lines
	
		; ;set PRINT.HEX16.RJ	parms
		; LDA RESULT+$1
		; STA BIN+$1
		; PHA ;save gold drop HO

		; LDA RESULT+$0
		; STA BIN+$0
		; PHA ;save gold drop LO
		; ; LDA #$03
		; ; STA HRCG.PAGES	;($01 = hi-res page 1, $02 = hi-res page 2, $03 = both pages)				
		; CLC ;(CLC = don't print CR | SEC = print CR to text window)
	; JSR PRINT.HEX16.RJ	
	
		
	; ;print "gold found"	
		; LDA #TM_1.TEXT_BLOCK.GOLD_FOUND					
		; STA TWF.STRING
		
		; LDA /TM_1.TEXT_BLOCK.GOLD_FOUND		
		; STA TWF.STRING+$1						
	; JSR PRINT.TEXT.WINDOW
					
	
.PRINT.GOLD_DROPPED.DONE


@END

.ADD.GOLD ;to party gold variable
@START
;ADD TOTAL PRICE TO PLAYER GOLD

		;PLA ;restore gold drop LO
		LDA RESULT+$0 ;gold drop LO
		CLC
		ADC TM_1.GOLD_DROP.TALLY+$0 ;TOTAL OF ALL GOLD DROPS (LO)
		STA TM_1.GOLD_DROP.TALLY+$0
		;PLA ;restore gold drop HO
		LDA RESULT+$1 ;gold drop HO
		ADC TM_1.GOLD_DROP.TALLY+$1 ;16-bit add: TOTAL OF ALL GOLD DROPS (HO)
		STA TM_1.GOLD_DROP.TALLY+$1

@END

		

@END



.ITEM_DROP.ENTRANCE
@START

.DOES.ITEM.DROP ;?
@START
;treasure_code / 2 = %item_chance
;if random # < item% chance then drop item


					; ;TROUBLESHOOTING.HOOK: init screen position
								; LDA HTAB
								; STA TWF.HTAB.SAVED
								; LDA VTAB
								; STA TWF.VTAB.SAVED
								
								; LDA #$00
								; STA HTAB
								; STA VTAB
							; JSR UPDATE.CHAR.POS

							
	;treasure code of current mob / 2
		LDA CHR_SHEET.MOB.TREASURE_CODE
		STA DIVIDEND+$0 ;number to be divided
		LDA #$00
		STA DIVIDEND+$1
		LDA #TM_1.ITEM_DROP.PROBABILITY.ADJ
		STA DIVISOR+$0 ;number to divide by
		LDA #$00
		STA DIVISOR+$1
	JSR DIV.16.BCD
		;RETURN VALUE: result+$0 (quotient LO), result+$1 (quotient HO), result+$2 (remainder LO), result+$3 (remainder HO)
		LDA RESULT+$0
		STA CHR_SHEET.MOB.TREASURE_CODE
		
		
					; ;TROUBLESHOOTING.HOOK: print treasure code (probability)
							; LDA CHR_SHEET.MOB.TREASURE_CODE
							; STA BCD+$0
							; LDA #$00
							; STA BCD+$1
						; JSR PRINT_FIXED.BCD_PACKED
					; ;
					; ;
								; LDA #$00
								; STA HTAB
								; LDA #$01
								; STA VTAB
							; JSR UPDATE.CHAR.POS
			
;GET BCD RANDOM NUMNER (!1 - !99)
@START
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
@END	
	
				

	;ACC: BCD random # !1-!99			
	SED ;set decimal mode
	CMP CHR_SHEET.MOB.TREASURE_CODE
	CLD ;clear decimal mode	
	BCC .DOES.ITEM.DROP.DONE ;branch if item drops
	BEQ .DOES.ITEM.DROP.DONE ;""
	JMP .EXIT_TEST ;branch if item doesn't drop. Proceed to next mob record

.DOES.ITEM.DROP.DONE
@END



.PRINT.ITEM_DROPPED
@START
	
		LDA CHR_SHEET.MOB.LEVEL			
	JSR TM_1.ITEM_DROP.RANDOM
		;RETURN: TM_1.DROPPED_ITEM.ITEM_TYPE, TM_1.DROPPED_ITEM.ITEM_CODE

					; sta $bf00
					; lda #$aa
					; ; ldx	#CHR_SHEET.RECORD.READ
					; ; ldy /CHR_SHEET.RECORD.READ
					; ldx TM_1.DROPPED_ITEM.ITEM_TYPE
					; ldy TM_1.DROPPED_ITEM.ITEM_CODE
					; jsr prep.brk
					; brk		


		
		
		

		;TROUBLESHOOTING.HOOK: PRINT TO VIDEO SCREEN: ITEM_TYPE, ITEM_CODE
@START					
					
					
					; LDa #$00
					; STa HTAB
					; STa VTAB
				; JSR UPDATE.CHAR.POS
					
						; LDA TM_1.DROPPED_ITEM.ITEM_TYPE
					; jsr CONVERT.HEX_TO_ASCII
					; lda result+$0
				; JSR COUT
				
					; LDA #$A0 ;ascii = space
				; JSR COUT
				
					; LDA #$00
					; STA RESULT+$1
					
						; LDA TM_1.DROPPED_ITEM.ITEM_CODE
					; JSR CONVERT.HEX_TO_ASCII
					
				; LDA RESULT+$1
				; BNE .PRINT.HO_BYTE

					; LDA #$A0 ;ascii = space
				; JSR COUT
				; JMP .PRINT.LO_BYTE
; .PRINT.HO_BYTE					
					; lda result+$1
				; JSR COUT

; .PRINT.LO_BYTE
					; lda result+$0
				; JSR COUT
				
				; JSR KEYIN
				

	

@END


		LDA TM_1.DROPPED_ITEM.ITEM_TYPE 
		;FILE.ITEM_TABLE.ID: shares memory with TM_1.DROPPED_ITEM.ITEM_CODE, which is set above
	JSR FILE.READ.ITEM_TABLES.ENTRANCE ;master item tables
		;RETURN: FILE.ITEM_TABLE.RECORD.READ($20)

;*OPT** Memory. Convert this section to use Screen Builder.


		
	;JSR PRINT.TEXT.WINDOW.CR



.PRINT.ITEM_FOUND
;VALIDATE ENTRANCE
	LDA TM_1.PICKUP_ALL.FLAG	;($00 = not set, >=$01 = set)
	BNE .PRINT.ITEM_FOUND.DONE

	
	;print "item found"	
		LDA #TM_1.TEXT_BLOCK.ITEM_FOUND					
		STA TWF.STRING
		
		LDA /TM_1.TEXT_BLOCK.ITEM_FOUND		
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW
.PRINT.ITEM_FOUND.DONE

.PRINT.ITEM_NAME	
	;print item name	
		LDA #INV.ITEM_TABLE.ALL.NAME					
		STA TWF.STRING
		
		LDA /INV.ITEM_TABLE.ALL.NAME		
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW

	;print <CR> if pickup flag is set
	;(this is so that next item is printed on the next line because the line feed is otherwise totally handled in teh ITEM_FOUND and PICKUP_PROMPT text blocks)
	LDA TM_1.PICKUP_ALL.FLAG	;($00 = not set, >=$01 = set)
	BEQ .PRINT.ITEM_NAME.DONE
	
	JSR PRINT.TEXT.WINDOW.CR
	
.PRINT.ITEM_NAME.DONE


.PRINT.PICKUP_PROMPT
;VALIDATE ENTRANCE
	LDA TM_1.PICKUP_ALL.FLAG	;($00 = not set, >=$01 = set)
	BNE .PRINT.PICKUP_PROMPT.DONE
	
	;print pickup (Y/N/A) prompt
		LDA #TM_1.TEXT_BLOCK.PICKUP_PROMPT				
		STA TWF.STRING
		
		LDA /TM_1.TEXT_BLOCK.PICKUP_PROMPT		
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW

.PRINT.PICKUP_PROMPT.DONE	
	
@END

.GET.PLAYER_INPUT.PICKUP_ITEM
@START
;VALIDATE ENTRANCE
	LDA TM_1.PICKUP_ALL.FLAG	;($00 = not set, >=$01 = set)
	BNE .GET.PLAYER_INPUT.PICKUP_ITEM.DONE
	

.GET.KEYPRESS
	;get keypress
	JSR KEYIN
	JSR CONVERT.ASCII.UCASE
		;RETURN: ACC = ASCII code of keypress converted to uppercase
	CMP #$D9 	;(Y)es
	BEQ .GET.PLAYER_INPUT.PICKUP_ITEM.DONE
	CMP #$CE	;(N)o
		;ACC = ASCII code of keypress
	BEQ .EXIT_TEST ;if no, then skip player inventory add. Proceed to next mob record
	CMP #$C1	;(A)ll
	BNE .GET.KEYPRESS ;no valid key was pressed, continue loop
	
.SET.PICKUP_ALL
	LDA #$01
	STA TM_1.PICKUP_ALL.FLAG	;($00 = not set, >=$01 = set)
	
	;**FALLS THROUGH**
	
	
.GET.PLAYER_INPUT.PICKUP_ITEM.DONE
@END



.UPDATE.PLAYER_INVENTORY
@START

	;read player inventory data
	JSR TM_1.FILE.READ.INVENTORY_DATA ;reads entire player inventory data table, but only does it once per instances of this loop. (it has a validate entrance routine).
									  ;The benifit of this approach is that if no mobs drop treaures, this loop is never called and the disk load to read the player inventory data is avoided.


.GET.POINTER ;to inventory record
		LDA TM_1.DROPPED_ITEM.ITEM_TYPE
		STA INV__.SEARCH.PLY.INV.ITEM_TYPE
		
		LDA TM_1.DROPPED_ITEM.ITEM_CODE
		STA INV__.SEARCH.PLY.INV.ITEM_ID
	
		LDA #INV.PLAYER.INVENTORY.DATA
		STA PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER+$0

		LDA /INV.PLAYER.INVENTORY.DATA
		STA PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER+$1		
		;JSR KEYIN
	JSR TM_1.SEARCH.PLY_INV
		;RETURN: PLAYER_MERCH.INVENTORY.DATA.POINTER(2) (set to the player inventory record matching the item parms or the first open record)
		
.ADD.ITEM




		;PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER(2): set above
		;FILE.ITEM_TABLE.RECORD.READ($20): set above
		;TM_1.DROPPED_ITEM.ITEM_TYPE: set above
		;TM_1.DROPPED_ITEM.ITEM_CODE: set above
	JSR TM_1.ADD_ITEM.PLAYER_INVENTORY

	
					; jsr keyin
					; lda TM_1.DROPPED_ITEM.ITEM_TYPE
					; sta $bf00
					; lda TM_1.DROPPED_ITEM.ITEM_CODE
					; sta $bf01		
					; lda INV_8.PARM.RE.SEARCH.ITEM_TYPE
					; sta $bf02
					; lda INV_8.PARM.RE.SEARCH.ITEM_ID
					; sta $bf03
					; lda #$aa
					; ldx PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER+$0
					; ldy PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER+$1
					; jsr prep.brk
					; brk


					
@END

@END

.EXIT_TEST
	CPX COMBAT.MOB.START.TOTAL ;do exit test before counter increment at top of loop because COMBAT.MOB.START.TOTAL is a quantity (starts with $1) and the mob sequential counter starts with $1
	BEQ .LOOP.ITEM_DROP.DONE ;branch if all mobs have been processed
	JMP .LOOP.ITEM_DROP ;more mobs to process; continue loop

.LOOP.ITEM_DROP.DONE


.PRINT.GOLD_DROP.TOTAL
@START

;ADD GOLD_DROP TALLY TO PLAYER GOLD
		LDA PLAYER.GOLD+$0
		CLC
		ADC TM_1.GOLD_DROP.TALLY+$0 ;TOTAL OF ALL GOLD DROPS (LO)
		STA PLAYER.GOLD+$0
		LDA	PLAYER.GOLD+$1
		ADC TM_1.GOLD_DROP.TALLY+$1 ;16-bit add: TOTAL OF ALL GOLD DROPS (HO)
		STA PLAYER.GOLD+$1
		
		
				; lda #$aa
				; ldx TM_1.GOLD_DROP.TALLY+$0
				; ldy TM_1.GOLD_DROP.TALLY+$1
				; jsr prep.brk
				; brk
				
			


;PRINT GOLD DROP TOTAL

.PRINT.LEADING.CR
	LDA TM_1.PICKUP_ALL.FLAG	;($00 = not set, >=$01 = set)
	BEQ .PRINT.LEADING.CR.DONE
	JSR PRINT.TEXT.WINDOW.CR ;only print the CR if the player selected to pickup (A)ll items
	
.PRINT.LEADING.CR.DONE	
	
		;set PRINT.HEX16.RJ	parms

		LDA TM_1.GOLD_DROP.TALLY+$0 ;TOTAL OF ALL GOLD DROPS (LO)
		STA BIN+$0
		
		LDA TM_1.GOLD_DROP.TALLY+$1 ;TOTAL OF ALL GOLD DROPS (LO)
		STA BIN+$1
		; LDA #$03
		; STA HRCG.PAGES	;($01 = hi-res page 1, $02 = hi-res page 2, $03 = both pages)
		CLC ;(CLC = don't print CR | SEC = print CR to text window)
	JSR PRINT.HEX16.RJ	
	

	;print "gold found"	
		LDA #TM_1.TEXT_BLOCK.GOLD_FOUND					
		STA TWF.STRING
		
		LDA /TM_1.TEXT_BLOCK.GOLD_FOUND		
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW

	
@END


	
.WRITE.FILE.PLAYER_INVENTORY.DATA ;write player inventory table data to disk
@START
;VALIDATE ENTRANCE
;(skip this routine if the player inventory file isn't already in memory)
	LDA TM_1.PLAYER_INVENTORY.FILE_DATA.FLAG  ;($00 = not in memory | >=$01 in memory)
	BEQ .WRITE_DISK.PLAYER_INVENTORY_TABLE.DONE
	
	
.INIT.WRITE
	JSR PRODOS.IO.RESET.FILE_OFFSET ;resets file offset so that seeks start at beginning of the file
		
;------SEEK FILE------
.SEEK.FILE
	lda #cmd_seek.current_drive
	sta parm.reqcmd

;set seek length (bytes)
	lda #INV.PLAYER_INVENTORY_DATA.SEEK_BYTES
	sta parm.sizelo	
	lda /INV.PLAYER_INVENTORY_DATA.SEEK_BYTES
	sta parm.sizehi

		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO
			
	;**FALLS THROUGH**


;----WRITE FILE-----
.WRITE.FILE
	
	lda #cmd_write.current_drive
	sta parm.reqcmd
		
;set source memory address for write
	lda #INV.PLAYER.INVENTORY.DATA
	sta parm.ldrlo
	lda /INV.PLAYER.INVENTORY.DATA
	sta parm.ldrhi
	
;set write length (bytes)
	lda #INV.INVENTORY_DATA.WRITE_SIZE		;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda /INV.INVENTORY_DATA.WRITE_SIZE		;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi

	;**falls through**
		
.load.file	
		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO		

.WRITE_DISK.PLAYER_INVENTORY_TABLE.DONE
	
@END

	
; COMBAT.READ_WRITE.CHR_SHEET.SPECIAL
; COMBAT.SPECIAL.TOTAL




;.REFRESH.PLAYER_GOLD.ONSCREEN ;**OPT** Memory. Move this routine to main memory and replace ;PRINT "9999" section in DRAW.SCREEN
@START
		; LDA #PLAYER.GOLD.HTAB
		; STA HTAB	
		; LDA #PLAYER.GOLD.VTAB
		; STA	VTAB
	; JSR	UPDATE.CHAR.POS
		
		
		; ;set PRINT.HEX16.RJ	parms
		; LDA PLAYER.GOLD+$0
		; STA BIN+$0
		; LDA PLAYER.GOLD+$1
		; STA BIN+$1
		
		; ; LDA #$03
		; ; STA HRCG.PAGES	;($01 = hi-res page 1, $02 = hi-res page 2, $03 = both pages)
		
		; CLC ;(CLC = don't print CR | SEC = print CR to text window)
	; JSR PRINT.HEX16.RJ	
	
@END
	
	
.EXIT
		JSR KEYIN
		; LDA #$AA
		; JSR PREP.BRK
		; BRK
		

	RTS
	
		
;info I might need


	; ;check health_s
	;LDY #$07 ;set index of CHR_SHEET.POINTER to the health_staus field
	; LDA (CHR_SHEET.POINTER),Y
	; BMI .EXIT_TEST			;COMBAT.S_ENTITY.STATUS.DEAD = $FF
	
	; COMBAT.S_ENTITY.STATUS.DEAD
	

@END

TM_1.ITEM_DROP.RANDOM
@START
;PARAMETERS: ACC = level group
;RETURN: TM_1.DROPPED_ITEM.ITEM_TYPE, TM_1.DROPPED_ITEM.ITEM_CODE

;=====================SUBROUTINE DOCUMENTATION===========================================================================
;
;The level group parm is expected to be the level of the source of the item drop. For example,
;the level of a mob or the level of a treasure chest. 
;
;This routine returns an item (type and code) within the target range. The target range spans two level groups.
;The level group specified by the ACC parm and the level group immediately below (/aka level group-1)
;The purpose of this is so that item drops progress gradually as the player encounters stronger mobs
;since they drop items at their level and one below. So if the player enters a level 2 area, he will see
;level 2 mobs dropping new items (level 2) and items he's seen before from level 1 mobs. 
;
;
;-item_type is selected by a random #
;
;-item_code is selectd by a random #
;	RND.LO = starting item_code of the level group -1*
;	RND.HI = ending item_code of the level group
;
;*If the level group = $1 then RND.LO is set to the starting item_code of the level group
;
;
;=============================================================================================================================


.SAVE.PARAMETERS
	;ACC = parm: level group
	STA TM_1.LEVEL_GROUP
	
	
.SAVE.REGISTERS
	TYA
	PHA
	
.INIT
						;LDA #$08 ;LEVEL
	LDA TM_1.LEVEL_GROUP
	SEC
	SBC #$01 ;subtract $01 because level starts with $01 and the array starts with $00
	; CMP #$00 ;is item group level = $01? 
	; BEQ .AVOID.UNDERFLOW ;if item group level = $01 then branch to avoid underflow
	; SEC
	; SBC #$01 ;subtract $01 to set index to item group level -1
; .AVOID.UNDERFLOW	
	ASL ;X2 (2 byte records)
	TAY ;set TM_1.LEVEL_GROUP.POINTER index
	
	
					; LDX #$AA
					; JSR PREP.BRK
					; BRK
					
.CHOOSE.ITEM_TYPE
@START


;GET RANDOM NUMBER

.CASES_3 ;SPLIT POINTS $55, $AA
	JSR RANDOM.8
	CMP #$55
	BCS .CASES_3.NEXT_TEST
;C3.CASE0
	LDA #$00 ;item type = weapon
	JMP .GET.RANDOM_NUMBER.DONE
.CASES_3.NEXT_TEST
	CMP #$AA
	BCC .C3.CASE2
;C3.CASE1
	LDA #$01 ;item type = armor
	JMP .GET.RANDOM_NUMBER.DONE
.C3.CASE2
	LDA #$02 ;item type = misc

	;**FALLS THROUGH**

.GET.RANDOM_NUMBER.DONE
	
	;ACC = item_type
	STA TM_1.DROPPED_ITEM.ITEM_TYPE
	
	
	
			; ;DRIVER
			; LDA #$02
			; STA TM_1.DROPPED_ITEM.ITEM_TYPE


			
;CONNECT POINTER TO LEVEL GROUP TABLE

	;item_type branch			;**OPT** Memory. Convert this branch-case section to load the offset values from a lookup table built from .DA directives
	
	;ACC = TM_1.DROPPED_ITEM.ITEM_TYPE
	BEQ .ITEM_TYPE0 ;weapon table
	CMP #$01
	BEQ .ITEM_TYPE1 ;armor table
	CMP #$02
	BEQ .ITEM_TYPE2 ;armor table
	
.ERROR_TRAP.ITEM_TYPE
;.CHOOSE.ITEM_TYPE (TM_1.ITEM_DROP.RANDOM) reports unexpectd item_type code
	JSR PREP.BRK
	BRK
	
	
.ITEM_TYPE0 ;weapon table
	LDA #TM_1.WEAPON_TABLE.LEVEL_GROUPS.START
	STA TM_1.LEVEL_GROUP.POINTER+$0
	LDA /TM_1.WEAPON_TABLE.LEVEL_GROUPS.START
	STA TM_1.LEVEL_GROUP.POINTER+$1
	JMP .CHOOSE.ITEM_TYPE.DONE
	
.ITEM_TYPE1 ;armor table
	LDA #TM_1.ARMOR_TABLE.LEVEL_GROUPS.START
	STA TM_1.LEVEL_GROUP.POINTER+$0
	LDA /TM_1.ARMOR_TABLE.LEVEL_GROUPS.START
	STA TM_1.LEVEL_GROUP.POINTER+$1
	JMP .CHOOSE.ITEM_TYPE.DONE
	
.ITEM_TYPE2 ;misc table
	LDA #TM_1.MISC_TABLE.LEVEL_GROUPS.START
	STA TM_1.LEVEL_GROUP.POINTER+$0
	LDA /TM_1.MISC_TABLE.LEVEL_GROUPS.START
	STA TM_1.LEVEL_GROUP.POINTER+$1
	
	;**FALLS THROUGH**

.CHOOSE.ITEM_TYPE.DONE
@END


.CHOOSE.ITEM_CODE
@START
	
;GET ITEM_CODE RANGE FOR RELEVANT 2 LEVEL GROUPS
;(this code detects if the level parm passed to this routine was $01, in which case
;only the level 1 groups is used since there is no level 0)


	;YREG = byte $0 of level group record
	INY ;advance to byte $1 of level group record
	LDA (TM_1.LEVEL_GROUP.POINTER),Y
	STA RND.HI
	CPY #$01 ;if Y-REG =$01 in this position then the level parm to this routine was $01 because if Y-REG us $01 in this position then the Y-REG index started at $00 as calculated in .INIT
	BEQ .SET.RND.LO ;branch to use level group for both RND.HI and RND.LO (i.e. skip the level group-1 code)
	DEY ;backup to byte $0 of level group record
	DEY ;backup to byte $1 of level group-1 record
.SET.RND.LO	
	DEY ;backup to byte $0 of level group-1 record. If the branch to .SET.RND.LO was taken then this DEY backs up to byte $0 of level group record
	LDA (TM_1.LEVEL_GROUP.POINTER),Y
	STA RND.LO



	
;get random number within the item_code range
		
		;RND.LO = set above
		;RND.HI = set above
	JSR RANDOM.8
		;ACC = random # (HEX)	

	STA TM_1.DROPPED_ITEM.ITEM_CODE
	
;TROUBLESHOOTING.HOOK: PRINT ITEM_TYPE, ITEM_CODE
@START					
					; pha ;save random number (item_code)
					
					
					; LDa #$00
					; STa HTAB
					; STa VTAB
				; JSR UPDATE.CHAR.POS
					
					; LDA TM_1.DROPPED_ITEM.ITEM_TYPE
					; jsr CONVERT.HEX_TO_ASCII
					; lda result+$0
				; JSR COUT
				
					; LDA #$A0 ;ascii = space
				; JSR COUT
				
					; LDA #$00
					; STA RESULT+$1
					
					; pla ;restore random number (item_code)
					; jsr CONVERT.HEX_TO_ASCII
					
				; LDA RESULT+$1
				; BNE .PRINT.HO_BYTE

					; LDA #$A0 ;ascii = space
				; JSR COUT
				; JMP .PRINT.LO_BYTE
; .PRINT.HO_BYTE					
					; lda result+$1
				; JSR COUT

; .PRINT.LO_BYTE
					; lda result+$0
				; JSR COUT
				
				; JSR KEYIN
				
				; ; LDx #$AA
			; ; ; ldx RND.LO
			; ; ; ldy RND.HI
			; ; JSR PREP.BRK
			; ; BRK
			
				
				; ; lda TM_1.DROPPED_ITEM.ITEM_TYPE
				; ; cmp #$02
				; ; beq .exit
				; JMP TM_1.ITEM_DROP.RANDOM
	
	

	
@END


	
	
.EXIT

;RESTORE.REGISTERS
	PLA
	TAY
	
	RTS

	
@END

	
@END
	
	


;LOCAL SUBROUTINES
@START

TM_1.SEARCH.PLY_INV
@START
;PARAMETERS: INV__.SEARCH.PLY.INV.ITEM_TYPE, INV__.SEARCH.PLY.INV.ITEM_ID*, PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER(2)**
;ENTRANCE: direct
;RETURN: PLAYER_MERCH.INVENTORY.DATA.POINTER(2)***
;
;*INV__.SEARCH.PLY.INV.ITEM_ID is .EQ to INV_8.PARM.RE.SEARCH.ITEM_ID
;**Init to the top of the player inventory data array
;***PLAYER_MERCH.INVENTORY.DATA.POINTER(2) contains the memory address of the inventory record for the item the search was for -if it was found.
;
		.IN 	c:\my_code\na\source_code\swap.routines.inv.search.ply_inv.asm

		
		
@END


TM_1.ADD_ITEM.PLAYER_INVENTORY
@START
;PARAMETERS: PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER(2), FILE.ITEM_TABLE.RECORD.READ($20), TM_1.DROPPED_ITEM.ITEM_TYPE, TM_1.DROPPED_ITEM.ITEM_CODE
;RETURN: ACC = ($00 = add succeeded, $01 = add failed because it would result in qty > !255), updated INV.PLAYER.INVENTORY.DATA

.SAVE.REGISTERS
	TYA
	PHA
	

.EVALUATE.RECORD ;is record open or occupied?
;(if the player already owns 1 or more units of the item which the player purchased, then an inventory
;record will exist for that item_type, item_id. In that case the returned pointer is to that existing 
;record. If the player doesn't own any units of the item, then the returned pointer is to the first
;open record searching down from the top of the player inventory table)
	
	;is record open?
	LDY #$00
	LDA (PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER),Y
	CMP #$FF ;don't use BMI, this field has item_level in the HO byte which can have a value of $F, setting the high bit. 
	BNE .ADD_TO.EXISTING.RECORD ;branch if record is open
	
.ADD_TO.OPEN.RECORD


;SAVE RECORD DATA

	;LDY #$00 ;init index to PLAYER_MERCH.INVENTORY.DATA at byte $0
	
	;save item level | item_type field (byte $00)
	LDA INV.ITEM_TABLE.ALL.LEVEL	;mask-in item level into HO nibble
	ASL
	ASL
	ASL
	ASL
	ORA TM_1.DROPPED_ITEM.ITEM_TYPE ;load item_type into LO nibble
	STA (PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER),Y
	
			; STA $BF01
			; LDA TM_1.DROPPED_ITEM.ITEM_TYPE
			; STA $BF02
			
			; LDA #$AA
			; LDX PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER+$0
			; LDY PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER+$1
			; JSR PREP.BRK
			; BRK

			
	;save item_id field (byte $01)
	INY ;advance to (byte $01)
	LDA TM_1.DROPPED_ITEM.ITEM_CODE ;load item_id
	STA (PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER),Y
	
	;save quantity field (byte $02)
	INY ;advance to (byte $02)
	LDA #$01 		 ;set qty = $01
	STA (PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER),Y

	;save readied flag #1 (byte $03)	
	INY ;advance to (byte $03)
	LDA #$00
	STA (PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER),Y
	
	;save readied flag #2 (byte $04)
	INY ;advance to (byte $04)
	STA (PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER),Y

	;save equipment field (byte $05)
	INY ;advance to (byte $05)
	LDA INV.ITEM_TABLE.ALL.EQUIPMENT_SLOT	
	STA (PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER),Y
	
			; STY $BF00
			; LDA INV_4.1a.SCREEN_ITEM_LIST.DATA+INV.ITEM_TABLE.ALL.EQUIPMENT_SLOT.OFFSET,X	
			; STA $BF01
			; LDA INV_4.1a.SCREEN_ITEM_LIST.DATA
			; STA $BF02
 			; LDA INV.ITEM_TABLE.ALL.EQUIPMENT_SLOT.OFFSET
			; STA $BF03
			; STX $BF04
			; LDA #$AA
			; LDX PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER+$0
			; LDY PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER+$1
			; JSR PREP.BRK
			; BRK

			
	JMP .ADD_SUCCEEDS
	
.ADD_TO.EXISTING.RECORD

	
	;save quantity field (byte $02)
	LDY #$02
	LDA (PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER),Y ;load item quantity from existing inventory record
	CLC
	ADC #$01 ;add $01 unit to item qty
	CMP (PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER),Y ;compare result of ADC to original value
	BCC .ADD_FAILS ;if the result of the ADC < original qty, then an 8-bit overflow has occured. 
	;ACC = item quantity owned after purchase quantity is added
	STA (PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER),Y ;save item adjusted quantity to existing inventory record
	

.ADD_SUCCEEDS
	LDA #$00 ;($00 = add succeeded, $01 = add failed because it would result in qty > !255)

	JMP .EXIT
	
.ADD_FAILS ;exceeds_capacity
	LDA #$01 ;($00 = add succeeded, $01 = add failed because it would result in qty > !255)

	;**FALLS THROUGH**


	
	
.EXIT
;RESTORE REGISTERS
	PLA
	TAY
	
	RTS
@END

TM_1.FILE.READ.INVENTORY_DATA ;read player inventory data from disk
@START
;VALIDATE ENTRANCE
	LDA TM_1.PLAYER_INVENTORY.FILE_DATA.FLAG ;($00 = not in memory | >=$01 in memory)
	BNE .EXIT ;branch if player inventory data is already in memory
	
	
		;set destination memory address
		lda #INV.PLAYER.INVENTORY.DATA
		sta parm.ldrlo
		lda /INV.PLAYER.INVENTORY.DATA
		sta parm.ldrhi
		
		lda #INV.INVENTORY_DATA.OFFSET 
		sta PARM.SEEK_BYTES+$0	;seek length (LO byte).
		lda /INV.INVENTORY_DATA.OFFSET
		sta PARM.SEEK_BYTES+$1	;seek length (HO byte).
		
		LDA #INV.INVENTORY_DATA.READ_SIZE
		sta PARM.READ_BYTES+$0		;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
		LDA /INV.INVENTORY_DATA.READ_SIZE
		sta PARM.READ_BYTES+$1		;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	JSR PRODOS.IO.READ_CURRENT_FILE

;set player inventory flag  = in memory
	LDA #$01
	STA TM_1.PLAYER_INVENTORY.FILE_DATA.FLAG ;($00 = not in memory | >=$01 in memory)

	
.EXIT	

	RTS
	
@END


TM_1.APPLY.PERCENTAGE.HEX_ONLY ;supports large base numbers. Tested up to !9999
@START
;PARAMETERS: TM_1.APPLY_PERCENT.PARM.PERCENT, TM_1.APPLY_PERCENT.PARM_BASE(2)
;ENTRANCE: TM_1 routines
;RETURN: RESULT(2)

;=====================SUBROUTINE DOCUMENTATION====================================
;
;(to avoid using floating decimals, this routine applies the percent using the
;the formula: %(as integer) * number / 100, which yields the same result as number * percent (as decimal)
;So instead of .75 * 50, this routine would calculate 75*50/100
;
;
;This routine is different than COMBAT.STATS.APPLY.PERCENTAGE:
;	*COMBAT.STATS.APPLY.PERCENTAGE base numbers must be fairly small. The base number * M% cannot exceed a 16-bit number (!65535)
;	*This routine uses MLP.32 and DIV.32 to, in theory, support a 16-bit base number up to !65535*
;	*this routine doesn't support BCD and COMBAT.STATS.APPLY.PERCENTAGE because MLP.32 doesn't support BCD. 
;
;*I've only tested base numbers up to !9999. I tried !20000 and got a garbage result, I'm not sure why. 
;
;
;=================================================================================


;Formula: RESULT(2) = base number * M% / 100


		LDA TM_1.APPLY_PERCENT.PARM.PERCENT
		STA MULR+$0
		LDA #$00
		STA MULR+$1
		STA MULR+$2
		STA MULR+$3
		STA MULND+$2
		STA MULND+$3		
		;			
		LDA TM_1.APPLY_PERCENT.PARM_BASE+$0
		STA MULND+$0
		LDA TM_1.APPLY_PERCENT.PARM_BASE+$1
		;combat only: AND #$7F ;strip off high-bit (see .INIT.TO_HIT.TALLY for docs on the special use of the high-bit for this variable)
		STA MULND+$1		
		;MULND+$2: set above
		;MULND+$3: set above
		;SED (set decimal mode is done above)
		
		
			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; LDA INV_8.APPLY_PERCENT.PARM.PERCENT
			; sta $bf00
			; lda #MULR
			; sta $bf01
			; lda /MULR
			; sta $bf02
			; lda #MULND
			; sta $bf03
			; lda /MULND
			; sta $bf04
			; ;			
			; LDA INV_8.APPLY_PERCENT.PARM_BASE+$0
			; sta $bf05
			; LDA INV_8.APPLY_PERCENT.PARM_BASE+$1					
			; sta $bf06

; .TEMP
			; LDA TEMP
			
			
			
	JSR MLP.32 ;hex only
		;RETURN VALUE: PROD(8)
		;CLD (clear decimal mode is done below)
		LDA PROD+$0 ;load multiplication return value LO byte
		STA DIVIDEND+$0
		LDA PROD+$1 ;load multiplication return value HO byte
		STA DIVIDEND+$1	
		LDA PROD+$2 ;load multiplication return value HO byte
		STA DIVIDEND32+$0
 
 			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP2

			; LDA #PROD
			; sta $bf07
			; lda /prod
			; sta $bf08

		
					; ; stx $bf00
					; ; lda INV_8.GET.ITEM_BASE_PRICE.PARM.ITEM_TYPE 
					; ; sta $bf01
					; ; lda INV_8.GET.ITEM_BASE_PRICE.PARM.ITEM_ID 
					; ; sta $bf02
					; ; lda INV_8.GET.ITEM_BASE_PRICE.RETURN.PRICE+$0
					; ; sta $bf03
					; ; lda INV_8.GET.ITEM_BASE_PRICE.RETURN.PRICE+$1
					; ; sta $bf04
					; ; lda INV_8.GET.ITEM_FINAL_PRICE.PARM.PERCENT
					; ; sta $bf05
					; ; LDA #INV_8.MASTER_PRICE_TABLE.ARMOR.START
					; ; sta $bf06
					; ; LDA /INV_8.MASTER_PRICE_TABLE.ARMOR.START
					; ; sta $bf07					
					; ; ldx prod+$0				
					; ; ldy prod+$1					
					; lda #$aa
					; ldx RESULT
					; ldy RESULT

					; JSR PREP.BRK
					; BRK
; .TEMP2
			; LDA TEMP
			

			
 
 
 
		LDA #$64	;!100
		STA DIVISOR+$0
		STA DIV.16.PARM.MODE ;($00 = 16-bit dividend mode | >=$01 = 32-bit dividend mode)
		LDA #$00
		STA DIVISOR+$1
		STA DIVIDEND32+$1
		
		
			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			
			; lda #$32
			; sta DIVIDEND+$0
			; lda #$64
			; sta DIVIDEND+$1
			; lda #$00
			; STA DIVIDEND32+$0


			; LDA #$64	;!100
			; STA DIVISOR+$0
			; STA DIV.16.PARM.MODE ;($00 = 16-bit dividend mode | >=$01 = 32-bit dividend mode)
			; LDA #$00
			; STA DIVISOR+$1
			; STA DIVIDEND32+$1
		
			; ; lda #DIVIDEND
			; ; sta $bf00
			; ; lda /DIVIDEND
			; ; sta $bf01
			; ; lda #DIVIDEND32
			; ; sta $bf02
			; ; lda /DIVIDEND32
			; ; sta $bf03
			; ; ;			
			; ; LDA #DIVISOR
			; ; sta $bf04
			; ; LDA /DIVISOR				
			; ; sta $bf05
			; ; ;
			; ; LDA #PROD
			; ; sta $bf06
			; ; lda /prod
			; ; sta $bf07
			
					; ; lda #$aa
					; ; ldx RESULT+$0
					; ; ldy RESULT+$1

					; ; JSR PREP.BRK
					; ; BRK

; .TEMP
			; LDA TEMP
			
			
			
			
	JSR DIV.32 ;hex division routine
		;RETURN: RESULT(x)
		
	;**FALLS THROUGH**

	


			
.EXIT
			
	RTS
@END	


TM_1.DRAW.IMAGE
@START
;PARAMETERS:  IMAGE_DRAW.LINE, IMAGE_DRAW.LINE.STOP, IMAGE_DRAW.CURRENT_BYTE, IMAGE_DRAW.STOP_BYTE
;RETURN: NONE
;ENTRANCE: TM_1 routines

;=====================SUBROUTINE DOCUMENTATION====================================
;
;=================================================================================



	

.INIT
	LDA IMAGE_DRAW.START_BYTE
	STA IMAGE_DRAW.CURRENT_BYTE
	
	LDX IMAGE_DRAW.LINE			;LOAD LINE IN X REGISTER	
	LDA #$00
	STA SHP.TBL.CNTR		;START DRAWING AT BEGINNING OF SHAPE TABLE

	; LDA TILE.LINE.START		;THE STARTING LINE OF THE TILES IN THE CURRENT ROW
	; CLC	
	; ADC TILE.DEPTH			;ADD DEPTH OF SHAPE TO STARTING LINE IN HEX (# of lines, not the position of last line....so line positions $0-$F is $10 (!16) total lines)
	; STA TILE.LINE.STOP		;SET THE LAST LINE TO DRAW BASED ON THE DEPTH OF THE TILE

.DRAW.LOOP
	
;==========INLINE CODE GET.LINE.ADDRESS1==================	
	
	LDA PAGE.BACKGROUND		
	CMP #$02
	BEQ .LOOKUP.PAGE2
	
	;**FALLS THROUGH**

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

.DRAW.LINE
	LDY SHP.TBL.CNTR				;LOAD Y WITH SHAPE TABLE COUNTER
	LDA (SHAPE),Y					;LOAD SHAPE BYTE 

	LDY IMAGE_DRAW.CURRENT_BYTE		;SCREEN BYTE INDEX IN Y REGISTER	
	STA (LINE.BASE.ADDR1),Y			;PLOT 
	
	INC SHP.TBL.CNTR				;NEXT SHAPE BYTE
	BNE .NO_FLIP
	INC SHAPE+$1 					;increment HO byte of shape table pointer
	

.NO_FLIP
	INC IMAGE_DRAW.CURRENT_BYTE		;NEXT SCREEN BYTE
	
	CPY IMAGE_DRAW.STOP_BYTE
	BNE .DRAW.LINE
	
	; LDY SHP.TBL.CNTR		;LOAD Y WITH SHAPE TABLE COUNTER
	; LDA (SHAPE),Y			;LOAD SHAPE BYTE (2nd screen byte of the tile)
	; LDY SCREEN.DRAW.CURRENT_BYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	; STA (LINE.BASE.ADDR1),Y				;PLOT (2st screen byte)
	
	; DEC SCREEN.DRAW.CURRENT_BYTE			;SWITCH BACK TO 1ST SCREEN BYTE IN THE TILE	;**OPT** if the counters are updated after the CMP/branch it might save 1 INC SCREEN.DRAW.CURRENT_BYTE instruction in the main loop because SCREEN.DRAW.CURRENT_BYTE will be in 2nd position when this loop ends. 
	; INC SHP.TBL.CNTR		;NEXT SHAPE BYTE

	
	LDA IMAGE_DRAW.START_BYTE		;RESET SCREEN BYTE TO LEFT EDGE OF IMAGE
	STA IMAGE_DRAW.CURRENT_BYTE

	INX								;NEXT IMAGE LINE	
	CPX IMAGE_DRAW.LINE.STOP		;IS IMAGE DONE?							
	BCC .DRAW.LOOP			;IF NO, DRAW NEXT LINE (BCC: is X-REG < CPX value)	

.EXIT

	RTS
	
@END
	
	

@END


;LOCAL TEXT BLOCKS
@START
TM_1.TEXT_BLOCK.GOLD_FOUND			.AZ		-/ Gold Found!/,#$8D,#$8D

;TM_1.TEXT_BLOCK.ITEM_FOUND			.AZ		-/**Item Found!**/,#$8D
TM_1.TEXT_BLOCK.ITEM_FOUND			.AZ		-/You found:/,#$8D

;TM_1.TEXT_BLOCK.PICKUP_PROMPT		.AZ		-#$8D,#$8D,/Pickup Item?/,#$8D,/(Y)es, (N)o, (A)ll/,#$8D,#$8D
;TM_1.TEXT_BLOCK.PICKUP_PROMPT		.AZ		-#$8D,#$8D,/Pickup Item?/,#$8D,/(Y)es/,#$8D,/(N)o/,#$8D,/(A)ll/,#$8D,#$8D

TM_1.TEXT_BLOCK.PICKUP_PROMPT		.AZ		-#$8D,#$8D,/Take it?/,#$8D,/(Y)es/,#$8D,/(N)o/,#$8D,/(A)ll that's left/,#$8D,#$8D


@END

;LOCAL VARIABLES
@START

@END
	
;LOCAL TABLES
@START

TM_1.WEAPON_TABLE.LEVEL_GROUPS.START
@START
;datagram (!16 records, size = $2 bytes): item_ID of 1st item in level group, item_ID of last item in level group

;LEVEL!1_GROUP
INV.WEAPON_TABLE.LEVEL1_GROUP_START_ID = $1
	.DB 	INV.WEAPON_TABLE.LEVEL1_GROUP_START_ID	
INV.WEAPON_TABLE.LEVEL1_GROUP.QTY = INV.WEAPON_TABLE.LEVEL2_GROUP.START-INV.WEAPON_TABLE.LEVEL1_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1	;-1 because the expression calculates a quantity and we want an increment
INV.WEAPON_TABLE.LEVEL1_GROUP.END_ID = INV.WEAPON_TABLE.LEVEL1_GROUP_START_ID+INV.WEAPON_TABLE.LEVEL1_GROUP.QTY
	.DB 	INV.WEAPON_TABLE.LEVEL1_GROUP.END_ID
	
;LEVEL!2_GROUP
INV.WEAPON_TABLE.LEVEL2_GROUP_START_ID = INV.WEAPON_TABLE.LEVEL1_GROUP.END_ID+$1
	.DB 	INV.WEAPON_TABLE.LEVEL2_GROUP_START_ID	
INV.WEAPON_TABLE.LEVEL2_GROUP.QTY = INV.WEAPON_TABLE.LEVEL3_GROUP.START-INV.WEAPON_TABLE.LEVEL2_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1 ;-1 because the expression calculates a quantity and we want an increment
INV.WEAPON_TABLE.LEVEL2_GROUP.END_ID = INV.WEAPON_TABLE.LEVEL2_GROUP_START_ID+INV.WEAPON_TABLE.LEVEL2_GROUP.QTY
	.DB 	INV.WEAPON_TABLE.LEVEL2_GROUP.END_ID
	
;LEVEL!3_GROUP
INV.WEAPON_TABLE.LEVEL3_GROUP_START_ID = INV.WEAPON_TABLE.LEVEL2_GROUP.END_ID+$1
	.DB 	INV.WEAPON_TABLE.LEVEL3_GROUP_START_ID	
INV.WEAPON_TABLE.LEVEL3_GROUP.QTY = INV.WEAPON_TABLE.LEVEL4_GROUP.START-INV.WEAPON_TABLE.LEVEL3_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1 ;-1 because the expression calculates a quantity and we want an increment
INV.WEAPON_TABLE.LEVEL3_GROUP.END_ID = INV.WEAPON_TABLE.LEVEL3_GROUP_START_ID+INV.WEAPON_TABLE.LEVEL3_GROUP.QTY
	.DB 	INV.WEAPON_TABLE.LEVEL3_GROUP.END_ID
	
	
;LEVEL!4_GROUP
INV.WEAPON_TABLE.LEVEL4_GROUP_START_ID = INV.WEAPON_TABLE.LEVEL3_GROUP.END_ID+$1
	.DB 	INV.WEAPON_TABLE.LEVEL4_GROUP_START_ID	
INV.WEAPON_TABLE.LEVEL4_GROUP.QTY = INV.WEAPON_TABLE.LEVEL5_GROUP.START-INV.WEAPON_TABLE.LEVEL4_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1 ;-1 because the expression calculates a quantity and we want an increment
INV.WEAPON_TABLE.LEVEL4_GROUP.END_ID = INV.WEAPON_TABLE.LEVEL4_GROUP_START_ID+INV.WEAPON_TABLE.LEVEL4_GROUP.QTY
	.DB 	INV.WEAPON_TABLE.LEVEL4_GROUP.END_ID	
	
	
;LEVEL!5_GROUP
INV.WEAPON_TABLE.LEVEL5_GROUP_START_ID = INV.WEAPON_TABLE.LEVEL4_GROUP.END_ID+$1
	.DB 	INV.WEAPON_TABLE.LEVEL5_GROUP_START_ID	
INV.WEAPON_TABLE.LEVEL5_GROUP.QTY = INV.WEAPON_TABLE.LEVEL6_GROUP.START-INV.WEAPON_TABLE.LEVEL5_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1 ;-1 because the expression calculates a quantity and we want an increment
INV.WEAPON_TABLE.LEVEL5_GROUP.END_ID = INV.WEAPON_TABLE.LEVEL5_GROUP_START_ID+INV.WEAPON_TABLE.LEVEL5_GROUP.QTY
	.DB 	INV.WEAPON_TABLE.LEVEL5_GROUP.END_ID	
	
	
;LEVEL!6_GROUP
INV.WEAPON_TABLE.LEVEL6_GROUP_START_ID = INV.WEAPON_TABLE.LEVEL5_GROUP.END_ID+$1
	.DB 	INV.WEAPON_TABLE.LEVEL6_GROUP_START_ID	
INV.WEAPON_TABLE.LEVEL6_GROUP.QTY = INV.WEAPON_TABLE.LEVEL7_GROUP.START-INV.WEAPON_TABLE.LEVEL6_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1 ;-1 because the expression calculates a quantity and we want an increment
INV.WEAPON_TABLE.LEVEL6_GROUP.END_ID = INV.WEAPON_TABLE.LEVEL6_GROUP_START_ID+INV.WEAPON_TABLE.LEVEL6_GROUP.QTY
	.DB 	INV.WEAPON_TABLE.LEVEL6_GROUP.END_ID


;LEVEL!7_GROUP
INV.WEAPON_TABLE.LEVEL7_GROUP_START_ID = INV.WEAPON_TABLE.LEVEL6_GROUP.END_ID+$1
	.DB 	INV.WEAPON_TABLE.LEVEL7_GROUP_START_ID	
INV.WEAPON_TABLE.LEVEL7_GROUP.QTY = INV.WEAPON_TABLE.LEVEL8_GROUP.START-INV.WEAPON_TABLE.LEVEL7_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1 ;-1 because the expression calculates a quantity and we want an increment
INV.WEAPON_TABLE.LEVEL7_GROUP.END_ID = INV.WEAPON_TABLE.LEVEL7_GROUP_START_ID+INV.WEAPON_TABLE.LEVEL7_GROUP.QTY
	.DB 	INV.WEAPON_TABLE.LEVEL7_GROUP.END_ID


;LEVEL!8_GROUP
INV.WEAPON_TABLE.LEVEL8_GROUP_START_ID = INV.WEAPON_TABLE.LEVEL7_GROUP.END_ID+$1
	.DB 	INV.WEAPON_TABLE.LEVEL8_GROUP_START_ID	
INV.WEAPON_TABLE.LEVEL8_GROUP.QTY = INV.WEAPON_TABLE.LEVEL9_GROUP.START-INV.WEAPON_TABLE.LEVEL8_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1 ;-1 because the expression calculates a quantity and we want an increment
INV.WEAPON_TABLE.LEVEL8_GROUP.END_ID = INV.WEAPON_TABLE.LEVEL8_GROUP_START_ID+INV.WEAPON_TABLE.LEVEL8_GROUP.QTY
	.DB 	INV.WEAPON_TABLE.LEVEL8_GROUP.END_ID
	
;LEVEL!9_GROUP
INV.WEAPON_TABLE.LEVEL9_GROUP_START_ID = INV.WEAPON_TABLE.LEVEL8_GROUP.END_ID+$1
	.DB 	INV.WEAPON_TABLE.LEVEL9_GROUP_START_ID	
INV.WEAPON_TABLE.LEVEL9_GROUP.QTY = INV.WEAPON_TABLE.LEVEL10_GROUP.START-INV.WEAPON_TABLE.LEVEL9_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1 ;-1 because the expression calculates a quantity and we want an increment
INV.WEAPON_TABLE.LEVEL9_GROUP.END_ID = INV.WEAPON_TABLE.LEVEL9_GROUP_START_ID+INV.WEAPON_TABLE.LEVEL9_GROUP.QTY
	.DB 	INV.WEAPON_TABLE.LEVEL9_GROUP.END_ID

;LEVEL!10_GROUP
INV.WEAPON_TABLE.LEVEL10_GROUP_START_ID = INV.WEAPON_TABLE.LEVEL9_GROUP.END_ID+$1
	.DB 	INV.WEAPON_TABLE.LEVEL10_GROUP_START_ID	
INV.WEAPON_TABLE.LEVEL10_GROUP.QTY = INV.WEAPON_TABLE.LEVEL11_GROUP.START-INV.WEAPON_TABLE.LEVEL10_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1 ;-1 because the expression calculates a quantity and we want an increment
INV.WEAPON_TABLE.LEVEL10_GROUP.END_ID = INV.WEAPON_TABLE.LEVEL10_GROUP_START_ID+INV.WEAPON_TABLE.LEVEL10_GROUP.QTY
	.DB 	INV.WEAPON_TABLE.LEVEL10_GROUP.END_ID

;LEVEL!11_GROUP
INV.WEAPON_TABLE.LEVEL11_GROUP_START_ID = INV.WEAPON_TABLE.LEVEL10_GROUP.END_ID+$1
	.DB 	INV.WEAPON_TABLE.LEVEL11_GROUP_START_ID	
INV.WEAPON_TABLE.LEVEL11_GROUP.QTY = INV.WEAPON_TABLE.LEVEL12_GROUP.START-INV.WEAPON_TABLE.LEVEL11_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1 ;-1 because the expression calculates a quantity and we want an increment
INV.WEAPON_TABLE.LEVEL11_GROUP.END_ID = INV.WEAPON_TABLE.LEVEL11_GROUP_START_ID+INV.WEAPON_TABLE.LEVEL11_GROUP.QTY
	.DB 	INV.WEAPON_TABLE.LEVEL11_GROUP.END_ID
	
;LEVEL!12_GROUP
INV.WEAPON_TABLE.LEVEL12_GROUP_START_ID = INV.WEAPON_TABLE.LEVEL11_GROUP.END_ID+$1
	.DB 	INV.WEAPON_TABLE.LEVEL12_GROUP_START_ID	
INV.WEAPON_TABLE.LEVEL12_GROUP.QTY = INV.WEAPON_TABLE.LEVEL13_GROUP.START-INV.WEAPON_TABLE.LEVEL12_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1 ;-1 because the expression calculates a quantity and we want an increment
INV.WEAPON_TABLE.LEVEL12_GROUP.END_ID = INV.WEAPON_TABLE.LEVEL12_GROUP_START_ID+INV.WEAPON_TABLE.LEVEL12_GROUP.QTY
	.DB 	INV.WEAPON_TABLE.LEVEL12_GROUP.END_ID
	
;LEVEL!13_GROUP
INV.WEAPON_TABLE.LEVEL13_GROUP_START_ID = INV.WEAPON_TABLE.LEVEL12_GROUP.END_ID+$1
	.DB 	INV.WEAPON_TABLE.LEVEL13_GROUP_START_ID	
INV.WEAPON_TABLE.LEVEL13_GROUP.QTY = INV.WEAPON_TABLE.LEVEL14_GROUP.START-INV.WEAPON_TABLE.LEVEL13_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1 ;-1 because the expression calculates a quantity and we want an increment
INV.WEAPON_TABLE.LEVEL13_GROUP.END_ID = INV.WEAPON_TABLE.LEVEL13_GROUP_START_ID+INV.WEAPON_TABLE.LEVEL13_GROUP.QTY
	.DB 	INV.WEAPON_TABLE.LEVEL13_GROUP.END_ID

;LEVEL!14_GROUP
INV.WEAPON_TABLE.LEVEL14_GROUP_START_ID = INV.WEAPON_TABLE.LEVEL13_GROUP.END_ID+$1
	.DB 	INV.WEAPON_TABLE.LEVEL14_GROUP_START_ID	
INV.WEAPON_TABLE.LEVEL14_GROUP.QTY = INV.WEAPON_TABLE.LEVEL15_GROUP.START-INV.WEAPON_TABLE.LEVEL14_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1 ;-1 because the expression calculates a quantity and we want an increment
INV.WEAPON_TABLE.LEVEL14_GROUP.END_ID = INV.WEAPON_TABLE.LEVEL14_GROUP_START_ID+INV.WEAPON_TABLE.LEVEL14_GROUP.QTY
	.DB 	INV.WEAPON_TABLE.LEVEL14_GROUP.END_ID

;LEVEL!15_GROUP
INV.WEAPON_TABLE.LEVEL15_GROUP_START_ID = INV.WEAPON_TABLE.LEVEL14_GROUP.END_ID+$1
	.DB 	INV.WEAPON_TABLE.LEVEL15_GROUP_START_ID	
INV.WEAPON_TABLE.LEVEL15_GROUP.QTY = INV.WEAPON_TABLE.LEVEL16_GROUP.START-INV.WEAPON_TABLE.LEVEL15_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1 ;-1 because the expression calculates a quantity and we want an increment
INV.WEAPON_TABLE.LEVEL15_GROUP.END_ID = INV.WEAPON_TABLE.LEVEL15_GROUP_START_ID+INV.WEAPON_TABLE.LEVEL15_GROUP.QTY
	.DB 	INV.WEAPON_TABLE.LEVEL15_GROUP.END_ID

;LEVEL!16_GROUP
INV.WEAPON_TABLE.LEVEL16_GROUP_START_ID = INV.WEAPON_TABLE.LEVEL15_GROUP.END_ID+$1
	.DB 	INV.WEAPON_TABLE.LEVEL16_GROUP_START_ID	
INV.WEAPON_TABLE.LEVEL16_GROUP.QTY = INV.WEAPON_TABLE.LEVEL_GROUPS.END-INV.WEAPON_TABLE.LEVEL16_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1 ;-1 because the expression calculates a quantity and we want an increment
INV.WEAPON_TABLE.LEVEL16_GROUP.END_ID = INV.WEAPON_TABLE.LEVEL16_GROUP_START_ID+INV.WEAPON_TABLE.LEVEL16_GROUP.QTY
	.DB 	INV.WEAPON_TABLE.LEVEL16_GROUP.END_ID

@END


TM_1.ARMOR_TABLE.LEVEL_GROUPS.START
@START
;datagram (!16 records, size = $2 bytes): item_ID of 1st item in level group, item_ID of last item in level group

;LEVEL!1_GROUP
INV.ARMOR_TABLE.LEVEL1_GROUP_START_ID = $1
	.DB 	INV.ARMOR_TABLE.LEVEL1_GROUP_START_ID	
INV.ARMOR_TABLE.LEVEL1_GROUP.QTY = INV.ARMOR_TABLE.LEVEL2_GROUP.START-INV.ARMOR_TABLE.LEVEL1_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1	;-1 because the expression calculates a quantity and we want an increment
INV.ARMOR_TABLE.LEVEL1_GROUP.END_ID = INV.ARMOR_TABLE.LEVEL1_GROUP_START_ID+INV.ARMOR_TABLE.LEVEL1_GROUP.QTY
	.DB 	INV.ARMOR_TABLE.LEVEL1_GROUP.END_ID
	
;LEVEL!2_GROUP
INV.ARMOR_TABLE.LEVEL2_GROUP_START_ID = INV.ARMOR_TABLE.LEVEL1_GROUP.END_ID+$1
	.DB 	INV.ARMOR_TABLE.LEVEL2_GROUP_START_ID	
INV.ARMOR_TABLE.LEVEL2_GROUP.QTY = INV.ARMOR_TABLE.LEVEL3_GROUP.START-INV.ARMOR_TABLE.LEVEL2_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1 ;-1 because the expression calculates a quantity and we want an increment
INV.ARMOR_TABLE.LEVEL2_GROUP.END_ID = INV.ARMOR_TABLE.LEVEL2_GROUP_START_ID+INV.ARMOR_TABLE.LEVEL2_GROUP.QTY
	.DB 	INV.ARMOR_TABLE.LEVEL2_GROUP.END_ID
	
;LEVEL!3_GROUP
INV.ARMOR_TABLE.LEVEL3_GROUP_START_ID = INV.ARMOR_TABLE.LEVEL2_GROUP.END_ID+$1
	.DB 	INV.ARMOR_TABLE.LEVEL3_GROUP_START_ID	
INV.ARMOR_TABLE.LEVEL3_GROUP.QTY = INV.ARMOR_TABLE.LEVEL4_GROUP.START-INV.ARMOR_TABLE.LEVEL3_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1 ;-1 because the expression calculates a quantity and we want an increment
INV.ARMOR_TABLE.LEVEL3_GROUP.END_ID = INV.ARMOR_TABLE.LEVEL3_GROUP_START_ID+INV.ARMOR_TABLE.LEVEL3_GROUP.QTY
	.DB 	INV.ARMOR_TABLE.LEVEL3_GROUP.END_ID
	
	
;LEVEL!4_GROUP
INV.ARMOR_TABLE.LEVEL4_GROUP_START_ID = INV.ARMOR_TABLE.LEVEL3_GROUP.END_ID+$1
	.DB 	INV.ARMOR_TABLE.LEVEL4_GROUP_START_ID	
INV.ARMOR_TABLE.LEVEL4_GROUP.QTY = INV.ARMOR_TABLE.LEVEL5_GROUP.START-INV.ARMOR_TABLE.LEVEL4_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1 ;-1 because the expression calculates a quantity and we want an increment
INV.ARMOR_TABLE.LEVEL4_GROUP.END_ID = INV.ARMOR_TABLE.LEVEL4_GROUP_START_ID+INV.ARMOR_TABLE.LEVEL4_GROUP.QTY
	.DB 	INV.ARMOR_TABLE.LEVEL4_GROUP.END_ID	
	
	
;LEVEL!5_GROUP
INV.ARMOR_TABLE.LEVEL5_GROUP_START_ID = INV.ARMOR_TABLE.LEVEL4_GROUP.END_ID+$1
	.DB 	INV.ARMOR_TABLE.LEVEL5_GROUP_START_ID	
INV.ARMOR_TABLE.LEVEL5_GROUP.QTY = INV.ARMOR_TABLE.LEVEL6_GROUP.START-INV.ARMOR_TABLE.LEVEL5_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1 ;-1 because the expression calculates a quantity and we want an increment
INV.ARMOR_TABLE.LEVEL5_GROUP.END_ID = INV.ARMOR_TABLE.LEVEL5_GROUP_START_ID+INV.ARMOR_TABLE.LEVEL5_GROUP.QTY
	.DB 	INV.ARMOR_TABLE.LEVEL5_GROUP.END_ID	
	
	
;LEVEL!6_GROUP
INV.ARMOR_TABLE.LEVEL6_GROUP_START_ID = INV.ARMOR_TABLE.LEVEL5_GROUP.END_ID+$1
	.DB 	INV.ARMOR_TABLE.LEVEL6_GROUP_START_ID	
INV.ARMOR_TABLE.LEVEL6_GROUP.QTY = INV.ARMOR_TABLE.LEVEL7_GROUP.START-INV.ARMOR_TABLE.LEVEL6_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1 ;-1 because the expression calculates a quantity and we want an increment
INV.ARMOR_TABLE.LEVEL6_GROUP.END_ID = INV.ARMOR_TABLE.LEVEL6_GROUP_START_ID+INV.ARMOR_TABLE.LEVEL6_GROUP.QTY
	.DB 	INV.ARMOR_TABLE.LEVEL6_GROUP.END_ID


;LEVEL!7_GROUP
INV.ARMOR_TABLE.LEVEL7_GROUP_START_ID = INV.ARMOR_TABLE.LEVEL6_GROUP.END_ID+$1
	.DB 	INV.ARMOR_TABLE.LEVEL7_GROUP_START_ID	
INV.ARMOR_TABLE.LEVEL7_GROUP.QTY = INV.ARMOR_TABLE.LEVEL8_GROUP.START-INV.ARMOR_TABLE.LEVEL7_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1 ;-1 because the expression calculates a quantity and we want an increment
INV.ARMOR_TABLE.LEVEL7_GROUP.END_ID = INV.ARMOR_TABLE.LEVEL7_GROUP_START_ID+INV.ARMOR_TABLE.LEVEL7_GROUP.QTY
	.DB 	INV.ARMOR_TABLE.LEVEL7_GROUP.END_ID


;LEVEL!8_GROUP
INV.ARMOR_TABLE.LEVEL8_GROUP_START_ID = INV.ARMOR_TABLE.LEVEL7_GROUP.END_ID+$1
	.DB 	INV.ARMOR_TABLE.LEVEL8_GROUP_START_ID	
INV.ARMOR_TABLE.LEVEL8_GROUP.QTY = INV.ARMOR_TABLE.LEVEL9_GROUP.START-INV.ARMOR_TABLE.LEVEL8_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1 ;-1 because the expression calculates a quantity and we want an increment
INV.ARMOR_TABLE.LEVEL8_GROUP.END_ID = INV.ARMOR_TABLE.LEVEL8_GROUP_START_ID+INV.ARMOR_TABLE.LEVEL8_GROUP.QTY
	.DB 	INV.ARMOR_TABLE.LEVEL8_GROUP.END_ID
	
;LEVEL!9_GROUP
INV.ARMOR_TABLE.LEVEL9_GROUP_START_ID = INV.ARMOR_TABLE.LEVEL8_GROUP.END_ID+$1
	.DB 	INV.ARMOR_TABLE.LEVEL9_GROUP_START_ID	
INV.ARMOR_TABLE.LEVEL9_GROUP.QTY = INV.ARMOR_TABLE.LEVEL10_GROUP.START-INV.ARMOR_TABLE.LEVEL9_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1 ;-1 because the expression calculates a quantity and we want an increment
INV.ARMOR_TABLE.LEVEL9_GROUP.END_ID = INV.ARMOR_TABLE.LEVEL9_GROUP_START_ID+INV.ARMOR_TABLE.LEVEL9_GROUP.QTY
	.DB 	INV.ARMOR_TABLE.LEVEL9_GROUP.END_ID

;LEVEL!10_GROUP
INV.ARMOR_TABLE.LEVEL10_GROUP_START_ID = INV.ARMOR_TABLE.LEVEL9_GROUP.END_ID+$1
	.DB 	INV.ARMOR_TABLE.LEVEL10_GROUP_START_ID	
INV.ARMOR_TABLE.LEVEL10_GROUP.QTY = INV.ARMOR_TABLE.LEVEL11_GROUP.START-INV.ARMOR_TABLE.LEVEL10_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1 ;-1 because the expression calculates a quantity and we want an increment
INV.ARMOR_TABLE.LEVEL10_GROUP.END_ID = INV.ARMOR_TABLE.LEVEL10_GROUP_START_ID+INV.ARMOR_TABLE.LEVEL10_GROUP.QTY
	.DB 	INV.ARMOR_TABLE.LEVEL10_GROUP.END_ID

;LEVEL!11_GROUP
INV.ARMOR_TABLE.LEVEL11_GROUP_START_ID = INV.ARMOR_TABLE.LEVEL10_GROUP.END_ID+$1
	.DB 	INV.ARMOR_TABLE.LEVEL11_GROUP_START_ID	
INV.ARMOR_TABLE.LEVEL11_GROUP.QTY = INV.ARMOR_TABLE.LEVEL12_GROUP.START-INV.ARMOR_TABLE.LEVEL11_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1 ;-1 because the expression calculates a quantity and we want an increment
INV.ARMOR_TABLE.LEVEL11_GROUP.END_ID = INV.ARMOR_TABLE.LEVEL11_GROUP_START_ID+INV.ARMOR_TABLE.LEVEL11_GROUP.QTY
	.DB 	INV.ARMOR_TABLE.LEVEL11_GROUP.END_ID
	
;LEVEL!12_GROUP
INV.ARMOR_TABLE.LEVEL12_GROUP_START_ID = INV.ARMOR_TABLE.LEVEL11_GROUP.END_ID+$1
	.DB 	INV.ARMOR_TABLE.LEVEL12_GROUP_START_ID	
INV.ARMOR_TABLE.LEVEL12_GROUP.QTY = INV.ARMOR_TABLE.LEVEL13_GROUP.START-INV.ARMOR_TABLE.LEVEL12_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1 ;-1 because the expression calculates a quantity and we want an increment
INV.ARMOR_TABLE.LEVEL12_GROUP.END_ID = INV.ARMOR_TABLE.LEVEL12_GROUP_START_ID+INV.ARMOR_TABLE.LEVEL12_GROUP.QTY
	.DB 	INV.ARMOR_TABLE.LEVEL12_GROUP.END_ID
	
;LEVEL!13_GROUP
INV.ARMOR_TABLE.LEVEL13_GROUP_START_ID = INV.ARMOR_TABLE.LEVEL12_GROUP.END_ID+$1
	.DB 	INV.ARMOR_TABLE.LEVEL13_GROUP_START_ID	
INV.ARMOR_TABLE.LEVEL13_GROUP.QTY = INV.ARMOR_TABLE.LEVEL14_GROUP.START-INV.ARMOR_TABLE.LEVEL13_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1 ;-1 because the expression calculates a quantity and we want an increment
INV.ARMOR_TABLE.LEVEL13_GROUP.END_ID = INV.ARMOR_TABLE.LEVEL13_GROUP_START_ID+INV.ARMOR_TABLE.LEVEL13_GROUP.QTY
	.DB 	INV.ARMOR_TABLE.LEVEL13_GROUP.END_ID

;LEVEL!14_GROUP
INV.ARMOR_TABLE.LEVEL14_GROUP_START_ID = INV.ARMOR_TABLE.LEVEL13_GROUP.END_ID+$1
	.DB 	INV.ARMOR_TABLE.LEVEL14_GROUP_START_ID	
INV.ARMOR_TABLE.LEVEL14_GROUP.QTY = INV.ARMOR_TABLE.LEVEL15_GROUP.START-INV.ARMOR_TABLE.LEVEL14_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1 ;-1 because the expression calculates a quantity and we want an increment
INV.ARMOR_TABLE.LEVEL14_GROUP.END_ID = INV.ARMOR_TABLE.LEVEL14_GROUP_START_ID+INV.ARMOR_TABLE.LEVEL14_GROUP.QTY
	.DB 	INV.ARMOR_TABLE.LEVEL14_GROUP.END_ID

;LEVEL!15_GROUP
INV.ARMOR_TABLE.LEVEL15_GROUP_START_ID = INV.ARMOR_TABLE.LEVEL14_GROUP.END_ID+$1
	.DB 	INV.ARMOR_TABLE.LEVEL15_GROUP_START_ID	
INV.ARMOR_TABLE.LEVEL15_GROUP.QTY = INV.ARMOR_TABLE.LEVEL16_GROUP.START-INV.ARMOR_TABLE.LEVEL15_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1 ;-1 because the expression calculates a quantity and we want an increment
INV.ARMOR_TABLE.LEVEL15_GROUP.END_ID = INV.ARMOR_TABLE.LEVEL15_GROUP_START_ID+INV.ARMOR_TABLE.LEVEL15_GROUP.QTY
	.DB 	INV.ARMOR_TABLE.LEVEL15_GROUP.END_ID

;LEVEL!16_GROUP
INV.ARMOR_TABLE.LEVEL16_GROUP_START_ID = INV.ARMOR_TABLE.LEVEL15_GROUP.END_ID+$1
	.DB 	INV.ARMOR_TABLE.LEVEL16_GROUP_START_ID	
INV.ARMOR_TABLE.LEVEL16_GROUP.QTY = INV.ARMOR_TABLE.LEVEL_GROUPS.END-INV.ARMOR_TABLE.LEVEL16_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1 ;-1 because the expression calculates a quantity and we want an increment
INV.ARMOR_TABLE.LEVEL16_GROUP.END_ID = INV.ARMOR_TABLE.LEVEL16_GROUP_START_ID+INV.ARMOR_TABLE.LEVEL16_GROUP.QTY
	.DB 	INV.ARMOR_TABLE.LEVEL16_GROUP.END_ID

@END

TM_1.MISC_TABLE.LEVEL_GROUPS.START
@START
;datagram (!16 records, size = $2 bytes): item_ID of 1st item in level group, item_ID of last item in level group

;LEVEL!1_GROUP
INV.MISC_TABLE.LEVEL1_GROUP_START_ID = $40 ;****MISC TABLE STARTS WITH $40 (to skip over the spell books)
	.DB 	INV.MISC_TABLE.LEVEL1_GROUP_START_ID	
INV.MISC_TABLE.LEVEL1_GROUP.QTY = INV.MISC_TABLE.LEVEL2_GROUP.START-INV.MISC_TABLE.LEVEL1_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1	;-1 because the expression calculates a quantity and we want an increment
INV.MISC_TABLE.LEVEL1_GROUP.END_ID = INV.MISC_TABLE.LEVEL1_GROUP_START_ID+INV.MISC_TABLE.LEVEL1_GROUP.QTY
	.DB 	INV.MISC_TABLE.LEVEL1_GROUP.END_ID
	
;LEVEL!2_GROUP
INV.MISC_TABLE.LEVEL2_GROUP_START_ID = INV.MISC_TABLE.LEVEL1_GROUP.END_ID+$1
	.DB 	INV.MISC_TABLE.LEVEL2_GROUP_START_ID	
INV.MISC_TABLE.LEVEL2_GROUP.QTY = INV.MISC_TABLE.LEVEL3_GROUP.START-INV.MISC_TABLE.LEVEL2_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1 ;-1 because the expression calculates a quantity and we want an increment
INV.MISC_TABLE.LEVEL2_GROUP.END_ID = INV.MISC_TABLE.LEVEL2_GROUP_START_ID+INV.MISC_TABLE.LEVEL2_GROUP.QTY
	.DB 	INV.MISC_TABLE.LEVEL2_GROUP.END_ID
	
;LEVEL!3_GROUP
INV.MISC_TABLE.LEVEL3_GROUP_START_ID = INV.MISC_TABLE.LEVEL2_GROUP.END_ID+$1
	.DB 	INV.MISC_TABLE.LEVEL3_GROUP_START_ID	
INV.MISC_TABLE.LEVEL3_GROUP.QTY = INV.MISC_TABLE.LEVEL4_GROUP.START-INV.MISC_TABLE.LEVEL3_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1 ;-1 because the expression calculates a quantity and we want an increment
INV.MISC_TABLE.LEVEL3_GROUP.END_ID = INV.MISC_TABLE.LEVEL3_GROUP_START_ID+INV.MISC_TABLE.LEVEL3_GROUP.QTY
	.DB 	INV.MISC_TABLE.LEVEL3_GROUP.END_ID
	
	
;LEVEL!4_GROUP
INV.MISC_TABLE.LEVEL4_GROUP_START_ID = INV.MISC_TABLE.LEVEL3_GROUP.END_ID+$1
	.DB 	INV.MISC_TABLE.LEVEL4_GROUP_START_ID	
INV.MISC_TABLE.LEVEL4_GROUP.QTY = INV.MISC_TABLE.LEVEL5_GROUP.START-INV.MISC_TABLE.LEVEL4_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1 ;-1 because the expression calculates a quantity and we want an increment
INV.MISC_TABLE.LEVEL4_GROUP.END_ID = INV.MISC_TABLE.LEVEL4_GROUP_START_ID+INV.MISC_TABLE.LEVEL4_GROUP.QTY
	.DB 	INV.MISC_TABLE.LEVEL4_GROUP.END_ID	
	
	
;LEVEL!5_GROUP
INV.MISC_TABLE.LEVEL5_GROUP_START_ID = INV.MISC_TABLE.LEVEL4_GROUP.END_ID+$1
	.DB 	INV.MISC_TABLE.LEVEL5_GROUP_START_ID	
INV.MISC_TABLE.LEVEL5_GROUP.QTY = INV.MISC_TABLE.LEVEL6_GROUP.START-INV.MISC_TABLE.LEVEL5_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1 ;-1 because the expression calculates a quantity and we want an increment
INV.MISC_TABLE.LEVEL5_GROUP.END_ID = INV.MISC_TABLE.LEVEL5_GROUP_START_ID+INV.MISC_TABLE.LEVEL5_GROUP.QTY
	.DB 	INV.MISC_TABLE.LEVEL5_GROUP.END_ID	
	
	
;LEVEL!6_GROUP
INV.MISC_TABLE.LEVEL6_GROUP_START_ID = INV.MISC_TABLE.LEVEL5_GROUP.END_ID+$1
	.DB 	INV.MISC_TABLE.LEVEL6_GROUP_START_ID	
INV.MISC_TABLE.LEVEL6_GROUP.QTY = INV.MISC_TABLE.LEVEL7_GROUP.START-INV.MISC_TABLE.LEVEL6_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1 ;-1 because the expression calculates a quantity and we want an increment
INV.MISC_TABLE.LEVEL6_GROUP.END_ID = INV.MISC_TABLE.LEVEL6_GROUP_START_ID+INV.MISC_TABLE.LEVEL6_GROUP.QTY
	.DB 	INV.MISC_TABLE.LEVEL6_GROUP.END_ID


;LEVEL!7_GROUP
INV.MISC_TABLE.LEVEL7_GROUP_START_ID = INV.MISC_TABLE.LEVEL6_GROUP.END_ID+$1
	.DB 	INV.MISC_TABLE.LEVEL7_GROUP_START_ID	
INV.MISC_TABLE.LEVEL7_GROUP.QTY = INV.MISC_TABLE.LEVEL8_GROUP.START-INV.MISC_TABLE.LEVEL7_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1 ;-1 because the expression calculates a quantity and we want an increment
INV.MISC_TABLE.LEVEL7_GROUP.END_ID = INV.MISC_TABLE.LEVEL7_GROUP_START_ID+INV.MISC_TABLE.LEVEL7_GROUP.QTY
	.DB 	INV.MISC_TABLE.LEVEL7_GROUP.END_ID


;LEVEL!8_GROUP
INV.MISC_TABLE.LEVEL8_GROUP_START_ID = INV.MISC_TABLE.LEVEL7_GROUP.END_ID+$1
	.DB 	INV.MISC_TABLE.LEVEL8_GROUP_START_ID	
INV.MISC_TABLE.LEVEL8_GROUP.QTY = INV.MISC_TABLE.LEVEL9_GROUP.START-INV.MISC_TABLE.LEVEL8_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1 ;-1 because the expression calculates a quantity and we want an increment
INV.MISC_TABLE.LEVEL8_GROUP.END_ID = INV.MISC_TABLE.LEVEL8_GROUP_START_ID+INV.MISC_TABLE.LEVEL8_GROUP.QTY
	.DB 	INV.MISC_TABLE.LEVEL8_GROUP.END_ID
	
;LEVEL!9_GROUP
INV.MISC_TABLE.LEVEL9_GROUP_START_ID = INV.MISC_TABLE.LEVEL8_GROUP.END_ID+$1
	.DB 	INV.MISC_TABLE.LEVEL9_GROUP_START_ID	
INV.MISC_TABLE.LEVEL9_GROUP.QTY = INV.MISC_TABLE.LEVEL10_GROUP.START-INV.MISC_TABLE.LEVEL9_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1 ;-1 because the expression calculates a quantity and we want an increment
INV.MISC_TABLE.LEVEL9_GROUP.END_ID = INV.MISC_TABLE.LEVEL9_GROUP_START_ID+INV.MISC_TABLE.LEVEL9_GROUP.QTY
	.DB 	INV.MISC_TABLE.LEVEL9_GROUP.END_ID

;LEVEL!10_GROUP
INV.MISC_TABLE.LEVEL10_GROUP_START_ID = INV.MISC_TABLE.LEVEL9_GROUP.END_ID+$1
	.DB 	INV.MISC_TABLE.LEVEL10_GROUP_START_ID	
INV.MISC_TABLE.LEVEL10_GROUP.QTY = INV.MISC_TABLE.LEVEL11_GROUP.START-INV.MISC_TABLE.LEVEL10_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1 ;-1 because the expression calculates a quantity and we want an increment
INV.MISC_TABLE.LEVEL10_GROUP.END_ID = INV.MISC_TABLE.LEVEL10_GROUP_START_ID+INV.MISC_TABLE.LEVEL10_GROUP.QTY
	.DB 	INV.MISC_TABLE.LEVEL10_GROUP.END_ID

;LEVEL!11_GROUP
INV.MISC_TABLE.LEVEL11_GROUP_START_ID = INV.MISC_TABLE.LEVEL10_GROUP.END_ID+$1
	.DB 	INV.MISC_TABLE.LEVEL11_GROUP_START_ID	
INV.MISC_TABLE.LEVEL11_GROUP.QTY = INV.MISC_TABLE.LEVEL12_GROUP.START-INV.MISC_TABLE.LEVEL11_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1 ;-1 because the expression calculates a quantity and we want an increment
INV.MISC_TABLE.LEVEL11_GROUP.END_ID = INV.MISC_TABLE.LEVEL11_GROUP_START_ID+INV.MISC_TABLE.LEVEL11_GROUP.QTY
	.DB 	INV.MISC_TABLE.LEVEL11_GROUP.END_ID
	
;LEVEL!12_GROUP
INV.MISC_TABLE.LEVEL12_GROUP_START_ID = INV.MISC_TABLE.LEVEL11_GROUP.END_ID+$1
	.DB 	INV.MISC_TABLE.LEVEL12_GROUP_START_ID	
INV.MISC_TABLE.LEVEL12_GROUP.QTY = INV.MISC_TABLE.LEVEL13_GROUP.START-INV.MISC_TABLE.LEVEL12_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1 ;-1 because the expression calculates a quantity and we want an increment
INV.MISC_TABLE.LEVEL12_GROUP.END_ID = INV.MISC_TABLE.LEVEL12_GROUP_START_ID+INV.MISC_TABLE.LEVEL12_GROUP.QTY
	.DB 	INV.MISC_TABLE.LEVEL12_GROUP.END_ID
	
;LEVEL!13_GROUP
INV.MISC_TABLE.LEVEL13_GROUP_START_ID = INV.MISC_TABLE.LEVEL12_GROUP.END_ID+$1
	.DB 	INV.MISC_TABLE.LEVEL13_GROUP_START_ID	
INV.MISC_TABLE.LEVEL13_GROUP.QTY = INV.MISC_TABLE.LEVEL14_GROUP.START-INV.MISC_TABLE.LEVEL13_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1 ;-1 because the expression calculates a quantity and we want an increment
INV.MISC_TABLE.LEVEL13_GROUP.END_ID = INV.MISC_TABLE.LEVEL13_GROUP_START_ID+INV.MISC_TABLE.LEVEL13_GROUP.QTY
	.DB 	INV.MISC_TABLE.LEVEL13_GROUP.END_ID

;LEVEL!14_GROUP
INV.MISC_TABLE.LEVEL14_GROUP_START_ID = INV.MISC_TABLE.LEVEL13_GROUP.END_ID+$1
	.DB 	INV.MISC_TABLE.LEVEL14_GROUP_START_ID	
INV.MISC_TABLE.LEVEL14_GROUP.QTY = INV.MISC_TABLE.LEVEL15_GROUP.START-INV.MISC_TABLE.LEVEL14_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1 ;-1 because the expression calculates a quantity and we want an increment
INV.MISC_TABLE.LEVEL14_GROUP.END_ID = INV.MISC_TABLE.LEVEL14_GROUP_START_ID+INV.MISC_TABLE.LEVEL14_GROUP.QTY
	.DB 	INV.MISC_TABLE.LEVEL14_GROUP.END_ID

;LEVEL!15_GROUP
INV.MISC_TABLE.LEVEL15_GROUP_START_ID = INV.MISC_TABLE.LEVEL14_GROUP.END_ID+$1
	.DB 	INV.MISC_TABLE.LEVEL15_GROUP_START_ID	
INV.MISC_TABLE.LEVEL15_GROUP.QTY = INV.MISC_TABLE.LEVEL16_GROUP.START-INV.MISC_TABLE.LEVEL15_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1 ;-1 because the expression calculates a quantity and we want an increment
INV.MISC_TABLE.LEVEL15_GROUP.END_ID = INV.MISC_TABLE.LEVEL15_GROUP_START_ID+INV.MISC_TABLE.LEVEL15_GROUP.QTY
	.DB 	INV.MISC_TABLE.LEVEL15_GROUP.END_ID

;LEVEL!16_GROUP
INV.MISC_TABLE.LEVEL16_GROUP_START_ID = INV.MISC_TABLE.LEVEL15_GROUP.END_ID+$1
	.DB 	INV.MISC_TABLE.LEVEL16_GROUP_START_ID	
INV.MISC_TABLE.LEVEL16_GROUP.QTY = INV.MISC_TABLE.LEVEL_GROUPS.END-INV.MISC_TABLE.LEVEL16_GROUP.START/FILE.ITEM_TABLE.RECORD.SIZE-1 ;-1 because the expression calculates a quantity and we want an increment
INV.MISC_TABLE.LEVEL16_GROUP.END_ID = INV.MISC_TABLE.LEVEL16_GROUP_START_ID+INV.MISC_TABLE.LEVEL16_GROUP.QTY
	.DB 	INV.MISC_TABLE.LEVEL16_GROUP.END_ID

@END


;testing: setup for level 1 = !100 gold, +100 gold for each level. 
TM_1.GOLD_DROP.BASELINE		.HS	64.00.C8.00.2C.01.90.01.F4.01.58.02.BC.02.20.03.84.03.E8.03.4C.04.B0.04.14.05.78.05.DC.05.40.06	
;Datagram (2 byte record): gold_baseline LO byte, gold_baseline HO byte


;****CAN BE REMOVED***	
	; .DA 	#INV.WEAPON_TABLE.LEVEL2_GROUP.START
	; .DA 	/INV.WEAPON_TABLE.LEVEL2_GROUP.START

	; .DA 	#INV.WEAPON_TABLE.LEVEL3_GROUP.START
	; .DA 	/INV.WEAPON_TABLE.LEVEL3_GROUP.START
	
	; .DA 	#INV.WEAPON_TABLE.LEVEL4_GROUP.START
	; .DA 	/INV.WEAPON_TABLE.LEVEL4_GROUP.START
	
	; .DA 	#INV.WEAPON_TABLE.LEVEL5_GROUP.START
	; .DA 	/INV.WEAPON_TABLE.LEVEL5_GROUP.START
	
	; .DA 	#INV.WEAPON_TABLE.LEVEL6_GROUP.START
	; .DA 	/INV.WEAPON_TABLE.LEVEL6_GROUP.START
	
	; .DA 	#INV.WEAPON_TABLE.LEVEL7_GROUP.START
	; .DA 	/INV.WEAPON_TABLE.LEVEL7_GROUP.START
	
	; .DA 	#INV.WEAPON_TABLE.LEVEL8_GROUP.START
	; .DA 	/INV.WEAPON_TABLE.LEVEL8_GROUP.START
	
	; .DA 	#INV.WEAPON_TABLE.LEVEL9_GROUP.START
	; .DA 	/INV.WEAPON_TABLE.LEVEL9_GROUP.START
	
	; .DA 	#INV.WEAPON_TABLE.LEVEL10_GROUP.START
	; .DA 	/INV.WEAPON_TABLE.LEVEL10_GROUP.START
	
	; .DA 	#INV.WEAPON_TABLE.LEVEL11_GROUP.START
	; .DA 	/INV.WEAPON_TABLE.LEVEL11_GROUP.START
	
	; .DA 	#INV.WEAPON_TABLE.LEVEL12_GROUP.START
	; .DA 	/INV.WEAPON_TABLE.LEVEL12_GROUP.START
	
	; .DA 	#INV.WEAPON_TABLE.LEVEL13_GROUP.START
	; .DA 	/INV.WEAPON_TABLE.LEVEL13_GROUP.START

	; .DA 	#INV.WEAPON_TABLE.LEVEL14_GROUP.START
	; .DA 	/INV.WEAPON_TABLE.LEVEL14_GROUP.START

	; .DA 	#INV.WEAPON_TABLE.LEVEL15_GROUP.START
	; .DA 	/INV.WEAPON_TABLE.LEVEL15_GROUP.START

	; .DA 	#INV.WEAPON_TABLE.LEVEL16_GROUP.START
	; .DA 	/INV.WEAPON_TABLE.LEVEL16_GROUP.START
	
	
;TM_1.ARMOR_TABLE.LEVEL_GROUPS.START
;TM_1.MISC_TABLE.LEVEL_GROUPS.START

@END

;LOCAL SHAPE TABLES
@START
SHAPE_TABLE.TREASURE_PILE ;**OPT** if memory becomes tight in this module, try reading the image from disk, rendering it, and then reading the rest of the module from disk, clobbering the image
TREASURE_PILE.Row0.0	.HS	D5.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.AA
TREASURE_PILE.Row0.1	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row0.2	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row0.3	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row0.4	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row0.5	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row0.6	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row0.7	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row0.8	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row0.9	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row0.10	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row0.11	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row0.12	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row0.13	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row0.14	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row0.15	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
		
TREASURE_PILE.Row1.0	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row1.1	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row1.2	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row1.3	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row1.4	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row1.5	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row1.6	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row1.7	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row1.8	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row1.9	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row1.10	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row1.11	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row1.12	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row1.13	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row1.14	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row1.15	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
		
TREASURE_PILE.Row2.0	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row2.1	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row2.2	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row2.3	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row2.4	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row2.5	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row2.6	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row2.7	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row2.8	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row2.9	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row2.10	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row2.11	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row2.12	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row2.13	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row2.14	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row2.15	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
		
TREASURE_PILE.Row3.0	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row3.1	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row3.2	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row3.3	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row3.4	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row3.5	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row3.6	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row3.7	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row3.8	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row3.9	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row3.10	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row3.11	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row3.12	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row3.13	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row3.14	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row3.15	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
		
TREASURE_PILE.Row4.0	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row4.1	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row4.2	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row4.3	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row4.4	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row4.5	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row4.6	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row4.7	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row4.8	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row4.9	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row4.10	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row4.11	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row4.12	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row4.13	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row4.14	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row4.15	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
		
TREASURE_PILE.Row5.0	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row5.1	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row5.2	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row5.3	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row5.4	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row5.5	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row5.6	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row5.7	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row5.8	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row5.9	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row5.10	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row5.11	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row5.12	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row5.13	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row5.14	.HS	81.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.A0
TREASURE_PILE.Row5.15	.HS	D5.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.AA



@END


