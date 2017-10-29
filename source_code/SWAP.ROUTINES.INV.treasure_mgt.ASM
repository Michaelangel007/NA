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


TM_1.COMBAT.ITEM_DROP ;only mobs drop items, not specials. 
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
	STA ANIMATION.FRAME_STATE ;this is to force TM_1.LOCAL.ANIMATION_UPDATE to start at the 1st frame. This is useful because the the animated tiles embbedded within TM_1.DRAW.IMAGE use the 4th (last) frame. 
							  ;(this is the same variable used for tile grid animation but alterting here should be fine becasue once the player exits the treasure drop, returning to the main map, a full screen draw will be done)

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
	JSR TM_1.LOCAL.ANIMATION_UPDATE
	LDA $C000 ;check last keypress
	BPL .GET.KEYPRESS
	STA $C010
	
		
	JSR CONVERT.ASCII.UCASE
		;RETURN: ACC = ASCII code of keypress converted to uppercase
	CMP #$D9 	;(Y)es
	BEQ .GET.PLAYER_INPUT.PICKUP_ITEM.DONE
	CMP #$CE	;(N)o
		;ACC = ASCII code of keypress
	BEQ .EXIT_TEST ;if no, then skip player inventory add. Proceed to next mob record
	CMP #$9B 	;ESC (skip rest of item drops. Print gold and exit)
	BEQ .LOOP.ITEM_DROP.DONE
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

;
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
;The "tiles" within the treasure image start on odd numbered screen bytes which reverse the odd/even color pattern from the regular tile grid.
;
;=================================================================================



	

.INIT
	LDA IMAGE_DRAW.START_BYTE
	STA IMAGE_DRAW.CURRENT_BYTE
	
	LDX IMAGE_DRAW.LINE		;LOAD LINE IN X REGISTER	
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
					
TM_1.LOCAL.ANIMATION_UPDATE ;animates part of treasure image
@START	
;=====================SUBROUTINE DOCUMENTATION===========================================================================
;
;-ADD ANIMATED TILE
;*increase constant TM_1.TREASURE_IMAGE.ANIMATED_TILE.QTY
;*update lookup tables with the upper left screen byte, first line, and shape table base address for the new tile
;		*TREASURE_PILE.SHAPE_BASE_ADDRESS_TABLE.LO, TREASURE_PILE.SHAPE_BASE_ADDRESS_TABLE.HO
;		*TREASURE_PILE.SBYTE_TABLE
;		*TREASURE_PILE.LINE_TABLE
;
;The "tiles" within the treasure image are not all on the tile grid (i.e the two screen bytes normally used for the time and gold amount are used for the treasure image),
;thus DRAW.TILE is used directly instead of DRAW.TILE.SINGLE which uses the sindex of a tile on screen to set the screen byte and line parameters. 
;
;The "tiles" start on odd numbered screen bytes which reverse the odd/even color pattern from the regular tile grid.
;
;=============================================================================================================================


	
.INIT	
;Sync Hi-Res pages for the treasure image portion of screen
		LDA #$15
		STA DRAW.START_BYTE

		LDA #$27
		STA DRAW.STOP_BYTE.ALT
	JSR TM_1.SYNC_PAGES ;sync player inventory window

	
;Sync Hi-Res pages for tile grid portion of screen			
;(otherwise the background page will contain the previous animation frame for the player characters, causing a 2 frame animation
;effect on the PCs)
	JSR COPY.SCREEN.ENTRANCE	
	
	
	LDX #$00 ;init animatio tile # to 1st tile
.ANIMATION.LOOP

;Calculate shape table address for current animation frame (current tile)


	LDA ANIMATION.FRAME_STATE		;LOAD CURRENT ANIMATION FRAME	
	ASL ;X2						
	ASL ;X4							
	ASL ;X8							
	ASL ;X16						
	ASL ;X32
					;STA TEMP
					
	CLC
	ADC TREASURE_PILE.SHAPE_BASE_ADDRESS_TABLE.LO,X ;load Lo byte of the shape table base address (not adjusted for animation frame) for current animated tile
	;STA AUX_MOVE.START				;SAVE BASE ADDRESS AS START ADDRESS FOR AUX MEMORY MOVE

	STA SHAPE+$0 ;save to DRAW.TILE.SINGLE parm for zpage pointer (LO byte) to shape table. 
	
	LDA TREASURE_PILE.SHAPE_BASE_ADDRESS_TABLE.HO,X ;load Lo byte of the shape table base address (not adjusted for animation frame) for current animated tile
	ADC #$00	;16-bit add
	STA SHAPE+$1 ;save to DRAW.TILE.SINGLE parm for zpage pointer (LO byte) to shape table. 


;Draw animation frame (current tile)
;	set parms for SBYTE, LINE etc (from lookup table)

		LDA TREASURE_PILE.LINE_TABLE,X	;load line address of current tile
		STA TILE.LINE
		STA TILE.LINE.START
		LDA TREASURE_PILE.SBYTE_TABLE,X	;load screen byte of current tile
		STA SCREEN.DRAW.CURRENT_BYTE
	JSR DRAW.TILE ;draw to background page


;EXIT TEST	
	INX ;increment current animation tile
	
	CPX #TM_1.TREASURE_IMAGE.ANIMATED_TILE.QTY
	BNE .ANIMATION.LOOP

	

;UPDATE VIDEO SCREEN
		LDA #$FF
	JSR WAIT.LOOP

	
	JSR FLIP.PAGE
	
					;jsr keyin

;INCREMENT CURRENT ANIMATION FRAME	
			LDA ANIMATION.FRAME_STATE
			CMP #ANIMATION.TOTAL_FRAMES
			BNE .NOFLIP
		
			LDA #$00
			STA ANIMATION.FRAME_STATE
			STA ANIMATION.DELAY.OVERRIDE ;reset to default: off. 
			JMP .DONE
.NOFLIP
	INC ANIMATION.FRAME_STATE

.DONE

						;jmp .INIT
						
	
.EXIT
	RTS

@END



	
	
TM_1.SYNC_PAGES ;Sync hi-res pages (treasure image portion of the screen only)
@START
;PARAMETERS: DRAW.START_BYTE, DRAW.STOP_BYTE


;=====================SUBROUTINE DOCUMENTATION====================================
;
;Copies the foreground page to the background page for the inventory text window only. 
;
;=================================================================================

;SAVE REGISTERS	
	TXA
	PHA
	TYA
	PHA

.INIT.SCREEN					;setup screen byte, line, depth	

	LDA #$08
	STA LINE.START				
	
	LDA #$70
	STA LINE.STOP				;last row of text characters to sync +$08. 

		LDX LINE.START			;hi-res line number
.LINELOOP	
	LDY DRAW.START_BYTE			;reset screen byte index to the left edge of text window	
	
.GET.LINE.ADDRESS
		;X-REG: hi-res line number
		LDA PAGE.FOREGROUND
	JSR GET.LINE.ADDRESS1
		LDA PAGE.BACKGROUND
	JSR GET.LINE.ADDRESS2

	
.COPYLOOP
;COPY TEXT CHARACTER DATA
	LDA (LINE.BASE.ADDR1),Y		;COPY FROM: foreground page	
	STA (LINE.BASE.ADDR2),Y		;COPY TO: background page
	
	INY							;advance to next screen byte

	CPY DRAW.STOP_BYTE.ALT		;is line complete? (i.e. have all screen bytes in the line been scrolled?)
	BNE .COPYLOOP
	
	INX
	CPX LINE.STOP				;has end of inventory window been reached?
	BCC .LINELOOP				;if no, scroll next line
	
	;**FALLS THROUGH**

.SCROLL.COMPLETE


;RESTORE REGISTERS	
	PLA
	TAY
	PLA
	TAX
	
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
@START
TREASURE_PILE.Row0.0	.HS	80.80.80.80.F0.8B.80.80.80.80.80.80.80.8C.80.80.80.80
TREASURE_PILE.Row0.1	.HS	C0.80.80.80.D4.8A.80.80.80.80.80.80.80.98.80.80.80.80
TREASURE_PILE.Row0.2	.HS	D0.82.80.80.D4.82.80.80.80.84.80.80.80.98.80.80.80.80
TREASURE_PILE.Row0.3	.HS	D0.82.80.80.D4.83.80.80.80.8C.80.80.80.98.80.80.80.80
TREASURE_PILE.Row0.4	.HS	C0.82.80.80.F0.82.80.80.80.8C.80.80.80.9E.80.80.80.80
TREASURE_PILE.Row0.5	.HS	C0.82.80.80.D0.83.80.80.80.8C.80.80.80.8F.80.80.80.80
TREASURE_PILE.Row0.6	.HS	C0.8A.80.80.F0.82.80.80.80.8C.80.80.C0.8B.80.80.80.80
TREASURE_PILE.Row0.7	.HS	D0.8A.80.80.D0.83.80.80.80.9C.80.80.E0.89.80.AA.9D.80
TREASURE_PILE.Row0.8	.HS	D0.AA.80.80.F0.82.80.80.80.9C.80.80.E0.88.C0.AB.BD.81
TREASURE_PILE.Row0.9	.HS	40.2A.80.80.D0.83.80.80.80.9C.80.80.F0.88.80.AA.9D.87
TREASURE_PILE.Row0.10	.HS	D0.8A.80.80.F0.82.80.80.80.9C.80.80.B0.88.80.88.84.87
TREASURE_PILE.Row0.11	.HS	50.2A.80.80.D0.83.80.80.80.98.80.80.B8.88.80.88.84.84
TREASURE_PILE.Row0.12	.HS	D4.BA.80.80.F0.8A.80.80.80.B8.80.80.98.88.80.88.84.84
TREASURE_PILE.Row0.13	.HS	50.3E.80.80.D0.AA.80.80.80.F8.80.80.9C.84.80.A8.85.84
TREASURE_PILE.Row0.14	.HS	F0.BB.80.80.D4.AA.80.80.80.F8.87.80.8C.84.80.88.84.8E
TREASURE_PILE.Row0.15	.HS	F0.BA.80.80.D5.AA.80.80.80.F8.8F.80.8C.84.80.80.80.8E
		
TREASURE_PILE.Row1.0	.HS	F0.9E.80.80.D5.A2.80.80.80.B8.98.80.86.84.80.80.80.84
TREASURE_PILE.Row1.1	.HS	E0.9B.80.80.D5.A6.80.80.B8.A8.80.80.86.84.80.80.80.84
TREASURE_PILE.Row1.2	.HS	E0.9A.80.80.F1.A3.80.80.FE.A8.80.80.86.84.98.B8.80.84
TREASURE_PILE.Row1.3	.HS	E0.9F.80.80.E1.A7.80.80.E7.A8.80.80.83.82.8E.CE.B1.86
TREASURE_PILE.Row1.4	.HS	80.BF.80.80.F1.83.80.80.BE.A8.80.80.83.C2.F9.B3.E6.B7
TREASURE_PILE.Row1.5	.HS	80.B8.80.80.E1.87.80.80.C7.FF.8F.80.83.82.E6.CC.99.9F
TREASURE_PILE.Row1.6	.HS	80.B8.80.80.F1.83.80.FE.FF.FF.9F.C0.81.C2.99.B3.C6.9F
TREASURE_PILE.Row1.7	.HS	80.E0.80.80.E0.87.FE.FF.FF.C0.99.C0.81.B2.E6.CC.F9.9F
TREASURE_PILE.Row1.8	.HS	80.E0.80.88.F0.C3.FF.81.80.B0.98.C0.81.C2.99.B3.E6.FF
TREASURE_PILE.Row1.9	.HS	80.C0.81.98.E0.E7.83.80.C0.BA.98.E0.80.B1.E6.CC.99.9F
TREASURE_PILE.Row1.10	.HS	80.C0.81.9C.F0.F3.83.80.D5.98.98.E0.80.C1.99.B3.E6.8F
TREASURE_PILE.Row1.11	.HS	80.80.83.9C.E0.F3.C3.AA.81.98.98.E0.80.B1.E6.CC.99.8E
TREASURE_PILE.Row1.12	.HS	80.80.83.9C.E1.9B.D7.80.C0.8E.98.B0.80.C1.99.B3.E6.98
TREASURE_PILE.Row1.13	.HS	80.80.86.CC.E1.9F.83.A8.D5.8C.98.B0.80.B1.E6.CC.99.80
TREASURE_PILE.Row1.14	.HS	80.80.86.C8.E3.8F.83.8A.8A.86.8C.B0.80.81.98.B3.E6.80
TREASURE_PILE.Row1.15	.HS	80.80.8C.C8.E3.8F.D7.82.EA.87.8C.B0.C0.B0.E6.CC.99.80
		
TREASURE_PILE.Row2.0	.HS	80.80.8C.C8.E3.87.83.E0.EA.87.8C.98.C0.80.98.B3.E0.80
TREASURE_PILE.Row2.1	.HS	80.80.98.88.E3.87.83.F8.EA.AB.8C.98.C0.B0.E6.CC.99.80
TREASURE_PILE.Row2.2	.HS	80.98.98.90.E1.87.D7.FE.EA.A3.8D.98.C0.80.98.B3.E0.80
TREASURE_PILE.Row2.3	.HS	80.F0.B0.90.E1.CF.81.EE.FA.AB.8D.98.C0.B0.E6.CC.99.80
TREASURE_PILE.Row2.4	.HS	80.F0.B0.90.E1.CD.81.FE.FE.BF.95.98.C0.80.98.B3.E0.80
TREASURE_PILE.Row2.5	.HS	80.E0.E0.90.E1.CD.C1.FE.FF.FF.94.9C.80.B0.E6.CC.99.80
TREASURE_PILE.Row2.6	.HS	80.C0.E1.90.E1.CD.F5.F7.BF.EF.95.9C.A0.80.98.B3.E0.80
TREASURE_PILE.Row2.7	.HS	80.80.C2.91.E5.CD.F1.FF.FF.FB.95.8C.A0.B0.E6.CC.99.80
TREASURE_PILE.Row2.8	.HS	80.80.C4.A9.E5.CD.F9.FF.FF.DE.D7.8C.A0.80.D4.BA.E0.80
TREASURE_PILE.Row2.9	.HS	80.80.88.AB.E5.CD.B9.EE.FB.FF.C7.8C.D0.AA.84.E8.99.80
TREASURE_PILE.Row2.10	.HS	80.80.90.AF.E5.CD.F9.FF.DF.AA.D5.8C.94.A0.81.E2.E0.80
TREASURE_PILE.Row2.11	.HS	80.80.A0.AE.F5.AD.C1.EE.DF.AA.D5.9B.C5.80.85.E8.98.80
TREASURE_PILE.Row2.12	.HS	80.80.C0.FC.F7.AD.C5.FE.D5.F8.D5.9A.94.A0.D5.EA.E0.80
TREASURE_PILE.Row2.13	.HS	80.80.E0.AE.FF.8D.C5.B2.D5.FF.FF.9B.D5.AA.95.A0.F9.80
TREASURE_PILE.Row2.14	.HS	80.80.F0.FD.F5.F1.95.AA.F5.AE.F0.8F.D4.AA.85.82.F9.80
TREASURE_PILE.Row2.15	.HS	80.80.FD.EB.8F.AE.95.AA.BD.AB.DD.AA.D5.AA.95.A0.E5.80
		
TREASURE_PILE.Row3.0	.HS	80.80.F5.FF.FF.AB.C4.BE.EF.BA.C1.AA.D4.AA.D1.AA.85.80
TREASURE_PILE.Row3.1	.HS	80.88.D5.FA.D7.8A.A5.FF.FB.AA.B5.80.D1.AA.D4.AA.85.80
TREASURE_PILE.Row3.2	.HS	80.80.D5.AA.D5.A2.C7.AA.D5.AA.D0.8A.85.80.D1.AA.85.80
TREASURE_PILE.Row3.3	.HS	80.80.D4.AA.D5.EA.A5.AA.D5.A0.F5.8A.D4.AA.D5.AA.85.80
TREASURE_PILE.Row3.4	.HS	80.80.C4.AA.95.BA.85.A8.C1.AA.A5.8A.D5.AA.D1.AA.81.80
TREASURE_PILE.Row3.5	.HS	80.82.D6.A8.D5.BA.A1.80.D5.AA.F5.8A.D4.AA.C5.AA.84.80
TREASURE_PILE.Row3.6	.HS	80.C0.D7.8A.D1.AE.81.A0.D5.8A.A5.8A.F5.A8.91.80.85.80
TREASURE_PILE.Row3.7	.HS	E0.E0.DF.AA.D5.AA.A1.A0.F5.8B.F5.8A.F4.BF.D4.AA.85.80
TREASURE_PILE.Row3.8	.HS	80.E0.D7.AB.F5.AB.81.A0.F5.8A.A5.8A.C1.E1.D5.AA.85.80
TREASURE_PILE.Row3.9	.HS	80.C0.D5.BA.DF.AA.A1.A0.D5.8A.F5.8A.C5.B3.D1.8A.C5.80
TREASURE_PILE.Row3.10	.HS	80.80.D5.AA.D5.AA.A5.A0.F5.AA.D5.8A.D4.BF.D5.AA.85.80
TREASURE_PILE.Row3.11	.HS	C0.C0.D5.AA.D5.AA.85.A0.D5.AA.D5.8A.DF.BF.D1.AA.E1.80
TREASURE_PILE.Row3.12	.HS	80.C0.D5.AA.D5.AA.A5.A0.F5.AA.D5.FE.DF.BE.C5.AA.84.80
TREASURE_PILE.Row3.13	.HS	80.C0.D5.AA.D5.AA.85.A0.D5.AA.D5.FA.D7.BE.95.80.85.80
TREASURE_PILE.Row3.14	.HS	C0.C2.D5.AA.D5.AA.85.A0.F5.AA.D5.EA.D7.B2.D1.AF.C5.81
TREASURE_PILE.Row3.15	.HS	80.E0.D5.AA.D5.AA.85.B8.FD.AB.D5.EA.DF.AC.D4.AF.85.80
		
TREASURE_PILE.Row4.0	.HS	80.E0.D5.AA.D5.AA.C5.EE.F5.AA.D1.EA.D3.B9.F4.B8.85.80
TREASURE_PILE.Row4.1	.HS	B0.C0.D5.AA.D5.AA.E5.2B.77.AA.C4.AA.E0.B9.F4.BC.85.80
TREASURE_PILE.Row4.2	.HS	B0.80.D5.AA.D5.AA.F9.AF.FD.AA.94.FA.87.B3.F5.BF.85.80
TREASURE_PILE.Row4.3	.HS	FC.81.D5.AA.D5.AA.BD.66.75.AA.D4.F8.BF.FE.F5.A7.81.90
TREASURE_PILE.Row4.4	.HS	B0.8C.D4.AA.D5.AA.8F.A7.F5.8A.D5.E2.FF.FC.D7.B2.D1.80
TREASURE_PILE.Row4.5	.HS	B4.80.D5.AA.D5.82.C3.2F.75.8E.D5.8A.FF.FC.D5.E6.80.80
TREASURE_PILE.Row4.6	.HS	80.80.D1.AA.D5.AA.FF.AB.D5.CB.DD.AA.FE.F9.D5.CE.83.99
TREASURE_PILE.Row4.7	.HS	80.84.D5.AA.D5.AA.7D.03.51.CA.F5.AA.FC.F3.F5.FE.8F.80
TREASURE_PILE.Row4.8	.HS	80.B8.D5.AA.D5.E8.F5.89.D5.A2.DC.AA.FC.F3.D5.FE.9F.80
TREASURE_PILE.Row4.9	.HS	84.9C.C4.AA.C5.FA.55.2B.55.B8.D5.AB.FC.F3.C3.FF.9F.80
TREASURE_PILE.Row4.10	.HS	82.8F.D4.AA.D5.F8.AD.AB.95.FE.E5.8E.FC.E7.C7.FF.9F.80
TREASURE_PILE.Row4.11	.HS	C2.8F.D4.A8.D5.2E.51.2F.45.AE.F7.8A.FE.CF.E7.FF.9F.86
TREASURE_PILE.Row4.12	.HS	C2.9F.C0.AA.D1.AB.D5.B5.85.8B.AD.8F.FE.9F.E7.FF.8F.80
TREASURE_PILE.Row4.13	.HS	E4.83.D0.82.F5.0A.75.2A.65.A8.BD.87.FF.9F.E7.FF.9F.80
TREASURE_PILE.Row4.14	.HS	88.83.D0.A8.F5.AA.D5.AA.C5.A2.95.E1.FF.9F.C7.F9.99.80
TREASURE_PILE.Row4.15	.HS	80.86.C0.AA.FD.6A.55.6A.65.8A.D7.ED.FF.9F.C7.E7.9F.80
		
TREASURE_PILE.Row5.0	.HS	80.90.80.80.DE.AE.F7.AA.C5.BA.A4.E4.FF.9F.83.FF.87.8A
TREASURE_PILE.Row5.1	.HS	80.80.80.B0.55.2A.5D.22.65.B2.91.E0.FF.9F.83.F8.80.80
TREASURE_PILE.Row5.2	.HS	80.80.82.C3.D5.AA.F7.AA.C5.AA.A5.E4.FF.99.83.80.A0.80
TREASURE_PILE.Row5.3	.HS	80.80.80.80.5D.62.75.2B.15.AA.85.20.E7.CF.E1.80.9C.80
TREASURE_PILE.Row5.4	.HS	80.87.80.80.D5.A7.D5.AF.D5.A8.A5.A4.8D.E6.81.80.F8.B0
TREASURE_PILE.Row5.5	.HS	80.80.98.80.57.2A.55.2E.5D.A2.85.A1.FD.F3.80.80.F0.81
TREASURE_PILE.Row5.6	.HS	80.80.D4.80.D5.BA.DD.BA.D5.8A.85.A8.F5.AB.D0.80.F8.83
TREASURE_PILE.Row5.7	.HS	80.80.86.80.56.2A.55.2A.15.2A.84.6A.D0.82.80.80.E0.83
TREASURE_PILE.Row5.8	.HS	80.80.80.80.D0.AA.D5.8A.D5.AA.C1.AA.80.80.94.80.E0.83
TREASURE_PILE.Row5.9	.HS	80.80.80.98.08.00.56.2B.00.40.55.81.80.8C.80.8C.F0.83
TREASURE_PILE.Row5.10	.HS	80.80.80.80.80.80.D4.AA.85.A8.D5.80.D0.80.80.80.80.86
TREASURE_PILE.Row5.11	.HS	80.80.80.80.50.14.00.00.14.00.80.80.80.80.80.80.80.84
TREASURE_PILE.Row5.12	.HS	80.80.80.80.D0.80.98.80.C0.81.80.A8.86.81.80.80.80.84
TREASURE_PILE.Row5.13	.HS	80.80.80.80.00.00.00.05.00.14.80.80.80.80.80.80.82.84
TREASURE_PILE.Row5.14	.HS	80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.80.84.82
TREASURE_PILE.Row5.15	.HS	80.80.80.80.00.00.00.00.00.00.80.80.80.80.80.80.A8.81

@END

;*See C:\My_code\NA\Map_Shapes\Shapes\misc\Treasure_Pile\Treasure_Pile.XLS

SHAPE_TABLE.TREASURE_PILE.ANIMATION 	;**OPT** Speed. If it turns out there is AUX memory available (after game engine fine tuning is done, consider moving the treasure image shape data to AUX to reduce disk load time)
@START

TREASURE_PILE.SBYTE_TABLE					.HS 15.15.1D.25
TREASURE_PILE.LINE_TABLE					.HS 08.48.18.58

;TREASURE_PILE.SINDEX_TABLE					.HS	

TREASURE_PILE.SHAPE_BASE_ADDRESS_TABLE.LO
	.DA #SHAPE_TABLE.ANIMATED_TILE0
	.DA #SHAPE_TABLE.ANIMATED_TILE1
	.DA #SHAPE_TABLE.ANIMATED_TILE2
	.DA #SHAPE_TABLE.ANIMATED_TILE3
	
TREASURE_PILE.SHAPE_BASE_ADDRESS_TABLE.HO
	.DA /SHAPE_TABLE.ANIMATED_TILE0
	.DA /SHAPE_TABLE.ANIMATED_TILE1
	.DA /SHAPE_TABLE.ANIMATED_TILE2
	.DA /SHAPE_TABLE.ANIMATED_TILE3


SHAPE_TABLE.ANIMATED_TILE0 ;(tile0, row0 in spreadsheet for treasure pile large image*)
.TREASURE_PILE.ANIMATED_TILE0.1			.HS	C0.80.D0.8A.80.8A.D0.A8.D0.A2.90.A8.C0.A0.C0.A0.D0.80.D4.88.D4.AA.40.2A.C0.BA.50.3E.F0.BB.F0.BA
.TREASURE_PILE.ANIMATED_TILE0.2			.HS	D0.88.80.A0.C0.80.D0.A0.D0.82.D4.82.D4.A2.C0.A8.C0.AA.40.0A.D0.8A.50.2A.D0.BA.50.3E.F0.BB.F0.BA
.TREASURE_PILE.ANIMATED_TILE0.3			.HS	84.A0.80.A8.C0.AA.C0.8A.D0.88.D0.A8.C0.80.00.0A.D0.AA.50.2A.D0.AA.50.2A.D0.BA.50.3E.F0.BB.F0.BA
.TREASURE_PILE.ANIMATED_TILE0.4			.HS	80.80.C0.80.D0.82.D0.82.C0.82.C0.82.C0.8A.D0.8A.D0.AA.40.2A.D0.8A.50.2A.D4.BA.50.3E.F0.BB.F0.BA


; TEST_TILE1.1			.HS	7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F
; TEST_TILE1.2			.HS	55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A
; TEST_TILE1.3			.HS	2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55
; TEST_TILE1.4			.HS	AA.D5.AA.D5.AA.D5.AA.D5.AA.D5.AA.D5.AA.D5.AA.D5.AA.D5.AA.D5.AA.D5.AA.D5.AA.D5.AA.D5.AA.D5.AA.D5



SHAPE_TABLE.ANIMATED_TILE1 ;(tile0, row4 in spreadsheet for treasure pile large image*)
.TREASURE_PILE.ANIMATED_TILE1.1			.HS	80.E0.80.C0.B0.80.F8.80.B0.8C.B4.80.80.80.80.80.80.80.08.04.04.38.04.1F.44.0F.44.1F.68.03.00.06
.TREASURE_PILE.ANIMATED_TILE1.2			.HS	80.E0.80.C0.80.80.80.80.80.8C.94.80.80.80.80.84.80.B8.04.1C.02.0F.42.0F.42.1F.64.03.08.03.00.06
.TREASURE_PILE.ANIMATED_TILE1.3			.HS	80.E0.80.C0.80.80.B0.80.B0.8C.B4.80.80.80.80.80.84.84.02.38.02.1F.42.0F.64.1F.48.03.00.03.00.06
.TREASURE_PILE.ANIMATED_TILE1.4			.HS	80.E0.B0.C0.B0.80.FC.81.B0.8C.B4.80.80.80.80.84.80.B8.04.1C.02.0F.42.0F.42.1F.64.03.08.03.00.06

; TEST1_TILE1.1			.HS	7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F
; TEST1_TILE1.2			.HS	55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A
; TEST1_TILE1.3			.HS	2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55
; TEST1_TILE1.4			.HS	AA.D5.AA.D5.AA.D5.AA.D5.AA.D5.AA.D5.AA.D5.AA.D5.AA.D5.AA.D5.AA.D5.AA.D5.AA.D5.AA.D5.AA.D5.AA.D5


SHAPE_TABLE.ANIMATED_TILE2 ;(tile4, row1 in spreadsheet for treasure pile large image*)
.TREASURE_PILE.ANIMATED_TILE2.1			.HS	80.B8.B8.A8.FE.A8.E7.A8.BE.A8.C7.FF.FF.FF.FF.C0.80.B0.C0.BA.D5.98.81.98.C0.8E.DF.8C.8A.86.EA.87
.TREASURE_PILE.ANIMATED_TILE2.2			.HS	80.B8.B8.A8.FE.A8.E7.A8.BE.A8.C7.FF.FF.FF.FF.C0.80.B0.C0.BA.D5.98.8D.98.CC.8E.FF.8C.9F.86.EC.87
.TREASURE_PILE.ANIMATED_TILE2.3			.HS	80.B8.B8.A8.FE.A8.E7.A8.BE.A8.C7.FF.FF.FF.FF.C0.80.B0.C0.BA.D5.98.81.98.CC.8E.DF.8C.8E.86.EA.87
.TREASURE_PILE.ANIMATED_TILE2.4			.HS	80.B8.B8.A8.FE.A8.E7.A8.BE.A8.C7.FF.FF.FF.FF.C0.80.B0.C0.BA.D5.98.81.98.C0.8E.D5.8C.8A.86.EA.87

; TEST2_TILE1.1			.HS	7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F
; TEST2_TILE1.2			.HS	55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A
; TEST2_TILE1.3			.HS	2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55
; TEST2_TILE1.4			.HS	AA.D5.AA.D5.AA.D5.AA.D5.AA.D5.AA.D5.AA.D5.AA.D5.AA.D5.AA.D5.AA.D5.AA.D5.AA.D5.AA.D5.AA.D5.AA.D5


SHAPE_TABLE.ANIMATED_TILE3 ;(tile8, row5 in spreadsheet for treasure pile large image*)
.TREASURE_PILE.ANIMATED_TILE3.1			.HS	87.94.20.00.1C.00.38.00.70.30.70.01.78.03.60.03.60.03.70.03.00.06.00.04.00.04.00.02.01.02.2A.01	
.TREASURE_PILE.ANIMATED_TILE3.2			.HS	87.94.00.00.00.00.20.18.1C.30.78.00.70.01.78.03.60.03.70.03.00.06.00.04.00.02.28.02.24.01.02.00	
.TREASURE_PILE.ANIMATED_TILE3.3			.HS	87.94.20.18.1C.3C.38.18.70.30.70.01.78.03.60.03.60.03.70.03.00.06.00.04.02.04.04.02.28.02.20.01	
.TREASURE_PILE.ANIMATED_TILE3.4			.HS	87.94.00.00.20.00.1C.00.78.30.70.01.78.03.60.03.60.03.70.03.00.06.00.04.00.04.02.04.04.02.28.01	

; TEST3_TILE1.1			.HS	7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F.7F
; TEST3_TILE1.2			.HS	55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A
; TEST3_TILE1.3			.HS	2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55.2A.55
; TEST3_TILE1.4			.HS	AA.D5.AA.D5.AA.D5.AA.D5.AA.D5.AA.D5.AA.D5.AA.D5.AA.D5.AA.D5.AA.D5.AA.D5.AA.D5.AA.D5.AA.D5.AA.D5


@END



@END




@END








