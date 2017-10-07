;************************INCLUDE FILE*****************************
;(do not assemble stand-alone.  )


;=====================SUBROUTINE DOCUMENTATION====================================
;
;This subroutine exists in both the INV_8 (merchant transaction module) and the TM_1 (treasury management)
;module inventory module in order to reduce main memory usage. This file is an include file in both modules so that this
;file is the source code for both, to avoid managing two code sets. The only implication of that is no parent labels can be
;defined within this routine. 
;
;
;=================================================================================


;__SEARCH.PLY_INV
@START
;PARAMETERS: INV__.SEARCH.PLY.INV.ITEM_TYPE, INV__.SEARCH.PLY.INV.ITEM_ID*, PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER(2)**
;ENTRANCE: direct
;RETURN: PLAYER_MERCH.INVENTORY.DATA.POINTER(2)***
;
;*INV__.SEARCH.PLY.INV.ITEM_ID is .EQ to INV_8.PARM.RE.SEARCH.ITEM_ID
;**Init to the top of the player inventory data array
;***PLAYER_MERCH.INVENTORY.DATA.POINTER(2) contains the memory address of the inventory record for the item the search was for -if it was found.
;

					
					
;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA
	
	

.INIT
	LDA INV__.SEARCH.PLY.INV.ITEM_TYPE  ;**OPT** Memory. The routine below may be able to just use the INV__ variables in the LDAs. If so, then this pair of LDAs/STAs can be removed. 
	STA INV_8.PARM.RE.SEARCH.ITEM_TYPE

	LDA INV__.SEARCH.PLY.INV.ITEM_ID	;""
	STA INV_8.PARM.RE.SEARCH.ITEM_ID
	
	;Original note: (but I changed my mind, hence the LDA/STA above). not needed for INV_8.PARM.RE.SEARCH.ITEM_ID because it is .EQ to INV_8.GET.ITEM_BASE_PRICE.PARM.ITEM_ID
	;INV_8.PARM.RE.SEARCH.ITEM_TYPE is not .EQ because it gets set to the open record value on the 2nd
	;iteration of the loop

	
				; LDA #$01  ;**OPT** Memory. The routine below may be able to just use the INV__ variables in the LDAs. If so, then this pair of LDAs/STAs can be removed. 
				; STA INV_8.PARM.RE.SEARCH.ITEM_TYPE

				; LDA #$1C
				; STA INV_8.PARM.RE.SEARCH.ITEM_ID
				
			
	
	
	
	
	
		
	LDX #$00
.LOOP.SEARCH.INVENTORY.DATA

	;open record check
	;(implicitly done by TEST1 below because Item_Type will never be equal to the open record value of $FF)

	LDY #$00 ;reset Y-REG on each iteration because array position is managed exclusively through PLAYER_MERCH.INVENTORY.DATA.POINTER 
	
	;Y-REG ;set to record byte $0 (item_level | item_type)
	LDA (PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER),Y
	;Item_Type is in LO nibble.
	CPX #$01 ;is current loop iteration an open record search?
	BEQ .TEST1 ;branch if yes, skipping the mask-out of HO nibble because the open record value is $FF
	AND #$F ;mask-out HO nibble. 
.TEST1: ;Item_Type
	CMP INV_8.PARM.RE.SEARCH.ITEM_TYPE 
	BNE .NEXT_RECORD
	CPX #$01 ;is current loop iteration an open record search?
	BEQ .ITEM.FOUND ;branch if yes, skipping the item_code search in byte $01 because the open record value is in byte $0
	INY ;advance to record byte $1 (item_code)
.TEST2 ;item_code
	LDA (PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER),Y
	CMP INV_8.PARM.RE.SEARCH.ITEM_ID
	BEQ .ITEM.FOUND
	
.NEXT_RECORD
	;INX
	LDA PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER+$0
	CLC
	ADC #INV.PLAYER.INVENTORY.DATA.RECORD_SIZE
	STA PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER+$0
	LDA PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER+$1
	ADC /INV.PLAYER.INVENTORY.DATA.RECORD_SIZE ;16-bit add
	STA PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER+$1
.EXIT.TEST	
	CMP /INV.PLAYER.INVENTORY.DATA.PATCH_END
	BCC .LOOP.SEARCH.INVENTORY.DATA
	BEQ .LOOP.SEARCH.INVENTORY.DATA

	;**FALLS THROUGH**
	
.ITEM.NOT_FOUND
	INX ;next loop interation (open record search)
	CPX #$02
	BCS .ERROR_TRAP
	
	;set open record value as the search parm
	LDA #$FF
	STA INV_8.PARM.RE.SEARCH.ITEM_TYPE
	
	;reset pointer to the top of the player inventory table
	LDA #INV.PLAYER.INVENTORY.DATA
	STA PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER+$0

	LDA /INV.PLAYER.INVENTORY.DATA
	STA PLAYER_MERCH.INVENTORY.DATA.SELECTED_RECORD.POINTER+$1	
	
	JMP .LOOP.SEARCH.INVENTORY.DATA
	
.ITEM.FOUND


				; LDX #$BC
				; LDX PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
				; LDY PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
				; ;LDX FILE.OPEN.INVENTORY.DEBUG_COUNTER
				; JSR PREP.BRK
				; BRK
				
				
.EXIT
;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX
	
	

					
	RTS
	
.ERROR_TRAP	
;INV_8.SEARCH.PLY_INV reports that the item search for was not found AND no open record was found. This should
;never happen because the player inventory table has enough records for each item in the master item table. 

	JSR PREP.BRK
	BRK
	
	
@END
	
	