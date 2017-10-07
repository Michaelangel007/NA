;************************INCLUDE FILE*****************************
;(do not assemble stand-alone. For a stand-alone version of this routine see examples/rwts2)


;=====================INCLUDE FILE DOCUMENTATION====================================
;
;Include file to SWAP.ROUTINES.INVENTORY.ASM
;
;=================================================================================



INV_0.DISPLAY.STATS_SUMMARY
@START

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
	

	;**FALLS THROUGH** (to screen0)
	

	
	
; ;CALL STATS: SCREEN0	
	; JMP INV_0.SCREEN_0 ;needs to be a JSR instead of a fall through because it's a shared routine (with combat module) and it needs to RTS back to it's caller. 
			
			
			;JSR KEYIN
	
		; LDA #$00 ;(high-bit not set = tab pressed, increment menu | $B1-B6 = ASCII code to hotkey to specific menu) 
	; JMP INV.NEXT.MENU
	
			; LDA #$AA
			; JSR PREP.BRK
			; BRK
			
			
				

				
@END


INV_0.SCREEN_0

	;this is a shared include file used by the combat module and inventory module, so that the same
	;code is inserted into each. 
	.IN 	c:\my_code\na\source_code\swap.routines.combat.stats_summary.asm
