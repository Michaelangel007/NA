;************************INCLUDE FILE*****************************
;(do not assemble stand-alone.)

;=====================TILE_FUNCTIONS2.ASM DOCUMENTATION====================================
;
;These subroutines replace those in TILE.FUNCTIONS.ASM, which did not support loader zones and kept tiles compresssed in main
;memory, which proved to be very complicated (prone to bugs), and slow. 
;
;These subroutines can all be called directly. They generally serve 
;the purpose of udpating SCREEN.TILE.DATA with Tile_IDs from the 
;regional map (RZONE.ARRAY).
;=================================================================================

TILE.LOOKUP.SCREEN ;	=====LOAD ENTIRE SCREEN OF TILES=================
@START
;PARAMETERS: RMAP
;RETURN: SCREEN.TILE.DATA
;ENTRANCE: DIRECT

;=====================SUBROUTINE DOCUMENTATION====================================
;
;This subroutine is called by DRAW.SCREEN to fill SCREEN.TILE.DATA with Tile_IDs
;from the regional map (RZONE.ARRAY) based on the player's location on the regional map (RMAP)	
;
;Each call to TILE.LOOKUP.ROW reads and writes a row of Tile_IDs. This routine
;increments SMAP.CURRENT which tells TILE.LOOKUP.ROW which row to lookup.
;X-REG contains the index for writing to SCREEN.TILE.DATA which is incremented within 
;TILE.LOOKUP.ROW
; 
;=================================================================================


;SAVE REGISTERS
	TYA
	PHA


.INIT.MAP
	LDA RMAP					;LOAD TILE_ID OF CURRENT MAP POSITION (CENTER/PLAYER TILE)
	STA OP1
	LDA RMAP+$1
	STA OP1+$1
	
	LDA #OFFSET.SCREEN			;CALCULATE TILE_ID OF TILE IN UPPER LEFT CORNER OF SCREEN
	STA OP2
	LDA /OFFSET.SCREEN
	STA OP2+$1
	
	JSR SBC.16					;RMAP(2) - SCREEN.OFFSET.LO/HO (2)
			
	LDA RESULT					;SAVE TILE_ID OF UPPER LEFT SCREEN TILE
	STA SMAP
	STA SMAP.CURRENT
	LDA RESULT+$1
	STA SMAP+$1
	STA SMAP.CURRENT+$1
	
;INIT COUNTERS
	LDX #$00					;INIT ROW COUNTER IN TILE.LOOKUP.ROW (IT DOESN'T INIT THE VALUE SO IT CAN WE USED TO LOAD ADJACENT ROWS)
	LDY #$00					;ROW COUNTER
.LOAD.LOOP		
	JSR TILE.LOOKUP.ROW
	

			
;INCREMENT COUNTERS
	LDA SMAP.CURRENT					
	STA OP1
	LDA SMAP.CURRENT+$1
	STA OP1+$1
	
	LDA #OFFSET.DOWN			
	STA OP2
	LDA #$00
	STA OP2+$1
	
	JSR ADC.16					;RMAP(2) - SCREEN.OFFSET.LO/HO (2)		;CALCULATE TILE_ID OF TILE IN UPPER LEFT CORNER OF SCREEN
			
	LDA RESULT					;SAVE TILE_ID OF UPPER LEFT SCREEN TILE
	STA SMAP.CURRENT
	LDA RESULT+$1
	STA SMAP.CURRENT+$1
	
	INY 						;INCREMENT ROW COUNTER
	CPY #SCREEN.COLUMN.SIZE		;# OF TILES IN A COLUMN == # OF ROWS ON THE SCREEN		 
	BNE .LOAD.LOOP				;LAST ROW?

.EXIT							;IF YES, EXIT
		

;RESTORE REGISTERS
	PLA
	TAY
	
	RTS							
@END

TILE.LOOKUP.ROW ; 		=====LOAD A ROW OF TILES=========================
@START
;PARAMETERS: SMAP.CURRENT (2), X-REG (SCREEN.TILE.DATA ELEMENT OF ROW)*1
;RETURN: SCREEN.TILE.DATA (1 ROW)
;ENTRANCE: DIRECT
;*1 THE SCREEN.TILE.DATA INDEX IS A PARAMATER, AND NOT AN INIT VALUE SO THAT 
;		THIS ROUTINE CAN BE USED TO CALL MULTIPLE ADJACENT ROWS, SUCH AS IS DONE WHEN DRAWING ALL TILES ON THE SCREEN.

;=====================SUBROUTINE DOCUMENTATION====================================
;
;This subroutine is called by DRAW.SCREEN reads a row of tiles from the
;regional map (RZONE.ARRAY) and writes it to SCREEN.TILE.DATA
; 
;The read location is determined by the parameter SMAP.CURRENT and the 
;write location is determined by X-REG.
;
;=================================================================================



;SAVE REGISTERS
	TYA
	PHA

;INIT INDEXES
	LDY #$00					;RZONE.ARRAY COLUMN INDEX


;INIT RZONE.ARRAY BASE ADDRESS

;=======INLINE CODE FOR ADC.16========	
;RZONE.ARRAY(2) + SMAP.CURRENT(2)
	
	CLC                          			;ALWAYS BEFORE ADD
	LDA #RZONE.ARRAY						;OP1
	ADC SMAP.CURRENT						;OP2  
	STA RZONE.ARRAY.INDEX_ROW
				 
	LDA /RZONE.ARRAY						;OP1+$1
	ADC SMAP.CURRENT+$1						;OP2+$2 carry flag not cleared via CLC intentionally, it's part of 16-bit adding. 
	STA RZONE.ARRAY.INDEX_ROW+$1
;======================================	

			
.LOOP
	LDA (RZONE.ARRAY.INDEX_ROW),Y
	STA SCREEN.TILE.DATA,X
	
	INY										;INCREMENT RZONE.ARRAY COLUMN
	INX 									;INCREMENT SCREEN.TILE.ARRAY ELEMENT	
	CPY #SCREEN.ROW.SIZE	
	BEQ .EXIT
	

	JMP .LOOP	
.EXIT							
		
		
;RESTORE REGISTERS
	PLA
	TAY

	RTS	
@END

TILE.LOOKUP.COLUMN ; 	=====LOAD A ROW OF TILES=========================
@START
;PARAMETERS: SMAP.CURRENT (2), Y-REG (SCREEN.TILE.DATA ELEMENT OF COLUMN)*1
;RETURN: SCREEN.TILE.HOPPER
;ENTRANCE: DIRECT
;*1 THE SCREEN.TILE.DATA INDEX IS A PARAMATER, AND NOT AN INIT VALUE SO THAT 
;		THIS ROUTINE CAN BE USED TO CALL MULTIPLE ADJACENT COLUMNS

;=====================SUBROUTINE DOCUMENTATION====================================
;
;This subroutine is called by DRAW.SCREEN reads a column of tiles from the
;regional map (RZONE.ARRAY) and writes it to SCREEN.TILE.DATA
; 
;The read location is determined by the parameter SMAP.CURRENT and the 
;write location is determined by X-REG.
;
;=================================================================================
		
;SAVE PARAMETERS
	STY SCREEN.ARRAY.INDEX
	
;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA

;INIT INDEXES
	LDX #$00					;SCREEN TILE HOPPER ONLY HOLDS ONE COLUMN SO THE INDEX ALWAYS STARTS WITH #$00

;INIT RZONE.ARRAY BASE ADDRESS

;=======INLINE CODE FOR ADC.16========	
;RZONE.ARRAY(2) SMAP.CURRENT(2)
	
	CLC                          			;ALWAYS BEFORE ADD
	LDA #RZONE.ARRAY						;OP1
	ADC SMAP.CURRENT						;OP2  
	STA RZONE.ARRAY.INDEX_ROW
				 
	LDA /RZONE.ARRAY						;OP1+$1
	ADC SMAP.CURRENT+$1						;OP2+$2 carry flag not cleared via CLC intentionally, it's part of 16-bit adding. 
	STA RZONE.ARRAY.INDEX_ROW+$1
;======================================	
	

;INIT STOP VALUE
	LDA #SCREEN.COLUMN.SIZE
	STA SCREEN.COLUMN.STOP
	

.LOOP
	LDY #$00						;THE RZONE ARRAY INDEX IS ALWAYS #$00, BECAUSE TO GET THE NEXT TILE IN THE COLUMN, WE MOVE DOWN 1 ROW IN THE SCREEN ARRAY.
	LDA (RZONE.ARRAY.INDEX_ROW),Y
	STA SCREEN.TILE.HOPPER,X
			
	LDY SCREEN.ARRAY.INDEX
	STA SCREEN.TILE.DATA,Y	
	STA SCREEN.TILE.HOPPER,X
	
	LDA SCREEN.DARK.DATA,Y			;HOLDS THE DARKNESS FLAG FOR THE TILES LOADED INTO SCREEN.TILE.HOPPER
	STA SCREEN.DARK.HOPPER,X
	
	LDA #$00						;THE SCROLL ROUTINE FOR THIS ARRAY LEAVES THE COLUMN/ROW ELEMENTS UNTOUCHED, AND THEY NEED TO BE INIT TO $00
	STA SCREEN.DARK.DATA_BEFORE,Y	;IT IS EFFICIENT TO DO THIS HEAR BECAUSE WE'RE ALREADY ITERATING THROUGH THE NEW COLUMN 

	
			
	TYA			
	CLC	
	ADC #SCREEN.ARRAY.OFFSET		;INCREMENT INDEX OF SCREEN ARRAYS
	STA SCREEN.ARRAY.INDEX
	
				
	INX 							;INCREMENT SCREEN.TILE.HOPPER INDEX
	CPX SCREEN.COLUMN.STOP	
	BEQ .EXIT
	
;=======INLINE CODE FOR ADC.16========	
;RZONE.ARRAY.INDEX_ROW(2) #RZONE.ARRAY.OFFSET(1)

;NEXT TILE	
	CLC                          			;ALWAYS BEFORE ADD
	LDA RZONE.ARRAY.INDEX_ROW				;OP1
	ADC #RZONE.ARRAY.OFFSET					;OP2  
	STA RZONE.ARRAY.INDEX_ROW
				 
	LDA RZONE.ARRAY.INDEX_ROW+$1			;OP1+$1
	ADC #$00								;OP2+$2 carry flag not cleared via CLC intentionally, it's part of 16-bit adding. 
	STA RZONE.ARRAY.INDEX_ROW+$1
;======================================		
	JMP .LOOP	
.EXIT							

			
		
;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX

	RTS		

@END

TILE.LOOKUP.OFFSCREEN.COLUMN ; 	=====LOAD A ROW OF TILES NOT ON THE VIEW SCREEN=========================
@START
;PARAMETERS: SMAP.CURRENT (2)
;RETURN: SCREEN.TILE.HOPPER
;ENTRANCE: DIRECT

;=====================SUBROUTINE DOCUMENTATION====================================
;
;This subroutine is called by DRAW.SCREEN reads a column of tiles, which are not
;located on the view screen, from the regional map (RZONE.ARRAY) and writes it to 
;SCREEN.TILE.HOPPER
;
;To read a column of tiles located on the view screen, use TILE.LOOKUP.COLUMN
;
;The read location is determined by the parameter SMAP.CURRENT and the 
;write location to SCREEN.TILE.HOPPER is indexed by X-REG (not a parameter, it is init to $00)
;
;=================================================================================


; ;SAVE PARAMETERS
	; STY SCREEN.ARRAY.INDEX
	
;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA

;INIT INDEXES
	LDX #$00					;SCREEN TILE HOPPER ONLY HOLDS ONE COLUMN SO THE INDEX ALWAYS STARTS WITH #$00

;INIT RZONE.ARRAY BASE ADDRESS

;=======INLINE CODE FOR ADC.16========	
;RZONE.ARRAY(2) SMAP.CURRENT(2)
	
	CLC                          			;ALWAYS BEFORE ADD
	LDA #RZONE.ARRAY						;OP1
	ADC SMAP.CURRENT						;OP2  
	STA RZONE.ARRAY.INDEX_ROW
				 
	LDA /RZONE.ARRAY						;OP1+$1
	ADC SMAP.CURRENT+$1						;OP2+$2 carry flag not cleared via CLC intentionally, it's part of 16-bit adding. 
	STA RZONE.ARRAY.INDEX_ROW+$1
;======================================	
	

;INIT STOP VALUE
	LDA #SCREEN.COLUMN.SIZE
	STA SCREEN.COLUMN.STOP
	
	

	
.LOOP
	LDY #$00						;THE RZONE ARRAY INDEX IS ALWAYS #$00, BECAUSE TO GET THE NEXT TILE IN THE COLUMN, WE MOVE DOWN 1 ROW IN THE SCREEN ARRAY.
	LDA (RZONE.ARRAY.INDEX_ROW),Y
	STA SCREEN.TILE.HOPPER,X
	
	; LDY SCREEN.ARRAY.INDEX
	; STA SCREEN.TILE.DATA,Y	
	; STA SCREEN.TILE.HOPPER,X
	
	; LDA SCREEN.DARK.DATA,Y			;HOLDS THE DARKNESS FLAG FOR THE TILES LOADED INTO SCREEN.TILE.HOPPER
	; STA SCREEN.DARK.HOPPER,X
	
	; LDA #$00						;THE SCROLL ROUTINE FOR THIS ARRAY LEAVES THE COLUMN/ROW ELEMENTS UNTOUCHED, AND THEY NEED TO BE INIT TO $00
	; STA SCREEN.DARK.DATA_BEFORE,Y	;IT IS EFFICIENT TO DO THIS HEAR BECAUSE WE'RE ALREADY ITERATING THROUGH THE NEW COLUMN 

	TYA			
	CLC	
	ADC #SCREEN.ARRAY.OFFSET		;INCREMENT INDEX OF SCREEN ARRAYS
	STA SCREEN.ARRAY.INDEX
	
				
	INX 							;INCREMENT SCREEN.TILE.HOPPER INDEX
	CPX SCREEN.COLUMN.STOP	
	BEQ .EXIT
	
;=======INLINE CODE FOR ADC.16========	
;RZONE.ARRAY.INDEX_ROW(2) #RZONE.ARRAY.OFFSET(1)

;NEXT TILE	
	CLC                          			;ALWAYS BEFORE ADD
	LDA RZONE.ARRAY.INDEX_ROW				;OP1
	ADC #RZONE.ARRAY.OFFSET					;OP2  
	STA RZONE.ARRAY.INDEX_ROW
				 
	LDA RZONE.ARRAY.INDEX_ROW+$1			;OP1+$1
	ADC #$00								;OP2+$2 carry flag not cleared via CLC intentionally, it's part of 16-bit adding. 
	STA RZONE.ARRAY.INDEX_ROW+$1
;======================================		
	JMP .LOOP	
	
.EXIT							

		
;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX

	RTS		

@END


TILE.LOOKUP.OFFSCREEN.ROW ; 	=====LOAD A ROW OF TILES NOT ON THE VIEW SCREEN =========================
@START
;PARAMETERS: SMAP.CURRENT (2),
;RETURN: SCREEN.TILE.ROW.HOPPER (1 ROW OF TILES)
;ENTRANCE: DIRECT

;=====================SUBROUTINE DOCUMENTATION====================================
;
;This subroutine is called by DRAW.SCREEN reads a row of tiles, which are not
;located on the view screen, from the regional map (RZONE.ARRAY) and writes it to 
;SCREEN.TILE.ROW.HOPPER
;
;To read a row of tiles located on the view screen, use TILE.LOOKUP.ROW
;
;The read location is determined by the parameter SMAP.CURRENT and the 
;write location to SCREEN.TILE.ROW.HOPPER is indexed by X-REG (not a parameter, it is init to $00)
;
;=================================================================================



;SAVE REGISTERS
	TYA
	PHA

;INIT INDEXES
	LDY #$00					;RZONE.ARRAY COLUMN & SCREEN.TILE.ROW.HOPPER INDEX


;INIT RZONE.ARRAY BASE ADDRESS

;=======INLINE CODE FOR ADC.16========	
;RZONE.ARRAY(2) + SMAP.CURRENT(2)
	
	CLC                          			;ALWAYS BEFORE ADD
	LDA #RZONE.ARRAY						;OP1
	ADC SMAP.CURRENT						;OP2  
	STA RZONE.ARRAY.INDEX_ROW
				 
	LDA /RZONE.ARRAY						;OP1+$1
	ADC SMAP.CURRENT+$1						;OP2+$2 carry flag not cleared via CLC intentionally, it's part of 16-bit adding. 
	STA RZONE.ARRAY.INDEX_ROW+$1
;======================================	

			
.LOOP
	LDA (RZONE.ARRAY.INDEX_ROW),Y
	STA SCREEN.TILE.ROW.HOPPER,Y
	
	INY										;INCREMENT RZONE.ARRAY COLUMN AND SCREEN.TILE.ROW.HOPPER INDEX
	CPY #SCREEN.ROW.SIZE	
	BEQ .EXIT
	

	JMP .LOOP	
.EXIT							
		
		
;RESTORE REGISTERS
	PLA
	TAY

	RTS	
@END

	
;TILE.LOOKUP.SINGLE ; 	======LOOKUP TILE_TYPE OF A SINGLE MAP TILE======
@START
;PARAMETERS: RMAP
;RETURN VALUE: SAVED.ACC.LOCAL (tile_type value)
;ENTRANCE: DIRECT

;=====================SUBROUTINE DOCUMENTATION====================================
;
;I think I desgined this code to by copy/past in-line figuring it would be used
;in places where speed was critial. I don't think I ever tested it as an 
;actual subroutine.
;
;=================================================================================


;========TEMPLATE=======

;INIT RZONE.ARRAY BASE ADDRESS

;=======INLINE CODE FOR ADC.16========	
;RZONE.ARRAY(2) + RMAP(2)
	
	; CLC                          			;ALWAYS BEFORE ADD
	; LDA #RZONE.ARRAY						;OP1
	; ADC RMAP								;OP2  
	; STA RZONE.ARRAY.INDEX_ROW
				 
	; LDA /RZONE.ARRAY						;OP1+$1
	; ADC RMAP+$1						;OP2+$2 carry flag not cleared via CLC intentionally, it's part of 16-bit adding. 
	; STA RZONE.ARRAY.INDEX_ROW+$1
; ;======================================	

	; LDY #$00
	; LDA (RZONE.ARRAY.INDEX_ROW),Y	
	; STA SAVED.ACC.LOCAL						;RETURN VALUE
		; ;**OPT** Speed. Memory. I think the return value can be provided in the ACC. I'm not sure why
		; ;it's saved to a variable. it would need to be changed in each routine that calls TILE.LOOKUP.SINGLE
	
	; RTS

@END	
		
