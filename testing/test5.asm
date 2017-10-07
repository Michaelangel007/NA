				.CR     6502            Use 6502 overlay
				.OR		$2000			**Always put before .TF directive and never use again in program
				.TF     TEST.BIN,BIN
				.EF		errors
				.LF		c:\my_code\na\build_folder\LIST
;				.LI 	OFF				Switches the assembler machine code conversion listing off so errors are easier to see				
;				.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_common


TEST

	LDX #$00
.INIT.LOOP
	LDA #$AA
	STA $200,X
	INX
	BNE .INIT.LOOP
	
	
	
	LDY #$0D
.RESTORE.LOOP
	
.COPY.CHUNK.TO.AUX_BSR

	;enable AUX memory (BSR: bank2 is already enabled)						
		TSX			;transfer stack pointer to X-REG
		;STX TEMP	;save stack pointer
		STX COW
	STA $C009 ;enable aux zero-page & aux BSR 

	;execute copy	
		; LDA #SHAPE.HOPPER0
		; STA COPY.FROM_START+$0
		; LDA /SHAPE.HOPPER0
		; STA COPY.FROM_START+$1

		; LDA COPY.FROM_START+$0
		; CLC
		; ADC #$FF
		; STA COPY.FROM_END+$0
		; LDA COPY.FROM_START+$1
		; ADC #$00 ;16-bit add
		; STA COPY.FROM_END+$1


		LDA #$00
		STA COPY.FROM_START+$0
		LDA #$02
		STA COPY.FROM_START+$1

		LDA #$FF
		STA COPY.FROM_END+$0
		LDA #$02
		STA COPY.FROM_END+$1

		
		; LDA COPY.FROM_START+$0
		; BEQ .TEST
		
				; STA $C008 ;enable main zero-page & main BSR 
				; LDX TEMP	;restore stack pointer to X-REG
				; TXS ;transfer X-REG to stack pointer
				
				; LDA #$AA
				; LDX COPY.FROM_START+$0
				; LDY COPY.FROM_START+$1
				; JSR PREP.BRK
				; BRK
; .TEST		
		;COPY.TO+$1 is init before loop starts and it is incremented below
	JSR MEMORY.COPY	
		
	STA $C008 ;enable main zero-page & main BSR 
		;LDX TEMP	;restore stack pointer to X-REG
		LDX COW
		TXS ;transfer X-REG to stack pointer

.INCREMENT.COUNTER
	INC COPY.TO+$1
	DEY
	BNE .RESTORE.LOOP


	LDA #$AA
	BRK
	
	
;VARIABLES
COPY.TO			.EQ $FA
COPY.FROM_END	.EQ $E0
COPY.FROM_START .EQ $FC

;COPY.SIZE		.BS $2
COPY.SIZE		.EQ $C0


COW .BS $1


MEMORY.COPY ;============FILL MEMORY WITH SPECIFIED VALUE=======
@START
;PARAMTERS; COPY.TO(2), COPY.FROM(2), COPY.FROM_END(2)
;RETURN: NONE
;ENTRANCE: DIRECT

;=====================SUBROUTINE DOCUMENTATION====================================
;
;This routine takes the approach of incrementing the start address and copy to address
;together. They both share Y-REG as the index and when it flips the HO byte for both is incremenet. 
;
;This works because COPY.SIZE+$1 and X-REG holds the number of bytes to copy and is decremented
;as bytes are copied. Exit teseting is based on COPY.SIZE+$1 and X-REG.
;
;=================================================================================
	
;DRIVER TEMPLATE	
		; LDA #$00
		; STA COPY.FROM_START
		; LDA #$70
		; STA COPY.FROM_START+$1
		; LDA #$00
		; STA COPY.FROM_END
		; LDA #$75
		; STA COPY.FROM_END+$1
		;	
		; LDA #$00
		; STA COPY.TO
		; LDA #$80
		; STA COPY.TO+$1
		;
		; JSR MEMORY.COPY
		

.START

;SAVE REGISTERS	
	TXA
	PHA
	TYA
	PHA

	
	;calculate # of bytes to copy (size of copy)
	;(the copy size is used for loop exit test)
	LDA COPY.FROM_END+$0		;set end address (LO)
	SEC
	SBC COPY.FROM_START+$0		;set start address (LO)
	;STA COPY.SIZE+$0  			;set # of byte to copy (LO)
	TAX ;init COPY.SIZE lo byte counter
	LDA COPY.FROM_END+$1		;set end address (HO)
	SBC COPY.FROM_START+$1 ;16-bit subtract
	STA COPY.SIZE+$1			;set # of byte to copy (HO)			
	
	LDY #$00					;COPY FROM/TO index
.LOOP					
	LDA (COPY.FROM_START),Y
	STA (COPY.TO),Y
	;**FALLS THROUGH**
.DECREMENT.SIZE.COUNTER
	CPX #$00 ;if lo byte counter is already #$00, then the next decrement will flip it. Branch so that the LO byte and HO byte decrements are both done. 
	BEQ .DECREMENT.SIZE.COUNTER.HO_BYTE
	DEX ;decrement COPY.SIZE LO byte counter
	JMP .INCREMENT.COUNTER.LO_BYTE
	
.DECREMENT.SIZE.COUNTER.HO_BYTE

	;exit test
	LDA COPY.SIZE+$1	;if COPY.SIZE HO byte is already zero then all bytes have been copied. 
	BEQ .COPY_DONE
	DEX ;flips to $FF
	DEC COPY.SIZE+$1	;decrement HO byte
		
	;**FALLS THROUGH**
	
.INCREMENT.COUNTER.LO_BYTE
	INY			;increment COPY TO/FROM index
	BNE .LOOP	;if y-reg hasn't flipped to $00, continue loop

.INCREMENT.COUNTER.HO_BYTE		
	INC COPY.FROM_START+$1
	INC COPY.TO+$1

	JMP .LOOP

.COPY_DONE					;IF YES, THEN COPY IS DONE. 

;RESTORE REGISTERS	
	PLA
	TAY
	PLA
	TAX
	
	RTS
	
@END
