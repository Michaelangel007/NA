                .CR     6502            Use 6502 overlay
				.OR		$6000			**Always put before .TF directive and never use again in program
				.TF     TEST.BIN,BIN
				.EF		errors
				.LF		C:\MY_CODE\LIST
;				.LI 	OFF				Switches the assembler machine code conversion listing off so errors are easier to see				
;				.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_common


;DEMO: 8-BIT RANDOM NUMBER RETURNED IN RND.
;THE ROUTINE IS ACTUALLY SUPPOSED TO BE 16-BIT BUT RND+$1 IS ALWAYS $00 OR $01. 
;
;DRIVER1 MAKES SEVERAL ONE-OFF CALLS
;DRIVER2 FILLS AN ARRAY (TAKES AWHILE TO RUN)
;ORIGINAL SOURCE: http://6502.org/source/integers/random/random.html search for "random8". The RAND routine used was one directly above the "RANDOM8" code listing
 		
		JMP DRIVER2
		
DRIVER1
			LDA $4F
			STA SEED2
			LDA $4E
			STA SEED3
			
			LDA #$01
			STA RND.LO
			LDA #$10
			STA RND.HI
			
			
			LDA #$02
			JSR RANDOM.16.WRAPPER

			LDX RND
			STX $9000
			LDY RND+$1
			STY $9001
			

			JSR RANDOM.16.WRAPPER

			LDX RND
			STX $9002
			LDY RND+$1
			STY $9003
			
			JSR RANDOM.16.WRAPPER
			
			LDX RND
			STX $9004
			LDY RND+$1
			STY $9005
			
			JSR RANDOM.16.WRAPPER
			
			LDX RND
			STX $9006
			LDY RND+$1
			STY $9007
			
			BRK

DRIVER2

		;LDX #$00
		;LDA $4F
		;STA SEED2
		LDA $4E
		STA SEED3
		
		LDA #$00
		STA RND.LO
		LDA #$FF
		STA RND.HI
		
		LDA #$00
		STA $EB
		LDA #$70
		STA $EC
		
		LDY #$00			;SET INDEX
		
		LDA #$02			;SET MOD
.LOOP		
		JSR RANDOM.16.WRAPPER
		;LDA	SAVED.ACC.LOCAL
		LDA RND
		STA ($EB),Y
;		LDA RND+$1
;		STA $9000+$1,X
				
				;JSR KEYSCAN
				;CMP #$CD
				;BEQ EXIT
						
		INY
		BNE .LOOP
		INC $EC
		LDA $EC
		CMP #$75
		BEQ .EXIT
		JMP .LOOP
		
.EXIT		
	BRK
			
RANDOM.16.WRAPPER ;=========
	LDA #$00
	STA RANDOM_NUMBER.ITERATION_TALLY
.CONTINUE

	JSR RANDOM.16
	
	LDA RND.HI
	CMP RND
	BCC .CONTINUE

	LDA RND
	CMP RND.LO
	BCC .CONTINUE
	
	INC RANDOM_NUMBER.ITERATION_TALLY
	
	LDA RANDOM_NUMBER.ITERATION_TALLY
	CMP #$20
	BEQ .ERROR
	RTS
	
.ERROR
;RANDOM REPORTS THAT IT'S RANDOM NUMBER GENERATOR COULDN'T MEET THE RANGE CRITERA
;AFTER $20 (!32) ATTEMPTS, WHICH IMPLIES THAT IT MAY HAVE STARTED RETURNING STATIC RESULTS.
;KICK IT IN THE HEAD WITH AN IRON BOOT. I KNOW THAT NEVER HAPPENS, BUT DO IT ANYWAY.  

	LDX #$AA
	LDY #$AA
	BRK
RANDOM.16 

;SAVE REGISTERS(1)
	STA MOD    ; store modulus in MOD
	TXA 
	PHA
	TYA
	PHA	
	LDA MOD
	
RANDOM8  STA MOD    ; store modulus in MOD
         JSR RAND   ; get next seed
         LDA #0     ; multiply SEED by MOD
         STA TMP+2
         STA TMP+1
         STA TMP
         SEC
         ROR MOD    ; shift out modulus, shifting in a 1 (will loop 8 times)
R8A      BCC R8B    ; branch if a zero was shifted out
         CLC        ; add SEED, keep upper 8 bits of product in accumulator
         TAX
         LDA TMP
         ADC SEED0
         STA TMP
         LDA TMP+1
         ADC SEED1
         STA TMP+1
         LDA TMP+2
         ADC SEED2
         STA TMP+2
         TXA
         ADC SEED3
R8B      ROR        ; shift product right
         ROR TMP+2
         ROR TMP+1
         ROR TMP
         LSR MOD    ; loop until all 8 bits of MOD have been shifted out
         BNE R8A
		 
.EXIT		 
;RESTORE REGISTERS(1)
		PLA
		TAY
		PLA
		TAX		
		;LDA SAVED.ACC.LOCAL				;RETURN VALUE IS IN ACC
		
         RTS

RAND     CLC         ; copy SEED into TMP
         LDA SEED0   ; and compute SEED = SEED * $10000 + SEED + 1
         STA TMP
         ADC #1
         STA SEED0
         LDA SEED1
         STA TMP+1
         ADC #0
         STA SEED1
         LDA SEED2
         STA TMP+2
         ADC TMP
         STA SEED2
         LDA SEED3
         STA TMP+3
         ADC TMP+1
         STA SEED3
;
; Bit 7 of $00, $19, $66, and $0D is 0, so only 6 shifts are necessary
;
         LDY #5
RAND1    ASL TMP     ; shift TMP (old seed) left
         ROL TMP+1
         ROL TMP+2
         ROL TMP+3
;
; Get X from the RAND4 table.  When:
;
; X = $00, SEED = SEED + $10000 * TMP
; X = $01, SEED = SEED + $100 * TMP
; X = $FE, SEED = SEED + $10000 * TMP + TMP
; X = $FF, SEED = SEED + $100 * TMP + TMP
;
         LDX RAND4,Y
         BPL RAND2   ; branch if X = $00 or X = $01
         CLC         ; SEED = SEED + TMP
         LDA SEED0
         ADC TMP
         STA SEED0
         LDA SEED1
         ADC TMP+1
         STA SEED1
         LDA SEED2
         ADC TMP+2
         STA SEED2
         LDA SEED3
         ADC TMP+3
         STA SEED3
         INX         ; $FE -> $00, $FF -> $01
         INX
RAND2    CLC
         BEQ RAND3   ; if X = $00, SEED = SEED + TMP * $10000
         LDA SEED1   ; SEED = SEED + TMP * $100
         ADC TMP
         STA SEED1
RAND3    LDA SEED2
         ADC TMP,X
         STA SEED2
         LDA SEED3
         ADC TMP+1,X
         STA SEED3
         DEY
         BPL RAND1
         RTS
RAND4    .HS	01.01.00.FE.FF.01	 
		 
         RTS
		 
		 
	
;======DEFINE VARIBLES======

RND		.BS $2		;2byt


TMP		.BS $4
MOD		.BS $1

SEED0	.BS $1
SEED1	.BS $1
SEED2	.BS $1
SEED3	.BS $1

RND.LO	.BS $1
RND.HI	.BS $1

RANDOM_NUMBER.STATIC_TALLY		.BS $1
RANDOM_NUMBER.LAST				.BS $1
RANDOM_NUMBER.ITERATION_TALLY	.BS $1