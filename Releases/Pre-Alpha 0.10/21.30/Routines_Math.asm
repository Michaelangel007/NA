;************************INCLUDE FILE*****************************
;(do not assemble stand-alone. For a stand-alone version of this routine see examples/rwts2)
;
;LIST OF FUNCTIONS;
;
;ADC.16							;ADDITION, 16-BIT
;CMP.16							;COMPARE, 16-BIT
;IGR.16							;IS GREATER, 16-BIT
;SBC.16							;SUBTRACTION, 16-BIT

ADC.16 ;==========ADDITION, 16-BIT=========
;PARAMETERS: OP1(2), OP2(2)
;RETURN: RESULT(2)

;WARNING: IF USING THIS FUNCTION WITH AN 8-BIT VALUE (BECAUSE THE LOOP MAY PRODUCE A 16-BIT VALUE), YOU
;MUST SET OP1+$1 AND OP2+$2 TO $00 BEFORE CALLING ADC.16. USUALLY BEST TO DO THIS JUST BEFORE THE LOOP 
;STARTS DURING INIT OF OTHER VARIABLES.

;INIT VARIABLES
	LDA #$00
	STA RESULT
	STA RESULT+$01

; DO THE MATH ($A0F + $01)
	CLD 
    CLC                          ;ALWAYS BEFORE ADD
    LDA OP1
    ADC OP2
    STA RESULT
		 
    LDA OP1+$1
    ADC OP2+$1					;carry flag not cleared via CLC intentionally, it's part of 16-bit adding. 
    STA RESULT+$1
    BCS .ERROR

    RTS
.ERROR
;ADC OVERFLOW ERROR
;
;DISABLE.BS_RAM
	LDA $C082				;READ ENABLE ROM, DISABLE WRITE ON BSM (NORMAL STATE).
							;REQUIRED FOR BEFORE BRK BECAUSE APPLE MONITOR IS A ROM ROUTINE

	LDA TEXT
	BRK	


CMP.16 		;==========COMPARE, 16-BIT=========
;PARAMETERS: OP1(2), OP2(2)
;RETURN: ACC ($00 EQUAL, $01 NOT EQUAL)

;WARNING: IF USING THIS FUNCTION WITH AN 8-BIT VALUE (BECAUSE THE LOOP MAY PRODUCE A 16-BIT VALUE), YOU
;MUST SET OP1+$1 AND OP2+$2 TO $00 BEFORE CALLING ADC.16. USUALLY BEST TO DO THIS JUST BEFORE THE LOOP 
;STARTS DURING INIT OF OTHER VARIABLES.


		 
;compare values

		LDA OP1
		CMP OP2
		BNE .NE
		LDA OP1+$1
		CMP OP2+$1
		BNE .NE
		JMP .EQUALS
.NE					;the 16-bit values are not equal
		LDA #$01
		RTS 
.EQUALS 
		LDA #$00
		RTS

DIV.16 ;==========DIVISION=========   
;DESCRIPTION: 32-BIT DIVIDEND, 16-BIT DIVISOR, QUOTIENT, REMAINDER
;PARAMETERS: DIVIDEND(2), [DIVIDEND32(2)], DIVISOR
;ENTRANCE: DIRECT
;RETURN:	RESULT(4)
;			result0 = quotient, lo byt
;			result1 = quotient, hi byt
;			result2 = remainder, lo byt
;			result3 = remainder, hi byt


;INIT VARIABLES
	LDA #$00
	STA TEMP1
	STA CARRY
	
.START
         SEC 
         LDA DIVIDEND32
         SBC DIVISOR
         LDA DIVIDEND32+$1
         SBC DIVISOR+$1
         BCS .OVRFLW
         LDX #$11
.LOOP ;     
         ROL DIVIDEND
         ROL DIVIDEND+$1
         DEX 
         BEQ .EXIT
         ROL DIVIDEND32
         ROL DIVIDEND32+$1
         LDA #$00
         STA CARRY
         ROL CARRY
         SEC 
         LDA DIVIDEND32
         SBC DIVISOR
         STA TEMP1
         LDA DIVIDEND32+$1
         SBC DIVISOR+$1
         TAY 
         LDA CARRY
         SBC #0
         BCC .LOOP
         LDA TEMP1
         STA DIVIDEND32
         STY DIVIDEND32+$1
         BCS .LOOP
.EXIT ;     
;copy 32-bit result from N to RESULT
         LDA DIVIDEND
         STA RESULT
         LDA DIVIDEND+$1
         STA RESULT+$1
         LDA DIVIDEND32
         STA RESULT+$2
         LDA DIVIDEND32+$1
         STA RESULT+$3
         RTS 
.OVRFLW ;    
         LDA #$FF
         STA DIVIDEND32
         STA DIVIDEND32+$1
         STA DIVIDEND
         STA DIVIDEND+$1
         RTS 
         BRK 
		 
IGRE.16		;==============IS GREATER THAN OR EQUAL==========
;PARAMETERS: OP1(2), OP2(2)
;RETURN: CARRY FLAG (SET >=, NOT SET !>=)

;WARNING: IF USING THIS FUNCTION WITH AN 8-BIT VALUE (BECAUSE THE LOOP MAY PRODUCE A 16-BIT VALUE), YOU
;MUST SET OP1+$1 AND OP2+$2 TO $00 BEFORE CALLING ADC.16. USUALLY BEST TO DO THIS JUST BEFORE THE LOOP 
;STARTS DURING INIT OF OTHER VARIABLES.

    LDA OP1
    CMP OP2
    LDA OP1+1
    SBC OP2+1
    BCS .IS.GRE   (BCS: is carry flag set)
    CLC
    RTS 
.IS.GRE
    SEC
    RTS 

RANDOM.8.INIT ;======INIT ROUTINE FOR 8-BIT RANDOM NUMBER GENERATOR
;PARAMETERS: NONE
;RETURN: NONE
;ENTRACE: DIRECT
;
;FUNCTION: Initializes the SEED values and MOD value for RANDOM.8, and set the 
;default lo/hi range of the return value (random #). Call this routine once
;at the start of a program. Calling it more than once can cause problems. Specifically,
;when developing the wrapper around the generator code at first I tried initializing the seed
;and MOD on each call and the result was that I got the same number returned every time. 
;
;NOTE: best to set the RND.LO value no lower than $01. I've seen a number of
;bugs where the return values turn to static $00s. If this is a valid return value
;then the static value error routine won't catch it because it's just based 
;on an endless loop occuring because the value isn't found to satisfy the range
;critera after $FF tries. 
;

	LDA $4F
	STA SEED2
	LDA $4E
	STA SEED3
			
	LDA #$01
	STA RND.LO		;see note above before using a different return value. 
	LDA #$FF
	STA RND.HI
			
			
	LDA #$02
	STA MOD
	
	RTS
	

RANDOM.8
;PARAMETERS: RND.LO, RND.HI, RND.ABORT
;PARAMETERS: (FILLED BY RANDOM.8.INIT): MOD, SEED2 (LOAD $4F TO THIS VARIABLE), SEED3 (LOAD $4E TO THIS VARIABLE),
;RETURN: ACC (8-BIT RANDOM NUMBER), for convenience also in RND
;ENTRANCE: DIRECT
;CUSTOMIZATIONS: added range capability, optimized for speed 
;NOTE: RND.ABORT = $01, then routine will abort at certain points if a key is pressed.
;      if an abort occurs due to a keypress, $02 is loaded into this variable on return. 

;ORIGINAL SOURCE: http://6502.org/source/integers/random/random.html search for "random16". I couldn't get the random8 code to work, so I used the random16 code, which ironically returns $00 for the HO byte so it's effectively an 8-bit generator. 
;The RAND routine used was the 2nd RAND routine up from the "RANDOM8" code listing. I got
;the impression that the author provided a bunch of RAND routines, each with advantages/disadvatanges, like speed.
;When I tried the random8 code I used the RAND routine directly above it but since I had problems I 
;I tried the 2nd RAND routine up from random8 when I started working with the random16 code.


;*ACC is used for the value of 'MOD' in the random number formula. My understanding from
;reading various comments by the author on the above web page is that MOD should be set
;to a power of 2. In my testing I used $02 and the results looked very random to me so
;that is the MOD set in RANDOM8.INIT.


;SAVE REGISTERS(1)
	STA MOD    ; store modulus in MOD
	TXA 
	PHA
	TYA
	PHA	


;RANGE CONTROLS: used by calling routine to control the range of the returned random number 
	LDA #$00
	STA RANDOM_NUMBER.ITERATION_TALLY
.CONTINUE
	LDA RND.ABORT								
	CMP #$01						;IS ABORT ON KEYPRESS ENABLED?
	BNE .NO_ABORT					;IF NO, CONTINUE
.KEY_CHECK							;IF YES, CHECK FOR KEYPRESS
	LDA KB_BUFFER					;LOAD NEXT KEY PRESS FROM BUFFER
    BPL .NO_ABORT					;IF NO KEY PRESS, CONTINUE
	JMP .EXIT.ABORT					;IF KEY PRESSED, LEAVE IT IN BUFFER AND EXIT


.NO_ABORT
	JMP .RANDOM.8.START	
.RETURN


;CHECK FOR REPEATING STATIC RETURN VALUES	
	INC RANDOM_NUMBER.ITERATION_TALLY

	LDA RANDOM_NUMBER.ITERATION_TALLY
	CMP #$FF
	BEQ .ERROR
	

	LDA RND.HI
	CMP RND
	BCC .CONTINUE

	LDA RND
	CMP RND.LO
	BCC .CONTINUE
			
	JMP .EXIT
	
.ERROR
;RANDOM.8 REPORTS THAT IT'S RANDOM NUMBER GENERATOR COULDN'T MEET THE RANGE CRITERA
;AFTER $FF (!256) ATTEMPTS, WHICH IMPLIES THAT IT MAY HAVE STARTED RETURNING STATIC RESULTS.
;KICK IT IN THE HEAD WITH AN IRON BOOT. I KNOW THAT NEVER HAPPENS, BUT DO IT ANYWAY.  
;DISABLE.BS_RAM
	LDA $C082				;READ ENABLE ROM, DISABLE WRITE ON BSM (NORMAL STATE).
							;REQUIRED FOR BEFORE BRK BECAUSE APPLE MONITOR IS A ROM ROUTINE

	LDA TEXT
	BRK

.RANDOM.8.START	
		LDA MOD
		
;START OF ORIGINAL "RAND" SUBROUTINE

; get next seed		
.RAND     CLC         ; copy SEED into TMP
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
.RAND1    ASL TMP     ; shift TMP (old seed) left
         ROL TMP+1
         ROL TMP+2
         ROL TMP+3
;
; Get X from the RAND_TABLE table.  When:
;
; X = $00, SEED = SEED + $10000 * TMP
; X = $01, SEED = SEED + $100 * TMP
; X = $FE, SEED = SEED + $10000 * TMP + TMP
; X = $FF, SEED = SEED + $100 * TMP + TMP
;
         LDX RAND_TABLE,Y
         BPL .RAND2   ; branch if X = $00 or X = $01
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
.RAND2    CLC
         BEQ .RAND3   ; if X = $00, SEED = SEED + TMP * $10000
         LDA SEED1   ; SEED = SEED + TMP * $100
         ADC TMP
         STA SEED1
.RAND3    LDA SEED2
         ADC TMP,X
         STA SEED2
         LDA SEED3
         ADC TMP+1,X
         STA SEED3
         DEY
         BPL .RAND1
        
	 
;THIS IS THE LOCATION WHERE THE JSR RAND IN THE ORIGINAL VERESION RETURN TO	
         LDA #0    ; multiply SEED by MOD
         STA RND+1
         STA RND
         STA TMP
         LDY #16
.R16A     LSR MOD+1 ; shift out modulus
         ROR MOD
         BCC .R16B  ; branch if a zero was shifted out
         CLC       ; add SEED, keep upper 16 bits of product in RND
         ADC SEED0
         TAX
         LDA TMP
         ADC SEED1
         STA TMP
         LDA RND
         ADC SEED2
         STA RND
         LDA RND+1
         ADC SEED3
         STA RND+1
         TXA
.R16B     ROR RND+1 ; shift product right
         ROR RND
         ROR TMP
         ROR
         DEY
         BNE .R16A
		 JMP .RETURN
.EXIT.ABORT
		LDA #$02
		STA RND.ABORT		;set $02 in case the calling routine wants to know that the abort on keypress occured
.EXIT
		LDA #$01
		STA RND.LO			;reset random # range to the default set by RANDOM.8.INIT
		LDA #$FF		 
		STA RND.HI
;RESTORE REGISTERS(1)
		PLA
		TAY
		PLA
		TAX		
		LDA RND				;return the LO byte of RND in the ACC
		
        RTS	

SBC.16			;==========SUBTRACTION, 16-BIT=========
;PARAMETERS: OP1(2), OP2(2)
;RETURN: RESULT(2)

;WARNING: IF USING THIS FUNCTION WITH AN 8-BIT VALUE (BECAUSE THE LOOP MAY PRODUCE A 16-BIT VALUE), YOU
;MUST SET OP1+$1 AND OP2+$2 TO $00 BEFORE CALLING ADC.16. USUALLY BEST TO DO THIS JUST BEFORE THE LOOP 
;STARTS DURING INIT OF OTHER VARIABLES.


;KNOWN PROBLEMS
	;12/4/2015: After calling this routine, I observed bizzare behavior of the monitor. if no LDA/LDX/LDY's were done
	;after the RTS from this routine, then the values in the RESULT(2) variable were observed as correct.
	;However, if an LDA/LDX/LDY was done immediately after the RTS from this routine, the values in the 
	;memory addresses (per the monitor) for OP1, OP2, and RESULT were no longer correct (it may have affected
	;other memory addresses too, I'm not sure).
	;However, I tested the program logic further downstream and at the BRK (which followed several LDAs, an 8-BIT SBC and some other code)
	;I was able to observe the correct values in the memory addresses for OP1,OP2, and RESULT, and a calculation which was derived from RESULT. 
	;I thought maybe the proximitey to JSR to this routine was the key factor, and it may be, but when I added
	;and LDX/LDY at the new BRK, the goofy behavior was exhibited. I added an LDA RESULT and everything was fine.
	;
	;BOTTOM LINE: be vigilant in programs using this routine. At the first sign of wierdness, start troubleshooting by
	;checking values in the memory addresses for the variables at the BRK instead of loading them into registers. 

;INIT VARIABLES
	LDA #$00
	STA RESULT
	STA RESULT+$01


; DO THE MATH
	CLD 
    SEC                           ;ALWAYS BEFORE SUBTRACTION
    LDA OP1
    SBC OP2
    STA RESULT
    LDA OP1+$1
    SBC OP2+$1
    STA RESULT+$1
    BCC .ERROR
    RTS
.ERROR    
    BRK	

;=================DEFINE VARIABLES===============


OP1      	.BS $2				;2byt
OP2      	.BS $2				;2byt
RESULT  	.BS $4				;2byt

;DIV.16 VARIABLES
DIVIDEND32	.BS $2			;UPPER 16-BIT PORTION OF DIVIDENT
DIVIDEND 	.BS $2			;NUMBER TO DIVIDE
DIVISOR 	.BS $2			;NUMBER TO DIVIDE BY
TEMP1	.BS $1
CARRY	.BS $1



;RANDOM.8 VARIABLES
;*****DO NOT CHANGE THE ORDER OF THESE VARIABLES.
;see note below
RND		.BS $2		;2byt
TMP		.BS $4
MOD		.BS $1
SEED0	.BS $1
SEED1	.BS $1
SEED2	.BS $1
SEED3	.BS $1
RND.LO	.BS $1
RND.HI	.BS $1
RAND_TABLE    .HS	01.01.00.FE.FF.01
RANDOM_NUMBER.ITERATION_TALLY	.BS $1
RND.ABORT	.BS $1							;IF $01 THEN RANDOM.8 WILL ABORT ON KEYPRESS AND LOAD $02 INTO THIS VARIABLE. 
;The issue seems to be with MOD. It appears to be a 2 byte variable used in .RA16,
;but if I define it as .BS $2, then the return value is always $00, not random. 
;by having it as .BS $1 .RA16 performs an operation on SEED0 (next variable in the list)
;intending it to be MOD+$1. Why this matter I don't know. SEED0 isn't supposed to be
;needed for 16-bit or 8bit return value. I tried doing an init of MOD+$1 and/OR SEED0
;to a power of 2 number but didn't work. 