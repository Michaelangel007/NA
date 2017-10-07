;************************INCLUDE FILE*****************************
;(do not assemble stand-alone. For a stand-alone version of this routine see examples/rwts2)
;
;LIST OF FUNCTIONS;
;
;ADC.16							;ADDITION, 16-BIT
;CMP.16							;COMPARE, 16-BIT
;IGR.16							;IS GREATER, 16-BIT
;SBC.16							;SUBTRACTION, 16-BIT

ADC.16			;==========ADDITION, 16-BIT=========
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


OP1      	.EQ $8100			;2byt
OP2      	.EQ $8102			;2byt
RESULT  	.EQ $8104	;2byt

;NORMAL DEFS
;OP1      	.BS	$2			;2byt
;OP2      	.BS	$2			;2byt
;RESULT  	.BS	$2			;2byt