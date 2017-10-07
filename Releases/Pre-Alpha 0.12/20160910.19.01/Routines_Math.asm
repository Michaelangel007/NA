;************************INCLUDE FILE*****************************
;(do not assemble stand-alone. For a stand-alone version of this routine see examples/rwts2)
;
;LIST OF FUNCTIONS;
;
;ADC.16							;ADDITION, 16-BIT
;CMP.16							;COMPARE, 16-BIT
;IGR.16							;IS GREATER, 16-BIT
;SBC.16							;SUBTRACTION, 16-BIT

ADC.16 ;==========ADDITION, 16-BIT (BCD SUPPORT)=========
@START
;PARAMETERS: OP1(2), OP2(2), [SED/CLD]*
;RETURN: RESULT(2)
;*SED: This routines was tested with decimal (BCD). To use
;	in BCD mode, use SED just before the JSR to call this routine, 
;	and CLD just after.

;WARNING: IF USING THIS FUNCTION WITH AN 8-BIT VALUE (BECAUSE THE LOOP MAY PRODUCE A 16-BIT VALUE), YOU
;MUST SET OP1+$1 AND OP2+$2 TO $00 BEFORE CALLING ADC.16. USUALLY BEST TO DO THIS JUST BEFORE THE LOOP 
;STARTS DURING INIT OF OTHER VARIABLES.

;==========================================================
;SOURCE CODE CREDIT
;"Using 6502 Assembly Language" by Randy Hyde
;==========================================================


; ;**DRIVER TEMPLATE**
	; LDA SMAP.CURRENT			;ADVANCE TO NEXT ROW ON THE SCREEN
	; STA OP1
	; LDA SMAP.CURRENT+$1
	; STA OP1+$1
	; LDA #OFFSET.DOWN
	; STA OP2
	; LDA #$00
	; STA OP2+$1
	;
	; JSR ADC.16						;SMAP.CURRENT(2) + #OFFSET.DOWN(1)
	;
	; LDA RESULT
	; STA SMAP.CURRENT
	; LDA RESULT+$1
	; STA SMAP.CURRENT+$1
	
	
	
; ;BCD MODE SWITCHING
	; CMP #$00			;is BCD parameter (ACC) enabled?
	; BEQ .START			;if no, start subroutine
	; SED					;if yes, enable BCD
	

.START	
;INIT VARIABLES
	LDA #$00
	STA RESULT
	STA RESULT+$01

; DO THE MATH ($A0F + $01)
	;CLD 
    CLC                     ;always before add
    LDA OP1
    ADC OP2
    STA RESULT
		 
    LDA OP1+$1
    ADC OP2+$1				;carry flag not cleared via CLC intentionally, it's part of 16-bit adding. 
    STA RESULT+$1
    BCS .ERROR

;	CLD						;Clear BCD mode, return to binary mode.
    RTS
.ERROR
;ADC OVERFLOW ERROR
;
;DISABLE.BS_RAM
	LDA $C082				;READ ENABLE ROM, DISABLE WRITE ON BSM (NORMAL STATE).
							;REQUIRED FOR BEFORE BRK BECAUSE APPLE MONITOR IS A ROM ROUTINE

	LDA TEXT
	BRK	

@END


BCD.UNPACK ;======UNPACK 2 BCD BYTES INTO 4 DIGITS=======
@START
;PARAMETERS: BCD (2)
;RETURN: RESULT (4)

;=====================SUBROUTINE DOCUMENTATION====================================
;
;Bit level operations are used to extract each BCD digit from 
;a nibble of the two BCD bytes passed as parameters. 
;
;The unpacked BCD digits are stored in RESULT with the lo order 
;BCD digits followed by the high order. Accordingly, to print
;the digits to the screen in the order visually expected, print
;in the following order: RESULT+$3, RESULT+$2,RESULT+$1,RESULT+$0  
;
;see UAL book page #9-19, 9-20
;=================================================================================


;;**DRIVER TEMPLATE**	
	; LDA #$12
	; STA BCD
	; LDA #$74
	; STA BCD+$1
	;;
	; JSR BCD.UNPACK
	;;
	; LDA RESULT+$1
	; LDX RESULT+$2
	; LDY RESULT+$3
	; BRK
	
	

;UNPACK 1ST TWO BCD DIGITS (BCD BYTE 0)
	LDA BCD		
	AND #$F				;mask out the higher order nibble
	STA RESULT			;store the lower order nibble as the 1st BCD digit
	LDA BCD				
	LSR 				;move the higher order nibble into the lower order nibble
	LSR
	LSR
	LSR
	STA RESULT+$1		;store the lower order nibble as the 2nd BCD digit

	
;UNPACK 2ND TWO BCD DIGITS (BCD BYTE 1)
	LDA BCD+$1
	AND #$F				;mask out the higher order nibble
	STA RESULT+$2		;store the lower order nibble as the 3rd BCD digit
	LDA BCD+$1
	LSR 				;move the higher order nibble into the lower order nibble
	LSR
	LSR
	LSR
	STA RESULT+$3		;store the lower order nibble as the 4th BCD digit	
	
	RTS

@END


CONVERT.BCD_PACKED.ASCII	
@START
;PARAMETERS: BCD (2)
;RETURN: RESULT (4)

;=====================SUBROUTINE DOCUMENTATION====================================
;
;Bit level operations are used to extract each BCD digit from 
;a nibble of the two BCD bytes passed as parameters. 
;
;The unpacked BCD digits are stored as ASCII values in RESULT 
;with the lo order BCD digits followed by the high order.
;Accordingly, to print the digits to the screen in the order 
;visually expected, print in the following order: 
;RESULT+$3, RESULT+$2,RESULT+$1,RESULT+$0  
;
;;see UAL book page #9-19, 9-20
;=================================================================================


; ;**DRIVER TEMPLATE**	
	; LDA #$12
	; STA BCD
	; LDA #$74
	; STA BCD+$1
	; ;
	; JSR CONVERT.BCD_PACKED.ASCII
	; ;
	; LDA RESULT
	; STA VARIABLE0
	; ;
	; LDA RESULT+$1
	; STA VARIABLE1
	; ;
	; LDA RESULT+$2
	; STA VARIABLE2	
	; ;
	; LDA RESULT+$3
	; STA VARIABLE3
	
	

;UNPACK 1ST TWO BCD DIGITS (BCD BYTE 0)
	LDA BCD		
	AND #$F				;mask out the higher order nibble
	CLC					;convert BCD to ASCII value
	ADC #$B0
	STA RESULT			;store the lower order nibble as the 1st BCD digit
	LDA BCD				
	LSR 				;move the higher order nibble into the lower order nibble
	LSR
	LSR
	LSR
	CLC					;convert BCD to ASCII value
	ADC #$B0
	STA RESULT+$1		;store the lower order nibble as the 2nd BCD digit

	
;UNPACK 2ND TWO BCD DIGITS (BCD BYTE 1)
	LDA BCD+$1
	AND #$F				;mask out the higher order nibble
	CLC					;convert BCD to ASCII value
	ADC #$B0
	STA RESULT+$2		;store the lower order nibble as the 3rd BCD digit
	LDA BCD+$1
	LSR 				;move the higher order nibble into the lower order nibble
	LSR
	LSR
	LSR
	CLC					;convert BCD to ASCII value
	ADC #$B0
	STA RESULT+$3		;store the lower order nibble as the 4th BCD digit	
	
	RTS

@END

CONVERT.HEX_TO_ASCII
@START
;PARAMETERS: ACC = 8-bit hex # to convert 
;RETURN: RESULT(2) ASCII code that represents the hex number contained in each nibble of the ACC parameter

;=====================SUBROUTINE DOCUMENTATION====================================
;
;Converts a hex value input to the ASCII codes needed to display that hex value onscreen. 
;
;Each nibble of the hex value parameter requires two an ASCII code to represent it. 
;An ASCII code is an 8-bit value. Thus we refer to the LO ASCII value and the HO ASCII 
;value with resepect to the ASCII representation of a single hex nibble of the input parameter.  
;
;map of the return value
;
;RESULT+$0 = ASCII code of parameter lo nibble value 
;RESULT+$1 = ASCII code of parameter ho nibble value
;
;To print the ASCII codes on screen, first print RESULT+$1 then RESULT+$0
;
;EXPECTED RESULT: X-REG = $B9, Y-REG = $B9
;
;=================================================================================


; .TEMPLATE
;
		; LDA #$FF
	; JSR CONVERT.HEX_TO_ASCII
;	
	; LDA #$AA
	; LDX RESULT+$1
	; LDY RESULT+$0
	; BRK
	

.CONVERT.LO_NIBBLE
	;ACC = parameter
	PHA ;save parameter to stack
	AND #$0F  ;mask out HO nibble
	CMP #$0A ;is ACC >= $0A.
	BCS .LO_NIBBLE.GRE.A ;If yes, we need to to do calculations to translate between the hex value and the LO ASCII value of this nibble. 
	;**FALLS THROUGH** ;If no, then the hex value of this nibble is already equal to the ASCII value. All that's left to do is mask in the HO ASCII value  
	
.LO_NIBBLE.LTH.A
	ORA #$B0	;masks in $B as the HO ASCII value, which is placed in the HO nibble of the return value. This is done because the ASCII code for the numbers $0-$09 all start with $B (i.e. $B0-B9)
	STA RESULT+$0 ;save return value
	
	JMP .CONVERT.HO_NIBBLE

.LO_NIBBLE.GRE.A
	SEC
	SBC #$09	;convert to ASCII (i.e. the LO nibble of the ascii code for the hex value $0A is $1, and $0A - $09 = $1)
	ORA #$E0	;masks in $E as the HO ASCII value, which is placed in the HO nibble of the return value. This is done because the ASCII code for the letters $A-$F all start with $E in lowercase (i.e. $E1-E6)
	STA RESULT+$0
	
	;**FALLS THROUGH**


.CONVERT.HO_NIBBLE
	PLA ;restore ACC parameter from stack
	LSR ;Move bits from HO nibble to low nibble
	LSR
	LSR
	LSR
	CMP #$0A ;is ACC >= $0A.
	BCS .HO_NIBBLE.GRE.A ;If yes, we need to to do calculations to translate between the hex value and the LO ASCII value of this nibble. 
	;**FALLS THROUGH** ;If no, then the hex value of this nibble is already equal to the ASCII value. All that's left to do is mask in the HO ASCII value  

.HO_NIBBLE.LTH.A
	ORA #$B0	;masks in $B as the HO ASCII value, which is placed in the HO nibble of the return value. This is done because the ASCII code for the numbers $0-$09 all start with $B (i.e. $B0-B9)
	STA RESULT+$1 ;save return value
	JMP .EXIT

.HO_NIBBLE.GRE.A
	SEC
	SBC #$09	;convert to ASCII (i.e. the LO nibble of the ascii code for the hex value $0A is $1, and $0A - $09 = $1)
	ORA #$E0	;masks in $E as the HO ASCII value, which is placed in the HO nibble of the return value. This is done because the ASCII code for the letters $A-$F all start with $E in lowercase (i.e. $E1-E6)
	STA RESULT+$1
	;**FALLS THROUGH**	

.EXIT
	RTS
	
@END
		
;CONVERT.ASCII_NUMBER.TO.HEX
		;These are my notes on an idea I had on how to do it. 
		;converting single digit ascii number to hex: mask out the HO nibble
		;converting two digits ascii number to hex: mask out HO nibble for both digits. logical shift left the bits for the HO digit into the HO nibble, then mask in that vaue to the LO digit.
		;	let's say the number is $12. 1 is the HO digits and 2 is the LO digit. 


CMP.16 ;==========COMPARE, 16-BIT  (BCD SUPPORT)=========
@START
;PARAMETERS: OP1(2), OP2(2),  [SED/CLD]*
;RETURN: ACC ($00 EQUAL, $01 NOT EQUAL)
;*SED: This routines was tested with decimal (BCD). To use
;	in BCD mode, use SED just before the JSR to call this routine, 
;	and CLD just after.

;WARNING: IF USING THIS FUNCTION WITH AN 8-BIT VALUE (BECAUSE THE LOOP MAY PRODUCE A 16-BIT VALUE), YOU
;MUST SET OP1+$1 AND OP2+$2 TO $00 BEFORE CALLING ADC.16. USUALLY BEST TO DO THIS JUST BEFORE THE LOOP 
;STARTS DURING INIT OF OTHER VARIABLES.

;==========================================================
;SOURCE CODE CREDIT
;"Using 6502 Assembly Language" by Randy Hyde
;==========================================================

; ;***DRIVER TEMPLATE	***
	; LDA #$10
	; STA OP1
	; LDA #$20
	; STA OP1+$1
	;
	; LDA #$10
	; STA OP2
	; LDA #$20
	; STA OP2+$1
	;	
	; JSR CMP.16	;RETURN: ACC ($00 EQUAL, $01 NOT EQUAL)

		
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
@END

DIV.8.BCD ;=======DIVISION, 16-BIT (BCD SUPPORT)=========
@START
;DESCRIPTION: 8-BIT DIVIDEND, 8-BIT DIVISOR, QUOTIENT, REMAINDER
;PARAMETERS: DIVIDEND(1), DIVISOR(1)
;ENTRANCE: DIRECT
;RETURN:	RESULT(4)
;			result0 = QUOTIENT
;			result1 = not used
;			result2 = REMAINDER
;			result3 = not used

;===========SOURCE CODE CREDIT===========
;I wrote this one. 
;==========================================================


;=====================SUBROUTINE DOCUMENTATION====================================
;This subroutine calculates the result of a divison operation by
;counting the number of times it can subtract the divisor from 
;the dividend without resulting in a number less than zero, which
;is detected as an underflow (carry flag clear).
;
;The quotient is equal to the "count" mentioned above.
;
;The remainder is the amount of the dividend left before the 
;subtraction which causes the underflow. 
;
;----NOTES ON CREATING A 16-BIT DIVISION ROUTINE FOR BCD--
;
;I think all I can just upgrade this routine to use 
;SBC 16 techniques in the loop when deducting the divisor from 
;the dividend until an overflow occurs
;
;=================================================================================




; ;****DRIVER TEMPLATE***	
	;
	; LDA #$53
	; STA DIVIDEND
	; LDA #$8
	; STA DIVISOR
	;
	;
	; JSR DIV.8.BCD
	;	
	; LDA #$00
	; LDX RESULT
	; LDY RESULT+$2
	; BRK

		
	SED			;automatically set decimal mode

	
;INIT VARIABLES
	LDA #$00		
	STA QUOTIENT.COUNTER	
	STA RESULT
	STA RESULT+$2

.LOOP
;INCREMENT QUOTIENT VALUE


;SUBTRACT DIVISOR FROM DIVIDEND
	LDA DIVIDEND
	SEC
	SBC DIVISOR
	BEQ .NO_REMAINDER
	BCC .UNDERFLOW
	STA DIVIDEND			;use dividend variable for running value	
	LDA QUOTIENT.COUNTER	
	CLC
	ADC #$01				;the quotient counter increases with each subtraction because the quotient is equal to the number of times the divisor can be subtracted from the dividend without the result being less than zero. 
	STA QUOTIENT.COUNTER
	JMP .LOOP
	
.UNDERFLOW
	LDA DIVIDEND			;the value of dividend before the SBC in the loop that resulted in the underflow is the remainder value. 
	STA RESULT+$2			;SET REMAINDER
	
	LDA QUOTIENT.COUNTER	;the quotient is equal to the number of times the divisor can be subtracted from the dividend without the result being less than zero.
	STA RESULT				;SET QUOTIENT
	
	JMP .EXIT
	
.NO_REMAINDER
	LDA QUOTIENT.COUNTER	;we have to do a final increment to the counter because the BEQ out of the loop occured before the counter increment. 	
	CLC
	ADC #$01
	STA RESULT				;SET QUOTIENT
	
.EXIT
	CLD						;clear decimal mode, return to binary mode
	RTS
	
@END

DIV.16 ;==========DIVISION, 16-BIT (**NO BCD SUPPORT**)=========
@START   
;DESCRIPTION: 32-BIT DIVIDEND, 16-BIT DIVISOR, QUOTIENT, REMAINDER
;PARAMETERS: DIVIDEND(2), DIVIDEND32(2)*, DIVISOR
;ENTRANCE: DIRECT
;RETURN:	RESULT(4)
;			result0 = quotient, lo byt
;			result1 = quotient, hi byt
;			result2 = remainder, lo byt
;			result3 = remainder, hi byt
;
;*DIVIDEND32(2)*Not needed if dividend is 16-bit, but must be init to $00 before calling this subroutine
;==========================================================
;SOURCE CODE CREDIT
;http://6502.org/source/integers/ummodfix/ummodfix.htm
;By Garth Wilson (2002)
;==========================================================


;****DRIVER TEMPLATE****
	; LDA MOB.CANDIDATE.RMAP
	; STA DIVIDEND
	; LDA MOB.CANDIDATE.RMAP+$1
	; STA DIVIDEND+$1
	; LDA	#OFFSET.UP
	; STA DIVISOR
	; LDA #$00
	; STA DIVISOR+$1
	; STA DIVIDEND32
	; STA DIVIDEND32+$1
	;
	; JSR DIV.16						
	;
	; LDX RESULT 					;QUOTIENT
	; STA MOB.CANDIDATE.MAP.Y			
	; LDA RESULT+$2					;REMAINDER
	; STY MOB.CANDIDATE.MAP.X		
	
	
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

@END

DIV.16.BCD
;NOTES ON CREATING A 16-BIT DIVISION ROUTINE FOR BCD
;
;
;I think all I can just upgrade the 8-BIT routine to use 
;SBC 16 techniques in the loop when deducting the divisor from 
;the dividend until an overflow occurs

		 
IGRE.16	;=========IS >= (BCD SUPPORT)==========
@START
;PARAMETERS: OP1(2), OP2(2), [SED/CLD]*
;RETURN: CARRY FLAG (SET >=, NOT SET !>=)
;*SED: This routines was tested with decimal (BCD). To use
;	in BCD mode, use SED just before the JSR to call this routine, 
;	and CLD just after.

;WARNING: IF USING THIS FUNCTION WITH AN 8-BIT VALUE (BECAUSE THE LOOP MAY PRODUCE A 16-BIT VALUE), YOU
;MUST SET OP1+$1 AND OP2+$2 TO $00 BEFORE CALLING ADC.16. USUALLY BEST TO DO THIS JUST BEFORE THE LOOP 
;STARTS DURING INIT OF OTHER VARIABLES.

;==========================================================
;SOURCE CODE CREDIT
;"Using 6502 Assembly Language" by Randy Hyde
;==========================================================

;****DRIVER TEMPLATE***
	; LDA #$10
	; STA OP1
	; LDA #$20
	; STA OP1+$1
	;
	; LDA #$10
	; STA OP2
	; LDA #$21
	; STA OP2+$1
	;
		; LDA #$01
		;
		; SED
	; JSR IGRE.16	;RETURN: CARRY FLAG (SET >=, NOT SET !>=)
		; CLD
		;
	; BCS .ISGRE
	; LDA #$00
	; BRK

.ISGRE
	LDA #$AA
	
	BRK
	
;IS OP1 >= OP2?	
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
@END

RANDOM.8.INIT ;======INIT ROUTINE FOR 8-BIT RANDOM NUMBER GENERATOR
@START
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
	BNE .STANDARD.INIT
	LDA $4E
	BNE .STANDARD.INIT
							;if $4E and $4F are both $00 then they need to be set to a different value
							;since these addresses are used as the seed value for the random number algorithm.
.ZERO_OVERRIDE							
	LDA #$DB
	STA $4F				;**OPT** Memory. Speed. May not be needed. See if random.8 ever uses $4F/$4E directly
	STA SEED2
	
	LDA #$BD
	STA $4E				;**OPT** Memory. Speed. May not be needed. See if random.8 ever uses $4F/$4E directly
	STA SEED3
	JMP .COMMON.INIT

.STANDARD.INIT	
	LDA $4F
	STA SEED2
	LDA $4E
	STA SEED3
	;**FALLS THROUGH**
	
.COMMON.INIT	
	LDA #$01
	STA RND.LO		;see note above before using a different return value. 
	LDA #$FF
	STA RND.HI
			
			
	LDA #$02
	STA MOD
	
	RTS
	
@END

MLP.16.NO_BCD	;16-BIT MULTIPLICATION (**NO BCD SUPPORT**)
@START
;PARAMETERS: MULPLR(2), MULCND(2)
;RETURN: RESULT(2)

;**DRIVER TEMPLATE**
        ; LDA #$D0
        ; STA MULPLR
        ; LDA #$40
        ; STA MULPLR+$1
; 		;
        ; LDA #$02
        ; STA MULCND
        ; LDA #$00
        ; STA MULCND+$01
		;
        ; JSR MLP.16.NO_BCD
		;
		; LDX RESULT
		; LDY RESULT+$1
		;	 
		; BRK
		
		
;SAVE REGISTERS
        PHA 
        TYA 
 ;       PHA
		
;INIT VARIABLES
        LDA #$0
        STA PARTIAL
        STA PARTIAL+$1
		
.USMUL1
        LDY #$10
.USMUL2
        LDA MULPLR
        LSR 
        BCC .USMUL4
        CLC 
        LDA PARTIAL
        ADC MULCND
		STA PARTIAL
        LDA PARTIAL+$1
        ADC MULCND+$1
        STA PARTIAL+$1
.USMUL4
        ROR PARTIAL+$1
        ROR PARTIAL
        ROR MULPLR+$1
        ROR MULPLR
;
;see if done yet
;
        DEY 
        BNE .USMUL2

.WRAPUP
;check for overflow (>655,536)
        LDA PARTIAL
        ORA PARTIAL+$1
        BNE .ERROR

;
;STORE RESULT OF MULTIPLICATION
        LDA MULPLR
        STA RESULT
		LDA MULPLR+$1
        STA RESULT+$1

.EXIT	
;RESTORE REGISTERS	
        PLA 
        TAY 
 ;       PLA 
        RTS 

		
.ERROR
;overflow error has occured (result > 655,536)
        BRK 

@END

MLP.16	;16-BIT MULTIPLICATION (BCD SUPPORT)
@START
;PARAMETERS: MULPLR(2), MULCND(2), [SED/CLD]
;RETURN: RESULT(2)
;*SED: This routines was tested with decimal (BCD). To use
;	in BCD mode, use SED just before the JSR to call this routine, 
;	and CLD just after.

;===========SOURCE CODE CREDIT===========
;I wrote this one. 
;==========================================================

;=====================SUBROUTINE DOCUMENTATION====================================
;This subroutine calculates the result of a multiplication operation
;by using a loop to add the multicand to itself until the loop counter
;is equal to the multicand.
;
;By using 16-bit addition, this routine can calculate a 16-bit result.
;Keep in mind that while the routine can accept a 16-bit multiplier and
;16-bit multicand, problems will occur if the result is a number 
;larger than 16-bits. 
;
;=================================================================================


; ;**DRIVER TEMPLATE**
	; LDA #$01
	; STA MULPLR
	; LDA #$00
	; STA MULPLR+$1
	; ;
	; LDA #$99
	; STA MULCND
	; LDA #$00
	; STA MULCND+$01
	;
	; JSR MLP.16
	;	
	; LDX RESULT
	; LDY RESULT+$1
		

		
;INIT VARIABLES, COUNTERS
	LDA #$01						;counter is set to $01 because MULPLR.TALLY doesn't start at $00, it starts at the value of MULPLR
	STA MULCND.COUNTER
	
	LDA MULPLR
	STA MULPLR.TALLY
	
.LOOP
;16-BIT ADDITION
	LDA MULPLR.TALLY
	CLC
	ADC MULPLR
	STA MULPLR.TALLY
	LDA MULPLR.TALLY+$01
	ADC #$00
	STA MULPLR.TALLY+$01

	LDA MULCND.COUNTER		
	CLC
	ADC #$01				;increment loop counter
	STA MULCND.COUNTER
			
	LDA MULCND.COUNTER
	CMP MULCND
	BNE .LOOP


			
;STORE RESULT OF MULTIPLICATION
	LDA MULPLR.TALLY
	STA RESULT
	LDA MULPLR.TALLY+$1
	STA RESULT+$1

		
.EXIT
	RTS
@END

RANDOM.8
@START
;PARAMETERS: RND.LO, RND.HI, RND.ABORT
;PARAMETERS: (FILLED BY RANDOM.8.INIT): MOD, SEED2 (LOAD $4F TO THIS VARIABLE), SEED3 (LOAD $4E TO THIS VARIABLE),
;RETURN: ACC (8-BIT RANDOM NUMBER), for convenience also in RND
;ENTRANCE: DIRECT
;CUSTOMIZATIONS: added range capability, optimized for speed 
;NOTE: RND.ABORT = $01, then routine will abort at certain points if a key is pressed.
;      if an abort occurs due to a keypress, $02 is loaded into this variable on return. 


;=======SOURCE CODE CREDIT=================================
;
;http://6502.org/source/integers/random/random.html search for "random16". I couldn't get the random8 code to work, so I used the random16 code, which ironically returns $00 for the HO byte so it's effectively an 8-bit generator. 
;
;Notes on which code on the above page was used
;The RAND routine used was the 2nd RAND routine up from the "RANDOM8" code listing. I got
;the impression that the author provided a bunch of RAND routines, each with advantages/disadvatanges, like speed.
;When I tried the random8 code I used the RAND routine directly above it but since I had problems I 
;I tried the 2nd RAND routine up from random8 when I started working with the random16 code.
;==========================================================
	

	
;====DRIVER TEMPLATE=====
;NOTE: the following are templates for the call to this subroutine
;and also 2-5 decision points based on a $01-$FF random number returned.
;For example, .PATHS_2 has two decision points and .PATHS_5 has five decision points.
;
; .PATHS_2
	; JSR RANDOM.8
	; CMP #$80
	; BCS .P2.OPTION1.CANDIDATE
; ;P2.OPTION0
	; LDA MOB.MOVE.OPEN_PATHS
	; STA MOB.MOVE.CURRENT
	; JMP MOB.MOVE.MAKE
; .P2.OPTION1.CANDIDATE
	; LDA MOB.MOVE.OPEN_PATHS+$1
	; STA MOB.MOVE.CURRENT
	; JMP MOB.MOVE.MAKE
	; ;
; .PATHS_3 ;SPLIT POINTS $55, $AA
	; JSR RANDOM.8
	; CMP #$55
	; BCS .PATHS_3.NEXT_TEST
; ;P3.OPTION0
	; LDA MOB.MOVE.OPEN_PATHS
	; STA MOB.MOVE.CURRENT
	; JMP MOB.MOVE.MAKE
; .PATHS_3.NEXT_TEST
	; CMP #$AA
	; BCC .P3.OPTION2.CANDIDATE
; ;P3.OPTION1
	; LDA MOB.MOVE.OPEN_PATHS+$1
	; STA MOB.MOVE.CURRENT
	; JMP MOB.MOVE.MAKE
; .P3.OPTION2.CANDIDATE
	; LDA MOB.MOVE.OPEN_PATHS+$2
	; STA MOB.MOVE.CURRENT
	; JMP MOB.MOVE.MAKE
	; ;
; .PATHS_4 ;SPLIT POINTS $3F, $74, $BF
	; JSR RANDOM.8
	; CMP #$3F
	; BCS .PATHS_4.NEXT_TEST1
; ;P4.OPTION0
	; LDA MOB.MOVE.OPEN_PATHS
	; STA MOB.MOVE.CURRENT
	; JMP MOB.MOVE.MAKE
; .PATHS_4.NEXT_TEST1
	; CMP #$BF
	; BCS .P4.OPTION3.CANDIDATE
; ;PATHS_4.NEXT_TEST2 
	; CMP #$74
	; BCS .P4.OPTION2.CANDIDATE
; ;P4.OPTION1	
	; LDA MOB.MOVE.OPEN_PATHS+$1
	; STA MOB.MOVE.CURRENT
	; JMP MOB.MOVE.MAKE
; .P4.OPTION2.CANDIDATE
	; LDA MOB.MOVE.OPEN_PATHS+$2
	; STA MOB.MOVE.CURRENT
	; JMP MOB.MOVE.MAKE
; .P4.OPTION3.CANDIDATE
	; LDA MOB.MOVE.OPEN_PATHS+$3
	; STA MOB.MOVE.CURRENT
	; JMP MOB.MOVE.MAKE
	; ;
	; ;
; .PATHS_5 ;SPLIT POINTS $33, $66, $99, $CC,
	; JSR RANDOM.8
	; CMP #$33
	; BCS .PATHS_5.NEXT_TEST1
; ;P5.OPTION0
	; LDA MOB.MOVE.OPEN_PATHS
	; STA MOB.MOVE.CURRENT
	; JMP MOB.MOVE.MAKE
; .PATHS_5.NEXT_TEST1
	; CMP #$CC
	; BCS .P5.OPTION3.CANDIDATE
; ;PATHS_5.NEXT_TEST2 
	; CMP #$99
	; BCS .P5.OPTION2.CANDIDATE
; ;PATHS_5.NEXT_TEST3
	; CMP #$66
	; BCS .P5.OPTION4.CANDIDATE		
; ;P5.OPTION1	
	; LDA MOB.MOVE.OPEN_PATHS+$1
	; STA MOB.MOVE.CURRENT
	; JMP MOB.MOVE.MAKE
; .P5.OPTION2.CANDIDATE
	; LDA MOB.MOVE.OPEN_PATHS+$2
	; STA MOB.MOVE.CURRENT
	; JMP MOB.MOVE.MAKE
; .P5.OPTION3.CANDIDATE
	; LDA MOB.MOVE.OPEN_PATHS+$3
	; STA MOB.MOVE.CURRENT
	; JMP MOB.MOVE.MAKE
; .P5.OPTION4.CANDIDATE
	; LDA MOB.MOVE.OPEN_PATHS+$4
	; STA MOB.MOVE.CURRENT
	; ;
	
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

@END

SBC.16	;==========SUBTRACTION, 16-BIT (BCD SUPPORT)=========
@START
;PARAMETERS: OP1(2), OP2(2), [SED/CLD]*
;RETURN: RESULT(2)
;*SED: This routines was tested with decimal (BCD). To use
;	in BCD mode, use SED just before the JSR to call this routine, 
;	and CLD just after.

;WARNING: IF USING THIS FUNCTION WITH AN 8-BIT VALUE (BECAUSE THE LOOP MAY PRODUCE A 16-BIT VALUE), YOU
;MUST SET OP1+$1 AND OP2+$2 TO $00 BEFORE CALLING ADC.16. USUALLY BEST TO DO THIS JUST BEFORE THE LOOP 
;STARTS DURING INIT OF OTHER VARIABLES.

;==========================================================
;SOURCE CODE CREDIT
;"Using 6502 Assembly Language" by Randy Hyde
;==========================================================

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
@END

;=================DEFINE VARIABLES===============


OP1      	.BS $2				;2byt
OP2      	.BS $2				;2byt
RESULT  	.BS $4				;2byt

;DIV.16 VARIABLES
;==========KEEP THESE VARIBLES IN THIS ORDER=======
;(the memory space is shared with other routines)
DIVIDEND32	.BS $2			;UPPER 16-BIT PORTION OF DIVIDEND
DIVIDEND 	.BS $2			;NUMBER TO DIVIDE
DIVISOR 	.BS $2			;NUMBER TO DIVIDE BY
TEMP1	.BS $1
CARRY	.BS $1
;===================================================



;DIV.8.BCD VARIABLES
QUOTIENT.COUNTER 	.EQ TEMP1

;MLP.16 VARIABLES	
MULPLR   .EQ DIVIDEND32
PARTIAL  .EQ DIVIDEND
MULCND   .EQ DIVISOR 

MULPLR.TALLY	.EQ	DIVIDEND
MULCND.COUNTER	.EQ TEMP1		

;BCD.UNPACK
BCD      		.EQ DIVIDEND32



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
RND.ABORT	.BS $1	

SAVED.RANDOM.NUMBER	.BS $1 ;variable for storing a random number for future use. 

;IF $01 THEN RANDOM.8 WILL ABORT ON KEYPRESS AND LOAD $02 INTO THIS VARIABLE. 
;The issue seems to be with MOD. It appears to be a 2 byte variable used in .RA16,
;but if I define it as .BS $2, then the return value is always $00, not random. 
;by having it as .BS $1 .RA16 performs an operation on SEED0 (next variable in the list)
;intending it to be MOD+$1. Why this matter I don't know. SEED0 isn't supposed to be
;needed for 16-bit or 8bit return value. I tried doing an init of MOD+$1 and/OR SEED0
;to a power of 2 number but didn't work. 



;MULTIPLICATION BY 6 
;used to calculate the offset to the base address of the 16-bit array NPC.PATHFINDER.SEARCH.PATHS
; NPC.PATHFINDER.MULTIPLY_TABLE.LO	.HS 00.06.0C.12.18.1E.24.2A.30.36.3C.42.48.4E.54.5A.60.66.6C.72.78.7E.84.8A.90.96.9C.A2.A8.AE.B4.BA.C0.C6.CC.D2.D8.DE.E4.EA.F0.F6.FC.02.08.0E.14.1A.20.26.2C.32.38.3E.44.4A.50.56.5C.62.68.6E.74.7A.80
; NPC.PATHFINDER.MULTIPLY_TABLE.HO	.HS 00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01

;MULTIPLICATION BY 8
; NPC.PATHFINDER.MULTIPLY_TABLE.LO	.HS	00.08.10.18.20.28.30.38.40.48.50.58.60.68.70.78.80.88.90.98.A0.A8.B0.B8.C0.C8.D0.D8.E0.E8.F0.F8.00.08.10.18.20.28.30.38.40.48.50.58.60.68.70.78.80.88.90.98.A0.A8.B0.B8.C0.C8.D0.D8.E0.E8.F0.F8.00.08.10.18.20.28.30.38.40.48.50.58.60.68.70.78.80.88.90.98.A0.A8.B0.B8.C0.C8.D0.D8.E0.E8.F0.F8.00.08.10.18.20.28.30.38.40.48.50.58.60.68.70.78.80.88.90.98.A0.A8.B0.B8.C0.C8.D0.D8.E0.E8.F0.F8.00.08.10.18.20.28.30.38.40.48.50.58.60.68.70.78.80.88.90.98.A0.A8.B0.B8.C0.C8.D0.D8.E0.E8.F0.F8.00.08.10.18.20.28.30.38.40.48.50.58.60.68.70.78.80.88.90.98.A0.A8.B0.B8.C0.C8.D0.D8.E0.E8.F0.F8.00.08.10.18.20.28.30.38.40.48.50.58.60.68.70.78.80.88.90.98.A0.A8.B0.B8.C0.C8.D0.D8.E0.E8.F0.F8.00.08.10.18.20.28.30.38.40.48.50.58.60.68.70.78.80.88.90.98.A0.A8.B0.B8.C0.C8.D0.D8.E0.E8.F0.F8
; NPC.PATHFINDER.MULTIPLY_TABLE.HO	.HS	00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07
