; ============================================
; Copyright (C) 2016-2018. 6502 Workshop, LLC
; contact: mark@6502workshop.com
; ============================================


;************************INCLUDE FILE*****************************
;(do not assemble stand-alone.  )
;
;LIST OF FUNCTIONS;
;
;ADC.16							;ADDITION, 16-BIT
;CMP.16							;COMPARE, 16-BIT
;IGR.16							;IS GREATER, 16-BIT
;SBC.16							;SUBTRACTION, 16-BIT

;ADC.16 ;==========ADDITION, 16-BIT (BCD SUPPORT)=========
@START
; ;PARAMETERS: OP1(2), OP2(2), [SED/CLD]*
; ;RETURN: RESULT(2)
; ;*SED: This routines was tested with decimal (BCD). To use
; ;	in BCD mode, use SED just before the JSR to call this routine, 
; ;	and CLD just after.

; ;WARNING: IF USING THIS FUNCTION WITH AN 8-BIT VALUE (BECAUSE THE LOOP MAY PRODUCE A 16-BIT VALUE), YOU
; ;MUST SET OP1+$1 AND OP2+$2 TO $00 BEFORE CALLING ADC.16. USUALLY BEST TO DO THIS JUST BEFORE THE LOOP 
; ;STARTS DURING INIT OF OTHER VARIABLES.

; ;==========================================================
; ;SOURCE CODE CREDIT
; ;"Using 6502 Assembly Language" by Randy Hyde
; ;==========================================================


; ; ;**DRIVER TEMPLATE**
	; ; LDA SMAP.CURRENT			;ADVANCE TO NEXT ROW ON THE SCREEN
	; ; STA OP1
	; ; LDA SMAP.CURRENT+$1
	; ; STA OP1+$1
	; ; LDA #OFFSET.DOWN
	; ; STA OP2
	; ; LDA #$00
	; ; STA OP2+$1
	; ;
	; ; JSR ADC.16						;SMAP.CURRENT(2) + #OFFSET.DOWN(1)
	; ;
	; ; LDA RESULT
	; ; STA SMAP.CURRENT
	; ; LDA RESULT+$1
	; ; STA SMAP.CURRENT+$1
	
	
	
; ; ;BCD MODE SWITCHING
	; ; CMP #$00			;is BCD parameter (ACC) enabled?
	; ; BEQ .START			;if no, start subroutine
	; ; SED					;if yes, enable BCD
	

; .START	
; ;INIT VARIABLES
	; LDA #$00
	; STA RESULT
	; STA RESULT+$01

; ; DO THE MATH ($A0F + $01)
	; ;CLD 
    ; CLC                     ;always before add
    ; LDA OP1
    ; ADC OP2
    ; STA RESULT
		 
    ; LDA OP1+$1
    ; ADC OP2+$1				;carry flag not cleared via CLC intentionally, it's part of 16-bit adding. 
    ; STA RESULT+$1
    ; BCS .ERROR

; ;	CLD						;Clear BCD mode, return to binary mode.
    ; RTS
; .ERROR
; ;ADC OVERFLOW ERROR
; ;
; ;DISABLE.BS_RAM
	; JSR PREP.BRK
	; BRK	

@END

;BCD.UNPACK ;======UNPACK 2 BCD BYTES INTO 4 DIGITS=======
@START
;PARAMETERS: BCD (2)
;RETURN: RESULT (4)

; ;=====================SUBROUTINE DOCUMENTATION====================================
; ;
; ;Bit level operations are used to extract each BCD digit from 
; ;a nibble of the two BCD bytes passed as parameters. 
; ;
; ;The unpacked BCD digits are stored in RESULT with the lo order 
; ;BCD digits followed by the high order. Accordingly, to print
; ;the digits to the screen in the order visually expected, print
; ;in the following order: RESULT+$3, RESULT+$2,RESULT+$1,RESULT+$0  
; ;
; ;see UAL book page #9-19, 9-20
; ;=================================================================================


; ;;**DRIVER TEMPLATE**	
	; ; LDA #$12
	; ; STA BCD
	; ; LDA #$74
	; ; STA BCD+$1
	; ;;
	; ; JSR BCD.UNPACK
	; ;;
	; ; LDA RESULT+$1
	; ; LDX RESULT+$2
	; ; LDY RESULT+$3
	; ; BRK
	
	

; ;UNPACK 1ST TWO BCD DIGITS (BCD BYTE 0)
	; LDA BCD		
	; AND #$F				;mask out the higher order nibble
	; STA RESULT			;store the lower order nibble as the 1st BCD digit
	; LDA BCD				
	; LSR 				;move the higher order nibble into the lower order nibble
	; LSR
	; LSR
	; LSR
	; STA RESULT+$1		;store the lower order nibble as the 2nd BCD digit

	
; ;UNPACK 2ND TWO BCD DIGITS (BCD BYTE 1)
	; LDA BCD+$1
	; AND #$F				;mask out the higher order nibble
	; STA RESULT+$2		;store the lower order nibble as the 3rd BCD digit
	; LDA BCD+$1
	; LSR 				;move the higher order nibble into the lower order nibble
	; LSR
	; LSR
	; LSR
	; STA RESULT+$3		;store the lower order nibble as the 4th BCD digit	
	
	; RTS

@END

;CONVERSIONS (low level)
@START
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

CONVERT.BCD_TO_HEX ;8* digit packed BCD -> 32-bit HEX 
@START
;PARAMETERS: BCD(4)*
;ENTRANCE: direct
;RETURN: BIN(4) (this is a .EQ to BINARY)
;
;*To use all 8 BCD digits, must comment out the .INIT.DISABLE.BCD5_6_7_8 section and make sure
;to init to $0 any BCD digits not used for the input result. For example, for 6 BCD digits, 
;BCD+$0-2 contains the packed input value and BCD+$3 is init to $0. 
;
;Souce Code from 6502 Software Design, Expanded by Greg
;http://6502.org/source/integers/32bcdbin.htm


;TEMPLATE
;Converts !999 to hex. expected result: $3E7
		;
		; LDA #$99
		; STA BCD+$0
		; LDA #$09 ;this byte represents the 3rd and 4th BCD digit in the order 4th/3rd. This is because it's in the format of HO/LO. 
		; STA BCD+$1
	;JSR CONVERT.BCD_TO_HEX
		;;RETURN: BIN(4)

		
;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA
	
		
.INIT
		LDA #$00
		STA BINARY+$0
		STA BINARY+$1
		STA BINARY+$2
		STA BINARY+$3
		
.INIT.DISABLE.BCD5_6_7_8 ;disables the last 4 BCD digits		
		;ACC: $00 expected
		STA BCD+$2
		STA BCD+$3		
		
BCD_BIN
 
			LDA #$00
			sta     BINARY+3 ;Reset MSBY
           jsr     NXT_BCD  ;Get next BCD value
           sta     BINARY   ;Store in LSBY
           ldx     #$07
GET_NXT
			jsr     NXT_BCD  ;Get next BCD value
           jsr     MPY10
           dex
           bne     GET_NXT
           asl     EXP      ;Move dp nibble left
           asl     EXP
           asl     EXP
           asl     EXP
           lda     BINARY+3 ;Get MSBY and filter it
           and     #$0f
           ora     EXP      ;Pack dp
           sta     BINARY+3
		   
		   
.EXIT
;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX
           rts
		   
NXT_BCD
			ldy     #$04
           lda     #$00
MV_BITS
			asl     BCD
           rol     BCD+1
           rol     BCD+2
           rol     BCD+3
           rol     
           dey
           bne     MV_BITS
           rts

;Conversion subroutine for BCD_BIN
MPY10
			sta     CONVERT.BCD_TO_HEX.TEMP    ;Save digit just entered
           lda     BINARY+3 ;Save partial result on
           pha              ;stack
           lda     BINARY+2
           pha
           lda     BINARY+1
           pha
           lda     BINARY
           pha
           asl     BINARY   ;Multiply partial
           rol     BINARY+1 ;result by 2
           rol     BINARY+2
           rol     BINARY+3
           asl     BINARY   ;Multiply by 2 again
           rol     BINARY+1
           rol     BINARY+2
           rol     BINARY+3
           pla              ;Add original result
           adc     BINARY
           sta     BINARY
           pla
           adc     BINARY+1
           sta     BINARY+1
           pla
           adc     BINARY+2
           sta     BINARY+2
           pla
           adc     BINARY+3
           sta     BINARY+3
           asl     BINARY   ;Multiply result by 2
           rol     BINARY+1
           rol     BINARY+2
           rol     BINARY+3
           lda     CONVERT.BCD_TO_HEX.TEMP    ;Add digit just entered
           adc     BINARY
           sta     BINARY
           lda     #$00
           adc     BINARY+1
           sta     BINARY+1
           lda     #$00
           adc     BINARY+2
           sta     BINARY+2
           lda     #$00
           adc     BINARY+3
           sta     BINARY+3
		   rts
		   
;.EXIT: see above

		   
@END

;**OPT** Memory. Setup BDC+$0 in the ACC as return value for 8-bit use
;**opt** Memory. Can CONVERT.HEX.16_TO_BCD.24 be used in place of CONVERT.HEX.8_TO_BCD.16
CONVERT.HEX.8_TO_BCD.16 ;(packed)
@START
;PARAMETERS: ACC: hex number to convert
;RETURN: BCD (2)

;====SUMMARY====
;converts an 8-bit hex number to a 16-bit packed BCD number. 
;The # to convert is set via the ACC 
;If only 2 BCD digits are needed, then they will be in BCD+$0. 
;if 3 or 4 BCD digits are needed, then they will apper in BCD+$1
;
;EXPECTED RESULT
;
;XREG (BCD+$0) = !02
;YREG (BCD+$1) = !55
;

;====AUTHORS DESCRIPTION====
;http://6502.org/source/integers/hex2dec-more.htm
; Convert an 8 bit binary value to BCD
;
; This function converts an 8 bit binary value into a 16 bit BCD. It
; works by transferring one bit a time from the source and adding it
; into a BCD value that is being doubled on each iteration. As all the
; arithmetic is being done in BCD the result is a binary to decimal
; conversion.  All conversions take 311 clock cycles.
;
; For example the conversion of a $96 would look like this:
;
; BIN = $96 -> BIN' = $2C C = 1 | BCD $0000 x2 + C -> BCD' $0001
; BIN = $2C -> BIN' = $58 C = 0 | BCD $0001 x2 + C -> BCD' $0002
; BIN = $58 -> BIN' = $B0 C = 0 | BCD $0002 x2 + C -> BCD' $0004
; BIN = $B0 -> BIN' = $60 C = 1 | BCD $0004 x2 + C -> BCD' $0009
; BIN = $60 -> BIN' = $C0 C = 0 | BCD $0009 x2 + C -> BCD' $0018
; BIN = $C0 -> BIN' = $80 C = 1 | BCD $0018 x2 + C -> BCD' $0037
; BIN = $80 -> BIN' = $00 C = 1 | BCD $0037 x2 + C -> BCD' $0075
; BIN = $00 -> BIN' = $00 C = 0 | BCD $0075 x2 + C -> BCD' $0150
;
; This technique is very similar to Garth Wilson's, but does away with
; the look up table for powers of two and much simpler than the approach
; used by Lance Leventhal in his books (e.g. subtracting out 1000s, 100s,
; 10s and 1s).
;
; Andrew Jacobs, 28-Feb-2004

;;====TEMPLATE===
;		LDA #$FF ;8-bit hex # to convert
;	JSR CONVERT.HEX.8_TO_BCD.16
;

;SAVE PARAMETERS
	;ACC = parm: hex number to convert
	STA TEMP
	
;SAVE REGISTERS
	TXA
	PHA
	
	LDA TEMP ;restore parm: hex number to convert
	
.START
	;ACC = parm: hex number to convert
	STA	BIN
	SED		; Switch to decimal mode
	LDA #0		; Ensure the result is clear
	STA BCD+0
	STA BCD+1
	LDX #8		; The number of source bits

.CONVERT.LOOP		
	ASL BIN		; Shift bits in BIN to the left. bit7 is shifted into carry flag
	LDA BCD+0	; 
	ADC BCD+0	; add bit from carry flag, in decimal mode
	STA BCD+0
	LDA BCD+1	; propagating any carry
	ADC BCD+1
	STA BCD+1
	DEX		; And repeat for next bit
				
	BNE .CONVERT.LOOP
	CLD		; Back to binary

;RESTORE REGISTERS
	PLA
	TAX
	
	RTS
@END

CONVERT.HEX.16_TO_BCD.24	
@START
;PARAMETERS: BIN(2) 16-bithex number to convert
;RETURN: BCD(3)

;====SUMMARY====
;converts an 16-bit hex number to a 24-bit packed BCD number. 
;The # to convert is set via the BIN parameter. 
;If only 2 BCD digits are needed, then they will be in BCD+$0. 
;if 3 or 4 BCD digits are needed, then they will apper in BCD+$1
;if 5 BCD digits are needed, then #5 will apper in BCD+$2

;====AUTHORS DESCRIPTION====
;http://6502.org/source/integers/hex2dec-more.htm
; Convert an 16 bit binary value to BCD
;
; This function converts a 16 bit binary value into a 24 bit BCD. It
; works by transferring one bit a time from the source and adding it
; into a BCD value that is being doubled on each iteration. As all the
; arithmetic is being done in BCD the result is a binary to decimal
; conversion. All conversions take 915 clock cycles.
;
; See BINBCD8 for more details of its operation.
;
; By Andrew Jacobs, 28-Feb-2004
		
;;====TEMPLATE===
;		LDA #$FF ;16-bit hex # to convert (LO byte)
;		STA BIN+$0
;		LDA #$FF ;16-bit hex # to convert (HO byte)
;		STA BIN+$1
;	JSR CONVERT.HEX.16_TO_BCD.24
;



;SAVE REGISTERS
	TXA
	PHA
	
.START
		SED		; Switch to decimal mode
		LDA #0		; Ensure the result is clear
		STA BCD+0
		STA BCD+1
		STA BCD+2
		LDX #16		; The number of source bits

.CONVERT.LOOP	
		ASL BIN+0	; Shift out one bit
		ROL BIN+1
		LDA BCD+0	; And add into result
		ADC BCD+0
		STA BCD+0
		LDA BCD+1	; propagating any carry
		ADC BCD+1
		STA BCD+1
		LDA BCD+2	; ... thru whole result
		ADC BCD+2
		STA BCD+2
		DEX		; And repeat for next bit
		BNE .CONVERT.LOOP
		CLD		; Back to binary

		
		
.EXIT
;RESTORE REGISTERS
	PLA
	TAX
	
	
		RTS		; All Done.
@END

CONVERT.HEX_TO_ASCII ;also converts BCD to ASCII
@START
;PARAMETERS: ACC = 8-bit hex # to convert 
;RETURN: RESULT(2) ASCII code that represents the hex number contained in each nibble of the ACC parameter
		 ;RESULT+$0 = LO nibble
		 ;RESULT+$1 = HO nibble
		 
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
		; LDA #$1F
	; JSR CONVERT.HEX_TO_ASCII
;	
	; LDA #$AA
	; LDX RESULT+$1 ;contains ascii code for $1
	; LDY RESULT+$0 ;contains ascii code for $F
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
		;converting single digit ascii number to hex: mask out the HO nibble (AND $0F)
		;converting two digits ascii number to hex: mask out HO nibble for both digits. logical shift left the bits for the HO digit into the HO nibble, then mask in that vaue to the LO digit.
		;	let's say the number is $12. 1 is the HO digits and 2 is the LO digit. 

CONVERT.ASCII.UCASE ;converts ASCII code to upper case.
@START
;PARAMETERS: ACC = ASCII value
;ENTRANCE: direct
;RETURN: ACC = upper case (if a letter), original ACC parm value (if not a letter)

	;ACC = ascii value of keypress
	CMP #$E1			;is ASCII value less than the lower end of the lowercase letter range?
	BCC .NOT.A.LETTER	;if yes then don't convert value to uppercase as that will damage the value, or if the value is already uppercase then no modification to the value is needed.
	CMP #$FB			;is ASCII value greater than the upper end of the lowecase letter range?	
	BCS .NOT.A.LETTER	;if yes then don't convert value to uppercase as that will damage the value, or if the value is already uppercase then no modification to the value is needed.
	AND #$DF ;masks out the 5th bit because the binary value of #$DF has 1 in each bit position except for bit 5.
.NOT.A.LETTER
	
	;RETURN VALUE = ACC
	RTS
	
@END
@END


;CMP.16 ;==========COMPARE, 16-BIT  (BCD SUPPORT)=========
@START
; ;PARAMETERS: OP1(2), OP2(2),  [SED/CLD]*
; ;RETURN: ACC ($00 EQUAL, $01 NOT EQUAL)
; ;*SED: This routines was tested with decimal (BCD). To use
; ;	in BCD mode, use SED just before the JSR to call this routine, 
; ;	and CLD just after.

; ;WARNING: IF USING THIS FUNCTION WITH AN 8-BIT VALUE (BECAUSE THE LOOP MAY PRODUCE A 16-BIT VALUE), YOU
; ;MUST SET OP1+$1 AND OP2+$2 TO $00 BEFORE CALLING ADC.16. USUALLY BEST TO DO THIS JUST BEFORE THE LOOP 
; ;STARTS DURING INIT OF OTHER VARIABLES.

; ;==========================================================
; ;SOURCE CODE CREDIT
; ;"Using 6502 Assembly Language" by Randy Hyde
; ;==========================================================

; ; ;***DRIVER TEMPLATE	***
	; ; LDA #$10
	; ; STA OP1
	; ; LDA #$20
	; ; STA OP1+$1
	; ;
	; ; LDA #$10
	; ; STA OP2
	; ; LDA #$20
	; ; STA OP2+$1
	; ;	
	; ; JSR CMP.16	;RETURN: ACC ($00 EQUAL, $01 NOT EQUAL)

		
; ;compare values

		; LDA OP1
		; CMP OP2
		; BNE .NE
		; LDA OP1+$1
		; CMP OP2+$1
		; BNE .NE
		; JMP .EQUALS
; .NE					;the 16-bit values are not equal
		; LDA #$01
		; RTS 
; .EQUALS 
		; LDA #$00
		; RTS
@END

DIV.16.BCD ;=======DIVISION, 16-BIT (BCD SUPPORT)=========
@START
;DESCRIPTION: 16-BIT DIVIDEND, 16-BIT DIVISOR, 16-BIT QUOTIENT, 16-BIT REMAINDER
;PARAMETERS: DIVIDEND(2), DIVISOR(2)
;ENTRANCE: DIRECT
;RETURN:	RESULT(4)
;			result0 = QUOTIENT (lo byte)
;			result1 = QUOTIENT (ho byte)
;			result2 = REMAINDER (lo byte)
;			result3 = REMAINDER (ho byte)

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
;=================================================================================




; ;****DRIVER TEMPLATE***	
		;
		; LDA #$53
		; STA DIVIDEND+$0 ;number to be divided
		; LDA #$00
		; STA DIVIDEND+$1
		; LDA #$8
		; STA DIVISOR+$0 ;number to divide by
		; LDA #$00
		; STA DIVISOR+$1
	; JSR DIV.16.BCD
		; ;RETURN VALUE: result+$0 (quotient LO), result+$1 (quotient HO), result+$2 (remainder LO), result+$3 (remainder HO)
		; LDA RESULT+$0
		; LDA RESULT+$1
		; BRK

		
	SED			;automatically set decimal mode

	
;INIT VARIABLES
	LDA #$00		
	STA RESULT+$0
	STA RESULT+$1
	STA RESULT+$2
	STA RESULT+$3

.LOOP	
;SUBTRACT DIVISOR FROM DIVIDEND
			
.SUBTRACT.LO_BYTE
	LDA DIVIDEND+$0
	PHA ;push remaining dividend (lo byte) before the subtraction in case an overflow results. Then this stack value will be used for the remainder
	SEC
	SBC DIVISOR+$0
	STA DIVIDEND+$0		 ;the amount of the original dividend (LO byte) which hasn't been subtracted away
.SUBTRACT.HO_BYTE
	LDA DIVIDEND+$1
	PHA ;push remaining dividend (ho byte) before the subtraction in case an overflow results. Then this stack value will be used for the remainder
	SBC DIVISOR+$1	
	BCC .UNDERFLOW
	STA DIVIDEND+$1		 ;the amount of the original dividend (LO byte) which hasn't been subtracted away

.INCREMENT.QUOTIENT.VALUE
	LDA RESULT+$0 		;the quotient counter increases with each subtraction because the quotient is equal to the 
	CLC					 ;number of times the divisor can be subtracted from the dividend without the result being 
	ADC #$01			 ;less than zero.
	STA RESULT+$0
	LDA RESULT+$1
	ADC #$00
	STA RESULT+$1

.EXIT_TEST	
;did the subtraction completed without an overflow 
;(i.e. DIVIDEND+$0 and DIVIDENT+$1 = $00)

	PLA ;pop the saved dividend (ho) value off the stack in case the loop doesn't exit
	PLA ;pop the saved dividend (lo) value off the stack in case the loop doesn't exit
	
	LDA DIVIDEND+$0
    BNE .LOOP
	LDA DIVIDEND+$1
	BEQ .NO_REMAINDER
	JMP .LOOP
	
.UNDERFLOW
	PLA ;pull DIVIDEND+$1 (saved before the subtraction which overflowed)
	STA RESULT+$3	;SET REMAINDER  (ho byte)

	PLA ;DIVIDEND+$0 (saved before the subtraction which overflowed)
	STA RESULT+$2	;SET REMAINDER  (lo byte)    			

	;**FALLS THROUGH**
	
.NO_REMAINDER

	;**FALLS THROUGH**

.EXIT
	CLD						;clear decimal mode, return to binary mode
	RTS

	
@END

DIV.16 ;==========DIVISION, 16-BIT (**NO BCD SUPPORT**)=========
DIV.32 ;this routine supports 32-bit divisors
@START   
;DESCRIPTION: 32-BIT DIVIDEND, 16-BIT DIVISOR, QUOTIENT, REMAINDER
;PARAMETERS: DIVIDEND(2), DIVIDEND32(2), DIVISOR, DIV.16.PARM.MODE ($00 = 16-bit dividend mode | >=$01 = 32-bit dividend mode))
;ENTRANCE: DIRECT
;RETURN:	RESULT(4)
;			result0 = quotient, lo byt
;			result1 = quotient, hi byt
;			result2 = remainder, lo byt
;			result3 = remainder, hi byt
;
;==========================================================
;SOURCE CODE CREDIT
;http://6502.org/source/integers/ummodfix/ummodfix.htm
;By Garth Wilson (2002)
;==========================================================


;****DRIVER TEMPLATE****
		; LDA MOB.CANDIDATE.RMAP
		; STA DIVIDEND+$0			;number to be divided
		; LDA MOB.CANDIDATE.RMAP+$1
		; STA DIVIDEND+$1
		; LDA #OFFSET.UP
		; STA DIVISOR+$0			;number to divide by
		; LDA #$00
		; STA DIVISOR+$1
		;
		; ;REMOVE NEXT TWO STAs UNLESS 32-BIT MODE IS ENABLED
		; STA DIVIDEND32
		; STA DIVIDEND32+$1
	; JSR DIV.16				;(dividend/divisor)					
		; ;RETURN VALUE: result+$0 (quotient LO), result+$1 (quotient HO), result+$2 (remainder LO), result+$3 (remainder HO)
		;
		; LDA RESULT 					;QUOTIENT
		; STA MOB.CANDIDATE.MAP.Y			
		; LDA RESULT+$2					;REMAINDER
		; STA MOB.CANDIDATE.MAP.X		
	
;SAVE PARAMETERS
	TXA
	PHA
	TYA
	PHA
	
;INIT VARIABLES
	LDA #$00
	STA TEMP1
	STA CARRY

	;init 32-bit variables? ;**OPT** Memory. 32-bit mode just needs to skip the init of DIVIDEND32+$0, DIVIDEND32+$1. This could be done if the init 
							;of these variables was at the top of the routine and DIV.32 was an alt entrance just after the init and just before the save registers section
	LDA DIV.16.PARM.MODE ;($00 = 16-bit dividend mode | >=$01 = 32-bit dividend mode)	
	BNE .START ;branch if 32-bit mode
	;disabled 32-bit mode
	;(this saves bytes by eliminating the need to init these variables on each call when only a 16-bit dividend is needed)
	STA DIVIDEND32+$0
	STA DIVIDEND32+$1
		
.START
         SEC  ;**OPT** Memory. This section appears to be designed to detect a mismatch in parameters. Double check it wasn't part of the original source. If not, it is probably safe to remove it and the .OVRFLW routine
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
         JMP .FINAL_EXIT
.OVRFLW ;    
         LDA #$FF
         STA DIVIDEND32
         STA DIVIDEND32+$1
         STA DIVIDEND
         STA DIVIDEND+$1
		 
.FINAL_EXIT
	
	;reset mode to 16-bit dividend
	LDA #$00
	STA DIV.16.PARM.MODE ;($00 = 16-bit dividend mode | >=$01 = 32-bit dividend mode)	


;RESTORE PARAMETERS
	PLA
	TAY
	PLA
	TAX
	
    RTS 

@END
	 
;IGRE.16	;=========IS >= (BCD SUPPORT)==========
@START
; ;PARAMETERS: OP1(2), OP2(2), [SED/CLD]*
; ;RETURN: CARRY FLAG (SET >=, NOT SET !>=)
; ;*SED: This routines was tested with decimal (BCD). To use
; ;	in BCD mode, use SED just before the JSR to call this routine, 
; ;	and CLD just after.

; ;WARNING: IF USING THIS FUNCTION WITH AN 8-BIT VALUE (BECAUSE THE LOOP MAY PRODUCE A 16-BIT VALUE), YOU
; ;MUST SET OP1+$1 AND OP2+$2 TO $00 BEFORE CALLING ADC.16. USUALLY BEST TO DO THIS JUST BEFORE THE LOOP 
; ;STARTS DURING INIT OF OTHER VARIABLES.

; ;==========================================================
; ;SOURCE CODE CREDIT
; ;"Using 6502 Assembly Language" by Randy Hyde
; ;==========================================================


; ;****INLINE VERSION*****
	;
	;;Note: CLC/SEC: testing confirmed that neither are needed for 16-bit >+ test.
	; LDA COMBAT.STATS.DAMAGE.FINAL ;OP1
    ; CMP CHR_SHEET.PC_MOB.HP_LO ;OP2
    ; LDA #$00 ;OP1+1
    ; SBC CHR_SHEET.PC_MOB.HP_HO ;OP2+1
    ; BCS .TARGET.IS.KILLED  ;if carry set then OP1 >= OP2, which means the target is killed 

	
; ;****DRIVER TEMPLATE***
	; ; LDA #$10
	; ; STA OP1
	; ; LDA #$20
	; ; STA OP1+$1
	; ;
	; ; LDA #$10
	; ; STA OP2
	; ; LDA #$21
	; ; STA OP2+$1
	; ;
		; ; LDA #$01
		; ;
		; ; SED
	; ; JSR IGRE.16	;RETURN: CARRY FLAG (SET >=, NOT SET !>=)
		; ; CLD
		; ;
	; ; BCS .ISGRE
	; ; LDA #$00
	; ; BRK

; .ISGRE
	; LDA #$AA
	
	; BRK
	
; ;IS OP1 >= OP2?	
    ; LDA OP1
    ; CMP OP2
    ; LDA OP1+1
    ; SBC OP2+1
    ; BCS .IS.GRE   (BCS: is carry flag set)
    ; CLC
    ; RTS 
; .IS.GRE
    ; SEC
    ; RTS 
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
	STA $4F				;**OPT** Memory. Speed. May not be needed now that we are seeding $4E/$4F after boot. try removing this section and test in the MAME emulator. 
	STA SEED2
	
	LDA #$BD
	STA $4E				;**OPT** Memory. Speed. May not be needed now that we are seeding $4E/$4F after boot. try removing this section and test in the MAME emulator. 
	STA SEED3
	JMP .COMMON.INIT

.STANDARD.INIT	
	LDA $4F
		;LDA #$22
	STA SEED2
	;STA $B000
	
	LDA $4E
		;LDA #$FE
	STA SEED3
	;STA $B001

	;**FALLS THROUGH**

		; LDA $C082
		; LDA #$BB

		; LDX SEED2
		; LDY SEED3
		; LDX $B000
		; LDY $B000
		; BRK

.COMMON.INIT	
	LDA #$01
	STA RND.LO		;see note above before using a different return value. 
	LDA #$FF
	STA RND.HI
			
			
	LDA #$02
	STA MOD

		; LDA $C082
		; LDA #$BC
		; ;LDX $B000
		; LDX SEED2
		; LDY SEED3
		; BRK

		
	RTS
	
@END

MLP.16.NO_BCD	;16-BIT MULTIPLICATION (**NO BCD SUPPORT**, but faster than MLP.16)
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
	; JSR MLP.16.NO_BCD
		; ;RETURN VALUE: result+$0 (product LO), result+$1 (product HO)
		;
		; LDA RESULT+$0
		; LDA RESULT+$1
		;	 
	; BRK
		
		
;SAVE REGISTERS
        TYA 
        PHA 
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

MLP.16	;16-BIT(partial) MULTIPLICATION (HEX & BCD SUPPORT**, 16-BIT MULPLR, 8-BIT MULCND*)			
@START
;PARAMETERS: MULPLR(2), MULCND(2), [SED/CLD]**
;RETURN: RESULT(2)
;*to support 16-bit MULCND upgrade .EXIT.TEST to 16-bit
;**SED: This routines was tested with decimal (BCD). To use
;	in BCD mode, use SED just before the JSR to call this routine, 
;	and CLD just after.

;**OPT** Try setting SED before a call to MLP.16.NO_BCD and see if it works for BCD. If so then this routine can be eliminated. 

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
	; JSR MLP.16
		; RETURN VALUE: result+$0 (product LO), result+$1 (product HO)
		; LDA RESULT
		; LDA RESULT+$1
		

		
;INIT VARIABLES, COUNTERS
	LDA #$01						;counter is set to $01 because MULPLR.TALLY doesn't start at $00, it starts at the value of MULPLR
	STA MULCND.COUNTER
	
	LDA MULPLR+$0
	STA MULPLR.TALLY+$0
	
	LDA MULPLR+$1
	STA MULPLR.TALLY+$1
	
.LOOP


			
;16-BIT ADDITION
	LDA MULPLR.TALLY+$0
	CLC
	ADC MULPLR+$0
	STA MULPLR.TALLY+$0
	LDA MULPLR.TALLY+$1
	ADC MULPLR+$1 ;16-bit addition
	STA MULPLR.TALLY+$1

			

.EXIT.TEST			
	LDA MULCND.COUNTER		
	CLC
	ADC #$01				;increment loop counter
	STA MULCND.COUNTER
			
	LDA MULCND.COUNTER
	CMP MULCND+$0
	BNE .LOOP


			
;STORE RESULT OF MULTIPLICATION
	LDA MULPLR.TALLY+$0
	STA RESULT+$0
	LDA MULPLR.TALLY+$1
	STA RESULT+$1

		
.EXIT
	RTS
	
	
@END

;MLP.16 using powers of 2
@START
;See examples/6502/math.multiply-16(powers of 2).ASM
;to confirm an open question in my comments in this example: yes, there should
;be more ASLs than ROLs. Position the first ROL after the first ASL that will result in an overflow. And, 
;an ROL is needed after each ASL that will overflow. But don't put ROLs after ASLs that don't overflow or the 
;HO byte will get shifted and that will mess up the calculation. 

@END
				
MLP.32	;NO BCD (32-BIT MULR/MLCND 64-BIT PRODUCT)
@START

;SAVE REGISTERS
	TXA
	PHA
	
;===DRIVER TEMPLATE====
@START
		; LDA #$20
		; STA MULR+$0
		; LDA #$03
		; STA MULR+$1
		; LDA #$00
		; STA MULR+$2
		; LDA #$00
		; STA MULR+$3
; ;
		; LDA #$00
		; STA MULND+$0
		; LDA #$01
		; STA MULND+$1
		; LDA #$00
		; STA MULND+$2
		; LDA #$00
		; STA MULND+$3
	; JSR MLP.32
		; ;RETURN VALUE: PROD(8)

@END

		
MULTIPLY
	lda     #$00
    sta     PROD+4   ;Clear upper half of
    sta     PROD+5   ;product
    sta     PROD+6
    sta     PROD+7
    ldx     #$20     ;Set binary count to 32

SHIFT_R
	lsr     MULR+3   ;Shift multiplyer right
    ror     MULR+2
    ror     MULR+1
    ror     MULR
    bcc     ROTATE_R ;Go rotate right if c = 0
    lda     PROD+4   ;Get upper half of product
    clc              ; and add multiplicand to
    adc     MULND    ; it
    sta     PROD+4
    lda     PROD+5
    adc     MULND+1
    sta     PROD+5
    lda     PROD+6
    adc     MULND+2
    sta     PROD+6
    lda     PROD+7
    adc     MULND+3
ROTATE_R:  
	ror     a        ;Rotate partial product
	sta     PROD+7   ; right
    ror     PROD+6
	ror     PROD+5
	ror     PROD+4
	ror     PROD+3
	ror     PROD+2
	ror     PROD+1
	ror     PROD
	dex              ;Decrement bit count and
	bne     SHIFT_R  ; loop until 32 bits are
	clc              ; done
	lda     MULXP1   ;Add dps and put sum in MULXP2
	adc     MULXP2
	sta     MULXP2
	
		; lda #$aa
		; ldx #result
		; ldy /result
		; jsr prep.brk
		; brk

.EXIT

;RESTORE REGISTERS
	PLA
	TAX
	
	rts
	
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

;=====================SUBROUTINE DOCUMENTATION====================================
;
;Returns an 8-bit random # >= RND.LO and <= RND.HI
;
;Note: This routine is supposed to be 16-bit (see SOURCE CODE CREDIT for more info)
;
;I mentioned the problem to Peter Ferrie once and here was his response. 
;
; > Though for some reason it always returns $00 for the high order byte so I'm
; > only using it to generate 8-bit random numbers. I digress...
;
; That's weird.  I just tried it and it's working for me.  Maybe you've
; mislabeled one of the variables so it's overwriting another?
;
;=================================================================================


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
	

	
;====DRIVER TEMPLATE1=====(RANGE = 1-$FF) /w select case
;NOTE: the following are templates for the call to this subroutine
;and also 2-5 cases based on a $01-$FF random number returned.
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

;====DRIVER TEMPLATE2=====(RANGE = 1-$C7)	
; ;get random number !1-!199
		; LDA #$C7	;=!199
		; STA RND.HI
		; LDA #$01 
		; STA RND.LO
	; JSR RANDOM.8
		; ;ACC = random # (HEX)

	
;====DRIVER TEMPLATE3=====(BCD)	
; ;get random number !1-!199, then convert it to !1-!99
		; ;LDA #$64	;=!100
		; LDA #$C7	;=!200
		; STA RND.HI
		; LDA #$01 
		; STA RND.LO
	; JSR RANDOM.8
		; ;ACC = random # (HEX)
	; CMP #$64
	; BCC .LESS
	; SEC
	; SBC #$64
	; ;LSR ;/2
; .LESS	
	; JSR CONVERT.HEX.8_TO_BCD.16
	; LDA BCD+$0 ;load converted value	
	
;====RANDOM.8 TESTING=====
@START
;note: generates a page of random numbers and
;tallies how many fall into the range specified

		; LDX #$00
		; STX $B000
; .LOOP.RND
			; LDA #$02 ;set modulus
		; JSR RANDOM.8
		; STA $A000,X
		; CMP #$E6 ;set range
		; BCS .GRE.C0
		; JMP .INCREMENT
; .GRE.C0
		; INC $B000
; .INCREMENT
		; INX
		; BNE .LOOP.RND
		
		; LDA $C082
		; LDA #$AA
		; LDX $B000
		; BRK
@END		
	
;*ACC is used for the value of 'MOD' in the random number formula. My understanding from
;reading various comments by the author on the above web page is that MOD should be set
;to a power of 2. In my testing I used $02 and the results looked very random to me so
;that is the MOD set in RANDOM8.INIT.


;SAVE REGISTERS(1)
	;STA MOD    ; store modulus in MOD
	TXA 
	PHA
	TYA
	PHA	

.RANGE.CONTROLS	
;RANGE CONTROLS: used by calling routine to control the range of the returned random number 
	
	;verify that RND.HI is >= RND.LO
	LDA RND.HI
	CMP RND.LO
	BCS .INIT.COUNTER
	JMP .ERROR.RANGE
	
.INIT.COUNTER	
	; ;init counter
	; LDA #$00
	; STA RANDOM_NUMBER.ITERATION_TALLY
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


; ;CHECK FOR REPEATING STATIC RETURN VALUES	
	; INC RANDOM_NUMBER.ITERATION_TALLY

	; LDA RANDOM_NUMBER.ITERATION_TALLY
	; CMP #$FF
	; BEQ .ERROR
	

	LDA RND.HI
	CMP RND
	BEQ .EXIT_STEP
	BCC .CONTINUE
	
	LDA RND
	CMP RND.LO
	BCC .CONTINUE

.EXIT_STEP	
	JMP .EXIT
	
.ERROR.RANGE
;RND.HI ! >= RND.LO reported in .RANGE.CONTROLS (RANDOM.8)
	JSR PREP.BRK
	BRK
	
.ERROR
;RANDOM.8 REPORTS THAT IT'S RANDOM NUMBER GENERATOR COULDN'T MEET THE RANGE CRITERA
;AFTER $FF (!256) ATTEMPTS, WHICH IMPLIES THAT IT MAY HAVE STARTED RETURNING STATIC RESULTS.
;KICK IT IN THE HEAD WITH AN IRON BOOT. I KNOW THAT NEVER HAPPENS, BUT DO IT ANYWAY.  
;DISABLE.BS_RAM

	; ; PLA ;pop saved register
	; ; PLA ;pop saved register
	; ; PLA 
	; ; TAX
	; ; PLA
	; ; TAY
	; LDA #$EE
	; ; LDX RND.LO
	; ; LDY RND.HI
	JSR PREP.BRK
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

;SBC.16	;==========SUBTRACTION, 16-BIT (BCD SUPPORT)=========
@START
; ;PARAMETERS: OP1(2), OP2(2), [SED/CLD]*
; ;RETURN: RESULT(2)
; ;*SED: This routines was tested with decimal (BCD). To use
; ;	in BCD mode, use SED just before the JSR to call this routine, 
; ;	and CLD just after.

; ;WARNING: IF USING THIS FUNCTION WITH AN 8-BIT VALUE (BECAUSE THE LOOP MAY PRODUCE A 16-BIT VALUE), YOU
; ;MUST SET OP1+$1 AND OP2+$2 TO $00 BEFORE CALLING ADC.16. USUALLY BEST TO DO THIS JUST BEFORE THE LOOP 
; ;STARTS DURING INIT OF OTHER VARIABLES.

; ;==========================================================
; ;SOURCE CODE CREDIT
; ;"Using 6502 Assembly Language" by Randy Hyde
; ;==========================================================

; ;INIT VARIABLES
	; LDA #$00
	; STA RESULT
	; STA RESULT+$01


; ; DO THE MATH
	; CLD 
    ; SEC                           ;ALWAYS BEFORE SUBTRACTION
    ; LDA OP1
    ; SBC OP2
    ; STA RESULT
    ; LDA OP1+$1
    ; SBC OP2+$1
    ; STA RESULT+$1
    ; BCC .ERROR
    ; RTS
; .ERROR
	; ; LDA OP1
	; ; STA $9600
	; ; LDA OP1+$1
	; ; STA $9601
	; ; LDA OP2
	; ; STA $9602
	; ; LDA OP2+$1
	; ; STA $9603
	; JSR PREP.BRK
    ; BRK	
@END

PERCENT.GET ;full 16-bit support for low and high number parameters*
@START
;PARAMETERS: PERCENT.GET.PARM.LOW_NUMBER(2), PERCENT.GET.PARM.HIGH_NUMBER(2)
;ENTRANCE: direct
;RETURN: BCD(2) = 16-percentage (!0-!100)

;*this was originally a problem because lower#*100 would overflow a 16-bit quotient,
;if the low# was > $280. This was solved by adding MLP.32, and enabling 32-bit divident 
;support in DIV.16

;=====================SUBROUTINE DOCUMENTATION====================================
;
;(to avoid using floating decimals, this routine calculates the percent using the
;the formula: low_number * 100 / high_number = 23%, which yields the same result as 
;low_number / high_number using floating point decimal.
;
;So instead of 18.17/79, this routine would calculate 18.17*100/79
;
;=================================================================================


;Formula: RESULT(2) = low_number * 100 / high_number

		
;low_number * 100
		LDA PERCENT.GET.PARM.LOW_NUMBER+$0
		STA MULR+$0
		LDA PERCENT.GET.PARM.LOW_NUMBER+$1
		STA MULR+$1
		LDA #$00
		STA MULR+$2
		STA MULR+$3
		;					
		LDA #$64	;!100
		STA MULND+$0
		LDA #$00
		STA MULND+$1		
		STA MULND+$2
		STA MULND+$3
	JSR MLP.32 ;supports HEX only
		LDA RESULT+$0 ;load multiplication return value LO byte
		STA DIVIDEND+$0
		LDA RESULT+$1 ;load multiplication return value HO byte
		STA DIVIDEND+$1		
		LDA RESULT+$2
		STA DIVIDEND32+$0 ;+$2		
		LDA RESULT+$3
		STA DIVIDEND32+$1 ;+$3
		
;/ high_number
		LDA PERCENT.GET.PARM.HIGH_NUMBER+$0
		STA DIVISOR+$0
		LDA PERCENT.GET.PARM.HIGH_NUMBER+$1
		STA DIVISOR+$1
		LDA #$01
		STA DIV.16.PARM.MODE ;($00 = 16-bit dividend mode | >=$01 = 32-bit dividend mode)
	JSR DIV.16 ;hex division routine
		;RETURN VALUE: result+$0 (quotient LO), result+$1 (quotient HO), result+$2 (remainder LO), result+$3 (remainder HO)
				
		; lda #$aa
		; ldx #result
		; ldy /result
		; jsr prep.brk
		; brk			


;CONVERT RESULT TO BCD				
		LDA RESULT+$0	;16-bit hex # to convert (LO byte)
	JSR CONVERT.HEX.8_TO_BCD.16	
		;RETURN: BCD(8)
		
		; ;set subroutine return value in ACC
		; LDA BCD+$0	
.EXIT
				
				; LDA #$AA
				; LDX BCD+$0
				; LDY BCD+$1
			; JSR PREP.BRK
			; BRK
			
	RTS
	
@END	

;PERCENT.APPLY  ;**see notes below and COMBAT.STATS.APPLY.PERCENTAGE for full routine
@START
;Notes
;
;A subroutine actually isn't needed for this.
;All that are needed are MLP.16, and DIV.16 (either the BCD or non-BCD versions)
;
;The following examples use the proportion method. 
;(see http://amby.com/educate/math/4-2_prop.html)
;(another example they gave was for: 75% of what number is 3)
;
;Example: What is 23% of 79?
;79 * 23% = 79 * .23 = 18.17
;79 * 23 / 100 = 18.17 
;
;Using DIV.16 will result in a remainder value
;insead of the .17 but for many purposes, just ignoring
;the remainder is close enough. 
;
;Example: What is 18.17 is what percent of 79?
;18.17*100/79 = 23%
;
;
;
@END

;=================DEFINE VARIABLES===============

;**see "GENERAL ROUTINES (LOW LEVEL)" section in offloaded_variables.ASM
 
