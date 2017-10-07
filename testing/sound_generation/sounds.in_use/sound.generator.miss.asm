                .CR     6502            Use 6502 overlay
				.OR		$1000			**Always put before .TF directive and never use again in program				.TF     TEST.BIN,BIN
				.TF     TEST.BIN,BIN
				.EF		errors
				;.LI 	OFF				Switches the assembler machine code conversion listing off so errors are easier to see								
				.LF		C:\MY_CODE\LIST

;HALFTIME is the frequency. i.e. the time interval between turning the speaker on and turning it off 				
;LENGTH is the duration. i.e. the length of time that the cycle described in frequency is repeated. 
;		For example, ignoring proper units of measurement, 
;		if the frequency is 1, then the cycle would be: 
;						ON <wait 1> OFF. 
;		If the duration was 2 seconds, then the computer would repeat this cycle for 2 seconds.  

;NOTE: If you want to have a sound with the shortest possible 
;		duration, set LENGTH to $01. If LENGTH is set to $00, 
;		that will produce a note of the maximum duration. 

DRIVER
	JSR TEST0
;	JSR TEST1
	;JSR TEST2
	;JSR TEST3

		; lda #$aa
		; ldx #$aa
		; brk
		
.ENDLESS_LOOP	
	JMP .ENDLESS_LOOP


TEST0		;SOUND TEMPLATE
;PLAY SOUND
    LDX #coun1a
.decrement
    ;PLAY SOUND
    LDA #tone1a               ;SET FREQUENCY   
    STA HALFTIME           
    LDA    #dur2a                ;SET DURATION
    STA    LENGTH               
    JSR SOUND.MAKE            ;MAKE THE SOUND AT THE SPECIFIE LENGTH AND FREQUENCY
   
;PLAY SOUND
    LDA #$tone2a                ;SET FREQUENCY   
    STA HALFTIME           
    LDA    #dur1a                ;SET DURATION
    STA    LENGTH               
    JSR SOUND.MAKE            ;MAKE THE SOUND AT THE SPECIFIE LENGTH AND FREQUENCY
  DEX
  CPX #$03
  BNE .decrement
	

	
	RTS
	

tone1a .EQ $60
tone2a .EQ $4a

dur1a .EQ $1
dur2a .EQ $2	

coun1a .EQ $10	
	
 RTS
;------------------------------END	




; TEST1		;Dumb ass sound

	; LDA #$F0				;SET FREQUENCY
	; STA HALFTIME	
	; LDA	#$3A				;SET DURATION
	; STA	LENGTH				
	; JSR SOUND.MAKE			;MAKE THE SOUND AT THE SPECIFIE LENGTH AND FREQUENCY
	
	; LDA $CD
	; JSR $FDED
	
	; LDA #$FF
	; JSR WAIT
	; JSR WAIT 
	; JSR WAIT
	
	; LDA $CD					;display an M
	; JSR $FDED


	; LDA #$21				;SET FREQUENCY
	; STA HALFTIME			
	; LDA	#$1D				;SET DURATION
	; STA	LENGTH				
	; JSR SOUND.MAKE			;MAKE THE SOUND AT THE SPECIFIE LENGTH AND FREQUENCY

	; LDA $CD
	; JSR $FDED
	
	; LDA #$F0				;SET FREQUENCY
	; STA HALFTIME	
	; LDA	#$1D				;SET DURATION
	; STA	LENGTH				
	; JSR SOUND.MAKE			;MAKE THE SOUND AT THE SPECIFIE LENGTH AND FREQUENCY
	
	; LDA $CD
	; JSR $FDED
	
	; RTS

; TEST2

	; LDA #$70				;SET FREQUENCY
	; STA HALFTIME		
	; LDA	#$1D				;SET DURATION
	; STA	LENGTH	
	; JSR SOUND.MAKE			;MAKE THE SOUND AT THE SPECIFIE LENGTH AND FREQUENCY

	; LDA #$6A				;SET FREQUENCY
	; STA HALFTIME	
	; LDA	#$1D				;SET DURATION
	; STA	LENGTH	
	; JSR SOUND.MAKE			;MAKE THE SOUND AT THE SPECIFIE LENGTH AND FREQUENCY

	; LDA #$64				;SET FREQUENCY
	; STA HALFTIME	
	; LDA	#$1D				;SET DURATION
	; STA	LENGTH	
	; JSR SOUND.MAKE			;MAKE THE SOUND AT THE SPECIFIE LENGTH AND FREQUENCY

	; LDA #$5E				;SET FREQUENCY
	; STA HALFTIME	
	; LDA	#$1D				;SET DURATION
	; STA	LENGTH	
	; JSR SOUND.MAKE			;MAKE THE SOUND AT THE SPECIFIE LENGTH AND FREQUENCY

	; LDA #$59				;SET FREQUENCY
	; STA HALFTIME
	; LDA	#$1D				;SET DURATION
	; STA	LENGTH	
	; JSR SOUND.MAKE			;MAKE THE SOUND AT THE SPECIFIE LENGTH AND FREQUENCY

	; LDA #$54				;SET FREQUENCY
	; STA HALFTIME
	; LDA	#$1D				;SET DURATION
	; STA	LENGTH	
	; JSR SOUND.MAKE			;MAKE THE SOUND AT THE SPECIFIE LENGTH AND FREQUENCY

	; LDA #$50				;SET FREQUENCY
	; STA HALFTIME
	; LDA	#$1D				;SET DURATION
	; STA	LENGTH	
	; JSR SOUND.MAKE			;MAKE THE SOUND AT THE SPECIFIE LENGTH AND FREQUENCY

	; LDA #$4B				;SET FREQUENCY
	; STA HALFTIME
	; LDA	#$1D				;SET DURATION
	; STA	LENGTH	
	; JSR SOUND.MAKE			;MAKE THE SOUND AT THE SPECIFIE LENGTH AND FREQUENCY

	; LDA #$47				;SET FREQUENCY
	; STA HALFTIME
	; LDA	#$1D				;SET DURATION
	; STA	LENGTH	
	; JSR SOUND.MAKE			;MAKE THE SOUND AT THE SPECIFIE LENGTH AND FREQUENCY

	; LDA #$43				;SET FREQUENCY
	; STA HALFTIME
	; LDA	#$1D				;SET DURATION
	; STA	LENGTH	
	; JSR SOUND.MAKE			;MAKE THE SOUND AT THE SPECIFIE LENGTH AND FREQUENCY

	; LDA #$3F				;SET FREQUENCY
	; STA HALFTIME
	; LDA	#$1D				;SET DURATION
	; STA	LENGTH	
	; JSR SOUND.MAKE			;MAKE THE SOUND AT THE SPECIFIE LENGTH AND FREQUENCY

	; LDA #$3C				;SET FREQUENCY
	; STA HALFTIME
	; LDA	#$1D				;SET DURATION
	; STA	LENGTH	
	; JSR SOUND.MAKE			;MAKE THE SOUND AT THE SPECIFIE LENGTH AND FREQUENCY

	; LDA #$38				;SET FREQUENCY
	; STA HALFTIME
	; LDA	#$1D				;SET DURATION
	; STA	LENGTH	
	; JSR SOUND.MAKE			;MAKE THE SOUND AT THE SPECIFIE LENGTH AND FREQUENCY

	; LDA #$35				;SET FREQUENCY
	; STA HALFTIME
	; LDA	#$1D				;SET DURATION
	; STA	LENGTH	
	; JSR SOUND.MAKE			;MAKE THE SOUND AT THE SPECIFIE LENGTH AND FREQUENCY

	; LDA #$32				;SET FREQUENCY
	; STA HALFTIME
	; LDA	#$1D				;SET DURATION
	; STA	LENGTH	
	; JSR SOUND.MAKE			;MAKE THE SOUND AT THE SPECIFIE LENGTH AND FREQUENCY

	; LDA #$2F				;SET FREQUENCY
	; STA HALFTIME
	; LDA	#$1D				;SET DURATION
	; STA	LENGTH	
	; JSR SOUND.MAKE			;MAKE THE SOUND AT THE SPECIFIE LENGTH AND FREQUENCY

	; LDA #$2D				;SET FREQUENCY
	; STA HALFTIME
	; LDA	#$1D				;SET DURATION
	; STA	LENGTH	
	; JSR SOUND.MAKE			;MAKE THE SOUND AT THE SPECIFIE LENGTH AND FREQUENCY

	; LDA #$2D				;SET FREQUENCY
	; STA HALFTIME
	; LDA	#$1D				;SET DURATION
	; STA	LENGTH	
	; JSR SOUND.MAKE			;MAKE THE SOUND AT THE SPECIFIE LENGTH AND FREQUENCY

	; LDA #$2A				;SET FREQUENCY
	; STA HALFTIME
	; LDA	#$1D				;SET DURATION
	; STA	LENGTH	
	; JSR SOUND.MAKE			;MAKE THE SOUND AT THE SPECIFIE LENGTH AND FREQUENCY

	; LDA #$28				;SET FREQUENCY
	; STA HALFTIME
	; LDA	#$1D				;SET DURATION
	; STA	LENGTH	
	; JSR SOUND.MAKE			;MAKE THE SOUND AT THE SPECIFIE LENGTH AND FREQUENCY

	; LDA #$26				;SET FREQUENCY
	; STA HALFTIME
	; LDA	#$1D				;SET DURATION
	; STA	LENGTH	
	; JSR SOUND.MAKE			;MAKE THE SOUND AT THE SPECIFIE LENGTH AND FREQUENCY

	; LDA #$23				;SET FREQUENCY
	; STA HALFTIME
	; LDA	#$1D				;SET DURATION
	; STA	LENGTH	
	; JSR SOUND.MAKE			;MAKE THE SOUND AT THE SPECIFIE LENGTH AND FREQUENCY

	; LDA #$21				;SET FREQUENCY
	; STA HALFTIME
	; LDA	#$1D				;SET DURATION
	; STA	LENGTH	
	; JSR SOUND.MAKE			;MAKE THE SOUND AT THE SPECIFIE LENGTH AND FREQUENCY

	; LDA #$20				;SET FREQUENCY
	; STA HALFTIME
	; LDA	#$1D				;SET DURATION
	; STA	LENGTH	
	; JSR SOUND.MAKE			;MAKE THE SOUND AT THE SPECIFIE LENGTH AND FREQUENCY

	; LDA #$1E				;SET FREQUENCY
	; STA HALFTIME
	; LDA	#$1D				;SET DURATION
	; STA	LENGTH	
	; JSR SOUND.MAKE			;MAKE THE SOUND AT THE SPECIFIE LENGTH AND FREQUENCY

	; LDA #$1C				;SET FREQUENCY
	; STA HALFTIME
	; LDA	#$1D				;SET DURATION
	; STA	LENGTH	
	; JSR SOUND.MAKE			;MAKE THE SOUND AT THE SPECIFIE LENGTH AND FREQUENCY

	; RTS

	
; TEST3		;MASH
	
	; LDX #$00
; .LOOP
	; LDA MASH,X
	; STA HALFTIME
	; LDA MASH+$1,X
	; STA LENGTH
	; JSR SOUND.MAKE			;MAKE THE SOUND AT THE SPECIFIE LENGTH AND FREQUENCY
	; INX
	; CMP #$00				;FINISHED? ($00 IS TACKED ONTO THE END OF THE ARRAY TO INDICATED THE END HAS BEEN REACHED)
	; BNE .LOOP				;IF NO, READ NEXT LENGTH/FREQUENCY PAIR
	
	; RTS
	
	

SOUND.MAKE	;========SINGLE NOTE==========

.SAVE		;SAVE REGISTERES
	PHA
	TXA
	PHA
	TYA
	PHA
	
.NOTE
	LDY	#$100				;!255

.NOTE1
	LDX	HALFTIME			;X contains the length of the note
	LDA	SPEAKER				;Toggle the speaker
			
	JMP .STALL1
.STALL
	NOP						;These NOPs compenstate for
	NOP						;banches to NOTE1 from line 37
	NOP						;They ensure that the overall loop
	NOP						;times are the same so that they units
	NOP						;of "length" don't vary with the frequency
	NOP	
	NOP
.STALL1
	DEY						;Loop time is 34 cycles
	BNE	.STALL2				
	DEC	LENGTH				;Reduce this evry 34*255 cycles
	BEQ	.EXIT
	BNE	.STALL3
.STALL2
	NOP						;These NOPs compensate even
	NOP						;out the loop time when the code
	NOP						;in lines 27-29 is not executed
	NOP		
	NOP
.STALL3
	DEX						;Loop time is 34 cycles
	BNE	.STALL
	BEQ	.NOTE1
.EXIT

.RESTORE	;RESTORE REGISTERES
	PLA
	TAY
	PLA
	TAX
	PLA

	RTS
	
	
;SUPPORTING SUBROUTINES
WAIT.LOOP
;PARAMETERS: ACC (LOOP LENGTH IN SUPER-PAGES)
@START
.SET.PARAMETERS
	STA TEMP
	BEQ .EXIT
	
.SAVE.REGISTERS
	TXA
	PHA
	TYA
	PHA
	
	
	LDX #$00 ;init delay counter, lo byte
	LDY #$00 ;init delay counter, ho byte
.WAIT.LOOP1	
.KB_BUFF.CHECK2.DONE
	INX ;increment lo byte delay counter
	BNE .WAIT.LOOP1
	INY ;increment ho byte delay counter
	CPY TEMP ;arbitrary value tuned to the visually observed animation speed
	BNE .WAIT.LOOP1	

	
.EXIT

.RESTORE.REGISTERS
	PLA
	TAY
	PLA
	TAX
	
	LDA TEMP ;reset ACC to parameter value so loop can be called multiple times in a row without resting the parameter
	RTS
@END
	
;======INCLUDE FILES======

				;.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_general
	
				
				
				
;=================DEFINE VARIABLES===============

SPEAKER		.EQ		$C030	;SPEAKER SOFTSWITCH (toggles speaker on/off)
HALFTIME	.BS		$1		;!33 ;= (1/frequency)/(2*34)
LENGTH		.BS		$1		;!29 ;Duration in units of 34*255 usec

MASH		.HS		3F.1D.43.1D.3F.1D.43.1D.3F.1D.43.1D.4B.3A.43.1D.4B.1D.43.1D.4B.1D.43.1D.4B.1D.54.1D.43.1D.4B.1D.54.1D.4B.1D.54.1D.4B.1D.54.1D.59.1D.4B.1D.54.1D.59.1D.54.1D.59.1D.54.1D.4B.1D.43.3A.43.3A.38.1D.32.1D.38.1D.32.1D.38.1D.32.1D.38.3A.38.1D.32.1D.38.1D.32.1D.38.1D.32.1D.38.3A.38.1D.43.1D.38.1D.32.1D.2A.1D.26.1D.2A.1D.32.1D.38.1D.32.73.32.3A.32.1D.38.1D.43.1D.38.1D.32.1D.2A.1D.26.1D.2A.1D.32.1D.38.1D.32.73.32.3A.00.00

TEMP 		.BS 	$1


;MASH		.HS		3F.1D.43.1D.3F.1D.43.1D.3F.1D.43.1D.4B.3A.43.1D.4B.1D.43.1D.4B.1D.43.1D.4B.1D.54.1D.43.1D.4B.1D.54.1D.4B.1D.54.1D.4B.1D.54.1D.59.1D.4B.1D.54.1D.59.1D.54.1D.59.1D.54.1D.4B.1D.43.3A.43.3A.38.1D.32.1D.38.1D.32.1D.38.1D.32.1D.38.3A.38.1D.32.1D.38.1D.32.1D.38.1D.32
;1D.38.3A.38.1D.43.1D.38.1D.32.1D.2A.1D.26.1D.2A.1D.32.1D.38.1D.32.73.32.3A.32.1D.38.1D.43.1D.38.1D.32.1D.2A.1D.26.1D.2A.1D.32.1D.38.1D.32.73.32.3A.0.0


;ORIGINAL VALUES
;HALFTIME	.EQ		$21		#CONSTANT		;!33 ;= (1/frequency)/(2*34)
;LENGTH		.EQ		$1D		#CONSTANT		;!29 ;Duration in units of 34*255 usec
;LTEMP		.BS		$1