;************************INCLUDE FILE*****************************
;(do not assemble stand-alone. For a stand-alone version of this routine see examples/rwts2)


PLAY.SOUND.SLOW_PROGRESS ;=============	

	LDA #$21				;SET FREQUENCY
	STA HALFTIME			
	LDA	#$1D				;SET DURATION
	STA	LENGTH				
	JSR SOUND.MAKE			;MAKE THE SOUND AT THE SPECIFIE LENGTH AND FREQUENCY

	; LDA #$21				;SET FREQUENCY
	; STA HALFTIME	
	; LDA	#$1D				;SET DURATION
	; STA	LENGTH				
	;JSR SOUND.MAKE			;MAKE THE SOUND AT THE SPECIFIE LENGTH AND FREQUENCY
	
	RTS
	
PLAY.SOUND.DUMB_ASS ;=============	

	LDA #$21				;SET FREQUENCY
	STA HALFTIME			
	LDA	#$1D				;SET DURATION
	STA	LENGTH				
	JSR SOUND.MAKE			;MAKE THE SOUND AT THE SPECIFIE LENGTH AND FREQUENCY

	LDA #$F0				;SET FREQUENCY
	STA HALFTIME	
	LDA	#$1D				;SET DURATION
	STA	LENGTH				
	JSR SOUND.MAKE			;MAKE THE SOUND AT THE SPECIFIE LENGTH AND FREQUENCY
	
	RTS

	
	
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

BRK

;======INCLUDE FILES======

;none
				
;======DEFINE VARIBLES======


SPEAKER		.EQ		$C030	;SPEAKER SOFTSWITCH (toggles speaker on/off)
HALFTIME	.BS		$1		;!33 ;= (1/frequency)/(2*34)
LENGTH		.BS		$1		;!29 ;Duration in units of 34*255 usec