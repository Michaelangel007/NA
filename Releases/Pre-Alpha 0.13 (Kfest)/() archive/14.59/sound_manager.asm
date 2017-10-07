;************************INCLUDE FILE*****************************
;(do not assemble stand-alone.  )


PLAY.SOUND.SLOW_PROGRESS ;=============	
@START
	LDA #$21				;SET FREQUENCY
	STA HALFTIME			
	LDA	#$1D				;SET DURATION
	STA	LENGTH				
	JSR SOUND.MAKE			;MAKE THE SOUND AT THE SPECIFIE LENGTH AND FREQUENCY


	RTS
@END
	
PLAY.SOUND.DUMB_ASS ;=============	
@START
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
@END

PLAY.SOUND.SQUISH
@START

	LDA #$F0				;SET FREQUENCY
	STA HALFTIME	
	LDA	#$1D				;SET DURATION
	STA	LENGTH				
	JSR SOUND.MAKE
	
	RTS
	
@END
	

PLAY.SOUND ;play sound from data array
@START
;PARAMETERS: SOUND_DATA.POINTER(2)
;ENTRANCE: DIRECT, or via entrance routine which sets pointer
;RETURN: Apple II speaker sound

;NOTE: the sound data pointer points to a hex table which contains data pairs. 
;data pair datagram:
;byte0: frequency
;byte1: duration

;SAVE REGISTERS
	TYA
	PHA

	LDY #$00
.PLAY.SOUND.LOOP	
	LDA (SOUND_DATA.POINTER),Y
	STA HALFTIME	;SET FREQUENCY
	INY ;next byte in data pair
	LDA (SOUND_DATA.POINTER),Y
	STA LENGTH	;SET DURATION
	JSR SOUND.MAKE			;MAKE THE SOUND AT THE SPECIFIE LENGTH AND FREQUENCY

	INY ;next data pair	
	CPY SOUND_DATA.LENGTH
	BNE .PLAY.SOUND.LOOP
	
;RESTORE REGISTERS
	PLA
	TAY
	
	RTS
@END

PLAY.SOUND.REPEAT2 ;play sound from data array /w repeats
@START
;(two byte pair are played, then repeat qty is read from next byte, the two byte
;pairs are played for the number of times specified)

.PLAY.SOUND.NO_WAITS.REPEATS2 ;2 sound data pairs
	LDY #$00 ;init sound data index
.PLAY.SOUND.LOOP2.ENTRANCE1		
	LDX #$01 ;init counter to $01 so that a repeat qty of 1 will play the sound data pair once	
.PLAY.SOUND.LOOP2.ENTRANCE2
	;play 1st sound data pair
	LDA (SOUND_DATA.POINTER),Y
	STA HALFTIME	;SET FREQUENCY
	INY ;advance to pair0, byte $01: length
	LDA (SOUND_DATA.POINTER),Y
	STA LENGTH	;SET DURATION
	JSR SOUND.MAKE			;MAKE THE SOUND AT THE SPECIFIE LENGTH AND FREQUENCY

	INY ;advance to pair1, byte $02: frequency

	;play 2nd sound data pair 
	LDA (SOUND_DATA.POINTER),Y
	STA HALFTIME	;SET FREQUENCY
	INY ;advance to pair1, byte $03: length
	LDA (SOUND_DATA.POINTER),Y
	STA LENGTH	;SET DURATION
	JSR SOUND.MAKE			;MAKE THE SOUND AT THE SPECIFIE LENGTH AND FREQUENCY

	
	INY ;advance to byte $04: repeat qty
	;check for repeating pair
	LDA (SOUND_DATA.POINTER),Y
.REPEAT.LOOP2
	STA TEMP ;repeat qty
	CPX TEMP ;has loop repeated the specified number of times?
	BEQ .REPEAT.LOOP.DONE2
	INX ;increment repeat counter
	
	DEY ;back up to 1st byte in record since it's repeating
	DEY
	DEY
	DEY
	JMP .PLAY.SOUND.LOOP2.ENTRANCE2
.REPEAT.LOOP.DONE2	

.EXIT.TEST2	
	INY ;next data pair, starting with byte $0: frequency
	CPY SOUND_DATA.LENGTH
	BNE .PLAY.SOUND.LOOP2.ENTRANCE1
	
	RTS
@END

SOUND.MAKE	;========SINGLE NOTE==========
@START

.SAVE		;SAVE REGISTERES
	;PHA
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

@END

.RESTORE	;RESTORE REGISTERES
	PLA
	TAY
	PLA
	TAX
	;PLA
		
		
	RTS
	
SOUND_MANAGER.END.ADDRESS


;======INCLUDE FILES======

;none
				
;======DEFINE VARIBLES======

;**see "SOUND MANAGER" section in offloaded_variables.ASM
 
 
