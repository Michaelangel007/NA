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
			
CHOOSE.SOUND
	;JSR TEST0
	
	;JSR DEMO.PLAY.MULTIPLE_SOUNDS
	
	;JSR DEMO.PLAY.SOUND_ARRAY1
	
	;JSR DEMO.PLAY.SOUND_ARRAY2
	
	JSR DEMO.ORIGINAL.METHOD

.EXIT	
	JSR KEYIN  ;wait for keypress
	LDA #$AA	;load $AA into ACC registers so that it's obvious the break occured at this location in the code
	BRK


DEMO.ORIGINAL.METHOD		;SOUND TEMPLATE
@START


;PRINT CHARACTER TO SCREEN THAT DENOTES START OF PROGRAM 
	LDA $CD
	JSR $FDED


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
@END



DEMO.PLAY.MULTIPLE_SOUNDS ;plays multiple sounds which already exist
@START

	JSR DEMO.PLAY.SOUND_ARRAY2
	
	INC TROUBLESHOOTING.HOOK

		LDA #$FF
	JSR WAIT.LOOP

		LDA #$FF
	JSR WAIT.LOOP

		LDA #$FF
	JSR WAIT.LOOP

		LDA #$FF
	JSR WAIT.LOOP

		LDA #$FF
	JSR WAIT.LOOP

		LDA #$FF
	JSR WAIT.LOOP

		LDA #$FF
	JSR WAIT.LOOP

		LDA #$FF
	JSR WAIT.LOOP
	
	JSR DEMO.PLAY.SOUND_ARRAY2


			
	RTS
@END
	

DEMO.PLAY.SOUND_ARRAY1 ;uses frequency/duration array, no waits. 
@START
;Description: plays the temporary combat attack sound


;PRINT CHARACTER TO SCREEN THAT DENOTES START OF PROGRAM 
	LDA $CD
	JSR $FDED
	
	
		;SET PARAMETERS
		
		;connect sound_data pointer to the sound array you want to play
		LDA #SOUND_DATA.COMBAT.ATTACK_HIT
		STA SOUND_DATA.POINTER+$0
		LDA /SOUND_DATA.COMBAT.ATTACK_HIT
		STA SOUND_DATA.POINTER+$1
		
		LDA #$1A
		STA SOUND_DATA.LENGTH ;the number of bytes in the sound array
		LDA #$00 ;parameter: mode ($00 = freq/duration data table only | $01 also use wait data table)
	JSR PLAY.SOUND
	
	RTS
	
;HEX ARRAYS	
SOUND_DATA.COMBAT.ATTACK_HIT .HS 06.02.20.02.28.02.3C.02.50.02.64.02.02.02.8C.02.A0.02.B4.02.C8.02.DC.02.F0.02


@END
	
	
DEMO.PLAY.SOUND_ARRAY2 ;uses frequency/duration array, with inserted wait points	
@START	
;description: plays dumb ass sound twice, with a wait inbetween
		
;PRINT CHARACTER TO SCREEN THAT DENOTES START OF PROGRAM 
	LDA $CD
	JSR $FDED
	
	
		;SET PARAMETERS
		
		;connect sound_data pointer to the sound array you want to play
		LDA #SOUND_DATA.DEMO2
		STA SOUND_DATA.POINTER+$0
		LDA /SOUND_DATA.DEMO2
		STA SOUND_DATA.POINTER+$1
		
		;connect sound_data pointer to the sound array you want to play
		LDA #SOUND_DATA.WAIT.DEMO2
		STA SOUND_DATA.WAIT.POINTER+$0
		LDA /SOUND_DATA.WAIT.DEMO2
		STA SOUND_DATA.WAIT.POINTER+$1
	
		
		
		
		LDA #$08 ;the number of bytes in the sound array. This is a quantity so don't start with 0. For example, .HS AA.FF. Length = 2 bytes. However byte0 = $AA and byte1 = $FF
		STA SOUND_DATA.LENGTH
		LDA #$02 ;the number of bytes in the sound wait array. This is a quantity so don't start with 0. For example, .HS AA.FF. Length = 2 bytes. However byte0 = $AA and byte1 = $FF	
		STA SOUND_DATA.WAIT_LENGTH		
		LDA #$01 ;parameter: mode ($00 = freq/duration data table only | $01 also use wait data table)
	JSR PLAY.SOUND
	
	RTS
	
;HEX ARRAYS
SOUND_DATA.DEMO2 		.HS 21.1D.F0.1D.21.1D.F0.1D
SOUND_DATA.WAIT.DEMO2	.HS 04.FF

@END


	
;------------------------------END	

SOUND.SUBROUTINES
@START

PLAY.SOUND ;play sound from data table
@START
;PARAMETERS: ACC = mode ($00 = freq/duration data table only | $01 also use wait data table), SOUND_DATA.POINTER(2), SOUND_DATA.WAIT.POINTER
;ENTRANCE: DIRECT, or via entrance routine which sets pointer
;RETURN: Apple II speaker sound

;NOTE: the sound data pointer points to a hex array which contains data pairs. 
;
;data pair array datagram:
;byte0: frequency
;byte1: duration
;
;wait array datagram:
;byte0: wait point (a wait is inserted after the byte # specified. Byte 0 is the first byte)
;byte1: wait length ($00 - $FF)


			

;SAVE PARAMETERS
	;ACC = parameter: mode ($00 = freq/duration data table only | $01 also use wait data table)
	STA PLAY.SOUND.MODE

;SAVE REGISTERS
	TYA
	PHA

;PARSE PARAMETERS	
	LDA PLAY.SOUND.MODE ;($00 = freq/duration data table only | $01 also use wait data table)
	BNE .PLAY.SOUND.WITH_WAITS
		
	;**FALLS THROUGH**
	
.PLAY.SOUND.NO_WAITS
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
	JMP .EXIT

;---------
	
.PLAY.SOUND.WITH_WAITS	
	LDY #$00 ;index to SOUND_DATA.POINTER and SOUND_DATA.WAIT.POINTER
	STY	SOUND_DATA.INDEX ;**OPT** memory. try removing
	STY SOUND_DATA.WAIT.INDEX
	
.PLAY.SOUND.WITH_WAITS.LOOP	
	;LDY SOUND_DATA.INDEX ;restore index to SOUND_DATA.POINTER
	LDA (SOUND_DATA.POINTER),Y
	STA HALFTIME	;SET FREQUENCY
	INY ;next byte in data pair
	LDA (SOUND_DATA.POINTER),Y
	STA LENGTH	;SET DURATION
	INY ;next data pair	
	STY SOUND_DATA.INDEX ;save index to SOUND_DATA.POINTER

	JSR SOUND.MAKE			;MAKE THE SOUND AT THE SPECIFIE LENGTH AND FREQUENCY

	;check for wait point
	LDY SOUND_DATA.WAIT.INDEX ;restore index to SOUND_DATA.WAIT.POINTER
	CPY SOUND_DATA.WAIT_LENGTH ;have all wait points been processed?
	BCS .EXIT_TEST ;if yes then generate the sound, skip checking the wait array.
	LDA (SOUND_DATA.WAIT.POINTER),Y
	CMP SOUND_DATA.INDEX
	BNE .EXIT_TEST
	
	;**FALLS THROUGH**
	
.INSERT.WAIT
	INY ;next byte in wait data pair

		;execute wait
		;(note: the LDA before each call shouldn't be needed but oddly removing them after the 1st one affects the sound)
		LDA (SOUND_DATA.WAIT.POINTER),Y ;load the wait duration						
	JSR WAIT.LOOP
		LDA (SOUND_DATA.WAIT.POINTER),Y ;load the wait duration				
	JSR WAIT.LOOP
		LDA (SOUND_DATA.WAIT.POINTER),Y ;load the wait duration				
	JSR WAIT.LOOP
		LDA (SOUND_DATA.WAIT.POINTER),Y ;load the wait duration				
	JSR WAIT.LOOP

.INCREMENT.WAIT.INDEX.ENTRANCE2
	INY ;next wait data pair
	STY SOUND_DATA.WAIT.INDEX ;save index to SOUND_DATA.POINTER
	
	;**FALLS THROUGH**
	

.EXIT_TEST
	LDY SOUND_DATA.INDEX ;restore index to SOUND_DATA.POINTER
	CPY SOUND_DATA.LENGTH
	BNE .PLAY.SOUND.WITH_WAITS.LOOP
	
	;**FALLS THROUGH**

.EXIT	
;RESTORE REGISTERS
	PLA
	TAY
	
	RTS
@END


SOUND.MAKE	;========SINGLE NOTE==========
@START

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
	
@END
	
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

	

.RESTORE.REGISTERS
	PLA
	TAY
	PLA
	TAX


.EXIT			
	LDA TEMP ;reset ACC to parameter value so loop can be called multiple times in a row without resting the parameter

			
	RTS
@END
	

KEYIN ;============WAITS FOR A KEYPRESS, NO ANIMATION=======
@START
;PARAMETERS: NONE
;RETURN VALUE: ACC (LAST KEY PRESS)
;ENTRACE: DIRECT

.KEYIN
	LDA $C000
    BPL .KEYIN
    STA $C010              ;CLR LAST KEY
	RTS
@END

@END


;======INCLUDE FILES======

				;.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_general
	
				
				
				
;=================DEFINE VARIABLES===============

SPEAKER		.EQ		$C030	;SPEAKER SOFTSWITCH (toggles speaker on/off)
HALFTIME	.BS		$1		;!33 ;= (1/frequency)/(2*34)
LENGTH		.BS		$1		;!29 ;Duration in units of 34*255 usec

MASH		.HS		3F.1D.43.1D.3F.1D.43.1D.3F.1D.43.1D.4B.3A.43.1D.4B.1D.43.1D.4B.1D.43.1D.4B.1D.54.1D.43.1D.4B.1D.54.1D.4B.1D.54.1D.4B.1D.54.1D.59.1D.4B.1D.54.1D.59.1D.54.1D.59.1D.54.1D.4B.1D.43.3A.43.3A.38.1D.32.1D.38.1D.32.1D.38.1D.32.1D.38.3A.38.1D.32.1D.38.1D.32.1D.38.1D.32.1D.38.3A.38.1D.43.1D.38.1D.32.1D.2A.1D.26.1D.2A.1D.32.1D.38.1D.32.73.32.3A.32.1D.38.1D.43.1D.38.1D.32.1D.2A.1D.26.1D.2A.1D.32.1D.38.1D.32.73.32.3A.00.00

TEMP 		.BS 	$1
TEMP16		.BS		$1
TROUBLESHOOTING.HOOK	.BS $1

SOUND_DATA.POINTER		.EQ $C6 ;#POINTER. Used by PLAY.SOUND. Points to the hex array containing the frequency and duration values for the sound to play. 
SOUND_DATA.WAIT.POINTER	.EQ $C8 ;#POINTER. Used by PLAY.SOUND. Points to the hex array containing the wait points and wait durations for the array connected to SOUND_DATA.POINTER
SOUND_DATA.LENGTH		.EQ $200 ;$1byte
SOUND_DATA.WAIT_LENGTH	.EQ $201 ;$1byte

;SOUND.NEXT.WAIT_POINT	.EQ $202 ;$1byte
SOUND_DATA.INDEX		.EQ $203 ;$1byte
SOUND_DATA.WAIT.INDEX	.EQ $204 ;$1byte
PLAY.SOUND.MODE			.EQ $205 ;$1byte


;MASH		.HS		3F.1D.43.1D.3F.1D.43.1D.3F.1D.43.1D.4B.3A.43.1D.4B.1D.43.1D.4B.1D.43.1D.4B.1D.54.1D.43.1D.4B.1D.54.1D.4B.1D.54.1D.4B.1D.54.1D.59.1D.4B.1D.54.1D.59.1D.54.1D.59.1D.54.1D.4B.1D.43.3A.43.3A.38.1D.32.1D.38.1D.32.1D.38.1D.32.1D.38.3A.38.1D.32.1D.38.1D.32.1D.38.1D.32
;1D.38.3A.38.1D.43.1D.38.1D.32.1D.2A.1D.26.1D.2A.1D.32.1D.38.1D.32.73.32.3A.32.1D.38.1D.43.1D.38.1D.32.1D.2A.1D.26.1D.2A.1D.32.1D.38.1D.32.73.32.3A.0.0


;ORIGINAL VALUES
;HALFTIME	.EQ		$21		#CONSTANT		;!33 ;= (1/frequency)/(2*34)
;LENGTH		.EQ		$1D		#CONSTANT		;!29 ;Duration in units of 34*255 usec
;LTEMP		.BS		$1