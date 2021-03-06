;************************INCLUDE FILE*****************************
;(do not assemble stand-alone. For a stand-alone version of this routine see examples/rwts2)

EVENT.MANAGER
@START

CHECK.SUN.STATUS

.CHECK.FOR.LOCATION.EXEMPTION
;DOES CURRENT LOCATION TYPE HAVE SUNRISE/SUNET?
	LDA PLAYER.MAP.LOCATION_TYPE
	CMP #LOCATION.TYPE.UNDERMAP			;Undermap locations are excempt; if true then exit
	BEQ .NEXT.EVENT


.CHECK.SUNRISE
	LDA TIME.CURRENT.HOUR				;check if the 24th game clock == the constants that designate the time that sunrise occurs
	CMP #EVENT.SUNRISE.HOUR
	BNE .CHECK.SUNSET
	LDA TIME.CURRENT.MINUTE
	CMP #EVENT.SUNRISE.MINUTE
	BNE .CHECK.SUNSET
	
	;ACTIVATE SUNRISE
	LDA #$00
	STA TIME.SUN.STATUS
	STA TIME.SUN.COUNTER
	STA TIME.SUN.SUB_COUNTER
	
	JMP .NEXT.EVENT
	
.CHECK.SUNSET
	LDA TIME.CURRENT.HOUR				;check if the 24th game clock == the constants that designate the time that sunset occurs
	CMP #EVENT.SUNSET.HOUR
	BNE .NEXT.EVENT
	LDA TIME.CURRENT.MINUTE
	CMP #EVENT.SUNSET.MINUTE
	BNE .NEXT.EVENT

	
	;ACTIVATE SUNSET
	LDA #$02
	STA TIME.SUN.STATUS	
	LDA #$04
	STA TIME.SUN.COUNTER
	LDA #$00
	STA TIME.SUN.SUB_COUNTER

.NEXT.EVENT
	;**FALLS THROUGH**

CHECK.NPC.SCHEDULER.INTERVAL
;.CHECK.FOR.LOCATION.EXEMPTION
	LDA PLAYER.MAP.LOCATION_TYPE		
	CMP #LOCATION.TYPE.BUILDING
	BEQ .START	
	JMP .NEXT.EVENT

.START	
	;INTERVAL = EVERY HOUR
	LDA TIME.CURRENT.MINUTE		
	CMP #$00
	BNE .NEXT.EVENT
	
	JSR NPC.SCHEDULER
	
.NEXT.EVENT
	;**FALLS THROUGH**
	
	
NEXT.EVENT ;REPLACE WITH NAME OF NEXT EVENT******
		
		
	RTS
	
	
@END


DETERMINE.SUNLIGHT.STATUS
@START
;=====================SUBROUTINE DOCUMENTATION====================================
;
;determine if sun is visible based on time of day and location type
;
;=================================================================================

	LDA PLAYER.MAP.LOCATION_TYPE
	CMP #LOCATION.TYPE.SURFACE
	BEQ	.SUNLIGHT.NORMAL
	CMP #LOCATION.TYPE.BUILDING
	BEQ .SUNLIGHT.NORMAL	
	CMP #LOCATION.TYPE.UNDERMAP
	BEQ .SUNLIGHT.BLOCKED

	
.ERROR
;UNEXPECTED LOCATION TYPE DETECTED IN DETERMINE.SUNLIGHT.STATUS
	LDA TEXT
	LDA $C082
	BRK

.SUNLIGHT.NORMAL
	JMP .TIME.DETERMINE.SUN.STATUS
	
.SUNLIGHT.BLOCKED
	LDA #$03							;sun status = night
	STA TIME.SUN.STATUS
	JMP .EXIT
	
.TIME.DETERMINE.SUN.STATUS
;PARAMETERS: TIME.CURRENT.HOUR, TIME.CURRENT.MINUTE
;RETURN: TIME.SUN.STATUS
;ENTRANCE: DIRECT


;IS IT DAY OR NIGHT?
.UPPER.TEST
	LDA TIME.CURRENT.HOUR				;check if current hour & minute is less than the hour at which sunset occurs
	CMP #EVENT.SUNSET.HOUR
	BCC	.LOWER.TEST
	CMP #EVENT.SUNSET.HOUR
	BEQ .IS.SUNSET.HOUR					
	JMP	.SET.NIGHT						;if current hour is not less than or equal to sunset hour, then it must be great. it is night
	
.IS.SUNSET.HOUR
	LDA TIME.CURRENT.MINUTE	
	CMP #EVENT.SUNSET.MINUTE			;if current hour = sunset hour, then the current minute must be compared to the sunset minute to determine if it is day or night. 
	BCC	.SET.DAY	
	JMP .SET.NIGHT
	
.LOWER.TEST	

			;TESTED;
	LDA TIME.CURRENT.HOUR				;check if current hour & minute is less than the hour at which sunrise occurs
	CMP #EVENT.SUNRISE.HOUR
	BCC	.SET.NIGHT						
	CMP #EVENT.SUNRISE.HOUR
	BEQ .IS.SUNRISE.HOUR					
	JMP	.SET.DAY						;if current hour is not less than or equal to sunrise hour, then it must be great. it is day
	
.IS.SUNRISE.HOUR
	LDA TIME.CURRENT.MINUTE	
	CMP #EVENT.SUNRISE.MINUTE			;if current hour = sunrise hour, then the current minute must be compared to the sunset minute to determine if it is day or night. 
	BCC	.SET.NIGHT	
	JMP .SET.DAY	

.SET.DAY
	LDA #$01							;sun status = day
	STA TIME.SUN.STATUS
	JMP .EXIT

.SET.NIGHT
	LDA #$03							;sun status = night
	STA TIME.SUN.STATUS
			; JSR PREP.BRK
			; LDA #$AA
			; BRK
	JMP .EXIT
	
.EXIT
	RTS
	
@END

TIME.DISPLAY ;=========DISPLAYS TIME ON VIDEO SCREEN======
@START
;PARAMETERS: NONE
;RETURN: NONE
;ENTRANCE: DIRECT


;PRINT "   "   (ERASE DISPLAY CLOCK)	
	LDA #$24
	STA HTAB	
	LDA #$4
	STA VTAB
	JSR	UPDATE.CHAR.POS

	JMP .TEXT1.PRINT

.TEXT1 .AZ -/    /			;ASCII text string
.TEXT1.PRINT
		LDA #.TEXT1 					
		STA STRING
		
		LDA /.TEXT1
		STA STRING+$1						
	JSR PRINT.STR

	
;PRINT $TIME.CURRENT.HOUR	
	LDA #$24
	STA HTAB	
	LDA #$4
	STA VTAB
	JSR	UPDATE.CHAR.POS
	
		LDA TIME.DISPLAY.MINUTE
		STA BCD
		LDA TIME.DISPLAY.HOUR
		STA BCD+$1
		
	JSR PRINT_FIXED.BCD_PACKED


	
;PRINT "AM" OR "PM"
	LDA #$24
	STA HTAB	
	LDA #$5
	STA VTAB
	JSR	UPDATE.CHAR.POS
	
	LDA TIME.DISPLAY.AM_PM
	BNE .IS.PM
;.IS.AM
	JMP .TEXT3.PRINT
.TEXT3 .AZ -/am/			;ASCII text string
.TEXT3.PRINT
		LDA #.TEXT3 					
		STA STRING
		
		LDA /.TEXT3
		STA STRING+$1

	JMP .PRINT.AM_PM

.IS.PM	
	JMP .TEXT4.PRINT		
.TEXT4 .AZ -/pm/			;ASCII text string
.TEXT4.PRINT
		LDA #.TEXT4					
		STA STRING
		
		LDA /.TEXT4
		STA STRING+$1						
		;**FALLS THROUGH**

.PRINT.AM_PM
	JSR PRINT.STR	

.EXIT	
	RTS
	
@END

	
;==============KEEP THESE ROUTINES IN THIS ORDER====== (1st one fall through to 2nd one)
TIME.UPDATE.MOVE ;========UPDATES THE 24-HR GAME CLOCK AFTER PLAYER MOVE=====
@START
;PARAMETERS: TIME.CURRENT.HOUR, TIME.CURRENT.MINUTE
;RETURN: TIME.CURRENT.HOUR, TIME.CURRENT.MINUTE
;ENTRANCE: DIRECT

	SED							;set decimal (BCD) mode

	INC TIME.MOVES.COUNTER		;increment time move counter to reflect current move
	
;CHECK MOVE COUNTER
	LDA TIME.MOVES.COUNTER
	CMP #TIME.MOVES.PER_MINUTE	;are we ready for a clock increment? 
	BEQ .INCREMENT.CLOCK		;if yes, continue
	JMP TIME.UPDATE.EXIT		;if no, exit

.INCREMENT.CLOCK ;(24hr)
	LDA #$00
	STA TIME.MOVES.COUNTER
	
	LDA TIME.CURRENT.MINUTE
	CLC
	ADC #$01
	CMP #$60
	BNE .UPDATE.CLOCK
	LDA TIME.CURRENT.HOUR
	CLC
	ADC #$01
	CMP #$24
	BNE .RESET.MINUTE.COUNTER
.RESET.HOUR.COUNTER
	LDA #$00
	;STA TIME.CURRENT.HOUR
	;**FALLS THROUGH**
.RESET.MINUTE.COUNTER
	STA TIME.CURRENT.HOUR				;saves value in ACC from prior section
	LDA #$00
	;**FALLS THROUGH**
.UPDATE.CLOCK
	STA TIME.CURRENT.MINUTE				;saves value in ACC from prior section	
	;**FALLS THROUGH**
	
@END
	
TIME.UPDATE.DISPLAY	;=====CONVERTS THE 24-HR GAME CLOCK TO A 12-HOUR DISPLAY CLOCK=====			
@START
;PARAMETERS: TIME.CURRENT.HOUR, TIME.CURRENT.MINUTE
;RETURN: TIME.DISPLAY.HOUR, TIME.DISPLAY.MINUTE, TIME.DISPLAY.AM_PM
;ENTRANCE: DIRECT, OR VIA TIME.UDATE.MOVE

;CONVERT TO 12-HOUR CLOCK

	SED							;set decimal (BCD) mode

	
	LDA TIME.CURRENT.HOUR
	CMP #$13
	BCC .SET.DISPLAY.CLOCK
	SEC
	SBC #$12
	;**FALLS THROUGH
.SET.DISPLAY.CLOCK
	STA TIME.DISPLAY.HOUR				;saves value in ACC from prior section
	LDA TIME.CURRENT.MINUTE
	STA TIME.DISPLAY.MINUTE

;FIX 12am
	LDA TIME.DISPLAY.HOUR
	BNE .SET.AM_PM
	LDA #$12
	STA TIME.DISPLAY.HOUR
.SET.AM_PM	
	LDA TIME.CURRENT.HOUR
	CMP #$12
	BCS .IS.PM
;.IS.AM
	LDA #$00
	STA TIME.DISPLAY.AM_PM
	JMP .EXIT
.IS.PM
	LDA #$01
	STA TIME.DISPLAY.AM_PM
	JMP .EXIT
	
.EXIT
	;**FALLS THROUGH**
	
TIME.UPDATE.EXIT
	CLD							;clear decimal (BCD) mode, return to binary mode
	RTS
@END
;==============KEEP THESE ROUTINES IN THIS ORDER======

