;************************INCLUDE FILE*****************************
;(do not assemble stand-alone. For a stand-alone version of this routine see examples/rwts2.1)

;WARNING: DO NOT USE THIS AS AN INCLUDE FILE IN THE GAME. IT ALREADY HAS MY.RWTS LOADED AS PART OF
;THE BOOT PROCESS. 

MY.RWTS  
;PARAMTERS: DESIRED MODICATIONS TO IOB TABLE, ACC (toggles DOS override)*1
;REQUIRED: TRK.SRT, SEC.SRT, TRK.END, SEC.END, DEST.ADDR(2)
;*1: (default) $00 == RWTS will skip DOS tracks/sectors 
;	 		   $01 == RWTS will read reserved DOS Tracks/sectors ($T11 and $T12, sectors $E & $D) 


; ;=====DRIVER TEMPLATE (READ&WRITE)=====
; ;EXPECTED RESULT: execute the entire driver template as-is
;and $8000-$81FF should be filled with #$BA. 
; SETUP.FILL_DATA
;
; ;create test data to write
		; LDA #$00
		; STA FILL.START
		; LDA #$70
		; STA FILL.START+$1
		; LDA #$FF
		; STA FILL.END
		; LDA #$75
		; STA FILL.END+$1
		; ;
		; LDA #$BA
		; STA FILL.VALUE
		; ;
		; JSR MEMORY.FILL
; ;		
; ;
; DRIVER.TEMPLATE.WRITE
	; LDA #$02		;$01 = READ, $02 = WRITE
    ; STA CMD.CODE
	; LDA #$01
    ; STA DRIVE
	; ;	
    ; LDA #$0A
    ; STA TRK.SRT    
    ; LDA #$00	
    ; STA SEC.SRT
	; ;
	; LDA #$0A
	; STA TRK.END
    ; LDA #$01
    ; STA SEC.END
	; ;
    ; LDA #$00
    ; STA DEST.ADDR					;LO
	; ;											
    ; LDA #$70
    ; STA DEST.ADDR+$1               	;HO
	; ;
	; LDA #$00						;TURN OFF DOS TRACK/SECTOR OVERRIDE
	; JSR MY.RWTS
; ;	
; ;
; DRIVER.TEMPLATE.READ
	; LDA #$01		;$01 = READ, $02 = WRITE
    ; STA CMD.CODE
	; LDA #$01
    ; STA DRIVE
	; ;	
    ; LDA #$0A
    ; STA TRK.SRT    
    ; LDA #$00	
    ; STA SEC.SRT
	; ;
	; LDA #$0A
	; STA TRK.END
    ; LDA #$01
    ; STA SEC.END
	; ;
    ; LDA #$00
    ; STA DEST.ADDR					;LO
	; ;												
    ; LDA #$80
    ; STA DEST.ADDR+$1               	;HO
	; ;
	; LDA #$00						;TURN OFF DOS TRACK/SECTOR OVERRIDE
	; JSR MY.RWTS
	; BRK


;set DOS.OVERRIDE variables
	STA RWTS.DOS.OVERRIDE


;save registers
    PHA 
    TXA 
    PHA 
    TYA 
    PHA 	

.RWBUFF.POINTER 	
;SETUP POINTER TO READ/WRITE BUFFER
	LDA #RWBUFF
	STA IOB.TBL+$08
	LDA /RWBUFF
	STA IOB.TBL+$09
				
				
;setup zero page vector to RW.BUFF
	LDA IOB.TBL+$08 
	STA RW.BUFF.VECTOR 
	LDA IOB.TBL+$09
	STA RW.BUFF.VECTOR+$1
										
;
;
.RW.LOOP   ;=======================
;
;call RWTS and loop through tracks and sectors
;

.VALIDATE.INPUT 

;Error Code List
;
;look for the error code in each test below. The error code is LDA at the end of each test. 

			
.TEST1a 
;is track start >= $00
	LDA TRK.SRT
	CMP #$00
	BCS .TEST1b		;BCS = greater than or equal to 
	LDA #$A0		;ERROR CODE
	JMP ERROR.RWTS

.TEST1b 
;is track end <= $22
	LDA #$22
	CMP TRK.END
	BCS .TEST1c		;BCS = greater than or equal to 
	LDA #$A1		;ERROR CODE
	JMP ERROR.RWTS
.TEST1c 
;end track >= start track
	LDA TRK.END
	CMP TRK.SRT
	BCS .TEST2		;BCS = greater than or equal to 
	LDA #$A2		;ERROR CODE
	JMP ERROR.RWTS

.TEST2 
;is sector end <= $22
	LDA #$0F
	CMP SEC.END
	BCS .TEST3		;BCS = greater than or equal to 
	LDA #$A3		;ERROR CODE
	JMP ERROR.RWTS


.TEST3 
;verify command code == $01 or $02
	LDA #$A5					;ERROR CODE
	LDX CMD.CODE
	CPX #$01
	BEQ .TEST4a	
	CPX #$02
    BEQ *+5					;branch out of range substituion. If the BEQ condition is satisfied, the program pointer is set to the instruction following the JMP. If the condition is not satisfied then the JMP is executed. 
	JMP ERROR.RWTS

						
	
.TEST4a 
									
;verify destination address (read/write data location), is >= $400, < B700
	LDA DEST.ADDR+$1
	CMP #$04			
					
	BCS .TEST4b		;BCS = greater than or equal to 							
	LDA #$A6		;ERROR CODE
	JMP ERROR.RWTS
					
.TEST4b
.CHECK.BS.STATUS
	LDA $C012
	CMP #$80				;IS BANK SWITCHED RAM ENABLED?
	BCS .END.TESTS 			;IF YES, SKIP THIS TEST AS $FFFF IS THE MAXIUM ACCEPTABLE VALUE
	
	LDA DEST.ADDR+$1
	CMP #$B8
	BCC .END.TESTS	;BCC = less than  
	LDA #$A7		;ERROR CODE
	JMP ERROR.RWTS		
	
.END.TESTS 

;
;update IOB.TBL with next track, sector
    LDA TRK.SRT
    STA IOB.TBL+$4
    LDA SEC.SRT
    STA IOB.TBL+$5

;Swap contents of WRITE buffer with next page of data				
	LDY #$00
.WRITE.BUFFER.UPDATE		
	LDA #$02
	CMP CMD.CODE
	BNE .CALL.RWTS.WRAPPER
;update write buffer
    LDA (DEST.ADDR),Y
	STA (RW.BUFF.VECTOR),Y
;finished?
    INY 
    CPY #$00
    BNE .WRITE.BUFFER.UPDATE

.CALL.RWTS.WRAPPER 

	JSR RWTS.GO
	
;Swap contents of READ buffer with next page of data	
	LDY #$00
.READ.BUFFER.UPDATE		
	LDA #$01
	CMP CMD.CODE
	BNE .CHECK.FINISHED
;update read buffer	
	LDA (RW.BUFF.VECTOR),Y
    STA (DEST.ADDR),Y
;finished?
    INY 
    CPY #$00
    BNE .READ.BUFFER.UPDATE

.CHECK.FINISHED 	
;INCREMENT COUNTERS: SETUP R/W FOR NEXT SECTOR, EXIT IF DONE

	LDA SEC.SRT
    CMP SEC.END
	BNE .INC.TS.COUNTERS
	LDA TRK.SRT
    CMP TRK.END
    BEQ .EXIT
	

.INC.TS.COUNTERS 	
;SKIP DOS RESERVED TRACKS/SECTORS?
;Note: Applecommander skips track !17 ($11) and track !18, seconds 14 & 15 ($T12,$E & $D)
;I'm almost postive this is becasue AppleCommander is writing data to the disk assuming a DOS format.
;I'm almost postive these tracks/sectors are reserved by DOS. I recall reading somethig about it in Beneath DOS
;and specifically recall the reserve space being near the middle of the disk. It was for catalog information or something. 

	LDA RWTS.DOS.OVERRIDE				;IS DOS OVERRIDE SET
	CMP #$01
	BEQ .CONTINUE						;IF YES, THEN THEN CONTINUE
										;IF NO, THEN DO THE DOS TRACK/SECTOR CHECKS

;DOS TRACK/SECTOR CHECKS	
	LDA TRK.SRT
	CMP #$11
	BEQ .SKIP.DEST.ADDR
	CMP #$12
	BNE .CONTINUE
	LDA SEC.SRT
	CMP #$0E
	BEQ .SKIP.DEST.ADDR
	CMP #$0F
	BEQ .SKIP.DEST.ADDR
	;***FALLS THROUGH
	
.CONTINUE			
	INC DEST.ADDR+$1
.SKIP.DEST.ADDR
	INC SEC.SRT

	LDA SEC.SRT
    CMP #$10
    BEQ *+5					;branch out of range substituion. If the BEQ condition is satisfied, the program pointer is set to the instruction following the JMP. If the condition is not satisfied then the JMP is executed. 
	JMP .RW.LOOP
    INC TRK.SRT
    LDA #$00
    STA SEC.SRT
	JMP .RW.LOOP
	


	
;
.EXIT     
;restore registers and return
    PLA 
    TAY 
    PLA 
    TAX 
    PLA 
    RTS 
;
;
RWTS.GO  
;
;
;
;
;*****NOTE: get vector and init IOB table only needs to be done once per run.
;these can both be moved up to my.rwts to improve speed
;get vector of IOB table
    JSR IOB.VEC
    STY IOB                       ;lo
    STA IOB+$1                    ;ho
;
;init IOB table
    LDY #$00
	
.SET.TBL5  
    LDA IOB.TBL,Y
    STA (IOB),Y
    INY 
    CPY #$0E
    BNE .SET.TBL5
;
;call RWTS, r/w/f is determined by IOB.TBL
;
;first load iob table memory address into acc,y-reg to pass to rwts
    LDY IOB
    LDA IOB+$1
;call RTWS
    JSR RWTS
	
	LDA #$00
    STA $48                 		;always after RWTS call

;Check for Error	
	BCC NOERR.RWTS					;If Carry Flag is clear, there is no error (per poster to comp.sys.apple2.programmer, subject "RWTS Return Codes", 11/24/2015)
								


;ERROR OCCURED
;

;load error code from return value output by RWTS
;Check ACC to view error code
;
;Return Codes from (from Beneath DOS p. 6-7)
;$08 = error during init
;$10 = write protect error
;$20 = volume mismatch
;$40 = drive error
;$80 = read error (obsolete)
;
;other codes I've seen
;$01 = seems to be the return code whenever I write. a read of the same data seems valid
;$09 = occurs when track 0, sec 0 is read, though data appears valid
;$D3 = i saw this on copy II plus disk (likely protected). Data was read in even though it ouput this code
;whether the data was valid or garbage is unknown
;However, I'm proceeding on the understanding that these were not errors and that an error has only occured if
;the carry flag is set after calling RWTS. (per poster to comp.sys.apple2.programmer, subject "RWTS Return Codes", 11/24/2015)

	LDY #$0D
	LDA (IOB),Y
	STA ERR.CODE
	

ERROR.RWTS 						;**OPT** Unless memory is REALLY tight, don't put error routine in include file or identifying the meaning of the error codes will be difficult, and error code would then have to be unique between subroutines which use the common error routine.  
;Error code will be in ACC. For Error Code list, see .ERROR.RWTS, and .VALIDATE.INPUT
;Note: there are two entry points to this routine: falling through from .ERROR.RWTS 
;or via a jump from .VALIDATE.INPUT
;
;***HEY! MAKE SURE YOU NOTICE THAT THIS ROUTINE IS USED FOR INPUT VALIDATION ERRORS NOT
;JUST RWTS ERROR CODES!!!!!!!!!

.CHECK.BS.STATUS
	STA TEMP
	LDA $C012
	CMP #$80				;IS BANK SWITCHED RAM ENABLED?
	BCC .EXIT				;IF NO, GO RIGHT TO EXIT. 
	LDA $C082				;IF YES, DISABLE BANK-SWITCHED RAM AND RENABLE ROM ROUTINES. OTHERWISE THE PROGRAM CAN'T REPORT THE ERROR BY RETURNING TO THE APPLE MONITOR SINCE THE MONITOR IS A ROM ROUTINE. 
	
.EXIT	
	LDA TEMP
    LDX #$EE
    LDY #$EE
    BRK
	
NOERR.RWTS
	RTS
	
;======DEFINE VARIBLES======
IOB.TBL .HS 01.60.01.00.00.00.FB.B7.00.16.00.00.01.00	;BYT 0 - $0D
;byt map    00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D

;byt0: table type (always $01)
;byt1: slot * 16 (s6 = $60)
;byt2: drive ($01 or $02)
;byt3: Volume expected ($00=any)
;byt4: Track ($00-$FF)
;byt5: Sector ($00-$0F)
;byt6: lo addr of DCT tbl (use default)
;byt7: hi addr of DCT tbl (use default)
;byt8: lo addr of r/w buff (custom)
;byt9: hi addr of r/w buff (custom)
;byt$0A: not used (always $00)
;byt$0B: byt count for partial sector ($00 for 256 bytes)
;byt$0C: command code ($00=seek,$01=read,$02=write,$04=format)
;byt$0D: return code (set to $00 to clear)
	
;BYT $E,$f,$10 DON'T MODIFY. RWTS UPDATES THESE AFTER EACH CALL (MY OBSERVATION).

IOB.VEC  .EQ $03E3					;subroutine that identified DOS vector for IOB table
IOB      .EQ $EB     				;$2byt			;stores IOB table DOS vector
RWTS     .EQ $03D9					;entry point to RWTS subtroutine
;
;
;assign labels to IOB.TBL array
;
USE.SLOT  .EQ IOB.TBL+$01			;use these variables as parameteres for a particular MY.RWTS call
;SLOT  .EQ IOB.TBL+$01			;use these variables as parameteres for a particular MY.RWTS call
DRIVE    .EQ IOB.TBL+$02
VOLUME   .EQ IOB.TBL+$03
TRACK    .EQ IOB.TBL+$04
SECTOR   .EQ IOB.TBL+$05
;RW.BUFF  .EQ IOB.TBL+$08			;for manually setting the r/w buffer location as a paramter. to use uncomment this line and uncomment the routine ".RWBUFF.POINTER". rw.buff sets up a pointer in IOB.TBL to a 256 byt buffer where read data, and data to be written, is stored temporarily. Handled by the program. Do not set directly as a parameter
BYT.CNT  .EQ IOB.TBL+$0B
CMD.CODE .EQ IOB.TBL+$0C
;
TRK.END			.BS $1	
SEC.END			.BS $1 
TRK.SRT  		.BS $1
SEC.SRT  		.BS $1
RWBUFF			.BS $100
DEST.ADDR 		.EQ $ED				;$2 byt		;the starting address of the data to be read via RWTS or of the data to be written. Set as a parameter when calling MY.RWTS
RW.BUFF.VECTOR 	.EQ	$FA				;$2 BYT		;zero page vector used by the program to reference RW.BUFF using indirect indexed by Y mode
;

RWTS.DOS.OVERRIDE	.BS $1			;(DEFAULT) $00 = SKIP DOS RESERVED TRACKS/SECTORS. $01  READ ALL TRACKS/SECTORS 
ERR.CODE			.BS $1			;reserve $1 byte		;err.code is set, via indirect indexed by Y ref, to the master IOB array that iob.tbl points to ;correction: that the array iob points to.
