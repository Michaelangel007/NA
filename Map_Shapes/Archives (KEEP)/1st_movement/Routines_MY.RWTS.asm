;************************INCLUDE FILE*****************************
;(do not assemble stand-alone. For a stand-alone version of this routine see examples/rwts2)

								
				
;=====PROGRAM======
;
;FUNCTION: provde an interface to RWTS that can read/write multiple tracks & sectors with robust input validation and RWTS return code error checking. 
;VERSION: 1.0
;
;===BEFORE USING====
;consider the location of DEST.ADDR, which points to the area 
;in memory where information is read/written to/from. Another key variable is RWBUFF, but it is defined relative to others so it 
;should be okay.
;
;===TESTING===
;I tested read/write succesfully on all tracks & sectors (track $00-$22 and sectors $00-$0F on each track)
;I tested various scenarios that are handled differently in RWLOOP5, such as multiple tracks, single sector, multiple sectors, tracks/sector combinations where the start/end sector isn't the same, start/end on the last sector ($0F)
;
;====OBSERVATIONS===
;ERROR CODES:
;	I get a lots of strange return codes even though the data appears to be read/written correctly.
;	it is very possible that my error routine should only trigger for the return codes
;	I tried r/w after setting BYT.CNT, which byt $0B of the IOB table refers to as being for partial sectors
;	the result was it read a full page (256 bytes), ($1600-$16FF, $1700 verified untouched)
;	I'm not sure what a partial sectoris, but I'm guessing it is the last secort on the disk or something
;	that Beneath DOS describes as errors
;DOS OVERLAP WARNING!!
;   reading data above $9600 makes strange things happen, such as the program hanging or an error code of $08 being returned.	
;	I figued that wouldn't matter as long as I stayed at the monitor prompt but I think once DOS is loaded it changes things around
;   in a way that bad stuff happens if DOS get's clobbered in memory. The behavior could be duplicated in a simple program that just writes $AA
;   to as many pages as specified (i.e. it hangs in a simular patter)
;	I verified this theory but booting up my boot loader (work in progress, so far just breaks out after boot1 ends) and trying the same
;   simple page data writing program and I was able to write the value $AA from $6000 to $B4FF with no trouble. I did this boot loader test
;   on my physical Apple IIe and in an emulator. 
;
;	NEXT TEST: once I get RWTS onto my bootloader, try reading data above $9600 and see if it works. I bet it will.  
;
;====USEFUL DISK INFORMATION
;
;RWTS
; RWTS is loaded by boot1 from Track 0, Sector 1-9 and stored in B700-B7FF
; 
; Track 0, sector 0A, 0B are empty (cbecked on bootloader disk)
; Track 0, sector 0C - 0F have information, but I doubt it is important if I'm not loading DOS and taking over the disk
;
;MASTER IOB TABLE ($B7E8) CROSS-REF
;the following are the memory addresses corresponding with each byte description in the IOB table. Very useful if you are 
;looking at the byte description list from Beneath DOS while looking at $B7E8 and trying to see what values are associated 
;with specific bytes.
;
;
;Byt0	$B7E8	Table
;Byt1	   E9	Slot
;Byt2	   EA	Drive
;Byt3	   EB	Volume 
;Byt4	   EC	Track
;Byt5	   ED	Sector
;Byt6	   EE	(lo) pointer to DCT
;Byt7	   EF	(hi) pointer to DCT
;Byt8	   F0	(lo) pointer to R/W Buffer
;Byt9	   F1	(hi) pointer to R/W Buffer
;BytA	   F2	NOT USED
;BytB	   F3	Byte count for Sector Read
;BytC	   F4	Command Code: Read
;BytD	   F5	Output return code
;BytE	   F6	Volume # of last access
;BytF	   F7	Slot # of last access
;Byt10	   F8	Drive # of last access

;=====PARAMETERS=====
;
;INPUTS
;an IOB table is setup by default with the values stored in IOB.TBL
;any of those values can be changed (DRIVE, TRACK, SECTOR, etc) via the labels setup to do so (see end of code listing)
;
;MACHINE CODE HACKS====
;If you want to modify the parameters after the program has been loaded into $6000, here are the memory addresses associated with
;some frequently used parameters:
;
;$6008 = CMD.CODE
;$600D = DRIVE
;$6017 = TRK.END
;$601C = SEC.SRT
;$6021 = SEC.END
;$6026 = DEST.ADDR LO
;$602D = DEST.ADDR HO
;
;CALL MY SUBRT
;(trace) reg values to test stack push/pull





MY.RWTS  NOP ;=====================================================================================
;
;save registers
    PHA 
    TXA 
    PHA 
    TYA 
    PHA 	

.RWBUFF.POINTER NOP	
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
.RW.LOOP  NOP ;=====
;
;call RWTS and loop through tracks and sectors
;

.VALIDATE.INPUT NOP

;Error Code List
;
;look for the error code in each test below. The error code is LDA at the end of each test. 

			
.TEST1a NOP
;is track start >= $00
	LDA TRK.SRT
	CMP #$00
	BCS .TEST1b		;BCS = greater than or equal to 
	LDA #$A0		;ERROR CODE
	JMP ERROR.RWTS

.TEST1b NOP
;is track end <= $22
	LDA #$22
	CMP TRK.END
	BCS .TEST1c		;BCS = greater than or equal to 
	LDA #$A1		;ERROR CODE
	JMP ERROR.RWTS
.TEST1c NOP
;end track >= start track
	LDA TRK.END
	CMP TRK.SRT
	BCS .TEST2		;BCS = greater than or equal to 
	LDA #$A2		;ERROR CODE
	JMP ERROR.RWTS

.TEST2 NOP
;is sector end <= $22
	LDA #$22
	CMP SEC.END
	BCS .TEST3		;BCS = greater than or equal to 
	LDA #$A3		;ERROR CODE
	JMP ERROR.RWTS


.TEST3 NOP
;verify command code == $01 or $02
	LDA #$A5					;ERROR CODE
	LDX CMD.CODE
	CPX #$01
	BEQ .TEST4a	
	CPX #$02
    BEQ *+5					;branch out of range substituion. If the BEQ condition is satisfied, the program pointer is set to the instruction following the JMP. If the condition is not satisfied then the JMP is executed. 
	JMP ERROR.RWTS

						
	
.TEST4a NOP
									
;verify destination address (read/write data location), is >= $400, < B600
	LDA DEST.ADDR+$1
	CMP #$04			
					
	BCS .TEST4b		;BCS = greater than or equal to 							
	LDA #$A6		;ERROR CODE
	JMP ERROR.RWTS
					
.TEST4b
					
	LDA DEST.ADDR+$1
	CMP #$B7
	BCC .END.TESTS	;BCC = less than  
	LDA #$A7		;ERROR CODE
	JMP ERROR.RWTS	
	
.END.TESTS NOP

;
;update IOB.TBL with next track, sector
    LDA TRK.SRT
    STA IOB.TBL+$4
    LDA SEC.SRT
    STA IOB.TBL+$5

;Swap contents of WRITE buffer with next page of data				
	LDY #$00
.WRITE.BUFFER.UPDATE	NOP	
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

.CALL.RWTS.WRAPPER NOP

	JSR RWTS.GO
	
;Swap contents of READ buffer with next page of data	
	LDY #$00
.READ.BUFFER.UPDATE	NOP	
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

.CHECK.FINISHED NOP	
;INCREMENT COUNTERS: SETUP R/W FOR NEXT SECTOR, EXIT IF DONE

	LDA SEC.SRT
    CMP SEC.END
	BNE .INC.TS.COUNTERS
	LDA TRK.SRT
    CMP TRK.END
    BEQ .EXIT
	

.INC.TS.COUNTERS NOP	

	INC DEST.ADDR+$1
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
.EXIT    NOP 
;restore registers and return
    PLA 
    TAY 
    PLA 
    TAX 
    PLA 
    RTS 
;
;
RWTS.GO NOP ;==============================
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
	
.SET.TBL5 NOP 
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

;=====================================================MISC SUBROUTINES==============
	

ERROR.RWTS NOP						;**OPT** Unless memory is REALLY tight, don't put error routine in include file or identifying the meaning of the error codes will be difficult, and error code would then have to be unique between subroutines which use the common error routine.  
;Error code will be in ACC. For Error Code list, see .ERROR.RWTS, and .VALIDATE.INPUT
;Note: there are two entry points to this routine: falling through from .ERROR.RWTS or via a jump from .VALIDATE.INPUT
    LDX #$EE
    LDY #$EE
    BRK
	
NOERR.RWTS NOP
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
IOB      .EQ $EB     ;$2byt			;stores IOB table DOS vector
RWTS     .EQ $03D9					;entry point to RWTS subtroutine
;
;
;assign labels to IOB.TBL array
;
SLOT     .EQ IOB.TBL+$01			;use these variables as parameteres for a particular MY.RWTS call
DRIVE    .EQ IOB.TBL+$02
VOLUME   .EQ IOB.TBL+$03
TRACK    .EQ IOB.TBL+$04
SECTOR   .EQ IOB.TBL+$05
;RW.BUFF  .EQ IOB.TBL+$08			;for manually setting the r/w buffer location as a paramter. to use uncomment this line and uncomment the routine ".RWBUFF.POINTER". rw.buff sets up a pointer in IOB.TBL to a 256 byt buffer where read data, and data to be written, is stored temporarily. Handled by the program. Do not set directly as a parameter
BYT.CNT  .EQ IOB.TBL+$0B
CMD.CODE .EQ IOB.TBL+$0C
;
TRK.END							;$1 byt		;""
SEC.END			.EQ TRK.END+$1  	;$1 byt
TRK.SRT  		.EQ SEC.END+$1		;$1 byt
SEC.SRT  		.EQ TRK.SRT+$1		;$1 byt
RWBUFF			.EQ	SEC.SRT+$1		;$256 byt
DEST.ADDR 		.EQ $ED				;$2 byt		;the starting address of the data to be read via RWTS or of the data to be written. Set as a parameter when calling MY.RWTS
RW.BUFF.VECTOR 	.EQ	$FA				;$2 BYT		;zero page vector used by the program to reference RW.BUFF using indirect indexed by Y mode
;


ERR.CODE	.EQ	DEST.ADDR+$2		;reserve $1 byte		;err.code is set, via indirect indexed by Y ref, to the master IOB array that iob.tbl points to ;correction: that the array iob points to.
