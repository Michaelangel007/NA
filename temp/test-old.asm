                .CR     6502            Use 6502 overlay
				.OR		$6000			**Always put before .TF directive and never use again in program
				.TF     TEST.BIN,BIN
				.EF		errors

;demo RWTS. pretty much ready to be used as a callable subroutine.
;
;
;I tested read/write, using track $15, sector $0. 
;observations:
;I get a lots of strange return codes even though the data appears to be read/written correctly.
;it is very possible that my error routine should only trigger for the return codes
;I tried r/w after setting BYT.CNT, which byt $0B of the IOB table refers to as being for partial sectors
;the result was it read a full page (256 bytes), ($1600-$16FF, $1700 verified untouched)
;I'm not sure what a partial sectoris, but I'm guessing it is the last secort on the disk or something
;that Beneath DOS describes as errors
;
;
;parameteres
;an IOB table is setup by default with the values stored in IOB.TBL
;any of those values can be changed (DRIVE, TRACK, SECTOR, etc) via the labels setup to do so (see end of code listing)
;
;
;CALL MY SUBRT
;(trace) reg values to test stack push/pull
	
	LDA #$FF
    LDX #$FF
    LDY #$FF
;
;set parameters
;


BEGIN   NOP 
;
    LDA #$01
    STA CMD.CODE
	LDA #$01
    STA DRIVE
		
    LDA #$0C
    STA TRK.SRT    
	LDA #$0C
	STA TRK.END

    LDA #$00
    STA SEC.SRT
    LDA #$00
    STA SEC.END

;
    LDA #$00
    STA DEST.ADDR					;LO
    LDA #$71
    STA DEST.ADDR+$1               	;HO
	
    LDA #$00
    STA RW.BUFF						;LO
    LDA #$70
    STA RW.BUFF+$1               	;HO	

;
;
;call my subroutine
;
    JSR MY.RWTS
    BRK 
;
MY.RWTS  NOP 
;
;save registers
    PHA 
    TXA 
    PHA 
    TYA 
    PHA 
;
;setup read/write counters

    INC SEC.END

;update IOB.TBL with r/w buffer address
	;MIGHT NOT NEED THIS
	
;setup zero page vector to RW.BUFF
	LDA #RW.BUFF 
	STA RW.BUFF.VECTOR 
	LDA /RW.BUFF
	STA RW.BUFF.VECTOR+$1
;
;
RWLOOP5  NOP 
;
;call RWTS and loop through tracks and sectors
;
;
;update IOB.TBL with next track, sector
    LDA TRK.SRT
    STA IOB.TBL+$4
    LDA SEC.SRT
    STA IOB.TBL+$5

;Swap contents of r/w buffer with next page of data				
	LDY #$00
.BUFFER.UPDATE	NOP	
	LDA #$02
	CMP CMD.CODE
	BNE .READ.MODE
;WRITE.MODE
    LDA (DEST.ADDR),Y
	STA (RW.BUFF.VECTOR),Y
.READ.MODE	NOP	
	LDA (RW.BUFF.VECTOR),Y
    STA (DEST.ADDR),Y
.FINISH	
    INY 
    CPY #$00
    BNE .BUFFER.UPDATE

;Call my RWTS wrapper 	
	JSR RWTS.GO5
	
;INCREMENT COUNTERS: SETUP R/W FOR NEXT SECTOR, EXIT IF DONE
    INC DEST.ADDR+$1
	INC SEC.SRT
    LDA SEC.SRT
    CMP #$00
    BNE NOFLIP5
    INC TRK.SRT
    LDA #$00
    STA SEC.SRT
	
NOFLIP5  NOP 
    LDA SEC.SRT
    CMP SEC.END
    BNE RWLOOP5
    LDA TRK.SRT
    CMP TRK.END
    BNE RWLOOP5
;
EXIT5    NOP 
;rest error code in IOB table
    LDY #$0D
    LDA #$00		;**OP** is this line needed? 
    STA (IOB),Y
    STA ERR.CODE
;restore registers and return
    PLA 
    TAY 
    PLA 
    TAX 
    PLA 
    RTS 
;
;
RWTS.GO5 NOP 
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
	
SET.TBL5 NOP 
    LDA IOB.TBL,Y
    STA (IOB),Y
    INY 
    CPY #$0E
    BNE SET.TBL5
;
;call RWTS, r/w/f is determined by IOB.TBL
;
;first load iob table memory address into acc,y-reg to pass to rwts
    LDY IOB
    LDA IOB+$1
;call RTWS
    JSR RWTS
    LDA #$00
    STA $48                       ;always after RWTS call
;check for error
    LDY #$0D
    LDA (IOB),Y
    STA ERR.CODE
    CMP #$00
    BEQ NOERR5
	
ERROR5   NOP 
;check ACC to view return code
;
;I'm beginning to think that not all return codes are errors and some should not trigger this error routine
;error code list (from Beneath DOS p. 6-7)
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
;
;end jibber jabber. back to the code
;confirm if error is on the Beneath DOS list. If so, error out. If not, continue
    CMP #$08
    BEQ ECONF
    CMP #$10
    BEQ ECONF
    CMP #$20
    BEQ ECONF
    CMP #$40
    BEQ ECONF
    CMP #$80
    BEQ ECONF
    JMP NOERR5
ECONF    NOP 
;error is confirmed
    LDX #$EE
    LDY #$EE
    BRK 
NOERR5   NOP 
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
RW.BUFF  .EQ IOB.TBL+$08			;rw.buff is a 256 byt buffer where read data, and data to be written, is stored temporarily. Handled by the program. Do not set directly as a parameter
BYT.CNT  .EQ IOB.TBL+$0B
CMD.CODE .EQ IOB.TBL+$0C
;
TRK.END							;$1 byt		;""
SEC.END			.EQ TRK.END+$1  	;$1 byt
TRK.SRT  		.EQ SEC.END+$1		;$1 byt
SEC.SRT  		.EQ TRK.SRT+$1		;$1 byt
DEST.ADDR 		.EQ $ED				;$2 byt		;the starting address of the data to be read via RWTS or of the data to be written. Set as a parameter when calling MY.RWTS
RW.BUFF.VECTOR 	.EQ	$EF				;$2 BYT		;zero page vector used by the program to reference RW.BUFF using indirect indexed by Y mode
;


ERR.CODE	.EQ	DEST.ADDR+$2		;reserve $1 byte		;err.code is set, via indirect indexed by Y ref, to the master IOB array that iob.tbl points to ;correction: that the array iob points to.

