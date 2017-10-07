;************************INCLUDE FILE*****************************
;(do not assemble stand-alone. For a stand-alone version of this routine see examples/rwts2)


;====DEMO FUNCTION====: Read/write of files via ProDOS driver,
;that allows ProDOS operating system to be \
;clobbered if only file access is needed. 
;
;Feature:
;Floppy support
;Hard Drive support
;Directory Support (theoretically subdirectories are supported but I didn't test that functionality)
;AUX and Main memory support
;
;
;Memory Support:
;The driver can read a file into aux or main memory
;and it can write data to a file located in aux
;or main memory.
;
;Since the driver resides in main bank switched memory ($D000 bank 1)
;it is able to access both aux and main memory.
;
;
;OTHER SPECIFICATIONS
;see documentation at top of pdriver.ASM for more details.
;
;*When writing a file with the driver, the file must already exist at its maxium size. Observe how dummy.file is generated from this program, and written to the disk image via go2p.bat
;*Hard Drive file writes must be done in 512byte chunks (the ProDOS block size). Thus, the number of pages specified for a write must be a multiople of 2 (i.e. 2, 4, 6 etc)
;*Floppy file writes must be done in 256byte (1page) increments, the same as RWTS
;*Only do JSR pdriver.init once. It does not need to be done again until the computer is rebooted. 
;			See pdriver.ASM documentation for the memory ranges used.

;=====DEMO INSTRUCTIONS=====
;
;EXPECTED RESULT:
;
;6 pages of data is written from main memory to 
;a dummy file and then read back in to 
;$7000-$75FF. The dummy file is a target
;file include of this program and is written to 
;the disk image with 6 pages of filler data.
;
;After the program runs $7000-$75FF should
;contain the fill value $BA
;
;
;====FLOPPY====
;gotest2.bat
;go2p.bat
;launch disk image
;bload pdriver
;brun test
;verify that the read/write worked by observing that #$BB is the value
;in $7000 - $75FF.
;
;====HARDDRIVE====
;gotest2.bat
;go2p.bat
;enabled harddrive support in AppleWIN
;place disk image in drive 2, and copy II plus 8.2 in drive 1
;boot harddrive
;PR#6 to boot copy II plus floppy
;copy pdriver, test, and dummy.file from floppy to harddrive (slot 7, drive1)
;boot harddrive again
;bload pdriver
;brun test
;verify that the read/write worked by observing that #$BB is the value
;in $7000 - $75FF.




PRODOS.IO
@START
;PARAMETERS: ACC (auxreq), reqcmd, ldrlo, ldrhi, namlo, namhi, sizehi, sizelo
;ENTRANCE: direct
;RETURN: status ($00 success, $01 file not found)

; TEMPLATE.RW_TEST	
	; LDA $C083				;enable BSR: bank2
	; LDA $C083
	;;
	; JSR DRIVER.WRITE
	; JSR DRIVER.READ
	;
	; LDA $C082				;enable ROM, disable BSR
; ;COPY DATA READ INTO AUX MEMORY BACK TO MAIN MEMORY
; ;(only needed if write was set to aux memory)
		; ; ;AUX MEMORY -> MAIN MEMORY 	
			; ; LDA #$00			;SET START ADDRESS
			; ; STA AUX_MOVE.START
			; ; LDA #$70		
			; ; STA AUX_MOVE.START+$1
			; ; ;
			; ; LDA #$FF			;SET END ADDRESS
			; ; STA AUX_MOVE.END
			; ; LDA #$75
			; ; STA AUX_MOVE.END+$1
			; ; ;
			; ; LDA #$00			;SET DESTINATION ADDRESS
			; ; STA AUX_MOVE.DEST
			; ; LDA #$80
			; ; STA AUX_MOVE.DEST+$1
			; ; CLC					;SET CARRY FLAG DESGINATD MOVE FROM AUX MEMORY -> MAIN
			; ; JSR AUX_MOVE
		;	;
	; BRK


; TEMPLATE.DRIVER.READ
;;
;;
; ;set command type (READ | WRITE)
	; lda #$1 ;([$1=read drive1, | $2=write drive1], [$81=read drive2 | $82=write drive2])
	; sta reqcmd
;;
; ;set destination memory address
	; lda #$00
	; sta ldrlo
	; lda #$70
	; sta ldrhi
;;	
; ;set filename to read from	
	; lda #file.to.read		;load LO address
	; sta namlo
	; lda /file.to.read		;load HO address
	; sta namhi
;;
		; LDA #$00	;PARM: $00=main, $01=aux
	; JSR PRODOS.IO
;;
;;	
	; RTS	

; TEMPLATE.DRIVER.WRITE
;;
; ;create test data to write
		; LDA #$00
		; STA FILL.START
		; LDA #$60
		; STA FILL.START+$1
		; LDA #$FF
		; STA FILL.END
		; LDA #$65
		; STA FILL.END+$1
		;;
		; LDA #$BA
		; STA FILL.VALUE
	;;	
		; JSR MEMORY.FILL
;;
	;;	
; ;LOAD FILL INTO AUX MEMORY (only needed if write is set to aux memory)
		; ; ;MAIN MEMORY -> AUX MEMORY -> 
			; ; LDA #$00			;SET START ADDRESS
			; ; STA AUX_MOVE.START
			; ; LDA #$60		
			; ; STA AUX_MOVE.START+$1
			; ; ;
			; ; LDA #$FF			;SET END ADDRESS
			; ; STA AUX_MOVE.END
			; ; LDA #$65
			; ; STA AUX_MOVE.END+$1
			; ; ;
			; ; LDA #$00			;SET DESTINATION ADDRESS
			; ; STA AUX_MOVE.DEST
			; ; LDA #$60
			; ; STA AUX_MOVE.DEST+$1
			; ; SEC
			; ; JSR AUX_MOVE
;
;		
; ;set command type (READ | WRITE)
	; lda #$2 ;([$1=read drive1, | $2=write drive1], [$81=read drive2 | $82=write drive2])
	; sta reqcmd
;
; ;set write data size (# of 512 byte blocks to write from memory)
	; lda #$00 ;always #$00
	; sta sizelo
;
	; lda #$06 ;number of pages to write (must be in multiples of 2. i.e. 2, 4, 6, 8 $0A, etc)
	; sta sizehi
;
;
; ;set write buffer address (source data)
	; lda #$00
	; sta ldrlo
	; lda #$60
	; sta ldrhi
;
;;
; ;set filename to write to	
	; lda #file.to.write		;load LO address
	; sta namlo
	; lda /file.to.write		;load HO address
	; sta namhi
;
; ;setup MAIN/AUX memory parameters
	; ; ;WRITE from aux memory
	; ; lda $c08b
	; ; lda $c08b
	; ; lda #$01 ;01=aux, 00=main
	; ; sta $f4 ;auxreq
;;
	; ; ;WRITE from main memory
	; ; lda $c08b
	; ; lda $c08b
	; ; lda #$00 ;01=aux, 00=main
	; ; sta $f4 ;auxreq
;
;	
; ; ;write file	
	; ; jsr $d000 ;opendir
	; ; lda $c082
	;;
		; LDA #$00	;PARM: $00=main, $01=aux 
	; JSR PRODOS.IO
	; RTS


	
;SETUP MAIN/AUX MEMORY PARAMETERS
	;ACC contains auxreq
	STA auxreq ; $00=main, $01=aux

;SAVE REGISTERS
	PHA
	TXA
	PHA
	TYA
	PHA
	
;INIT VARIABLES
	LDA #$00
	STA IO.ATTEMPTS
	;LDX #$00		;COUNTS IO ATTEMPTS
	
	LDA REQCMD
	STA REQCMD.SAVED
	
;GET MEMORY-STATUS UPON ENTRY
	LDA $C012		;ROM/BSR soft-switch flag (bit7 = 1: BSR, bit7=0 ROM)
	STA BSR.STATE
	LDA $C011		;BANK1/BANK2 soft-switch flag (bit7 = 1: BANK2, bit7=0 BANK1)
	STA BANK.STATE

	
;ENABLE BANK-SWITCHED RAM ($D000 Bank 1)
	LDA $c08b
	LDA $c08b

		
.READ_WRITE.FILE

	JSR $d000 ;opendir, the I/O routine

		
	LDA STATUS			;load opendir return code
	BEQ .IO.SUCCEEDED	;was file-not-found ($01) reported? If no, then treat IO as successful
	LDA IO.ATTEMPTS		;check whether both drives have been searched for the file requested
;	CPX IO.ATTEMPTS		;check whether both drives have been searched for the file requested
	BNE .INSERT.DISK	;if both drives have been checked (>=$01) then prompt player to insert disk
						;if not, try other drive 
		
	;determine which drive to try next

		
	LDA REQCMD.SAVED
	BMI .TRY.DRIVE1		;is bit7 = 1? Then drive2 was just tried, check drive1 next
;.TRY.DRIVE2
	ORA #$80			;effectively adds #$80 to the ACC by changing bit 7 to $1. After ORA the ACC will contain a 1 for each bit which had a value of 1 in either the ORA value or the value in the ACC before the ORA was executed. 
						;Since we know that the read/write was first attempted on drive1 we know the value in the ACC was either $1 (read) or $2= (write). Adding $80 resuts in the values needed for drive2, for whichever operation (read or write) was aready specified. i.e. ($81 = read drive2, $82 = write drive2)
	STA REQCMD			;update the drive in the read/write command parameter
	INC IO.ATTEMPTS

	JMP .READ_WRITE.FILE

	
.TRY.DRIVE1
	AND #$03		;effectively subtracts #$80 from the ACC by changing bit 7 to $0. After the logical "AND" the ACC will contain a 1 for each bit which had a value of 1 in both the AND value and the value in the ACC before the AND was executed. In this case, bit 0 was the only bit with a value of $1 in both.  
					;Since we know that the read/write was first attempted on drive2 we know the value in the ACC was either $81 (read) or $82= (write). Subtracting $80 resuts in the values needed for drive1, for whichever operation (read or write) was aready specified. i.e. ($1 = read drive1, $2 = write drive1)
	STA REQCMD		;update the drive in the read/write command parameter

	INC IO.ATTEMPTS

	JMP .READ_WRITE.FILE
	
.INSERT.DISK
	;<WRITE CODE FOR DISK PROMPT>. 
	;probably will be positioned on screen differently
	;if the prompt occurs during boot than if it occurs
	;during game play
	
	
			JSR PREP.BRK
		
;Print "NO FILE" to text screen		
			LDA #$CE
			JSR COUT.ADDRESS
			LDA #$CF
			JSR COUT.ADDRESS
			LDA #$A0
			JSR COUT.ADDRESS
			LDA #$C6
			JSR COUT.ADDRESS
			LDA #$C9
			JSR COUT.ADDRESS
			LDA #$CC
			JSR COUT.ADDRESS
			LDA #$C5
			JSR COUT.ADDRESS			
		
			BRK

	
.IO.SUCCEEDED
	
;RESTORE MEMORY-STATUS
	LDA BSR.STATE					;get BSR State (either ROM enabled or BSR enabled) before P.IO driver was called
	BMI .GET.PRIOR.BSR.BANK_STATE	;is bit7 = 1? If yes, BSR was enabled, find out which bank
	STA $C082						;if no, ROM was enabled, so re-enable ROM
	BPL .alldone					;BPL must be true if BMI was false
.GET.PRIOR.BSR.BANK_STATE
	LDA BANK.STATE					;get BSR Bank State (either Bank1 or Bank2 enabled) before P.IO driver was called
	BMI .BANK2						;is bit7=1? If yes, bank2 was enabled
	STA $C08B						;if no, Bank1 was enabled, so re-enable bank1
	STA $C08B
	BPL .ALLDONE
.BANK2
	STA $C083						;re-enable bank2
	STA $C083
.ALLDONE


;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX
	PLA

	RTS
@End


;======DEFINE VARIBLES======

;PDRIVER 
;These are variables used by NOXARCH.SYSTEM, a file
;Peter built and installed on the bootloader. 

status    = $f3         ;returns non-zero on error
auxreq	  = $f4			;$00 = main memory, $01 = aux memory
sizelo    = $f5         ;must set if writing
sizehi    = $f6         ;must set if writing
reqcmd    = $f8         ;used if enable_write=1, 1=read, 2=write
ldrlo     = $f9         ;used if override_adr=1
ldrhi     = $fa         ;used if override_adr=1
namlo     = $fb
namhi     = $fc

;PRODOS.IO			
BSR.STATE 		.BS $1
BANK.STATE 		.BS $1

IO.ATTEMPTS		.BS $1
REQCMD.SAVED 	.EQ SAVED.ACC.LOCAL

;======INCLUDE FILES======

;none
	

