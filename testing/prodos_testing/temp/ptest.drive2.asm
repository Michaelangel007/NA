				.CR     6502            Use 6502 overlay
				.OR		$1000			**Always put before .TF directive and never use again in program
				.TF     TEST.BIN,BIN
				.EF		errors
				.LF		C:\MY_CODE\LIST
;				.LI 	OFF				Switches the assembler machine code conversion listing off so errors are easier to see				
;				.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_common



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

	
DRIVER
;init prodos driver	
	;JSR PDRIVER.INIT
	

	
	LDA $C083				;enable BSR: bank2
	LDA $C083
	
	JSR DRIVER.WRITE


	JSR DRIVER.READ


		
	LDA $C082				;enable ROM, disable BSR
;COPY DATA READ INTO AUX MEMORY BACK TO MAIN MEMORY
;(only needed if write was set to aux memory)
		; ;AUX MEMORY -> MAIN MEMORY 	
			; LDA #$00			;SET START ADDRESS
			; STA AUX_MOVE.START
			; LDA #$70		
			; STA AUX_MOVE.START+$1
			; ;
			; LDA #$FF			;SET END ADDRESS
			; STA AUX_MOVE.END
			; LDA #$75
			; STA AUX_MOVE.END+$1
			; ;
			; LDA #$00			;SET DESTINATION ADDRESS
			; STA AUX_MOVE.DEST
			; LDA #$80
			; STA AUX_MOVE.DEST+$1
			; CLC					;SET CARRY FLAG DESGINATD MOVE FROM AUX MEMORY -> MAIN
			; JSR AUX_MOVE
	LDA STATUS
	
	BRK

		
DRIVER.READ


;set command type (READ | WRITE)
	lda #$81 ;([$1=read drive1, | $2=write drive1], [$81=read drive2 | $82=write drive2])
;	lda #$1 ;([$1=read drive1, | $2=write drive1], [$81=read drive2 | $82=write drive2])
	sta reqcmd

;set destination memory address
	lda #$00
	sta ldrlo
	lda #$70
	sta ldrhi
	
;set filename to read from	
	lda #file.to.read		;load LO address
	sta namlo
	lda /file.to.read		;load HO address
	sta namhi

		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO

	
	RTS	

DRIVER.WRITE

;create test data to write
		LDA #$00
		STA FILL.START
		LDA #$60
		STA FILL.START+$1
		LDA #$FF
		STA FILL.END
		LDA #$65
		STA FILL.END+$1
		
		LDA #$BA
		STA FILL.VALUE
		
		JSR MEMORY.FILL

		
;LOAD FILL INTO AUX MEMORY (only needed if write is set to aux memory)
		; ;MAIN MEMORY -> AUX MEMORY -> 
			; LDA #$00			;SET START ADDRESS
			; STA AUX_MOVE.START
			; LDA #$60		
			; STA AUX_MOVE.START+$1
			; ;
			; LDA #$FF			;SET END ADDRESS
			; STA AUX_MOVE.END
			; LDA #$65
			; STA AUX_MOVE.END+$1
			; ;
			; LDA #$00			;SET DESTINATION ADDRESS
			; STA AUX_MOVE.DEST
			; LDA #$60
			; STA AUX_MOVE.DEST+$1
			; SEC
			; JSR AUX_MOVE

		
;set command type (READ | WRITE)
	lda #$82 ;([$1=read drive1, | $2=write drive1], [$81=read drive2 | $82=write drive2])
;	lda #$2 ;([$1=read drive1, | $2=write drive1], [$81=read drive2 | $82=write drive2])
	sta reqcmd

;set write data size (# of 512 byte blocks to write from memory)
	lda #$00 ;always #$00
	sta sizelo

	lda #$06 ;number of pages to write (must be in multiples of 2. i.e. 2, 4, 6, 8 $0A, etc)
	sta sizehi


;set write buffer address (source data)
	lda #$00
	sta ldrlo
	lda #$60
	sta ldrhi


;set filename to write to	
	lda #file.to.write		;load LO address
	sta namlo
	lda /file.to.write		;load HO address
	sta namhi

;setup MAIN/AUX memory parameters
	; ;WRITE from aux memory
	; lda $c08b
	; lda $c08b
	; lda #$01 ;01=aux, 00=main
	; sta $f4 ;auxreq

	; ;WRITE from main memory
	; lda $c08b
	; lda $c08b
	; lda #$00 ;01=aux, 00=main
	; sta $f4 ;auxreq

	
; ;write file	
	; jsr $d000 ;opendir
	; lda $c082
	
		LDA #$00	;PARM: $00=main, $01=aux 
	JSR PRODOS.IO
	RTS




PRODOS.IO
;SETUP MAIN/AUX MEMORY PARAMETERS
	;lda #$00 ;01=aux, 00=main
	STA $f4 ;auxreq

;SAVE REGISTERS
	PHA
	TXA
	PHA
	TYA
	PHA
	
	
;GET MEMORY-STATUS UPON ENTRY
	LDA $C012		;ROM/BSR soft-switch flag (bit7 = 1: BSR, bit7=0 ROM)
	STA BSR.STATE
	LDA $C011		;BANK1/BANK2 soft-switch flag (bit7 = 1: BANK2, bit7=0 BANK1)
	STA BANK.STATE

	
;ENABLE BANK-SWITCHED RAM ($D000 Bank 1)
	LDA $c08b
	LDA $c08b

;READ/WRITE FILE		
	JSR $d000 ;opendir, the I/O routine
	LDA $c082

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


;======DEFINE VARIBLES======

;PDRIVER
status    = $f4         ;returns non-zero on error
sizelo    = $f5         ;must set if writing
sizehi    = $f6         ;must set if writing
reqcmd    = $f8         ;used if enable_write=1, 1=read, 2=write
ldrlo     = $f9         ;used if override_adr=1
ldrhi     = $fa         ;used if override_adr=1
namlo     = $fb
namhi     = $fc

;P.IO			
BSR.STATE .BS $1
BANK.STATE .BS $1


;======(TEST) DEFINE VARIBLES======

;file.to.read .AZ #$01,/M/	;ascii array, using LO values. Filename is limited to !15 characters
;file.to.read .AZ #$04,/DATA/	;ascii array, using LO values. Filename is limited to !15 characters
file.to.read  .AZ #$0A,/DUMMY.FILE/	;ascii array, using LO values. Filename is limited to !15 characters
file.to.write .AZ #$0A,/DUMMY.FILE/	;ascii array, using LO values. Filename is limited to !15 characters



animation.update .eq $B000
generate.debug.log .eq $B100


;FORMAT IS FILENAME LENGTH (BYTES), FILENAME. LENGTH IS RAW HEX NUMBER, NOT ASCII VALUE

;file.to.read .EQ $6100
	
;======INCLUDE FILES======


;				.IN 	C:\MY_CODE\testing\prodos_testing\pdriver.test.ASM
;				.IN 	C:\MY_CODE\TEMP\pdriver.TEST.ASM
			
				;My libraries
				.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_graphics				
				.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_text
				.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_math
				.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_general
				.IN 	C:\MY_CODE\INCLUDES_LIBS\ptools.ASM

;seperate target files	
				.IN 	C:\MY_CODE\testing\prodos_testing\dummy.file.ASM					
;				.IN 	C:\MY_CODE\INCLUDES_LIBS\DATA.SHAPES.SURFACE
	
	
	

