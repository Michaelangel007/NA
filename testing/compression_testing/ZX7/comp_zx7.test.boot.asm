				.OR		$1000			**Always put before .TF directive and never use again in program
				.TF    	COMP_ZX7.BOOT.BIN,BIN
				
				; .OR		$1000			**Always put before .TF directive and never use again in program
				; .TF     NOXARCH.MAIN.BIN,BIN
				; .EF		errors
				; .LF		C:\MY_CODE\LIST
; ;				.LI 	OFF				Switches the assembler machine code conversion listing off so errors are easier to see				
; ;				.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_common

;=====================SUBROUTINE DOCUMENTATION====================================
;
;See COMP.TEST.ASM for the main test code
;this is just the ProDOS bootloader's hand-off file. The comments below contain more details about what this file does. 
;
;
;This file, NOXARCH.MAIN, is the last file loaded by the booloader (NOX.SYSTEM).
;It appears that there is a size limit to how large NOXARCH.MAIN
;can be. I'm not sure exactly how large, but $3000 bytes was too much. 
;
;For this reason, all this file does is load LOADER.P, which is the primary
;game loader file. 
;
;=================================================================================

NOXARCH.MAIN

		; LDA $C082
		; BRK
		
	JSR NOXARCH.MAIN.READ
		
	JMP COMP.TEST.START
	;JMP TEST.PROGRAM
	

NOXARCH.MAIN.READ
@START
;set command type (READ | WRITE)
	lda #$1 ;([$1=read drive1, | $2=write drive1], [$81=read drive2 | $82=write drive2])
	sta reqcmd

;set destination memory address
	lda #$00
	sta ldrlo
	lda #$B0
	sta ldrhi
	
;set filename to read from	
	lda #COMP_ZX7.PROG		;load LO address
	sta namlo
	lda /COMP_ZX7.PROG		;load HO address
	sta namhi

		LDA #$00	;PARM: $00=main, $01=aux
	JSR NOXARCH.MAIN.PRODOS.IO

	
	RTS	
@END



;SUBROUTINES (LOCAL COPY)


NOXARCH.MAIN.PRODOS.IO
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
	;lda #$00 ;01=aux, 00=main
	STA $f4 ;auxreq

;SAVE REGISTERS
	PHA
	TXA
	PHA
	TYA
	PHA
	
;INIT VARIABLES
	LDA #$00
	STA IO.ATTEMPTS2
	;LDX #$00		;COUNTS IO ATTEMPTS
	
	LDA REQCMD
	STA REQCMD.SAVED2
	
;GET MEMORY-STATUS UPON ENTRY
	LDA $C012		;ROM/BSR soft-switch flag (bit7 = 1: BSR, bit7=0 ROM)
	STA BSR.STATE2
	LDA $C011		;BANK1/BANK2 soft-switch flag (bit7 = 1: BANK2, bit7=0 BANK1)
	STA BANK.STATE2

	
;ENABLE BANK-SWITCHED RAM ($D000 Bank 1)
	LDA $c08b
	LDA $c08b

		
.READ_WRITE.FILE
	
	JSR $d000 ;opendir, the I/O routine
		
	LDA STATUS			;load opendir return code
	BEQ .IO.SUCCEEDED	;was file-not-found ($01) reported? If no, then treat IO as successful
	LDA IO.ATTEMPTS2	;check whether both drives have been searched for the file requested
;	CPX IO.ATTEMPTS		;check whether both drives have been searched for the file requested
	BNE .INSERT.DISK	;if both drives have been checked (>=$01) then prompt player to insert disk
						;if not, try other drive 
		
	;determine which drive to try next

		
	LDA REQCMD.SAVED2
	BMI .TRY.DRIVE1		;is bit7 = 1? Then drive2 was just tried, check drive1 next
;.TRY.DRIVE2		


			
	ORA #$80			;effectively adds #$80 to the ACC by changing bit 7 to $1. After ORA the ACC will contain a 1 for each bit which had a value of 1 in either the ORA value or the value in the ACC before the ORA was executed. 
						;Since we know that the read/write was first attempted on drive1 we know the value in the ACC was either $1 (read) or $2= (write). Adding $80 resuts in the values needed for drive2, for whichever operation (read or write) was aready specified. i.e. ($81 = read drive2, $82 = write drive2)
	STA REQCMD			;update the drive in the read/write command parameter
	INC IO.ATTEMPTS2

	JMP .READ_WRITE.FILE

	
.TRY.DRIVE1
	AND #$03		;effectively subtracts #$80 from the ACC by changing bit 7 to $0. After the logical "AND" the ACC will contain a 1 for each bit which had a value of 1 in both the AND value and the value in the ACC before the AND was executed. In this case, bit 0 was the only bit with a value of $1 in both.  
					;Since we know that the read/write was first attempted on drive2 we know the value in the ACC was either $81 (read) or $82= (write). Subtracting $80 resuts in the values needed for drive1, for whichever operation (read or write) was aready specified. i.e. ($1 = read drive1, $2 = write drive1)
	STA REQCMD		;update the drive in the read/write command parameter

	INC IO.ATTEMPTS2

	JMP .READ_WRITE.FILE
	
.INSERT.DISK
	;<WRITE CODE FOR DISK PROMPT>. 
	;probably will be positioned on screen differently
	;if the prompt occurs during boot than if it occurs
	;during game play


			LDA $C082
			
;Print "NO FILE" to text screen
			JSR CLEAR.TEXT.SCREEN
			
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
	LDA BSR.STATE2					;get BSR State (either ROM enabled or BSR enabled) before P.IO driver was called
	BMI .GET.PRIOR.BSR.BANK_STATE	;is bit7 = 1? If yes, BSR was enabled, find out which bank
	STA $C082						;if no, ROM was enabled, so re-enable ROM
	BPL .alldone					;BPL must be true if BMI was false
.GET.PRIOR.BSR.BANK_STATE
	LDA BANK.STATE2					;get BSR Bank State (either Bank1 or Bank2 enabled) before P.IO driver was called
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
;PRODOS.IO			
BSR.STATE2 		.BS $1
BANK.STATE2 	.BS $1

IO.ATTEMPTS2	.BS $1
REQCMD.SAVED2 	.BS $1

;======(TEST) DEFINE VARIBLES======

;FORMAT IS FILENAME LENGTH (BYTES), FILENAME. LENGTH IS RAW HEX NUMBER, NOT ASCII VALUE

COMP_ZX7.PROG  .AZ #$0D,/COMP_ZX7.PROG/	;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 
;COMP_ZX7.PROG  .AZ #$0D,/COMP_ZX7.PROG/	;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 

	
;======INCLUDE FILES======

;NONE

	.NO $1100				;ENSURES THAT THE PROGRAM CODE DOESN'T GET CLOBBERED BY THE INCLUDE TARGET FILES,

	




