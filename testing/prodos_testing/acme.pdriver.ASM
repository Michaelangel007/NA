!cpu 6502
!to "test.bin",plain
*=$800


;*****WARNING: I only did the conversion to the point
;of not getting assembler errors. When I ran this code I got a break at an unexpected location, the file didn't seem to be read in. 

	; lda #$aa
	; brk

			
;set command type (READ | WRITE)
;	lda #$82 ;([$1=read drive1, | $2=write drive1], [$81=read drive2 | $82=write drive2])
	lda #$2 ;([$1=read drive1, | $2=write drive1], [$81=read drive2 | $82=write drive2])
	sta reqcmd	
	JSR DRIVER_WRITE

	LDA $C082				;enable ROM, disable BSR

	
			lda $c082
			lda #$aa
			brk

;set command type (READ | WRITE)
;	lda #$81 ;([$1=read drive1, | $2=write drive1], [$81=read drive2 | $82=write drive2])
	lda #$1 ;([$1=read drive1, | $2=write drive1], [$81=read drive2 | $82=write drive2])
	sta reqcmd
	JSR DRIVER_READ


	LDA $C082				;enable ROM, disable BSR	
	LDA status
	LDX #$DD
	LDY #$AA
	BRK
;
;	
DRIVER_READ
;set destination memory address
	lda #$00
	sta ldrlo
	lda #$70
	sta ldrhi
	
;set filename to read from	
	lda #<DATA_SPR_SURF	;load LO address
	sta namlo
	lda #>DATA_SPR_SURF		;load HO address
	sta namhi

		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS_IO

	
	RTS

DRIVER_WRITE
;set write data size (# of 512 byte blocks to write from memory)
	lda #$00 ;always #$00
	sta sizelo

	lda #$03 ;number of pages to write (must be in multiples of 2. i.e. 2, 4, 6, 8 $0A, etc)
	sta sizehi


;set write buffer address (source data)
	lda #$00
	sta ldrlo
	lda #$A0
	sta ldrhi


;set filename to write to	

	lda #<DATA_SPR_SURF	;load LO address
	sta namlo
	lda #>DATA_SPR_SURF	;load HO address
	sta namhi

	; lda #file.to.write		;load LO address
	; sta namlo
	; lda /file.to.write		;load HO address
	; sta namhi



	
		LDA #$00	;PARM: $00=main, $01=aux 
	JSR PRODOS_IO
	RTS

;c7_parms        !byte 1	
;DATA_SPR_SURF	!byte 1
DATA_SPR_SURF	!byte $0D


	
;.AZ #$01,/D/	;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 

PRODOS_IO
	
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
	STA IO_ATTEMPTS
	;LDX #$00		;COUNTS IO ATTEMPTS
	STA BSR_STATE
	STA BANK_STATE
	STA IO_ATTEMPTS
	STA REQCMD_SAVED
	
	LDA reqcmd
	STA REQCMD_SAVED

	
;GET MEMORY-STATUS UPON ENTRY
	LDA $C012		;ROM/BSR soft-switch flag (bit7 = 1: BSR, bit7=0 ROM)
	STA BSR_STATE
	LDA $C011		;BANK1/BANK2 soft-switch flag (bit7 = 1: BANK2, bit7=0 BANK1)
	STA BANK_STATE

	
;ENABLE BANK-SWITCHED RAM ($D000 Bank 1)
	LDA $c08b
	LDA $c08b

		
READ_WRITE_FILE
			
			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; JSR PREP.BRK
			; LDA STATUS
			; ;LDA #$AB
			; LDX #$AB
			; ;LDX CURRENT.LOCATION.SPR_DATA
			; ;LDY CURRENT.LOCATION.SPR_DATA+$1			
			; BRK
; .TEMP
			; LDA TEMP
			
		NOP
		NOP
		NOP
		NOP
	
	JSR $d000 ;opendir, the I/O routine

		
				; LDA IO.ATTEMPTS
				; BEQ .TEMP2
				; LDA $C082
				; LDA STATUS
				; LDX #$AB
				; BRK
; .TEMP2
		
			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK2
			; CMP #$01
			; BNE .TEMP2
			; JSR PREP.BRK
			; BRK
; .TEMP2
			; LDA TEMP
		NOP
		NOP
		NOP
		NOP	
		
	LDA status			;load opendir return code
	BEQ IO_SUCCEEDED	;was file-not-found ($01) reported? If no, then treat IO as successful
	LDA IO_ATTEMPTS		;check whether both drives have been searched for the file requested
;	CPX IO.ATTEMPTS		;check whether both drives have been searched for the file requested
	BNE INSERT_DISK	;if both drives have been checked (>=$01) then prompt player to insert disk
						;if not, try other drive 
		
	;determine which drive to try next

		
	LDA REQCMD_SAVED
	BMI TRY_DRIVE1		;is bit7 = 1? Then drive2 was just tried, check drive1 next
;.TRY.DRIVE2
	ORA #$80			;effectively adds #$80 to the ACC by changing bit 7 to $1. After ORA the ACC will contain a 1 for each bit which had a value of 1 in either the ORA value or the value in the ACC before the ORA was executed. 
						;Since we know that the read/write was first attempted on drive1 we know the value in the ACC was either $1 (read) or $2= (write). Adding $80 resuts in the values needed for drive2, for whichever operation (read or write) was aready specified. i.e. ($81 = read drive2, $82 = write drive2)
	STA reqcmd			;update the drive in the read/write command parameter
	INC IO_ATTEMPTS

	JMP READ_WRITE_FILE

	
TRY_DRIVE1
	AND #$03		;effectively subtracts #$80 from the ACC by changing bit 7 to $0. After the logical "AND" the ACC will contain a 1 for each bit which had a value of 1 in both the AND value and the value in the ACC before the AND was executed. In this case, bit 0 was the only bit with a value of $1 in both.  
					;Since we know that the read/write was first attempted on drive2 we know the value in the ACC was either $81 (read) or $82= (write). Subtracting $80 resuts in the values needed for drive1, for whichever operation (read or write) was aready specified. i.e. ($1 = read drive1, $2 = write drive1)
	STA reqcmd		;update the drive in the read/write command parameter

	INC IO_ATTEMPTS

	JMP READ_WRITE_FILE
	
INSERT_DISK
	;<WRITE CODE FOR DISK PROMPT>. 
	;probably will be positioned on screen differently
	;if the prompt occurs during boot than if it occurs
	;during game play
								
			;JSR PREP.BRK
							
; ;Print "NO FILE" to text screen		
			; LDA #$CE
			; JSR COUT_ADDRESS
			; LDA #$CF
			; JSR COUT.ADDRESS
			; LDA #$A0
			; JSR COUT.ADDRESS
			; LDA #$C6
			; JSR COUT.ADDRESS
			; LDA #$C9
			; JSR COUT.ADDRESS
			; LDA #$CC
			; JSR COUT.ADDRESS
			; LDA #$C5
			; JSR COUT.ADDRESS			
		
			; BRK

	
IO_SUCCEEDED
	
;RESTORE MEMORY-STATUS
	LDA BSR_STATE					;get BSR State (either ROM enabled or BSR enabled) before P.IO driver was called
	BMI GET_PRIOR_BSR_BANK_STATE	;is bit7 = 1? If yes, BSR was enabled, find out which bank
	STA $C082						;if no, ROM was enabled, so re-enable ROM
	BPL ALLDONE					;BPL must be true if BMI was false
GET_PRIOR_BSR_BANK_STATE
	LDA BANK_STATE					;get BSR Bank State (either Bank1 or Bank2 enabled) before P.IO driver was called
	BMI BANK2						;is bit7=1? If yes, bank2 was enabled
	STA $C08B						;if no, Bank1 was enabled, so re-enable bank1
	STA $C08B
	BPL ALLDONE
BANK2
	STA $C083						;re-enable bank2
	STA $C083
ALLDONE


;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX
	PLA

	RTS



;======DEFINE VARIBLES======

;OpenDir 
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
BSR_STATE 		= $8000
BANK_STATE 		= $8001

IO_ATTEMPTS		= $8002
REQCMD_SAVED 	= $8003