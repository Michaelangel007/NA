				.CR     6502            Use 6502 overlay
				.OR		$1000			**Always put before .TF directive and never use again in program
				.TF     TEST.BIN,BIN
				.EF		errors
				.LF		c:\my_code\na\build_folder\LIST
;				.LI 	OFF				Switches the assembler machine code conversion listing off so errors are easier to see				
;				.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_common


PDRIVER.DEMO.PROGRAM ;HARD DISK, PRODOS INSTALLED


;=====SCENARIO: WRITE/READ/SEEK/READ========
;Comments: this example does the following:
;		*Open new file, write $1000 bytes from $A000-$AFFF
;		*Open new file, read $1000 bytes into $7000-$7FFF
;		*Seek current file +$1000 bytes
;		*Read current file, $1000 bytes into $8000
;
;DATA FILE SETUP
;		$0000-$0FFF = $AA
;		$1000-$1FFF = $BB
;		$2000-$2FFF = $CC
;
;EXPECTED RESULT:
;		$7000-7FFF = $EE
;		$8000-8FFF = $CC
;
;===DISK IMAGES===
;
;sbasm_test.dsk is the one that boots.
;sbasm_test2.PO contains "DUMMY.FILE", which is the I/O target. See bottom of this source file for the include that causes SBASM to generate a binary version of DUMMY FILE.
;=========================================================================================================================================================================




;-PROGRAM START


	;JSR PDRIVER.AUX_BSR.INIT
.SETUP.MEMORY
@START

;SETUP FILL TO DETECT READ OVERFLOW
		LDA #$00
		STA FILL.START
		LDA #$70
		STA FILL.START+$1
		LDA #$FF
		STA FILL.END
		LDA #$7F
		STA FILL.END+$1
		
		LDA #$42
		STA FILL.VALUE
		
		JSR MEMORY.FILL
		
		
;SETUP WRITE SOURCE DATA 
		LDA #$00
		STA FILL.START
		LDA #$A0
		STA FILL.START+$1
		LDA #$FF
		STA FILL.END
		LDA #$AF
		STA FILL.END+$1
		
		LDA #$EE
		STA FILL.VALUE
		
		JSR MEMORY.FILL
@END




; ;open new file and write $1000 bytes
; ;file position start:	$0
; ;file position end:		$1000

;TEMPLATE.WRITE.OPEN_FILE
@START

;set command type (READ | WRITE | SEEK)
; cmd_seek.current_drive 	.EQ $90
; cmd_read.current_drive  	.EQ $91
; cmd_write.current_drive 	.EQ $92
;			
; cmd_read.drive1			.EQ $1
; cmd_read.drive2		  	.EQ $81
; cmd_write.drive1			.EQ $2
; cmd_write.drive2			.EQ $82
	lda #cmd_write.drive2 ;([$1=read drive1, | $2=write drive1], [$81=read drive2 | $82=write drive2], [$90=seek current_drive, | $91=read current_drive, | $92=write current_drive])
	sta parm.reqcmd	
	
;set write data size (# of 512 byte blocks to write from memory)
	lda #$00 ;always #$00
	sta parm.sizelo

	lda #$10 ;number of pages to write. ;# of pages to write (LO byte). (must be in multiples of 2. i.e. 2, 4, 6, 8 $0A, etc)
	sta parm.sizehi


;set write buffer address (source data)
	lda #$00
	sta parm.ldrlo
	lda #$A0
	sta parm.ldrhi
	
;set filename to write to	
	lda #file.to.write	;load LO address
	sta parm.namlo
	lda /file.to.write	;load HO address
	sta parm.namhi

		LDA #$00	;PARM: $00=main, $01=aux 
	JSR PRODOS.IO
@END


; open new file and read $1000 bytes 
; file position start:	$0
; file position end:		$1000

;TEMPLATE.READ.OPEN_FILE
@START
; ;set command type (READ | WRITE | SEEK)
; ; cmd_seek.current_drive 	.EQ $90
; ; cmd_read.current_drive  	.EQ $91
; ; cmd_write.current_drive 	.EQ $92
; ;			
; ; cmd_read.drive1			.EQ $1
; ; cmd_read.drive2		  	.EQ $81
; ; cmd_write.drive1			.EQ $2
; ; cmd_write.drive2			.EQ $82

	lda #cmd_read.drive2
	sta parm.reqcmd

;set destination memory address
	lda #$00
	sta parm.ldrlo
	lda #$70
	sta parm.ldrhi

;set read length (bytes)
	lda #$00				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda #$10				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi
	
		LDA #$00	;PARM: $00=main, $01=aux 
	JSR PRODOS.IO


@END
	

; ; seek to position $2000 in opened file
; ; file position start:	$1000
; ; file position end:		$2000

;TEMPLATE.SEEK.CURRENT_FILE		
@START
; ;set command type (READ | WRITE | SEEK)
; ; cmd_seek.current_drive 	.EQ $90
; ; cmd_read.current_drive  	.EQ $91
; ; cmd_write.current_drive 	.EQ $92
; ;			
; ; cmd_read.drive1			.EQ $1
; ; cmd_read.drive2		  	.EQ $81
; ; cmd_write.drive1			.EQ $2
; ; cmd_write.drive2			.EQ $82

	lda #cmd_seek.current_drive
	sta parm.reqcmd	
	
;set # of bytes to seek
	lda #$00 
	sta parm.sizelo

	lda #$10 ;number of pages to write. ;# of pages to write (LO byte). (must be in multiples of 2. i.e. 2, 4, 6, 8 $0A, etc)
	sta parm.sizehi
	
		LDA #$00	;PARM: $00=main, $01=aux 
	JSR PRODOS.IO

@END

; ; seek to position $2000 in opened file
; ; file position start:	$1000
; ; file position end:		$2000
		; lda #$00		;([$1=read drive1, | $2=write drive1], [$81=read drive2 | $82=write drive2 | $00 = seek]
		; sta reqcmd
		
		; lda #$00		;number of bytes to seek (LO byte)
		; sta sizelo
		
		; lda #$10		;number of bytes to seek (HO byte)
		; sta sizehi

		; LDA $c08b		;enable bank-switched ram ($d000 bank 1)
		; LDA $c08b

		; LDA #$00		;select memory (this may not be needed for a seek, but to be safe set it to the same value as specified when opening the file)
		; STA AUXREQ

; ;ENABLE AUX BSR & ZERO PAGE
			; TSX			;transfer stack pointer to X-REG
			; STX TEMP	;save stack pointer
		; STA $C009 ;enable aux zero-page & aux BSR.
			
		; jsr $d000
		
		; STA $C008 ;enable main zero-page & main BSR 
			; LDX TEMP	;restore stack pointer to X-REG
			; TXS ;transfer X-REG to stack pointer

			
			
; read $1000 bytes to $8000 from opened file
; file position start:	$2000
; file position end:		$3000		
	
;TEMPLATE.READ.CURRENT_FILE
@START
;set command type (READ | WRITE | SEEK)
; cmd_seek.current_drive 	.EQ $90
; cmd_read.current_drive  	.EQ $91
; cmd_write.current_drive 	.EQ $92
;			
; cmd_read.drive1			.EQ $1
; cmd_read.drive2		  	.EQ $81
; cmd_write.drive1			.EQ $2
; cmd_write.drive2			.EQ $82
	lda #cmd_read.current_drive
	sta parm.reqcmd

;set destination memory address
	lda #$00
	sta parm.ldrlo
	lda #$80
	sta parm.ldrhi

;set read length (bytes)
	lda #$00				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda #$10				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi
	
		LDA #$00	;PARM: $00=main, $01=aux 
	JSR PRODOS.IO


@END

	LDA $C082				;enable ROM, disable BSR
	LDA STATUS
	LDX #$DD
	LDY #$AA
	BRK		
	


;===========SUBROUTINES COPIED FROM MY LIBRARY FILES=======

MEMORY.COPY ;============FILL MEMORY WITH SPECIFIED VALUE=======
@START
;PARAMTERS; COPY.TO(2), COPY.FROM(2), COPY.FROM_END(2)
;RETURN: NONE
;ENTRANCE: DIRECT

;DRIVER TEMPLATE	
		; LDA #$00
		; STA COPY.FROM_START
		; LDA #$70
		; STA COPY.FROM_START+$1
		; LDA #$00
		; STA COPY.FROM_END
		; LDA #$75
		; STA COPY.FROM_END+$1
		;	
		; LDA #$00
		; STA COPY.TO
		; LDA #$80
		; STA COPY.TO+$1
		;
		; JSR MEMORY.COPY


		
.START

;SAVE REGISTERS	
	TYA
	PHA

	LDY #$00				;LO BYTE COUNTER
;	COPY.FROM_START+$1		;DOUBLES AS HO BYTE COUNTER
	
.LOOP					
	LDA (COPY.FROM_START),Y
	STA (COPY.TO),Y
	LDA COPY.FROM_START+$1	
	CMP COPY.FROM_END+$1	;HAS HO BYTE COUNTER (COPY.FROM_START+$1) COUNTER REACHED FILL_END HO BYTE?
	BEQ .EXIT.TEST			;IF YES, CHECK TO SEE IF FILL_END LO BYTE HAS BEEN REACHED
.INCREMENT.COUNTER.LO_BYTE
	INY						;IF NO, INCREMENT LO BYTE COUNTER
	BNE .LOOP				;IF Y-REG HASN'T FLIPPED TO $00, CONTINUE LOOP
.INCREMENT.COUNTER.HO_BYTE		
	INC COPY.FROM_START+$1	;FILL_START+$1 DOUBLES AS THE HO_BYTE COUNTER
	INC COPY.TO+$1			
	JMP .LOOP
	
.EXIT.TEST
	TYA
	CLC
	ADC COPY.FROM_START		;THE LO BYTE START VALUE + Y-REG (COUNTER FOR LO BYTE) IS WHAT WE NEED TO COMPARE TO COPY.FROM.END
	CMP	COPY.FROM_END		;DOES Y-REG (LO_BYTE COUNTER) == COPY.FROM_END (THE LO BYTE OF END ADDRESS)?
	BNE .INCREMENT.COUNTER.LO_BYTE	;IF NO, INCREMENT COUNTER AND CONTINUE LOOP	
.COPY_DONE					;IF YES, THEN COPY IS DONE. 

;RESTORE REGISTERS	
	PLA
	TAY
	
	RTS
	
@END
	
MEMORY.FILL ;============FILL MEMORY WITH SPECIFIED VALUE=======	
@START
;PARAMTERS; FILL.START(2), FILL.END(2), FILL.VALUE(1)
;RETURN: NONE
;ENTRANCE: DIRECT

;DRIVER TEMPLATE
		; LDA #$00
		; STA FILL.START
		; LDA #$70
		; STA FILL.START+$1
		; LDA #$00
		; STA FILL.END
		; LDA #$75
		; STA FILL.END+$1
		
		; LDA #$AA
		; STA FILL.VALUE
		
		; JSR MEMORY.FILL

		
.START

;RESTORE REGISTERS	
	TYA
	PHA
	
	LDY #$00			;LO BYTE COUNTER
;	FILL.START+$1		;DOUBLES AS HO BYTE COUNTER
.LOOP					
	LDA FILL.VALUE
	STA (FILL.START),Y
	LDA FILL.START+$1	
	CMP FILL.END+$1		;HAS HO BYTE COUNTER (FILL.START+$1) COUNTER REACHED FILL_END HO BYTE?
	BEQ .EXIT.TEST		;IF YES, CHECK TO SEE IF FILL_END LO BYTE HAS BEEN REACHED
.INCREMENT.COUNTER.LO_BYTE
	INY					;IF NO, INCREMENT LO BYTE COUNTER
	BNE .LOOP			;IF Y-REG HASN'T FLIPPED TO $00, CONTINUE LOOP
.INCREMENT.COUNTER.HO_BYTE		
	INC FILL.START+$1	;FILL_START+$1 DOUBLES AS THE HO_BYTE COUNTER
	JMP .LOOP
	
.EXIT.TEST
	TYA
	CLC
	ADC FILL.START		;THE LO BYTE START VALUE + Y-REG (COUNTER FOR LO BYTE) IS WHAT WE NEED TO COMPARE TO FILL.END
	CMP	FILL.END		;DOES Y-REG (LO_BYTE COUNTER) == FILL_END (THE LO BYTE OF END ADDRESS)?
	BNE .INCREMENT.COUNTER.LO_BYTE	;IF NO, INCREMENT COUNTER AND CONTINUE LOOP	
.FILL_DONE				;IF YES, THEN FILL IS DONE. 

;SAVE REGISTERS	
	PLA
	TAY
	
	RTS
@END


PDRIVER.AUX_BSR.INIT ;COPY PDRIVER TO AUX BSR:BANK1
@START
;COPY PDRIVER FROM MAIN BSR:BANK1 TO AUX BSR:BANK1
	LDA $c08b		;enable bank-switched ram ($d000 bank 1)
	LDA $c08b
		
	;copy pdriver to main memory temp location
		LDA #$00
		STA COPY.FROM_START
		LDA #$D0
		STA COPY.FROM_START+$1
		LDA #$FF
		STA COPY.FROM_END
		LDA #$D8
		STA COPY.FROM_END+$1
			
		LDA #$00
		STA COPY.TO
		LDA #$B0
		STA COPY.TO+$1
	
	JSR MEMORY.COPY		

		
		TSX			;transfer stack pointer to X-REG
		STX TEMP	;save stack pointer
	STA $C009 ;enable aux zero-page & aux BSR 

	;copy pdriver from main memory temp location to aux BSR
		LDA #$00
		STA COPY.FROM_START
		LDA #$B0
		STA COPY.FROM_START+$1
		LDA #$FF
		STA COPY.FROM_END
		LDA #$B8
		STA COPY.FROM_END+$1
			
		LDA #$00
		STA COPY.TO
		LDA #$D0
		STA COPY.TO+$1
	
	JSR MEMORY.COPY	
		
	STA $C008 ;enable main zero-page & main BSR 
		LDX TEMP	;restore stack pointer to X-REG
		TXS ;transfer X-REG to stack pointer

	LDA $c08b		;enable bank-switched ram ($d000 bank 1)
	LDA $c08b
	
	;erase original pdriver location in main BSR:bank1
	;Note: the erase isn't required, it is included for testing purposes to ensure that the driver isn't accidentally run via it's original location in main BSR:bank1
		LDA #$00
		STA COPY.FROM_START
		LDA #$A0
		STA COPY.FROM_START+$1
		LDA #$FF
		STA COPY.FROM_END
		LDA #$A8
		STA COPY.FROM_END+$1
			
		LDA #$00
		STA COPY.TO
		LDA #$D0
		STA COPY.TO+$1
	
	JSR MEMORY.COPY
	
	
	RTS
@END
	
;======TEST PROGRAM VARIABLES======
file.to.read  .AZ #$0A,/DUMMY.FILE/	;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 
file.to.write .AZ #$0A,/DUMMY.FILE/	;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 

; ;needed for OpenDIR (prodos driver)
; bleftlo   = $EF ;Bytes left in file. If no bytes have been read then it is the total bytes in the file. 
; bleftho   = $F0
; status    = $f3         ;returns non-zero on error
; auxreq	  = $f4			;$00 = main memory, $01 = aux memory
; sizelo    = $f5         ;must set if reading or writing. For writing, # of pages to write must be in multiples of 2. i.e. 2, 4, 6, 8 $0A, etc) 
; sizehi    = $f6         ;must set if reading or writing.
; reqcmd    = $f8         ;used if enable_write=1, 1=read, 2=write
; ldrlo     = $f9         ;used if override_adr=1
; ldrhi     = $fa         ;used if override_adr=1
; namlo     = $fb
; namhi     = $fc


;for fill and copy subroutines
COPY.TO				.EQ $FA				;2byt	
COPY.FROM_START		.EQ $FC				;2byt
COPY.FROM_END		.EQ $E0



FILL.START	.EQ	$EB			;START ADDRESS TO FILL
FILL.END	.BS $2			;END ADDRESS TO FILL
FILL.VALUE 	.BS $1			;VALUE TO FILL WITH
		

SAVED.YREG.GLOBAL1						.BS $01		;1byt
SAVED.YREG.GLOBAL2						.BS $01		;1byt
SAVED.YREG.LOCAL						.BS $01		;1byt
SAVED.YREG.LOCAL1						.BS $01		;1byt
;ANIMATION.XREG.STORAGE2						.BS $01		;1byt
SAVED.XREG.GLOBAL1						.BS $01		;1byt
ANIMATION.XREG.STORAGE						.BS $01		;1byt
ANIMATION.XREG.STORAGE1 						.BS $01		;1byt
SAVED.ACC.GLOBAL1						.BS $01
SAVED.ACC.LOCAL							.BS $01
SAVED.ACC.LOCAL2						.BS $01
TEMP									.BS $01		;1byt

TEMP16



;=====================================

;======INCLUDE FILES======

;OpenDir wrapper
				.IN 	C:\MY_CODE\INCLUDES_LIBS\ptools.ASM

;seperate target files	
				.IN 	C:\MY_CODE\testing\prodos_testing\dummy.file.ASM					
