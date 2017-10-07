				.CR     6502            Use 6502 overlay
				.OR		$1000			**Always put before .TF directive and never use again in program
				.TF     TEST.BIN,BIN
				.EF		errors
				.LF		c:\my_code\na\build_folder\LIST
;				.LI 	OFF				Switches the assembler machine code conversion listing off so errors are easier to see				
;				.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_common


OpenDir.TEST.PROGRAM ;HARD DRIVE /W DRIVER IN AUX BSR

;=====================SUBROUTINE DOCUMENTATION====================================
;
;===OVERVIEW===
;Uses prodos driver to writes 3 pages of $EE values to a $1000 byte file called "DUMMY.FILE" in drive 1
;and then read "DUMMY.FILE" from drive 1
;
;Before the write, "DUMMY.FILE" contains the value $AA in all bytes.
;
;===EXPECTED RESULT===
;
;Break at $1052 (monitor reports $1054), X-REG = $DD, YREG = $AA 
;
;7000.72FF = $EE
;7300.7FFF = $AA
;
;===DISK IMAGES===
;
;sbasm_test.dsk is the one that boots to this program and contains "DUMMY.FILE"
;sbasm_test2.dsk is plumbed into the batch files so that a dual drive test could easily be setup.
;
;=================================================================================

			
	JSR OpenDir.AUX_BSR.INIT
			
			
		TXA
		PHA
		TYA
		PHA
.RESTORE.WEAPONS.SHAPE_TABLES
@START	
; ;=====LOAD SPELL FILE======
;filename = "SRTN.SPELL_FILE"
	
;------OPEN FILE------
.OPEN.FILE
	
	;set command type (READ | WRITE | SEEK)
; cmd_seek.current_drive 	.EQ $90
; cmd_read.current_drive  	.EQ $91
; cmd_write.current_drive 	.EQ $92
;			
; cmd_read.drive1			.EQ $1
; cmd_read.drive2		  	.EQ $81
; cmd_write.drive1			.EQ $2
; cmd_write.drive2			.EQ $82

	LDA #cmd_read.drive2
	sta parm.reqcmd
	
;set read length (bytes)
	LDA #$01 	;$01
	sta parm.sizelo	
	LDA #$00	;$00
	sta parm.sizehi
	
; ;set destination memory address
	; lda #SHAPE.HOPPER0
	; sta parm.ldrlo
	; lda /SHAPE.HOPPER0
	; sta parm.ldrhi

;set destination memory address
	;lda #SHAPE.HOPPER0
	LDA #$00
	sta parm.ldrlo
	;lda /SHAPE.HOPPER0
	LDA #$60
	sta parm.ldrhi	

;set filename to read from	
	lda #file.to.read	;load LO address
	sta parm.namlo
	lda /file.to.read	;load HO address
	sta parm.namhi
					
		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO

					; LDA #$EE
					; ;JSR PREP.BRK
					; BRK
			
;------SEEK FILE------
;(seek to the start of the weapon shape tables)

.SEEK.FILE
	lda #cmd_seek.current_drive
	sta parm.reqcmd

;set seek length (bytes)
	lda #WEAPON.SHAPE_TABLES.SEEK_BYTES	;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda /WEAPON.SHAPE_TABLES.SEEK_BYTES	;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi

; ;set seek length (bytes)
	; lda #$01	;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	; sta parm.sizelo	
	; lda #$00	;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	; sta parm.sizehi

	
		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO	
	
	;**FALLS THROUGH**
	
			; LDA #$AA
			; ;JSR PREP.BRK
			; BRK


			
.INIT.LOOP.VARIABLES
	; LDA #SPELL.FILE.WEAPONS.SHAPE_TABLE.AUX.START
	; STA COPY.TO
	; LDA /SPELL.FILE.WEAPONS.SHAPE_TABLE.AUX.START
	; STA COPY.TO+$1
		
	LDA /WEAPON.SHAPE_TABLES.READ_BYTES	
	TAY ;set loop counter (only the HO byte is needed for the counter because the weapons shape tables always end on a page boundry, they are $400bytes each)
	
	LDA #$00
	STA COPY.TO+$0
	LDA #$70
	STA COPY.TO+$1
	
.RESTORE.LOOP

.READ.CHUNK
;----READ FILE-----
;(read file in $100 byte chunks until the remaining bytes to 
;read are < $100)
	
	lda #cmd_read.current_drive
	sta parm.reqcmd

;set destination memory address
	;lda #SHAPE.HOPPER0
	LDA #$00
	sta parm.ldrlo
	;lda /SHAPE.HOPPER0
	LDA #$60
	sta parm.ldrhi
	
;set read length (bytes)
	lda #$00				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda #$01		;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi

		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO


	;execute copy	
		LDA #$00
		STA COPY.FROM_START+$0
		LDA #$60
		STA COPY.FROM_START+$1

		LDA #$FF
		STA COPY.FROM_END+$0
		LDA #$60
		STA COPY.FROM_END+$1

		; LDA #$00
		; STA COPY.TO+$0
		; LDA #$70
		; STA COPY.TO+$1
		
		;COPY.TO+$1 is init before loop starts and it is incremented below
	JSR MEMORY.COPY	

	
			; ; CPY #$00
			; ; BNE .TEMP
			; LDA #$AA
			; ;JSR PREP.BRK
			; BRK
	
;.TEMP

; .COPY.CHUNK.TO.AUX_BSR	
		; LDA $C083 ;ENABLE BSR
	; ;enable AUX memory (BSR: bank2 is already enabled)						
		; TSX			;transfer stack pointer to X-REG
		; STX TEMP	;save stack pointer
	; STA $C009 ;enable aux zero-page & aux BSR 

	; ;execute copy	
		; ; LDA #SHAPE.HOPPER0
		; ; STA COPY.FROM_START+$0
		; ; LDA /SHAPE.HOPPER0
		; ; STA COPY.FROM_START+$1

		; ; LDA COPY.FROM_START+$0
		; ; CLC
		; ; ADC #$FF
		; ; STA COPY.FROM_END+$0
		; ; LDA COPY.FROM_START+$1
		; ; ADC #$00 ;16-bit add
		; ; STA COPY.FROM_END+$1


		; LDA #$00
		; STA COPY.FROM_START+$0
		; LDA #$06
		; STA COPY.FROM_START+$1

		; LDA #$FF
		; STA COPY.FROM_END+$0
		; LDA #$06
		; STA COPY.FROM_END+$1

		
		; ; LDA COPY.FROM_START+$0
		; ; BEQ .TEST
		
				; ; STA $C008 ;enable main zero-page & main BSR 
				; ; LDX TEMP	;restore stack pointer to X-REG
				; ; TXS ;transfer X-REG to stack pointer
				
				; ; LDA #$AA
				; ; LDX COPY.FROM_START+$0
				; ; LDY COPY.FROM_START+$1
				; ; JSR PREP.BRK
				; ; BRK
; ; .TEST		
		; ;COPY.TO+$1 is init before loop starts and it is incremented below
	; JSR MEMORY.COPY	
		
	; STA $C008 ;enable main zero-page & main BSR 
		; LDX TEMP	;restore stack pointer to X-REG
		; TXS ;transfer X-REG to stack pointer
	; LDA $C082 ;ENABLE rom


.INCREMENT.COUNTER
	INC COPY.TO+$1
	DEY
	BNE .RESTORE.LOOP


.RESTORE.DONE
		; JSR KEYIN
		; JSR FLIP.PAGE
		; JSR KEYIN
@END
		PLA
		TAY
		PLA
		TAX

		
; ;****TEST TRANSFER

	; LDA $C083 ;ENABLE BSR

	; ;enable AUX memory (BSR: bank2 is already enabled)						
		; TSX			;transfer stack pointer to X-REG
		; STX TEMP	;save stack pointer
	; STA $C009 ;enable aux zero-page & aux BSR 

	; ;execute copy	
		; LDA #$00
		; STA COPY.FROM_START+$0
		; LDA #$D0
		; STA COPY.FROM_START+$1

		; LDA #$FF
		; STA COPY.FROM_END+$0
		; LDA #$DF
		; STA COPY.FROM_END+$1

		; LDA #$00
		; STA COPY.TO+$0
		; LDA #$70
		; STA COPY.TO+$1
		
		; ;COPY.TO+$1 is init before loop starts and it is incremented below
	; JSR MEMORY.COPY	

	
	; STA $C008 ;enable main zero-page & main BSR 
		; LDX TEMP	;restore stack pointer to X-REG
		; TXS ;transfer X-REG to stack pointer
	; LDA $C082 ;ENABLE rom

		
		LDA #$AB
		BRK

			
;scenario specific variables
SRTN.SPELL_FILE		.AZ #$0F,/SRTN.SPELL_FILE/				;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 
WEAPON.SHAPE_TABLES.SEEK_BYTES .EQ $2049
WEAPON.SHAPE_TABLES.READ_BYTES .EQ $0D00
	
SWAP_SPACE.MAIN_MEMORY			.EQ $9600 ;#POINTER ADDRESS
;SWAP_SPACE.MAIN_MEMORY			.EQ $A000 ;#POINTER ADDRESS
SWAP_SPACE.MAIN_MEMORY.END		.EQ $BFFF ;#POINTER ADDRESS
SWAP_SPACE.MAIN_MEMORY.POINTER	.EQ $ED	;Zero page pointer for swap space memory region

SWAP_SPACE.AUX_MEMORY			.EQ $2F00 ;#POINTER ADDRESS
SWAP_SPACE.AUX_MEMORY.END		.EQ $58FF ;#POINTER ADDRESS

SWAP_SPACE2.AUX_MEMORY			.EQ $D600 ;#POINTER ADDRESS. BSR bank2
SWAP_SPACE2.AUX_MEMORY.END		.EQ $FFFF ;#POINTER ADDRESS. BSR bank2
SWAP_SPACE2.AUX_MEMORY.MOB.START	.EQ $FCFF ;#POINTER ADDRESS. The start address of the mob map objects array when the main memory swap area is swapped out to this buffer. 



SPELL.FILE.ENTRANCE.BUFFER			.EQ	SWAP_SPACE.MAIN_MEMORY ;#CONSTANT. Memory address that SPELL_FILE.ENTRANCE 
										;$9600				;is read into from disk. This routine parses spell code stored in the player 
															;keypress and then branches to the routine to load the associated spell code 
															;block from disk.
;SPELL.FILE.ENTRANCE.BUFFER.SIZE	.EQ $xxx  ;#CONSTANT.
SPELL.FILE.ENTRANCE.BUFFER.END		.EQ $98FF  ;#CONSTANT.
SPELL.FILE.CODE_BLOCK.BUFFER.START.START		.EQ	SPELL.FILE.ENTRANCE.BUFFER.END+1 ;#CONSTANT. Memory address that the graphics effects for spells are read into from disk
										;$9900
SPELL.FILE.CODE_BLOCK.BUFFER.SIZE	.EQ $1B00  ;#CONSTANT.
SPELL.FILE.WEAPONS.SHAPE_TABLE.AUX.START	.EQ $D000 ;#CONSTANT. Location in aux memory where weapon shape tables are loaded. 



;****TEST TRANSFER


	; ;enable AUX memory (BSR: bank2 is already enabled)						
		; TSX			;transfer stack pointer to X-REG
		; STX TEMP	;save stack pointer
	; STA $C009 ;enable aux zero-page & aux BSR 

	; ;execute copy	
		; LDA #$00
		; STA COPY.FROM_START+$0
		; LDA #$D0
		; STA COPY.FROM_START+$1

		; LDA #$FF
		; STA COPY.FROM_END+$0
		; LDA #$DF
		; STA COPY.FROM_END+$1

		; LDA #$00
		; STA COPY.TO+$0
		; LDA #$10
		; STA COPY.TO+$1
		
		; ;COPY.TO+$1 is init before loop starts and it is incremented below
	; JSR MEMORY.COPY	
		
	; STA $C008 ;enable main zero-page & main BSR 
		; LDX TEMP	;restore stack pointer to X-REG
		; TXS ;transfer X-REG to stack pointer

		
			; LDA #$AB
			; JSR PREP.BRK
			; ; PLA
			; ; TAX
			; ; PLA
			; ; TAY
			; LDX PAGE.FOREGROUND
			; BRK


OpenDir.AUX_BSR.INIT ;COPY OpenDir TO AUX BSR:BANK1
@START
;COPY OpenDir FROM MAIN BSR:BANK1 TO AUX BSR:BANK1
	LDA $c08b		;enable bank-switched ram ($d000 bank 1)
	LDA $c08b
		
	;copy OpenDir to main memory temp location
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

	;copy OpenDir from main memory temp location to aux BSR
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
	
	;erase original OpenDir location in main BSR:bank1
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


@END



;======TEST PROGRAM VARIABLES======

;NOTE: The variables in this section are the only variables needed to call the driver
;the rest below were added to support the subroutines I copied over from my routines_general library, 
;which helped to construct the test scenario. 

;file.to.read .AZ #$01,/M/	;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 
;file.to.read .AZ #$04,/DATA/	;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 
file.to.read  .AZ #$0A,/DUMMY.FILE/	;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 
file.to.write .AZ #$0A,/DUMMY.FILE/	;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 
;DATA.SPR.SURF	.AZ #$0D,/DATA.SPR.SURF/	;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 
;DATA.SHP.SURF2 	.AZ #$0D,/DATA.SHP.SURF/	;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 


ZONE_TOOLS.INPUT_BUFFER .EQ $2000 ;PRODOS.IO has variable definitions which are pointers to this variable
;========ROUTEINES_GENERAL.ASM VARIABLES=========

PREP.BRK	.BS $1
COUT.ADDRESS .EQ $FDED	


WAIT.ADDRESS		.EQ	$FCA8
DELAY				.EQ	$FF				;#CONSTANT
APPLE_BELL.ADDRESS	.EQ $FF3A			

KB_BUFFER			.EQ	$C000
KB_BUFFER_ACK		.EQ $C010

AUX_MOVE		.EQ $C311
AUX_MOVE.START	.EQ $3C
AUX_MOVE.END	.EQ $3E
AUX_MOVE.DEST	.EQ	$42



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

TEMP16									.BS $02		;1byt


;ProRWTS 
;These are variables used by NOXARCH.SYSTEM, a file
;Peter built and installed on the bootloader. 

bleftlo   = $EF ;Bytes left in file. If no bytes have been read then it is the total bytes in the file. 
bleftho   = $F0
status    = $f3         ;returns non-zero on error
auxreq	  = $f4			;$00 = main memory, $01 = aux memory
sizelo    = $f5         ;must set if reading or writing. For writing, # of pages to write must be in multiples of 2. i.e. 2, 4, 6, 8 $0A, etc) 
sizehi    = $f6         ;must set if reading or writing.
reqcmd    = $f8         ;used if enable_write=1, 1=read, 2=write
ldrlo     = $f9         ;used if override_adr=1
ldrhi     = $fa         ;used if override_adr=1
namlo     = $fb
namhi     = $fc

ProRWTS		.EQ $D000	;entrance for opening a new file. $D003 is entrace for read/write/seek on file already open. 

;main memory storage for ProRWTS variables
;Note: this is because once PRODOS.IO enables aux BSR, aux zero-page is also enabled. So, the parms can't be set in zero-page before calling PRODOS.IO. Instead we set the parms in main memory and PRODOS.IO will copy them onto the aux zero-page. 

; ;(ALTERNATE) dedicated memory reservations
parm.bleftlo	.BS $1
parm.bleftho	.BS $1
parm.status		.BS $1
parm.auxreq		.BS $1
parm.sizelo		.BS $1
parm.sizehi		.BS $1
parm.reqcmd		.BS $1
parm.ldrlo 		.BS $1
parm.ldrhI 		.BS $1
parm.namlo 		.BS $1
parm.namhi 		.BS $1
parm.current.file .BS $1 ;if set to $01, then PRODOS.IO wrapper assumes it should open a new file via $D003 instead of using the current file via $D000
;
BSR.STATE 		.BS $1
BANK.STATE 		.BS $1
IO.ATTEMPTS		.BS $1




;constants
cmd_seek.current_drive  .EQ $90
cmd_read.current_drive  .EQ $91
cmd_write.current_drive .EQ $92
			
cmd_read.drive1			.EQ $1
cmd_read.drive2		  	.EQ $81
cmd_write.drive1		.EQ $2
cmd_write.drive2		.EQ $82




;======INCLUDE FILES======


;				.IN 	C:\MY_CODE\testing\prodos_testing\OpenDir.test.ASM
;				.IN 	C:\MY_CODE\TEMP\OpenDir.TEST.ASM
			
				;My libraries
				;.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_graphics				
				;.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_text
				;.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_math
;				.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_general
;				.IN 	C:\MY_CODE\INCLUDES_LIBS\offloaded_variables.ASM
				.IN 	C:\MY_CODE\INCLUDES_LIBS\ptools.ASM


;seperate target files	


;				.IN 	C:\MY_CODE\testing\prodos_testing\dummy.file.ASM					

				
;				.IN 	C:\MY_CODE\INCLUDES_LIBS\DATA.SHAPES.SURFACE
;				.IN 	C:\MY_CODE\INCLUDES_LIBS\DATA.SPR.SURFACE.ASM
;				.IN 	C:\MY_CODE\INCLUDES_LIBS\DATA.SHAPES.SURFACE
