				.CR     6502            Use 6502 overlay
				.OR		$1000			**Always put before .TF directive and never use again in program
				.TF     TEST.BIN,BIN
				.EF		errors
				.LF		C:\MY_CODE\LIST
;				.LI 	OFF				Switches the assembler machine code conversion listing off so errors are easier to see				
;				.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_common


OpenDir.TEST.PROGRAM
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


.EXECUTE.READ_WRITE
@START
;set command type (READ | WRITE)
;	lda #$82 ;([$1=read drive1, | $2=write drive1], [$81=read drive2 | $82=write drive2])
	lda #$2 ;([$1=read drive1, | $2=write drive1], [$81=read drive2 | $82=write drive2])
	sta reqcmd	
	JSR DRIVER.WRITE

	; LDA $C082				;enable ROM, disable BSR
	; LDA STATUS
	; LDX #$DD
	; LDY #$AA
	; BRK
	

;set command type (READ | WRITE)
;	lda #$81 ;([$1=read drive1, | $2=write drive1], [$81=read drive2 | $82=write drive2])
	lda #$1 ;([$1=read drive1, | $2=write drive1], [$81=read drive2 | $82=write drive2])
	sta reqcmd
	JSR DRIVER.READ

@END

	LDA $C082				;enable ROM, disable BSR
	LDA STATUS
	LDX #$DD
	LDY #$AA
	BRK
	
	
	
;
;	
DRIVER.READ
@START

;set destination memory address
	lda #$00
	sta ldrlo
	lda #$70
	sta ldrhi

;set read length (bytes)
	lda #$FF				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta sizelo	
	lda #$FF				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta sizehi
	
;set filename to read from	
	lda #file.to.read	;load LO address
	sta namlo
	lda /file.to.read		;load HO address
	sta namhi

		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO

	
	RTS
@END

DRIVER.WRITE
@START
;set write data size (# of 512 byte blocks to write from memory)
	lda #$00 ;always #$00
	sta sizelo

	lda #$03 ;number of pages to write. (must be in multiples of 2. i.e. 2, 4, 6, 8 $0A, etc)
	sta sizehi


;set write buffer address (source data)
	lda #$00
	sta ldrlo
	lda #$A0
	sta ldrhi


;set filename to write to	

	lda #file.to.write	;load LO address
	sta namlo
	lda /file.to.write	;load HO address
	sta namhi

	; lda #file.to.write		;load LO address
	; sta namlo
	; lda /file.to.write		;load HO address
	; sta namhi
	
		LDA #$00	;PARM: $00=main, $01=aux 
	JSR PRODOS.IO
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
;SAVED.XREG.LOCAL2						.BS $01		;1byt
SAVED.XREG.GLOBAL1						.BS $01		;1byt
SAVED.XREG.LOCAL						.BS $01		;1byt
SAVED.XREG.LOCAL1 						.BS $01		;1byt
SAVED.ACC.GLOBAL1						.BS $01
SAVED.ACC.LOCAL							.BS $01
SAVED.ACC.LOCAL2						.BS $01
TEMP									.BS $01		;1byt

TEMP16									.BS $02		;1byt




;======INCLUDE FILES======


;				.IN 	C:\MY_CODE\testing\prodos_testing\OpenDir.test.ASM
;				.IN 	C:\MY_CODE\TEMP\OpenDir.TEST.ASM
			
				;My libraries
				;.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_graphics				
				;.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_text
				;.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_math
;				.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_general
				.IN 	C:\MY_CODE\INCLUDES_LIBS\ptools.ASM


;seperate target files	
				.IN 	C:\MY_CODE\testing\prodos_testing\dummy.file.ASM					
;				.IN 	C:\MY_CODE\INCLUDES_LIBS\DATA.SHAPES.SURFACE
;				.IN 	C:\MY_CODE\INCLUDES_LIBS\DATA.SPR.SURFACE.ASM
;				.IN 	C:\MY_CODE\INCLUDES_LIBS\DATA.SHAPES.SURFACE
