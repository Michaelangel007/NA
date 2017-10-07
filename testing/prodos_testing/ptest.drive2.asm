				.CR     6502            Use 6502 overlay
				.OR		$1000			**Always put before .TF directive and never use again in program
				.TF     TEST.BIN,BIN
				.EF		errors
				.LF		c:\my_code\na\build_folder\LIST
;				.LI 	OFF				Switches the assembler machine code conversion listing off so errors are easier to see				
;				.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_common


OpenDir.TEST.PROGRAM ;HARD DRIVE

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



; ;get file size
@START

; ;		lda #cmd_write.drive2		;write drive2
		; lda #cmd_read.drive2		;([$1=read drive1, | $2=write drive1], [$81=read drive2 | $82=write drive2])
		; sta reqcmd			
		
		; lda #$00
		; sta sizelo			;read length (LO byte)
		
		; lda #$00			;read length (HO byte)
		; sta sizehi
		
		; lda #$00			;read destination (LO byte)
		; sta ldrlo
		
		; lda #$A0			;read destination (HO byte) 
		; sta ldrhi
		
		; lda #file.to.read	;pointer to ascii array containing file name, with filename length byte header. 15 character max.
		; sta namlo
		
		; lda /file.to.read   ;pointer to ascii array containing file name, with filename length byte header. 15 character max.
		; sta namhi
		
		; LDA $c08b		;enable bank-switched ram ($d000 bank 1)
		; LDA $c08b
	
		; LDA #$00		;use main memory ($00 = main, $01 = aux)
		; STA AUXREQ
		; jsr $d003


			; ; LDA bleftlo
			; ; STA $4000
			; ; LDA bleftho
			; ; STA $4001
			
			; LDA $C082				;enable ROM, disable BSR
			; LDA STATUS
			; ; LDX #$AA
			; ; LDY #$AA
			; LDX bleftlo
			; LDY bleftho
			
			; BRK	
			
@END
			
; ;================
		
;=======direct access templates====
@START			
; ;open new file and write $1000 bytes
; ;file position start:	$0
; ;file position end:		$1000

		; lda #cmd_write.drive2	;([$1=read drive1, | $2=write drive1], [$81=read drive2 | $82=write drive2])
		; sta reqcmd
		
		; lda #$00				;write length (LO byte)
		; sta sizelo
		
		; lda #$01				;write length (HO byte)
		; sta sizehi
		
		; lda #$00				;write source data (LO byte)
		; sta ldrlo
		
		; lda #$A0				;write source data (HO byte)
		; sta ldrhi
		
		; lda #file.to.write		;pointer to ascii array containing file name, with filename length byte header. 15 character max.	
		; sta namlo
		
		; lda /file.to.write		;pointer to ascii array containing file name, with filename length byte header. 15 character max.
		; sta namhi
		
		; LDA $c08b		;enable bank-switched ram ($d000 bank 1)
		; LDA $c08b
	
		; LDA #$00		;Select memory ($00 = main, $01 = aux)
		; STA AUXREQ
		; jsr $d003				;entrance for open new file, read, write

			
; ;open new file and read $1000 bytes 
; ;file position start:	$0
; ;file position end:	$1000
		; lda #cmd_read.drive2		
		; sta reqcmd
		
		; lda #$00				;read length (LO byte)
		; sta sizelo
		
		; lda #$20				;read length (HO byte)
		; sta sizehi
		
		; lda #$00				;read destination address (LO byte)
		; sta ldrlo
		
		; lda #$70				;read destination address (HO byte)
		; sta ldrhi
		
		; lda #file.to.read		;pointer to ascii array containing file name, with filename length byte header. 15 character max.	
		; sta namlo
		
		; lda /file.to.read		;pointer to ascii array containing file name, with filename length byte header. 15 character max.
		; sta namhi
		
		; LDA $c08b		;enable bank-switched ram ($d000 bank 1)
		; LDA $c08b
	
		; LDA #$00		;Select memory ($00 = main, $01 = aux)
		; STA AUXREQ
		
		; jsr $d003				;entrance for open new file, read, write

		
; ;seek to position $2000 in opened file
; ;file position start:	$1000
; ;file position end:		$2000
		; lda #$00		 ;([$1=read drive1, | $2=write drive1], [$81=read drive2 | $82=write drive2]) ;do not specify drive2. This constant points to the drive1 cmdreq code, which tells the driver to use the drive specified when opening the file. 
		; sta reqcmd
		
		; lda #$00		;number of bytes to seek (LO byte)
		; sta sizelo
		
		; lda #$10		;number of bytes to seek (HO byte)
		; sta sizehi

		; LDA $c08b		;enable bank-switched ram ($d000 bank 1)
		; LDA $c08b

		; LDA #$00		;select memory (this may not be needed for a seek, but to be safe set it to the same value as specified when opening the file)
		; STA AUXREQ
		; jsr $d000

; ;read $1000 bytes to $8000 from opened file
; ;file position start:	$2000
; ;file position end:		$3000		
		; lda #cmd_read.current_drive ;([$1=read drive1, | $2=write drive1], [$81=read drive2 | $82=write drive2])
		; sta reqcmd
		
		; lda #$00				;read length (LO byte)
		; sta sizelo
		
		; lda #$10				;read length (HO byte)
		; sta sizehi
		
		; lda #$00				;read destination address (LO byte)
		; sta ldrlo
		
		; lda #$80				;read destination address (HO byte)
		; sta ldrhi
		
		; lda #file.to.read		;pointer to ascii array containing file name, with filename length byte header. 15 character max.	
		; sta namlo
		
		; lda /file.to.read		;pointer to ascii array containing file name, with filename length byte header. 15 character max.
		; sta namhi
		
		; LDA $c08b		;enable bank-switched ram ($d000 bank 1)
		; LDA $c08b
	
		; LDA #$00		;Select memory ($00 = main, $01 = aux)
		; STA AUXREQ

		; jsr $d000
				
			; LDA $C082				;enable ROM, disable BSR
			; LDA STATUS
			; LDX #$AA
			; LDY #$AA
			
			; BRK				
@END


; ;get file size
@START

; ;		lda #cmd_write.drive2		;write drive2
		; lda #cmd_read.drive2		;([$1=read drive1, | $2=write drive1], [$81=read drive2 | $82=write drive2])
		; sta reqcmd			
		
		; lda #$00
		; sta sizelo			;read length (LO byte)
		
		; lda #$00			;read length (HO byte)
		; sta sizehi
		
		; lda #$00			;read destination (LO byte)
		; sta ldrlo
		
		; lda #$A0			;read destination (HO byte) 
		; sta ldrhi
		
		; lda #file.to.read	;pointer to ascii array containing file name, with filename length byte header. 15 character max.
		; sta namlo
		
		; lda /file.to.read   ;pointer to ascii array containing file name, with filename length byte header. 15 character max.
		; sta namhi
		
		; LDA $c08b		;enable bank-switched ram ($d000 bank 1)
		; LDA $c08b
	
		; LDA #$00		;use main memory ($00 = main, $01 = aux)
		; STA AUXREQ
		; jsr $d003


			; ; LDA bleftlo
			; ; STA $4000
			; ; LDA bleftho
			; ; STA $4001
			
			; LDA $C082				;enable ROM, disable BSR
			; LDA STATUS
			; ; LDX #$AA
			; ; LDY #$AA
			; LDX bleftlo
			; LDY bleftho
			
			; BRK	
			
@END
			
; ;================
		
;=======direct access templates====
@START			
; ;open new file and write $1000 bytes
; ;file position start:	$0
; ;file position end:		$1000

		; lda #cmd_write.drive2	;([$1=read drive1, | $2=write drive1], [$81=read drive2 | $82=write drive2])
		; sta reqcmd
		
		; lda #$00				;write length (LO byte)
		; sta sizelo
		
		; lda #$01				;write length (HO byte)
		; sta sizehi
		
		; lda #$00				;write source data (LO byte)
		; sta ldrlo
		
		; lda #$A0				;write source data (HO byte)
		; sta ldrhi
		
		; lda #file.to.write		;pointer to ascii array containing file name, with filename length byte header. 15 character max.	
		; sta namlo
		
		; lda /file.to.write		;pointer to ascii array containing file name, with filename length byte header. 15 character max.
		; sta namhi
		
		; LDA $c08b		;enable bank-switched ram ($d000 bank 1)
		; LDA $c08b
	
		; LDA #$00		;Select memory ($00 = main, $01 = aux)
		; STA AUXREQ
		; jsr $d003				;entrance for open new file, read, write

			
; ;open new file and read $1000 bytes 
; ;file position start:	$0
; ;file position end:	$1000
		; lda #cmd_read.drive2		
		; sta reqcmd
		
		; lda #$00				;read length (LO byte)
		; sta sizelo
		
		; lda #$20				;read length (HO byte)
		; sta sizehi
		
		; lda #$00				;read destination address (LO byte)
		; sta ldrlo
		
		; lda #$70				;read destination address (HO byte)
		; sta ldrhi
		
		; lda #file.to.read		;pointer to ascii array containing file name, with filename length byte header. 15 character max.	
		; sta namlo
		
		; lda /file.to.read		;pointer to ascii array containing file name, with filename length byte header. 15 character max.
		; sta namhi
		
		; LDA $c08b		;enable bank-switched ram ($d000 bank 1)
		; LDA $c08b
	
		; LDA #$00		;Select memory ($00 = main, $01 = aux)
		; STA AUXREQ
		
		; jsr $d003				;entrance for open new file, read, write

		
; ;seek to position $2000 in opened file
; ;file position start:	$1000
; ;file position end:		$2000
		; lda #$00		 ;([$1=read drive1, | $2=write drive1], [$81=read drive2 | $82=write drive2]) ;do not specify drive2. This constant points to the drive1 cmdreq code, which tells the driver to use the drive specified when opening the file. 
		; sta reqcmd
		
		; lda #$00		;number of bytes to seek (LO byte)
		; sta sizelo
		
		; lda #$10		;number of bytes to seek (HO byte)
		; sta sizehi

		; LDA $c08b		;enable bank-switched ram ($d000 bank 1)
		; LDA $c08b

		; LDA #$00		;select memory (this may not be needed for a seek, but to be safe set it to the same value as specified when opening the file)
		; STA AUXREQ
		; jsr $d000

; ;read $1000 bytes to $8000 from opened file
; ;file position start:	$2000
; ;file position end:		$3000		
		; lda #cmd_read.current_drive ;([$1=read drive1, | $2=write drive1], [$81=read drive2 | $82=write drive2])
		; sta reqcmd
		
		; lda #$00				;read length (LO byte)
		; sta sizelo
		
		; lda #$10				;read length (HO byte)
		; sta sizehi
		
		; lda #$00				;read destination address (LO byte)
		; sta ldrlo
		
		; lda #$80				;read destination address (HO byte)
		; sta ldrhi
		
		; lda #file.to.read		;pointer to ascii array containing file name, with filename length byte header. 15 character max.	
		; sta namlo
		
		; lda /file.to.read		;pointer to ascii array containing file name, with filename length byte header. 15 character max.
		; sta namhi
		
		; LDA $c08b		;enable bank-switched ram ($d000 bank 1)
		; LDA $c08b
	
		; LDA #$00		;Select memory ($00 = main, $01 = aux)
		; STA AUXREQ

		; jsr $d000
				
			; LDA $C082				;enable ROM, disable BSR
			; LDA STATUS
			; LDX #$AA
			; LDY #$AA
			
			; BRK				
@END


; ;open new file and write $1000 bytes
; ;file position start:	$0
; ;file position end:		$1000

		; lda #cmd_write.drive2	;([$1=read drive1, | $2=write drive1], [$81=read drive2 | $82=write drive2])
		; sta reqcmd
		
		; lda #$00				;write length (LO byte)
		; sta sizelo
		
		; lda #$04				;write length (HO byte)
		; sta sizehi
		
		; lda #$00				;write source data (LO byte)
		; sta ldrlo
		
		; lda #$A0				;write source data (HO byte)
		; sta ldrhi
		
		; lda #file.to.write		;pointer to ascii array containing file name, with filename length byte header. 15 character max.	
		; sta namlo
		
		; lda /file.to.write		;pointer to ascii array containing file name, with filename length byte header. 15 character max.
		; sta namhi
		
		; LDA $c08b		;enable bank-switched ram ($d000 bank 1)
		; LDA $c08b
	
		; LDA #$00		;Select memory ($00 = main, $01 = aux)
		; STA AUXREQ
		; jsr $d003				;entrance for open new file, read, write


			; LDA $C082				;enable ROM, disable BSR
			; LDA STATUS
			; LDX #$AA
			; LDY #$AA
			
			; BRK
			
;open new file and read $1000 bytes 
;file position start:	$0
;file position end:	$1000
		lda #cmd_read.drive2		
		sta reqcmd
		
		lda #$03				;read length (LO byte)
		sta sizelo
		
		lda #$00				;read length (HO byte)
		sta sizehi
		
		lda #$00				;read destination address (LO byte)
		sta ldrlo
		
		lda #$70				;read destination address (HO byte)
		sta ldrhi
		
		lda #file.to.read		;pointer to ascii array containing file name, with filename length byte header. 15 character max.	
		sta namlo
		
		lda /file.to.read		;pointer to ascii array containing file name, with filename length byte header. 15 character max.
		sta namhi
		
		LDA $c08b		;enable bank-switched ram ($d000 bank 1)
		LDA $c08b
	
		LDA #$00		;Select memory ($00 = main, $01 = aux)
		STA AUXREQ
		
		jsr $d003				;entrance for open new file, read, write

		
		
;seek to position $2000 in opened file
;file position start:	$1000
;file position end:		$2000
		lda #$00		;do not specify drive2. This constant points to the drive1 cmdreq code, which tells the driver to use the drive specified when opening the file. 
		sta reqcmd
		
		lda #$FE		;number of bytes to seek (LO byte)
		sta sizelo
		
		lda #$00		;number of bytes to seek (HO byte)
		sta sizehi

		LDA $c08b		;enable bank-switched ram ($d000 bank 1)
		LDA $c08b

		LDA #$00		;select memory (this may not be needed for a seek, but to be safe set it to the same value as specified when opening the file)
		STA AUXREQ
		jsr $d000

;read $1000 bytes to $8000 from opened file
;file position start:	$2000
;file position end:		$3000		
		lda #$01 ;([$1=read drive1, | $2=write drive1], [$81=read drive2 | $82=write drive2])
		sta reqcmd
		
		lda #$03				;read length (LO byte)
		sta sizelo
		
		lda #$00				;read length (HO byte)
		sta sizehi
		
		lda #$00				;read destination address (LO byte)
		sta ldrlo
		
		lda #$70				;read destination address (HO byte)
		sta ldrhi
		
		; lda #file.to.read		;pointer to ascii array containing file name, with filename length byte header. 15 character max.	
		; sta namlo
		
		; lda /file.to.read		;pointer to ascii array containing file name, with filename length byte header. 15 character max.
		; sta namhi
		
		LDA $c08b		;enable bank-switched ram ($d000 bank 1)
		LDA $c08b
	
		LDA #$00		;Select memory ($00 = main, $01 = aux)
		STA AUXREQ

		jsr $d000

;seek to position $2000 in opened file
;file position start:	$1000
;file position end:		$2000
		lda #$00		;do not specify drive2. This constant points to the drive1 cmdreq code, which tells the driver to use the drive specified when opening the file. 
		sta reqcmd
		
		lda #$FD		;number of bytes to seek (LO byte)
		sta sizelo
		
		lda #$00		;number of bytes to seek (HO byte)
		sta sizehi

		LDA $c08b		;enable bank-switched ram ($d000 bank 1)
		LDA $c08b

		LDA #$00		;select memory (this may not be needed for a seek, but to be safe set it to the same value as specified when opening the file)
		STA AUXREQ
		jsr $d000

;read $1000 bytes to $8000 from opened file
;file position start:	$2000
;file position end:		$3000		
		lda #$01 ;([$1=read drive1, | $2=write drive1], [$81=read drive2 | $82=write drive2])
		sta reqcmd
		
		lda #$03				;read length (LO byte)
		sta sizelo
		
		lda #$00				;read length (HO byte)
		sta sizehi
		
		lda #$00				;read destination address (LO byte)
		sta ldrlo
		
		lda #$80				;read destination address (HO byte)
		sta ldrhi
		
		; lda #file.to.read		;pointer to ascii array containing file name, with filename length byte header. 15 character max.	
		; sta namlo
		
		; lda /file.to.read		;pointer to ascii array containing file name, with filename length byte header. 15 character max.
		; sta namhi
		
		LDA $c08b		;enable bank-switched ram ($d000 bank 1)
		LDA $c08b
	
		LDA #$00		;Select memory ($00 = main, $01 = aux)
		STA AUXREQ

		jsr $d000
		
			LDA $C082				;enable ROM, disable BSR
			LDA STATUS
			LDX #$AA
			LDY #$AA
			
			BRK

			
		
			
;********START PRODOS.IO WRAPPER TEMPLATES****

;COPY OpenDir TO AUX BSR
	JSR OpenDir.AUX_BSR.INIT

WRITE
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
	lda #cmd_write.drive1 ;([$1=read drive1, | $2=write drive1], [$81=read drive2 | $82=write drive2], [$90=seek current_drive, | $91=read current_drive, | $92=write current_drive])
	sta parm.reqcmd	
	
;set write data size (# of 512 byte blocks to write from memory)
	lda #$00 ;always #$00
	sta parm.sizelo

	lda #$02 ;number of pages to write. ;# of pages to write (LO byte). (must be in multiples of 2. i.e. 2, 4, 6, 8 $0A, etc)
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

SEEK
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
	lda #cmd_seek.current_drive
	sta parm.reqcmd	
	
;set # of bytes to seek
	lda #$00 ;always #$00
	sta parm.sizelo

	lda #$0E ;number of pages to write. ;# of pages to write (LO byte). (must be in multiples of 2. i.e. 2, 4, 6, 8 $0A, etc)
	sta parm.sizehi
	
		LDA #$00	;PARM: $00=main, $01=aux 
	JSR PRODOS.IO

@END
	
READ
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
	lda #$70
	sta parm.ldrhi

;set read length (bytes)
	lda #$FF				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda #$FF				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi
	
;set filename to read from	
	lda #file.to.read	;load LO address
	sta parm.namlo
	lda /file.to.read		;load HO address
	sta parm.namhi
	
		LDA #$00	;PARM: $00=main, $01=aux 
	JSR PRODOS.IO
	
@END

	LDA $C082				;enable ROM, disable BSR
	LDA STATUS
	LDX #$DD
	LDY #$AA
	BRK
	

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
