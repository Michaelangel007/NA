				.TF     noxarch.main.bin,BIN
				.OR		$1000			**Always put before .TF directive and never use again in program
				
				; .OR		$1000			**Always put before .TF directive and never use again in program
				; .TF     NOXARCH.MAIN.BIN,BIN
				; .EF		errors
				; .LF		C:\MY_CODE\LIST
; ;				.LI 	OFF				Switches the assembler machine code conversion listing off so errors are easier to see				
; ;				.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_common

;=====================SUBROUTINE DOCUMENTATION====================================
;
;This file, NOXARCH.MAIN, is the last file loaded by the booloader (NOX.SYSTEM).
;It appears that there is a size limit to how large NOXARCH.MAIN
;can be. I'm not sure exactly how large, but $3000 bytes was too much. 
;
;For this reason, all this file does is load LOADER.P, which is the primary
;game loader file. This load is done before the prodos driver is copied to aux BSR,
;so it still resides in main BSR:bank1. Accodingly, the zero page variables are set as 
;parameters instead of the main memory memory versions (i.e. use reqcmd not parm.reqcmd).
;
;=================================================================================

;****MAX MEMORY ADDRESS = $11FF.
;($1200 is used as temp location for loader zone header table)


		
NOXARCH.MAIN

		
			; LDA #$FF
			; STA $4E
			; STA $4F
	;JSR $FD1B ;wait for user to press a key (return not required)	

	
.STANDARD.INIT	
	; LDA $4F
	; ;STA SEED2
	; STA $B000
	
	; LDA $4E
	; ;STA SEED3
	; STA $B001

	; ;**FALLS THROUGH**

		; LDA $C082
		; LDA #$BB

		; ; LDX SEED2
		; ; LDY SEED3
		; LDX $B000
		; LDY $B000
		; BRK
		
		; LDA $C082
		; LDA #$AA
		; BRK
		
		;***NOTE: OpenDir not moved yet to aux BSR
	JSR NOXARCH.MAIN.READ

		; LDA $C082
		; LDX #$AA
		; BRK
		
	JMP GAME.LOADER1


NOXARCH.MAIN.READ
@START
;Note: This load is done before the prodos driver is copied to aux BSR,
;so it still resides in main BSR:bank1. Accodingly, the zero page variables are set as 
;parameters instead of the main memory memory versions (i.e. use reqcmd not parm.reqcmd).

;set command type (READ | WRITE)
	lda #$1 ;([$1=read drive1, | $2=write drive1], [$81=read drive2 | $82=write drive2])
	sta reqcmd

;set destination memory address
	lda #$00
	sta ldrlo
	lda #$20
	sta ldrhi

;set read length (bytes)
	; lda #$0				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	; sta sizelo	
	; lda #$10				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	; sta sizehi
	lda #$FF				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta sizelo	
	lda #$FF				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta sizehi
	
;set filename to read from	
	lda #LOADER.P		;load LO address
	sta namlo
	lda /LOADER.P		;load HO address
	sta namhi

	



		
		LDA $c08b		;enable bank-switched ram ($d000 bank 1)
		LDA $c08b
		
		LDA #$00	;PARM: $00=main, $01=aux
	JSR $D003
		LDA $C082		;enable ROM, disable BSR
		

		; LDA #$AA
		; ldx status
		; BRK


		
	RTS	
@END



;SUBROUTINES (LOCAL COPY)



;======DEFINE VARIBLES======

;none

;======(TEST) DEFINE VARIBLES======

;FORMAT IS FILENAME LENGTH (BYTES), FILENAME. LENGTH IS RAW HEX NUMBER, NOT ASCII VALUE

LOADER.P  .AZ #$08,/LOADER.P/	;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 


	
;======INCLUDE FILES======

;NONE

	.NO $1100				;ENSURES THAT THE PROGRAM CODE DOESN'T GET CLOBBERED BY THE INCLUDE TARGET FILES,



