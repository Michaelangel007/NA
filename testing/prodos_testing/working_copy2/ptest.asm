				.CR     6502            Use 6502 overlay
				.OR		$1000			**Always put before .TF directive and never use again in program
				.TF     TEST.BIN,BIN
				.EF		errors
				.LF		C:\MY_CODE\LIST
;				.LI 	OFF				Switches the assembler machine code conversion listing off so errors are easier to see				
;				.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_common


TEST.PROGRAM

;set destination memory address
	lda #$00
	sta ldrlo
	lda #$60
	sta ldrhi
	
;init prodos driver	
	jsr init

;set filename to read	
	lda #file.to.read		;load LO address
	sta namlo
	lda /file.to.read		;load HO address
	sta namhi

;read file	
	jsr opendir
	

	
; ;#2
		; ;set destination memory address
			; lda #$00
			; sta ldrlo
			; lda #$60
			; sta ldrhi
		
		; ;set filename to read	
			; lda #file.to.read		;load LO address
			; sta namlo
			; lda /file.to.read		;load HO address
			; sta namhi

		; ;read file	
			; jsr opendir

			
; ;#3			
		; ;set destination memory address
			; lda #$00
			; sta ldrlo
			; lda #$60
			; sta ldrhi

		; ;set filename to read	
			; lda #file.to.read		;load LO address
			; sta namlo
			; lda /file.to.read		;load HO address
			; sta namhi

		; ;read file	
			; jsr opendir
	
	brk	

file.to.read .AZ #$04,/DATA/	;ascii array, using LO values. Filename is limited to !15 characters
;FORMAT IS FILENAME LENGTH (BYTES), FILENAME. LENGTH IS RAW HEX NUMBER, NOT ASCII VALUE

;file.to.read .EQ $6100
	
;======INCLUDE FILES======

				 .IN 	C:\MY_CODE\temp\opendsk.ASM
				.IN 	C:\MY_CODE\INCLUDES_LIBS\DATA.SHAPES.SURFACE
	
				
;======DEFINE VARIBLES======

