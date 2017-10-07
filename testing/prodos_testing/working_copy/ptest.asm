				.CR     6502            Use 6502 overlay
				.OR		$6000			**Always put before .TF directive and never use again in program
				.TF     TEST.BIN,BIN
				.EF		errors
				.LF		C:\MY_CODE\LIST
;				.LI 	OFF				Switches the assembler machine code conversion listing off so errors are easier to see				
;				.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_common


TEST.PROGRAM

;set filename to read
	lda #$01
	sta file.to.read
	lda #$4D
	sta file.to.read+$1

;set destination memory address
	lda #$00
	sta ldrlo
	lda #$70
	sta ldrhi
	
	
	jsr init
	
	lda #file.to.read		;load LO address
	sta namlo
	lda /file.to.read		;load HO address
	sta namhi
	 
	jsr opendir
	
	brk	

;file.to.read .AZ /1M/	;ascii array, using LO values

file.to.read .EQ $6100
	
;======INCLUDE FILES======

				 .IN 	C:\MY_CODE\temp\opendsk.ASM
	
				
;======DEFINE VARIBLES======

