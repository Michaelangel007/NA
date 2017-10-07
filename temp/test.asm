                .CR     6502            Use 6502 overlay
				.OR		$6000			**Always put before .TF directive and never use again in program
				.TF     TEST.BIN,BIN
				.EF		errors
;				.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_common
				.LF		C:\MY_CODE\LIST
	

;DEMO: division by powers of 2. Works like ASL does for multiplication

unrelocdsk 	.EQ $9000
reloc		.EQ $8000
	
MY.PROGRAM

unrdrvon2 .EQ	unrelocdsk+*-reloc


testlable 	.EQ 37
testlable2	.EQ *

		LDY #$00
		LDX testlable+256,Y

;unrdrvon2 .EQ	unrelocdsk+*-reloc

		LDA #$AA

		BRK
		
		
;======DEFINE VARIBLES======
; unrelocdsk 	.EQ $9000
; reloc		.EQ $1000