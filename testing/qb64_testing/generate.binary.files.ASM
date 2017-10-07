                .CR     6502            Use 6502 overlay
				.OR		$6000			**Always put before .TF directive and never use again in program
				.TF     TEST.BIN,BIN
				.EF		errors
;				.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_common
				.LF		C:\MY_CODE\LIST

TEST.PROGRAM

	lda #$AF
	brk
	

	BRK
	
	;INCLUDES: SEPERATE TARGET FILE
				
				.IN 	C:\MY_CODE\testing\qb64_testing\data.tlk.M1.00.ASM				
				.IN 	C:\MY_CODE\testing\qb64_testing\data.tlk.M1.08.ASM					
				.IN 	C:\MY_CODE\testing\qb64_testing\data.tlk.M1.10.ASM				
