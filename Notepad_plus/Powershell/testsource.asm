                .CR     6502            Use 6502 overlay
				.OR		$6000			**Always put before .TF directive and never use again in program
				.TF     TEST.BIN,BIN
				.EF		errors
				;.LI 	OFF				Switches the assembler machine code conversion listing off so errors are easier to see				
				.LF		C:\MY_CODE\LIST
				
	LDA #$AA
	BRK
	