                .CR     6502            Use 6502 overlay
				.OR		$2000			**Always put before .TF directive and never use again in program
				.TF     TEST.BIN,BIN
				.EF		errors
				;.LI 	OFF				Switches the assembler machine code conversion listing off so errors are easier to see				
				.LF		c:\my_code\na\build_folder\LIST

	LDA #$CD
	STA $C000
	
	LDA #$00
	
	LDA $C000
	BRK

	BRK
	