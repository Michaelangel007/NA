                .CR     65C02            Use 6502 overlay
				.OR		$6000			**Always put before .TF directive and never use again in program
				.TF     TEST.BIN,BIN
				.EF		errors
				;.LI 	OFF				Switches the assembler machine code conversion listing off so errors are easier to see				
				.LF		C:\MY_CODE\LIST

				
				
	LDA #dirbuf+$4
	
	BRK
	
	LDA #$AA
	INC
	STA TEST
	BRK
	
TEST .EQ $9000
dirbuf .EQ $1050
