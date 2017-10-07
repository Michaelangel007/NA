                .CR     6502            Use 6502 overlay
				.OR		$1000			**Always put before .TF directive and never use again in program
				;.OR		$2000			**Always put before .TF directive and never use again in program
				.TF     TEST.BIN,BIN
				.EF		errors
;				.IN 	C:\MY_CODE\source_code\routines_common
				.LF		c:\my_code\na\build_folder\LIST

	
;!!!!!!!!!WARNING: noxarchmain expected at $1000 (old location)

	lDA #$AA
	TAX
	TAY
	BRK
	
		   

	; LDA #$AA
	; LDX #COW+HIPPO
	; LDY /COW+HIPPO
	; BRK	
	
	; .NO $11FF

; COW
	; .BS $10, $BB
	; ;.NO $1220

; PIG
	; .BS $10, $CC


; HIPPO		.EQ $10

	
; COW
	; .DA #$56


; ;	.BS $1, .COMMAND_TABLE1.RECORDS.END-.COMMAND_TABLE1.RECORDS.START+1 ;+1 the byte quantity includes this byte that we are writing to the file with this directive. 

	; .DB 	.COMMAND_TABLE1.RECORDS.END-.COMMAND_TABLE1.RECORDS.START+1 ;+1 the byte quantity includes this byte that we are writing to the file with this directive. 
	
; .COMMAND_TABLE1.RECORDS.START

	; .bs $5
	
; .COMMAND_TABLE1.RECORDS.END


INV.TEXT_BLOCK.LEVEL	
; .KEYIN
	; LDA $C000
	; BPL .KEYIN
	; STA $C010
	
	; BRK
	
;LOCAL VARIABLES
@START

TEMP .BS $2
TEMP16 .BS $2

ZPAGE	.EQ $B0



@END


