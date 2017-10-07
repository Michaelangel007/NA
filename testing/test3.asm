				.CR     6502            Use 6502 overlay
				.OR		$2000			**Always put before .TF directive and never use again in program
				.TF     TEST.BIN,BIN
				.EF		errors
				.LF		c:\my_code\na\build_folder\LIST
;				.LI 	OFF				Switches the assembler machine code conversion listing off so errors are easier to see				
;				.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_common


MY.PROGRAM

	JSR .TEST
	NOP
	NOP
	NOP
	NOP

.TEST
	NOP
	NOP
	PLA
	TAX
	PLA
	TAY

	BRK
	
;====DEFINE VARIABLES===
WRITE.HOPPER		.EQ $70B3	;address where custom boot1 code is store before it is written to disk. (i.e. the disk write looks for the code at this address)


ANIMATION.UPDATE .EQ $8000
GENERATE.DEBUG.LOG .EQ $8100


;====INCLUDE FILES======
				.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_graphics				
				.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_text
				.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_math
				.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_general
				.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_my.rwts

				.IN 	C:\MY_CODE\INCLUDES_LIBS\offloaded_variables2				
				.IN 	C:\MY_CODE\INCLUDES_LIBS\offloaded_variables

;SEPERATE TARGET FILE
				;.IN 	C:\MY_CODE\bootloader\OpenDir.ASM
				

