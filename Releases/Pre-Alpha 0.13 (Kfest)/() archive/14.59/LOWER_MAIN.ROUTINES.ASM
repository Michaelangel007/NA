;************************INCLUDE FILE*****************************
;(do not assemble stand-alone.  )
				.TF     LWR.MAIN.RTNS.BIN,BIN
				.OR		$0C00			**Always put before .TF directive and never use again in program
				.EB		OFF
				
;NOTE: This file is loaded into the main memory range $0C00-$1200


				.IN 	c:\my_code\includes_libs\routines_graphics				
				.IN 	c:\my_code\includes_libs\routines_math
				.IN 	c:\my_code\includes_libs\routines_general
				.IN 	c:\my_code\includes_libs\general_subroutines.asm
		
				;.IN 	C:\MY_CODE\INCLUDES_LIBS\sound_manager	
						
	.NO $2000		;FILLER TO THE UPPER LIMIT OF THE MEMORY RANGE ALLOCATED FOR THIS FILE 
					;This enables SBASM to generate an error if/when the code overshoots this memory address.
					;It also kees this target file at a fixed size so the track/sector stays the same on the disk image, which is needed to load the file via MY.RWTS
					