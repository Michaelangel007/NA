;************************INCLUDE FILE*****************************
;(do not assemble stand-alone. For a stand-alone version of this routine see examples/rwts2)
				.OR		$D700			**Always put before .TF directive and never use again in program
				;.TF     graphics_scrolling.bin,BIN
				.TF     BS.ROUTINES.BANK1.bin,BIN

				.IN 	C:\MY_CODE\INCLUDES_LIBS\animation_manager
				.IN 	C:\MY_CODE\INCLUDES_LIBS\graphics_scrolling
				;.IN 	C:\MY_CODE\INCLUDES_LIBS\tile_functions2
				
	.NO $E000,$AA	;FILLER TO THE UPPER LIMIT OF BSR $D000 BANK1.  
					;This enables SBASM to generate an error if/when the code overshoots this memory address.
					;It also kees this target file at a fixed size so the track/sector stays the same on the disk image, which is needed to load the file via MY.RWTS
					