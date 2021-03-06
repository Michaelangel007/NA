; ============================================
; Copyright (C) 2016-2018. 6502 Workshop, LLC
; contact: mark@6502workshop.com
; ============================================


;************************INCLUDE FILE*****************************
;(do not assemble stand-alone.  )
				.TF     BS.ROUTINES.BANK1.bin,BIN
				.OR		$D000			**Always put before .TF directive and never use again in program
				;.TF     graphics_scrolling.bin,BIN
				.EB		OFF



				.IN 	animation_manager
				.IN 	graphics_scrolling
				
	.NO $E000,$AE	;FILLER TO THE UPPER LIMIT OF BSR $D000 BANK1.  
					;This enables SBASM to generate an error if/when the code overshoots this memory address.
					;It also kees this target file at a fixed size so the track/sector stays the same on the disk image, which is needed to load the file via MY.RWTS
					
