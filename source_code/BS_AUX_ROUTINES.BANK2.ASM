; ============================================
; Copyright (C) 2016-2018. 6502 Workshop, LLC
; contact: mark@6502workshop.com
; ============================================




;************************INCLUDE FILE*****************************
;(do not assemble stand-alone.  )
				.TF     BS_AUX.ROUTINES.BANK2.bin,BIN
				.OR		$D000			**Always put before .TF directive and never use again in program
				;.TF     graphics_scrolling.bin,BIN
				.EB		OFF

;=====BS_AUX_ROUTINES.BANK2.ASM DOCUMENTATION====================================
;
;The routines in this file are accessible from memory at all times except for
;	1) when the COMBAT module is loaded. COMBAT.LOADER loads weapon shape tables into AUX BSR bank2
;	   and COMBAT.EXIT restores the routines in this file to AUX BSR. 
;
;	2) when main BSR is loaded. i.e. before placing a routine in this file it's important
;	to verify that it doesn't access any routines in main BSR. 
;
;=================================================================================

BS_AUX_ROUTINES.BANK2.FILE_START

	; ;must come before BS_AUX_ROUTINES.BANK2.START

	; NOP	;this is a filler byte read in to open the file, before the seek/read routine which reads in
		; ;this file one page at a time. 
	
BS_AUX_ROUTINES.BANK2.COPY_TO.START
	
	.IN 	darkness_manager


		
		;**OPT** Disk. Reduce the .NO to the actual amount of memory needed for the routines in this file. 
	.NO $E200		;FILLER CAN BE RAISED TO THE UPPER LIMIT OF BANK SWITCHED MEMORY ($FFFF)

BS_AUX_ROUTINES.BANK2.FILE_END	

BS_AUX_ROUTINES.BANK2.FILE_SIZE = BS_AUX_ROUTINES.BANK2.FILE_END-BS_AUX_ROUTINES.BANK2.FILE_START			
