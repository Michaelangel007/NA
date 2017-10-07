;************************INCLUDE FILE*****************************
;(do not assemble stand-alone. For a stand-alone version of this routine see examples/rwts2)

				.OR		$1A04			**Always put before .TF directive and never use again in program
				.TF     LOADER.BIN,BIN


;=====================LOADER.ASM DOCUMENTATION====================================
;
;NOTE: boot time is currently 8 seconds (3/8/2016)
;
;This source code, Custom_Boot2.ASM and Game_Loop.ASM contains 4 target files (see below for list).
;SBASM creates a binary output file for each target file, each of which
;is loaded to the disk image the custom batch files (go1.bat, go2c.bat)
;via calls to AppleCommander. 
;
;The bulk of the game code is in GAME.BIN, which is incorporated via
;include files. 
;
;To be executable, the target files are require the BOOTLDR.DSK
;disk image, which contains custom boot loader code designed specifically
;to load these target files into memory without an operating system in memory.
;
;For detailed inforamtion on the custom boot loader, and on the game boot
;process, part of which is contained within these target files, see the
;following documentation (/my_code/documentation). They are presented
;in a good order to read, starting general and gradully diving deeper
;into the details, ultimately down to the level of describing the process
;of modifying the custom boot loader code on the disk image. 
;
;--Bootloader & Game Boot Process Documentation--
;Technical Overview.doc (Deep Under the Hood)
;Chart 0.0 (GAME BOOT)
;BootLoader.Code_summary
;BootLoader.Code_Walkthrough
;
;
;--INVENTORY OF TARGET FILES--
;(note: the .BIN is removed when the file is written to the disk image,
;(it exists only on the PC as a .BIN file, briefly, as the batch files
;delete the .BIN files as part of cleanup)
;
;GAME.BIN
;BS_ROUTINES.BANK1.BIN ;subroutines of GAME which are loaded into bank-switched RAM
;BS_ROUTINES.BANK2.BIN ;subroutines of GAME which are loaded into bank-switched RAM
;LOADER.BIN
;MY.RWTS.BIN
;
;
;--GLOSSARY--
;
;MO = MAP OBJECT
;MOB = MONSTER OR EVENTUALLY PROBABLY NPCs.
;NPC = NON-PLAYER CHARACTER
; 
;=================================================================================


GAME.LOADER1	
@START		

;=====================SUBROUTINES DOCUMENTATION====================================
;
;
;This program is responsible for:
;1) Move shape tables into AUX memory. 
;2) Load game files from disk into main and bank-switched memory. (the game files overrite the shape tables and this loader program in main memory
;	GOTHIC.SET				$0C00
;	CONT.HRCG				$0300
;	GAME	 				$6000-$B700
;	BS_ROUTINES.BANK1		$D000-$DFFF
;	BS_ROUTINES.BANK2		$D000-$FFFF
;3) Compress world map zone data, and load to aux memory. Map data included in this file as hex tables (output from map editor spreadsheet)


;--PROGRAM MEMORY MAP--
;$1D04 - $1FFF		Load shape tables, game files. Compress map data and load to aux memory.
;$2000 - $6FFF		HEX tables for shapes
;$7000 - $AFFF		HEX tables for world map
;$B300 - $B6FF		Temp memory for various copy routines (GOTHIC.SET, CONT.HRCG each have one)
;
;=================================================================================


	JMP LOAD.SHAPE.TABLES
	
DISPLAY.INTRO.TEXT
@START

	JSR CLEAR.TEXT.SCREEN

						
	LDA #LINE0 					;NOX ARCHAIST HEADER
	STA STRING
	
	LDA /LINE0
	STA STRING+$1			
	
	LDA #$0E
	STA HTAB	
	LDA #$00
	STA VTAB

		
	JSR UPDATE.CHAR.POS2

	JSR PRINT.STR2
	
						; LDA #$AA
						; LDX #$AA
						; LDY #$AA
						; BRK
						
						;BRK
				
	LDA #LINE0.1 				;(Copyright (C) 2016. 6502 Workshop, LLC)
	STA STRING
	
	LDA /LINE0.1
	STA STRING+$1			
	
	LDA #$00
	STA HTAB	
	LDA #$01
	STA VTAB
	
	JSR UPDATE.CHAR.POS2	
	JSR PRINT.STR2
	
	
	LDA #LINE1 					;RELEASE VERSION
	STA STRING
	
	LDA /LINE1
	STA STRING+$1	
	
	LDA #$0
	STA HTAB	
	LDA #$4
	STA VTAB

	JSR UPDATE.CHAR.POS2
	JSR PRINT.STR2

	LDA #LINE2 					;RELEASE NOTES
	STA STRING
	
	LDA /LINE2
	STA STRING+$1	
	
	LDA #$0
	STA HTAB	
	LDA #$6
	STA VTAB

	JSR UPDATE.CHAR.POS2
	JSR PRINT.STR2

	LDA #LINE3					;PRESS ANY KEY
	STA STRING
	
	LDA /LINE3
	STA STRING+$1	
	
	JSR PRINT.STR2
	
	JSR KEYIN2	
	CMP #$D3
	BEQ .START.GAME
	
	JSR CLEAR.TEXT.SCREEN


	LDA #LINE0 					;NOX ARCHAIST HEADER
	STA STRING
	
	LDA /LINE0
	STA STRING+$1			
	
	LDA #$0E
	STA HTAB	
	LDA #$00
	STA VTAB
	
	JSR UPDATE.CHAR.POS2	
	JSR PRINT.STR2

	LDA #LINE0.1 				;(Copyright (C) 2016. 6502 Workshop, LLC)
	STA STRING
	
	LDA /LINE0.1
	STA STRING+$1			
	
	LDA #$00
	STA HTAB	
	LDA #$01
	STA VTAB
	
	JSR UPDATE.CHAR.POS2	
	JSR PRINT.STR2
	
	LDA #$0						
	STA HTAB	
	LDA #$4
	STA VTAB
	JSR UPDATE.CHAR.POS2	
	
	LDA #LINE4						;COMMAND LIST
	STA STRING
	
	LDA /LINE4
	STA STRING+$1	
	
	JSR PRINT.STR2

	LDA #LINE5						;PLAYTEST KEYS
	STA STRING
	
	LDA /LINE5
	STA STRING+$1	
	
	JSR PRINT.STR2

	LDA #LINE6						;PRESS ANY KEY TO START
	STA STRING
	
	LDA /LINE6
	STA STRING+$1	
	
	JSR PRINT.STR2
	
	; LDA #LINE4
	; STA STRING
	
	; LDA /LINE4
	; STA STRING+$1	
	
	; JSR PRINT.STR2
	
	JSR KEYIN2	

.START.GAME

	JSR CLEAR.TEXT.SCREEN


	LDA #LINE0 					;NOX ARCHAIST HEADER
	STA STRING
	
	LDA /LINE0
	STA STRING+$1			
	
	LDA #$0E
	STA HTAB	
	LDA #$00
	STA VTAB
	
	JSR UPDATE.CHAR.POS2	
	JSR PRINT.STR2

	LDA #LINE0.1 				;(Copyright (C) 2016. 6502 Workshop, LLC)
	STA STRING
	
	LDA /LINE0.1
	STA STRING+$1			
	
	LDA #$00
	STA HTAB	
	LDA #$01
	STA VTAB
	
	JSR UPDATE.CHAR.POS2	
	JSR PRINT.STR2
	
	LDA #$0						
	STA HTAB	
	LDA #$4
	STA VTAB
	JSR UPDATE.CHAR.POS2	
	
	LDA #LINE7					;INSERT MAIN PLAYER DISK IN DRIVE 2
	STA STRING
	
	LDA /LINE7
	STA STRING+$1	
	
	JSR PRINT.STR2	
	
	JSR KEYIN2
	
	
@END

LOAD.SHAPE.TABLES
@START
;LOAD FILE "DATA.SHAPES.SURFACE"

	LDA #$01
    STA CMD.CODE
	LDA #$02
    STA DRIVE
		
    LDA #DATA.SHAPES.SURFACE1.START.TRACK	
    STA TRK.SRT    
    LDA #DATA.SHAPES.SURFACE1.START.SECTOR	
    STA SEC.SRT

	LDA #DATA.SHAPES.SURFACE1.END.TRACK
	STA TRK.END
    LDA #DATA.SHAPES.SURFACE1.END.SECTOR
    STA SEC.END

    LDA #$00
    STA DEST.ADDR					;LO
												
    LDA #$40
    STA DEST.ADDR+$1               	;HO

	LDA #$00						;TURN OFF DOS TRACK/SECTOR OVERRIDE
	JSR MY.RWTS

			
;=====COPY SHAPE TABLES TO AUX MEMORY====			
	LDA #$00			;SET START ADDRESS
	STA AUX_MOVE.START
	LDA #$40
	STA AUX_MOVE.START+$1
	
	LDA #$FF			;SET END ADDRESS
	STA AUX_MOVE.END
	LDA #$8F
	STA AUX_MOVE.END+$1
	
	LDA #SHP.TBL.START.ADDRESS		;SET DESTINATION ADDRESS
	STA AUX_MOVE.DEST
	LDA /SHP.TBL.START.ADDRESS
	STA AUX_MOVE.DEST+$1

	SEC                 ;SET CARRY FLAG DESGINATD MOVE FROM MAIN MEMORY -> AUX
	JSR AUX_MOVE



				; ;TROUBLESHOOTING HOOK
				; ;
				; LDA #$00							;START ADDRESS OF CURRENT ZONE
				; STA AUX_MOVE.START
				; LDA #$70	
				; STA AUX_MOVE.START+$1
				; ;
				; LDA #$FF							;END ADDRESS OF AUX MOVE WILL BE START ADDRESS OF NEXT ZONE
				; STA AUX_MOVE.END
				; LDA #$70					
				; STA AUX_MOVE.END+$1		
				; ;
				; LDA #$00							;SET DESTINATION ADDRESS
				; STA AUX_MOVE.DEST
				; LDA #$90
				; STA AUX_MOVE.DEST+$1
				; ;
				; CLC								;CLEAR CARRY FLAG DESGINATD MOVE FROM AUX -> MAIN MEMORY
				; JSR AUX_MOVE
				; ;
				; LDA TEXT
				; LDX #ZONE_TOOLS.OUTPUT_BUFFER
				; LDY /ZONE_TOOLS.OUTPUT_BUFFER
				; ; LDX #ZONE.LOOKUP.HO
				; ; LDY /ZONE.LOOKUP.HO
				; LDA TEXT
				; LDA $C082
				; BRK
				
@END

;LOAD GAME FILES

LOAD.GOTHIC.SET	
@START	
;LOAD FILE "GOTHIC.SET" (THIS IS THE HIGH-RES CHARACTER SET)

;NOTES
;
;***THE 4 EXTRA 4 BYTES AT START OF THE FILE (SEEMS ALL FILES HAVE THIS) PUSHES IT FROM 3 SECTORS INTO 4 SECTORS AND THUS 4 PAGES.
;HRCG CONTROLLER EXPECTS GOTHIC.SET TO START AT $0C00. TO ACCOMIDATE THIS AND KEEP THE CODE ENDING ON A PAGE
;BOUNDRY, THIS ROUTINE READS THE GOTHIC.SET DATA INTO A TEMP MEMORY LOCATION AND SHAVES OFF THE 
;EXTRA 4 BYTES PRE-PREPEND USING MEMORY.COPY. 


	;READ GOTHIC.SET FROM DISK TO TEMP MEMORY LOCATION 
	LDA #$01
    STA CMD.CODE
	LDA #$01
    STA DRIVE
		
    LDA #$03
    STA TRK.SRT    
    LDA #$01
    STA SEC.SRT

	LDA #$03
	STA TRK.END
    LDA #$04 
    STA SEC.END

    LDA #$00
    STA DEST.ADDR					;LO
												
    LDA #$B3
    STA DEST.ADDR+$1               	;HO
	
	LDA #$00						;TURN OFF DOS TRACK/SECTOR OVERRIDE	
	JSR MY.RWTS
	
	;COPY GOTHIC.SET FROM TEMP MEMORY TO PERMENANT MEMORY LOCATION
	LDA #$04
	STA COPY.FROM_START
	LDA #$B3
	STA COPY.FROM_START+$1
	LDA #$03
	STA COPY.FROM_END
	LDA #$B6
	STA COPY.FROM_END+$1
		
	LDA #$00
	STA COPY.TO
	LDA #$0C
	STA COPY.TO+$1

	LDY #$00				;LO BYTE COUNTER
;	COPY.FROM_START+$1		;DOUBLES AS HO BYTE COUNTER
.LOOP					
	LDA (COPY.FROM_START),Y
	STA (COPY.TO),Y
	LDA COPY.FROM_START+$1	
	CMP COPY.FROM_END+$1	;HAS HO BYTE COUNTER (COPY.FROM_START+$1) COUNTER REACHED FILL_END HO BYTE?
	BEQ .EXIT.TEST			;IF YES, CHECK TO SEE IF FILL_END LO BYTE HAS BEEN REACHED
.INCREMENT.COUNTER.LO_BYTE
	INY						;IF NO, INCREMENT LO BYTE COUNTER
	BNE .LOOP				;IF Y-REG HASN'T FLIPPED TO $00, CONTINUE LOOP
.INCREMENT.COUNTER.HO_BYTE		
	INC COPY.FROM_START+$1	;FILL_START+$1 DOUBLES AS THE HO_BYTE COUNTER
	INC COPY.TO+$1			
	JMP .LOOP
	
.EXIT.TEST
	CPY	COPY.FROM_END		;DOES Y-REG (LO_BYTE COUNTER) == COPY.FROM_END (THE LO BYTE OF END ADDRESS)?
	BNE .INCREMENT.COUNTER.LO_BYTE	;IF NO, INCREMENT COUNTER AND CONTINUE LOOP	
.COPY_DONE					;IF YES, THEN COPY IS DONE. 
	
@END

LOAD.CONT.HRCG	
@START
;LOAD FILE "CONT.HRCG" 

;(THIS IS THE CUSTOM OUTPUT CONTROLLER FOR HIGH-RES CHARACTER GENERATION)
;NOTE: There is a schmaz. CONT.HRCG doesn't occupy the entire page $300, and there is system critical
;code in the upper part of it. Or at least there is if DOS is loaded. In any event, the program hangs
;if I try to write an entire sector to $300. One solution may be to have RWTS read it to a different 
;memory region and use a memory copy routine to copy exactly the bytes needed to the $300 page. 
;
; 
;
	LDA #$01
    STA CMD.CODE
	LDA #$01
    STA DRIVE
		
    LDA #$03
    STA TRK.SRT    
    LDA #$06
    STA SEC.SRT

	LDA #$03
	STA TRK.END
    LDA #$06
    STA SEC.END

    LDA #$00
    STA DEST.ADDR					;LO
												
    LDA #$B3 						;***INTENTIONALLY SET THE WRONG ADDRESS. SEE NOTES ABOVE. 
    STA DEST.ADDR+$1               	;HO
	
	LDA #$00						;TURN OFF DOS TRACK/SECTOR OVERRIDE
	JSR MY.RWTS

	;COPY CONT.HRCG FROM TEMP MEMORY TO PERMENANT MEMORY LOCATION
	LDA #$04
	STA COPY.FROM_START
	LDA #$B3
	STA COPY.FROM_START+$1
	LDA #$91
	STA COPY.FROM_END
	LDA #$B3
	STA COPY.FROM_END+$1
		
	LDA #$00
	STA COPY.TO
	LDA #$03
	STA COPY.TO+$1	

	LDY #$00				;LO BYTE COUNTER
;	COPY.FROM_START+$1		;DOUBLES AS HO BYTE COUNTER
.LOOP					
	LDA (COPY.FROM_START),Y
	STA (COPY.TO),Y
	LDA COPY.FROM_START+$1	
	CMP COPY.FROM_END+$1	;HAS HO BYTE COUNTER (COPY.FROM_START+$1) COUNTER REACHED FILL_END HO BYTE?
	BEQ .EXIT.TEST			;IF YES, CHECK TO SEE IF FILL_END LO BYTE HAS BEEN REACHED
.INCREMENT.COUNTER.LO_BYTE
	INY						;IF NO, INCREMENT LO BYTE COUNTER
	BNE .LOOP				;IF Y-REG HASN'T FLIPPED TO $00, CONTINUE LOOP
.INCREMENT.COUNTER.HO_BYTE		
	INC COPY.FROM_START+$1	;FILL_START+$1 DOUBLES AS THE HO_BYTE COUNTER
	INC COPY.TO+$1			
	JMP .LOOP
	
.EXIT.TEST
	CPY	COPY.FROM_END		;DOES Y-REG (LO_BYTE COUNTER) == COPY.FROM_END (THE LO BYTE OF END ADDRESS)?
	BNE .INCREMENT.COUNTER.LO_BYTE	;IF NO, INCREMENT COUNTER AND CONTINUE LOOP	
.COPY_DONE					;IF YES, THEN COPY IS DONE. 

;PATCH CONT.HRCG
	LDA #$60				;For reasons I don't understand, when CONT.HCRG is run without DOS
	STA $03EA				;this patch is needed. My notes on this are in controller.hrcg.ASM.

@END


LOAD.DATA.OTHER	
@START
;LOAD FILE "DATA.OTHER.SUNRISE_SUNSET" 
;(note: overwrites the shape table data at $2000. The
;shape table data is already copied to aux memory so that's okay.)
;
	LDA #$01
    STA CMD.CODE
	LDA #$01
    STA DRIVE
		
    LDA #DATA.OTHER.SUNRISE_SUNSET.START.TRACK
;				LDA #$0B
	STA TRK.SRT    
    LDA #DATA.OTHER.SUNRISE_SUNSET.START.SECTOR
;				LDA #$06
    STA SEC.SRT

	LDA #DATA.OTHER.SUNRISE_SUNSET.END.TRACK	
;				LDA #$0B
	STA TRK.END
    LDA #DATA.OTHER.SUNRISE_SUNSET.END.SECTOR
;				LDA #$09
    STA SEC.END

    LDA #$00
    STA DEST.ADDR					;LO
												
    LDA #$40 						
    STA DEST.ADDR+$1               	;HO
	
	LDA #$00						;TURN OFF DOS TRACK/SECTOR OVERRIDE
	JSR MY.RWTS


		
;=====COPY SUNRISE_SUNSET STENCILS TO AUX MEMORY====			
	LDA #$00			;SET START ADDRESS
	STA AUX_MOVE.START
	LDA #$40
	STA AUX_MOVE.START+$1
	
	LDA #$FF			;SET END ADDRESS
	STA AUX_MOVE.END
	LDA #$43
	STA AUX_MOVE.END+$1
	
	LDA #DATA.OTHER.SUNRISE_SUNSET.AUX.START		;SET DESTINATION ADDRESS
	STA AUX_MOVE.DEST
	LDA /DATA.OTHER.SUNRISE_SUNSET.AUX.START
	STA AUX_MOVE.DEST+$1

	SEC                 ;SET CARRY FLAG DESGINATD MOVE FROM MAIN MEMORY -> AUX
	JSR AUX_MOVE
	
	
			; LDA TEXT
			; ; LDX #SCREEN.DARK.DATA
			; ; LDY /SCREEN.DARK.DATA 
			; LDA $C082
			; LDX AUX_MOVE.START
			; LDY AUX_MOVE.START+$1
			; BRK
			
@END

LOAD.WORLD.DATA
@START
;LOAD MAP DATA FROM DISK TO AUX MEMORY

;INITIAL VARIABLES & COUNTERS
		LDA #DATA.MAP.SURFACE.TOTAL.SECTORS	
		STA TOTAL.SECTORS
		;SETUP MY.RWTS VARIABLES;
		LDA #$02
		STA DRIVE
		;	
		LDA #DATA.MAP.SURFACE.START.TRACK
		STA TRK.SRT   
		STA TRK.SRT.COUNTER
		STA TRK.END
		STA TRK.END.COUNTER
		;
		LDA #DATA.MAP.SURFACE.START.SECTOR
		STA SEC.SRT
		STA SEC.SRT.COUNTER
		STA SEC.END
		STA SEC.END.COUNTER	
		;
		;SETUP AUX MEMORY DESTINATION ADDRESS
		LDA #WORLD.COMPRESS.AUX_MEMORY.START_LO	;SET DESTINATION ADDRESS/LO BYTE
		STA AUX.WRITE_ADDRESS.START			
		STA AUX_MOVE.DEST
		LDA #WORLD.COMPRESS.AUX_MEMORY.START_HO	;SET DESTINATION ADDRESS/HO BYTE
		STA AUX.WRITE_ADDRESS.START+$1
		STA AUX_MOVE.DEST+$1
		;
		LDA #NEW.MAP
		STA DISK.IO.BUFFER
		;
		LDA /NEW.MAP
		STA DISK.IO.BUFFER+$1
		;
;		LDA #$00		;don't skip any header sectors because the data to be read from disk was written to disk by My.RWTS
		LDA #$01		;skip 1 header sector because data was written to disk by AppleCommander
	JSR DISK.READ.AUX2
		;
		;
		DEC TOTAL.SECTORS ;decrement to exclue header sector, because that wasn't loaded into aux memory)
				
	JSR ZONE_TOOLS.BUILD.WZONE_HEADERS2

					
.COPY.ZONE.TABLE	
;COPY ZONE.LOOKUP.HO2/LO2 TO TEMP MEMORY LOCATION 
;	Note: this is so we can later retreive the data from the main game program. Any variables used by 
;	this loader program will get clobbered when the main game program is loaded into memory. 

	
				; TYA
				; LDX #ZONE.LOOKUP.LO2
				; LDY /ZONE.LOOKUP.LO2
				; LDA #$AF
				; LDA TEXT
				; BRK
				
	LDA #ZONE.LOOKUP.LO2
	STA COPY.FROM_START
	LDA /ZONE.LOOKUP.LO2
	STA COPY.FROM_START+$1
	
;-----16-BIT ADD
	LDA #ZONE.LOOKUP.LO2
	CLC
	ADC #WZONE.TOTAL.EXTENDED
	STA COPY.FROM_END
	LDA /ZONE.LOOKUP.LO2
	ADC #$00
	STA COPY.FROM_END+$1
;-----
		
	LDA #$00
	STA COPY.TO
	LDA #$12
	STA COPY.TO+$1

	JSR MEMORY.COPY2
	
;=======OLD METHOD=======
;(map data compressed by loader)
; ;LOAD FILE "DATA.MAP.SURFACE"
	;
	; LDA #$01
    ; STA CMD.CODE
	; LDA #$01
    ; STA DRIVE
	;	
    ; LDA #DATA.MAP.SURFACE1.START.TRACK	
    ; STA TRK.SRT    
    ; LDA #DATA.MAP.SURFACE1.START.SECTOR	
    ; STA SEC.SRT
	;
	; LDA #DATA.MAP.SURFACE1.END.TRACK
	; STA TRK.END
    ; LDA #DATA.MAP.SURFACE1.END.SECTOR
    ; STA SEC.END
	;
    ; LDA #$00
    ; STA DEST.ADDR					;LO
	;											
    ; LDA #$40
    ; STA DEST.ADDR+$1               	;HO
	;
	; LDA #$00						;TURN OFF DOS TRACK/SECTOR OVERRIDE
	; JSR MY.RWTS
	;
	;
	;			
;COMPRESS WORLD ZONE DATA AND LOAD INTO AUXILIARY MEMORY
;	JSR WORLD.COMPRESS2				;COMPRESS WORLD ZONE DATA AND COPY INTO AUX MEMORY. 
	;	
;COPY.ZONE.TABLE	
;COPY ZONE.LOOKUP.HO2/LO2 TO TEMP MEMORY LOCATION 
	;Note: this is so we can later retreive the data from the main game program. Any variables used by 
	;this loader program will get clobbered when the main game program is loaded into memory. 
;
;	
				; ; TYA
				; ; LDX #ZONE.LOOKUP.LO2
				; ; LDY /ZONE.LOOKUP.LO2
				; ; LDA #$AF
				; ; LDA TEXT
				; ; BRK
;				
	; LDA #ZONE.LOOKUP.LO2
	; STA COPY.FROM_START
	; LDA /ZONE.LOOKUP.LO2
	; STA COPY.FROM_START+$1
;	
; ;-----16-BIT ADD
	; LDA #ZONE.LOOKUP.LO2
	; CLC
	; ADC #WZONE.TOTAL.EXTENDED
	; STA COPY.FROM_END
	; LDA /ZONE.LOOKUP.LO2
	; ADC #$00
	; STA COPY.FROM_END+$1
; ;-----
;		
	; LDA #$00
	; STA COPY.TO
	; LDA #$12
	; STA COPY.TO+$1
;
	; JSR MEMORY.COPY2

	
				; ;TROUBLESHOOTING HOOK
				; ;
				; LDA #$00							;START ADDRESS OF CURRENT ZONE
				; STA AUX_MOVE.START
				; LDA #$02	
				; STA AUX_MOVE.START+$1
				; ;
				; LDA #$FF							;END ADDRESS OF AUX MOVE WILL BE START ADDRESS OF NEXT ZONE
				; STA AUX_MOVE.END
				; LDA #$02					
				; STA AUX_MOVE.END+$1		
				; ;
				; LDA #ZONE_TOOLS.OUTPUT_BUFFER		;SET DESTINATION ADDRESS
				; STA AUX_MOVE.DEST
				; LDA /ZONE_TOOLS.OUTPUT_BUFFER
				; STA AUX_MOVE.DEST+$1
				; ;
				; CLC
				; JSR AUX_MOVE						;CLEAR CARRY FLAG DESGINATD MOVE FROM AUX -> MAIN MEMORY
				; ;
				; LDA TEXT
				; LDX #ZONE_TOOLS.OUTPUT_BUFFER
				; LDY /ZONE_TOOLS.OUTPUT_BUFFER
				; ; LDX #ZONE.LOOKUP.HO2
				; ; LDY /ZONE.LOOKUP.HO2
				; LDA TEXT
				; LDA $C082
				; BRK
@END
			

LOAD.GAME
@START	
	
;LOAD FILE "GAME" (THIS IS THE MAIN GAME PROGRAM)
	LDA #$01
    STA CMD.CODE
	LDA #$02
    STA DRIVE
		
    LDA #$03
    STA TRK.SRT    
    LDA #$01
    STA SEC.SRT

	LDA #$08
	STA TRK.END
    LDA #$07
    STA SEC.END

    LDA #$00
    STA DEST.ADDR					;LO
												
    LDA #$60
    STA DEST.ADDR+$1               	;HO
	
	LDA #$00						;TURN OFF DOS TRACK/SECTOR OVERRIDE
	JSR MY.RWTS
@END

LOAD.BS_ROUTINES
@START

;LOAD FILE "BS_ROUTINES.BANK2"
	LDA #$01
    STA CMD.CODE
	LDA #$02
    STA DRIVE
		
    LDA #$08
    STA TRK.SRT    
    LDA #$09
    STA SEC.SRT

	LDA #$0B
	STA TRK.END
    LDA #$08
    STA SEC.END

    LDA #$00
    STA DEST.ADDR					;LO
												
    LDA #$D0
    STA DEST.ADDR+$1               	;HO
				
;ENABLE BANK-SWITCHED RAM ($D000 Bank 2)
	LDA $C083				;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK2 2nd)
	LDA $C083
	;The game code has BSR:bank2 enabled by default moving forward. ROM routines and BSR:Bank1 are enabled as-needed and then disabled. 
	
	LDA #$00						;TURN OFF DOS TRACK/SECTOR OVERRIDE
	JSR MY.RWTS

	
	
;LOAD FILE "BS_ROUTINES.BANK1"
	LDA #$01
    STA CMD.CODE
	LDA #$02
    STA DRIVE
		
    LDA #$1B
    STA TRK.SRT    
    LDA #$0D
    STA SEC.SRT

	LDA #$1C
	STA TRK.END
    LDA #$05
    STA SEC.END

    LDA #$00
    STA DEST.ADDR					;LO
												
    LDA #$D7
    STA DEST.ADDR+$1               	;HO
				
;ENABLE BANK-SWITCHED RAM ($D000 Bank 1)
	LDA $C08B					;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK1, 1ST)
	LDA $C08B

				
	LDA #$00						;TURN OFF DOS TRACK/SECTOR OVERRIDE
	JSR MY.RWTS

;ENABLE BANK-SWITCHED RAM ($D000 Bank 2)
	LDA $C083				;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK2 2nd)
	LDA $C083
		
;TRANSFER CONTROL			
	JMP GAME.LOADER2				;TRANSFER CONTORL TO THE MAIN GAME PROGRAM, WHICH STARTS WITH THE FINAL LOADER ROUTINE.

@END

;=====LIBRARY ROUTINES (COPY)=====
@START
ADC.16.2 ;==========ADDITION, 16-BIT=========
@START
;PARAMETERS: OP1(2), OP2(2)
;RETURN: RESULT(2)

;WARNING: IF USING THIS FUNCTION WITH AN 8-BIT VALUE (BECAUSE THE LOOP MAY PRODUCE A 16-BIT VALUE), YOU
;MUST SET OP1+$1 AND OP2+$2 TO $00 BEFORE CALLING ADC.16. USUALLY BEST TO DO THIS JUST BEFORE THE LOOP 
;STARTS DURING INIT OF OTHER VARIABLES.

;INIT VARIABLES
	LDA #$00
	STA RESULT
	STA RESULT+$01

; DO THE MATH ($A0F + $01)
	CLD 
    CLC                          ;ALWAYS BEFORE ADD
    LDA OP1
    ADC OP2
    STA RESULT
		 
    LDA OP1+$1
    ADC OP2+$1					;carry flag not cleared via CLC intentionally, it's part of 16-bit adding. 
    STA RESULT+$1
    BCS .ERROR

    RTS
.ERROR
	;ADC OVERFLOW ERROR
;DISABLE.BS_RAM
	LDA $C082				;READ ENABLE ROM, DISABLE WRITE ON BSM (NORMAL STATE).
							;REQUIRED FOR BEFORE BRK BECAUSE APPLE MONITOR IS A ROM ROUTINE
	
    BRK

@END	


;CLEAR.TEXT.SCREEN2
@START
;DECIDED TO USE ROM ROUTINE INSTEAD AT $FC58
	;
	; LDX #$0
	; ;STA HTAB.COUNTER	
	; LDY #$0
	; ;STA VTAB.COUNTER
	;
; .LOOP	
	; ;LDA HTAB.COUNTER
	; STY HTAB	
	; ;LDA VTAB.COUNTER
	; STX VTAB
	; JSR UPDATE.CHAR.POS2
	; LDA #$A0
	; JSR COUT.ADDRESS
	;
	; INX
	; CPX #$40
	; BNE .LOOP
	; LDX #$00
	; INY
	; CPY #$18
	; BNE .LOOP
	;
; RTS
	;
@END	
	

KEYIN2 ;============WAITS FOR A KEYPRESS=======
@START
;PARAMETERS: NONE
;RETURN VALUE: ACC (LAST KEY PRESS)

.KEYIN
	LDA KB_BUFFER
    BPL .KEYIN
    STA KB_BUFFER_ACK               ;CLR LAST KEY
	RTS
@END

MEMORY.COPY2
@START
;**OPT** Memory. There are 3 memory copies in this routine. They could get modified to use MEMORY.COPY2 below, which was
;added for the last one but is an exact copy of the MEMORY.COPY routine in the routines_general.asm include file. 	


.START

;SAVE REGISTERS	
	TYA
	PHA

	LDY #$00				;LO BYTE COUNTER
;	COPY.FROM_START+$1		;DOUBLES AS HO BYTE COUNTER
.LOOP					
	LDA (COPY.FROM_START),Y
	STA (COPY.TO),Y
	LDA COPY.FROM_START+$1	
	CMP COPY.FROM_END+$1	;HAS HO BYTE COUNTER (COPY.FROM_START+$1) COUNTER REACHED FILL_END HO BYTE?
	BEQ .EXIT.TEST			;IF YES, CHECK TO SEE IF FILL_END LO BYTE HAS BEEN REACHED
.INCREMENT.COUNTER.LO_BYTE
	INY						;IF NO, INCREMENT LO BYTE COUNTER
	BNE .LOOP				;IF Y-REG HASN'T FLIPPED TO $00, CONTINUE LOOP
.INCREMENT.COUNTER.HO_BYTE		
	INC COPY.FROM_START+$1	;FILL_START+$1 DOUBLES AS THE HO_BYTE COUNTER
	INC COPY.TO+$1			
	JMP .LOOP
	
.EXIT.TEST
	CPY	COPY.FROM_END		;DOES Y-REG (LO_BYTE COUNTER) == COPY.FROM_END (THE LO BYTE OF END ADDRESS)?
	BNE .INCREMENT.COUNTER.LO_BYTE	;IF NO, INCREMENT COUNTER AND CONTINUE LOOP	
.COPY_DONE					;IF YES, THEN COPY IS DONE. 

;RESTORE REGISTERS	
	PLA
	TAY
	
	RTS
@END
	
PRINT.STR2 ; ======OUTPUT ASCII STRING TO VIDEO DISPLAY, WITH CARRIAGE RETURN=====
@START
;PARAMETERS: STRING (HO/LO ADDRESS OF ASCII STRING TO PRINT)
;ENTRANCE: DIRECT
;RETURN VALUE: NONE

;NOTE: ascii string must use the LO values on the ascii chart
;in order to work with this subtroutine. SBASM uses the LO values
;when the .AZ directive is used.
;
;--LO Value Quick Reference--
;Carriage Return: $08

; ;DRIVER TEMPLATE
	;
	; LDA #LINE1 
	; STA STRING
	;
	; LDA /LINE1
	; STA STRING+$1			
	;
	;
	; JSR PRINT.STR
	
	LDY #$0				;init string index

	
.LOOP
	LDA (STRING),Y
	BEQ .EXIT			;if string stop value found, exit

;	CLC
;	ADC #$80			;convert from LO hex ascii value (used by SBASM) to HI hex ascii value (required by COUT ROM routine)
		 
	JSR COUT.ADDRESS	;***ASSUMES BSM IS DISABLED AND ROM ROUTINES ARE ENABLED
	INY					;increment string index
	BNE .LOOP

.EXIT     
	RTS
@END	
@END

DISK.READ.AUX2		;====READS FROM DISK TO AUX MEMORY, 1 PAGE AT A TIME====
@START
;PARAMETERS: ACC**, TOTAL.SECTORS, DRIVE, TRK.SRT, SEC.SRT, AUX.WRITE_ADDRESS.START (2), *DISK.IO.BUFFER (2)
;ENTRANCE: DIRECT
;RETURN:	
;*DISK.IO.BUFFER MUST POINT TO THE START OF A 1 PAGE MEMORY BLOCK. 
	;THIS MEMORY BLOCK ISN'T DEFINED BY THIS SUBROUTINE SO THAT OTHER MEMORY BLOCKS IN THE PROGRAM CALLING THIS 
	;SUBROUTINE CAN BE USED FOR DOUBLE DUTY. 
;**ACC: if ACC > $00, ACC = number of sectors to skip. An example of when sectors are skipped is when reading data written to disk by AppleCommander which writes a header. 

; ;DRIVER ==TEMPLATE==
; ;INITIAL VARIABLES & COUNTERS
	; LDA #DATA.SHAPES.BUILDING.TOTAL.SECTORS
	; STA TOTAL.SECTORS
	; ;
; ;SETUP MY.RWTS VARIABLES
	; LDA #$02
    ; STA DRIVE
	; ;	
    ; LDA #DATA.SHAPES.BUILDING.START.TRACK
    ; STA TRK.SRT   
	; STA TRK.SRT.COUNTER
	; STA TRK.END
	; STA TRK.END.COUNTER
	; ;
    ; LDA #DATA.SHAPES.BUILDING.START.SECTOR
    ; STA SEC.SRT
	; STA SEC.SRT.COUNTER
	; STA SEC.END
    ; STA SEC.END.COUNTER	
	; ;
; ;SETUP AUX MEMORY DESTINATION ADDRESS
	; LDA #SHP.TBL.START.ADDRESS			;SET DESTINATION ADDRESS/LO BYTE
	; STA AUX.WRITE_ADDRESS.START			
	; STA AUX_MOVE.DEST
	; LDA /SHP.TBL.START.ADDRESS			;SET DESTINATION ADDRESS/HO BYTE
	; STA AUX.WRITE_ADDRESS.START+$1
	; STA AUX_MOVE.DEST+$1
	; ;
	; LDA #NEW.SHAPES
	; STA DISK.IO.BUFFER
	; ;
	; LDA /NEW.SHAPES
	; STA DISK.IO.BUFFER+$1
	; ;
	; LDA #$01		;skipping $01 header sector because the data to be read from disk was written to disk by Apple Commander which adds a header. 
	; JSR DISK.READ.AUX	
	
	STA TEMP ;SAVE ACC PARAMETER
	
;SETUP REQUIRED MY.RWTS VARIABLES NOT PASSED AS PARAMETERS
	LDA #$01
    STA CMD.CODE

;SETUP TRACK/SECTOR COUNTERS		
    LDA TRK.SRT   
	STA TRK.SRT.COUNTER
	STA TRK.END.COUNTER
	STA TRK.END
	
    LDA SEC.SRT
	STA SEC.SRT.COUNTER
	STA SEC.END.COUNTER
	STA SEC.END

;SKIP ANY SECTORS?
	LDA TEMP					;LOAD ACC PARAMETER
	BEQ .LOOP.LOAD.ENTRANCE		;ACC PARAMETER DOESN'T APPLY UNLESS >0
	
	;ACC > 0, SO ACC = NUMBER OF SECTORS TO SKIP.
	
	STA SAVED.ACC.LOCAL
	LDX #$00			;LOOP COUNTER
.LOOP.SECTOR.SKIP
	
	INC SEC.SRT.COUNTER
	LDA SEC.SRT.COUNTER
	CMP #$10
	BCC .NO_FLIP1.1a
	INC TRK.SRT.COUNTER
	LDA TRK.SRT.COUNTER
	STA TRK.SRT
	LDA #$00
	STA SEC.SRT.COUNTER
.NO_FLIP1.1a
	STA SEC.SRT
	INC SEC.END.COUNTER
	LDA SEC.END.COUNTER
	CMP #$10
	BCC .NO_FLIP2.1a
	INC TRK.END.COUNTER
	LDA TRK.END.COUNTER
	STA TRK.END	
	LDA #$00
	STA SEC.END.COUNTER	
.NO_FLIP2.1a
	STA SEC.END	
	
;LOOP EXIT TEST	
	INX						;INCREMENT LOOP COUNTER, WHICH IS EQUAL TO THE # OF SECTORS SKIPPED
	CPX SAVED.ACC.LOCAL		;IS THE LOOP COUNTER EQUAL TO THE NUMBER OF SECTORS THE CALLING ROUTINE WANTS SKIPPED? (PASSED IN PARAMETER ACC)
	BNE .LOOP.SECTOR.SKIP	;IF NO, CONTINUE LOOP	
	
	;**FALLS THROUGH
	
.LOOP.LOAD.ENTRANCE
;INIT VARIABLES
	LDX #$00						;COUNTER:  TOTAL SECTORS READ. START AT $01 BECAUSE 1ST SECTOR IS GARBAGE DATA)

	STX COUNTER.RWTS
	STX COUNTER.LOOP
	STX COUNTER.AUX	

.LOOP.LOAD

;RWTS SECTION
	
;SKIP DOS RESERVED TRACKS/SECTORS
;Note: Applecommander skips track !17 ($11) and track !18, seconds 14 & 15 ($T12,$E & $D)
;I'm almost postive this is becasue AppleCommander is writing data to the disk assuming a DOS format.
;I'm almost postive these tracks/sectors are reserved by DOS. I recall reading somethig about it in Beneath DOS
;and specifically recall the reserve space being near the middle of the disk. It was for catalog information or something. 

;MUST INIT FOR EACH ITERATION
	LDA #$00
	STA DOS.SKIP.SECTOR				;LOAD FLAG USED BY RWTS WRAPPER ABOVE WHEN THE CURRENT ITERATION IS A SECTOR THAT DOS SKIPS
	
;DOS TRACK/SECTOR CHECKS	
	LDA TRK.SRT
	CMP #$11
	BEQ .SKIP.SECTOR			;SKIP WRITE THIS PASS, BUT INCREMENT TRACK/SECTOR. DON'T INCREMENT SECTOR COUNTER
	CMP #$12
	BNE .CONTINUE
	LDA SEC.SRT
	CMP #$0E
	BEQ .SKIP.SECTOR
	CMP #$0F
	BEQ .SKIP.SECTOR
	JMP .CONTINUE
	
.SKIP.SECTOR	
	LDA #$01
	STA DOS.SKIP.SECTOR  		;SET FLAG TO NOTIFY THE AUX ROUTINE TO SKIP THE WRITE FOR THIS SECTOR
	JMP .INCREMENT.TRACK_SECTOR
.CONTINUE	

	
	;my.rwts increments dest.addr so it must be set before each call
    LDA DISK.IO.BUFFER
    STA DEST.ADDR					;LO
												
    LDA DISK.IO.BUFFER+$1
    STA DEST.ADDR+$1               	;HO

				
;READ NEXT SECTOR FROM DISK

	INC COUNTER.RWTS
	
	LDA #$00						;TURN OFF DOS TRACK/SECTOR OVERRIDE
	JSR MY.RWTS

	INX 							;INCREMENT TOTAL SECTORS READ
									;NOTE: MUST COME BEFORE TRACK/SECTOR INCREMENT SO THAT DOS SKIPPED TRACKS CAN BYPASS THE SECTOR COUNTER INCREMENT, BUT STILL INCREMENT TRACK/SECTOR
	
.INCREMENT.TRACK_SECTOR			
;INCREMENT MY.RWTS COUNTERS	 (DONE AT FRONT END BECAUSE 1ST SECTOR IS GARBAGE DATA)
	INC SEC.SRT.COUNTER
	LDA SEC.SRT.COUNTER
	CMP #$10
	BCC .NO_FLIP1.1
	INC TRK.SRT.COUNTER
	LDA TRK.SRT.COUNTER
	STA TRK.SRT
	LDA #$00
	STA SEC.SRT.COUNTER
.NO_FLIP1.1
	STA SEC.SRT
	INC SEC.END.COUNTER
	LDA SEC.END.COUNTER
	CMP #$10
	BCC .NO_FLIP2.1
	INC TRK.END.COUNTER
	LDA TRK.END.COUNTER
	STA TRK.END	
	LDA #$00
	STA SEC.END.COUNTER	
.NO_FLIP2.1
	STA SEC.END
				

		
;SKIP AUX_MOVE? 
	LDA DOS.SKIP.SECTOR  		;LOAD FLAG USED BY RWTS WRAPPER ABOVE WHEN THE CURRENT ITERATION IS A SECTOR THAT DOS SKIPS
	CMP #$01					;IS FLAG SET?

				; STA TEMP
				; LDA TROUBLESHOOTING.HOOK
				; CMP #$01
				; BNE .TEMP2
				; LDA TEXT
				; LDA $C082
				; BRK
; .TEMP2
				; LDA TEMP
				; CMP $01						;IS FLAG SET?
				
				
	BNE .AUX_MOVE				;IF NO, PROCEED TO AUX MOVE.
;	JMP .LOOP.LOAD				;IF YES, SKIP AUX MOVE
	JMP .EXIT_TEST				;IF YES, SKIP AUX MOVE

	
;AUX MOVE SECTION

.AUX_MOVE
;INCREMENT AUX_MOVE COUNTERS	
	;AUX_MOVE start & end address zero page memory locations are not persistent and must be setup again before each call to AUX_MOVE
	LDA DISK.IO.BUFFER					
	STA AUX_MOVE.START					;SET START ADDRESS
	LDA DISK.IO.BUFFER+$1
	STA AUX_MOVE.START+$1

;16-BIT ADD	
	LDA DISK.IO.BUFFER
	CLC
	ADC #$FF	
	STA AUX_MOVE.END					;SET END ADDRESS
	LDA DISK.IO.BUFFER+$1
	ADC #$00
	STA AUX_MOVE.END+$1
		
	LDA AUX.WRITE_ADDRESS.START
	STA AUX_MOVE.DEST
	LDA AUX.WRITE_ADDRESS.START+$1
	STA AUX_MOVE.DEST+$1
			

;WRITE NEXT PAGE FROM AUX MEMORY
	INC COUNTER.AUX	
	
	SEC								;MAIN MEMORY -> AUX MEMORY
	JSR AUX_MOVE

	INC AUX.WRITE_ADDRESS.START+$1

		
	

.EXIT_TEST
	INC COUNTER.LOOP
	
	CPX TOTAL.SECTORS
	BEQ .EXIT
	JMP .LOOP.LOAD

.EXIT

		; LDA TEXT
		; LDA $C082
		; BRK
		
				; ;TROUBLESHOOTING HOOK
				; LDA TEXT
				; ; LDX #ZONE_TOOLS.OUTPUT_BUFFER
				; ; LDY /ZONE_TOOLS.OUTPUT_BUFFER
				; ; LDX #ZONE.LOOKUP.LO2
				; ; LDY /ZONE.LOOKUP.LO2
				; LDA TEXT
				; LDA $C082
				; BRK
	
	RTS
	
	
@END

ZONE_TOOLS.BUILD.WZONE_HEADERS2
@START	
;PARAMETERS: TOTAL.SECTORS
;ENTRANCE: DIRECT
;RETURN: ZONE.LOOKUP.LO2/HO
;
;*Set equal to TOTAL.SECTORS from JSR DISK.READ.AUX used to read
;the compressed data from disk.
;The value is the number of pages of aux memory the compressed data resides in, which is also the
;the same as the number of sectors the data takes up on disk, except for the header sector
;if the compressed data was written to disk by AppleCommander.
 

;=====================SUBROUTINE DOCUMENTATION====================================
;
;-Overview
;This subroutine is designed to build the lookup table which
;contains the starting aux memory address of each world zone (ZONE.LOOKUP.HO2/LO)
;
;It should be called right away after loading compressed map data
;from disk for a full sized world map (128x128 tiles, 64 zones)
;
;==DETAILED REVIEW==
;The basic approach of this routine is to copy two pages of zone data
;from aux memory to a main memory buffer, and then search for the
;$00 stop value at the end of each zone, setting the start address
;of the next zone equal to the address after a stop value is found.
;
;-The Search Pattern
;The search pattern is a little more complicated to take into consideration
;other uses of the value $00 in the tile_qty field (a compressed tile record is 2 bytes: tile qty, tile_type)
;
;The other uses are: $00 in tile_qty can also indicate !256 tiles,
;in which case candidate stop value +$2 is always $00 because it's the next stop value
;
;However, we can't conclude that candidate stop value +$2 isn't a real
;stop value because +$2 is = $00 because that pattern could result
;from a zone with !256 mountains. With it's stop value that zone's
;compressed data would be 00.00.00. Accordingly, the search pattern
;checks the next 3 bytes after any occurance of $00 before concluding
;it is a stop value. 
;
;See the first section in .INDEX.SEARCH_PASS1.LOOP & .INDEX.SEARCH_PASS2.LOOP
;for the code that implements the search pattern.
;
;-Multiple Pass Search
;The code is very complex because it is searching a data stream
;which spans multiple pages, and due to the search pattern required
;as described above, access to the first few bytes after the end of
;the current page is required. 
;
;As a result, the 8-bit index has to be reset during the search
;for a given page of data. To do this the search code was logically
;divided into PASS1 and PASS2, arbitrarily transitioning when the 
;counter reaches $80. Since the presense of stop values can alter whether
;the tile_qty is in an odd or even byte, the arbitrary break can
;cause the tile counter to become out of sync with tile_qty, since
;the tile counter starts at $00 in PASS2. To remedy this, COUNTER.ADJ
;is used to adjust the tile counter at the start of PASS2 if needed.
;
;-Zones with Compression Disabled
;The code for this is complicated. On the surface it seems 
;simple enough; there is a routine .ZONE.COMPRESSION.CHECK at
;the top of .OUTERLOOP which takes care of the lookup table updates
;for compression disabled zones since the the next zone's 
;start address is always 1 page ahead of the start address of 
;a compression disabled zone. 
; 
;One wrinkle is that if a compression disabled zone is found, the
;the stop value search in the next compression enabled zone must start
;at the last byte of the compresion disabled zone (which doesn't
;have a stop value, BTW). The issue is, normally the stop value
;search starts at X-REG = $00, the beginning of the first page
;of zone data in the buffer. But in the circumstances described, it
;needs to start somewhere in the middle...essentially the value of
;X-REG+1 when the stop value when the compression disabled zone 
;was found. This is managed through the variables X-REG.RESUME
;
;There is also code at the end of the PASS1/PASS2 stop value found
;routines (.STOP_VALUE_FOUND.PASS1/2) to check to see if the next
;zone has compression disabled because if so, the search routine 
;needs to terminate and let the .ZONE.COMPRESSION.CHECK routine 
;at the top of .OUTERLOOP take over. 
;
;There are just quite frankly a lot of values being passed around
;that have to be exactly correct at specific times in a code structure
;which is a bit intertwined. The value of ZONE.BASE.ADDRESS is another
;example. It gets affected by X-REG.RESUME if a compression disabled
;zone was found, but that means it isn't always affected. 
;
;If a compression disabled zone is found with a stop value at
;X-REG >= $80, then the next compression enabled zone needs
;to start the stop value search in PASS2.
;
;Lots of little quirks like this.
;
;Lesson learned: if possible avoid situations where a search
;must be done through multiple pages of data where not only the
;arrays don't have consistent lengths (the compressed zone data)
;but the records don't start/stop consistently on odd/even bytes,
;which introduces the problem of transitioning from one page of data 
;to the next. 
;
;=================================================================================

; ;======DRIVER TEMPLATE========
		; LDA #$14 		;TOTAL.SECTORS-1 (doesn't include header sector, because that wasn't loaded into aux memory)
		; STA TOTAL.SECTORS	



			; ;TROUBLESHOOTING HOOK
			; ;(set zone number hear, uncomment the 3 other hooks below, and execution will stop when zone # is found)
			; LDA #$FF			;SET ZONE # FOR TROUBLESHOOTING TRAP
			; STA TEMP16
			
			; ;TROUBLESHOOTING HOOK
			; ;(erase existing zone table)
			; LDX #$00
			
; .TEMP.LOOP
			; LDA #$AA
			; STA ZONE.LOOKUP.LO2,X
			; INX
			; CPX #$82
			; BNE .TEMP.LOOP
			
		
;=======INIT==========
@START


;INDEX THE ZONE DATA

	;INIT COUNTERS	
	LDX #$00							;outerloop (page/sector) counter, index.search.loop (tile record) index/counter, which must stay in sync with the first field in the 2 byte compressed tile record (tile_qty, tile_type)
	LDY #$00							;ZONE.LOOKUP TABLE INDEX
	
	;SETUP AUX MOVE	
	LDA #WORLD.COMPRESS.AUX_MEMORY.START_LO				;start address of current zone
	STA ZONE.LOOKUP.LO2,Y								;save first zone's address to lookup table, the loop will do the rest of the zones
	STA AUX.READ_ADDRESS.START
	
	LDA #WORLD.COMPRESS.AUX_MEMORY.START_HO				;save first zone's address to lookup table, the loop will do the rest of the zones
	STA ZONE.LOOKUP.HO2,Y
	STA AUX.READ_ADDRESS.START+$1
	STA AUX.READ_ADDRESS.END			;this isn't a 2 byte variable, because the LO byte of end address is always #$FF, which is hard coded. 
	INC AUX.READ_ADDRESS.END			;we're reading in two pages of aux memory at a time so that the search has access to the first few values after the end of a page when identifying the stop value. 
	
	INY									;next zone table record

				
;SETUP ZONE START ADDRESSES USED IN LOOKUP TABLE BUILD	
	LDA AUX.READ_ADDRESS.START			;the aux addresses are copied to the zone addresses in several locations. They are all needed. It has to do with the intertwined nature of the code. This particular instance in INIT is needed because the zone address values are used in .ZONE.COMPRESSION.CHECK	at the top of .OUTERLOOP and the other instances need to be placed further downstream in case .ZONE.COMPRESSION.CHECK changes the aux_memory values which in turn requries the zone address values to be updated. 
	STA ZONE.START.BASE_ADDRESS

	LDA AUX.READ_ADDRESS.START+$1
	STA ZONE.START.BASE_ADDRESS+$1	
	
	
;SETUP OTHER STUFF
	LDA #$00
	STA COUNTER.ADJ						;Used to sync up the value of the compressed tile record index between search PASS1 & PASS2
	STA PRIOR.ZONE.INDEX				;Aways == ZONE.LOOKUP.TABLE index -1. This because the index is used to record the zone start address, and the a zone's start address is automatically calcualted as (prior zone start address + 1 page) if the prior zone had compression disabled. PRIOR.ZONE.INDEX holds the index to the prior zone. 
	STA XREG.RESUME						;Used to carryover the compressed tile record index to the first compression enabled zone after a compression disabled zone has been found. 


@END
	
.OUTERLOOP.ENTRANCE
@START

.OUTERLOOP
	STX SAVED.XREG.LOCAL				;save outerloop (page/sector) counter, index.search.loop (tile record) index/counter,


;UPDATE ZONE START ADDRESSES USED IN LOOKUP TABLE BUILD	
;(to reflect the increments made in .EXIT_TEST.OUTERLOOP)
	LDA AUX.READ_ADDRESS.START
	STA ZONE.START.BASE_ADDRESS

	LDA AUX.READ_ADDRESS.START+$1
	STA ZONE.START.BASE_ADDRESS+$1		

;RESET COMPRESSED TILE RECORD
	LDX #$00
	STX SAVED.XREG.LOCAL1				;save compressed tile record index

			
		
.ZONE.COMPRESSION.CHECK	
@START
;(no need to search for zone stop values until the first compressed zone starts)	


;WAS PRIOR ZONE COMPRESSION DISABLED?
;(if yes then the start address of the current zone is automatically +1page from the last zone start address)
	LDX PRIOR.ZONE.INDEX				;= YREG -1. 
	LDA WZONE.COMPRESSION.FLAGS2, X
	BNE .LOAD.ZONE.DATA

	
;SET X-REG TO COMPRESSED TILE RECORD INDEX
	LDA #$00							;set x-reg = to xreg.resume, which is #$00 unless an uncompressed zone was found. 
	CLC
	ADC XREG.RESUME						;used to carryover the compressed tile record index if an uncompressed zone was found in the last iteration of SEARCH.INDEX.LOOP
	TAX
	STA ZONE.START.BASE_ADDRESS			;must be updated because we're about to use it to record a zone start address in this loop.
	STA SAVED.XREG.LOCAL1				;index was reset and saved above, so we need to override it so that when INDEX.SEARCH restores the index it will have the resume value, since a zone with compression disabled was found this iteration. 	
	LDX PRIOR.ZONE.INDEX				;restore needed because X-REG was modified a couple lines above

	LDA ZONE.START.BASE_ADDRESS			;record the start address of the current zone		
	STA ZONE.LOOKUP.LO2,Y
		
	LDA ZONE.START.BASE_ADDRESS+$1
	STA ZONE.LOOKUP.HO2,Y

			; ;TROUBLESHOOTING HOOK
			; CPY TEMP16
			; BNE .TEMP1
			; LDA TEXT
			; LDA $C082
			; ;LDY TOTAL.SECTORS
			; ;LDA AUX.READ_ADDRESS.START+$1
			; ;LDA NEW.MAP,X
			; ;LDA ZONE.LOOKUP.LO2,Y
			; ;LDY TEMP
			; ;
			; LDA ZONE.START.BASE_ADDRESS
			; ;LDX PRIOR.ZONE.INDEX					;= YREG -1. 
			; ;LDA WZONE.COMPRESSION.FLAGS2, X			
			; LDY ZONE.START.BASE_ADDRESS+$1
			; ;LDX AUX.READ_ADDRESS.START+$1
			; BRK
; .TEMP1 


	INC PRIOR.ZONE.INDEX				;= YREG -1. 
	INY									;increment zone lookup table index		
	INX									;increment WZONE.COMPREESSION.FLAGS index

	CPX #WZONE.TOTAL.PLUSONE				;Has last zone been processed?
	BNE .NEXT_ZONE						;if no, continue loop
	JMP .EXIT							;all zones are uncompressed, all zone indexes recorded. 
	
.NEXT_ZONE
	LDA WZONE.COMPRESSION.FLAGS2, X		;is next zone compression disabled?
	BNE .LOAD.ZONE.DATA					;if no, exit loop. No need to increment load counters because the next zone's stop value might be in the zone data already in memory. 

	
.INCREMENT.LOAD.COUNTERS
	INC AUX.READ_ADDRESS.START+$1		;increment aux move by 1 page/sector
	LDA AUX.READ_ADDRESS.START+$1
	STA ZONE.START.BASE_ADDRESS+$1		;need to be updated now because it is used right away at the top of the loop. The aux_end address won't be used until .LOAD.ZONE.DATA which takes care of the update. 

	INC AUX.READ_ADDRESS.END

	INC SAVED.XREG.LOCAL				;outerloop (page/sector) counter, index.search.loop (tile record) index/counter,
	JMP .ZONE.COMPRESSION.CHECK			;continue loop
	

@END

.LOAD.ZONE.DATA

;RESET ZONE START ADDRESSES USED IN LOOKUP TABLE BUILD
;(to reflect any increments made in .ZONE.COMPRESSION.CHECK)	
	
	LDA AUX.READ_ADDRESS.START
	STA ZONE.START.BASE_ADDRESS

	LDA AUX.READ_ADDRESS.START+$1
	STA ZONE.START.BASE_ADDRESS+$1			
		
	
;EXECUTE AUX MOVE (PAGE 1)
	LDA AUX.READ_ADDRESS.START			;set aux move start address
	STA AUX_MOVE.START
	
	LDA AUX.READ_ADDRESS.START+$1	
	STA AUX_MOVE.START+$1

			
	LDA #$FF							;set aux move end address
	STA AUX_MOVE.END					;this isn't a 2 byte variable, because the LO byte of end address is always #$FF, which is hard coded. 

	LDA AUX.READ_ADDRESS.END
	STA AUX_MOVE.END+$1			

	
	LDA #MAP.DATA.DEST_ADDR				;set destination address
	STA AUX_MOVE.DEST
	LDA /MAP.DATA.DEST_ADDR
	STA AUX_MOVE.DEST+$1

	
	CLC									;clear carry flag desginatd move from aux -> main memory
	JSR AUX_MOVE						;copy 2 pages of zone data from aux memory to main memory	


	
.INDEX.SEARCH.ENTRANCE
@START	
;CALCUALTE ZONE INDEXES IN CURRENT SECTOR

	LDX SAVED.XREG.LOCAL1				;restore compressed tile record index

			
	CPX #$80							;X-REG.RESUME was applied to the compressed tile record index, if appropriate, in .ZONE.COMPRESSION.CHECK. This CPX/branch is to determine whether the next search should start in PASS1 or PASS2. The later starts at byte $80 of the current page of zone data. 
	BCC	.INDEX.SEARCH_PASS1.LOOP.ENTRANCE
	
	;ADJUST X-REG COMRPESSED TILE INDEX
	TXA 								;PASS2 starts $80 bytes into the zone data in the current page, but x-reg is reset to $00. So, to resume x-reg at the position contemplated by XREG.RESUME, we have to subtract $80 first. 
	SEC
	SBC #$80
	TAX
	
	LDA ZONE.START.BASE_ADDRESS			;this adjustment makes PASS2 start $80 bytes in. This step is normally performed in the loops but we're dong it here since this is a shoe horn entrance to PASS2. 
	CLC
	ADC #$80 
	STA ZONE.START.BASE_ADDRESS
	
	LDA #$00							;**OPT** Memory. This reset may no longer me needed because after a stop value is found, this variable is now automatically set. But, if a stop value isn't found, maybe it being reset here (or somewhere) is still needed. 
	STA XREG.RESUME
	
	JMP .INDEX.SEARCH_PASS2.LOOP		;resume search at PASS2 because the compressed tile record index ix >= $80. PASS2 starts at byte $80 of the current page of zone data. 
	

.INDEX.SEARCH_PASS1.LOOP.ENTRANCE
;SYNC COMPRESSED TILE RECORD INDEX
;(if needed)	
	LDA COUNTER.ADJ							
	BEQ .INDEX.SEARCH_PASS1.LOOP
	INX

	LDA #$00
	STA COUNTER.ADJ
	STA XREG.RESUME						;**OPT** Memory. This reset may no longer me needed because after a stop value is found, this variable is now automatically set. But, if a stop value isn't found, maybe it being reset here (or somewhere) is still needed.


.INDEX.SEARCH_PASS1.LOOP
@START	
;SEARCH CURRENT SECTOR FOR ZONE DATA STOP VALUE ($00)
	LDA MAP.DATA.PASS1,X					;LOAD BYTE OF ZONE DATA FROM CURRENT SECTOR
	CMP #$00								;IS CURRENT BYTE == STOP VALUE?					
	BNE .NEXT_BYTE.PASS1_STEP
		LDA MAP.DATA.PASS1+$2,X				;check the next tile_qty byte, to make sure the current byte #$00 doesn't represent !256 tiles. 
		CMP #$00							;IS CURRENT BYTE == STOP VALUE?					
		BNE .STOP_VALUE_FOUND.PASS1			;$00.xx.xx (starting with current byte) pattern found, this is a stop value	
											;$00.xx.$00 (starting with current byte) pattern found, either a stop value or pseudo value for !256 tiles. If current byte is a stop value, then next tile pair is !256 mountains, which has the pattern $00.$00.$00...so if the current byte is a stop value, the current byte plus the next 3 bytes would be $00.$00.$00.$00. Accordingly, we need to test $x+1, $x+2 (already done) and $x+3
			LDA MAP.DATA.PASS1+$1,X			;CHECK THE NEXT (1) TILE_QTY BYTE.
			CMP #$00						;IS CURRENT BYTE == STOP VALUE?					
			BNE .NEXT_BYTE.PASS1_STEP		;since x+$1, +$2, +$3 are not all $00, then current byte is not a stop value.  
			LDA MAP.DATA.PASS1+$3,X			;CHECK THE NEXT (3) TILE_QTY BYTE.
			CMP #$00						;IS CURRENT BYTE == STOP VALUE?					
			BNE .NEXT_BYTE.PASS1_STEP		;since x+$1, +$2, +$3 are not all $00, then current byte is not a stop value.  
			JMP .STOP_VALUE_FOUND.PASS1		;$00.$00.$00.$00 (starting with current byte) pattern found, this is a stop value followed by !256 mountain tiles

.NEXT_BYTE.PASS1_STEP
	JMP .NEXT_BYTE.PASS1
	
.STOP_VALUE_FOUND.PASS1
;RECORD START ADDRESS OF NEXT ZONE
;(NEXT ZONE START = AUX.READ_ADDRESS.START + XREG +1.)
;(the +1 is because the XREG (stop value) of the last zone is 1 byte less than the start address of the next zone)
	LDA ZONE.START.BASE_ADDRESS				;=AUX.READ_ADDRESS.START		
	CLC
	INX										;+1	
	STX TEMP
	ADC TEMP								;+XREG
	BNE .NOFLIP								;A flip can occur if XREG is $FF. The +$1 is because the next zone starts 1 byte after the address of the stop value of the current zone. 
	INC ZONE.START.BASE_ADDRESS+$1			;in the even of a flip we need to temporarily increment the HO byte of the base address. 
.NOFLIP
	STA ZONE.LOOKUP.LO2,Y					;record start address of current zone
	STA	XREG.RESUME							;used to carryover x-reg value to start of next search loop if the next sector(s) have compression disabled. that would result in a load of the next page(s) of zone data, and we can't start searching for stop values at the beginning of the an uncompressed zone would end up somewhere in the middle. if the next zone has compression enabled then xreg.resume is discarded.
	DEX										;reverse the +1 to restore register to its original value at start of this code section
			
	LDA ZONE.START.BASE_ADDRESS+$1			;record start address of current zone
	STA ZONE.LOOKUP.HO2,Y
			
	LDA TEMP
	BNE .NO.FLIP.OCCURED
	DEC ZONE.START.BASE_ADDRESS+$1			;reversing increment above because .EXIT_TEST.OUTERLOOP automatically does an increment to the aux read address HO byte which will be saved to the zone address at the top of the outerloop.		
.NO.FLIP.OCCURED
			
			; ;TROUBLESHOOTING HOOK			
			; CPY TEMP16
			; BNE .TEMP2
			; LDA TEXT
			; LDA $C082
			; ;LDY TOTAL.SECTORS
			; ;LDA AUX.READ_ADDRESS.START+$1
			; ;LDA NEW.MAP,X
			; LDX #ZONE.LOOKUP.LO2,Y
			; LDY /ZONE.LOOKUP.LO2,Y
			; ;LDY TEMP
			; BRK
; .TEMP2 
	
	INY										;NEXT ZONE TABLE RECORD
	INC PRIOR.ZONE.INDEX						;= YREG -1. 

	CPY #WZONE.TOTAL.PLUSONE				;even if there are more sectors to read, exit if all zones have been processed. 
	BCS .EXIT_STEP
	JMP .CHECK.NEXT.ZONE
	
.EXIT_STEP
	JMP .EXIT
	
	
.CHECK.NEXT.ZONE
;IS NEXT ZONE ENABLED FOR COMPRESSION?
;(if not, no need to search for a stop value for this zone)
	STY SAVED.YREG.LOCAL					;save ZONE.LOOKUP.LO2/HO index
	
	LDY PRIOR.ZONE.INDEX					;= YREG -1. 		
	LDA WZONE.COMPRESSION.FLAGS2, Y			;is next zone compressed? 
	BNE .NOT_COMPRESSED1					;if no, use normal exit
	
	LDY SAVED.YREG.LOCAL					;restore ZONE.LOOKUP.LO2/HO index
	JMP .EXIT_TEST.OUTERLOOP				;the intention is to pass control to .ZONE.COMPRESSION.CHECK at the top of outerloop but the code goes through the exit_test so that the aux counters and page/sector counter get updated. 

	
.NOT_COMPRESSED1 ;normal exit	
	LDY SAVED.YREG.LOCAL						;restore ZONE.LOOKUP.LO2/HO index	
	JMP .NEXT_BYTE.PASS1.ALTERNATE.ENTRANCE		;if a stop value is found, the tile record index should only be incremented once to get baack in sync with the next record, because the stop value is added to the end of a record. 

		
.NEXT_BYTE.PASS1
;EXIT TEST INDEX.SEARCH.LOOP
	INX										;increment loop (tile record) counter
	CPX #$80								;exit test 1 of 2. the exit test is done in two stages because sometimes the loop counter is incremented once, sometimes twice, and either could result in a flip to $00. 
	BNE .NEXT_BYTE.PASS1.ALTERNATE.ENTRANCE	;loop until the counter flips to $00
	LDA #$01								
	STA COUNTER.ADJ							;used to sync the compressed tile record index. Since the search loop is terminating with only one INX, that means the added bytes of the stop values have caused the tile_qty field to start on an odd byte instead of an even byte, which means it is out of sync. Without this adjustment, PASS2 would start with the index at $00, which would mean it would be reading in the tile_type field instead of the tile_qty field. This would be very bad since the stop values are located in the tile_qty field.  
	JMP .NEXT.PASS							;exit this loop (index.search.loop), proceed to exit test for outer loop
.NEXT_BYTE.PASS1.ALTERNATE.ENTRANCE			;if a stop value is found, the tile record index should only be incremented once to get baack in sync with the next record, because the stop value is added to the end of a record. 
	INX										;record is 2 bytes, so two inx, except for right after finding a stop value.  
	CPX #$80								;exit test 2 of 2. the exit test is done in two stages because sometimes the loop counter is incremented once, sometimes twice, and either could result in a flip to $00. 
	BEQ .NEXT.PASS 	
	JMP .INDEX.SEARCH_PASS1.LOOP		;loop until the counter flips to $00

@END
	
.NEXT.PASS
;CLOSE OUT LOOP COUNTER, ADVANCING BASE ADDRESS FOR RESUMED SEARCH  
;(this is so we can include 1 page + 3 bytes in the search, which exceeds an 8-bit counter)
	TXA	;should contain #$80
	CLC
	ADC ZONE.START.BASE_ADDRESS
	STA ZONE.START.BASE_ADDRESS

;UPDATE COUNTERS AND INDEXES	
	LDX #$00							;rest loop counter (tile compressed record index)

	LDA COUNTER.ADJ						;sync the tile compression record index, if needed.
	BEQ .INDEX.SEARCH_PASS2.LOOP
	INX
	
	LDA #$00
	STA COUNTER.ADJ

.INDEX.SEARCH_PASS2.LOOP
@START

;SEARCH CURRENT SECTOR FOR ZONE DATA STOP VALUE ($00)
	LDA MAP.DATA.PASS2,X					;LOAD BYTE OF ZONE DATA FROM CURRENT SECTOR
	CMP #$00								;IS CURRENT BYTE == STOP VALUE?					
	BNE .NEXT_BYTE.PASS2
		LDA MAP.DATA.PASS2+$2,X				;CHECK THE NEXT TILE_QTY BYTE, TO MAKE SURE THE CURRENT BYTE #$00 DOESN'T REPRESENT !256 TILES. 
		CMP #$00							;IS CURRENT BYTE == STOP VALUE?					
		BNE .STOP_VALUE_FOUND.PASS2			;$00.xx.xx (starting with current byte) pattern found, this is a stop value	
											;$00.xx.$00 (starting with current byte) pattern found, either a stop value or pseudo value for !256 tiles. If current byte is a stop value, then next tile pair is !256 mountains, which has the pattern $00.$00.$00...so if the current byte is a stop value, the current byte plus the next 3 bytes would be $00.$00.$00.$00. Accordingly, we need to test $x+1, $x+2 (already done) and $x+3
			LDA MAP.DATA.PASS2+$1,X			;CHECK THE NEXT (1) TILE_QTY BYTE.
			CMP #$00						;IS CURRENT BYTE == STOP VALUE?					
			BNE .NEXT_BYTE.PASS2			;since x+$1, +$2, +$3 are not all $00, then current byte is not a stop value.  
			LDA MAP.DATA.PASS2+$3,X			;CHECK THE NEXT (3) TILE_QTY BYTE.
			CMP #$00						;IS CURRENT BYTE == STOP VALUE?					
			BNE .NEXT_BYTE.PASS2			;since x+$1, +$2, +$3 are not all $00, then current byte is not a stop value.  
			;**FALLS THROUGH**				;$00.$00.$00.$00 (starting with current byte) pattern found, this is a stop value followed by !256 mountain tiles
.STOP_VALUE_FOUND.PASS2
;RECORD START ADDRESS OF NEXT ZONE
;(NEXT ZONE START = AUX.READ_ADDRESS.START + XREG +1.)
;(the +1 is because the XREG (stop value) of the last zone is 1 byte less than the start address of the next zone)

	
	LDA ZONE.START.BASE_ADDRESS				;=AUX.READ_ADDRESS.START		
	CLC
	INX										;+1	
	STX TEMP
	ADC TEMP								;+XREG+$1
	BNE .NOFLIP2								;A flip can occur if XREG is $FF. The +$1 is because the next zone starts 1 byte after the address of the stop value of the current zone. 
	INC ZONE.START.BASE_ADDRESS+$1			;in the even of a flip we need to temporarily increment the HO byte of the base address. 
.NOFLIP2
	STA ZONE.LOOKUP.LO2,Y					;record current zone start address
	STA	XREG.RESUME							;used to carryover x-reg value to start of next search loop if the next sector(s) have compression disabled. that would result in a load of the next page(s) of zone data, and we can't start searching for stop values at the beginning of the an uncompressed zone would end up somewhere in the middle. if the next zone has compression enabled then xreg.resume is discarded.
	DEX										;REVERSE THE +1 TO RESTORE REGISTER TO ITS ORIGINAL VALUE AT START OF THIS CODE SECTION

			
	LDA ZONE.START.BASE_ADDRESS+$1			;record current zone start address
	STA ZONE.LOOKUP.HO2,Y					

	LDA TEMP
	BNE .NO.FLIP.OCCURED2
	DEC ZONE.START.BASE_ADDRESS+$1			;reversing increment above because .EXIT_TEST.OUTERLOOP automatically does an increment to the aux read address HO byte which will be saved to the zone address at the top of the outerloop.		
.NO.FLIP.OCCURED2

			; ;TROUBLESHOOTING HOOK
			; CPY TEMP16
			; BNE .TEMP3
			; LDA TEXT
			; LDA $C082
			; LDA ZONE.START.BASE_ADDRESS
			; LDY TEMP
			; ;LDY TOTAL.SECTORS
			; ;LDA AUX.READ_ADDRESS.START+$1
			; ;LDA NEW.MAP,X
			; ;LDA PRIOR.ZONE.INDEX
			; BRK
; .TEMP3


	INY										;NEXT ZONE TABLE RECORD
	CPY #WZONE.TOTAL.PLUSONE				;even if there are more sectors to read, exit if all zones have been processed. 
	BEQ .EXIT


	INC PRIOR.ZONE.INDEX					;= YREG -1. 
	
;IS NEXT ZONE ENABLED FOR COMPRESSION?
;(if not, no need to search for a stop value for this zone)
	STY SAVED.YREG.LOCAL					;save ZONE.LOOKUP.LO2/HO index
	
	LDY PRIOR.ZONE.INDEX					;= YREG -1. 		
	LDA WZONE.COMPRESSION.FLAGS2, Y			;is next zone compression disabled?
	BNE .NOT_COMPRESSED2					;if no, use normal exit
	
	LDY SAVED.YREG.LOCAL					;restore ZONE.LOOKUP.LO2/HO index
	JMP .EXIT_TEST.OUTERLOOP				;the intention is to pass control to .ZONE.COMPRESSION.CHECK at the top of outerloop but the code goes through the exit_test so that the aux counters and page/sector counter get updated. 

	
.NOT_COMPRESSED2 ; normal exit
	LDY SAVED.YREG.LOCAL						;restore ZONE.LOOKUP.LO2/HO index
	JMP .NEXT_BYTE.PASS2.ALTERNATE.ENTRANCE		;if a stop value is found, the tile record index should only be incremented once to get baack in sync with the next record, because the stop value is added to the end of a record. 

		
		
.NEXT_BYTE.PASS2
;EXIT TEST INDEX.SEARCH.LOOP
	INX										;increment loop (tile record) counter
	CPX #$80								;exit test 1 of 2. the exit test is done in two stages because sometimes the loop counter is incremented once, sometimes twice, and either could result in a flip to $00. 
	BNE .NEXT_BYTE.PASS2.ALTERNATE.ENTRANCE	;loop until the counter flips to $00
	LDA #$01
	STA COUNTER.ADJ							;see the line for COUNTER.ADJ in .NEXT_BYTE.PASS1. I don't recall why we need it here a second time as after PASS2 it should just get reset, but there may be a reason I don't recall. 
	JMP .EXIT_TEST.OUTERLOOP				;exit this loop (index.search.loop), proceed to exit test for outer loop
.NEXT_BYTE.PASS2.ALTERNATE.ENTRANCE			;if a stop value is found, the tile record index should only be incremented once to get baack in sync with the next record, because the stop value is added to the end of a record. 
	INX										;record is 2 bytes, so two inx, except for right after finding a stop value.  
	CPX #$80								;exit test 2 of 2. the exit test is done in two stages because sometimes the loop counter is incremented once, sometimes twice, and either could result in a flip to $00. 
	BEQ .EXIT_TEST.OUTERLOOP
	JMP .INDEX.SEARCH_PASS2.LOOP			;loop until the counter flips to $00
	
@END
@END
	
	
.EXIT_TEST.OUTERLOOP


		
	INC AUX.READ_ADDRESS.START+$1		;increment aux move by 1 page/sector
	INC AUX.READ_ADDRESS.END

	LDX SAVED.XREG.LOCAL				;load outerloop counter (page/sector)
	
	INX									;increment zone/page counter
	CPX TOTAL.SECTORS					;the # of sectors of map data read in == the number of pages of aux memory to read
	BCS .EXIT

	
	CPY #WZONE.TOTAL.PLUSONE			;even if there are more sectors to read, exit if all zones have been processed. 
	BCS .EXIT
	
	JMP .OUTERLOOP
@END

.EXIT
	CPY #WZONE.TOTAL.PLUSONE			;even if there are more sectors to read, exit if all zones have been processed. 
	BCC .ERROR


			; ;;TROUBLESHOOTING HOOK
			; LDA TEXT
			; LDA $C082
			; ;LDY TOTAL.SECTORS
			; ; LDX #ZONE.LOOKUP.LO2
			; ; LDY /ZONE.LOOKUP.LO2	
			; ;LDA #$AA			
			; BRK	
			
			
			
	RTS

.ERROR
;ERROR REPORTED BY ZONE_TOOLS.BUILD.WZONE_HEADERS.
;SUBROUTINES TERMINATED BECAUSE THE NUMBER OF SECTORS 
;SPECIFIED BY TOTAL.SECTORS WERE ALL PROCESSED, BUT 
;ALL ZONES WERE NOT PROCSSED. 
	LDA TEXT
	LDA $C082
	BRK
	
@END

WORLD.COMPRESS2 ; ======COMPRESS THE DATA FOR ALL ZONES AND STORE IN AUX MEMORY=====
@START

; ;SAVE REGISTERS
	; TYA
	; PHA
					
	
; ;PSEUDO CODE
; ;
; ;load first input zone base address (from label, WORLD.ZONE0)
	; ;save it to a ho/lo variable (input base address) WORLD.COMPRESS.ZONE_INPUT.ADDRESS
	; ;save to zone tools input (zero page) ZONE_TOOLS.INPUT
; ;load aux memory start address for world map zone data (from constant)	#WORLD.COMPRESS.AUX_MEMORY.START_LO/HO
	; ;save it to a ho/lo variable (output base address)	WORLD.COMPRESS.ZONE_OUTPUT.ADDRESS2
	; ;save to lookup table ho/lo ZONE.LOOKUP.HO, ZONE.LOOKUP.LO	
	
; ;loop
	; ;jsr compress.single
	; ;add return value in acc, add to output base address WORLD.COMPRESS.ZONE_OUTPUT.ADDRESS2
		; ;save to lookup table ho/lo ZONE.LOOKUP.HO, ZONE.LOOKUP.LO	
	; ;increment input base address/HO (zones are on page boundaries)	WORLD.COMPRESS.ZONE_INPUT.ADDRESS
		; ;;save to zone tools input (zero page)	ZONE_TOOLS.INPUT
	; ;do loop
; ;INIT VARIABLES
	; LDY #$00											;ZONE LOOKUP TABLE INDEX

; ;INPUT: load first input zone base address
	; LDA #WORLD.ZONE_0
	; STA WORLD.COMPRESS.ZONE_INPUT.ADDRESS2				;THIS WILL BE THE INPUT BASE ADDRESS COUNTER
	; STA ZONE_TOOLS.INPUT2								;SAVE TO INPUT POINTER FOR COMPRESSION ROUTINE	

	; LDA /WORLD.ZONE_0
	; STA WORLD.COMPRESS.ZONE_INPUT.ADDRESS2+$1
	; STA ZONE_TOOLS.INPUT2+$1


; ;OUTPUT: load aux memory start address for world map zone data
	; LDA #WORLD.COMPRESS.AUX_MEMORY.START_LO2
	; STA WORLD.COMPRESS.ZONE_OUTPUT.ADDRESS2				;THIS WILL BE THE OUTPUT BASE ADDRESS COUNTER
	; STA ZONE.LOOKUP.LO2,Y								;SAVE FIRST ZONE'S ADDRESS TO LOOKUP TABLE, THE LOOP WILL DO THE REST OF THE ZONES
		
	; LDA #WORLD.COMPRESS.AUX_MEMORY.START_HO2
	; STA WORLD.COMPRESS.ZONE_OUTPUT.ADDRESS2+$1	
	; STA ZONE.LOOKUP.HO2,Y
	

	

; .LOOP.COMPRESS2
; ;PSEUDO, BRANCH BASED ON WHETHER ZONE SHOULD BE COMPRESSED	
	; LDA WZONE.COMPRESSION.FLAGS2,Y
	; CMP #$01							;#$01 = COMPRESSION ON
	; BNE .LEAVE.UNCOMPRESSED

	
	; JSR ZONE_TOOLS.COMPRESS.SINGLE2	

	; INY													;INCREMENT LOOKUP TABLE INDEX
		
	; ;INCREMENT OUTPUT BASE ADDRESS
; ;(this will be the location on aux memory that the compressed data for the next zone will be written)


	; STA OP1												;ACC IS RETURN VALUE FROM JSR ZONE_TOOLS.COMPRESS.SINGLE. IT CONTAINS THE OUTPUT ARRAY SIZE (LAST ELEMENT DATA WAS WRITTNE TO)
	; LDA #$00
	; STA OP1+$1
	; LDA WORLD.COMPRESS.ZONE_OUTPUT.ADDRESS2
	; STA OP2
	; LDA WORLD.COMPRESS.ZONE_OUTPUT.ADDRESS2+$1
	; STA OP2+$1
	
	; JSR ADC.16.2
	
	; LDA RESULT
	; STA WORLD.COMPRESS.ZONE_OUTPUT.ADDRESS2
	; STA ZONE.LOOKUP.LO2,Y
	
	; LDA RESULT+$1
	; STA WORLD.COMPRESS.ZONE_OUTPUT.ADDRESS2+$1
	; STA ZONE.LOOKUP.HO2,Y
	; JMP .COMMON.CODE

	
; .LEAVE.UNCOMPRESSED	
						; ; LDA #$AF
						; ; LDX WZONE.COMPRESSION.FLAGS2,Y
						; ; BRK

	; JSR ZONE_TOOLS.WCOPY2								;COPY ZONE DATA UNCOMPRESSED FROM MAIN MEMORY TO AUX MEMORY
	
	; LDA ZONE.LOOKUP.LO2,Y								;THE LO BYTE DOESN'T CHANGE SINCE ALL ZONES ARE 1 PAGE IN SIZE
	; INY													;INCREMENT LOOKUP TABLE INDEX
	; STA ZONE.LOOKUP.LO2,Y
	; INC WORLD.COMPRESS.ZONE_OUTPUT.ADDRESS2+$1			;INCREMENT HO BYTE OF THE VARIABLES USED FOR AUX MEMORY DESTINATION ADDRESS
	; LDA WORLD.COMPRESS.ZONE_OUTPUT.ADDRESS2+$1
	; STA ZONE.LOOKUP.HO2,Y
	; ;**FALLS THROUGH
	

; .COMMON.CODE	;Code used by both the compress and leave uncompressed routines above
; ;INCREMENT INPUT BASE ADDRESS
; ;(This will be the address of the uncompressed data for the next zone)

	; INC WORLD.COMPRESS.ZONE_INPUT.ADDRESS2+$1			;THE UNCOMPRESSED ZONE DATA BEINGS ON PAGE BOUNDARIES. 
	; LDA WORLD.COMPRESS.ZONE_INPUT.ADDRESS2+$1
	; STA ZONE_TOOLS.INPUT2+$1							;UPDATE THE INPUT POINTER FOR COMPRESSION ROUTINE
				
; ;EXIT TEST
	; CPY #WZONE.TOTAL2									;HAS LAST ZONE BEEN COMPRESSED?
	; BNE .LOOP.COMPRESS2									;IF NO, CONTINUE LOOP
				
				; ; TYA
				; ; LDX #ZONE.LOOKUP.LO2
				; ; LDY /ZONE.LOOKUP.LO2
				; ; BRK
				
; ;RESTORE REGISTERS
	; PLA
	; TAY
		
	; RTS			
@END

ZONE_TOOLS.WCOPY2
@START
; ;SAVE REGISTERS
	; TXA
	; PHA
	; TYA
	; PHA

; ;THE START AND OUTPUT ADDRESS FOR THE AUX MOVE ARE KNOWN AT THE START OF THIS ROUTINE 
; ;AS THEY ARE CALCULATED IN WORLD.COMPRESS2
; ;
; ;NO STOP VALUE IS ADDED TO THE END SINCE LEAVING THE DATA UNCOMRPESSED MEANS THAT IT'S EXACT SIZE (1 PAGE) IS KNOWN TO THE UNCOMPRESS ROUTINE. 

; ;CALCULATE THE END ADDRESS FOR THE AUX MOVE	(SAVE VALUES DIRECTLY TO THE AUX MOVE VARIABLES)
	; LDA WORLD.COMPRESS.ZONE_INPUT.ADDRESS2
	; STA OP1
	; LDA WORLD.COMPRESS.ZONE_INPUT.ADDRESS2+$1
	; STA OP1+$1
	
	; LDA #$FF
	; STA OP2
	; LDA #$00
	; STA OP2+$1
	
	; JSR ADC.16.2									;WORLD.COMPRESS.ZONE_INPUT.ADDRESS2(2) + #$FF
	
	; LDA RESULT										;SET END ADDRESS
	; STA AUX_MOVE.END
	; LDA RESULT+$1
	; STA AUX_MOVE.END+$1
	
	
	; SEC               							  	;SET CARRY FLAG DESGINATD MOVE FROM MAIN MEMORY -> AUX
	; LDA WORLD.COMPRESS.ZONE_INPUT.ADDRESS2			;SET START ADDRESS
	; STA AUX_MOVE.START
	; LDA WORLD.COMPRESS.ZONE_INPUT.ADDRESS2+$1		
	; STA AUX_MOVE.START+$1

	; LDA WORLD.COMPRESS.ZONE_OUTPUT.ADDRESS2			;SET DESTINATION ADDRESS
	; STA AUX_MOVE.DEST
	; LDA WORLD.COMPRESS.ZONE_OUTPUT.ADDRESS2+$1
	; STA AUX_MOVE.DEST+$1


	; JSR AUX_MOVE


; ;RESTORE REGISTERS
	; PLA
	; TAY
	; PLA
	; TAX
	
	; RTS
@END

;ZONE_TOOLS.COMPRESS.SINGLE2 ; ===COMPRESS ZONE AND COPY TO AUX MEMORY===
@START
;PARAMETERS: ZONE_TOOLS.INPUT(2)*1, WORLD.COMPRESS.ZONE_OUTPUT.ADDRESS2(*2)
;RETURN VALUE: ACC (the output array index to the last element written+1)
;ENTRANCE: DIRECT

;*1: THE HO/LO ADDRESS OF THE UNCOMPRESSED ZONE INPUT ARRAY
;*2: THE AUX MEMORY HO/LO ADDRESS TO WRITE THE COMPRESSED DATA TO

;DOCUMENTATION
;The compressed record format is:
;Tile_Qty, Tile_Type
;
;If Tile_QTY = $00, that stands for !256 tiles. 


; ;SAVE REGISTERS
	; TXA
	; PHA
	; TYA
	; PHA


; ;INIT COUNTERS AND INDEXES
	; LDY #$00									;INIT INPUT INDEX
	; STY ZONE_TOOLS.OUTPUT.INDEX2				;INIT OUTPUT INDEX

	; LDX #$00									;INIT TILE_QTY COUNTER (FOR ADJACENT TILES OF THE SAME TYPE)

	
; ;LOAD FIRST TILE
	; LDA (ZONE_TOOLS.INPUT2),Y
	; STA ZONE_TOOLS.TILE_LAST2	
	; INY											;INCREMENT INPUT INDEX
	; INX											;INCREMENT TILE_QTY COUNTER
; .LOOP.LOAD_TILE2	
	; LDA (ZONE_TOOLS.INPUT2),Y					;LOAD NEXT TILE_TYPE FROM ZONE DATA	
	; CMP ZONE_TOOLS.TILE_LAST2					;LAST TILE_TYPE TO BE LOADED FROM ZONE DATA
	; BNE .SAVE.TILE_PAIR2

	; INX											;INCREMENT TILE_QTY COUNTER
	; INY											;INCREMENT INPUT INDEX	
	; BNE .LOOP.LOAD_TILE2						;UNLESS INPUT INDEX HAS FLIPPED TO #$00, CONTINUE THE LOAD LOOP
	; ;END OF INPUT ARRAY REACHED

	; ;**FALLS THROUGH

; .SAVE.TILE_PAIR2
	; STA SAVED.ACC.LOCAL2						;SAVE CURRENT TILE_TYPE FOR FUTURE USE

; ;SAVE AND LOAD INDEX
	; STY ZONE_TOOLS.INPUT.INDEX2
	; LDY ZONE_TOOLS.OUTPUT.INDEX2

; ;SAVE TILE_PAIR TO COMRPESSED OUTPUT ARRAY
	; TXA	
	; STA ZONE_TOOLS.OUTPUT_BUFFER2,Y				;SAVE TILE_QTY
	; INY											;INCREMENT OUTPUT COUNTER
	; LDA ZONE_TOOLS.TILE_LAST2
	; STA ZONE_TOOLS.OUTPUT_BUFFER2,Y				;SAVE TILE_TYPE
	; INY											;INCREMENT OUTPUT COUNTER FORWARD TO THE NEXT TILE PAIR	
; ;	BEQ .ERROR									;IF OUTPUT COUNTER FLIPS TO $00, GENERATE ERROR
	; STY ZONE_TOOLS.OUTPUT.INDEX2
	; LDY ZONE_TOOLS.INPUT.INDEX2
	; BEQ .EXIT2									;EXIT IF THIS WAS LAST TILE
	
; ;RESTORE/INCREMENT COUNTERS AND CURRENT TILE	
	; LDA SAVED.ACC.LOCAL2						;RESTORE CURRENT TILE_TYPE
	; STA ZONE_TOOLS.TILE_LAST2
	; LDX #$01									;RESET TILE_QTY COUNTER, TO #$01 (REFLECTING CURRENT TILE IN ACC)

	; INY
	; BNE .LOOP.LOAD_TILE2 						;AT END OF ZONE_TOOLS ARRAY? (IT'S $100, SO WE'RE CHECKING FOR A FLIP TO $00)
	; JMP .SAVE.TILE_PAIR2
		
; .EXIT2

; ;WRITE STOP VALUE TO END OF OUTPUT ARRAY
	; LDY ZONE_TOOLS.OUTPUT.INDEX2
	; LDA #$00
	; STA ZONE_TOOLS.OUTPUT_BUFFER2,Y

						
; ;COPY OUTPUT ARRAY TO AUX MEMORY

; ;IDENTIFY OUTPUT BUFFER START/END ADDRESS
	; LDA #ZONE_TOOLS.OUTPUT_BUFFER2
	; STA WORLD.COMPRESS.BUFFER_ADDRESS.START2
	; STA OP1
	; LDA /ZONE_TOOLS.OUTPUT_BUFFER2
	; STA WORLD.COMPRESS.BUFFER_ADDRESS.START2+$1
	; STA OP1+$1

; ;THE OUTPUT INDEX/COUNTER IS THE OFFSET TO CALC THE END ADDRESS	
	; LDA ZONE_TOOLS.OUTPUT.INDEX2
	; STA OP2
	; LDA #$00
	; STA OP2+$1
	
	; JSR ADC.16.2										;WORLD.COMPRESS.BUFFER_ADDRESS(2) + ZONE_TOOLS.OUTPUT_BUFFER.INDEX(1) 

; ;SAVE OUTPUT BUFFER END ADDRESS
	; LDA RESULT
	; STA WORLD.COMPRESS.BUFFER_ADDRESS.END2
	; LDA RESULT+$1
	; STA WORLD.COMPRESS.BUFFER_ADDRESS.END2+$1

; ;AUX MOVE
	; SEC               							  	;SET CARRY FLAG DESGINATD MOVE FROM MAIN MEMORY -> AUX
	; LDA WORLD.COMPRESS.BUFFER_ADDRESS.START2		;SET START ADDRESS
	; STA AUX_MOVE.START
	; LDA WORLD.COMPRESS.BUFFER_ADDRESS.START2+$1		
	; STA AUX_MOVE.START+$1
	
	; LDA WORLD.COMPRESS.BUFFER_ADDRESS.END2			;SET END ADDRESS
	; STA AUX_MOVE.END
	; LDA WORLD.COMPRESS.BUFFER_ADDRESS.END2+$1
	; STA AUX_MOVE.END+$1
	
	; LDA WORLD.COMPRESS.ZONE_OUTPUT.ADDRESS2			;SET DESTINATION ADDRESS
	; STA AUX_MOVE.DEST
	; LDA WORLD.COMPRESS.ZONE_OUTPUT.ADDRESS2+$1
	; STA AUX_MOVE.DEST+$1

	; JSR AUX_MOVE

; ;RESTORE REGISTERS
	; PLA
	; TAY
	; PLA
	; TAX

; ;SETUP RETURN VALUE IN ACC
	; INC ZONE_TOOLS.OUTPUT.INDEX2					;INCREMENT SO THAT IT BECOMES THE LAST ARRAY ELEMENT WRITTEN +1
	; LDA ZONE_TOOLS.OUTPUT.INDEX2					;THE INDEX IS THE LAST ARRAY ELEMENT WRITTEN+1
		
			; ; LDX WORLD.COMPRESS.ZONE_OUTPUT.ADDRESS2
			; ; LDY WORLD.COMPRESS.ZONE_OUTPUT.ADDRESS2+$1
			; ; BRK
	; RTS

; .ERROR2
; ;ZONE_TOOLS.COMPRESS REPORTS THAT OUTPUT COUNTER FLIPPED TO $00 IN .SAVE.TILE_PAIR. 
; ;
; ;THIS IMPLIES THAT THE TILES WERE SO DISSIMULAR (HORITZONALLY) THAT THE COMRPESSED DATA TOOK MORE 
; ;BYTES THAN THE UNCOMPRESSED DATA. IF THAT OCCURS, I HAVEN'T THROUGHT OF A WAY TO HANDLE IT OTHER 
; ;THAN TO MODIFY THE MAP DATA SO COMPRESSION TAKES LESS BYTES. 

; ;DISABLE.BS_RAM
	; LDA $C082				;READ ENABLE ROM, DISABLE WRITE ON BSM (NORMAL STATE).
							; ;REQUIRED FOR BEFORE BRK BECAUSE APPLE MONITOR IS A ROM ROUTINE

	; LDA TEXT
	; BRK
@END

;=========DEFINE VARIABLES=================
@START
 
UPDATE.CHAR.POS2							.EQ $FC22		;RECALCULATE TEXT SCREEN LINE VALUE STORED AT $28, BASED ON ROW VALUE STORED AT $25


ZONE_TOOLS.OUTPUT_BUFFER2					.BS	$100,$42			;TEMPORARY BUFFER FOR OUTPUT FROM ZONE_TOOLS.UNCOMPRESS.SINGLE
NEW.MAP2									.EQ ZONE_TOOLS.OUTPUT_BUFFER2

ZONE_TOOLS.INPUT.INDEX2						.BS $1
ZONE_TOOLS.OUTPUT.INDEX2					.BS $1
ZONE_TOOLS.INPUT2							.EQ	$EA
ZONE_TOOLS.TILE_LAST2 						.BS	$1	


WORLD.COMPRESS.BUFFER_ADDRESS.START2		.BS $2	
WORLD.COMPRESS.BUFFER_ADDRESS.END2			.BS $2				
WORLD.COMPRESS.ZONE_INPUT.ADDRESS2			.BS	$2
WORLD.COMPRESS.ZONE_OUTPUT.ADDRESS2			.BS $2
WORLD.COMPRESS.AUX_MEMORY.START_LO2			.EQ	$00					;#CONSTANT. The starting location in aux memory where compressed zone data is stored	
WORLD.COMPRESS.AUX_MEMORY.START_HO2			.EQ	$02					;#CONSTANT. The starting location in aux memory where compressed zone data is stored	



WZONE.TOTAL2								.EQ $40					;#CONSTANT.	TOTAL NUMBER OF ZONES ON THE WORLD MAP - $1.

;==========KEEP THESE TWO TOGETHER, IN THIS ORDER====
ZONE.LOOKUP.LO2								.BS	$41,$42				;CONTAINS THE AUX LO ADDRESS FO EACH WORLD ZONE.
ZONE.LOOKUP.HO2								.BS	$41,$42				;CONTAINS THE AUX HO ADDRESS FO EACH WORLD ZONE. 
;=====================================
@END


;=========SET COMPRESSION STATUS FLAGS=====
@START
	;manually set these flags ($00=off, $01=on) to disable/enable compression on each world zone
	;Note: These flags must be set in two locations, here and in offloaded_variables2.ASM
WZONE.COMPRESSION.FLAGS2			.HS		01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.00.01.01.01.01.01.01.01.00.00.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01
	;WZONE (!DEC) #							0	1  2  3	 4	5  6  7	 8	9 10 11	12 13 14 15	16 17 18 19	20 21 22 23	24 25 26 27	28 29 30 31	32 33 34 35	36 37 38 39	40 41 42 43	44 45 46 47	48 49 50 51	52 53 54 55	56 57 58 59	60 61 62 63
;this one is for the file size estimate map
;WZONE.COMPRESSION.FLAGS2				.HS	01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.00.01.01.01.01.00.01.01.00.00.01.00.01.00.00.01.01.01.01.01.01.01.01.01.00.01.00.00.01.01.01.01.00.00.01.01.01.00.01.01.01.01.01.01.01.01.01




@END



;====INTRO TEXT ASCII STRINGS=====
LINE0	.AZ -/NOX ARCHAIST/, #$8D

LINE0.1	.AZ -/(Copyright (C) 2016. 6502 Workshop, LLC)/, #$0D
LINE1  .AZ -/PRE-ALPHA VERSION 0.10a/, #$0D

LINE2  .AZ -/RELEASE NOTES/, #$0D, /*No storyline or objective/,#$0D,/*Player can traverse outdoor map/,#$0D,/*No wrap around world: map edge alerts with sound/, #$0D, /*There are mobs, but no combat yet/, #$0D, /*Transport objects/, #$0D, /*Test map. much more land coming/, #$0D, /*Only 2 sounds. many more coming/, #$0D, #$0D

LINE3  .AZ -/PRESS ANY KEY FOR LIST OF GAME COMMANDS OR (S) TO START/, #$0D


LINE4  .AZ -/COMMAND LIST/,#$0D,/*MOVEMENT: arrow keys/,#$0D,/*(B)oard transport object/,#$0D,/*(X)it transport object/, #$0D, /*(J)ump horse/,#$0D, /*(Q)uit to apple monitor/,#$0D, /*(Y)ell to horse, giddyup/, #$0D, #$0D

LINE5  .AZ -/PLAYTEST KEYS/, #$0D, /*(1) toggle player collision controls/,#$0D,/*(2) toggle mob collision controls/, #$0D, /*(3) + arrow key, zap mob/, #$0D, /*(4,5) increase-decrease mob generation probability/, #$0D, #$0D

LINE6  .AZ -/PRESS ANY KEY TO START GAME/, #$0D

LINE7  .AZ -/<INSERT MAIN PLAYER DISK IN DRIVE 2>/, #$0D, #$0D, /THEN PRESS ANY KEY TO CONTINUE/

;Release Note: this version has no storyline or game objectives. Its scope is limited to the players ability to walk around on a map. 


;<playtest keys>

;Press any key to continue.


;LINE1  .AZ /M/, #$0D
	
@END	


;======INCLUDE FILES======	
;					.IN 	C:\MY_CODE\INCLUDES_LIBS\ROUTINES_TEXT.ASM
;					.IN 	C:\MY_CODE\INCLUDES_LIBS\ROUTINES_GENERAL.ASM
					
;======(SAME TARGET FILE)======


				
	.NO $4000				;ENSURES THAT THE PROGRAM CODE DOESN'T GET CLOBBERED BY THE INCLUDE TARGET FILES,
							;WHICH GET LOADED STARTING AT $4000. IF THE PROGRAM CODE OVERSHOOTS $2000 THE ASSEMBLER WILL GENERATE AN ERROR. 
				
	;none

;======(SEPERATE TARGET FILE)======				
				.IN 	C:\MY_CODE\INCLUDES_LIBS\DATA.SHAPES.SURFACE
				.IN 	C:\MY_CODE\INCLUDES_LIBS\DATA.OTHER.SUNRISE_SUNSET
				.IN 	C:\MY_CODE\INCLUDES_LIBS\COMPRESSED_DATA\COMPRESSED.DATA.MAP.SURFACE.ASM
