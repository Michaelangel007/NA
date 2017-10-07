;************************INCLUDE FILE*****************************
;(do not assemble stand-alone. For a stand-alone version of this routine see examples/rwts2)

				.TF     LOADER.P.BIN,BIN
				.OR		$2000			**Always put before .TF directive and never use again in program


GAME.LOADER1	
@START		
	
;=====================SUBROUTINES DOCUMENTATION====================================
;
;
;This program is responsible for:
;1) Move shape tables into AUX memory. 
;2) Load game files from disk into main and bank-switched memory. (the game files overrite the shape tables and this loader program in main memory
;	GOTHIC.SET				aux ($5900-5BFF)
;	LWR.MAIN.RTNS			$0C00-$1FFF
;	CONT.HRCG				$0300
;	GAME	 				$6000-$B700
;	BS_ROUTINES.BANK1		$D000-$DFFF (BSR:bank1)
;	BS_ROUTINES.BANK2		$D000-$FFFF (BSR:bank2)
;3) Compress world map zone data, and load to aux memory. Map data included in this file as hex tables (output from map editor spreadsheet)


;--PROGRAM MEMORY MAP--
;$2000 - $xxxx		main code
;$6000 - $8FFF		Temp main memory for loading BSR routines from disk so they can be copied from main memory into BSR
;$B000 - $B8FF		Temp main memory for various copy routines (i.e. CONT.HRCG, OpenDir.AUX_BSR.INIT)
;
;=================================================================================



		; LDA $C082
		; LDX #$AA
		; BRK
		

	;JMP OpenDir.AUX_BSR.INIT
	
		;(SPECIAL TEST) JMP JMP.TEST.ENTRANCE
	
DISPLAY.INTRO.TEXT
@START

	JSR CLEAR.TEXT.SCREEN

;NOX ARCHAIST HEADER						
	LDA #LINE0
	STA STRING
	
	LDA /LINE0
	STA STRING+$1	
		;NOP
	
	LDA #$0E
	STA HTAB	
	LDA #$00
	STA VTAB

		
	JSR UPDATE.CHAR.POS2

	JSR PRINT.STR2
	
;(Copyright (C) 2016. 6502 Workshop, LLC)			
	LDA #LINE0.1
	STA STRING
	
	LDA /LINE0.1
	STA STRING+$1			
	
	LDA #$00
	STA HTAB	
	LDA #$01
	STA VTAB
	
	JSR UPDATE.CHAR.POS2	
	JSR PRINT.STR2
	
	;RELEASE VERSION	
	LDA #LINE1 					
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

OpenDir.AUX_BSR.INIT ;COPY OpenDir TO AUX BSR:BANK1
@START
;COPY OpenDir FROM MAIN BSR:BANK1 TO AUX BSR:BANK1
	LDA $c08b		;enable bank-switched ram ($d000 bank 1)
	LDA $c08b
		
	;copy OpenDir to main memory temp location
		LDA #$00
		STA COPY.FROM_START
		LDA #$D0
		STA COPY.FROM_START+$1
		LDA #$FF
		STA COPY.FROM_END
		LDA #$D8
		STA COPY.FROM_END+$1
			
		LDA #$00
		STA COPY.TO
		LDA #$B0
		STA COPY.TO+$1
	
	JSR MEMORY.COPY2		

		
		TSX			;transfer stack pointer to X-REG
		STX TEMP	;save stack pointer
	STA $C009 ;enable aux zero-page & aux BSR 

	;copy OpenDir from main memory temp location to aux BSR
		LDA #$00
		STA COPY.FROM_START
		LDA #$B0
		STA COPY.FROM_START+$1
		LDA #$FF
		STA COPY.FROM_END
		LDA #$B8
		STA COPY.FROM_END+$1
			
		LDA #$00
		STA COPY.TO
		LDA #$D0
		STA COPY.TO+$1
	
	JSR MEMORY.COPY2	
		
	STA $C008 ;enable main zero-page & main BSR 
		LDX TEMP	;restore stack pointer to X-REG
		TXS ;transfer X-REG to stack pointer
	
	LDA $C082		;enable ROM, disable BSR
	
	; LDA $c08b		;enable bank-switched ram ($d000 bank 1)
	; LDA $c08b
	
	; ;erase original OpenDir location in main BSR:bank1
	; ;Note: the erase isn't required, it is included for testing purposes to ensure that the driver isn't accidentally run via it's original location in main BSR:bank1
		; LDA #$00
		; STA COPY.FROM_START
		; LDA #$A0
		; STA COPY.FROM_START+$1
		; LDA #$FF
		; STA COPY.FROM_END
		; LDA #$A8
		; STA COPY.FROM_END+$1
			
		; LDA #$00
		; STA COPY.TO
		; LDA #$D0
		; STA COPY.TO+$1
	
	; JSR MEMORY.COPY2

@END

LOAD.SHAPE.TABLES
@START
;LOAD FILE "DATA.SHP.SURF"
		
;set command type (READ | WRITE | SEEK)
; cmd_seek.current_drive 	.EQ $90
; cmd_read.current_drive  	.EQ $91
; cmd_write.current_drive 	.EQ $92
;			
; cmd_read.drive1			.EQ $1
; cmd_read.drive2		  	.EQ $81
; cmd_write.drive1			.EQ $2
; cmd_write.drive2			.EQ $82

	lda #cmd_read.drive1
	sta parm.reqcmd2

;set destination memory address
	lda #SHP.TBL.START.ADDRESS
	sta parm.ldrlo2
	lda /SHP.TBL.START.ADDRESS
	sta parm.ldrhi2

;set read length (bytes)
	lda #$FF				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo2	
	lda #$FF				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi2
	
;set filename to read from	
	lda #DATA.SHP.SURF2		;load LO address
	sta parm.namlo2
	lda /DATA.SHP.SURF2		;load HO address
	sta parm.namhi2

		LDA #$01	;PARM: $00=main, $01=aux
	JSR NOXARCH.MAIN.PRODOS.IO

		
@END

;LOAD GAME FILES

LOAD.GOTHIC.SET	
@START	
;LOAD FILE "GOTHIC.SET" (THIS IS THE HIGH-RES CHARACTER SET)

;set command type (READ | WRITE | SEEK)
; cmd_seek.current_drive 	.EQ $90
; cmd_read.current_drive  	.EQ $91
; cmd_write.current_drive 	.EQ $92
;			
; cmd_read.drive1			.EQ $1
; cmd_read.drive2		  	.EQ $81
; cmd_write.drive1			.EQ $2
; cmd_write.drive2			.EQ $82


	lda #cmd_read.drive1
	sta parm.reqcmd2

;set destination memory address
	lda #HRCG.AUX.START
	sta parm.ldrlo2
	lda /HRCG.AUX.START
	sta parm.ldrhi2

;set read length (bytes)
	lda #$FF				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo2	
	lda #$FF				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi2
	
;set filename to read from	
	lda #GOTHIC.SET				;load LO address
	sta parm.namlo2
	lda /GOTHIC.SET				;load HO address
	sta parm.namhi2

		LDA #$01	;PARM: $00=main, $01=aux
	JSR NOXARCH.MAIN.PRODOS.IO
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

;set command type (READ | WRITE | SEEK)
; cmd_seek.current_drive 	.EQ $90
; cmd_read.current_drive  	.EQ $91
; cmd_write.current_drive 	.EQ $92
;			
; cmd_read.drive1			.EQ $1
; cmd_read.drive2		  	.EQ $81
; cmd_write.drive1			.EQ $2
; cmd_write.drive2			.EQ $82

	lda #cmd_read.drive1
	sta parm.reqcmd2
	
;set destination memory address
	lda #$00
	sta parm.ldrlo2
	lda #$B3
	sta parm.ldrhi2

;set read length (bytes)
	lda #$FF				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo2
	lda #$FF				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi2
	
;set filename to read from	
	lda #CONT.HRCG				;load LO address
	sta parm.namlo2
	lda /CONT.HRCG				;load HO address
	sta parm.namhi2

		LDA #$00	;PARM: $00=main, $01=aux
	JSR NOXARCH.MAIN.PRODOS.IO

	;COPY CONT.HRCG FROM TEMP MEMORY TO PERMENANT MEMORY LOCATION
	LDA #$00
	STA COPY.FROM_START
	LDA #$B3
	STA COPY.FROM_START+$1
	LDA #$8D
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


;set command type (READ | WRITE | SEEK)
; cmd_seek.current_drive 	.EQ $90
; cmd_read.current_drive  	.EQ $91
; cmd_write.current_drive 	.EQ $92
;			
; cmd_read.drive1			.EQ $1
; cmd_read.drive2		  	.EQ $81
; cmd_write.drive1			.EQ $2
; cmd_write.drive2			.EQ $82

	lda #cmd_read.drive1	
	sta parm.reqcmd2

;set destination memory address
	lda #DATA.OTHER.SUNRISE_SUNSET.AUX.START
	sta parm.ldrlo2
	lda /DATA.OTHER.SUNRISE_SUNSET.AUX.START
	sta parm.ldrhi2

;set read length (bytes)
	lda #$FF				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo2	
	lda #$FF				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi2
	
;set filename to read from	
	lda #DATA.OTHER.SUN		;load LO address
	sta parm.namlo2
	lda /DATA.OTHER.SUN		;load HO address
	sta parm.namhi2

		LDA #$01	;PARM: $00=main, $01=aux
	JSR NOXARCH.MAIN.PRODOS.IO
	
@END
	
LOAD.WORLD.DATA
@START
;LOAD MAP DATA FROM DISK TO AUX MEMORY

;set command type (READ | WRITE)
	lda #$81 	;([$1=read drive1, | $2=write drive1], [$81=read drive2 | $82=write drive2])
	sta parm.reqcmd2

;set destination memory address
	lda #WORLD.COMPRESS.AUX_MEMORY.START_LO
	sta parm.ldrlo2
	lda #WORLD.COMPRESS.AUX_MEMORY.START_HO
	sta parm.ldrhi2

;set read length (bytes)
	lda #$FF				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo2	
	lda #$FF				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi2
	
;set filename to read from	
	lda #DATA.MAP.SURF2	;load LO address
	sta parm.namlo2
	lda /DATA.MAP.SURF2	;load HO address
	sta parm.namhi2

		LDA #$01	;PARM: $00=main, $01=aux
	JSR NOXARCH.MAIN.PRODOS.IO
	
@END
			
LOAD.BS_ROUTINES
@START
	
;LOAD FILE "BS.ROUTINES.BK1"

;READ SUBROUTINES FROM DISK TO MAIN MEMORY (LOWER 48K)	

;set command type (READ | WRITE | SEEK)
; cmd_seek.current_drive 	.EQ $90
; cmd_read.current_drive  	.EQ $91
; cmd_write.current_drive 	.EQ $92
;			
; cmd_read.drive1			.EQ $1
; cmd_read.drive2		  	.EQ $81
; cmd_write.drive1			.EQ $2
; cmd_write.drive2			.EQ $82

    lda #cmd_read.drive1
	sta parm.reqcmd2

;set destination memory address
	lda #$00
	sta parm.ldrlo2
	lda #$60
	sta parm.ldrhi2

;set read length (bytes)
	lda #$FF				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo2	
	lda #$FF				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi2
	
;set filename to read from	
	lda #BS.ROUTINES.BK1	;load LO address
	sta parm.namlo2	
	lda /BS.ROUTINES.BK1	;load HO address
	sta parm.namhi2
		
		LDA #$00	;PARM: $00=main, $01=aux
	JSR NOXARCH.MAIN.PRODOS.IO

	
		LDA #$00
		STA COPY.FROM_START
		LDA #$60
		STA COPY.FROM_START+$1
		LDA #$FF
		STA COPY.FROM_END
		LDA #$6F
		STA COPY.FROM_END+$1
		
		LDA #$00
		STA COPY.TO
		LDA #$D0
		STA COPY.TO+$1

		;ENABLE BSR:B1 ($D000 Bank 1)
		LDA $C08B					;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK1, 1ST)
		LDA $C08B
		
	JSR MEMORY.COPY2
	
;LOAD FILE "BS.ROUTINES.BK2"
	
;READ SUBROUTINES FROM DISK TO MAIN MEMORY (LOWER 48K)	

;set command type (READ | WRITE | SEEK)
; cmd_seek.current_drive 	.EQ $90
; cmd_read.current_drive  	.EQ $91
; cmd_write.current_drive 	.EQ $92
;			
; cmd_read.drive1			.EQ $1
; cmd_read.drive2		  	.EQ $81
; cmd_write.drive1			.EQ $2
; cmd_write.drive2			.EQ $82

	lda #cmd_read.drive1
	sta parm.reqcmd2

;set destination memory address
	lda #$00
	sta parm.ldrlo2
	lda #$60
	sta parm.ldrhi2

;set read length (bytes)
	lda #$FF				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo2	
	lda #$FF				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi2
	
;set filename to read from	
	lda #BS.ROUTINES.BK2	;load LO address
	sta parm.namlo2	
	lda /BS.ROUTINES.BK2	;load HO address
	sta parm.namhi2
	
		LDA #$00	;PARM: $00=main, $01=aux
	JSR NOXARCH.MAIN.PRODOS.IO


		
;COPY SUBROUTINES TO BSR:BANK2
	
	;ENABLE BSR:B2 ($D000 Bank 2)
	LDA $C083				;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK2 2nd)
	LDA $C083
	;The game code has BSR:bank2 enabled by default moving forward. ROM routines and BSR:Bank1 are enabled as-needed and then disabled. 
		
		LDA #$00
		STA COPY.FROM_START
		LDA #$60
		STA COPY.FROM_START+$1
		LDA #$FF
		STA COPY.FROM_END
		LDA #$8F
		STA COPY.FROM_END+$1
		
		LDA #$00
		STA COPY.TO
		LDA #$D0
		STA COPY.TO+$1
	
	JSR MEMORY.COPY2
		
@END
	
;(SPECIAL TEST) JMP.TEST.ENTRANCE
LOAD.GAME
@START	
;LOAD FILE "GAME" (THIS IS THE MAIN GAME PROGRAM)


;set command type (READ | WRITE | SEEK)
; cmd_seek.current_drive 	.EQ $90
; cmd_read.current_drive  	.EQ $91
; cmd_write.current_drive 	.EQ $92
;			
; cmd_read.drive1			.EQ $1
; cmd_read.drive2		  	.EQ $81
; cmd_write.drive1			.EQ $2
; cmd_write.drive2			.EQ $82

    lda #cmd_read.drive1
	sta parm.reqcmd2

;set destination memory address
	lda #$00
	sta parm.ldrlo2
	lda #$60
	sta parm.ldrhi2

;set read length (bytes)
	lda #$FF				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo2	
	lda #$FF				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi2
	
;set filename to read from	
	lda #GAME		;load LO address
	sta parm.namlo2	
	lda /GAME		;load HO address
	sta parm.namhi2


		
		LDA #$00	;PARM: $00=main, $01=aux
	JSR NOXARCH.MAIN.PRODOS.IO
	
	; LDA $C082
	; LDA #$AA
	; LDA STATUS
	; BRK
		
@END

;(SPECIAL TEST) 	JMP TEST.ENTRANCE
	
;*****USE PRODOS.IO to read files after this point, instead of NOXARCH.MAIN.PRODOS.IO
;****All subroutines and variables should be available after this point 	
LOAD.LOWER_MAIN.ROUTINES 		
@START							
;=====================CODE-SECTION DOCUMENTATION====================================
;
;LOADS SUBROUTINES STORED IN THE FILE "LWR.MAIN.RTNS" into the lower main memory regiong $0C00-$1200
;
;Loading this file clobbers NOXARCH.MAIN
;=================================================================================

;LOAD FILE "LWR.MAIN.RTNS"
	
;READ SUBROUTINES FROM DISK TO MAIN MEMORY (LOWER 48K)	

;set command type (READ | WRITE | SEEK)
; cmd_seek.current_drive 	.EQ $90
; cmd_read.current_drive  	.EQ $91
; cmd_write.current_drive 	.EQ $92
;			
; cmd_read.drive1			.EQ $1
; cmd_read.drive2		  	.EQ $81
; cmd_write.drive1			.EQ $2
; cmd_write.drive2			.EQ $82

    lda #cmd_read.drive1
	sta parm.reqcmd

;set destination memory address
	lda #$00
	sta parm.ldrlo
	lda #$0C
	sta parm.ldrhi

;set read length (bytes)
	lda #$FF				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda #$FF				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi
	
;set filename to read from	
	lda #LWR.MAIN.RTNS	;load LO address
	sta parm.namlo	
	lda /LWR.MAIN.RTNS	;load HO address
	sta parm.namhi

		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO


			

		
@END

	
LOAD.SURFACE.SPR.DATA
@START
;LOAD FILE "LWR.MAIN.RTNS"
	
;READ SUBROUTINES FROM DISK TO MAIN MEMORY (LOWER 48K)	

;set command type (READ | WRITE | SEEK)
; cmd_seek.current_drive 	.EQ $90
; cmd_read.current_drive  	.EQ $91
; cmd_write.current_drive 	.EQ $92
;			
; cmd_read.drive1			.EQ $1
; cmd_read.drive2		  	.EQ $81
; cmd_write.drive1			.EQ $2
; cmd_write.drive2			.EQ $82
 
	lda #cmd_read.drive1
	sta parm.reqcmd

;set destination memory address
	lda #MAP_OBJECTS.MOB
	sta parm.ldrlo
	lda /MAP_OBJECTS.MOB
	sta parm.ldrhi

;set read length (bytes)
	lda #$FF				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda #$FF				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi
	
;set filename to read from	
	lda #DATA.SPR.SURF		;load LO address
	sta parm.namlo	
	sta CURRENT.LOCATION.SPR_DATA
	lda /DATA.SPR.SURF		;load HO address
	sta parm.namhi
	sta CURRENT.LOCATION.SPR_DATA+$1
		
		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO


@END
@END

;TRANSFER CONTROL	
	JMP GAME.START.DRIVER



;=====SUBROUTINES ROUTINES (LOCAL COPY)=====
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

MEMORY.FILL2 ;============FILL MEMORY WITH SPECIFIED VALUE=======	
@START
;PARAMTERS; FILL.START(2), FILL.END(2), FILL.VALUE(1)
;RETURN: NONE
;ENTRANCE: DIRECT

;DRIVER TEMPLATE
		; LDA #$00
		; STA FILL.START
		; LDA #$70
		; STA FILL.START+$1
		; LDA #$00
		; STA FILL.END
		; LDA #$75
		; STA FILL.END+$1
		
		; LDA #$AA
		; STA FILL.VALUE
		
		; JSR MEMORY.FILL

		
.START

;RESTORE REGISTERS	
	TYA
	PHA
	
	LDY #$00			;LO BYTE COUNTER
;	FILL.START+$1		;DOUBLES AS HO BYTE COUNTER
.LOOP					
	LDA FILL.VALUE
	STA (FILL.START),Y
	LDA FILL.START+$1	
	CMP FILL.END+$1		;HAS HO BYTE COUNTER (FILL.START+$1) COUNTER REACHED FILL_END HO BYTE?
	BEQ .EXIT.TEST		;IF YES, CHECK TO SEE IF FILL_END LO BYTE HAS BEEN REACHED
.INCREMENT.COUNTER.LO_BYTE
	INY					;IF NO, INCREMENT LO BYTE COUNTER
	BNE .LOOP			;IF Y-REG HASN'T FLIPPED TO $00, CONTINUE LOOP
.INCREMENT.COUNTER.HO_BYTE		
	INC FILL.START+$1	;FILL_START+$1 DOUBLES AS THE HO_BYTE COUNTER
	JMP .LOOP
	
.EXIT.TEST
	TYA
	CLC
	ADC FILL.START		;THE LO BYTE START VALUE + Y-REG (COUNTER FOR LO BYTE) IS WHAT WE NEED TO COMPARE TO FILL.END
	CMP	FILL.END		;DOES Y-REG (LO_BYTE COUNTER) == FILL_END (THE LO BYTE OF END ADDRESS)?
	BNE .INCREMENT.COUNTER.LO_BYTE	;IF NO, INCREMENT COUNTER AND CONTINUE LOOP	
.FILL_DONE				;IF YES, THEN FILL IS DONE. 

;SAVE REGISTERS	
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

NOXARCH.MAIN.PRODOS.IO
@START
;PARAMETERS: ACC (parm.auxreq), parm.reqcmd, parm.ldrlo, parm.ldrhi, parm.namlo, parm.namhi, parm.sizehi, parm.sizelo
;ENTRANCE: direct
;RETURN: parm.status ($00 success, $01 file not found), parm.bleftlo*, parm.bleftho*
;*file size, if a read of $00 bytes is done. 

	
;SETUP MAIN/AUX MEMORY PARAMETERS
	;ACC contains auxreq
	STA parm.auxreq2 ; $00=main, $01=aux
	
;SAVE REGISTERS
	PHA
	TXA
	PHA
	TYA
	PHA
	
;INIT VARIABLES
	LDA #$00
	STA IO.ATTEMPTS2
	
;GET MAIN BSR MEMORY-STATUS UPON ENTRY
	LDA $C012		;ROM/BSR soft-switch flag (bit7 = 1: BSR, bit7=0 ROM)
	STA BSR.STATE2
	LDA $C011		;BANK1/BANK2 soft-switch flag (bit7 = 1: BANK2, bit7=0 BANK1)
	STA BANK.STATE2


;ENABLE AUX BANK-SWITCHED RAM ($D000 Bank 1)
	LDA $c08b
	LDA $c08b

;ENABLE AUX BSR & ZERO PAGE
		TSX			;transfer stack pointer to X-REG
		STX TEMP2	;save stack pointer
	STA $C009 ;enable aux zero-page & aux BSR.
		;***Must be done before copying the OpenDir parameters from main memory onto zero-page because switching to aux BSR also switches to aux zero-page.

		
;LOAD AUX ZERO-PAGE PARMS, THEN CALL OpenDir
		lda parm.reqcmd2 	
		sta reqcmd

.READ_WRITE.FILE

		lda parm.auxreq2 	
		sta auxreq
		
		lda parm.sizelo2 	
		sta sizelo
		
		lda parm.sizehi2 	
		sta sizehi
		
		lda parm.ldrlo2 		
		sta ldrlo

		lda parm.ldrhi2 		
		sta ldrhi

		lda parm.namlo2 		
		sta namlo

		lda parm.namhi2 	
		sta namhi

	JSR $d003 ;opendir I/O: entrance for opening new files + read/write
			
	LDA status			;load opendir return code
	BEQ .IO.SUCCEEDED	;was file-not-found ($01) reported? If no, then treat IO as successful
	LDA IO.ATTEMPTS2		;check whether both drives have been searched for the file requested
;	CPX IO.ATTEMPTS		;check whether both drives have been searched for the file requested
	BNE .INSERT.DISK	;if both drives have been checked (>=$01) then prompt player to insert disk
						;if not, try other drive 
		
	;determine which drive to try next
		
	LDA parm.reqcmd2 	;the reqcmd value specified by the calling routine. Don't use reqcmd because it could have been modified by the OpenDir. 
	BMI .TRY.DRIVE1		;is bit7 = 1? Then drive2 was just tried, check drive1 next
;.TRY.DRIVE2
	ORA #$80			;effectively adds #$80 to the ACC by changing bit 7 to $1. After ORA the ACC will contain a 1 for each bit which had a value of 1 in either the ORA value or the value in the ACC before the ORA was executed. 
						;Since we know that the read/write was first attempted on drive1 we know the value in the ACC was either $1 (read) or $2= (write). Adding $80 resuts in the values needed for drive2, for whichever operation (read or write) was aready specified. i.e. ($81 = read drive2, $82 = write drive2)
	STA REQCMD			;update the drive in the read/write command parameter
	INC IO.ATTEMPTS

	JMP .READ_WRITE.FILE

	
.TRY.DRIVE1
	AND #$03		;effectively subtracts #$80 from the ACC by changing bit 7 to $0. After the logical "AND" the ACC will contain a 1 for each bit which had a value of 1 in both the AND value and the value in the ACC before the AND was executed. In this case, bit 0 was the only bit with a value of $1 in both.  
					;Since we know that the read/write was first attempted on drive2 we know the value in the ACC was either $81 (read) or $82= (write). Subtracting $80 resuts in the values needed for drive1, for whichever operation (read or write) was aready specified. i.e. ($1 = read drive1, $2 = write drive1)
	STA REQCMD		;update the drive in the read/write command parameter

	INC IO.ATTEMPTS2

	JMP .READ_WRITE.FILE
	
.INSERT.DISK
	;<WRITE CODE FOR DISK PROMPT>. 
	;probably will be positioned on screen differently
	;if the prompt occurs during boot than if it occurs
	;during game play
			
		STA $C008 ;enable main zero-page & main BSR 
			LDX TEMP2	;restore stack pointer to X-REG
			TXS ;transfer X-REG to stack pointer

			JSR PREP.BRK
							
;Print "NO FILE" to text screen		
			LDA #$CE
			JSR COUT.ADDRESS
			LDA #$CF
			JSR COUT.ADDRESS
			LDA #$A0
			JSR COUT.ADDRESS
			LDA #$C6
			JSR COUT.ADDRESS
			LDA #$C9
			JSR COUT.ADDRESS
			LDA #$CC
			JSR COUT.ADDRESS
			LDA #$C5
			JSR COUT.ADDRESS			
		
			BRK

	
.IO.SUCCEEDED

;COPY SOME PARMS FROM AUX ZERO-PAGE TO MAIN MEMORY
;Note: this is so they can be retreived by the calling routine after exit, when main memory zero-page and BSR are enabled. 
	lda bleftlo
	sta parm.bleftlo2 
	lda bleftho
	sta parm.bleftho2 	
	lda status
	sta parm.status2 	

;ENABLE MAIN BSR AND ZERO-PAGE
	STA $C008 ;enable main zero-page & main BSR 
		LDX TEMP2	;restore stack pointer to X-REG
		TXS ;transfer X-REG to stack pointer
	
;RESTORE MEMORY-STATUS
	LDA BSR.STATE2					;get BSR State (either ROM enabled or BSR enabled) before P.IO driver was called
	BMI .GET.PRIOR.BSR.BANK_STATE	;is bit7 = 1? If yes, BSR was enabled, find out which bank
	STA $C082						;if no, ROM was enabled, so re-enable ROM
	BPL .alldone					;BPL must be true if BMI was false
.GET.PRIOR.BSR.BANK_STATE
	LDA BANK.STATE2					;get BSR Bank State (either Bank1 or Bank2 enabled) before P.IO driver was called
	BMI .BANK2						;is bit7=1? If yes, bank2 was enabled
	STA $C08B						;if no, Bank1 was enabled, so re-enable bank1
	STA $C08B
	BPL .ALLDONE
.BANK2
	STA $C083						;re-enable bank2
	STA $C083
.ALLDONE
	

;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX
	PLA

	RTS
	
@END

	
;WORLD.COMPRESS2 ; ======COMPRESS THE DATA FOR ALL ZONES AND STORE IN AUX MEMORY=====
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

;ZONE_TOOLS.WCOPY2
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
TOTAL.SECTORS2			.BS $1


;PRODOS.IO			
BSR.STATE2 		.BS $1
BANK.STATE2 	.BS $1

IO.ATTEMPTS2	.BS $1
TEMP2			.BS $1

parm.bleftlo2 	.BS $1
parm.bleftho2 	.BS $1
parm.status2 	.BS $1
parm.auxreq2 	.BS $1
parm.sizelo2 	.BS $1
parm.sizehi2 	.BS $1
parm.reqcmd2 	.BS $1
parm.ldrlo2 	.BS $1
parm.ldrhi2 	.BS $1
parm.namlo2 	.BS $1
parm.namhi2 	.BS $1


@END


;=========SET COMPRESSION STATUS FLAGS=====
@START
	;manually set these flags ($00=off, $01=on) to disable/enable compression on each world zone
	;Note: These flags must be set in three locations, here, in offloaded_variables2.ASM, and in map.compression.ps1 (top of file)
WZONE.COMPRESSION.FLAGS2			.HS		01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.00.01.01.01.01.01.01.01.00.00.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01
	;WZONE (!DEC) #							0	1  2  3	 4	5  6  7	 8	9 10 11	12 13 14 15	16 17 18 19	20 21 22 23	24 25 26 27	28 29 30 31	32 33 34 35	36 37 38 39	40 41 42 43	44 45 46 47	48 49 50 51	52 53 54 55	56 57 58 59	60 61 62 63
;this one is for the file size estimate map
;WZONE.COMPRESSION.FLAGS2				.HS	01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.00.01.01.01.01.00.01.01.00.00.01.00.01.00.00.01.01.01.01.01.01.01.01.01.00.01.00.00.01.01.01.01.00.00.01.01.01.00.01.01.01.01.01.01.01.01.01




@END



;====INTRO TEXT ASCII STRINGS=====
LINE0	.AZ -/NOX ARCHAIST/, #$8D

LINE0.1	.AZ -/(Copyright (C) 2016. 6502 Workshop, LLC)/, #$0D
LINE1  .AZ -/PRE-ALPHA VERSION 0.12/, #$0D

LINE2  .AZ -/RELEASE NOTES/, #$0D, /*No storyline or objective/,#$0D,/*Player can traverse outdoor map/,#$0D,/*No wrap around world: map edge alerts with sound/, #$0D, /*There are mobs, but no combat yet/, #$0D, /*Transport objects/, #$0D, /*Test map. much more land coming/, #$0D, /*Only 2 sounds. many more coming/, #$0D, #$0D

LINE3  .AZ -/PRESS ANY KEY FOR LIST OF GAME COMMANDS OR (S) TO START/, #$0D


LINE4  .AZ -/COMMAND LIST/,#$0D,/*MOVEMENT: arrow keys/,#$0D,/*(B)oard transport object/,#$0D,/*(X)it transport object/, #$0D, /*(J)ump horse/,#$0D,/*(Q)uit to apple monitor/,#$0D,/*(Y)ell to horse, giddyup/,#$0D,/*(T)alk to NPC/,#$0D,/*(P)ush object/,#$0D,/*(O)pen-operate lever/,#$0D,#$0D

LINE5  .AZ -/PLAYTEST KEYS/, #$0D, /*(1) toggle player collision controls/,#$0D,/*(2) toggle mob collision controls/, #$0D, /*(3) + arrow key, zap mob/, #$0D, /*(4,5) increase-decrease mob generation probability/, #$0D, #$0D

LINE6  .AZ -/PRESS ANY KEY TO START GAME/, #$0D

LINE7  .AZ -/FLOPPY VERSION ONLY/,#$0D,/<INSERT MAIN PLAYER DISK IN DRIVE 2>/, #$0D, #$0D, /THEN PRESS ANY KEY TO CONTINUE/

;Release Note: this version has no storyline or game objectives. Its scope is limited to the players ability to walk around on a map. 


;<playtest keys>

;Press any key to continue.


;LINE1  .AZ /M/, #$0D
	
@END	




;====DEFINE FILENAMES===

;*also see offloaded_variables.bin, Data Files section
;PRODOS FILENAMES
;format is filename length (bytes), filename. length is raw hex number, not ascii value
DATA.SHP.SURF2 	.AZ #$0D,/DATA.SHP.SURF/	;ascii array, using LO values. Filename is limited to !15 characters
CONT.HRCG		.AZ #$09,/CONT.HRCG/		;ascii array, using LO values. Filename is limited to !15 characters		
GOTHIC.SET		.AZ #$0A,/GOTHIC.SET/		;ascii array, using LO values. Filename is limited to !15 characters
DATA.OTHER.SUN	.AZ #$0E,/DATA.OTHER.SUN/	;ascii array, using LO values. Filename is limited to !15 characters
DATA.MAP.SURF2	.AZ #$0D,/DATA.MAP.SURF/	;ascii array, using LO values. Filename is limited to !15 characters
GAME			.AZ #$04,/GAME/				;ascii array, using LO values. Filename is limited to !15 characters
BS.ROUTINES.BK1	.AZ #$0F,/BS.ROUTINES.BK1/				;ascii array, using LO values. Filename is limited to !15 characters
BS.ROUTINES.BK2	.AZ #$0F,/BS.ROUTINES.BK2/				;ascii array, using LO values. Filename is limited to !15 characters
LWR.MAIN.RTNS	.AZ #$0D,/LWR.MAIN.RTNS/				;ascii array, using LO values. Filename is limited to !15 characters
;FILE.VARIABLES	.AZ #$09,/VARIABLES/				;ascii array, using LO values. Filename is limited to !15 characters




;FORMAT IS FILENAME LENGTH (BYTES), FILENAME. LENGTH IS RAW HEX NUMBER, NOT ASCII VALUE

; file.to.read  .AZ #$0A,/DUMMY.FILE/	;ascii array, using LO values. Filename is limited to !15 characters
; file.to.write .AZ #$0A,/DUMMY.FILE/	;ascii array, using LO values. Filename is limited to !15 characters



;======INCLUDE FILES======	
;					.IN 	C:\MY_CODE\INCLUDES_LIBS\ROUTINES_TEXT.ASM
;					.IN 	C:\MY_CODE\INCLUDES_LIBS\ROUTINES_GENERAL.ASM
					
;======(SAME TARGET FILE)======

	;none
				
;	.NO $4000				;ENSURES THAT THE PROGRAM CODE DOESN'T GET CLOBBERED BY THE INCLUDE TARGET FILES,
							;WHICH GET LOADED STARTING AT $4000. IF THE PROGRAM CODE OVERSHOOTS $2000 THE ASSEMBLER WILL GENERATE AN ERROR. 
				
	

;======(SEPERATE TARGET FILE)======	
				.IN 	C:\MY_CODE\INCLUDES_LIBS\DATA.SHAPES.SURFACE
				.IN 	C:\MY_CODE\INCLUDES_LIBS\DATA.OTHER.SUNRISE_SUNSET
				.IN 	C:\MY_CODE\INCLUDES_LIBS\COMPRESSED_DATA\COMPRESSED.DATA.MAP.SURFACE.ASM

				.IN 	C:\MY_CODE\testing\prodos_testing\dummy.file.ASM								

