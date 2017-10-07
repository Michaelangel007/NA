;************************INCLUDE FILE*****************************
;(do not assemble stand-alone. For a stand-alone version of this routine see examples/rwts2)
				.TF     SWAP.ROUTINES.NPC.TALK.bin,BIN
				.OR		$AC00			**Always put before .TF directive and never use again in program
				;.TF     graphics_scrolling.bin,BIN
				.EB		OFF
				
				
				
NPC.TALK
@START	
;PARAMETERS: XREG (MO record # of Active PC), Y-REG (NPC screen tile location)
;RETURN
;ENTRANCE:DIRECT (must swap main memory/aux)

;=====================SUBROUTINE DOCUMENTATION====================================
;
;===OVERVIEW===
;
;NPC Talk is a module loaded in to the main memory swap area, reserved for modules that do not need to be in memory at the same time. This is the first such
;module to be written. It is expected that the modules for functions like combat, inventory, merchant transactions and possibly non-combat spell casting will be loaded into the same swap area.  
;
;The main loop in NPC.TALK is very simple because we developed a rudamentary high(er) level programming
;language called NTALK with which to write and interpret the talk data files.
;
;The entrance to the interpreter is NTALK.SEARCH and it contains
;3 modes, specified by a parameters:
;	Find NPC Record
;	Find Intro Text
;	Search for player question response text.
;
;===MAIN LOOP FUNCTIONS===
;
;The main loop performs the following tasks:
;	*Setup Text Windows and manage screen clears
;	*Use NTALK.SEARCH to load the NPC talk data into memory for the NPC the player is talking to
;	*Use NTALK.SEARCH to locate the NPC's intro text 
;	*Print the intro text to the video screen. Note: each text block has a stop value of $00. This is added by the SBASM cross assembler as all text blocks are created as ASCII tables. 
;	*(loop) Use NPC.TALK.INPUT to let player input a question via keyboard
;	*(loop) Use NTALK.SEARCH to locate the response text for the player's question, if it exists, and if not locate the default response text 
;	*(loop) Print the response to the player's question onscreen. Note: each text block has a stop value of $00. This is added by the SBASM cross assembler as all text blocks are created as ASCII tables. 
;	*continue loop until bye is detected.
;	*clean up text windows, return screen to normal
;
;Additionally, before NPC.TALK is called from the main game loop, 
;the main memory swap area is swapped out to aux memory as it contains 
;A* data which needs to be restored. Once NPC Talk returns, the main 
;game loop calls for this data to be swapped back in. 
;
;===VOICE MODES===
;Currently three voice modes are supported: normal, yell, whisper
;Using NTALK subcommands in the talk data files, a particular response text block
;can be restricted to a specific voice mode
;
;===OTHER FEATURES===
;*Character Matching
;Only the first 4 characters of a keyword the player enters are required to
;be found in the the NPC.TALK.ARRAY (the unpacked data buffer) for it to be considered
;a match. The number 4 can be changed via the constant NTALK.SEARCH.MATCH_QTY.
;If a keyword is defined in the talk data which is less then 4 characters, then only
;the number of characters of the defined keyword are required to be found for it to be
;considered a match. For example, player input of "JOBZ" will match against a defined keyword of "JOB"
;
;*Case-Insensitive Matching
;
;===TERMINOLOGY===
;
;Active NPC = the NPC the player has requested to talk to. 
;Active NPC ID = the Map Object Array record # of the Active NPC. This value is passed
;			to NPC.TALK via TALK.COMMAND in the main game loop. 
;
;===ADDITIONAL DOCUMENTATION
;*Talk Data File Format: see docs at the top of each talk data file. The Talk Data file for L1.M1 is the most authoratative. 
;*NTALK: See NTALK.SEARCH and it's subroutines for more details on the innerworkings of NTALK
;*Compression Scheme: see below, "COMPRESSION SCHEME"
;*Alternate Compression Scheme: see below, "ALTERNATE COMPRESSION SCHEME"
;
;
;
;===IDEAS FOR FREEING UP MEMORY==
;clobber mob map object array at $A900.
;Do this by placing a patch code section at $B900 that gets copied
;to $A900 when NPC.TALK is launched. The patch code will get clobbered by the packed data.
;As to what code to put there, the ascii tables at the end of this file are a good place to start and
;from there just find a subroutine that fits in a page of memory. 
;
;===MODULE MEMORY MAP===
;$A000-A8FF**	NPC.TALK.ARRAY. Unpacked data buffer. (don't change the size without adjusting overflow checks. See .ERROR.TALK_DATA.OVERFLOW)
;$A900-ABFF*	Map Object Arrays (mob, general, and npc) 
;$AC00-B9FF		main program code 
;$BA00-BEFF**	NPC.TALK.ARRAY.PACKED. Packed data buffer. (don't change the size without adjusting overflow checks. See .NPC.FOUND)
;$BF00-BFFF**	ZX7 unpacker
;
;*The map object arrays are swapped (copied) out to aux memory when NPC.TALK is loaded. However, the general and npc screen arrays ($AA00-$ABFF) for sure must be kept in memory at their normal locations, otherwise animation manager won't draw those objects. Animation runs during the keyboard input routines. 
;
;**The ZX7 unpacked must reside at a memory address higher than the unpacked and packed data. The packed data must reside at a memory address higher than the unpacked data. I don't have the details on the reason for this. 
;
;=================================================================================

	;-XREG contains map object record # of the Active NPC
	;-YREG contains Active NPC screen tile location when NPC.TALK is called

	

			
	
.INIT.TALK
@START
.INIT.PARAMETERS
	STX NPC.TALK.RECORD	
	STY NPC.TALK.SCREEN_LOCATION

.INIT.MEMORY

		LDA #PATCH.CODE.SECTION
		STA COPY.FROM_START
		LDA /PATCH.CODE.SECTION
		STA COPY.FROM_START+$1
		LDA #PATCH.CODE.SECTION+$FF
		STA COPY.FROM_END
		LDA /PATCH.CODE.SECTION+$FF
		STA COPY.FROM_END+$1
			
		LDA #MAP_OBJECTS.MOB
		STA COPY.TO
		LDA /MAP_OBJECTS.MOB
		STA COPY.TO+$1
		
		JSR MEMORY.COPY
		
.INIT.VARIABLES
	LDA #$00
	STA NTALK.VOICE_MODE
	STA NTALK.FLAG.BYE		;flag defaults to off. Will be set to $01 if player entered "BYE" in NPC.TALK.INPUT

	
.INIT.SCREEN	
	JSR INIT.TEXT_WINDOW.RIGHT
	JSR UPDATE.VOICE_MODE.DISPLAY
	JSR RESET.TEXT_WINDOW.TALK_INPUT ;displays cursor prompt and voice mode text

;PRINT "Voice Mode:"
		LDA #TWS.RIGHT_WINDOW.LEFT_SBYTE
		STA HTAB	
		LDA #$15
		STA VTAB
		JSR	UPDATE.CHAR.POS
			
.PRINT.VOICE.HEADER
		LDA #VOICE.HEADER.TEXT					
		STA STRING
		
		LDA /VOICE.HEADER.TEXT						
		STA STRING+$1
	JSR PRINT.STR


			
;SET CURSOR IN START POSITION	
		
		;SET CURSOR POSITION TO UPPER LEFT OF TEXT WINDOW
		LDA #$19			;$19full / $1E test
		STA HTAB	
		LDA TWF.BOTTOM_ROW	;always set to 1 line less than the bottom of the text window so that text will start printing as close to the player input prompt as possible. It needs to be 1 lines less for scroll to work properly. 
		SBC #$01
		STA VTAB
		JSR	UPDATE.CHAR.POS
	
.INIT.NPC.TEXT

.CHECK.HOSTILE
	LDX NPC.TALK.RECORD ;load index to map object record for Active NPC
	LDA MAP_OBJECTS.NPC+$6,X ;load transit flag of the record
	CMP #$FF			;is the NPC hostile?
	BNE .CHECK.HOSTILE.DONE	;if no, then next check
							;if yes, then print a generic text block to the video screen

.PRINT1
		LDA #HOSTILE.TEXT.BLOCK					
		STA TWF.STRING

		LDA /HOSTILE.TEXT.BLOCK
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW
	JMP .EXIT	
.CHECK.HOSTILE.DONE

.CHECK.SLEEPING	
;Check to see if NPC is sleeping by checking for bed and cot tiles
	LDY NPC.TALK.SCREEN_LOCATION
	LDA SCREEN.TILE.DATA,Y	;load tile type for terrain the NPC is located on
	CMP #TILE_ID.BED_LEFT_UNOCCUPIED	 ;is terrain a bed?		.EQ $4C				;#CONSTANT		TILE TYPE ID
	BEQ .IS.SLEEPING				;if yes, then NPC is sleeping
	CMP #TILE_ID.COT_UNOCCUPIED		;is terrain a cot?	
	BNE .CHECK.FOR.SLEEPING.DONE	;if yes, then NPC is sleeping
	;**FALLS THROUGH**	

.IS.SLEEPING
.PRINT2
		LDA #SLEEPING.TEXT.BLOCK					
		STA TWF.STRING

		LDA /SLEEPING.TEXT.BLOCK
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW
	JMP .EXIT
.CHECK.FOR.SLEEPING.DONE

			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; JSR KEYIN
			; JSR PREP.BRK
			; LDA #$AA
			; BRK
; .TEMP
			; LDA TEMP
			
.FIND.NPC_RECORD1
			
			; LDA #$28
			; STA NPC.TALK.RECORD
		LDA #$00 ;set mode: Find NPC Record
	JSR NTALK.SEARCH
		; ;parse return values
		; ;ACC = $00 found, $01 = not found
		; CMP #$00 ;was NPC Record Found?
		; BEQ .FIND.INTRO.TEXT	;if yes, continue with NPC conversation
		; JMP .ERROR.NPC_RECORD.NOT_FOUND ;if no, report error

.FIND.INTRO.TEXT		
		LDA #$01 ;set mode: Find Into Text
	JSR NTALK.SEARCH
		;parse return values
		;ACC = $00 found, $01 = not found
		CMP #$00 ;was Intro text found?
		BEQ .PRINT.INTRO.TEXT		;if yes, continue with NPC conversation
		JMP .ERROR.INTRO.NOT_FOUND	;if no, report error

		
.PRINT.INTRO.TEXT
		;set parms
		;load index to text into the input pointer for the text window routine
		LDA NPC.TALK.TEXT.INDEX+$0					
		STA TWF.STRING
		
		LDA NPC.TALK.TEXT.INDEX+$1
		STA TWF.STRING+$1							
	JSR PRINT.TEXT.WINDOW


			
			; ;JSR KEYIN
			; LDA #$AA
			; JSR PREP.BRK
			; BRK
			
.CHECK.FOR.BYE1 ;did player exit conversation?
;NOTE: this is in case the intro text had an NPC termination subcommand, giving the NPC a probability of deciding to terminate the conversation.
	LDA NTALK.FLAG.BYE
	CMP #$01			;is flag set? (flag is set if NPC.TALK.KEYWORD contains the word "$!BYE")
	BCS .EXIT	
	
	
@END
	;**FALLS THROUGH**




.TALK.MAIN.LOOP
@START	
.GET.PLAYER.QUESTION
	JSR NPC.TALK.INPUT
	

		
.SEARCH.ARRAY.FOR.QUESTION
				;LDA #$01
				;STA TROUBLESHOOTING.HOOK
				;INC TROUBLESHOOTING.HOOK
				
		LDA #$02 ;set mode: Search for question as keyword
	JSR NTALK.SEARCH


			
	;ACC: $00 = question keyword found, $01 = question keyword not found
	;return value not current used by this routine as it should always be $00 (found)
	;because even if the player's keyword isn't found, the lower level routines replace it with a search for "$!DFL" to retreive the NPC's default reponse. 
	;A check for whether the player or NPC initiated "BYE" mode is done below, after the NPC's response is printed to the screen.


			

 

;ADVANCE TWO TEXT WINDOW LINES, AND PRINT SEPERATOR

	; LDA NTALK.FLAG.BYE
	; CMP #$02 	;is bye flag set to the value used to indicate that the NPC terminated the conversation?
	; BEQ .PRINT.NPC.RESPONSE	;if yes, then skip .CR.PRINT as the line advance would already have been done. 
 
.CR.PRINT
		LDA #CR.TEXT					
		STA TWF.STRING

		LDA /CR.TEXT
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW

.PRINT.NPC.RESPONSE	
;Note: each text block has a stop value of $00. This is added by the SBASM cross assembler as all text blocks are created as ASCII tables. 

		;set parms
		;load index to text into the input pointer for the text window routine
		LDA NPC.TALK.TEXT.INDEX+$0					
		STA TWF.STRING+$0
		
		LDA NPC.TALK.TEXT.INDEX+$1
		STA TWF.STRING+$1			
	JSR PRINT.TEXT.WINDOW	

	

			
.RESET.INPUT.WINDOW
	JSR RESET.TEXT_WINDOW.TALK_INPUT

		; JSR PREP.BRK
		; LDA #$EA
		; ;LDX HTAB
		; ;LDY VTAB
		; LDX NPC.TALK.TEXT.INDEX
		; LDY NPC.TALK.TEXT.INDEX+$1
		; BRK	

.CHECK.FOR.BYE2 ;did player exit conversation?
	LDA NTALK.FLAG.BYE
	CMP #$01			;is flag set? (flag is set if NPC.TALK.KEYWORD contains the word "$!BYE")
	BCS .EXIT	
	
	JMP .GET.PLAYER.QUESTION

@END
		
	
.EXIT
		
;PRINT "ANY KEY" PROMPT
		LDA #TEXT_BLOCK.ANY_KEY1					
		STA TWF.STRING
		
		LDA /TEXT_BLOCK.ANY_KEY1
		STA TWF.STRING+$1
	JSR PRINT.TEXT.WINDOW	

.WAIT.FOR.KEY


			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP1
			; JSR KEYIN
			; LDA #$AA
			; JSR PREP.BRK
			; BRK
; .TEMP1
			; LDA TEMP
			
			
		;LDA #$01
		;STA KEYIN.NO_CLEAR	;don't clear the keypress buffer so that the keypress will be processed as the next player command by the main game loop
	JSR KEYIN.ANIMATION.SINGLE

		LDA #$03 ;clear both pages
	JSR CLEAR.TEXT_WINDOW.RIGHT


		; LDA EVENT.FLAGS+$02
		; CMP #$01
		; BNE .FLAG.NOT.SET		
		; LDA #$24
		; STA MAP_OBJECTS.GENERAL+$03
			
		; LDA #$AA
		; JSR PREP.BRK
		; BRK
		
;.FLAG.NOT.SET

	RTS

.ERROR.INTRO.NOT_FOUND	
	JSR PREP.BRK
	BRK
	
NTALK.SEARCH
@START
;PARAMETERS: ACC = MODE, NPC.TALK.ARRAY(x), [NPC.TALK.KEYWORD(x)]
;RETURN: ACC = keyword found/not found return value ($00 keyword found, $01 = keyword not found), except in MODE $00, which will error if it fails.
;ENTRACE: DIRECT, NPC.TALK

;=====================SUBROUTINE DOCUMENTATION====================================
;
;===OVERVIEW===
;NTALK.SEARCH is the primary entrance to the
;interpreter to the NTALK programming language we developed 
;for managing NPC conversation text.
;
;There are 3 primary fuctions provided by NTALK,
;which are organized into modes for calling NTALK.SEARCH. 
;
;----------
;MODE:	$00 = find NPC Record (searches talk data file)
;		$01 = find Intro text (searches unpacked talk data for command $I)
;		$02 = search for player question response text (searches unpacked talk data file for $![keyword])
;
;The MODE parameter is set via the ACC before calling this subroutine. 
;If using keyword mode ($03), also load keyword into NPC.TALK.KEYWORD without the $ command character.
;-----------
;
;===MODE DETAILS===
;
;-Mode $00: find NPC Record
;The goal of this mode is to find the packed data 
;for the Active NPC and unpack it using the 6502 ZX7 unpacker. 
;
;For details on how this is done, see "COMPRESSION SCHEME" below.
;
;-Mode $01: find Intro Text
;This mode and the next are both based on the 
;keyword search capability of NTALK.SEARCH.LOOP
;and it's subordinate routine NTALK.ARRAY.SEARCH
;
;All that happens in this routine is
;NPC.TALK.KEYWORD is loaded with $!Intro
;and then NTALK.SEARCH.LOOP is called. This is setup
;as a special mode distinct from mode $02
;because mode $02 searches for a player input keyword whereas this
;mode searches for a static keyword, which is needed since 
;every NPC has an introduction text block in the talk data file. 
;
;-Mode $02
;This mode searches for a player keyword obtained via NPC.TALK.INPUT.
;Like with mode $01, executes the search via NTALK.SEARCH.
;It returns a pointer to the text block associated with the keyword the player entered.
;
;There is also logic to handle some specific scenarios:
;	*Player Keyword not found
;		In this event, the $!BYE is loaded into NPC.TALK.KEYWORD and the search is run again
;		so that the default response will be located. To do this, the pointer must be reset to the beginning of the unpacked data for the Active NPC. 
;	*Subcommand processing
;		If there were NTALK subcommands on the NTALK keyword command that was matched, NPC.SEARCH.LOOP
;		sets the associated flags in the array NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS. 
;		In general, this routine processes those flags and implements any functionality. 
;		Such as voice mode (normal, yell, whisper). See Voice Mode section below for more details.
;	*Subcommand: voice mode
;		If the flag indicating a voice mode subcommand (normal, yell, whisper) was present, that means
;		the keyword was found but the text block is restricted to a particular voice mode. The player's active voice mode is checked. 
;		If the player's active voice mode matches, then a pointer to the restricted text block is returned so that it can be displayed.  
;		If the player's active voice doesn't match, then the search for the keyword is done over again (pointer must be rest to the start of the unpacked data for the Active NPC)
;		This process continues until a voice match is found or until the keyword is not found, in which case the "Player Keyword Not Found" logic described above 
;		is initiated. 
;
;
; 
;===COMPRESSION SCHEME===
;The NPC Talk data file (example filename: data.talk.L1.M1.ASM, which contains the talk data for map $01.)
;has a 3 byte header before each packed data record. The header
;contains the NPC Record ID, and the packed data length (2 bytes). The NPC
;record ID uses the same scheme as the Map Object arrays. 
;
;In summary, the NPC record is found by
;loading header, checking to see if the NPC Record ID matches the Active NPC ID.
;If there is a match, then the packed data for the current record is unpacked
;and if there is not a match, then the packed data for the curren record 
;is skipped via the ProDOS driver's seek function, and
;then the header for the next record is read in and examined to see if it matches
;the the Active NPC ID. This process continues until the Active NPC ID is found or 
;the search goes through 32 iterations, resulting an an error. 32 is
;the max NPCs per building map. 
;
;The talk data is packed during the build process (on a PC). 
;The ZX7 compressor is setup to take 1 input file and generate 1 output file,
;so we made a QB64 program to manage the process. 
;
;The talk data files are binary files containing
;ASCII text values and NTALK command value. These binary files are
;generated by the SBASM cross assembler. Each NPC Record has it's own binary file. 
;The QB64 program calls the PC ZX7 compressor to pack each binary talk data file
;add 3 header bytes for each record, and collate NPC talk data for an entire building map 
;into a single packed binary file. 
;
;For more documentation on the QB64 program,
;see binary.collator.bas
;
;For information on an alternate compression scheme that
;could be used to significantly improve the 
;compression ratio, see "ALTERNATE COMPRESSION SCHEME" below. 
;
;
;===ALTERNATE COMPRESSION SCHEME===
;
;The downsize to the current scheme is a low compression ratio. 20-30%. 
;This is because each NPC Record's talk data is compressed seperatly and smaller files don't compress as efficiently.
;
;An alternate compression scheme would be to make the packed data bubbled equal to 
;a specific number of bytes, like $600, the size of the packed data buffer (the buffer can probably
;be increased in size, depending on the final size of the main program code for this module)
;Each $600byte record would have a 2 byte header containing the start and end NPC ID for the bubble. 
;or maybe just a 1 byte header with the end. 
;
;Under this setup, aPLib or LZ4
;compression could be used. aPLib if I recall has the best compression ratio. That plus the larger
;bubble size should significantly improve the overall compression ratio for NPC text. 
;
;Using this scheme, searching for an NPC Record ID could be done
;by comparing the Active NPC ID to the header and once the correct packed bubble is
;found, unpack it into the unpacked data buffer.
;
;NPC.SEARCH.LOOP and NPC.KEYWORD.SEARCH were originally designed to be able to 
;search for a $R command, if mode $00 was active. This command was for imbedding the
;NPC ID in the NPC talk data file. Thus, this functionality could be used to search the
;unpacked data buffer for the NPC ID, and return the index to it for use with all other
;NTALK searches against that record. This functinality might be commented out. 
;
;Of course, some additional code would have to be written to manage
;the fact that the unpacked data buffer will often have a parial record loaded at the end.
;
;=================================================================================

.INIT	
;INIT VARIABLES

		
		;ACC = parameter passed when calling this subroutine
	STA NTALK.SEARCH.MODE


	;set NTALK.ARRAY.SEARCH parameters
	LDA #NPC.TALK.KEYWORD				;array storing the player input keyword (via NPC.TALK.INPUT)
	STA NTALK.KEYWORD.POINTER+$0		;NTALK.ARRAY.SEARCH parameter: pointer to array used as keyword search string

	LDA /NPC.TALK.KEYWORD
	STA NTALK.KEYWORD.POINTER+$1

	LDA #NPC.TALK.KEYWORD.STOP_VALUE			;stop value for arraying storing player input keyword (via NPC.TALK.INPUT)
	STA NTALK.KEYWORD.STOP_VALUE

	;SETUP ZERO-PAGE POINTER @ START OF TALK ARRAY
	LDA NPC.TALK.RECORD.INDEX+$0	;this is a bit inefficient, but it is setup to support the unpacked data being in a dynamic location, which would be needed of the ALTERNATE COMPRESSION SCHEME (see NTALK.SEARCH subroutine docs) were implemented.
	STA NTALK.ARRAY.POINTER+$0	
	
	LDA NPC.TALK.RECORD.INDEX+$1	;this is a bit inefficient, but it is setup to support the unpacked data being in a dynamic location, which would be needed of the ALTERNATE COMPRESSION SCHEME (see NTALK.SEARCH subroutine docs) were implemented.
	STA NTALK.ARRAY.POINTER+$1
	
	LDA #$06	
	STA NTALK.SEARCH.MATCH_QTY	; Number of characters that must be the same for a match to be found. For exampe, if this variable is set to 4 then if the first 4 characters of the keyword are found then the entire keyword is considered to be found. The number of characters must include the NTALK keyword subcommand code of "$!" 

	
;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA
	
.DETERMINE.SEARCH.MODE
	LDA NTALK.SEARCH.MODE	;load search mode code, passed as a parameter
	BEQ .FIND.NPC.RECORD2
	CMP #$01
	BEQ .FIND.INTRO.TEXT_STEP
	CMP #$02
	BEQ .SEARCH.QUESTION_RESPONSE.TEXT_STEP	
	;**FALLS THROUGH**

		
.ERROR.INVALID.SEARCH_MODE
;NTALK.SEARCH reports value in NTALK.SEARCH.MODE parameter is invalid.  
	LDA NTALK.SEARCH.MODE
	JSR PREP.BRK
	BRK

.FIND.INTRO.TEXT_STEP
	JMP .FIND.INTRO.TEXT
	
.SEARCH.QUESTION_RESPONSE.TEXT_STEP
	JMP .SEARCH.QUESTION_RESPONSE.TEXT
	
.FIND.NPC.RECORD2
@START
;INIT VARIABLES
	LDA #$01
	STA NPC.TALK.FILE.RECORD_COUNTER ;starts at 1 to count the first record read before the loop starts
	LDA #$FD
	
	STA NPC.TALK.ARRAY.END		;unpacked data overflow detection value
	STA NPC.TALK.ARRAY.END-$1	;unpacked data overflow detection value
	STA NPC.TALK.ARRAY.END-$2	;unpacked data overflow detection value

;OPEN TALK DATA FILE + READ 3 BYTES
;(the 3 bytes are the NPC ID of the first packed data record and the 2 byte length of the packed data in the record).


;set command type (READ | WRITE | SEEK)
; cmd_seek.current_drive 	.EQ $90
; cmd_read.current_drive  	.EQ $91
; cmd_write.current_drive 	.EQ $92
;			
; cmd_read.drive1			.EQ $1
; cmd_read.drive2		  	.EQ $81
; cmd_write.drive1			.EQ $2
; cmd_write.drive2			.EQ $82

	lda #cmd_read.drive2	
	sta parm.reqcmd
	
;set destination memory address
	lda #NPC.TALK.FILE.RECORD_ID
	sta parm.ldrlo
	lda /NPC.TALK.FILE.RECORD_ID	
	sta parm.ldrhi

;set read length (bytes)
	lda #$03				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda #$00				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi
	
;set filename to read from	

	lda CURRENT.LOCATION.TLK_DATA		;load LO address
	;LDA #D.TLK.L1.M1	
	sta parm.namlo
	lda CURRENT.LOCATION.TLK_DATA+$1	;load HO address
	;LDA /D.TLK.L1.M1	
	sta parm.namhi 
	
		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO
	
	LDA NPC.TALK.FILE.RECORD_ID		;load the record ID of the first record in the NPC TALK data file
	CMP NPC.TALK.RECORD				;is the file record ID = the NPC ID of the NPC the player is talking to?
	BEQ .NPC.FOUND	;if yes, then the packed data for the desired NPC is found
	;**FALLS THROUGH**
	

		
.NPC_ID.SEARCH.LOOP
;SKIP CURRENT RECORD'S PACKED DATA

;SEEK (current file) by number of bytes of packed data
;(this is because we're already established that the current record ID doesn't match the desired NPC Record ID)
;(so we're skipping past the packed data in the current record)

;set command type (READ | WRITE | SEEK)
; cmd_seek.current_drive 	.EQ $90
; cmd_read.current_drive  	.EQ $91
; cmd_write.current_drive 	.EQ $92
;			
; cmd_read.drive1			.EQ $1
; cmd_read.drive2		  	.EQ $81
; cmd_write.drive1			.EQ $2
; cmd_write.drive2			.EQ $82
	lda #cmd_seek.current_drive
	sta parm.reqcmd

;set seek length (bytes)
	lda NPC.TALK.FILE.PACKED_RECORD_LENGTH		;packed data length (HO byte). 
	sta parm.sizelo	
	lda NPC.TALK.FILE.PACKED_RECORD_LENGTH+$1 	;packed data length (HO byte). 
	sta parm.sizehi
	
		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO

;READ NEXT RECORD

	INC NPC.TALK.FILE.RECORD_COUNTER ;increment after each record is read in

;READ 3 BYTES (current file)
;(the 3 bytes are the NPC ID of the first packed data record and the 2 byte length of the packed data in the record).

;set command type (READ | WRITE | SEEK)
; cmd_seek.current_drive 	.EQ $90
; cmd_read.current_drive  	.EQ $91
; cmd_write.current_drive 	.EQ $92
;			
; cmd_read.drive1			.EQ $1
; cmd_read.drive2		  	.EQ $81
; cmd_write.drive1			.EQ $2
; cmd_write.drive2			.EQ $82
	lda #cmd_read.current_drive	
	sta parm.reqcmd
	
;set destination memory address
	lda #NPC.TALK.FILE.RECORD_ID
	sta parm.ldrlo
	lda /NPC.TALK.FILE.RECORD_ID	
	sta parm.ldrhi

;set read length (bytes)
	lda #$03				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda #$00				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi
	
		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO


		; LDA NPC.TALK.FILE.RECORD_COUNTER
		; CMP #$02
		; BNE .TEMP50
		; ;LDA NPC.TALK.RECORD
		; LDA NPC.TALK.FILE.PACKED_RECORD_LENGTH
		; LDX NPC.TALK.FILE.PACKED_RECORD_LENGTH+$1
		; ;LDX NPC.TALK.FILE.RECORD_ID
		; LDY #$AA
		; JSR PREP.BRK
		; BRK
; .TEMP50	

		
	LDA NPC.TALK.FILE.RECORD_ID		;load the record ID of the first record in the NPC TALK data file
	CMP NPC.TALK.RECORD				;is the file record ID = the NPC ID of the NPC the player is talking to?
	BEQ .NPC.FOUND	;if yes, then the packed data for the desired NPC is found
					
	
	LDA NPC.TALK.FILE.RECORD_COUNTER ;load the number of records loaded from the NPC Talk data file
	CMP #MAXIMUM.NPCS.PER.BUILDING	 ;has end of file been reached?	
	BEQ .ERROR.NPC_RECORD.NOT_FOUND  ;if yes, then report error because NPC ID was not found in the file. 

	
	JMP .NPC_ID.SEARCH.LOOP

.NPC.FOUND
;IS PACKED DATA LARGER THAN PACKED DATA BUFFER?
	LDA NPC.TALK.FILE.PACKED_RECORD_LENGTH+$1	;load HO byte of packed data length (= # of pages)
	CMP /NPC.TALK.ARRAY.PACKED.SIZE				;is >= HO byte of packed data buffer size (= $ of pages)	
	BCS .ERROR.TALK_DATA.OVERFLOW				;if yes, then the data will overflow the buffer, report error. 
	;**FALLS THROUGH**
	
			; LDA #$AA
			; JSR PREP.BRK
			; BRK
			
;READ PACKED TALKED DATA FOR THIS NPC

;READ (packed date length bytes) (current file) 

;set command type (READ | WRITE | SEEK)
; cmd_seek.current_drive 	.EQ $90
; cmd_read.current_drive  	.EQ $91
; cmd_write.current_drive 	.EQ $92
;			
; cmd_read.drive1			.EQ $1
; cmd_read.drive2		  	.EQ $81
; cmd_write.drive1			.EQ $2
; cmd_write.drive2			.EQ $82
	lda #cmd_read.current_drive	
	sta parm.reqcmd
	
;set destination memory address
	lda #NPC.TALK.ARRAY.PACKED
	sta parm.ldrlo
	lda /NPC.TALK.ARRAY.PACKED	
	sta parm.ldrhi

;set read length (bytes)
	lda NPC.TALK.FILE.PACKED_RECORD_LENGTH		;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda NPC.TALK.FILE.PACKED_RECORD_LENGTH+$1	;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi
	
		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO	

;UNPACK TALK DATA
;(packed data must be higher in memory)

	lda     #NPC.TALK.ARRAY.PACKED			;lo byte (start_of_packed_data). Note: packed data must be higher in memory)
	sta     SRC.ZX7
	lda     /NPC.TALK.ARRAY.PACKED			;ho byte (start_of_packed_data). Note: packed data must be higher in memory)
	sta     SRC.ZX7+$1

	lda     #NPC.TALK.ARRAY					;lo byte (place_to_unpack_data)
	sta    	DST.ZX7
	lda     /NPC.TALK.ARRAY				;ho byte (place_to_unpack_data)
	sta     DST.ZX7+$1

	JSR UNPACK.ZX7
	
;IS UNPACKED DATA LARGER THAN UNPACKED DATA BUFFER?
;Note: the last 3 bytes at the end of the unpacked data buffer are init to $FD at the start of this subroutine. 
;if the value of any of these bytes is no longer $FD that strongly indicates an overflow has occured.
	LDA NPC.TALK.ARRAY.END				
	CMP #$FD
	BNE .ERROR.TALK_DATA.OVERFLOW				
	LDA NPC.TALK.ARRAY.END-$1			
	CMP #$FD
	BNE .ERROR.TALK_DATA.OVERFLOW				
	LDA NPC.TALK.ARRAY.END-$2				
	CMP #$FD
	BNE .ERROR.TALK_DATA.OVERFLOW				
		
;setup index to the NPC Record's unpacked talk data
	LDA #NPC.TALK.ARRAY	
	STA NPC.TALK.RECORD.INDEX+$0	;Save lo byte index to NPC Record
	
	LDA /NPC.TALK.ARRAY	
	STA NPC.TALK.RECORD.INDEX+$1	;Save ho byte index to NPC Record
	
	JMP .EXIT

.ERROR.NPC_RECORD.NOT_FOUND
;.NPC_ID.SEARCH.LOOP (NPC.TALK.SEARCH) reports that NPC record value in NPC.TALK.RECORD is not found in the NPC.TALK.ARRAY loaded from disk for the current location. 
	JSR PREP.BRK
	LDA NPC.TALK.RECORD
	BRK
	
.ERROR.TALK_DATA.OVERFLOW
;.NPC.FOUND (NTALK.SEARCH) reports that packed data for NPC that player is trying to talk to is larger
;than the packed data buffer size, or the unpacked data size is larger than the unpacked data buffer.
	JSR PREP.BRK
	BRK

;--------ALTERNATE COMPRESSION SCHEME CODE-----
;This is the code that would be needed to invoke
;the funtionality to NPC.SEARCH.LOOP to search for the
;$R command in the unpacked data and for NPC.SEARCH.LOOP to return
;a pointer to the start of the Active NPC's record. 
;For more details see "ALTERNATE COMPRESSION SCHEME" in the NTALK.SEARCH subroutine documentation. 
;

;***WARNING*** This code was written before NTALK.ARRAY.POINTER became a persistint value and NTALK.ARRAY.POINTER.RETURN became the pointer value updated with each call to NPC.ARRAY.SEARCH. Some changes may be needed to this alternate compression code. 
		; ;setup parms
		; ;setup zero-page pointer @ start of array
		; LDA #NPC.TALK.ARRAY
		; STA NTALK.ARRAY.POINTER		

		; LDA /NPC.TALK.ARRAY
		; STA NTALK.ARRAY.POINTER+$1
	
		; ;set keyword parameters
		; LDA #NTALK.COMMAND.START_CODE
		; STA NPC.TALK.KEYWORD+$0
		
		; LDA #NTALK.COMMAND.FIND_RECORD
		; STA NPC.TALK.KEYWORD+$1

		; LDA #NPC.TALK.KEYWORD.STOP_VALUE
		; STA NPC.TALK.KEYWORD+$2
		
		; LDA #NTALK.END.OF.FILE
		; STA NTALK.ARRAY.STOP.VALUE		
	; JSR NTALK.SEARCH.LOOP
		; ;ACC = $00 keyword found, $01 = keyword not found
		; STA SAVED.ACC.LOCAL	;save keyword found/not found return value ($00 keyword found, $01 = keyword not found)
		; ;save return values
		; LDA NTALK.ARRAY.POINTER+$0	
		; STA NPC.TALK.RECORD.INDEX+$0	;Save lo byte index to NPC Record
		
		; LDA NTALK.ARRAY.POINTER+$1	
		; STA NPC.TALK.RECORD.INDEX+$1	;Save ho byte index to NPC Record
		
	; JMP .EXIT
	
@END
			
.FIND.INTRO.TEXT
@START
		;****POINTER NEEDS TO BE START OF NPC RECORD
		
		; ;SETUP ZERO-PAGE POINTER @ START OF ARRAY
		; LDA NPC.TALK.RECORD.INDEX+$0	;this is a bit inefficient, but it is setup to support the unpacked data being in a dynamic location, which would be needed of the ALTERNATE COMPRESSION SCHEME (see NTALK.SEARCH subroutine docs) were implemented.
		; STA NTALK.ARRAY.POINTER+$0	
		
		; LDA NPC.TALK.RECORD.INDEX+$1	;this is a bit inefficient, but it is setup to support the unpacked data being in a dynamic location, which would be needed of the ALTERNATE COMPRESSION SCHEME (see NTALK.SEARCH subroutine docs) were implemented.
		; STA NTALK.ARRAY.POINTER+$1
		
		;set keyword parameters
		LDA #TEXT_BLOCK.INTRO
		STA NTALK.KEYWORD.POINTER+$0

		LDA /TEXT_BLOCK.INTRO
		STA NTALK.KEYWORD.POINTER+$1

		; LDA #NTALK.COMMAND.START_CODE
		; STA NPC.TALK.KEYWORD+$0
		
		; LDA #NTALK.COMMAND.FIND_INTRO
		; STA NPC.TALK.KEYWORD+$1

		; LDA #NPC.TALK.KEYWORD.STOP_VALUE
		; STA NPC.TALK.KEYWORD+$2
		
		LDA #NTALK.END.OF.RECORD
		STA NTALK.ARRAY.STOP.VALUE		
	JSR NTALK.SEARCH.LOOP			
		;ACC = $00 keyword found, $01 = keyword not found
		STA SAVED.ACC.LOCAL	;save keyword found/not found return value ($00 keyword found, $01 = keyword not found)

		
		LDA NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS+$9 ;load return flag for the NPC chat terminate subcommand
		CMP #$01 ;does subcommand apply to this text block?
		BCC .NOT.FOUND
		JSR .NPC.TERMINATE.CHAT ;if yes, then check probability of termination
		
.NOT.FOUND	


			
		; ;save return values
		; ;Note: save the array pointer as it is the index which will be used for PRINT.TEXT.WINDOW. Don't update NPC.TALK.RECORD.INDEX because we want future searches to begin at the start of the NPC record

		; LDA NTALK.ARRAY.POINTER+$0	
		; STA NPC.TALK.TEXT.INDEX+$0	;Save lo byte index to intro text
		
		; LDA NTALK.ARRAY.POINTER+$1	
		; STA NPC.TALK.TEXT.INDEX+$1	;Save ho byte index to intro text

	JMP .EXIT
@END

.SEARCH.QUESTION_RESPONSE.TEXT
@START

			; LDA #$01
			; STA TROUBLESHOOTING.HOOK
			
.INIT.VARIABLES

	;MAKE ORIGINAL COPY OF PLAYER'S KEYWORD INPUT
	;Note: this is necessary because the search routines will modify the contents of the keyword array in order to facilitate internal searches, such as for subcommand codes. 
	;thus, if this routine needs to run the same search again, the keyword copy will be needed to reset NPC.TALK.KEYWORD (such as may occur if the first keyword match is for a response that required a voice mode that is not enabled) 
	;additionally, a copy which is preserved as the original is also needed for the keyword_insert subcommand to work as it inserts the player input keyword into the text bloxk.
	
	;ACC = $00 
	;TAX ;init X-REG to $00
	LDY #$00
.KEYWORD.COPY.LOOP
	LDA (NTALK.KEYWORD.POINTER),Y
	STA NPC.TALK.KEYWORD.ORIGINAL,Y
	INY
	CMP #NPC.TALK.KEYWORD.STOP_VALUE
	BNE .KEYWORD.COPY.LOOP

	
	JSR NTALK.PARSE.KEYWORD	;searches inside player keyword for recognized synonyms and curse words, and modify the player keyword if found to trigger the desired NPC response. 


			
.CHECK.KEYWORD.BYE
		
	;did player press only the return/enter key?
	LDA NPC.TALK.KEYWORD+$02
	CMP #NPC.TALK.KEYWORD.STOP_VALUE ;NPC.TALK.KEYWORD stop value. Return isn't written to the array as it is the key that terminates input. 
	BEQ .SET.BYE.KEYWORD ;if return was pressed, treat it as though the player entered the word "BYE" 
					 ;if no, check to see if the player input was "BYE"

	LDX #$02 ;start at element #$02 of NPC.TALK.KEYWORD array because NPC.TALK.INPUT prepends "$!", the NTALK keyword command code, to the keyword the player entered.
.SEARCH.BYE.LOOP
	LDA NPC.TALK.KEYWORD,X
	AND #$DF ;masks out the 5th bit because the binary value of #$DF has 1 in each bit position except for bit 5.
	CMP TEXT_BLOCK.BYE,X ;contains "$!BYE"
	BNE .STANARD.KEYWORD	;if no, then player didn't enter "BYE", treat input as a standard keyword
	INX ;advance to next letter in keyword
	CPX #$05 ;have characters 2-4 been checked for "BYE:
	BNE .SEARCH.BYE.LOOP ;if no, then continue loop
	
	
	; LDX #$02 ;start at element #$02 of NPC.TALK.KEYWORD array because NPC.TALK.INPUT prepends "$!", the NTALK keyword command code, to the keyword the player entered.
	; LDA NPC.TALK.KEYWORD,X
	; AND #$DF ;masks out the 5th bit because the binary value of #$DF has 1 in each bit position except for bit 5.
	; CMP #$C2 ;ASCII ('B' key), is the first letter of player keyword "K"
	; BNE .STANARD.KEYWORD	;if no, then player didn't enter "BYE", treat input as a standard keyword
	; INX ;advance to 2nd letter (element #$03 of array)	

	; LDA NPC.TALK.KEYWORD,X
	; AND #$DF ;masks out the 5th bit because the binary value of #$DF has 1 in each bit position except for bit 5.
	; CMP #$D9 ;ASCII ('Y' key), is the first letter of player keyword "K"
	; BNE .STANARD.KEYWORD	;if no, then player didn't enter "BYE", treat input as a standard keyword
	; INX ;advance to 2nd letter (element #$03 of array)	

	; LDA NPC.TALK.KEYWORD,X
	; AND #$DF ;masks out the 5th bit because the binary value of #$DF has 1 in each bit position except for bit 5.
	; CMP #$C5 ;ASCII ('E' key), is the first letter of player keyword "K"
	; BNE .STANARD.KEYWORD	;if no, then player didn't enter "BYE", treat input as a standard keyword

	;**FALLS THROUGH**		;if yes, then player entered "BYE", set flag which will cause NPC.TALK to exit the conversation after the text for "$!BYE" is displayed
	;(technically could JMP directly to .BYE.KEYWORD, since keyword is already "BYE", but it doesn't hurt so saving the memory and disk space of adding the extra opcode)

.SET.BYE.KEYWORD ;set NPC.TALK.KEYWORD to the ASCII value for "BYE"

		;set keyword parameters
		
		LDA #TEXT_BLOCK.BYE
		STA NTALK.KEYWORD.POINTER+$0

		LDA /TEXT_BLOCK.BYE
		STA NTALK.KEYWORD.POINTER+$1

		
		; ;keyword command code already set
		; LDA #$C2 ;ASCII ('B' key), is the first letter of player keyword "K"
		; STA NPC.TALK.KEYWORD+$2
	
		; LDA #$D9 ;ASCII ('Y' key), is the first letter of player keyword "K"
		; STA NPC.TALK.KEYWORD+$3

		; LDA #$C5 ;ASCII ('E' key), is the first letter of player keyword "K"
		; STA NPC.TALK.KEYWORD+$4

		; LDA #NPC.TALK.KEYWORD.STOP_VALUE	;stop value needs to be set again because it is currently in NPC.TALK.KEYWORD+$02, which is overwritten by the keyword "BYE"
		; STA NPC.TALK.KEYWORD+$5
	
		;**FALLS THROUGH**
		
.BYE.KEYWORD				
		LDA #$01
		STA NTALK.FLAG.BYE		;set flag because player entered "BYE" in NPC.TALK.INPUT
			
.STANARD.KEYWORD
;MAKE WORKING COPY OF PLAYER'S KEYWORD INPUT
;Note: this is necessary because the search routines will modify the contents of the keyword array in order to facilitate internal searches, such as for subcommand codes. 
;thus, if this routine needs to run the same search again, the keyword copy will be needed to reset NPC.TALK.KEYWORD (such as may occur if the first keyword match is for a response that required a voice mode that is not enabled) 
;additionally, a copy which is preserved as the original is also needed for the keyword_insert subcommand to work as it inserts the player input keyword into the text bloxk.
	LDY #$00
.KEYWORD.COPY.LOOP5
	LDA (NTALK.KEYWORD.POINTER),Y
	STA NPC.TALK.KEYWORD.COPY,Y
	INY
	CMP #NPC.TALK.KEYWORD.STOP_VALUE
	BNE .KEYWORD.COPY.LOOP5

	
		; ;****POINTER IS START OF NPC RECORD

		; ;SETUP ZERO-PAGE POINTER @ START OF ARRAY
		; LDA NPC.TALK.RECORD.INDEX+$0 	;this is a bit inefficient, but it is setup to support the unpacked data being in a dynamic location, which would be needed of the ALTERNATE COMPRESSION SCHEME (see NTALK.SEARCH subroutine docs) were implemented.
		; STA NTALK.ARRAY.POINTER+$0	
		
		; LDA NPC.TALK.RECORD.INDEX+$1	;this is a bit inefficient, but it is setup to support the unpacked data being in a dynamic location, which would be needed of the ALTERNATE COMPRESSION SCHEME (see NTALK.SEARCH subroutine docs) were implemented.
		; STA NTALK.ARRAY.POINTER+$1
		
		LDA #NTALK.END.OF.RECORD
		STA NTALK.ARRAY.STOP.VALUE		
	
.STANARD.KEYWORD.CONTINUE.SEARCH.ENTRANCE ;entering here means search array pointer value is not reset		
	
	JSR NTALK.SEARCH.LOOP
		;ACC = $00 keyword found, $01 = keyword not found
		BEQ .CHECK.SUBCOMMAND.FLAGS ;if keyword found check subcommand return flags to see if the text block is permitted.
		JMP .DEFAULT.RESPONSE	;if keyword not found, rerun search for NPC's default response.
		STA SAVED.ACC.LOCAL	;save keyword found/not found return value ($00 keyword found, $01 = keyword not found)

.CHECK.SUBCOMMAND.FLAGS
;=====================CODE-SECTION DOCUMENTATION====================================
;
;If a keyword is found, this code section makes
;the determination on whether the text block associated
;with the keyword is permitted based on the flags set by any
;subcommands in the talk data for this keyword. 
;
;The voice mode checks are done first. 
;	*If a voice mode is required and the requirement is satisifed then event flag checks are performed. If the requirement is not satified then the text block is not permitted.
;	*If no voice mode is required then, event flag checks are performed.
;	
;Event flag checks
;	*If an event flag contingency exists for the keyword, then the text block is only permitted if the event flag has the required value, as specified by the subcommand return flags. 
;
;Text Block (response) Permitted
;	*If a push event flag subcommand applies to the keyword then the specified event flag's value is updated at this time. 
;	*Return pointer to the text block and exit
;
;=================================================================================


			; LDX #$AC
			; JSR PREP.BRK
			; LDY NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS+$9
			; BRK
			
;====KEYWORD INSERT CHECK====

.CHECK.KEYWORD_INSERT
;Note: if both flags are $00 then the keyword insert subcommand wasn't found


			
	LDA NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS+$7	;load the LO byte of the # of characters from the start of the text block to the insertion point. 
	BNE .KEYWORD_INSERT.FOUND
	LDA NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS+$8	;load the HO byte of the # of characters from the start of the text block to the insertion point. 
	BEQ .VOICE_MODE.CHECKS
	;**FALLS THROUGH**
	
.KEYWORD_INSERT.FOUND	
	JSR .EXECUTE.KEYWORD_INSERT
		;returns a modified text block with keyword inserted, and an updated pointer containing the address to the modified text block. 
	
;====VOICE MODE CHECKS====
.VOICE_MODE.CHECKS
;IS NORMAL MODE REQUIRED FOR THIS TEXT BLOCK?
.CHECK.NORMAL
	LDA NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS+$0E	;is normal flag off?
	BEQ .CHECK.WHISPER					;if no, then next check
	LDA NTALK.VOICE_MODE			;if yes, load player's current voice mode
	CMP #NTALK.VOICE.NORMAL		;does player have voice_mode set to normal?
	BEQ .VOICE_MODE.CHECKS.COMPLETE	;if yes, then the voice mode requirement is met. Now check for any independent contingencies.
	JMP .TEXT_BLOCK.NOT_PERMITTED
	

;IS WHISPER REQUIRED FOR THIS TEXT BLOCK?
.CHECK.WHISPER
	LDA NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS+$05		;is whisper flag off?
	BEQ .CHECK.YELL					;if no, then next check
	LDA NTALK.VOICE_MODE			;if yes, load player's current voice mode
	CMP #NTALK.VOICE.WHISPER		;does player have voice_mode set to whisper?
	BEQ .VOICE_MODE.CHECKS.COMPLETE	;if yes, then the voice mode requirement is met. Now check for any independent contingencies.
	JMP .TEXT_BLOCK.NOT_PERMITTED
	
.CHECK.YELL
;IS YELL REQUIRED FOR THIS TEXT BLOCK?
	LDA NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS+$04		;is yell flag off?
	BEQ .VOICE_MODE.CHECKS.COMPLETE ;if no, then next check
	LDA NTALK.VOICE_MODE			;if no, load player's current voice mode
	CMP #NTALK.VOICE.YELL			;does player have voice_mode set to whisper?
	BEQ .VOICE_MODE.CHECKS.COMPLETE	;if yes, then the voice mode requirement is met. Now check for any independent contingencies.
	JMP .TEXT_BLOCK.NOT_PERMITTED	
.VOICE_MODE.CHECKS.COMPLETE	
	;**FALLS THROUGH**
	
;====INDEPENDENT CHECKS====
.EVENT.FLAG.CHECKS
.CHECK.EVENT_FLAG.CONTINGENT
;IS A SPECIFIC EVENT FLAG VALUE REQUIRED FOR THIS TEXT BLOCK?
;NOTE: if event flag contingency is satisfied, any voice mode restrictions on the text block still apply, but those were already checked for above. 
	LDA NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS+$0	;is event contingency flag off?
	CMP #$FF				;this is the init value for the event flag related subcommands. If the an event flag subcommand is present, the values is always changed to something else. 
	BEQ .EVENT.CHECKS_COMPLETE	;if no, then next check

	LDX	NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS+$0	;if yes, load player's current voice mode
	LDA EVENT.FLAGS,X		;load the value of the event flag which the contingency is attached to
	CMP NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS+$1 ;is the value of the event flag = to the required value to satisfy the contingency?
	BEQ .EVENT.CHECKS_COMPLETE	;if yes, then exit back to NPC.TALK using this response.

	JMP .TEXT_BLOCK.NOT_PERMITTED	
.EVENT.CHECKS_COMPLETE

.CHECK.ADULT_MODE
;IS ADULT GAME MODE REQUIRED FOR THIS TEXT BLOCK?
	LDA NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS+$06		;is adult mode flag off?
	BEQ .ADULT.CHECK_COMPLETE ;if no, then next check
	LDA GAME.ADULT.MODE		;if yes, load player's current voice mode
	BNE .ADULT.CHECK_COMPLETE	;is adult game mode active? (yes if flag = $01). ;if yes, then the adult game mode requirement is met. Now check for other independent contingencies.
	JMP .TEXT_BLOCK.NOT_PERMITTED ;if not, text block is not permitted.
.ADULT.CHECK_COMPLETE


.CHECK.HOUR.CONTINGENT		
;IS THE CURRENT HOUR REQUIRED TO BE A SPECIFIC VALUE FOR THIS TEXT BLOCK?
	LDA NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS+$A	;is hour contingency flag off?
	CMP #$FF				;this is the init value for the hour contingnecy subcommand. If the the hour contingency subcommand is present, the values is always changed to something else. 
	BEQ .HOUR.CHECKS_COMPLETE	;if no, then next check

.START.TIME.CHECK	
	LDA TIME.CURRENT.HOUR
	CMP NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS+$A ;is current hour < than lower end of range?
	BCC .TEXT_BLOCK.NOT_PERMITTED ;if yes, then current hour is not within the range paramaters specified by the subcommand	
	BNE .CHECK.UPPER.HOUR 		;if the current hour is = the lowest hour in the range, then we need to check the minute values. 
	LDA TIME.CURRENT.MINUTE
	CMP NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS+$B ;is current minute >= than this value?	
	BCS .CHECK.UPPER.HOUR 		;if yes, then check the upper end of the range.
	JMP .TEXT_BLOCK.NOT_PERMITTED ;if no, then current minute is not within the paramaters specified by the subcommand	
.CHECK.UPPER.HOUR
	LDA NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS+$C 
	CMP TIME.CURRENT.HOUR 	;is upper end of range < the current hour?
	BCC .TEXT_BLOCK.NOT_PERMITTED ;if yes, then current hour is not in the permitted range
	BEQ .CHECK.UPPER.MINUTE	;is current hour equal to the hour at the upper end of range? If yes, then we need to check the minutes at the upper end of range too.
	JMP .HOUR.CHECKS_COMPLETE ;if no, then hour is < upper end of permitted range and this contingency is satisfied.
.CHECK.UPPER.MINUTE			
	LDA TIME.CURRENT.MINUTE
	CMP NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS+$D ;is current minute >= than this value?	
	BCC .HOUR.CHECKS_COMPLETE ;if no, then the current hour and minute are within the range specified by the subcommand. The contingency is satisfied.
	JMP .TEXT_BLOCK.NOT_PERMITTED ;if yes, then current minute is not within the paramaters specified by the subcommand	
.HOUR.CHECKS_COMPLETE


;****MUST ALWAYS BE LAST CHECK**
;(that way all other subcommand can be used on the text block)			
.CHECK.NPC_TERMINATE
	LDA NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS+$9 ;load return flag for the NPC chat terminate subcommand
	CMP #$01 ;does subcommand apply to this text block?
	BCC .NPC_TERMINATE.CHECK.COMPLETE
	JSR .NPC.TERMINATE.CHAT ;if yes, then check probability of termination
							;if control returns from the above JSR, then it means the NPC didn't terminate the conversation and since there are no other checks, the text block permitted, using the standard routine below. The above JSR has special code to display the text block if the NPC does termiante the chat.

	;**FALLS THROUGH**
.NPC_TERMINATE.CHECK.COMPLETE
	;all check passed
	JMP .TEXT_BLOCK.PERMITTED ;exit back to NPC.TALK using this response.


.TEXT_BLOCK.NOT_PERMITTED
		;INC TROUBLESHOOTING.HOOK
		
			
;RESET KEYWORD
;Note: this is necessary because the search routines will modify the contents of the keyword array in order to facilitate internal searches, such as for subcommand codes. 
;thus, if this routine needs to run the same search again, the keyword copy will be needed to reset NPC.TALK.KEYWORD (such as may occur if the first keyword match is for a response that required a voice mode that is not enabled) 

	LDY #$00		;init NPC.TALK.KEYWORD index
.KEYWORD.COPY.LOOP2
	LDA NPC.TALK.KEYWORD.COPY,Y
	STA (NTALK.KEYWORD.POINTER),Y
	INY
	CMP #NPC.TALK.KEYWORD.STOP_VALUE
	BNE .KEYWORD.COPY.LOOP2		


			
	;set search array pointer
	;(we set the pointer to the return value because we need to pickup the search where we left off. Otherwise if there are multiple text blocks for the same keyword the loop could get stuck on the first one with a unsatisfied contingency and never be able to search past it)
	LDA NTALK.ARRAY.POINTER.RETURN+$0	
	STA NTALK.ARRAY.POINTER+$0

	LDA NTALK.ARRAY.POINTER.RETURN+$1	
	STA NTALK.ARRAY.POINTER+$1
	
		
	JMP .STANARD.KEYWORD.CONTINUE.SEARCH.ENTRANCE
		
		;save return values
		;Note: save the array pointer as it is the index which will be used for PRINT.TEXT.WINDOW. Don't update NPC.TALK.RECORD.INDEX because we want future searches to begin at the start of the NPC record

		
.TEXT_BLOCK.PERMITTED	
;==EVENT FLAG PUSH CHECK==
.CHECK.EVENT_FLAG.PUSH
;DOES TEXT BLOCK TRIGGER A VALUE TO BE PUSHED TO AN EVENT FLAG?
				
	LDA NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS+$2 ;is event push flag off?
	CMP #$FF				;this is the init value for the event flag related subcommands. If the an event flag subcommand is present, the values is always changed to something else. 
	BEQ .EVENT_PUSH.CHECK_COMPLETE	;if no, then next check
	
	;set event flag value based on the subcommands parameters
	LDX NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS+$2
	LDA NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS+$3
	STA EVENT.FLAGS,X	
.EVENT_PUSH.CHECK_COMPLETE
			
	JMP .EXIT

.DEFAULT.RESPONSE	;used when player's keyword isn't found

			;INC TROUBLESHOOTING.HOOK
		
		;set keyword parameters

		LDA #TEXT_BLOCK.DFL
		STA NTALK.KEYWORD.POINTER+$0

		LDA /TEXT_BLOCK.DFL
		STA NTALK.KEYWORD.POINTER+$1

	
		; ;keyword command code already set
		; LDA #$C4 ;ASCII ('D' key), is the first letter of player keyword "K"
		; STA NPC.TALK.KEYWORD+$2

		; LDA #$C6 ;ASCII ('F' key), is the first letter of player keyword "K"
		; STA NPC.TALK.KEYWORD+$3

		; LDA #$CC ;ASCII ('L' key), is the first letter of player keyword "K"
		; STA NPC.TALK.KEYWORD+$4

		; LDA #NPC.TALK.KEYWORD.STOP_VALUE	;stop value needs to be set again because it is currently in NPC.TALK.KEYWORD+$02, which is overwritten by the keyword "BYE"
		; STA NPC.TALK.KEYWORD+$5	

;=====pending delete, cool down====
	; LDX #$00
; .KEYWORD.COPY.LOOP3
	; LDA NPC.TALK.KEYWORD,X
	; STA NPC.TALK.KEYWORD.COPY,X
	; INX
	; CMP #NPC.TALK.KEYWORD.STOP_VALUE
	; BNE .KEYWORD.COPY.LOOP3





			
;RETURN TO QUESTION SEARCH LOOP
	;Note: by doing the JMP to JMP .STANARD.KEYWORD that will result in NPC.TALK.KEYWORD.COPY being updated
	;this is necessary for the following reason.
	;since the default response routine has been reached, we've
	;established that the player's keyword wasn't found. Accordingly any further searches will be for
	;the default keyword. And, there may need to be multiple searches for the default keyword if the 
	;first keyword match is for a response that required a voice mode that is not enabled
	;In that event, the keyword copy will be reset NPC.TALK.KEYWORD.COPY	
	JMP .STANARD.KEYWORD
@END


;======SUBROUTINES CALLED BY THE MODE ROUTINES ABOVE=====

.NPC.TERMINATE.CHAT ;check if NPC will terminate chat and execute term
@START		


			; LDA SAVED.ACC.LOCAL
			; LDX #$AB
			; JSR PREP.BRK
			; ; LDX NTALK.ARRAY.POINTER+$1
			; ; LDY NTALK.ARRAY.POINTER+$1
			; ;SEARCH.RETURN.SUB_COMMAND.FLAGS+$9
			; BRK
			
;CHECK PROBABILITY OF TERMINATION


			
	;get random #
	JSR RANDOM.8
	;ACC = RANDOM NUMBER
	CMP NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS+$9 ;load probability that the NPC termintes chat
	BCS .EXECUTE.TERMINATION ;if random # is >= then terminate chat
					;if not, then no termination. Return to calling routine.

			; LDX #$AA
			; JSR PREP.BRK
			; LDY NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS+$9
			; BRK
			
	RTS ;to .FIND.INTRO.TEXT or
	

.EXECUTE.TERMINATION ;NPC TERMINATES CHAT

			; LDX #$AB
			; JSR PREP.BRK
			; LDY NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS+$9
			; BRK

			;JSR KEYIN

;ADVANCE TWO TEXT WINDOW LINES, AND PRINT SEPERATOR
.CR.PRINT
		LDA #CR.TEXT					
		STA TWF.STRING

		LDA /CR.TEXT
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW
	
;PRINT TEXT BLOCK WITH TERMINATION TRIGGER
.PRINT.TRIGGER.TEXT
		;set parms
		;load index to text into the input pointer for the text window routine
		LDA NTALK.ARRAY.POINTER.RETURN+$0					
		STA TWF.STRING
		
		LDA NTALK.ARRAY.POINTER.RETURN+$1
		STA TWF.STRING+$1							
	JSR PRINT.TEXT.WINDOW

			;JSR KEYIN
	LDA NTALK.SEARCH.MODE
	CMP #$01
	BNE .GET.TERMINATE.TEXT_BLOCK
	
;ADVANCE TWO TEXT WINDOW LINES, AND PRINT SEPERATOR
.CR.PRINT2
		LDA #CR.TEXT					
		STA TWF.STRING

		LDA /CR.TEXT
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW


.GET.TERMINATE.TEXT_BLOCK	
;GET TERMINATION TEXT BLOCK (%END)

		;set keyword parameters

		LDA #TEXT_BLOCK.END
		STA NTALK.KEYWORD.POINTER+$0

		LDA /TEXT_BLOCK.END
		STA NTALK.KEYWORD.POINTER+$1

		
		; LDA #NTALK.COMMAND.START_CODE 	;ASCII = $.	This is needed in case NPC term subcommand is in the intro text block and NPC.TALK hasn't been called yet, which normally adds the commmand codes for keyword to NPC.TALK.KEYWORD
		; STA NPC.TALK.KEYWORD+$0
		
		; LDA #NTALK.COMMAND.FIND_KEYWORD ;ASCII = !.	""
		; STA NPC.TALK.KEYWORD+$1
		
		; LDA #$A5 ;ASCII ('%' key)
		; STA NPC.TALK.KEYWORD+$2

		; LDA #$C5 ;ASCII ('E' key)
		; STA NPC.TALK.KEYWORD+$3

		; LDA #$CE ;ASCII ('N' key)
		; STA NPC.TALK.KEYWORD+$4

		; LDA #$C4 ;ASCII ('D' key)
		; STA NPC.TALK.KEYWORD+$5

		; LDA #NPC.TALK.KEYWORD.STOP_VALUE 
		; STA NPC.TALK.KEYWORD+$6
		
		LDA #NTALK.END.OF.RECORD
		STA NTALK.ARRAY.STOP.VALUE		
			
	JSR NTALK.SEARCH.LOOP
		;ACC: $00 = keyword found, $01 = keyword not found
		STA SAVED.ACC.LOCAL ;save keyword status as return value

			;JSR KEYIN
			
	;exit directly
	
	LDA #$01
	STA NTALK.FLAG.BYE ;set BYE flag so that NPC.TALK knows to end the conversation
	
	PLA ;pop last RTS address from stack because this routie was called with JSR but we are exiting via JMP
	PLA
	JMP .EXIT
	
@END
		
.EXECUTE.KEYWORD_INSERT ;insert player keyword into a text block
@START


			
;RESET KEYWORD
;Note: this is necessary because the search routines will modify the contents of the keyword array in order to facilitate internal searches, such as for subcommand codes
;or to find the default text block if the player keyword isn't found. 

	LDX #$00		;init NPC.TALK.KEYWORD index
.KEYWORD.COPY.LOOP4
	LDA NPC.TALK.KEYWORD.ORIGINAL,X
	STA NPC.TALK.KEYWORD,X
	INX
	BEQ .ERROR.KEYWORD_INSERT.OVERFLOW_STEP ;if index flips then report overflow error
	CMP #NPC.TALK.KEYWORD.STOP_VALUE
	BNE .KEYWORD.COPY.LOOP4	
			
;MODIFY TEXT BLOCK	

	;NTALK.ARRAY.POINTER.RETURN currently points to the start of the text block	
	LDX #$00 ;init index for NTALK.MODIFIED.TEXT_BLOCK
	LDY #$00 ;init index for NTALK.ARRAY.POINTER
.KEYWORD_INSERT.LOOP1
;loop until the keyword insert code is found ($k) in the text block
	LDA (NTALK.ARRAY.POINTER.RETURN),Y		;load character from original text block
	STA NTALK.MODIFIED.TEXT_BLOCK,X		;save character to modified text block
	INY ;increment index
	INX ;increment index
	BEQ .ERROR.KEYWORD_INSERT.OVERFLOW_STEP ;if index flips then report overflow error
	CPX NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS+$7 ;has insertion point been reached? this variable contains the # of characters from the start of the text block to the insertion point. 
	BNE .KEYWORD_INSERT.LOOP1 ;if no, continue loop
			
	TYA
	PHA ;push NTALK.TEXT_BLOCK.START index to stack
	
	LDY #$02	;set to 3rd byte to skip the $! keyword command code that is always prepended to the keyword the player types in
.KEYWORD_INSERT.LOOP2
;copy player keyword to the modifed text block
	LDA NPC.TALK.KEYWORD,Y ;load character from player input keyword
	STA NTALK.MODIFIED.TEXT_BLOCK,X ;save character to the modified text block
	INY ;increment index
	INX ;increment index
	BEQ .ERROR.KEYWORD_INSERT.OVERFLOW_STEP ;if index flips then report overflow error
	CMP #NPC.TALK.KEYWORD.STOP_VALUE	;has end of keyword array been reached?
	BNE .KEYWORD_INSERT.LOOP2 ;if no, continue loop
	
	DEX ;back up one byte in NTALK.MODIFIED.TEXT_BLOCK so the stop value from NPC.TALK.KEYWORD gets overwritten.
	
	PLA ;pull NTALK.TEXT_BLOCK.START index from stack
	TAY

	INY	;advance two bytes in the text block so that the embedded subcommand code for insert keyword isn't copied into the modified text block. 
	INY
				
.KEYWORD_INSERT.LOOP3
;loop until the text block stop value is found
	LDA (NTALK.ARRAY.POINTER.RETURN),Y  ;character from original text block
	STA NTALK.MODIFIED.TEXT_BLOCK,X ;save character to modified text block
	INY ;increment index
	INX ;increment index
	BEQ .ERROR.KEYWORD_INSERT.OVERFLOW_STEP ;if index flips then report overflow error
	CMP #NPC.TEXT.BLOCK.STOP_VALUE	;has end of text block been reached?
	BNE .KEYWORD_INSERT.LOOP3 ;if no, continue loop

				; LDA #$AA
				; ldx #NTALK.MODIFIED.TEXT_BLOCK
				; ldy /NTALK.MODIFIED.TEXT_BLOCK
				; ;LDX NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS+$7
				; ;LDY NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS+$8
				; ;LDX NTALK.TEXT_BLOCK.START+$0
				; ;LDY NTALK.TEXT_BLOCK.START+$1
				; JSR PREP.BRK
				; BRK	
				
;change pointer to the modified text block
	LDA #NTALK.MODIFIED.TEXT_BLOCK 
	STA NTALK.ARRAY.POINTER.RETURN+$0

	LDA /NTALK.MODIFIED.TEXT_BLOCK 
	STA NTALK.ARRAY.POINTER.RETURN+$1



			
	;***add >$FF error trap. just BEQ after the Y-REG index



	RTS
@END

.ERROR.KEYWORD_INSERT.OVERFLOW_STEP
	JMP ERROR.KEYWORD_INSERT.OVERFLOW
	
.EXIT
			
			; LDA SAVED.ACC.LOCAL
			; LDX #$AA
			; JSR PREP.BRK
			; ; LDX NTALK.ARRAY.POINTER+$1
			; ; LDY NTALK.ARRAY.POINTER+$1
			; ;SEARCH.RETURN.SUB_COMMAND.FLAGS+$9
			; BRK

	
.PROCESS.EVENT.FLAGS
;PROCESS ANY EVENT FLAG CHANGES
@START

;****WARNING*****
;For one-time events, the event flag must be turned off after processing to prevent
;strange things from happening. For example, a one-time event like an NPC opening a door. 
;If the event flag isn't cleared, then every time the player tried to talk to an NPC, this subroutine
;will try to open the door and feed bad GMAP.X/Y values into the drawing routines as the door
;isn't on screen. That particular scenario resulted in tiles being drawn in darkness areas and the text window
;as well as full crashes randomly occuring when initiating talk with an NPC.
 


		LDA EVENT.FLAGS+$02	;check event flag status
		CMP #$01
		BNE .FLAG.NOT.SET		
		LDA #$24 ;open door status code
		STA MAP_OBJECTS.GENERAL+$03 ;update door MO in main memory

		;modify door status in aux memory
			STA $C005 ;enable lower aux memory ($200 - $BFFF, zero-page and BSR status doesn't change)
		LDA #$24 ;open door status code
		STA MAP_OBJECTS.GENERAL.AUX+$03 ;update door MO in aux memory
			STA $C004 ;enable lower main memory ($200 - $BFFF, zero-page and BSR status doesn't change)

		;update door's tile graphic
		
			LDA MAP_OBJECTS.GENERAL+$00	;load door X-axis
			STA PARM.GMAP.X

			LDA MAP_OBJECTS.GENERAL+$01 ;load door Y-axis
			STA PARM.GMAP.Y
		JSR CONVERT.GMAP.TO.PLAYER_RELATIVE.XY
			;parse return values and set parms for next call
			LDA RETURN.RELATIVE.X		
			STA PARM.RELATIVE.X
		
			LDA RETURN.RELATIVE.Y		
			STA PARM.RELATIVE.Y		
		JSR CONVERT.PLAYER_RELATIVE.XY.TO.SCREEN_ARRAY_INDEX
			;parse return values and set parms for next call
			LDY RETURN.SCREEN_ARRAY_INDEX
		JSR DRAW.TILE.SINGLE	;replace door tile with underlying floor tile, which makes the door appear to be open. (FOREGROUND PAGE)
		JSR FLIP.PAGE
		JSR DRAW.TILE.SINGLE	;replace door tile with underlying floor tile, which makes the door appear to be open. (PREVIOUS BACKGROUND PAGE)
		
		;turn off event flag
		;note: this is because opening the door is a one-time event.
		LDA #$00
		STA EVENT.FLAGS+$02
		
			; LDA #$02
			; STA TROUBLESHOOTING.HOOK
		
			
			
.FLAG.NOT.SET
@END


;SETUP POINTER TO TEXT BLOCK AS RETURN VALUE
	LDA NTALK.ARRAY.POINTER.RETURN+$0 ;this is a bit inefficient, but it is setup to support the unpacked data being in a dynamic location, which would be needed of the ALTERNATE COMPRESSION SCHEME (see NTALK.SEARCH subroutine docs) were implemented.	
	STA NPC.TALK.TEXT.INDEX+$0	;Save lo byte index to intro text
	
	LDA NTALK.ARRAY.POINTER.RETURN+$1 ;this is a bit inefficient, but it is setup to support the unpacked data being in a dynamic location, which would be needed of the ALTERNATE COMPRESSION SCHEME (see NTALK.SEARCH subroutine docs) were implemented.	
	STA NPC.TALK.TEXT.INDEX+$1	;Save ho byte index to intro text

	
;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX

	LDA SAVED.ACC.LOCAL	;restore keyword found/not found return value ($00 keyword found, $01 = keyword not found)
			
	RTS
@END
	
NTALK.SEARCH.LOOP
@START

;PARMATERS: NTALK.ARRAY.POINTER(2) = index to array start position, NPC.TALK.KEYWORD, NTALK.SEARCH.MODE
;RETURN: ACC ($00 = keyword found, $01 = keyword not found), NTALK.ARRAY.POINTER(2) (index to the next element to be searched (i.e. the last elemenet to be searched +$1)), NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS(x)
;ENTRANCE: DIRECT, NTALK.SEARCH

;=====================SUBROUTINE DOCUMENTATION====================================
;
;==OVERVIEW==
;Calls NTALK.ARRAY.SEARCH to search the 16-bit array specified by parameter NTALK.ARRAY.POINTER for the keyword specified by the parameter arrray NPC.TALK.KEYWORD(x)
;Return value (ACC) = $00 if keyword found, $01 if keyword not found
;Return value (NTALK.ARRAY.POINTER) = the memory address of the last character of the keyword found + $1
;
;This routine is a wrapper to NTALK.ARRAY.SEARCH which is called for the purpose of
;searching for NTALK commands within the array, and if a given command is found, check the subsequent characters for
;NTALK subcommand codes. Accordingly, this routine refers to keywords as commands, but conceptually a keyword search is being done
;and NPC.TAlK.KEYWORD just happens to be loaded with the ASCII values that represent an NTALK command. 
;For example, if NTALK.SEARCH.MODE = $03 (search for player question response) then the NTALK.KEYWORD might contain
;a value like "$!JOB" or "$!BOOK". "$!" is the command code for a question response. Any text which follows it will be displayed if 
;the player types in the keyword portion of the command (BOOK or JOB) in this case.  
;
;
;==Subcommands==
;After a command is found, the check for subcommand codes first looks to see if the next charcter after the end of the primary commad is ":", 
;which is the subcommand code. If this character is present, the keyword array is loaded with the characters which
;make up each subcommand (for example $W for the whisper subcommand), and NTALK.ARRAY.SEARCH is called again
;to search for the array these values. 
;
;Each subcommand is searched for one at time, and if found some processing 
;of the subcommand is done within this routine. 
;
;Multiple subcommands for the same primary command are seperated by a ":" so that each time NTALK.ARRAY.SEARCH complete,
;the top of this routine's loop will detect a subcommand and continue the process of seaching for the subcommand codes. 
;
;On each iteration to search for a subcommand, NTALK.COMMAND_FOUND.INDEX is used as the array pointer
;so that the seach starts at the character after the primary command, which is the start of the subcommand string if there are subcommands.  
;
;
;-No More Subcommands or No Subcommands Found
;
;Once there the last subcommand is found and processed or if there are no subcommands at all,
;the command end code "." is searched for. This is is the method for finding the start of the text associated
;with the command/keyword because the text always starts with the first character after the command end code.
;If there are no subcommands the command end code is the first character after the end of the
;primary command (for example "$!JOB."). If there are subcommands then the command end
;code is the first character after the end of the last subcommand (for example "$!Job:$W.")
;
;(NOT CURRENTLY IN USE) Additionally, if this routine is called in NPC Record search mode (NTALK.SEARCH.MODE = $00) then
;.NPC_RECORD_SEARCH_ONLY takes over to confirm that the NPC Record found is the NPC the player is trying to talk to (NPC.TALK.RECORD).
;If it isn't, then the search loop starts over from the position it left off and this process
;will continue until the desired NPC Record # is found or until the end of NPC.TALK.ARRAY is reached. 
;This function is not currently in use, since the NPC IDs are stored as headers in the packed data file,
;but this function would be needed to implement the ALTERNATE COMPRESION SCHEME described in the subroutine docs for NTALK.SEARCH 
;
;-Subcommand Return Flags (NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS)
;Subcommands set return flags which enable higher level routines to create functionality for them.  
;All return flags are an offset to the array ;NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS
;
;Currently the subcommand return flags are as follows.
;
;===RETURN FLAGS===
;datagram of NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS
;
;$00 = ($E) Contingent Event Flag # (Contingent for seeing text)
;$01 = ($E) Contingent Event Flag Status Required (Contingent for seeing text)
;$02 = ($P) Push Event Flag # (seeing the text triggers an event flag to be set)
;$03 = ($P) Push Event Flag Status 
;$04 = ($Y) Yell Status ($00 = not required, $01 = required to see text)
;$05 = ($W) Whisper Status ($00 = not required, $01 = required to see text)
;$06 = ($A) Adult game mode required, or off = $00
;$07-08 ($K) Distance from command end code to the insertion point of the player keyword (2 byte value, but other parts of the code only support an $FF byte text block length)
;$09 = ($T) NPC chat termination probability represented by the 8-bit hex value which the random number must be >= in order for the termination to occur.
;$0A = ($H) the hour (24 hour format) at lower end of the permitted range.
;$0B = ($H) the minute at lower end of the permitted range. Only checked if the current hour matches the lowest hour in the range. 
;$0C = ($H) the hour (24 hour format) at upper end of the permitted range.
;$0D = ($H) the minute (BCD) at upper end of the permitted range.Only checked if the current hour matches the upper hour in the range. 
;$0E = ($N) Normal (voice mode) Status ($00 = not required, $01 = required to see text)
;=================================================================================


;SAVE PERSISTENT ARRAY POINTER TO STACK
	LDA NTALK.ARRAY.POINTER+$0
	PHA
	LDA NTALK.ARRAY.POINTER+$1
	PHA
	
	LDA NTALK.KEYWORD.POINTER+$0
	PHA
	LDA NTALK.KEYWORD.POINTER+$1
	PHA
	
.INIT
	LDX #$00	;for reason it starts at $03 see.INIT.MANUAL comment below
	; LDA #$06	
	; STA NTALK.SEARCH.MATCH_QTY	; Number of characters that must be the same for a match to be found. For exampe, if this variable is set to 4 then if the first 4 characters of the keyword are found then the entire keyword is considered to be found. The number of characters must include the NTALK keyword subcommand code of "$!" 

.INIT.LOOP
	LDA #$00
	STA NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS,X
	INX
	CPX #SUB_COMMAND.FLAGS.STOP_VALUE
	BNE .INIT.LOOP

.INIT.MANUAL
	;manually set values (for contingent event and hour contingent event subcommands,) to $FF so that if either event subcommand isn't found, the $FF value can indicate that whereas $00 would indicate event flag $00 is required or will be pushed to. The rest are init to $00 because I expect the rest will be boolean flags and this sets their default state to OFF. 
	LDA #$FF
	STA NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS+$0
	STA NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS+$2
	STA NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS+$A
	STA NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS+$B

	;INIT INTERNAL SEARCH FLAG
	;certain internal searches, such as searching for the command end code, are less than the minimum charcters
	;required for a match as determined by (NTALK.SEARCH.MATCH_QTY). 
	;the internal search flag is used to override that minumum. 
	;Since the first two NTALK search modes ($00-$01) are internal searched (i.e. not driven by player keyboard input), we set the internal search flag if this routine was entered using those modes. 
	
	;ACC = $00
	STA NTALK.FLAG.INTERNAL_SEARCH		;This flag is on when internal searches, such as for the command end code, are being done. This flag is used to override the minimum characters required for a match as specified by NTALK.SEARCH.MATCH_QTY


;SET INTERNAL SEARCH FLAG IF SEARCH MODE = PLAYER QUESTION/KEYWORD SEARCH	
	LDA NTALK.SEARCH.MODE	;load active search mode
	CMP #$02				;search mode $00 = NPC Record Search, mode $01 = introdution search, both of which are internal searches. So if the active search mode is >= $02 we can conclude that the the active search mode is player question/keyword, which is not an internal search. 
	BCS .START.SEARCH
	LDA #$01
	STA NTALK.FLAG.INTERNAL_SEARCH		;This flag is on when internal searches, such as for the command end code, are being done. This flag is used to override the minimum characters required for a match as specified by NTALK.SEARCH.MATCH_QTY

.START.SEARCH
		;using parms passed to NTALK.SEARCH.LOOP
	JSR NTALK.ARRAY.SEARCH			
		;was keyword found?
		;ACC = $00 = found, $01 = not found
	
		CMP #$00		
		BEQ .SAVE1					;if yes, save the index to NPC talk array			
		JMP .COMMAND.NOT.FOUND		;if no, then exit
.SAVE1		
		;save return values
		LDA NTALK.ARRAY.POINTER.RETURN+$0	;;Save ho byte index to NPC Record
		STA NTALK.COMMAND_FOUND.INDEX+$0
		STA NTALK.ARRAY.POINTER+$0	
	
		LDA NTALK.ARRAY.POINTER.RETURN+$1	;;Save ho byte index to NPC Record
		STA NTALK.COMMAND_FOUND.INDEX+$1
		STA NTALK.ARRAY.POINTER+$1	
	
	;set keyword pointer
	;Note: this is done because the subcommand internal searches need the pointer set to NPC.TALK.KEYWORD and
	;some of the calling routines to this routine (NPC.SEARCH.LOOP) set the pointer to a different value (i.e. ASCII tables containing command values instead of setting them manually)
	LDA #NPC.TALK.KEYWORD				;array storing the player input keyword (via NPC.TALK.INPUT)
	STA NTALK.KEYWORD.POINTER+$0		;NTALK.ARRAY.SEARCH parameter: pointer to array used as keyword search string

	LDA /NPC.TALK.KEYWORD
	STA NTALK.KEYWORD.POINTER+$1

	
.SEARCH.SUBCOMMANDS
@START
			
;SEARCH FOR SUB-COMMANDS

					
;CHECK FOR SUBCOMMAND CODE

	;Y-REG = $00: reset by NTALK.ARRAY.SEARCH upon exit
	LDA (NTALK.ARRAY.POINTER.RETURN),Y		;load next character in talk array
	CMP #NTALK.COMMAND.SUBCOMMAND_CODE	;was the subcommand code found?
	BEQ .CHECK.SUBCOMMAND.EVENT_CONTINGENT	;if yes, then start checks for subcommands
	JMP .PRIMARY_COMMAND.ONLY.ENTRANCE	;skip the subcommand checks and proceed directly to the code section which searches the array for the command end character; currently "."
	
.CHECK.SUBCOMMAND.EVENT_CONTINGENT	

		;set search parms

		;already set		
		; LDA NTALK.COMMAND_FOUND.INDEX+$0
		; STA NTALK.ARRAY.POINTER+$0	

		; LDA NTALK.COMMAND_FOUND.INDEX+$1
		; STA NTALK.ARRAY.POINTER+$1
			
		;set keyword command start code
		;needs to be set because the last internal search for finding the command end code overwrites the command start code value at NPC.TALK.KEYWORD+$0
		LDA #NTALK.COMMAND.START_CODE
		STA NPC.TALK.KEYWORD+$0

		LDA #NTALK.SUB_COMMAND.EVENT_CONT_CODE
		STA NPC.TALK.KEYWORD+$1

		;already set
		LDA #NPC.TALK.KEYWORD.STOP_VALUE
		STA NPC.TALK.KEYWORD+$2

		LDA #NTALK.COMMAND.END_CODE
		STA NTALK.ARRAY.STOP.VALUE			;set the stop value to the command end code because we do not want to search past that delimiter, otherwise we'd be searching the conversation text for subcommands.
		
		LDA #$01
		STA NTALK.FLAG.INTERNAL_SEARCH		;This flag is on when internal searches, such as for the command end code, are being done. This flag is used to override the minimum characters required for a match as specified by NTALK.SEARCH.MATCH_QTY
											;even though this flag is set in the init section at the top of the routine for search mode $00, $01, we need to set the flag here because subcommand are searched for in search mode $03 as well, and subcommands require an internal search.
											
		;parm NTALK.ARRAY.POINTER(2) are already set. Don't reset the pointer values because we want to search starting at element of the NPC TALK array where we left off. 
	JSR NTALK.ARRAY.SEARCH
		;parse return values
		;ACC = $00 = found, $01 = not found
		CMP #$00				;was keyword found?
		BNE .CHECK.EVENT_CONTINGENT.COMPLETE	;if no, then next command
		
	;Y-REG = $00: reset by NTALK.ARRAY.SEARCH upon exit
	LDA (NTALK.ARRAY.POINTER.RETURN),Y ;load the event flag # that the command will be contingent on
	AND #$7F ;mask out the hi bit to convert to a hex value. This is needed because the SBASM cross assembler sets the hi-bit for the entire ascii string if negative ASCII is enabled, even for hex codes inserted into the ASCII string. 
	STA NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS+$0 ;save return value. Total flags available = $20
			
	;NEXT ARRAY ELEMENT
	INY	;increment LO byte index
	BNE .INCREMENT_COMPLETE3		;did lo byte index flip over to $00? If no, don't increment HO byte
	INC NTALK.ARRAY.POINTER.RETURN+$1	;if yes, increment HO byte index 
.INCREMENT_COMPLETE3

	LDA (NTALK.ARRAY.POINTER.RETURN),Y ;load the event flag value that the command will be contingent on
	AND #$7F ;mask out the hi bit to convert to a hex value. This is needed because the SBASM cross assembler sets the hi-bit for the entire ascii string if negative ASCII is enabled, even for hex codes inserted into the ASCII string. 
	STA NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS+$1 ;save return value. Total flags available = $20
.CHECK.EVENT_CONTINGENT.COMPLETE


			
.CHECK.SUBCOMMAND.EVENT_PUSH
		;set parms
		
		;already set
		; LDA NTALK.COMMAND_FOUND.INDEX+$0
		; STA NTALK.ARRAY.POINTER+$0	

		; LDA NTALK.COMMAND_FOUND.INDEX+$1
		; STA NTALK.ARRAY.POINTER+$1
		
		;set keyword parms

		; LDA #NTALK.COMMAND.START_CODE
		; STA NPC.TALK.KEYWORD+$0

		LDA #NTALK.SUB_COMMAND.EVENT_PUSH_CODE
		STA NPC.TALK.KEYWORD+$1

		;alredy set
		; LDA #NPC.TALK.KEYWORD.STOP_VALUE
		; STA NPC.TALK.KEYWORD+$2

		; LDA #NTALK.COMMAND.END_CODE
		; STA NTALK.ARRAY.STOP.VALUE			;set the stop value to the command end code because we do not want to search past that delimiter, otherwise we'd be searching the conversation text for subcommands.

		; LDA #$01
		; STA NTALK.FLAG.INTERNAL_SEARCH		;This flag is on when internal searches, such as for the command end code, are being done. This flag is used to override the minimum characters required for a match as specified by NTALK.SEARCH.MATCH_QTY
		
	JSR NTALK.ARRAY.SEARCH
		;parse return values
		;ACC = $00 = found, $01 = not found
		CMP #$00				;was keyword found?
		BNE .CHECK.EVENT_PUSH.COMPLETE	;if no, then next command
		
	;Y-REG = $00: reset by NTALK.ARRAY.SEARCH upon exit
	LDA (NTALK.ARRAY.POINTER.RETURN),Y ;load the event flag # that the command will set
	AND #$7F ;mask out the hi bit to convert to a hex value. This is needed because the SBASM cross assembler sets the hi-bit for the entire ascii string if negative ASCII is enabled, even for hex codes inserted into the ASCII string. 
	STA NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS+$2 ;save return value. Total flags available = $20
		
	;NEXT ARRAY ELEMENT
	INY	;increment LO byte index
	BNE .INCREMENT_COMPLETE5		;did lo byte index flip over to $00? If no, don't increment HO byte
	INC NTALK.ARRAY.POINTER.RETURN+$1	;if yes, increment HO byte index 
.INCREMENT_COMPLETE5

	LDA (NTALK.ARRAY.POINTER.RETURN),Y ;load the event flag value that the command will push
	AND #$7F ;mask out the ho nibble to convert to a hex value. This is needed because the SBASM cross assembler sets the hi-bit for the entire ascii string if negative ASCII is enabled, even for hex codes inserted into the ASCII string. 
	STA NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS+$3 ;save return value. Total flags available = $20
.CHECK.EVENT_PUSH.COMPLETE
		
.CHECK.SUBCOMMAND.YELL
;CHECK FOR YELL SUBCOMMAND

	
		;set parms

		;already set
		; LDA NTALK.COMMAND_FOUND.INDEX+$0
		; STA NTALK.ARRAY.POINTER+$0	

		; LDA NTALK.COMMAND_FOUND.INDEX+$1
		; STA NTALK.ARRAY.POINTER+$1	

		;set keyword parm	

		;alredy set
		; LDA #NTALK.COMMAND.START_CODE
		; STA NPC.TALK.KEYWORD+$0

		LDA #NTALK.SUB_COMMAND.YELL_CODE
		STA NPC.TALK.KEYWORD+$1

		;alredy set
		; LDA #NPC.TALK.KEYWORD.STOP_VALUE
		; STA NPC.TALK.KEYWORD+$2
	
		;alredy set
		; LDA #NTALK.COMMAND.END_CODE
		; STA NTALK.ARRAY.STOP.VALUE			;set the stop value to the command end code because we do not want to search past that delimiter, otherwise we'd be searching the conversation text for subcommands.

		; LDA #$01
		; STA NTALK.FLAG.INTERNAL_SEARCH		;This flag is on when internal searches, such as for the command end code, are being done. This flag is used to override the minimum characters required for a match as specified by NTALK.SEARCH.MATCH_QTY
		
		;parms Y-REG and NTALK.ARRAY.POINTER are already set. Use same values as from last call because we want to search starting at element of the NPC TALK array where we left off. 
	JSR NTALK.ARRAY.SEARCH
		;parse return values
		;ACC = $00 = found, $01 = not found
		CMP #$00					;was keyword found?
		BNE .CHECK.YELL.COMPLETE	;if no, then next command
		
		LDA #$01
		STA NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS+$4 ;save return value: yell required
.CHECK.YELL.COMPLETE

.CHECK.SUBCOMMAND.WHISPER
;CHECK FOR WHISPER SUBCOMMAND

		;set parms

		;already set
		; LDA NTALK.COMMAND_FOUND.INDEX+$0
		; STA NTALK.ARRAY.POINTER+$0	

		; LDA NTALK.COMMAND_FOUND.INDEX+$1
		; STA NTALK.ARRAY.POINTER+$1	

		;set keyword parm	
		
		;alredy set
		; LDA #NTALK.COMMAND.START_CODE
		; STA NPC.TALK.KEYWORD+$0

		LDA #NTALK.SUB_COMMAND.WHISPER_CODE
		STA NPC.TALK.KEYWORD+$1

		;alredy set
		; LDA #NPC.TALK.KEYWORD.STOP_VALUE
		; STA NPC.TALK.KEYWORD+$2
	
		;alredy set
		; LDA #NTALK.COMMAND.END_CODE
		; STA NTALK.ARRAY.STOP.VALUE			;set the stop value to the command end code because we do not want to search past that delimiter, otherwise we'd be searching the conversation text for subcommands.

	JSR NTALK.ARRAY.SEARCH
		;parse return values
		;ACC = $00 = found, $01 = not found
		CMP #$00					;was keyword found?
		BNE .CHECK.WHISPER.COMPLETE	;if no, then next command
		
		LDA #$01
		STA NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS+$5 ;save return value: whisper required	
.CHECK.WHISPER.COMPLETE


			
;CHECK FOR NORMAL (VOICE) SUBCOMMAND
	
		;set parms

		;already set
		; LDA NTALK.COMMAND_FOUND.INDEX+$0
		; STA NTALK.ARRAY.POINTER+$0	

		; LDA NTALK.COMMAND_FOUND.INDEX+$1
		; STA NTALK.ARRAY.POINTER+$1	

		;set keyword parm	
		
		;alredy set
		; LDA #NTALK.COMMAND.START_CODE
		; STA NPC.TALK.KEYWORD+$0

		LDA #NTALK.SUB_COMMAND.NORMAL_CODE
		STA NPC.TALK.KEYWORD+$1

		;alredy set
		; LDA #NPC.TALK.KEYWORD.STOP_VALUE
		; STA NPC.TALK.KEYWORD+$2
	
		;alredy set
		; LDA #NTALK.COMMAND.END_CODE
		; STA NTALK.ARRAY.STOP.VALUE			;set the stop value to the command end code because we do not want to search past that delimiter, otherwise we'd be searching the conversation text for subcommands.

	JSR NTALK.ARRAY.SEARCH
		;parse return values
		;ACC = $00 = found, $01 = not found
		CMP #$00					;was keyword found?
		BNE .CHECK.NORMAL.COMPLETE	;if no, then next command
		
		LDA #$01
		STA NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS+$E ;save return value: whisper required	
.CHECK.NORMAL.COMPLETE

.CHECK.SUBCOMMAND.ADULT
;CHECK FOR ADULT SUBCOMMAND
	
		;set parms

		;already set
		; LDA NTALK.COMMAND_FOUND.INDEX+$0
		; STA NTALK.ARRAY.POINTER+$0	

		; LDA NTALK.COMMAND_FOUND.INDEX+$1
		; STA NTALK.ARRAY.POINTER+$1	

		;set keyword parm	
		; LDA #NTALK.COMMAND.START_CODE
		; STA NPC.TALK.KEYWORD+$0

		LDA #NTALK.SUB_COMMAND.ADULT_CODE
		STA NPC.TALK.KEYWORD+$1

		;alredy set
		; LDA #NPC.TALK.KEYWORD.STOP_VALUE
		; STA NPC.TALK.KEYWORD+$2
		
		;alredy set
		; LDA #NTALK.COMMAND.END_CODE
		; STA NTALK.ARRAY.STOP.VALUE			;set the stop value to the command end code because we do not want to search past that delimiter, otherwise we'd be searching the conversation text for subcommands.

	JSR NTALK.ARRAY.SEARCH
		;parse return values
		;ACC = $00 = found, $01 = not found
		CMP #$00					;was keyword found?
		BNE .CHECK.ADULT.COMPLETE	;if no, then next command
		
		LDA #$01
		STA NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS+$6 ;save return value: whisper required	
.CHECK.ADULT.COMPLETE

.CHECK.SUBCOMMAND.NPC_TERMINATE
;CHECK FOR NPC TERMINATE CHAT SUBCOMMAND
	
		;set parms

		;already set
		; LDA NTALK.COMMAND_FOUND.INDEX+$0
		; STA NTALK.ARRAY.POINTER+$0	

		; LDA NTALK.COMMAND_FOUND.INDEX+$1
		; STA NTALK.ARRAY.POINTER+$1	

		;set keyword parm	
		; LDA #NTALK.COMMAND.START_CODE
		; STA NPC.TALK.KEYWORD+$0

		LDA #NTALK.SUB_COMMAND.NPC_TERM_CODE
		STA NPC.TALK.KEYWORD+$1

		;alredy set
		; LDA #NPC.TALK.KEYWORD.STOP_VALUE
		; STA NPC.TALK.KEYWORD+$2
	
		;alredy set
		; LDA #NTALK.COMMAND.END_CODE
		; STA NTALK.ARRAY.STOP.VALUE			;set the stop value to the command end code because we do not want to search past that delimiter, otherwise we'd be searching the conversation text for subcommands.		
	JSR NTALK.ARRAY.SEARCH
		;parse return values
		;ACC = $00 = found, $01 = not found


			
		CMP #$00					;was keyword found?
		BNE .CHECK.NPC_TERM.COMPLETE	;if no, then next command

		;Y-REG = $00: reset by NTALK.ARRAY.SEARCH upon exit
		LDA (NTALK.ARRAY.POINTER.RETURN),Y ;load the probability that NPC will terminate the chat
		;AND #$7F ;mask out the ho nibble to convert to a hex value. This is needed because the SBASM cross assembler sets the hi-bit for the entire ascii string if negative ASCII is enabled, even for hex codes inserted into the ASCII string. 
		STA NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS+$9 ;save return value. Total flags available = $20
			
.CHECK.NPC_TERM.COMPLETE

.CHECK.SUBCOMMAND.HOUR_CONTINGENT	

		;set parms

		;already set
		; LDA NTALK.COMMAND_FOUND.INDEX+$0
		; STA NTALK.ARRAY.POINTER+$0	

		; LDA NTALK.COMMAND_FOUND.INDEX+$1
		; STA NTALK.ARRAY.POINTER+$1	

		;set keyword parm
		
		;alredy set
		; LDA #NTALK.COMMAND.START_CODE
		; STA NPC.TALK.KEYWORD+$0

		LDA #NTALK.SUB_COMMAND.HOUR_CONT_CODE
		STA NPC.TALK.KEYWORD+$1

		;alredy set
		; LDA #NPC.TALK.KEYWORD.STOP_VALUE
		; STA NPC.TALK.KEYWORD+$2

		LDA #NTALK.COMMAND.END_CODE
		STA NTALK.ARRAY.STOP.VALUE			;set the stop value to the command end code because we do not want to search past that delimiter, otherwise we'd be searching the conversation text for subcommands.
					
		;parm NTALK.ARRAY.POINTER(2) are already set. Don't reset the pointer values because we want to search starting at element of the NPC TALK array where we left off. 
	JSR NTALK.ARRAY.SEARCH
		;parse return values
		;ACC = $00 = found, $01 = not found
		CMP #$00				;was keyword found?
		BNE .CHECK.HOUR_CONTINGENT.COMPLETE	;if no, then next command

	;**OPT**.Memory. Setup this loop as a JSR and call it for the other subcommands that need to load values from the talk data array to write to return flags. 
	LDX #$A ;this is the loop counter and also the index to the return flag array, so we start it at the value of the first array element we need to write to. The loop runs for 4 iterations so we just adjust the loop exit test accordingly.  
.TIME.LOAD.LOOP
	;Y-REG = $00: reset by NTALK.ARRAY.SEARCH upon exit
	LDA (NTALK.ARRAY.POINTER.RETURN),Y ;BCD. load the next parameter
	AND #$7F ;mask out the hi bit to convert to a hex value. This is needed because the SBASM cross assembler sets the hi-bit for the entire ascii string if negative ASCII is enabled, even for hex codes inserted into the ASCII string. 
	STA NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS,X ;save return value. Total flags available = $20
			
	;NEXT ARRAY ELEMENT
	INY	;increment LO byte index
	BNE .INCREMENT_COMPLETE6		;did lo byte index flip over to $00? If no, don't increment HO byte
	INC NTALK.ARRAY.POINTER.RETURN+$1	;if yes, increment HO byte index 
.INCREMENT_COMPLETE6
	INX
	CPX #$0E
	BNE .TIME.LOAD.LOOP
.CHECK.HOUR_CONTINGENT.COMPLETE


.CHECK.SUBCOMMAND.INSERT_KEYWORD
;CHECK FOR INSERT KEYWORD SUBCOMMAND

;=====================CODE-SECTION DOCUMENTATION====================================
;
;==OVERVIEW==
;The insert keyword subcommand functions in a fairly different way from 
;the other subcommands. The subcommand code is placed once it the normal subcommand string
;after the subcommand start code ":" and before the primary command end code ".". It is placed
;again, embedded, in the text block itself. The 2nd instance marks the location where
;the player keyword should be inserted. 
;
;==DETAILS==
;This code section does the following:
;		*Check to see if the insert keyword subcommand is present ($K). This check is done in the normal subcommand string space.
;		*Find the primary commadn end code, so we know where the text block starts.
;		*Find the 2nd instance of the insert keyword subcommand code ($K) and note the location.
;		*Calculate the number of characters between the start of the text block and the 2nd instance of the subcommand code, and return this value via the subcommand flags. 
;	
;Note: currently the modified text block, including the player keyword must be <= $FF characters. Several error traps are setup for this.
;=================================================================================

	
		;set parms

		;already set
		; LDA NTALK.COMMAND_FOUND.INDEX+$0
		; STA NTALK.ARRAY.POINTER+$0	

		; LDA NTALK.COMMAND_FOUND.INDEX+$1
		; STA NTALK.ARRAY.POINTER+$1	

		;set keyword parm	
		
		;alredy set
		; LDA #NTALK.COMMAND.START_CODE
		; STA NPC.TALK.KEYWORD+$0

		LDA #NTALK.SUB_COMMAND.KEYWORD_INSERT_CODE
		STA NPC.TALK.KEYWORD+$1

		;alredy set
		; LDA #NPC.TALK.KEYWORD.STOP_VALUE
		; STA NPC.TALK.KEYWORD+$2
	
		;alredy set
		; LDA #NTALK.COMMAND.END_CODE
		; STA NTALK.ARRAY.STOP.VALUE			;set the stop value to the command end code because we do not want to search past that delimiter, otherwise we'd be searching the conversation text for subcommands.

	JSR NTALK.ARRAY.SEARCH
		;parse return values
		;ACC = $00 = found, $01 = not found
		CMP #$00	;was keyword found?
		BNE .CHECK.KEYWORD_INSERT.COMPLETE	;if no, then next command

;find command end code (so we know where text block starts)
		
			;set parms

			;already set
			; LDA NTALK.COMMAND_FOUND.INDEX+$0
			; STA NTALK.ARRAY.POINTER+$0	

			; LDA NTALK.COMMAND_FOUND.INDEX+$1
			; STA NTALK.ARRAY.POINTER+$1	

			;set keyword parm	
			LDA #NTALK.COMMAND.END_CODE
			STA NPC.TALK.KEYWORD+$0

			LDA #NPC.TALK.KEYWORD.STOP_VALUE
			STA NPC.TALK.KEYWORD+$1
		
			LDA #NTALK.END.OF.RECORD
			STA NTALK.ARRAY.STOP.VALUE			;set the stop value to the command end code because we do not want to search past that delimiter, otherwise we'd be searching the conversation text for subcommands.

		JSR NTALK.ARRAY.SEARCH				
			;parse return values
			;ACC = $00 = found, $01 = not found
			CMP #$00							;was end of command character found?
			BNE .ERROR.COMMAND.END.NOT.FOUND_STEP	;if no, then report error
		
			;save pointer to start of text block
			LDA NTALK.ARRAY.POINTER.RETURN+$0
			STA NTALK.TEXT_BLOCK.START+$0
			STA NTALK.ARRAY.POINTER+$0 ;			;also save to the pointer parameter since in the next section we're continuing the search from the command end code, the start of the text block. 

			
			LDA NTALK.ARRAY.POINTER.RETURN+$1
			STA NTALK.TEXT_BLOCK.START+$1
			STA NTALK.ARRAY.POINTER+$1
					
;search for 2nd instance of keyword insert subcommand code	
;note: the 2nd instance is embedded in the text block and it indicates the location to insert the player input keyword. 

		;set pointer parm
		;already set at the end of the last section
		
		;set keyword parm	
		LDA #NTALK.COMMAND.START_CODE
		STA NPC.TALK.KEYWORD+$0

		LDA #NTALK.SUB_COMMAND.KEYWORD_INSERT_CODE
		STA NPC.TALK.KEYWORD+$1

		LDA #NPC.TALK.KEYWORD.STOP_VALUE
		STA NPC.TALK.KEYWORD+$2
	
		LDA #NPC.TEXT.BLOCK.STOP_VALUE
		STA NTALK.ARRAY.STOP.VALUE			;set the stop value to the command end code because we do not want to search past that delimiter, otherwise we'd be searching the conversation text for subcommands.
		
	JSR NTALK.ARRAY.SEARCH				
		;parse return values
		;ACC = $00 = found, $01 = not found
		CMP #$00					;was keyword found?
		BEQ .2ND.INSTANCE.FOUND 
		JMP .ERROR.CHECK.KEYWORD_INSERT	;if the 2nd instance of the subcommand code isn't found then report error. Withtou the 2nd instance there is not way to know where in the text block to insert the player input keyword. 

.2ND.INSTANCE.FOUND
;save subcommand return flags
		
		;save # of characters between start of text block and the point to insert the player keyword
		LDA NTALK.ARRAY.POINTER.RETURN+$0 ;load LO byte of the pointer, marking the location of the keyword insert. 
		SEC
		SBC NTALK.TEXT_BLOCK.START+$0 ;subtract the LO byte of the pointer to the start of the text block. 
		SBC #$02 ;Subtract 2 characters. This is because pointer is set to the character after the $K subcommand code and we don't want $K to be included in the modified text block. 
		STA NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS+$7 ;save return value: insertion point distance, LO Byte
		LDA NTALK.ARRAY.POINTER.RETURN+$1 ;load HO byte of the pointer, marking the location of the keyword insert. 
		SBC NTALK.TEXT_BLOCK.START+$1 ;16-BIT SUBTRACT. subtract the HO byte of the pointer to the start of the text block. 
		STA NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS+$8 ;save return value: insertion point distance, HO Byte
		
		CMP #$01 ;is the distance a 16-bit value? Check the HO byte of distance
		BCS .ERROR.KEYWORD_INSERT.OVERFLOW_STEP ;if yes, report error. Only 8-bit value is supported by .EXECUTE.KEYWORD_INSERT (NTALK.SEARCH)
			
.CHECK.KEYWORD_INSERT.COMPLETE
	JMP .PRIMARY_COMMAND.ONLY.ENTRANCE

	
.ERROR.COMMAND.END.NOT.FOUND_STEP
	JMP .ERROR.COMMAND.END.NOT.FOUND
			
.ERROR.KEYWORD_INSERT.OVERFLOW_STEP
	JMP ERROR.KEYWORD_INSERT.OVERFLOW
	
			
.PRIMARY_COMMAND.ONLY.ENTRANCE	
.FIND.COMMAND.END
			
			;set parms

			;set pointer
			LDA NTALK.COMMAND_FOUND.INDEX+$0 ;set pointer back to the command found index because some subcommands (like keyword insert) modify the pointer value to do multiple searches that continue where they left off
			STA NTALK.ARRAY.POINTER+$0	

			LDA NTALK.COMMAND_FOUND.INDEX+$1
			STA NTALK.ARRAY.POINTER+$1	

			;set keyword parm	
			LDA #NTALK.COMMAND.END_CODE
			STA NPC.TALK.KEYWORD+$0

			LDA #NPC.TALK.KEYWORD.STOP_VALUE
			STA NPC.TALK.KEYWORD+$1
		
			LDA #NTALK.END.OF.RECORD
			STA NTALK.ARRAY.STOP.VALUE		;set the stop value to the command end code because we do not want to search past that delimiter, otherwise we'd be searching the conversation text for subcommands.

			LDA #$01
			STA NTALK.FLAG.INTERNAL_SEARCH	;This flag is on when internal searches, such as for the command end code, are being done. This flag is used to override the minimum characters required for a match as specified by NTALK.SEARCH.MATCH_QTY
											;even though this flag is set in the init section at the top of the routine for search mode $00, $01, we need to set the flag here because subcommand are searched for in search mode $03 as well, and searching for the command end code requires an internal search.			
		JSR NTALK.ARRAY.SEARCH			
		;parse return values
		;ACC = $00 = found, $01 = not found
		CMP #$00							;was end of command character found?
		BNE .ERROR.COMMAND.END.NOT.FOUND	;if no, then report error
		;**FALLS THROUGH**
		
		JMP .COMMAND.FOUND

;**ALTERNATE COMPRESSION SCHEME ROUTINE***		
;see NTALK.SEARCH subroutine documentation for details
@START
; .NPC_RECORD_SEARCH_ONLY
		; LDA NTALK.SEARCH.MODE
		; CMP #$00
		; BNE .COMMAND.FOUND
		
		; ;Y-REG = $00: reset by NTALK.ARRAY.SEARCH upon exit
		; LDA (NTALK.ARRAY.POINTER.RETURN),Y	;load next character in the NPC.TALK.ARRAY, which should be the NPC Record # associated with the $R command just found.
		; AND #$7F ;mask out the ho nibble to convert to a hex value. This is needed because the SBASM cross assembler sets the hi-bit for the entire ascii string if negative ASCII is enabled, even for hex codes inserted into the ASCII string. 
		; CMP NPC.TALK.RECORD				;does the NPC Record # found in NPC.TALK.ARRAY match the NPC Record # of the NPC that player is trying to talk to?
		; BEQ .COMMAND.FOUND			 	;if set, set return values and exit
					
; ;SETUP ORIGINAL PARMS BEFORE RETURNING TO SEARCH LOOP

			; ;setup parms
			
			; ;zero-page pointer already set to correct values in .FIND.COMMAND.END (the character after the command end code), because we want to pickup the search at the array element where we left off 

				; ;set keyword parameters
				; LDA #NTALK.COMMAND.START_CODE
				; STA NPC.TALK.KEYWORD+$0
				
				; LDA #NTALK.COMMAND.FIND_RECORD
				; STA NPC.TALK.KEYWORD+$1

				; LDA #NPC.TALK.KEYWORD.STOP_VALUE
				; STA NPC.TALK.KEYWORD+$2
				
				; LDA #NTALK.END.OF.FILE
				; STA NTALK.ARRAY.STOP.VALUE	

			; JMP .START.SEARCH
@END

@END
	
.COMMAND.FOUND
	LDA #$00		;set keyword found/not_found return value
	
	JMP .EXIT
	
.COMMAND.NOT.FOUND			
	LDA #$01		;set keyword found/not_found return value
	;**FALLS THROUGH**

	
			
.EXIT

			
	STA SAVED.ACC.LOCAL2 ;save return value to stack

;RESTORE PERSISTENT ARRAY POINTER TO STACK
	
	PLA
	STA NTALK.KEYWORD.POINTER+$1
	PLA
	STA NTALK.KEYWORD.POINTER+$0
	
	PLA
	STA NTALK.ARRAY.POINTER+$1
	PLA
	STA NTALK.ARRAY.POINTER+$0

	LDA SAVED.ACC.LOCAL2 ;restore return value from stack


	
	RTS
	
.ERROR.COMMAND.END.NOT.FOUND
;Command end code not found in .FIND.COMMAND.END (NTALK.SEARCH.LOOP)
	JSR PREP.BRK
	BRK

.ERROR.CHECK.KEYWORD_INSERT
;NTALK.SEARCH.LOOP reports that a keyword_insert command code ($K) was found in the subcommand string but a 2nd instance
;of the keyword_insert command code was not found embedded in the text block, which resultsin the code not knowing where 
;in the text block to insert the player input keyword.
	JSR PREP.BRK
	BRK
	
ERROR.KEYWORD_INSERT.OVERFLOW
;NTALK.SEARCH.LOOP or .EXECUTE.KEYWORD_INSERT (NPC.SEARCH) reports that a keywors insert was attempted
;at > $FF characters from the start of the text block. 
;16-bit offsets not supported for this subcommand. To enable
;16-bit offsets, add support to .KEYWORD_INSERT.LOOP (NPC.SEARCH). A ho byte counter would needed to be added and checked. 
;but take a look at MEMORY.COPY first in the general library. The concept is very simular and I recall having issues with bugs when doing it in MEMORY.COPY the first time, which were eventually resolved. 		
	JSR PREP.BRK
	BRK
	

@END	

NTALK.ARRAY.SEARCH
@START
;PARMATERS: NTALK.ARRAY.POINTER(2) = index to array to be searched or start position within an array, NTALK.ARRAY.STOP.VALUE, NTALK.KEYWORD.POINTER(2) = index to the array storing the keyword search string, NTALK.KEYWORD.STOP_VALUE, NTALK.SEARCH.MATCH_QTY = # of chars required for a match,  
;RETURN: ACC ($00 = keyword found, $01 = keyword not found), NTALK.ARRAY.POINTER.RETURN(2) (index to the next element to be searched (i.e. the last elemenet to be searched +$1)), NTALK.KEYWORD.POINTER.UPDATED (last element in keyword +1), YREG = $00 (always reset to $00 since pointer contains the exact index to the last element searched +$1. Other code in the calling routines expects Y-REG to be $00 upon return)
;ENTRANCE: NTALK.SEARCH.LOOP
;*If keyword is not found, NTALK.ARRAY.POINTER will be an index to the last charater searched, the +$1 only applies if the keyword is found, which occurs because an additional increment occurs to iterate the keyword stop value. 

;=====================SUBROUTINE DOCUMENTATION====================================
;
;Searches the 16-bit array specified by parameter NTALK.ARRAY.POINTER for the keyword specified by the parameter array NTALK.KEYWORD.POINTER(x),
;with both the keyword and array values being converted to uppercase if letters. NTALK.KEYWORD.POINTER is copied into NTALK.KEYWORD.BUFFER in the init routine so that
;X-REG can be used as the index during the main search loop.
;
;The value in the parameter NTALK.SEARCH.MATCH_QTY is the number of characteres that must match for the keyword to be considered found. 
;If the keyword in the search array is shorter than the keyword in NTALK.KEYWORD.BUFFER, then the extra characters in NTALK.KEYWORD.BUFFER are discarded for purposes of considering a match.
;
;If NTALK.ARRAY.STOP.VALUE is found, then the keyword wasn't found. NTALK.ARRAY.STOP.VALUE is a paramter
;set by the calling routine. After a primary command if found, NPC.SEARCH sets this value as being the NPC Record stop value. NPC.SEARCH.LOOP sets
;it to the NTALK command end code when doing seaches to determine which, if any, subcommands are present.
;
;
;Return value (ACC) = $00 if keyword found, $01 if keyword not found
;Return value (NTALK.ARRAY.POINTER.RETURN)
;		keyword found = the memory address of the last character of the keyword found + $1
;		keyword not found = NTALK.ARRAY.POINTER.RETURN will be an index to the last charater searched, the +$1 only applies if the keyword is found, which occurs because an additional increment occurs to iterate the keyword stop value. 
;Return value NTALK.KEYWORD.POINTER.UPDATED = last element of keyword +1
;
;In all cases Y-REG is rest to #$00 upon exit of this routine as it is closed out to the LO byte of NTALK.ARRAY.POINTER
;The original values at entry are preserved for NTALK.ARRAY.POINTER & NTALK.KEYWORD.POINTER
;so that sequential calls which use the same values do not need to set the parameters each time. 
;
;=================================================================================

	;LDA #$06	
	;STA NTALK.SEARCH.MATCH_QTY	; Number of characters that must be the same for a match to be found. For exampe, if this variable is set to 4 then if the first 4 characters of the keyword are found then the entire keyword is considered to be found. The number of characters must include the NTALK keyword subcommand code of "$!" 

		; LDA NTALK.ARRAY.POINTER.RETURN+$0 ;this value will incremented and be available for calls which require the starting search position to resume where the last search left off. 
		; STA NTALK.ARRAY.POINTER+$0		;this value will be preserved and available for sequential calls which require the same starting search position. 

		; LDA NTALK.ARRAY.POINTER.RETURN+$1
		; STA NTALK.ARRAY.POINTER+$1
			
.SEARCH.INIT
	;setup the pointer to the array to be searched.
	LDA NTALK.ARRAY.POINTER+$0		;this value will be preserved and available for sequential calls which require the same starting search position. 
	STA NTALK.ARRAY.POINTER.RETURN+$0 ;this value will incremented and be available for calls which require the starting search position to resume where the last search left off. 

	LDA NTALK.ARRAY.POINTER+$1
	STA NTALK.ARRAY.POINTER.RETURN+$1

			
;COPY KEYWORD TO BUFFER
	LDY #$00			;Init NTALK.ARRAY.POINTER & NTALK.KEYWORD.BUFFER index.

.KEYWORD.COPY.LOOP	
	LDA (NTALK.KEYWORD.POINTER),Y ;load character from the keyword array
	STA NTALK.KEYWORD.BUFFER,Y	;save character to keyword buffer	
	INY ;increment index
	BNE .NO_OVERFLOW
	JMP .ERROR.KEYWORD.OVERFLOW	;if index flips over then report overflow error. If the keyword array is less than $100 byte this isn't fool proof as a stop value might be found in the random values in memory past the end of the array but before the index flips at $100 bytes.
.NO_OVERFLOW
	CMP NTALK.KEYWORD.STOP_VALUE ;has end of keyword array been reached?
	BNE .KEYWORD.COPY.LOOP	;if no, continue copying data
		
	;update pointer so it can be reused for a group search (like is done for curse words in NTALK.PARSE.KEYWORD)
	TYA ;transfer keyword pointer index to ACC
	CLC
	ADC NTALK.KEYWORD.POINTER+$0 ;add LO byte of keyword pointer index
	STA NTALK.KEYWORD.POINTER.UPDATED+$0
	LDA NTALK.KEYWORD.POINTER+$1 ;load HO byte of keyword pointer index
	ADC #$00 ;16-BIT add
	STA NTALK.KEYWORD.POINTER.UPDATED+$1
		
;INIT INDEXES FOR MAIN LOOP
 	LDY #$00			;Init NTALK.ARRAY.POINTER index. Y_REG must be init to $00 or it will cause a block of addresses to be skipped if the lo byte of the pointer is > $00
	LDX #$00 			;init keyword index
	
;CONVERT 1ST CHAR OF KEYWORD TO UCASE
	LDA NTALK.KEYWORD.BUFFER+$0		;does it match the first character of the keyword? 
	CMP #$E1			;is ASCII value less than the lower end of the lowercase letter range?
	BCC .NOT.A.LETTER4	;if yes then don't convert value to uppercase as that will damage the value, or if the value is already uppercase then no modification to the value is needed.
	CMP #$FB			;is ASCII value greater than the upper end of the lowecase letter range?	
	BCS .NOT.A.LETTER4	;if yes then don't convert value to uppercase as that will damage the value, or if the value is already uppercase then no modification to the value is needed.
	AND #$DF ;masks out the 5th bit because the binary value of #$DF has 1 in each bit position except for bit 5.
.NOT.A.LETTER4
	STA NTALK.KEYWORD.1ST_CHAR.UCASE ;must be after the labels non-letter values are saved to the variable

	
.KEYWORD.SEARCH.LOOP
;Note: search for the first character of the keyword. Once found, pass control to .FIRST.CHAR.FOUND takes over and continues the search for the rest of the keyword. If the remainder of the keyword is not found, control will be passed back to this loop to continue searching for the first character
;NTALK.ARRAY.STOP.VALUE is the stop value specified by the calling routine. It might be the stop value indicating the end of the array or it might be a stop value within the array, such as for indicating the end of a record.

.CONVERT.UPPER_CASE2
	LDA (NTALK.ARRAY.POINTER.RETURN),Y	;load next character from talk array
	CMP #$E1			;is ASCII value less than the lower end of the lowercase letter range?
	BCC .NOT.A.LETTER3	;if yes then don't convert value to uppercase as that will damage the value, or if the value is already uppercase then no modification to the value is needed.
	CMP #$FB			;is ASCII value greater than the upper end of the lowercase letter range?	
	BCS .NOT.A.LETTER3	;if yes then don't convert value to uppercase as that will damage the value, or if the value is already uppercase then no modification to the value is needed.
	AND #$DF ;masks out the 5th bit because the binary value of #$DF has 1 in each bit position except for bit 5.
.NOT.A.LETTER3
	STA NTALK.ARRAY.UCASE ;must be after the labels non-letter values are saved to the variable

.COMPARE.CHARACTERS1			
	LDA NTALK.ARRAY.UCASE		;load next character from talk array

;	CMP NTALK.KEYWORD.BUFFER+$0		;does it match the first character of the keyword? 
	CMP NTALK.KEYWORD.1ST_CHAR.UCASE		;does it match the first character of the keyword?
	BEQ .FIRST.CHAR.FOUND			;if yes, then branch to the first character found routine to look for the rest of the keyword characters in sequence
	CMP NTALK.ARRAY.STOP.VALUE		;is array character the array stop value specified by the calling routine?
	BEQ .ARRAY.STOP_VALUE.FOUND		;if yes, keyword was not found.
.NEXT.ARRAY.ELEMENT
	INY	;increment LO byte index
	BNE .KEYWORD.SEARCH.LOOP		;did lo byte index flip over to $00? If no, continue the loop
	INC NTALK.ARRAY.POINTER.RETURN+$1	;if yes, increment HO byte index before continuing the loop
	BNE .KEYWORD.SEARCH.LOOP
	JMP .ERROR.ARRAY.OVERFLOW		;ho byte of pointer flipped over to $00. A stop value should have been found, something has gone wrong, report error.

.FIRST.CHAR.FOUND
	;save index and pointer base address 
	STY	SAVED.YREG.LOCAL ;saved array pointer index
	
	LDA NTALK.ARRAY.POINTER.RETURN+$0
	STA NTALK.ARRAY.POINTER.RETURN.LO.SAVED
	
	LDA NTALK.ARRAY.POINTER.RETURN+$1
	STA NTALK.ARRAY.POINTER.RETURN.HO.SAVED
	
.SEARCH.REMAINDER.LOOP	

.INCREMENT.INDEX	
	INX ;next keyword element

	;next array element
	INY	;increment LO byte index
	BNE .INCREMENT.COMPLETE2		;did lo byte index flip over to $00? If no, continue the loop



	INC NTALK.ARRAY.POINTER.RETURN+$1	;if yes, increment HO byte index before continuing the loop
	BEQ .ERROR.ARRAY.OVERFLOW		;ho byte of pointer flipped over to $00. A stop value should have been found, something has gone wrong, report error.
.INCREMENT.COMPLETE2

.CHECK.MATCH.CHARS
	CPX NTALK.SEARCH.MATCH_QTY
	BEQ .KEYWORD.FOUND
	
			
.CONVERT.UPPER_CASE
	LDA NTALK.KEYWORD.BUFFER,X ;load next keyword character
	CMP #$E1			;is ASCII value less than the lower end of the lowercase letter range?
	BCC .NOT.A.LETTER1	;if yes then don't convert value to uppercase as that will damage the value, or if the value is already uppercase then no modification to the value is needed.
	CMP #$FB			;is ASCII value greater than the upper end of the lowercase letter range?	
	BCS .NOT.A.LETTER1	;if yes then don't convert value to uppercase as that will damage the value, or if the value is already uppercase then no modification to the value is needed.
	AND #$DF ;masks out the 5th bit because the binary value of #$DF has 1 in each bit position except for bit 5.
.NOT.A.LETTER1
	STA NTALK.KEYWORD.UCASE ;must be after the labels non-letter values are saved to the variable
	LDA (NTALK.ARRAY.POINTER.RETURN),Y	;load next array character
	CMP #$E1			;is ASCII value less than the lower end of the lowercase letter range?
	BCC .NOT.A.LETTER2	;if yes then don't convert value to uppercase as that will damage the value, or if the value is already uppercase then no modification to the value is needed.
	CMP #$FB			;is ASCII value greater than the upper end of the lowecase letter range?	
	BCS .NOT.A.LETTER2	;if yes then don't convert value to uppercase as that will damage the value, or if the value is already uppercase then no modification to the value is needed.
	AND #$DF ;masks out the 5th bit because the binary value of #$DF has 1 in each bit position except for bit 5.
.NOT.A.LETTER2
	STA NTALK.ARRAY.UCASE ;must be after the labels non-letter values are saved to the variable
		
.COMPARE.CHARACTERS			
	
	LDA NTALK.ARRAY.UCASE 			;reload next array character
	CMP #NTALK.COMMAND.SUBCOMMAND_CODE	;is next array character the subcommand code?
	BEQ	.KEYWORD.FOUND					;if yes, then the array keyword is shorter than the players input keyword, and if this loop is still running it means all the characters of the array keyword match the player's keyword if the extra characters are discarded. This is considered a match. For example, if the player enters "JOBS" it will match on "JOB"
	CMP #NTALK.COMMAND.END_CODE			;is next array character the command end code?
	BEQ	.KEYWORD.FOUND					;if yes, then the array keyword is shorter than the players input keyword, and if this loop is still running it means all the characters of the array keyword match the player's keyword if the extra characters are discarded. This is considered a match. For example, if the player enters "JOBS" it will match on "JOB"	

	LDA NTALK.KEYWORD.UCASE			;reload keyword character
	CMP #NPC.TALK.KEYWORD.STOP_VALUE	;is it the keyword stop value?
	BEQ .KEYWORD.STOP_VALUE.FOUND	;if yes, then keyword might be found, more checks are needed

	LDA NTALK.ARRAY.UCASE 	
	CMP NTALK.KEYWORD.UCASE 		;is it equal to the next keyword character?
	BNE	.REMAINDER_NOT.FOUND		;if no, then the keyword was not found in this sequence. The first character found was a red herring. Or, also possible, it may have been a blue hippopotamus. 

	JMP .SEARCH.REMAINDER.LOOP

.KEYWORD.STOP_VALUE.FOUND
;This routine would not be entered unless the player keyword was less than the minimum chars required to find a match except in
;certain internal searches such as searching for the command end code. 
;Thus, if the interal search flag isn't set, we can concluded that the player keyword didn't match because it was too short (less than the minimum characters required for a match).

	LDA NTALK.FLAG.INTERNAL_SEARCH 
	CMP #$01	;is this an internal search?
	BEQ .KEYWORD.FOUND	;if yes, then this is a match because all characters in NTALK.KEYWORD.BUFFER matched with a series of sequential characters in the search array
	;**FALLS THROUGH**
	
.REMAINDER_NOT.FOUND
	LDX #$00 ;reset keyword index
	
	;restore index and pointer base address to continue search as of the talk array element after the one containing the first character of the keyword which turned out to be a red herring
	LDY	SAVED.YREG.LOCAL ;restore array pointer index

	LDA NTALK.ARRAY.POINTER.RETURN.LO.SAVED
	STA NTALK.ARRAY.POINTER.RETURN+$0

	LDA NTALK.ARRAY.POINTER.RETURN.HO.SAVED
	STA NTALK.ARRAY.POINTER.RETURN+$1
	JMP .NEXT.ARRAY.ELEMENT	;enter loop here so that the talk array element is incremented before proceeding with the search. It would have ended on the red herring element. 		
	
.KEYWORD.FOUND
			
	LDA #$00	;load return value
	JMP .EXIT

	
.ARRAY.STOP_VALUE.FOUND
	;next array element
	INY	;advance one element so that this routine will return NTALK.ARRAY.POINTER with an index to the next element to be searched (i.e. the last elemenet to be searched +$1)
	BNE .INCREMENT.COMPLETE3		;did lo byte index flip over to $00? If no, continue the loop
	INC NTALK.ARRAY.POINTER.RETURN+$1	;if yes, increment HO byte index before continuing the loop
.INCREMENT.COMPLETE3

			LDA #$01	;load return value
	;**FALLS THROUGH**	
	
.EXIT
;FINISH SETTING RETURN VALUES
	PHA ;save ACC return value already set
	;set index return value 
	TYA
	CLC
	ADC NTALK.ARRAY.POINTER.RETURN+$0
	STA NTALK.ARRAY.POINTER.RETURN+$0	;close out Y-REG to the lo byte index to NPC.TALK.ARRAY, so that it reflects the last seached element
	LDA NTALK.ARRAY.POINTER.RETURN+$1
	ADC #$00 ;16-BIT ADD
	STA NTALK.ARRAY.POINTER.RETURN+$1
	
	
	PLA ;restore ACC return value already set
	
	LDY #$00; reset Y-reg since pointer contains the exact index to the last element searched +$1. Other code in the calling routines expects Y-REG to be reset to $00 upon return
			
	RTS

.ERROR.KEYWORD.OVERFLOW
;NTALK.ARRAY.SEARCH reports an overflow in .KEYWORD.COPY.LOOP
;possibly cause be a lack of a stop value or an incorrectly specified stop 
;value in the NTALK.KEYWORD.STOP_VALUE parameter. 
	JSR PREP.BRK
	BRK
	
	
.ERROR.ARRAY.OVERFLOW		
;NTALK.ARRAY.SEARCH reports overflow when incrementing
;ho byte of NTALK.ARRAY.POINTER. A stop value in NPC.TALK.ARRAY should have been found, something has gone wrong.
	JSR PREP.BRK
	BRK
	
@END
		
NTALK.PARSE.KEYWORD
@START
;PARAMETERS:
;RETURN:
;ENTRANCE:

;=====================SUBROUTINE DOCUMENTATION====================================
;
;This routine searches inside player keyword for recognized synonyms and curse words, and modify the player keyword if found to trigger the desired NPC response. 
;
;=================================================================================

;SAVE PERSISTENT ARRAY POINTER TO STACK
	LDA NTALK.ARRAY.POINTER+$0
	PHA
	LDA NTALK.ARRAY.POINTER+$1
	PHA


;set constant parameters (NTALK.ARRAY.SEARCH)
	LDA #NPC.TALK.KEYWORD			;set player keyword as the array to be searched
	STA	NTALK.ARRAY.POINTER+$0

	LDA /NPC.TALK.KEYWORD			;set player keyword as the array to be searched
	STA	NTALK.ARRAY.POINTER+$1

	LDA #NPC.TALK.KEYWORD.STOP_VALUE
	STA NTALK.ARRAY.STOP.VALUE

		
;SEARCH FOR "JOB" SYNONYMS

	;search "work"
		
		;set parms
		LDA #.TEXT_STRING.WORK			;set player keyword as the array to be searched
		STA	NTALK.KEYWORD.POINTER+$0

		LDA /.TEXT_STRING.WORK			;set player keyword as the array to be searched
		STA	NTALK.KEYWORD.POINTER+$1
	JSR NTALK.ARRAY.SEARCH
	
			; ;LDA #$AA
			; LDX #NPC.TALK.KEYWORD
			; LDY /NPC.TALK.KEYWORD
			; JSR PREP.BRK
			; BRK		
		
		;ACC = $00 keyword found, $01 = keyword not found
		CMP #$00
		BEQ .JOB.SYNONYM.FOUND	

		
	;search "trade"

		;set parms
		LDA #NPC.TALK.KEYWORD			;set player keyword as the array to be searched
		STA	NTALK.ARRAY.POINTER+$0

		LDA /NPC.TALK.KEYWORD			;set player keyword as the array to be searched
		STA	NTALK.ARRAY.POINTER+$1

		LDA #.TEXT_STRING.TRADE			;set player keyword as the array to be searched
		STA	NTALK.KEYWORD.POINTER+$0

		LDA /.TEXT_STRING.TRADE			;set player keyword as the array to be searched
		STA	NTALK.KEYWORD.POINTER+$1
	JSR NTALK.ARRAY.SEARCH
		;ACC = $00 keyword found, $01 = keyword not found
		CMP #$00
		BEQ .JOB.SYNONYM.FOUND

		
	JMP .JOB.SYNONYM.SEARCH.COMPLETE
	
.JOB.SYNONYM.FOUND

			
	;ADD CODE TO CHANGE KEYWORD TO CJOB

;CHANGE KEYWORD TO STANDARDIZED KEYWORD "JOB"	
	LDX #$00 ;init index
.KEYWORD.COPY.LOOP
	LDA .TEXT_STRING.JOB,X
	STA NPC.TALK.KEYWORD+$2,X	;offset by $2 because the player input keyword was prepended with the NTALK keyword command code "$!"
;	STA NPC.TALK.KEYWORD,X
	INX
	CMP #NPC.TALK.KEYWORD.STOP_VALUE
	BNE .KEYWORD.COPY.LOOP

	;**FALLS THROUGH**
	
.JOB.SYNONYM.SEARCH.COMPLETE

			
.CURSE_WORD.SEARCH
;Note: the value of NTALK.KEYWORD.POINTER will be updated by NTALK.ARRAY.SEARCH as a return value.
;It willl be returned = to the byte after the stop value. As a result, the keyword pointer doesn't need to be updated
;with each call to NTALK.ARRAY.SEARCH. It will iterate through the curse word ascii tables as a group, since they are sequential.
 
.CURSE.INIT
		LDA #$00
		STA NTALK.CURSE_WORD.COUNTER
		
		;set parms 
		LDA #.TEXT_STRING.CURSE0			;set player keyword as the array to be searched
		STA	NTALK.KEYWORD.POINTER+$0

		LDA /.TEXT_STRING.CURSE0			;set player keyword as the array to be searched
		STA	NTALK.KEYWORD.POINTER+$1
		
.CURSE.LOOP	
		;set parms
		LDA #NPC.TALK.KEYWORD			;set player keyword as the array to be searched
		STA	NTALK.ARRAY.POINTER+$0

		LDA /NPC.TALK.KEYWORD			;set player keyword as the array to be searched
		STA	NTALK.ARRAY.POINTER+$1

	JSR NTALK.ARRAY.SEARCH
		;ACC = $00 keyword found, $01 = keyword not found
		CMP #$00
		BEQ .CURSE_WORD.FOUND	
		;save return value: updated keyword pointer
		LDA NTALK.KEYWORD.POINTER.UPDATED+$0			;set player keyword as the array to be searched
		STA	NTALK.KEYWORD.POINTER+$0

		LDA NTALK.KEYWORD.POINTER.UPDATED+$1			;set player keyword as the array to be searched
		STA	NTALK.KEYWORD.POINTER+$1
		
	INC NTALK.CURSE_WORD.COUNTER

			; STA TEMP
			; LDA NTALK.CURSE_WORD.COUNTER
			; CMP #$08
			; BNE .TEMP
			; LDA #$AA
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP
			
			
	LDA NTALK.CURSE_WORD.COUNTER
	CMP #NTALK.TOTAL.CURSE_WORDS	
	BEQ .CURSE_WORD.SEARCH.COMPLETE
	JMP .CURSE.LOOP	
	
.CURSE_WORD.FOUND
;CHANGE KEYWORD TO STANDARDIZED KEYWORD "JOB"	
	LDX #$00 ;init index
.KEYWORD.COPY.LOOP2
	LDA .TEXT_STRING.CURSE_KEYWORD,X
	STA NPC.TALK.KEYWORD+$2,X	;offset by $2 because the player input keyword was prepended with the NTALK keyword command code "$!"
;	STA NPC.TALK.KEYWORD,X
	INX
	CMP #NPC.TALK.KEYWORD.STOP_VALUE
	BNE .KEYWORD.COPY.LOOP2

	;**FALLS THROUGH**

	JMP .EXIT
.CURSE_WORD.SEARCH.COMPLETE
	;**FALLS THROUGH**
	


.EXIT

			
;restore to defalts before exiting
			
	;set NTALK.ARRAY.SEARCH parameters
	LDA #NPC.TALK.KEYWORD				;array storing the player input keyword (via NPC.TALK.INPUT)
	STA NTALK.KEYWORD.POINTER+$0		;NTALK.ARRAY.SEARCH parameter: pointer to array used as keyword search string

	LDA /NPC.TALK.KEYWORD
	STA NTALK.KEYWORD.POINTER+$1

	
;RESTORE PERSISTENT ARRAY POINTER TO STACK
	PLA
	STA NTALK.ARRAY.POINTER+$1
	PLA
	STA NTALK.ARRAY.POINTER+$0

	
	RTS

;TEXT STRINGS

;JOB SYNONYMS
.TEXT_STRING.JOB 	.AZ -/JOB/	;ASCII text string
.TEXT_STRING.WORK 	.AZ -/WORK/	;ASCII text string
.TEXT_STRING.TRADE 	.AZ -/TRADE/	;ASCII text string

;CURSE WORDS
.TEXT_STRING.CURSE_KEYWORD .AZ -/%CRS/	;ASCII text string

.TEXT_STRING.CURSE0 	.AZ -/FUCK/	;ASCII text string
.TEXT_STRING.CURSE1 	.AZ -/SHIT/	;ASCII text string
.TEXT_STRING.CURSE2 	.AZ -/ASS/	;ASCII text string
.TEXT_STRING.CURSE3 	.AZ -/BITCH/	;ASCII text string
.TEXT_STRING.CURSE4 	.AZ -/CUNT/	;ASCII text string
.TEXT_STRING.CURSE5 	.AZ -/DICK/	;ASCII text string
.TEXT_STRING.CURSE6 	.AZ -/COCK/	;ASCII text string
.TEXT_STRING.CURSE7 	.AZ -/PUSSY/	;ASCII text string

@END

NPC.TALK.INPUT
@START
;PARAMETERS: none
;ENTRANCE: NPC.TALK
;RETURN: NPC.TALK.KEYWORD (stop value of $8D added)

;=====================SUBROUTINE DOCUMENTATION====================================
;
;=================================================================================

			; LDA #NTALK.COMMAND.START_CODE ;ASCII = $
			; STA NPC.TALK.KEYWORD+$0
			
			; LDA #NTALK.COMMAND.FIND_KEYWORD ;ASCII = !
			; STA NPC.TALK.KEYWORD+$1
			
		; LDA #$C2 ;ASCII ('B' key), is the first letter of player keyword "K"
		; STA NPC.TALK.KEYWORD+$2
	
		; LDA #$D9 ;ASCII ('Y' key), is the first letter of player keyword "K"
		; STA NPC.TALK.KEYWORD+$3

		; LDA #$C5 ;ASCII ('E' key), is the first letter of player keyword "K"
		; STA NPC.TALK.KEYWORD+$4

		; LDA #NPC.TALK.KEYWORD.STOP_VALUE	;stop value needs to be set again because it is currently in NPC.TALK.KEYWORD+$02, which is overwritten by the keyword "BYE"
		; STA NPC.TALK.KEYWORD+$5
	

			; RTS
			
			
;SAVE REGISTERS	
	TXA
	PHA
	TYA
	PHA
	
;SAVE CURSOR POSITION
		LDA HTAB	
		STA CURSOR.POSITION.SAVED+$0
		LDA VTAB
		STA CURSOR.POSITION.SAVED+$1
	;JSR	UPDATE.CHAR.POS

	
;INIT INDEXES
;	LDX #$1F	;set max # of characters to input
	LDX #$0D	;set max # of characters to input
	LDY #$02	;init keyboard input array (RESULT) index. Start index at $02 because the first two characters will be filled manually with $!, the NTALK keyword command code

;MANUALLY SET KEYWORD VALUES $00-$01
	LDA #NTALK.COMMAND.START_CODE ;ASCII = $
	STA NPC.TALK.KEYWORD+$0
	
	LDA #NTALK.COMMAND.FIND_KEYWORD ;ASCII = !
	STA NPC.TALK.KEYWORD+$1
	
;SETUP TEXT OUTPUT STARTING POSITION
		LDA #TWS.TALK_INPUT_WINDOW.CURSOR_START.HTAB ;1A
		STA HTAB	
		LDA #TWS.TALK_INPUT_WINDOW.CURSOR_START.VTAB ;14 originally
		STA VTAB
	JSR	UPDATE.CHAR.POS

;SET TEXT WINDOW EDGE FOR BACKSPACE KEY
	LDA #TWB.RIGHT_WINDOW.LEFT_SBYTE+$2
	STA KEYIN.STRING.LEFT_EDGE
	;**FALLS THROUGH**

		; LDA #$00				;reset to "not tracked" value
		; STA ANIMATION.CALLED_BY
NPC.TALK.INPUT.LOOP
	; JSR NPC.TALK.INPUT.KEYIN
	; JMP .GET.INPUT
	;BEQ * ;branches back to the JSR
	
	
NPC.TALK.INPUT.KEYIN
	LDA ANIMATION.CALLED_BY
	BEQ .KEYIN.LOOP
		
	LDA KEYIN.SAVED ;restore keypress captured in animation routine.
	LDX KEYIN.STRING.XREG
	LDY KEYIN.STRING.YREG
	JMP .ALTERNATE.ENTRANCE
	
.KEYIN.LOOP ;wait for keypress
		LDA #$01				;reset to "not tracked" value
		STA ANIMATION.CALLED_BY
		
		STX KEYIN.STRING.XREG
		STY KEYIN.STRING.YREG
		;ENABLE BANK-SWITCHED RAM ($D000 Bank 1)
		LDA $C08B
		LDA $C08B
	JSR ANIMATION.UPDATE	;continue updating animation while waiting for keypress
		;RE-ENABLE BANK-SWITCHED RAM ($D000 Bank 2)
		LDA $C083				;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK2 2nd)
		LDA $C083	

		LDX KEYIN.STRING.XREG
		LDY KEYIN.STRING.YREG	

		LDA #$00				;reset to "not tracked" value
		STA ANIMATION.CALLED_BY

	LDA KB_BUFFER
    BPL .KEYIN.LOOP				;loop until key is pressed, depositing its ascii value in the keyboard buffer
.ALTERNATE.ENTRANCE
    STA KB_BUFFER_ACK       ;clr last key
	;ACC = keypress captured
	CMP #$88 ;left arrow key and delete key (AppleWIN) 
	BEQ .BACKSPACE.PRESSED	;if backspace is pressed, erase character in left adjacent column, move cursor 1 column left, and restart loop. 
	CMP #$FF ;delete key (real Apple II, Virutal ][)
	BEQ .BACKSPACE.PRESSED	;if backspace is pressed, erase character in left adjacent column, move cursor 1 column left, and restart loop. 
	CMP #$89	;was tab key pressed
	BEQ	.TOGGLE.VOICE_MODE_STEP
	CMP #$8D	;was return key pressed?
	BEQ	.END.INPUT_STEP ;if yes then end input. Return key is not written to NPC.TALK.KEYWORD
	STA NPC.TALK.KEYWORD,Y	;save input value

	
		;ACC = keypress
	JSR COUT				;print key press to video screen

.INCREMENT_INDEX
	INY						;increment keyboard input array (RESULT) index	
	DEX						;decrement loop counter
	BEQ	.END.INPUT_STEP			;if counter (# of characters to input) = $00 then exit loop
	STX KEYIN.STRING.XREG
	STY KEYIN.STRING.YREG

	JMP .ITERATION.DONE

.TOGGLE.VOICE_MODE_STEP
	JMP .TOGGLE.VOICE_MODE

.END.INPUT_STEP
	JMP .END.INPUT
	
.NPC.TALK.INPUT.LOOP_STEP
	JMP NPC.TALK.INPUT.LOOP
	
.BACKSPACE.PRESSED	;if backspace is pressed, erase character in left adjacent column, move cursor 1 column left, and restart loop. 	
	LDA KEYIN.STRING.LEFT_EDGE ;load the left edge of the input field
	CMP HTAB ;is the cursor at the left edge? 
	BCS .ITERATION.DONE ;if yes, then don't permit backspace, get next input character
	DEC HTAB
	JSR UPDATE.CHAR.POS

.UPDATE.INDEX
;UPDATE INDEX/COUNTERS TO REFLECT BACKSPACE	
	INX						;add one back to the character input counter
	DEY						;subtract one from the keyboard input array (RESULT) index
	STX KEYIN.STRING.XREG
	STY KEYIN.STRING.YREG
	
	LDA #$A0						;ASCII CODE: space
	JSR COUT

	DEC HTAB
	JSR UPDATE.CHAR.POS
	;**FALLS THROUGH**


.ITERATION.DONE

	LDA ANIMATION.CALLED_BY
	BEQ .NPC.TALK.INPUT.LOOP_STEP2
	;**FALLS THROUGH**

	
.ANIMATION_MANAGER.RETURN
	RTS ;return to animation manager


.NPC.TALK.INPUT.LOOP_STEP2
	JMP NPC.TALK.INPUT.LOOP
	

.END.INPUT				
.ADD.STOP_VALUE			
	LDA #NPC.TALK.KEYWORD.STOP_VALUE
	STA NPC.TALK.KEYWORD,Y


	JMP .EXIT
	
.TOGGLE.VOICE_MODE	;switch between NORMAL/YELL/WHISPER				
	INC NTALK.VOICE_MODE
	LDA NTALK.VOICE_MODE ;load current voice mode ($00 = normal, $01 = yell, $02 = normal)
	CMP #NTALK.VOICE_MODE.MAX  ;is variable ready to flip back to $00?
	BNE .UPDATE.VOICE_MODE.DISPLAY ;if no, the value after the INC above stands, resume gathering player input
	LDA #$00
	STA NTALK.VOICE_MODE ;if yes, then flip NTALK.VOICE_MODE to #$00 (normal voice)

.UPDATE.VOICE_MODE.DISPLAY
	JSR UPDATE.VOICE_MODE.DISPLAY
	JMP .ITERATION.DONE


.EXIT	
;RESTORE CURSOR POSITION
		LDA CURSOR.POSITION.SAVED+$0
		STA HTAB	
		LDA CURSOR.POSITION.SAVED+$1
		STA VTAB
	JSR	UPDATE.CHAR.POS

;Fix stack if calling routine was animation manager
	LDA ANIMATION.CALLED_BY
	BEQ .RESTORE.REGISTERS

	
	; PLA ;registers saved at entrance to NPC.TALK.INPUT
	; PLA

	LDX #$00 ;init counter
.POP.LOOP
;Note: this loop is does the equivilent of the following:
	; PLA ;pop 1 RTS off the stack (JSR ANIMATION.UPDATE)
	; PLA
	;
	; PLA ;registers saved at entrance to ANIMATION.UPDATE
	; PLA
	; PLA
	;
	; PLA ;pop 1 RTS off the stack (JSR NPC.TALK.INPUT.CHECK.KB_BUFF)
	; PLA	
	;
	; PLA ;registers saved just before recusive call to NPC.TALK.INPUT
	; PLA
	;
	; PLA ;pop 1 RTS off the stack (JSR NPC.TALK.INPUT)
	; PLA	
	
	PLA ;pop a value off the stack
	INX
	CPX #$0B 
	BNE .POP.LOOP


;RESET CALL TRACER
	LDA #$00				;reset to "not tracked" value
	STA ANIMATION.CALLED_BY
	
	; LDA #$01
	; STA TROUBLESHOOTING.HOOK
	
	;**FALLS THROUGH**	

.RESTORE.REGISTERS	
	PLA
	TAY
	PLA
	TAX	

	RTS ;return to the routine that called NPC.TALK.INPUT	

	
@END

PRINT.TEXT.WINDOW
@START
;=====================SUBROUTINE DOCUMENTATION====================================
;
;Parses an input string into a buffer, to facilitate word wrap functionality and
;screen scrolling within a defined text window. After any adjustments are needed to the 
;cursor position, or to the text within the window (scrolling), the buffer is output via
;PRINT.STR and ultimately COUT, which updates the HTAB/VTAB zero page addresses to track the
;cursor position. 
;
;The tracking of the cursor position could easily be done with this routine but the
;HRCG controller is setup to use COUT and I haven't spend the time to consider disconnecting them.
;The HRCG controller handles the plotting of the characters to the high-res screen, by 
;calculating a shape table address based on the ASCII value of the character sent to it. 
;
;=================================================================================

; ; ;DRIVER TEMPLATE (INIT)
; ;INIT TEXT WINDOW
; ;NOTE: HTAB/VTAB must be set before the text window is set. If they are set after, or not set at all, strange things happen. 
;
		; ;INIT TEXT WINDOW BOUNDARIES (for the text itself, excluding borders)
		; LDA #$19  ;$19full  /$1Etest 
		; STA TWF.LEFT_SBYTE
		;
		; LDA #$0D ;right edge of text is $26, border is at $27
		; STA TWF.WIDTH
		;
		; LDA #$01
		; STA TWF.TOP_ROW
		;
		; LDA #$03			
		; STA TWF.BOTTOM_ROW
;
		; ;SET CURSOR POSITION TO UPPER LEFT OF TEXT WINDOW
		; LDA #$19			;$19full / $1E test
		; STA HTAB	
		; LDA #$1
		; STA VTAB
		; JSR	UPDATE.CHAR.POS
				
; ;DRIVER TEMPLATE (PRINTING)
;
;
; ;PRINT "TEST"	
;
;		
	; JMP .TEXT1.PRINT
; ;.TEXT1 .AZ -/Test/,#$8D,#$8D			;ASCII text string
; .TEXT1.PRINT
		; LDA #.TEXT1 					
		; STA TWF.STRING
		; ;STA STRING
;		
		; LDA /.TEXT1
		; STA TWF.STRING+$1						
		; ;STA STRING+$1
	; ;JSR PRINT.STR	
		; ;JSR KEYIN
	; JSR PRINT.TEXT.WINDOW

	
.SAVE.REGISTERS
	TXA
	PHA
	TYA
	PHA


;INIT VARIABLES
	LDY #$00 ;init input string index
	LDX #$00 ;init buffer index
	
	STX TWF.PRINTED_LINE.COUNTER
	
	;setup remaining text window dimesions
	LDA TWF.LEFT_SBYTE		;load left edge position
	CLC
	ADC TWF.WIDTH	;add width
	STA TWF.RIGHT_SBYTE		;result is screen byte of right edge of text space
	INC TWF.RIGHT_SBYTE		;add 1 so that it can be used as a stop value after an index increment
	
	;setup bottom row trigger for scroll prompt
	LDA TWF.BOTTOM_ROW
	STA TWF.BOTTOM_ROW.TRIGGER
	DEC TWF.BOTTOM_ROW.TRIGGER	;the trigger should be 1 row less than the bottom so that the prompt can be printed on the last row of the text window
	
.LOOP.PARSE.STRING	
	LDA (TWF.STRING),Y
	CMP #$8D ;ASCII = return key
	BEQ	.CARRIAGE.RETURN
	CMP #NPC.TEXT.BLOCK.STOP_VALUE
	BEQ .END.OF.STRING
	;**FALLS THROUGH** ;input character is something other than a space or carriage return
.ADD.TO.BUFFER 
;Add current character to the buffer
	STA TEXT.WINDOW.CHAR	;save current character parsed from TW.STRING
	CPX TWF.WIDTH			;does the buffer contain the same number of characters as the width of the text window?
	BCS .LINE.FULL			;if yes then exit loop
	
	STA TEXT.WINDOW.BUFFER,X ;if no, then save current character to buffer

	INX	;increment buffer index
	INY	;increment input string index
	BEQ	.OVERFLOW.ERROR_STEP 	;if index flip to $00, then report error
	LDA TEXT.WINDOW.CHAR	;restore current character parsed from TW.STRING
	CMP #$A0 ;ASCII = space key
	BNE .LOOP.PARSE.STRING	;if no space character found, continue loop		
	;**FALLS THROUGH** ;if space char found, then determine which line buffer should be output to
		
.OUTPUT.BUFFER
	;WILL BUFFER FIT ON CURRENT LINE?
	STX TEXT.WINDOW.BUFFER.TALLY	;save buffer index which is also the number of characters in the buffer+1
	LDA TWF.RIGHT_SBYTE						;load text window right edge
	SEC
	SBC HTAB						;subtract cursor horizontal (x-axis) position, which = chars left in line. 
	; CLC
	; ADC #01							;converts the grid positional distnce between right edge and cursor into the number of characters left in the current line. Takes into account that even though the buffer index was increment in preperation for the stop value to be written below, TEXT.WINDOW.BUFFER.TALLY is not 1 greater than the actual number of characters in the buffer because the tally is equal to the index which started at 0.  
	
	CMP TEXT.WINDOW.BUFFER.TALLY	;is the number of chars in buffer+1 less than chars left in line?
	BCS .PRINT.STRING				;if yes, then word will fit on current line, output word via COUT
				;if no, reset cursor position to next line

	;RESET CURSOR TO START OF NEXT LINE
.NEXT_LINE
		INC VTAB	;move cursor to next row
		LDA TWF.LEFT_SBYTE	
		STA HTAB	;rest cursor X-axis to the left edge of text window
	JSR	UPDATE.CHAR.POS
	INC TWF.PRINTED_LINE.COUNTER		;increment counter to reflect that another line of text has been printed to the text window

	LDA VTAB 						;load the row number of the cursor position
	CMP TWF.BOTTOM_ROW.TRIGGER		;is the cursor position <= the bottom row-1 of the text window?
	BCC .PRINT.STRING				;output word via COUT
	BEQ .PRINT.STRING				;output word via COUT

	JSR SCROLL.TEXT.WINDOW			;if yes, the scroll the text window with prompt	
	
		;move cursor to 2nd to last row in the text window. last row is reserved for the <ANY KEY> page break prompt. 
		LDA TWF.BOTTOM_ROW.TRIGGER ;load bottom row -1
		STA VTAB
		LDA TWF.LEFT_SBYTE	;load screen byte on left edge of text space
		STA HTAB	;reset cursor X-axis to the left edge of text window
	JSR	UPDATE.CHAR.POS

	; ;reset printed line counter
	; LDA #$00
	; STA TWF.PRINTED_LINE.COUNTER
			
	JMP .PRINT.STRING				;output word via COUT

.OVERFLOW.ERROR_STEP
	JMP .OVERFLOW.ERROR
	
.CARRIAGE.RETURN
	CPX #$00			;is buffer empty?
	BEQ .PROCESS.CARRIAGE.RETURN	;if yes, then nothing to output, proceed directly to processing of carriage return

	;ACC = carrige return char from TW.STRING
	STA TEXT.WINDOW.CHAR	;save current character parsed from TW.STRING

	JMP .OUTPUT.BUFFER		;output current contents of buffer (not including carriage return character). Carriage return will get processed in .PRINT.STRING after the JSR PRINT.STR (which calls COUT)
		
.END.OF.STRING
	;ACC = stop value from TW.STRING
	STA TEXT.WINDOW.CHAR	;save current character parsed from TW.STRING, which should be the $00 stop value
	
	CPX #$00			;is buffer empty?
	BEQ .EXIT			;if yes, then exit
				;if no, then output the buffer via COUT

;FINAL BUFFER OUTPUT
	JMP .OUTPUT.BUFFER

; .EXIT_STEP
	; JMP .EXIT
	
.LINE.FULL
;The buffer has the maximum number of characters that can fit on a line.
;even though a space or end-of-string value was not encountered. Unless
;the cursor is on the left edge of the text window, the buffer needs to be output to
;the next line

	LDA TWF.LEFT_SBYTE		;load column of ledge edge of text window
	CMP HTAB		;is it less than the column that the cursor is in?
	BCC .NEXT_LINE  ;if yes, then advance to next line and output the buffer	
	;**FALLS THROUGH** ;if no, then output the buffer on the current line (cursor is on the left edge of the text window)
	
.PRINT.STRING
	;add stop value
	LDA #TWF.STRING.STOP.VALUE
	STA TEXT.WINDOW.BUFFER,X

	;setup pointer for PRINT.STRING
	LDA #TEXT.WINDOW.BUFFER
	STA STRING+$0
	
	LDA /TEXT.WINDOW.BUFFER
	STA STRING+$1
	
		;parm STRING set in init above
	JSR PRINT.STR

	LDX #$00 ;reset buffer index

		;JSR KEYIN

		
	LDA TEXT.WINDOW.CHAR		;end of string?
	BEQ .EXIT					;if yes, exit

	LDA TEXT.WINDOW.CHAR		;is current input character a carriage return? (which would not have been written to the buffer just output)
	CMP #$8D ;ASCII = return key
	BNE	.RETURN.TO.LOOP			;if no, return to the loop without further ado

.PROCESS.CARRIAGE.RETURN	
	;ADVANCE TO NEXT LINE, RESET X-AXIS (VTAB)
		INC VTAB	;move cursor to next row
		LDA TWF.LEFT_SBYTE	
		STA HTAB	;rest cursor X-axis to the left edge of text window
	JSR	UPDATE.CHAR.POS
	INC TWF.PRINTED_LINE.COUNTER		;increment counter to reflect that another line of text has been printed to the text window

	LDA VTAB 						;load the row number of the cursor position
	CMP TWF.BOTTOM_ROW.TRIGGER		;is the cursor position <= the bottom row-1 of the text window?
	BCC .INCREMENT.INDEX			;don't scroll text window
	BEQ .INCREMENT.INDEX			;don't scroll text window

	JSR SCROLL.TEXT.WINDOW			;if yes, the scroll the text window with prompt	
	
		;move cursor to 2nd to last row in the text window. last row is reserved for the <ANY KEY> page break prompt. 
		LDA TWF.BOTTOM_ROW.TRIGGER ;load bottom row -1
		STA VTAB
		LDA TWF.LEFT_SBYTE	;load screen byte on left edge of text space
		STA HTAB	;reset cursor X-axis to the left edge of text window
	JSR	UPDATE.CHAR.POS
	
.INCREMENT.INDEX	
	INY	;increment input string index
		;buffer index not incremented because we aren't adding the carriage return character to the buffer since we don't want PRINT.STRING and thus COUT to print it.
	BEQ	.OVERFLOW.ERROR_STEP 	;if index flip to $00, then report error

	;**FALLS THROUGH**

.RETURN.TO.LOOP	
	JMP .LOOP.PARSE.STRING	

.EXIT

		; JSR KEYIN
		; LDA #$AA
		; LDX #TEXT.WINDOW.BUFFER
		; LDY /TEXT.WINDOW.BUFFER
		; JSR PREP.BRK
		; BRK
		
.RESTORE.REGISTERS
	PLA
	TAY
	PLA
	TAX

	RTS

.OVERFLOW.ERROR
;PRINT.TEXT.WINDOW reports an overflow in the input array index (Y-REG)
	JSR PREP.BRK
	BRK
	
; .OVERFLOW.ERROR_STEP
		; JMP .OVERFLOW.ERROR
		
SCROLL.TEXT.WINDOW
@START

;SAVE REGISTERS	
	TXA
	PHA
	TYA
	PHA

.CHECK.PAGE.BREAK
;Note: if the input string printed since the PRINT.TEXT.WINDOW was called, or since the last scroll, is larger than the text window, generate a page break prompt before scrolling the screen so that the player has a chance to read all the text.

	LDA TWF.PRINTED_LINE.COUNTER ;load number of lines printed to the text window since the PRINT.TEXT.WINDOW was called, or since the last scroll
	CMP TWF.BOTTOM_ROW.TRIGGER   ;has the text window been filled by a contigous string? (TWF.BOTTOM_ROW.TRIGGER contains the 2nd to last row of the text window, which we check because the last row is reserved for the page break prompt)
	BNE .SCROLL				     ;if no, proceed with scroll 

.PAGE.BREAK.PROMPT
;PRINT "<any key>"

	;reset printed line counter
	LDA #$00
	STA TWF.PRINTED_LINE.COUNTER
	
	;SET CURSOR POSITION TO LOWER LEFT OF TEXT WINDOW
		LDA TWF.LEFT_SBYTE			;$19full / $1E test
		STA HTAB	
		LDA TWF.BOTTOM_ROW
		STA VTAB
	JSR	UPDATE.CHAR.POS

;PRINT "ANY KEY" PROMPT.
		LDA #TEXT_BLOCK.ANY_KEY2					
		STA STRING
		
		LDA /TEXT_BLOCK.ANY_KEY2
		STA STRING+$1
	JSR PRINT.STR	

.WAIT.FOR.KEY

	JSR KEYIN.ANIMATION.SINGLE	

.ERASE.ANY_KEY.PROMPT
	LDA TWF.BOTTOM_ROW		;load the text screen Y-AXIS (VTAB) of the bottom row
	ASL ;X 2
	ASL	;X 4
	ASL ;X 8								
	STA DRAW.START_LINE		;set starting line for the draw routine
	CLC
	ADC #TEXT.CHAR.DEPTH
	STA DRAW.STOP_LINE		;set stop line for the draw routine
	
	LDA TWF.LEFT_SBYTE
	STA DRAW.START_BYTE
	
	LDA TWF.RIGHT_SBYTE
	STA DRAW.STOP_BYTE
	
		LDA #$83		;set draw to both pages
		;STA USE.PAGE
			;we set high bit as a parameter to indicate we want the byte value for the draw to be $00
	JSR DRAW.LINE	


.SCROLL	
.INIT.SCREEN					;setup screen byte, line, depth	

	LDA TWF.TOP_ROW	;load the Y-AXIS of text cursor position
	ASL ;X 2
	ASL	;X 4
	ASL ;X 8								
	STA LINE.START.COPY_TO		;set starting line for the COPY TO part of the scroll
	CLC
	ADC #TEXT.CHAR.DEPTH
	STA LINE.START.COPY_FROM	;set starting line for the copy from part of the scroll operation
	
	LDA TWF.BOTTOM_ROW
	ASL ;X 2
	ASL	;X 4
	ASL ;X 8
	STA TWF.SCROLLING.STOP_LINE	;set the stop line, which is the last line of the last row of text characters to scroll +$08. The last row of text characters to scroll is 1 less than the bottom row. Thus, taking the bottom row # *8 gets us exactly the value we need. 
	
.ROWLOOP

	LDX LINE.START.COPY_TO		;prepare for new row of text characters: reset the current COPY TO line to the starting line
	
	LDA LINE.START.COPY_TO
	CLC
	ADC #TEXT.CHAR.DEPTH		;add depth of shape to starting line in hex (# of lines, not the position of last line....so line positions $0-$f is $10 (!16) total lines)
	STA LINE.STOP				;set the last line to draw based on the depth of the text characters
	
.LINELOOP
	
		
	LDY TWF.LEFT_SBYTE			;reset screen byte index to the left edge of text window	
	
.GET.LINE.ADDRESS1B	
	TXA						;X is the tile.line counter, and used by the get line address routine, so we need to save it to the stack
	PHA
		LDX LINE.START.COPY_FROM
		LDA PAGE.FOREGROUND
	JSR GET.LINE.ADDRESS1
	
;CALCULATE BASE ADDRESS FOR OPPOSITE PAGE	
	LDA PAGE.FOREGROUND
	CMP #$01
	BEQ .PAGE.01.FOREGROUND1
.PAGE.01.BACKGROUND1
	;CALCULATE BASE ADDRESS FOR FOREGROUND PAGE
	LDA LINE.BASE.ADDR1+$0
	STA LINE.BASE.ADDR3+$0

	LDA LINE.BASE.ADDR1+$1
	SEC
	SBC #$20
	STA LINE.BASE.ADDR3+$1
	
	JMP .LOOKUP1B.COMPLETE
	
.PAGE.01.FOREGROUND1
	;CALCULATE BASE ADDRESS FOR BACKGROUND PAGE
	LDA LINE.BASE.ADDR1+$0
	STA LINE.BASE.ADDR3+$0

	LDA LINE.BASE.ADDR1+$1
	CLC
	ADC #$20
	STA LINE.BASE.ADDR3+$1
	;**FALLS THROUGH**

.LOOKUP1B.COMPLETE
	PLA						;restore x-reg from stack. it was transfered there at the start of the get line address2 routine
	TAX
	
.GET.LINE.ADDRESS2B

		LDA PAGE.FOREGROUND
	JSR GET.LINE.ADDRESS2

;CALCULATE BASE ADDRESS FOR OPPOSITE PAGE	
	LDA PAGE.FOREGROUND
	CMP #$01
	BEQ .PAGE.01.FOREGROUND2
.PAGE.01.BACKGROUND2
	;CALCULATE BASE ADDRESS FOR FOREGROUND PAGE
	LDA LINE.BASE.ADDR2+$0
	STA LINE.BASE.ADDR4+$0

	LDA LINE.BASE.ADDR2+$1
	SEC
	SBC #$20
	STA LINE.BASE.ADDR4+$1
	
	JMP .LOOKUP2B.COMPLETE
	
.PAGE.01.FOREGROUND2
	;CALCULATE BASE ADDRESS FOR BACKGROUND PAGE
	LDA LINE.BASE.ADDR2+$0
	STA LINE.BASE.ADDR4+$0

	LDA LINE.BASE.ADDR2+$1
	CLC
	ADC #$20
	STA LINE.BASE.ADDR4+$1
	;**FALLS THROUGH**
.LOOKUP2B.COMPLETE
	
.COPYLOOP
;COPY TILE DATA	

	LDA (LINE.BASE.ADDR1),Y		;TEXT CHAR DATA: COPY FROM  (foreground page)	
	STA (LINE.BASE.ADDR2),Y		;TEXT CHAR DATA: COPY TO	(foreground page)

	LDA (LINE.BASE.ADDR3),Y		;TEXT CHAR DATA: COPY FROM  (background page)	
	STA (LINE.BASE.ADDR4),Y		;TEXT CHAR DATA: COPY TO	(background page)

	
	;ERASE AS WE GO 
	;Note: This is done because the next line of text printed by the text window might not take the entire line, and thus might n0t take care of erasing all of it
	LDA #$00
	STA (LINE.BASE.ADDR1),Y		;foreground page
	STA (LINE.BASE.ADDR3),Y		;background page
	
	INY								;advance to next screen byte

	CPY TWF.RIGHT_SBYTE				;is line complete? (i.e. have all screen bytes in the line been scrolled?)
	BCS .LINE.COMPLETE				;if yes, switch over to next row (bcs: is y-reg >= cpy value)
	JMP .COPYLOOP					;if no, copy next screen byte

.LINE.COMPLETE
			
	INX								;next tile line
	INC LINE.START.COPY_FROM
		
	CPX LINE.STOP					;is tile done?	
	BCC .LINELOOP					;if no, draw next line (bcc: is acc < cmp value)
	

.ROW.COMPLETE.SOUTH			
	LDA LINE.START.COPY_TO			;advance line to next row of tiles
	CLC
	ADC #TEXT.CHAR.DEPTH
	STA LINE.START.COPY_TO
	CLC
	ADC #TEXT.CHAR.DEPTH
	STA LINE.START.COPY_FROM		;start line of tile 1 row down	
	CMP	TWF.SCROLLING.STOP_LINE		;are all rows scrolled?
	BCS .SCROLL.COMPLETE			;if yes, exit (BCS: is ACC >= CMP value)
	JMP .ROWLOOP

		
.SCROLL.COMPLETE

;RESTORE REGISTERS	
	PLA
	TAY
	PLA
	TAX
	
	RTS
	
@END
@END

CLEAR.TEXT_WINDOW.RIGHT
@START
;PARAMETERS:  USE.PAGE*
;RETURN: NONE
;ENTRANCE: NPC.TALK, DIRECT
;*Specifies which hi-res page the text window should be erased on (BOTH = $03)

;=====================SUBROUTINE DOCUMENTATION====================================
;
;=================================================================================

	PHA ;save USE.PAGE parameter to ACC

.INIT
	LDA #TWS.RIGHT_WINDOW.TOP_LINE		;load line in x register	
	STA DRAW.START_LINE
	
	LDA #TWS.RIGHT_WINDOW.LEFT_SBYTE
	STA DRAW.START_BYTE
	
	LDA #TWS.RIGHT_WINDOW.RIGHT_SBYTE+$1
	STA DRAW.STOP_BYTE
		
	LDA #TWS.RIGHT_WINDOW.BOTTOM_LINE+$1
	STA DRAW.STOP_LINE

.DRAW
		PLA ;restore USE.PAGE parameter to ACC
		ORA #$80	;set high bit as a parameter to indicate we want the byte value for the draw to be $00
	JSR DRAW.LINE

.EXIT
	RTS

@END

CLEAR.TEXT_WINDOW.RIGHT_SMALL
@START
; ;PARAMETERES: USE.PAGE*
; ;ENTRANCE: DIRECT
; ;RETURN: NONE
; ;*which hi-res page to erase on (1 or 2, not both)

; ;**OPT** The draw/erase loops of the various text window draw/erase subroutines can probably be consolidated into a generic draw routine that takes the start & stop  line/screenbyte as parameters. The individual subroutines as they exist now would just set the parameters and JSR the generic draw function

	; STA USE.PAGE
	
; .SAVE.REGISTERS
	; TXA
	; PHA
	; TYA
	; PHA
	
; .INIT
	; LDX #TWB.RIGHT_S_WINDOW.TOP_LINE		;load line in x register	
	; LDY #TWB.RIGHT_S_WINDOW.LEFT_SBYTE	;set screen byte index in y register	

; .ERASE.LOOP
; @START

	; ; LDA USE.PAGE
	; ; CMP #$03			;clear window on both hi-res pages?
	; ; BEQ .CLEAR.BOTH.PAGES	

	; ; ;ACC = USE.PAGE
		; ;LDA PAGE.FOREGROUND
		; LDA USE.PAGE
	; JSR GET.LINE.ADDRESS1
	; JMP .ERASE.SINGLE.PAGE
	; ; ;**FALLS THROUGH**
	
; ; .CLEAR.BOTH.PAGES			
	; ; LDA #$01	;get page1 address first
	; ; JSR GET.LINE.ADDRESS1
	
	; ; ;CALCULATE PAGE2 ADDRESS 
	; ; LDA LINE.BASE.ADDR1+$1
	; ; CLC
	; ; ADC #$20
	; ; STA LINE.BASE.ADDR2+$1	;subtract $2000 from line address

	; ; ;SET LO ADDRESS
	; ; LDA LINE.BASE.ADDR1
	; ; STA LINE.BASE.ADDR2
	; ; ;**FALLS THROUGH**
	
; ;.ERASE.BOTH.PAGES
	
; ; .LOOP.ROW
	; ; LDA #$00
	
	; ; STA (LINE.BASE.ADDR1),Y	;PLOT (1st screen byte)
	; ; STA (LINE.BASE.ADDR2),Y	;PLOT (1st screen byte)
	
	; ; INY  					;next screen byte
	; ; CPY #TWB.RIGHT_S_WINDOW.RIGHT_SBYTE+$1  ;at text window edge?
	; ; BNE .LOOP.ROW

	; ; LDY #TWB.RIGHT_S_WINDOW.LEFT_SBYTE	;reset screen byte to text window edge (it's actually the start byte in this routine)			
	; ; INX						;next tile line
	
	; ; CPX #TWB.RIGHT_S_WINDOW.BOTTOM_LINE+$1		;LAST LINE?							
	; ; BCC .ERASE.LOOP			;IF NO, DRAW NEXT LINE (BCC: is X-REG < CPX value)	
	; ; JMP .EXIT
	
	
; .ERASE.SINGLE.PAGE



; .LOOP.ROW2
	; LDA #$00
	
	; STA (LINE.BASE.ADDR1),Y	;PLOT (1st screen byte)
	
	; INY  					;next screen byte
	; CPY #TWB.RIGHT_S_WINDOW.RIGHT_SBYTE+$1
	; BNE .LOOP.ROW2

	; LDY #TWB.RIGHT_S_WINDOW.LEFT_SBYTE	;reset screen byte to text window edge			;SCREEN BYTE INDEX IN Y REGISTER	
	; INX						;next line
	
	; CPX #TWB.RIGHT_S_WINDOW.BOTTOM_LINE+$1  ;last line?							
	; BCC .ERASE.LOOP			;IF NO, DRAW NEXT LINE (BCC: is X-REG < CPX value)	
	; ;**FALLS THROUGH**
	
; .EXIT
		
	; PLA
	; TAY
	; PLA
	; TAX
	
	; RTS

@END

CLEAR.TEXT_WINDOW.TOP
@START
;PARAMETERES: NONE
;ENTRANCE: DIRECT
;RETURN: NONE


.INIT
	LDA #TWB.TOP_WINDOW.TOP_LINE		;load line in x register	
	STA DRAW.START_LINE
	
	LDA #TWB.TOP_WINDOW.LEFT_SBYTE
	STA DRAW.START_BYTE
	
	LDA #TWB.TOP_WINDOW.RIGHT_SBYTE+$1
	STA DRAW.STOP_BYTE
		
	LDA #TWB.TOP_WINDOW.BOTTOM_LINE+$1
	STA DRAW.STOP_LINE

.DRAW	
		LDA #$83		;set draw to both pages
			;we set high bit as a parameter to indicate we want the byte value for the draw to be $00
		;STA USE.PAGE
	JSR DRAW.LINE
	
	RTS
	


@END

CLEAR.TEXT_WINDOW.BOTTOM
@START
;PARAMETERES: NONE
;ENTRANCE: DIRECT
;RETURN: NONE

.INIT
	LDA #TWB.BOTTOM_WINDOW.TOP_LINE		;load line in x register	
	STA DRAW.START_LINE
	
	LDA #TWB.BOTTOM_WINDOW.LEFT_SBYTE
	STA DRAW.START_BYTE
	
	LDA #TWB.BOTTOM_WINDOW.RIGHT_SBYTE+$1
	STA DRAW.STOP_BYTE
		
	LDA #TWB.BOTTOM_WINDOW.BOTTOM_LINE+$1
	STA DRAW.STOP_LINE

	
.DRAW	
		LDA #$83		;set draw to both pages
			;we set high bit as a parameter to indicate we want the byte value for the draw to be $00
		;STA USE.PAGE
	JSR DRAW.LINE
	

.EXIT
	
	RTS
@END

RESET.TEXT_WINDOW.TALK_INPUT
@START
;PARAMETERES: NONE
;ENTRANCE: DIRECT
;RETURN: NONE


.INIT
	LDA #TWS.TALK_INPUT_WINDOW.TOP_LINE		;load line in x register	
	STA DRAW.START_LINE
	
	LDA #TWS.TALK_INPUT_WINDOW.LEFT_SBYTE
	STA DRAW.START_BYTE
	
	LDA #TWS.TALK_INPUT_WINDOW.RIGHT_SBYTE+$1
	STA DRAW.STOP_BYTE
		
	LDA #TWS.TALK_INPUT_WINDOW.BOTTOM_LINE+$1
	STA DRAW.STOP_LINE
	
.DRAW	
		LDA #$83		;set draw to both pages
			;we set high bit as a parameter to indicate we want the byte value for the draw to be $00
		;STA USE.PAGE
	JSR DRAW.LINE
	
.UPDATE.OTHER.TEXT
;SAVE TEXT WINDOW CURSOR POSITION
;(Note: because we have to modify HTAB/VTAB to draw the cursor prompt and display the voice mode text)
		LDA HTAB	
		STA CURSOR.POSITION.SAVED+$0
		LDA VTAB
		STA CURSOR.POSITION.SAVED+$1
	JSR	UPDATE.CHAR.POS
	
;draw cursor prompt

;PRINT "@"
		LDA #TWS.TALK_INPUT_WINDOW.PROMPT_START.HTAB ;19 originally
		STA HTAB	
		LDA #TWS.TALK_INPUT_WINDOW.PROMPT_START.VTAB ;14 originally
		STA VTAB
	JSR	UPDATE.CHAR.POS

		
	JMP .CURSOR.PRINT
	
.CURSOR .AZ -/@/			;ASCII text string
.CURSOR.PRINT
		LDA #.CURSOR 					
		STA STRING
		
		LDA /.CURSOR						
		STA STRING+$1
	JSR PRINT.STR

	JSR UPDATE.VOICE_MODE.DISPLAY ;this is text normally in the input window so it needs to be output to video screen again after the clear. 

;RESTORE TEXT WINDOW CURSOR POSITION
		LDA CURSOR.POSITION.SAVED+$0
		STA HTAB	
		LDA CURSOR.POSITION.SAVED+$1
		STA VTAB
	JSR	UPDATE.CHAR.POS
	
.EXIT
	
	RTS
@END



INIT.TEXT_WINDOW.RIGHT
@START
	
		;LDA PAGE.BACKGROUND	
		LDA #$03
	JSR CLEAR.TEXT_WINDOW.RIGHT
	JSR CLEAR.TEXT_WINDOW.TOP	
	JSR CLEAR.TEXT_WINDOW.BOTTOM
	JSR DRAW.TEXT_WINDOW.RIGHT
		JSR FLIP.PAGE
		;JSR KEYIN
		

;INIT TEXT WINDOW

		;INIT TEXT WINDOW BOUNDARIES (for the text itself, excluding borders)
		LDA #$19  ;$19full  /$1Etest 
		STA TWF.LEFT_SBYTE
		
		LDA #$0D  ;right edge of text is $26, border is at $27
		STA TWF.WIDTH
		
		LDA #$01
		STA TWF.TOP_ROW
		
		LDA #$11 ;$12original	
		STA TWF.BOTTOM_ROW

		; ;draw cursor prompt

; ;PRINT "@"
		; LDA #$19			;$19full / $1E test
		; STA HTAB	
		; LDA #$14
		; STA VTAB
		; JSR	UPDATE.CHAR.POS
		
	; JMP .TEXT0.PRINT
	
; .TEXT0 .AZ -/@/			;ASCII text string
; .TEXT0.PRINT
		; LDA #.TEXT0 					
		; STA STRING
		
		; LDA /.TEXT0						
		; STA STRING+$1
	; JSR PRINT.STR	

	
	
; ;SET CURSOR IN START POSITION	
		
		; ;SET CURSOR POSITION TO UPPER LEFT OF TEXT WINDOW
		; LDA #$19			;$19full / $1E test
		; STA HTAB	
		; LDA #$11		;$12
		; STA VTAB
		; JSR	UPDATE.CHAR.POS
	
	RTS
	
@END


DRAW.TEXT_WINDOW.RIGHT
@START

;INIT VARIABLES
	LDA #$00
	STA SCREEN_BYTE.COUNTER

	LDA #$01
	STA TW.RIGHT_WINDOW.CLEAN_UP.FLAG   ;turn flag on so that DRAW.SCREEN will erase the right edge and first two bytes of the top/bottom lines of the text window border. 
										;This is needed because this is the portion of the text window border that isn't normally onscreen, AND CLEAR.TEXT_WINDOW.RIGHT only clears the text space, not the border. If it were to clear the border on both pages the foreground erase would be very noticable.
										;Also used via TW.RIGHT_WINDOW.STATUS.FLAG (.EQ) to tell the animation manager not to draw on top of the text window
	
.DRAW.SCREEN_BORDER
;Note: this is to erase any contents of the 1 line text windows at the top and bottom of the general screen

;TOP    LINE: $3000 PAGE1 (ADDR1), $5000 PAGE2 (ADDR2)
;BOTTOM LINE: $33D0 PAGE3 (ADDR3), $53D0 PAGE4 (ADDR4)

	LDY #$02			;init screen byte index

;INIT LINE BASE ADDRESSES

	;init LO bytes
	LDA #$00
	STA LINE.BASE.ADDR1
	STA LINE.BASE.ADDR2
	
	LDA #$D0
	STA LINE.BASE.ADDR3
	STA LINE.BASE.ADDR4
	
	;init HO bytes
	LDA #$30
	STA LINE.BASE.ADDR1+$1

	LDA #$50
	STA LINE.BASE.ADDR2+$1

	LDA #$33
	STA LINE.BASE.ADDR3+$1

	LDA #$53
	STA LINE.BASE.ADDR4+$1	
	
;DRAW TOP AND BOTTOM LINE OF BORDER ON BOTH HI-RES PAGES	
.SCREEN_BORDER.LOOP
	LDA #$D5
	STA (LINE.BASE.ADDR1),Y
	STA (LINE.BASE.ADDR2),Y
	STA (LINE.BASE.ADDR3),Y
	STA (LINE.BASE.ADDR4),Y
	
	INY ;move 1 screen byte right
	
	LDA #$AA
	STA (LINE.BASE.ADDR1),Y
	STA (LINE.BASE.ADDR2),Y
	STA (LINE.BASE.ADDR3),Y
	STA (LINE.BASE.ADDR4),Y
	
	INY ;move 1 screen byte right
	
	CPY #SCREEN.STOP_BYTE-$04
	BNE .SCREEN_BORDER.LOOP

	
.DRAW.TW.BORDER.TOP
		LDA #TWB.RIGHT_WINDOW.TOP_LINE
		STA DRAW.START_LINE
		
		LDA #$24
		STA DRAW.START_BYTE
		
		LDA #$28
		STA DRAW.STOP_BYTE
			
		LDA #TWB.RIGHT_WINDOW.TOP_LINE+$1
		STA DRAW.STOP_LINE

		LDA #$D5	
		STA DRAW.BYTE_VALUE+$0
		
		LDA #$AA	
		STA DRAW.BYTE_VALUE+$1
		LDA #$03		;set draw to both pages
			;we set high bit as a parameter to indicate we want the byte value for the draw to be $00
	JSR DRAW.LINE
	
	
;SETUP DRAW, TEXT WINDOW BORDER (RIGHT SIDE): RIGHT EDGE
	LDY #TWB.RIGHT_WINDOW.RIGHT_SBYTE
	
	LDA PAGE.BACKGROUND
	LDX #TWB.RIGHT_WINDOW.TOP_LINE+$1 ;line 5
	JSR GET.LINE.ADDRESS1

	LDA PAGE.FOREGROUND
	JSR GET.LINE.ADDRESS2
	
;DRAW TEXT WINDOW BORDER (RIGHT SIDE): RIGHT EDGE
.DRAW.TW.BORDER.RIGHT
	LDA #$A0					;bit mapped byte
	STA (LINE.BASE.ADDR1),Y		;save bit mapped byte to video screen memory (foreground page)
	STA (LINE.BASE.ADDR2),Y		;save bit mapped byte to video screen memory (background page)

	INX ;next line, move down
		LDA PAGE.BACKGROUND
	JSR GET.LINE.ADDRESS1 

	LDA PAGE.FOREGROUND
	JSR GET.LINE.ADDRESS2
	
	CPX #TWB.RIGHT_WINDOW.BOTTOM_LINE
	BNE .DRAW.TW.BORDER.RIGHT

	
	
;DRAW TEXT WINDOW BORDER (RIGHT SIDE): BOTTOM LINE
	LDY #TWB.RIGHT_WINDOW.RIGHT_SBYTE
	LDA #$AA
	STA (LINE.BASE.ADDR1),Y		;save bit mapped byte to video screen memory (foreground page) 
	STA (LINE.BASE.ADDR2),Y		;save bit mapped byte to video screen memory (background page)
	DEY ;move 1 screen byte left
	LDA #$D5
	STA (LINE.BASE.ADDR1),Y		;save bit mapped byte to video screen memory (foreground page) 
	STA (LINE.BASE.ADDR2),Y		;save bit mapped byte to video screen memory (background page)

	DEY ;move 1 screen byte left
	LDA #$AA
	STA (LINE.BASE.ADDR1),Y		;save bit mapped byte to video screen memory (foreground page) 
	STA (LINE.BASE.ADDR2),Y		;save bit mapped byte to video screen memory (background page)
	DEY ;move 1 screen byte left
	LDA #$D5
	STA (LINE.BASE.ADDR1),Y		;save bit mapped byte to video screen memory (foreground page)  
	STA (LINE.BASE.ADDR2),Y		;save bit mapped byte to video screen memory (background page)

;SETUP DRAW, TEXT WINDOW BORDER (RIGHT SIDE): LEFT EDGE

	;Move up 1 line
	LDA LINE.BASE.ADDR1+$1
	SEC
	SBC #$04				;Subtracts $400 from the line base address.
	STA LINE.BASE.ADDR1+$1

	;Move up 1 line
	LDA LINE.BASE.ADDR2+$1
	SEC
	SBC #$04				;Subtracts $400 from the line base address.
	STA LINE.BASE.ADDR2+$1
	
	LDY #TWB.RIGHT_WINDOW.LEFT_SBYTE
.DRAW.TW.BORDER.LEFT
	LDA #$84					;bit mapped byte
	STA (LINE.BASE.ADDR1),Y		;save bit mapped byte to video screen memory (foreground page)  
	STA (LINE.BASE.ADDR2),Y		;save bit mapped byte to video screen memory (background page)

	DEX ;next line, move up
		LDA PAGE.BACKGROUND
	JSR GET.LINE.ADDRESS1

		LDA PAGE.FOREGROUND
	JSR GET.LINE.ADDRESS2
	
	CPX #TWB.RIGHT_WINDOW.TOP_LINE
	BNE .DRAW.TW.BORDER.LEFT
	

;DRAW TALK INPUT WINDOW BORDER (RIGHT SIDE): TOP LINE
	
.DRAW.TW.INPUT.BORDER.TOP
	LDA PAGE.BACKGROUND
	LDX #TWB.TALK_INPUT_WINDOW.TOP_LINE
	JSR GET.LINE.ADDRESS1

	LDA PAGE.FOREGROUND
	JSR GET.LINE.ADDRESS2
	
	LDY #TWS.TALK_INPUT_WINDOW.LEFT_SBYTE
	LDA #$D5
.LOOP.TW.INPUT.BORDER.TOP
	LDA #$D5	
	STA (LINE.BASE.ADDR1),Y	;save to background page
	STA (LINE.BASE.ADDR2),Y ;save to foreground page
	INY ;move 1 screen byte right
	LDA #$AA
	STA (LINE.BASE.ADDR1),Y ;save to background page
	STA (LINE.BASE.ADDR2),Y ;save to foreground page

	INY ;move 1 screen byte right
	CPY #TWS.TALK_INPUT_WINDOW.RIGHT_SBYTE
	BNE .LOOP.TW.INPUT.BORDER.TOP

	; LDA #$D5
	; STA (LINE.BASE.ADDR1),Y ;save to background page
	; STA (LINE.BASE.ADDR2),Y ;save to foreground page
	; INY ;move 1 screen byte right
	; LDA #$AA
	; STA (LINE.BASE.ADDR1),Y ;save to background page
	; STA (LINE.BASE.ADDR2),Y ;save to foreground page
	
.EXIT
	RTS
	

@END

;FILLER TO PACKED DATA ADDRESS	
	.NO $BA00,$00	
					;This enables SBASM to generate an error if/when the code overshoots this memory address.

;**WARNING: code past this point will get
;clobbered by the packed data
					
PATCH.CODE.SECTION

		.PH MAP_OBJECTS.MOB ;start patch code section

;TEXT BLOCKS
@START
	;advance two lines and print two carriage return characters
CR.TEXT .AZ -#$8D,#$AD,#$8D,#$8D		;ASCII #$8D = carriage return, ASCII #$AD = dash 

HOSTILE.TEXT.BLOCK 	.AZ -/You can't talk your way out of this!/,#$8D,#$8D,/Seize him!/
SLEEPING.TEXT.BLOCK .AZ -/Zzzzzzzzz/
VOICE.HEADER.TEXT 	.AZ -/Voice Mode:/	;ASCII text string
NORMAL.MODE.TEXT 	.AZ -/*normal* /			;ASCII text string
YELL.MODE.TEXT 		.AZ -/ *yell*  /			;ASCII text string
WHISPER.MODE.TEXT 	.AZ -/*whisper*/			;ASCII text string	

;INTERNAL SEARCHES
TEXT_BLOCK.DFL		.AZ -/$!DFL/			;ASCII text string
TEXT_BLOCK.BYE		.AZ -/$!BYE/			;ASCII text string
TEXT_BLOCK.END		.AZ -/$!%END/			;ASCII text string
TEXT_BLOCK.INTRO	.AZ -/$I/			;ASCII text string
TEXT_BLOCK.ANY_KEY1	.AZ -#$8D, #$8D,/<Press any key>/ ;ASCII text string	
TEXT_BLOCK.ANY_KEY2	.AZ -/<any key>/ ;ASCII text string	




; HOSTILE.TEXT.BLOCK
; SLEEPING.TEXT.BLOCK
; VOICE.HEADER.TEXT
; NORMAL.MODE.TEXT
; YELL.MODE.TEXT
; WHISPER.MODE.TEXT

 .BS $1

@END



UPDATE.VOICE_MODE.DISPLAY
@START

;SAVE TEXT WINDOW CURSOR POSITION
;(Note: because we have to modify HTAB/VTAB to draw the cursor prompt and display the voice mode text)
		LDA HTAB	
		STA CURSOR.POSITION.SAVED+$0
		LDA VTAB
		STA CURSOR.POSITION.SAVED+$1
	JSR	UPDATE.CHAR.POS
	
;PRINT "*<VOICE MODE>*"
		LDA #$1D			;$19full / $1E test
		STA HTAB	
		LDA #$16
		STA VTAB
		JSR	UPDATE.CHAR.POS


	LDA NTALK.VOICE_MODE
	BEQ .NORMAL.MODE
	CMP #$01
	BEQ .YELL.MODE
	;DEFAULT TO WHISPER MODE


.WHISPER.MODE
		LDA #WHISPER.MODE.TEXT					
		STA STRING
		
		LDA /WHISPER.MODE.TEXT						
		STA STRING+$1
	JSR PRINT.STR
	JMP .EXIT

.NORMAL.MODE
		LDA #NORMAL.MODE.TEXT					
		STA STRING
		
		LDA /NORMAL.MODE.TEXT						
		STA STRING+$1
	JSR PRINT.STR
	JMP .EXIT
	
.YELL.MODE
		LDA #YELL.MODE.TEXT					
		STA STRING
		
		LDA /YELL.MODE.TEXT						
		STA STRING+$1
	JSR PRINT.STR
	;**FALLS THROUGH**
	
.EXIT

;RESTORE TEXT WINDOW CURSOR POSITION
		LDA CURSOR.POSITION.SAVED+$0
		STA HTAB	
		LDA CURSOR.POSITION.SAVED+$1
		STA VTAB
	JSR	UPDATE.CHAR.POS
	
	
	
	RTS
@END

		
	.NO $AA00
	
		.EP ;end patch code section
	

;LOAD.TALK.DATA
@START
;=======this is the routine for loading the talk data file before ZX7 compression was implimented====

; ;LOAD FILE "DATA.TLK.L1"

; ;set command type (READ | WRITE | SEEK)
; ; cmd_seek.current_drive 	.EQ $90
; ; cmd_read.current_drive  	.EQ $91
; ; cmd_write.current_drive 	.EQ $92
; ;			
; ; cmd_read.drive1			.EQ $1
; ; cmd_read.drive2		  	.EQ $81
; ; cmd_write.drive1			.EQ $2
; ; cmd_write.drive2			.EQ $82
	; lda #cmd_read.drive2			
	; sta parm.reqcmd
	
; ;set destination memory address
	; lda #NPC.TALK.ARRAY
	; sta parm.ldrlo
	; lda /NPC.TALK.ARRAY		
	; sta parm.ldrhi

; ;set read length (bytes)
	; lda #$FF				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	; sta parm.sizelo	
	; lda #$FF				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	; sta parm.sizehi
	
; ;set filename to read from	
	; lda CURRENT.LOCATION.TLK_DATA		;load LO address
	; sta parm.namlo
	; lda CURRENT.LOCATION.TLK_DATA+$1	;load HO address
	; sta parm.namhi 
	
		; LDA #$00	;PARM: $00=main, $01=aux
	; JSR PRODOS.IO

	; RTS
@END



;LOAD ZX7 DRIVER (unpacker)
	.NO $BF00, $00
		
	;***Warning: must reassemble ZX7.BIN to load it into any location other than $BF00 (see c:\my_code\compression\npc.speech.text\zx7.6502.s)
	
	.BI C:\MY_CODE\COMPRESSION\NPC.SPEECH.TEXT\zx7.bf00.bin,BIN	;unpack driver for ZX7 compression using the version built to run at $BF00
									