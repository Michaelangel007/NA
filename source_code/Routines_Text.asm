; ============================================
; Copyright (C) 2016-2018. 6502 Workshop, LLC
; contact: mark@6502workshop.com
; ============================================


;************************INCLUDE FILE*****************************
;(do not assemble stand-alone.  )

;NOTE: Part of this include file is not setup in a normal format. 
;Below the line labeled COPY/PASE the code for the routines is 
;commented out. It is designed to be a copy/paste resource, as I
;am not sure yet what I want text routines I want in an include 
;file. 
;
;The other benifit of this include file is it's variable 
;definition section. Note that the real comments in the 
;copy/paste section have two ;; I commented out the code so it 
;doesn't take up space when assembling

SCREEN_BUILDER
@START
;PARAMETERS: SCREEN_BUILDER.COMMAND_TABLE.POINTER(2)*, SCREEN_BUILDER.HTAB.LEFT
;
;*zpage variable. Set to the memroy address of the command table to be processed by this subroutine. 
;
;

;=====================SUBROUTINE DOCUMENTATION===========================================================================
;
;A single command table can have multiple command records. The command table can be up to !256 bytes long.
;
;The first byte ($00) of the command table must be the command table size (bytes). The first command record
;starts at the second byte ($01)
;
;-datagram of command record(s)
; command_code*, command parameter field (ASSC)**, [command parameter field], .., command parameter field (PRC)**, [command parameter field], ..
;
;*command_code: (LO nibble = print_routine_code (PRC), HO nibble = ascii_string_source_code (ASSC)
;**the command paramter fields can be different for each command. They depend on the  
;value that the ascii_string_source_code and the print_command source code is set to (see list below).
;The interpreter processes the ASCC parms first and then the command parms. 
;
;
;
;
;-ascii_string_source_code list (HO nibble)
;$0: local ascii string stored in command fields. $00 stop value. 
	 ;Command parm fields: the command fields are simply the ascii string to print with $00 at the end. 
;
;$1: external ascii table/variable stored in memory, outside of the command table. 
	 ;Command parm fields:  LO address, HO address
;
;$2: numerical variable, sourced from lookup table*
	 ;Command parm fields:  LO address (lookup table), HO address (lookup table), LO address (table index), HO address (table index),  
;
;$3: corner case. parse variable: On $00 print local ascii table, on !=$00 print variable
	 ;Command parm fields:  LO address (variable), HO address (variable), LO address (external ascii table), HO address (external ascii table)
	 ;example: (.CORNER_CASE.LHAND) .CT.MODIFIED_DAMAGE
	 
;*A lookup table with two byte records which contain the memory address of external ascii tables. For example,
;see .CT.RACE on the stats0 screen. (SWAP.ROUTINES.Combat.stats_summary.ASM)
;
;-print_routine_code list (LO nibble)
; $0: PRINT.STR (used in screen2, combat stats)
	;Command parm fields: none
;
; $1: PRINT.STR.APPEND_RIGHT
	;Command parm fields: none
;
; $2: PRINT.TEXT.WINDOW
;	Command parm fields: none
;
; $3: PRINT.TEXT.WINDOW.HEX8
;	Command parm fields: none
;
; $4: PRINT.HEX8.LJ
;	Command parm fields: none
;
; $5: PRINT.HEX8.RJ
;	Command parm fields: none
;
; $6: PRINT.TEXT.WINDOW.BCD8
;	Command parm fields: none
;
; $7: PRINT.HEX16.LJ
;	Command parm fields: none
;
; $8: PRINT.HEX16.RJ
;	Command parm fields: none
;
; $9-$B: future routines
;
; $C: *toggle text window <CR> parm (off by default)
;	Command parm fields: none
;
; $D: **update cursor position: custom
	;Command parm fields: HTAB, VTAB
;
; $E: **update cursor position: reset HTAB to SCREEN_BUILDER.HTAB.LEFT and increment VTAB+1 
	;Command parm fields: none
	;
; $F: **update cursor position: reset HTAB to SCREEN_BUILDER.HTAB.LEFT and increment VTAB+2
	;Command parm fields: none
;
;*the PRINT.HEX routines support a carry flag parameter that triggers a <CR> to be printed after the
;number but it requires the text window function to be active. When this print_routine_code is used, 
;the HO nibble (ascii_string_source_code) must be set to $0. 
;
;**Uses UPDATE.CHAR.POS*. When this print_routine_code is used, the HO nibble (ascii_string_source_code) 
;must be set to $0. 
;
;=============================================================================================================================

;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA
	


				
.INIT
	LDY #$00 ;set (SCREEN_BUILDER.COMMAND_TABLE.POINTER) index to byte $0
	STY SCREEN_BUILDER.CR.PARM ;($00 = off | $01 = on)

	
	LDA (SCREEN_BUILDER.COMMAND_TABLE.POINTER),Y
	STA SCREEN_BUILDER.COMMAND_TABLE.SIZE

.PROCESS.COMMAND_RECORDS	
	INY ;advance to 1st byte of 1st command record
	

			
	LDA (SCREEN_BUILDER.COMMAND_TABLE.POINTER),Y ;load command code
	STA SCREEN_BUILDER.COMMAND_CODE	 ;save command code
	;exit test: is end of command table reached?
	CPY SCREEN_BUILDER.COMMAND_TABLE.SIZE
	BEQ .EXIT_STEP ;if yes, branch
	

	;LOOKS FOR PRCs that have no ASSC
	CMP #$0C
	BEQ .COMMAND.TOGGLE.CR_FLAG
	CMP #$0D
	BEQ .COMMAND.UPDATE_CURSOR_POSITION
	CMP #$0E
	BEQ .COMMAND.NEXT_LINE.ENTRANCE ;HTAB reset, VTAB+1
	CMP #$0F
	BEQ .COMMAND.NEXT_LINE2 ;HTAB reset, VTAB+2

	
.PARSE.ASSC	
@START
	INY ;advance to ASSC command parm fields byte $00
	;get ascii_string_source_code (HO nibble)
	LSR ;move HO nibble to LO nibble
	LSR
	LSR
	LSR

		
	;CMP #$00
	BEQ .ASCII_SOURCE.COMMAND_FIELDS
	CMP #$01
	BEQ .ASCII_SOURCE.EXTERNAL_TABLE_OR_VARIABLE
	CMP #$02
	BEQ .ASCII_SOURCE.EXTERNAL_LOOKUP_TABLE_STEP
	CMP #$03
	BEQ .ASCII_SOURCE.CORNER_CASE.BOOLEAN_FLAG_STEP
	
.ERROR_TRAP
;.PROCESS.COMMAND_RECORDS reports unexpected value in command code HO nibble (ascii_string_source_code)
;or in LO nibble (print_routine_code). This error trap has two entry paths. If it triggers, setup 
;.PARSE.PRC it's own error trap to tell which nibble has unexpected value. 
	JSR PREP.BRK
	BRK
	
.ASCII_SOURCE.EXTERNAL_LOOKUP_TABLE_STEP
	JMP .ASCII_SOURCE.EXTERNAL_LOOKUP_TABLE

.ASCII_SOURCE.CORNER_CASE.BOOLEAN_FLAG_STEP
	JMP .ASCII_SOURCE.CORNER_CASE.BOOLEAN_FLAG
	
.EXIT_STEP
	JMP .EXIT


;OTHER COMMANDS TRIGGERED BY PRC ;not related to ASSC. These routines are placed here for location convenience to avoid a branch step. 
@START

.COMMAND.TOGGLE.CR_FLAG
@START
	LDA SCREEN_BUILDER.CR.PARM ;($00 = off | $01 = on)
	BNE .SET.TO.ZERO
;.SET.TO.ONE
	LDA #$01
	JMP .UPDATE.CR_FLAG
.SET.TO.ZERO
	LDA #$00

.UPDATE.CR_FLAG
	STA SCREEN_BUILDER.CR.PARM ;($00 = off | $01 = on)

	JMP .PROCESS.COMMAND_RECORDS
	
	
@END
	
.COMMAND.UPDATE_CURSOR_POSITION
@START
	INY ;advance to PRC command parm fields byte $00

		;YREG: command parm fields byte $00
	LDA (SCREEN_BUILDER.COMMAND_TABLE.POINTER),Y ;load LO byte of memory address to the ascii table
	STA HTAB
	INY ;advance to command parm fields byte $01
	LDA (SCREEN_BUILDER.COMMAND_TABLE.POINTER),Y ;load LO byte of memory address to the ascii table
	STA VTAB
	
	;Y-REG = last byte of command field parms 
	JMP .COMMAND.UPDATE_CURSOR_POSITION.JMP
@END

					
.COMMAND.NEXT_LINE2  ;HTAB reset, VTAB+2
@START				
		INC VTAB
.COMMAND.NEXT_LINE.ENTRANCE ;HTAB reset, VTAB+1
		INC VTAB
		LDA SCREEN_BUILDER.HTAB.LEFT	
		STA HTAB

.COMMAND.UPDATE_CURSOR_POSITION.JMP	
	JSR	UPDATE.CHAR.POS
		
		


			
	JMP .PROCESS.COMMAND_RECORDS
@END


@END
	

.ASSC.ROUTINES
@START	
.ASCII_SOURCE.COMMAND_FIELDS
@START
	;YREG: command parm fields byte $00
	
	;set string pointer to the memory address of the ascii table stored in the command parm fields
	STY TEMP
	LDA SCREEN_BUILDER.COMMAND_TABLE.POINTER+$0
	CLC
	ADC TEMP
	STA STRING+$0
	LDA SCREEN_BUILDER.COMMAND_TABLE.POINTER+$1
	ADC #$00 ;16-bit add
	STA STRING+$1
	
.LOOP.SEEK.COMMAND_FIELDS.END
	LDA (SCREEN_BUILDER.COMMAND_TABLE.POINTER),Y ;load LO byte of memory address to the ascii table
	BEQ .LOOP.SEEK.COMMAND_FIELDS.END.DONE ;has stop value ($00) been found?
	INY ;advance to next local ascii table byte
	BNE .LOOP.SEEK.COMMAND_FIELDS.END
.LOOP.SEEK.COMMAND_FIELDS.END.DONE



	;Y-REG: set to the last byte of the PRC commmand parm fields
	JMP .PARSE.PRC	
@END	

.ASCII_SOURCE.EXTERNAL_TABLE_OR_VARIABLE
@START
;SET STRING POINTER
;(reads the first two bytes of the command parms, which either contain the memory address of
;of the external ascii table or the memory address of the variable.
;This routine processes both cases because it won't hurt anything; the print routine will either use STRING(2)
;or BIN(2), not both.

	;CORNER CASE: is PRC set to PRINT.STR.APPEND_RIGHT
	LDA SCREEN_BUILDER.COMMAND_CODE	 ;save command code
	;get print_routine_code (LO nibble)
	AND #$F ;mask-out HO nibble	
	CMP #$01 
	BEQ .SET_VARS.PRINT.STR.APPEND_RIGHT
	
.SET.STRING.POINTER	
	;YREG: command parm fields byte $00
		
		;YREG: already set
		;SCREEN_BUILDER.COMMAND_TABLE.POINTER(2): already set
	JSR SCREEN_BUILDER.GET.COMMAND_TABLE.PUT.STRING ;LDA (SCREEN_BUILDER.COMMAND_TABLE.POINTER), STA STRING(2)
		;RETURN: STRING(2), YREG+1
	
;LOAD VARIABLE

	
;SAVE REGISTERS
	TYA 
	PHA ;save command table index
	
	LDY #$00	;byte0 of (STRING)
	LDA (STRING), Y ;load byte0 of variable. It will either be the 8-bit value to print or the LO byte of the 16-bit variable to print.
	STA BIN+$0 ;this works for 8-bit BCD variables as well because BIN+$0 is loaded into the ACC for all print routines and the 8-bit BCD print routine expects the value parm to be in the ACC
	INY ;advance to byte1 of (STRING)
	LDA (STRING), Y ;load byte1 of variable. It will either be the 8-bit value to print or the LO byte of the 16-bit variable to print.
	STA BIN+$1
	
	
;RESTORE REGISTERS	
	PLA ;restore command table index
	TAY
	
	;Y-REG: set to the last byte of the PRC commmand parm fields
	JMP .PARSE.PRC

.SET_VARS.PRINT.STR.APPEND_RIGHT
@START
	;YREG: command parm fields byte $00
	LDA (SCREEN_BUILDER.COMMAND_TABLE.POINTER),Y ;load LO byte of memory address to the ascii table
	STA PRINT.STR.APPEND_RIGHT.HTAB
	INY ;advance to command parm fields byte $01
	LDA (SCREEN_BUILDER.COMMAND_TABLE.POINTER),Y ;load LO byte of memory address to the ascii table
	STA PRINT.STR.APPEND_RIGHT.VTAB	
	INY ;advance to command parm fields byte $02
	LDA (SCREEN_BUILDER.COMMAND_TABLE.POINTER),Y ;load LO byte of memory address to the ascii table
	STA PRINT.STR.APPEND_RIGHT.WINDOW_SIZE
	INY ;advance to command parm fields byte $03
	
	JMP .SET.STRING.POINTER ;uses the code in the main routine to set STRING(2) to the external ascii table

@END
	
@END	
	
	

.ASCII_SOURCE.CORNER_CASE.BOOLEAN_FLAG ;***OUT OF ORDER**** PRC $03
@START

;SET VARIABLE POINTER

		;YREG: already set
		;SCREEN_BUILDER.COMMAND_TABLE.POINTER(2): already set
	JSR SCREEN_BUILDER.GET.COMMAND_TABLE.PUT.LOOKUP_OR_VARIABLE.POINTER
		;RETURN: (SCREEN_BUILDER.VARIABLE_POINTER | SCREEN_BUILDER.LOOKUP_TABLE.POINTER) , YREG+1
		
;SET EXTERNAL ASCII TABLE POINTER	
	INY ;advance to command parm fields byte $02
		;YREG: already set
		;SCREEN_BUILDER.COMMAND_TABLE.POINTER(2): already set
	JSR SCREEN_BUILDER.GET.COMMAND_TABLE.PUT.STRING ;LDA (SCREEN_BUILDER.COMMAND_TABLE.POINTER), STA STRING(2)
	;RETURN: STRING(2), YREG+1


;PRINT EXTENRAL ASCII TABLE OR VARIABLE	
	;save registers
	TYA 
	PHA ;save command table index


	LDY #$00 ;set SCREEN_BUILDER.LOOKUP_TABLE.POINTER index to byte $00
	LDA (SCREEN_BUILDER.VARIABLE_POINTER), Y ;load LO byte of memory address of the lookup table index
	BNE .NOT_ZERO
	
		;STRING (2): already set above
	JSR PRINT.STR
	JMP .PRINT.DONE
	
.NOT_ZERO
;LOAD VARIABLE (to print)
	LDY #$00	;byte0 of (STRING)
	LDA (SCREEN_BUILDER.VARIABLE_POINTER), Y ;load byte0 of variable. It will either be the 8-bit value to print or the LO byte of the 16-bit variable to print.
	
		;ACC: already set above
	JSR PRINT.HEX8.RJ
	
	;**FALLS THROUGH*8
	
.PRINT.DONE	
	;restore registers	
	PLA ;restore command table index
	TAY	
	
	
	JMP .PROCESS.COMMAND_RECORDS  ;skip .PARSE.PRC because the print routines are called directly in the code above
@END


	
.ASCII_SOURCE.EXTERNAL_LOOKUP_TABLE ;***OUT OF ORDER**** PRC $02
@START

;SET LOOKUP TABLE POINTER

		;YREG: already set
		;SCREEN_BUILDER.COMMAND_TABLE.POINTER(2): already set
	JSR SCREEN_BUILDER.GET.COMMAND_TABLE.PUT.LOOKUP_OR_VARIABLE.POINTER
		;RETURN: (SCREEN_BUILDER.VARIABLE_POINTER | SCREEN_BUILDER.LOOKUP_TABLE.POINTER) , YREG+1
			

;SET LOOKUP TABLE INDEX	
	INY ;advance to command parm fields byte $02
		;YREG: already set
		;SCREEN_BUILDER.COMMAND_TABLE.POINTER(2): already set
	JSR SCREEN_BUILDER.GET.COMMAND_TABLE.PUT.STRING ;LDA (SCREEN_BUILDER.COMMAND_TABLE.POINTER), STA STRING(2)
		;RETURN: STRING(2), YREG+1
		

;SET LOOKUP TABLE INDEX

	;save registers
	TYA 
	PHA ;save command table index
	
	LDY #$00
	LDA (STRING), Y ;load index to lookup table
	ASL ;X2 (the lookup table has 2 byte records)
	TAY
				
;SET EXTERNAL ASCII TABLE POINTER					
	LDA (SCREEN_BUILDER.LOOKUP_TABLE.POINTER), Y ;load LO byte of memory address of external ascii table
	STA STRING+$0
	INY ;advance to next byte of lookup table
	LDA (SCREEN_BUILDER.LOOKUP_TABLE.POINTER), Y ;load HO byte of memory address of external ascii table
	STA STRING+$1

	
	;restore registers	
	PLA ;restore command table index
	TAY	
				

	;Y-REG: set to the last byte of the PRC commmand parm fields
	
	;**FALLS THROUGH** (to .PARSE.PRC)

@END


	
@END
	
	
@END

	
	
.PARSE.PRC ;(parse print routine code)
@START
;PARAMETERS: STRING(2), [BIN(2)], [HTAB], [VTAB]
	
;(this routine executes the print routine specified by the PRC by using the PRC as the index to an indirect jump table)	

			LDA STRING+$0
			STA TWF.STRING+$0
			LDA STRING+$1
			STA TWF.STRING+$1

	;get indirect jump table index
	LDA SCREEN_BUILDER.COMMAND_CODE	 ;save command code
	;get print_routine_code (LO nibble)
	AND #$F ;mask-out HO nibble	
	;calculate index (based on the PRC)
	ASL ;X2 (because the jump table records are two bytes long)
	TAX 
	
	;set jump destination address
	lda SCREEN_BUILDER.JMP_TABLE,X
	sta JMP.DESTINATION.ADDR+$0
	lda SCREEN_BUILDER.JMP_TABLE+$1,X
	sta JMP.DESTINATION.ADDR+$1
	
	;set jump return address
	LDA /.PROCESS.COMMAND_RECORDS-1 ;-1 because RTS returns the address on the stack +1, which is normally 1 byte after the last opcode in a JSR
	PHA
	LDA #.PROCESS.COMMAND_RECORDS-1
	PHA

	; LDA /.JMP.RETURN-1 ;-1 because RTS returns the address on the stack +1, which is normally 1 byte after the last opcode in a JSR
	; PHA
	; LDA #.JMP.RETURN-1
	; PHA
	
	
	
			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; LDA #SCREEN_BUILDER.JMP_TABLE
			; STA $BF00
			; LDA /SCREEN_BUILDER.JMP_TABLE
			; STA $BF01
			; LDA SCREEN_BUILDER.COMMAND_CODE	 ;save command code
			; STA $BF02
			; LDA #$AA
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP

			
			
	
		;set paramters of print routine
		LDA SCREEN_BUILDER.CR.PARM ;($00 = off | $01 = on)
		BNE	.CR_FLAG.SET	
		CLC ;(CLC don't print CR | SEC = print CR to text window )
		JMP .CR_FLAG.DONE
.CR_FLAG.SET
		SEC ;(CLC don't print CR | SEC = print CR to text window )
.CR_FLAG.DONE			
		;BIN(2), STRING(2), expected as parameter by some print routines, are set by .PARSE.ASSC	
		LDA BIN+$0	;sets the ACC parm for the print routines which expect it as a parameter. 
					;Note: ****Also used to store 8-bit BCD values to print
	JMP (JMP.DESTINATION.ADDR)	
		;SEEDED RTS TO: .PROCESS.COMMAND_RECORDS-1 ;-1 because RTS returns the address on the stack +1, which is normally 1 byte after the last opcode in a JSR
;.JMP.RETURN
	

	


	;JMP .PROCESS.COMMAND_RECORDS

			
@END


	
	
.EXIT
;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX
	
	RTS
	
	
;LOCAL VARIABLES	
SCREEN_BUILDER.JMP_TABLE
@START
;PRC $0
		.DA	#PRINT.STR
		.DA	/PRINT.STR
		
;PRC $1
		.DA	#PRINT.STR.APPEND_RIGHT
		.DA	/PRINT.STR.APPEND_RIGHT

;PRC $2
		.DA	#PRINT.TEXT.WINDOW
		.DA	/PRINT.TEXT.WINDOW
		
;PRC $3
		.DA	#PRINT.TEXT.WINDOW.HEX8
		.DA	/PRINT.TEXT.WINDOW.HEX8

;PRC $4
		.DA	#PRINT.HEX8.LJ
		.DA	/PRINT.HEX8.LJ

;PRC $5
		.DA	#PRINT.HEX8.RJ
		.DA	/PRINT.HEX8.RJ

;PRC $6
		.DA	#PRINT.TEXT.WINDOW.BCD8
		.DA	/PRINT.TEXT.WINDOW.BCD8
		
;PRC $7
		.DA	#PRINT.HEX16.LJ
		.DA	/PRINT.HEX16.LJ	

;PRC $8
		.DA	#PRINT.HEX16.RJ
		.DA	/PRINT.HEX16.RJ	
 
@END
 

;LOCAL SUBROUTINES
SCREEN_BUILDER.GET.COMMAND_TABLE.PUT.STRING
@START
;PARAMETERS: Y-REG = SCREEN_BUILDER.COMMAND_TABLE index, SCREEN_BUILDER.COMMAND_TABLE.POINTER(2)
;RETURN: STRING(2), YREG+1


	LDA (SCREEN_BUILDER.COMMAND_TABLE.POINTER), Y ;load LO byte of memory address of the lookup table index
	STA STRING+$0
	INY ;advance to next byte of command parm fields 
	LDA (SCREEN_BUILDER.COMMAND_TABLE.POINTER), Y ;load HO byte of memory address of the lookup table index
	STA STRING+$1

	RTS
@END

SCREEN_BUILDER.GET.COMMAND_TABLE.PUT.LOOKUP_OR_VARIABLE.POINTER ;LDA (SCREEN_BUILDER.COMMAND_TABLE.POINTER),Y, STA SCREEN_BUILDER.VARIABLE_POINTER+$0 ;.EQ SCREEN_BUILDER.LOOKUP_TABLE.POINTER
@START
;PARAMETERS: Y-REG = SCREEN_BUILDER.COMMAND_TABLE index, SCREEN_BUILDER.COMMAND_TABLE.POINTER(2)
;RETURN: (SCREEN_BUILDER.VARIABLE_POINTER | SCREEN_BUILDER.LOOKUP_TABLE.POINTER) , YREG+1


	LDA (SCREEN_BUILDER.COMMAND_TABLE.POINTER),Y ;load LO byte of memory address to the Boolean variable
	STA SCREEN_BUILDER.VARIABLE_POINTER+$0 ;.EQ SCREEN_BUILDER.LOOKUP_TABLE.POINTER
	INY ;advance to next byte of command parm fields 
	LDA (SCREEN_BUILDER.COMMAND_TABLE.POINTER),Y ;load LO byte of memory address to the Boolean variable
	STA SCREEN_BUILDER.VARIABLE_POINTER+$1 ;.EQ SCREEN_BUILDER.LOOKUP_TABLE.POINTER
	
	
	RTS
	
@END

@END
 
;=========================================================================

;UPDATE.CHAR.POS ;============MOVES CURSOR TO HTAB/VTAB POSITION ON TEXT SCREEN

COUT ;========OUTPUT 1 CHARACTER TO DEFAULT OUTPUT DEVICE=====
@START
;PARAMETERS: ACC (ascii value of char to output), [COUT_CHAR_TYPE ;($00 = normal, $7F = inverse)], [HRCG.PAGES ($01 = hi-res page 1, $02 = hi-res page 2, $03 = both pages)]
;RETURN: character output to video screen
	

;SAVE REGISTERS (HRCG, accessed via COUT.ADDRESS, uses X-REG)
;(see below )
			
			
;SAVE PARAMETERS			
	
	STA SAVED.ACC.LOCAL		;save parameter, output character
				
;CALCULATE CHARACTER SHAPE ADDRESS
	AND #$7F		;CLEAR HI BIT  
	STA HRCG.SHAPE.OFFSET
	LDA #$00
	STA HRCG.SHAPE.OFFSET+$1

;CALC1	
	SEC
	LDA HRCG.SHAPE.OFFSET
	SBC #$20
	STA HRCG.SHAPE.OFFSET		; CHAR > 96
	ASL	HRCG.SHAPE.OFFSET		; *2 = CHAR < 192
	ASL	HRCG.SHAPE.OFFSET		; *4 < 384
	ROL HRCG.SHAPE.OFFSET+$1	; transfer overflow bit from carry flag to HO byte	
	ASL HRCG.SHAPE.OFFSET		; *8 < 768
	ROL HRCG.SHAPE.OFFSET+$1	; transfer overflow bit from carry flag to HO byte


;SAVE REGISTERS (HRCG, accessed via COUT.ADDRESS, uses X-REG)
	TXA
	PHA

	
;LOAD CHARACTER SHAPE TABLE
			
		;AUX MEMORY	-> MAIN MEMORY 
		LDA #HRCG.AUX.START	;SET START ADDRESS
		CLC 
		ADC HRCG.SHAPE.OFFSET
		STA AUX_MOVE.START
		LDA /HRCG.AUX.START
		ADC HRCG.SHAPE.OFFSET+$1	;16-BIT ADD
		STA AUX_MOVE.START+$1
		
		LDA AUX_MOVE.START		;SET END ADDRESS
		CLC
		ADC #HRCG.SHAPE.SIZE
		STA AUX_MOVE.END
		LDA AUX_MOVE.START+$1
		ADC #$00				;16-BIT ADD
		STA AUX_MOVE.END+$1
		
		LDA #HRCG.BUFFER		;SET DESTINATION ADDRESS
		STA AUX_MOVE.DEST
		LDA /HRCG.BUFFER
		STA AUX_MOVE.DEST+$1
		CLC                ;CLEAR CARRY FLAG DESGINATD MOVE FROM AUX -> MAIN MEMORY 
		JSR AUX_MOVE



			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP

			; STA $C008 ;enable main zero-page & main BSR 
			; LDX STACK_POINTER.SAVED	;restore stack pointer to X-REG
			; TXS ;transfer X-REG to stack pointer

; .TEMP
			; LDA TEMP



			
	JSR GET.BSR_BANK.STATUS

.SET.AUX_MAIN.ZPAGE_BSR
		LDA $C016		;MAIN/AUX ZPAGE & BSR) (bit7 = 1: AUX, bit7=0 MAIN)
		STA AUX_MAIN.ZPAGE_BSR.STATE ;(bit7 = 1: AUX, bit7=0 MAIN)
		BPL .SET.AUX_MAIN.ZPAGE_BSR.DONE ;if bit7=1 then AUX zpage and bsr was enabled. 		
		;aux enabled. set main zpage & bsr
		;(this is needed because COUT.ADDRESS (in ROM) uses zpage variables to track HTAB, VTAB, hi-res line number etc.)		
		STA $C008 ;enable main zero-page & main BSR 
			TSX							;transfer AUX stack pointer to X-REG
			STX STACK_POINTER.SAVED_AUX	;save AUX stack pointer
			
			LDX STACK_POINTER.SAVED	;restore stack pointer to X-REG
			TXS ;transfer X-REG to stack pointer		
.SET.AUX_MAIN.ZPAGE_BSR.DONE


	
		;ENABLE ROM ROUTINES
		LDA $C082			;READ ENABLE ROM, DISABLE WRITE ON BSM (NORMAL STATE).

					

			;REQUIRED FOR BEFORE BRK BECAUSE APPLE MONITOR IS A ROM ROUTINE
			LDA SAVED.ACC.LOCAL		;restore output character

		JSR COUT.ADDRESS	;note: HRCG uses X-REG
		

				
.RESTORE.AUX_MAIN.ZPAGE_BSR
		LDA AUX_MAIN.ZPAGE_BSR.STATE	;(bit7 = 1: AUX, bit7=0 MAIN)
		BPL .RESTORE.AUX_MAIN.ZPAGE_BSR.DONE ;if bit7=1 then AUX zpage and bsr was enabled when the COUT wrapper was called. 	
		;set AUX zpage & bsr (restore to original state)
			TSX			;transfer stack pointer to X-REG
			STX STACK_POINTER.SAVED	;save main stack pointer
			
			LDX STACK_POINTER.SAVED_AUX	;restore AUX stack pointer to X-REG
			TXS ;transfer X-REG to stack pointer					
		STA $C009 ;enable aux zero-page & aux BSR 	
.RESTORE.AUX_MAIN.ZPAGE_BSR.DONE

	
	JSR RESTORE.BSR_BANK.STATUS


			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK2
			; CMP #$01
			; BNE .TEMP
			; jsr keyin
			; ; LDA #$AA
			; ; LDX COUT_CHAR_TYPE ;($00 = normal, $7F = inverse)
		
			; ; JSR PREP.BRK
			; ; BRK
; .TEMP
			; LDA TEMP

			
.EXIT	

	
	;reset char type to normal
	LDA #$00
	STA COUT_CHAR_TYPE ;($00 = normal, $7F = inverse)

	LDA #$03 ;set both pages
	STA HRCG.PAGES ;($01 = hi-res page 1, $02 = hi-res page 2, $03 = both pages)



			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP2
			; pla
			; tax
			; pla
			; tay
			; LDA #$AA
			; JSR full.BRK
			; BRK
; .TEMP2
			; LDA TEMP
			

;RESTORE REGISTERS
	PLA
	TAX

				
	RTS

@END

;COUT.SINGLE_PAGE ;====Uses COUT to print a character to a single hi-res page====  (turns out not to be needed because the function it performed is build directly into routines line PRINT.STR and PRINT.BCD_PACKED)
@START
; ;PARAMETERS: ;ACC = ascii character to print, HRCG.PAGES.SAVED (($01 = hi-res page 1, $02 = hi-res page 2, $03 = both pages))
; ;ENTRANCE: from a routine that sets HRCG.PAGES.SAVED from HRCG.PAGES
; ;RETURN: text character printed to the video screen on the specified hi-res page

; ;=====================SUBROUTINE DOCUMENTATION===========================================================================
; ;
; ;This routine is used by other subroutines that need to call COUT and only have the output
; ;printed to a single hi-res page. Accordingly, this routine assumes that the HRCG.PAGES
; ;parameter has already been set. This routine just restores the parm since the parm is set
; ;back to the default value of $03 (both pages) by COUT after each call. 
; ;
; ;In contrast COUT.BACKGROUND is designed for direct use anytime a text character needs to be printed 
; ;to the background page. 
; ;
; ;=============================================================================================================================

		; ;ACC = ascii character to print
		; PHA ;save ascii character to print
		; ;set hi-res page parameter 
		; LDA HRCG.PAGES.SAVED ;($01 = hi-res page 1, $02 = hi-res page 2, $03 = both pages)
		; STA HRCG.PAGES
		; PLA ;restore ascii character to print
	; JSR COUT

	; RTS
@END

COUT.SPACE_CHAR	
@START
	;erase animation cursor 
	LDA #$A0						;ASCII CODE: space
	JSR COUT
	

	RTS
@END


COUT.BACKGROUND ;====Uses COUT to print a single character to the background page====
@START
;PARAMETERS: ;ACC = ascii character to print
;ENTRANCE: direct
;RETURN: text character printed to the video screen on the background hi-res page

;=====================SUBROUTINE DOCUMENTATION===========================================================================
;
;This routine is designed for direct use anytime a text character needs to be printed 
;to the background page. 
;
;See COUT.SINGLE_PAGE documentation for an explanation of the difference between this routine and COUT.SINGLE_PAGE
;
;=============================================================================================================================


		PHA ;save ascii character to print
		;set hi-res page parameter 
		LDA PAGE.BACKGROUND ;save ascii character to print
		STA HRCG.PAGES
		PLA ;restore ascii character to print
	JSR COUT
	

@END


HCG.ON	;=====TURN ON HI-RES CHARACTER GENERATION======
@START
;ENABLE ROM ROUTINES
	LDA $C082				;READ ENABLE ROM, DISABLE WRITE ON BSM (NORMAL STATE).
							;REQUIRED FOR BEFORE BRK BECAUSE APPLE MONITOR IS A ROM ROUTINE

	JSR HCG.ON.ADDRESS

	;ENABLE.BS_RAM BEFORE EXIT
	LDA $C083				;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK 1ST)
	LDA $C083
	
	RTS
@END

HCG.OFF			;======EMERGENCY BREAK, NEVER USE=====
@START

;=====================SUBROUTINE DOCUMENTATION====================================
;
;After calling HCG.ON to connect the custom HRCG driver to COUT, 
;the apple monitor will have problems displaying the contents
;of memory addresses (at endless stream of zeros is one of the
;results I've seen)
;
;This routine is designed to be called just before a BRK. This
;routine disconnects the custom HRCG driver and returns the COUT
;vector to the normal COUT address. 
;
;NOTE: HCG.ON ends with a JMP VECT ($03EA), which in turn does a JMP somewhere else
;and I think is needed to implement the change to the vector values in $36. 
;However, after HCG.ON is executed, $03EA becomes $60 (RTS). 
;For awhile I was doing the JMP VECT at the end of this routine
;and it was working just fine, in AppleWIN, presumably because
;this routine is called via a JSR so the RTS was just returning to 
;the command after teh JSR HCG.OFF. However, for some reason on the
;physical Apple IIe, doing the JMP VECT at the end of this 
;routine was causing a screen of garbage text with
;no access to the monitor after a BRK. So, I stopped doing
;the JMP VECT and instead just do an RTS at the end of this routine. That fixed the isssue
;on the physical IIe and it also works in AppleWIN. 
;**UPDATE**: Actually, this fix seems to result in it working on the
;physical IIe some of the time, but not all of the time. haven't
;been able to find a pattern.
;
;
;
;=================================================================================

; ;SAVE REGISTERS	
	;***NOT NEEDED, NO REGISTERES ARE MODIFIED, AND FOR SOME
	;REASON PUSHING AND PULLING THE REGISTERES AT THE START/END
	;OF THIS ROUTINE CAUSES STRANGE UNDESIRED RESULTS.
	;
	;NOTE: since the register values stay intact it looks like
	;using LDA to flip soft switches (i.e. LDA TEXT, LDA $C082) 
	;doesn't actually change the contents of the accumulator. 
	
	
;RETURN OUTPUT HOOK TO NORMAL SETTINGS	
	LDA #$F0		;PRODUCES LOW BYTE
	STA CSW
	LDA #$FD		;PRODUCES HIGH BYTES
	STA CSW+$1
	;JMP VECT

; ;RESTORE REGISTERS
	;***DON'T DO IT. DON'T RAISE THAT IRON! SEE NOTES UNDER "SAVE REGISTERS" ABOVE
	
	RTS
@END
			
PRINT.BCD_PACKED ;======OUTPUT 4 BCD DIGITS (2 BYTES PACKED) TO VIDEO DISPLAY (LEADING ZEROS SUPRESSED)======		 
@START 			 ;======(!9999 largest # supported)
;PARAMETERS: BCD (2), Carry Flag (CLC = left justify | SEC = right justify), [HRCG.PAGES ($01 = hi-res page 1, $02 = hi-res page 2, $03 = both pages)], [COUT_CHAR_TYPE ($00 = normal, $7F = inverse)]
;RETURN: SCREEN OUTPUT

;=====================SUBROUTINE DOCUMENTATION====================================
;
;Prints 4 BCD digits and supresses leading 0s. For example, if 
;the digits 1-4 are 0907, the output to the video screen will be
;907
;
;The digits output are organized as LO and HO, just like most
;things in assembler. 
;
;BCD		= 3rd/4th digits 
;BCD+$1		= 1st/2nd digits
;
;=================================================================================


	; ;**DRIVER TEMPLATE**	
	; LDA #$12
	; STA BCD				;2ND # TO DISPLAY ONSCREEN
	; LDA #$74				;1ST # TO DISPLAY ONSCREEN
	; STA BCD+$1
	;	SEC ;(CLC = left justify | SEC = right justify)
	; JSR PRINT.BCD_PACKED

	
.SAVE.PARAMETERS
	;Carry Flag (CLC = left justify | SEC = right justify)
	BCS .SET.RIGHT_JUSTIFY
	LDA #$00 ;set left justify mode
	JMP .SAVE.PARAMETERS.DONE
.SET.RIGHT_JUSTIFY
	LDA #$01 ;set right justify mode
	;**FALLS THROUGH**
.SAVE.PARAMETERS.DONE
	STA PRINT.BCD_PACKED.PARM.MODE ;($00 = left justify | $01 = right justify)

	;**FALLS THROUGH**
	
.INIT
	LDA HRCG.PAGES ;($01 = hi-res page 1, $02 = hi-res page 2, $03 = both pages)
	STA HRCG.PAGES.SAVED ;save the value at routine entrance because it is reset by COUT to $03 on exit

	LDA COUT_CHAR_TYPE ;($00 = normal, $7F = inverse)
	STA COUT_CHAR_TYPE.SAVED
	
	

	
	
;SAVE REGISTERS
	TXA
	PHA
	
			
	JSR CONVERT.BCD_PACKED.ASCII
			
			;**OPT** Memory. The code below this point can probably be rolled up into a loop. 
			;Load RESULT using an index. X-REG looks like it's 1 digit off from being aligned for this purpose, 
			;but maybe that can be changed, or otherwise use Y-REG and X-REG. If using Y-REG, add it to the save/restore registers sections. 
			
	LDX #$04
.DIGIT1
	LDA RESULT+$3
	CMP #$B0
	BNE .PRINT.DIGIT1
	DEX
	LDA PRINT.BCD_PACKED.PARM.MODE ;($00 = left justify | $01 = right justify)
	BEQ .DIGIT2
	LDA #$A0 ;ASCII - 'space'
	;JMP .DIGIT2
	
.PRINT.DIGIT1

		;ACC = ascii character to print			
	JSR .CALL.COUT

	
.DIGIT2
	LDA RESULT+$2
	CMP #$B0
	BNE .PRINT.DIGIT2
	CPX #$03
	BNE .PRINT.DIGIT2
	DEX
	LDA PRINT.BCD_PACKED.PARM.MODE ;($00 = left justify | $01 = right justify)
	BEQ .DIGIT3	
	LDA #$A0 ;ASCII - 'space'
	;JMP .DIGIT3
	
.PRINT.DIGIT2
		;ACC = ascii character to print			
	JSR .CALL.COUT


				
.DIGIT3
	LDA RESULT+$1
	CMP #$B0
	BNE .PRINT.DIGIT3
	CPX #$02
	BNE .PRINT.DIGIT3
	DEX
	LDA PRINT.BCD_PACKED.PARM.MODE ;($00 = left justify | $01 = right justify)
	BEQ .DIGIT4
	LDA #$A0 ;ASCII - 'space'
	;JMP .DIGIT4
	
.PRINT.DIGIT3
		;ACC = ascii character to print			
	JSR .CALL.COUT
	


.DIGIT4
	LDA RESULT+$0
		;ACC = ascii character to print			
	JSR .CALL.COUT


		
.EXIT

;RESTORE REGISTERS
	PLA
	TAX
	
	RTS

.CALL.COUT	
@START
		PHA ;save char to print

		;set hi-res page parameter 
		LDA HRCG.PAGES.SAVED ;($01 = hi-res page 1, $02 = hi-res page 2, $03 = both pages)
		STA HRCG.PAGES
		
		LDA COUT_CHAR_TYPE.SAVED ;($00 = normal, $7F = inverse)
		STA COUT_CHAR_TYPE 
	
		PLA ;restore char to print
	JSR COUT
	
	RTS
@END

	
@END

PRINT.BCD_PACKED.STR.BACKGROUND ;**OPT** Memory. If only inventory routines use this then it can be moved to the inventory file
@START
;PARAMETERS: [COUT_CHAR_TYPE ($00 = normal, $7F = inverse)]
;ENTRANCE: DIRECT
;RETURN: text on video screen

;NOTE: if the optional COUT_CHAR_TYPE parm is set, PRINT.BCD_PACKED will call the correct COUT entrance for it. 

		LDA PAGE.BACKGROUND
		STA HRCG.PAGES ;($01 = hi-res page 1, $02 = hi-res page 2, $03 = both pages)	
		SEC ;(CLC = left justify | SEC = right justify)		
	JSR PRINT.BCD_PACKED

.EXIT
	
	RTS
	
@END

PRINT_FIXED.BCD_PACKED ;======OUTPUT 4 BCD DIGITS (2 BYTES PACKED) TO VIDEO DISPLAY (LEADING ZEROS KEPT)======
@START
;PARAMETERS: BCD (2)
;RETURN: SCREEN OUTPUT

;=====================SUBROUTINE DOCUMENTATION====================================
;
;Prints 4 BCD digits and do not supresses leading 0s. For example, if 
;the digits 1-4 are 0907, the output to the video screen will be
;0907
;
;The digits output are organized as LO and HO, just like most
;things in assembler. 
;
;BCD		= 3rd/4th digits 
;BCD+$1		= 1st/2nd digits
;
;=================================================================================



	; ;**DRIVER TEMPLATE**	
	; LDA #$12
	; STA BCD				;2ND # TO DISPLAY ONSCREEN
	; LDA #$74				;1ST # TO DISPLAY ONSCREEN
	; STA BCD+$1
	; ;
	; JSR PRINT_FIXED.BCD_PACKED
	
			
	JSR CONVERT.BCD_PACKED.ASCII
	
.DIGIT1
	LDA RESULT+$3				
	JSR COUT
	
.DIGIT2
	LDA RESULT+$2
	JSR COUT
				
.DIGIT3
	LDA RESULT+$1
	JSR COUT	


.DIGIT4
	LDA RESULT+$0
	JSR COUT

	RTS
	
	
@END


;PRINT.HEX_VALUE  ;(convert hex value to ascii and print onscreen)
@START
; ;PARAMETERS: ACC (hex value to print)

	; STA TEMP ;save hex value to print
	
; ;SAVE X/Y REGISTERS
	; TXA
	; PHA
	; TYA
	; PHA
	
	; LDA TEMP
	; PHA ;transfer hex value to print to stack
	
; ;SAVE CURSOR POSITION
	; LDA HTAB	
	; STA CURSOR.POSITION.SAVED+$0
	; LDA VTAB
	; STA CURSOR.POSITION.SAVED+$1

	; LDA #$1E 
	; STA HTAB
	; LDA #$5 
	; STA VTAB
	; JSR	UPDATE.CHAR.POS				

; ;PRINT HEX VALUE

	; PLA ;restore hex value to print
	; JSR CONVERT.HEX_TO_ASCII
		; LDA RESULT+$1
	; JSR COUT
		; LDA RESULT+$0
	; JSR COUT
		; LDA #$A0 ;space
	; JSR COUT
				
	
; ;RESTORE CURSOR POSITION
		; LDA CURSOR.POSITION.SAVED+$0
		; STA HTAB	
		; LDA CURSOR.POSITION.SAVED+$1
		; STA VTAB
	; JSR	UPDATE.CHAR.POS	

; ;RESTORE X/Y REGISTERS
	; PLA
	; TAY
	; PLA
	; TAX
	
	; RTS
@END

PRINT.STR ; ======OUTPUT ASCII STRING TO VIDEO DISPLAY, NO CARRIAGE RETURN=====
@START
;PARAMETERS: ACC = ($7F = inverse characters, != $7F = normal characters), STRING (HO/LO ADDRESS OF ASCII STRING TO PRINT), PRINT.STR.MODE ($00 = normal | >=$01 = wait/pause functionality enabled), [HRCG.PAGES ($01 = hi-res page 1, $02 = hi-res page 2, $03 = both pages)], [COUT_CHAR_TYPE ;($00 = normal, $7F = inverse)]
;ENTRANCE: DIRECT, PRINT.STR.BACKGROUND
;RETURN VALUE: NONE

;NOTE: ascii string must use the LO values on the ascii chart
;in order to work with this subtroutine. SBASM uses the LO values
;when the .AZ directive is used.
;
;--LO Value Quick Reference--
;Carriage Return: $08

; ;*****DRIVER TEMPLATE (normal characters)****
; ;PRINT "GOLD"
	; JMP .TEXT1.PRINT
	;
; .TEXT1 .AZ /Gold/			;ASCII text string
; .TEXT1.PRINT
		; LDA #TEXT1 					
		; STA STRING
		;
		; LDA /TEXT1
		; STA STRING+$1						
	; JSR PRINT.STR

; ;*****DRIVER TEMPLATE (inverse characters)****
; ;PRINT "GOLD"
	; JMP .TEXT1.PRINT
	;
; .TEXT1 .AZ /Gold/			;ASCII text string
; .TEXT1.PRINT
		; LDA #TEXT1 					
		; STA STRING
		;
		; LDA /TEXT1
		; STA STRING+$1				
		;LDA #$7F 	;($7F = inverse, != $7F = normal)
	; JSR PRINT.STR
	
;PROCESS PARMS
	
	;ACC = ($7F = inverse, != $7F = normal)
	STA TEMP

	LDA HRCG.PAGES ;($01 = hi-res page 1, $02 = hi-res page 2, $03 = both pages)]
	STA HRCG.PAGES.SAVED ;save the value at routine entrance because it is reset by COUT to $03 on exit

	LDA COUT_CHAR_TYPE ;($00 = normal, $7F = inverse)
	STA COUT_CHAR_TYPE.SAVED
		
	;**FALLS THROUGH**
				
.SAVE.REGISTERS
	TYA
	PHA
							
	LDY #$0				;init string index

	
.LOOP
	LDA (STRING),Y
	BEQ .EXIT			;if string stop value found, exit. SBASM adds a $00 stop value to its .AZ ascii hex tables. 

;	CLC
;	ADC #$80			;convert from LO hex ascii value (used by SBASM) to HI hex ascii value (required by COUT ROM routine)

	PHA ;save char to print
	LDA TEMP ;($7F = inverse, != $7F = normal)
	CMP #$7F
	BNE .CALL.COUT.ENTRANCE	
	STA COUT_CHAR_TYPE ;($00 = normal, $7F = inverse)


.CALL.COUT.ENTRANCE
	
.KEYPRESS.CHECK
	LDA PLAYER.MAP.LOCATION_TYPE		;load map type code
	CMP #MAP.TYPE.COMBAT		;is map type = building?
	BNE .CALL.COUT
	
	LDA PRINT.STR.MODE	;($00 = normal | >=$01 = wait/pause functionality enabled)
	BEQ .CALL.COUT

;SCOLL SPEED ADJUSTMENT	
		LDA GAME.SCROLL_SPEED
	JSR WAIT.LOOP

;CHECK FOR USER SCROLL PAUSE REQUEST				
	LDA KB_BUFFER
    BPL .CALL.COUT ;branch if no keypress				
 	CMP #SCROLL_SPPED.PAUSE_KEY ;ASCII = space (was space bar key pressed?)
	BEQ .PAUSE.SCROLL_TEXT
	CMP #$AB			;(+) INCREASE SCROLL SPEED
	BEQ .INCREASE.SCROLL_SPEED	
	CMP #$BD			;(=) INCREASE SCROLL SPEED
	BEQ .INCREASE.SCROLL_SPEED
	CMP #$AD			;(-) INCREASE SCROLL SPEED
	BEQ .DECREASE.SCROLL_SPEED
	CMP #$DF			;(_) INCREASE SCROLL SPEED
	BEQ .DECREASE.SCROLL_SPEED
	JMP .CLEAR.KEYPRESS
	
.PAUSE.SCROLL_TEXT
	STA KB_BUFFER_ACK               ;CLR LAST KEY			
	JSR KEYIN ;insert pause
	JMP .CLEAR.KEYPRESS
	
.INCREASE.SCROLL_SPEED
	JSR COMMAND.INCREASE.SCROLL_SPEED
	JMP .CLEAR.KEYPRESS

.DECREASE.SCROLL_SPEED
	JSR COMMAND.DECREASE.SCROLL_SPEED
	
	;**FALLS THROUGH**
	
.CLEAR.KEYPRESS
    STA KB_BUFFER_ACK               ;CLR LAST KEY
	
		;JSR KEYIN
.CALL.COUT
		;set hi-res page parameter 
		LDA HRCG.PAGES.SAVED ;($01 = hi-res page 1, $02 = hi-res page 2, $03 = both pages)
		STA HRCG.PAGES
		
		LDA COUT_CHAR_TYPE.SAVED ;($00 = normal, $7F = inverse)
		STA COUT_CHAR_TYPE 
	
		PLA ;restore char to print
	JSR COUT
			
	INY					;increment string index
	BNE .LOOP

.EXIT 

; ;RESET PARMS TO DEFAULT VALUES
	; LDA #$03 ;set both pages
	; STA HRCG.PAGES ;($01 = hi-res page 1, $02 = hi-res page 2, $03 = both pages)
 

;RESTORE REGISTERS
	PLA
	TAY
  

			
	RTS

@END

	
PRINT.STR.BACKGROUND ;**OPT** Memory. If only inventory routines use this then it can be moved to the inventory file
@START
;PARAMETERS: [COUT_CHAR_TYPE ($00 = normal, $7F = inverse)]
;ENTRANCE: DIRECT
;RETURN: text on video screen

		LDA PAGE.BACKGROUND
		STA HRCG.PAGES ;($01 = hi-res page 1, $02 = hi-res page 2, $03 = both pages)		
	JSR PRINT.STR
	
	RTS
	
@END

PRINT.STR.APPEND_RIGHT
@START
;PARAMETERS: STRING(2), PRINT.STR.APPEND_RIGHT.HTAB, PRINT.STR.APPEND_RIGHT.VTAB, PRINT.STR.APPEND_RIGHT.WINDOW_SIZE
;ENTRANCE: DIRECT
;RETURN:

;SAVE REGISTERS
	TYA
	PHA

.INIT
		LDA PRINT.STR.APPEND_RIGHT.HTAB ;left edge screen byte of the text window to print appended text
		STA HTAB	
		LDA PRINT.STR.APPEND_RIGHT.VTAB ;VTAB of the text window to print appended text
		STA VTAB
	JSR	UPDATE.CHAR.POS
	

.PREPARE.APPENDED.TEXT
	;get size of text block to print
	LDY #$FF ;start at $FF because loop counter increment is at top of loop. 
.GET.SIZE.LOOP
	INY ;increment text block index & loop counter
	LDA (STRING),Y
	BNE .GET.SIZE.LOOP
		;INY convert Y-REG value to a quantity (i.e. with floor value of 1 instead of 0) isn't necessary because the byte 
		;that triggers exit is the stop value, which is +1 byte more than the true end of the text block for video screen print purposes.
	
	;Y-REG = text block size
	STY TEMP ;save text block size

;CREATE APPENDED TEXT BLOCK
;(copy the text block submitted as a parameter until the size of the
;the text window is reached, appending the rest. If the size of the text block is reached
;first then stop copying at that point.)	
	LDY #$00
.APPEND.LOOP ;copy until append point or end of text blocked reached, whichever comes first
	LDA (STRING),Y
	STA PRINT.STR.APPEND_RIGHT.BUFFER,Y
	INY
	CPY #PRINT.STR.APPEND_RIGHT.BUFFER.SIZE-1
	BEQ .ERROR.APPEND_BUFFER.OVERFLOW
	CPY TEMP ;has size of text block been reached? 
	BEQ .APPEND.LOOP.DONE ;branch if yes
	CPY PRINT.STR.APPEND_RIGHT.WINDOW_SIZE ;has size of window been reached? 
	BNE .APPEND.LOOP ;branch if no
.APPEND.LOOP.DONE
	;add stop value to end of appended text string
	LDA #$00
	STA PRINT.STR.APPEND_RIGHT.BUFFER,Y
	
	;connect string pointer to buffer
	LDA #PRINT.STR.APPEND_RIGHT.BUFFER
	STA STRING+$0

	LDA /PRINT.STR.APPEND_RIGHT.BUFFER
	STA STRING+$1	
	
	
;PRINT TEXT BLOCK
	
		;STRING+$0: set above
		;STRING+$1: set above
	JSR PRINT.STR

.EXIT
;RESTORE REGISTERS
	PLA
	TAY
	
	RTS
	
;; .ERROR.TEXT_BLOCK_OVERFLOW
;PRINT.TW_BOTTOM reports that text block to print is larger than the size of the bottom text window?

	; JSR PREP.BRK
	; BRK

.ERROR.APPEND_BUFFER.OVERFLOW
;PRINT.STR.APPEND_RIGHT reports that the text block and/or window size passed as parms are larger than the append buffer size.
	JSR PREP.BRK
	BRK
	

@END
	
PRINT.STR.CENTERED
@START
;PARAMETERS: STRING(2), PRINT.STR.CENTERED.HTAB, PRINT.STR.CENTERED.VTAB, PRINT.STR.CENTERED.WINDOW_SIZE
;ENTRANCE: DIRECT
;RETURN:

;SAVE REGISTERS
	TYA
	PHA

;SAVE PARAMETERS
	LDA STRING+$0
	PHA ;push text block pointer LO byte from stack

	LDA STRING+$1
	PHA ;push text block pointer HO byte from stack

.INIT
		LDA PRINT.STR.CENTERED.HTAB ;left edge screen byte of the centered space.
		STA HTAB	
		LDA PRINT.STR.CENTERED.VTAB ;VTAB of the centered space. 
		STA VTAB
	JSR	UPDATE.CHAR.POS

	LDA PRINT.STR.CENTERED.WINDOW_SIZE ;# of characters in the centered space
	CLC
	ADC #$01
	STA PRINT.STR.CENTERED.WINDOW_SIZE.PLUS_1


;!!WARNING!! this clear window routine may not be appropriate
;for all scenarios where this subroutine could be used. A default resetting parm may need to be added
;to skip the clear. The clear is needed when centered text is printed
;to the same space over and over without any other routines clearning it. However
;the clear routine below is sized for the main screen bottom text window. 

	;clear text window
		LDA #GLOBAL.TEXT_BLOCK.CLEAR_BOTTOM.TW		
		STA STRING
		
		LDA /GLOBAL.TEXT_BLOCK.CLEAR_BOTTOM.TW
		STA STRING+$1						
	JSR PRINT.STR
	
	;restore parameters
	PLA ;pull text block HO byte from stack
	STA STRING+$1
	PLA ;pull text block LO byte from stack
	STA STRING+$0
		

.CALCULATE.PRINT_HTAB
	;get size of text block to print
	LDY #$FF ;start at $FF because loop counter increment is at top of loop. 
.GET.SIZE.LOOP
	INY ;increment text block index & loop counter
	LDA (STRING),Y
	BNE .GET.SIZE.LOOP
		;INY convert Y-REG value to a quantity (i.e. with floor value of 1 instead of 0) isn't necessary because the byte 
		;that triggers exit is the stop value, which is +1 byte more than the true end of the text block for video screen print purposes.
	
	;Y-REG = text block size
	STY TEMP ;save text block size
	
	;error check
	;(is text block to print larger than the size of the bottom text window?)
	CPY PRINT.STR.CENTERED.WINDOW_SIZE.PLUS_1
	BCS .ERROR.TEXT_BLOCK_OVERFLOW
	
	;calculate start position adjustment
	;(formula: start position adjustment = (text window size - text block size)/2). Round down (i.e. ignore remainder value) 
	LDA PRINT.STR.CENTERED.WINDOW_SIZE
	SEC
	SBC TEMP ;text block size
	LSR ;X2 (rounded down)
	;calculate start position			
	CLC
	ADC PRINT.STR.CENTERED.HTAB	
		
		;ACC = print start position
		STA HTAB	
		LDA PRINT.STR.CENTERED.VTAB
		STA VTAB
	JSR	UPDATE.CHAR.POS
	
	
	;print text block
	
		;STRING+$0: set above
		;STRING+$1: set above
	JSR PRINT.STR

.EXIT
;RESTORE REGISTERS
	PLA
	TAY
	
	RTS
	
.ERROR.TEXT_BLOCK_OVERFLOW
;PRINT.STR.CENTERED reports that text block to print is larger than the size of the text window

	JSR PREP.BRK
	BRK
	

@END


STOP	;========CUSTOM EXIT GRAPHICS ROUTINE=======
@START
	
;PAUSE UNTIL A KEY IS PRESSED	
.KEYIN  

	LDA $C000
	BPL .KEYIN
	STA $C010               ;CLR LAST KEY


;RETURN TEXT WINDOW TO NORMAL
	LDA #$00		;(!0-39) (X) SET UPPER LEFT COLUMN (TEXT WRITES TO THIS COLUMN)
	STA TW1			;($1-28)
	LDA #$28		;(!1-40) (X) SET WIDTH (I.E. $03 = 3 COLUMNS WIDE, 0 DOESN'T COUNT)
	STA TW2			;($1-28)
	LDA #$00		;(!0-23) (Y) TOP ROW 
	STA TW3			;($1-18)
	LDA #$18		;(!0-23) (Y) BOTTOM ROW (TEXT STOPS 1 ROW ABOVE THIS VALUE)
	STA TW4			;($1-18)	(bottom row - top row = total # of rows)

	
	
	LDA TEXT
	LDA PAGE1
	JMP $FF69

	BRK

@END


UPDATE.CHAR.POS ;============MOVES CURSOR TO HTAB/VTAB POSITION ON TEXT SCREEN
@START

;**OPT** Memory. Review the ROM code at UPDATE.CHAR.POS.ADDRESS (copy it to main memory). See if it takes less memory to put this code
;inline that it does to do all this switching back and forth of memory. 
;
;inline code would only update the HTAB, VTAB and line position zpage variables on the active zpage, but that might be okay since combat
;(the only programmed planned to run from AUX BSR) would init these variables at the start and the calling routine would init them again.
;i.e. there is no continity of the text cursor position expected between the combat module and other programs....except inventory, but that
;might work out too if it uses aux zpage for everything. 


;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA
	
	JSR GET.BSR_BANK.STATUS

.SET.AUX_MAIN.ZPAGE_BSR
		LDA $C016		;MAIN/AUX ZPAGE & BSR) (bit7 = 1: AUX, bit7=0 MAIN)
		STA AUX_MAIN.ZPAGE_BSR.STATE ;(bit7 = 1: AUX, bit7=0 MAIN)
		BPL .SET.AUX_MAIN.ZPAGE_BSR.DONE ;if bit7=1 then AUX zpage and bsr was enabled. 		
		;aux enabled. set main zpage & bsr
		;(this is needed because COUT.ADDRESS (in ROM) uses zpage variables to track HTAB, VTAB, hi-res line number etc.)		
			LDX HTAB
			LDY VTAB
		STA $C008 ;enable main zero-page & main BSR 
			STX HTAB
			STY VTAB
			
			TSX							;transfer AUX stack pointer to X-REG
			STX STACK_POINTER.SAVED_AUX	;save AUX stack pointer
			
			LDX STACK_POINTER.SAVED	;restore stack pointer to X-REG
			TXS ;transfer X-REG to stack pointer		
.SET.AUX_MAIN.ZPAGE_BSR.DONE

	
;ENABLE ROM ROUTINES
	LDA $C082				;READ ENABLE ROM, DISABLE WRITE ON BSM (NORMAL STATE).
							;REQUIRED FOR BEFORE BRK BECAUSE APPLE MONITOR IS A ROM ROUTINE
 
	JSR UPDATE.CHAR.POS.ADDRESS

; ;ENABLE.BS_RAM BEFORE EXIT
	; LDA $C083				;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK 1ST)
	; LDA $C083

.RESTORE.AUX_MAIN.ZPAGE_BSR
		LDA AUX_MAIN.ZPAGE_BSR.STATE	;(bit7 = 1: AUX, bit7=0 MAIN)
		BPL .RESTORE.AUX_MAIN.ZPAGE_BSR.DONE ;if bit7=1 then AUX zpage and bsr was enabled when the COUT wrapper was called. 	
		;set AUX zpage & bsr (restore to original state)
			TSX			;transfer stack pointer to X-REG
			STX STACK_POINTER.SAVED	;save main stack pointer
			
			LDX STACK_POINTER.SAVED_AUX	;restore AUX stack pointer to X-REG
			TXS ;transfer X-REG to stack pointer	
			LDX HTAB
			LDY VTAB
		STA $C009 ;enable aux zero-page & aux BSR 	
			STX HTAB
			STY VTAB
.RESTORE.AUX_MAIN.ZPAGE_BSR.DONE

	
	JSR RESTORE.BSR_BANK.STATUS

.EXIT
;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX
	
	RTS

@END

;MAIN SCREEN TEXT WINDOW WRAPPERS
@START

PRINT.TW_BOTTOM
@START
;PARAMETERS: STRING(2)
;ENTRANCE: direct
;RETURN: updated video screen

		LDA #TWS.BOTTOM_WINDOW.PRINT.HTAB
		STA PRINT.STR.CENTERED.HTAB
		
		LDA #TWS.BOTTOM_WINDOW.PRINT.VTAB	
		STA PRINT.STR.CENTERED.VTAB
		
		LDA #TWS.BOTTOM_WINDOW.SIZE
		STA PRINT.STR.CENTERED.WINDOW_SIZE
		;STRING(2): pointer to text string to print (already set)
	JSR PRINT.STR.CENTERED

	RTS





	
@END

INIT.TW_BOTTOM
@START
	LDA TW.BOTTOM_WINDOW.INIT_CODE	;($00 = none | $01 = Ignite Torch | $02 = Open door | $03 = Unlock Door | $04 = Operate lever | $05 = Invalid Command)
	BEQ .EXIT
	CMP #$01
	BEQ .IGNITE_TORCH
	CMP #$02
	BEQ .OPEN_DOOR
	CMP #$03
	BEQ .UNLOCK_DOOR
	CMP #$04
	BEQ .OPERATE_LEVER
	
.IGNITE_TORCH	
	;.update.bottom.text_window
		LDA #GLOBAL.TEXT_BLOCK.COMMAND_IGNITE_TORCH			
		STA STRING+$0		
		LDA /GLOBAL.TEXT_BLOCK.COMMAND_IGNITE_TORCH
		STA STRING+$1
	JSR PRINT.TW_BOTTOM
	JMP .EXIT

.OPEN_DOOR	
	;.update.bottom.text_window
		LDA #GLOBAL.TEXT_BLOCK.COMMAND_OPEN					
		STA STRING+$0		
		LDA /GLOBAL.TEXT_BLOCK.COMMAND_OPEN
		STA STRING+$1
	JSR PRINT.TW_BOTTOM
	JMP .EXIT


.UNLOCK_DOOR	
	;.update.bottom.text_window
		LDA #GLOBAL.TEXT_BLOCK.COMMAND_UNLOCK			
		STA STRING+$0		
		LDA /GLOBAL.TEXT_BLOCK.COMMAND_UNLOCK
		STA STRING+$1
	JSR PRINT.TW_BOTTOM
	JMP .EXIT
	
.OPERATE_LEVER	
	;.update.bottom.text_window
		LDA #GLOBAL.TEXT_BLOCK.COMMAND_OPERATE		
		STA STRING+$0		
		LDA /GLOBAL.TEXT_BLOCK.COMMAND_OPERATE
		STA STRING+$1
	JSR PRINT.TW_BOTTOM

	;**FALLS THROUGH**
	
.EXIT

	;reset init code
	LDA #$00
	STA TW.BOTTOM_WINDOW.INIT_CODE	;($00 = none | $01 = Ignite Torch | $02 = Open door | $03 = Unlock Door | $04 = Operate lever | $05 = Invalid Command)
	
	
	RTS
	
@END



@END


;PRINT NUMBER FUNCTIONS ;suppresses leading zeros. left or right justify options.
@START

;****NOTE: the print <CR> paramters controlled by the carry flag (CLC|SEC) only
;works if the text window function is active. The core routine to print the number does
;it's print via COUT without going through the text window's print wrapper. Thus, if printing
;a number within a text window, you must make sure HTAB/VTAB is positioned so that the number 
;will fit.
;
;If printing numbers via the text window print wrapper is desired perhaps these routines could be
;expanded but make sure to update SCREEN_BUILDER which uses these functions.
;

PRINT.HEX8.RJ ;suppresses leading zeros. right justify. 
@START
;PARAMETERS: ACC = 8-bit hex number

		;convert to 16-bit hex number. 
		
		;ACC = 8-bit hex number
		STA BIN+$0
		LDA #$00
		STA BIN+$1
	
		LDA #$01 ;($00 = left justify | $01 = right justify)  ;**OPT** Memory. This parm is already set in PRINT.HEX16.RJ. Check the left version for the same issue
		CLC ;(CLC = don't print CR | SEC = print CR to text window)
	JSR PRINT.HEX16.RJ
	
	RTS
@END

PRINT.HEX8.LJ ;suppresses leading zeros. left justify. 
@START
;PARAMETERS: ACC = 8-bit hex number, Carry Flag (CLC = don't print CR | SEC = print CR to text window)

		;convert to 16-bit hex number. 
		
		;ACC = 8-bit hex number
		STA BIN+$0
		LDA #$00
		STA BIN+$1

	
		LDA #$00 ;($00 = left justify | $01 = right justify)
	JSR PRINT.HEX16.LJ
				
	RTS
@END
	
PRINT.HEX16.RJ ;suppresses leading zeros. right justify. 
@START
;PARAMETERS: Carry Flag (CLC = don't print CR | SEC = print CR to text window), BIN(2) = 16bit hex number

		LDA #$01 ;($00 = left justify | $01 = right justify)
	JSR PRINT.HEX16
	
	RTS
@END

			
PRINT.HEX16.LJ ;suppresses leading zeros. left justify. 
@START
;PARAMETERS: Carry Flag (CLC = don't print CR | SEC = print CR to text window), BIN(2) = 16bit hex number

		LDA #$00 ;($00 = left justify | $01 = right justify)
	JSR PRINT.HEX16
	
	RTS
@END

PRINT.HEX16 ;suppresses leading zeros. left or right justify options. 
@START
;PARAMETERS: ACC ($00 = left justify | $01 = right justify), Carry Flag (CLC = don't print CR | SEC = print CR to text window), BIN(2) = 16bit hex number
;ENTRANCE: direct
;RETURN: 3-6* unpacked BCD digits printed to text window
;

;=====================SUBROUTINE DOCUMENTATION===========================================================================
;
;converts 16-bit HEX->BCD24->ASCII then prints
;
;
;---orginal note---
;*currently routine is setup to print 4 digits, but
;# of digits desired could be passed via a parameters, and then
;parse the parm and JMP after each BCD subsection
;or setup this routine to print all 6 digits but don't print
;digits that are $0. However, then the question becomes whether to
;left-justify or right-justify the number 
;
;=============================================================================================================================
;
;

		
;SAVE PARAMETERES

	;PARM: ACC ($00 = left justify | $01 = right justify)
	STA PRINT.HEX16.PARM.MODE
	
	BCS .SET.PRINT_CR.PARM
;.SET.DONT_PRINT_CR.PARM
	LDA #$00
	PHA ;save print_cr flag to stack ($00 = don't print CR | $01 = print CR to text window)
	JMP .SAVE.PARMS.DONE
.SET.PRINT_CR.PARM
	LDA #$01
	PHA ;save print_cr flag to stack ($00 = don't print CR | $01 = print CR to text window)
.SAVE.PARMS.DONE

			
		;parm = BIN(2)
	JSR CONVERT.HEX.16_TO_BCD.24
		;RETURN: BCD(3)
	
.SET.JUSTIFICATION.PARM ;for PRINT.BCD_PACKED
	LDA PRINT.HEX16.PARM.MODE ;($00 = left justify | $01 = right justify)
	BNE .RIGHT_JUSTIFY
		CLC ;(CLC = left justify | SEC = right justify)
		JMP .EXECUTE.PRINT
.RIGHT_JUSTIFY		
		SEC ;(CLC = left justify | SEC = right justify)
		;**FALLS THROUGH**
			
.EXECUTE.PRINT		
		;BCD(2): use value returned above		
	JSR PRINT.BCD_PACKED	


	
				; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
				; JSR KEYIN
				; LDA #$CD
				; JSR COUT
				
				; JSR KEYIN
				; lda #$aa
				; pla
				; tax
				; jsr prep.brk
				; brk
				
; .TEMP
			; LDA TEMP
			
; ;BCD+$1
		; LDA BCD+$1
	; JSR CONVERT.HEX_TO_ASCII	
		 ; ;RETURN = RESULT+$0 = LO nibble, RESULT+$1 = HO nibble

		; LDA RESULT+$1
	; JSR PRINT.TEXT.WINDOW.SINGLE.CHAR
		; ;JSR COUT

		; LDA RESULT+$0
	; JSR PRINT.TEXT.WINDOW.SINGLE.CHAR
		; ;JSR COUT

; ;BCD+$0
		; LDA BCD+$0
	; JSR CONVERT.HEX_TO_ASCII	
		 ; ;RETURN = RESULT+$0 = LO nibble, RESULT+$1 = HO nibble

		; LDA RESULT+$1
	; JSR PRINT.TEXT.WINDOW.SINGLE.CHAR
		; ;JSR COUT
		
		; LDA RESULT+$0
	; JSR PRINT.TEXT.WINDOW.SINGLE.CHAR
		; ;JSR COUT

;PRINT_CR?
	PLA ;pull print_cr flag to stack ($00 = don't print CR | $01 = print CR to text window)
	BEQ .EXIT
	
	;print carriage return at the end of the text line. 
	JSR PRINT.TEXT.WINDOW.CR	
	;**OPT** Memory. A version of PRINT.TEXT.WINDOW.SINGLE.CHAR could be created that adds a <CR> at the end. It could be placed in main memory with the rest of the text window functions or in the combat module only 

.EXIT


			
	RTS

@END
@END
	
;TEXT WINDOW FUNCTIONS
@START
;**OPT** Memory. I think I moved these here assuming I'd use them them in inventory. If that's true, it might still not make sense to move them since combat and NPC talk used them and
;if combat ends up in memory permentantly, keeping a local copy in combat vs. main game engine memory doesn't gain anything. 

;base print routine
PRINT.TEXT.WINDOW
@START
;PARAMETERS: various (see DRIVER TEMPLATE (INIT)), PRINT.STR.MODE ($00 = normal | >=$01 = wait/pause functionality enabled)

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
; .TEXT1 .AZ -/Test/,#$8D,#$8D			;ASCII text string
; .TEXT1.PRINT
		; LDA #.TEXT1 					
		; STA TWF.STRING
;		
		; LDA /.TEXT1
		; STA TWF.STRING+$1						
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
	;INC TWF.RIGHT_SBYTE		;add 1 so that it can be used as a stop value after an index increment
	
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
			; LDA TWF.RIGHT_SBYTE
			; CMP HTAB
			; BEQ .NEXT_LINE
			
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
		;parm PRINT.STR.MODE set as parm to this routine
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
;COPY TEXT CHARACTER DATA

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
	LDA LINE.START.COPY_TO			;advance line to next row of characters
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

PRINT.TEXT.WINDOW.SINGLE.CHAR ;print single char without <CR>
@START
;PARAMETERS: ACC = ASCII code of character to print
;ENTRANCE: direct
;RETURN: printed character within text window
	

		;ACC = ASCII code of character to print
		STA .TEXT_BLOCK.SPACE+$0
		
		LDA #.TEXT_BLOCK.SPACE
		STA TWF.STRING+$0
		
		LDA /.TEXT_BLOCK.SPACE
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW	

.EXIT

	RTS
	
.TEXT_BLOCK.SPACE .HS 00.00

@END

;print wrapper routines
PRINT.TEXT.WINDOW.CR
@START
;PARAMETERS: none
;ENTRANCE: direct
;RETURN: carriage return within text window
	
		LDA #.TEXT_BLOCK.CR	
		STA TWF.STRING
		
		LDA /.TEXT_BLOCK.CR
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW	

		; LDA #$8D
	; JSR COUT
	
.EXIT

	RTS
	
.TEXT_BLOCK.CR .AZ -#$8D

@END

PRINT.TEXT.WINDOW.SPACE
@START
;PARAMETERS: none
;ENTRANCE: direct
;RETURN: printed space within text window
	
		LDA #.TEXT_BLOCK.SPACE
		STA TWF.STRING
		
		LDA /.TEXT_BLOCK.SPACE
		STA TWF.STRING+$1						
	JSR PRINT.TEXT.WINDOW	

		; LDA #$8D
	; JSR COUT
	
.EXIT

	RTS
	
.TEXT_BLOCK.SPACE .AZ -#$A0

@END

PRINT.TEXT.WINDOW.BCD8 ;suppresses leading zeros. right justify. Appended Carriage Return
@START
;PARAMETERS: ACC = 8bit BCD number
;ENTRANCE: direct
;RETURN: 1-2 unpacked BCD digit printed to text window
;

;=====================SUBROUTINE DOCUMENTATION===========================================================================
;
;converts BCD8->ASCII then prints
;
;=============================================================================================================================

		;parm: ACC = 8bit BCD number
	JSR CONVERT.HEX_TO_ASCII	
		;RETURN VALUES: RESULT(2)
	
.CHAR1
		LDA RESULT+$1
		CMP #$B0
		BNE .CHAR1_PRINT
		LDA #$A0 ;ASCII - 'space'
.CHAR1_PRINT		
	JSR PRINT.TEXT.WINDOW.SINGLE.CHAR
		LDA RESULT+$0
.CHAR2
	JSR PRINT.TEXT.WINDOW.SINGLE.CHAR

			
.EXIT
			
	;print carriage return at the end of the text line. 
	JSR PRINT.TEXT.WINDOW.CR	
	;**OPT** Memory. A version of PRINT.TEXT.WINDOW.SINGLE.CHAR could be created that adds a <CR> at the end. It could be placed in main memory with the rest of the text window functions or in the combat module only 

	RTS	
	
	
@END

PRINT.TEXT.WINDOW.HEX8 ;***future**, for printing an 8-bit hex variable with no left/right justification
@START
;PARAMETERS: ACC = 8-BIT HEX NUMBER
;RETURN: 8-bit number printed to video screen 

		;ACC = parm: 8-BIT HEX NUMBER
	JSR CONVERT.HEX_TO_ASCII	
		LDA RESULT+$0
	JSR PRINT.TEXT.WINDOW.SINGLE.CHAR

	RTS
@END


;setup routines
INIT.TEXT_WINDOW.RIGHT
@START
;PARAMETERS: ACC = ($00 = draw input window line | >=$01 = don't draw input window line), DRAW.START_BYTE

;SAVE PARAMETERS
	STA INIT.TEXT_WINDOW.RIGHT.MODE ;save ACC parm ($00 = draw input window line | >=$01 = don't draw input window line)
		
		;LDA PAGE.BACKGROUND	
		LDA #$03
	;JSR CLEAR.TEXT_WINDOW.RIGHT
	;JSR CLEAR.TEXT_WINDOW.TOP	
	;JSR CLEAR.TEXT_WINDOW.BOTTOM
	JSR DRAW.TEXT_WINDOW.RIGHT
		JSR FLIP.PAGE
		;JSR KEYIN
		

;INIT TEXT WINDOW 
;(for NPC Talk, other windows need to set these parmeteres after calling this routine)

		;INIT TEXT WINDOW BOUNDARIES (for the text itself, excluding borders)

		LDA #$19  ;$19full  /$1Etest 		;**OPT** Memory. Since settings these parms is for the NPC talk window only, move this section to NPC Talk module
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
;PARAMETERS: DRAW.START_BYTE

;INIT VARIABLES
	LDA #$00
	STA SCREEN_BYTE.COUNTER

	LDA #$01
	STA TW.RIGHT_WINDOW.CLEAN_UP.FLAG   ;turn flag on so that DRAW.SCREEN will erase the right edge and first two bytes of the top/bottom lines of the text window border.      ;**OPT** Memory. Review the code this flag triggered. Now that the NPC.TALK window is rendered to overlap the top/bottom text windows part of the clean up may no longer be needed
										;This is needed because this is the portion of the text window border that isn't normally onscreen, AND CLEAR.TEXT_WINDOW.RIGHT only clears the text space, not the border. If it were to clear the border on both pages the foreground erase would be very noticable.
										;Also used via TW.RIGHT_WINDOW.STATUS.FLAG (.EQ) to tell the animation manager not to draw on top of the text window
	
; .DRAW.SCREEN_BORDER
	LDA #TWB.RW.NPC_TALK.TOP_LINE		;load line in x register	
	STA DRAW.START_LINE
	
	; LDA #TWB.RW.NPC_TALK.LEFT_SBYTE
	; STA DRAW.START_BYTE
	
	LDA #TWB.RW.NPC_TALK.RIGHT_SBYTE+1
	STA DRAW.STOP_BYTE
		
	LDA #TWB.RW.NPC_TALK.BOTTOM_LINE+1
	STA DRAW.STOP_LINE

	LDA #$D5
	STA DRAW.BYTE_VALUE.HORIZONTAL+$0
	
	LDA #$AA
	STA DRAW.BYTE_VALUE.HORIZONTAL+$1
	
	LDA #$81
	STA DRAW.BYTE_VALUE.VERTICLE+$0

	LDA #$A0
	STA DRAW.BYTE_VALUE.VERTICLE+$1

.DRAW.RECTANGLE
		LDA PAGE.BACKGROUND
		ORA #$7C ;set DRAW.LINE flags
	JSR DRAW.LINE
	
	JSR FLIP.PAGE
					
		LDA PAGE.BACKGROUND
		ORA #$7C ;set DRAW.LINE flags
	JSR DRAW.LINE
	

	
;DRAW TALK INPUT WINDOW BORDER (RIGHT SIDE): TOP LINE
	
;VALIDATE ENTRANCE
	LDA INIT.TEXT_WINDOW.RIGHT.MODE	;restore ACC parm ($00 = draw input window line | >=$01 = don't draw input window line)
	BNE .EXIT

.DRAW.TW.INPUT.BORDER.TOP
	LDA PAGE.BACKGROUND
	LDX #TWB.TALK_INPUT_WINDOW.TOP_LINE
	JSR GET.LINE.ADDRESS1

	LDA PAGE.FOREGROUND
	JSR GET.LINE.ADDRESS2
	
	LDY #TWB.TALK_INPUT_WINDOW.LEFT_SBYTE+$2
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

CLEAR.TEXT_WINDOW.RIGHT ;Combat scroll window **OPT** Memory. Make sure this call is needed. If the combat window is drawn using DRAW.LINE the erase can be done by that routine if it's not already.  
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
	LDA #TWS.RW.NPC_TALK.TOP_ROW		;load line in x register	
	STA DRAW.START_LINE
	
	LDA #TWS.RW.NPC_TALK.LEFT_SBYTE
	STA DRAW.START_BYTE
	
	LDA #TWS.RW.NPC_TALK.RIGHT_SBYTE+$1
	STA DRAW.STOP_BYTE
		
	LDA #TWS.RW.NPC_TALK.BOTTOM_ROW+$1
	STA DRAW.STOP_LINE

.DRAW
		PLA ;restore USE.PAGE parameter to ACC
		ORA #$80	;set high bit as a parameter to indicate we want the byte value for the draw to be $00
	JSR DRAW.LINE

.EXIT
	RTS

@END

CLEAR.TW.RIGHT.DISPLAY_CHARACTER_ROSTER
@START
;PARAMETERS:  none
;RETURN: NONE
;ENTRANCE: DIRECT
;*Specifies which hi-res page the text window should be erased on (BOTH = $03)

;=====================SUBROUTINE DOCUMENTATION====================================
;
;=================================================================================

	;PHA ;save USE.PAGE parameter to ACC

.INIT
	; LDA #TWB.RW.DISPLAY_CHARACTER_ROSTER.TOP_LINE		;load line in x register	
	; STA DRAW.START_LINE
	
	; LDA #TWB.RW.DISPLAY_CHARACTER_ROSTER.LEFT_SBYTE
	; STA DRAW.START_BYTE
	
	; LDA #TWB.RW.DISPLAY_CHARACTER_ROSTER.RIGHT_SBYTE
	; STA DRAW.STOP_BYTE
		
	; LDA #TWB.RW.DISPLAY_CHARACTER_ROSTER.BOTTOM_LINE
	; STA DRAW.STOP_LINE

; .DRAW
		; PLA ;restore USE.PAGE parameter to ACC
		; ORA #$80	;set high bit as a parameter to indicate we want the byte value for the draw to be $00
	; JSR DRAW.LINE

	

	LDA #SCREEN_BORDER.TOP_LINE
	STA DRAW.START_LINE
	
	LDA #TWB.RW.DISPLAY_CHARACTER_ROSTER.LEFT_SBYTE
	STA DRAW.START_BYTE
	
	LDA #TWB.RW.DISPLAY_CHARACTER_ROSTER.RIGHT_SBYTE
	STA DRAW.STOP_BYTE
		
	LDA #TWB.RW.DISPLAY_CHARACTER_ROSTER.BOTTOM_LINE
	STA DRAW.STOP_LINE

	LDA #$D5
	STA DRAW.BYTE_VALUE.HORIZONTAL+$0
	
	LDA #$AA
	STA DRAW.BYTE_VALUE.HORIZONTAL+$1
	
	LDA #$81
	STA DRAW.BYTE_VALUE.VERTICLE+$0

	LDA #$A0
	STA DRAW.BYTE_VALUE.VERTICLE+$1

.DRAW.RECTANGLE
		LDA PAGE.BACKGROUND
		ORA #$7C ;set DRAW.LINE flags
	JSR DRAW.LINE
	
	JSR FLIP.PAGE
					
		LDA PAGE.BACKGROUND
		ORA #$7C ;set DRAW.LINE flags
	JSR DRAW.LINE
	

	
			
	
.EXIT
	RTS

@END

CLEAR.TW.RIGHT_SMALL.MAIN_SCREEN
@START
;PARAMETERS:  USE.PAGE*
;RETURN: NONE
;ENTRANCE: DIRECT
;*Specifies which hi-res page the text window should be erased on (BOTH = $03)

;=====================SUBROUTINE DOCUMENTATION====================================
;
;=================================================================================

	PHA ;save USE.PAGE parameter to ACC

.INIT
	LDA #TWB.RIGHT_S_WINDOW.TOP_LINE		;load line in x register	
	STA DRAW.START_LINE
	
	LDA #TWB.RIGHT_S_WINDOW.LEFT_SBYTE
	STA DRAW.START_BYTE
	
	LDA #TWB.RIGHT_S_WINDOW.RIGHT_SBYTE
	STA DRAW.STOP_BYTE
		
	LDA #TWB.RIGHT_S_WINDOW.BOTTOM_LINE
	STA DRAW.STOP_LINE

.DRAW
		PLA ;restore USE.PAGE parameter to ACC
		ORA #$80	;set high bit as a parameter to indicate we want the byte value for the draw to be $00
	JSR DRAW.LINE

.EXIT
	RTS

@END

;CLEAR.TEXT_WINDOW.RIGHT_SMALL
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
	JSR UPDATE.VOICE_MODE.DISPLAY ;this is text normally in the input window so it needs to be output to video screen again after the clear. 

	
.EXIT
	
	RTS
@END

@END



;=============================COPY/PASTE ONLY===========
@START				
				
;;	JMP START		;SKIP GRAPHICS MODE

	
	; LDA GRAPHICS	;TURN ON GRAPHICS MODES
	; LDA HIRES		;SELECT HI-RES MODE
	; LDA	PAGE1		;SELECT PAGE 1
	; LDA MIXOFF		;SELECT FULL SCREEN GRAPHICS (PAGE 1)
	
	
	; LDA #$00		;SPECIFY BOTH PAGES FOR CLEAR SCREEN
	; JSR SCLEAR		;CLEAR SCREEN BEFORE TURNING ON GRAPHICS (AVOIDS UNSIGHTLY FLASH OF RANDOM DOTS) 


;START
	
	; JSR HCG.ON		;REDIRECT DOS OUTPUT ROUTINES VIA CUSTOM HCG OUTPUT HANDLER AT $03D0
	
	; JMP	SCENARIO4
	
	
;SCENARIO1	;;======CONTROL CURSOR POSITION=====
	
;;SET CURSOR POSITION 
	; LDA #$0
	; STA HTAB	
	; LDA #$0
	; STA VTAB
	; JSR	UPDATE.CHAR.POS
	; LDA #$C8
	; JSR COUT

	; JSR STOP

;SCENARIO2	;=======TEXT WINDOW

	; LDA #$00	
	; TAX
	; TAY
	
	; JSR TEXT.WINDOW
	; LDA #$00
	; STA HTAB
	; STA VTAB
	; JSR UPDATE.CHAR.POS
	; LDA #$A4
	; LDX #$00
; .LOOP0	
	; JSR COUT
	; INX
	; CPX #$40
	; BNE .LOOP0
	
	; JSR STOP
	


;SCENARIO3	;DEMONSTRATE CONTROL OVER EVERY X,Y COORDINATE
	; LDA #$00	
	; TAX
	; TAY
	
; .LOOP1	
	; STY VTAB		;Y (VERT POSITION)
; .PLOT.ROW
	; STX HTAB		;X	(HOR POSITION)		
	; JSR	UPDATE.CHAR.POS
	; LDA #$A4
	; JSR	COUT
	
	; JSR WAIT
	; INX
	; CPX #$28
	; BNE .PLOT.ROW	
	; LDX #$00
	; INY
	; CPY	#$19 
	; BNE .LOOP1
	

	; JSR STOP

;SCENARIO4	;DISPLAY EACH CHARACTER IN SET
	; LDA #$00	
	; TAX
	; TAY
	
	; LDA #$A1
	; STA CHAR
; .LOOP1	
	; STY VTAB		;Y (VERT POSITION)
; .PLOT.ROW
	; STX HTAB		;X	(HOR POSITION)		
	; JSR	UPDATE.CHAR.POS
	; LDA CHAR
	; JSR	COUT
	; LDA #$A0		;SPACE
	; JSR COUT
	
	; JSR WAIT
	; INC CHAR
	; INX
	; INX
	; CPX #$28
	; BNE .PLOT.ROW	
	; LDX #$00
	; INY
	; CPY	#$19 
	; BNE .LOOP1
	

	; JSR STOP	


	
;=================SUBROUTINES=============

;TEXT.WINDOW
;;SETUP TEXT WINDOW
	; LDA #$00		;(!0-39) (X) SET UPPER LEFT COLUMN (TEXT WRITES TO THIS COLUMN)
	; STA TW1			;($0-27)
	; LDA #$02		;(!0-39) (X) SET WIDTH (I.E. $03 = 3 COLUMNS WIDE, 0 DOESN'T COUNT)
	; STA TW2			;($0-27)
	; LDA #$00		;(!0-23) (Y) TOP ROW 
	; STA TW3			;($0-18)
	; LDA #$18		;(!0-23) (Y) BOTTOM ROW (TEXT STOPS 1 ROW ABOVE THIS VALUE)
	; STA TW4			;($0-18)	(bottom row - top row = total # of rows)

	; RTS
@END

;======DEFINE VARIBLES======

;**see "GENERAL ROUTINES (LOW LEVEL)" section in offloaded_variables.ASM
 