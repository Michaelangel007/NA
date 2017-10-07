;************************INCLUDE FILE*****************************
;(do not assemble stand-alone.  )


;=====================INCLUDE FILE DOCUMENTATION====================================
;
;Include file to SWAP.ROUTINES.INVENTORY.ASM
;
;=================================================================================






INV.INVENTORY_MODULE.ENTRANCE
@START
;PARAMETERS: ACC = inventory sub-module # to launch, INV_4.ACTIVE_PLAYER*
;ENTRANCE: direct
;RETURN: none

;*Only needs to be set if calling from combat module. Otherwise INV_4.ACTIVE_PLAYER
;is set to player one by the .INIT.ACTIVE_PLAYER.SELECTOR section below. 


;=====================SUBROUTINE DOCUMENTATION====================================
;
;There are 3 state loops: INV_4.STATE.LOOP (inventory menu) INV.STATS.STATE_LOOP (stats menu: regular entrance)
;and .COMBAT.STATS.STATE_LOOP (stats menu: combat entrance)
;
;---Add New Sub-Module---
;*Add code block to do seek/read in INV.READ_LAUNCH.SUB_MODULE
;
;=================================================================================



;---PSEDUO CODE---
;
;Outside Routine Changes
;	just JSR launch inventory module, INV.READ_LAUNCH.SUB_MODULE will take care of launching entrance routine
;
;INV.STATE_LOOP.MENU_ICONS (tab between menu icons)  ****rename existing to this label***
;	INC INV.ACTIVE_MENU.CODE (rest on max value)
;
;BRANCH INV.ACTIVE_MENU.CODE
;
;
;STATS
;	display menu icon selector
;	load sub-module (it will take care of is-loaded check)	
;	call entrance routine taken care of by INV.READ_LAUNCH.SUB_MODULE
;
;
;WEAPONS
;	set mode ***NOT YET DONE**
;	load sub-module (it will take care of is-loaded check)
;	call entrance routine taken care of by INV.READ_LAUNCH.SUB_MODULE
;
;ARMOR
;	display menu icon selector
;	set mode ***NOT YET DONE**
;	JMP entrance routine
;
;MISC ITEMS
;	display menu icon selector
;	set mode ***NOT YET DONE**
;	JMP entrance routine
;
;SPELLS
;	display menu icon selector
;	set mode ***NOT YET DONE**
;	JMP entrance routine
;
;INV_0.0 (stats) 
;	entrance
;			if init has been done before then JMP screen0.start (detect via high bit on ;INV.SUB_MODULE.LOAD_FLAGS )
;			do any inits
;					set INV.SUB_MODULE.LAUNCH_CODE to 0 (1st stats screen page)
;	screen0.start
;		<insert include file with the stats summary code>
;
;	JMP or fall through to INV_0.STATE_LOOP
;
;INV_0.STATE_LOOP
;	on 1-6, tab: seed $C000, JMP INV.STATE_LOOP
;	on ESC: exit inventory module (not figured out yet)
;	on up/down arrow: JSR INV.NEXT_PC, which should increment the active player parm (make sure all sub_modules use the same memory address vi .EQ), 
;			This routine should JMP to LAUNCH.SUB_MODULE so that control is passed to the entrance of the current screen so that it redraws
;	on right arrow: INC INV_0.ACTIVE_SCREEN.CODE, set to $00 on max, then LAUNCH.SUB_MODULE
;	on left arrow: DEC INV_0.ACTIVE_SCREEN.CODE, set to $03 on min, then LAUNCH.SUB_MODULE
;
;LAUNCH.SUB_MODULE
;	call INV.READ_LAUNCH.SUB_MODULE, ACC = INV_0.ACTIVE_SCREEN.CODE
;
;
;INV_1-INV_3 (other stats screens)
;		clear text space in inventory window
;		display the stats screen
;		JMP to INV_0.STATE_LOOP
;
;		
;
;
;INV_4.0 (display_inventory)
;		;local state loop
;		;on tab, 1-6, seed tab back into $C000 (so top level state loop will pick it up), run local exit routine
;		on up/down arrow: JSR INV.NEXT_PC, which should increment the active player parm (make sure all sub_modules use the same memory address vi .EQ), 
;					and then JMP to the init section of display inventory so the screen refreshes (make sure the JMP destination cleares the text space in the inventory window)
;	on ESC: exit inventory module (not figured out yet, but run it through local exit, INV_4.0.EXIT I think)
;
;.LOCAL_EXIT
;		JMP INV.STATE_LOOP
;
;INV_5 (game settings)
;		<do nothing for now, will write this screen just before game is finished>
;		on tab, 1-6 JMP INV.STATE_LOOP.MENU_ICONS
;		on ESC: exit inventory module (not figured out yet)
;
;(NOT NEEDED IF I SETUP INV.READ_LAUNCH.SUB_MODULE.WITH_RTS)****modify .DETERMINE.READY_UNREADY.MODE (display inventory sub_module) to
;use INV.READ_LAUNCH.SUB_MODULE to launch in place of the JSR to the entrance routine. Save the 
;ACC parm to INV.READ_LAUNCH.SUB_MODULE.PARM_ACC so that it passes the parm via the ACC to the entrance of the sub_module loaded
;
;
;(DONE) INV.READ_LAUNCH.SUB_MODULE changes
;	at top of routine check INV.SUB_MODULE.LOAD_FLAGS using the ACC parm as index. If sub_module is loaded then goto local exit
;	*consider using ACC parm as index to lookup table seeded with the LO/HO address of the sub_module entrance routines using SBASM directive (also also get the read and seek bytes via lookup table)
;	at least remove the INV.SUB_MODULE.LOAD_FLAGS checks from the branch sections
;
;	*the idea here is do an indirect JMP to the module entrance. 
;
;	(DONE) For calc stats and ready/unready, move their load code to their own branch style routine which
;	RTSs back. INV.READ_LAUNCH.SUB_MODULE.WITH_RTS.  
;
;
;(DONE, i.e. converted to new format) ----variables (OLD). New. INV.SUB_MODULE.LOAD_FLAGS mirrors the INV_(x) designation
;INV.SUB_MODULE.LOAD_FLAGS	.EQ?
;	FLAG_0-8 = INV_0.0-INV_0.8
;	FLAG_9	= INV_4.0 (display_inventory)
;	FLAG_A	= INV 1.0 (ready_unready)
;	FLAG_B	= INV 2.0 (calculate combat stats)
;

			
					
;SAVE PARAMTERS
	STA INV.SUB_MODULE.LAUNCH_CODE ;save parm: inventory sub-module code to stack	(PARM: ACC = inventory sub-module # to launch)




.INIT	;****Connect combat to inventory note and **OPT** Memory. I'm not sure if this section should be here or in the screen0 module which is the first loaded from combat or non-combat. See .SET.ACTIVE.SCREEN. This section is in the location where this code would go. There is currently already some duplication
	LDA #$00
	STA INV.READY_UNREADY.RUN_FLAG	;($00 = ready_unready not called | $01 ready_unready called. ready_unready refers to INV.READY_UNREADY.EQUIPMENT.ENTRANCE)
	STA INV.RE.UPDATE_DAMAGE.FLAG 	;($00 = no changes made that effect DAMAGE on the character sheet. $01 changes were made that affect the same)
	STA INV.RE.UPDATE_DEFENSE.FLAG 	;($00 = no changes made that effect DAMAGE on the character sheet. $01 changes were made that affect the same)
	STA INV.ACTIVE_MENU.CODE 		;($00 = stats menu)
	STA INV.ACTIVE_STATS_SCREEN.CODE ;($00 = stats: screen0 - stats summary)

	
	;!!!!!TEMP!!!!
		ldx #$00		;**OPT** Memory. Remove
.init.loop3
		lda #$00
		sta INV.DEBUG.LOG,X
		INX
		bne .init.loop3
		
.INIT.READ.INVENTORY.DATA				
			;**OPT** Memory. If INV.READ_FILE.CHAR_SHEET.READIED is converted to use PRODOS.IO.READ_CURRENT_FILE then the JSR PRODOS.IO.RESET.FILE_OFFSET 
			;can be remove below because it is done at the start of PRODOS.IO.READ_CURRENT_FILE
	;reset file offset
	JSR PRODOS.IO.RESET.FILE_OFFSET

	JSR INV.READ_FILE.CHAR_SHEET.READIED
		;RETURN VALUE: CHR_SHEET.PC.READIED_EQUIP

.INIT.OTHER
;init INV.SUB_MODULE.LOAD_FLAGS

	LDX #$00 ;init loop counter
	LDA #$00 ;set all flags to "not loaded in memory"
.INIT.LOOP2
	STA INV.SUB_MODULE.LOAD_FLAGS,X
	INX
	CPX #INV.SUB_MODULE.LOAD_FLAGS.ARRAY_SIZE
	BNE .INIT.LOOP2


.SYNC.HI_RES.PAGES			
;SYNC HI-RES PAGES
;(this makes sure both pages are the same so that no unexpectd graphics occur
;when the inventory display routines modify the background page and then flip it. Those
;routines only sync the portion of the screen that contains the inventory window).

		;ENABLE BANK-SWITCHED RAM ($D000 Bank 1)
		LDA $C08B	 	;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK1, 1ST)
		LDA $C08B		;**OPT** Memory. If it turns out lots of copy.screen calls are needed, setup a wrapper subroutine that handles the BSR enable/disable...then in this location, and in every other location with a copy screen call, just JSR the wrapper	
	JSR COPY.SCREEN	
		;RE-ENABLE BANK-SWITCHED RAM ($D000 Bank 2)
		LDA $C083				;READ TWICE TO READ/WRITE ENABLE BANK SWITCHED-RAM ($Dxxx BANK2 2nd)
		LDA $C083
			
			
@END
	

.LOAD.SUB_MODULE
		LDA INV.SUB_MODULE.LAUNCH_CODE ;inventory sub-module #
		CMP #$06
		BCS .NON_MENU.SUB_MODULES ;if sub_module is not a menu (i.e. ready_unready and calculate stats, use the read-only version with RTS rather than launching the sub_module via JMP after reading it. 
	JMP INV.READ_LAUNCH.SUB_MODULE
	;JMP .LOAD.SUB_MODULE.DONE

.NON_MENU.SUB_MODULES
	;ACC: INV.SUB_MODULE.LAUNCH_CODE ;inventory sub-module # to read from disk without launchin
	JSR INV.READ_LAUNCH.SUB_MODULE.WITH_RTS
.LOAD.SUB_MODULE.DONE
	
	RTS ;exit to routine that called INV.INVENTORY_MODULE.ENTRANCE. INV.INVENTORY_MODULE.EXIT doesn't need to run because 
		;the entrance routine to the sub_module loaded will be called by the calling routine directly. This routine just read the sub_module from disk. 

@END

INV.INVENTORY_MODULE.EXIT
@START


;****INV DEBUG LOG*** 			;**OPT** Memory. Remove.
@START
.DEBUG.LABEL
		;PHA
		LDA #.DEBUG.LABEL
		STA INV.DEBUG.ADDRESS+$0
		LDA /.DEBUG.LABEL
		STA INV.DEBUG.ADDRESS+$1
	JSR INV.DEBUG.LOG.WRITE
		;PLA
@END


.RESET_FLAGS
	LDA #$00
	STA INV_0.SCREEN0.FIRST_RUN.FLAG ;($00 =  first run of screen0 not complete | $01 first run screen0 complete)


	

.WRITE_DISK.READIED_EQUIPMENT_TABLE
@START
	;reset file offset
	JSR PRODOS.IO.RESET.FILE_OFFSET

	
;set command type (READ | WRITE | SEEK)
; cmd_seek.current_drive 	.EQ $90
; cmd_read.current_drive  	.EQ $91
; cmd_write.current_drive 	.EQ $92
;			
; cmd_read.drive1			.EQ $1
; cmd_read.drive2		  	.EQ $81
; cmd_write.drive1			.EQ $2
; cmd_write.drive2			.EQ $82

	lda #cmd_write.current_drive
	sta parm.reqcmd

;set write data size (# of 512 byte blocks to write from memory)
	lda #CHR_SHEET.PC.READIED_EQUIP.FILE_WRITE.SIZE 		;always #$00
	sta parm.sizelo

	lda /CHR_SHEET.PC.READIED_EQUIP.FILE_WRITE.SIZE ;number of pages to write (must be in multiples of 2. i.e. 2, 4, 6, 8 $0A, etc)
	sta parm.sizehi
	
;set source memory address for write
	lda #CHR_SHEET.PC.READIED_EQUIP
	sta parm.ldrlo
	lda /CHR_SHEET.PC.READIED_EQUIP
	sta parm.ldrhi
	
;set filename to write to	
	lda #DATA.GME.INVENTORY ;load LO address
	sta parm.namlo	
	lda /DATA.GME.INVENTORY ;load HO address
	sta parm.namhi	
		
		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO



@END




;****INV DEBUG LOG*** 			;**OPT** Memory. Remove.
@START
.DEBUG.LABEL2
		;PHA
		LDA #.DEBUG.LABEL2
		STA INV.DEBUG.ADDRESS+$0
		LDA /.DEBUG.LABEL2
		STA INV.DEBUG.ADDRESS+$1
	JSR INV.DEBUG.LOG.WRITE
		;PLA
@END


.WRITE_DISK.PLAYER_INVENTORY_TABLE
@START
;set command type (READ | WRITE | SEEK)
; cmd_seek.current_drive 	.EQ $90
; cmd_read.current_drive  	.EQ $91
; cmd_write.current_drive 	.EQ $92
;			
; cmd_read.drive1			.EQ $1
; cmd_read.drive2		  	.EQ $81
; cmd_write.drive1			.EQ $2
; cmd_write.drive2			.EQ $82

	lda #cmd_write.current_drive
	sta parm.reqcmd

;set write data size (# of 512 byte blocks to write from memory)
	lda #INV.INVENTORY_DATA.WRITE_SIZE 		;always #$00
	sta parm.sizelo

	lda /INV.INVENTORY_DATA.WRITE_SIZE ;number of pages to write (must be in multiples of 2. i.e. 2, 4, 6, 8 $0A, etc)
	sta parm.sizehi

	
;set source memory address for write
	lda #INV.PLAYER.INVENTORY.DATA
	sta parm.ldrlo
	lda /INV.PLAYER.INVENTORY.DATA
	sta parm.ldrhi
	
;set filename to write to	
	lda #DATA.GME.INVENTORY ;load LO address
	sta parm.namlo	
	lda /DATA.GME.INVENTORY ;load HO address
	sta parm.namhi	



;****INV DEBUG LOG*** 			;**OPT** Memory. Remove.
@START
.DEBUG.LABEL3
		;PHA
		LDA #.DEBUG.LABEL3
		STA INV.DEBUG.ADDRESS+$0
		LDA /.DEBUG.LABEL3
		STA INV.DEBUG.ADDRESS+$1
	JSR INV.DEBUG.LOG.WRITE
		;PLA
@END

	
		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO
	
@END


	
;====TROUBLESHOOTING HOOK (check character sheet and readied equipment data)=======
@START

; ;TROUBLESHOOTING: PC READIED EQUIPMENT TABLE READ
		; LDA #$03
		; ;ACC = player sequential # (high-bit not set = read mode)
	; JSR INV.READ_WRITE_RECORD.CHAR_SHEET.READIED
		; ;RETURN VALUE: CHR_SHEET.PC.READIED_EQUIP.RECORD.READ($10)

		
			; ;TROUBLESHOOTING LOOP
			; LDX #$00
; .TEST.LOOP1			
			; LDA CHR_SHEET.PC.READIED_EQUIP.RECORD.READ,X
			; STA $BF00,X
			; INX
			; CPX #$10
			; BNE .TEST.LOOP1
			
	
; ;TROUBLESHOOTING: PC CHARACTER SHEET READ
	; ;read PC character sheet data
		; LDA #$03
		; ;ACC = player sequential # (high-bit not set = read mode)
	; JSR COMBAT.READ_WRITE.CHR_SHEET.PC
		; ;RETURN VALUE = CHR_SHEET.RECORD.READ
		; ;
			; ;TROUBLESHOOTING LOOP
			; LDX #$00
; .TEST.LOOP4			
			; LDA CHR_SHEET.RECORD.READ,X
			; STA $BC00,X
			; INX
			; BNE .TEST.LOOP4
			; LDA #$AC
			; ldx #CHR_SHEET.PC.READIED_EQUIP
			; ldy /CHR_SHEET.PC.READIED_EQUIP
			; JSR PREP.BRK
			; BRK
			
@END


			; jsr flip.page
			; jsr keyin
			; LDA #$AA
			; jmp full.brk
			; BRK

			; LDA #$01
			; STA TROUBLESHOOTING.HOOK

			
.EXIT

	RTS
@END


;COMMON SUBROUTINES (accessible to all inventory sub-modules)
@START

;file I/O and in-memory array I/O
INV.FILE.READ.INVENTORY_DATA.ENTRANCE
@START
;PARAMETERS: none
;ENTRANCE: direct
;RETURN: INV.PLAYER.INVENTORY.DATA, INV.FILE.READ.INVENTORY.OPEN_STATUS ($00 = file was open. $01 = file was not open)


			;**OPT** Memory. It may not be necesssary to check for the open file since stats is the first sub_module lauched. 
	
	LDA #$00 ;**OPT** Memory. Seperate from the note above, the open file status code isn't critical. it's for an error trap. 
	STA INV.FILE.READ.INVENTORY.OPEN_STATUS ;($00 = file was open. $01 = file was not open)
	
			
;---CHECK FILE STATUS
		LDA #DATA.GME.INVENTORY
		STA FILENAME.CHECK_LO
		LDA /DATA.GME.INVENTORY
		STA FILENAME.CHECK_HI
	JSR PRODOS.IO.CHECK_FILE_STATUS
		;RETURN VALUE: ACC ($00 = file open | $01 = file closed)

;---PROCESS FILE STATUS CODE
.IS.FILE.OPEN
		LDA #DATA.GME.INVENTORY
		STA FILENAME.CHECK_LO
		LDA /DATA.GME.INVENTORY
		STA FILENAME.CHECK_HI
	JSR PRODOS.IO.CHECK_FILE_STATUS
		;RETURN VALUE: ACC ($00 = file open | $01 = file closed)
	CMP #$00 ;is file open?
	BEQ .FILE_OPEN ;branch if yes
	
	;**FALLS THROUGH**
	
.FILE_NOT_OPEN
	LDA #$01
	STA INV.FILE.READ.INVENTORY.OPEN_STATUS ;($00 = file was open. $01 = file was not open)

	JSR FILE.OPEN.INVENTORY ;***WARNING*** WHEN DUPLICATING THIS CODE REPLACE THIS JSR WITH A FILE OPEN ROUTINE FOR THE FILE THAT NEEDS TO BE OPENED

	;**FALLS THROUGH**
	
.FILE_OPEN


.READ.FILE
		;set destination memory address
		lda #INV.PLAYER.INVENTORY.DATA
		sta parm.ldrlo
		lda /INV.PLAYER.INVENTORY.DATA
		sta parm.ldrhi
		
		lda #INV.INVENTORY_DATA.OFFSET
		sta PARM.SEEK_BYTES+$0	;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
		lda /INV.INVENTORY_DATA.OFFSET
		sta PARM.SEEK_BYTES+$1	;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
		
		LDA #INV.INVENTORY_DATA.READ_SIZE
		sta PARM.READ_BYTES+$0		;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
		LDA /INV.INVENTORY_DATA.READ_SIZE
		sta PARM.READ_BYTES+$1		;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	JSR PRODOS.IO.READ_CURRENT_FILE
	
.EXIT

.DEBUG_TRAP ;garabge data in inventory display (INV_4)
@START
		sta temp
		lda INV.PLAYER.INVENTORY.DATA+$C
		cmp #$80
		bne .DEBUG_TRAP.REPORT_ERROR1
		lda INV.PLAYER.INVENTORY.DATA+$D
		cmp #$07
		bne .DEBUG_TRAP.REPORT_ERROR1
		lda INV.PLAYER.INVENTORY.DATA+$E
		cmp #$01
		bne .DEBUG_TRAP.REPORT_ERROR1
		JMP .DEBUG_TRAP.NO_ERROR
		
.DEBUG_TRAP.REPORT_ERROR1
;.DEBUG_TRAP (INV.FILE.READ.INVENTORY_DATA.ENTRANCE) reports that INV.PLAYER.INVENTORY.DATA does not contain
;the test bytes expected. This trap was created to track a bug causing garbage to be displyed in the inventory window. 
;based on some initial testing, when the bug occurs, the inventory data array ($BA00-$BFFF) contains all $00 values except for
;a portion of the $BD00 page. Thus, the theory is that when the bug occurs, the data in the file never makes it into memory for some reason.


		;save debug info
		lda parm.ldrlo
		sta $be00
		lda parm.ldrhi
		sta $be01
		
		lda PARM.SEEK_BYTES+$0	;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
		sta $be02
		lda PARM.SEEK_BYTES+$1	;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
		sta $be03
		
		lda PARM.READ_BYTES+$0		;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
		sta $be04
		lda PARM.READ_BYTES+$1		;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
		sta $be05
		
		
	LDA #$EE
	LDX #$EE
	LDY #$EE
	JSR PREP.BRK
	BRK
	
.DEBUG_TRAP.NO_ERROR



		lda temp
		
@END


	RTS

@END

INV.READ_FILE.CHAR_SHEET.READIED
@START
;PARAMETERS: none
;ENTRANCE: ****FILE.OPEN.INVENTORY must be called first
;RETURN: CHR_SHEET.PC.READIED_EQUIP	(x)

;=====================SUBROUTINE DOCUMENTATION====================================
;
;This routine reads the readied equipment data for all players into memory from
;the start of the inventory file. 
;
;This routine expects that the inventory file is already open, which is done in FILE.OPEN.INVENTORY 
;
;=================================================================================


;--------OPEN FILE------
;aready opened via FILE.OPEN.INVENTORY


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
	lda #CHR_SHEET.PC.READIED_EQUIP	
	sta parm.ldrlo
	lda /CHR_SHEET.PC.READIED_EQUIP	
	sta parm.ldrhi

;set read length (bytes)
	lda #INV.READIED_EQUIPMENT_TABLE.READ_SIZE								;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda /INV.READIED_EQUIPMENT_TABLE.READ_SIZE								;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi
	
; ;set filename to read from	
	; lda #DATA.PLY.READIED	;load LO address
	; sta parm.namlo
	; lda /DATA.PLY.READIED	;load HO address
	; sta parm.namhi

		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO

	RTS
	
@END

INV.READ_WRITE_RECORD.CHAR_SHEET.READIED ;in-memory array
@START
;PARAMETERS: *ACC (sequential player #) - (high-bit not set = read mode | high-bit set = write mode)
;ENTRANCE: direct
;RETURN: CHR_SHEET.PC.READIED_EQUIP.RECORD.READ($10)
	
;*ACC parameter examples:
;	player 1, read mode: set to $01  (high-bit not set)
;	player 1, wrote mode: set to $81 (high-bit set)

;=====================SUBROUTINE DOCUMENTATION====================================
;
;This routine copies record data for the specified PC from the in-memory table 
;into the read buffer used by .LOOP.CALC.DERIVED_STATS
;
;=================================================================================


	
;SAVE PARAMETERS

.SET.PC.NUMBER
	;ACC = parm: read/write mode
	STA TEMP ;push parameter for use later		
	AND #$7F ;mask-out high-bit 
	;ACC = parm: PC sequential number (i.e. player 1-6)
	SEC
	SBC #$01 ;subtract one because the first character sheet index starts with $00 (so for player 1, we need the calculation to be 0*$20 = $00)
	STA CHAR_SHEET.READIED.S_ENTITY.NUMBER
	;**FALLS THROUGH**
	
;.SET.MODE
	; PLA ;pull mode/player# parameter
	; BMI .SET.WRITE_MODE ;branch if high-bit is set (write mode)
; ;.SET.READ_MODE
	; CLC ;set read mode
	; ;LDA #$00 ;set read mode value
	; JMP .SET.MODE.COMPLETE
	
;.SET.WRITE_MODE

			; LDA #$AA
			; ; LDX PLAYER.INVENTORY.DATA.POINTER+$0
			; ; LDY PLAYER.INVENTORY.DATA.POINTER+$1
			; ldx #CHR_SHEET.PC.READIED_EQUIP.RECORD.READ
			; ldy /CHR_SHEET.PC.READIED_EQUIP.RECORD.READ
			; JSR PREP.BRK
			; BRK	
			
	; ;LDA #$01 ;set write mode value
	; SEC ;set write mode
	; ;**FALLS THROUGH**
; .SET.MODE.COMPLETE
		
;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA
	
.INIT
	;calculate player record index
	LDA CHAR_SHEET.READIED.S_ENTITY.NUMBER
	;ACC = player number
	ASL ;!X2
	ASL ;!X4
	ASL ;!X8
	ASL ;!X16
	TAY

			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; LDA #$CB
			; ;lda FILE.ITEM_TABLE.ID
			; ;LDA CHR_SHEET.READIED_EQUIP.ID.LHAND ;load readied armor_id (left hand)
			; LDX CHAR_SHEET.READIED.S_ENTITY.NUMBER			
			; ; LDX COMBAT.STATS.REGULAR.ARMOR.ROLL
			; ; LDY COMBAT.STATS.BASE_DAMAGE.ROLL
			; ;LDX CHR_SHEET.PC_MOB.ARMOR_HI
			; ;LDY CHR_SHEET.PC_MOB.ARMOR
			; ;LDX INV.ITEM_TABLE.WP.MAGIC_FLAG5
			; ; ldx #FILE.ITEM_TABLE.RECORD.READ
			; ; ldy /FILE.ITEM_TABLE.RECORD.READ
			; ; ldx #CHR_SHEET.PC.READIED_EQUIP.RECORD.READ
			; ; ldy /CHR_SHEET.PC.READIED_EQUIP.RECORD.READ
			; JSR PREP.BRK
			; BRK
; .TEMP

	LDX #$00 ;init loop/field counter	
.READ_WRITE.LOOP
	LDA TEMP ;load mode/player# parameter (high-bit not set = read mode | high-bit set = write mode)
	; ;CARRY FLAG: (clear = read | set = write)
	; BCS .WRITE ;if carry set then use write mode
	BMI .WRITE ;if carry set then use write mode
	LDA CHR_SHEET.PC.READIED_EQUIP,Y
	STA CHR_SHEET.PC.READIED_EQUIP.RECORD.READ,X
	JMP .INCREMENT.INDEX
.WRITE 


			
	LDA CHR_SHEET.PC.READIED_EQUIP.RECORD.READ,X
	STA CHR_SHEET.PC.READIED_EQUIP,Y
	;**FALLS THROUGH**
.INCREMENT.INDEX
	INY
	INX
	CPX #CHR_SHEET.PC.READIED_EQUIP.RECORD.SIZE
	BNE .READ_WRITE.LOOP
	
	;**FALLS THROUGH**
	
.EXIT
;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX
	
	RTS
	
@END

INV.READ_LAUNCH.SUB_MODULE
@START
;PARAMETERS: ACC = (inventory sub-module # to read in)
;ENTRANCE: INV.INVENTORY_MODULE.ENTRANCE
;RETURN: sub-module code block


;INV.SUB_MODULE.FILE.PARAMETERS datagram
;byte $0-1: seek bytes HO/LO
;byte $2-3: read bytes HO/LO
;byte $4-5: patch section start HO/LO
;byte $6-7: sub_module entrance HO/LO

.IN_MEMORY.CHECK ;is sub_module already in memory?
	;PARM: ACC = (inventory sub-module # to read in)
	TAX ;set INV.SUB_MODULE.LOAD_FLAGS index
	
	;calculate INV.SUB_MODULE.FILE.PARAMETERS index
	ASL ;X2
	ASL ;X4
	ASL ;X8
	TAY
	
	;is sub_module already in memory?
	LDA INV.SUB_MODULE.LOAD_FLAGS,X ;($00 = sub-module not loaded in memory | $01 = sub-module loaded in memory)
	BNE .LAUNCH.SUB_MODULE ;branch if sub_module is already in memory


.READ.SUB_MODULE.FROM_FILE	
	;set flag for this sub_module to loaded in memory
	LDA #$01
	STA INV.SUB_MODULE.LOAD_FLAGS,X ;($00 = sub-module not loaded in memory | $01 = sub-module loaded in memory)

	
		;set PARM.SEEK_BYTES and PARM.READ_BYTES
		LDA INV.SUB_MODULE.FILE.PARAMETERS+$0,Y
		STA PARM.SEEK_BYTES+$0
		LDA INV.SUB_MODULE.FILE.PARAMETERS+$1,Y
		STA PARM.SEEK_BYTES+$1
		LDA INV.SUB_MODULE.FILE.PARAMETERS+$2,Y
		STA PARM.READ_BYTES+$0
		LDA INV.SUB_MODULE.FILE.PARAMETERS+$3,Y
		STA PARM.READ_BYTES+$1	

		;set destination memory address
		lda INV.SUB_MODULE.FILE.PARAMETERS+$4,Y
		sta parm.ldrlo
		lda INV.SUB_MODULE.FILE.PARAMETERS+$5,Y
		sta parm.ldrhi	

	;JSR PRODOS.IO.RESET.FILE_OFFSET
	JSR PRODOS.IO.READ_CURRENT_FILE	

	
.LAUNCH.SUB_MODULE
		lda INV.SUB_MODULE.FILE.PARAMETERS+$6,Y
		sta JMP.DESTINATION.ADDR+$0
		lda INV.SUB_MODULE.FILE.PARAMETERS+$7,Y
		sta JMP.DESTINATION.ADDR+$1


;****INV DEBUG LOG*** 			;**OPT** Memory. Remove.
@START
.DEBUG.LABEL
		;PHA
		LDA #.DEBUG.LABEL
		STA INV.DEBUG.ADDRESS+$0
		LDA /.DEBUG.LABEL
		STA INV.DEBUG.ADDRESS+$1
	JSR INV.DEBUG.LOG.WRITE
		;PLA
@END
		
	JMP (JMP.DESTINATION.ADDR)	
		

@END
	
INV.READ_LAUNCH.SUB_MODULE.WITH_RTS
@START
;PARAMETERS: ACC = (inventory sub-module # to read in)
;ENTRANCE: INV.INVENTORY_MODULE.ENTRANCE
;RETURN: sub-module code block

.BRANCH.SUB_MODULE
	;ACC = INV.SUB_MODULE.LAUNCH_CODE ;inventory sub-module # to launch)
	CMP #$06
	BEQ .SUB_MODULE.06 ;INV_6.READY_UNREADY
	CMP #$07
	BEQ .SUB_MODULE.07 ;INV_7.CALC.STATS
	
.ERROR
;.BRANCH.SUB_MODULE (INV.INVENTORY_MODULE.ENTRANCE) reports unexpected sub_module launch code 
	JSR PREP.BRK
	BRK

.SUB_MODULE.06 ;INV_6.READY_UNREADY
@START
	;is sub_module already in memory?
	LDA INV.SUB_MODULE.LOAD_FLAGS+$6 ;($00 = sub-module not loaded in memory | $01 = sub-module loaded in memory)
	BNE .EXIT_STEP
	
	LDA #$01
	STA INV.SUB_MODULE.LOAD_FLAGS+$6 ;($00 = sub-module not loaded in memory | $01 = sub-module loaded in memory)
		
		;set PARM.SEEK_BYTES
		lda #INV_6.READY_UNREADY.SEEK_BYTES		;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
		STA PARM.SEEK_BYTES+$0
		
		lda /INV_6.READY_UNREADY.SEEK_BYTES		;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
		STA PARM.SEEK_BYTES+$1				

		;set PARM.READ_BYTES
		lda #INV_6.READY_UNREADY.READ_BYTES		;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
		STA PARM.READ_BYTES+$0
		
		lda /INV_6.READY_UNREADY.READ_BYTES		;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
		STA PARM.READ_BYTES+$1				

		;set destination memory address
		lda #INV_6.READY_UNREADY.PATCH_START
		sta parm.ldrlo
		lda /INV_6.READY_UNREADY.PATCH_START
		sta parm.ldrhi	
	;JSR PRODOS.IO.RESET.FILE_OFFSET
	JSR PRODOS.IO.READ_CURRENT_FILE

.EXIT_STEP		
	JMP .EXIT
@END

.SUB_MODULE.07 ;INV_7.CALC.STATS
@START
	;is sub_module already in memory?
	LDA INV.SUB_MODULE.LOAD_FLAGS+$7 ;($00 = sub-module not loaded in memory | $01 = sub-module loaded in memory)
	BNE .EXIT_STEP
	
	LDA #$01
	STA INV.SUB_MODULE.LOAD_FLAGS+$7 ;($00 = sub-module not loaded in memory | $01 = sub-module loaded in memory)

		;set PARM.SEEK_BYTES
		lda #INV_7.CALCULATE.COMBAT.STATS.SEEK_BYTES		;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
		STA PARM.SEEK_BYTES+$0
		
		lda /INV_7.CALCULATE.COMBAT.STATS.SEEK_BYTES	;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
		STA PARM.SEEK_BYTES+$1				

		;set PARM.READ_BYTES
		lda #INV_7.CALCULATE.COMBAT.STATS.READ_BYTES		;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
		STA PARM.READ_BYTES+$0
		
		lda /INV_7.CALCULATE.COMBAT.STATS.READ_BYTES	;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
		STA PARM.READ_BYTES+$1				

		;set destination memory address
		lda #INV_7.CALCULATE.COMBAT.STATS.PATCH_START
		sta parm.ldrlo
		lda /INV_7.CALCULATE.COMBAT.STATS.PATCH_START
		sta parm.ldrhi	
	;JSR PRODOS.IO.RESET.FILE_OFFSET
	JSR PRODOS.IO.READ_CURRENT_FILE

		
	JMP .EXIT
@END


.EXIT

	RTS
	


@END

INV.REFRESH.ACTIVE_PLAYER.DATA	;in-memory arrays
@START
;PARAMETERS: INV.ACTIVE_PLAYER
;ENTRANCE: any inventory sub_module except INV_0 (stats: screen0) when the local copy in combat module is launched

;Note: reads character sheet record (from in-memory array) and readied equipment record (from in-memory array)

;READ ACTIVE PC CHARACTER SHEET DATA
		LDA INV.ACTIVE_PLAYER ;parm: player sequential # (high-bit not set = read mode)
	JSR COMBAT.READ_WRITE.CHR_SHEET.PC
		;RETURN VALUE = CHR_SHEET.RECORD.READ
	
;READ READIED EQUIPMENT TABLE RECORD (for active player)
	;***WARNING: Assumes file is open. add file-open check to this routine
		LDA INV.ACTIVE_PLAYER ;ACC = player sequential # (high-bit not set = read mode)
	JSR INV.READ_WRITE_RECORD.CHAR_SHEET.READIED
		;RETURN VALUE: CHR_SHEET.PC.READIED_EQUIP.RECORD.READ($10)
		
	RTS


.TEST.THIS_SECTION_NOT_NEEDED	
;is this needed? (as of 7/1/2017 11.42, it's commented out to see if a problem occurs without the code active)
@start
	
.SCREEN_TILE
@START	
		LDA #INV_1.SCREEN1.TITLE.HTAB
		STA HTAB	
		LDA #INV_1.SCREEN1.TITLE.VTAB	
		STA VTAB
	JSR	UPDATE.CHAR.POS

		LDA #INV_1.SCREEN1_TITLE
		STA STRING+$0
		LDA /INV_1.SCREEN1_TITLE
		STA STRING+$1
	JSR PRINT.STR
@END



.LH.WEAPON.NAME	
@START
;UPDATE SCREEN POSITION
	JSR INV.TEXT_TOOLS.VTAB2	;;increment VTAB by 2, reset HTAB to #$01, and update screen position

;PRINT NAME
		LDA #CHR_SHEET.PC.NAME.READIED_WP_LHAND				
		STA STRING+$0		
		LDA /CHR_SHEET.PC.NAME.READIED_WP_LHAND
		STA STRING+$1
	JSR PRINT.STR
		
@END

.RH.WEAPON.NAME	
@START
;UPDATE SCREEN POSITION
	JSR INV.TEXT_TOOLS.VTAB1 ;increment VTAB by 1, reset HTAB to #$01, and update screen position

;PRINT NAME
		LDA #CHR_SHEET.PC.NAME.READIED_WP_RHAND				
		STA STRING+$0		
		LDA /CHR_SHEET.PC.NAME.READIED_WP_RHAND
		STA STRING+$1
	JSR PRINT.STR
		
@END


.ALL_OTHER_ITEMS
@START
	LDX #CHR_SHEET.READIED_EQUIP.TYPE.HEAD.OFFSET ;the left/right hand weapons names are already printed, so the index is started at the record after. 
.LOOP.PRINT.ITEM_NAME


.VALIDATE.ITERATION
;IS SKIN READIED TO THIS EQUIPMENT SLOT?
;(if yes, skip. Otherwise skin could print out multiple times; once for each "empty" armor equipment slot)
	INX ;advance to byte $01 of readied item record (item_ID)

	;ITEM_type isn't checked because it can be assumed to be type armor since these slots are only permitted to have armor readied. 
	;there would be a corner case with item_ID $00 in the misc_items table, except that slot is used by a spell and spells don't get readied 
	;via the readied equipment table.
	LDA CHR_SHEET.PC.READIED_EQUIP.RECORD.READ,X ;load item_ID
	BEQ .INCREMENT_INDEX

;UPDATE SCREEN POSITION
	JSR INV.TEXT_TOOLS.VTAB1 ;increment VTAB by 1, reset HTAB to #$01, and update screen position

;READ ITEM NAME
;(from master item table)

		DEX ;back up to byte $00 of readied item record (item_type)

	
		LDA CHR_SHEET.PC.READIED_EQUIP.RECORD.READ,X ;load item_type
		PHA ;save item type
		INX ;advance to byte $01 of readied item record (item_ID)
		LDA CHR_SHEET.PC.READIED_EQUIP.RECORD.READ,X ;load item_ID
		STA FILE.ITEM_TABLE.ID
		PLA ;restore item_type	
		;ACC = item table code ($00 = weapon | $01 armor | $02 misc item)
	JSR FILE.READ.ITEM_TABLES.ENTRANCE
		;RETURN: FILE.ITEM_TABLE.RECORD.READ($20)

;PRINT NAME
		LDA #INV.ITEM_TABLE.ALL.NAME		
		STA STRING+$0		
		LDA /INV.ITEM_TABLE.ALL.NAME
		STA STRING+$1
	JSR PRINT.STR	
	
;INCREMENT INDEX
.INCREMENT_INDEX	
	INX ;next readied item record
	CPX #CHR_SHEET.PC.READIED_EQUIP.RECORD.SIZE
	BNE .LOOP.PRINT.ITEM_NAME
	
	
@END


.FOOTER
@START
		LDA #$B2 ;ASCII code for current screen number: 2
	JSR INV.TEXT_TOOLS.PRINT_FOOTER

@END


.EXIT

	JMP INV.STATS.STATE_LOOP
	
@end
	


@END

;graphics
INV.ERASE.TEXT_WINDOW
@START	
;PARAMETERS: INV.ERASE.START_LINE
;ENTRANCE: direct
;RETURN: updated hi-res screen

;NOTE: This subroutine only takes one parameter, the start line of the erase. The rest of the
;dimensions of the erase rectangle are fixed. This is because there are a variety of scenarios
;in the inventory module which require an erase of the space where text goes, where the start
;line is the only difference. 

;DRAW BORDER: LEFT TEXT WINDOW

	LDA INV.ERASE.START_LINE		
	STA DRAW.START_LINE
	
	LDA #TWS.LW.INVENTORY.LEFT_SBYTE
	STA DRAW.START_BYTE
	
	LDA #TWS.LW.INVENTORY.RIGHT_SBYTE
	STA DRAW.STOP_BYTE
		
	LDA #TWS.LW.INVENTORY.BOTTOM_LINE
	STA DRAW.STOP_LINE

.DRAW.RECTANGLE
		LDA PAGE.BACKGROUND
		CMP #$02
		BEQ .DRAW.PAGE2
.DRAW.PAGE1
		ORA #$81 ;set DRAW.LINE flags (use hi-res page1). (set bit 0 & 1 contain the hi-res page value, and set bit7 to enable erase mode. The border edge flag and byte values are ignored when erase mode is set.)
		;ORA #$105 ;set DRAW.LINE flags (use hi-res page1). (set bit 0 & 1 contain the hi-res page value, and set bit7 to enable erase mode. The border edge flag and byte values are ignored when erase mode is set.)
		JMP .CALL.DRAW.LINE
.DRAW.PAGE2
		ORA #$82 ;set DRAW.LINE flags (use hi-res page1). (set bit 0 & 1 contain the hi-res page value, and set bit7 to enable erase mode. The border edge flag and byte values are ignored when erase mode is set.)
		;ORA #$106 ;set DRAW.LINE flags (use hi-res page2). (set bit 0 & 1 contain the hi-res page value, and set bit7 to enable erase mode. The border edge flag and byte values are ignored when erase mode is set.)
	
	;**FALLS THROUGH**
	
.CALL.DRAW.LINE
		;ACC: flag settings
	JSR DRAW.LINE

.EXIT
	
	RTS
@END

INV.DRAW.MENU.ICON
@START
;PARAMETERS: SCREEN.DRAW.CURRENT_BYTE, SHAPE(2)
;ENTRANCE: direct
;RETURN: updated hi-res pages (both)

		LDA #$08
		STA TILE.LINE
		STA TILE.LINE.START
	JSR DRAW.TILE ;draw to background page

		LDA #$01
		STA PAGE.FOREGROUND.OVERRIDE
	JSR DRAW.TILE ;draw to foreground page
	
	RTS

@END

INV.DRAW_ERASE.INVENTORY_WINDOW
@START
			; lda #$aa
			; LDx INV.SUB_MODULE.LAUNCH_CODE ;load sub_module launch code
			; ldy #INV.LOAD_FLAG.CALC.COMBAT.STATS
			; jsr prep.brk
			; brk
			
;DRAW BORDER: LEFT TEXT WINDOW
@START
	LDA #TWB.LW.INVENTORY.TOP_LINE		;load line in x register	
	STA DRAW.START_LINE
	
	LDA #TWB.LW.INVENTORY.LEFT_SBYTE
	STA DRAW.START_BYTE
	
	LDA #TWB.LW.INVENTORY.RIGHT_SBYTE
	STA DRAW.STOP_BYTE
		
	LDA #TWB.LW.INVENTORY.BOTTOM_LINE
	STA DRAW.STOP_LINE

	LDA #$D5
	STA DRAW.BYTE_VALUE.HORIZONTAL+$0
	
	LDA #$AA
	STA DRAW.BYTE_VALUE.HORIZONTAL+$1
	
	LDA #$81
	;LDA #$84
	STA DRAW.BYTE_VALUE.VERTICLE+$0

	LDA #$A0
	;LDA #$88
	STA DRAW.BYTE_VALUE.VERTICLE+$1

.DRAW.RECTANGLE
		LDA PAGE.BACKGROUND
		ORA #$7C ;set DRAW.LINE flags
	JSR DRAW.LINE
	
	JSR FLIP.PAGE
					
		LDA PAGE.BACKGROUND
		ORA #$7C ;set DRAW.LINE flags
	JSR DRAW.LINE
	


			
@END

;DRAW LINE: BELOW ICONS
@START

	LDA #TWB.LW.INVENTORY.SEPERATOR_LINE							;load line in x register	
	STA DRAW.START_LINE
	
	LDA #TWB.LW.INVENTORY.LEFT_SBYTE+1
	STA DRAW.START_BYTE
	
	LDA #TWB.LW.INVENTORY.RIGHT_SBYTE-1
	STA DRAW.STOP_BYTE
		
	LDA #TWB.LW.INVENTORY.SEPERATOR_LINE+1 ;(set to last line to draw + 1)
	STA DRAW.STOP_LINE
	
	LDA #$AA
	STA DRAW.BYTE_VALUE.HORIZONTAL+$0
	
	LDA #$D5
	STA DRAW.BYTE_VALUE.HORIZONTAL+$1
	
.DRAW.LINE
;(when both hi-res pages need to stay in sync, instead of drawing to both pages, sometimes it is best to the pages individually
;and flip pages inbetween. This way the draw isn't as noticable to the user)
		
					; LDA #$01
					; STA TROUBLESHOOTING.HOOK
					
		LDA #$07 ;BIT0-2 set: use both hi-res pages. draw top line of rectangle. ignore all other bit flags.
	JSR DRAW.LINE



@END

;DRAW MENU ICONS
@START
;DRAW MENU ICON_0
		LDA #INV.MENU_ICON0.SBYTE
		STA SCREEN.DRAW.CURRENT_BYTE
		
		LDA	#MENU_ICON0.STATS
		STA SHAPE+$0
		LDA /MENU_ICON0.STATS
		STA SHAPE+$1
	JSR INV.DRAW.MENU.ICON

;DRAW MENU ICON_1
		LDA #INV.MENU_ICON1.SBYTE
		STA SCREEN.DRAW.CURRENT_BYTE
		
		LDA	#MENU_ICON1.WEAPONS
		STA SHAPE+$0
		LDA /MENU_ICON1.WEAPONS
		STA SHAPE+$1
	JSR INV.DRAW.MENU.ICON

;DRAW MENU ICON_2
		LDA #INV.MENU_ICON2.SBYTE
		STA SCREEN.DRAW.CURRENT_BYTE
		
		LDA	#MENU_ICON2.ARMOR
		STA SHAPE+$0
		LDA /MENU_ICON2.ARMOR
		STA SHAPE+$1
	JSR INV.DRAW.MENU.ICON


;DRAW MENU ICON_3
		LDA #INV.MENU_ICON3.SBYTE
		STA SCREEN.DRAW.CURRENT_BYTE
		
		LDA	#MENU_ICON3.MISC_ITEMS
		STA SHAPE+$0
		LDA /MENU_ICON3.MISC_ITEMS
		STA SHAPE+$1
	JSR INV.DRAW.MENU.ICON	

;DRAW MENU ICON_4
		LDA #INV.MENU_ICON4.SBYTE
		STA SCREEN.DRAW.CURRENT_BYTE
		
		LDA	#MENU_ICON4.SPELLS
		STA SHAPE+$0
		LDA /MENU_ICON4.SPELLS
		STA SHAPE+$1
	JSR INV.DRAW.MENU.ICON	
	
;DRAW MENU ICON_5
		LDA #INV.MENU_ICON5.SBYTE
		STA SCREEN.DRAW.CURRENT_BYTE
		
		LDA	#MENU_ICON5.GAME_SETTINGS
		STA SHAPE+$0
		LDA /MENU_ICON5.GAME_SETTINGS
		STA SHAPE+$1
	JSR INV.DRAW.MENU.ICON	
	
@END


.EXIT
	RTS
@END

INV.ERASE.INVENTORY_WINDOW.TEXT_SPACE ;foreground and background page /w flip
@START
	
		LDA #TWS.LW.INVENTORY.TOP_LINE.TEXT_SPACE
		STA INV.ERASE.START_LINE
	JSR INV.ERASE.TEXT_WINDOW
	JSR FLIP.PAGE
		LDA #TWS.LW.INVENTORY.TOP_LINE.TEXT_SPACE
		STA INV.ERASE.START_LINE
	JSR INV.ERASE.TEXT_WINDOW
	JSR FLIP.PAGE

.EXIT
	RTS
	
@END

	
;text
INV.TEXT_TOOLS.VTAB2 ;;increment VTAB by 2, reset HTAB to #$01, and update screen position
@START
;PARAMETERS: none
;ENTRANCE: any inventory sub_module
;RETURN: updated screen position


		INC VTAB
INV.TEXT_TOOLS.VTAB1 ;ALTERNATE ENTRANCE: increment VTAB by 1 and update screen position
		LDA #$01
		STA HTAB
		INC VTAB
	JSR	UPDATE.CHAR.POS	

	RTS
@END

INV.TEXT_TOOLS.PRINT_FOOTER
@START
;PARAMETERS: ACC = ASCII code of the current screen number (i.e for screen1 use $B1)
;ENTRANCE: any inventory sub_module except screen0 when it's local copy is loaded via combat module
;RETURN: text output to video screen, both pages. 


;SAVE PARAMETERS

	;PARM: ACC = ASCII code of the current screen number (i.e for screen1 use $B1)
	STA INV.TEXT_BLOCK.FOOTER+$7 ;embedd screen number in footer text block
		
;UPDATE SCREEN POSITION
		LDA #INV.SCREEN.FOOTER.HTAB
		STA HTAB	
		LDA #INV.SCREEN.FOOTER.VTAB
		STA VTAB	
	JSR	UPDATE.CHAR.POS
	

;PRINT FOOTER	
		LDA #INV.TEXT_BLOCK.FOOTER
		STA STRING+$0
		LDA /INV.TEXT_BLOCK.FOOTER
		STA STRING+$1
	JSR PRINT.STR	

.EXIT
	
	RTS
	
@END




;other
INV.GET.READIED.QTY ;returns the number of times an item is readied
@START
;PARAMETERS: PLAYER.INVENTORY.DATA.POINTER
;ENTRANCE: INV.READY_EQUIPMENT, from the UI
;RETURN: INV.RE.READIED.QTY (the number of times the ITEM-TO-READY is already readied by all PCs, including active PC)


;SAVE REGISTERS
	TXA
	PHA
	TYA
	PHA
	
.INIT	
	LDA #$00
	STA INV.RE.READIED.QTY
	
	
	LDY #$03 ;set index to byte $03, Readied Flags field #1 of the inventory record for the ITEM-TO-READY
	LDX #$01 ;set INV.READIED_FLAGS.AND_MASK index to sequential PC #1. 		
.LOOP.COUNT.READIED_INSTANCES
;.TEST.READIED.FLAGS1	
	LDA (PLAYER.INVENTORY.DATA.POINTER),Y ;load Readied Equipment Flags #1
	AND INV.READIED_FLAGS.BIT_TEST_MASK,X ;test bit flag for active PC
	BEQ .TEST.READIED.FLAGS1.DONE ;branch if bit not set (PC for this iteration does't have item readied)	
	;PC has item readied via flag #1
	INC INV.RE.READIED.QTY
.TEST.READIED.FLAGS1.DONE

			
.TEST.READIED.FLAGS2
	INY ;advance index to byte $04, Readied Flags field #2 of the inventory record for the ITEM-TO-READY
	LDA (PLAYER.INVENTORY.DATA.POINTER),Y ;load Readied Equipment Flags #1
	AND INV.READIED_FLAGS.BIT_TEST_MASK,X ;test bit flag for active PC
	BEQ .NEXT_PC ;branch if bit not set (PC for this iteration does't have item readied)	
	INC INV.RE.READIED.QTY	
.NEXT_PC


			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; CPX #$03
			; BNE .TEMP
			; STY $BE00
			; STX $BE01
			; LDA (PLAYER.INVENTORY.DATA.POINTER),Y ;load Readied Equipment Flags #1
			; STA $BE02
			; LDA INV.READIED_FLAGS.BIT_TEST_MASK,X ;test bit flag for active player	
			; STA $BE03

			; LDA #$BB
			; LDX INV.RE.READIED.QTY
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP
			
			
	INX 
	DEY ;back up index to byte $03, Readied Flags field #1 of the inventory record for the ITEM-TO-READY
	CPX #COMBAT.MAX.PC+1 ;+1 because the loop counter starts with $1 (sequential PC#) instead of $0
	BNE .LOOP.COUNT.READIED_INSTANCES



			
			
;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX

.EXIT
	
	RTS
@END

;INV.DRAW.SHAPE ;			;=============DRAW A SINGLE TILE SIZED SHAPE AT THE LOCATION SPECIFIED======
@START
; ;PARAMETERS:  TILE.LINE, *TILE.DEPTH, SCREEN.DRAW.CURRENT_BYTE
; ;RETURN: NONE
; ;ENTRANCE: direct
; ;*Set to default value on game launch. When modified (i.e. tall grass), it is reset to default when done.

; ;=====================SUBROUTINE DOCUMENTATION====================================
; ;This subroutine is based on DRAW.TILE. The purpose of this version is to support 

; ;This subroutine is the lowest level routine associated with tile graphics plotting. 
; ;It is not designed for direct entrance. Management of all graphics screen variables
; ;is required by the calling routine. This doesn't setup any start values and it only increments
; ;screen byte and line in the context of a single tile (i.e. it assumes the calling routine will increment or reinitialize screen byte and line if another tile is to be plotted)
; ;
; ;This routie uses lookup tables to obtain the graphic screen memory address associated with
; ;each line of the tile to be drawn. This is done in a loop, in which both screen bytes of the tile are drawn
; ;on each iteration. Tiles are drawn top to bottom.  
; ;=================================================================================



; ;**OPT** Speed. Include an in-line code version of DRAW.TILE with DRAW.COLUMN and DRAW.ROW to avoid the costly JSR/RTS to call as a subroutine. 

; .SAVE.REGISTERS
	; TXA
	; PHA
	; TYA
	; PHA

; .START

	; LDX TILE.LINE			;LOAD LINE IN X REGISTER	
	; LDA #$00
	; STA SHP.TBL.CNTR		;START DRAWING AT BEGINNING OF SHAPE TABLE


	; STX TILE.LINE.START		;THE STARTING LINE OF THE TILES IN THE CURRENT ROW
	; TXA
	; CLC	
	; ADC TILE.DEPTH			;ADD DEPTH OF SHAPE TO STARTING LINE IN HEX (# of lines, not the position of last line....so line positions $0-$F is $10 (!16) total lines)
	; STA TILE.LINE.STOP		;SET THE LAST LINE TO DRAW BASED ON THE DEPTH OF THE TILE

; .DRAW.LOOP
		; LDA PAGE.BACKGROUND
	; JSR GET.LINE.ADDRESS1
		; ;RETURN VALUE: LINE.BASE.ADDR1(2)

	; LDY SHP.TBL.CNTR		;LOAD Y WITH SHAPE TABLE COUNTER
	; LDA (SHAPE),Y			;LOAD SHAPE BYTE (1st screen byte of the tile)
	
	; LDY SCREEN.DRAW.CURRENT_BYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	; STA (LINE.BASE.ADDR1),Y	;PLOT (1st screen byte)
	
	; INC SHP.TBL.CNTR		;NEXT SHAPE BYTE, NEXT TILE LINE
	; INC SCREEN.DRAW.CURRENT_BYTE			;SWITCH TO 2ND SCREEN BYTE IN THE TILE
	
	; LDY SHP.TBL.CNTR		;LOAD Y WITH SHAPE TABLE COUNTER
	; LDA (SHAPE),Y			;LOAD SHAPE BYTE (2nd screen byte of the tile)
	; LDY SCREEN.DRAW.CURRENT_BYTE			;SCREEN BYTE INDEX IN Y REGISTER	
	; STA (LINE.BASE.ADDR1),Y				;PLOT (2st screen byte)
	
	; DEC SCREEN.DRAW.CURRENT_BYTE			;SWITCH BACK TO 1ST SCREEN BYTE IN THE TILE	;**OPT** if the counters are updated after the CMP/branch it might save 1 INC SCREEN.DRAW.CURRENT_BYTE instruction in the main loop because SCREEN.DRAW.CURRENT_BYTE will be in 2nd position when this loop ends. 
	; INX						;NEXT TILE LINE
	; INC SHP.TBL.CNTR		;NEXT SHAPE BYTE
	
	; CPX TILE.LINE.STOP		;IS TILE DONE?							
	; BCC .DRAW.LOOP			;IF NO, DRAW NEXT LINE (BCC: is X-REG < CPX value)	

		
			
; .RESTORE.REGISTERS
	; PLA
	; TAY
	; PLA
	; TAX
	
	; RTS

@END	

INV.NEXT.MENU
@START
;PARAMETERS: ACC = ($00 = next menu | $1-6 = hotkey to specific menu)
;ENTRANCE; direct from any inventory sub_module
;RETURN: updated INV.ACTIVE_MENU.CODE

;=====================SUBROUTINE DOCUMENTATION===========================================================================
;
;Manages the TABing between menu icons, and menu icon hotkeys (1-6) 
;
;=============================================================================================================================



			;STA TEMP
.INIT
	PHA ;save ACC parm
	


;****INV DEBUG LOG*** 			;**OPT** Memory. Remove.
@START
.DEBUG.LABEL
		;PHA
		LDA #.DEBUG.LABEL
		STA INV.DEBUG.ADDRESS+$0
		LDA /.DEBUG.LABEL
		STA INV.DEBUG.ADDRESS+$1
	JSR INV.DEBUG.LOG.WRITE
		;PLA
@END

	
	LDA INV.ACTIVE_MENU.CODE
	STA INV.ACTIVE_MENU.CODE.LAST	;Saves the value of INV.ACTIVE_MENU.CODE just before it's incremented/decremented.

	
	; ;set active 
	; LDA #$00
	; STA INV.ACTIVE_STATS_SCREEN.CODE ;($00 = stats: screen0 - stats summary)

	
.PARSE.PARAMETERS
	PLA ;restore ACC parm
	
	;PARM: ACC = (high-bit not set = tab pressed, increment menu | $B1-B6 = ASCII code to hotkey to specific menu)
	BPL	.PARSE.PARAMETERS.DONE ;tab was pressed 
	;use hotkey
	AND #$0F ;mask-out HO nibble (strips off the $B, leaving a value of $1-6)
	SEC
	SBC #$01 ;convert ACC parm to INV.ACTIVE_MENU.CODE format, which is $0-5 instead of $1-6			
	JMP .BRANCH.ACTIVE_MENU.ALT ;use the same branch setup as when tab is pressed, but enter after the LDA so that the ACC parm -1 is used by the branches
.PARSE.PARAMETERS.DONE
	
	
.INCREMENT.MENU	
	LDA INV.ACTIVE_MENU.CODE 
	CMP #INV.ACTIVE_MENU.CODE.MAX
	BEQ .AT.LAST.MENU
	INC INV.ACTIVE_MENU.CODE
	JMP .BRANCH.ACTIVE_MENU
.AT.LAST.MENU ;last menu icon on the right
	LDA #$00 ;reset to 1st menu
	STA INV.ACTIVE_MENU.CODE
	
	;**FALLS THROUGH**

.BRANCH.ACTIVE_MENU
	LDA INV.ACTIVE_MENU.CODE
.BRANCH.ACTIVE_MENU.ALT
	STA INV.ACTIVE_MENU.CODE ;update menu code in case if hotkey was used, the value of which would be in the ACC as a result of the use of the alternate entrance. 
	;did player just leave INV_4 (displays weapons/armor/misc_items/spells)?
	CMP #INV.ACTIVE_MENU.WEAPONS
	BCC .TEST.LAST.MENU ;new menu is not in INV_4. Test last menu. 
	CMP #INV.ACTIVE_MENU.GAME_SETTINGS ;new menu is in INV_4. no need to test further
	BCC .EXECUTE.BRANCH
.TEST.LAST.MENU
	LDA INV.ACTIVE_MENU.CODE.LAST
	CMP #INV.ACTIVE_MENU.WEAPONS ;new menu and last menu are not in INV_4. CALCULATE.COMBAT.STATS update not needed
	BCC .EXECUTE.BRANCH
	CMP #INV.ACTIVE_MENU.GAME_SETTINGS ;new menu last menu are not in INV_4. CALCULATE.COMBAT.STATS update not needed
	BCS .EXECUTE.BRANCH	
	;player just left INV_4. Check if any ready/unreadies occured and if so update combat stats
	JSR INV.RUN.CALCULATE.COMBAT.STATS.IF_CHANGES_OCUURED
	LDA INV.ACTIVE_MENU.CODE
	
	;**FALLS THROUGH**
	
.EXECUTE.BRANCH	

;****INV DEBUG LOG*** 			;**OPT** Memory. Remove.
@START
.DEBUG.LABEL1
		PHA
		LDA #.DEBUG.LABEL1
		STA INV.DEBUG.ADDRESS+$0
		LDA /.DEBUG.LABEL1
		STA INV.DEBUG.ADDRESS+$1
	JSR INV.DEBUG.LOG.WRITE
		PLA
@END

		PHA ;save ACC (contains menu code)
	JSR INV.DRAW.MENU_ICON.SELECTOR
		PLA ;restore ACC (contains menu code)
	BEQ .MENU0.STATS
	CMP #INV.ACTIVE_MENU.WEAPONS
	BEQ .MENU1.DISPLAY.WEAPONS
	CMP #INV.ACTIVE_MENU.ARMOR
	BEQ .MENU2.DISPLAY.ARMOR
	CMP #INV.ACTIVE_MENU.MISC_ITEMS
	BEQ .MENU3.DISPLAY.MISC_ITEMS
	CMP #INV.ACTIVE_MENU.SPELLS
	BEQ .MENU4.DISPLAY.SPELLS
	CMP #INV.ACTIVE_MENU.GAME_SETTINGS
	BEQ .MENU5.GAME_SETTINGS



.ERROR.INVALID.LAUNCH
;.BRANCH.ACTIVE_MENU (INV.NEXT.MENU) reports unexpected active menu code (should be $0-$05)
	JSR PREP.BRK
	BRK

	
	;******THE PLAN: in the branch sections, call the load submodule routine and then JSR to the entrance
	;the submodule routine will handle the "is-loaded-already" check.
	;each menu will need to init and track a screen_code and manage the associated sub_module loads 
.MENU0.STATS
		; ;LDA #$00	;inventory sub-module # to launch)
		; LDA INV.ACTIVE_STATS_SCREEN.CODE		
	; JMP INV.READ_LAUNCH.SUB_MODULE
	
	JMP INV.STATS.LAUNCH.SCREEN
	
.MENU1.DISPLAY.WEAPONS
;****INV DEBUG LOG*** 			;**OPT** Memory. Remove.
@START
.DEBUG.LABEL2
		;PHA
		LDA #.DEBUG.LABEL2
		STA INV.DEBUG.ADDRESS+$0
		LDA /.DEBUG.LABEL2
		STA INV.DEBUG.ADDRESS+$1
	JSR INV.DEBUG.LOG.WRITE
		;PLA
@END

		LDA #$00 ;set weapons mode
		STA INV_4.DISPLAY.MODE ;($00 = display weapons | $01 = display armor | $02 = misc items)	 

				; lda #$01
				; sta troubleshooting.hook
				
			
;LOAD SUB_MODULE FROM DISK
		LDA #$04	;inventory sub-module # to launch)
	JMP INV.READ_LAUNCH.SUB_MODULE

.MENU2.DISPLAY.ARMOR
		LDA #$01 ;set armor mode
		STA INV_4.DISPLAY.MODE ;($00 = display weapons | $01 = display armor | $02 = misc items)	 
	;JMP INV_4.DISPLAY_INVENTORY
	
;LOAD SUB_MODULE FROM DISK
		LDA #$04	;inventory sub-module # to launch)
	JMP INV.READ_LAUNCH.SUB_MODULE
	
.MENU3.DISPLAY.MISC_ITEMS
		LDA #$02 ;set misc items mode
		STA INV_4.DISPLAY.MODE ;($00 = display weapons | $01 = display armor | $02 = misc items)	 
	;JMP INV_4.DISPLAY_INVENTORY
	
;LOAD SUB_MODULE FROM DISK
		LDA #$04	;inventory sub-module # to launch)
	JMP INV.READ_LAUNCH.SUB_MODULE
	
.MENU4.DISPLAY.SPELLS 
;*****ADD CODE: need to add another parm to detect spell mode. Spells will be detected as an item_ID range in the misc items table


		LDA #$02 ;set misc items mode
		STA INV_4.DISPLAY.MODE ;($00 = display weapons | $01 = display armor | $02 = misc items)	 
	;JMP INV_4.DISPLAY_INVENTORY
	
;LOAD SUB_MODULE FROM DISK
		LDA #$04	;inventory sub-module # to launch)
	JMP INV.READ_LAUNCH.SUB_MODULE	
	
.MENU5.GAME_SETTINGS
			
			; JSR KEYIN
			; ; LDA #$89
			; ; STA $C000 ;seed TAB press so that any keypress results in next menu
			
	; ;ADD CODE: for game setings menu. The JMP below causes this menu to be skipped for now. 
	
		; LDA #$00
	; JMP INV.NEXT.MENU		
	
@END

INV.STATS.STATE_LOOP
@START
;PARAMETERS: NONE
;ENTRNACE: INV_0 - INV_3 (the 4 stats screens)


;****INV DEBUG LOG*** 			;**OPT** Memory. Remove.
@START
.DEBUG.LABEL
		;PHA
		LDA #.DEBUG.LABEL
		STA INV.DEBUG.ADDRESS+$0
		LDA /.DEBUG.LABEL
		STA INV.DEBUG.ADDRESS+$1
	JSR INV.DEBUG.LOG.WRITE
		;PLA
@END
	
	
	
.KEYIN.LOOP
	LDA $C000
    BPL .KEYIN.LOOP
	STA $C010               ;CLR LAST KEY
	
		;;ACC = keypress
	;JSR CONVERT.ASCII.UCASE
		;;RETURN VALUE: ACC = ASCII code (upper case, if the code is a letter)

.HOTKEY.CHECK ;test for keypress of 1-6
	CMP #$B1			
	BCC .HOTKEY.CHECK.DONE
	CMP #$B7
	BCC .CHANGE.MENU ;exit, then goto the menu associated with the hotkey
.HOTKEY.CHECK.DONE	

	CMP #$88			;LEFT ARROW
	BEQ	INV.STATS.NEXT_SCREEN
	CMP #$95			;RIGHT ARROW
	BEQ INV.STATS.NEXT_SCREEN
	CMP #$8B			;UP ARROW
	BEQ .CHANGE.ACTIVE_PLAYER.UP
	CMP #$8A			;DOWN ARROW
	BEQ .CHANGE.ACTIVE_PLAYER.DOWN	
		
	CMP #$89			;TAB (next menu)
	BEQ .CHANGE.MENU

	CMP #$9B			;ESC (EXIT COMBAT)
	BEQ	.INV.INVENTORY_MODULE.EXIT.STEP

	JMP .KEYIN.LOOP

.CHANGE.MENU	
.SET.MENU_CODE
	;ACC = ASCII code of keypress
	CMP #$89
	BNE .SET.MENU_CODE.DONE ;if tab wasn't pressed, that means a hotkey was pressed and the hotkey ASCII code ($B1-$B6) is the parm to INV.NEXT.MENU
	LDA #$00 ; load "tab pressed" code for INV.NEXT.MENU
.SET.MENU_CODE.DONE


.NEXT_MENU
;****INV DEBUG LOG*** 			;**OPT** Memory. Remove.
@START
.DEBUG.LABEL3
		;PHA
		LDA #.DEBUG.LABEL3
		STA INV.DEBUG.ADDRESS+$0
		LDA /.DEBUG.LABEL3
		STA INV.DEBUG.ADDRESS+$1
	JSR INV.DEBUG.LOG.WRITE
		;PLA
@END
	
		;PARM: ACC = (high-bit not set = tab pressed, increment menu | $B1-B6 = ASCII code to hotkey to specific menu) 
	JMP INV.NEXT.MENU

.CHANGE.ACTIVE_PLAYER.UP	
	JSR CHARACTER.ROSTER.INCREMENT_UP
	JMP INV.STATS.LAUNCH.SCREEN
	
.CHANGE.ACTIVE_PLAYER.DOWN
	JSR CHARACTER.ROSTER.INCREMENT_DOWN
	JMP INV.STATS.LAUNCH.SCREEN	

.INV.INVENTORY_MODULE.EXIT.STEP
	JMP INV.INVENTORY_MODULE.EXIT
	
@END

INV.STATS.NEXT_SCREEN
@START
;PARAMETERS: ACC = ($00 = 
;ENTRANCE; direct from any inventory sub_module
;RETURN: updated INV.ACTIVE_MENU.CODE

;=====================SUBROUTINE DOCUMENTATION===========================================================================
;
;Manages the TABing between menu icons, and menu icon hotkeys (1-6) 
;
;=============================================================================================================================

.PARSE.PARAMETERS
	;ACC = PARM: ($88 = left arrow pressed in state loop | $95 = left arrow pressed in state loop)
	CMP #$88	;ASCII: left arrow
	BEQ .INCREMENT_LEFT
	; CMP #$95	;ASCII: right arrow
	; BEQ .INCREMENT_RIGHT
	
	;default case: assume right arrow was pressed
	
	;**FALLS THROUGH** 

	
; .PARSE.PARAMETERS
	; ;PARM: ACC = (high-bit not set = tab pressed, increment menu | $B1-B6 = ASCII code to hotkey to specific menu)
	; BPL	.PARSE.PARAMETERS.DONE ;tab was pressed 
	; ;use hotkey
	; AND #$0F ;mask-out HO nibble (strips off the $B, leaving a value of $1-6)
	; SEC
	; SBC #$01 ;convert ACC parm to INV.ACTIVE_MENU.CODE format, which is $0-5 instead of $1-6			
	; JMP .BRANCH.ACTIVE_MENU.ALT ;use the same branch setup as when tab is pressed, but enter after the LDA so that the ACC parm -1 is used by the branches
; .PARSE.PARAMETERS.DONE
	
	
.INCREMENT_RIGHT
	LDA INV.ACTIVE_STATS_SCREEN.CODE 
	CMP #INV.ACTIVE_STATS_SCREEN.MAX
	BEQ .AT.LAST.SCREEN
	INC INV.ACTIVE_STATS_SCREEN.CODE
	JMP .INCREMENT.DONE
.AT.LAST.SCREEN 
	LDA #$00 ;reset to 1st screen
	JMP .SAVE.SCREEN_CODE
	
.INCREMENT_LEFT
	LDA INV.ACTIVE_STATS_SCREEN.CODE 
	BEQ .AT.FIRST.SCREEN
	DEC INV.ACTIVE_STATS_SCREEN.CODE
	JMP .INCREMENT.DONE
.AT.FIRST.SCREEN 
	LDA #INV.ACTIVE_STATS_SCREEN.MAX ;reset to last screen
.SAVE.SCREEN_CODE	
	STA INV.ACTIVE_STATS_SCREEN.CODE 
.INCREMENT.DONE


.LAUNCH_SCREEN

					; ldx #$00
					; jsr prep.brk
					; brk

				

INV.STATS.LAUNCH.SCREEN
;CLEAR INVENTORY TEXT SPACE
	JSR INV.ERASE.INVENTORY_WINDOW.TEXT_SPACE ;foreground and background page /w flip
				
		LDA INV.ACTIVE_STATS_SCREEN.CODE
		;ACC = inventory sub-module # to launch)
	JMP INV.READ_LAUNCH.SUB_MODULE

	
@END

INV.DRAW.MENU_ICON.SELECTOR
@START	
;PARAMTERS: INV.ACTIVE_MENU.CODE, INV.ACTIVE_MENU.CODE.LAST
;ENTRANCE: any inventory sub_module except stats screen0 when it's launched via the local copy in combat module

;FORMULA FOR CALCULATING SCREEN BYTES
;	  INV.MENU.SELECTOR_PILLAR1.SBYTE = INV.ACTIVE_MENU.CODE * 3
;	  INV.MENU.SELECTOR_PILLAR2.SBYTE = INV.MENU.SELECTOR_PILLAR1.SBYTE + 3
;	
;	  IF INV.MENU.SELECTOR_PILLAR1.SBYTE = $0F (game settings menu is active)
;		  then INV.MENU.SELECTOR_PILLAR1.SBYTE = INV.MENU.SELECTOR_PILLAR1.SBYTE+1
;			   INV.MENU.SELECTOR_PILLAR2.SBYTE = INV.MENU.SELECTOR_PILLAR2.SBYTE+1




				; LDA #$B0
			; JSR COUT


.ERASE.MENU_SELECTOR.LAST
@START

		LDA INV.ACTIVE_MENU.CODE.LAST
	JSR .CALCUALTE.SCREEN_BYTES ;calculate screen byte for each pillar of the menue selector

				; LDA #$B2
			; JSR COUT
			
					; lda INV.MENU.SELECTOR_PILLAR1.SBYTE 
					; sta $be00
					; lda INV.MENU.SELECTOR_PILLAR2.SBYTE
					; sta $be01
					; lda #$aa
					; jsr prep.brk
					; brk			
										
			; STA TEMP
			; LDA INV.ACTIVE_MENU.CODE.LAST
			; STA $BE00
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; JSR KEYIN
			; LDA #$AA
			; LDX INV.MENU.SELECTOR_PILLAR1.SBYTE 
			; LDY INV.MENU.SELECTOR_PILLAR2.SBYTE 
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP


			
		LDA #$00
		STA DRAW.BYTE_VALUE.VERTICLE+$0
		STA DRAW.BYTE_VALUE.VERTICLE+$1	
		;INV.MENU.SELECTOR_PILLAR1.SBYTE: already set above
		;INV.MENU.SELECTOR_PILLAR2.SBYTE: already set above	
	JSR .DRAW_ERASE.PILLARS




			
	
;FIX INVENTORY WINDOW BORDER
;(it might get clobbered during the erase of the 1st or 2nd pillar, depending on which menu is active)	
	
	;JSR INV.DRAW_ERASE.INVENTORY_WINDOW	
	
	;fix left border
		LDA #TWB.LW.INVENTORY.LEFT_SBYTE
		STA INV.MENU.SELECTOR_PILLAR1.SBYTE
		STA INV.MENU.SELECTOR_PILLAR2.SBYTE
		LDA #$81
		;LDA #$84
		STA DRAW.BYTE_VALUE.VERTICLE+$0
	JSR .DRAW_ERASE.PILLARS
	
	;fix right border
		LDA #TWB.LW.INVENTORY.RIGHT_SBYTE-1
		STA INV.MENU.SELECTOR_PILLAR1.SBYTE
		STA INV.MENU.SELECTOR_PILLAR2.SBYTE
		LDA #$A0
		;LDA #$88
		STA DRAW.BYTE_VALUE.VERTICLE+$0
	JSR .DRAW_ERASE.PILLARS	
	
	
@END

				; LDA #$B3
			; JSR COUT
			
.DRAW.MENU_SELECTOR.NEW
@START
		LDA INV.ACTIVE_MENU.CODE
	JSR .CALCUALTE.SCREEN_BYTES ;calculate screen byte for each pillar of the menu selector

				; LDA #$B4
			; JSR COUT


			
.SET.DRAW_BYTE.VALUE
@START
	;is screen byte odd or reven?
	LDA #$01 ;set AND mask
	BIT INV.MENU.SELECTOR_PILLAR1.SBYTE ;test bit0 of memory address. BIT ANDs the memory address value with the ACC and sets the zero flag based on the result. 
	BNE .PILLAR1.ODD.SCREEN_BYTES
.PILLAR1.EVEN.SCREEN_BYTES
	LDA #$84
	STA DRAW.BYTE_VALUE.VERTICLE+$0
	LDA #$88
	STA DRAW.BYTE_VALUE.VERTICLE+$1
	JMP .BYTE_VALUES.TENATIVE_SET
.PILLAR1.ODD.SCREEN_BYTES
	LDA #$88
	STA DRAW.BYTE_VALUE.VERTICLE+$0
	LDA #$84
	STA DRAW.BYTE_VALUE.VERTICLE+$1	
.BYTE_VALUES.TENATIVE_SET 




	;check corner cast (left edge: menu 1, stats)
	LDA INV.MENU.SELECTOR_PILLAR1.SBYTE
	BNE .LEFT_EDGE.CORNER_CASE.DONE
	LDA #$81 ;this is the byte value of the left edge border of the inventory window, one column off from where the menu selector pillars are normally drawn
	STA DRAW.BYTE_VALUE.VERTICLE+$0
.LEFT_EDGE.CORNER_CASE.DONE

	;check corner cast (right edge: menu 6, game stats)
	LDA INV.MENU.SELECTOR_PILLAR2.SBYTE
	CMP #$13
	BNE .RIGHT_EDGE.CORNER_CASE.DONE
	LDA #$A0 ;this is the byte value of the right edge border of the inventory window, one column off from where the menu selector pillars are normally drawn
	STA DRAW.BYTE_VALUE.VERTICLE+$1
.RIGHT_EDGE.CORNER_CASE.DONE

.SET.DRAW_BYTE.VALUE.DONE
@END


		;DRAW.BYTE_VALUE.VERTICLE+$0: already set above
		;DRAW.BYTE_VALUE.VERTICLE+$1: already set above	
		;INV.MENU.SELECTOR_PILLAR1.SBYTE: already set above
		;INV.MENU.SELECTOR_PILLAR2.SBYTE: already set above
	JSR .DRAW_ERASE.PILLARS

				; LDA #$B5
			; JSR COUT





			
@END


.EXIT
				; jsr keyin
				; LDA #$AB
				; JSR PREP.BRK
				; BRK

	RTS
	
.DRAW_ERASE.PILLARS
@START	
;PARAMETERS: INV.MENU.SELECTOR_PILLAR1.SBYTE,INV.MENU.SELECTOR_PILLAR2.SBYTE, DRAW.BYTE_VALUE.VERTICLE(2)

	LDY INV.MENU.SELECTOR_PILLAR1.SBYTE
	LDX #TWB.LW.INVENTORY.TOP_LINE+1
.LOOP.DRAW_ERASE
		LDA PAGE.BACKGROUND
	JSR GET.LINE.ADDRESS1

		LDA PAGE.FOREGROUND
	JSR GET.LINE.ADDRESS2

	LDA DRAW.BYTE_VALUE.VERTICLE+$0	;bit mapped byte
	STA (LINE.BASE.ADDR1),Y		;save bit mapped byte to video screen memory (foreground page)  
	STA (LINE.BASE.ADDR2),Y		;save bit mapped byte to video screen memory (background page)

	INX ;next line (down)
	
	CPX #TWB.LW.INVENTORY.SEPERATOR_LINE
	BNE .LOOP.DRAW_ERASE
	
.EXIT_TEST
	CPY INV.MENU.SELECTOR_PILLAR2.SBYTE ;is this 2nd iteration?
	BEQ .DRAW_ERASE.PILLARS.DONE ;branch, if yet
	
	;set screen byte and draw byte for 2nd pillar
	LDY INV.MENU.SELECTOR_PILLAR2.SBYTE
	LDA DRAW.BYTE_VALUE.VERTICLE+$1	;save byte for 2nd pillar into the variable used in the draw_erase loop
	STA DRAW.BYTE_VALUE.VERTICLE+$0	

	;reset hi-res line to top of pillars
	LDX #TWB.LW.INVENTORY.TOP_LINE+1
	JMP .LOOP.DRAW_ERASE
	
	
.DRAW_ERASE.PILLARS.DONE
	RTS
	
@END

.CALCUALTE.SCREEN_BYTES ;calculate screen byte for each pillar of the menue selector
@START
;PARAMETERS: ACC: menu code
;ENTRANCE: local
;RETURN: INV.MENU.SELECTOR_PILLAR1.SBYTE, INV.MENU.SELECTOR_PILLAR2.SBYTE


		;ACC: menu code
		; SEC
		; SBC #$01
		STA MULPLR ;INV.ACTIVE_MENU.CODE
		LDA #$00
		STA MULPLR+$1
		;
		LDA #$03
		STA MULCND
		LDA #$00
		STA MULCND+$01
	JSR MLP.16.NO_BCD
		;RETURN VALUE: result+$0 (product LO), result+$1 (product HO)
		
		LDA RESULT+$0
		STA INV.MENU.SELECTOR_PILLAR1.SBYTE
		CLC
		ADC #$03
		STA INV.MENU.SELECTOR_PILLAR2.SBYTE
		
	; LDA INV.ACTIVE_MENU.CODE
	; CMP #$06 ;is game settings menue active? 
	; BNE .CALCUALTE.SCREEN_BYTE.DONE ;if yes, branch
	; ;add 1 to screen byte because game settings menu icon is offset from the prior icon by an extra column. 
	; INC INV.MENU.SELECTOR_PILLAR1.SBYTE
	; INC INV.MENU.SELECTOR_PILLAR2.SBYTE


.CHECK.CORNER_CASE
	LDA INV.MENU.SELECTOR_PILLAR1.SBYTE
	CMP #$0F
	BNE .CHECK.CORNER_CASE.DONE
	INC INV.MENU.SELECTOR_PILLAR1.SBYTE
	INC INV.MENU.SELECTOR_PILLAR2.SBYTE	
			
.CHECK.CORNER_CASE.DONE

.CALCUALTE.SCREEN_BYTE.DONE


	RTS
	
@END

		
@END

INV.RUN.CALCULATE.COMBAT.STATS.IF_CHANGES_OCUURED	
@START	

			
				; lda #$01
				; sta troubleshooting.hook
				
				

				
				
.READY_UNREADY.RUN.CHECK
;(don't recalculate stats if no changes to equipment slots were made by INV.READY_UNREADY.EQUIPMENT.ENTRANCE)
	LDA INV.READY_UNREADY.RUN_FLAG	;($00 = ready_unready not called | $01 ready_unready called. ready_unready refers to INV.READY_UNREADY.EQUIPMENT.ENTRANCE)
	BEQ .READY_UNREADY.RUN.CHECK.DONE
	
	
	;read CALCULATE.COMBAT.STATS module from disk
		LDA #$07 ;set sub_module code
	JSR INV.READ_LAUNCH.SUB_MODULE.WITH_RTS
	
	;process only equipment slots that changed since last run. 
	JSR CALCULATE.COMBAT.STATS.GENERAL_ENTRANCE ;(uses INV.RE.UPDATE_DAMAGE.FLAG and INV.RE.UPDATE_DEFENSE.FLAG to determine which equipment slots to process (i.e. which ones have values that changed from the last run)

	
	;reset flags so that switch menus during this session doesn't trigger calculate stats unless another change is made to readied equipment
	LDA #$00
	STA INV.READY_UNREADY.RUN_FLAG	;($00 = ready_unready not called | $01 ready_unready called. ready_unready refers to INV.READY_UNREADY.EQUIPMENT.ENTRANCE)
	STA INV.RE.UPDATE_DAMAGE.FLAG 	;($00 = no changes made that effect DAMAGE on the character sheet. $01 changes were made that affect the same)
	STA INV.RE.UPDATE_DEFENSE.FLAG 	;($00 = no changes made that effect DAMAGE on the character sheet. $01 changes were made that affect the same)

	
	
.READY_UNREADY.RUN.CHECK.DONE

.EXIT
	RTS
@END

	
INV.DEBUG.LOG.WRITE	;**OPT** Memory. Remove.
@START
					
	TXA
	PHA
	
	LDX INV.DEBUG.LOG.INDEX
	LDA INV.DEBUG.ADDRESS+$0
	STA INV.DEBUG.LOG,X

	INC INV.DEBUG.LOG.INDEX

	LDX INV.DEBUG.LOG.INDEX
	LDA INV.DEBUG.ADDRESS+$1
	STA INV.DEBUG.LOG,X
	
	INC INV.DEBUG.LOG.INDEX
	
					; LDA #$AA
					; LDX INV.DEBUG.ADDRESS+$0
					; LDY INV.DEBUG.ADDRESS+$1
					; JSR PREP.BRK
					; BRK
					
	
	PLA
	TAX

.EXIT	
	RTS
@END
	
@END

;COMMON SHAPE TABLES
@START
MENU_ICON0.STATS			.HS	FE.BF.9F.FC.87.F0.93.E3.93.E4.93.E2.93.E1.93.E7.83.E0.C3.E1.83.E2.C3.E1.83.E2.C7.F1.9F.FC.FE.BF
MENU_ICON1.WEAPONS			.HS	FE.BF.FF.FF.87.F0.83.E0.A3.E2.F3.E7.F3.E7.F3.E7.A3.E2.A3.E0.A3.E0.A3.E0.A3.E0.87.F0.FF.FF.FE.BF
MENU_ICON2.ARMOR			.HS	FE.BF.FF.FF.87.F0.C3.E1.F3.E3.0B.E1.C3.E3.E3.E7.E3.E1.E3.E7.C3.E7.C3.E3.E3.E7.87.F0.FF.FF.FE.BF
MENU_ICON3.MISC_ITEMS		.HS	FE.BF.FF.FF.87.F0.C3.E0.D3.E2.93.E2.93.E2.D3.E2.C3.E0.C3.E0.C3.E2.C3.E0.C3.E2.87.F0.FF.FF.FE.BF
MENU_ICON4.SPELLS			.HS	FE.BF.FF.FF.87.F1.A3.E4.83.E1.C3.E1.C3.E1.C3.E1.E3.E3.E3.E3.F3.E7.F3.E7.E3.E3.87.F0.FF.FF.FE.BF
MENU_ICON5.GAME_SETTINGS	.HS	FE.BF.FF.FF.83.E0.83.E0.83.E0.83.E0.C3.E1.E3.E3.C3.E1.83.E0.C3.E1.C3.E1.C3.E1.83.E0.FF.FF.FE.BF


@END

;COMMON TEXT BLOCKS
@START
INV.TEXT_BLOCK.FOOTER					.AZ -/<-MORE !/,#$AF,/4 MORE->/ ;screen number is at byte $7


@END

;COMMON VARIABLES (accessible to all inventory sub-modules)
@START
;CHR_SHEET.PC.READIED_EQUIP  is needed by calculate.stats and ready equipment
CHR_SHEET.PC.READIED_EQUIP 				.BS DATA.PLY.CHR_SHEET.READIED.SIZE 
CHR_SHEET.PC.READIED_EQUIP.RECORD.SIZE	.EQ $10 ;#CONSTANT
CHR_SHEET.PC.READIED_EQUIP.RECORD.READ	.BS CHR_SHEET.PC.READIED_EQUIP.RECORD.SIZE
CHR_SHEET.PC.READIED_EQUIP.FILE_WRITE.SIZE	.EQ $200 ;the size to write to disk 

;byte $00 of AND/ORA hex tables is used to unready an item for all players. This works because the index is the sequential player # which starts with $01, so byte $00 is free for this purpose. 
INV.READIED_FLAGS.ORA_MASK		.HS .00.04.08.10.20.40.80
INV.READIED_FLAGS.AND_MASK		.HS .00.FB.F7.EF.DF.BF.7F
;the bit task mask is applied by ANDing the ORA mask
INV.READIED_FLAGS.BIT_TEST_MASK	.EQ INV.READIED_FLAGS.ORA_MASK 


;===INVENTORY SUB_MODULE FILE PARAMETERS====

INV.SUB_MODULE.FILE.PARAMETERS
@START
;byte $0-1: seek bytes HO/LO
;byte $2-3: read bytes HO/LO
;byte $4-5: patch section start HO/LO
;byte $6-7: sub_module entrance HO/LO

;SUB_MODULE0
	.DA	#INV_0.STATS_SUMMARY.SEEK_BYTES
	.DA	/INV_0.STATS_SUMMARY.SEEK_BYTES

	.DA	#INV_0.STATS_SUMMARY.READ_BYTES
	.DA	/INV_0.STATS_SUMMARY.READ_BYTES	
	
	.DA #INV_0.STATS_SUMMARY.PATCH_START
	.DA /INV_0.STATS_SUMMARY.PATCH_START
	
	.DA	#INV_0.DISPLAY.STATS_SUMMARY
	.DA	/INV_0.DISPLAY.STATS_SUMMARY
	
;SUB_MODULE1
	.DA	#INV_1.READIED_EQUIPMENT.SEEK_BYTES
	.DA	/INV_1.READIED_EQUIPMENT.SEEK_BYTES

	.DA	#INV_1.READIED_EQUIPMENT.READ_BYTES
	.DA	/INV_1.READIED_EQUIPMENT.READ_BYTES	
	
	.DA #INV_1.READIED_EQUIPMENT.PATCH_START
	.DA /INV_1.READIED_EQUIPMENT.PATCH_START
	
	.DA	#INV_1.DISPLAY.READIED_EQUIPMENT
	.DA	/INV_1.DISPLAY.READIED_EQUIPMENT

	
;SUB_MODULE2
	.DA	#INV_2.COMBAT_STATS.SEEK_BYTES
	.DA	/INV_2.COMBAT_STATS.SEEK_BYTES

	.DA	#INV_2.COMBAT_STATS.READ_BYTES
	.DA	/INV_2.COMBAT_STATS.READ_BYTES	
	
	.DA #INV_2.COMBAT_STATS.PATCH_START
	.DA /INV_2.COMBAT_STATS.PATCH_START
	
	.DA	#INV_2.DISPLAY.COMBAT_STATS
	.DA	/INV_2.DISPLAY.COMBAT_STATS

;SUB_MODULE3
	.DA	#INV_3.SKILLS.SEEK_BYTES
	.DA	/INV_3.SKILLS.SEEK_BYTES

	.DA	#INV_3.SKILLS.READ_BYTES
	.DA	/INV_3.SKILLS.READ_BYTES	
	
	.DA #INV_3.SKILLS.PATCH_START
	.DA /INV_3.SKILLS.PATCH_START
	
	.DA	#INV_3.DISPLAY.SKILLS
	.DA	/INV_3.DISPLAY.SKILLS

	
;SUB_MODULE4
	.DA	#INV_4.DISPLAY_INVENTORY.SEEK_BYTES
	.DA	/INV_4.DISPLAY_INVENTORY.SEEK_BYTES

	.DA	#INV_4.DISPLAY_INVENTORY.READ_BYTES
	.DA	/INV_4.DISPLAY_INVENTORY.READ_BYTES	
	
	.DA #INV_4.DISPLAY_INVENTORY.PATCH_START
	.DA /INV_4.DISPLAY_INVENTORY.PATCH_START
	
	.DA	#INV_4.DISPLAY_INVENTORY
	.DA	/INV_4.DISPLAY_INVENTORY	

;SUB_MODULE5

@END

;FIELD LABELS: CHARACTER SHEET (READIED FIELDS)
@START

;***WARNING** Make sure to update the non-authoratative copy in the offload_variables.ASM file.
;Search for the name of this code section. see the note at the top of this code section in the
;offload_variables.ASM file for documentation about why two copies are kept. 

CHR_SHEET.READIED_EQUIP.TYPE.LHAND	.EQ CHR_SHEET.PC.READIED_EQUIP.RECORD.READ+$0
CHR_SHEET.READIED_EQUIP.ID.LHAND	.EQ CHR_SHEET.PC.READIED_EQUIP.RECORD.READ+$1
CHR_SHEET.READIED_EQUIP.TYPE.RHAND	.EQ CHR_SHEET.PC.READIED_EQUIP.RECORD.READ+$2
CHR_SHEET.READIED_EQUIP.ID.RHAND	.EQ CHR_SHEET.PC.READIED_EQUIP.RECORD.READ+$3
CHR_SHEET.READIED_EQUIP.TYPE.HEAD	.EQ CHR_SHEET.PC.READIED_EQUIP.RECORD.READ+$4
CHR_SHEET.READIED_EQUIP.TYPE.HEAD.OFFSET	.EQ $04 ;#CONSTANT
CHR_SHEET.READIED_EQUIP.ID.HEAD		.EQ CHR_SHEET.PC.READIED_EQUIP.RECORD.READ+$5
CHR_SHEET.READIED_EQUIP.TYPE.TORSO	.EQ CHR_SHEET.PC.READIED_EQUIP.RECORD.READ+$6
CHR_SHEET.READIED_EQUIP.ID.TORSO	.EQ CHR_SHEET.PC.READIED_EQUIP.RECORD.READ+$7
CHR_SHEET.READIED_EQUIP.TYPE.FEET	.EQ CHR_SHEET.PC.READIED_EQUIP.RECORD.READ+$8
CHR_SHEET.READIED_EQUIP.ID.FEET		.EQ CHR_SHEET.PC.READIED_EQUIP.RECORD.READ+$9
CHR_SHEET.READIED_EQUIP.TYPE.HAND_COVERING	.EQ CHR_SHEET.PC.READIED_EQUIP.RECORD.READ+$A
CHR_SHEET.READIED_EQUIP.ID.HAND_COVERING	.EQ CHR_SHEET.PC.READIED_EQUIP.RECORD.READ+$B
CHR_SHEET.READIED_EQUIP.TYPE.FINGER			.EQ CHR_SHEET.PC.READIED_EQUIP.RECORD.READ+$C
CHR_SHEET.READIED_EQUIP.ID.FINGER			.EQ CHR_SHEET.PC.READIED_EQUIP.RECORD.READ+$D
CHR_SHEET.READIED_EQUIP.TYPE.NECK			.EQ CHR_SHEET.PC.READIED_EQUIP.RECORD.READ+$E
CHR_SHEET.READIED_EQUIP.ID.NECK				.EQ CHR_SHEET.PC.READIED_EQUIP.RECORD.READ+$F

@END

@END
