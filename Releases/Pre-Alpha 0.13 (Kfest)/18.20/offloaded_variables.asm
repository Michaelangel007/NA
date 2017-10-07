;************************INCLUDE FILE*****************************
;(do not assemble stand-alone.)
				;.OR		$1200			**Always put before .TF directive and never use again in program
				;.TF     VARIABLES.bin,BIN


;======SHARED/COMMON VARIABLE SPACE=====
@START
;=======================DOCS===========================================
;many variables have .EQ definitions pointing back to these
;shared variables to make use of memory that isn't being used at
;a conflicting time. 
;
;A summary of the reservations is below:
;
;-----BLOCK1------**Shared Var space**
;-Required Player is in Building
;**Shared Var space**: (NPC.PATHGENERATOR): SHARED.VARIABLE_SPACE.BLOCK1+$00 - $87
;
;-Modules/multiple (some cross module use occurs
;**Shared Var space**: (TEXT WINDOW FUNCTIONS): SHARED.VARIABLE_SPACE.BLOCK1+$88 - $CC
;**Shared Var space**: (COMBAT overflow): SHARED.VARIABLE_SPACE.BLOCK1+$CD - ??
;
;
;-----BLOCK2------**Shared Var space**
;-Required When Entering Building
;**Shared Var space**: (NPC.INIT): SHARED.VARIABLE_SPACE.BLOCK2+$00 - $BF
;**Shared Var space**: (COMBAT.READ_WRITE.CHR_SHEET.MOB): SHARED.VARIABLE_SPACE.BLOCK2+70-$EF (used by CHR_SHEET.RECORD.READ($80))
;
;
;-Required For File I/O
;**Shared Var space**: (PRODOS.IO) SHARED.VARIABLE_SPACE.BLOCK2+$F2 - $FF
;
;-Modules/single (only required when active)
;**Shared Var space**: (NPC.TALK): SHARED.VARIABLE_SPACE.BLOCK2+$0 - $BE
;**Shared Var space**: (COMBAT): SHARED.VARIABLE_SPACE.BLOCK2+$00- $FF
;
;-----BLOCK3------**Shared Var space**
;**Shared Var space**: anything that used to point to shape.hopper
;
;-----BLOCK4------**Shared Var space**
;
;Block 4 uses part of Text Page #2 ($800-$BFF)
;$0800-$BA6: screen arrays
;$0BA7-$BFF: used for Block4 ($59 bytes)
;
;**Shared Var space**: (COMBAT): SHARED.VARIABLE_SPACE.BLOCK4+$00 - $3F
;
;-STEP RESERVATIONS
;(used for pointing a variables to a variable that uses shared space. This is useful because sometimes it is easier
;to understand the dependancies by seeing the more descriptive variable name. This is typically use for large variable
;definitions which use a different shared block that the rest of it's section. The step definitions here are a work
;around to SBASM's restriction preventing forward references via the .EQ directive, which is probably generally a good
;rule, just not ideal for this approach to variable management)
;
;**see below for the actual reservations
;
;=======================DOCS===========================================

;PRIMARY RESERVATIONS
				;used for loader zone output buffer, and other things
SHARED.VARIABLE_SPACE.BLOCK1	.BS	$100,$42			;temporary buffer for output from zone_tools.uncompress.single. **Shared**

;used for loader zone input buffer, and other things
SHARED.VARIABLE_SPACE.BLOCK2	.BS $101,$42			;TEMPORARY BUFFER WHERE COMPRESSED ZONE DATA IS COPIED FROM AUX MEMORY BEFORE UNCOMPRESSING IT

SHARED.VARIABLE_SPACE.BLOCK3	.BS $20,$00

SHARED.VARIABLE_SPACE.BLOCK4	.EQ $0BA7 ;$59bytes. Ends at $0BFF

;STEP RESERVATIONS
;see docs above for overview of how these are used

;only is use during a print operation to a text window
TEXT.WINDOW.BUFFER_STEP		.EQ SHARED.VARIABLE_SPACE.BLOCK1+$88 ;$40bytes 
MAP_OBJECTS.X_ADJ_STEP		.BS $1
MAP_OBJECTS.Y_ADJ_STEP		.BS $1

;---must be kept in a group in this sequence---
MOB.POSITION.X_GR_STEP	.BS $1
MOB.POSITION.X_LT_STEP	.BS $1
MOB.POSITION.Y_GR_STEP 	.BS $1
MOB.POSITION.Y_LT_STEP	.BS $1
;-------------------------------------------

RESULT_STEP				.BS $8
SPRITE.RECORD_STEP		.BS $0C		
SHAPE.HOPPER0_STEP		.EQ $200 ;$100bytes
;SHAPE.HOPPER0_STEP		.EQ $B300 ;$100bytes

COMBAT.OUT_OF_ORDER.SHARED.MEMORY_D_STEP	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$3F ;$1 byte
COMBAT.OUT_OF_ORDER.SHARED.MEMORY_E_STEP	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$40 ;$1 byte
COMBAT.OUT_OF_ORDER.SHARED.MEMORY_F_STEP	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$41 ;$1 byte
COMBAT.OUT_OF_ORDER.SHARED.MEMORY10_STEP	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$42 ;$1 byte
COMBAT.OUT_OF_ORDER.SHARED.MEMORY11_STEP	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$43 ;$1 byte
COMBAT.OUT_OF_ORDER.SHARED.MEMORY15_STEP	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$0F ;$1 byte

@END

;======SWAP SPACE=======
@START

SWAP_SPACE.MAIN_MEMORY.ALT.ADDR	.EQ $9000 ;#POINTER ADDRESS
SWAP_SPACE.MAIN_MEMORY			.EQ $9600 ;#POINTER ADDRESS
;SWAP_SPACE.MAIN_MEMORY			.EQ $A000 ;#POINTER ADDRESS
SWAP_SPACE.MAIN_MEMORY.END		.EQ $BFFF ;#POINTER ADDRESS
SWAP_SPACE.MAIN_MEMORY.POINTER	.EQ $ED	;Zero page pointer for swap space memory region

SWAP_SPACE.AUX_MEMORY			.EQ $2900 ;#POINTER ADDRESS
;SWAP_SPACE.AUX_MEMORY			.EQ $2F00 ;#POINTER ADDRESS
SWAP_SPACE.AUX_MEMORY.END		.EQ $58FF ;#POINTER ADDRESS

SWAP_SPACE2.AUX_MEMORY			.EQ $D000 ;#POINTER ADDRESS. BSR bank2
;SWAP_SPACE2.AUX_MEMORY			.EQ $D600 ;#POINTER ADDRESS. BSR bank2
SWAP_SPACE2.AUX_MEMORY.END		.EQ $FFFF ;#POINTER ADDRESS. BSR bank2
SWAP_SPACE2.AUX_MEMORY.MOB.START	.EQ $FCFF ;#POINTER ADDRESS. The start address of the mob map objects array when the main memory swap area is swapped out to this buffer. 

RZONE.ARRAY.TEMP_BUFFER			.EQ $B000 ;#POINTER ADDRESS. data is loaded here from disk temporarily when a new location is entered. Once loaded various zone tools subroutines are used to reorder and copy the data into the permenant RZONE.ARRAY memory location.
;RZONE.ARRAY.TEMP_BUFFER			.EQ $A600 ;data is loaded here from disk temporarily when a new location is entered. Once loaded various zone tools subroutines are used to reorder and copy the data into the permenant RZONE.ARRAY memory location.
;RZONE.ARRAY.TEMP_BUFFER			.EQ $B700 ;data is loaded here from disk temporarily when a new location is entered. Once loaded various zone tools subroutines are used to reorder and copy the data into the permenant RZONE.ARRAY memory location.

@END

;===========MAP OBJECT INFORMATION========
@START

MO.DOOR.OPEN.START		.EQ $24	;#CONSTANT (starting value for an open door)
MO.DOOR.OPEN.START2		.EQ $23	;#CONSTANT (alternate starting value for an open door)
MO.DOOR.OPEN.GRE 		.EQ $21 ;#CONSTANT
MO.DOOR.OPEN.LT 		.EQ $25 ;#CONSTANT
MO.DOOR.CLOSING 		.EQ $21 ;#CONSTANT  (door that will close this turn when MO.DRAW runs)
MO.DOOR.CLOSE_TRIGGER	.EQ $20	;#CONSTANT  (trigger for MO.DRAW to set MO record to closed door)
MO.DOOR.CLOSED.UNLOCKED .EQ $10 ;#CONSTANT
MO.DOOR.CLOSED.GRE 		.EQ $10 ;#CONSTANT
MO.DOOR.CLOSED.LT 		.EQ $20 ;#CONSTANT
MO.DOOR.ALL.GRE 		.EQ $10 ;#CONSTANT
MO.DOOR.ALL.LT  		.EQ $25 ;#CONSTANT
MO.DOOR.LOCKED.GRE		.EQ $11 ;#CONSTANT
MO.DOOR.LOCKED.LT		.EQ $20 ;#CONSTANT
MO.PORTCULLIS.LEFT		.EQ $30	;#CONSTANT
MO.PORTCULLIS.RIGHT		.EQ $31	;#CONSTANT
MO.PORTCULLIS.LOWERED	.EQ $32	;#CONSTANT

;SPECIAL
MO.NOT_PUSHABLE.GRE		.EQ $10	;#CONSTANT	non-transport objects with byte $3 >= this constant are not pushable
MO.SKIP.DRAW.FLAG		.EQ $FE	;#CONSTANT
	
@END

;============TILE INFORMATION=============
@START

;TOWN MAP: general
@START
TILE_ID.CELL_DOOR.GRE1			.EQ $1C				;#CONSTANT		TILE TYPE ID
TILE_ID.CELL_DOOR.LT1			.EQ $1F				;#CONSTANT		TILE TYPE ID

TILE_ID.FIREPLACE				.EQ $A3				;#CONSTANT		TILE TYPE ID			

TILE_ID.GUARD.GRE1				.EQ $86				;#CONSTANT	
TILE_ID.GUARD.LT1				.EQ $88				;#CONSTANT	

TILE_ID.LAMP_POST			.EQ $AA		;#CONSTANT	
TILE_ID.LAMP_POST.REPAIR	.EQ $AD		;#CONSTANT	


TILE_ID.PORTCULLIS				.EQ $20				;#CONSTANT		TILE TYPE ID
TILE_ID.PORTCULLIS_LEVER.LEFT	.EQ $21				;#CONSTANT		TILE TYPE ID
TILE_ID.PORTCULLIS_LEVER.RIGHT	.EQ $22				;#CONSTANT		TILE TYPE ID

TILE_ID.SCONCE.LEFT_WALL		.EQ $A5		;#CONSTANT	
TILE_ID.SCONCE.RIGHT_WALL		.EQ $A4		;#CONSTANT	

TILE_ID.UNDEAD_LORD				.EQ $8E				;#CONSTANT		TILE TYPE ID

TILE_ID.WALL.BRICK				.EQ $08
TILE_ID.WALL.STONE				.EQ $09

TILE_ID.WINDOW_DOOR.GRE1		.EQ $13				;#CONSTANT		TILE TYPE ID
TILE_ID.WINDOW_DOOR.LT1			.EQ $1C				;#CONSTANT		TILE TYPE ID

;occupied tiles swaps
;**at some point these need to be restricted to building map type.
TILE_ID.OUTHOUSE_HOLE.UNOCCUPIED	.EQ $53				;#CONSTANT		TILE TYPE ID
TILE_ID.OUTHOUSE_HOLE.OCCUPIED		.EQ $54				;#CONSTANT		TILE TYPE ID
TILE_ID.UNOCCUPIED.TILE.SWAP.GRE1		.EQ $D0 			;#CONSTANT
TILE_ID.UNOCCUPIED.TILE.SWAP.LT1		.EQ $F0				;#CONSTANT

; TILE_ID.BED_LEFT_UNOCCUPIED		.EQ $49				;#CONSTANT		TILE TYPE ID
; TILE_ID.BED_LEFT_OCCUPIED		.EQ $4A				;#CONSTANT		TILE TYPE ID
; TILE_ID.COT_UNOCCUPIED			.EQ $4C				;#CONSTANT		TILE TYPE ID
; TILE_ID.COT_OCCUPIED			.EQ $4D				;#CONSTANT		TILE TYPE ID
@END


;BUILDING MAP: Nox A* Grid tiles
@START
;street
TILE_ID.ROAD.GRE1				.EQ $5B				;#CONSTANT		TILE TYPE ID
TILE_ID.ROAD.LT1				.EQ $5D				;#CONSTANT		TILE TYPE ID

;misc
TILE_ID.NOXA_GRID.GROUP_A.GRE1			.EQ $49				;#CONSTANT		TILE TYPE ID
TILE_ID.NOXA_GRID.GROUP_A.LT1			.EQ $54				;#CONSTANT		TILE TYPE ID

;occupied tile swaps: regular chairs, bar table chairs
TILE_ID.NOXA_GRID.GROUP_B.GRE1			.EQ $D0				;#CONSTANT		TILE TYPE ID
TILE_ID.NOXA_GRID.GROUP_B.LT1			.EQ $EE				;#CONSTANT		TILE TYPE ID

;----THE FOLLOWING ARE INCLUDED IN NOXA_GRID
TILE_ID.LADDER_OUTHOUSE.GRE1	.EQ $51				;#CONSTANT		TILE TYPE ID
TILE_ID.LADDER_OUTHOUSE.LT1		.EQ $54				;#CONSTANT		TILE TYPE ID
TILE_ID.BED_LEFT_UNOCCUPIED		.EQ $49				;#CONSTANT		TILE TYPE ID
TILE_ID.BED_LEFT_OCCUPIED		.EQ $4A				;#CONSTANT		TILE TYPE ID
TILE_ID.BED_RIGHT				.EQ $4B				;#CONSTANT		TILE TYPE ID
TILE_ID.BED_COT.GRE1			.EQ $49				;#CONSTANT		TILE TYPE ID
TILE_ID.BED_COT.LT1				.EQ $4E				;#CONSTANT		TILE TYPE ID
TILE_ID.COT_UNOCCUPIED			.EQ $4C				;#CONSTANT		TILE TYPE ID
TILE_ID.COT_OCCUPIED			.EQ $4D				;#CONSTANT		TILE TYPE ID
TILE_ID.FLOOR_PATH.GRE1			.EQ $4E				;#CONSTANT		TILE TYPE ID
TILE_ID.FLOOR_PATH.LT1			.EQ $50				;#CONSTANT		TILE TYPE ID
;--------------------------------------------
@END

;CASTLE MAP
@START

TILE_ID.CASTLE.ARCHWAY.LEFT					.EQ $01
TILE_ID.CASTLE.PORTCULLIS_ARCHWAY.LEFT		.EQ $03
TILE_ID.CASTLE.PORTCULLIS_ARCHWAY.SINGLE	.EQ $05
TILE_ID.CASTLE.WALL_DECORATION.SHIELD		.EQ $06
TILE_ID.CASTLE.WALL_DECORATION.CHEVRON		.EQ $09


TILE_ID.BUILDING.ARCHWAY.GRE			.EQ TILE_ID.CASTLE.ARCHWAY.LEFT
TILE_ID.BUILDING.ARCHWAY.LT				.EQ TILE_ID.CASTLE.PORTCULLIS_ARCHWAY.SINGLE+1


@END

;SURFACE MAP
@START
TILE_ID.MOUNTAINS.TALL			.EQ $00

TILE_ID.SKIFF					.EQ $33				;#CONSTANT		TILE TYPE ID
TILE_ID.GRASS					.EQ $34				;#CONSTANT		TILE TYPE ID
TILE_ID.TALL_GRASS_A			.EQ $35				;#CONSTANT		TILE TYPE ID
TILE_ID.TALL_GRASS_B			.EQ $36				;#CONSTANT		TILE TYPE ID

TILE_ID.TREES.GRE				.EQ $37				;#CONSTANT		TILE TYPE ID
TILE_ID.TREES.LT				.EQ $3B				;#CONSTANT		TILE TYPE ID

TILE_ID.TREES.BIRCH				.EQ $39
TILE_ID.BEACH					.EQ $41				;#CONSTANT		TILE TYPE ID

TILE_ID.HORSE_A					.EQ $42				;#CONSTANT		TILE TYPE ID
TILE_ID.HORSE_B					.EQ $43				;#CONSTANT		TILE TYPE ID
TILE_ID.HORSE_C					.EQ $44				;#CONSTANT		TILE TYPE ID
;NOTE: MAP OBJECT RECORD ALWAYS HAS HORSE_C

TILE_ID.HILLS					.EQ $45				;#CONSTANT		TILE TYPE ID
TILE_ID.QUICKSAND				.EQ $46				;#CONSTANT		TILE TYPE ID

TILE_ID.SHORELINE.GRE			.EQ $63				;#CONSTANT		TILE TYPE ID
TILE_ID.SHORELINE.LT			.EQ $7B				;#CONSTANT		TILE TYPE ID

TILE_ID.CARAVEL					.EQ $7B				;#CONSTANT		TILE TYPE ID
TILE_ID.FRIGATE1.1				.EQ $7C				;#CONSTANT		TILE TYPE ID
TILE_ID.FRIGATE1.2				.EQ $7D				;#CONSTANT		TILE TYPE ID
TILE_ID.FRIGATE1.3				.EQ $7E				;#CONSTANT		TILE TYPE ID
TILE_ID.FRIGATE1.4				.EQ $7F				;#CONSTANT		TILE TYPE ID

TILE_ID.SURF					.EQ $88				;#CONSTANT		TILE TYPE ID
TILE_ID.SHALLOW_WATER			.EQ $89				;#CONSTANT		TILE TYPE ID
TILE_ID.DEEP_WATER				.EQ $8B				;#CONSTANT		TILE TYPE ID
TILE_ID.CROC_A					.EQ $92				;#CONSTANT		TILE TYPE ID
TILE_ID.CROC_B					.EQ $93				;#CONSTANT		TILE TYPE ID
TILE_ID.WYVERN					.EQ $95				;#CONSTANT		TILE TYPE ID
TILE_ID.PLAYER_ICON.HALF_SUNK	.EQ $99				;#CONSTANT		TILE TYPE ID
TILE_ID.PLAYER_ICON.FULL_SUNK	.EQ $9A				;#CONSTANT		TILE TYPE ID
TILE_ID.STORM					.EQ $A9				;#CONSTANT		TILE TYPE ID
TILE_ID.STORM.GRE				.EQ $A9				;#CONSTANT		TILE TYPE ID
TILE_ID.STORM.LT				.EQ $AD				;#CONSTANT		TILE TYPE ID
TILE_ID.SHARK					.EQ $FB
@END

;SLOW PROGRESS NOTE: SET TO $FF FOR 0% SLOW PROGRESS; $00 FOR 100% AUTOMATIC SLOW PROGRESS
TILE.QUICKSAND.SINK.HEIGHT		.EQ	$05				;#CONSTANT		Height to which player icon sinks when standing in quicksand
TILE.QUICKSAND.SINK.AUTOMATIC	.EQ $0E				;#CONSTANT		The height to which the player is automatically sunk in quicksand, even if player is holding down a movement key
TILE.QUICKSAND.PLAYER.SLOW_PROGRESS.HEIGHT	.EQ $0D ;#CONSTANT		The height at which slow progress is a possibility. THIS IS SO THAT THE PLAYER, IF HOLDING DOWN MOVEMENT KEY, CAN EASILY TRAVERSE A COUPLE QUICK STAND TILES WITH ONLY MINOR DEALY.
TILE.QUICKSAND.PLAYER.SLOW_PROGRESS.RATIO	.EQ $40		;#CONSTANT		SLOW PROGRESS IF RANDOM # IS LESS THAN THIS VALUE

TILE.HILLS.PLAYER.SLOW_PROGRESS				.EQ $C0		;#CONSTANT		SLOW PROGRESS IF RANDOM # IS LESS THAN THIS VALUE


;UNDERMAP
@START
TILE_ID.CAVERN_WALL.RANGE.GRE	.EQ $00	;#CONSTANT, TILE TYPE ID
TILE_ID.CAVERN_WALL.RANGE.LT	.EQ $40	;#CONSTANT, TILE TYPE ID

TILE_ID.CAVERN_TORCH.RANGE.GRE	.EQ $8D	;#CONSTANT, TILE TYPE ID
TILE_ID.CAVERN_TORCH.RANGE.LT	.EQ $91	;#CONSTANT, TILE TYPE ID

TILE_ID.SPIDER_WEB				.EQ $6A	;#CONSTANT, TILE TYPE ID


@END

;======COMBAT==== 
;(must be in all tilesets, or setup a special area for them triggered by a parameter)


;special effects
TILE_ID.FIRE_A					.EQ $9D				;#CONSTANT		TILE TYPE ID
TILE_ID.FIRE_B					.EQ $9E				;#CONSTANT		TILE TYPE ID

;summoning
TILE_ID.SKELETON		.EQ $E0	;#CONSTANT TILE TYPE ID
TILE_ID.DEMON_LORD		.EQ $D0	;#CONSTANT TILE TYPE ID
TILE_ID.COW				.EQ $9F ;#CONSTANT TILE TYPE ID

@END


;======GENERAL ROUTINES (LOW LEVEL)========
@START

;GENERAL
@START
WAIT.ADDRESS		.EQ	$FCA8
DELAY				.EQ	$FF				;#CONSTANT
APPLE_BELL.ADDRESS	.EQ $FF3A			

KB_BUFFER			.EQ	$C000
KB_BUFFER_ACK		.EQ $C010

AUX_MOVE		.EQ $C311
AUX_MOVE.START	.EQ $3C
AUX_MOVE.END	.EQ $3E
AUX_MOVE.DEST	.EQ	$42


COPY.SIZE			.EQ $C0
COPY.TO				.EQ $C2				;2byt	
COPY.FROM_START		.EQ $C4				;2byt
COPY.FROM_END		.EQ $C6
GENERIC.ARRAY.POINTER		.EQ COPY.FROM_END ;used when a zero page pointer is needed for something quick and minor. 




FILL.START	.EQ	$EB			;START ADDRESS TO FILL
FILL.END	.BS $2			;END ADDRESS TO FILL
FILL.VALUE 	.BS $1			;VALUE TO FILL WITH
		
KEYIN.STRING.LEFT_EDGE	.EQ FILL.VALUE

SAVED.YREG.GLOBAL1						.BS $01		;1byt
SAVED.YREG.GLOBAL2						.BS $01		;1byt
SAVED.YREG.LOCAL						.BS $01		;1byt
SAVED.YREG.LOCAL1						.BS $01		;1byt
;SAVED.XREG.LOCAL2						.BS $01		;1byt
SAVED.XREG.GLOBAL1						.BS $01		;1byt
SAVED.XREG.LOCAL						.BS $01		;1byt
SAVED.XREG.LOCAL1 						.BS $01		;1byt
SAVED.ACC.GLOBAL1						.BS $01
SAVED.ACC.LOCAL							.BS $01
SAVED.ACC.LOCAL2						.BS $01
TEMP									.BS $01		;1byt
TEMP16									.BS $02		;1byt

KEYIN.NO_CLEAR .EQ FILL.END	;parmeter of KEYIN.ANIMATION.SINGLE used to tell it not to clear the keypress buffer
KEYIN.SAVED	 .EQ FILL.END+$1


; KEYIN.STRING.XREG		.EQ FILL.END+$0
; KEYIN.STRING.YREG		.EQ FILL.END+$1

KEYIN.STRING.XREG	.BS $1
KEYIN.STRING.YREG	.BS $1

;INSERTION_SORT
;
;ORIGINAL AUTHOR's comments:
; This describes the sequence h(0)=1; h(n)=k*h(n-1)+1 for k=3 (1,4,13,40...)
; All word-values are muliplied by 2, since we are sorting 2-byte values

;**OPT** Memory. The sep_low and sep_high tables can likely be reduced to the first element, unless shell sort is used
;(currently only insertion sort is used, which seems to use the values in the first element as the sort entrance point)
;Also, all of the .BS definitions should be able to be converted to shared variables space that is only in use by modules.
;as of the writing of this comment I'm only anticipting sorts will be used in NPC.PATHFINDER and the inventory and
;merchant modules. 

;sep = sort entrance point

sep_low           .HS 02.08.18.50.F2.D8.8A.A0.E2 ;renamed from h_low
sep_high          .HS 00.00.00.00.00.00.08.19.4C ;renamed from h_high

sep_start_index  	.BS $1 ;renamed from h_start_index 
sep_index        	.BS $1 ;renamed from h_index
sort.entrance.point	.BS $2 ;renamed from h. address in the array where sort beings. It's the start of the array for an insertion sort but could be after the start for a shell sort. 
sort.table.address	.EQ sort.entrance.point ;parameter containing HO/LO address of the table to be sorted. 

array.record_size	.BS $1 ;**NEW**. # of bytes in the records in the array to be sorted. 

input.start.addr .BS $2 ;renamed from in_address
arr_start      	.BS $2
arr_end         .BS $2
unsorted.record.index    	.BS $2 ;renamed from I. the record that is being sorted by the current iteration of .NEXT.UNSORTED_RECORD
unsorted_record.values 		.EQ TEXT.WINDOW.BUFFER_STEP ;$10bytes. renamed from V. Must be set equal to array.record_size
unsorted_record.values_plus1 .EQ TEXT.WINDOW.BUFFER_STEP+$10 ;$10bytes. renamed from v_plus_1. "" unsorted_record.values +$01 (the values stored, not the address). Must be set equal to array.record_size

copy_from.pointer	.EQ $fb  ;renamed from J.  Uses two bytes. Has to be on zero-page
copy_to.pointer 	.EQ $fd    ;renamed from J_plus_H.  Has to be on zero-page
arr_length			.EQ copy_to.pointer              ; Can safely use the same location as


;MONITOR VARIABLE
VARIABLE1.HTAB .EQ RESULT_STEP+$0
VARIABLE1.VTAB .EQ RESULT_STEP+$1
VARIABLE2.HTAB .EQ RESULT_STEP+$2
VARIABLE2.VTAB .EQ RESULT_STEP+$3

@END

;GRAPHICS
@START
GRAPHICS 			.EQ	$C050
HIRES				.EQ	$C057
PAGE1				.EQ	$C054
MIXOFF				.EQ	$C052
PAGE2				.EQ	$C055




SCLEAR.PAGE.STOP.ADDRESS	.BS		$01	
COPY.STOP.ADDRESS			.BS		$01		

PAGE.FOREGROUND				.BS $1		;1byt	USED WHEN CALLING DRAW.TILE SUBROUTINE (POSSIBLY OTHERS IN FUTURE) TO SPECIFY THE BACKGROUND PAGE (WHICH IS USUALLY THE ONE DRAWN ON)
PAGE.BACKGROUND				.BS	$1		;1byt	CONTAINS THE OPPOSITE VALUE OF PAGE, SO IT CAN BE USED BY ROUTINES THAT NEED TO DRAW TO THE BACKGROUND PAGE. 
PAGE.FOREGROUND.OVERRIDE	.BS $1		;1byt 	$01 = override on, $00 = off
DRAW.TILE.SHAPE_TABLE.OVERRIDE .BS $1	;($00 = load data into shape table from aux memory | $01 don't load data into shape table)
LINE.BASE.ADDR1		.EQ	$EA				;2byt
LINE.BASE.ADDR2		.EQ	$EC				;2byt


LINE.BASE.ADDR3		.EQ	$E6				;2byt
LINE.BASE.ADDR4		.EQ	$E8				;2byt


COPYTO				.EQ $FA				;2byt
COPYFROM			.EQ $FC				;2byt

;Line Lookup Tables. LINE.LO is used for Hi-Res Page 1 and Page 2. 
;
;(THIS COMMENT NO LONGER APPLIES) *****see offloaded_variables.asm****
LINE.LO			.HS 00.00.00.00.00.00.00.00.80.80.80.80.80.80.80.80.00.00.00.00.00.00.00.00.80.80.80.80.80.80.80.80.00.00.00.00.00.00.00.00.80.80.80.80.80.80.80.80.00.00.00.00.00.00.00.00.80.80.80.80.80.80.80.80.28.28.28.28.28.28.28.28.A8.A8.A8.A8.A8.A8.A8.A8.28.28.28.28.28.28.28.28.A8.A8.A8.A8.A8.A8.A8.A8.28.28.28.28.28.28.28.28.A8.A8.A8.A8.A8.A8.A8.A8.28.28.28.28.28.28.28.28.A8.A8.A8.A8.A8.A8.A8.A8.50.50.50.50.50.50.50.50.D0.D0.D0.D0.D0.D0.D0.D0.50.50.50.50.50.50.50.50.D0.D0.D0.D0.D0.D0.D0.D0.50.50.50.50.50.50.50.50.D0.D0.D0.D0.D0.D0.D0.D0.50.50.50.50.50.50.50.50.D0.D0.D0.D0.D0.D0.D0.D0
LINE.HO.P1 		.HS 20.24.28.2C.30.34.38.3C.20.24.28.2C.30.34.38.3C.21.25.29.2D.31.35.39.3D.21.25.29.2D.31.35.39.3D.22.26.2A.2E.32.36.3A.3E.22.26.2A.2E.32.36.3A.3E.23.27.2B.2F.33.37.3B.3F.23.27.2B.2F.33.37.3B.3F.20.24.28.2C.30.34.38.3C.20.24.28.2C.30.34.38.3C.21.25.29.2D.31.35.39.3D.21.25.29.2D.31.35.39.3D.22.26.2A.2E.32.36.3A.3E.22.26.2A.2E.32.36.3A.3E.23.27.2B.2F.33.37.3B.3F.23.27.2B.2F.33.37.3B.3F.20.24.28.2C.30.34.38.3C.20.24.28.2C.30.34.38.3C.21.25.29.2D.31.35.39.3D.21.25.29.2D.31.35.39.3D.22.26.2A.2E.32.36.3A.3E.22.26.2A.2E.32.36.3A.3E.23.27.2B.2F.33.37.3B.3F.23.27.2B.2F.33.37.3B.3F
LINE.HO.P2 		.HS 40.44.48.4C.50.54.58.5C.40.44.48.4C.50.54.58.5C.41.45.49.4D.51.55.59.5D.41.45.49.4D.51.55.59.5D.42.46.4A.4E.52.56.5A.5E.42.46.4A.4E.52.56.5A.5E.43.47.4B.4F.53.57.5B.5F.43.47.4B.4F.53.57.5B.5F.40.44.48.4C.50.54.58.5C.40.44.48.4C.50.54.58.5C.41.45.49.4D.51.55.59.5D.41.45.49.4D.51.55.59.5D.42.46.4A.4E.52.56.5A.5E.42.46.4A.4E.52.56.5A.5E.43.47.4B.4F.53.57.5B.5F.43.47.4B.4F.53.57.5B.5F.40.44.48.4C.50.54.58.5C.40.44.48.4C.50.54.58.5C.41.45.49.4D.51.55.59.5D.41.45.49.4D.51.55.59.5D.42.46.4A.4E.52.56.5A.5E.42.46.4A.4E.52.56.5A.5E.43.47.4B.4F.53.57.5B.5F.43.47.4B.4F.53.57.5B.5F
@END

;FILE I/O (ptool.ASM, ProRWTS2, PRODOS.IO)
@START

;ProRWTS 
;These are variables used by NOXARCH.SYSTEM, a file
;Peter built and installed on the bootloader. 

; bleftlo   = $EF ;Bytes left in file. If no bytes have been read then it is the total bytes in the file. 
; bleftho   = $F0
; status    = $f3         ;returns non-zero on error
; auxreq	  = $f4			;$00 = main memory, $01 = aux memory
; sizelo    = $f5         ;must set if reading or writing. For writing, # of pages to write must be in multiples of 2. i.e. 2, 4, 6, 8 $0A, etc) 
; sizehi    = $f6         ;must set if reading or writing.
; reqcmd    = $f8         ;used if enable_write=1, 1=read, 2=write
; ldrlo     = $f9         ;used if override_adr=1
; ldrhi     = $fa         ;used if override_adr=1
; namlo     = $fb
; namhi     = $fc

;zpage usage


;there are also buffers that can be moved if necessary:
;dirbuf, encbuf, treebuf (and corresponding hdd* versions that load to the same place)
;they are independent of each other so they can be placed separately
;see near EOF for those

; ;zpage usage (after REV 201705170-01). arbitrary selection except for the "ProDOS constant" ones
; ;Peter: feel free to move them around
;
;
;

; ;status    = $50         ;returns non-zero on error
; status    = $f3         ;returns non-zero on error
; auxreq    = $51         ;set to 1 to read/write aux memory, else main memory is used
; sizelo    = $52         ;set if enable_write=1 and writing, or reading, or if enable_seek=1 and seeking
; sizehi    = $53         ;set if enable_write=1 and writing, or reading, or if enable_seek=1 and seeking
; reqcmd    = $54         ;set (read/write/seek) if enable_write=1 or enable_seek=1
						; ;if allow_multi=1, bit 7 selects floppy drive in current slot (clear=drive 1, set=drive 2) during open call
						; ;bit 7 must be clear for read/write/seek on opened file
; ldrlo     = $55         ;set to load address if override_adr=1
; ldrhi     = $56         ;set to load address if override_adr=1
; namlo     = $57         ;name of file to access
; namhi     = $58         ;name of file to access

; tmpsec    = $3c         ;(internal) sector number read from disk
; reqsec    = $3d         ;(internal) requested sector number
; curtrk    = $40         ;(internal) track number read from disk

; command   = $42         ;ProDOS constant
; unit      = $43         ;ProDOS constant
; adrlo     = $44         ;ProDOS constant
; adrhi     = $45         ;ProDOS constant
; bloklo    = $46         ;ProDOS constant
; blokhi    = $47         ;ProDOS constant
; treeidx   = $f6         ;(internal) index into tree block
; istree    = $f7         ;(internal) flag to indicate tree file
; entries   = $f8         ;(internal) total number of entries in directory
; bleftlo   = $f9         ;(internal) bytes left in file
; blefthi   = $fa         ;(internal) bytes left in file
; bleftho   .EQ blefthi
; blkofflo  = $fb         ;(internal) offset within cache block
; blkoffhi  = $fc         ;(internal) offset within cache block
; step      = $fd         ;(internal) state for stepper motor
; tmptrk    = $fe         ;(internal) temporary copy of current track
; phase     = $ff         ;(internal) current phase for seek

; ;constants
; cmdseek   = 0           ;requires enable_seek=1
; cmdread   = 1           ;requires enable_write=1
; cmdwrite  = 2           ;requires enable_write=1
; SETKBD    = $fe89
; SETVID    = $fe93
; DEVNUM    = $bf30
; PHASEOFF  = $c080
; MOTOROFF  = $c088
; MOTORON   = $c089
; DRV0EN    = $c08a
; Q6L       = $c08c
; Q6H       = $c08d
; Q7L       = $c08e
; Q7H       = $c08f
; MLI       = $bf00
; NAME_LENGTH = $4        ;ProDOS constant
; MASK_SUBDIR = $d0       ;ProDOS constant
; MASK_ALL    = $f0       ;ProDOS constant
; KEY_POINTER = $11       ;ProDOS constant
; EOF_LO    = $15         ;ProDOS constant
; EOF_HI    = $16         ;ProDOS constant
; AUX_TYPE  = $1f         ;ProDOS constant
; ENTRY_SIZE = $27        ;ProDOS constant
; NEXT_BLOCK_LO = $2      ;ProDOS constant
; NEXT_BLOCK_HI = $3      ;ProDOS constant
; SAPLING   = $20         ;ProDOS constant
; FILE_COUNT = $25        ;ProDOS constant
; ROMIN     = $c081
; LCBANK2   = $c089
; CLRAUXRD  = $c002
; CLRAUXWR  = $c004
; SETAUXWR  = $c005
; CLRAUXZP  = $c008
; SETAUXZP  = $c009

				
				
;zpage usage (before REV 201705170-01)
;-NOTE: Convering to this version
;
; 1) loader.P / Noxarch.main. 
;	This version loads NOXARCH.MAIN to $1000, which in turn LOADER.P to $2000
;	Grab a copy of NOXARCH.MAIN from a backup before 201705170-01, and then change the .TF
;	directive on the current NOXARCH.MAIN to LOADER.P.BIN
;
; 2) comment/uncomment the calc stats section in loader.P
;	In theory this is needed because this version doesn't support seek resets, but
;   based on testing I actually think it does. I think Peter just made is more efficient
;	in the later versions. 
;
; 3) swap out the NOXARCH.SYS, and disk image masters
;	Find a backup that is before 201705170-01 (I used 5/4/2017 on occasion) and 
;	copy the NOXARCH.SYS, and disk image masters into the c:\my_code folder
;
; 4) zpage used changed from $Fx to $5x
;	Comment out the zero page values for the later version (above) and uncomment the values below. 
;
;
;
tmpsec    = $3c
reqsec    = $3d
A1L       = $3c         ;only during init
A1H       = $3d         ;only during init
A2L       = $3e         ;only during init
A2H       = $3f         ;only during init
A3L       = $40         ;only during init
A3H       = $41         ;only during init
curtrk    = $40

command   = $42         ;ProDOS constant
unit      = $43         ;ProDOS constant
adrlo     = $44         ;ProDOS constant
adrhi     = $45         ;ProDOS constant
bloklo    = $46         ;ProDOS constant
blokhi    = $47         ;ProDOS constant

bleftlo   = $ef         ;(internal) bytes left in file
blefthi   = $f0         ;(internal) bytes left in file
bleftho	  = blefthi
blkofflo  = $f1         ;(internal) used if override_size=1
blkoffhi  = $f2         ;(internal) used if override_size=1
status    = $f3         ;returns non-zero on error
auxreq    = $f4         ;set to 1 to read/write aux memory
sizelo    = $f5         ;set if enable_write=1 and writing, or if override_size=1 and reading, or if enable_seek=1 and seeking
sizehi    = $f6         ;set if enable_write=1 and writing, or if override_size=1 and reading, or if enable_seek=1 and seeking
entries   = $f7         ;(internal) total number of entries
reqcmd    = $f8         ;set if enable_write=1 or enable_seek=1
ldrlo     = $f9         ;set to load address if override_adr=1
ldrhi     = $fa         ;set to load address if override_adr=1
namlo     = $fb         ;set to name of file to open
namhi     = $fc         ;set to name of file to open
step      = $fd         ;(internal) state for stepper motor
tmptrk    = $fe         ;(internal) temporary copy of current track
phase     = $ff         ;(internal) current phase for seek
reloc     = $d000       ;$400 bytes code, $100 bytes data
dirbuf    = reloc+$500  ;$200 bytes
encbuf    = dirbuf+$200 ;$200 bytes


ProRWTS		.EQ $D000	;entrance for opening a new file. $D003 is entrace for read/write/seek on file already open. 

;main memory storage for ProRWTS variables
;Note: this is because once PRODOS.IO enables aux BSR, aux zero-page is also enabled. So, the parms can't be set in zero-page before calling PRODOS.IO. Instead we set the parms in main memory and PRODOS.IO will copy them onto the aux zero-page. 
parm.bleftlo 	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$F2
parm.bleftho 	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$F3
parm.status 	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$F4
parm.auxreq 	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$F5
parm.sizelo 	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$F6
parm.sizehi 	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$F7
parm.reqcmd 	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$F8
parm.ldrlo 		.EQ SHARED.VARIABLE_SPACE.BLOCK2+$F9
parm.ldrhi 		.EQ SHARED.VARIABLE_SPACE.BLOCK2+$FA
parm.namlo 		.EQ SHARED.VARIABLE_SPACE.BLOCK2+$FB
parm.namhi 		.EQ SHARED.VARIABLE_SPACE.BLOCK2+$FC
parm.current.file .BS $1 ;if set to $01, then PRODOS.IO wrapper assumes it should open a new file via $D003 instead of using the current file via $D000

;BSR.STATE 		.EQ SHARED.VARIABLE_SPACE.BLOCK2+$FD
BSR.STATE 		.BS $1
;BANK.STATE 		.EQ SHARED.VARIABLE_SPACE.BLOCK2+$FE
BANK.STATE 		.BS $1

BSR.STATE_SAVED .BS $1		;ROM/BSR soft-switch flag (bit7 = 1: BSR, bit7=0 ROM)
BANK.STATE_SAVED .BS $1		;BANK1/BANK2 soft-switch flag (bit7 = 1: BANK2, bit7=0 BANK1)

;PRODOS.IO
IO.ATTEMPTS			.EQ SHARED.VARIABLE_SPACE.BLOCK2+$FF
FILENAME.CHECK_LO	.EQ $C2		;used to check whether a filename is open or closed
FILENAME.CHECK_HI	.EQ $C3		;used to check whether a filename is open or closed


; ;(ALTERNATE) dedicated memory reservations
; parm.bleftlo	.BS $1
; parm.bleftho	.BS $1
; parm.status		.BS $1
; parm.auxreq		.BS $1
; parm.sizelo		.BS $1
; parm.sizehi		.BS $1
; parm.reqcmd		.BS $1
; parm.ldrlo 		.BS $1
; parm.ldrhI 		.BS $1
; parm.namlo 		.BS $1
; parm.namhi 		.BS $1
; parm.current.file .BS $1 ;if set to $01, then PRODOS.IO wrapper assumes it should open a new file via $D003 instead of using the current file via $D000
; ;
; BSR.STATE 		.BS $1
; BANK.STATE 		.BS $1
; IO.ATTEMPTS		.BS $1




;constants
cmd_seek.current_drive  .EQ $90
cmd_read.current_drive  .EQ $91
cmd_write.current_drive .EQ $92
			
cmd_read.drive1			.EQ $1
cmd_read.drive2		  	.EQ $81
cmd_write.drive1		.EQ $2
cmd_write.drive2		.EQ $82


;misc
COUT.ADDRESS		.EQ $FDED
TEXT				.EQ $C051
@END

;MATH
@START

OP1      	.BS $2				;2byt
OP2      	.BS $2				;2byt
RESULT  	.EQ RESULT_STEP				;2byt

;DIV.16 VARIABLES

;==========KEEP THESE VARIBLES IN THIS ORDER=======
;(the memory space is shared with other routines)
DIVIDEND32	.BS $2			;UPPER 16-BIT PORTION OF DIVIDEND
DIVIDEND 	.BS $2			;NUMBER TO DIVIDE
DIVISOR 	.BS $2			;NUMBER TO DIVIDE BY
TEMP1	.BS $1
CARRY	.BS $1
DIV.16.PARM.MODE .BS $1		;($00 = 16-bit dividend mode | >=$01 = 32-bit dividend mode)

;===================================================

;DIV.16.BCD VARIABLES
QUOTIENT.COUNTER 	.EQ TEMP1

;MLP.16.NO_BCD VARIABLES
PARTIAL  .EQ DIVIDEND
MULPLR   .EQ DIVIDEND32
MULCND   .EQ DIVISOR 

; MULPLR .BS $2	;**OPT** Memory. these can be removed or commented out and the above uncommented. I only created the .BS versions for debbugging. 
; MULCND .BS $2
; PARTIAL .BS $1


;MLP.16 VARIABLES	
MULPLR.TALLY	.EQ	DIVIDEND
MULCND.COUNTER	.EQ TEMP1
;MULPLR   .EQ DIVIDEND32  (defined above in MLP.16.NO_BCD)
;MULCND   .EQ DIVISOR 	(defined above in MLP.16.NO_BCD)

		
;MLP.32 VARIABLES
	;**OPT** Memory. Move the RANDOM.8 definitions to just below DIV.16 and change the MLP.32 definitions to .EQ
	;it may take some more jiggering, but that is the general idea. Alternatively, keep the MLP32 definitions and .EQ the other math variables to them.
	;UPDATE: I increased the RESULT_STEP definition to $8 so that PROD could .EQ to it so all math routines would continue to use RESULT as the return value. 
	;the reorg mentiond above could still be done and those $4 bytes could be relaimed. 
PROD 	.EQ RESULT_STEP			;$8bytes
MULR 	.EQ DIVIDEND32	  	;$4bytes 
MULND 	.EQ DIVISOR			;$4bytes
MULXP1	.EQ OP1				;$1byte
MULXP2	.EQ	OP1+$1			;$1byte

	
;BCD.UNPACK
BCD      		.EQ DIVIDEND32	;$3bytes
;BCD      		.BS $4	;$3bytes

;CONVERT.HEX_TO_BCD
BIN				.EQ DIVISOR		;$2byte

;PERCENT.GET
PERCENT.GET.PARM.LOW_NUMBER		.EQ SHAPE.HOPPER0_STEP+$70 ;$2bytes
						;$71 in use			;$71 in use
PERCENT.GET.PARM.HIGH_NUMBER	.EQ SHAPE.HOPPER0_STEP+$72 ;$2bytes
						;$73 in use			;$73 in use





;RANDOM.8 VARIABLES
;*****DO NOT CHANGE THE ORDER OF THESE VARIABLES.
;see note below
RND		.BS $2		;2byt
TMP		.BS $4
MOD		.BS $1
SEED0	.BS $1
SEED1	.BS $1
SEED2	.BS $1
SEED3	.BS $1
RND.LO	.BS $1
RND.HI	.BS $1
RAND_TABLE    .HS	01.01.00.FE.FF.01
RANDOM_NUMBER.ITERATION_TALLY	.BS $1
RND.ABORT	.BS $1	

SAVED.RANDOM.NUMBER	.BS $1 ;variable for storing a random number for future use. 

;IF $01 THEN RANDOM.8 WILL ABORT ON KEYPRESS AND LOAD $02 INTO THIS VARIABLE. 
;The issue seems to be with MOD. It appears to be a 2 byte variable used in .RA16,
;but if I define it as .BS $2, then the return value is always $00, not random. 
;by having it as .BS $1 .RA16 performs an operation on SEED0 (next variable in the list)
;intending it to be MOD+$1. Why this matter I don't know. SEED0 isn't supposed to be
;needed for 16-bit or 8bit return value. I tried doing an init of MOD+$1 and/OR SEED0
;to a power of 2 number but didn't work. 



;MULTIPLICATION BY 6 
;used to calculate the offset to the base address of the 16-bit array NPC.PATHFINDER.SEARCH.PATHS
; NPC.PATHFINDER.MULTIPLY_TABLE.LO	.HS 00.06.0C.12.18.1E.24.2A.30.36.3C.42.48.4E.54.5A.60.66.6C.72.78.7E.84.8A.90.96.9C.A2.A8.AE.B4.BA.C0.C6.CC.D2.D8.DE.E4.EA.F0.F6.FC.02.08.0E.14.1A.20.26.2C.32.38.3E.44.4A.50.56.5C.62.68.6E.74.7A.80
; NPC.PATHFINDER.MULTIPLY_TABLE.HO	.HS 00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01

;MULTIPLICATION BY 8
; NPC.PATHFINDER.MULTIPLY_TABLE.LO	.HS	00.08.10.18.20.28.30.38.40.48.50.58.60.68.70.78.80.88.90.98.A0.A8.B0.B8.C0.C8.D0.D8.E0.E8.F0.F8.00.08.10.18.20.28.30.38.40.48.50.58.60.68.70.78.80.88.90.98.A0.A8.B0.B8.C0.C8.D0.D8.E0.E8.F0.F8.00.08.10.18.20.28.30.38.40.48.50.58.60.68.70.78.80.88.90.98.A0.A8.B0.B8.C0.C8.D0.D8.E0.E8.F0.F8.00.08.10.18.20.28.30.38.40.48.50.58.60.68.70.78.80.88.90.98.A0.A8.B0.B8.C0.C8.D0.D8.E0.E8.F0.F8.00.08.10.18.20.28.30.38.40.48.50.58.60.68.70.78.80.88.90.98.A0.A8.B0.B8.C0.C8.D0.D8.E0.E8.F0.F8.00.08.10.18.20.28.30.38.40.48.50.58.60.68.70.78.80.88.90.98.A0.A8.B0.B8.C0.C8.D0.D8.E0.E8.F0.F8.00.08.10.18.20.28.30.38.40.48.50.58.60.68.70.78.80.88.90.98.A0.A8.B0.B8.C0.C8.D0.D8.E0.E8.F0.F8.00.08.10.18.20.28.30.38.40.48.50.58.60.68.70.78.80.88.90.98.A0.A8.B0.B8.C0.C8.D0.D8.E0.E8.F0.F8
; NPC.PATHFINDER.MULTIPLY_TABLE.HO	.HS	00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07
@END

;TEXT
@START
GRAPHICS 			.EQ	$C050
TEXT				.EQ $C051
HIRES				.EQ	$C057
PAGE1				.EQ	$C054
MIXOFF				.EQ	$C052
PAGE2				.EQ	$C055		
	
	
;TEXT SCREEN VARIABLES 
CLEAR.TEXT.SCREEN	.EQ $FC58
HCG.ON.ADDRESS		.EQ $0300
COUT.ADDRESS		.EQ $FDED			;SEND ACC TO DEFAULT OUTPUT DEVICE
;COUT				.EQ $FDED			;SEND ACC TO DEFAULT OUTPUT DEVICE
COUT.V				.EQ	$FDF0			;SEND ACC TO VIDEO SCREEN
UPDATE.CHAR.POS.ADDRESS		.EQ	$FC22			;RECALCULATE TEXT SCREEN LINE VALUE STORED AT $28, BASED ON ROW VALUE STORED AT $25
UPDATE.CURSOR.POS.ADDRESS 	.EQ UPDATE.CHAR.POS.ADDRESS
;UPDATE.CURSOR.POS		 	.EQ UPDATE.CHAR.POS

CURSOR.POSITION.SAVED		.BS $2 ;$2byts. 


HTAB				.EQ $24				;(X) HORIZONTAL CURSOR POSITION
VTAB				.EQ $25				;(Y) VERTICLE CURSOR POSITION
COUT_CHAR_TYPE		.BS $1			;applied as a mask to character shape data ($00 = normal, $7F = inverse)
;Text Window: ROM (the only problem with it is that it sucks)
TW1					.EQ $20				;(X) TEXT WINDOW UPPER LEFT
TW2					.EQ	$21				;(X) WIDTH
TW3					.EQ $22				;(Y) TOP ROW
TW4					.EQ	$23				;(Y) BOTTOM ROW

;Text Window: Custom
;See ========NPC TALK=====(TEXT WINDOW FUNCTION)  in offloaded_variables.ASM


CHAR				.BS		$1			

CSW					.EQ		$36
VECT				.EQ		$3EA		

;PRINT.STR VARIBLES
STRING				.EQ 	$FC		;2byte. Pointer to the ascii string to be output to video screen.
PRINT.STR.MODE		.BS		$1		;($00 = normal | >=$01 = wait/pause functionality enabled)
SCROLL_SPEED.INCREMENT 	.EQ $20
SCROLL_SPEED.MAX		.EQ $80
SCROLL_SPPED.PAUSE_KEY	.EQ $A0		;#CONSTANT. ASCII CODE.

;PRINT.STR.CENTERED
PRINT.STR.CENTERED.HTAB					.EQ DIVIDEND32+$0
PRINT.STR.CENTERED.VTAB					.EQ DIVIDEND32+$1
PRINT.STR.CENTERED.WINDOW_SIZE			.EQ DIVIDEND+$0
PRINT.STR.CENTERED.WINDOW_SIZE.PLUS_1	.EQ DIVIDEND+$1

;PRINT.STR.APPEND_RIGHT
PRINT.STR.APPEND_RIGHT.HTAB					.EQ PRINT.STR.CENTERED.HTAB
PRINT.STR.APPEND_RIGHT.VTAB					.EQ PRINT.STR.CENTERED.VTAB
PRINT.STR.APPEND_RIGHT.WINDOW_SIZE			.EQ PRINT.STR.CENTERED.WINDOW_SIZE
PRINT.STR.APPEND_RIGHT.WINDOW_SIZE.PLUS_1	.EQ PRINT.STR.CENTERED.WINDOW_SIZE.PLUS_1
PRINT.STR.APPEND_RIGHT.BUFFER				.EQ SHARED.VARIABLE_SPACE.BLOCK3 ;$20 bytes
PRINT.STR.APPEND_RIGHT.BUFFER.SIZE			.EQ $20

;PRINT.BCD_PACKED
PRINT.BCD_PACKED.PARM.MODE					.EQ OP1	;($00 = left justify | $01 = right justify)

;PRINT.TEXT.WINDOW.HEX16
PRINT.TEXT.WINDOW.HEX16.PARM.MODE			.EQ OP1 ;($00 = left justify | $01 = right justify)

; ;PRINT.BCD VARIABLES
; BCD.DIGITS 			.BS 1
@END



@END


;======MAP OBJECT ARRAYS=====
@START
;See beginning of this file

MAP_OBJECT.ARRAY.POINTER 					.EQ $E6	;#POINTER. Used to lookup the address of a specific map object array
MAP_OBJECT.ARRAY.CODE						.EQ SHAPE.HOPPER0_STEP+$9 ;($FE = MAP.OBJECTS.MOB | $FF = MAP.OBJECTS.NPC)
MAP_OBJECT.ARRAY.S_ENTITY.TYPE				.EQ SHAPE.HOPPER0_STEP+$A
MAP_OBJECT.ARRAY.MO_INDEX					.EQ SHAPE.HOPPER0_STEP+$B
;MAP_OBJECT.ARRAY.ERROR_TRAP_OVERRIDE		.EQ SHAPE.HOPPER0_STEP+$  ;(!= $FF = override off | $FF = override on )
;MAP_OBJECT.ARRAY.S_ENTITY.NOT_FOUND		.EQ SHAPE.HOPPER0_STEP+$  ;($00 = S_ENTITY found | $01 = S_ENTITY not found)

;MAP_OBJECT.RECORD.WRITE	.EQ SHAPE.HOPPER0_STEP+$0	 ;8bytes. contains data to write to a map object record
MAP_OBJECT.RECORD.WRITE	.EQ SHARED.VARIABLE_SPACE.BLOCK3 ;8bytes. contains data to write to a map object record. This shared block was the original shape hopper. Also used by ANIMATION.SHAPE.HOPPER
MAP_OBJECT.RECORD.READ	.EQ MAP_OBJECT.RECORD.WRITE   ;contains data read from a map object record

NPC.SCHEDULE		.EQ SWAP_SPACE.MAIN_MEMORY+$2400		;$300 bytes. tracks the map location, time of day, and other information which determines where on the map each NPC is located. 
					;$BA00
					
MAP_OBJECTS.MOB		.EQ SWAP_SPACE.MAIN_MEMORY+$2700		;$100 bytes. tracks x,y position (relative to player) and other data for MOBs (i.e. monsters, enemies of player)
					;$BD00

MAP_OBJECTS.GENERAL	.EQ SWAP_SPACE.MAIN_MEMORY+$2800		;$100 bytes. tracks x,y position (relative to player) and other data of transport map objects and "other" map objects
					;$BE00
MAP_OBJECTS.GENERAL.OFFSET .EQ $2800	;$1700, $D00					;the size of the offset in the above definition
MAP_OBJECTS.GENERAL.AUX .EQ SWAP_SPACE.AUX_MEMORY+MAP_OBJECTS.GENERAL.OFFSET ;the address of this map objects array when it is swapped out to aux memory via SWAP.MAIN_MEMORY.OUT 

MAP_OBJECTS.NPC		.EQ SWAP_SPACE.MAIN_MEMORY+$2900		;$100 bytes. tracks actual x,y map coordinates and other data of NPCs (non-player characters)
					;$BF00

;LOAD LOCATION FOR SPRITE DATA FILES
DATA.LOAD.ADDRESS.SURFACE 	.EQ NPC.SCHEDULE
DATA.LOAD.ADDRESS.BUILDINGS .EQ NPC.SCHEDULE
DATA.LOAD.ADDRESS.UNDERMAP 	.EQ NPC.SCHEDULE

DATA.SPR.WRITE.ADDRESS	 .EQ NPC.SCHEDULE

@END

;========SCREEN ARRAYS====
@START
;(hold data which determines the tile makeup on the view screen)


;SCREEN.TILE.DATA 			.BS $BB						;HOLDS TILE_TYPE DATA FOR EACH TILE ON VIEW SCREEN
SCREEN.TILE.DATA 			.EQ $0800	;$BB bytes.					;HOLDS TILE_TYPE DATA FOR EACH TILE ON VIEW SCREEN

;	mirrors screen tile layout. 
;	Values for each tile are 0 (normal), 1 (dark)
;SCREEN.DARK.DATA			.BS $BB		;ENDS AT $92BA
SCREEN.DARK.DATA			.EQ $8BB	;$BB bytes. $00 = visible, $01 = hidden (dark)

;SCREEN.DARK.DATA_BEFORE	.BS $BB		
SCREEN.DARK.DATA_BEFORE		.EQ $0976	;$BB bytes.	

;	the data from screen.dark.data the move before


SCREEN.MO_SPRITE.DATA		.EQ $0A31		;$BB bytes. TRACKS TILE LOCATION OF MAP OBJECTS (MOB) WHOS X,Y IS ON VIEW SCREEN

SCREEN.MO_SPRITE_TYPE.DATA		.EQ $0AEC	;$BB bytes. (ends $0BA6). tracks the type of sprite located in the corresponding element of SCREEN.MO_SPRITE.DATA. See constants below for the type code. This is necessary so that ANIMATION_MANAGER.ASM knows which map objects array to look in. 
S_ENTITY_TYPE.NC_MOB	.EQ $00	;#CONSTANT: non-combat mob code for use with SCREEN.MO_SPRITE_TYPE.DATA
S_ENTITY_TYPE.C_MOB		.EQ $01	;#CONSTANT: combat mob code for use with SCREEN.MO_SPRITE_TYPE.DATA
S_ENTITY_TYPE.SPECIAL	.EQ $02	;#CONSTANT: combat "special" S_ENTITY, for use with SCREEN.MO_SPRITE_TYPE.DATA
S_ENTITY_TYPE.PC		.EQ $03	;#CONSTANT: player characer code for use with SCREEN.MO_SPRITE_TYPE.DATA
S_ENTITY_TYPE.BLD_NPC	.EQ $04	;#CONSTANT: building NPC code for use with SCREEN.MO_SPRITE_TYPE.DATA
S_ENTITY_TYPE.DNG_NPC	.EQ $05	;#CONSTANT: Dungeon NPC code for use with SCREEN.MO_SPRITE_TYPE.DATA



SCREEN.MO_GENERAL.DATA	.BS $BB		;TRACKS TILE LOCATION OF MAP OBJECTS (MOB) WHOS X,Y IS ON VIEW SCREEN
;SCREEN.MO_GENERAL.DATA	.EQ $07A7	;TRACKS TILE LOCATION OF MAP OBJECTS (MOB) WHOS X,Y IS ON VIEW SCREEN





;==========BORROWED MEMORY: THESE ARRAYS MUST STAY IN THIS SEQUENCE;===========
;NOTE: These memory locations are borrowed as a contiguous block by other arrays.
SCREEN.TILE.HOPPER			.BS $0B						;HOLDS ONE COLUMN OF TILES TILE_TYPE DATA ON DECK FOR SCREEN DRAWING (USED AFTER SCREEN SCROLLING)
SCREEN.DARK.HOPPER			.BS $0B						;(MIRROR OF SCREEN.TILE.HOPPER, BUT FOR DARKNESS FLAGS)
;================================================================================							

SCREEN.TILE.ROW.HOPPER 		.EQ SCREEN.TILE.HOPPER		;$11bytes HOLDS ONE ROW OF TILE_TYPE DATA. USED BY DARKNESS.ELS FOR OFFSCREEN TILE. BORROWS MEMORY FROM BOTH SCREEN.ROW.HOPPER AND SCREEN.DARK.HOPPER
@END
							
;========SCREEN TABLES====
@START
;(table related to view screen operations)

;Line Lookup Tables. LINE.LO is used for Hi-Res Page 1 and Page 2. 
;*****see routines_graphics.asm****
;LINE.HO.P1 		.HS 20.24.28.2C.30.34.38.3C.20.24.28.2C.30.34.38.3C.21.25.29.2D.31.35.39.3D.21.25.29.2D.31.35.39.3D.22.26.2A.2E.32.36.3A.3E.22.26.2A.2E.32.36.3A.3E.23.27.2B.2F.33.37.3B.3F.23.27.2B.2F.33.37.3B.3F.20.24.28.2C.30.34.38.3C.20.24.28.2C.30.34.38.3C.21.25.29.2D.31.35.39.3D.21.25.29.2D.31.35.39.3D.22.26.2A.2E.32.36.3A.3E.22.26.2A.2E.32.36.3A.3E.23.27.2B.2F.33.37.3B.3F.23.27.2B.2F.33.37.3B.3F.20.24.28.2C.30.34.38.3C.20.24.28.2C.30.34.38.3C.21.25.29.2D.31.35.39.3D.21.25.29.2D.31.35.39.3D.22.26.2A.2E.32.36.3A.3E.22.26.2A.2E.32.36.3A.3E.23.27.2B.2F.33.37.3B.3F.23.27.2B.2F.33.37.3B.3F
;LINE.LO			.HS 00.00.00.00.00.00.00.00.80.80.80.80.80.80.80.80.00.00.00.00.00.00.00.00.80.80.80.80.80.80.80.80.00.00.00.00.00.00.00.00.80.80.80.80.80.80.80.80.00.00.00.00.00.00.00.00.80.80.80.80.80.80.80.80.28.28.28.28.28.28.28.28.A8.A8.A8.A8.A8.A8.A8.A8.28.28.28.28.28.28.28.28.A8.A8.A8.A8.A8.A8.A8.A8.28.28.28.28.28.28.28.28.A8.A8.A8.A8.A8.A8.A8.A8.28.28.28.28.28.28.28.28.A8.A8.A8.A8.A8.A8.A8.A8.50.50.50.50.50.50.50.50.D0.D0.D0.D0.D0.D0.D0.D0.50.50.50.50.50.50.50.50.D0.D0.D0.D0.D0.D0.D0.D0.50.50.50.50.50.50.50.50.D0.D0.D0.D0.D0.D0.D0.D0.50.50.50.50.50.50.50.50.D0.D0.D0.D0.D0.D0.D0.D0
;LINE.HO.P2 		.HS 40.44.48.4C.50.54.58.5C.40.44.48.4C.50.54.58.5C.41.45.49.4D.51.55.59.5D.41.45.49.4D.51.55.59.5D.42.46.4A.4E.52.56.5A.5E.42.46.4A.4E.52.56.5A.5E.43.47.4B.4F.53.57.5B.5F.43.47.4B.4F.53.57.5B.5F.40.44.48.4C.50.54.58.5C.40.44.48.4C.50.54.58.5C.41.45.49.4D.51.55.59.5D.41.45.49.4D.51.55.59.5D.42.46.4A.4E.52.56.5A.5E.42.46.4A.4E.52.56.5A.5E.43.47.4B.4F.53.57.5B.5F.43.47.4B.4F.53.57.5B.5F.40.44.48.4C.50.54.58.5C.40.44.48.4C.50.54.58.5C.41.45.49.4D.51.55.59.5D.41.45.49.4D.51.55.59.5D.42.46.4A.4E.52.56.5A.5E.42.46.4A.4E.52.56.5A.5E.43.47.4B.4F.53.57.5B.5F.43.47.4B.4F.53.57.5B.5F

;HO/LO address for the tile shape tables in AUX memory. 
TILE.SHAPES.LO		 		.HS	00.20.40.60.80.A0.C0.E0.00.20.40.60.80.A0.C0.E0.00.20.40.60.80.A0.C0.E0.00.20.40.60.80.A0.C0.E0.00.20.40.60.80.A0.C0.E0.00.20.40.60.80.A0.C0.E0.00.20.40.60.80.A0.C0.E0.00.20.40.60.80.A0.C0.E0.00.20.40.60.80.A0.C0.E0.00.20.40.60.80.A0.C0.E0.00.20.40.60.80.A0.C0.E0.00.20.40.60.80.A0.C0.E0.00.20.40.60.80.A0.C0.E0.00.20.40.60.80.A0.C0.E0.00.20.40.60.80.A0.C0.E0.00.20.40.60.80.A0.C0.E0.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80.00.80
TILE.SHAPES.HO		 		.HS	70.70.70.70.70.70.70.70.71.71.71.71.71.71.71.71.72.72.72.72.72.72.72.72.73.73.73.73.73.73.73.73.74.74.74.74.74.74.74.74.75.75.75.75.75.75.75.75.76.76.76.76.76.76.76.76.77.77.77.77.77.77.77.77.78.78.78.78.78.78.78.78.79.79.79.79.79.79.79.79.7A.7A.7A.7A.7A.7A.7A.7A.7B.7B.7B.7B.7B.7B.7B.7B.7C.7C.7C.7C.7C.7C.7C.7C.7D.7D.7D.7D.7D.7D.7D.7D.7E.7E.7E.7E.7E.7E.7E.7E.7F.7F.7F.7F.7F.7F.7F.7F.80.80.81.81.82.82.83.83.84.84.85.85.86.86.87.87.88.88.89.89.8A.8A.8B.8B.8C.8C.8D.8D.8E.8E.8F.8F.90.90.91.91.92.92.93.93.94.94.95.95.96.96.97.97.98.98.99.99.9A.9A.9B.9B.9C.9C.9D.9D.9E.9E.9F.9F.A0.A0.A1.A1.A2.A2.A3.A3.A4.A4.A5.A5.A6.A6.A7.A7.A8.A8.A9.A9.AA.AA.AB.AB.AC.AC.AD.AD.AE.AE.AF.AF.B0.B0.B1.B1.B2.B2.B3.B3.B4.B4.B5.B5.B6.B6.B7.B7.B8.B8.B9.B9.BA.BA.BB.BB.BC.BC.BD.BD.BE.BE.BF.BF
;array elements (!DEC)			00.01.02.03.04.05.06.07.08.09.0A.0B.0C.0D.0E.0F.10.11.12.13.14.15.16.17.18.19.1A.1B.1C.1D.1E.1F.20.21.22.23.24.25.26.27.28.29.2A.2B.2C.2D.2E.2F.30.31.32.33.34.35.36.37.38.39.3A.3B.3C.3D.3E.3F.40.41.42.43.44.45.46.47.48.49.4A.4B.4C.4D.4E.4F.50.51.52.53.54.55.56.57.58.59.5A.5B.5C.5D.5E.5F.60.61.62.63.64.65.66.67.68.69.6A.6B.6C.6D.6E.6F.70.71.72.73.74.75.76.77.78.79.7A.7B.7C.7D.7E.7F.80.81.82.83.84.85.86.87.88.89.8A.8B.8C.8D.8E.8F.90.91.92.93.94.95.96.97.98.99.9A.9B.9C.9D.9E.9F.A0.A1.A2.A3.A4.A5



;the current element of the screen.tile.data (/aka sindex, screen.dark.data or screen.dark.data_before arrays, are the index to the row/column arrays. return value is the column/row #.
SCREEN.INDEX.COLUMN			.HS	00.01.02.03.04.05.06.07.08.09.0A.0B.0C.0D.0E.0F.10.00.01.02.03.04.05.06.07.08.09.0A.0B.0C.0D.0E.0F.10.00.01.02.03.04.05.06.07.08.09.0A.0B.0C.0D.0E.0F.10.00.01.02.03.04.05.06.07.08.09.0A.0B.0C.0D.0E.0F.10.00.01.02.03.04.05.06.07.08.09.0A.0B.0C.0D.0E.0F.10.00.01.02.03.04.05.06.07.08.09.0A.0B.0C.0D.0E.0F.10.00.01.02.03.04.05.06.07.08.09.0A.0B.0C.0D.0E.0F.10.00.01.02.03.04.05.06.07.08.09.0A.0B.0C.0D.0E.0F.10.00.01.02.03.04.05.06.07.08.09.0A.0B.0C.0D.0E.0F.10.00.01.02.03.04.05.06.07.08.09.0A.0B.0C.0D.0E.0F.10.00.01.02.03.04.05.06.07.08.09.0A.0B.0C.0D.0E.0F.10
SCREEN.INDEX.ROW			.HS 00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.02.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.03.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.04.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.05.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.06.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.07.08.08.08.08.08.08.08.08.08.08.08.08.08.08.08.08.08.09.09.09.09.09.09.09.09.09.09.09.09.09.09.09.09.09.0A.0A.0A.0A.0A.0A.0A.0A.0A.0A.0A.0A.0A.0A.0A.0A.0A
SCREEN.INDEX.TILE_SBYTE		.HS 02.04.06.08.0A.0C.0E.10.12.14.16.18.1A.1C.1E.20.22.02.04.06.08.0A.0C.0E.10.12.14.16.18.1A.1C.1E.20.22.02.04.06.08.0A.0C.0E.10.12.14.16.18.1A.1C.1E.20.22.02.04.06.08.0A.0C.0E.10.12.14.16.18.1A.1C.1E.20.22.02.04.06.08.0A.0C.0E.10.12.14.16.18.1A.1C.1E.20.22.02.04.06.08.0A.0C.0E.10.12.14.16.18.1A.1C.1E.20.22.02.04.06.08.0A.0C.0E.10.12.14.16.18.1A.1C.1E.20.22.02.04.06.08.0A.0C.0E.10.12.14.16.18.1A.1C.1E.20.22.02.04.06.08.0A.0C.0E.10.12.14.16.18.1A.1C.1E.20.22.02.04.06.08.0A.0C.0E.10.12.14.16.18.1A.1C.1E.20.22.02.04.06.08.0A.0C.0E.10.12.14.16.18.1A.1C.1E.20.22
SCREEN.INDEX.TILE_LINE		.HS 08.08.08.08.08.08.08.08.08.08.08.08.08.08.08.08.08.18.18.18.18.18.18.18.18.18.18.18.18.18.18.18.18.18.28.28.28.28.28.28.28.28.28.28.28.28.28.28.28.28.28.38.38.38.38.38.38.38.38.38.38.38.38.38.38.38.38.38.48.48.48.48.48.48.48.48.48.48.48.48.48.48.48.48.48.58.58.58.58.58.58.58.58.58.58.58.58.58.58.58.58.58.68.68.68.68.68.68.68.68.68.68.68.68.68.68.68.68.68.78.78.78.78.78.78.78.78.78.78.78.78.78.78.78.78.78.88.88.88.88.88.88.88.88.88.88.88.88.88.88.88.88.88.98.98.98.98.98.98.98.98.98.98.98.98.98.98.98.98.98.A8.A8.A8.A8.A8.A8.A8.A8.A8.A8.A8.A8.A8.A8.A8.A8.A8


;The order of tile are checked in for obscuring tiles, by DARKNESS.REVIEW
SCREEN.DARK.SEARCH_INDEX	.HS 6D.5C.4B.4C.4D.5E.6F.6E.39.4A.5B.6C.7D.3A.3B.3C.3D.4E.5F.70.81.7E.7F.80.27.38.49.5A.6B.7C.8D.28.29.2A.2B.2C.2D.3E.4F.60.71.82.93.8E.8F.90.91.92.15.26.37.48.59.6A.7B.8C.9D.16.17.18.19.1A.1B.1C.1D.2E.3F.50.61.72.83.94.A5.9E.9F.A0.A1.A2.A3.A4.14.25.36.47.58.69.7A.8B.9C.1E.2F.40.51.62.73.84.95.A6.13.24.35.46.57.68.79.8A.9B.1F.30.41.52.63.74.85.96.A7.12.23.34.45.56.67.78.89.9A.20.31.42.53.64.75.86.97.A8

;Multiplication table for calculating the north/south offset for multiple tiles. 
;OTHER USE: ROW # IS THE INDEX, RETURNS THE SCREEN TILE # OF THE FIRST COLUMN IN THE SPECIFIED ROW. 
;(used in DARKNESS.ELS)
SCREEN.MULTIPLY_TABLE		.HS	00.11.22.33.44.55.66.77.88.99.AA	

;ROW # IS THE INDEX, RETURNS THE SCREEN TILE # OF THE LAST COLUMN IN THE SPECIFIED ROW. 
;(used in DARKNESS.ELS)
SCREEN.LAST_COLUMN.TABLE	.HS	10.21.32.43.54.65.76.87.98.A9.BA

SCREEN.LAST_ROW.TABLE		.HS	AA.AB.AC.AD.AE.AF.B0.B1.B2.B3.B4.B5.B6.B7.B8.B9.BA

@END

;========SHAPE TABLE OPERATIONS=======
@START
SHAPE							.EQ	$FA				;2byt			;Used by DRAW.TILE. Holds the pointer to the active shape table to be drawn. 	
SHAPE2							.EQ	$FC				;2byt			;Used by DRAW.TILE. Holds the pointer to the active shape table to be drawn. 	
; SHAPE.HOPPER0					.BS $20  			;!32 BYTES
; SHAPE.HOPPER1					.BS $20
; SHAPE.HOPPER2					.BS $20
; SHAPE.HOPPER3					.BS $20
; SHAPE.HOPPER4					.BS $20
; SHAPE.HOPPER5					.BS $20
; SHAPE.HOPPER6					.BS $20
; SHAPE.HOPPER7					.BS $20

SHAPE.HOPPER.END	.EQ SHAPE.HOPPER0_STEP+$FF	
SHAPE.HOPPER0		.EQ SHAPE.HOPPER0_STEP	;$100bytes. Keyboard Buffer. Don't share this variable space that uses ROM input routines (none currently are in use).
SHAPE.HOPPER1		.EQ SHAPE.HOPPER0+$20
SHAPE.HOPPER2		.EQ SHAPE.HOPPER0+$40
SHAPE.HOPPER3		.EQ SHAPE.HOPPER0+$60
SHAPE.HOPPER4		.EQ SHAPE.HOPPER0+$80
SHAPE.HOPPER5		.EQ SHAPE.HOPPER0+$A0
SHAPE.HOPPER6		.EQ SHAPE.HOPPER0+$C0
SHAPE.HOPPER7		.EQ SHAPE.HOPPER0+$E0

;SHAPE.HOPPER0			.EQ SHAPE.HOPPER0
SHAPE.HOPPER0.AND_MASK	.EQ SHAPE.HOPPER0
SHAPE.HOPPER0.ORA_MASK	.EQ SHAPE.HOPPER1
SHAPE.HOPPER1.AND_MASK	.EQ SHAPE.HOPPER2
SHAPE.HOPPER1.ORA_MASK	.EQ SHAPE.HOPPER3
SHAPE.HOPPER2.AND_MASK	.EQ SHAPE.HOPPER4
SHAPE.HOPPER2.ORA_MASK	.EQ SHAPE.HOPPER5
SHAPE.HOPPER3.AND_MASK	.EQ SHAPE.HOPPER6
SHAPE.HOPPER3.ORA_MASK	.EQ SHAPE.HOPPER7

;SHAPE.HOPPER1					.EQ	SHARED.VARIABLE_SPACE.BLOCK1+$CD ;$20bytes

SHP.TBL.CNTR					.BS	$1				;1byt
TILE.LINE.CNTR					.BS $1

SHAPE.SIZE						.EQ $1F				;#CONSTANT		(NUMBER OF BYTES-1) IN EACH TILE SHAPE TABLE
SHP.TBL.START.ADDRESS			.EQ $7000			;#CONSTANT. THE STARTING ADDRESS IN AUX MEMORY OF THE TILE SHAPE TABLES


@END





;-------START ABC ORDER--------



;======ANIMATION_MANAGER======
@START

;OPT NOTE: animation must be available
;from almost everywhere, if not everywhere.
;examples: during combat shape draws, NPC Talk, during map object manager execution during combat (because for mobs combat.fire.projectile is called)

ANIMATION.SHAPE.HOPPER			.EQ SHARED.VARIABLE_SPACE.BLOCK3 ;$20 bytes. This shared block was the original shape hopper before combat.fire.projectile was created and the shape hopper was pointed to the kb buffer ($200)
;ANIMATION.SHAPE.HOPPER			.bs $20 ;$20 bytes. This shared block was the original shape hopper before combat.fire.projectile was created and the shape hopper was pointed to the kb buffer ($200)


ANIMATION.CALLED_BY				.BS $1			;Used to track the routine which called animation manager. $00 = not tracked (no special handling), $01 = KEYIN.STRING. This variable is reset when ANIMATION.MANAGER exits
ANIMATION.FORCED				.BS $1			;IF FLAG IS SET ($01), THEN AN ANIMATION ABORT TO DUE PLAYER KEY PRESS IS NOT ALLOWED, FORCING THE FULL DRAW OF THE NEXT ANIMATION FRAME.  
ANIMATION.FORCED.OVERRIDE		.BS $1			;USED FOR PLAYTESTING. IF FLAG IS SET ($01), THEN AN ANIMATION ABORT TO DUE PLAYER KEY PRESS IS NOT ALLOWED, FORCING THE FULL DRAW OF THE NEXT ANIMATION FRAME.   
ANIMATION.FRAME_STATE			.BS	$1			;CURRENT ANIMATION FRAME FOR ALL ANIMATION TILES ON THE VIEW SCREEN (0-3)
ANIMATION.DELAY.OVERRIDE		.BS $1 			;IF FLAG IS SET ($01) THEN THE ANIMATION DELAY IS SKIPPED. OVERRIDE IS AUTOMATICALLY INIT TO OFF AFTER EACH CALL TO ANIMATION.UPDATE
ANIMATION.TILE_RANGE.START		.EQ $80			;#CONSTANT	;BEGINNING OF ANIMATION TILE SECTION OF THE 256 TILE SHAPES.
ANIMATION.TOTAL_FRAMES			.EQ $03			;#CONSTANT	;TOTAL ANIMATION FRAMES PER TILE (0 COUNTS)

ANIMATION.SCREEN.TALLY			.BS	$1			;A TALLY OF THE NUMBER OF ANIMATED TILES ON THE VIEW SCREEN. UPDATED AFTER COMPLETING AN ANIMATION FRAME CYCLE. USED TO DETERMINE IF A DELAY SHOULD BE INSERTED TO SMOOTH OUT ANIMATION SPEED
ANIMATION.DEEP_WATER.TALLY		.BS $1			;A TALLY OF THE NUMEBR OF DEEP WATER TILES ON THE VIEW SCREEN, used to determine whether to force animation to complete when entire screen is deep water tiles

ANIMATION.DELAY_TRIGGER1		.EQ	$3E			;#CONSTANT	;A DELAY IS TRIGGERD IF MORE THAN THIS MANY ANIMATION TILES ARE ON THE CURRENT VIEW SCREEN
ANIMATION.DELAY1				.EQ $02			;#CONSTANT	;number of JSR WAIT (/w LDA #$FF) to delay for associated trigger
ANIMATION.DELAY_TRIGGER2		.EQ	$7C			;#CONSTANT	;A DELAY IS TRIGGERD IF MORE THAN THIS MANY ANIMATION TILES ARE ON THE CURRENT VIEW SCREEN
ANIMATION.DELAY2				.EQ $01			;#CONSTANT	;number of JSR WAIT (/w LDA #$FF) to delay for associated trigger
ANIMATION.WATER_RANGE.START 	.EQ $88			;#CONSTANT	;FIRST WATER TILE IN THE TILE SET. 
ANIMATION.WATER_RANGE.END 		.EQ $8C			;#CONSTANT	;LAST WATER TILE IN THE TILE SET + $1 

ANIMATION.CURRENT_TILE_TYPE		.BS	$1			;HOLDS THE TILE_TYPE FOR THE CURRENT TILE LOCATION BEING PROCESSED (HOLDS A TILE_TYPE LOADED FROM SCREEN.TILE.DATA OR SCREEN.MO_SPRITE.DATA)

ANIMATION.SCROLL.HOPPER 		.BS $2			;USED WHEN A BYTES FROM ANOTHER SHAPE ARE COPIED IN TO A SCROLLING TILE (EXAMPLE, PLAYER ICON SINKING IN WATER)
ANIMATION.SCROLL.COUNTER		.EQ SHP.TBL.CNTR	;USED IN .LOOP.SCROLL IN ANIMATION.SCROLL.PLAYER TO COUNT THE BYTES READ BY THE LOOP
;the following are used to determine which bytes to load from water shape table when player is sinking
;ANIMATION.SCROLL.WATER_TABLE0	.HS 00.02.04.06.08.0A.0C.0E.10.12.14.16.18.1A.1C.1E
;ANIMATION.SCROLL.WATER_TABLE1	.HS 01.03.05.07.09.0B.0D.0F.11.13.15.17.19.1B.1D.1F
		
;ANIMATION.SPRITE.RECORD			.BS $C
	
;MULTI-TILE MOB HANDLING
ANIMATION.MT.TRACKING			.BS $10			;10byt	TRACKS MT OBJECTS AS ANIMATION MANAGER REVIEWS THE SCREEN. THE DATA IS USED TO DETERMINE THE TILE_TYPE TO USE FOR EACH OF THE 4 MT TILE #S PER OBJECT. 
;ANIMATION.MT.TRACKING			.EQ SCREEN.TILE.HOPPER	;10byt	TRACKS MT OBJECTS AS ANIMATION MANAGER REVIEWS THE SCREEN. THE DATA IS USED TO DETERMINE THE TILE_TYPE TO USE FOR EACH OF THE 4 MT TILE #S PER OBJECT. 

;**OPT** Memory. try setting animation tracking back to .EQ value (I don't think it solved the problem I was troubleshooting at the time. Or otherwise set to the original shape hopper, taking into consideration
;that the map object read buffer points there too)


ANIMATION.MT.INDEX				.BS $1			;HOLDS THE RECORD INDEX TO THE MOB IN THE CURRENT SCREEN LOCATION
ANIMATION.MT.TILE_NUMBER		.BS	$1			;HOLDS THE CURRENT MT TILE NUMBER OF THE MOB IN THE CURRENT SCREEN LOCATION
ANIMATION.MT.FINAL_TILE_TYPE	.BS $3		;HOLDS THE FINAL TILE_TYPE FOR MT MOBS, NET OF ANY ADJUSTMENTS FOR THE MT TILE # (0-3)

ANIMATION.XREG.STORAGE			.BS $1
ANIMATION.YREG.STORAGE			.BS $1
ANIMATION.YREG.STORAGE1			.BS $1


;WATER SCROLLLING
TILE.LINE.COPY_TO				.EQ SHARED.VARIABLE_SPACE.BLOCK3+$00
TILE.LINE.COPY_FROM 			.EQ SHARED.VARIABLE_SPACE.BLOCK3+$01
TILE.LINE.FIRST					.EQ SHARED.VARIABLE_SPACE.BLOCK3+$02	;FIRST LINE IN THE TILE, TO WHICH THE BUFFER (HOLDING THE LAST LINE OF THE TILE) GETS COPIED

@END

;=====COMBAT (DISK MODULE)======
@START

;MAIN COMBAT PROGRAM
@START
COMBAT.MAIN.MODULE.START		.EQ SWAP_SPACE.MAIN_MEMORY.ALT.ADDR
COMBAT.MAIN.MODULE.END			.EQ $A200 ;this memory range is clobbered when the spell file is loaded
COMBAT.PERSISTANT.MEMORY.START	.EQ COMBAT.MAIN.MODULE.END+$0 ;(not +$1 because COMBAT.MAIN.MODULE.END is used with .NO directive which stops 1 byte less than the value of the memory address specified). This memory range is never clobbered until combat exist. It contains the map object arrays and subroutines which are access from the spell file. 

COMBAT.FIRST_PC_TURN.FLAG		.EQ COMBAT.OUT_OF_ORDER.SHARED.MEMORY15_STEP	;($00 = first PC turn | >=$01 not first PC turn)


;SPELL FILE 
@START
;OFFSETS: see end of spell file
;
;PARAMETERS FOR FILE MANAGEMENT
;
PARM.SEEK_BYTES		.EQ SHAPE.HOPPER0_STEP+$0 ;$2 byte
PARM.READ_BYTES		.EQ SHAPE.HOPPER0_STEP+$2 ;$2 byte
SPELL.FILE.ENTRANCE.BUFFER			.EQ	COMBAT.MAIN.MODULE.START ;#CONSTANT. Memory address that SPELL_FILE.ENTRANCE 
										;$9000				;is read into from disk. This routine parses spell code stored in the player 
															;keypress and then branches to the routine to load the associated spell code 														;block from disk.
SPELL.FILE.ENTRANCE.BUFFER.END		.EQ $94FF  ;#CONSTANT.
SPELL.FILE.CODE_BLOCK.BUFFER.START		.EQ	SPELL.FILE.ENTRANCE.BUFFER.END+1 ;#CONSTANT. Memory address that the graphics effects for spells are read into from disk
							;			;$9500
SPELL.FILE.CODE_BLOCK.BUFFER.SIZE	.EQ COMBAT.MAIN.MODULE.END-SPELL.FILE.CODE_BLOCK.BUFFER.START  ;#CONSTANT.
							;			;$D00	;spell code block size notes: (as of 3/20/2017: mass death ($549 bytes), Summon ($A66 bytes), Lightning ($A12 bytes). summon and lightning are consolidated and could be broken down if needed, but best to avoid if possible. 
							;					;Also for summon, there is probably material savings from consolidating GE.SPELL.ARMGDN_COW into GE.SPELL.SUMMON.STANDARD.ENTRANCE. The only difference is that cows can squish mobs and cow destination tiles are always random. 
							;					;This could be managed with a few parameters. 
SPELL.FILE.CODE_BLOCK.BUFFER.END	.EQ COMBAT.MAIN.MODULE.END
							;			;A200

SPELL.FILE.WEAPONS.SHAPE_TABLE.AUX.START	.EQ $D000 ;#CONSTANT. Location in aux memory where weapon shape tables are loaded. 

@END
 
;S_ENTITY ARRAYS
@START

;Maximum 6 PCs
COMBAT.MAP_OBJECTS.PC		.EQ MAP_OBJECTS.MOB ;$30bytes. Player character sprite data. 6 records, 4 bytes long.
COMBAT.MAP_OBJECTS.PC.SIZE	.EQ	$18 ;#CONSTANT. Number of bytes in COMBAT.MAP_OBJECTS.PC array
COMBAT.MAP_OBJECTS.PC.RECORD_SIZE	.EQ $04
COMBAT.MAP_OBJECTS.PC.MAX_QTY	.EQ $06 ;#CONTANT. max # of record of this S_ENTITY type


;Maximum 16 Specials
COMBAT.MAP_OBJECTS.SPECIAL		.EQ MAP_OBJECTS.NPC+$80 ;$80bytes. 16 special S_ENTITY record, 8 bytes each.
COMBAT.MAP_OBJECTS.SPECIAL.START_RECORD		.EQ $80 ;#CONSTANT; The first record number used by special S_ENTITIES.
COMBAT.MAP_OBJECTS.SPECIAL.RECORD_SIZE	.EQ $08
COMBAT.MAP_OBJECTS.SPECIAL.MAX_QTY		.EQ $10 ;#CONTANT. max # of record of this S_ENTITY type

COMBAT.MAP_OBJECTS.SPECIAL.TRUE_INDEX	.EQ MAP_OBJECTS.NPC ;use when accessing the special map object array from a 

;Maximum 16 MOBs
COMBAT.MAP_OBJECTS.MOB				.EQ MAP_OBJECTS.NPC ;$80bytes. 16 MOB record, 8 bytes each. 
COMBAT.MAP_OBJECTS.MOB.RECORD_SIZE	.EQ $08
COMBAT.MAP_OBJECTS.MOB.MAX_QTY		.EQ $10 ;#CONTANT. max # of record of this S_ENTITY type

@END


;SAVE VARIABLES AT COMBAT ENTRANCE
@START
COMBAT.GMAP.X.SAVED			.EQ	SHARED.VARIABLE_SPACE.BLOCK2+$0 ;$1bytes. Stores certain game variables upon entrance to the combat module so it can be restore upon exit
COMBAT.GMAP.Y.SAVED			.EQ	SHARED.VARIABLE_SPACE.BLOCK2+$1 ;$1bytes. ""
COMBAT.GMAP.X.LAST.SAVED	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$2 ;$1bytes. ""
COMBAT.GMAP.Y.LAST.SAVED	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$3 ;$1bytes. ""
COMBAT.RMAP.X.SAVED			.EQ SHARED.VARIABLE_SPACE.BLOCK2+$4 ;$1bytes. ""
COMBAT.RMAP.Y.SAVED			.EQ SHARED.VARIABLE_SPACE.BLOCK2+$5 ;$1bytes. ""
COMBAT.RMAP.SAVED			.EQ SHARED.VARIABLE_SPACE.BLOCK2+$6 ;$2bytes. ""
										;$7 in use
COMBAT.WZONE.SAVED			.EQ SHARED.VARIABLE_SPACE.BLOCK2+$8 ;$1bytes. ""



; COMBAT.CENTER_OF_SCREEN.X	.EQ $08 ;#CONSTANT
; COMBAT.CENTER_OF_SCREEN.Y	.EQ $05 ;#CONSTANT

COMBAT.PLAYER.MAP.LOCATION_TYPE.SAVED .EQ COMBAT.OUT_OF_ORDER.SHARED.MEMORY_D_STEP ;$1bytes. ""
COMBAT.TIME.SUN.STATUS.SAVED .EQ COMBAT.OUT_OF_ORDER.SHARED.MEMORY_E_STEP ;$1bytes. ""

@END

;SPELL OPERATIONS ;***also see spell special effects further down***
@START
COMBAT.SPELL_CASTER.GMAP.X .EQ COMBAT.OUT_OF_ORDER.SHARED.MEMORY_F_STEP ;$01 byte.
COMBAT.SPELL_CASTER.GMAP.Y .EQ COMBAT.OUT_OF_ORDER.SHARED.MEMORY10_STEP ;$01 byte.
COMBAT.PLAYER_SELECTED.TARGET.GMAP.X	.EQ COMBAT.OUT_OF_ORDER.SHARED.MEMORY11_STEP ;$01 byte.
COMBAT.PLAYER_SELECTED.TARGET.GMAP.Y	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$9 ;$01 byte.

SPECIAL.DATA.POINTER		.EQ $B0 ;#POINTER. $2bytes

;**OPT** Memory. The challenge with COMBAT.TARGET_HIT sharing space is that it can't use the shape related buffers since it is called from SHAPE.MOVE.
;the plan is to move it to the combat file when complete if memory is available. 

;COMBAT.TARGET_HIT.DB					.EQ SHAPE.HOPPER0+$C2 ;(out of order) $20 bytes. Used by each spell routine with capability to hit multiple targets to store the screen tile location of each target hit. 
COMBAT.TARGET_HIT.DB		.BS $20 ;$20 bytes. Used by each spell routine with capability to hit multiple targets to store the screen tile location of each target hit. 
COMBAT.TARGET_HIT.DB.SIZE	.EQ $20	;#CONSTANT
COMBAT.TARGET_HIT.DB.STOP_VALUE .EQ $FF ;#CONSTANT
COMBAT.TARGET_HIT.DB.INDEX	.BS $1
;COMBAT.TARGET_HIT.DB.INDEX	.EQ SHAPE.HOPPER0+$E2	;$1byte
COMBAT.SPELL_CODE.CAST		.EQ SHARED.VARIABLE_SPACE.BLOCK2+$A ;$01 byte.	($FF = not set, all other values are a spell code)
COMBAT.CAST.KEYPRESS.ASCII	.EQ COMBAT.SPELL_CODE.CAST
;COW .BS $1
COMBAT.ACTIVE_PLAYER.SINDEX .BS $1 ;Used by stats routines like roll damage .BS because it needs to persist through drawing routines (no SHAPE.HOPPER0) and a run of map objects manager since mob damage is rolled while that routine is running.

SPELL_CODE.INFERNAL.BLAST		  .EQ $05 ;#CONSTANT
SPELL_CODE.LIGHTNING.BOLT		  .EQ $06 ;#CONSTANT
SPELL_CODE.LIGHTNING.BLAST		  .EQ $07 ;#CONSTANT
SPELL_CODE.LIGHTNING.MEGA_BLAST		.EQ $08 ;#CONSTANT
SPELL_CODE.CHAIN_LIGHTNING.MEDIUM  .EQ $09 ;#CONSTANT
SPELL_CODE.CHAIN_LIGHTNING.LARGE	.EQ $0A ;#CONSTANT
SPELL_CODE.SUMMON.LESSER_UNDEAD		.EQ $0E	;#CONSTANT
SPELL_CODE.SUMMON.DEMON_LORD		.EQ $13	;#CONSTANT
SPELL_CODE.SMITE_SWORD				.EQ $14 ;#CONSTANT
SPELL_CODE.MASS_SMITE_SWORD 		.EQ $15 ;#CONSTANT
SPELL_CODE.SMITE_AXE				.EQ $16 ;#CONSTANT
SPELL_CODE.EDS_AXE					.EQ $17 ;#CONSTANT
SPELL_CODE.SMITE_BOULDER			.EQ $18 ;#CONSTANT
SPELL_CODE.MASS_SMITE_BOULDER		.EQ $19 ;#CONSTANT

;BLOCK2 left variable space: NONE

@END

;COMBAT TURN MANAGEMENT
@START
COMBAT.TURN_STATUS			.EQ SHARED.VARIABLE_SPACE.BLOCK2+$0B ;$1bytes. $00 = PCs turn. $01 = Special(s) S_ENTITY (example Wyvern Ally, Summoned Demon), $02 = MOBs turn
COMBAT.TURN_STATUS.PC		.EQ $00 ;#CONSTANT. Status code for setting combat turn to Player Characters
COMBAT.TURN_STATUS.SPECIAL	.EQ $01 ;#CONSTANT. Status code for setting combat turn to Special(s) S_ENTITIES
COMBAT.TURN_STATUS.MOB		.EQ $02 ;#CONSTANT. Status code for setting combat turn to MOBs

COMBAT.S_ENTITY.STATUS.GOOD			.EQ $00 ;#CONSTANT. 
COMBAT.S_ENTITY.STATUS.POSSESSED	.EQ $01 ;#CONSTANT. 
COMBAT.S_ENTITY.STATUS.SLEEPING		.EQ $02 ;#CONSTANT. 
COMBAT.S_ENTITY.STATUS.DEAD			.EQ $FF ;#CONSTANT. 

COMBAT.OUT_OF_ORDER.SHARED.MEMORY5	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$0C ;$1 bytes
COMBAT.OUT_OF_ORDER.SHARED.MEMORY6	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$0D ;$1 bytes
COMBAT.OUT_OF_ORDER.SHARED.MEMORY7	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$0E ;$1 bytes
COMBAT.OUT_OF_ORDER.SHARED.MEMORY15	.EQ COMBAT.OUT_OF_ORDER.SHARED.MEMORY15_STEP ;$1 bytes 

;BLOCK2 variable space <NONE> available


;PLAYER CHARACTER MOVEMENT
COMBAT.PC.ACTIVE		 	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$10 		;$1bytes. The player character number whose turn it is. Starts with $01
COMBAT.PC.ACTIVE.RECORD		.EQ SHARED.VARIABLE_SPACE.BLOCK2+$11		;$1bytes. The map_object array record # of the player character number whos turn it is. 
COMBAT.ATTACKER.RECORD 		.EQ COMBAT.PC.ACTIVE.RECORD
COMBAT.PC.ACTIVE.SINDEX		.EQ SHARED.VARIABLE_SPACE.BLOCK2+$12		;$1bytes. The screen index of the player character number whos turn it is. 
COMBAT.PC.ACTIVE.HEALTH_STATUS		.EQ SHARED.VARIABLE_SPACE.BLOCK2+$13 ;$1bytes. The value of byte $03 for the active PC's map object record, which contains health status (good/possessed/sleeping/dead etc.) 
;COMBAT.PC.MOVE.ELIGIBLE		.EQ SHARED.VARIABLE_SPACE.BLOCK2+$12 ;$1bytes. The quantity of player characteres in the party who are eligible to move (i.e. alive, concious and not possessed)
COMBAT.PC.ACTIVE.LOCK		.EQ SHARED.VARIABLE_SPACE.BLOCK2+$14 		;$1bytes. $00 = off, otherwise contains the player # that is locked (i.e. the only player character given a turn in combat.)
COMBAT.ACTIVE_PC.BLINK.COUNTER	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$15 	;$1bytes. Controls when the icon blinks, designating the active player character

COMBAT.MAX.PC					 .EQ $06 ;CONSTANT; The maximum # of player characters that can be active in the party

COMBAT.OUT_OF_ORDER.SHARED.MEMORY2	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$16 ;$1 bytes
COMBAT.OUT_OF_ORDER.SHARED.MEMORY3	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$17 ;$1 bytes
COMBAT.OUT_OF_ORDER.SHARED.MEMORY4	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$18 ;$1 bytes

COMBAT.PC.BLOCKED_MOVE.FLAG			.EQ SHARED.VARIABLE_SPACE.BLOCK2+$19 ;$1 bytes  ($00 = not blocked | $01 = blocked)	

;BLOCK2 variable space (NONE) available

;COMBAT SPECIAL S_ENTITY(s) MOVEMENT
;(Wyvern Ally, Summoned Demons, etc.)
COMBAT.SPECIAL.START.TOTAL		.EQ SHARED.VARIABLE_SPACE.BLOCK2+$1A ;$1bytes. The number of MOBs at the start of the battle. 
COMBAT.SPECIAL.ACTIVE			.EQ SHARED.VARIABLE_SPACE.BLOCK2+$1B ;$1bytes.  The MOB number whose turn it is. Starts with $01
COMBAT.OUT_OF_ORDER.SHARED.MEMORY16	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$1C ;$1 byte
COMBAT.SPECIAL.ACTIVE.RECORD	.EQ COMBAT.PC.ACTIVE.RECORD
COMBAT.ATTACKER.TRUE_RECORD		.EQ SHAPE.HOPPER0+$30 ;contains the actual SPECIAL map objec index in the underlying NPC map objects array.

COMBAT.OUT_OF_ORDER.SHARED.MEMORY8	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$1D ;$1 bytes
;COMBAT.OUT_OF_ORDER.SHARED.MEMORY9 (**AVAILABLE**)	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$1E ;$1 bytes
COMBAT.OUT_OF_ORDER.SHARED.MEMORY_A .EQ SHARED.VARIABLE_SPACE.BLOCK2+$1F ;$1 bytes
COMBAT.OUT_OF_ORDER.SHARED.MEMORY_B .EQ SHARED.VARIABLE_SPACE.BLOCK2+$20 ;$1 bytes
COMBAT.OUT_OF_ORDER.SHARED.MEMORY_C .EQ SHARED.VARIABLE_SPACE.BLOCK2+$21 ;$1 bytes 

;BLOCK2 variable space (none) available

;COMBAT MOB MOVEMENT
COMBAT.MOB.START.TOTAL		.BS $1 ;(must be .BS due to conflicts) The number of MOBs at the start of the battle. 
COMBAT.MOB.ACTIVE			.EQ SHARED.VARIABLE_SPACE.BLOCK2+$22 ;$1bytes.  The MOB number whose turn it is. Starts with $01
COMBAT.MOB.ACTIVE.RECORD	.EQ COMBAT.PC.ACTIVE.RECORD

COMBAT.OUT_OF_ORDER.SHARED.MEMORY17	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$24 ;$1 byte
COMBAT.OUT_OF_ORDER.SHARED.MEMORY18	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$25 ;$1 byte
COMBAT.OUT_OF_ORDER.SHARED.MEMORY19	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$26 ;$1 byte
COMBAT.OUT_OF_ORDER.SHARED.MEMORY1A	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$23 ;$1 byte

;BLOCK2 variable space <none> available


;ACQUIRE TARGET (mobs and "specials")
@START

COMBAT.ACQUIRE.RETURN_VALUE.PARM	 .BS $1	;($00 = standard return values | >=$01 = COMBAT.TARGET_HIT.DB(x))


COMBAT.ENEMY_TYPE.MOB		.EQ $01	;S_ENTITY_TYPE.C_MOB ;#CONSTANT. Used to set the type of enemy a given S_ENTITY will pursue & attack 
COMBAT.ENEMY_TYPE.PC		.EQ $03 ;S_ENTITY_TYPE.PC ;#CONSTANT. Used to set the type of enemy a given S_ENTITY will pursue & attack 
;COMBAT.ENEMY_TYPE.ALL_SPECIAL	.EQ $03 S_ENTITY_TYPE.SPECIAL ;#CONSTANT. Used to set the type of enemy a given S_ENTITY will pursue & attack 
; COMBAT.ENEMY_TYPE.MOB_SPECIAL	.EQ $F1 S_ENTITY_TYPE.SPECIAL ;#CONSTANT. Used to set the type of enemy a given S_ENTITY will pursue & attack 
; COMBAT.ENEMY_TYPE.PC_SPECIAL	.EQ $F2 S_ENTITY_TYPE.SPECIAL ;#CONSTANT. Used to set the type of enemy a given S_ENTITY will pursue & attack 
COMBAT.ENEMY_TYPE.ALL		.EQ $FF ;#CONSTANT. Used to set the type of enemy a given S_ENTITY will pursue & attack 

COMBAT.ALIGNMENT.MOB		.EQ $01	;S_ENTITY_TYPE.C_MOB ;#CONSTANT. Used to set the type S_ENTITY that will pursue & attack S_ENTITIES with this alignment 
COMBAT.ALIGNMENT.PC			.EQ $03 ;S_ENTITY_TYPE.PC	 ;#CONSTANT. Used to set the type S_ENTITY that will pursue & attack S_ENTITIES with this alignment

COMBAT.ACQUIRE.TARGET.SCREEN_ARRAY.INDEX .EQ SHARED.VARIABLE_SPACE.BLOCK2+$27 ;$1bytes. Used for searching the S_ENTITY screen array for possible targets	
COMBAT.MAP_OBJECT.ARRAY.POINTER	.EQ $E8 ;#POINTER. 2byts. Used for dynamically accessing the various MAP_OBJECT arrays	
COMBAT.TARGET.RECORD			.EQ SHARED.VARIABLE_SPACE.BLOCK2+$28 ;$0C bytes. Stores the map object array data for a Found S_ENTITY being examined as a possible target
COMBAT.TARGET.RECORD.SIZE		.EQ $0C ;#CONSTANT. # of bytes in COMBAT.TARGET.RECORD	
COMBAT.TARGET.S_ENTITY_TYPE		.EQ SHARED.VARIABLE_SPACE.BLOCK2+$34 ;$01 bytes.
COMBAT.CHASE.TARGET.X		.EQ SHARED.VARIABLE_SPACE.BLOCK2+$35 ;$01 bytes. Each time a prospective target is found, this variables is updated if the prospective target is more desireable that the last one stored in this variables. 	
COMBAT.CHASE.TARGET.Y		.EQ SHARED.VARIABLE_SPACE.BLOCK2+$36 ;$01 bytes. Each time a prospective target is found, this variables is updated if the prospective target is more desireable that the last one stored in this variables. 	

COMBAT.CHASE.TARGET.DISTANCE		.EQ SHARED.VARIABLE_SPACE.BLOCK2+$37 ;$01 bytes. Each time a prospective target is found, this variables is updated if the prospective target is more desireable that the last one stored in this variables. 	

COMBAT.CHASE.TARGET.HEALTH			.EQ SHARED.VARIABLE_SPACE.BLOCK2+$38 ;$01 bytes. Each time a prospective target is found, this variables is updated if the prospective target is more desireable that the last one stored in this variables. 	
COMBAT.CHASE.PROSPECTIVE_TARGET.DISTANCE	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$39 ;$01 bytes. Each time a prospective target is found, the prospective target's distance is saved in this variable
COMBAT.CHASE.VALID_TARGET.CLOSER		.EQ SHARED.VARIABLE_SPACE.BLOCK2+$3A ;$01 bytes. ($00 = prospective target is not closer to the attacker | >=$01 prospective target is closer to attacker, considing all attacker tiles if MTT)


;---these variables must stay in this sequence----
COMBAT.CHASE.TARGET.X_GR					.EQ SHARED.VARIABLE_SPACE.BLOCK2+$3B ;$01 bytes. set to MOB.POSITION.X_GR set by distance calculation for the closest target
COMBAT.CHASE.TARGET.X_LT					.EQ SHARED.VARIABLE_SPACE.BLOCK2+$3C ;$01 bytes. set to MOB.POSITION.X_LT set by distance calculation for the closest target
COMBAT.CHASE.TARGET.Y_GR					.EQ SHARED.VARIABLE_SPACE.BLOCK2+$3D ;$01 bytes. set to MOB.POSITION.Y_GR set by distance calculation for the closest target
COMBAT.CHASE.TARGET.Y_LT					.EQ SHARED.VARIABLE_SPACE.BLOCK2+$3E ;$01 bytes. set to MOB.POSITION.Y_LT set by distance calculation for the closest target
;------------


; COMBAT.CHASE.TARGET.X_LT					.EQ MOB.POSITION.X_LT_STEP ;$01 bytes. set to MOB.POSITION.X_LT set by distance calculation for the closest target
; COMBAT.CHASE.TARGET.X_GR					.EQ MOB.POSITION.X_GR_STEP ;$01 bytes. set to MOB.POSITION.X_GR set by distance calculation for the closest target
; COMBAT.CHASE.TARGET.Y_LT					.EQ MOB.POSITION.Y_LT_STEP ;$01 bytes. set to MOB.POSITION.Y_LT set by distance calculation for the closest target
; COMBAT.CHASE.TARGET.Y_GR					.EQ MOB.POSITION.Y_GR_STEP ;$01 bytes. set to MOB.POSITION.Y_GR set by distance calculation for the closest target


COMBAT.CHASE.FURTHEST_TARGET.DISTANCE		.EQ	SHAPE.HOPPER0+$31 ;$1byte. only used very briefly, between .MOB.ACQUIRE.TARGET and .MOB.DETERMINE.MELEE_RANGE  
COMBAT.FURTHEST_TARGET.X					.EQ	SHAPE.HOPPER0+$32 ;$1byte. only used very briefly, between .MOB.ACQUIRE.TARGET and .MOB.DETERMINE.MELEE_RANGE
COMBAT.FURTHEST_TARGET.Y					.EQ	SHAPE.HOPPER0+$33 ;$1byte. only used very briefly, between .MOB.ACQUIRE.TARGET and .MOB.DETERMINE.MELEE_RANGE
COMBAT.ACQUIRE.FURTHEST_TARGET.FINAL.SINDEX .EQ	SHAPE.HOPPER0+$34 ;$1byte. only used very briefly, between .MOB.ACQUIRE.TARGET and .MOB.DETERMINE.MELEE_RANGE
COMBAT.ACQUIRE.LOOP.INDEX					.EQ SHAPE.HOPPER0+$35 ;$1byte. screen index for each iteration of the loop
;COMBAT.ACQUIRE.FURTHEST_TARGET.FLAG 		.EQ SHAPE.HOPPER0+$	  ;$1byte. ($00 = furthest target branch not used this iteration | >$01 furthest target branch used on this iteration)
; COMBAT.ACQUIRE.VALID_TARGET.DISTANCE		.EQ SHAPE.HOPPER0+$ ;$1byte/
; COMBAT.ACQUIRE.ATTACKER.SINDEX				.EQ SHAPE.HOPPER0+$ ;$1byte/

;---these variables must stay in this sequence----
COMBAT.CHASE.VALID_TARGET.X_GR		.EQ SHAPE.HOPPER0+$36 ;$01 bytes. set to MOB.POSITION.X_GR set by distance calculation for the closest target
COMBAT.CHASE.VALID_TARGET.X_LT		.EQ SHAPE.HOPPER0+$37 ;$01 bytes. set to MOB.POSITION.X_LT set by distance calculation for the closest target
COMBAT.CHASE.VALID_TARGET.Y_GR		.EQ SHAPE.HOPPER0+$38 ;$01 bytes. set to MOB.POSITION.Y_GR set by distance calculation for the closest target
COMBAT.CHASE.VALID_TARGET.Y_LT		.EQ SHAPE.HOPPER0+$39 ;$01 bytes. set to MOB.POSITION.Y_LT set by distance calculation for the closest target
;------------

COMBAT.OUT_OF_ORDER.SHARED.MEMORY_D	.EQ COMBAT.OUT_OF_ORDER.SHARED.MEMORY_D_STEP
COMBAT.OUT_OF_ORDER.SHARED.MEMORY_E	.EQ COMBAT.OUT_OF_ORDER.SHARED.MEMORY_E_STEP
COMBAT.OUT_OF_ORDER.SHARED.MEMORY_F	.EQ COMBAT.OUT_OF_ORDER.SHARED.MEMORY_F_STEP
COMBAT.OUT_OF_ORDER.SHARED.MEMORY10	.EQ COMBAT.OUT_OF_ORDER.SHARED.MEMORY10_STEP
COMBAT.OUT_OF_ORDER.SHARED.MEMORY11	.EQ COMBAT.OUT_OF_ORDER.SHARED.MEMORY11_STEP



@END

;BLOCK2 variable space $44-$45 available

@END

;SHAPE MOVEMENT (FIRING PROJECTILES)
@START
COMBAT.PROJECTILE.DELAY	 			.EQ $10 ;#CONSTANT Delay between shape draws during shape movement (time target = 1.6 seconds when there are 7 empty tiles between source and target)			
COMBAT.PROJECTILE.DELAY.LIGHTNING	.EQ $10 ;#CONSTANT Delay between shape draws during shape movement (time target = 1.6 seconds when there are 7 empty tiles between source and target)			

;COMBAT.FIRE.PROJECTILE
@START
; FP.SOURCE.GMAP.X .BS $1 ;			.EQ SHARED.VARIABLE_SPACE.BLOCK2+$43 ;$01 bytes. The source position of the projectile being fired		
; FP.SOURCE.GMAP.Y .BS $1 ;			.EQ SHARED.VARIABLE_SPACE.BLOCK2+$44 ;$01 bytes. The source position of the projectile being fired		
; FP.TARGET.GMAP.X .BS $1 ;			.EQ SHARED.VARIABLE_SPACE.BLOCK2+$45 ;$01 bytes. The source position of the projectile being fired		
; FP.TARGET.GMAP.Y .BS $1 ;			.EQ SHARED.VARIABLE_SPACE.BLOCK2+$46 ;$01 bytes. The source position of the projectile being fired		

; FP.SOURCE.GMAP.X			.EQ SHARED.VARIABLE_SPACE.BLOCK2+$43 ;$01 bytes. The source position of the projectile being fired		
; FP.SOURCE.GMAP.Y			.EQ SHARED.VARIABLE_SPACE.BLOCK2+$44 ;$01 bytes. The source position of the projectile being fired		
; FP.TARGET.GMAP.X			.EQ SHARED.VARIABLE_SPACE.BLOCK2+$45 ;$01 bytes. The source position of the projectile being fired		
; FP.TARGET.GMAP.Y			.EQ SHARED.VARIABLE_SPACE.BLOCK2+$46 ;$01 bytes. The source position of the projectile being fired		

FP.SOURCE.GMAP.X			.EQ COMBAT.SPELL_CASTER.GMAP.X ;$01 bytes. The source position of the projectile being fired		
FP.SOURCE.GMAP.Y			.EQ COMBAT.SPELL_CASTER.GMAP.Y ;$01 bytes. The source position of the projectile being fired		
FP.TARGET.GMAP.X			.EQ COMBAT.PLAYER_SELECTED.TARGET.GMAP.X ;$01 bytes. The source position of the projectile being fired		
FP.TARGET.GMAP.Y			.EQ COMBAT.PLAYER_SELECTED.TARGET.GMAP.Y ;$01 bytes. The source position of the projectile being fired		

SHAPE.SINDEX.TARGET			.EQ SHARED.VARIABLE_SPACE.BLOCK2+$47 ;$01 bytes. Contains the screen array index of the target S_ENTITY
SHAPE.SBYTE.START			.EQ SHARED.VARIABLE_SPACE.BLOCK2+$48 ;$01 bytes.
SHAPE.SBYTE.TARGET			.EQ SHARED.VARIABLE_SPACE.BLOCK2+$49 ;$01 bytes.
SHAPE.SBYTE.TARGET.ORIGINAL .EQ SHARED.VARIABLE_SPACE.BLOCK2+$46 ;(out of order, renumber when convenient) $01 bytes.
SHAPE.LINE.TARGET 			.EQ SHARED.VARIABLE_SPACE.BLOCK2+$4A ;$01 bytes.
SHAPE.LINE.START			.EQ SHARED.VARIABLE_SPACE.BLOCK2+$4B ;$01 bytes.
SHAPE.PROJECTILE.RISE 		.EQ SHARED.VARIABLE_SPACE.BLOCK2+$4C ;$01 bytes.
SHAPE.PROJECTILE.RUN.PIXEL 	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$4D ;$01 bytes.
SHAPE.PROJECTILE.RUN.SBYTE  .EQ SHARED.VARIABLE_SPACE.BLOCK2+$4E ;$01 bytes. The run of the geometric line between S_ENTITIES as measured in screen bytes. 
SHAPE.PROJECTILE.SLOPE_RISE .EQ SHARED.VARIABLE_SPACE.BLOCK2+$4F ;$01 bytes.
SHAPE.PROJECTILE.SLOPE_RISE.REMAINDER .EQ SHARED.VARIABLE_SPACE.BLOCK2+$50 ;$01 bytes.
SHAPE.PROJECTILE.SLOPE_RISE.SBYTE .EQ SHARED.VARIABLE_SPACE.BLOCK2+$51 ;$01 bytes.
SHAPE.PROJECTILE.SLOPE_RUN	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$52 ;$01 bytes.
RETURN.SHAPE.RUN 			.EQ MAP_OBJECTS.X_ADJ_STEP ;this is an alias, not a memory share.
RETURN.SHAPE.RISE			.EQ MAP_OBJECTS.Y_ADJ_STEP ;this is an alias, not a memory share.
SHAPE.PROJECTILE.HORIZONAL_INC.MODE		 .EQ SHARED.VARIABLE_SPACE.BLOCK2+$53 ;$01 bytes. $00 = screen byte, >=$01 = pixels (bit shifting)


SHAPE.PROJECTILE.DRAW_THREASHOLD .EQ $10
SHAPE.MOVE.DRAW_THREASHOLD.COUNTER .EQ SHARED.VARIABLE_SPACE.BLOCK2+$54 ;$01 bytes. Tracks when to do shape draw if the rise is large and the run is small, 
SHAPE.MOVE.RECURSIVE.CALL 		.EQ SHARED.VARIABLE_SPACE.BLOCK2+$55 ;$01 bytes.

SHAPE.MOVE.EXTRA_RISE.SLOPE_THREASHOLD 		.EQ $0A ;#CONSTANT. if SHAPE.PROJECTILE.SLOPE_RISE is greater than this constant, then an extra rise increment is done every few run increments
SHAPE.MOVE.EXTRA_RISE.COUNTER_THREASHOLD 	.EQ $02 ;#CONSTANT. The constant value +1 (because zero counts) is the interval of run increments refered to in the above constant. 
SHAPE.MOVE.EXTRA_RISE.COUNTER 	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$56 ;$01 bytes.												;to avoid the shape movement from being choppy. 

;ANGULAR SHAPE OFFSETS
@START
;definition: angular shapes are designed where the direction it points in matters (i.e. an arrow, lightning bolt etc. )
SHAPE_TABLE.ANGULAR.SHAPE_PAIR.SIZE		.EQ $40 ;the size of the AND mask and ORA mask shape tables for a single version of an angular shape
SHAPE_TABLE.ANGULAR.UPPER_OCTET.SIZE	.EQ SHAPE_TABLE.ANGULAR.SHAPE_PAIR.SIZE*3 ;the size of each of the octets that contain angled lines (non-verticle/horitzontal). Octets 4-7

SHAPE_TABLE.ANGULAR.OCTET1.OFFSET		.EQ SHAPE_TABLE.ANGULAR.SHAPE_PAIR.SIZE*1
SHAPE_TABLE.ANGULAR.OCTET2.OFFSET		.EQ SHAPE_TABLE.ANGULAR.SHAPE_PAIR.SIZE*2
SHAPE_TABLE.ANGULAR.OCTET3.OFFSET		.EQ SHAPE_TABLE.ANGULAR.SHAPE_PAIR.SIZE*3

SHAPE_TABLE.ANGULAR0.OCTET4.OFFSET		.EQ SHAPE_TABLE.ANGULAR.SHAPE_PAIR.SIZE*4 ;used to calculate the memory address where the angled versions (octet 4-7) of angular shapes starts. 
SHAPE_TABLE.ANGULAR1.OCTET4.OFFSET		.EQ SHAPE_TABLE.ANGULAR0.OCTET4.OFFSET+SHAPE_TABLE.ANGULAR.SHAPE_PAIR.SIZE ;used to calculate the memory address where the angled versions (octet 4-7) of angular shapes starts. 
SHAPE_TABLE.ANGULAR2.OCTET4.OFFSET		.EQ SHAPE_TABLE.ANGULAR1.OCTET4.OFFSET+SHAPE_TABLE.ANGULAR.SHAPE_PAIR.SIZE ;used to calculate the memory address where the angled versions (octet 4-7) of angular shapes starts. 

SHAPE_TABLE.ANGULAR0.OCTET5.OFFSET		.EQ SHAPE_TABLE.ANGULAR0.OCTET4.OFFSET+SHAPE_TABLE.ANGULAR.UPPER_OCTET.SIZE  ;used to calculate the memory address where the angled versions (octet 4-7) of angular shapes starts. 
SHAPE_TABLE.ANGULAR1.OCTET5.OFFSET		.EQ SHAPE_TABLE.ANGULAR0.OCTET5.OFFSET+SHAPE_TABLE.ANGULAR.SHAPE_PAIR.SIZE ;used to calculate the memory address where the angled versions (octet 4-7) of angular shapes starts. 
SHAPE_TABLE.ANGULAR2.OCTET5.OFFSET		.EQ SHAPE_TABLE.ANGULAR1.OCTET5.OFFSET+SHAPE_TABLE.ANGULAR.SHAPE_PAIR.SIZE ;used to calculate the memory address where the angled versions (octet 4-7) of angular shapes starts. 

SHAPE_TABLE.ANGULAR0.OCTET6.OFFSET		.EQ SHAPE_TABLE.ANGULAR0.OCTET5.OFFSET+SHAPE_TABLE.ANGULAR.UPPER_OCTET.SIZE ;used to calculate the memory address where the angled versions (octet 4-7) of angular shapes starts. 
SHAPE_TABLE.ANGULAR1.OCTET6.OFFSET		.EQ SHAPE_TABLE.ANGULAR0.OCTET6.OFFSET+SHAPE_TABLE.ANGULAR.SHAPE_PAIR.SIZE ;used to calculate the memory address where the angled versions (octet 4-7) of angular shapes starts. 
SHAPE_TABLE.ANGULAR2.OCTET6.OFFSET		.EQ SHAPE_TABLE.ANGULAR1.OCTET6.OFFSET+SHAPE_TABLE.ANGULAR.SHAPE_PAIR.SIZE ;used to calculate the memory address where the angled versions (octet 4-7) of angular shapes starts. 

SHAPE_TABLE.ANGULAR0.OCTET7.OFFSET		.EQ SHAPE_TABLE.ANGULAR0.OCTET6.OFFSET+SHAPE_TABLE.ANGULAR.UPPER_OCTET.SIZE ;used to calculate the memory address where the angled versions (octet 4-7) of angular shapes starts. 
SHAPE_TABLE.ANGULAR1.OCTET7.OFFSET		.EQ SHAPE_TABLE.ANGULAR0.OCTET7.OFFSET+SHAPE_TABLE.ANGULAR.SHAPE_PAIR.SIZE ;used to calculate the memory address where the angled versions (octet 4-7) of angular shapes starts. 
SHAPE_TABLE.ANGULAR2.OCTET7.OFFSET		.EQ SHAPE_TABLE.ANGULAR1.OCTET7.OFFSET+SHAPE_TABLE.ANGULAR.SHAPE_PAIR.SIZE ;used to calculate the memory address where the angled versions (octet 4-7) of angular shapes starts. 
@END


COMBAT.SHAPE_TABLE.POINTER	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$57 ;$02 bytes. Points to the shape table for the type of 
							;shape to be used. For angular shapes, Doesn't point to a specific shape version, 
							;that must be calculated.
COMBAT.SHAPE_TABLE.START_ADDRESS .EQ SHARED.VARIABLE_SPACE.BLOCK2+$59 ;$02 bytes. The calculated start address of a 
							;specific shape version. 						
COMBAT.SHAPE_TABLE.OFFSET.PARTIAL .EQ SHARED.VARIABLE_SPACE.BLOCK2+$5B ;$02 bytes.

SHAPE.PROJECTILE.ANGLE.TYPE	 .EQ SHARED.VARIABLE_SPACE.BLOCK2+$5D ;$01 bytes. Stores the type of angle of the current geometric 
							;line between the start/target as determined by comparing the slope to the angle type 
							;constants below							

;SHAPE.PROJECTILE.ANGLE.TYPE0		 ;slope >= $0E
SHAPE.PROJECTILE.ANGLE.TYPE1 .EQ $0E ;slope > $03 and < $0E
SHAPE.PROJECTILE.ANGLE.TYPE2 .EQ $04 ;slope < $04
SHAPE.PROJECTILE.STEEP_SLOPE .EQ $18 ;slope >= $28. For steep slopes, use +/- 2 bit/pixel horizontal increment instead of 1 screen byte. 
;SHAPE.PROJECTILE.STEEP_SLOPE .EQ $28 ;slope >= $28. For steep slopes, use +/- 2 bit/pixel horizontal increment instead of 1 screen byte. 


SHAPE.MOVE.INTRA_SBYTE.COUNTER 	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$5E ;$1 byte. tracks the number of bits a shape has been shifted horizontally. Used to trigger a reset of the shape tables by copying the master shape table into the hopper again. 
SHAPE.MOVE.BIT_SHIFT.QTY		.EQ SAVED.ACC.LOCAL
SHAPE_TABLE.RESET.FLAG			.EQ SHARED.VARIABLE_SPACE.BLOCK2+$5F ;$1 byte. $01 = set, $00 = not set. If set then in SHAPE.MOVE, the shape tables are refreshed from the master after the screen draw is complete. 
BIT_SHIFT_SKIP.FLAG				.EQ SHARED.VARIABLE_SPACE.BLOCK2+$60 ;$1 byte. $01 = set, $00 = not set. If set then in SHAPE.MOVE, then a screen byte increment is performed instead of a bit shift when incrementing run
SHAPE.PROJECTILE.ANIMATION_FRAME	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$61 ;$1 byte.
SHAPE_TABLE.SIZE				.EQ SHARED.VARIABLE_SPACE.BLOCK2+$62 ;$1 bytes.
	;(no HO byte is used because .COPY.SHAPE_TABLE only copies shape tables $100 bytes or less. $100 byte is $00 - $FF)
	;there is no HO byte, used with copies less than $100 bytes in size
	
;BLOCK2 left variable space $63-$65 available
@END

;COMBAT.SHAPE.MOVE
@START
SHAPE.POSITION.X_GR 	.EQ MOB.POSITION.X_GR_STEP
SHAPE.POSITION.X_LT 	.EQ MOB.POSITION.X_LT_STEP
SHAPE.POSITION.Y_GR 	.EQ MOB.POSITION.Y_GR_STEP
SHAPE.POSITION.Y_LT 	.EQ MOB.POSITION.Y_LT_STEP

SHAPE.MOVE.SCREEN_ANIMATION.COUNTER	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$66 ;$01 bytes. Run ANIMATION.UPDATE (entire screen) every few projectile shape draws.
SHAPE.MOVE.SCREEN_ANIMATION.THREASHOLD .EQ $03
SHAPE.MOVE.MODE			.EQ SHARED.VARIABLE_SPACE.BLOCK2+$67 ;$01 bytes. ($00 = angled static shape | >=$01 = non-angled, animated shape)

SHAPE.MOVE.DELAY_LENGTH			.EQ SHARED.VARIABLE_SPACE.BLOCK2+$68 ;$01 bytes. The length of the delay between the 
																	 ;shape draw and shape erase. The value is not a 
																	 ;meausre of a specific unit of time, it's a 
																	 ;counter stop value for the loop 
																	 ;in COMBAT.WAIT.LOOP
																	 
SHAPE.MOVE.SCREEN_ANIMATION.TOGGLE		.EQ SHARED.VARIABLE_SPACE.BLOCK2+$69 ;$01 bytes. ($00 = screen animation on | >=$01 = screen animation off)
SHAPE.MOVE.ERASE.TOGGLE			.EQ SHARED.VARIABLE_SPACE.BLOCK2+$6A ;$01 bytes. ($00 = shape erase on | >=$01 = shape erase off). 
SHAPE.MOVE.ITERATION_COUNTER	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$6B ;$01 bytes. Used to force the 1st iteration of the draw loop to be skipped, preventing the projectile from totally covering the source tile. 														;Refers to the incremental erase between shape draws. 

																
;BLOCK2 left variable space $6C-$6C available

;BLOCK4 left variable space $46-$58 (double check range, see SHAPE.EXISTING_DATA.BUFFER below)

@END

;COMBAT.DRAW.SHAPE
@START
SHAPE.EXISTING_DATA.BUFFER	.EQ SHARED.VARIABLE_SPACE.BLOCK4+$00 ;(out of order, **block4**) $40 bytes.
COMBAT.OUT_OF_ORDER.SHARED.MEMORY12 .EQ SHARED.VARIABLE_SPACE.BLOCK4+$40 ;(out of order, **block4**) $2 bytes.
COMBAT.OUT_OF_ORDER.SHARED.MEMORY13 .EQ SHARED.VARIABLE_SPACE.BLOCK4+$42 ;(out of order, **block4**) $2 bytes.
COMBAT.OUT_OF_ORDER.SHARED.MEMORY14 .EQ SHARED.VARIABLE_SPACE.BLOCK4+$44 ;(out of order, **block4**) $2 bytes.
							;**$45 in use**					;**$45 in use**
												
SHAPE.BACKGROUND.AND_MASK 	.EQ $D2	;pointer	
SHAPE.COMPOSITE.ORA_MASK 	.EQ $D4  ;pointer
DRAW.SHAPE.MODE 			.EQ SHARED.VARIABLE_SPACE.BLOCK2+$6D ;(back in order) $01bytes. mode parameter for DRAW.SHAPE. $00 = draw mode | $01 = erase mode
SHAPE.DRAW.LINE_ROW.NUMBER	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$6E ;$01bytes. The screen byte that SHAPE.DRAW will use to start plotting the specified shape table. NOTE: refers to the sequential line/row number as visually seen on the hi-res screen, not the line memory address. 
SHAPE.DRAW.CURRENT.SBYTE 	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$6F ;$01bytes. 
;SHAPE.DRAW.USE_PAGE			.EQ PAGE.FOREGROUND.OVERRIDE ;($00 = both, $01 = foreground, $02 = background)

SHAPE.DRAW.RELATED_STEP		.EQ SHARED.VARIABLE_SPACE.BLOCK2+$70 ;not used directly. used as a .EQ by CHR_SHEET.RECORD.READ
SHAPE.DRAW.START.SBYTE		.EQ SHARED.VARIABLE_SPACE.BLOCK2+$70 ;$01bytes
SHAPE.DRAW.STOP.SBYTE		.BS $1

SHAPE.DRAW.HOPPER		 	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$80 ;(out of order) $80bytes
SHAPE.DRAW.HOPPER.AND_MASK  .EQ SHAPE.DRAW.HOPPER
SHAPE.DRAW.HOPPER.ORA_MASK  .EQ SHAPE.DRAW.HOPPER+$40
 													
SHAPE.DRAW.BYTE.TRANSFER_VALUE .EQ SHARED.VARIABLE_SPACE.BLOCK2+$71 ;(back in order) $01bytes
SHAPE.DRAW.SCREEN_BYTE.COUNTER .EQ SHARED.VARIABLE_SPACE.BLOCK2+$72 ;$1 bytes	

;**WARNING: SHARED.VARIABLE_SPACE.BLOCK2+$70-$EF is used by CHR_SHEET.RECORD.READ 

COMBAT.OUT_OF_ORDER.SHARED.MEMORY0	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$73 ;$1 bytes
COMBAT.OUT_OF_ORDER.SHARED.MEMORY1	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$74 ;$1 bytes

;BLOCK2 left variable space $75-$7F available
;(note: $80-$FF used by SHAPE.DRAW.HOPPER)
@END
@END
;***MEMORY: end shared block2, start shared block1

;BLOCK1 left variable space $D0, $D5-$D6 available (see below, consider renumbering)

;SPECIAL EFFECTS
@START
COMBAT.EXPLOSION.START
COMBAT.EXPLOSION.CENTER 		.EQ SHARED.VARIABLE_SPACE.BLOCK1+$CD ;$01 bytes
COMBAT.EXPLOSION.RADIUS 		.EQ SHARED.VARIABLE_SPACE.BLOCK1+$CE ;$01 bytes
COMBAT.EXPLOSION.RADIUS.COUNTER .EQ SHARED.VARIABLE_SPACE.BLOCK1+$CF ;$01 bytes. 
;COMBAT.EXPLOSION.RADIUS.COUNTER_X2 	.EQ SHARED.VARIABLE_SPACE.BLOCK1+$D0 ;$01 bytes. COMBAT.EXPLOSION.RADIUS * 2
COMBAT.EXPLOSION.RUN.COUNTER 	.EQ SHARED.VARIABLE_SPACE.BLOCK1+$D1 ;$01 bytes. 
COMBAT.EXPLOSION.TILE_TYPE .EQ SHARED.VARIABLE_SPACE.BLOCK1+$D2 ;$01 bytes. The tile_type that will be used to draw the explosion
COMBAT.EXPLOSION.PROB		.EQ SHARED.VARIABLE_SPACE.BLOCK1+$D3 ;$01 bytes. The probability that an explosion tile will be drawn at a given location. This is the start value for the first explosion "ring". The center of the explosion is always 100%. 
COMBAT.EXPLOSION.PROB.DEC	.EQ SHARED.VARIABLE_SPACE.BLOCK1+$D4 ;$01 bytes. The amount by which probability is decreased between "rings" of the explosion

COMBAT.SCREEN.LAST_COLUMN 	.EQ $0A ;#CONSTANT. The tile column # of the right edge of the combat screen.
COMBAT.SCREEN.LAST_ROW 		.EQ $0A ;#CONSTANT. The tile row # of the bottom edge of the combat screen.
COMBAT.SCREEN.FIRST_COLUMN 	.EQ $00 ;#CONSTANT. The tile column # of the right edge of the combat screen.
COMBAT.SCREEN.FIRST_ROW 	.EQ $00 ;#CONSTANT. The tile row # of the bottom edge of the combat screen.

;COMBAT.EXPLOSION.SCREEN_EDGE.COLLISION	.EQ SHARED.VARIABLE_SPACE.BLOCK1+$D5 ;$01 bytes. $00 = not set $01 = set, screen edge collision occured
;COMBAT.EXPLOSION.SCREEN_EDGE.RIGHT		.EQ SHARED.VARIABLE_SPACE.BLOCK1+$D6 ;$01 bytes. $00 = not set $01 = set, screen edge collision occured
COMBAT.EXPLOSION.START.COLUMN			.EQ SHARED.VARIABLE_SPACE.BLOCK1+$D7 ;$01 bytes. tile column # of target 
COMBAT.EXPLOSION.START.ROW				.EQ SHARED.VARIABLE_SPACE.BLOCK1+$D8 ;$01 bytes. tile column # of target 
COMBAT.EXPLOSION.RUN_LEFT.STOP_VALUE		.EQ SHARED.VARIABLE_SPACE.BLOCK1+$D9 ;$01 bytes. 
COMBAT.EXPLOSION.RUN_RIGHT.STOP_VALUE	.EQ SHARED.VARIABLE_SPACE.BLOCK1+$DA ;$01 bytes. 
COMBAT.EXPLOSION.RUN_UP.STOP_VALUE		.EQ SHARED.VARIABLE_SPACE.BLOCK1+$DB ;$01 bytes. 
COMBAT.EXPLOSION.RUN_DOWN.STOP_VALUE		.EQ SHARED.VARIABLE_SPACE.BLOCK1+$DC ;$01 bytes. 

;CHAIN LIGHTNING
COMBAT.S_ENTITY.FOUND.GMAP.X		.EQ COMBAT.OUT_OF_ORDER.SHARED.MEMORY0	;$1 byte.
COMBAT.S_ENTITY.FOUND.GMAP.Y		.EQ COMBAT.OUT_OF_ORDER.SHARED.MEMORY1	;$1 byte.
CL.TARGET_LAST.GMAP.X 	.EQ SHARED.VARIABLE_SPACE.BLOCK1+$DD	;$1 byte. 
CL.TARGET_LAST.GMAP.Y	.EQ SHARED.VARIABLE_SPACE.BLOCK1+$DE	;$1 byte. 
	
COMBAT.S_ENTITY.DISTANCE.LENGTH		.EQ SHAPE.HOPPER0+$40 ;$2 bytes
COMBAT.S_ENTITY.DISTANCE.DB			.EQ SHAPE.HOPPER0+$42 ;$80 bytes
COMBAT.S_ENTITY.DISTANCE.STOP_VALUE	.EQ $FF ;#CONSTANT
;(see general section above) COMBAT.TARGET_HIT.DB				.EQ SHAPE.HOPPER0+$C2 ;(out of order) $20 bytes. Used by each spell routine with capability to hit multiple targets to store the screen tile location of each target hit. 


;MASS DEATH
;(doesn't use COMBAT.SHAPE.DRAW so the associated memory space can be shared)
MD.CAST.DIRECTION				.EQ SHAPE.EXISTING_DATA.BUFFER+$0 ;$1 byte

;north/south
SHAPE.SBYTE.END					.EQ SHAPE.EXISTING_DATA.BUFFER+$1 ;$1 byte
SHAPE.BIT.SCREEN_LEFT.CURRENT	.EQ SHAPE.EXISTING_DATA.BUFFER+$2 ;$1 byte
SHAPE.BIT.SCREEN_RIGHT.CURRENT	.EQ SHAPE.EXISTING_DATA.BUFFER+$3 ;$1 byte
SHAPE.ORA.BIT_VALUE				.EQ SHAPE.EXISTING_DATA.BUFFER+$4 ;$1 byte
SHAPE.VALUE.SCREEN_LEFT			.EQ SHAPE.EXISTING_DATA.BUFFER+$5 ;$1 byte
SHAPE.VALUE.SCREEN_RIGHT		.EQ SHAPE.EXISTING_DATA.BUFFER+$6 ;$1 byte
SHAPE.NON_EDGE.BYTE.VALUE		.EQ SHAPE.EXISTING_DATA.BUFFER+$7 ;$1 byte
SHAPE.LINE.LOWER_EDGE.START		.EQ SHAPE.EXISTING_DATA.BUFFER+$8 ;$1 byte

;east/west
MD.SPELL.AND_MASK.TABLE 	.EQ $D4
MD.SPELL.ORA_MASK.TABLE		.EQ $D2	

;SHAPE.LINE.START 			.EQ SHAPE.DRAW.LINE_ROW.NUMBER 
SHAPE.LINE.END				.EQ SHAPE.EXISTING_DATA.BUFFER+$9 ;$1 byte
SHAPE.SBYTE.CURRENT			.EQ SHAPE.SBYTE.START
SHAPE.BIT.CURRENT 			.EQ SHAPE.BIT.SCREEN_LEFT.CURRENT
SHAPE.VALUE.TOP_LINE 		.EQ SHAPE.VALUE.SCREEN_LEFT
SHAPE.VALUE.BOTTOM_LINE 	.EQ SHAPE.VALUE.SCREEN_RIGHT
MD.SCREEN_EDGE.TOP.FLAG		.EQ SHAPE.EXISTING_DATA.BUFFER+$A ;$1 byte
MD.EAST_WEST.ORA.FINAL_VALUE .EQ SHAPE.EXISTING_DATA.BUFFER+$B ;$1 byte
MD.FLICKER.WAIT_VALUE 		.EQ SHAPE.EXISTING_DATA.BUFFER+$C ;$1 byte
IS.SPELL.MASS_DEATH.FLAG	.EQ COMBAT.OUT_OF_ORDER.SHARED.MEMORY16	;($00 = no, >$01 = yes)

;SUMMON
;***SEE ;SPELL.SUMMON.LOCAL.VARIABLES

;ARMAGEDON BY THE COWS
;ARMGDN.CLOBBERED.MOB.MO_INDEX	.EQ SHAPE.EXISTING_DATA.BUFFER+$11 ;$1 bytes ($FF = disabled | any other value is a map object index)
ARMGDN.CLOBBERED.MOB.MO_INDEX	.BS $1, $FF ;(.BS for init purpose). ($FF = disabled | any other value is a map object index)
ARMGDN.COW.QTY					.EQ COMBAT.EXPLOSION.CENTER ;$1 bytes

;SMITE
SPELL.SMITE.MULTI_TARGET.FLAG	.BS $1 ;($00 = single target | $01 = multi-target)
; SPELL.SMITE.CODE.GRE			.EQ $14 ;start of spell code range containing SMITE spells
; SPELL.SMITE.CODE.LT				.EQ $18 ;start of spell code range containing SMITE spells
SPELL.SMITE.ACTIVE.FLAG			.EQ COMBAT.OUT_OF_ORDER.SHARED.MEMORY1A ;$1byte. ($00 = SMITE not active | >=$01 = SMITE active)
SMITE.SHAPE_TABLE.POINTER		.EQ $B0 ;$2bytes

@END

;SELECT TARGET (PC)  (COMBAT.SELECT.ATTACK_TARGET)
;(shared memory with various spell routines. There is no conflict because select target is called before the spell routine when player casts a spell)
@START
SELECT_TARGET.ACTIVE.SELECTION.SINDEX	.EQ SHARED.VARIABLE_SPACE.BLOCK1+$DF ;$1byte
SELECT_TARGET.RADIUS					.EQ COMBAT.EXPLOSION.RADIUS			 ;$1byte
;SELECT_TARGET.RADIUS					.BS $1		 ;$1byte

SELECT_TARGET.CLOSEST_TARGET.X			.EQ COMBAT.CHASE.TARGET.X
SELECT_TARGET.CLOSEST_TARGET.Y			.EQ COMBAT.CHASE.TARGET.Y
SELECT_TARGET.CURRENT_TARGET.X			.EQ SELECT_TARGET.CLOSEST_TARGET.X
SELECT_TARGET.CURRENT_TARGET.Y			.EQ SELECT_TARGET.CLOSEST_TARGET.Y	

SELECT_TARGET.SEARCH_POSITION.X			.EQ SHARED.VARIABLE_SPACE.BLOCK1+$E0 ;$1byte
SELECT_TARGET.SEARCH_POSITION.Y			.EQ SHARED.VARIABLE_SPACE.BLOCK1+$E1 ;$1byte

SELECT_TARGET.EXAMINE.S_ENTITY.FOUND	.EQ COMBAT.TARGET.RECORD ;$1byte ($00 = not found, $01 = found)
SELECT_TARGET.PROSPECTIVE.S_ENTITY.DISTANCE .EQ COMBAT.TARGET.S_ENTITY_TYPE ;$1 byte

SELECT_TARGET.PROSPECTIVE.S_ENTITY.X 	.EQ COMBAT.CHASE.TARGET.DISTANCE ;$1 byte
SELECT_TARGET.PROSPECTIVE.S_ENTITY.Y 	.EQ COMBAT.CHASE.TARGET.HEALTH ;$1 byte

SELECT_TARGET.EXAMINE.ITERATION.DIRECTION .EQ COMBAT.EXPLOSION.START.ROW ;$1 byte ;($00 = iterate left, $01 = right)

COMBAT.ABORT.FLAG						.EQ SHARED.VARIABLE_SPACE.BLOCK1+$E2 ;$1byte ($00 = not set, $01 = set)

SELECT_TARGET.LAST_TARGET.DB			.EQ SHARED.VARIABLE_SPACE.BLOCK1+$E3 ;$0C bytes. 
SELECT_TARGET.LAST_TARGET.SIZE			.EQ $0C ;#CONSTANT
SELECT_TARGET.LAST_TARGET.INIT_VALUE	.EQ $FF ;#CONSTANT
SELECT_TARGET.LAST_TARGET.INDEX			.EQ	COMBAT.EXPLOSION.RUN.COUNTER	;1byte
SELECT_TARGET.ITERATION.COUNTER			.EQ	COMBAT.EXPLOSION.TILE_TYPE		;1byte
SELECT_TARGET.LAST_TARGET.FLAG			.EQ	COMBAT.EXPLOSION.PROB			;1byte    ($00 = not set, >= $01 = set)
COMBAT.DRAW.SINGLE.OFF_GRID.ANIMATION_FLAG .EQ COMBAT.EXPLOSION.PROB.DEC ;$1byte ($00 = not animated | $01 = animated)
COMBAT.DRAW.SINGLE.OFF_GRID.WAIT_VALUE	 .EQ COMBAT.EXPLOSION.START.COLUMN ;$1byte. Parameter value for COMBAT.WAIT.LOOP between drawing the shape and screen animation update.
;COMBAT.DRAW.SINGLE.OFF_GRID.WAIT_VALUE	 .BS $1 ;$1byte. Parameter value for COMBAT.WAIT.LOOP between drawing the shape and screen animation update.
SELECT_TARGET.PC_VALID.FLAG				.EQ SHARED.VARIABLE_SPACE.BLOCK1+$EF ;$01 bytes. ;($00 = off, PCs not valid targets | $01 = on, PCs are valid targets)
SELECT_TARGET.LAST_KEY					.EQ SHARED.VARIABLE_SPACE.BLOCK1+$F0 ;$01 bytes.

SELECT_TARGET.MTT.PRESENT 	.BS $1	;($00 = MTT s_entity not present at current search position | $01 = MTT s_entity not present at current search position)

@END

;CHARACTER SHEET RELATED
@START
;also see COMBAT_LOADER.MOB.DATA

;CHR_SHEET.RECORD.READ			.eq $bf00	;$80bytes. Buffer for reading character sheet data into
;CHR_SHEET.RECORD.READ			.EQ SHAPE.HOPPER0+$20		;$80bytes. Buffer for reading character sheet data into

;CHR_SHEET.RECORD.READ			.BS $80		;$80bytes. Buffer for reading character sheet data into
CHR_SHEET.RECORD.READ			.EQ SHAPE.DRAW.RELATED_STEP		;$80bytes. Buffer for reading character sheet data
CHR_SHEET.RECORD.STOP_LENGTH	.EQ SHAPE.HOPPER0+$0		;$1bytes
READ_WRITE.CHR_SHEET.MODE 		.EQ SHAPE.HOPPER0+$1
	;.EQ SHAPE.EXISTING_DATA.BUFFER+$0	;($00 = read | $01 = write)
;READ_WRITE.CHR_SHEET.MODE 	.BS $1 ;for some reason only works as .BS. ;($00 = read | $01 = write)

CHR_SHEET.PC.RECORD_SIZE		.EQ $80		;#CONSTANT
CHR_SHEET.MOB.RECORD_SIZE		.EQ $20		;#CONSTANT
CHR_SHEET.SPECIAL.RECORD_SIZE	.EQ $20		;#CONSTANT
CHR_SHEET.SPECIAL.STOP_VALUE	.EQ $FF		;#CONSTANT. Used to detecte the next open record in the SPECIALs character sheet

CHR_SHEET.PC.AUX_MEMORY.START			.EQ $D900	;#CONSTANT
CHR_SHEET.MOB.AUX_MEMORY.START		.EQ $DC00	;#CONSTANT
CHR_SHEET.SPECIAL.AUX_MEMORY.START	.EQ $DE00	;#CONSTANT
CHR_SHEET.SPECIAL.AUX_MEMORY.END	.EQ	$DFFF	;#CONSTANT
CHR_SHEET.POINTER				.EQ $CA ;#POINTER
CHR_SHEET.S_ENTITY.NUMBER		.EQ SHAPE.HOPPER0+$2


@END

;COMBAT STATS MECHANICS
@START
COMBAT.STATS.ATTACKER_LEVEL			.EQ SHAPE.HOPPER0+$80 ;$1byte
COMBAT.STATS.DEFENDER.RESIST_CRITCAL_HIT	.EQ SHAPE.HOPPER0+$81 ;$1byte
COMBAT.STATS.DEFENDER_LEVEL			.EQ SHAPE.HOPPER0+$82 ;$1byte
COMBAT.STATS.DEFENDER.SIZE_ATTRIB	.EQ SHAPE.HOPPER0+$83 ;$1byte
COMBAT.STATS.DEFENDER.RESIST.MAGIC	.EQ SHAPE.HOPPER0+$84 ;$1byte
COMBAT.STATS.DEFENDER.INT			.EQ SHAPE.HOPPER0+$85 ;$1byte
COMBAT.STATS.DEFENDER.ENGAGED.MO_INDEX	.EQ SHAPE.HOPPER0+$86 ;$1byte
COMBAT.STATS.DEFENDER.READIED_EQUIPMENT_WEIGHT	.EQ SHAPE.HOPPER0+$87 ;$1byte	

COMBAT.STATS.TO_HIT.MIN				.EQ $05 ;#CONSTANT (BCD)
COMBAT.STATS.TO_HIT.MAX				.EQ $95 ;#CONSTANT (BCD)
;COMBAT.STATS.TO_HIT.MAX				.EQ $99 ;#CONSTANT (BCD)

;***SHAPE.HOPPER0 $88-$93 available	

COMBAT.STATS.MODIFIED_TO_HIT		.EQ COMBAT.OUT_OF_ORDER.SHARED.MEMORY5 ;$1byte
COMBAT.STATS.TO_HIT.ROLL			.EQ COMBAT.OUT_OF_ORDER.SHARED.MEMORY6 ;$1byte
COMBAT.STATS.HIT_MISS_KILL.FLAG		.EQ SHARED.VARIABLE_SPACE.BLOCK1+$F1 ;$1byte ;($00 = hit | $01 = miss | $02 = kill)
COMBAT.STATS.TO_HIT.MOD_TALLY		.EQ SHAPE.HOPPER0+$94 ;$2byte
;													 ;$95 in use
;***SHAPE.HOPPER0 $96 available
COMBAT.STATS.ATTACKER.ENAGED.FLAG	.BS $1 ;$1byte ;($00 = attacker is NOT engaged in melee combat, >=$01 = attacker IS engaged in melee combat, the value is the SINDEX of the engaged S_ENTITY)
COMBAT.STATS.RANGE_INTERFERENCE.FLAG	.EQ COMBAT.OUT_OF_ORDER.SHARED.MEMORY7  ;($00 = inteference successful | $01 = interference not attempted | $03 = inteference failed)
COMBAT.STATS.RANGE_INTERFERENCE.PROB	.EQ $80 ;#CONSTANT. If less than this number, range interference occurs

;***SHAPE.HOPPER0 $97-$9B available													  
;
COMBAT.STATS.DEFENDER.DODGE_SKILL	.BS $1 ;$1byte. can't use shape hopper because this value needs to be used in TO-HIT roll and damage roll and there are shape draws in between. 
COMBAT.STATS.DEFENDER.DODGE.PROB	.EQ SHAPE.HOPPER0+$9C ;$1byte. If less than this number, dodge occurs
COMBAT.STATS.DODGE_SKILL.ROLL		.EQ SHAPE.HOPPER0+$9D ;$1byte
COMBAT.STATS.DODGE_FLAG				.EQ SHARED.VARIABLE_SPACE.BLOCK1+$F2 ;$1byte ($00 = dodge succeeded, $01 dodge not attempted, $03 dodge failed)
COMBAT.STATS.DODGE.WEIGHT.LIMIT		.EQ $08			;#CONSTANT. PCs readied equipment weight must be less than this value to use his/her dodge-parry skill

;***SHAPE.HOPPER0 $EE-$A5 available	

COMBAT.STATS.LEVEL_TO_HIT_MODIFIER					.EQ SHAPE.HOPPER0+$A6 ;$1byte.
COMBAT.STATS.LEVEL_TO_HIT_MODIFIER.MAX				.EQ $05 ;#CONSTANT. The max level difference between attacker and defender for which the modifier increases. 
COMBAT.STATS.LEVEL_TO_HIT_MODIFIER.MAX.PLUS_ONE		.EQ $06 ;#CONSTANT. The max level difference between attacker and defender for which the modifier increases. 
;COMBAT.STATS.LEVEL_TO_HIT_MODIFIER.MODIFIER			.EQ SHAPE.HOPPER0+ ;$1byte

COMBAT.STATS.LEVEL_XP_MODIFIER					.EQ COMBAT.STATS.LEVEL_TO_HIT_MODIFIER ;$1byte.
COMBAT.STATS.LEVEL_XP_MODIFIER.MAX				.EQ COMBAT.STATS.LEVEL_TO_HIT_MODIFIER.MAX ;#CONSTANT. The max level difference between attacker and defender for which the modifier increases. 
COMBAT.STATS.LEVEL_XP_MODIFIER.MAX.PLUS_ONE		.EQ COMBAT.STATS.LEVEL_TO_HIT_MODIFIER.MAX.PLUS_ONE ;#CONSTANT. The max level difference between attacker and defender for which the modifier increases. 
COMBAT.STATS.LEVEL_XP_MODIFIER.FLAG 			.EQ COMBAT.STATS.DEFENDER.DODGE_SKILL ;($00 = attacker is higher level, or level is equal | $01 = defender is higher level)
COMBAT.STATS.XP.X.MOD							.EQ COMBAT.STATS.DEFENDER.DODGE.PROB ;$1byte
COMBAT.STATS.DEFENDER.XP						.EQ SHAPE.HOPPER0+$A7 ;$1byte. Unlike most attacker/defender stats variables, this one is not set at the start of the TO-HIT and DAMAGE routines. It is only set in the kill routine					
COMBAT.STATS.XP.AWARD							.EQ SHAPE.HOPPER0+$A8 ;$1byte. Unlike most attacker/defender stats variables, this one is not set at the start of the TO-HIT and DAMAGE routines. It is only set in the kill routine					

COMBAT.STATS.SIZE.LT_SMALL 				.EQ $05 ;#CONSTANT
COMBAT.STATS.SIZE.GRE_LARGE 			.EQ $0A ;#CONSTANT
COMBAT.STATS.SIZE_MOD.LT_DEC.DIVIDEND	.EQ $50 ;#CONSTANT
COMBAT.STATS.SIZE_PERCENT				.EQ SHAPE.HOPPER0+$A9 ;$1byte
COMBAT.STATS.SIZE.MODIFIER				.EQ SHAPE.HOPPER0+$AA ;$2byte
;														;$AB in use
COMBAT.STATS.SIZE.MODIFIER.RETURN_FLAG	.EQ SHAPE.HOPPER0+$AC ;$1byte  ($00 = small mob | $01 larger mob)
;**SHAPE.HOPPER0   $AD-$AF available
COMBAT.STATS.CALC.RESIST_SPELL_MAGIC.DEFENSE_RATING.FLOOR	.EQ $0A ;#CONSTANT modifier isn't applied if skill is below this level
COMBAT.STATS.RESIST_MAGIC.R_X_R			.EQ SHAPE.HOPPER0+$B0 ;$2byte
;														;$B1 in use
COMBAT.STATS.RESIST_MAGIC.PERCENT			.EQ SHAPE.HOPPER0+$B2 ;$1byte
COMBAT.STATS.RESIST_MAGIC.DEFENSE_RATING	.BS $2 ;$1byte

;***SHAPE.HOPPER0+$B3-$BA is available


COMBAT.STATS.DAMAGE.RANGE.PERCENT		.EQ $A	;#CONSTANT. Applied to the armor median to calculate the LO/HI range values for the damage roll
COMBAT.STATS.DAMAGE_MEDIAN				.EQ SHAPE.HOPPER0+$BB ;$1byte
COMBAT.STATS.BASE_DAMAGE_LO				.EQ SHAPE.HOPPER0+$BC ;$1byte
COMBAT.STATS.BASE_DAMAGE_HI				.EQ SHAPE.HOPPER0+$BD ;$1byte
COMBAT.STATS.BASE_DAMAGE.MOD_TALLY	.EQ SHAPE.HOPPER0+$BE ;$2byte
;													;$BF
COMBAT.STATS.DAMAGE.TALLY			.EQ SHAPE.HOPPER0+$C0 ;$2byte
;													;$C1 in use
COMBAT.STATS.BASE_DAMAGE.ROLL		.EQ SHAPE.HOPPER0+$C2 ;$1byte
COMBAT.STATS.BASE_DAMAGE.ROLL_X2	.EQ COMBAT.OUT_OF_ORDER.SHARED.MEMORY12 ;$2byte
COMBAT.STATS.CRTL_HIT_DAMAGE		.EQ COMBAT.OUT_OF_ORDER.SHARED.MEMORY13 ;$2byte												;$C7 in use
COMBAT.STATS.CRITICAL_HIT.FLOOR		.EQ $07	;#CONSTANT critial hit below this skill level does no damage
COMBAT.STATS.CRTL_HIT.TO_HIT.ROLL	.EQ SHAPE.HOPPER0+$C8 ;$1byte
COMBAT.STATS.CRTL_HIT_BLOCK.ROLL	.EQ SHAPE.HOPPER0+$C9 ;$1byte
COMBAT.STATS.CRTL_HIT_FLAG			.EQ SHARED.VARIABLE_SPACE.BLOCK1+$F3 ;($00 = critical hit succeeded, $01 = critical hit not attempted, $03 = critical hit blocked by defender)
COMBAT.STATS.CRTL_HIT.NOT_ENGAGED_VALUE	.EQ $FF ;#CONSTANT
COMBAT.STATS.CRTL_HIT.PROB			.EQ SHAPE.HOPPER0+$CA ;$1byte. If less than this number, critical hit occurs
COMBAT.STATS.CRTL_HIT.DEX 			.EQ COMBAT.STATS.DEFENDER.XP ;$1byte
COMBAT.STATS.CRTL_HIT.PROB.MAX 		.EQ $5F ;#CONSTANT= !95 (BCD)

;***SHAPE.HOPPER0 (none) is available

COMBAT.STATS.REGULAR.ARMOR.RANGE.PERCENT	.EQ COMBAT.STATS.DAMAGE.RANGE.PERCENT	;#CONSTANT. Applied to the armor median to calculate the LO/HI range values for the armor roll
;COMBAT.STATS.REGULAR.ARMOR_RANGE.SIZE		.EQ $A ;#CONSTANT. =PC defense HI - PC defense LO
;COMBAT.STATS.REGULAR.ARMOR_RANGE.HALF_SIZE	.EQ $5  ;#CONSTANT. =INV_COMBAT.ARMOR_RANGE.SIZE/2 rounded up
COMBAT.STATS.REGULAR.ARMOR.ROLL				.EQ SHARED.VARIABLE_SPACE.BLOCK1+$F4 ;$1byte

COMBAT.STATS.DAMAGE.ARMOR_TALLY		.EQ SHAPE.HOPPER0+$CB ;$2byte
;													 ;$CC in use

;***SHAPE.HOPPER0+$CD-$DA is available
COMBAT.STATS.DAMAGE.SUBTOTAL		.EQ SHAPE.HOPPER0+$DB ;$2byte	
;													 ;$DC in use
COMBAT.STATS.DAMAGE.TYPE			.EQ SHARED.VARIABLE_SPACE.BLOCK1+$F5 ;$1byte   ($00 = melee | $01 range, $02 spell. High-bit not set = non-magic weapon (doesn't apply to type=spell or MOBs) | high-bit set = magic weapon (doesn't apply to type=spell or MOBs)
;COMBAT.STATS.DAMAGE.FINAL			.EQ SHAPE.HOPPER0+$DD ;$2byte	
COMBAT.STATS.DAMAGE.FINAL			.EQ COMBAT.OUT_OF_ORDER.SHARED.MEMORY14 ;$2byte	
;													;$DE in use
COMBAT.STATS.BASE_DAMAGE.MIN		.EQ $05	 ;#CONSTANT				
COMBAT.STATS.BASE_DAMAGE.MAX		.EQ $FF	 ;#CONSTANT				

COMBAT.STATS.DMG_RANGE.SIZE			.EQ $14 ;#CONSTANT. =damage HI - damage LO
COMBAT.STATS.DMG_RANGE.HALF_SIZE	.EQ $A  ;#CONSTANT. =INV_COMBAT.DAMAGE_RANGE.SIZE/2 rounded up
 
;***SHAPE.HOPPER0+$DF-$E8 is available

COMBAT.ACQUIRE.TARGET.FINAL.SINDEX		.EQ SHARED.VARIABLE_SPACE.BLOCK1+$F6 ;$1byte
COMBAT.STATS.PARM.PERCENT				.EQ SHAPE.HOPPER0+$E9 ;$1byte
COMBAT.STATS.APPLY.PERCENT.MODE			.EQ SHAPE.HOPPER0+$EA ;$1byte  ($00 = hex mode | $01 = BCD mode)
COMBAT.STATS.APPLY_PERCENT.PARM_BASE	.EQ SHAPE.HOPPER0+$EB ;$2byte
;													;$EC in use
COMBAT.ENGAGED_SINDEX.UPDATE.PARM		.EQ SHAPE.HOPPER0+$ED ;$1byte
;

COMBAT.STATS.CALC.RANGE.MEDIAN.PARM		.EQ SHAPE.HOPPER0+$EE ;$2byte  (setup as 2 bytes for future expansion)
													;$EF in use
;***SHAPE.HOPPER0+$F0-$F3 is available for above section

;***SHAPE.HOPPER0+$F4-$FF is available for new sections

;BLOCK1 left variable space $F7-$FF

@END

;COMBAT SPELL DAMAGE
@START
COMBAT.STATS.SPELL.KILL_FLAG_MASTER	.EQ COMBAT.OUT_OF_ORDER.SHARED.MEMORY2 ;death magic attack: ($00 = succeeded | $01 = not attempted | $03 = failed). If set then all targets hit by the spell are automatically killed. Sometimes saving thows may be available. 
COMBAT.STATS.SPELL.KILL_FLAG		.EQ COMBAT.OUT_OF_ORDER.SHARED.MEMORY8 ;death magic attack: ($00 = succeeded | $01 = not attempted | $03 = failed). If set then all targets hit by the spell are automatically killed. Sometimes saving thows may be available. 
COMBAT.STATS.DEATH_MAGIC_ST.ROLL	.EQ COMBAT.STATS.DODGE_SKILL.ROLL
COMBAT.STATS.DEATH_MAGIC_ST.PROB 	.EQ COMBAT.STATS.DEFENDER.DODGE.PROB

COMBAT.STATS.SPELL.DAMAGE		.EQ COMBAT.OUT_OF_ORDER.SHARED.MEMORY3 ;$1 byte. median damage (not divided by 2)
COMBAT.STATS.SPELL.MP_COST		.EQ COMBAT.OUT_OF_ORDER.SHARED.MEMORY4



;**OPT** Memory. replace the .BS defintions with unused shared variables in the early part of the combat
;section (lots of empty ones were left between sections)
@END


;***for below see ;LOCAL VARIABLES in SWAP.ROUTINES.Combat.stats_routines.asm
;COMBAT.LEVEL_TO_HIT_MODIFIER.TABLE

;COMBAT ATTACK COMMAND
@START
COMBAT.ATTACK_COMMAND.ITERATION_COUNTER	;($00 = left hand | $01 = right hand)	.EQ SHARED.VARIABLE_SPACE.BLOCK1+$F7 ;$1byte
COMBAT.ATTACK_COMMAND.WEAPON.RADIUS		.EQ SHARED.VARIABLE_SPACE.BLOCK1+$F8 ;$1byte
COMBAT.ATTACK_COMMAND.SHAPE_ID			.EQ SHARED.VARIABLE_SPACE.BLOCK1+$F9 ;$1byte
COMBAT.ATTACK_COMMAND.WP_SHAPE_TYPE		.EQ SHARED.VARIABLE_SPACE.BLOCK1+$FA ;$1byte
;COMBAT.ATTACK_COMMAND.CURRENT_HAND		.EQ COMBAT.ATTACK_COMMAND.ITERATION_COUNTER	;($00 = left hand | $01 = right hand)
COMBAT.ATTACK_COMMAND.NO_READIED_WEAPON .EQ $01 ;used when this hand has shield or other hand has 2hd weapon
COMBAT.ATTACK_COMMAND.MOB_SPELL_PROFILE	.EQ COMBAT.ATTACK_COMMAND.ITERATION_COUNTER	;($00 = left hand | $01 = right hand)
			
COMBAT.ATTACK_COMMAND.LH_STATUS.FLAG .EQ COMBAT.OUT_OF_ORDER.SHARED.MEMORY_A ;($00 = left hand attack not complete | $01 = left hand attack complete | $02 target not in range OR no weapon readied in left hand)
COMBAT.ATTACK_COMMAND.RH_STATUS.FLAG .EQ COMBAT.OUT_OF_ORDER.SHARED.MEMORY_B ;($00 = right hand attack not complete | $01 = right hand attack complete)
COMBAT.ATTACK_COMMAND.ACTIVE_HAND 		 .EQ COMBAT.OUT_OF_ORDER.SHARED.MEMORY_C ;(high bit not set = left hand active | high bit set = right hand active)
COMBAT.ATTACK_COMMAND.ATTACKS_MADE		.EQ COMBAT.OUT_OF_ORDER.SHARED.MEMORY17 ;$1byte ($00 = none | $01 = one | $02 = two ). # of range/melee attacks made by the active player this turn. 
COMBAT.ATTACK_COMMAND.NO_TARGET.FLAG	.EQ COMBAT.OUT_OF_ORDER.SHARED.MEMORY18 ;$1byte
COMBAT.ATTACK_COMMAND.DUAL_WIELDING.SPACE.FLAG	.EQ COMBAT.OUT_OF_ORDER.SHARED.MEMORY19 ;$1byte ($00 = not set | $01 = set)


COMBAT.ANGLED_PROJECTILE.SHAPE_ID.START			.EQ $03 ;#CONSTANT. The first shape_ID used for range weapon projectiles
COMBAT.NON_ANGLED_PROJECTILE.SHAPE_ID.START		.EQ $06 ;#CONSTANT. The first shape_ID used for range weapon projectiles

@END

;COMBAT STATS DISPLAY
@START
COMBAT.ATTACKER.SINDEX			.EQ SHARED.VARIABLE_SPACE.BLOCK1+$FB ;$1byte 
COMBAT.TARGET.SINDEX			.EQ SHARED.VARIABLE_SPACE.BLOCK1+$FC ;$1byte 
COMBAT.DEFENDER.SINDEX			.EQ COMBAT.TARGET.SINDEX

@END


;*****WARNING: review shared variable space before 
;using higher than SHARED.VARIABLE_SPACE.BLOCK2+$F2 (conflict with PRODOS.IO reservation)

@END

;COMBAT SETUP
@START

;PARAMETERS
;COMBAT_SE.MODE.PARM				.EQ	SHARED.VARIABLE_SPACE.BLOCK2+$0  ;$1bytes. ($00 = player initiated | $01 = mob initiated | $FF = test mode)
COMBAT_SE.MODE.PARM				.BS $1	;($00 = player initiated | $01 = mob initiated | $FF = test mode)
COMBAT_SE.S_ENTITY.MO_INDEX		.BS $1
COMBAT_SE.S_ENTITY.TYPE			.BS $1
COMBAT_SE.MOB_TILE_ID.PARM		.BS $1 ;The Tile_ID of the MOB on the main map which attacked the player or the player attacked
COMBAT_SE.TERRAIN_TILE_ID.PARM	.EQ	SHARED.VARIABLE_SPACE.BLOCK2+$0  ;$1bytes. The Tile_ID of the terrain the player was standing on when combat was initiated. 
COMBAT_SE.MOB.GROUP_ID.RECORD.SIZE	.EQ	$04 ;#CONSTANT

MOB.GROUP_ID.LOOKUP.POINTER		.EQ $B0		;#POINTER
COMBAT.BATTLEFILED.POINTER		.EQ $B0		;#POINTER

COMBAT_SE.GROUP_TABLE.SEEK_BYTES		.EQ	SHARED.VARIABLE_SPACE.BLOCK2+$1  ;$2bytes.
							;.EQ	SHARED.VARIABLE_SPACE.BLOCK2+$2 ---IN USE-

COMBAT_SE.ONE_TIME_ADJUSTMENT.SEEK_BYTES .EQ	SHARED.VARIABLE_SPACE.BLOCK2+$3  ;$2bytes.
							;.EQ	SHARED.VARIABLE_SPACE.BLOCK2+$4 ---IN USE-
COMBAT_SE.CHAR_SHEET.NEXT_RECORD.SEEK_BYTES	.EQ	SHARED.VARIABLE_SPACE.BLOCK2+$5  ;$2bytes.
							;.EQ	SHARED.VARIABLE_SPACE.BLOCK2+$6 ---IN USE-


; COMBAT_SE.CHAR_SHEET.LAST_RECORD.OFFSET		.EQ	SHARED.VARIABLE_SPACE.BLOCK2+  ;$2bytes.
							; ;.EQ	SHARED.VARIABLE_SPACE.BLOCK2+ ---IN USE-
							
							
; COMBAT_SE.SEEK_BYTES.CURRENT	.EQ	SHARED.VARIABLE_SPACE.BLOCK2+  ;$2bytes.
							; ;.EQ	SHARED.VARIABLE_SPACE.BLOCK2+ ---IN USE-
						
	;BLOCK2 variables space might be free from $07 - $10. I'm not sure the first variable needs to start at $10, I copied these from the inventory routine. 

;COMBAT_SE.READ.MOB_TABLE.FILE
@START
COMBAT_SE.MOB_GROUP_TABLE.RECORD.READ	.EQ	SHARED.VARIABLE_SPACE.BLOCK2+$11  ;$42bytes. The mob tables file contains multiple tables. The constant should be set to the number of bytes in the table with the largest record since all mob tables share the same read record array. 
COMBAT_SE.MOB_GROUP_TABLE.SLOT.DATA .EQ COMBAT_SE.MOB_GROUP_TABLE.RECORD.READ+$2 ;Skips the table bytes with non-slot specific data.
COMBAT_SE.MOB_GROUP_TABLE.SLOT.DATA.SIZE .EQ $40 ;#CONSTANT. The number of bytes associated with the slotd data portion of the group tables records (i.e. doesn't include max mob qty or fluctuation fields)
COMBAT_SE.MOB_IDS.THIS_ENCOUNTER		.EQ SHARED.VARIABLE_SPACE.BLOCK1+$C8 ;***OUT-OF-ORDER*** $11bytes. Stores the MOB_IDs identified in the set map object loop phase1 so they can be processed in phase2 of the loop.
COMBAT_SE.MOB_IDS.THIS_ENCOUNTER.SORTED	.EQ SHARED.VARIABLE_SPACE.BLOCK1+$D9 ;***OUT-OF-ORDER*** $12bytes. Stores the sorted MOB_IDs identified in the set map object loop phase1 so they can be processed in phase2 of the loop.
COMBAT_SE.MOB_GROUP_TABLE.RECORD.SIZE		.EQ COMBAT_SE.MOB_GROUP_TABLE.SLOT.DATA.SIZE+2 ;#CONSTANT. The mob tables file contains multiple tables with different record sizes that share the same read record output array.
;COMBAT_SE.MOB.NUMBER						.EQ SHARED.VARIABLE_SPACE.BLOCK2+ ;$1byte. Tracks the mob sequential number in the the phase 2 loop

	;BLOCK2 variable space $53-$56 available (SHARED.VARIABLE_SPACE.BLOCK2)

COMBAT_SE.MOB_GROUP_TABLE.RECORD_ID .EQ	SHARED.VARIABLE_SPACE.BLOCK2+$57  ;$1bytes.
COMBAT_SE.MOB_SLOT.RECORD.SIZE	.EQ $04 ;#CONSTANT. The mobs slots are sub-records of the mob group record. 
COMBAT_SE.MOB.FLAG_VALUE			.EQ	SHARED.VARIABLE_SPACE.BLOCK2+$58  ;$1bytes.
COMBAT_SE.MOB.X_AXIS			.EQ	SHARED.VARIABLE_SPACE.BLOCK2+$59  ;$1bytes.
COMBAT_SE.MOB.Y_AXIS			.EQ	SHARED.VARIABLE_SPACE.BLOCK2+$5A  ;$1bytes.

COMBAT_SE.MOB_CHAR_SHEET_TABLE.RECORD.SIZE	.EQ $20 ;#CONSTANT. The mob tables file contains multiple tables with different record sizes that share the same read record output array.
COMBAT_SE.MAP_OBJECT.INDEX		.EQ	SHARED.VARIABLE_SPACE.BLOCK2+$5B  ;$1bytes.


	
@END

	;BLOCK2 variable space $5B-$60 available (SHARED.VARIABLE_SPACE.BLOCK2)

;COMBAT_SE.STATS.APPLY.PERCENTAGE
@START
COMBAT_SE.STATS.PERCENT.PARM				.EQ SHARED.VARIABLE_SPACE.BLOCK2+$61 ;$1byte
COMBAT_SE.STATS.APPLY.PERCENT.MODE			.EQ SHARED.VARIABLE_SPACE.BLOCK2+$62 ;$1byte  ($00 = hex mode | $01 = BCD mode)
COMBAT_SE.STATS.APPLY_PERCENT.PARM_BASE	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$63 ;$2byte
					;			;SHARED.VARIABLE_SPACE.BLOCK2+$64 -IN USE-

@END

	;BLOCK2 variable space $65-$66 available (SHARED.VARIABLE_SPACE.BLOCK2)


;COMBAT_SE.RANDOMIZE.VARIANCE
@START
COMBAT_SE.RANDOM_NUMBER.TIERS	.EQ SHARED.VARIABLE_SPACE.BLOCK2+67 ;$1byte
COMBAT_SE.MOB_QTY.RANDOM_NUMBER	.EQ SHARED.VARIABLE_SPACE.BLOCK2+68 ;$1byte
		
		
@END

;**WARNING: don't use above $70 in SHARED.VARIABLE_SPACE.BLOCK2 as that is used by CHR_SHEET.RECORD.READ($80)
;**WARNING: don't use SHARED.VARIABLE_SPACE.BLOCK2+$F2 - $FF, required by PRORWTS


;SCREEN SETUP
@START
;the routines to setup the text window for NPC talk
;were copied into the combat setup file and then modified for
;the few differences that exist. 

;ROSTER WINDOW
TWB.ROSTER_WINDOW.TOP_LINE		.EQ	$04 ;#CONSTANT. 
TWB.ROSTER_WINDOW.LEFT_SBYTE	.EQ $18	;#CONSTANT. left edge of top line
TWB.ROSTER_WINDOW.RIGHT_SBYTE	.EQ $27 ;#CONSTANT. right edge screen byte
TWB.ROSTER_WINDOW.BOTTOM_LINE	.EQ $6F ;#CONSTANT. right edge screen byte



;SCROLL WINDOW
TWB.COMBAT.SCROLL_WINDOW.TOP_LINE				.EQ	$68 ;#CONSTANT. 
TWB.COMBAT.SCROLL_WINDOW.LEFT_SBYTE				.EQ $18	;#CONSTANT. left edge of top line
TWB.COMBAT.SCROLL_WINDOW.RIGHT_SBYTE			.EQ $27 ;#CONSTANT. right edge screen byte



	;scroll window
TWS.COMBAT.SCROLL_WINDOW.TOP_LINE				.EQ $70	;#CONSTANT.	Top hi-res line
TWS.COMBAT.SCROLL_WINDOW.BOTTOM_LINE			.EQ $B8	;#CONSTANT.	Top hi-res line
TWS.COMBAT.SCROLL_WINDOW.LEFT_SBYTE				.EQ $19	;#CONSTANT.
TWS.COMBAT.SCROLL_WINDOW.RIGHT_SBYTE			.EQ $26 ;#CONSTANT. right edge screen byte
TWS.COMBAT.SCROLL_WINDOW.TOP_ROW				.EQ $E ;#CONSTANT		
TWS.COMBAT.SCROLL_WINDOW.BOTTOM_ROW				.EQ $16 ;#CONSTANT		
TWS.COMBAT.SCROLL_WINDOW.WIDTH					.EQ TWS.COMBAT.SCROLL_WINDOW.RIGHT_SBYTE-TWS.COMBAT.SCROLL_WINDOW.LEFT_SBYTE+$1	;#CONSTANT
TWS.COMBAT.SCROLL_WINDOW.CURSOR_START_SBYTE		.EQ TWS.COMBAT.SCROLL_WINDOW.LEFT_SBYTE ;#CONSTANT
TWS.COMBAT.SCROLL_WINDOW.CURSOR_START_ROW		.EQ TWS.COMBAT.SCROLL_WINDOW.TOP_ROW ;#CONSTANT

	
	
;ROSTER WINDOW
CHR_ROSTER.STATUS_BAR.VDIST				.EQ $10 ;#CONSTANT. Verticle distance in hi-res lines between character status bar locations
CHR_ROSTER.HP_STATUS_BAR.START_SBYTE 	.EQ $1F ;#CONSTANT.
CHR_ROSTER.MP_STATUS_BAR.START_SBYTE 	.EQ $20 ;#CONSTANT.


CHR_ROSTER.STATUS_BAR.START_LINE		.EQ $12	;#CONSTANT.
CHR_ROSTER.STATUS_BAR.CURRENT_LINE		.EQ SHAPE.HOPPER0+$80 ;$1byte

CHR_ROSTER.NAME.START_SBYTE				.EQ $19 ;#CONSTANT.
CHR_ROSTER.HEALTH_STATUS.SBYTE			.EQ $26 ;#CONSTANT.

;DRAW STATUS BAR SUBROUTINE
STATUS_BAR.LOOP_COUNTER		.EQ SHAPE.HOPPER0+$90 ;#CONSTANT. $1byte
STATUS_BAR.SEGMENTS			.EQ SHAPE.HOPPER0+$91 ;#CONSTANT. $1byte
STATUS_BAR.DIRECTION_CODE	.EQ SHAPE.HOPPER0+$92 ;#CONSTANT. $1byte
STATUS_BAR.CURRENT_SBYTE	.EQ SHAPE.HOPPER0+$93 ;#CONSTANT. $1byte
STATUS_BAR.CURRENT_LINE		.EQ SHAPE.HOPPER0+$94 ;#CONSTANT. $1byte
STATUS_BAR.BYTE0_VALUE		.EQ SHAPE.HOPPER0+$95 ;#CONSTANT. $1byte	
STATUS_BAR.BYTE1_VALUE		.EQ SHAPE.HOPPER0+$96 ;#CONSTANT. $1byte	
STATUS_BAR.MAX_SEGMENTS.PLUS_ONE .EQ SHAPE.HOPPER0+$97 ;#CONSTANT. $1byte

;**SHAPE.HOPPER2 $98-$9F available
	
; ;.ROLLING_CLEAR.ROSTER_WINDOW	
; CLEAR_ROSTER.CURRENT.START_LINE		.EQ SHAPE.HOPPER0+ ;#CONSTANT. $1byte	


@END

@END

;COMBAT EXIT
@START

;LEVELUP
COMBAT.LEVELUP.NEXT_LEVEL.XP	.EQ SHAPE.HOPPER0+$30 ;$2byte
;									;$31 in use
COMBAT.LEVELUP.FLAG				.EQ SHAPE.HOPPER0+$32 ;$1byte ($00 = levelup, $01 = no levelup)
COMBAT.LEVELUP.MAX_LEVEL		.EQ $0A ;#CONSTANT
COMBAT.SKILL.MAX				.EQ $FF ;#CONSTANT

COMBAT.LEVELUP.SKILL_THREASHOLD.POINTER	.EQ $C4	;#POINTER
COMBAT.LEVELUP.SKILL_PROG_SIZE.POINTER	.EQ $C2 ;#POIBTER

COMBAT.LEVELUP.SKILL.PARM		.EQ SHAPE.HOPPER0+$33 ;$1byte
COMBAT.LEVELUP.SKILL_PROG.PARM	.EQ SHAPE.HOPPER0+$34 ;$1byte

			

;***also see ;LOCAL VARIABLES in SWAP.ROUTINES.COMBAT.SETUP_EXIT.ASM
;MELEE_RANGE_DODGE.SKILL.TABLE.SIZE


@END

;*****WARNING: review shared variable space before 
;using higher than SHARED.VARIABLE_SPACE.BLOCK2+$F2 (conflict with PRODOS.IO reservation)

@END

;======COMMAND ENTRANCES (main game loop)========
@START
;COMMAND.ATTAACK
@START
COMMAND.ATTACK.KEYPRESS.PARM	.EQ	SHARED.VARIABLE_SPACE.BLOCK2+$0  ;$1bytes.


@END
@END

;======COMPRESSION===========
@START
;unpacker variables, no need to change these
SRC.ZX7	=	$0
DST.ZX7 =	$2
;END.LZ4	=	$4

UNPACK.ZX7	= $BD62	;ZX7 unpacker subroutine


@END

;======DARKNESS_MANAGER======
@START
SCREEN.DARK.ALGORITHM.ROW				.BS	$1
SCREEN.DARK.ALGORITHM.COLUMN			.BS	$1
SCREEN.DARK.ALGORITHM.LOCATION			.BS $1

SCREEN.DARK.MIDDLE_ROW					.EQ $05		;#CONSTANT
SCREEN.DARK.MIDDLE_COLUMN				.EQ $08		;#CONSTANT

SCREEN.DARK.ALGORITHM.LOCATION_STOP		.BS $1
SCREEN.DARK.ALGORITHM.LOCATION_START	.BS $1
SCREEN.DARK.ALGORITHM.LOCATION_CURRENT	.BS $1

SCREEN.DARK.SEARCH_INDEX.STOP			.EQ $85		;#CONSTANT

;===DESIGNATE OBSCURING TILES===
;(SURFACE TILE SETS)
DARK_FLAGS.EQ1							.EQ TILE_ID.MOUNTAINS.TALL	;#CONSTANT, mountains
DARK_FLAGS.EQ2							.EQ TILE_ID.TREES.BIRCH		;#CONSTANT, birch trees


;(BUILDING TILE SETS)
;DARK_FLAGS.GRE100						.EQ TILE_ID.CASTLE.PORTCULLIS_ARCHWAY.LEFT	;#CONSTANT, brick walls
;DARK_FLAGS.GRE100						.EQ TILE_ID.CASTLE.ARCHWAY.LEFT	;#CONSTANT, brick walls
DARK_FLAGS.GRE100						.EQ TILE_ID.CASTLE.WALL_DECORATION.SHIELD	;#CONSTANT, brick walls
DARK_FLAGS.LT101						.EQ TILE_ID.CASTLE.WALL_DECORATION.CHEVRON+1	;#CONSTANT, stone walls
;DARK_FLAGS.EQ101						.EQ TILE_ID.FIREPLACE	;#CONSTANT, 

;(UNDERMAP TILE SETS)
DARK_FLAGS.GRE200						.EQ TILE_ID.CAVERN_WALL.RANGE.GRE	;#CONSTANT
DARK_FLAGS.LT200						.EQ TILE_ID.CAVERN_WALL.RANGE.LT	;#CONSTANT

DARK_FLAGS.GRE201						.EQ TILE_ID.CAVERN_TORCH.RANGE.GRE	;#CONSTANT
DARK_FLAGS.LT201						.EQ TILE_ID.CAVERN_TORCH.RANGE.LT	;#CONSTANT


;==USED IN ELS==
SCREEN.DARK.ELS.TILE					.EQ SAVED.YREG.LOCAL	;HOLDS THE SCREEN LOCATION OF THE ELS OBJECT BEING EXAMINED FOR IT'S LIGHTING EFFECTS. 
SCREEN.DARK.ELS.ONSCREEN_ANCHOR			.EQ SAVED.YREG.LOCAL	;FOR OFFSCREEN ELS OBJECTS, HOLDS THE ONSCREEN LOCATION USED AS AN ANCHOR FOR WHILE THE ELS OBJECT BEING EXAMINED FOR IT'S LIGHTING EFFECTS. 

SCREEN.DARK.ELS.FLOATING_TILE			.EQ SAVED.ACC.LOCAL		;USED IN THE ALGORITHM FOR ELS LIGHTING TO ITERATE THROUGH THE ROWS OF THE LIGHTING PATTERN WITHOUT STARTING AT THE ELS SCREEN TILE LOCATION EACH TIME. 
SCREEN.DARK.ELS.COLUMN.COUNTER			.EQ SAVED.YREG.LOCAL1

;DESIGNATE EXTERNAL LIGHT SOURCE (ELS) TILES

;BUILDINGS
DARK_FLAGS.ELS.EQ1		.EQ TILE_ID.LAMP_POST				;#CONSTANT
DARK_FLAGS.ELS.EQ2		.EQ TILE_ID.LAMP_POST.REPAIR		;#CONSTANT
DARK_FLAGS.ELS.EQ3		.EQ TILE_ID.SCONCE.LEFT_WALL		;#CONSTANT
DARK_FLAGS.ELS.EQ4		.EQ TILE_ID.SCONCE.RIGHT_WALL		;#CONSTANT
DARK_FLAGS.ELS.EQ5		.EQ TILE_ID.FIREPLACE				;#CONSTANT

;UNDERMAP
DARK_FLAGS.ELS.GRE6		.EQ TILE_ID.CAVERN_TORCH.RANGE.GRE	;#CONSTANT
DARK_FLAGS.ELS.LT6		.EQ TILE_ID.CAVERN_TORCH.RANGE.LT	;#CONSTANT


;TILE_ID.FIRE_A
;TILE_ID.FIRE_B


;DELETE ME
TEMPORARY.VARIABLE	.BS $1


;PLS CONSTANTS
SCREEN.DARK.PLS.TORCH					.EQ	$01		;#CONSTANT. 
;**NOTE: SCREEN SIZE CHANGE: there are a bunch of static LDA/STAs that need to be updated in DARKNESS.PLS to implement the torch lighting pattern

@END

;======EVENT MANAGER=========
;***see TIME & EVENTS section below

;======FILES=================
@START

;PRODOS FILENAMES
;format is filename length (bytes), filename. length is raw hex number, not ascii value

;MAIN FILES
BS_AUX.ROUTINES.BK2		.AZ #$0E,/BS_AUX.RTN.BK2/				;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 


SRTN.NPC.TALK			.AZ #$0D,/SRTN.NPC.TALK/			;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 
SRTN.NPC.TALK.ADDRESS	.EQ SWAP_SPACE.MAIN_MEMORY+$1800		;memory address where file is loaded
						;$AE00
SRTN.COMBAT.SETUP		.AZ #$0D,/SRTN.COMBAT.S/			;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 
SRTN.COMBAT				.AZ #$0B,/SRTN.COMBAT/				;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 
SRTN.COMBAT.EXIT		.AZ #$0D,/SRTN.COMBAT.E/			;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 
SRTN.COMBAT.ADDRESS		.EQ COMBAT.MAIN.MODULE.START		;memory address where file is loaded
						;$9000
SRTN.COMBAT.ADDRESS.END	.EQ MAP_OBJECTS.MOB-1 ;#POINTER ADDRESS. Ends just before the mob, general, and NPC map object arrays. 
						;$BCFF	
SRTN.CAST_SPELL.SETUP	.AZ #$0F,/SRTN.CAST_SPELL/				;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 
SRTN.CAST_SPELL.FILE_ADDRESS	.EQ COMBAT.PERSISTANT.MEMORY.START-$600		;memory address where this routine is loaded from the SRTN.CAST_SPELL.SETUP file.						
SRTN.SPELL_FILE			.AZ #$0F,/SRTN.SPELL_FILE/				;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 
SRTN.SPELL_FILE.ADDRESS	.EQ SPELL.FILE.ENTRANCE.BUFFER

SRTN.INVENTORY			.AZ #$0E,/SRTN.INVENTORY/				;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 
SRTN.INVENTORY.ADDRESS	.EQ SWAP_SPACE.MAIN_MEMORY+$0			;memory address where file is loaded
						;$9600

SRTN.NON.BLD			.AZ #$0C,/SRTN.NON.BLD/			;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 
SRTN.NON.BLD.ADDRESS	.EQ SWAP_SPACE.MAIN_MEMORY+$0		;memory address where file is loaded
						;$9600
SRTN.BLD				.AZ #$08,/SRTN.BLD/			;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 
SRTN.BLD.ADDRESS		.EQ SWAP_SPACE.MAIN_MEMORY+$0		;memory address where file is loaded
						;$9600	
						
;PLAYER FILES
DATA.PLY.CHR_SHEET				.AZ #$0E,/DATA.CHR_SHEET/	;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 
DATA.PLY.CHR_SHEET.DATA.SIZE 	.EQ $0300						;#CONSTANT
DATA.PLY.CHR_SHEET.FILE_SIZE	.EQ $0400	 					;#CONSTANT. # of pages. block = 2 pages
;DATA.PLY.READIED				.AZ #$0C,/DATA.READIED/		;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 
DATA.PLY.CHR_SHEET.READIED.SIZE .EQ $60						;#CONSTANT

;DATA FILES: GAME
;DATA.GME.INVENTORY				.AZ #$0C,/DATA.GME.INV/		;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 
DATA.GME.INVENTORY				.EQ SRTN.INVENTORY			;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 
;DATA.GME.MOB_TABLES				.AZ #$0A,/DT.GME.MOB/		;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 



;SHAPE FILES
DATA.SHP.SURF 				.AZ #$0D,/DATA.SHP.SURF/	;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 
DATA.SHP.BLD				.AZ #$0C,/DATA.SHP.BLD/		;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 
DATA.SHP.CASTLE_COURTYARD	.AZ #$0F,/DATA.SHP.CSL_CT/		;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 
DATA.SHP.UM					.AZ #$0B,/DATA.SHP.UM/		;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 


;SURFACE FILES
DATA.MAP.SURF	.AZ #$0D,/DATA.MAP.SURF/	;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 
DATA.SPR.SURF	.AZ #$0D,/DATA.SPR.SURF/	;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 


;UNDERMAP FILES
DATA.MAP.ULV1	.AZ #$0D,/DATA.MAP.ULV1/	;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 
DATA.SPR.ULV1	.AZ #$0D,/DATA.SPR.ULV1/	;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 


;BUILDING FILES

    ;LOCATION 1
	     ;floor1 (map1)
DATA.MAP.M1		.AZ #$0B,/DATA.MAP.M1/	;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 
DATA.SPR.M1		.AZ #$0B,/DATA.SPR.M1/	;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 
DATA.TLK.L001	.AZ #$0D,/DATA.TLK.L001/	;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 
	     ;floor2 (map2)
DATA.MAP.M2		.AZ #$0B,/DATA.MAP.M2/	;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 
DATA.SPR.M2		.AZ #$0B,/DATA.SPR.M2/	;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 
	;LOCATION 2
		;floor1.1 (map3)
DATA.MAP.M3		.AZ #$0B,/DATA.MAP.M3/	;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 
DATA.SPR.M3		.AZ #$0B,/DATA.SPR.M3/	;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 
		;floor2.1 (map4)
DATA.MAP.M4		.AZ #$0B,/DATA.MAP.M4/	;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 
DATA.SPR.M4		.AZ #$0B,/DATA.SPR.M4/	;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 
		;floor3.1 (map5)
DATA.MAP.M5		.AZ #$0B,/DATA.MAP.M5/	;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 
DATA.SPR.M5		.AZ #$0B,/DATA.SPR.M5/	;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 


;*also see offloaded_variables.bin, Data Files section


;FILES LOAD MEMORY ADDRESSES
DATA.OTHER.SUNRISE_SUNSET.AUX.START			.EQ $6C00 ;#CONSTANT

DATA.OTHER.SUNRISE_SUNSET.AUX.LO			.HS	00.BB.76.31.EC.A7
DATA.OTHER.SUNRISE_SUNSET.AUX.HO			.HS	6C.6C.6D.6E.6E.6F

;OTHER FILE INFORMATION
DATA.MAP.SURFACE.TOTAL.SECTORS				.EQ $20 ;#CONSTANT
DATA.MAP.ULV1.TOTAL.SECTORS					.EQ $20 ;#CONSTANT


;FILE RELATED VARIABLES
TOTAL.SECTORS				.BS $1 
USE.COMPRESSION_FLAGS		.BS $1  ($00=OFF | $01 = ON). If OFF, then all zones are compressed. If on, then WZONE.COMPRESSION.FLAGS determines whether a zone is compressed or not. 

;LOCATION.DATA.FILE.CURRENT: see MAP/PLAYER LOCATION


@END

;======GRAPHIC OPERATIONS====
@START

;RED/BLUE TILE BUFFERS (for sunset/sunrise swap)
SHAPE_BUFFER.FLOOR.BRICK.ORANGE		.EQ $2800 ;#POINTER ADDRESS; $20byte buffer. Aux memory start address of the buffer that holds an extra copy of the red version of the floor tile. 
SHAPE_BUFFER.FLOOR.BRICK.BLUE		.EQ $2820 ;#POINTER ADDRESS; $20byte buffer. Aux memory start address of the buffer that holds an extra copy of the blue version of the floor tile. 
SHAPE_BUFFER.STREET.BRICK.ORANGE	.EQ $2840 ;#POINTER ADDRESS; $20byte buffer. Aux memory start address of the buffer that holds an extra copy of the red version of the street tile. 
SHAPE_BUFFER.STREET.BRICK.BLUE		.EQ $2860 ;#POINTER ADDRESS; $20byte buffer. Aux memory start address of the buffer that holds an extra copy of the blue version of the street tile. 

SHAPE_BUFFER.STREET.GRAVEL.ORANGE	.EQ $2880 ;#POINTER ADDRESS; $20byte buffer. Aux memory start address of the buffer that holds an extra copy of the red version of the street tile. 
SHAPE_BUFFER.STREET.GRAVEL.BLUE		.EQ $28A0 ;#POINTER ADDRESS; $20byte buffer. Aux memory start address of the buffer that holds an extra copy of the blue version of the street tile. 

SHAPE_BUFFER.FLOOR_STREET.END	.EQ $28BF ;#POINTER ADDRESS; $20byte buffer. Aux memory end address of the preceding shape buffers.
SHAPE_BUFFER.FLOOR_STREET.SIZE	.EQ $C0  ;#CONSTANT; Size (bytes) of the preceding shape buffers.

SHAPE.BUFFER.MAIN_MEMORY		.EQ SHARED.VARIABLE_SPACE.BLOCK3 ;#POINTER ADDRESS; $20byte buffer. Used as a temporary main memory location when copying between AUX memory shape buffers. 

;DRAWING TILES
TILE.LINE						.BS	$1				;1byt			KEEPS TRACK OF THE CURRENT LINE IN DRAW.TILE

TILE.DEPTH.STANDARD				.EQ $10				;#CONSTANT		# OF LINES IN 1 TILE
TILE.DEPTH.HALF					.EQ $08				;#CONSTANT		NUMBER OF LINES TO DRAW FOR A "HALF TILE". 

;don't borrow TILE.DEPTH's memory for another variables. I think it has a persistent value most of the time. Borrowing it has caused the graphics to go wonky. 
TILE.DEPTH						.BS $1				;				STORES THE NUMBER OF LINES IN THE CURRENT TILE. USUALLY SET TO THE CONSTANT #TILE.DEPTH.STANDARD
TILE.LINE.START					.BS	$1				;1byt			START LINE OF CURRENT TILE TO BE DRAWN (DRAW.TILE)
TILE.LINE.STOP					.BS	$1				;1byt			STOP LINE OF CURRENT TILE TO BE DRAWN (DRAW.TILE)
TILE.LINE.COPY					.BS $1				;1byt			USED FOR COPYING TILES UP/DOWN IN SCROLL.SCREEN AND ITS' SUBROUTINES

TILE.SHAPE.SIZE					.EQ $20				;#CONSTANT 		NUMBER OF BYTES IN THE SHAPE TABLE FOR EACH TILE

	
	
;Graphics Screen Variables (i.e. the entire Apple Hi-Res screen)
SCREEN.STOP_BYTE				.EQ $28				;#CONSTANT		Last screen byte number (+1) on right edge of Apple Hi-Res Screen
SCREEN.EDGE_BYTE				.EQ $27				;#CONSTANT		Last screen byte number on right edge of Apple Hi-Res Screen

;Screen/Tile Draw Variables (i..e the map portion of the screen)
SCREEN.DRAW.START_BYTE			.EQ $02				;#CONSTANT		STARTING SCREEN BYTE OF FIRST TILE
SCREEN.DRAW.START_LINE			.EQ	$08				;#CONSTANT		STARTING LINE OF FIRST TILE
SCREEN.DRAW.STOP_BYTE			.EQ $22				;#CONSTANT		STOP SCREEN BYTE ON RIGHT SIDE
SCREEN.DRAW.STOP_LINE			.EQ	$B7				;#CONSTANT		STOP LINE AT SCREEN BOTTOM
SCREEN.DRAW.STOP_LINE2			.EQ	$A7				;#CONSTANT		DRAW.ROW.SINGLE LIKES THIS VALUE FOR SOME REASON, NOT SURE WHY
SCREEN.DRAW.LAST_COLUMN			.EQ	$20				;#CONSTANT		STARTING SCREEN BYTE FOR DRAWN THE COLUMN ON THE RIGHT EDGE OF SCREEN
COMBAT.SCREEN.DRAW.START_BYTE	.EQ $02				;#CONSTANT		STARTING SCREEN BYTE OF FIRST TILE
COMBAT.SCREEN.DRAW.STOP_BYTE	.EQ $17				;#CONSTANT		STOP SCREEN BYTE ON RIGHT SIDE OF COMBAT SCREEN

SCREEN.DRAW.CURRENT_BYTE		.BS $1				;1byt			KEEPS TRACK OF THE CURRENT SCREEN BYTE IN DRAW.TILE
SCREEN.DRAW.BYTE1				.BS	$1				;1byt			USED IN DRAW.TILE.FOREGROUND TO TRACK SCREEN BYTES INSTEAD OF A REGISTER
SCREEN.DRAW.BYTE2				.BS	$1				;1byt			USED IN DRAW.TILE.FOREGROUND TO TRACK SCREEN BYTES INSTEAD OF A REGISTER
 
SCREEN.ROW.LAST					.EQ $0A				;#CONSTANT	(!10)
COMBAT.SCREEN.ROW.LAST			.EQ $0A			;#CONSTANT 		LAST COLUMN, RIGHT EDGE OF COMBAT SCREEN.
SCREEN.ROW.SIZE					.EQ $11				;#CONSTANT  (!17)
SCREEN.COLUMN.SIZE				.EQ	$0B				;#CONSTANT	(!11)
SCREEN.COLUMN.LAST				.EQ $10				;#CONSTANT	(!16)
COMBAT.SCREEN.COLUMN.LAST		.EQ $0A				;#CONSTANT 		LAST COLUMN, RIGHT EDGE OF COMBAT SCREEN.
SCREEN.COLUMN_CENTER			.EQ $08 ;#CONSTANT
SCREEN.ROW_CENTER				.EQ $05 ;#CONSTANT


SCREEN.ARRAY.LAST_ROW_START		.EQ	$AA				;#CONSTANT		STARTING ARRAY INDEX FOR COPYING TILE DATA INTO SCREEN.TILE.HOPPER
SCREEN.ARRAY.LAST_COLUMN_START	.EQ	$10				;#CONSTANT		STARTING ARRAY INDEX FOR COPYING TILE DATA INTO SCREEN.TILE.HOPPER
SCREEN.ARRAY.LAST_ELEMENT		.EQ	$BA				;#CONSTANT		THE LAST ELEMENT OF SCREEN.TILE.DATA ARRAY
SCREEN.ARRAY.LAST_ELEMENT2		.EQ	$BB				;#CONSTANT		THE LAST ELEMENT OF SCREEN.TILE.DATA ARRAY+$1 (USED FOR STOP VALUE IN CERTAIN SITUATIONS)
SCREEN.ARRAY.OFFSET				.EQ	$11				;#CONSTANT		OFFSET BETWEEN ROWS WHEN ITERATING THROUGH SCREEN.TILE.ARRAY (SAID ANOTHER WAY, ;NUMBER OF ELEMENTS TO ADVANCE IN SCREEN.TILE.ARRAY TO REFERENCE THE NEXT ROW IN THE SAME COLUMN)	
SCREEN.ARRAY.OFFSET_LEFT_DIAGONAL		.EQ $12		;#CONSTANT ;the left/right designation refers to the location of the top of diagonal in an X pattern.
SCREEN.ARRAY.OFFSET_RIGHT_DIAGONAL 		.EQ $10		;#CONSTANT ;the left/right designation refers to the location of the top of diagonal in an X pattern.

SCREEN.ARRAY.STOP_VALUE			.BS $01				;1byt			USED TO DETECTED THE END OF DATA WHEN ITERATING THROUGH A COLUMN IN SCREEN.TILE.DATA

SCREEN.ARRAY.PLAYER_LOCATION	.EQ	$5D				;#CONSTANT		CENTER OF THE SCREEN IN THE SCREEN.TILE.DATA ARRAY
SCREEN.ARRAY.ADJACENT_NORTH		.EQ $4C				;#CONSTANT		TILE DIRECTLY NORTH OF PLAYER LOCATION
SCREEN.ARRAY.ADJACENT_NORTH2	.EQ $3B				;#CONSTANT		2 TILES NORTH OF PLAYER LOCATION

SCREEN.ARRAY.ADJACENT_SOUTH		.EQ $6E				;#CONSTANT		TILE DIRECTLY SOUTH OF PLAYER LOCATION
SCREEN.ARRAY.ADJACENT_SOUTH2	.EQ $7F				;#CONSTANT		2 TILES SOUTH OF PLAYER LOCATION
SCREEN.ARRAY.ADJACENT_EAST		.EQ $5E				;#CONSTANT		TILE DIRECTLY EAST OF PLAYER LOCATION
SCREEN.ARRAY.ADJACENT_EAST2		.EQ $5F				;#CONSTANT		2 TILES EAST OF PLAYER LOCATION
SCREEN.ARRAY.ADJACENT_WEST		.EQ $5C				;#CONSTANT		TILE DIRECTLY WEST OF PLAYER LOCATION
SCREEN.ARRAY.ADJACENT_WEST2		.EQ $5B				;#CONSTANT		2 TILES WEST OF PLAYER LOCATION


SCROLL.STOP_BYTE				.EQ $24				;#CONSTANT		STOP SCREEN BYTE ON RIGHT SIDE WHEN SCREEN SCROLLING
SCROLL.WEST.STOP_BYTE			.EQ $22				;#CONSTANT		STOP SCREEN BYTE ON RIGHT SIDE WHEN SCREEN SCROLLING WEST. NOT SURE WHY THESE NEED TO BE DIFFERENT
SCROLL.STOP.LINE				.EQ $A7				;#CONSTANT		STOP LINE AT BOTTOM WHEN SCREEN SCROLLING

SAVED_TILE_TYPE					.BS $1				;1byt			DRAW.TILE.SINGLE USES THE TILE TYPE STORED IN THIS VARIABLE UNLESS IT IS SET TO $00


;GENERIC DRAWING/SCROLLING VARIABLES
LINE.START 						.EQ TILE.LINE.START
LINE.COPY						.EQ TILE.LINE.COPY
LINE.STOP						.EQ TILE.LINE.STOP

LINE.START.COPY_TO				.EQ LINE.START
LINE.START.COPY_FROM			.EQ LINE.COPY

DRAW.START_BYTE 	.EQ SCREEN.DRAW.BYTE1
DRAW.STOP_BYTE		.EQ TILE.LINE.STOP
DRAW.START_LINE		.EQ TILE.LINE.START
DRAW.STOP_LINE		.EQ TILE.LINE
DRAW.BYTE_VALUE.HORIZONTAL		.EQ SHARED.VARIABLE_SPACE.BLOCK3+$0 ;$2 bytes.
DRAW.BYTE_VALUE.VERTICLE	.EQ SHARED.VARIABLE_SPACE.BLOCK3+$2 ;$2 bytes.
;DRAW.BYTE_VALUE.VERTICLE	.BS $2 ;$2 bytes.
DRAW.STOP_BYTE.MINUS_1		.EQ SHARED.VARIABLE_SPACE.BLOCK3+$4 ;$1 bytes.
DRAW.STOP_LINE.MINUS_1		.EQ SHARED.VARIABLE_SPACE.BLOCK3+$5 ;$1 bytes.
;DRAW.BYTE.ERASE_FLAG 		.EQ SHARED.VARIABLE_SPACE.BLOCK3+ ;$1 bytes. ($00 = not set | $01 = set)
DRAW.LINE.EDGE.FLAGS 		.EQ SHARED.VARIABLE_SPACE.BLOCK3+$6 ;$1 bytes. (bit 2 set = top edge, bit 3 set = bottom edge, bit 4 set = left edge, bit 5 set = right edge)
DRAW.BYTE_VALUE2	 		.EQ SHARED.VARIABLE_SPACE.BLOCK3+$7 ;$1 bytes. 
DRAW.LINE.COMPOSITE.PARM 	.EQ SHARED.VARIABLE_SPACE.BLOCK3+$8 ;$1 bytes. (bit0 set = use hi-res page1 | bit1 set = use page2 | bit0&1 set = use both pages | bit2 set = top edge | bit3 set = bottom edge | bit4 set = left edge | bit5 set = right edge | bit6 = draw/erase mode | bit7 = erase mode)


;SHAPE MOVEMENT VARIABLES (off tile-grid shapes)

;***see shape movement (firing projectiles)
	
@END

;======HI-RES CHARACTER GENERATION (HRCG)=======
@START

HRCG.SIZE.MINUS_1		.EQ $2FF	;#CONSTANT. Size of character set data, in bytes, minus 1. The minus 1 is so that is can be used to dynamically calculate the memory address of the last byte of data. 
	
HRCG.AUX.START			.EQ $5900	;#CONSTANT; Starting memory address in aux memory where Hi Res character set is stored until needed to print a character on screen
HRCG.AUX.END			.EQ $5BFF	;#CONSTANT; Starting memory address in aux memory where Hi Res character set is stored until needed to print a character on screen

HRCG.MAIN.START			.EQ $0C00	;#CONSTANT; Starting memory address in main memory where Hi Res character set is swaped in, when a character needs to be printed to screen
HRCG.MAIN.END			.EQ $0EFF	;#CONSTANT; Starting memory address in main memory where Hi Res character set is swaped in, when a character needs to be printed to screen

HRCG.SHAPE.OFFSET		.EQ $E0		;Stores the offset used to calculate the exact starting address of the shape table for a specific character, using the ASCII value of the character
HRCG.SHAPE.SIZE			.EQ $07		;#CONSTANT; the number of bytes in each character's shape table 

HRCG.BUFFER				.BS $8		;Buffer where a single character copies from aux memory is stored. The HRCG controller at $300 looks in this buffer for the bit map graphics data for all characters it is asked to print. 
HRCG.PAGES				.BS $1		;($01 = hi-res page 1, $02 = hi-res page 2, $03 = both pages)
HRCG.PAGES.SAVED		.BS $1		;Used by wrapper routines to COUT (i.e. PRINT.STR) to set the value of HRCG.PAGES after each COUT call, which resets the value of HRCG.PAGES to $03 on exit. 
	;**OPT** Memory. The above vars can probably be .EQs to some shared variable space. Maybe shape hopper

COUT_CHAR_TYPE.SAVED	.BS $1		;Used by wrapper routines to COUT (i.e. PRINT.STR) to set the value of COUT_CHAR_TYPE after each COUT call, which resets the value of COUT_CHAR_TYPE to $00 on exit. 


	
@END

;=====INVENTORY=====
@START

;MODULES
@START

;SHAPE.HOPPER0 variable space $00-$1F might be mostly free, but some routines used a few of the lower bytes. Also keep in mind this range isn't visible from the monitor

;SHAPE.HOPPER0 variable space $20-$4F reserved for FILE.ITEM_TABLE.RECORD.READ and related variables, but not all of it is used. 

;SHAPE.HOPPER0; out-of-order variable space $E9 - $F8
INV.SHAPE.HOPPER0.OUT_OF_ORDER.E9			.EQ $E9		;#CONSTANT
INV.SHAPE.HOPPER0.OUT_OF_ORDER.EA			.EQ $EA		;#CONSTANT


;GENERAL
INV.SUB_MODULE.LOAD_FLAGS			.EQ	SHAPE.HOPPER0+$17 ;$08 bytes.  ($00 = sub-module not loaded in memory | $01 = sub-module loaded in memory)
;							;SHAPE.HOPPER0+$14-$1F   ====IN USE==== 
;		FLAG_0-8 = INV_0.0-INV_0.8
;		FLAG_9	= INV_4.0 (display_inventory)
;		FLAG_A	= INV 1.0 (ready_unready)
;		FLAG_B	= INV 2.0 (calculate combat stats)
INV.LOAD_FLAG.READY_UNREADY				.EQ $06 ;#CONSTANT
INV.LOAD_FLAG.CALC.COMBAT.STATS			.EQ $07 ;#CONSTANT
INV.SUB_MODULE.LOAD_FLAGS.ARRAY_SIZE	.EQ $08


INV.READY_UNREADY.RUN_FLAG		 	.EQ	SHAPE.HOPPER0+$50 ;$1bytes.   ($00 = ready_unready not called | $01 ready_unready called. ready_unready refers to INV.READY_UNREADY.EQUIPMENT.ENTRANCE)
INV.RE.UPDATE_DAMAGE.FLAG 			.EQ	SHAPE.HOPPER0+$51 ;$1bytes.   ($00 = no changes made that effect DAMAGE on the character sheet | $01 changes were made that affect the same)
INV.RE.UPDATE_DEFENSE.FLAG 			.EQ	SHAPE.HOPPER0+$52 ;$1bytes.   ($00 = no changes made that effect DEFENSE on the character sheet | $01 changes were made that affect the same)

; ;**TEMP**
; INV.DEBUG.ADDRESS	.EQ $B5

**TEMP**
INV.DEBUG.ADDRESS		 			.EQ	SHAPE.HOPPER0+$53 ;$1bytes.   ($00 = no changes made that effect DEFENSE on the character sheet | $01 changes were made that affect the same)
;							;SHAPE.HOPPER0+$54   ====IN USE==== 

;SHAPE.HOPPER0 variable space $54-$58 available

;MENU ICONS
INV.MENU_ICON0.SBYTE .EQ	$01
INV.MENU_ICON1.SBYTE .EQ	$04
INV.MENU_ICON2.SBYTE .EQ	$07
INV.MENU_ICON3.SBYTE .EQ	$0A
INV.MENU_ICON4.SBYTE .EQ	$0D
INV.MENU_ICON5.SBYTE .EQ	$11

;ENTRANCE
@START
INV.SUB_MODULE.LAUNCH_CODE			.EQ	SHAPE.HOPPER0+$59 	;$1bytes.   (inventory sub-module # to launch)
INV.ACTIVE_MENU.CODE				.EQ	SHAPE.HOPPER0+$5A 	;$1bytes. ($0-$5 represents the menu icon active)
INV.ACTIVE_MENU.CODE.MAX			.EQ $05 				;#CONSTANT. Maximum value permitted for this variable
INV.ACTIVE_MENU.CODE.LAST			.EQ	SHAPE.HOPPER0+$5B 	;$1bytes. Saves the value of INV.ACTIVE_MENU.CODE just before it's incremented/decremented.

INV.ACTIVE_MENU.STATS				.EQ $00					;#CONSTANT
INV.ACTIVE_MENU.WEAPONS				.EQ $01					;#CONSTANT
INV.ACTIVE_MENU.ARMOR				.EQ $02					;#CONSTANT
INV.ACTIVE_MENU.MISC_ITEMS			.EQ $03					;#CONSTANT
INV.ACTIVE_MENU.SPELLS				.EQ $04					;#CONSTANT
INV.ACTIVE_MENU.GAME_SETTINGS		.EQ $05				;#CONSTANT


;INV.ERASE.TEXT_WINDOW
INV.ERASE.START_LINE				.EQ	SHAPE.HOPPER0+$5C 	;$1bytes.

INV.ACTIVE_STATS_SCREEN.CODE		.EQ	SHAPE.HOPPER0+$5D 	;$1bytes.
INV.ACTIVE_STATS_SCREEN.MAX			.EQ $03					;#CONSTANT. Maximum value permitted for this variable

;INV.DRAW.MENU_ICON.SELECTOR		
INV.MENU.SELECTOR_PILLAR1.SBYTE				.EQ SHAPE.HOPPER0+$5E 					;#CONSTANT
INV.MENU.SELECTOR_PILLAR2.SBYTE				.EQ INV.SHAPE.HOPPER0.OUT_OF_ORDER.E9	;#CONSTANT

;INV.FILE.READ.INVENTORY_DATA.ENTRANCE
INV.FILE.READ.INVENTORY.OPEN_STATUS			.EQ INV.SHAPE.HOPPER0.OUT_OF_ORDER.EA	;#CONSTANT. ($00 = file was open. $01 = file was not open). Refers to the open status of the inventory file when the read routine is called. 


;SHAPE.HOPPER0 variable space <NONE> available

@END


;READY/UNREADY EQUIPMENT
@START
READY_EQUIPMENT.MODE		.EQ	SHAPE.HOPPER0+$5F ;$1bytes.	($00 = ready equipment mode | $01 = unready equipment mode)
PARM.RE.ITEM_TYPE			.EQ	SHAPE.HOPPER0+$60 ;$1bytes.
PARM.RE.ITEM_ID				.EQ	SHAPE.HOPPER0+$61 ;$1bytes.
RE.ACTIVE_PLAYER			.EQ	SHAPE.HOPPER0+$62 ;$1bytes.
INV.ACTIVE_PLAYER			.EQ RE.ACTIVE_PLAYER
PARM.RE.READY.ITEM_TYPE		.EQ	PARM.RE.ITEM_TYPE	;$1bytes.
PARM.RE.READY.ITEM_ID		.EQ	PARM.RE.ITEM_ID 	;$1bytes.
PARM.RE.UNREADY.ITEM_TYPE	.EQ	SHAPE.HOPPER0+$63 ;$1bytes.
PARM.RE.UNREADY.ITEM_ID		.EQ	SHAPE.HOPPER0+$64 ;$1bytes.
PARM.RE.SEARCH.ITEM_TYPE	.EQ	SHAPE.HOPPER0+$65 ;$1bytes.
PARM.RE.SEARCH.ITEM_ID		.EQ	SHAPE.HOPPER0+$66 ;$1bytes.
INV.RE.RETURN_VALUE			.EQ	SHAPE.HOPPER0+$67 ;$1bytes.

;SHAPE.HOPPER0 variable space $68-$6F available

PARM.RE.UNREADY.CLEAR_FLAG	.EQ	SHAPE.HOPPER0+$70 ;$1bytes. ($00 = clear 1st readied flags field only | $01 clear both readied flags field)
INV.RE.CONFLICT_RECORD1 	.EQ	SHAPE.HOPPER0+$71 ;$6bytes. ($00 = conflicting inventory record LO byte | $01 = conflicting inventory record HO byte | $02 # of times active player has readied the item associated with a given conflict record | $03 = INVENTORY RECORD ITEM_TYPE | $04 = INVENTORY RECORD ITEM_ID)
							;SHAPE.HOPPER0+$72 ;===IN USE===
							;SHAPE.HOPPER0+$73 ;===IN USE===
							;SHAPE.HOPPER0+$74 ;===IN USE===
							;SHAPE.HOPPER0+$75 ;===IN USE===
							;SHAPE.HOPPER0+$76 ;===IN USE===
INV.RE.CONFLICT_RECORD2 	.EQ	SHAPE.HOPPER0+$77 ;$6bytes. ""
							;SHAPE.HOPPER0+$78 ;===IN USE===
							;SHAPE.HOPPER0+$79 ;===IN USE===
							;SHAPE.HOPPER0+$7A ;===IN USE===
							;SHAPE.HOPPER0+$7B ;===IN USE===
							;SHAPE.HOPPER0+$7C ;===IN USE===
INV.RE.CONFLICT.TABLE.SIZE	 .EQ $0C ;#CONSTANT. # of bytes in both conflict records
IN.RE.ITEMS_READIED_IN_HANDS.TOTAL 		 		.EQ	SHAPE.HOPPER0+$7D ;$1bytes.
IN.RE.ITEMS_READIED_IN_HANDS.THIS_RECORD 		.EQ	SHAPE.HOPPER0+$7E ;$1bytes.
;IN.RE.ITEMS_READIED_IN_HANDS.THIS_RECORD.QTY	.EQ	SHAPE.HOPPER0+   ;$1bytes.
IN.RE.DETECT.READIED_INSTANCE.FLAG	.EQ	SHAPE.HOPPER0+$7F ;$1bytes. ($00 = no active readied PC flag found active on inventory record this iteration | $01 = active PC readied flag found active on inventory record this iteration) 
INV.GATE_CHECK.WEIGHT.TALLY 		.EQ	SHAPE.HOPPER0+$80 ;$1bytes.
INV.GATE_CHECK.ADJ_TOTAL_WEIGHT 	.EQ	SHAPE.HOPPER0+$81 ;$1bytes.
INV.SAVED.ITEM_TABLE.RECORD			.EQ	SHAPE.HOPPER0+$82 ;$20bytes.
							;SHAPE.HOPPER0+$83-$A1 ;===IN USE===

PLAYER.INVENTORY.DATA.POINTER.SAVED	.EQ	SHAPE.HOPPER0+$A2 ;$2bytes.
							;SHAPE.HOPPER0+$A3 ;===IN USE===
INV.RE.READIED.QTY			.EQ	SHAPE.HOPPER0+$A4 ;$1bytes.
INV.ITEM_TABLE.WP.SHAPE_ID_HANDS.SAVED	.EQ	SHAPE.HOPPER0+$A5 ;$1bytes. ($00 = not found | $01 =  INV.SEARCH.PLY_INV.CONFLICTS.HANDS found a conflicting 2-handed weapon)
INV.UNREADY.ITERATION_COUNTER			.EQ	SHAPE.HOPPER0+$A6 ;$1bytes. 



;SHAPE.HOPPER0 variable space $A7-$AF available

	
PLAYER.INVENTORY.DATA.POINTER .EQ $B0 ;2bytes. #POINTER

INV.RE.SLOT.UNREADIABLE_ITEM	.EQ $00 ;#CONSTANT. 
INV.RE.SLOT.HANDS				.EQ $01 ;#CONSTANT. (left of right)
INV.RE.SLOT.HEAD				.EQ $02 ;#CONSTANT.
INV.RE.SLOT.TORSO				.EQ $03 ;#CONSTANT.
INV.RE.SLOT.FEET				.EQ $04 ;#CONSTANT.
INV.RE.SLOT.HAND_COVERING		.EQ $05 ;#CONSTANT.
INV.RE.SLOT.FINGER				.EQ $06 ;#CONSTANT.
INV.RE.SLOT.NECK				.EQ $07 ;#CONSTANT.

INV.ITEM_TYPE.ARMOR.SKIN		.EQ $01 ;#CONSTANT.
INV.ITEM_ID.ARMOR.SKIN			.EQ $00 ;#CONSTANT.
;**WARNING** fists type/code value is hard coded in .CHECK.NO_WEAPONS.READIED
INV.ITEM_TYPE.WEAPON.FISTS		.EQ $00 ;#CONSTANT.	
INV.ITEM_ID.WEAPON.FISTS		.EQ $00 ;#CONSTANT.	
INV.ITEM_TYPE.WEAPON			.EQ $00 ;#CONSTANT.	
INV.ITEM_TYPE.ARMOR				.EQ $01 ;#CONSTANT.	
INV.ITEM_TYPE.MISC				.EQ $02 ;#CONSTANT.	
INV.ITEM_TYPE.NO_ATTACK_TURN			.EQ $03 ;#CONSTANT. Hand is in use. This is because other hand has fists readied or a 2-handed weapon readied. In both cases, PC only gets 1 attack per turn. 
@END


;CALCULATE COMBAT STATS
@START
;INV_COMBAT.NO_READIED_WEAPON		.EQ $FE ;#CONSTANT
;INV_COMBAT.PLAYER.SKILL.TYPE 			.EQ	SHAPE.HOPPER0+ ;$1bytes.
INV_COMBAT.PLAYER.SKILL.DAMAGE 			.EQ	SHAPE.HOPPER0+$B0 ;$1bytes.
INV_COMBAT.PLAYER.SKILL.CALC.SUBTOTAL 	.EQ INV_COMBAT.PLAYER.SKILL.DAMAGE
INV_COMBAT.STR_MODIFIER					.EQ	SHAPE.HOPPER0+$B1 ;$1bytes.
INV.CALCULATE.STATS.MODE 				.EQ	SHAPE.HOPPER0+$B2 ;$1bytes.	($00 = general mode. only modified equip slots are processed | $01 = levelup mode. Only lhand/rhand and TO-HIT are processed because they use STR/DEX. | $02 = process all mode. Used for character creation)

;INV_COMBAT.STR_ADJ.FLOOR			.EQ $F ;#CONSTANT. The strength adjustment is based on the number of strength points the player has above this level
;INV_COMBAT.DEX_ADJ.FLOOR			.EQ $F ;#CONSTANT. The DEX adjustment is based on the number of DEX points the player has above this level
;INV_COMBAT.STATS.MOD_DEDUCTION		.EQ	SHAPE.HOPPER0+ ;$1bytes.
;INV_COMBAT.STATS.MODIFIER			.EQ	SHAPE.HOPPER0+ ;$1bytes.

;INV_COMBAT.TO_HIT.BASE				.EQ $99 ;#CONSTANT (DECIMAL). The TO-HIT % for level matched combatants before modifiers are applied
INV_COMBAT.TO_HIT.BASE				.EQ $50 ;#CONSTANT (DECIMAL). The TO-HIT % for level matched combatants before modifiers are applied
INV_COMBAT.DAMAGE.MAX				.EQ $FF ;#CONSTANT

;INV_COMBAT.DEFENSE_RATING.TALLY	.EQ	SHAPE.HOPPER0+ ;$1bytes.

;SHAPE.HOPPER0 variable space $B3-$B6 available (RESERVED FOR THE ABOVE ROUTINE)




@END

;INV_4.DISPLAY_INVENTORY
@START

;PLAYER.INVENTORY.DATA.POINTER			.EQ $B2 ;#ZPAGE POINTER
INV_4.MAX_ONSCREEN.ITEMS 					.EQ $06 ;#CONSTANT. Max number of items that can be displayed in the inventory window
;INV_4.DISPLAY.MODE 							.EQ	SHAPE.HOPPER0+$B7 	;$1bytes ($00 = display weapons | $01 = display armor | $02 = misc items)
INV_4.DISPLAY.MODE 							.BS $1 ;$1bytes ($00 = display weapons | $01 = display armor | $02 = misc items)
INV_4.SPELL_RANGE.LTH						.EQ $40 ;#CONSTANT
INV_4.SCREEN_ITEM_LIST.ARRAY 				.EQ SHAPE.HOPPER0+$B8	;$18bytes	 
;datagram: (inventory table record # LO address, inventory table record # HO address, QTY_AVAILABLE, readied status)
INV_4.SCREEN_ITEM_LIST.ARRAY.RECORD_SIZE 	.EQ $04 ;#CONSTANT. 
INV_4.SCREEN_ITEM_LIST.ARRAY.ARRAY_SIZE 	= INV_4.SCREEN_ITEM_LIST.ARRAY.RECORD_SIZE*INV_4.MAX_ONSCREEN.ITEMS
INV_4.SCREEN_ITEM_LIST.SELECTED.ITEM		.EQ SHAPE.HOPPER0+$D0	;$1bytes	
INV_4.SCREEN_LIST.TOTAL_RECORDS				.EQ SHAPE.HOPPER0+$D1	;$1bytes. ;(0-5 = item # of last item on screen | $FF = no inventory records are eligible for display). If < #INV_4.MAX_ONSCREEN.ITEMS that means less than #INV_4.MAX_ONSCREEN.ITEMS in the party inventory are relevant for display to the active player 
INV_4.ACTIVE_PLAYER 						.EQ RE.ACTIVE_PLAYER
;INV_4.SCREEN_ITEM_LIST.DATA *****see local variables in INV_4.DISPLAY_INVENTORY
INV_4.NEXT_ITEM.QTY_AVAILABLE 				.EQ SHAPE.HOPPER0+$D2	;$1bytes. (quantity of the item in the record found that is available to be readied by the active player)
INV_4.NEXT_ITEM.READIED_STATUS 				.EQ SHAPE.HOPPER0+$D3	;$1bytes. Flag that indicates if the active player has the item (in the record found) readied. ($00 = not readed, $01 = readied).
INV_4.GET.NEXT_ITEM.PARM.MODE 				.EQ SHAPE.HOPPER0+$D4	;$1bytes. ($00 = iterate down | $01 = iterate up)
INV_4.GET_ITEMS.LOOP_COUNTER				.EQ SHAPE.HOPPER0+$D5	;$1bytes.
INV_4.DUAL_WIELDING.FLAG					.EQ SHAPE.HOPPER0+$D6	;$1bytes. ($00 = single wielding | $01 = dual wielding)
; PLAYER.INVENTORY.DATA.POINTER.SAVED			.EQ SHAPE.HOPPER0+	;$2bytes.
; ;									;SHAPE.HOPPER0+$ ===IN USE====
TWF.LEFT_SBYTE.SAVED						.EQ SHAPE.HOPPER0+$D7	;$1bytes.
TWF.WIDTH.SAVED								.EQ SHAPE.HOPPER0+$D8	;$1bytes.
TWF.TOP_ROW.SAVED							.EQ SHAPE.HOPPER0+$D9	;$1bytes.
TWF.BOTTOM_ROW.SAVED						.EQ SHAPE.HOPPER0+$DA	;$1bytes.
TWF.HTAB.SAVED								.EQ SHAPE.HOPPER0+$DB	;$1bytes.
TWF.VTAB.SAVED								.EQ SHAPE.HOPPER0+$DC	;$1bytes.

INV_4.SPELL_READY.HOTKEY_SELECTED			.EQ SHAPE.HOPPER0+$DD	;$1bytes.
INV_4.SPELL_READY.SPELL_CODE				.EQ SHAPE.HOPPER0+$DE	;$1bytes.
;INV_4.INVENTORY_WINDOW.HTAB.SAVED			
INV_4.PRINT.MISC_TEXT.MODE					.EQ SHAPE.HOPPER0+$DF	;$1bytes.

;screen position constants

;****also see INV_4.SCREEN_LIST_ITEM.LOOKUP_TABLE.VTAB in INV_7 local variables
INV_4.HEADER1.HTAB		.EQ 	$01	;#CONSTANT
INV_4.HEADER1.VTAB		.EQ 	$04	;#CONSTANT

INV_4.SCREEN_LIST.ITEM0.VTAB				.EQ $06		;#CONSTANT.	
INV_4.SCREEN_LIST.ITEM_ALL.HTAB.LEFT 		.EQ $01		;#CONSTANT
INV_4.SCREEN_LIST.ITEM_ALL.HTAB.RIGHT 		.EQ $12		;#CONSTANT
INV_4.SCREEN_LIST.ITEM_ALL.READIED_HTAB 	.EQ $01		;#CONSTANT
INV_4.SCREEN_LIST.ITEM_ALL.WT_HTAB			.EQ $0B		;#CONSTANT (1 character before column heading because the print routine used prints 4 BCD unpacked numbers, and the 1st digit is always zero so it is surpressed and replaced with a space)
INV_4.SCREEN_LIST.ITEM_ALL.QTY_HTAB			.EQ $0F		;#CONSTANT (1 character before column heading because the print routine used prints 4 BCD unpacked numbers, and the 1st digit is always zero so it is surpressed and replaced with a space)
INV_4.SCREEN_LIST.ITEM_ALL.QTY_DASH_HTAB	.EQ $12		;#CONSTANT (used when QTY = 0, but active player has item readied. Since QTY supresses zeros, the HTAB needs to be adjusted to print a - for the last digit)
INV_4.SCREEN_LIST.ITEM_ALL.NAME_HTAB 		.EQ $01		;#CONSTANT





;SHAPE.HOPPER0 variable space $E0-$E8 available


@END

;SHAPE.HOPPER0; out-of-order variable space $E9 - $F8


;INV_0.STATS: screen0 (status summary)
INV_0.SCREEN0.FIRST_RUN.FLAG		 			.BS $1 ;$1bytes. .BS to avoid init.   ($00 =  first run of screen0 not complete | $01 first run screen0 complete)




;SHAPE.HOPPER0 variable space F8-$FF available (FOR NEW ROUTINES)

@END

;EQUIPMENT TABLES
@START


FILE.ITEM_TABLE.RECORD.READ 	.EQ	SHAPE.HOPPER0+$20  ;$20bytes.
FILE.ITEM_TABLE.RECORD.SIZE		.EQ $20 ;#CONSTANT

FILE.ITEM_TABLE.TYPE_CODE	 	.EQ	SHAPE.HOPPER0+$40  ;$1bytes.
FILE.ITEM_TABLE.ID			 	.EQ	SHAPE.HOPPER0+$41  ;$1bytes.

INV.ITEM_TABLE.NAME.MAX_SIZE	.EQ $12 ;#CONSTANT (max size of the name itself, the stop value ($00) automatically added by .AZ takes an additional byte which is factored into the field size on the datagram)

INV.WEAPON_TABLE.NAME.MAX_SIZE		.EQ INV.ITEM_TABLE.NAME.MAX_SIZE
INV.ARMOR_TABLE.NAME.MAX_SIZE		.EQ INV.ITEM_TABLE.NAME.MAX_SIZE
INV.MISC_ITEM_TABLE.NAME.MAX_SIZE	.EQ INV.ITEM_TABLE.NAME.MAX_SIZE

;BLOCK2 variable space $42-$48 available (SHARED.VARIABLE_SPACE.BLOCK2)



;INV.READ_WRITE_RECORD.CHAR_SHEET.READIED

CHAR_SHEET.READIED.S_ENTITY.NUMBER .EQ SHAPE.HOPPER0+$49  ;$1bytes.	

;BLOCK2 variable space $4A-$4F available (SHARED.VARIABLE_SPACE.BLOCK2)


;READ_WRITE.CHAR_SHEET.READIED.MODE .EQ SHARED.VARIABLE_SPACE.BLOCK2+ ;$1bytes.	
@END

;FIELD LABELS
@START
;FIELD LABELS:ITEM TABLE
@START

;NOTE: INV_4.DISPLAY_INVENTORY copies the master item table records into a buffer for 
;onscreen items. As a result, the offset constants must be are instead the constants below.
;For example, this occurs in .PRINT.LINE1 (INV_4.PRINT.ALL.ITEMS) when printing the item weight.



;common
INV.ITEM_TABLE.ALL.RESIST.MAGIC			.EQ FILE.ITEM_TABLE.RECORD.READ+$0
INV.ITEM_TABLE.ALL.STR_WEIGHT			.EQ FILE.ITEM_TABLE.RECORD.READ+$2		
INV.ITEM_TABLE.ALL.STR_WEIGHT.OFFSET	.EQ $2	;#CONSTANT	
INV.ITEM_TABLE.ALL.DEX					.EQ FILE.ITEM_TABLE.RECORD.READ+$3
INV.ITEM_TABLE.ALL.INT					.EQ FILE.ITEM_TABLE.RECORD.READ+$4
INV.ITEM_TABLE.ALL.EQUIPMENT_SLOT		.EQ FILE.ITEM_TABLE.RECORD.READ+$C
INV.ITEM_TABLE.ALL.NAME					.EQ FILE.ITEM_TABLE.RECORD.READ+$D
INV.ITEM_TABLE.ALL.NAME.OFFSET			.EQ $D	;#CONSTANT	
INV.ITEM_TABLE.ALL.NAME.MAX_SIZE		.EQ	$12 ;#CONSTANT.

;weapon table
INV.ITEM_TABLE.WP.MAGIC_FLAG			.EQ FILE.ITEM_TABLE.RECORD.READ+$0
INV.ITEM_TABLE.WP.MAGIC_FLAG.OFFSET		.EQ $0
INV.ITEM_TABLE.WP.LEVEL					.EQ FILE.ITEM_TABLE.RECORD.READ+$1
INV.ITEM_TABLE.WP.STR_WEIGHT			.EQ FILE.ITEM_TABLE.RECORD.READ+$2
INV.ITEM_TABLE.WP.DEX					.EQ FILE.ITEM_TABLE.RECORD.READ+$3
INV.ITEM_TABLE.WP.INT					.EQ FILE.ITEM_TABLE.RECORD.READ+$4
INV.ITEM_TABLE.WP.DMG_PWR				.EQ FILE.ITEM_TABLE.RECORD.READ+$5
INV.ITEM_TABLE.WP.DMG_PWR.OFFSET		.EQ $5
INV.ITEM_TABLE.WP.UNSKILLED_MIN			.EQ FILE.ITEM_TABLE.RECORD.READ+$6
INV.ITEM_TABLE.WP.SKILLED_MAX			.EQ FILE.ITEM_TABLE.RECORD.READ+$7
INV.ITEM_TABLE.WP.UPGRADE_RATING		.EQ FILE.ITEM_TABLE.RECORD.READ+$8
INV.ITEM_TABLE.WP.RADIUS				.EQ FILE.ITEM_TABLE.RECORD.READ+$9
INV.ITEM_TABLE.WP.SHAPE_TYPE			.EQ FILE.ITEM_TABLE.RECORD.READ+$A
INV.ITEM_TABLE.WP.SHAPE_ID_HANDS		.EQ FILE.ITEM_TABLE.RECORD.READ+$B
INV.ITEM_TABLE.WP.SHAPE_ID_HANDS.OFFSET	.EQ $B

;armor & misc item table
INV.ITEM_TABLE.AR_MISC.MGK.RESIST				.EQ FILE.ITEM_TABLE.RECORD.READ+$0
INV.ITEM_TABLE.AR_MISC.MGK.RESIST.OFFSET		.EQ $0
INV.ITEM_TABLE.AR.LEVEL							.EQ FILE.ITEM_TABLE.RECORD.READ+$1
INV.ITEM_TABLE.AR.STR_WEIGHT					.EQ FILE.ITEM_TABLE.RECORD.READ+$2
INV.ITEM_TABLE.AR.DEX							.EQ FILE.ITEM_TABLE.RECORD.READ+$3
INV.ITEM_TABLE.AR.INT							.EQ FILE.ITEM_TABLE.RECORD.READ+$4
INV.ITEM_TABLE.AR_MISC.DEFENSE_RATING			.EQ FILE.ITEM_TABLE.RECORD.READ+$5
INV.ITEM_TABLE.AR_MISC.DEFENSE_RATING.OFFSET	.EQ $5

INV.ITEM_TABLE.AR.UPGRADE_RATING				.EQ FILE.ITEM_TABLE.RECORD.READ+$8

;misc item table only
INV.ITEM_TABLE.MI.SPELL_CODE					.EQ FILE.ITEM_TABLE.RECORD.READ+$6
INV.ITEM_TABLE.MI.SPELL_CODE.OFFSET				.EQ $06


@END

;FIELD LABELS: CHARACTER SHEET
@START
;(for a datagram on these fields see the combat spreadsheet)

;-shared (PC & MOB)
CHR_SHEET.PC_MOB.LEVEL		.EQ CHR_SHEET.RECORD.READ+$1
CHR_SHEET.PC_MOB.HP_LO		.EQ CHR_SHEET.RECORD.READ+$2
CHR_SHEET.PC_MOB.HP_HO		.EQ CHR_SHEET.RECORD.READ+$3
CHR_SHEET.PC_MOB.HP_LO.OFFSET	.EQ $02
CHR_SHEET.PC_MOB.HP_HO.OFFSET	.EQ $03
CHR_SHEET.PC_MOB.MP			.EQ CHR_SHEET.RECORD.READ+$4
CHR_SHEET.PC_MOB.XP			.EQ CHR_SHEET.RECORD.READ+$5	;PC = $2bytes, MOB = $1byte

CHR_SHEET.PC_MOB.ARMOR 		.EQ CHR_SHEET.RECORD.READ+$7
;(field eliminted) CHR_SHEET.PC_MOB.ARMOR_HI 	.EQ CHR_SHEET.RECORD.READ+$8

CHR_SHEET.PC_MOB.TO_HIT 			.EQ CHR_SHEET.RECORD.READ+$8
CHR_SHEET.PC_MOB.RESIST_MAGIC 		.EQ CHR_SHEET.RECORD.READ+$B
CHR_SHEET.PC_MOB.SKILL.CRITICAL_HIT .EQ CHR_SHEET.RECORD.READ+$D

CHR_SHEET.PC_MOB.ENGAGED.SINDEX		.EQ CHR_SHEET.RECORD.READ+$C

;-pc only
@START
CHR_SHEET.PC.HEALTH_STATUS					.EQ CHR_SHEET.RECORD.READ+$0

CHR_SHEET.PC.DMG.LHAND 						.EQ CHR_SHEET.RECORD.READ+$9
CHR_SHEET.PC.DMG.RHAND 						.EQ CHR_SHEET.RECORD.READ+$A

CHR_SHEET.PC.RESIST_CRITICAL_HIT 			.EQ CHR_SHEET.RECORD.READ+$D
CHR_SHEET.PC.SKILL.CRITICAL_HIT 			.EQ CHR_SHEET.RECORD.READ+$D
CHR_SHEET.PC.SKILL.CRITICAL_HIT.INDEX 		.EQ $D
CHR_SHEET.PC.SKILL.CRITICAL_HIT.PROG		.EQ CHR_SHEET.RECORD.READ+$E
CHR_SHEET.PC.SKILL.CRITICAL_HIT.PROG.INDEX	.EQ $E
;
CHR_SHEET.PC.ATTRIB.STR			.EQ CHR_SHEET.RECORD.READ+$10
CHR_SHEET.PC.ATTRIB.DEX			.EQ CHR_SHEET.RECORD.READ+$11
CHR_SHEET.PC.ATTRIB.INT			.EQ CHR_SHEET.RECORD.READ+$12

CHR_SHEET.PC.GENDER				.EQ CHR_SHEET.RECORD.READ+$13
;
CHR_SHEET.PC.RACE.MGK_FLG		.EQ CHR_SHEET.RECORD.READ+$14
CHR_SHEET.PC.SHAPE_ID			.EQ CHR_SHEET.RECORD.READ+$15
CHR_SHEET.PC.WP_RADIUS			.EQ CHR_SHEET.RECORD.READ+$16
CHR_SHEET.PC.WP.SHAPE_TYPE		.EQ CHR_SHEET.RECORD.READ+$17
CHR_SHEET.PC.TILE_TYPE			.EQ CHR_SHEET.RECORD.READ+$18
;
CHR_SHEET.PC.SKILL.MELEE 	 		.EQ CHR_SHEET.RECORD.READ+$19
CHR_SHEET.PC.SKILL.MELEE.INDEX 	 	.EQ $19
CHR_SHEET.PC.SKILL.MELEE.PROG  		.EQ CHR_SHEET.RECORD.READ+$1A
CHR_SHEET.PC.SKILL.MELEE.PROG.INDEX	.EQ $1A

CHR_SHEET.PC.SKILL.RANGE 		.EQ CHR_SHEET.RECORD.READ+$1C
CHR_SHEET.PC.SKILL.RANGE.INDEX 	.EQ $1C
CHR_SHEET.PC.SKILL.RANGE.PROG 	.EQ CHR_SHEET.RECORD.READ+$1D
CHR_SHEET.PC.SKILL.RANGE.PROG.INDEX .EQ $1D

CHR_SHEET.PC.SKILL.DODGE 		.EQ CHR_SHEET.RECORD.READ+$1F
CHR_SHEET.PC.SKILL.DODGE.INDEX 	.EQ $1F
CHR_SHEET.PC.SKILL.DODGE.PROG 	.EQ CHR_SHEET.RECORD.READ+$20
CHR_SHEET.PC.SKILL.DODGE.PROG.INDEX .EQ $20

CHR_SHEET.PC.SKILL.LOCKPICKING 			.EQ $22
CHR_SHEET.PC.SKILL.LOCKPICKING.INDEX	.EQ CHR_SHEET.RECORD.READ+$22
CHR_SHEET.PC.SKILL.LOCKPICKING.PROG 	.EQ CHR_SHEET.RECORD.READ+$23
CHR_SHEET.PC.SKILL.LOCKPICKING.PROG.INDEX .EQ $23

CHR_SHEET.PC.SKILL.PILFER				.EQ CHR_SHEET.RECORD.READ+$25
CHR_SHEET.PC.SKILL.PILFER.INDEX			.EQ $25
CHR_SHEET.PC.SKILL.PILFER.PROG 			.EQ CHR_SHEET.RECORD.READ+$26
CHR_SHEET.PC.SKILL.PILFER.PROG.INDEX	.EQ $26
;
CHR_SHEET.PC.NAME.CHARACTER 			.EQ CHR_SHEET.RECORD.READ+$4B ;size = !14 ($E) (useble. doesn't,include stop value)
CHR_SHEET.PC.CHARACTER_NAME.MAX_SIZE	.EQ $E	;#CONSTANT. used in data.ply.character_sheet.ASM
CHR_SHEET.PC.NAME.READIED_WP_LHAND		.EQ CHR_SHEET.RECORD.READ+$5A ;size = !18 ($12)(useble. doesn't,include stop value)
CHR_SHEET.PC.WP_LEFT_NAME.MAX_SIZE		.EQ	INV.ITEM_TABLE.ALL.NAME.MAX_SIZE ;#CONSTANT. ($12) used in data.ply.character_sheet.ASM
CHR_SHEET.PC.NAME.READIED_WP_RHAND		.EQ CHR_SHEET.RECORD.READ+$6D ;size = !18 ($12)(useble. doesn't,include stop value)
CHR_SHEET.PC.WP_RIGHT_NAME.MAX_SIZE		.EQ CHR_SHEET.PC.WP_LEFT_NAME.MAX_SIZE ;#CONSTANT. used in data.ply.character_sheet.ASM
;
CHR_SHEET.PC.READIED_EQUIPMENT_WEIGHT	.EQ CHR_SHEET.RECORD.READ+$46
CHR_SHEET.PC.ATTRIB.UPGRADE_POINTS		.EQ CHR_SHEET.RECORD.READ+$47
CHR_SHEET.PC.HP_MAX.LO					.EQ CHR_SHEET.RECORD.READ+$48
CHR_SHEET.PC.HP_MAX.HO					.EQ CHR_SHEET.RECORD.READ+$49


@END

	

;-mob only
CHR_SHEET.MOB.INT.WP_SHAPE_TYPE		.EQ CHR_SHEET.RECORD.READ+$6

CHR_SHEET.MOB.DMG				  .EQ CHR_SHEET.RECORD.READ+$9
CHR_SHEET.MOB.RESIST_CRITICAL_HIT .EQ CHR_SHEET.RECORD.READ+$A
CHR_SHEET.MOB.SIZE_DODGE 		  .EQ CHR_SHEET.RECORD.READ+$E

CHR_SHEET.MOB.ENGAGED.WP_SHAPE_ID.SPELL_CODE	.EQ CHR_SHEET.RECORD.READ+$F

CHR_SHEET.MOB.TILE_ID			  .EQ CHR_SHEET.RECORD.READ+$10
CHR_SHEET.MOB.TILE_ID.OFFSET	  .EQ $10 ;#CONSTANT. the byte # this field starts at. 

CHR_SHEET.MOB.NAME		 			.EQ CHR_SHEET.RECORD.READ+$11 ;size = !14 ($E) (useble. doesn't,include stop value)
CHR_SHEET.MOB.NAME.MAX_SIZE				.EQ $E	;#CONSTANT. used in SWAP_ROUTINES.Combat.test_data2.ASM

;special only
;(SPECIALS and MOBS use the same character sheet datagram and physical memory space. The following are special specific labels for convenience)
CHR_SHEET.SPECIAL.NAME		 			.EQ CHR_SHEET.MOB.NAME
CHR_SHEET.SPECIAL.NAME.MAX_SIZE			.EQ CHR_SHEET.MOB.NAME.MAX_SIZE


@END

;FIELD LABELS: CHARACTER SHEET (READIED FIELDS)
@START
;for master copy see the bottom of the SWAP.ROUTINES.INV.entrance_exit.ASM file
;(a non-authoratative copy is kept here for easy copy/paste into the inventory module)
;(they couldn't be placed here due to them being a forward reference, since 
;the item table is a local variable in the inventory file)

; CHR_SHEET.READIED_EQUIP.TYPE.LHAND	.EQ CHR_SHEET.PC.READIED_EQUIP.RECORD.READ+$0
; CHR_SHEET.READIED_EQUIP.ID.LHAND	.EQ CHR_SHEET.PC.READIED_EQUIP.RECORD.READ+$1
; CHR_SHEET.READIED_EQUIP.TYPE.RHAND	.EQ CHR_SHEET.PC.READIED_EQUIP.RECORD.READ+$2
; CHR_SHEET.READIED_EQUIP.ID.RHAND	.EQ CHR_SHEET.PC.READIED_EQUIP.RECORD.READ+$3
; CHR_SHEET.READIED_EQUIP.TYPE.HEAD	.EQ CHR_SHEET.PC.READIED_EQUIP.RECORD.READ+$4
; CHR_SHEET.READIED_EQUIP.TYPE.HEAD.OFFSET	.EQ $04 ;#CONSTANT
; CHR_SHEET.READIED_EQUIP.ID.HEAD		.EQ CHR_SHEET.PC.READIED_EQUIP.RECORD.READ+$5
; CHR_SHEET.READIED_EQUIP.TYPE.TORSO	.EQ CHR_SHEET.PC.READIED_EQUIP.RECORD.READ+$6
; CHR_SHEET.READIED_EQUIP.ID.TORSO	.EQ CHR_SHEET.PC.READIED_EQUIP.RECORD.READ+$7
; CHR_SHEET.READIED_EQUIP.TYPE.FEET	.EQ CHR_SHEET.PC.READIED_EQUIP.RECORD.READ+$8
; CHR_SHEET.READIED_EQUIP.ID.FEET		.EQ CHR_SHEET.PC.READIED_EQUIP.RECORD.READ+$9
; CHR_SHEET.READIED_EQUIP.TYPE.HAND_COVERING	.EQ CHR_SHEET.PC.READIED_EQUIP.RECORD.READ+$A
; CHR_SHEET.READIED_EQUIP.ID.HAND_COVERING	.EQ CHR_SHEET.PC.READIED_EQUIP.RECORD.READ+$B
; CHR_SHEET.READIED_EQUIP.TYPE.FINGER			.EQ CHR_SHEET.PC.READIED_EQUIP.RECORD.READ+$C
; CHR_SHEET.READIED_EQUIP.ID.FINGER			.EQ CHR_SHEET.PC.READIED_EQUIP.RECORD.READ+$D
; CHR_SHEET.READIED_EQUIP.TYPE.NECK			.EQ CHR_SHEET.PC.READIED_EQUIP.RECORD.READ+$E
; CHR_SHEET.READIED_EQUIP.ID.NECK				.EQ CHR_SHEET.PC.READIED_EQUIP.RECORD.READ+$F

@END

@END

;DISPLAY SCREENS
@START

;STATS: SCREEN0 (stats summary screen)
@START
; ;character name
; INV.SCREEN.CHAR_NAME.HTAB	.EQ $01		;#CONSTANT. left edge screen byte of the centered space.
; INV.SCREEN.CHAR_NAME.VTAB	.EQ $04		;#CONSTANT. VTAB of the centered space.  
; INV.SCREEN.CHAR_NAME.SIZE	.EQ $12		;#CONSTANT. # of characters in the centered space 

;health
INV.SCREEN.HEALTH.HTAB	.EQ $01		;#CONSTANT. location to print the label preceeding the variable value. 
INV.SCREEN.HEALTH.VTAB	.EQ $04		;#CONSTANT. location to print the label preceeding the variable value.  

;gender
INV.SCREEN.GENDER.HTAB	.EQ $01		;#CONSTANT. location to print the label preceeding the variable value. 
INV.SCREEN.GENDER.VTAB	.EQ $05		;#CONSTANT. location to print the label preceeding the variable value.  

;race
INV.SCREEN.RACE.HTAB	.EQ $01		;#CONSTANT. location to print the label preceeding the variable value. 
INV.SCREEN.RACE.VTAB	.EQ $06		;#CONSTANT. location to print the label preceeding the variable value.  

;level
INV.SCREEN.LEVEL.HTAB	.EQ $01		;#CONSTANT. location to print the label preceeding the variable value. 
INV.SCREEN.LEVEL.VTAB	.EQ $07		;#CONSTANT. location to print the label preceeding the variable value.  

;H.P.
INV.SCREEN.HP.HTAB		.EQ $01		;#CONSTANT. location to print the label preceeding the variable value. 
INV.SCREEN.HP.VTAB		.EQ $09		;#CONSTANT. location to print the label preceeding the variable value.  

;X.P.
INV.SCREEN.XP.HTAB		.EQ $01		;#CONSTANT. location to print the label preceeding the variable value. 
INV.SCREEN.XP.VTAB		.EQ $0A		;#CONSTANT. location to print the label preceeding the variable value.  

;M.P.
INV.SCREEN.MP.HTAB		.EQ $01		;#CONSTANT. location to print the label preceeding the variable value. 
INV.SCREEN.MP.VTAB		.EQ $0B		;#CONSTANT. location to print the label preceeding the variable value.  

;STR
INV.SCREEN.STR.HTAB		.EQ $01		;#CONSTANT. location to print the label preceeding the variable value. 
INV.SCREEN.STR.VTAB		.EQ $0D		;#CONSTANT. location to print the label preceeding the variable value.  

;DEX
INV.SCREEN.DEX.HTAB		.EQ $01		;#CONSTANT. location to print the label preceeding the variable value. 
INV.SCREEN.DEX.VTAB		.EQ $0E		;#CONSTANT. location to print the label preceeding the variable value.  

;INT
INV.SCREEN.INT.HTAB		.EQ $01		;#CONSTANT. location to print the label preceeding the variable value. 
INV.SCREEN.INT.VTAB		.EQ $0F		;#CONSTANT. location to print the label preceeding the variable value.  

;WEAPON NAME: LEFT HAND
INV.SCREEN.LH_WP_NAME.HTAB		.EQ $01		;#CONSTANT. location to print the label preceeding the variable value. 
INV.SCREEN.LH_WP_NAME.VTAB		.EQ $11		;#CONSTANT. location to print the label preceeding the variable value.  

INV.SCREEN.LH_WP_NAME.APPEND_TEXT.HTAB		.EQ $04		;#CONSTANT. location to print the label preceeding the variable value. 
INV.SCREEN.LH_WP_NAME.APPEND_TEXT.VTAB		.EQ $11		;#CONSTANT. location to print the label preceeding the variable value.  
INV.SCREEN.LH_WP_NAME.APPEND_TEXT.SIZE		.EQ $F		;#CONSTANT. # of characters permitted before append

;WEAPON NAME: RIGHT HAND
INV.SCREEN.RH_WP_NAME.HTAB		.EQ $01		;#CONSTANT. location to print the label preceeding the variable value. 
INV.SCREEN.RH_WP_NAME.VTAB		.EQ $12		;#CONSTANT. location to print the label preceeding the variable value.  

INV.SCREEN.RH_WP_NAME.APPEND_TEXT.HTAB		.EQ $04		;#CONSTANT. location to print the label preceeding the variable value. 
INV.SCREEN.RH_WP_NAME.APPEND_TEXT.VTAB		.EQ $12		;#CONSTANT. location to print the label preceeding the variable value.  
INV.SCREEN.RH_WP_NAME.APPEND_TEXT.SIZE		.EQ $F		;#CONSTANT. # of characters permitted before append

;FOORTER
INV.SCREEN.FOOTER.HTAB			.EQ $01 ;#CONSTANT
INV.SCREEN.FOOTER.VTAB			.EQ $16 ;#CONSTANT



@END

;STATS: SCREEN1 (readied equipment)
@START
INV_1.SCREEN1.TITLE.HTAB			.EQ $01 ;#CONSTANT.
INV_1.SCREEN1.TITLE.VTAB			.EQ $04 ;#CONSTANT.


@END

;STATS: SCREEN2 (combat stats)
@START
INV_2.SCREEN2.TITLE.HTAB					.EQ $04 ;#CONSTANT.
INV_2.SCREEN2.TITLE.VTAB					.EQ $04 ;#CONSTANT.

INV_2.SCREEN2.LHAND.VARIABLE.HTAB			.EQ $0F ;#CONSTANT.
INV_2.SCREEN2.LHAND.VARIABLE.VTAB			.EQ $07 ;#CONSTANT.

INV_2.SCREEN2.RHAND.VARIABLE.HTAB			.EQ $0F ;#CONSTANT.
INV_2.SCREEN2.RHAND.VARIABLE.VTAB			.EQ $08 ;#CONSTANT.

INV_2.SCREEN2.ARMOR.VARIABLE.HTAB			.EQ $0F ;#CONSTANT.
INV_2.SCREEN2.ARMOR.VARIABLE.VTAB			.EQ $0A ;#CONSTANT.

INV_2.SCREEN2.RESIST.VARIABLE.HTAB			.EQ $0F ;#CONSTANT.
INV_2.SCREEN2.RESIST.VARIABLE.VTAB			.EQ $0B ;#CONSTANT.

INV_2.SCREEN2.TO_HIT.VARIABLE.HTAB			.EQ $11 ;#CONSTANT.
INV_2.SCREEN2.TO_HIT.VARIABLE.VTAB			.EQ $0D ;#CONSTANT.

@END

;STATS: SCREEN3 (Skills)
@START
INV_3.SCREEN3.TITLE.HTAB					.EQ $07 ;#CONSTANT.
INV_3.SCREEN3.TITLE.VTAB					.EQ $04 ;#CONSTANT.

INV_3.SCREEN3.SKILL_VARIABLE.HTAB			.EQ $0F ;#CONSTANT.
INV_3.SCREEN3.SKILL_PROGRESS_VARIABLE.HTAB	.EQ $09 ;#CONSTANT.


@END


@END


	
;*****WARNING: review shared variable space before 
;using higher than SHARED.VARIABLE_SPACE.BLOCK2+$F2 (conflict with PRODOS.IO reservation)



@END

;======MAP=====
@START
;(relating to the player position on the map and movement on it)
;
;WORMHOLE.DATA
;
;=====================DOCUMENTATION====================================
;
;Synonyms: Wormhole (between maps), enterable location, enterable map location 
;
;Documentation in this section is not complete.
;
;Each map that has wormholes has a hex table below.
;MAP.WORMHOLES_(xx), where xx is the map code. 
;
;-Source/Destination
;Wormholes between maps are added by adding a record to the
;MAP.WORMHOLE hex table for the source map. If the wormhole is two-way, then a record
;needs to be added to the MAP.WORMHOLE hex table for the destination map (effectively they are really two one-way wormholes that work together to create the illusion of a two-way portal)  
;
;-Starting Position
;The starting position for the wormhole destination map is
;defined in MAP.WORMHOLES.START.POSITION_DATA
;There is also a way to use the default starting position for the map type (I think it's based on map type). 
;I need to double check in the code but I think the way to do this is to follow the steps above to 
;add the source/destination of the wormhole and simply skip the step to add a record
;to MAP.WORMHOLES.START.POSITION_DATA. **Update 6/30/17: I'm pretty sure this is the case. 
;
;**Update 6/30/17: it looks like wormholes between town maps floors do not have
;an entry in MAP.WORMHOLES.START.POSITION_DATA, and yet these wormholes start at the 
;correct X,Y on the destination map (floor2), which is the same X/Y as the wormhole entrance (floor1),
;rather than starting at the default map position such as when entering a town map from the surface map. 
;
;Possibly when there is no entry found in MAP.WORMHOLES.START.POSITION_DATA that a check is done to see if a building map
;is active and if so it uses the source X,Y for the destination X,Y.;
;This would also be useful for undermap since my plan is to do it to scale. 
;
;POSSIBLE MAP_TYPE HIGH-BIT USE (label used to refer to this note from elsewhere)
;It would be great to attach this feature to something like the high-bit on the map code
;rather than make it depedant on map type. That way there could still be magical portals
;that don't align with the map scale.
;
;
;=================================================================================



;---MAPS with WORMHOLES---
;FORMAT: (*SOURCE MAP GMAP.X), (*SOURCE MAP GMAP.Y), (DEST. MAP CODE), (DEST. MAP TYPE)
;Stop value is $FF in byte 2
;
;*The X/Y of the wormhole. So if a town map:floor1 map has a ladder that leads to the 2nd floor. The Source MAP X/Y
;would be the X/Y of the ladder and should be entered in the MAP.WORMHOLES_xx with the map code for the town map:floor1.  
;
;												1ST			2nd		    3rd		    4th         5th        	6th         7th         STOP VALUE
MAP.WORMHOLES_00			.HS					18.38.01.01.17.38.01.01.17.37.01.01.18.37.01.01.1D.30.40.03.52.6A.40.03.58.5C.03.02.00.00.FF												;1ST			STOP VALUE
MAP.WORMHOLES_01			.HS					27.22.02.01.11.26.02.01.0A.15.02.01.00.00.FF
MAP.WORMHOLES_02			.HS					27.22.01.01.11.26.01.01.0A.15.01.01.00.00.FF

MAP.WORMHOLES_03			.HS					14.09.04.02.14.20.04.02.00.00.FF
MAP.WORMHOLES_04			.HS					14.09.03.02.14.20.03.02.0B.0A.05.02.00.00.FF
MAP.WORMHOLES_05			.HS					0B.0A.04.02.00.00.FF
;*****WARNING: !!!MAKE SURE STOP VALUE IS IN CORRECT BYTE (#2) OF MAP.WORMHOLES_xx!!!!

MAP.WORMHOLES_40			.HS					38.40.00.00.32.44.00.00.00.00.FF
;
;
;FORMAT: MAP CODE.DESTINATION, *SOURCE GMAP.X, *SOURCE GMAP.Y, **START GMAP.X, **START GMAP.Y
;Stop value is $FF in byte 0
;
;*The wormhole X/Y on the map the player is transporting from, as stored in MAP.WORMHOLES_xx. 
;**The starting X/Y position on the map the player is transporting to. 
;
;												1ST			   2ND	    	  3RD            4TH            STOP VALUE
MAP.WORMHOLES.START.POSITION_DATA	.HS			40.1D.30.32.44.00.32.44.1D.30.40.52.6A.38.40.00.38.40.52.6A.FF						
;MAP.WORMHOLES.START.POSITION_DATA	.HS			40.1D.30.38.40.00.38.40.1D.30.40.52.6A.37.40.04.15.09.0X.0X.FF						

MAP.WORMHOLES.POINTER		.EQ	SHAPE				;HOLDS A POINTER TO THE MAP.LOCAITONS_xx ARRAY ASSOCIATED WITH THE PLAYER'S CURRENT LOCATION
MAP.WORMHOLES.RECORD		.EQ SHARED.VARIABLE_SPACE.BLOCK3		;4 BYTES. HOLDS THE CURRENT RECORD WHEN A MAP.LOCATIONS.xx ARRAY IS BEING ITERATED
MAP.WORMHOLES.RECORD.LENGTH .EQ $04					;#CONSTANT. THE NUMBER OF BYTES FOR EACH RECORD IN THE MAP.LOCAITONS_xx ARRAYS. 
MAP.WORMHOLES.SP.RECORD		.EQ SHARED.VARIABLE_SPACE.BLOCK3+$4	;5 BYTES. HOLDS THE CURRENT RECORD WHEN MAP.WORMHOLES.START.POSITION_DATA ARRAY IS BEING ITERATED
MAP.WORMHOLES.SP.RECORD.LENGTH .EQ $05				;#CONSTANT. THE NUMBER OF BYTES FOR EACH RECORD IN THE MAP.LOCAITONS_xx ARRAYS. 


;MAP TYPES (formerly LOCATION TYPES)
MAP.TYPE.SURFACE				.EQ $00				;#CONSTANT
MAP.TYPE.BUILDING.GRE			.EQ $01				;#CONSTANT
MAP.TYPE.BUILDING.LT			.EQ $03				;#CONSTANT
MAP.TYPE.TOWN_VILLAGE			.EQ $01				;#CONSTANT
MAP.TYPE.CASTLE					.EQ $02				;#CONSTANT
MAP.TYPE.UNDERMAP				.EQ $03				;#CONSTANT
MAP.TYPE.COMBAT				.EQ $FF				;#CONSTANT
MAP.TYPE.SURFACE			.EQ MAP.TYPE.SURFACE ;#CONSTANT
MAP.TYPE.TOWN_VILLAGE			.EQ MAP.TYPE.TOWN_VILLAGE ;#CONSTANT
MAP.TYPE.UNDERMAP			.EQ MAP.TYPE.UNDERMAP ;#CONSTANT

;OTHER
FORWARD.TRANSPORT.BUFFER	.EQ SHARED.VARIABLE_SPACE.BLOCK3+$1C ;4byts. Stores the transport record values for the location the player is exiting so they can be copied to a transport record for the location the player is entering. 

;------PLAYER LOCATION-----

PLAYER.MAP.LOCATION				.BS $1	;contains the location code of the player's current location. 
PLAYER.MAP.LOCATION_TYPE		.BS $1	;contains the location type (i.e. castle, dungeon etc) of the player's current location. 
PLAYER.MAP.LOCATION.LAST		.BS $4	;Tracks position data on the last location the player was in (including surface map). See Map Objects.xls for a datagram on this array

CURRENT.MAP_LOCATION.SPR_DATA		.BS $2 ;Tracks the memory address of the SPR data file associated with the map location the player is currently in
CURRENT.MAP_LOCATION.TLK_DATA		.BS $2 ;Tracks the memory address of the SPR data file associated with the map location the player is currently in

;TILE ID number: a term used to describe the value stored in RMAP, from which other variables such as SMAP, and SMAP.CURRENT are dervied. The tile ID is a unique reference number to a specific tile on the map (from a human perspective). To the computer it is meaningful because the value of Tile ID is also equal to the quantity of tiles on the map in GMAP.TILE.DATA up to and including the tile assocaited with Tile ID. 

;-------this group must stay in this order-----
GMAP					.BS $2			;Tracks the position on the map in computer terms. Specifically, it is the tile ID number of the tile at the center of the screen where the player stands. 
GMAP.X					.BS $1		;Tracks player's x/y position on world map (whereas RMAP tracks position on the regional map which is 9 zones within the world map). Used to identify enterable location, and map edge detection. Future use. compass tracking, longitude. 
GMAP.Y					.BS $1		;""
GMAP.X.LAST				.BS $1		;Stores the GMAP X-axis of player prior to the execution of a movement command. Used to deal with differences in Mob and NPC sprite map tracking. 
GMAP.Y.LAST				.BS $1		;Stores the GMAP Y-axis of player prior to the execution of a movement command. Used to deal with differences in Mob and NPC sprite map tracking. 
RMAP.X					.BS $1			;Tracks player x/y axis on regional map. 
RMAP.Y					.BS $1			;Tracks player x/y axis on regional map. 
RMAP					.BS $2			;TRACKS PLAYERS POSITION IN THE REGIONAL MAP ARRAY
PLAYER.WMAP.ZONE		.BS $1			;PLAYERS CURRENT WORLD ZONE LOCATION
;-----------------


SMAP					.BS $2			;2byt
SMAP.CURRENT			.BS $2			;2byt
; RMAP					.EQ $AB40		;TRACKS PLAYERS POSITION IN THE REGIONAL MAP ARRAY
; RMAP.X					.EQ $AB42		;Tracks player x/y axis on regional map. 
; RMAP.Y					.EQ $AB43		;Tracks player x/y axis on regional map. 
; PLAYER.WMAP.ZONE		.EQ $AB44		;PLAYERS CURRENT WORLD ZONE LOCATION


;MAP MOVEMENT OFFSETS
OFFSET.UP				.EQ $30		;#CONSTANT
OFFSET.DOWN				.EQ $30		;#CONSTANT
OFFSET.HORIZONTAL		.EQ $01		;#CONSTANT


OFFSET.SCREEN			.EQ $F8	;#CONSTANT (offset from RMAP calculating RMAP of upper left screen tile)
OFFSET.SCREEN.LL		.EQ $E8	;#CONSTANT (offset from RMAP for calculating RMAP of lower left screen tile)
OFFSET.SCREEN.UR		.EQ $10 ;#CONSTANT (offset from SMAP for calculating RMAP of upper right screen tile)

;MAP CONVERSION TOOLS
TEMPX					.BS $1	;used for math calculations in CONVERT.xxx routines (see map_tools.asm) **SHARED**
TEMPY					.BS $1	;used for math calculations in CONVERT.xxx routines (see map_tools.asm) **SHARED**
TEMPY2					.BS $1	;used for math calculations in CONVERT.xxx routines (see map_tools.asm) **SHARED**


PARM.GMAP.X				.BS $1	;stores GMAP.X as a parameter for CONVERT.xxx routines (see map_tools.asm) **SHARED**
PARM1.GMAP.X			.EQ PARM.GMAP.X	;""
PARM2.GMAP.X			.EQ	TEMPX ;""
PARM.COLUMN				.EQ PARM.GMAP.X	;$1byte
PARM1.SHAPE.SBYTE		.EQ PARM1.GMAP.X
PARM2.SHAPE.SBYTE		.EQ PARM2.GMAP.X

PARM.GMAP.Y				.BS $1	;stores GMAP.Y as a parameter for CONVERT.xxx routines (see map_tools.asm)
PARM1.GMAP.Y			.EQ PARM.GMAP.Y	;""
PARM2.GMAP.Y			.EQ	TEMPY ;""
PARM.ROW				.EQ PARM.GMAP.Y ;$1byte
PARM1.SHAPE.LINE		.EQ PARM1.GMAP.Y
PARM2.SHAPE.LINE		.EQ PARM2.GMAP.Y

PARM.RMAP.X				.EQ PARM.GMAP.X	;stores RMAP.X as a parameter for CONVERT.xxx routines (see map_tools.asm)
PARM.RMAP.Y				.EQ PARM.GMAP.Y	;stores RMAP.Y as a parameter for CONVERT.xxx routines (see map_tools.asm)
PARM.WZONE				.BS $1	;stores PLAYER.WMAP.ZONE as a parameter for CONVERT.xxx routines (see map_tools.asm)
PARM.RELATIVE.X			.EQ PARM.GMAP.X
PARM.RELATIVE.Y			.EQ PARM.GMAP.Y

RETURN.GMAP.X			.BS $1	;stores return value for CONVERT.GMAP_XY.RMAP_XY
RETURN.GMAP.Y			.BS $1	;stores return value for CONVERT.GMAP_XY.RMAP_XY
RETURN.RMAP.X			.EQ RETURN.GMAP.X	;stores return value for CONVERT.xxx routines (see map_tools.asm)
RETURN.RMAP.Y			.EQ RETURN.GMAP.Y	;stores return value for CONVERT.xxx routines (see map_tools.asm)
RETURN.RELATIVE.X		.EQ PARM.GMAP.X		;""
RETURN.RELATIVE.Y		.EQ PARM.GMAP.Y		;""
RETURN.SCREEN_ARRAY_INDEX .EQ RETURN.GMAP.X ;1 byte
;RETURN.DISTANCE			.EQ RETURN.GMAP.X 	;""

RETURN.RMAP				.BS $2	;stores return value for CONVERT.RMAP_XY.RMAP
RETURN.WZONE			.EQ PARM.WZONE	;stores return value for CONVERT.GMAP_XY.WZONE	




;Multiplication table for calculating the north/south offset for multiple tiles	
RMAP.MULTIPLY_TABLE.LO	.HS	00.30.60.90.C0.F0.20.50.80.B0.E0.10.40.70.A0.D0.00.30.60.90.C0.F0.20.50.80.B0.E0.10.40.70.A0.D0.00.30.60.90.C0.F0.20.50.80.B0.E0.10.40.70.A0.D0
RMAP.MULTIPLY_TABLE.HO 	.HS	00.00.00.00.00.00.01.01.01.01.01.02.02.02.02.02.03.03.03.03.03.03.04.04.04.04.04.05.05.05.05.05.06.06.06.06.06.06.07.07.07.07.07.08.08.08.08.08

@END

;======MAP OBJECT ARRAYS=====
@START
;See beginning of this file

@END
		
;======MAP_OBJECT_MANAGEMENT======	
@START

SPRITE.RECORD						.EQ SPRITE.RECORD_STEP		;Hold the current map object record being processed by MO.DRAW
SPRITE.RECORD.SIZE					.EQ $C		;#CONSTANT. # of bytes in SPRITE.RECORD
GENERAL_MO.RECORD					.EQ SPRITE.RECORD ;same as SPRITE.RECORD but for general map objects


;MAP OBJECT CREATION
MAP_OBJECTS.CREATE.TRANSPORT.X				.EQ SCREEN.TILE.HOPPER+$A			;PARAMETER FOR MO.TRANSPORT.CREATE
MAP_OBJECTS.CREATE.TRANSPORT.Y				.EQ SCREEN.TILE.HOPPER+$B			;PARAMETER FOR MO.TRANSPORT.CREATE
MAP_OBJECTS.CREATE.TRANSPORT.TILE_TYPE		.EQ SCREEN.TILE.HOPPER+$C			;PARAMETER FOR MO.TRANSPORT.CREATE
MAP_OBJECTS.CREATE.TRANSPORT.SKIFFS			.EQ SCREEN.TILE.HOPPER+$D			;PARAMETER FOR MO.TRANSPORT.CREATE



;MAP/SCREEN LOCATION
MAP_OBJECTS.MAP_LOCATION	.BS $2		;STORES THE GMAP LOCATION A GIVEN MAP OBJECT
MAP_OBJECTS.TILE_LOCATION	.BS $1		;STORES THE SCREEN TILE # OF A GIVEN MAP OBJECT
MAP_OBJECTS.PLAYER_LOCATION .EQ $80		;#CONSTANT. THE X,Y LOCATION THAT REPRESENTS THE PLAYER AS IT RELATES TO MAP OBJECTS AND CALCULATING THEIR PROXIMITY TO THE PLAYER


;MAP/SCREEN OPERATIONS (GENERAL)
MANAGE_OBJECTS.NPC_MOB.INDEX.FLAG	.BS $01		;since the MOB and NPC map object arrays are not the same size, this flag tracks whether the index points to a MOB record only or a MOB & NPC Record. $00 = Mob & NPC, $01 = Mob only,
MANAGE_OBJECTS.NPC_MOB.RECORD.FLAG	.BS $01		;tracks whether the sprite object loop is in mob or NPC mode. ;$00=mob record, $01=npc record, >=$02=next map object record

GMAP.X.TO_CONVERT					.EQ TEMPX	;Used to hold the value for GMAP to player-relative.X/Y conversions because the value could be GMAP.X/Y or GMAP.X/Y.LAST
GMAP.Y.TO_CONVERT					.EQ TEMPY	;""
; CONVERT.SPRITE.GMAP.X				.EQ TEMP16 		;Used to hold the value of either a MOB or NPC's GMAP.X/Y during the conversion to player relative X/Y
; CONVERT.SPRITE.GMAP.Y				.EQ TEMP16+$1   ;""



MAP_OBJECTS.X.LAST_COLUMN 			.EQ $88		;#CONSTANT. STORES THE VALUE OF THE LAST COLUMN ON THE X-AXIS
MAP_OBJECTS.X.COMBAT.LAST_COLUMN 	.EQ $82		;#CONSTANT. STORES THE VALUE OF THE LAST COLUMN ON THE X-AXIS
MAP_OBJECTS.X.FIRST_COLUMN			.EQ $78		;#CONSTANT. STORES THE VALUE OF THE FIRST COLUMN ON THE X-AXIS
MAP_OBJECTS.Y.LAST_ROW 				.EQ $85		;#CONSTANT. STORES THE VALUE OF THE LAST ROW ON THE Y-AXIS
MAP_OBJECTS.Y.FIRST_ROW				.EQ $7B		;#CONSTANT. STORES THE VALUE OF THE 2ND ROW ON THE Y-AXIS

MAP_OBJECTS.X_APPROACH				.HS	76.77.89.8A	;X-AXIS APPROACH VALUES, FOR MOBS IN THE COLUMN/ROW ON THE EDGE OF VIEW SCREEN. FIRST COLUMN -1, -2, LAST COLUMN +1,+2. DOUBLE MOVERS ARE TAKEN INTO CONSIDERATION. 
MAP_OBJECTS.Y_APPROACH				.HS	79.7A.86.87	;Y-AXIS APPROACH VALUES, FOR MOBS IN THE COLUMN/ROW ON THE EDGE OF VIEW SCREEN. FIRST ROW -1,-2, LAST ROW +1,+2. DOUBLE MOVERS ARE TAKEN INTO CONSIDERATION. 

MOB.POSITION.X_GR		.EQ MOB.POSITION.X_GR_STEP ;($00 = false, $01 = true). based on the position of a mob (net of current player move, before mob moves). if != $00 then mob's x position is greater than player. 
MOB.POSITION.X_LT		.EQ MOB.POSITION.X_LT_STEP	;based on the position of a mob (net of current player move, before mob moves). if != $00 then mob's x position is less than player. 
MOB.POSITION.Y_GR		.EQ MOB.POSITION.Y_GR_STEP	;based on the position of a mob (net of current player move, before mob moves). if != $00 then mob's y position is greater than player.
MOB.POSITION.Y_LT		.EQ MOB.POSITION.Y_LT_STEP ;based on the position of a mob (net of current player move, before mob moves). if != $00 then mob's y position is less than player. 

MAP_OBJECTS.SS.X_FLAG.LOWER			.BS $1		;STORES THE FLOATING VALUES REPRESENTING THE EDGE OF THE REGIONAL MAP IN MOB X/Y TERMS, WHICH ADJUSTS AS THE PLAYER MOVES. 
MAP_OBJECTS.SS.X_FLAG.UPPER			.BS $1		;STORES THE FLOATING VALUES REPRESENTING THE EDGE OF THE REGIONAL MAP IN MOB X/Y TERMS, WHICH ADJUSTS AS THE PLAYER MOVES.
MAP_OBJECTS.SS.Y_FLAG.LOWER			.BS $1		;STORES THE FLOATING VALUES REPRESENTING THE EDGE OF THE REGIONAL MAP IN MOB X/Y TERMS, WHICH ADJUSTS AS THE PLAYER MOVES.
MAP_OBJECTS.SS.Y_FLAG.UPPER			.BS $1		;STORES THE FLOATING VALUES REPRESENTING THE EDGE OF THE REGIONAL MAP IN MOB X/Y TERMS, WHICH ADJUSTS AS THE PLAYER MOVES.



MAP_OBJECTS.SS.X_FLAG.LOWER.START	.EQ $67		;#CONSTANT. Marks the mob x/y left edge of the regional map when player is at the starting zone position. Use for SS in-region check.
MAP_OBJECTS.SS.X_FLAG.UPPER.START	.EQ $98		;#CONSTANT. Marks the mob x/y right edge of the regional map when player is at the starting zone position. Use for SS in-region check.
MAP_OBJECTS.SS.Y_FLAG.LOWER.START	.EQ $68		;#CONSTANT. Marks the mob x/y top edge of the regional map when player is at the starting zone position. Use for SS in-region check.
MAP_OBJECTS.SS.Y_FLAG.UPPER.START	.EQ $99		;#CONSTANT. Marks the mob x/y bottom edge of the regional map when player is at the starting zone position. Use for SS in-region check.

MOB.SCREEN_STATUS.START				.BS $1		;BEFORE THE EFFECT OF THE PLAYER MOVE IS APPLIED, IS THE CURRENT MOB RECORD ONSCREEN? $00=YES, $01=NO
MOB.SCREEN_STATUS.NPM				.BS $1		;AFTER THE EFFECT OF THE PLAYER MOVE IS APPLIED, IS THE CURRENT MOB RECORD ONSCREEN? $00=YES, $01=NO

MOB.SCREEN_STATUS.SS				.BS $1		;AFTER THE EFFECT OF THE PLAYER MOVE IS APPLIED, IF $01 THE CURRENT MOB RECORD BEING PROCESED FOR MOVEMENT HAS THE SS FLAG SET, AND THAT MOB IS CURRENTLY NOT LOCATED ON THE VIEW SCREEN


;MAP/SCREEN OPERATIONS (ONSCREEN CHECK)
MAP_OBJECTS.X_FLAG.LOWER	.EQ $78		;#CONSTANT. LEFT SIDE X BOUNDARY. See Screen Map, in map matrix worksheet, in Game Map spreadsheet
MAP_OBJECTS.X_FLAG.UPPER	.EQ $89		;#CONSTANT. RIGHT SIDE X BOUNDARY+1. See Screen Map, in map matrix worksheet, in Game Map spreadsheet
MAP_OBJECTS.Y_FLAG.LOWER	.EQ $7B		;#CONSTANT. UPPER SIDE Y BOUNDARY. See Screen Map, in map matrix worksheet, in Game Map spreadsheet
MAP_OBJECTS.Y_FLAG.UPPER	.EQ $86		;#CONSTANT. LOWER SIDE X BOUNDARY+1. See Screen Map, in map matrix worksheet, in Game Map spreadsheet
		
MOB.RZONE.POSITION			.EQ MOB.POSITION.X_GR	;1byt	Stores a value used to compare to the player RMAP.X/Y to determine if the SS Mob is location in the regional map

;MAP OBJECT DATA
MAP_OBJECTS.RECORD_LENGTH	.EQ	$04		;#CONSTANT. NUMBER OF FIELDS (BYTES) IN A MAP OBJECTS RECORD. IT'S A QTY FOR INCREMENTING SO $00 COUNTS AS 1. CURRENTLY IT'S THE SAME FOR BOTH ARRAYS (MAP_OBJECTS.GENERAL, MAP_OBJECTS.MOBS)										
MAP_OBJECTS.X_ADJ			.EQ MAP_OBJECTS.X_ADJ_STEP	;STORES THE ADJUSTMENT TO PLAYER LOCATION NEEDED TO CALCUALTE SCREEN TILE FOR A MAP OBJECT, BASED ON ITS X LOCATION.
MAP_OBJECTS.Y_ADJ			.EQ MAP_OBJECTS.Y_ADJ_STEP		;SAME CONCEPT AS X_ADJ, BUT NEEDS 2BYTES BECAUSE Y ADJUSTMENTS CAN BE MUCH LARGER. 


;MISC
SPRITE.DRAWTILE.OVERRIDE	.BS $1		;used to force a sprite tile to be drawn even if under certain conditional logic would other prevent it from being draw.
SPRITE.ERASETILE.OVERRIDE	.BS $1		;used to force a sprite tile to be drawn even if under certain conditional logic would other prevent it from being erased.

;FLAGS
MOB.FLAG0					.BS	$1
MOB.FLAG1					.BS	$1
MOB.FLAG2					.BS	$1
MOB.FLAG3					.BS	$1
MOB.FLAG4					.BS	$1
MOB.FLAG5					.BS	$1
MOB.FLAG6					.BS	$1
MOB.FLAG7					.BS	$1
SPRITE.FLAGS_BYTE3			.EQ	MOB.FLAG0	;USED WITH A LOOP FOR WRITING VALUES TO ALL FLAGS AT ONCE

GENERAL.FLAG0				.EQ MOB.FLAG0
GENERAL.FLAG1				.EQ MOB.FLAG1
GENERAL.FLAG2				.EQ MOB.FLAG2
GENERAL.FLAG3				.EQ MOB.FLAG3
GENERAL.FLAG4				.EQ MOB.FLAG4
GENERAL.FLAG5				.EQ MOB.FLAG5
GENERAL.FLAG6				.EQ MOB.FLAG6
GENERAL.FLAG7				.EQ MOB.FLAG7
MAP_OBJECTS.GENERAL_FLAGS	.EQ	MOB.FLAG0	;USED WITH A LOOP FOR WRITING VALUES TO ALL FLAGS AT ONCE

;FLAG VALUES
MAP_OBJECT.FLAG.MTT_AGGR	.EQ $09 ;#CONSTANT. Sets S_ENTITY to multi-tile, aggressive
MAP_OBJECT.FLAG.AGGR		.EQ	$01 ;#CONSTANT. Sets S_ENTITY to aggressive
@END

;=====MOB/TRANSPORT MOVEMENT=====
@START
MOB.MOVE.CURRENT			.BS $1		;IF A MOB DECIDED TO MOVE, THIS VARIABLE HOLDS THE DIRECTION CODE. SAME DIRECTION CODES AS FOR PLAYER MOVEMENT (SEE GAME_LOOP.ASM)
MOB.MOVE.COUNTER			.BS $1		;COUNTS MOVES FOR DOUBLE-MOVER MOBS
MOB.MOVE.ERASE_COUNTER		.BS $1		;RECORD WHEN ERASE TILE IS EXECUTED ON A GIVEN TURN FOR A MOB, SO THAT DOUBLE MOVER'S DON'T ERASE TWICE
MOB.MOVE.OPTIONS_PRIMARY	.BS $5		;SIZED TO $5 SO THE MOVE DECISION ARRAYS CAN BE INIT IN ONE LOOP
MOB.MOVE.OPTIONS_SECONDARY	.BS $5		;SIZED TO $5 SO THE MOVE DECISION ARRAYS CAN BE INIT IN ONE LOOP
MOB.MOVE.OPEN_PATHS			.BS $5		;SIZED TO $5 SO THE MOVE DECISION ARRAYS CAN BE INIT IN ONE LOOP

MOB.MOVE.TOTAL_OPEN_PATHS	.BS $1		;USED TO GUIDE THE PARSING OF THE RANDOM NUMBER RESULTS USED TO SELECT THE MOVE OPTION
MOB.MOVE.CANDIDATE			.BS $1		;USED TO STORE THE MOVE DIRECTION CODE OF THE MOVE THE MOB WANTS TO MAKE, AND WILL MAKE UNLESS BLOCKED.
MOB.MOVE.LAST				.BS	$1		;RECORDS THE LAST MOVE THE MOB MADE. IN THE INITIAL IMPLEMENTATION, IT WOULD NOT RECORD A PASS AS A MOVE, SO MOBS NEVER PASS TWICE
MOB.MOVE.SLOW.PROGRESS 		.EQ SCREEN.TILE.HOPPER+$9	;SET TO $01 IF MOB ENCOUNTERS SLOW PROGRESS ON A GIVEN MOVE. NEEDED FOR DOUBLE MOVERS, TO FORCE A TILE DRAW BECAUSE SLOW PROGRESS = PASS AND NORMALLY TILES AREN'T DRAWN ON A PASS. 


SPRITE.USE.ADHOC.PATHFINDER	.EQ $5		;Distance (in tiles) between player and sprite for when adhoc pathfinder algorithm is used to control sprite movement (if distance >= this constant, then use adhoc pathfinder)

;MOVEMENT COLLISION RULES
MOB.COLLISION_OVERRIDE		.BS $1		;$00 = OFF, $01 = ON
MOB.MOVES.BLOCKED			.BS $05		;STORES A VALUE INDICATING WHETHER EACH OF THE TILES ADJACENT TO THE MOB IS BLOCKED ($00 = PERMITTED, $01 BLOCKED)


	
MOB.MT.ADJACENT_TILES		.EQ SHARED.VARIABLE_SPACE.BLOCK3			;$8byt. Stores the screen tile # of all adjacent tiles to a MT MOB, organized into 4 directional groups of 4 tiles each. 
MOB.MT.NORTH_GROUP			.EQ SHARED.VARIABLE_SPACE.BLOCK3			;$2byt. Stores the screen tile # of the 4 tiles adjacent to the MT MOB in the direction noted. Used for collision controls.
MOB.MT.SOUTH_GROUP			.EQ SHARED.VARIABLE_SPACE.BLOCK3+$02		;$2byt. Stores the screen tile # of the 4 tiles adjacent to the MT MOB in the direction noted. Used for collision controls.
MOB.MT.EAST_GROUP			.EQ SHARED.VARIABLE_SPACE.BLOCK3+$04		;$2byt. Stores the screen tile # of the 4 tiles adjacent to the MT MOB in the direction noted. Used for collision controls.
MOB.MT.WEST_GROUP			.EQ SHARED.VARIABLE_SPACE.BLOCK3+$06		;$2byt. Stores the screen tile # of the 4 tiles adjacent to the MT MOB in the direction noted. Used for collision controls.

MOB.ADJACENT_TILES			.EQ SCREEN.TILE.HOPPER		;4byt. STORES THE TILE #'S OF THE TILES ADJACENT TO THE MOB FOR USE WITH COLLISION CHECKS. BORROWS THE MEMORY USED BY SCREEN.TILE.HOPPER 
;MOB.ADJACENT_TILES			.BS $4			;4byt. STORES THE TILE #'S OF THE TILES ADJACENT TO THE MOB FOR USE WITH COLLISION CHECKS. BORROWS THE MEMORY USED BY SCREEN.TILE.HOPPER 

MOB.MT.TILE_LOCATIONS		.EQ SCREEN.TILE.HOPPER+$4	;4byt. STORES THE SCREEN TILE # OF EACH TILE OF A MULTI-TILE MOB. BORROWS THE MEMORY USED BY SCREEN.TILE.HOPPER
MOB.MT.TILE_TYPES			.EQ SCREEN.DARK.HOPPER		;4byt. STORES THE TILE TYPES OF ALL 4 TILES IN A MT MOB. BORROWS MEMORY USED FOR SCREEN.DARK.HOPPER. 
MOB.MT.GROUP_COUNTER		.EQ SHP.TBL.CNTR			;1byt. COUNTER FOR THE #COLLISION CHECK ROUTINE FOR MULTI-TILE MOBS. BORROWS MEMORY. 
MOB.MT.POSITION.X			.EQ SCREEN.TILE.HOPPER+$8	;1byt. STORES THE X-AXIS OF TILE0 OF THE MT MOB. BORROWS MEMORY. 
MOB.MT.ADJACENT_TILES.PTR 	.EQ $FA						;STORES A POINTER TO THE DIRECTIONAL GROUP IN MOB.MT.ADJACENT_TILES FOR MULT-TILE MOBS COLLISION CONTROLS.

TRANSPORT.MT.TILE_LOCATIONS .EQ SCREEN.TILE.HOPPER+$4   ;SEE MOB.MT.TILE_LOCATIONS
TRANSPORT.MT.TILE_TYPES	 	.EQ SCREEN.DARK.HOPPER		;SEE MOB.MT.TILE_TYPES
TRANSPORT.MT.POSITION.X 	.EQ SCREEN.TILE.HOPPER+$8	;SEE MOB.MT.POSITION.X
;TRANSPORT.MT.ADJACENT_TILES	.EQ SHARED.VARIABLE_SPACE.BLOCK3			;$8byt. Stores the screen tile # of all adjacent tiles to a MT MOB, organized into 4 directional groups of 4 tiles each. 
; TRANSPORT.MT.GROUP_COUNTER	.EQ SHP.TBL.CNTR			;SEE MOB.MT.GROUP_COUNTER
; TRANSPORT.MT.ADJACENT_TILES.PTR 	.EQ $FA				;STORES A POINTER TO THE DIRECTIONAL GROUP IN MOB.MT.ADJACENT_TILES FOR MULT-TILE MOBS COLLISION CONTROLS.

TRANSPORT.MT.COLLISSION.HOPPER	.EQ SHARED.VARIABLE_SPACE.BLOCK3			;$2byt. Stores the screen tile # of the two tiles in the candidate move direction which need to be checked by collision controls.

; TRANSPORT.MT.NORTH_GROUP		.EQ SHARED.VARIABLE_SPACE.BLOCK3			;$2byt. Stores the screen tile # of the 4 tiles adjacent to the MT MOB in the direction noted. Used for collision controls.
; TRANSPORT.MT.SOUTH_GROUP		.EQ SHARED.VARIABLE_SPACE.BLOCK3+$02		;$2byt. Stores the screen tile # of the 4 tiles adjacent to the MT MOB in the direction noted. Used for collision controls.
; TRANSPORT.MT.EAST_GROUP			.EQ SHARED.VARIABLE_SPACE.BLOCK3+$04		;$2byt. Stores the screen tile # of the 4 tiles adjacent to the MT MOB in the direction noted. Used for collision controls.
; TRANSPORT.MT.WEST_GROUP			.EQ SHARED.VARIABLE_SPACE.BLOCK3+$06		;$2byt. Stores the screen tile # of the 4 tiles adjacent to the MT MOB in the direction noted. Used for collision controls.


COLLISION_FLAG.MOB_SEA.START	.EQ $F3		;FIRST TILE IN SEA MOB RANGE, USE TO DETERMINE IF THE CURRENT MOB OBJECT IS A LAND MOB OR SEA MOB
COLLISION_FLAG.MOB_LAND.GRE1.1	.EQ	$5D		;#CONSTANT
COLLISION_FLAG.MOB_LAND.LT1.1	.EQ	$80		;#CONSTANT
COLLISION_FLAG.MOB_LAND.LT1		.EQ	$33		;#CONSTANT
COLLISION_FLAG.MOB_LAND.GRE		.EQ	$80		;#CONSTANT
COLLISION_FLAG.MOB_LAND.LT2		.EQ	$8C		;#CONSTANT
COLLISION_FLAG.MOB_LAND.EQ1		.EQ	$46		;#CONSTANT

COLLISION_FLAG.MOB_CROC.GRE1	.EQ	$63		;#CONSTANT
COLLISION_FLAG.MOB_CROC.LT1		.EQ	$7B		;#CONSTANT
COLLISION_FLAG.MOB_CROC.EQ1		.EQ	$41		;#CONSTANT
COLLISION_FLAG.MOB_CROC.EQ2		.EQ	$88		;#CONSTANT
@END

;========MOB GENERATION=======
@START

GAME.MOB_GEN.CONTROL		.BS	$1		;STORES $00 FOR MOB GEN = OFF, HOLDS $01 FOR MOB GEN = ON
GAME.TURN.CONTROL			.BS	$1		;STORES $00 FOR PLAYER TURN, HOLDS $01 FOR MOB/NPC TURN

MOB.GEN.QUEUE				.BS $1		;WHEN MOB GENERATION IS ABORTED DUE TO PLAYER KEYPRESS, #$01 IS ADDED TO THIS VARIABLE SO THAT GENERATION CAN CATCHUP WHEN THE PROCESSOR IS FREE
;MOB.GEN.QUEUE				.EQ $910D	;WHEN MOB GENERATION IS ABORTED DUE TO PLAYER KEYPRESS, #$01 IS ADDED TO THIS VARIABLE SO THAT GENERATION CAN CATCHUP WHEN THE PROCESSOR IS FREE

MOB.GEN.SEA_FLAG			.BS $1		;RANDOM GENERATOR SELECTED A SEA MOB TYPE IF THIS FLAG IS SET TO #$01
MOB.GEN.PROBABILITY			.BS $1		;CHANCE THAT GAME WILL ATTEMPT TO GENERATE A MOB ON A GIVEN RUN OF MOB.GENERATION, PROVIDED THAT IT'S NOT PLAYERS TURN

;MOB.GEN.SS_QTY				.EQ SHARED.VARIABLE_SPACE.BLOCK3	;The current number of SS in the current region
MOB.GEN.SS_QTY				.BS $1		;The current number of off screen SS in the current region

MOB.GEN.SS_LIMIT			.EQ $10		;#CONSTANT. THE MAXIMUM NUMBER OF SS PERMITTED TO BE ACTIVE IN THE CURRENT REGION (NO NEW ONES GENERATED BEYOND THIS LIMIT)
;MOB.GEN.ARRAY_FULL_COUNTER	.EQ SHARED.VARIABLE_SPACE.BLOCK3+$01	;WHEN THE MAP OBJECTS/MOB ARRAY IS FULL, THIS COUNTER IS USED TO INCREMENT THE RECORD THAT WILL BE OVERWRITTEN, SO IT'S NOT ALWAYS THE SAME ONE.
MOB.GEN.ARRAY_FULL_COUNTER	.BS $1		;WHEN THE MAP OBJECTS/MOB ARRAY IS FULL, THIS COUNTER IS USED TO INCREMENT THE RECORD THAT WILL BE OVERWRITTEN, SO IT'S NOT ALWAYS THE SAME ONE.


MOB.CANDIDATE.RMAP			.BS $2		;STORES THE RANDOM NUMBER GENERATED FOR A NEW MOB'S LOCATION, BEFORE THE LOCATION HAS BEEN CHECKED FOR COLLISION
;MOB.CANDIDATE.GMAP			.EQ $9102		;STORES THE RANDOM NUMBER GENERATED FOR A NEW MOB'S LOCATION, BEFORE THE LOCATION HAS BEEN CHECKED FOR COLLISION

MOB.CANDIDATE.TYPE			.BS $1		;STORES THE TILE_TYPE VALUE (BYTE2 OF THE MOB RECORD) FOR A NEW MOB BEING CREATED
MOB.CANDIDATE.FLAGS			.BS $1		;STORES THE FLAG VALUE (BYTE3 OF THE MOB RECORD) FOR A NEW MOB BEING CREATED

MOB.GEN.RANDOM_LO.START		.EQ	$01		;#CONSTANT. USED FOR RANDOM NUMBER GENERATION. CORRESPONDS TO THE LO BYTE OF THE SCREEN ARRAY
MOB.GEN.RANDOM_LO.END		.EQ	$FF		;#CONSTANT. USED FOR RANDOM NUMBER GENERATION. CORRESPONDS TO THE LO BYTE OF THE SCREEN ARRAY
MOB.GEN.RANDOM_HO.START		.EQ	$00		;#CONSTANT. USED FOR RANDOM NUMBER GENERATION. CORRESPONDS TO THE LO BYTE OF THE SCREEN ARRAY
MOB.GEN.RANDOM_HO.END		.EQ	$08		;#CONSTANT. USED FOR RANDOM NUMBER GENERATION. CORRESPONDS TO THE LO BYTE OF THE SCREEN ARRAY

MOB.CANDIDATE.MAP.X			.BS $1		;STORES CANDIDATE MOB'S MAP X-AXIS LOCATION. DIFFERENT THAN THE X,Y USED TO STORE MOB'S RELATIVE POSITION TO PLAYER
MOB.CANDIDATE.MAP.Y			.BS $1		;STORES CANDIDATE MOB'S MAP X-AXIS LOCATION. DIFFERENT THAN THE X,Y USED TO STORE MOB'S RELATIVE POSITION TO PLAYER
MOB.CANDIDATE.RELATIVE.X	.BS $1		;STORES CANDIDATE MOB'S RELATIVE (TO PLAYER) X-AXIS LOCATION. 
MOB.CANDIDATE.RELATIVE.Y	.BS $1		;STORES CANDIDATE MOB'S RELATIVE (TO PLAYER) Y-AXIS LOCATION.

; MOB.CANDIDATE.MAP.X			.EQ $9106		;STORES CANDIDATE MOB'S MAP X-AXIS LOCATION. DIFFERENT THAN THE X,Y USED TO STORE MOB'S RELATIVE POSITION TO PLAYER
; MOB.CANDIDATE.MAP.Y			.EQ $9107	;STORES CANDIDATE MOB'S MAP X-AXIS LOCATION. DIFFERENT THAN THE X,Y USED TO STORE MOB'S RELATIVE POSITION TO PLAYER
; MOB.CANDIDATE.RELATIVE.X	.EQ $9108	;STORES CANDIDATE MOB'S RELATIVE (TO PLAYER) X-AXIS LOCATION. 
; MOB.CANDIDATE.RELATIVE.Y	.EQ $9109	;STORES CANDIDATE MOB'S RELATIVE (TO PLAYER) Y-AXIS LOCATION.


;GENERATION FLAGS (COLLISION AND DETERMINE IF SEA MOB)		
MOB.GEN_FLAG.GRE				.EQ $88		;#CONSTANT
MOB.GEN_FLAG.LT2				.EQ $8C		;#CONSTANT
MOB.GEN_FLAG.LT1				.EQ $33		;#CONSTANT
@END

;========NPC MOVEMENT/SCHEDULES=====
@START


;===========NPC.PATHFINDER=========
PATHFINDER.SPRITE.RECORD			.BS $0C		;Holds the information on the sprite for which pathfinder is generating a path. 

NPC.PATHFINDER.ABORT_FLAG			.BS $01 	;$00 = No abort, $01 = aborted path is pending completion
KEYPRESS.ABORT.ITERATIONS.DEFAULT	.EQ $03		;#CONSTANT. required iterations before auto-abort 
;set to $FF to disable auto abort. normally default value is $03
KEYPRESS.ABORT.ITERATIONS			.BS $01		;usually set to the default value (see above constant). Or set to $FF to disable auto abort.


NPC.PATHFINDER.DIRECTIONS_CHECKED	.BS $4		;tracks which directions, from the current tile, have been checked for open paths.
NPC.PATHFINDER.TILE_NUMBER.TALLY	.BS $1		;the next tile number available to be assigned by the pathfinder subroutine, for use in the NPC.PATHFINDER.SEARCH.PATHS array
NPC.PATHFINDER.CURRENT.TILE			.BS $1		;the tile number, unique to the pathfinder subroutine, associated with the current tile used as the center of the search pattern. 
NPC.PATHFINDER.CURRENT.TILE.X		.BS $1		;the RMAP.X and RMAP.Y axis of the current tile number are recorded in the NPC.PATHFINDER.SEARCH.PATHS array as the source x,y for all neigbor tiles. 
NPC.PATHFINDER.CURRENT.TILE.Y		.BS $1		;""
NPC.PATHFINDER.CURRENT.DISTANCE		.BS $1		;distance from the current tile to the destination. 
NPC.PATHFINDER.CURRENT.RMAP			.BS $1		;the RMAP associated with the current tile used as the center of the search pattern. 
NPC.PATHFINDER.NEIGHBOR.X			.BS $1		;X-axis of the most recetly acquired neighbor tile.
NPC.PATHFINDER.NEIGHBOR.Y			.BS $1		;Y-axis of the most recetly acquired neighbor tile.
NPC.PATHFINDER.NEIGHBOR.RMAP		.BS $2		;RMAP of the most recetly acquired neighbor tile.



NPC.PATHFINDER.PRIORITY.QUE			.EQ SWAP_SPACE.MAIN_MEMORY+$1300	;$100bytes stores the order in which the tile_numbers discovered are applied to the search pattern to discover new neigbor tiles.	
									;$A900
									
NPC.PATHFINDER.PRIORITY.QUE.SIZE			.EQ NPC.PATHFINDER.PRIORITY.QUE		;the first byte of the priority que holds the # of records in the array, which is incremented each time a record is added and decremented each time a record is deleted. 
NPC.PATHFINDER.PRIORITY.QUE.RECORD_TALLY	.BS $1	;number of 2byte records in the priority que, including the 2 byte length header. For this reason, it is equal to the number of records in the que +1
NPC.PATHFINDER.PRIORITY.QUE.RECORD_INDEX	.BS $1  ;derived from NPC.PATHFINDER.PRIORITY.QUE.RECORD_TALLY. Used to toggle between writing new records after last record in que or overwriting last record in que. 
NPC.PATHFINDER.PRIORITY.QUE.PROCESSED.RECORDS	.BS $1	;the number of processed records which are included in NPC.PATHFINDER.PRIORITY.QUE.SIZE. 
PATHFINDER.STREET.PREFERENCE					.BS $1  ;PARAMETER. The value to add to the distance of non-street tiles in order to help pathfinder generate a path that uses streets when available.  


;----------------------------------
;*****MUST START ON PAGE BOUNDARY
NPC.PATHFINDER.SEARCH.PATHS			.EQ SWAP_SPACE.MAIN_MEMORY+$1400	;16-BIT ($19Ebytes) Built by the pathfinder subroutine to contain the path from the NPCs current location (when the algorithm is run) and the destination location that the NPC is trying to travel to. 
									;$AA00

	;----------------------------------
									
SEARCH.PATHS.NEIGHBORS_ADDED.COUNTER	.BS $1		;stores the number of neighbors added each iteration so that it is detectable when no neighbors are added.
SEARCH.PATHS.RECORD.SIZE			.EQ $08		;#CONSTANT. the number of bytes in each record stored in NPC.PATHFINDER.SEARCH.PATHS
SEARCH.PATHS.LO_INDEX				.BS $1 		;used to track the lo byte index when iterating NPC.PATHFINDER.SEARCH.PATHS
SEARCH.PATHS.HIMEM					.EQ SWAP_SPACE.MAIN_MEMORY+$1EFF	;the upper limit of the memory reserved for the NPC.PATHFINDER.SEARCH.PATHS array 
									;$B4FF
NPC.PATHFINDER.SEARCH.PATHS.POINTER		.EQ $EC		;2byt. Used by .open.paths to load data into NPC.PATHFINDER.SEARCH.PATHS, when an open path is found for a given tile.
NPC.PATHFINDER.SEARCH.PATHS.POINTER2	.EQ $FC		;2byt. Used by .LOAD.NEXT.TILE to lookup data from NPC.PATHFINDER.SEARCH.PATHS, to load the next path tile.
SEARCH.PATHS.POINTER.SAVED				.BS $01		;This pointer needs to be persistent through an entire instance of pathfinder. This variable saves the value of the pointer when a keypress abort occurs so it can be reloaded upon re-entry.

NPC.PATHFINDER.FINAL.PATH			.EQ SWAP_SPACE.MAIN_MEMORY+$2300	;stores the final path, which is derviced from the data in NPC.PATHFINDER.SEARCH.PATHS after the algorith completes.  
									;$B900
FINAL.PATH.HIMEM					.EQ SWAP_SPACE.MAIN_MEMORY+$23FF	;the upper limit of the memory reserved for the NPC.PATHFINDER.FINAL.PATH array 
									;$B9FF
FINAL.PATH.RECORD.COUNTER			.BS $1		;counts records written to NPC.PATHFINDER.FINAL.PATH so that the last record's index can be calculated. The last record will be the next move the NPC makes to reach destination. 
FINAL.PATH.INDEX					.EQ FINAL.PATH.RECORD.COUNTER ;used in NPC.MOVE_MANGER to point to the next move in an NPC's existing path

NPC.PATHFINDER.DESTINATION.TILE.X	.BS $1		;the RMAP.X and RMAP.Y axis of the tile which NPC wants to move to. 
NPC.PATHFINDER.DESTINATION.TILE.Y	.BS $1		;""
PATH_TILE.SHORTEST.DISTANCE			.BS $1		;the path tile # that has the lowest distance to destination. Used by the algorithm to prioritize the search. 
PATHFINDER.SHORTEST.DISTANCE.COUNTER .BS $1		;used to track when a neighbor is found which is closer to the destination than the previous closest tile found. If at least one such neighbor is found in a given iteration, no sort is performed.
NPC.PATHFINDER.NEXT.SOURCE_TILE		.BS $1		;when assembling the final path, this is the next source path tile #, working backwards from destination to NPC's current position.


PATHFINDER.SWAP.DISTANCE			.BS $1		;used for temp data storage when moving records around in the priority que. 
PATHFINDER.SWAP.TILE_NUMBER			.BS $1		;""


ACQUIRE.LOOP.COUNTER				.BS $1 		;counter for the loop which uses a T shaped search pattern to acquire new neighbors
ITERATION.COUNTER					.BS $1
;ABORT.COUNTER						.BS $1		;Tracks the number of iterations 
NPC.MOVE.COUNTER					.BS $1 ;***TEMP
DIRECTION.TEST.COUNTER				.BS $1		;incremented each time a direction, for a given tile, is tested for open paths. When this counter == $04 then all directions have been tested.



;TEMP FOR TESTING. REMOVE
;GAME_LOOP.ITERATION.COUNTER			.BS $1
;SCHEDULER.ITERATION.COUNTER			.BS $1

NPC.PATHFINDER.CLOSED_SET			.EQ SWAP_SPACE.MAIN_MEMORY+$1F00	;$03C1 bytes. tracks tiles which have been examined by pathfinder already
									;$B500
NPC.PATHFINDER.CLOSED_SET.POINTER	.EQ $EA		;2byt
CLOSED_SET.OFFSET					.BS $20		;number of bytes per row in the closet set array 
NEIGHBOR.ADJUSTED.X					.EQ MAP_OBJECTS.X_ADJ ;stores the x-axis of a prospective neighbor, adjusted to align with the closed_set array memory map. 
NEIGHBOR.ADJUSTED.Y					.EQ MAP_OBJECTS.Y_ADJ ;"" (y-axis)
CURRENT.TILE.ADJUSTED.X				.EQ NEIGHBOR.ADJUSTED.X
CURRENT.TILE.ADJUSTED.Y				.EQ NEIGHBOR.ADJUSTED.Y
CLOSED_SET.X.ADJ					.EQ $08		;#CONSTANT
CLOSED_SET.Y.ADJ					.EQ $07		;#CONSTANT


;corresponding !y-axis				00.01.02.03.04.05.06.07.08.09.10.11.12.13.14.15.16.17.18.19.20.21.22.23.24.25.26.27.28.29.30.31
CLOSED_SET.MULTIPLY_TABLE.LO	.HS 00.20.40.60.80.A0.C0.E0.00.20.40.60.80.A0.C0.E0.00.20.40.60.80.A0.C0.E0.00.20.40.60.80.A0.C0.E0
CLOSED_SET.MULTIPLY_TABLE.HO	.HS 00.00.00.00.00.00.00.00.01.01.01.01.01.01.01.01.02.02.02.02.02.02.02.02.03.03.03.03.03.03.03.03

;SORTING VARIABLES	
; BUBBLE.SORT.LIST.SIZE				.BS $1
; BUBBLE.SORT.LIST.POINTER			.EQ $EA
; BUBBLE.SORT.LIST.EXCHANGE_FLAG 		.BS $1

;INDEX SORT

; h_low           .HS 02.08.18.50.F2.D8.8A.A0.E2
; h_high          .HS 00.00.00.00.00.00.08.19.4C


; h_start_index   .BS $1
; h_index         .BS $1
; h               .BS $2
; in_address      .BS $2
; arr_start       .BS $2
; arr_end         .BS $2
; i               .BS $2
; v               .BS $2
; v_plus_1        .BS $2


; J			.EQ $fa                   ; Uses two bytes. Has to be on zero-page
; j_plus_h	.EQ $fc                   ; Uses two bytes. Has to be on zero-page
; arr_length	.EQ j_plus_h              ; Can safely use the same location as
                                      ; ; j_plus_h, but doesn't have to be on ZP

									  
									  
;==============NPC.PATHGENERATOR===========

									  
NPC.PATHGENERATOR.SAVED_PATHS.AUX	.EQ $5C00	;#CONSTANT. Start address of the AUX memory reserved for saved NPC paths.						  
;SAVED.PATH.HO_ADDRESS.OFFSET	.BS $1		;the offset stored in this variable plus #SAVED.PATH.AUX_START equals the start memory address for the path for a specific NPC

NPC.PATHFINDER.FINAL.PATH.POINTER	.EQ NPC.PATHFINDER.SEARCH.PATHS.POINTER2
NPC.SCHEDULE.POINTER				.EQ $EA	

;SAVED.PATH.MAXIMUM: SAVED.PATH.LOOKUP.TABLE & NPC.PATHGENERATOR.SAVED_PATHS.AUX must have a large enough memory reservation to accomidate the number of paths specified by the constant #SAVED.PATH.MAXIMUM
SAVED.PATH.LOOKUP.TABLE				.EQ SHARED.VARIABLE_SPACE.BLOCK1+$0 ;$40byt. 
SAVED.PATH.LOOKUP.TABLE.SIZE		.EQ $40	;#CONSTANT. number of bytes in lookup table and NPC.PATHGENERATOR.SAVED_PATHS.AUX has memory for. 
SAVED.PATH.LOOKUP.TABLE.LAST_RECORD_START .EQ $3C ;#CONSTANT. # of bytes in Que - record size (6). (For a $40 byte array, the last record start is $3C, not $3D, because $0 is the first byte)
SAVED.PATH.LOOKUP.TABLE.INDEX		.EQ SHARED.VARIABLE_SPACE.BLOCK1+$7C	;$1byt. Stores the index to the last empty record found when iterating the array	
SAVED.PATH.MAXIMUM					.EQ $0F ;#CONSTANT. Max number of saved paths -1  that the lookup table and the
SAVED.PATH.COUNTER					.EQ SHARED.VARIABLE_SPACE.BLOCK1+$7D	;$1byt. Counts iterations of the SAVED.PATH array	
SAVED.PATH.NUMBER					.EQ SHARED.VARIABLE_SPACE.BLOCK1+$7E	;$1byt. Value updated from SAVED.PATH.COUNTER each time an empty record is found. Used to calculate the HO address offset when saving to aux memory
					

;NPC.PATHGENERATOR.QUE				.BS $40	;$3F byt. Stores the anchor locations which will need paths soon
NPC.PATHGENERATOR.QUE				.EQ SHARED.VARIABLE_SPACE.BLOCK1+$40	;$3F byt. Stores the anchor locations which will need paths soon
NPC.PATHGENERATOR.QUE.SIZE			.EQ $3C ;#CONSTANT.
NPC.PATHGENERATOR.QUE.LAST_RECORD_START	.EQ $36	;#CONSTANT. # of bytes in Que - record size (6). (For a $3C byte array, the last record start is $36, not $37, because $0 is the first byte)
PATHGENERATOR.QUE.INDEX				.EQ SHARED.VARIABLE_SPACE.BLOCK1+$7F	;$1byt. Stores the index to the last empty record found when iterating the array


PATHGENERATOR.LOOK_AHEAD.DAY_TRANSITION.HOUR	.EQ $19	;!BCD! #CONSTANT. This is the hour after which a special routine is needed to detrmine whether a scheduler record occurs before or after the current time. (i.e. if the look ahead period is 3 hours, and it is 23:00, then 01:00 is within the look ahead period even though 01 is less than 23. For this reason a special routine is required to evalute whether the scheduler entry is in the past or in the future) 	
PATHGENERATOR.LOOK_AHEAD.HOURS					.EQ $02	;!BCD! #CONSTANT. The number of hours +0, ahead which NPC.GENERATOR will generate paths for anchors in the scheduler arrray. If this value is changed PATHGENERATOR.LOOK_AHEAD.HOURS must be changed too. For example, if the time is 1:00am and the look ahead hours is set to 1, then any schedule entries with a time of 2:59am or sooner is within the lookahead period. Only hours are considered in the calculation (schedule entry hour - current hour), minutes are ignored. Note that the minimum value is $02, which means 1 hour, 59 minutes. 

NPC.MAXIMUM.ANCHORS					.EQ $A0	;#CONSTANT. Maximum number of NPCs anchor locations per building map.

;the following must have as much memory allocated as the value of NPC.MAXIMUM.ANCHORS 
;NPC.ANCHORS.X						.EQ SHARED.VARIABLE_SPACE.BLOCK1+$C0	;$A0 byts. Flows over into SHARED.VARIABLE_SPACE.BLOCK2. X-axis of all Anchor locations for the building the player is currently in, if any.
;NPC.ANCHORS.Y						.EQ SHARED.VARIABLE_SPACE.BLOCK2+$60		;$A0 byt. Y-axis of all Anchor locations for the building the player is currently in, if any.
NPC.ANCHORS.X						.BS NPC.MAXIMUM.ANCHORS	
NPC.ANCHORS.Y						.BS NPC.MAXIMUM.ANCHORS	



;======NPC TRANSIT & ANCHOR MOVEMENT=======
TRANSIT.NEXT_MOVE.INDEX				.EQ SHARED.VARIABLE_SPACE.BLOCK1+$80	;$1byt. the index to byte in the saved path which holds the next move for a particular NPC in transit to another anchor location			
TRANSIT.NEXT_MOVE.X					.EQ SHARED.VARIABLE_SPACE.BLOCK1+$81	;$1byt. the X-axis of the next move for an NPC in transit to another anchor location
TRANSIT.NEXT_MOVE.Y					.EQ SHARED.VARIABLE_SPACE.BLOCK1+$82	;$1byt. the X-axis of the next move for an NPC in transit to another anchor location

DESTINATION_REACHED_WHILE_SEEKING	.BS $1	;If an NPC is seeking due to a blocked path, and the seek tile is the destination tile of the path, this flag is used to indicate that. $01 = on, $00 = off. 


PLAYER.BLOCKED.NPC.COUNTER			.EQ SHARED.VARIABLE_SPACE.BLOCK1+$83	;$1byt. A tally of the number of times the player was in the path of an NPC. Resets after threshold is reached. Used in PLAYER.HARASSMENT.CHECK

;PLAYER.BLOCKED.NPC.THRESHOLD		.EQ $03	;#CONSTANT. Number of times a player must be in the path of an NPC before the NPC will react verbally. Used in PLAYER.HARASSMENT.CHECK	
PLAYER.BLOCKED.NPC.THRESHOLD		.EQ $03	;#CONSTANT. Number of times a player must be in the path of an NPC before the NPC will react verbally. Used in PLAYER.HARASSMENT.CHECK	
NPC.ASSIGNED_PATHS 					.EQ MOB.MOVE.OPEN_PATHS	;stores the paths that have already been assigned as primary move options in PRIORITIZE.NPC.PATHS

NPC.ACTIVE.ANCHOR.X					.EQ SHARED.VARIABLE_SPACE.BLOCK1+$84	;$1byt. Used anytime the X,Y of an NPC's active anchor need to be saved temporarily, such as in FLOCKING_POINT.CHECK
NPC.ACTIVE.ANCHOR.Y					.EQ SHARED.VARIABLE_SPACE.BLOCK1+$85	;$1byt. Used anytime the X,Y of an NPC's active anchor need to be saved temporarily, such as in FLOCKING_POINT.CHECK


DISTANCE.NORTH.OF_FLOCKING_POINT	.EQ MAP_OBJECTS.Y_ADJ	;$1byt.	
DISTANCE.SOUTH.OF_FLOCKING_POINT	.EQ MOB.POSITION.Y_GR_STEP	;$1byt.
DISTANCE.EAST.OF_FLOCKING_POINT		.EQ MAP_OBJECTS.X_ADJ	;$1byt.
DISTANCE.WEST.OF_FLOCKING_POINT		.EQ MOB.POSITION.X_GR_STEP	;$1byt.

; DISTANCE.NORTH.OF_FLOCKING_POINT	.BS $1	;$1byt.	
; DISTANCE.SOUTH.OF_FLOCKING_POINT	.BS $1	;$1byt.
; DISTANCE.EAST.OF_FLOCKING_POINT		.BS $1	;$1byt.
; DISTANCE.WEST.OF_FLOCKING_POINT		.BS $1	;$1byt.

NPC.SPECIAL_FLAG01.FORCED_PASS		.EQ $80	;#CONSTANT. 50% pass. An in-transit NPC will pass if random # is less than this value. 
NPC.SPECIAL_FLAG02.FORCED_PASS		.EQ $40	;#CONSTANT. 75% pass. An in-transit NPC will pass if random # is less than this value. 
PASS.PROBABILITY					.EQ SHARED.VARIABLE_SPACE.BLOCK1+$86	;$1byt.

;****DELETE THIS***
;TEMP.ARRAY							.EQ $A500
	

NPC.FLOCKING_POINT.RADIUS			.EQ SHARED.VARIABLE_SPACE.BLOCK1+$87	;1byte. Stores # of moves NPC is permitted to travel from flocking point in any direction.
NPC.FLOCKING_POINT.RADIUS1			.EQ $01	;#CONSTANT. Sets the radius to $01 tiles. 	
NPC.FLOCKING_POINT.RADIUS2			.EQ $02	;#CONSTANT. Sets the radius to $02 tiles. 	
NPC.FLOCKING_POINT.RADIUS3			.EQ $03	;#CONSTANT. Sets the radius to $03 tiles. 	
NPC.FLOCKING_POINT.RADIUS4			.EQ $04	;#CONSTANT. Sets the radius to $04 tiles. 	


;==============NPC.INIT===========	


MAXIMUM.NPCS.PER.BUILDING	.EQ $20 ;#CONSTANT. The maximum number of NPCs which can exist in on a building map and thus the max NPC records in a talk data file.
NPC.RECORD.SIZE				.EQ $08 ;#CONSTANT. number of bytes in NPC map object record
MAXIMUM.NPC.RECORD_NUMBER	.EQ MAXIMUM.NPCS.PER.BUILDING-1*NPC.RECORD.SIZE ;#CONSTANT. (currently should be $F8) The highest value that an NPC record # can be, which is equal to the maxium number of (NPCs per building-1) * the NPC record size. The -1 is because the max # of NPCs is a quantity and the first NPC record # starts with $00.


NPC.SCHEDULE.WORKSPACE				.EQ SHARED.VARIABLE_SPACE.BLOCK2 ;$C0 bytes. A temporary table created when figuring out which scheduler record to use for the NPCs init values. 	
NPC.SCHEDULE.WORKSPACE.SIZE			.EQ $FF

;NPC.INIT.ITERATION_COUNTER	.EQ SAVED.ACC.LOCAL ;used by NPC.INIT.NPC_RECORDS to detect if the iteration is after the first in order to detect X-REG = flipped to $00 as the exit value. This requires an interation counter because the X-REG increment is at the top of the loop.

;===========ADHOC PATHFINDER======
NPC.PATHFINDER.ADOC.FLAG			.BS $01	;1byte. $FF = on, $00 = off. Used to let NPC.PATHFINDER subroutine know that it is being used for an adhoc generated path rather than for NPC scheduled paths

	
@END

;========NPC.TALK (DISK MODULE)=====
@START



NPC.TALK.RECORD				.EQ SHARED.VARIABLE_SPACE.BLOCK2+$0	;1byt. Holds the map objects array record index for the NPC which the player is talking to
NPC.TALK.RECORD.INDEX		.EQ SHARED.VARIABLE_SPACE.BLOCK2+$1	;2byt. Holds the HO/LO byte index to the start of the current NPC record in the NPC.TALK.ARRAY
NPC.TALK.SCREEN_LOCATION	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$3 ;$1byts. The screen tile location of the NPC the player is talking to. 

NPC.TALK.TEXT.INDEX		.EQ $D0	;Index to a block of text which NPC.TALK requests for display on the video screen. 	

NPC.TALK.ARRAY			.EQ SRTN.NPC.TALK.ADDRESS-$E00	;#CONSTANT. memory address where unpacked data is loaded for NPC player is talking to.  As long as NPC.TALK (SRTN.NPC.TALK.ADDRESS) is loaded to $AC00, then this constant points to $A000.
						;$A000
NPC.TALK.ARRAY.SIZE		.EQ $900						;#CONSTANT.
NPC.TALK.ARRAY.END		.EQ SRTN.NPC.TALK.ADDRESS-$501	;#CONSTANT. memory address where unpacked data end.
						;$A8FF
NPC.TEXT.BLOCK.STOP_VALUE .EQ $00 ;#CONSTANT. This is the stop value added to ASCII tables by the SBASM cross assembler, which is used to create all NPC talk data files. 

NPC.TALK.ARRAY.PACKED	.EQ SRTN.NPC.TALK.ADDRESS-$500	;#CONSTANT. memory address where packed data is loaded for NPC player is talking to. As long as NPC.TALK (SRTN.NPC.TALK.ADDRESS) is loaded to $AC00, then this constant points to $B900.
						;$A900
NPC.TALK.ARRAY.PACKED.SIZE .EQ $500 ;#CONSTANT.
NPC.TALK.ARRAY.PACKED.END .EQ SRTN.NPC.TALK.ADDRESS-$1	;#CONSTANT. memory address where unpacked data end. As long as NPC.TALK (SRTN.NPC.TALK.ADDRESS) is loaded to $AC00, then this constant points to $BEFF.
						;$ADFF

NPC.TALK.KEYWORD		.EQ SHARED.VARIABLE_SPACE.BLOCK2+$4  ;$20byts. Holds the string input from player when talking to an NPC
NPC.TALK.KEYWORD.COPY	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$24 ;$20byts. Holds a copy of NPC.TALK.KEYWORD
NPC.TALK.KEYWORD.STOP_VALUE	.EQ $00 ;#CONSTANT.
;(see GENERAL ROUTINES (.TEXT)) CURSOR.POSITION.SAVED		.BS $2 ;$2byts. Holds saved cursor position when flipping between the NPC.TALK.INPUT subroutine (get player's question) and the PRINT.TEXT.WINDOW subroutine to output NPC text to the screen

;****SHARED.VARIABLE_SPACE.BLOCK2+$44 - $45 are open

;NTALK PROGRAMMING LANGUAGE
NTALK.COMMAND.START_CODE		.EQ $A4 ;#CONSTANT. The ASCII value used to indicate the start of an NTALK command
NTALK.COMMAND.FIND_RECORD		.EQ $D2 ;#CONSTANT. The ASCII value used to indicate the command to find an NPC Record value. 
NTALK.COMMAND.FIND_INTRO		.EQ $C9 ;#CONSTANT. The ASCII value used to indicate the command to find the introduction text block
NTALK.COMMAND.FIND_KEYWORD		.EQ $A1 ;#CONSTANT. The ASCII value used to indicate the command to find the specified keyword 
NTALK.COMMAND.END_CODE			.EQ $AE ;#CONSTANT. The ASCII value used to indicate the end of an NTALK command
NTALK.COMMAND.SUBCOMMAND_CODE 	.EQ $BA ;#CONSTANT. The ASCII value used to indicate the end of an NTALK command
NTALK.SUB_COMMAND.EVENT_CONT_CODE	.EQ $C5 ;#CONSTANT. The ASCII value used to indicate the contingent event subcommand, which makes the current command only available based on the value of the event flag # specified in the next byte. The subsequent byte contains the on/off status of the flag required for the command to be eligible. 
NTALK.SUB_COMMAND.EVENT_PUSH_CODE	.EQ $D0 ;#CONSTANT. The ASCII value used to indicate the push event subcommand, which sets the value of the specified event flag (in the next byte) after the current command is executed. The subsequent byte contains the on/off status of the flag that will be pushed 
NTALK.SUB_COMMAND.YELL_CODE		.EQ $D9 ;#CONSTANT. The ASCII value used to indicate the yell subcommand, which makes the current command contingent on the player being in yell mode. 
NTALK.SUB_COMMAND.WHISPER_CODE	.EQ $D7 ;#CONSTANT. The ASCII value used to indicate the whisper subcommand, which makes the current command contingent on the player being in whisper mode. 
NTALK.SUB_COMMAND.NORMAL_CODE	.EQ $CE ;#CONSTANT. The ASCII value used to indicate the normal subcommand, which makes the current command contingent on the player being in normal mode. 
NTALK.SUB_COMMAND.ADULT_CODE	.EQ $C1 ;#CONSTANT. The ASCII value used to indicate the adult_mode subcommand, which makes the current command contingent on the adult game mode being active. 
NTALK.SUB_COMMAND.NPC_TERM_CODE .EQ $D4 ;#CONSTANT. The ASCII value used to indicate the NPC has a % chance of terminating the conversation, and telling the player to stick it. Or something like that. 
NTALK.SUB_COMMAND.HOUR_CONT_CODE .EQ $C8 ;#CONSTANT. The ASCII value used to indicate the text block is contingent on the hour of the day. 
NTALK.SUB_COMMAND.KEYWORD_INSERT_CODE	.EQ $CB ;#CONSTANT. The ASCII value used to indicate the insert keyword subcommand, which inserts the player input keyword into the text block at the location of the subcommand.  

;NTALK.SEARCH
NTALK.VOICE_MODE			.EQ SHARED.VARIABLE_SPACE.BLOCK2+$46 ;$1byts. current voice mode when talking to NPCs ($00 = normal, $01 = yell, $02 = whisper)	
NTALK.VOICE_MODE.MAX		.EQ $03 ;#CONSTANT. The quant0ity of voice modes. 
NTALK.VOICE.NORMAL			.EQ $00	;#CONSTANT. The voice mode value when normal mode is enabled
NTALK.VOICE.YELL			.EQ $01	;#CONSTANT. The voice mode value when yell mode is enabled
NTALK.VOICE.WHISPER			.EQ $02	;#CONSTANT. The voice mode value when whisper mode is enabled

NTALK.SEARCH.MODE		.EQ SHARED.VARIABLE_SPACE.BLOCK2+$47 ;$1byts. $00 = find record, $01 = find intro text, $02 = find keyword 
NTALK.FLAG.BYE				.EQ SHARED.VARIABLE_SPACE.BLOCK2+$48 ;$1byts. Set to $01 if the player's input to NPC.TALK.INPUT was "BYE", otherwise defaults to #$00.			

NTALK.TEXT_BLOCK.START		.EQ SHARED.VARIABLE_SPACE.BLOCK2+$49 ;2bytes. Stores a pointer to the start of the text block. Current only used for the keyword insert subcommand. 
NTALK.MODIFIED.TEXT_BLOCK	.EQ NPC.TALK.ARRAY.PACKED	;#CONSTANT. When a text block in a talk data file is modified, this address where the modified text is stored. 
NPC.TALK.KEYWORD.ORIGINAL			.EQ SHARED.VARIABLE_SPACE.BLOCK2+$4B ;$20byts. Holds a copy of NPC.TALK.KEYWORD, which is preserved as the original for the duration of NPC.SEARCH. 

NPC.TALK.FILE.RECORD_ID				.EQ SHARED.VARIABLE_SPACE.BLOCK2+$6B ;$1byts. NPC ID of the first packed data record 
NPC.TALK.FILE.PACKED_RECORD_LENGTH	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$6C ;$2byts. Length (2 bytes) of the packed data in the record
NPC.TALK.FILE.RECORD_COUNTER		.EQ SHARED.VARIABLE_SPACE.BLOCK2+$6E ;$1byts. this counter is incremented each time a record is read in from the curret NPC Talk data file.

;NTALK.ARRAY.SEARCH
NTALK.SEARCH.MATCH_QTY	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$6F ;$1byts. Number of characters that must be the same for a match to be found. For exampe, if this variable is set to 4 then if the first 4 characters of the keyword are found then the entire keyword is considered to be found. The number of characters must include the NTALK keyword subcommand code of "$!"
NTALK.KEYWORD.POINTER	.EQ $D2 ;a pointer to the keyword which is being search for within an array.
NTALK.ARRAY.POINTER		.EQ SHARED.VARIABLE_SPACE.BLOCK2+$70 ;$2byts. Parameter to NPC.ARRAY.SEARCH, containing the address of the array to be searched, usually NPC.TALK.ARRAY
NTALK.ARRAY.POINTER.RETURN	 .EQ $E2 ;pointer to the array being searched for a keyword. Usually points to NPC.TALK.ARRAY. It is also a return value, returing the updated pointer position, pointing to the next byte after first character after the end of the keyword. 

NTALK.ARRAY.POINTER.RETURN.HO.SAVED .EQ SHARED.VARIABLE_SPACE.BLOCK2+$72 ;$1byt
NTALK.ARRAY.POINTER.RETURN.LO.SAVED .EQ SHARED.VARIABLE_SPACE.BLOCK2+$73 ;$1byt
NTALK.ARRAY.STOP.VALUE	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$74 ;$1byts. $00 = find record, $01 = find intro text, $03 = find keyword 

NTALK.KEYWORD.STOP_VALUE			.EQ SHARED.VARIABLE_SPACE.BLOCK2+$75 ;$1byts. Stores parameter for the stop value of the keyword string

;**OPT**. NTALK.KEYWORD.BUFFER and all variables below this point (confirmed thru $BF)
;can be pointed to TEXT.WINDOW.BUFFER (setup a stepper definition in the shared variable section at the top of this file)
;This because NTALK.KEYWORD.BUFFER isn't needed after NTALK.SEARCH returns a pointer to the
;text block response. If the keyword is parroted it is collated into the text block.
;The other variables past this point (confirmed thru $BF) aren't used at the same time as the text window buffer either.
;I accidentally had things setup this way originally and it passed rigorous testing. It only got changed to be less efficient when I
;regorganized the shared variables  into pointers to the newly created shared variable section of this file.

NTALK.KEYWORD.BUFFER				.EQ SHARED.VARIABLE_SPACE.BLOCK2+$76 ;$20byts. Holds a copy of NPC.TALK.KEYWORD or whichever keyword is provided to NPC.ARRAY.SEARCH via the keyword pointer. The pointer keyword array is copied into this buffer so X-REG can be used as the index. 
NTALK.KEYWORD.1ST_CHAR.UCASE		.EQ SHARED.VARIABLE_SPACE.BLOCK2+$96 ;$1byts. 
NTALK.KEYWORD.POINTER.UPDATED		.EQ SHARED.VARIABLE_SPACE.BLOCK2+$97 ;$2byts. NPC.SEARCH.ARRAY returns the final value of the keyword pointer via this variable. It is equal to the byte after the keyword array's stop value.  

NTALK.END.OF.FILE		.EQ $FF ;#CONSTANT. Indicates the end of the NPC.TALK.ARRAY has been reached.
NTALK.END.OF.RECORD		.EQ $FC ;#CONSTANT. Indicates the end of a record in the NPC.TALK.ARRAY has been reached.

NTALK.KEYWORD.UCASE			.EQ SHARED.VARIABLE_SPACE.BLOCK2+$99 ;$1byts. Holds the uppercase value (for letters) of the next keyword character during the search loop. 
NTALK.ARRAY.UCASE			.EQ SHARED.VARIABLE_SPACE.BLOCK2+$9A ;$1byts. Holds the uppercase value (for letters) of the next search array character during the search loop.

;NTALK.SEARCH.LOOP
NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS .EQ SHARED.VARIABLE_SPACE.BLOCK2+$9B ;$20byts.
SUB_COMMAND.FLAGS.STOP_VALUE	.EQ	$20	;#CONSTANT. Should be equal to the # of bytes of NTALK.SEARCH.RETURN.SUB_COMMAND.FLAGS 
NTALK.COMMAND_FOUND.INDEX	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$BB ;$2byts. Index to the last character of the primary command (+$1) last searched for in the NPC TALK array. Used as the starting element to search for subcommands. So if the command searched for was $R, the index would point to the character after the R
NTALK.FLAG.INTERNAL_SEARCH	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$BD ;$1byts. This flag is on when internal searches, such as for the command end code, are being done. This flag is used to override the minimum characters required for a match as specified by NTALK.SEARCH.MATCH_QTY

;NTALK.PARSE.KEYWORD
NTALK.CURSE_WORD.COUNTER			.EQ SHARED.VARIABLE_SPACE.BLOCK2+$BE ;$1byts
NTALK.TOTAL.CURSE_WORDS				.EQ $08		



@END

;========PLAYER MOVEMENT======
@START

PLAYER.MOVE.CURRENT 			.BS	$1					;CONTAINS THE DIRECTION OF PLAYERS REQUESTED MOVE, AFTER #COLLISION CHECKS. 
PLAYER.MOVE.CANDIDATE_TILE_LOC 	.EQ	SHARED.VARIABLE_SPACE.BLOCK3		;CONTAINS THE TILE IN THE DESTINATION OF THE PLAYERS REQUESTED MOVE, BEFORE #COLLISION CHECKS. (1 TILE AWAY FROM PLAYER)
PLAYER.MOVE.CANDIDATE_TILE_LOC2	.EQ SHARED.VARIABLE_SPACE.BLOCK3+$1	;CONTAINS THE TILE IN THE DESTINATION OF THE PLAYERS REQUESTED MOVE, BEFORE #COLLISION CHECKS. (2 TILES AWAY FROM PLAYER)
PLAYER.MOVE.JUMP				.BS $1					;CONTAINS $01 IF THE PLAYER EXECUTES A JUMP COMMAND WHILE ON A HORSE
PLAYER.MOVE.COUNTER				.BS $1					;TRACKS WHETHER PLAYER MOVE 1 OR 2 IS BEING PROCESSED WHEN FAST HORSE IS ACTIVE. NEEDED WHEN (J)UMP COMMAND IS USED, WHICH TEMPORARILY ENABLES FAST HORSE. 
PLAYER.MOVE.FORCED				.BS $1					;$01 == FORCED. USED WHEN MOVE.N/S/E/W ARE CALLED IN SITUATIONS OTHER THNA A PLAYER KEYPRESS TRIGGER (I.E. TO USE THEM TO MOVE PLAYERS LOCATION AUTOMATICALLY FOR SOME REASON, SUCH AS WHEN AN FRIGATE (MT OBJECT) IS BOARDED AND THE SHIP ISN'T CENTERED ON THE SCREEN. 
PLAYER.TILE.LAST				.BS $1					;THE TILE TYPE THE PLAYER WAS STANDING ON AT THE END OF THE LAST MOVE

;PLAYER #COLLISION RULES  --WALKING
COLLISION_FLAG.WALKING.LT1		.EQ	$0B		;#CONSTANT
COLLISION_FLAG.WALKING.GRE1.1	.EQ	$5D		;#CONSTANT
COLLISION_FLAG.WALKING.LT1.1	.EQ	$80		;#CONSTANT
COLLISION_FLAG.WALKING.GRE2		.EQ	$80		;#CONSTANT
COLLISION_FLAG.WALKING.LT2		.EQ	$8C		;#CONSTANT
COLLISION_FLAG.WALKING.GRE3		.EQ	$80		;#CONSTANT
COLLISION_FLAG.WALKING.LT3		.EQ	$88		;#CONSTANT

;PLAYER #COLLISION RULES  --WALKING (UNDERMAP)
COLLISION_FLAG.UNDERMAP.WALKING.GRE1	.EQ	$00		;#CONSTANT
COLLISION_FLAG.UNDERMAP.WALKING.LT1		.EQ	$40		;#CONSTANT
COLLISION_FLAG.UNDERMAP.WALKING.GRE2	.EQ	$78		;#CONSTANT
COLLISION_FLAG.UNDERMAP.WALKING.LT2		.EQ	$7C		;#CONSTANT
COLLISION_FLAG.UNDERMAP.WALKING.GRE3	.EQ	$8D		;#CONSTANT
COLLISION_FLAG.UNDERMAP.WALKING.LT3		.EQ	$91		;#CONSTANT

;PLAYER #COLLISION RULES  --CARAVEL--
COLLISION_FLAG.CARAVEL.LT		.EQ $8A		;#CONSTANT


;PLAYER #COLLISION RULES  --FRIGATE--
COLLISION_FLAG.FRIGATE.LT		.EQ $8B		;#CONSTANT


;PLAYER #COLLISION RULES  --SKIFF--
COLLISION_FLAG.SKIFF.LT			.EQ $63		;#CONSTANT	
COLLISION_FLAG.SKIFF.EQ1		.EQ $7C		;#CONSTANT
COLLISION_FLAG.SKIFF.EQ2		.EQ $8B		;#CONSTANT

;PLAYER #COLLISION RULES  --HORSE--
COLLISION_FLAG.HORSE.LT1		.EQ $0B		;#CONSTANT
COLLISION_FLAG.HORSE.GRE1.1		.EQ	$5D		;#CONSTANT
COLLISION_FLAG.HORSE.LT1.1		.EQ	$80		;#CONSTANT
COLLISION_FLAG.HORSE.GRE		.EQ $80		;#CONSTANT
COLLISION_FLAG.HORSE.LT2		.EQ $8B		;#CONSTANT
COLLISION_FLAG.HORSE.EQ1		.EQ $46		;#CONSTANT

;PLAYER #COLLISION RULES  --WYVERN--
COLLISION_FLAG.WYVERN.LT1		.EQ	$0B		;#CONSTANT
COLLISION_FLAG.WYVERN.GRE		.EQ	$80		;#CONSTANT
COLLISION_FLAG.WYVERN.LT2		.EQ	$8C		;#CONSTANT
@END

;========PLAYER VARIABLES======
@START

;GAME SETTINGS
GAME.ADULT.MODE						.BS $1	;($00 = OFF | $01 >= ON)
GAME.COMBAT_MATH_DISPLAY.MODE 		.BS $1	;($00 = OFF | $01 >= ON)
GAME.SCROLL_SPEED					.BS $1	;the length of the delay inserted between HRCG text characters printed in the combat scroll window. 		

;ICON RELATED
PLAYER.HEIGHT.DEFAULT				.EQ $10			;#CONSTANT. INTENTIONALLY SET TO TILE HEIGHT+$3. THIS IT THE HEIGHT SET AT GAME LAUNCH AND AFTER PLAYER RETURNS TO A REGULAR TILE AFTER STEPPING ON A SINKING TILE LIKE QUICKSAND
PLAYER.HEIGHT						.BS $1			;HEIGHT IN LINES. USED TO CONTROL SPECIAL EFFECTS LIKE SINKING IN SHALLOW WATER
PLAYER.WALKING.TILE					.BS	$1			;STORE THE TILE ID OF THE PLAYER ICON WHEN NO TRANSPORT IS ACTIVE
PLAYER.WALKING.TILE.DEFAULT			.BS $1			;STORES THE DEFAULT TILE ID FOR THE PLAYER ICON IN CASE IT GETS CHANGED TEMPORARILY
PLAYER.ICON.BUFFER					.BS $20			;STORES THE CURRENT WALKING PLAYER ICON. KEEPS TRACK OF CHANGES TO IT SUCH AS WHEN THE PLAYER SINKS IN WATER.

;PARTY RELATED
PARTY.TOTAL.PC	.BS $1 ;The number of player characters currently enrolled in the player's party (regardless of health status)

;TRANSPORT RELATED
PLAYER.TILE.ACTIVE					.BS $1			;THE TILE TYPE OF THE TRANSPORT RECORD POINTED TO BY THE INDEX STORED IN PLAYER.TRANSPORT.ACTIVE
PLAYER.TRANSPORT.ACTIVE				.BS $1			;TRACKS AN INDEX TO THE TRANSPORT OBJECT BOARDED. IF SET TO $FF, PLAYER IS WALKING AND PLAYER.MAP.ICON IS USED TO DETERMINE TILE_TYPE FOR PLAYER ICON. 
PLAYER.TRANSPORT.SPEED				.BS $1			;THE NUMBER OF MOVES THE PLAYER GETS FOR EVERY ONE MOB MOVE. $00 = 1 MOVE. > $00 = 2 MOVES
PLAYER.TRANSPORT.STATUS				.BS $1			;$00 = NOT SET, $02 = FAST HORSE, $04 = WYVERN  (THIS VARIABLE EXISTS SO THAT PLAYER.TRANSPORT.SPEED CAN BE USED BY OTHER COMMANDS (LIKE JUMP) WITHOUT LOOSING KNOWLEDGE OF TRANSPORT STATUS (I.E. FAST HORSE ENABLED) WHEN THE OTHER COMMAND WAS ISSUED)
PLAYER.TRANSPORT.FRIGATE.SKIFF_MAX	.EQ $02			;#CONSTANT. THE MAXIMUM NUMBER OF SKIFFS A FRIGATE CAN HOLD
PLAYER.TRANSPORT.MT.TILE_LOCATIONS	.BS $4			;STORES THE SCREEN LOCATIONS OF ANY ACTIVE MULTI TILE TRANSPORT. THE LOCATIONS VERY BASED ON WHICH TILE OF THE MO THE PLAYER BOARDS. 

PLAYER.TRANSPORT.MT.TILE_LOCATION0	.EQ	$5D			;#CONSTANT. THE SCREEN ARRAY LOCATIONS OF THE PLAYER WHEN MULTI-TILE TRANSPORT IS ACTIVE 
PLAYER.TRANSPORT.MT.TILE_LOCATION1	.EQ	$5E			;#CONSTANT. THE SCREEN ARRAY LOCATIONS OF THE PLAYER WHEN MULTI-TILE TRANSPORT IS ACTIVE 
PLAYER.TRANSPORT.MT.TILE_LOCATION2	.EQ	$6E			;#CONSTANT. THE SCREEN ARRAY LOCATIONS OF THE PLAYER WHEN MULTI-TILE TRANSPORT IS ACTIVE 
PLAYER.TRANSPORT.MT.TILE_LOCATION3	.EQ	$6F			;#CONSTANT. THE SCREEN ARRAY LOCATIONS OF THE PLAYER WHEN MULTI-TILE TRANSPORT IS ACTIVE 



;#PLAYER.MT.ADJACENT_TILES are constants that refer to the tile locations adjacent to a
;Frigate (multi-tile transport) when it is the active transport for the player. 
;
;The constants map to the screen array as follows, where 0-3 are the tiles #s of the multi-tile shape
; NN
;W01E
;W23E
; SS
;
;0-3 correspond to the screen array locations in the PLAYER.TRANSPORT.MT.TILE_LOCATION(0-3) set of constants


PLAYER.MT.ADJACENT_TILES.NORTH0	.EQ $4C				;#CONSTANT. THE SCREEN ARRAY LOCATIONS OF THE TILE ADJACENT TO THE PLAYER WHEN MULTI-TILE TRANSPORT IS ACTIVE 
PLAYER.MT.ADJACENT_TILES.NORTH1	.EQ $4D				;#CONSTANT. THE SCREEN ARRAY LOCATIONS OF THE TILE ADJACENT TO THE PLAYER WHEN MULTI-TILE TRANSPORT IS ACTIVE 
PLAYER.MT.ADJACENT_TILES.SOUTH0	.EQ $7F				;#CONSTANT. THE SCREEN ARRAY LOCATIONS OF THE TILE ADJACENT TO THE PLAYER WHEN MULTI-TILE TRANSPORT IS ACTIVE 
PLAYER.MT.ADJACENT_TILES.SOUTH1	.EQ $80				;#CONSTANT. THE SCREEN ARRAY LOCATIONS OF THE TILE ADJACENT TO THE PLAYER WHEN MULTI-TILE TRANSPORT IS ACTIVE 
PLAYER.MT.ADJACENT_TILES.EAST0	.EQ $5F				;#CONSTANT. THE SCREEN ARRAY LOCATIONS OF THE TILE ADJACENT TO THE PLAYER WHEN MULTI-TILE TRANSPORT IS ACTIVE 
PLAYER.MT.ADJACENT_TILES.EAST1	.EQ $70				;#CONSTANT. THE SCREEN ARRAY LOCATIONS OF THE TILE ADJACENT TO THE PLAYER WHEN MULTI-TILE TRANSPORT IS ACTIVE 
PLAYER.MT.ADJACENT_TILES.WEST0	.EQ $5C				;#CONSTANT. THE SCREEN ARRAY LOCATIONS OF THE TILE ADJACENT TO THE PLAYER WHEN MULTI-TILE TRANSPORT IS ACTIVE 
PLAYER.MT.ADJACENT_TILES.WEST1	.EQ $6D				;#CONSTANT. THE SCREEN ARRAY LOCATIONS OF THE TILE ADJACENT TO THE PLAYER WHEN MULTI-TILE TRANSPORT IS ACTIVE 


;MAP RELATED		
PLAYER.MAP.ICON				.EQ PLAYER.WALKING.TILE		;HOLDS THE TILE_TYPE OF THE PLAYERS WALKING ICON (NOT BOARDED TRANSPORT)
;PLAYER.COLLISSION_OVERRIDE	.BS $1		;$00 = OFF, $01 = ON
PLAYER.COLLISSION_OVERRIDE	.BS $1		;$00 = OFF, $01 = ON
PLAYER.DARKNESS_OVERRIDE	.BS $1		;$00 = OFF, >=$01 = ON	
PLAYER.PLS_STATUS			.BS $1		;PLAYER LIGHT SOURCE. $00 = OFF, $01 = TORCH

PLAYER.MAP.X				.BS $1		;STORES PLAYER'S MAP X-AXIS (DIFFERENT THAN THE X,Y USED FOR TRACKING MOB LOCATIONS)
PLAYER.MAP.Y				.BS $1		;STORES PLAYER'S MAP X-AXIS (DIFFERENT THAN THE X,Y USED FOR TRACKING MOB LOCATIONS)
;PLAYER.MAP.X				.EQ $9104	;STORES PLAYER'S MAP X-AXIS (DIFFERENT THAN THE X,Y USED FOR TRACKING MOB LOCATIONS)
;PLAYER.MAP.Y				.EQ $9105	;STORES PLAYER'S MAP X-AXIS (DIFFERENT THAN THE X,Y USED FOR TRACKING MOB LOCATIONS)
PLAYER.COMMAND.LAST			.BS $1		;STORES THE UPPER CASE ASCII VALUE OF THE LAST VALID COMMAND ISSUED BY THE PLAYER. THIS VALUE IS SET AT THE END OF THE SUBROUTINE FOR THE ASSOCIATED COMMAND. 
PLAYER.COMMAND.CURRENT		.BS $1
PLAYER.ADJACENT.SCREEN_LOCATIONS .EQ SPRITE.RECORD ;4bytes. stores the screen array index to the tiles adjacent to the player, which can vary depending on if the combat is active. 
PLAYER.ADJACENT.NORTH		.EQ $4C ;#CONSTANT. Screen index to the tile to the north of player when combat is not active
PLAYER.ADJACENT.SOUTH		.EQ $6E ;#CONSTANT. Screen index to the tile to the south of player when combat is not active
PLAYER.ADJACENT.EAST		.EQ $5E ;#CONSTANT. Screen index to the tile to the east of player when combat is not active
PLAYER.ADJACENT.WEST		.EQ $5C ;#CONSTANT. Screen index to the tile to the west of player when combat is not active

;COMBAT RELATED

;PLAYER.HOTKEYS.SPELLS	.BS $A
PLAYER.HOTKEYS.SPELLS.ARRAY_SIZE 	.EQ $A		;#CONSTANT
PLAYER.HOTKEYS.SPELLS
; LOWER SPELLS
	; ; .BS $1, $14 ;key 0
	; ; .BS $1, $15 ;key 1
	; ; .BS $1, $16 ;key 2
	; ; .BS $1, $17 ;key 3
	; ; .BS $1, $0E ;key 4
	; ; .BS $1, $12 ;key 5
	; ; .BS $1, $13 ;key 6


	
	; ;.BS $1, $0A ;key 0
	; .BS $1, $00 ;key 0
	; .BS $1, $01 ;key 1
	; .BS $1, $02 ;key 2
	; .BS $1, $03 ;key 3
	; .BS $1, $04 ;key 4
	; .BS $1, $05 ;key 5
	; .BS $1, $06 ;key 6
	; .BS $1, $07 ;key 7
	; .BS $1, $08 ;key 8
	; .BS $1, $09 ;key 9
	
	; ; .BS $1, $1A ;key 6
	; ; .BS $1, $1B ;key 7	
	; ; .BS $1, $1C ;key 8
	; ; .BS $1, $1D ;key 9
	
; ;INVENTORY TEST SPELLS	
	; .BS $1, $00 ;key 0
	; .BS $1, $FF ;key 1
	; .BS $1, $FF ;key 2
	; .BS $1, $FF ;key 3
	; .BS $1, $FF ;key 4
	; .BS $1, $FF ;key 5
	; .BS $1, $FF ;key 6
	; .BS $1, $07 ;key 7
	; .BS $1, $FF ;key 8
	; .BS $1, $FF ;key 9

;KFEST DEMO SPELLS	
	.BS $1, $00 ;key 0
	.BS $1, $03 ;key 1
	.BS $1, $30 ;key 2
	.BS $1, $15 ;key 3
	.BS $1, $FF ;key 4
	.BS $1, $FF ;key 5
	.BS $1, $FF ;key 6
	.BS $1, $07 ;key 7
	.BS $1, $FF ;key 8
	.BS $1, $FF ;key 9

	
; ;UPPER SPELLS
	; .BS $1, $05 ;key 0
	; .BS $1, $06 ;key 1
	; .BS $1, $07 ;key 2
	; .BS $1, $08 ;key 3
	; .BS $1, $09 ;key 4
	; .BS $1, $0A ;key 5
	; .BS $1, $0B ;key 6
	; .BS $1, $0C ;key 7
	; .BS $1, $12 ;key 8
	; .BS $1, $13 ;key 9
	
;original
; ;	.BS $1, $00 ;key 0
	; .BS $1, $00 ;key 0
; ;	.BS $1, $01 ;key 1
	; .BS $1, $02 ;key 1
; ;	.BS $1, $03 ;key 1
	; .BS $1, $05 ;key 2
	; .BS $1, $07 ;key 3
	; .BS $1, $09 ;key 4
	; .BS $1, $0B ;key 5
	; .BS $1, $0C ;key 6
	; .BS $1, $12 ;key 7
	; .BS $1, $13 ;key 8
	; .BS $1, $0A ;key 9
; ;	.BS $1, $08 ;key 9

PLAYER.HOTKEYS.SPELLS.UNASSIGNED_CODE .EQ $FF ;#CONSTANT

;PLS CONSTANTS
;see DARKNESS section

@END

;========PLAYTEST KEYS=========
@START


@END

;========SCREEN OPERATIONS======
@START

SCREEN.COLUMN.STOP	.BS $1						;STORES THE STOP VALUE WHEN LOADING A COLUMN OF TILES (TILE.LOOKUP.COLUMN)
SCREEN.ARRAY.INDEX 			.EQ SHARED.VARIABLE_SPACE.BLOCK3+$0	
RZONE.ARRAY.COLUMN.INDEX 	.EQ SHARED.VARIABLE_SPACE.BLOCK3+$1

RMAP.LOOKUP					.BS $2						;USED TO STORE AN RMAP VALUE FOR PURPOSES OF LOOKING UP A TILE TYPE USING TILE.LOOKUP.SINGLE OR THE IN-LINE CODE EQUIVILENT
	
@END

;========SOUND MANAGER==========
@START
SPEAKER		.EQ		$C030	;SPEAKER SOFTSWITCH (toggles speaker on/off)
HALFTIME	.BS		$1		;!33 ;= (1/frequency)/(2*34)
LENGTH		.BS		$1		;!29 ;Duration in units of 34*255 usec

SOUND_DATA.POINTER		.EQ $C6 ;#POINTER. Used by PLAY.SOUND. Points to the hex table containing the frequency and duration values for the sound to play. 
SOUND_DATA.WAIT.POINTER	.EQ $C8 ;#POINTER. Used by PLAY.SOUND. Points to the hex array containing the wait points and wait durations for the array connected to SOUND_DATA.POINTER
SOUND_DATA.LENGTH		.EQ SHAPE.HOPPER0 ;$1byte

@END

;========SWAP SPACE=============
;See beginning of this file

;========TEXT WINDOWS & SCREEN BORDERS================
@START


;SCREEN BORDER
SCREEN_BORDER.TOP_LINE		.EQ $04	;#CONSTANT
SCREEN_BORDER.BOTTOM_LINE	.EQ $BC	;#CONSTANT
SCREEN_BORDER.LR_SBYTE		.EQ $27	;#CONSTANT. Lower right screen byte



;TEXT WINDOW: RIGHT (NPC TALK)

;border dimensions
TWB.RW.NPC_TALK.TOP_LINE		.EQ $04	;#CONSTANT.
TWB.RW.NPC_TALK.BOTTOM_LINE		.EQ $BC	;#CONSTANT.
TWB.RW.NPC_TALK.LEFT_SBYTE		.EQ $18	;#CONSTANT. left edge screen byte
TWB.RW.NPC_TALK.RIGHT_SBYTE		.EQ $27 ;#CONSTANT. right edge screen byte
TWB.RW.NPC_TALK.WIDTH			.EQ $08 ;#CONSTANT. of tiles in NPC Talk Text Window
TWB.RW.NPC_TALK.LEFT_TILE_COLUMN	.EQ $0B ;#CONSTANT. the screen tile column number of the left side of the text window

;text space dimensions (hi-res)
TWS.RW.NPC_TALK.TOP_ROW		.EQ $05	;#CONSTANT
TWS.RW.NPC_TALK.BOTTOM_ROW	.EQ $BB	;#CONSTANT
TWS.RW.NPC_TALK.LEFT_SBYTE	.EQ $19	;#CONSTANT. left edge screen byte
TWS.RW.NPC_TALK.RIGHT_SBYTE	.EQ $26 ;#CONSTANT. right edge screen byte
TWS.RW.NPC_TALK.WIDTH		.EQ $07 ;# of tiles in NPC Talk Text Window

;TEXT WINDOW: INPUT (NPC TALK)
;border dimensions
TWB.TALK_INPUT_WINDOW.TOP_LINE		.EQ $93	;#CONSTANT.
TWB.TALK_INPUT_WINDOW.LEFT_SBYTE	.EQ $18	;#CONSTANT. left edge screen byte
TWB.TALK_INPUT_WINDOW.RIGHT_SBYTE	.EQ $27 ;#CONSTANT. right edge screen byte

	;text space dimensions (hi-res)
TWS.TALK_INPUT_WINDOW.TOP_LINE		.EQ $97	;#CONSTANT
TWS.TALK_INPUT_WINDOW.BOTTOM_LINE	.EQ $A3	;#CONSTANT
TWS.TALK_INPUT_WINDOW.LEFT_SBYTE	.EQ $19	;#CONSTANT. left edge of top line, it's shorter than width of window.
TWS.TALK_INPUT_WINDOW.RIGHT_SBYTE	.EQ $26 ;#CONSTANT. right edge screen byte, it's shorter than width of window.

;setup
; TWS.TALK_INPUT_WINDOW.PROMPT_START.HTAB	.EQ $19	;#CONSTANT. The VTAB of the cursor after the window is reset, before any input is received. 
; TWS.TALK_INPUT_WINDOW.PROMPT_START.VTAB	.EQ $13	;#CONSTANT. The HTAB of the cursor after the window is reset, before any input is received. 

; TWS.TALK_INPUT_WINDOW.CURSOR_START.HTAB	.EQ $1A	;#CONSTANT. The HTAB of the cursor after the window is reset, before any input is received. 
; TWS.TALK_INPUT_WINDOW.CURSOR_START.VTAB	.EQ $13	;#CONSTANT. The VTAB of the cursor after the window is reset, before any input is received. 
TWS.TALK_INPUT_WINDOW.CURSOR_START.HTAB	.EQ $19	;#CONSTANT. The HTAB of the cursor after the window is reset, before any input is received. 
TWS.TALK_INPUT_WINDOW.CURSOR_START.VTAB	.EQ $13	;#CONSTANT. The VTAB of the cursor after the window is reset, before any input is received. 

		
TW.RIGHT_WINDOW.CLEAN_UP.FLAG	.BS	$01	;($00 = off | $01 = on). controls whether DRAW.SCREEN will erase the right edge and first two bytes of the top/bottom lines of the text window border. 
TW.RIGHT_WINDOW.STATUS.FLAG		.EQ TW.RIGHT_WINDOW.CLEAN_UP.FLAG  ;$00 = not active, $01 = active

SCREEN_BYTE.COUNTER		.EQ SHARED.VARIABLE_SPACE.BLOCK2+$80 ;$1byt
USE.PAGE			 	.EQ SHARED.VARIABLE_SPACE.BLOCK2+$81 ;$1byt
LINE.COUTNER 			.EQ SCREEN_BYTE.COUNTER


;TEXT WINDOW HI-RES DIMENSIONS: TOP
TWB.TOP_WINDOW.TOP_LINE		.EQ $00 ;#CONSTANT
TWB.TOP_WINDOW.BOTTOM_LINE	.EQ $07 ;#CONSTANT
TWB.TOP_WINDOW.LEFT_SBYTE	.EQ $00 ;#CONSTANT. Lower right screen byte
TWB.TOP_WINDOW.RIGHT_SBYTE	.EQ $27 ;#CONSTANT. Lower right screen byte

;TEXT WINDOW HI-RES DIMENSIONS: BOTTOM
;Note: not technically a text window since it is only one line in size. Print to it with PRINT.STR not PRINT.TEXT.WINDOW
TWB.BOTTOM_WINDOW.TOP_LINE		.EQ $B8 ;#CONSTANT
TWB.BOTTOM_WINDOW.BOTTOM_LINE	.EQ $BF ;#CONSTANT
TWB.BOTTOM_WINDOW.LEFT_SBYTE	.EQ $00 ;#CONSTANT. Lower right screen byte
TWB.BOTTOM_WINDOW.RIGHT_SBYTE	.EQ $27 ;#CONSTANT. Lower right screen byte

	;text window space dimensions: bottom
TWS.BOTTOM_WINDOW.PRINT.HTAB	.EQ $B		;#CONSTANT. use this position when printing text to the text window. 
TWS.BOTTOM_WINDOW.PRINT.VTAB	.EQ $17		;#CONSTANT. use this position when printing text to the text window.  
TWS.BOTTOM_WINDOW.SIZE			.EQ $F		;#CONSTANT. # of characters. 
	
TW.BOTTOM_WINDOW.INIT_CODE		.BS $1	;set this variable to trigger INIT.TW_BOTTOM to print a specific text block when GAME.LAUNCH is called to force a full screen draw.  
				;						;($00 = none | $01 = Ignite Torch | $02 = Open door | $03 = Unlock Door | $04 = Operate lever | $05 = Invalid Command)
	
;TEXT WINDOW: RIGHT-SMALL (GENERAL SCREEN)
;border dimensions
TWB.RIGHT_S_WINDOW.TOP_LINE		.EQ $00 ;#CONSTANT
TWB.RIGHT_S_WINDOW.BOTTOM_LINE	.EQ $C0 ;#CONSTANT
TWB.RIGHT_S_WINDOW.LEFT_SBYTE	.EQ $24 ;#CONSTANT. Lower right screen byte
TWB.RIGHT_S_WINDOW.RIGHT_SBYTE	.EQ $27 ;#CONSTANT. Lower right screen byte


;TEXT WINDOW: RIGHT; DISPLAY CHARACTER ROSTER (GENERAL SCREEN)

;border dimensions
TWB.RW.DISPLAY_CHARACTER_ROSTER.TOP_LINE		.EQ $08	;#CONSTANT.
TWB.RW.DISPLAY_CHARACTER_ROSTER.BOTTOM_LINE		.EQ $70	;#CONSTANT.
TWB.RW.DISPLAY_CHARACTER_ROSTER.LEFT_SBYTE		.EQ $18	;#CONSTANT. left edge screen byte
TWB.RW.DISPLAY_CHARACTER_ROSTER.RIGHT_SBYTE		.EQ $28 ;#CONSTANT. right edge screen byte

;TEXT WINDOW: LEFT; INVENTORY

;border dimensions
TWB.LW.INVENTORY.TOP_LINE			.EQ $04	;#CONSTANT.
TWB.LW.INVENTORY.BOTTOM_LINE		.EQ $BD	;#CONSTANT.
TWB.LW.INVENTORY.LEFT_SBYTE			.EQ $0	;#CONSTANT. left edge screen byte
TWB.LW.INVENTORY.RIGHT_SBYTE		.EQ $14 ;#CONSTANT. right edge screen byte
TWB.LW.INVENTORY.SEPERATOR_LINE		.EQ $1C	;#CONSTANT.

;text space dimensions
TWS.LW.INVENTORY.TOP_LINE						.EQ $28	;#CONSTANT. The top line below the column headings
TWS.LW.INVENTORY.TOP_LINE.TEXT_SPACE			.EQ $20	;#CONSTANT. The top hi-res line of the text row just below the text row that the line is in that seperates the menu icons from the text space. 
TWS.LW.INVENTORY.BOTTOM_LINE		.EQ $BC	;#CONSTANT.
TWS.LW.INVENTORY.LEFT_SBYTE			.EQ $1	;#CONSTANT. left edge screen byte
TWS.LW.INVENTORY.RIGHT_SBYTE		.EQ $13 ;#CONSTANT. right edge screen byte




;TEXT WINDOW: RIGHT (SCROLL); INVENTORY

;border dimensions
TWB.INVENTORY.SCROLL_WINDOW.TOP_LINE				.EQ	$68 ;#CONSTANT. This is the top line if there entire space were used as a scroll window (i.e. it's the top line of the combat window scroll window). See TWS.INVENTORY.SCROLL_WINDOW.SEPERATOR.LINE
TWB.INVENTORY.SCROLL_WINDOW.LEFT_SBYTE				.EQ $18	;#CONSTANT. left edge of top line
TWB.INVENTORY.SCROLL_WINDOW.RIGHT_SBYTE				.EQ $27 ;#CONSTANT. right edge screen byte
TWB.INVENTORY.SCROLL_WINDOW.BOTTOM_LINE				.EQ $BC	;#CONSTANT.


;text space dimensions
TWS.INVENTORY.SCROLL_WINDOW.TOP_LINE				.EQ $70	;#CONSTANT.	Top hi-res line
TWS.INVENTORY.SCROLL_WINDOW.BOTTOM_LINE				.EQ $B8	;#CONSTANT.	Top hi-res line
TWS.INVENTORY.SCROLL_WINDOW.LEFT_SBYTE				.EQ $19	;#CONSTANT.
TWS.INVENTORY.SCROLL_WINDOW.RIGHT_SBYTE				.EQ $26 ;#CONSTANT. right edge screen byte
TWS.INVENTORY.SCROLL_WINDOW.TOP_ROW					.EQ $11 ;#CONSTANT		
TWS.INVENTORY.SCROLL_WINDOW.BOTTOM_ROW				.EQ $16 ;#CONSTANT		
TWS.INVENTORY.SCROLL_WINDOW.WIDTH					.EQ TWS.INVENTORY.SCROLL_WINDOW.RIGHT_SBYTE-TWS.INVENTORY.SCROLL_WINDOW.LEFT_SBYTE+$1	;#CONSTANT
TWS.INVENTORY.SCROLL_WINDOW.CURSOR_START_SBYTE		.EQ TWS.INVENTORY.SCROLL_WINDOW.LEFT_SBYTE ;#CONSTANT
TWS.INVENTORY.SCROLL_WINDOW.CURSOR_START_ROW		.EQ TWS.INVENTORY.SCROLL_WINDOW.TOP_ROW ;#CONSTANT

TWS.INVENTORY.SCROLL_WINDOW.STATIC_TEXT.ROW			.EQ $0E ;#CONSTANT. Where readied equipment weight is printed 
TWS.INVENTORY.SCROLL_WINDOW.SEPERATOR.LINE			.EQ $84 ;#CONSTANT. The line that seperates the actual scrolling part of the window from the static text

@END


;========TEXT WINDOW FUNCTION===
@START

;----------------------must stay in order---------------
;dimensions
TWF.LEFT_SBYTE			.EQ $F0	;1byt
TWF.WIDTH				.EQ $F1	;1byt
TWF.TOP_ROW				.EQ $F2	;1byt
TWF.BOTTOM_ROW			.EQ $F3	;1byt
TWF.RIGHT_SBYTE			.EQ $F4	;1byt. Right screen byte of text space +$1 (which means it is equal to the screen byte of the right edge border of the window)
;--------------------------------------------------------



;printing characters
TWF.STRING				.EQ $E4	;2byt. zero page pointer for the input string
TWF.STRING.STOP.VALUE	.EQ	$00	;#CONSTANT


;TEXT.WINDOW.BUFFER		.EQ	SHARED.VARIABLE_SPACE.BLOCK1+$88 ;$40byt
TEXT.WINDOW.BUFFER		.EQ	TEXT.WINDOW.BUFFER_STEP ;$40byt
TEXT.WINDOW.CHAR		.EQ SHARED.VARIABLE_SPACE.BLOCK1+$C8 ;$1byt   Current character when parsing STRING


TEXT.WINDOW.BUFFER.TALLY	.EQ SHARED.VARIABLE_SPACE.BLOCK1+$C9 ;$1byt.   # of characters in the buffer
TWF.BOTTOM_ROW.TRIGGER		.EQ SHARED.VARIABLE_SPACE.BLOCK1+$CA ;$1byt.  The row which triggers the scroll prompt

;Scrolling
TEXT.CHAR.DEPTH				.EQ $08	;#CONSTANT. number of lines in a text character
TWF.SCROLLING.STOP_LINE		.EQ SHARED.VARIABLE_SPACE.BLOCK1+$CB 	;$1byt.
TWF.PRINTED_LINE.COUNTER	.EQ SHARED.VARIABLE_SPACE.BLOCK1+$CC 	;$1byt. Tracks the number of lines printed to the text screen. Used to determine when the <ANY KEY> prompt should be displayed.


@END	
	
;============TILE INFORMATION=============
;****See beginning of file


;============TIME & EVENTS=============
@START
TIME.SUN.STATUS				.BS $1		;$00 = SUN RISING, $01 = DAY, $02 = SUN SETTING, $03 = NIGHT
TIME.SUN.COUNTER			.BS $1		;$00-$04, COUNTS DOWN FROM $04 FOR SUNSET, COUNTS UP FROM $00 FOR SUNRISE. EACH VALUE IS A PHASE OF THE SUNSET/SUNRISE
TIME.SUN.SUB_COUNTER		.BS $1		;COUNTS THE MOVES BETWEEN INCREMENTS TO TIME.SUN.COUNTER
TIME.SUN.SUB_COUNTER.STOP	.EQ $02		;#CONSTANT. MOVES BETWEEN INCREMENTS OF TIME.SUN.COUNTER (PHASES OF SUNSET AND SUNRISE)

TIME.CURRENT.HOUR			.BS $1		;!BCD!. 24-hour clock. The current game time, hours. 
TIME.CURRENT.MINUTE			.BS $1		;!BCD!. 24-hour clock. The current game time, minutes. 
TIME.MOVES.PER_MINUTE		.EQ	$02		;#CONSTANT. The number of moves that must occur for the game clock to advance 1 minute
TIME.MOVES.COUNTER			.BS $1		;Tracks the number of moves that have occured since the game clock was incremented by 1 minute.

TIME.DISPLAY.HOUR			.BS $1		;!BCD!. 12-hour clock. The current game time, hours. 
TIME.DISPLAY.MINUTE			.BS $1		;!BCD!. 12-hour clock. The current game time, minutes. 
TIME.DISPLAY.AM_PM			.BS $1		;$HEX$. $00=AM, $01=PM

EVENT.SUNRISE.HOUR 			.EQ $05			;#CONSTANT. The time that sunrise occurs.
EVENT.SUNRISE.MINUTE		.EQ	$30			;#CONSTANT. ""
EVENT.SUNSET.HOUR 			.EQ $20			;#CONSTANT. The time that sunset occurs.
EVENT.SUNSET.MINUTE			.EQ	$30			;#CONSTANT. ""

;game state flags
EVENT.FLAGS					.BS $100	;$100byt. Stores boolean flags ($01 = on, $00 off) that trigger certain events, allow/dissallow certain things, or otherwise dynamically affect gameplay 



;EVENT CONSTANTS
EVENT.UNDEAD_LORD.FIRE_DROP.PROB	.EQ $20	;if random # is less than this value, fire is dropped. Roughly 15% chance if random number generator was working correctly. My intention is for it to be more like 50% chance or 25% chance.  

@END

;========ZONE OPERATIONS==========
@START
;ARRAYS, POINTERS, AND SUCH
@START

	;.NO $9000	
;RZONE.ARRAY							.BS $900,$42			;STORES UNCOMPRESSED ZONE DATA FOR THE REGIONAL MAP IN MAIN MEMORY
RZONE.ARRAY							.EQ $A000			;STORES UNCOMPRESSED ZONE DATA FOR THE REGIONAL MAP IN MAIN MEMORY

;==========KEEP THESE TWO TOGETHER, IN THIS ORDER====
;(because some routines use these two buffers as a contiguous 2 page block of memory)
;
;	$9900 (ONLY WHEN ABOVE .NO IS ACTIVE)


ZONE_TOOLS.OUTPUT_BUFFER	.EQ SHARED.VARIABLE_SPACE.BLOCK1  ;$100bytes (old definition .BS	$100,$42)			;temporary buffer for output from zone_tools.uncompress.single. **Shared**
   ;set below back to output
NEW.MAP				.EQ ZONE_TOOLS.OUTPUT_BUFFER
MAP.DATA.DEST_ADDR  .EQ ZONE_TOOLS.OUTPUT_BUFFER
MAP.DATA.PASS1		.EQ ZONE_TOOLS.OUTPUT_BUFFER
	;**OPT** Memory. change to a pointer to the RWTS buffer.
MAP.DATA.PASS2		.EQ SHARED.VARIABLE_SPACE.BLOCK1+$80
	;$9A00 (ONLY WHEN ABOVE .NO IS ACTIVE)
ZONE_TOOLS.INPUT_BUFFER	.EQ SHARED.VARIABLE_SPACE.BLOCK2 ;$101bytes (old definition .BS $101,$42)			;TEMPORARY BUFFER WHERE COMPRESSED ZONE DATA IS COPIED FROM AUX MEMORY BEFORE UNCOMPRESSING IT
 ;set below back to input
NEW.SHAPES			.EQ ZONE_TOOLS.INPUT_BUFFER
;NOTE: INPUT BUFFER IS $101 BYTES INSTEAD OF $100 BECAUSE THE ZONE.LOOKUP.LO/HO LOOKUP TABLE CONTAIN THE START ADDRESS OF EACH ZONE, WHICH IS USED AS THE START AND END ADDRESS IN AN AUX MOVE (I.E. THE START OF THE NEXT ZONE IS USED AS THE END ADDRESS OF THE CURRENT ZONE). AS A RESULT, AUX MOVE WRITES AN EXTRA BYTE TO INPUT BUFFER, AND WHICH WOULD CAUSE AN OVERFLOW FOR UNCOMPRESSED ZONES. THE EXTRA BYTE IN THE .BS $101 ACCOMIDATES THIS SO THE AUX MOVE DOESN'T OVERFLOW INTO THE LOOKUP TABLE. 
;=====================================


;==========KEEP THESE TWO TOGETHER, IN THIS ORDER====
;WARNING: DO NOT PUT THESE IN VARIABLES.BIN OR THEY WILL GET CLOBBERED
	;$9B01 (ONLY WHEN ABOVE .NO IS ACTIVE)
	;
	;***NOTE THE EXTRA BYTE IN THE START ADDRESS DUE TO EXTRA BYTE AT END OF LAST VARIABLE
ZONE.LOOKUP.LO								.BS	$41,$42				;CONTAINS THE AUX LO ADDRESS FOR EACH WORLD ZONE.
ZONE.LOOKUP.HO								.BS	$41,$42				;CONTAINS THE AUX HO ADDRESS FOR EACH WORLD ZONE. 
;=====================================




ZONE_TOOLS.INPUT					.EQ	$EA
ZONE_TOOLS.INPUT.INDEX				.EQ	SAVED.XREG.LOCAL
ZONE_TOOLS.OUTPUT.INDEX				.EQ	SAVED.XREG.LOCAL1	

@END

;========COMPRESS/UNCOMPRESS============
@START
ZONE_TOOLS.TILE_LAST 				.BS $1	

	;manually set these flags ($00=off, $01=on) to disable/enable compression on each world zone
	;Note: These flags must be set in two locations, here and in map.compression.ps1 (top of file)											
WZONE.COMPRESSION.FLAGS				.HS		01.01.01.01.01.01.01.01.01.01.01.00.01.01.01.01.01.00.01.01.01.01.01.01.01.00.00.01.01.01.01.01.01.01.01.00.01.01.01.01.01.01.01.01.01.00.01.01.01.01.01.01.01.01.01.00.01.01.01.01.01.01.01.01
	;WZONE (!DEC) #							0	1  2  3	 4	5  6  7	 8	9 10 11	12 13 14 15	16 17 18 19	20 21 22 23	24 25 26 27	28 29 30 31	32 33 34 35	36 37 38 39	40 41 42 43	44 45 46 47	48 49 50 51	52 53 54 55	56 57 58 59	60 61 62 63
;this one is for the file size estimate map
;WZONE.COMPRESSION.FLAGS					.HS	01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.01.00.01.01.01.01.00.01.01.00.00.01.00.01.00.00.01.01.01.01.01.01.01.01.01.00.01.00.00.01.01.01.01.00.00.01.01.01.00.01.01.01.01.01.01.01.01.01




;WORLD ZONES
WORLD.COMPRESS.BUFFER_ADDRESS.START	.BS $2		;shared
WORLD.COMPRESS.BUFFER_ADDRESS.END	.BS $2		;shared		
WORLD.COMPRESS.ZONE_INPUT.ADDRESS	.BS	$2		;shared
WORLD.COMPRESS.ZONE_OUTPUT.ADDRESS	.BS $2
WORLD.COMPRESS.AUX_MEMORY.START_LO	.EQ	$00		;#CONSTANT. The starting location in aux memory where compressed zone data is stored	
WORLD.COMPRESS.AUX_MEMORY.START_HO	.EQ	$02		;#CONSTANT. The starting location in aux memory where compressed zone data is stored	


WZONE.WIDTH							.EQ $10		;#CONSTANT. The width/depth of a world zone. Not used in compression, but this was the logical place to put it. Used in CONVERT.GMAP_XY.RMAP_XY 
WZONE.TOTAL.EXTENDED				.EQ $81		;#CONSTANT. TOTAL NUMBER OF ZONES ON THE WORLD MAP *2
WZONE.TOTAL.PLUSONE					.EQ $41		;#CONSTANT. There are 64 zones ($40), so +1 is $41. Oops. Counting zero, there are $3F.

;NOTE: I THINK WZONE.TOTAL SHOULD BE $40..THE LOOP THAT USES IT INCREMENTS INDEX BEFORE THE EXIT CHECK, SO THE -1 DOESN'T APPLY. AND THE $39 IS A HEX/DEC TYPO. 
WZONE.TOTAL2						.EQ $40		;#CONSTANT. These WZONE.TOTAL constants need to get cleaned up. 
WZONE.TOTAL							.EQ $39		;#CONSTANT.	TOTAL NUMBER OF ZONES ON THE WORLD MAP -1.
;********READ NOTE BEFORE USING THIS CONSTANT*****

;NOTE: also check end of loader.bin file, there are temporary variables defined there which are part of the loading process

WZONE.UNCOMPRESS.START 				.BS $1		;STARTING WORLD ZONE WHEN UNCOMPRESSING DATA INTO THE REGIONAL MAP
WZONE.UNCOMPRESS.CURRENT			.BS $1		;CURRENT WORLD ZONE WHEN UNCOMPRESSING DATA INTO THE REGIONAL MAP
WZONE.UNCOMPRESS.ROW				.BS $1		;USED TO RESET THE CURRENT WORLD ZONE AT THE START OF EACH ROW. shared
WZONE.OFFSET						.EQ $08		;#CONSTANT. ZONE NUMBER OFFSET FOR MOVING UP OR DOWN BY 1 WORLD ZONE. 
WZONE.OFFSET2						.EQ	$10		;#CONSTANT. WZONE.OFFSET X 2

;REGIONAL ZONES
RMAP.WIDTH							.EQ $2F		;#CONSTANT. The width/depth of the regional map (cotaining 3 zones). Not used in compression, but this was the logical place to put it. Used in CONVERT.RMAP_XY.GMAP_XY
RZONE.WIDTH							.EQ WZONE.WIDTH
RZONE.TOTAL							.EQ $09 	;#CONSTANT. Total number of zones in the regional map
RZONE.OFFSET						.EQ $03		;#CONSTANT. ZONE NUMBER OFFSET FOR MOVING UP OR DOWN BY 1 REGION ZONE. 
RZONE.ARRAY.ZONE.HORIZONTAL			.EQ $10		;#CONSTANT. NUMBER OF TILES BETWEEN ZONES, LEFT/RIGHT
RZONE.ARRAY.ZONE.VERTICLE			.EQ $300	;#CONSTANT. NUMBER OF TILES BETWEEN ZONES, UP/DOWN

RZONE.ARRAY.OFFSET					.EQ $30		;#CONSTANT. NUMBER OF TILES BETWEEN ROWS, UP/DOWN. 
RZONE.ARRAY.OFFSET.HORIZONTAL		.EQ $01		;#CONSTANT. NUMBER OF TILES BETWEEN ROWS, LEFT/RIGHT.
RZONE.ARRAY.INDEX_ROW				.EQ $FC		;USED FOR ITERATING THROUGH THE ARRAY WHEN UNCOMPRESSING ZONE DATA
RZONE.ARRAY.INDEX.SCROLL_FROM		.EQ $EA		;USED FOR SCOLLING ZONE DATA WHEN LOAD THREASHOLD IS REACHED 
RZONE.ARRAY.INDEX.SCROLL_TO			.EQ $EC		;USED FOR SCOLLING ZONE DATA WHEN LOAD THREASHOLD IS REACHED 


RZONE.ARRAY.STOP					.BS $2		;THE 16-BIT STOP VALUE FOR ITERATING THE ARRAY, WHICH IS DIFFERNT FOR EACH ZONE. 
RZONE.UNCOMPRESS.CURRENT 			.BS $1		;THE CURRENT REGIONAL ZONE NUMBER BEING PROCESSED

;RZONE #							 0	1  2  3	 4	5  6  7	 8											
RZONE.LOOKUP.ARRAY_START.LO		.HS 00.10.20.00.10.20.00.10.20
RZONE.LOOKUP.ARRAY_START.HO		.HS	00.00.00.03.03.03.06.06.06

;the stop value isn't the last array element of the zone, it is the first element of the last row. 											
RZONE.LOOKUP.ARRAY_STOP.LO		.HS	D0.E0.F0.D0.E0.F0.D0.E0.F0
RZONE.LOOKUP.ARRAY_STOP.HO		.HS	02.02.02.05.05.05.08.08.08


;ZONE LOOKUP TABLE CALCULATION

;AUX.READ_ADDRESS.START			;already defined in Routines_Disk.ASM ;2byte

;AUX.READ_ADDRESS.END			.EQ WORLD.COMPRESS.BUFFER_ADDRESS.START  ;1byte
AUX.READ_ADDRESS.END			.BS $2

;MAP.DATA						.EQ WORLD.COMPRESS.BUFFER_ADDRESS.END	;2 byte
MAP.DATA						.BS $2

;ZONE.START.BASE_ADDRESS			.EQ WORLD.COMPRESS.ZONE_INPUT.ADDRESS	;2 byte	
ZONE.START.BASE_ADDRESS			.BS $2
	
;COUNTER.ADJ					.EQ WZONE.UNCOMPRESS.START				;1 byte
COUNTER.ADJ					.BS $1

;ZONE.COUNTER				.EQ WZONE.UNCOMPRESS.CURRENT	;1byte
PRIOR.ZONE.INDEX			.BS $1

;XREG.RESUME				.EQ WZONE.UNCOMPRESS.ROW ;USED TO CARRYOVER THE COMPRESSED TILE RECORD INDEX IF AN UNCOMPRESSED ZONE IS FOUND
XREG.RESUME					.BS $1 ;USED TO CARRYOVER THE COMPRESSED TILE RECORD INDEX IF AN UNCOMPRESSED ZONE IS FOUND

@END


;WORLD EDGE
WORLD.EDGE.NORTH					.EQ $08				;#CONSTANT. The trigger for when zones are scrolled and 3 new zones are loaded.
WORLD.EDGE.SOUTH					.EQ $76				;#CONSTANT. The trigger for when zones are scrolled and 3 new zones are loaded.
WORLD.EDGE.EAST						.EQ $77				;#CONSTANT. The trigger for when zones are scrolled and 3 new zones are loaded.
WORLD.EDGE.WEST						.EQ $09				;#CONSTANT. The trigger for when zones are scrolled and 3 new zones are loaded.



;=========ZONE TRANSITION===========
@START
RZONE.LOAD_THREASHOLD.NORTH			.EQ $07				;#CONSTANT. The trigger for when zones are scrolled and 3 new zones are loaded. 
RZONE.LOAD_THREASHOLD.SOUTH			.EQ $27				;#CONSTANT. The trigger for when zones are scrolled and 3 new zones are loaded. 
RZONE.LOAD_THREASHOLD.EAST			.EQ $28				;#CONSTANT. The trigger for when zones are scrolled and 3 new zones are loaded. 
RZONE.LOAD_THREASHOLD.WEST			.EQ $08				;#CONSTANT. The trigger for when zones are scrolled and 3 new zones are loaded. 

;RZONE.LOAD_THREASHOLD				.EQ $10				;#CONSTANT. The trigger for when zones are scrolled and 3 new zones are loaded. 
RZONE.LOAD.OFFSET.UP				.EQ $300			;#CONSTANT. The amount by which players RMAP is adjusted after new zones are loaded, when player is moving north
RZONE.LOAD.OFFSET.DOWN				.EQ $300			;#CONSTANT. The amount by which players RMAP is adjusted after new zones are loaded, when player is moving south
RZONE.LOAD.OFFSET.LEFT				.EQ $10				;#CONSTANT. The amount by which players RMAP is adjusted after new zones are loaded, when player is moving east
RZONE.LOAD.OFFSET.RIGHT				.EQ $10				;#CONSTANT. The amount by which players RMAP is adjusted after new zones are loaded, when player is moving west

RZONE.LOAD.X.START					.EQ $18				;THE STARTING X AXIS POSITION AFTER A ZONE TRANSITION
RZONE.LOAD.Y.START					.EQ $17				;THE STARTING Y AXIS POSITION AFTER A ZONE TRANSITION

RZONE.SOUTH.TABLE_TO.LO 		.HS					00.00
RZONE.SOUTH.TABLE_TO.HO 		.HS					00.03
RZONE.SOUTH.TABLE_FROM.LO 		.HS					00.00
RZONE.SOUTH.TABLE_FROM.HO  		.HS					03.06

RZONE.EAST.TABLE_TO.LO 			.HS					00.00.00.10.10.10
RZONE.EAST.TABLE_FROM.LO 		.HS					10.10.10.20.20.20

RZONE.WEST.TABLE_TO.LO 			.HS					20.20.20.10.10.10
RZONE.EAST_WEST.TABLE_TO.HO		.HS					00.03.06.00.03.06

RZONE.WEST.TABLE_FROM.LO 		.HS					10.10.10.00.00.00
RZONE.EAST_WEST.TABLE_FROM.HO  	.HS					00.03.06.00.03.06

RZONE.TRANSITION.TOP.HO				.EQ	$0000			;#CONSTANT. The HO byte of the start address of the top row of zones 	(0,1,2) on the regional map. Used in scrolling zones. 
RZONE.TRANSITION.MIDDLE.HO			.EQ	$0300			;#CONSTANT. The HO byte of the start address of the middle row of zones (3,4,5) on the regional map. Used in scrolling zones. 
RZONE.TRANSITION.BOTTOM.HO			.EQ	$0600			;#CONSTANT. The HO byte of the start address of the bottom row of zones (6,7,8) on the regional map. Used in scrolling zones. 



;START AND END RMAP ADDRESSES OF THE REGIONAL ZONES
RZONE.ARRAY.Z0.START				.EQ $00				;#CONSTANT
RZONE.ARRAY.Z0.END					.EQ $2DF			;#CONSTANT

RZONE.ARRAY.Z1.START				.EQ $10				;#CONSTANT
RZONE.ARRAY.Z1.END					.EQ $2EF			;#CONSTANT

RZONE.ARRAY.Z2.START				.EQ $20				;#CONSTANT
RZONE.ARRAY.Z2.END					.EQ $2FF			;#CONSTANT

RZONE.ARRAY.Z3.START				.EQ $300			;#CONSTANT
RZONE.ARRAY.Z3.END					.EQ $5DF			;#CONSTANT

RZONE.ARRAY.Z4.START				.EQ $310			;#CONSTANT
RZONE.ARRAY.Z4.END					.EQ $5EF			;#CONSTANT

RZONE.ARRAY.Z5.START				.EQ $320			;#CONSTANT
RZONE.ARRAY.Z5.END					.EQ $5FF			;#CONSTANT

RZONE.ARRAY.Z6.START				.EQ $600			;#CONSTANT
RZONE.ARRAY.Z6.END					.EQ $8DF			;#CONSTANT

RZONE.ARRAY.Z7.START				.EQ $610			;#CONSTANT
RZONE.ARRAY.Z7.END					.EQ $8EF			;#CONSTANT

RZONE.ARRAY.Z8.START				.EQ $620			;#CONSTANT
RZONE.ARRAY.Z8.END					.EQ $8FF			;#CONSTANT

@END
@END


;========SYSTEM VARIABLES/LABELS=======
AUX.READ_ADDRESS.START 			.BS $2

DEBUG.LOG						.EQ $3000 ;***HARD CODED REFERENCE**

STACK_POINTER.SAVED 			.BS $1
JMP.DESTINATION.ADDR			.EQ $B4


;RANDOM.NUMBER.SEED.COUNTER	.BS $1

;TEMPCOUNTER		.BS $1

;========MISC=======
@START

TROUBLESHOOTING.HOOK			.BS $1				;$01 = HOOK SET. CODE CAN BE INSERTED IN A ROUTINE TO BREAK IF THE HOOK IS SET.
TROUBLESHOOTING.HOOK2			.BS $1				;$01 = HOOK SET. CODE CAN BE INSERTED IN A ROUTINE TO BREAK IF THE HOOK IS SET.

CALLED_BY.DRAW.SCREEN			.BS	$1				;1byt			IF SET TO $01, INDICATES THAT DRAW.SCREEN IS THE CALLING ROUTINE. THIS VARAIBLES IS SEPERATE BECAUSE FLOW CONTROL DECISIONS ARE MADE BASED ON IT. 
CALLED_BY.DRAW.TILE.PLAYER.STANDARD_ICON			.BS	$1				;1byt			IF SET TO $01, INDICATES THAT .STANDARD_ICON IN DRAW.TILE.PLAYER IS THE CALLING ROUTINE. THIS VARAIBLES IS SEPERATE BECAUSE FLOW CONTROL DECISIONS ARE MADE BASED ON IT. 
CALLED_BY						.BS $1				;1byt			USED TO TRACE THE CALLING ROUTINE. $00 = NOT SET
		;CALLED_BY CODE CHART
		;$00 = NOT SET  (KNOWN LOCATIONS WHERE IT'S NOT SET:  DRAW.TILE.PLAYER)
		;$01 = NOT USED (TO AVOID CONFUSION WITH DRAW.SCREEN, WHICH USES CALLED_BY.DRAW.SCREEN)
		;$02 = MOVE.PASS
		;$03 = MOVE.NORTH
		;$04 = MOVE.SOUTH
		;$05 = MOVE.EAST 
		;$06 = MOVE.WEST
		;$07 = TRANSPORT.ENTRANCE
		;$08 = .MOB.MOVE.ERASETILE
		;$09 = .MOB.MOVE.ERASETILE.MT
		;$0A = .MOB.DRAWTILE.ENTRANCE2   1ST INSTANCE
		;$0B = .MOB.DRAWTILE.ENTRANCE2   2ND INSTANCE
		;$0C = MOB.DRAWTILE.MT
		;$0D = DRAW.MISC
		;$0E = DRAWTILE
		;$0F = TRANSPORT.DRAWTILE.MT

@END
		
;========TEMPORARY=======
;PLAYER_ICON		.HS AA.D5.AA.D5.CA.D4.AA.D5.A8.C5.A8.C5.A2.D1.8A.D4.8A.D4.8A.D4.8A.D4.A2.D1.A2.D1.A2.D1.A2.D1.A8.C5

;TEST.ITERATION.COUNTER .BS $1

;test.cow .az -/cow/

COW  .BS $1
COW2 .BS $6
FILE.OPEN.INVENTORY.DEBUG_COUNTER .BS $1

;DEBUG.TRAP1 .BS $1


;see game_loop.asm, at the end of the file				

				
	;.NO $2000 	;advance program counter to this address, backfill $00




				








