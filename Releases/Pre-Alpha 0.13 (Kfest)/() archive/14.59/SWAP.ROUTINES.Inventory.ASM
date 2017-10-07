;************************INCLUDE FILE*****************************
;(do not assemble stand-alone.  )
				.TF     swap.routines.inventory.bin,BIN
				;.OR		$9600			**Always put before .TF directive and never use again in program
				.OR		$0000			**Always put before .TF directive and never use again in program
				.EB		OFF

				

;=====SWAP.ROUTINES.INVENTORY.ASM DOCUMENTATION====================================
;
;The inventory file contains the inventory module source code and also
;player and game inventory/item related data. 
;
;--DATA--
;CHR_SHEET.PC.READIED_EQUIP.START
;INV.PLAYER.INVENTORY.DATA.START
;INV.ITEM_TABLES.START
;
;--INVENTORY SUB-MODULES
;INV.CALCULATE.COMBAT.STATS.START
;
;Each inventory sub-module is setup in it's own patch
;section to match up to the memory addresses that it is loaded to.
;Each patch section has labels for the start/end of the section
;both inside and outside of the patch section. 
;
;For example, the start label outside the patch section is used in conjunction with the
;label for the start of this file to calculate the seek bytes for each 
;sub-module. For the seek/read offsets see ;==INVENTORY FILE OFFSETS==
;
;
;
;
;==Inventory Manager launched from combat(tenative)==
;When the player launches the inventory manager, the entire combat module is swapped out to aux memory
;and when the player exists the inventory manager, the combat module is swapped back in. 
;
;
;===ADDING A SUB_MODULE====
;
;expand INV.SUB_MODULE.LOAD_FLAGS unless the new sub_module already has a load flag assigned
;add a patch section below. The .PH should be set to the patch end label of the patch section above it. 
;		if inserting a new patch section between two existing, make sure to change the .PH of the exiting patch below the new section to be the end lable of the new patch section. 
;add to INV.FILE.OFFSETS
;add to INV.SUB_MODULE.FILE.PARAMETERS
;
;
;===MODULE MEMORY MAP===  (OUT OF DATE)
;$9600-$BFFF	main inventory program
;$BA00-BFFF*	Map Object Arrays (npc schedule, mob, general, and npc) ;I'm not sure yet if these are required to stay in memory while the inventory module
;				is loaded. If not they would need to be loaded again from disk of course. 
;	;$BD00-BFFF*	Mob, General and NPC map object arrays 
;
;
;=================================================================================



SRTN.INVENTORY.FILE.START
;LOCAL POINTERS/CONSTANTS
@START
INV.MAIN_MODULE.END						.EQ $B9FF
INV.PLAYER.INVENTORY.DATA				.EQ INV.MAIN_MODULE.END+1 ;This is the main inventory data array pointer

;INV.PLAYER.INVENTORY.DATA.PATCH_END			.EQ $BFFF ;#CONSTANT. It's really END+1
INV.PLAYER.INVENTORY.DATA.RECORD_SIZE	.EQ $06 ;#CONSTANT
INV.PLAYER.INVENTORY.DATA.LESS_ONE_RECORD	.EQ INV.PLAYER.INVENTORY.DATA-INV.PLAYER.INVENTORY.DATA.RECORD_SIZE

@END



;GAME INVENTORY FILE DATA				
				.IN 	c:\my_code\includes_libs\data.game.inventory.ASM
				
INV.MODULE.START
;*****polish these up a bit and add to the file docs
;
;(done) ;***inventory mgr rule: check STR/DEX/INT gates before permitting item to be readied
;***inventory mgr rule: only allow ready of a 2HD weapon to a given hand if the other hand has hands (first) readied. (i.e. the hand is empty)
;***inventory mgr rule: if a hand has a 2HD weapon readied, set other hand to item_type $03 (IN USE for 2HD Weapon)
;***inventory mgr rule: when weapon/armor is unreadied, set body part to empty hands/skin. fists should be in the players inventory automatically (weapon_ID $00) which has to be readied. Skin is item_ID $00 in the armor table. Fists get an attack turn, empty hands (item_type $03) do not. 
;***inventory mgr rule: shields are straight forward. set the item_type for that hand to armor and the item_ID to the ID for the shield. it won't be given an attack turn because it is not type weapon. This would get handled by the ready armor routine assuming there are separate routines 
		;for ready armor and ready weapons. Armor screen should probably list hands with room for two items (a hand covering and hand-held armor like a shield). It may be useful for shields to be setup in specific range so they can be detected when readying as they are the only armor that would get readied to a hand. the hand coverings have differnet fields. 
		;in fact, the entire armor range needs to be subdivided in advance so the ready code can tell which field to assign the armor to (i.e head, torso, etc). Shiels will just be one of these sub-divisions  
;		
;***inventory mgr rule
	;(when weapon/armor is unreadied, set body part 
	;to empty hands/skin. fists should be in the players 
	;inventory automatically (weapon_ID $00) which has to be readied. 
	;Skin is item_ID $00 in the armor table. Fists get an attack turn, empty hands (item_type $03) do not.) 
;
;Update 5/14/2017
	;the armor and misc items table will have a body part field, the contents of which indicate
	;if the item is unreadiable, what body part can ready it (hands collectively are one body part), and if a weapon whether it is 1/2 handed
	;
	;the ready equipment routine logic is this:
	;	*assume any item from the weapons table should be readied to hands
	;			*If the body part is empty then ready the item. 
	;			*If the body part is not empty then ready the item and toggle the other item's readied flag (player inv data file) to unreadied. 
	;				*Use body part field to determine 1/2 handed weapon
	;				*2hd weapons should bump items in both hands. 
	;				*Readying a 2nd 1hd weapon should bump a shield if one is readied.
	;				*Goal is for there not to be any corner cases that require an onscreen "problem/instruction" message
	;
	;	*if an item from the armor or misc items table is readied, then check the body part specified. 
	;			*If the body part is empty then ready the item. 
	;			*If the body part is not empty then ready the item and toggle the other item's readied flag (player inv data file) to unreadied. 
	;	
	;shields are implicitly addressed by this approach because they would be in the armor table and have a body part ID of hands
	


;==========================================================
INV.ENTRANCE.START
	.PH SWAP_SPACE.MAIN_MEMORY
INV.ENTRANCE.PATCH_START
	
			.IN 	c:\my_code\includes_libs\swap.routines.inv.entrance_exit.asm
	
INV.ENTRANCE.PATCH_END
	.EP ;end patch mode
INV.ENTRANCE.END

;==========================================================
;

;
;==========================================================
INV_0.STATS_SUMMARY.START	;screen0		
	.PH INV.ENTRANCE.PATCH_END ;start patch mode
INV_0.STATS_SUMMARY.PATCH_START	
	

	.IN 	c:\my_code\includes_libs\swap.routines.inv.stats_summary.asm


INV_0.STATS_SUMMARY.PATCH_END
	.EP ;end patch mode
INV_0.STATS_SUMMARY.END	
;==========================================================
;

;
;==========================================================
INV_1.READIED_EQUIPMENT.START	;screen1		
	.PH INV_0.STATS_SUMMARY.PATCH_END ;start patch mode
INV_1.READIED_EQUIPMENT.PATCH_START	
	

	.IN 	c:\my_code\includes_libs\swap.routines.inv.readied_equipment.asm


INV_1.READIED_EQUIPMENT.PATCH_END
	.EP ;end patch mode
INV_1.READIED_EQUIPMENT.END	
;==========================================================
;

;
;==========================================================
INV_2.COMBAT_STATS.START	;screen2		
	.PH INV_1.READIED_EQUIPMENT.PATCH_END ;start patch mode
INV_2.COMBAT_STATS.PATCH_START	
	

	.IN 	c:\my_code\includes_libs\swap.routines.inv.combat_stats.asm


INV_2.COMBAT_STATS.PATCH_END
	.EP ;end patch mode
INV_2.COMBAT_STATS.END	
;==========================================================
;

;
;==========================================================
INV_3.SKILLS.START	;screen3		
	.PH INV_2.COMBAT_STATS.PATCH_END ;start patch mode
INV_3.SKILLS.PATCH_START	
	

	.IN 	c:\my_code\includes_libs\swap.routines.inv.skills.asm


INV_3.SKILLS.PATCH_END
	.EP ;end patch mode
INV_3.SKILLS.END	
;==========================================================
;

;
;==========================================================
INV_4.DISPLAY_INVENTORY.START
	.PH INV_3.SKILLS.PATCH_END
INV_4.DISPLAY_INVENTORY.PATCH_START
	
			.IN 	c:\my_code\includes_libs\swap.routines.inv.display_inventory.ASM

INV_4.DISPLAY_INVENTORY.PATCH_END
	.EP ;end patch mode
INV_4.DISPLAY_INVENTORY.END
;==========================================================
;

;
;==========================================================
INV_6.READY_UNREADY.START
	.PH INV_4.DISPLAY_INVENTORY.PATCH_END
INV_6.READY_UNREADY.PATCH_START

			.IN 	c:\my_code\includes_libs\swap.routines.inv.ready_unready.ASM

INV_6.READY_UNREADY.PATCH_END
	.EP ;end patch mode
INV_6.READY_UNREADY.END
;==========================================================
;

;
;==========================================================
INV_7.CALCULATE.COMBAT.STATS.START
	.PH INV_6.READY_UNREADY.PATCH_END
INV_7.CALCULATE.COMBAT.STATS.PATCH_START
	
			.IN 	c:\my_code\includes_libs\swap.routines.inv.calc.combat.stats.ASM

INV_7.CALCULATE.COMBAT.STATS.PATCH_END
	.EP ;end patch mode
INV_7.CALCULATE.COMBAT.STATS.END
;==========================================================
;



INV.FILLER.START
	.PH INV_7.CALCULATE.COMBAT.STATS.PATCH_END ;start patch mode
	
	; .NO $B900

; INV.DEBUG.LOG .BS $100
; INV.DEBUG.LOG.INDEX	.EQ INV.DEBUG.LOG+$FF

INV.DEBUG.LOG .EQ EVENT.FLAGS
INV.DEBUG.LOG.INDEX	.EQ INV.DEBUG.LOG+$FF


INV.FILLER.PATCH_START

	.NO INV.MAIN_MODULE.END+1 ;**OPT** Speed. Disk. optimize the .NO to the actual space needed. 
							 ;!!!WARNING: lowering this address would mess up the order of any inventory test data
							 ;that follows the .NO. If there is no inventory test data then it lowering the .NO address should be fine.


	 ; .NO INV.MAIN_MODULE.END-1 ;**OPT** Speed. Disk. optimize the .NO to the actual space needed. 
	 ; ;!!!WARNING: lowering this address would mess up the order of any inventory test data
	 ; ;that follows the .NO. If there is no inventory test data then it lowering the .NO address should be fine.

	 
	;*****moved because this part of the inventory file is never loaded into memory. 
	; ;write the inventory array length to the source file
	; .DA #INV.PLAYER.INVENTORY.TEST_DATA.BYTES.USED  
	; .DA /INV.PLAYER.INVENTORY.TEST_DATA.BYTES.USED
							 
INV.FILLER.PATCH_END
	.EP ;end patch mode
INV.FILLER.END


;INV.PLAYER.INVENTORY.DATA.START (destination read location)
;($BA00 - $BF00), !256 items max
		 

;==INVENTORY FILE OFFSETS==
@START
INV.FILE.OFFSETS

INV.ENTRANCE.SEEK_BYTES = INV.ENTRANCE.START-SRTN.INVENTORY.FILE.START
INV.ENTRANCE.READ_BYTES = INV.ENTRANCE.END-INV.ENTRANCE.START

INV_0.STATS_SUMMARY.SEEK_BYTES = INV_0.STATS_SUMMARY.START-SRTN.INVENTORY.FILE.START
INV_0.STATS_SUMMARY.READ_BYTES = INV_0.STATS_SUMMARY.END-INV_0.STATS_SUMMARY.START

INV_1.READIED_EQUIPMENT.SEEK_BYTES = INV_1.READIED_EQUIPMENT.START-SRTN.INVENTORY.FILE.START
INV_1.READIED_EQUIPMENT.READ_BYTES = INV_1.READIED_EQUIPMENT.END-INV_1.READIED_EQUIPMENT.START

INV_2.COMBAT_STATS.SEEK_BYTES = INV_2.COMBAT_STATS.START-SRTN.INVENTORY.FILE.START
INV_2.COMBAT_STATS.READ_BYTES = INV_2.COMBAT_STATS.END-INV_2.COMBAT_STATS.START

INV_3.SKILLS.SEEK_BYTES = INV_3.SKILLS.START-SRTN.INVENTORY.FILE.START
INV_3.SKILLS.READ_BYTES = INV_3.SKILLS.END-INV_3.SKILLS.START

INV_4.DISPLAY_INVENTORY.SEEK_BYTES = INV_4.DISPLAY_INVENTORY.START-SRTN.INVENTORY.FILE.START
INV_4.DISPLAY_INVENTORY.READ_BYTES = INV_4.DISPLAY_INVENTORY.END-INV_4.DISPLAY_INVENTORY.START

INV_6.READY_UNREADY.SEEK_BYTES = INV_6.READY_UNREADY.START-SRTN.INVENTORY.FILE.START
INV_6.READY_UNREADY.READ_BYTES = INV_6.READY_UNREADY.END-INV_6.READY_UNREADY.START

INV_7.CALCULATE.COMBAT.STATS.SEEK_BYTES = INV_7.CALCULATE.COMBAT.STATS.START-SRTN.INVENTORY.FILE.START
INV_7.CALCULATE.COMBAT.STATS.READ_BYTES = INV_7.CALCULATE.COMBAT.STATS.END-INV_7.CALCULATE.COMBAT.STATS.START
	




