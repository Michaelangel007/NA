REM ============================================
REM Copyright (C) 2016-2018. 6502 Workshop, LLC
REM contact: mark@6502workshop.com
REM ============================================


REM ===================GO2b.BAT========================================================
REM ========FUNCTION: backup code and build disk image=====
REM ========(use after go1.bat)
REM ===================================================================================
REM
REM Make Backup Copy of Code Iteration
set Hour=%Time:~0,2%
if "%hour:~0,1%" == " " set hour=0%hour:~1,1%
set Min=%Time:~3,2%

mkdir c:\my_code\backups\%hour%.%min%
mkdir c:\my_code\backups\%hour%.%min%\compressed_data
mkdir c:\my_code\backups\%hour%.%min%\compressed_data\npc.speech.text\


cd\my_code\na\build_folder
copy c:\my_code\na\source_code\*.* c:\my_code\backups\%hour%.%min%
copy c:\my_code\na\build_folder\list.lst c:\my_code\backups\%hour%.%min%
copy c:\my_code\na\source_code\compressed_data\*.* c:\my_code\backups\%hour%.%min%\compressed_data
copy c:\my_code\na\source_code\compressed_data\npc.speech.text\*.* c:\my_code\backups\%hour%.%min%\compressed_data\npc.speech.text\

copy c:\my_code\na\map_shapes\map\*.xlsx c:\my_code\backups\%hour%.%min%
copy c:\my_code\na\Binaries.prebuilt\NOXARCH.SYSTEM.BIN c:\my_code\backups\%hour%.%min%
copy c:\my_code\na\Binaries.prebuilt\NOXARCH.SYSTEM.PROTECTED.BIN c:\my_code\backups\%hour%.%min%
copy c:\my_code\na\compression\world.map\map.compression.* c:\my_code\backups\%hour%.%min%
copy c:\my_code\na\COMPRESSION\NPC.SPEECH.TEXT\*.bas c:\my_code\backups\%hour%.%min%

copy c:\qb64\*.bat c:\my_code\backups\%hour%.%min%
copy c:\sbasm3\*.bat c:\my_code\backups\%hour%.%min%
copy c:\my_code\na\disk_image_masters\*.* c:\my_code\backups\%hour%.%min%






@ECHO ON

REM TESTING FILES
copy DUMMY.FILE.BIN c:\ac1.3.5
del DUMMY.FILE.BIN

REM BOOT FILES
copy c:\my_code\na\Binaries.prebuilt\NOXARCH.SYSTEM.BIN c:\ac1.3.5

copy NOXARCH.MAIN.BIN c:\ac1.3.5
del NOXARCH.MAIN.BIN

copy LOADER.P.BIN c:\ac1.3.5
del LOADER.P.BIN


REM MAIN FILES
copy CONT.HRCG.BIN c:\ac1.3.5
del CONT.HRCG.bin

copy c:\my_code\na\Binaries.prebuilt\nox_font.set.bin c:\ac1.3.5

copy GAME.bin c:\ac1.3.5
del GAME.bin

copy swap.routines.combat.setup.bin c:\ac1.3.5
del swap.routines.combat.setup.bin

copy swap.routines.combat.bin c:\ac1.3.5
del swap.routines.combat.bin

copy swap.routines.combat.exit.bin c:\ac1.3.5
del swap.routines.combat.exit.bin

copy swap.routines.combat.bin c:\ac1.3.5
del swap.routines.combat.bin

REM copy swap.routines.combat.stats_routines.bin c:\ac1.3.5
REM del swap.routines.combat.stats_routines.bin

copy swap.routines.cast_spell.setup.bin c:\ac1.3.5
del swap.routines.cast_spell.setup.bin

copy swap.routines.spell_file.bin c:\ac1.3.5
del swap.routines.spell_file.bin


copy swap.routines.inventory.bin c:\ac1.3.5
del swap.routines.inventory.bin

copy swap.routines.npc.talk.bin c:\ac1.3.5
del swap.routines.npc.talk.bin

copy swap.routines.non_building.bin c:\ac1.3.5
del swap.routines.non_building.bin

copy swap.routines.building.bin c:\ac1.3.5
del swap.routines.building.bin

copy lwr.main.rtns.bin c:\ac1.3.5
del lwr.main.rtns.bin

copy bs.routines.bank2.bin c:\ac1.3.5
del bs.routines.bank2.bin

copy bs.routines.bank1.bin c:\ac1.3.5
del bs.routines.bank1.bin

copy bs_aux.routines.bank2.bin c:\ac1.3.5
del bs_aux.routines.bank2.bin

REM Data Files: Shapes
copy data.shapes.surface.bin c:\ac1.3.5
del data.shapes.surface.bin

copy data.shapes.building.bin c:\ac1.3.5
del data.shapes.building.bin

copy data.shapes.castle_courtyard.bin c:\ac1.3.5
del data.shapes.castle_courtyard.bin

copy data.shapes.undermap.bin c:\ac1.3.5
del data.shapes.undermap.bin


REM Data Files: Game

REM copy data.game.mob_tables.bin c:\ac1.3.5
REM del data.game.mob_tables.bin

REM copy data.game.inventory.bin c:\ac1.3.5
REM del data.game.inventory.bin


REM Data Files: Player
copy data.ply.chr_sheet.bin c:\ac1.3.5
del data.ply.chr_sheet.bin


REM Data Files: Other

copy DATA.OTHER.SUNRISE_SUNSET.BIN c:\ac1.3.5
del DATA.OTHER.SUNRISE_SUNSET.BIN

REM Data Files: Surface
copy COMPRESSED.DATA.MAP.SURFACE.BIN c:\ac1.3.5
del COMPRESSED.DATA.MAP.SURFACE.BIN
copy DATA.SPR.SURFACE.BIN c:\ac1.3.5
del DATA.SPR.SURFACE.BIN

REM Data Files: Location1
copy c:\my_code\na\source_code\COMPRESSED_DATA\NPC.SPEECH.TEXT\DATA.TLK.L001.BIN c:\ac1.3.5
REM             Floor1 (map1)
copy DATA.MAP.L1.F1.M1.BIN c:\ac1.3.5
del DATA.MAP.L1.F1.M1.BIN
copy DATA.SPR.L1.F1.M1.BIN c:\ac1.3.5
del DATA.SPR.L1.F1.M1.BIN

REM             Floor2 (map2)
copy DATA.MAP.L1.F2.M2.BIN c:\ac1.3.5
del DATA.MAP.L1.F2.M2.BIN
copy DATA.SPR.L1.F2.M2.BIN c:\ac1.3.5
del DATA.SPR.L1.F2.M2.BIN

REM data files: location2
REM (map3-5)
copy data.map.l2.f1.1.m3.bin c:\ac1.3.5
del data.map.l2.f1.1.m3.bin
copy data.map.l2.f2.1.m4.bin c:\ac1.3.5
del data.map.l2.f2.1.m4.bin
copy data.map.l2.f3.1.m5.bin c:\ac1.3.5
del data.map.l2.f3.1.m5.bin

copy data.spr.l2.f1.1.m3.bin c:\ac1.3.5
del data.spr.l2.f1.1.m3.bin
copy data.spr.l2.f2.1.m4.bin c:\ac1.3.5
del data.spr.l2.f2.1.m4.bin
copy data.spr.l2.f3.1.m5.bin c:\ac1.3.5
del data.spr.l2.f3.1.m5.bin
REM -------------------------

REM Data Files: Undermap
copy COMPRESSED.DATA.MAP.UNDERMAP_LV1.BIN c:\ac1.3.5
del COMPRESSED.DATA.MAP.UNDERMAP_LV1.BIN

copy DATA.SPR.UNDERMAP_LV1.BIN c:\ac1.3.5
del DATA.SPR.UNDERMAP_LV1.BIN 





REM copy master disk images
copy c:\my_code\na\disk_image_masters\BOOTLDR.PRODOS_FS.DSK c:\ac1.3.5\my.program.dsk
copy c:\my_code\na\disk_image_masters\NOXARCH.PLAYER1.PO c:\ac1.3.5\my.main_player.PO
copy c:\my_code\na\disk_image_masters\NOXARCH.PLAYER1.PO c:\ac1.3.5\na.castle_disk.PO
copy c:\my_code\na\disk_image_masters\NOXARCH.PLAYER1.PO c:\ac1.3.5\na.town_disk.PO
copy c:\my_code\na\disk_image_masters\NOXARCH.PLAYER1.PO c:\ac1.3.5\na.undermap_disk.PO


REM Use AppleCommander to load binary files onto disk image
cd\ac1.3.5

REM TESTING FILES
java -jar ac.jar -p my.program.dsk DUMMY.FILE bin 0x0000 < DUMMY.FILE.BIN


REM BOOT FILES
java -jar ac.jar -p my.program.dsk NOXARCH.SYSTEM bin 0x2000 < NOXARCH.SYSTEM.bin 
java -jar ac.jar -p my.program.dsk NOXARCH.MAIN bin 0x1000 < NOXARCH.MAIN.BIN
java -jar ac.jar -p my.program.dsk LOADER.P bin 0x2000 < LOADER.P.BIN

REM MAIN FILES
java -jar ac.jar -p my.program.dsk NOX_FONT.SET bin 0xC00 < c:\my_code\na\Binaries.prebuilt\nox_font.set.bin
java -jar ac.jar -p my.program.dsk CONT.HRCG bin 0x300 < CONT.HRCG.bin
java -jar ac.jar -p my.program.dsk DATA.OTHER.SUN bin 0x6C00 < DATA.OTHER.SUNRISE_SUNSET.BIN									
java -jar ac.jar -p my.program.dsk GAME bin 0x6000 < GAME.bin

java -jar ac.jar -p my.program.dsk BS.ROUTINES.BK2 bin 0xD000 < BS.ROUTINES.BANK2.bin
java -jar ac.jar -p my.program.dsk BS.ROUTINES.BK1 bin 0xD700 < BS.ROUTINES.BANK1.bin
java -jar ac.jar -p my.program.dsk LWR.MAIN.RTNS bin 0x0C00 < LWR.MAIN.RTNS.BIN
java -jar ac.jar -p my.main_player.PO BS_AUX.RTN.BK2 bin 0xD700 < BS_AUX.ROUTINES.BANK2.bin
REM BS_AUX routines are not on program disk because they need to be reloaded when combat exits.  

java -jar ac.jar -p my.main_player.PO SRTN.COMBAT.S bin 0x9000 < SWAP.ROUTINES.COMBAT.SETUP.BIN
java -jar ac.jar -p my.main_player.PO SRTN.COMBAT bin 0xA000 < SWAP.ROUTINES.COMBAT.BIN
java -jar ac.jar -p my.main_player.PO SRTN.COMBAT.E bin 0x9000 < SWAP.ROUTINES.COMBAT.EXIT.BIN

java -jar ac.jar -p my.main_player.PO SRTN.CAST_SPELL bin 0x9000 < SWAP.ROUTINES.CAST_SPELL.SETUP.BIN
java -jar ac.jar -p my.main_player.PO SRTN.SPELL_FILE bin 0x9000 < SWAP.ROUTINES.SPELL_FILE.BIN


java -jar ac.jar -p my.main_player.PO SRTN.NPC.TALK bin 0xAE00 < SWAP.ROUTINES.NPC.TALK.BIN
java -jar ac.jar -p my.main_player.PO SRTN.INVENTORY bin 0x0000 < SWAP.ROUTINES.INVENTORY.BIN
java -jar ac.jar -p my.main_player.PO SRTN.NON.BLD bin 0x9600 < SWAP.ROUTINES.NON_BUILDING.BIN
java -jar ac.jar -p my.main_player.PO SRTN.BLD bin 0x9600 < SWAP.ROUTINES.BUILDING.BIN



REM Data Files: Player
java -jar ac.jar -p my.main_player.PO DATA.CHR_SHEET bin 0x7000 < DATA.PLY.CHR_SHEET.BIN

REM Data Files: Game
REM java -jar ac.jar -p my.main_player.PO DT.GME.MOB bin 0x9000 < DATA.GAME.MOB_TABLES.BIN
REM java -jar ac.jar -p my.main_player.PO DATA.GME.INV bin 0x7000 < data.game.inventory.bin


REM Data Files: Shapes
java -jar ac.jar -p my.main_player.PO DATA.SHP.SURF bin 0x7000 < DATA.SHAPES.SURFACE.BIN
java -jar ac.jar -p na.town_disk.PO DATA.SHP.BLD bin 0x7000 < DATA.SHAPES.BUILDING.BIN
java -jar ac.jar -p na.castle_disk.PO DATA.SHP.CSL_CT bin 0x7000 < DATA.SHAPES.CASTLE_COURTYARD.BIN
java -jar ac.jar -p na.undermap_disk.PO DATA.SHP.UM bin 0x7000 < DATA.SHAPES.UNDERMAP.BIN

REM Data Files: Surface
java -jar ac.jar -p my.main_player.PO DATA.MAP.SURF bin 0x200 < COMPRESSED.DATA.MAP.SURFACE.BIN
java -jar ac.jar -p my.main_player.PO DATA.SPR.SURF bin 0x6000 < DATA.SPR.SURFACE.BIN


REM Data Files: Location1 (temporarily add TLK file to all disk since for testing multiple locations are sharing the same TLK file)
java -jar ac.jar -p na.town_disk.PO DATA.TLK.L001 bin 0x6000 < DATA.TLK.L001.BIN
java -jar ac.jar -p na.castle_disk.PO DATA.TLK.L001 bin 0x6000 < DATA.TLK.L001.BIN
java -jar ac.jar -p na.undermap_disk.PO DATA.TLK.L001 bin 0x6000 < DATA.TLK.L001.BIN

REM				Floor1 (map1)
java -jar ac.jar -p na.town_disk.PO DATA.MAP.M1 bin 0x6000 < DATA.MAP.L1.F1.M1.BIN
java -jar ac.jar -p na.town_disk.PO DATA.SPR.M1 bin 0x6000 < DATA.SPR.L1.F1.M1.BIN
REM				Floor2 (map2)
java -jar ac.jar -p na.town_disk.PO DATA.MAP.M2 bin 0x6000 < DATA.MAP.L1.F2.M2.BIN
java -jar ac.jar -p na.town_disk.PO DATA.SPR.M2 bin 0x6000 < DATA.SPR.L1.F2.M2.BIN

REM Data Files: Location2
REM				Floor1.1 (map3)
java -jar ac.jar -p na.castle_disk.PO DATA.MAP.M3 bin 0x6000 < DATA.MAP.L2.F1.1.M3.BIN
java -jar ac.jar -p na.castle_disk.PO DATA.SPR.M3 bin 0x6000 < DATA.SPR.L2.F1.1.M3.BIN
REM				Floor2.1 (map4)
java -jar ac.jar -p na.castle_disk.PO DATA.MAP.M4 bin 0x6000 < DATA.MAP.L2.F2.1.M4.BIN
java -jar ac.jar -p na.castle_disk.PO DATA.SPR.M4 bin 0x6000 < DATA.SPR.L2.F2.1.M4.BIN
REM				Floor3.1 (map5)
java -jar ac.jar -p na.castle_disk.PO DATA.MAP.M5 bin 0x6000 < DATA.MAP.L2.F3.1.M5.BIN
java -jar ac.jar -p na.castle_disk.PO DATA.SPR.M5 bin 0x6000 < DATA.SPR.L2.F3.1.M5.BIN
REM --------------------




REM Data Files: Undermap
java -jar ac.jar -p na.undermap_disk.PO DATA.MAP.ULV1 bin 0x6000 < COMPRESSED.DATA.MAP.UNDERMAP_LV1.BIN
java -jar ac.jar -p na.undermap_disk.PO DATA.SPR.ULV1 bin 0x6000 < DATA.SPR.UNDERMAP_LV1.BIN



REM Distribute packed disk images to other applications (AppleWIN and ADTPro)
copy my.program.dsk c:\applewin\games\my.program.dsk
copy my.program.dsk c:\AppleWIN_1.26.2.2\games\
copy my.program.dsk c:\ADTPro-2.0.0\disks\my.program.dsk
copy my.program.dsk c:\mame\my.program.dsk
copy my.program.dsk c:\my_code\na\build_folder\my.program.dsk

REM copy my.program.dsk c:\USERS\MARK\DROPBOX\TEMP2\my.program.dsk

copy my.main_player.PO c:\applewin\games\my.main_player.PO
copy my.main_player.PO c:\AppleWIN_1.26.2.2\games\
copy my.main_player.PO c:\ADTPro-2.0.0\disks\my.main_player.PO
copy my.main_player.PO c:\mame\my.main_player.PO
copy my.main_player.PO c:\my_code\na\build_folder\my.main_player.PO

REM copy my.main_player.dsk c:\USERS\MARK\DROPBOX\TEMP2\my.main_player.dsk


copy na.castle_disk.PO c:\applewin\games\na.castle_disk.PO
copy na.castle_disk.PO c:\AppleWIN_1.26.2.2\games\
copy na.castle_disk.PO c:\ADTPro-2.0.0\disks\na.castle_disk.PO
copy na.castle_disk.PO c:\mame\na.castle_disk.PO
copy na.castle_disk.PO c:\my_code\na\build_folder\na.castle_disk.PO

REM copy na.castle_disk.PO c:\USERS\MARK\DROPBOX\TEMP2\na.castle_disk.PO

copy na.town_disk.PO c:\applewin\games\na.town_disk.PO
copy na.town_disk.PO c:\AppleWIN_1.26.2.2\games\
copy na.town_disk.PO c:\ADTPro-2.0.0\disks\na.town_disk.PO
copy na.town_disk.PO c:\mame\na.town_disk.PO
copy na.town_disk.PO c:\my_code\na\build_folder\na.town_disk.PO

REM na.town_disk.PO c:\USERS\MARK\DROPBOX\TEMP2\na.town_disk.PO


copy na.undermap_disk.PO c:\applewin\games\na.undermap_disk.PO
copy na.undermap_disk.PO c:\AppleWIN_1.26.2.2\games\
copy na.undermap_disk.PO c:\ADTPro-2.0.0\disks\na.undermap_disk.PO
copy na.undermap_disk.PO c:\mame\na.undermap_disk.PO
copy na.undermap_disk.PO c:\my_code\na\build_folder\na.undermap_disk.PO

REM copy na.undermap_disk.PO c:\USERS\MARK\DROPBOX\TEMP2\na.undermap_disk.PO

copy c:\applewin\games\my.program.dsk c:\my_code\backups\%hour%.%min%
copy c:\applewin\games\my.main_player.PO c:\my_code\backups\%hour%.%min%
copy c:\applewin\games\na.castle_disk.PO c:\my_code\backups\%hour%.%min%
copy c:\applewin\games\na.town_disk.PO c:\my_code\backups\%hour%.%min%
copy c:\applewin\games\na.undermap_disk.PO c:\my_code\backups\%hour%.%min%



REM ====CLEANUP====: 
REM (delete the files created in this iteration to avoid mixups in future iterations)
REM (update: I decided to just delete all binary files in the C:\AC1.3.5 directory
REM (because I really don't keep any binary files there. They are all temporary)
del *.bin
del *.zx7
del *.dsk
del *.po

cd\my_code\na\build_folder
del *.bin

ECHO END BUILD PROCESS

ECHO LAUNCHING AppleWIN
cd\AppleWIN
cd\AppleWIN_1.26.2.2
REM applewin.exe -d1 c:\applewin\games\my.program.dsk 
REM applewin.exe -d2 c:\applewin\games\my.main_player.PO
applewin.exe



