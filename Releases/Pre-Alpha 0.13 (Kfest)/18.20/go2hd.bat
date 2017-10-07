REM ===================GO2b.BAT========================================================
REM ========FUNCTION: backup code and build disk image=====
REM ========(use after go1.bat)
REM ===================================================================================
REM
REM Make Backup Copy of Code Iteration
setlocal

set Hour=%Time:~0,2%
if "%hour:~0,1%" == " " set hour=0%hour:~1,1%
set Min=%Time:~3,2%

mkdir c:\my_code\backups\%hour%.%min%
mkdir c:\my_code\backups\%hour%.%min%\compressed_data
mkdir c:\my_code\backups\%hour%.%min%\compressed_data\npc.speech.text\

cd\my_code
copy c:\my_code\includes_libs\*.* c:\my_code\backups\%hour%.%min%
copy c:\my_code\list.lst c:\my_code\backups\%hour%.%min%
copy c:\my_code\includes_libs\compressed_data\*.* c:\my_code\backups\%hour%.%min%\compressed_data
copy c:\my_code\includes_libs\compressed_data\npc.speech.text\*.* c:\my_code\backups\%hour%.%min%\compressed_data\npc.speech.text\

copy c:\my_code\map_shapes\map\*.xlsx c:\my_code\backups\%hour%.%min%
copy game_loop.asm c:\my_code\backups\%hour%.%min%
copy map.compression.* c:\my_code\backups\%hour%.%min%
copy NOXARCH.SYSTEM.BIN c:\my_code\backups\%hour%.%min%
copy NOXARCH.SYSTEM.PROTECTED c:\my_code\backups\%hour%.%min%
copy c:\my_code\compression\npc.speech.text\*.bas c:\my_code\backups\%hour%.%min%

copy c:\qb64\*.bat c:\my_code\backups\%hour%.%min%
copy c:\sbasm3\*.bat c:\my_code\backups\%hour%.%min%
copy c:\my_code\*.dsk c:\my_code\backups\%hour%.%min%
copy c:\my_code\*.po c:\my_code\backups\%hour%.%min%

@ECHO ON

REM copy c:\applewin\games\APPLEHDD.PO c:\my_code\backups\%hour%.%min%
CD\my_code\backups\%hour%.%min%
del master_harddrive.po
cd\my_code



REM BOOT FILES
copy noxarch.system.bin c:\ac1.3.5

copy noxarch.main.bin c:\ac1.3.5
del noxarch.main.bin

copy loader.p.bin c:\ac1.3.5
del loader.p.bin


REM MAIN FILES
copy cont.hrcg.bin c:\ac1.3.5
del cont.hrcg.bin

copy nox_font.set.bin c:\ac1.3.5

copy game.bin c:\ac1.3.5
del game.bin


copy swap.routines.combat.setup.bin c:\ac1.3.5
del swap.routines.combat.setup.bin

copy swap.routines.combat.bin c:\ac1.3.5
del swap.routines.combat.bin

copy swap.routines.combat.exit.bin c:\ac1.3.5
del swap.routines.combat.exit.bin


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

REM Data Files: Player
copy data.ply.chr_sheet.bin c:\ac1.3.5
del data.ply.chr_sheet.bin

REM Data Files: Game

REM copy data.game.mob_tables.bin c:\ac1.3.5
REM del data.game.mob_tables.bin

REM copy data.game.inventory.bin c:\ac1.3.5
REM del data.game.inventory.bin

REM Data Files: Other
copy data.other.sunrise_sunset.bin c:\ac1.3.5
del data.other.sunrise_sunset.bin

REM Data Files: Surface
copy compressed.data.map.surface.bin c:\ac1.3.5
del compressed.data.map.surface.bin
copy data.spr.surface.bin c:\ac1.3.5
REM del DATA.SPR.SURFACE.BIN

REM Data Files: Location1
copy c:\my_code\includes_libs\compressed_data\npc.speech.text\data.tlk.l001.bin c:\ac1.3.5

REM             Floor1 (map1)
copy data.map.l1.f1.m1.bin c:\ac1.3.5
del data.map.l1.f1.m1.bin
copy data.spr.l1.f1.m1.bin c:\ac1.3.5
del data.spr.l1.f1.m1.bin

REM             Floor2 (map2)
copy data.map.l1.f2.m2.bin c:\ac1.3.5
del data.map.l1.f2.m2.bin
copy data.spr.l1.f2.m2.bin c:\ac1.3.5
del data.spr.l1.f2.m2.bin

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
copy compressed.data.map.undermap_lv1.bin c:\ac1.3.5
del compressed.data.map.undermap_lv1.bin

copy data.spr.undermap_lv1.bin c:\ac1.3.5
del data.spr.undermap_lv1.bin 



REM copy master disk images
copy nox.test_hdd.po c:\ac1.3.5\applehdd.po
REM copy APPLEHDD.HDV c:\ac1.3.5\APPLEHDD.PO
REM copy HARDDRIVE.1.4MB.PO c:\ac1.3.5\APPLEHDD.PO


REM Use AppleCommander to load binary files onto disk image
cd\ac1.3.5

REM BOOT FILES
java -jar ac.jar -p APPLEHDD.PO NOXARCH.SYSTEM bin 0x2000 < NOXARCH.SYSTEM.bin 
java -jar ac.jar -p APPLEHDD.PO NOXARCH.MAIN bin 0x1000 < NOXARCH.MAIN.BIN

java -jar ac.jar -p APPLEHDD.PO LOADER.P bin 0x2000 < LOADER.P.BIN

REM MAIN FILES

java -jar ac.jar -p APPLEHDD.PO NOX_FONT.SET bin 0xC00 < nox_font.set.bin
java -jar ac.jar -p APPLEHDD.PO CONT.HRCG bin 0x300 < CONT.HRCG.bin
java -jar ac.jar -p APPLEHDD.PO DATA.OTHER.SUN bin 0x6C00 < DATA.OTHER.SUNRISE_SUNSET.BIN									
java -jar ac.jar -p APPLEHDD.PO GAME bin 0x6000 < GAME.bin

java -jar ac.jar -p APPLEHDD.PO BS.ROUTINES.BK2 bin 0xD000 < BS.ROUTINES.BANK2.bin
java -jar ac.jar -p APPLEHDD.PO BS.ROUTINES.BK1 bin 0xD700 < BS.ROUTINES.BANK1.bin
java -jar ac.jar -p APPLEHDD.PO BS_AUX.RTN.BK2 bin 0xD700 < BS_AUX.ROUTINES.BANK2.bin
java -jar ac.jar -p APPLEHDD.PO LWR.MAIN.RTNS bin 0x0C00 < LWR.MAIN.RTNS.BIN

java -jar ac.jar -p APPLEHDD.PO SRTN.COMBAT.S bin 0x9000 < SWAP.ROUTINES.COMBAT.SETUP.BIN
java -jar ac.jar -p APPLEHDD.PO SRTN.COMBAT bin 0x9000 < SWAP.ROUTINES.COMBAT.BIN
java -jar ac.jar -p APPLEHDD.PO SRTN.COMBAT.E bin 0x9000 < SWAP.ROUTINES.COMBAT.EXIT.BIN

java -jar ac.jar -p APPLEHDD.PO SRTN.CAST_SPELL bin 0x9000 < SWAP.ROUTINES.CAST_SPELL.SETUP.BIN
java -jar ac.jar -p APPLEHDD.PO SRTN.SPELL_FILE bin 0x9000 < SWAP.ROUTINES.SPELL_FILE.BIN

java -jar ac.jar -p APPLEHDD.PO SRTN.NPC.TALK bin 0xAE00 < SWAP.ROUTINES.NPC.TALK.BIN
java -jar ac.jar -p APPLEHDD.PO SRTN.INVENTORY bin 0x0000 < SWAP.ROUTINES.INVENTORY.BIN
java -jar ac.jar -p APPLEHDD.PO SRTN.NON.BLD bin 0x9600 < SWAP.ROUTINES.NON_BUILDING.BIN
java -jar ac.jar -p APPLEHDD.PO SRTN.BLD bin 0x9600 < SWAP.ROUTINES.BUILDING.BIN


REM Data Files: Player
java -jar ac.jar -p APPLEHDD.PO DATA.CHR_SHEET bin 0x7000 < DATA.PLY.CHR_SHEET.BIN

REM Data Files: Game
REM java -jar ac.jar -p APPLEHDD.PO DT.GME.MOB bin 0x9000 < DATA.GAME.MOB_TABLES.BIN
REM java -jar ac.jar -p APPLEHDD.PO DATA.GME.INV bin 0x7000 < data.game.inventory.bin


REM Data Files: Shapes
java -jar ac.jar -p APPLEHDD.PO DATA.SHP.SURF bin 0x7000 < DATA.SHAPES.SURFACE.BIN
java -jar ac.jar -p APPLEHDD.PO DATA.SHP.BLD bin 0x7000 < DATA.SHAPES.BUILDING.BIN
java -jar ac.jar -p APPLEHDD.PO DATA.SHP.CSL_CT bin 0x7000 < DATA.SHAPES.CASTLE_COURTYARD.BIN
java -jar ac.jar -p APPLEHDD.PO DATA.SHP.UM bin 0x7000 < DATA.SHAPES.UNDERMAP.BIN



REM Data Files: Surface
java -jar ac.jar -p APPLEHDD.PO DATA.MAP.SURF bin 0x200 < COMPRESSED.DATA.MAP.SURFACE.BIN
java -jar ac.jar -p APPLEHDD.PO DATA.SPR.SURF bin 0x6000 < DATA.SPR.SURFACE.BIN

REM Data Files: Location1
java -jar ac.jar -p APPLEHDD.PO DATA.TLK.L001	bin 0xA000 < DATA.TLK.L001.BIN
REM				Floor1 (map1)
java -jar ac.jar -p APPLEHDD.PO DATA.MAP.M1 bin 0x6000 < DATA.MAP.L1.F1.M1.BIN
java -jar ac.jar -p APPLEHDD.PO DATA.SPR.M1 bin 0x6000 < DATA.SPR.L1.F1.M1.BIN
REM				Floor2 (map2)
java -jar ac.jar -p APPLEHDD.PO DATA.MAP.M2 bin 0x6000 < DATA.MAP.L1.F2.M2.BIN
java -jar ac.jar -p APPLEHDD.PO DATA.SPR.M2 bin 0x6000 < DATA.SPR.L1.F2.M2.BIN
REM Data Files: Location2
REM				Floor1.1 (map3)
java -jar ac.jar -p APPLEHDD.PO DATA.MAP.M3 bin 0x6000 < DATA.MAP.L2.F1.1.M3.BIN
java -jar ac.jar -p APPLEHDD.PO DATA.SPR.M3 bin 0x6000 < DATA.SPR.L2.F1.1.M3.BIN
REM				Floor2.1 (map4)
java -jar ac.jar -p APPLEHDD.PO DATA.MAP.M4 bin 0x6000 < DATA.MAP.L2.F2.1.M4.BIN
java -jar ac.jar -p APPLEHDD.PO DATA.SPR.M4 bin 0x6000 < DATA.SPR.L2.F2.1.M4.BIN
REM				Floor3.1 (map5)
java -jar ac.jar -p APPLEHDD.PO DATA.MAP.M5 bin 0x6000 < DATA.MAP.L2.F3.1.M5.BIN
java -jar ac.jar -p APPLEHDD.PO DATA.SPR.M5 bin 0x6000 < DATA.SPR.L2.F3.1.M5.BIN
REM --------------------

REM Data Files: Undermap
java -jar ac.jar -p APPLEHDD.PO DATA.MAP.ULV1 bin 0x6000 < COMPRESSED.DATA.MAP.UNDERMAP_LV1.BIN
java -jar ac.jar -p APPLEHDD.PO DATA.SPR.ULV1 bin 0x6000 < DATA.SPR.UNDERMAP_LV1.BIN


REM Testing
REM java -jar ac.jar -p APPLEHDD.PO unpacked.00 bin 0xA002 < DATA.TLK.L1.M1.00.BIN
REM java -jar ac.jar -p APPLEHDD.PO packed.00 bin 0xA002 < DATA.TLK.L1.M1.00.BIN.ZX7

REM java -jar ac.jar -p APPLEHDD.PO unpacked.08 bin 0xA103 < DATA.TLK.L1.M1.08.BIN
REM java -jar ac.jar -p APPLEHDD.PO packed.08 bin 0xA103 < DATA.TLK.L1.M1.08.BIN.ZX7

REM java -jar ac.jar -p APPLEHDD.PO unpacked.10 bin 0xA103 < DATA.TLK.L1.M1.10.BIN
REM java -jar ac.jar -p APPLEHDD.PO packed.10 bin 0xA103 < DATA.TLK.L1.M1.10.BIN.ZX7


REM Distribute packed disk images to other applications (AppleWIN and ADTPro)
copy APPLEHDD.PO c:\applewin\games\APPLEHDD.PO
REM copy APPLEHDD.PO c:\applewin\games\APPLEHDD.HDV
copy APPLEHDD.PO c:\AppleWIN_1.26.2.2\games\APPLEHDD.PO
copy APPLEHDD.PO c:\ADTPro-2.0.0\disks\APPLEHDD.PO
REM copy APPLEHDD.PO c:\USERS\MARK\DROPBOX\TEMP2\APPLEHDD.PO


REM ====CLEANUP====: 
REM (delete the files created in this iteration to avoid mixups in future iterations)
REM (update: I decided to just delete all binary files in the C:\AC1.3.5 directory
REM (because I really don't keep any binary files there. They are all temporary)

del *.bin
del *.zx7
del *.dsk
del *.po

cd\my_code

ECHO END BUILD PROCESS

ECHO LAUNCHING AppleWIN
cd\AppleWIN_1.26.2.2
applewin.exe
