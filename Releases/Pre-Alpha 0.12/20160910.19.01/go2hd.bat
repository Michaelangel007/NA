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
copy c:\my_code\includes_libs\compressed_data\*.* c:\my_code\backups\%hour%.%min%\compressed_data
copy c:\my_code\includes_libs\compressed_data\npc.speech.text\*.* c:\my_code\backups\%hour%.%min%\compressed_data\npc.speech.text\

copy c:\my_code\map_shapes\map\*.xlsx c:\my_code\backups\%hour%.%min%
copy game_loop.asm c:\my_code\backups\%hour%.%min%
copy map.compression.* c:\my_code\backups\%hour%.%min%
copy C:\MY_CODE\COMPRESSION\NPC.SPEECH.TEXT\*.bas c:\my_code\backups\%hour%.%min%

copy c:\qb64\*.bat c:\my_code\backups\%hour%.%min%
copy c:\sbasm3\*.bat c:\my_code\backups\%hour%.%min%
copy c:\my_code\*.dsk c:\my_code\backups\%hour%.%min%
copy c:\my_code\*.PO c:\my_code\backups\%hour%.%min%

@ECHO ON

REM copy c:\applewin\games\APPLEHDD.PO c:\my_code\backups\%hour%.%min%
CD\my_code\backups\%hour%.%min%
del MASTER_HARDDRIVE.PO
cd\my_code



REM BOOT FILES
copy NOXARCH.SYSTEM.BIN c:\ac1.3.5

copy NOXARCH.MAIN.BIN c:\ac1.3.5
del NOXARCH.MAIN.BIN

copy LOADER.P.BIN c:\ac1.3.5
del LOADER.P.BIN


REM MAIN FILES
copy CONT.HRCG.BIN c:\ac1.3.5
del CONT.HRCG.bin

copy GOTHIC.SET.bin c:\ac1.3.5

copy GAME.bin c:\ac1.3.5
del GAME.bin

copy SWAP.ROUTINES.NPC.TALK.BIN c:\ac1.3.5
del SWAP.ROUTINES.NPC.TALK.BIN

copy LWR.MAIN.RTNS.BIN c:\ac1.3.5
del LWR.MAIN.RTNS.BIN

copy BS.ROUTINES.BANK2.bin c:\ac1.3.5
del BS.ROUTINES.BANK2.bin

copy BS.ROUTINES.BANK1.bin c:\ac1.3.5
del BS.ROUTINES.BANK1.bin


REM Data Files: Shapes
copy DATA.SHAPES.SURFACE.BIN c:\ac1.3.5
del DATA.SHAPES.SURFACE.BIN

copy DATA.SHAPES.BUILDING.BIN c:\ac1.3.5
del DATA.SHAPES.BUILDING.BIN

REM Data Files: Other

copy DATA.OTHER.SUNRISE_SUNSET.BIN c:\ac1.3.5
del DATA.OTHER.SUNRISE_SUNSET.BIN

REM Data Files: Surface
copy COMPRESSED.DATA.MAP.SURFACE.BIN c:\ac1.3.5
del COMPRESSED.DATA.MAP.SURFACE.BIN
copy DATA.SPR.SURFACE.BIN c:\ac1.3.5
REM del DATA.SPR.SURFACE.BIN

REM Data Files: Location1
copy C:\MY_CODE\INCLUDES_LIBS\COMPRESSED_DATA\NPC.SPEECH.TEXT\DATA.TLK.L001.BIN c:\ac1.3.5

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

REM -------------------------

REM Data Files: Undermap
copy COMPRESSED.DATA.MAP.UNDERMAP_LV1.BIN c:\ac1.3.5
del COMPRESSED.DATA.MAP.UNDERMAP_LV1.BIN

copy DATA.SPR.UNDERMAP_LV1.BIN c:\ac1.3.5
del DATA.SPR.UNDERMAP_LV1.BIN 



REM copy master disk images
copy NOX.TEST_HDD.PO c:\ac1.3.5\APPLEHDD.PO
REM copy APPLEHDD.HDV c:\ac1.3.5\APPLEHDD.PO
REM copy HARDDRIVE.1.4MB.PO c:\ac1.3.5\APPLEHDD.PO


REM Use AppleCommander to load binary files onto disk image
cd\ac1.3.5

REM BOOT FILES
java -jar ac.jar -p APPLEHDD.PO NOXARCH.SYSTEM bin 0x2000 < NOXARCH.SYSTEM.bin 
java -jar ac.jar -p APPLEHDD.PO NOXARCH.MAIN bin 0x1000 < NOXARCH.MAIN.BIN

java -jar ac.jar -p APPLEHDD.PO LOADER.P bin 0x2000 < LOADER.P.BIN

REM MAIN FILES

java -jar ac.jar -p APPLEHDD.PO GOTHIC.SET bin 0xC00 < GOTHIC.SET.bin
java -jar ac.jar -p APPLEHDD.PO CONT.HRCG bin 0x300 < CONT.HRCG.bin
java -jar ac.jar -p APPLEHDD.PO DATA.OTHER.SUN bin 0x6C00 < DATA.OTHER.SUNRISE_SUNSET.BIN									
java -jar ac.jar -p APPLEHDD.PO GAME bin 0x6000 < GAME.bin
java -jar ac.jar -p APPLEHDD.PO BS.ROUTINES.BK2 bin 0xD000 < BS.ROUTINES.BANK2.bin
java -jar ac.jar -p APPLEHDD.PO BS.ROUTINES.BK1 bin 0xD700 < BS.ROUTINES.BANK1.bin
java -jar ac.jar -p APPLEHDD.PO LWR.MAIN.RTNS bin 0x0C00 < LWR.MAIN.RTNS.BIN
java -jar ac.jar -p APPLEHDD.PO SRTN.NPC.TALK bin 0xAC00 < SWAP.ROUTINES.NPC.TALK.BIN


REM Data Files: Shapes
java -jar ac.jar -p APPLEHDD.PO DATA.SHP.SURF bin 0x7000 < DATA.SHAPES.SURFACE.BIN
java -jar ac.jar -p APPLEHDD.PO DATA.SHP.BLD bin 0x7000 < DATA.SHAPES.BUILDING.BIN

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
cd\applewin
applewin.exe
