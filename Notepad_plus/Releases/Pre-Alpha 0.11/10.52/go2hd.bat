REM ===================GO2b.BAT========================================================
REM ========FUNCTION: ASSEMBLY GAME FILES TO BE LOADED BY CUSTOM BOOTLOADER		  =====
REM ========(use after go1.bat)
REM ===================================================================================
REM
REM Make Backup Copy of Code Iteration
set Hour=%Time:~0,2%
if "%hour:~0,1%" == " " set hour=0%hour:~1,1%
set Min=%Time:~3,2%

mkdir c:\my_code\backups\%hour%.%min%
mkdir c:\my_code\backups\%hour%.%min%\compressed_data

cd\my_code
copy c:\my_code\includes_libs\*.* c:\my_code\backups\%hour%.%min%
copy c:\my_code\includes_libs\compressed_data\*.* c:\my_code\backups\%hour%.%min%\compressed_data
copy c:\my_code\map_shapes\map\*.xlsx c:\my_code\backups\%hour%.%min%
copy game_loop.asm c:\my_code\backups\%hour%.%min%
copy map.compression.* c:\my_code\backups\%hour%.%min%


copy c:\sbasm3\*.bat c:\my_code\backups\%hour%.%min%
copy c:\my_code\*.dsk c:\my_code\backups\%hour%.%min%
copy c:\my_code\*.PO c:\my_code\backups\%hour%.%min%


REM BOOT FILES
copy NOXARCH.SYSTEM.BIN c:\ac1.3.5

copy NOXARCH.MAIN.BIN c:\ac1.3.5
del NOXARCH.MAIN.BIN

copy LOADER.P.BIN c:\ac1.3.5
del LOADER.P.BIN

REM DRIVER FILES
copy CONT.HRCG.BIN c:\ac1.3.5
REM copy CONT.HRCG.bin c:\ac1.3.5
del CONT.HRCG.bin

REM MAIN FILES
copy GOTHIC.SET.bin c:\ac1.3.5

copy GAME.bin c:\ac1.3.5
del GAME.bin

copy LWR.MAIN.RTNS.BIN c:\ac1.3.5
del LWR.MAIN.RTNS.BIN

copy BS.ROUTINES.BANK2.bin c:\ac1.3.5
del BS.ROUTINES.BANK2.bin

copy BS.ROUTINES.BANK1.bin c:\ac1.3.5
del BS.ROUTINES.BANK1.bin

REM Data Files
copy DATA.SHAPES.SURFACE.BIN c:\ac1.3.5
del DATA.SHAPES.SURFACE.BIN

copy DATA.SHAPES.BUILDING.BIN c:\ac1.3.5
del DATA.SHAPES.BUILDING.BIN

copy DATA.OTHER.SUNRISE_SUNSET.BIN c:\ac1.3.5
del DATA.OTHER.SUNRISE_SUNSET.BIN

copy DATA.MAP.L1.BIN c:\ac1.3.5
del DATA.MAP.L1.BIN

copy COMPRESSED.DATA.MAP.SURFACE.BIN c:\ac1.3.5
del COMPRESSED.DATA.MAP.SURFACE.BIN

copy COMPRESSED.DATA.MAP.UNDERMAP_LV1.BIN c:\ac1.3.5
del COMPRESSED.DATA.MAP.UNDERMAP_LV1.BIN



REM copy master disk images
copy NOX.TEST_HDD.HDV c:\ac1.3.5\APPLEHDD.HDV
REM copy MASTER_HARDDRIVE.HDV c:\ac1.3.5\APPLEHDD.HDV


REM Use AppleCommander to load binary files onto disk image
cd\ac1.3.5

java -jar ac.jar -p APPLEHDD.HDV NOXARCH.SYSTEM bin 0x2000 < NOXARCH.SYSTEM.bin 
java -jar ac.jar -p APPLEHDD.HDV NOXARCH.MAIN bin 0x1000 < NOXARCH.MAIN.BIN

java -jar ac.jar -p APPLEHDD.HDV LOADER.P bin 0x2000 < LOADER.P.BIN

java -jar ac.jar -p APPLEHDD.HDV GOTHIC.SET bin 0xC00 < GOTHIC.SET.bin
java -jar ac.jar -p APPLEHDD.HDV CONT.HRCG bin 0x300 < CONT.HRCG.bin
java -jar ac.jar -p APPLEHDD.HDV DATA.OTHER.SUN bin 0x6C00 < DATA.OTHER.SUNRISE_SUNSET.BIN									
java -jar ac.jar -p APPLEHDD.HDV GAME bin 0x6000 < GAME.bin
java -jar ac.jar -p APPLEHDD.HDV BS.ROUTINES.BK2 bin 0xD000 < BS.ROUTINES.BANK2.bin
java -jar ac.jar -p APPLEHDD.HDV DATA.SHP.BLD bin 0x7000 < DATA.SHAPES.BUILDING.BIN
java -jar ac.jar -p APPLEHDD.HDV DATA.MAP.L1 bin 0x6000 < DATA.MAP.L1.BIN
java -jar ac.jar -p APPLEHDD.HDV DATA.SHP.SURF bin 0x7000 < DATA.SHAPES.SURFACE.BIN
java -jar ac.jar -p APPLEHDD.HDV DATA.MAP.SURF bin 0x200 < COMPRESSED.DATA.MAP.SURFACE.BIN
java -jar ac.jar -p APPLEHDD.HDV DATA.UMAP.L1 bin 0x6000 < COMPRESSED.DATA.MAP.UNDERMAP_LV1.BIN

java -jar ac.jar -p APPLEHDD.HDV LWR.MAIN.RTNS bin 0x0C00 < LWR.MAIN.RTNS.BIN
java -jar ac.jar -p APPLEHDD.HDV BS.ROUTINES.BK1 bin 0xD700 < BS.ROUTINES.BANK1.bin




REM Distribute packed disk images to other applications (AppleWIN and ADTPro)
copy APPLEHDD.HDV c:\applewin\games\APPLEHDD.HDV
copy APPLEHDD.HDV c:\ADTPro-2.0.0\disks\APPLEHDD.HDV
REM copy APPLEHDD.HDV c:\USERS\MARK\DROPBOX\TEMP2\APPLEHDD.HDV


REM Cleanup: delete the files created in this iteration to avoid mixups in future iterations
del APPLEHDD.HDV

del noxarch.system.bin
del NOXARCH.MAIN.BIN
del LOADER.P.BIN

del CONT.HRCG.bin
del GOTHIC.SET.bin
del GAME.bin

del LWR.MAIN.RTNS.BIN
del BS.ROUTINES.BANK2.bin
del BS.ROUTINES.BANK1.bin

del DATA.SHAPES.SURFACE.BIN
del DATA.SHAPES.BUILDING.BIN
del DATA.OTHER.SUNRISE_SUNSET.BIN

del COMPRESSED.DATA.MAP.SURFACE.BIN
del COMPRESSED.DATA.MAP.UNDERMAP_LV1.BIN
del DATA.MAP.L1.BIN

cd\my_code

ECHO END BUILD PROCESS
