REM ===================GO2b.BAT========================================================
REM ========FUNCTION: ASSEMBLY GAME FILES TO BE LOADED BY CUSTOM BOOTLOADER		  =====
REM ===================================================================================
REM
REM Make Backup Copy of Code Iteration
set Hour=%Time:~0,2%
if "%hour:~0,1%" == " " set hour=0%hour:~1,1%
set Min=%Time:~3,2%

mkdir c:\my_code\backups\%hour%.%min%

cd\my_code
copy c:\my_code\includes_libs\*.* c:\my_code\backups\%hour%.%min%
copy game_loop.asm c:\my_code\backups\%hour%.%min%
copy map.compression.* c:\my_code\backups\%hour%.%min%


copy BOOTLDR.DSK c:\my_code\backups\%hour%.%min%
copy c:\sbasm3\go2c.bat c:\my_code\backups\%hour%.%min%
copy c:\sbasm3\go2.bat c:\my_code\backups\%hour%.%min%
copy c:\sbasm3\go1.bat c:\my_code\backups\%hour%.%min%

copy CONT.HRCG.bin c:\ac1.3.5

copy GOTHIC.SET.bin c:\ac1.3.5

copy MY.RWTS.BIN c:\ac1.3.5
del MY.RWTS.BIN

copy LOADER.bin c:\ac1.3.5
del LOADER.bin

copy VARIABLES.BIN c:\ac1.3.5
del VARIABLES.BIN

copy GAME.bin c:\ac1.3.5
del GAME.bin

copy BS_ROUTINES.bin c:\ac1.3.5
del BS_ROUTINES.bin

REM copy data files
copy DATA.SHAPES.SURFACE.BIN c:\ac1.3.5
del DATA.SHAPES.SURFACE.BIN

copy DATA.SHAPES.BUILDING.BIN c:\ac1.3.5
del DATA.SHAPES.BUILDING.BIN

copy DATA.OTHER.SUNRISE_SUNSET.BIN c:\ac1.3.5
del DATA.OTHER.SUNRISE_SUNSET.BIN

copy DATA.MAP.UNDERMAP_LV1.BIN c:\ac1.3.5
del DATA.MAP.UNDERMAP_LV1

copy DATA.MAP.L1.BIN c:\ac1.3.5
del DATA.MAP.L1.BIN

REM copy compressed data files
REM cd\My_Code\INCLUDES_LIBS\compressed_data

copy COMPRESSED.DATA.MAP.SURFACE.BIN c:\ac1.3.5
del COMPRESSED.DATA.MAP.SURFACE.BIN

REM cd\my_code


REM copy master disk image that you want AppleCommander to use
copy BOOTLDR.DSK c:\ac1.3.5\my.program.dsk
copy BOOTLDR.DSK c:\ac1.3.5\my.main_player.dsk
REM copy BOOTLDR.DSK c:\ac1.3.5\my.sbasm_test.dsk

REM Use AppleCommander to load binary files onto disk image
cd\ac1.3.5

java -jar ac.jar -p my.program.dsk GOTHIC.SET B 0xC00 < GOTHIC.SET.bin
java -jar ac.jar -p my.program.dsk CONT.HRCG B 0x300 < CONT.HRCG.bin
java -jar ac.jar -p my.program.dsk MY.RWTS B 0x0F00 < MY.RWTS.bin
java -jar ac.jar -p my.program.dsk LOADER B 0x1F04 < LOADER.bin
java -jar ac.jar -p my.program.dsk VARIABLES B 0x1200 < VARIABLES.bin
java -jar ac.jar -p my.program.dsk DATA.OTHER.SUNRISE_SUNSET B 0x6000 < DATA.OTHER.SUNRISE_SUNSET.BIN
REM java -jar ac.jar -p my.program.dsk TEST.MAP B 0x6000 < TEST.MAP.BIN

java -jar ac.jar -p my.main_player.dsk GAME B 0x6000 < GAME.bin
java -jar ac.jar -p my.main_player.dsk BS_ROUTINES B 0xD000 < BS_ROUTINES.bin
java -jar ac.jar -p my.main_player.dsk DATA.SHAPES.BUILDING B 0x6000 < DATA.SHAPES.BUILDING.BIN
java -jar ac.jar -p my.main_player.dsk DATA.MAP.L1 B 0x6000 < DATA.MAP.L1.BIN
java -jar ac.jar -p my.main_player.dsk DATA.SHAPES.SURFACE B 0x6000 < DATA.SHAPES.SURFACE.BIN
REM java -jar ac.jar -p my.main_player.dsk DATA.MAP.UNDERMAP_LV1 B 0x6000 < DATA.MAP.UNDERMAP_LV1.BIN
java -jar ac.jar -p my.main_player.dsk DATA.MAP.SURFACE B 0x6000 < COMPRESSED.DATA.MAP.SURFACE.BIN
java -jar ac.jar -p my.main_player.dsk test B 0x6000 < DATA.SHAPES.SURFACE.BIN

REM java -jar ac.jar -p my.sbasm_test.dsk DATA.SHAPES.SURFACE B 0x6000 < DATA.SHAPES.SURFACE.BIN





REM Distribute packed disk images to other applications (AppleWIN and ADTPro)
copy my.program.dsk c:\applewin\games\my.program.dsk
copy my.program.dsk c:\ADTPro-2.0.0\disks\my.program.dsk
copy my.program.dsk c:\USERS\MARK\DROPBOX\TEMP2\my.program.dsk

copy my.main_player.dsk c:\applewin\games\my.main_player.dsk
copy my.main_player.dsk c:\ADTPro-2.0.0\disks\my.main_player.dsk
copy my.main_player.dsk c:\USERS\MARK\DROPBOX\TEMP2\my.main_player.dsk


REM copy my.sbasm_test.dsk c:\applewin\games\my.sbasm_test.dsk
REM copy my.sbasm_test.dsk c:\ADTPro-2.0.0\disks\my.sbasm_test.dsk
REM copy my.sbasm_test.dsk c:\USERS\MARK\DROPBOX\TEMP2\my.sbasm_test.dsk



REM Cleanup: delete the files created in this iteration to avoid mixups in future iterations
del my.program.dsk
del my.main_player.dsk
REM del my.sbasm_test.dsk

del CONT.HRCG.bin
del GOTHIC.SET.bin
del MY.RWTS.BIN
del LOADER.bin
del GAME.bin
del VARIABLES.BIN

del BS_ROUTINES.bin

del DATA.SHAPES.SURFACE.BIN
del DATA.SHAPES.BUILDING.BIN
del DATA.OTHER.SUNRISE_SUNSET.BIN

del COMPRESSED.DATA.MAP.SURFACE.BIN

del DATA.MAP.UNDERMAP_LV1.BIN
del DATA.MAP.L1.BIN

cd\my_code

ECHO END BUILD PROCESS
