REM ===================GO2b.BAT========================================================
REM ========FUNCTION: ASSEMBLY GAME FILES TO BE LOADED BY DOS 3.3				  =====
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

REM Copy binary files assembled by SBASM to AppleCommander's directory
REM copy cont.hrcg.bin c:\ac1.3.5
copy CONT.HRCG.bin c:\ac1.3.5
copy GOTHIC.SET.bin c:\ac1.3.5
copy LOADER.bin c:\ac1.3.5
del LOADER.bin
copy GAME.bin c:\ac1.3.5
del GAME.bin
copy VARIABLES.BIN c:\ac1.3.5
del VARIABLES.BIN


REM copy master disk image that you want AppleCommander to use
copy MASTER_CURRENT_GAME.DSK c:\ac1.3.5\sbasm_test.dsk

REM Use AppleCommander to load binary files onto disk image
cd\ac1.3.5
 
java -jar ac.jar -p sbasm_test.dsk GOTHIC.SET B 0xC00 < GOTHIC.SET.bin
java -jar ac.jar -p sbasm_test.dsk CONT.HRCG B 0x300 < CONT.HRCG.bin
java -jar ac.jar -p SBASM_test.dsk LOADER B 0x1F00 < LOADER.bin
java -jar ac.jar -p SBASM_test.dsk GAME B 0x6000 < GAME.bin
java -jar ac.jar -p sbasm_test.dsk VARIABLES B 0x1200 < VARIABLES.bin
REM java -jar ac.jar -p sbasm_test.dsk TEST.OFFLOAD B 0x1600 < TEST.OFFLOAD2.bin
REM java -jar ac.jar -p SBASM_test.dsk cont.hrcg B 0x9500 < cont.hrcg.bin
REM java -jar ac.jar -p SBASM_test.dsk table B 0x0C00 < table.bin

REM Distribute packed disk image to other applications (AppleWIN and ADTPro)
copy sbasm_test.dsk c:\applewin\games\SBASM_TEST.DSK
copy sbasm_test.dsk c:\ADTPro-2.0.0\disks\SBASM_TEST.DSK
copy sbasm_test.dsk c:\USERS\MARK\DROPBOX\TEMP2\SBASM_TEST.DSK

REM Cleanup: delete the files created in this iteration to avoid mixups in future iterations
del sbasm_test.dsk
del CONT.HRCG.bin
del GOTHIC.SET.bin
del MY.RWTS.BIN
del LOADER.bin
del GAME.bin
del VARIABLES.BIN
cd\my_code
