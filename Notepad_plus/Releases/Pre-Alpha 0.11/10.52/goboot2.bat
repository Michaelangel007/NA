REM ===================GOPU.BAT========================================================
REM ========FUNCTION: UPDATE PRODOS FILE SYSTEM BOOTLOADER =====
REM ========(update the disk controller Peter Ferrie (qkumba) wrote, stores at Track0, Sector1-9======
REM ========(use after gotest.bat)
REM ==================================================================================
REM
REM Make Backup Copy of Code Iteration
set Hour=%Time:~0,2%
if "%hour:~0,1%" == " " set hour=0%hour:~1,1%
set Min=%Time:~3,2%

mkdir c:\my_code\backups\%hour%.%min%
cd\my_code
copy c:\my_code\INCLUDES_LIBS\pdriver.asm c:\my_code\backups\%hour%.%min%
copy c:\my_code\bootloader\*.asm c:\my_code\backups\%hour%.%min%
copy c:\sbasm3\*.bat c:\my_code\backups\%hour%.%min%


copy BOOTLDR_TOOL.BIN c:\ac1.3.5
del BOOTLDR_TOOL.BIN

copy PRODOS.DRIVER.BIN  c:\ac1.3.5
del PRODOS.DRIVER.BIN 


copy CONT.HRCG.bin c:\ac1.3.5
copy GOTHIC.SET.bin c:\ac1.3.5



copy BOOTLDR.PRODOS_FS.PO c:\ac1.3.5\sbasm_test.PO

copy MASTER_CURRENT_BASIC.DSK c:\ac1.3.5\sbasm_test.dsk


cd\ac1.3.5
java -jar ac.jar -p sbasm_test.dsk pdriver B 0xB700 < prodos.driver.bin
java -jar ac.jar -p sbasm_test.dsk t B 0x6000 < bootldr_tool.bin

REM java -jar ac.jar -p sbasm_test.po PDRIVER bin 0xB704 < PRODOS.DRIVER.BIN
REM java -jar ac.jar -p sbasm_test.po CONT.HRCG bin 0x300 < CONT.HRCG.bin
REM java -jar ac.jar -p sbasm_test.po GOTHIC.SET bin 0xC00 < GOTHIC.SET.bin


copy sbasm_test.dsk c:\applewin\games
copy sbasm_test.PO c:\applewin\games
copy sbasm_test.dsk c:\ADTPro-2.0.0\disks\
REM copy sbasm_test.dsk c:\USERS\MARK\DROPBOX\TEMP2\
del sbasm_test.dsk
del sbasm_test.PO
del PRODOS.DRIVER.BIN 
del BOOTLDR_TOOL.BIN


del CONT.HRCG.bin
del GOTHIC.SET.bin


cd\my_code