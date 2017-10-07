REM ===================GO2.BAT========================================================
REM ========FUNCTION: ASSEMBLY SINGLE TARGET FILE PROGRAMS TO BE LOADED BY DOS3.3=====
REM ========(use after gotest.bat)
REM ==================================================================================
REM
REM Make Backup Copy of Code Iteration
set Hour=%Time:~0,2%
if "%hour:~0,1%" == " " set hour=0%hour:~1,1%
set Min=%Time:~3,2%

mkdir c:\my_code\backups\%hour%.%min%
cd\my_code
copy test*.asm c:\my_code\backups\%hour%.%min%


copy test.bin c:\ac1.3.5
del test.bin
REM copy MASTER_DOS3.3.DSK c:\ac1.3.5\sbasm_test.dsk
copy MASTER_CURRENT_BASIC.DSK c:\ac1.3.5\sbasm_test.dsk
cd\ac1.3.5
java -jar ac.jar -p sbasm_test.dsk t B 0x6000 < test.bin
copy sbasm_test.dsk c:\applewin\games
copy sbasm_test.dsk c:\ADTPro-2.0.0\disks\SBASM_TEST.DSK
REM copy sbasm_test.dsk c:\USERS\MARK\DROPBOX\TEMP2\SBASM_TEST.DSK
del sbasm_test.dsk
del test.bin
cd\my_code

