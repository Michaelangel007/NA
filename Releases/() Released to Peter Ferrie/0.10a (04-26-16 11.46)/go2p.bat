REM ===================GO2p.BAT========================================================
REM ========FUNCTION: ASSEMBLY SINGLE TARGET FILE PROGRAMS TO BE LOADED BY ProDOS====
REM ========(use after gotest.bat)
REM ==================================================================================
REM
REM Make Backup Copy of Code Iteration
set Hour=%Time:~0,2%
if "%hour:~0,1%" == " " set hour=0%hour:~1,1%
set Min=%Time:~3,2%

mkdir c:\my_code\backups\%hour%.%min%
cd\my_code
copy zone_functions.asm c:\my_code\backups\%hour%.%min%


copy test.bin c:\ac1.3.5
del test.bin
REM copy MASTER_DOS3.3.DSK c:\ac1.3.5\sbasm_test.dsk
REM copy testhdd.hdv c:\ac1.3.5\sbasm_test.hdv
copy MASTER_PRODOS_1.9.PO c:\ac1.3.5\sbasm_test.PO

cd\ac1.3.5
REM java -jar ac.jar -p sbasm_test.hdv test B 0x6000 < test.bin
java -jar ac.jar -p sbasm_test.po TEST bin 0x6000 < test.bin 

copy sbasm_test.po c:\applewin\games
copy sbasm_test.po c:\ADTPro-2.0.0\disks\
copy sbasm_test.po c:\USERS\MARK\DROPBOX\TEMP2\
del sbasm_test.po
del test.bin
cd\my_code

