REM ===================GO2p.BAT========================================================
REM ========FUNCTION: ASSEMBLY SINGLE TARGET FILE PROGRAMS TO BE LOADED BY ProDOS====
REM ========(use after gotest.bat)
REM ==================================================================================
REM
REM Make Backup Copy of Code Iteration
set Hour=%Time:~0,2%
if "%hour:~0,1%" == " " set hour=0%hour:~1,1%
set Min=%Time:~3,2%

COPY C:\MY_CODE\test.bin c:\ac1.3.5

copy NOX.DSK c:\ac1.3.5\sbasm_test.dsk
REM copy MASTER_PRODOS_1.9.PO c:\ac1.3.5\sbasm_test.PO

cd\ac1.3.5
java -jar ac.jar -p sbasm_test.DSK NOXARCH.MAIN bin 0x1000 < test.bin 




copy sbasm_test.dsk c:\applewin\games
copy sbasm_test.dsk c:\ADTPro-2.0.0\disks\

del sbasm_test.dsk
del test.bin

cd\my_code

