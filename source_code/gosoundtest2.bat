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
copy c:\my_code\testing\sound_generation\*.* c:\my_code\backups\%hour%.%min%
copy c:\sbasm3\*.bat c:\my_code\backups\%hour%.%min%
copy c:\my_code\*.dsk c:\my_code\backups\%hour%.%min%
copy c:\my_code\*.PO c:\my_code\backups\%hour%.%min%

copy NOXARCH.SYSTEM.BIN c:\ac1.3.5


copy test.bin c:\ac1.3.5
del test.bin

copy NOX.TEST_HDD.PO c:\ac1.3.5\APPLEHDD.PO


cd\ac1.3.5
java -jar ac.jar -p APPLEHDD.PO NOXARCH.SYSTEM bin 0x2000 < NOXARCH.SYSTEM.bin 
java -jar ac.jar -p APPLEHDD.PO NOXARCH.MAIN bin 0x1000 < test.bin 

copy APPLEHDD.PO c:\applewin\games
copy APPLEHDD.PO c:\ADTPro-2.0.0\disks\

REM copy sbasm_test.dsk c:\USERS\MARK\DROPBOX\TEMP2\SBASM_TEST.DSK
del APPLEHDD.PO
del test.bin


ECHO LAUNCHING AppleWIN
cd\applewin
applewin.exe

cd\my_code
