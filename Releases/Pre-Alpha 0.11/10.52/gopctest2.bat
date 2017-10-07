REM ===================GOPTEST2.BAT========================================================
REM ========FUNCTION: ASSEMBLY SINGLE TARGET FILE PROGRAMS TO BE LOADED BY ProDOS====
REM ========(use after GOPTEST1.BAT)
REM ==================================================================================
REM
REM Make Backup Copy of Code Iteration
set Hour=%Time:~0,2%
if "%hour:~0,1%" == " " set hour=0%hour:~1,1%
set Min=%Time:~3,2%

mkdir c:\my_code\backups\%hour%.%min%
cd\my_code
copy c:\my_code\testing\prodos_testing\*.* c:\my_code\backups\%hour%.%min%
copy c:\sbasm3\*.bat c:\my_code\backups\%hour%.%min%
copy c:\my_code\*.PO c:\my_code\backups\%hour%.%min%
copy c:\my_code\*.dsk c:\my_code\backups\%hour%.%min%


copy test.bin c:\cadius_v1
del test.bin


REM copy MASTER_DOS3.3.DSK c:\ac1.3.5\sbasm_test.dsk
REM copy testhdd.hdv c:\ac1.3.5\sbasm_test.hdv
copy MASTER_PRODOS_1.9.PO c:\cadius_v1\sbasm_test.PO

cd\cadius_v1
cadius.exe addfile c:\cadius_v1\sbasm_test.PO /NEW.DISK/ c:\cadius_v1\test.bin 

copy sbasm_test.PO c:\applewin\games
copy sbasm_test.PO c:\ADTPro-2.0.0\disks\

REM copy sbasm_test.po c:\USERS\MARK\DROPBOX\TEMP2\

del sbasm_test.PO

del test.bin


cd\my_code

