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
cd\my_code\
copy c:\my_code\testing\compression_testing\*.* c:\my_code\backups\%hour%.%min%
copy c:\sbasm3\*.bat c:\my_code\backups\%hour%.%min%
copy c:\my_code\*.dsk c:\my_code\backups\%hour%.%min%
copy c:\my_code\*.PO c:\my_code\backups\%hour%.%min%

REM copy outlaw_output.bin c:\ac1.3.5
REM del outlaw_output.bin

REM copy test.bin.lz4 c:\ac1.3.5

copy c:\my_code\testing\compression_testing\TESTFILE.BIN c:\ac1.3.5

copy NOXARCH.SYSTEM.BIN c:\ac1.3.5

copy test.bin c:\ac1.3.5
del test.bin

REM copy MASTER_DOS3.3.DSK c:\ac1.3.5\sbasm_test.dsk
REM copy MASTER_CURRENT_BASIC.DSK c:\ac1.3.5\sbasm_test.dsk
REM copy MASTER_CURRENT_BASIC.DSK c:\ac1.3.5\sbasm_test2.dsk
copy BOOTLDR.PRODOS_FS.DSK c:\ac1.3.5\sbasm_test.dsk


cd\ac1.3.5
java -jar ac.jar -p sbasm_test.dsk NOXARCH.SYSTEM bin 0x2000 < NOXARCH.SYSTEM.bin 
java -jar ac.jar -p sbasm_test.dsk NOXARCH.MAIN bin 0x1000 < TEST.BIN
java -jar ac.jar -p sbasm_test.DSK D bin 0x6000 < TESTFILE.BIN


REM java -jar ac.jar -p sbasm_test.dsk outlaw_output B 0x6000 < outlaw_output.bin
REM java -jar ac.jar -p sbasm_test.dsk test.comp B 0x6000 < test.bin.lz4


copy sbasm_test.dsk c:\applewin\games
copy sbasm_test.dsk c:\ADTPro-2.0.0\disks\

REM copy sbasm_test2.dsk c:\applewin\games
REM copy sbasm_test2.dsk c:\ADTPro-2.0.0\disks\

REM copy sbasm_test.dsk c:\USERS\MARK\DROPBOX\TEMP2\SBASM_TEST.DSK
del sbasm_test.dsk
REM del sbasm_test2.dsk
del test.bin
del TESTFILE.BIN
REM del test.bin.lz4
REM del outlaw_output.bin
cd\my_code\

