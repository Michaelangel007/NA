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
copy c:\my_code\INCLUDES_LIBS\pdriver.asm c:\my_code\backups\%hour%.%min%
copy c:\sbasm3\*.bat c:\my_code\backups\%hour%.%min%
copy c:\my_code\*.dsk c:\my_code\backups\%hour%.%min%
copy c:\my_code\*.PO c:\my_code\backups\%hour%.%min%


copy NOXARCH.SYSTEM.BIN c:\ac1.3.5


REM copy gothic.set.bin c:\ac1.3.5

copy test.bin c:\ac1.3.5
del test.bin

REM copy PRODOS.DRIVER.BIN  c:\ac1.3.5
REM del PRODOS.DRIVER.BIN 

REM copy DATA.SHAPES.SURFACE.BIN c:\ac1.3.5
REM del DATA.SHAPES.SURFACE.BIN

copy DUMMY.FILE.BIN c:\ac1.3.5
del DUMMY.FILE.BIN



copy BOOTLDR.PRODOS_FS.DSK c:\ac1.3.5\sbasm_test.DSK
copy NOXARCH.PLAYER1.PO c:\ac1.3.5\sbasm_test2.PO


cd\ac1.3.5
java -jar ac.jar -p sbasm_test.DSK NOXARCH.SYSTEM bin 0x2000 < NOXARCH.SYSTEM.bin 
java -jar ac.jar -p sbasm_test.DSK NOXARCH.MAIN bin 0x1000 < test.bin 
java -jar ac.jar -p sbasm_test2.po DUMMY.FILE bin 0x6000 < DUMMY.FILE.BIN
REM java -jar ac.jar -p sbasm_test.dsk DUMMY.FILE bin 0x6000 < DUMMY.FILE.BIN



copy sbasm_test.dsk c:\applewin\games
copy sbasm_test2.po c:\applewin\games
copy sbasm_test.dsk c:\ADTPro-2.0.0\disks\
copy sbasm_test2.po c:\ADTPro-2.0.0\disks\

REM copy sbasm_test.po c:\USERS\MARK\DROPBOX\TEMP2\

del sbasm_test.dsk
del sbasm_test2.po

del noxarch.system.bin

del DUMMY.FILE.BIN
del test.bin


cd\my_code

