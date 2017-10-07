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
copy test*.asm c:\my_code\backups\%hour%.%min%
copy c:\my_code_temp\test*.asm c:\my_code\backups\%hour%.%min%

copy test.bin c:\ac1.3.5
copy gothic.set.bin c:\ac1.3.5
copy PRODOS.DRIVER.BIN  c:\ac1.3.5
copy OPENDSK.BIN  c:\ac1.3.5
copy DATA.SHAPES.SURFACE.BIN c:\ac1.3.5
del DATA.SHAPES.SURFACE.BIN
del test.bin
del PRODOS.DRIVER.BIN 

REM copy MASTER_DOS3.3.DSK c:\ac1.3.5\sbasm_test.dsk
REM copy testhdd.hdv c:\ac1.3.5\sbasm_test.hdv
copy MASTER_PRODOS_1.9.PO c:\ac1.3.5\sbasm_test.PO

cd\ac1.3.5
REM java -jar ac.jar -p sbasm_test.hdv test B 0x6000 < test.bin
java -jar ac.jar -p sbasm_test.po TEST bin 0x1000 < test.bin 
java -jar ac.jar -p sbasm_test.po PDRIVER bin 0x800 < PRODOS.DRIVER.BIN

java -jar ac.jar -p sbasm_test.po GOTHIC.SET bin 0x7000 < gothic.set.bin 
java -jar ac.jar -p sbasm_test.po DATA bin 0x4000 < DATA.SHAPES.SURFACE.BIN

java -jar ac.jar -p sbasm_test.po ORG bin 0x8000 < opendsk.bin 


copy sbasm_test.po c:\applewin\games
copy sbasm_test.po c:\ADTPro-2.0.0\disks\
copy sbasm_test.po c:\USERS\MARK\DROPBOX\TEMP2\
del sbasm_test.po
del test.bin
del gothic.set.bin
del DATA.SHAPES.SURFACE.BIN
del PRODOS.DRIVER.BIN 
del OPENDSK.BIN

cd\my_code

