REM ===================GOPTEST2d.BAT========================================================
REM ========FUNCTION: ASSEMBLY SINGLE TARGET FILE PROGRAMS TO BE LOADED BY ProDOS====
REM ========(use after GOPTEST1d.BAT)
REM ==================================================================================
REM
REM Make Backup Copy of Code Iteration
set Hour=%Time:~0,2%
if "%hour:~0,1%" == " " set hour=0%hour:~1,1%
set Min=%Time:~3,2%

mkdir c:\my_code\backups\%hour%.%min%
cd\my_code
copy c:\my_code\testing\prodos_testing\*.* c:\my_code\backups\%hour%.%min%
copy c:\my_code\INCLUDES_LIBS\ptools.asm c:\my_code\backups\%hour%.%min%
copy c:\sbasm3\*.bat c:\my_code\backups\%hour%.%min%
copy c:\my_code\*.dsk c:\my_code\backups\%hour%.%min%
copy c:\my_code\*.PO c:\my_code\backups\%hour%.%min%


copy NOXARCH.SYSTEM.BIN c:\ac1.3.5


REM copy gothic.set.bin c:\ac1.3.5

copy test.bin c:\ac1.3.5
del test.bin

REM copy DATA.SPR.SURFACE.BIN c:\ac1.3.5
REM del DATA.SPR.SURFACE.BIN

copy DUMMY.FILE.BIN c:\ac1.3.5
del DUMMY.FILE.BIN


copy NOX.TEST_HDD.PO c:\ac1.3.5\APPLEHDD.PO
REM copy BOOTLDR.PRODOS_FS.DSK c:\ac1.3.5\sbasm_test.DSK
REM copy NOXARCH.PLAYER1.PO c:\ac1.3.5\sbasm_test2.PO


cd\ac1.3.5
java -jar ac.jar -p APPLEHDD.PO NOXARCH.SYSTEM bin 0x2000 < NOXARCH.SYSTEM.bin 
java -jar ac.jar -p APPLEHDD.PO NOXARCH.MAIN bin 0x1000 < test.bin 
REM java -jar ac.jar -p sbasm_test.DSK DUMMY.FILE bin 0x6000 < DUMMY.FILE.BIN
java -jar ac.jar -p APPLEHDD.PO DUMMY.FILE bin 0x6000 < DUMMY.FILE.BIN

REM java -jar ac.jar -p APPLEHDD.PO DATA.SPR.SURF bin 0x6000 < DATA.SPR.SURFACE.BIN
REM java -jar ac.jar -p sbasm_test2.po DATA.SPR.SURF bin 0x6000 < DATA.SPR.SURFACE.BIN

REM ####FLOPPY VERSION
REM cd\ac1.3.5
REM java -jar ac.jar -p sbasm_test.DSK NOXARCH.SYSTEM bin 0x2000 < NOXARCH.SYSTEM.bin 
REM java -jar ac.jar -p sbasm_test.DSK NOXARCH.MAIN bin 0x1000 < test.bin 
REM REM java -jar ac.jar -p sbasm_test.DSK DUMMY.FILE bin 0x6000 < DUMMY.FILE.BIN
REM java -jar ac.jar -p sbasm_test2.po DUMMY.FILE bin 0x6000 < DUMMY.FILE.BIN

REM REM java -jar ac.jar -p sbasm_test.DSK DATA.SPR.SURF bin 0x6000 < DATA.SPR.SURFACE.BIN
REM REM java -jar ac.jar -p sbasm_test2.po DATA.SPR.SURF bin 0x6000 < DATA.SPR.SURFACE.BIN


REM ####DISTRIBUTION: HARD DRIVE
copy APPLEHDD.PO c:\applewin\games
copy APPLEHDD.PO c:\ADTPro-2.0.0\disks\

REM ####DISTRIBUTION: FLOPPY
REM copy sbasm_test.dsk c:\applewin\games
REM copy sbasm_test2.po c:\applewin\games
REM copy sbasm_test.dsk c:\ADTPro-2.0.0\disks\
REM copy sbasm_test2.po c:\ADTPro-2.0.0\disks\

REM copy sbasm_test.po c:\USERS\MARK\DROPBOX\TEMP2\

REM ====CLEANUP====: 
REM (delete the files created in this iteration to avoid mixups in future iterations)
REM (update: I decided to just delete all binary files in the C:\AC1.3.5 directory
REM (because I really don't keep any binary files there. They are all temporary)
del *.bin
del *.zx7
del *.dsk
del *.po

cd\my_code