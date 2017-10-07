REM ===================GOPTEST2b.BAT========================================================
REM ========FUNCTION: ASSEMBLY SINGLE TARGET FILE PROGRAMS TO BE LOADED BY ProDOS====
REM ========(use after GOPTEST1b.BAT)
REM ==================================================================================
REM
REM Make Backup Copy of Code Iteration
set Hour=%Time:~0,2%
if "%hour:~0,1%" == " " set hour=0%hour:~1,1%
set Min=%Time:~3,2%

mkdir c:\my_code\backups\%hour%.%min%
cd\my_code
copy c:\my_code\testing\compression_testing\*.* c:\my_code\backups\%hour%.%min%
copy c:\my_code\INCLUDES_LIBS\OpenDir.asm c:\my_code\backups\%hour%.%min%
copy c:\sbasm3\*.bat c:\my_code\backups\%hour%.%min%
copy c:\my_code\*.dsk c:\my_code\backups\%hour%.%min%
copy c:\my_code\*.PO c:\my_code\backups\%hour%.%min%


copy NOXARCH.SYSTEM.BIN c:\ac1.3.5

copy COMP.PROG.BIN c:\ac1.3.5
del COMP.PROG.BIN

copy COMP.BOOT.BIN c:\ac1.3.5
del COMP.BOOT.BIN

REM copy test.packed.data.LZ4 c:\ac1.3.5

REM copy test.unpacked.data.bin c:\ac1.3.5



copy BOOTLDR.PRODOS_FS.DSK c:\ac1.3.5\sbasm_test.DSK


cd\ac1.3.5
java -jar ac.jar -p sbasm_test.DSK NOXARCH.SYSTEM bin 0x2000 < NOXARCH.SYSTEM.bin 
java -jar ac.jar -p sbasm_test.DSK NOXARCH.MAIN bin 0x1000 < COMP.BOOT.BIN 
java -jar ac.jar -p sbasm_test.DSK COMP.PROG bin 0x6000 < COMP.PROG.BIN
java -jar ac.jar -p sbasm_test.DSK lz4 bin 0x6000 < LZ4.BIN
java -jar ac.jar -p sbasm_test.DSK PACKED.DATA bin 0x6000 < OUTPUT.BIN


REM java -jar ac.jar -p sbasm_test.DSK test.unpacked.data bin 0x6000 < test.unpacked.data.bin
REM java -jar ac.jar -p sbasm_test.DSK PACKED.DATA bin 0x6000 < TEST.PACKED.DATA.LZ4



copy sbasm_test.dsk c:\applewin\games
copy sbasm_test.dsk c:\ADTPro-2.0.0\disks\


del sbasm_test.dsk

del noxarch.system.bin

REM del test.packed.data.LZ4
REM del test.unpacked.data.bin

del COMP.PROG.BIN
del COMP.BOOT.BIN

cd\my_code

