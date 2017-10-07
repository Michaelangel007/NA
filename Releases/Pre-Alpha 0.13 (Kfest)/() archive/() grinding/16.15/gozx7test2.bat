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

copy COMP_ZX7.PROG.BIN c:\ac1.3.5
del COMP_ZX7.PROG.BIN

copy COMP_ZX7.BOOT.BIN c:\ac1.3.5
del COMP_ZX7.BOOT.BIN

REM copy C:\MY_CODE\INCLUDES_LIBS\COMPRESSED_DATA\NPC.SPEECH.TEXT\D.TLK.L1.M1.BIN c:\ac1.3.5

cd\my_code\testing\compression_testing\zx7
copy testfile.packed.bin.zx7 c:\ac1.3.5
copy testfile.unpacked.bin c:\ac1.3.5
copy D.TLK.L1.M1.BIN c:\ac1.3.5

cd\my_code

copy C:\MY_CODE\TESTING\COMPRESSION_TESTING\ZX7\COMP_ZX7.TEST.BOOT.ASM c:\ac1.3.5
copy C:\MY_CODE\testing\compression_testing\zx7\zx7.bf00.bin c:\ac1.3.5

copy BOOTLDR.PRODOS_FS.DSK c:\ac1.3.5\sbasm_test.DSK


cd\ac1.3.5
java -jar ac.jar -p sbasm_test.DSK NOXARCH.SYSTEM bin 0x2000 < NOXARCH.SYSTEM.bin 
java -jar ac.jar -p sbasm_test.DSK NOXARCH.MAIN bin 0x1000 < COMP_ZX7.BOOT.BIN 
java -jar ac.jar -p sbasm_test.DSK COMP_ZX7.PROG bin 0xB000 < COMP_ZX7.PROG.BIN
REM java -jar ac.jar -p sbasm_test.DSK UNPACKED.DATA bin 0xA000 < testfile.unpacked.bin
REM java -jar ac.jar -p sbasm_test.DSK PACKED.DATA bin 0xBA00 < testfile.packed.bin.zx7
java -jar ac.jar -p sbasm_test.DSK ZX7 bin 0x6000 < ZX7.BF00.BIN

java -jar ac.jar -p sbasm_test.DSK PACKED.DATA bin 0xBA00 < D.TLK.L1.M1.BIN



copy sbasm_test.dsk c:\applewin\games
copy sbasm_test.dsk c:\ADTPro-2.0.0\disks\

REM ====CLEANUP====: 
REM (delete the files created in this iteration to avoid mixups in future iterations)
REM (update: I decided to just delete all binary files in the C:\AC1.3.5 directory
REM (because I really don't keep any binary files there. They are all temporary)
del *.bin
del *.zx7
del *.dsk
del *.po

cd\my_code

