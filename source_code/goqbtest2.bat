REM ===================GOQBTEST2.BAT========================================================
REM ========FUNCTION: backup code and build disk image=====
REM ========(use after GOQBTEST1.BAT)
REM ==================================================================================
REM


REM Make Backup Copy of Code Iteration
set Hour=%Time:~0,2%
if "%hour:~0,1%" == " " set hour=0%hour:~1,1%
set Min=%Time:~3,2%

mkdir c:\my_code\backups\%hour%.%min%

copy c:\my_code\testing\qb64_testing\*.* c:\my_code\backups\%hour%.%min%
copy c:\qb64\*.exe c:\my_code\backups\%hour%.%min%

copy c:\qb64\*.bat c:\my_code\backups\%hour%.%min%



copy OUTPUT.BIN c:\ac1.3.5
copy data.tlk.m1.00.BIN.LZ4 c:\ac1.3.5
copy data.tlk.m1.08.BIN.LZ4 c:\ac1.3.5
copy data.tlk.m1.10.BIN.LZ4 c:\ac1.3.5

cd\my_code
copy MASTER_CURRENT_BASIC.DSK c:\ac1.3.5\test_data.dsk

cd\ac1.3.5
java -jar ac.jar -p test_data.dsk output B 0x6000 < output.bin
java -jar ac.jar -p test_data.dsk packed.m1.00 B 0x6000 < data.tlk.m1.00.BIN.LZ4
java -jar ac.jar -p test_data.dsk packed.m1.08 B 0x6000 < data.tlk.m1.08.BIN.LZ4
java -jar ac.jar -p test_data.dsk packed.m1.10 B 0x6000 < data.tlk.m1.10.BIN.LZ4


copy test_data.dsk c:\applewin\games

del test_data.dsk
REM del output.bin



REM DELETE TEMP FILES

cd\MY_CODE\TESTING\QB64_TESTING\
del OUTPUT.BIN
del data.tlk.m1.00.BIN.LZ4
del data.tlk.m1.08.BIN.LZ4
del data.tlk.m1.10.BIN.LZ4

cd\my_code