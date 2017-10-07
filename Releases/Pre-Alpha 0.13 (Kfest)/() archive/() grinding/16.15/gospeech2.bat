REM ===================GOSPEECH1.BAT========================================================
REM ========FUNCTION: backup code and build disk image=====
REM ========(use after GOSPEECH2.BAT)
REM ==================================================================================
REM


REM Make Backup Copy of Code Iteration
set Hour=%Time:~0,2%
if "%hour:~0,1%" == " " set hour=0%hour:~1,1%
set Min=%Time:~3,2%

mkdir c:\my_code\backups\%hour%.%min%

copy c:\my_code\compression\NPC.Speech.Text\data\*.* c:\my_code\backups\%hour%.%min%
copy c:\my_code\compression\NPC.Speech.Text\*.bas c:\my_code\backups\%hour%.%min%

copy c:\qb64\*.bat c:\my_code\backups\%hour%.%min%



cd\my_code\compression\NPC.Speech.Text\data

copy *.bin c:\ac1.3.5
copy *.zx7 c:\ac1.3.5
REM del *.zx7


copy C:\MY_CODE\INCLUDES_LIBS\COMPRESSED_DATA\NPC.SPEECH.TEXT\*.bin c:\ac1.3.5

cd\ac1.3.5
java -jar ac.jar -p test_data.dsk D.TLK.L1.M1 bin 0xA000 < D.TLK.L1.M1.BIN


java -jar ac.jar -p test_data.dsk unpacked.00 bin 0xA003 < DATA.TLK.L1.M1.00.BIN
java -jar ac.jar -p test_data.dsk packed.00 bin 0xA003 < DATA.TLK.L1.M1.00.BIN.ZX7

java -jar ac.jar -p test_data.dsk unpacked.08 bin 0xA103 < DATA.TLK.L1.M1.08.BIN
java -jar ac.jar -p test_data.dsk packed.08 bin 0xA103 < DATA.TLK.L1.M1.08.BIN.ZX7

java -jar ac.jar -p test_data.dsk unpacked.10 bin 0xA103 < DATA.TLK.L1.M1.10.BIN
java -jar ac.jar -p test_data.dsk packed.10 bin 0xA103 < DATA.TLK.L1.M1.10.BIN.ZX7


copy test_data.dsk c:\applewin\games\test_data.dsk

del *.dsk
del *.po
del *.bin
del *.zx7

cd\my_code



 

