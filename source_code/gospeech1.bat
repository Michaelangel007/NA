REM ===================GOSPEECH1.BAT========================================================
REM ========QB64
REM ========FUNCTION: COMPILE QB64 .BAS SOURCE FILE, PRODUCING .EXE FILE		  ====
REM ========[OPTIONAL: RUN PROGRAM AND BUILD DISK IMAGE]				  													  ====
REM ==================================================================================
REM
REM TO CHANGE SOURCE CODE FILES, JUST CHANGE THE FILENAME BELOW
REM



REM ==========COMPILE PROGRAM====
CALL qb64 -c c:\my_code\compression\NPC.Speech.Text\binary.collator.bas


cd\my_code\compression\NPC.Speech.Text\data
copy c:\qb64\binary.collator.exe c:
del c:\qb64\binary.collator.exe

cd\my_code


REM ==========RUN PROGRAM======


cd\my_code
copy MASTER_CURRENT_BASIC.DSK c:\ac1.3.5\test_data.dsk


cd\my_code\compression\NPC.Speech.Text\data


del C:\MY_CODE\INCLUDES_LIBS\COMPRESSED_DATA\NPC.SPEECH.TEXT\D.TLK.L1.M1.BIN
REM CALL binary.collator.exe DATA.TLK.L1.M1.00.BIN


REM CALL GOSPEECH2.BAT

 

