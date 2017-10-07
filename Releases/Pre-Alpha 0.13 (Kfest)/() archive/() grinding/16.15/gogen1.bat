REM ===================GOGEN1.BAT========================================================
REM ========FUNCTION: GENERATE BINARY FILES FOR USE WITH QB64====											  ====
REM ==================================================================================
REM
REM TO CHANGE SOURCE CODE FILES, JUST CHANGE THE FILENAME BELOW
REM

CALL sbasm c:\my_code\testing\qb64_testing\generate.binary.files.asm

copy data.tlk.* c:\my_code\testing\qb64_testing\
copy data.tlk.* c:\ac1.3.5
copy MASTER_CURRENT_BASIC.DSK c:\ac1.3.5\test_data.dsk

del data.tlk.*


cd\ac1.3.5
java -jar ac.jar -p test_data.dsk data.tlk.M1.00 B 0x6000 < data.tlk.M1.00.BIN
java -jar ac.jar -p test_data.dsk data.tlk.M1.08 B 0x6000 < data.tlk.M1.08.BIN


cd\my_code
