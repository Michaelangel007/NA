REM ===================GOQBTEST1.BAT========================================================
REM ========QB64
REM ========FUNCTION: COMPILE QB64 .BAS SOURCE FILE, PRODUCING .EXE FILE		  ====
REM ==				  													  ====
REM ==================================================================================
REM
REM TO CHANGE SOURCE CODE FILES, JUST CHANGE THE FILENAME BELOW
REM

CALL qb64 -c c:\my_code\testing\qb64_testing\test.bas


cd\MY_CODE\TESTING\QB64_TESTING\
copy c:\qb64\test.exe c:
CALL c:\MY_CODE\TESTING\QB64_TESTING\test.exe "data.tlk.m1.00.BIN"


CALL GOQBTEST2.BAT

REM END
