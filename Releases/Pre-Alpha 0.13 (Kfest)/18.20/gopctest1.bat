REM ===================GOPCTEST1.BAT========================================================
REM ========PRODOS, CADIUS
REM ========FUNCTION: ASSEMBLY THE SOURCE CODE, PRODUCING BINARY FILE FOR USE WITH====
REM ==				  GOPCTEST2.BAT												  ====
REM ==================================================================================
REM
REM TO CHANGE SOURCE CODE FILES, JUST CHANGE THE FILENAME BELOW
REM

CALL sbasm c:\my_code\testing\test.asm

ECHO ===PRESS ANY KEY TO LAUNCH 2ND BATCH FILE===
PAUSE

CALL GOPCTEST2.BAT