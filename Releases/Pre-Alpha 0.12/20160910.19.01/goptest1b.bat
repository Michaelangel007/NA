REM ===================GOPTEST1b.BAT========================================================
REM ========PRODOS, APPLECOMMANDER
REM ========FUNCTION: ASSEMBLY THE SOURCE CODE, PRODUCING BINARY FILE FOR USE WITH====
REM ==				  GOPTEST2b.BAT												  ====
REM ==================================================================================
REM
REM TO CHANGE SOURCE CODE FILES, JUST CHANGE THE FILENAME BELOW
REM

REM CALL sbasm c:\my_code\testing\prodos_testing\ptest.asm
CALL sbasm c:\my_code\testing\prodos_testing\ptest_b.asm

ECHO ===PRESS ANY KEY TO LAUNCH 2ND BATCH FILE===
PAUSE

CALL GOPTEST2b.BAT	