REM ===================GOTEST1.BAT========================================================
REM ========DOS3.3, APPLECOMMANDER
REM ========FUNCTION: ASSEMBLY THE SOURCE CODE, PRODUCING BINARY FILE FOR USE WITH====
REM ==				  GOTEST2.BAT												  ====
REM ==================================================================================
REM
REM TO CHANGE SOURCE CODE FILES, JUST CHANGE THE FILENAME BELOW
REM

CALL sbasm c:\my_code\testing\test.asm

ECHO ===PRESS ANY KEY TO LAUNCH 2ND BATCH FILE===
PAUSE

CALL GOTEST2.BAT