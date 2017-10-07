REM ===================GOZX7TEST1.BAT========================================================
REM ========PRODOS, APPLECOMMANDER
REM ========FUNCTION: ASSEMBLY THE SOURCE CODE, PRODUCING BINARY FILE FOR USE WITH====
REM ==				  GOZX7TEST2.BAT												  ====
REM ==================================================================================
REM
REM TO CHANGE SOURCE CODE FILES, JUST CHANGE THE FILENAME BELOW
REM

CALL sbasm c:\my_code\testing\compression_testing\ZX7\comp_zx7.test.asm

ECHO ===PRESS ANY KEY TO LAUNCH 2ND BATCH FILE===
PAUSE

CALL GOZX7TEST2.BAT	