REM ===================GOTEST1.BAT========================================================
REM ========DOS3.3, APPLECOMMANDER
REM ========FUNCTION: ASSEMBLY THE SOURCE CODE, PRODUCING BINARY FILE FOR USE WITH====
REM ==				  GOTEST2.BAT												  ====
REM ==================================================================================
REM
REM TO CHANGE SOURCE CODE FILES, JUST CHANGE THE FILENAME BELOW
REM

REM CALL sbasm c:\my_code\testing\test5.asm
REM CALL sbasm C:\My_Code\testing\sound_generation\sound.generator.asm
CALL sbasm C:\My_Code\testing\sound_generation\sound.generator.master.asm

ECHO ===PRESS ANY KEY TO LAUNCH 2ND BATCH FILE===
PAUSE

CALL GOSOUNDTEST2.BAT