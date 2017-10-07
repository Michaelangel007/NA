REM ===================GO1.BAT========================================================
REM ========FUNCTION: ASSEMBLY THE SOURCE CODE, PRODUCING BINARY FILE FOR USE WITH====
REM ==				  GO2(x).BAT												  ====
REM ==================================================================================
REM
REM TO CHANGE SOURCE CODE FILES, JUST CHANGE THE FILENAME BELOW
REM


REM compress map data
REM map.compression.bat <data file name> <target file size, $sectors>
REM sectors includes apple commander header

cd\my_code\
CALL map.compression.bat DATA.MAP.SURFACE.ASM 20
REM CALL map.compression.bat DATA.MAP.UNDERMAP_LV1.ASM 20

REM set default sectors to 20


REM copy compressed.data.map.surface.asm C:\My_Code\INCLUDES_LIBS\compressed_data

 				
REM assemble 6502 source code
CALL sbasm c:\my_code\game_loop.asm		

rem CALL sbasm c:\my_code\INCLUDES_LIBS\NOXARCH.MAIN.asm

REM CLEANUP
REM del C:\My_Code\INCLUDES_LIBS\compressed_data\*.asm

ECHO ===PRESS ANY KEY TO LAUNCH 2ND BATCH FILE===
PAUSE

CALL GO2HD.BAT
