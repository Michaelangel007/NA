REM ===================GO1.BAT========================================================
REM ========FUNCTION: ASSEMBLY THE SOURCE CODE, PRODUCING BINARY FILE FOR USE WITH====
REM ==				  GO2(x).BAT												  ====
REM ==================================================================================
REM
REM TO CHANGE SOURCE CODE FILES, JUST CHANGE THE FILENAME BELOW
REM
REM sbasm c:\my_code\graphics_engine.asm
REM sbasm c:\my_code\examples\math.subtract-16.prep.asm
REM sbasm c:\my_code\examples\math.add-16.prep.asm
REM sbasm c:\my_code\examples\math.if-greater-16.prep.asm
REM sbasm c:\my_code\examples\rwts2.asm
REM sbasm c:\my_code\graphics_engine0.18.asm
REM sbasm c:\my_code\examples\graphics\graphics.print.text.asm
REM sbasm c:\my_code\simple.draw.asm
REM sbasm c:\my_code\test2.asm
REM sbasm c:\my_code\controller.hrcg.asm
REM sbasm c:\my_code\graphics.print.text0.1.asm
REM sbasm c:\my_code\includes_libs\darkness_manager.asm
REM sbasm c:\my_code\zone_functions.asm



REM compress map data
REM map.compression.bat <data file name> <target file size, $sectors>
REM sectors includes apple commander header

cd\my_code\
REM CALL map.compression.bat DATA.MAP.SURFACE.ASM 20
REM CALL map.compression.bat DATA.MAP.UNDERMAP_LV1.ASM 20

REM set default sectors to 20


REM copy compressed.data.map.surface.asm C:\My_Code\INCLUDES_LIBS\compressed_data

 				
REM assemble 6502 source code
CALL sbasm c:\my_code\game_loop.asm		

REM CLEANUP
REM del C:\My_Code\INCLUDES_LIBS\compressed_data\*.asm


ECHO SBASM NOW RETURNS TO BATCH FILE, MERGE TOGEHER

