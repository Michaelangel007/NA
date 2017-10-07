@ECHO OFF
setlocal

REM ===================GO1.BAT========================================================
REM ===============(FLOPPY BUILD -protected)==========================================
REM ========FUNCTION: ASSEMBLY THE SOURCE CODE, PRODUCING BINARY FILE FOR USE WITH====
REM ==				  GO2(x).BAT												  ====
REM ==================================================================================
REM
REM TO CHANGE SOURCE CODE FILES, JUST CHANGE THE FILENAME BELOW
REM


REM =======COMPRESS MAP DATA====
REM ==Format: map.compression.bat <data file name> <target file size, $sectors>, <use compression flags; $01 = ON | $00 = OFF)
REM ==Note: sectors includes apple commander header

cd\my_code\
CALL map.compression.bat DATA.MAP.SURFACE.ASM 1C 1
CALL map.compression.bat DATA.MAP.UNDERMAP_LV1.ASM 1C 1

REM Note: set default sectors to 20


 				
REM =======ASSEMBLE 6502 SOURCE CODE=======
CALL sbasm c:\my_code\game_loop.asm		


		REM Is SBASM error file empty? (if yes, proceed with build process, if no then stop the script as the errors need to be fixed)
set file="errors.err"
set maxbytesize=1

FOR /F "usebackq" %%A IN ('%file%') DO set size=%%~zA

if %size% LSS %maxbytesize% (

GOTO NOERRORS

) ELSE (

    ECHO ****SBASM REPORTS ERRORS THAT NEED FIXING****
	EXIT /B
)


:NOERRORS

REM =======COMPRESS NPC SPEECH TEXT===========
cd\my_code\compression\NPC.Speech.Text\DATA

	REM move binary files (unpacked speech text) generated by SBASM
move c:\my_code\DATA.TLK.*.BIN c:

	REM LOCATION 1
del C:\MY_CODE\INCLUDES_LIBS\COMPRESSED_DATA\NPC.SPEECH.TEXT\D.TLK.L1.M1.BIN
CALL binary.collator.exe DATA.TLK.L001.00.BIN

	copy *.bin c:\ac1.3.5
	copy *.zx7 c:\ac1.3.5
	
cd\my_code\





REM =======CLEANUP=================
REM del C:\My_Code\INCLUDES_LIBS\compressed_data\*.asm

REM delete the packed data files created by binary.collator.exe
cd\MY_CODE\COMPRESSION\NPC.SPEECH.TEXT\DATA\
del *.ZX7
cd\my_code\


REM ======NEXT BUILD STAGE=====
REM ======(create disk images)
REM ======HANDOFF CONTROL======

CALL GO2CP.BAT
