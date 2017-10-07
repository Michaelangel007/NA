
REM
REM
@echo off

REM ****TROUBLESHOOTING*****
REM Error message "1 was unexpected at this time" means that %file% was not found
REM


setlocal
set file="errors.err"
set maxbytesize=1

FOR /F "usebackq" %%A IN ('%file%') DO set size=%%~zA

	
if %size% LSS %maxbytesize% (

    echo is ^< %maxbytesize% bytes
GOTO COW

) ELSE (

    echo is ^>= %maxbytesize% bytes
	EXIT /B
)


:COW
echo moo




