@ECHO OFF
PowerShell.exe -NoProfile -ExecutionPolicy Bypass -Command "& '%~dpn0.ps1' %1 %2"
REM PAUSE
@ECHO ON
