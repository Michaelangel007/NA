REM =====TEMPLATE====
REM =USE TO RUN POWERSHELLS SCRIPT FROM A REGULAR BATCH FILE==
REM =
REM =save this file as regular batch file in the same 
REM =directory as the powershell script. Give it the 
REM =same name, but with a .bat extension. So if you 
REM =want to run test.ps1 from a batch file, save this file
REM as test.bat. 
REM
REM Good Internet article on running powershell from batch files
REM http://www.howtogeek.com/204088/how-to-use-a-batch-file-to-make-powershell-scripts-easier-to-run/


@ECHO OFF
PowerShell.exe -NoProfile -ExecutionPolicy Bypass -Command "& '%~dpn0.ps1'"
REM PAUSE
REM add -noexit parameter to stay in powershell after script completes
