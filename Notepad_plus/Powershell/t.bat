REM Make Backup Copy of Code Iteration
set Hour=%Time:~0,2%
if "%hour:~0,1%" == " " set hour=0%hour:~1,1%
set Min=%Time:~3,2%

mkdir c:\my_code\powershell\ps_backups\%hour%.%min%
copy c:\my_code\powershell\map.compression.* c:\my_code\powershell\ps_backups\%hour%.%min%

cd\my_code\powershell\

REM Execute Script
map.compression.bat
