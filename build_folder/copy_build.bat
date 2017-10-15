
REM copy the disk images for the current build to the AppleWIN folder
REM this is used after restoring a GIT commit, which has the disk images associated with the commit in c:\my_code\na\build_folder
REM but they need to be copied to AppleWIN



copy c:\my_code\na\build_folder\*.PO c:\applewin\games\
copy c:\my_code\na\build_folder\*.DSK c:\applewin\games\

copy c:\my_code\na\build_folder\*.PO c:\AppleWIN_1.26.2.2\
copy c:\my_code\na\build_folder\*.DSK c:\AppleWIN_1.26.2.2\
