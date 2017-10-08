# Nox-Archaist
*repository files must be in C:\MY_CODE\NA for the project to build  

*Requires SBASM cross-assembler. Add it's folder to the PATH environemnt variable. I have it installed in c:\SBASM, but the project shouldn't care where it is.

*Requires AppleCommander. And it must be installed in C:\AC1.3.5. Java can't find AC.JAR unless the Java calls are executed from the folder AC.JAR resides in. The build batch files make many such calls.

*To build the project call one of the following batch files from the windows command prompt when in c:\my_code\NA\build_folder  
      go1.bat  - build 5.25" floppy disk images  
      go1cp.bat -build copy protected 5.25" floppy disk images (not fully funtional)  
      go1hd.bat -build hard drive disk image  
      
