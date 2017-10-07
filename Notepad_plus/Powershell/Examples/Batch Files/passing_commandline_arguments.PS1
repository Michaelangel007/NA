<#DEMO: Passing command line arguments to a powershell script 
called by a batch file.

Note: an example batch file is included below
	
#>

#################POWERSHELL SCRIPT#################

#STORE ARGUMENTS
$a=$args[0]		#1st command line argument
$b=$args[1]		#2nd command line argument



#VERIFY ARGUMNETS STORED CORRECTLY
Write-Host $a	#output argument #1 to the screen
Write-Host $b	#output argument #2 to the screen

exit

#################EXAMPLE BATCH FILE#################

#place this code in a batch file in the same folder
#and same name (except .bat extension) as the powershell script.
#
#executing the batch file will pass the arguments "dev" and "dev2"
#to the powershell script. 

#PowerShell.exe -NoProfile -ExecutionPolicy Bypass -Command "& '%~dpn0.ps1' dev dev2"

