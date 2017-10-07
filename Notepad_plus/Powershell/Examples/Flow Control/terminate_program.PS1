<#DEMO Terminating Program

The "exit" statement terminates the program,
thus the statement after the loop braces does
not print onscreen

see Windows Powershell 3.0 Step by Step.PDF (p160)

#>

$ary = 1..5
ForEach($i in $ary)
{
 if($i -eq 3) { exit }
$i }
"Statement following foreach loop"