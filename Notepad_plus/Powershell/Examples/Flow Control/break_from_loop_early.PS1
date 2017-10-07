<#DEMO Terminating a loop early

The "break" statement causes loop to terminate 
but program continues to run, so the statement
after the loop is printed to the screen. 

see Windows Powershell 3.0 Step by Step.PDF (p160)

#>

$ary = 1..5
ForEach($i in $ary)
{
 if($i -eq 3) { break }
$i }
"Statement following foreach loop"