<#demo of writing the contents of a variable to the screen
while in a function set equal to a variable.

Use write-host

For example, if a function called Test were called like this:

$cow = Test

Then any output in Test would end up in $cow. This includes
return commands and the other methods of printing to the screen
such as:

"print to screen"

and

$a   #just type the variable name


However if you use write-host then powershell knows you
really want the output to go to the screen. 

#>

$a = "Hello, World!"
Write-Host $a


