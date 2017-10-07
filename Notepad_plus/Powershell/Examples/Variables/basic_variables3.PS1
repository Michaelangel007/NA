<#demo restricting variable to type integer
For a list of variable type aliases 
see Windows Powershell 3.0 Step by Step.PDF (p 146)
#>

$a = "this is a string"
[int]$b = 20

$b = $a + $b

$b

