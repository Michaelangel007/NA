<#DEMO ELSE-IF statement
can be used for multiple conditions. Also see Switch

see Windows Powershell 3.0 Step by Step.PDF (p. 192)

#>


$a = 3
If ($a -eq 5)
{
 '$a equals 5'
}
ElseIf ($a -eq 3)
{
 '$a is equal to 3'
}
Else 
{
 '$a does not equal 3 or 5'
}
	  