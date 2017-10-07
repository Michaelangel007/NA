<#DEMO:Convert from hex value to integer value
	
#>

$hex = 14	#decimal number
$int1 = [System.Convert]::ToInt32($hex,16)    #stores the integer value of $hex in $int1. 16 refers to base16. This function can also be used to convert from binary (base 2) to integer.

$int1
exit
