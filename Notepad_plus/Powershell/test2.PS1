<#DEMO:Convert from hex value to integer value
	
#>

$hex = 14	#decimal number
$int1 = [System.Convert]::ToInt32($hex,16)    #stores the hex value of $dec in $hexstr as a string. 16 refers to base16. This function can also be used to convert to a binary string (base 2)

$int1
exit
