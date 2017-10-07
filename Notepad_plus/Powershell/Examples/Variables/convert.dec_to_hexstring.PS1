<#DEMO:Convert from decimal to hex string
	
#>

$dec = 64636	#decimal number
$hexstr = [System.Convert]::ToString($dec,16)    #stores the hex value of $dec in $hexstr as a string. 16 refers to base16. This function can also be used to convert to a binary string (base 2)

$hexstr
exit
