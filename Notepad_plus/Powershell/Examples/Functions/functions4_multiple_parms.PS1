<#DEMO: Function with two parameters. 

Note that the statement at the end calling the function
does not have the parameters in parenthesis and
does not seperate the arguments with commas.

This is really dumn but it is the way it works.

Calling a function while passing one parameter can be 
done with the parm in (). 

And, when defining a function, the parameter list is
in () sepearated by commas. Again this seems really dumb
and I just spent a couple hours horsing around with it. 


	
#>

function get.test ($a, $b)
{
	$a
	$b
}


####MULTI-DIMENSION ARRAY###########
$dim0 = 10
$dim1 = 20

$array2 = New-Object 'object[,]' $dim0,$dim1
$array2[0,0] = " "
$array2[0,1] = "COW"
$array2[0,2] = "."
$array2[0,3] = "0"
$array2[0,4] = "B"
$array2[1,0] = "0"
$array2[1,1] = "A"


$a = 5
$b = 11
get.test $array2 $b  #CALL FUNCTION
