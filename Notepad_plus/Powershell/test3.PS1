<#DEMO: Functions returning multiple values


#>


function get-multiplereturnvalues {
  $a = 10
  $b = 57
  $a		#returns $a
  $b
}

$return = get-multiplereturnvalues
$return[0]  # captures first return value, $a
$return[1]  # captures first return value, $b

exit
