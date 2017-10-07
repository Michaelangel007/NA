<#DEMO: Conditional Logic: OR / AND
	
#>

$a = 2
$b = 2
$c = 3


####EXAMPLE: OR####
if ($a -eq $b -OR $a -eq $c)
 {
 "cow"
 }

 
$a = 2
$b = 2
$c = 2

if ($a -eq $b -AND $a -eq $c)
 {
 "cow"
 }