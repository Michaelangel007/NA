<#The backtick ` character is used to print the 
variable name. When the ` character is not used, 
powshell displays the value of the 
variable ($i in the example below)

#>


$i = 0
While ($i -lt 5)
 {
  "`$i equals $i. This is less than  5"
  $i++
 } #end while $i lt 5