<#DEMO Define an Array (2 methods)

An array must be defined before values can be assigned
to it. There is no way I've found to define an array
without assigning values.

Thus, if an array is needed for use by the program
(perhaps for calculations being done an an array read
in from a file), define an array with arbitrary values
using the 1st method (range), then use a loop to init
the values to 0. 

For more inforamtion on Arrays:

https://technet.microsoft.com/en-us/library/hh847882(v=wps.620).aspx

#>

$a = 0..5 #define an array using a range. This example creates an array with 6 elements, with the numbers inbetween the first value (0) and the second value (5)

$b = 0,10,50,"Bob" #define an array with hard coded values

$a[1]

$b[3]