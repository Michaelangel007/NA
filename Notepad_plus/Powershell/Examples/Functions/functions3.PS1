<#DEMO: Printing output to screen in functions. 
Understanding how output and return values work in functions. 


The two functions below demonstrate there are
3 ways to print output to the screen via functions
	1) in quotes on a line alone, for example "test"
	2) printing a variable to the screen. For example, $cow
	3) using the return command. For example, return $cow. 

Notice that $moo = 	my.ftest2 results in no output
printed to the screen.

Ok, so what's going on?

First of all, why is the return statement printing
output to the screen? Second of all, why doesn't
"test" print to the screen when the function is 
called via $moo = my.ftest2?

The key concept is that functions are creating
output when "test", $cow, or return $cow is used.

When the function is called directly (i.e. myftest1, myftest2),
the output is automatically directed to the screen.

However, if the function is called to set a variable
(i.e. $moo = my.ftest2), then the output is directed
to the variable.

In the program below, add a line at the end
to display $moo to the screen and you'll find
that the "test2" output in my.ftest2 was directed
to the variables $moo right along with the output
for return $cow (6).

	
#>

function my.ftest1
{
"test"
$cow = 6
$cow
#return $cow
}

function my.ftest2
{
""
"test2"
$cow = 6
return $cow
}
	  
	my.ftest1
	my.ftest2
	
	$moo = 	my.ftest2
	

	