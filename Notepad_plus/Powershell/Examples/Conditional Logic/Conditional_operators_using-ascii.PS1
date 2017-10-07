<#DEMO: Conditional Logic and Ascii Codes

The "\x4D" is a regular expression. The \x is a regular 
expression element, where the two digits following the \x
refer to a hex ascii code, which for "M" happens to be $4D.

For more information on regular expressions:
http://powershell.com/cs/blogs/ebookv2/archive/2012/03/20/chapter-13-text-and-regular-expressions.aspx
http://windowsitpro.com/powershell/windows-powershell-operators

ASCII TABLE

http://www.asciitable.com


#>

##EXAMPLE1####
$a = "M"
if ($a -match "\x4D")
 {
 "cow"
 }

##EXAMPLE2####
#matches tab character in $a

$a = "	"
if ($a -match "\x09")
 {
 "cow"
 }
 

##EXAMPLE3####
#matches space character in $a
 
$a = " "
if ($a -match "\x20")
 {
 "cow"
 }