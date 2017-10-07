<#DEMO: The following is a list of comparison operators
available in power shell, and a script which
uses one. Replace -lt can be replaced with
any operator on the list, though other script changes
may be needed for the operator to produce a result
that makes sense.

#>

#for more information type the following into the
#powershell command line
#get-help about_comparison_operators
#
#or see Windows Powershell 3.0 Step by Step.PDF (p. 162)
#
#Operator	Description
#-eq		Equal
#-ne		Not Equal
#-lt		Less Than
#-le		Less Than or Equal
#-gt		Greater Than 
#-ge		Greater Than or Equal 
#
#
###ADVANCED##
#In depth discussion on operators
#http://windowsitpro.com/powershell/windows-powershell-operators
#
$i = 0
While ($i -lt 5)
 {
  "`$i equals $i. This is less than  5"
  $i++
 } #end while $i lt 5