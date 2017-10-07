<#demo read file into an array

#>


$i = 0
$fileContents = Get-Content -path textfile.txt 
While ( $i -le $fileContents.length )
 {
  $fileContents[$i]
  $i++
}

#the contents of the file textfile.txt are now
#stored in the array $fileContents.
#
#each elements can be accessed via the usual array
#notation $fileContents[0], $fileContents[1], etc.
#
#****the contents can also be accessed as a string, 
#
#the string contents are stored in the last element of the the array.
#since the first element in the array is 0, the
#way to do it is $fileContents[$fileContents.length-1]
#note that adding .length to an array name is actually
#a powershell command that returns the array length. 
#so putting $fileContents[$fileContents.length-1]
#in your code would display the contents of $filecontents 
#array as a string. 
#
#A file with multiple lines results in a multi-dimensional
#array with one top level dimension per line. 