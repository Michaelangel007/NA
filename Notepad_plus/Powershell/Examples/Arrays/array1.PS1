<#demo read file into an array
#note the format of the array $filecontents. 
#Very straightforward. 

#>


$i = 0
$fileContents = Get-Content -path textfile.txt 
While ( $i -le $fileContents.length )
 {
  $fileContents[$i]
  $i++
}