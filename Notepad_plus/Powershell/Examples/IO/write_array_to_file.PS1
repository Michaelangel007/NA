<#demo read file (textfile.txt ) into an array then write
it to output.txt

#Good Internet articles on writing files
#http://blogs.technet.com/b/gbordier/archive/2009/05/05/powershell-and-writing-files-how-fast-can-you-write-to-a-file.aspx

#>


$i = 0
$fileContents = Get-Content -path textfile.txt 
While ( $i -le $fileContents.length )
 {
  $fileContents[$i]
  $i++
}

$fileContents | out-file ".\output.txt"