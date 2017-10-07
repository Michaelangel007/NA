<#demo do-until loop using arrays. 

For depth discussion on when to use do-while, do-until
and other types of loops, see Windows Powershell 3.0 Step by Step.PDF (p155-156)

#>

$i = 0
$ary = 1..5
Do {
$ary[$i]
 $i ++
} Until ($i -eq 5)