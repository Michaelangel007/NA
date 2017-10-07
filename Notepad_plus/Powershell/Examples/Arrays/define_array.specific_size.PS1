<#DEMO: Define an array using a specific array size.

	
#>
####SINGLE DIMENSION ARRAY###########
#Create Array
$reader = 5000

$arr = new-object object[] $reader	#creates a new array called $arr with a size of 5000 (elements will be numbered 0 - 4999)


#Set Array Values
$arr[5] = 8
$arr[4999] = 5

#Print Contents of Select Arraty Elements
$arr[5]
$arr[4999]




####MULTI-DIMENSION ARRAY###########
$dim0 = 10
$dim1 = 20

$array2 = New-Object 'object[,]' $dim0,$dim1
$array2[4,8] = 'Hello'
$array2[9,16] = 'Test'
$array2