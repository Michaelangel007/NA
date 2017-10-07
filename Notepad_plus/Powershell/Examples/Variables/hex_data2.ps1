<#DEMO: convert hex string to byte array, do math
on the byte array, then convert byte array to hex string,
then output to file.

A byte array is the format powershell needs to 
do hex math. 

a hex string is data that the programmer knows 
contains hex values but powershell treats as a string.


#>

####FUNCTIONS DOWNLOADED ONLINE########
#https://cyber-defense.sans.org/blog/2010/02/11/powershell-byte-array-hex-convert
########################################




function Convert-HexStringToByteArray
{
################################################################
#.Synopsis
# Convert a string of hex data into a System.Byte[] array. An
# array is always returned, even if it contains only one byte.
#.Parameter String
# A string containing hex data in any of a variety of formats,
# including strings like the following, with or without extra
# tabs, spaces, quotes or other non-hex characters:
# 0x41,0x42,0x43,0x44
# \x41\x42\x43\x44
# 41-42-43-44
# 41424344
# The string can be piped into the function too.
################################################################
[CmdletBinding()]
Param ( [Parameter(Mandatory = $True, ValueFromPipeline = $True)] [String] $String )
 
#Clean out whitespaces and any other non-hex crud.
$String = $String.ToLower() -replace '[^a-f0-9\\,x\-\:]',"
 
#Try to put into canonical colon-delimited format.
$String = $String -replace '0x|\x|\-|,',':'
 
#Remove beginning and ending colons, and other detritus.
$String = $String -replace '^:+|:+$|x|\',"
 
#Maybe there's nothing left over to convert...
if ($String.Length -eq 0) { ,@() ; return }
 
#Split string with or without colon delimiters.
if ($String.Length -eq 1)
{ ,@([System.Convert]::ToByte($String,16)) }
elseif (($String.Length % 2 -eq 0) -and ($String.IndexOf(":") -eq -1))
{ ,@($String -split '([a-f0-9]{2})' | foreach-object { if ($_) {[System.Convert]::ToByte($_,16)}}) }
elseif ($String.IndexOf(":") -ne -1)
{ ,@($String -split ':+' | foreach-object {[System.Convert]::ToByte($_,16)}) }
else
{ ,@() }
#The strange ",@(...)" syntax is needed to force the output into an
#array even if there is only one element in the output (or none).
}

function Convert-ByteArrayToHexString
{
################################################################
#.Synopsis
# Returns a hex representation of a System.Byte[] array as
# one or more strings. Hex format can be changed.
#.Parameter ByteArray
# System.Byte[] array of bytes to put into the file. If you
# pipe this array in, you must pipe the [Ref] to the array.
# Also accepts a single Byte object instead of Byte[].
#.Parameter Width
# Number of hex characters per line of output.
#.Parameter Delimiter
# How each pair of hex characters (each byte of input) will be
# delimited from the next pair in the output. The default
# looks like "0x41,0xFF,0xB9" but you could specify "\x" if
# you want the output like "\x41\xFF\xB9" instead. You do
# not have to worry about an extra comma, semicolon, colon
# or tab appearing before each line of output. The default
# value is ",0x".
#.Parameter Prepend
# An optional string you can prepend to each line of hex
# output, perhaps like '$x += ' to paste into another
# script, hence the single quotes.
#.Parameter AddQuotes
# A switch which will enclose each line in double-quotes.
#.Example
# [Byte[]] $x = 0x41,0x42,0x43,0x44
# Convert-ByteArrayToHexString $x
#
# 0x41,0x42,0x43,0x44
#.Example
# [Byte[]] $x = 0x41,0x42,0x43,0x44
# Convert-ByteArrayToHexString $x -width 2 -delimiter "\x" -addquotes
#
# "\x41\x42"
# "\x43\x44"
################################################################
[CmdletBinding()] Param (
[Parameter(Mandatory = $True, ValueFromPipeline = $True)] [System.Byte[]] $ByteArray,
[Parameter()] [Int] $Width = 10,
[Parameter()] [String] $Delimiter = ",0x",
[Parameter()] [String] $Prepend = "",
[Parameter()] [Switch] $AddQuotes )
 
if ($Width -lt 1) { $Width = 1 }
if ($ByteArray.Length -eq 0) { Return }
$FirstDelimiter = $Delimiter -Replace "^[\,\:\t]",""
$From = 0
$To = $Width - 1
Do
{
$String = [System.BitConverter]::ToString($ByteArray[$From..$To])
$String = $FirstDelimiter + ($String -replace "\-",$Delimiter)
if ($AddQuotes) { $String = '"' + $String + '"' }
if ($Prepend -ne "") { $String = $Prepend + $String }
$String
$From += $Width
$To += $Width
} While ($From -lt $ByteArray.Length)
}

##################
#MY FUNCTIONS    #
##################


function Get.MyData
{
$i = 0
$fileContents = Get-Content -path hexinput.txt 
While ( $i -le $fileContents.length )
 {
  $fileContents[$i]
  $i++
}
return $filecontents
}

function Convert.MyData ($input_data)
{
#CONVERT HEX STRING ARRAY (RAW DATA) TO HEX STRING
#$input_data = "0","F", "0","B"

$hexstr = $input_data[0]+$input_data[1]

$hexstr = $hexstr + $input_data[2] + $input_data[3]

#CONVERT HEX STRING TO BYTE ARRAY
$byteary = Convert-HexStringToByteArray ($hexstr)
return $byteary
}


####MY CODE#####

#DEFINE ARRAYS

#GET INPUT

$filecontents = Get.MyData


#CONVERT INPUT
$byteary = Convert.MyData($filecontents)

#DO THE MATH (ON THE BYTE ARRAY)
$byteary[0] = $byteary[0] + 1
$byteary[1] = $byteary[1] + 1


#CONVERT BYTE ARRAY TO HEX STRING

$hexstr = Convert-ByteArrayToHexString $byteary -width 2 -delimiter "."

$hexstr

#OUTPUT HEX STRING TO FILE
$hexstr | out-file ".\output.txt"


