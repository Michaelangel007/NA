<#DEMO: convert hex string to byte array, do math
on the byte array, then convert byte array to hex string,
then output to file.

A byte array is the format powershell needs to 
do hex math. 

a hex string is data that the programmer knows 
contains hex values but powershell treats as a string.


#>


#CODE STRUCTURE NOTE
#Because powershell is an interpreted language,
#all functions must be declared before the are 
#invoked by the code. As a result, the functions are
#listed a the top of the source code file, and the
#main program is at the bottom. Within the functions,
#child functions are listed at the top of the function.


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


function Get.ArraySize ($array) 
#####RETURNS THE NUMBER OF CHARACTERS IN A TWO DIMENSIONAL ARRAY
{

$init_counter_char = 0
$init_counter_line = 0
$init_stop_value = ($array.length-1)/2	
$array_size_counter = 0

Do	{
	Do	{
		$init_counter_char++
		$array_size_counter++
		} Until ($init_counter_char -eq $array[$init_counter_line].length)
	$init_counter_char = 0
	$init_counter_line ++
	} Until ($init_counter_line -eq $init_stop_value)

	
return $array_size_counter
}


function Get.ArrayDim0 ($array) 
####RETURNS THE MAX NUMBER OF CHARACTERS IN THE 2ND DIMENSION OF A TWO DIMENSIONAL ARRAY
{

$array_dim0_size = ($array.length-1)/2	

return $array_dim0_size
}

function Get.ArrayDim1 ($array) 
####RETURNS THE MAX NUMBER OF CHARACTERS IN THE 2ND DIMENSION OF A TWO DIMENSIONAL ARRAY
{

$init_counter_char = 0
$init_counter_max = 0
$init_counter_line = 0
$init_stop_value = ($array.length-1)/2	

Do	{
	Do	{
		$init_counter_char++
		$array_size_counter++
		} Until ($init_counter_char -eq $array[$init_counter_line].length)
	if ($init_counter_char -gt $init_counter_max)
		{
		$init_counter_max = $init_counter_char
		}
	
	
	$init_counter_char = 0
	$init_counter_line ++
	} Until ($init_counter_line -eq $init_stop_value)

	
return $init_counter_max
}



function Join.MyData ($input_data, $line_counter)
#DESCRIPTION: CONCATENATES THE DATA CONTAINS IN THE SPECIFIED 2ND DIMENSION ($LINE_COUNTER) OF A 2-DIMENSIONAL ARRAY ($INPUT_DATA) AND RETURNS THE CONCATENTATED DATA VIA $STRING
#RETURN: $STRING
{

	$char_counter = 0
	
	Do	{
	
	$string = $string+$input_data[$line_counter][$char_counter]
	$char_counter ++
	
	
	} Until ($char_counter -eq $input_data[$line_counter].length)

return $string
}
	
function Get.MyData
{
$i = 0
$fileContents = Get-Content -path test_map_data.ASM 
#$fileContents = Get-Content -path hexinput.txt 
While ( $i -le $fileContents.length )
 {
  $fileContents[$i]
  $i++
}
return $filecontents
}


function Parse.MyData ($input_data)
{




	function Compress.MyData
	{
	#IS HEX TABLE EXEMPT FROM BEING COMPRESSED
	#(there is a filler table at the start of each data file which shouldn't be compressed because the filler data is used to make sure the start of the hex tables begins at the start of an Apple II disk sector)
	$exempt=0		#default = no
	if ($input_data[$line_counter][0] -eq "F")
		{
			if ($input_data[$line_counter][1] -eq "I")
				{
					if ($input_data[$line_counter][2] -eq "L")
						{
							if ($input_data[$line_counter][3] -eq "L")
								{
									if ($input_data[$line_counter][4] -eq "E")
										{
											if ($input_data[$line_counter][5] -eq "R")
												{
												$exempt=1	#set exempt to yes
												}
										}
								}
						}
				}
		}
		

	
	$char_counter2 = 0
	$write_counter = 0
	[byte]$same_tile_tally = 0x1	#used in the compression algorithm to record the number of adjacent tiles of the same type. A byte array holds hex values which is necesary to do math on them, but powershell outputs those values as decimal. Later a function is called to convert the byte array value to a string containing the hex value in standard hex notation. 
	$records_written = 0			#stores the total compression records written for the current hex table
	$overflow_flag = 0 
	
	
	
	
	####COMPRESSION ALGORITHM####
	if ($exempt -ne 1)				#only run if the current hex table found is not exempt from compression
		{		
		#COPY LINE HEADER TO OUTPUT STRING 
		#(portion of line prior to the start of the hex table data)
		
		Do	{
			$string = $string + $input_data[$line_counter][$char_counter2]		#$string is the variable used to assemble the contents of the current line, the header and the compressed hex data. 
			$char_counter2 ++
			} Until ($char_counter2 -eq $hex_table_index)
		
		#SET COMRPESSION LOOP STOP VALUE
		#If {$input_data[$line_counter][$hex_table_index]}

		
		Do {

			#NEXT TILE PAIR
			$tile0 = $input_data[$line_counter][$char_counter2+0] + $input_data[$line_counter][$char_counter2+1]
			$tile1 = $input_data[$line_counter][$char_counter2+3] + $input_data[$line_counter][$char_counter2+4]

			If ($tile0 -eq $tile1)     #is the current tile_type the same as the next tile?
				{
				 if ($same_tile_tally -lt 255)
					{
					$same_tile_tally ++  	#if yes, increment tally
					}
					ELSE {
					$overflow_flag = 1			#increment attempted above !256 ($FF), set overflow flag
					
					}
				}
				ELSE {					#if no, write compression record to output string 	(record format is tile_qty, tile_type)			
					
				If ($overflow_flag -eq 0)
					{
						$same_tile_tally_hex = Convert-ByteArrayToHexString $same_tile_tally -width 2 -delimiter "."   #converts $same_tile_tally from a byte array to a hex string. A byte array holds hex values which is necesary to do math on them, but powershell outputs those values as decimal. The function called converts the byte array value to a string containing the hex value in standard hex notation. 	
						$string = $string + $same_tile_tally_hex
					}	
					ELSE {
					$string = $string + "."
					$string = $string + "00"
					}
						
				$string = $string + "."
				$string = $string + $tile0  #add current tile_type to output string

				$write_counter ++

				$same_tile_tally = 1	#reset tally, prepare for next record
				$records_written ++
				}
			
			$char_counter2 ++
			$char_counter2 ++
			$char_counter2 ++
			} until ($char_counter2 -ge $input_data[$line_counter].length)
		}		
	#ADD ARRAY STOP VALUE TO OUTPUT LINE
	$string = $string + "."
	$string = $string + "00"
	
	
	if ($exempt -eq 1)		#Is current hex table exempt from compression
		{
		$string = Join.MyData $input_data $line_counter		#If yes, then assemble output for this line using the same function call that would have been used if a hex table were not found in this line. 
		}
	
		
		
<# 	if ($exempt -eq 0)
	{
	$string
	exit
	} #>
	return $string
	#END FUNCTION
	} 
	
#MAIN SCOPE


#INIT COUNTERS
$hex_table_counter = 0 #for troubleshooting
$line_counter = 0	
$char_counter = 0
$line_stop_value = ($input_data.length-1)/2	#I couldn't find a way to directly calculate the number of 1st dimensional elements in an array. But, $array.length appears to be coorelated. Every time I add a line to the input, regardless of # of characters, the value of array.length increases by 2, and with one in the array array.length = 3. So there is an extra value in there. Hence the formula of (array.length-1)/2, to arrive at the # of lines. Since the first element is 0, this the stop value we need for the look which increments the coutner before testing the stop value.. 
$hex_table_found = 0   #Used to override the default line output and instead used the data assembled by Compress.MyData

#$array_size = Get.ArraySize ($input_data)
#$array_dim1 = Get.ArrayDim1 ($input_data)

$array_dim0 = Get.ArrayDim0 ($input_data)
	
	
$data_assembly = New-Object 'object[,]' $array_dim0, 1

	
	#$data_assembly[1,0]=5
	#$data_assembly[1,0]
	#exit
	
<# 	if ($input_data[1][4] -eq " ")
	 {
	  "space"
	 }
	 ELSE
	 {
	 "no space"
	 }
	exit #>
	
<# 	if ($input_data[4].length -eq 0)
	 {
	 "equal"
	 }
	 ELSE
	 {
	 "NE"
	 }
	exit #>
	
Do {
	Do {
		if ($input_data[$line_counter][$char_counter] -match "\x20" -OR $input_data[$line_counter][$char_counter] -match "\x09")				#test for space (ascii $20) or tab character (ascii $09)
			{
			#"space or tab found"
				if ($input_data[$line_counter][$char_counter-1] -eq "S")
					{
					#"S found"
					
						if ($input_data[$line_counter][$char_counter-2] -eq "H")
							{
							#"H found"
							
								if ($input_data[$line_counter][$char_counter-3] -eq ".")
								{
								#". found, which means hex table is found"
									Do {
									$char_counter++
									} while ($input_data[$line_counter][$char_counter] -eq " ")
								$hex_table_index = $char_counter
								#"$line_counter, $hex_table_index"
								$compressed_line_output = Compress.MyData 
								$hex_table_counter ++		#used for troubleshooting
								$hex_table_found = 1
								}
								Else {
								#". not found, which means hex table is not found"
								
								}
							}
							Else {
							#"H not found"
							
							}
					}
					Else {
					#"S not found"
					
					}

			}
			Else {
			#"space or tab not found"
			
			}	
	
		$char_counter ++
		} until ($char_counter -ge $input_data[$line_counter].length)
	
	if ($input_data[$line_counter].length -eq 0)
		{
		#skip data join, line is empty
		
		}
		ELSE {
		
		if ($hex_table_found -eq 0)			#if a hex table is found we don't want to use the default output line because Compress.MyData assembled an output line for us
			{
			$data_assembly[$line_counter,0] = Join.MyData $input_data $line_counter
			}
			ELSE
			{
			$data_assembly[$line_counter,0] = $data_assembly[$line_counter,0] + $compressed_line_output	#$compressed_line_output was the return value from Compress.MyData
			}
		}
	$hex_table_found = 0
	#$data_assembly[$line_counter,0] = Join.MyData $input_data $line_counter
	#$data_assembly[$line_counter,0]
	#exit


	
	$char_counter = 0			#reset counter for next line
	$line_counter++

					#$line_counter
					#$name = Read-Host 'What is your username?'

					
	} Until ($line_counter -eq $line_stop_value)


	#$hex_table_counter
	#exit
return $data_assembly
#END OF FUNCTION
}

function Convert.MyData ($input_data)
{
#CONVERT HEX DATA ARRAY (RAW DATA) TO HEX STRING
#HEX DATA ARRAY IS MY TERM FOR A STRING CONTAINING HEX VALUES WHERE EACH CHARACTER OF THE STRING IS STORED IN A SEPEARAE ARRAY ELEMENT. 


#$input_data = "0","F",".","0","B"


#*****remove -2 from $input_data.length-2 when
#$input_data is no longer coming directly from a
#file read generated array


$counter = 0

Do {

if ($input_data[$counter] -eq ".")
		{
#			"equal"
			#$input_data[0]
			#break
		}
		Else {
			#$input_data[0]
			$hexstr = $hexstr + $input_data[$counter]
#			"not equal"
			#break
		}
#	$counter	
	$counter ++
	} Until ($counter -eq $input_data.length-2)

#$counter

#$hexstr



#$hexstr = $input_data[0]+$input_data[1]

#$hexstr = $hexstr + $input_data[2] + $input_data[3]

#CONVERT HEX STRING TO BYTE ARRAY
$byteary = Convert-HexStringToByteArray ($hexstr)
return $byteary
}


####MY CODE#####

#DEFINE ARRAYS

#GET INPUT

 
 
$filecontents = Get.MyData
	

	#$filecontents
$output_data = Parse.MyData ($filecontents)
#Parse.MyData ($filecontents)
#	exit
	
	#[System.IO.File]::WriteAllLines("c:\my_code\powershell\", "data.map.surface.ASM")
	#$output_data | out-file -encoding UTF8 ".\data.map.surface.ASM"
	$output_data | out-file -encoding UTF8 ".\output.asm"
	
	exit


#CONVERT INPUT

$byteary = Convert.MyData($filecontents)
	
	
#$byteary = Convert.MyData($filecontents)

#DO THE MATH (ON THE BYTE ARRAY)
$byteary[0] = $byteary[0] + 1
$byteary[1] = $byteary[1] + 1

#CONVERT BYTE ARRAY TO HEX STRING

$hexstr = Convert-ByteArrayToHexString $byteary -width 2 -delimiter "."

$hexstr

#OUTPUT HEX STRING TO FILE
$hexstr | out-file ".\output.txt"


