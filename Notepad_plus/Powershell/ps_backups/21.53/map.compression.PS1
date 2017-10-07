

#STORE COMMAND LINE ARGUMENTS
$source_filename=$args[0]		#The filename of the file containing uncompressed map data. We will read data from this file.
$output_targetsize_sectors=$args[1]	#The number of DOS 3.3 sectors we want the target file to be, including the header sector written by AppleCommander. 


$source_path = "C:\My_Code\INCLUDES_LIBS\"+ $source_filename 

$output_path = "C:\My_Code\INCLUDES_LIBS\compressed_data\compressed."+$source_filename


##########ZONE COMPRESSION FLAGS#######
#	;WZONE Number     						0	1  2  3	 4	5  6  7	 8	9 10 11	12 13 14 15	16 17 18 19	20 21 22 23	24 25 26 27	28 29 30 31	32 33 34 35	36 37 38 39	40 41 42 43	44 45 46 47	48 49 50 51	52 53 54 55	56 57 58 59	60 61 62 63
$SURFACE_COMPRESSION_FLAGS 				  = 01,01,01,01,01,01,01,01,01,01,01,01,01,01,01,01,01,00,01,01,01,01,01,01,01,00,00,01,01,01,01,01,01,01,01,01,01,01,01,01,01,01,01,01,01,01,01,01,01,01,01,01,01,01,01,01,01,01,01,01,01,01,01,01
#########################################


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
	
function Get.MyData ($source)
{
$i = 0
$fileContents = Get-Content -path $source
#$fileContents = Get-Content -path hexinput.txt 
While ( $i -le $fileContents.length )
 {
  $fileContents[$i]
  $i++
}
return $filecontents
}


function Parse.MyData ($input_data, $zone_compression_table)
{


	function Find.ExemptTable
	#SEARCHES CURRENT LINE FOR A HEX TABLE THAT IS EXEMPT FROM BEING COMPRESSED
	#(a hex table is exempt if it's label starts with FILLER)
	#(there is a filler table at the start of each data file which shouldn't be compressed because the filler data is used to make sure the start of the hex tables begins at the start of an Apple II disk sector)
	{
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
	
	return $exempt
	}

	function Compress.MyData
	{

	$char_counter2 = 0
	#$write_counter = 0
	[byte]$same_tile_tally = 0x1	#used in the compression algorithm to record the number of adjacent tiles of the same type. A byte array holds hex values which is necesary to do math on them, but powershell outputs those values as decimal. Later a function is called to convert the byte array value to a string containing the hex value in standard hex notation. 
	$records_written = 0			#stores the total compression records written for the current hex table
	$overflow_flag = 0 
	$byte_tally_line = 0			#stores the number of bytes output for each iteration of this function

	####COMPRESSION ALGORITHM####	
	
	#COPY LINE HEADER TO OUTPUT STRING 
	#(portion of line prior to the start of the hex table data)
	if ($zone_compression_table[$zone_index] -eq 1)
		{
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

			If ($tile0 -eq $tile1)     			#is the current tile_type the same as the next tile?
				{
				 if ($same_tile_tally -lt 255)
					{
					$same_tile_tally ++  		#if yes, increment tally
					}
				ELSE{
					$overflow_flag = 1			#increment attempted above !256 ($FF), set overflow flag
				
					}
				}
				ELSE{							#if no, write compression record to output string 	(record format is tile_qty, tile_type)			
					
					If ($overflow_flag -eq 0)
						{
						$same_tile_tally_hex = Convert-ByteArrayToHexString $same_tile_tally -width 2 -delimiter "."   #converts $same_tile_tally from a byte array to a hex string. A byte array holds hex values which is necesary to do math on them, but powershell outputs those values as decimal. The function called converts the byte array value to a string containing the hex value in standard hex notation. 	
						$string = $string + $same_tile_tally_hex
						$byte_tally_line++		#Tracks total bytes in output line
						}	
						ELSE{
							$string = $string + "."
							$string = $string + "00"
							$byte_tally_line++		#Tracks total bytes in output line
							}
							
					$string = $string + "."
					$string = $string + $tile0  #add current tile_type to output string
					
					$byte_tally_line++			#Tracks total bytes in output line
					#$write_counter ++

					$same_tile_tally = 1	#reset tally, prepare for next record
					$records_written ++
					}
			
			$char_counter2 ++
			$char_counter2 ++
			$char_counter2 ++
			} until ($char_counter2 -ge $input_data[$line_counter].length)
		}
		ELSE{	#If zone compression is not enabled for the current zone
			$string = Join.MyData $input_data $line_counter
			$byte_tally_line = 256
			}
	

	
	#ADD ARRAY STOP VALUE TO OUTPUT LINE
	if ($zone_compression_table[$zone_index] -eq 1)	#Is compression enabled for the current zone? No stop value is added to the end of the array since leaving the data uncompressed means that its exact size (1 page) is known to the uncompress routine. 

		{
		$string = $string + "."	#If no, add stop value. 
		$string = $string + "00"
		$byte_tally_line++
		}
	
####RETURN VALUE####
	$string				#return value #1: output line (compressed)
	$byte_tally_line	#return value #2: the number of bytes requried to store the hex values contained in the output line, using 6502 assembly langauge. 
			
	#return $string
	#END FUNCTION
	} 
	
######MAIN SCOPE#######



#INIT COUNTERS
$hex_table_counter = 0	#for troubleshooting
$line_counter = 0	  	#index for 1st dimension of $input_Data
$char_counter = 0		#index for 2nd dimension of $input_Data	
$line_stop_value = ($input_data.length-1)/2	#I couldn't find a way to directly calculate the number of 1st dimensional elements in an array. But, $array.length appears to be coorelated. Every time I add a line to the input, regardless of # of characters, the value of array.length increases by 2, and with one in the array array.length = 3. So there is an extra value in there. Hence the formula of (array.length-1)/2, to arrive at the # of lines. Since the first element is 0, this the stop value we need for the look which increments the counter before testing the stop value.. 
$zone_index = 0					#stores the index to the $zone_compression_table, used to determine if compression is enabled or disabled for a given zone. 
$compressed_hex_table_found = 0   #Used to override the default line output and instead used the data assembled by Compress.MyData
$byte_tally_file			= 0   #Used to tally up all the $byte_tally_line return values from Compress.MyData,, each which equal the bytes in one line of output and the sum of which equals the total # of bytes which will be written to the .BIN file the cross-assembler creates from the hex tables in $source_filename

#$array_size = Get.ArraySize ($input_data)
#$array_dim1 = Get.ArrayDim1 ($input_data)

$array_dim0 = Get.ArrayDim0 ($input_data)
	
	
$data_assembly = New-Object 'object[,]' $array_dim0, 1

	
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
						
						$exempt = Find.ExemptTable	#searches start of current line for the word "FILLER", which denotes an hex table which should not be compressed
						
						if ($exempt -eq 0)
							{
							#CALCUALTE HEX TABLE INDEX
							Do	{
								$char_counter++
								} while ($input_data[$line_counter][$char_counter] -eq " ")
							$hex_table_index = $char_counter
							#"$line_counter, $hex_table_index"

							$compressed_line_output = Compress.MyData 
							#Compress.MyData
							$byte_tally_file = $byte_tally_file + $compressed_line_output[1]
							
							
							$compressed_hex_table_found = 1
							$zone_index ++	
							}
						$hex_table_counter ++		#used for troubleshooting
						###open question: keep $hex_table_found here?			
						}
					Else{
					#". not found, which means hex table is not found"
					
						}
					}
				Else{
					#"H not found"
				
					}
				}
			Else{
				#"S not found"
			
				}

			}
		Else{
		#"space or tab not found"
		
			}	
	
		$char_counter ++
		} until ($char_counter -ge $input_data[$line_counter].length)
	
	if ($input_data[$line_counter].length -eq 0)
		{
		#skip data join, line is empty
		
		}
	ELSE{
	
		if ($compressed_hex_table_found -eq 0)	#if a hex table is found we don't want to use the default output line because Compress.MyData assembled an output line for us
			{
			$data_assembly[$line_counter,0] = Join.MyData $input_data $line_counter
			}
		ELSE{
			$data_assembly[$line_counter,0] = $data_assembly[$line_counter,0] + $compressed_line_output[0]	#$compressed_line_output was the return value from Compress.MyData
			}
		}
	$compressed_hex_table_found = 0

	
	$char_counter = 0			#reset counter for next line
	$line_counter++


					
	} Until ($line_counter -eq $line_stop_value)

####ADJUST TO TARGET OUTPUT FILE SIZE POSTPEND FILLER####



$output_targetsize_sectors--	#remove AppleCommander header from calculation

$output_targetsize_sectors_dec = [System.Convert]::ToInt32($output_targetsize_sectors,16)   #stores the integer value of $hex in $int1.

$total_bytes = $output_targetsize_sectors_dec * 256

$postpend_filler_bytes = $total_bytes - $byte_tally_file

if ($postpend_filler_bytes -ge 256)
	{
	#add postpend filler
	#.BS

		
		$filler_sectors = $postpend_filler_bytes / 256	#Number of sectors of filler to postpend to end of output to hit target number of sectors
		$filler_sectors = [math]::floor($filler_sectors)
		
		$data_assembly = $data_assembly + "	.BS '$$filler_sectors"

		
		#$total_bytes
		#$byte_tally_file
		#$postpend_filler_bytes
		#$filler_sectors
		
		$data_assembly

		exit	
	}
ELSE{
	#No action required, the output file will be the desired number of DOS 3.3 sectors without any postpend filler. 
	"cow"
	}

		exit



			#$byte_tally_file
			#exit
	
return $data_assembly
#END OF FUNCTION
}

####MAIN PROGRAM#####


#GET INPUT FROM DATA (UNCOMPRSSED)
$filecontents = Get.MyData $source_path

#COMPRESS DATA
#$output_data = Parse.MyData $filecontents $SURFACE_COMPRESSION_FLAGS
		Parse.MyData $filecontents $SURFACE_COMPRESSION_FLAGS
		exit


#####OUTPUT DATA TO FILE (COMPRSSED)#####
$output_data | out-file -encoding UTF8 $output_path


#REMOVE BOM ENCODING
#(sbasm requires its source files to be pure UTF8 encoding and the in 
#powershell the out-file cmdlet with -encoding UTF8 parm actually 
#results in UTF8-BOM

$MyData = Get-Content $output_path
$Utf8NoBomEncoding = New-Object System.Text.UTF8Encoding($False)
[System.IO.File]::WriteAllLines($output_path, $MyData, $Utf8NoBomEncoding)

	

	
	
###DELETE CONVERT.MY.DATA??? I don't think it's used


