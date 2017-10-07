''By 6502 Workshop,LLC (mark@6502workshop.com)

''=====================SUBROUTINE DOCUMENTATION====================================
''
''COMPILING THIS FILE INSTRUCTIONS
''(run gospeech1.bat, comment out the two calls if you just want to compile a new version but not run it)
''
''-MAIN DOCUMENTATION
''Collate a series of binary files created by SBASM, and packed with ZX7.EXE,
''which contain the talk data for an entire building, 
''with each file containing the packed speech data for one NPC in the building. 
''
''This subroutine outputs a single binary file containing all of the individual
''binary input files with unpacked two header bytes inserted inbetween each packed speech chunk.
''The binary input files expect that the input filenames have a sequence number and that
''the sequence number is the last characters in the filename before the extension. This is how this subroutine knows which order to read the input files.  
''For example: data.tlk.m1.00.bin, data.tlk.m1.08.bin. In this example the sequence numbers are 00 and 08. 
''
''Additionally, this subroutine can be configued to strip of any unneeded header or trailer bytes that the compression utility
''adds to the packed data files (see CONFIGURATION OPTIONS below). For example, when using LZ4.EXE for compression set config to
''remove 11 header bytes and 8 trailer bytes.
''
''-Header bytes
''Byte 0 = NPC_ID (mirrors the NPC Record # in the Nox Archaist assembly code)
''Byte 1 = the byte length of the packed speech chunk for that NPC
''
''The header bytes are used by NPC.TALK to identify which packed speech chunk is
''associated with the NPC the player tried to talk to. Once the packed speech chunk is 
''identified, the byte length is also used in conjunction with unpacking the data so
''that is can be searched by the NTALK routines. 
''
''-Variables Notes
''	ID~%%		Refers to the NPC Record # in the 6502 assembly code for Nox Archaist. Each NPC_ID is offset by $08 bytes.
''
''-QB64 Language Notes
''*A byte type variable (~%% suffix) is written to binary files as hex. All other QB64 commands treat it
''		as a decimal value. i.e PRINT, and math operations. HEX$(byte_variable~%%) returns the hex string value or the byte type variable's contents. 
''
''-Compression Utility Support
''This program was designed for use with LZ4.EXE and ZX7.EXE but should work with any utility that produces a binary packed data output file as long as the filename convention for the
''packed data file follows the following convenion:
''		(this program assumes that the compression utility will create a packed file using the same filename as the unpacked input file plus an extension) 
''
''
''=================================================================================




''===================MAIN PROGRAM==============

''PARSE COMMAND LINE ARGUMENTS

	''Argument1 = first binary file in sequence to collate (this routine use this filename to find the request of the binary files for that building)
	arg.filename$ = COMMAND$				''in testing, used "data.tlk.L1.00.bin"
		
	
''CONFIGURATION OPTIONS

	''ID
	''(this value will be added, along with the packed data byte length, before each chunk of packed data)
	ID~%% = 00				''This value is used as the first header byte
	ID.OFFSET~%% = 08		''The number of bytes between ID values


	''Filename Of Compression Utility
	arg.compressor.program$ = "ZX7.EXE"

	''Extension of Packed Data Files
	''(this program assumes that the compression utility will create a packed file using the same filename as the unpacked input file plus an extension. This variable holds the extension that is added)
	arg.compressor.ext$ = ".ZX7" ''the extension the compressor appends to the filename when creating the packed output file.
	
	
	''File Header Bytes to Remove
	file.header.bytes% = 0			''number of bytes at the start of the input files to discard

	''File Trailer Bytes to Remove
	file.trailer.bytes% = 0			''number of bytes at the end of the input files to discard
	
	''Input File-Sequence Path
	input.filename_path$ = "C:\MY_CODE\COMPRESSION\NPC.SPEECH.TEXT\DATA\"
	
	''Output File Path
	output.filename_path$ = "C:\MY_CODE\INCLUDES_LIBS\COMPRESSED_DATA\NPC.SPEECH.TEXT\"
	
	''Input File Name NPC Number Position (from left)
	non.sequence_number.chars% = 14	''the number of character in the filename that come before the NPC number. For example, if the filename is DATA.TLK.L001.00.BIN and the sequence number is 00, then this variables should be set to 12. 

	''Input File Name Location Number Position (from right)
	location_right.chars% = 12	''the number of character in the input filename, starting from the right, which contain the location number and the extension and the "." before the location number. The location number is the unique identifier to distinguish each output file if this program is called multiple times (it is called once per castle/town/village or any location that has NPCs) For example, if the filename is DATA.TLK.L001.00.BIN, then this variables should be set to 10. 
	
	''Input File Name # of Chars in Location Code 
	location_code.chars% = 5	''the number of character in the location code, including it's label and preceding "."; For example, if the location code is L001 then this variable should be set to 5.
		
	''Output File Name Prefix (.Lxx.BIN is appeneded, where Lxx is the location number, obtained from the input file name passed as an argument)
	output.filename_base$ = "DATA.TLK"
	
	

	
	
''INIT PROGRAM
'@START


''DEFINE VARIABLES
	DIM byte_array(3072) AS _UNSIGNED _BYTE ''first element is 0
	DIM check_array(3072) AS _UNSIGNED _BYTE ''first element is 0


''INIT VARIABLES
	ID.STR$ = hex$(ID~%%)		''converts the ID to a hex string so that it can be collated with other strings
		if ID~%% < 16 THEN ID.STR$ = "0"+ID.STR$	''adds a leading padded zero if necessary


	input.filename_base$ = left$ (arg.filename$,non.sequence_number.chars%)	''strip off the NPC ID and extension from the filename passed as a command line argument, which is the first file in the sequence to collate
	input.filename_ext$ = right$ (arg.filename$,4)

	input.filename_full$ = arg.filename$+arg.compressor.ext$ ''This is the first filename to read. The first filename to read is always the filename passed via command line argument, plus the extension the compressor will add. 
 
	input.filename$ = input.filename_path$+input.filename_full$	''this is the filename that the read routine will open. 
	
	output.filename_workspace1$ = right$ (arg.filename$,location_right.chars%) ''Strip off the based file name, leaving only the location number and extension. 
	output.filename_ext$ = right$ (output.filename_workspace1$,4) ''Strip off the based file name, leaving only the location number and extension. 
	output.filename_append$ = left$ (output.filename_workspace1$,location_code.chars%) ''Strip off the based file name, leaving only the location number and extension. 
	output.filename$ = output.filename_path$+output.filename_base$+output.filename_append$+output.filename_ext$ ''this is the full output file name.

	'' print "new cow2"
	'' print output.filename_path$
	'' print " "
	'' print output.filename_base$
	'' print " "
	'' print output.filename_workspace1$
	'' print " "
	'' print output.filename_append$
	'' print " "
	'' print output.filename_ext$
	'' print " "	
	'' print output.filename$
	'' input a$
	'' end
	
	compressor.filename$ = arg.compressor.program$ + " " + filename.path$ + arg.filename$ ''this is the filename than the compressor will open

	file.write.position% = 1		''the write routine will start writing to the first byte of the output file


'@END

''OUTER LOOP
DO 	
	iteration.counter% = iteration.counter% +1 ''tracks which iteration the outerloop is on; 1st, 2nd, 3rd etc.
	
	''HAS LAST FILE BEEN READ?
	next_up.filename$ = input.filename_path$+input.filename_base$+ID.STR$+input.filename_ext$  ''input file for this iteration 

	open next_up.filename$ FOR BINARY AS #1
		file.length% = lof (1)		''gets the length, in bytes, of the open file number specified
	close #1
	if file.length% > 0 then goto 100
		delete.filename$ = "del "+input.filename_path$+input.filename_base$+ID.STR$+input.filename_ext$
		shell (delete.filename$)
		
		if iteration.counter% > 1 THEN EXIT DO
		''REPORT ERROR: invalid input filename
		print "ERROR: input filename provided as command line parameter was not found"
		print next_up.filename$
		print " "
		print "to find this error in source code search for: REPORT ERROR: invalid input filename" 
		print " "
		print "Bob's your uncle"
		input a$
		''SYSTEM	''causes the program to return to the command prompt, or calling program/batch file, without pausing and prompting the user to press a key
		END
100	
 
''PACK INPUT DATA
	print "compressing input file: ";compressor.filename$	

	SHELL (compressor.filename$) ''runs the compression program specified in the configuration section above, using the next binary file in the sequence as a command line argument. The SHELL command returns control to this program when the compression program completes. 
	


					
''READ INPUT (DATA.TLK PACKED BINARY FILE)	
'@START
	print "reading packed input file to buffer:";input.filename$

	''init header byte:0, 1st)
	''(byte 1-2, 2nd/3rd is below)
	record.header_bytes~%%(0) = ID~%%

	''init counters
	input.byte.counter% = 0
	array.position% = 0

	
	''read file

	open input.filename$ FOR BINARY AS #1

		file.length% = lof (1) 	''gets the length, in bytes, of the open file number specified
		file.length.adjusted% = file.length% - file.header.bytes% - file.trailer.bytes% 
		file.read.stop_value% = file.length% - file.trailer.bytes%+2			''deducted header and trailer bytes from file length so that those bytes at the end of the file aren't read in. +1 because the loop exit test is after the counter increment, and another +1 because QB64 use 1 as the first file position instead of 0. 
			
		file.read.position% = 1 + file.header.bytes%		''the first position in files is 1, the first element in arrays is 0. The read position is calculated so that the file header bytes aren't read. 
		do 
			input.byte.counter% = input.byte.counter%+1
			get #1, file.read.position%, byte_array(array.position%)	''Read one byte of data. 
			array.position%=array.position%+1
			file.read.position%=file.read.position%+1
		loop until file.read.position% = file.read.stop_value%		
	close #1

'' if iteration.counter% < 2 then goto 400
					'' print "file.read.position: !";file.read.position%
					'' print "file.read.stop_value: !";file.read.stop_value%
					'' print "file.length: !";file.length%
					'' print "array.position: !";array.position%

					'' print " "
					'' print "array !248: ";hex$(byte_array(248))
					'' print " "
					'' print "iteration.counter: !"; iteration.counter%
					'' end
'' 400

'@END


''COLLATE FILES
''(add headers and binary input file with the output file)
'@START

	print "adding header to buffer. ID:$";hex$(ID~%%)

	''init header Byte:1-2, 2nd/3rd
	''(see above for init of header byte 1st)	
	''(set the 2nd/3rd header byte to the number of bytes in the binary file read (file.length%),  which the assembly code (NPC.TALK) will access to determine the length of each speech chunk (packed data))
	
	''set LO Byte
	record.header_bytes~%%(1) = file.length.adjusted% MOD 256			''sets testbyte~%%(0) equal to the remainder of the division
	
	''set HO Byte	
	record.header_bytes~%%(2) = int(file.length.adjusted% / 256)		''sets testbyte~%%(0) equal to the result of the division, rounded down to nearest whole number. 

	
	''init write variables
	array.position% = 0
	output.byte.counter%= 3 ''this counter is checked against the stop value to end the write loop. Since two header bytes will be written before the loop starts, we want the output counter to start at the 3rd byte, the first byte that will be written by the loop
	file.write.stop_value% = file.length.adjusted%+2+1 ''add 2 to account for the 2 header bytes we'll write. The +1 is because the loop exit check is after the counter increment
			
	''write file
	print "writing buffer to output file"
	print ""

if iteration.counter% > 1 then goto 1000	
	open output.filename$ FOR BINARY AS #2
1000				
		''write header bytes
			''Byte0: ID
			put #2, file.write.position%, record.header_bytes~%%(0)		''write first header byte
					print "file.write.position: !";file.write.position%;" $";hex$(record.header_bytes~%%(0))
			file.write.position%=file.write.position%+1

			''Byte1: LO byte (file length)
				put #2, file.write.position%, record.header_bytes~%%(1)		''write second header byte
					print "file.write.position: !";file.write.position%;" $";hex$(record.header_bytes~%%(1))
				file.write.position%=file.write.position%+1

			''Byte3: HO byte (file length)
				put #2, file.write.position%, record.header_bytes~%%(2)		''write second header byte
					print "file.write.position: !";file.write.position%;" $";hex$(record.header_bytes~%%(2))
				file.write.position%=file.write.position%+1
								
		''write input file data  (the data from the last binary file read)
		do					
			put #2, file.write.position%, byte_array(array.position%) ''write 1 byte from the last binary file read into byte_array	
				print "file.write.position: !";file.write.position%;" array position: $";hex$(array.position%);" $";hex$(byte_array(array.position%))
				
''				if iteration.counter% < 2 then goto 300

					'' ''if a$ <> "b" then goto 300
						'' print input.filename$
						'' print output.filename$
						
						'' print byte_array(array.position%)
						'' print array.position%
						'' print "output.byte.counter: !";output.byte.counter%
						'' print "file.write.stop_value: !";file.read.stop_value%
						'' print "file.length: !";file.length%
						
						'' input a$
						'' end
'' 300				
			array.position% = array.position%+1
			file.write.position%=file.write.position%+1
			output.byte.counter%=output.byte.counter%+1
		loop until output.byte.counter% = file.write.stop_value% 	
	''close
		''file.write.position%=file.write.position%+1		''not sure why this is needed. haveing trouble with the last byte in the collated output file either being missing, or a garbage value, or an extra $00. Part of the issue seems to be that QB64 is at least sometimes writing a $00 to the end of the file on it's own, without a PUT command. 
		
''REM=====TROUBLESHOOTING HOOK		
		'' ''REM this is the iteration # to stop on
'' if iteration.counter% >= 1 then goto 500
	'' goto 510
'' 500
					'' print " "
					'' print "output.byte.counter: !";output.byte.counter%
					'' print "file.write.stop_value: !";file.write.stop_value%
					'' ''print "file.length: !";file.length%
					'' print " "
					'' print "file.write.position (at loop term): !";file.write.position%
					'' print "array.position (at loop term): !";array.position%

					'' print " "
					'' print "last array position write: !";array.position%;":";hex$(byte_array(array.position%-1))
					'' print " "
					'' print "iteration.counter: !"; iteration.counter%
					'' ''print "array !248: ";hex$(byte_array(248))
					'' close #2 ''close output file
					'' end
'' 510
			
'@END


''NEXT INPUT FILE
	print "looking for next input file in sequence"

	''CALCULATE NEXT ID
	ID~%% = ID~%% + ID.OFFSET~%%	
	ID.STR$ = hex$(ID~%%)		''converts the ID to a hex string so that it can be collated with other strings
		if ID~%% < 16 THEN ID.STR$ = "0"+ID.STR$	''adds a leading padded zero if necessary

	''UPDATE COMPRESSOR FILENAME PARAMETER
	compressor.filename$ = arg.compressor.program$ + " " + input.filename_base$+ID.STR$+input.filename_ext$
				
	''UPDATE INPUT FILENAME
		
	input.filename_full$ = input.filename_base$+ID.STR$+input.filename_ext$+arg.compressor.ext$	
	input.filename$ = input.filename_path$+input.filename_full$	''this is the filename that the read routine will open. 
	
	
''RETURN TO OUTER LOOP
LOOP	
	close #2 ''close output file
	print " "
	print "All files in sequence have been processed."
	print "END PROGRAM"

				
SYSTEM
END

'' ''CHECK-READ FILE  
'' ''the files contains two bytes, values = 10,20

	'' print "collation complete. Printing output file"

	'' open output.filename$ FOR BINARY AS #3
		'' file.read.position% = 1		''the first position in files is 1, the first element in arrays is 0.	
	
		'' do while not eof(3)
			'' get #3, file.read.position%, check_array(file.read.position%-1)	''Read one byte of data. the reason for the -1 is this: the first position in files is 1, the first element in arrays is 0.
			'' file.read.position%=file.read.position%+1
		'' loop
	'' close

	
	'' ''print file.length%
	'' print print.array2 (check_array(), file.write.position%)
	
	
	'' end


'' END

'' NPC_ID.STR$ = HEX$(NPC_ID)
	'' if NPC_ID < 16 THEN NPC_ID.STR$ = "0"+NPC_ID.STR$	''adds a leading padded zero if necessary

	
	

''===================FUNCTIONS==============

''Functions appear to be required to end with $
''Functions appear to need to be used withing the context of another command.
	''For example, A$ = MY.FUNCTION$ or PRINT MY.FUNCTION$. However, putting MY.FUNCTION$ alone on a line doesn't seem to work. 

FUNCTION PRINT.ARRAY(ARRAY() AS _UNSIGNED _BYTE) '{
''I can't figure out how to detect the end of the array or lookup the array size for anything but a byte array.
''I had a lot of problems getting subscript out of range errors even though the logic in this function seems correct.
''finally the errors went away but it prints a 0 at the end for no apparent reason.

x=0
array_end = len(array())


do
print hex$(array(x));".";
x=x+1
loop until x = array_end

END FUNCTION
'}


FUNCTION PRINT.ARRAY2 (ARRAY() AS _UNSIGNED _BYTE, LENGTH AS INTEGER) '{
''Print specified number of elements of a byte array
x=0

do
print hex$(array(x));".";
x=x+1
loop until x = length

END FUNCTION
'}