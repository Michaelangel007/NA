

	print "cow"
	end
	



''By 6502 Workshop,LLC (mark@6502workshop.com)


	DIM byte_array(3072) AS _UNSIGNED _BYTE ''first element is 0
	DIM check_array(3072) AS _UNSIGNED _BYTE ''first element is 0

	
	''init write variables
	array.position% = 0
	output.byte.counter%= 3 ''this counter is checked against the stop value to end the write loop. Since two header bytes will be written before the loop starts, we want the output counter to start at the 3rd byte, the first byte that will be written by the loop
	file.write.stop_value% = 12
		''file.length.adjusted%+2+1 ''add 2 to account for the 2 header bytes we'll write. The +1 is because the loop exit check is after the counter increment
			
	''write file
				print "write.position: $";hex$(file.write.position%-1);" $";hex$(record.header_bytes~%%(0))
			file.write.position%=file.write.position%+1

					print "write.position: $";hex$(file.write.position%-1);" $";hex$(record.header_bytes~%%(1))
				file.write.position%=file.write.position%+1

					print "write.position: $";hex$(file.write.position%-1);" $";hex$(record.header_bytes~%%(2))
				file.write.position%=file.write.position%+1
								
		''write input file data  (the data from the last binary file read)
		do					
				print "write.position: $";hex$(file.write.position%-1);" array position: !";array.position%;" $";hex$(byte_array(array.position%))
				
''				if iteration.counter% < 2 then goto 300

					'' ''if a$ <> "b" then goto 300
						'' print byte_array(array.position%)
						'' print array.position%
						'' print "output.byte.counter: !";output.byte.counter%
						'' print "file.write.stop_value: !";file.read.stop_value%
						'' input a$
						'' end
'' 300				
			array.position% = array.position%+1
			file.write.position%=file.write.position%+1
			output.byte.counter%=output.byte.counter%+1
		loop until output.byte.counter% = file.write.stop_value% 	
	close

					print "output.byte.counter: !";output.byte.counter%
					print "file.write.stop_value: !";file.read.stop_value%

				end
				

'' ''=====================SUBROUTINE DOCUMENTATION====================================
'' ''
'' ''Collate a series of binary files created by SBASM, and packed with LZ4.EXE,
'' ''which contain the talk data for an entire building, 
'' ''with each file containing the packed speech data for one NPC in the building. 
'' ''
'' ''This subroutine outputs a single binary file containing all of the individual
'' ''binary input files with unpacked two header bytes inserted inbetween each packed speech chunk.
'' ''The binary input files expect that the input filenames have a sequence number and that
'' ''the sequence number is the last characters in the filename before the extension. This is how this subroutine knows which order to read the input files.  
'' ''For example: data.tlk.m1.00.bin, data.tlk.m1.08.bin. In this example the sequence numbers are 00 and 08. 
'' ''
'' ''Additionally, this subroutine can be configued to strip of any unneeded header or trailer bytes that the compression utility
'' ''adds to the packed data files (see CONFIGURATION OPTIONS below). By default the config is
'' ''set to remove 11 header bytes and 8 trailer bytes (added by the utility LZ4.EXE)
'' ''
'' ''-Header bytes
'' ''Byte 0 = NPC_ID (mirrors the NPC Record # in the Nox Archaist assembly code)
'' ''Byte 1 = the byte length of the packed speech chunk for that NPC
'' ''
'' ''The header bytes are used by NPC.TALK to identify which packed speech chunk is
'' ''associated with the NPC the player tried to talk to. Once the packed speech chunk is 
'' ''identified, the byte length is also used in conjunction with unpacking the data so
'' ''that is can be searched by the NTALK routines. 
'' ''
'' ''-Variables Notes
'' ''	ID~%%		Refers to the NPC Record # in the 6502 assembly code for Nox Archaist. Each NPC_ID is offset by $08 bytes.
'' ''
'' ''-QB64 Language Notes
'' ''*A byte type variable (~%% suffix) is written to binary files as hex. All other QB64 commands treat it
'' ''		as a decimal value. i.e PRINT, and math operations. HEX$(byte_variable~%%) returns the hex string value or the byte type variable's contents. 
'' ''
'' ''-Compression Utility Support
'' ''This program was designed for use with LZ4.EXE but should work with any utility that produces a binary packed data output file as long as the filename convention for the
'' ''packed data file follows the following convenion:
'' ''		(this program assumes that the compression utility will create a packed file using the same filename as the unpacked input file plus an extension) 
'' ''
'' ''
'' ''=================================================================================


		

'' ''===================MAIN PROGRAM==============

'' ''PARSE COMMAND LINE ARGUMENTS

	'' ''Argument1 = first binary file in sequence to collate (this routine use this filename to find the request of the binary files for that building)
	'' arg.filename$ = COMMAND$
		
	
'' ''CONFIGURATION OPTIONS

	'' ''ID
	'' ''(this value will be added, along with the packed data byte length, before each chunk of packed data)
	'' ID~%% = 00				''This value is used as the first header byte
	'' ID.OFFSET~%% = 08		''The number of bytes between ID values


	'' ''Filename Of Compression Utility
	'' arg.compressor.program$ = "LZ4.EXE"

	'' ''Extension of Packed Data Files
	'' ''(this program assumes that the compression utility will create a packed file using the same filename as the unpacked input file plus an extension. This variable holds the extension that is added)
	'' arg.compressor.ext$ = ".LZ4" ''the extension the compressor appends to the filename when creating the packed output file.
	
	
	'' ''File Header Bytes to Remove
	'' file.header.bytes% = 11			''number of bytes at the start of the input files to discard

	'' ''File Trailer Bytes to Remove
	'' file.trailer.bytes% = 8			''number of bytes at the end of the input files to discard
	
	'' ''Input File Sequence Path
	'' input.filename_path$ = "C:\MY_CODE\TESTING\QB64_TESTING\"

	'' ''Input Sequence Number Position
	'' non.sequence_number.chars% = 12	''the number of character in the filename that come before the sequence number. For example, if the filename is data.tlk.m1.00.bin and the sequence number is 00, then this variables should be set to 12. 

	
'' ''INIT PROGRAM
'' '@START


'' ''DEFINE VARIABLES
	'' DIM byte_array(3072) AS _UNSIGNED _BYTE ''first element is 0
	'' DIM check_array(3072) AS _UNSIGNED _BYTE ''first element is 0


'' ''INIT VARIABLES
	'' ID.STR$ = hex$(ID~%%)		''converts the ID to a hex string so that it can be collated with other strings
		'' if ID~%% < 16 THEN ID.STR$ = "0"+ID.STR$	''adds a leading padded zero if necessary

	'' output.filename$ = "C:\MY_CODE\TESTING\QB64_TESTING\OUTPUT.BIN"

	'' input.filename_base$ = left$ (arg.filename$,non.sequence_number.chars%)	''strip off the NPC ID and extension from the filename passed as a command line argument, which is the first file in the sequence to collate
	'' input.filename_ext$ = ".BIN"

	'' input.filename_full$ = arg.filename$+arg.compressor.ext$ ''This is the first filename to read. The first filename to read is always the filename passed via command line argument, plus the extension the compressor will add. 
 
	'' input.filename$ = input.filename_path$+input.filename_full$	''this is the filename that the read routine will open. 
	'' compressor.filename$ = arg.compressor.program$ + " " + filename.path$ + arg.filename$ ''this is the filename than the compressor will open

	'' file.write.position% = 1		''the write routine will start writing to the first byte of the output file


'' '@END

'' ''OUTER LOOP
'' DO 	
	'' iteration.counter% = iteration.counter% +1 ''tracks which iteration the outerloop is on; 1st, 2nd, 3rd etc.
	
	'' ''HAS LAST FILE BEEN READ?
	'' next_up.filename$ = input.filename_path$+input.filename_base$+ID.STR$+input.filename_ext$  ''input file for this iteration 

	'' open next_up.filename$ FOR BINARY AS #1
		'' file.length% = lof (1)		''gets the length, in bytes, of the open file number specified
	'' close
	'' if file.length% > 0 then goto 100
		'' delete.filename$ = "del "+input.filename_path$+input.filename_base$+ID.STR$+input.filename_ext$
		'' shell (delete.filename$)
		'' EXIT DO

'' 100	
'' ''PACK INPUT DATA
	'' print "compressing input file: ";compressor.filename$	

	'' SHELL (compressor.filename$) ''runs the compression program specified in the configuration section above, using the next binary file in the sequence as a command line argument. The SHELL command returns control to this program when the compression program completes. 
	

'' ''READ INPUT (DATA.TLK PACKED BINARY FILE)	
'' '@START
	'' print "reading packed input file to buffer:";input.filename$

	'' ''init header byte #1 
	'' ''(byte #2 is below)
	'' record.header_bytes~%%(0) = ID~%%

	'' ''init counters
	'' input.byte.counter% = 0
	'' array.position% = 0

	
	'' ''read file

	'' open input.filename$ FOR BINARY AS #1

		'' file.length% = lof (1) 	''gets the length, in bytes, of the open file number specified
		'' file.length.adjusted% = file.length% - file.header.bytes% - file.trailer.bytes% +1 
		'' file.read.stop_value% = file.length% - file.trailer.bytes%+1			''deducted header and trailer bytes from file length so that those bytes at the end of the file aren't read in. +1 because the loop exit test is after the counter increment
			
		'' file.read.position% = 1 + file.header.bytes%		''the first position in files is 1, the first element in arrays is 0. The read position is calculated so that the file header bytes aren't read. 
		'' do 
			'' input.byte.counter% = input.byte.counter%+1
			'' get #1, file.read.position%, byte_array(array.position%)	''Read one byte of data. 
			'' array.position%=array.position%+1
			'' file.read.position%=file.read.position%+1
		'' loop until file.read.position% = file.read.stop_value%		
	'' close

'' '@END


'' ''COLLATE FILES
'' ''(add headers and binary input file with the output file)
'' '@START

	'' print "adding header to buffer. ID:$";hex$(ID~%%)

	'' ''init header byte #2
	'' ''(see above for init of header byte #1)
	'' record.header_bytes~%%(1) = file.length.adjusted%	''set the 2nd header byte to the number of bytes in the binary file read (file.length%),  which the assembly code (NPC.TALK) will access to determine the length of each speech chunk (packed data)  

	'' ''init write variables
	'' array.position% = 0
	'' output.byte.counter%= 3 ''this counter is checked against the stop value to end the write loop. Since two header bytes will be written before the loop starts, we want the output counter to start at the 3rd byte, the first byte that will be written by the loop
	'' file.write.stop_value% = file.length.adjusted%+2+1 ''add 2 to account for the 2 header bytes we'll write. The +1 is because the loop exit check is after the counter increment
			
	'' ''write file
	'' print "writing buffer to output file"
	'' print ""
	
	'' open output.filename$ FOR BINARY AS #2
				
		'' ''write header bytes
		'' put #2, file.write.position%, record.header_bytes~%%(0)		''write first header byte
		'' file.write.position%=file.write.position%+1
		'' put #2, file.write.position%, record.header_bytes~%%(1)		''write second header byte
		'' file.write.position%=file.write.position%+1
		
		'' ''write input file data  (the data from the last binary file read)
		'' do					
			'' put #2, file.write.position%, byte_array(array.position%) ''write 1 byte from the last binary file read into byte_array	

			'' array.position% = array.position%+1
			'' file.write.position%=file.write.position%+1
			'' output.byte.counter%=output.byte.counter%+1
		'' loop until output.byte.counter% = file.write.stop_value% 	
	'' close
			
'' '@END


'' ''NEXT INPUT FILE
	'' print "looking for next input file in sequence"

	'' ''CALCULATE NEXT ID
	'' ID~%% = ID~%% + ID.OFFSET~%%	
	'' ID.STR$ = hex$(ID~%%)		''converts the ID to a hex string so that it can be collated with other strings
		'' if ID~%% < 16 THEN ID.STR$ = "0"+ID.STR$	''adds a leading padded zero if necessary

	'' ''UPDATE COMPRESSOR FILENAME PARAMETER
	'' compressor.filename$ = arg.compressor.program$ + " " + input.filename_base$+ID.STR$+input.filename_ext$
				
	'' ''UPDATE INPUT FILENAME
		
	'' input.filename_full$ = input.filename_base$+ID.STR$+input.filename_ext$+arg.compressor.ext$	
	'' input.filename$ = input.filename_path$+input.filename_full$	''this is the filename that the read routine will open. 
	
	
'' ''RETURN TO OUTER LOOP
'' LOOP	


'' END

'' '' ''CHECK-READ FILE  
'' '' ''the files contains two bytes, values = 10,20

	'' '' print "collation complete. Printing output file"

	'' '' open output.filename$ FOR BINARY AS #3
		'' '' file.read.position% = 1		''the first position in files is 1, the first element in arrays is 0.	
	
		'' '' do while not eof(3)
			'' '' get #3, file.read.position%, check_array(file.read.position%-1)	''Read one byte of data. the reason for the -1 is this: the first position in files is 1, the first element in arrays is 0.
			'' '' file.read.position%=file.read.position%+1
		'' '' loop
	'' '' close

	
	'' '' ''print file.length%
	'' '' print print.array2 (check_array(), file.write.position%)
	
	
	'' '' end


'' '' END

'' '' NPC_ID.STR$ = HEX$(NPC_ID)
	'' '' if NPC_ID < 16 THEN NPC_ID.STR$ = "0"+NPC_ID.STR$	''adds a leading padded zero if necessary

	
	

'' ''===================FUNCTIONS==============

'' ''Functions appear to be required to end with $
'' ''Functions appear to need to be used withing the context of another command.
	'' ''For example, A$ = MY.FUNCTION$ or PRINT MY.FUNCTION$. However, putting MY.FUNCTION$ alone on a line doesn't seem to work. 

'' FUNCTION PRINT.ARRAY(ARRAY() AS _UNSIGNED _BYTE) '{
'' ''I can't figure out how to detect the end of the array or lookup the array size for anything but a byte array.
'' ''I had a lot of problems getting subscript out of range errors even though the logic in this function seems correct.
'' ''finally the errors went away but it prints a 0 at the end for no apparent reason.

'' x=0
'' array_end = len(array())


'' do
'' print hex$(array(x));".";
'' x=x+1
'' loop until x = array_end

'' END FUNCTION
'' '}


'' FUNCTION PRINT.ARRAY2 (ARRAY() AS _UNSIGNED _BYTE, LENGTH AS INTEGER) '{
'' ''Print specified number of elements of a byte array
'' x=0

'' do
'' print hex$(array(x));".";
'' x=x+1
'' loop until x = length

'' END FUNCTION
'' '}