				.CR     6502            Use 6502 overlay
				.OR		$B000			**Always put before .TF directive and never use again in program
				.TF     COMP_ZX7.PROG.BIN,BIN
				.EF		errors
				.LF		C:\MY_CODE\LIST
;				.LI 	OFF				Switches the assembler machine code conversion listing off so errors are easier to see				
;				.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_common

;=====================EXAMPLE DOCUMENTATION====================================
;
;-EXPECTED RESULT
;
;Break at $B029 ($B02B reported by monitor), with X-REG = $EE, Y-REG = $AA
;and from monitor A000L should show the value $10 in $A000-$A009 
;
;-OVERVIEW
;This program demos how to unpack data that was packed using ZX7.EXE on a PC. 
;
;Unlike LZ4.EXE, ZX7.EXE (PC PACKER) doesn't add any header or footer bytes to the packed data which need to be removed
;
;The packed data is loaded from disk (.LOAD.PACKED.DATA) from disk using Peter's ProDOS IO driver (he calls it opendir).
;
;The unpack subroutine (written by Peter Ferrie), is at $BF00, placed there as a binary
;include of ZX7.BF00.BIN, which was build using ACME cross-assembler from ZX7.6502.S
;
;unpack's parameteres are simple. Just specify the address of the packed data and the destination address for the unpacked data.
;unlike LZ4, no end address parameter is needed because ZX7 can figure out the end of the packed data on its own. 
;
;===NOTES ON OTHER FILES===
;ZX7.6502.S is a modified version of the code on Peter's website (ZXDSTSRC.S in the ZX7 zip archive). He modified it to be 6502 compatible instead of 65c02. It still requires Acme0.91. 
;The only change I had to make was to set paksize = 0 (it was originally $1234, and Peter said to set it to 0 when it's called as a subroutine rather than 
;used as a self-extracting program that ships with packed data, which is what paksize refers to...the packed data that ships with the program)
;
;ZX7.6502.S is designed for the packed data to reside at a higher address in memory than the unpacked data destination address.
;Peter has a different version (ZXSRCDST.S) that supports the opposite, a packed data address lower in memory than the unpacked destination address. 
;However, to use it, I would need to ask him to make it compatible with 6502. 
;
;ZX7.EXE is a free download online.
;
;-Disk Image Files
;UNAPCKED.DATA			The unpacked binary file generated by a different SBASM program. This is the file which gets packed by LZ4.EXE on the PC. I added it to the disk image so I could open it with CiderPress and compare it to the unpacked output in memory ($7000) generated by this program. It isn't functionally needed.
;PACKED.DATA 			The packed binary file generated by ZX7.EXE on the PC. This is the file loaded by PRODOS.IO (/aka opendir) to $BA00
;ZX7.BIN				The ZX7 unpacker written by Peter Ferrie (subroutine unpack), which is loaded at $BF00 in this program
;COMP_ZX7.PROG			The main test program (this file)
;NOX.SYSTEM				ProDOS Bootloader
;NOXARCH.MAIN			The file NOX.SYSTEM hands off control to, which is limited in size, so it's job is to load COMP.PROG. 
;
;
;-Source Files (.ASM)
;COMP_ZX7.PROG				The main test program (this file)
;COMP_ZX7.TEST.BOOT.ASM		The boot file which loads COMP_ZX7.PROG. AppleCommander writes it to the disk image as NOXARCH.MAIN. See NOXARCH.MAIN under disk image files for more details.
;=================================================================================

COMP.TEST.START


		
.LOAD.PACKED.DATA
;set command type (READ | WRITE)
;	lda #$81 ;([$1=read drive1, | $2=write drive1], [$81=read drive2 | $82=write drive2])
	lda #$1 ;([$1=read drive1, | $2=write drive1], [$81=read drive2 | $82=write drive2])
	sta reqcmd
	JSR DRIVER.READ

		; LDA $C082
		; LDA #$AB
		; BRK	
		
.TEST.UNPACK
	lda     #$03			;lo byte (start_of_packed_data)
	sta     src
	lda     #$BA			;ho byte (start_of_packed_data)
	sta     src+1

	; lda     #$0E			;lo byte (end_of_packed_data+1) ;i.e. first byte after end of packed data
	; sta     end
	; lda     #$BA			;ho byte (end_of_packed_data+1) ;i.e. first byte after end of packed data
	; sta     end+1

	lda     #$00			;lo byte (place_to_unpack_data)
	sta     dst
	lda     #$A0			;ho byte (place_to_unpack_data)
	sta     dst+1

	jsr unpack

	
	LDA $C082				;enable ROM, disable BSR	
	LDA STATUS
	LDX #$EE
	LDY #$AA
	BRK
;
;	
DRIVER.READ
;set destination memory address
	lda #$00
	sta ldrlo
	lda #$BA
	sta ldrhi

;set read length (bytes)
	lda #$FF				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta sizelo	
	lda #$FF				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta sizehi
	
;set filename to read from	
	lda #PACKED.DATA	;load LO address
	sta namlo
	lda /PACKED.DATA		;load HO address
	sta namhi

		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO

	
	RTS


;======DEFINE VARIBLES======

; file.to.read  .AZ #$0A,/DUMMY.FILE/	;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 
; file.to.write .AZ #$0A,/DUMMY.FILE/	;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 
;DATA.SPR.SURF	.AZ #$0D,/DATA.SPR.SURF/	;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 
;DATA.SHP.SURF2 	.AZ #$0D,/DATA.SHP.SURF/	;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 
PACKED.DATA	.AZ #$0B,/PACKED.DATA/	;ascii array, using LO values. Filename is limited to !15 characters. Hex value after .AZ is the char quantity. 


;PREP.BRK			.BS $1


;unpacker variables, no need to change these
src	=	$0
dst	=	$2
;end	=	$4

unpack	= $BF00	;ZX7 unpacker subroutine



;-----VARIABLES INCLUDED TO GET THE ERROR TO SHUT UP, CAUSED BY TRYING TO USE THE ROUTINES_GENERAL.ASM LIBRARY WITHOUT ALL THE OTHER LIBRARIES LOADED


;=================DEFINE MATH VARIABLES===============


OP1      	.BS $2				;2byt
OP2      	.BS $2				;2byt
RESULT  	.BS $4				;2byt

;DIV.16 VARIABLES
;==========KEEP THESE VARIBLES IN THIS ORDER=======
;(the memory space is shared with other routines)
DIVIDEND32	.BS $2			;UPPER 16-BIT PORTION OF DIVIDEND
DIVIDEND 	.BS $2			;NUMBER TO DIVIDE
DIVISOR 	.BS $2			;NUMBER TO DIVIDE BY
TEMP1	.BS $1
CARRY	.BS $1
;===================================================



;DIV.8.BCD VARIABLES
QUOTIENT.COUNTER 	.EQ TEMP1

;MLP.16 VARIABLES	
MULPLR   .EQ DIVIDEND32
PARTIAL  .EQ DIVIDEND
MULCND   .EQ DIVISOR 

MULPLR.TALLY	.EQ	DIVIDEND
MULCND.COUNTER	.EQ TEMP1		

;BCD.UNPACK
BCD      		.EQ DIVIDEND32



;RANDOM.8 VARIABLES
;*****DO NOT CHANGE THE ORDER OF THESE VARIABLES.
;see note below
RND		.BS $2		;2byt
TMP		.BS $4
MOD		.BS $1
SEED0	.BS $1
SEED1	.BS $1
SEED2	.BS $1
SEED3	.BS $1
RND.LO	.BS $1
RND.HI	.BS $1
RAND_TABLE    .HS	01.01.00.FE.FF.01
RANDOM_NUMBER.ITERATION_TALLY	.BS $1
RND.ABORT	.BS $1	

SAVED.RANDOM.NUMBER	.BS $1 ;variable for storing a random number for future use. 




animation.update .eq $B000
generate.debug.log .eq $B100
USE.PAGE			 	.BS $1

DRAW.START_BYTE 	.BS $1
DRAW.STOP_BYTE		.BS $1
DRAW.START_LINE		.BS $1
DRAW.STOP_LINE		.BS $1


HRCG.SIZE.MINUS_1		.EQ $2FF	;#CONSTANT. Size of character set data, in bytes, minus 1. The minus 1 is so that is can be used to dynamically calculate the memory address of the last byte of data. 
	
HRCG.AUX.START			.EQ $5900	;#CONSTANT; Starting memory address in aux memory where Hi Res character set is stored until needed to print a character on screen
HRCG.AUX.END			.EQ $5BFF	;#CONSTANT; Starting memory address in aux memory where Hi Res character set is stored until needed to print a character on screen

HRCG.MAIN.START			.EQ $0C00	;#CONSTANT; Starting memory address in main memory where Hi Res character set is swaped in, when a character needs to be printed to screen
HRCG.MAIN.END			.EQ $0EFF	;#CONSTANT; Starting memory address in main memory where Hi Res character set is swaped in, when a character needs to be printed to screen

HRCG.SHAPE.OFFSET		.EQ $E0		;Stores the offset used to calculate the exact starting address of the shape table for a specific character, using the ASCII value of the character
HRCG.SHAPE.SIZE			.EQ $08		;#CONSTANT; the number of bytes in each character's shape table 

HRCG.BUFFER				.BS $8		;Buffere where a single character copies from aux memory is stored. The HRCG controller at $300 looks in this buffer for the bit map graphics data for all characters it is asked to print. 


;======DEFINE VARIBLES======
GRAPHICS 			.EQ	$C050
TEXT				.EQ $C051
HIRES				.EQ	$C057
PAGE1				.EQ	$C054
MIXOFF				.EQ	$C052
PAGE2				.EQ	$C055		
	

	
;TEXT SCREEN VARIABLES 
CLEAR.TEXT.SCREEN	.EQ $FC58
HCG.ON.ADDRESS		.EQ $0300
COUT.ADDRESS		.EQ $FDED			;SEND ACC TO DEFAULT OUTPUT DEVICE
;COUT				.EQ $FDED			;SEND ACC TO DEFAULT OUTPUT DEVICE
COUT.V				.EQ	$FDF0			;SEND ACC TO VIDEO SCREEN
UPDATE.CHAR.POS.ADDRESS		.EQ	$FC22			;RECALCULATE TEXT SCREEN LINE VALUE STORED AT $28, BASED ON ROW VALUE STORED AT $25
; UPDATE.CURSOR.POS		 	.EQ UPDATE.CHAR.POS
; UPDATE.CURSOR.POS.ADDRESS 	.EQ UPDATE.CHAR.POS.ADDRESS

COUT					 .EQ $FDED
UPDATE.CHAR.POS			.BS $01
HCG.OFF 				.BS $01

HTAB				.EQ $24				;(X) HORIZONTAL CURSOR POSITION
VTAB				.EQ $25				;(Y) VERTICLE CURSOR POSITION

;Text Window: ROM (the only problem with it is that it sucks)
TW1					.EQ $20				;(X) TEXT WINDOW UPPER LEFT
TW2					.EQ	$21				;(X) WIDTH
TW3					.EQ $22				;(Y) TOP ROW
TW4					.EQ	$23				;(Y) BOTTOM ROW

;Text Window: Custom
;See ========NPC TALK=====(TEXT WINDOW FUNCTION)  in offloaded_variables.ASM


CHAR				.BS		$1			

CSW					.EQ		$36
VECT				.EQ		$3EA		

;PRINT.STR VARIBLES
STRING				.EQ 	$FC		;2byte. Pointer to the ascii string to be output to video screen.

;PRINT.BCD VARIABLES
BCD.DIGITS 			.BS 1


;FORMAT IS FILENAME LENGTH (BYTES), FILENAME. LENGTH IS RAW HEX NUMBER, NOT ASCII VALUE

;file.to.read .EQ $6100
	
;======INCLUDE FILES======


;				.IN 	C:\MY_CODE\testing\prodos_testing\OpenDir.test.ASM
;				.IN 	C:\MY_CODE\TEMP\OpenDir.TEST.ASM
			
				;My libraries
				;.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_graphics				
				;.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_text
				;.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_math
				.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_general
				.IN 	C:\MY_CODE\INCLUDES_LIBS\ptools.ASM


				
;LOAD ZX7 UNPACKER
		.NO $BF00, $00
		
				.BI C:\MY_CODE\testing\compression_testing\zx7\zx7.bf00.bin,BIN	;unpack driver for LZ4 compression
				
				
				
;seperate target files	
				.IN 	C:\MY_CODE\TESTING\COMPRESSION_TESTING\ZX7\COMP_ZX7.TEST.BOOT.ASM
				
