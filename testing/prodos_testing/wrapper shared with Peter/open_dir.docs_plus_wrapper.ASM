

;=============DOCUMENTATION===========
;
;The following is documentation written by Mark Lemmert for ProRWTS (by Peter Ferrie) 
;and a front-end interface wrapper (PRODOS.IO) for ProRWTS, written by Mark Lemmert of 6502 Workshop, LLC with guidance from Peter Ferrie. 
;
;***THIS DOCUMENTATION AND CODE IS PROVIDED "AS-IS" WITHOUT EXPRESS OR IMPLIED WARRANTY. USE AT YOUR OWN RISK.***
;
;=====ProRWTS FUNCTION DESCRIPTION==== 
;
;ProRWTS provides read/write file access routines for ProDOS ordered file systems,
;both hard disk and floppy. I/O Performance is several times faster than DOS 3.3 RWTS.
;
;a) ProDOS loaded on boot (Floppy or Hard Disk)
;After ProRWTS is loaded, the main ProDOS operating system can be clobberted,
;while retaining I/O access to the ProDOS file structure. 
;
;See "SETUP DISK IMAGE" section below for details on how to use ProRWTS with 
;with a disk image that already contains ProDOS. 
;
;
;OR (Floppy Disk only)
;
;b) Bootloader
;Enables read/write of files stored on a floppy disk
;formatted with a ProDOS ordered file system, 
;without loading ProDOS into memory or storing it on disk.
;
;See "BOOTLOADER" section below for details on how on the bootloader.
;
;===VERSIONS==
;Two versions of ProRWTS curretly exist. 
;The original, refered to as ProRWTS, and ProRWTS2 which supports arbitrary file access. 
;
;===TEMPLATES====
;See "TEMPLATES, NO WRAPPER" section below and for wrapper templates see PRODOS.IO subroutine below. The templates are just below
;that, commented out. 
;
;The wrapper simplifies I/O calls by doing a memory state check/restore and automatically recalling ProRWTS to check the other floppy drive if the file is not found.
;
;===FEATURES====
;Floppy support
;Hard Drive support
;Directory Support (subdirectories are supported but I didn't test that functionality)
;AUX and Main memory support
;Get filesize (ProRWTS2 only)
;Read/Write length in bytes (read length only in ProRWTS2)
;Seek (forward only) to a new position in an open file (ProRWTS2 only)
;
;===ENTRANCE====
;$D003 (ProRWTS2 only): open new file, read and write  (use drive1 or drive2 reqcmd).
;$D000 				  : current open file operations: seek, read and write (use only drive1 reqcmd)
;
;Unless seeking within a file will be done, $D000
;can be used for everything. 
;
;If a seek is needed then first open the file with a call
;to $D003, then seek to the desired position wihin the file via a call to $D000,
;then read or write to the file at the seeked position via a call to $D000
;
;Only $D003 needs the reqcmd parameter to contain the drive2 code if drive2 is being used. 
;Just use the drive1 code in reqcmd when calling $D000 as this will result in $D000 using whatever drive was specified to open the file. 
;If a drive2 code is used in reqcmd when calling $D000, that can cause problems. 
;
;
;===AUX/MAIN MEMORY SUPPORT====
;The driver can read a file into aux or main memory
;and it can write data to a file located in aux
;or main memory.
;
;Since the driver resides in bank switched ram ($D000 bank 1)
;it is able to access both aux and main memory.
;
;To load data into bank switched ram, first read it into lower memory and then copy it into banks switched memory. It
;is possible to use ProRWTS to read directly into the portion of BSR bank1 not occupied by the driver, but the wrapper hasn't been tested for that scenario.
;
;====INFORMATION ON PARAMETERS=====
; All parameters must be set before each call to ProRWTS as driver will modify the values of the parameters.
;
; 	reqcmd 			;(read/write/seek) ([$1=read drive1, | $2=write drive1], [$81=read drive2 | $82=write drive2])
; 	sizelo/sizehi*	;(read/write/seek) number of bytes to read or write. Use $FFFF to read an entire file (it's okay for sizelo/hi to exceed the size of the file). For write value must be even number of pages.
; 	ldrlo/ldrhi		;(read/write) destination address for read, source address for write. (ProRWTS2 only, for read length)
; 	namlo/namhi** 	;(read/write) pointer to filename to use (ascii array). 
;	auxreq***		;(read/write) ($00 = main memory, $01 = aux memory)
;
;*To conform to the 2 page block structure of the ProDOS file system, sizelo/hi for all write operations must be an even number of pages (i.e. 2, 4, 6, 8, $A).
;*sizelo/hi for read and seek operations may be an arbitrary byte value. 
;
;**Filename array must be preceded by byte containing the number of bytes in the filename array. 
;**Maximum file name size = !15 bytes. 
;**Requires filename to be stored as ASCII low values. In SBASM terminology, don't use negative ASCII. 
;**Example (SBASM cross-assembler format):
;		file.to.read  .AZ #$0A,/DUMMY.FILE/
;
;***Peter's comments: Just to be clear about this - since you have to specify auxreq to each
;read or write, you can change the value of auxreq between reads or
;writes.  It means that file content can be loaded to either main or
;aux on one read, and then other content from the same file can be
;loaded to aux or main on another read, without needing to reopen the
;file in between.
;
;===SEEK FEATURE====(ProRWTS2 only)
;From Peter:
;Calls to $d000 must not specify the drive (i.e. don't try to select
;drive 2 in the reqcmd).  The current drive that was selected by $d003
;will be used.
;
;Seek is relative to the current position, so if you read 5 bytes and
;then seek 5 bytes, you'll be at position 10.
;
;You can get the size of a file by calling $d000 with reqcmd=cmdseek
;and size of 0.
;However, you'll have to call $d000 again to re-open the file if you
;want to read it.
;
;===MINUMUM READ/WRITE SIZE======
;HardDrive: 512 bytes (1 ProDOS block, 2 pages)
;			*Thus, the number of pages specified for a write must be a multiple of 2 (i.e. 2, 4, 6 etc)
;Floppy: 256 bytes (1/2 ProDOS block, 1 page)
;			*Thus, floppy file writes must be done in 256byte (1page) increments, the same as DOS 3.3 RWTS
;           *Note: ProRWTS2 has a 512 byte minimum read/write size for both floppy and hard disk
;
;;*The above describes how read/write operations work at the file system level.
;However, the driver abstracts this from it's interface. For example,
;if a read of 3 pages is performed on a hard disk, the driver will read 2 blocks (4 pages) from disk, but
;it will only place the 3 pages of legit data at the destination address.
;
;===MEMORY USAGE==== (ProRWTS2 only)
;
;The code uses memory starting at $800 before
;it moves itself to bank switched ram: bank1.
;
;Final memory range $D000-$D8FF (as of the upgrade to include support for read length/seek)
;
;$D000-D4FF permanently (driver code plus static data).
;
;$D500-D6FF is used only during a file-system request, and free for all
;other times (and for files < 512 bytes, it's available even during
;read requests, but you can't write to the file if you use it).
;
;$D700-D8FF* is the block cache which allows you to do partial reads.
;The first page (i.e. $D7xx) is also used during a floppy-only write
;request to reduce the memory use, which is why you can't read after
;write within the same 256-bytes range for floppy. i.e. if you seek to
;offset 512, write 3 bytes, don't try to read the next 10 bytes.  You'd
;need to seek into the next page first.
;
;*Note: if you only ever access a file using block-sized reads
;(i.e. multiples of 512 bytes at offsets that are also multiples of 512
;bytes), then $D700-D8FF becomes available.  However, as soon as you
;read fewer than 512 bytes, or read from an offset that isn't a
;multiple of 512, the buffer will be used.
;
;======RUN ProRWTS FROM AUX MEMORY====
;
;ProRWTS can be run from aux bank switched ram instead of
;from main bank switched ram. 
;
;To do this, copy ProRWTS to aux bank switched ram:bank2 before
;making any file I/O calls. Sample code for doing this is below.
;The code for MEMORY.COPY is not included. 
;
;Additionally, the front-end interface wrapper (PRODOS.IO) needs to enable/disable
;aux memory before and after ProRWTS is called. To enable this feature,
;uncomment the code in the sections tilted "Enable Aux" and "Disable Aux"
;
;
;ProRWTS.AUX_BSR.INIT ;COPY ProRWTS TO AUX BSR:BANK1
;;COPY ProRWTS FROM MAIN BSR:BANK1 TO AUX BSR:BANK1
;	LDA $c08b		;enable bank-switched ram ($d000 bank 1)
;	LDA $c08b
;		
;	;copy ProRWTS to main memory temp location
;		LDA #$00
;		STA COPY.FROM_START
;		LDA #$D0
;		STA COPY.FROM_START+$1
;		LDA #$FF
;		STA COPY.FROM_END
;		LDA #$D8
;		STA COPY.FROM_END+$1
;			
;		LDA #$00
;		STA COPY.TO
;		LDA #$B0
;		STA COPY.TO+$1
;	
;	JSR MEMORY.COPY		
;
;		
;		TSX			;transfer stack pointer to X-REG
;		STX TEMP	;save stack pointer
;	STA $C009 ;enable aux zero-page & aux BSR 
;
;	;copy ProRWTS from main memory temp location to aux BSR
;		LDA #$00
;		STA COPY.FROM_START
;		LDA #$B0
;		STA COPY.FROM_START+$1
;		LDA #$FF
;		STA COPY.FROM_END
;		LDA #$B8
;		STA COPY.FROM_END+$1
;			
;		LDA #$00
;		STA COPY.TO
;		LDA #$D0
;		STA COPY.TO+$1
;	
;	JSR MEMORY.COPY	
;		
;	STA $C008 ;enable main zero-page & main BSR 
;		LDX TEMP	;restore stack pointer to X-REG
;		TXS ;transfer X-REG to stack pointer
;
;	LDA $c08b		;enable bank-switched ram ($d000 bank 1)
;	LDA $c08b
;	
;	;erase original ProRWTS location in main BSR:bank1
;	;Note: the erase isn't required, it is included for testing purposes to ensure that the driver isn't accidentally run via it's original location in main BSR:bank1
;		LDA #$00
;		STA COPY.FROM_START
;		LDA #$A0
;		STA COPY.FROM_START+$1
;		LDA #$FF
;		STA COPY.FROM_END
;		LDA #$A8
;		STA COPY.FROM_END+$1
;			
;		LDA #$00
;		STA COPY.TO
;		LDA #$D0
;		STA COPY.TO+$1
;	
;	JSR MEMORY.COPY
;
;	
;========OTHER INFORMATION========
;
;*When writing a file with the driver, the file must already exist at its maxium size. Observe how dummy.file in the example progams (my_code/examples) and added to the disk image. 
;
;*Prdriver returns a file found ($00) or not found ($01) code 
;	via the variable STATUS, whether there is a disk in both drives or not. 
;	For ProRWTS2, the timeout detection (if a disk is not in the drive) only 
;	is inly supported by the $D003 entrance (open new files), not the $D000 entrance (perform operation on file already open)
;
;*# of Files impact on disk space
; (from Peter) - file entries in the file system aren't "free".  There
; are 13 files per block.  The 14th file will require one more block on
; the disk.  For each file that is larger than a block, there will be a
; block that holds the block list for that file.  If you have, say,
; three files that are 513 bytes long, that's six blocks on the disk
; (three block lists, three data blocks).
; With the new driver, you can pack many files into a single one if you
; want to.  It could save a lot of space.  We can talk more about that
; if it's interesting to you.
;
;=====SETUP DISK IMAGE=====
;*To use ProRWTS with a harddrive or floppy ProDOS disk image, copy NOXARCH.MAIN and NOXARCH.SYSTEM
;onto the harddrive, either to the root directory to a folder. 
;To run, use the command:
;
;-NOXARCH.SYSTEM
;
;Note: this file cannot be executed via the BRUN command. 
;
;The disk image can be setup to auto-boot by creating a STARTUP
;file (ProDOS hello program), which contains the above command. 
;
;About NOXARCH.MAIN; this file contains your code. (.ORG = $1000, loaded by NOXARCH.SYSTEM).
;
;=====BOOTLOADER=====
;
;The bootloader loads a file into $2000 called NOXARCH.SYSTEM, which
;takes care of loading ProRWTS to bank switched ram: bank1. 
;NOXARCH.SYSTEM concludes handing off control by loading NOXARCH.MAIN at $1000 (put your code here)
;
;Earlier versions of ProRWTS had a limit to the size of NOXARCH.MAIN ($1000 bytes) without modifying
;NOXARCH.SYSTEM so that it doesn't try to return to $2000. However, the current
;version of ProRWTS and ProRWTS2 does not have this limitation. 
;
;Before the size limitation was remove, one approach was to use NOXARCH.MAIN to continue the bootstrapping process
;and load another file using ProRWTS, which will contain the actual program you want to run. 
;This file could be loaded anywhere, including $2000 where NOXARCHSYSTEM was loaded. 
;NOXARCH.MAIN just needs to JMP to it after the read is complete. 
;
;
;========ZERO PAGE USAGE=====
;
;                tmpsec    = $3c
;                reqsec    = $3d
;                A1L       = $3c         ;only during init
;                A1H       = $3d         ;only during init
;                A2L       = $3e         ;only during init
;                A2H       = $3f         ;only during init
;                A3L       = $40         ;only during init
;                A3H       = $41         ;only during init
;                curtrk    = $40
;
;                command   = $42         ;ProDOS constant
;                unit      = $43         ;ProDOS constant
;                adrlo     = $44         ;ProDOS constant
;                adrhi     = $45         ;ProDOS constant
;                bloklo    = $46         ;ProDOS constant
;                blokhi    = $47         ;ProDOS constant
;
;                bleftlo   = $ef         ;(internal) bytes left in file
;                blefthi   = $f0         ;(internal) bytes left in file
;                blkofflo  = $f1         ;(internal) offset within cache block
;                blkoffhi  = $f2         ;(internal) offset within cache block
;                status    = $f3         ;returns non-zero on error
;                auxreq    = $f4         ;set to 1 to read/write aux memory
;                sizelo    = $f5         ;size of request
;                sizehi    = $f6         ;size of request
;                entries   = $f7         ;(internal) total number of entries
;                reqcmd    = $f8         ;0=seek, 1=read, 2=write
;                ldrlo     = $f9         ;set to load address
;                ldrhi     = $fa         ;set to load address
;                namlo     = $fb         ;set to name of file to open
;                namhi     = $fc         ;set to name of file to open
;                step      = $fd         ;(internal) state for stepper motor
;                tmptrk    = $fe         ;(internal) temporary copy of
;				 current track
;                phase     = $ff         ;(internal) current phase for seek
;                reloc     = $d000
;                dirbuf    = reloc+$500  ;$200 bytes
;                encbuf    = dirbuf+$200 ;$200 bytes
;				
;
;=====================================

;=====TEMPLATES, DIRECT ACCESS (NO WRAPPER)======
;(wrapper templates are in PRODOS.IO, below)

;CONSTANTS
; cmd_seek.current_drive  .EQ $0
; cmd_read.current_drive  .EQ $1
; cmd_write.current_drive .EQ $2	
;			
; cmd_read.drive1		.EQ $1
; cmd_read.drive2		.EQ $81
; cmd_write.drive1		.EQ $2
; cmd_write.drive2		.EQ $82
;
; bleftlo 				.EQ $EF ;Bytes left in file. If no bytes have been read then it is the total bytes in the file. 
; bleftho 				.EQ $F0
;
;
;
;====GET FILE SIZE======
;Comments: by opening the file but reading zero bytes, the
;variable bleftlo/ho will contain the total bytes in the file. 
;
;		lda #cmd_read.drive2		;([$1=read drive1, | $2=write drive1], [$81=read drive2 | $82=write drive2])
;		sta reqcmd			
;		
;		lda #$00
;		sta sizelo			;read length (LO byte)
;		
;		lda #$00			;read length (HO byte)
;		sta sizehi
;		
;		lda #$00			;read destination (LO byte)
;		sta ldrlo
;		
;		lda #$A0			;read destination (HO byte) 
;		sta ldrhi
;		
;		lda #file.to.read	;pointer to ascii array containing file name, with filename length byte header. 15 character max.
;		sta namlo
;		
;		lda /file.to.read   ;pointer to ascii array containing file name, with filename length byte header. 15 character max.
;		sta namhi
;		
;		LDA $c08b		;enable bank-switched ram ($d000 bank 1)
;		LDA $c08b
;	
;		LDA #$00		;use main memory ($00 = main, $01 = aux)
;		STA AUXREQ
;		jsr $d003
;
;
;		LDA $c082
;		LDA STATUS		;($00 = file found, $01 = file not found)
;		LDX bleftlo		;load file length LO byte
;		LDY bleftho		;load file length HO byte
;		BRK
;
;
;=====WRITE/READ/SEEK/READ========
;Comments: this example does the following:
;		*Open new file, write $1000 bytes from $A000-$AFFF
;		*Open new file, read $1000 bytes into $7000-$7FFF
;		*Seek current file +$1000 bytes
;		*Read current file, $1000 bytes into $8000
;
;;-START
;;open new file and write $1000 bytes
;;file position start:	$0
;;file position end:		$1000
;
;		lda #$82	;([$1=read drive1, | $2=write drive1], [$81=read drive2 | $82=write drive2])
;		sta reqcmd
;		
;		lda #$00				;# of pages to write (LO byte). (must be in multiples of 2. i.e. 2, 4, 6, 8 $0A, etc)
;		sta sizelo
;		
;		lda #$10				;# of pages to write (HO byte). (must be in multiples of 2. i.e. 2, 4, 6, 8 $0A, etc)
;		sta sizehi
;		
;		lda #$00				;write source data (LO byte)
;		sta ldrlo
;		
;		lda #$A0				;write source data (HO byte)
;		sta ldrhi
;		
;		lda #file.to.write		;pointer to ascii array containing file name, with filename length byte header. 15 character max.	
;		sta namlo
;		
;		lda /file.to.write		;pointer to ascii array containing file name, with filename length byte header. 15 character max.
;		sta namhi
;		
;		LDA $c08b		;enable bank-switched ram ($d000 bank 1)
;		LDA $c08b
;	
;		LDA #$00		;Select memory ($00 = main, $01 = aux)
;		STA AUXREQ
;		jsr $d003				;entrance for open new file, read, write
;
;			
; ;open new file and read $1000 bytes 
; ;file position start:	$0
; ;file position end:		$1000
;		lda #$81	;([$1=read drive1, | $2=write drive1], [$81=read drive2 | $82=write drive2])
;		sta reqcmd
;		
;		lda #$00				;read length (LO byte)
;		sta sizelo
;		
;		lda #$10				;read length (HO byte)
;		sta sizehi
;		
;		lda #$00				;read destination address (LO byte)
;		sta ldrlo
;		
;		lda #$70				;read destination address (HO byte)
;		sta ldrhi
;		
;		lda #file.to.read		;pointer to ascii array containing file name, with filename length byte header. 15 character max.	
;		sta namlo
;		
;		lda /file.to.read		;pointer to ascii array containing file name, with filename length byte header. 15 character max.
;		sta namhi
;		
;		LDA $c08b		;enable bank-switched ram ($d000 bank 1)
;		LDA $c08b
;	
;		LDA #$00		;Select memory ($00 = main, $01 = aux)
;		STA AUXREQ
;		
;		jsr $d003				;entrance for open new file, read, write
;
;		
; ;seek to position $2000 in opened file
; ;file position start:	$1000
; ;file position end:		$2000
		; lda #$00	;([$1=read drive1, | $2=write drive1], [$81=read drive2 | $82=write drive2 | $00 = seek]
		; sta reqcmd
		
		; lda #$00				;# of pages to write (LO byte). (must be in multiples of 2. i.e. 2, 4, 6, 8 $0A, etc)
		; sta sizelo
		
		; lda #$10				;# of pages to write (HO byte). (must be in multiples of 2. i.e. 2, 4, 6, 8 $0A, etc)
		; sta sizehi
		
		; lda #$00				;write source data (LO byte)
		; sta ldrlo
		
		; lda #$A0				;write source data (HO byte)
		; sta ldrhi
		
		; lda #file.to.write		;pointer to ascii array containing file name, with filename length byte header. 15 character max.	
		; sta namlo
		
		; lda /file.to.write		;pointer to ascii array containing file name, with filename length byte header. 15 character max.
		; sta namhi
		
		; LDA $c08b		;enable bank-switched ram ($d000 bank 1)
		; LDA $c08b
	
		; LDA #$00		;Select memory ($00 = main, $01 = aux)
		; STA AUXREQ
		; jsr $d003				;entrance for open new file, read, write

			
; ; open new file and read $1000 bytes 
; ; file position start:	$0
; ; file position end:		$1000
		; lda #$81	;([$1=read drive1, | $2=write drive1], [$81=read drive2 | $82=write drive2 | $00 = seek]
		; sta reqcmd
		
		; lda #$00				;read length (LO byte)
		; sta sizelo
		
		; lda #$10				;read length (HO byte)
		; sta sizehi
		
		; lda #$00				;read destination address (LO byte)
		; sta ldrlo
		
		; lda #$70				;read destination address (HO byte)
		; sta ldrhi
		
		; lda #file.to.read		;pointer to ascii array containing file name, with filename length byte header. 15 character max.	
		; sta namlo
		
		; lda /file.to.read		;pointer to ascii array containing file name, with filename length byte header. 15 character max.
		; sta namhi
		
		; LDA $c08b		;enable bank-switched ram ($d000 bank 1)
		; LDA $c08b
	
		; LDA #$00		;Select memory ($00 = main, $01 = aux)
		; STA AUXREQ
		
		; jsr $d003				;entrance for open new file, read, write


		
; ; seek to position $2000 in opened file
; ; file position start:	$1000
; ; file position end:		$2000
		; lda #$00		;([$1=read drive1, | $2=write drive1], [$81=read drive2 | $82=write drive2 | $00 = seek]
		; sta reqcmd
		
		; lda #$00		;number of bytes to seek (LO byte)
		; sta sizelo
		
		; lda #$10		;number of bytes to seek (HO byte)
		; sta sizehi

		; LDA $c08b		;enable bank-switched ram ($d000 bank 1)
		; LDA $c08b

		; LDA #$00		;select memory (this may not be needed for a seek, but to be safe set it to the same value as specified when opening the file)
		; STA AUXREQ
		; jsr $d000
		
; ; read $1000 bytes to $8000 from opened file
; ; file position start:	$2000
; ; file position end:		$3000		

			; ;don't specify drive2 when read/writing from existing file. The driver knows to use the drive the file was found on when the file was opened
		; lda #$1 			;([$1=read drive1, | $2=write drive1], [$81=read drive2 | $82=write drive2 | $00 = seek]
		; sta reqcmd
		
		; lda #$00				;read length (LO byte)
		; sta sizelo
		
		; lda #$10				;read length (HO byte)
		; sta sizehi
		
		; lda #$00				;read destination address (LO byte)
		; sta ldrlo
		
		; lda #$80				;read destination address (HO byte)
		; sta ldrhi
		
		; lda #file.to.read		;pointer to ascii array containing file name, with filename length byte header. 15 character max.	
		; sta namlo
		
		; lda /file.to.read		;pointer to ascii array containing file name, with filename length byte header. 15 character max.
		; sta namhi
		
		; LDA $c08b		;enable bank-switched ram ($d000 bank 1)
		; LDA $c08b
	
		; LDA #$00		;Select memory ($00 = main, $01 = aux)
		; STA AUXREQ

		; jsr $d000
				
		; LDA $C082				;enable ROM, disable BSR
		; LDA STATUS		;($00 = file found, $01 = file not found)
		; LDX #$AA
		; BRK		

		

PRODOS.IO ;front-end interface wrapper for ProRWTS 

;PARAMETERS: ACC (parm.auxreq), parm.reqcmd*, parm.ldrlo, parm.ldrhi, parm.namlo, parm.namhi, parm.sizehi, parm.sizelo
;ENTRANCE: direct
;RETURN: parm.status ($00 success, $01 file not found), parm.bleftlo**, parm.bleftho**
;**file size, if a read of $00 bytes is done. 
;**parm.current.file is not a ProRWTS parameter. It is a parameter I created for this wrapper to indicate if an operation (read/write/seek) is being performed on the current file from the current position. 
;		if set to $01 (current file), then PRODOS.IO wrapper assumes it should open a new file via $D003 instead of using the current file via $D000
;		If this parm is set by a call, make sure to reset it to $00 at the end of the call so that by default calls to PRODOS.IO are assumed to be for opening new files, which is the case most of the time. 
;
;*parm.reqcmd: the values expected byte the driver are
;	([$1=read drive1, | $2=write drive1], [$81=read drive2 | $82=write drive2])
;However, I added the following:
;	[$90=seek current_drive, | $91=read current_drive, | $92=write current_drive]
;	see subroutine documentation for details

;=====================DOCUMENTATION====================================
;
;This routine is a front-end interface wrapper for calling ProRWTS written by Peter Ferrie. 
;
;***THIS DOCUMENTATION AND CODE IS PROVIDED "AS-IS" WITHOUT EXPRESS OR IMPLIED WARRANTY. USE AT YOUR OWN RISK.***
;
;The wrapper does the following:
;
;*makes sure that the memory state is returned to it's status upon entry to 
;the wrapper
;*checks to see if reqcmd is >= $90 (see below)
;*detect "file not found" (STATUS = $01 after the driver returns control
;*Checks the disk in both drives for the file requested
;*Support for ProRWTS resident in main bank switched ram and aux bank switched ram.
;		;If ProRWTS is in aux bank switched ram, then uncomment the code sections titles "Enable Aux" and "Disable Aux"
;
;==REQCMD >= $90==
;reqcmd: the values expected byte the driver are
;	([$1=read drive1, | $2=write drive1], [$81=read drive2 | $82=write drive2])
;However, I added the following:
;	[$90=seek current_file, | $91=read current_file, | $92=write current_file]
;
;When a current drive operation is preformed on the current file, this wrapper
;routine converts the REQCMD value to $00 - $02 by masking out
;bit 4 and bit 7 via AND #$6F (effectively subtracting $90 from REQCMD) 
;
;This conversion is done because the ProRWTS expects $00-$02 to be used for
;a current file operation. It distinguishes a current file operation from an operation
;in conjunction with opening a new file by the entrace address.
;$d000 is the entrance for read/write/seek on the current open file (same drive is used as when the file was opened) 
;$d003 is the entrance for opening a new file in conjunction with a read or write operation. 
;=================================================================================


;======TEMPLATES=========

;;COPY ProRWTS TO AUX BSR
;	JSR ProRWTS.AUX_BSR.INIT

;TEMPLATE.WRITE.OPEN_FILE

;
; ;set command type (READ | WRITE | SEEK)
; ; cmd_seek.current_drive 	.EQ $90
; ; cmd_read.current_drive  	.EQ $91
; ; cmd_write.current_drive 	.EQ $92
; ;			
; ; cmd_read.drive1			.EQ $1
; ; cmd_read.drive2		  	.EQ $81
; ; cmd_write.drive1			.EQ $2
; ; cmd_write.drive2			.EQ $82

;	lda #cmd_write.drive1 ;([$1=read drive1, | $2=write drive1], [$81=read drive2 | $82=write drive2], [$90=seek current_drive, | $91=read current_drive, | $92=write current_drive])
;	sta parm.reqcmd	
;	
;;set write data size (# of 512 byte blocks to write from memory)
;	lda #$00 ;always #$00
;	sta parm.sizelo
;
;	lda #$02 ;number of pages to write. ;# of pages to write (LO byte). (must be in multiples of 2. i.e. 2, 4, 6, 8 $0A, etc)
;	sta parm.sizehi
;
;
;;set write buffer address (source data)
;	lda #$00
;	sta parm.ldrlo
;	lda #$A0
;	sta parm.ldrhi
;	
;;set filename to write to	
;	lda #file.to.write	;load LO address
;	sta parm.namlo
;	lda /file.to.write	;load HO address
;	sta parm.namhi
;
;		LDA #$00	;PARM: $00=main, $01=aux 
;	JSR PRODOS.IO

;
;TEMPLATE.SEEK.CURRENT_FILE

; ;set command type (READ | WRITE | SEEK)
; ; cmd_seek.current_drive 	.EQ $90
; ; cmd_read.current_drive  	.EQ $91
; ; cmd_write.current_drive 	.EQ $92
; ;			
; ; cmd_read.drive1			.EQ $1
; ; cmd_read.drive2		  	.EQ $81
; ; cmd_write.drive1			.EQ $2
; ; cmd_write.drive2			.EQ $82

;	lda #cmd_seek.current_drive
;	sta parm.reqcmd	
;	
;;set # of bytes to seek
;	lda #$00 
;	sta parm.sizelo
;
;	lda #$0E ;number of pages to write. ;# of pages to write (LO byte). (must be in multiples of 2. i.e. 2, 4, 6, 8 $0A, etc)
;	sta parm.sizehi
;	
;		LDA #$00	;PARM: $00=main, $01=aux 
;	JSR PRODOS.IO
;

;	
;TEMPLATE.READ.CURRENT_FILE

; ;set command type (READ | WRITE | SEEK)
; ; cmd_seek.current_drive 	.EQ $90
; ; cmd_read.current_drive  	.EQ $91
; ; cmd_write.current_drive 	.EQ $92
; ;			
; ; cmd_read.drive1			.EQ $1
; ; cmd_read.drive2		  	.EQ $81
; ; cmd_write.drive1			.EQ $2
; ; cmd_write.drive2			.EQ $82

;	lda #cmd_read.current_drive
;	sta parm.reqcmd
;
;;set destination memory address
;	lda #$00
;	sta parm.ldrlo
;	lda #$70
;	sta parm.ldrhi
;
;;set read length (bytes)
;	lda #$FF				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
;	sta parm.sizelo	
;	lda #$FF				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
;	sta parm.sizehi
;	
;		LDA #$00	;PARM: $00=main, $01=aux 
;	JSR PRODOS.IO
;
;	LDA $C082				;enable ROM, disable BSR
;	LDA STATUS
;	LDX #$DD
;	LDY #$AA
;	BRK	



	
;SETUP MAIN/AUX MEMORY PARAMETERS
	;ACC contains auxreq
	STA parm.auxreq ; $00=main, $01=aux
	
;SAVE REGISTERS
	PHA
	TXA
	PHA
	TYA
	PHA
	
;INIT VARIABLES
	LDA #$00
	STA IO.ATTEMPTS
	
;GET MAIN BSR MEMORY-STATUS UPON ENTRY
	; LDA $C012		;ROM/BSR soft-switch flag (bit7 = 1: BSR, bit7=0 ROM)
	; STA BSR.STATE
	; LDA $C011		;BANK1/BANK2 soft-switch flag (bit7 = 1: BANK2, bit7=0 BANK1)
	; STA BANK.STATE

	JSR GET.BSR_BANK.STATUS

;ENABLE AUX BANK-SWITCHED RAM ($D000 Bank 1)
	LDA $c08b
	LDA $c08b

;ENABLE AUX BSR & ZERO PAGE
		TSX			;transfer stack pointer to X-REG
		STX TEMP	;save stack pointer
	STA $C009 ;enable aux zero-page & aux BSR.
		;***Must be done before copying the ProRWTS parameters from main memory onto zero-page because switching to aux BSR also switches to aux zero-page.

		
;LOAD AUX ZERO-PAGE PARMS, THEN CALL ProRWTS
		lda parm.reqcmd 	
		sta reqcmd

.READ_WRITE.FILE

		lda parm.auxreq 	
		sta auxreq
		
		lda parm.sizelo 	
		sta sizelo
		
		lda parm.sizehi 	
		sta sizehi
		
		lda parm.ldrlo 		
		sta ldrlo

		lda parm.ldrhi 		
		sta ldrhi

		lda parm.namlo 		
		sta namlo

		lda parm.namhi 	
		sta namhi

	LDA reqcmd
	CMP #cmd_seek.current_drive
	BCS .USE.CURRENT.FILE
	;**FALLS THROUGH**
.OPEN.NEW.FILE	
	JSR $d003 ;ProRWTS I/O: entrance for opening new files + read/write
	JMP .FILE.OPERATION.COMPLETE
	
.USE.CURRENT.FILE
	;ACC = reqcmd
	
	;convert $90-$91 (values I made up for seek/read/write to current file) to $00-$02 (the actual values the driver expects)
	AND #$6F 	;mask out bit4 and bit7
	STA reqcmd	;save converted reqcmd value. 
	JSR $d000 ;ProRWTS I/O: entrance for operations on files already open (read/write/seek)
	;**FALLS THROUGH**
	
.FILE.OPERATION.COMPLETE	
	LDA status			;load ProRWTS return code
	BEQ .IO.SUCCEEDED	;was file-not-found ($01) reported? If no, then treat IO as successful
	LDA IO.ATTEMPTS		;check whether both drives have been searched for the file requested
	BNE .INSERT.DISK	;if both drives have been checked (>=$01) then prompt player to insert disk
						;if not, try other drive 
		
	;determine which drive to try next
		
	LDA parm.reqcmd 	;the reqcmd value specified by the calling routine. Don't use reqcmd because it could have been modified by the ProRWTS. 
	BMI .TRY.DRIVE1		;is bit7 = 1? Then drive2 was just tried, check drive1 next
;.TRY.DRIVE2
	ORA #$80			;effectively adds #$80 to the ACC by changing bit 7 to $1. After ORA the ACC will contain a 1 for each bit which had a value of 1 in either the ORA value or the value in the ACC before the ORA was executed. 
						;Since we know that the read/write was first attempted on drive1 we know the value in the ACC was either $1 (read) or $2= (write). Adding $80 resuts in the values needed for drive2, for whichever operation (read or write) was aready specified. i.e. ($81 = read drive2, $82 = write drive2)
	STA REQCMD			;update the drive in the read/write command parameter
	INC IO.ATTEMPTS

	JMP .READ_WRITE.FILE

	
.TRY.DRIVE1
	AND #$03		;effectively subtracts #$80 from the ACC by changing bit 7 to $0. After the logical "AND" the ACC will contain a 1 for each bit which had a value of 1 in both the AND value and the value in the ACC before the AND was executed. In this case, bit 0 was the only bit with a value of $1 in both.  
					;Since we know that the read/write was first attempted on drive2 we know the value in the ACC was either $81 (read) or $82= (write). Subtracting $80 resuts in the values needed for drive1, for whichever operation (read or write) was aready specified. i.e. ($1 = read drive1, $2 = write drive1)
	STA REQCMD		;update the drive in the read/write command parameter

	INC IO.ATTEMPTS

	JMP .READ_WRITE.FILE
	
.INSERT.DISK
	;<WRITE CODE FOR DISK PROMPT>. 
	;probably will be positioned on screen differently
	;if the prompt occurs during boot than if it occurs
	;during game play
			
		STA $C008 ;enable main zero-page & main BSR 
			LDX TEMP	;restore stack pointer to X-REG
			TXS ;transfer X-REG to stack pointer

			JSR PREP.BRK
							
;Print "NO FILE" to text screen		
			LDA #$CE
			JSR COUT.ADDRESS
			LDA #$CF
			JSR COUT.ADDRESS
			LDA #$A0
			JSR COUT.ADDRESS
			LDA #$C6
			JSR COUT.ADDRESS
			LDA #$C9
			JSR COUT.ADDRESS
			LDA #$CC
			JSR COUT.ADDRESS
			LDA #$C5
			JSR COUT.ADDRESS			
		
			BRK

	
.IO.SUCCEEDED

;COPY SOME PARMS FROM AUX ZERO-PAGE TO MAIN MEMORY
;Note: this is so they can be retreived by the calling routine after exit, when main memory zero-page and BSR are enabled. 
	lda bleftlo
	sta parm.bleftlo 
	lda bleftho
	sta parm.bleftho 	
	lda status
	sta parm.status 	

;ENABLE MAIN BSR AND ZERO-PAGE
	STA $C008 ;enable main zero-page & main BSR 
		LDX TEMP	;restore stack pointer to X-REG
		TXS ;transfer X-REG to stack pointer
	
;RESTORE MEMORY-STATUS

	JSR RESTORE.BSR_BANK.STATUS

	; LDA BSR.STATE					;get BSR State (either ROM enabled or BSR enabled) before P.IO driver was called
	; BMI .GET.PRIOR.BSR.BANK_STATE	;is bit7 = 1? If yes, BSR was enabled, find out which bank
	; STA $C082						;if no, ROM was enabled, so re-enable ROM
	; BPL .alldone					;BPL must be true if BMI was false
; .GET.PRIOR.BSR.BANK_STATE
	; LDA BANK.STATE					;get BSR Bank State (either Bank1 or Bank2 enabled) before P.IO driver was called
	; BMI .BANK2						;is bit7=1? If yes, bank2 was enabled
	; STA $C08B						;if no, Bank1 was enabled, so re-enable bank1
	; STA $C08B
	; BPL .ALLDONE
; .BANK2
	; STA $C083						;re-enable bank2
	; STA $C083
; .ALLDONE
	

;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX
	PLA

	RTS





GET.BSR_BANK.STATUS


;GET MAIN BSR MEMORY-STATUS UPON ENTRY
	LDA $C012		;ROM/BSR soft-switch flag (bit7 = 1: BSR, bit7=0 ROM)
	STA BSR.STATE
	LDA $C011		;BANK1/BANK2 soft-switch flag (bit7 = 1: BANK2, bit7=0 BANK1)
	STA BANK.STATE

	RTS


RESTORE.BSR_BANK.STATUS

;RESTORE MEMORY-STATUS
	LDA BSR.STATE					;get BSR State (either ROM enabled or BSR enabled) before P.IO driver was called
	BMI .GET.PRIOR.BSR.BANK_STATE	;is bit7 = 1? If yes, BSR was enabled, find out which bank
	STA $C082						;if no, ROM was enabled, so re-enable ROM
	BPL .alldone					;BPL must be true if BMI was false
.GET.PRIOR.BSR.BANK_STATE
	LDA BANK.STATE					;get BSR Bank State (either Bank1 or Bank2 enabled) before P.IO driver was called
	BMI .BANK2						;is bit7=1? If yes, bank2 was enabled
	STA $C08B						;if no, Bank1 was enabled, so re-enable bank1
	STA $C08B
	BPL .ALLDONE
.BANK2
	STA $C083						;re-enable bank2
	STA $C083
.ALLDONE

	RTS

	
	
;======DEFINE VARIBLES======


;ProRWTS 
;These are variables used by NOXARCH.SYSTEM, a file
;Peter built and installed on the bootloader. 

bleftlo   = $EF ;Bytes left in file. If no bytes have been read then it is the total bytes in the file. 
bleftho   = $F0
status    = $f3         ;returns non-zero on error
auxreq	  = $f4			;$00 = main memory, $01 = aux memory
sizelo    = $f5         ;must set if reading or writing. For writing, # of pages to write must be in multiples of 2. i.e. 2, 4, 6, 8 $0A, etc) 
sizehi    = $f6         ;must set if reading or writing.
reqcmd    = $f8         ;used if enable_write=1, 1=read, 2=write
ldrlo     = $f9         ;used if override_adr=1
ldrhi     = $fa         ;used if override_adr=1
namlo     = $fb
namhi     = $fc

ProRWTS		.EQ $D000	;entrance for opening a new file. $D003 is entrace for read/write/seek on file already open. 

;main memory storage for ProRWTS variables
;Note: this is because once PRODOS.IO enables aux BSR, aux zero-page is also enabled. So, the parms can't be set in zero-page before calling PRODOS.IO. Instead we set the parms in main memory and PRODOS.IO will copy them onto the aux zero-page. 
parm.bleftlo 	.EQ ZONE_TOOLS.INPUT_BUFFER+$F2
parm.bleftho 	.EQ ZONE_TOOLS.INPUT_BUFFER+$F3
parm.status 	.EQ ZONE_TOOLS.INPUT_BUFFER+$F4
parm.auxreq 	.EQ ZONE_TOOLS.INPUT_BUFFER+$F5
parm.sizelo 	.EQ ZONE_TOOLS.INPUT_BUFFER+$F6
parm.sizehi 	.EQ ZONE_TOOLS.INPUT_BUFFER+$F7
parm.reqcmd 	.EQ ZONE_TOOLS.INPUT_BUFFER+$F8
parm.ldrlo 		.EQ ZONE_TOOLS.INPUT_BUFFER+$F9
parm.ldrhi 		.EQ ZONE_TOOLS.INPUT_BUFFER+$FA
parm.namlo 		.EQ ZONE_TOOLS.INPUT_BUFFER+$FB
parm.namhi 		.EQ ZONE_TOOLS.INPUT_BUFFER+$FC
parm.current.file .BS $1 ;if set to $01, then PRODOS.IO wrapper assumes it should open a new file via $D003 instead of using the current file via $D000

BSR.STATE 		.EQ ZONE_TOOLS.INPUT_BUFFER+$FD
BANK.STATE 		.EQ ZONE_TOOLS.INPUT_BUFFER+$FE

IO.ATTEMPTS		.EQ ZONE_TOOLS.INPUT_BUFFER+$FF

; ;(ALTERNATE) dedicated memory reservations
; parm.bleftlo	.BS $1
; parm.bleftho	.BS $1
; parm.status		.BS $1
; parm.auxreq		.BS $1
; parm.sizelo		.BS $1
; parm.sizehi		.BS $1
; parm.reqcmd		.BS $1
; parm.ldrlo 		.BS $1
; parm.ldrhI 		.BS $1
; parm.namlo 		.BS $1
; parm.namhi 		.BS $1
; parm.current.file .BS $1 ;if set to $01, then PRODOS.IO wrapper assumes it should open a new file via $D003 instead of using the current file via $D000
; ;
; BSR.STATE 		.BS $1
; BANK.STATE 		.BS $1
; IO.ATTEMPTS		.BS $1




;constants
cmd_seek.current_drive  .EQ $90
cmd_read.current_drive  .EQ $91
cmd_write.current_drive .EQ $92
			
cmd_read.drive1			.EQ $1
cmd_read.drive2		  	.EQ $81
cmd_write.drive1		.EQ $2
cmd_write.drive2		.EQ $82


;misc
COUT.ADDRESS		.EQ $FDED
TEXT				.EQ $C051



;======INCLUDE FILES======

;none
	
