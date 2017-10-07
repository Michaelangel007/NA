
;************************INCLUDE FILE*****************************
;(do not assemble stand-alone.  )


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
;or main memory. This is only possible when the driver resides in bank switched ram ($D000 bank 1)
;which enables it to access both aux and main memory at the same time. 
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
;
;-Information on what variables need to be reset between calls
;
;Q: If after a call to $D000 to do a seek, do these parameters need to be reinitialized before calling $D000 to do a read on the open file?
;A:  reqcmd - no, you can read multiple times in a row without setting it again.   (11/22/16; I tried leavig out reqcmd between a seek and read and it didn't seem to work)
; auxreq - no, it's never modified.
; ldrlo/hi - not unless you want to move the file position away from the
; current position.  If you just seek and then read, no need to touch
; it.
; namlo/hi - no, it's never modified.
;
;Q: If two open file calls are made via $D003, do all of the parameters need to be reset before the 2nd call?
;A: namlo/hi: yes, to specify the new name;
; reqcmd: yes, if you want to change drive, otherwise no;
; ldrlo/hi: yes
; auxreq: yes, if you want to change the memory type, otherwise no.
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
;-Resetting file offset (so seek starts with byte $0)
;(the following code will reset the file offset of the current
;file, without reopening the file. This is helpful because opening
;files is very slow on because the file location has to be found on the disk).
;On floppy disks the speed impact of avoiding extra file opens is huge. 
;
;Peter's comments which led to the code below
;
	; "In that case, you definitely want to seek, not re-open the file.
	; You might be able to achieve this right now, assuming that you won't
	; be reading off the end of the file - set the bleftlo and blefthi to
	; #$FF so that the file looks very large, set blkofflo and blkoffhi to
	; #$00 to reset the block pointer, and finally set entries to #$00 to
	; reset the file position.
	; I believe that this will fool ProRWTS into thinking that you've just
	; opened the file.  From there, issue a seek to where you'd like to be,
	; by JSR $D000.  That should not incur any disk activity at all, other
	; than spinning up the drive if it wasn't on already."
;
;
;
; ;RESET SEEK POINTER
			; STA $c009 ;enable aux zero page if ProRWTS is running in AUX BSR
	;
	; LDA #$FF
	; sta bleftlo
	; sta blefthi
	; lda #$00
	; sta blkofflo
	; sta blkoffhi
	; sta entries
	;
			; STA $c008 ;enable main zero page
;			
;
;-Resetting file offset (with the same block)
;Peter provided the following information that explains how to reset the 
;file offset to the start of the block thaat the current fileoffset is currently
;in. This could be useful if a lot of small reads are being done which are often in the
;the same block but not in sequential order. This is faster than resetting the fileoffset
;to the start of the file. 
;
;
; "Another trick regarding the file offset, that you might consider:
; if you are doing small reads within the same block (i.e. if (new file
; offset hi AND #$FE) == (old file offset hi AND #$FE)), then you can
; set blkofflo and hi to the new file offset AND #$1FF, and call read
; directly.  You don't touch the entries field at all, and don't seek!
; It works because the current block will be cached if only part of it was read.
; A few cycles of calculation could be a big saving if you are a long
; way into the file by that time, because seeking from offset 0 is
; expensive compared to seeking from the nearest block."
;
;I have not tried to impliment this because I don't think the Nox Archaist file I/O
;currently fit that profile. Additionally, from my testing, I can't detect the
;difference between sequential seek/reads and reset/seek/reads where the reset is to the
;beginning of the file. I think it would probably take a huge number of reads with large seek distances
;for it to be detectable to the user and therefore relevant to optimize further. 
;
;-Seeking when writing to a file
;The short answer is: yes it can be done, and it can be
;done as a file open+read, seek, write. i.e. a file opened with a read can then be written to. 
;
;Detailed Q&A with Peter (for original email, in personal gmail see "ProRWTS Seek+Write" on 5/31/2017)
;
;
; TO: Peter
; Is it possible to open a file, seek to an offset, and then write to the file? I've only used seek with reads so far. 
;
; If so, when I open the file can reqcmd be set to read ($1 or $81)? 
;
; The reason I'm thinking that is useful is that if reqcmd needs to be set to write when opening the file then it will write 2 pages to the start of the file and that data might not be in memory.
;
; Thanks.
;
; FROM: Peter
; Yes, you can start by opening with seek request, and then send the
; write request.
; …
;
;
; > If so, when I open the file can reqcmd be set to read ($1 or $81)?
;
; Or that.  You can switch to write at any time, and the write will
; occur at the current file position (aligned to a multiple of 512).
; …
;
;
; > The reason I'm thinking that is useful is that if reqcmd needs to be set to
; > write when opening the file then it will write 2 pages to the start of the
; > file and that data might not be in memory.
;
; Yes, it doesn't work like that for that reason.



;
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
;
;----UPDATE FROM PETER 3/24/2017---
;To the extent this info below about memory usage conflicts with the above, 
;the info below should be authoratative as it is more recent.
;
;floppy code: $D000-$D3FF
;floppy data: $D400-$D4FF
;floppy dirbuf: $D500-$D6FF
;floppy encbuf: $D700-$D8FF
;
; Also keep in mind that the above are all separate elements that can be put in different places in
; memory.  They don't have to be side-by-side.  For example, you can
; have the floppy code at $D800, the data at $E000, the dirbuf at $E800
; and the encbuf at $F000 or whatever combination you like.  They don't
; have to be in increasing order, either.  encbuf can be first or in the
; middle, dirbuf can be last, etc.
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
;=====SOURCE FILES====
;OPENDSK.S is the source for NOXARCH.SYSTEM.
;proboot.s is the 2nd stage loader in the bootloader
;
;see below for 1st stage loader code
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
;-Changing the NOXARCH.MAIN load address from $1000 to something else
;
;From Peter "NOXARCH.MAIN can load to wherever you want.  Just patch that one byte
;to the new page and you're done.  The system file passes control to
;the driver and the driver returns directly to main, not through the
;system file, so you can load as much as you want, anywhere that you
;want."
;
;-Bootloader Details
;
;The bootloader is track $0, sectors $0 and $E.  Per Peter "It's a very cut-down
;version of OPENDSK.S". Sector $0 and $E are used because these sectors
;are ProDOS block 0 (see Beneath Apple ProDOS).
;
;1st Stage Loader
;(loads proboot.S from Track $0, Sector $E)
;
;		!byte   1
;       ;branch if physical sector 2
;        lsr
;        bne     +
;        ;choose physical sector 2
;        inc     $3d
;        ;slot to address
;        txa
;        lsr
;        lsr
;        lsr
;        lsr
;        ora     #$c0
;        pha
;        lda     #$5b
;        pha
;        ;return to read second sector
;        rts
;
;        ;fake $BF00 interface
;        pla
;        tax
;        inx
;        inx
;        inx
;        txa
;        pha
;        rts
;
;filename
;        !byte filename_e-filename_b
;filename_b
;        !text "NOXARCH.SYSTEM"
;filename_e
;
;-2nd Stage Loader
;proboot.S
;
;-3rd Stage Loader
;proboot.S loades NOXARCH.SYSTEM into $2000 
;
;-4th Stage Loader
;NOXARCH.SYSTEM loads NOXARCH.MAIN into $1000
;
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
@START
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
@END
		
		
PRODOS.IO ;front-end interface wrapper for ProRWTS 
@START
;PARAMETERS: ACC (parm.auxreq), parm.reqcmd*, parm.ldrlo, parm.ldrhi, parm.namlo, parm.namhi, parm.sizehi, parm.sizelo
;ENTRANCE: direct
;RETURN: parm.status ($00 success, $01 file not found), parm.bleftlo**, parm.bleftho**
;**file size, if a read of $00 bytes is done. 
;**parm.current.file is not a ProRWTS parameter. It is a parameter I created for this wrapper to indicate if an operation (read/write/seek) is being performed on the current file from the current position. 
;		if set to $01 (current file), then PRODOS.IO wrapper assumes it should open a new file via $D003 instead of using the current file via $D000
;		 If this parm is set by a call, make sure to reset it to $00 at the end of the call so that by default calls to PRODOS.IO are assumed to be for opening new files, which is the case most of the time. 
;
;*parm.reqcmd: the values expected by ProRWTS2 are
;	([$1=read drive1, | $2=write drive1], [$81=read drive2 | $82=write drive2])
;However, I added the following:
;	[$90=seek current_drive, | $91=read current_drive, | $92=write current_drive]
;	see subroutine documentation for details
;
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
@START
;;COPY ProRWTS TO AUX BSR
;	JSR ProRWTS.AUX_BSR.INIT

;TEMPLATE.WRITE.OPEN_FILE
@START
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
@END

;TEMPLATE.SEEK.CURRENT_FILE
@START
; ;set command type (READ | WRITE | SEEK)
; ; cmd_seek.current_drive 		.EQ $90
; ; cmd_read.current_drive  	.EQ $91
; ; cmd_write.current_drive 	.EQ $92
; ;			
; ; cmd_read.drive1			.EQ $1
; ; cmd_read.drive2		  	.EQ $81
; ; cmd_write.drive1			.EQ $2
; ; cmd_write.drive2			.EQ $82
;
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
@END
	
;TEMPLATE.READ.CURRENT_FILE
@START
; ;set command type (READ | WRITE | SEEK)
; ; cmd_seek.current_drive 	.EQ $90
; ; cmd_read.current_drive  	.EQ $91
; ; cmd_write.current_drive 	.EQ $92
; ;			
; ; cmd_read.drive1			.EQ $1
; ; cmd_read.drive2		  	.EQ $81
; ; cmd_write.drive1			.EQ $2
; ; cmd_write.drive2			.EQ $82
; ;
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
@END

;TEMPLATE.RESET.SEEK
@START
;DESCRIPTION
;this template is more of an example to demostrate how
;to reset the file offset (for seeking) on the current file without
;reopening the file. This is helpful because reopening the file is slow as
;the file location has to be found on the disk.
;
;SUMMARY (of what this template does)
;
; open file / read $1     (JSR $D003)
; seek $5  (JSR $D000)
; read $2  (JSR $D000)
;
;
; ;RESET SEEK
			; STA $c009 ;enable aux zero page if ProRWTS is running in AUX BSR
	; LDA #$FF
	; sta bleftlo
	; sta blefthi
	; lda #$00
	; sta blkofflo
	; sta blkoffhi
	; sta entries
			; STA $c008 ;enable main zero page	
;
; seek $1 (JSR $D000)
; read $2 (JSR $D000)



; ;set command type (READ | WRITE | SEEK)
; ; cmd_seek.current_drive 	.EQ $90
; ; cmd_read.current_drive  	.EQ $91
; ; cmd_write.current_drive 	.EQ $92
; ;			
; ; cmd_read.drive1			.EQ $1
; ; cmd_read.drive2		  	.EQ $81
; ; cmd_write.drive1			.EQ $2
; ; cmd_write.drive2			.EQ $82

    ; lda #cmd_read.drive1
	; sta parm.reqcmd
	
; ;set destination memory address
	; lda #$FF
	; sta parm.ldrlo
	; lda #$8F
	; sta parm.ldrhi

; ;set read length (bytes)
	; lda #$01				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	; sta parm.sizelo	
	; lda #$00				;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	; sta parm.sizehi
	
; ;set filename to read from	
	; lda #DUMMY.FILE	;load LO address
	; sta parm.namlo
	; lda /DUMMY.FILE	;load HO address
	; sta parm.namhi

		; LDA #$00	;PARM: $00=main, $01=aux
	; JSR PRODOS.IO


	
; .LOOP.INIT

	; ; lda #$40
	; ; sta parm.size.counter+$0
	; ; lda #$01
	; ; sta parm.size.counter+$1


	; LDX #$00
	
; ;set destination memory address
	; lda #$00
	; sta parm.ldrlo
	; sta parm.ldrlo.counter
	; lda #$90	
	; sta parm.ldrhi
	; sta parm.ldrhi.counter
	
; .LOOP.SEEK_READ

; ;------SEEK FILE------
; .SEEK.FILE
	; lda #cmd_seek.current_drive
	; sta parm.reqcmd

; ;set seek length (bytes)
	; lda #$05				;seek length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	; sta parm.sizelo	
	; lda #$00				;seek length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	; sta parm.sizehi
	
; ; ;set seek length (bytes)
	; ; lda #$18				;seek length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	; ; sta parm.sizelo	
	; ; lda #$E6				;seek length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	; ; sta parm.sizehi

		; LDA #$00	;PARM: $00=main, $01=aux
	; JSR PRODOS.IO
			
	; ;**FALLS THROUGH**
	
; ;----READ FILE-----
; .READ.FILE
	
	; lda #cmd_read.current_drive
	; sta parm.reqcmd


	
; ;set read length (bytes)
	; lda #$02				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	; sta parm.sizelo	
	; lda #$00	;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	; sta parm.sizehi

	; ;**falls through**

		
; .load.file	
		; LDA #$00	;PARM: $00=main, $01=aux
	; JSR PRODOS.IO		

	
; ; ;INCREMENT DESTINATION ADDRESS
	; LDA parm.ldrlo.counter
	; CLC
	; ADC #$02
	; STA parm.ldrlo.counter
	; sta parm.ldrlo
	; LDA parm.ldrhi.counter
	; ADC #$00 ;16-bit add
	; STA parm.ldrhi.counter
	; sta parm.ldrhi
			
	; INX
	; ;CPX #$6
	; ;CPX #$2
	; CPX #$01
	; BNE .LOOP.SEEK_READ

			
			
; ;RESET SEEK POINTER
			; STA $c009 ;enable aux zero page if ProRWTS is running in AUX BSR

	; LDA #$FF
	; sta bleftlo
	; sta blefthi
	; lda #$00
	; sta blkofflo
	; sta blkoffhi
	; sta entries

			; STA $c008 ;enable main zero page
			
			

			
; ;------SEEK FILE------
; .AFTER_RESET.SEEK.FILE
	; lda #cmd_seek.current_drive
	; sta parm.reqcmd

; ;set seek length (bytes)
	; lda #$01				;seek length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	; sta parm.sizelo	
	; lda #$00				;seek length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	; sta parm.sizehi
	
; ; ;set seek length (bytes)
	; ; lda #$18				;seek length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	; ; sta parm.sizelo	
	; ; lda #$E6				;seek length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	; ; sta parm.sizehi

		; LDA #$00	;PARM: $00=main, $01=aux
	; JSR PRODOS.IO
			
	; ;**FALLS THROUGH**
	
; ;----READ FILE-----
; .AFTER_RESET.READ.FILE
	
	; lda #cmd_read.current_drive
	; sta parm.reqcmd


	
; ;set read length (bytes)
	; lda #$02				;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	; sta parm.sizelo	
	; lda #$00	;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	; sta parm.sizehi

	; ;**falls through**

		
; .AFTER_RESET.load.file	
		; LDA #$00	;PARM: $00=main, $01=aux
	; JSR PRODOS.IO

@END

@END

;WARNING!!!! DO NOT USE TEMP16+$0 in this routine. It is
;used as the load location in a higher level wrapper (PRODOS.IO.OPEN_FILE) that calls
;this routine. 

;SETUP MAIN/AUX MEMORY PARAMETERS
	;ACC contains auxreq
	STA parm.auxreq ; $00=main, $01=aux
	
;SAVE REGISTERS
	PHA
	TXA
	PHA
	TYA
	PHA
	
;GET MAIN BSR MEMORY-STATUS UPON ENTRY
	; LDA $C012		;ROM/BSR soft-switch flag (bit7 = 1: BSR, bit7=0 ROM)
	; STA BSR.STATE
	; LDA $C011		;BANK1/BANK2 soft-switch flag (bit7 = 1: BANK2, bit7=0 BANK1)
	; STA BANK.STATE

	JSR GET.BSR_BANK.STATUS



			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
				; LDA #$B4
			; JSR COUT
; .TEMP
			; LDA TEMP

			
.DEBUG.INVENTORY_DISPLAY_PROBLEM ;**OPT** Memory. Remove when inventory display bug is resolved. 
@START

;check for write command
			LDA parm.reqcmd
			CMP #$2
			BEQ .WRITE.COMMAND
			LDA parm.reqcmd
			CMP #$82
			BEQ .WRITE.COMMAND 
			LDA parm.reqcmd
			CMP #$92			
			BEQ .WRITE.COMMAND
			;not a write command
			JMP .DEBUG.INVENTORY_DISPLAY_PROBLEM.DONE

.WRITE.COMMAND
;is inventory file the filename for the current request? 		
			LDA #DATA.GME.INVENTORY
			STA FILENAME.CHECK_LO
			LDA /DATA.GME.INVENTORY
			STA FILENAME.CHECK_HI
		JSR PRODOS.IO.CHECK_FILE_STATUS
			;RETURN VALUE: ACC ($00 = file is inventory | $01 = file is not inventory)
		CMP #$00 ;does filename = inventory?
		BNE .DEBUG.INVENTORY_DISPLAY_PROBLEM.DONE ;branch if no
.DEBUG.ERROR
;reports that a call was made to PRODOS.IO for the inventory file, which should never happen
;until the inventory module is exited.  

		;permit the error trap to be skipped if 'M' or 'm' is pressed
		JSR KEYIN
		CMP #$CD
		BEQ .DEBUG.INVENTORY_DISPLAY_PROBLEM.DONE
		CMP #$ED
		BEQ .DEBUG.INVENTORY_DISPLAY_PROBLEM.DONE
		
		JSR PREP.BRK
		LDX #$40
		TXS
		LDA #$E1
		LDX #$E1
		LDY #$E1
		BRK
	
.DEBUG.INVENTORY_DISPLAY_PROBLEM.DONE
@END

			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP1
				; LDA #$B5
			; JSR COUT
; .TEMP1
			; LDA TEMP

					
.DISK_CHANGE.ENTRANCE
;INIT VARIABLES
	LDA #$00
	STA IO.ATTEMPTS
			
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

		
;======MACHINE LANGUAGE MONITOR TROUBLESHOOTING==========
;To check the values of the parameters being sent to ProWTS, insert the following
;machine opcodes at the memory address that the "LDA reqcmd" below is located at:
;
;AD 82 C0		LDA $C082 (disable BSR, enable ROM)
;8D 08 C0		STA $C008 (disable AUX zpage/BSR, enable main zpage/BSR)
;00				BRK
;
;The JSR $D003 will be clobbered but that is fine since we're doing a hard break
;The new file open / current file branch is also clobbered so when observing the
;parm.reqcmd value consider the expected outcome of the branch. 
;
;========================================================

		;*****Update 9/5/17; Peters said the write trap has been gone for awhile****
		;*****Update 9/6/17; I tried commenting out the $B8 set below and ProRWTS would then hang on inventory/merchant trans exit, which is the only time a write to 
		;SRTN.INVENTORY occurs. So the version of NOXARCH.SYS I'm running now (20170811) seems to still have the write detect trap in it, which is strange because
		;when I was contacting Peter it was to report that it wasn't working. 
		;DISABLE PRORWTS WRITE-DETECT ERROR TRAP (SRTN.INVENTORY)
		;(inventory file write clobber debugging)
		;
		;lda #$00 ;test error trap
		LDA #$01 ;disable ProRWTS write errror trap for inventory file
		STA $B8

		
		
		
		
			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP2
		; ;restore main memory
		; STA $C008 ;enable main zero-page & main BSR 
			; LDX TEMP	;restore stack pointer to X-REG
			; TXS ;transfer X-REG to stack pointer			
	; JSR RESTORE.BSR_BANK.STATUS
			
			; LDA #$AA
			; JSR PREP.BRK
			; BRK
			
; .TEMP2
			; LDA TEMP
			
			
			
			
			
			
;BRANCH: new file open / current file
	LDA reqcmd
	CMP #cmd_seek.current_drive
	BCS .USE.CURRENT.FILE
	
	;**FALLS THROUGH**
	
.OPEN.NEW.FILE


			
	JSR $d003 ;ProRWTS I/O: entrance for opening new files + read/write
			
			; LDY STATUS
			; STA $C008 ;enable main zero-page & main BSR 
					; LDX TEMP	;restore stack pointer to X-REG
					; TXS ;transfer X-REG to stack pointer
				; LDA $C082 ;ENABLE ROM
					; LDA #$EE
					; ;JSR PREP.BRK
					; BRK
					

	JMP .FILE.OPERATION.COMPLETE
	
.USE.CURRENT.FILE
	;ACC = reqcmd
	
	;convert $90-$91 (values I made up for seek/read/write to current file) to $00-$02 (the actual values the driver expects)
	AND #$6F 	;mask out bit4 and bit7
	STA reqcmd	;save converted reqcmd value. 
	JSR $d000 ;ProRWTS I/O: entrance for operations on files already open (read/write/seek)
	
;======MACHINE LANGUAGE MONITOR TROUBELSHOOTING==========
;To check the value of STATUS ($50) as returned by ProRWTS, insert the following
;machine opcodes at the memory address immediatly after the JSR $D000 above
;
;A6 50			LDX $50 	;(STATUS)
;AD 82 C0		LDA $C082 	;(disable BSR, enable ROM)
;8D 08 C0		STA $C008 	;(disable AUX zpage/BSR, enable main zpage/BSR)
;00				BRK
;
;
;========================================================
	
	
	
	;**FALLS THROUGH**
	
.FILE.OPERATION.COMPLETE	
	LDA status			;load ProRWTS return code
	BEQ .IO.SUCCEEDED	;was file-not-found ($01) reported? If no, then treat IO as successful
	; BNE .COW
	; JMP .IO.SUCCEEDED	;was file-not-found ($01) reported? If no, then treat IO as successful
; .COW

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
				
		;restore main memory
		STA $C008 ;enable main zero-page & main BSR 
			LDX TEMP	;restore stack pointer to X-REG
			TXS ;transfer X-REG to stack pointer

			;JSR PREP.BRK

				
;Print "INSERT DSK" to bottom text window		

		;save bsr_bank state variables
		;(because PRINT.STR also calls GET.BSR_BANK.STATUS and RESTORE.BSR_BANK.STATUS, which sets BSR.STATE and BANK.STATE, which 
		;will clobber the values for the memory status when PRODOS.IO was called)	
		LDA BSR.STATE 		;ROM/BSR soft-switch flag (bit7 = 1: BSR, bit7=0 ROM)
		STA BSR.STATE_SAVED
		LDA BANK.STATE 		;BANK1/BANK2 soft-switch flag (bit7 = 1: BANK2, bit7=0 BANK1)
		STA BANK.STATE_SAVED
		;set parameters
		LDA #$B
		STA HTAB	
		LDA #$17
		STA VTAB
	JSR	UPDATE.CHAR.POS

	;JSR PRODOS.IO.FILE_NOT_FOUND.LOOKUP


			
		LDA #.INSERT_DKS.TEXT			
		STA STRING
		
		LDA /.INSERT_DKS.TEXT
		STA STRING+$1						
	JSR PRINT.STR
		;restore BSR_BANK state variables
		;(see above call to JSR	UPDATE.CHAR.POS for documentation on why this is done here in this way)
		LDA BSR.STATE_SAVED
		STA BSR.STATE 		;ROM/BSR soft-switch flag (bit7 = 1: BSR, bit7=0 ROM)
		LDA BANK.STATE_SAVED
		STA BANK.STATE 		;BANK1/BANK2 soft-switch flag (bit7 = 1: BANK2, bit7=0 BANK1)
	JSR KEYIN ;pause for keypress to give player time to switch disks


			; ;
			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; jsr keyin

		; ; ;GET MAIN BSR MEMORY-STATUS UPON ENTRY
			; ; LDA $C012		;ROM/BSR soft-switch flag (bit7 = 1: BSR, bit7=0 ROM)
			; ; STA BSR.STATE
			; ; LDA $C011		;BANK1/BANK2 soft-switch flag (bit7 = 1: BANK2, bit7=0 BANK1)
			; ; STA BANK.STATE
			
			
			; lda #$aa
			; ldx BSR.STATE
			; ldy BANK.STATE
			; ; ldx #TILE_SWAP.SUN.SET_DAY
			; ; ldy /TILE_SWAP.SUN.SET_DAY
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP
	; ;---------------------
	
	JMP .DISK_CHANGE.ENTRANCE

.INSERT_DKS.TEXT .AZ -/Insert Dsk/			;ASCII text string
	
			;print "NO FILE"
			; LDA #$CE
			; JSR COUT.ADDRESS
			; LDA #$CF
			; JSR COUT.ADDRESS
			; LDA #$A0
			; JSR COUT.ADDRESS
			; LDA #$C6
			; JSR COUT.ADDRESS
			; LDA #$C9
			; JSR COUT.ADDRESS
			; LDA #$CC
			; JSR COUT.ADDRESS
			; LDA #$C5
			; JSR COUT.ADDRESS			
		


.IO.SUCCEEDED

;COPY SOME PARMS FROM AUX ZERO-PAGE TO MAIN MEMORY
;Note: this is so they can be retreived by the calling routine after exit, when main memory zero-page and BSR are enabled. 
	lda bleftlo
	sta parm.bleftlo 
	lda bleftho
	sta parm.bleftho 	
	lda status
	sta parm.status 	

			; LDA $B8
			; STA COW
			
			
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
	

	;---------------------
	
	

	
;RESTORE REGISTERS
	PLA
	TAY
	PLA
	TAX
	PLA

	RTS
@END
@END

PRODOS.IO.READ_CURRENT_FILE ;automatically does a file offset reset (/aka seek reset), that could get determine with an ACC parm
@START
;PARAMETERS: PARM.SEEK_BYTES, PARM.READ_BYTES, parm.ldrlo, parm.ldrhi
;RETURN: memory pointed to by (parm.ldrlo, parm.ldrhi) 
;ENTRANCE: any routine which opens a file. this routine assumes a file is open. 

;**OPT** Speed. Memory. Look for other place in the source code where a seek/read is done and replace with a call to this routine.
;the savings are the two calls to PRODOS.IO and the drive parm set. (14 byte, or 11 bytes considering the JSR to this routine to replace it)


;------RESET FILE-OFFSET?-------
;reset the file offset if the number of seek bytes for the current call
;is less than the number of seek bytes in the last call. Resetting the file offset
;causes ProRWTS to seek from the beginning of the file. 
;
;update 8/29/17; I think the above was an idea Peter suggested but I don' think it's applicable
;to all situations. 
	
	JSR PRODOS.IO.RESET.FILE_OFFSET ;resets file offset so that seeks start at beginning of the file
	
	;**FALLS THROUGH**
	

PRODOS.IO.READ_CURRENT_FILE.NO_RESET			
;------SEEK FILE------
.SEEK.FILE
	lda #cmd_seek.current_drive
	sta parm.reqcmd

;set seek length (bytes)
	lda PARM.SEEK_BYTES+$0
	sta parm.sizelo	
	lda PARM.SEEK_BYTES+$1
	sta parm.sizehi

		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO
			
	;**FALLS THROUGH**

PRODOS.IO.READ_CURRENT_FILE.READ_ONLY
;----READ FILE-----
.READ.FILE
	
	lda #cmd_read.current_drive
	sta parm.reqcmd
		
;set read length (bytes)
	lda PARM.READ_BYTES+$0		;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizelo	
	lda PARM.READ_BYTES+$1		;read length (HO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
	sta parm.sizehi

	;**falls through**

			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; lda PARM.SEEK_BYTES+$0		;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
			; STA $BE00
			; lda PARM.SEEK_BYTES+$1		;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
			; STA $BE01
			; lda PARM.READ_BYTES+$0		;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
			; STA $BE02
			; lda PARM.READ_BYTES+$1		;read length (LO byte). Set to $FFFF to read entire file. It's ok if the read length is > file size
			; STA $BE03
			; lda parm.ldrlo 		
			; STA $BE04
			; lda parm.ldrhi
			; STA $BE05
			
			; LDA #$BC
			; ; LDX PLAYER_MERCH.INVENTORY.DATA.POINTER+$0
			; ; LDY PLAYER_MERCH.INVENTORY.DATA.POINTER+$1
			; LDX #FILE.ITEM_TABLE.RECORD.READ
			; LDY /FILE.ITEM_TABLE.RECORD.READ
			; JSR PREP.BRK
			; BRK	
; .TEMP
			; LDA TEMP
			
						
			
.load.file	
		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO		


			
	RTS

@END
@END

	;**OPT** Memory. Look for other code that does a file open, reading only 1 byte, to prepare for a seek. For example, the spell file has code like this. 
PRODOS.IO.OPEN_FILE ;opens a file so that a seek can be done
@START
;PARAMETERS: parm.namlo, parm.namhi
;ENTRANCE: direct
;RETURN: none

;Note: this routine is intended to provide a generic routine for opening files,
;so that subsequent code can seek to a position in the file before reading or writing.
					
;------OPEN FILE------
.OPEN.FILE
	
	;set command type (READ | WRITE | SEEK)
; cmd_seek.current_drive 	.EQ $90
; cmd_read.current_drive  	.EQ $91
; cmd_write.current_drive 	.EQ $92
;			
; cmd_read.drive1			.EQ $1
; cmd_read.drive2		  	.EQ $81
; cmd_write.drive1			.EQ $2
; cmd_write.drive2			.EQ $82
	LDA #cmd_read.drive2
	sta parm.reqcmd
	
;set read length (bytes)
	LDA #$01
	sta parm.sizelo	
	LDA #$00
	sta parm.sizehi
	
;set destination memory address
	lda #TEMP16
	sta parm.ldrlo
	lda /TEMP16
	sta parm.ldrhi

;set filename to read from
	
	;set by calling routine
	;sta parm.namlo
	;sta parm.namhi


			
		LDA #$00	;PARM: $00=main, $01=aux
	JSR PRODOS.IO

	

			
			
	;reset file offset
	;(the file offset was incremented by reading in the one byte above. Other routines will assume the fileset is set to the beginning of the file)
	JSR PRODOS.IO.RESET.FILE_OFFSET
		;**OPT** Memory. Peter said that reading $00 bytes when opening a file should work. Try it again and if still an issue let him know. 

.EXIT
	
	RTS

@END


PRODOS.IO.RESET.FILE_OFFSET ;**WARNING** assumes ProRWTS2 is running in AUX BSR
@START
;PARAMETERS: NONE
;ENTRANCE: after a read or before a seek
;RETURN: reset file offset for ProRWTS2

;Notes: call this subroutine so that seek will start
;from byte $00 in the file. It is a lot faster to do this than
;open the file again. 
	
	STA $c009 ;enable aux zero page if ProRWTS is running in AUX BSR

	LDA #$FF
	sta bleftlo
	sta blefthi
	lda #$00
	sta blkofflo
	sta blkoffhi
	sta entries

	STA $c008 ;enable main zero page		

	
	RTS

@END

PRODOS.IO.CHECK_FILE_STATUS
@START
;PARAMETERS: PARM.NAMLO, PARM.NAMHI, FILENAME.CHECK_LO, FILENAME.CHECK_HI
;ENTRANCE: direct
;RETURN: ACC ($00 = file open | $01 = file closed)
;
;PARM.NAMLO/HI: doesn't need to be set by the calling routine. This parm is set in the last PRODOS.IO call. This parm is the file currently open by ProRWTS
;FILENAME.CHECK_LO/HI: needs to be set by the calling routine. This is a pointer to the filename whose status this routine will check by comparing it to PARM.NAMLO/HI

;==TEMPLATE==
;
;;---CHECK FILE STATUS
;
;		LDA #DATA.GME.INVENTORY
;		STA FILENAME.CHECK_LO
;		LDA /DATA.GME.INVENTORY
;		STA FILENAME.CHECK_HI
;	JSR PRODOS.IO.CHECK_FILE_STATUS
;		;RETURN VALUE: ACC ($00 = file open | $01 = file closed)
;
;;---PROCESS FILE STATUS CODE
; .IS.FILE.OPEN
		; LDA #DATA.GME.INVENTORY
		; STA FILENAME.CHECK_LO
		; LDA /DATA.GME.INVENTORY
		; STA FILENAME.CHECK_HI
	; JSR PRODOS.IO.CHECK_FILE_STATUS
		; ;RETURN VALUE: ACC ($00 = file open | $01 = file closed)
	; CMP #$00 ;is file open?
	; BEQ .FILE_OPEN ;branch if yes
	;
	; ;**FALLS THROUGH**
	;
; .FILE_NOT_OPEN	
	; JSR FILE.OPEN.INVENTORY ;***WARNING*** REPLACE WITH A FILE OPEN ROUTINE FOR THE FILE THAT NEEDS TO BE OPENED
;
	; ;**FALLS THROUGH**
;	
; .FILE_OPEN


;SAVE_REGISTERS
	TYA
	PHA


	;set a zpage variable with the last filename used by PRODOS.IO
	;(we can't just use namlo/hi because the value of those zpage locations changes when ProRWTS returns control)
	LDA PARM.NAMLO
	STA namlo ;using namlo because it's convenient. It's only used inside of ProRWTS.
	LDA PARM.NAMHI
	STA namhi
	
	LDY #$00
.IS.FILE.OPEN
;compare ProRWTS filename pointer to the filename
;we want to test for open status
	
	LDA (namlo),Y ;namlo and namhi are the zpage pointers used by ProRWTS for the filename. parm.namlo and parm.namhi are regular variables used by the PRODOS.IO wrappet to set namlo/hi
	BEQ .FILE_POSSIBLY_OPEN ;if the ASCII string stop value is reached ($00) then the filenames match
	CMP (FILENAME.CHECK_LO),Y
	BNE .FILE_NOT_OPEN	
	INY
	JMP .IS.FILE.OPEN

.FILE_POSSIBLY_OPEN
;verify that ASCII stop value ($00) has also been reached in the test filename or otherwise a false open could be reported
;if the two files are not the same but the test filename is longer and matches the ProRWTS filename up to it's last character.
	LDA (FILENAME.CHECK_LO),Y
	BEQ	.FILE_OPEN
	
	;**FALLS THROUGH**

.FILE_NOT_OPEN
	LDA #$01	;set return value: file not open ($00 = file open | $01 = file closed)
	JMP .EXIT
	
.FILE_OPEN
	LDA #$00	;set return value: file open ($00 = file open | $01 = file closed)
	

			; STA TEMP
			; ;LDA TROUBLESHOOTING.HOOK
			; LDA FILE.OPEN.INVENTORY.DEBUG_COUNTER
			; CMP #$01
			; BNE .TEMP
			; LDA #$CC
			; LDX TEMP
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP
			
.EXIT
	;ACC = RETURN VALUE ($00 = file open | $01 = file closed)
	STA TEMP ;save return value 

;RESTORE_REGISTERS
	PLA
	TAY
	
	LDA TEMP ;LOAD RETURN VALUE ($00 = file open | $01 = file closed)

	RTS
@END


PRODOS.IO.FILE_NOT_FOUND.LOOKUP
@START	
; ;TROUBLESHOOTING HOOK: FILE NOT FOUND
; ;(uncomment this code if ProRWTS hangs at the insert disk prompt and
; ;lookup the memory address reported in X-REG/YREG to see what the filename specified was. 
; ;do this lookup in the .LST file)
; ;
; ;!!!WARNING: when done, make sure to comment out the JSR PRODOS.IO.FILE_NOT_FOUND.LOOKUP
; ;in the code above after commenting out this routine. 

 
			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; LDA #$AA
			; ldX parm.namlo 		
			; LDY PARM.NAMHI
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP
		
				; lda #$01
				; sta TROUBLESHOOTING.HOOK
			
			; RTS
@END

GET.BSR_BANK.STATUS
@START

;GET MAIN BSR MEMORY-STATUS UPON ENTRY
	LDA $C012		;ROM/BSR soft-switch flag (bit7 = 1: BSR, bit7=0 ROM)
	STA BSR.STATE
	LDA $C011		;BANK1/BANK2 soft-switch flag (bit7 = 1: BANK2, bit7=0 BANK1)
	STA BANK.STATE

	
			
			
			
	RTS
@END

RESTORE.BSR_BANK.STATUS
@START
;RESTORE MEMORY-STATUS
	LDA BSR.STATE	;$C012			;get BSR State (either ROM enabled or BSR enabled) before P.IO driver was called
	BMI .GET.PRIOR.BSR.BANK_STATE	;is bit7 = 1? If yes, BSR was enabled, find out which bank

				
	;STA $C082						;if no, ROM was enabled, so re-enable ROM
	LDA $C082						;if no, ROM was enabled, so re-enable ROM
	;BPL .alldone					;BPL must be true if BMI was false
	JMP .ALLDONE
	
.GET.PRIOR.BSR.BANK_STATE	
	LDA BANK.STATE	;$C011			;get BSR Bank State (either Bank1 or Bank2 enabled) before P.IO driver was called
	BMI .BANK2						;is bit7=1? If yes, bank2 was enabled
	LDA $C08B						;if no, Bank1 was enabled, so re-enable bank1
	LDA $C08B						;""
	;BPL .ALLDONE
	JMP .ALLDONE
.BANK2
	LDA $C083						;re-enable bank2
	LDA $C083
	
	; STA $C083						;re-enable bank2
	; STA $C083
.ALLDONE


			
	RTS
@END
	
	
;======DEFINE VARIBLES======

;**see "GENERAL ROUTINES (LOW LEVEL)" section in offloaded_variables.ASM
 
;======INCLUDE FILES======

;none
	
