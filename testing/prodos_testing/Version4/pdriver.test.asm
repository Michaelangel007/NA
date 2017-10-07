;open/read/write binary file in ProDOS filesystem
;copyright (c) Peter Ferrie 2013-16
;Used with permission. 
				
				.OR		$800			**Always put before .TF directive and never use again in program
				.TF     PRODOS.DRIVER.BIN,BIN

;=====================SUBROUTINE DOCUMENTATION====================================
;
;error check ideas
;pages to write specified not a multiple of 2 or less than 2
;command not 1 or 2
;
;
;====OVERVIEW=======
;
;PRODOS_DRIVER.ASM contains read/write file access routines for
;ProDOS, which are intended to enable a program to clobber 
;the main ProDOS operating system in memory while retaining
;file access via the ProDOS file structure. The results is less
;memory used than a classic bare metal bootloader + DOS 3.3 RWTS
;setup. This ProDOS driver is also 26% faster at disk reads than
;DOS 3.3 RWTS. No write speed test has been performed. 
;
;This subroutine is designed to be useds as read only or read & writem
;which resutled in the author including a lot of conditional assembler
;logic to reduce the size of the code generated if the programmer 
;flips the switches in the source code to use the routine as read only.
;
;For details on my discussion with the author on the routines
;capabilities, see the email "filesystem code" dated 5/1/2016
;with Peter Ferrie.
;
;===MINUMUM READ/WRITE SIZE
;HardDrive: 512 bytes (1 ProDOS block, 2 pages)
;Floppy: 256 bytes (1/2 ProDOS block, 1 page)
;
;===MEMORY USAGE====
;
;The code uses ? byte of memory starting at $800 when 
;PRODOS.DRIVER.BIN Is loaded into memory.
;
;
;After a JSR PDRIVER.INIT is executed the driver builds itself
;in upper main memory, in the following ranges:
;
;$BC00-BFFF permanently.  Don't clobber that.
;$BA00-BBFF is used only during a file-system request, and free for all
;other times.
;$B900-B9FF is used only during a write request, and free for all other times.
;
;
;====INFORMATION ON VARIABLES=====
; 	sizelo/sizehi	;must set if writing
; 	reqcmd 		;must set if enable_write=1, 1=read, 2=write
; 	ldrlo/ldrhi	;must set if override_adr=1
; 	namlo/namhi 	;filename to use
;
;
;=================================================================================



tmpsec    = $3c
reqsec    = $3d
A1L       = $3c
A1H       = $3d
A2L       = $3e
A2H       = $3f
A3L       = $40
A3H       = $41
curtrk    = $40

command   = $42         ;ProDOS constant
unit      = $43         ;ProDOS constant
adrlo     = $44         ;ProDOS constant
adrhi     = $45         ;ProDOS constant
bloklo    = $46         ;ProDOS constant
blokhi    = $47         ;ProDOS constant

secsize   = $46
secsize1  = $47
secsize2  = $48

auxreq    = $f4         ;set to 1 to read/write aux memory
sizelo    = $f5         ;must set if writing
sizehi    = $f6         ;must set if writing
entries   = $f7         ;total number of entries
reqcmd    = $f8         ;used if enable_write=1, 1=read, 2=write
ldrlo     = $f9         ;used if override_adr=1
ldrhi     = $fa         ;used if override_adr=1
namlo     = $fb
namhi     = $fc
step      = $fd         ;state for stepper motor
tmptrk    = $fe         ;temporary copy of current track
phase     = $ff         ;current phase for seek
reloc     = $d000
dirbuf    = $d400
encbuf    = $d600

pdriver.init    jsr $fe93
                jsr $fe89
                lda $bf30
                sta x80_parms+1
                sta unrunit+1
                and #$70
                pha
                ora #$80
                sta unrseek+1
                ora #8
                sta unrdrvoff+1
                tax
                inx
                stx unrdrvon+1
                eor #4
                sta unrread1+1
                sta unrread2+1
                sta unrread3+1
                sta unrread4+1
                sta unrread5+1
                sta unrread6+1
                sta unrread7+1
                sta unrread8+1
                tax
                inx
                stx unrlatch1+1
                stx unrlatch2+1
                stx unrlatch3+1
                inx
                stx unrlatchin+1
                inx
                stx unrlatchout+1
                ldx #1
                stx namlo
                inx
                stx namhi

                jsr $bf00
                ; !byte $c7
                ; !word c7_parms
				.DA #$c7
				.DA c7_parms
                ldx $200
                dex
                stx sizelo
                bmi plus05

readblock       jsr $bf00
                .DA #$80
                .DA x80_parms

                sta A2L
                lda #readbuff+4
                sta A1L
                lda /readbuff+4
                sta A1H
inextent        ldy #0
                lda (A1L), y
                sta sizehi
                and #$d0

                ;watch for subdirectory entries

                cmp #$d0
                bne plus01

                lda (A1L), y
                and #$0f
                tax
                iny
minus01         lda (namlo), y
                cmp (A1L), y
                beq ifoundname

                ;match failed, move to next directory in this block, if possible

minus02
plus01          clc
                lda A1L
                adc #$27
                sta A1L
                bcc plus02

                ;there can be only one page crossed, so we can increment instead of adc

                inc A1H
plus02          inc A2L
                lda A2L
                cmp #$0d
                bcc inextent

                ;read next directory block when we reach the end of this block

                ldy readbuff+2
                ldx readbuff+3
                bcs plus03

ifoundname      iny
                dex
                bne minus01
                lda (namlo), y
                cmp #'/'
                bne minus02
                tya
                eor #$ff
                adc sizelo
                sta sizelo
                clc
                tya
                adc namlo
                sta namlo
                lda sizehi
                and #$20
                bne plus04
                ldy #$12
                lda (A1L), y
                tax
                dey
                lda (A1L), y
                tay
                sty unrblocklo+1
                stx unrblockhi+1
                sty unrhddblocklo+1
                stx unrhddblockhi+1
plus03          sty x80_parms+4
                stx x80_parms+5
plus04          lda sizelo
                bne readblock

plus05          pla
                lsr
                lsr
                lsr
                lsr
                ora #$c0
                sta slot+2
                sta unrentry+2
                ldx /unrelocdsk
                ldy #unrelocdsk
slot            lda $cfff
                sta unrentry+1
                php
                beq copydrv
                ldx /unrelochdd
                ldy #unrelochdd

copydrv         stx A1H
                sty A1L
                inx
                stx A2H
                sty A2L
                inx
                stx A3H
                sty A3L
                lda $c089
                lda $c089
                ldy #0
minus03         lda (A1L), y
                sta reloc, y
                lda (A2L), y
                sta reloc+$100, y
                lda (A3L), y
                sta reloc+$200, y
                iny
                bne minus03
                plp
                bne plus07
                ldx #3
minus04         stx $3c
                txa
                asl
                bit $3c
                beq plus06
                ora $3c
                eor #$ff
                and #$7e
minus05         bcs plus06
                lsr
                bne minus05
                tya
                sta nibtbl, x
                txa
                ora #$80
                sta xlattbl, y
                iny
plus06          inx
                bpl minus04
plus07          rts

c7_parms        .DA #$1
                .DA $200

x80_parms       .DA #$3, #$d1
				.DA readbuff, $2

unrelocdsk
	.PH reloc ;====START PATCH===

opendir         ;read volume directory key block
                ldx #1
                stx command
                dex
                stx adrlo
                stx secsize1
unrblocklo 	.EQ	unrelocdsk+*-reloc
                lda #2
unrblockhi	.EQ	unrelocdsk+*-reloc
                ldx #0
                jsr readdirsec

                ;include volume directory header in count

readdir         ldy dirbuf+37
                iny
                sty entries

                lda #0
                sta A2H
firstent        lda #0
                sta A2L
                lda #dirbuf+4
                sta A1L
                lda /dirbuf+4
                sta A1H
nextent         ldy #0
                lda (A1L), y
                and #$f0

                ;skip deleted entries without counting

                beq plus10

                ;watch for seedling and saplings only

                cmp #$30
                bcs plus09

                ;remember type

savetype
                cmp #$20
                php

                ;match name lengths before attempting to match names

                lda (A1L), y
                and #$0f
                cmp (namlo), y
                bne plus08
                tax
                iny
minus06         lda (A1L), y
                cmp (namlo), y
                beq foundname

                ;match failed, check if any directory entries remain

plus08          plp
plus09          inc A2H
                lda A2H
                cmp entries

                ;lock if entry not found

                beq *

                ;move to next directory in this block, if possible

plus10          clc
                lda A1L
                adc #$27
                sta A1L
                bcc plus11

                ;there can be only one page crossed, so we can increment instead of adc

                inc A1H
plus11          inc A2L
                lda A2L
                cmp #$0d
                bcc nextent

                ;read next directory block when we reach the end of this block

                lda dirbuf+2
                ldx dirbuf+3
                jsr readdirsec
                beq firstent

foundname       iny
                dex
                bne minus06

                ldy reqcmd
                dey
                beq plus13 

                ;round requested size up to nearest sector
                ;and cache requested size if writing

                ldx sizehi
                beq plus12 
                lda sizelo
                beq plus13 
plus12          inx
plus13 

                ;cache EOF (file size)

                ldy #$15
                lda (A1L), y
                sta sizelo
                iny
                lda (A1L), y
                sta sizehi

                ldy reqcmd
                dey
                beq plus16 

                ;round file size up to nearest sector
                ;and check against requested size if writing

                lda sizehi
                beq plus14 
                lda sizelo
                beq plus15 
plus14          lda #0
                sta sizelo
                inc sizehi

                ;set read size to min(length, requested size)

plus15          cpx sizehi
                bcs plus16 
                stx sizehi
plus16 

                ;cache KEY_POINTER (loaded backwards)
                ;and construct single-entry index block in case of seedling

                ldy #$12
                lda (A1L), y
                tax
                sta dirbuf+256
                dey
                lda (A1L), y
                sta dirbuf
                ldy #0
                sty dirbuf+257
                sty dirbuf+1

                ;read index block in case of sapling

                plp
                bcc plus17
                jsr readdirsec

                ;restore load offset

plus17
                ldx ldrhi
                lda ldrlo
                ldy reqcmd
                sty command
                sta adrlo
                stx adrhi
                lda #0
                sta namlo

                ldx auxreq
                sta $c002, x
                sta $c004, x

                ;set read size to min(length, $200)

readfile        lda sizelo
                ldx sizehi
                cpx #2
                bcc plus19
                lda #0
                ldx #2
plus19          sta secsize1
                stx secsize2

                ;fetch data block and read it

                ldy namlo
                inc namlo
                lda dirbuf, y
                ldx dirbuf+256, y
                jsr seekread

                ;if low count is non-zero then we are done
                ;(can happen only for partial last block)

                lda secsize1
                bne readdone

                ;if count is $1xx then we are done
                ;(can happen only for partial last block)

                ldx sizehi
                dex
                beq readdone

                ;loop while size-$200 is non-zero

                dex
                inc adrhi
                stx sizehi
                txa
                ora sizelo
                bne readfile

unrdrvoff	.EQ	unrelocdsk+*-reloc
readdone        lda $c0e8
                sta $c002
                sta $c004
readret         rts

readdirsec
unrdrvon	.EQ	unrelocdsk+*-reloc
                ldy $c0e9
                ldy #2
                sty secsize2
                ldy /dirbuf
                sty adrhi

                ;convert block number to track/sector

seekread        pha
                and #7
                cmp #4
                and #3
                php
                asl
                plp
                rol
                sta reqsec
                txa
                lsr
                pla
                ror
                lsr
                lsr
                sta phase

                ;set read size to min(first size, $100) and then read address

                ldy #0
                lda secsize2
                bne plus20
                ldy secsize1
plus20          sty secsize
                dec secsize2
                jsr readadr

                ;if track does not match, then seek

                lda curtrk
                cmp phase
                beq checksec
                jsr seek

                ;[re-]read sector

checksec        jsr cmpsec

                ;return if less than one sector requested

                tya
                bne readret

                ;return if only one sector requested

                lda secsize1
                cmp secsize2
                beq readret
                sta secsize
                inc adrhi
                inc reqsec
                inc reqsec

cmpsec          ldy command
                dey
                bne encsec
cmpsecrd        jsr readadr
                cmp reqsec
                bne cmpsecrd

                ;read sector data

readdata        jsr readd5aa
                eor #$ad                ;zero A if match
                bne *                   ;lock if read failure
unrread1	.EQ	unrelocdsk+*-reloc
minus07         ldx $c0ec
                bpl minus07
                eor nibtbl-$80, x
                sta bit2tbl-$aa, y
                iny
                bne minus07
unrread2	.EQ unrelocdsk+*-reloc
minus08         ldx $c0ec
                bpl minus08
                eor nibtbl-$80, x
                sta (adrlo), y          ;the real address
                iny
                cpy secsize
                bne minus08
                ldy #0
minus09         ldx #$a9
minus10         inx
                beq minus09
                lda (adrlo), y
                lsr bit2tbl-$aa, x
                rol
                lsr bit2tbl-$aa, x
                rol
                sta (adrlo), y
                iny
                cpy secsize
                bne minus10
                rts

encsec          iny
minus11         ldx #0
minus12         dey
                lda (adrlo), y
                lsr
                rol bit2tbl, x
                lsr
                rol bit2tbl, x
                sta encbuf, y
                inx
                cpx #$56
                bcc minus12
                tya
                bne minus11
minus13         lda bit2tbl-1, x
                and #$3f
                sta bit2tbl-1, x
                dex
                bne minus13

cmpsecwr        jsr readadr
                cmp reqsec
                bne cmpsecwr
minus14         jsr readnib
                cmp #$de
                bne minus14
                jsr readnib
                cmp #$aa
minus15         bne minus15

                ;write sector data

                lda #$ff
unrlatchout	.EQ unrelocdsk+*-reloc
                sta $c0ef
unrread4	.EQ unrelocdsk+*-reloc
                ora $c0ec
                pha
                pla
                nop
                ldy #4
minus16         pha
                pla
                jsr writenib2
                dey
                bne minus16
                lda #$d5
                jsr writenib1
                lda #$aa
                jsr writenib1
                lda #$ad
                jsr writenib1
                tya
                ldy #$56
minus17         eor bit2tbl-1, y
                tax
                lda xlattbl, x
unrlatch1	.EQ	unrelocdsk+*-reloc
                sta $c0ed
unrread5	.EQ unrelocdsk+*-reloc
                lda $c0ec
                lda bit2tbl-1, y
                dey
                bne minus17
minus18         eor encbuf, y
                tax
                lda xlattbl, x
unrlatch2	.EQ unrelocdsk+*-reloc
                sta $c0ed
unrread6	.EQ	unrelocdsk+*-reloc
                lda $c0ec
                lda encbuf, y
                iny
                bne minus18
                tax
                lda xlattbl, x
                jsr writenib3
                lda #$de
                jsr writenib1
                lda #$aa
                jsr writenib1
                lda #$eb
                jsr writenib1
                lda #$ff
                jsr writenib1
unrlatchin	.EQ unrelocdsk+*-reloc
                lda $c0ee
unrread7	.EQ unrelocdsk+*-reloc
                lda $c0ec
                rts

writenib1       clc
writenib2       pha
                pla
writenib3
unrlatch3	.EQ unrelocdsk+*-reloc
                sta $c0ed
unrread8	.EQ unrelocdsk+*-reloc
                ora $c0ec
                rts

                ;no tricks here, just the regular stuff

readadr
minus19         jsr readd5aa
                cmp #$96
                bne minus19
                ldy #3
minus20         sta curtrk
                jsr readnib
                rol
                sta tmpsec
                jsr readnib
                and tmpsec
                dey
                bne minus20
                rts

readd5aa
minus21         jsr readnib
minus22         cmp #$d5
                bne minus21
                jsr readnib
                cmp #$aa
                bne minus22
                tay                    ;we need Y=#$AA later

readnib
unrread3	.EQ unrelocdsk+*-reloc
minus23         lda $c0ec
                bpl minus23
seekret         rts

seek            asl curtrk
                lda #0
                sta step
                asl phase
copy_cur        lda curtrk
                sta tmptrk
                sec
                sbc phase
                beq seekret
                bcs plus21
                eor #$ff
                inc curtrk
                bcc plus22
plus21          sbc #1
                dec curtrk
plus22          cmp step
                bcc plus23
                lda step
plus23          cmp #8
                bcs plus24
                tay
                sec
plus24          lda curtrk
                ldx step1, y
                bne plus25
minus24         clc
                lda tmptrk
                ldx step2, y
plus25          stx tmpsec
                and #3
                rol
                tax
unrseek	.EQ unrelocdsk+*-reloc
                sta $c0e0, x
minus25         ldx #$13
minus26         dex
                bne minus26
                dec tmpsec
                bne minus25
                lsr
                bcs minus24
                inc step
                bne copy_cur

;step1           !byte 1, $30, $28, $24, $20, $1e, $1d, $1c
step1			.HS	01.30.28.24.20.1E.1D.1C

;step2           !byte $70, $2c, $26, $22, $1f, $1e, $1d, $1c
step2			.HS 70.2C.26.22.1F.1E.1D.1C


nibtbl          = *
bit2tbl         = nibtbl+128
xlattbl         = bit2tbl+86
dataend         = xlattbl+64
		; ;Peter suggested adding this error check
		; .DO dataend>=$C000
			; .ER   F,***Custom Error: dataend>= #$C000
		; .FI
		
	.EP ;==END PATCH==

unrelochdd
 .PH reloc ;====START PATCH===
                ;read volume directory key block

hddopendir      lda #0
                sta adrlo
unrhddblocklo	.EQ unrelochdd+*-reloc
                lda #2
unrhddblockhi	.EQ unrelochdd+*-reloc
                ldx #0
                jsr hddreaddirsec

; !if (*-hddopendir) < (readdir-opendir) {
                ; ;essential padding to match offset with floppy version
; !fill (readdir-opendir)-(*-hddopendir), $ea
; }

value1	.EQ	*-hddopendir	
value2	.EQ readdir-opendir

	.DO value1 < value2
                ;essential padding to match offset with floppy version
		.BS readdir-opendir-*+hddopendir, $ea
	.FI ;finished conditional 
	
	
                ;include volume directory header in count

hddreaddir      ldy dirbuf+37
                iny
                sty entries

                lda #0
                sta A2H
hddfirstent     lda #0
                sta A2L
                lda #dirbuf+4
                sta A1L
                lda /dirbuf+4
                sta A1H
hddnextent      ldy #0
                lda (A1L), y
                and #$f0

                ;skip deleted entries without counting

                beq plus28

                ;watch for seedling and saplings only

                cmp #$30
                bcs plus27

                ;remember type

hddsavetype
                cmp #$20
                php

                ;match name lengths before attempting to match names

                lda (A1L), y
                and #$0f
                cmp (namlo), y
                bne plus26
                tax
                iny
minus27         lda (A1L), y
                cmp (namlo), y
                beq hddfoundname

                ;match failed, check if any directory entries remain

plus26          plp
plus27          inc A2H
                lda A2H
                cmp entries

                ;lock if entry not found

                beq *

                ;move to next directory in this block, if possible

plus28          clc
                lda A1L
                adc #$27
                sta A1L
                bcc plus29

                ;there can be only one page crossed, so we can increment instead of adc

                inc A1H
plus29          inc A2L
                lda A2L
                cmp #$0d
                bcc hddnextent

                ;read next directory block when we reach the end of this block

                lda dirbuf+2
                ldx dirbuf+3
                jsr hddreaddirsec
                bcc hddfirstent

hddfoundname    iny
                dex
                bne minus27

                ldy reqcmd
                dey
                beq plus32

                ;round requested size up to nearest block
                ;and cache requested size if writing

                lda sizehi
                tax
                lsr
                bcc plus30
                inx
plus30          cpx #2
                bcc plus31
                lda sizelo
                beq plus32
plus31          ldx #2
plus32

                ;cache EOF (file size)

                ldy #$15
                lda (A1L), y
                sta sizelo
                iny
                lda (A1L), y
                sta sizehi

                ldy reqcmd
                dey
                beq plus36

                ;round file size up to nearest block
                ;and check against requested size if writing

                lda sizehi
                tay
                lsr
                bcc plus33
                iny
plus33          cpy #2
                bcc plus34
                lda sizelo
                beq plus35
plus34          lda #0
                sta sizelo
                ldy #2
plus35          sty sizehi

                ;set read size to min(length, requested size)

                cpx sizehi
                bcs plus36
                stx sizehi
plus36

                ;cache KEY_POINTER (loaded backwards)
                ;and construct single-entry index block in case of seedling

                ldy #$12
                lda (A1L), y
                tax
                sta dirbuf+256
                dey
                lda (A1L), y
                sta dirbuf
                ldy #0
                sty dirbuf+257
                sty dirbuf+1

                ;read index block in case of sapling

                plp
                bcc plus37
                jsr hddreaddirsec

                ;restore load offset

plus37
                ldx ldrhi
                lda ldrlo
                sta adrlo
                stx adrhi
                lda #0
                sta namlo

                ldx auxreq
                sta $c002, x
                sta $c004, x

                ;set read size to min(length, $200)

hddreadfile
                ldy reqcmd
                sty command
                lda sizehi
                cmp #2
                bcs plus39
                pha
                lda #2
                sta sizehi
                lda adrhi
                pha
                lda adrlo
                pha
                lda /dirbuf
                sta adrhi
                lda #0
                sta adrlo
plus39          php

                ;fetch data block and read it

                ldy namlo
                inc namlo
                lda dirbuf, y
                ldx dirbuf+256, y
                jsr hddseekread

                plp
                inc adrhi
                inc adrhi
                dec sizehi
                dec sizehi
                bne hddreadfile
                bcc plus40
                lda sizelo
                bne hddreadfile
hddreaddone     sta $c002
                sta $c004
                rts

plus40          pla
                sta A1L
                pla
                sta A1H
                dec adrhi
                dec adrhi
                pla
                tay
                beq plus41
                dey
minus28         lda (adrlo), y
                sta (A1L), y
                iny
                bne minus28
                inc adrhi
                inc A1H
plus41
minus29         lda (adrlo), y
                sta (A1L), y
                iny
                cpy sizelo
                bne minus29
                beq hddreaddone

hddreaddirsec   ldy #1
                sty command
                ldy /dirbuf
                sty adrhi

hddseekread     sta bloklo
                stx blokhi

unrunit		.EQ	unrelochdd+*-reloc
                lda #$d1
                sta unit

unrentry	.EQ	unrelochdd+*-reloc
                jmp $d1d1
	.EP ;END PATCH
readbuff
;!byte $D3,$C1,$CE,$A0,$C9,$CE,$C3,$AE
Peters.signature .HS D3.C1.CE.A0.C9.CE.C3.AE
