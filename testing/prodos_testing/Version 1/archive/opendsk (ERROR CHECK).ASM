;open/read binary file in ProDOS filesystem
;copyright (c) Peter Ferrie 2016

 				.OR		$800			**Always put before .TF directive and never use again in program
				.TF     PRODOS.DRIVER.BIN,BIN
	


;PRODOS DRIVER
		
tmpsec    .EQ $3c         ;constant from boot prom
reqsec    .EQ $3d         ;constant from boot prom
A1L       .EQ $3c         ;constant from ROM
A1H       .EQ $3d         ;constant from ROM
A2L       .EQ $3e         ;constant from ROM
A2H       .EQ $3f         ;constant from ROM
curtrk    .EQ $40         ;constant from boot prom

adrlo     .EQ $44         ;ProDOS constant
adrhi     .EQ $45         ;ProDOS constant
bloklo    .EQ $46         ;ProDOS constant
blokhi    .EQ $47         ;ProDOS constant

secsize   .EQ $46
secsize1  .EQ $47
secsize2  .EQ $48

sizelo    .EQ $f8
sizehi    .EQ $f9
entries   .EQ $fa         ;total number of entries
namlo     .EQ $fb
namhi     .EQ $fc
step      .EQ $fd         ;state for stepper motor
tmptrk    .EQ $fe         ;temporary copy of current track
phase     .EQ $ff         ;current phase for seek
reloc     .EQ $bd00
dirbuf    .EQ $bb00

init
                jsr $fe93
                jsr $fe89
                lda $bf30
                sta unrunit+$1
                and #$70
                sta $2b
                pha
                ora #$80
                sta unrseek+$1
                ora #$8
                sta unrdrvoff+$1
                inc
                sta unrdrvon1+$1
                sta unrdrvon2+$1
                eor #$5
                sta unrread1+$1
                sta unrread2+$1
                sta unrread3+$1
                pla
                lsr
                lsr
                lsr
                lsr
                ora #$c0
                sta slot+$2
                sta unrentry+$2
                ldx /unrelocdsk
                ldy #unrelocdsk

slot
                lda $cfff
                sta unrentry+$1
                php
                beq copydrv
                ldx /unrelochdd
                ldy #unrelochdd

copydrv
                stx A1H
                sty A1L
                inx
                stx A2H
                sty A2L
                ldy #$0
.m1a            lda (A1L), y				;was -
                sta reloc, y
                lda (A2L), y
                sta reloc+$100, y
                iny
                bne .m1a
                plp
                bne .p2
                ldx #$3
.m2	            stx $3c						;was --
                txa
                asl
                bit $3c
                beq .p1
                ora $3c
                eor #$ff
                and #$7e
.m1b            bcs .p1						;was -
                lsr
                bne .m1b
                tya
                sta nibtbl, x
                iny
.p1             inx							;was +
                bpl .m2
.p2             rts							;was ++

unrelocdsk
	.PH reloc ;=====START PATCH=====

                ;turn on drive and read volume directory key block

opendir
unrdrvon1	.EQ	unrelocdsk+*-reloc
                lda $c0e9
                stz adrlo
                stz secsize1
                lda #$2
                ldx #$0
                jsr readdirsec

readdir         ;turn on drive (again) in case of subdirectories

unrdrvon2	.EQ	unrelocdsk+*-reloc
                lda $c0e9

                ;include volume directory header in count

                lda dirbuf+37
                inc
                sta entries
                stz sizehi
firstent        stz sizelo
                lda #dirbuf+$4
                sta A1L
                lda /dirbuf+$4
                sta A1H
nextent         lda (A1L)
                and #$f0

                ;skip deleted entries without counting

                beq savedtype.p3

                ;subdirectory entries are seedlings
                ;but we need to distinguish between them later

                cmp #$d0
                beq savetype

                ;watch for seedling and saplings only

                cmp #$30
                bcs savedtype.p2

                ;remember type

savetype        asl
                asl
                php

                ;match name lengths before attempting to match names

                lda (A1L)
                and #$0f
                cmp (namlo)
                bne savedtype.p1
                tay
savetype.m1     lda (A1L), y				;was -
                cmp (namlo), y
                beq foundname

                ;match failed, check if any directory entries remain

savedtype.p1    plp							;was +
savedtype.p2    inc sizehi					;was ++
                lda sizehi
                cmp entries

                ;lock if entry not found

					;beq *
				BNE .NOBRK				;MGL
				BRK						;MGL
.NOBRK									;MGL

                ;move to next directory in this block, if possible

savedtype.p3    clc							;was +++
                lda A1L
                adc #$27
                sta A1L
                bcc .p1 

                ;there can be only one page crossed, so we can increment instead of adc

                inc A1H
.p1             inc sizelo					;was +
                lda sizelo
                cmp #$0d
                bcc nextent

                ;read next directory block when we reach the end of this block

                lda dirbuf+$2
                ldx dirbuf+$3
                jsr readdirsec
                bra firstent

foundname       dey
                bne savetype.m1

                ;cache EOF (file size)

                ldy #$15
                lda (A1L), y
                sta sizelo
                iny
                lda (A1L), y
                sta sizehi

                ;cache AUX_TYPE (load offset for binary files)

                plx
                ldy #$1f
                lda (A1L), y
                pha
                iny
                lda (A1L), y
                pha
                phx

                ;cache KEY_POINTER (loaded backwards)
                ;and construct single-entry index block in case of seedling

                ldy #$12
                lda (A1L), y
                tax
                sta dirbuf+256
                stz dirbuf+257
                dey
                lda (A1L), y
                sta dirbuf
                stz dirbuf+$1

                ;read index block in case of sapling

                plp
                php
                bpl .p1
                jsr readdirsec

                ;restore load offset

.p1             plp							;was +
                plx
                pla

                ;check file type and fake load address for subdirectories

                bcc readfile.p1
                ldx /dirbuf
.p2             sta adrlo					;was ++
                stx adrhi
                stz namlo

                ;set read size to min(length, $200)

readfile        lda sizelo
                ldx sizehi
                cpx #$2
                bcc readfile.p1
                lda #$0
                ldx #$2
readfile.p1     sta secsize1				;was +
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

                lda sizehi
                dec
                beq readdone

                ;loop while size-$200 is non-zero

                dec
                inc adrhi
                sta sizehi
                ora sizelo
                bne readfile

unrdrvoff	.EQ	unrelocdsk+*-reloc
readdone        lda $c0e8
readret         rts

readdirsec      ldy #$2
                sty secsize2
                ldy /dirbuf
                sty adrhi

                ;convert block number to track/sector

seekread        pha
                and #$7
                cmp #$4
                and #$3
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

                ldy #$0
                lda secsize2
                bne seekread.p1
                ldy secsize1
seekread.p1     sty secsize					;was +
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

cmpsec          jsr readadr
                cmp reqsec
                bne cmpsec

                ;read sector data

readdata
		jsr readd5aa
		eor #$ad		;zero A if match
					;bne *                   ;lock if read failure
				BEQ .NOBRK				;MGL
				BRK						;MGL
.NOBRK									;MGL
unrread1	.EQ	unrelocdsk+*-reloc
.m1a            ldx $c0ec				;was -
                bpl .m1a
                eor nibtbl-$80, x
                sta bit2tbl-$aa, y
                iny
                bne .m1a
unrread2	.EQ	unrelocdsk+*-reloc
.m1b            ldx $c0ec				;was -
                bpl .m1b
                eor nibtbl-$80, x
                sta (adrlo), y          ;the real address
                iny
                cpy secsize
                bne .m1b
                ldy #$0
.m1c            ldx #$a9				;was -
.m2             inx						;was --
                beq .m1c
                lda (adrlo), y
                lsr bit2tbl-$aa, x
                rol
                lsr bit2tbl-$aa, x
                rol
                sta (adrlo), y
                iny
                cpy secsize
                bne .m2
                rts

                ;no tricks here, just the regular stuff

readadr
.m2a	jsr readd5aa					;was --
                cmp #$96
                bne .m2a
                ldy #$3
.m2b            sta curtrk				;was -
		jsr readnib
                rol
                sta tmpsec
		jsr readnib
                and tmpsec
                dey
                bne .m2b
                rts

readd5aa
.m2		jsr readnib					;was --
.m1		cmp #$d5					;was -
		bne .m2
		jsr readnib
		cmp #$aa
		bne .m1
		tay			;we need Y=#$AA later

readnib
unrread3	.EQ	unrelocdsk+*-reloc
.m1		lda $c0ec					;was -
		bpl .m1
seekret		rts			;hard-coded relative to seek

seek            asl curtrk
                stz step
                asl phase
copy_cur        lda curtrk
                sta tmptrk
                sec
                sbc phase
                beq seekret
                bcs .p1a
                eor #$ff
                inc curtrk
                bcc .p2
.p1a            dec					;was +
                dec curtrk
.p2          	cmp step			;was ++
                bcc .p1b
                lda step
.p1b            cmp #$8				;was +
                bcs .p1c
                tay
                sec
.p1c            lda curtrk			;was +
                ldx step1, y
		bne A.p1d
A.m3	clc							;was ---
		lda tmptrk
		ldx step2, y
A.p1d	stx tmpsec					;was +
		and  #$3
		rol
		tax
unrseek	.EQ	unrelocdsk+*-reloc
		sta $c0e0, x
.m2		ldx #$13					;was --
.m1		dex							;was -
		bne .m1
		dec tmpsec
		bne .m2
		lsr
		bcs A.m3
		inc step
		bne copy_cur

;step1           !byte $1, $30, $28, $24, $20, $1e, $1d, $1c
step1		.HS	01.30.28.24.20.1E.1D.1C

;step2           !byte $70, $2c, $26, $22, $1f, $1e, $1d, $1c
step2		.HS 70.2C.26.22.1F.1E.1D.1C

nibtbl          .EQ *
bit2tbl         .EQ nibtbl+128
dataend         .EQ bit2tbl+86
	.EP	;=====END PATCH

unrelochdd
	.PH reloc
		lda	#$1
		sta	$42
unrunit	.EQ	unrelochdd+*-reloc
		lda	#$d1
		sta	$43

                ;turn on drive and read volume table of contents
                stz adrlo
                lda #$2
                ldx #$0
                jsr hddreaddirsec

hddreaddir      ;include volume directory header in count

                lda dirbuf+37
                inc
                sta entries
                stz sizehi
hddfirstent     stz sizelo
                lda #dirbuf+$4
                sta A1L
                lda /dirbuf+$4
                sta A1H
hddnextent      lda (A1L)
                and #$f0

                ;skip deleted entries without counting

                beq B.p3

                ;subdirectory entries are seedlings
                ;but we need to distinguish between them later

                cmp #$d0
                beq hddsavetype

                ;watch for seedling and saplings only

                cmp #$30
                bcs B.p2

                ;remember type

hddsavetype     asl
                asl
                php

                ;match name lengths before attempting to match names

                lda (A1L)
                and #$0f
                cmp (namlo)
                bne B.p1
                tay
B.m1            lda (A1L), y			;was -
                cmp (namlo), y
                beq hddfoundname

                ;match failed, check if any directory entries remain

B.p1            plp						;was +
B.p2            inc sizehi				;was ++
                lda sizehi
                cmp entries

                ;lock if entry not found

					;beq *
				BNE .NOBRK				;MGL
				BRK						;MGL
.NOBRK									;MGL

                ;move to next directory in this block, if possible

B.p3            clc						;was +++
                lda A1L
                adc #$27
                sta A1L
                bcc .p1b

                ;there can be only one page crossed, so we can increment instead of adc

                inc A1H
.p1b            inc sizelo				;was +
                lda sizelo
                cmp #$0d
                bcc hddnextent

                ;read next directory block when we reach the end of this block

                lda dirbuf+$2
                ldx dirbuf+$3
                jsr hddreaddirsec
                bra hddfirstent

hddfoundname    dey
                bne B.m1

                ;cache EOF (file size)

                ldy #$15
                lda (A1L), y
                sta sizelo
                iny
                lda (A1L), y
                sta sizehi

                ;cache AUX_TYPE (load offset for binary files)

                plx
                ldy #$1f
                lda (A1L), y
                pha
                iny
                lda (A1L), y
                pha
                phx

                ;cache KEY_POINTER (loaded backwards)
                ;and construct single-entry index block in case of seedling

                ldy #$12
                lda (A1L), y
                tax
                sta dirbuf+256
                stz dirbuf+257
                dey
                lda (A1L), y
                sta dirbuf
                stz dirbuf+1

                ;read index block in case of sapling

                plp
                php
                bpl .p1a
                jsr hddreaddirsec

                ;restore load offset

.p1a            plp						;was +
                plx
                pla

                ;check file type and fake load address for subdirectories

                bcc .p1b
                ldx /dirbuf
.p1b            sta adrlo				;was +
                stx adrhi
                stz namlo

                ;set read size to min(length, $200)

hddreadfile     lda sizehi
                cmp #$2
                bcs .p1c
		pha
		lda #$2
		sta sizehi
		lda adrhi
		pha
		lda adrlo
		pha
                lda /dirbuf
                sta adrhi
                stz adrlo
.p1c	php								;was +

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
		bcc .p1d
		lda sizelo
		bne hddreadfile
		rts

.p1d	pla						;was +
		sta A1L
		pla
		sta A1H
		dec adrhi
		dec adrhi
		ply
		beq .p1e
		ldy #$0
.m1a	lda (adrlo), y			;was -
		sta (A1L), y
		iny
		bne .m1a
		inc adrhi
		inc A1H
.p1e							;was +
.m1b	lda (adrlo), y			;was -
		sta (A1L), y
		iny
		cpy sizelo
		bne .m1b
		rts

hddreaddirsec   ldy /dirbuf
                sty adrhi

hddseekread     sta bloklo
                stx blokhi

unrentry	.EQ	unrelochdd+*-reloc
                jmp $d1d1
	.EP ;=========END PATCH=====
