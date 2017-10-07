;open/read binary file in ProDOS filesystem
;copyright (c) Peter Ferrie 2016
!cpu 65c02
!to "opendsk",plain
*=$800







                tmpsec    = $3c         ;constant from boot prom
                reqsec    = $3d         ;constant from boot prom
                A1L       = $3c         ;constant from ROM
                A1H       = $3d         ;constant from ROM
                A2L       = $3e         ;constant from ROM
                A2H       = $3f         ;constant from ROM
                curtrk    = $40         ;constant from boot prom

		adrlo     = $44         ;ProDOS constant
		adrhi     = $45         ;ProDOS constant
		bloklo    = $46         ;ProDOS constant
		blokhi    = $47         ;ProDOS constant

                secsize   = $46
                secsize1  = $47
                secsize2  = $48

                sizelo    = $f8
                sizehi    = $f9
                entries   = $fa         ;total number of entries
                namlo     = $fb
                namhi     = $fc
                step      = $fd         ;state for stepper motor
                tmptrk    = $fe         ;temporary copy of current track
                phase     = $ff         ;current phase for seek
                reloc     = $bd00
                dirbuf    = $bb00

init
                jsr $fe93
                jsr $fe89
                lda $bf30
                sta unrunit+1
                and #$70
                sta $2b
                pha
                ora #$80
                sta unrseek+1
                ora #8
                sta unrdrvoff+1
                inc
                sta unrdrvon1+1
                sta unrdrvon2+1
                eor #5
                sta unrread1+1
                sta unrread2+1
                sta unrread3+1
                pla
                lsr
                lsr
                lsr
                lsr
                ora #$c0
                sta slot+2
                sta unrentry+2
                ldx #>unrelocdsk
                ldy #<unrelocdsk

slot
                lda $cfff
                sta unrentry+1
                php
                beq copydrv
                ldx #>unrelochdd
                ldy #<unrelochdd

copydrv
                stx A1H
                sty A1L
                inx
                stx A2H
                sty A2L
                ldy #0
-               lda (A1L), y
                sta reloc, y
                lda (A2L), y
                sta reloc+$100, y
                iny
                bne -
                plp
                bne ++
                ldx #3
--              stx $3c
                txa
                asl
                bit $3c
                beq +
                ora $3c
                eor #$ff
                and #$7e
-               bcs +
                lsr
                bne -
                tya
                sta nibtbl, x
                iny
+               inx
                bpl --
++              rts

unrelocdsk
!pseudopc reloc {

                ;turn on drive and read volume directory key block

opendir
unrdrvon1=unrelocdsk+(*-reloc)
                lda $c0e9
                stz adrlo
                stz secsize1
                lda #2
                ldx #0
                jsr readdirsec

readdir         ;turn on drive (again) in case of subdirectories

unrdrvon2=unrelocdsk+(*-reloc)
                lda $c0e9

                ;include volume directory header in count

                lda dirbuf+37
                inc
                sta entries
                stz sizehi
firstent        stz sizelo
                lda #<(dirbuf+4)
                sta A1L
                lda #>(dirbuf+4)
                sta A1H
nextent         lda (A1L)
                and #$f0

                ;skip deleted entries without counting

                beq +++

                ;subdirectory entries are seedlings
                ;but we need to distinguish between them later

                cmp #$d0
                beq savetype

                ;watch for seedling and saplings only

                cmp #$30
                bcs ++

                ;remember type

savetype        asl
                asl
                php

                ;match name lengths before attempting to match names

                lda (A1L)
                and #$0f
                cmp (namlo)
                bne +
                tay
-               lda (A1L), y
                cmp (namlo), y
                beq foundname

                ;match failed, check if any directory entries remain

+               plp
++              inc sizehi
                lda sizehi
                cmp entries

                ;lock if entry not found

                beq *

                ;move to next directory in this block, if possible

+++             clc
                lda A1L
                adc #$27
                sta A1L
                bcc +

                ;there can be only one page crossed, so we can increment instead of adc

                inc A1H
+               inc sizelo
                lda sizelo
                cmp #$0d
                bcc nextent

                ;read next directory block when we reach the end of this block

                lda dirbuf+2
                ldx dirbuf+3
                jsr readdirsec
                bra firstent

foundname       dey
                bne -

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
                bpl +
                jsr readdirsec

                ;restore load offset

+               plp
                plx
                pla

                ;check file type and fake load address for subdirectories

                bcc +
                ldx #>dirbuf
+               sta adrlo
                stx adrhi
                stz namlo

                ;set read size to min(length, $200)

readfile        lda sizelo
                ldx sizehi
                cpx #2
                bcc +
                lda #0
                ldx #2
+               sta secsize1
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

unrdrvoff=unrelocdsk+(*-reloc)
readdone        lda $c0e8
readret         rts

readdirsec      ldy #2
                sty secsize2
                ldy #>dirbuf
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
                bne +
                ldy secsize1
+               sty secsize
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
+               sta secsize
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
                bne *                   ;lock if read failure
unrread1=unrelocdsk+(*-reloc)
-               ldx $c0ec
                bpl -
                eor nibtbl-$80, x
                sta bit2tbl-$aa, y
                iny
                bne -
unrread2=unrelocdsk+(*-reloc)
-               ldx $c0ec
                bpl -
                eor nibtbl-$80, x
                sta (adrlo), y          ;the real address
                iny
                cpy secsize
                bne -
                ldy #0
-               ldx #$a9
--              inx
                beq -
                lda (adrlo), y
                lsr bit2tbl-$aa, x
                rol
                lsr bit2tbl-$aa, x
                rol
                sta (adrlo), y
                iny
                cpy secsize
                bne --
                rts

                ;no tricks here, just the regular stuff

readadr
--		jsr readd5aa
                cmp #$96
                bne --
                ldy #3
--              sta curtrk
		jsr readnib
                rol
                sta tmpsec
		jsr readnib
                and tmpsec
                dey
                bne --
                rts

readd5aa
--		jsr readnib
-		cmp #$d5
		bne --
		jsr readnib
		cmp #$aa
		bne -
		tay			;we need Y=#$AA later

readnib
unrread3=unrelocdsk+(*-reloc)
-		lda $c0ec
		bpl -
seekret		rts			;hard-coded relative to seek

seek            asl curtrk
                stz step
                asl phase
copy_cur        lda curtrk
                sta tmptrk
                sec
                sbc phase
                beq seekret
                bcs +
                eor #$ff
                inc curtrk
                bcc ++
+               dec
                dec curtrk
++              cmp step
                bcc +
                lda step
+               cmp #8
                bcs +
                tay
                sec
+               lda curtrk
                ldx step1, y
		bne +
---		clc
		lda tmptrk
		ldx step2, y
+		stx tmpsec
		and  #3
		rol
		tax
unrseek=unrelocdsk+(*-reloc)
		sta $c0e0, x
--		ldx #$13
-		dex
		bne -
		dec tmpsec
		bne --
		lsr
		bcs ---
		inc step
		bne copy_cur

step1           !byte 1, $30, $28, $24, $20, $1e, $1d, $1c
step2           !byte $70, $2c, $26, $22, $1f, $1e, $1d, $1c

nibtbl          = *
bit2tbl         = nibtbl+128
dataend         = bit2tbl+86
}

unrelochdd
!pseudopc reloc {
		lda	#1
		sta	$42
unrunit=unrelochdd+(*-reloc)
		lda	#$d1
		sta	$43

                ;turn on drive and read volume table of contents
                stz adrlo
                lda #2
                ldx #0
                jsr hddreaddirsec

hddreaddir      ;include volume directory header in count

                lda dirbuf+37
                inc
                sta entries
                stz sizehi
hddfirstent     stz sizelo
                lda #<(dirbuf+4)
                sta A1L
                lda #>(dirbuf+4)
                sta A1H
hddnextent      lda (A1L)
                and #$f0

                ;skip deleted entries without counting

                beq +++

                ;subdirectory entries are seedlings
                ;but we need to distinguish between them later

                cmp #$d0
                beq hddsavetype

                ;watch for seedling and saplings only

                cmp #$30
                bcs ++

                ;remember type

hddsavetype     asl
                asl
                php

                ;match name lengths before attempting to match names

                lda (A1L)
                and #$0f
                cmp (namlo)
                bne +
                tay
-               lda (A1L), y
                cmp (namlo), y
                beq hddfoundname

                ;match failed, check if any directory entries remain

+               plp
++              inc sizehi
                lda sizehi
                cmp entries

                ;lock if entry not found

                beq *

                ;move to next directory in this block, if possible

+++             clc
                lda A1L
                adc #$27
                sta A1L
                bcc +

                ;there can be only one page crossed, so we can increment instead of adc

                inc A1H
+               inc sizelo
                lda sizelo
                cmp #$0d
                bcc hddnextent

                ;read next directory block when we reach the end of this block

                lda dirbuf+2
                ldx dirbuf+3
                jsr hddreaddirsec
                bra hddfirstent

hddfoundname    dey
                bne -

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
                bpl +
                jsr hddreaddirsec

                ;restore load offset

+               plp
                plx
                pla

                ;check file type and fake load address for subdirectories

                bcc +
                ldx #>dirbuf
+               sta adrlo
                stx adrhi
                stz namlo

                ;set read size to min(length, $200)

hddreadfile     lda sizehi
                cmp #2
                bcs +
		pha
		lda #2
		sta sizehi
		lda adrhi
		pha
		lda adrlo
		pha
                lda #>dirbuf
                sta adrhi
                stz adrlo
+		php

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
		bcc +
		lda sizelo
		bne hddreadfile
		rts

+		pla
		sta A1L
		pla
		sta A1H
		dec adrhi
		dec adrhi
		ply
		beq +
		ldy #0
-		lda (adrlo), y
		sta (A1L), y
		iny
		bne -
		inc adrhi
		inc A1H
+
-		lda (adrlo), y
		sta (A1L), y
		iny
		cpy sizelo
		bne -
		rts

hddreaddirsec   ldy #>dirbuf
                sty adrhi

hddseekread     sta bloklo
                stx blokhi

unrentry=unrelochdd+(*-reloc)
                jmp $d1d1
}
!byte $D3,$C1,$CE,$A0,$C9,$CE,$C3,$AE