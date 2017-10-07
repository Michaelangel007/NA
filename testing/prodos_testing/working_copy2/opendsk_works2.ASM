;open/read binary file in ProDOS filesystem
;copyright (c) Peter Ferrie 2013-16

				.OR		$800			**Always put before .TF directive and never use again in program
				.TF     PRODOS.DRIVER.BIN,BIN

tmpsec    = $3c
reqsec    = $3d
A1L       = $3c
A1H       = $3d
A2L       = $3e
A2H       = $3f
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

sizelo    = $f6
sizehi    = $f7
entries   = $f8         ;total number of entries
ldrlo     = $f9
ldrhi     = $fa
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
                tax
                inx
                stx unrdrvon1+1
                stx unrdrvon2+1
                eor #4
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
                ldx /unrelocdsk
                ldy #unrelocdsk

slot
                lda $cfff
                sta unrentry+1
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
                ldy #0
minus01         lda (A1L), y
                sta reloc, y
                lda (A2L), y
                sta reloc+$100, y
                iny
                bne minus01
                plp
                bne plus02
                ldx #3
minus02         stx $3c
                txa
                asl
                bit $3c
                beq plus01
                ora $3c
                eor #$ff
                and #$7e
minus03         bcs plus01
                lsr
                bne minus03
                tya
                sta nibtbl, x
                iny
plus01          inx
                bpl minus02
plus02          rts

unrelocdsk
	.PH reloc ;=====START PATCH=====

                ;turn on drive and read volume directory key block

opendir
unrdrvon1	.EQ	unrelocdsk+*-reloc
                lda $c0e9
                ldx #0
                stx adrlo
                stx secsize1
                lda #2
                jsr readdirsec

                ;essential padding to match offset with HDD version
                nop
                nop
                nop

readdir         ;turn on drive (again) in case of subdirectories

unrdrvon2	.EQ	unrelocdsk+*-reloc
                lda $c0e9

                ;include volume directory header in count

                ldy dirbuf+37
                iny
                sty entries
                lda #0
                sta sizehi
firstent        sta sizelo
                lda #dirbuf+4
                sta A1L
                lda /dirbuf+4
                sta A1H
nextent         ldy #0
                lda (A1L), y
                and #$f0

                ;skip deleted entries without counting

                beq plus05

                ;subdirectory entries are seedlings
                ;but we need to distinguish between them later

                cmp #$d0
                beq savetype

                ;watch for seedling and saplings only

                cmp #$30
                bcs plus04

                ;remember type

savetype        asl
                asl
                php

                ;match name lengths before attempting to match names

                lda (A1L), y
                and #$0f
                cmp (namlo), y
                bne plus03
                tax
                iny
minus04         lda (A1L), y
                cmp (namlo), y
                beq foundname

                ;match failed, check if any directory entries remain

plus03          plp
plus04          inc sizehi
                lda sizehi
                cmp entries

                ;lock if entry not found

                beq *

                ;move to next directory in this block, if possible

plus05          clc
                lda A1L
                adc #$27
                sta A1L
                bcc plus06

                ;there can be only one page crossed, so we can increment instead of adc

                inc A1H
plus06          inc sizelo
                lda sizelo
                cmp #$0d
                bcc nextent

                ;read next directory block when we reach the end of this block

                lda dirbuf+2
                ldx dirbuf+3
                jsr readdirsec
                jmp firstent

foundname       iny
                dex
                bne minus04

                ;cache EOF (file size)

                ldy #$15
                lda (A1L), y
                sta sizelo
                iny
                lda (A1L), y
                sta sizehi

                ;cache AUX_TYPE (load offset for binary files)

;                pla
;                tax
;                ldy #$1f
;                lda (A1L), y
;                pha
;                iny
;                lda (A1L), y
;                pha
;                txa
;                pha

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
                bpl plus07
                php
                jsr readdirsec
                plp

                ;restore load offset

plus07
;                pla
;                tax
;                pla
                ldx ldrhi
                lda ldrlo

                ;check file type and fake load address for subdirectories

                bcc plus08
                ldx /dirbuf
                lda #dirbuf
plus08          sta adrlo
                stx adrhi
                lda #0
                sta namlo

                ;set read size to min(length, $200)

readfile        lda sizelo
                ldx sizehi
                cpx #2
                bcc plus09
                lda #0
                ldx #2
plus09          sta secsize1
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
readret         rts

readdirsec      ldy #2
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
                bne plus10
                ldy secsize1
plus10          sty secsize
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
                eor #$ad                ;zero A if match
                bne *                   ;lock if read failure
unrread1	.EQ	unrelocdsk+*-reloc
minus05         ldx $c0ec
                bpl minus05
                eor nibtbl-$80, x
                sta bit2tbl-$aa, y
                iny
                bne minus05
unrread2	.EQ	unrelocdsk+*-reloc
minus06         ldx $c0ec
                bpl minus06
                eor nibtbl-$80, x
                sta (adrlo), y          ;the real address
                iny
                cpy secsize
                bne minus06
                ldy #0
minus07         ldx #$a9
minus08         inx
                beq minus07
                lda (adrlo), y
                lsr bit2tbl-$aa, x
                rol
                lsr bit2tbl-$aa, x
                rol
                sta (adrlo), y
                iny
                cpy secsize
                bne minus08
                rts

                ;no tricks here, just the regular stuff

readadr
minus09         jsr readd5aa
                cmp #$96
                bne minus09
                ldy #3
minus10         sta curtrk
                jsr readnib
                rol
                sta tmpsec
                jsr readnib
                and tmpsec
                dey
                bne minus10
                rts

readd5aa
minus11         jsr readnib
minus12         cmp #$d5
                bne minus11
                jsr readnib
                cmp #$aa
                bne minus12
                tay                        ;we need Y=#$AA later

readnib
unrread3	.EQ	unrelocdsk+*-reloc
minus13         lda $c0ec
                bpl minus13
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
                bcs plus11
                eor #$ff
                inc curtrk
                bcc plus12
plus11          sbc #1
                dec curtrk
plus12          cmp step
                bcc plus13
                lda step
plus13          cmp #8
                bcs plus14
                tay
                sec
plus14          lda curtrk
                ldx step1, y
                bne plus15
minus14         clc
                lda tmptrk
                ldx step2, y
plus15          stx tmpsec
                and #3
                rol
                tax
unrseek	.EQ	unrelocdsk+*-reloc
                sta $c0e0, x
minus15         ldx #$13
minus16         dex
                bne minus16
                dec tmpsec
                bne minus15
                lsr
                bcs minus14
                inc step
                bne copy_cur

;step1           !byte 1, $30, $28, $24, $20, $1e, $1d, $1c
step1	.HS		01.30.28.24.20.1E.1D.1C

;step2           !byte $70, $2c, $26, $22, $1f, $1e, $1d, $1c
step2 	.HS		70.2C.26.22.1F.1E.1D.1C

nibtbl          = *
bit2tbl         = nibtbl+128
dataend         = bit2tbl+86
	.EP	;=====END PATCH====
	

unrelochdd
	.PH reloc ======START PATCH
                ;read volume directory key block

hddopendir
                lda #1
                sta command
unrunit	.EQ unrelochdd+*-reloc
                lda #$d1
                sta unit
                ldx #0
                stx adrlo
                lda #2
                jsr hddreaddirsec

hddreaddir      ;include volume directory header in count

                ldy dirbuf+37
                iny
                sty entries
                lda #0
                sta sizehi
hddfirstent     sta sizelo
                lda #dirbuf+4
                sta A1L
                lda /dirbuf+4
                sta A1H
hddnextent      ldy #0
                lda (A1L), y
                and #$f0

                ;skip deleted entries without counting

                beq plus18

                ;subdirectory entries are seedlings
                ;but we need to distinguish between them later

                cmp #$d0
                beq hddsavetype

                ;watch for seedling and saplings only

                cmp #$30
                bcs plus17

                ;remember type

hddsavetype     asl
                asl
                php

                ;match name lengths before attempting to match names

                lda (A1L), y
                and #$0f
                cmp (namlo), y
                bne plus16
                tax
                iny
minus17         lda (A1L), y
                cmp (namlo), y
                beq hddfoundname

                ;match failed, check if any directory entries remain

plus16          plp
plus17          inc sizehi
                lda sizehi
                cmp entries

                ;lock if entry not found

                beq *

                ;move to next directory in this block, if possible

plus18          clc
                lda A1L
                adc #$27
                sta A1L
                bcc plus19

                ;there can be only one page crossed, so we can increment instead of adc

                inc A1H
plus19          inc sizelo
                lda sizelo
                cmp #$0d
                bcc hddnextent

                ;read next directory block when we reach the end of this block

                lda dirbuf+2
                ldx dirbuf+3
                jsr hddreaddirsec
                jmp hddfirstent

hddfoundname    iny
                dex
                bne minus17

                ;cache EOF (file size)

                ldy #$15
                lda (A1L), y
                sta sizelo
                iny
                lda (A1L), y
                sta sizehi

                ;cache AUX_TYPE (load offset for binary files)

;                pla
;                tax
;                ldy #$1f
;                lda (A1L), y
;                pha
;                iny
;                lda (A1L), y
;                pha
;                txa
;                pha

                ;cache KEY_POINTER (loaded backwards)
                ;and construct single-entry index block in case of seedling

                ldy #$12
                lda (A1L), y
                tax
                sta dirbuf+256
                dey
                lda (A1L), y
                sta dirbuf
                lda #0
                sta dirbuf+257
                sta dirbuf+1

                ;read index block in case of sapling

                plp
                bpl plus20
                php
                jsr hddreaddirsec
                plp

                ;restore load offset

plus20
;                pla
;                tax
;                pla
                ldx ldrhi
                lda ldrlo

                ;check file type and fake load address for subdirectories

                bcc plus21
                ldx /dirbuf
                lda #dirbuf
plus21          sta adrlo
                stx adrhi
                lda #0
                sta namlo

                ;set read size to min(length, $200)

hddreadfile     lda sizehi
                cmp #2
                bcs plus22
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
plus22          php

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
                bcc plus23
                lda sizelo
                bne hddreadfile
                rts

plus23          pla
                sta A1L
                pla
                sta A1H
                dec adrhi
                dec adrhi
                pla
                tay
                beq plus24
                ldy #0
minus18         lda (adrlo), y
                sta (A1L), y
                iny
                bne minus18
                inc adrhi
                inc A1H
plus24
minus19         lda (adrlo), y
                sta (A1L), y
                iny
                cpy sizelo
                bne minus19
                rts

hddreaddirsec   ldy /dirbuf
                sty adrhi

hddseekread     sta bloklo
                stx blokhi

unrentry	.EQ unrelochdd+*-reloc
                jmp $d1d1
	.EP ;=========END PATCH=====
;Peter's signature
;!byte $D3,$C1,$CE,$A0,$C9,$CE,$C3,$AE
Peter_sig	.HS D3.C1.CE.A0.C9.CE.C3.AE