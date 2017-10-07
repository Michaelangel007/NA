;open/read/write binary file in ProDOS filesystem
;copyright (c) Peter Ferrie 2013-16
!cpu 6502
!to "openflp",plain
*=$800

                tmpsec    = $3c
                reqsec    = $3d          ;boot PROM constant
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

                sizelo    = $f5         ;must set if writing
                sizehi    = $f6         ;must set if writing
                namlo     = $fb
                namhi     = $fc
                step      = $fd         ;state for stepper motor
                tmptrk    = $fe         ;temporary copy of current track
                phase     = $ff         ;current phase for seek

                nibtbl    = $2d6        ;boot PROM constant
                bit2tbl   = $300        ;boot PROM constant

                dirbuf    = $1000

                !byte 1
                cmp #5
                txa
                bcs init
                inc reqsec
                lsr
                lsr
                lsr
                lsr
                ora #$c0
                pha
                lda #$5b
                pha
                rts

fakepro         pla
                tax
                inx
                inx
                inx
                txa
                pha
                rts
fakepro_e

matchname       !byte matchname_e-matchname_b
matchname_b     !text "NOXARCH.SYSTEM"
matchname_e

init            sta $bf30
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

                ldy #(fakepro_e-fakepro)
minus03         lda fakepro-1, y
                sta $beff, y
                dey
                bne minus03
                sty $200

opendir         ;read volume directory key block
                ldx #0
                stx adrlo
                stx secsize1
                lda #2
                jsr readdirsec

                ;include volume directory header in count

readdir

firstent        sty A2L
                lda #<(dirbuf+4)
                sta A1L
                lda #>(dirbuf+4)
                sta A1H
nextent         ldy #0
                lda (A1L), y
                and #$f0

                ;watch for seedling and saplings only

                cmp #$30
                bcs plus08

                ;remember type

savetype
                cmp #$20
                php

                ;match name lengths before attempting to match names

                lda (A1L), y
                and #$0f
                tax
                inx
                !byte $2c
minus06         lda (A1L), y
                cmp matchname, y
                beq foundname

                ;match failed, check if any directory entries remain

                plp
plus08

                ;move to next directory in this block, if possible

plus09          clc
                lda A1L
                adc #$27
                sta A1L
                bcc plus10

                ;there can be only one page crossed, so we can increment instead of adc

                inc A1H
plus10          inc A2L
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

                ;cache EOF (file size)

                ldy #$15
                lda (A1L), y
                sta sizelo
                iny
                lda (A1L), y
                sta sizehi

                ;cache AUX_TYPE (load offset for binary files)

                plp
                ldy #$1f
                lda (A1L), y
                pha
                iny
                lda (A1L), y
                pha

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
                sty namlo

                ;read index block in case of sapling

                bcc plus16
                jsr readdirsec

                ;restore load offset

plus16          pla
                tax
                pla

                sta adrlo
                stx adrhi

                ;set read size to min(length, $200)

readfile        lda sizelo
                ldx sizehi
                cpx #2
                bcc plus18
                lda #0
                ldx #2
plus18          sta secsize1
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

                dec sizehi
                beq readdone

                ;loop while size-$200 is non-zero

                dec sizehi
                inc adrhi
                lda sizehi
                ora sizelo
                bne readfile

unrdrvoff
readdone        lda $c0e8
                jmp $2000

readdirsec
unrdrvon
                ldy $c0e9
                ldy #2
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
                bne plus19
                ldy secsize1
plus19          sty secsize
                dec secsize2
                jsr readadr

                ;if track does not match, then seek

                ldx curtrk
                cpx phase
                beq checksec
                jsr seek

                ;force sector mismatch

                lda #$ff

                ;match or read sector

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

cmpsecrd        jsr readadr
cmpsec
                cmp reqsec
                bne cmpsecrd

                ;read sector data

readdata        jsr readd5aa
                eor #$ad                ;zero A if match
;;                bne *                   ;lock if read failure
unrread1
minus07         ldx $c0ec
                bpl minus07
                eor nibtbl, x
                sta bit2tbl-$aa, y
                iny
                bne minus07
unrread2
minus08         ldx $c0ec
                bpl minus08
                eor nibtbl, x
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
readret         rts

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
unrread3
minus23         lda $c0ec
                bpl minus23
seekret         rts

seek            sty step
                asl phase
                txa
                asl
copy_cur        tax
                sta tmptrk
                sec
                sbc phase
                beq seekret
                bcs plus20
                eor #$ff
                inx
                bcc plus21
plus20          sbc #1
                dex
plus21          cmp step
                bcc plus22
                lda step
plus22          cmp #8
                bcs plus23
                tay
                sec
plus23          txa
                pha
                ldx step1, y
                bne plus24
minus24         clc
                lda tmptrk
                ldx step2, y
plus24          stx tmpsec
                and #3
                rol
                tax
unrseek
                sta $c0e0, x
minus25         ldx #$13
minus26         dex
                bne minus26
                dec tmpsec
                bne minus25
                lsr
                bcs minus24
                pla
                inc step
                bne copy_cur

step1           !byte 1, $30, $28, $24, $20, $1e, $1d, $1c
step2           !byte $70, $2c, $26, $22, $1f, $1e, $1d, $1c

!byte $D3,$C1,$CE,$A0,$C9,$CE,$C3,$AE
