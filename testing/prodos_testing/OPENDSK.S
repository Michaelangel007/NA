;This subroutine is designed to be useds as read only or read & writem
;which resutled in the author including a lot of conditional assembler
;logic to reduce the size of the code generated if the programmer 
;flips the switches in the source code to use the routine as read only.
;
;For details on my discussion with the author on the routines
;capabilities, see the email "filesystem code" dated 5/1/2016
;with Peter Ferrie.


;open/read/write binary file in ProDOS filesystem
;copyright (c) Peter Ferrie 2013-16
!cpu 6502
!to "opendsk",plain
*=$2000

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

                status    = $f4         ;returns non-zero on error
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

                jsr init
                lda #1
                sta reqcmd
                lda #0
                sta ldrlo
                lda #$10
                sta ldrhi
                lda #<noxmain
                sta namlo
                lda #>noxmain
                sta namhi
                lda $c08b
                lda $c08b
                jsr $d000
                lda $c089
                jmp $1000

noxmain
!byte noxarch_e-noxarch_b
noxarch_b
!text "NOXARCH.MAIN"
noxarch_e

init            jsr $fe93
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
                inx
                stx unrdrvsel+1
                inx
                inx
                stx unrread1+1
                stx unrread2+1
                stx unrread3+1
                stx unrread4+1
                stx unrread5+1
                stx unrread6+1
                stx unrread7+1
                stx unrread8+1
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
                !byte $c7
                !word c7_parms
                ldx $200
                dex
                stx sizelo
                bmi plus05

readblock       jsr $bf00
                !byte $80
                !word x80_parms

                sta A2L
                lda #<(readbuff+4)
                sta A1L
                lda #>(readbuff+4)
                sta A1H
inextent        ldy #0
                lda (A1L), y
                pha
                and #$d0

                ;watch for subdirectory entries

                cmp #$d0
                bne plus01

                lda (A1L), y
                and #$0f
                tax
                iny
minus01         lda (A1L), y
                cmp (namlo), y
                beq ifoundname

                ;match failed, move to next directory in this block, if possible

minus02
plus01          pla
                clc
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
                pla
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
                ldx #>unrelocdsk
                ldy #<unrelocdsk
slot            lda $cfff
                sta unrentry+1
                php
                beq copydrv
                ldx #>unrelochdd
                ldy #<unrelochdd

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

c7_parms        !byte 1
                !word $200

x80_parms       !byte 3, $d1
                !word readbuff, 2

unrelocdsk
!pseudopc reloc {

opendir         ;read volume directory key block
                ldx #1
                stx command
                dex
                stx adrlo
                stx secsize1
                lda reqcmd
                asl
                bcc plus08
                inx
unrdrvsel=unrelocdsk+(*-reloc)
plus08          lda $c0ea, x

unrblocklo=unrelocdsk+(*-reloc)
                lda #2
unrblockhi=unrelocdsk+(*-reloc)
                ldx #0
                jsr readdirsec

                ;include volume directory header in count

readdir         ldx dirbuf+37
                inx
                stx entries

                sty status
                sty A2H
firstent        sty A2L
                lda #<(dirbuf+4)
                sta A1L
                lda #>(dirbuf+4)
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
                tax
                inx
                !byte $2c
minus06         lda (A1L), y
                cmp (namlo), y
                beq foundname

                ;match failed, check if any directory entries remain

                plp
plus09
                inc A2H
                lda A2H
                cmp entries

                ;lock if entry not found

                bne plus10
                inc status
                rts

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
                php
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

                plp
                beq plus16 

                ;round file size up to nearest sector
                ;and check against requested size if writing

                tay
                beq plus14 
                lda sizelo
                beq plus15
                ldy #0
plus14          sty sizelo
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
                sty namlo

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

                dec sizehi
                beq readdone

                ;loop while size-$200 is non-zero

                dec sizehi
                inc adrhi
                lda sizehi
                ora sizelo
                bne readfile

unrdrvoff=unrelocdsk+(*-reloc)
readdone        lda $c0e8
                rts

readdirsec
unrdrvon=unrelocdsk+(*-reloc)
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
                bne plus20
                ldy secsize1
plus20          sty secsize
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

cmpsec          ldy command
                dey
                bne encsec
                cmp reqsec
                bne cmpsecrd

                ;read sector data

readdata        jsr readd5aa
                eor #$ad                ;zero A if match
;;                bne *                   ;lock if read failure
unrread1=unrelocdsk+(*-reloc)
minus07         ldx $c0ec
                bpl minus07
                eor nibtbl-$80, x
                sta bit2tbl-$aa, y
                iny
                bne minus07
unrread2=unrelocdsk+(*-reloc)
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
readret         rts

encsec          iny
minus11         ldx #$aa
minus12         dey
                lda (adrlo), y
                lsr
                rol bit2tbl-$aa, x
                lsr
                rol bit2tbl-$aa, x
                sta encbuf, y
                lda bit2tbl-$aa, x
                and #$3f
                sta bit2tbl-$aa, x
                inx
                bne minus12
                tya
                bne minus11

cmpsecwr        jsr readadr
                cmp reqsec
                bne cmpsecwr
                ldy #(epilog_e-epilog)-1
minus13         jsr readnib
                cmp epilog, y
                bne minus13
                dey
                bpl minus13

                ;write sector data

                tya
unrlatchout=unrelocdsk+(*-reloc)
                sta $c0ef
unrread4=unrelocdsk+(*-reloc)
                ora $c0ec
                nop       ;2 cycles
                pha       ;3 cycles
                pla       ;4 cycles
                ldy #4
minus14         jsr writenib1
                dey
                bne minus14
                cmp $ea   ;3 cycles
                ldy #(prolog_e-prolog)
minus15         lda prolog-1, y
                jsr writenib3
                dey
                bne minus15
                tya
                ldy #$56
minus16         eor bit2tbl-1, y
                tax
                lda xlattbl, x
unrlatch1=unrelocdsk+(*-reloc)
                sta $c0ed
unrread5=unrelocdsk+(*-reloc)
                lda $c0ec
                asl phase ;5 cycles
                lda bit2tbl-1, y
                dey
                bne minus16
minus17         asl phase ;5 cycles
                eor encbuf, y
                tax
                lda xlattbl, x
unrlatch2=unrelocdsk+(*-reloc)
                sta $c0ed
unrread6=unrelocdsk+(*-reloc)
                lda $c0ec
                lda encbuf, y
                iny
                bne minus17
                tax
                lda xlattbl, x
                jsr writenib2
                cmp $ea   ;3 cycles
                ldy #(epilog_e-epilog)
minus18         lda epilog-1, y
                jsr writenib3
                dey
                bne minus18
unrlatchin=unrelocdsk+(*-reloc)
                lda $c0ee
unrread7=unrelocdsk+(*-reloc)
                lda $c0ec
                rts

writenib1       cmp ($ea,x) ;6 cycles
                cmp $ea,x   ;4 cycles
                !byte $c9   ;2 cycles (turns into "cmp #$c9 / cmp $ea")
writenib2       !byte $c9   ;2 cycles (turns into "cmp #$c9 / nop")
writenib3       cmp $ea     ;3 cycles

unrlatch3=unrelocdsk+(*-reloc)
                sta $c0ed
unrread8=unrelocdsk+(*-reloc)
                ora $c0ec
                rts
prolog          !byte $ad, $aa, $d5
prolog_e
epilog          !byte $ff, $eb, $aa, $de
epilog_e

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
unrread3=unrelocdsk+(*-reloc)
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
                bcs plus21
                eor #$ff
                inx
                bcc plus22
plus21          sbc #1
                dex
plus22          cmp step
                bcc plus23
                lda step
plus23          cmp #8
                bcs plus24
                tay
                sec
plus24          txa
                pha
                ldx step1, y
                bne plus25
minus24         clc
                lda tmptrk
                ldx step2, y
plus25          stx tmpsec
                and #3
                rol
                tax
unrseek=unrelocdsk+(*-reloc)
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

nibtbl          = *
bit2tbl         = nibtbl+128
xlattbl         = bit2tbl+86
dataend         = xlattbl+64
} ;reloc

unrelochdd
!pseudopc reloc {
                ;read volume directory key block

hddopendir      lda #0
                sta adrlo
unrhddblocklo=unrelochdd+(*-reloc)
                lda #2
unrhddblockhi=unrelochdd+(*-reloc)
                ldx #0
                jsr hddreaddirsec

  !if (*-hddopendir) < (readdir-opendir) {
                ;essential padding to match offset with floppy version
    !fill (readdir-opendir)-(*-hddopendir), $ea
  }

                ;include volume directory header in count

hddreaddir      ldx dirbuf+37
                inx
                stx entries

                sta status
                sta A2H
hddfirstent     sta A2L
                lda #<(dirbuf+4)
                sta A1L
                lda #>(dirbuf+4)
                sta A1H
hddnextent      ldy #0
                lda (A1L), y
                and #$f0

                ;skip deleted entries without counting

                beq plus27

                ;watch for seedling and saplings only

                cmp #$30
                bcs plus26

                ;remember type

hddsavetype
                cmp #$20
                php

                ;match name lengths before attempting to match names

                lda (A1L), y
                and #$0f
                tax
                inx
                !byte $2c
minus27         lda (A1L), y
                cmp (namlo), y
                beq hddfoundname

                ;match failed, check if any directory entries remain

                plp
plus26
                inc A2H
                lda A2H
                cmp entries

                ;lock if entry not found

                bne plus27
                inc status
                rts

                ;move to next directory in this block, if possible

plus27          clc
                lda A1L
                adc #$27
                sta A1L
                bcc plus28

                ;there can be only one page crossed, so we can increment instead of adc

                inc A1H
plus28          inc A2L
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
                php
                beq plus31

                ;round requested size up to nearest block
                ;and cache requested size if writing

                lda sizehi
                tax
                lsr
                bcc plus29
                inx
plus29          cpx #2
                bcc plus30
                lda sizelo
                beq plus31
plus30          ldx #2
plus31

                ;cache EOF (file size)

                ldy #$15
                lda (A1L), y
                sta sizelo
                iny
                lda (A1L), y
                sta sizehi

                plp
                beq plus35

                ;round file size up to nearest block
                ;and check against requested size if writing

                tay
                lsr
                bcc plus32
                iny
plus32          cpy #2
                bcc plus33
                lda sizelo
                beq plus34
plus33          lda #0
                sta sizelo
                ldy #2
plus34          sty sizehi

                ;set read size to min(length, requested size)

                cpx sizehi
                bcs plus35
                stx sizehi
plus35

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

                plp
                bcc plus36
                jsr hddreaddirsec

                ;restore load offset

plus36
                ldx ldrhi
                lda ldrlo

                sta adrlo
                stx adrhi

                ;set read size to min(length, $200)

hddreadfile
                ldy reqcmd
                sty command
                lda sizehi
                cmp #2
                bcs plus38
                pha
                lda #2
                sta sizehi
                lda adrhi
                pha
                lda adrlo
                pha
                lda #>dirbuf
                sta adrhi
                lda #0
                sta adrlo
plus38          php

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
                bcc plus39
                lda sizelo
                bne hddreadfile
                rts

plus39          pla
                sta A1L
                pla
                sta A1H
                dec adrhi
                dec adrhi
                pla
                tay
                beq plus40
                dey
minus28         lda (adrlo), y
                sta (A1L), y
                iny
                bne minus28
                inc adrhi
                inc A1H
plus40
minus29         lda (adrlo), y
                sta (A1L), y
                iny
                cpy sizelo
                bne minus29
                rts

hddreaddirsec   ldy #1
                sty command
                ldy #>dirbuf
                sty adrhi

hddseekread     sta bloklo
                stx blokhi

unrunit=unrelochdd+(*-reloc)
                lda #$d1
                sta unit

unrentry=unrelochdd+(*-reloc)
                jmp $d1d1
}
readbuff
!byte $D3,$C1,$CE,$A0,$C9,$CE,$C3,$AE
