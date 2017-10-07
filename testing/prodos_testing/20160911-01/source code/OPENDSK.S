;open/read/write binary file in ProDOS filesystem
;copyright (c) Peter Ferrie 2013-16
!cpu 6502
!to "NOXARCH.SYSTEM#FF0000",plain
*=$2000

;writes can be performed beginning at any block in a file, by seeking to the block first
;file size can be retrieved by seeking to position 0 within a file and then reading bleft
;to read a complete file, set the size to something large, eg $ffff


                ;constants
                cmdseek   = 0           ;requires override_size=1 and enable_seek=1
                cmdread   = 1           ;requires enable_write=1 or override_size=1
                cmdwrite  = 2           ;requires enable_write=1

                ;zpage usage
                tmpsec    = $3c
                reqsec    = $3d
                A1L       = $3c         ;only during init
                A1H       = $3d         ;only during init
                A2L       = $3e         ;only during init
                A2H       = $3f         ;only during init
                A3L       = $40         ;only during init
                A3H       = $41         ;only during init
                curtrk    = $40

                command   = $42         ;ProDOS constant
                unit      = $43         ;ProDOS constant
                adrlo     = $44         ;ProDOS constant
                adrhi     = $45         ;ProDOS constant
                bloklo    = $46         ;ProDOS constant
                blokhi    = $47         ;ProDOS constant

                bleftlo   = $ef         ;(internal) bytes left in file
                blefthi   = $f0         ;(internal) bytes left in file
                blkofflo  = $f1         ;(internal) used if override_size=1
                blkoffhi  = $f2         ;(internal) used if override_size=1
                status    = $f3         ;returns non-zero on error
                auxreq    = $f4         ;set to 1 to read/write aux memory
                sizelo    = $f5         ;set if enable_write=1 and writing, or if override_size=1 and reading, or if enable_seek=1 and seeking
                sizehi    = $f6         ;set if enable_write=1 and writing, or if override_size=1 and reading, or if enable_seek=1 and seeking
                entries   = $f7         ;(internal) total number of entries
                reqcmd    = $f8         ;set if enable_write=1 or enable_seek=1
                ldrlo     = $f9         ;set to load address if override_adr=1
                ldrhi     = $fa         ;set to load address if override_adr=1
                namlo     = $fb         ;set to name of file to open
                namhi     = $fc         ;set to name of file to open
                step      = $fd         ;(internal) state for stepper motor
                tmptrk    = $fe         ;(internal) temporary copy of current track
                phase     = $ff         ;(internal) current phase for seek
                reloc     = $d000       ;$400 bytes code, $100 bytes data
                dirbuf    = reloc+$500  ;$200 bytes
                encbuf    = dirbuf+$200 ;$200 bytes

                jsr init
                ldx #4
-               lda jmpmain-1, x
                sta $1ff, x
                stx reqcmd
                dex
                stx auxreq
                stx ldrlo
                bne -
                lda #$10
                sta ldrhi
                pha
                txa
                pha
                php
                dex
                stx sizelo
                stx sizehi
                lda #1
                pha
                txa
                pha
                lda #<noxmain
                sta namlo
                lda #>noxmain
                sta namhi
                lda $c08b
                lda $c08b
                jmp $d003

jmpmain         lda $c089
                rti

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
                sta unrslot1+1
                sta unrslot2+1
                sta unrslot3+1
                sta unrslot4+1
                pha
                ora #$80
                sta unrseek+1
                ora #8
                sta unrdrvoff1+1
                sta unrdrvoff2+1
                tax
                inx
                stx unrdrvon1+1
                stx unrdrvon2+1
                inx
                stx unrdrvsel+1
                inx
                inx
                stx unrread1+1
                stx unrready+1
                stx unrread2+1
                stx unrread3+1
                stx unrread4+1
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
                bmi +++

readblock       jsr $bf00
                !byte $80
                !word x80_parms

                lda #<(readbuff+4)
                sta bloklo
                lda #>(readbuff+4)
                sta blokhi
inextent        ldy #0
                lda (bloklo), y
                pha
                and #$d0

                ;watch for subdirectory entries

                cmp #$d0
                bne +

                lda (bloklo), y
                and #$0f
                tax
                iny
--              lda (bloklo), y
                cmp (namlo), y
                beq ifoundname

                ;match failed, move to next directory in this block, if possible

-
+               pla
                clc
                lda bloklo
                adc #$27
                sta bloklo
                bcc +

                ;there can be only one page crossed, so we can increment instead of adc

                inc blokhi
+               cmp #<(readbuff+$1ff) ;4+($27*$0d)
                lda blokhi
                sbc #>(readbuff+$1ff)
                bcc inextent

                ;read next directory block when we reach the end of this block

                lda readbuff+2
                ldx readbuff+3
                bcs +

ifoundname      iny
                dex
                bne --
                lda (namlo), y
                cmp #'/'
                bne -
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
                bne ++
                ldy #$12
                lda (bloklo), y
                tax
                dey
                lda (bloklo), y
                sta unrblocklo+1
                stx unrblockhi+1
                sta unrhddblocklo+1
                stx unrhddblockhi+1
+               sta x80_parms+4
                stx x80_parms+5
++              lda sizelo
                bne readblock

+++             pla
                lsr
                lsr
                lsr
                tax
                lsr
                ora #$c0
                ldy $bf11, x
                cpy #$c8
                bcs set_slot
                tya
set_slot        sta slot+2
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
                lda $c089
                lda $c089
                ldx #4
                ldy #0
-               lda (A1L), y
reladr          sta reloc, y
                iny
                bne -
                inc A1H
                inc reladr+2
                dex
                bne -
                plp
                bne ++
                ldx #$16
--              stx A1L
                txa
                asl
                bit A1L
                beq +
                ora A1L
                eor #$ff
                and #$7e
-               bcs +
                lsr
                bne -
                tya
                sta nibtbl-$16, x
                txa
                ora #$80
                sta xlattbl, y
                iny
+               inx
                bpl --
++              rts

c7_parms        !byte 1
                !word $200

x80_parms       !byte 3, $d1
                !word readbuff, 2

unrelocdsk
!pseudopc reloc {

rdwrpart        jmp rdwrfile

opendir         ;read volume directory key block
unrblocklo=unrelocdsk+(*-reloc)
                ldx #2
unrblockhi=unrelocdsk+(*-reloc)
                lda #0
                jsr readdirsel

                ;include volume directory header in count

readdir         ldx dirbuf+37
                inx
                stx entries

firstent        lda #<(dirbuf+4)
                sta bloklo
                lda #>(dirbuf+4)
                sta blokhi
nextent         ldy #0
                sty status
                lda (bloklo), y
                and #$f0

                ;skip deleted entries without counting

                beq ++

                ;watch for seedling and saplings only

                cmp #$30
                bcs +

                ;remember type

savetype
                cmp #$20
                php

                ;match name lengths before attempting to match names

                lda (bloklo), y
                and #$0f
                tax
                inx
                !byte $2c
-               lda (bloklo), y
                cmp (namlo), y
                beq foundname

                ;match failed, check if any directory entries remain

                plp
+
                dec entries
                bne ++
nodisk
unrdrvoff1=unrelocdsk+(*-reloc)
                lda $c0e8
                inc status
                rts

                ;move to next directory in this block, if possible

++              clc
                lda bloklo
                adc #$27
                sta bloklo
                bcc +

                ;there can be only one page crossed, so we can increment instead of adc

                inc blokhi
+               cmp #<(dirbuf+$1ff) ;4+($27*$0d)
                lda blokhi
                sbc #>(dirbuf+$1ff)
                bcc nextent

                ;read next directory block when we reach the end of this block

                ldx dirbuf+2
                lda dirbuf+3
                jsr readdirsec
                bne firstent

foundname       iny
                dex
                bne -
                stx entries
                stx blkofflo
                stx blkoffhi

                ldy reqcmd
                cpy #cmdwrite
                php
                bne +

                ;round requested size up to nearest block if writing

                lda sizelo
                ldx sizehi
                jsr round
                sta sizehi
+

                ;cache EOF (file size, loaded backwards)

                ldy #$16
                lda (bloklo), y
                tax
                dey
                lda (bloklo), y

                plp
                bne ++

                ;round file size up to nearest block if writing

                ldy #2
                jsr round
                beq +
                tax
                lda #0
+               sta sizelo

++
                stx blefthi
                sta bleftlo

                ;cache KEY_POINTER

                ldy #$11
                lda (bloklo), y
                tax
                sta dirbuf
                iny
                lda (bloklo), y
                sta dirbuf+256

                ;read index block in case of sapling

                plp
                bcc rdwrfile
                jsr readdirsec

rdwrfile
                ;restore load offset

                ldx ldrhi
                lda ldrlo
                sta adrlo
                stx adrhi

                ;set requested size to min(length, requested size)

                lda bleftlo
                tay
                cmp sizelo
                lda blefthi
                tax
                sbc sizehi
                bcs copyblock
                sty sizelo
                stx sizehi

copyblock
                ldy reqcmd
                ;cpy #cmdseek
                beq +

                ldx auxreq
                jsr setaux

                dey ;cpy #cmdread
                bne rdwrloop
+
                lda blkofflo
                tax
                ora blkoffhi
                beq rdwrloop
                lda sizehi
                pha
                lda sizelo
                pha
                lda adrhi
                sta blokhi
                lda adrlo
                sta bloklo
                stx adrlo
                lda #>encbuf
                clc
                adc blkoffhi
                sta adrhi

                ;determine bytes left in block

                tya ;if cmdseek=0 and cmdread=1
                sec
                sbc blkofflo
                tay
                lda #2
                sbc blkoffhi
                tax

                ;set requested size to min(bytes left, requested size)

                cpy sizelo
                sbc sizehi
                bcs +
                sty sizelo
                stx sizehi

+               lda sizehi
                jsr copycache
                clc
                lda ldrlo
                adc sizelo
                sta ldrlo
                lda ldrhi
                adc sizehi
                sta ldrhi
                sec
                pla
                sbc sizelo
                sta sizelo
                pla
                sbc sizehi
                sta sizehi
                ora sizelo
                bne rdwrfile
                beq rdwrdone

                ;set read/write size to min(length, $200)

rdwrloop        ldx reqcmd
                lda sizehi
                cmp #2
                bcs +
                pha
                lda #2
                sta sizehi
                lda adrhi
                pha
                lda adrlo
                pha
                lda #>encbuf
                sta adrhi
                lda #0
                sta adrlo
                ldx #cmdread

                ;fetch data block and read/write it

+               ldy entries
                inc entries
                txa ;cpx #cmdseek, requires php before here
                beq +
                stx command
                ldx dirbuf, y
                lda dirbuf+256, y
                php
                jsr seekrdwr
                plp
+               bcc +
                dec blefthi
                dec blefthi
+               dec sizehi
                dec sizehi
                bne rdwrloop
unrdrvoff2=unrelocdsk+(*-reloc)
                lda $c0e8
                bcc +
                lda sizelo
                bne rdwrloop
rdwrdone        ldx #0
setaux          sta $c002, x
                sta $c004, x
                rts

                ;cache partial block offset

+               pla
                sta bloklo
                pla
                sta blokhi
                pla
                sta sizehi
                dec adrhi
                dec adrhi
copycache       ldy reqcmd
                ;cpy #cmdseek
                beq ++
                tay
                beq +
                dey
-               lda (adrlo), y
                sta (bloklo), y
                iny
                bne -
                inc blokhi
                inc adrhi
                bne +
-               lda (adrlo), y
                sta (bloklo), y
                iny
+               cpy sizelo
                bne -
++              lda bleftlo
                sec
                sbc sizelo
                sta bleftlo
                lda blefthi
                sbc sizehi
                sta blefthi
                clc
                lda blkofflo
                adc sizelo
                sta blkofflo
                lda blkoffhi
                adc sizehi
                and #$fd
                sta blkoffhi
                bcc rdwrdone

round           adc #$fe
                txa
                adc #1
                and #$fe
seekret         rts

seek            sty step
                asl phase
                txa
                asl
copy_cur        tax
                sta tmptrk
                sec
                sbc phase
                beq +++
                bcs +
                eor #$ff
                inx
                bcc ++
+               sbc #1
                dex
++              cmp step
                bcc +
                lda step
+               cmp #8
                bcs +
                tay
                sec
+               txa
                pha
                ldx step1, y
+++             php
                bne +
---             clc
                lda tmptrk
                ldx step2, y
+               stx tmpsec
                and #3
                rol
                tax
                lsr
unrseek=unrelocdsk+(*-reloc)
                lda $c0e0, x
--              ldx #$13
-               dex
                bne -
                dec tmpsec
                bne --
                bcs ---
                plp
                beq seekret
                pla
                inc step
                bne copy_cur

step1           !byte 1, $30, $28, $24, $20, $1e, $1d, $1c
step2           !byte $70, $2c, $26, $22, $1f, $1e, $1d, $1c

readadr
-               jsr readd5aa
                cmp #$96
                bne -
                ldy #3
-               sta curtrk
                jsr readnib
                rol
                sta tmpsec
                jsr readnib
                and tmpsec
                dey
                bne -
                rts

readd5aa
--              jsr readnib
-               cmp #$d5
                bne --
                jsr readnib
                cmp #$aa
                bne -
                tay                    ;we need Y=#$AA later

readnib
unrread1=unrelocdsk+(*-reloc)
-               lda $c0ec
                bpl -
                rts

readdirsel
                ldy #0
                sty adrlo
                sty status

                asl reqcmd
                bcc seldrv
                iny
seldrv          lsr reqcmd
unrdrvsel=unrelocdsk+(*-reloc)
                cmp $c0ea, y
unrdrvon1=unrelocdsk+(*-reloc)
                ldy $c0e9
unrready=unrelocdsk+(*-reloc)
-               ldy $c0ec
                bpl -
-               cpy $c0ec
                bne readdirsec
                inc status
                bne -
                pla
                pla
                jmp nodisk

readdirsec      ldy #cmdread
                sty command
                ldy #>dirbuf
                sty adrhi

                ;convert block number to track/sector

seekrdwr
unrdrvon2=unrelocdsk+(*-reloc)
                ldy $c0e9
                lsr
                txa
                ror
                lsr
                lsr
                sta phase
                txa
                and #3
                php
                asl
                plp
                rol
                sta reqsec
                jsr readadr

                ;if track does not match, then seek

                ldx curtrk
                cpx phase
                beq checksec
                jsr seek

                ;force sector mismatch

                lda #$ff

                ;match or read/write sector

checksec        jsr cmpsec
                inc reqsec
                inc reqsec

                ;force sector mismatch

cmpsecrd        lda #$ff

cmpsec          ldy command
                cpy #cmdwrite           ;we need Y=2 below
                beq encsec
cmpsec2         cmp reqsec
                beq readdata
                jsr readadr
                beq cmpsec2

                ;read sector data

readdata        jsr readd5aa
                eor #$ad                ;zero A if match
;;                bne *                   ;lock if read failure
unrread2=unrelocdsk+(*-reloc)
-               ldx $c0ec
                bpl -
                eor nibtbl-$96, x
                sta bit2tbl-$aa, y
                iny
                bne -
unrread3=unrelocdsk+(*-reloc)
-               ldx $c0ec
                bpl -
                eor nibtbl-$96, x
                sta (adrlo), y          ;the real address
                iny
                bne -
unrread4=unrelocdsk+(*-reloc)
-               ldx $c0ec
                bpl -
                eor nibtbl-$96, x
                bne cmpsecrd
--              ldx #$a9
-               inx
                beq --
                lda (adrlo), y
                lsr bit2tbl-$aa, x
                rol
                lsr bit2tbl-$aa, x
                rol
                sta (adrlo), y
                iny
                bne -
                inc adrhi
                rts

encsec
--              ldx #$aa
-               dey
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
                bne -
                tya
                bne --

cmpsecwr        jsr readadr
                cmp reqsec
                bne cmpsecwr

                ;skip tail #$DE #$AA #$EB some #$FFs ...

                ldy #$24
-               dey
                bpl -

                ;write sector data

unrslot1=unrelocdsk+(*-reloc)
                ldx #$d1
                lda $c08d, x   ;prime drive
                lda $c08e, x   ;required by Unidisk
                tya
                sta $c08f, x
                ora $c08c, x

                ;40 cycles

                ldy #4                 ;2 cycles
                cmp $ea                ;3 cycles
                cmp ($ea,x)            ;6 cycles
-               jsr writenib1          ;(29 cycles)

                                       ;+6 cycles
                dey                    ;2 cycles
                bne -                  ;3 cycles if taken, 2 if not

                ;36 cycles
                                       ;+10 cycles
                ldy #(prolog_e-prolog) ;2 cycles
                cmp $ea                ;3 cycles
-               lda prolog-1, y        ;4 cycles
                jsr writenib3          ;(17 cycles)

                ;32 cycles if branch taken
                                       ;+6 cycles
                dey                    ;2 cycles
                bne -                  ;3 cycles if taken, 2 if not

                ;36 cycles on first pass
                                       ;+10 cycles
                tya                    ;2 cycles
                ldy #$56               ;2 cycles
-               eor bit2tbl-1, y       ;5 cycles
                tax                    ;2 cycles
                lda xlattbl, x         ;4 cycles
unrslot2=unrelocdsk+(*-reloc)
                ldx #$d1               ;2 cycles
                sta $c08d, x           ;4 cycles
                lda $c08c, x           ;4 cycles

                ;32 cycles if branch taken

                lda bit2tbl-1, y       ;5 cycles
                dey                    ;2 cycles
                bne -                  ;3 cycles if taken, 2 if not

                ;32 cycles
                                       ;+8 cycles
                clc                    ;2 cycles
--              eor encbuf, y          ;4 cycles
-               tax                    ;2 cycles
                lda xlattbl, x         ;4 cycles
unrslot3=unrelocdsk+(*-reloc)
                ldx #$d1               ;2 cycles
                sta $c08d, x           ;5 cycles
                lda $c08c, x           ;4 cycles
                bcs +                  ;3 cycles if taken, 2 if not

                ;32 cycles if branch taken

                lda encbuf, y          ;4 cycles
                iny                    ;2 cycles
                bne --                 ;3 cycles if taken, 2 if not

                ;32 cycles
                                       ;+10 cycles
                sec                    ;2 cycles
                bcs -                  ;3 cycles

                ;32 cycles
                                       ;+3 cycles
+               ldy #(epilog_e-epilog) ;2 cycles
                cmp ($ea,x)            ;6 cycles
-               lda epilog-1, y        ;4 cycles
                jsr writenib3          ;(17 cycles)

                ;32 cycles if branch taken
                                       ;+6 cycles
                dey                    ;2 cycles
                bne -                  ;3 cycles if branch taken, 2 if not

                lda $c08e, x
                lda $c08c, x
                inc adrhi
                rts

writenib1       cmp ($ea,x)            ;6 cycles
writenib2       cmp ($ea,x)            ;6 cycles
writenib3
unrslot4=unrelocdsk+(*-reloc)
                ldx #$d1               ;2 cycles
writenib4       sta $c08d, x           ;4 cycles
                ora $c08c, x           ;5 cycles
                rts                    ;6 cycles

prolog          !byte $ad, $aa, $d5
prolog_e
epilog          !byte $ff, $eb, $aa, $de
epilog_e

bit2tbl         = (*+255) & -256
nibtbl          = bit2tbl+86
xlattbl         = nibtbl+106
dataend         = xlattbl+64
      !if dataend>dirbuf {
        !serious "code is too large"
      }
} ;reloc

unrelochdd
!pseudopc reloc {

hddrdwrpart     jmp hddrdwrfile

                ;read volume directory key block

hddopendir
unrhddblocklo=unrelochdd+(*-reloc)
                ldx #2
unrhddblockhi=unrelochdd+(*-reloc)
                lda #0
                jsr hddreaddirsel

  !if (*-hddopendir) < (readdir-opendir) {
                ;essential padding to match offset with floppy version
    !fill (readdir-opendir)-(*-hddopendir), $ea
  }

                ;include volume directory header in count

hddreaddir      ldx dirbuf+37
                inx
                stx entries

hddfirstent     lda #<(dirbuf+4)
                sta bloklo
                lda #>(dirbuf+4)
                sta blokhi
hddnextent      ldy #0
                sty status
                lda (bloklo), y
                and #$f0

                ;skip deleted entries without counting

                beq ++

                ;watch for seedling and saplings only

                cmp #$30
                bcs +

                ;remember type

hddsavetype
                cmp #$20
                php

                ;match name lengths before attempting to match names

                lda (bloklo), y
                and #$0f
                tax
                inx
                !byte $2c
-               lda (bloklo), y
                cmp (namlo), y
                beq hddfoundname

                ;match failed, check if any directory entries remain

                plp
+
                dec entries
                bne ++
                inc status
                rts

                ;move to next directory in this block, if possible

++              clc
                lda bloklo
                adc #$27
                sta bloklo
                bcc +

                ;there can be only one page crossed, so we can increment instead of adc

                inc blokhi
+               cmp #<(dirbuf+$1ff) ;4+($27*$0d)
                lda blokhi
                sbc #>(dirbuf+$1ff)
                bcc hddnextent

                ;read next directory block when we reach the end of this block

                ldx dirbuf+2
                lda dirbuf+3
                jsr hddreaddirsec
                bcc hddfirstent

hddfoundname    iny
                dex
                bne -
                stx entries
                stx blkofflo
                stx blkoffhi

                ldy reqcmd
                cpy #cmdwrite
                php
                bne +

                ;round requested size up to nearest block if writing

                lda sizelo
                ldx sizehi
                jsr hddround
                sta sizehi
+

                ;cache EOF (file size, loaded backwards)

                ldy #$16
                lda (bloklo), y
                tax
                dey
                lda (bloklo), y

                plp
                bne ++

                ;round file size up to nearest block if writing

                ldy #2
                jsr hddround
                beq +
                tax
                lda #0
+               sta sizelo

++
                stx blefthi
                sta bleftlo

                ;cache KEY_POINTER

                ldy #$11
                lda (bloklo), y
                tax
                sta dirbuf
                iny
                lda (bloklo), y
                sta dirbuf+256

                ;read index block in case of sapling

                plp
                bcc hddrdwrfile
                jsr hddreaddirsec

hddrdwrfile
                ;restore load offset

                ldx ldrhi
                lda ldrlo
                sta adrlo
                stx adrhi

                ;set requested size to min(length, requested size)

                lda bleftlo
                tay
                cmp sizelo
                lda blefthi
                tax
                sbc sizehi
                bcs hddcopyblock
                sty sizelo
                stx sizehi

hddcopyblock
                ldy reqcmd
                ;cpy #cmdseek
                beq +

                ldx auxreq
                jsr hddsetaux

                dey ;cpy #cmdread
                bne hddrdwrloop
+
                lda blkofflo
                tax
                ora blkoffhi
                beq hddrdwrloop
                lda sizehi
                pha
                lda sizelo
                pha
                lda adrhi
                sta blokhi
                lda adrlo
                sta bloklo
                stx adrlo
                lda #>encbuf
                clc
                adc blkoffhi
                sta adrhi

                ;determine bytes left in block

                tya ;if cmdseek=0 and cmdread=1
                sec
                sbc blkofflo
                tay
                lda #2
                sbc blkoffhi
                tax

                ;set requested size to min(bytes left, requested size)

                cpy sizelo
                sbc sizehi
                bcs +
                sty sizelo
                stx sizehi

+               lda sizehi
                jsr hddcopycache
                clc
                lda ldrlo
                adc sizelo
                sta ldrlo
                lda ldrhi
                adc sizehi
                sta ldrhi
                sec
                pla
                sbc sizelo
                sta sizelo
                pla
                sbc sizehi
                sta sizehi
                ora sizelo
                bne hddrdwrfile
                beq hddrdwrdone

                ;set read/write size to min(length, $200)

hddrdwrloop     ldx reqcmd
                lda sizehi
                cmp #2
                bcs +
                pha
                lda #2
                sta sizehi
                lda adrhi
                pha
                lda adrlo
                pha
                lda #>encbuf
                sta adrhi
                lda #0
                sta adrlo
                ldx #cmdread

                ;fetch data block and read/write it

+               ldy entries
                inc entries
                txa ;cpx #cmdseek, requires php before here
                beq +
                stx command
                ldx dirbuf, y
                lda dirbuf+256, y
                php
                jsr hddseekrdwr
                plp
+               bcc +
                inc adrhi
                inc adrhi
                dec blefthi
                dec blefthi
+               dec sizehi
                dec sizehi
                bne hddrdwrloop
                bcc +
                lda sizelo
                bne hddrdwrloop
hddrdwrdone     ldx #0
hddsetaux       sta $c002, x
                sta $c004, x
                rts

                ;cache partial block offset

+               pla
                sta bloklo
                pla
                sta blokhi
                pla
                sta sizehi
hddcopycache    ldy reqcmd
                ;cpy #cmdseek
                beq ++
                tay
                beq +
                dey
-               lda (adrlo), y
                sta (bloklo), y
                iny
                bne -
                inc blokhi
                inc adrhi
                bne +
-               lda (adrlo), y
                sta (bloklo), y
                iny
+               cpy sizelo
                bne -
++              lda bleftlo
                sec
                sbc sizelo
                sta bleftlo
                lda blefthi
                sbc sizehi
                sta blefthi
                clc
                lda blkofflo
                adc sizelo
                sta blkofflo
                lda blkoffhi
                adc sizehi
                and #$fd
                sta blkoffhi
                bcc hddrdwrdone

hddround        adc #$fe
                txa
                adc #1
                and #$fe
                rts

hddreaddirsel   ldy #0
                sty adrlo
                asl reqcmd
                lsr reqcmd

hddreaddirsec   ldy #cmdread
                sty command
                ldy #>dirbuf
                sty adrhi

hddseekrdwr     stx bloklo
                sta blokhi

unrunit=unrelochdd+(*-reloc)
                lda #$d1
                sta unit

unrentry=unrelochdd+(*-reloc)
                jmp $d1d1
}
readbuff
!byte $D3,$C1,$CE,$A0,$C9,$CE,$C3,$AE
