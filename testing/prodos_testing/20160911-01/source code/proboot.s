;open/read/write binary file in ProDOS filesystem
;copyright (c) Peter Ferrie 2013-16
!cpu 6502
!to "proboot",plain
*=$829

                enable_floppy = 1       ;set to 1 to enable floppy drive support
                override_adr = 0        ;set to 1 to require an explicit load address
                enable_write = 0        ;set to 1 to enable write support
                                        ;file must exist already and its size cannot be altered
                                        ;writes occur in multiples of block size (256 bytes for floppy, 512 bytes for HDD)
                allow_multi  = 0        ;set to 1 to allow multiple floppies
                allow_subdir = 0        ;set to 1 to allow opening subdirectories to access files
                might_exist  = 0        ;set to 1 if file is not known to always exist already
                                        ;makes use of status to indicate success or failure
                allow_aux    = 0        ;set to 1 to allow read/write directly to/from aux memory
                load_high    = 0        ;load into banked RAM instead of main RAM
                lc_bank      = 1        ;load into specified bank (1 or 2) if load_high=1

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

                sizehi    = $f6         ;must set if writing
                entries   = $f7         ;total number of entries
                reqcmd    = $f8         ;used if enable_write=1, 1=read, 2=write; if allow_multi=1, bit 7 selects drive
                ldrlo     = $f9         ;used if override_adr=1
                ldrhi     = $fa         ;used if override_adr=1
                namlo     = $fb
                namhi     = $fc
                step      = $fd         ;state for stepper motor
                tmptrk    = $fe         ;temporary copy of current track
                phase     = $ff         ;current phase for seek
                reloc     = $852
                dirbuf    = $e00

init            txa
                tay
                ora #$80
                sta unrseek+1
                ora #$0c
                sta unrread1+1
                sta unrread2+1
                sta unrread3+1
                sta unrread4+1

minus01         lda $811, x
                sta $beff, x
                lda #$ff
                sta $bf0f, x
                dex
                bne minus01
                sty $bf30

                sta $200
unrelocdsk
;!pseudopc reloc {

opendir         ;read volume directory key block
                txa
                ldx #2
                jsr readdirsel

                ;include volume directory header in count

firstent        lda #<(dirbuf+4)
                sta A1L
                lda #>(dirbuf+4)
                sta A1H
nextent         ldy #0
                lda (A1L), y

                ;watch for seedling and saplings only

                cmp #$30
                bcs plus08

                ;match name lengths before attempting to match names

                lda (A1L), y
                and #$0f
                tax
                inx
                !byte $2c
minus07         lda (A1L), y
                cmp $81a, y
                beq foundname

                ;move to next directory in this block, if possible

plus08
plus09          clc
                lda A1L
                adc #$27
                sta A1L
                bcc plus10

                ;there can be only one page crossed, so we can increment instead of adc

                inc A1H
plus10          cmp #<(dirbuf+$1ff) ;4+($27*$0d)
                lda A1H
                sbc #>(dirbuf+$1ff)
                bcc nextent

                ;read next directory block when we reach the end of this block

                ldx dirbuf+2
                lda dirbuf+3
                jsr readdirsec
                bne firstent

foundname       iny
                dex
                bne minus07

                ;cache EOF (file size)

                ldy #$15
                lda (A1L), y
                cmp #1
                iny
                lda (A1L), y
                adc #1
                lsr
                sta sizehi

                ;cache KEY_POINTER

                ldy #$11
                lda (A1L), y
                tax
                iny
                lda (A1L), y

                ;read index block in case of sapling

                jsr readdirsec

                ;restore load offset

                asl adrhi

readfile
                ;fetch data block and read it

                ldy $41 ;zeroed by boot PROM
                inc $41
                ldx dirbuf, y
                lda dirbuf+256, y
                jsr seekread

                ;loop while size-$200 is non-zero

                dec sizehi
                bne readfile

readdone        jmp $2000

                ;no tricks here, just the regular stuff

seek            sty step
                asl phase
                txa
                asl
copy_cur        tax
                sta tmptrk
                sec
                sbc phase
                beq plus23x
                bcs plus19x
                eor #$ff
                inx
                bcc plus20x
plus19x          sbc #1
                dex
plus20x          cmp step
                bcc plus21x
                lda step
plus21x          cmp #8
                bcs plus22x
                tay
                sec
plus22x          txa
                pha
                ldx step1, y
plus23x          php
                bne plus24x
minus08x         clc
                lda tmptrk
                ldx step2, y
plus24x          stx tmpsec
                and #3
                rol
                tax
                lsr
unrseek=unrelocdsk+(*-reloc)
                lda $c0e0, x
minus09x         ldx #$13
minus10x         dex
                bne minus10x
                dec tmpsec
                bne minus09x
                bcs minus08x
                plp
                beq seekret
                pla
                inc step
                bne copy_cur

step1           !byte 1, $30, $28, $24, $20, $1e, $1d, $1c
step2           !byte $70, $2c, $26, $22, $1f, $1e, $1d, $1c

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
seekret         rts

readd5aa
minus21         jsr readnib
minus22         cmp #$d5
                bne minus21
                jsr readnib
                cmp #$aa
                bne minus22
                tay                    ;we need Y=#$AA later

readnib
unrread1=unrelocdsk+(*-reloc)
minus23         lda $c0ec
                bpl minus23
                rts

readdirsel      sta adrlo
readdirsec
                ldy #>dirbuf
                sty adrhi

                ;convert block number to track/sector

seekread        lsr
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

                ;match or read sector

checksec        jsr cmpsec
       
                inc reqsec
                inc reqsec

                ;force sector mismatch

cmpsecrd        lda #$ff

cmpsec          cmp reqsec
                beq readdata
                jsr readadr
                beq cmpsec

                ;read sector data

readdata        jsr readd5aa
                eor #$ad                ;zero A if match
;;                bne *                   ;lock if read failure
unrread2=unrelocdsk+(*-reloc)
minus08         ldx $c0ec
                bpl minus08
                eor nibtbl-$96, x
                sta bit2tbl-$aa, y
                iny
                bne minus08
unrread3=unrelocdsk+(*-reloc)
minus09         ldx $c0ec
                bpl minus09
                eor nibtbl-$96, x
                sta (adrlo), y          ;the real address
                iny
                bne minus09
unrread4=unrelocdsk+(*-reloc)
minus10         ldx $c0ec
                bpl minus10
                eor nibtbl-$96, x
                bne cmpsecrd
minus11         ldx #$a9
minus12         inx
                beq minus11
                lda (adrlo), y
                lsr bit2tbl-$aa, x
                rol
                lsr bit2tbl-$aa, x
                rol
                sta (adrlo), y
                iny
                bne minus12
readret         inc adrhi
                rts

bit2tbl         = $300
nibtbl          = bit2tbl+108
dataend         = nibtbl+106
;} ;reloc
readbuff
!byte $D3,$C1,$CE,$A0,$C9,$CE,$C3,$AE
