;ZX7 data decompressor for Apple II
;Peter Ferrie (peter.ferrie@gmail.com)
;assemble using ACME
;src<dst
!cpu 65c02
!to "zx7",plain
*=$800

init	=	0 ;set to 1 if you know the values
!if init {
  oep		=	$1234 ;first unpacked byte to run, you must set this by yourself
  orgoff	=	$1234 ;offset of first unpacked byte, you must set this by yourself
  orgsize	=	$1234 ;size of unpacked data, you must set this by yourself
  paksize	=	$1234 ;size of packed data, you must set this by yourself
}

;unpacker variables, no need to change these
src	=	$0
dst	=	$2
ecx	=	$4
last	=	$6
tmp	=	$8

!if init {
	lda	#>pakoff+paksize ;packed data offset + packed data size
	sta	src+1
	lda	#<pakoff+paksize
	sta	src
	lda	#>orgoff+orgsize ;original unpacked data offset + original unpacked size
	sta	dst+1
  !if (>(oep-1)=>(orgoff+orgsize)) { ;oep = original entrypoint
	pha
  } else {
	lda	#>(oep-1)
	pha
  }
	lda	#<orgoff+orgsize
	sta	dst
	lda	#<(oep-1)
	pha
}

unpack ;unpacker entrypoint
	stz	last
	stz	ecx
	stz	ecx+1

dzx7s_copy_byte_loop:
	jsr	getput

dzx7s_main_loop:
	jsr	dzx7s_next_bit
	bcc	dzx7s_copy_byte_loop
	ldy	#0

dzx7s_len_size_loop:
	iny
	jsr	dzx7s_next_bit
	bcc	dzx7s_len_size_loop
	bra	dzx7s_len_value_skip

dzx7s_next_bit:
	asl	last
	bne	dzx7s_next_bit_ret
	jsr	getsrc
	sec
	rol
	sta	last

dzx7s_next_bit_ret:
	rts

dzx7s_len_value_loop:
	jsr	dzx7s_next_bit

dzx7s_len_value_skip:
	rol	ecx
	rol	ecx+1
	bcs	dzx7s_next_bit_ret
	dey
	bne	dzx7s_len_value_loop
	inc	ecx
	bne	+
	inc	ecx+1
+	jsr	getsrc
	sec
	rol
	sta	tmp
	tya
	bcc	dzx7s_offset_end
	lda	#$10

dzx7s_rld_next_bit:
	pha
	jsr	dzx7s_next_bit
	pla
	rol
	bcc	dzx7s_rld_next_bit
	inc
	lsr

dzx7s_offset_end:
	sta	tmp+1
	ror	tmp
	ldx	src+1
	ldy	src
	lda	dst
	adc	tmp
	sta	src
	lda	dst+1
	adc	tmp+1
	sta	src+1
-	jsr	getput
	jsr	dececx
	ora	ecx+1
	bne	-
	sty	src
	stx	src+1
	bra	dzx7s_main_loop

dececx
	lda	ecx
	bne	+
	dec	ecx+1
+	dec
	sta	ecx
	rts

getput
	jsr	getsrc

putdst
	pha
	lda	dst
	bne	+
	dec	dst+1
+	dec	dst
	pla
	sta	(dst)
	rts

getsrc
	lda	src
	bne	+
	dec	src+1
+	dec	src
	lda	(src)
	rts

pakoff
	;place packed data here
