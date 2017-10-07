;NOTE: *******INSTRUCTIONS TO ASSEMBLE WITH ACME****
;(cd\my_code\compression\npc.speech.text)
;(acme ZXDSTSRC.S)
;(that is it! It should generate ZX7.BIN as the output file)


;ZX7 data decompressor for Apple II
;Peter Ferrie (peter.ferrie@gmail.com)
;assemble using ACME
;dst<src
!cpu 65c02
!to "zx7.bf00.bin",plain
*=$BF00

init	=	0 ;set to 1 if you know the values
hiunp	=	0 ;unpacker entirely in high memory
hipak	=	0 ;packed data entirely in high memory (requires hiunp)
!if init {
  oep		=	$1234 ;first unpacked byte to run, you must set this by yourself
  orgoff	=	$1234 ;offset of first unpacked byte, you must set this by yourself
}
!if hiunp {
  hioff		=	$d000 ;address of unpacker in high memory, you can change this but leave room for packed data if hipak=1
  !if hipak {
    paksize	=	$0 ;size of packed data, you must set this by yourself if hiunp=1
  }
} else {
  paksize	=	$0 ;size of packed data, you must set this by yourself if hiunp=0
}

;unpacker variables, no need to change these
src	=	$0
dst	=	$2
ecx	=	$4
last	=	$6
tmp	=	$8
A1L	=	$3c
A1H	=	$3d
A2L	=	$3e
A2H	=	$3f
A4L	=	$42
A4H	=	$43
LCBANK2	=	$c083
MOVE	=	$fe2c

!if init {
	lda	#>pakoff ;packed data offset
	sta	src+1
	lda	#<pakoff
	sta	src
	lda	#>orgoff ;original unpacked data offset
	sta	dst+1
  !if (>(oep-1)=>orgoff) { ;oep = original entrypoint
	pha
  } else {
	lda	#>(oep-1)
	pha
  }
	lda	#<orgoff
	sta	dst
  !if (<(oep-1)=<orgoff) {
	pha
  } else {
	lda	#<(oep-1)
	pha
  }
}

unpack ;unpacker entrypoint
!if hiunp {
	ldy	#0
	lda	#>unpmain
	sta	A1H
	lda	#<unpmain
	sta	A1L
  !if hipak {
	lda	#>pakoff+paksize ;packed data offset + packed data size
	sta	A2H
	lda	#<pakoff+paksize
	sta	A2L
  } else {
	lda	#>pakoff
	sta	A2H
	lda	#<pakoff
	sta	A2L
  }
	lda	#>hioff
	sta	A4H
	lda	#<hioff
	sta	A4L
	jsr	MOVE
	lda	LCBANK2
	jmp	hioff
} else {
	jmp	unpmain

pakoff
	;place packed data here for low memory unpacking
*=pakoff+paksize
}

;!pseudopc hioff { ;uncomment if hiunp
unpmain
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
	sbc	tmp
	sta	src
	lda	dst+1
	sbc	tmp+1
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
	sta	(dst)
	inc	dst
	bne	+
	inc	dst+1
+	rts

getsrc
	lda	(src)
	inc	src
	bne	+
	inc	src+1
+	rts

!if hiunp {
pakoff
	;place packed data here for high memory unpacking
}
;} ;uncomment if hiunp
