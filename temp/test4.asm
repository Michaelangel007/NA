				.CR     6502            Use 6502 overlay
				.OR		$6000			**Always put before .TF directive and never use again in program
				.TF     TEST.BIN,BIN
				.EF		errors
				.LF		C:\MY_CODE\LIST
;				.LI 	OFF				Switches the assembler machine code conversion listing off so errors are easier to see				
;				.IN 	C:\MY_CODE\INCLUDES_LIBS\routines_common



;DEMO
;16-BIT/8-bit SHELL/INSERT SORT (both tested)
;
;****The 8-bit version is enabled***
;
;=====Recod Size=====
;If 8-bit is enabled, a 2 field, 2 byte record is used with the 1st byte
;as the sort key. 
;
;If 16-bit is enabked, a 1 field, 2 byte record is used because
;both bytes are used for the single 16-bit field, effectively
;making it a sort of a 1 item list. This was the original routine,
;and I devised a way to modify it to the 8-bit capability noted
;above
;
;====SORT ORDER=====
;
;Both 16-bit and 8bit version sorts the values from 
;lowest to highest. 
;
;====PARAMETERS=====
;The first two bytes of the data array (TEST.TABLE) is the
;array length in bytes. This must be set before calling the
;subroutine. 
;
;Also the LO address of the data array must be loaded into
;ACC and the HO address into X-REG, before calling the subroutine
;
;
;===SHELL SORT VS. INSERT SORT===
;
;Either JSR SHELL_SORT or INSERTION_SORT directly. They are
;two slightly different techniques which the SHELL_SORT being
;a variation of the INSERTION_SORT.
;
;The URL below in source code credits has some performance metrics
;and author comments. In summary he says insert sorts are
;faster when the values in the array are mostly sorted at the
;start, and the shell sort is faster for mostly unsorted arrays.
;He gives some specific metrics on this, number of unsorted records
;and run time in seconds for both sort types. 
;
;====CHANGING RECORD SIZE====
;
;I think that the amount of the increment to the variable i 
;in .I_LOOP controls the record size. Right now it is a two byte
;record. 
;
;To increase the recore size I think the increment in .I_LOOP 
;would get increased to the number of bytes in the new record
;size. 
;
;Additionally code would need to be added to copy the additionl
;bytes in the new record size. The sections which handle
;the copy of record bytes from one location to another are:
;
;.MOVE.DATA
;.EXIT_J_LOOP
;
;Also worth noting, .DO.COMPARISON does an IS LESS THAN type
;comparison to a record pair, which is a key driver in the order
;of the sorted list. The 16-bit/8-bit modification is in this
;this section. I don't think it would need to be changed if 
;the record size were increased but worth double checking.
;
;
;
;====Expected Result====
;View memory location $7000
;$08.00.01.09.02.07.03.05.04.0A
;
;;==========================================================
;SOURCE CODE CREDIT
;By Fredrik Ramsberg
;http://6502.org/source/sorting/shell.html
;==========================================================
;
;




DRIVER

	lda #TEST.TABLE			;input address.LO
	ldx /TEST.TABLE			;input address.HO
	jsr insertion_sort
	;jsr shell_sort
	BRK
	

	
	
	
	
SHELL_SORT
;PARAMETERS: ACC (LO address of data array), X-REG (HO address of data array), array length*
;ENTRANCE: DIRECT
;RETURN: SORTED LIST in the same memory space used prior to the sort
;*the array length, in bytes, must be stored in the first byte of the data array
;        
				ldy #h_high - h_low - 1
                bne SORT_MAIN         ; Always branch
				
				
INSERTION_SORT
;PARAMETERS: ACC (LO address of data array), X-REG (HO address of data array), array length*
;ENTRANCE: DIRECT
;RETURN: SORTED LIST in the same memory space used prior to the sort
;*the array length, in bytes, must be stored in the first byte of the data array
;  
				ldy #$0

SORT_MAIN
;INIT VALUES
;
;j(2) = input address.LO/HO
;in-address(2) = input address.LO/HO
;arr_start(2) = input address.LO/HO
;arr_length(2) = # of bytes in array 
;
;input address.LO/HO refers to the address of the data array being sorted,
;the HO/LO address of which is loaded into the ACC register and X-REG in 
;the DRIVER section above.
;      
				sty h_start_index
                cld
                sta J
                sta in_address


                clc
                adc #$2
                sta arr_start

                stx j+$1
                stx in_address+$1
												
                txa
                adc #$0
                sta arr_start+$1

                ldy #$0
                lda (j),y
                sta arr_length

                clc
                adc arr_start
                sta arr_end

                iny
                lda (j),y
                sta arr_length+$1

                adc arr_start+$1
                sta arr_end+$1
				
				
							; LDA h_start_index
							; LDX J
							; LDY J + 1
							; BRK
							

;original author comment:   for (h=1; h < length; h=3*h+1);

                ldx h_start_index     ; Start with highest value of h
.CHK_PREV_H  
;seems to test the length of the data array to be sorted and
;initiate an exit from the subroutine if no sort is needed
;because the data array is 0 or 1 bytes in length
;    
				lda h_low,x
                cmp arr_length
                lda h_high,x
                sbc arr_length+$1
                bcc .END_OF_INIT       ; If h < array_length, we've found the right h
                dex
                bpl .CHK_PREV_H
                rts                   ; array length is 0 or 1. No sorting needed.

.END_OF_INIT     
				inx
                stx h_index

;   while (h=(h-1)/3)

.H_LOOP          
				dec h_index
                bpl .GET_H
                rts                   ; All done!

.GET_H 
;some values I traced on the first iteration of the loop
;h   = input address.LO + $2[from table]
;h+1 = $0 [from table]
;i	 = input address.LO + $2[from table]
;i+1 = input address.HO 

				ldy h_index
                lda h_low,y
                sta h
                clc
                adc in_address        ; ( in_address is arr_start - 2)
                sta i
                lda h_high,y
                sta h+$1
                adc in_address+$1
                sta i+$1

; for (i=h, j=i, v=arr[i]; i<=length; arr[j+h]=v, i++, j=i, v=arr[i])

.I_LOOP 
;This section is where the index to the data array, the 
;variable i(2), is incremented. Some data is loaded into
;the variable v(2) which is used later in .EXIT_J_LOOP
;
;I think that the amount of the increment controls the record 
;size. Right now it is a two byte record. To increase the record
;size I think the increment 
;would get increased to the number of bytes in the new record
;size. Additionally code would need to be adde to copy the additionl
;bytes in the new record size. 
;
;
;some values I traced on the first iteration of the loop
;i	 = input address.LO + $2[from table] +$2
;i+1 = input address.HO 
;j	 = input address.LO + $2[from table] +$2
;j+1 = input address.HO  
;v(2)= whe the input address was $7000, v = $7004, v+1 & v = $7005
;v_plus_1(2) just adds $01 to v(2) using a 16-bit add.


				lda i
                clc
                adc #$2			;array index increment
                sta i
                sta j
                lda i+$1
                adc #$0				
                sta i+$1
                sta j+$1

                ldx i
                cpx arr_end
                lda i+$1
                sbc arr_end+$1
                bcs .H_LOOP

                ldy #$0
                lda (j),y
                sta v
                clc
                adc #$1
                sta v_plus_1
                iny
                lda (j),y
                sta v+$1
                adc #0
                bcs .I_LOOP            ; v=$ffff, so no j-loop necessary
                sta v_plus_1+$1

                dey                   ; Set y=0

;         while((j-=h) >= 0 && arr[j] > v)

.J_LOOP 
;some values I traced on the first iteration of the loop
;h		= 2
;j 		= input address.LO +$2[from table]
;j+1 	= input address.HO
;j_plus_h = input address.HO
;x-reg  = j
;h + 1 = $00 [from table]
;j + 1 = input address.HO
;         
				lda j
                sta j_plus_h
                sec
                sbc h
                sta j
                tax
                lda j+$1
                sta j_plus_h+$1
                sbc h+$1
                sta j+$1

; Check if we've reached the bottom of the array

                bcc .EXIT_J_LOOP
                cpx arr_start
                sbc arr_start+$1
                bcc .EXIT_J_LOOp


.DO.COMPARISON
;Original author comment: Do the actual comparison:  arr[j] > v
;
;This is the code section where a comparison test is done on
;a pair of records to shape the order of records in the array.
;
;(j),y 		= record 0
;vplus_1(2) = record 1
;
;The 16-bit version of ths routine compares both bytes
;of record0 and record1 and branches to .EXIT_J_LOOP if
;record 1 is less.
;
;The 8-bit version of ths routine compares the LO byte
;of record0 and record1 and branches to .EXIT_J_LOOP if
;record 1 is less.
;
;The code below has notes on which lines can be commented/uncommented
;to enable/disable 16-bit. Note that enabling 16-bit in this
;way results in a 1 field record format as both bytes are used
;for one 16-bit field.
; 
;some values I traced on the first iteration of the loop:
;
;$7002 (the LO byte (j),y), $7003(the HO byte (j),y) was compared to v_plus_1 (value from $7004) and v_plus_1+1 (value from $9005) 
;
;
;

                lda (j),y
                tax
                iny                   ; Set y=1
                ; lda (j),y			  ;16-BIT/8-BIT: uncomment this line for 16-bit, comment it out for 8-bit

                cpx v_plus_1
                ; sbc v_plus_1+$1	  ;16-BIT/8-BIT: uncomment this line for 16-bit, comment it out for 8-bit	
                bcc .EXIT_J_LOOP		;branch if vplus is less than (j),y,


.MOVE.DATA
;original author comment: arr[j+h]=arr[j]
;
;some values I traced on the first iteration of the loop:
;
;Record0: $7002-7003
;Record1: $7004-7005
;
;Another way to summarize this section is, using the values traced
;on the first iteration:
;
;LDA $7003  (y=1)
;STA $7005  (y=1)
;
;LDA $7002  (y=0)
;STA $7004  (y=0)

						
                lda (j),y
                sta (j_plus_h),y
                dey                   ; Set y=0
                txa
                sta (j_plus_h),y
				

							
                bcs .J_LOOP            ; Always branch


							

.EXIT_J_LOOP     
;original author comment:       for (i=h, j=i, v=arr[i]; i<length; arr[j+h]=v, i++, j=i, v=arr[i])  ***  arr[j+h]=v part
;
;using values traced in the first iteration, this section
;can be summarized as
;
;LDA data previously loaded in .I_LOOP into the variable v(2)
;STA $7002
;STA $7003 
;
				lda v
                ldy #$0
                sta (j_plus_h),y
                iny
                lda v+$1
                sta (j_plus_h),y
 		
				JMP .I_LOOP



;======DEFINE VARIBLES======

; This describes the sequence h(0)=1; h(n)=k*h(n-1)+1 for k=3 (1,4,13,40...)
; All word-values are muliplied by 2, since we are sorting 2-byte values

h_low           .HS 02.08.18.50.F2.D8.8A.A0.E2
h_high          .HS 00.00.00.00.00.00.08.19.4C


h_start_index   .BS $1
h_index         .BS $1
h               .BS $2
in_address      .BS $2
arr_start       .BS $2
arr_end         .BS $2
i               .BS $2
v               .BS $2
v_plus_1        .BS $2


J			.EQ $fa                   ; Uses two bytes. Has to be on zero-page
j_plus_h	.EQ $fc                   ; Uses two bytes. Has to be on zero-page
arr_length	.EQ j_plus_h              ; Can safely use the same location as
                                      ; j_plus_h, but doesn't have to be on ZP

	.NO $7000
			;  00.01.02.03.04.05.06.07.08.09
TEST.TABLE .HS 08.00.23.01.FE.AA.25.02.23.03
;			   08.00.01.09.02.07.03.05.04.0A
