; ============================================
; Copyright (C) 2016-2018. 6502 Workshop, LLC
; contact: mark@6502workshop.com
; ============================================


;************************INCLUDE FILE*****************************
;(do not assemble stand-alone.  )


;NOTE: THERE ARE VARIABLES USED HERE THAT ARE NOT DEFINED HERE. I SHOULD EITHER MOVE THE VARIABLES DEFINITIONS
;HERE FROM THE GRAPHICS ENGINE, OR USE DIFFERENT VARIABLES. 

;LIST OF FUNCTIONS;
;
;FLIP.PAGE					;SET ACTIVE PAGE TO BACKGROUND AND BACKGROUND PAGE TO ACTIVE
;SCLEAR						;CLEAR BOTH PAGES
;GET.LINE.ADDRESS			;GET BASE ADDRESS FOR LINE, FOR PAGE 1 OR PAGE 2

FLIP.PAGE 		;============SET ACTIVE PAGE TO BACKGROUND==========
@START
;PARAMETERS: PAGE (#$01 OR #$02)
;RETURN: NONE

;DETERMINE WHICH PAGE IS IN FOREGROUND
	LDA PAGE.FOREGROUND
	CMP #$02
	BEQ .PAGE2.CURRENTLY.ACTIVE
	CMP #$01				;DOES ACC = PAGE1? IF, YES FALL THROUGH TO PAGE 1 LOOKUP
	BNE .ERROR				;IF NO, INPUT NOT VALID, EXIT ERROR. 

.PAGE1.CURRENTLY.ACTIVE
	LDA PAGE2				;SET PAGE1 AS BACKGROUND, SET PAGE2 AS FOREGROUND
	LDA #$02				;SAVE TO PAGE VARIABLE
	STA PAGE.FOREGROUND
	LDA #$01
	STA PAGE.BACKGROUND
	RTS
	
.PAGE2.CURRENTLY.ACTIVE
	LDA PAGE1				;SET PAGE2 AS BACKGROUND, SET PAGE1 AS FOREGROUND
	LDA #$01				;SAVE TO PAGE VARIABLE
	STA PAGE.FOREGROUND
	LDA #$02
	STA PAGE.BACKGROUND
	RTS
	
.ERROR
;DISABLE.BS_RAM
	JSR PREP.BRK
	BRK
@END
	
;SCLEAR					 ;============Clear Hi-Res Screen (Page 1 and 2)==========
@START
; ;PARAMTERS: ACC[DEFAULT = BOTH, #$01, PAGE1, #$02, PAGE2, #$03 BACKGROUND PAGE] 
; ;OPTIONAL: (PAGE.BACKGROUND), only required if ACC == #$03 is passed			
; ;RETURN: NONE


; ;DETERMINE WHICH PAGE TO CLEAR
	; CMP #$01
	; BEQ .P1			;PAGE1
	; CMP #$02		
	; BEQ .P2			;PAGE2	
	; CMP #$03
	; BEQ	.P3			;BACKGROUND PAGE
; .DEFAULT
	; LDA #$60
	; STA SCLEAR.PAGE.STOP.ADDRESS	;SETUP PAGE1&2 CLEAR, BY DEFAULT
	
	; LDA #$20
	; STA LINE.BASE.ADDR1+$1
	; JMP .START
	
; .P3 ;SETUP BACKGROUND PAGE CLEAR
		
	; LDA PAGE.BACKGROUND
	; CMP #$02
	; BEQ .P2					;PAGE 2 IS BACKGROUND. 
	; CMP #$01				
	; BEQ .P1					;PAGE 1 IS BACKGROUND
	; JMP .ERROR				;OTHERWISE, INPUT NOT VALID, EXIT ERROR. 


; .P2	;SETUP PAGE2 CLEAR
	; LDA #$60
	; STA SCLEAR.PAGE.STOP.ADDRESS	
	
	; LDA #$30
	; STA LINE.BASE.ADDR1+$1
	; JMP .START
	
	
; .P1	;SETUP PAGE 1 CLEAR
	; LDA #$40
	; STA SCLEAR.PAGE.STOP.ADDRESS
	; LDA #$20
	; STA LINE.BASE.ADDR1+$1
	
; .START
	
	; LDA #$00
	; STA LINE.BASE.ADDR1				;ALL 3 SCENARIOS HAVE $00 IN THE LO BYTE
	
; .CLR1
	; LDY #$00
	; LDA	#$00
; .CLR
	; STA	(LINE.BASE.ADDR1), Y
	; INY
	; BNE .CLR
	; INC LINE.BASE.ADDR1+$1
	; LDA LINE.BASE.ADDR1+$1
	; CMP SCLEAR.PAGE.STOP.ADDRESS
	; BCC	.CLR1		;is acc less than cmp

	; RTS

; .ERROR
	; BRK	
@END	
	
GET.LINE.ADDRESS1		;============GET BASE MEMORY ADDRESS FOR GIVEN LINE (X)=========
@START
;PARAMETERS: X-REG = line number, ACC = Page Number (#$1 or #$2)
;RETURN: LINE.BASE.ADDR (2)

;SAVE REGISTERS
	PHA   ;**OPT** Memory. I'm not sure why this is needed. GET.LINE.ADDRESS2 doesn't have it. 

.START
	CMP #$02
	BEQ .LOOKUP.PAGE2
	CMP #$01				;DOES ACC = PAGE1? IF, YES FALL THROUGH TO PAGE 1 LOOKUP
	BNE .ERROR				;IF NO, INPUT NOT VALID, EXIT ERROR. 

	
.LOOKUP.PAGE1	
	LDA LINE.HO.P1,X		;GET LINE ADDRESS
	STA LINE.BASE.ADDR1+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR1
	JMP .EXIT

.LOOKUP.PAGE2
	LDA LINE.HO.P2,X		;GET LINE ADDRESS
	STA LINE.BASE.ADDR1+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR1
	;** FALLS THROUGH**
	
.EXIT
;RESTORE REGISTERS
	PLA
	
	RTS

.ERROR
	;Error if ACC not set to page 1 (#$01) or page 2 (#$02), when calling this routine
	;Make sure calling routine sets pager parm in ACC before the call. 
	JSR PREP.BRK
	BRK
@END

GET.LINE.ADDRESS2		;============DUPLICATE OF GET.LINE.ADDRESS1 SO TWO LINES CAN BE STORED IN MEMORY=========
@START
; ;PARAMETERS: X-REG = line number, ACC = Page Number (#$1 or #$2)
; ;RETURN: LINE.BASE.ADDR (2)

	CMP #$02
	BEQ .LOOKUP.PAGE2
	CMP #$01				;DOES ACC = PAGE1? IF, YES FALL THROUGH TO PAGE 1 LOOKUP
	BNE .ERROR				;IF NO, INPUT NOT VALID, EXIT ERROR. 

	
.LOOKUP.PAGE1	
	LDA LINE.HO.P1,X		;GET LINE ADDRESS
	STA LINE.BASE.ADDR2+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR2
	RTS

.LOOKUP.PAGE2
	LDA LINE.HO.P2,X		;GET LINE ADDRESS
	STA LINE.BASE.ADDR2+$1
	LDA LINE.LO,X
	STA LINE.BASE.ADDR2
	RTS

.ERROR
	;Error if ACC not set to page 1 (#$01) or page 2 (#$02), when calling this routine
	JSR PREP.BRK
	BRK
@END

;PAGE.MIRROR ;============COPIES FOREGROUND PAGE TO BACKGROUND PAGE=======
@START
; ;PARAMETERS: PAGE.FOREGROUND

; ;DETERMINE WHICH PAGE IS IN FOREGROUND
	; LDA PAGE.FOREGROUND
	; CMP #$02
	; BEQ .PAGE2.CURRENTLY.FOREGROUND
	; CMP #$01				;DOES ACC = PAGE1? IF, YES FALL THROUGH TO PAGE 1 LOOKUP
	; BNE .ERROR				;IF NO, INPUT NOT VALID, EXIT ERROR. 

; .PAGE1.CURRENTLY.FOREGROUND
		; LDA #$00
		; STA COPY.FROM_START
		; LDA #$20
		; STA COPY.FROM_START+$1
		; LDA #$FF
		; STA COPY.FROM_END
		; LDA #$3F
		; STA COPY.FROM_END+$1
		
		; LDA #$00
		; STA COPY.TO
		; LDA #$40
		; STA COPY.TO+$1
		
		; JSR MEMORY.COPY
	
	; JMP .EXIT
	
; .PAGE2.CURRENTLY.FOREGROUND

		; LDA #$00
		; STA COPY.FROM_START
		; LDA #$40
		; STA COPY.FROM_START+$1
		; LDA #$FF
		; STA COPY.FROM_END
		; LDA #$5F
		; STA COPY.FROM_END+$1
		
		; LDA #$00
		; STA COPY.TO
		; LDA #$20
		; STA COPY.TO+$1
		
		; JSR MEMORY.COPY

; .EXIT
	; RTS

; .ERROR
; ;PAGE.MIRROR reports error; PAGE.FOREGROUND doesn't contain a valid hi-res page #
	; JSR PREP.BRK
	; BRK	
@END

DRAW.BYTE ;draws a single byte to the HI-RES screen (both pages)
@START
;PARAMETERS: X-REG = line #, Y-REG = screen byte, ACC = byte value to save to video memory
;ENTRANCE: DIRECT
;RETURN: updated video memory

	;parm: ACC = byte value to save to video memory
	PHA ;push draw byte to stack 
	
		LDA PAGE.BACKGROUND
	JSR GET.LINE.ADDRESS1

		LDA PAGE.FOREGROUND
	JSR GET.LINE.ADDRESS2
	
.UPDATE.VIDEO_MEMORY
	PLA ;pull draw byte from stack
	STA (LINE.BASE.ADDR1),Y		;save bit mapped byte to video screen memory (foreground page)
	STA (LINE.BASE.ADDR2),Y		;save bit mapped byte to video screen memory (background page)

.EXIT

	RTS
	
@END

DRAW.LINE ;=====DRAW SINGLE COLOR LINE OR RECTANGLE=======
@START
;PARAMTERS: ACC = PAGE, DRAW.START_LINE, DRAW.START_BYTE, *DRAW.STOP_BYTE, *DRAW.STOP_LINE, DRAW.BYTE_VALUE.HORIZONTAL(2), DRAW.BYTE_VALUE.VERTICLE(2)
;ENTRANCE: DIRECT
;RETURN: NONE
;*set the stop byte/line to the last byte/line you want drawn +$1, because the index increment is before the conditional test. 
;**$01 = draw on hi-res page 1, $02 = draw on page 2, $03 = draw on both pages

;=====================SUBROUTINE DOCUMENTATION====================================
;
;Draws variable size single color line or rectangle using the parameters provided for the coordinates.  
;No shape tables are used.  
;
;==DRAWING LINES==
;*note: must set the appropriate edge flag (see rectangles docs below). For example,
;to draw a horizontal line, set the top edge flag. 
;
;
;==DRAWING RECTANGLES==
;The rectangle can have a border, or be a solid color. All border edges 
;can be drawn, or only certain border edges. 
;(Update 6/10/2017: It looks like a draw/draw parm is needed in order
;to draw a solid color rectangle other than black. Best option seems to be to tie the parm to both bit6 and bit 7 set. 
;The BMI at the top of .LOOP.ROW would need to check both bits before concluding erase mode was set. .BYTE0.NOT_EDGE would
;need to check both bits as well, and branch to a new label that would load a draw byte passed as a parm (presumably one of the existing parms).
;there may be other changed needed, these are notes from a quick review)
;
;The byte values that are drawn are stored in these parms:
;		top/bottom edge: DRAW.BYTE_VALUE.HORIZONTAL(2)*
;		left/right edge: DRAW.BYTE_VALUE.VERTICLE(2)*
;Two bytes for each are needed because the bit pattern for color lines alternates every other screen byte. 
;The draw routine loop draws two bytes at a time so that a different value can be used for each. Each byte has it's own
;exit check so I don't think there is a problem drawing lines that have an odd number of bytes, but whether the line starts on an 
;odd or even byte will effect which bytes values should be set in the parms. 
;
;The parameters are bit flags passed in a single byte via the ACC.
;			bit0 set = use hi-res page1
;			bit1 set = use page2
;			bit0&1 set = use both pages
;			bit2 set = top edge**
;			bit3 set = bottom edge**
;			bit4 set = left edge**
;			bit5 set = right edge**
;			bit6 = draw/erase mode
;			bit7 = erase mode
;
;*The verticle byte values are ignored for drawing a horizontal line. 
;     I haven't tried drawing a verticle line but I think it will work and it should ignore the horizontal byte values
;**The border edge flag and byte values are ignored when erase mode is set. 
;  but they must be set for drawing a line.
;
;-MODES
;
;DRAW/ERASE	 : draws the border of the rectangle and draws black inside the borders of the rectangle.
;ERASE	   	 : draws black ($00) in the entire rectangle, including the border, effectively erasing it's contents on black background screens.
;DEFAULT MODE: draws the border of the rectangle but leaves the hi-res contents inside the rectangle untouched. 
;
;See TEMPLATES below for examples
;
;=================================================================================


;====TEMPLATES====
@START

;Scenario 1
;(draw rectangle with border, erase inside using draw/erase mode)
@START
	; LDA #TWB.LW.INVENTORY.TOP_LINE		;load line in x register	
	; STA DRAW.START_LINE
	;
	; LDA #TWB.LW.INVENTORY.LEFT_SBYTE
	; STA DRAW.START_BYTE
	;
	; LDA #TWB.LW.INVENTORY.RIGHT_SBYTE
	; STA DRAW.STOP_BYTE
	;	
	; LDA #TWB.LW.INVENTORY.BOTTOM_LINE
	; STA DRAW.STOP_LINE
	;
	; LDA #$D5
	; STA DRAW.BYTE_VALUE.HORIZONTAL+$0
	;
	; LDA #$AA
	; STA DRAW.BYTE_VALUE.HORIZONTAL+$1
	;
	; LDA #$81	
	; STA DRAW.BYTE_VALUE.VERTICLE+$0
	;
	; LDA #$A0
	; STA DRAW.BYTE_VALUE.VERTICLE+$1
	;
; .DRAW
; ;(when both hi-res pages need to stay in sync, instead of drawing to both pages, sometimes it is best to the pages individually
; ;and flip pages inbetween. This way the draw isn't as noticable to the user)
		;
		; LDA PAGE.BACKGROUND ;set bit 0 & 1 with the desired hi-res page value
		; ORA #$7C 			;sets bits 2-5 so all border edges are drawn. set bit6 to enable draw/erase mode
	; JSR DRAW.LINE
	;
	; JSR FLIP.PAGE
		;			
		; LDA PAGE.BACKGROUND
		; ORA #$7C	
	; JSR DRAW.LINE
@END
;	
	
;Scenario 2
;(erase a rectangle shape, including the border at the edges, using erase mode) 
@START
	; LDA #TWB.LW.INVENTORY.TOP_LINE		;load line in x register	
	; STA DRAW.START_LINE
	;
	; LDA #TWB.LW.INVENTORY.LEFT_SBYTE
	; STA DRAW.START_BYTE
	;
	; LDA #TWB.LW.INVENTORY.RIGHT_SBYTE
	; STA DRAW.STOP_BYTE
	;	
	; LDA #TWB.LW.INVENTORY.BOTTOM_LINE
	; STA DRAW.STOP_LINE
	;
; .ERASE
		;
		; LDA #$83 ;set bit 0 & 1 with the desired hi-res page value, and set bit7 to enable erase mode. The border edge flag and byte values are ignored when erase mode is set. 
	; JSR DRAW.LINE
@END

;Scenario 3
;(draw rectangle with border but don't draw left edge, erase inside using draw/erase mode)
@START
	; LDA #TWB.LW.INVENTORY.TOP_LINE		;load line in x register	
	; STA DRAW.START_LINE
	;
	; LDA #TWB.LW.INVENTORY.LEFT_SBYTE
	; STA DRAW.START_BYTE
	;
	; LDA #TWB.LW.INVENTORY.RIGHT_SBYTE
	; STA DRAW.STOP_BYTE
	;	
	; LDA #TWB.LW.INVENTORY.BOTTOM_LINE
	; STA DRAW.STOP_LINE
	;
	; LDA #$D5
	; STA DRAW.BYTE_VALUE.HORIZONTAL+$0
	;
	; LDA #$AA
	; STA DRAW.BYTE_VALUE.HORIZONTAL+$1
	;
	; LDA #$81	
	; STA DRAW.BYTE_VALUE.VERTICLE+$0
	;
	; LDA #$A0
	; STA DRAW.BYTE_VALUE.VERTICLE+$1
	;
; .DRAW
; ;(when both hi-res pages need to stay in sync, instead of drawing to both pages, sometimes it is best to the pages individually
; ;and flip pages inbetween. This way the draw isn't as noticable to the user)
		;
		; LDA PAGE.BACKGROUND ;set bit 0 & 1 with the desired hi-res page value
		; ORA #$6C 			;sets bits 2-5 so all border edges are drawn except the left edge. set bit6 to enable draw/erase mode
	; JSR DRAW.LINE
	;
	; JSR FLIP.PAGE
		;			
		; LDA PAGE.BACKGROUND
		; ORA #$6C	
	; JSR DRAW.LINE
@END

;Scenario 4
;(draw a horizontal line)
@START
	; LDA #$00	
	; STA DRAW.START_LINE
	;
	; LDA #$01 ;(set to last line to draw + 1)
	; STA DRAW.STOP_LINE
	;
	; LDA #$00
	; STA DRAW.START_BYTE
	;
	; LDA #$20
	; STA DRAW.STOP_BYTE	
	;
	; LDA #$D5
	; STA DRAW.BYTE_VALUE.HORIZONTAL+$0
	;
	; LDA #$AA
	; STA DRAW.BYTE_VALUE.HORIZONTAL+$1
	;
; .DRAW
; ;(when both hi-res pages need to stay in sync, instead of drawing to both pages, sometimes it is best to the pages individually
; ;and flip pages inbetween. This way the draw isn't as noticable to the user)
		;
		;LDA #$07 ;BIT0-2 set: use both hi-res pages. draw top line of rectangle. ignore all other bit flags. See subroutine docs for full bit flag list.
	; JSR DRAW.LINE

@END

;Scenario 5
;(draw a verticle line)  *****UNTESTED*****
@START
	; LDA #$00							;load line in x register	
	; STA DRAW.START_LINE
	;
	; LDA #$00
	; STA DRAW.START_BYTE
	;
	; LDA #$20
	; STA DRAW.STOP_BYTE
	;	
	; LDA #$01 ;(set to last line to draw + 1)
	; STA DRAW.STOP_LINE
	;
	; LDA #$D5
	; STA DRAW.BYTE_VALUE.HORIZONTAL+$0
	;
	; LDA #$AA
	; STA DRAW.BYTE_VALUE.HORIZONTAL+$1
	;
; .DRAW
; ;(when both hi-res pages need to stay in sync, instead of drawing to both pages, sometimes it is best to the pages individually
; ;and flip pages inbetween. This way the draw isn't as noticable to the user)
		;
		;LDA #$013 ;BIT0-2, BIT4 set: use both hi-res pages. draw left line of rectangle. ignore all other bit flags. See subroutine docs for full bit flag list.
	; JSR DRAW.LINE

@END
	
@END



			
.START	
	;ACC = COMPOSITE PARAMETER (bit0 set = use hi-res page1 | bit1 set = use page2 | bit0&1 set = use both pages | bit2 set = top edge | bit3 set = bottom edge | bit4 set = left edge | bit5 set = right edge | bit6 = draw/erase mode | bit7 = erase mode)

	
.SPLIT.COMPOSITE.PARM
;COMPOSITE PARM = (bit0 set = use hi-res page1 | bit1 set = use page2 | bit0&1 set = use both pages | bit2 set = top edge | bit3 set = bottom edge | bit4 set = left edge | bit5 set = right edge | bit6 = draw/erase mode | bit7 = erase mode)
;DRAW.LINE.COMPOSITE.PARM = all bits
;DRAW.LINE.EDGE.FLAGS	  = bit2-5
;USE.PAGE				  = bit0-1

	PHA ;push composite parm to stack				
	STA DRAW.LINE.COMPOSITE.PARM ;(bit0 set = use hi-res page1 | bit1 set = use page2 | bit0&1 set = use both pages | bit2 set = top edge | bit3 set = bottom edge | bit4 set = left edge | bit5 set = right edge | bit6 = draw/erase mode | bit7 = erase mode)
	AND	#$3C ;mask-out bits 0,1,6,7, which are not edge draw parameters
	STA DRAW.LINE.EDGE.FLAGS ;$1 bytes. (bit 2 set = top edge, bit 3 set = bottom edge, bit 4 set = left edge, bit 5 set = right edge)
	PLA ;pull composite parm to stack
	AND #$03 ;mask-out bits 2-7
	STA USE.PAGE ;expects a value of $00-$03



			
.SAVE.REGISTERS
	TXA
	PHA
	TYA
	PHA

.INIT
	LDX DRAW.START_LINE
	LDY DRAW.START_BYTE
	
	LDA DRAW.STOP_BYTE
	SEC
	SBC #$01
	STA DRAW.STOP_BYTE.MINUS_1

	LDA DRAW.STOP_LINE
	SEC
	SBC #$01
	STA DRAW.STOP_LINE.MINUS_1
	

			
.DRAW.LOOP
	LDA USE.PAGE
	CMP #$03			;use both hi-res pages?
	BEQ .DRAW.BOTH.PAGES	

				

			
			
	;ACC = USE.PAGE
	JSR GET.LINE.ADDRESS1
	JMP .LOOP.ROW

	
.DRAW.BOTH.PAGES			
	LDA #$01	;get page1 address first
	JSR GET.LINE.ADDRESS1
	
	;CALCULATE PAGE2 ADDRESS 
	LDA LINE.BASE.ADDR1+$1
	CLC
	ADC #$20
	STA LINE.BASE.ADDR2+$1	;subtract $2000 from line address

	;SET LO ADDRESS
	LDA LINE.BASE.ADDR1
	STA LINE.BASE.ADDR2
	
	;**FALLS THROUGH**
	
.LOOP.ROW
;BYTE0
	LDA DRAW.LINE.COMPOSITE.PARM ;(bit0 set = use hi-res page1 | bit1 set = use page2 | bit0&1 set = use both pages | bit2 set = top edge | bit3 set = bottom edge | bit4 set = left edge | bit5 set = right edge | bit6 = draw/erase mode | bit7 = erase mode
	BMI .BYTE0.SET.ERASE

	; JSR DRAW.LINE.GET.POSITION
		; ;RETURN VALUE: ACC ($00 = draw position not on edge | bit 2 set = top edge, bit 3 set = bottom edge, bit 4 set = left edge, bit 5 set = right edge)

.BYTE0.GET.POSITION ;is current draw byte located on a rectangle edge? 
@START
	LDA #$00 ;set default return value
	
	CPX DRAW.START_LINE
	BEQ .BYTE0.TOP_EDGE
	CPX DRAW.STOP_LINE.MINUS_1
	BEQ .BYTE0.BOTTOM_EDGE
	CPY DRAW.START_BYTE
	BEQ .BYTE0.LEFT_EDGE
	CPY DRAW.STOP_BYTE.MINUS_1
	BEQ .BYTE0.RIGHT_EDGE
	JMP .BYTE0.GET.POSITION.EXIT
	
.BYTE0.TOP_EDGE
	ORA #$04
	JMP .BYTE0.GET.POSITION.EXIT
	
.BYTE0.BOTTOM_EDGE
	ORA #$08
	JMP .BYTE0.GET.POSITION.EXIT

.BYTE0.LEFT_EDGE
	ORA #$10
	JMP .BYTE0.GET.POSITION.EXIT

.BYTE0.RIGHT_EDGE
	ORA #$20
	
	;**FALLS THROUGH
	

.BYTE0.GET.POSITION.EXIT
	;ACC = RETURN VALUE ($00 = draw position not on edge | bit 2 set = top edge, bit 3 set = bottom edge, bit 4 set = left edge, bit 5 set = right edge)

@END
	
	;ACC = RETURN VALUE ($00 = draw position not on edge | bit 2 set = top edge, bit 3 set = bottom edge, bit 4 set = left edge, bit 5 set = right edge)		
	AND DRAW.LINE.EDGE.FLAGS ;$1 bytes. (bit 2 set = top edge, bit 3 set = bottom edge, bit 4 set = left edge, bit 5 set = right edge)
	BEQ .BYTE0.NOT_EDGE		;branch if the edge flag is not set for the edge the draw position is currently on
	AND #$0C ;mask-out all bits except for 2-3, which are the top/bottom edge flags
	BNE .DRAW.BYTE0.SET.HORIZONTAL
	;default case (if it's not horizontal then it should be verticle)
	JMP .DRAW.BYTE0.SET.VERTICLE 

.BYTE0.NOT_EDGE
	;draw position not on edge of rectangle	
	;is parm set to draw/erase?
	BIT	DRAW.LINE.COMPOSITE.PARM ;(bit0 set = use hi-res page1 | bit1 set = use page2 | bit0&1 set = use both pages | bit2 set = top edge | bit3 set = bottom edge | bit4 set = left edge | bit5 set = right edge | bit6 = draw/erase mode | bit7 = erase mode
	BVC	.BYTE0.INCREMENT	;branch if bit6 not set

	;**FALLS THROUGH**
	
;SET THE DRAW BYTE
;(base on the current draw position on the rectangle and whether
;erase mode is set)	
.BYTE0.SET.ERASE
	LDA #$00 ;set blank byte for erase
	JMP .DRAW.BYTE0
		
.DRAW.BYTE0.SET.VERTICLE
	LDA DRAW.BYTE_VALUE.VERTICLE+$0		;set value of byte to draw
	JMP .DRAW.BYTE0			
	
.DRAW.BYTE0.SET.HORIZONTAL
	LDA DRAW.BYTE_VALUE.HORIZONTAL+$0		;set value of byte to draw
	;**FALLS THROUGH**

.DRAW.BYTE0
		; ;ACC = byte value
	; JSR DRAW.LINE.PLOT_BYTE

	;ACC = byte value
	STA DRAW.BYTE_VALUE2 ;save byte value so we can load USE.PAGE below
	STA (LINE.BASE.ADDR1),Y	;PLOT (1st screen byte)

	LDA USE.PAGE
	CMP #$03
	BNE .DRAW.BYTE0.DONE

	LDA DRAW.BYTE_VALUE2 ;restore byte value saved above
	STA (LINE.BASE.ADDR2),Y	;PLOT (1st screen byte)
.DRAW.BYTE0.DONE

.BYTE0.INCREMENT
	INY  					;next screen byte
	CPY DRAW.STOP_BYTE 		;at rectangle bottom edge?
	BEQ .INCREMENT_LINE

;BYTE1
	LDA DRAW.LINE.COMPOSITE.PARM ;(bit0 set = use hi-res page1 | bit1 set = use page2 | bit0&1 set = use both pages | bit2 set = top edge | bit3 set = bottom edge | bit4 set = left edge | bit5 set = right edge | bit6 = draw/erase mode | bit7 = erase mode
	BMI .BYTE1.SET.ERASE
	
	; JSR DRAW.LINE.GET.POSITION
		; ;RETURN VALUE: ACC ($00 = draw position not on edge | bit 2 set = top edge, bit 3 set = bottom edge, bit 4 set = left edge, bit 5 set = right edge)

.BYTE1.GET.POSITION ;is current draw byte located on a rectangle edge? 
@START
	LDA #$00 ;set default return value
	
	CPX DRAW.START_LINE
	BEQ .BYTE1.TOP_EDGE
	CPX DRAW.STOP_LINE.MINUS_1
	BEQ .BYTE1.BOTTOM_EDGE
	CPY DRAW.START_BYTE
	BEQ .BYTE1.LEFT_EDGE
	CPY DRAW.STOP_BYTE.MINUS_1
	BEQ .BYTE1.RIGHT_EDGE
	JMP .BYTE1.GET.POSITION.EXIT
	
.BYTE1.TOP_EDGE
	ORA #$04
	JMP .BYTE1.GET.POSITION.EXIT
	
.BYTE1.BOTTOM_EDGE
	ORA #$08
	JMP .BYTE1.GET.POSITION.EXIT

.BYTE1.LEFT_EDGE
	ORA #$10
	JMP .BYTE1.GET.POSITION.EXIT

.BYTE1.RIGHT_EDGE
	ORA #$20
	
	;**FALLS THROUGH
	

.BYTE1.GET.POSITION.EXIT
	;ACC = RETURN VALUE ($00 = draw position not on edge | bit 2 set = top edge, bit 3 set = bottom edge, bit 4 set = left edge, bit 5 set = right edge)

@END

	;ACC = RETURN VALUE ($00 = draw position not on edge | bit 2 set = top edge, bit 3 set = bottom edge, bit 4 set = left edge, bit 5 set = right edge)		
	AND DRAW.LINE.EDGE.FLAGS ;$1 bytes. (bit 2 set = top edge, bit 3 set = bottom edge, bit 4 set = left edge, bit 5 set = right edge)
	BEQ .BYTE1.NOT_EDGE.BOTH_PAGES ;branch if the edge flag is not set for the edge the draw position is currently on
	AND #$0C ;mask-out all bits except for 2-3, which are the top/bottom edge flags
	;default case (if it's not horizontal then it should be verticle)
 	BNE .DRAW.BYTE1.SET.HORIZONTAL
	JMP .DRAW.BYTE1.SET.VERTICLE

.BYTE1.NOT_EDGE.BOTH_PAGES	
	;draw position not on edge of rectangle	
	;is parm set to draw/erase?
	BIT	DRAW.LINE.COMPOSITE.PARM ;(bit0 set = use hi-res page1 | bit1 set = use page2 | bit0&1 set = use both pages | bit2 set = top edge | bit3 set = bottom edge | bit4 set = left edge | bit5 set = right edge | bit6 = draw/erase mode | bit7 = erase mode
	BVC	.BYTE1.INCREMENT	;branch if bit6 not set
	
	;**FALLS THROUGH**

.BYTE1.SET.ERASE
	LDA #$00 ;set blank byte for erase
	JMP .DRAW.BYTE1
	
.DRAW.BYTE1.SET.VERTICLE
	LDA DRAW.BYTE_VALUE.VERTICLE+$1		;set value of byte to draw
	JMP .DRAW.BYTE1

.DRAW.BYTE1.SET.HORIZONTAL
	LDA DRAW.BYTE_VALUE.HORIZONTAL+$1		;set value of byte to draw
	
	;**FALLS THROUGH**
	
.DRAW.BYTE1
		; ;ACC = byte value
	; JSR DRAW.LINE.PLOT_BYTE

	STA DRAW.BYTE_VALUE2 ;save byte value so we can load USE.PAGE below
	STA (LINE.BASE.ADDR1),Y	;PLOT (1st screen byte)

	LDA USE.PAGE
	CMP #$03
	BNE .DRAW.BYTE1.DONE

	LDA DRAW.BYTE_VALUE2 ;restore byte value saved above
	STA (LINE.BASE.ADDR2),Y	;PLOT (1st screen byte)
.DRAW.BYTE1.DONE
	
.BYTE1.INCREMENT
	INY  					;next screen byte
	CPY DRAW.STOP_BYTE  	;at rectangle bottom edge?
	BEQ .BYTE1.INCREMENT.DONE
	JMP .LOOP.ROW
.BYTE1.INCREMENT.DONE
	
	;**FALLS THROUGH**
	
.INCREMENT_LINE
	LDY DRAW.START_BYTE		;reset screen byte to text window edge (it's actually the start byte in this routine)			
	INX						;next tile line	
	CPX DRAW.STOP_LINE		;LAST LINE?							
	BCS .EXIT				;IF YES, EXIT LOOP
	JMP .DRAW.LOOP			;IF NO, DRAW NEXT LINE (BCC: is X-REG < CPX value)	
.LOOP.ROW.DONE
	


.EXIT

;RESTORE REGISTERS		
	PLA
	TAY
	PLA
	TAX
	
	RTS

;***I decided not to use the subroutine versions to increase speed. 
;it saves 3 JSR/RTSs * number of bytes drawn. They would need some slight modications
;to be used in the future, I think to the way USE.PAGE is parsed. 

;DRAW.LINE.GET.POSITION
@START
; ;PARAMETERS: none
; ;ENTRANCE: DRAW.LINE
; ;RETURN: ACC ($00 = draw position not on edge | bit 2 set = top edge, bit 3 set = bottom edge, bit 4 set = left edge, bit 5 set = right edge)

	; LDA #$00
	
	; CPX DRAW.START_LINE
	; BEQ .TOP_EDGE
	; CPX DRAW.STOP_LINE.MINUS_1
	; BEQ .BOTTOM_EDGE
	; CPY DRAW.START_BYTE
	; BEQ .LEFT_EDGE
	; CPY DRAW.STOP_BYTE.MINUS_1
	; BEQ .RIGHT_EDGE
	; JMP .EXIT
	
; .TOP_EDGE
	; ORA #$04
	; JMP .EXIT
	
; .BOTTOM_EDGE
	; ORA #$08
	; JMP .EXIT

; .LEFT_EDGE
	; ORA #$10
	; JMP .EXIT

; .RIGHT_EDGE
	; ORA #$20
	
	; ;**FALLS THROUGH
	

; .EXIT
	; ;ACC = RETURN VALUE ($00 = draw position not on edge | bit 2 set = top edge, bit 3 set = bottom edge, bit 4 set = left edge, bit 5 set = right edge)
	
	; RTS
	
	
@END

;DRAW.LINE.PLOT_BYTE
@START
; ;PARAMETERS: byte value to plot
; ;RETURN: updated video memory
	
	; STA DRAW.BYTE_VALUE2
	; STA (LINE.BASE.ADDR1),Y	;PLOT (1st screen byte)

	; LDA USE.PAGE
	; AND #$03 ;mask-out bits all bits except bit2, which has to be set for USE.PAGE to be set to use both pages
	; CMP #$03
	; BNE .EXIT

	; LDA DRAW.BYTE_VALUE2
	; STA (LINE.BASE.ADDR2),Y	;PLOT (1st screen byte)

; .EXIT

	; RTS
	
@END	

@END

;=================DEFINE VARIABLES===============

;**see "GENERAL ROUTINES (LOW LEVEL)" section in offloaded_variables.ASM
 
 
 ;**OPT** Memory. Go through all library routines and disable any subroutines that aren't in use. 
 ;Search for the routine name in all open files and if there are no JSRs or JMPs to the routine then 
 ;disable it. 