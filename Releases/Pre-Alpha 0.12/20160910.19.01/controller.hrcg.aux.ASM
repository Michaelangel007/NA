 ;************************INCLUDE FILE*****************************
;(do not assemble stand-alone. For a stand-alone version of this routine see examples/rwts2)

				.TF     CONT.HRCG.BIN,BIN
				.OR		$300			**Always put before .TF directive and never use again in program

				
;=====================SUBROUTINE DOCUMENTATION====================================
;
;CUSTOM CONROLLER FOR HI-RES CHARACTER GENERATION
;
;To update on a master disk image......
;first delete the existing
;controller file "CONT" from the disk image. Then, use SBASM
;to compile this file and use AC Commander to add it to the master
;disk image. Verify the hello program BLOADs this file at $300.
;
;The apple commander line was in go2b.bat and I just commented it out
;look for the one involving the file "CONT"
;
;Note: The entire $300 page is not available for code. Pushing past a certain address will FUBAR the system.
;I don't recall exactly what that address is, I noted it somewhere. Probably in one of the files
;related to the development of this controller. 
;
;
;BootLoader Note:
;
;For reasons I don't understand, when CONT.HCRG is run without DOS, the JMP COUT1 at $038A
;doesn't return to then next instruction (RTS at $038D) as it normally does. I observed this
;by launching the game in a bootloader, and putting a BRK just before the HRCG code. Then 
;I did $300G from the monitor and saw that it breaks at $3EC, which of course refers to the BRK
;at $3EA. I ran the game again and before doing $300G I set $3EA = $60. The result was no break
;and I was returned to the monitor prompt, which is what normally would happen. 
;
;I then added a patch to LOADER.BIN to set $3EA = $60 after loading this controller. The result was
;the HRCG code in the game then worked just fine. 
;
;=================================================================================



HOOK	;=====CHANGES DOS I/O PATH TO ENTRY ROUTIE BELOW=====
	LDA #ENTRY		;PRODUCES LOW BYTE
	STA CSW
	LDA /ENTRY		;PRODUCES HIGH BYTES
	STA CSW+$1
	JMP VECT	
ENTRY

	CMP	#$A0		;IS CONTROL CHACTER IN ACC?
			;LDA #$BD		;PRODUCES LOW BYTE
			;STA CSW
			;LDA #$9E	;PRODUCES HIGH BYTES
			;STA CSW+$1
			;JMP VECT
			;JMP $FF69
			;BRK
	BCC	OUT			;IF YES, EXIT AND PASS IT TO COUT1
	PHA				;STORE CHAR
	; AND #$7F		;CLEAR HI BIT
	; STA POSN
	; LDA #$00
	; STA POSN+$1
	TYA
	PHA				;SAVE Y-REG
	
; CALC1	
	; SEC
	; LDA POSN
	; SBC #$20
	; STA POSN		; CHAR > 96
	; ASL	POSN		; *2 = CHAR < 192
	; ASL	POSN		; *4 < 384
	; ROL POSN+$1		
	; ASL POSN		; *8 < 768
	; ROL POSN+$1

;POSN = (ASCII VALUE - $20)*8byt PER CHAR	

	; CLC
	; LDA #TABLE		;LOW BYTE
	; ADC POSN
	; STA POSN
	; LDA /TABLE		;HIGH BYTE
	; ADC	POSN+$1
	; STA POSN+$1		;POSN=POSN + TABLE ADDR
	
;	.BS $24, $EA
	
	LDA #HRCG.BUFFER
	STA POSN
	LDA /HRCG.BUFFER
	STA POSN+$1
	;LDA TEXT+

	;JSR $FF59
	
;	.BS $7, $EA
	
CALC2
	CLC
	LDA BASL
	ADC CH
	STA SCRN
	STA SCRN2		;PAGE2
	LDA BASL+$1
	ADC	#$1C
	STA SCRN+$1		;SCRN = BASL + CH + $1C00
	ADC #$20
	STA SCRN2+$1	;PAGE2

GETBYTE
	LDY #$00
G1	
	LDA (POSN),Y
	STA (SCRN),Y
	STA (SCRN2),Y
INC
	INY
	CLC
	LDA SCRN
	ADC #$FF
	STA SCRN
	STA SCRN2		;PAGE2
	LDA SCRN+$1
	ADC #$03
	STA SCRN+$1		;SCRN SCRN + $3FF
 	ADC #$20
	STA SCRN2+$1	;PAGE2
	
	CPY #$08		;DONE?
	BCC G1			;IF NO, CONTINUE LOOP

	PLA				;IF YES, RESTORE REGISTERES, PASS CHAR TO COUT1, AND EXIT
	TAY			
	PLA				;RESTORE ORIGINAL CHAR VALUE TO ACC
OUT

	;JSR COUT1
	JMP COUT1	
		; PLA
		; STA $A000
		; PLA $A001
		; JSR $FF59
	;JMP COUT.RETURN
	
	RTS ;this RTS doesn't seem to do anything. If I 
		;comment it out everything works fine, but if I
		;jump to monitor (via JMP $FF59) just after the JMP COUT1, I get the monitor prompt
		;which suggets that the JMP COUT doesn't return control back to the calling routine. 
		;this would suggest that the RTS is necessary, but as previously noted, commenting it out 
		;doesn't seem to break anything.
		;This line is listed as CHK, an opcode I've never heard of, in the original source, so that just makes it all more confusing. 
	
	
	
CSW		.EQ		$36
BASL	.EQ		$28
CH		.EQ		$24
POSN	.EQ		$3C		;(BAS2)
SCRN	.EQ		$3E		;(A4)
SCRN2	.EQ		$EB
VECT	.EQ		$3EA
COUT1	.EQ		$FDF0
TABLE	.EQ		$0C00

TEXT				.EQ $C051
PAGE1				.EQ	$C054

