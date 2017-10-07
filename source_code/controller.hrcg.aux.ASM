 ;************************INCLUDE FILE*****************************
;(do not assemble stand-alone.  )

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
;===Update (3/6/2017): Inverse character support
;*added the following line to GETBYTE routine below.
;		EOR COUT_CHAR_TYPE ;($00 = normal, $7F = inverse)				
;
;*COUT_CHAR_TYPE is set by COUT (my HRCG wrapper, not the ROM COUT, which my code calls via the label COUT.ADDRESS)
;*Note1: that this inverse character implimentation is different than ROM COUT which uses $7F for flashing characters and $3F for inverse. 
;		;I think this is because it uses the parameter mask as an AND mask on the ASCII value of the character and it uses the ascii value
;		of the character (I think) to deterine the value of the XOR mask to apply to the character shape table. 
;		Since I'm only supporting inverse character I thought it was easier to just apply $7F as the XOR mask and call it a day. 
;*Note2: the parameter mask used by ROM COUT is stored in $32. However, COUT_CHAR_TYPE is not = $32 because doing so would cause problems with PREP.BRK and FULL.BRK
;		after the ROM COUT vector is reconnected. The problem is that since I'm using $00 as the default mask value, storing it in $32 would cause ROM COUT to 
;		calculated the wrong ASCII code since it uses the mask differently than my COUT as described above. 
;=================================================================================


;**OPT** Memory. The character set takes up $300 bytes in aux memory. Consider whether the entire character set is needed.
;Review ascii table. There might be a bunch of unnecessary stuff in the lower or upper ends of the range. 

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
	EOR COUT_CHAR_TYPE ;($00 = normal, $7F = inverse)				
			LDX HRCG.PAGES
			CPX #$02
			BEQ .DRAW.PAGE2
	STA (SCRN),Y ;draw page 1
			CPX #$01
			BEQ .DRAW.PAGE2.DONE	
.DRAW.PAGE2
	STA (SCRN2),Y ;draw page2
.DRAW.PAGE2.DONE
	
INC
	INY
	CLC
	LDA SCRN
	ADC #$FF

	STA SCRN		;PAGE1 ADDRESS (LO)
	STA SCRN2		;PAGE2 ADDRESS (LO)
	LDA SCRN+$1
	ADC #$03
	STA SCRN+$1		;SCRN SCRN + $3FF
 	ADC #$20
	STA SCRN2+$1	;PAGE2 ADDRESS (HO)
	
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


			; STA TEMP
			; LDA TROUBLESHOOTING.HOOK
			; CMP #$01
			; BNE .TEMP
			; JSR KEYIN
			; PLA
			; TAX
			; PLA
			; TAY
			; LDA #$AC
			; ;LDX SAVED.ACC.LOCAL		;restore output character
			; ;LDX HRCG.PAGES
			; ; LDX $C012		;ROM/BSR soft-switch flag (bit7 = 1: BSR, bit7=0 ROM)
			; ; LDY $C011		;BANK1/BANK2 soft-switch flag (bit7 = 1: BANK2, bit7=0 BANK1)
			; ; LDX BSR.STATE
			; ; LDY BANK.STATE
			; JSR PREP.BRK
			; BRK
; .TEMP
			; LDA TEMP


			
	RTS ;this RTS doesn't seem to do anything. If I 
		;comment it out everything works fine, but if I
		;jump to monitor (via JMP $FF59) just after the JMP COUT1, I get the monitor prompt
		;which suggets that the JMP COUT doesn't return control back to the calling routine. 
		;this would suggest that the RTS is necessary, but as previously noted, commenting it out 
		;doesn't seem to break anything.
		;This line is listed as CHK, an opcode I've never heard of, in the original source, so that just makes it all more confusing. 
	
	
	
CSW		.EQ		$36
BASL	.EQ		$28		;*appears to contain the LO/HO address of the text screen line associated with VTAB. See page 302 in Assemblylines cookbook for a chart with the memory address for each line of the text screen.
CH		.EQ		$24		;*appears to equal HTAB+1
POSN	.EQ		$3C		;(BAS2)   ;pointer to the text character shape table
SCRN	.EQ		$3E		;(A4)     ;hi-res screen memory address (LO/HO)
SCRN2	.EQ		$EB
VECT	.EQ		$3EA
COUT1	.EQ		$FDF0
TABLE	.EQ		$0C00

TEXT				.EQ $C051
PAGE1				.EQ	$C054

;*I'm pretty sure that these values are updated from HTAB and VTAB when JSR	UPDATE.CHAR.POS is executed, which 
;I typically do in the game code before a call to COUT or any of its' wrapper routines. 