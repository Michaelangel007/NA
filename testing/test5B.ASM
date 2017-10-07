                .CR     6502            Use 6502 overlay
				.OR		$6300			**Always put before .TF directive and never use again in program
				.TF     TEST.BIN,BIN
				.EF		errors
				.LF		C:\MY_CODE\LIST
;				.LI 	OFF				Switches the assembler machine code conversion listing off so errors are easier to see				
;				.IN 	C:\MY_CODE\source_code\routines_common


;DEMO: THE LAST ITERATION (#5) BECAME THE RANDOM.INIT & RANDOM.8 ROUTINES 
;IN ROUTINES_MATH.ASM.
;THIS ITERATION (#6), IS DEMO CALL TO RANDOM.INIT & RANDOM.8 IN ROUTINES_MATH.ASM.
;
;FOR REFERENCE, THE RANDOM.8 ROUTINE FUNCTION IS:
;8-BIT RANDOM NUMBER RETURNED IN RND, WITH RANGE CONTROL VIA RND.LO, RND.HI
;THE ROUTINE IS ACTUALLY SUPPOSED TO BE 16-BIT BUT RND+$1 IS ALWAYS $00 OR $01. 
;
;
;
;DRIVER1 MAKES SEVERAL ONE-OFF CALLS
;DRIVER2 FILLS AN ARRAY (TAKES AWHILE TO RUN)
;ORIGINAL SOURCE: http://6502.org/source/integers/random/random.html search for "random8". The RAND routine used was one directly above the "RANDOM8" code listing
 		
		JMP DRIVER2
		
DRIVER1
			JSR RANDOM.8.INIT
			
			LDA #$01
			STA RND.LO

			LDA #$10
			STA RND.HI
			
			JSR RANDOM.8
			STA $9000
			
			JSR RANDOM.8
			STA $9001
			
			JSR RANDOM.8
			STA $9002			
			
			JSR RANDOM.8
			STA $9003
			
			JSR RANDOM.8
			STA $9004
			
			JSR RANDOM.8
			STA $9005
	
			JSR RANDOM.8
			STA $9006
	
			JSR RANDOM.8
			STA $9007

			JSR RANDOM.8
			STA $9008
			
			JSR RANDOM.8
			STA $9009			
			
			BRK

DRIVER2

		JSR RANDOM.8.INIT
		
		LDA #$01
		STA RND.LO

		LDA #$10
		STA RND.HI
			
		LDA #$00			;FILL DESTINATION LO BYTE
		STA $EB
		LDA #$70			;FILL DESTINATION HO BYTE
		STA $EC
		
		LDY #$00			;SET INDEX
.LOOP		
		JSR RANDOM.8
		STA ($EB),Y

		INY
		BNE .LOOP
		INC $EC
		LDA $EC
		CMP #$71
		BEQ .EXIT
		JMP .LOOP
		
.EXIT		
	BRK
		 
;
	.IN	 C:\MY_CODE\source_code\ROUTINES_MATH	 
	
;======DEFINE VARIBLES======

