				.TF     DUMMY.FILE.BIN,BIN
				.OR		$0000			**Always put before .TF directive and never use again in program

;NOT USED IN ANY OF THE PROGRAMS IN THE TEST FOLDER
;(i.e. it is safe to modify for testing in the game engine)

		NOP
		
		; .BS $1000,$AA
		; .BS $1000,$BB
		; .BS $1000,$CC

; ;simulated items file
		; .BS $1F0,$42
		; .BS $1000,$AB
		; .BS $1000,$BB
		; .BS $1000,$CC
		; .BS $1000,$DD
		; .BS $1000,$EE
		; .BS $1000,$FF

;test_data .hs 00.01.02.03.04.05.06.07.08.09.0A.0B.0C.0D.0E.0F
		
		.BS $E618,$AA
		; .BS $1000,$BB

		
;FILLER_DATA		.BS $1000,$AA

