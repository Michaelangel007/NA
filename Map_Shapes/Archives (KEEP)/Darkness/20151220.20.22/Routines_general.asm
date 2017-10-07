;************************INCLUDE FILE*****************************
;(do not assemble stand-alone. For a stand-alone version of this routine see examples/rwts2)


KEYIN 		;============WAITS FOR A KEYPRESS=======
;PARAMETERS: NONE
;RETURN VALUE: LAST KEY PRESS

.KEYIN
	LDA $C000
    BPL .KEYIN
    STA $C010               ;CLR LAST KEY
	RTS

;========DEFINE VARIABLES=========

WAIT				.EQ	$FCA8
DELAY				.EQ	$41				;#CONSTANT
APPLE_BELL			.EQ $FF3A			


