;************************INCLUDE FILE*****************************
;(do not assemble stand-alone. For a stand-alone version of this routine see examples/rwts2)

				.OR		$2000			**Always put before .TF directive and never use again in program
				.TF     P.LOADER.BIN,BIN

	LDA #$AA
	BRK
	