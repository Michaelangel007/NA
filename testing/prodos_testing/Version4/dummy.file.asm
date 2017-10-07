;open/read/write binary file in ProDOS filesystem
;copyright (c) Peter Ferrie 2013-16
				
				.OR		$6000			**Always put before .TF directive and never use again in program
				.TF     DUMMY.FILE.BIN,BIN


FILLER_DATA		.BS $600,$AA

