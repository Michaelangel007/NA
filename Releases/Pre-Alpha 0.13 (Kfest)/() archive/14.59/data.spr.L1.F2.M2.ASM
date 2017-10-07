;************************INCLUDE FILE*****************************
;(do not assemble stand-alone. For a stand-alone version of this routine see examples/rwts2)
				.TF     DATA.SPR.L1.F2.M2.BIN,BIN
				.OR		$A900			**Always put before .TF directive and never use again in program
				.EB		OFF
				
;=====================DATA FILE DOCUMENTATION====================================
;
;==Overview==
;This data files stores the map objects data for the location described in the .TF filename above.
;
;==Order of Arrays==
;The order of the data arrays is critial. 
;They must be in the following order:
;
;DATA.SPR.SURFACE.MOB
;DATA.SPR.SURFACE.GENERAL
;DATA.SPR.SURFACE.NPC
;NPC.L1.F1.SCHEDULE
;NPC.L1.F1.SCHEDULE2
;
;Or, instead of NPC scheduled arrays, FILLER.DATA.SPR.SURFACE if on any map other than a building
;
;==Size of Data==
;PRODOS.IO can only handle writes that are for an even number of pages (i.. 2,4,6,8,A)
;If an odd number of pages is used, the driver will round up the to the nearest even page and clobber data in the file. 
;As an alternative to this file containing an even number of pages of data, filler can also be used, in which case the driver's rounding of the write
;size has no impact on real data.  
;=================================================================================
					

					
	;****ORDER OF ARRAYS IS CRITICAL (see data file documentation section above)

	;****THIS FILE MUST BE AN EVEN # OF PAGES OR END WITH FILLER (see ==Size of Data== in the data file documentation section above)



; NPC.SCHEDULE Datagram								
; Byte 0	Byte 1			Byte 2				Byte 3				Byte 4					Byte 5		Byte 6		Byte 7							X+$0
; Anchor #	NPC Record #	Start Time: Hour	Start Time: Minute	Non-road/floor weight	X ofAnchor	Y ofAnchor	at-anchor move routine flag		Stop value
;see Map Objects spreadsheet for authoratative version
;						   	 	1st record			    2nd record				3rd record				4th Record				5th Record				 6th Record
NPC.L1.F2.SCHEDULE		.HS		00.00.09.00.01.27.22.FF.01.00.19.00.00.25.26.00.02.18.08.00.04.11.26.FF.04.18.22.00.02.09.26.00.02.20.07.00.03.11.26.FF.03.20.22.00.02.0D.26.00.05.70.08.00.06.0A.15.FF.06.70.20.00.05.0A.13.00.05.B0.08.00.07.0A.15.FF.07.B0.23.00.05.0A.19.00.05.B8.08.00.08.0A.15.FF.08.B8.23.00.05.0A.1B.00.FF


		;**This .NO fills any unused space in the 3 pages allocated for the NPC.SCHEDULE arrray. This is needed to
		;ensure the subsquent arrays end up in the correct memory locations
	.NO $AC00

	
DATA.SPR.L1.F2.MOB		.HS		00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00
DATA.SPR.L1.F2.GENERAL	.HS		26.23.0D.10.00.00.00.00.0D.24.0D.10.0B.24.0D.10.00.00.00.00.09.1E.0D.10.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00


;NPC Movement Testing
;DATA.SPR.SURFACE.NPC Datagram
;Byte 0	Byte 1	Byte 2		Byte 3		Byte 4			Byte 5			Byte 6				Byte 7
;GMAP.X	GMAP.Y	Tile_type	Set to $03	Active Anchor	Path Index($00)	In Transit? ($00)	at-anchor move routine flag
;see Map Objects spreadsheet for authoratative version


;BLANK
;DATA.SPR.L1.F2.NPC 	.HS		00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00
;NPC.L1.F2.SCHEDULE		.HS		00.00.00.00.00.00.00.00.FF



;ROUND 3 (ALLWORKS)
DATA.SPR.L1.F2.NPC		.HS		00.00.B4.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.B3.00.00.00.00.00.00.00.98.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.98.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.98.00.00.00.00.00.00.00.98.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00

		;**this .NO makes sure NPC.XX.SCHEDULE doesnt' use more than the 3 pages of memory reserved for it		
	.NO $AF00,$00 ;FILLER TO THE UPPER LIMIT OF OF THE MEMORY ALLOCATED FOR THIS DATA FILE

