;************************INCLUDE FILE*****************************
;(do not assemble stand-alone. For a stand-alone version of this routine see examples/rwts2)
				.OR		$1F04			**Always put before .TF directive and never use again in program
				.TF     DATA.MAP.L1.BIN,BIN


;see DATA FILE DOCUMENTATION for information on the purpose of the filler data.
FILLER.MAP.L1		.HS 00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00.00

;=====================DATS FILE DOCUMENTATION====================================
;
;For general data file documentation, see data.shapes.building.ASM
;
;This data file contains uncompressed world map tile data which is compressed and loaded
;into auxiliary memory by WORLD.ZONE.COMPRESS2. 
;
;The source of this data is the map editor spreadsheet 
;(my_code/RPG Project/Map)
;
;=================================================================================

;MUST START AT $2000 AND END AT $28FF
;(it starts at $2000 as a result of filler data above)
L1.BUILDING_0			.HS	8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.31.31.31.31.31.31.31.31.8B.8B.8B.8B.8B.8B.8B.8B.31.35.35.61.31.35.35.61.8B.8B.8B.8B.8B.8B.8B.8B.31.62.62.61.31.61.62.62.8B.8B.8B.8B.8B.8B.8B.8B.31.61.62.62.31.31.31.35.8B.8B.8B.8B.8B.8B.8B.8B.31.61.61.62.62.5C.31.31.8B.8B.8B.8B.8B.8B.8B.8B.31.31.31.31.31.5C.5C.5C.8B.8B.8B.8B.8B.8B.8B.8B.31.34.34.34.34.5C.5C.5C.8B.8B.8B.8B.8B.8B.8B.8B.31.31.31.31.31.5C.5C.5C.8B.8B.8B.8B.8B.8B.8B.8B.31.61.61.61.61.5C.5C.5C
L1.BUILDING_1			.HS	8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.31.31.31.31.31.31.31.31.31.31.31.31.31.31.31.31.61.61.35.35.61.35.35.61.35.35.61.35.35.61.35.35.62.62.62.61.61.61.62.61.61.62.61.61.62.62.62.61.35.62.62.62.62.62.62.62.62.62.62.62.62.62.35.35.31.31.31.31.31.31.31.62.31.31.31.31.31.31.31.31.34.47.34.34.34.34.34.5C.34.34.34.47.34.34.34.34.5C.5C.5C.5C.5C.5C.5C.5C.5C.5C.5C.5C.5C.5C.5B.5B.5C.3A.5C.5C.5C.5C.5C.5C.5C.5C.5C.5C.5C.5C.5B.5B.5C.5C.5C.5C.5C.5C.5C.5C.5C.5C.5C.5C.5C.5C.5B.5B
L1.BUILDING_2			.HS	8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.31.31.31.31.31.31.31.31.31.8B.8B.8B.8B.8B.8B.8B.31.61.61.61.61.61.61.61.31.8B.8B.8B.8B.8B.8B.8B.31.62.62.62.62.62.62.61.31.8B.8B.8B.8B.8B.8B.8B.31.35.35.61.61.31.62.62.31.8B.8B.8B.8B.8B.8B.8B.31.31.31.31.31.31.31.62.31.8B.8B.8B.8B.8B.8B.8B.34.34.34.31.3A.31.62.62.31.8B.8B.8B.8B.8B.8B.8B.5B.5B.34.31.62.61.62.61.31.8B.8B.8B.8B.8B.8B.8B.5B.5B.47.31.62.61.62.61.31.8B.8B.8B.8B.8B.8B.8B.5B.5B.5B.5B.62.62.62.61.31.8B.8B.8B.8B.8B.8B.8B
L1.BUILDING_8			.HS	8B.8B.8B.8B.8B.8B.8B.8B.31.61.61.61.31.34.5C.5C.8B.8B.8B.8B.8B.8B.8B.8B.31.35.35.61.31.47.5C.5C.8B.8B.8B.8B.8B.8B.8B.8B.31.31.31.31.31.34.5C.5C.8B.8B.8B.8B.8B.8B.8B.8B.31.45.61.61.61.5C.5C.5C.8B.8B.8B.8B.8B.8B.8B.8B.31.61.61.61.61.5C.5C.5C.8B.8B.8B.8B.8B.8B.8B.8B.31.61.61.62.62.5C.5C.5C.8B.8B.8B.8B.8B.8B.8B.8B.31.62.62.62.31.34.5C.5C.8B.8B.8B.8B.8B.8B.8B.8B.31.62.31.31.31.34.5C.5C.8B.8B.8B.8B.8B.8B.8B.8B.31.62.31.31.31.34.5C.5C.8B.8B.8B.8B.8B.8B.8B.8B.31.62.62.3A.31.34.5C.5C.8B.8B.8B.8B.8B.8B.8B.8B.31.31.31.31.31.34.5C.5C.8B.8B.8B.8B.8B.8B.8B.8B.31.61.61.61.31.34.5C.5C.8B.8B.8B.8B.8B.8B.8B.8B.31.61.31.61.31.34.5B.5C.8B.8B.8B.8B.8B.8B.8B.8B.31.61.31.3A.31.47.5B.5B.8B.8B.8B.8B.8B.8B.8B.8B.31.61.31.61.62.5B.5B.5B.8B.8B.8B.8B.8B.8B.8B.8B.31.61.61.61.62.5B.5B.5B
L1.BUILDING_9			.HS	5C.34.34.34.34.34.34.34.34.34.34.34.34.34.5B.5B.5C.34.31.31.31.31.31.31.31.31.31.31.31.31.5B.5B.5C.34.31.61.61.61.61.61.61.3A.61.61.61.31.5B.5B.5C.34.31.61.61.31.61.61.31.31.31.61.61.31.5B.5B.5C.34.31.61.61.31.61.61.61.62.61.61.47.31.5B.5B.5C.34.31.61.31.31.61.61.61.62.3A.61.61.31.5B.5B.5C.34.31.61.61.31.61.61.61.62.62.62.62.62.5B.5B.5C.34.31.61.61.31.61.61.61.62.61.61.61.31.5B.5B.5C.34.31.61.35.31.61.61.62.62.61.61.61.31.5B.5B.5C.34.31.61.35.31.47.3A.62.61.3A.61.47.31.5B.5B.5C.34.31.31.31.31.31.31.62.31.31.31.31.31.5B.5B.5C.34.34.34.34.34.34.5B.5B.5B.34.34.34.34.5B.5B.5C.34.34.34.34.34.34.5B.5B.5B.34.34.34.34.5B.5B.5C.34.34.34.3A.34.34.5B.5B.5B.34.34.34.34.5B.5B.5B.5B.5B.5B.5B.5B.5B.5B.5B.5B.5B.5B.5B.5B.5B.5B.5B.5B.5B.5B.5B.5B.5B.5B.5B.5B.5B.5B.5B.5B.5B.5B
L1.BUILDING_A			.HS	5B.5B.5B.5B.61.61.61.61.31.8B.8B.8B.8B.8B.8B.8B.31.31.31.31.31.31.31.31.31.8B.8B.8B.8B.8B.8B.8B.31.3A.61.31.3A.31.61.3A.31.8B.8B.8B.8B.8B.8B.8B.31.61.61.31.61.31.61.61.31.8B.8B.8B.8B.8B.8B.8B.31.31.61.31.61.31.61.31.31.8B.8B.8B.8B.8B.8B.8B.31.61.61.61.61.61.61.61.31.8B.8B.8B.8B.8B.8B.8B.31.61.61.61.61.61.31.61.31.8B.8B.8B.8B.8B.8B.8B.31.61.61.61.62.62.31.3A.31.8B.8B.8B.8B.8B.8B.8B.31.61.61.61.62.61.31.61.31.8B.8B.8B.8B.8B.8B.8B.31.61.61.3A.62.61.61.61.31.8B.8B.8B.8B.8B.8B.8B.31.31.31.31.62.31.31.31.31.8B.8B.8B.8B.8B.8B.8B.34.34.34.5B.5B.5B.47.34.31.8B.8B.8B.8B.8B.8B.8B.34.34.5B.5B.5B.34.34.34.31.8B.8B.8B.8B.8B.8B.8B.34.5B.5B.5B.34.34.34.34.31.8B.8B.8B.8B.8B.8B.8B.5B.5B.5B.5B.5C.5C.34.34.31.8B.8B.8B.8B.8B.8B.8B.5B.5B.5B.5C.5C.5C.5C.34.31.8B.8B.8B.8B.8B.8B.8B
L1.BUILDING_10			.HS	8B.8B.8B.8B.8B.8B.8B.8B.31.62.62.62.62.61.31.5B.8B.8B.8B.8B.8B.8B.8B.8B.31.62.31.61.61.61.31.34.8B.8B.8B.8B.8B.8B.8B.8B.31.62.31.61.61.61.31.31.8B.8B.8B.8B.8B.8B.8B.8B.31.61.31.61.61.61.31.61.8B.8B.8B.8B.8B.8B.8B.8B.31.61.61.61.61.47.31.61.8B.8B.8B.8B.8B.8B.8B.8B.31.61.3A.61.61.31.31.61.8B.8B.8B.8B.8B.8B.8B.8B.31.61.61.61.61.61.61.61.8B.8B.8B.8B.8B.8B.8B.8B.31.31.31.31.31.31.31.31.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B
L1.BUILDING_11			.HS	5B.5B.5B.5B.5B.5B.5B.5B.5B.5B.5B.5B.5B.5B.5B.5B.34.34.34.34.34.34.34.5B.5B.5B.34.34.34.34.34.34.31.31.34.34.34.34.34.5B.5B.5B.34.34.34.34.34.34.61.31.34.34.34.34.34.5B.5B.5B.47.34.34.34.34.34.3A.31.34.34.34.34.47.5B.5B.5B.34.34.34.34.34.34.61.31.34.34.34.31.31.34.34.34.31.31.34.34.34.34.45.31.34.34.31.31.3A.34.34.34.3A.31.31.34.34.34.31.31.31.31.31.34.34.34.34.34.3A.34.31.31.31.31.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B
L1.BUILDING_12			.HS	5B.34.34.34.34.5C.5C.34.31.8B.8B.8B.8B.8B.8B.8B.34.34.34.31.31.62.61.31.31.8B.8B.8B.8B.8B.8B.8B.34.34.34.31.62.62.61.45.31.8B.8B.8B.8B.8B.8B.8B.34.34.34.31.62.61.61.61.31.8B.8B.8B.8B.8B.8B.8B.34.34.34.31.62.61.61.61.31.8B.8B.8B.8B.8B.8B.8B.34.34.34.31.62.31.31.31.31.8B.8B.8B.8B.8B.8B.8B.34.34.34.31.62.62.3A.61.31.8B.8B.8B.8B.8B.8B.8B.31.31.31.31.31.31.31.31.31.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B.8B

@END
