;************************INCLUDE FILE*****************************
;(do not assemble stand-alone. For a stand-alone version of this routine see examples/rwts2)
				.TF     COMPRESSED.DATA.MAP.UNDERMAP_LV1.BIN,BIN
				.OR		$0200			**Always put before .TF directive and never use again in program
;===================UNDERMAP ZONE DATA=================
@START
;
;=====================DATA FILE DOCUMENTATION====================================
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
;
;
UNDERMAP.L1_0			.HS	.00.1E.00
UNDERMAP.L1_1			.HS	.00.1E.00
UNDERMAP.L1_2			.HS	.00.1E.00
UNDERMAP.L1_3			.HS	.00.1E.00
UNDERMAP.L1_4			.HS	.00.1E.00
UNDERMAP.L1_5			.HS	.00.1E.00
UNDERMAP.L1_6			.HS	.00.1E.00
UNDERMAP.L1_7			.HS	.00.1E.00
UNDERMAP.L1_8			.HS	.00.1E.00
UNDERMAP.L1_9			.HS	.00.1E.00
UNDERMAP.L1_A			.HS	.00.1E.00
UNDERMAP.L1_B			.HS	7D.7D.7D.7D.7D.88.88.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.88.88.88.88.88.88.88.7D.7D.7D.7D.7D.7D.7D.7D.88.88.88.88.88.88.88.88.7D.7D.7D.7D.7D.7D.7D.88.88.88.88.88.88.88.88.88.88.7D.7D.7D.7D.7D.7D.88.88.88.88.88.88.88.88.88.7D.7D.7D.7D.7D.7D.88.88.88.88.88.88.88.88.7D.80.7D.7D.7D.7D.7D.7D.7D.88.88.88.88.88.88.7D.7D.82.81.84.7D.7D.7D.7D.7D.80.7D.7D.7D.7D.80.7D.7D.7D.7D.82.84.7D.7D.7D.7D.82.81.84.7D.7D.82.81.84.7D.7D.7D.80.7D.7D.7D.7D.7D.7D.82.84.7D.7D.7D.82.84.7D.7D.AF.7D.7D.7D.7D.7D.7D.7D.80.7D.7D.7D.7D.80.7D.7D.B0.7D.7D.7D.7D.7D.7D.7D.AF.7D.7D.7D.7D.AF.7D.7D.B1.7D.7D.7D.7D.7D.7D.7D.B0.7D.7D.7D.7D.B0.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.B1.7D.7D.7D.7D.B1.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.80.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D	
UNDERMAP.L1_C			.HS	.00.1E.00
UNDERMAP.L1_D			.HS	.00.1E.00
UNDERMAP.L1_E			.HS	.00.1E.00
UNDERMAP.L1_F			.HS	.00.1E.00
UNDERMAP.L1_10			.HS	.00.1E.00
UNDERMAP.L1_11			.HS	1E.1E.1E.1E.1E.1E.1E.1E.1E.7D.7D.7D.7D.7D.7D.7D.1E.1E.1E.1E.1E.1E.1E.1E.1E.7D.7D.7D.7D.7D.7D.7D.1E.1E.1E.1E.1E.1E.1E.1E.1E.7D.7D.7D.7D.7D.7D.7D.1E.1E.1E.1E.1E.1E.1E.1E.1E.7D.7D.7D.7D.7D.7D.7D.1E.1E.1E.1E.1E.1E.1E.1E.1E.7D.7D.7D.7D.7D.7D.7D.1E.1E.1E.1E.1E.1E.1E.1E.1E.7D.7D.7D.7D.7D.7D.7D.1E.1E.1E.1E.1E.1E.1E.1E.1E.7D.7D.7D.7D.7D.7D.7D.1E.1E.1E.1E.1E.1E.1E.1E.1E.7D.7D.7D.7D.7D.7D.7D.1E.1E.1E.1E.1E.1E.1E.1E.1E.7D.7D.7D.7D.7D.7D.7D.1E.1E.1E.1E.1E.1E.1E.1E.1E.7D.7D.7D.7D.7D.7D.7D.1E.1E.1E.1E.1E.1E.1E.1E.1E.7D.7D.7D.7D.7D.7D.7D.1E.1E.1E.1E.1E.1E.1E.1E.1E.7D.7D.7D.7D.7D.7D.7D.1E.1E.1E.1E.1E.1E.1E.1E.1E.7D.7D.7D.7D.7D.7D.7D.1E.1E.1E.1E.1E.1E.1E.1E.1E.7D.7D.7D.7D.7D.7D.7D.1E.1E.1E.1E.1E.1E.1E.1E.1E.7D.7D.7D.7D.7D.7D.7D.1E.1E.1E.1E.1E.1E.1E.1E.1E.7D.7D.7D.7D.7D.7D.7D	
UNDERMAP.L1_12			.HS	.51.7D.01.56.01.64.01.56.01.65.01.56.01.66.01.56.01.67.01.56.01.68.01.56.01.69.03.56.A0.7D.00
UNDERMAP.L1_13			.HS	.22.7D.01.55.04.7D.01.55.0A.7D.01.55.04.7D.01.55.0A.7D.01.55.04.7D.01.55.08.7D.02.56.01.5C.01.56.01.59.02.7D.01.55.02.7D.01.58.01.56.01.5E.03.56.04.7D.01.57.02.56.01.5B.02.56.01.5A.01.7D.01.55.0A.7D.01.55.04.7D.01.55.0A.7D.01.55.04.7D.01.55.0A.7D.01.55.04.7D.01.55.0A.7D.01.5D.04.56.01.5A.0A.7D.01.55.0F.7D.01.55.0B.7D.04.56.01.5F.0F.7D.01.55.0F.7D.01.55.08.7D.00
UNDERMAP.L1_14			.HS	.00.1E.00
UNDERMAP.L1_15			.HS	.00.1E.00
UNDERMAP.L1_16			.HS	.00.1E.00
UNDERMAP.L1_17			.HS	.00.1E.00
UNDERMAP.L1_18			.HS	.00.1E.00
UNDERMAP.L1_19			.HS	1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E	
UNDERMAP.L1_1A			.HS	7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E	
UNDERMAP.L1_1B			.HS	.06.1E.01.00.01.55.01.03.0D.1E.01.00.01.55.01.03.0D.1E.01.00.01.55.01.03.0D.1E.01.00.01.55.01.03.0D.1E.01.00.01.55.01.03.0D.1E.01.00.01.55.01.03.0D.1E.01.00.01.55.01.03.07.1E.82.E8.01.2C.01.08.01.0C.01.0D.01.0C.01.8D.02.0C.01.0A.01.2D.04.1E.00
UNDERMAP.L1_1C			.HS	.00.1E.00
UNDERMAP.L1_1D			.HS	.00.1E.00
UNDERMAP.L1_1E			.HS	.00.1E.00
UNDERMAP.L1_1F			.HS	.00.1E.00
UNDERMAP.L1_20			.HS	.00.1E.00
UNDERMAP.L1_21			.HS	.48.1E.03.7D.02.40.01.11.01.2D.09.1E.06.7D.01.11.01.2D.08.1E.07.7D.01.11.08.1E.03.7D.02.9C.03.7D.08.1E.03.7D.04.9C.01.7D.08.1E.02.7D.04.9C.02.7D.08.1E.02.7D.04.9C.02.7D.08.1E.01.7D.03.9C.01.7D.01.BC.02.7D.08.1E.02.9C.03.7D.01.BC.01.7D.01.7B.08.1E.05.7D.01.BE.01.C0.01.7D.08.1E.05.7D.01.7B.01.BC.01.7D.08.1E.04.7D.02.7B.01.BC.01.7D.00
UNDERMAP.L1_22			.HS	.0D.1E.03.E8.0D.1E.03.E8.0D.1E.03.E8.0D.1E.03.E8.0D.1E.03.E8.0D.1E.03.E8.01.2D.0C.1E.03.E8.01.11.01.2D.0B.1E.03.E8.01.7D.01.11.01.2D.0A.1E.03.E8.02.7D.01.11.01.2D.09.1E.03.E8.01.B3.02.7D.01.11.01.2D.08.1E.03.E8.01.B4.03.7D.01.11.01.2D.07.1E.03.E8.05.7D.01.18.04.0C.01.0D.01.21.01.20.03.0C.01.7D.01.7A.08.7D.01.D0.01.01.01.00.01.79.01.78.07.7D.01.55.03.0F.01.0E.01.23.01.22.03.0F.03.7D.01.7A.02.7D.01.00.09.1E.00
UNDERMAP.L1_23			.HS	E8.2C.06.7C.7C.7C.7C.7C.7E.7C.7C.11.2D.E8.E8.E8.2C.06.7C.7C.7C.09.0E.0F.0B.7C.7C.7C.13.E8.E8.E8.04.7C.7C.7C.07.2F.E8.E8.2E.10.7C.7C.01.E8.E8.E8.90.7C.7C.05.2F.20.0C.21.E8.2E.12.7C.25.0D.0D.0C.00.7D.7E.01.E8.31.7D.33.E8.E8.00.7C.7C.7D.7D.7D.00.7D.7D.03.E8.00.7D.01.E8.E8.02.7D.7D.7D.7D.7D.00.7D.7D.1B.2D.02.7D.03.E8.2C.14.7D.27.0E.0F.0E.1A.7D.7D.7D.18.24.7D.25.0C.16.7D.7D.03.E8.E8.E8.2E.19.26.7D.7D.7D.7D.7D.7D.7D.7D.7D.15.E8.E8.E8.20.0C.34.7C.27.0F.3A.7D.3B.0F.0E.17.2F.1E.1E.1E.00.7D.7D.7D.03.E8.00.7D.03.E8.20.0C.21.1E.1E.1E.22.0F.36.7D.25.30.24.7D.25.30.24.7D.35.0C.0D.0C.0C.0C.34.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.7D.27.32.26.7D.27.32.26.7D.37.0E.0F.0E.0F.0F.36.7C.03.1E.31.7D.33.1E.00.7D.35.1E.1E.1E.1E.40.24.7D.25.21.00.7D.03.1E.02.7D.7D.1E.1E.1E	
UNDERMAP.L1_24			.HS	.30.1E.01.0D.02.0C.02.0D.01.0C.01.0D.01.0C.01.0D.03.0C.01.21.03.1E.09.7D.02.78.01.B3.01.00.03.1E.09.7D.01.78.01.79.01.B4.01.00.03.1E.02.0F.02.0E.01.0F.01.0E.01.0F.02.0E.03.0F.01.23.16.1E.01.20.01.0C.01.0A.01.2D.0B.1E.01.20.01.24.02.6A.01.11.01.2D.09.1E.01.2C.01.14.01.6A.01.9B.02.6A.01.11.01.2D.07.1E.01.0D.01.16.01.7D.02.6A.01.9B.02.6A.01.11.01.2D.06.1E.01.7D.03.7C.05.6A.01.11.01.2D.05.1E.01.0F.01.0B.02.7C.02.6A.01.9B.02.6A.01.9B.01.11.01.2D.05.1E.01.2E.01.10.03.7C.01.6A.01.9B.03.6A.01.11.01.2D.05.1E.01.2E.01.12.02.7C.04.6A.01.9B.01.6A.01.13.03.1E.00
UNDERMAP.L1_25			.HS	.00.1E.00
UNDERMAP.L1_26			.HS	.D8.1E.08.E8.08.1E.08.E8.08.1E.08.E8.00
UNDERMAP.L1_27			.HS	.D0.1E.02.E8.0E.1E.02.E8.0E.1E.02.E8.0E.1E.00
UNDERMAP.L1_28			.HS	.00.1E.00
UNDERMAP.L1_29			.HS	.08.1E.04.7D.01.7B.01.7D.01.BE.01.C2.08.1E.01.7D.01.BC.05.7D.01.BC.08.1E.01.7D.01.BC.05.7D.01.BC.08.1E.01.7D.01.BC.05.7D.01.BC.08.1E.01.BD.01.C2.01.BD.01.C2.03.BD.01.C2.08.1E.03.7D.01.BC.03.7D.01.BC.08.1E.03.7D.01.BC.03.7D.01.BC.08.1E.03.7D.01.C2.02.BD.02.7D.08.1E.03.7D.01.BC.04.7D.08.1E.03.7D.01.BC.04.7D.06.1E.04.7D.01.BF.01.C1.04.7D.06.1E.04.7D.01.BC.05.7D.06.1E.04.7D.01.BC.05.7D.06.1E.01.BD.01.C5.02.BD.01.C3.01.BD.01.C0.03.7D.06.1E.01.7D.01.BC.04.7D.01.C4.01.BD.02.7D.06.1E.01.7D.01.BC.04.7D.01.BC.03.7D.00
UNDERMAP.L1_2A			.HS	.03.BD.03.7D.01.00.09.1E.01.7D.01.7A.04.7D.01.00.09.1E.01.7D.02.7A.03.7D.01.00.09.1E.02.7D.01.7A.03.7D.01.00.09.1E.06.7D.01.00.09.1E.06.7D.01.00.09.1E.07.40.29.1E.0D.0C.01.0D.01.21.01.20.0E.7D.01.01.01.00.0D.0F.01.0E.01.23.01.22.40.1E.00
UNDERMAP.L1_2B			.HS	.01.1E.01.40.03.7D.01.25.01.38.01.7D.01.39.01.0C.01.24.01.7D.01.37.04.1E.01.40.01.3C.01.3D.08.7D.01.03.04.1E.01.40.01.3E.01.3F.02.7D.01.3A.01.7D.01.3B.01.0F.01.8E.01.0F.01.23.04.1E.07.40.06.E8.0A.1E.06.E8.0A.1E.06.E8.A2.1E.00
UNDERMAP.L1_2C			.HS	.03.1E.01.22.01.0E.01.0F.01.0E.01.0F.01.0B.03.6A.01.01.0B.1E.01.2E.01.10.02.7C.01.03.0C.1E.01.2E.01.12.01.7C.01.01.0D.1E.01.00.01.7C.01.03.05.1E.01.20.03.0C.01.0D.03.0C.01.24.01.7C.01.01.05.1E.01.00.01.B3.02.78.05.7D.01.7C.01.25.01.0C.01.0D.01.0C.02.1E.01.00.01.B4.01.79.01.78.04.7D.06.7C.02.1E.01.22.03.0F.01.0E.03.0F.01.0E.01.0F.01.0E.01.0F.01.0E.01.0F.80.1E.00
UNDERMAP.L1_2D			.HS	1E.1E.1E.1E.1E.1E.20.0C.0D.0C.0C.0A.2D.1E.1E.1E.1E.1E.1E.1E.1E.2C.14.88.7C.7C.7C.7C.11.2D.1E.1E.1E.1E.20.0C.0C.16.88.88.88.7C.7C.7C.7D.11.2D.1E.1E.20.24.7C.7C.7C.88.88.88.88.7C.7C.7D.7D.11.2D.2C.14.7C.7C.7C.7C.7C.88.88.88.88.7C.7C.7D.7D.11.16.7D.7C.7C.6C.6C.7C.6C.88.88.88.7C.7D.7D.7D.7D.7C.7C.7C.6C.6C.6C.88.88.88.F4.88.88.7D.6C.7D.7D.0B.7C.7C.7C.6C.88.88.88.88.88.88.88.88.88.6C.7D.2E.10.7C.7C.88.88.88.88.88.88.88.88.88.88.88.7D.1E.2E.12.88.88.88.88.88.88.88.88.88.88.88.7D.7D.1E.1E.2E.12.88.6C.88.88.88.88.6C.7C.88.88.7D.7D.1E.1E.1E.00.7C.7C.6C.7C.7C.6C.6C.7C.7D.7D.7D.7D.1E.1E.1E.00.7C.7C.7C.7C.7C.7C.7C.7C.7D.7D.7D.7D.1E.1E.1E.02.7C.7C.7C.7C.7C.7C.7C.27.26.7D.27.0E.1E.1E.1E.00.7C.7C.7C.7C.7C.7C.7C.03.00.7D.03.1E.1E.1E.1E.22.26.7C.7C.7C.7C.27.0E.23.00.7D.01.1E	
UNDERMAP.L1_2E			.HS	.38.E8.01.20.01.0C.01.0D.01.8D.02.0C.01.8D.01.0A.01.2D.06.E8.01.2C.01.14.01.6E.01.6F.01.70.01.6D.03.7D.01.11.01.2D.03.E8.01.20.01.8D.01.16.04.7D.01.71.01.72.03.7D.01.13.02.E8.01.2C.01.14.01.6D.05.7D.01.72.02.71.01.01.01.7D.01.35.02.8D.01.16.01.6D.08.7D.01.72.01.01.01.7D.01.7C.0C.7D.01.72.01.03.01.7D.01.37.02.8E.01.0B.01.6E.08.7D.01.71.01.03.01.7D.01.03.02.E8.01.2E.01.10.07.7D.01.71.01.72.02.7D.01.03.03.E8.01.2E.01.12.01.6E.01.70.01.6D.01.6F.02.7D.01.71.03.7D.01.15.04.E8.01.22.01.8E.01.0F.01.0B.01.7C.04.7D.01.07.01.17.01.2F.07.E8.01.2E.01.12.03.7D.01.1F.01.2F.0A.E8.01.22.01.0E.01.8E.01.0E.01.23.11.E8.00
UNDERMAP.L1_2F			.HS	.05.E8.0B.1E.05.E8.0B.1E.05.E8.0B.1E.05.E8.0B.1E.01.2D.04.E8.0B.1E.01.13.04.E8.0B.1E.01.8F.04.E8.0B.1E.01.01.04.E8.0B.1E.01.03.04.E8.0B.1E.01.8F.04.E8.0B.1E.01.15.04.E8.0B.1E.01.2F.04.E8.0B.1E.05.E8.0B.1E.05.E8.0B.1E.05.E8.0B.1E.05.E8.0B.1E.00
UNDERMAP.L1_30			.HS	.00.1E.00
UNDERMAP.L1_31			.HS	.07.1E.02.7D.03.BD.01.C6.03.7D.07.1E.05.7D.01.BC.03.7D.07.1E.05.7D.01.BC.03.7D.D0.1E.00
UNDERMAP.L1_32			.HS	.20.1E.0D.0C.01.0D.01.21.01.20.0D.7D.01.D1.01.01.01.00.0D.0F.01.0E.01.23.01.22.B0.1E.00
UNDERMAP.L1_33			.HS	.00.1E.00
UNDERMAP.L1_34			.HS	.00.1E.00
UNDERMAP.L1_35			.HS	.04.1E.01.00.04.7C.01.03.02.1E.01.02.01.7D.01.03.05.1E.01.22.01.0E.01.26.01.7C.01.27.01.23.02.1E.01.00.01.7D.01.01.07.1E.01.00.01.7C.01.03.02.1E.01.20.01.38.01.7C.01.39.01.0C.06.1E.01.00.01.7C.01.03.02.1E.01.00.01.9F.01.7D.02.9F.06.1E.01.00.01.7C.01.03.02.1E.01.00.04.9F.04.1E.01.20.01.0D.01.38.01.7C.01.39.01.0C.01.21.01.02.04.9F.04.1E.01.00.05.7C.01.01.01.22.01.0E.01.0F.01.0E.01.0F.04.1E.01.02.05.7C.01.03.09.1E.01.02.01.7C.01.74.03.7C.01.01.09.1E.01.22.01.0E.02.0F.01.0E.01.0F.01.23.65.1E.00
UNDERMAP.L1_36			.HS	.08.1E.08.E8.08.1E.08.E8.01.21.0F.1E.01.01.0F.1E.01.03.0F.1E.01.01.0F.1E.01.23.9F.1E.00
UNDERMAP.L1_37			.HS	E8.E8.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.E8.E8.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E.1E	
UNDERMAP.L1_38			.HS	.00.1E.00
UNDERMAP.L1_39			.HS	.00.1E.00
UNDERMAP.L1_3A			.HS	.00.1E.00
UNDERMAP.L1_3B			.HS	.00.1E.00
UNDERMAP.L1_3C			.HS	.00.1E.00
UNDERMAP.L1_3D			.HS	.00.1E.00
UNDERMAP.L1_3E			.HS	.00.1E.00
UNDERMAP.L1_3F			.HS	.00.1E.00
@END
	.BS $c00
