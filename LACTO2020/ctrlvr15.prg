* REPORTE DE DIAS EN LOTE DE RETO
* -------------------------------
PROCEDURE RV1093
Dimension TG(60),AW(60),AV(60),VP(5)
TG=0
AW=0
AV=0
VP=0

Select REG 
Set Order To 2
Set Filter To NP>0 And DER>0 And FB2=B 
Scan 
	Do Case
		Case NP=1
			AW(1)=AW(1)+1	

		Case NP=2
			AW(2)=AW(2)+1	

		Case NP>=3
			AW(3)=AW(3)+1	

	EndCase
			AW(4)=AW(4)+1	
EndScan

Scan 
	Do Case
		Case NP=1
				Do Case
					Case DER>0 And DER<=5
							TG(1)=TG(1)+1
*							AV(1)=(TG(1)/AW(1))*100
															
					Case DER>5 And DER<=10
							TG(2)=TG(2)+1

					Case DER>10 And DER<=15
							TG(3)=TG(3)+1

					Case DER>15 And DER<=20
							TG(4)=TG(4)+1

					Case DER>20 And DER<=25
							TG(5)=TG(5)+1

					Case DER>25 And DER<=30
							TG(6)=TG(6)+1

					Case DER>30 And DER<=35
							TG(7)=TG(7)+1

					Case DER>35 And DER<=40
							TG(8)=TG(8)+1

					Case DER>40 And DER<=45
							TG(9)=TG(9)+1

					Case DER>45 And DER<=50
							TG(10)=TG(10)+1

					Case DER>50 And DER<=55
							TG(11)=TG(11)+1

					Case DER>55 And DER<=60
							TG(12)=TG(12)+1

					Case DER>65
							TG(13)=TG(13)+1
				EndCase

		Case NP=2
				Do Case
					Case DER>0 And DER<=5
							TG(16)=TG(16)+1
															
					Case DER>5 And DER<=10
							TG(17)=TG(17)+1

					Case DER>10 And DER<=15
							TG(18)=TG(18)+1

					Case DER>15 And DER<=20
							TG(19)=TG(19)+1

					Case DER>20 And DER<=25
							TG(20)=TG(20)+1

					Case DER>25 And DER<=30
							TG(21)=TG(21)+1

					Case DER>30 And DER<=35
							TG(22)=TG(22)+1

					Case DER>35 And DER<=40
							TG(23)=TG(23)+1

					Case DER>40 And DER<=45
							TG(24)=TG(24)+1

					Case DER>45 And DER<=50
							TG(25)=TG(25)+1

					Case DER>50 And DER<=55
							TG(26)=TG(26)+1

					Case DER>55 And DER<=60
							TG(27)=TG(27)+1

					Case DER>65
							TG(28)=TG(28)+1
				EndCase

		Case NP>=3
				Do Case
					Case DER>0 And DER<=5
							TG(31)=TG(31)+1
															
					Case DER>5 And DER<=10
							TG(32)=TG(32)+1

					Case DER>10 And DER<=15
							TG(33)=TG(33)+1

					Case DER>15 And DER<=20
							TG(34)=TG(34)+1

					Case DER>20 And DER<=25
							TG(35)=TG(35)+1

					Case DER>25 And DER<=30
							TG(36)=TG(36)+1

					Case DER>30 And DER<=35
							TG(37)=TG(37)+1

					Case DER>35 And DER<=40
							TG(38)=TG(38)+1

					Case DER>40 And DER<=45
							TG(39)=TG(39)+1

					Case DER>45 And DER<=50
							TG(40)=TG(40)+1

					Case DER>50 And DER<=55
							TG(41)=TG(41)+1

					Case DER>55 And DER<=60
							TG(42)=TG(42)+1

					Case DER>65
							TG(43)=TG(43)+1
				EndCase
	EndCase		

		If NP>0
				Do Case
					Case DER>0 And DER<=5
							TG(46)=TG(46)+1
															
					Case DER>5 And DER<=10
							TG(47)=TG(47)+1

					Case DER>10 And DER<=15
							TG(48)=TG(48)+1

					Case DER>15 And DER<=20
							TG(49)=TG(49)+1

					Case DER>20 And DER<=25
							TG(50)=TG(50)+1

					Case DER>25 And DER<=30
							TG(51)=TG(51)+1

					Case DER>30 And DER<=35
							TG(52)=TG(52)+1

					Case DER>35 And DER<=40
							TG(53)=TG(53)+1

					Case DER>40 And DER<=45
							TG(54)=TG(54)+1

					Case DER>45 And DER<=50
							TG(55)=TG(55)+1

					Case DER>50 And DER<=55
							TG(56)=TG(56)+1

					Case DER>55 And DER<=60
							TG(57)=TG(57)+1

					Case DER>65
							TG(58)=TG(58)+1
				EndCase

		EndIF
EndScan
Select REG
Set Filter To

Create Cursor REPORTE (Concepto c(20),COL1 c(5),COL2 c(5),COL3 c(5),COL4 c(5))
Append Blank
Replace CONCEPTO With 'Dias en Reto'
Append Blank
Replace CONCEPTO With '  0-5', COL1 With Str(TG(1),4),COL2 With Str(TG(16),4),COL3 With Str(TG(31),4),COL4 With Str(TG(46),4)
Append Blank
Replace CONCEPTO With '  5-10',COL1 With Str(TG(2),4),COL2 With Str(TG(17),4),COL3 With Str(TG(32),4),COL4 With Str(TG(47),4)
Append Blank
Replace CONCEPTO With ' 10-15',COL1 With Str(TG(3),4),COL2 With Str(TG(18),4),COL3 With Str(TG(33),4),COL4 With Str(TG(48),4)
Append Blank
Replace CONCEPTO With ' 15-20',COL1 With Str(TG(4),4),COL2 With Str(TG(19),4),COL3 With Str(TG(34),4),COL4 With Str(TG(49),4)
Append Blank
Replace CONCEPTO With ' 20-25',COL1 With Str(TG(5),4),COL2 With Str(TG(20),4),COL3 With Str(TG(35),4),COL4 With Str(TG(50),4)
Append Blank
Replace CONCEPTO With ' 25-30',COL1 With Str(TG(6),4),COL2 With Str(TG(21),4),COL3 With Str(TG(36),4),COL4 With Str(TG(51),4)
Append Blank
Replace CONCEPTO With ' 30-35',COL1 With Str(TG(7),4),COL2 With Str(TG(22),4),COL3 With Str(TG(37),4),COL4 With Str(TG(52),4)
Append Blank
Replace CONCEPTO With ' 35-40',COL1 With Str(TG(8),4),COL2 With Str(TG(23),4),COL3 With Str(TG(38),4),COL4 With Str(TG(53),4)
Append Blank
Replace CONCEPTO With ' 40-45',COL1 With Str(TG(9),4),COL2 With Str(TG(24),4),COL3 With Str(TG(39),4),COL4 With Str(TG(54),4)
Append Blank
Replace CONCEPTO With ' 45-50',COL1 With Str(TG(10),4),COL2 With Str(TG(25),4),COL3 With Str(TG(40),4),COL4 With Str(TG(55),4)
Append Blank
Replace CONCEPTO With ' 50-55',COL1 With Str(TG(11),4),COL2 With Str(TG(26),4),COL3 With Str(TG(41),4),COL4 With Str(TG(56),4)
Append Blank
Replace CONCEPTO With ' 55-60',COL1 With Str(TG(12),4),COL2 With Str(TG(27),4),COL3 With Str(TG(42),4),COL4 With Str(TG(57),4)
Append Blank
Replace CONCEPTO With ' 60 ->',COL1 With Str(TG(13),4),COL2 With Str(TG(27),4),COL3 With Str(TG(42),4),COL4 With Str(TG(57),4)
Append Blank
Replace CONCEPTO With ''

GO TOP
RETURN


* DISTRIBUCION DE DIAS A PRIMER SERVICIO
* --------------------------------------
PROCEDURE RV10157
Dimension TG(60),AW(60),AV(60),VP(5)
TG=0
AW=0
AV=0
VP=0
Select REG 
Set Order To 2
Set Filter To NP>0 And PSER#B And NS>0 And (PSER-FPAR)>0 And UCAL-FPAR>40 And FB2=B AND PSER>=CTOD(FREPS5.CB1.Value) AND PSER<=CTOD(FREPS5.CB2.Value)&&AND STAT='CARGA'
Scan 
	Do Case
		Case NP=1
			AW(1)=AW(1)+1	
			If STAT='CARGA' And NS=1
				VP(1)=VP(1)+1
			EndIF	

		Case NP=2
			AW(2)=AW(2)+1	
			If STAT='CARGA' And NS=1
				VP(2)=VP(2)+1
			EndIF	

		Case NP>=3
			AW(3)=AW(3)+1	
			If STAT='CARGA' And NS=1
				VP(3)=VP(3)+1
			EndIF	

	EndCase
			AW(4)=AW(4)+1	
			If STAT='CARGA' And NS=1
				VP(4)=VP(4)+1
			EndIF	
EndScan

Scan 
	Do Case
		Case NP=1
				Do Case
					Case (PSER-FPAR)>0 And (PSER-FPAR)<=30
							TG(1)=TG(1)+1
							AV(1)=(TG(1)/AW(1))*100
															
					Case (PSER-FPAR)>30 And (PSER-FPAR)<=45
							TG(2)=TG(2)+1
							AV(2)=(TG(2)/AW(1))*100

					Case (PSER-FPAR)>45 And (PSER-FPAR)<=60
							TG(3)=TG(3)+1
							AV(3)=(TG(3)/AW(1))*100

					Case (PSER-FPAR)>60 And (PSER-FPAR)<=70
							TG(4)=TG(4)+1
							AV(4)=(TG(4)/AW(1))*100

					Case (PSER-FPAR)>70 And (PSER-FPAR)<=80
							TG(5)=TG(5)+1
							AV(5)=(TG(5)/AW(1))*100

					Case (PSER-FPAR)>80 And (PSER-FPAR)<=100
							TG(6)=TG(6)+1
							AV(6)=(TG(6)/AW(1))*100

					Case (PSER-FPAR)>100 And (PSER-FPAR)<=120
							TG(7)=TG(7)+1
							AV(7)=(TG(7)/AW(1))*100

					Case (PSER-FPAR)>120 And (PSER-FPAR)<=140
							TG(8)=TG(8)+1
							AV(8)=(TG(8)/AW(1))*100

					Case (PSER-FPAR)>140 And (PSER-FPAR)<=160
							TG(9)=TG(9)+1
							AV(9)=(TG(9)/AW(1))*100

					Case (PSER-FPAR)>160 And (PSER-FPAR)<=180
							TG(10)=TG(10)+1
							AV(10)=(TG(10)/AW(1))*100

					Case (PSER-FPAR)>180 And (PSER-FPAR)<=200
							TG(11)=TG(11)+1
							AV(11)=(TG(11)/AW(1))*100

					Case (PSER-FPAR)>200
							TG(12)=TG(12)+1
							AV(12)=(TG(12)/AW(1))*100
				EndCase

		Case NP=2
				Do Case
					Case (PSER-FPAR)>0 And (PSER-FPAR)<=30
							TG(16)=TG(16)+1
							AV(16)=(TG(16)/AW(2))*100
								
					Case (PSER-FPAR)>30 And (PSER-FPAR)<=45
							TG(17)=TG(17)+1
							AV(17)=(TG(17)/AW(2))*100

					Case (PSER-FPAR)>45 And (PSER-FPAR)<=60
							TG(18)=TG(18)+1
							AV(18)=(TG(18)/AW(2))*100

					Case (PSER-FPAR)>60 And (PSER-FPAR)<=70
							TG(19)=TG(19)+1
							AV(19)=(TG(19)/AW(2))*100

					Case (PSER-FPAR)>70 And (PSER-FPAR)<=80
							TG(20)=TG(20)+1
							AV(20)=(TG(20)/AW(2))*100

					Case (PSER-FPAR)>80 And (PSER-FPAR)<=100
							TG(21)=TG(21)+1
							AV(21)=(TG(21)/AW(2))*100

					Case (PSER-FPAR)>100 And (PSER-FPAR)<=120
							TG(22)=TG(22)+1
							AV(22)=(TG(22)/AW(2))*100

					Case (PSER-FPAR)>120 And (PSER-FPAR)<=140
							TG(23)=TG(23)+1
							AV(23)=(TG(23)/AW(2))*100

					Case (PSER-FPAR)>140 And (PSER-FPAR)<=160
							TG(24)=TG(24)+1
							AV(24)=(TG(24)/AW(2))*100

					Case (PSER-FPAR)>160 And (PSER-FPAR)<=180
							TG(25)=TG(25)+1
							AV(25)=(TG(25)/AW(2))*100

					Case (PSER-FPAR)>180 And (PSER-FPAR)<=200
							TG(26)=TG(26)+1
							AV(26)=(TG(26)/AW(2))*100

					Case (PSER-FPAR)>200
							TG(27)=TG(27)+1
							AV(27)=(TG(27)/AW(2))*100
				EndCase

		Case NP>=3
				Do Case
					Case (PSER-FPAR)>0 And (PSER-FPAR)<=30
							TG(31)=TG(31)+1
							AV(31)=(TG(31)/AW(3))*100
								
					Case (PSER-FPAR)>30 And (PSER-FPAR)<=45
							TG(32)=TG(32)+1
							AV(32)=(TG(32)/AW(3))*100

					Case (PSER-FPAR)>45 And (PSER-FPAR)<=60
							TG(33)=TG(33)+1
							AV(33)=(TG(33)/AW(3))*100

					Case (PSER-FPAR)>60 And (PSER-FPAR)<=70
							TG(34)=TG(34)+1
							AV(34)=(TG(34)/AW(3))*100

					Case (PSER-FPAR)>70 And (PSER-FPAR)<=80
							TG(35)=TG(35)+1
							AV(35)=(TG(35)/AW(3))*100

					Case (PSER-FPAR)>80 And (PSER-FPAR)<=100
							TG(36)=TG(36)+1
							AV(36)=(TG(36)/AW(3))*100

					Case (PSER-FPAR)>100 And (PSER-FPAR)<=120
							TG(37)=TG(37)+1
							AV(37)=(TG(37)/AW(3))*100

					Case (PSER-FPAR)>120 And (PSER-FPAR)<=140
							TG(38)=TG(38)+1
							AV(38)=(TG(38)/AW(3))*100

					Case (PSER-FPAR)>140 And (PSER-FPAR)<=160
							TG(39)=TG(39)+1
							AV(39)=(TG(39)/AW(3))*100

					Case (PSER-FPAR)>160 And (PSER-FPAR)<=180
							TG(40)=TG(40)+1
							AV(40)=(TG(40)/AW(3))*100

					Case (PSER-FPAR)>180 And (PSER-FPAR)<=200
							TG(41)=TG(41)+1
							AV(41)=(TG(41)/AW(3))*100

					Case (PSER-FPAR)>200
							TG(42)=TG(42)+1
							AV(42)=(TG(42)/AW(3))*100
				EndCase
	EndCase		

		If NP>0
				Do Case
					Case (PSER-FPAR)>0 And (PSER-FPAR)<=30
							TG(46)=TG(46)+1
							AV(46)=(TG(46)/AW(4))*100
								
					Case (PSER-FPAR)>30 And (PSER-FPAR)<=45
							TG(47)=TG(47)+1
							AV(47)=(TG(47)/AW(4))*100

					Case (PSER-FPAR)>45 And (PSER-FPAR)<=60
							TG(48)=TG(48)+1
							AV(48)=(TG(48)/AW(4))*100

					Case (PSER-FPAR)>60 And (PSER-FPAR)<=70
							TG(49)=TG(49)+1
							AV(49)=(TG(49)/AW(4))*100

					Case (PSER-FPAR)>70 And (PSER-FPAR)<=80
							TG(50)=TG(50)+1
							AV(50)=(TG(50)/AW(4))*100

					Case (PSER-FPAR)>80 And (PSER-FPAR)<=100
							TG(51)=TG(51)+1
							AV(51)=(TG(51)/AW(4))*100

					Case (PSER-FPAR)>100 And (PSER-FPAR)<=120
							TG(52)=TG(52)+1
							AV(52)=(TG(52)/AW(4))*100

					Case (PSER-FPAR)>120 And (PSER-FPAR)<=140
							TG(53)=TG(53)+1
							AV(53)=(TG(53)/AW(4))*100

					Case (PSER-FPAR)>140 And (PSER-FPAR)<=160
							TG(54)=TG(54)+1
							AV(54)=(TG(54)/AW(4))*100

					Case (PSER-FPAR)>160 And (PSER-FPAR)<=180
							TG(55)=TG(55)+1
							AV(55)=(TG(55)/AW(4))*100

					Case (PSER-FPAR)>180 And (PSER-FPAR)<=200
							TG(56)=TG(56)+1
							AV(56)=(TG(56)/AW(4))*100

					Case (PSER-FPAR)>200
							TG(57)=TG(57)+1
							AV(57)=(TG(57)/AW(4))*100
				EndCase
		EndIF
EndScan

Select REG
Set Filter To

Create Cursor REPORTE (Concepto c(20),COL1 c(5),COL2 c(5),COL3 c(5),COL4 c(5))
Append Blank
Replace CONCEPTO With 'Dias en Leche'
Append Blank
Replace CONCEPTO With '   0-30', COL1 With Str(AV(1),5,1),COL2 With Str(AV(16),5,1),COL3 With Str(AV(31),5,1),COL4 With Str(AV(46),5,1)
Append Blank
Replace CONCEPTO With '  30-45',COL1 With Str(AV(2),5,1),COL2 With Str(AV(17),5,1),COL3 With Str(AV(32),5,1),COL4 With Str(AV(47),5,1)
Append Blank
Replace CONCEPTO With '  45-60',COL1 With Str(AV(3),5,1),COL2 With Str(AV(18),5,1),COL3 With Str(AV(33),5,1),COL4 With Str(AV(48),5,1)
Append Blank
Replace CONCEPTO With '  60-70',COL1 With Str(AV(4),5,1),COL2 With Str(AV(19),5,1),COL3 With Str(AV(34),5,1),COL4 With Str(AV(49),5,1)
Append Blank
Replace CONCEPTO With '  70-80',COL1 With Str(AV(5),5,1),COL2 With Str(AV(20),5,1),COL3 With Str(AV(35),5,1),COL4 With Str(AV(50),5,1)
Append Blank
Replace CONCEPTO With ' 80-100',COL1 With Str(AV(6),5,1),COL2 With Str(AV(21),5,1),COL3 With Str(AV(36),5,1),COL4 With Str(AV(51),5,1)
Append Blank
Replace CONCEPTO With '100-120',COL1 With Str(AV(7),5,1),COL2 With Str(AV(22),5,1),COL3 With Str(AV(37),5,1),COL4 With Str(AV(52),5,1)
Append Blank
Replace CONCEPTO With '120-140',COL1 With Str(AV(8),5,1),COL2 With Str(AV(23),5,1),COL3 With Str(AV(38),5,1),COL4 With Str(AV(53),5,1)
Append Blank
Replace CONCEPTO With '140-160',COL1 With Str(AV(9),5,1),COL2 With Str(AV(24),5,1),COL3 With Str(AV(39),5,1),COL4 With Str(AV(54),5,1)
Append Blank
Replace CONCEPTO With '160-180',COL1 With Str(AV(10),5,1),COL2 With Str(AV(25),5,1),COL3 With Str(AV(40),5,1),COL4 With Str(AV(55),5,1)
Append Blank
Replace CONCEPTO With '180-200',COL1 With Str(AV(11),5,1),COL2 With Str(AV(26),5,1),COL3 With Str(AV(41),5,1),COL4 With Str(AV(56),5,1)
Append Blank
Replace CONCEPTO With '200  ->',COL1 With Str(AV(12),5,1),COL2 With Str(AV(27),5,1),COL3 With Str(AV(42),5,1),COL4 With Str(AV(57),5,1)
Append Blank
Replace CONCEPTO With ''

*Append Blank
*Replace CONCEPTO With 'Fertilidad %',COL1 With Str(VP(1)/AW(1)*100,5,1),COL2 With Str(VP(2)/AW(2)*100,5,1),COL3 With Str(VP(3)/AW(3)*100,5,1),COL4 With Str(VP(4)/AW(4)*100,5,1)
*Replace CONCEPTO With 'Fertilidad %',COL1 With Str(VP(1)/AW(1)*100,5,1),COL2 With Str(VP(2)/AW(2)*100,5,1),COL3 With Str(VP(3)/AW(3)*100,5,1),COL4 With Str(AW(4),5,1)

GO TOP
RETURN

* DISTRIBUCION DE DIAS ABIERTOS TOTAL
* ------------------------------------------
PROCEDURE RV1059
HOY=DATE()

Dimension TG(60),DA(10),VT(10)
TG=0
DA=0
VT=0
VTOTAL=0
DABT=0

Select REG 
Set Order To 2
Set Filter To (FB2=B AND PSV='N' AND DPR>Q5) OR (FB2=B AND PSV='S' AND DPR<=Q5 AND NS>0)

Scan 

	xDAB=IIF(STAT='CARGA' Or STAT='INSEM',UCAL-FPAR,HOY-FPAR)
	Replace DAB With xDAB
	
	* Total del Hato
	* --------------

	Do Case
		Case STAT='FRESC' Or STAT='LIMPI' Or STAT='SUCIA' Or STAT='ANEST' Or STAT='TRATA' Or STAT='ABORT'&&NP=1
		DA(1)=DA(1)+DAB
		VT(1)=VT(1)+1				
				Do Case
					Case DAB>0 And DAB<=30
							TG(1)=TG(1)+1
															
					Case DAB>30 And DAB<=60
							TG(2)=TG(2)+1
							
					Case DAB>60 And DAB<=90
							TG(3)=TG(3)+1

					Case DAB>90 And DAB<=120
							TG(4)=TG(4)+1

					Case DAB>120 And DAB<=150
							TG(5)=TG(5)+1

					Case DAB>150 And DAB<=180
							TG(6)=TG(6)+1

					Case DAB>180 And DAB<=210
							TG(7)=TG(7)+1

					Case DAB>210 And DAB<=240
							TG(8)=TG(8)+1

					Case DAB>240 And DAB<=270
							TG(9)=TG(9)+1

					Case DAB>270 And DAB<=300
							TG(10)=TG(10)+1

					Case DAB>300 And DAB<=330
							TG(11)=TG(11)+1

					Case DAB>330
							TG(12)=TG(12)+1
				ENDCASE

		Case STAT='INSEM' &&NP=2
		DA(2)=DA(2)+DAB
		VT(2)=VT(2)+1	
				Do Case
					Case DAB>0 And DAB<=30
							TG(16)=TG(16)+1
								
					Case DAB>30 And DAB<=60
							TG(17)=TG(17)+1

					Case DAB>60 And DAB<=90
							TG(18)=TG(18)+1

					Case DAB>90 And DAB<=120
							TG(19)=TG(19)+1

					Case DAB>120 And DAB<=150
							TG(20)=TG(20)+1

					Case DAB>150 And DAB<=180
							TG(21)=TG(21)+1

					Case DAB>180 And DAB<=210
							TG(22)=TG(22)+1

					Case DAB>210 And DAB<=240
							TG(23)=TG(23)+1

					Case DAB>240 And DAB<=270
							TG(24)=TG(24)+1

					Case DAB>270 And DAB<=300
							TG(25)=TG(25)+1

					Case DAB>300 And DAB<=330
							TG(26)=TG(26)+1

					Case DAB>330 
							TG(27)=TG(27)+1
				EndCase

		Case STAT='CARGA' &&NP>=3 
		DA(3)=DA(3)+DAB
		VT(3)=VT(3)+1	
				Do Case
					Case DAB>0 And DAB<=30
							TG(31)=TG(31)+1
								
					Case DAB>30 And DAB<=60
							TG(32)=TG(32)+1

					Case DAB>60 And DAB<=90
							TG(33)=TG(33)+1

					Case DAB>90 And DAB<=120
							TG(34)=TG(34)+1

					Case DAB>120 And DAB<=150
							TG(35)=TG(35)+1

					Case DAB>150 And DAB<=180
							TG(36)=TG(36)+1

					Case DAB>180 And DAB<=210
							TG(37)=TG(37)+1

					Case DAB>210 And DAB<=240
							TG(38)=TG(38)+1

					Case DAB>240 And DAB<=270
							TG(39)=TG(39)+1

					Case DAB>270 And DAB<=300
							TG(40)=TG(40)+1

					Case DAB>300 And DAB<=330
							TG(41)=TG(41)+1

					Case DAB>330 
							TG(42)=TG(42)+1
				EndCase
	EndCase		

		If NP>0
		DABT=DABT+DAB
		VTOTAL=VTOTAL+1
				Do Case
					Case DAB>0 And DAB<=30
							TG(46)=TG(46)+1
								
					Case DAB>30 And DAB<=60
							TG(47)=TG(47)+1

					Case DAB>60 And DAB<=90
							TG(48)=TG(48)+1

					Case DAB>90 And DAB<=120
							TG(49)=TG(49)+1

					Case DAB>120 And DAB<=150
							TG(50)=TG(50)+1

					Case DAB>150 And DAB<=180
							TG(51)=TG(51)+1

					Case DAB>180 And DAB<=210
							TG(52)=TG(52)+1

					Case DAB>210 And DAB<=240
							TG(53)=TG(53)+1

					Case DAB>240 And DAB<=270
							TG(54)=TG(54)+1

					Case DAB>270 And DAB<=300
							TG(55)=TG(55)+1

					Case DAB>300 And DAB<=330
							TG(56)=TG(56)+1

					Case DAB>330
							TG(57)=TG(57)+1
				EndCase
		EndIF
EndScan

Select REG
Set Filter To

Create Cursor REPORTE (Concepto c(20),COL1 c(5),COL2 c(5),COL3 c(5),COL4 c(5))
Append Blank
Replace CONCEPTO With 'Dias Abiertos'
Append Blank
Replace CONCEPTO With '   0-30', COL1 With Str(TG(1),4),COL2 With Str(TG(16),4),COL3 With Str(TG(31),4),COL4 With Str(TG(46),4)
Append Blank
Replace CONCEPTO With '  30-60',COL1 With Str(TG(2),4),COL2 With Str(TG(17),4),COL3 With Str(TG(32),4),COL4 With Str(TG(47),4)
Append Blank
Replace CONCEPTO With '  60-90',COL1 With Str(TG(3),4),COL2 With Str(TG(18),4),COL3 With Str(TG(33),4),COL4 With Str(TG(48),4)
Append Blank
Replace CONCEPTO With ' 90-120',COL1 With Str(TG(4),4),COL2 With Str(TG(19),4),COL3 With Str(TG(34),4),COL4 With Str(TG(49),4)
Append Blank
Replace CONCEPTO With '120-150',COL1 With Str(TG(5),4),COL2 With Str(TG(20),4),COL3 With Str(TG(35),4),COL4 With Str(TG(50),4)
Append Blank
Replace CONCEPTO With '150-180',COL1 With Str(TG(6),4),COL2 With Str(TG(21),4),COL3 With Str(TG(36),4),COL4 With Str(TG(51),4)
Append Blank
Replace CONCEPTO With '180-210',COL1 With Str(TG(7),4),COL2 With Str(TG(22),4),COL3 With Str(TG(37),4),COL4 With Str(TG(52),4)
Append Blank
Replace CONCEPTO With '210-240',COL1 With Str(TG(8),4),COL2 With Str(TG(23),4),COL3 With Str(TG(38),4),COL4 With Str(TG(53),4)
Append Blank
Replace CONCEPTO With '240-270',COL1 With Str(TG(9),4),COL2 With Str(TG(24),4),COL3 With Str(TG(39),4),COL4 With Str(TG(54),4)
Append Blank
Replace CONCEPTO With '270-300',COL1 With Str(TG(10),4),COL2 With Str(TG(25),4),COL3 With Str(TG(40),4),COL4 With Str(TG(55),4)
Append Blank
Replace CONCEPTO With '300-330',COL1 With Str(TG(11),4),COL2 With Str(TG(26),4),COL3 With Str(TG(41),4),COL4 With Str(TG(56),4)
Append Blank
Replace CONCEPTO With '330  ->',COL1 With Str(TG(12),4),COL2 With Str(TG(27),4),COL3 With Str(TG(42),4),COL4 With Str(TG(57),4)
Append Blank
Replace CONCEPTO With ''

GO TOP
RETURN

* DISTRIBUCION DE DIAS ABIERTOS EN PORCIENTO
* ------------------------------------------
PROCEDURE RV10171
HOY=DATE()
Dimension TG(60),AW(60),AV(60),VP(5)
TG=0
AW=0
AV=0
VP=0
Select REG 
Set Order To 2
Set Filter To FB2=B 
Go Top
Count To AW(1)

Scan 

	xDAB=IIF(STAT='CARGA' Or STAT='INSEM',UCAL-FPAR,HOY-FPAR)
	Replace DAB With xDAB
	
	* Total del Hato
	* --------------

	Do Case
		Case STAT='FRESC' Or STAT='LIMPI' Or STAT='SUCIA' Or STAT='ANEST' Or STAT='TRATA' Or STAT='ABORT'&&NP=1
				Do Case
					Case DAB>0 And DAB<=30
							TG(1)=TG(1)+1
							AV(1)=(TG(1)/AW(1))*100
															
					Case DAB>30 And DAB<=60
							TG(2)=TG(2)+1
							AV(2)=(TG(2)/AW(1))*100

					Case DAB>60 And DAB<=90
							TG(3)=TG(3)+1
							AV(3)=(TG(3)/AW(1))*100

					Case DAB>90 And DAB<=120
							TG(4)=TG(4)+1
							AV(4)=(TG(4)/AW(1))*100

					Case DAB>120 And DAB<=150
							TG(5)=TG(5)+1
							AV(5)=(TG(5)/AW(1))*100

					Case DAB>150 And DAB<=180
							TG(6)=TG(6)+1
							AV(6)=(TG(6)/AW(1))*100

					Case DAB>180 And DAB<=210
							TG(7)=TG(7)+1
							AV(7)=(TG(7)/AW(1))*100

					Case DAB>210 And DAB<=240
							TG(8)=TG(8)+1
							AV(8)=(TG(8)/AW(1))*100

					Case DAB>240 And DAB<=270
							TG(9)=TG(9)+1
							AV(9)=(TG(9)/AW(1))*100

					Case DAB>270 And DAB<=300
							TG(10)=TG(10)+1
							AV(10)=(TG(10)/AW(1))*100

					Case DAB>300 And DAB<=330
							TG(11)=TG(11)+1
							AV(11)=(TG(11)/AW(1))*100

					Case DAB>330
							TG(12)=TG(12)+1
							AV(12)=(TG(12)/AW(1))*100
				EndCase

		Case STAT='INSEM' &&NP=2
				Do Case
					Case DAB>0 And DAB<=30
							TG(16)=TG(16)+1
							AV(16)=(TG(16)/AW(1))*100
								
					Case DAB>30 And DAB<=60
							TG(17)=TG(17)+1
							AV(17)=(TG(17)/AW(1))*100

					Case DAB>60 And DAB<=90
							TG(18)=TG(18)+1
							AV(18)=(TG(18)/AW(1))*100

					Case DAB>90 And DAB<=120
							TG(19)=TG(19)+1
							AV(19)=(TG(19)/AW(1))*100

					Case DAB>120 And DAB<=150
							TG(20)=TG(20)+1
							AV(20)=(TG(20)/AW(1))*100

					Case DAB>150 And DAB<=180
							TG(21)=TG(21)+1
							AV(21)=(TG(21)/AW(1))*100

					Case DAB>180 And DAB<=210
							TG(22)=TG(22)+1
							AV(22)=(TG(22)/AW(1))*100

					Case DAB>210 And DAB<=240
							TG(23)=TG(23)+1
							AV(23)=(TG(23)/AW(1))*100

					Case DAB>240 And DAB<=270
							TG(24)=TG(24)+1
							AV(24)=(TG(24)/AW(1))*100

					Case DAB>270 And DAB<=300
							TG(25)=TG(25)+1
							AV(25)=(TG(25)/AW(1))*100

					Case DAB>300 And DAB<=330
							TG(26)=TG(26)+1
							AV(26)=(TG(26)/AW(1))*100

					Case DAB>330 
							TG(27)=TG(27)+1
							AV(27)=(TG(27)/AW(1))*100
				EndCase

		Case STAT='CARGA' &&NP>=3 
				Do Case
					Case DAB>0 And DAB<=30
							TG(31)=TG(31)+1
							AV(31)=(TG(31)/AW(1))*100
								
					Case DAB>30 And DAB<=60
							TG(32)=TG(32)+1
							AV(32)=(TG(32)/AW(1))*100

					Case DAB>60 And DAB<=90
							TG(33)=TG(33)+1
							AV(33)=(TG(33)/AW(1))*100

					Case DAB>90 And DAB<=120
							TG(34)=TG(34)+1
							AV(34)=(TG(34)/AW(1))*100

					Case DAB>120 And DAB<=150
							TG(35)=TG(35)+1
							AV(35)=(TG(35)/AW(1))*100

					Case DAB>150 And DAB<=180
							TG(36)=TG(36)+1
							AV(36)=(TG(36)/AW(1))*100

					Case DAB>180 And DAB<=210
							TG(37)=TG(37)+1
							AV(37)=(TG(37)/AW(1))*100

					Case DAB>210 And DAB<=240
							TG(38)=TG(38)+1
							AV(38)=(TG(38)/AW(1))*100

					Case DAB>240 And DAB<=270
							TG(39)=TG(39)+1
							AV(39)=(TG(39)/AW(1))*100

					Case DAB>270 And DAB<=300
							TG(40)=TG(40)+1
							AV(40)=(TG(40)/AW(1))*100

					Case DAB>300 And DAB<=330
							TG(41)=TG(41)+1
							AV(41)=(TG(41)/AW(1))*100

					Case DAB>330 
							TG(42)=TG(42)+1
							AV(42)=(TG(42)/AW(1))*100
				EndCase
	EndCase		

		If NP>0
				Do Case
					Case DAB>0 And DAB<=30
							TG(46)=TG(46)+1
							AV(46)=(TG(46)/AW(1))*100
								
					Case DAB>30 And DAB<=60
							TG(47)=TG(47)+1
							AV(47)=(TG(47)/AW(1))*100

					Case DAB>60 And DAB<=90
							TG(48)=TG(48)+1
							AV(48)=(TG(48)/AW(1))*100

					Case DAB>90 And DAB<=120
							TG(49)=TG(49)+1
							AV(49)=(TG(49)/AW(1))*100

					Case DAB>120 And DAB<=150
							TG(50)=TG(50)+1
							AV(50)=(TG(50)/AW(1))*100

					Case DAB>150 And DAB<=180
							TG(51)=TG(51)+1
							AV(51)=(TG(51)/AW(1))*100

					Case DAB>180 And DAB<=210
							TG(52)=TG(52)+1
							AV(52)=(TG(52)/AW(1))*100

					Case DAB>210 And DAB<=240
							TG(53)=TG(53)+1
							AV(53)=(TG(53)/AW(1))*100

					Case DAB>240 And DAB<=270
							TG(54)=TG(54)+1
							AV(54)=(TG(54)/AW(1))*100

					Case DAB>270 And DAB<=300
							TG(55)=TG(55)+1
							AV(55)=(TG(55)/AW(1))*100

					Case DAB>300 And DAB<=330
							TG(56)=TG(56)+1
							AV(56)=(TG(56)/AW(1))*100

					Case DAB>330
							TG(57)=TG(57)+1
							AV(57)=(TG(57)/AW(1))*100
				EndCase
		EndIF
EndScan

Select REG
Set Filter To

Create Cursor REPORTE (Concepto c(20),COL1 c(5),COL2 c(5),COL3 c(5),COL4 c(5))
Append Blank
Replace CONCEPTO With 'Dias Abiertos'
Append Blank
Replace CONCEPTO With '   0-30', COL1 With Str(AV(1),5,1),COL2 With Str(AV(16),5,1),COL3 With Str(AV(31),5,1),COL4 With Str(AV(46),5,1)
Append Blank
Replace CONCEPTO With '  30-60',COL1 With Str(AV(2),5,1),COL2 With Str(AV(17),5,1),COL3 With Str(AV(32),5,1),COL4 With Str(AV(47),5,1)
Append Blank
Replace CONCEPTO With '  60-90',COL1 With Str(AV(3),5,1),COL2 With Str(AV(18),5,1),COL3 With Str(AV(33),5,1),COL4 With Str(AV(48),5,1)
Append Blank
Replace CONCEPTO With ' 90-120',COL1 With Str(AV(4),5,1),COL2 With Str(AV(19),5,1),COL3 With Str(AV(34),5,1),COL4 With Str(AV(49),5,1)
Append Blank
Replace CONCEPTO With '120-150',COL1 With Str(AV(5),5,1),COL2 With Str(AV(20),5,1),COL3 With Str(AV(35),5,1),COL4 With Str(AV(50),5,1)
Append Blank
Replace CONCEPTO With '150-180',COL1 With Str(AV(6),5,1),COL2 With Str(AV(21),5,1),COL3 With Str(AV(36),5,1),COL4 With Str(AV(51),5,1)
Append Blank
Replace CONCEPTO With '180-210',COL1 With Str(AV(7),5,1),COL2 With Str(AV(22),5,1),COL3 With Str(AV(37),5,1),COL4 With Str(AV(52),5,1)
Append Blank
Replace CONCEPTO With '210-240',COL1 With Str(AV(8),5,1),COL2 With Str(AV(23),5,1),COL3 With Str(AV(38),5,1),COL4 With Str(AV(53),5,1)
Append Blank
Replace CONCEPTO With '240-270',COL1 With Str(AV(9),5,1),COL2 With Str(AV(24),5,1),COL3 With Str(AV(39),5,1),COL4 With Str(AV(54),5,1)
Append Blank
Replace CONCEPTO With '270-300',COL1 With Str(AV(10),5,1),COL2 With Str(AV(25),5,1),COL3 With Str(AV(40),5,1),COL4 With Str(AV(55),5,1)
Append Blank
Replace CONCEPTO With '300-330',COL1 With Str(AV(11),5,1),COL2 With Str(AV(26),5,1),COL3 With Str(AV(41),5,1),COL4 With Str(AV(56),5,1)
Append Blank
Replace CONCEPTO With '330  ->',COL1 With Str(AV(12),5,1),COL2 With Str(AV(27),5,1),COL3 With Str(AV(42),5,1),COL4 With Str(AV(57),5,1)
Append Blank
Replace CONCEPTO With ''

GO TOP
RETURN


* DISTRIBUCION DE DIAS EN SECAS
* -----------------------------
PROCEDURE RV10221
Dimension TG(60),AW(60),AV(60),VP(5)
TG=0
AW=0
AV=0
VP=0
Select REG 
Set Order To 2
Set Filter To NP>0 And PDSC>0 And FB2=B &&AND STAT='CARGA'
Count To AW(1)

Scan 
	Do Case
		Case NP=1
			AW(1)=AW(1)+1	
		Case NP=2
			AW(2)=AW(2)+1	
		Case NP>=3
			AW(3)=AW(3)+1	
	EndCase
			AW(4)=AW(4)+1	
EndScan

Scan 
	Do Case
		Case NP=1
				Do Case
					Case PDSC>0 And PDSC<=10
							TG(1)=TG(1)+1
							AV(1)=(TG(1)/AW(4))*100
															
					Case PDSC>10 And PDSC<=20
							TG(2)=TG(2)+1
							AV(2)=(TG(2)/AW(4))*100

					Case PDSC>20 And PDSC<=30
							TG(3)=TG(3)+1
							AV(3)=(TG(3)/AW(4))*100

					Case PDSC>30 And PDSC<=40
							TG(4)=TG(4)+1
							AV(4)=(TG(4)/AW(4))*100

					Case PDSC>40 And PDSC<=50
							TG(5)=TG(5)+1
							AV(5)=(TG(5)/AW(4))*100

					Case PDSC>50 And PDSC<=60
							TG(6)=TG(6)+1
							AV(6)=(TG(6)/AW(4))*100

					Case PDSC>60 And PDSC<=70
							TG(7)=TG(7)+1
							AV(7)=(TG(7)/AW(4))*100

					Case PDSC>70 And PDSC<=80
							TG(8)=TG(8)+1
							AV(8)=(TG(8)/AW(4))*100

					Case PDSC>80 And PDSC<=90
							TG(9)=TG(9)+1
							AV(9)=(TG(9)/AW(4))*100

					Case PDSC>90 And PDSC<=100
							TG(10)=TG(10)+1
							AV(10)=(TG(10)/AW(4))*100

					Case PDSC>100 And PDSC<=110
							TG(11)=TG(11)+1
							AV(11)=(TG(11)/AW(4))*100

					Case PDSC>110
							TG(12)=TG(12)+1
							AV(12)=(TG(12)/AW(4))*100
				EndCase

		Case NP=2
				Do Case
					Case PDSC>0 And PDSC<=10
							TG(16)=TG(16)+1
							AV(16)=(TG(16)/AW(4))*100
								
					Case PDSC>10 And PDSC<=20
							TG(17)=TG(17)+1
							AV(17)=(TG(17)/AW(4))*100

					Case PDSC>20 And PDSC<=30
							TG(18)=TG(18)+1
							AV(18)=(TG(18)/AW(4))*100

					Case PDSC>30 And PDSC<=40
							TG(19)=TG(19)+1
							AV(19)=(TG(19)/AW(4))*100

					Case PDSC>40 And PDSC<=50
							TG(20)=TG(20)+1
							AV(20)=(TG(20)/AW(4))*100

					Case PDSC>50 And PDSC<=60
							TG(21)=TG(21)+1
							AV(21)=(TG(21)/AW(4))*100

					Case PDSC>60 And PDSC<=70
							TG(22)=TG(22)+1
							AV(22)=(TG(22)/AW(4))*100

					Case PDSC>70 And PDSC<=80
							TG(23)=TG(23)+1
							AV(23)=(TG(23)/AW(4))*100

					Case PDSC>80 And PDSC<=90
							TG(24)=TG(24)+1
							AV(24)=(TG(24)/AW(4))*100

					Case PDSC>90 And PDSC<=100
							TG(25)=TG(25)+1
							AV(25)=(TG(25)/AW(4))*100

					Case PDSC>100 And PDSC<=110
							TG(26)=TG(26)+1
							AV(26)=(TG(26)/AW(4))*100

					Case PDSC>110 
							TG(27)=TG(27)+1
							AV(27)=(TG(27)/AW(4))*100
				EndCase

		Case NP>=3
				Do Case
					Case PDSC>0 And PDSC<=10
							TG(31)=TG(31)+1
							AV(31)=(TG(31)/AW(4))*100
								
					Case PDSC>10 And PDSC<=20
							TG(32)=TG(32)+1
							AV(32)=(TG(32)/AW(4))*100

					Case PDSC>20 And PDSC<=30
							TG(33)=TG(33)+1
							AV(33)=(TG(33)/AW(4))*100

					Case PDSC>30 And PDSC<=40
							TG(34)=TG(34)+1
							AV(34)=(TG(34)/AW(4))*100

					Case PDSC>40 And PDSC<=50
							TG(35)=TG(35)+1
							AV(35)=(TG(35)/AW(4))*100

					Case PDSC>50 And PDSC<=60
							TG(36)=TG(36)+1
							AV(36)=(TG(36)/AW(4))*100

					Case PDSC>60 And PDSC<=70
							TG(37)=TG(37)+1
							AV(37)=(TG(37)/AW(4))*100

					Case PDSC>70 And PDSC<=80
							TG(38)=TG(38)+1
							AV(38)=(TG(38)/AW(4))*100

					Case PDSC>80 And PDSC<=90
							TG(39)=TG(39)+1
							AV(39)=(TG(39)/AW(4))*100

					Case PDSC>90 And PDSC<=100
							TG(40)=TG(40)+1
							AV(40)=(TG(40)/AW(4))*100

					Case PDSC>100 And PDSC<=110
							TG(41)=TG(41)+1
							AV(41)=(TG(41)/AW(4))*100

					Case PDSC>110 
							TG(42)=TG(42)+1
							AV(42)=(TG(42)/AW(4))*100
				EndCase
	EndCase		

		If NP>0
				Do Case
					Case PDSC>0 And PDSC<=10
							TG(46)=TG(46)+1
							AV(46)=(TG(46)/AW(4))*100
								
					Case PDSC>10 And PDSC<=20
							TG(47)=TG(47)+1
							AV(47)=(TG(47)/AW(4))*100

					Case PDSC>20 And PDSC<=30
							TG(48)=TG(48)+1
							AV(48)=(TG(48)/AW(4))*100

					Case PDSC>30 And PDSC<=40
							TG(49)=TG(49)+1
							AV(49)=(TG(49)/AW(4))*100

					Case PDSC>40 And PDSC<=50
							TG(50)=TG(50)+1
							AV(50)=(TG(50)/AW(4))*100

					Case PDSC>50 And PDSC<=60
							TG(51)=TG(51)+1
							AV(51)=(TG(51)/AW(4))*100

					Case PDSC>60 And PDSC<=70
							TG(52)=TG(52)+1
							AV(52)=(TG(52)/AW(4))*100

					Case PDSC>70 And PDSC<=80
							TG(53)=TG(53)+1
							AV(53)=(TG(53)/AW(4))*100

					Case PDSC>80 And PDSC<=90
							TG(54)=TG(54)+1
							AV(54)=(TG(54)/AW(4))*100

					Case PDSC>90 And PDSC<=100
							TG(55)=TG(55)+1
							AV(55)=(TG(55)/AW(4))*100

					Case PDSC>100 And PDSC<=110
							TG(56)=TG(56)+1
							AV(56)=(TG(56)/AW(4))*100

					Case PDSC>110
							TG(57)=TG(57)+1
							AV(57)=(TG(57)/AW(4))*100
				EndCase
		EndIF
EndScan

Select REG
Set Filter To

Create Cursor REPORTE (Concepto c(20),COL1 c(5),COL2 c(5),COL3 c(5),COL4 c(5))
Append Blank
Replace CONCEPTO With 'Dias en Secas'
Append Blank
Replace CONCEPTO With '  0-10', COL1 With Str(AV(1),5,1),COL2 With Str(AV(16),5,1),COL3 With Str(AV(31),5,1),COL4 With Str(AV(46),5,1)
Append Blank
Replace CONCEPTO With ' 10-20',COL1 With Str(AV(2),5,1),COL2 With Str(AV(17),5,1),COL3 With Str(AV(32),5,1),COL4 With Str(AV(47),5,1)
Append Blank
Replace CONCEPTO With ' 20-30',COL1 With Str(AV(3),5,1),COL2 With Str(AV(18),5,1),COL3 With Str(AV(33),5,1),COL4 With Str(AV(48),5,1)
Append Blank
Replace CONCEPTO With ' 30-40',COL1 With Str(AV(4),5,1),COL2 With Str(AV(19),5,1),COL3 With Str(AV(34),5,1),COL4 With Str(AV(49),5,1)
Append Blank
Replace CONCEPTO With ' 40-50',COL1 With Str(AV(5),5,1),COL2 With Str(AV(20),5,1),COL3 With Str(AV(35),5,1),COL4 With Str(AV(50),5,1)
Append Blank
Replace CONCEPTO With ' 50-60',COL1 With Str(AV(6),5,1),COL2 With Str(AV(21),5,1),COL3 With Str(AV(36),5,1),COL4 With Str(AV(51),5,1)
Append Blank
Replace CONCEPTO With ' 60-70',COL1 With Str(AV(7),5,1),COL2 With Str(AV(22),5,1),COL3 With Str(AV(37),5,1),COL4 With Str(AV(52),5,1)
Append Blank
Replace CONCEPTO With ' 70-80',COL1 With Str(AV(8),5,1),COL2 With Str(AV(23),5,1),COL3 With Str(AV(38),5,1),COL4 With Str(AV(53),5,1)
Append Blank
Replace CONCEPTO With ' 80-90',COL1 With Str(AV(9),5,1),COL2 With Str(AV(24),5,1),COL3 With Str(AV(39),5,1),COL4 With Str(AV(54),5,1)
Append Blank
Replace CONCEPTO With ' 90-100',COL1 With Str(AV(10),5,1),COL2 With Str(AV(25),5,1),COL3 With Str(AV(40),5,1),COL4 With Str(AV(55),5,1)
Append Blank
Replace CONCEPTO With '100-110',COL1 With Str(AV(11),5,1),COL2 With Str(AV(26),5,1),COL3 With Str(AV(41),5,1),COL4 With Str(AV(56),5,1)
Append Blank
Replace CONCEPTO With '110  ->',COL1 With Str(AV(12),5,1),COL2 With Str(AV(27),5,1),COL3 With Str(AV(42),5,1),COL4 With Str(AV(57),5,1)
Append Blank
Replace CONCEPTO With ''

GO TOP
RETURN

* DISTRIBUCION DE DIAS EN LECHE (BAJAS) 
* -------------------------------------
PROCEDURE RV10227
Dimension TG(60),AW(60),AV(60),VP(5),TT(15),TP(15),TD(15),TV(2)
TG=0
AW=0
AV=0
VP=0
TT=0
TP=0
TV=0
Select REG 
Set Order To 2
SET FILTER TO FB2=B
COUNT TO TV(1)

Set Filter To NP>0 And FB2#B AND FB2>=DATE()-365
Count To AW(1)

Scan 
	Do Case
		Case NP=1
			AW(1)=AW(1)+1	
		Case NP=2
			AW(2)=AW(2)+1	
		Case NP>=3
			AW(3)=AW(3)+1	
	EndCase
			AW(4)=AW(4)+1	
EndScan

Scan 
	xDEL=(FB2-FPAR)
	Do Case
		Case NP=1
				Do Case
					Case xDEL>0 And xDEL<=30
							TG(1)=TG(1)+1
							AV(1)=(TG(1)/AW(4))*100
															
					Case xDEL>30 And xDEL<=60
							TG(2)=TG(2)+1
							AV(2)=(TG(2)/AW(4))*100
						
					Case xDEL>60 And xDEL<=90
							TG(3)=TG(3)+1
							AV(3)=(TG(3)/AW(4))*100

					Case xDEL>90 And xDEL<=120
							TG(4)=TG(4)+1
							AV(4)=(TG(4)/AW(4))*100

					Case xDEL>120 And xDEL<=150
							TG(5)=TG(5)+1
							AV(5)=(TG(5)/AW(4))*100

					Case xDEL>150 And xDEL<=180
							TG(6)=TG(6)+1
							AV(6)=(TG(6)/AW(4))*100

					Case xDEL>180 And xDEL<=210
							TG(7)=TG(7)+1
							AV(7)=(TG(7)/AW(4))*100

					Case xDEL>210 And xDEL<=240
							TG(8)=TG(8)+1
							AV(8)=(TG(8)/AW(4))*100

					Case xDEL>240 And xDEL<=270
							TG(9)=TG(9)+1
							AV(9)=(TG(9)/AW(4))*100

					Case xDEL>270 And xDEL<=300
							TG(10)=TG(10)+1
							AV(10)=(TG(10)/AW(4))*100

					Case xDEL>300 And xDEL<=330
							TG(11)=TG(11)+1
							AV(11)=(TG(11)/AW(4))*100

					Case xDEL>330
							TG(12)=TG(12)+1
							AV(12)=(TG(12)/AW(4))*100
				EndCase

		Case NP=2
				Do Case
					Case xDEL>0 And xDEL<=30
							TG(16)=TG(16)+1
							AV(16)=(TG(16)/AW(4))*100
								
					Case xDEL>30 And xDEL<=60
							TG(17)=TG(17)+1
							AV(17)=(TG(17)/AW(4))*100

					Case xDEL>60 And xDEL<=90
							TG(18)=TG(18)+1
							AV(18)=(TG(18)/AW(4))*100

					Case xDEL>90 And xDEL<=120
							TG(19)=TG(19)+1
							AV(19)=(TG(19)/AW(4))*100

					Case xDEL>120 And xDEL<=150
							TG(20)=TG(20)+1
							AV(20)=(TG(20)/AW(4))*100

					Case xDEL>150 And xDEL<=180
							TG(21)=TG(21)+1
							AV(21)=(TG(21)/AW(4))*100

					Case xDEL>180 And xDEL<=210
							TG(22)=TG(22)+1
							AV(22)=(TG(22)/AW(4))*100

					Case xDEL>210 And xDEL<=240
							TG(23)=TG(23)+1
							AV(23)=(TG(23)/AW(4))*100

					Case xDEL>240 And xDEL<=270
							TG(24)=TG(24)+1
							AV(24)=(TG(24)/AW(4))*100

					Case xDEL>270 And xDEL<=300
							TG(25)=TG(25)+1
							AV(25)=(TG(25)/AW(4))*100

					Case xDEL>300 And xDEL<=330
							TG(26)=TG(26)+1
							AV(26)=(TG(26)/AW(4))*100

					Case xDEL>330 
							TG(27)=TG(27)+1
							AV(27)=(TG(27)/AW(4))*100
				EndCase

		Case NP>=3
				Do Case
					Case xDEL>0 And xDEL<=30
							TG(31)=TG(31)+1
							AV(31)=(TG(31)/AW(4))*100
								
					Case xDEL>30 And xDEL<=60
							TG(32)=TG(32)+1
							AV(32)=(TG(32)/AW(4))*100

					Case xDEL>60 And xDEL<=90
							TG(33)=TG(33)+1
							AV(33)=(TG(33)/AW(4))*100

					Case xDEL>90 And xDEL<=120
							TG(34)=TG(34)+1
							AV(34)=(TG(34)/AW(4))*100

					Case xDEL>120 And xDEL<=150
							TG(35)=TG(35)+1
							AV(35)=(TG(35)/AW(4))*100

					Case xDEL>150 And xDEL<=180
							TG(36)=TG(36)+1
							AV(36)=(TG(36)/AW(4))*100

					Case xDEL>180 And xDEL<=210
							TG(37)=TG(37)+1
							AV(37)=(TG(37)/AW(4))*100

					Case xDEL>210 And xDEL<=240
							TG(38)=TG(38)+1
							AV(38)=(TG(38)/AW(4))*100

					Case xDEL>240 And xDEL<=270
							TG(39)=TG(39)+1
							AV(39)=(TG(39)/AW(4))*100

					Case xDEL>270 And xDEL<=300
							TG(40)=TG(40)+1
							AV(40)=(TG(40)/AW(4))*100

					Case xDEL>300 And xDEL<=330
							TG(41)=TG(41)+1
							AV(41)=(TG(41)/AW(4))*100

					Case xDEL>330 
							TG(42)=TG(42)+1
							AV(42)=(TG(42)/AW(4))*100
				EndCase
	EndCase		

		If NP>0
				Do Case
					Case xDEL>=0 And xDEL<=30
							TG(46)=TG(46)+1
							AV(46)=(TG(46)/AW(4))*100
															
					Case xDEL>30 And xDEL<=60
							TG(47)=TG(47)+1
							AV(47)=(TG(47)/AW(4))*100
                                                        
					Case xDEL>60 And xDEL<=90
							TG(48)=TG(48)+1
							AV(48)=(TG(48)/AW(4))*100
                          							
					Case xDEL>90 And xDEL<=120
							TG(49)=TG(49)+1
							AV(49)=(TG(49)/AW(4))*100
															
					Case xDEL>120 And xDEL<=150
							TG(50)=TG(50)+1
							AV(50)=(TG(50)/AW(4))*100

					Case xDEL>150 And xDEL<=180
							TG(51)=TG(51)+1
							AV(51)=(TG(51)/AW(4))*100

					Case xDEL>180 And xDEL<=210
							TG(52)=TG(52)+1
							AV(52)=(TG(52)/AW(4))*100

					Case xDEL>210 And xDEL<=240
							TG(53)=TG(53)+1
							AV(53)=(TG(53)/AW(4))*100

					Case xDEL>240 And xDEL<=270
							TG(54)=TG(54)+1
							AV(54)=(TG(54)/AW(4))*100

					Case xDEL>270 And xDEL<=300
							TG(55)=TG(55)+1
							AV(55)=(TG(55)/AW(4))*100

					Case xDEL>300 And xDEL<=330
							TG(56)=TG(56)+1
							AV(56)=(TG(56)/AW(4))*100

					Case xDEL>330
							TG(57)=TG(57)+1
							AV(57)=(TG(57)/AW(4))*100
				EndCase
		EndIF
ENDSCAN

TP(1)=AV(46)
TP(2)=AV(46)+AV(47)
TP(3)=AV(46)+AV(47)+AV(48)
TP(4)=AV(46)+AV(47)+AV(48)+AV(49)
TP(5)=AV(46)+AV(47)+AV(48)+AV(49)+AV(50)
TP(6)=AV(46)+AV(47)+AV(48)+AV(49)+AV(50)+AV(51)
TP(7)=AV(46)+AV(47)+AV(48)+AV(49)+AV(50)+AV(51)+AV(52)
TP(8)=AV(46)+AV(47)+AV(48)+AV(49)+AV(50)+AV(51)+AV(52)+AV(53)
TP(9)=AV(46)+AV(47)+AV(48)+AV(49)+AV(50)+AV(51)+AV(52)+AV(53)+AV(54)
TP(10)=AV(46)+AV(47)+AV(48)+AV(49)+AV(50)+AV(51)+AV(52)+AV(53)+AV(54)+AV(55)
TP(11)=AV(46)+AV(47)+AV(48)+AV(49)+AV(50)+AV(51)+AV(52)+AV(53)+AV(54)+AV(55)+AV(56)
TP(12)=AV(46)+AV(47)+AV(48) +AV(49)+AV(50)+AV(51)+AV(52)+AV(53)+AV(54)+AV(55)+AV(56)+AV(57)


TT(1)=TG(46)
TT(2)=TG(46)+TG(47)
TT(3)=TG(46)+TG(47)+TG(48)
TT(4)=TG(46)+TG(47)+TG(48)+TG(49)
TT(5)=TG(46)+TG(47)+TG(48)+TG(49)+TG(50)
TT(6)=TG(46)+TG(47)+TG(48)+TG(49)+TG(50)+TG(51)
TT(7)=TG(46)+TG(47)+TG(48)+TG(49)+TG(50)+TG(51)+TG(52)
TT(8)=TG(46)+TG(47)+TG(48)+TG(49)+TG(50)+TG(51)+TG(52)+TG(53)
TT(9)=TG(46)+TG(47)+TG(48)+TG(49)+TG(50)+TG(51)+TG(52)+TG(53)+TG(54)
TT(10)=TG(46)+TG(47)+TG(48)+TG(49)+TG(50)+TG(51)+TG(52)+TG(53)+TG(54)+TG(55)
TT(11)=TG(46)+TG(47)+TG(48)+TG(49)+TG(50)+TG(51)+TG(52)+TG(53)+TG(54)+TG(55)+TG(56)
TT(12)=TG(46)+TG(47)+TG(48) +TG(49)+TG(50)+TG(51)+TG(52)+TG(53)+TG(54)+TG(55)+TG(56)+TG(57)

TD(1)=(TG(46)/TV(1))*100
TD(2)=(TG(47)/TV(1))*100
TD(3)=(TG(48)/TV(1))*100
TD(4)=(TG(49)/TV(1))*100
TD(5)=(TG(50)/TV(1))*100
TD(6)=(TG(51)/TV(1))*100
TD(7)=(TG(52)/TV(1))*100
TD(8)=(TG(53)/TV(1))*100
TD(9)=(TG(54)/TV(1))*100
TD(10)=(TG(55)/TV(1))*100
TD(11)=(TG(56)/TV(1))*100
TD(12)=(TG(57)/TV(1))*100

TD(13)=(TG(46)+TG(47)+TG(48) +TG(49)+TG(50)+TG(51)+TG(52)+TG(53)+TG(54)+TG(55)+TG(56)+TG(57))/TV(1)*100

Select REG
Set Filter To

Create Cursor REPORTE (Concepto c(20),COL1 c(5),COL2 c(5),COL3 c(5),COL4 c(5),COL5 c(4),COL6 c(4),COL7 c(5),COL8 c(5))
Append Blank
Replace CONCEPTO With 'Dias en Leche'
Append Blank
Replace CONCEPTO With '  0-30', COL1 With Str(AV(1),5,1),COL2 With Str(AV(16),5,1),COL3 With Str(AV(31),5,1),COL4 With Str(AV(46),5,1),COL5 WITH STR(TG(46),3),COL6 WITH STR(TT(1),4),COL7 WITH STR(TP(1),5,1),COL8 WITH STR(TD(1),5,1)
Append Blank
Replace CONCEPTO With ' 30-60',COL1 With Str(AV(2),5,1),COL2 With Str(AV(17),5,1),COL3 With Str(AV(32),5,1),COL4 With Str(AV(47),5,1),COL5 WITH STR(TG(47),3),COL6 WITH STR(TT(2),4),COL7 WITH STR(TP(2),5,1),COL8 WITH STR(TD(2),5,1)
Append Blank
Replace CONCEPTO With ' 60-90',COL1 With Str(AV(3),5,1),COL2 With Str(AV(18),5,1),COL3 With Str(AV(33),5,1),COL4 With Str(AV(48),5,1),COL5 WITH STR(TG(48),3),COL6 WITH STR(TT(3),4),COL7 WITH STR(TP(3),5,1),COL8 WITH STR(TD(3),5,1)
APPEND Blank
Replace CONCEPTO With ' 90-120',COL1 With Str(AV(4),5,1),COL2 With Str(AV(19),5,1),COL3 With Str(AV(34),5,1),COL4 With Str(AV(49),5,1),COL5 WITH STR(TG(49),3),COL6 WITH STR(TT(4),4),COL7 WITH STR(TP(4),5,1),COL8 WITH STR(TD(4),5,1)
APPEND Blank
Replace CONCEPTO With '120-150',COL1 With Str(AV(5),5,1),COL2 With Str(AV(20),5,1),COL3 With Str(AV(35),5,1),COL4 With Str(AV(50),5,1),COL5 WITH STR(TG(50),3),COL6 WITH STR(TT(5),4),COL7 WITH STR(TP(5),5,1),COL8 WITH STR(TD(5),5,1)
Append Blank
Replace CONCEPTO With '150-180',COL1 With Str(AV(6),5,1),COL2 With Str(AV(21),5,1),COL3 With Str(AV(36),5,1),COL4 With Str(AV(51),5,1),COL5 WITH STR(TG(51),3),COL6 WITH STR(TT(6),4),COL7 WITH STR(TP(6),5,1),COL8 WITH STR(TD(6),5,1)
Append Blank
Replace CONCEPTO With '180-210',COL1 With Str(AV(7),5,1),COL2 With Str(AV(22),5,1),COL3 With Str(AV(37),5,1),COL4 With Str(AV(52),5,1),COL5 WITH STR(TG(52),3),COL6 WITH STR(TT(7),4),COL7 WITH STR(TP(7),5,1),COL8 WITH STR(TD(7),5,1)
Append Blank
Replace CONCEPTO With '210-240',COL1 With Str(AV(8),5,1),COL2 With Str(AV(23),5,1),COL3 With Str(AV(38),5,1),COL4 With Str(AV(53),5,1),COL5 WITH STR(TG(53),3),COL6 WITH STR(TT(8),4),COL7 WITH STR(TP(8),5,1),COL8 WITH STR(TD(8),5,1)
Append Blank
Replace CONCEPTO With '240-270',COL1 With Str(AV(9),5,1),COL2 With Str(AV(24),5,1),COL3 With Str(AV(39),5,1),COL4 With Str(AV(54),5,1),COL5 WITH STR(TG(54),3),COL6 WITH STR(TT(9),4),COL7 WITH STR(TP(9),5,1),COL8 WITH STR(TD(9),5,1)
Append Blank
Replace CONCEPTO With '270-300',COL1 With Str(AV(10),5,1),COL2 With Str(AV(25),5,1),COL3 With Str(AV(40),5,1),COL4 With Str(AV(55),5,1),COL5 WITH STR(TG(55),3),COL6 WITH STR(TT(10),4),COL7 WITH STR(TP(10),5,1),COL8 WITH STR(TD(10),5,1)
Append Blank
Replace CONCEPTO With '30-330',COL1 With Str(AV(11),5,1),COL2 With Str(AV(26),5,1),COL3 With Str(AV(41),5,1),COL4 With Str(AV(56),5,1),COL5 WITH STR(TG(56),3),COL6 WITH STR(TT(11),4),COL7 WITH STR(TP(11),5,1),COL8 WITH STR(TD(11),5,1) 
Append Blank
Replace CONCEPTO With '330  ->',COL1 With Str(AV(12),5,1),COL2 With Str(AV(27),5,1),COL3 With Str(AV(42),5,1),COL4 With Str(AV(57),5,1),COL5 WITH STR(TG(57),3),COL6 WITH STR(TT(12),4),COL7 WITH STR(TP(12),5,1),COL8 WITH STR(TD(12),5,1)
Append Blank
Replace CONCEPTO WITH 'TOTAL %',COL8 WITH STR(TD(13),5,1)
APPEND Blank
Replace CONCEPTO With ''

GO TOP
RETURN


* LISTADO DE ARETES RFID REPETIDOS
* --------------------------------
PROCEDURE RV10224
CREATE TABLE XDATOS (ID n(5),CORR n(3),RFID c(15),NP n(3),PSV c(1),FTXR d(8))

SELECT REG
SET ORDER TO 9
xRFID=REG.RFID
xID=REG.ID
xCORR=REG.CORR
xNP=REG.NP
xPSV=REG.PSV

GO TOP
SCAN FOR RFID#SPACE(15) 

	IF RIGHT(PADL(ALLTRIM(REG.RFID),15,'0'),8)=RIGHT(PADL(ALLTRIM(xRFID),15,'0'),8) AND REG.ID#XID
		SELECT xDATOS
		APPEND BLANK
		REPLACE ID WITH XID,RFID WITH XRFID,CORR WITH xCORR,NP WITH xNP,PSV WITH xPSV 
		APPEND BLANK
		REPLACE ID WITH REG.ID,RFID WITH REG.RFID,CORR WITH REG.CORR,NP WITH REG.NP,PSV WITH REG.PSV
		SELECT REG
	ENDIF
	xRFID=REG.RFID
	xID=REG.ID
	xCORR=REG.CORR
	xNP=REG.NP
	xPSV=REG.PSV
ENDSCAN
SELECT xDATOS
SELECT REG
RETURN


* LISTADO DE IDE REPETIDAS
* --------------------------------
PROCEDURE RV10231
CREATE TABLE XDATOS (ID n(5),CORR n(3),IDE c(10),NP n(3),PSV c(1),FTXR d(8))

SELECT REG
SET ORDER TO 8
xIDE=REG.IDE
xID=REG.ID
xCORR=REG.CORR
xNP=REG.NP
xPSV=REG.PSV
GO TOP
SCAN FOR IDE#SPACE(10)
	IF REG.IDE=xIDE AND REG.ID#XID
		SELECT xDATOS
		APPEND BLANK
		REPLACE ID WITH XID,IDE WITH XIDE,CORR WITH xCORR,NP WITH xNP,PSV WITH xPSV
		APPEND BLANK
		REPLACE ID WITH REG.ID,IDE WITH REG.IDE,CORR WITH REG.CORR,NP WITH REG.NP,PSV WITH REG.PSV
		SELECT REG
	ENDIF
	xIDE=REG.IDE
	xID=REG.ID
	xCORR=REG.CORR
	xNP=REG.NP
	xPSV=REG.PSV
ENDSCAN
SELECT xDATOS
SELECT REG
RETURN


* LISTADO DE ARETES SNGA REPETIDOS
* --------------------------------
PROCEDURE RV10233
CREATE TABLE XDATOS (ID n(5),CORR n(3),IDAL c(15),NP n(3),PSV c(1),FTXR d(8))

SELECT REG
SET ORDER TO 10

xIDAL=REG.IDAL
xID=REG.ID
xCORR=REG.CORR
xNP=REG.NP
xPSV=REG.PSV

GO TOP
SCAN FOR IDAL#SPACE(15) 
	IF RIGHT(PADL(ALLTRIM(REG.IDAL),15,'0'),8)=RIGHT(PADL(ALLTRIM(xIDAL),15,'0'),8) AND REG.ID#XID
		SELECT xDATOS
		APPEND BLANK
		REPLACE ID WITH XID,IDAL WITH XIDAL,CORR WITH xCORR,NP WITH xNP,PSV WITH xPSV 
		APPEND BLANK
		REPLACE ID WITH REG.ID,IDAL WITH REG.IDAL,CORR WITH REG.CORR,NP WITH REG.NP,PSV WITH REG.PSV
		SELECT REG
	ENDIF
	xIDAL=REG.IDAL
	xID=REG.ID
	xCORR=REG.CORR
	xNP=REG.NP
	xPSV=REG.PSV
ENDSCAN
SELECT xDATOS
SELECT REG
RETURN


