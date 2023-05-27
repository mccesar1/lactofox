* ESTADISTICA DE PRODUCCION POR CORRAL Y ULTIMAS DIEZ PESADAS  ³
* ----------------------------------------------------------------
PROCEDURE RV1010
PARAMETERS nLOTE,xP1,xP2,xFILTER
Public xLOTE

If nLOTE=0
	xLote="And CORR>0"
Else
	xLote="And CORR=nLote"	
EndIf	

Declare aw(60),pw(16),tw(10),tv(5)
mc=0
pw=0
tw=0
tv=0

Create Table xDATOS (CONCEPTO c(8),COL1 c(5),COL2 c(5),COL3 c(5),COL4 c(5),COL5 c(5),COL6 c(5),COL7 c(5),COL8 c(5),COL9 c(5),COL10 c(4),COL11 c(4),COL12 c(4),COL13 c(4),COL14 c(4),COL15 c(4),COL16 c(4),COL17 c(4),COL18 c(4),COL19 c(4),COL20 c(10),COL21 c(5),COL22 c(5),COL23 c(3),COL24 c(1))

Select PROD
Set Order To 2

Select REG
Set Order To 2
GO TOP
Set Filter To NP>0 And FSEC=B And FB2=B And PRM>=xP1 And PRM<=xP2 And &xFilter &xLote

SCAN
xID=REG.ID
xNP=REG.NP
xAVAL=0
If M305>0
	xAVAL=(M305/X305)*100
EndIf
** Traslada las pesadas a variables
** --------------------------------
xx=0
aw=space(2)
	Select PROD
	xIDP=Str(xID,5)+Dtoc(FP)
	Seek Left(xIDP,5)
	
	If Found()
			Scan While xID=PROD.ID And xNP=PROD.NP and xx<10
				xx=xx+1
				aw(xx)=alltrim(aw(xx)+str(prd,3))
				If Len(aw(xx))=1
 					aw(xx)=" "+aw(xx)
				Endif
			Endscan
	Else
		aw=space(2)
		xx=0
	Endif
** --------------------------------

Select xDATOS
Append Blank
Replace CONCEPTO With STR(REG.ID,5),COL1 With Str(REG.CORR,3),COL2 With Str(REG.NP,2),;
				COL3 With REG.STAT,COL4 With Str(REG.DIA,4),COL5 With Str(REG.DPR,4),COL6 With Str(REG.DAB,4),;
				COL7 With Str(REG.NS,2),COL8 With Str(xAVAL,3),COL9 With Str(REG.PRX,5,1),COL10 With aw(10),;
				COL11 With aw(9),COL12 With aw(8),COL13 With aw(7),COL14 With aw(6),COL15 With aw(5),COL16 With aw(4),;
				COL17 With aw(3),COL18 With aw(2),COL19 With aw(1),COL20 With DTOC(REG.PESA),;
				COL21 With Str(REG.PLAC,5,1),COL22 With Str(REG.DPIC,3),COL23 With Str(REG.CONDC,3,1),COL24 With REG.PSV
	
*Select REG


*	xx=1
*	aw=0
*	xID=REG.ID

	* Estadistica de Estado Reproductivo
*	Do Case
*		Case Left(REG.STAT,1)="I"
*			tv(1)=tv(1)+1
*		Case Left(REG.STAT,1)="C"
*			tv(2)=tv(2)+1
*		Otherwise
*		    tv(3)=tv(3)+1
*	Endcase
ENDSCAN
Select REG
Set Filter TO

Select xDATOS
RETURN


* DISTRIBUCION POR DIAS EN LECHE
* ------------------------------
PROCEDURE RV1069
Dimension TG(75),AW(75),AV(75),TT(20)
TG=0
AW=0
AV=0
TT=0

Use REG In 1
Select REG

Select ID,FPAR,NP From REG Where NP>0 And FSEC=B And FB2=B Order By ID Into Cursor xDIAS
Scan
	Do Case
		Case NP=1
				Do Case
					Case (DATE()-FPAR)>=0 And (DATE()-FPAR)<=30
							TG(1)=TG(1)+1
								
					Case (DATE()-FPAR)>30 And (DATE()-FPAR)<=60
							TG(2)=TG(2)+1

					Case (DATE()-FPAR)>60 And (DATE()-FPAR)<=90
							TG(3)=TG(3)+1

					Case (DATE()-FPAR)>90 And (DATE()-FPAR)<=120
							TG(4)=TG(4)+1

					Case (DATE()-FPAR)>120 And (DATE()-FPAR)<=150
							TG(5)=TG(5)+1

					Case (DATE()-FPAR)>150 And (DATE()-FPAR)<=180
							TG(6)=TG(6)+1

					Case (DATE()-FPAR)>180 And (DATE()-FPAR)<=210
							TG(7)=TG(7)+1

					Case (DATE()-FPAR)>210 And (DATE()-FPAR)<=240
							TG(8)=TG(8)+1

					Case (DATE()-FPAR)>240 And (DATE()-FPAR)<=270
							TG(9)=TG(9)+1

					Case (DATE()-FPAR)>270 And (DATE()-FPAR)<=300
							TG(10)=TG(10)+1

					Case (DATE()-FPAR)>300 And (DATE()-FPAR)<=330
							TG(11)=TG(11)+1

					Case (DATE()-FPAR)>330 And (DATE()-FPAR)<=360
							TG(12)=TG(12)+1

					Case (DATE()-FPAR)>360 And (DATE()-FPAR)<=390
							TG(13)=TG(13)+1

					Case (DATE()-FPAR)>390 And (DATE()-FPAR)<=420
							TG(14)=TG(14)+1

					Case (DATE()-FPAR)>420 
							TG(15)=TG(15)+1

				EndCase

		Case NP=2
				Do Case
					Case (DATE()-FPAR)>=0 And (DATE()-FPAR)<=30
							TG(16)=TG(16)+1
								
					Case (DATE()-FPAR)>30 And (DATE()-FPAR)<=60
							TG(17)=TG(17)+1

					Case (DATE()-FPAR)>60 And (DATE()-FPAR)<=90
							TG(18)=TG(18)+1

					Case (DATE()-FPAR)>90 And (DATE()-FPAR)<=120
							TG(19)=TG(19)+1

					Case (DATE()-FPAR)>120 And (DATE()-FPAR)<=150
							TG(20)=TG(20)+1

					Case (DATE()-FPAR)>150 And (DATE()-FPAR)<=180
							TG(21)=TG(21)+1

					Case (DATE()-FPAR)>180 And (DATE()-FPAR)<=210
							TG(22)=TG(22)+1

					Case (DATE()-FPAR)>210 And (DATE()-FPAR)<=240
							TG(23)=TG(23)+1

					Case (DATE()-FPAR)>240 And (DATE()-FPAR)<=270
							TG(24)=TG(24)+1

					Case (DATE()-FPAR)>270 And (DATE()-FPAR)<=300
							TG(25)=TG(25)+1

					Case (DATE()-FPAR)>300 And (DATE()-FPAR)<=330
							TG(26)=TG(26)+1

					Case (DATE()-FPAR)>330 And (DATE()-FPAR)<=360
							TG(27)=TG(27)+1

					Case (DATE()-FPAR)>360 And (DATE()-FPAR)<=390
							TG(28)=TG(28)+1

					Case (DATE()-FPAR)>390 And (DATE()-FPAR)<=420
							TG(29)=TG(29)+1

					Case (DATE()-FPAR)>420 
							TG(30)=TG(30)+1
				EndCase

		Case NP>=3
				Do Case
					Case (DATE()-FPAR)>=0 And (DATE()-FPAR)<=30
							TG(31)=TG(31)+1
								
					Case (DATE()-FPAR)>30 And (DATE()-FPAR)<=60
							TG(32)=TG(32)+1

					Case (DATE()-FPAR)>60 And (DATE()-FPAR)<=90
							TG(33)=TG(33)+1

					Case (DATE()-FPAR)>90 And (DATE()-FPAR)<=120
							TG(34)=TG(34)+1

					Case (DATE()-FPAR)>120 And (DATE()-FPAR)<=150
							TG(35)=TG(35)+1

					Case (DATE()-FPAR)>150 And (DATE()-FPAR)<=180
							TG(36)=TG(36)+1

					Case (DATE()-FPAR)>180 And (DATE()-FPAR)<=210
							TG(37)=TG(37)+1

					Case (DATE()-FPAR)>210 And (DATE()-FPAR)<=240
							TG(38)=TG(38)+1

					Case (DATE()-FPAR)>240 And (DATE()-FPAR)<=270
							TG(39)=TG(39)+1

					Case (DATE()-FPAR)>270 And (DATE()-FPAR)<=300
							TG(40)=TG(40)+1

					Case (DATE()-FPAR)>300 And (DATE()-FPAR)<=330
							TG(41)=TG(41)+1

					Case (DATE()-FPAR)>330 And (DATE()-FPAR)<=360
							TG(42)=TG(42)+1

					Case (DATE()-FPAR)>360 And (DATE()-FPAR)<=390
							TG(43)=TG(43)+1

					Case (DATE()-FPAR)>390 And (DATE()-FPAR)<=420
							TG(44)=TG(44)+1

					Case (DATE()-FPAR)>420 
							TG(45)=TG(45)+1
				EndCase
	EndCase		

		If NP>0
				Do Case
					Case (DATE()-FPAR)>=0 And (DATE()-FPAR)<=30
							TG(46)=TG(46)+1
								
					Case (DATE()-FPAR)>30 And (DATE()-FPAR)<=60
							TG(47)=TG(47)+1

					Case (DATE()-FPAR)>60 And (DATE()-FPAR)<=90
							TG(48)=TG(48)+1

					Case (DATE()-FPAR)>90 And (DATE()-FPAR)<=120
							TG(49)=TG(49)+1

					Case (DATE()-FPAR)>120 And (DATE()-FPAR)<=150
							TG(50)=TG(50)+1

					Case (DATE()-FPAR)>150 And (DATE()-FPAR)<=180
							TG(51)=TG(51)+1

					Case (DATE()-FPAR)>180 And (DATE()-FPAR)<=210
							TG(52)=TG(52)+1

					Case (DATE()-FPAR)>210 And (DATE()-FPAR)<=240
							TG(53)=TG(53)+1

					Case (DATE()-FPAR)>240 And (DATE()-FPAR)<=270
							TG(54)=TG(54)+1

					Case (DATE()-FPAR)>270 And (DATE()-FPAR)<=300
							TG(55)=TG(55)+1

					Case (DATE()-FPAR)>300 And (DATE()-FPAR)<=330
							TG(56)=TG(56)+1

					Case (DATE()-FPAR)>330 And (DATE()-FPAR)<=360
							TG(57)=TG(57)+1

					Case (DATE()-FPAR)>360 And (DATE()-FPAR)<=390
							TG(58)=TG(58)+1

					Case (DATE()-FPAR)>390 And (DATE()-FPAR)<=420
							TG(59)=TG(59)+1

					Case (DATE()-FPAR)>420 
							TG(60)=TG(60)+1
				EndCase
		EndIF
ENDSCAN

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
TT(13)=TG(46)+TG(47)+TG(48) +TG(49)+TG(50)+TG(51)+TG(52)+TG(53)+TG(54)+TG(55)+TG(56)+TG(57)+TG(58)
TT(14)=TG(46)+TG(47)+TG(48) +TG(49)+TG(50)+TG(51)+TG(52)+TG(53)+TG(54)+TG(55)+TG(56)+TG(57)+TG(58)+TG(59)
TT(15)=TG(46)+TG(47)+TG(48) +TG(49)+TG(50)+TG(51)+TG(52)+TG(53)+TG(54)+TG(55)+TG(56)+TG(57)+TG(58)+TG(59)+TG(60)


Create Cursor REPORTE (Concepto c(20),COL1 c(5),COL2 c(5),COL3 c(5),COL4 c(5),COL5 c(5))
Append Blank
Replace CONCEPTO With 'Dias en Leche'
Append Blank
Replace CONCEPTO With '  0 -  30',COL1 With Str(TG(1),4),COL2 With Str(TG(16),4),COL3 With Str(TG(31),4),COL4 With Str(TG(46),4),COL5 WITH STR(TT(1),4)
Append Blank
Replace CONCEPTO With ' 30 -  60',COL1 With Str(TG(2),4),COL2 With Str(TG(17),4),COL3 With Str(TG(32),4),COL4 With Str(TG(47),4),COL5 WITH STR(TT(2),4)
Append Blank
Replace CONCEPTO With ' 60 -  90',COL1 With Str(TG(3),4),COL2 With Str(TG(18),4),COL3 With Str(TG(33),4),COL4 With Str(TG(48),4),COL5 WITH STR(TT(3),4)
Append Blank
Replace CONCEPTO With ' 90 - 120',COL1 With Str(TG(4),4),COL2 With Str(TG(19),4),COL3 With Str(TG(34),4),COL4 With Str(TG(49),4),COL5 WITH STR(TT(4),4)
Append Blank
Replace CONCEPTO With '120 - 150',COL1 With Str(TG(5),4),COL2 With Str(TG(20),4),COL3 With Str(TG(35),4),COL4 With Str(TG(50),4),COL5 WITH STR(TT(5),4)
Append Blank
Replace CONCEPTO With '150 - 180',COL1 With Str(TG(6),4),COL2 With Str(TG(21),4),COL3 With Str(TG(36),4),COL4 With Str(TG(51),4),COL5 WITH STR(TT(6),4)
Append Blank
Replace CONCEPTO With '180 - 210',COL1 With Str(TG(7),4),COL2 With Str(TG(22),4),COL3 With Str(TG(37),4),COL4 With Str(TG(52),4),COL5 WITH STR(TT(7),4)
Append Blank
Replace CONCEPTO With '210 - 240',COL1 With Str(TG(8),4),COL2 With Str(TG(23),4),COL3 With Str(TG(38),4),COL4 With Str(TG(53),4),COL5 WITH STR(TT(8),4)
Append Blank
Replace CONCEPTO With '240 - 270',COL1 With Str(TG(9),4),COL2 With Str(TG(24),4),COL3 With Str(TG(39),4),COL4 With Str(TG(54),4),COL5 WITH STR(TT(9),4)
Append Blank
Replace CONCEPTO With '270 - 300',COL1 With Str(TG(10),4),COL2 With Str(TG(25),4),COL3 With Str(TG(40),4),COL4 With Str(TG(55),4),COL5 WITH STR(TT(10),4)
Append Blank
Replace CONCEPTO With '300 - 330',COL1 With Str(TG(11),4),COL2 With Str(TG(26),4),COL3 With Str(TG(41),4),COL4 With Str(TG(56),4),COL5 WITH STR(TT(11),4)
Append Blank
Replace CONCEPTO With '330 - 360',COL1 With Str(TG(12),4),COL2 With Str(TG(27),4),COL3 With Str(TG(42),4),COL4 With Str(TG(57),4),COL5 WITH STR(TT(12),4)
Append Blank
Replace CONCEPTO With '360 - 390',COL1 With Str(TG(13),4),COL2 With Str(TG(28),4),COL3 With Str(TG(43),4),COL4 With Str(TG(58),4)COL5 WITH STR(TT(13),4)
Append Blank
Replace CONCEPTO With '390 - 420',COL1 With Str(TG(14),4),COL2 With Str(TG(29),4),COL3 With Str(TG(44),4),COL4 With Str(TG(59),4),COL5 WITH STR(TT(14),4)
Append Blank
Replace CONCEPTO With '420 -  > ',COL1 With Str(TG(15),4),COL2 With Str(TG(30),4),COL3 With Str(TG(45),4),COL4 With Str(TG(60),4),COL5 WITH STR(TT(15),4)
Append Blank
Replace CONCEPTO With ''

GO TOP
RETURN



* ANALISIS DE LA PESADA
* ---------------------
PROCEDURE RV1091
Dimension TG(75),AW(75),AV(75)
TG=0
AW=0
AV=0
Use REG In 1
Select REG
Calculate MAX(PESA) To mPESA

Select ID,FPAR,NP,PESA,PRM From REG Where NP>0 And PESA=mPESA And PRM>0 Order By ID Into Cursor xPESADA
Scan
	Do Case
		Case NP=1
				Do Case
					Case (PESA-FPAR)>0 And (PESA-FPAR)<=30
							TG(1)=TG(1)+1
							AW(1)=AW(1)+PRM
							AV(1)=AW(1)/TG(1)				
								
					Case (PESA-FPAR)>30 And (PESA-FPAR)<=60
							TG(2)=TG(2)+1
							AW(2)=AW(2)+PRM
							AV(2)=AW(2)/TG(2)				

					Case (PESA-FPAR)>60 And (PESA-FPAR)<=90
							TG(3)=TG(3)+1
							AW(3)=AW(3)+PRM
							AV(3)=AW(3)/TG(3)				

					Case (PESA-FPAR)>90 And (PESA-FPAR)<=120
							TG(4)=TG(4)+1
							AW(4)=AW(4)+PRM
							AV(4)=AW(4)/TG(4)				

					Case (PESA-FPAR)>120 And (PESA-FPAR)<=150
							TG(5)=TG(5)+1
							AW(5)=AW(5)+PRM
							AV(5)=AW(5)/TG(5)				

					Case (PESA-FPAR)>150 And (PESA-FPAR)<=180
							TG(6)=TG(6)+1
							AW(6)=AW(6)+PRM
							AV(6)=AW(6)/TG(6)				

					Case (PESA-FPAR)>180 And (PESA-FPAR)<=210
							TG(7)=TG(7)+1
							AW(7)=AW(7)+PRM
							AV(7)=AW(7)/TG(7)				

					Case (PESA-FPAR)>210 And (PESA-FPAR)<=240
							TG(8)=TG(8)+1
							AW(8)=AW(8)+PRM
							AV(8)=AW(8)/TG(8)				

					Case (PESA-FPAR)>240 And (PESA-FPAR)<=270
							TG(9)=TG(9)+1
							AW(9)=AW(9)+PRM
							AV(9)=AW(9)/TG(9)				

					Case (PESA-FPAR)>270 And (PESA-FPAR)<=300
							TG(10)=TG(10)+1
							AW(10)=AW(10)+PRM
							AV(10)=AW(10)/TG(10)				

					Case (PESA-FPAR)>300 And (PESA-FPAR)<=330
							TG(11)=TG(11)+1
							AW(11)=AW(11)+PRM
							AV(11)=AW(11)/TG(11)				

					Case (PESA-FPAR)>330 And (PESA-FPAR)<=360
							TG(12)=TG(12)+1
							AW(12)=AW(12)+PRM
							AV(12)=AW(12)/TG(12)				

					Case (PESA-FPAR)>360 And (PESA-FPAR)<=390
							TG(13)=TG(13)+1
							AW(13)=AW(13)+PRM
							AV(13)=AW(13)/TG(13)				

					Case (PESA-FPAR)>390 And (PESA-FPAR)<=420
							TG(14)=TG(14)+1
							AW(14)=AW(14)+PRM
							AV(14)=AW(14)/TG(14)				

					Case (PESA-FPAR)>420 
							TG(15)=TG(15)+1
							AW(15)=AW(15)+PRM
							AV(15)=AW(15)/TG(15)				

				EndCase

		Case NP=2
				Do Case
					Case (PESA-FPAR)>0 And (PESA-FPAR)<=30
							TG(16)=TG(16)+1
							AW(16)=AW(16)+PRM
							AV(16)=AW(16)/TG(16)				
								
					Case (PESA-FPAR)>30 And (PESA-FPAR)<=60
							TG(17)=TG(17)+1
							AW(17)=AW(17)+PRM
							AV(17)=AW(17)/TG(17)				

					Case (PESA-FPAR)>60 And (PESA-FPAR)<=90
							TG(18)=TG(18)+1
							AW(18)=AW(18)+PRM
							AV(18)=AW(18)/TG(18)				

					Case (PESA-FPAR)>90 And (PESA-FPAR)<=120
							TG(19)=TG(19)+1
							AW(19)=AW(19)+PRM
							AV(19)=AW(19)/TG(19)				

					Case (PESA-FPAR)>120 And (PESA-FPAR)<=150
							TG(20)=TG(20)+1
							AW(20)=AW(20)+PRM
							AV(20)=AW(20)/TG(20)				

					Case (PESA-FPAR)>150 And (PESA-FPAR)<=180
							TG(21)=TG(21)+1
							AW(21)=AW(21)+PRM
							AV(21)=AW(21)/TG(21)				

					Case (PESA-FPAR)>180 And (PESA-FPAR)<=210
							TG(22)=TG(22)+1
							AW(22)=AW(22)+PRM
							AV(22)=AW(22)/TG(22)				

					Case (PESA-FPAR)>210 And (PESA-FPAR)<=240
							TG(23)=TG(23)+1
							AW(23)=AW(23)+PRM
							AV(23)=AW(23)/TG(23)				

					Case (PESA-FPAR)>240 And (PESA-FPAR)<=270
							TG(24)=TG(24)+1
							AW(24)=AW(24)+PRM
							AV(24)=AW(24)/TG(24)				

					Case (PESA-FPAR)>270 And (PESA-FPAR)<=300
							TG(25)=TG(25)+1
							AW(25)=AW(25)+PRM
							AV(25)=AW(25)/TG(25)				

					Case (PESA-FPAR)>300 And (PESA-FPAR)<=330
							TG(26)=TG(26)+1
							AW(26)=AW(26)+PRM
							AV(26)=AW(26)/TG(26)				

					Case (PESA-FPAR)>330 And (PESA-FPAR)<=360
							TG(27)=TG(27)+1
							AW(27)=AW(27)+PRM
							AV(27)=AW(27)/TG(27)				

					Case (PESA-FPAR)>360 And (PESA-FPAR)<=390
							TG(28)=TG(28)+1
							AW(28)=AW(28)+PRM
							AV(28)=AW(28)/TG(28)				

					Case (PESA-FPAR)>390 And (PESA-FPAR)<=420
							TG(29)=TG(29)+1
							AW(29)=AW(29)+PRM
							AV(29)=AW(29)/TG(29)				

					Case (PESA-FPAR)>420 
							TG(30)=TG(30)+1
							AW(30)=AW(30)+PRM
							AV(30)=AW(30)/TG(30)				
				EndCase

		Case NP>=3
				Do Case
					Case (PESA-FPAR)>0 And (PESA-FPAR)<=30
							TG(31)=TG(31)+1
							AW(31)=AW(31)+PRM
							AV(31)=AW(31)/TG(31)				
								
					Case (PESA-FPAR)>30 And (PESA-FPAR)<=60
							TG(32)=TG(32)+1
							AW(32)=AW(32)+PRM
							AV(32)=AW(32)/TG(32)				

					Case (PESA-FPAR)>60 And (PESA-FPAR)<=90
							TG(33)=TG(33)+1
							AW(33)=AW(33)+PRM
							AV(33)=AW(33)/TG(33)				

					Case (PESA-FPAR)>90 And (PESA-FPAR)<=120
							TG(34)=TG(34)+1
							AW(34)=AW(34)+PRM
							AV(34)=AW(34)/TG(34)				

					Case (PESA-FPAR)>120 And (PESA-FPAR)<=150
							TG(35)=TG(35)+1
							AW(35)=AW(35)+PRM
							AV(35)=AW(35)/TG(35)				

					Case (PESA-FPAR)>150 And (PESA-FPAR)<=180
							TG(36)=TG(36)+1
							AW(36)=AW(36)+PRM
							AV(36)=AW(36)/TG(36)				

					Case (PESA-FPAR)>180 And (PESA-FPAR)<=210
							TG(37)=TG(37)+1
							AW(37)=AW(37)+PRM
							AV(37)=AW(37)/TG(37)				

					Case (PESA-FPAR)>210 And (PESA-FPAR)<=240
							TG(38)=TG(38)+1
							AW(38)=AW(38)+PRM
							AV(38)=AW(38)/TG(38)				

					Case (PESA-FPAR)>240 And (PESA-FPAR)<=270
							TG(39)=TG(39)+1
							AW(39)=AW(39)+PRM
							AV(39)=AW(39)/TG(39)				

					Case (PESA-FPAR)>270 And (PESA-FPAR)<=300
							TG(40)=TG(40)+1
							AW(40)=AW(40)+PRM
							AV(40)=AW(40)/TG(40)				

					Case (PESA-FPAR)>300 And (PESA-FPAR)<=330
							TG(41)=TG(41)+1
							AW(41)=AW(41)+PRM
							AV(41)=AW(41)/TG(41)				

					Case (PESA-FPAR)>330 And (PESA-FPAR)<=360
							TG(42)=TG(42)+1
							AW(42)=AW(42)+PRM
							AV(42)=AW(42)/TG(42)				

					Case (PESA-FPAR)>360 And (PESA-FPAR)<=390
							TG(43)=TG(43)+1
							AW(43)=AW(43)+PRM
							AV(43)=AW(43)/TG(43)				

					Case (PESA-FPAR)>390 And (PESA-FPAR)<=420
							TG(44)=TG(44)+1
							AW(44)=AW(44)+PRM
							AV(44)=AW(44)/TG(44)				

					Case (PESA-FPAR)>420 
							TG(45)=TG(45)+1
							AW(45)=AW(45)+PRM
							AV(45)=AW(45)/TG(45)				
				EndCase
	EndCase		

		If NP>0
				Do Case
					Case (PESA-FPAR)>0 And (PESA-FPAR)<=30
							TG(46)=TG(46)+1
							AW(46)=AW(46)+PRM
							AV(46)=AW(46)/TG(46)				
								
					Case (PESA-FPAR)>30 And (PESA-FPAR)<=60
							TG(47)=TG(47)+1
							AW(47)=AW(47)+PRM
							AV(47)=AW(47)/TG(47)				

					Case (PESA-FPAR)>60 And (PESA-FPAR)<=90
							TG(48)=TG(48)+1
							AW(48)=AW(48)+PRM
							AV(48)=AW(48)/TG(48)				

					Case (PESA-FPAR)>90 And (PESA-FPAR)<=120
							TG(49)=TG(49)+1
							AW(49)=AW(49)+PRM
							AV(49)=AW(49)/TG(49)				

					Case (PESA-FPAR)>120 And (PESA-FPAR)<=150
							TG(50)=TG(50)+1
							AW(50)=AW(50)+PRM
							AV(50)=AW(50)/TG(50)				

					Case (PESA-FPAR)>150 And (PESA-FPAR)<=180
							TG(51)=TG(51)+1
							AW(51)=AW(51)+PRM
							AV(51)=AW(51)/TG(51)				

					Case (PESA-FPAR)>180 And (PESA-FPAR)<=210
							TG(52)=TG(52)+1
							AW(52)=AW(52)+PRM
							AV(52)=AW(52)/TG(52)				

					Case (PESA-FPAR)>210 And (PESA-FPAR)<=240
							TG(53)=TG(53)+1
							AW(53)=AW(53)+PRM
							AV(53)=AW(53)/TG(53)				

					Case (PESA-FPAR)>240 And (PESA-FPAR)<=270
							TG(54)=TG(54)+1
							AW(54)=AW(54)+PRM
							AV(54)=AW(54)/TG(54)				

					Case (PESA-FPAR)>270 And (PESA-FPAR)<=300
							TG(55)=TG(55)+1
							AW(55)=AW(55)+PRM
							AV(55)=AW(55)/TG(55)				

					Case (PESA-FPAR)>300 And (PESA-FPAR)<=330
							TG(56)=TG(56)+1
							AW(56)=AW(56)+PRM
							AV(56)=AW(56)/TG(56)				

					Case (PESA-FPAR)>330 And (PESA-FPAR)<=360
							TG(57)=TG(57)+1
							AW(57)=AW(57)+PRM
							AV(57)=AW(57)/TG(57)				

					Case (PESA-FPAR)>360 And (PESA-FPAR)<=390
							TG(58)=TG(58)+1
							AW(58)=AW(58)+PRM
							AV(58)=AW(58)/TG(58)				

					Case (PESA-FPAR)>390 And (PESA-FPAR)<=420
							TG(59)=TG(59)+1
							AW(59)=AW(59)+PRM
							AV(59)=AW(59)/TG(59)				

					Case (PESA-FPAR)>420 
							TG(60)=TG(60)+1
							AW(60)=AW(60)+PRM
							AV(60)=AW(60)/TG(60)				
				EndCase
		EndIF
EndScan

Create Cursor REPORTE (Concepto c(20),COL1 c(5),COL2 c(5),COL3 c(5),COL4 c(5))
Append Blank
Replace CONCEPTO With 'Dias en Leche'
Append Blank
Replace CONCEPTO With '  0 -  30', COL1 With Str(AV(1),5,1),COL2 With Str(AV(16),5,1),COL3 With Str(AV(31),5,1),COL4 With Str(AV(46),5,1)
Append Blank
Replace CONCEPTO With ' 30 -  60',COL1 With Str(AV(2),5,1),COL2 With Str(AV(17),5,1),COL3 With Str(AV(32),5,1),COL4 With Str(AV(47),5,1)
Append Blank
Replace CONCEPTO With ' 60 -  90',COL1 With Str(AV(3),5,1),COL2 With Str(AV(18),5,1),COL3 With Str(AV(33),5,1),COL4 With Str(AV(48),5,1)
Append Blank
Replace CONCEPTO With ' 90 - 120',COL1 With Str(AV(4),5,1),COL2 With Str(AV(19),5,1),COL3 With Str(AV(34),5,1),COL4 With Str(AV(49),5,1)
Append Blank
Replace CONCEPTO With '120 - 150',COL1 With Str(AV(5),5,1),COL2 With Str(AV(20),5,1),COL3 With Str(AV(35),5,1),COL4 With Str(AV(50),5,1)
Append Blank
Replace CONCEPTO With '150 - 180',COL1 With Str(AV(6),5,1),COL2 With Str(AV(21),5,1),COL3 With Str(AV(36),5,1),COL4 With Str(AV(51),5,1)
Append Blank
Replace CONCEPTO With '180 - 210',COL1 With Str(AV(7),5,1),COL2 With Str(AV(22),5,1),COL3 With Str(AV(37),5,1),COL4 With Str(AV(52),5,1)
Append Blank
Replace CONCEPTO With '210 - 240',COL1 With Str(AV(8),5,1),COL2 With Str(AV(23),5,1),COL3 With Str(AV(38),5,1),COL4 With Str(AV(53),5,1)
Append Blank
Replace CONCEPTO With '240 - 270',COL1 With Str(AV(9),5,1),COL2 With Str(AV(24),5,1),COL3 With Str(AV(39),5,1),COL4 With Str(AV(54),5,1)
Append Blank
Replace CONCEPTO With '270 - 300',COL1 With Str(AV(10),5,1),COL2 With Str(AV(25),5,1),COL3 With Str(AV(40),5,1),COL4 With Str(AV(55),5,1)
Append Blank
Replace CONCEPTO With '300 - 330',COL1 With Str(AV(11),5,1),COL2 With Str(AV(26),5,1),COL3 With Str(AV(41),5,1),COL4 With Str(AV(56),5,1)
Append Blank
Replace CONCEPTO With '330 - 360',COL1 With Str(AV(12),5,1),COL2 With Str(AV(27),5,1),COL3 With Str(AV(42),5,1),COL4 With Str(AV(57),5,1)
Append Blank
Replace CONCEPTO With '360 - 390',COL1 With Str(AV(13),5,1),COL2 With Str(AV(28),5,1),COL3 With Str(AV(43),5,1),COL4 With Str(AV(58),5,1)
Append Blank
Replace CONCEPTO With '390 - 420',COL1 With Str(AV(14),5,1),COL2 With Str(AV(29),5,1),COL3 With Str(AV(44),5,1),COL4 With Str(AV(59),5,1)
Append Blank
Replace CONCEPTO With ''

GO TOP
RETURN


* ANALISIS DE PRODUCCION DEL DIA DE PESADA
* ----------------------------------------
PROC RV10166
Select REG
Set Order To 2
Calculate MAX(pesa) To mpesa
Set Filter To NP>0 And PRM>0 And PESA-FPAR>7 And PESA>=mPESA-7 And m305>0 

DIMENSION aw(300)
aw=0
dat=0

SCAN
xx=(pesa-fpar)-(pesa-fsec)

** Calculo de Dias Abiertos
** ------------------------
	do case
		case stat=[CARGA] or stat=[INSEM]
			xdab=(ucal-fpar)
	
		case stat!=[CARGA] or stat!=[INSEM]
			xdab=pesa-fpar
	endcase						
** ------------------------

dat=dat+1
**-------------------------------------*
do case
	** Lactancia 1
	** -----------
	case np=1
		aw(1)=aw(1)+1
		aw(2)=aw(2)+xx
		aw(3)=aw(3)+prm			
		aw(4)=aw(4)+fcm
		aw(5)=aw(5)+ecm
		aw(6)=aw(6)+pgra
		aw(7)=aw(7)+ppro
		aw(8)=aw(8)+lsc
		aw(9)=aw(9)+xdab			
		aw(10)=aw(10)+pdsc
		aw(11)=aw(11)+m305
				
		do case
			case xx<45
				aw(12)=aw(12)+1
				aw(13)=aw(13)+xx			
				aw(14)=aw(14)+prm
				aw(15)=aw(15)+fcm
				aw(16)=aw(16)+ecm		
				aw(17)=aw(17)+pgra
				aw(18)=aw(18)+ppro
				aw(19)=aw(19)+lsc
				aw(20)=aw(20)
				aw(21)=aw(21)+pdsc
				aw(22)=aw(22)+m305

			case xx>=45 and xx<=100
				aw(23)=aw(23)+1
				aw(24)=aw(24)+xx			
				aw(25)=aw(25)+prm
				aw(26)=aw(26)+fcm
				aw(27)=aw(27)+ecm		
				aw(28)=aw(28)+pgra
				aw(29)=aw(29)+ppro
				aw(30)=aw(30)+lsc
				aw(31)=aw(31)+xdab
				aw(32)=aw(32)+pdsc
				aw(33)=aw(33)+m305

			case xx>100 and xx<=200
				aw(34)=aw(34)+1
				aw(35)=aw(35)+xx			
				aw(36)=aw(36)+prm
				aw(37)=aw(37)+fcm
				aw(38)=aw(38)+ecm
				aw(39)=aw(39)+pgra
				aw(40)=aw(40)+ppro
				aw(41)=aw(41)+lsc			
				aw(42)=aw(42)+xdab
				aw(43)=aw(43)+pdsc
				aw(44)=aw(44)+m305

			case xx>200 and xx<=300
				aw(45)=aw(45)+1
				aw(46)=aw(46)+xx			
				aw(47)=aw(47)+prm
				aw(48)=aw(48)+fcm
				aw(49)=aw(49)+ecm
				aw(50)=aw(50)+pgra
				aw(51)=aw(51)+ppro
				aw(52)=aw(52)+lsc			
				aw(53)=aw(53)+xdab
				aw(54)=aw(54)+pdsc
				aw(55)=aw(55)+m305

			case xx>300
				aw(56)=aw(56)+1
				aw(57)=aw(57)+xx			
				aw(58)=aw(58)+prm
				aw(59)=aw(59)+fcm
				aw(60)=aw(60)+ecm
				aw(61)=aw(61)+pgra
				aw(62)=aw(62)+ppro
				aw(63)=aw(63)+lsc			
				aw(64)=aw(64)+xdab
				aw(65)=aw(65)+pdsc
				aw(66)=aw(66)+m305
		endcase		

	case np=2
	** Lactancia 2
	** -----------
		aw(71)=aw(71)+1
		aw(72)=aw(72)+xx
		aw(73)=aw(73)+prm			
		aw(74)=aw(74)+fcm
		aw(75)=aw(75)+ecm
		aw(76)=aw(76)+pgra
		aw(77)=aw(77)+ppro
		aw(78)=aw(78)+lsc
		aw(79)=aw(79)+xdab			
		aw(80)=aw(80)+pdsc
		aw(81)=aw(81)+m305
				
		do case
			case xx<45
				aw(82)=aw(82)+1
				aw(83)=aw(83)+xx			
				aw(84)=aw(84)+prm
				aw(85)=aw(85)+fcm
				aw(86)=aw(86)+ecm		
				aw(87)=aw(87)+pgra
				aw(88)=aw(88)+ppro
				aw(89)=aw(89)+lsc
				aw(90)=aw(90)
				aw(91)=aw(91)+pdsc
				aw(92)=aw(92)+m305

			case xx>=45 and xx<=100
				aw(93)=aw(93)+1
				aw(94)=aw(94)+xx			
				aw(95)=aw(95)+prm
				aw(96)=aw(96)+fcm
				aw(97)=aw(97)+ecm		
				aw(98)=aw(98)+pgra
				aw(99)=aw(99)+ppro
				aw(100)=aw(100)+lsc
				aw(101)=aw(101)+xdab
				aw(102)=aw(102)+pdsc
				aw(103)=aw(103)+m305

			case xx>100 and xx<=200
				aw(104)=aw(104)+1
				aw(105)=aw(105)+xx			
				aw(106)=aw(106)+prm
				aw(107)=aw(107)+fcm
				aw(108)=aw(108)+ecm
				aw(109)=aw(109)+pgra
				aw(110)=aw(110)+ppro
				aw(111)=aw(111)+lsc			
				aw(112)=aw(112)+xdab
				aw(113)=aw(113)+pdsc
				aw(114)=aw(114)+m305

			case xx>200 and xx<=300
				aw(115)=aw(115)+1
				aw(116)=aw(116)+xx			
				aw(117)=aw(117)+prm
				aw(118)=aw(118)+fcm
				aw(119)=aw(119)+ecm
				aw(120)=aw(120)+pgra
				aw(121)=aw(121)+ppro
				aw(122)=aw(122)+lsc			
				aw(123)=aw(123)+xdab
				aw(124)=aw(124)+pdsc
				aw(125)=aw(125)+m305

			case xx>300
				aw(126)=aw(126)+1
				aw(127)=aw(127)+xx			
				aw(128)=aw(128)+prm
				aw(129)=aw(129)+fcm
				aw(130)=aw(130)+ecm
				aw(131)=aw(131)+pgra
				aw(132)=aw(132)+ppro
				aw(133)=aw(133)+lsc			
				aw(134)=aw(134)+xdab
				aw(135)=aw(135)+pdsc
				aw(136)=aw(136)+m305
		endcase		

	case np>=3
	** Lactancia 3+
	** ------------
		aw(141)=aw(141)+1
		aw(142)=aw(142)+xx
		aw(143)=aw(143)+prm			
		aw(144)=aw(144)+fcm
		aw(145)=aw(145)+ecm
		aw(146)=aw(146)+pgra
		aw(147)=aw(147)+ppro
		aw(148)=aw(148)+lsc
		aw(149)=aw(149)+xdab			
		aw(150)=aw(150)+pdsc
		aw(151)=aw(151)+m305
				
		do case
			case xx<45
				aw(152)=aw(152)+1
				aw(153)=aw(153)+xx			
				aw(154)=aw(154)+prm
				aw(155)=aw(155)+fcm
				aw(156)=aw(156)+ecm		
				aw(157)=aw(157)+pgra
				aw(158)=aw(158)+ppro
				aw(159)=aw(159)+lsc
				aw(160)=aw(160)
				aw(161)=aw(161)+pdsc
				aw(162)=aw(162)+m305

			case xx>=45 and xx<=100
				aw(163)=aw(163)+1
				aw(164)=aw(164)+xx			
				aw(165)=aw(165)+prm
				aw(166)=aw(166)+fcm
				aw(167)=aw(167)+ecm		
				aw(168)=aw(168)+pgra
				aw(169)=aw(169)+ppro
				aw(170)=aw(170)+lsc
				aw(171)=aw(171)+xdab
				aw(172)=aw(172)+pdsc
				aw(173)=aw(173)+m305

			case xx>100 and xx<=200
				aw(174)=aw(174)+1
				aw(175)=aw(175)+xx			
				aw(176)=aw(176)+prm
				aw(177)=aw(177)+fcm
				aw(178)=aw(178)+ecm
				aw(179)=aw(179)+pgra
				aw(180)=aw(180)+ppro
				aw(181)=aw(181)+lsc			
				aw(182)=aw(182)+xdab
				aw(183)=aw(183)+pdsc
				aw(184)=aw(184)+m305

			case xx>200 and xx<=300
				aw(185)=aw(185)+1
				aw(186)=aw(186)+xx			
				aw(187)=aw(187)+prm
				aw(188)=aw(188)+fcm
				aw(189)=aw(189)+ecm
				aw(190)=aw(190)+pgra
				aw(191)=aw(191)+ppro
				aw(192)=aw(192)+lsc			
				aw(193)=aw(193)+xdab
				aw(194)=aw(194)+pdsc
				aw(195)=aw(195)+m305

			case xx>300
				aw(196)=aw(196)+1
				aw(197)=aw(197)+xx			
				aw(198)=aw(198)+prm
				aw(199)=aw(199)+fcm
				aw(200)=aw(200)+ecm
				aw(201)=aw(201)+pgra
				aw(202)=aw(202)+ppro
				aw(203)=aw(203)+lsc			
				aw(204)=aw(204)+xdab
				aw(205)=aw(205)+pdsc
				aw(206)=aw(206)+m305
		endcase		
endcase	
**-------------------------------------*

	** Todas las Lactancias
	** --------------------
if np>0
		aw(211)=aw(211)+1
		aw(212)=aw(212)+xx
		aw(213)=aw(213)+prm			
		aw(214)=aw(214)+fcm
		aw(215)=aw(215)+ecm
		aw(216)=aw(216)+pgra
		aw(217)=aw(217)+ppro
		aw(218)=aw(218)+lsc
		aw(219)=aw(219)+xdab			
		aw(220)=aw(220)+pdsc
		aw(221)=aw(221)+m305
				
		do case
			case xx<45
				aw(222)=aw(222)+1
				aw(223)=aw(223)+xx			
				aw(224)=aw(224)+prm
				aw(225)=aw(225)+fcm
				aw(226)=aw(226)+ecm		
				aw(227)=aw(227)+pgra
				aw(228)=aw(228)+ppro
				aw(229)=aw(229)+lsc
				aw(230)=aw(230)
				aw(231)=aw(231)+pdsc
				aw(232)=aw(232)+m305

			case xx>=45 and xx<=100
				aw(233)=aw(233)+1
				aw(234)=aw(234)+xx			
				aw(235)=aw(235)+prm
				aw(236)=aw(236)+fcm
				aw(237)=aw(237)+ecm		
				aw(238)=aw(238)+pgra
				aw(239)=aw(239)+ppro
				aw(240)=aw(240)+lsc
				aw(241)=aw(241)+xdab
				aw(242)=aw(242)+pdsc
				aw(243)=aw(243)+m305

			case xx>100 and xx<=200
				aw(244)=aw(244)+1
				aw(245)=aw(245)+xx			
				aw(246)=aw(246)+prm
				aw(247)=aw(247)+fcm
				aw(248)=aw(248)+ecm
				aw(249)=aw(249)+pgra
				aw(250)=aw(250)+ppro
				aw(251)=aw(251)+lsc			
				aw(252)=aw(252)+xdab
				aw(253)=aw(253)+pdsc
				aw(254)=aw(254)+m305

			case xx>200 and xx<=300
				aw(255)=aw(255)+1
				aw(256)=aw(256)+xx			
				aw(257)=aw(257)+prm
				aw(258)=aw(258)+fcm
				aw(259)=aw(259)+ecm
				aw(260)=aw(260)+pgra
				aw(261)=aw(261)+ppro
				aw(262)=aw(262)+lsc			
				aw(263)=aw(263)+xdab
				aw(264)=aw(264)+pdsc
				aw(265)=aw(265)+m305

			case xx>300
				aw(266)=aw(266)+1
				aw(267)=aw(267)+xx			
				aw(268)=aw(268)+prm
				aw(269)=aw(269)+fcm
				aw(270)=aw(270)+ecm
				aw(271)=aw(271)+pgra
				aw(272)=aw(272)+ppro
				aw(273)=aw(273)+lsc			
				aw(274)=aw(274)+xdab
				aw(275)=aw(275)+pdsc
				aw(276)=aw(276)+m305
		endcase		
endi
*-------------------------------------*
ends

** Revalora las variables
** ----------------------
** Lactancia 1
** -----------
if aw(1)>0
	aw(2)=aw(2)/aw(1)
	aw(3)=aw(3)/aw(1)
	aw(4)=aw(4)/aw(1)
	aw(5)=aw(5)/aw(1)
	aw(6)=aw(6)/aw(1)
	aw(7)=aw(7)/aw(1)
	aw(8)=aw(8)/aw(1)
	aw(9)=aw(9)/(aw(1)-aw(12))
	aw(10)=aw(10)/aw(1)
	aw(11)=aw(11)/aw(1)
endi

if aw(12)>0
	aw(13)=aw(13)/aw(12)
	aw(14)=aw(14)/aw(12)
	aw(15)=aw(15)/aw(12)
	aw(16)=aw(16)/aw(12)
	aw(17)=aw(17)/aw(12)
	aw(18)=aw(18)/aw(12)
	aw(19)=aw(19)/aw(12)
	aw(20)=aw(20)/aw(12)
	aw(21)=aw(21)/aw(12)
	aw(22)=aw(22)/aw(12)
endi

if aw(23)>0
	aw(24)=aw(24)/aw(23)
	aw(25)=aw(25)/aw(23)
	aw(26)=aw(26)/aw(23)
	aw(27)=aw(27)/aw(23)
	aw(28)=aw(28)/aw(23)
	aw(29)=aw(29)/aw(23)
	aw(30)=aw(30)/aw(23)
	aw(31)=aw(31)/aw(23)
	aw(32)=aw(32)/aw(23)
	aw(33)=aw(33)/aw(23)
endi

if aw(34)>0
	aw(35)=aw(35)/aw(34)
	aw(36)=aw(36)/aw(34)
	aw(37)=aw(37)/aw(34)
	aw(38)=aw(38)/aw(34)
	aw(39)=aw(39)/aw(34)
	aw(40)=aw(40)/aw(34)
	aw(41)=aw(41)/aw(34)
	aw(42)=aw(42)/aw(34)
	aw(43)=aw(43)/aw(34)
	aw(44)=aw(44)/aw(34)
endi
if aw(45)>0
	aw(46)=aw(46)/aw(45)
	aw(47)=aw(47)/aw(45)
	aw(48)=aw(48)/aw(45)
	aw(49)=aw(49)/aw(45)
	aw(50)=aw(50)/aw(45)
	aw(51)=aw(51)/aw(45)
	aw(52)=aw(52)/aw(45)
	aw(53)=aw(53)/aw(45)
	aw(54)=aw(54)/aw(45)
	aw(55)=aw(55)/aw(45)
endi
if aw(56)>0
	aw(57)=aw(57)/aw(56)
	aw(58)=aw(58)/aw(56)
	aw(59)=aw(59)/aw(56)
	aw(60)=aw(60)/aw(56)
	aw(61)=aw(61)/aw(56)
	aw(62)=aw(62)/aw(56)
	aw(63)=aw(63)/aw(56)
	aw(64)=aw(64)/aw(56)
	aw(65)=aw(65)/aw(56)
	aw(66)=aw(66)/aw(56)
endi

** Lactancia 2
** -----------
if aw(71)>0
	aw(72)=aw(72)/aw(71)
	aw(73)=aw(73)/aw(71)
	aw(74)=aw(74)/aw(71)
	aw(75)=aw(75)/aw(71)
	aw(76)=aw(76)/aw(71)
	aw(77)=aw(77)/aw(71)
	aw(78)=aw(78)/aw(71)
	aw(79)=aw(79)/(aw(71)-aw(82))
	aw(80)=aw(80)/aw(71)
	aw(81)=aw(81)/aw(71)
endi

if aw(82)>0
	aw(83)=aw(83)/aw(82)
	aw(84)=aw(84)/aw(82)
	aw(85)=aw(85)/aw(82)
	aw(86)=aw(86)/aw(82)
	aw(87)=aw(87)/aw(82)
	aw(88)=aw(88)/aw(82)
	aw(89)=aw(89)/aw(82)
	aw(90)=aw(90)/aw(82)
	aw(91)=aw(91)/aw(82)
	aw(92)=aw(92)/aw(82)
endi

if aw(93)>0
	aw(94)=aw(94)/aw(93)
	aw(95)=aw(95)/aw(93)
	aw(96)=aw(96)/aw(93)
	aw(97)=aw(97)/aw(93)
	aw(98)=aw(98)/aw(93)
	aw(99)=aw(99)/aw(93)
	aw(100)=aw(100)/aw(93)
	aw(101)=aw(101)/aw(93)
	aw(102)=aw(102)/aw(93)
	aw(103)=aw(103)/aw(93)
endi

if aw(104)>0
	aw(105)=aw(105)/aw(104)
	aw(106)=aw(106)/aw(104)
	aw(107)=aw(107)/aw(104)
	aw(108)=aw(108)/aw(104)
	aw(109)=aw(109)/aw(104)
	aw(110)=aw(110)/aw(104)
	aw(111)=aw(111)/aw(104)
	aw(112)=aw(112)/aw(104)
	aw(113)=aw(113)/aw(104)
	aw(114)=aw(114)/aw(104)
endi
if aw(115)>0
	aw(116)=aw(116)/aw(115)
	aw(117)=aw(117)/aw(115)
	aw(118)=aw(118)/aw(115)
	aw(119)=aw(119)/aw(115)
	aw(120)=aw(120)/aw(115)
	aw(121)=aw(121)/aw(115)
	aw(122)=aw(122)/aw(115)
	aw(123)=aw(123)/aw(115)
	aw(124)=aw(124)/aw(115)
	aw(125)=aw(125)/aw(115)
endi
if aw(126)>0
	aw(127)=aw(127)/aw(126)
	aw(128)=aw(128)/aw(126)
	aw(129)=aw(129)/aw(126)
	aw(130)=aw(130)/aw(126)
	aw(131)=aw(131)/aw(126)
	aw(132)=aw(132)/aw(126)
	aw(133)=aw(133)/aw(126)
	aw(134)=aw(134)/aw(126)
	aw(135)=aw(135)/aw(126)
	aw(136)=aw(136)/aw(126)
endi

** Lactancia 3+
** ------------
if aw(141)>0
	aw(142)=aw(142)/aw(141)
	aw(143)=aw(143)/aw(141)
	aw(144)=aw(144)/aw(141)
	aw(145)=aw(145)/aw(141)
	aw(146)=aw(146)/aw(141)
	aw(147)=aw(147)/aw(141)
	aw(148)=aw(148)/aw(141)
	aw(149)=aw(149)/(aw(141)-aw(152))
	aw(150)=aw(150)/aw(141)
	aw(151)=aw(151)/aw(141)
endi

if aw(152)>0
	aw(153)=aw(153)/aw(152)
	aw(154)=aw(154)/aw(152)
	aw(155)=aw(155)/aw(152)
	aw(156)=aw(156)/aw(152)
	aw(157)=aw(157)/aw(152)
	aw(158)=aw(158)/aw(152)
	aw(159)=aw(159)/aw(152)
	aw(160)=aw(160)/aw(152)
	aw(161)=aw(161)/aw(152)
	aw(162)=aw(162)/aw(152)
endi

if aw(163)>0
	aw(164)=aw(164)/aw(163)
	aw(165)=aw(165)/aw(163)
	aw(166)=aw(166)/aw(163)
	aw(167)=aw(167)/aw(163)
	aw(168)=aw(168)/aw(163)
	aw(169)=aw(169)/aw(163)
	aw(170)=aw(170)/aw(163)
	aw(171)=aw(171)/aw(163)
	aw(172)=aw(172)/aw(163)
	aw(173)=aw(173)/aw(163)
endi

if aw(174)>0
	aw(175)=aw(175)/aw(174)
	aw(176)=aw(176)/aw(174)
	aw(177)=aw(177)/aw(174)
	aw(178)=aw(178)/aw(174)
	aw(179)=aw(179)/aw(174)
	aw(180)=aw(180)/aw(174)
	aw(181)=aw(181)/aw(174)
	aw(182)=aw(182)/aw(174)
	aw(183)=aw(183)/aw(174)
	aw(184)=aw(184)/aw(174)
endi
if aw(185)>0
	aw(186)=aw(186)/aw(185)
	aw(187)=aw(187)/aw(185)
	aw(188)=aw(188)/aw(185)
	aw(189)=aw(189)/aw(185)
	aw(190)=aw(190)/aw(185)
	aw(191)=aw(191)/aw(185)
	aw(192)=aw(192)/aw(185)
	aw(193)=aw(193)/aw(185)
	aw(194)=aw(194)/aw(185)
	aw(195)=aw(195)/aw(185)
endi
if aw(196)>0
	aw(197)=aw(197)/aw(196)
	aw(198)=aw(198)/aw(196)
	aw(199)=aw(199)/aw(196)
	aw(200)=aw(200)/aw(196)
	aw(201)=aw(201)/aw(196)
	aw(202)=aw(202)/aw(196)
	aw(203)=aw(203)/aw(196)
	aw(204)=aw(204)/aw(196)
	aw(205)=aw(205)/aw(196)
	aw(206)=aw(206)/aw(196)
endi

** Todas
** -----------
if aw(211)>0
	aw(212)=aw(212)/aw(211)
	aw(213)=aw(213)/aw(211)
	aw(214)=aw(214)/aw(211)
	aw(215)=aw(215)/aw(211)
	aw(216)=aw(216)/aw(211)
	aw(217)=aw(217)/aw(211)
	aw(218)=aw(218)/aw(211)
	aw(219)=aw(219)/(aw(211)-aw(222))
	aw(220)=aw(220)/(aw(211)-aw(1))
	aw(221)=aw(221)/aw(211)
endi

if aw(222)>0
	aw(223)=aw(223)/aw(222)
	aw(224)=aw(224)/aw(222)
	aw(225)=aw(225)/aw(222)
	aw(226)=aw(226)/aw(222)
	aw(227)=aw(227)/aw(222)
	aw(228)=aw(228)/aw(222)
	aw(229)=aw(229)/aw(222)
	aw(230)=aw(230)/aw(222)
	aw(231)=aw(231)/(aw(222)-aw(12))
	aw(232)=aw(232)/aw(222)
endi

if aw(233)>0
	aw(234)=aw(234)/aw(233)
	aw(235)=aw(235)/aw(233)
	aw(236)=aw(236)/aw(233)
	aw(237)=aw(237)/aw(233)
	aw(238)=aw(238)/aw(233)
	aw(239)=aw(239)/aw(233)
	aw(240)=aw(240)/aw(233)
	aw(241)=aw(241)/aw(233)
	aw(242)=aw(242)/(aw(233)-aw(23))
	aw(243)=aw(243)/aw(233)
endi

if aw(244)>0
	aw(245)=aw(245)/aw(244)
	aw(246)=aw(246)/aw(244)
	aw(247)=aw(247)/aw(244)
	aw(248)=aw(248)/aw(244)
	aw(249)=aw(249)/aw(244)
	aw(250)=aw(250)/aw(244)
	aw(251)=aw(251)/aw(244)
	aw(252)=aw(252)/aw(244)
	aw(253)=aw(253)/(aw(244)-aw(34))
	aw(254)=aw(254)/aw(244)
endi
if aw(255)>0
	aw(256)=aw(256)/aw(255)
	aw(257)=aw(257)/aw(255)
	aw(258)=aw(258)/aw(255)
	aw(259)=aw(259)/aw(255)
	aw(260)=aw(260)/aw(255)
	aw(261)=aw(261)/aw(255)
	aw(262)=aw(262)/aw(255)
	aw(263)=aw(263)/aw(255)
	aw(264)=aw(264)/(aw(255)-aw(45))
	aw(265)=aw(265)/aw(255)
endi
if aw(266)>0
	aw(267)=aw(267)/aw(266)
	aw(268)=aw(268)/aw(266)
	aw(269)=aw(269)/aw(266)
	aw(270)=aw(270)/aw(266)
	aw(271)=aw(271)/aw(266)
	aw(272)=aw(272)/aw(266)
	aw(273)=aw(273)/aw(266)
	aw(274)=aw(274)/aw(266)
	aw(275)=aw(275)/(aw(266)-aw(56))
	aw(276)=aw(276)/aw(266)
endi

Select REG
Set Filter To

Create Cursor REPORTE (Concepto c(20),COL1 c(5),COL2 c(5),COL3 c(5),COL4 c(5),COL5 c(5),COL6 c(5),COL7 c(5))
Append Blank
Replace CONCEPTO With 'Lactancia 1'
Append Blank
Replace CONCEPTO With 'Numero de Vientres'   ,COL1 With Str(aw(12),5),COL2 With Str(aw(23),5),COL3 With Str(aw(34),5),COL4 With Str(aw(45),5),COL5 With Str(aw(56),5),COL6 With Str(aw(1),5)
Append Blank
Replace CONCEPTO With 'Promedio DEL'         ,COL1 With Str(aw(13),5),COL2 With Str(aw(24),5),COL3 With Str(aw(35),5),COL4 With Str(aw(46),5),COL5 With Str(aw(57),5),COL6 With Str(aw(2),5)
Append Blank
Replace CONCEPTO With 'Promedio Leche (Kgs.)',COL1 With Str(aw(14),5,1),COL2 With Str(aw(25),5),COL3 With Str(aw(36),5),COL4 With Str(aw(47),5),COL5 With Str(aw(58),5),COL6 With Str(aw(3),5)
Append Blank
Replace CONCEPTO With 'Promedio LCG   (Kgs.)',COL1 With Str(aw(15),5,1),COL2 With Str(aw(26),5,1),COL3 With Str(aw(37),5,1),COL4 With Str(aw(48),5,1),COL5 With Str(aw(59),5,1),COL6 With Str(aw(4),5,1)
Append Blank
Replace CONCEPTO With 'Promedio LCE   (Kgs.)',COL1 With Str(aw(16),5,1),COL2 With Str(aw(27),5,1),COL3 With Str(aw(38),5,1),COL4 With Str(aw(49),5,1),COL5 With Str(aw(60),5,1),COL6 With Str(aw(5),5,1)
Append Blank
Replace CONCEPTO With 'Promedio Grasa(%)'    ,COL1 With Str(aw(17),5,1),COL2 With Str(aw(28),5,1),COL3 With Str(aw(39),5,1),COL4 With Str(aw(50),5,1),COL5 With Str(aw(61),5,1),COL6 With Str(aw(6),5,1)
Append Blank
Replace CONCEPTO With 'Promedio Proteina (%)',COL1 With Str(aw(18),5,1),COL2 With Str(aw(29),5,1),COL3 With Str(aw(40),5,1),COL4 With Str(aw(51),5,1),COL5 With Str(aw(62),5,1),COL6 With Str(aw(7),5,1)
Append Blank
Replace CONCEPTO With 'Promedio CCS (sl)'    ,COL1 With Str(aw(19),5,1),COL2 With Str(aw(30),5,1),COL3 With Str(aw(41),5,1),COL4 With Str(aw(52),5,1),COL5 With Str(aw(63),5,1),COL6 With Str(aw(8),5,1)
Append Blank
Replace CONCEPTO With 'Prom. Dias Abiertos'  ,COL1 With Str(aw(20),5),COL2 With Str(aw(31),5),COL3 With Str(aw(42),5),COL4 With Str(aw(53),5),COL5 With Str(aw(64),5),COL6 With Str(aw(9),5)
Append Blank
Replace CONCEPTO With 'Promedio Dias Seca'   ,COL1 With Str(aw(21),5),COL2 With Str(aw(32),5),COL3 With Str(aw(43),5),COL4 With Str(aw(54),5),COL5 With Str(aw(65),5),COL6 With Str(aw(10),5)
Append Blank
Replace CONCEPTO With 'Promedio 305d EM'     ,COL1 With Str(aw(22),5),COL2 With Str(aw(33),5),COL3 With Str(aw(44),5),COL4 With Str(aw(55),5),COL5 With Str(aw(66),5),COL6 With Str(aw(11),5)
Append Blank
Replace CONCEPTO With ''

Append Blank
Replace CONCEPTO With 'Lactancia 2'
Append Blank
Replace CONCEPTO With 'Numero de Vientres'   ,COL1 With Str(aw(82),5),COL2 With Str(aw(93),5),COL3 With Str(aw(104),5),COL4 With Str(aw(115),5),COL5 With Str(aw(126),5),COL6 With Str(aw(71),5)
Append Blank
Replace CONCEPTO With 'Promedio DEL'         ,COL1 With Str(aw(83),5),COL2 With Str(aw(94),5),COL3 With Str(aw(105),5),COL4 With Str(aw(116),5),COL5 With Str(aw(127),5),COL6 With Str(aw(72),5)
Append Blank
Replace CONCEPTO With 'Promedio Leche (Kgs.)',COL1 With Str(aw(84),5,1),COL2 With Str(aw(95),5,1),COL3 With Str(aw(106),5,1),COL4 With Str(aw(117),5,1),COL5 With Str(aw(128),5,1),COL6 With Str(aw(73),5,1)
Append Blank
Replace CONCEPTO With 'Promedio LCG   (Kgs.)',COL1 With Str(aw(85),5,1),COL2 With Str(aw(96),5,1),COL3 With Str(aw(107),5,1),COL4 With Str(aw(118),5,1),COL5 With Str(aw(129),5,1),COL6 With Str(aw(74),5,1)
Append Blank
Replace CONCEPTO With 'Promedio LCE   (Kgs.)',COL1 With Str(aw(86),5,1),COL2 With Str(aw(97),5,1),COL3 With Str(aw(108),5,1),COL4 With Str(aw(119),5,1),COL5 With Str(aw(130),5,1),COL6 With Str(aw(75),5,1)
Append Blank
Replace CONCEPTO With 'Promedio Grasa(%)'    ,COL1 With Str(aw(87),5,1),COL2 With Str(aw(98),5,1),COL3 With Str(aw(109),5,1),COL4 With Str(aw(120),5,1),COL5 With Str(aw(131),5,1),COL6 With Str(aw(76),5,1)
Append Blank
Replace CONCEPTO With 'Promedio Proteina (%)',COL1 With Str(aw(88),5,1),COL2 With Str(aw(99),5,1),COL3 With Str(aw(110),5,1),COL4 With Str(aw(121),5,1),COL5 With Str(aw(132),5,1),COL6 With Str(aw(77),5,1)
Append Blank
Replace CONCEPTO With 'Promedio CCS (sl)'    ,COL1 With Str(aw(89),5,1),COL2 With Str(aw(100),5,1),COL3 With Str(aw(111),5,1),COL4 With Str(aw(122),5,1),COL5 With Str(aw(133),5,1),COL6 With Str(aw(78),5,1)
Append Blank
Replace CONCEPTO With 'Prom. Dias Abiertos'  ,COL1 With Str(aw(90),5),COL2 With Str(aw(101),5),COL3 With Str(aw(112),5),COL4 With Str(aw(123),5),COL5 With Str(aw(134),5),COL6 With Str(aw(79),5)
Append Blank
Replace CONCEPTO With 'Promedio Dias Seca'   ,COL1 With Str(aw(91),5),COL2 With Str(aw(102),5),COL3 With Str(aw(113),5),COL4 With Str(aw(124),5),COL5 With Str(aw(135),5),COL6 With Str(aw(80),5)
Append Blank
Replace CONCEPTO With 'Promedio 305d EM'     ,COL1 With Str(aw(92),5),COL2 With Str(aw(103),5),COL3 With Str(aw(114),5),COL4 With Str(aw(125),5),COL5 With Str(aw(136),5),COL6 With Str(aw(81),5)
Append Blank
Replace CONCEPTO With ''

Append Blank
Replace CONCEPTO With 'Lactancia 3+'
Append Blank
Replace CONCEPTO With 'Numero de Vientres'   ,COL1 With Str(aw(152),5),COL2 With Str(aw(163),5),COL3 With Str(aw(174),5),COL4 With Str(aw(185),5),COL5 With Str(aw(196),5),COL6 With Str(aw(141),5)
Append Blank
Replace CONCEPTO With 'Promedio DEL'         ,COL1 With Str(aw(153),5),COL2 With Str(aw(164),5),COL3 With Str(aw(175),5),COL4 With Str(aw(186),5),COL5 With Str(aw(197),5),COL6 With Str(aw(142),5)
Append Blank
Replace CONCEPTO With 'Promedio Leche (Kgs.)',COL1 With Str(aw(154),5,1),COL2 With Str(aw(165),5,1),COL3 With Str(aw(176),5,1),COL4 With Str(aw(187),5,1),COL5 With Str(aw(198),5,1),COL6 With Str(aw(143),5,1)
Append Blank
Replace CONCEPTO With 'Promedio LCG   (Kgs.)',COL1 With Str(aw(155),5,1),COL2 With Str(aw(166),5,1),COL3 With Str(aw(177),5,1),COL4 With Str(aw(188),5,1),COL5 With Str(aw(199),5,1),COL6 With Str(aw(144),5,1)
Append Blank
Replace CONCEPTO With 'Promedio LCE   (Kgs.)',COL1 With Str(aw(156),5,1),COL2 With Str(aw(167),5,1),COL3 With Str(aw(178),5,1),COL4 With Str(aw(189),5,1),COL5 With Str(aw(200),5,1),COL6 With Str(aw(145),5,1)
Append Blank
Replace CONCEPTO With 'Promedio Grasa(%)'    ,COL1 With Str(aw(157),5,1),COL2 With Str(aw(168),5,1),COL3 With Str(aw(179),5,1),COL4 With Str(aw(190),5,1),COL5 With Str(aw(201),5,1),COL6 With Str(aw(146),5,1)
Append Blank
Replace CONCEPTO With 'Promedio Proteina (%)',COL1 With Str(aw(158),5,1),COL2 With Str(aw(169),5,1),COL3 With Str(aw(180),5,1),COL4 With Str(aw(191),5,1),COL5 With Str(aw(202),5,1),COL6 With Str(aw(147),5,1)
Append Blank
Replace CONCEPTO With 'Promedio CCS (sl)'    ,COL1 With Str(aw(159),5,1),COL2 With Str(aw(170),5,1),COL3 With Str(aw(181),5,1),COL4 With Str(aw(192),5,1),COL5 With Str(aw(203),5,1),COL6 With Str(aw(148),5,1)
Append Blank
Replace CONCEPTO With 'Prom. Dias Abiertos'  ,COL1 With Str(aw(160),5),COL2 With Str(aw(171),5),COL3 With Str(aw(182),5),COL4 With Str(aw(193),5),COL5 With Str(aw(204),5),COL6 With Str(aw(149),5,1)
Append Blank
Replace CONCEPTO With 'Promedio Dias Seca'   ,COL1 With Str(aw(161),5),COL2 With Str(aw(172),5),COL3 With Str(aw(183),5),COL4 With Str(aw(194),5),COL5 With Str(aw(205),5),COL6 With Str(aw(150),5)
Append Blank
Replace CONCEPTO With 'Promedio 305d EM'     ,COL1 With Str(aw(162),5),COL2 With Str(aw(173),5),COL3 With Str(aw(184),5),COL4 With Str(aw(195),5),COL5 With Str(aw(206),5),COL6 With Str(aw(151),5)
Append Blank
Replace CONCEPTO With ''

Append Blank
Replace CONCEPTO With 'Todas'
Append Blank
Replace CONCEPTO With 'Numero de Vientres'   ,COL1 With Str(aw(222),5),COL2 With Str(aw(233),5),COL3 With Str(aw(244),5),COL4 With Str(aw(255),5),COL5 With Str(aw(266),5),COL6 With Str(aw(211),5)
Append Blank
Replace CONCEPTO With 'Promedio DEL'         ,COL1 With Str(aw(223),5),COL2 With Str(aw(234),5),COL3 With Str(aw(245),5),COL4 With Str(aw(256),5),COL5 With Str(aw(267),5),COL6 With Str(aw(212),5)
Append Blank
Replace CONCEPTO With 'Promedio Leche (Kgs.)',COL1 With Str(aw(224),5,1),COL2 With Str(aw(235),5,1),COL3 With Str(aw(246),5,1),COL4 With Str(aw(257),5,1),COL5 With Str(aw(268),5,1),COL6 With Str(aw(213),5,1)
Append Blank
Replace CONCEPTO With 'Promedio LCG   (Kgs.)',COL1 With Str(aw(225),5,1),COL2 With Str(aw(236),5,1),COL3 With Str(aw(247),5,1),COL4 With Str(aw(258),5,1),COL5 With Str(aw(269),5,1),COL6 With Str(aw(214),5,1)
Append Blank
Replace CONCEPTO With 'Promedio LCE   (Kgs.)',COL1 With Str(aw(226),5,1),COL2 With Str(aw(237),5,1),COL3 With Str(aw(248),5,1),COL4 With Str(aw(259),5,1),COL5 With Str(aw(270),5,1),COL6 With Str(aw(215),5,1)
Append Blank
Replace CONCEPTO With 'Promedio Grasa(%)'    ,COL1 With Str(aw(227),5,1),COL2 With Str(aw(238),5,1),COL3 With Str(aw(249),5,1),COL4 With Str(aw(260),5,1),COL5 With Str(aw(271),5,1),COL6 With Str(aw(216),5,1)
Append Blank
Replace CONCEPTO With 'Promedio Proteina (%)',COL1 With Str(aw(228),5,1),COL2 With Str(aw(239),5,1),COL3 With Str(aw(250),5,1),COL4 With Str(aw(261),5,1),COL5 With Str(aw(272),5,1),COL6 With Str(aw(217),5,1)
Append Blank
Replace CONCEPTO With 'Promedio CCS (sl)'    ,COL1 With Str(aw(229),5,1),COL2 With Str(aw(240),5,1),COL3 With Str(aw(251),5,1),COL4 With Str(aw(262),5,1),COL5 With Str(aw(273),5,1),COL6 With Str(aw(218),5,1)
Append Blank
Replace CONCEPTO With 'Prom. Dias Abiertos'  ,COL1 With Str(aw(230),5),COL2 With Str(aw(241),5),COL3 With Str(aw(252),5),COL4 With Str(aw(263),5),COL5 With Str(aw(274),5),COL6 With Str(aw(219),5)
Append Blank
Replace CONCEPTO With 'Promedio Dias Seca'   ,COL1 With Str(aw(231),5),COL2 With Str(aw(242),5),COL3 With Str(aw(253),5),COL4 With Str(aw(264),5),COL5 With Str(aw(275),5),COL6 With Str(aw(220),5)
Append Blank
Replace CONCEPTO With 'Promedio 305d EM'     ,COL1 With Str(aw(232),5),COL2 With Str(aw(243),5),COL3 With Str(aw(254),5),COL4 With Str(aw(265),5),COL5 With Str(aw(276),5),COL6 With Str(aw(221),5)
Append Blank
Replace CONCEPTO With ''
GO TOP
RETURN



* ANALISIS DE PERSISTENCIA EN PRODUCCION
* --------------------------------------
PROC RV10167
Select REG
Set Order To 2
Calculate MAX(pesa) To mpesa
Set Filter To NP>0 And PRM>0 And PESA-FPAR>7 And PESA>=mPESA-7 &&And m305>0 

DIMENSION aw(200),sw(30)
aw=0
sw=0
dat=0

SCAN
xx=(pesa-fpar)-(pesa-fsec)
dat=dat+1
**-------------------------------------*
do case 
	** Lactancia 1
	** -----------
	case np=1 and pesa=mpesa
		aw(1)=aw(1)+1
		aw(2)=aw(2)+prm
		aw(3)=aw(3)+xx

		aw(4)=aw(2)/aw(1)
		aw(5)=aw(3)/aw(1)

		if tota>0
		aw(6)=aw(6)+1
		aw(7)=aw(7)+tota
		aw(8)=aw(7)/aw(6)
		endi
		if aw(1)>0
			sw(1)=(aw(4)/aw(8)*100)
		endi

	do case
		case xx<45
			aw(9)=aw(9)+1
			aw(10)=aw(10)+prm
			aw(11)=aw(11)+xx

			aw(12)=aw(10)/aw(9)
			aw(13)=aw(11)/aw(9)

		if tota>0
			aw(14)=aw(14)+1
			aw(15)=aw(15)+tota
			aw(16)=aw(15)/aw(14)
		endi
		if aw(9)>0
			sw(2)=(aw(12)/aw(16)*100)
		endi
	
		case xx>=45 and xx<=100
			aw(17)=aw(17)+1
			aw(18)=aw(18)+prm
			aw(19)=aw(19)+xx

			aw(20)=aw(18)/aw(17)
			aw(21)=aw(19)/aw(17)

		if tota>0
			aw(22)=aw(22)+1
			aw(23)=aw(23)+tota
			aw(24)=aw(23)/aw(22)
		endi
		if aw(17)>0
			sw(3)=(aw(20)/aw(24)*100)
		endi
	
		case xx>100 and xx<=200
			aw(25)=aw(25)+1
			aw(26)=aw(26)+prm
			aw(27)=aw(27)+xx

			aw(28)=aw(26)/aw(25)
			aw(29)=aw(27)/aw(25)

		if tota>0
			aw(30)=aw(30)+1
			aw(31)=aw(31)+tota
			aw(32)=aw(31)/aw(30)
		endi
		if aw(25)>0
			sw(4)=(aw(28)/aw(32)*100)
		endi

		case xx>200 and xx<=300
			aw(33)=aw(33)+1
			aw(34)=aw(34)+prm
			aw(35)=aw(35)+xx

			aw(36)=aw(34)/aw(33)
			aw(37)=aw(35)/aw(33)

		if tota>0
			aw(38)=aw(38)+1
			aw(39)=aw(39)+tota
			aw(40)=aw(39)/aw(38)
		endi
		if aw(33)>0
			sw(5)=(aw(36)/aw(40)*100)
		endi

		case xx>300
			aw(41)=aw(41)+1
			aw(42)=aw(42)+prm
			aw(43)=aw(43)+xx

			aw(44)=aw(42)/aw(41)
			aw(45)=aw(43)/aw(41)

		if tota>0
			aw(46)=aw(46)+1
			aw(47)=aw(47)+tota
			aw(48)=aw(47)/aw(46)
		endi
		if aw(41)>0
			sw(6)=(aw(44)/aw(48)*100)
		endi
	
		endcase

	** Lactancia 2
	** -----------
	case np=2 and pesa=mpesa
		aw(51)=aw(51)+1
		aw(52)=aw(52)+prm
		aw(53)=aw(53)+xx

		aw(54)=aw(52)/aw(51)
		aw(55)=aw(53)/aw(51)

		if tota>0
		aw(56)=aw(56)+1
		aw(57)=aw(57)+tota
		aw(58)=aw(57)/aw(56)
		endi
		if aw(51)>0
			sw(7)=(aw(54)/aw(58)*100)
		endi

	do case
		case xx<45
			aw(59)=aw(59)+1
			aw(60)=aw(60)+prm
			aw(61)=aw(61)+xx

			aw(62)=aw(60)/aw(59)
			aw(63)=aw(61)/aw(59)

		if tota>0
			aw(64)=aw(64)+1
			aw(65)=aw(65)+tota
			aw(66)=aw(65)/aw(64)
		endi
		if aw(59)>0
			sw(8)=(aw(62)/aw(66)*100)
		endi
	
		case xx>=45 and xx<=100
			aw(67)=aw(67)+1
			aw(68)=aw(68)+prm
			aw(69)=aw(69)+xx

			aw(70)=aw(68)/aw(67)
			aw(71)=aw(69)/aw(67)

		if tota>0
			aw(72)=aw(72)+1
			aw(73)=aw(73)+tota
			aw(74)=aw(73)/aw(72)
		endi
		if aw(67)>0
			sw(9)=(aw(70)/aw(74)*100)
		endi
	
		case xx>100 and xx<=200
			aw(75)=aw(75)+1
			aw(76)=aw(76)+prm
			aw(77)=aw(77)+xx

			aw(78)=aw(76)/aw(75)
			aw(79)=aw(77)/aw(75)

		if tota>0
			aw(80)=aw(80)+1
			aw(81)=aw(81)+tota
			aw(82)=aw(81)/aw(80)
		endi
		if aw(75)>0
			sw(10)=(aw(78)/aw(82)*100)
		endi

		case xx>200 and xx<=300
			aw(83)=aw(83)+1
			aw(84)=aw(84)+prm
			aw(85)=aw(85)+xx

			aw(86)=aw(84)/aw(83)
			aw(87)=aw(85)/aw(83)

		if tota>0
			aw(88)=aw(88)+1
			aw(89)=aw(89)+tota
			aw(90)=aw(89)/aw(88)
		endi
		if aw(83)>0
			sw(11)=(aw(86)/aw(90)*100)
		endi

		case xx>300
			aw(91)=aw(91)+1
			aw(92)=aw(92)+prm
			aw(93)=aw(93)+xx

			aw(94)=aw(92)/aw(91)
			aw(95)=aw(93)/aw(91)

		if tota>0
			aw(96)=aw(96)+1
			aw(97)=aw(97)+tota
			aw(98)=aw(97)/aw(96)
		endi
		if aw(91)>0
			sw(12)=(aw(94)/aw(98)*100)
		endi
	
		endcase

	** Lactancia 3+
	** ------------
	case np>=3 and pesa=mpesa
		aw(101)=aw(101)+1
		aw(102)=aw(102)+prm
		aw(103)=aw(103)+xx

		aw(104)=aw(102)/aw(101)
		aw(105)=aw(103)/aw(101)

		if tota>0
		aw(106)=aw(106)+1
		aw(107)=aw(107)+tota
		aw(108)=aw(107)/aw(106)
		endi
		if aw(101)>0
			sw(13)=(aw(104)/aw(108)*100)
		endi

	do case
		case xx<45
			aw(109)=aw(109)+1
			aw(110)=aw(110)+prm
			aw(111)=aw(111)+xx

			aw(112)=aw(110)/aw(109)
			aw(113)=aw(111)/aw(109)

		if tota>0
			aw(114)=aw(114)+1
			aw(115)=aw(115)+tota
			aw(116)=aw(115)/aw(114)
		endi
		if aw(109)>0
			sw(14)=(aw(112)/aw(116)*100)
		endi
	
		case xx>=45 and xx<=100
			aw(117)=aw(117)+1
			aw(118)=aw(118)+prm
			aw(119)=aw(119)+xx

			aw(120)=aw(118)/aw(117)
			aw(121)=aw(119)/aw(117)

		if tota>0
			aw(122)=aw(122)+1
			aw(123)=aw(123)+tota
			aw(124)=aw(123)/aw(122)
		endi
		if aw(117)>0
			sw(15)=(aw(120)/aw(124)*100)
		endi
	
		case xx>100 and xx<=200
			aw(125)=aw(125)+1
			aw(126)=aw(126)+prm
			aw(127)=aw(127)+xx

			aw(128)=aw(126)/aw(125)
			aw(129)=aw(127)/aw(125)

		if tota>0
			aw(130)=aw(130)+1
			aw(131)=aw(131)+tota
			aw(132)=aw(131)/aw(130)
		endi
		if aw(125)>0
			sw(16)=(aw(128)/aw(132)*100)
		endi

		case xx>200 and xx<=300
			aw(133)=aw(133)+1
			aw(134)=aw(134)+prm
			aw(135)=aw(135)+xx

			aw(136)=aw(134)/aw(133)
			aw(137)=aw(135)/aw(133)

		if tota>0
			aw(138)=aw(138)+1
			aw(139)=aw(139)+tota
			aw(140)=aw(139)/aw(138)
		endi
		if aw(133)>0
			sw(17)=(aw(136)/aw(140)*100)
		endi

		case xx>300
			aw(141)=aw(141)+1
			aw(142)=aw(142)+prm
			aw(143)=aw(143)+xx

			aw(144)=aw(142)/aw(141)
			aw(145)=aw(143)/aw(141)

		if tota>0
			aw(146)=aw(146)+1
			aw(147)=aw(147)+tota
			aw(148)=aw(147)/aw(146)
		endi
		if aw(141)>0
			sw(18)=(aw(144)/aw(148)*100)
		endi
	
		endcase
	endcase

	** Todas las Lactancias
	** --------------------
	IF np>=1 and pesa=mpesa
		aw(151)=aw(151)+1
		aw(152)=aw(152)+prm
		aw(153)=aw(153)+xx

		aw(154)=aw(152)/aw(151)
		aw(155)=aw(153)/aw(151)

		if tota>0
		aw(156)=aw(156)+1
		aw(157)=aw(157)+tota
		aw(158)=aw(157)/aw(156)
		endi
		if aw(151)>0
			sw(19)=(aw(154)/aw(158)*100)
		endi

	do case
		case xx<45
			aw(159)=aw(159)+1
			aw(160)=aw(160)+prm
			aw(161)=aw(161)+xx

			aw(162)=aw(160)/aw(159)
			aw(163)=aw(161)/aw(159)

		if tota>0
			aw(164)=aw(164)+1
			aw(165)=aw(165)+tota
			aw(166)=aw(165)/aw(164)
		endi
		if aw(159)>0
			sw(20)=(aw(162)/aw(166)*100)
		endi
	
		case xx>=45 and xx<=100
			aw(167)=aw(167)+1
			aw(168)=aw(168)+prm
			aw(169)=aw(169)+xx

			aw(170)=aw(168)/aw(167)
			aw(171)=aw(169)/aw(167)

		if tota>0
			aw(172)=aw(172)+1
			aw(173)=aw(173)+tota
			aw(174)=aw(173)/aw(172)
		endi
		if aw(167)>0
			sw(21)=(aw(170)/aw(174)*100)
		endi
	
		case xx>100 and xx<=200
			aw(175)=aw(175)+1
			aw(176)=aw(176)+prm
			aw(177)=aw(177)+xx

			aw(178)=aw(176)/aw(175)
			aw(179)=aw(177)/aw(175)

		if tota>0
			aw(180)=aw(180)+1
			aw(181)=aw(181)+tota
			aw(182)=aw(181)/aw(180)
		endi
		if aw(175)>0
			sw(22)=(aw(178)/aw(182)*100)
		endi

		case xx>200 and xx<=300
			aw(183)=aw(183)+1
			aw(184)=aw(184)+prm
			aw(185)=aw(185)+xx

			aw(186)=aw(184)/aw(183)
			aw(187)=aw(185)/aw(183)

		if tota>0
			aw(188)=aw(188)+1
			aw(189)=aw(189)+tota
			aw(190)=aw(189)/aw(188)
		endi
		if aw(183)>0
			sw(23)=(aw(186)/aw(190)*100)
		endi

		case xx>300
			aw(191)=aw(191)+1
			aw(192)=aw(192)+prm
			aw(193)=aw(193)+xx

			aw(194)=aw(192)/aw(191)
			aw(195)=aw(193)/aw(191)

		if tota>0
			aw(196)=aw(196)+1
			aw(197)=aw(197)+tota
			aw(198)=aw(197)/aw(196)
		endi
		if aw(191)>0
			sw(24)=(aw(194)/aw(198)*100)
		endi
	endcase
	ENDI
Endscan

Select REG
Set Filter To

Create Cursor REPORTE (Concepto c(30),COL1 c(7),COL2 c(7),COL3 c(5),COL4 c(5),COL5 c(5),COL6 c(5))
Append Blank
Replace CONCEPTO With 'Lactancia 1'
Append Blank
Replace CONCEPTO With 'Numero de Vientres (Actual)    ',COL1 With Str(aw(9),4),COL2 With Str(aw(17),4),COL3 With Str(aw(25),4),COL4 With Str(aw(33),4),COL5 With Str(aw(41),4),COL6 With Str(aw(1),4)
Append Blank
Replace CONCEPTO With 'Numero de Vientres (Previo)    ',COL1 With Str(aw(14),4),COL2 With Str(aw(22),4),COL3 With Str(aw(30),4),COL4 With Str(aw(38),4),COL5 With Str(aw(46),4),COL6 With Str(aw(6),4)
Append Blank
Replace CONCEPTO With 'Promedio de Dias en Leche      ',COL1 With Str(aw(13),4),COL2 With Str(aw(21),4),COL3 With Str(aw(29),4),COL4 With Str(aw(37),4),COL5 With Str(aw(45),4),COL6 With Str(aw(5),4)
Append Blank
Replace CONCEPTO With 'Promedio Leche Kgs. (Actual)   ',COL1 With Str(aw(12),4,1),COL2 With Str(aw(20),4,1),COL3 With Str(aw(28),4,1),COL4 With Str(aw(36),4,1),COL5 With Str(aw(44),4,1),COL6 With Str(aw(4),4,1)
Append Blank
Replace CONCEPTO With 'Promedio Leche Kgs. (Previo)   ',COL1 With Str(aw(16),4,1),COL2 With Str(aw(24),4,1),COL3 With Str(aw(32),4,1),COL4 With Str(aw(40),4,1),COL5 With Str(aw(48),4,1),COL6 With Str(aw(8),4,1)
Append Blank
Replace CONCEPTO With 'Persistencia (%)               ',COL1 With '',COL2 With Str(sw(3),5,1),COL3 With Str(sw(4),5,1),COL4 With Str(sw(5),5,1),COL5 With Str(sw(6),5,1),COL6 With Str(sw(1),5,1)
Append Blank
Replace CONCEPTO With ''

Append Blank
Replace CONCEPTO With 'Lactancia 2'
Append Blank
Replace CONCEPTO With 'Numero de Vientres (Actual)    ',COL1 With Str(aw(59),4),COL2 With Str(aw(67),4),COL3 With Str(aw(75),4),COL4 With Str(aw(83),4),COL5 With Str(aw(91),4),COL6 With Str(aw(51),4)
Append Blank
Replace CONCEPTO With 'Numero de Vientres (Previo)    ',COL1 With Str(aw(64),4),COL2 With Str(aw(72),4),COL3 With Str(aw(80),4),COL4 With Str(aw(88),4),COL5 With Str(aw(96),4),COL6 With Str(aw(56),4)
Append Blank
Replace CONCEPTO With 'Promedio de Dias en Leche      ',COL1 With Str(aw(63),4),COL2 With Str(aw(71),4),COL3 With Str(aw(79),4),COL4 With Str(aw(87),4),COL5 With Str(aw(95),4),COL6 With Str(aw(55),4)
Append Blank
Replace CONCEPTO With 'Promedio Leche Kgs. (Actual)   ',COL1 With Str(aw(62),4,1),COL2 With Str(aw(70),4,1),COL3 With Str(aw(78),4,1),COL4 With Str(aw(86),4,1),COL5 With Str(aw(94),4,1),COL6 With Str(aw(54),4,1)
Append Blank
Replace CONCEPTO With 'Promedio Leche Kgs. (Previo)   ',COL1 With Str(aw(66),4,1),COL2 With Str(aw(74),4,1),COL3 With Str(aw(82),4,1),COL4 With Str(aw(90),4,1),COL5 With Str(aw(98),4,1),COL6 With Str(aw(58),4,1)
Append Blank
Replace CONCEPTO With 'Persistencia (%)               ',COL1 With '',COL2 With Str(sw(9),5,1),COL3 With Str(sw(10),5,1),COL4 With Str(sw(11),5,1),COL5 With Str(sw(12),5,1),COL6 With Str(sw(7),5,1)
Append Blank
Replace CONCEPTO With ''

Append Blank
Replace CONCEPTO With 'Lactancia 3+'
Append Blank
Replace CONCEPTO With 'Numero de Vientres (Actual)    ',COL1 With Str(aw(109),4),COL2 With Str(aw(117),4),COL3 With Str(aw(125),4),COL4 With Str(aw(133),4),COL5 With Str(aw(141),4),COL6 With Str(aw(101),4)
Append Blank
Replace CONCEPTO With 'Numero de Vientres (Previo)    ',COL1 With Str(aw(114),4),COL2 With Str(aw(122),4),COL3 With Str(aw(130),4),COL4 With Str(aw(138),4),COL5 With Str(aw(146),4),COL6 With Str(aw(106),4)
Append Blank
Replace CONCEPTO With 'Promedio de Dias en Leche      ',COL1 With Str(aw(113),4),COL2 With Str(aw(121),4),COL3 With Str(aw(129),4),COL4 With Str(aw(137),4),COL5 With Str(aw(145),4),COL6 With Str(aw(105),4)
Append Blank
Replace CONCEPTO With 'Promedio Leche Kgs. (Actual)   ',COL1 With Str(aw(112),4,1),COL2 With Str(aw(120),4,1),COL3 With Str(aw(128),4,1),COL4 With Str(aw(136),4,1),COL5 With Str(aw(144),4,1),COL6 With Str(aw(104),4,1)
Append Blank
Replace CONCEPTO With 'Promedio Leche Kgs. (Previo)   ',COL1 With Str(aw(116),4,1),COL2 With Str(aw(124),4,1),COL3 With Str(aw(132),4,1),COL4 With Str(aw(140),4,1),COL5 With Str(aw(148),4,1),COL6 With Str(aw(108),4,1)
Append Blank
Replace CONCEPTO With 'Persistencia (%)               ',COL1 With '',COL2 With Str(sw(15),5,1),COL3 With Str(sw(16),5,1),COL4 With Str(sw(17),5,1),COL5 With Str(sw(18),5,1),COL6 With Str(sw(13),5,1)
Append Blank
Replace CONCEPTO With ''


Append Blank
Replace CONCEPTO With 'Todas las Lactancias'
Append Blank
Replace CONCEPTO With 'Numero de Vientres (Actual)    ',COL1 With Str(aw(159),4),COL2 With Str(aw(167),4),COL3 With Str(aw(175),4),COL4 With Str(aw(183),4),COL5 With Str(aw(191),4),COL6 With Str(aw(151),4)
Append Blank
Replace CONCEPTO With 'Numero de Vientres (Previo)    ',COL1 With Str(aw(164),4),COL2 With Str(aw(172),4),COL3 With Str(aw(180),4),COL4 With Str(aw(188),4),COL5 With Str(aw(196),4),COL6 With Str(aw(156),4)
Append Blank
Replace CONCEPTO With 'Promedio de Dias en Leche      ',COL1 With Str(aw(163),4),COL2 With Str(aw(171),4),COL3 With Str(aw(179),4),COL4 With Str(aw(187),4),COL5 With Str(aw(195),4),COL6 With Str(aw(155),4)
Append Blank
Replace CONCEPTO With 'Promedio Leche Kgs. (Actual)   ',COL1 With Str(aw(162),4,1),COL2 With Str(aw(170),4,1),COL3 With Str(aw(178),4,1),COL4 With Str(aw(186),4,1),COL5 With Str(aw(194),4,1),COL6 With Str(aw(154),4,1)
Append Blank
Replace CONCEPTO With 'Promedio Leche Kgs. (Previo)   ',COL1 With Str(aw(166),4,1),COL2 With Str(aw(174),4,1),COL3 With Str(aw(182),4,1),COL4 With Str(aw(190),4,1),COL5 With Str(aw(198),4,1),COL6 With Str(aw(158),4,1)
Append Blank
Replace CONCEPTO With 'Persistencia (%)               ',COL1 With '',COL2 With Str(sw(21),5,1),COL3 With Str(sw(22),5,1),COL4 With Str(sw(23),5,1),COL5 With Str(sw(24),5,1),COL6 With Str(sw(19),5,1)
Append Blank
Replace CONCEPTO With ''

Append Blank
Replace CONCEPTO With 'Metas para Persistencia (%)'
Append Blank
Replace CONCEPTO With 'Lactancia 1',COL1 With '-',COL2 With '106-109',COL3 With '95-97',COL4 With '93-96',COL5 With '91-93'
Append Blank
Replace CONCEPTO With 'Lactancia 2',COL1 With '-',COL2 With '103-107',COL3 With '91-93',COL4 With '88-91',COL5 With '85-87'
Append Blank
Replace CONCEPTO With 'Lactancia 3+',COL1 With '-',COL2 With '103-105',COL3 With '91-93',COL4 With '88-91',COL5 With '85-87'
Append Blank
Replace CONCEPTO With 'Todas las Lactancias',COL1 With '-',COL2 With '104-106',COL3 With '92-95',COL4 With '90-93',COL5 With '88-90'
Append Blank
Replace CONCEPTO With ''

GO TOP
RETURN

* REPORTE DE PRUEBAS DE MASTITIS
* ------------------------------
PROC RV1034
*Select REG
*Set Order To 2
*Set Filter To FB2=B And FSEC=B
*Calculate CNT() To NVP


Select Mast
Go Bottom
xFMAX=FTES

CALCULATE CNT() FOR FTES=xFMAX TO NVP

Set Filter To FTES=xFMAX
*------------------------------------------------------*
CALC CNT() FOR DD=1 TO DD1
CALC CNT() FOR DI=1 TO DI1
CALC CNT() FOR TD=1 TO TD1
CALC CNT() FOR TI=1 TO TI1

CALC CNT() FOR DD=2 TO DD2
CALC CNT() FOR DI=2 TO DI2
CALC CNT() FOR TD=2 TO TD2
CALC CNT() FOR TI=2 TO TI2

CALC CNT() FOR DD=3 TO DD3
CALC CNT() FOR DI=3 TO DI3
CALC CNT() FOR TD=3 TO TD3
CALC CNT() FOR TI=3 TO TI3

CALC CNT() FOR DD=4 TO DD4
CALC CNT() FOR DI=4 TO DI4
CALC CNT() FOR TD=4 TO TD4
CALC CNT() FOR TI=4 TO TI4

CALC CNT() FOR DD=5 TO DD5
CALC CNT() FOR DI=5 TO DI5
CALC CNT() FOR TD=5 TO TD5
CALC CNT() FOR TI=5 TO TI5


CALC CNT() FOR DD=6 TO DD6
CALC CNT() FOR DI=6 TO DI6
CALC CNT() FOR TD=6 TO TD6
CALC CNT() FOR TI=6 TO TI6

CALC CNT() FOR DD=7 TO DD7
CALC CNT() FOR DI=7 TO DI7
CALC CNT() FOR TD=7 TO TD7
CALC CNT() FOR TI=7 TO TI7

CALC CNT() FOR DD=8 TO DD8
CALC CNT() FOR DI=8 TO DI8
CALC CNT() FOR TD=8 TO TD8
CALC CNT() FOR TI=8 TO TI8

CALC CNT() FOR DD=9 TO DD9
CALC CNT() FOR DI=9 TO DI9
CALC CNT() FOR TD=9 TO TD9
CALC CNT() FOR TI=9 TO TI9

*CALC CNT() FOR DD=9 OR DI=9 OR TD=9 OR TI=9 TO JJ

FF=DD1+DI1+TD1+TI1
GG=DD2+DI2+TD2+TI2
HH=DD3+DI3+TD3+TI3
II=DD4+DI4+TD4+TI4
KK=DD5+DI5+TD5+TI5
MM=DD6+DI6+TD6+TI6
NN=DD7+DI7+TD7+TI7
OO=DD8+DI8+TD8+TI8
JJ=DD9+DI9+TD9+TI9

II2=II+KK+MM+NN+OO

LL=NVP*4-(FF+GG+HH+II+KK+MM+NN+OO+JJ)
TC=NVP*4
*------------------------------------------------------*
CALC CNT() FOR DD>0 AND DD<9 OR DI>0 AND DI<9 OR TD>0 AND TD<9 OR TI>0 AND TI<9 TO VPOS
CALC CNT() FOR DD>3 AND DD<9 OR DI>3 AND DI<9 OR TD>3 AND TD<9 OR TI>3 AND TI<9 TO VMC
CALC CNT() FOR DD>8 OR DI>8 OR TD>8 OR TI>8 TO VCP
*------------------------------------------------------*

Select REG
Set Filter To

Create Cursor REPORTE (concepto c(30),COL1 c(6),COL2 c(6),COL3 c(6),COL4 c(6))
	Append Blank
	Replace CONCEPTO With 'RESULTADOS POR CUARTO'
	Append Blank
	Replace CONCEPTO With "Total de Cuartos ",COL1 With Str(TC,5),COL2 With '100.0 %'
	Append Blank
	Replace CONCEPTO With "Cuartos Grado 1  ",COL1 With Str(FF,5),COL2 With Str(FF/TC*100,4,1)
	Append Blank
	Replace CONCEPTO With "Cuartos Grado 2  ",COL1 With Str(GG,5),COL2 With Str(GG/TC*100,4,1)
	Append Blank
	Replace CONCEPTO With "Cuartos Grado 3  ",COL1 With Str(HH,5),COL2 With Str(HH/TC*100,4,1)
	Append Blank
	Replace CONCEPTO With "Cuartos Clinicos ",COL1 With Str(II2,5),COL2 With Str(II2/TC*100,4,1)
	Append Blank
	Replace CONCEPTO With "Cuartos Perdidos ",COL1 With Str(JJ,5),COL2 With Str(JJ/TC*100,4,1)
	Append Blank
	Replace CONCEPTO With "Cuartos Libres   ",COL1 With Str(LL,5),COL2 With Str(LL/TC*100,4,1)
	Append Blank
	Replace CONCEPTO With ""

	Append Blank
	Replace CONCEPTO With 'RESULTADOS POR VIENTRE'
	Append Blank
	Replace CONCEPTO With "Vientres en Total   ",COL1 With Str(NVP,4),COL2 With '100.0 %'
	Append Blank
	Replace CONCEPTO With "Con Prueba Negativa ",COL1 With Str(NVP-VPOS,4),COL2 With Str((NVP-VPOS)/NVP*100,4,1)
	Append Blank
	Replace CONCEPTO With "Con Prueba Positiva ",COL1 With Str(VPOS,4),COL2 With Str(VPOS/NVP*100,4,1)
	Append Blank
	Replace CONCEPTO With "Hasta Grado 3       ",COL1 With Str(VPOS-VMC,4),COL2 With str(((VPOS-VMC)/NVP)*100,4,1)
	Append Blank
	Replace CONCEPTO With "Con Mastitis Clinica",COL1 With Str(VMC,4),COL2 With Str(VMC/NVP*100,4,1)
	Append Blank
	Replace CONCEPTO With "Con Cuartos Perdidos",COL1 With Str(VCP,4),COL2 With Str(VCP/NVP*100,4,1)
		
	Append Blank
	Replace CONCEPTO With ""
	Append Blank
	Replace CONCEPTO With "NUMERO DE CUARTOS"
	Append Blank
	Replace CONCEPTO With "POSICION",COL1 WITH "  DI",COL2 WITH "  DD",COL3 WITH "  TI",COL4 WITH "  TD"
	Append Blank
	Replace CONCEPTO With "GRADO 1",COL1 With STR(DI1,4),COL2 With Str(DD1,4),COL3 With Str(TI1,4),COL4 With Str(TD1,4) 
	Append Blank
	Replace CONCEPTO With "GRADO 2",COL1 With STR(DI2,4),COL2 With Str(DD2,4),COL3 With Str(TI2,4),COL4 With Str(TD2,4) 
	Append Blank
	Replace CONCEPTO With "GRADO 3",COL1 With STR(DI3,4),COL2 With Str(DD3,4),COL3 With Str(TI3,4),COL4 With Str(TD3,4) 
	Append Blank
	Replace CONCEPTO With "GRADO 4",COL1 With STR(DI4,4),COL2 With Str(DD4,4),COL3 With Str(TI4,4),COL4 With Str(TD4,4) 

	Append Blank
	Replace CONCEPTO With "GRADO 5",COL1 With STR(DI5,4),COL2 With Str(DD5,4),COL3 With Str(TI5,4),COL4 With Str(TD5,4) 
	Append Blank
	Replace CONCEPTO With "GRADO 6",COL1 With STR(DI6,4),COL2 With Str(DD6,4),COL3 With Str(TI6,4),COL4 With Str(TD6,4) 
	Append Blank
	Replace CONCEPTO With "GRADO 7",COL1 With STR(DI7,4),COL2 With Str(DD7,4),COL3 With Str(TI7,4),COL4 With Str(TD7,4) 
	Append Blank
	Replace CONCEPTO With "GRADO 8",COL1 With STR(DI8,4),COL2 With Str(DD8,4),COL3 With Str(TI8,4),COL4 With Str(TD8,4) 
	Append Blank
	Replace CONCEPTO With "GRADO 9",COL1 With STR(DI9,4),COL2 With Str(DD9,4),COL3 With Str(TI9,4),COL4 With Str(TD9,4) 

Go Top
Return


* ESTADISTICA DE PRODUCCION POR CORRAL Y ULTIMAS DIEZ PESADAS  ³
* ----------------------------------------------------------------
PROCEDURE RV10172
PARAMETERS nLOTE,nLAC1,nLAC2,nORIGEN
Public xLOTE,xLAC1,xLAC2

If nLOTE=0
	xLote="And CORR>0"
Else
	xLote="And CORR=nLote"	
ENDIF

IF nLAC1=0
	xLACTANCIA="And NP>0"
ELSE
	xLACTANCIA="And NP>=nLAC1 And NP<=nLAC2"	
Endif	

Declare aw(60),pw(16),tw(10),tv(5)
mc=0
pw=0
tw=0
tv=0

Create Table xDATOS (Concepto c(8),COL1 c(5),COL2 c(5),COL3 c(5),COL4 c(5),COL5 c(5),COL6 c(5),COL7 c(5),COL8 c(5),COL9 c(5),COL10 c(4),COL11 c(4),COL12 c(4),COL13 c(4),COL14 c(4),COL15 c(4),COL16 c(4),COL17 c(4),COL18 c(4),COL19 c(4),COL20 c(10),COL21 c(5),COL22 c(5),COL23 c(3),COL24 c(1))

Select PROD
Set Order To 2

Select REG
Set Order To 2

Set Filter To FB2=B AND PROC=nORIGEN &xLote &xLACTANCIA
GO TOP

SCAN  && ----------------------------------------------*
xID=REG.ID
xNP=REG.NP
xAVAL=0
If M305>0
	xAVAL=(M305/X305)*100
EndIf
** Traslada las pesadas a variables
** --------------------------------
xx=0
aw=space(2)
	Select PROD
	xIDP=Str(xID,5)+Dtoc(FP)
	Seek Left(xIDP,5)
	
	If Found()
			Scan While xID=PROD.ID And xNP=PROD.NP and xx<10
				xx=xx+1
				aw(xx)=alltrim(aw(xx)+str(prd,3))
				If Len(aw(xx))=1
 					aw(xx)=" "+aw(xx)
				Endif
			Endscan
	Else
		aw=space(2)
		xx=0
	Endif
** --------------------------------

Select xDATOS
Append Blank
Replace CONCEPTO With Str(REG.ID,5),COL1 With Str(REG.CORR,3),COL2 With Str(REG.NP,2),;
				COL3 With REG.STAT,COL4 With Str(REG.DIA,4),COL5 With Str(REG.DPR,4),COL6 With Str(REG.DAB,4),;
				COL7 With Str(REG.NS,2),COL8 With Str(xAVAL,3),COL9 With Str(REG.PRX,5,1),COL10 With aw(10),;
				COL11 With aw(9),COL12 With aw(8),COL13 With aw(7),COL14 With aw(6),COL15 With aw(5),COL16 With aw(4),;
				COL17 With aw(3),COL18 With aw(2),COL19 With aw(1),COL20 With DTOC(REG.PESA),;
				COL21 With Str(REG.PLAC,5,1),COL22 With Str(REG.DPIC,3),COL23 With Str(REG.CONDC,3,1),COL24 With REG.PSV
				
*Select REG
	*xx=1
*	aw=0
*	xID=REG.ID

	* Estadistica de Estado Reproductivo
	*Do Case
	*	Case Left(REG.STAT,1)="I"
	*		tv(1)=tv(1)+1
	*	Case Left(REG.STAT,1)="C"
	*		tv(2)=tv(2)+1
	**	Otherwise
	*	    tv(3)=tv(3)+1
*	Endcase
ENDSCAN

Select REG
Set Filter To

Select xDATOS
RETURN


* ANALISIS DE INTERVALOS ENTRE SERVICIOS  
* --------------------------------------
PROCEDURE RV10164
GO TOP
Dimension aw(60)
aw=0
dat=0
nciclo=0

Scan 
	dat=dat+1
	nciclo=nciclo+ciclo
do case
	** Lactancia 1
	** -----------
	case np=1
	aw(1)=aw(1)+1
		do case
			case ciclo>=0 AND ciclo<4 
				aw(4)=aw(4)+1
			case ciclo>=4 and ciclo<=17
				aw(5)=aw(5)+1
			case ciclo>=18 and ciclo<=24
				aw(6)=aw(6)+1
			case ciclo>=25 and ciclo<=35
				aw(7)=aw(7)+1
			case ciclo>=36 and ciclo<=48
				aw(8)=aw(8)+1
			case ciclo>=49
				aw(9)=aw(9)+1
		endcase		
	
	** Lactancia 2
	** -----------
	case np=2
	aw(2)=aw(2)+1
		do case
			case ciclo>=0 AND ciclo<4 
				aw(10)=aw(10)+1
			case ciclo>=4 and ciclo<=17
				aw(11)=aw(11)+1
			case ciclo>=18 and ciclo<=24
				aw(12)=aw(12)+1
			case ciclo>=25 and ciclo<=35
				aw(13)=aw(13)+1
			case ciclo>=36 and ciclo<=48
				aw(14)=aw(14)+1
			case ciclo>=49
				aw(15)=aw(15)+1
		endcase		
	
	** Lactancia 3+
	case np>=3
	aw(3)=aw(3)+1
		do case
			case ciclo>=0 AND ciclo<4 
				aw(16)=aw(16)+1
			case ciclo>=4 and ciclo<=17
				aw(17)=aw(17)+1
			case ciclo>=18 and ciclo<=24
				aw(18)=aw(18)+1
			case ciclo>=25 and ciclo<=35
				aw(19)=aw(19)+1
			case ciclo>=36 and ciclo<=48
				aw(20)=aw(20)+1
			case ciclo>=49
				aw(21)=aw(21)+1
		endcase		
endcase
** Todas las lactancias
** -------------------
	aw(22)=aw(22)+1
		do case
			case ciclo>=0 AND ciclo<4 
				aw(23)=aw(23)+1
			case ciclo>=4 and ciclo<=17
				aw(24)=aw(24)+1
			case ciclo>=18 and ciclo<=24
				aw(25)=aw(25)+1
			case ciclo>=25 and ciclo<=35
				aw(26)=aw(26)+1
			case ciclo>=36 and ciclo<=48
				aw(27)=aw(27)+1
			case ciclo>=49
				aw(28)=aw(28)+1
		endcase		
*endi
endscan

if aw(1)>0
	aw(30)=aw(4)/aw(1)*100
	aw(31)=aw(5)/aw(1)*100
	aw(32)=aw(6)/aw(1)*100
	aw(33)=aw(7)/aw(1)*100
	aw(34)=aw(8)/aw(1)*100
	aw(35)=aw(9)/aw(1)*100
endi
if aw(2)>0
	aw(36)=aw(10)/aw(2)*100
	aw(37)=aw(11)/aw(2)*100
	aw(38)=aw(12)/aw(2)*100
	aw(39)=aw(13)/aw(2)*100
	aw(40)=aw(14)/aw(2)*100
	aw(41)=aw(15)/aw(2)*100
endi
if aw(3)>0
	aw(42)=aw(16)/aw(3)*100
	aw(43)=aw(17)/aw(3)*100
	aw(44)=aw(18)/aw(3)*100
	aw(45)=aw(19)/aw(3)*100
	aw(46)=aw(20)/aw(3)*100
	aw(47)=aw(21)/aw(3)*100
endi
if aw(22)>0
	aw(48)=aw(23)/aw(22)*100
	aw(49)=aw(24)/aw(22)*100
	aw(50)=aw(25)/aw(22)*100
	aw(51)=aw(26)/aw(22)*100
	aw(52)=aw(27)/aw(22)*100
	aw(53)=aw(28)/aw(22)*100
Endif

Create Cursor REPORTE (Concepto c(20),COL1 c(5),COL2 c(5),COL3 c(5),COL4 c(5),COL5 c(5),COL6 c(5))
Append Blank
Replace CONCEPTO With 'PORCIENTO'
Append Blank
Replace CONCEPTO With 'Lactancia 1 ',COL1 With Str(aw(30),4,1),COL2 With Str(aw(31),4,1),COL3 With Str(aw(32),4,1),COL4 With Str(aw(33),4,1),COL5 With Str(aw(34),4,1),COL6 With Str(aw(35),4,1)
Append Blank
Replace CONCEPTO With 'Lactancia 2 ',COL1 With Str(aw(36),4,1),COL2 With Str(aw(37),4,1),COL3 With Str(aw(38),4,1),COL4 With Str(aw(39),4,1),COL5 With Str(aw(40),4,1),COL6 With Str(aw(41),4,1)
Append Blank
Replace CONCEPTO With 'Lactancia 3+',COL1 With Str(aw(42),4,1),COL2 With Str(aw(43),4,1),COL3 With Str(aw(44),4,1),COL4 With Str(aw(45),4,1),COL5 With Str(aw(46),4,1),COL6 With Str(aw(47),4,1)
Append Blank
Replace CONCEPTO With 'Todas       ',COL1 With Str(aw(48),4,1),COL2 With Str(aw(49),4,1),COL3 With Str(aw(50),4,1),COL4 With Str(aw(51),4,1),COL5 With Str(aw(52),4,1),COL6 With Str(aw(53),4,1)
Append Blank
Append Blank
Replace CONCEPTO With 'TOTAL'
Append Blank
Replace CONCEPTO With 'Lactancia 1 ',COL1 With Str(aw(4),4),COL2 With Str(aw(5),4),COL3 With Str(aw(6),4),COL4 With Str(aw(7),4),COL5 With Str(aw(8),4),COL6 With Str(aw(9),4)
Append Blank
Replace CONCEPTO With 'Lactancia 2 ',COL1 With Str(aw(10),4),COL2 With Str(aw(11),4),COL3 With Str(aw(12),4),COL4 With Str(aw(13),4),COL5 With Str(aw(14),4),COL6 With Str(aw(15),4)
Append Blank
Replace CONCEPTO With 'Lactancia 3+',COL1 With Str(aw(16),4),COL2 With Str(aw(17),4),COL3 With Str(aw(18),4),COL4 With Str(aw(19),4),COL5 With Str(aw(20),4),COL6 With Str(aw(21),4)
Append Blank
Replace CONCEPTO With 'Todas       ',COL1 With Str(aw(23),4),COL2 With Str(aw(24),4),COL3 With Str(aw(25),4),COL4 With Str(aw(26),4),COL5 With Str(aw(27),4),COL6 With Str(aw(28),4)
Return

* RESUMEN DE INFECCION DE UBRES
* -----------------------------
PROCEDURE RV10169
Declare aw(20)
aw=0

Select REG

Calculate MAX(FCCS) To xFECHA1
Set Filter To NP>0 And FB2=B And FSEC=B And CCS>0 	

SCAN
	xx=(xFECHA1-fpar)-(xFECHA1-fsec)

	** Current test day
	** ----------------
		** Todas las vacas
		** ---------------
			aw(1)=aw(1)+1
		if lsc>0
			aw(16)=aw(16)+ccs
			aw(17)=aw(17)+lsc
				if alsc>0
					aw(18)=aw(18)+accs
					aw(19)=aw(19)+alsc
				endi
		endi
		
		do case
			case lsc>=Q38 
				aw(2)=aw(2)+1
			case lsc<=Q37 
				aw(3)=aw(3)+1
		endcase
	

	** Cuenta diferentes rangos
	** ------------------------
	do case
		case alsc>=Q38 and lsc<Q38   && Infeccion Incidental
			aw(4)=aw(4)+1
			
		case alsc<Q38 and lsc>=Q38   && Nuevas Infecciones
			aw(5)=aw(5)+1

		case alsc>=Q38 and lsc>=Q38   && Vacas Cronicas
			aw(6)=aw(6)+1
	endcase	
	
	** Infectadas con menos de 90 Dias en Leche
	** ----------------------------------------
	if xx<=90 and alsc>=Q38
		aw(7)=aw(7)+1
	endi	

ENDSCAN

if aw(1)>0
	aw(10)=aw(2)/aw(1)*100
endi	
if aw(3)>0
	aw(11)=aw(3)/aw(1)*100
endi	
if aw(4)>0
	aw(12)=aw(4)/aw(1)*100
endi	
if aw(5)>0
	aw(13)=aw(5)/aw(1)*100
endi	
if aw(6)>0
	aw(14)=aw(6)/aw(1)*100
endi	
if aw(7)>0
	aw(15)=aw(7)/aw(1)*100
endi	

Select REG
Set Filter To

Create Cursor REPORTE (Concepto c(35),COL1 c(5),COL2 c(5))
Append Blank
Replace CONCEPTO With 'Lectura Lineal de CCS-(sl)>='+Str(Q38,3,1),COL1 With Str(aw(2),4),COL2 With Str(aw(10),4,1)
Append Blank
Replace CONCEPTO With 'Lectura Lineal de CCS-(sl)<='+Str(Q37,3,1),COL1 With Str(aw(3),4),COL2 With Str(aw(11),4,1)
Append Blank
Replace CONCEPTO With 'Infecciones Incidentales                 ',COL1 With Str(aw(4),4),COL2 With Str(aw(12),4,1)
Append Blank
Replace CONCEPTO With 'Nuevas Infecciones                       ',COL1 With Str(aw(5),4),COL2 With Str(aw(13),4,1)
Append Blank
Replace CONCEPTO With 'Infecciones Cronicas                     ',COL1 With Str(aw(6),4),COL2 With Str(aw(14),4,1)
Append Blank
Replace CONCEPTO With 'Infectadas con <90 Dias en Leche         ',COL1 With Str(aw(7),4),COL2 With Str(aw(15),4,1)
Return


* ANALISIS ANUAL DE ABORTOS
* -------------------------
	PROCEDURE RV10213
*----------------
DIMENSION XS(60),TS(60),MES(12)
xs=0
TG=0
TS=0
mes=1

Select REG 
Set Order TO 2
Set Filter TO
Calculate CNT() For NP>0 And FB2=B TO TV

Select ABORTOS
Set Filter To NP>0 And Year(FECHA)=xan
GO TOP

SCAN
DO CASE

CASE NP=1
xs(51)=xs(51)+1
Do Case
			Case month(FECHA)=1 
					xs(1)=xs(1)+1
	
			case month(FECHA)=2 
					xs(2)=xs(2)+1
	
			case month(FECHA)=3
					xs(3)=xs(3)+1
	
			case month(FECHA)=4
					xs(4)=xs(4)+1
	
			case month(FECHA)=5
					xs(5)=xs(5)+1
	
			case month(FECHA)=6
					xs(6)=xs(6)+1
	
			case month(FECHA)=7
					xs(7)=xs(7)+1
	
			case month(FECHA)=8
					xs(8)=xs(8)+1
	
			case month(FECHA)=9
					xs(9)=xs(9)+1
	
			case month(FECHA)=10 
					xs(10)=xs(10)+1
	
			Case month(FECHA)=11
					xs(11)=xs(11)+1
	
			Case month(FECHA)=12
					xs(12)=xs(12)+1
	
		Endcase

CASE NP=2
xs(52)=xs(52)+1
Do Case
			Case month(FECHA)=1 
					xs(13)=xs(13)+1
	
			case month(FECHA)=2 
					xs(14)=xs(14)+1
	
			case month(FECHA)=3
					xs(15)=xs(15)+1
	
			case month(FECHA)=4
					xs(16)=xs(16)+1
	
			case month(FECHA)=5
					xs(17)=xs(17)+1
	
			case month(FECHA)=6
					xs(18)=xs(18)+1
	
			case month(FECHA)=7
					xs(19)=xs(19)+1
				
			case month(FECHA)=8
					xs(20)=xs(20)+1
	
			case month(FECHA)=9
					xs(21)=xs(21)+1
	
			case month(FECHA)=10 
					xs(22)=xs(22)+1
	
			Case month(FECHA)=11
					xs(23)=xs(23)+1
	
			Case month(FECHA)=12
					xs(24)=xs(24)+1
	
		Endcase


CASE NP>=3
xs(53)=xs(53)+1
Do Case
			Case month(FECHA)=1 
					xs(25)=xs(25)+1
	
			case month(FECHA)=2 
					xs(26)=xs(26)+1
	
			case month(FECHA)=3
					xs(27)=xs(27)+1
	
			case month(FECHA)=4
					xs(28)=xs(28)+1
	
			case month(FECHA)=5
					xs(29)=xs(29)+1
	
			case month(FECHA)=6
					xs(30)=xs(30)+1
	
			case month(FECHA)=7
					xs(31)=xs(31)+1
	
			case month(FECHA)=8
					xs(32)=xs(32)+1
	
			case month(FECHA)=9
					xs(33)=xs(33)+1
	
			case month(FECHA)=10 
					xs(34)=xs(34)+1
	
			Case month(FECHA)=11
					xs(35)=xs(35)+1
	
			Case month(FECHA)=12
					xs(36)=xs(36)+1
	
		Endcase

ENDCASE		

Do Case
			Case month(FECHA)=1 
					xs(37)=xs(37)+1
					TS(1)=xs(37)/TV*100
											
			case month(FECHA)=2 
					xs(38)=xs(38)+1
					TS(2)=xs(38)/TV*100
					
			case month(FECHA)=3
					xs(39)=xs(39)+1
					TS(3)=xs(39)/TV*100
	
			case month(FECHA)=4
					xs(40)=xs(40)+1
					TS(4)=xs(40)/TV*100
	
			case month(FECHA)=5
					xs(41)=xs(41)+1
					TS(5)=xs(41)/TV*100

			case month(FECHA)=6
					xs(42)=xs(42)+1
					TS(6)=xs(42)/TV*100
	
			case month(FECHA)=7
					xs(43)=xs(43)+1
					TS(7)=xs(43)/TV*100
	
			case month(FECHA)=8
					xs(44)=xs(44)+1
					TS(8)=xs(44)/TV*100
	
			case month(FECHA)=9 
					xs(45)=xs(45)+1
					TS(9)=xs(45)/TV*100
	
			Case month(FECHA)=10
					xs(46)=xs(46)+1
					TS(10)=xs(46)/TV*100
	
			Case month(FECHA)=11
					xs(47)=xs(47)+1
					TS(11)=xs(47)/TV*100
	
			Case month(FECHA)=12
					xs(48)=xs(48)+1
					TS(12)=xs(48)/TV*100

		Endcase
		* Total de Partos
		* ---------------
		xs(50)=xs(50)+1

ENDSCAN

xMAXIMA=XS(50)

Create Cursor REPORTE (Concepto c(20),COL1 c(5),COL2 c(5),COL3 c(5),COL4 c(5),COL5 c(5))
Append Blank
Replace CONCEPTO With 'Mes'
Append Blank
Replace CONCEPTO With 'Enero'     ,COL1 With Str(XS(1),3),COL2 With Str(XS(13),3),COL3 With Str(XS(25),3),COL4 With Str(XS(37),3),COL5 With Str(TS(1),5,1)
Append Blank
Replace CONCEPTO With 'Febrero'   ,COL1 With Str(XS(2),3),COL2 With Str(XS(14),3),COL3 With Str(XS(26),3),COL4 With Str(XS(38),3),COL5 With Str(TS(2),5,1)
Append Blank
Replace CONCEPTO With 'Marzo'     ,COL1 With Str(XS(3),3),COL2 With Str(XS(15),3),COL3 With Str(XS(27),3),COL4 With Str(XS(39),3),COL5 With Str(TS(3),5,1)
Append Blank
Replace CONCEPTO With 'Abril'     ,COL1 With Str(XS(4),3),COL2 With Str(XS(16),3),COL3 With Str(XS(28),3),COL4 With Str(XS(40),3),COL5 With Str(TS(4),5,1)
Append Blank
Replace CONCEPTO With 'Mayo'      ,COL1 With Str(XS(5),3),COL2 With Str(XS(17),3),COL3 With Str(XS(29),3),COL4 With Str(XS(41),3),COL5 With Str(TS(5),5,1)
Append Blank
Replace CONCEPTO With 'Junio'     ,COL1 With Str(XS(6),3),COL2 With Str(XS(18),3),COL3 With Str(XS(30),3),COL4 With Str(XS(42),3),COL5 With Str(TS(6),5,1)
Append Blank
Replace CONCEPTO With 'Julio'     ,COL1 With Str(XS(7),3),COL2 With Str(XS(19),3),COL3 With Str(XS(31),3),COL4 With Str(XS(43),3),COL5 With Str(TS(7),5,1)
Append Blank
Replace CONCEPTO With 'Agosto'    ,COL1 With Str(XS(8),3),COL2 With Str(XS(20),3),COL3 With Str(XS(32),3),COL4 With Str(XS(44),3),COL5 With Str(TS(8),5,1)
Append Blank
Replace CONCEPTO With 'Septiembre',COL1 With Str(XS(9),3),COL2 With Str(XS(21),3),COL3 With Str(XS(33),3),COL4 With Str(XS(45),3),COL5 With Str(TS(9),5,1)
Append Blank
Replace CONCEPTO With 'Octubre'   ,COL1 With Str(XS(10),3),COL2 With Str(XS(22),3),COL3 With Str(XS(34),3),COL4 With Str(XS(46),3),COL5 With Str(TS(10),5,1)
Append Blank
Replace CONCEPTO With 'Noviembre' ,COL1 With Str(XS(11),3),COL2 With Str(XS(23),3),COL3 With Str(XS(35),3),COL4 With Str(XS(47),3),COL5 With Str(TS(11),5,1)
Append Blank
Replace CONCEPTO With 'Diciembre' ,COL1 With Str(XS(12),3),COL2 With Str(XS(24),3),COL3 With Str(XS(36),3),COL4 With Str(XS(48),3),COL5 With Str(TS(12),5,1)

Append Blank
Replace CONCEPTO With ''
Append Blank
Replace CONCEPTO With 'Total     ',COL1 With Str(XS(51),4),COL2 With Str(XS(52),4),COL3 With Str(XS(53),4),COL4 With Str(XS(50),4),COL5 With Str((XS(50)/TV)*100,6,2)

GO TOP
RETURN


* ANALISIS DE DIAGNOSTICO DE PREÑEZ
* -------------------------------
PROCEDURE RV1063
PARAMETERS xP1,xP2,xSP1,xFILTER

CREATE TABLE DATOS (ID n(5),NP n(2),FECHA d(8),STAT c(5),DGES n(3),SER1 n(2),FECHA2 d(8),STAT2 c(5),DIAS n(3),SER2 n(2))

SELECT ID,NP,FECHA,STAT,DGES,SER FROM CALOR WHERE NP>0 AND STAT="CARGA" AND DGES>=xP1 AND DGES<=xP2 AND &xFILTER ORDER BY ID INTO TABLE DATOS35
xTOTAL=RECCOUNT()

SELECT DATOS35
nVACAS=0

SCAN 
xID=DATOS35.ID

SELECT CALOR
SEEK xID

	SCAN WHILE CALOR.ID=DATOS35.ID AND CLAVE#9
		IF CALOR.NP=DATOS35.NP AND CALOR.FECHA>=DATOS35.FECHA AND CALOR.FECHA<=DATOS35.FECHA+xSP1 AND STAT="INSEM"
			 nVACAS=nVACAS+1
			
			SELECT DATOS
			APPEND BLANK
			REPLACE ID WITH DATOS35.ID,NP WITH DATOS35.NP,FECHA WITH DATOS35.FECHA,STAT WITH DATOS35.STAT;
					DGES WITH DATOS35.DGES,SER1 WITH DATOS35.SER,FECHA2 WITH CALOR.FECHA,STAT2 WITH CALOR.STAT;
					DIAS WITH CALOR.FECHA-DATOS35.FECHA,SER2 WITH CALOR.SER 
		EXIT
		ENDIF
	ENDSCAN
	SELECT DATOS35
ENDSCAN
RETURN




