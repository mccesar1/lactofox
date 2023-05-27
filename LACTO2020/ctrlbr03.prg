* PESO Y ESTATURA POR RANGO DE EDADES
* -----------------------------------
PROCEDURE RV10176
Dimension TG(25),TG1(25),TG2(25),AW(25),AV(25),AG(25),XP(25),XE(25),XG(25)
x=0
TG=0
TG1=0
TG2=0

AW=0
AV=0
AG=0

XP=0
XE=0
XG=0

Scan
xEdad=(FECHA-FNAC)/30.4

	Do Case
		Case xEDAD=0
			x=1
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	
	
		Case xEDAD>=0  And xEDAD<=1
			x=2
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	
	

		Case xEDAD>1  And xEDAD<=2
			x=3
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	

		Case xEDAD>2  And xEDAD<=3
			x=4
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	

		Case xEDAD>3  And xEDAD<=4
			x=5
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	

		Case xEDAD>4  And xEDAD<=5
			x=6
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	

		Case xEDAD>5  And xEDAD<=6
			x=7
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	

		Case xEDAD>6  And xEDAD<=7
			x=8
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	

		Case xEDAD>7  And xEDAD<=8
			x=9
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	

		Case xEDAD>8  And xEDAD<=9
			x=10
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	

		Case xEDAD>9  And xEDAD<=10
			x=11
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	

		Case xEDAD>10  And xEDAD<=11
			x=12
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	

		Case xEDAD>11  And xEDAD<=12
			x=13
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	

		Case xEDAD>12  And xEDAD<=13
			x=14
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	

		Case xEDAD>13  And xEDAD<=14
			x=15
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	

		Case xEDAD>14  And xEDAD<=15
			x=16
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	

		Case xEDAD>15 And xEDAD<=16
			x=17
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	

		Case xEDAD>16 And xEDAD<=17
			x=18
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	

		Case xEDAD>17 And xEDAD<=18
			x=19
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	

		Case xEDAD>18 And xEDAD<=19
			x=20
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	

		Case xEDAD>19 And xEDAD<=20
			x=21
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	

		Case xEDAD>20 And xEDAD<=21
			x=22
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	

		Case xEDAD>21  And xEDAD<=22
			x=23
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	

		Case xEDAD>22  And xEDAD<=23
			x=24
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	

		Case xEDAD>23  And xEDAD<=24
			x=25
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	
	EndCase

EndScan

Create Cursor REPORTE (Concepto c(3),COL1 c(5),COL2 c(5),COL3 c(5),COL4 c(5))
Append Blank
Replace CONCEPTO With ' 0', COL1 With Str(TG(1),4),COL2 With Str(XP(1),4,1),COL3 With Str(XE(1),3),COL4 With Str(XG(1),5,3)
Append Blank
Replace CONCEPTO With ' 1',COL1 With Str(TG(2),4),COL2 With Str(XP(2),4,1),COL3 With Str(XE(2),3),COL4 With Str(XG(2),5,3)
Append Blank
Replace CONCEPTO With ' 2',COL1 With Str(TG(3),4),COL2 With Str(XP(3),4,1),COL3 With Str(XE(3),3),COL4 With Str(XG(3),5,3)
Append Blank
Replace CONCEPTO With ' 3',COL1 With Str(TG(4),4),COL2 With Str(XP(4),4,1),COL3 With Str(XE(4),3),COL4 With Str(XG(4),5,3)
Append Blank
Replace CONCEPTO With ' 4',COL1 With Str(TG(5),4),COL2 With Str(XP(5),4,1),COL3 With Str(XE(5),3),COL4 With Str(XG(5),5,3)
Append Blank
Replace CONCEPTO With ' 5',COL1 With Str(TG(6),4),COL2 With Str(XP(6),4,1),COL3 With Str(XE(6),3),COL4 With Str(XG(6),5,3)
Append Blank
Replace CONCEPTO With ' 6',COL1 With Str(TG(7),4),COL2 With Str(XP(7),4,1),COL3 With Str(XE(7),3),COL4 With Str(XG(7),5,3)
Append Blank
Replace CONCEPTO With ' 7',COL1 With Str(TG(8),4),COL2 With Str(XP(8),4,1),COL3 With Str(XE(8),3),COL4 With Str(XG(8),5,3)
Append Blank
Replace CONCEPTO With ' 8',COL1 With Str(TG(9),4),COL2 With Str(XP(9),4,1),COL3 With Str(XE(9),3),COL4 With Str(XG(9),5,3)
Append Blank
Replace CONCEPTO With ' 9',COL1 With Str(TG(10),4),COL2 With Str(XP(10),4,1),COL3 With Str(XE(10),3),COL4 With Str(XG(10),5,3)
Append Blank
Replace CONCEPTO With '10',COL1 With Str(TG(11),4),COL2 With Str(XP(11),4,1),COL3 With Str(XE(11),3),COL4 With Str(XG(11),5,3)
Append Blank
Replace CONCEPTO With '11',COL1 With Str(TG(12),4),COL2 With Str(XP(12),4,1),COL3 With Str(XE(12),3),COL4 With Str(XG(12),5,3)
Append Blank
Replace CONCEPTO With '12',COL1 With Str(TG(13),4),COL2 With Str(XP(13),4,1),COL3 With Str(XE(13),3),COL4 With Str(XG(13),5,3)
Append Blank
Replace CONCEPTO With '13',COL1 With Str(TG(14),4),COL2 With Str(XP(14),4,1),COL3 With Str(XE(14),3),COL4 With Str(XG(14),5,3)
Append Blank
Replace CONCEPTO With '14',COL1 With Str(TG(15),4),COL2 With Str(XP(15),4,1),COL3 With Str(XE(15),3),COL4 With Str(XG(15),5,3)
Append Blank
Replace CONCEPTO With '15',COL1 With Str(TG(16),4),COL2 With Str(XP(16),4,1),COL3 With Str(XE(16),3),COL4 With Str(XG(16),5,3)
Append Blank
Replace CONCEPTO With '16',COL1 With Str(TG(17),4),COL2 With Str(XP(17),4,1),COL3 With Str(XE(17),3),COL4 With Str(XG(17),5,3)
Append Blank
Replace CONCEPTO With '17',COL1 With Str(TG(18),4),COL2 With Str(XP(18),4,1),COL3 With Str(XE(18),3),COL4 With Str(XG(18),5,3)
Append Blank
Replace CONCEPTO With '18',COL1 With Str(TG(19),4),COL2 With Str(XP(19),4,1),COL3 With Str(XE(19),3),COL4 With Str(XG(19),5,3)
Append Blank
Replace CONCEPTO With '19',COL1 With Str(TG(20),4),COL2 With Str(XP(20),4,1),COL3 With Str(XE(20),3),COL4 With Str(XG(20),5,3)
Append Blank
Replace CONCEPTO With '20',COL1 With Str(TG(21),4),COL2 With Str(XP(21),4,1),COL3 With Str(XE(21),3),COL4 With Str(XG(21),5,3)
Append Blank
Replace CONCEPTO With '21',COL1 With Str(TG(22),4),COL2 With Str(XP(22),4,1),COL3 With Str(XE(22),3),COL4 With Str(XG(22),5,3)
Append Blank
Replace CONCEPTO With '22',COL1 With Str(TG(23),4),COL2 With Str(XP(23),4,1),COL3 With Str(XE(23),3),COL4 With Str(XG(23),5,3)
Append Blank
Replace CONCEPTO With '23',COL1 With Str(TG(24),4),COL2 With Str(XP(24),4,1),COL3 With Str(XE(24),3),COL4 With Str(XG(24),5,3)
Append Blank
Replace CONCEPTO With '24',COL1 With Str(TG(25),4),COL2 With Str(XP(25),4,1),COL3 With Str(XE(25),3),COL4 With Str(XG(25),5,3)
GO TOP
RETURN


* PESO Y ESTATURA POR RANGO DE EDADES (MACHOS)
* --------------------------------------------
PROCEDURE RV10186
Dimension TG(25),TG1(25),TG2(25),AW(25),AV(25),AG(25),XP(25),XE(25),XG(25)
x=0
TG=0
TG1=0
TG2=0

AW=0
AV=0
AG=0

XP=0
XE=0
XG=0

Scan
xEdad=(FECHA-FNAC)/30.4

	Do Case
		Case xEDAD=0
			x=1
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	
	
		Case xEDAD>=0  And xEDAD<=1
			x=2
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	
	

		Case xEDAD>1  And xEDAD<=2
			x=3
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	

		Case xEDAD>2  And xEDAD<=3
			x=4
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	

		Case xEDAD>3  And xEDAD<=4
			x=5
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	

		Case xEDAD>4  And xEDAD<=5
			x=6
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	

		Case xEDAD>5  And xEDAD<=6
			x=7
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	

		Case xEDAD>6  And xEDAD<=7
			x=8
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	

		Case xEDAD>7  And xEDAD<=8
			x=9
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	

		Case xEDAD>8  And xEDAD<=9
			x=10
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	

		Case xEDAD>9  And xEDAD<=10
			x=11
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	

		Case xEDAD>10  And xEDAD<=11
			x=12
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	

		Case xEDAD>11  And xEDAD<=12
			x=13
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	

		Case xEDAD>12  And xEDAD<=13
			x=14
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	

		Case xEDAD>13  And xEDAD<=14
			x=15
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	

		Case xEDAD>14  And xEDAD<=15
			x=16
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	

		Case xEDAD>15 And xEDAD<=16
			x=17
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	

		Case xEDAD>16 And xEDAD<=17
			x=18
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	

		Case xEDAD>17 And xEDAD<=18
			x=19
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	

		Case xEDAD>18 And xEDAD<=19
			x=20
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	

		Case xEDAD>19 And xEDAD<=20
			x=21
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	

		Case xEDAD>20 And xEDAD<=21
			x=22
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	

		Case xEDAD>21  And xEDAD<=22
			x=23
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	

		Case xEDAD>22  And xEDAD<=23
			x=24
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	

		Case xEDAD>23  And xEDAD<=24
			x=25
			TG(x)=TG(x)+1
			AW(x)=AW(x)+PESO
			XP(x)=AW(x)/TG(x)

			If GANP>0
				TG1(x)=TG1(x)+1
				AG(x)=AG(x)+GANP			
				XG(x)=AG(x)/TG1(x)											
			EndIF

			If ESTA>0
				TG2(x)=TG2(x)+1
				AV(x)=AV(x)+ESTA				
				XE(x)=AV(x)/TG2(x)
			EndIf	
	EndCase

EndScan

Create Cursor REPORTE (Concepto c(3),COL1 c(5),COL2 c(5),COL3 c(5),COL4 c(5))
Append Blank
Replace CONCEPTO With ' 0', COL1 With Str(TG(1),4),COL2 With Str(XP(1),4),COL3 With Str(XE(1),3),COL4 With Str(XG(1),5,3)
Append Blank
Replace CONCEPTO With ' 1',COL1 With Str(TG(2),4),COL2 With Str(XP(2),4),COL3 With Str(XE(2),3),COL4 With Str(XG(2),5,3)
Append Blank
Replace CONCEPTO With ' 2',COL1 With Str(TG(3),4),COL2 With Str(XP(3),4),COL3 With Str(XE(3),3),COL4 With Str(XG(3),5,3)
Append Blank
Replace CONCEPTO With ' 3',COL1 With Str(TG(4),4),COL2 With Str(XP(4),4),COL3 With Str(XE(4),3),COL4 With Str(XG(4),5,3)
Append Blank
Replace CONCEPTO With ' 4',COL1 With Str(TG(5),4),COL2 With Str(XP(5),4),COL3 With Str(XE(5),3),COL4 With Str(XG(5),5,3)
Append Blank
Replace CONCEPTO With ' 5',COL1 With Str(TG(6),4),COL2 With Str(XP(6),4),COL3 With Str(XE(6),3),COL4 With Str(XG(6),5,3)
Append Blank
Replace CONCEPTO With ' 6',COL1 With Str(TG(7),4),COL2 With Str(XP(7),4),COL3 With Str(XE(7),3),COL4 With Str(XG(7),5,3)
Append Blank
Replace CONCEPTO With ' 7',COL1 With Str(TG(8),4),COL2 With Str(XP(8),4),COL3 With Str(XE(8),3),COL4 With Str(XG(8),5,3)
Append Blank
Replace CONCEPTO With ' 8',COL1 With Str(TG(9),4),COL2 With Str(XP(9),4),COL3 With Str(XE(9),3),COL4 With Str(XG(9),5,3)
Append Blank
Replace CONCEPTO With ' 9',COL1 With Str(TG(10),4),COL2 With Str(XP(10),4),COL3 With Str(XE(10),3),COL4 With Str(XG(10),5,3)
Append Blank
Replace CONCEPTO With '10',COL1 With Str(TG(11),4),COL2 With Str(XP(11),4),COL3 With Str(XE(11),3),COL4 With Str(XG(11),5,3)
Append Blank
Replace CONCEPTO With '11',COL1 With Str(TG(12),4),COL2 With Str(XP(12),4),COL3 With Str(XE(12),3),COL4 With Str(XG(12),5,3)
Append Blank
Replace CONCEPTO With '12',COL1 With Str(TG(13),4),COL2 With Str(XP(13),4),COL3 With Str(XE(13),3),COL4 With Str(XG(13),5,3)
Append Blank
Replace CONCEPTO With '13',COL1 With Str(TG(14),4),COL2 With Str(XP(14),4),COL3 With Str(XE(14),3),COL4 With Str(XG(14),5,3)
Append Blank
Replace CONCEPTO With '14',COL1 With Str(TG(15),4),COL2 With Str(XP(15),4),COL3 With Str(XE(15),3),COL4 With Str(XG(15),5,3)
Append Blank
Replace CONCEPTO With '15',COL1 With Str(TG(16),4),COL2 With Str(XP(16),4),COL3 With Str(XE(16),3),COL4 With Str(XG(16),5,3)
Append Blank
Replace CONCEPTO With '16',COL1 With Str(TG(17),4),COL2 With Str(XP(17),4),COL3 With Str(XE(17),3),COL4 With Str(XG(17),5,3)
Append Blank
Replace CONCEPTO With '17',COL1 With Str(TG(18),4),COL2 With Str(XP(18),4),COL3 With Str(XE(18),3),COL4 With Str(XG(18),5,3)
Append Blank
Replace CONCEPTO With '18',COL1 With Str(TG(19),4),COL2 With Str(XP(19),4),COL3 With Str(XE(19),3),COL4 With Str(XG(19),5,3)
Append Blank
Replace CONCEPTO With '19',COL1 With Str(TG(20),4),COL2 With Str(XP(20),4),COL3 With Str(XE(20),3),COL4 With Str(XG(20),5,3)
Append Blank
Replace CONCEPTO With '20',COL1 With Str(TG(21),4),COL2 With Str(XP(21),4),COL3 With Str(XE(21),3),COL4 With Str(XG(21),5,3)
Append Blank
Replace CONCEPTO With '21',COL1 With Str(TG(22),4),COL2 With Str(XP(22),4),COL3 With Str(XE(22),3),COL4 With Str(XG(22),5,3)
Append Blank
Replace CONCEPTO With '22',COL1 With Str(TG(23),4),COL2 With Str(XP(23),4),COL3 With Str(XE(23),3),COL4 With Str(XG(23),5,3)
Append Blank
Replace CONCEPTO With '23',COL1 With Str(TG(24),4),COL2 With Str(XP(24),4),COL3 With Str(XE(24),3),COL4 With Str(XG(24),5,3)
Append Blank
Replace CONCEPTO With '24',COL1 With Str(TG(25),4),COL2 With Str(XP(25),4),COL3 With Str(XE(25),3),COL4 With Str(XG(25),5,3)
GO TOP
RETURN

* ANALISIS ANUAL DE ABORTOS
* -------------------------
PROCEDURE RV10214
*----------------
DIMENSION XS(60),TI(60),MES(12)
xs=0
TG=0
TI=0
mes=1

Select REG 
Set Order To 3
Set Filter To
Calculate CNT() For NP=0 And FB2=B TO TV

Select ABORTOS
Set Filter To NP=0 And Year(FECHA)=xan
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


* Total de Abortos
* ---------------
xs(50)=xs(50)+1

Do Case
			Case month(FECHA)=1 
					xs(37)=xs(37)+1
					TI(1)=xs(37)/TV*100
											
			case month(FECHA)=2 
					xs(38)=xs(38)+1
					TI(2)=xs(38)/TV*100
					
			case month(FECHA)=3
					xs(39)=xs(39)+1
					TI(3)=xs(39)/TV*100
	
			case month(FECHA)=4
					xs(40)=xs(40)+1
					TI(4)=xs(40)/TV*100
	
			case month(FECHA)=5
					xs(41)=xs(41)+1
					TI(5)=xs(41)/TV*100

			case month(FECHA)=6
					xs(42)=xs(42)+1
					TI(6)=xs(42)/TV*100
	
			case month(FECHA)=7
					xs(43)=xs(43)+1
					TI(7)=xs(43)/TV*100
	
			case month(FECHA)=8
					xs(44)=xs(44)+1
					TI(8)=xs(44)/TV*100
	
			case month(FECHA)=9 
					xs(45)=xs(45)+1
					TI(9)=xs(45)/TV*100
	
			Case month(FECHA)=10
					xs(46)=xs(46)+1
					TI(10)=xs(46)/TV*100
	
			Case month(FECHA)=11
					xs(47)=xs(47)+1
					TI(11)=xs(47)/TV*100
	
			Case month(FECHA)=12
					xs(48)=xs(48)+1
					TI(12)=xs(48)/TV*100

		Endcase
ENDSCAN
Set Filter To

xMAXIMA=XS(50)

Create Cursor REPORTE (Concepto c(20),COL1 c(5),COL2 c(5),COL3 c(5),COL4 c(5),COL5 c(5))
Append Blank
Replace CONCEPTO With 'Mes'
Append Blank
Replace CONCEPTO With 'Enero'     ,COL1 With Str(XS(1),3),COL2 With Str(XS(13),3),COL3 With Str(XS(25),3),COL4 With Str(XS(37),3),COL5 With Str(TI(1),5,1)
Append Blank
Replace CONCEPTO With 'Febrero'   ,COL1 With Str(XS(2),3),COL2 With Str(XS(14),3),COL3 With Str(XS(26),3),COL4 With Str(XS(38),3),COL5 With Str(TI(2),5,1)
Append Blank
Replace CONCEPTO With 'Marzo'     ,COL1 With Str(XS(3),3),COL2 With Str(XS(15),3),COL3 With Str(XS(27),3),COL4 With Str(XS(39),3),COL5 With Str(TI(3),5,1)
Append Blank
Replace CONCEPTO With 'Abril'     ,COL1 With Str(XS(4),3),COL2 With Str(XS(16),3),COL3 With Str(XS(28),3),COL4 With Str(XS(40),3),COL5 With Str(TI(4),5,1)
Append Blank
Replace CONCEPTO With 'Mayo'      ,COL1 With Str(XS(5),3),COL2 With Str(XS(17),3),COL3 With Str(XS(29),3),COL4 With Str(XS(41),3),COL5 With Str(TI(5),5,1)
Append Blank
Replace CONCEPTO With 'Junio'     ,COL1 With Str(XS(6),3),COL2 With Str(XS(18),3),COL3 With Str(XS(30),3),COL4 With Str(XS(42),3),COL5 With Str(TI(6),5,1)
Append Blank
Replace CONCEPTO With 'Julio'     ,COL1 With Str(XS(7),3),COL2 With Str(XS(19),3),COL3 With Str(XS(31),3),COL4 With Str(XS(43),3),COL5 With Str(TI(7),5,1)
Append Blank
Replace CONCEPTO With 'Agosto'    ,COL1 With Str(XS(8),3),COL2 With Str(XS(20),3),COL3 With Str(XS(32),3),COL4 With Str(XS(44),3),COL5 With Str(TI(8),5,1)
Append Blank
Replace CONCEPTO With 'Septiembre',COL1 With Str(XS(9),3),COL2 With Str(XS(21),3),COL3 With Str(XS(33),3),COL4 With Str(XS(45),3),COL5 With Str(TI(9),5,1)
Append Blank
Replace CONCEPTO With 'Octubre'   ,COL1 With Str(XS(10),3),COL2 With Str(XS(22),3),COL3 With Str(XS(34),3),COL4 With Str(XS(46),3),COL5 With Str(TI(10),5,1)
Append Blank
Replace CONCEPTO With 'Noviembre' ,COL1 With Str(XS(11),3),COL2 With Str(XS(23),3),COL3 With Str(XS(35),3),COL4 With Str(XS(47),3),COL5 With Str(TI(11),5,1)
Append Blank
Replace CONCEPTO With 'Diciembre' ,COL1 With Str(XS(12),3),COL2 With Str(XS(24),3),COL3 With Str(XS(36),3),COL4 With Str(XS(48),3),COL5 With Str(TI(12),5,1)

Append Blank
Replace CONCEPTO With ''
Append Blank
Replace CONCEPTO With 'Total     ',COL1 With Str(XS(51),4),COL2 With Str(XS(52),4),COL3 With Str(XS(53),4),COL4 With Str(XS(50),4),COL5 With Str((XS(50)/TV)*100,4,1)
GO TOP
RETURN


* ANALISIS ANUAL POR EDAD DE BAJAS
* --------------------------------
PROCEDURE RV10189
Declare xb(40),xt(15),xp(5)
xb=0
xp=0
xt=0

Select BAJAS
Set Filter To ID>0 And FECHA#B And MOT#"PARTO" AND &xFilter 

Go Top

SCAN For FECHA#B
* Para calcular el total por mes de todas las etapas
* --------------------------------------------------
	xp(5)=xp(5)+1
	do case
	case month(fecha)=1
		xt(1)=xt(1)+1
	case month(fecha)=2
		xt(2)=xt(2)+1
	case month(fecha)=3
		xt(3)=xt(3)+1
	case month(fecha)=4
		xt(4)=xt(4)+1
	case month(fecha)=5
		xt(5)=xt(5)+1
	case month(fecha)=6
		xt(6)=xt(6)+1
	case month(fecha)=7
		xt(7)=xt(7)+1
	case month(fecha)=8
		xt(8)=xt(8)+1
	case month(fecha)=9
		xt(9)=xt(9)+1
	case month(fecha)=10
		xt(10)=xt(10)+1
	case month(fecha)=11
		xt(11)=xt(11)+1
	case month(fecha)=12
		xt(12)=xt(12)+1
Endcase
* --------------------------------------------------

* Para calcular el total por etapa y por mes
* --------------------------------------------------
do case
	case (fecha-nac)>=0 .and. (fecha-nac)<=180
	xp(1)=xp(1)+1
*
	do case
	case month(fecha)=1
		xb(1)=xb(1)+1
	case month(fecha)=2
		xb(2)=xb(2)+1
	case month(fecha)=3
		xb(3)=xb(3)+1
	case month(fecha)=4
		xb(4)=xb(4)+1
	case month(fecha)=5
		xb(5)=xb(5)+1
	case month(fecha)=6
		xb(6)=xb(6)+1
	case month(fecha)=7
		xb(7)=xb(7)+1
	case month(fecha)=8
		xb(8)=xb(8)+1
	case month(fecha)=9
		xb(9)=xb(9)+1
	case month(fecha)=10
		xb(10)=xb(10)+1
	case month(fecha)=11
		xb(11)=xb(11)+1
	case month(fecha)=12
		xb(12)=xb(12)+1
endcase
*		
	case (fecha-nac)>180 .and. (fecha-nac)<=365
	xp(2)=xp(2)+1
*
	do case
	case month(fecha)=1
		xb(13)=xb(13)+1
	case month(fecha)=2
		xb(14)=xb(14)+1
	case month(fecha)=3
		xb(15)=xb(15)+1
	case month(fecha)=4
		xb(16)=xb(16)+1
	case month(fecha)=5
		xb(17)=xb(17)+1
	case month(fecha)=6
		xb(18)=xb(18)+1
	case month(fecha)=7
		xb(19)=xb(19)+1
	case month(fecha)=8
		xb(20)=xb(20)+1
	case month(fecha)=9
		xb(21)=xb(21)+1
	case month(fecha)=10
		xb(22)=xb(22)+1
	case month(fecha)=11
		xb(23)=xb(23)+1
	case month(fecha)=12
		xb(24)=xb(24)+1
endcase
*		
	case (fecha-nac)>365
	xp(3)=xp(3)+1
*
	do case
	case month(fecha)=1
		xb(25)=xb(25)+1
	case month(fecha)=2
		xb(26)=xb(26)+1
	case month(fecha)=3
		xb(27)=xb(27)+1
	case month(fecha)=4
		xb(28)=xb(28)+1
	case month(fecha)=5
		xb(29)=xb(29)+1
	case month(fecha)=6
		xb(30)=xb(30)+1
	case month(fecha)=7
		xb(31)=xb(31)+1
	case month(fecha)=8
		xb(32)=xb(32)+1
	case month(fecha)=9
		xb(33)=xb(33)+1
	case month(fecha)=10
		xb(34)=xb(34)+1
	case month(fecha)=11
		xb(35)=xb(35)+1
	case month(fecha)=12
		xb(36)=xb(36)+1
endcase
*		
Endcase

ENDSCAN
Set Filter To

xtotal=xt(1)+xt(2)+xt(3)+xt(4)+xt(5)+xt(6)+xt(7)+xt(8)+xt(9)+xt(10)+xt(11)+xt(12)
xMAXIMA=xTOTAL

Create Cursor REPORTE (Concepto c(20),COL1 c(4),COL2 c(4),COL3 c(4),COL4 c(4))
Append Blank
Replace CONCEPTO With 'Mes'
Append Blank
Replace CONCEPTO With 'Enero'     ,COL1 With Str(XB(1),3),COL2 With Str(XB(13),3),COL3 With Str(XB(25),3),COL4 With Str(XT(1),3)
Append Blank
Replace CONCEPTO With 'Febrero'   ,COL1 With Str(XB(2),3),COL2 With Str(XB(14),3),COL3 With Str(XB(26),3),COL4 With Str(XT(2),3)
Append Blank
Replace CONCEPTO With 'Marzo'     ,COL1 With Str(XB(3),3),COL2 With Str(XB(15),3),COL3 With Str(XB(27),3),COL4 With Str(XT(3),3)
Append Blank
Replace CONCEPTO With 'Abril'     ,COL1 With Str(XB(4),3),COL2 With Str(XB(16),3),COL3 With Str(XB(28),3),COL4 With Str(XT(4),3)
Append Blank
Replace CONCEPTO With 'Mayo'      ,COL1 With Str(XB(5),3),COL2 With Str(XB(17),3),COL3 With Str(XB(29),3),COL4 With Str(XT(5),3)
Append Blank
Replace CONCEPTO With 'Junio'     ,COL1 With Str(XB(6),3),COL2 With Str(XB(18),3),COL3 With Str(XB(30),3),COL4 With Str(XT(6),3)
Append Blank
Replace CONCEPTO With 'Julio'     ,COL1 With Str(XB(7),3),COL2 With Str(XB(19),3),COL3 With Str(XB(31),3),COL4 With Str(XT(7),3)
Append Blank
Replace CONCEPTO With 'Agosto'    ,COL1 With Str(XB(8),3),COL2 With Str(XB(20),3),COL3 With Str(XB(32),3),COL4 With Str(XT(8),3)
Append Blank
Replace CONCEPTO With 'Septiembre',COL1 With Str(XB(9),3),COL2 With Str(XB(21),3),COL3 With Str(XB(33),3),COL4 With Str(XT(9),3)
Append Blank
Replace CONCEPTO With 'Octubre'   ,COL1 With Str(XB(10),3),COL2 With Str(XB(22),3),COL3 With Str(XB(34),3),COL4 With Str(XT(10),3)
Append Blank
Replace CONCEPTO With 'Noviembre' ,COL1 With Str(XB(11),3),COL2 With Str(XB(23),3),COL3 With Str(XB(35),3),COL4 With Str(XT(11),3)
Append Blank
Replace CONCEPTO With 'Diciembre' ,COL1 With Str(XB(12),3),COL2 With Str(XB(24),3),COL3 With Str(XB(36),3),COL4 With Str(XT(12),3)

Append Blank
Replace CONCEPTO With ''
Append Blank
Replace CONCEPTO With 'Total     ',COL1 With Str(XP(1),4),COL2 With Str(XP(2),4),COL3 With Str(XP(3),4),COL4 With Str(XP(5),4)
GO TOP
RETURN


