* PRODUCCION POR CORRAL    
* ---------------------
PROC RV1051
dat=0
xTOTAL=0
xLECHE=0
xDLECH=0

Create Cursor REPORTE (Concepto c(5),COL1 c(5),COL2 c(5),COL3 c(5),COL4 c(5),COL5 c(5),COL6 c(5),COL7 c(4),COL8 c(4),;
			  COL9 c(4),COL10 c(4),COL11 c(4),COL12 c(5),COL13 c(4),COL14 c(5),COL15 c(4),COL16 c(5))

Select REG
Set Order To 2
Set Filter To FB2=B 

Select CORRAL
Set Order To 1
GO TOP

DO WHILE .NOT. EOF()
	dat=dat+1
	NC=CNUM

	Select REG
	SET FILTER TO FPAR#B AND NP>0 AND FSEC=B AND FB2=B &&AND VAL(ALLTRIM(IDE))>0
	COUNT FOR CORR=NC TO TV
	If TV>0
		CALCULATE AVG(PRM),MIN(PRM),MAX(PRM),STD(PRM) FOR CORR=NC AND PRM>0 TO UM1,UO1,UP1,UQ1
		CALCULATE CNT(),AVG(DATE()-FPAR) FOR CORR=NC and FSEC=B and FPAR#B and FB2=B TO UN1,UR1
		CALCULATE AVG(DAB) FOR CORR=NC AND DAB>0 TO DAB1
		CALCULATE CNT() FOR STAT=[INSEM] AND CORR=NC TO I1
		CALCULATE CNT() FOR STAT=[CARGA] AND CORR=NC TO C1
		CALCULATE CNT() FOR (STAT#[CARGA] AND STAT#[INSEM]) AND CORR=NC TO AB1
		CALCULATE AVG(PLAC),AVG(DPIC) FOR PLAC>0 AND CORR=NC TO P1,D1
		CALCULATE AVG(NS) FOR CORR=NC AND NS>0 TO NS1 
		XINSEM=(I1/TV)*100
		XCARGA=(C1/TV)*100
		XABIER=(AB1/TV)*100
		
			
	EndIf

	IF TV>0
		Select REPORTE
		Append Blank
		Replace CONCEPTO With Str(NC,4),COL1 With Str(TV,4),COL2 With Str(UM1,4,1),COL3 With Str(UO1,4,1),;
						COL4 With Str(UP1,4,1),COL5 With Str(UQ1,4,1),COL6 With Str(UR1,4),COL7 WITH STR(DAB1,3),;
						COL8 WITH STR(P1,4,1),COL9 WITH STR(D1,3),COL10 WITH STR(NS1,4,2),COL11 WITH STR(I1,3),;
						COL12 WITH STR(XINSEM,5,1),COL13 WITH STR(C1,3),COL14 WITH STR(XCARGA,5,1),COL15 WITH STR(AB1,3),;
						COL16 WITH STR(XABIER,5,1)
	Else
	EndIf

	Select CORRAL
	SKIP
	If EOF()
		Exit
	Endif

	If TIPO#'P'
		Skip
	Endif
ENDDO 
Select REPORTE
GO TOP
SET FILTER TO 
RETURN


* PRODUCCION POR CORRAL    
* ---------------------
PROC RV1051X
dat=0
Create Cursor REPORTE (Concepto c(5),COL1 c(5),COL2 c(5),COL3 c(5),COL4 c(5),COL5 c(5),COL6 c(5))

Select REG
Set Order To 2
Set Filter To FB2=B

Select CORRAL
Set Order To 1
GO TOP

DO WHILE .NOT. EOF()
	dat=dat+1
	NC=CNUM

	Select REG
	COUNT FOR CORR=NC and FPAR#B and FSEC=B and FB2=B TO TV
	If TV>0
		CALCULATE AVG(PRM),MIN(PRM),MAX(PRM),STD(PRM) FOR CORR=NC and FSEC=B and FPAR#B and FB2=B and prm>0 TO UM1,UO1,UP1,UQ1
		CALCULATE CNT(),AVG(DATE()-FPAR) FOR CORR=NC and FSEC=B and FPAR#B and FB2=B TO UN1,UR1
		
	EndIf

	IF TV>0
		Select REPORTE
		Append Blank
		Replace CONCEPTO With Str(NC,4),COL1 With Str(TV,4),COL2 With Str(UM1,4,1),COL3 With Str(UO1,4,1),;
						COL4 With Str(UP1,4,1),COL5 With Str(UQ1,4,1),COL6 With Str(UR1,4)
	Else
	EndIf

	Select CORRAL
	SKIP
	If EOF()
		Exit
	Endif

	If TIPO#'P'
		Skip
	Endif
ENDDO 
Select REPORTE

GO TOP
RETURN


* PRODUCCION POR PARTO     
* --------------------
PROC RV1052
Declare UNP[13],UN1[13],UM1[13],UO1[13],UP1[13],UQ1[13],UR1[13]
Create Cursor REPORTE (Concepto c(8),COL1 c(5),COL2 c(5),COL3 c(5),COL4 c(5),COL5 c(5),COL6 c(5))

Select REG
Set Order To 2
Set Filter To FB2=B
UNP=1
X=1

DO WHILE UNP<=13
	GO TOP
	CALCULATE CNT(),AVG((((DATE()-FNAC)/30.4))) FOR NP=UNP And FB2=B TO UN1[X],UR1[X]
	CALCULATE AVG(PRM),MIN(PRM),MAX(PRM),STD(PRM) FOR NP=UNP And FB2=B And PRM>0 TO UM1[X],UO1[X],UP1[X],UQ1[X]

	If UN1[X]>0
			Select REPORTE
			Append Blank
			Replace CONCEPTO With Alltrim(Str(UNP,8)),COL1 With Str(UN1(x),4),COL2 With Str(UM1(x),4,1),;
							COL3 With Str(UO1(x),4,1),COL4 With Str(UP1(x),4,1),COL5 With Str(UQ1(x),4,1),;
							COL6 With STR(INT(UR1(x)/12),2)+'-'+STR(MOD(UR1(x),2),2)  
	EndIf
	
	Select REG
	X=X+1
	UNP=UNP+1
ENDD 

CALCULATE CNT(),AVG(NP),AVG((((DATE()-FNAC)/30.4))) FOR NP>0 .AND. FB2=B TO TOTAL,PPROM,PEDAD

Select REPORTE
Append Blank
Append Blank
Replace CONCEPTO With 'PROMEDIO :'
Append Blank
Replace CONCEPTO With Str(PPROM,3,1),COL1 With Str(TOTAL,4),COL6 With STR(INT(PEDAD/12),2)+'-'+STR(MOD(PEDAD,2),2) 
GO TOP
RETURN

* RESUMEN DE CAUSAS DE BAJA
* -------------------------
PROCEDURE RV1032
Create Cursor REPORTE (Concepto c(20),COL1 c(4),COL2 c(5),COL3 c(5),COL4 c(5),COL5 c(5),COL6 c(5))
Select REG
Set Order To 2
Set Filt To 
COUNT FOR REG.FB2#B And REG.CAU2>0 And REG.FB2>=xFECHA1 And REG.FB2<=xFECHA2 TO xTOTAL

Select CBAJA
Scan
	NC=CBAJA.CNUM
	NM=CBAJA.CNOM

Select REG
COUNT FOR REG.FB2#B And REG.CAU2=CBAJA.CNUM And REG.FB2>=xFECHA1 And REG.FB2<=xFECHA2 TO TV
*--------------------------------------------------------------------------*
IF TV>0
	Select REPORTE
	Append Blank
	Replace CONCEPTO With CBAJA.CNOM,COL1 With Str(TV,4),COL2 With Str((TV/xTOTAL)*100,5,1)
Else
Endif
*------------------------*
Select CBAJA
	If EOF()
		Exit
 Endif
EndScan
Select REPORTE
GO TOP
RETURN

* INCIDENCIA POR ENFERMEDAD
* -------------------------
PROCEDURE RV1033
Create Cursor REPORTE (Concepto c(20),COL1 c(4),COL2 c(5),COL3 c(5),COL4 c(5),COL5 c(5),COL6 c(5))

Select CLIN
Set Order To 2
COUNT FOR NTRAT=1 And FECHA>=xFECHA1 And FECHA<=xFECHA2 TO xTOTAL

Select ENFERM
Scan
	NC=ENFERM.NUM
	NM=ENFERM.NOMBRE

	Select CLIN
	Set Order To 2
	COUNT FOR ENF=NC And NTRAT=1 And FECHA>=xFECHA1 And FECHA<=xFECHA2 TO TV
	*--------------------------------------------------------------------------*
IF TV>0
	Select REPORTE
	Append Blank
	Replace CONCEPTO With NM,COL1 With Str(TV,4),COL2 With Str((TV/xTOTAL)*100,5,1)
Else
Endif
*------------------------*
Select ENFERM
	If EOF()
		Exit
 Endif
EndScan
Select REPORTE
GO TOP
RETURN

* RESUMEN DE CAUSAS DE BAJA (CRIANZA)
* -------------------------
PROCEDURE RV10114
Create Cursor REPORTE (Concepto c(20),COL1 c(4),COL2 c(5),COL3 c(5),COL4 c(5),COL5 c(5),COL6 c(5))
Select BAJAS
Set Order To 1
Set Filt To 
COUNT FOR FECHA#B And CAUSA>0 And FECHA>=xFECHA1 And FECHA<=xFECHA2 TO xTOTAL

Select CBAJA
Scan
	NC=CBAJA.CNUM
	NM=CBAJA.CNOM

Select BAJAS
COUNT FOR FECHA#B And CAUSA=CBAJA.CNUM And FECHA>=xFECHA1 And FECHA<=xFECHA2 TO TV
*--------------------------------------------------------------------------*
IF TV>0
	Select REPORTE
	Append Blank
	Replace CONCEPTO With CBAJA.CNOM,COL1 With Str(TV,4),COL2 With Str((TV/xTOTAL)*100,5,1)
Else
Endif
*------------------------*
Select CBAJA
	If EOF()
		Exit
 Endif
EndScan
Select REPORTE
GO TOP
RETURN

* INCIDENCIA POR ENFERMEDAD
* -------------------------
PROCEDURE RV10115
Create Cursor REPORTE (Concepto c(20),COL1 c(4),COL2 c(5),COL3 c(5),COL4 c(5),COL5 c(5),COL6 c(5))

Select CLIN
Set Order To 3
COUNT FOR NTRAT=1 And FECHA>=xFECHA1 And FECHA<=xFECHA2 TO xTOTAL

Select ENFERM
Scan
	NC=ENFERM.NUM
	NM=ENFERM.NOMBRE

	Select CLIN
	Set Order To 3
	COUNT FOR ENF=NC And NTRAT=1 And FECHA>=xFECHA1 And FECHA<=xFECHA2 TO TV
	*--------------------------------------------------------------------------*
IF TV>0
	Select REPORTE
	Append Blank
	Replace CONCEPTO With NM,COL1 With Str(TV,4),COL2 With Str((TV/xTOTAL)*100,5,1)
Else
Endif
*------------------------*
Select ENFERM
	If EOF()
		Exit
 Endif
EndScan
Select REPORTE
GO TOP
RETURN

* PRUEBA DE 24 DIAS
* -----------------
PROCEDURE RV10173
PARAMETERS xD1,xD2
Select REG
Set Order To 2

Scan For FB2=B And DPR>=xD1 And DPR<=xD2 AND PSV#'S'
Replace DTC With ' '
	Do Case
		Case UCAL>=Date()-21 And UCAL<=Date()
					Replace DTC With '+'
		Case UCAL<Date()-21 And (STAT='INSEM' Or STAT='CARGA')
					Replace DTC With ' '
		Otherwise
					Replace DTC With '-'
	EndCase												
EndScan
Set Filter To

Select ID,CORR,DPR,PRM,STAT,NP,UTRA,MED1,UCAL,(DATE()-UCAL),DTC From REG Where NP>0 And FB2=B And (DTC='+' Or DTC='-') And DPR>=xD1 And DPR<=xD2 AND PSV#'S'Into Table xDATOS
Count For DTC='+' To xx
Count For DTC='-' Or DTC='+' To xTOTAL
xFERT=(xx/xTOTAL)*100
RETURN

* PRUEBA DE 24 DIAS (CRIANZA)
* -----------------
PROCEDURE RV10208
Select REG
Set Order To 3

Scan For FB2=B And (DATE()-FNAC)>420  
Replace DTC With ' '
	Do Case
		Case UCAL>=Date()-24 And UCAL<=Date()
					Replace DTC With '+'
		Case UCAL<Date()-24 And (STAT='INSEM' Or STAT='CARGA')
					Replace DTC With ' '
		Otherwise
					Replace DTC With '-'
	EndCase												
EndScan
Set Filter To

Select ID,CORR,FNAC,Str((DATE()-FNAC)/30.4,5,1),STAT,UTRA,MED1,UCAL,(DATE()-UCAL),DTC From REG Where NP=0 And FB2=B And (DTC='+' Or DTC='-') And DATE()-FNAC>420 Into Table xDATOS
Count For DTC='+' To xx
Count For DTC='-' Or DTC='+' To xTOTAL
xFERT=(xx/xTOTAL)*100
RETURN						


* RELACION POR DIAS EN LECHE Y EDO REPRODUCTIVO
* ---------------------------------------------
PROC RV10168
Select REG
Set Order To 2
Set Filter To FB2=B
DIMENSION aw(100)
aw=0
dat=0

scan
xx=(date()-fpar)-(date()-fsec)

dat=dat+1
**-------------------------------------*
do case
	case stat=[FRESC]
		aw(1)=aw(1)+1
		do case 
			case xx<45
				aw(2)=aw(2)+1

			case xx>=45 and xx<=100
				aw(3)=aw(3)+1

			case xx>100 and xx<=150
				aw(4)=aw(4)+1

			case xx>150 and xx<=200
				aw(5)=aw(5)+1

			case xx>200 and xx<=300
				aw(6)=aw(6)+1

			case xx>300
				aw(7)=aw(7)+1
		endcase		

	case stat=[LIMPI]
		aw(8)=aw(8)+1
		do case 
			case xx<45
				aw(9)=aw(9)+1

			case xx>=45 and xx<=100
				aw(10)=aw(10)+1

			case xx>100 and xx<=150
				aw(11)=aw(11)+1

			case xx>150 and xx<=200
				aw(12)=aw(12)+1

			case xx>200 and xx<=300
				aw(13)=aw(13)+1

			case xx>300
				aw(14)=aw(14)+1
		endcase		

	case stat=[SUCIA]
		aw(15)=aw(15)+1
		do case 
			case xx<45
				aw(16)=aw(16)+1

			case xx>=45 and xx<=100
				aw(17)=aw(17)+1

			case xx>100 and xx<=150
				aw(18)=aw(18)+1

			case xx>150 and xx<=200
				aw(19)=aw(19)+1

			case xx>200 and xx<=300
				aw(20)=aw(20)+1

			case xx>300
				aw(21)=aw(21)+1
		endcase		

	case stat=[ANEST]
		aw(22)=aw(22)+1
		do case 
			case xx<45
				aw(23)=aw(23)+1

			case xx>=45 and xx<=100
				aw(24)=aw(24)+1

			case xx>100 and xx<=150
				aw(25)=aw(25)+1

			case xx>150 and xx<=200
				aw(26)=aw(26)+1

			case xx>200 and xx<=300
				aw(27)=aw(27)+1

			case xx>300
				aw(28)=aw(28)+1
		endcase		

	case stat=[TRATA]
		aw(29)=aw(29)+1
		do case 
			case xx<45
				aw(30)=aw(30)+1

			case xx>=45 and xx<=100
				aw(31)=aw(31)+1

			case xx>100 and xx<=150
				aw(32)=aw(32)+1

			case xx>150 and xx<=200
				aw(33)=aw(33)+1

			case xx>200 and xx<=300
				aw(34)=aw(34)+1

			case xx>300
				aw(35)=aw(35)+1
		endcase		

	case stat=[INSEM]
		aw(36)=aw(36)+1
		do case 
			case xx<45
				aw(37)=aw(37)+1

			case xx>=45 and xx<=100
				aw(38)=aw(38)+1

			case xx>100 and xx<=150
				aw(39)=aw(39)+1

			case xx>150 and xx<=200
				aw(40)=aw(40)+1

			case xx>200 and xx<=300
				aw(41)=aw(41)+1

			case xx>300
				aw(42)=aw(42)+1
		endcase		

	case stat=[CARGA]
		aw(43)=aw(43)+1
		do case 
			case xx<45
				aw(44)=aw(44)+1

			case xx>=45 and xx<=100
				aw(45)=aw(45)+1

			case xx>100 and xx<=150
				aw(46)=aw(46)+1

			case xx>150 and xx<=200
				aw(47)=aw(47)+1

			case xx>200 and xx<=300
				aw(48)=aw(48)+1

			case xx>300
				aw(49)=aw(49)+1
		endcase		

	case stat=[VACIA]
		aw(50)=aw(50)+1
		do case 
			case xx<45
				aw(51)=aw(51)+1

			case xx>=45 and xx<=100
				aw(52)=aw(52)+1

			case xx>100 and xx<=150
				aw(53)=aw(53)+1

			case xx>100 and xx<=200
				aw(54)=aw(54)+1

			case xx>200 and xx<=300
				aw(55)=aw(55)+1

			case xx>300
				aw(56)=aw(56)+1
		endcase		

	case stat=[ABORT]
		aw(57)=aw(57)+1
		do case 
			case xx<45
				aw(58)=aw(58)+1

			case xx>=45 and xx<=100
				aw(59)=aw(59)+1

			case xx>100 and xx<=150
				aw(60)=aw(60)+1

			case xx>150 and xx<=200
				aw(61)=aw(61)+1

			case xx>200 and xx<=300
				aw(62)=aw(62)+1

			case xx>300
				aw(63)=aw(63)+1
		endcase		

endcase	
**-------------------------------------*
ends

** Suma por dias en leche
** ----------------------
aw(65)=aw(2)+aw(9)+aw(16)+aw(23)+aw(30)+aw(37)+aw(44)+aw(51)+aw(58)
aw(66)=aw(3)+aw(10)+aw(17)+aw(24)+aw(31)+aw(38)+aw(45)+aw(52)+aw(59)
aw(67)=aw(4)+aw(11)+aw(18)+aw(25)+aw(32)+aw(39)+aw(46)+aw(53)+aw(60)
aw(68)=aw(5)+aw(12)+aw(19)+aw(26)+aw(33)+aw(40)+aw(47)+aw(54)+aw(61)
aw(69)=aw(6)+aw(13)+aw(20)+aw(27)+aw(34)+aw(41)+aw(48)+aw(55)+aw(62)
aw(70)=aw(7)+aw(14)+aw(21)+aw(28)+aw(35)+aw(42)+aw(49)+aw(56)+aw(63)

aw(71)=aw(1)+aw(8)+aw(15)+aw(22)+aw(29)+aw(36)+aw(43)+aw(50)+aw(57)

Set Filter To

Create Cursor REPORTE (Concepto c(20),COL1 c(5),COL2 c(5),COL3 c(5),COL4 c(5),COL5 c(5),COL6 c(5),COL7 c(5))
Append Blank
Replace CONCEPTO With 'Estado'
Append Blank
Replace CONCEPTO With 'Frescas',COL1 With Str(aw(2),4),COL2 With Str(aw(3),4),COL3 With Str(aw(4),4),COL4 With Str(aw(5),4),COL5 With Str(aw(6),4),COL6 With Str(aw(7),4),COL7 With Str(aw(1),4)
Append Blank
Replace CONCEPTO With 'Limpias',COL1 With Str(aw(9),4),COL2 With Str(aw(10),4),COL3 With Str(aw(11),4),COL4 With Str(aw(12),4),COL5 With Str(aw(13),4),COL6 With Str(aw(14),4),COL7 With Str(aw(8),4)
Append Blank
Replace CONCEPTO With 'Sucias',COL1 With Str(aw(16),4),COL2 With Str(aw(17),4),COL3 With Str(aw(18),4),COL4 With Str(aw(19),4),COL5 With Str(aw(20),4),COL6 With Str(aw(21),4),COL7 With Str(aw(15),4)
Append Blank
Replace CONCEPTO With 'Anestros',COL1 With Str(aw(23),4),COL2 With Str(aw(24),4),COL3 With Str(aw(25),4),COL4 With Str(aw(26),4),COL5 With Str(aw(27),4),COL6 With Str(aw(28),4),COL7 With Str(aw(22),4)
Append Blank
Replace CONCEPTO With 'Tratamiento',COL1 With Str(aw(30),4),COL2 With Str(aw(31),4),COL3 With Str(aw(32),4),COL4 With Str(aw(33),4),COL5 With Str(aw(34),4),COL6 With Str(aw(35),4),COL7 With Str(aw(29),4)
Append Blank
Replace CONCEPTO With 'Inseminadas',COL1 With Str(aw(37),4),COL2 With Str(aw(38),4),COL3 With Str(aw(39),4),COL4 With Str(aw(40),4),COL5 With Str(aw(41),4),COL6 With Str(aw(42),4),COL7 With Str(aw(36),4)
Append Blank
Replace CONCEPTO With 'Gestantes',COL1 With Str(aw(44),4),COL2 With Str(aw(45),4),COL3 With Str(aw(46),4),COL4 With Str(aw(47),4),COL5 With Str(aw(48),4),COL6 With Str(aw(49),4),COL7 With Str(aw(43),4)
Append Blank
Replace CONCEPTO With 'Vacias',COL1 With Str(aw(51),4),COL2 With Str(aw(52),4),COL3 With Str(aw(53),4),COL4 With Str(aw(54),4),COL5 With Str(aw(55),4),COL6 With Str(aw(56),4),COL7 With Str(aw(50),4)
Append Blank
Replace CONCEPTO With 'Abortadas',COL1 With Str(aw(58),4),COL2 With Str(aw(59),4),COL3 With Str(aw(60),4),COL4 With Str(aw(61),4),COL5 With Str(aw(62),4),COL6 With Str(aw(63),4),COL7 With Str(aw(57),4)
Append Blank
Replace CONCEPTO With ''
Append Blank
Replace CONCEPTO With 'Total',COL1 With Str(aw(65),4),COL2 With Str(aw(66),4),COL3 With Str(aw(67),4),COL4 With Str(aw(68),4),COL5 With Str(aw(69),4),COL6 With Str(aw(70),4),COL7 With Str(aw(71),4)
GO TOP
RETURN



*--------------------------- PROCEDURES -------------------------*
PROCEDURE O1
PARAMETERS COLS,ROWS,COLW,FIELD
DO WHILE MOD(RECCOUNT(),COLS) # 0
APPE BLANK
ENDD
GO TOP
PAG=1
PRINTED=0
NVAC=0
SET CONS OFF
SET PRIN ON
DO WHILE PRINTED<RECCOUNT() &&
?SPACE(1)+'PAG. NO. '+STR(PAG,2)
?SPACE(78)+NOM
?
?SPACE(1)+DTOC(DATE())+SPACE((82-LEN(NTIT))/2)+NTIT
IF NT2=[Kilos]
?SPACE(40)+' De '+STR(PR1,4)+'  A'+STR(PR2,4)+' '+NT2
ELSE
?SPACE(40)+' De '+STR(DL1,4)+'  A'+STR(DL2,4)+' '+NT2
ENDI
?'  '+REPLI(CHR(254),94)
?'       ID    DEL  Status   Ult. Prod.  Corral         ID    DEL  Status   Ult. Prod.  Corral'
?'  '+REPLI('-',94)
?
ONTHISPAGE=MIN(ROWS*COLS,RECCOUNT()-PRINTED)
ROWS=MIN(ROWS,(ONTHISPAGE/COLS))
THISROW=1
DO WHILE THISROW<=ROWS .AND. PRINTED<=RECCOUNT()
THISCOL=1
if id>0
?SPACE(4),LEFT(STR(ID,5,0)+'   '+STR(DATE()-FPAR,4)+'   '+STAT+'    '+STR(PRM,4,1)+'    '+STR(CORR,2),COLW)+SPACE(MAX(0,COLW-LEN(FIELD)))
NVAC=NVAC+1
endi
PRINTED=PRINTED+1
DO WHILE THISCOL<COLS .AND. PRINTED<=RECCOUNT()
SKIP ROWS
if id>0
??SPACE(4),LEFT(STR(ID,5,0)+'   '+STR(DATE()-FPAR,4)+'   '+STAT+'    '+STR(PRM,4,1)+'    '+STR(CORR,2),COLW)+SPACE(MAX(0,COLW-LEN(FIELD)))
NVAC=NVAC+1
endi
THISCOL=THISCOL+1
PRINTED=PRINTED+1
ENDD
THISROW=THISROW+1
SKIP (((COLS-1)*ROWS)-1)*-1

IF THISROW-1=ROWS .AND. PRINTED<RECCOUNT()
?'  '+REPLI(CHR(254),94)
ENDI

IF PRINTED=RECCOUNT()
?
?'  '+REPLI(CHR(254),94)
?
?'   TOTAL = '+STR(NVAC,4)
ENDI
ENDD
EJECT
PAG=PAG+1
SKIP ((COLS-1)*ROWS)
ENDD
CLOS DATA
ERAS LOTES.DBF
ERAS LOTES.IDX
ERAS REG2.IDX


PROCEDURE O2
*---------------------------------------------------------------------*
PARAMETERS COLS,ROWS,COLW,FIELD
DO WHILE MOD(RECCOUNT(),COLS) # 0
APPE BLANK
ENDD
GO TOP
PAG=1
PRINTED=0
NVAC=0
SET CONS OFF
SET PRIN ON
???&q19
DO WHILE PRINTED<RECCOUNT() &&
?SPACE(1)+'PAG. NO. '+STR(PAG,2)
?space(77)+nom
?
?SPACE(1)+DTOC(DATE())+SPACE((82-LEN(NTIT))/2)+NTIT
IF NT2=[Kilos]
?SPACE(40)+' De '+STR(PR1,4)+'  A'+STR(PR2,4)+' '+NT2
ELSE
?SPACE(40)+' De '+STR(DL1,4)+'  A'+STR(DL2,4)+' '+NT2
ENDI
?'  '+REPLI(CHR(254),94)
?'      ID   Parto   DEL  Prod Status Corral            ID   Parto   DEL  Prod Status Corral  '
?'  '+REPLI('-',94)
?
ONTHISPAGE=MIN(ROWS*COLS,RECCOUNT()-PRINTED)
ROWS=MIN(ROWS,(ONTHISPAGE/COLS))
THISROW=1
DO WHILE THISROW<=ROWS .AND. PRINTED<=RECCOUNT()
THISCOL=1
if id>0
?SPACE(1),LEFT(STR(ID,5,0)+str(np,8)+'  '+STR(DATE()-FPAR,4)+'  '+STR(PRM,4,1)+'  '+LEFT(STAT,5)+'  '+STR(CORR,2),COLW)+SPACE(MAX(0,COLW-LEN(FIELD)))
NVAC=NVAC+1
endi
PRINTED=PRINTED+1
DO WHILE THISCOL<COLS .AND. PRINTED<=RECCOUNT()
SKIP ROWS
if id>0
??SPACE(1),LEFT(STR(ID,5,0)+str(np,8)+'  '+STR(DATE()-FPAR,4)+'  '+STR(PRM,4,1)+'  '+LEFT(STAT,5)+'  '+STR(CORR,2),COLW)+SPACE(MAX(0,COLW-LEN(FIELD)))
NVAC=NVAC+1
endi
THISCOL=THISCOL+1
PRINTED=PRINTED+1
ENDD
THISROW=THISROW+1
SKIP (((COLS-1)*ROWS)-1)*-1

IF THISROW-1=ROWS .AND. PRINTED<RECCOUNT()
?'  '+REPLI(CHR(254),94)
ENDI

IF PRINTED=RECCOUNT()
?
?'  '+REPLI(CHR(254),94)
?
?'   TOTAL = '+STR(NVAC,4)
ENDI
ENDD
*EJECT
PAG=PAG+1
SKIP ((COLS-1)*ROWS)
ENDD
CLOS DATA
ERAS LOTES.DBF
ERAS LOTES.IDX
ERAS REG2.IDX

PROCEDURE CC1  && Impresion de ENFERM,CBAJA,MEDIC,GNVACUNA,CORRAL.
*-----------------------------------------------------------------
PARAMETERS COLS,ROWS,COLW,FIELD
DO WHILE MOD(RECCOUNT(),COLS) # 0
APPE BLANK
ENDD
GO TOP
PAG=1
PRINTED=0
NVAC=0

 DO WHILE PRINTED<RECCOUNT() 
 ?'  Hato     : '+Q36+'   '+nom 
 ?'  Area     : ESTABLO'
 ?'  Reporte  : '+NTIT
 ?'  Fecha    : '+DTOC(DATE())+'  Hora : '+TIME()
 ?'  Pagina   : '+STR(PAG,2,0)+space(65)+[DAIRYFOX v.3.0]
 ?'  '+REPLI('-',94)
 ?
 ONTHISPAGE=MIN(ROWS*COLS,RECCOUNT()-PRINTED)
 ROWS=MIN(ROWS,(ONTHISPAGE/COLS))
 THISROW=1
 DO WHILE THISROW<=ROWS .AND. PRINTED<=RECCOUNT()
 THISCOL=1
 if CNUM>0
	?SPACE(1),Left(Str(CNUM,4)+'    '+CNOM,COLW)+SPACE(MAX(0,COLW-LEN(FIELD)))
  NVAC=NVAC+1
 endi
 PRINTED=PRINTED+1
 DO WHILE THISCOL<COLS .AND. PRINTED<=RECCOUNT()
  SKIP ROWS
  If CNUM>0
   ??SPACE(1),Left(Str(CNUM,4)+'    '+CNOM,COLW)+SPACE(MAX(0,COLW-LEN(FIELD)))
   NVAC=NVAC+1
  endi
  THISCOL=THISCOL+1
  PRINTED=PRINTED+1
 ENDD
THISROW=THISROW+1
SKIP (((COLS-1)*ROWS)-1)*-1
*? Para Doble espacio
	
	If THISROW-1=ROWS .AND. PRINTED<RECCOUNT()
		?'  '+REPLI('-',94)
	Endif

	If PRINTED=RECCOUNT()	
		?
		?'  '+REPLI('-',94)
		?
		?'   TOTAL = '+STR(NVAC,4)
	Endif
ENDD
*EJECT
PAG=PAG+1
SKIP ((COLS-1)*ROWS)
ENDD
Use
ERASE LOTES.DBF
RETURN



PROCEDURE CC2 && Impresion de TECNICOS,GNCODIGO.
*-----------------------------------------------
PARAMETERS COLS,ROWS,COLW,FIELD
DO WHILE MOD(RECCOUNT(),COLS) # 0
APPE BLANK
ENDD
GO TOP
PAG=1
PRINTED=0
NVAC=0
SET CONS OFF
SET PRIN ON
DO WHILE PRINTED<RECCOUNT() 
?'  Hato     : '+Q36+'   '+nom 
?'  Area     : ESTABLO'
?'  Reporte  : '+NTIT
?'  Fecha    : '+DTOC(DATE())+'  Hora : '+TIME()
?'  Pagina   : '+STR(PAG,2,0)+space(65)+[DAIRYFOX v.3.0]
?'  '+REPLI('-',94)
?
ONTHISPAGE=MIN(ROWS*COLS,RECCOUNT()-PRINTED)
ROWS=MIN(ROWS,(ONTHISPAGE/COLS))
THISROW=1
DO WHILE THISROW<=ROWS .AND. PRINTED<=RECCOUNT()
THISCOL=1
if Not Empty(CNUM)
?SPACE(1),Left(CNUM+'    '+CNOM,COLW)+SPACE(MAX(0,COLW-LEN(FIELD)))
NVAC=NVAC+1
endi
PRINTED=PRINTED+1
DO WHILE THISCOL<COLS .AND. PRINTED<=RECCOUNT() 
SKIP ROWS
if Not Empty(CNUM)
??SPACE(1),LEFT(CNUM+'    '+CNOM,COLW)+SPACE(MAX(0,COLW-LEN(FIELD)))
NVAC=NVAC+1
endi
THISCOL=THISCOL+1
PRINTED=PRINTED+1
ENDD
THISROW=THISROW+1
SKIP (((COLS-1)*ROWS)-1)*-1
*?
IF THISROW-1=ROWS .AND. PRINTED<RECCOUNT()
?'  '+REPLI('-',94)
ENDI

IF PRINTED=RECCOUNT()
?
?'  '+REPLI('-',94)
?
?'   TOTAL = '+STR(NVAC,4)
ENDI
ENDD
*EJECT
PAG=PAG+1
SKIP ((COLS-1)*ROWS)
ENDD
Use
ERASE LOTES.DBF
RETURN


PROCEDURE CC3
*---------------------------------------------------------------------*
PARAMETERS COLS,ROWS,COLW,FIELD
DO WHILE MOD(RECCOUNT(),COLS) # 0
APPE BLANK
ENDD
GO TOP
PAG=1
PRINTED=0
NVAC=0
SET CONS OFF
SET PRIN ON
DO WHILE PRINTED<RECCOUNT() 
?'  Hato     : '+Q36+'   '+nom 
?'  Area     : ESTABLO'
?'  Reporte  : '+NTIT
?'  Fecha    : '+DTOC(DATE())+'  Hora : '+TIME()
?'  Pagina   : '+STR(PAG,2,0)+space(70)+[DAIRYFOX]
?'  '+REPLI(CHR(254),94)
?
ONTHISPAGE=MIN(ROWS*COLS,RECCOUNT()-PRINTED)
ROWS=MIN(ROWS,(ONTHISPAGE/COLS))
THISROW=1
DO WHILE THISROW<=ROWS .AND. PRINTED<=RECCOUNT()
THISCOL=1
if CNUM>0
?SPACE(1),LEFT(STR(CNUM,4,0)+'    '+CNOM,COLW)+SPACE(MAX(0,COLW-LEN(FIELD)))
NVAC=NVAC+1
endi
PRINTED=PRINTED+1
DO WHILE THISCOL<COLS .AND. PRINTED<=RECCOUNT()
SKIP ROWS
if CNUM>0
??SPACE(1),LEFT(STR(CNUM,4,0)+'    '+CNOM,COLW)+SPACE(MAX(0,COLW-LEN(FIELD)))
NVAC=NVAC+1
endi
THISCOL=THISCOL+1
PRINTED=PRINTED+1
ENDD
THISROW=THISROW+1
SKIP (((COLS-1)*ROWS)-1)*-1
*?
IF THISROW-1=ROWS .AND. PRINTED<RECCOUNT()
?'  '+REPLI(CHR(254),94)
ENDI

IF PRINTED=RECCOUNT()
?
?'  '+REPLI(CHR(254),94)
?
?'   TOTAL = '+STR(NVAC,4)
ENDI
ENDD
*EJECT
PAG=PAG+1
SKIP ((COLS-1)*ROWS)
ENDD
CLOS DATA
ERAS LOTES.DBF

PROCEDURE CC4
*---------------------------------------------------------------------*
PARAMETERS COLS,ROWS,COLW,FIELD
DO WHILE MOD(RECCOUNT(),COLS) # 0
APPE BLANK
ENDD
GO TOP
PAG=1
PRINTED=0
NVAC=0
SET CONS OFF
SET PRIN ON
DO WHILE PRINTED<RECCOUNT() 
?'  Hato     : '+Q36+'   '+nom 
?'  Area     : VIENTRES'
?'  Reporte  : '+NTIT
?'  Fecha    : '+DTOC(DATE())+'  Hora : '+TIME()
?'  Pagina   : '+STR(PAG,2,0)+space(70)+[DAIRYFOX]
?'  '+REPLI(CHR(254),94)
?
?'     ID   Edad (A/M)           ID   Edad (A/M)            ID   Edad (A/M)'
?'  '+REPLI('-',94)
ONTHISPAGE=MIN(ROWS*COLS,RECCOUNT()-PRINTED)
ROWS=MIN(ROWS,(ONTHISPAGE/COLS))
THISROW=1
DO WHILE THISROW<=ROWS .AND. PRINTED<=RECCOUNT()
THISCOL=1
if ID>0
EDADN=(DATE()-FNAC)/30.4
ECP=STR(INT(EDADN/12),2)+'-'+STR(MOD(EDADN,12),2)
?SPACE(1),LEFT(STR(ID,5,0)+'    '+ECP,COLW)+SPACE(MAX(0,COLW-LEN(FIELD)))
NVAC=NVAC+1
endi
PRINTED=PRINTED+1
DO WHILE THISCOL<COLS .AND. PRINTED<=RECCOUNT()
SKIP ROWS
if ID>0
EDADN=(DATE()-FNAC)/30.4
ECP=STR(INT(EDADN/12),2)+'-'+STR(MOD(EDADN,12),2)
??SPACE(1),LEFT(STR(ID,5,0)+'    '+ECP,COLW)+SPACE(MAX(0,COLW-LEN(FIELD)))
NVAC=NVAC+1
endi
THISCOL=THISCOL+1
PRINTED=PRINTED+1
ENDD
THISROW=THISROW+1
SKIP (((COLS-1)*ROWS)-1)*-1
*?
IF THISROW-1=ROWS .AND. PRINTED<RECCOUNT()
?'  '+REPLI(CHR(254),94)
ENDI

IF PRINTED=RECCOUNT()
?
?'  '+REPLI(CHR(254),94)
?
?'   TOTAL = '+STR(NVAC,4)
ENDI
ENDD
*EJECT
PAG=PAG+1
SKIP ((COLS-1)*ROWS)
ENDD
CLOS DATA
ERAS LOTES.DBF
ERAS LOTES.IDX

PROCEDURE IDS
*---------------------------------------------------------------------*
PARAMETERS COLS,ROWS,COLW,FIELD
DO WHILE MOD(RECCOUNT(),COLS) # 0
APPE BLANK
ENDD
GO TOP
PAG=1
PRINTED=0
NVAC=0
SET CONS OFF
SET PRIN ON
DO WHILE PRINTED<RECCOUNT() 
?'  Hato     : '+Q36+'   '+nom 
?'  Area     : ESTABLO'
?'  Reporte  : '+NTIT
?'  Fecha    : '+DTOC(DATE())+'  Hora : '+TIME()
?'  Pagina   : '+STR(PAG,2,0)+space(70)+[DAIRYFOX]
?'  '+REPLI(CHR(254),94)
?
ONTHISPAGE=MIN(ROWS*COLS,RECCOUNT()-PRINTED)
ROWS=MIN(ROWS,(ONTHISPAGE/COLS))
THISROW=1
DO WHILE THISROW<=ROWS .AND. PRINTED<=RECCOUNT()
THISCOL=1
IF ID>0
?SPACE(4),LEFT(STR(ID,5,0),COLW)+SPACE(MAX(0,COLW-LEN(FIELD)))
NVAC=NVAC+1
endi
PRINTED=PRINTED+1
DO WHILE THISCOL<COLS .AND. PRINTED<=RECCOUNT()
SKIP ROWS
if ID>0
??SPACE(4),LEFT(STR(ID,5,0),COLW)+SPACE(MAX(0,COLW-LEN(FIELD)))
NVAC=NVAC+1
endi
THISCOL=THISCOL+1
PRINTED=PRINTED+1
ENDD
THISROW=THISROW+1
SKIP (((COLS-1)*ROWS)-1)*-1
*?
IF THISROW-1=ROWS .AND. PRINTED<RECCOUNT()
?'  '+REPLI(CHR(254),94)
ENDI

IF PRINTED=RECCOUNT()
?
?'  '+REPLI(CHR(254),94)
?
?'   TOTAL = '+STR(NVAC,4)
ENDI
ENDD
*EJECT
PAG=PAG+1
SKIP ((COLS-1)*ROWS)
ENDD
retu

