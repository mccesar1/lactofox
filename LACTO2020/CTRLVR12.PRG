* REPORTE DE CALCULO DE TASA DE PREÑEZ
* ------------------------------------
PROCEDURE RV1068
PARAMETER xCDIAS
Dimension xVACAS(30),xDETEC(30),nVACAS(30),xCARGA(30)
xVACAS=0
xDETEC=0

nVACAS=0
xCARGA=0

Select CALOR
Set Order To 2
SET FILTER TO SER>0 

Select REG
Set Order To 2
Set Filter To FB2=B AND PSV#'S' 
Set Relation To ID Into CALOR

SCAN

** DETECCION DE CELO
** ------------------------------*
		If FPAR+xCDIAS<=DATE()-231
			xVACAS(1)=IIF(UCAL<=DATE()-231 And STAT='CARGA',xVACAS(1),xVACAS(1)+1)
			
			Select CALOR
			Seek REG.ID
			Scan While CALOR.ID=REG.ID 
				xDETEC(1)=IIF(FECHA>=DATE()-252 And FECHA<=DATE()-231 And (CLAVE=4 Or CLAVE=5),xDETEC(1)+1,xDETEC(1))
			EndScan
			Select REG
		Endif

		If FPAR+xCDIAS<=DATE()-210
			xVACAS(2)=IIF(UCAL<=DATE()-210 And STAT='CARGA',xVACAS(2),xVACAS(2)+1)
			
			Select CALOR
			Seek REG.ID
			Scan While CALOR.ID=REG.ID 
				xDETEC(2)=IIF(FECHA>=DATE()-231 And FECHA<=DATE()-210 And (CLAVE=4 Or CLAVE=5),xDETEC(2)+1,xDETEC(2))
			EndScan
			Select REG
		Endif

		If FPAR+xCDIAS<=DATE()-189
			xVACAS(3)=IIF(UCAL<DATE()-189 And STAT='CARGA',xVACAS(3),xVACAS(3)+1)

			Select CALOR
			Seek REG.ID
			Scan While CALOR.ID=REG.ID 
				xDETEC(3)=IIF(FECHA>=DATE()-210 And FECHA<=DATE()-189 And (CLAVE=4 Or CLAVE=5),xDETEC(3)+1,xDETEC(3))
			EndScan
			Select REG
		Endif

		If FPAR+xCDIAS<=DATE()-168
			xVACAS(4)=IIF(UCAL<DATE()-168 And STAT='CARGA',xVACAS(4),xVACAS(4)+1)

			Select CALOR
			Seek REG.ID
			Scan While CALOR.ID=REG.ID 
				xDETEC(4)=IIF(FECHA>=DATE()-189 And FECHA<DATE()-168 And (CLAVE=4 Or CLAVE=5),xDETEC(4)+1,xDETEC(4))
			EndScan
			Select REG
		Endif

		If FPAR+xCDIAS<=DATE()-147
			xVACAS(5)=IIF(UCAL<DATE()-147 And STAT='CARGA',xVACAS(5),xVACAS(5)+1)

			Select CALOR
			Seek REG.ID
			Scan While CALOR.ID=REG.ID 
				xDETEC(5)=IIF(FECHA>=DATE()-168 And FECHA<DATE()-147 And (CLAVE=4 Or CLAVE=5),xDETEC(5)+1,xDETEC(5))
			EndScan
			Select REG
		Endif

		If FPAR+xCDIAS<=DATE()-126
			xVACAS(6)=IIF(UCAL<DATE()-126 And STAT='CARGA',xVACAS(6),xVACAS(6)+1)

			Select CALOR
			Seek REG.ID
			Scan While CALOR.ID=REG.ID 
				xDETEC(6)=IIF(FECHA>=DATE()-147 And FECHA<DATE()-126 And (CLAVE=4 Or CLAVE=5),xDETEC(6)+1,xDETEC(6))
			EndScan
			Select REG
		Endif

		If FPAR+xCDIAS<=DATE()-105
			xVACAS(7)=IIF(UCAL<DATE()-105 And STAT='CARGA',xVACAS(7),xVACAS(7)+1)

			Select CALOR
			Seek REG.ID
			Scan While CALOR.ID=REG.ID 
				xDETEC(7)=IIF(FECHA>=DATE()-126 And FECHA<DATE()-105 And (CLAVE=4 Or CLAVE=5),xDETEC(7)+1,xDETEC(7))
			EndScan
			Select REG
		Endif

		If FPAR+xCDIAS<=DATE()-84
			xVACAS(8)=IIF(UCAL<DATE()-84 And STAT='CARGA',xVACAS(8),xVACAS(8)+1)

			Select CALOR
			Seek REG.ID
			Scan While CALOR.ID=REG.ID 
				xDETEC(8)=IIF(FECHA>=DATE()-105 And FECHA<DATE()-84 And (CLAVE=4 Or CLAVE=5),xDETEC(8)+1,xDETEC(8))
			EndScan
			Select REG
		Endif

		If FPAR+xCDIAS<=DATE()-63
			xVACAS(9)=IIF(UCAL<DATE()-63 And STAT='CARGA',xVACAS(9),xVACAS(9)+1)

			Select CALOR
			Seek REG.ID
			Scan While CALOR.ID=REG.ID 
				xDETEC(9)=IIF(FECHA>=DATE()-84 And FECHA<DATE()-63 And (CLAVE=4 Or CLAVE=5),xDETEC(9)+1,xDETEC(9))
			EndScan
			Select REG
		Endif
		
		If FPAR+xCDIAS<=DATE()-42
			xVACAS(10)=IIF(UCAL<DATE()-42 And STAT='CARGA',xVACAS(10),xVACAS(10)+1)

			Select CALOR
			If Seek(REG.ID)
			Scan While CALOR.ID=REG.ID 
				xDETEC(10)=IIF(FECHA>=DATE()-63 And FECHA<DATE()-42 And (CLAVE=4 Or CLAVE=5),xDETEC(10)+1,xDETEC(10))
			EndScan
			Endif
			Select REG
		Endif
		
		If FPAR+xCDIAS<=DATE()-21
			xVACAS(11)=IIF(UCAL<DATE()-21 And STAT='CARGA' AND REG.FB1<=DATE()-21,xVACAS(11),xVACAS(11)+1)
*			xVACAS(11)=IIF(UCAL<DATE()-21 And STAT='CARGA',xVACAS(11),xVACAS(11)+1)
			
			Select CALOR
			If Seek(REG.ID)
			Scan While CALOR.ID=REG.ID 
				xDETEC(11)=IIF(FECHA>=DATE()-42 And FECHA<DATE()-21 And (CLAVE=4 Or CLAVE=5),xDETEC(11)+1,xDETEC(11))
			EndScan
			Endif
			Select REG
		Endif


		If FPAR+xCDIAS<=DATE()
			xVACAS(12)=IIF(UCAL<DATE()-21 And (STAT='INSEM' Or STAT='CARGA'),xVACAS(12),xVACAS(12)+1)   && CORRECTO
			
			Select CALOR
			If Seek(REG.ID)
			Scan While CALOR.ID=REG.ID 
				xDETEC(12)=IIF(FECHA>=DATE()-21 And FECHA<=DATE() And REG.UCAL=CALOR.FECHA,xDETEC(12)+1,xDETEC(12))
			EndScan
			Endif
			Select REG
		Endif

		xVACAS(15)=xVACAS(1)+xVACAS(2)+xVACAS(3)+xVACAS(4)+xVACAS(5)+xVACAS(6)+xVACAS(7)+xVACAS(8)+xVACAS(9)+xVACAS(10)+xVACAS(11)+xVACAS(12)
		xDETEC(15)=xDETEC(1)+xDETEC(2)+xDETEC(3)+xDETEC(4)+xDETEC(5)+xDETEC(6)+xDETEC(7)+xDETEC(8)+xDETEC(9)+xDETEC(10)+xDETEC(11)+xDETEC(12)

ENDSCAN


** CALCULO DE TASA DE PREÑEZ
** ------------------------------*
SCAN
		If FPAR+Q5<=DATE()-231 
*			nVACAS(1)=IIF(UCAL<=DATE()-252 And (STAT='INSEM' Or STAT='CARGA'),nVACAS(1),nVACAS(1)+1)
			nVACAS(1)=IIF(UCAL<=DATE()-231 And STAT='CARGA',nVACAS(1),nVACAS(1)+1)
			xCARGA(1)=IIF(UCAL>=DATE()-252 And UCAL<=DATE()-231 And STAT='CARGA',xCARGA(1)+1,xCARGA(1))
		Endif

		If FPAR+Q5<=DATE()-210 
			nVACAS(2)=IIF(UCAL<=DATE()-210 And STAT='CARGA',nVACAS(2),nVACAS(2)+1)
			xCARGA(2)=IIF(UCAL>=DATE()-231 And UCAL<=DATE()-210 And STAT='CARGA',xCARGA(2)+1,xCARGA(2))
		Endif

		If FPAR+Q5<=DATE()-189 
			nVACAS(3)=IIF(UCAL<=DATE()-189 And STAT='CARGA',nVACAS(3),nVACAS(3)+1)
			xCARGA(3)=IIF(UCAL>=DATE()-210 And UCAL<=DATE()-189 And STAT='CARGA',xCARGA(3)+1,xCARGA(3))
		Endif

		If FPAR+Q5<=DATE()-168
			nVACAS(4)=IIF(UCAL<=DATE()-168 And STAT='CARGA',nVACAS(4),nVACAS(4)+1)
			xCARGA(4)=IIF(UCAL>=DATE()-189 And UCAL<=DATE()-168 And STAT='CARGA',xCARGA(4)+1,xCARGA(4))
		Endif

		If FPAR+Q5<=DATE()-147
			nVACAS(5)=IIF(UCAL<=DATE()-147 And STAT='CARGA',nVACAS(5),nVACAS(5)+1)
			xCARGA(5)=IIF(UCAL>=DATE()-168 And UCAL<=DATE()-147 And STAT='CARGA',xCARGA(5)+1,xCARGA(5))
		Endif

		If FPAR+Q5<=DATE()-126
			nVACAS(6)=IIF(UCAL<=DATE()-126 And STAT='CARGA',nVACAS(6),nVACAS(6)+1)
			xCARGA(6)=IIF(UCAL>=DATE()-147 And UCAL<=DATE()-126 And STAT='CARGA',xCARGA(6)+1,xCARGA(6))
		Endif

		If FPAR+Q5<=DATE()-105
			nVACAS(7)=IIF(UCAL<=DATE()-105 And STAT='CARGA',nVACAS(7),nVACAS(7)+1)
			xCARGA(7)=IIF(UCAL>=DATE()-126 And UCAL<=DATE()-105 And STAT='CARGA',xCARGA(7)+1,xCARGA(7))
		Endif

		If FPAR+Q5<=DATE()-84
			nVACAS(8)=IIF(UCAL<=DATE()-84 And STAT='CARGA',nVACAS(8),nVACAS(8)+1)
			xCARGA(8)=IIF(UCAL>=DATE()-105 And UCAL<=DATE()-84 And STAT='CARGA',xCARGA(8)+1,xCARGA(8))
		Endif

		If FPAR+Q5<=DATE()-63
			nVACAS(9)=IIF(UCAL<=DATE()-63 And STAT='CARGA',nVACAS(9),nVACAS(9)+1)
			xCARGA(9)=IIF(UCAL>=DATE()-84 And UCAL<=DATE()-63 And STAT='CARGA',xCARGA(9)+1,xCARGA(9))
		Endif
		
		If FPAR+Q5<=DATE()-42 
			nVACAS(10)=IIF(UCAL<=DATE()-42 And STAT='CARGA',nVACAS(10),nVACAS(10)+1)
			xCARGA(10)=IIF(UCAL>=DATE()-63 And UCAL<=DATE()-42 And STAT='CARGA',xCARGA(10)+1,xCARGA(10))
		Endif
		
		If FPAR+Q5<=DATE()-21 
			nVACAS(11)=0
			xCARGA(11)=0
		Endif

		If FPAR+Q5<=DATE()
			nVACAS(12)=0
			xCARGA(12)=0
		Endif

		nVACAS(15)=nVACAS(1)+nVACAS(2)+nVACAS(3)+nVACAS(4)+nVACAS(5)+nVACAS(6)+nVACAS(7)+nVACAS(8)+nVACAS(9)+nVACAS(10)+nVACAS(11)+nVACAS(12)
		xCARGA(15)=xCARGA(1)+xCARGA(2)+xCARGA(3)+xCARGA(4)+xCARGA(5)+xCARGA(6)+xCARGA(7)+xCARGA(8)+xCARGA(9)+xCARGA(10)+xCARGA(11)+xCARGA(12)

ENDSCAN
Set Filter To

Create Cursor REPORTE (Concepto c(20),COL1 c(5),COL2 c(5),COL3 c(5),COL4 c(5),COL5 c(5),COL6 c(5))
Append Blank
Replace CONCEPTO With DTOC(DATE()-252),COL1 With Str(xVACAS(1),4),COL2 With Str(xDETEC(1),4),COL3 With Str(xDETEC(1)/xVACAS(1)*100,3),COL4 With Str(nVACAS(1),4),COL5 With Str(xCARGA(1),4),COL6 With Str(xCARGA(1)/nVACAS(1)*100,3)
Append Blank
Replace CONCEPTO With DTOC(DATE()-231),COL1 With Str(xVACAS(2),4),COL2 With Str(xDETEC(2),4),COL3 With Str(xDETEC(2)/xVACAS(2)*100,3),COL4 With Str(nVACAS(2),4),COL5 With Str(xCARGA(2),4),COL6 With Str(xCARGA(2)/nVACAS(2)*100,3)
Append Blank
Replace CONCEPTO With DTOC(DATE()-210),COL1 With Str(xVACAS(3),4),COL2 With Str(xDETEC(3),4),COL3 With Str(xDETEC(3)/xVACAS(3)*100,3),COL4 With Str(nVACAS(3),4),COL5 With Str(xCARGA(3),4),COL6 With Str(xCARGA(3)/nVACAS(3)*100,3)
Append Blank
Replace CONCEPTO With DTOC(DATE()-189),COL1 With Str(xVACAS(4),4),COL2 With Str(xDETEC(4),4),COL3 With Str(xDETEC(4)/xVACAS(4)*100,3),COL4 With Str(nVACAS(4),4),COL5 With Str(xCARGA(4),4),COL6 With Str(xCARGA(4)/nVACAS(4)*100,3)
Append Blank
Replace CONCEPTO With DTOC(DATE()-168),COL1 With Str(xVACAS(5),4),COL2 With Str(xDETEC(5),4),COL3 With Str(xDETEC(5)/xVACAS(5)*100,3),COL4 With Str(nVACAS(5),4),COL5 With Str(xCARGA(5),4),COL6 With Str(xCARGA(5)/nVACAS(5)*100,3)
Append Blank
Replace CONCEPTO With DTOC(DATE()-147),COL1 With Str(xVACAS(6),4),COL2 With Str(xDETEC(6),4),COL3 With Str(xDETEC(6)/xVACAS(7)*100,3),COL4 With Str(nVACAS(6),4),COL5 With Str(xCARGA(6),4),COL6 With Str(xCARGA(6)/nVACAS(6)*100,3)
Append Blank
Replace CONCEPTO With DTOC(DATE()-126),COL1 With Str(xVACAS(7),4),COL2 With Str(xDETEC(7),4),COL3 With Str(xDETEC(7)/xVACAS(7)*100,3),COL4 With Str(nVACAS(7),4),COL5 With Str(xCARGA(7),4),COL6 With Str(xCARGA(7)/nVACAS(7)*100,3)
Append Blank
Replace CONCEPTO With DTOC(DATE()-105),COL1 With Str(xVACAS(8),4),COL2 With Str(xDETEC(8),4),COL3 With Str(xDETEC(8)/xVACAS(8)*100,3),COL4 With Str(nVACAS(8),4),COL5 With Str(xCARGA(8),4),COL6 With Str(xCARGA(8)/nVACAS(8)*100,3)
Append Blank
Replace CONCEPTO With DTOC(DATE()- 84),COL1 With Str(xVACAS(9),4),COL2 With Str(xDETEC(9),4),COL3 With Str(xDETEC(9)/xVACAS(9)*100,3),COL4 With Str(nVACAS(9),4),COL5 With Str(xCARGA(9),4),COL6 With Str(xCARGA(9)/nVACAS(9)*100,3)
Append Blank
Replace CONCEPTO With DTOC(DATE()-63),COL1 With Str(xVACAS(10),4),COL2 With Str(xDETEC(10),4),COL3 With Str(xDETEC(10)/xVACAS(10)*100,3),COL4 With Str(nVACAS(10),4),COL5 With Str(xCARGA(10),4),COL6 With Str(xCARGA(10)/nVACAS(10)*100,3)
Append Blank
Replace CONCEPTO With DTOC(DATE()-42),COL1 With Str(xVACAS(11),4),COL2 With Str(xDETEC(11),4),COL3 With IIF(xDETEC(11)>0,Str(xDETEC(11)/xVACAS(11)*100,3),'  0'),COL4 With Str(nVACAS(11),4),COL5 With Str(xCARGA(11),4),COL6 With IIF(nVACAS(11)>0,Str(xCARGA(11)/nVACAS(11)*100,3),'  0')
Append Blank
Replace CONCEPTO With DTOC(DATE()-21),COL1 With Str(xVACAS(12),4),COL2 With Str(xDETEC(12),4),COL3 With IIF(xDETEC(12)>0,Str(xDETEC(12)/xVACAS(12)*100,3),'  0'),COL4 With Str(nVACAS(12),4),COL5 With Str(xCARGA(12),4),COL6 With IIF(nVACAS(12)>0,Str(xCARGA(12)/nVACAS(12)*100,3),'  0')
Append Blank
Append Blank
Replace CONCEPTO With 'Total',COL1 With Str(xVACAS(15),5),COL2 With Str(xDETEC(15),4),COL3 With IIF(xDETEC(15)>0,Str(xDETEC(15)/xVACAS(15)*100,3),'  0'),COL4 With Str(nVACAS(15),5),COL5 With Str(xCARGA(15),4),COL6 With IIF(nVACAS(15)>0,Str(xCARGA(15)/nVACAS(15)*100,3),'  0')
*Browse
Select REG
Set Filter To
Set Relation To
RETURN


PROCEDURE RV1065
Select REG
Set Order To 2
Set Filter To NP>0 And FB2=b And FSEC=B

Dimension aw(40),bw(30),cw(30),dw(30),ew(30)
aw=0
bw=0
cw=0
dw=0
ew=0

SCAN
xx=(date()-fpar)-(date()-fsec)
dg=date()-ucal

**-------------------------------------*
aw(1)=aw(1)+1
	if stat=[CARGA]
		aw(38)=aw(38)+1
	endi
		do case 
			case xx>=1 and xx<=30
				aw(2)=aw(2)+1
				if prm>0
					bw(2)=bw(2)+prm
					ew(2)=ew(2)+1
				endi
				if stat=[CARGA]
					cw(2)=cw(2)+1
					dw(2)=dw(2)+dg
				endi
					aw(14)=bw(2)/ew(2)
					if dw(2)>0
					aw(15)=dw(2)/cw(2)
					else
					aw(15)=0
					endi

			case xx>30 and xx<=60
				aw(3)=aw(3)+1
				if prm>0
					bw(3)=bw(3)+prm
					ew(3)=ew(3)+1
				endi
				if stat=[CARGA]
					cw(3)=cw(3)+1
					dw(3)=dw(3)+dg
				endi
					aw(16)=bw(3)/ew(3)
					if dw(3)>0
					aw(17)=dw(3)/cw(3)
					else
					aw(17)=0
					endi


			case xx>60 and xx<=90
				aw(4)=aw(4)+1
				if prm>0
					bw(4)=bw(4)+prm
					ew(4)=ew(4)+1
				endi
				if stat=[CARGA]
					cw(4)=cw(4)+1
					dw(4)=dw(4)+dg
				endi
					aw(18)=bw(4)/ew(4)
					if dw(4)>0
					aw(19)=dw(4)/cw(4)
					else
					aw(19)=0
					endi

			case xx>90 and xx<=120
				aw(5)=aw(5)+1
				if prm>0
					bw(5)=bw(5)+prm
					ew(5)=ew(5)+1
				endi
				if stat=[CARGA]
					cw(5)=cw(5)+1
					dw(5)=dw(5)+dg
				endi
					aw(20)=bw(5)/ew(5)
					if dw(5)>0
					aw(21)=dw(5)/cw(5)
					else
					aw(21)=0
					endi

			case xx>120 and xx<=150
				aw(6)=aw(6)+1
				if prm>0
					bw(6)=bw(6)+prm
					ew(6)=ew(6)+1
				endi
				if stat=[CARGA]
					cw(6)=cw(6)+1
					dw(6)=dw(6)+dg
				endi
					aw(22)=bw(6)/ew(6)
					aw(23)=dw(6)/cw(6)

			case xx>150 and xx<=180
				aw(7)=aw(7)+1
				if prm>0
					bw(7)=bw(7)+prm
					ew(7)=ew(7)+1
				endi
				if stat=[CARGA]
					cw(7)=cw(7)+1
					dw(7)=dw(7)+dg
				endi
					aw(24)=bw(7)/ew(7)
					aw(25)=dw(7)/cw(7)

			case xx>180 and xx<=210
				aw(8)=aw(8)+1
				if prm>0
					bw(8)=bw(8)+prm
					ew(8)=ew(8)+1
				endi
				if stat=[CARGA]
					cw(8)=cw(8)+1
					dw(8)=dw(8)+dg
				endi
					aw(26)=bw(8)/ew(8)
					aw(27)=dw(8)/cw(8)

			case xx>210 and xx<=240
				aw(9)=aw(9)+1
				if prm>0
					bw(9)=bw(9)+prm
					ew(9)=ew(9)+1
				endi
				if stat=[CARGA]
					cw(9)=cw(9)+1
					dw(9)=dw(9)+dg
				endi
					aw(28)=bw(9)/ew(9)
					aw(29)=dw(9)/cw(9)

			case xx>240 and xx<=270
				aw(10)=aw(10)+1
				if prm>0
					bw(10)=bw(10)+prm
					ew(10)=ew(10)+1
				endi
				if stat=[CARGA]
					cw(10)=cw(10)+1
					dw(10)=dw(10)+dg
				endi
					aw(30)=bw(10)/ew(10)
					aw(31)=dw(10)/cw(10)

			case xx>270 and xx<=300
				aw(11)=aw(11)+1
				if prm>0
					bw(11)=bw(11)+prm
					ew(11)=ew(11)+1
				endi
				if stat=[CARGA]
					cw(11)=cw(11)+1
					dw(11)=dw(11)+dg
				endi
					aw(32)=bw(11)/ew(11)
					aw(33)=dw(11)/cw(11)

			case xx>300 and xx<=330
				aw(12)=aw(12)+1
				if prm>0
					bw(12)=bw(12)+prm
					ew(12)=ew(12)+1
				endi
				if stat=[CARGA]
					cw(12)=cw(12)+1
					dw(12)=dw(12)+dg
				endi
					aw(34)=bw(12)/ew(12)
					aw(35)=dw(12)/cw(12)

			case xx>330
				aw(13)=aw(13)+1
				if prm>0
					bw(13)=bw(13)+prm
					ew(13)=ew(13)+1
				endi
				if stat=[CARGA]
					cw(13)=cw(13)+1
					dw(13)=dw(13)+dg
				endi
					aw(36)=bw(13)/ew(13)
					aw(37)=dw(13)/cw(13)
		endcase		
**-------------------------------------*
ENDSCAN
Set Filter To

Create Cursor REPORTE (Concepto c(20),COL1 c(5),COL2 c(5),COL3 c(5),COL4 c(5))
Append Blank
Replace CONCEPTO With ' 0 -  30',COL1 With Str(aw(2),4),COL2 With Str(aw(14),4,1),COL3 With Str(cw(2),4),COL4 With Str(aw(15),4)
Append Blank
Replace CONCEPTO With '31 -  60',COL1 With Str(aw(3),4),COL2 With Str(aw(16),4,1),COL3 With Str(cw(3),4),COL4 With Str(aw(17),4)
Append Blank
Replace CONCEPTO With '61 -  90',COL1 With Str(aw(4),4),COL2 With Str(aw(18),4,1),COL3 With Str(cw(4),4),COL4 With Str(aw(19),4)
Append Blank
Replace CONCEPTO With '91 - 120',COL1 With Str(aw(5),4),COL2 With Str(aw(20),4,1),COL3 With Str(cw(5),4),COL4 With Str(aw(21),4)
Append Blank
Replace CONCEPTO With '121 - 150',COL1 With Str(aw(6),4),COL2 With Str(aw(22),4,1),COL3 With Str(cw(6),4),COL4 With Str(aw(23),4)
Append Blank
Replace CONCEPTO With '151 - 180',COL1 With Str(aw(7),4),COL2 With Str(aw(24),4,1),COL3 With Str(cw(7),4),COL4 With Str(aw(25),4)
Append Blank
Replace CONCEPTO With '181 - 210',COL1 With Str(aw(8),4),COL2 With Str(aw(26),4,1),COL3 With Str(cw(8),4),COL4 With Str(aw(27),4)
Append Blank
Replace CONCEPTO With '211 - 240',COL1 With Str(aw(9),4),COL2 With Str(aw(28),4,1),COL3 With Str(cw(9),4),COL4 With Str(aw(29),4)
Append Blank
Replace CONCEPTO With '241 - 270',COL1 With Str(aw(10),4),COL2 With Str(aw(30),4,1),COL3 With Str(cw(10),4),COL4 With Str(aw(31),4)
Append Blank
Replace CONCEPTO With '271 - 300',COL1 With Str(aw(11),4),COL2 With Str(aw(32),4,1),COL3 With Str(cw(11),4),COL4 With Str(aw(33),4)
Append Blank
Replace CONCEPTO With '301 - 330',COL1 With Str(aw(12),4),COL2 With Str(aw(34),4,1),COL3 With Str(cw(12),4),COL4 With Str(aw(35),4)
Append Blank
Replace CONCEPTO With '331  - > ',COL1 With Str(aw(13),4),COL2 With Str(aw(36),4,1),COL3 With Str(cw(13),4),COL4 With Str(aw(37),4)
Append Blank
Append Blank
Replace CONCEPTO With 'Total',COL1 With Str(aw(1),4),COL3 With Str(aw(38),4)

Release All Like aw*,bw*,cw*,dw,ew*
RETURN

PROC RV10158
*-----------
DIMENSION XS(30),TG(15),TI(15),MES(12)
xs=0
TG=0
TI=0
mes=1

Select REG 
Calculate CNT() To TV For NP>0 And FB2=B
Calculate CNT() To TC For NP=0 And FB2=B

*Select CALOR
If REPORTES.NUM=158
	SELECT GNFERT
	Set Filter To NP>0 and STAT="CARGA"
	xFilter="NP>0"
Else
	SELECT CALOR
	Set Filter To NP=0 and STAT="CARGA"
	xFilter="NP=0"
EndIf

Calculate MAX(fecha) To xfecha
xfecha=xfecha-Q7

IF REPORTES.NUM=158
Select ID,FECHA2,SER,STAT From GNFERT Where YEAR(fecha2)=xan And ser>0 And fecha2<=xfecha And fecha2#B and (Clave=5 Or Clave=6) And &xFilter Into Table xDATOS
Else
Select ID,FECHA2,SER,STAT From CALOR  Where YEAR(fecha2)=xan And ser>0 And fecha2<=xfecha And fecha2#B and (Clave=5 Or Clave=6) And &xFilter Into Table xDATOS
ENDIF

SCAN
DO CASE
			Case month(fecha2)=1 
				If left(stat,1)="I"
					xs(1)=xs(1)+1
				endi
				if left(stat,1)="C"
					xs(2)=xs(2)+1
				endi
				TI(1)=(xs(1)/TV)*100
				TG(1)=(xs(2)/TV)*100

			case month(fecha2)=2 
				if left(stat,1)="I"
					xs(3)=xs(3)+1
				endi
				if left(stat,1)="C"
					xs(4)=xs(4)+1
				endi
				TI(2)=(xs(3)/TV)*100
				TG(2)=(xs(4)/TV)*100

			case month(fecha2)=3
				if left(stat,1)="I"
					xs(5)=xs(5)+1
				endi
				if left(stat,1)="C"
					xs(6)=xs(6)+1
				endi
				TI(3)=(xs(5)/TV)*100
				TG(3)=(xs(6)/TV)*100

			case month(fecha2)=4
				if left(stat,1)="I"
					xs(7)=xs(7)+1
				endi
				if left(stat,1)="C"
					xs(8)=xs(8)+1
				endi
				TI(4)=(xs(7)/TV)*100
				TG(4)=(xs(8)/TV)*100

			case month(fecha2)=5
				if left(stat,1)="I"
					xs(9)=xs(9)+1
				endi
				if left(stat,1)="C"
					xs(10)=xs(10)+1
				endi
				TI(5)=(xs(9)/TV)*100
				TG(5)=(xs(10)/TV)*100

			case month(fecha2)=6
				if left(stat,1)="I"
					xs(11)=xs(11)+1
				endi
				if left(stat,1)="C"
					xs(12)=xs(12)+1
				endi
				TI(6)=(xs(11)/TV)*100
				TG(6)=(xs(12)/TV)*100

			case month(fecha2)=7
				if left(stat,1)="I"
					xs(13)=xs(13)+1
				endi
				if left(stat,1)="C"
					xs(14)=xs(14)+1
				endi
				TI(7)=(xs(13)/TV)*100
				TG(7)=(xs(14)/TV)*100

			case month(fecha2)=8
				if left(stat,1)="I"
					xs(15)=xs(15)+1
				endi
				if left(stat,1)="C"
					xs(16)=xs(16)+1
				endi
				TI(8)=(xs(15)/TV)*100
				TG(8)=(xs(16)/TV)*100

			case month(fecha2)=9
				if left(stat,1)="I"
					xs(17)=xs(17)+1
				endi
				if left(stat,1)="C"
					xs(18)=xs(18)+1
				endi
				TI(9)=(xs(17)/TV)*100
				TG(9)=(xs(18)/TV)*100

			case month(fecha2)=10 
				if left(stat,1)="I"
					xs(19)=xs(19)+1
				endi
				if left(stat,1)="C"
					xs(20)=xs(20)+1
				endi
				TI(10)=(xs(19)/TV)*100
				TG(10)=(xs(20)/TV)*100

			Case month(fecha2)=11
				if left(stat,1)="I"
					xs(21)=xs(21)+1
				endi
				if left(stat,1)="C"
					xs(22)=xs(22)+1
				endi
				TI(11)=(xs(21)/TV)*100
				TG(11)=(xs(22)/TV)*100

			Case month(fecha2)=12
				if left(stat,1)="I"
					xs(23)=xs(23)+1
				endi
				if left(stat,1)="C"
					xs(24)=xs(24)+1
				endi
				TI(12)=(xs(23)/TV)*100
				TG(12)=(xs(24)/TV)*100

		Endcase
		* Total
		* -----
				If Left(Stat,1)="I"
					xs(25)=xs(25)+1
				Endif
				If left(stat,1)="C"
					xs(26)=xs(26)+1
				Endif
				TI(13)=(xs(25)/TV)*100
				TG(14)=(xs(26)/TV)*100
	
ENDSCAN
Set Filter To
xMAXIMA=XS(26)

Create Cursor REPORTE (Concepto c(20),COL1 c(5),COL2 c(5),COL3 c(5),COL4 c(5),COL5 c(5),COL6 c(5))
Append Blank
Replace CONCEPTO With 'Mes'
Append Blank
Replace CONCEPTO With 'Enero'     ,COL1 With Str(XS(1),4),COL2 With Str(TI(1),5,1),COL3 With Str(XS(2),3),COL4 With Str(TG(1),5,1)
Append Blank
Replace CONCEPTO With 'Febrero'   ,COL1 With Str(XS(3),4),COL2 With Str(TI(2),5,1),COL3 With Str(XS(4),3),COL4 With Str(TG(2),5,1)
Append Blank
Replace CONCEPTO With 'Marzo'     ,COL1 With Str(XS(5),4),COL2 With Str(TI(3),5,1),COL3 With Str(XS(6),3),COL4 With Str(TG(3),5,1)
Append Blank
Replace CONCEPTO With 'Abril'     ,COL1 With Str(XS(7),4),COL2 With Str(TI(4),5,1),COL3 With Str(XS(8),3),COL4 With Str(TG(4),5,1)
Append Blank
Replace CONCEPTO With 'Mayo'      ,COL1 With Str(XS(9),4),COL2 With Str(TI(5),5,1),COL3 With Str(XS(10),3),COL4 With Str(TG(5),5,1)
Append Blank
Replace CONCEPTO With 'Junio'     ,COL1 With Str(XS(11),4),COL2 With Str(TI(6),5,1),COL3 With Str(XS(12),3),COL4 With Str(TG(6),5,1)
Append Blank
Replace CONCEPTO With 'Julio'     ,COL1 With Str(XS(13),4),COL2 With Str(TI(7),5,1),COL3 With Str(XS(14),3),COL4 With Str(TG(7),5,1)
Append Blank
Replace CONCEPTO With 'Agosto'    ,COL1 With Str(XS(15),4),COL2 With Str(TI(8),5,1),COL3 With Str(XS(16),3),COL4 With Str(TG(8),5,1)
Append Blank
Replace CONCEPTO With 'Septiembre',COL1 With Str(XS(17),4),COL2 With Str(TI(9),5,1),COL3 With Str(XS(18),3),COL4 With Str(TG(9),5,1)
Append Blank
Replace CONCEPTO With 'Octubre'   ,COL1 With Str(XS(19),4),COL2 With Str(TI(10),5,1),COL3 With Str(XS(20),3),COL4 With Str(TG(10),5,1)
Append Blank
Replace CONCEPTO With 'Noviembre' ,COL1 With Str(XS(21),4),COL2 With Str(TI(11),5,1),COL3 With Str(XS(22),3),COL4 With Str(TG(11),5,1)
Append Blank
Replace CONCEPTO With 'Diciembre' ,COL1 With Str(XS(23),4),COL2 With Str(TI(12),5,1),COL3 With Str(XS(24),3),COL4 With Str(TG(12),5,1)
Append Blank
Replace CONCEPTO With ''
Append Blank
Replace CONCEPTO With 'Total     ',COL1 With Str(XS(25),5),COL2 With Str(TI(13),5,1),COL3 With Str(XS(26),5),COL4 With Str(TG(14),5,1)

GO TOP
RETURN

PROCEDURE RV10159
* Partos
*----------------
DIMENSION XS(60),TI(60),MES(12)
xs=0
TG=0
TI=0
mes=1

Select REG 
Calculate CNT() For NP>0 And FB2=B TO TV

Set Filter To NP>0 And Year(FPAR)=Year(Date()) And (FB2=B Or YEAR(FB2)=YEAR(DATE()))
GO TOP

SCAN
DO CASE

CASE NP=1
xs(51)=xs(51)+1
Do Case
			Case month(FPAR)=1 
					xs(1)=xs(1)+1
	
			case month(FPAR)=2 
					xs(2)=xs(2)+1
	
			case month(FPAR)=3
					xs(3)=xs(3)+1
	
			case month(FPAR)=4
					xs(4)=xs(4)+1
	
			case month(FPAR)=5
					xs(5)=xs(5)+1
	
			case month(FPAR)=6
					xs(6)=xs(6)+1
	
			case month(FPAR)=7
					xs(7)=xs(7)+1
	
			case month(FPAR)=8
					xs(8)=xs(8)+1
	
			case month(FPAR)=9
					xs(9)=xs(9)+1
	
			case month(FPAR)=10 
					xs(10)=xs(10)+1
	
			Case month(FPAR)=11
					xs(11)=xs(11)+1
	
			Case month(FPAR)=12
					xs(12)=xs(12)+1
	
		Endcase

CASE NP=2
xs(52)=xs(52)+1
Do Case
			Case month(FPAR)=1 
					xs(13)=xs(13)+1
	
			case month(FPAR)=2 
					xs(14)=xs(14)+1
	
			case month(FPAR)=3
					xs(15)=xs(15)+1
	
			case month(FPAR)=4
					xs(16)=xs(16)+1
	
			case month(FPAR)=5
					xs(17)=xs(17)+1
	
			case month(FPAR)=6
					xs(18)=xs(18)+1
	
			case month(FPAR)=7
					xs(19)=xs(19)+1
				
			case month(FPAR)=8
					xs(20)=xs(20)+1
	
			case month(FPAR)=9
					xs(21)=xs(21)+1
	
			case month(FPAR)=10 
					xs(22)=xs(22)+1
	
			Case month(FPAR)=11
					xs(23)=xs(23)+1
	
			Case month(FPAR)=12
					xs(24)=xs(24)+1
	
		Endcase


CASE NP>=3
xs(53)=xs(53)+1
Do Case
			Case month(FPAR)=1 
					xs(25)=xs(25)+1
	
			case month(FPAR)=2 
					xs(26)=xs(26)+1
	
			case month(FPAR)=3
					xs(27)=xs(27)+1
	
			case month(FPAR)=4
					xs(28)=xs(28)+1
	
			case month(FPAR)=5
					xs(29)=xs(29)+1
	
			case month(FPAR)=6
					xs(30)=xs(30)+1
	
			case month(FPAR)=7
					xs(31)=xs(31)+1
	
			case month(FPAR)=8
					xs(32)=xs(32)+1
	
			case month(FPAR)=9
					xs(33)=xs(33)+1
	
			case month(FPAR)=10 
					xs(34)=xs(34)+1
	
			Case month(FPAR)=11
					xs(35)=xs(35)+1
	
			Case month(FPAR)=12
					xs(36)=xs(36)+1
	
		Endcase

ENDCASE		


* Total de Partos
* ---------------
Do Case
			Case month(FPAR)=1 
					xs(37)=xs(37)+1
					TI(1)=xs(37)/(TV-xs(1))*100
											
			case month(FPAR)=2 
					xs(38)=xs(38)+1
					TI(2)=xs(38)/(TV-xs(2))*100
					
			case month(FPAR)=3
					xs(39)=xs(39)+1
					TI(3)=xs(39)/(TV-xs(3))*100
	
			case month(FPAR)=4
					xs(40)=xs(40)+1
					TI(4)=xs(40)/(TV-xs(4))*100
	
			case month(FPAR)=5
					xs(41)=xs(41)+1
					TI(5)=xs(41)/(TV-xs(5))*100

			case month(FPAR)=6
					xs(42)=xs(42)+1
					TI(6)=xs(42)/(TV-xs(6))*100
	
			case month(FPAR)=7
					xs(43)=xs(43)+1
					TI(7)=xs(43)/(TV-xs(7))*100
	
			case month(FPAR)=8
					xs(44)=xs(44)+1
					TI(8)=xs(44)/(TV-xs(8))*100
	
			case month(FPAR)=9 
					xs(45)=xs(45)+1
					TI(9)=xs(45)/(TV-xs(9))*100
	
			Case month(FPAR)=10
					xs(46)=xs(46)+1
					TI(10)=xs(46)/(TV-xs(10))*100
	
			Case month(FPAR)=11
					xs(47)=xs(47)+1
					TI(11)=xs(47)/(TV-xs(11))*100
	
			Case month(FPAR)=12
					xs(48)=xs(48)+1
					TI(12)=xs(48)/(TV-xs(12))*100

		Endcase
		xs(50)=xs(50)+1
ENDSCAN
Set Filter To

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
Replace CONCEPTO With 'Total     ',COL1 With Str(XS(51),4),COL2 With Str(XS(52),4),COL3 With Str(XS(53),4),COL4 With Str(XS(50),4),COL5 With Str((XS(50)/TV)*100,5,1)
GO TOP
RETURN


PROC RV1028
elim=xDESECHO

dimension mes(12),p1(12),s1(12),tpr(12),tsc(12),tot(12),tel(12),p2(12)
	mes=0
	p1=0
	s1=0
	tpr=0
	tsc=0
	tot=0
	tel=0
	p2=0
	tsec=0
	tpro=0
	total=0
	meses=[EneFebMarAbrMayJunJulAgoSepOctNovDic]

Select REG
Set Order To 1
GO TOP
Set Filter To FB2=B 

SCAN
DO CASE
CASE NP>0 
	** Calculo de Vientres en produccion y secas
	** ------------------------------
	if fsec=b
		tpro=tpro+1
	else
		tsec=tsec+1
	endi
	** Calculo de Partos y Secas
	** -------------------------
	if stat=[INSEM] .or. stat=[CARGA] 
		repl fps with ucal+219,fpp with ucal+Q39
		xx=month(fpp)
		xy=month(fps)
		
		do case
			case xx=1
				p1(1)=p1(1)+1
			case xx=2
				p1(2)=p1(2)+1
			case xx=3
				p1(3)=p1(3)+1
			case xx=4
				p1(4)=p1(4)+1
			case xx=5
				p1(5)=p1(5)+1
			case xx=6
				p1(6)=p1(6)+1
			case xx=7
				p1(7)=p1(7)+1
			case xx=8
				p1(8)=p1(8)+1
			case xx=9
				p1(9)=p1(9)+1
			case xx=10
				p1(10)=p1(10)+1
			case xx=11
				p1(11)=p1(11)+1
			case xx=12
				p1(12)=p1(12)+1
		
		endcase	

		if fsec=b

		do case
			case xy=1
			s1(1)=s1(1)+1
			case xy=2
			s1(2)=s1(2)+1
			case xy=3
			s1(3)=s1(3)+1
			case xy=4
			s1(4)=s1(4)+1
			case xy=5
			s1(5)=s1(5)+1
			case xy=6
			s1(6)=s1(6)+1
			case xy=7
			s1(7)=s1(7)+1
			case xy=8
			s1(8)=s1(8)+1
			case xy=9
			s1(9)=s1(9)+1
			case xy=10
			s1(10)=s1(10)+1
			case xy=11
			s1(11)=s1(11)+1
			case xy=12
			s1(12)=s1(12)+1
		endcase	
endi

	endi

CASE NP=0 
	if stat=[CARGA] .and. UCAL#B .or. stat=[INSEM] .and. UCAL#B
	repl fpp with ucal+Q39
	xv=MONTH(fpp)

		do case
			case xv=1
				p2(1)=p2(1)+1
			case xv=2
				p2(2)=p2(2)+1
			case xv=3
				p2(3)=p2(3)+1
			case xv=4
				p2(4)=p2(4)+1
			case xv=5
				p2(5)=p2(5)+1
			case xv=6
				p2(6)=p2(6)+1
			case xv=7
				p2(7)=p2(7)+1
			case xv=8
				p2(8)=p2(8)+1
			case xv=9
				p2(9)=p2(9)+1
			case xv=10
				p2(10)=p2(10)+1
			case xv=11
				p2(11)=p2(11)+1
			case xv=12
				p2(12)=p2(12)+1
		
		endcase	
	endi	

ENDCASE

	mes(1)=month(date())
		if mes(1)>12
			mes(1)=mes(1)-12
		endi
	mes(2)=mes(1)+1
		if mes(2)>12
			mes(2)=mes(2)-12
		endi
	mes(3)=mes(1)+2
		if mes(3)>12
			mes(3)=mes(3)-12
		endi
	mes(4)=mes(1)+3
		if mes(4)>12
			mes(4)=mes(4)-12
		endi
	mes(5)=mes(1)+4
		if mes(5)>12
			mes(5)=mes(5)-12
		endi
	mes(6)=mes(1)+5
		if mes(6)>12
			mes(6)=mes(6)-12
		endi
		
ENDSCAN

tpr(1)=tpro-s1(mes(1))+p1(mes(1))+p2(mes(1))
tsc(1)=tsec+s1(mes(1))-p1(mes(1))
tot(1)=tsc(1)+tpr(1) 
tel(1)=INT(tot(1)*elim/12)
tpr(1)=tpro-s1(mes(1))+p1(mes(1))+p2(mes(1))-tel(1)
tot(1)=tsc(1)+tpr(1) 
*
tpr(2)=tpr(1)-s1(mes(2))+p1(mes(2))+p2(mes(2))
tsc(2)=tsc(1)+s1(mes(2))-p1(mes(2))
tot(2)=tsc(2)+tpr(2) 
tel(2)=INT(tot(2)*elim/12)
tpr(2)=tpr(1)-s1(mes(2))+p1(mes(2))+p2(mes(2))-tel(2)
tot(2)=tsc(2)+tpr(2)
*
tpr(3)=tpr(2)-s1(mes(3))+p1(mes(3))+p2(mes(3))
tsc(3)=tsc(2)+s1(mes(3))-p1(mes(3))
tot(3)=tsc(3)+tpr(3) 
tel(3)=INT(tot(3)*elim/12)
tpr(3)=tpr(2)-s1(mes(3))+p1(mes(3))+p2(mes(3))-tel(3)
tot(3)=tsc(3)+tpr(3)
*
tpr(4)=tpr(3)-s1(mes(4))+p1(mes(4))+p2(mes(4))
tsc(4)=tsc(3)+s1(mes(4))-p1(mes(4))
tot(4)=tsc(4)+tpr(4) 
tel(4)=INT(tot(4)*elim/12)
tpr(4)=tpr(3)-s1(mes(4))+p1(mes(4))+p2(mes(4))-tel(4)
tot(4)=tsc(4)+tpr(4)
*
tpr(5)=tpr(4)-s1(mes(5))+p1(mes(5))+p2(mes(5))
tsc(5)=tsc(4)+s1(mes(5))-p1(mes(5))
tot(5)=tsc(5)+tpr(5) 
tel(5)=INT(tot(5)*elim/12)
tpr(5)=tpr(4)-s1(mes(5))+p1(mes(5))+p2(mes(5))-tel(5)
tot(5)=tsc(5)+tpr(5)
*
tpr(6)=tpr(5)-s1(mes(6))+p1(mes(6))+p2(mes(6))
tsc(6)=tsc(5)+s1(mes(6))-p1(mes(6))
tot(6)=tsc(6)+tpr(6) 
tel(6)=INT(tot(6)*elim/12)
tpr(6)=tpr(5)-s1(mes(6))+p1(mes(6))+p2(mes(6))-tel(6)
tot(6)=tsc(6)+tpr(6)
*


Create Cursor REPORTE (Concepto c(20),COL1 c(5),COL2 c(5),COL3 c(5),COL4 c(5),COL5 c(5),COL6 c(5),COL7 c(5),COL8 c(5),COL9 c(5))
Append Blank
Replace CONCEPTO With "Inventario Actual",COL5 With Str(tpro,4),COL6 With Str(tsec,4),COL7 With Str(tpro+tsec,4),COL8 With Str(tpro/(tpro+tsec)*100,5,1)

xx=1
Do While xx<7
	Do Case
		Case mes(xx)=1
			xMES="Enero"
		Case mes(xx)=2
			xMES="Febrero"
		Case mes(xx)=3
			xMES="Marzo"
		Case mes(xx)=4
			xMES="Abril"
		Case mes(xx)=5
			xMES="Mayo"
		Case mes(xx)=6
			xMES="Junio"
		Case mes(xx)=7
			xMES="Julio"
		Case mes(xx)=8
			xMES="Agosto"
		Case mes(xx)=9
			xMES="Septiembre"
		Case mes(xx)=10
			xMES="Octubre"
		Case mes(xx)=11
			xMES="Noviembre"
		Case mes(xx)=12
			xMES="Diciembre"
	EndCase

	Append Blank
	Replace CONCEPTO With xMES,COL1 With Str(p1(mes(xx)),4),COL2 With Str(p2(mes(xx)),4),;
			COL3 With Str(s1(mes(xx)),4),COL4 With Str(tel(xx),4),COL5 With Str(tpr(xx),4),;
			COL6 With Str(tsc(xx),4),COL7 With Str(tot(xx),4),COL8 With Str(tpr(xx)/tot(xx)*100,5,1)
	xx=xx+1
Enddo

GO TOP
RETURN


* Proyeccion de Partos (CRIANZA)
* ------------------------------
PROCEDURE RV10120
Create Cursor REPORTE (Concepto c(20),COL1 c(5),COL2 c(5))

	Select REG
	Set Order To 3
	Replace All FPP With UCAL+Q39 For (STAT='INSEM' Or STAT='CARGA')
	Set Filter To (DATE()-FNAC)/30.4>=Q26
	Calculate CNT() TO NVAQ
	Set Filter To
	
	Select ID,UCAL,FPP,STAT From REG Where NP=0 And YEAR(FPP)=XAN And (STAT='INSEM' Or STAT='CARGA') Order By FPP Into Table XPARTOS

	DIMENSION MES(12),XPOR(12)
	X=1
	I=1
	nmes=0
	npor=0
	XMES=1

	DO WHILE XMES<13
		If XMES>12 And XAN=YEAR(XAN)+1
			EXIT
		Endif 

	Calculate CNT() FOR MONTH(FPP)=xmes TO MES(x)
	xpor(i)=mes(x)/NVAQ*100

	nmes=nmes+mes(x)
	npor=npor+xpor(i)

	Do Case
		Case XMES=1
			cMES="Enero"
		Case XMES=2
			cMES="Febrero"
		Case XMES=3
			cMES="Marzo"
		Case XMES=4
			cMES="Abril"
		Case XMES=5
			cMES="Mayo"
		Case XMES=6
			cMES="Junio"
		Case XMES=7
			cMES="Julio"
		Case XMES=8
			cMES="Agosto"
		Case XMES=9
			cMES="Septiembre"
		Case XMES=10
			cMES="Octubre"
		Case XMES=11
			cMES="Noviembre"
		Case XMES=12
			cMES="Diciembre"
	EndCase
	
	SELECT REPORTE
	Append Blank
	Replace CONCEPTO With cMES,COL1 With Str(MES(X),4),COL2 With Str(xpor(i),5,1)
	Select XPARTOS
	XMES=XMES+1
	X=X+1
	I=I+1
ENDD
Select REPORTE
Append Blank
Replace CONCEPTO With ""
Append Blank
Replace CONCEPTO With "Total",COL1 With Str(nMES,4)
Go Top
RETURN


PROC RV10209
*-----------
DIMENSION xs(120),ts(60),mes(12)
xs=0
ts=0
mes=1

Select GNFERT
Set Filter To
Set Relation TO

If REPORTES.NUM=209
	xFilter="NP>0"
Else
	xFilter="NP=0"
EndIf

Calculate MAX(fecha) To xfecha
xfecha=xfecha-Q7

Select ID,FECHA2,SER,STAT From GNFERT Where YEAR(fecha2)=xan And ser>0 And fecha2<=xfecha And fecha2#B and (Clave=5 Or Clave=6) And &xFilter Into Table xDATOS

x=0
SCAN
x=x+1

DO CASE
	CASE ser=1
		do case 
			case month(fecha2)=1 
				if left(stat,1)="I"
					xs(1)=xs(1)+1
				endi
				if left(stat,1)="C"
					xs(2)=xs(2)+1
				endi
				ts(1)=(xs(2)/xs(1))*100

			case month(fecha2)=2 
				if left(stat,1)="I"
					xs(3)=xs(3)+1
				endi
				if left(stat,1)="C"
					xs(4)=xs(4)+1
				endi
				ts(2)=(xs(4)/xs(3))*100

			case month(fecha2)=3
				if left(stat,1)="I"
					xs(5)=xs(5)+1
				endi
				if left(stat,1)="C"
					xs(6)=xs(6)+1
				endi
				ts(3)=(xs(6)/xs(5))*100

			case month(fecha2)=4
				if left(stat,1)="I"
					xs(7)=xs(7)+1
				endi
				if left(stat,1)="C"
					xs(8)=xs(8)+1
				endi
				ts(4)=(xs(8)/xs(7))*100

			case month(fecha2)=5
				if left(stat,1)="I"
					xs(9)=xs(9)+1
				endi
				if left(stat,1)="C"
					xs(10)=xs(10)+1
				endi
				ts(5)=(xs(10)/xs(9))*100

			case month(fecha2)=6
				if left(stat,1)="I"
					xs(11)=xs(11)+1
				endi
				if left(stat,1)="C"
					xs(12)=xs(12)+1
				endi
				ts(6)=(xs(12)/xs(11))*100

			case month(fecha2)=7
				if left(stat,1)="I"
					xs(13)=xs(13)+1
				endi
				if left(stat,1)="C"
					xs(14)=xs(14)+1
				endi
				ts(7)=(xs(14)/xs(13))*100

			case month(fecha2)=8
				if left(stat,1)="I"
					xs(15)=xs(15)+1
				endi
				if left(stat,1)="C"
					xs(16)=xs(16)+1
				endi
				ts(8)=(xs(16)/xs(15))*100

			case month(fecha2)=9
				if left(stat,1)="I"
					xs(17)=xs(17)+1
				endi
				if left(stat,1)="C"
					xs(18)=xs(18)+1
				endi
				ts(9)=(xs(18)/xs(17))*100

			case month(fecha2)=10 
				if left(stat,1)="I"
					xs(19)=xs(19)+1
				endi
				if left(stat,1)="C"
					xs(20)=xs(20)+1
				endi
				ts(10)=(xs(20)/xs(19))*100

			case month(fecha2)=11
				if left(stat,1)="I"
					xs(21)=xs(21)+1
				endi
				if left(stat,1)="C"
					xs(22)=xs(22)+1
				endi
				ts(11)=(xs(22)/xs(21))*100

			case month(fecha2)=12
				if left(stat,1)="I"
					xs(23)=xs(23)+1
				endi
				if left(stat,1)="C"
					xs(24)=xs(24)+1
				endi
				ts(12)=(xs(24)/xs(23))*100

		endcase

	CASE ser=2
		do case 
			case month(fecha2)=1 
				if left(stat,1)="I"
					xs(25)=xs(25)+1
				endi
				if left(stat,1)="C"
					xs(26)=xs(26)+1
				endi
				ts(13)=(xs(26)/xs(25))*100

			case month(fecha2)=2 
				if left(stat,1)="I"
					xs(27)=xs(27)+1
				endi
				if left(stat,1)="C"
					xs(28)=xs(28)+1
				endi
				ts(14)=(xs(28)/xs(27))*100

			case month(fecha2)=3
				if left(stat,1)="I"
					xs(29)=xs(29)+1
				endi
				if left(stat,1)="C"
					xs(30)=xs(30)+1
				endi
				ts(15)=(xs(30)/xs(29))*100

			case month(fecha2)=4
				if left(stat,1)="I"
					xs(31)=xs(31)+1
				endi
				if left(stat,1)="C"
					xs(32)=xs(32)+1
				endi
				ts(16)=(xs(32)/xs(31))*100

			case month(fecha2)=5
				if left(stat,1)="I"
					xs(33)=xs(33)+1
				endi
				if left(stat,1)="C"
					xs(34)=xs(34)+1
				endi
				ts(17)=(xs(34)/xs(33))*100

			case month(fecha2)=6
				if left(stat,1)="I"
					xs(35)=xs(35)+1
				endi
				if left(stat,1)="C"
					xs(36)=xs(36)+1
				endi
				ts(18)=(xs(36)/xs(35))*100

			case month(fecha2)=7
				if left(stat,1)="I"
					xs(37)=xs(37)+1
				endi
				if left(stat,1)="C"
					xs(38)=xs(38)+1
				endi
				ts(19)=(xs(38)/xs(37))*100

			case month(fecha2)=8
				if left(stat,1)="I"
					xs(39)=xs(39)+1
				endi
				if left(stat,1)="C"
					xs(40)=xs(40)+1
				endi
				ts(20)=(xs(40)/xs(39))*100

			case month(fecha2)=9
				if left(stat,1)="I"
					xs(41)=xs(41)+1
				endi
				if left(stat,1)="C"
					xs(42)=xs(42)+1
				endi
				ts(21)=(xs(42)/xs(41))*100

			case month(fecha2)=10 
				if left(stat,1)="I"
					xs(43)=xs(43)+1
				endi
				if left(stat,1)="C"
					xs(44)=xs(44)+1
				endi
				ts(22)=(xs(44)/xs(43))*100

			case month(fecha2)=11
				if left(stat,1)="I"
					xs(45)=xs(45)+1
				endi
				if left(stat,1)="C"
					xs(46)=xs(46)+1
				endi
				ts(23)=(xs(46)/xs(45))*100

			case month(fecha2)=12
				if left(stat,1)="I"
					xs(47)=xs(47)+1
				endi
				if left(stat,1)="C"
					xs(48)=xs(48)+1
				endi
				ts(24)=(xs(48)/xs(47))*100

		endcase

	CASE ser>=3
		do case 
			case month(fecha2)=1 
				if left(stat,1)="I"
					xs(49)=xs(49)+1
				endi
				if left(stat,1)="C"
					xs(50)=xs(50)+1
				endi
				ts(25)=(xs(50)/xs(49))*100

			case month(fecha2)=2 
				if left(stat,1)="I"
					xs(51)=xs(51)+1
				endi
				if left(stat,1)="C"
					xs(52)=xs(52)+1
				endi
				ts(26)=(xs(52)/xs(51))*100

			case month(fecha2)=3
				if left(stat,1)="I"
					xs(53)=xs(53)+1
				endi
				if left(stat,1)="C"
					xs(54)=xs(54)+1
				endi
				ts(27)=(xs(54)/xs(53))*100

			case month(fecha2)=4
				if left(stat,1)="I"
					xs(55)=xs(55)+1
				endi
				if left(stat,1)="C"
					xs(56)=xs(56)+1
				endi
				ts(28)=(xs(56)/xs(55))*100

			case month(fecha2)=5
				if left(stat,1)="I"
					xs(57)=xs(57)+1
				endi
				if left(stat,1)="C"
					xs(58)=xs(58)+1
				endi
				ts(29)=(xs(58)/xs(57))*100

			case month(fecha2)=6
				if left(stat,1)="I"
					xs(59)=xs(59)+1
				endi
				if left(stat,1)="C"
					xs(60)=xs(60)+1
				endi
				ts(30)=(xs(60)/xs(59))*100

			case month(fecha2)=7
				if left(stat,1)="I"
					xs(61)=xs(61)+1
				endi
				if left(stat,1)="C"
					xs(62)=xs(62)+1
				endi
				ts(31)=(xs(62)/xs(61))*100

			case month(fecha2)=8
				if left(stat,1)="I"
					xs(63)=xs(63)+1
				endi
				if left(stat,1)="C"
					xs(64)=xs(64)+1
				endi
				ts(32)=(xs(64)/xs(63))*100

			case month(fecha2)=9
				if left(stat,1)="I"
					xs(65)=xs(65)+1
				endi
				if left(stat,1)="C"
					xs(66)=xs(66)+1
				endi
				ts(33)=(xs(66)/xs(65))*100

			case month(fecha2)=10 
				if left(stat,1)="I"
					xs(67)=xs(67)+1
				endi
				if left(stat,1)="C"
					xs(68)=xs(68)+1
				endi
				ts(34)=(xs(68)/xs(67))*100

			case month(fecha2)=11
				if left(stat,1)="I"
					xs(69)=xs(69)+1
				endi
				if left(stat,1)="C"
					xs(70)=xs(70)+1
				endi
				ts(35)=(xs(70)/xs(69))*100

			case month(fecha2)=12
				if left(stat,1)="I"
					xs(71)=xs(71)+1
				endi
				if left(stat,1)="C"
					xs(72)=xs(72)+1
				endi
				ts(36)=(xs(72)/xs(71))*100

		endcase
ENDCASE

** Todos los servicios
** -------------------

		do case 
			case month(fecha2)=1 
				if left(stat,1)="I"
					xs(73)=xs(73)+1
				endi
				if left(stat,1)="C"
					xs(74)=xs(74)+1
				endi
				ts(37)=(xs(74)/xs(73))*100

			case month(fecha2)=2 
				if left(stat,1)="I"
					xs(75)=xs(75)+1
				endi
				if left(stat,1)="C"
					xs(76)=xs(76)+1
				endi
				ts(38)=(xs(76)/xs(75))*100

			case month(fecha2)=3
				if left(stat,1)="I"
					xs(77)=xs(77)+1
				endi
				if left(stat,1)="C"
					xs(78)=xs(78)+1
				endi
				ts(39)=(xs(78)/xs(77))*100

			case month(fecha2)=4
				if left(stat,1)="I"
					xs(79)=xs(79)+1
				endi
				if left(stat,1)="C"
					xs(80)=xs(80)+1
				endi
				ts(40)=(xs(80)/xs(79))*100

			case month(fecha2)=5
				if left(stat,1)="I"
					xs(81)=xs(81)+1
				endi
				if left(stat,1)="C"
					xs(82)=xs(82)+1
				endi
				ts(41)=(xs(82)/xs(81))*100

			case month(fecha2)=6
				if left(stat,1)="I"
					xs(83)=xs(83)+1
				endi
				if left(stat,1)="C"
					xs(84)=xs(84)+1
				endi
				ts(42)=(xs(84)/xs(83))*100

			case month(fecha2)=7
				if left(stat,1)="I"
					xs(85)=xs(85)+1
				endi
				if left(stat,1)="C"
					xs(86)=xs(86)+1
				endi
				ts(43)=(xs(86)/xs(85))*100

			case month(fecha2)=8
				if left(stat,1)="I"
					xs(87)=xs(87)+1
				endi
				if left(stat,1)="C"
					xs(88)=xs(88)+1
				endi
				ts(44)=(xs(88)/xs(87))*100

			case month(fecha2)=9
				if left(stat,1)="I"
					xs(89)=xs(89)+1
				endi
				if left(stat,1)="C"
					xs(90)=xs(90)+1
				endi
				ts(45)=(xs(90)/xs(89))*100

			case month(fecha2)=10 
				if left(stat,1)="I"
					xs(91)=xs(91)+1
				endi
				if left(stat,1)="C"
					xs(92)=xs(92)+1
				endi
				ts(46)=(xs(92)/xs(91))*100

			case month(fecha2)=11
				if left(stat,1)="I"
					xs(93)=xs(93)+1
				endi
				if left(stat,1)="C"
					xs(94)=xs(94)+1
				endi
				ts(47)=(xs(94)/xs(93))*100

			case month(fecha2)=12
				if left(stat,1)="I"
					xs(95)=xs(95)+1
				endi
				if left(stat,1)="C"
					xs(96)=xs(96)+1
				endi
				ts(48)=(xs(96)/xs(95))*100

		endcase
    
       ** Calculo General
       ** --------------- 
		do case
			case ser=1
				if left(stat,1)="I"
					xs(97)=xs(97)+1
				endi
				if left(stat,1)="C"
					xs(98)=xs(98)+1
				endi
				ts(49)=(xs(98)/xs(97))*100

			case ser=2
				if left(stat,1)="I"
					xs(99)=xs(99)+1
				endi
				if left(stat,1)="C"
					xs(100)=xs(100)+1
				endi
				ts(50)=(xs(100)/xs(99))*100

			case ser>=3
				if left(stat,1)="I"
					xs(101)=xs(101)+1
				endi
				if left(stat,1)="C"
					xs(102)=xs(102)+1
				endi
				ts(51)=(xs(102)/xs(101))*100
			endcase
				
				** Total
				** -----
				if left(stat,1)="I"
					xs(103)=xs(103)+1
				endi
				if left(stat,1)="C"
					xs(104)=xs(104)+1
				endi
				ts(52)=(xs(104)/xs(103))*100
	
ENDSCAN
Set Filter To

Create Cursor REPORTE (Concepto c(20),COL1 c(5),COL2 c(5),COL3 c(5),COL4 c(5),COL5 c(5),COL6 c(5))
Append Blank
Replace CONCEPTO With 'Mes'
Append Blank
Replace CONCEPTO With 'Enero',COL1 With Str(TS(1),5,1),COL2 With Str(TS(13),5,1),COL3 With Str(TS(25),5,1),COL4 With Str(TS(37),5,1)
Append Blank
Replace CONCEPTO With 'Febrero',COL1 With Str(TS(2),5,1),COL2 With Str(TS(14),5,1),COL3 With Str(TS(26),5,1),COL4 With Str(TS(38),5,1)
Append Blank
Replace CONCEPTO With 'Marzo',COL1 With Str(TS(3),5,1),COL2 With Str(TS(15),5,1),COL3 With Str(TS(27),5,1),COL4 With Str(TS(39),5,1)
Append Blank
Replace CONCEPTO With 'Abril',COL1 With Str(TS(4),5,1),COL2 With Str(TS(16),5,1),COL3 With Str(TS(28),5,1),COL4 With Str(TS(40),5,1)
Append Blank
Replace CONCEPTO With 'Mayo',COL1 With Str(TS(5),5,1),COL2 With Str(TS(17),5,1),COL3 With Str(TS(29),5,1),COL4 With Str(TS(41),5,1)
Append Blank
Replace CONCEPTO With 'Junio',COL1 With Str(TS(6),5,1),COL2 With Str(TS(18),5,1),COL3 With Str(TS(30),5,1),COL4 With Str(TS(42),5,1)
Append Blank
Replace CONCEPTO With 'Julio',COL1 With Str(TS(7),5,1),COL2 With Str(TS(19),5,1),COL3 With Str(TS(31),5,1),COL4 With Str(TS(43),5,1)
Append Blank
Replace CONCEPTO With 'Agosto',COL1 With Str(TS(8),5,1),COL2 With Str(TS(20),5,1),COL3 With Str(TS(32),5,1),COL4 With Str(TS(44),5,1)
Append Blank
Replace CONCEPTO With 'Septiembre',COL1 With Str(TS(9),5,1),COL2 With Str(TS(21),5,1),COL3 With Str(TS(33),5,1),COL4 With Str(TS(45),5,1)
Append Blank
Replace CONCEPTO With 'Octubre',COL1 With Str(TS(10),5,1),COL2 With Str(TS(22),5,1),COL3 With Str(TS(34),5,1),COL4 With Str(TS(46),5,1)
Append Blank
Replace CONCEPTO With 'Noviembre',COL1 With Str(TS(11),5,1),COL2 With Str(TS(23),5,1),COL3 With Str(TS(35),5,1),COL4 With Str(TS(47),5,1)
Append Blank
Replace CONCEPTO With 'Diciembre',COL1 With Str(TS(12),5,1),COL2 With Str(TS(24),5,1),COL3 With Str(TS(36),5,1),COL4 With Str(TS(48),5,1)
Append Blank
Replace CONCEPTO With ''
Append Blank
Replace CONCEPTO With 'Promedio',COL1 With Str(TS(49),5,1),COL2 With Str(TS(50),5,1),COL3 With Str(TS(51),5,1),COL4 With Str(TS(52),5,1)
GO TOP
RETURN

* ANALISIS DE PICOS DE LACTANCIA 
* ------------------------------
PROCEDURE RV10195
Select REG
Set Order To 2
Set Filter To NP>0 And (Date()-FPAR)>=120 And FSEC=B And FB2=B

Dimension aw(50),sw(10)
aw=0
sw=0
dat=0

Scan
dat=dat+1
**-------------------------------------*
Do Case 
	* Lactancia 1
	* -----------
	Case np=1 and dpic>0
		aw(1)=aw(1)+1
		aw(2)=aw(2)+dpic
		aw(3)=aw(3)+plac
		aw(4)=aw(4)+p305
		aw(5)=aw(5)+(date()-fpar)

	* Lactancia 2
	* -----------
	Case np=2 and dpic>0
		aw(8)=aw(8)+1
		aw(9)=aw(9)+dpic
		aw(10)=aw(10)+plac
		aw(11)=aw(11)+p305
		aw(12)=aw(12)+(date()-fpar)

	** Lactancia 3+
	** ------------
	Case np>=3 and dpic>0
		aw(15)=aw(15)+1
		aw(16)=aw(16)+dpic
		aw(17)=aw(17)+plac
		aw(18)=aw(18)+p305
		aw(19)=aw(19)+(date()-fpar)
Endcase

	** Todas las Lactancias
	** --------------------
	If np>0 and dpic>0
		aw(23)=aw(23)+1
		aw(24)=aw(24)+dpic
		aw(25)=aw(25)+plac
		aw(26)=aw(26)+p305
		aw(27)=aw(27)+(date()-fpar)
	Endif
Endscan
Set Filter To

sw(1)=aw(3)/aw(1)
sw(2)=aw(10)/aw(8)
sw(3)=aw(17)/aw(15)
sw(4)=aw(25)/aw(23)

sw(5)=sw(1)/sw(3)*100
sw(6)=sw(2)/sw(3)*100
sw(7)=sw(3)/sw(3)*100

Create Cursor REPORTE (Concepto c(30),COL1 c(5),COL2 c(5),COL3 c(5),COL4 c(5),COL5 c(5),COL6 c(5))
Append Blank
Replace CONCEPTO With 'Vientres',COL1 With Str(AW(1),4),COL2 With Str(AW(8),4),COL3 With Str(AW(15),4),COL4 With Str(AW(23),4)
Append Blank
Replace CONCEPTO With 'Dias al Pico',COL1 With Str(AW(2)/AW(1),4),COL2 With Str(AW(9)/AW(8),4),COL3 With Str(AW(16)/AW(15),4),COL4 With Str(AW(24)/AW(23),4)
Append Blank
Replace CONCEPTO With 'Leche al Pico',COL1 With Str(SW(1),4,1),COL2 With Str(SW(2),5,1),COL3 With Str(SW(3),5,1),COL4 With Str(SW(4),5,1)
Append Blank
Replace CONCEPTO With 'Relacion vs Maduras',COL1 With Str(SW(5),5,1),COL2 With Str(SW(6),5,1),COL3 With Str(SW(7),5,1),COL4 With '-----'
Append Blank
Replace CONCEPTO With 'Proyeccion a 305 d',COL1 With Str(AW(4)/AW(1),5),COL2 With Str(AW(11)/AW(8),5),COL3 With Str(AW(18)/AW(15),5),COL4 With Str(AW(26)/AW(23),5)
Append Blank
Replace CONCEPTO With 'Promedio de Dias en Leche',COL1 With Str(AW(5)/AW(1),4),COL2 With Str(AW(12)/AW(8),4),COL3 With Str(AW(19)/AW(15),4),COL4 With Str(AW(27)/AW(23),4)
GO TOP
Return

* CENSO DE GANADO ACTUAL
* ----------------------
PROCEDURE RV10196
Declare a1(75)
xtot=0
xtot2=0
xtot3=0

a1=0

** Censo de Vacas
** --------------
Select REG
Set Order To 2
Set Filter To FB2=B
GO TOP

Scan
		** Vacas Activas y Vacas candidato Rastro
		** --------------------------------------
		xtot=xtot+1
			if fsec=B
				a1(1)=a1(1)+1
			else	
				a1(2)=a1(2)+1
			endi
			
			if stat=[CARGA] 
				a1(5)=a1(5)+1
			endi
		
			if stat=[FRESC]
				a1(3)=a1(3)+1
			endi		
			if stat=[INSEM]
				a1(4)=a1(4)+1
			endi
			if stat#[INSEM] and stat#[CARGA] and stat#[FRESC]
				a1(6)=a1(6)+1	
			endi		


			** Por Lactancia
			** -------------
			do case
				case np=1
					a1(9)=a1(9)+1
				case np=2
					a1(10)=a1(10)+1
				case np=3
					a1(11)=a1(11)+1
				case np=4
					a1(12)=a1(12)+1
				case np=5
					a1(13)=a1(13)+1
				case np=6
					a1(14)=a1(14)+1
				case np=7
					a1(15)=a1(15)+1
				case np=8
					a1(16)=a1(16)+1
				case np=9
					a1(17)=a1(17)+1
				case np>9
					a1(18)=a1(18)+1
			endcase	
endscan &&


Set Order To 3
GO TOP

Scan
xtot2=xtot2+1
	
	if date()-fnac<=(Q26*30.4) And (Stat#[INSEM] And Stat#[CARGA])
		a1(20)=a1(20)+1
	endi	
	if stat=[INSEM]
		a1(21)=a1(21)+1
	endi
	if stat=[CARGA]
		a1(22)=a1(22)+1
	endi
	if stat#[INSEM] and stat#[CARGA] and date()-fnac>(Q26*30.4)
		a1(23)=a1(23)+1
	endi			 

			** Por Edad
			** --------
			xx=date()-fnac
			do case
				case xx>=0 and xx<=90
					a1(24)=a1(24)+1
				case xx>90 and xx<=180
					a1(25)=a1(25)+1
				case xx>180 and xx<=270
					a1(26)=a1(26)+1
				case xx>270 and xx<=365
					a1(27)=a1(27)+1
				case xx>365 and xx<=450
					a1(28)=a1(28)+1
				case xx>450 and xx<=540
					a1(29)=a1(29)+1
				case xx>540 and xx<=630
					a1(30)=a1(30)+1
				case xx>630 and xx<=720
					a1(31)=a1(31)+1
				case xx>720 and xx<=810
					a1(32)=a1(32)+1
				case xx>810
					a1(33)=a1(33)+1
			endcase	

endscan

Select SREG
GO TOP
Scan
xtot3=xtot3+1
	
			** Por Edad
			** --------
			xx=date()-fnac
			do case
				case xx>=0 and xx<=90
					a1(34)=a1(34)+1
				case xx>90 and xx<=180
					a1(35)=a1(35)+1
				case xx>180 and xx<=270
					a1(36)=a1(36)+1
				case xx>270 and xx<=365
					a1(37)=a1(37)+1
				case xx>365 and xx<=450
					a1(38)=a1(38)+1
				case xx>450 and xx<=540
					a1(39)=a1(39)+1
				case xx>540 and xx<=630
					a1(40)=a1(40)+1
				case xx>630 and xx<=720
					a1(41)=a1(41)+1
				case xx>720 and xx<=810
					a1(42)=a1(42)+1
				case xx>810
					a1(43)=a1(43)+1
			endcase	
endscan
Set Filter To

Create Cursor REPORTE (Concepto c(30),COL1 c(5),COL2 c(5))
Append Blank
Replace CONCEPTO With 'VIENTRES'
Append Blank
Replace CONCEPTO With 'En Hato    ',COL1 With Str(xtot,4),COL2 With '100.0'
Append Blank
Replace CONCEPTO With 'En Ordena  ',COL1 With Str(A1(1),4),COL2 With Str(A1(1)/xtot*100,4,1)
Append Blank
Replace CONCEPTO With 'En Secas   ',COL1 With Str(A1(2),4),COL2 With Str(A1(2)/xtot*100,4,1)
Append Blank
Replace CONCEPTO With 'Frescas    ',COL1 With Str(A1(3),4),COL2 With Str(A1(3)/xtot*100,4,1)
Append Blank
Replace CONCEPTO With 'Inseminadas',COL1 With Str(A1(4),4),COL2 With Str(A1(4)/xtot*100,4,1)
Append Blank
Replace CONCEPTO With 'Abiertas   ',COL1 With Str(A1(6),4),COL2 With Str(A1(6)/xtot*100,4,1)
Append Blank
Replace CONCEPTO With 'Gestantes  ',COL1 With Str(A1(5),4),COL2 With Str(A1(5)/xtot*100,4,1)
Append Blank
Replace CONCEPTO With 'En Lactancia 1',COL1 With Str(A1(9),4),COL2 With Str(A1(9)/xtot*100,4,1)
Append Blank
Replace CONCEPTO With 'En Lactancia 2',COL1 With Str(A1(10),4),COL2 With Str(A1(10)/xtot*100,4,1)
Append Blank
Replace CONCEPTO With 'En Lactancia 3',COL1 With Str(A1(11),4),COL2 With Str(A1(11)/xtot*100,4,1)
Append Blank
Replace CONCEPTO With 'En Lactancia 4',COL1 With Str(A1(12),4),COL2 With Str(A1(12)/xtot*100,4,1)
Append Blank
Replace CONCEPTO With 'En Lactancia 5',COL1 With Str(A1(13),4),COL2 With Str(A1(13)/xtot*100,4,1)
Append Blank
Replace CONCEPTO With 'En Lactancia 6',COL1 With Str(A1(14),4),COL2 With Str(A1(14)/xtot*100,4,1)
Append Blank
Replace CONCEPTO With 'En Lactancia 7',COL1 With Str(A1(15),4),COL2 With Str(A1(15)/xtot*100,4,1)
Append Blank
Replace CONCEPTO With 'En Lactancia 8',COL1 With Str(A1(16),4),COL2 With Str(A1(16)/xtot*100,4,1)
Append Blank
Replace CONCEPTO With 'En Lactancia 9',COL1 With Str(A1(17),4),COL2 With Str(A1(17)/xtot*100,4,1)
Append Blank
Replace CONCEPTO With 'En Lactancia10',COL1 With Str(A1(18),4),COL2 With Str(A1(18)/xtot*100,4,1)
Append Blank
Append Blank
Replace CONCEPTO With 'CRIANZA'
Append Blank
Replace CONCEPTO With 'En Hato   ',COL1 With Str(xtot2,4),COL2 With '100.0'
Append Blank
Replace CONCEPTO With 'Virgenes  ',COL1 With Str(A1(20),4),COL2 With Str(A1(20)/xtot2*100,4,1)
Append Blank
Replace CONCEPTO With 'Inseminadas',COL1 With Str(A1(21),4),COL2 With Str(A1(21)/xtot2*100,4,1)
Append Blank
Replace CONCEPTO With 'Gestantes ',COL1 With Str(A1(22),4),COL2 With Str(A1(22)/xtot2*100,4,1)
Append Blank
Replace CONCEPTO With 'Abiertas  ',COL1 With Str(A1(23),4),COL2 With Str(A1(23)/xtot2*100,4,1)
Append Blank
Replace CONCEPTO With 'De 1 a 3 Meses',COL1 With Str(A1(24),4),COL2 With Str(A1(24)/xtot2*100,4,1)
Append Blank
Replace CONCEPTO With 'De 4 a 6 Meses',COL1 With Str(A1(25),4),COL2 With Str(A1(25)/xtot2*100,4,1)
Append Blank
Replace CONCEPTO With 'De 7 a 9 Meses',COL1 With Str(A1(26),4),COL2 With Str(A1(26)/xtot2*100,4,1)
Append Blank
Replace CONCEPTO With 'De 10 a 12 Meses',COL1 With Str(A1(27),4),COL2 With Str(A1(27)/xtot2*100,4,1)
Append Blank
Replace CONCEPTO With 'De 13 a 15 Meses',COL1 With Str(A1(28),4),COL2 With Str(A1(28)/xtot2*100,4,1)
Append Blank
Replace CONCEPTO With 'De 16 a 18 Meses',COL1 With Str(A1(29),4),COL2 With Str(A1(29)/xtot2*100,4,1)
Append Blank
Replace CONCEPTO With 'De 19 a 21 Meses',COL1 With Str(A1(30),4),COL2 With Str(A1(30)/xtot2*100,4,1)
Append Blank
Replace CONCEPTO With 'De 22 a 24 Meses',COL1 With Str(A1(31),4),COL2 With Str(A1(31)/xtot2*100,4,1)
Append Blank
Replace CONCEPTO With 'De 25 a 27 Meses',COL1 With Str(A1(32),4),COL2 With Str(A1(32)/xtot2*100,4,1)
Append Blank
Replace CONCEPTO With 'De 27 a  + Meses',COL1 With Str(A1(33),4),COL2 With Str(A1(33)/xtot2*100,4,1)

Append Blank
Append Blank
Replace CONCEPTO With 'MACHOS'
Append Blank
Replace CONCEPTO With 'En Hato   ',COL1 With Str(xtot3,4),COL2 With '100.0'
Append Blank
Replace CONCEPTO With 'De 1 a 3 Meses',COL1 With Str(A1(34),4),COL2 With Str(A1(34)/xtot3*100,4,1)
Append Blank
Replace CONCEPTO With 'De 4 a 6 Meses',COL1 With Str(A1(35),4),COL2 With Str(A1(35)/xtot3*100,4,1)
Append Blank
Replace CONCEPTO With 'De 7 a 9 Meses',COL1 With Str(A1(36),4),COL2 With Str(A1(36)/xtot3*100,4,1)
Append Blank
Replace CONCEPTO With 'De 10 a 12 Meses',COL1 With Str(A1(37),4),COL2 With Str(A1(37)/xtot3*100,4,1)
Append Blank
Replace CONCEPTO With 'De 13 a 15 Meses',COL1 With Str(A1(38),4),COL2 With Str(A1(38)/xtot3*100,4,1)
Append Blank
Replace CONCEPTO With 'De 16 a 18 Meses',COL1 With Str(A1(39),4),COL2 With Str(A1(39)/xtot3*100,4,1)
Append Blank
Replace CONCEPTO With 'De 19 a 21 Meses',COL1 With Str(A1(40),4),COL2 With Str(A1(40)/xtot3*100,4,1)
Append Blank
Replace CONCEPTO With 'De 22 a 24 Meses',COL1 With Str(A1(41),4),COL2 With Str(A1(41)/xtot3*100,4,1)
Append Blank
Replace CONCEPTO With 'De 25 a 27 Meses',COL1 With Str(A1(42),4),COL2 With Str(A1(42)/xtot3*100,4,1)
Append Blank
Replace CONCEPTO With 'De 27 a  + Meses',COL1 With Str(A1(43),4),COL2 With Str(A1(43)/xtot3*100,4,1)

GO TOP
Return


PROCEDURE RXC
*---------------------------------------------------------------------*
PARAMETERS COLS,ROWS,COLW,FIELD
DO WHILE MOD(RECCOUNT(),COLS) # 0
APPE BLANK
ENDD
GO TOP
PGCOUNT=1
PRINTED=0
NVAC=0
SET CONS OFF
SET PRIN ON
DO WHILE PRINTED<RECCOUNT() &&
?'  Rancho   : '+Q36+'   '+nom 
?'  Area     : VIENTRES'
?'  Reporte  : '+NTIT
?'  Fecha    : '+DTOC(DATE())+'  Hora : '+TIME()
?'  Pagina   : '+STR(PGCOUNT,2,0)+space(70)+[LACTOFOX]
?'  Periodo  : '+NTIT2
?'  '+REPLI(CHR(254),94)
?
?'      ID   DEL  Prod  Status Corr  Cond          ID   DEL  Prod  Status Corr  Cond'
?'  '+REPLI('-',94)
ONTHISPAGE=MIN(ROWS*COLS,RECCOUNT()-PRINTED)
ROWS=MIN(ROWS,(ONTHISPAGE/COLS))
THISROW=1
DO WHILE THISROW<=ROWS .AND. PRINTED<=RECCOUNT()
THISCOL=1
if id>0
?space(2),LEFT(STR(ID,5,0)+'  '+STR(DPR,4)+'  '+STR(PRM,4,1)+'  '+STAT+'  '+STR(CORR,4)+'   '+STR(CONDC,3,1),COLW)+SPACE(MAX(0,COLW-LEN(FIELD)))
NVAC=NVAC+1
endi
PRINTED=PRINTED+1
DO WHILE THISCOL<COLS .AND. PRINTED<=RECCOUNT()
SKIP ROWS
if id>0
??space(2),LEFT(STR(ID,5,0)+'  '+STR(DPR,4)+'  '+STR(PRM,4,1)+'  '+STAT+'  '+STR(CORR,4)+'   '+STR(CONDC,3,1),COLW)+SPACE(MAX(0,COLW-LEN(FIELD)))
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
?'   TOTAL = '+STR(NVAC,4)+'   DEL Promedio   = '+str(mdias,3)+'   Prod. Promedio = '+str(mprm,4,1)+'   Cond. Corporal = '+str(mTCON,4,1)
ENDI
ENDD
EJECT
PGCOUNT=PGCOUNT+1
SKIP ((COLS-1)*ROWS)
ENDD
CLOS DATA
ERAS LOTES2.DBF


PROCEDURE RXD
*---------------------------------------------------------------------*
PARAMETERS COLS,ROWS,COLW,FIELD
DO WHILE MOD(RECCOUNT(),COLS) # 0
APPE BLANK
ENDD
GO TOP
PGCOUNT=1
PRINTED=0
NVAC=0
SET CONS OFF
SET PRIN ON
DO WHILE PRINTED<RECCOUNT() &&
?'  Rancho   : '+Q36+'   '+nom 
?'  Area     : VIENTRES'
?'  Reporte  : '+NTIT
?'  Fecha    : '+DTOC(DATE())+'  Hora : '+TIME()
?'  Pagina   : '+STR(PGCOUNT,2,0)+space(70)+[LACTOFOX]
?'  Periodo  : '+NTIT2
?'  '+REPLI(CHR(254),94)
?
?'      ID   DEL  Prod   CCS    sl  Corr          ID   DEL  Prod   CCS    sl  Corr'
?'  '+REPLI('-',94)
ONTHISPAGE=MIN(ROWS*COLS,RECCOUNT()-PRINTED)
ROWS=MIN(ROWS,(ONTHISPAGE/COLS))
THISROW=1
DO WHILE THISROW<=ROWS .AND. PRINTED<=RECCOUNT()
THISCOL=1
if id>0
?space(2),LEFT(STR(ID,5,0)+'  '+STR(DPR,4)+'  '+STR(PRM,4,1)+'  '+STR(CCS,4)+'  '+STR(LSC,4,1)+'   '+STR(CORR,2),COLW)+SPACE(MAX(0,COLW-LEN(FIELD)))
NVAC=NVAC+1
endi
PRINTED=PRINTED+1
DO WHILE THISCOL<COLS .AND. PRINTED<=RECCOUNT()
SKIP ROWS
if id>0
??space(2),LEFT(STR(ID,5,0)+'  '+STR(DPR,4)+'  '+STR(PRM,4,1)+'  '+STR(CCS,4)+'  '+STR(LSC,4,1)+'   '+STR(CORR,2),COLW)+SPACE(MAX(0,COLW-LEN(FIELD)))
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
PGCOUNT=PGCOUNT+1
SKIP ((COLS-1)*ROWS)
ENDD
CLOS DATA
ERAS LOTES2.DBF

PROC LCAMBIO2
PUSH KEY CLEAR
save screen to ww
@ 4,0 clea to 24,79
@ 23,0 to 23,79
@ 22,29 say [LISTA DE CAMBIOS DE ID] colo gr+/&cz
@ 24,1 say [Total] colo w+/&cz
@ 4,1 say [                                                                              ] colo n/w
@ 5,1 SAY [     ID   IDant     ID   IDant     ID   IDant     ID   IDant     ID   IDant   ] colo n/w
ROW=7
COL=3
DAT=0
@ 24,70 SAY [Espere] colo n*/w
SCAN WHILE .NOT. EOF() FOR NP>0 .AND. IDAN>0
@ 24,70 SAY [Espere] colo n*/w
@ 24,8 SAY DAT+1 PICT '9999'

@ ROW,COL SAY ID colo gr+/&cz
@ ROW,COL+8 SAY IDAN
IF ROW=20 .AND. COL=63 
@ 24,70 SAY [Enter] COLO N/W
@ 24,75 SAY [ ] COLO W+/&CZ
@ 24,76 SAY CHR(16) colo w+/&cz
DO WHILE INKEY()#13
IF LASTKEY()=6
@ 4,0 CLEA TO 20,78
GO BOTTOM
EXIT
ENDI
ENDD
@ 6,2 CLEA TO 20,77
@ 24,58 clea to 24,78
ROW=6
COL=3
ENDI
IF ROW=20
ROW=6
COL=COL+15
ENDI
DAT=DAT+1
ROW=ROW+1
ENDS
??chr(7)
@ 24,70 SAY [Enter] COLO N/W
@ 24,75 SAY [ ] COLO W+/&CZ
@ 24,76 SAY CHR(4) colo w+/&cz
DO WHILE INKEY() # 13
ENDD
@ 24,69 SAY [Imprimir] GET SN PICT '@M S,N' colo n/w 
READ
IF SN='N' .OR. LASTKEY()=27
ELSE
IF SN='S' .AND. SYS(13)='READY'
NTIT=[LISTA DE CAMBIOS DE ID]
@ 24,67 clea to 24,79
@ 24,70 SAY [Espere] colo n*/w
use REG order 2
GO TOP
COPY TO LOTES FIELDS ID,IDAN FOR NP>0 .AND. IDAN>0
CLOS ALL
USE LOTES
@ 24,67 CLEA TO 24,79
@ 24,70 SAY [Espere] colo n*/w
DO R4 WITH 5,(WLI),12,STR(ID,5)+STR(IDAN,5)
SET PRIN OFF
ELSE
??CHR(7)
WAIT WIND [ IMPRESORA APAGADA O DESCONECTADA ] TIMEOUT 2
ENDI
ENDI
*----------------------------------*
POP KEY
use REG order 2
go top
restore screen from ww
RETU


PROCEDURE R4
*---------------------------------------------------------------------*
PARAMETERS COLS,ROWS,COLW,FIELD
DO WHILE MOD(RECCOUNT(),COLS) # 0
APPE BLANK
ENDD
GO TOP
PGCOUNT=1
PRINTED=0
NVAC=0
SET CONS OFF
SET PRIN ON
DO WHILE PRINTED<RECCOUNT() &&
?'  Rancho   : '+Q36+'   '+nom 
?'  Area     : VIENTRES'
?'  Reporte  : '+NTIT
?'  Fecha    : '+DTOC(DATE())+'  Hora : '+TIME()
?'  Pagina   : '+STR(PGCOUNT,2,0)+space(70)+[LACTOFOX]
?'  '+REPLI(CHR(254),94)
?
?'      ID  IDant        ID  IDant        ID  IDant        ID  IDant        ID  IDant'
?SPACE(1),REPLI('-',94)
ONTHISPAGE=MIN(ROWS*COLS,RECCOUNT()-PRINTED)
ROWS=MIN(ROWS,(ONTHISPAGE/COLS))
THISROW=1

DO WHILE THISROW<=ROWS .AND. PRINTED<=RECCOUNT()
THISCOL=1
if id>0
?space(2),LEFT(STR(ID,5,0)+'  '+STR(IDAN,5),COLW)+SPACE(MAX(0,COLW-LEN(FIELD)))
NVAC=NVAC+1
endi
PRINTED=PRINTED+1
DO WHILE THISCOL<COLS .AND. PRINTED<=RECCOUNT()
SKIP ROWS
if id>0
??space(2),LEFT(STR(ID,5,0)+'  '+STR(IDAN,5),COLW)+SPACE(MAX(0,COLW-LEN(FIELD)))
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
?'  '+REPLI(CHR(254),94)
?
?'   TOTAL = '+STR(NVAC,4)
ENDI
ENDD
EJECT
PGCOUNT=PGCOUNT+1
SKIP ((COLS-1)*ROWS)
ENDD
CLOS DATA
ERAS LOTES.DBF

PROCEDURE RV1067
****************  Nuevo 2009.08.20
SELECT REG 
SET ORDER TO 2
*SET FILTER TO FB2=B

SELECT CALOR
SET ORDER TO 2
SET FILTER TO FECHA2>=CTOD(FREPS.CB1.Value) AND FECHA2<=CTOD(FREPS.CB2.Value) AND SER>0 AND (STAT="INSEM" OR STAT="CARGA") &&(CLAVE=5 OR CLAVE=6)

SET RELATION TO ID INTO REG 

Dimension aw(40),cw(40),dw(30),ew(30)
aw=0
cw=0

SCAN
*xx=EXP_8

xx=(FECHA2-PARTO)

**-------------------------------------*
IF STAT="INSEM" &&AND CLAVE=5
aw(1)=aw(1)+1
ENDIF

IF STAT=[CARGA] &&AND CLAVE=6
	cw(38)=cw(38)+1
ENDIF

* TOTAL *****************
aw(40)=(cw(38)/aw(1))*100


* Por Periodos
* ------------
		do case 
			case xx>=1 and xx<=60
				if STAT="INSEM" 
				 aw(2)=aw(2)+1
				endi
				if STAT="CARGA" 
				 cw(2)=cw(2)+1
				endif
				aw(15)=(cw(2)/aw(2))*100
				
			case xx>60 and xx<=80
				if STAT="INSEM"
				aw(3)=aw(3)+1
				endif
				if stat=[CARGA]
					cw(3)=cw(3)+1
				endi
				aw(17)=(cw(3)/aw(3))*100

			case xx>80 and xx<=100
				if STAT="INSEM"
				aw(4)=aw(4)+1
				endif

				if stat=[CARGA]
					cw(4)=cw(4)+1
				endi
				aw(19)=(cw(4)/aw(4))*100

			case xx>100 and xx<=120
				if STAT="INSEM"
				aw(5)=aw(5)+1
                endif
                
				if stat=[CARGA]
					cw(5)=cw(5)+1
				endi
				aw(21)=(cw(5)/aw(5))*100

			case xx>120 and xx<=150
				if STAT="INSEM"
				aw(6)=aw(6)+1
                endif
				if stat=[CARGA]
					cw(6)=cw(6)+1
				endi
				aw(23)=(cw(6)/aw(6))*100

			case xx>150 and xx<=180
				if STAT="INSEM"
				aw(7)=aw(7)+1
				endif
				if stat=[CARGA]
					cw(7)=cw(7)+1
				endi
				aw(25)=(cw(7)/aw(7))*100

			case xx>180 and xx<=210
				if STAT="INSEM"
				aw(8)=aw(8)+1
                endif
				if stat=[CARGA]
					cw(8)=cw(8)+1
				endi
				aw(27)=(cw(8)/aw(8))*100

			case xx>210 and xx<=240
				if STAT="INSEM"
				aw(9)=aw(9)+1
                endif
				if stat=[CARGA]
					cw(9)=cw(9)+1
				endi
				aw(29)=(cw(9)/aw(9))*100

			case xx>240 and xx<=270
				if STAT="INSEM"
				aw(10)=aw(10)+1
				endif
				if stat=[CARGA]
					cw(10)=cw(10)+1
				endi
				aw(31)=(cw(10)/aw(10))*100

			case xx>270 and xx<=300
				if STAT="INSEM"
				aw(11)=aw(11)+1
				endif
				if stat=[CARGA]
					cw(11)=cw(11)+1
				endi
				aw(33)=(cw(11)/aw(11))*100

			case xx>300 and xx<=330
				if STAT="INSEM"
				aw(12)=aw(12)+1
				endif
				if stat=[CARGA]
					cw(12)=cw(12)+1
				endi
				aw(35)=(cw(12)/aw(12))*100

			case xx>330
				if STAT="INSEM"
				aw(13)=aw(13)+1
				endif
				if stat=[CARGA]
					cw(13)=cw(13)+1
				endi
				aw(37)=(cw(13)/aw(13))*100

		endcase		
**-------------------------------------*
ENDSCAN

Create Cursor REPORTE (Concepto c(20),COL1 c(5),COL2 c(5),COL3 c(5),COL4 c(5))
Append Blank
Replace CONCEPTO With ' 0 -  60',COL1 With Str(aw(2),4),COL2 With Str(cw(2),4),COL3 With Str(aw(15),4,1)
Append Blank
Replace CONCEPTO With '61 -  80',COL1 With Str(aw(3),4),COL2 With Str(cw(3),4),COL3 With Str(aw(17),4,1)
Append Blank
Replace CONCEPTO With '81 - 100',COL1 With Str(aw(4),4),COL2 With Str(cw(4),4),COL3 With Str(aw(19),4,1)
Append Blank
Replace CONCEPTO With '101 - 120',COL1 With Str(aw(5),4),COL2 With Str(cw(5),4),COL3 With Str(aw(21),4,1)
Append Blank
Replace CONCEPTO With '121 - 150',COL1 With Str(aw(6),4),COL2 With Str(cw(6),4),COL3 With Str(aw(23),4,1)
Append Blank
Replace CONCEPTO With '151 - 180',COL1 With Str(aw(7),4),COL2 With Str(cw(7),4),COL3 With Str(aw(25),4,1)
Append Blank
Replace CONCEPTO With '181 - 210',COL1 With Str(aw(8),4),COL2 With Str(cw(8),4),COL3 With Str(aw(27),4,1)
Append Blank
Replace CONCEPTO With '211 - 240',COL1 With Str(aw(9),4),COL2 With Str(cw(9),4),COL3 With Str(aw(29),4,1)
Append Blank
Replace CONCEPTO With '241 - 270',COL1 With Str(aw(10),4),COL2 With Str(cw(10),4),COL3 With Str(aw(31),4,1)
Append Blank
Replace CONCEPTO With '271 - 300',COL1 With Str(aw(11),4),COL2 With Str(cw(11),4),COL3 With Str(aw(33),4,1)
Append Blank
Replace CONCEPTO With '301 - 330',COL1 With Str(aw(12),4),COL2 With Str(cw(12),4),COL3 With Str(aw(35),4,1)
Append Blank
Replace CONCEPTO With '331  - > ',COL1 With Str(aw(13),4),COL2 With Str(cw(13),4),COL3 With Str(aw(37),4,1)
Append Blank
Append Blank
Replace CONCEPTO With 'Total',COL1 With Str(aw(1),4),COL2 With Str(cw(38),4),COL3 With Str(aw(40),4,1)

Release All Like aw*,bw*,cw*,dw,ew*
RETURN


PROC RV102099
*-----------
DIMENSION xs(120),ts(60),mes(12)
xs=0
ts=0
mes=1

Select GNFERT
Set Filter TO xTORO
Set Relation TO

If REPORTES.NUM=209
	xFilter="NP>0"
Else
	xFilter="NP=0"
EndIf

Calculate MAX(fecha) To xfecha
xfecha=xfecha-Q7

Select ID,FECHA2,SER,STAT From GNFERT Where YEAR(fecha2)=xan And ser>0 And fecha2<=xfecha And fecha2#B and (Clave=5 Or Clave=6) And &xFilter Into Table xDATOS

x=0
SCAN
x=x+1

DO CASE
	CASE ser=1
		do case 
			case month(fecha2)=1 
				if left(stat,1)="I"
					xs(1)=xs(1)+1
				endi
				if left(stat,1)="C"
					xs(2)=xs(2)+1
				endi
				ts(1)=(xs(2)/xs(1))*100

			case month(fecha2)=2 
				if left(stat,1)="I"
					xs(3)=xs(3)+1
				endi
				if left(stat,1)="C"
					xs(4)=xs(4)+1
				endi
				ts(2)=(xs(4)/xs(3))*100

			case month(fecha2)=3
				if left(stat,1)="I"
					xs(5)=xs(5)+1
				endi
				if left(stat,1)="C"
					xs(6)=xs(6)+1
				endi
				ts(3)=(xs(6)/xs(5))*100

			case month(fecha2)=4
				if left(stat,1)="I"
					xs(7)=xs(7)+1
				endi
				if left(stat,1)="C"
					xs(8)=xs(8)+1
				endi
				ts(4)=(xs(8)/xs(7))*100

			case month(fecha2)=5
				if left(stat,1)="I"
					xs(9)=xs(9)+1
				endi
				if left(stat,1)="C"
					xs(10)=xs(10)+1
				endi
				ts(5)=(xs(10)/xs(9))*100

			case month(fecha2)=6
				if left(stat,1)="I"
					xs(11)=xs(11)+1
				endi
				if left(stat,1)="C"
					xs(12)=xs(12)+1
				endi
				ts(6)=(xs(12)/xs(11))*100

			case month(fecha2)=7
				if left(stat,1)="I"
					xs(13)=xs(13)+1
				endi
				if left(stat,1)="C"
					xs(14)=xs(14)+1
				endi
				ts(7)=(xs(14)/xs(13))*100

			case month(fecha2)=8
				if left(stat,1)="I"
					xs(15)=xs(15)+1
				endi
				if left(stat,1)="C"
					xs(16)=xs(16)+1
				endi
				ts(8)=(xs(16)/xs(15))*100

			case month(fecha2)=9
				if left(stat,1)="I"
					xs(17)=xs(17)+1
				endi
				if left(stat,1)="C"
					xs(18)=xs(18)+1
				endi
				ts(9)=(xs(18)/xs(17))*100

			case month(fecha2)=10 
				if left(stat,1)="I"
					xs(19)=xs(19)+1
				endi
				if left(stat,1)="C"
					xs(20)=xs(20)+1
				endi
				ts(10)=(xs(20)/xs(19))*100

			case month(fecha2)=11
				if left(stat,1)="I"
					xs(21)=xs(21)+1
				endi
				if left(stat,1)="C"
					xs(22)=xs(22)+1
				endi
				ts(11)=(xs(22)/xs(21))*100

			case month(fecha2)=12
				if left(stat,1)="I"
					xs(23)=xs(23)+1
				endi
				if left(stat,1)="C"
					xs(24)=xs(24)+1
				endi
				ts(12)=(xs(24)/xs(23))*100

		endcase

	CASE ser=2
		do case 
			case month(fecha2)=1 
				if left(stat,1)="I"
					xs(25)=xs(25)+1
				endi
				if left(stat,1)="C"
					xs(26)=xs(26)+1
				endi
				ts(13)=(xs(26)/xs(25))*100

			case month(fecha2)=2 
				if left(stat,1)="I"
					xs(27)=xs(27)+1
				endi
				if left(stat,1)="C"
					xs(28)=xs(28)+1
				endi
				ts(14)=(xs(28)/xs(27))*100

			case month(fecha2)=3
				if left(stat,1)="I"
					xs(29)=xs(29)+1
				endi
				if left(stat,1)="C"
					xs(30)=xs(30)+1
				endi
				ts(15)=(xs(30)/xs(29))*100

			case month(fecha2)=4
				if left(stat,1)="I"
					xs(31)=xs(31)+1
				endi
				if left(stat,1)="C"
					xs(32)=xs(32)+1
				endi
				ts(16)=(xs(32)/xs(31))*100

			case month(fecha2)=5
				if left(stat,1)="I"
					xs(33)=xs(33)+1
				endi
				if left(stat,1)="C"
					xs(34)=xs(34)+1
				endi
				ts(17)=(xs(34)/xs(33))*100

			case month(fecha2)=6
				if left(stat,1)="I"
					xs(35)=xs(35)+1
				endi
				if left(stat,1)="C"
					xs(36)=xs(36)+1
				endi
				ts(18)=(xs(36)/xs(35))*100

			case month(fecha2)=7
				if left(stat,1)="I"
					xs(37)=xs(37)+1
				endi
				if left(stat,1)="C"
					xs(38)=xs(38)+1
				endi
				ts(19)=(xs(38)/xs(37))*100

			case month(fecha2)=8
				if left(stat,1)="I"
					xs(39)=xs(39)+1
				endi
				if left(stat,1)="C"
					xs(40)=xs(40)+1
				endi
				ts(20)=(xs(40)/xs(39))*100

			case month(fecha2)=9
				if left(stat,1)="I"
					xs(41)=xs(41)+1
				endi
				if left(stat,1)="C"
					xs(42)=xs(42)+1
				endi
				ts(21)=(xs(42)/xs(41))*100

			case month(fecha2)=10 
				if left(stat,1)="I"
					xs(43)=xs(43)+1
				endi
				if left(stat,1)="C"
					xs(44)=xs(44)+1
				endi
				ts(22)=(xs(44)/xs(43))*100

			case month(fecha2)=11
				if left(stat,1)="I"
					xs(45)=xs(45)+1
				endi
				if left(stat,1)="C"
					xs(46)=xs(46)+1
				endi
				ts(23)=(xs(46)/xs(45))*100

			case month(fecha2)=12
				if left(stat,1)="I"
					xs(47)=xs(47)+1
				endi
				if left(stat,1)="C"
					xs(48)=xs(48)+1
				endi
				ts(24)=(xs(48)/xs(47))*100

		endcase

	CASE ser>=3
		do case 
			case month(fecha2)=1 
				if left(stat,1)="I"
					xs(49)=xs(49)+1
				endi
				if left(stat,1)="C"
					xs(50)=xs(50)+1
				endi
				ts(25)=(xs(50)/xs(49))*100

			case month(fecha2)=2 
				if left(stat,1)="I"
					xs(51)=xs(51)+1
				endi
				if left(stat,1)="C"
					xs(52)=xs(52)+1
				endi
				ts(26)=(xs(52)/xs(51))*100

			case month(fecha2)=3
				if left(stat,1)="I"
					xs(53)=xs(53)+1
				endi
				if left(stat,1)="C"
					xs(54)=xs(54)+1
				endi
				ts(27)=(xs(54)/xs(53))*100

			case month(fecha2)=4
				if left(stat,1)="I"
					xs(55)=xs(55)+1
				endi
				if left(stat,1)="C"
					xs(56)=xs(56)+1
				endi
				ts(28)=(xs(56)/xs(55))*100

			case month(fecha2)=5
				if left(stat,1)="I"
					xs(57)=xs(57)+1
				endi
				if left(stat,1)="C"
					xs(58)=xs(58)+1
				endi
				ts(29)=(xs(58)/xs(57))*100

			case month(fecha2)=6
				if left(stat,1)="I"
					xs(59)=xs(59)+1
				endi
				if left(stat,1)="C"
					xs(60)=xs(60)+1
				endi
				ts(30)=(xs(60)/xs(59))*100

			case month(fecha2)=7
				if left(stat,1)="I"
					xs(61)=xs(61)+1
				endi
				if left(stat,1)="C"
					xs(62)=xs(62)+1
				endi
				ts(31)=(xs(62)/xs(61))*100

			case month(fecha2)=8
				if left(stat,1)="I"
					xs(63)=xs(63)+1
				endi
				if left(stat,1)="C"
					xs(64)=xs(64)+1
				endi
				ts(32)=(xs(64)/xs(63))*100

			case month(fecha2)=9
				if left(stat,1)="I"
					xs(65)=xs(65)+1
				endi
				if left(stat,1)="C"
					xs(66)=xs(66)+1
				endi
				ts(33)=(xs(66)/xs(65))*100

			case month(fecha2)=10 
				if left(stat,1)="I"
					xs(67)=xs(67)+1
				endi
				if left(stat,1)="C"
					xs(68)=xs(68)+1
				endi
				ts(34)=(xs(68)/xs(67))*100

			case month(fecha2)=11
				if left(stat,1)="I"
					xs(69)=xs(69)+1
				endi
				if left(stat,1)="C"
					xs(70)=xs(70)+1
				endi
				ts(35)=(xs(70)/xs(69))*100

			case month(fecha2)=12
				if left(stat,1)="I"
					xs(71)=xs(71)+1
				endi
				if left(stat,1)="C"
					xs(72)=xs(72)+1
				endi
				ts(36)=(xs(72)/xs(71))*100

		endcase
ENDCASE

** Todos los servicios
** -------------------

		do case 
			case month(fecha2)=1 
				if left(stat,1)="I"
					xs(73)=xs(73)+1
				endi
				if left(stat,1)="C"
					xs(74)=xs(74)+1
				endi
				ts(37)=(xs(74)/xs(73))*100

			case month(fecha2)=2 
				if left(stat,1)="I"
					xs(75)=xs(75)+1
				endi
				if left(stat,1)="C"
					xs(76)=xs(76)+1
				endi
				ts(38)=(xs(76)/xs(75))*100

			case month(fecha2)=3
				if left(stat,1)="I"
					xs(77)=xs(77)+1
				endi
				if left(stat,1)="C"
					xs(78)=xs(78)+1
				endi
				ts(39)=(xs(78)/xs(77))*100

			case month(fecha2)=4
				if left(stat,1)="I"
					xs(79)=xs(79)+1
				endi
				if left(stat,1)="C"
					xs(80)=xs(80)+1
				endi
				ts(40)=(xs(80)/xs(79))*100

			case month(fecha2)=5
				if left(stat,1)="I"
					xs(81)=xs(81)+1
				endi
				if left(stat,1)="C"
					xs(82)=xs(82)+1
				endi
				ts(41)=(xs(82)/xs(81))*100

			case month(fecha2)=6
				if left(stat,1)="I"
					xs(83)=xs(83)+1
				endi
				if left(stat,1)="C"
					xs(84)=xs(84)+1
				endi
				ts(42)=(xs(84)/xs(83))*100

			case month(fecha2)=7
				if left(stat,1)="I"
					xs(85)=xs(85)+1
				endi
				if left(stat,1)="C"
					xs(86)=xs(86)+1
				endi
				ts(43)=(xs(86)/xs(85))*100

			case month(fecha2)=8
				if left(stat,1)="I"
					xs(87)=xs(87)+1
				endi
				if left(stat,1)="C"
					xs(88)=xs(88)+1
				endi
				ts(44)=(xs(88)/xs(87))*100

			case month(fecha2)=9
				if left(stat,1)="I"
					xs(89)=xs(89)+1
				endi
				if left(stat,1)="C"
					xs(90)=xs(90)+1
				endi
				ts(45)=(xs(90)/xs(89))*100

			case month(fecha2)=10 
				if left(stat,1)="I"
					xs(91)=xs(91)+1
				endi
				if left(stat,1)="C"
					xs(92)=xs(92)+1
				endi
				ts(46)=(xs(92)/xs(91))*100

			case month(fecha2)=11
				if left(stat,1)="I"
					xs(93)=xs(93)+1
				endi
				if left(stat,1)="C"
					xs(94)=xs(94)+1
				endi
				ts(47)=(xs(94)/xs(93))*100

			case month(fecha2)=12
				if left(stat,1)="I"
					xs(95)=xs(95)+1
				endi
				if left(stat,1)="C"
					xs(96)=xs(96)+1
				endi
				ts(48)=(xs(96)/xs(95))*100

		endcase
    
       ** Calculo General
       ** --------------- 
		do case
			case ser=1
				if left(stat,1)="I"
					xs(97)=xs(97)+1
				endi
				if left(stat,1)="C"
					xs(98)=xs(98)+1
				endi
				ts(49)=(xs(98)/xs(97))*100

			case ser=2
				if left(stat,1)="I"
					xs(99)=xs(99)+1
				endi
				if left(stat,1)="C"
					xs(100)=xs(100)+1
				endi
				ts(50)=(xs(100)/xs(99))*100

			case ser>=3
				if left(stat,1)="I"
					xs(101)=xs(101)+1
				endi
				if left(stat,1)="C"
					xs(102)=xs(102)+1
				endi
				ts(51)=(xs(102)/xs(101))*100
			endcase
				
				** Total
				** -----
				if left(stat,1)="I"
					xs(103)=xs(103)+1
				endi
				if left(stat,1)="C"
					xs(104)=xs(104)+1
				endi
				ts(52)=(xs(104)/xs(103))*100
	
ENDSCAN
Set Filter To

Create Cursor REPORTE (Concepto c(20),COL1 c(5),COL2 c(5),COL3 c(5),COL4 c(5),COL5 c(5),COL6 c(5))
Append Blank
Replace CONCEPTO With 'Mes'
Append Blank
Replace CONCEPTO With 'Enero',COL1 With Str(TS(1),5,1),COL2 With Str(TS(13),5,1),COL3 With Str(TS(25),5,1),COL4 With Str(TS(37),5,1)
Append Blank
Replace CONCEPTO With 'Febrero',COL1 With Str(TS(2),5,1),COL2 With Str(TS(14),5,1),COL3 With Str(TS(26),5,1),COL4 With Str(TS(38),5,1)
Append Blank
Replace CONCEPTO With 'Marzo',COL1 With Str(TS(3),5,1),COL2 With Str(TS(15),5,1),COL3 With Str(TS(27),5,1),COL4 With Str(TS(39),5,1)
Append Blank
Replace CONCEPTO With 'Abril',COL1 With Str(TS(4),5,1),COL2 With Str(TS(16),5,1),COL3 With Str(TS(28),5,1),COL4 With Str(TS(40),5,1)
Append Blank
Replace CONCEPTO With 'Mayo',COL1 With Str(TS(5),5,1),COL2 With Str(TS(17),5,1),COL3 With Str(TS(29),5,1),COL4 With Str(TS(41),5,1)
Append Blank
Replace CONCEPTO With 'Junio',COL1 With Str(TS(6),5,1),COL2 With Str(TS(18),5,1),COL3 With Str(TS(30),5,1),COL4 With Str(TS(42),5,1)
Append Blank
Replace CONCEPTO With 'Julio',COL1 With Str(TS(7),5,1),COL2 With Str(TS(19),5,1),COL3 With Str(TS(31),5,1),COL4 With Str(TS(43),5,1)
Append Blank
Replace CONCEPTO With 'Agosto',COL1 With Str(TS(8),5,1),COL2 With Str(TS(20),5,1),COL3 With Str(TS(32),5,1),COL4 With Str(TS(44),5,1)
Append Blank
Replace CONCEPTO With 'Septiembre',COL1 With Str(TS(9),5,1),COL2 With Str(TS(21),5,1),COL3 With Str(TS(33),5,1),COL4 With Str(TS(45),5,1)
Append Blank
Replace CONCEPTO With 'Octubre',COL1 With Str(TS(10),5,1),COL2 With Str(TS(22),5,1),COL3 With Str(TS(34),5,1),COL4 With Str(TS(46),5,1)
Append Blank
Replace CONCEPTO With 'Noviembre',COL1 With Str(TS(11),5,1),COL2 With Str(TS(23),5,1),COL3 With Str(TS(35),5,1),COL4 With Str(TS(47),5,1)
Append Blank
Replace CONCEPTO With 'Diciembre',COL1 With Str(TS(12),5,1),COL2 With Str(TS(24),5,1),COL3 With Str(TS(36),5,1),COL4 With Str(TS(48),5,1)
Append Blank
Replace CONCEPTO With ''
Append Blank
Replace CONCEPTO With 'Promedio',COL1 With Str(TS(49),5,1),COL2 With Str(TS(50),5,1),COL3 With Str(TS(51),5,1),COL4 With Str(TS(52),5,1)
GO TOP
RETURN



