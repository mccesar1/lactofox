*PROGRAMA PARA LA IMPRESION DE LOS REPORTED DEFINIDOS POR EL USUARIO
*3 DE JULIO DE 2002 18:00
*JOSE DE JESUS AMARO RUBIO

LOCAL xArea,NTIT3_1,NTIT3_2,xspa1,xspa2,xspa3,xspa4,xspa5,xprn,NTIT4,xpunto,WLI,I,xa1,xa2,NTIT5,xtotal,xcamtmp,xnrg
PUBLIC PAG,NTIT2

xArea=""
NTIT3_1="" && Encabezados de las columnas
NTIT3_2=""
NTIT4=""
NTIT5=""
xpunto=0
PAG=1
NTIT2=""
_Plength=85
_Plineno=0
xArea=Iif(Reporteador.Opg2.Value=1,"VIENTRES","CRIAS")
WLI=80
xcamtmp=""
xtotal=0
xnrg=0

Select Repdet
Scan
	*Se crea la primera linea de los encabezados de columna
	xspa1=Int((Anchop-Len(Alltrim(Titulo1)))/2)
	xspa2=Anchop-Len(Alltrim(Titulo1))-xspa1+2
	NTIT3_1=NTIT3_1+Space(xspa1)+Alltrim(Titulo1)+Space(xspa2)
	
	*Se crea la segunda linea de los encabezados de columna
	xspa3=Int((Anchop-Len(Alltrim(Titulo2)))/2)
	xspa4=Anchop-Len(Alltrim(Titulo2))-xspa3+2
	NTIT3_2=NTIT3_2+Space(xspa3)+Alltrim(Titulo2)+Space(xspa4)
	*xpunto= Len(NTIT3_2)-2
	xpunto=120
EndScan
	
*Se crea la linea de datos a imprimir
Select Camnode
Set Order To 3
Select Repdet
Set Order To
Set Filter To rnum=REPORTEADOR.Pgf1.Page1.Text2.Value AND Tipo=xtip
Set Relation to numcampo Into Camnode
*Browse Fields numcampo,campo,Camnode.nomcampo,Camnode.Tipo

*CICLO PARA JUNTAR LOS DATOS DEL CONTENIDO DE LAS COLUMNAS
I=0
Scan
	I=I+1
	Select xRDatos
	Do Case
	Case Camnode.Tipo="C"
		xa1=Camnode.Anchop-Fsize(Field(I))
		NTIT4=NTIT4+Field(I)+Chr(34)+Space(xa1)+Chr(34)+"+"+Chr(34)+Space(2)+Chr(34)+"+"
	Case Camnode.Tipo="F"
		xa1=(Camnode.Anchop-10)/2
		xa2=Camnode.Anchop-10-xa1+2
		NTIT4=NTIT4+Chr(34)+Space(xa1)+Chr(34)+"Dtoc("+Field(I)+")+"+Chr(34)+Space(xa2)+Chr(34)+"+"
	Case Camnode.Tipo="N"
		xa1=Int(Camnode.Anchop)
		xa2=(Camnode.Anchop-xa1)*10
		NTIT4=NTIT4+"Str("+Field(I)+","+Alltrim(Str(xa1))+","+Alltrim(Str(xa2))+")+"+Chr(34)+Space(2)+Chr(34)+"+"
	EndCase
EndScan

*CICLO PARA CALCULAR TOTALES
I=0
Select Repdet
Go top
Scan
	xnrg=recno()
	I=I+1
	Select xRDatos
	xcamtmp="xRdatos."+Field(I)
	Select Repdet
	xa1=Int(Camnode.Anchop)
	xa2=(Camnode.Anchop-xa1)*10
	Do Case
	Case Camnode.total="C"
		Select xRDatos
		xtotal=Reccount()
		Select Repdet
		NTIT5=NTIT5+Str(xtotal,xa1,xa2)+Space(2)
	Case Camnode.total="P"
		Calculate Avg(&xcamtmp) For &xcamtmp>0 To xtotal
		NTIT5=NTIT5+Str(xtotal,xa1,xa2)+Space(2)
	Case Camnode.Total="S"
		xtotal=Sum &xcamtmp
		NTIT5=NTIT5+Str(xtotal,xa1,xa2)+Space(2)
	Otherwise
		NTIT5=NTIT5+Space(xa1+2)
	Endcase
	Select Repdet
	Goto xnrg
EndScan
NTIT4=Left(NTIT4,Len(NTIT4)-1)

*Se Imprime
xprn=GetPrinter()
If  NOT Empty(xprn)
	*Se configura la impresora
	*Set Printer To DEFAULT
	Set Printer To &xprn
	Set Console Off
	Set Printer On
	Set Printer Font 'Courier New',8 Style 'N'
	
	*Se imprime el encabezado
	Do Titulo1 In CtrlVrpt With xArea
	? '  '+Replicate('-',xpunto)
	? NTIT3_1
	If !Empty(Alltrim(NTIT3_2))
		? NTIT3_2
	EndIf
	? '  '+Replicate('-',xpunto)
	*Se Imprimen los datos
	Select xRDatos
	Scan
		? &NTIT4
		?
		
		If _Plineno>WLI
			? '  '+Replicate('-',xpunto)
			Eject
			PAG=PAG+1
			_Plineno=0
			Do Titulo1 In CtrlVrpt With xArea
			? '  '+Replicate('-',xpunto)
			? NTIT3_1
			If !Empty(Alltrim(NTIT3_2))
				? NTIT3_2
			EndIf
			? '  '+Replicate('-',xpunto)
			?
		EndIf
	EndScan
	? '  '+Replicate('-',xpunto)
	? NTIT5
	
	Set Printer Off
	Set Console On
	Close Printer
EndIf
	
