* INVENTARIO DE TOROS * IMPRESION 
* --------------------------------
	Select CTOROS
	calc avg(ptal) for ptal>0 to xPTAL 
	calc avg(ptag) for ptag>0 to xPTAG
	calc avg(ptap) for ptap>0 to xPTAP
	calc avg(ptas) for ptas>0 to xPTAS
	calc avg(precio) for precio>0 to xPRECIO

	SUM CANT FOR CANT>0 TO NTOTAL

*With THISFORM
_Plength=85
WLI=80
xpunto=104
xAREA="SEMENTALES"
XNtor=0
XNdosis=0
xPRINTER=GETPRINTER()


	If Empty(xPRINTER)
	Else
		Set PRINTER TO NAME(xPRINTER)
		Set Console OFF
		Set Printer ON    &&PROMPT= Para Abrir Cuadro de Dialogo de Propiedades de Impresora
		SET PRINTER FONT 'Courier New',8 Style 'N'

* IMPRESION DEL REPORTE
* ---------------------
	NTIT='INVENTARIO Y CATALOGO'
	NTIT2=DTOC(Date())
	NTIT3='  Codigo         Nombre            PTAL   PTAG  PTAP  PTA$   # Dosis    %     Precio      Total'
	
	PAG=1
	_PLINENO=0
	
	Do TITULO5 IN CTRLVRPT With xAREA
	?'  '+Replicate('-',xpunto)
	?nTIT3
	?'  '+Replicate('-',xpunto)

	SCAN 
		?'  '+TORO+'      '+NOMBRE+'     '+STR(PTAL,4)+'   '+STR(PTAG,3)+'   ';
		+STR(PTAP,3)+'   '+STR(PTAS,4)+'    '+STR(CANT,4,0)+'    '+str(((cant/ntotal)*100),4,1)+'   '+str(PRECIO,7,2)+'    '+STR((CANT*PRECIO),7,2)

		XNtor=XNtor+1
		XNdosis=XNdosis+cant
		
		If _PLINENO>WLI
			?'  '+Replicate('-',xpunto)
			Eject
			PAG=PAG+1
			_PLINENO=0
			 
			Do TITULO1 IN CTRLVRPT With xAREA
			?'  '+Replicate('-',xpunto)
  	 	?nTIT3
			?'  '+Replicate('-',xpunto)
		EndIf							
		
		
	ENDSCAN
	?'  '+Replicate('-',xpunto)
	
	?'  R E S U M E N                    '+STR(XPTAL,4)+'   '+STR(XPTAG,3)+'   ';
			+STR(XPTAP,3)+'   '+STR(XPTAS,4)+'    '+STR(NTOTAL,4,0)+'           '+str(XPRECIO,7,2)+'    '+STR((NTOTAL*XPRECIO),7,2)
	?'  '+Replicate('-',xpunto)
	?'  T O T A L = '+STR(XNTOR,4)  
	
		* ---------------
		* FIN DEL REPORTE

		Set Printer OFF
		Set Console ON
		Close Printer

EndIf
*EndWith






