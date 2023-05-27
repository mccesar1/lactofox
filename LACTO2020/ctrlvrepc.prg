PARAMETERS NTIT
xNVE=0
NTIT2=""
WLI=60
xPRINTER=GETPRINTER()

If Empty(xPRINTER)
Else
	Set PRINTER TO NAME(xPRINTER)
	Set Console OFF
	Set Printer ON &&PROMPT= Para Abrir Cuadro de Dialogo de Propiedades de Impresora
	SET PRINTER FONT 'Courier New',8 Style 'N'

  * INICIO DEL REPORTE
  * ---------------	
  Do Case 
	Case nREP=26
		Calculate CNT() To NT
		SET PRINTER FONT 'Courier New',9 Style 'N'
		
		DO R26 With 4,WLI,17,Str(ID,5)+Str(CORR,3)+STR(CORA,3)
  EndCase	  

  * ---------------
  * FIN DEL REPORTE
  Set Printer OFF
  Set Console ON
  Close Printer
EndIf

PROCEDURE R26
*---------------------------------------------------------------------*
PARAMETERS COLS,ROWS,COLW,FIELD,NTIT4
DO WHILE MOD(RECCOUNT(),COLS) # 0
APPE BLANK
ENDD
GO TOP
PGCOUNT=1
PRINTED=0
NVAC=0
SET CONS OFF
SET PRIN ON
DO WHILE PRINTED<RECCOUNT() 
?'  Hato     : '+Q36+'   '+nom 
?'  Area     : VIENTRES'
?'  Reporte  : '+UPPER(NTIT)
?'  Fecha    : '+DTOC(DATE())+'  Hora : '+TIME()
?'  Pagina   : '+STR(PGCOUNT,2,0)+space(65)+[DAIRYFOX v.4.0]
?'  Periodo  : CAMBIOS'
?'  '+REPLI('-',94)
?
*?'      ID  Lote >               ID  Lote >               ID  Lote >               ID  Lote >'        
?'  Lote      ID >           Lote      ID  >          Lote      ID  >          Lote      ID  >'        
?'  '+REPLI('-',94)

ONTHISPAGE=MIN(ROWS*COLS,RECCOUNT()-PRINTED)
ROWS=MIN(ROWS,(ONTHISPAGE/COLS))
THISROW=1
DO WHILE THISROW<=ROWS .AND. PRINTED<=RECCOUNT()
THISCOL=1
If ID>0
*?space(2),LEFT(STR(ID,5)+'   '+STR(CORA,3)+'  '+STR(CORR,3),COLW)+SPACE(MAX(0,COLW-LEN(FIELD)))
?space(2),STR(CORA,3)+'   '+LEFT(STR(ID,5)+'  '+STR(CORR,3),COLW)+SPACE(MAX(0,COLW-LEN(FIELD)))

NVAC=NVAC+1
endi
PRINTED=PRINTED+1
DO WHILE THISCOL<COLS .AND. PRINTED<=RECCOUNT()
SKIP ROWS
If ID>0
*??SPACE(2),LEFT(STR(ID,5)+'   '+STR(CORA,3)+'  '+STR(CORR,3),COLW)+SPACE(MAX(0,COLW-LEN(FIELD)))
??space(2),STR(CORA,3)+'   '+LEFT(STR(ID,5)+'  '+STR(CORR,3),COLW)+SPACE(MAX(0,COLW-LEN(FIELD)))
NVAC=NVAC+1
endi
THISCOL=THISCOL+1
PRINTED=PRINTED+1
ENDD
THISROW=THISROW+1
SKIP (((COLS-1)*ROWS)-1)*-1

IF THISROW-1=ROWS .AND. PRINTED<RECCOUNT()
?'  '+REPLI('-',94)
EJECT
ENDI

IF PRINTED=RECCOUNT()
?'  '+REPLI('-',94)
?'   TOTAL = '+STR(NVAC,4)
ENDI
ENDD
*EJECT
PGCOUNT=PGCOUNT+1
SKIP ((COLS-1)*ROWS)
ENDD
RETURN


