PROCEDURE TITULO1
* ---------------
PARAMETERS xAREA
	?'  Hato     : '+Q36+'   '+nom 
	?'  Area     : '+xAREA
	?'  Reporte  : '+Upper(NTIT) 
	?'  Fecha    : '+DTOC(DATE())+'  Hora : '+TIME()
	?'  Pagina   : '+STR(PAG,2,0)+space(70)+TVersion
	?'  Periodo  : '+NTIT2
RETURN


PROCEDURE TITULO2
* ---------------
PARAMETERS xAREA
	?'  Hato     : '+Q36+'   '+nom 
	?'  Area     : '+xAREA
	?'  Reporte  : '+Upper(NTIT) 
	?'  Fecha    : '+DTOC(DATE())+'  Hora : '+TIME()
	?'  Pagina   : '+STR(PAG,2,0)+space(70)+TVersion
	?'  ID Numero: '+NTIT2
RETURN


PROCEDURE TITULO3
* ---------------
PARAMETERS xAREA
	?'  Hato     : '+Q36+'   '+nom 
	?'  Area     : '+xAREA
	?'  Reporte  : '+Upper(NTIT) 
	?'  Fecha    : '+DTOC(DATE())+'  Hora : '+TIME()
	?'  Pagina   : '+STR(PAG,2,0)+space(60)+TVersion
	?'  Periodo  : '+NTIT2
RETURN

PROCEDURE TITULO5
* ---------------
PARAMETERS xAREA
	?'  Hato     : '+Q36+'   '+nom 
	?'  Area     : '+xAREA
	?'  Reporte  : '+NTIT
	?'  Fecha    : '+DTOC(DATE())+'  Hora : '+TIME()
	?'  Pagina   : '+STR(PAG,2,0)+space(70)+TVersion
RETURN

PROCEDURE TITULO6
* ---------------
PARAMETERS xAREA,xSTATUS
	?'  Hato     : '+Q36+'   '+nom 
	?'  Area     : '+xAREA
	?'  Reporte  : '+Upper(NTIT) 
	?'  Fecha    : '+DTOC(DATE())+'  Hora : '+TIME()
	?'  Pagina   : '+STR(PAG,2,0)+space(70)+TVersion
	?'  ID Numero: '+NTIT2+'      Categoria : '+xSTATUS
RETURN

PROCEDURE TITULO7
* ---------------
PARAMETERS xAREA
	?'  Hato     : '+Q36+'   '+nom 
	?'  Area     : '+xAREA
	?'  Reporte  : '+Upper(NTIT) 
	?'  Fecha    : '+DTOC(DATE())+'  Hora : '+TIME()
	?'  Pagina   : '+STR(PAG,2,0)+space(100)+TVersion
	?'  Periodo  : '+NTIT2
RETURN

PROCEDURE TITULO8
* ---------------
PARAMETERS xAREA
	?'  Hato     : '+Q36+'   '+nom 
	?'  Area     : '+xAREA
	?'  Reporte  : '+Upper(NTIT) 
	?'  Fecha    : '+DTOC(DATE())+'  Hora : '+TIME()
	?'  Pagina   : '+STR(PAG,2,0)+space(70)+TVersion
	?'  ID Arete : '+NTIT2
RETURN

PROCEDURE TITULO10
* ---------------
PARAMETERS xAREA
	?'  Hato     : '+Q36+'   '+nom 
	?'  Area     : '+xAREA
	?'  Reporte  : '+Upper(NTIT) 
	?'  Fecha    : '+DTOC(DATE())+'  Hora : '+TIME()
	?'  Pagina   : '+STR(PAG,2,0)+space(70)+TVersion
	?'  ID Arete : '+NTIT2
RETURN


PROCEDURE TITULO12
* ----------------
PARAMETERS xAREA
?'  '+NOM
?'  Modo RACION   '+STR(THISFORM.Text2.Value,3)+'  '+DRACION.NOMB+'     '+DTOC(DATE())+'   Hora : '+TIME()
?'  '+Replicate('-',xpunto)
?'       Ingrediente         CARGAR   Lectura   |   LOTE  DESCARGAR  Lectura S'
?'  '+Replicate('-',xpunto)
RETURN


PROCEDURE TITULO13
* ---------------
PARAMETERS xAREA,xSTATUS
	?'  Hato     : '+Q36+'   '+nom 
	?'  Area     : '+xAREA
	?'  Reporte  : '+Upper(NTIT) 
	?'  Fecha    : '+DTOC(DATE())+'  Hora : '+TIME()
	?'  Pagina   : '+STR(PAG,2,0)+space(66)+"QUICK CARD"
	?'  ID Arete : '+NTIT2+'      Categoria : '+xSTATUS
RETURN

