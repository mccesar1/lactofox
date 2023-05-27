Private HOY2
hide popup foo1
hide popup soo1
push key clear
on key label F1 do CTRLAYUD with VARREAD()
store .F. to BST
store space(10) to SMEDIC
@ 4,0 clea to 24,79
@23,0 to 23,79

do case
 case BAR()=1
      do Parametros

 case BAR()=2
      do Captura

  case BAR()=3
      do Listado

  case BAR()=4
      do Programa

  case BAR()=5
      do Archivo

  case BAR()=6
     do Marcar

  otherwise or LASTKEY()=27

endcase
pop key
@4,0 clea to 24,79
@23,0 to 23,79

*⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
*≥           SUB-RUTINAS           ≥
*¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
PROC PARAMETROS
*----------------------------------*
@22,28 SAY [PARAMETROS PARA INYECTAR BST] colo gr+/&cz
@24,70 say [Esc] colo n/w
@24,74 say [Salir] colo w+/&cz

USE CAMBIO
do while .t.
set colo to w/&cy
SN=[N]
    @ 10,21 FILL TO 18,62  COLO N/N
	@ 9,20 FILL TO 17,60 colo SCHE 16
	@ 9,20 TO 17,60 colo SCHE 16
scatter to bstd
@ 11,22 SAY [Dias en Leche Minimos...........] get fr valid Fr>=15 and Fr<=1085 error [ Mayor de 14 dias ] COLO SCHE 16
@ 12,22 SAY [Dias en Gestacion Maximos.......] get gr valid Gr>=60 and Gr<=280 error [ Rango de 60 a 195 dias ]  COLO SCHE 16
@ 13,22 SAY [Produccion de Leche Minima.....] get hr valid Hr>=1 and Hr<=100 error [ Rango de 1 a 100 Litros ]  COLO SCHE 16
@ 14,22 SAY [Produccion de Leche Maxima....] get kr valid kr>=hr and kr<=100 error [ Rango de 1 a 100 Litros ]  COLO SCHE 16
@ 15,22 SAY [Intervalo en Dias/Aplicaciones...] get IR pict '99' FUNC 'J' valid IR>8 and IR<29 error [ Rango de 8 a 28 dias ] COLO SCHE 16
	READ
set colo to &color
IF LASTKEY()=27 
	gather from bstd
	exit
ENDI
@ 22,68 SAY [Correcto] get SN pict '@M N,S' COLO w/&cz
READ
IF LASTKEY()=27 
	gather from bstd
	exit
ENDI

IF SN='N'
	gather from bstd
	LOOP
ELSE
*-- Cambia las variables --*
Q27=FR
Q28=GR
Q29=HR
Q30=IR
Q31=JR
Q32=KR
exit
*--------------------------*
ENDI
endd
CLOS DATA


PROC CAPTURA
*----------------------------------*

ON KEY LABEL F10 DO TARJETA 
*⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
    RTIT=[CAPTURA DE INYECCIONES BST]
    XID=0
   	AXID=0
	@ 22,27 SAY RTIT colo gr+/&cz
    @22,70 SAY [IDa] COLO N/W
    @23,0 to 23,79
    @24,1 say [F1] COLO N/W
    @24,4 SAY [Ayuda] 
*   @24,11 say [F2] COLO N/W
*   @24,14 SAY [Directorio] 
    @24,55 say [F10] COLO N/W
	@24,59 say [Historial]
    @24,70 SAY [Esc] colo n/w
    @24,74 say [Salir] 

    @ 24,35 GET HOY VALID HOY>=DATE()-180 ;
    .AND. HOY<=DATE() ERROR tit(3) 
    READ TIMEOUT 30
    if lastkey()=27
	    clos data
		@4,0 clea to 24,79
	    return 
    endi
    @ 24,35 SAY HOY COLO W+/&CZ
*¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
** Empieza Seleccion 
** ---------------------------------------------------------------

xchoice=0
@ 22,0 clea to 22,24
@ 22,1 get xchoice function'*TH Manual;Archivo' colo ,,,,,w+/w,,,w/b
read cycle

DO CASE

** Captura Manual
** --------------
CASE xchoice=1

sele 3
USE CONDC order 1
sele 2 
USE BST order 1
sele 1
USE REG order 2

DO WHILE .T.
	AXID=XID
	SN='N'
	XID=0
	MCC=0
	@22,1 clea to 22,24
	@22,1 SAY [ID] GET XID PICT '99999' FUNC 'J,T' colo w+/&cz,N/W
	@22,74 SAY AXID PICT '99999' FUNC 'J' COLO W+/&CZ
	READ
IF LASTKEY()=27 or XID=0
	EXIT
ENDI
	SEEK XID
IF .NOT. FOUND()
	?CHR(7)
	WAIT WIND [ ID No Existe ] TIMEOUT 2
ELSE

DO CASE

CASE np>0 and FB2=B and HOY-FPAR>=Q27 and HOY-UCAL<=Q28 and HOY>UBST and FSEC=B 

DO CASE
CASE STAT='INSEM' or STAT='CARGA'
IF FB2=B
REPL DIA WITH (HOY-UCAL),DAB WITH UCAL-FPAR
ELSE
REPL DIA WITH (HOY-UCAL)-(HOY-FB2),DAB WITH UCAL-FPAR
ENDI
OTHERWISE
REPL DIA with 0,DAB WITH 0
ENDC
REPL DPR WITH HOY-FPAR

DO WHILE .T.
	*------Variables-------------------*
	MDIAS=0
	MCOND=0
	UFECHA=B
    RNP=NP
    *-----------------------------------*

IF Q34="S"
	@22,11 SAY [Condici¢n] GET MCOND PICT '9.9' VALID MCOND>=1 and MCOND<=5  WHEN Q34="S" ERROR [ Escala de 1 a 5 ] colo w+/&cz,n/w
	READ
ELSE
ENDI

if lastkey()=27
exit
else
REPL UBST WITH HOY,MBST WITH Q31
	if MCOND>0
	MCEVE=[Aplic STB]
	REPL FCON WITH HOY,CONDC WITH MCOND
	SELE 3
	APPE BLANK
	REPL ID WITH XID,FECHA WITH HOY,NP WITH A->NP,CONDC WITH MCOND,CDEL WITH A->DPR,CEVE WITH MCEVE

	endi
endi

SELE 2
SEEK XID

IF .NOT. FOUND()
	UFECHA=B
    XNAPLI=1
ELSE
	SCAN WHILE ID=XID FOR NP=RNP
	UFECHA=FECHA
	ENDS
ENDI

	APPE BLANK
	REPL ID WITH A->ID,NP WITH A->NP,FECHA WITH A->UBST,DIM WITH A->DPR,;
	CONDC WITH MCOND,UPESA WITH A->PRM,STAT WITH A->STAT
IF UFECHA=B 
	REPL DIAS WITH 0
ELSE
	REPL DIAS WITH HOY-UFECHA
ENDI
sele 1
exit
endd
*ENDI

CASE FPAR<>B and FB2=B and UBST=HOY and FSEC=B 
SET BELL TO 311,3
??CHR(7)
WAIT WINDOW [ ID ya actualizada ] TIMEOUT 1
SET BELL TO

OTHERWISE
??CHR(7)
wait wind []+tit(6) timeout 1

ENDCASE
ENDI	
AXID=XID
ENDD

** Registro por Archivo
** --------------------
CASE xchoice=2

on key label F10 do ARCHIVO2

sn="N"
	@4,0 clea to 24,79
	@9,4 say [NOTA :] COLO W+/&CZ
	@11,4 fill to 19,77 colo n/n
	@10,3 fill to 18,76 COLO SCHE 15	
	@11,4 say [ Este programa importara la informacion de un archivo externo de nombre ] COLO sche 15
	@12,4 say [ EVENTOS.TXT, debiendo de estar delimitado por comas y encontrarse en   ] colo SCHE 15
	@13,4 say [ el directorio LACTOFOX.                                                ] COLO SCHE 15
	@15,4 say [ Solo aquellos animales cuya ID exista en el archivo principal seran    ] COLO SCHE 15
	@16,4 say [ actualizados, y al termino del proceso el sistema borrara el archivo   ] COLO SCHE 15
	@17,4 say [ externo EVENTOS.TXT.                                                   ] COLO SCHE 15

	@22,(80-LEN(RTIT))/2 SAY RTIT colo gr+/&cz

	@23,0 to 23,79
    @24,1 say [F1] COLO N/W
    @24,4 SAY [Ayuda] 
*   @24,11 say [F2] COLO N/W
*   @24,14 SAY [Directorio] 
    @24,55 say [F10] COLO N/W
	@24,59 say [Archivo]
	@24,35 SAY HOY COLO W+/&CZ
	@24,68 say [Continuar] get sn pict '@M N,S' COLO n/w
read

if lastkey()=27 or sn="N" 
	retu
else
	if not file("EVENTOS.TXT")
		??chr(7)
		wait wind [ Archivo no existente ]+"EVENTOS.TXT" timeout 2
	else
		create table abst (ID n(5),DCON n(3,1))
		appe from eventos.txt deli 

		use abst in 1 alias XBS
		use reg order 2 in 2 alias REG
		use bst order 1 in 3 alias BST
		use condc order 1 in 4 alias CCO
		sele 1
		@22,1 say [ID] colo w+/b
		scan
			xid=XBS.id
			xcon=XBS.dcon
			sele 2
			seek xid
			if found() and fb2=B and hoy>REG.ubst and hoy>=REG.fpar 
					xnp=REG.np
					xdel=(hoy-REG.fpar)
					xstat=REG.stat
					mPRM=REG.prm
					if REG.fsec#B 
						mPRM=0
						xdel=0
					endi	
				@22,4 say xid pict'#####' colo n/w
				@22,11 SAY [OK] colo w+/&cz
				repl REG.UBST with hoy
			
				** Verifica Condicion Corporal
				** ---------------------------
				if xcon>0 and xcon<=5.0
					repl REG.fcon with hoy,REG.condc with xcon
				sele 4
				seek xid
				if not found()
					appe blank
					repl ID with xid,NP with xnp,FECHA with HOY,;
					CONDC with xcon,CDEL with xdel,CEVE with [Aplic BST],;
					CPROD with mPRM
				else
					scan while ID=xid
					endscan
					skip-1
						do case
							case FECHA=hoy
								repl CONDC with xcon
							case FECHA<hoy
							    appe blank
								repl ID with xid,NP with xnp,;
								FECHA with hoy,CONDC with xcon,;
								CDEL with xdel,CEVE with [Aplic BST],;
								CPROD with mPRM
							otherwise
						endcase
				endif
            endif
			** Da de Alta en BST.dbf
			** ---------------------
			sele 3		
			seek xid
			if not found()
				ufecha=B
			    xnapli=1
			else
				scan while id=xid for np=REG.np
					ufecha=fecha
				endscan
			endif

			appe blank
			repl id with REG.id,np with REG.np,fecha with REG.ubst,;
			dim with xdel,condc with xcon,upesa with REG.prm,;
			stat with REG.stat
			if ufecha=B 
				repl dias with 0
			else
				repl dias with hoy-ufecha
			endif
		   endif
		   sele 1
		endscan
    endif
endif

ENDCASE

on key label F10 *
close all
erase ABST.DBF
erase EVENTOS.TXT
@ 4,0 clea to 24,79
RETURN


*   ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
*   ≥        ARCHIVO EXTERNO           ≥
*   ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
PROC ARCHIVO2
on key label F10 *
if file("EVENTOS.TXT")
	modi file EVENTOS.TXT
else
	??chr(7)
	wait wind [ Archivo no existente ]+"EVENTOS.TXT" timeout 2
endif
on key label F10 do ARCHIVO2
return	

*----------------------------------*
          PROC LISTADO
*----------------------------------*
HOY2=HOY
HOY=DATE()
private xorden,xchoice,xchoice2,ncho1,ncho2,ncho3,mc
xtit=[ID]
xorden="2"
xchoice=1
xchoice2=2
store 1 to ncho1,ncho2,ncho3
mc=0
XSTAT=[CARGADAS]
ntipo=1

@21,0 to 21,79
@22,24 SAY [LISTADO DE VACAS PARA INYECTAR BST] colo gr+/&cz
@23,0 to 23,79
@ 24,35 GET HOY VALID HOY>=DATE() and HOY<=DATE()+Q30 ERROR tit(3)
read
	if lastkey()=27
		close all
		HOY=HOY2
		return
	endif

defi wind orden from 7,19 to 18,61 title [ Par†metros ] shadow colo sche 16
acti wind orden
@ 1,2 say [Orden] colo n/w
@ 2,3 get xchoice func'*R Identificaci¢n;Corral/Lote' colo ,,,,,gr+/b,,,w+/w

@ 5,2 SAY [Incluir] colo n/w
@ 6,3 get ncho1 func'*C Abiertas' colo ,,,,,,,,w+/w
@ 7,3 get ncho2 func'*C Inseminadas' colo ,,,,,,,,w+/w
@ 8,3 get ncho3 func'*C Pre§adas' colo ,,,,,,,,w+/w

@ 2,25 say [Corral] colo n/w
@ 3,26 get mc pict'99' when xchoice=2 colo ,n+/w
@ 3,25 say [(  ) 0=Todos]

@5,25 say [Reporte] colo n/w
@6,26 get ntipo func'*R Resumen;General' colo ,,,,,gr+/b,,,w+/w

@ 9,20 get xchoice2 func'*H Aceptar;Cancelar' DEFAULT 1 colo ,,,,,gr+/b,,,n+/w

read cycle valid lastkey()=13 or lastkey()=27

	if lastkey()=27 or xchoice2=2
		rele wind orden
		clos all
		return
	else
	rele wind orden
	
	** Inicia Calculo de listado
    ** -------------------------		
		do case
			case xchoice=1
				 xorden="2"
				 xtit=[ID]
				 xtit2=[x ID]	
			
			case xchoice=2
				 xorden="4"
 				 xtit=[CORR,ID]
        	     xtit2="x Corral "+str(mc,2)
		endcase

	do case
		case ncho1=1 and ncho2=0 and ncho3=0
			 xfilt="stat#[CARGA] and stat#[INSEM]"
		case ncho1=1 and ncho2=1 and ncho3=0
			 xfilt="stat#[CARGA]" 
		case ncho1=0 and ncho2=1 and ncho3=1
			 xfilt="stat#[FRESC] and stat#[ANEST] and stat#[ABORT] and stat#[LIMPI] and stat#[SUCIA] and stat#[VACIA] and stat#[TRATA]"
		case ncho1=0 and ncho2=0 and ncho3=1
			 xfilt="stat=[CARGA]"
		case ncho1=1 and ncho2=0 and ncho3=1
			 xfilt="stat#[INSEM]"
		case ncho1=0 and ncho2=1 and ncho3=0
			 xfilt="stat=[INSEM]"
		otherwise
			 xfilt="np>0"	
	endcase	
endif

if mc=0
select ID,CORR,STAT,CONDC,FPAR,UCAL,PRM,UBST,OBS,FSEC,NS from REG where FB2=B and FSEC=B ;
       and ubst<=HOY-(Q30) and STB#"N" and (hoy-fpar)>=Q27 and PRM>=Q29 ;
       and PRM<=Q32 and &xfilt and not (stat=[CARGA] and (hoy-ucal)>Q28);
       or STB="S" and FSEC=B and FB2=B ORDER BY &xtit INTO TABLE vbst
else
select ID,CORR,STAT,CONDC,FPAR,UCAL,PRM,UBST,OBS,FSEC,NS from REG where FB2=B and FSEC=B ;
       and ubst<=HOY-(Q30) and STB#"N" and (hoy-fpar)>=Q27 and PRM>=Q29 ;
       and PRM<=Q32 and &xfilt and not (stat=[CARGA] and (hoy-ucal)>Q28);
       and CORR=mc or STB="S" and CORR=mc and FSEC=B and FB2=B ORDER BY &xtit INTO TABLE vbst
endif

@22,1 say xtit2 colo w+/b
@ 4,1 say [     ID  Corral  Estado   Condicion        Ultimo           Prod    Ultima    ] colo n/w
@ 5,1 say [    Num   Num    Reprod   Corporal  DEL    Calor      DUC  Actual  Inyeccion  ] colo n/w
@ 24,70 SAY [Espere] colo n*/w

ROW=7
COL=3
DAT=0
GO TOP
@ 24,1 say [Total] colo w+/&cz
@ 24,6 CLEA TO 24,11

scan while inkey()#6
@ 24,8 SAY DAT+1 PICT '9999'
@ row,COL SAY ID colo gr+/&cz
@ row,COL+9 SAY CORR 
@ row,COL+15 SAY STAT 
@ row,COL+26 SAY CONDC
@ row,COL+34 SAY hoy-fpar PICT '999' colo w+/b
@ row,COL+40 SAY UCAL
@ row,COL+52 SAY hoy-ucal pict'###' colo g+/b
@ row,COL+57 SAY PRM colo r+/&cz
@ row,COL+65 SAY UBST colo g+/&cz


IF ROW=20 
	@ 24,58 CLEA TO 24,78
	@ 24,71 SAY [Enter] COLO N/W
	@ 24,77 SAY CHR(16) colo w+/&cz
	DO WHILE INKEY() # 13
	IF LASTKEY()=6
		@ 6,2 CLEA TO 20,77
		GO BOTTOM
	EXIT
	ENDI
	ENDD

@ 6,2 CLEA TO 20,77
@ 24,58 CLEA TO 24,78
ROW=6
ENDI
DAT=DAT+1
ROW=ROW+1
ENDSCAN
@ 24,67 clea to 24,78
??CHR(7)
@ 24,70 SAY [Enter] COLO N/W
@ 24,76 SAY CHR(4) colo w+/&cz
DO WHILE INKEY() # 13
ENDD


IF DAT=0
ELSE
SN='N'
@ 24,67 SAY [Imprimir] GET SN PICT '@M S,N' colo n/w
READ
IF SN='N' or LASTKEY()=27
ELSE

IF SN='S' and SYS(13)='READY'

*NTIPO=[G]
*@ 22,60 say [General/Resumen] get ntipo PICT "@M G,R" 
*READ

IF LASTKEY()=27
SET FILT TO
ELSE

DO CASE

CASE NTIPO=2
SET CONS OFF
SET PRIN ON
*…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª
*  Cambio a Letra 12 cpi
	???&q19
*»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº
NVE=0
PAG=1
NVAC=0
NTIT=[CONTROL DE STB]
NTIT2=[LISTADO DE VACAS PARA INYECTAR]
GO TOP
DO WHILE .NOT. EOF()
?'  Rancho   : '+Q36+'   '+nom 
?'  Area     : VIENTRES'
?'  Reporte  : '+NTIT
?'  Fecha    : '+DTOC(DATE())+'  Hora : '+TIME()
?'  Pagina   : '+STR(PAG,2,0)+space(70)+[LACTOFOX]
?'  Periodo  : '+NTIT2
?'  '+REPLI(CHR(254),94)
?
?'     ID   CORRAL ESTADO   FECHA                    ULTIMO           NUM        PROD  ULTIMA'
?'    VACA   NUM   REPROD   PARTO     OBSERV   DEL   CALOR      COND  SER   DUC  ACTU  INYECC'
?'  '+REPLI('-',94)

SCAN while inkey()#6
?'   '+STR(ID,5)+'    '+STR(CORR,2)+'   '+STAT+'    '+fx(FPAR)+'  '+OBS+'   '+STR((DATE())-FPAR-(DATE()-FSEC),3)+'   '+;
fx(UCAL)+'   '+str(condc,4,1)+'   '+STR(NS,2)+'   '+STR(DATE()-UCAL,3)+'  '+STR(PRM,4,1)+'  '+fx(UBST)
NVAC=NVAC+1
NVE=NVE+1
if nve=WLI
?'  '+REPLI(CHR(254),94)
EJEC
PAG=PAG+1
NVE=0
SKIP
EXIT
ENDI

ENDS
ENDD
?'  '+REPLI(CHR(254),94)
?
?'   TOTAL = '+STR(NVAC,4)
EJEC
SET PRIN OFF
SET CONS ON
SET FILT TO


CASE NTIPO=1
NTIT=[CONTROL DE STB]
NTIT2=[LISTADO DE VACAS POR INYECTAR]
@ 24,70 SAY [Espere] colo n*/w
GO TOP


if xchoice=1
	COPY TO LOTES FIELDS ID 
	CLOS ALL
	SELE 2
	USE CAMBIO
	SELE 1
	USE LOTES
	@ 24,66 CLEA TO 24,77
	DO R4 in ctrlvbst WITH 19,(WLI),5,STR(ID,5)
else
	COPY TO LOTES FIELDS ID,CORR,STAT,UCAL 
	CLOS ALL
	SELE 2
	USE CAMBIO
	SELE 1
	USE LOTES
	@ 24,66 CLEA TO 24,77
	DO R5 in ctrlvbst WITH 6,(WLI),12,STR(ID,5)+STAT+STR(CORR,4)
endif

SET PRIN OFF
SET CONS ON

ENDCASE
ENDI
ELSE
??CHR(7)
WAIT WIND [ IMPRESORA APAGADA O DESCONECTADA ] TIMEOUT 2
ENDI

ENDI
ENDI
hoy=hoy2
clos all
eras VBST.DBF
return

*----------------------------------*
         PROC PROGRAMA
*----------------------------------*
STORE ctod("") TO FIN1,FIN2
@ 21,0 to 21,79
@22,31 SAY [PROGRAMA BST ACTUAL] colo gr+/&cz
@24,70 say [Esc] colo n/w
@24,74 say [Salir] colo w+/&cz

@ 10,20 say [NOTA:] colo w+/b
@ 12,19 FILL TO 17,65 COLO N/N
@ 11,18 fill to 16,63 COLO SCHE 15
@ 12,19 say [  Este programa emitira una lista de todas  ] COLO SCHE 15
@ 13,19 say [  aquellas vacas cuya siguiente aplicacion  ] colo sche 15
@ 14,19 say [  de BST se encuentre en un rango de fechas ] COLO SCHE 15
@ 15,19 say [  a partir de HOY.                          ] COLO SCHE 15

@ 24,26 SAY [Del] get FIN1 valid FIN1#B and FIN1>=date() error tit(3)
@ 24,42 SAY [Al] get FIN2 valid FIN2#B and FIN2>=FIN1 and FIN2<=date()+60 error tit(3)
READ

IF lastkey()=27
 return
else
@ 4,0 clea to 20,79
endi 

@ 4,1 say [     ID  Corral  Estado   Fecha             Ultimo          Prod    Proxima   ] colo n/w
@ 5,1 say [    Num   Num    Reprod   Parto       DEL   Calor     DUC  Actual  Inyeccion  ] colo n/w
@ 24,1 say [Total] colo w+/&cz
ROW=7
COL=3
DAT=0
use reg 
set filt to FPAR#B
INDE ON DTOS(UBST+Q30)+STR(corr)+str(ID) TO CORR 
use reg inde corr
go top
SET FILT TO UBST+Q30>=FIN1 and UBST+Q30<=FIN2 and STB#"N"

@ 24,60 clea to 24,78
@ 24,70 SAY [Espere] colo n*/w

SCAN FOR np>0 and FB2=B and FIN2-FPAR>=Q27 and FIN2-UCAL<=Q28 ;
and PRM>=Q29 and PRM<=Q32 and FSEC=B while inkey()#6

@ 24,8 SAY DAT+1 PICT '9999'
@ ROW,COL SAY ID colo gr+/&cz
@ ROW,COL+9 SAY CORR 
@ ROW,COL+15 SAY STAT
@ ROW,COL+23 SAY FPAR
@ ROW,COL+36 SAY date()-fpar PICT '999'
@ ROW,COL+41 SAY UCAL
if STAT#[CARGA] and STAT#[INSEM]
@ ROW,COL+52 SAY DIA Pict '999'
else
@ ROW,COL+52 SAY date()-UCAL Pict '999'
endi
@ ROW,COL+57 SAY PRM colo r+/&cz
@ ROW,COL+64 SAY UBST+Q30 colo g+/&cz

IF ROW=20 
@ 24,58 CLEA TO 23,78
@ 24,70 SAY [Enter] COLO N/W
@ 24,76 SAY CHR(16) colo w+/&cz
DO WHILE INKEY() # 13
IF LASTKEY()=6
@ 6,2 CLEA TO 20,77
GO BOTTOM
EXIT
ENDI
ENDD

@ 6,2 CLEA TO 20,79
@ 24,58 CLEA TO 24,78
ROW=6
ENDI
DAT=DAT+1
ROW=ROW+1
ENDS
@ 24,67 clea to 24,78
??CHR(7)
@ 24,70 SAY [Enter] COLO N/W
@ 24,76 SAY CHR(4) colo w+/&cz
DO WHILE INKEY() # 13
ENDD

IF DAT=0
ELSE
SN='N'
@ 24,69 SAY [Imprimir] GET SN PICT '@M S,N' colo n/w
READ
IF SN='N' or LASTKEY()=27
ELSE
IF SN='S' and SYS(13)='READY'
SET CONS OFF
SET PRIN ON
*…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª
*  Cambio a Letra 17 cpi
	???&q21
*»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº
NVE=0
PAG=1
NVAC=0
NTIT=[CONTROL DE STB]
NTIT2=DTOC(FIN1)+'  Al '+DTOC(FIN2)
GO TOP
DO WHILE .NOT. EOF()
?'  Rancho   : '+Q36+'   '+nom 
?'  Area     : VIENTRES'
?'  Reporte  : '+NTIT
?'  Fecha    : '+DTOC(DATE())+'  Hora : '+TIME()
?'  Pagina   : '+STR(PAG,2,0)+space(70)+[LACTOFOX]
?'  Periodo  : '+NTIT2
?'  '+REPLI(CHR(254),94)
?
?'     ID   CORRAL ESTADO   FECHA                    ULTIMO    SEMEN       N  DIAS  PROD PROXIMA'
?'    VACA   NUM   REPROD   PARTO     OBSERV   DEL   CALOR     UTILIZADO   S  INSE  ACTU  INYECC'
?'  '+REPLI('-',94)

SCAN FOR np>0 and FB2=B and FIN2-FPAR>=Q27 and FIN2-UCAL<=Q28 ;
and PRM>=Q29 and PRM<=Q32 and FSEC=B while inkey()#6

?'   '+STR(ID,5)+'    '+STR(CORR,2)+'   '+STAT+'    '+fx(FPAR)+'  '+OBS+'   '+STR((DATE())-FPAR-(DATE()-FSEC),3)+'   '+;
fx(UCAL)+'  '+IDT+'  '+STR(NS,2)+'   '+STR(DATE()-UCAL,3)+'  '+STR(PRM,4,1)+'  '+fx(UBST+Q30)
NVAC=NVAC+1
NVE=NVE+1

if nve=WLI
?'  '+REPLI(CHR(254),94)
EJEC
PAG=PAG+1
NVE=0
SKIP
EXIT
ENDI

ENDS
ENDD
?'  '+REPLI(CHR(254),94)
?
?'   TOTAL DE VACAS = '+STR(NVAC,4)
EJEC
SET PRIN OFF
SET CONS ON
SET FILT TO
ELSE
??CHR(7)
WAIT WIND [ IMPRESORA APAGADA O DESCONECTADA ] TIMEOUT 2
ENDI
ENDI
ENDI

*----------------------------------*
        PROC ARCHIVO
*----------------------------------*
USE REG order 2
@24,2 say [F1] COLO N/W
@24,5 SAY [Ayuda] 
@22,27 SAY [ARCHIVO DE INYECCIONES BST] colo gr+/&cz
@24,30 say [* Solo en Impresora *] colo r+/&cz

XID=0
NTIT=[TARJETA DE APLICACIONES STB]
@ 24,70 SAY [Esc] colo n/w
@ 24,74 say [Salir] colo w+/&cz
NV=0
DO WHILE .T.
XID=0
XNP=0
@22,2 SAY [ID] GET XID PICT '99999'colo w+/&cz
@22,11 CLEA TO 22,15

READ
IF XID=0 or LASTKEY()=27
EXIT
ENDI

seek xid
	if not found()
		??chr(7)
		wait wind tit(1) timeout 1
	else

	do case
		case np>0 
		xnp=NP			
			if fb2#B
				??chr(7)
				wait wind [Vaca dada de Baja ] timeout 1
			endi

IF SYS(13)='READY'
SET CONS OFF
SET PRIN ON
PAG=1
DO CTRLVRP0
?'     ID VACA ......  '+str(XID,5,0)+'        Fecha de Parto ...... '+fx(fpar)
?SPACE(2),REPLI('-',90)
USE BST order 1
SEEK XID

DO CASE
CASE .NOT. FOUND()
?
?
?'                                         No tiene Datos'
?
?
CASE FOUND()
?'      Parto    Fecha de    Dias en  Intervalo  Numero   Prod   Condicion ' 
?'      Numero   Aplicacion   Leche    en Dias   Aplic.   Leche  Corporal  ' 
?'       '+str(np,2)
xcount=0
SCAN FOR ID=XID and NP=xnp
REPL NAPLI WITH XCOUNT
xcount=xcount+1
?'                '+fx(Fecha)+'    '+STR(Dim,4)+'      '+STR(Dias,4)+'       '+STR(xcount,2)+'     '+STR(upesa,4,1)+'       '+STR(condc,3,1)
*?'        '+str(np,2)+'      '+fx(Fecha)+'    '+STR(Dim,4)+'      '+STR(Dias,4)+'       '+STR(xcount,2)+'     '+STR(upesa,4,1)+'       '+STR(condc,3,1)+'        '+str(pto,2)
ENDS
?
ENDC
?SPACE(2),REPLI(CHR(254),90)
EJEC
SET PRIN OFF
SET CONS ON
ELSE
??CHR(7)
WAIT WIND [ IMPRESORA APAGADA O DESCONECTADA ] TIMEOUT 2
ENDI

ENDC
ENDI
use reg order 2
ENDD
CLOS DATA

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
*…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª
*  Cambio a Letra 17 cpi
	???&q21
*»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº
DO WHILE PRINTED<RECCOUNT() &&
?'  Rancho   : '+Q36+'   '+nom 
?'  Area     : VIENTRES'
?'  Reporte  : '+NTIT
?'  Fecha    : '+DTOC(DATE())+'  Hora : '+TIME()
?'  Pagina   : '+STR(PGCOUNT,2,0)+space(100)+[LACTOFOX]
?'  Periodo  : '+NTIT2+'  x ID'
?'  '+REPLI(CHR(254),132)
*?
*?'      ID    ID'
*?SPACE(1),REPLI('-',132)
ONTHISPAGE=MIN(ROWS*COLS,RECCOUNT()-PRINTED)
ROWS=MIN(ROWS,(ONTHISPAGE/COLS))
THISROW=1
DO WHILE THISROW<=ROWS and PRINTED<=RECCOUNT()
THISCOL=1
if ID>0
?space(1),LEFT(STR(ID,5,0),COLW)+SPACE(MAX(0,COLW-LEN(FIELD)))
NVAC=NVAC+1
endi
PRINTED=PRINTED+1
DO WHILE THISCOL<COLS and PRINTED<=RECCOUNT()
SKIP ROWS
if ID>0
??space(1),LEFT(STR(ID,5,0),COLW)+SPACE(MAX(0,COLW-LEN(FIELD)))
NVAC=NVAC+1
endi
THISCOL=THISCOL+1
PRINTED=PRINTED+1
ENDD
THISROW=THISROW+1
SKIP (((COLS-1)*ROWS)-1)*-1

IF THISROW-1=ROWS and PRINTED<RECCOUNT()
?'  '+REPLI(CHR(254),132)
ENDI

IF PRINTED=RECCOUNT() 
?'  '+REPLI(CHR(254),132)
?'  TOTAL = '+STR(NVAC,4,0)
ENDI
ENDD
EJECT
PGCOUNT=PGCOUNT+1
SKIP ((COLS-1)*ROWS)
ENDD
CLOS DATA
ERAS LOTES.DBF

PROCEDURE R5
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
*…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª
*  Cambio a Letra 12 cpi
	???&q19
*»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº
DO WHILE PRINTED<RECCOUNT() &&
?'  Rancho   : '+Q36+'   '+nom 
?'  Area     : VIENTRES'
?'  Reporte  : '+NTIT
?'  Fecha    : '+DTOC(DATE())+'  Hora : '+TIME()
?'  Pagina   : '+STR(PGCOUNT,2,0)+space(70)+[LACTOFOX]
?'  Periodo  : '+NTIT2
?'  '+REPLI(CHR(254),94)
?
?'      ID  Corral    ID  Corral    ID  Corral    ID  Corral    ID  Corral    ID  Corral'
?SPACE(1),REPLI('-',94)
ONTHISPAGE=MIN(ROWS*COLS,RECCOUNT()-PRINTED)
ROWS=MIN(ROWS,(ONTHISPAGE/COLS))
THISROW=1
DO WHILE THISROW<=ROWS and PRINTED<=RECCOUNT()
THISCOL=1
if ID>0
?space(2),LEFT(STR(ID,5,0)+'    '+STR(corr,2),COLW)+SPACE(MAX(0,COLW-LEN(FIELD)))
NVAC=NVAC+1
endi
PRINTED=PRINTED+1
DO WHILE THISCOL<COLS and PRINTED<=RECCOUNT()
SKIP ROWS
if ID>0
??space(2),LEFT(STR(ID,5,0)+'    '+STR(corr,2),COLW)+SPACE(MAX(0,COLW-LEN(FIELD)))
NVAC=NVAC+1
endi
THISCOL=THISCOL+1
PRINTED=PRINTED+1
ENDD
THISROW=THISROW+1
SKIP (((COLS-1)*ROWS)-1)*-1

IF THISROW-1=ROWS and PRINTED<RECCOUNT()
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

** Tarjeta de Consulta
** -------------------
PROCEDURE TARJETA
ON KEY LABEL F10 *
USE REG order 2
SEEK XID
IF XID=0
*ON KEY LABEL F10 DO TARJETA
*RETU
ELSE
DEFI WIND ACTVAC FROM 4,0 to 20,78 PANEL TITLE [ R E G I S T R O ] shadow COLO sche 17
DEFI WIND PROG FROM 6,51 TO 22,78  PANEL title [ A G E N D A ]  shadow FLOAT COLO SCHE 17
ACTI WIND ACTVAC
EDADN=((DATE()-FNAC)-(DATE()-FB2))/30.4
ECP=STR(INT(EDADN/12),2)+'-'+STR(MOD(EDADN,12),2)
REPL DPR WITH (DATE()-FPAR)-(DATE()-FSEC),EDAD WITH (FPAR-FNAC)/30.4 ,DSC WITH DATE()-FSEC
EDADN=((DATE()-FNAC)-(DATE()-FB2))/30.4

if FB2=B 
	if DPR>Q6 and NS<1 and UCAL=B and UTRA=B
		repl STAT WITH 'ANEST'
	endi

** CALCULO DE DIAS INSEMINADA Y DIAS ABIERTOS 
** ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ-
if stat=[INSEM] .or. stat=[CARGA]
	repl dia with date()-ucal,dab with ucal-fpar
else
	repl dia with 0,dab with date()-fpar
endi	

IF FSEC<>B
REPL DSC WITH DATE()-FSEC
ENDI
	** Reemplaza el valor de PAC
	** -------------------------
	Qpac=PAC+PRM*((DATE()-PESA)-(DATE()-FSEC))

IF EDAD<=36
DO 305
ELSE
IF EDAD>36
DO 305M
ENDI
ENDI
IF P305>0
REPL M305 WITH P305*FAC
ELSE
REPL M305 WITH 0
ENDI
IF FB2<>B
ENDI
ENDI

@ 1,3 SAY  [Vaca Num......] 
@ 1,18 SAY ID PICT '99999' FUNC 'J' COLO W+/&CZ/N
@ 2,3 SAY  [F. Nacim.] 
@ 2,13 SAY FNAC COLO sche 13
@ 3,3 say [Edad..........]
@ 3,18 SAY ECP FUNC 'J,T' COLO sche 13
@ 4,3 SAY [Origen..... ]
@ 4,15 say proc FUNC 'J' COLO sche 13
@ 5,3 SAY  [ID Madre......]
@ 5,18 SAY IDM PICT '99999' FUNC 'J' COLO sche 13
@ 6,3 SAY  [ID Padre..]  
@ 6,14 say idp FUNC '!,J,T' COLO sche 13
@ 7,3 SAY [F Parto..] 
@ 7,13 SAY FPAR COLO sche 13
@ 8,3 SAY [Numero...........] 
@ 8,21 SAY NP COLO sche 13
@ 9,3 SAY [Observ.......]
@ 9,17 say OBS COLO sche 13
@ 10,3 SAY [Sexo Cria........]
@ 10,21 SAY SEXC COLO sche 13 

@ 11,3 SAY [ID Cria 1.....] 
@ 11,18 SAY IDC COLO sche 13
@ 12,3 SAY [ID Cria 2.....] 
@ 12,18 SAY IDC2 COLO sche 13  
@ 13,3 say [Codigos....] COLO SCHE 17
@ 13,15 SAY codigo func'j' COLO r+/n

@ 1,27 SAY [Edo. Reprod.....] 
@ 1,44 SAY STAT COLO W+/&CZ/N
@ 3,27 SAY [U. Rev/Tx..] 
@ 3,39 SAY UTRA COLO sche 13  
@ 4,27 SAY [Ult. Calor.] 
@ 4,39 SAY UCAL COLO sche 13 
@ 5,27 say [F. Aborto..] 
@ 5,39 SAY ABTO COLO sche 13 
@ 6,27 SAY [Ult. Medic.........]  
@ 6,47 SAY MED1 PICT '99' FUNC 'J' COLO sche 13
@ 7,27 SAY [ID Toro.....] 
@ 7,40 say IDT FUNC '!,J,T' colo sche 13  
@ 8,27 SAY [Servicios..........] 
@ 8,47 say ns COLO sche 13
@ 9,27 SAY [ID Tecnico........]
@ 9,46 SAY UTEC COLO sche 13
@ 10,27 SAY [Palpacion..]
@ 10,39 SAY PALP  COLO sche 13
@ 11,27 SAY [ID Tecnico........]
@ 11,46 SAY TECP COLO sche 13
*@ 12,27 SAY [Cuerno Gestante.....]
*@ 12,27 say CGES colo sche 13  && 98.01.19 Pendiente
@ 13,27 say [Observ.] 
@ 13,35 say OBSC func'j' colo sche 13

@ 1,53 SAY [A Secas...] 
@ 1,64 SAY FSEC COLO sche 13
@ 2,53 say [U Pesada..]
@ 2,64 say pesa COLO sche 13
@ 3,53 say [Ult. Prod.......]
@ 3,70 say prm pict '99.9' func 'j' COLO sche 13
@ 4,53 say [Prod Diaria.....]
@ 4,70 say prx pict '99.9' func 'j' COLO sche 13
@ 5,53 say [Prod Acum...]
@ 5,69 say PAC+PRM*((DATE()-PESA)-(DATE()-FSEC)) PICT '99999' FUNC 'J' COLO sche 13
@ 6,53 say [Prod 305......] COLO sche 17
@ 6,69 SAY p305 COLO sche 13
@ 7,53 say [Prod 305 EM...] COLO sche 17
@ 7,69 SAY m305 COLO sche 13

@ 8,53 SAY [Dias en Leche....]
@ 8,70 say DPR COLO sche 13
@ 9,53 say [Dias Insem........]
@ 9,70 say dia COLO sche 13
@ 10,53 SAY [Dias a 1er Serv...]
@ 10,71 say d1s COLO sche 13
@ 11,53 SAY [Dias Abiertos...]
@ 11,70 say dab COLO sche 13
@ 12,53 SAY [Dias en Secas.....]
@ 12,70 say dsc COLO sche 13
@ 13,53 SAY [Corral Num........]
@ 13,72 SAY CORR PICT '99' FUNC 'J' COLO W+/&CZ/n
@ 14,69 SAY [Enter] COLO sche 17
@ 14,75 SAY CHR(16) COLO sche 17
DO WHILE INKEY()#13
ENDD
ACTI WIND PROG
if Fpro=B .and. Fpro2=B and FT1=B and FT3=B
	@ 6,6 SAY [No Tiene Datos] COLO SCHE 13
else
		@ 1,2 say [MANEJO] colo SCHE 13
		@ 2,2 say [Fecha 1]
		@ 3,2 say [Fecha 2]
		@ 4,2 say [Medic. ]
		@ 5,2 say [Revisar]
		@ 6,2 say [Motivo ]

		@ 2,14 say FT1 colo sche 13
		@ 3,14 say FT2 colo sche 13
		@ 4,10 say proper(MD1) func'j' colo sche 13
		@ 5,14 say FPRO colo sche 13
		@ 6,10 say proper(MEDP) func 'j' colo sche 13

 		@ 7,2 say [HOSPITAL] colo SCHE 13
		@ 8,2 say [Fecha 1] 
		@ 9,2 say [Fecha 2] 
		@10,2 say [Medic. ] 
		@11,2 say [Revisar] 
		@12,2 say [Motivo ] 

		@ 8,14 say FT3 colo sche 13
		@ 9,14 say FT4 colo sche 13
		@10,10 say proper(MD2) func 'j' colo sche 13
		@11,14 say FPRO2 colo sche 13
		@12,10 say proper(MEDH) func 'j' colo sche 13
endi
@ 14,17 SAY [Enter] 
@ 14,23 SAY CHR(16) 
DO WHILE INKEY() # 13
ENDD
DEACT WIND ACTVAC
RELE WIND PROG
ENDI
ON KEY LABEL F10 DO TARJETA


PROC Marcar
push key clear
sn="N"
@ 22,30 say [MARCAR/DESMARCAR BST] colo gr+/&cz
@24,1  say [F10] colo n/w
@24,5  say [Lista]
@24,70 SAY [Esc] colo n/w
@24,74 say [Salir] colo w+/b

use reg order 2
ON KEY LABEL F10 DO Lista

xid=0
mCAU4=space(8)
do while .T.
	axid=XID
	sn='S'
	xid=0
*	@22,1 clea to 22,27
	
	@22,70 say [IDa] colo N/W
	@22,74 say axid PICT '#####' func 'j' colo w+/&cz
	@22,1 SAY [ID] get xid pict '#####' func'j,t' colo w+/&cz,n/w
	read timeout 60
	
	if lastkey()=27 or xid=0
		exit
	endi
	
	SEEK XID
	if .not. found()
		?CHR(7)
		WAIT WIND tit(1) TIMEOUT 1
	else
		if np=0 or np>0 and fsec#B 
			?chr(7)
			WAIT WIND tit(2) TIMEOUT 1
		else
		mcorr=CORR
		xchoice=1
		xchoice2=2
		
	defi wind win12 from 7,20 to 16,60 title [ Tipo de Selecci¢n : ]  shadow colo sche 16
	on key label F10 *
	acti wind win12
	@ 1,2 get xchoice func'*R SI BST;NO BST;Desmarcar' colo ,,,,,gr+/b,,,w+/w
	@ 5,6 say [Causa ] get mCAU4 func'!'  valid not empty(mCAU4) error tit(2) when  xchoice<3
	@ 7,18 get xchoice2 func'*H Aceptar;Cancelar' DEFAULT 1 colo ,,,,,gr+/b,,,n+/w
	read cycle
	rele wind win12
	ON KEY LABEL F10 DO Lista

	if xchoice2=2
		loop
	endif	

	do case
		case xchoice=1
			repl STB with "S",CAU4 with mCAU4
		case xchoice=2
			repl STB with "N",CAU4 with mCAU4
		case xchoice=3
			repl STB with " ",CAU4 with space(8)
	endcase	

	endif
endif
endd
clos data
pop key
return


PROC LISTA
push key clear
save screen to xx
@ 4,0 clea to 24,79
@21,0 to 21,79
@22,30 say [VACAS MARCADAS BST]  colo gr+/&cz
@23,0 to 23,79

@24,2 SAY [Total] colo w+/&cz
@ 4,1 say [                                                                              ] colo n/w
@ 5,1 say [      ID   Causa BST  U Prod  Corral        ID   Causa BST  U Prod  Corral    ] colo n/w
row=7
col=4
dat=0
set filt to STB="N" and fb2=B or STB="S" and fb2=B
go top
scan while inkey()#6
@24,8 say dat+1 pict '####'
@24,70 SAY [Espere] colo n*/w
@row,col say ID pict '#####' colo gr+/&cz

	if PSV="S"
		@ ROW,COL+5 SAY "+" colo w+/&cz
	endi
@ row,col+8 say CAU4
@ row,col+16 say STB colo g+/&cz
@ row,col+21 SAY PRM PICT '99.9' colo r+/&cz
@ row,col+29 SAY CORR PICT '99'

IF ROW=20 and COL=42
@ 23,70 SAY [Enter] COLO N/W
@ 23,75 say [ ] colo w/&cz
@ 23,76 SAY CHR(16) colo w+/&cz
DO WHILE INKEY() # 13
ENDD
@ 6,2 CLEA TO 20,77
@ 24,58 CLEA TO 24,78
ROW=6
COL=4
ENDI

IF ROW=20
ROW=6
COL=COL+38
ENDI

DAT=DAT+1
ROW=ROW+1
ENDS

IF DAT=0
@ 24,9 SAY [0]
??CHR(7)
@ 24,70 SAY [Enter] COLO N/W
@ 24,75 say [ ] colo w/&cz
@ 24,76 SAY CHR(4) colo w+/&cz
DO WHILE INKEY() # 13
ENDD

ELSE
??CHR(7)
@ 24,70 SAY [Enter] COLO N/W
@ 24,75 say [ ] colo w/&cz
@ 24,76 SAY CHR(4) colo w+/&cz
DO WHILE INKEY() # 13
ENDD

SN='N'
@ 24,67 say [Imprimir] get SN pict '@M S,N' colo n/w
READ
IF SN='N' or LASTKEY()=27
ELSE
IF SN='S' and SYS(13)='READY'
SET CONS OFF
SET PRIN ON
NVE=0
PAG=1
NVAC=0
GO TOP
NTIT=[VACAS MARCADAS BST]
DO WHILE .NOT. EOF()
DO CTRLVRP0
?'     ID    FECHA                          ESTADO   PROD    ULTIMA   ULT.   CORRAL'
?'    VACA   PARTO      DEL  BST  CAUSA     REPROD   PROM    PESADA   PROD   NUMERO'
?'  '+REPLI('-',94)
set filt to STB="N" and fb2=B or STB="S" and fb2=B
SCAN while inkey()#6
CR=[ ]
IF PSV="S"
CR=[+]
ENDI
?space(2),str(id,5,0)+CR+' '+fx(FPAR)+'   '+STR(DPR,4)+'    '+STB+'  '+CAU4;
+''+STAT+'    '+STR(PRX,4,1)+'   '+fx(PESA)+'  '+STR(PRM,4,1)+'     '+STR(CORR,2)

NVE=NVE+1
NVAC=NVAC+1
IF NVE=WLI
?REPLI(CHR(254),94)
EJEC
PAG=PAG+1
NVE=0
SKIP
EXIT
ENDI
ENDS
ENDD
?'  '+REPLI(CHR(254),94)
?
?'   TOTAL = '+STR(NVAC,4)
EJEC
SET PRIN OFF
SET CONS ON
SET FILT TO
ELSE
??CHR(7)
WAIT WIND [ IMPRESORA APAGADA O DESCONECTADA ] TIMEOUT 2
ENDI
ENDI
ENDI
set filt to
pop key
restore screen from xx
return

***************
PROCEDURE ORDEN
***************
public xorden,xchoice2
norden=1
xorden="2"
xchoice2=2
defi wind orden from 7,21 to 17,59 title [ Ordenado por : ] shadow colo sche 16
acti wind orden
@ 1,2 get norden func'*R Identificaci¢n;Corral/Lote;Ultima Producci¢n;Dias en Leche;Dias al Ultimo Calor/Celo' DEFAULT 1 colo ,,,,,gr+/b,,,w+/w
@ 8,16 get xchoice2 func'*H Aceptar;Cancelar' DEFAULT 1 colo ,,,,,gr+/b,,,n+/w
read cycle valid lastkey()=13 or lastkey()=27
		do case
			case norden=1
				 xorden="2"
			case norden=2
				 xorden="4"
			case norden=3
				 xorden="6"
			case norden=4
				 xorden="7"
			case norden=5
				 xorden="9"
		endcase
rele wind orden
return

***************
PROCEDURE ORDEN2
***************
public xorden
norden='1'
xorden="2"
defi wind orden from 8,21 to 17,59 title [ Ordenadas Por : ] shadow colo sche 16
acti wind orden
@1,2 say [ Numero de Identificacion .... 1] 
@2,2 say [ Numero de Corral o Lote ..... 2] 
@3,2 say [ Ultima Prod. Leche (Kgs) .... 3] 
@4,2 say [ Dias en Leche ............... 4] 
@5,2 say [ Dias al Ultimo Calor......... 5] 

@1,33 say [1] colo sche 18
@2,33 say [2] colo sche 18 
@3,33 say [3] colo sche 18
@4,33 say [4] colo sche 18
@5,33 say [5] colo sche 18

@7,2 say [ Seleccion ...................] get norden pict'@M 1,2,3,4,5'
read valid lastkey()=13
	if lastkey()=27
		xorden=2
	else
		do case
			case norden="1"
				 xorden="2"
			case norden="2"
				 xorden="4"
			case norden="3"
				 xorden="6"
			case norden="4"
				 xorden="7"
			case norden="5"
				 xorden="9"

		endcase
	endi
rele wind orden
return

FUNCTION FX
	parameters xfecha
	retu right(dtoc(xfecha),8)
return


