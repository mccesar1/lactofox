FUNCTION FX
	parameters xfecha
	retu right(dtoc(xfecha),8)
return

*   ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
*   ³  VACAS CON +12 MESES DE FECHA DE BAJA ³
*   ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC RV10152
@ 4,1 clea to 20,78
@ 21,0 to 21,79
@ 23,0 to 23,79
@ 22,22 say [VACAS CON +12 MESES DE FECHA DE BAJA] colo gr+/&cz
@ 24,70 say [Esc] COLO N/W
@ 24,74 say [Salir] colo w+/&cz

@ 24,1 SAY [Total] colo w+/&cz

@ 4,1 say [                                                                              ] colo n/w
@ 5,1 say [      ID  Fecha    Causa       ID  Fecha    Causa       ID  Fecha    Causa    ] colo n/w
ROW=7
COL=4
DAT=0
USE REG order 2
set filt to fb2#B and fb2<(date()-365)
go top
SCAN while inkey()#6
@ 24,7 SAY DAT+1 PICT '9999'
@ ROW,COL SAY ID colo gr+/&cz
@ ROW,COL+7 SAY fx(FB2)
@ ROW,COL+18 SAY CAU2
IF ROW=20 .AND. COL=54  
@ 24,68 clea to 24,79
@ 24,70 SAY [Enter] COLO N/W
@ 24,75 SAY [ ] COLO W+/&CZ
@ 24,76 SAY CHR(16) colo w+/&cz
DO WHILE INKEY() # 13
IF LASTKEY()=6
GO BOTTOM
EXIT
ENDI
ENDD
@ 6,2 clea to 20,77
@ 24,58 CLEA to 24,78
ROW=6
COL=4
ENDI
IF ROW=20
ROW=6
COL=COL+25
ENDI
DAT=DAT+1
ROW=ROW+1
ENDS
IF DAT=0
@ 24,8 SAY [0]
ELSE
ENDI
??CHR(7)
@ 24,68 clea to 24,79
@ 24,70 SAY [Enter] COLO N/W
@ 24,75 SAY [ ] COLO W+/&CZ
@ 24,76 SAY CHR(4) colo w+/&cz
DO WHILE INKEY() # 13
ENDD
IF DAT=0
ELSE
SN='N'
@ 24,69 SAY [Imprimir] GET SN PICT '@M S,N' colo N/W
READ
IF SN='N' .OR. LASTKEY()=27
ELSE
IF SN='S' .AND. SYS(13)='READY'
SET CONS OFF
SET PRIN ON
NVE=0
PAG=1
NVAC=0
NTIT=[VACAS CON +12 MESES DE FECHA DE BAJA]
GO TOP
DO WHILE .NOT. EOF()
DO CTRLVRP0
?
?'     ID    FECHA             FECHA    CAUSA            ESTADO   PROD    ULTIMA   ULT.   CORRAL'
?'    VACA   PARTO      DEL    BAJA      BAJA   DESTINO  REPROD    DIA    PESADA   PROD   NUMERO'
?'  '+REPLI('-',94)
SCAN while inkey()#6

?space(2),str(id,5,0)+'  '+fx(FPAR)+'   '+STR(DPR,4)+'   '+fx(FB2)+'    '+STR(CAU2,2);
+'     '+MOT2+'   '+STAT+'   '+STR(PRX,4,1)+'   '+fx(PESA)+'  '+STR(PRM,4,1)+'    '+STR(CORR,2)

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
*----------------------------------
SET CONS ON
SET FILT TO
ELSE
??CHR(7)
WAIT WIND [ IMPRESORA APAGADA O DESCONECTADA ] TIMEOUT 2
ENDI
ENDI
do dbaja
ENDI


*   ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
*   ³  REPORTE GENERAL PARA REVISION  ³
*   ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC RV10159
SN='P'
@ 4,1 clea to 20,78
@ 21,0 to 21,79
@ 23,0 to 23,79
@ 22,26 SAY [REPORTE GENERAL PARA REVISION] colo gr+/&cz
@ 24,70 say [Esc] COLO N/W
@ 24,74 say [Salir] colo w+/&cz

@ 10,4 say [NOTA :] COLO W+/&CZ
@ 12,4 FILL TO 16,77 COLO N/N
@ 11,3 fill to 15,76 COLO SCHE 15
@ 12,4 say [ Este opcion emitira una lista de todas las vacas activas, con los datos] COLO SCHE 15
@ 13,4 say [ mas importantes de la lactancia actual, el cual podra ser utilizado en ] COLO SCHE 15
@ 14,4 say [ campo como hoja de trabajo al efectuar revisiones o tratamientos.      ] COLO SCHE 15

@ 24,30 SAY [* Solo en Impresora *] colo r+/&cz
@ 24,68 say [Continuar] get sn pict '@M N,S' COLO n/W

READ
IF SN='N'.or. lastkey()=27
	clos all
	return
ELSE	
*@ 24,66 clea to 24,78
*@ 24,70 SAY [Esc] colo n/w
*@ 24,74 SAY [Salir] colo w+/&cz

if SYS(13)='READY'
SET PRIN ON
SET CONS OFF

SET ESCA ON
ON ESCA DO CTRLSTOP
*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*  Cambio a Letra 17 cpi
	???&q21
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
*@ 24,60 clea to 24,78
*@ 24,70 say [Espere] colo n*/w
use REG order 2
NV=0
PAG=1
NVAC=0
GO TOP
NTIT=[REPORTE GENERAL PARA REVISION]
ximprime=.T.
DO WHILE .NOT. EOF()
	if ximprime=.F.
	exit
	endi
?'  Rancho   : '+Q36+'   '+nom 
?'  Area     : VIENTRES'
?'  Reporte  : '+NTIT
?'  Fecha    : '+DTOC(DATE())+'  Hora : '+TIME()
?'  Pagina   : '+STR(PAG,2,0)+space(108)+[LACTOFOX]
?'  '+REPLI(CHR(254),132)
?'   ID   C                EDO        ULTIMA    ULTIMO   ULT SER  ULT  OBSERVACIONES   FECHA A   MOTIVO'
?'  VACA  #  CODIGO   DEL  REP   DUC  REV/Tx    CALOR    MED  #  PROD  ULT   EVENTO    REVISAR   REVISION            OBSERVACIONES'
?'  '+REPLI('-',132)

SCAN for np>0 and fb2=B WHILE INKEY() # 6
xst=[ ]
cra=[ ]
	if ximprime=.F.
	exit
	endi


EDADN=((DATE()-FNAC)-(DATE()-FB2))/30.42
ECP=STR(INT(EDADN/12),2)+'-'+STR(MOD(EDADN,12),2)
*-------------------------------*
if stat=[INSEM] .or. stat=[CARGA]
	repl dia with date()-ucal,dab with ucal-fpar
else
	repl dia with 0,dab with (date()-fpar)-(date()-fb2)
endi	

repl dpr with (date()-fpar)-(date()-fsec)

if FSEC#B
	xst='S'
endi

if PSV="S"
cra=[+]
endi
*-------------------------------*
?' '+str(ID,5)+cra+''+str(CORR,2)+'  '+CODIGO+' '+str(dpr,3)+'  '+stat+' ';
+str(dia,3)+'  '+fx(UTRA)+'  '+fx(UCAL)+' ';
+str(MED1,3)+' '+str(NS,2)+'  '+str(PRM,4,1)+'  '+OBSC+'  ';
+fx(fpro)+'  '+medp+'  __________________________'

NV=NV+1
NVAC=NVAC+1
IF NV=WLI
?'  '+REPLI(CHR(254),132)
EJEC
PAG=PAG+1
NV=0
SKIP
EXIT
ENDI
ENDS
ENDD
?'  '+REPLI(CHR(254),132)
?
?'   TOTAL DE VACAS = '+STR(NVAC,4)
EJEC
*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*  Cambio a Letra 12 cpi
	???&q19
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
SET PRIN OFF
SET CONS ON
ELSE
??CHR(7)
WAIT WIND [ IMPRESORA APAGADA O DESCONECTADA ] TIMEOUT 2
endi
ENDI
*----------------*
ON ESCAPE
SET ESCA OFF
ximprime=.t.
RETU

*   ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
*   ³  ANALISIS DE PRODUCCION POR CORRAL  ³
*   ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC RV10160
DO WHILE .T.
SN='N'
MC=0
@ 4,1 clea to 24,78
@ 21,0 to 21,79
@ 23,0 to 23,79
@ 22,24 SAY [ANALISIS DE PRODUCCION POR CORRAL] colo gr+/&cz
@ 24,70 say [Esc] COLO N/W
@ 24,74 say [Salir] colo w+/&cz

@ 22,1 say [Corral ] GET MC PICT '99' colo w+/b,n/w
READ
IF MC=0 .OR. LASTKEY() = 27
EXIT
ENDI
@ 24,68 CLEA to 24,78
IF MC>0
declare c1(12)
c1=0
USE REG order 2
set filt to np>0 and fb2=b and corr=mc 


@ 24,1 say [Total] colo w+/&cz
@ 4,1 say [     ID  Lact  Estado  Prod   Prod         Ultima           Prod  Cond  Valor ] colo n/w
@ 5,1 say [    Num   Num  Reprod   Ant    Act   DIF   Pesada     DEL   305d  Corp  Rel % ] colo n/w
ROW=7
COL=3
DAT=0
GO TOP
scan
@ 24,7 SAY DAT+1 PICT '9999'
@ 24,70 SAY [Espere] colo n*/w

*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*
* CALCULO DE DIAS INSEMINADA Y DIAS ABIERTOS 
*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*
if stat=[INSEM] or stat=[CARGA]
	repl dia with date()-ucal,dab with ucal-fpar
else
	repl dia with 0,dab with (date()-fpar)-(date()-fb2)
endi	
*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*


** Calculo de Promedios
** --------------------
	c1(1)=c1(1)+1
	c1(2)=c1(2)+(date()-fpar)-(date()-fsec)

if prm>0
	c1(3)=c1(3)+1
	c1(4)=c1(4)+prm
endi

if condc>0
	c1(5)=c1(5)+1
	c1(6)=c1(6)+condc
endi

if tota>0
	c1(7)=c1(7)+1
	c1(8)=c1(8)+(prm-tota)
	c1(9)=c1(9)+tota
endi

if p305>0	
	c1(10)=c1(10)+p305
	c1(11)=c1(11)+(m305/x305)*100
	c1(12)=c1(12)+1
endi	
** ---------------------

@ row,col say ID colo gr+/&cz
	IF PSV="S"
		@ ROW,COL+5 SAY &mensa2
	ENDI
@ row,col+8 say NP 
@ row,col+13 say stat
@ row,col+20 say tota colo bg/B
@ row,col+27 say prm colo r+/B
if tota>0
	@ row,col+33 say (prm-tota) pict '###.#' colo g+/B
else
	@ row,col+35 say [0.0] colo g+/B
endi
@ row,col+41 say fx(pesa)
@ row,col+51 say (date()-fpar)-(date()-fsec) pict '9999'
@ row,col+57 say p305 
@ row,col+64 say condc
@ row,col+71 say (m305/x305)*100 pict '999' colo w+/B

if row=20
		@ 24,70 say [Enter] colo N/W
		@ 24,75 say [ ] colo W+/&CZ
		@ 24,76 say CHR(16) colo w+/&cz
	do while inkey()#13
	endd
		@ 6,2 clea to 20,77
		@ 24,58 clea to 24,78
		row=6
		col=3
endi
	dat=dat+1
	row=row+1
ENDS
??chr(7)
	@ 24,70 SAY [Enter] COLO N/W
	@ 24,75 SAY [ ] COLO W+/&CZ
	@ 24,76 SAY CHR(4) colo w+/&cz
DO WHILE INKEY() # 13
ENDD
@ 24,69 say [Espere] colo n*/w

mdias=c1(2)/c1(1)
mprm=c1(4)/c1(3)
mtcon=c1(6)/c1(5)
mtota=c1(9)/c1(7)
mdif=c1(8)/c1(7)
mp305=c1(10)/c1(12)
mvalr=c1(11)/c1(12)

@ 20,1 say [   Promedios                                                                  ] colo n/w
@ 20,col+21 say str(mtota,4,1) colo n/w
@ 20,col+28 say str(mprm,4,1) colo n/w
@ 20,col+33 say str(mdif,5,1) colo n/w
@ 20,col+51 say str(mdias,4) colo n/w
@ 20,col+57 say str(mp305,5) colo n/w
@ 20,col+64 say str(mtcon,4,1) colo n/w
@ 20,col+71 say str(mvalr,3) colo n/w

@ 24,69 to 24,78 clea

@ 24,69 SAY [Imprimir] GET SN PICT '@M S,N' colo n/w 
read
rele wind CORRAL
if sn='N' or lastkey()=27
else
if sn='S' and SYS(13)='READY'
	set cons off
	set prin on
	nve=0
	pag=1
	nvac=0
	ntit=[ANALISIS DE PRODUCCION]
	ntit2=[CORRAL NUM.]+STR(MC,2)
@ 22,67 clea to 22,78
@ 22,67 SAY [Espere] colo n*/w
go top
do while not eof()
do ctrlvrpt

?'     ID  LACT   ESTADO   PROD    PROD          ULTIMA              PROD   COND   VALOR'
?'   VACA   NUM   REPROD    ANT     ACT    DIF   PESADA      DEL     305D   CORP   REL %'

?'  '+REPLI('-',94)

scan while inkey()#6

** Para poner el signo (+) a candidato a rastro.
** --------------------------------------------
cra=[ ]
if PSV="S"
cra=[+]
endi
*---------------------------------------------*

?'  '+str(ID,5)+cra+'  '+str(NP,2)+'    '+stat+'   '+str(tota,5,1)+'   '+str(prm,5,1)+'  '+str(prm-tota,5,1)+'   '+;
fx(pesa)+'   '+str((date()-fpar)-(date()-fsec),4)+'   '+str(p305,6)+'   '+str(condc,4,1)+'   '+;
str((m305/x305)*100,4)

	nve=nve+1
	nvac=nvac+1
if nve=WLI
	?
	?'  '+REPLI(CHR(254),94)
	ejec
	pag=pag+1
	nve=0
skip
exit
endi
ENDS
ENDD
?'  '+REPLI(CHR(254),94)
?'   PROMEDIOS :          '+str(mtota,5,1)+'    '+str(mprm,4,1)+'   '+str(mdif,4,1);
+'              '+str(mdias,4)+'   '+str(mp305,6)+'   '+str(mtcon,4,1)+'   '+str(mvalr,3)
?
?'   TOTAL DE VACAS : '+str(c1(1),4)
EJEC
SET PRIN OFF
SET FILT TO
SET CONS ON
ELSE
??CHR(7)
WAIT WIND [ IMPRESORA APAGADA O DESCONECTADA ] TIMEOUT 2
ENDI
ENDI
ENDI
ENDD
clos data
rele all like c1*
retu

*   ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
*   ³  REPORTE PARA INSEMINACION  ³
*   ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC RV10158
@ 4,1 clea to 24,78
@ 21,0 to 21,79
@ 23,0 to 23,79
@ 22,23 SAY [REPORTE PARA REVISION/INSEMINACION] colo gr+/&cz
@ 24,70 say [Esc] COLO N/W
@ 24,74 say [Salir] colo w+/&cz
@ 24,29 say [* Solo en Impresora *]colo r+/&cz	
*
@ 4,1 say [                                                                              ] colo n/w
@ 5,1 SAY [     ID Corral      ID Corral      ID Corral      ID Corral      ID Corral    ] colo n/w

use CBAJA order 1 in 3 alias CB
use MEDIC order 1 in 2
use REG order 2 alias REG in 1

	xid=0
	row=7
	col=3
	xco=0
	xdias=1
	nvac=0

	@ 24,1 say [Total] colo w+/&cz
	sele 1
	do while .t.
		@ 22,1 say [ID ] get xid valid seek(xid) pict [#####] func 'j,t' error [ID no existe ] colo w+/&cz,n/w
		read
		if lastkey()=27
			exit
		endi

		if FB2#B
    	   ??chr(7)
    	   wait wind [ Dada de Baja ] timeout 1
		   loop
	    else
		if ximp=.t.
    	   ??chr(7)
    	   wait wind [ ID Repetida ] timeout 1
		   loop
		else
		@row,col say xid pict "#####" colo gr+/&cz
		@row,col+8 say corr pict"##"
		repl ximp with .t.
		
		row=row+1
		nvac=nvac+1
		@ 24,7 say nvac pict'###' 
			do case
				case row=20 and col#63
				row=7
				col=col+15
				case row=20 and col=63
				@7,1 clea to 20,78
				row=7
				col=3
			endcase
		endi
	 endif
	enddo	
*		
**  Salida para Impresora
** ----------------------
sn='N'
@ 24,67 clea to 24,78
@ 24,67 SAY [Imprimir] GET SN PICT '@M S,N' colo n/w
read
if sn='N' or lastkey()=27
else
xchoice=1
xchoice2=2
xfilt=""
xtit=""
	defi wind win12 from 8,21 to 16,59 title [ Ordenado por : ]  shadow colo sche 16
	acti wind win12
	@ 1,2 get xchoice func'*R Identificaci¢n;Corral/Lote' colo ,,,,,gr+/b,,,w+/w
	@ 6,16 get xchoice2 func'*H Aceptar;Cancelar' DEFAULT 1 colo ,,,,,gr+/b,,,n+/w

	read cycle
	rele wind win12
	if xchoice2=2
      sele 1
	  repl all ximp with .f.
	  clos all
	  return
	endif
	
	do case
     case xchoice=1
	  select * from REG where np>0 and fb2=B and ximp=.t. order by ID into table DAT
	 case xchoice=2
	  select * from REG where np>0 and fb2=B and ximp=.t. order by CORR,ID into table DAT
	endcase	

	set rela to CAU2 into CB,MED1 into MEDIC

		if sys(13)='READY'

			set console off
		    set printer on
			*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
			*  Cambio a Letra 17 cpi
				???&q21
			*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼

	xpag=1			
	ximprime=.T.

	go top

			nvac=0
			ntit=[REPORTE PARA INSEMINACION]

do titulo3
_plineno=0

do while not eof()

		DO WHILE not eof()
		
			if _plineno=Q45
				do titulo3
				_plineno=0
			endi	

			xid=DAT.id
			xnp=DAT.np
			xduc=date()-ucal
			xdel=(date()-fpar)-(date()-fsec)
			if stat=[INSEM]
			xdab=ucal-fpar
			else
			xdab=date()-fpar
			endi
			mnum=0
			mnom=space(12)
			cra=[ ]
			
			if PSV="S"								
				cra="+"
			endi 	
			
if xdel>=xdias
*-----------------------------------------------------------------------

?'  '+str(ID,5)+cra+' '+str(CORR,2)+'  '+str(xdel,4)+'  '+str(np,2);
+'  '+str(PRM,4,1)+'  '+STAT+'  '+MEDIC.mnom+'  '+fx(UTRA)+'  '+fx(UCAL);
+'  '+STR(NS,2)+'  '+str(xduc,5)+'   '+CB.cnom+space(10)+idt1+' '+idt2
*-----------------------------------------------------------------------

?SPACE(1),REPLI('-',89)+'  -----  '+REPLI('-',33)

		skip
		xid=DAT.id
		nvac=nvac+1
endi
endd				
		ENDD
		?'  '+repli(CHR(254),132)
		?
		?'   TOTAL = '+STR(nvac,4)
		EJEC
*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*  Cambio a Letra 12 cpi
	???&q19
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
set prin off
set cons on
SET FILT TO
ELSE
??CHR(7)
WAIT WIND [ IMPRESORA APAGADA O DESCONECTADA ] TIMEOUT 2
endi
endi
rele wind win12
sele 1
repl all ximp with .f.
clos all
eras DAT.dbf
on page
return

*   ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
*   ³  ANALISIS DE INTERVALOS ENTRE SERVICIOS  ³
*   ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC RV10164
set decimals to 1
STORE ctod("") TO FIN1,FIN2
@ 4,1 clea to 24,78
@ 21,0 to 21,79
@ 23,0 to 23,79
@ 22,21 SAY [ANALISIS DE INTERVALO ENTRE SERVICIOS] colo gr+/&cz
@ 24,70 say [Esc] COLO N/W
@ 24,74 say [Salir] colo w+/&cz

@ 24,26 SAY [Del] get FIN1 valid FIN1<>B .AND. FIN1<=date() error tit(3) 
@ 24,42 SAY [Al] get FIN2 valid FIN2<>B .AND. FIN2>=FIN1 .and. FIN2<=date() error tit(3) 
READ

IF LASTKEY()=27
RETU
ENDI

use CALOR order 2
set filt to np>0 and ciclo>0 and fecha>=FIN1 and fecha<=FIN2
dimension aw(60)
aw=0
@ 20,1 say [ID] colo w+/b
@ 24,1 say [Total] colo w+/&cz
@ 24,69 clea to 24,78
@ 24,70 say [Espere] colo n*/w

dat=0
nciclo=0
scan while inkey()#6
@ 20,4 say id colo n/w
@ 24,7 say dat pict '####'

*if ciclo>0 and fecha>=FIN1 and fecha<=FIN2
	dat=dat+1
	nciclo=nciclo+ciclo
do case
	** Lactancia 1
	** -----------
	case np=1
	aw(1)=aw(1)+1
		do case
			case ciclo<10 
				aw(4)=aw(4)+1
			case ciclo>=10 and ciclo<=17
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
			case ciclo<10 
				aw(10)=aw(10)+1
			case ciclo>=10 and ciclo<=17
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
			case ciclo<10 
				aw(16)=aw(16)+1
			case ciclo>=10 and ciclo<=17
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
			case ciclo<10 
				aw(23)=aw(23)+1
			case ciclo>=10 and ciclo<=17
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
endi

defi wind ciclo from 4,0 to 20,77 shadow colo sche 16
acti wind ciclo
@ 1,28 say [Porcentaje por Lactancia]
@ 3,1 say [Dias/Servicios     <10     10-17     18-24     25-35     36-48      49+] colo n/w
@ 4,0 to 4,77

@ 6,1 say [Lactancia 1] colo n/w
@ 6,19 say aw(30) pict '##.#'
@ 6,29 say aw(31) pict '##.#'
@ 6,39 say aw(32) pict '##.#'
@ 6,49 say aw(33) pict '##.#'
@ 6,59 say aw(34) pict '##.#'
@ 6,68 say aw(35) pict '##.#'

@ 8,1 say [Lactancia 2] colo n/w
@ 8,19 say aw(36) pict '##.#'
@ 8,29 say aw(37) pict '##.#'
@ 8,39 say aw(38) pict '##.#'
@ 8,49 say aw(39) pict '##.#'
@ 8,59 say aw(40) pict '##.#'
@ 8,68 say aw(41) pict '##.#'

@10,1 say [Lactancia 3+] colo n/w
@10,19 say aw(42) pict '##.#'
@10,29 say aw(43) pict '##.#'
@10,39 say aw(44) pict '##.#'
@10,49 say aw(45) pict '##.#'
@10,59 say aw(46) pict '##.#'
@10,68 say aw(47) pict '##.#'

@13,1 say [Todas] colo n/w
@13,19 say aw(48) pict '##.#'
@13,29 say aw(49) pict '##.#'
@13,39 say aw(50) pict '##.#'
@13,49 say aw(51) pict '##.#'
@13,59 say aw(52) pict '##.#'
@13,68 say aw(53) pict '##.#'

ACTI SCREE
??chr(7)
@ 24,69 clea to 24,77
@ 24,70 SAY [Enter] COLO N/W
@ 24,76 SAY chr(16) colo w+/&cz
DO WHILE INKEY() # 13
ENDD
acti wind ciclo

@ 0,0 clea to 15,76
@ 1,29 say [N£mero de Intervalos]
@ 3,1 say [Dias/Servicios     <10     10-17     18-24     25-35     36-48      49+] colo n/w
@ 4,0 to 4,77

@ 6,1 say [Lactancia 1] colo n/w
@ 6,19 say aw(4) pict '###'
@ 6,29 say aw(5) pict '###'
@ 6,39 say aw(6) pict '###'
@ 6,49 say aw(7) pict '###'
@ 6,59 say aw(8) pict '###'
@ 6,68 say aw(9) pict '###'

@ 8,1 say [Lactancia 2] colo n/w
@ 8,19 say aw(10) pict '###'
@ 8,29 say aw(11) pict '###'
@ 8,39 say aw(12) pict '###'
@ 8,49 say aw(13) pict '###'
@ 8,59 say aw(14) pict '###'
@ 8,68 say aw(15) pict '###'

@10,1 say [Lactancia 3+] colo n/w
@10,19 say aw(16) pict '###'
@10,29 say aw(17) pict '###'
@10,39 say aw(18) pict '###'
@10,49 say aw(19) pict '###'
@10,59 say aw(20) pict '###'
@10,68 say aw(21) pict '###'

@13,1 say [Total] colo n/w
@13,18 say aw(23) pict '####'
@13,28 say aw(24) pict '####'
@13,38 say aw(25) pict '####'
@13,48 say aw(26) pict '####'
@13,58 say aw(27) pict '####'
@13,67 say aw(28) pict '####'

*@ 15,1 say [% Estimado de Deteccion de Calores : ] colo n/w
*@ 15,38 say aw(31) pict '###.#'

ACTI SCREE
??chr(7)
@ 24,69 clea to 24,79
@ 24,70 SAY [Enter] COLO N/W
@ 24,76 SAY chr(4) colo w+/&cz
DO WHILE INKEY() # 13
ENDD

SN='N'
??chr(7)
@ 24,69 SAY [Imprimir] GET SN PICT '@M S,N' colo n/w 
READ
IF SN='N' .OR. LASTKEY()=27
ELSE
IF SN='S' .AND. SYS(13)='READY'
PAG=1
NTIT=[ANALISIS DE INTERVALOS ENTRE SERVICIOS]
SET CONS OFF
SET PRIN ON
?'  Rancho   : '+Q36+'   '+nom 
?'  Area     : VIENTRES'
?'  Reporte  : '+NTIT
?'  Fecha    : '+DTOC(DATE())+'  Hora : '+TIME()
?'  Pagina   : '+STR(PAG,2,0)+space(70)+[LACTOFOX]
?'  Periodo  : Del '+dtoc(FIN1)+'  Al '+dtoc(FIN2) 
?'  '+REPLI(CHR(254),94)
?
?'  Dias entre Servicios      <10       10-17       18-24       25-35       36-48         +49'    
?'  '+REPLI('-',94)
?
?'  Lactancia 1  (%)'+space(9)+str(aw(30),4,1)+'        '+str(aw(31),4,1)+'        '+str(aw(32),4,1)+'        '+str(aw(33),4,1)+'        '+str(aw(34),4,1)+'        '+str(aw(35),4,1)      
?
?'  Lactancia 2  (%)'+space(9)+str(aw(36),4,1)+'        '+str(aw(37),4,1)+'        '+str(aw(38),4,1)+'        '+str(aw(39),4,1)+'        '+str(aw(40),4,1)+'        '+str(aw(41),4,1)      
?
?'  Lactancia 3  (%)'+space(9)+str(aw(42),4,1)+'        '+str(aw(43),4,1)+'        '+str(aw(44),4,1)+'        '+str(aw(45),4,1)+'        '+str(aw(46),4,1)+'        '+str(aw(47),4,1)      
?
?
?'  Todas        (%)'+space(9)+str(aw(48),4,1)+'        '+str(aw(49),4,1)+'        '+str(aw(50),4,1)+'        '+str(aw(51),4,1)+'        '+str(aw(52),4,1)+'        '+str(aw(53),4,1)
?
?'  '+REPLI('-',94)
?
?'  Lactancia 1  (#)'+space(9)+str(aw(4),4)+'        '+str(aw(5),4)+'        '+str(aw(6),4)+'        '+str(aw(7),4)+'        '+str(aw(8),4)+'        '+str(aw(9),4)      
?
?'  Lactancia 2  (#)'+space(9)+str(aw(10),4)+'        '+str(aw(11),4)+'        '+str(aw(12),4)+'        '+str(aw(13),4)+'        '+str(aw(14),4)+'        '+str(aw(15),4)      
?
?'  Lactancia 3  (#)'+space(9)+str(aw(16),4)+'        '+str(aw(17),4)+'        '+str(aw(18),4)+'        '+str(aw(19),4)+'        '+str(aw(20),4)+'        '+str(aw(21),4)      
?
?
?'  Total      '+str(aw(22),5)+space(9)+str(aw(23),4)+'        '+str(aw(24),4)+'        '+str(aw(25),4)+'        '+str(aw(26),4)+'        '+str(aw(27),4)+'        '+str(aw(28),4)
?
?'  '+REPLI(CHR(254),94)
?
EJEC
SET PRIN OFF
SET CONS ON
ELSE
??CHR(7)
WAIT WIND [ IMPRESORA APAGADA O DESCONECTADA ] TIMEOUT 2
ENDI
ENDI
rele wind ciclo
clos data
rele all like AW*
set decimals to
retu


*  ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
*  ³  PROGRAMA PARA ENVIAR DATOS DE CONSULTAS  ³
*  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC RV10170
sn="N"
xdias=180
@ 4,1 clea to 24,78
@ 21,0 to 21,79
@ 23,0 to 23,79
@ 22,25 SAY [CREAR ARCHIVOS PARA CONSULTAS] colo gr+/&cz
@ 24,70 say [Esc] COLO N/W
@ 24,74 say [Salir] colo w+/&cz

xchoice=1
xchoice2=2
xfilt=""
xtit=""
	defi wind win12 from 7,18 to 15,62 title [ Tipo de Equipo : ]  shadow colo sche 16
	acti wind win12
	@ 1,2 get xchoice func'*R Con sistema PALM-OS;Psion WorkAbout o Serie 3c' colo ,,,,,gr+/b,,,w+/w
	@ 6,16 get xchoice2 func'*H Aceptar;Cancelar' DEFAULT 1 colo ,,,,,gr+/b,,,n+/w

	read cycle
	rele wind win12
	if xchoice2=2
		clos all
		return
	endif	
	do case
		case xchoice=1
			DO PALM
		case xchoice=2
			DO PSION
	endcase	

*   ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
*   ³  PROGRAMA PARA PALM-OS  ³
*   ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROCEDURE PALM
clos all
neven=5
nevec=5
nevep=5
n=0
nx=0

@22,72 say [PALM-OS] colo w+/&cz
@22,1 say [# de Eventos ] colo w+/&cz
@24,17 say [Reproducci¢n] get neven pict'##' func't' colo w+/b
@24,35 say [Hospital] get nevec pict'##' func't' colo w+/b
@24,49 say [Producci¢n] get nevep pict'##' func't' colo w+/b
read
if lastkey()=27
	clos all
	return
endif	

use REG   order 2 alias REG in 1
use CALOR order 2 alias CAL in 2
use CLIN  order 2 alias CLI in 3
use PROD  order 1 alias PRO in 4

use PCAL2 alias PC2 in 5
use PCLI2 alias PCL in 6
use PPRO1 alias PPR in 7
use PREG2 alias PR2 in 8

** Empieza el Proceso
** ------------------
sele 5
zap
sele 6
zap
sele 7
zap
sele 8
zap

sele 1
set filt to fb2=B
go top

@24,1 say [ID] colo w+/&cz
scan
@24,4 say REG.id colo n/w	
@24,10 say nx pict'####'
nx=nx+1
xid=REG.id
xnp=REG.np

** Tarjeta General
** ---------------
repl DPR with (date()-fpar)-(date()-fsec),DSC with date()-FSEC

if ucal#ctod("")
 repl DIA with date()-ucal
endi

** Dias Abiertos
** -------------
if stat=[INSEM] .or. stat=[CARGA]
 repl DAB with ucal-fpar,FPS with ucal+(Q39-Q35),FPP with ucal+Q39
else
 repl DAB with date()-fpar,FPS with B,FPP with B
endif	

sele 8
append blank
repl ID with REG.id,IDT with REG.idt,NS with REG.ns,STAT with REG.stat,;
     FPAR with REG.fpar,np with REG.np,PRM with REG.prm,DAB with REG.DAB,;
     DIA with REG.dia,DPR with REG.dpr,CORR with REG.corr,DSC with REG.dsc,;
     FPRO with REG.fpro,MEDP with REG.medp,PSV with REG.psv,IDT1 with REG.idt1,;
     IDT2 with REG.idt2,IDT3 with REG.idt3,CODIGO with REG.codigo,;
     FPS with REG.fps,FPP with REG.fpp,OBSC with REG.obsc

** Empieza Ficha de Reproduccion
** -----------------------------
	sele 2
	seek xid
	if found()
		xlin=0
		scan while ID=xid for np=xnp 
			xlin=xlin+1
		endscan
		if xlin>neven
			skip-neven
		else
			skip-xlin
		endif	
		scan while CAL.id=REG.id for np=xnp
			sele 5
			appe blank
			repl ID with CAL.id,FECHA with CAL.fecha,NP with CAL.np,;
			STAT with CAL.stat,SER with CAL.ser,MED with CAL.med,;
			DIAS with CAL.dias
		endscan
	endif

** Empieza Ficha de Hospital
** -------------------------
	sele 3
	seek xid
	if found()
		xlin=0
		scan while ID=xid 
			xlin=xlin+1
		endscan
		if xlin>nevec
			skip-nevec
		else
			skip-xlin
		endif	
		scan while CLI.id=REG.id 
			sele 6
			appe blank
			repl ID with CLI.id,FECHA with CLI.fecha,;
			ENF with CLI.enf,TRAT with CLI.trat,DOSIS with CLI.dosis
		endscan
	endif

** Empieza Ficha de Producci¢n
** ---------------------------
	sele 4
	seek xid
	if found()
		xlin=0
		scan while ID=xid for np=xnp 
			xlin=xlin+1
		endscan
		if xlin>nevep
			skip-nevep
		else
			skip-xlin
		endif	
		scan while PRO.id=REG.id for np=xnp
			sele 7
			appe blank
			repl ID with PRO.id,FECHA with PRO.fp,;
			PRM with PRO.prd,TOTAL with PRO.tol,DEL with FECHA-REG.fpar
		endscan
	endif
**-------------------------------------------------------------------
sele 1
endscan
clos all

** Realiza Calculo de Inventario en Corrales
** -----------------------------------------
create table PLOT1 (Lote n(2),NVAC n(3),PROD n(4,1),DEL n(3),DESV n(4,1),PMAX n(4,1))
clos all

sele 3
USE PLOT1

sele 2
USE CORRAL order 1 alias CORRAL
sele 1
USE REG order 2

sele 2
GO TOP
dat=0
DO WHILE CNUM<100 or not eof()
dat=dat+1
NC=CNUM

sele 1
COUNT FOR CORR=NC and FPAR#B and FB2=B TO TV
if TV>0 
	do case
	case CORRAl.Tipo="P"
		CALCULATE AVG(PRM),MIN(PRM),MAX(PRM),STD(PRM) FOR CORR=NC and FPAR#B and FB2=B and prm>0 TO UM1,UO1,UP1,UQ1
		CALCULATE CNT(),AVG(DATE()-FPAR) FOR CORR=NC and FPAR#B and FB2=B TO UN1,UR1
		sele 3
		appe blank
		repl LOTE with NC,NVAC WITH TV,PROD WITH UM1,DEL with UR1,DESV WITH UQ1,PMAX with UP1
	case CORRAL.Tipo="S"
		CALCULATE CNT() FOR CORR=NC and FPAR#B and FB2=B TO UN1
		sele 3
		appe blank
		repl LOTE with NC,NVAC WITH TV,PROD WITH 0,DEL with 0,DESV WITH 0,PMAX with 0
	endcase
else
endif

sele 2
skip
if eof()
	exit
endif
ENDD 
clos all
** -----------------------------

** Copia para Hato 2
** -----------------
 COPY FILE PREG2.DBF to PREG0.DBF
 COPY FILE PCAL2.DBF to PCAL0.DBF
 COPY FILE PCLI2.DBF to PCLI0.DBF
 COPY FILE PPRO1.DBF to PPRO2.DBF
 COPY FILE PLOT1.DBF to PLOT2.DBF

RETURN

*  ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
*  ³  PROGRAMA PARA ENVIAR DATOS A PSION    ³
*  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROCEDURE PSION
sn="N"
xdias=180
@22,74 say [PSION] colo w+/&cz

@ 20,13 say [D¡as para incluir eventos en el Kardex Reproductivo ] get xdias pict '###' valid xdias>=30 and xdias<=400 error [ Rango de 30-400 dias Default 180 ] 
@ 24,68 say [Continuar] get SN PICT '@M N,S' colo n/w
read
	if lastkey()=27 or sn='N'
		return
	else
		@ 24,26 say [* Creando Archivo GENERAL *] colo r+/&cz
		@ 24,67 clea to 24,78
		@ 24,70 SAY [Espere] colo n*/w

		use reg order 2
		set filt to np>0 and fb2=B
		@ 20,1 say [ID] colo w+/b
	scan
		@ 20,4 say id colo n/w
		repl DPR with (date()-fpar)-(date()-fsec),DSC with date()-FSEC

			if ucal#ctod("")
				repl DIA with date()-ucal
			endi

	** CALCULO DE DIAS ABIERTOS 
	** ------------------------------------------
			if stat=[INSEM] .or. stat=[CARGA]
				repl DAB with ucal-fpar,FPS with ucal+(Q39-Q35),FPP with ucal+Q39
			else
				repl DAB with date()-fpar,FPS with B,FPP with B
			endi	
	** ------------------------------------------
	endscan

** Copiando datos de ARCHIVO GENERAL
** -------------------------------------
	set century off

	** PSION
	copy to PDATOS fields id,fpar,np,corr,stat,ns,ucal,utra,med1,fsec, ;
	idt,prm,prx,obs,abto,utec,palp,tecp,fpes,dpr,dia,dab,idt1,idt2,idt3,;
	psv,idm,idp,fpro,medp for np>0 and fb2=ctod("") DELIMITED

** Copiando datos de KARDEX REPRODUCTIVO
** -------------------------------------
	@ 24,21 say [* Creando Archivo KARDEX REPRODUCTIVO *] colo r+/&cz
	sele 1
	use REG order 2 alias XREG
	sele 2
	use CALOR order 2 alias CAL
	set relation to ID into XREG 
	
	**PSION
	copy to CDATOS fields ID,np,fecha,stat,med,toro,ser,tec,dias,obsc,medp ;
	for XREG.fb2=B and XREG.np=CAL.np and CAL.fecha>=date()-xdias DELIMITED

set relation to
set century on
clos data

@ 24,70 clea to 24,78

defi wind ntxt from 4,20 to 20,76 shadow colo sche 13
acti wind ntxt
dir pdatos.txt
dir cdatos.txt
wait [Presione cualquier tecla]
rele wind ntxt
endif
RETURN

*   ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
*   ³  PROGRAMA PARA BORRAR LAS VACAS DADAS DE BAJA COMN +365 DIAS  ³
*   ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROCEDURE DBAJA
@ 24,69 SAY [Eliminar] GET SN PICT '@M S,N' colo N/W
read
if lastkey()=27 or sn="N"
retu
else
??CHR(7)
wait wind [ SE RECOMIENDA REALIZAR UN RESPALDO DE DATOS PREVIO ] TIMEOUT 2
xx=0
dat=0
@ 24,31 say [Hasta el numero ] get xx pict '#####'
read
if lastkey()=27 or xx=0
retu
endi

use reg order 2
@ 22,1 say [ID] colo w+/&cz
@ 24,1 clea to 24,10
@ 24,1 say [Total] colo w+/&cz
go top

do while not eof()

		@22,5 say ID pict '#####' colo n/w
		if fb2#b and fb2<date()-365
			dat=dat+1
			@ 24,7 say dat pict'####'
		*skip
		xid=id
		do borrar 
		xid=id
			if xx<xid
				exit
			endi
	else
	skip
	xid=id
	endi

enddo
endi
do dcompact
retu

PROCEDURE BORRAR
** -------------
SET DELE OFF
??CHR(7)
@22,13 say "*" colo W+*/&cz

USE LACTS order 1 exclusive
seek XID
DELE WHILE ID=XID

USE PROD order 1 exclusive
seek XID
DELE WHILE ID=XID 

USE MAST order 1 exclusive
seek XID
DELE WHILE ID=XID

USE BST order 1 exclusive
seek XID
DELE WHILE ID=XID

USE CCS order 1 exclusive
seek XID
DELE WHILE ID=XID

use CLIN order 2 exclusive
seek XID
DELE WHILE ID=XID

use CALOR order 2 exclusive
seek XID
DELE WHILE ID=XID

USE VACUNAS order 2 exclusive
seek XID
DELE WHILE ID=XID

use PTB order 2 exclusive
seek XID
DELE WHILE ID=XID

USE CONDC order 1 exclusive
seek XID
DELE WHILE ID=XID

USE CALIF order 1 exclusive
seek XID
DELE WHILE ID=XID

USE REG order 2 exclusive
seek XID
DELE WHILE ID=XID 
*------------------------
set dele on
@22,12 clea to 22,15
retu


PROCEDURE DCOMPACT
** ---------------
nn=0
@ 24,60 clea to 24,78
SET DEFA TO (cmd_nam5)
@ 24,70 say [Espere] COLO N*/W
@ 24,28 SAY [Compactando Archivo] 

@ 24,49 SAY [0001] colo n/w
USE REG EXCLUSIVE
PACK

@ 24,49 SAY [0002] colo n/w
USE LACTS  Exclusive
DELE ALL FOR ID<1 or np=0
PACK

@ 24,49 SAY [0003] colo n/w
USE CLIN Exclusive
DELE ALL FOR FECHA=B or ID<1
PACK

@ 24,49 SAY [0004] colo n/w
USE PROD Exclusive
DELE ALL FOR ID<1 or FP=B
PACK

@ 24,49 SAY [0005] colo n/w
USE MAST Exclusive
DELE ALL FOR FTES=B or ID<1
PACK

@ 24,49 SAY [0006] colo n/w
USE CALOR Exclusive
DELE ALL FOR FECHA=B or ID<1
PACK

@ 24,49 SAY [0007] colo n/w
USE PTB Exclusive
DELE ALL FOR FPU=B or ID<1
PACK

@ 24,49 SAY [0008] colo n/w
USE BST Exclusive
DELE ALL FOR FECHA=B or ID<1
PACK

@ 24,49 SAY [0009] colo n/w
USE CONDC Exclusive
DELE ALL FOR FECHA=B or ID<1
PACK

@ 24,49 SAY [0010] colo n/w
USE CCS Exclusive
DELE ALL FOR FECHA=B or ID<1
PACK

@ 24,49 SAY [0011] colo n/w
USE VACUNAS exclusive
DELE ALL FOR FECHA=B or ID<1
PACK
@ 24,49 SAY [0012] colo n/w
USE MEDIDA exclusive
DELE ALL FOR ID<1
PACK

CLOS DATA
set defa to (cmd_nam0)
retu


*   ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
*   ³  KARDEX REPRODUCTIVO DE VACAS  ³
*   ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC RV10171
set esca on
on esca do ctrlstop

store ctod("") TO fin1,fin2
xdias=1
xdias2=1095
xfilt=""
store 1 to ncho1,ncho2,ncho3
@ 4,1 clea to 24,78
@ 21,0 to 21,79
@ 23,0 to 23,79
@ 22,25 SAY [LISTADO DE KARDEX REPRODUCTIVO] colo gr+/&cz
@ 24,29 say [* Solo en Impresora *] colo r+/&cz	
@ 24,70 say [Esc] COLO N/W
@ 24,74 say [Salir] colo w+/&cz


	defi wind win12 from 6,20 to 18,60 title [ Parametros ] shadow colo sche 16
	acti wind win12
		@ 1,2 SAY [Incluir Vacas   ] get ncho1 pict'@*C Abiertas' colo ,,,,,,,,w+/w
		@ 2,2 SAY [                ] get ncho2 func'*C Inseminadas' colo ,,,,,,,,w+/w
		@ 3,2 SAY [                ] get ncho3 func'*C Pre¤adas' colo ,,,,,,,,w+/w

		@ 5,2 SAY [Dias en Leche de] get xdias PICT '####' func'j,t' valid xdias>0 and xdias<1095 error [ Rango de 1 a 1091 ]
		@ 5,25 SAY [a ] get xdias2 PICT '####' func'j,t' valid xdias2>=xdias and xdias2<=1095 error [ Rango de 1 a 1095 ]

		@ 7,2 say [Fecha Inicial   ] get fin1 valid fin1<=date() and fin1#B error [FECHA NO VALIDA]
		@ 8,2 say [Fecha Final     ] get fin2 valid fin2>=fin1 and fin1#B and fin2<=date() error [FECHA NO VALIDA]
	READ
		 
	if lastkey()=27
		rele wind win12
	else
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

activate screen
		if sys(13)='READY'
			use reg order 2 alias REG in 1
			set filt to np>0 and fb2=B and &xfilt
			use calor order 2 alias CAL in 2

			set console off
		    set printer on
			*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
			*  Cambio a Letra 17 cpi
				???&q21
			*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
	xpag=1			
	ximprime=.T.
			nvac=0
			ntit=[LISTADO DE KARDEX REPRODUCTIVO]
			ntit2=dtoc(fin1)+" Al "+dtoc(fin2)+" Dias en Leche de"+str(xdias,4)+" a "+str(xdias2,4)

do titulo
_plineno=0

		DO WHILE not eof()
					
			if ximprime=.F.
				exit
			endi	

			scan

			if ximprime=.F.
				exit
			endi	

			if _plineno=Q45
				do titulo
				_plineno=0
			endi			
			
			sele 1
			xid=REG.id
			xnp=REG.np
			xduc=date()-ucal
			xdel=(date()-fpar)-(date()-fsec)
			if stat=[INSEM]
			xdab=ucal-fpar
			else
			xdab=date()-fpar
			endi
								
if xdel>=xdias and xdel<=xdias2
		?'  '+str(id,5)+'  '+str(corr,2)+'  '+fx(fpar)+' ';
		    +str(xdel,4)+'  '+stat+'  '+str(xduc,4);
		    +' '+str(xdab,4)+'  '+str(prm,4,1)+space(2)+repli(chr(95),82)

*	    ?space(52)+'Fecha    Status Med  Evento    Ins  Tec Dias  DEL  Observaciones   Programacion'

		** Empieza Ficha de Reproduccion
		** -----------------------------
		sele 2
		seek xid
		ndias=0
		scan while CAL.id=REG.id for np=xnp and FECHA>=fin1 and FECHA<=fin2

			if _plineno=Q45
				do titulo
				_plineno=0
			endi			

		xtoro=space(9)
		xser=0
		xtec=tec
		ndias=ndias+dias
		do case
			case TEC=[Rev]
				xtoro=[REVISION ]
				xtec="   "
			case TEC=[Cal]
				xtoro=[CELO     ]
				xtec="   "
			case TEC#space(3)
				xtoro=TORO					
		endcase				
		if stat=[CARGA]
			xtec=tecp
		endi

		if stat=[INSEM]
			xser=ser
			xtoro=TORO
		endi	

		?space(52)+fx(fecha)+'  '+stat+' '+str(med,3)+'  '+xtoro+' ';
		+str(xser,2)+'   '+xtec+' '+str(dias,4)+' '+str(ndias,4)+'  '+obsc+'  '+medp
		endscan
		** ------------------------------
*		?
		nvac=nvac+1
endi
endscan

endd				
		?'  '+repli(CHR(254),132)
		?
		?'   TOTAL = '+STR(nvac,4)
		EJEC
set prin off

*----------------------------------
SET CONS ON
SET FILT TO

ELSE
??CHR(7)
WAIT WIND [ IMPRESORA APAGADA O DESCONECTADA ] TIMEOUT 2
endi
ENDI

rele wind win12
clos all
on escape
set esca off
ximprime=.T.
on page
return


*   ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
*   ³  ESTADISTICA DE PRODUCCION POR CORRAL Y ULTIMAS DIEZ PESADAS  ³
*   ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC RV10172
declare aw(60),pw(16),tw(10),tv(5)
do while .t.
nvac=0
row=7
sn='N'
mc=0
xpag=0
pw=0
tw=0
tv=0
@ 4,1 clea to 24,78
@ 21,0 to 21,79
@ 23,0 to 23,79
@ 22,27 SAY [ESTADISTICA DE PRODUCCION] colo gr+/&cz
@ 24,70 say [Esc] COLO N/W
@ 24,74 say [Salir] colo w+/&cz
*------------
xchoice=1
xchoice2=2
xfilt=""
xtit=""
	defi wind win12 from 8,21 to 16,59 title [ Ordenado por : ]  shadow colo sche 16
	acti wind win12
	@ 1,2 get xchoice func'*R Identificaci¢n;Corral/Lote' colo ,,,,,gr+/b,,,w+/w
	@ 6,16 get xchoice2 func'*H Aceptar;Cancelar' DEFAULT 1 colo ,,,,,gr+/b,,,n+/w

	read cycle
	rele wind win12
	if xchoice2=2
		clos all
		return
	endif	
	do case
		case xchoice=1
			xfilt="ID"
			xtit="x ID"
		case xchoice=2
			xfilt="CORR,ID"
			xtit="x Corral"
	endcase	

@22,2 say xtit colo w+/b

*-------
DO CASE
CASE xchoice=1
		use reg order 2 alias REG in 1
		use prod order 2 alias PROD in 2 
		sele 1
		go top
		set filt to np>0 and fb2=B 

@ 24,69 clea to 24,78
@ 4,1 say [                                 VRel  VRel  Prod        Ultimas Pesadas      ] colo N/w
@ 5,1 say [    ID Cor Lac    DUC  DEL  DAB  Prom  Actu  Prom   7   6   5   4   3   2   1 ] colo N/w

	@ 24,1 say [Total  ] colo w+/&cz
scan 
	@ 24,7 say nvac+1 pict"####"

	xid=REG.id
	xnp=REG.np
	xfpar=REG.fpar
	xduc=date()-ucal
	xdel=(date()-fpar)-(date()-fsec)
	xprm=REG.prm

** Calculo de ERPA
** ----------------
if M305>0
	mfac=0.5
	mnreg=nreg+1
	mfac=(mnreg*mfac)/(1+((mnreg-1)*mfac))

repl ERPA with mfac*(((LIFEL+M305)/mnreg)-x305)						
endi
**--------------------------------------------**


	if stat=[INSEM] or stat=[CARGA]
		xdab=ucal-fpar
	else
		xdab=date()-fpar
	endi
	if lifel>0
		xval=((lifel/nreg)/x305)*100
	else
		xval=0
	endi
	if m305>0
		aval=(m305/x305)*100
	else
		aval=0
	endi
		cr=[ ]
		ab=[ ]
	if PSV="S"
		cr="+"
	endi
	if abto#B
		ab=[A]
	endi
** Traslada las pesadas a variables
** --------------------------------
xx=0
aw=space(2)
	sele 2
	xidp=str(xid,5)+fx(fp)
	seek left(xidp,5)
	if found()
			scan while xid=PROD.id and xnp=PROD.np and xx<10
				xx=xx+1
				aw(xx)=alltrim(aw(xx)+str(prd,3))
				if len(aw(xx))=1
 					aw(xx)=" "+aw(xx)
				endi
			endscan
	else
		aw=space(2)
		xx=0
	endi
** --------------------------------

sele 1
@ row,2 say xid pict '#####' colo gr+/&cz
*--------------------------*
if FPRO>=date() or FT2>=date() 
	@ row,2 SAY &mensa1
endi
if PSV="S" 
	@ row,7 say &mensa2
endi
	if abto#B
		@ row,1 say [A] colo w+/&cz
	endi
*--------------------------*
@ row,9 say CORR 
@ row,12 say NP
@ row,16 say left(stat,1) colo w+/&cz
@ row,19 say xduc pict '###'
@ row,24 say xdel pict '###'
@ row,29 say xdab pict '###'
@ row,35 say xval pict '###'
@ row,41 say aval pict '###'
@ row,46 say PRX pict '##.#' colo 3+/&cz

@ row,52 say aw(7) pict '##' colo r+/&cz
@ row,56 say aw(6) pict '##' colo r+/&cz
@ row,60 say aw(5) pict '##' colo r+/&cz
@ row,64 say aw(4) pict '##' colo r+/&cz
@ row,68 say aw(3) pict '##' colo r+/&cz
@ row,72 say aw(2) pict '##' colo r+/&cz
@ row,76 say aw(1) pict '##' colo r+/&cz
if row=20
	@ 24,69 clea to 24,78
	@ 24,70 say [Enter] colo n/w
	@ 24,76 say chr(16) colo w+/&cz
	
	do while inkey()#13
	if lastkey()=6
	go bottom
	exit
	endi
	endd
	row=6
	@ 7,1 clea to 20,78
endi	

row=row+1
	xx=1
	aw=0
	xid=id

** Calculo Total y Promedios
** -------------------------
	nvac=nvac+1
	if xdel>0
		pw(1)=pw(1)+1
		pw(2)=pw(2)+xdel
		tw(1)=pw(2)/pw(1)
	endi

	if xdab>0
		pw(3)=pw(3)+1
		pw(4)=pw(4)+xdab
		tw(2)=pw(4)/pw(3)
	endi
	if xprm>0
		pw(5)=pw(5)+1
		pw(6)=pw(6)+xprm
		tw(3)=pw(6)/pw(5)
	endi
	if plac>0
		pw(7)=pw(7)+1
		pw(8)=pw(8)+plac
		tw(4)=pw(8)/pw(7)
	endi
	if dpic>0
		pw(9)=pw(9)+1
		pw(10)=pw(10)+dpic
		tw(5)=pw(10)/pw(9)
	endi
	* Estadistica de Estado Reproductivo
	do case
		case left(stat,1)="I"
			tv(1)=tv(1)+1
		case left(stat,1)="C"
			tv(2)=tv(2)+1
		otherwise
		    tv(3)=tv(3)+1
	endcase
endscan

	??chr(7)
	@ 24,70 say [Enter] colo n/w
	@ 24,76 say chr(4) colo w+/&cz
	do while inkey()#13
	endd
	* Resumen de Datos
	*-----------------
	DEFI WIND CORRAL FROM 13,51 TO 19,78 TITLE [ RESUMEN ] DOUB SHADOW COLO SCHE 16
	ACTI WIND CORRAL
	@ 1,1 SAY [Inseminadas: ]+str(tv(1),3)+" "++str((tv(1)/nvac)*100,3)+"%" 
	@ 2,1 SAY [Pre¤adas   : ]+str(tv(2),3)+" "++str((tv(2)/nvac)*100,3)+"%"  
	@ 3,1 SAY [Abiertas   : ]+str(tv(3),3)+" "++str((tv(3)/nvac)*100,3)+"%"  
	ACTI SCREEN

	sn="N"
	@ 24,69 SAY [Imprimir] get SN pict '@M S,N' colo n/w 
	read
	if SN='N' or LASTKEY()=27
	else
	** Checa si esta encendida la impresora
	** ------------------------------------	
	IF sys(13)='READY'
		clos all
		nvac=0
		sn='N'
		pw=0
		tw=0
		tv=0
		@ 24,67 clea to 24,78
		@ 24,70 SAY [Espere] colo n*/w
			
		set console off
		set printer on
		*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
		*  Cambio a Letra 17 cpi
			???&q21
		*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
		xpag=1			
		ximprime=.t.
		nvac=0
		ntit=[ESTADISTICA DE PRODUCCION]
		ntit2=[CORRAL NUM.]+STR(MC,2)

		do titulo2
		_plineno=0
		
	DO WHILE not eof()
			if ximprime=.F.
				exit
			endi	

		use reg order 2 alias REG in 1
		use prod order 2 alias PROD in 2 
		sele 1
		go top
		set filt to np>0 and fb2=B 
scan 
	if ximprime=.F.
		exit
	endi	

	if _plineno=wli
		?'  '+repli(CHR(254),132)
		do titulo2
		_plineno=0
	endi	

	xid=REG.id
	xnp=REG.np
	xfpar=REG.fpar
	xduc=date()-ucal
	xdel=(date()-fpar)-(date()-fsec)
	xprm=REG.prm

** Calculo de ERPA
** ----------------
if M305>0
	mfac=0.5
	mnreg=nreg+1
	mfac=(mnreg*mfac)/(1+((mnreg-1)*mfac))

repl ERPA with mfac*(((LIFEL+M305)/mnreg)-x305)						
endi
**--------------------------------------------**


	if stat=[INSEM] or stat=[CARGA]
		xdab=ucal-fpar
	else
		xdab=date()-fpar
	endi
	if lifel>0
		xval=((lifel/nreg)/x305)*100
	else
		xval=0
	endi
	if m305>0
		aval=(m305/x305)*100
	else
		aval=0
	endi
		cr=[ ]
		ab=[ ]
	if PSV="S"
		cr="+"
	endi
	if abto#B
		ab=[A]
	endi
		xx=0
		aw=space(2)


	sele 2

		xidp=str(xid,5)+fx(fp)

	seek left(xidp,5)
	
	if found()
			scan while xid=PROD.id and xnp=PROD.np and xx<10
				xx=xx+1
				aw(xx)=alltrim(aw(xx)+str(prd,3))
				if len(aw(xx))=1
 					aw(xx)=" "+aw(xx)
				endi


			endscan
	else
		aw=space(2)
		xx=0
	endi
sele 1
** IMPRIME
		?'  '+str(id,5)+cr+ab+' '+str(corr,2)+'  '+str(np,2)+'  '+stat+' ';
		    +str(xduc,4)+'  '+str(xdel,4)+'  '+str(xdab,4)+'  '+str(ns,2);
		    +'   '+str(xval,3)+'   '+str(aval,3)+"  "+str(prx,4,1)+'   '+aw(10)+"   "+aw(9)+"   ";
		    +aw(8)+"   "+aw(7)+"   "+aw(6)+"   "+aw(5)+"   "+aw(4)+"   "+aw(3);
		    +"   "+aw(2)+"   "+aw(1)+"  "+fx(pesa)+"  "+str(plac,4,1)+"  "+str(dpic,3)
	xx=1
	aw=0
	xid=id

** Calculo Total y Promedios
** -------------------------
	nvac=nvac+1
	if xdel>0
		pw(1)=pw(1)+1
		pw(2)=pw(2)+xdel
		tw(1)=pw(2)/pw(1)
	endi

	if xdab>0
		pw(3)=pw(3)+1
		pw(4)=pw(4)+xdab
		tw(2)=pw(4)/pw(3)
	endi
	if xprm>0
		pw(5)=pw(5)+1
		pw(6)=pw(6)+xprm
		tw(3)=pw(6)/pw(5)
	endi
	if plac>0
		pw(7)=pw(7)+1
		pw(8)=pw(8)+plac
		tw(4)=pw(8)/pw(7)
	endi
	if dpic>0
		pw(9)=pw(9)+1
		pw(10)=pw(10)+dpic
		tw(5)=pw(10)/pw(9)
	endi

endscan
ENDD
		?'  '+repli(CHR(254),132)
		?SPACE(18)+'PROMEDIO'+space(4);
			 		  +str(tw(1),4)+space(2)+str(tw(2),4)+space(69);
			 		  +str(tw(3),3)+space(12)+str(tw(4),4,1)+space(2);
					  +str(tw(5),3)	
        ?'   TOTAL VACAS='+STR(NVAC,4)
        ?'   INSEMINADAS= '+str(tv(1),3)+'  '+str((tv(1)/nvac)*100,3)+"%"
        ?'   PRE¥ADAS   = '+str(tv(2),3)+'  '+str((tv(2)/nvac)*100,3)+"%"
        ?'   ABIERTAS   = '+str(tv(3),3)+'  '+str((tv(3)/nvac)*100,3)+"%"
eject
set prin off
set cons on
ELSE
??CHR(7)
WAIT WIND [ IMPRESORA APAGADA O DESCONECTADA ] TIMEOUT 2
ENDI
clos all
on escape
ximprime=.t.
endi

clos all
rele wind CORRAL

case xchoice=2 
@ 22,1 say [Corral ] get mc pict '##' colo w+/b,n/w
@ 22,14 say [0] colo w+/&cz
@ 22,16 say [=Todos]
read
	if lastkey()=27
		exit
	endi

	if mc>0
		mm=[corr=mc]
	else
		mm=[corr>0]
	endi		
		use reg order 4 alias REG in 1
		use prod order 2 alias PROD in 2 
		sele 1
		go top
		set filt to np>0 and fb2=B and &mm 

@ 24,69 clea to 24,78
@ 4,1 say [                                 VRel  VRel  Prod        Ultimas Pesadas      ] colo N/w
@ 5,1 say [    ID Cor Lac    DUC  DEL  DAB  Prom  Actu  Prom   7   6   5   4   3   2   1 ] colo N/w

	@ 24,1 say [Total  ] colo w+/&cz
scan 
	@ 24,7 say nvac+1 pict"####"

	xid=REG.id
	xnp=REG.np
	xfpar=REG.fpar
	xduc=date()-ucal
	xdel=(date()-fpar)-(date()-fsec)
	xprm=REG.prm
	
** Calculo de ERPA
** ----------------
if M305>0
	mfac=0.5
	mnreg=nreg+1
	mfac=(mnreg*mfac)/(1+((mnreg-1)*mfac))

repl ERPA with mfac*(((LIFEL+M305)/mnreg)-x305)						
endi
**--------------------------------------------**


	if stat=[INSEM] or stat=[CARGA]
		xdab=ucal-fpar
	else
		xdab=date()-fpar
	endi
	if lifel>0
		xval=((lifel/nreg)/x305)*100
	else
		xval=0
	endi
	if m305>0
		aval=(m305/x305)*100
	else
		aval=0
	endi
		cr=[ ]
		ab=[ ]
	if PSV="S"
		cr="+"
	endi
	if abto#B
		ab=[A]
	endi
** Traslada las pesadas a variables
** --------------------------------
xx=0
aw=space(2)
	sele 2
	xidp=str(xid,5)+fx(fp)
	seek left(xidp,5)
	if found()
			scan while xid=PROD.id and xnp=PROD.np and xx<10
				xx=xx+1
				aw(xx)=alltrim(aw(xx)+str(prd,3))
				if len(aw(xx))=1
 					aw(xx)=" "+aw(xx)
				endi
			endscan
	else
		aw=space(2)
		xx=0
	endi
** --------------------------------

sele 1
@ row,2 say xid pict '#####' colo gr+/&cz
*--------------------------*
if FPRO>=date() or FT2>=date() 
	@ row,2 SAY &mensa1
endi
if PSV="S" 
	@ row,7 say &mensa2
endi
	if abto#B
		@ row,1 say [A] colo w+/&cz
	endi
*--------------------------*
@ row,9 say CORR 
@ row,12 say NP
@ row,16 say left(stat,1) colo w+/&cz
@ row,19 say xduc pict '###'
@ row,24 say xdel pict '###'
@ row,29 say xdab pict '###'
@ row,35 say xval pict '###'
@ row,41 say aval pict '###'
@ row,46 say PRX pict '##.#' colo 3+/&cz

@ row,52 say aw(7) pict '##' colo r+/&cz
@ row,56 say aw(6) pict '##' colo r+/&cz
@ row,60 say aw(5) pict '##' colo r+/&cz
@ row,64 say aw(4) pict '##' colo r+/&cz
@ row,68 say aw(3) pict '##' colo r+/&cz
@ row,72 say aw(2) pict '##' colo r+/&cz
@ row,76 say aw(1) pict '##' colo r+/&cz
if row=20
	@ 24,69 clea to 24,78
	@ 24,70 say [Enter] colo n/w
	@ 24,76 say chr(16) colo w+/&cz
	
	do while inkey()#13
	if lastkey()=6
	go bottom
	exit
	endi
	endd
	row=6
	@ 7,1 clea to 20,78
endi	

row=row+1
	xx=1
	aw=0
	xid=id

** Calculo Total y Promedios
** -------------------------
	nvac=nvac+1
	if xdel>0
		pw(1)=pw(1)+1
		pw(2)=pw(2)+xdel
		tw(1)=pw(2)/pw(1)
	endi

	if xdab>0
		pw(3)=pw(3)+1
		pw(4)=pw(4)+xdab
		tw(2)=pw(4)/pw(3)
	endi
	if xprm>0
		pw(5)=pw(5)+1
		pw(6)=pw(6)+xprm
		tw(3)=pw(6)/pw(5)
	endi
	if plac>0
		pw(7)=pw(7)+1
		pw(8)=pw(8)+plac
		tw(4)=pw(8)/pw(7)
	endi
	if dpic>0
		pw(9)=pw(9)+1
		pw(10)=pw(10)+dpic
		tw(5)=pw(10)/pw(9)
	endi
	* Estadistica de Estado Reproductivo
	do case
		case left(stat,1)="I"
			tv(1)=tv(1)+1
		case left(stat,1)="C"
			tv(2)=tv(2)+1
		otherwise
		    tv(3)=tv(3)+1
	endcase
endscan

	??chr(7)
	@ 24,70 say [Enter] colo n/w
	@ 24,76 say chr(4) colo w+/&cz
	do while inkey()#13
	endd
	* Resumen de Datos
	*-----------------
	DEFI WIND CORRAL FROM 13,51 TO 19,78 TITLE [ RESUMEN ] DOUB SHADOW COLO SCHE 16
	ACTI WIND CORRAL
	@ 1,1 SAY [Inseminadas: ]+str(tv(1),3)+" "++str((tv(1)/nvac)*100,3)+"%" 
	@ 2,1 SAY [Pre¤adas   : ]+str(tv(2),3)+" "++str((tv(2)/nvac)*100,3)+"%"  
	@ 3,1 SAY [Abiertas   : ]+str(tv(3),3)+" "++str((tv(3)/nvac)*100,3)+"%"  
	ACTI SCREEN

	sn="N"
	@ 24,69 SAY [Imprimir] get SN pict '@M S,N' colo n/w 
	read
	if SN='N' or LASTKEY()=27
	else
	** Checa si esta encendida la impresora
	** ------------------------------------	
	IF sys(13)='READY'
		clos all
		nvac=0
		sn='N'
		pw=0
		tw=0
		tv=0
		@ 24,67 clea to 24,78
		@ 24,70 SAY [Espere] colo n*/w
			
		set console off
		set printer on
		*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
		*  Cambio a Letra 17 cpi
			???&q21
		*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
		xpag=1			
		ximprime=.t.
		nvac=0
		ntit=[ESTADISTICA DE PRODUCCION]
		ntit2=[CORRAL NUM.]+STR(MC,2)

		do titulo2
		_plineno=0
		
	DO WHILE not eof()
			if ximprime=.F.
				exit
			endi	

			if mc>0
				mm=[corr=mc]
			else
				mm=[corr>0]
			endi
					
		use reg order 4 alias REG in 1
		use prod order 2 alias PROD in 2 
		sele 1
		go top
		set filt to np>0 and fb2=B and &mm 
		ncorral=REG.corr
scan  
	xcorral=REG.corr 	
		if xcorral#ncorral and nvac>0
			*** Imprime Promedios por Corral
			*** ----------------------------
			?'  '+repli(CHR(254),132)
			?SPACE(18)+'PROMEDIO'+space(4);
			 		  +str(tw(1),4)+space(2)+str(tw(2),4)+space(69);
			 		  +str(tw(3),3)+space(12)+str(tw(4),4,1)+space(2);
					  +str(tw(5),3)	
    	    ?'   TOTAL VACAS='+STR(NVAC,4)
        	?'   INSEMINADAS= '+str(tv(1),3)+'  '+str((tv(1)/nvac)*100,3)+"%"
	        ?'   PRE¥ADAS   = '+str(tv(2),3)+'  '+str((tv(2)/nvac)*100,3)+"%"
    	    ?'   ABIERTAS   = '+str(tv(3),3)+'  '+str((tv(3)/nvac)*100,3)+"%"
			*** -----------------------------
			do titulo2
			_plineno=0
			tv=0
			tw=0
			pw=0
			nvac=0
		endif
	
	if ximprime=.F.
		exit
	endi	

	if _plineno=wli
		?'  '+repli(CHR(254),132)
		do titulo2
		_plineno=0
	endi	

	xid=REG.id
	xnp=REG.np
	xfpar=REG.fpar
	xduc=date()-ucal
	xdel=(date()-fpar)-(date()-fsec)
	xprm=REG.prm

** Calculo de ERPA
** ----------------
if M305>0
	mfac=0.5
	mnreg=nreg+1
	mfac=(mnreg*mfac)/(1+((mnreg-1)*mfac))

repl ERPA with mfac*(((LIFEL+M305)/mnreg)-x305)						
endi
**--------------------------------------------**


	if stat=[INSEM] or stat=[CARGA]
		xdab=ucal-fpar
	else
		xdab=date()-fpar
	endi
	if lifel>0
		xval=((lifel/nreg)/x305)*100
	else
		xval=0
	endi
	if m305>0
		aval=(m305/x305)*100
	else
		aval=0
	endi
		cr=[ ]
		ab=[ ]
	if PSV="S"
		cr="+"
	endi
	if abto#B
		ab=[A]
	endi
		xx=0
		aw=space(2)


	sele 2

		xidp=str(xid,5)+fx(fp)

	seek left(xidp,5)
	
	if found()
			scan while xid=PROD.id and xnp=PROD.np and xx<10
				xx=xx+1
				aw(xx)=alltrim(aw(xx)+str(prd,3))
				if len(aw(xx))=1
 					aw(xx)=" "+aw(xx)
				endi


			endscan
	else
		aw=space(2)
		xx=0
	endi
sele 1
** IMPRIME
		?'  '+str(id,5)+cr+ab+' '+str(corr,2)+'  '+str(np,2)+'  '+stat+' ';
		    +str(xduc,4)+'  '+str(xdel,4)+'  '+str(xdab,4)+'  '+str(ns,2);
		    +'   '+str(xval,3)+'   '+str(aval,3)+"  "+str(prx,4,1)+'   '+aw(10)+"   "+aw(9)+"   ";
		    +aw(8)+"   "+aw(7)+"   "+aw(6)+"   "+aw(5)+"   "+aw(4)+"   "+aw(3);
		    +"   "+aw(2)+"   "+aw(1)+"  "+fx(pesa)+"  "+str(plac,4,1)+"  "+str(dpic,3)
	xx=1
	aw=0
	xid=id

** Calculo Total y Promedios
** -------------------------
	nvac=nvac+1
	if xdel>0
		pw(1)=pw(1)+1
		pw(2)=pw(2)+xdel
		tw(1)=pw(2)/pw(1)
	endi

	if xdab>0
		pw(3)=pw(3)+1
		pw(4)=pw(4)+xdab
		tw(2)=pw(4)/pw(3)
	endi
	if xprm>0
		pw(5)=pw(5)+1
		pw(6)=pw(6)+xprm
		tw(3)=pw(6)/pw(5)
	endi
	if plac>0
		pw(7)=pw(7)+1
		pw(8)=pw(8)+plac
		tw(4)=pw(8)/pw(7)
	endi
	if dpic>0
		pw(9)=pw(9)+1
		pw(10)=pw(10)+dpic
		tw(5)=pw(10)/pw(9)
	endi

	* Estadistica de Estado Reproductivo
	do case
		case left(stat,1)="I"
			tv(1)=tv(1)+1
		case left(stat,1)="C"
			tv(2)=tv(2)+1
		otherwise
		    tv(3)=tv(3)+1
	endcase
	ncorral=REG.corr
endscan
ENDD
		?'  '+repli(CHR(254),132)
		?SPACE(18)+'PROMEDIO'+space(4);
			 		  +str(tw(1),4)+space(2)+str(tw(2),4)+space(69);
			 		  +str(tw(3),3)+space(12)+str(tw(4),4,1)+space(2);
					  +str(tw(5),3)	
        ?'   TOTAL VACAS='+STR(NVAC,4)
        ?'   INSEMINADAS= '+str(tv(1),3)+'  '+str((tv(1)/nvac)*100,3)+"%"
        ?'   PRE¥ADAS   = '+str(tv(2),3)+'  '+str((tv(2)/nvac)*100,3)+"%"
        ?'   ABIERTAS   = '+str(tv(3),3)+'  '+str((tv(3)/nvac)*100,3)+"%"
eject
set prin off
set cons on
ELSE
??CHR(7)
WAIT WIND [ IMPRESORA APAGADA O DESCONECTADA ] TIMEOUT 2
ENDI
clos all
on escape
ximprime=.t.
endi

clos all
rele wind CORRAL

ENDCASE
ENDDO

clos all
on escape
set esca off
ximprime=.t.
return


*   ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
*   ³  ANALISIS DE DETECCION DE CALORES EN 21/24 DIAS   ³
*   ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC RV10173
set decimals to 1
dimension aw(6)
aw=0
aw(6)=24
@ 4,1 clea to 24,78
@ 21,0 to 21,79
@ 23,0 to 23,79
@ 22,25 SAY [DETECCION DE CALORES EN 24 DIAS] colo gr+/&cz
@ 24,25 say [   DIAS EN LECHE DE    A    ] colo gr+/&cz
@ 24,70 say [Esc] COLO N/W
@ 24,74 say [Salir] colo w+/&cz

@ 22,49 get aw(6) pict '##' valid aw(6)>=21 and aw(6)<=24 error [ Rango de 21 a 24 Dias ] default 21
@ 24,45 get aw(4) pict '##' valid aw(4)>=30 and aw(4)<=99 error [ Rango de 30 a 99 Dias ]
@ 24,50 get aw(5) pict '###' valid aw(5)>aw(4) and aw(5)<=150 error [ Rango hasta 150 Dias ]
read
	if lastkey()=27
		rele all like aw*
		retu
	endi

	@ 22,49 say aw(6) pict '##' colo w+/&cz
	@ 24,45 say aw(4) pict '##' colo w+/&cz
	@ 24,50 say aw(5) pict '###' colo w+/&cz
	@ 22,65 say [De] colo w+/&cz
	@ 22,68 say date()-aw(6)
use reg order 2
set filt to np>0 and fb2=b
go top

	@ 24,69 clea to 24,78
	@ 24,70 SAY [Espere] COLO N*/W

	@ 4,1 say [     ID  Fecha    Lact       Ultimo   Ult   Ultimo   Dias  Edo Num  Prod Corr ] colo N/w
	@ 5,1 say [    Num  Parto    Num   DEL  Tratam   Med   Calor    Tran  Rep Ser  Actu  Num ] colo N/w

	@ 22,1 say [Total] colo w+/&cz
	@ 24,1 say [Detec] colo w+/&cz
	@ 22,7 say chr(4) colo w+/w
	@ 22,8 say chr(4) colo w+/r
	@ 24,7 say chr(4) colo w+/w

ROW=7
COL=2

scan for (date()-fpar)-(date()-fsec)>=aw(4)+aw(6) ;
	     and (date()-fpar)-(date()-fsec)<=aw(5)+aw(6)

		aw(1)=aw(1)+1
		@ 22,10 say aw(1) pict '####' 
		@row,col+1 say id pict'#####' colo w+/r

**-------------------------------------*
		do case

			case ucal>=date()-aw(6) and ucal<=date()
				aw(2)=aw(2)+1
				@ 24,10 say aw(2) pict '####' 
       			@row,col+1 say id pict'#####' colo w+/w

			case stat=[INSEM] and ucal<date()-aw(6) or ;
				 stat=[CARGA] and ucal<date()-aw(6)
				aw(1)=aw(1)-1
				@row,col+1 say id pict'#####' colo gr+/&cz
				
		endcase	
**-------------------------------------*
@ row,col+8 SAY fx(FPAR)
@ row,col+18 SAY NP
@ row,col+22 SAY (DATE()-FPAR)-(DATE()-FSEC) pict '9999'
@ row,col+28 SAY fx(UTRA)
@ row,col+37 SAY MED1 PICT '###'
@ row,col+43 SAY fx(UCAL) 
@ row,col+53 SAY date()-UCAL PICT '999'
@ row,col+59 SAY LEFT(stat,1) COLO W+/&CZ
	if stat=[ABORT]
		@ row,col+60 SAY [AB] COLO W+/&CZ
	endi
@ row,col+62 SAY NS pict '99'
@ row,col+67 SAY prm pict '99.9' colo r+/b
@ row,col+73 say CORR
**-------------------------------------*
if row=20
	row=6
	@ 24,69 clea to 24,78
	@ 24,71 SAY [Enter] COLO N/W
	@ 24,77 SAY chr(16) colo w+/&cz
do while inkey()#13
endd
	@7,2 clea to 20,77
endi	
row=row+1
ends
@ 24,17 SAY (aw(2)/aw(1)*100) PICT '99.9' FUNC 'J' colo w+/&cz
@ 24,21 SAY '%' colo w+/&cz
??chr(7)
@ 24,69 clea to 24,78
@ 24,71 SAY [Enter] COLO N/W
@ 24,77 SAY chr(4) colo w+/&cz
do while inkey()#13
endd

IF aw(1)=0
ELSE
SN='N'
@ 24,68 SAY [Imprimir] GET SN PICT '@M S,N' colo n/w 
READ
IF SN='N' or LASTKEY()=27
ELSE
IF SN='S' .AND. SYS(13)='READY'
SET CONS OFF
SET PRIN ON
aw(1)=0
AW(2)=0
GO TOP
NVE=0
PAG=1
NVAC=0
NTIT=[DETECCION DE CALORES EN ]+STR(AW(6),2)+[ DIAS]
NTIT2=[DE ]+STR(aw(4),3)+[ A ]+STR(AW(5),3)+[ DIAS EN LECHE A PARTIR DEL ]+DTOC(DATE()-AW(6))
DO WHILE .NOT. EOF()
DO CTRLVRPT

?'     ID   FECHA     LACT          ULTIMO    ULT   ULTIMO    DIAS   ESTADO   ULT   CORRAL'
?'   VACA   PARTO     NUM    DEL    TRATAM    MED   CALOR     TRAN   REPROD   PROD    NUM '

?'  '+REPLI('-',94)
scan for (date()-fpar)-(date()-fsec)>=aw(4)+aw(6) ;
	     and (date()-fpar)-(date()-fsec)<=aw(5)+aw(6) WHILE INKEY()#6
cra=[-]
		aw(1)=aw(1)+1
**-------------------------------------*
		do case

			case ucal>=date()-aw(6) and ucal<=date()
				aw(2)=aw(2)+1
				cra=[*]

			case stat=[INSEM] and ucal<date()-aw(6) or ;
				 stat=[CARGA] and ucal<date()-aw(6)
				aw(1)=aw(1)-1
				cra=[ ]				
		endcase	
**-------------------------------------*
?'  '+STR(ID,5)+CRA+'  '+fx(FPAR)+'   '+STR(NP,2)+'   ';
	 +STR((DATE()-FPAR)-(DATE()-FSEC),4)+'    '+fx(UTRA)+'  ';
	 +STR(MED1,3)+'   '+fx(UCAL)+'  '+STR(DATE()-UCAL,4)+'    ';
	 +STAT+'   '+STR(PRM,4,1)+'    '+STR(CORR,2)

NVE=NVE+1
NVAC=NVAC+1
IF NVE=WLI
?
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
?'  TOTAL DE VACAS  = '+STR(NVAC,4)
?'  EXCLUIDAS  ( )  = '+STR(NVAC-aw(1),4)
?'  DETECTADAS (*)  = '+STR(aw(2),4)
?'  NO DETECT. (-)  = '+STR((aw(1)-aw(2)),4)
?'  DETECCION  (%)  = '+STR((aw(2)/aw(1))*100,4,1)
EJEC
SET PRIN OFF
SET FILT TO
SET CONS ON
ELSE
??CHR(7)
WAIT WIND [ IMPRESORA APAGADA O DESCONECTADA ] TIMEOUT 2
SET FILT TO
ENDI
ENDI
ENDI
CLOS DATA
return


PROCEDURE TITULO
** -------------
if xpag>1
eject page
endi

?'  Rancho   : '+Q36+'   '+nom
?'  Reporte  : '+NTIT
?'  Fecha    : '+DTOC(DATE())+'  Hora : '+TIME()
?'  Pagina   : '+STR(xpag,2,0)+space(108)+[LACTOFOX]
?'  Periodo  : '+NTIT2
		?'  '+repli(CHR(254),132)
		?'    ID   NUM  FECHA         ESTADO             ULT'
		?'   VACA  COR  PARTO    DEL  REPROD  DUC  DAB   PES   FECHA   STATUS MED  EVENTO   SER#  TEC DIAS  DEL  OBSERVACIONES   PROGRAMACION'
		?'  '+repli(CHR(254),132)
xpag=xpag+1
return

PROCEDURE TITULO0
** -------------
if xpag>1
eject page
endi

?'  Rancho   : '+Q36+'   '+nom
?'  Reporte  : '+NTIT
?'  Fecha    : '+DTOC(DATE())+'  Hora : '+TIME()
?'  Pagina   : '+STR(xpag,2,0)+space(108)+[LACTOFOX]
?'  Periodo  : '+NTIT2
		?'  '+repli(CHR(254),132)
		?'    ID   NUM  FECHA         ESTADO             ULT'
		?'   VACA  COR  PARTO    DEL  REPROD  DUC  DAB   PES'
		?'  '+repli(CHR(254),49)+space(83)
xpag=xpag+1
return

PROCEDURE TITULO2
** --------------
if xpag>1
eject page
endi

?'  Rancho   : '+Q36+'   '+nom
?'  Reporte  : '+NTIT
?'  Fecha    : '+DTOC(DATE())+'  Hora : '+TIME()
?'  Pagina   : '+STR(xpag,2,0)+space(108)+[LACTOFOX]
		?'  '+repli(CHR(254),132)
		?'    ID   COR LAC  EDO                    SER  VREL  VREL  PROD  [    P E S A D A S  A N T E R I O R E S    ] ULT  F. ULT   PICO  PROD'
		?'   VACA  NUM NUM  REPRO  DUC   DEL   DAB NUM  PROM  ACTU  PROM  (10)  (9)  (8)  (7)  (6)  (5)  (4)  (3)  (2) PES  PESADA    KGS  DIAS'
		?'  '+repli('-',132)
xpag=xpag+1
return



PROCEDURE TITULO3
** -------------
if xpag>1
eject page
endi

?'  Rancho   : '+Q36+'   '+nom
?'  Reporte  : '+NTIT
?'  Fecha    : '+DTOC(DATE())+'  Hora : '+TIME()
?'  Pagina   : '+STR(xpag,2,0)+space(108)+[LACTOFOX]
		?'  '+repli(CHR(254),132)
		?'    ID                 ULT   ESTADO ULTIMO         ULTIMA    ULTIMO   NUM           CAUSA'
		?'   VACA  CN   DEL LAC  PROD  REPROD MEDICAMENTO    REV/TX    CALOR    SER   DUC   NO INSEM    TEC       SEMEN O TRATAMIENTO'
		?'  '+repli(CHR(254),132)
xpag=xpag+1
return

