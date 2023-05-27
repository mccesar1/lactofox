* PESADA *
*
DO CTRLSENT
*
set esca off
private factor,pw1,pw2
@ 4,0 clea to 24,79
factor=1
pw1="N"
pw2="N"
*ออออออออออออออออออออออออออออออออออออออออ
	IF CLAVEX=Q22 or CLAVEX=Q23 
	ELSE
    ??CHR(7)
    WAIT WIND [ Acceso Negado ] TIMEOUT 1
    RETU
    ENDI
*ออออออออออออออออออออออออออออออออออออออออ
hide popup foo1
hide popup soo2

** EMPIEZA PROGRAMA DE PESADA
** --------------------------
do case
   case BAR()=1
        DO Captura
   case BAR()=2
        DO Borrar 
   case BAR()=3
        DO Pexpo
   case BAR()=4
        DO Pimpo
   case BAR()=5
        DO Pimpo2
   case BAR()=6
        DO Resumen
   case BAR()=7
        DO Lista
   case BAR()=8
        DO Actualizar
   case BAR()=9
        DO Cambiadas
   case BAR()=10
        DO Hojat
   case Lastkey()=27
        clos all
        exit
endcase
@ 4,0 clea to 24,79
@23,0 to 23,79
return

*   ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*   ณ    CAPTURA DE LA PESADA     ณ
*   ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
PROC CAPTURA
xtit=""
on key label F10 do FICHA  && Pendiente 29/sep/99

    RTIT=[PESADA DE LECHE - CAPTURA]
    @ 4,0 clea to 24,79
	@ 22,(80-LEN(RTIT))/2 SAY RTIT colo gr+/&cz
	@23,0 to 23,79
	@24,70 SAY [Esc] colo n/w
	@24,74 say [Salir] 

defi wind werror from 1,60 to 3,78 shadow colo w/r
defi wind win12 from 10,20 to 16,60 title [ Parametros ] shadow colo sche 16
acti wind win12
@ 1,1 say [Incluir Datos de Grasa y Proteกna.] get pw1 pict '@M N,S'
@ 1,36 say [N] colo w+/n
@ 3,1 say [Cambiar por el corral de Captura..] get pw2 pict '@M N,S'
read
rele wind win12
*-------
defi wind P1 from 5,24 to 19,56 title [ PRODUCCION ] shadow colo sche 16
use REG order 2
*set filt to FSEC=B or FSEC>=HOY 
DO WHILE .T.
store space(2) to MCO
XID=0
NPES=0

@24,1 SAY [Corral] GET MCO PICT '##' FUNC 'j,t' VALID CHCORRP(MCO) ERROR tit(2) colo w+/&cz,n/w
READ

if ASC(MCO)=0 and pw2="S"
 loop
endi

if lastkey()=27 or val(MCO)=0
 @4,0 clea to 24,79
 @23,0 to 23,79
 on key label F10 *
 return
endi

@ 24,11 SAY [Turno] GET NPES PICT '##' FUNC 'j,t' VALID NPES>0 and NPES<5 ERROR [Pesa Valida 1-2-3-4 ] colo w+/&cz,n/w
read
 if lastkey()=27 or NPES=0
   @4,0 clea to 24,79
   @23,0 to 23,79
   return
 endi

XID2=0

DO WHILE .T. and VAL(MCO)>0
	SN='N'
	XID=0
    @22,11 clea to 22,24
    @24,55 say [F10] COLO N/W
	@24,59 say [Historial]
    @24,70 SAY [Esc] colo n/w
    @24,74 say [Salir] 
    @22,70 SAY [IDa] COLO N/W
	@22,74 SAY XID2 PICT '#####' FUNC 'J' colo w+/&cz

	@ 22,1 SAY [ID] get XID PICT '#####' FUNC 'T' colo w+/&cz,n/w
	read
	*-----------------------*
	if XID=0
		exit
	endi

seek XID
if not found()
	??CHR(7)
@22,11 say [ NO Existe ] colo w+/r
*	acti wind werror
*	@0,1 say [ID no Valida] colo w+/r
	do while INKEY() # 27
	endd
*	deact wind werror
else
do case
	case FB2#B
		??CHR(7)
		@22,11 SAY [Baja] colo w+/R
		do while inkey()#27
		enddo

	case FSEC#B and FSEC<=HOY and FB2=B
	??CHR(7)
	@22,11 say [ Seca ] colo w+/r
	do while INKEY() # 27
	endd

	case NPES=1 and P1>0 or NPES=2 and P2>0 or NPES=3 and P3>0 
			SET BELL TO 315,3
			??CHR(7)
			@22,11 SAY [ Repetida ] colo w+/R
			ACTI WIND P1
			@ 1,2 say [Pesa #1 .........]   
			@ 2,2 say [Pesa #2 .........]
			@ 3,2 say [Pesa #3 .........]
			@ 4,2 say [Total ...........]
			@ 6,2 say [% Grasa .........] 
			@ 7,2 say [% Proteกna.......]
			@ 9,2 say [Prod. Ant........]
			@10,2 say [Dias/Leche ......]
			@11,2 say [Corral Num.......]

			@ 1,24 say P1 PICT '99.9' FUNC 'J' colo w+/n
			@ 2,24 SAY P2  PICT '99.9' FUNC 'J' colo w+/n
			@ 3,24 SAY P3  PICT '99.9' FUNC 'J' colo w+/n 
			@ 4,23 SAY P1+P2+P3 PICT '#99.9' FUNC 'J'  colo sche 18
			@ 6,24 SAY MGRA pict '#.##' COLO W+/N
			@ 7,24 SAY MPRO pict '#.##' COLO W+/N

			@ 9,24 say prm pict '99.9' colo sche 18
			@10,24 SAY DATE()-FPAR PICT '9999' FUNC 'J' colo sche 18
			@11,26 SAY CORR PICT '##' FUNC 'J' colo sche 18   

			DO WHILE INKEY() # 27
			ENDD
			DEACT WIND P1

	case fsec=B or fsec#B and fsec>HOY
			@ 19,60 clea to 19,78

			IF HOSP=1
				@ 22,11 SAY [ En Hospital ] colo w+/R
			ENDI
			
			if pesa=HOY
				??chr(7)
				wait wind [ Ya Actualizada ] timeout 1
			endi
						
			NPAR=FPAR
			dnp=np
			acti wind P1
			@ 1,2 say [Pesa #1 .........]   
			@ 2,2 say [Pesa #2 .........]
			@ 3,2 say [Pesa #3 .........]
			@ 4,2 say [Total ...........]
			@ 6,2 say [% Grasa .........] 
			@ 7,2 say [% Proteกna.......]
			@ 9,2 say [Prod. Ant........]
			@10,2 say [Dias/Leche ......]
			@11,2 say [Corral Num.......]

DO CASE
*-------------------------------------------------------------------*
CASE NPES=1
@ 1,24 GET P1 PICT '99.9' FUNC 'J' VALID P1>=0 and P1<=100 ERROR [ Rango de 0 a 100 ] COLO SCHE 19
@ 2,24 SAY P2  PICT '99.9' FUNC 'J' colo sche 18
@ 3,24 SAY P3  PICT '99.9' FUNC 'J' colo sche 18 
@ 4,23 SAY P1+P2+P3 PICT '#99.9' FUNC 'J'  colo sche 18
@ 6,24 get MGRA pict '#.##' valid MGRA>=0 and MGRA<=8 error tit(2) when pw1="S"
@ 7,24 get MPRO pict '#.##' valid MPRO>=0 and MPRO<=8 error tit(2) when pw1="S"

@ 9,24 say prm pict '99.9' colo sche 18
@10,24 SAY DATE()-FPAR PICT '9999' FUNC 'J' colo sche 18
@11,26 SAY CORR PICT '##' FUNC 'J' colo sche 18   

CASE NPES=2
@ 1,24 say P1 PICT '99.9' FUNC 'J' colo sche 18
@ 2,24 get P2  PICT '99.9' FUNC 'J'  VALID P2>=0 and P2<=100 ERROR [ Rango de 0 a 100 ] COLO SCHE 19
@ 3,24 SAY P3  PICT '99.9' FUNC 'J'  colo sche 18
@ 4,23 SAY P1+P2+P3 PICT '#99.9' FUNC 'J' colo sche 18
@ 6,24 get MGRA pict '#.##' valid MGRA>=0 and MGRA<=8 error tit(2) when pw1="S"
@ 7,24 get MPRO pict '#.##' valid MPRO>=0 and MPRO<=8 error tit(2) when pw1="S"

@ 9,24 say prm pict '99.9' colo sche 18
@10,24 SAY DATE()-FPAR PICT '9999' FUNC 'J' colo sche 18
@11,26 SAY CORR PICT '##' FUNC 'J' colo sche 18   

CASE NPES=3
@ 1,24 say P1 PICT '99.9' FUNC 'J' colo sche 18
@ 2,24 SAY P2  PICT '99.9' FUNC 'J'  colo sche 18
@ 3,24 get P3  PICT '99.9' FUNC 'J' VALID P3>=0 and P3<=100 ERROR [ Rango de 0 a 100 ] COLO SCHE 19 
@ 4,23 SAY P1+P2+P3 PICT '#99.9' FUNC 'J' colo sche 18 
@ 6,24 get MGRA pict '#.##' valid MGRA>=0 and MGRA<=8 error tit(2) when pw1="S"
@ 7,24 get MPRO pict '#.##' valid MPRO>=0 and MPRO<=8 error tit(2) when pw1="S"

@ 9,24 say prm pict '99.9' colo sche 18
@10,24 SAY DATE()-FPAR PICT '9999' FUNC 'J' colo sche 18
@11,26 SAY CORR PICT '##' FUNC 'J' colo sche 18   

CASE NPES=4
@ 1,24 get P1 PICT '99.9' FUNC 'J' VALID P1>=0 and P1<=100 ERROR [ Rango de 0 a 100 ] COLO SCHE 19
@ 2,24 get P2  PICT '99.9' FUNC 'J' VALID P2>=0 and P2<=100 ERROR [ Rango de 0 a 100 ] COLO SCHE 19
@ 3,24 get P3  PICT '99.9' FUNC 'J' VALID P3>=0 and P3<=100 ERROR [ Rango de 0 a 100 ] COLO SCHE 19
@ 4,23 SAY P1+P2+P3 PICT '#99.9' FUNC 'J' colo sche 18
@ 6,24 get MGRA pict '#.##' valid MGRA>=0 and MGRA<=8 error tit(2) when pw1="S"
@ 7,24 get MPRO pict '#.##' valid MPRO>=0 and MPRO<=8 error tit(2) when pw1="S"

@ 9,24 say prm pict '99.9' colo sche 18
@10,24 SAY DATE()-FPAR PICT '9999' FUNC 'J' colo sche 18
@11,26 SAY CORR PICT '##' FUNC 'J' colo sche 18   

ENDCASE
READ
IF LASTKEY()=27
ELSE
REPL TOT WITH P1+P2+P3

** Cambio de corral por el de captura
** ----------------------------------	

	if pw2="S"
		if corr#val(mco)
			REPL CORA WITH CORR,CORR WITH VAL(MCO)
		else
			REPL CORA WITH VAL(MCO),CORR WITH VAL(MCO)
		endi		
	endi
** ----------------------------------	

ENDI
DEACT WIND P1

ENDCASE &&
ENDI
XID2=XID
ENDD
@22,1 clea to 22,24
ENDD
clos all
rele wind P1
@4,0 clea to 24,79
@23,0 to 23,79
return

*   ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*   ณ  BORRAR DATOS DE LA PESADA  ณ
*   ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
PROC BORRAR
sn='N'
xturno="0"

@ 4,0 CLEA TO 24,79
@23,0 to 23,79
@ 8,4 say [NOTA :] COLO W+/&CZ
@10,4 FILL TO 17,77 COLO N/N
@ 9,3 fill to 16,76 COLO SCHE 15
@10,4 say [ Este programa borrara los datos de la tarjeta de registro de ordeคas,  ] COLO SCHE 15
@11,4 say [ y se debera utilizar en caso de que existan errores en la captura de   ] COLO SCHE 15
@12,4 say [ datos del dia de la pesada, y se quiera empezar de nuevo.              ] COLO SCHE 15
@13,4 say [ Se podran borrar tanto los datos de un turno o pesada, asi como los de ] COLO SCHE 15
@14,4 say [ Grasa o Proteกna respectivamente.                                      ] COLO SCHE 15
@15,4 say [ Se podra seleccionar la opcion con la barra espaciadora.               ] COLO SCHE 15

@22,28 SAY [PESADA DE LECHE - BORRAR] colo gr+/&cz
@24,70 SAY [Esc] colo n/w
@24,74 say [Salir] colo w+/&cz

@24,1 say [Pesada o Turno a Borrar ] get xturno pict'@M 0,1,2,3,G,P,T' error tit(2) colo w+/&cz,n/w
read
	if lastkey()=27 or xturno="0"
	else
		use REG order 2
		SET FILT TO NP>0
		@22,1 SAY [ID] COLO W+/&CZ
		GO TOP
	scan
		@ 22,4 SAY ID colo n/w
		do case
			case xturno="1"
				repl P1 with 0,TOT with P1+P2+P3
			case xturno="2"
				repl P2 with 0,TOT with P1+P2+P3
			case xturno="3"
				repl P3 with 0,TOT with P1+P2+P3
			case xturno="G"
				repl MGRA with 0
			case xturno="P"
				repl MPRO with 0
			case xturno="T"
				repl P1 with 0,P2 with 0,P3 with 0,TOT with 0,;
				MGRA with 0,MPRO with 0
		endcase	
	endscan
endi
@4,0 clea to 24,79
@23,0 to 23,79
return

*   ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*   ณ  RESUMEN DE PESADA DE LECHE ณ
*   ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
PROC RESUMEN
@23,0 to 23,79
@10,4 say [NOTA :] COLO W+/&CZ
@12,4 FILL TO 17,77 COLO N/N
@11,3 fill to 16,76 COLO SCHE 15
@12,4 say [ Este programa analizara los datos de pesada de las ordeคas para que    ] COLO SCHE 15
@13,4 say [ para aquellas vacas que cuenten con datos irregulares o que les falte  ] COLO SCHE 15
@14,4 say [ una o mas pesadas, asi como las que se encuentren (E)nfermas sean      ] colo sche 15
@15,4 say [ corregidas o estimadas antes de utilizar la opcion ACTUALIZAR.         ] COLO SCHE 15
@15,56 SAY [ACTUALIZAR.] colo sche 13

@22,28 SAY [PESADA DE LECHE - RESUMEN] colo gr+/&cz
@24,70 say [Esc] colo n/w
@24,74 say [Salir] colo w+/&cz

NO=space(1)
dif=50

@22,1 say [Ordeคas] get NO PICT '@M 2,3' colo w+/&cz
@22,12 say [Dif.>] get DIF PICT '##' colo w+/&cz
@22,21 say "%" colo w+/&cz
@24,1 say [Total] colo w+/&cz
read 

if lastkey()=27 
retu
else
@22,9 say NO pict'#' 
@22,18 say DIF pict'##' 

use REG order 2
@4,0 clea to 21,78
@21,0 to 21,79
@ 4,1 say [               Pesa  Pesa  Pesa                      Pesa  Pesa  Pesa         ] colo n/w
@ 5,1 say [     ID Corral   1     2     3  Total      ID Corral   1     2     3  Total   ] colo n/w
row=7
col=4
dat=0
dif=dif/100
use REG order 2
set filt to np>0 and fsec=B and fb2=B 
go top

if NO="2"
 xx="P1<P2-(P2*dif) or P2<P1-(P1*dif); 
  or P1=0 and P2=0"
else
 xx="P1<P2-(P2*dif) or P1<P3-(P3*dif);
 or  P2<P1-(P1*dif) or P2<P3-(P3*dif);
 or  P3<P1-(P1*dif) or P3<P2-(P2*dif);
 or P1=0 and P2=0 and P3=0"
endi

SCAN while not eof() for &xx 
	@24,7 say dat+1 pict '####'
	@row,col-1 say ID colo gr+/&cz
	@row,col+7 say CORR
	@row,col+11 say P1
	@row,col+17 say P2

	if NO="3"
    	@row,col+23 SAY P3 
	endi

	if hosp=1
		@ row,col+5 say [E] colo w+/&cz
	endi
	
	@row,col+29 SAY TOT colo r+/&cz

if row=20 and col=42
	@ 24,70 clea to 24,79
	@ 24,70 say [Enter] colo n/w
	@ 24,75 say [ ] colo w+/&cz
	@ 24,76 say chr(16) colo w+/&cz
		do while inkey()#13
			if lastkey()=6
				@ 4,0 clea to 20,78
				go bottom
				exit
			endi
		endd
	@ 6,2 clea to 20,79
	@ 24,58 clea to 24,79
	row=6
	col=4
endi

if row=20
	row=6
	col=col+38
endi
	dat=dat+1
	row=row+1
ENDSCAN

if dat=0
	@ 24,9 SAY [0]
endi
	??chr(7)
	@ 24,67 clea to 24,79
	@ 24,70 say [Enter] colo n/w
	@ 24,75 say [ ] colo w+/&cz
	@ 24,76 say chr(4) colo w+/&cz
do while inkey()#13
endd

IF DAT=0
retu
else
SN='N'
@ 24,67 clea to 24,79
@ 24,67 SAY [Imprimir] GET SN PICT '@M S,N' colo n/w
read valid lastkey()=13
if SN='N' &&or LASTKEY()=27
else
if SN='S' and SYS(13)='READY'
	*ษอออออออออออออออออออออออออออป
	*  Cambio a Letra 12 cpi
		???&q19
	*ศอออออออออออออออออออออออออออผ
	set console off
    set printer on
NVE=0
PAG=1
NVAC=0
NTIT=[VACAS CON PESADA IRREGULAR]
GO TOP
DO WHILE .NOT. EOF()
DO CTRLVRP0
?'     ID     CORRAL   FECHA            ESTADO    ULTIMA    ULT.   HOSP ENFER  PESA   PESA   PESA'    
?'    VACA     NUM.    PARTO      DEL   REPROD    PESADA    PROD         NUM     1      2      3 '
?'  '+REPLI(CHR(45),94)

SCAN while not eof() for &xx 
REPL DPR WITH DATE()-FPAR
IF HOSP=1
MHOSP='SI'
MENF=STR(ENF,2)
ELSE
MHOSP='  '
MENF='  '
ENDI

?SPACE(3)+STR(ID,5)+'     '+STR(CORR,2)+'     '+fx(FPAR)+'    '+STR(DPR,3);
+'    '+STAT+'   '+fx(PESA)+'   '+STR(PRM,4,1)+'    '+MHOSP+'   '+MENF+'    '+STR(P1,4,1)+'   '+STR(P2,4,1)+'   '+STR(P3,4,1)
NVE=NVE+1
nvac=nvac+1

IF NVE=WLI
?'   '+REPLI(CHR(254),94)
EJEC
PAG=PAG+1
NVE=0
EXIT
ENDI

ENDS
ENDD
*NVAC=NVAC
?'  '+REPLI(CHR(254),94)
?
?'   TOTAL = '+STR(NVAC,4)
EJEC
SET PRIN OFF
SET CONS ON
@ 4,0 clea to 20,79
*@ 22,1 clea to 23,78

ELSE
??CHR(7)
WAIT WIND [ IMPRESORA APAGADA O DESCONECTADA ] TIMEOUT 2
@ 4,0 CLEA TO 20,79
*@ 22,1 CLEA TO 23,78
ENDI
ENDI
ENDI
endi
CLOS DATA
return

*   ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*   ณ  LISTADO DE PESADA DE LECHE ณ
*   ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
PROC LISTA
tprd=0
row=7
col=4
dat=0
tpr=0

@ 9,4 say [NOTA :] COLO W+/&CZ
@ 11,4 FILL TO 18,77 COLO N/N
@ 10,3 fill to 17,76 COLO SCHE 15
@ 11,4 say [ Este programa emitira un lista de todas las vacas que se encuentren en ] COLO SCHE 15
@ 12,4 say [ produccion, mostrando los datos de registro de 2 o 3 ordeคas y el total] COLO SCHE 15
@ 13,4 say [ de cada una, asi como calculando al final el total de leche producida. ] COLO SCHE 15
@ 14,4 say [ Las vacas que al dia de pesada cuenten con 15 o menos dias en leche,   ] colo sche 15
@ 15,4 say [ seran consideradas como (F)rescas y su proyeccion a 305 dias y 305 EM  ] colo sche 15
@ 16,4 say [ asi como el calculo de ERPA no seran efectuados.                       ] colo sche 15

@22,28 say [PESADA DE LECHE - LISTADO] colo gr+/&cz
@23,0 to 23,79
@24,1 say [Total] colo w+/&cz
@24,13 SAY [Producciขn] get tprd pict '######' colo w+/&cz,n/w
@24,32 say [Calculada] colo w+/&cz
@24,52 say [Dif %] colo w+/&cz
@24,70 say [Esc] colo n/w
@24,74 say [Salir] 
read
if lastkey()=27
 return
endi
@ 4,0 clea to 20,78
@21,0 to 21,79

@ 4,1 say [               Pesa  Pesa  Pesa                      Pesa  Pesa  Pesa         ] colo n/w
@ 5,1 say [     ID Corral   1     2     3  Total      ID Corral   1     2     3  Total   ] colo n/w
use REG order 2

*Filtro solo para Pruebas
*set filt to np>0 and fsec=B and fb2=B or np>0 and fb2=B and fsec#B and fsec>HOY
*------------------------

** Listado de Vacas que cuenten con pesada
** ---------------------------------------
set filt to np>0 and TOT>0 and fb2=B
go top

SCAN while inkey()#6 
	tpr=tpr+tot
	@24,7 say dat+1 pict '####'
	@24,43 say tpr pict'######' colo w+/w

	@row,col-1 say ID colo gr+/&cz
		if hoy-fpar<=15
			@ row,col+5 say [F] colo w+/&cz
		endi
	@row,col+7 say CORR
	@row,col+11 say P1 
	@row,col+17 say P2 
	@row,col+23 SAY P3 
	@row,col+29 SAY TOT colo r+/&cz

if row=20 and col=42
	@24,68 clea to 24,79
	@ 24,70 say [Enter] colo n/w
	@ 24,75 say [ ] colo w+/&cz
	@ 24,76 say chr(16) colo w+/&cz
		do while inkey()#13
			if lastkey()=6
				@ 4,1 clea to 20,78
				go bottom
				exit
			endi
		endd
	@ 6,2 clea to 20,77
*	@ 23,58 clea to 23,78
	row=6
	col=4
endi

if row=20
	row=6
	col=col+38
endi
	dat=dat+1
	row=row+1
ENDSCAN
@24,59 say str(100-(tprd*100)/tpr,5,1) colo w+/r
*@23,22 say str(tprd/tpr,5,1)
if dat=0
	@ 24,7 SAY [0]
endi
	??chr(7)
	@24,70 clea to 24,79
	@ 24,70 say [Enter] colo n/w
	@ 24,75 say [ ] colo w+/&cz
	@ 24,76 say chr(4) colo w+/&cz
do while inkey()#13
endd

IF dat=0
else
	sn='N'
	@ 24,67 say [Imprimir] get sn pict '@M S,N' colo n/w 
read
if sn='N' or lastkey()=27
else
if sn='S' and SYS(13)='READY'
	*ษอออออออออออออออออออออออออออป
	*  Cambio a Letra 12 cpi
		???&q19
	*ศอออออออออออออออออออออออออออผ
	set console off
    set printer on
	NTIT=[LISTADO DE ORDEฅAS]
	NTIT2=dtoc(hoy)
	@ 24,67 clea to 24,79
	@ 24,67 SAY [Espere] colo n*/w

	COPY TO LISTA FIELDS ID,CORR,P1,P2,P3,TOT,fpar
	clos all
	sele 2
		use CAMBIO
	sele 1
		use LISTA
	@ 24,67 clea to 24,79
		go top
	DO R8 WITH 2,(WLI),40,str(ID,5)+str(corr,3)+str(P1,5,1)+str(P2,5,1)+str(P3,5,1)+str(TOT,5,1)
		set print off
else
		??CHR(7)
		wait wind [ IMPRESORA APAGADA O DESCONECTADA ] timeout 1
endi
endi
ENDI
return

*  ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*  ณ  PROGRAMA PARA ACTUALIZAR DATOS DE LA PESADA  ณ
*  ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
PROC ACTUALIZAR
factor=1
sn="N"
ON KEY LABEL F10 DO PESADAS
@ 9,4 say [NOTA :] COLO W+/&CZ
@ 11,4 FILL TO 17,77 COLO N/N
@ 10,3 fill to 16,76 COLO SCHE 15
@ 11,4 say [ Este programa actualizara las tarjetas de Registro y Produccion de cada] COLO SCHE 15
@ 12,4 say [ vaca cuya suma total sea mayor de 0. Todas aquellas vacas que al dia de] colo sche 15
@ 13,4 say [ pesada cuenten con 15 o menos dias en leche, seran consideradas como   ] colo sche 15
@ 14,4 say [ (F)rescas y su proyeccion a 305 dias y 305 EM, asi como el calculo de  ] colo sche 15
@ 15,4 say [ ERPA no seran efectuados.  Ver opcion RESUMEN.                         ]  colo sche 15
@ 15,43 SAY [RESUMEN Y LISTADO.] COLO SCHE 15

@ 22,28 SAY [PESADA DE LECHE - ACTUALIZAR] colo gr+/&cz
@23,0 to 23,79
@24,1 say [F1] COLO N/W
@24,4 SAY [Ayuda] 
@24,11 say [F10] colo n/w
@24,15 say [Fechas]
@24,70 SAY [Esc] colo n/w
@24,74 say [Salir] 

*---Verifica la fecha ----------------------------------------------------*
    @ 24,35 GET HOY VALID HOY>=DATE()-180 ;
    .AND. HOY<=DATE() ERROR tit(3) 
    read timeout 30
    if lastkey()=27
      clos data
      return
    endi
    @ 24,35 SAY HOY COLO W+/&CZ
    USE PESADAS
    SN='N'
    GO BOTTOM
    if FPES>HOY
	   ??CHR(7)
	   wait wind tit(3) timeout 1
	   do PESADAS
	   return
	endi
*--------------------------------------------------------------------------*
@24,50 say [Factor ] get factor pict '#.#' valid factor=1 or factor=2.2 error [ Kilos=1.0  Libras=2.2 ] colo w+/&cz,n/w
@24,68 say [Continuar] get SN PICT '@M N,S' colo n/w
READ
if lastkey()=27 or SN='N'
 return
 else
@ 24,67 clea to 24,79
@ 24,70 SAY [Espere] colo n*/w
@ 4,0 clea to 20,79

@ 19,2 CLEA TO 19,20
@ 22,1 SAY [ID] colo w+/&cz
SELE 1
use REG order 2
SELE 2
USE PROD order 1
SELE 1

*set filt to fpar<HOY and TOT>0 and fb2=B
set filt to fpar<HOY and (P1+P2+P3)>0 and FB2=B
GO TOP
XID=0
CVAC=0

** Selecciona a la vacas que estuvieron en produccion el dia de la pesada ;
** y que hayan sido pesadas (TOT>0) y que no esten Actualizadas.

SCAN WHILE not eof() and pesa<HOY for fsec=B ;
		   or fsec#B and fsec>HOY  

@ 22,5 SAY ID PICT '#####' colo n/w

IF PESA=B
NPESA=FPAR
ELSE
NPESA=PESA
ENDI
XID=ID
NFPAR=FPAR
NPAC=PAC
NPRM=(P1+P2+P3)/factor

** Actualiza los datos en Reg.DBF
** ------------------------------
REPL TOTA WITH PRM,;
     APES with PESA,;
     PESA WITH HOY,;
     PRM WITH (P1+P2+P3)/factor,;
     DPR WITH HOY-FPAR

** Para anotar el pico de produccion
** ---------------------------------
if (hoy-fpar)>=15 and (hoy-fpar)<=120 and PLAC<PRM
repl PLAC with PRM,DPIC with (hoy-fpar)
endi

** Para anotar los datos de grasa y proteina
** -----------------------------------------
repl APRO with PPRO,;
	 PPRO with MPRO,;
	 MPRO with 0,;
     AGRA with PGRA,;
     PGRA with MGRA,;
     MGRA with 0

** Selecciona el Archivo de Produccion
** para ver la ultima pesada.
** -----------------------------------
SELE 2
seek xid

IF .NOT. FOUND()
	JPRD=NPRM
ELSE
scan while id=xid 

IF FP>NFPAR
	JPRD=PRD
ELSE
	JPRD=NPRM
ENDI
ends
ENDI

** Agrega datos en la tarjeta de Produccion.
** --------------------------------------
APPE BLANK
REPL ID WITH A->ID,;
	 NP WITH A->NP,;
	 FP WITH HOY,;
	 PRD WITH (A->P1+A->P2+A->P3)/factor,;
	 DIAS WITH HOY-NPESA,;
	 TOM WITH (PRD+JPRD)/2*DIAS,;
	 TOL WITH TOM+NPAC

** Datos de Grasa y Proteina
** -------------------------
repl PGR with a->PGRA,;
	 PPR with a->PPRO

RPAC=TOL
NPRX=TOL/(HOY-NFPAR)

** Reemplaza la produccion ajustada y el promedio acumulado en Reg.DBF
** -------------------------------------------------------------------
SELE 1
SEEK XID
REPL PAC WITH RPAC,;
	 PRX WITH NPRX,;
	 TLAC with PAC

** Para calcular los Valores de 305 d y 305 EM C\Vaca
** --------------------------------------------------

if (hoy-fpar)>15

** Reemplaza el valor de PAC
** -------------------------
Qpac=PAC

IF EDAD<=36
	DO 305
ELSE
IF EDAD>36
	DO 305m
ENDI
ENDI

IF P305>0
	REPL M305 WITH P305*FAC
ELSE
	REPL M305 WITH 0
ENDI
else
endi

** Calculo de ERPA
** ----------------------------------------
if M305>0
	mfac=0.5
	mnreg=nreg+1
	mfac=(mnreg*mfac)/(1+((mnreg-1)*mfac))

repl ERPA with mfac*(((LIFEL+M305)/mnreg)-x305)						
endi


** Termina calculos de pesada en cada vaca
** ---------------------------------------
CVAC=CVAC+1
ENDS

@ 19,1 CLEA TO 19,78
SET FILT TO

IF CVAC>0
	** Pone en cero los datos de la tarjeta de Ordeคas
	** -----------------------------------------------
	REPL ALL P1 WITH 0,;
		     P2 WITH 0,;
		     P3 WITH 0,;
		     TOT WITH 0
	** Pone en el Archivo de Pesadas la fecha nueva
	** --------------------------------------------
	USE PESADAS
	APPE BLANK
	REPL FPES WITH HOY,NVAC with CVAC
	CLOS DATA
	ELSE
	CLOS DATA
ENDI
CVAC=0
@ 23,1 CLEA TO 23,78
endi


*   ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*   ณ   EXPORTAR DATOS A PESA.TXT   ณ
*   ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
PROC PEXPO
sn="N"
set decimals to 1
	@23,0 to 23,79
	@9,4 say [NOTA :] COLO W+/&CZ
	@11,4 fill to 19,77 colo n/n
	@10,3 fill to 18,76 COLO SCHE 15	
	@11,4 say [ Este programa crear  dos archivos llamados PESAS.TXT y PESAS.DBF para   ] colo SCHE 15
	@12,4 say [ ser usados en computadoras de mano PSION, PALM etc.                     ] COLO SCHE 15
	@14,4 say [ Podr n ser utilizados para captura de informaciขn de la pesada de leche ] colo sche 15
	@15,4 say [ y posteriormente trasladar los datos mediante la opciขn IMPORTAR.       ] COLO SCHE 15
	@17,4 say [ El formato ser  (Identificaciขn, Pesa 1, Pesa 2 y Pesa 3.)              ] COLO SCHE 15

	@22,27 SAY [PESADA DE LECHE - EXPORTAR] colo gr+/&cz
	@24,68 say [Continuar] get sn pict '@M N,S' COLO n/w
read

if lastkey()=27 or sn="N" 
	retu
else
	@24,67 clea to 24,79
	@24,70 say [Espere] colo n*/w
	use REG order 2
	copy to PESAS.TXT fields ID,P1,P2,P3 for np>0 and fsec=b and fb2=b delimited
	copy to PESAS.DBF fields ID,P1,P2,P3 for np>0 and fsec=b and fb2=b 

endi
clos data
@4,0 clea to 24,79
@23,0 to 23,79
return	

*   ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*   ณ   IMPORTAR DATOS DE PESAS.TXT    ณ
*   ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
PROC PIMPO
sn="N"
set decimals to 1
	@23,0 to 23,79
	@9,4 say [NOTA :] COLO W+/&CZ
	@11,4 fill to 17,77 colo n/n
	@10,3 fill to 16,76 COLO SCHE 15	
	@11,4 say [ Este programa importara los datos de las pesadas de leche de un archivo] COLO SCHE 15
	@12,4 say [ externo, y este debera de tener el nombre de PESAS.TXT, delimitado por ] COLO SCHE 15
	@13,4 say [ comas y los campos deberan tener el siguiente orden y estructura :     ] COLO SCHE 15
	@14,4 say [ Identificacion (Max. 5 caracts.), y las pesadas (2/3) en formato ##.#  ] COLO SCHE 15
	@15,4 say [          Ejemplos : 4250,12.5,10.5,8.0  ข  12,10.5,10.0,9.0            ] COLO SCHE 15

	@22,25 SAY [PESADA DE LECHE - IMPORTAR TXT] colo gr+/&cz
	@24,68 say [Continuar] get sn pict '@M N,S' COLO n/w
read

if lastkey()=27 or sn="N" 
	retu
else
	if not file("PESAS.TXT")
		wait wind [ Archivo no existente ] timeout 2
	else
		create table apesa (ID n(5),P1 n(4,1),P2 n(4,1),P3 n(4,1))
		appe from pesas.txt deli 
		clos data

use apesa in 1
use REG order 2 in 2
set relation to id into reg
sele 1
go top
@22,1 say [ID] colo w+/b
scan
	@22,4 say apesa.ID colo n/w
	if apesa.ID=reg.ID &&and apesa.P1>0
		repl reg.P1 with apesa.P1,reg.P2 with apesa.P2,reg.P3 with apesa.P3,;
		reg.TOT with apesa.P1+apesa.P2+apesa.P3 
	endi
endscan
	endi
endi
clos all
eras apesa.dbf
eras pesas.txt
@4,0 clea to 24,79
@23,0 to 23,79
return

*   ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*   ณ     IMPORTAR DATOS DE PESAS.DBF  ณ
*   ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
PROC PIMPO2
sn="N"
set decimals to 1
	@23,0 to 23,79
	@9,4 say [NOTA :] COLO W+/&CZ
	@11,4 fill to 17,77 colo n/n
	@10,3 fill to 16,76 COLO SCHE 15	
	@11,4 say [ Este programa importara los datos de las pesadas de leche de un archivo] COLO SCHE 15
	@12,4 say [ externo, y este debera de tener el nombre de PESAS.DBF.                ] COLO SCHE 15
	@13,4 say [ Los campos deberan tener el siguiente orden y estructura :             ] COLO SCHE 15
	@14,4 say [ Identificacion (Max. 5 caracts.), y las pesadas (2/3) en formato ##.#  ] COLO SCHE 15
	@15,4 say [ Ejemplo : 00000 00.0 00.0 00.0                                         ] COLO SCHE 15

	@22,25 SAY [PESADA DE LECHE - IMPORTAR DBF] colo gr+/&cz
	@24,68 say [Continuar] get sn pict '@M N,S' COLO n/w
read

if lastkey()=27 or sn="N" 
	retu
else
	if not file("PESAS.DBF")
		wait wind [ Archivo no existente ] timeout 2
	else

use pesas in 1 alias apesa
use REG order 2 in 2
set relation to id into reg
sele 1
go top
@22,1 say [ID] colo w+/b
scan
	@22,4 say apesa.ID colo n/w
	if apesa.ID=reg.ID &&and apesa.P1>0
		repl reg.P1 with apesa.P1,reg.P2 with apesa.P2,reg.P3 with apesa.P3,;
		reg.TOT with apesa.P1+apesa.P2+apesa.P3 
	endi
endscan
	endi
endi
clos all
eras pesas.dbf
@4,0 clea to 24,79
@23,0 to 23,79
return

*   ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*   ณ   DIRECTORIOS  ณ
*   ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

PROC FICHA
ON KEY LABEL F10 *
IF XID>0
USE PROD order 1 
SEEK XID
DEFI WIND PROD FROM 5,13 TO 19,66 PANEL TITLE [ FICHA DE PRODUCCION ] SHADOW colo sche 16
ACTI WIND PROD
IF .NOT. FOUND()
?CHR(7)
@ 6,17 SAY [No tiene Registros] COLO N/W
ELSE
@ 1,2 SAY [Parto] colo N/W
@ 1,8 say dnp pict '99' colo w+/&cz/w  
@ 1,12 SAY NPAR COLO N/W
@ 2,2 say [   Pesada    Dias   Prod   Tot/mes  Acum/Lact.] colo w+/&cz/W
R=3
MNP=NP
SCAN WHILE ID=XID 
if np=dnp
@ 1,8 say np colo w+/&cz/W
@ R,4 SAY FP COLO N/W
@ R,15 SAY DIAS COLO N/W

@ r,22 say prd pict '#9.9' func 't' COLO N/W
@ r,31 say tom pict '#999' func 't' COLO N/W
@ R,40 SAY TOL PICT '99999' FUNC 'T' COLO N/W
r=r+1
endi
if r>10
@ 12,44 say [Enter] 
@ 12,50 say chr(16) 
do while inkey() # 13
endd
@ 3,0 clea to 12,52
r=3
endi
ENDSCAN
ENDI
?chr(7)
@ 12,44 say [Enter]
@ 12,50 say chr(4)
do while inkey() # 13
endd
DEACT WIND PROD
acti screen
CLOS DATA
use REG order 2
SEEK XID
ON KEY LABEL F10 DO FICHA
ELSE
ON KEY LABEL F10 DO FICHA
RETU
ENDI


*  ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*  ณ  VACAS CAMBIADAS DE CORRAL  ณ
*  ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
PROC CAMBIADAS
SN='N'
@21,0 to 21,79
@ 22,27 SAY [VACAS CAMBIADAS DE CORRAL] colo gr+/&cz
@ 23,1 to 23,79
use REG order 2
@ 24,1 say [Total] colo w+/&cz
@ 4,1 say [                                                                              ] colo n/w
@ 5,1 say [      ID    Lact DEL   Prod  Del  Al        ID    Lact DEL   Prod  Del  Al    ] colo n/w
ROW=7
COL=4
DAT=0
GO TOP
SCAN FOR NP>0 AND FB2=B AND FSEC=B AND CORA#CORR WHILE INKEY()#6
@ 24,7 SAY DAT+1 PICT '####'
@ 24,70 SAY [Espere] colo n*/w
@ ROW,COL SAY ID  colo gr+/&cz
@ row,col+7 say left(stat,1) colo w+/&cz
@ ROW,COL+10 SAY np 
@ ROW,COL+14 SAY DATE()-FPAR PICT '999'
@ ROW,COL+20 SAY PRM PICT '99.9' colo r+/&cz
@ ROW,COL+26 SAY CORA PICT '999'
@ ROW,COL+30 SAY CORR PICT '999'


IF ROW=20 and COL=42  
	@ 24,70 SAY [Enter] COLO N/W
	@ 24,75 SAY [ ] COLO W+/&CZ
	@ 24,76 SAY CHR(16) colo w+/&cz
DO WHILE INKEY() # 13
IF LASTKEY()=6
@ 4,0 CLEA TO 20,79
GO BOTTOM
EXIT
ENDI
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
	@ 24,70 SAY [Enter] COLO N/W
	@ 24,75 SAY [ ] COLO W+/&CZ
	@ 24,76 SAY CHR(4) colo w+/&cz
DO WHILE INKEY() # 13
ENDD

sn='N'
@ 24,67 SAY [Imprimir] GET SN PICT '@M S,N' colo n/w 
READ
IF SN='N' or LASTKEY()=27
ELSE
IF SN='S' and SYS(13)='READY'
NTIT=[VACAS CAMBIADAS DE CORRAL]
NTIT2=DTOC(HOY)
@ 24,67 clea to 24,79
@ 24,67 SAY [Espere] colo n*/w
use REG order 2
REPL ALL DPR WITH (DATE()-FPAR)-(DATE()-FSEC) FOR FPAR<>B and FB2=B
GO TOP
COPY TO LOTES2 FIELDS ID,DPR,PRM,STAT,CORA,CORR FOR NP>0 AND FB2=B AND FSEC=B AND CORA#CORR
CLOS ALL
SELE 1
USE LOTES2
INDE ON STR(CORA)+STR(ID) TO LOTES2
USE LOTES2 INDE LOTES2
@ 24,67 clea to 24,79
DO RCAM WITH 2,(WLI),34,STR(ID,5)+STR(DPR,4)+STR(PRM,4,1)+STAT+STR(CORA,3)+STR(CORR,3)
SET PRIN OFF
ELSE
??CHR(7)
WAIT WIND [ IMPRESORA APAGADA O DESCONECTADA ] TIMEOUT 2
ENDI
ENDI
*  ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*  ณ  REPORTE DE VACAS CAMBIADAS DE CORRAL  ณ
*  ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
PROCEDURE RCAM
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
*ษอออออออออออออออออออออออออออป
*  Cambio a Letra 12 cpi
	???&q19
*ศอออออออออออออออออออออออออออผ
DO WHILE PRINTED<RECCOUNT() &&
?'  Rancho   : '+Q36+'   '+nom 
?'  Area     : VIENTRES'
?'  Reporte  : '+NTIT
?'  Fecha    : '+DTOC(DATE())+'  Hora : '+TIME()
?'  Pagina   : '+STR(PGCOUNT,2,0)+space(70)+[LACTOFOX]
?'  Periodo  : '+NTIT2
?'  '+REPLI(CHR(254),94)
?'      ID   DEL  Prod  Status  Del  Al                ID   DEL  Prod  Status  Del  Al'
?'  '+REPLI('-',94)
ONTHISPAGE=MIN(ROWS*COLS,RECCOUNT()-PRINTED)
ROWS=MIN(ROWS,(ONTHISPAGE/COLS))
THISROW=1
DO WHILE THISROW<=ROWS and PRINTED<=RECCOUNT()
THISCOL=1
if id>0
?space(2),LEFT(STR(ID,5,0)+'  '+STR(DPR,4)+'  '+STR(PRM,4,1)+'  '+STAT+'   '+STR(CORA,3)+' '+STR(CORR,3),COLW)+SPACE(MAX(0,COLW-LEN(FIELD)))
NVAC=NVAC+1
endi
PRINTED=PRINTED+1
DO WHILE THISCOL<COLS and PRINTED<=RECCOUNT()
SKIP ROWS
if id>0
??space(2),LEFT(STR(ID,5,0)+'  '+STR(DPR,4)+'  '+STR(PRM,4,1)+'  '+STAT+'   '+STR(CORA,3)+' '+STR(CORR,3),COLW)+SPACE(MAX(0,COLW-LEN(FIELD)))
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
ERAS LOTES2.IDX

*  ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*  ณ  REPORTE DE LISTA DE ORDEฅAS  ณ
*  ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
PROCEDURE R8
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
?'                 Pesa   Pesa  Pesa                            Pesa   Pesa  Pesa'
?'      ID Corral    1      2     3  Total           ID Corral    1      2     3  Total'    
?'  '+REPLI('-',94)
ONTHISPAGE=MIN(ROWS*COLS,RECCOUNT()-PRINTED)
ROWS=MIN(ROWS,(ONTHISPAGE/COLS))
THISROW=1
DO WHILE THISROW<=ROWS and PRINTED<=RECCOUNT()
THISCOL=1
if id>0
	wfre=[ ]
	if (hoy-fpar)<=15
		wfre=[F]
	endi	

?space(2),LEFT(STR(ID,5,0)+' '+wfre+' '+STR(CORR,3)+'  '+STR(P1,5,1)+'  '+STR(P2,5,1)+' '+STR(P3,5,1)+' '+STR(TOT,5,1),COLW)+SPACE(5)&&SPACE(MAX(0,COLW-LEN(FIELD)))

NVAC=NVAC+1
endi
PRINTED=PRINTED+1
DO WHILE THISCOL<COLS and PRINTED<=RECCOUNT()
SKIP ROWS
if id>0
	wfre=[ ]
	if (hoy-fpar)<=15
		wfre=[F]
	endi	
??space(2),LEFT(STR(ID,5,0)+' '+wfre+' '+STR(CORR,3)+'  '+STR(P1,5,1)+'  '+STR(P2,5,1)+' '+STR(P3,5,1)+' '+STR(TOT,5,1),COLW)+SPACE(5)&&SPACE(MAX(0,COLW-LEN(FIELD)))
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
?
?'  '+REPLI(CHR(254),94)
?'   VACAS = '+STR(NVAC,4)+'    PROD. CALCULADA (Kgs.) = '+str(tpr,6)
?'                   PROD. REAL (Kgs.)      = '+str(tprd,6)
?'                   DIFERENCIA ( % )       =  '+str(100-(tprd*100)/tpr,5,1)
ENDI
ENDD
EJECT
PGCOUNT=PGCOUNT+1
SKIP ((COLS-1)*ROWS)
ENDD
CLOS DATA
ERAS lista.DBF


** Checa el corral en la Pesada
** Esta  debe de ir Aqui.
** ----------------------------
** Checa el corral
** ---------------
FUNCTION CHCORRP
	parameter MCOR
	xbase=alias()
	sele 0
	USE CORRAL order 1 AGAIN
	seek MCOR
	do case
		case not found()
			??CHR(7)
			use
			sele (xbase)
			retu .F.

		case found() and tipo#'P'
			??CHR(7)
			use
			sele (xbase)
			retu .F.

		case found() and tipo='P'
			use
			sele (xbase)
			retu .T.
		OTHERWISE
			use
			sele (xbase)
			RETU .F.
	endcase
return

PROCEDURE pesadas
	defi wind PESAW from 3,57 to 20,78 title [ Pesadas ] ;
    SHADOW FLOAT COLO SCHEME 24
	acti wind PESAW
	use PESADAS
	go bottom
	BROWSE fields FPES:H=" Fecha   ",NVAC:H="# Vacas" NOEDIT NOAPPE NODELE TIMEOUT 10 IN PESAW
	RELE WIND PESAW
	clos all
return


PROCEDURE BUSQUEDA
push key clear
defi wind BUSCA from 8,50 to 10,70 shadow colo sche 17
acti wind BUSCA
do while .t.
xid=0
@0,1 say [ID ] get xid pict '#####'
read
if lastkey()=27
	exit
endi	
seek xid
if not found()
??chr(7)
wait wind tit(6) timeout 1
go top
else
exit
endi 
endd
rele wind BUSCA
pop key
return

PROCEDURE CFECHA
*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
    RTIT=[PESADA DE LECHE - PARAMETROS]
    XID=0
   	AXID=0
    @ 4,1 CLEA TO 19,78
	@ 22,1 CLEA TO 23,78
	@ 22,(80-LEN(RTIT))/2 SAY RTIT colo gr+/&cz
	@ 22,2 say [F1] COLO N/W
	@ 22,5 SAY [Ayuda] 
	@ 23,2 say [F2] COLO N/W
	@ 23,5 SAY [Directorio] 
	@ 22,69 say [F10] colo n/w
	@ 22,73 say [Fecha]
	@ 23,69 SAY [Esc] colo n/w
	@ 23,73 say [Salir] 

    @ 23,35 GET HOY VALID HOY>=DATE()-180 ;
    and HOY<=DATE() ERROR tit(3)
    READ TIMEOUT 30
    if lastkey()=27
    clos data
    retu
    endi
    @ 23,35 SAY HOY COLO W+/&CZ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

*   ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*   ณ HOJA DE TRABAJO PESADA DE LECHE ณ
*   ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
PROC HOJAT
SN='P'
@ 4,1 clea to 20,78
@ 21,0 to 21,79
@ 23,0 to 23,79
@ 22,22 SAY [HOJA DE TRABAJO PARA PESADA DE LECHE] colo gr+/&cz
@ 24,70 say [Esc] COLO N/W
@ 24,74 say [Salir] colo w+/&cz
@ 24,30 SAY [* Solo en Impresora *] colo r+/&cz

mc=0
xchoice=1
xchoice2=2
xfilt=""
xtit=""
	defi wind win12 from 8,21 to 16,59 title [ Ordenado por : ]  shadow colo sche 16
	acti wind win12
	@ 1,2 get xchoice func'*R Identificaciขn;Corral/Lote' colo ,,,,,gr+/b,,,w+/w
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
			xfilt="CORR"
			xtit="x CORRAL"
	endcase	
@22,1 say xtit colo w+/b

if xchoice=2
	@ 24,1 say [Corral] get mc pict '##' colo w+/b,n/w 
	@ 24,14 say [0] colo w+/&cz 
	@ 24,15 say [=Todos] 
read
endif
if lastkey()=27
	clos all
	return
else
do case
	case xchoice=1	
		select ID,CORR,FPAR,UCAL,STAT,PRM,DIA,DPR from reg where np>0 ;
		and fsec=B and fb2=B order by &xfilt into cursor PESA
	case xchoice=2 and mc=0
		select ID,CORR,FPAR,UCAL,STAT,PRM,DIA,DPR from reg where np>0 ;
		and fsec=B and fb2=B order by &xfilt into cursor PESA
	case xchoice=2 and mc#0
		select ID,CORR,FPAR,UCAL,STAT,PRM,DIA,DPR from reg where np>0 ;
		and fsec=B and fb2=B and corr=mc order by &xfilt into cursor PESA
endcase

@ 24,66 clea to 24,78
@ 24,70 SAY [Esc] colo n/w*
@ 24,74 SAY [Salir] colo w+/&cz

if SYS(13)='READY'
SET PRIN ON
SET CONS OFF

SET ESCA ON
ON ESCA DO CTRLSTOP
*ษอออออออออออออออออออออออออออป
*  Cambio a Letra 12 cpi
	???&q19
*ศอออออออออออออออออออออออออออผ
NV=0
PAG=1
NVAC=0
GO TOP
NTIT=[HOJA DE TRABAJO PARA PESADA DE LECHE]
ximprime=.T.
pag=1
DO WHILE .NOT. EOF()
	if ximprime=.F.
	exit
	endi
?'  Rancho   : '+Q36+'   '+nom 
?'  Area     : VIENTRES'
?'  Reporte  : '+NTIT
?'  Fecha    : '+DTOC(DATE())+'  Hora : '+TIME()
?'  Pagina   : '+STR(PAG,2,0)+space(70)+[LACTOFOX]
?'  '+REPLI(CHR(254),132)
?'   ID                     EDO     PROD'    
?'  NUM  CORRAL  DEL  DUC   REPRO    ANT     TURNO 1      TURNO 2      TURNO 3      TOTAL' 
?'  '+REPLI('-',132)

SCAN while inkey()#6
	if ximprime=.F.
	exit
	endi
*-------------------------------*
xdia=0
xdab=date()-fpar
xdpr=date()-fpar

if stat=[INSEM] .or. stat=[CARGA]
	xdia=date()-ucal
	xdab=ucal-fpar
endif	
*-------------------------------*
?' '+str(ID,5)+'   '+str(CORR,2)+'    '+str(xdia,3)+'  '+str(xdpr,3)+'   '+stat+'   '+str(prm,4,1)+'     ________     ________     ________     ________'

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
*ษอออออออออออออออออออออออออออป
*  Cambio a Letra 12 cpi
	???&q19
*ศอออออออออออออออออออออออออออผ
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
clos all
RETURN

FUNCTION FX
	parameters xfecha
	retu right(dtoc(xfecha),8)
return

