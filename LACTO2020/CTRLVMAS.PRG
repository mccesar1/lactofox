*зддддддддддддддддддддддд©
*Ё  PRUEBAS DE MASTITIS  Ё
*юддддддддддддддддддддддды
*мммммммммммммммммммммммммммммммммммммммм
	IF CLAVEX=Q22 .OR. CLAVEX=Q23 
	ELSE
    ??CHR(7)
    WAIT WIND [ Acceso Negado ] TIMEOUT 1
    RETU
    ENDI
*мммммммммммммммммммммммммммммммммммммммм
SPRU=[1]
	@23,0 to 23,79
    @24,70 SAY [Esc] colo n/w
    @24,74 say [Salir] 

	@ 22,30 SAY [PRUEBAS DE MASTITIS] COLO GR+/&CZ
	@ 24,1 SAY [1.Prueba California]
	@ 24,23 SAY [2.Celulas Somaticas]
	@ 24,1 SAY [1] COLO W+/&cz
	@ 24,23 SAY [2] COLO W+/&cz
	@ 22,51 GET SPRU PICT '@M 1,2'
READ
if lastkey()=27
	@4,0 clea to 24,79
	retu
endi	
	do case
		case SPRU=[1]
			do PCALIF
		case SPRU=[2]
			do PCCS
	endcase
@ 4,0 clea to 24,79
@23,0 to 23,79
return

*зддддддддддддддддддддддд©
*Ё  PRUEBAS CALIFORNIA   Ё
*юддддддддддддддддддддддды
PROCEDURE PCALIF
push key clear
on key label F1 do CTRLAYUD with VARREAD()
@4,0 clea to 24,79
RTIT=[PRUEBA CALIFORNIA]
DO CFECHA
on key label F2 do APLICMAST
@ 24,14 SAY [Total     ]
SN='N'
AXID=0

use REG order 2 in 1 alias REG
use MAST order 1 in 2 alias MAST

DO WHILE .T.
SN='N'
XID=0
@ 4,0 CLEA TO 21,79
@ 22,74 SAY AXID PICT '99999' FUNC 'J' colo w+/&cz
@ 22,1 SAY [ID] get XID PICT '#####' FUNC 'T' colo w+/&cz
READ
FLUSH
*-----------------------*
AXID=XID
	if xid=0
		@ 4,1 CLEA TO 19,78
		exit
	endi

sele REG
seek xid
if not found()
	??chr(7)
	wait wind tit(1) timeout 1
else
	do case
		case np>0 and fb2#B 
			??chr(7)
			wait wind [ Vaca dada de Baja ] TIMEOUT 1

		case np>0 and fsec#B and fb2=B or np>0 and fsec#B and fb2#B ;
			 and mot2=[C.RAST]
			??chr(7)
			wait wind [ Vaca en Secas ] TIMEOUT 2

		case np>0 and fsec=B and fb2=B or np>0 and fsec=B and fb2#B 
			sele MAST
			SEEK XID
				if FOUND()
					SCAN WHILE XID=ID
					ENDS
					SKIP-1
	
					if FTES>=HOY .AND. XID=ID
						??CHR(7)
						WAIT WIND [ Ya Actualizada ] TIMEOUT 2
						@ 19,11 clea to 19,28
					else
						DO ACTPRUEBA
					endi
				else
					DO ACTPRUEBA
				endi
		endcase
endi
ENDD
clos all
pop key

*зддддддддддддддддддддддд©
*Ё  TARJETA DE CUARTOS   Ё
*юддддддддддддддддддддддды
PROC actprueba
	ON KEY LABEL F9 DO AYUDA1
	ON KEY LABEL F2 *
	ON KEY LABEL F10 DO FCMAST
DEFI WIND MAST1 FROM 9,19 to 16,60 SHADOW COLO SCHE 16
@24,21 SAY [F9] colo n/w
@24,24 SAY [Grados]
ACTI WIND MAST1
DO WHILE SN='N'
STORE SPACE(1) TO RDD,RDI,RTD,RTI
@ 1,2 SAY [Cuartos      DI    DD    TI    TD] colo sche 18
@ 3,2 SAY [Grado] 
@ 3,16 GET RDI VALID VAL(RDI)>-1 .AND. VAL(RDI)<6 ERROR [ SOLO DE 0 A 5 ] 
@ 3,22 GET RDD VALID VAL(RDD)>-1 .AND. VAL(RDD)<6 ERROR [ SOLO DE 0 A 5 ] 
@ 3,28 GET RTI VALID VAL(RTI)>-1 .AND. VAL(RTI)<6 ERROR [ SOLO DE 0 A 5 ] 
@ 3,34 GET RTD VALID VAL(RTD)>-1 .AND. VAL(RTD)<6 ERROR [ SOLO DE 0 A 5 ] 
READ
IF LASTKEY()=27
DEACT WIND MAST1
*@ 23,64 clea to 23,78
EXIT
ENDI

@ 5,29 SAY [Correcto] GET SN PICT '@M S,N' 
READ
IF LASTKEY() = 27
DEACT WIND MAST1
EXIT
ENDI
IF SN='N'
@ 5,23 CLEA TO 5,38
LOOP
ELSE
sele MAST
APPE BLANK
REPL ID WITH XID,NP with REG.np,FTES WITH HOY,DD WITH VAL(RDD),;
DI WITH VAL(RDI),TD WITH VAL(RTD),TI WITH VAL(RTI)
ENDI
ENDD
sele REG
SEEK XID
DEACT WIND MAST1
*CLOS DATA 

*здддддддддддддддддддддддддддддддддд©
*Ё  PRUEBAS ANTERIORES DE P. CALIF  Ё
*юдддддддддддддддддддддддддддддддддды
PROCEDURE FCmast
ON KEY LABEL F2 *
ON KEY LABEL F10 *

IF XID=0
ELSE
sele MAST
*USE MAST order 1
SEEK XID
DEFI WIND FC FROM 6,16 TO 18,66 PANEL TITLE [ PRUEBAS ANTERIORES ] SHADOW COLO SCHE 17
IF .NOT. FOUND()
??CHR(7)
WAIT WIND [ No tiene Datos ] TIMEOUT 2
ELSE
ACTI WIND FC
@ 1,2 SAY [  Fecha      CUARTOS    DI    DD    TI    TD ] COLO SCHE 17
r=2
SCAN while ID=XID FOR FTES<>B
@ r,3 say FTES colo sche 13
@ r,27 say DI colo sche 13
@ r,33 say DD colo sche 13
@ r,39 say TI colo sche 13
@ r,45 SAY TD colo sche 13
r=r+1

if r>=10 
@ 10,39 SAY [Enter] 
@ 10,45 SAY CHR(16) 
DO WHILE INKEY() # 13
IF LASTKEY()=27
EXIT
ENDI
ENDD
r=2
@ R,1 FILL TO 10,64 
endi
endS
@ 10,39 SAY [Enter] 
@ 10,45 SAY CHR(16) 
DO WHILE INKEY()#13
ENDD
ENDI

ENDI
DEACT WIND FC
ON KEY LABEL F10 DO FCMAST
ON KEY LABEL F2 DO APLICMAST
sele REG
*use REG order 2
SEEK XID

*зддддддддддддддддддддддддддд©
*Ё   GRADOS PARA P. CALIF    Ё
*юддддддддддддддддддддддддддды
PROC AYUDA1
ON KEY LABEL F2 *
ON KEY LABEL F10 *
DEFI WIND VMAST2 FROM 4,50 TO 19,77  panel title [ GRADOS ] SHADOW colo SCHE 17
ACTI WIND VMAST2
@ 1,6 SAY [0 = Negativo] COLO SCHE 17
@ 3,6 SAY [1 = Trazas] COLO SCHE 17
@ 5,6 SAY [2 = Grado A] COLO SCHE 17
@ 7,6 SAY [3 = Grado B] COLO SCHE 17
@ 9,6 SAY [4 = Clinica] COLO SCHE 17
@ 11,6 SAY [5 = Perdido] COLO SCHE 17
@ 13,17 say [Enter] colo SCHE 17
@ 13,23 SAY CHR(16) 
DO WHILE INKEY() # 13
IF LASTKEY()=27
EXIT
ENDI
ENDD
RELE WIND VMAST2
ON KEY LABEL F9 DO AYUDA1
ON KEY LABEL F10 DO FCMAST
RETU

*зддддддддддддддддддддддддддддддддд©
*Ё   PRUEBA CALIF NEG. AUTOMATICA  Ё
*юддддддддддддддддддддддддддддддддды
PROC APLICmast
ON KEY LABEL F2 *
SN='N'
DEFI WIND APLICTMAST FROM 8,18 to 16,60 COLO SCHE 17 SHADOW
ACTI WIND APLICTMAST
@ 1,2 SAY [Este programa adicional le permitira] COLO SCHE 17
@ 2,2 say [anotar la fecha de prueba y resultado] COLO SCHE 17
@ 3,2 say [Negativo a todas las vacas.] COLO SCHE 17
@ 6,28 say [Continuar] get sn PICT '@M S,N'COLO SCHE 17
READ
IF LASTKEY()=27 .OR. SN='N'
RELE WIND APLICTMAST
ELSE
acti screen
CLOS DATA
sele 2
USE MAST order 1
SELE 1
use REG order 2
SET FILT TO FPAR<>B .AND. FB2=B .AND. FSEC=B .AND. FPAR<=HOY .OR. FPAR<>B .AND. FB2<>B .AND. MOT2="C.RAST" .AND. FSEC=B .AND. FPAR<=HOY
GO TOP
XID=ID
DO WHILE .NOT. EOF()
XID=ID
@ 22,4 say xid pict '99999' colo n/w
SELE 2
SEEK XID
*----------------------------------*
DO CASE
CASE .NOT. FOUND() 
APPE BLANK
REPL ID WITH XID,np with REG.np,FTES WITH HOY,DD WITH 0,DI WITH 0,TD WITH 0,TI WITH 0

CASE FOUND()
SCAN WHILE XID=ID
ENDS
SKIP-1
IF ID=XID .AND. FTES<HOY
APPE BLANK
REPL ID WITH XID,NP with REG.np,FTES WITH HOY,DD WITH 0,DI WITH 0,TD WITH 0,TI WITH 0
ELSE
ENDI

ENDCASE
*----------------------------------*
SELE 1
SKIP
ENDD
XID=0
??CHR(7)
@ 22,11 SAY [OK] colo w+/&cz*
*@ 19,18 clea to 19,65
RELE WIND APLICTMAST
clos data
ENDI
CLOS ALL
ON KEY LABEL F2 DO APLICMAST

*зддддддддддддддддддддддд©
*Ё  CELULAS SOMATICAS    Ё
*юддддддддддддддддддддддды
PROC PCCS
@4,0 clea to 24,79
RTIT=[CELULAS SOMATICAS]
DO CFECHA
@ 24,55 clea to 24,68
*ON KEY LABEL F10 DO TARJETA 
	CLOS DATA
	SELE 2
	USE CCS order 1
	SELE 1
	use REG order 2

xid=0
axid=0
DO WHILE .T.
	@ 22,11 clea to 22,30
	AXID=XID
	SN='S'
	XID=0
    Mccs=0
	mLSC=0
	@ 22,74 SAY AXID PICT '99999' FUNC 'J' colo w+/&cz
	@ 22,1 SAY [ID] GET XID PICT '99999' FUNC 'J,T' COLO W+/&CZ,N/W
	READ TIMEOUT 60
IF LASTKEY()=27 .OR. XID=0
	EXIT
ENDI
	SEEK XID
	if not found()
		??chr(7)
		wait wind tit(1) timeout 1
	else

	do case
		case np>0 and fb2#B 
			??chr(7)
			wait wind [ Vaca dada de Baja ] TIMEOUT 1

		case np>0 and fb2=B and fsec=B

		if FCCS>=HOY and xid=ID
			??chr(7)
			wait wind[ Ya Actualizada ] TIMEOUT 1
			@ 19,11 clea to 19,28
		else
		do while .t.
		@ 22,11 say [CCSx1000] get Mccs pict '#####' valid Mccs>0 ;
				and Mccs<9999 error [ Dato No Valido ] 
		read TIMEOUT 60
		if lastkey()=27 
		exit
		endi
		xnp=NP
	
	** Calcula el valor del score lineal.
	** ----------------------------------
	do case
		case Mccs>0 and Mccs<=4525
			mLSC=(log(Mccs/100)/.693146)+3
		case Mccs>4525
			mLSC=9.0
		otherwise
			mLSC=0
	endcase
	
	REPL FCCS WITH HOY,ACCS WITH CCS,CCS WITH Mccs,ALSC WITH LSC,;
	LSC WITH mLSC
	NDPR=(hoy-FPAR)-(hoy-FSEC)
SELE 2
SEEK XID
	if not found()
	APPE BLANK
	REPL ID WITH XID,NP WITH XNP,FECHA WITH HOY,CCS WITH A->CCS,;
	LSC WITH A->LSC,DEL WITH NDPR,PRM WITH A->PRM
		ELSE
	SCAN WHILE ID=XID
	ENDS
	SKIP-1
	DO CASE
	CASE FECHA=HOY
	REPL CCS WITH A->CCS,LSC WITH A->LSC
	CASE FECHA<HOY
	APPE BLANK
	REPL ID WITH XID,NP WITH XNP,FECHA WITH HOY,CCS WITH A->CCS,;
	LSC WITH A->LSC,DEL WITH NDPR,PRM WITH A->PRM
	OTHERWISE
	ENDCASE
	ENDI
SELE 1
EXIT
ENDD
ENDI 
****

OTHERWISE
	?CHR(7)
	WAIT WIND tit(6)

ENDCASE
endi
AXID=XID
ENDD
CLOS DATA
ON KEY LABEL F10 *
*ммммм Termina  Procedure мммммммммммммммммммммммммммммммммм*

PROC CFECHA
*зддддддддддддддддддддддддддддддддддддддддддддд©
    XID=0
   	AXID=0
	@ 22,(80-LEN(RTIT))/2 SAY RTIT colo gr+/&cz
    @22,70 SAY [IDa] COLO N/W
    @23,0 to 23,79
    @24,1 say [F1] COLO N/W
    @24,4 SAY [Ayuda] 
    @24,11 say [F2] COLO N/W
*   @ 24,14 SAY [Total     ]
    @24,14 SAY [Directorio] 
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
	@23,0 to 23,79
    return to CTRLVMAS &&TO MENUM
    endi
    @ 24,35 SAY HOY COLO W+/&CZ
*юддддддддддддддддддддддддддддддддддддддддддддды

