FUNCTION FX
	parameters xfecha
*	if empty(xfecha)
*		return space(8)
*	else
		retu right(dtoc(xfecha),8)
*	endif
return


*---------------------------------------------------
function FX0
	parameters xfecha
	wmeses="EneFebMarAbrMayJunJulAgoSepOctNovDic"

	if empty(xfecha)
		return space(8)
	else
*		return right("00"+ltrim(str(day(xfecha),2)),2)+"/"+subs(wmeses,month(xfecha)*3-2,3)+"/"+right(str(year(xfecha),4),2)

*		return right("00"+ltrim(str(year(xfecha),2)),2)  &&+"/"+subs(wmeses,month(xfecha)*3-2,3)+"."+ltrim(str(day(xfecha),2)),2)

		return right(str(year(xfecha)),2)+" ";
			   +subs(wmeses,month(xfecha)*3-2,3);
			   +str(day(xfecha),2)
	endif
*---------------------------------------------------

*   ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
*   ≥  VACAS A REVISION DE MATRIZ  ≥
*   ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
PROC RV101
*if ctod(qfecham)>date()
*??chr(7)
*wait wind [ Licencia Expirada ]+qFECHAM  timeout 2
*return
*endi
@ 4,0 CLEA TO 24,79
@21,0 to 21,79
RTIT=[VACAS SIN REVISION POST-PARTO] 
@22,(80-LEN(RTIT))/2 SAY RTIT colo gr+/&cz
@23,0 to 23,79
*--------
use REG order 2
do orden
if xchoice2=2
	clos all
	return
endif	

if val(xorden)=9
@ 24,70 SAY [Espere] colo n*/w
inde on (date()-UCAL) tag DUC of DUC for NP>0 
set order to DUC
else
set order to val(xorden)
endi

*---------
@ 4,1 say [                                                                              ] colo n/w
@ 5,1 say [      ID  Corral   DEL         ID  Corral   DEL         ID  Corral   DEL      ] colo n/w
@ 24,33 SAY 'De '+STR(Q3,2)+' a '+str(Q6,2)+' Dias'
@ 24,1 say [Total] colo w+/&cz
ROW=7
COL=4
DAT=0
@ 24,70 SAY [Espere] colo n*/w
GO TOP
@ 24,79 SAY []
SET FILT TO (NP>0 AND FB2=B)
SCAN FOR UTRA=B and STAT=[FRESC] and DATE()-FPAR>=Q3 AND DATE()-FPAR<=Q6  WHILE INKEY()#6
@ 24,70 SAY [Espere] colo n*/w
@ 24,8 SAY DAT+1 PICT '9999'
@ ROW,COL SAY ID PICT '#####' colo gr+/&cz
*--------------------------*
if FPRO>=date() or FT2>=date() 
@ ROW,COL SAY &mensa1
ENDI
IF PSV="S"
@ ROW,COL+5 SAY &mensa2
ENDI
*--------------------------*
@ ROW,COL+9 SAY CORR PICT '99' 
@ ROW,COL+15 SAY DATE()-FPAR PICT '9999' 

IF ROW=20 .AND. COL=54
@ 24,70 SAY [Enter] COLO N/W
@ 24,75 say [ ] colo w/&cz
@ 24,76 SAY CHR(16) colo w+/&cz
DO WHILE INKEY() # 13
IF LASTKEY()=6
@ 4,1 CLEA TO 20,78
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
COL=COL+25
ENDI
DAT=DAT+1
ROW=ROW+1
ENDS


IF DAT=0
@ 24,9 SAY [0]
ENDI
??CHR(7)
@ 24,70 SAY [Enter] COLO N/W
@ 24,75 say [ ] colo w/&cz
@ 24,76 SAY CHR(4) colo w+/&cz
DO WHILE INKEY() # 13
ENDD
IF DAT=0
ELSE
SN='N'
@ 24,69 say [Imprimir] GET SN PICT '@M S,N' colo n/w
READ
IF SN='N' .OR. LASTKEY()=27
ELSE
IF SN='S' .AND. SYS(13)='READY'
SET CONS OFF
SET PRIN ON
NVE=0
PAG=1
NVAC=0
CR=[ ]
NTIT=[VACAS SIN REVISION POST-PARTO]
NTIT2=[DE ]+STR(Q3,2)+[ A ]+str(Q6,2)+[ DIAS EN LECHE]
GO TOP
DO WHILE .NOT. EOF() 

DO CTRLVRPT
*----------------------------------------------------------------------*
?'    ID     FECHA    OBSERV    PARTO    SEXO    ESTADO            CORRAL'           
?'   VACA    PARTO               NUM.    CRIA    REPROD    DEL      NUM      N O T A'
?'  '+REPLI('-',94)

SCAN FOR UTRA=B AND STAT='FRESC' AND DATE()-FPAR>=Q3 ;
AND DATE()-FPAR<=Q6 AND FPAR#B and FT2<date() ;
and FPRO<DATE() WHILE INKEY() # 6 

CR=[ ]
IF PSV="S"
CR=[+]
ENDI

REPL DPR WITH (DATE()-FPAR)-(DATE()-FSEC)
?SPACE(2)+STR(ID,5)+CR+'  '+fx(FPAR)+'  '+OBS+'     '+STR(NP,2,0);
+'       '+SEXC+'     '+STAT+'     '+STR(DPR,3,0)+'       '+STR(CORR,2,0)+'     __________________'
NVE=NVE+1
nvac=nvac+1
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
?'   TOTAL = '+STR(NVAC,4)
EJEC
SET PRIN OFF
SET CONS ON
SET FILT TO
ELSE
??CHR(7)
WAIT WIND [ IMPRESORA APAGADA O DESCONECTADA ] TIMEOUT 2
SET FILT TO
ENDI
ENDI
ENDI
CLOS DATA

*   ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
*   ≥  VACAS SUCIAS  ≥
*   ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
PROC RV102
SELE 2
USE CAMBIO
SELE 1
use REG order 2
@ 4,0 CLEA TO 24,79
@21,0 to 21,79
RTIT=[VACAS SUCIAS] 
@22,(80-LEN(RTIT))/2 SAY RTIT colo gr+/&cz
@23,0 to 23,79
@ 4,1 say [                                                                              ] colo n/w
@ 5,1 say [      ID  Corral   DEL         ID  Corral   DEL         ID  Corral   DEL      ] colo n/w
@ 24,33 SAY 'Hasta'+STR(Q6,3)+' Dias'
@ 24,1 say [Total] colo w+/&cz
ROW=7
COL=4
DAT=0
GO TOP
SET FILT TO (NP>0 AND FB2=B) 
SCAN FOR STAT='SUCIA' AND UCAL+21<DATE() AND DATE()-FPAR<=B->FP while inkey() # 6
@ 24,70 SAY [Espere] colo n*/w
@ 24,8 SAY DAT+1 PICT '9999'
@ ROW,COL SAY ID PICT '#####' colo gr+/&cz
*--------------------------*
if FPRO>=date() or FT2>=date() 
@ ROW,COL SAY &mensa1
ENDI
IF PSV="S"
@ ROW,COL+5 SAY &mensa2
ENDI
*--------------------------*
@ ROW,COL+9 SAY CORR
@ ROW,COL+16 SAY (DATE()-FPAR)-(DATE()-FSEC) PICT '999'

IF ROW=20 .AND. COL=54
@ 24,70 SAY [Enter] COLO N/W
@ 24,75 say [ ] colo w/&cz
@ 24,76 SAY CHR(16) colo w+/&cz
DO WHILE INKEY() # 13
IF LASTKEY()=6
@ 4,1 clea to 20,79
GO BOTTOM
EXIT
ENDI
ENDD
@ 6,2 CLEA TO 20,79
@ 24,58 CLEA TO 24,78
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
@ 24,9 SAY [0]
ENDI
??CHR(7)
@ 24,70 SAY [Enter] COLO N/W
@ 24,75 say [ ] colo w/&cz
@ 24,76 SAY CHR(4) colo w+/&cz
DO WHILE INKEY() # 13
ENDD
IF DAT=0
ELSE
SN='N'
@ 24,69 say [Imprimir] GET SN PICT '@M S,N' colo n/w 
READ
IF LASTKEY()=27 .OR. SN='N'
ELSE
IF SN='S'.AND. SYS(13)='READY'
SET CONS OFF
SET PRIN ON
GO TOP
NTIT=[VACAS SUCIAS]
NTIT2='HASTA'+STR(Q6,3)+' DIAS'
NVE=0
PAG=1
NVAC=0
DO WHILE .NOT. EOF()
DO CTRLVRPT
*----------------------------------------------------------------------*
?'   ID    FECHA           ULT.     ULTIMA    ULTIMO    MEDIC  POSIBLE    DIAS    ESTADO    CORRAL'
?'  VACA   PARTO     DEL   PROD     REV/Tx    CALOR     APLIC   CALOR      PAS    REPROD     NUM'
?'  '+REPLI('-',94)

SCAN FOR STAT='SUCIA' AND FPRO<DATE() AND UCAL+21<DATE() ;
AND DATE()-FPAR<=Q6 and FT2<date() WHILE INKEY()#6

CR=[ ]
IF PSV="S"
CR=[+]
ENDI
?' '+STR(ID,5)+CR+' '+fx(FPAR)+'   '+STR(DATE()-FPAR,3)+'   '+STR(PRM,4,1);
+'     '+fx(UTRA)+'  '+fx(UCAL)+'   '+STR(MED1,3)+'   '+fx(UCAL+21);
+'   '+STR((UCAL+21)-DATE(),4)+'     '+STAT+'      '+ STR(CORR,2)
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
SET FILT TO
SET PRIN OFF
SET CONS ON
SET FILT TO
ELSE
??CHR(7)
WAIT WIND [ IMPRESORA APAGADA O DESCONECTADA ] TIMEOUT 2
SET FILT TO
ENDI
ENDI
ENDI
CLOS DATA

*   ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
*   ≥  VACAS A NUEVA REVISION POST-PARTO     ≥
*   ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
PROC RV103
@ 4,0 CLEA TO 24,79
@21,0 to 21,79
@23,0 to 23,79
RTIT=[VACAS A NUEVA REVISION POST-PARTO] 
@22,(80-LEN(RTIT))/2 SAY RTIT colo gr+/&cz
@24,35 SAY STR(Q4,3)+'+ Dias'
@24,1 SAY [Total] colo w+/&cz

@ 4,0 CLEA TO 20,79
@ 4,1 say [        Fecha                  Ultima     Ult   Dias    Ult    Estado   Corr  ] colo N/w
@ 5,1 say [    ID  Parto     Observ  DEL  Rev/Tx     Med   Tran    Prod   Reprod   Num   ] colo N/w
ROW=7
COL=2
DAT=0
@ 24,70 SAY [Espere] colo n*/w
use REG order 2
GO TOP
SET FILT TO (NP>0 AND FB2=B)

SCAN FOR DATE()-FPAR<Q6 AND DATE()-UTRA>=Q4 AND UCAL=B ;
AND STAT='LIMPI' AND UTRA#B WHILE INKEY() # 6

@ 24,8 SAY DAT+1 PICT '9999'
@ 24,70 SAY [Espere] colo n*/w

@ ROW,COL SAY ID PICT '#####' colo gr+/&cz
*--------------------------*
if FPRO>=date() or FT2>=date() 
@ ROW,COL SAY &mensa1
ENDI
IF PSV="S"
@ ROW,COL+5 SAY &mensa2
ENDI
*--------------------------*

@ ROW,COL+7 SAY fx(FPAR)
@ row,col+17 say OBS
@ ROW,COL+24 SAY (DATE()-FPAR)-(DATE()-FSEC) PICT '9999'
@ ROW,COL+30 SAY fx(UTRA)
@ ROW,COL+41 SAY MED1
@ ROW,COL+48 SAY date()-utra pict '999' colo g+/&cz
@ ROW,COL+54 SAY PRM colo r+/&cz
@ ROW,COL+62 SAY STAT
@ ROW,COL+72 SAY CORR PICT '99'

IF ROW=20 
@24,70 SAY [Enter] COLO N/W
@24,75 say [ ] colo w/&cz
@24,76 SAY CHR(16) colo w+/&cz
DO WHILE INKEY() # 13
IF LASTKEY()=6
@ 4,1 clea to 20,78
GO BOTTOM
EXIT
ENDI
ENDD
@ 6,2 CLEA TO 20,77
@ 24,58 CLEA TO 24,78
ROW=6
COL=2
ENDI
DAT=DAT+1
ROW=ROW+1
ENDS
IF DAT=0
@ 24,9 SAY [0]
ENDI
??CHR(7)
@ 24,70 SAY [Enter] COLO N/W
@ 24,75 say [ ] colo w/&cz
@ 24,76 SAY CHR(4) colo w+/&cz
DO WHILE INKEY() # 13
ENDD
IF DAT=0
ELSE
SN='N'
@ 24,69 say [Imprimir] GET SN PICT '@M S,N' colo n/w 
READ
IF SN='N' .OR. LASTKEY()=27
ELSE
IF SN='S' .AND. SYS(13)='READY'
SET CONS OFF
SET PRIN ON
GO TOP
NTIT='VACAS A NUEVA REVISION POST-PARTO'
NTIT2='DE '+STR(Q4,3)+'+ DIAS'
NVE=0
PAG=1
NVAC=0
DO WHILE .NOT. EOF() 
DO CTRLVRPT
*----------------------------------------------------------------------*
?'    ID      FECHA                   ULTIMA       ULT   DIAS   ULT.    ESTADO   CORRAL'
?'   VACA     PARTO     OBSERV  DEL   REV/Tx       MED   TRANS  PROD    REPROD    NUM  '
?'  '+REPLI('-',94)

SCAN FOR DATE()-FPAR<Q6 AND DATE()-utra>=Q4 AND UCAL=B ;
AND STAT='LIMPI' AND utra#B and FT2<date() AND FPRO<DATE() ;
WHILE INKEY() # 6

CR=[ ]
IF PSV="S"
CR=[+]
ENDI

?SPACE(2)+STR(ID,5)+CR+'   '+fx(FPAR)+'   '+OBS+'  '+STR(DATE()-FPAR,3)+'   '+fx(utra)+'     '+STR(MED1,3)+'    ';
+STR(DATE()-utra,3)+'   '+STR(PRM,4,1)+'     '+STAT+'     '+STR(CORR,2)
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
SET FILT TO
ENDI
ENDI
ENDI
CLOS DATA


*   ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
*   ≥  VACAS EN ANESTRO  ≥
*   ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
PROC RV104
set procedure to ctrludfs
@ 4,0 CLEA TO 24,79
@21,0 to 21,79
@22,20 say [VACAS EN ANESTRO O CALORES NO DETECTADOS] colo gr+/&cz
@23,0 to 23,79
@24,35 SAY STR(Q6,3)+'+ Dias' 
@24,1 SAY [Total] colo w+/&cz
*--------
use REG order 2
do orden
if xchoice2=2
	clos all
	return
endif	
if val(xorden)=9
@ 24,70 SAY [Espere] colo n*/w
inde on (date()-UCAL) tag DUC of DUC for NP>0 
set order to DUC
else
set order to val(xorden)
endi

SET FILT TO (NP>0 AND FB2=B)
*---------
@24,70 SAY [Espere] colo n*/w
go top
repl all stat with 'ANEST' for ns<1 and (date()-fpar)>=Q6 ;
and ucal=B and stat#'SUCIA' and stat#'TRATA'

@ 4,1 say [                     Lact  Ultimo   Ins        Edo     Prod   Posible   Dias  ] colo N/w
@ 5,1 say [    ID  Corral  DEL  Num   Calor    Num   DUC  Repro   Actu   Calor      Pas  ] colo N/w

ROW=7
COL=1
DAT=0
GO TOP

SCAN for date()-fpar>=Q6 and ns<1 and stat='ANEST' ;
or stat='LIMPI' and NS<1 and date()-fpar>=Q6 and ucal+21<date() ;
WHILE INKEY()#6

@ 24,8 SAY DAT+1 PICT '9999'
@ 24,70 SAY [Espere] colo n*/w

@ ROW,COL+2 SAY ID colo gr+/&cz
*--------------------------*
if FPRO>=date() or FT2>=date() 
@ ROW,COL+2 SAY &mensa1
ENDI


IF PSV="S"
@ ROW,COL+7 SAY &mensa2
ENDI
*--------------------------*
@ ROW,COL+10 SAY CORR 
@ ROW,COL+15 SAY (DATE()-FPAR) pict '9999'
@ ROW,COL+22 SAY NP
@ ROW,COL+27 SAY fx(UCAL)
@ ROW,COL+37 SAY NS
@ ROW,COL+41 SAY (DATE()-UCAL) pict '9999'
@ ROW,COL+47 SAY stat
@ ROW,COL+55 SAY PRM  pict '##.#' colo r+/&cz 
@ ROW,COL+62 SAY fx(ucal+21)

@ ROW,COL+72 SAY (UCAL+21)-date() pict'####' colo g+/&cz

IF ROW=20 
	@ 24,70 SAY [Enter] COLO N/W
	@ 24,75 SAY [ ] COLO W+/&CZ
	@ 24,76 SAY CHR(16) colo w+/&cz
DO WHILE INKEY() # 13
	IF LASTKEY()=6
		@ 6,2 CLEA TO 20,77
		GO BOTTOM
		EXIT
	ENDI
ENDD
	@ 6,2 CLEA TO 20,79
	ROW=6
	COL=1
ENDI
	DAT=DAT+1
	ROW=ROW+1
ENDS
IF DAT=0
	@ 24,9 SAY [0]
ENDI
	@ 24,60 CLEA TO 24,78
	??CHR(7)
	@ 24,70 SAY [Enter] COLO N/W
	@ 24,75 SAY [ ] COLO W+/&CZ
	@ 24,76 SAY CHR(4) colo w+/&cz
DO WHILE INKEY() # 13
ENDD
IF DAT=0
ELSE
SN='N'
@ 24,69 say [Imprimir] GET SN PICT '@M S,N' colo n/w 
READ
IF SN='N' .OR. LASTKEY()=27
ELSE
IF SN='S' .AND. SYS(13)='READY'
SET CONS OFF
SET PRIN ON
GO TOP
NTIT=[VACAS EN ANESTRO & CALORES NO DETECTADOS]
NTIT2='DE '+STR(Q6,3)+'+ DIAS'
NVE=0
PAG=1
NVAC=0
DO WHILE .NOT. EOF()
DO CTRLVRPT
*--------------------------------------------------------------------------------------*
?'    ID     FECHA           ULT.   ULTIMO   MEDIC    POSIBLE   DIAS    ESTADO    CORRAL'
?'   VACA    PARTO     DEL   PROD   CALOR    APLIC     CALOR     PAS    REPROD     NUM'
?'  '+REPLI('-',94)
SCAN FOR DATE()-FPAR>=Q6 AND NS=0 AND STAT='ANEST' AND FSEC=B AND FPRO<DATE() and FT2<date() ;
OR STAT='LIMPI' AND NS=0 AND DATE()-FPAR>=Q6 AND UCAL+21<DATE() AND FPRO<DATE() and FT2<date() ;
WHILE INKEY() # 6 
CR=[ ]
IF PSV="S"
CR=[+]
ENDI
?'  '+STR(ID,5)+CR+'  '+fx(FPAR)+'   '+STR(DATE()-FPAR,3)+'   '+STR(PRM,4,1);
+'  '+fx(UCAL)+'   '+STR(MED1,3)+'    '+fx(UCAL+21);
+'   '+STR((UCAL+21)-DATE(),4)+'     '+STAT+'      '+ STR(CORR,2)
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
SET FILT TO
SET PRIN OFF
SET CONS ON
ELSE
??CHR(7)
WAIT WIND [ IMPRESORA APAGADA O DESCONECTADA ] TIMEOUT 2
SET FILT TO
ENDI
ENDI
ENDI
CLOS DATA
@4,0 clea to 24,79
if file("DUC.CDX")
erase DUC.cdx
endi

*   ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
*   ≥  VACAS A DIAGNOSTICO DE GESTACION  ≥
*   ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
PROC RV105
@21,0 TO 21,79
@22,24 say [VACAS A DIAGNOSTICO DE GESTACION] colo gr+/&cz
@23,0 TO 23,79
xQ7=Q7
@ 24,1 say [Total] colo w+/&cz
*@ 24,35 SAY STR(Q7,3)+'+ Dias'
@ 24,35 say [DUC >= ] get xQ7 pict'###'valid xQ7>=7 and xQ7<=282 erro [ RANGO DE 7 A 282 DIAS ]
read
SN=[S]
ROW=7
COL=1
DAT=0
*--------
use REG order 2
do orden
if xchoice2=2
	clos all
	return
endif	

if val(xorden)=9
	@ 24,70 SAY [Espere] colo n*/w
	inde on (date()-UCAL) tag DUC of DUC for NP>0 and STAT=[INSEM]
	set order to DUC
else
	set order to val(xorden)
endi

@ 4,1 say [                     Lact  Ultimo   Ins                Prod   Serie           ] colo N/w
@ 5,1 say [    ID  Corral  DEL  Num   Calor    Num   DUC    DAB   Actu   Toro      Tec   ] colo N/w

SET FILT TO (NP>0 AND FB2=B)
*---------
@ 24,69 clea to 24,79

SCAN for fb2=b and date()-UCAL>=xQ7 and STAT='INSEM' 

@ 24,8 SAY DAT+1 PICT '9999'
@ 24,70 SAY [Espere] colo n*/w

@ ROW,COL+2 SAY ID colo gr+/&cz
*--------------------------*
if FPRO>=date() or FT2>=date() 
@ ROW,COL+2 SAY &mensa1
ENDI
IF PSV="S"
@ ROW,COL+7 SAY &mensa2
ENDI
*--------------------------*
@ ROW,COL+10 SAY CORR 
@ ROW,COL+15 SAY (DATE()-FPAR) pict '9999'
@ ROW,COL+22 SAY NP
@ ROW,COL+27 SAY fx(UCAL)
@ ROW,COL+37 SAY NS
@ ROW,COL+41 SAY (DATE()-UCAL) pict '9999' colo g+/&cz
@ ROW,COL+48 SAY ucal-fpar pict '9999'
@ ROW,COL+55 SAY PRM  pict '99.9' colo r+/&cz
@ ROW,COL+61 SAY idt
@ ROW,COL+72 SAY utec

IF ROW=20 
	@ 24,70 SAY [Enter] COLO N/W
	@ 24,75 SAY [ ] COLO W+/&CZ
	@ 24,76 SAY CHR(16) colo w+/&cz
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
	COL=1
ENDI
	DAT=DAT+1
	ROW=ROW+1
ENDS
IF DAT=0
	@ 24,9 SAY [0]
ENDI
	@ 24,60 CLEA TO 24,78
	??CHR(7)
	@ 24,70 SAY [Enter] COLO N/W
	@ 24,75 SAY [ ] COLO W+/&CZ
	@ 24,76 SAY CHR(4) colo w+/&cz

DO WHILE INKEY() # 13
ENDD

IF DAT=0
ELSE
SN='N'
@ 24,69 say [Imprimir] GET SN PICT '@M S,N' colo n/w 
READ
IF SN='N' .OR. LASTKEY()=27
ELSE
IF SN='S' .AND. SYS(13)='READY'
SET CONS OFF
SET PRIN ON
GO TOP
NVE=0
PAG=1
NVAC=0
NTIT=[VACAS A PALPAR]
NTIT2='DE '+STR(Q7,3)+'+ DIAS'
DO WHILE .NOT. EOF()
DO CTRLVRPT
*----------------------------------------------------------------------*
?'    ID   CORRAL  ULTIMO     DIAS  NUM   ULT.         DIAS   DIAS   ESTADO'
?'   VACA   NUM    CALOR      ABIE  SER   PROD   DEL   SECA   INSE   REPROD   N O T A'
?'  '+REPLI('-',94)
SCAN for fb2=b and DATE()-UCAL>=xQ7 AND STAT='INSEM' and FPRO<date() and FT2<date() while inkey()#6
CR=[ ]
IF PSV="S"
CR=[+]
ENDI
?'  '+STR(ID,5)+CR+'   '+STR(CORR,2)+'    '+fx(UCAL)+'   '+STR((UCAL-FPAR),4)+'   ';
+STR(NS,2)+'   '+STR(PRM,4,1)+'   '+STR((DATE()-FPAR),3)+'    '+STR((DATE()-FSEC),3)+'    ';
+STR((DATE()-UCAL),3)+'    '+STAT+'  __________________'

NVAC=NVAC+1
NVE=NVE+1
IF NVE=WLI 
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
ELSE
??CHR(7)
WAIT WIND [ IMPRESORA APAGADA O DESCONECTADA ] TIMEOUT 2
SET FILT TO
ENDI
ENDI
ENDI
CLOS DATA
if file("DUC.CDX")
erase DUC.cdx
endi


*   ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
*   ≥  VACAS A SECAR POR GESTACION CON HISTORIAL DE PESADAS     ≥
*   ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
PROC RV106
declare aw(60),pw(16),tw(10)
nvac=0
row=7
sn='N'
mc=0
xpag=0
pw=0
tw=0
@21,0 TO 21,79
@23,0 TO 23,79
@22,27 SAY [VACAS A SECAR POR GESTACION] COLO gr+/&cz
*--------
use prod order 2 alias PROD in 2 
use REG order 2 alias  REG  in 1

do orden
if xchoice2=2
	clos all
	return
endif	

if val(xorden)=9
@ 24,70 SAY [Espere] colo n*/w
inde on ucal+(Q39-q35) tag DUC of DUC for NP>0 and FSEC=B and STAT=[CARGA] 
set order to DUC
else
set order to val(xorden)
endi

sele 1
set filt to (np>0 and fb2=B and fsec=B and stat=[CARGA])
go top

@ 24,69 clea to 24,78
@ 4,1 say [                                 VRel  VRel  Prod     Ultimas Pesadas   Dias  ] colo N/w
@ 5,1 say [    ID Cor Lac    DUC  DEL  DAB  Prom  Actu  Prom    5   4   3   2   1  Falt  ] colo N/w

@24,2 say [Total  ] colo w+/&cz

SCAN for (date()-ucal)>=(Q39-Q35)-Q8 while inkey()#6

	@24,8 say nvac+1 pict"####"
	xid=REG.id
	xnp=REG.np
	xfpar=REG.fpar
	xduc=date()-ucal
	xdel=(date()-fpar)-(date()-fsec)
	xprm=REG.prm
	xdias=(REG.ucal+(Q39-Q35))-date()

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

@ row,53 say aw(5) pict '##' colo r+/&cz
@ row,57 say aw(4) pict '##' colo r+/&cz
@ row,61 say aw(3) pict '##' colo r+/&cz
@ row,65 say aw(2) pict '##' colo r+/&cz
@ row,69 say aw(1) pict '##' colo r+/&cz

@ row,73 say xdias pict '####' colo g+/&cz

IF ROW=20
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

endscan

	??chr(7)
	@ 24,70 say [Enter] colo n/w
	@ 24,76 say chr(4) colo w+/&cz
	do while inkey()#13
	endd
	sn="N"
	@ 24,69 say [Imprimir] get SN pict '@M S,N' colo n/w 
	read
	if SN='N' or LASTKEY()=27
	else
	** Checa si esta encendida la impresora
	** ------------------------------------	
	IF sys(13)='READY'
*		clos databases
		nvac=0
		sn='N'
		xpag=0
		pw=0
		tw=0
		@24,67 clea to 24,78
		@24,70 SAY [Espere] colo n*/w
			
		*…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª
		*  Cambio a Letra 17 cpi
			???&q21
		*»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº
		set console off
		set printer on
		_peject="AFTER"
		_padvance="LINEFEED"
*		_plength=60
		_plineno=0
		_pageno=1
		xpag=1			
		ximprime=.t.

	PRINTJOB
		nvac=0
		ntit=[VACAS A SECAR POR GESTACION]
		
		sele 1
		set filt to (np>0 and fb2=B and fsec=B and stat=[CARGA])
		go top
		
	DO WHILE not eof()
			if ximprime=.F.
				exit
			endi	
	ON PAGE AT LINE Q45 do titulo2

		do titulo2

SCAN for (date()-ucal)>=(Q39-Q35)-Q8 while inkey()#6

	if ximprime=.F.
		exit
	endi	
	xid=REG.id
	xnp=REG.np
	xfpar=REG.fpar
	xduc=date()-ucal
	xdel=(date()-fpar)-(date()-fsec)
	xprm=REG.prm
	xfecha=fx(ucal+(Q39-Q35))
	xdias=(REG.ucal+(Q39-Q35))-date()

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
		?'  '+str(id,5)+cr+ab+' '+str(corr,2)+'  '+str(np,2)+'  '+stat+'  ';
		    +str(xduc,4)+'  '+str(xdel,4)+'  '+str(xdab,4)+'  '+str(ns,2);
		    +'   '+str(xval,3)+'   '+str(aval,3)+"  "+str(prx,4,1)+'    '+aw(10)+"   "+aw(9)+"   ";
		    +aw(8)+"   "+aw(7)+"   "+aw(6)+"   "+aw(5)+"   "+aw(4)+"   "+aw(3);
		    +"   "+aw(2)+"   "+aw(1)+"  "+xfecha+"  "+str(xdias,4)
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
		?'  '+repli(CHR(254),135)
		?'   TOTAL = '+STR(nvac,4)+space(3)+'PROMEDIO'+space(5);
			 		  +str(tw(1),4)+space(2)+str(tw(2),4);
			 		  +space(70)+str(tw(3),5,1)
		?
		?'   NOTA: Incluye vacas esten en Agenda de Manejo.'
ENDPRINTJOB
set prin off
*…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª
*  Cambio a Letra 12 cpi
	???&q19
*»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº
set cons on
ELSE
??CHR(7)
WAIT WIND [ IMPRESORA APAGADA O DESCONECTADA ] TIMEOUT 2
ENDI
clos all
ximprime=.t.
on page
endi

clos all
ximprime=.t.
on page
if file("DUC.CDX")
eras DUC.CDX
endi
return

*   ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
*   ≥  VACAS PROBLEMA  ≥
*   ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
PROC RV107
AST=6
@21,0 TO 21,79
@23,0 TO 23,79
@22,33 SAY [VACAS PROBLEMA] COLO gr+/&cz
@ 24,1 say [Total] colo w+/&cz
ROW=7
COL=2
DAT=0
use REG order 2
do orden
if xchoice2=2
	clos all
	return
endif	

if val(xorden)=9
@ 24,70 SAY [Espere] colo n*/w
inde on (date()-UCAL) tag DUC of DUC for NP>0 
set order to DUC
else
set order to val(xorden)
endi

SET FILT TO (NP>0 AND FB2=B) 
go top
@ 4,1 say [    ID   Fecha    Lact        Ultima    Ult  Ultimo   Ins  Estado  Prod  Corr ] colo N/w
@ 5,1 say [   Num   Parto    Num    DEL  Rev/Tx    Med  Calor    Num  Reprod  Actu  Num  ] colo N/w

SCAN FOR STAT='VACIA' OR FSEC#B AND STAT#'CARGA' OR STAT='SUCIA' ;
AND UCAL+21<DATE() AND NS>0 OR STAT='LIMPI' .AND. UCAL+21<DATE() ;
AND NS>0 OR STAT='SUCIA' AND DATE()-FPAR>Q6 .AND. UCAL+21<DATE() WHILE INKEY()#6
@ 24,8 SAY DAT+1 PICT '9999'
@ 24,70 SAY [Espere] colo n*/w
@ ROW,COL SAY ID colo gr+/&cz
*--------------------------*
if FPRO>=date() or FT2>=date() 
@ ROW,COL SAY &mensa1
ENDI
IF PSV="S"
@ ROW,COL+5 SAY &mensa2
ENDI
*--------------------------*
@ ROW,COL+8 SAY fx(FPAR)
@ ROW,COL+18 SAY NP
@ ROW,COL+23 SAY (DATE()-FPAR) pict '9999' 
@ ROW,COL+29 SAY fx(UTRA)
@ ROW,COL+39 SAY MED1
@ ROW,COL+44 SAY fx(UCAL)
@ ROW,COL+54 SAY NS
@ ROW,COL+58 SAY stat
@ ROW,COL+66 SAY prm pict '##.#' colo r+/&cz
@ ROW,COL+72 say CORR
IF ROW=20 

@ 24,70 SAY [Enter] COLO N/W
@ 24,75 SAY [ ] COLO W+/&CZ
@ 24,76 SAY CHR(16) colo w+/&cz
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
COL=2
ENDI
DAT=DAT+1
ROW=ROW+1
ENDS
IF DAT=0
@ 24,9 SAY [0]
ENDI
@ 24,60 CLEA TO 24,78
??CHR(7)
@ 24,70 SAY [Enter] COLO N/W
@ 24,75 SAY [ ] COLO W+/&CZ
@ 24,76 SAY CHR(4) colo w+/&cz
DO WHILE INKEY() # 13
ENDD
IF DAT=0
ELSE
SN='N'
@ 24,69 say [Imprimir] GET SN PICT '@M S,N' colo n/w 
READ
IF SN='N' .OR. LASTKEY()=27
ELSE
IF SN='S' .AND. SYS(13)='READY'
SET CONS OFF
SET FILT TO (NP>0 .AND. FB2=B) and FPRO<date() and FT2<date()
SET PRIN ON
GO TOP
NVE=0
PAG=1
NVAC=0
NTIT=[VACAS CON PROBLEMAS DE FERTILIDAD]
DO WHILE .NOT. EOF()
DO CTRLVRP0
?'    ID     FECHA             ULTIMO     CODIGO    NUM  DIAS   ULT.  DIAS  ESTADO   FECHA  CORRAL'
?'   VACA    PARTO      DEL    CALOR       TORO     SER  INSE   PROD  SECA  REPROD   PALP     NUM'
?'  '+REPLI('-',94)

SCAN FOR STAT='VACIA' OR FSEC#B AND STAT#'CARGA' ;
OR STAT='SUCIA' AND UCAL+21<DATE() AND NS>0 ;
OR STAT='LIMPI' AND UCAL+21<DATE() AND NS>0 ;
OR STAT='SUCIA' AND DATE()-FPAR>Q6 AND UCAL+21<DATE() WHILE INKEY()#6

IF STAT='INSEM' .OR. STAT='CARGA'
REPL DIA WITH DATE()-UCAL
ELSE
REPL DIA WITH 0
ENDI
cra=[ ]
if PSV="S"
cra=[+]
endi
?'  '+STR(ID,5)+CRA+'  '+fx(FPAR)+'   '+STR((DATE()-FPAR)-(DATE()-FSEC),4);
+'   '+fx(UCAL)+'   '+IDT+'   '+STR(NS,2)+'   '+STR(DIA,3)+'   ';
+STR(PRM,4,1)+'   '+STR((DATE()-FSEC),3)+'   '+STAT+'   '+fx(PALP)+'  ';
+STR(CORR,2)
NVE=NVE+1
NVAC=NVAC+1
IF NVE=WLI
?
?'   '+REPLI(CHR(254),94)
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

*   ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
*   ≥  VACAS POR PARIR  ≥
*   ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
PROC RV108
AST=5
@21,0 TO 21,79
@23,0 TO 23,79
@22,33 SAY [VACAS A PARIR] COLO gr+/&cz
@ 24,32 say 'Proximos '+str(Q9,3)+' Dias'
@ 24,1 say [Total] colo w+/&cz
@ 4,1 say [                         Serie   Ins       VRel  Prod  Ult    Posible   Dias  ] colo N/w
@ 5,1 say [    ID  Corral DEL  Lac  Toro    Num  DSC  Actu  Prom  Prod   Parto     Falt  ] colo N/w
ROW=7
COL=1
DAT=0
*--------
@ 24,70 SAY [Espere] colo n*/w
USE REG 
INDE ON UCAL+Q39 TO PARTOS FOR NP>0 AND STAT=[CARGA] AND FSEC#B AND UCAL+Q39<=DATE()+Q9 
USE REG INDE PARTOS
SET FILT TO (NP>0 AND FB2=B) 

@ 18,66 clea to 18,77
*--------
GO TOP
SCAN FOR UCAL+Q39<=DATE()+Q9 WHILE INKEY()#6
	xdsc=date()-fsec
	xdel=(date()-fpar)-(date()-fsec)
	xval=(m305/x305*100)
@ 24,8 SAY DAT+1 PICT '9999'
@ 24,70 SAY [Espere] colo n*/w

@ ROW,COL+2 SAY ID colo gr+/&cz
*--------------------------*
if FPRO>=date() or FT2>=date() 
@ ROW,COL+2 SAY &mensa1
ENDI
IF PSV="S"
@ ROW,COL+7 SAY &mensa2
ENDI
*--------------------------*
@ ROW,COL+10 SAY CORR 
@ ROW,COL+14 SAY xdel pict '####'
@ ROW,COL+20 SAY NP
@ ROW,COL+24 SAY IDT
@ ROW,COL+34 SAY NS
@ ROW,COL+37 SAY xdsc pict '####'
@ ROW,COL+43 SAY xval pict '####'
@ ROW,COL+49 SAY PRX  pict'##.#' colo 3+/&cz 
@ ROW,COL+55 SAY PRM  pict'##.#' colo r+/&cz 
@ ROW,COL+62 SAY fx(ucal+Q39)
@ ROW,COL+72 SAY (UCAL+Q39)-date() pict'####' colo g+/&cz

IF ROW=20 
	@ 24,70 SAY [Enter] COLO N/W
	@ 24,75 SAY [ ] COLO W+/&CZ
	@ 24,76 SAY CHR(16) colo w+/&cz
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
	COL=1
ENDI
	DAT=DAT+1
	ROW=ROW+1
ENDS
IF DAT=0
	@ 24,9 SAY [0]
ENDI
	@ 24,60 CLEA TO 24,78
	??CHR(7)
	@ 24,70 SAY [Enter] COLO N/W
	@ 24,75 SAY [ ] COLO W+/&CZ
	@ 24,76 SAY CHR(4) colo w+/&cz
DO WHILE INKEY() # 13
ENDD

IF DAT=0
ELSE
SN='N'
@ 24,69 say [Imprimir] GET SN PICT '@M S,N' colo n/w 
READ
IF SN='N' .OR. LASTKEY()=27
ELSE
IF SN='S' .AND. SYS(13)='READY'
SET CONS OFF
*----------------------------------
SET PRIN ON
NVE=0
PAG=1
NVAC=0
NTIT=[VACAS POR PARIR]
NTIT2='PROXIMOS '+str(Q9,3)+' DIAS'
go top
DO WHILE .NOT. EOF()
DO CTRLVRPT
?'     ID   CORR             SERIE    SER DIAS   PROD   ULT  VREL  PROD    POSIBLE   DIAS  INTER'
?'    VACA   NUM  DEL  LAC   TORO     NUM SECA   PROM  PROD  ACTU  305EM   PARTO     FALT  PARTO'
?'  '+REPLI('-',94)
SCAN FOR UCAL+Q39<=DATE()+Q9 WHILE INKEY()#6
CRA=[ ]
IF FB2#B
CRA=[+]
ENDI
?space(2),str(id,5,0)+CRA+'   '+str(corr,2)+'  '+STR(FSEC-FPAR,3)+'   ';
+str(np,2)+'   '+IDT+' '+STR(NS,2)+'  '+STR(DATE()-FSEC,3)+'   ';
+STR(PRX,4,1)+'  '+str(prm,4,1)+'  '+STR(M305/X305*100,4)+'  '+STR(M305,5)+'   ';
+fx(UCAL+Q39)+'  '+STR((UCAL+Q39-DATE()),4)+'   '+STR(UCAL+Q39-FPAR,3)
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
ELSE
??CHR(7)
WAIT WIND [ IMPRESORA APAGADA O DESCONECTADA ] TIMEOUT 2
ENDI
ENDI
ENDI
SET FILT TO
CLOS DATA
ERAS PARTOS.IDX


*   ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
*   ≥  VACAS EN TRATAMIENTO  ≥
*   ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
PROC RV109
XMED=0
xmnom=space(14)
@21,0 TO 21,79
@23,0 TO 23,79
@22,24 SAY [VACAS EN TRATAMIENTO REPRODUCTIVO] COLO gr+/&cz
@24,1 SAY [Total] colo w+/&cz
@24,26 SAY [Medicamento ?] GET XMED pict '###' func 'J' VALID CHKMED(XMED) .OR. XMED=0 ERROR tit(5)
@24,48 say [0=Todos] colo w+/b
@24,70 SAY [Esc] colo n/w
@24,74 say [Salir] colo w+/b

READ
IF LASTKEY()=27 
@ 5,2 CLEA TO 20,77
@ 22,1 CLEA TO 24,79
@ 5,37 SAY [Manejo] colo w+/&cz
RETU
ENDI

IF XMED>0
MM=[MED1=XMED]
ELSE
MM=[MED1>0]
XMNOM=[TODOS]
ENDI

*===================================================================*
@ 22,1 say proper(xmnom) colo r+/&cz
@ 24,1 say [Total] colo w+/&cz
@ 24,69 CLEA TO 24,79
@ 24,70 SAY [Espere] colo n*/w
SELE 1
ROW=7
COL=5
DAT=0
@ 4,1 say [                                                                              ] colo n/w
@ 5,1 say [      ID  Corral  Medic        ID  Corral  Medic        ID  Corral  Medic     ] colo n/w
use REG order 2
GO TOP
SET FILT TO (NP>0 AND FB2=B)

SCAN FOR STAT='TRATA' AND &MM WHILE INKEY()#6
@ 24,8 SAY DAT+1 PICT '9999'
@ 24,70 SAY [Espere] colo n*/w
@ ROW,COL-1 SAY ID colo gr+/&cz
*--------------------------*
if FPRO>=date() or FT2>=date() 
@ ROW,COL-1 SAY &mensa1
ENDI
IF PSV="S"
@ ROW,COL+4 SAY &mensa2
ENDI
*--------------------------*
@ ROW,COL+8 SAY CORR
@ ROW,COL+15 SAY MED1

IF ROW=20 .AND. COL=55  
@ 24,70 SAY [Enter] COLO N/W
@ 24,75 say [ ] colo w/&cz
@ 24,76 SAY CHR(16) colo w+/&cz
DO WHILE INKEY() # 13
IF LASTKEY()=6
@ 4,1 CLEA TO 20,78
GO BOTTOM
EXIT
ENDI
ENDD
@ 6,2 CLEA TO 20,77
ROW=6
COL=5
ENDI
IF ROW=20
ROW=6
COL=COL+25
ENDI
DAT=DAT+1
ROW=ROW+1
ENDS
IF DAT=0
@ 24,9 SAY [0]
ENDI
??CHR(7)
@ 24,70 SAY [Enter] COLO N/W
@ 24,75 say [ ] colo w/&cz
@ 24,76 SAY CHR(4) colo w+/&cz
DO WHILE INKEY() # 13
ENDD
IF DAT=0
ELSE
SN='N'
@ 24,69 say [Imprimir] GET SN PICT '@M S,N' colo n/w 
READ
IF SN='N' .OR. LASTKEY()=27
ELSE
IF SN='S' .AND. SYS(13)='READY'
SET CONS OFF
SET PRIN ON
NVE=0
PAG=1
NVAC=0
NTIT=[VACAS EN TRATAMIENTO   MEDIC= ]+XMNOM
GO TOP
DO WHILE .NOT. EOF()
DO CTRLVRP0
?'      ID   FECHA            ULTIMO    MEDIC     ULTIMO   NUM   CODIGO       PROD  CORRAL'
?'    VACA   PARTO      DEL   TRATAM    APLIC     CALOR    SER    TORO         DIA    NUM'
?'  '+REPLI('-',94)
SCAN FOR STAT='TRATA' and FT2<date() and FPRO<date() AND &MM WHILE INKEY()#6
REPL DPR WITH (DATE()-FPAR)-(DATE()-FSEC)
	cra=[ ]
	if PSV="S"
		cra=[+]
	endi
?space(2),str(id,5,0)+CRA+' '+fx(FPAR)+'   '+STR(DPR,4)+'   '+fx(UTRA)+'   '+STR(MED1,3);
+'      '+fx(UCAL)+'  '+STR(NS,2)+'   '+IDT+'    '+STR(PRX,4,1)+'    '+STR(CORR,2)

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
CLOS DATA

*   ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
*   ≥  VACAS DE BAJA PRODUCCION CON LAS ULTIMAS SIETE PESADAS       ≥
*   ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
PROC RV1010
declare aw(60),pw(16),tw(10)
nvac=0
row=7
sn='N'
mc=0
xpag=0
pw=0
tw=0
mstatus=space(8)
mdel=1
mprod=Q1/Q2
xfilt=""

@21,0 TO 21,79
@23,0 TO 23,79
@22,28 SAY [VACAS DE BAJA PRODUCCION] COLO gr+/&cz
@24,1 SAY [Total] colo w+/&cz
@24,70 SAY [Esc] colo n/w
@24,74 say [Salir] colo w+/b

use reg  order 2 alias REG  in 1
use prod order 2 alias PROD in 2 
*
@24,23 get mstatus Pict "@M Abiertas,Cargadas,Todas"
@24,33 say [DEL >=] get mdel pict "###" func"j,t"
@24,45 say [Prod <=] get mprod pict "##.#" func "j,t"
read
	if lastkey()=27
		clos all
		retu
	else

sele 1
do case
	case mstatus=[Abiertas]
		xfilt="(np>0 and fb2=B and fsec=B and stat#[CARGA] ;
		and prm>0 and prm<=mprod and hoy-fpar>=mdel)"
	case mstatus=[Cargadas]
		xfilt="(np>0 and fb2=B and fsec=B and stat=[CARGA] ;
		and prm>0 and prm<=mprod and hoy-fpar>=mdel)"
	case mstatus=[Todas]
		xfilt="(np>0 and fb2=B and fsec=B ;
		and prm>0 and prm<=mprod and hoy-fpar>=mdel)"
endcase
	endi
	set filt to &xfilt
	go top
		
@ 24,69 clea to 24,79
@ 4,1 say [                                 VRel  VRel  Prod        Ultimas Pesadas      ] colo N/w
@ 5,1 say [    ID Cor Lac    DUC  DEL  DAB  Prom  Actu  Prom   7   6   5   4   3   2   1 ] colo N/w

go top
SCAN while inkey()#6

	@24,8 say nvac+1 pict"####"
	xid=REG.id
	xnp=REG.np
	xfpar=REG.fpar
	xduc=date()-ucal
	xdel=(date()-fpar)-(date()-fsec)
	xprm=REG.prm

*if xduc>=(Q39-Q35) and REG.stat=[CARGA]  && Para no incluir vacas a secarse por gestacion.
*else
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
IF ROW=20
	@ 24,69 clea to 24,79
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
*endi
endscan

	??chr(7)
	@ 24,70 say [Enter] colo n/w
	@ 24,76 say chr(4) colo w+/&cz
	do while inkey()#13
	endd
	sn="N"
	@ 24,69 say [Imprimir] get SN pict '@M S,N' colo n/w 
	read
	if SN='N' or LASTKEY()=27
	else
	** Checa si esta encendida la impresora
	** ------------------------------------	
	IF sys(13)='READY'
		nvac=0
		sn='N'
		xpag=0
		pw=0
		tw=0
		@ 24,67 clea to 24,79
		@ 24,70 SAY [Espere] colo n*/w
		*…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª
		*  Cambio a Letra 17 cpi
			???&q21
		*»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº
		set console off
		set printer on
		_peject="AFTER"
		_padvance="LINEFEED"
*		_plength=60
		_plineno=0
		_pageno=1
		xpag=1			
		ximprime=.t.
	PRINTJOB
			nvac=0
			ntit=[VACAS DE BAJA PRODUCCION]
			ntit2=mstatus+"  DEL >= "+str(mdel,3)+"  Prod. <= "+str(mprod,4,1)
			sele 1
			set filt to &xfilt
			go top
			
	DO WHILE not eof()
			if ximprime=.F.
				exit
			endi	
	ON PAGE AT LINE Q45 do titulo
*		?'  '+repli(CHR(254),135)
		do titulo

SCAN while inkey()#6

	if ximprime=.F.
		exit
	endi	
	xid=REG.id
	xnp=REG.np
	xfpar=REG.fpar
	xduc=date()-ucal
	xdel=(date()-fpar)-(date()-fsec)
	xprm=REG.prm

*if xduc>=(Q39-Q35) and REG.stat=[CARGA]  && Para no incluir vacas a secarse por gestacion.
*else
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
*endi
endscan
ENDD
		?'  '+repli(CHR(254),135)
		?'   TOTAL = '+STR(nvac,4)+space(3)+'PROMEDIO'+space(5);
			 		  +str(tw(1),4)+space(2)+str(tw(2),4)+space(68);
			 		  +str(tw(3),3)+space(12)+str(tw(4),4,1)+space(2);
					  +str(tw(5),3)	
		?
		?'   NOTA: Incluye vacas esten en Agenda de Manejo.'
ENDPRINTJOB
*…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª
*  Cambio a Letra 12 cpi
	???&q19
*»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº
set prin off
set cons on
ELSE
??CHR(7)
WAIT WIND [ IMPRESORA APAGADA O DESCONECTADA ] TIMEOUT 2
ENDI
clos all
*on escape
ximprime=.t.
on page
endi

clos all
*on escape
*set esca off
ximprime=.t.
on page
return

*   ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
*   ≥  VACAS A PROBABLE CALOR   ≥
*   ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
PROC RV1011
STORE ctod("") TO FPC1
@21,0 TO 21,79
@23,0 TO 23,79
@22,28 SAY [VACAS A PROBABLE CALOR] COLO gr+/&cz
@24,1 SAY [Total] colo w+/&cz
@24,70 SAY [Esc] colo n/w
@24,74 say [Salir] colo w+/b

@24,31 SAY [Fecha ] get FPC1 valid FPC1>DATE()-365  .AND. FPC1<=date()+7 .OR. FPC1=B error tit(3)
read
IF LASTKEY()=27 .or. FPC1=B
	return
ENDI
xchoice=1
xchoice2=2
xfilt=""
xtit=""
	defi wind win12 from 8,21 to 16,59 title [ Ordenado por : ]  shadow colo sche 16
	acti wind win12
	@ 1,2 get xchoice func'*R Identificaci¢n;Corral;Estado Reproductivo' colo ,,,,,gr+/b,,,w+/w
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
		case xchoice=3
			xfilt="STAT,CORR,ID"
			xtit="x Estado"
	endcase	
@22,2 say xtit colo w+/b
select id,fpar,ucal,stat,corr from REG where NP>0 and fb2=B;
	   and stat#"CARGA" order by &xfilt into cursor INSEM

@ 24,68 clea to 24,78
@ 24,70 SAY [Espere] colo n*/w

@ 4,1 say [                                                                              ] colo n/w
@ 5,1 say [          ID  Corral  Edo Rep  DUC          ID  Corral  Edo Rep  DUC          ] colo n/w
ROW=7
COL=9
DAT=0

SCAN FOR UCAL+18=FPC1 AND STAT='LIMPI' OR UCAL+19=FPC1 AND STAT='LIMPI' OR UCAL+20=FPC1 AND STAT='LIMPI' OR UCAL+21=FPC1 AND STAT='LIMPI' OR UCAL+22=FPC1 AND STAT='LIMPI' OR UCAL+23=FPC1 AND STAT='LIMPI' OR UCAL+24=FPC1 AND STAT='LIMPI';
OR UCAL+18=FPC1 AND STAT='SUCIA' OR UCAL+19=FPC1 AND STAT='SUCIA' OR UCAL+20=FPC1 AND STAT='SUCIA' OR UCAL+21=FPC1 AND STAT='SUCIA' OR UCAL+22=FPC1 AND STAT='SUCIA' OR UCAL+23=FPC1 AND STAT='SUCIA' OR UCAL+24=FPC1 AND STAT='SUCIA';
OR UCAL+18=FPC1 AND STAT='LIMPI' OR UCAL+19=FPC1 AND STAT='INSEM' OR UCAL+20=FPC1 AND STAT='INSEM' OR UCAL+21=FPC1 AND STAT='INSEM' OR UCAL+22=FPC1 AND STAT='INSEM' OR UCAL+23=FPC1 AND STAT='INSEM' OR UCAL+24=FPC1 AND STAT='INSEM'WHILE INKEY()#6
@ 24,8 SAY DAT+1 PICT '9999'
@ 24,70 SAY [Espere] colo n*/w
@ ROW,COL-1 SAY ID colo gr+/&cz
@ ROW,COL+9 say CORR
@ ROW,COL+15 SAY STAT
@ ROW,COL+24 SAY 21-(UCAL+21-FPC1) pict '99'
IF ROW=20 .AND. COL=43  
@ 24,70 SAY [Enter] COLO N/W
@ 24,75 say [ ] colo w/&cz
@ 24,76 SAY CHR(16) colo w+/&cz
DO WHILE INKEY() # 13
IF LASTKEY()=6
@ 4,1 CLEA TO 20,78
GO BOTTOM
EXIT
ENDI
ENDD
@ 6,2 CLEA TO 20,77
ROW=6
COL=9
ENDI
IF ROW=20
ROW=6
COL=COL+34
ENDI
DAT=DAT+1
ROW=ROW+1
ENDS
IF DAT=0
@ 24,9 SAY [0]
ENDI
@ 24,60 CLEA TO 24,78
??CHR(7)
@ 24,70 SAY [Enter] COLO N/W
@ 24,75 say [ ] colo w/&cz
@ 24,76 SAY CHR(4) colo w+/&cz
DO WHILE INKEY() # 13
ENDD
IF DAT=0
ELSE
SN='N'
@ 24,69 say [Imprimir] GET SN PICT '@M S,N' colo n/w 
READ
IF SN='N' .OR. LASTKEY()=27
ELSE
IF SN='S' .AND. SYS(13)='READY'
SET CONS OFF
SET PRIN ON
NVE=0
PAG=1
NVAC=0
NTIT=[VACAS PROBABLE CALOR]
NTIT2=DTOC(FPC1)
GO TOP
DO WHILE .NOT. EOF()
DO CTRLVRPT
?'     ID       CORRAL      DIA     ESTADO     DIAS       CALOR          HORA         REPORTO '
?'    VACA      NUMERO      NUM     REPROD     LECHE      SI/NO'

?'  '+REPLI('-',94)

SCAN WHILE .NOT. EOF() FOR UCAL+18=FPC1 .AND. STAT='LIMPI' .OR. UCAL+19=FPC1 .AND. STAT='LIMPI' .OR. UCAL+20=FPC1 .AND. STAT='LIMPI' .OR. UCAL+21=FPC1 .AND. STAT='LIMPI' .OR. UCAL+22=FPC1 .AND. STAT='LIMPI' .OR. UCAL+23=FPC1 .AND. STAT='LIMPI' .OR. UCAL+24=FPC1 .AND. STAT='LIMPI';
.OR. UCAL+18=FPC1 .AND. STAT='SUCIA' .OR. UCAL+19=FPC1 .AND. STAT='SUCIA' .OR. UCAL+20=FPC1 .AND. STAT='SUCIA' .OR. UCAL+21=FPC1 .AND. STAT='SUCIA' .OR. UCAL+22=FPC1 .AND. STAT='SUCIA' .OR. UCAL+23=FPC1 .AND. STAT='SUCIA' .OR. UCAL+24=FPC1 .AND. STAT='SUCIA';
.OR. UCAL+18=FPC1 .AND. STAT='LIMPI' .OR. UCAL+19=FPC1 .AND. STAT='INSEM' .OR. UCAL+20=FPC1 .AND. STAT='INSEM' .OR. UCAL+21=FPC1 .AND. STAT='INSEM' .OR. UCAL+22=FPC1 .AND. STAT='INSEM' .OR. UCAL+23=FPC1 .AND. STAT='INSEM' .OR. UCAL+24=FPC1 .AND. STAT='INSEM'

?space(2),str(id,5,0)+'        '+STR(CORR,2)+'         '+STR((21-(UCAL+21-FPC1)),2)+'      '+STAT+'     '+STR((DATE()-FPAR),4)+'      _______        _______      __________  '
?
NVE=NVE+1
NVAC=NVAC+1
IF NVE=23
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
ENDI
CLOS DATA
erase INSEM.DBF
return

*   ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
*   ≥  RESUMEN DE VACAS (NUEVO) ≥
*   ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
PROC RV1021
clos all
@21,0 TO 21,79
@23,0 TO 23,79
@22,31 SAY [RESUMEN DE DATOS] COLO gr+/&cz
@24,29 say [* Solo en Impresora *]colo r+/&cz	
@24,70 SAY [Esc] colo n/w
@24,74 say [Salir] colo w+/b
@24,1 say [Total] colo w+/b

*
@ 4,1 say [                                                                              ] colo n/w
@ 5,1 SAY [     ID Corral      ID Corral      ID Corral      ID Corral      ID Corral    ] colo n/w

	create table DAT (id N(5),corr N(2))
	use reg order 2 alias REG in 1
	use DAT in 3

	xid=0
	row=7
	col=3
	xco=0
	xdias=1
	nvac=0
	sele 1
	axid=0
	do while .t.
		XID=0
		@22,70 say [IDa] colo n/w 
		@22,74 say axid pict'#####'
		@22,1 say [ID] get xid valid seek(xid) pict [#####] func 'j,t' error [ID no existe ] colo w+/&cz
		read
		axid=xid
		if lastkey()=27
			exit
		else
		xco=REG.CORR
		sele 3
		locate for id=xid
		if not found() &&seek(xid)
		@row,col say xid pict "#####" colo gr+/&cz
		@row,col+8 say xco pict"##"
		appe blank
		repl id with xid,corr with REG.CORR
		row=row+1
		nvac=nvac+1
		@24,8 say nvac pict'###' colo w+/&cz
			do case
				case row=19 and col#63
				row=7
				col=col+15
				case row=19 and col=63
				@7,1 clea to 20,78
				row=7
				col=3
			endcase
		else

		??chr(7)
		sn="N"
		@22,59 say [ Repetida  Borrar ] get sn pict'@M N,S' colo n/w
		read
		if sn="S"		
		delete
		@7,0 clea to 20,79
		row=7
		col=3
		nvac=0
		scan
			@row,col say id pict "#####" colo gr+/&cz
			@row,col+8 say corr pict"##"
		
			row=row+1
			nvac=nvac+1
			@24,8 say nvac pict'###' colo w+/&cz
			do case
				case row=19 and col#63
					row=7
					col=col+15
				case row=19 and col=63
					@7,1 clea to 20,78
					row=7
					col=3
			endcase
		endscan
		else
		endif
		@22,58 clea to 22,79
		endif
		endif
	sele 1
	enddo	
*		
**  Salida para Impresora
** ----------------------
sn='N'
@ 24,67 clea to 24,79
@ 24,69 say [Imprimir] GET SN PICT '@M S,N' colo n/w
read
if sn='N' or lastkey()=27
else
		if sys(13)='READY'

			*…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª
			*  Cambio a Letra 17 cpi
				???&q21
			*»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº
			set console off
		    set printer on
	xpag=1			
	ximprime=.T.

	sele 3
	index on ID to DAT
	go top

			nvac=0
			ntit=[RESUMEN DE VACAS]

do titulo3
_plineno=0

do while not eof()

		DO WHILE not eof()
			sele 3
			xid=DAT.id
			
			if ximprime=.F.
				exit
			endi	

			if _plineno=Q45
				do titulo3
				_plineno=0
			endi	

			sele 1
			seek xid
			xid=REG.id
			xnp=REG.np
			xduc=date()-ucal
			xdel=(date()-fpar)-(date()-fsec)
			xdab=0
			if stat=[INSEM] or stat=[CARGA]
			xdab=ucal-fpar
			else
			xdab=date()-fpar
			endi
			cra=" "
			if PSV="S"								
				cra="+"
			endi	
if xdel>=xdias
		** -----------------------------------------------------------
		?'  '+str(id,5)+cra+' '+str(corr,2)+'  '+fx(fpar)+' ';
		    +str(xdel,4)+'  '+stat+'  '+str(xduc,4)+' '+str(xdab,4);
		    +'  '+fx(utra),+'  '+fx(ucal)+' '+str(med1,3);
		    +'  '+str(ns,2)+'  '+str(tota,4,1)+'  '+str(prm,4,1)+'  ';
		    +obsc+'  '+fx(fpro)+'  '+medp
		** -----------------------------------------------------------
		sele 3
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
*…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª
*  Cambio a Letra 12 cpi
	???&q19
*»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº
set prin off
set cons on
SET FILT TO
ELSE
??CHR(7)
WAIT WIND [ IMPRESORA APAGADA O DESCONECTADA ] TIMEOUT 2
endi
endi
rele wind win12
clos all
eras DAT.dbf
eras DAT.idx
ximprime=.T.
on page
on key label F10 *
return


*   ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
*   ≥  VACAS DE PROBABLE DESECHO CON HISTORIAL DE PESADAS       ≥
*   ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
PROC RV1016
declare aw(60),pw(16),tw(10)
nvac=0
row=7
sn='N'
mc=0
xpag=0
pw=0
tw=0

@21,0 TO 21,79
@23,0 TO 23,79
@22,27 SAY [VACAS DE PROBABLE DESECHO] COLO gr+/&cz
@24,1 SAY [Total] colo w+/&cz
@24,70 SAY [Esc] colo n/w
@24,74 say [Salir] colo w+/b

use reg  order 2 alias REG  in 1
use prod order 2 alias PROD in 2 
sele 1
go top

@ 24,69 clea to 24,79
@ 4,1 say [                                 VRel  VRel  Prod     Ultimas Pesadas         ] colo N/w
@ 5,1 say [    ID Cor Lac    DUC  DEL  DAB  Prom  Actu  Prom    5   4   3   2   1  Causa ] colo N/w

SCAN for (np>0 and PSV="S" and mot2=space(6)) while inkey()#6

	@24,8 say nvac+1 pict"####"
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

@ row,53 say aw(5) pict '##' colo r+/&cz
@ row,57 say aw(4) pict '##' colo r+/&cz
@ row,61 say aw(3) pict '##' colo r+/&cz
@ row,65 say aw(2) pict '##' colo r+/&cz
@ row,69 say aw(1) pict '##' colo r+/&cz

@ row,75 say cau2  pict '##' colo g+/&cz
IF ROW=20
	@ 24,69 clea to 24,79
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

endscan

	??chr(7)
	@ 24,70 say [Enter] colo n/w
	@ 24,76 say chr(4) colo w+/&cz
	do while inkey()#13
	endd
	sn="N"
	@ 24,69 say [Imprimir] get SN pict '@M S,N' colo n/w 
	read
	if SN='N' or LASTKEY()=27
	else
	** Checa si esta encendida la impresora
	** ------------------------------------	
	IF sys(13)='READY'
		nvac=0
		sn='N'
		xpag=0
		pw=0
		tw=0
		@24,67 clea to 24,79
		@ 24,70 SAY [Espere] colo n*/w
			
		set console off
		set printer on
		*…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª
		*  Cambio a Letra 17 cpi
			???&q21
		*»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº
		_peject="AFTER"
		_padvance="LINEFEED"
*		_plength=60
		_plineno=0
		_pageno=1
		xpag=1			
		ximprime=.t.
	PRINTJOB
			nvac=0
			ntit=[VACAS DE PROBABLE DESECHO]
			sele 1
			go top
		
	DO WHILE not eof()
			if ximprime=.F.
				exit
			endi	
	ON PAGE AT LINE Q45 do titulo1

		do titulo1

SCAN for (np>0 and PSV="S" and mot2=space(6)) while inkey()#6

	if ximprime=.F.
		exit
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
		    +"   "+aw(2)+"   "+aw(1)+"  "+fx(pesa)+"  "+str(plac,4,1)+"  "+str(cau2,3)
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
		?'  '+repli(CHR(254),135)
		?'   TOTAL = '+STR(nvac,4)+space(3)+'PROMEDIO'+space(5);
			 		  +str(tw(1),4)+space(2)+str(tw(2),4)+space(70);
			 		  +str(tw(3),3)+space(12)+str(tw(4),4,1)
		?
		?'   NOTA: Incluye vacas esten en Agenda de Manejo.'
ENDPRINTJOB
*…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª
*  Cambio a Letra 12 cpi
	???&q19
*»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº
set prin off
set cons on
ELSE
??CHR(7)
WAIT WIND [ IMPRESORA APAGADA O DESCONECTADA ] TIMEOUT 2
ENDI
clos all

ximprime=.t.
on page
endi

clos all
ximprime=.t.
on page
return

*   ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
*   ≥  VACAS EN HOSPITAL      ≥
*   ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
PROC RV1017
PUBLIC XNOMBRE,XPO
DO WHILE .T.
XENF=0

@ 4,0 CLEA TO 24,79
@21,0 TO 21,79
@23,0 TO 23,79
@22,32 SAY [VACAS EN HOSPITAL] COLO gr+/&cz
@24,1 SAY [Total] colo w+/&cz
@24,70 SAY [Esc] colo n/w
@24,74 say [Salir] colo w+/b

@24,31 SAY [Enfermedad Num. :] GET XENF PICT '99' func 'j' VALID CHENF2(XENF) .OR. XENF=0 ERROR tit(5)
read
if lastkey()=27 
retu
endi

IF XENF>0
MM=[ENF=XENF]
ELSE
MM=[ENF>0]
XNOMBRE=[Todas]
ENDI

@ 24,69 CLEA TO 24,79
@ 22,2 SAY PROPER(xNOMBRE) COLO r+/&CZ
@ 24,1 say [Total] colo w+/&cz
@ 24,70 SAY [Espere] colo n*/w

@ 4,1 say [                                                                              ] colo n/w
@ 5,1 say [        ID    Enf  Corral     ID    Enf  Corral     ID    Enf  Corral         ] colo n/w
ROW=7
COL=8
DAT=0
use REG order 2
GO TOP
SET FILT TO (NP>0 AND FB2=B)
SCAN FOR HOSP=1 AND &MM WHILE INKEY()#6
@ 24,8 SAY DAT+1 PICT '9999'
@ 24,70 SAY [Espere] colo n*/w

@ ROW,COL-1 SAY ID PICT '99999' colo gr+/&cz
*--------------------------*
if FPRO2>=date() or FT4>=date() 
@ ROW,COL-1 SAY &mensa1
ENDI
IF PSV="S"
@ ROW,COL+4 SAY &mensa2
ENDI
*--------------------------*
@ ROW,COL+8 SAY ENF PICT '99'
@ ROW,COL+14 SAY CORR PICT '99'

IF ROW=20 .AND. COL=52
@ 24,70 SAY [Enter] COLO N/W
@ 24,75 say [ ] colo w/&cz
@ 24,76 SAY CHR(16) colo w+/&cz
DO WHILE INKEY() # 13
IF LASTKEY()=6
@ 4,1 CLEA TO 20,78
GO BOTTOM
EXIT
ENDI
ENDD
@ 6,2 CLEA TO 20,77
@ 24,58 CLEA TO 24,79
ROW=6
COL=8
ENDI
IF ROW=20
ROW=6
COL=COL+22
ENDI
DAT=DAT+1
ROW=ROW+1
ENDS
IF DAT=0
@ 24,9 SAY [0]
ELSE
CALC CNT() FOR FPAR#B and FB2=B TO NUMV
XPO=(DAT/NUMV)*100
@ 24,15 SAY (DAT/NUMV)*100 PICT '99.9' FUNC 'J' colo w+/&cz
@ 24,20 SAY '%' colo w+/&cz
ENDI
??CHR(7)
@ 24,70 SAY [Enter] COLO N/W
@ 24,75 say [ ] colo w/&cz
@ 24,76 SAY CHR(4) colo w+/&cz
DO WHILE INKEY() # 13
ENDD
IF DAT=0
ELSE
SN='N'

@ 24,69 say [Imprimir] GET SN PICT '@M S,N' colo n/w
READ
IF SN='N' .OR. LASTKEY()=27
ELSE
IF SN='S' .AND. SYS(13)='READY'
NTIT=[REPORTE DE HOSPITAL  ENF.-> ]+XNOMBRE
@24,69 clea to 27,79
@24,69 SAY [Espere] colo n*/w
use REG order 2
GO TOP
COPY TO LOTES FIELDS ID,ENF,CORR FOR FPAR#B and FB2=B and HOSP=1 and &MM
CLOS ALL
SELE 2
USE CAMBIO
SELE 1
USE LOTES
@ 22,67 CLEA TO 22,77
DO R6 WITH 4,(WLI),15,STR(ID,5)+STR(ENF,2)+STR(CORR,2)
SET PRIN OFF
ELSE
??CHR(7)
WAIT WIND [ IMPRESORA APAGADA O DESCONECTADA ] TIMEOUT 2
ENDI
ENDI
ENDI
ENDD
RELE XNOMBRE,XPO
CLOS DATA

*   ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
*   ≥  VACAS POR ULTIMA PRODUCCION  ≥
*   ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
PROC RV1018
STORE 0 TO PR1,PR2
@ 4,0 CLEA TO 24,79
@21,0 TO 21,79
@23,0 TO 23,79
@22,28 SAY [VACAS POR ULTIMA PRODUCCION] COLO gr+/&cz
@24,1 SAY [Total] colo w+/&cz
@24,70 SAY [Esc] colo n/w
@24,74 say [Salir] colo w+/b

@24,32 SAY [De ] get PR1 PICT '99.9' FUNC 'J' valid PR1>0 .AND. PR1<90 ERROR [ Dato No Valido Rango 1-60 ] 
@24,43 SAY [a ] get PR2 PICT '99.9' FUNC 'J' valid PR2>=PR1 .AND. PR2<90 ERROR [ Dato No Valido Rango 1-60 ] 
READ
IF LASTKEY()=27
RETU
ENDI
@ 24,68 clea to 24,79
@ 4,1 say [                                                                              ] colo n/w
@ 5,1 say [      ID    Parto DEL  U.Prod  Corral       ID    Parto DEL  U.Prod  Corral   ] colo n/w
ROW=7
COL=4
DAT=0
@ 24,70 SAY [Espere] colo n*/w

USE REG 
inde on str(prm,4,1)+str(ID,5) for np>0 and fsec=b and fb2=b to reg2 
use reg inde reg2
set filt to prm>0 and prm>=PR1 and prm<=PR2

@ 24,60 clea to 24,79
SCAN WHILE INKEY()#6
@ 24,8 SAY DAT+1 PICT '9999'
@ 24,70 SAY [Espere] colo n*/w
@ ROW,COL SAY ID colo gr+/&cz
@ row,col+7 say left(stat,1) colo w+/&cz
	if stat=[ABORT]
		@ ROW,COL+8 SAY [B] COLO W+/&CZ
	endi
@ ROW,COL+10 SAY np 
@ ROW,COL+15 SAY DATE()-FPAR PICT '999'
@ ROW,COL+21 SAY PRM PICT '99.9'colo r+/&cz
@ ROW,COL+29 SAY CORR PICT '99'


IF ROW=20 .AND. COL=42  
@ 24,70 clea to 24,79
@ 24,70 SAY [Enter] COLO N/W
@ 24,75 say [ ] colo w/&cz
@ 24,76 SAY CHR(16) colo w+/&cz
DO WHILE INKEY() # 13
IF LASTKEY()=6
@ 4,1 CLEA TO 20,78
GO BOTTOM
EXIT
ENDI
ENDD
@ 6,2 CLEA TO 20,79
@ 24,58 CLEA TO 24,79
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
ENDI
@ 24,70 clea to 24,79
@ 24,70 SAY [Enter] COLO N/W
@ 24,75 say [ ] colo w/&cz
@ 24,76 SAY CHR(4) colo w+/&cz
DO WHILE INKEY() # 13
ENDD

IF DAT=0
ELSE
SN='N'
@ 24,69 say [Imprimir] GET SN PICT '@M N,S' colo n/w
READ
IF SN='N' .OR. LASTKEY()=27
ELSE
IF SN='S' .AND. SYS(13)='READY'
NTIT=[VACAS POR PRODUCCION DE ACUERDO A SU ULTIMA PESADA]
NTIT2='DE '+STR(PR1,4,1)+' A '+STR(PR2,4,1)
NT2=[Kilos]
@ 22,67 SAY [Espere] colo n*/w
GO TOP
COPY TO LOTES FIELDS ID,fpar,np,STAT,PRM,CORR FOR FPAR#B and FSEC=B and ;
FB2=B and PRM>=PR1 and PRM<=PR2 
CLOS ALL
SELE 2
USE CAMBIO
SELE 1
USE LOTES
@ 22,67 CLEA TO 22,77
DO O2 WITH 2,(WLI),36,STR(ID,5)+str(np,8)+STR((DATE()-FPAR),3)+STAT+STR(PRM,4,1)+STR(CORR,2)
SET PRIN OFF
ELSE
??CHR(7)
WAIT WIND [ IMPRESORA APAGADA O DESCONECTADA ] TIMEOUT 2
ENDI
ENDI
ENDI
CLOS DATA

*   ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
*   ≥  VACAS POR DIAS EN LECHE      ≥
*   ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
PROC RV1019
STORE 0 TO DL1,DL2
@ 4,0 CLEA TO 24,79
@21,0 TO 21,79
@23,0 TO 23,79
@22,28 SAY [VACAS POR DIAS EN LECHE] COLO gr+/&cz
@24,1 SAY [Total] colo w+/&cz
@24,70 SAY [Esc] colo n/w
@24,74 say [Salir] colo w+/b
@24,32 SAY [De ] get DL1 PICT '9999' FUNC 'J' valid DL1>0 .AND. DL1<=1095 ERROR [ Dato No Valido Rango 1-1095 ] 
@24,43 SAY [a ] get DL2 PICT '9999' FUNC 'J' valid DL2>=DL1 .AND. DL2<=1095 ERROR [ Dato no Valido Rango 1-1095 ]
READ
IF LASTKEY()=27
RETU
ENDI
@ 24,68 clea to 24,79
@ 24,70 SAY [Espere] colo n*/w
@ 4,1 say [                                                                              ] colo n/w
@ 5,1 say [      ID    Lact  DEL  U.Prod  Corral       ID    Lact  DEL  U.Prod  Corral   ] colo n/w
ROW=7
COL=4
DAT=0
@ 24,70 SAY [Espere] colo n*/w
USE REG order 2 

CALC CNT() FOR NP>0 and FB2=B TO NUMV

inde on str(date()-fpar,5)+str(ID,5) FOR np>0 and fsec=b and fb2=b to REG2

USE REG inde reg2
set filt to date()-fpar>=DL1 and date()-fpar<=DL2
go top

SCAN WHILE INKEY()#6
@ 24,8 SAY DAT+1 PICT '9999'
@ 24,70 SAY [Espere] colo n*/w
@ ROW,COL SAY ID colo gr+/&cz
@ row,col+7 say left(stat,1) colo w+/&cz
	if stat=[ABORT]
		@ ROW,COL+8 SAY [B] COLO W+/&CZ
	endi
@ ROW,COL+10 SAY np 
@ ROW,COL+15 SAY DATE()-FPAR PICT '999'
@ ROW,COL+21 SAY PRM PICT '99.9' colo r+/&cz
@ ROW,COL+29 SAY CORR PICT '99'

IF ROW=20 .AND. COL=42  
@ 24,70 clea to 24,79
@ 24,70 SAY [Enter] COLO N/W
@ 24,75 say [ ] colo w/&cz
@ 24,76 SAY CHR(16) colo w+/&cz
DO WHILE INKEY() # 13
IF LASTKEY()=6
@ 4,1 CLEA TO 20,78
GO BOTTOM
EXIT
ENDI
ENDD
@ 6,2 CLEA TO 20,79
@ 24,58 CLEA TO 24,79
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
IF DAT=0 .or. lastkey()=6
@ 24,9 SAY [0]
ELSE
@ 24,15 SAY (DAT/NUMV)*100 PICT '99.9' FUNC 'J' colo w+/&cz
@ 24,20 SAY '%' colo w+/&cz
ENDI
@ 24,70 clea to 24,79
@ 24,70 SAY [Enter] COLO N/W
@ 24,75 say [ ] colo w/&cz
@ 24,76 SAY CHR(4) colo w+/&cz
DO WHILE INKEY() # 13
ENDD
IF DAT=0
ELSE
SN='N'
@ 24,69 say [Imprimir] GET SN PICT '@M N,S' colo n/w
READ
IF SN='N' .OR. LASTKEY()=27
ELSE
IF SN='S' .AND. SYS(13)='READY'
NTIT=[VACAS POR DIAS EN LECHE]
NTIT2='DE '+STR(DL1,4)+' A '+STR(DL2,4)+' DIAS'
NT2=[Dias]
@ 24,68 clea to 24,79
@ 24,70 say [Espere] colo n*/w
GO TOP
COPY TO LOTES FIELDS ID,np,fpar,PRM,CORR,STAT FOR FPAR#B and FSEC=B and ;
FB2=B and DATE()-FPAR>=DL1 and DATE()-FPAR<=DL2 
CLOS ALL
SELE 2
USE CAMBIO
SELE 1
USE LOTES
@ 22,67 CLEA TO 22,77
DO O2 WITH 2,(WLI),36,STR(ID,5)+str(np,8)+STR((DATE()-FPAR),3)+STAT+STR(PRM,4,1)+STR(CORR,2)
SET PRIN OFF
ELSE
??CHR(7)
WAIT WIND [ IMPRESORA APAGADA O DESCONECTADA ] TIMEOUT 2
ENDI
ENDI
ENDI
CLOS DATA

*   ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
*   ≥  VACAS CON MAS DE nDIAS Y ABIERTAS  ≥
*   ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
PROC RV1020
STORE 0 TO DL1,DL2
@ 4,0 CLEA TO 24,79
@21,0 TO 21,79
@23,0 TO 23,79
@22,33 SAY [VACAS ABIERTAS] COLO gr+/&cz
@24,1 SAY [Total] colo w+/&cz
@24,70 SAY [Esc] colo n/w
@24,74 say [Salir] colo w+/b

@ 24,31 SAY [De] get DL1 PICT '9999' FUNC 'J' valid DL1>0 .AND. DL1<=1095 ERROR [ Dato No Valido Rango 1-1095 ] 
@ 24,39 SAY [a] get DL2 PICT '9999' FUNC 'J' valid DL2>=DL1 .AND. DL2<=1095 ERROR [ Dato No Valido Rango 1-1095 ]
@ 24,46 SAY [DEL]
READ
IF LASTKEY()=27
RETU
ENDI

*--------
use REG order 2
do orden
if xchoice2=2
	clos all
	return
endif	

if val(xorden)=9
@ 24,70 SAY [Espere] colo n*/w
inde on (date()-UCAL) tag DUC of DUC for NP>0 
set order to DUC
else
set order to val(xorden)
endi

*---------

@ 24,68 clea to 24,79
@ 24,70 SAY [Espere] colo n*/w
@ 4,1 say [                        Fecha    Edo  Ult   Ultima   Ult  Ins  Ultimo         ] colo n/w
@ 5,1 say [    ID  Corr  DEL  Lac  Parto    Rep  Prod  Rev/Tx   Med  Num  Calor     DUC  ] colo N/w
ROW=7
COL=1
DAT=0
*USE REG order 2
CALC CNT() FOR NP>0 and FB2=B TO NUMV

*INDE ON (DATE()-FPAR)-(DATE()-FSEC) FOR NP>0 AND (DATE()-FPAR)-(DATE()-FSEC)>=DL1 ;
*AND (DATE()-FPAR)-(DATE()-FSEC)<=DL2 AND STAT !='CARGA' AND STAT !='INSEM' and fb2=B TO REG2

*USE REG inde REG2
GO TOP
SCAN for (date()-fpar)-(date()-fsec)>=dl1 and ;
(date()-fpar)-(date()-fsec)<=dl2  and fb2=B and ;
stat!='CARGA' and stat!='INSEM' while INKEY()#6


@ 24,8 SAY DAT+1 PICT '9999'
@ 24,70 SAY [Espere] colo n*/w
@ ROW,COL+1 SAY ID colo gr+/&cz
*--------------------------*
if FPRO>=date() or FT2>=date() 
@ ROW,COL+1 SAY &mensa1
ENDI
IF PSV="S"
@ ROW,COL+6 SAY &mensa2
ENDI
*--------------------------*
@ ROW,COL+9 say CORR
@ ROW,COL+13 SAY (DATE()-FPAR)-(DATE()-FSEC) pict '9999'
@ ROW,COL+19 SAY NP
@ ROW,COL+24 SAY fx(FPAR)
@ ROW,COL+34 SAY LEFT(stat,1) COLO W+/&CZ
	if stat=[ABORT]
		@ ROW,COL+35 SAY [B] COLO W+/&CZ
	endi
@ ROW,COL+38 SAY prm pict '99.9' colo r+/&cz
@ ROW,COL+44 SAY fx(UTRA) 
@ ROW,COL+53 SAY MED1 PICT '###'
@ ROW,COL+59 SAY NS pict '99'
@ ROW,COL+63 SAY fx(UCAL)
@ ROW,COL+73 SAY date()-UCAL PICT '999' colo g+/&cz
IF ROW=20 

@ 24,70 SAY [Enter] COLO N/W
@ 24,75 SAY [ ] COLO W+/&CZ
@ 24,76 SAY CHR(16) colo w+/&cz
DO WHILE INKEY() # 13
IF LASTKEY()=6
@ 6,2 CLEA TO 20,79
GO BOTTOM
EXIT
ENDI
ENDD
@ 6,2 CLEA TO 20,79
@ 24,58 CLEA TO 24,79
ROW=6
COL=1
ENDI
DAT=DAT+1
ROW=ROW+1
ENDS
IF DAT=0 .or. lastkey()=6
@ 24,9 SAY [0]
ELSE
@ 24,15 SAY (DAT/NUMV)*100 PICT '99.9' FUNC 'J' colo w+/&cz
@ 24,20 SAY '%' colo w+/&cz
ENDI

@ 24,60 CLEA TO 24,78
??CHR(7)
@ 24,70 SAY [Enter] COLO N/W
@ 24,75 SAY [ ] COLO W+/&CZ
@ 24,76 SAY CHR(4) colo w+/&cz
DO WHILE INKEY() # 13
ENDD
IF DAT=0
ELSE
SN='N'
@ 24,69 say [Imprimir] GET SN PICT '@M S,N' colo n/w 
READ
IF SN='N' .OR. LASTKEY()=27
ELSE
IF SN='S' .AND. SYS(13)='READY'
SET CONS OFF
SET PRIN ON
SET FILT TO FB2=B and FT2<date() and FPRO<date()
GO TOP
NVE=0
PAG=1
NVAC=0
NTIT=[VACAS ABIERTAS]
NTIT2=[DE ]+STR(DL1,4)+[ A ]+STR(DL2,4)+[ DIAS EN LECHE]
DO WHILE .NOT. EOF()
DO CTRLVRPC
?'     ID     FECHA     LACT          ULTIMA    ULT   ULTIMO           ESTADO   ULT   CORRAL   OBSERVACION'
?'   VACA     PARTO     NUM    DEL    REV/Tx    MED   CALOR      DUC   REPROD   PROD    NUM    ULT  EVENTO'

?'  '+REPLI('-',132)
SCAN for (date()-fpar)-(date()-fsec)>=dl1 and ;
(date()-fpar)-(date()-fsec)<=dl2  and fb2=B and ;
stat!='CARGA' and stat!='INSEM' while INKEY()#6
cra=[   ]
if PSV="S"
cra=[+]+str(CAU2,2)
endi
?'  '+STR(ID,5)+CRA+'  '+fx(FPAR)+'   '+STR(NP,2)+'   ';
+STR((DATE()-FPAR)-(DATE()-FSEC),4)+'    '+fx(UTRA)+'  ';
+STR(MED1,3)+'   '+fx(UCAL)+'  '+STR(DATE()-UCAL,4)+'    '+STAT+'   ';
+STR(PRM,4,1)+'    '+STR(CORR,2)+'     '+OBSC
NVE=NVE+1
NVAC=NVAC+1
IF NVE=WLI
?
?'   '+REPLI(CHR(254),132)
EJEC
PAG=PAG+1
NVE=0
SKIP
EXIT
ENDI
ENDS
ENDD
?'  '+REPLI(CHR(254),132)
?
?'   TOTAL = '+STR(NVAC,4)
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


*   ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
*   ≥  VACAS CON MAS DE nDIAS Y ABIERTAS  ≥
*   ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
PROC RV1020x
AST=6
STORE 0 TO DL1,DL2
@ 4,0 CLEA TO 24,79
@21,0 TO 21,79
@23,0 TO 23,79
@22,33 SAY [VACAS ABIERTAS] COLO gr+/&cz
@24,1 SAY [Total] colo w+/&cz
@24,70 SAY [Esc] colo n/w
@24,74 say [Salir] colo w+/b

@ 24,31 SAY [De] get DL1 PICT '9999' FUNC 'J' valid DL1>0 .AND. DL1<=1095 ERROR [ Dato No Valido Rango 1-1095 ] 
@ 24,39 SAY [a] get DL2 PICT '9999' FUNC 'J' valid DL2>=DL1 .AND. DL2<=1095 ERROR [ Dato No Valido Rango 1-1095 ]
@ 24,46 SAY [DEL]
READ
IF LASTKEY()=27
RETU
ENDI
@ 24,68 clea to 24,79
@ 24,70 SAY [Espere] colo n*/w
@ 4,1 say [                        Fecha    Edo  Ult   Ultima   Ult  Ins  Ultimo         ] colo n/w
@ 5,1 say [    ID  Corr  DEL  Lac  Parto    Rep  Prod  Rev/Tx   Med  Num  Calor     DUC  ] colo N/w
ROW=7
COL=1
DAT=0
GO TOP
USE REG order 2
CALC CNT() FOR NP>0 and FB2=B TO NUMV

INDE ON (DATE()-FPAR)-(DATE()-FSEC) FOR NP>0 AND (DATE()-FPAR)-(DATE()-FSEC)>=DL1 ;
AND (DATE()-FPAR)-(DATE()-FSEC)<=DL2 AND STAT !='CARGA' AND STAT !='INSEM' and fb2=B TO REG2

USE REG inde REG2
GO TOP
SCAN WHILE INKEY()#6
@ 24,8 SAY DAT+1 PICT '9999'
@ 24,70 SAY [Espere] colo n*/w
@ ROW,COL+1 SAY ID colo gr+/&cz
*--------------------------*
if FPRO>=date() or FT2>=date() 
@ ROW,COL+1 SAY &mensa1
ENDI
IF PSV="S"
@ ROW,COL+6 SAY &mensa2
ENDI
*--------------------------*
@ ROW,COL+9 say CORR
@ ROW,COL+13 SAY (DATE()-FPAR)-(DATE()-FSEC) pict '9999'
@ ROW,COL+19 SAY NP
@ ROW,COL+24 SAY fx(FPAR)
@ ROW,COL+34 SAY LEFT(stat,1) COLO W+/&CZ
	if stat=[ABORT]
		@ ROW,COL+35 SAY [B] COLO W+/&CZ
	endi
@ ROW,COL+38 SAY prm pict '99.9' colo r+/&cz
@ ROW,COL+44 SAY fx(UTRA) 
@ ROW,COL+53 SAY MED1 PICT '###'
@ ROW,COL+59 SAY NS pict '99'
@ ROW,COL+63 SAY fx(UCAL)
@ ROW,COL+73 SAY date()-UCAL PICT '999' colo g+/&cz
IF ROW=20 

@ 24,70 SAY [Enter] COLO N/W
@ 24,75 SAY [ ] COLO W+/&CZ
@ 24,76 SAY CHR(16) colo w+/&cz
DO WHILE INKEY() # 13
IF LASTKEY()=6
@ 6,2 CLEA TO 20,79
GO BOTTOM
EXIT
ENDI
ENDD
@ 6,2 CLEA TO 20,79
@ 24,58 CLEA TO 24,79
ROW=6
COL=1
ENDI
DAT=DAT+1
ROW=ROW+1
ENDS
IF DAT=0 .or. lastkey()=6
@ 24,9 SAY [0]
ELSE
@ 24,15 SAY (DAT/NUMV)*100 PICT '99.9' FUNC 'J' colo w+/&cz
@ 24,20 SAY '%' colo w+/&cz
ENDI

@ 24,60 CLEA TO 24,78
??CHR(7)
@ 24,70 SAY [Enter] COLO N/W
@ 24,75 SAY [ ] COLO W+/&CZ
@ 24,76 SAY CHR(4) colo w+/&cz
DO WHILE INKEY() # 13
ENDD
IF DAT=0
ELSE
SN='N'
@ 24,69 say [Imprimir] GET SN PICT '@M S,N' colo n/w 
READ
IF SN='N' .OR. LASTKEY()=27
ELSE
IF SN='S' .AND. SYS(13)='READY'
SET CONS OFF
SET PRIN ON
SET FILT TO FB2=B and FT2<date() and FPRO<date()
GO TOP
NVE=0
PAG=1
NVAC=0
NTIT=[VACAS ABIERTAS]
NTIT2=[DE ]+STR(DL1,4)+[ A ]+STR(DL2,4)+[ DIAS EN LECHE]
DO WHILE .NOT. EOF()
DO CTRLVRPC

?'     ID     FECHA     LACT          ULTIMA    ULT   ULTIMO    DIAS   ESTADO   ULT   CORRAL   OBSERVACION'
?'   VACA     PARTO     NUM    DEL    REV/Tx    MED   CALOR     TRAN   REPROD   PROD    NUM    ULT  EVENTO'

?'  '+REPLI('-',132)
SCAN WHILE INKEY()#6
cra=[   ]
if PSV="S"
cra=[+]+str(CAU2,2)
endi
?'  '+STR(ID,5)+CRA+'  '+fx(FPAR)+'   '+STR(NP,2)+'   ';
+STR((DATE()-FPAR)-(DATE()-FSEC),4)+'    '+fx(UTRA)+'  ';
+STR(MED1,3)+'   '+fx(UCAL)+'  '+STR(DATE()-UCAL,4)+'    '+STAT+'   ';
+STR(PRM,4,1)+'    '+STR(CORR,2)+'     '+OBSC
NVE=NVE+1
NVAC=NVAC+1
IF NVE=WLI
?
?'   '+REPLI(CHR(254),132)
EJEC
PAG=PAG+1
NVE=0
SKIP
EXIT
ENDI
ENDS
ENDD
?'  '+REPLI(CHR(254),132)
?
?'   TOTAL = '+STR(NVAC,4)
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
ERAS REG2.IDX


*   ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
*   ≥  VACAS +365 D Y NO SECAS      ≥
*   ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
PROC RV10192
@ 4,0 CLEA TO 24,79
@21,0 TO 21,79
@23,0 TO 23,79
@22,20 SAY [VACAS CON +365 DIAS EN LECHE Y NO SECAS] COLO gr+/&cz
@24,1 SAY [Total] colo w+/&cz
@24,70 SAY [Esc] colo n/w
@24,74 say [Salir] colo w+/b

@ 4,1 say [                                                                              ] colo n/w
@ 5,1 say [      ID  Status  Prod    DEL  Corral      ID  Status  Prod    DEL  Corral    ] colo n/w
ROW=7
COL=5
DAT=0
use REG order 2
@ 24,70 SAY [Espere] colo n*/w
SCAN FOR FPAR#B and FSEC=B and FB2=B and DATE()-FPAR>=365 WHILE INKEY()#6
@ 24,8 SAY DAT+1 PICT '9999'
@ 24,70 SAY [Espere] colo n*/w

@ ROW,COL SAY ID PICT '99999' colo gr+/&cz
*--------------------------*
if FPRO2>=date() or FT2>=date() 
@ ROW,COL SAY &mensa1
ENDI
IF PSV="S"
@ ROW,COL+5 SAY &mensa2
ENDI
*--------------------------*
@ ROW,COL+7 SAY STAT
@ ROW,COL+14 SAY PRM PICT '99.9' colo r+/&cz
@ ROW,COL+22 SAY DATE()-FPAR PICT '999'
@ ROW,COL+29 SAY CORR PICT '99'

IF ROW=20 .AND. COL=42
@ 24,70 clea to 24,79
@ 24,70 SAY [Enter] COLO N/W
@ 24,76 SAY CHR(16) colo w+/&cz
DO WHILE INKEY() # 13
IF LASTKEY()=6
@ 4,1 CLEA TO 20,78
GO BOTTOM
EXIT
ENDI
ENDD
@ 6,2 CLEA TO 20,79
@ 24,58 CLEA TO 24,79
ROW=6
COL=5
ENDI
IF ROW=20
ROW=6
COL=COL+37
ENDI
DAT=DAT+1
ROW=ROW+1
ENDS
IF DAT=0
@ 24,9 SAY [0]
ENDI
@ 24,70 clea to 24,79
@ 24,70 SAY [Enter] COLO N/W
@ 24,76 SAY CHR(4) colo w+/&cz
DO WHILE INKEY() # 13
ENDD
IF DAT=0
ELSE
SN='N'
@ 24,69 say [Imprimir] GET SN PICT '@M S,N' colo n/w 
READ
IF SN='N' .OR. LASTKEY()=27
ELSE
IF SN='S' .AND. SYS(13)='READY'
SET CONS OFF
SET PRIN ON
use REG order 2
NV=0
PAG=1
NVAC=0
GO TOP
NTIT=[VACAS CON + 365 DIAS EN PRODUCCION NO SECAS]

DO WHILE .NOT. EOF()
DO CTRLVRP0
?'   ID      #  FECHA              ESTADO  ULTIMA    ULTIMO  ULT   SEMEN    SER DIAS  PROD  ULT.'
?'  VACA     C  PARTO     NP  DEL  REPROD  REV/Tx    CALOR   MED UTILIZADO   #  INSE  DIA   PROD'
?'  '+REPLI('-',94)

SCAN FOR FPAR#B and FSEC=B and FB2=B and DATE()-FPAR>=365 WHILE INKEY()#6
XST=[ ]
IF STAT='CARGA' .OR. STAT='INSEM'
REPL DIA WITH DATE()-UCAL
ELSE
REPL DIA WITH 0
ENDI


cra=[   ]
if PSV="S"
cra=[+]+str(CAU2,2)
endi

?' '+str(ID,5)+CRA+' '+STR(CORR,2)+'  '+fx(FPAR)+'  '+STR(NP,2)+'  '+STR(DATE()-FPAR,3)+'  ';
+STAT+'  '+fx(UTRA)+'  '+fx(UCAL)+''+STR(MED1,3)+'  '+IDT+'  '+STR(NS,2)+'   ';
+STR(DIA,3)+'  '+STR(PRX,4,1)+'  '+STR(PRM,4,1)

NV=NV+1
NVAC=NVAC+1
IF NV=WLI
?'  '+REPLI(CHR(254),94)
EJEC
PAG=PAG+1
NV=0
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
ELSE
??CHR(7)
WAIT WIND [ IMPRESORA APAGADA O DESCONECTADA ] TIMEOUT 2
ENDI
ENDI
ENDI
CLOS DATA


*   ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
*   ≥  VACAS CON DIF. EN PROD.      ≥
*   ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
PROC RV1049
Ndif=0
@ 4,0 CLEA TO 24,79
@21,0 TO 21,79
@23,0 TO 23,79
@22,23 SAY [VACAS CON DIFERENCIA EN PRODUCCION] colo gr+/&cz
@24,1 SAY [Total] colo w+/&cz
@24,70 SAY [Esc] colo n/w
@24,74 say [Salir] colo w+/b

@ 11,18 FILL TO 17,65 COLO N/N
@ 10,17 fill to 16,64 COLO SCHE 15
@ 10,17 to 16,64 COLO SCHE 15
@ 11,18 say [  Este programa emitira una lista de todas    ] COLO SCHE 15
@ 12,18 say [  aquellas vacas que hayan aumentado o dis-   ] colo sche 15
@ 13,18 say [  minuido su produccion, con respecto a la    ] colo sche 15
@ 14,18 say [  pesada anterior en un porcentaje dado.      ] colo sche 15
@ 15,18 say [           Ejemplo : 15 ¢ -15 %               ] colo sche 15
@ 24,32 say [Porcentaje >=] get Ndif pict '#99'func 'j,t' ;
							valid Ndif>=-99 .and. Ndif<=99 ;
							error tit(2) COLO W+/&CZ,n/W
READ

IF lastkey()=27 .or. Ndif=0
retu
else
@4,0 CLEA TO 20,79
xdif=(100+Ndif)/100
IF xdif>1
xx="PRM>=(tota*xdif)"
else
xx="PRM<=(tota*xdif)"
endi
use pesadas
calc max(fpes) to dFecha

@ 4,1 say [     ID  Corral  Estado   Fecha           Ultima    Produccion                ] colo n/w
@ 5,1 say [    Num   Num    Reprod   Parto    DEL    Pesada   Anter  Actual  Diferencia  ] colo n/w

@ 24,58 CLEA TO 24,79
@ 24,70 SAY [Espere] colo n*/w
use REG order 2
SET FILT TO (NP>0 and FB2=B) 
	*INDE ON str(corr)+STR(ID) TO NCORR 
	*use reg inde Ncorr

ROW=7
COL=3
DAT=0
SCAN for pesa=dFecha .AND. TOTA>0 .and. &xx WHILE INKEY()#6
@ 24,8 SAY DAT+1 PICT '9999'
@ 24,58 CLEA TO 24,79
@ 24,70 SAY [Espere] colo n*/w
@ ROW,COL SAY ID colo gr+/&cz
*--------------------------*
if FPRO2>=date() or FT2>=date() 
@ ROW,COL SAY &mensa1
ENDI
IF PSV="S"
@ ROW,COL+5 SAY &mensa2
ENDI
*--------------------------*
@ ROW,COL+9 SAY CORR 
@ ROW,COL+15 SAY STAT
@ ROW,COL+23 SAY fx(FPAR)
@ ROW,COL+33 SAY PESA-FPAR PICT '999'
@ ROW,COL+39 SAY fx(PESA)
@ ROW,COL+50 SAY TOTA colo 3+/&cz
@ ROW,COL+57 SAY PRM colo r+/&cz
@ ROW,COL+63 SAY prm-tota pict '999.9' colo g+/&cz
@ ROW,COL+69 SAY (ABS(tota-prm)/tota)*100 pict '999' colo w+/&cz
@ ROW,COL+73 SAY [%]

IF ROW=20 
@ 24,58 CLEA TO 24,79
@ 24,71 SAY [Enter] COLO N/W
@ 24,77 SAY CHR(16) colo w+/&cz
DO WHILE INKEY() # 13
IF LASTKEY()=6
@ 6,2 CLEA TO 20,79
GO BOTTOM
EXIT
ENDI
ENDD

@ 6,2 CLEA TO 20,79
@ 24,58 CLEA TO 24,79
ROW=6
ENDI
DAT=DAT+1
ROW=ROW+1
ENDS
??chr(7)
@ 24,70 clea to 24,79
@ 24,70 SAY [Enter] COLO N/W
@ 24,76 SAY CHR(4) colo w+/&cz
DO WHILE INKEY() # 13
ENDD

IF DAT=0
ELSE
SN='N'
@ 24,58 CLEA TO 24,79
@ 24,69 say [Imprimir] GET SN PICT '@M S,N' colo n/w 
READ
IF SN='N' .OR. LASTKEY()=27
SET FILT TO
ELSE
IF SN='S' .AND. SYS(13)='READY'
SET CONS OFF
SET PRIN ON
NVE=0
PAG=1
NVAC=0
NTIT=[VACAS CON DIFERENCIA EN PRODUCCION >= ]+STR(NDIF,4)+' %'
GO TOP
DO WHILE .NOT. EOF()
DO CTRLVRP0
?'     ID     CORR  ESTADO   FECHA            ULTIMO    ULTIMA        PRODUCCION             '
?'    VACA     NUM  REPROD   PARTO      DEL   CALOR     PESADA       ANTE    ACTU    DIFERENCIA'
?'  '+REPLI('-',94)

SCAN for pesa=dFecha .AND. TOTA>0 .and. &xx WHILE INKEY()#6

cra=[   ]
if PSV="S"
cra=[+]+str(CAU2,2)
endi

?'   '+STR(ID,5)+CRA+'  '+STR(CORR,2)+'    '+STAT+'   '+fx(FPAR)+'   '+STR((PESA-FPAR),3)+'   '+;
fx(UCAL)+'  '+fx(PESA)+'     '+STR(TOTA,4,1)+'    '+STR(PRM,4,1)+'   '+STR((PRM-TOTA),5,1)+' '+STR(((ABS(tota-prm)/tota)*100),4)+' %'
NVAC=NVAC+1
NVE=NVE+1

IF NVE=WLI
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
ELSE
??CHR(7)
WAIT WIND [ IMPRESORA APAGADA O DESCONECTADA ] TIMEOUT 2
ENDI
ENDI
ENDI
endi
CLOS DATA

*   …ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª
*   ∫         U T I L E R I A S             ∫
*   »ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº

PROC O2
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
?'      ID   Parto   DEL  Prod Status Corral            ID   Parto   DEL  Prod Status Corral  '
?'  '+REPLI('-',94)
ONTHISPAGE=MIN(ROWS*COLS,RECCOUNT()-PRINTED)
ROWS=MIN(ROWS,(ONTHISPAGE/COLS))
THISROW=1
DO WHILE THISROW<=ROWS .AND. PRINTED<=RECCOUNT()
THISCOL=1
if id>0
?space(2),LEFT(STR(ID,5,0)+str(np,8)+'  '+STR(DATE()-FPAR,4)+'  '+STR(PRM,4,1)+'  '+LEFT(STAT,5)+'  '+STR(CORR,2),COLW)+SPACE(MAX(0,COLW-LEN(FIELD)))
NVAC=NVAC+1
endi
PRINTED=PRINTED+1
DO WHILE THISCOL<COLS .AND. PRINTED<=RECCOUNT()
SKIP ROWS
if id>0
??space(2),LEFT(STR(ID,5,0)+str(np,8)+'  '+STR(DATE()-FPAR,4)+'  '+STR(PRM,4,1)+'  '+LEFT(STAT,5)+'  '+STR(CORR,2),COLW)+SPACE(MAX(0,COLW-LEN(FIELD)))
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
ERAS LOTES.IDX

FUNC CHkMED
PARAMETER XMED
USE MEDIC order 1
SEEK XMED
DO CASE
CASE .NOT. FOUND()
SELE 1
use REG order 2
RETU .F.
CASE FOUND ()
xmnom=mnom
SELE 1
use REG order 2
RETU .T.
ENDCASE

FUNC CHENF2
PARAMETER XENF
USE ENFERM order 1
SEEK XENF 
DO CASE 
CASE .NOT. FOUND()
CLOS DATA 
RETU .F.
CASE FOUND()
xnombre=nombre
CLOS DATA
RETU .T.
ENDCASE
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*

PROCEDURE R6
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
?'      ID  Enf  Corral         ID  Enf  Corral         ID  Enf  Corral         ID  Enf  Corral'        
?'  '+REPLI('-',94)
ONTHISPAGE=MIN(ROWS*COLS,RECCOUNT()-PRINTED)
ROWS=MIN(ROWS,(ONTHISPAGE/COLS))
THISROW=1
DO WHILE THISROW<=ROWS .AND. PRINTED<=RECCOUNT()
THISCOL=1
if id>0
?space(2),LEFT(STR(ID,5)+'   '+STR(ENF,2)+'   '+STR(CORR,2),COLW)+SPACE(MAX(0,COLW-LEN(FIELD)))
NVAC=NVAC+1
endi
PRINTED=PRINTED+1
DO WHILE THISCOL<COLS .AND. PRINTED<=RECCOUNT()
SKIP ROWS
if id>0
??SPACE(2),LEFT(STR(ID,5)+'   '+STR(ENF,2)+'   '+STR(CORR,2),COLW)+SPACE(MAX(0,COLW-LEN(FIELD)))
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
?'   TOTAL = '+STR(NVAC,4)
?
?'   NOTA: Incluye vacas esten en Agenda de Hospital.'
ENDI
ENDD
EJECT
PGCOUNT=PGCOUNT+1
SKIP ((COLS-1)*ROWS)
ENDD
CLOS DATA
ERAS LOTES.DBF

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
defi wind orden from 6,21 to 17,59 title [ Ordenadas Por : ] shadow colo sche 16
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

PROCEDURE TITULO
** --------------

if xpag>1
eject page
endi

?'  Rancho   : '+Q36+'   '+nom
?'  Reporte  : '+NTIT
?'  Fecha    : '+DTOC(DATE())+'  Hora : '+TIME()
?'  Pagina   : '+STR(xpag,2,0)+space(108)+[LACTOFOX]
?'  Status   : '+ntit2
		?'  '+repli(CHR(254),135)
		?
		?'    ID   COR LAC  EDO                    SER  VREL  VREL  PROD  [    P E S A D A S  A N T E R I O R E S    ] ULT  F. ULT    PICO  PROD'
		?'   VACA  NUM NUM  REPRO  DUC   DEL   DAB NUM  PROM  ACTU  PROM  (10)  (9)  (8)  (7)  (6)  (5)  (4)  (3)  (2) PES  PESADA    KGS   DIAS'
		?'  '+repli('-',135)
	*	?
xpag=xpag+1
return

PROCEDURE TITULO1
** --------------
if xpag>1
eject page
endi

?'  Rancho   : '+Q36+'   '+nom
?'  Reporte  : '+NTIT
?'  Fecha    : '+DTOC(DATE())+'  Hora : '+TIME()
?'  Pagina   : '+STR(xpag,2,0)+space(108)+[LACTOFOX]
		?'  '+repli(CHR(254),135)
		?
		?'    ID   COR LAC  EDO                    SER  VREL  VREL  PROD  [    P E S A D A S  A N T E R I O R E S    ] ULT  F. ULT    PICO      '
		?'   VACA  NUM NUM  REPRO  DUC   DEL   DAB NUM  PROM  ACTU  PROM  (10)  (9)  (8)  (7)  (6)  (5)  (4)  (3)  (2) PES  PESADA    KGS  CAUSA'
		?'  '+repli('-',135)
*		?
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
		?'  '+repli(CHR(254),135)
		?
		?'    ID   COR LAC  EDO                     SER  VREL  VREL  PROD   [    P E S A D A S  A N T E R I O R E S    ] ULT  FECHA     DIAS'
		?'   VACA  NUM NUM  REPRO   DUC   DEL   DAB NUM  PROM  ACTU  PROM   (10)  (9)  (8)  (7)  (6)  (5)  (4)  (3)  (2) PES  SECAR     FALT'
		?'  '+repli('-',135)
*		?
xpag=xpag+1
return


PROCEDURE TITULO3
** -------------
if xpag>1
eject page
endi

?'  Rancho   : '+Q36+'   '+nom
?'  Area     :  VIENTRES'
?'  Reporte  : '+NTIT
?'  Fecha    : '+DTOC(DATE())+'  Hora : '+TIME()
?'  Pagina   : '+STR(xpag,2,0)+space(108)+[LACTOFOX]
		?'  '+repli(CHR(254),132)
		?'    ID    C  FECHA          EDO               ULTIMA     ULTIMO   ULT  SER PROD  ULT.  OBSERVACIONES   FECHA A   MOTIVO'
		?'   VACA   #  PARTO     DEL  REP     DUC  DAB  REV/Tx     CALOR    MED   #  ANT   PROD  ULT   EVENTO    REVISAR   REVISION'
		?'  '+repli(CHR(254),132)
xpag=xpag+1
return

