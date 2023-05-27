* CTRLARCH
*ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
	IF CLAVEX=Q22 or CLAVEX=Q23 
	ELSE
    ??CHR(7)
    WAIT WIND [ Acceso Negado ] TIMEOUT 1
    RETU
    ENDI
*ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
hide popup foo6
@4,0 clea to 24,79
@23,0 to 23,79

do case
   case BAR()=1
        DO reorg
   case BAR()=2
        DO CTRLAmod
   case BAR()=3
        DO Actualiza
   case BAR()=4
        DO Reprod
   case BAR()=5
        DO Analisis
   case BAR()=7
        DO TRASPASO
   case BAR()=8
        DO TRASPASO1
   case BAR()=9
        DO TRASPASO2
   case BAR()=11
        DO CTRLAMOV
   case BAR()=13
         hide menu foo
        DO LACTOIFS.EXE
*        Do CTRLDBOU
   case BAR()=15
         hide menu foo
         DO LACTOEVX.EXE
*        Do CTRLPDAT
   case lastkey()=27
        CLOS ALL
endcase
@ 4,0 clea to 24,79
@23,0 to 23,79
return

*************
PROC REORG
*************
SN='N'
@ 22,30 say [REORGANIZAR INDICES] colo gr+/&cz
@ 10,4 say [NOTA :] COLO W+/&CZ
@ 12,4 fill to 16,77 colo n/n
@ 11,3 fill to 15,76 COLO SCHE 15
@ 12,4 say [ Este programa reindexar  todos los archivos y deber  usarse en caso ] COLO SCHE 15
@ 13,4 say [ de tener problemas con los datos. Si despu‚s de esto, el programa no   ] COLO SCHE 15
@ 14,4 say [ funciona correctamente, favor de llamar al tel. (14) 72-05-15.         ] COLO SCHE 15
@ 24,67 say [Continuar] get sn pict '@M N,S' COLO n/w
READ
IF SN='S'
@24,67 clea to 24,79
@24,70 say [Espere] COLO N*/W
@24,1 SAY [Archivo] COLO W+/&CZ

	** Comandos para Indexar
	** --------------------
	xsub=(cmd_nam5)
	do CTRLACDX with xsub
	SET DEFA TO (cmd_nam0)
	** --------------------

CLOS DATA
??CHR(7)
ELSE
ENDI
return


PROC CTRLAmod
*-----------------------------*
SN='N'
@ 22,31 SAY [COMPACTAR ARCHIVOS] colo gr+/&cz

@ 10,4 say [NOTA :] COLO W+/&CZ
@ 12,4 fill to 16,77 colo n/n
@ 11,3 fill to 15,76 COLO SCHE 15
@ 12,4 say [ Este programa depurar  todos aquellos archivos borrando de estos toda   ] COLO SCHE 15
@ 13,4 say [ aquella informaci¢n no necesaria; as¡ como tambi‚n proceder  a elimi-   ] COLO SCHE 15
@ 14,4 say [ nar definitivamente todos los animales que hayan sido borrados.         ] COLO SCHE 15

@ 24,67 say [Continuar] get sn pict '@M N,S' COLO n/w
READ
IF SN='S'
@24,14 say '/0021'
@ 24,60 clea to 24,79
SET DEFA TO (cmd_nam5)
@ 24,70 say [Espere] COLO N*/W
@ 24,1 SAY [Archivo] COLO W+/&CZ

@ 24,10 SAY [0001]
USE REG exclusive
dele all for id<1
PACK

@ 24,10 SAY [0002]
USE LACTS exclusive
DELE ALL FOR ID<1 or FPAR=B
PACK

@24,10 SAY [0003]
USE CLIN exclusive
DELE ALL FOR FECHA=B or ID<1
PACK

@24,10 SAY [0004]
USE PROD exclusive
DELE ALL FOR ID<1 or FP=B
PACK

@24,10 SAY [0005]
USE MAST exclusive
DELE ALL FOR FTES=B or ID<1
PACK

@24,10 SAY [0006]
USE CALOR exclusive
DELE ALL FOR FECHA=B or ID<1 
PACK

@24,10 SAY [0007]
USE PTB exclusive
DELE ALL FOR FPU=B or ID<1
PACK

@24,10 SAY [0008]
USE TECNIC exclusive
DELE ALL FOR TINIC=[   ]
PACK

@24,10 SAY [0009]
USE MEDIC exclusive
DELE ALL FOR MNUM<1
PACK

@24,10 SAY [0010]
USE ENFERM exclusive
DELE ALL FOR NUM<1
PACK

@24,10 SAY [0011]
USE TOROS exclusive
DELE ALL FOR IDTORO=[         ] or CANT<1
PACK

@24,10 SAY [0012]
USE CBAJA exclusive
DELE ALL FOR CNUM<1
PACK

@24,10 SAY [0013]
USE CORRAL exclusive
DELE ALL FOR CNUM<1
PACK

@24,10 SAY [0014]
USE BST exclusive
DELE ALL FOR FECHA=B or ID<1
PACK

@24,10 SAY [0015]
USE PARAM exclusive
DELE ALL FOR FECHA=B
PACK

@24,10 SAY [0016]
USE PARAM2 exclusive
DELE ALL FOR FECHA=B 
PACK

@24,10 SAY [0017]
USE CONDC exclusive
DELE ALL FOR FECHA=B or ID<1
PACK

@24,10 SAY [0018]
USE CCS exclusive
DELE ALL FOR FECHA=B or ID<1
PACK

@24,10 SAY [0019]
USE VACUNAS exclusive
DELE ALL FOR FECHA=B or ID<1
PACK

@24,10 SAY [0020]
USE MEDIDA exclusive
DELE ALL FOR ID<1 or fecha=ctod("")
PACK

@24,10 SAY [0021]
use BAJAS exclusive
dele all for ID<1
PACK

CLOS DATA
set defa to (cmd_nam0)
??CHR(7)
ELSE
ENDI

*************
PROC TRASPASO
*************
clos all
@22,0 clea to 22,79
@22,29 SAY [VER DATOS EN DISKETTE] COLO GR+/&CZ
ldrive='A'
@22,1 say [Drive :] get ldrive pict '@M A,B'
read
	if lastkey()=27
		return
	endi
ldrive=ldrive+":"

SET DEFA TO &ldrive

IF FILE("REG2.DBF") and FILE("CLIN2.DBF") and FILE("CALOR2.DBF") and FILE("MAST2.DBF") and FILE("LACTS2.DBF") and FILE("PTB2.DBF") and FILE("BST2.DBF")
defi wind dire from 4,2 to 19,20 title [ Drive ]+(ldrive) shadow colo sche 16
acti wind dire
use reg2 
row=1
@ 0,1 say [ID Num.   Lact]
scan while .t.
@ row,2 say id colo sche 18
@ row,12 say np colo sche 18
row=row+1
if row=13
row=1
do while inkey()#13
@ 13,8 say [Enter]
@ 13,14 say chr(16)
endd
@ 1,1 clea to 13,17
endi
ends
do while inkey()#13
@ 13,8 say [Enter]
@ 13,14 say chr(16) 
endd
clos all
rele wind dire
else
??CHR(7)
WAIT WIND [ DISKETTE SIN ARCHIVOS DE DATOS....VERIFICAR  ] TIMEOUT 2
@ 16,1 CLEA TO 17,78
endi
SET DEFA TO 
@22,0 clea to 22,30
clos all
return

**************
PROC TRASPASO1
**************
@22,0 clea to 22,79
@22,28 SAY [EXPORTAR DATOS A DISKETTE] COLO GR+/&CZ
ldrive='A'
@22,1 say [Drive :] get ldrive pict '@M A,B'
read
	if lastkey()=27
		retu
	endi
ldrive=ldrive+":"

SET PATH TO &cmd_nam5  &&,c:\dos
@24,32 say [CREANDO ARCHIVOS] colo w+/B
@24,72 say [Espere] COLO N*/W

if file(ldrive+"reg2.dbf")
else
use reg
copy stru to (ldrive+"reg2")
endi

if file(ldrive+"lacts2.dbf")
else
use lacts
copy stru to (ldrive+"lacts2")
endi

if file(ldrive+"clin2.dbf")
else
use clin
copy stru to (ldrive+"clin2")
endi

if file(ldrive+"prod2.dbf")
else
use prod
copy stru to (ldrive+"prod2")
endi

if file(ldrive+"calor2.dbf")
else
use calor
copy stru to (ldrive+"calor2")
endi

if file(ldrive+"mast2.dbf")
else
use mast
copy stru to (ldrive+"mast2")
endi

if file(ldrive+"ptb2.dbf")
else
use ptb
copy stru to (ldrive+"ptb2")
endi

if file(ldrive+"bst2.dbf")
else
use bst
copy stru to (ldrive+"bst2")
endi

if file(ldrive+"CONDC2.dbf")
else
use CONDC
copy stru to (ldrive+"CONDC2")
endi

if file(ldrive+"CALIF2.dbf")
else
use CALIF
copy stru to (ldrive+"CALIF2")
endi

if file(ldrive+"CCS2.dbf")
else
use CCS
copy stru to (ldrive+"CCS2")
endi

if file(ldrive+"MEDIDA2.dbf")
else
use MEDIDA
copy stru to (ldrive+"MEDIDA2")
endi

if file(ldrive+"VACUNAS2.dbf")
else
use VACUNAS
copy stru to (ldrive+"VACUNAS2")
endi
* Termina creacion de archivos
* ----------------------------

@ 24,0 clea to 24,79
xx=[V]
@24,32 say [Vacas - Crianza] get xx pict '@M V,C' error tit(2) colo w+/&cz,n/w
read
	if lastkey()=27
		return
	endi

	do case
		case xx="V"
			do TVACAS
		case xx="C"
		    do TCRIANZA
		otherwise
	endcase
return

PROCEDURE TVACAS
****************
xtot=0
xid=0
@ 22,70 say [IDa] colo n/w
@ 24,1 say [Total] colo w+/&cz
@ 24,70 SAY [Esc] colo n/w
@ 24,74 say [Salir] 

do while .t.
	sele 1
	use reg order 2
	sele 2
	use ldrive+"REG2"
	sele 1
@ 22,11 clea to 22,20
axid=xid
xid=0
@ 22,1 SAY [ID ] get XID PICT '#####' FUNC 'T' COLO W+/&CZ,n/w
read
if xid=0 or lastkey()=27
@22,0 to 22,79

exit
endi

seek xid
if not found()
??chr(7)
wait wind [ ID no existe] timeout 1
else
@22,11 say [Procesando] COLO w+*/&cz
		scatter to mdatos 
		sele 2
 				append blank

 				gather from mdatos
				* Copia los demas archivos
				* ------------------------
				use LACTS order 1
				seek xid
				if found()
					copy to array CDATOS for ID=xid
			 		use ldrive+"LACTS2"
					append from array CDATOS				
			    endif 
                *
				use PROD order 1
				seek xid
				if found()
					copy to array CDATOS for ID=xid
			 		use ldrive+"PROD2"
					append from array CDATOS				
			    endif 
				*
				use MAST order 1
				seek xid
				if found()
					copy to array CDATOS2 for ID=xid
			 		use ldrive+"MAST2"
					append from array CDATOS2				
			    endif 
			    *
				use BST order 1
				seek xid
				if found()
					copy to array CDATOS for ID=xid
			 		use ldrive+"BST2"
					append from array CDATOS				
			    endif 
			    *
				use CCS order 1
				seek xid
				if found()
					copy to array CDATOS for ID=xid
			 		use ldrive+"CCS2"
					append from array CDATOS				
			    endif 
			    *
				use CLIN order 2
				seek xid
				if found()
					copy to array CDATOS for ID=xid
			 		use ldrive+"CLIN2"
					append from array CDATOS				
			    endif 
			    *
				use CALOR order 2
				seek xid
				if found()
					copy to array CDATOS for ID=xid
			 		use ldrive+"CALOR2"
					append from array CDATOS				
			    endif 
			    *
				use VACUNAS order 2
				seek xid
				if found()
					copy to array CDATOS for ID=xid
			 		use ldrive+"VACUNAS2"
					append from array CDATOS				
			    endif 
			    *
				use PTB order 2
				seek xid
				if found()
					copy to array CDATOS for ID=xid
			 		use ldrive+"PTB2"
					append from array CDATOS				
			    endif 
			    *
				use CONDC order 1
				seek xid
				if found()
					copy to array CDATOS for ID=xid
			 		use ldrive+"CONDC2"
					append from array CDATOS				
			    endif 
			    *
				use CALIF order 1
				seek xid
				if found()
					copy to array CDATOS3 for ID=xid
			 		use ldrive+"CALIF2"
					append from array CDATOS3				
			    endif 

				??chr(7)
				clos all
				@ 22,11 clea to 22,20
				@ 22,11 say [OK] colo r+/&cz
				xtot=xtot+1
		endif
@ 24,7 say xtot pict'###'
enddo
return


PROCEDURE TCRIANZA
****************
xtot=0
xid=0
@ 22,70 say [IDa] colo n/w
@ 24,1 say [Total] colo w+/&cz
@ 24,70 SAY [Esc] colo n/w
@ 24,74 say [Salir] 

do while .t.
	sele 1
	use reg order 3
	sele 2
	use ldrive+"REG2"
	sele 1
@ 22,11 clea to 22,20
axid=xid
xid=0
@ 22,74 say axid pict'#####' func'j' colo w+/&cz
@22,1 SAY [ID ] get XID PICT '#####' FUNC 'T' COLO W+/&CZ,n/w
read
if xid=0 or lastkey()=27
@22,0 to 22,79

exit
endi

seek xid
if not found()
??chr(7)
wait wind [ ID no existe] timeout 1
else
@22,11 say [Procesando] COLO w+*/&cz
		scatter to mdatos 
		sele 2
 				append blank

 				gather from mdatos
				* Copia los demas archivos
				* ------------------------
				use CLIN order 3
				seek xid
				if found()
					copy to array CDATOS for ID=xid
			 		use ldrive+"CLIN2"
					append from array CDATOS				
			    endif 
			    *
				use CALOR order 3
				seek xid
				if found()
					copy to array CDATOS for ID=xid
			 		use ldrive+"CALOR2"
					append from array CDATOS				
			    endif 
			    *
				use VACUNAS order 3
				seek xid
				if found()
					copy to array CDATOS for ID=xid
			 		use ldrive+"VACUNAS2"
					append from array CDATOS				
			    endif 
			    *
				use PTB order 3
				seek xid
				if found()
					copy to array CDATOS for ID=xid
			 		use ldrive+"PTB2"
					append from array CDATOS				
			    endif 
			    *
				use MEDIDA order 1
				seek xid
				if found()
					copy to array CDATOS3 for ID=xid
			 		use ldrive+"MEDIDA2"
					append from array CDATOS3				
			    endif 

				??chr(7)
				clos all
				@ 22,11 clea to 22,20
				@ 22,11 say [OK] colo r+/&cz
				xtot=xtot+1
		endif
@ 24,7 say xtot pict'###'
enddo
return

**************
PROC TRASPASO2
**************
ldrive='A'
@22,28 say [IMPORTAR DATOS DE DISKETTE] colo gr+/&cz
@22,1 say [Drive :] get ldrive pict '@M A,B'
read
	if lastkey()=27
		retu
	endi
ldrive=ldrive+":"

SET PATH TO &cmd_nam5  &&,c:\dos
IF FILE(ldrive+"REG2.DBF") and FILE(ldrive+"CLIN2.DBF") ;
and FILE(ldrive+"CALOR2.DBF") and FILE(ldrive+"MAST2.DBF") ;
and FILE(ldrive+"LACTS2.DBF") and FILE(ldrive+"PTB2.DBF") ;
and FILE(ldrive+"BST2.DBF") and FILE(ldrive+"PROD2.DBF")

sn="N"
@24,22 say [ESTA OPCION BORRARA LOS DATOS DEL DISKETTE] COLO w+/B
@24,67 SAY [Continuar] GET SN PICT '@M S,N' COLO n/w
READ
IF LASTKEY()=27 or SN='N'
 @ 16,1 clea to 19,78
ELSE
 @24,67 clea to 24,79
 @24,72 say [Espere] COLO N*/W
USE REG
APPE FROM ldrive+"REG2"

USE LACTS
APPE FROM ldrive+"LACTS2"

USE CLIN 
APPE FROM ldrive+"CLIN2"

USE PROD
APPE FROM ldrive+"PROD2"

USE MAST
APPE FROM ldrive+"MAST2"

USE CALOR
APPE FROM ldrive+"CALOR2"

USE PTB 
APPE FROM ldrive+"PTB2"

USE BST
APPE FROM ldrive+"BST2"

USE CONDC
APPE FROM ldrive+"CONDC2"

USE CALIF
APPE FROM ldrive+"CALIF2"

USE CCS
APPE FROM ldrive+"CCS2"

USE MEDIDA
APPE FROM ldrive+"MEDIDA2"

USE VACUNAS
APPE FROM ldrive+"VACUNAS2"

CLOS ALL
use ldrive+"reg2"
zap
use ldrive+"lacts2"
zap
use ldrive+"calor2"
zap
use ldrive+"prod2"
zap
use ldrive+"mast2"
zap
use ldrive+"ptb2"
zap
use ldrive+"clin2"
zap
use ldrive+"BST2"
zap
use ldrive+"CONDC2"
zap
use ldrive+"CALIF2"
zap
use ldrive+"CCS2"
zap
use ldrive+"MEDIDA2"
zap
use ldrive+"VACUNAS2"
zap

clos all
endi
else
??CHR(7)
WAIT WIND [ DISKETTE SIN ARCHIVOS DE DATOS....VERIFICAR  ] TIMEOUT 2
endi
@17,0 clea to 17,79
@24,67 clea to 24,79
return

*************
PROC respaldo
*************
SN='N'
xDRIVE="A"
@ 22,1 CLEA TO 23,78
@ 22,36 say [RESPALDO] 

@ 10,4 say [NOTA :] COLO W+/&CZ
@ 12,4 FILL TO 16,77 COLO N/N
@ 11,3 fill to 15,76 COLO SCHE 15
@ 12,4 say [ Este programa elaborar  una copia en diskette de todos los archivos de ] COLO SCHE 15
@ 13,4 say [ datos, por lo que es necesario que se cuente con diskettes formateados ] COLO SCHE 15
@ 14,4 say [ para grabar la informaci¢n.                                            ] COLO SCHE 15

@ 23,67 say [Continuar] get sn pict '@M N,S' COLO n/w
READ
IF SN='N'or lastkey()=27
retu
else
SET PATH TO &cmd_nam5,c:\dos
@ 23,60 say [        Drive : ] get xdrive pict '@M A,B' 
read
CLEA
clos all
RESP="BACKUP "+"&cmd_nam5"+" "+"&xDRIVE"+":" 
*RESP="RESPALDA "+SubStr(cmd_nam5,RAt("\",cmd_nam5)+1)+" "+"&xDRIVE"+":"
*REST="RESTAURA "+"&xDRIVE"+":"+" "+sUBSTR...
! CLS
! FOXSWAP &RESP 
do pantalla
SET PATH TO &cmd_nam5
retu
endi
proc pantalla
clea
@ 0,0 TO 2,79 
@ 1,2 SAY DATE() COLO W+/&CZ
@ 1,30 SAY [  CONTROL DE VACAS ] COLO W+/&CZ
@ 1,52 SAY [Ver ]+version
@ 1,78-LEN(NOM) SAY NOM COLO W+/&CZ
@ 3,0 to 20,79 
@ 21,0 to 24,79
@ 22,1 CLEA TO 23,78
@ 24,0 say []

*************
PROC ANALISIS
*************
SN='N'
@ 22,29 SAY [REVISAR BASES DE DATOS] colo gr+/&cz

@ 10,4 say [NOTA :] COLO W+/&CZ
@ 12,4 FILL TO 16,77 COLO N/N
@ 11,3 fill to 15,76 COLO SCHE 15
@ 12,4 say [ Este programa emitir  una lista de los par metros de cada uno de los   ] COLO SCHE 15
@ 13,4 say [ animales registrados para que puedan ser revisados y poder determinar  ] COLO SCHE 15
@ 14,4 say [ si existe algun animal con errores de informaci¢n. (Solo en pantalla)  ] COLO SCHE 15

@ 24,67 SAY [Continuar] GET SN PICT '@M N,S' colo n/w
READ

if LASTKEY()=27 or SN='N'
return
else

@ 4,0 CLEA TO 20,79
USE reg order 2
@ 24,37 say [VACAS] colo w+/&cz

DEFI WIND CORRALD FROM 4,0 TO 20,78 PANEL TITLE [  P A R A M E T R O S   I N D I V I D U A L E S  ]  SHADOW COLO SCHEME 22
DEFI WIND CALOR2 FROM 4,0 TO 20,78 PANEL TITLE [  R E P R O D U C C I O N  ]  SHADOW COLO SCHEME 22
@ 24,67 CLEA TO 24,79
@ 24,72 say [Espere] colo n*/w
@22,1 say [ID] colo w+/b
SCAN
	@24,72 say [Espere] colo n*/w
	@22,4 SAY ID pict"#####"
*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*
* CALCULO DE DIAS INSEMINADA Y DIAS ABIERTOS 
*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*
if stat=[INSEM] .or. stat=[CARGA]
	repl dia with date()-ucal,dab with ucal-fpar
else
	repl dia with 0,dab with date()-fpar
endi	
*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*
ENDS
@22,0 clea to 22,20
@24,1 say [Pagina] colo n/w
@24,9 SAY chr(24)+chr(25) colo w+/&cz
@24,67 clea to 24,79
@24,70 SAY [Esc] COLO N/W
@24,74 SAY [Salir] colo w+/&cz
GO TOP
ACTI WIND CORRALD
BROWSE FIELDS ID:H="ID Num":6,NP:H="Lact",D1S:H="D.1ser":6,DAB:H="D.Abie":6,;
DPR:H="D.Prod":6,PDSC:H="D.Seca":6,DIA:H="D.Inse":6,;
PRM:H="P.Linea":7,P305:H="P.305d",M305:H="P.305EM",;
vr=int((p305/x305)*100):H="V Rel":5,PCIP:H="IP Act" NOEDIT NOAPPE NODELE TIMEOUT 60 IN corrald
rele WIND CORRALD
SET FILT TO

USE CALOR order 2
@ 24,37 SAY [VACAS] colo w+/&cz
GO TOP
ACTI WIND CALOR2
BROWSE FIELDS ID:H="ID Num":6,NP:H='Lact':4,FECHA:H="Fecha   ",TORO:H="ID Toro   ":10,;
SER:H="Servs":5,TEC:H="Tec I":5,STAT:H="E Reprod":7,TORO2:H="ID Toro 2":10,FECHA2:H="Fecha 2":10;
NOEDIT NOAPPE NODELE TIMEOUT 60 IN CALOR2
deact wind CALOR2
SET FILT TO

USE CALOR order 3
@ 24,36 SAY [CRIANZA] colo w+/&cz
GO TOP
ACTI WIND CALOR2
BROWSE FIELDS ID:H="ID Num":6,NP:H='Lact':4,FECHA:H="Fecha   ",TORO:H="ID Toro   ":10,;
SER:H="Servs":5,TEC:H="Tec I":5,STAT:H="E Reprod":7,TORO2:H="ID Toro 2":10,FECHA2:H="Fecha 2":10;
NOEDIT NOAPPE NODELE TIMEOUT 60 IN CALOR2
rele wind CALOR2
SET FILT TO
*-----------*
CLOS DATA
endi
return


*ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
*³    AJUSTE DE ARCHIVOS PARA NUEVAS VERSIONES   ³
*ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PROC REPROD
ON KEY LABEL F9 *
sn="N"

@22,30 SAY [AJUSTE DE PARAMETROS] colo gr+/&cz

@ 10,4 say [NOTA :] COLO W+/&CZ
@ 12,4 FILL TO 16,77 COLO N/N
@ 11,3 fill to 15,76 COLO SCHE 15
@ 12,4 say [ Este programa analizar  los archivos para ajustarlos y que puedan ser  ] COLO SCHE 15
@ 13,4 say [ utilizados en la nueva versi¢n del sistema y que los reportes tanto de ] COLO SCHE 15
@ 14,4 say [ reproducci¢n como de eficiencia etc. esten adecuados al nuevo formato. ] COLO SCHE 15

@ 24,67 SAY [Continuar] GET SN PICT '@M N,S' colo n/w
READ valid lastkey()=13 or lastkey()=27


if LASTKEY()=27 or SN='N'
 clos all
 return
else

** Ajuste de Archivo Reproduccion de VIENTRES
** ------------------------------------------
USE CALOR order 2
@ 24,67 CLEA TO 24,79
@ 24,70 SAY [Espere] colo n*/w
REPL ALL FECHA2 WITH FECHA,toro2 with toro  FOR STAT=[INSEM]
IF EOF()
GO TOP
ENDI

@24,30 say "[                  ]"
@24,31 say chr(219)

DO WHILE .T.

XID=ID
SCAN WHILE ID=XID and STAT#[CARGA]
@ 22,1 say [ID] colo w+/B
@ 22,4 SAY ID pict'#####' colo n/w
ENDS

SKIP-1

XTEC=TEC
XSER=SER
XTOR=TORO
XFECHA=FECHA
XSTAT=STAT

kfecha=FECHA2
ktec=TECP
ktoro=TORO2

SKIP

	DO CASE
	case STAT='CARGA' and ID=XID and XSTAT=[INSEM] and XSER>0
	REPL TEC WITH XTEC,SER WITH XSER,TORO WITH XTOR,TORO2 WITH XTOR,FECHA2 WITH XFECHA

	case STAT='VACIA' and ID=XID and XSTAT=[INSEM]
	REPL TEC WITH XTEC,SER WITH XSER,TORO WITH XTOR,TORO2 WITH XTOR,FECHA2 WITH XFECHA

	case STAT='CARGA' and ID=XID and XSTAT=[VACIA]
	REPL TEC WITH XTEC,SER WITH XSER,TORO WITH XTOR,TORO2 WITH XTOR,FECHA2 WITH kFECHA

	case STAT='CARGA' and ID=XID and XSTAT=[CARGA]
*	REPL TEC WITH XTEC,SER WITH XSER,TORO WITH space(9),TORO2 WITH space(9),FECHA2 WITH B
	REPL TEC WITH XTEC,SER WITH 0,TORO WITH space(9),TORO2 WITH space(9),FECHA2 WITH B

OTHERWISE
	endcase
SKIP
IF LASTKEY()=27 or EOF()
EXIT
ENDI
ENDD
CLOS ALL

** Ajuste de Archivo Reproduccion de CRIANZA
** -----------------------------------------
@24,32 say chr(219)
USE CALOR order 3
@ 24,67 CLEA TO 24,79
@ 24,70 SAY [Espere] colo n*/w
REPL ALL FECHA2 WITH FECHA,toro2 with toro  FOR STAT=[INSEM]
IF EOF()
GO TOP
ENDI
DO WHILE .T.

XID=ID
SCAN WHILE ID=XID and STAT#[CARGA]
@ 22,1 say [ID] colo w+/B
@ 22,4 SAY ID pict'#####' colo n/w
ENDS

SKIP-1

XTEC=TEC
XSER=SER
XTOR=TORO
XFECHA=FECHA
XSTAT=STAT

kfecha=FECHA2
ktec=TECP
ktoro=TORO2

SKIP

	DO CASE
	case STAT='CARGA' and ID=XID and XSTAT=[INSEM] and XSER>0
	REPL TEC WITH XTEC,SER WITH XSER,TORO WITH XTOR,TORO2 WITH XTOR,FECHA2 WITH XFECHA

	case STAT='VACIA' and ID=XID and XSTAT=[INSEM]
	REPL TEC WITH XTEC,SER WITH XSER,TORO WITH XTOR,TORO2 WITH XTOR,FECHA2 WITH XFECHA

	case STAT='CARGA' and ID=XID and XSTAT=[VACIA]
	REPL TEC WITH XTEC,SER WITH XSER,TORO WITH XTOR,TORO2 WITH XTOR,FECHA2 WITH kFECHA

	case STAT='CARGA' and ID=XID and XSTAT=[CARGA]
*	REPL TEC WITH XTEC,SER WITH XSER,TORO WITH space(9),TORO2 WITH space(9),FECHA2 WITH B
	REPL TEC WITH XTEC,SER WITH 0,TORO WITH space(9),TORO2 WITH space(9),FECHA2 WITH B

OTHERWISE
	endcase
SKIP
IF LASTKEY()=27 or EOF()
EXIT
ENDI
ENDD

endif
CLOS ALL

** Ajuste de Dias Seca e Intervalo/Partos
** --------------------------------------
@24,33 say chr(219)
use lacts order 1 in 1
use reg order 2 in 2
set rela to id into reg
scan
@22,4 say id pict'#####' colo n/w
repl REG.pcip with LACTS.ip,REG.pdsc with LACTS.dsc
ends
clos data

** Ajuste de Vacas Inseminadas sobre pre¤ez.
** -----------------------------------------
@24,34 say chr(219)
use calor order 2 in 1
use reg order 2 in 2
set rela to id into reg
scan
@22,4 say id pict'#####' colo n/w
		if rinse#b
			repl REG.prin with 1 
		endi
ends
clos data

** Ajuste de Picos de Lactancia (Kgs. y Dias)
** -----------------------------------------
@24,35 say chr(219)
		use reg  order 2 alias REG in 1
		use prod order 1 alias PROD in 2 
		go top
		set filt to np>0 and fb2=B
scan while not eof()
@22,4 say id pict'#####' colo n/w
xid=REG.id
xnp=REG.np
xpico=0
xdias=0
xdiast=0

	sele 2
	seek xid
	
	if found()
			scan while xid=PROD.id and xnp=PROD.np 
				xdiast=xdiast+dias
				repl diast with xdiast
				if xpico<prd and diast>=15 and diast<=120
				   xpico=prd
				   xdias=diast
				endi				   	
			endscan
	else

	endi
sele 1
repl PLAC with xpico,DPIC with xdias
endscan
clos data

** Ajuste de Factor 305EM Primer Parto Reg.dbf
** -------------------------------------------
@24,36 say chr(219)
use reg order 2
set filt to np=1 and fac=0
repl edad with (fpar-fnac)/30.4
scan
    @22,4 say id pict'#####' colo n/w
	do EQM

if p305>0
repl m305 with p305*fac
endi
endscan

** Ajuste de Factor 305EM Primer Parto Lacts.dbf
** -------------------------------------------
@24,37 say chr(219)
use lacts order 1
set filt to np=1 and fac=0
scan
repl edad with eap
	@22,4 say id pict'#####' colo n/w
	do EQM

repl M305 with p305*fac
endscan
clos data

** Ajuste de Produccion LIFEL en REG de acuerdo a LACTS Anteriores.
** ----------------------------------------------------------------
@24,38 say chr(219)
		use reg order 2 alias REG in 1
		use lacts order 1 alias LAC in 2 
		go top
		set filt to np>0 and fb2=B
scan while not eof()
@22,4 say id pict'#####' colo n/w
xid=REG.id
xnp=REG.np

	sele 2
	seek xid
		xlifel=0
		xregs=0
	
	if found()
			scan while xid=LAC.id 
				if M305>0
				   xlifel=xlifel+M305
				   xregs=xregs+1
				endi				   	
			endscan
	else
	endi
sele 1
repl LIFEL with xlifel,NREG with xregs
endscan
clos data

** Calculo de ERPA
** ----------------------------------------
@24,39 say chr(219)

use reg order 2
go top
scan for np>0 and fb2=b 
@22,4 say id pict'#####' colo n/w
xERPA=0
xval=0
aval=0
if M305>0
	mfac=0.5
	mh2=.25
	aval=(M305/X305)*100
		if LIFEL=0
		mreg=1
		else		
		xval=((LIFEL/NREG)/X305)*100
		mreg=nreg+1
		mfac=(mreg*mfac)/(1+((mreg-1)*mfac))
		endi
	mpta=(mreg*.25)/(1+((mreg-1)*mfac))
	mpta=mpta*(((LIFEL+M305)/mreg)-x305)	
	mpta=mpta/2
    xERPA=mfac*(((LIFEL+M305)/mreg)-x305)	
else
xerpa=0
mpta=0
endi
repl ERPA with xERPA
endscan
clos data

** Ajuste de fecha de Rev. Matriz a Ult. Rev/Tx. y
** -----------------------------------------------------------
** Ajuste de Vacas Candidato a Rastro  Version (2.50)
** -----------------------------------------------------------
** Ajuste de dias en agenda, si la programacion>=date() y los dias=0 Version (2.60)
** ----------------------------------------------------------------------
@24,40 say chr(219)

use reg order 2
scan
	xid=id
    @22,4 say id pict'#####' colo n/w
		if rmat>fpar and rmat>utra
			repl utra with rmat
		endi
		do case
			case mot2=[C.RAST]
				repl FB2 with B,MOT2 with space(6),PSV with "S"

			case mot2=space(6) and PSV=" " or fb2#B
				repl PSV with "N"
		
		endcase	

endscan		

** Ajuste de Crianza Candidato a Rastro  Version (2.50)
** -----------------------------------------------------------
@24,41 say chr(219)

use reg order 3
scan
	xid=id
    @22,4 say id pict'#####' colo n/w
		do case
			case mot1=[C.RAST]
				repl FB1 with B,MOT1 with space(6),PSV with "S"

			case mot1=space(6) and PSV=" " or FB1=B and PSV=" "
				repl PSV with "N"
		
		endcase	

endscan		

** Reemplaza la letra de corral para CRIANZA.
** -----------------------------------------
@24,42 say chr(219)

use corral order 1
	scan
		if tipo="R"
			repl TIPO with "C"
		endi
	endscan		

** Actualiza datos en Archivo PTB (Pruebas) de PTB (ant.)
** -----------------------------------------------------------
@24,43 say chr(219)

use PTB order 1
scan
	xid=id
    @22,4 say id pict'#####' colo n/w
		if empty(PNOM)
			repl PNOM with [TUBERCULOSIS]
		endi
		
		if PNUM=0
			repl PNUM with 1
		endi

		if empty(TIPO)
			repl TIPO with [DC]
		endi
			
endscan		

* Actualiza # de Pruebas TB de Acuerdo al Kardex
* ----------------------------------------------
* Crianza
* -------
@24,44 say chr(219)

sele 1
use reg order 3
sele 2 
use ptb order 3
set filt to pnum=1
sele 1
	scan
	xid=id
    @22,4 say id pict'#####' colo n/w
		sele 2
			xFPU=ctod("")
			xRES=""
			xnpu=0
			
			seek xid
			if found()
				scan while xid=id		
					xfpu=FPU
					xres=RES
					xnpu=xnpu+1
				endscan
			endi
		sele 1
			repl FP1 with xfpu,RS1 with xres,NP1 with xnpu
	endscan						
	clos data

** Ajuste de Clave en Hospital a version 2.60c
** -----------------------------------------------------------
@24,45 say chr(219)

use CLIN
go top
scan
	xid=id
    @22,4 say id pict'#####' colo n/w
	if clave=0 
		repl clave with 8
	endif	
endscan		

* Machos
* ------
@24,46 say chr(219)

sele 1
use sreg order 1
sele 2 
use sptb order 1
set filt to pnum=1
sele 1
	scan
	xid=id
    @22,4 say id pict'#####' colo n/w
		sele 2
			xFPU=ctod("")
			xRES=""
			xnpu=0
			
			seek xid
			if found()
				scan while xid=id		
					xfpu=FPU
					xres=RES
					xnpu=xnpu+1
				endscan
			endi
		sele 1
			repl FP1 with xfpu,RS1 with xres,NP1 with xnpu
	endscan						
	clos data
* -------------------------------

** Ajusta la agenda del sistema a ganado INSEMINADO y PRE¥ADAS 2001.06.02
** ----------------------------------------------------------------------
@24,47 say chr(219)

use REG order 1
set filt to fb2=B
go top

scan
    @22,4 say id pict'#####' colo n/w

	if stat="INSEM" and FPRO<date()
		repl TPA with 5,FPRO with UCAL+Q7,MEDP with [DX PRE¥EZ]
	endif

	if stat="CARGA" and Q46>0 and PALP<UCAL+Q46 and PALP#B and FPRO<UCAL+Q46 and (UCAL+Q46)>date()
		repl TPA with 5,FPRO with UCAL+Q46,MEDP with [DX CONFIRMAR]
	endif

	if stat="CARGA" and Q46=0 and PALP#B and TPA=5
		repl TPA with 0,FPRO with B,MEDP with space(14)
	endif

endscan

** Ajusta la Clave en CALOR.DBF
* -----------------------------
Use CALOR
go top
	scan
    @22,4 say id pict'#####' colo n/w
		do case
			case stat="TRATA" 
				repl CLAVE with 3
			case stat="INSEM" and SER>0
				repl CLAVE with 5	
			case stat="CARGA" or stat="VACIA"
				repl CLAVE with 6
		endcase		
	endscan
@24,48 say chr(219)


** Termina los ajustes ***********
CLOS DATA
return

**************
PROC ACTUALIZA
**************
SN="N"
@ 22,31 say [ACTUALIZAR VERSION] colo gr+/&cz
@ 10,4 say [NOTA :] COLO W+/&CZ
@ 12,4 FILL TO 16,77 COLO N/N
@ 11,3 fill to 15,76 COLO SCHE 15
@ 12,4 say [ Este programa modificar  la estructura de los archivos de acuerdo a la ] COLO SCHE 15
@ 13,4 say [ £ltima versi¢n por lo que se recomienda realizar un RESPALDO DE DATOS  ] COLO SCHE 15
@ 14,4 say [ y reorganizar los archivos despu‚s de utilizar esta opci¢n.            ] COLO SCHE 15

@ 24,67 say [Continuar] get sn pict '@M N,S' COLO n/w
READ
IF LASTKEY()=27 or SN='N'
retu
ELSE
@ 24,1 say [Archivo] colo w+/&cz
@ 24,60 clea to 24,79
@ 24,72 say [Espere] COLO N*/W
clos data
*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
set defa to (cmd_nam5)
*save screen to xx
*!del *.idx
*restore screen from xx
i=1
prog="base"
do while i<35
DO prog+alltrim(str(i,2))
i=i+1
endd

COPY FILE ..\REP1.DEF to REP1.DBF
copy file ..\GNPRUEBA.DEF to GNPRUEBA.DBF
copy file ..\EVENTOS.DEF to EVENTOS.DBF
copy file ..\DIARIO.DEF to DIARIO.DBF
copy file ..\DIARIO2.DEF to DIARIO2.DBF
copy file ..\AGENDA.DEF to AGENDA.DBF

CLOS DATA

	** Comandos para Indexar
	** --------------------
	xsub=(cmd_nam5)
	set defa to (cmd_nam0)
	do CTRLACDX with xsub
	SET DEFA TO (cmd_nam0)
	** --------------------
ENDI

PROC BASE1
@24,10 SAY [0001] 
* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³  Checa y Actualiza CAMBIO.DBF       ³
* ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
CLOS DATA
	if file("CAMBIO3.DBF")
		dele file CAMBIO3.DBF
	endi
	if file("CAMBIO.DBF")
				rena cambio.dbf to cambio3.dbf
  				copy file ..\cambio.def to cambio.dbf
  				use CAMBIO exclusive
  				appe from CAMBIO3
  				clos data
  				dele file CAMBIO3.DBF
	else
  		copy file ..\cambio.def to cambio.dbf
	endi
retu

PROC BASE2
@24,10 SAY [0002] 
* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³   Checa y Actualiza REG.DBF         ³
* ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
CLOS DATA
	if file("REG3.DBF")
		dele file REG3.DBF
	endi
if file("REG.DBF")
	rena REG.DBF to REG3.DBF
    copy file ..\REG.DEF to REG.DBF
  	use REG exclusive
  	appe from REG3
  	clos data
  	dele file REG3.DBF
else
  	copy file ..\REG.DEF to REG.DBF
endi
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
retu

PROC BASE3
@24,10 SAY [0003] 
* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³   Checa y Actualiza CALOR.DBF       ³
* ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
CLOS DATA
	if file("CALOR3.DBF")
		dele file CALOR3.DBF
	endi
if file("CALOR.DBF")
	rena CALOR.DBF to CALOR3.DBF
    copy file ..\CALOR.DEF to CALOR.DBF
  	use CALOR exclusive
  	appe from CALOR3
  	clos data
  	dele file CALOR3.DBF
else
  	copy file ..\CALOR.DEF to CALOR.DBF
endi
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
retu

PROC BASE4
@24,10 SAY [0004] 
* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³   Checa y Actualiza CLIN.DBF        ³
* ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
CLOS DATA
	if file("CLIN3.DBF")
		dele file CLIN3.DBF
	endi
if file("CLIN.DBF")
	rena CLIN.DBF to CLIN3.DBF
    copy file ..\CLIN.DEF to CLIN.DBF
  	use CLIN exclusive
  	appe from CLIN3
  	clos data
  	dele file CLIN3.DBF
else
  	copy file ..\CLIN.DEF to CLIN.DBF
endi
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
retu

PROC BASE5
@24,10 SAY [0005] 
* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³   Checa y Actualiza condc.DBF       ³
* ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
CLOS DATA
	if file("condc3.DBF")
		dele file condc3.DBF
	else
	endi
if file("condc.DBF")
	rena condc.DBF to condc3.DBF
    copy file ..\condc.DEF to condc.DBF
  	use condc exclusive
  	appe from condc3
  	clos data
  	dele file condc3.DBF
else
  	copy file ..\condc.DEF to condc.DBF
endi
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
retu

PROC BASE6
@24,10 SAY [0006] 
* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³   Checa y Actualiza ctoros.DBF      ³
* ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
CLOS DATA
	if file("ctoros3.DBF")
		dele file ctoros3.DBF
	else
	endi
if file("ctoros.DBF")
	rena ctoros.DBF to ctoros3.DBF
    copy file ..\ctoros.DEF to ctoros.DBF
  	use ctoros exclusive
  	appe from ctoros3
  	clos data
  	dele file ctoros3.DBF
else
  	copy file ..\ctoros.DEF to ctoros.DBF
endi
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
retu

PROC BASE7
@24,10 SAY [0007] 
* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³   Checa y Actualiza ccs.DBF         ³
* ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
CLOS DATA
	if file("ccs3.DBF")
		dele file ccs3.DBF
	else
	endi
if file("ccs.DBF")
	rena ccs.DBF to ccs3.DBF
    copy file ..\ccs.DEF to ccs.DBF
  	use ccs exclusive
  	appe from ccs3
  	clos data
  	dele file ccs3.DBF
else
  	copy file ..\ccs.DEF to ccs.DBF
endi
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
retu

PROC BASE8
@24,10 SAY [0008] 
* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³   Checa y Actualiza param.DBF       ³
* ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
CLOS DATA
	if file("param3.DBF")
		dele file param3.DBF
	else
	endi
if file("param.DBF")
	rena param.DBF to param3.DBF
    copy file ..\param.DEF to param.DBF
  	use param exclusive
  	appe from param3
  	clos data
  	dele file param3.DBF
else
  	copy file ..\param.DEF to param.DBF
endi
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
retu

PROC BASE9
@24,10 SAY [0009] 
* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³   Checa y Actualiza toros.DBF       ³
* ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
CLOS DATA
	if file("toros3.DBF")
		dele file toros3.DBF
	else
	endi
if file("toros.DBF")
	rena toros.DBF to toros3.DBF
    copy file ..\toros.DEF to toros.DBF
  	use toros exclusive
  	appe from toros3
  	clos data
  	dele file toros3.DBF
else
  	copy file ..\toros.DEF to toros.DBF
endi
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
retu

PROC BASE10
@24,10 SAY [0010] 
* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³   Checa y Actualiza tecnic.DBF      ³
* ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
CLOS DATA
	if file("tecnic3.DBF")
		dele file tecnic3.DBF
	else
	endi
if file("tecnic.DBF")
	rena tecnic.DBF to tecnic3.DBF
    copy file ..\tecnic.DEF to tecnic.DBF
  	use tecnic exclusive
  	appe from tecnic3
  	clos data
  	dele file tecnic3.DBF
else
  	copy file ..\tecnic.DEF to tecnic.DBF
endi
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
retu

PROC BASE11
@24,10 SAY [0011] 
* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³   Checa y Actualiza PARAM2.DBF      ³
* ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
CLOS DATA
	if file("PARAM23.DBF")
		dele file PARAM23.DBF
	else
	endi
if file("PARAM2.DBF")
	rena PARAM2.DBF to PARAM23.DBF
    copy file ..\PARAM2.DEF to PARAM2.DBF
  	use PARAM2 exclusive
  	appe from PARAM23
  	clos data
  	dele file PARAM23.DBF
else
  	copy file ..\PARAM2.DEF to PARAM2.DBF
endi
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
retu

PROC BASE12
@24,10 SAY [0012] 
* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³   Checa y Actualiza CALIF.DBF       ³
* ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
CLOS DATA
	if file("CALIF3.DBF")
		dele file CALIF3.DBF
	else
	endi
if file("CALIF.DBF")
	rena CALIF.DBF to CALIF3.DBF
    copy file ..\CALIF.DEF to CALIF.DBF
  	use CALIF exclusive
  	appe from CALIF3
  	clos data
  	dele file CALIF3.DBF
else
  	copy file ..\CALIF.DEF to CALIF.DBF
endi
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
retu

PROC BASE13
@24,10 SAY [0013] 
* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³   Checa y Actualiza LOGREP.DBF      ³
* ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
CLOS DATA
	if file("LOGREP3.DBF")
		dele file LOGREP3.DBF
	else
	endi
if file("LOGREP.DBF")
	rena LOGREP.DBF to LOGREP3.DBF
    copy file ..\LOGREP.DEF to LOGREP.DBF
  	use LOGREP exclusive
  	ZAP
  	appe from LOGREP3
	clos data
  	dele file LOGREP3.DBF
else
  	copy file ..\LOGREP.DEF to LOGREP.DBF
endi
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
retu

PROC BASE14
@24,10 SAY [0014] 
* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³   Checa y Actualiza REPORTES.DBF    ³
* ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
CLOS DATA
  	copy file ..\REPORTES.DEF to REPORTES.DBF
	retu

PROC BASE15
@24,10 SAY [0015] 

* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³   Checa y Actualiza MEDIDA.DBF      ³
* ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
CLOS DATA
	if file("MEDIDA3.DBF")
		dele file MEDIDA3.DBF
	else
	endi
if file("MEDIDA.DBF")
	rena MEDIDA.DBF to MEDIDA3.DBF
    copy file ..\MEDIDA.DEF to MEDIDA.DBF
  	use MEDIDA exclusive
  	appe from MEDIDA3
  	clos data
  	dele file MEDIDA3.DBF
else
  	copy file ..\MEDIDA.DEF to MEDIDA.DBF
endi
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
retu

PROC BASE16
@24,10 SAY [0016] 
* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³   Checa y Actualiza PROD.DBF        ³
* ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
CLOS DATA
	if file("PROD3.DBF")
		dele file PROD3.DBF
	else
	endi
if file("PROD.DBF")
	rena PROD.DBF to PROD3.DBF
    copy file ..\PROD.DEF to PROD.DBF
  	use PROD exclusive
  	appe from PROD3
  	clos data
  	dele file PROD3.DBF
else
  	copy file ..\PROD.DEF to PROD.DBF
endi
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
retu

PROC BASE17
@24,10 SAY [0017] 
* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³   Checa y Actualiza VACUNAS.DBF     ³
* ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
CLOS DATA
	if file("VACUNAS3.DBF")
		dele file VACUNAS3.DBF
	else
	endi
if file("VACUNAS.DBF")
	rena VACUNAS.DBF to VACUNAS3.DBF
    copy file ..\VACUNAS.DEF to VACUNAS.DBF
  	use VACUNAS exclusive
  	appe from VACUNAS3
  	clos data
  	dele file VACUNAS3.DBF
else
  	copy file ..\VACUNAS.DEF to VACUNAS.DBF
endi
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
retu

PROC BASE18
@24,10 SAY [0018] 
* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³   Checa y Actualiza LACTS.DBF       ³
* ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
CLOS DATA
	if file("LACTS3.DBF")
		dele file LACTS3.DBF
	endi
if file("LACTS.DBF")
	rena LACTS.DBF to LACTS3.DBF
    copy file ..\LACTS.DEF to LACTS.DBF
  	use LACTS exclusive
  	appe from LACTS3
  	clos data
  	dele file LACTS3.DBF
else
  	copy file ..\LACTS.DEF to LACTS.DBF
endi
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
retu

PROC BASE19
@24,10 SAY [0019] 
* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³   Checa y Actualiza PTB.DBF         ³
* ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
CLOS DATA
	if file("PTB3.DBF")
		dele file PTB3.DBF
	endi
if file("PTB.DBF")
	rena PTB.DBF to PTB3.DBF
    copy file ..\PTB.DEF to PTB.DBF
  	use PTB exclusive
  	appe from PTB3
  	clos data
  	dele file PTB3.DBF
else
  	copy file ..\PTB.DEF to PTB.DBF
endi
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
retu

PROC BASE20
@24,10 SAY [0020] 
* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³   Checa y Actualiza MAST.DBF        ³
* ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
CLOS DATA
	if file("MAST3.DBF")
		dele file MAST3.DBF
	endi
if file("MAST.DBF")
	rena MAST.DBF to MAST3.DBF
    copy file ..\MAST.DEF to MAST.DBF
  	use MAST exclusive
  	appe from MAST3
  	clos data
  	dele file MAST3.DBF
else
  	copy file ..\MAST.DEF to MAST.DBF
endi
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
retu

** Archivo de Becerros
** -------------------
PROC BASE21
@24,10 SAY [0021] 
* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³   Checa y Actualiza SREG.DBF        ³
* ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
CLOS DATA
	if file("SREG3.DBF")
		dele file SREG3.DBF
	endi
if file("SREG.DBF")
	rena SREG.DBF to SREG3.DBF
    copy file ..\SREG.DEF to SREG.DBF
  	use SREG exclusive
  	appe from SREG3
  	clos data
  	dele file SREG3.DBF
else
  	copy file ..\SREG.DEF to SREG.DBF
endi
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
retu

PROC BASE22
@24,10 SAY [0022] 
* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³   Checa y Actualiza SCLIN.DBF       ³
* ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
CLOS DATA
	if file("SCLIN3.DBF")
		dele file SCLIN3.DBF
	endi
if file("SCLIN.DBF")
	rena SCLIN.DBF to SCLIN3.DBF
    copy file ..\SCLIN.DEF to SCLIN.DBF
  	use SCLIN exclusive
  	appe from SCLIN3
  	clos data
  	dele file SCLIN3.DBF
else
  	copy file ..\SCLIN.DEF to SCLIN.DBF
endi
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
retu

PROC BASE23
@24,10 SAY [0023] 
* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³   Checa y Actualiza SMED.DBF        ³
* ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
CLOS DATA
	if file("SMED3.DBF")
		dele file SMED3.DBF
	endi
if file("SMED.DBF")
	rena SMED.DBF to SMED3.DBF
    copy file ..\SMED.DEF to SMED.DBF
  	use SMED exclusive
  	appe from SMED3
  	clos data
  	dele file SMED3.DBF
else
  	copy file ..\SMED.DEF to SMED.DBF
endi
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
retu

PROC BASE24
@24,10 SAY [0024] 
* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³   Checa y Actualiza SVAC.DBF        ³
* ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
CLOS DATA
	if file("SVAC3.DBF")
		dele file SVAC3.DBF
	endi
if file("SVAC.DBF")
	rena SVAC.DBF to SVAC3.DBF
    copy file ..\SVAC.DEF to SVAC.DBF
  	use SVAC exclusive
  	appe from SVAC3
  	clos data
  	dele file SVAC3.DBF
else
  	copy file ..\SVAC.DEF to SVAC.DBF
endi
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
retu

PROC BASE25
@24,10 SAY [0025] 
* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³   Checa y Actualiza SPTB.DBF        ³
* ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
CLOS DATA
	if file("SPTB3.DBF")
		dele file SPTB3.DBF
	endi
if file("SPTB.DBF")
	rena SPTB.DBF to SPTB3.DBF
    copy file ..\SPTB.DEF to SPTB.DBF
  	use SPTB exclusive
  	appe from SPTB3
  	clos data
  	dele file SPTB3.DBF
else
  	copy file ..\SPTB.DEF to SPTB.DBF
endi
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
retu

PROC BASE26
@24,10 SAY [0026] 
* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³   Checa y Actualiza PESADAS.DBF     ³
* ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
CLOS DATA
	if file("PESADAS3.DBF")
		dele file PESADAS3.DBF
	endi
if file("PESADAS.DBF")
	rena PESADAS.DBF to PESADAS3.DBF
    copy file ..\PESADAS.DEF to PESADAS.DBF
  	use PESADAS exclusive
  	appe from PESADAS3
  	clos data
  	dele file PESADAS3.DBF
else
  	copy file ..\PESADAS.DEF to PESADAS.DBF
endi
retu

PROC BASE27
@24,10 SAY [0027] 
* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³   Checa y Actualiza CORRAL.DBF      ³
* ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
CLOS DATA
	if file("CORRAL3.DBF")
		dele file CORRAL3.DBF
	endi
if file("CORRAL.DBF")
	rena CORRAL.DBF to CORRAL3.DBF
    copy file ..\CORRAL.DEF to CORRAL.DBF
  	use CORRAL exclusive
  	appe from CORRAL3
  	clos data
  	dele file CORRAL3.DBF
else
  	copy file ..\CORRAL.DEF to CORRAL.DBF
endi

*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
retu

PROC BASE28
@24,10 SAY [0028] 
* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³   Checa y Actualiza BAJAS.DBF       ³
* ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
CLOS DATA
	if file("BAJAS3.DBF")
		dele file BAJAS3.DBF
	endi
if file("BAJAS.DBF")
	rena BAJAS.DBF to BAJAS3.DBF
    copy file ..\BAJAS.DEF to BAJAS.DBF
  	use BAJAS exclusive
  	appe from BAJAS3
  	clos data
  	dele file BAJAS3.DBF
else
  	copy file ..\BAJAS.DEF to BAJAS.DBF
endi

*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
retu

PROC BASE29
@24,10 SAY [0029] 
* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³   Checa y Actualiza SBAJAS.DBF      ³
* ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
CLOS DATA
	if file("SBAJAS3.DBF")
		dele file SBAJAS3.DBF
	endi
if file("SBAJAS.DBF")
	rena SBAJAS.DBF to SBAJAS3.DBF
    copy file ..\SBAJAS.DEF to SBAJAS.DBF
  	use SBAJAS exclusive
  	appe from SBAJAS3
  	clos data
  	dele file SBAJAS3.DBF
else
  	copy file ..\SBAJAS.DEF to SBAJAS.DBF
endi
retu

PROC BASE30
@24,10 SAY [0030] 
* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³   Checa y Actualiza ABORTOS.DBF     ³
* ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
CLOS DATA
	if file("ABORTOS3.DBF")
		dele file ABORTOS3.DBF
	endi
if file("ABORTOS.DBF")
	rena ABORTOS.DBF to ABORTOS3.DBF
    copy file ..\ABORTOS.DEF to ABORTOS.DBF
  	use ABORTOS exclusive
  	appe from ABORTOS3
  	clos data
  	dele file ABORTOS3.DBF
else
  	copy file ..\ABORTOS.DEF to ABORTOS.DBF
endi
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
retu

PROC BASE31
@24,10 SAY [0031] 
* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³   Checa y Actualiza MEDIC.DBF       ³
* ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
CLOS DATA
	if file("MEDIC3.DBF")
		dele file MEDIC3.DBF
	endi
if file("MEDIC.DBF")
	rena MEDIC.DBF to MEDIC3.DBF
    copy file ..\MEDIC.DEF to MEDIC.DBF
  	use MEDIC exclusive
  	appe from MEDIC3
  	clos data
  	dele file MEDIC3.DBF
else
  	copy file ..\MEDIC.DEF to MEDIC.DBF
endi
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
retu

PROC BASE32
@24,10 SAY [0032] 
* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³   Checa y Actualiza GNCODIGO.DBF    ³
* ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
CLOS DATA
	if file("GNCODI03.DBF")
		dele file GNCODI03.DBF
	endi
if file("GNCODIGO.DBF")
	rena GNCODIGO.DBF to GNCODI03.DBF
    copy file ..\GNCODIGO.DEF to GNCODIGO.DBF
  	use GNCODIGO exclusive
  	appe from GNCODI03
  	clos data
  	dele file GNCODI03.DBF
else
  	copy file ..\GNCODIGO.DEF to GNCODIGO.DBF
endi
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
retu

PROC BASE33
@24,10 SAY [0033] 
* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³   Checa y Actualiza ENFERM.DBF      ³
* ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
CLOS DATA
	if file("ENFERM3.DBF")
		dele file ENFERM3.DBF
	endi
if file("ENFERM.DBF")
	rena ENFERM.DBF to ENFERM3.DBF
    copy file ..\ENFERM.DEF to ENFERM.DBF
  	use ENFERM exclusive
  	appe from ENFERM3
  	clos data
  	dele file ENFERM3.DBF
else
  	copy file ..\ENFERM.DEF to ENFERM.DBF
endi
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼

PROC BASE34
@24,10 SAY [0034] 
* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³   Checa y Actualiza GNVACUNA.DBF    ³
* ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
CLOS DATA
	if file("GNVACUN3.DBF")
		dele file GNVACUN3.DBF
	endi
if file("GNVACUNA.DBF")
	rena GNVACUNA.DBF to GNVACUN3.DBF
    copy file ..\GNVACUNA.DEF to GNVACUNA.DBF
  	use GNVACUNA exclusive
  	appe from GNVACUN3
  	clos data
  	dele file GNVACUN3.DBF
else
  	copy file ..\GNVACUNA.DEF to GNVACUNA.DBF
endi
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼

??chr(7)
return

