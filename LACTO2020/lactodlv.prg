* Poner Clave de Acceso al checar con el Sentinel
* -----------------------------------------------
*Use LDATOS
*If LINT=SYS(2007,AllTrim(_SERIE)+"GABY2.55")
*	Use
*Else
*	MessageBox("La clave para este modulo no es valida. VERIFICAR !!",0+16,"DAIRYFOX - Aviso")
*	Use
*	Return
*EndIf	

LOCAL sn,xtexto,B
SET CENTURY ON
SET SAFETY OFF
SET DECIMALS TO 1
SET DELETED ON
xTFECHA=SET("Date")
SET DATE YMD

B = CTOD("")
CLOSE TABLES ALL
xtexto = "Esta opcion actualizara la informacion del archivo de datos del sistema"
xtexto = xtexto + CHR(13) + "ALPRO, con la informacion capturada en DAIRYFOX."
xtexto = xtexto + CHR(13) + CHR(13) + "Se recomienda realizar un respaldo de datos de ALPRO."
xtexto = xtexto + CHR(13) + CHR(13) + "Desea Continuar?"
sn = MESSAGEBOX(xtexto,1+32,"DAIRYFOX - Interfase Alpro 6.31")

If sn=2
Else

	** ----------------------
	** Solicita Datos a ALPRO
	** ----------------------
	if file("LACTODLV.ISO")
		delete file LACTODLV.ISO
	endif
	LACTODLV=FCREATE('LACTODLV.ISO')

	** Sender Version
	** --------------
	ww="DH9900010090000624000900008 80"             +chr(13)+chr(10)
	ww=ww+"DH99000100900002 8000900002 8000900002 80"+chr(13)+chr(10)
	ww=ww+"VH990001 ALPRO6 ADED9411AADB9611"+chr(13)+chr(10)

	ww=ww+"RN990001"
	ww=ww+"00300001"+"040"  && Cowno
	ww=ww+"00300002"+"040"  && Transponder
	ww=ww+"00300003"+"020"  && Group
	ww=ww+"00300017"+"010"  && Slaughter Flag
	ww=ww+"00300033"+"080"  && Birth Date
	ww=ww+"00300035"+"080"  && Calving Date
	ww=ww+"00300034"+"080"  && Heat Date
	ww=ww+"00300357"+"080"  && Insem Date
	ww=ww+"00300194"+"080"  && Dry Off Date
	ww=ww+"00300036"+"020"  && Lactations
	ww=ww+"00300037"+"020"  && Inseminations
	ww=ww+"00300374"+"150"  && Sire Name
	ww=ww+"00300347"+"010"  && Delivery
	ww=ww+"00300339"+"040"  && Calf 1 ID
	ww=ww+"00300340"+"040"  && Calf 2 ID
	ww=ww+"00300032"+"010"  && Breed State
	ww=ww+"00300142"+"031"  && Milk_Yesterday 1
	ww=ww+"00300145"+"031"  && Milk_Yesterday 2
	ww=ww+"00300148"+"031"  && Milk_Yesterday 3
	ww=ww+"00300151"+"031"  && Milk_Yesterday 4
	ww=ww+"00304036"+"031"  && Total_Yieldk_Yesterday 
	ww=ww+"00300282"+"050"  && ActTag_No
	ww=ww+"00300327"+"020"  && Last_Send_Time
	ww=ww+"00300328"+"020"  && Heat_Start_Time
	ww=ww+"00300329"+"030"  && Heat_Index

	ww=ww+chr(13)+chr(10)
	ww=ww+"EN"+chr(13)+chr(10)
	ww=ww+"ZN"+chr(13)+chr(10)

	=FWRITE(LACTODLV,ww)
	ww=""
	erase LACTODLV.dat
	=FCLOSE(LACTODLV)
	copy file LACTODLV.ISO to LACTODLV.inf
	
	xdirentr2 = "LACTODLV.DAT"
	xdirentra = "LACTODLV.ISO"
	xdirsalid = "DATABASE.APW"

	create table ALPROTEM (ID n(4),IDE c(4),CORR n(2),PSV n(1),FNAC d(8),FPAR d(8),;
                      UCEL d(8),UCAL d(8),FSEC d(8),NP n(2),NS n(2),IDT c(15),;
					  OBS n(1),C1 n(4),C2 n(4),TODO n(1),P1 n(4,1),P2 n(4,1),;
					  P3 n(4,1),P4 n(4,1),TOT n(4,1),ACT n(5),LST n(2),HST n(2),;
					  HIX c(3))

	FOR i=1 TO 10
		WAIT "Procesando Informacion..." WINDOW TIMEOUT 1
	NEXT
	
	DO WHILE !file("LACTODLV.DAT") 
		*Esperar a que Alpro grabe y libere el archivo.
		ENDDO
	
	If FILE("LACTODLV.DAT")
		appe from &xdirentr2 deli with TAB

	USE ALPROTEM
	INDE ON ID TAG ID
	CLOSE ALL

	DO COMPARA
	Else
	EndIf
ENDIF
Set DATE TO &XTFECHA
RETURN

********************************************************************
PROCEDURE COMPARA
********************************************************************
** Empieza a Comparar
** ------------------
erase LACTODLV.inf
erase c:\Alpro\salida.log

SALIDA=FCREATE('LACTODLV.INF')
ww="DH9900010090000624000900008 80"             +chr(13)+chr(10)
ww=ww+"VH990001 Alfa Laval Alpro/ISO       6.31"+chr(13)+chr(10)
ww=ww+"DH99000100900002 8000900002 8000900002 80"+chr(13)+chr(10)
ww=ww+"VH990001 ALPRO6 ADED9411AADB9611"+chr(13)+chr(10)
=FWRITE(SALIDA,ww)

ww=""

use REG order 1 in 1 alias REG
use CALOR order 1 in 2 alias CAL

use ALPROTEM order 1 in 3 alias ALP

** ALTAS, BAJAS Y CAMBIOS EN ALPRO
** -------------------------------
sele 1
go top
altas = .f.
cont = 0
scan 
	xid=REG.ID
	ww=""
*	wait window nowait "Espere procesando vaca "+ str(reg.id)
		sele 3
			seek xid
				if not found() and REG.fb2=B 
					ww=   "DN990001003120080400030000302000300033080003040640100030003508000300036020"+chr(13)+CHR(10)
					ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
					ww=ww+str(iif(REG.corr=0,99,REG.corr),2)
					ww=ww+substr(dtoc(REG.fnac),1,4)+substr(dtoc(REG.fnac),6,2)+right(dtoc(REG.fnac),2)
					ww=ww+"1"
     				ww=ww+substr(dtoc(REG.fpar),1,4)+substr(dtoc(REG.fpar),6,2)+right(dtoc(REG.fpar),2)
					ww=ww+str(REG.np,2)
					ww=ww+chr(13)+chr(10)
					=FWRITE(SALIDA,ww)
					ww=""
				endif
endscan
sele 3
go top
scan
	ww=""
	xid=ALP.id
*	wait wind nowait [Espere un Momento]+str(xid,5)
	sele 1
		seek xid
			if not FOUND()
				ww=   "DN99000100312007040"+chr(13)+CHR(10)
				ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
				ww=ww+chr(13)+chr(10)
				=FWRITE(SALIDA,ww)
				ww=""
			else
				if REG.fb2#B
					ww=   "DN99000100312007040"+chr(13)+CHR(10)
					ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
					ww=ww+chr(13)+chr(10)
					=FWRITE(SALIDA,ww)
					ww=""
				ELSE
					Replace Reg.P1 WITH Alp.P1, Reg.P2 WITH Alp.P2,Reg.P3 WITH Alp.p3
					DO CAMBIOS1					
				endif
			endif
		ww=   "DN9900010030000104000300348150"+chr(13)+CHR(10)
		ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
		ww=ww+REG.stat+"  "+REG.codigo
		ww=ww+chr(13)+chr(10)
		=FWRITE(SALIDA,ww)
		ww=""
endscan
ww=ww+"EN"+chr(13)+chr(10)
ww=ww+"ZN"+chr(13)+chr(10)

=FWRITE(Salida,ww)
ww=""
=FCLOSE(Salida)

COPY FILE LACTODLV.INF TO LACTODLV.ISO
CLOSE ALL
RETURN

********************************************************************
PROCEDURE CAMBIOS1
********************************************************************
** Corral
** ------
if ALP.corr#Reg.corr and REG.corr#0
	ww=   "DN9900010030000104000300003020"+chr(13)+CHR(10)
	ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
	ww=ww+str(REG.corr,2)
	ww=ww+chr(13)+chr(10)
	=FWRITE(SALIDA,ww)
	ww = ""
endif

** Transponder
** -----------
if ALP.ide#REG.ide
	repl REG.ide with ALP.ide
endif		

** Marca de Baja
** -------------
if ALP.psv#iif(Reg.psv="S",1,0)
	ww=   "DN9900010030000104000300017010"+chr(13)+CHR(10)
	ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)+iif(REG.psv="S","1","0")
	ww=ww+chr(13)+chr(10)
	=FWRITE(SALIDA,ww)
	ww = ""
endif

** Fecha de Nacimiento
** -------------------
if ALP.fnac#Reg.fnac
	ww=   "DN9900010030000104000300033080"+chr(13)+CHR(10)
	ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
	ww=ww+substr(dtoc(REG.fnac),1,4)+substr(dtoc(REG.fnac),6,2)+right(dtoc(REG.fnac),2)
	ww=ww+chr(13)+chr(10)
	=FWRITE(SALIDA,ww)
	ww = ""
endif

** Crias
** -----
if ALP.c1#REG.IDC and REG.idc#0
	ww=   "DN9900010030000104000300339040"+chr(13)+CHR(10)
	ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)+;
	RIGHT("0000"+ALLTRIM(STR(REG.idc)),4)
	ww=ww+chr(13)+chr(10)
	=FWRITE(SALIDA,ww)
	ww = ""
endif

if ALP.c2#REG.IDC2 and REG.idc2#0
	ww=   "DN9900010030000104000300340040"+chr(13)+CHR(10)
	ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)+RIGHT("0000"+ALLTRIM(STR(REG.idc2)),4)
	ww=ww+chr(13)+chr(10)
	=FWRITE(SALIDA,ww)
	ww = ""
endif

** Tipo de Parto
** -------------
if ALP.obs#iif(REG.obs="NORMAL",1,3)
	ww=   "DN9900010030000104000300347010"+chr(13)+CHR(10)
	ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)+iif(REG.obs="NORMAL","1","3")
	ww=ww+chr(13)+chr(10)
	=FWRITE(SALIDA,ww)
	ww = ""
endif

** Copia Produccion a DAIRYFOX
** ---------------------------
repl REG.p1 with ALP.p1,REG.p2 with ALP.p2,REG.p3 with ALP.p3,REG.p4 with ALP.p4,REG.tot with ALP.tot
 
** Para Verificar numero de lactancia y fecha de parto
** ---------------------------------------------------
if ALP.np#reg.np or ALP.fpar#REG.fpar
	ww=   "DN990001003000010400030003508000300036020"+chr(13)+CHR(10)
	ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
	ww=ww+substr(dtoc(REG.fpar),1,4)+substr(dtoc(REG.fpar),6,2)+right(dtoc(REG.fpar),2)
	ww=ww+str(REG.np,2)
	ww=ww+chr(13)+chr(10)
	=FWRITE(SALIDA,ww)
	ww = ""
endif

*****************
**** RETURN *****
*****************

** Reproduccion
** ------------
if ALP.todo=0 
	do case
	case left(REG.stat,1)="F"
	 *nada

	case left(REG.stat,1)="S" 
		if REG.ucal#ALP.ucel and reg.ucal#ctod("") 
			ww=   "DN990001003000010400030405101000300034080"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"1"
			ww=ww+substr(dtoc(REG.ucal),1,4)+substr(dtoc(REG.ucal),6,2)+right(dtoc(REG.ucal),2)
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww = ""
		endif

	case left(REG.stat,1)="L" and REG.ucal#ALP.ucel and reg.ucal#ctod("")
		ww=   "DN990001003000010400030405101000300034080"+chr(13)+CHR(10)
		ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
		ww=ww+"1"
		ww=ww+substr(dtoc(REG.ucal),1,4)+substr(dtoc(REG.ucal),6,2)+right(dtoc(REG.ucal),2)
		ww=ww+chr(13)+chr(10)
		=FWRITE(SALIDA,ww)
		ww = ""

	case left(REG.stat,1)="I"
		** Manda Inseminacion
		** ------------------
		ww=   "DN9900010030000104000304054010003000340800030035708000300374150"+chr(13)+CHR(10)
		ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)+;
			   "1"+;
				substr(dtoc(REG.ucal),1,4)+substr(dtoc(REG.ucal),6,2)+right(dtoc(REG.ucal),2)+;
				substr(dtoc(REG.ucal),1,4)+substr(dtoc(REG.ucal),6,2)+right(dtoc(REG.ucal),2)+;
				left(REG.idt+"               ",15)
		ww=ww+chr(13)+chr(10)
		=FWRITE(SALIDA,ww)
		ww=""

	case left(REG.stat,1)="V" 
	* nada

 	case left(REG.stat,1)="C" 
		ww=   "DN9900010030000104000300348150"+chr(13)+CHR(10)
		ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
		ww=ww+"INSEM Previo   "
		ww=ww+chr(13)+chr(10)
		=FWRITE(SALIDA,ww)
		ww = ""

	case left(REG.stat,1)="T"
		ww=   "DN9900010030000104000300348150"+chr(13)+CHR(10)
		ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
		ww=ww+"TRATAMIENTO    "
		ww=ww+chr(13)+chr(10)
		=FWRITE(SALIDA,ww)
		ww = ""

	case left(REG.stat,2)="AB" 
		ww=   "DN9900010030000104000300348150"+chr(13)+CHR(10)
		ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
		ww=ww+"ABORTO         "
		ww=ww+chr(13)+chr(10)
		=FWRITE(SALIDA,ww)
		ww=""

	case left(REG.stat,2)="AN" 
		ww=   "DN9900010030000104000300348150"+chr(13)+CHR(10)
		ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
		ww=ww+"ANESTRO        "
		ww=ww+chr(13)+chr(10)
		=FWRITE(SALIDA,ww)
		ww=""
	 
	endcase
endif

** to do 1 report insemination 
**********************************************************************+
if ALP.todo=1 
	do case
	case left(REG.stat,1)="F"
	 *nada

	case left(REG.stat,1)="S" 
		if REG.ucal#ALP.ucel and reg.ucal#ctod("")
			ww=   "DN990001003000010400030405101000300034080"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"1"
			ww=ww+substr(dtoc(REG.ucal),1,4)+substr(dtoc(REG.ucal),6,2)+right(dtoc(REG.ucal),2)
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""
		endif


	case left(REG.stat,1)="L" and REG.ucal#ALP.ucel and reg.ucal#ctod("")
		ww=   "DN990001003000010400030405101000300034080"+chr(13)+CHR(10)
		ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
		ww=ww+"1"
		ww=ww+substr(dtoc(REG.ucal),1,4)+substr(dtoc(REG.ucal),6,2)+right(dtoc(REG.ucal),2)
		ww=ww+chr(13)+chr(10)
		=FWRITE(SALIDA,ww)
		ww=""

	case left(REG.stat,1)="I"
		if REG.ucal#ALP.ucal
		ww=   "DN9900010030000104000304054010003000340800030035708000300374150"+chr(13)+CHR(10)
		ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)+;
			   "1"+;
				substr(dtoc(REG.ucal),1,4)+substr(dtoc(REG.ucal),6,2)+right(dtoc(REG.ucal),2)+;
				substr(dtoc(REG.ucal),1,4)+substr(dtoc(REG.ucal),6,2)+right(dtoc(REG.ucal),2)+;
				left(REG.idt+"               ",15)
		ww=ww+chr(13)+chr(10)
		=FWRITE(SALIDA,ww)
		ww=""
		endif
		
	case left(REG.stat,1)="V" 
* nada

 	case left(REG.stat,1)="C" 
		ww=   "DN990001003000010400030405401000300037020003000340800030035708000300374150"+chr(13)+CHR(10)
		ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)+;
			   "1"+str(REG.ns,2)+;
				substr(dtoc(REG.ucal),1,4)+substr(dtoc(REG.ucal),6,2)+right(dtoc(REG.ucal),2)+;
				substr(dtoc(REG.ucal),1,4)+substr(dtoc(REG.ucal),6,2)+right(dtoc(REG.ucal),2)+;
				left(REG.idt+"               ",15)
		ww=ww+chr(13)+chr(10)

		ww=ww+"DN9900010030000104000300348150"+chr(13)+CHR(10)
		ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
		ww=ww+"CARGA Dx Previo"
		ww=ww+chr(13)+chr(10)
		=FWRITE(SALIDA,ww)
		ww=""

	case left(REG.stat,1)="T"
		ww=   "DN9900010030000104000300348150"+chr(13)+CHR(10)
		ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
		ww=ww+"TRATAMIENTO    "
		ww=ww+chr(13)+chr(10)
		=FWRITE(SALIDA,ww)
		ww=""

	case left(REG.stat,2)="AB" 
		ww=   "DN9900010030000104000300348150"+chr(13)+CHR(10)
		ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
		ww=ww+"ABORTO         "
		ww=ww+chr(13)+chr(10)
		=FWRITE(SALIDA,ww)
		ww=""

	case left(REG.stat,2)="AN" 
		ww=   "DN9900010030000104000300348150"+chr(13)+CHR(10)
		ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
		ww=ww+"ANESTRO        "
		ww=ww+chr(13)+chr(10)
		=FWRITE(SALIDA,ww)
		ww=""
	 
	endcase
endif

** to do 2 Check Pregnancy 1 
**********************************************************************+
If ALP.todo=2 or ALP.todo=3 
	do case
	case left(REG.stat,1)="F"
			ww=   "DN9900010030000104000304058010"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"0"
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""

	case left(REG.stat,1)="S" 
			ww=   "DN9900010030000104000304058010"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"0"
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""

		if REG.ucal#ALP.ucel and reg.ucal#ctod("")
			ww=   "DN990001003000010400030405101000300034080"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"1"
			ww=ww+substr(dtoc(REG.ucal),1,4)+substr(dtoc(REG.ucal),6,2)+right(dtoc(REG.ucal),2)
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""
		endif

	case left(REG.stat,1)="L" and REG.ucal#ALP.ucel and reg.ucal#ctod("")
			ww=   "DN9900010030000104000304058010"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"0"
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""

		ww=   "DN990001003000010400030405101000300034080"+chr(13)+CHR(10)
		ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
		ww=ww+"1"
		ww=ww+substr(dtoc(REG.ucal),1,4)+substr(dtoc(REG.ucal),6,2)+right(dtoc(REG.ucal),2)
		ww=ww+chr(13)+chr(10)
		=FWRITE(SALIDA,ww)
		ww=""

	case left(REG.stat,1)="I"
			ww=   "DN9900010030000104000304058010"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"0"
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""

		if REG.ucal#ALP.ucal
		ww=   "DN9900010030000104000304054010003000340800030035708000300374150"+chr(13)+CHR(10)
		ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)+;
			   "1"+;
				substr(dtoc(REG.ucal),1,4)+substr(dtoc(REG.ucal),6,2)+right(dtoc(REG.ucal),2)+;
				substr(dtoc(REG.ucal),1,4)+substr(dtoc(REG.ucal),6,2)+right(dtoc(REG.ucal),2)+;
				left(REG.idt+"               ",15)
		ww=ww+chr(13)+chr(10)
		=FWRITE(SALIDA,ww)
		ww=""
		endif

	case left(REG.stat,1)="V" 
			ww=   "DN9900010030000104000304058010"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"0"
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""

 	case left(REG.stat,1)="C" 
			ww=   "DN9900010030000104000304058010"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"2"
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""

	case left(REG.stat,1)="T"
			ww=   "DN9900010030000104000304058010"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"0"
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""

		ww=   "DN9900010030000104000300348150"+chr(13)+CHR(10)
		ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
		ww=ww+"TRATAMIENTO    "
		ww=ww+chr(13)+chr(10)
		=FWRITE(SALIDA,ww)
		ww=""

	case left(REG.stat,2)="AB" 
			ww=   "DN9900010030000104000304058010"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"0"
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""

		ww=   "DN9900010030000104000300348150"+chr(13)+CHR(10)
		ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
		ww=ww+"ABORTO         "
		ww=ww+chr(13)+chr(10)
		=FWRITE(SALIDA,ww)
		ww=""

	case left(REG.stat,2)="AN" 
			ww=   "DN9900010030000104000304058010"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"0"
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""

		ww=   "DN9900010030000104000300348150"+chr(13)+CHR(10)
		ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
		ww=ww+"ANESTRO        "
		ww=ww+chr(13)+chr(10)
		=FWRITE(SALIDA,ww)
		ww=""
	 
	endcase
endif

** to do 4 Dry off 
**********************************************************************+
if ALP.todo=4 
	do case
	case left(REG.stat,1)="F"
			ww=   "DN9900010030000104000304058010"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"0"
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""

	case left(REG.stat,1)="S" 
			ww=   "DN9900010030000104000304058010"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"0"
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""

		if REG.ucal#ALP.ucel and reg.ucal#ctod("")
			ww=   "DN990001003000010400030405101000300034080"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"1"
			ww=ww+substr(dtoc(REG.ucal),1,4)+substr(dtoc(REG.ucal),6,2)+right(dtoc(REG.ucal),2)
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""
		endif

	case left(REG.stat,1)="L"
			ww=   "DN9900010030000104000304058010"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"0"
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""

		if REG.ucal#ALP.ucel and reg.ucal#ctod("")
			ww=   "DN990001003000010400030405101000300034080"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"1"
			ww=ww+substr(dtoc(REG.ucal),1,4)+substr(dtoc(REG.ucal),6,2)+right(dtoc(REG.ucal),2)
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""
		endif
		
	case left(REG.stat,1)="I"
			ww=   "DN9900010030000104000304058010"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"0"
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""

		if REG.ucal#ALP.ucal
		ww=   "DN9900020030000104000304054010003000340800030035708000300374150"+chr(13)+CHR(10)
		ww=ww+"VN990002"+RIGHT("0000"+ALLTRIM(STR(XID)),4)+;
			   "1"+;
				substr(dtoc(REG.ucal),1,4)+substr(dtoc(REG.ucal),6,2)+right(dtoc(REG.ucal),2)+;
				substr(dtoc(REG.ucal),1,4)+substr(dtoc(REG.ucal),6,2)+right(dtoc(REG.ucal),2)+;
				left(REG.idt+"               ",15)
		ww=ww+chr(13)+chr(10)
		=FWRITE(SALIDA,ww)
		ww=""
		endif

	case left(REG.stat,1)="V" 
			ww=   "DN9900010030000104000304058010"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"0"
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""

 	case left(REG.stat,1)="C" 
		if REG.fsec#ctod("")
			ww=   "DN990001003000010400030406101000300194080"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"1"
     		ww=ww+substr(dtoc(REG.fsec),1,4)+substr(dtoc(REG.fsec),6,2)+right(dtoc(REG.fsec),2)
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""
		endif
				
	case left(REG.stat,1)="T"
			ww=   "DN9900010030000104000304058010"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"0"
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""

		ww=   "DN9900010030000104000300348150"+chr(13)+CHR(10)
		ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
		ww=ww+"TRATAMIENTO    "
		ww=ww+chr(13)+chr(10)
		=FWRITE(SALIDA,ww)
		ww=""

	case left(REG.stat,2)="AB" 
			ww=   "DN9900010030000104000304058010"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"0"
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""

		ww=   "DN9900010030000104000300348150"+chr(13)+CHR(10)
		ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
		ww=ww+"ABORTO         "
		ww=ww+chr(13)+chr(10)
		=FWRITE(SALIDA,ww)
		ww=""

	case left(REG.stat,2)="AN" 
			ww=   "DN9900010030000104000304058010"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"0"
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""

		ww=   "DN9900010030000104000300348150"+chr(13)+CHR(10)
		ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
		ww=ww+"ANESTRO        "
		ww=ww+chr(13)+chr(10)
		=FWRITE(SALIDA,ww)
		ww=""
	 
	endcase
endif

** to do 5 Build Up 
**********************************************************************+
if ALP.todo=5 
	do case
	case left(REG.stat,1)="F"
			ww=   "DN9900010030000104000304063010"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"1"
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""
			ww=   "DN99000100300001040003040640100030003508000300036020"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"1"
     		ww=ww+substr(dtoc(REG.fpar),1,4)+substr(dtoc(REG.fpar),6,2)+right(dtoc(REG.fpar),2)
			ww=ww+str(REG.np,2)
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""

	case left(REG.stat,1)="S" 
			ww=   "DN9900010030000104000304058010"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"0"
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""

		if REG.ucal#ALP.ucel and reg.ucal#ctod("")
			ww=   "DN990001003000010400030405101000300034080"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"1"
			ww=ww+substr(dtoc(REG.ucal),1,4)+substr(dtoc(REG.ucal),6,2)+right(dtoc(REG.ucal),2)
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""
		endif

	case left(REG.stat,1)="L" 
			ww=   "DN9900010030000104000304058010"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"0"
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""

		if REG.ucal#ALP.ucel and reg.ucal#ctod("")
			ww=   "DN990001003000010400030405101000300034080"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"1"
			ww=ww+substr(dtoc(REG.ucal),1,4)+substr(dtoc(REG.ucal),6,2)+right(dtoc(REG.ucal),2)
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""
		endif

	case left(REG.stat,1)="I"
			ww=   "DN9900010030000104000304058010"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"0"
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""

	  		if REG.ucal#ALP.ucal
			ww=   "DN9900020030000104000304054010003000340800030035708000300374150"+chr(13)+CHR(10)
			ww=ww+"VN990002"+RIGHT("0000"+ALLTRIM(STR(XID)),4)+;
				   "1"+;
					substr(dtoc(REG.ucal),1,4)+substr(dtoc(REG.ucal),6,2)+right(dtoc(REG.ucal),2)+;
					substr(dtoc(REG.ucal),1,4)+substr(dtoc(REG.ucal),6,2)+right(dtoc(REG.ucal),2)+;
					left(REG.idt+"               ",15)
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""
			endif

	case left(REG.stat,1)="I"
			ww=   "DN9900010030000104000304058010"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"0"
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
            ww=""			

		if REG.ucal#ALP.ucal
		ww=   "DN9900010030000104000304054010003000340800030035708000300374150"+chr(13)+CHR(10)
		ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)+;
			   "1"+;
				substr(dtoc(REG.ucal),1,4)+substr(dtoc(REG.ucal),6,2)+right(dtoc(REG.ucal),2)+;
				substr(dtoc(REG.ucal),1,4)+substr(dtoc(REG.ucal),6,2)+right(dtoc(REG.ucal),2)+;
				left(REG.idt+"               ",15)
		ww=ww+chr(13)+chr(10)
		=FWRITE(SALIDA,ww)
		ww=""
		endif

	case left(REG.stat,1)="V" 
			ww=   "DN9900010030000104000304058010"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"0"
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""

 	case left(REG.stat,1)="C" 
			ww=   "DN9900010030000104000304063010"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"1"
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""
		
	case left(REG.stat,1)="T"
			ww=   "DN9900010030000104000304058010"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"0"
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""

		ww=   "DN9900010030000104000300348150"+chr(13)+CHR(10)
		ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
		ww=ww+"TRATAMIENTO    "
		ww=ww+chr(13)+chr(10)
		=FWRITE(SALIDA,ww)
		ww=""

	case left(REG.stat,2)="AB" 
			ww=   "DN9900010030000104000304058010"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"0"
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""

		ww=   "DN9900010030000104000300348150"+chr(13)+CHR(10)
		ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
		ww=ww+"ABORTO       "
		ww=ww+chr(13)+chr(10)
		=FWRITE(SALIDA,ww)
		ww=""

	case left(REG.stat,2)="AN" 
			ww=   "DN9900010030000104000304058010"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"0"
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""
			

		ww=   "DN9900010030000104000300348150"+chr(13)+CHR(10)
		ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
		ww=ww+"ANESTRO        "
		ww=ww+chr(13)+chr(10)
		=FWRITE(SALIDA,ww)
		ww=""
	 
	endcase
endif

** to do 6 Confirm Calving 
**********************************************************************+
if ALP.todo=6 
	do case
	case left(REG.stat,1)="F"
			ww=   "DN9900010030000104000304063010"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"1"
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""
			ww=   "DN99000100300001040003040640100030003508000300036020"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"1"
     		ww=ww+substr(dtoc(REG.fpar),1,4)+substr(dtoc(REG.fpar),6,2)+right(dtoc(REG.fpar),2)
			ww=ww+str(REG.np,2)
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""

	case left(REG.stat,1)="S" 
			ww=   "DN9900010030000104000304058010"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"0"
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""

		if REG.ucal#ALP.ucel and reg.ucal#ctod("")
			ww=   "DN990001003000010400030405101000300034080"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"1"
			ww=ww+substr(dtoc(REG.ucal),1,4)+substr(dtoc(REG.ucal),6,2)+right(dtoc(REG.ucal),2)
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""
		endif

	case left(REG.stat,1)="L"
			ww=   "DN9900010030000104000304058010"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"0"
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""

		if REG.ucal#ALP.ucel and reg.ucal#ctod("")
			ww=   "DN990001003000010400030405101000300034080"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"1"
			ww=ww+substr(dtoc(REG.ucal),1,4)+substr(dtoc(REG.ucal),6,2)+right(dtoc(REG.ucal),2)
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""
		endif

	case left(REG.stat,1)="I"
			ww=   "DN9900010030000104000304058010"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"0"
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""

	case left(REG.stat,1)="I"
			ww=   "DN9900010030000104000304058010"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"0"
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""

			if REG.ucal#ALP.ucal
			ww=   "DN9900020030000104000304054010003000340800030035708000300374150"+chr(13)+CHR(10)
			ww=ww+"VN990002"+RIGHT("0000"+ALLTRIM(STR(XID)),4)+;
				   "1"+;
					substr(dtoc(REG.ucal),1,4)+substr(dtoc(REG.ucal),6,2)+right(dtoc(REG.ucal),2)+;
					substr(dtoc(REG.ucal),1,4)+substr(dtoc(REG.ucal),6,2)+right(dtoc(REG.ucal),2)+;
					left(REG.idt+"               ",15)
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""
			endif

	case left(REG.stat,1)="V" 
			ww=   "DN9900010030000104000304058010"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"0"
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""

 	case left(REG.stat,1)="C" 
	* nada
			
	case left(REG.stat,1)="T"
			ww=   "DN9900010030000104000304058010"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"0"
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""

		ww=   "DN9900010030000104000300348150"+chr(13)+CHR(10)
		ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
		ww=ww+"TRATAMIENTO  "
		ww=ww+chr(13)+chr(10)
		=FWRITE(SALIDA,ww)
		ww=""

	case left(REG.stat,2)="AB" 
			ww=   "DN9900010030000104000304058010"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"0"
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""

		ww=   "DN9900010030000104000300348150"+chr(13)+CHR(10)
		ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
		ww=ww+"ABORTO       "
		ww=ww+chr(13)+chr(10)
		=FWRITE(SALIDA,ww)
		ww=""

	case left(REG.stat,2)="AN" 
			ww=   "DN9900010030000104000304058010"+chr(13)+CHR(10)
			ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
			ww=ww+"0"
			ww=ww+chr(13)+chr(10)
			=FWRITE(SALIDA,ww)
			ww=""

		ww=   "DN9900010030000104000300348150"+chr(13)+CHR(10)
		ww=ww+"VN990001"+RIGHT("0000"+ALLTRIM(STR(XID)),4)
		ww=ww+"ANESTRO        "
		ww=ww+chr(13)+chr(10)
		=FWRITE(SALIDA,ww)
		ww=""
	 
	endcase
ENDIF
RETURN

