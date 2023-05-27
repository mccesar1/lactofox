** PROGRAMA PARA TRANSFERIR EVENTOS DE DIARIO2
*--------------------------------------------*
** Actualiza VACAS
** ---------------
Select DIA
Set Filt To XTIPO="V" And FECHA<=Date()
GO TOP
SCAN
 XID=DIA.ID
 HOY=DIA.fecha
 UFECHA=B
 XSTAT="LIMPI"
  
Select REG
Set Order To 2
	Seek xid
		If Found() 
		Do Case
		   Case DIA.evento=1 
			 		Do PARTOS		   
		   Case DIA.evento=2  or DIA.evento=3 
			 		Do REVTRAT
		   Case DIA.evento=4 
			 		Do CELO
		   Case DIA.evento=5 
			 		Do INSEM
		   Case DIA.evento=6 
		     	Do PALPACION
		   Case DIA.evento=7 
		     	Do SECADo
		   Case DIA.evento=8 
          Do HOSPITAL
		   Case DIA.evento=9 
			 		Do ABORTOS
		   Case DIA.evento=10 
          Do VACUNAS
		   Case DIA.evento=11 
			 		Do CONDICION
		   Case DIA.evento=14
		   	 	Do CAMBIOS	
		   Case DIA.evento=15
		   	 	Do CODIGOS		
		   Case DIA.evento=16
		   		Do CID01	 		
		Endcase
		Else
		Select DIA
		Replace prob With "ID No existe"
EndIf
Select DIA
Endscan	


** Actualiza Crianza
** -----------------
Select DIA
Set Filt To xtipo="C" and fecha<=Date()
Go Top
SCAN
 XID=DIA.id
 HOY=DIA.fecha
 UFECHA=B 
 XSTAT="LIMPI"
  
 Select REG
 Set Order To 3
	Seek xid
		If found() 
		Do Case
		   Case DIA.evento=1 
			 		Do PARTOS2		   
		   Case DIA.evento=3 
			 		Do TRAT2
		   Case DIA.evento=4 
			    Do CELO2
		   Case DIA.evento=5 
			    Do INSEM2
		   Case DIA.evento=6 
		      Do PALPACION2
		   Case DIA.evento=8 
          Do HOSPITAL2
		   Case DIA.evento=9 
			    Do ABORTOS2
		   Case DIA.evento=10 
          Do VACUNAS2
		   Case DIA.evento=12 
          Do DESTETES
		   Case DIA.evento=13 
          Do MEDIDAS
		   Case DIA.evento=14
		   	 	Do CAMBIOS2
		   Case DIA.evento=15
		   	 	Do CODIGOS 	
		   Case DIA.evento=16
		   		Do CID02	
		EndCase
		Else
		Select DIA
		Replace PROB With "ID No existe"
EndIf
Select DIA
ENDSCAN	
RETURN


PROCEDURE PARTOS
****************************************************************************
* Verifica si ya existe el ID de CRIA
* -----------------------------------	
Do Case
	* HEMBRA/HEMBRAS
	* --------------
	Case DIA.Sexc="HE" Or DIA.Sexc="HH"
		Select REG
		Set Order To 3
		Do Case
			Case Seek(DIA.C1) And Left(SEXC,1)="H"
   				Replace DIA.prob With "ID Cria1 ya existe"
   				Return
			Case Seek(DIA.C2)
   				Replace DIA.prob With "ID Cria2 ya existe"
   				Return
		EndCase
	
	* MACHO/MACHOS
	* ------------
	Case DIA.Sexc="MA" Or DIA.Sexc="MM"
		Select SREG
		Set Order To 1
		Do Case
			Case Seek(DIA.C1)
   				Replace DIA.prob With "ID Cria1 ya existe"
   				Return
			Case Seek(DIA.C2)
   				Replace DIA.prob With "ID Cria2 ya existe"
   				Return
		EndCase
EndCase	

* Inicia el Proceso
* -----------------
Select REG
Set Order To 2
Seek xid 
Do Case
   Case fb2#B 
	Replace DIA.prob With  [Dada de Baja]
   Case stat='CARGA' and fsec#B and hoy-ucal<=Q17 
	Replace DIA.prob With [Gestacion Baja]
   Case stat#[CARGA] 
	Replace DIA.prob With  [No Cargada]
   Case stat=[CARGA] and FSEC=B 
	Replace DIA.prob With  [No en Secas]
   Case stat=[CARGA] and FSEC#B and hoy-ucal>=Q17 and FSEC<=hoy
		ANP=NP
		KIDC=0
		KIDC2=0
		STORE 0 TO MCC,mest1,mest2
	    STORE SPACE(2) TO KSEXC,PPNAC1,PPNAC2,ALCORR
	    STORE SPACE(6) TO POBS
	    STORE SPACE(9) TO IDPC
		xmed1=space(2)
		T3=0
		T2=space(14)
		mTEC=space(3)
		
*--  PARTO NUEVO  ----*
Replace EDAD With (FPAR-FNAC)/30.4
MFCON=HOY
MCEVE=[Parto]
MIDD=REG.IDMD

** Guarda el codigo del toro padre de la cria
** y actualiza si fue becerra o becerro
** ------------------------------------------
mtoro=REG.IDT
Select CTOROS
Set Order to 1
seek mtoro
	If found()
		Do Case
			Case DIA.sexc="HE" or DIA.sexc="HH"
				Replace NBAS With NBAS+1

			Case DIA.sexc="MA" or DIA.sexc="MM"
				Replace NBOS With NBOS+1

			Case DIA.sexc="HM"
				Replace NBAS With NBAS+1,NBOS With NBOS+1
		endCase
	Else
	endi
	
** Actualiza la Lactancia Anterior
** -------------------------------
Select LACTS
	Append Blank
	Replace ID With XID,NP With A->NP,FPAR With A->FPAR,D1S With A->D1S,;
	DAB With A->DAB,DPR With A->DPR,UCAL With A->UCAL,fpan With hoy,;
	IP With HOY-(A->FPAR),IDT With A->IDT,NS With A->NS,;
	UTEC With A->UTEC,STAT With A->STAT,PRX With A->PRX,PAC With A->PAC,;
	P305 With A->P305,M305 With A->M305,FSEC With A->FSEC,EAP With A->EDAD,;
	DSC With HOY-(A->FSEC),SEXC With A->SEXC,IDC With A->IDC,IDC2 With A->IDC2

* ELIMINA REGISTROS DE PARTOS ANTERIORES
* **************************************
** Depura los registros PROD
** -------------------------
Select 0 
	Use PROD Order 1 Exclusive AGAIN
	Seek XID
	Delete while ID=XID
Use

** Depura los registros CALOR
** --------------------------
Select 0
	Use CALOR Order 2 Exclusive AGAIN
	Seek XID
	Delete While ID=XID
	
* Para no borrar los de los partos pasados
* ----------------------------------------
*	Scan While ID=xid
*		If np>0 and NP<ANP
*			Delete
*		Endif
*	Endscan	
Use

** Depura los registros BST
** ------------------------
Select 0
	Use BST Order 1 Exclusive AGAIN
	Seek XID
	Delete While ID=XID
Use

** Depura los registros CCS
** ------------------------
Select 0
	Use CCS Order 1 Exclusive AGAIN
	Seek XID
	Delete While ID=XID
Use
* Fin de Eliminar Registros
* **************************************

** Actualiza la Tarjeta de Registro Actual
** ---------------------------------------
Select REG
	Replace FPAR With HOY,NP With NP+1,EDAD With (FPAR-FNAC)/30.4,UCAL With B,CORR With DIA.cn,IDPC With IDT,;
	MED1 With DIA.med,IDT With space(9),NS With 0,UTEC With space(3),PALP With B,;
	STAT With 'FRESC',RMAT With B,NREV With 0,UTRA With B,FSEC With B,;
	PRM With 0,PRX With 0,P305 With 0,M305 With 0,D1S With 0,DAB With 0,IDMD With 0

	Replace DIA With 0,;
    	 DPR With (DATE()-FPAR),;
	     FPC With B,;
	     FPP With B,;
	     PESA With B,;
	     PAC With 0,;
	     PSER With B,;
	     ABTO With B,;
	     TECP With '   ',;
	     IDC With DIA.c1,;
	     IDC2 With DIA.c2,;
	     SEXC With DIA.SEXC,;
		   OBS With DIA.OBS,;
	     DGA With 0,;
	     PCEL With B,;
	     PRIN With 0,;
	     TOTA With 0,;
	     LIfEL With LIfEL+M305,;
	     NREG With NREG+1,;
	     PLAC With 0,;
	     DPIC With 0,;
	     CAU4 With space(10),;
	     STB With " "
	     
** Para Calculos de CTAP.
** ---------------------
Replace PCIP With LACTS.IP,PDSC With LACTS.DSC,PGES With LACTS.IP-LACTS.DAB
** ------------------------------------------

Do ACONDC

Select REG
	If SEXC='HE' or SEXC='MA' or SEXC='MH'
		Replace IDC2 With 0
	ENDI

Do EQM

If DIA.med>0
	Do ACLIN With DIA.evento
endIf
Select REG
	ABIDC=IDC
	ABIDC2=IDC2
	ABFPAR=HOY
	ABIDPC=IDPC
	ABIDP=IDP
	ABPNAC=PPNAC1
	ABPNAC2=PPNAC2
	ABIDM=ID	

** Da de Alta las Crias en Registro y Medidas
** -----------------------------------------
Do Case
	Case SEXC='HE' and IDC>0 or SEXC='HH' and IDC>0
		Do ALTAB	

	Case SEXC='MA' and IDC>0 or SEXC='MM' and IDC>0 or sexc='MH' and IDC>0
		Do ALTAM

	Otherwise
EndCase


** Rutina para borrar Codigos al PARIR.
** -----------------------------------
Declare xcod(8)
xcod=" "
Select REG
		xx=1
		Do while xx<9
			xcod(xx)=substr(CODIGO,xx,1)
		xx=xx+1	
		Endd

		xx=1
		Do while xx<9
		If not empty(CODIGO)
			Select GNCODIGO
			Set Order to 1
			Seek xcod(xx)
			If Found() and BORRA="+"
				Select REG
				If xcod(xx)$CODIGO
					xcodigo=chrtran(CODIGO,xcod(xx),"")
					Replace CODIGO With trim(xcodigo)
				endIf
			endIf
			Select REG
		endIf
		xx=xx+1
		endd
** ----------------

** Actualiza AGENDA
** ----------------
Do MAGENDA
** ----------------

Select DIA
Delete
Otherwise
Replace DIA.prob With [ID No Valida]
EndCase
RETURN

PROCEDURE REVTRAT
****************************************************************************
Do Case
	Case fb2#B
		Replace DIA.prob With  [Dada de Baja]

	Case STAT=[CARGA]
		Replace DIA.prob With [Cargada]

   Case STAT=[INSEM]
	  	Replace MED1 With DIA.med,UTRA With HOY,UTEC With DIA.utec,;
					 		OBSC With space(14)
	  			    mceve=[Tratamiento]

	Otherwise	  

		Replace STAT With DIA.STAT,MED1 With DIA.MED,;
					  UTRA With HOY,UTEC With DIA.utec,OBSC With space(14)
			
			If STAT=space(5)
				Replace STAT With [LIMPI]
	  	Endif		
     	
     	If DIA.evento=3
     		Replace stat With [TRATA]
     	EndIf	
  	  
  	  MFCON=HOY
	  If DIA.evento=2
	 	 mceve=[Revision]
	  Else
	  	mceve=[Tratamiento]
	  endIf
EndCase

** Actualiza AGENDA
** ----------------
Do MAGENDA
** ----------------

Select CALOR
Set Order to 2
seek xid

If not found()
	UFECHA=B
Else
	scan while ID=xid for NP=REG.np
     UFECHA=fecha
	endscan
endIf
	Appe blank
	Replace ID With xid,parto With REG.fpar,NP With REg.np,FECHA With hoy,;
	STAT With REG.stat,MED With REG.med1,TEC With REg.utec,;
	CLAVE With DIA.evento,OBSC With DIA.obsc

If UFECHA=B 
	Replace DIAS With hoy-PARTO
Else
	Replace DIAS With hoy-UFECHA
endIf

 Do ACONDC
If REG.med1>0 
	Do ACLIN With DIA.evento
endIf

Select DIA
Delete
RETURN

PROCEDURE INSEM
****************************************************************************
If HOY>UCAL
  muc=UCAL
  mns=NS  
  cstat=STAT
  AIDT=idt
  	
  Replace UCAL With hoy,STAT With [INSEM],MED1 With DIA.med,;
  UTEC With DIA.utec,IDT With DIA.idt

If muc#B and hoy<=muc+3
	** Se considera reinseminacion
	** ---------------------------
		MIDT=IDT
		Do INVT With IDT
		Select CALOR
		Set order to 2
		seek XID
			If not found()
				UFECHA=B
			Else
				scan while ID=xid for NP=REG.np
					UFECHA=FECHA
				endscan
			endIf
		appe blank
		Replace ID With DIA.id,PARTO With REG.fpar,NP With REG.np,FECHA With hoy,;
		TORO With DIA.idt,SER With 0,STAT With REG.stat,TEC With DIA.utec,;
		MED With DIA.med,CLAVE With 5,OBSC With DIA.obsc

	If fecha=B
		Replace DIAS With hoy-PARTO
	Else
		Replace DIAS With hoy-UFECHA
	endi

Else 


Select REG

** Automatico borra la programacion (Por Compatibilidad con V. 2.60c)
** ------------------------------------------------------------------
Replace FPRO With B,MEDP With space(14),;
     FT1 With B,FT2 With B,MD1 With space(14),TPA With 0
** ---------------------------------

Replace NS With NS+1

** Si ya estaba diagnosticada como preñada.
** ----------------------------------------
If cstat=[CARGA]
	Replace PRIN With PRIN+1
endi
** ----------------------------------------

If NS=1 and STAT='INSEM'
   Replace D1S With UCAL-FPAR,PSER With UCAL
	If PCEL=B
	Replace pcel With ucal
	endi
EndIf

If FPRO=B and len(MEDP)>0 
	Replace MEDP With space(14)
endi

mfcon=UCAL
mceve=[Insem #]+str(NS,2)

** Para poner dias a ciclo de calor en REG.DBF
** -------------------------------------------
	If muc#B
		Replace CICLO With UCAL-muc
	endIf	
** -------------------------------------------

If mns<NS
	midt=IDT
	Do INVT With IDT
endIf

** Actualiza AGENDA
** ----------------
	Do MAgenda
** ----------------

Select CALOR
Set Order to 2

seek xid
If not found()
 UFECHA=B
Else
 scan while ID=xid for NP=REG.np
  UFECHA=FECHA
 endscan
endIf

appe blank
 Replace ID With REG.id,PARTO With REG.fpar,NP With REG.np,FECHA With REG.ucal,;
 TORO With DIA.idt,SER With REG.ns,STAT With REG.stat,TEC With REG.utec,;
 MED With DIA.med,FECHA2 With REg.ucal,TORO2 With DIA.idt,CLAVE With 5

If UFECHA=B
 Replace DIAS With hoy-PARTO
Else
 Replace DIAS With hoy-UFECHA
endIf

If cstat=[CARGA]
 Replace RINSE With REG.ucal,UPALP With REG.palp,UTECP With REG.tecp,;
 UTORO With AIDT,UDIAS With FECHA-REG.palp
endIf

** Para poner dias a ciclo de calor en CALOR.DBF
** -------------------------------------------
	If muc#B
		Replace CICLO With REG.ucal-MUC
	endi	
** -------------------------------------------

Do ACONDC

If DIA.med>0
	Do ACLIN With DIA.evento
endIf
endIf

Select DIA
dele

Else
Replace DIA.prob With [Fecha no Valida]
Endi
RETURN

PROCEDURE CELO
****************************************************************************
If hoy>ucal
 cstat=STAT
 muc=UCAL
 Replace UCAL With HOY,STAT With DIA.stat,MED1 With DIA.med,UTEC With DIA.utec

 If cstat="FRESC" or stat=space(5)
	Replace stat With "LIMPI"
 endIf

	* Checa si es el primer celo
	*----------------------------*
	If PCEL=B
	  Replace PCEL With ucal
	  mfcon=UCAL
      mceve=[Celo]
    Else
	  mfcon=UCAL
      mceve=[Celo]
    endi

** Para poner dias a ciclo de calor en REG.DBF
** -------------------------------------------
	If muc#B
		Replace REG.CICLO With REG.UCAL-muc
	EndIf	
** -------------------------------------------

** Actualiza AGENDA
** ----------------
Do MAGENDA
** ----------------

Select CALOR
Set Order to 2
Seek xid
If not found()
 UFECHA=B
Else
 scan while ID=xid for NP=REG.np
   UFECHA=FECHA
 endscan
endIf

Appe blank
Replace ID With xid,PARTO With REG.fpar,NP With REG.np,FECHA With REG.ucal,;
				STAT With REG.stat,MED With DIA.med,tec With DIA.utec,CLAVE With DIA.evento,;
				OBSC With DIA.obsc


If UFECHA=B 
 Replace DIAS With hoy-PARTO
Else
 Replace DIAS With hoy-UFECHA
endIf

** Para poner dias a ciclo de calor en CALOR.DBF
** -------------------------------------------
	If muc#B
		Replace CICLO With REG.ucal-muc
	endi	
** -------------------------------------------

Do ACONDC

If DIA.med>0 
	Do ACLIN With DIA.evento
endIf

Select DIA
dele
Else
Replace DIA.prob With [Fecha no Valida]
ENDIf
return

PROCEDURE PALPACION
****************************************************************************
cstat=STAT
If STAT=[INSEM] or STAT=[CARGA]
	Replace stat With DIA.stat,MED1 With DIA.med,;
	PALP With hoy,TECP With DIA.utec
	
	If STAT=[VACIA] and DIA.med>0 
	 Replace STAT With [TRATA],UTRA With hoy
	endIf
	
	mfcon=PALP
	mceve=[Palpacion]

Do ACONDC

** Actualiza AGENDA
** ----------------
Do MAGENDA
** ----------------
	
Select CALOR
Set order to 2
Seek xid

If not found()
	UFECHA=B
    XFECHA=B
Else
	scan while ID=xid for NP=REG.np
	UFECHA=FECHA
*---------------------*
	Do Case
	  Case STAT=[INSEM]
	    xfecha=fecha
	  otherwise
	    xfecha=B
	endCase
*---------------------*
	endscan
endIf

* Si la vaca ya estaba diagnosticada CARGA ya no actualiza FECHA2 ni TORO2

If cstat="CARGA"
	appe blank
	Replace ID With DIA.id,PARTO With REG.fpar,NP With REG.np,FECHA With hoy,;
	STAT With cstat,MED With DIA.med,TECP With DIA.utec,DGES With HOY-REG.ucal,;
	CLAVE With DIA.evento,OBSC With DIA.obsc

Else
	appe blank
	Replace ID With DIA.id,PARTO With REG.fpar,NP With REG.NP,FECHA With hoy,;
	TORO With DIA.idt,STAT With DIA.stat,MED With DIA.med,TORO2 With REG.idt,TECP With DIA.utec,;
	SER With REG.ns,TEC With REG.utec,fecha2 With xfecha,DGES With HOY-REG.ucal,;
	CLAVE With DIA.evento,OBSC With DIA.obsc

endi

If UFECHA=B 
	Replace DIAS With hoy-PARTO
Else
	Replace DIAS With hoy-UFECHA
endIf

If DIA.med>0
	Do ACLIN With DIA.evento
endIf

Select DIA
dele
Else
Replace DIA.prob With [No Inseminada]
ENDIf
RETURN

PROCEDURE SECADO
****************************************************************************
If FSEC=B
Replace FSEC With hoy,CORR With DIA.cn
mfcon=FSEC
mceve=[SecaDo]
Do ACONDC

Select REG

** Ajusta la Produccion a 305d y EM, ERPA y PTA
** --------------------------------------------
	Replace DPR With (date()-fpar)-(date()-fsec)
	Qpac=PAC+PRM*((DATE()-PESA)-(DATE()-FSEC))

	Do Case
	   Case FSEC#B and DPR<305 
	        Replace P305 With Qpac,M305 With P305*fac
	   otherwise	
			If edad<=36
				Do 305
			Else
			If edad>36
				Do 305m
			endi
			endi
			Replace M305 With P305*fac
	endCase

** Calculo de ERPA
** ----------------------------------------
If M305>0
	mfac=0.5
	mnreg=nreg+1
	mfac=(mnreg*mfac)/(1+((mnreg-1)*mfac))

Replace ERPA With mfac*(((LIfEL+M305)/mnreg)-x305)						
endi
* ----------------------------------------------	
If DIA.med>0
	Do ACLIN With DIA.evento
endIf

** Actualiza AGENDA
** ----------------
Do MAGENDA
** ----------------

Select DIA
Delete
Else
Replace DIA.prob With [ID ya en Secas]
Endif
RETURN

PROCEDURE HOSPITAL
****************************************************************************
xhos="N"
xtime=0
xtrat=0

Do Case
 Case DIA.enf>0
  Replace ENF With DIA.enf,FTRA With hoy,TRAT With MEDIC.MNOM,;
  NTRAT With ntrat+1  			
	
	  If HOSP=0
   	 Replace HOSP With 1,NTRAT With 1
  	EndIf	

    Set Relation To DIA.MED Into MEDIC	
    
    Select CLIN
	  Append blank
	  Replace ID With xid,NP With REG.np,FECHA With HOY,ENF With DIA.enf,;
	  TRAT With MEDIC.MNOM,CURO With DIA.utec,DOSIS With DIA.Dosis,;
	  MNUM With DIA.MED,NTRAT With REG.NTRAT,CLAVE With DIA.evento,;
	  stat With REG.stat
    
      
    Set Relation To DIA.MEDR Into MEDIC
	  Replace MEDH With MEDIC.MNOM

** Alta de Hospital
** ----------------
Case DIA.enf=0
	If HOSP=1
		xenf=ENF
		xnp=NP
		Replace HOSP With 0,ENF With 0,fpro2 With B,MEDH With space(14),;
		     ft3 With B,ft4 With B,md2 With space(14),TXH With space(1)
	   Select CLIN
		Appe Blank
		Replace ID With xid,NP With REG.np,FECHA With hoy,ENF With xenf,;
		CURO With DIA.utec,TRAT With [ALTA],evento With "S",clave With DIA.evento
	endIf

EndCase 

** Actualiza AGENDA
** ----------------
Do MAGENDA
** ----------------

Select DIA
Delete
RETURN

PROCEDURE ABORTOS
****************************************************************************
If stat=[CARGA] and (hoy-ucal)<=Q17
	Replace ABTO With hoy,STAT With 'ABORT',MED1 With DIA.med,;
	DGA With ABTO-UCAL
	mfcon=ABTO
	mceve=[Aborto]

Do ACONDC

** Actualiza AGENDA
** ----------------
Do MAGENDA
** ----------------

Select CALOR
Set order to 2
seek xid
If not found()
	UFECHA=B
Else
	scan while ID=xid for NP=REG.np
		UFECHA=FECHA
	endscan
endIf
appe blank
Replace ID With xid,PARTO With REG.fpar,NP With REG.np,FECHA With REG.abto,;
STAT With 'ABORT',MED With DIA.med,CLAVE With DIA.evento,OBSC With DIA.obsc

 If UFECHA=B 
  Replace DIAS With hoy-PARTO
 Else
  Replace DIAS With hoy-UFECHA
 endIf

** Anota el Aborto en el Archivo de ABORTOS (2000.05.25)
	Select 0
	 Use ABORTOS AGAIN
	Appe blank
	Replace ID With xid,FNAC With REG.fnac,NP With REg.np,FPAR With REG.fpar,;
	     FECHA With REG.abto,DGA With REG.dga,IDT With REG.idt,CORR With REG.corr,;
	     NS With REG.ns,DPR With (FECHA-FPAR),DAB With REG.ucal-FPAR,CONDC With DIA.cond,;
	     PRM With REG.prm
	Use

**	Replace MON With a->MON
If DIA.med>0
	Do ACLIN With DIA.evento
endIf

Select DIA
dele
Else
Replace DIA.prob With [ID no valida]
endIf
return

PROCEDURE VACUNAS
****************************************************************************
Set Relation To DIA.vac Into GNVACUNA

If DIA.vac>0 and DIA.vac<10
	xcampo="V0"+str(DIA.vac,1)
Else
	xcampo="V"+str(DIA.vac,2)
endIf

Replace &xcampo With hoy

Select 0
	Use VACUNAS Order 2 Exclusive AGAIN
	Appe blank
	Replace ID With xid,np With reg.np,fecha With hoy,;
	numv With DIA.vac,nombre With GNVACUNA.nombre,DIAST With FECHA-REG.FNAC
Use

Select REG
	Replace FVAC With HOY,UVAC With DIA.VAC,NVAC With GNVACUNA.NOMBRE

Select DIA
dele
return

PROCEDURE CONDICION
****************************************************************************
If FCON<hoy
XNP=REG.NP
xdel=(HOY-REG.fpar)
mPRM=REG.PRM

If FSEC#B 
	mPRM=0
	xdel=0
endIf	

Replace FCON With HOY,CONDC With DIA.cond,PRODC With mPRM

mCDEL=xdel

Select CONDC
Set order to 1
Seek xid
If not found()
  appe blank
	Replace ID With xid,NP With REG.np,FECHA With HOY,CONDC With DIA.cond,;
	CDEL With mCDEL,CEVE With [Directa],CPROD With REG.prm
Else
   scan while ID=xid
   endscan
	skip-1
	Do Case
	Case FECHA=hoy
	 Replace CONDC With DIA.cond
	Case FECHA<hoy
	 appe blank
	 Replace ID With xid,NP With REG.np,FECHA With HOY,CONDC With DIA.cond,;
	 CDEL With mCDEL,CEVE With [Directa],CPROD With REG.PRM
	otherwise
	endCase
endIf
endIf
Select DIA
dele
return

PROCEDURE CAMBIOS
****************************************************************************
If DIA.cn>0
	Replace CORA With CORR,CORR With DIA.cn
	Select DIA
	dele
endIf
return

PROCEDURE CODIGOS
****************************************************************************
		If NOT Empty(DIA.CODIGO)
			Do Case
				Case DIA.TCOD="+" And NOT DIA.CODIGO$CODIGO And Len(Alltrim(CODIGO))<8
					Replace CODIGO With Alltrim(CODIGO)+Alltrim(DIA.CODIGO)

				Case DIA.TCOD="-" And DIA.CODIGO$CODIGO
					xCODIGO=CHRTRAN(CODIGO,DIA.CODIGO,"")
					Replace CODIGO With Alltrim(XCODIGO)
			EndCase
			
			Select DIA
			Delete
		Else
			Replace DIA.prob With [Codigo no valido]
		EndIf


** C R I A N Z A
** -------------

PROCEDURE PARTOS2
****************************************************************************
Set Order to 2
Do Case
   Case seek(DIA.id) and DIA.idn=0
    Select DIA
	  Replace prob With "ID ya existe"
    Return

   Case seek(DIA.idn)
   	xtext=""
   	If FB2#B
		xtext="BAJA"
   	EndIf
   	Select DIA
	  Replace prob With "ID Nueva ya existe "+xtext
    Return
EndCase
 
     If DIA.idn>0
      xxid=DIA.idn	
     Else
      xxid=DIA.id	
     endIf

** Empieza traspaso
** ----------------
* Verifica si ya existe el ID de CRIA
* -----------------------------------	
Do Case
	* HEMBRA/HEMBRAS
	* --------------
	Case DIA.Sexc="HE" Or DIA.Sexc="HH"
		Select REG
		Set Order To 3
		Do Case
			Case Seek(DIA.C1)
   				Replace DIA.prob With "ID Cria1 ya existe"
   				Return
			Case Seek(DIA.C2)
   				Replace DIA.prob With "ID Cria2 ya existe"
   				Return
		EndCase
	
	* MACHO/MACHOS
	* ------------
	Case DIA.Sexc="MA" Or DIA.Sexc="MM"
		Select SREG
		Set Order To 1
		Do Case
			Case Seek(DIA.C1)
   				Replace DIA.prob With "ID Cria1 ya existe"
   				Return
			Case Seek(DIA.C2)
   				Replace DIA.prob With "ID Cria2 ya existe"
   				Return
		EndCase
EndCase	


* EMPIEZA EL TRASPASO
* -------------------
	xid=DIA.id
	seek xid
    Do MAGENDA
  Replace ID With xxid,;
	FPAR With HOY,;
	NP With 1,;
	SEXC With DIA.sexc,;
	IDC With DIA.c1,;
	IDC2 With DIA.c2,;
	IDPC With IDT,;
	UTRA With B,;
	MED1 With 0,;
	PSER With B,;
	UCAL With B,;
	IDT With space(9),;
	NS With 0,;
	UTEC With space(3),;
	TECP With space(3),;
	PALP With B,;
	STAT With [FRESC],;
	CORR With DIA.cn

	** Anota la ID Anterior
	If DIA.idn#DIA.id
		Replace IDAN With DIA.id
	endi	  

	** Actualiza el factor de ajuste para EM
	** -------------------------------------
		Replace EDAD With (FPAR-FNAC)/30.25
		Do EQM

If sexc='HE' or sexc='MA' or sexc='MH'
	Replace IDC2 With 0
	AIDC2=0
endi

** Rutina para borrar Codigos al PARIR.
** -----------------------------------
declare xcod(8)
xcod=" "
Select REG
		xx=1
		Do while xx<9
			xcod(xx)=substr(CODIGO,xx,1)
		xx=xx+1	
		endd

		xx=1
		Do while xx<9
		If not empty(CODIGO)
			Select GNCODIGO
			seek xcod(xx)
			If found() and BORRA="+"
				Select REG
				If xcod(xx)$CODIGO
					xcodigo=chrtran(CODIGO,xcod(xx),"")
					Replace CODIGO With trim(xcodigo)
				endIf
			endIf
			Select REG
		endIf
		xx=xx+1
		endd

If DIA.med>0 
	Do ACLIN With DIA.evento
endIf

Select REG
ABIDM=ID
ABIDPC=IDPC
ABIDP=IDP
MIDD=REG.IDMD
MFNAC=REG.FNAC

** Da de Alta las Crias en Registro y Medidas
** -----------------------------------------
Select REG
Do Case
	Case SEXC='HE' and IDC>0 or SEXC='HH' and IDC>0
		Do ALTAB	

	Case SEXC='MA' and IDC>0 or SEXC='MM' and IDC>0 or sexc='MH' and IDC>0
		Do ALTAM

	otherwise
endCase

** y actualiza si fue becerra o becerro
** ------------------------------------------
mtoro=REG.IDT
Select CTOROS
seek mtoro
	If found()
		Do Case
			Case DIA.sexc="HE" or DIA.sexc="HH"
				Replace NBAS With NBAS+1

			Case DIA.sexc="MA" or DIA.sexc="MM"
				Replace NBOS With NBOS+1

			Case DIA.sexc="HM"
				Replace NBAS With NBAS+1,NBOS With NBOS+1
		endCase
	Else
	endi

** Cambia EL ID por NXID
** ---------------------
  Do CAMBIAID
Select DIA
dele
RETURN

PROCEDURE TRAT2
****************************************************************************
private UFECHA
ufecha=B
	Do Case
	Case (hoy-fnac)<=365
	  Replace DIA.prob With [Edad No V lida]	
	Case stat=[CARGA]
	  Replace DIA.prob With [Pre¤ada]
	Case stat=[INSEM]
      Replace MED1 With DIA.med,UTRA With HOY,UTEC With DIA.utec,;
			  OBSC With space(14)
	otherwise
      Replace STAT With [TRATA],MED1 With DIA.med,;
	  UTRA With HOY,UTEC With DIA.utec,OBSC With space(14)
endCase

	** Actualiza AGENDA
	** ----------------
	Do MAGENDA
	** ----------------
	
	Select CALOR
	Set Order to 3
	seek xid

	If not found()
		UFECHA=B
	Else
		scan while ID=xid for NP=0
     		UFECHA=fecha
		endscan
	endIf
	appe blank
	Replace ID With xid,NP With 0,FECHA With REG.utra,;
	STAT With REG.stat,MED With REG.med1,TEC With REg.utec,;
	CLAVE With DIA.evento

	If UFECHA=B 
		Replace DIAS With hoy-REG.fnac
	Else
		Replace DIAS With hoy-UFECHA
	endIf
	If REG.med1>0 
		Do ACLIN With DIA.evento
	endIf
	Select DIA
	dele
return

PROCEDURE CELO2
****************************************************************************
private UFECHA
ufecha=B
	Do Case
	Case (hoy-fnac)<=365
	  Replace DIA.prob With [Edad No Valida]	
	Case stat=[CARGA]
	  Replace DIA.prob With [Prenada]
	Case stat=[INSEM]
	  Replace DIA.prob With [Inseminada]

	otherwise

	If hoy>ucal
 	   cstat=STAT
 	   muc=UCAL
       Replace UCAL With HOY,STAT With DIA.stat,MED1 With DIA.med,;
            UTEC With DIA.utec
	   If REG.stat=space(5)
		Replace REG.stat With "LIMPI"
	   endIf
			    	
       * Checa si es el primer celo
	   *----------------------------*
		If PCEL=B
		  Replace PCEL With ucal
		  mfcon=UCAL
	      mceve=[Celo]
	    Else
		  mfcon=UCAL
	      mceve=[Celo]
	    endi

		** Actualiza AGENDA
		** ----------------
			Do MAGENDA
		** ----------------

		** Para poner dias a ciclo de calor en REG.DBF
		** -------------------------------------------
		If muc#B
			Replace CICLO With UCAL-muc
		endIf	
	** -------------------------------------------

	Select CALOR
	Set Order to 3
	seek xid
	If not found()
		 UFECHA=B
	Else
	 scan while ID=xid for NP=REG.np
	   UFECHA=FECHA
	 endscan
	endIf

	appe blank
	Replace ID With xid,PARTO With REG.fpar,NP With REG.np,FECHA With REG.ucal,;
	STAT With REG.stat,MED With DIA.med,tec With DIA.utec,;
	CLAVE With DIA.evento

	If UFECHA=B 
	 Replace DIAS With hoy-REG.FNAC
	Else
	 Replace DIAS With hoy-UFECHA
	endIf

	** Para poner dias a ciclo de calor en CALOR.DBF
	** -------------------------------------------
	If muc#B
		Replace CICLO With REG.ucal-muc
	endi	
	** -------------------------------------------

	If DIA.med>0 
		Do ACLIN With DIA.evento
	endIf
	endIf
	Select DIA
	dele
endCase
return

PROCEDURE INSEM2
****************************************************************************
private UFECHA
ufecha=B
	Do Case
	Case (hoy-fnac)<=365
	  Replace DIA.prob With [Edad No V lida]	
*	Case stat=[CARGA]
*	  Replace DIA.prob With [Pre¤ada]

	otherwise

	If hoy>ucal
       muc=UCAL
       mns=NS  
       cstat=STAT
	   AIDT=idt

      Replace UCAL With hoy,STAT With [INSEM],MED1 With DIA.med,;
	  UTEC With DIA.utec,IDT With DIA.idt

	  If muc#B and hoy<=muc+3
		** Se considera reinseminacion
		** ---------------------------
		MIDT=IDT
		Do INVT With IDT
		Select CALOR
		Set Order to 3
		seek XID
			If not found()
				UFECHA=B
			Else
				scan while ID=xid for NP=REG.np
					UFECHA=FECHA
				endscan
			endIf
		appe blank
		Replace ID With DIA.id,PARTO With REG.fpar,NP With REG.np,FECHA With hoy,;
		TORO With DIA.idt,SER With 0,STAT With REG.stat,TEC With DIA.utec,;
		MED With DIA.med,CLAVE With 5

		If fecha=B
			Replace DIAS With hoy-REG.fnac
		Else
			Replace DIAS With hoy-UFECHA
		endi

	Else 
	
	** Automatico borra la programacion (Por Compatibilidad con V. 2.60c)
	** ------------------------------------------------------------------
	Replace FPRO With B,MEDP With space(14),;
  		    FT1 With B,FT2 With B,MD1 With space(14),TPA With 0
	** ---------------------------------
	
	** Actualiza AGENDA
		** ----------------
			Do MAGENDA
	** ----------------
		
	Replace ns With ns+1

	** Si ya estaba diagnosticada como pre¤ada.
	** ----------------------------------------
	If cstat=[CARGA]
		Select REG
		Replace PRIN With PRIN+1
	endi
	** ----------------------------------------

	If NS=1 and STAT='INSEM'
	   Replace D1S With UCAL-FPAR,PSER With UCAL
		If PCEL=B
			Replace pcel With ucal
		endIf
	endIf

	mfcon=UCAL
	mceve=[Insem #]+str(NS,2)

	** Para poner dias a ciclo de calor en REG.DBF
	** -------------------------------------------
	If muc#B
		Replace CICLO With UCAL-muc
	endIf	
	** -------------------------------------------

	If mns<NS
		midt=IDT
		Do INVT With IDT
	endIf

	Select CALOR
	Set Order To 3

	seek xid
	If not found()
	 UFECHA=B
	Else
	 scan while ID=xid for NP=REG.np
	  UFECHA=FECHA
	 endscan
	endIf
	appe blank
	 Replace ID With DIA.id,PARTO With REG.fpar,NP With REG.np,FECHA With hoy,;
	 TORO With DIA.idt,SER With REG.ns,STAT With REG.stat,TEC With REG.utec,;
	 MED With DIA.med,FECHA2 With REg.ucal,TORO2 With DIA.idt,CLAVE With 5

	If UFECHA=B
       Replace DIAS With hoy-REG.fnac
	Else
	   Replace DIAS With hoy-UFECHA
	endIf

	If cstat=[CARGA]
	 Replace RINSE With REG.ucal,UPALP With REG.palp,UTECP With REG.tecp,;
	 UTORO With AIDT,UDIAS With FECHA-REG.palp
	endIf

	** Para poner dias a ciclo de calor en CALOR.DBF
	** -------------------------------------------
	If muc#B
		Replace CICLO With REG.ucal-MUC
	endi	
	** -------------------------------------------

	If DIA.med>0
		Do ACLIN With DIA.evento
	endIf
endIf

endIf
Select DIA
dele
endCase
return

PROCEDURE PALPACION2
****************************************************************************
cstat=STAT
If STAT=[INSEM] or STAT=[CARGA]
	Replace stat With DIA.stat,MED1 With DIA.med,;
	PALP With hoy,TECP With DIA.utec
	
	If STAT=[VACIA] and DIA.med>0 
	 Replace STAT With [TRATA],UTRA With hoy
	endIf
	
	mfcon=PALP
	mceve=[Palpacion]

** Actualiza AGENDA
** ----------------
Do MAGENDA
** ----------------

	Select CALOR
	Set Order to 3
	seek xid

If not found()
	UFECHA=B
    XFECHA=B
Else
	scan while ID=xid for NP=REG.np
	UFECHA=FECHA
*---------------------*
	Do Case
	  Case STAT=[INSEM]
	    xfecha=fecha
	  otherwise
	    xfecha=B
	endCase
*---------------------*
	endscan
endIf

* Si la vaquilla ya estaba diagnosticada CARGA ya no actualiza FECHA2 ni TORO2

If cstat="CARGA"
	appe blank
	Replace ID With DIA.id,PARTO With REG.fpar,NP With REG.np,FECHA With hoy,;
	STAT With cstat,MED With DIA.med,TECP With DIA.utec,DGES With HOY-REG.ucal,;
	CLAVE With DIA.evento
Else
	appe blank
	Replace ID With DIA.id,PARTO With REG.fpar,NP With REG.NP,FECHA With hoy,;
	TORO With DIA.idt,STAT With DIA.stat,MED With DIA.med,TORO2 With REG.idt,TECP With DIA.utec,;
	SER With REG.ns,TEC With REG.utec,fecha2 With xfecha,DGES With HOY-REG.ucal,;
	CLAVE With DIA.evento
endi

If UFECHA=B 
	Replace DIAS With hoy-REG.fnac
	Else
	Replace DIAS With hoy-UFECHA
endIf

If DIA.med>0
	Do ACLIN With DIA.evento
endIf

Select DIA
dele
Else
 Replace DIA.prob With [No Inseminada]
ENDIf
return

PROCEDURE HOSPITAL2
****************************************************************************
xhos="N"
xtime=0
xtrat=0

Do Case
 Case DIA.enf>0
  replace ENF With DIA.enf,FTRA With hoy,TRAT With MEDIC.mnom,;
  NTRAT With ntrat+1  			

  If HOSP=0
    replace HOSP With 1,NTRAT With 1
  endIf	

    set rela to DIA.med into MEDIC	
    Select CLIN
	  append blank
	  Replace ID With xid,NP With REG.np,FECHA With HOY,ENF With DIA.enf,;
	  TRAT With MEDIC.mnom,CURO With DIA.utec,DoSIS With DIA.Dosis,;
	  MNUM With DIA.med,NTRAT With REG.ntrat,CLAVE With DIA.evento,;
	  stat With REG.stat
      
     set rela to DIA.medr into MEDIC
	  Replace MEDH With MEDIC.mnom

** Alta de Hospital
** ----------------
Case DIA.enf=0
	If HOSP=1
		xenf=ENF
		xnp=NP
		Replace HOSP With 0,ENF With 0,fpro2 With B,MEDH With space(14),;
		     ft3 With B,ft4 With B,md2 With space(14)
	   Select CLIN
		appe blank
		Replace ID With xid,NP With REG.np,FECHA With hoy,ENF With xenf,;
		CURO With DIA.utec,TRAT With [ALTA],evento With "S",clave With DIA.evento
	endIf

endCase 
Select REG
Do MAGENDA
Select DIA
dele
return

PROCEDURE ABORTOS2
****************************************************************************
If stat=[CARGA] and (hoy-ucal)<=Q17
	Replace ABTO With hoy,STAT With 'ABORT',MED1 With DIA.med,;
	DGA With ABTO-UCAL

** Actualiza AGENDA
** ----------------
Do MAGENDA
** ----------------

Select CALOR
Set Order to 3
seek xid
If not found()
	UFECHA=B
Else
	scan while ID=xid for NP=REG.np
		UFECHA=FECHA
	endscan
endIf
appe blank
Replace ID With xid,PARTO With REG.fpar,NP With REG.np,FECHA With REG.abto,;
STAT With 'ABORT',MED With DIA.med,CLAVE With DIA.evento
 If UFECHA=B 
  Replace DIAS With hoy-REG.fnac
 Else
  Replace DIAS With hoy-UFECHA
 endIf

** Anota el Aborto en el Archivo de ABORTOS (2000.05.25)
	Select 0
	Use ABORTOS AGAIN
	appe blank
	Replace ID With xid,FNAC With REG.fnac,NP With REg.np,FPAR With REG.fpar,;
	     FECHA With REG.abto,DGA With REG.dga,IDT With REG.idt,CORR With REG.corr,;
	     NS With REG.ns,DPR With (FECHA-FPAR),DAB With REG.ucal-FPAR,CONDC With DIA.cond,;
	     PRM With REG.prm
	Use
	     
**	Replace MON With a->MON

If DIA.med>0
	Do ACLIN With DIA.evento
endIf
Select DIA
dele
Else
Replace DIA.prob With [ID no v lida]
endIf
return

PROCEDURE VACUNAS2
****************************************************************************
set rela to DIA.vac into GNVACUNA

If DIA.vac>0 and DIA.vac<10
	xcampo="V0"+str(DIA.vac,1)
Else
	xcampo="V"+str(DIA.vac,2)
endIf

Replace &xcampo With hoy

Select 0
Use Vacunas Order 3 Exclusive AGAIN
	appe blank
	Replace ID With xid,np With reg.np,fecha With hoy,;
	numv With DIA.vac,nombre With GNVACUNA.nombre,DIAST With FECHA-REG.FNAC
Use

Select REG
	Replace FVAC With HOY,UVAC With DIA.VAC,NVAC With GNVACUNA.NOMBRE

Select DIA
Delete
Return

PROCEDURE DESTETES
******************
private UFECHA
ufecha=B
xfnac=REG.fnac
Do Case
   Case DEST#B 
    Replace DIA.prob With [Ya Destetada] 

   Case DEST=B and (hoy-fnac)>=Q25 

	** Reemplaza Datos en Archivo REG
	** ------------------------------
	Replace dest With hoy,pdes With DIA.peso,edes With DIA.alt,;
		 corr With DIA.cn
		 
	** Actualiza AGENDA
	** ----------------
	Do MAGENDA

	** Da de alta en Archivo de Medidas la becerra
	** -------------------------------------------
	xx=.t.
	Select MEDIDA
	Set Order to 1
		seek xid
			If found()
				scan while xid=ID
					xfecha=fecha
					nfecha=fecha
					npeso=peso
					nesta=esta
				endscan
				
				If xfecha>=hoy
					xx=.f.
				Else	
					** Da de Alta en el Archivo
					** ------------------------
					appe blank
					Replace id With xid,;
					fnac With xfnac,;
 					fecha With hoy,;
 					peso With DIA.peso,;
 					esta With DIA.alt

					** Calcula Ganancias de Peso Y Estatura
					** ------------------------------------
					Replace dias With fecha-nfecha,;
			    	ganp With (peso-npeso)/dias,;
			    	gane With (esta-nesta)/dias,;
			    	edad With (fecha-fnac)
					
				endi
			Else
					** Da de Alta en el Archivo (NO Calculo)
					** ------------------------
					appe blank
					Replace id With xid,;
					fnac With xfnac,;
 					fecha With hoy,;
 					peso With DIA.peso,;
 					esta With DIA.alt,;
			    	edad With (fecha-fnac)
			endi								 		
	Select REG
		If xx=.t.
			seek xid
			Replace FMED With hoy,upes With DIA.peso,uest With DIA.alt
	 	endi
Select DIA
dele
otherwise
Replace DIA.prob With [ID no v lida]
endCase
return

PROCEDURE MEDIDAS
*****************
private UFECHA
ufecha=B
xfnac=REG.fnac
If FPES>=hoy 
    Replace DIA.prob With [Ya Actualizada] 
Else	 
	** Actualiza AGEndA
	** ----------------
	Do MAGEndA

	** Da de alta en Archivo de Medidas la becerra
	** -------------------------------------------
	xx=.t.
	Select MEDIDA
	Set Order to 1
		seek xid
			If found()
				scan while xid=ID
					xfecha=fecha
					nfecha=fecha
					npeso=peso
					nesta=esta
				Endscan
				
				If xfecha>=hoy
					xx=.f.
				Else	
					** Da de Alta en el Archivo
					** ------------------------
					appe blank
					Replace id With xid,;
					fnac With xfnac,;
 					fecha With hoy,;
 					peso With DIA.peso,;
 					esta With DIA.alt

					** Calcula Ganancias de Peso Y Estatura
					** ------------------------------------
					Replace dias With fecha-nfecha,;
			    	ganp With (peso-npeso)/dias,;
			    	gane With (esta-nesta)/dias,;
			    	edad With (fecha-fnac)
					
				Endi
			Else
					** Da de Alta en el Archivo (NO Calculo)
					** ------------------------
					appe blank
					Replace id With xid,;
					fnac With xfnac,;
 					fecha With hoy,;
 					peso With DIA.peso,;
 					esta With DIA.alt,;
			    	edad With (fecha-fnac)
			Endi								 		
	Select REG
		If xx=.t.
			seek xid
			Replace FMED With hoy,upes With DIA.peso,uest With DIA.alt
	 	Endi

Select DIA
dele
EndIf
return

PROCEDURE CAMBIOS2
****************************************************************************
If DIA.cn>0
	Replace CORA With CORR,CORR With DIA.cn
	Select DIA
	dele
EndIf
Return

*   ***********************************************
*   *               U T I L E R I A S             *
*   ***********************************************

* Actualiza Agenda de Manejo en AGENDA.DBF
* ----------------------------------------
* Version 3.00

PROCEDURE MAGENDA
	
	If DIA.XTIPO="V"
		xfilt="And NP>0"
	Else
		xfilt="And NP=0"
	EndIF
	
	Select AGENDA
	Set Order to 1

	If DIA.evento=8
		Delete All FOR ID=XID And FECHA<Date() And TIPO='H' &xfilt
	Else
		Delete All FOR ID=XID And FECHA<Date() And TIPO='R' &xfilt
	EndIf
	
	** Borra la AGENDA si esta Inseminada y QUITAR="S"
	** -----------------------------------------------
	If REG.STAT="INSEM"
		Delete All For ID=XID And  QIA="S" And TIPO='R' &xfilt Or ID=XID And AGN=9 And TIPO='R' &xfilt

	* La Programa a Dx Gestacion
	* --------------------------
		Append Blank
		Replace ID With REG.ID,NP With REG.NP,AGN With 9,FECHA With REG.UCAL+Q7,M1 With [Dx Preñez],QIA With "S",TIPO With 'R'
		
	EndIf

	* La Programa a Confirmar Preñez
	* ------------------------------
	If Q46>0 and REG.STAT="CARGA" And REG.PALP<REG.UCAL+Q46 And REG.PALP#B And REG.DIA.diast=0
		Append Blank
		Replace ID with REG.ID,NP With REG.NP,AGN With 9,FECHA With REG.UCAL+Q46,M1 With [Dx Confirmar],QIA With "S",TIPO With 'R' 
	EndIf	
	
	* Verifica la Ultima fecha en AGENDA
	* ----------------------------------
	Select AGENDA
	
	If DIA.evento=8
		Set Filter To TIPO='H'
	Else
		Set Filter TO TIPO='R'
	EndIf		
	GO TOP
	
	xFECHA=B
	Scan For ID=XID
		xFECHA=FECHA
	EndScan
	
	* Reemplaza la Fecha de Programacion en REG
	* -----------------------------------------
	Select REG
		If DIA.evento=8
			Replace REG.FTXH With xFECHA	
		Else
			Replace REG.FTXR With xFECHA	
		EndIf		
	
* -------------Fin


** Actualiza Condicion Corporal
** ----------------------------
PROCEDURE ACONDC
Select REG
If DIA.cond>0 
mPRM=PRM
If FSEC#B and FSEC<hoy
	mPRM=0
Endi	

Replace FCON With hoy,CONDC With DIA.cond,PRODC With REG.PRM
MCDEL=(FCON-FPAR)-(FCON-FSEC)

	Select CONDC
	appe blank
	Replace ID With REG.id,NP With REG.np,FECHA With hoy,CONDC With DIA.cond,;
	CEVE With MCEVE,CDEL With MCDEL,CPROD With REG.PRM
	EndI
Select REG
Return

** Actualiza Tarjeta Clinica Vientres
** ----------------------------------
PROCEDURE ACLIN
parameter xclave
set rela to DIA.MED into MEDIC
Select CLIN
Set Order to 2
seek XID

If not found()
	ufecha=B
Else
	scan while ID=xid for NP=REG.NP
	ufecha=fecha
	Endscan
EndIf
	Append Blank
	Replace ID With DIA.id,NP With REG.np,fecha With HOY,;
	ENF With 0,MNUM With DIA.med,TRAT With MEDIC.mnom;
	DoSIS With DIA.Dosis,CLAVE With xclave,CURO With DIA.utec
return
* ----------------------------

** Modifica Inventario de Toros
** --------------------------
PROCEDURE INVT
parameter IDT
Select 0
Use CTOROS order 1 AGAIN EXCLUSIVE 
seek IDT
Replace cant With cant-1
If cant<1
	delete
*	pack
EndIf	
Use
Select REG
seek xid
return


** Da de Alta la(s) becerra(s)
** --------------------------
PROCEDURE ALTAB
Select REG
Set Order to 3
seek DIA.c1
If not found()
	Append Blank
	Replace ID With DIA.C1,FNAC With DIA.fecha,PNAC With DIA.P1,IDP With ABIDPC,;
	EDADF With (date()-DIA.fecha)/30.4,IDAB With ABIDP,PROC With ' CRIANZA',ENAC With DIA.E1
	Replace FMED With FNAC,UPES With PNAC,UEST With ENAC
	
	** Verifica si fue Transplante de Embrion
	** --------------------------------------
	If MIDD#0
		Replace IDM With MIDD,IDMR With ABIDM
	Else
		Replace IDM With ABIDM
	Endif		
	* ---------------------------------------
	
	Select MEDIDA
		Appe blank
		Replace id With DIA.c1,;
		fnac With DIA.fecha,;
		fecha With DIA.fecha,;
		peso With DIA.p1,;
		esta With DIA.e1
Else
	MessageBox("La ID de la Cria ya existe en el Archivo Principal !!    ",0+48,"Lactofox - Aviso")
EndIf

If DIA.c2>0 and DIA.c2#DIA.c1
 Select REG
 Set Order to 3
 seek DIA.c2
 If not found()
  Append blank
  Replace ID With DIA.C2,FNAC With DIA.fecha,PNAC With DIA.p2,IDP With ABIDPC,IDM With ABIDM,;
  EDADF With (date()-DIA.fecha)/30.4,IDAB With ABIDP,PROC With ' CRIANZA',ENAC With DIA.e2
  Replace FMED With FNAC,UPES With PNAC,UEST With ENAC
  Select MEDIDA
		appe blank
		Replace id With DIA.c2,;
		fnac With DIA.fecha,;
		fecha With DIA.fecha,;
		peso With DIA.p2,;
		esta With DIA.e2
 Else
	MessageBox("La ID de la Cria ya existe en el Archivo Principal !!    ",0+48,"Lactofox - Aviso")
 EndIf
EndIf
return

** Da de Alta lo(s) becerro(s)
** --------------------------
PROCEDURE ALTAM
Select SREG
Set Order to 1
Seek DIA.c1
If not found()
 Append blank
 Replace ID With ABIDC,FNAC With ABFPAR,PNAC With VAL(ABPNAC),IDP With ABIDPC,IDM With XID,;
 EDADF With (DATE()-ABFPAR)/30.4,IDAB With ABIDP,PROC With ' CRIANZA',ENAC With mest1
 Replace FMED With FNAC,UPES With PNAC,UEST With ENAC

	** Da de alta en Archivo de Medidas 
	** --------------------------------
	Select SMED
		appe blank
		Replace id With DIA.c1,;
		fnac With DIA.fecha,;
		fecha With DIA.fecha,;
		peso With DIA.p1,;
		esta With DIA.e1
 Else
	MessageBox("La ID de la Cria ya existe en el Archivo Principal !!    ",0+48,"Lactofox - Aviso")
 EndIf


If DIA.c2>0 and DIA.c2#DIA.c1
 Select SREG
 Set Order to 1
 Seek DIA.c2
 If not found()
  Append blank
  Replace ID With ABIDC2,FNAC With ABFPAR,PNAC With VAL(ABPNAC2),IDP With ABIDPC,IDM With XID,;
  EDADF With (DATE()-ABFPAR)/30.4,IDAB With ABIDP,PROC With ' CRIANZA',ENAC With mest2
  Replace FMED With FNAC,UPES With PNAC,UEST With ENAC

	** Da de alta en Archivo de Medidas 
	** --------------------------------
	Select SMED
		appe blank
		Replace id With DIA.c2,;
		fnac With DIA.fecha,;
		fecha With DIA.fecha,;
		peso With DIA.p2,;
		esta With DIA.e2
 Else
	MessageBox("La ID de la Cria ya existe en el Archivo Principal !!    ",0+48,"Lactofox - Aviso")
 EndIf
EndIf
return


PROCEDURE CAMBIAID
** Reemplaza por el numero de ID nuevo en Partos de Vaquillas
** ----------------------------------------------------------
	Select CALOR
	Set Order to 3
	seek DIA.id
	dele while ID=DIA.id and np=0

	Select MEDIDA
	seek DIA.id
	dele while ID=DIA.id and np=0

	Select CLIN
	Set Order to 3
	seek DIA.id
	Replace ID With xxid,NP With 1 for ID=DIA.id 

	Select 0
	Use PTB Order 3 AGAIN
	Seek DIA.id
	Replace ID With xxid,NP With 1 for ID=DIA.id
	Use
	
	Select VACUNAS
	Set Order to 3 
	seek DIA.id
	Replace ID With xxid,NP With 1 for ID=DIA.id

	Select 0
	Use BAJAS Exclusive AGAIN
	appe blank
	Replace ID With DIA.id,IDN With DIA.idn,NAC With MFNAC,FECHA With hoy,;
	MOT With [PARTO]
	Use
return


PROCEDURE CID01
* Reemplaza por numero nuevo en vacas
* -----------------------------------
Select REG
Set Order to 2

Seek DIA.IDN
If Found()
	Replace DIA.Prob With "ID Nueva ya Existe"
Else
Seek DIA.ID
	AXID=ID	
	* Reemplaza por el Numero Nuevo
	* -----------------------------
		Replace ID With DIA.IDN,IDAN With AXID
		
		SELECT LACTS
			Replace ID With DIA.IDN For ID=AXID

		Select 0
		USE MAST Again
			Replace ID With DIA.IDN For ID=AXID
		Use

		Select 0
		USE BST Again
			Replace ID With DIA.IDN For ID=AXID
		Use
		
		Select 0
		USE CCS Again
			Replace ID With DIA.IDN For ID=AXID
		Use
		
		SELECT CLIN
			Replace ID With DIA.IDN For ID=AXID And NP>0
		
		SELECT CALOR
			Replace ID With DIA.IDN For ID=AXID And NP>0
		
		SELECT VACUNAS
			Replace ID With DIA.IDN For ID=AXID And NP>0
		
		SELECT AGENDA
			Replace ID With DIA.IDN For ID=AXID And NP>0

		Select 0
		USE PTB Again
			Replace ID With DIA.IDN For ID=AXID And NP>0
		Use
		
Select DIA
Delete			
Endif
Return	

PROCEDURE CID02
* Reemplaza por numero nuevo en CRIAS
* -----------------------------------
Select REG
Set Order to 3

Seek DIA.IDN
If Found()
	Replace DIA.Prob With "ID Nueva ya Existe"
Else
Seek DIA.ID
	AXID=ID	
	* Reemplaza por el Numero Nuevo
	* -----------------------------
		Replace ID With DIA.IDN,IDAN With AXID
		
		Select CLIN
			Replace ID With DIA.IDN For ID=AXID And NP=0

		Select CALOR
			Replace ID With DIA.IDN For ID=AXID And NP=0

		Select VACUNAS
			Replace ID With DIA.IDN For ID=AXID And NP=0

		Select 0
		Use PTB Again
			Replace ID With DIA.IDN For ID=AXID And NP=0
		Use	
		
		Select MEDIDA
			Replace ID With DIA.IDN For ID=AXID And NP=0

		Select AGENDA
			Replace ID With DIA.IDN For ID=AXID And NP=0
			
Select DIA
Delete			
Endif
Return	

