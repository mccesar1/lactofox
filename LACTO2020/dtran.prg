** PROGRAMA PARA TRANSFERIR EVENTOS DE DIARIO2
*--------------------------------------------*
** Actualiza VACAS
** ---------------
Private xREG,mTORO,mREGT,mNOMT,ABIDMR,ABIDMN,MCEVE

Select DIA
Set Filt To XTIPO="V" And FECHA<=Date()
GO TOP
SCAN
 XID=DIA.ID
 HOY=DIA.fecha
 UFECHA=B
 XSTAT="LIMPI"
  
Select REG
Set Filter To
Set Order To 2
	Seek xid
		If Found() 
		* Realiza el Cambio de Lote/Corral
		* --------------------------------	
		If DIA.CN#REG.CORR
			Replace REG.CORA With REG.CORR,REG.CORR With DIA.CN,REG.FCORR WITH HOY		
		EndIf	
					
		* Pone la NOTA, Fecha de Revision y si fue Scaneada o NO
		* ------------------------------------------------------
		Replace REG.NOTA1 With DIA.OBSC
		Replace REG.REVT WITH DIA.REVT,REG.SCAN WITH DIA.SCAN

		* Pasa el Ultimo Medicamento Aplicado
		* -----------------------------------
		REPLACE REG.MED1 WITH 0    && 2012.07.03 
		
		If Not Empty(DIA.MED)
			Replace MED1 With DIA.MED
		EndIf		
		
		* Realiza el Evento Seleccionado
		* ------------------------------		
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
		     	Do SECADO
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
		   Case DIA.evento=19
		   		Do ARETO
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
 Set Filter To
 Set Order To 3
	Seek xid
		If found() 
		* Realiza el Cambio de Lote/Corral
		* --------------------------------	
		If DIA.CN#REG.CORR
			Replace REG.CORA With REG.CORR,REG.CORR With DIA.CN		
		EndIf	
		
		* Pone la Anotacion   Comp- Con LM 2012.07.10
		* -----------------
		*If Not Empty(DIA.OBSC)
			Replace REG.NOTA1 With DIA.OBSC
		*Endif	
		
		* Pasa el Ultimo Medicamento Aplicado
		* -----------------------------------
		If Not Empty(DIA.MED)
		REPLACE REG.MED1 WITH 0  && 2012.07.03
		
			Replace MED1 With DIA.MED
		EndIf		 
		
		* Realiza el Evento Seleccionado
		* ------------------------------		
		Do Case
		   Case DIA.evento=1 
			 	Do PARTOS2		   
		   Case DIA.evento=2 
			 	Do REV2
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
		   Case DIA.evento=19
		   		Do ARETO2
		   Case DIA.evento=20
		   		Do LECTURA
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
	Case DIA.SX1='H' Or DIA.SX2='H' Or DIA.SX3='H'
		Select REG
		Set Order To 3
	
		Do Case
			Case Seek(DIA.C1) And DIA.SX1='H'
   				Replace DIA.prob With "ID Cria1 ya existe"
   				Return
			Case Seek(DIA.C2) And DIA.SX2='H'
   				Replace DIA.prob With "ID Cria2 ya existe"
   				Return
			Case Seek(DIA.C3) And DIA.SX3='H'
   				Replace DIA.prob With "ID Cria2 ya existe"
   				Return
		EndCase
	
	* MACHO/MACHOS
	* ------------
	Case DIA.SX1='M' Or DIA.SX2='M' Or DIA.SX3='M'
		Select SREG
		Set Order To 1
		Do Case
			Case Seek(DIA.C1)
   				Replace DIA.prob With "ID Cria1 ya existe"
   				Return
			Case Seek(DIA.C2)
   				Replace DIA.prob With "ID Cria2 ya existe"
   				Return
			Case Seek(DIA.C3)
   				Replace DIA.prob With "ID Cria3 ya existe"
   				Return
		EndCase
EndCase	

* Inicia el Proceso
* -----------------
Select REG
Set Order To 2
Seek xid 
Do Case
   Case FB2#B 
			Replace DIA.prob With  [Dada de Baja]
   Case stat='CARGA' and fsec#B and hoy-ucal<=Q17 
			Replace DIA.prob With [Gestacion Baja]
   Case stat#[CARGA] 
			Replace DIA.prob With  [No Gestante]
   Case stat=[CARGA] and FSEC=B 
			Replace DIA.prob With  [No en Secas]
		Case (Date()-FNAC)<Q10
			Replace DIA.prob With [Edad abajo del Limite]
	
   Case stat=[CARGA] and FSEC#B and hoy-ucal>=Q17 and FSEC<=hoy
		ANP=NP
		KIDC=0
		KIDC2=0
		KIDC3=0
	  STORE 0 TO MCC,mest1,mest2,mest3
	  STORE SPACE(2) TO KSEXC,ALCORR
	  STORE SPACE(6) TO POBS
	  STORE SPACE(10) TO IDPC
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
** y actualiza si fue HEMBRA o MACHO.
** ------------------------------------------
mTORO=REG.IDT

mREGT=REG.REGIDT
mNOMT=''

Select CTOROS
Set Order to 1
Seek mTORO
	If Found()
		mREGT=NREG
		mNOMT=NOMBRE
		
		Do Case
			Case DIA.sx1="H" or DIA.sx2="H" or DIA.sx3="H"
				Replace NBAS With NBAS+1

			Case DIA.sx1="M" or DIA.sx2="M" or DIA.sx3="M"
				Replace NBOS With NBOS+1
		EndCase
	Else
	Endif
	
** Actualiza la Lactancia Anterior
** -------------------------------
Select LACTS
	Append Blank
	Replace ID With XID,NP With REG.NP,FPAR With REG.FPAR,D1S With REG.D1S,;
	DAB With REG.DAB,DPR With REG.DPR,UCAL With REG.UCAL,FPAN With hoy,;
	IP With HOY-(REG.FPAR),IDT With REG.IDT,NS With REG.NS,UTEC With REG.UTEC,;
	STAT With REG.STAT,FSEC With REG.FSEC,EAP With REG.EDAD,;
	DSC With HOY-(REG.FSEC),DRT WITH HOY-(REG.FRETO),SEXC With REG.SEXC,;
	IDC With REG.IDC,IDC2 With REG.IDC2,IDC3 With REG.IDC3
	
	Replace PRX With REG.PRX,PAC With REG.PAC,FAC With REG.FAC,P305 With REG.P305,M305 With REG.M305

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
	Replace FPAR With HOY,NP With NP+1,EDAD With (FPAR-FNAC)/30.4,UCAL With B,CORR With DIA.cn,REG.IDPC With REG.IDT,;
	REG.IDT With space(10),NS With 0,UTEC With space(3),PALP With B,;
	STAT With 'FRESC',RMAT With B,NREV With 0,UTRA With B,FSEC With B,;
	PRM With 0,PRX With 0,P305 With 0,D1S With 0,DAB With 0,IDMD With 0,CTE WITH "",AREA WITH 'ORDEÑO',PTEC WITH DIA.UTEC

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
	     SX1 With DIA.sx1,;
	     IDC2 With DIA.c2,;
	     SX2 With DIA.sx2,;
	     IDC3 With DIA.c3,;
	     SX3 With DIA.sx3,;
	     SEXC With DIA.SEXC,;
		 OBS With UPPER(DIA.OBS),;
	     DGA With 0,;
	     PCEL With B,;
	     PRIN With 0,;
	     TOTA With 0,;
	     LIFEL With LIFEL+M305,;
	     NREG With NREG+1,;
	     PLAC With 0,;
	     DPIC With 0,;
	     CAU4 With space(10),;
	     STB With " ",;
	     M305 With 0,;
	     DER With FPAR-FRETO,;
	     FRETO With B,;
	     MED1 WITH 0,;
	     DM1 WITH 0,;
	     DM2 With "",;
	     DM3 WITH "",;
	     FPSV WITH B,;
	     PSV WITH ""
	     
	     
			* Pone la Anotacion
			* -----------------
			*If Not Empty(DIA.OBSC)
				Replace NOTA1 With DIA.OBSC
			*Endif	     
     
	     
** Para Calculos de CTAP.
** ---------------------
IF REG.NP>1
	Replace REG.PCIP With LACTS.IP,REG.PDSC With LACTS.DSC,REG.PGES With LACTS.IP-LACTS.DAB
ENDIF
** ------------------------------------------

Do ACONDC

Select REG

*	If SEXC='HE' or SEXC='MA' or SEXC='MH'
*		Replace IDC2 With 0
*	ENDI

Do EQM

If DIA.med>0
	Do ACLIN With DIA.evento
endIf

Select REG
	ABFPAR=HOY
	ABIDP=IDP
	ABIDM=ID
	ABIDMR=REGID
	ABIDMN=NOMID	

** Da de Alta las Crias en Registro y Medidas
** -----------------------------------------
		Do ALTAB	
		Do ALTAM

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
	Do AGENDATX
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
	Case FB2#B
		Replace DIA.prob With  [Dada de Baja]

	Case STAT=[INSEM] Or STAT=[CARGA]
		* La anota en el Kardex de Reproduccion sin quitar el estado Reproductivo,
		* solo como una revision o tratamiento, para poder poner un medicamento a un 
		* animal INSEMINADO o GESTANTE.
	   
	   MCEVE=IIF(DIA.Evento=2,'Revision','Tratamiento')
	
	Otherwise
		* Cambia el Estado a LIMPIA, SUCIA o TRATA.	  
		* -----------------------------------------
		Replace STAT With DIA.STAT,UTRA With HOY,UTEC With DIA.utec,OBSC With DIA.OBSC,MED1 With DIA.MED,CTE WITH "",IDMD WITH 0,IDT WITH SPACE(10)
			
			If STAT=space(5)
				Replace STAT With [LIMPI]
	  	Endif		
     	
     	If DIA.evento=3
     		Replace stat With [TRATA]
     	EndIf	
  	  
  	  MFCON=HOY
	  MCEVE=IIF(DIA.Evento=2,'Revision','Tratamiento')
	  	
EndCase

** Actualiza AGENDA
** ----------------
	Do MAGENDA
** ----------------
	Do AGENDATX
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
	Replace ID With xid,parto With REG.fpar,NP With REg.NP,FECHA With hoy,;
	STAT With REG.stat,MED With DIA.MED,TEC With DIA.UTEC,;
	CLAVE With DIA.EVENTO,OBSC With DIA.OBSC,TXA WITH DIA.TXA
	
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
If HOY>=UCAL
  muc=UCAL
  mns=NS  
  cstat=STAT
  AIDT=IDT
  	
  Replace UCAL With HOY,STAT With "INSEM",UTEC With DIA.UTEC,IDT With DIA.IDT,IDMD WITH DIA.IDMD
  IF DIA.IDMD>0
  	Replace CTE WITH "Receptora"
  ELSE
  Replace CTE WITH ""	
  Endif	

If muc#B and hoy<=muc+2
	** Se considera reinseminacion
	** ---------------------------
		MIDT=IDT
		Do INVT With AllTrim(IDT)
		
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
		Replace ID With DIA.id,PARTO With REG.fpar,NP With REG.NP,FECHA With hoy,;
		TORO With DIA.idt,SER With 0,STAT With REG.stat,TEC With DIA.utec,;
		MED With DIA.med,CLAVE With 5,OBSC With DIA.obsc,TXA With DIA.TXA
		
	If fecha=B
		Replace DIAS With hoy-PARTO
	Else
		Replace DIAS With hoy-UFECHA
	ENDIF
	
	*-------------------------------  2018.01.18
	DO MAGENDA
	* ------------------------------
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
	Do INVT With AllTrim(IDT)
EndIf

** Actualiza AGENDA
** ----------------
	Do MAGENDA
** ----------------
	Do AGENDATX
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

Appe Blank
 Replace ID With REG.ID,PARTO With REG.FPAR,NP With REG.NP,FECHA With REG.UCAL,;
 TORO With DIA.IDT,SER With REG.NS,STAT With REG.STAT,TEC With REG.UTEC,;
 MED With DIA.MED,FECHA2 With REG.UCAL,TORO2 With DIA.IDT,CLAVE With 5,;
 OBSC With DIA.OBSC,TXA With DIA.TXA,IDMD WITH DIA.IDMD

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

** Archivo Nuevo de FERTILIDAD  2002.12.07
** ---------------------------
	 Select GNFERT
	 Append Blank
		Replace ID With REG.ID,NP With REG.NP,PARTO WITH REG.FPAR,FECHA With REG.UCAL,TORO With DIA.IDT,SER With REG.NS,;
						STAT With REG.STAT,TEC With REG.UTEC,FECHA2 With REG.UCAL,TORO2 With DIA.IDT,;
						CLAVE With 5,TXA With DIA.TXA
**

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
 Replace UCAL With HOY,STAT With DIA.stat,UTEC With DIA.UTEC,MED1 WITH DIA.MED,CTE WITH "",IDMD WITH 0,IDT WITH SPACE(10)

 *If cstat="FRESC" or stat=space(5)
 If STAT=space(5)
	Replace STAT With "LIMPI"
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
	Do AGENDATX
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
Replace ID With xid,PARTO With REG.fpar,NP With REG.NP,FECHA With REG.ucal,;
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
Replace DIA.prob With [Checar Fecha]
ENDIf
return

PROCEDURE PALPACION
****************************************************************************
cstat=STAT
If STAT=[INSEM] or STAT=[CARGA]
	Replace stat With DIA.stat,PALP With hoy,TECP With DIA.utec
	
	If STAT=[VACIA] 
	 Replace STAT With [VACIA],MED1 WITH DIA.MED,CTE WITH "",IDMD WITH 0,IDT WITH SPACE(10)

	 cstat=[VACIA]
	 
		If DIA.Med>0
			replace UTRA With HOY		
		EndIf
	EndIf
	
	mfcon=PALP
	mceve=[Palpacion]

Do ACONDC

** Actualiza AGENDA
** ----------------
	Do MAGENDA
** ----------------
	Do AGENDATX
** ----------------	
	
Select CALOR
Set order to 2
Seek xid

If not found()
	UFECHA=B
  XFECHA=B
Else
	xFECHA=B
	xTXA=0
	xTEC=Space(3)
	
	Scan While ID=xid For NP=REG.np
	UFECHA=FECHA

	* Calcula la Fecha2 o de Inseminacion para anotarla en GNFERT
	* -----------------------------------------------------------
		If STAT='INSEM' And CLAVE=5 And SER>0
			xFECHA=FECHA
			xTXA=TXA
			xTEC=TEC
		EndIf	

*---------------------*
	Endscan
EndIf

* Si la vaca ya estaba diagnosticada CARGA ya no actualiza FECHA2 ni TORO2, ni Tecnico

If cstat="CARGA"
	Append Blank
	Replace ID With DIA.id,PARTO With REG.fpar,NP With REG.NP,FECHA With hoy,;
	STAT With cstat,MED With DIA.med,TECP With DIA.utec,DGES With HOY-REG.ucal,;
	CLAVE With DIA.evento,OBSC With DIA.obsc
Else
	Append Blank
	Replace ID With DIA.id,PARTO With REG.fpar,NP With REG.NP,FECHA With hoy,;
	TORO With REG.idt,STAT With DIA.stat,MED With DIA.med,TORO2 With REG.idt,TECP With DIA.utec,;
	SER With REG.ns,TEC With xTEC,FECHA2 With xfecha,DGES With HOY-REG.ucal,;
	CLAVE With DIA.evento,OBSC With DIA.obsc,TXA With xTXA

** Archivo Nuevo de FERTILIDAD  2002.12.07
** ---------------------------
	 Select GNFERT
	 If DIA.STAT='CARGA'
	 	Append Blank
		Replace ID With REG.ID,NP With REG.NP,PARTO WITH REG.FPAR,FECHA With HOY,TORO With REG.IDT,SER With REG.NS,;
						STAT With DIA.STAT,FECHA2 With xFECHA,TORO2 With REG.IDT,CLAVE With DIA.EVENTO,;
						TEC With xTEC,TECP With DIA.UTEC,TXA With xTXA
		EndIf
**
Endif

Select CALOR
Set Order To 2

If UFECHA=B 
	Replace DIAS With hoy-PARTO
Else
	Replace DIAS With hoy-UFECHA
EndIf

If DIA.med>0
	Do ACLIN With DIA.evento
endIf

Select DIA
Delete
Else
Replace DIA.prob With [No Inseminada]
ENDIf
RETURN

PROCEDURE SECADO
****************************************************************************
If FSEC=B
Replace FSEC With hoy,CORR With DIA.cn,STAT With DIA.STAT,LTSN WITH ' ',AREA WITH 'SECA'
mfcon=FSEC
mceve=[Secado]

Do ACONDC

Select REG

** Ajusta la Produccion a 305d y EM, ERPA y PTA
** --------------------------------------------
	Replace DPR With (date()-fpar)-(date()-fsec)
	Qpac=PAC+PRM*((DATE()-PESA)-(DATE()-FSEC))
	
	If FSEC>PESA
		Replace PAC With PAC+PRM*((DATE()-PESA)-(DATE()-FSEC)),;
						PRX With PAC/DPR
	Endif
	
	Do Case
	   Case FSEC#B and DPR<305 
	        Replace P305 With Qpac,M305 With P305*fac
	   Otherwise	
			If EDAD<=36
				Do 305
			Else
			If EDAD>36
				Do 305m
			Endif
			Endif
			Replace M305 With P305*fac
	EndCase

** Calculo de ERPA  && Modificado 04.03.2021
** -----------------------------------------
IF M305>0

	xNP=NP*ICASE(DPR>=45 and DPR<=100,.72,DPR>100 and DPR<=286,.88,DPR>286,1)

	mfac=0.5  
	mnreg=xNP && nreg+1 se cambio a numero de parto y se agrego % de correccion de acuerdo a DEL en curso 05.03.2021
	mfac=(mfac*mnreg)/(1+(mfac*(mnreg-1)))
	
*	Replace ERPA WITH 0				
	Replace ERPA With mfac*(((LIFEL+M305)/mnreg)-x305)
	Replace PTA WITH ERPA/2						
Endif

* ----------------------------------------------	
If DIA.med>0
	Do ACLIN With DIA.evento
endIf

** Actualiza AGENDA
** ----------------
	Do MAGENDA
** ----------------
	Do AGENDATX
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
mceve=[Hospital]

Do Case
 Case DIA.ALTA=0
  Replace ENF With DIA.enf,FTRA With hoy,TRAT With MEDIC.MNOM,NTRAT With NTRAT+1  			
	
    Set Relation To DIA.MED Into MEDIC	
    xMEDIC=MEDIC.MNOM
    
    Select CLIN
	  Append blank
	  Replace ID With xid,NP With REG.np,FECHA With HOY,ENF With DIA.enf,TRAT With xMEDIC,CURO With DIA.utec,;
	  DOSIS With DIA.Dosis,MNUM With DIA.MED,CLAVE With DIA.evento,STAT With REG.stat,;
	  QTO With DIA.QTO,BACT With DIA.OBSC
	  
	  Replace CLIN.EVENTO WITH IIF(REG.HOSP=0,"I","")
	  Replace CLIN.NTRAT WITH  IIF(REG.HOSP=0,1,REG.NTRAT)
	  
	  SELECT REG
		If HOSP=0
	   	 Replace HOSP With 1,NTRAT With 1
  		EndIf	

*   Set Relation To DIA.MED Into MEDIC
   	  Replace MEDH With MEDIC.MNOM
   	  
   	  DO ACONDC   && Nuevo 060820

** Alta de Hospital
** ----------------
Case DIA.ALTA>0
	If HOSP=1
		xenf=ENF
		xnp=NP
		Replace HOSP With 0,ENF With 0,fpro2 With B,MEDH With space(14),;
		     ft3 With B,ft4 With B,md2 With space(14),TXH With space(1)
	  Select CLIN
		Appe Blank
		Replace ID With xid,NP With REG.np,FECHA With hoy,ENF With DIA.enf,;
		CURO With DIA.utec,TRAT With [ALTA],evento With "S",clave With DIA.evento
	Else
	  Select CLIN
		Appe Blank
		Replace ID With xid,NP With REG.np,FECHA With hoy,ENF With DIA.enf,;
		CURO With DIA.utec,TRAT With [ALTA],evento With "S",clave With DIA.evento
	EndIf

EndCase 

** Actualiza AGENDA
** ----------------
	Do MAGENDA
** ----------------
	Do AGENDATX
** ----------------	

Select DIA
Delete
RETURN

PROCEDURE ABORTOS
****************************************************************************
If stat=[CARGA] and (hoy-ucal)<=Q17
	Replace ABTO With hoy,STAT With 'ABORT',DGA With ABTO-UCAL,CTE WITH "",IDMD WITH 0,IDT WITH SPACE(10)
*	Replace ABTO With hoy,STAT With 'ABORT',DGA With ABTO-UCAL,NS WITH 0
	mfcon=ABTO
	mceve=[Aborto]

Do ACONDC

** Actualiza AGENDA
** ----------------
	Do MAGENDA
** ----------------
	Do AGENDATX
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
*STAT With 'ABORT',MED With DIA.med,CLAVE With DIA.evento,OBSC With DIA.obsc,SER WITH 0

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
	Use In 0

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
	*Replace CORA With CORR,CORR With DIA.cn

If DIA.med>0
	Do ACLIN With DIA.evento
EndIf

	Select DIA
	Delete
EndIf
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


PROCEDURE ARETO
****************************************************************************
	If FSEC#B And FRETO=B And DIA.Fecha>=FSEC
	mceve=[A Reto]
	
		Replace FRETO With DIA.Fecha,DER WITH DATE()-FRETO
		
		If DIA.med>0
			Do ACLIN With DIA.evento
		ENDIF

		DO ACONDC  && Nuevo 060820

		Select DIA
		Delete
	Else
		Replace DIA.prob With [Fecha no Valida]
	EndIf
	
*	SELECT REG

** C R I A N Z A
** -------------
					
PROCEDURE PARTOS2
****************************************************************************
mceve=[Parto]
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
	* HEMBRA/HEMBRAS
	* --------------
		Select REG
		Set Order To 3
		
		If Seek(DIA.C1) And DIA.sx1='H'
   			Replace DIA.prob With "ID Cria1 ya existe"
   			Return
		EndIf
			
		If Seek(DIA.C2) And DIA.sx2='H'
   			Replace DIA.prob With "ID Cria2 ya existe"
   			Return
   		EndIf
   				
		If Seek(DIA.C3) And DIA.sx3='H'
   			Replace DIA.prob With "ID Cria3 ya existe"
   			Return
   		EndIf		
	
	* MACHO/MACHOS
	* ------------
		Select SREG
		Set Order To 1
		If Seek(DIA.C1) And DIA.sx1='M'
   				Replace DIA.prob With "ID Cria1 ya existe"
   				Return
   	EndIf			
		If Seek(DIA.C2) And DIA.sx1='M'
   				Replace DIA.prob With "ID Cria2 ya existe"
   				Return
   	EndIf			
		If Seek(DIA.C3) And DIA.sx1='M'
   				Replace DIA.prob With "ID Cria3 ya existe"
   				Return
   	EndIf			

* EMPIEZA EL TRASPASO
* -------------------
	xid=DIA.id
	Select REG
	Set Order To 3
	Seek xid
	Do Case
		Case (Date()-FNAC)<Q10
			Replace DIA.prob With 'Edad abajo del Limite'
		Case STAT#'CARGA' 
			Replace DIA.prob With 'No Gestante'
		Case STAT#'CARGA' Or NS<1
			Replace DIA.prob With 'Servicio No Reportado'
		
		OtherWise		

		* Reemplaza las variables del Semental
		* ------------------------------------
		mTORO=REG.IDT
		mREGT=REG.REGIDT
		mNOMT=''	
	
		Do MAGENDA                                                                                        
		
 		Replace ID With xxid,FPAR With HOY,NP With 1,PGES WITH FPAR-UCAL,OBS With UPPER(DIA.OBS),SEXC With DIA.sexc,IDC With DIA.c1,SX1 With DIA.sx1
		REPLACE IDC2 With DIA.c2,SX2 With DIA.sx2,IDC3 With DIA.c3,SX3 With DIA.sx3,IDPC With IDT,UTRA With B,MED1 With 0
		REPLACE PCEL With B,PSER With B,UCAL With B,IDT With space(10),NS With 0,UTEC With space(3),TECP With space(3),PALP With B
		REPLACE STAT With [FRESC],CORR With DIA.cn,DER With FPAR-FRETO,FRETO With B,DM1 WITH 0,DM2 WITH "",DM3 WITH "",PRIN WITH 0.AREA WITH [ORDEÑO]
        REPLACE PTEC WITH DIA.UTEC,FPSV WITH B,PSV WITH ""
        		
		* Pone la Anotacion
		* -----------------
		If Not Empty(DIA.OBSC)
			Replace REG.NOTA1 With DIA.OBSC
		ENDIF
		
		** Anota la ID Anterior
		If DIA.idn#DIA.id
			Replace IDAN With DIA.id
		Endif	  

		** Actualiza el factor de ajuste para EM
		** -------------------------------------
		Replace EDAD With (FPAR-FNAC)/30.4
		Do EQM

	** Rutina para borrar Codigos al PARIR.
	** -----------------------------------
	Declare xcod(8)
	xcod=" "
	Select REG
		xx=1
		Do while xx<9
			xcod(xx)=substr(CODIGO,xx,1)
			xx=xx+1	
		Enddo

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
					EndIf
				EndIf
			Select REG
		EndIf
		xx=xx+1
		Enddo

	If DIA.med>0 
		Do ACLIN With DIA.evento
	ENDIF
	
	DO ACONDC && Nuevo 060820

	Select REG
	ABFPAR=HOY
	ABIDP=IDP
	ABIDM=ID
	ABIDMR=REGID
	ABIDMN=NOMID		
	MIDD=REG.IDMD
	MFNAC=REG.FNAC
	
		** Archivo de Sementales Actualiza si fue Hembra o Macho
	** -----------------------------------------------------
	Select CTOROS
	Set Order to 1
	Seek mTORO
	If Found()
		mREGT=NREG
		mNOMT=NOMBRE
		Do Case
			Case DIA.sx1="H" Or DIA.sx2="H" Or DIA.sx3="H"
				Replace NBAS With NBAS+1

			Case DIA.sx1="M" Or DIA.sx2="M" Or DIA.sx3="M"
				Replace NBOS With NBOS+1
		EndCase
	Else
	Endif

	** Da de Alta las Crias en Registro y Medidas
	** -----------------------------------------
	Select REG
		Do ALTAB	
		Do ALTAM


	** Cambia EL ID por NXID
	** ---------------------
  	Do CAMBIAID
  	
  
  ** Programa en Agenda Via PALM
  ** ---------------------------
  	DO AGENDATX

		Select DIA
		Delete
EndCase		
RETURN

PROCEDURE REV2
****************************************************************************
Private UFECHA
ufecha=B
	Do Case
	Case ((hoy-fnac)/30.4)<Q26
	  Replace DIA.prob With [Edad Abajo del Limite]	
	Case stat=[CARGA]
	  Replace DIA.prob With [Gestante]
	Case stat=[INSEM]
    Replace DIA.prob With [Inseminada]
	Otherwise
      Replace STAT With DIA.STAT,UTRA With HOY,UTEC With DIA.utec,OBSC With space(14),MED1 With DIA.MED
	EndCase

	** Actualiza AGENDA
	** ----------------
		Do MAGENDA
	** ----------------
		Do AGENDATX
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
	Replace ID With xid,NP With 0,FECHA With HOY,;
	STAT With REG.stat,MED With REG.med1,TEC With REg.utec,;
	CLAVE With DIA.evento,OBSC With DIA.OBSC

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
Return

PROCEDURE TRAT2
****************************************************************************
private UFECHA
ufecha=B
	Do Case
	Case ((hoy-fnac)/30.4)<Q26
	  Replace DIA.prob With [Edad Abajo del Limite]	
	Case stat=[CARGA]
	  Replace DIA.prob With [Gestante]
	Case stat=[INSEM]
      Replace UTRA With HOY,UTEC With DIA.utec,OBSC With space(14),MED1 With DIA.MED
	Otherwise
      Replace STAT With [TRATA],UTRA With HOY,UTEC With DIA.utec,OBSC With space(14),MED1 With DIA.MED
	EndCase

	** Actualiza AGENDA
	** ----------------
		Do MAGENDA
	** ----------------
		Do AGENDATX
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
	STAT With REG.stat,MED With DIA.MED,TEC With DIA.UTEC,;
	CLAVE With DIA.evento,OBSC With DIA.OBSC
	
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
	Case ((hoy-fnac)/30.4)<Q26
	  Replace DIA.prob With [Edad No Valida]	
	Case stat=[CARGA]
	  Replace DIA.prob With [Gestante]

	otherwise

	If hoy>ucal
 	   cstat=STAT
 	   muc=UCAL
       Replace UCAL With HOY,STAT With DIA.stat,UTEC With DIA.utec

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
			Do AGENDATX
		** ----------------	


		** Para poner dias a ciclo de calor en REG.DBF
		** -------------------------------------------
		Select REG
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
	CLAVE With DIA.evento,OBSC With DIA.OBSC

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
	Case ((hoy-fnac)/30.4)<Q26
	  Replace DIA.prob With [Edad Abajo del Limite]	
*	Case stat=[CARGA]
*	  Replace DIA.prob With [Pre¤ada]

	otherwise

	If hoy>ucal
       muc=UCAL
       mns=NS  
       cstat=STAT
	   AIDT=idt

      Replace UCAL With hoy,STAT With [INSEM],UTEC With DIA.utec,IDT With DIA.idt,IDMD WITH DIA.IDMD
        IF DIA.IDMD>0
  			Replace CTE WITH "Receptora"
 		ELSE
  			Replace CTE WITH ""	
  		Endif	

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
			EndIf
		appe blank
		Replace ID With DIA.id,PARTO With REG.fpar,NP With REG.np,FECHA With hoy,;
		TORO With DIA.idt,SER With 0,STAT With REG.stat,TEC With DIA.utec,;
		MED With DIA.med,CLAVE With 5,OBSC With DIA.OBSC,TXA With DIA.TXA

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
			Do AGENDATX
	** ----------------	

	Select REG
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
	 MED With DIA.med,FECHA2 With REg.ucal,TORO2 With DIA.idt,CLAVE With 5,;
	 OBSC With DIA.OBSC,TXA With DIA.TXA

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

	** Archivo Nuevo de FERTILIDAD  2002.12.07
	** ---------------------------
		 Select GNFERT
		 Append Blank
			Replace ID With REG.ID,NP With REG.NP,PARTO WITH REG.FPAR,FECHA With REG.UCAL,TORO With DIA.IDT,SER With REG.NS,;
							STAT With REG.STAT,TEC With REG.UTEC,FECHA2 With REG.UCAL,TORO2 With DIA.IDT,;
							CLAVE With 5,TXA With DIA.TXA,FNAC WITH REG.FNAC
	**

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
	Replace stat With DIA.stat,PALP With hoy,TECP With DIA.utec
	
	If STAT=[VACIA] 
	 Replace STAT With [VACIA]

	 cstat=[VACIA]
	 
		If DIA.Med>0
			Replace UTRA With HOY
		EndIf
	EndIf
	
	mfcon=PALP
	mceve=[Palpacion]

** Actualiza AGENDA
** ----------------
	Do MAGENDA
** ----------------
	Do AGENDATX
** ----------------	

	Select CALOR
	Set Order to 3
	seek xid

If not found()
	UFECHA=B
  XFECHA=B
Else
	xFECHA=B
	xTXA=0
	xTEC=Space(3)

	Scan while ID=xid For NP=REG.np
	UFECHA=FECHA

	* Calcula la Fecha2 o de Inseminacion para anotarla en GNFERT
	* -----------------------------------------------------------
		If STAT='INSEM' And CLAVE=5 And SER>0
			xFECHA=FECHA
			xTXA=TXA
			xTEC=TEC
		EndIf	

*---------------------*
	Endscan
EndIf

* Si la vaquilla ya estaba diagnosticada CARGA ya no actualiza FECHA2 ni TORO2

If cstat="CARGA"
	appe blank
	Replace ID With DIA.id,PARTO With REG.fpar,NP With REG.np,FECHA With hoy,;
	STAT With cstat,MED With DIA.med,TECP With DIA.utec,DGES With HOY-REG.ucal,;
	CLAVE With DIA.evento,OBSC With DIA.OBSC
Else
	appe blank
	Replace ID With DIA.id,PARTO With REG.fpar,NP With REG.NP,FECHA With hoy,;
	TORO With DIA.idt,STAT With DIA.stat,MED With DIA.med,TORO2 With REG.idt,TECP With DIA.utec,;
	SER With REG.ns,TEC With xTEC,FECHA2 With xfecha,DGES With HOY-REG.ucal,;
	CLAVE With DIA.evento,OBSC With DIA.OBSC,TXA With xTXA

** Archivo Nuevo de FERTILIDAD  2002.12.07
** ---------------------------
	 If DIA.STAT='CARGA'
		 Select GNFERT
		 Append Blank
			Replace ID With REG.ID,NP With REG.NP,PARTO WITH REG.FPAR,FECHA With HOY,TORO With DIA.IDT,SER With REG.NS,;
							STAT With DIA.STAT,FECHA2 With xFECHA,TORO2 With REG.IDT,CLAVE With DIA.EVENTO,;
							TEC With xTEC,TECP With DIA.UTEC,TXA With xTXA,FNAC WITH REG.FNAC
	 EndIf	
**
Endif

Select CALOR
Set Order To 3

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
*mceve=[Hospital]

Do Case
 Case DIA.ALTA=0
  Replace ENF With DIA.enf,FTRA With hoy,TRAT With MEDIC.mnom,NTRAT With ntrat+1  			

    Set Relation to DIA.med into MEDIC	
    xMEDIC=MEDIC.MNOM
    
    Select CLIN
	  Append blank
	  Replace ID With xid,NP With REG.np,FECHA With HOY,ENF With DIA.enf,TRAT With MEDIC.mnom,CURO With DIA.utec,;
	  DOSIS With DIA.Dosis,MNUM With DIA.med,NTRAT With REG.ntrat,CLAVE With DIA.evento,stat With REG.stat,;
	  QTO WITH DIA.QTO,BACT WITH DIA.OBSC
	  
	  Replace CLIN.EVENTO WITH IIF(REG.HOSP=0,"I","")
	  Replace CLIN.NTRAT WITH  IIF(REG.HOSP=0,1,REG.NTRAT)	  
	
	 SELECT REG
		  If HOSP=0
    		Replace HOSP With 1,NTRAT With 1
 		 EndIf	
      
   * Set Relation  to DIA.MED Into MEDIC
	  Replace MEDH With MEDIC.mnom

** Alta de Hospital
** ----------------
Case DIA.ALTA>0
	If HOSP=1
		xenf=ENF
		xnp=NP
		Replace HOSP With 0,ENF With 0,fpro2 With B,MEDH With space(14),;
		     ft3 With B,ft4 With B,md2 With space(14),TXH With space(1)
	  Select CLIN
		Appe Blank
		Replace ID With xid,NP With REG.np,FECHA With hoy,ENF With DIA.enf,;
		CURO With DIA.utec,TRAT With [ALTA],evento With "S",clave With DIA.evento
	Else
	  Select CLIN
		Appe Blank
		Replace ID With xid,NP With REG.np,FECHA With hoy,ENF With DIA.enf,;
		CURO With DIA.utec,TRAT With [ALTA],evento With "S",clave With DIA.evento
	EndIf

EndCase 

Select REG
** ----------------
	Do MAGENDA
	Do AGENDATX
** ----------------	

Select DIA
dele
return

PROCEDURE ABORTOS2
****************************************************************************
If stat=[CARGA] and (hoy-ucal)<=Q17
	Replace ABTO With hoy,STAT With 'ABORT',DGA With ABTO-UCAL

** Actualiza AGENDA
** ----------------
	Do MAGENDA
** ----------------
	Do AGENDATX
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
	Use In 0
	     
**	Replace MON With a->MON

If DIA.med>0
	Do ACLIN With DIA.evento
endIf
Select DIA
dele
Else
Replace DIA.prob With [ID no Valida]
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
mGANP=0
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
		Do AGENDATX
	** ----------------	

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
			    	mGANP=ganp
					
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
			Replace FMED With hoy,upes With DIA.peso,uest With DIA.alt,GANP With mGANP,UGNP With mGANP
	 	endi
Select DIA
dele
otherwise
Replace DIA.prob With [ID no Valida]
endCase
return

PROCEDURE MEDIDAS
*****************
private UFECHA
ufecha=B
xfnac=REG.fnac
mGANP=0

If FMED>=hoy 
    Replace DIA.prob With [Ya Actualizada] 
Else	 
	** Actualiza AGENDA
	** ----------------
	Do MAGENDA
	Do AGENDATX
** ----------------	

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
			    	mGANP=ganp
					
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
			Replace FMED With hoy,upes With DIA.peso,uest With DIA.alt,UGNP with mGANP,GANP With mGANP
	 	Endi

Select DIA
dele
EndIf
return

PROCEDURE CAMBIOS2
****************************************************************************
If DIA.cn>0
	*Replace CORA With CORR,CORR With DIA.cn
	Select DIA
	dele
EndIf
Return

PROCEDURE ARETO2
****************************************************************************
If STAT='CARGA' And FRETO=B 
	Replace FRETO With DIA.Fecha,DER WITH DATE()-FRETO
	Select DIA
	Delete
Else
	Replace DIA.prob With [Fecha no Valida]
EndIf

If DIA.med>0
	Do ACLIN With DIA.evento
EndIf
Return

PROCEDURE LECTURA
****************************************************************************
If FIMG>HOY 
  Replace DIA.prob With [Ya Actualizado] 
Else
	Replace REG.FIMG With Hoy,LIMG WITH .SP25.Value
	Select DIA
	Delete			
EndIf		    
RETURN

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
		Delete All FOR ID=XID And FECHA<=HOY And TIPO='H' &xfilt
	Else
		Delete All FOR ID=XID And FECHA<=HOY And TIPO='R' &xfilt
	EndIf
	
	* Elimina la Agenda 9 en cualquier Evento para Reprogramarla si esta INSEMINADA o CARGADA
	* ------------------------------------------------------------------------------------
	Delete All For ID=XID And AGN=9 And TIPO='R' &xfilt

	** Borra la AGENDA si esta Inseminada y QUITAR="S"
	** -----------------------------------------------
	If REG.STAT="INSEM"
		DELETE ALL For ID=XID And QIA="S" And TIPO='R' &xfilt
		DELETE ALL For ID=XID And AGN=9 And TIPO='R' &xfilt

	* La Programa a Dx Gestacion
	* --------------------------
		Append Blank
		Replace ID With REG.ID,NP With REG.NP,AGN With 9,FECHA With REG.UCAL+Q7,TX With [Dx Preñez],QIA With "S",TIPO With 'R'
	EndIf

	* La Programa a Confirmar Preñez
	* ------------------------------
	If Q46>0 And REG.STAT="CARGA" And REG.PALP<REG.UCAL+(Q46-30) And REG.PALP#B 
		Append Blank
		Replace ID with REG.ID,NP With REG.NP,AGN With 9,FECHA With REG.UCAL+Q46,TX With [Dx Confirmar],QIA With "S",TIPO With 'R' 
	EndIf	
	
	* La Programa a Secar Si se da como CARGA en Dx Gestacion  12-Julio-2006  Pendiente
	
	*----------------------------------------------------------------------------------
		
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
	Endif	

	Replace FCON With hoy,CONDC With DIA.cond,PRODC With REG.PRM
	MCDEL=(FCON-FPAR)-(FCON-FSEC)

	Select CONDC
	APPEND BLANK
	Replace ID With REG.ID,NP With REG.NP,FECHA With HOY,CONDC With DIA.COND,CDEL With MCDEL,CPROD With REG.PRM,CEVE With MCEVE
Endif

Select REG
Return

** Actualiza Tarjeta Clinica Vientres
** ----------------------------------
PROCEDURE ACLIN
parameter xclave
set rela to DIA.MED into MEDIC
Select CLIN

If REG.NP>0
	Set Order to 2
Else
	Set Order to 3
Endif
	
seek XID

If not found()
	ufecha=B
Else
	scan while ID=xid for NP=REG.NP
	ufecha=fecha
	Endscan
EndIf
	Append Blank
	Replace ID With REG.ID,NP With REG.np,fecha With HOY,;
	ENF With 0,MNUM With DIA.med,TRAT With MEDIC.mnom;
	DOSIS With DIA.Dosis,CLAVE With xclave,CURO With DIA.utec;
	BACT WITH DIA.OBSC
return
* ----------------------------

** Modifica Inventario de Toros
** --------------------------
PROCEDURE INVT
Parameter IDT
Select 0
Use CTOROS Order 1 AGAIN EXCLUSIVE 

Seek Alltrim(IDT)
xREG=CTOROS.NREG

Replace CANT With IIF(CANT>0,CANT-1,CANT)
Use
Select REG
Seek XID
Replace REGIDT With xREG
Return

** Da de Alta la(s) HEMBRA(s)
** --------------------------
PROCEDURE ALTAB

* Da de Alta la Cria 1
* --------------------
If DIA.c1>0 And DIA.sx1='H'
	Select REG
	Set Order to 3
	Seek DIA.c1
	If not found()
		Append Blank
		Replace ID With DIA.C1,FNAC With DIA.fecha,PNAC With DIA.P1,ENAC With DIA.E1,;
		IDP With mTORO,REGIDP With mREGT,NOMIDP With mNOMT,IDM With ABIDM,REGIDM With ABIDMR,NOMIDM With ABIDMN,;
		EDADF With (date()-DIA.fecha)/30.4,IDAB With ABIDP,PROC With ' CRIANZA'
		Replace FMED With FNAC,UPES With PNAC,UEST With ENAC,RAZID WITH DIA.RAZA,AREA WITH 'CRIANZA'
	
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
		MessageBox("La ID de la Cria ya existe en el Archivo Principal !!    ",0+48,"DAIRYFOX - Aviso")
	EndIf
EndIf

* Da de Alta la Cria 2
* --------------------
If DIA.c2>0 And DIA.sx2='H'
 Select REG
 Set Order to 3
 seek DIA.c2
 If not found()
  Append blank
  Replace ID With DIA.C2,FNAC With DIA.fecha,PNAC With DIA.p2,;
  IDP With mTORO,REGIDP With mREGT,NOMIDP With mNOMT,IDM With ABIDM,REGIDM With ABIDMR,NOMIDM With ABIDMN,;
  EDADF With (date()-DIA.fecha)/30.4,IDAB With ABIDP,PROC With ' CRIANZA',ENAC With DIA.e2
  Replace FMED With FNAC,UPES With PNAC,UEST With ENAC,RAZID WITH DIA.RAZA,AREA WITH 'CRIANZA'

	** Verifica si fue Transplante de Embrion
	** --------------------------------------
	If MIDD#0
		Replace IDM With MIDD,IDMR With ABIDM
	Else
		Replace IDM With ABIDM
	Endif		
	* ---------------------------------------

  Select MEDIDA
		appe blank
		Replace id With DIA.c2,;
		fnac With DIA.fecha,;
		fecha With DIA.fecha,;
		peso With DIA.p2,;
		esta With DIA.e2
 Else
	MessageBox("La ID de la Cria ya existe en el Archivo Principal !!    ",0+48,"DAIRYFOX - Aviso")
 EndIf
EndIf

* Da de Alta la Cria 3
* --------------------
If DIA.c3>0 and DIA.sx3='H'
 Select REG
 Set Order to 3
 Seek DIA.c3
 If not found()
  Append blank
  Replace ID With DIA.C3,FNAC With DIA.fecha,PNAC With DIA.p3,;
  IDP With mTORO,REGIDP With mREGT,NOMIDP With mNOMT,IDM With ABIDM,REGIDM With ABIDMR,NOMIDM With ABIDMN,;
  EDADF With (date()-DIA.fecha)/30.4,IDAB With ABIDP,PROC With ' CRIANZA',ENAC With DIA.e3,AREA WITH 'CRIANZA'
  Replace FMED With FNAC,UPES With PNAC,UEST With ENAC

  Select MEDIDA
		Appe blank
		Replace ID With DIA.c3,;
		fnac With DIA.fecha,;
		fecha With DIA.fecha,;
		peso With DIA.p3,;
		esta With DIA.e3
 Else
	MessageBox("La ID de la Cria ya existe en el Archivo Principal !!    ",0+48,"DAIRYFOX - Aviso")
 EndIf
EndIf

RETURN

** Da de Alta el(s) MACHO(s)
** --------------------------
PROCEDURE ALTAM
If DIA.c1>0 and DIA.sx1='M'
	Select SREG
	Set Order to 1
	Seek DIA.c1
	If not found()
 		Append blank
 		Replace ID With DIA.c1,FNAC With DIA.Fecha,PNAC With DIA.p1,;
 		IDP With mTORO,REGIDP With mREGT,NOMIDP With mNOMT,IDM With ABIDM,REGIDM With ABIDMR,NOMIDM With ABIDMN,;		
 		EDADF With (DATE()-ABFPAR)/30.4,IDAB With ABIDP,PROC With ' CRIANZA',ENAC With DIA.e1
 		Replace FMED With FNAC,UPES With PNAC,UEST With ENAC,CLASF With 'Venta',RAZID WITH DIA.RAZA,AREA WITH 'CRIANZA'

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
		MessageBox("La ID de la Cria ya existe en el Archivo Principal !!    ",0+48,"DAIRYFOX - Aviso")
 	EndIf
EndIf

* Da de Alta el Macho 2
* ---------------------
If DIA.c2>0 and DIA.sx2='M'
	Select SREG
 	Set Order to 1
 	Seek DIA.c2
 	If not found()
  	Append blank
  	Replace ID With DIA.c2,FNAC With DIA.Fecha,PNAC With DIA.p2,;
 		IDP With mTORO,REGIDP With mREGT,NOMIDP With mNOMT,IDM With ABIDM,REGIDM With ABIDMR,NOMIDM With ABIDMN,;
  	EDADF With (DATE()-ABFPAR)/30.4,IDAB With ABIDP,PROC With ' CRIANZA',ENAC With DIA.e2
  	Replace FMED With FNAC,UPES With PNAC,UEST With ENAC,CLASF With 'Venta',RAZID WITH DIA.RAZA,AREA WITH 'CRIANZA'

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
		MessageBox("La ID de la Cria ya existe en el Archivo Principal !!    ",0+48,"DAIRYFOX - Aviso")
 	EndIf
EndIf

* Da de Alta el Macho 3
* ---------------------
If DIA.c3>0 And DIA.sx3='M'
 Select SREG
 Set Order to 1
 Seek DIA.c3
 If not found()
		Append blank
	 	Replace ID With DIA.c3,FNAC With DIA.Fecha,PNAC With DIA.p3,;
	 	IDP With mTORO,REGIDP With mREGT,NOMIDP With mNOMT,IDM With ABIDM,REGIDM With ABIDMR,NOMIDM With ABIDMN,;
	 	EDADF With (DATE()-ABFPAR)/30.4,IDAB With ABIDP,PROC With ' CRIANZA',ENAC With DIA.e3
  	Replace FMED With FNAC,UPES With PNAC,UEST With ENAC,CLASF With 'Venta'

		** Da de alta en Archivo de Medidas 
		** --------------------------------
		Select SMED
			Appe blank
			Replace ID With DIA.c3,;
			fnac With DIA.fecha,;
			fecha With DIA.fecha,;
			peso With DIA.p3,;
			esta With DIA.e3
  Else
		MessageBox("La ID de la Cria ya existe en el Archivo Principal !!    ",0+48,"DAIRYFOX - Aviso")
 	EndIf
EndIf

RETURN

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
	
	Select AGENDA
	Set Order To 1
	Replace ID With xxid,NP With 1 For ID=DIA.id And NP=0
	
	Select LTX
	Replace ID With xxid,NP With 1 For ID=DIA.id And NP=0

Return


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
	AXID=REG.ID	
	
	* Reemplaza por el Numero Nuevo
	* -----------------------------
		Replace ID With DIA.IDN,IDAN With AXID
		
		SELECT LACTS
		Set Order To 1
		Go Top
		Replace ID With DIA.IDN For ID=AXID IN LACTS

		SELECT CLIN
		Set Order To 1
		Go Top
		Replace ID With DIA.IDN For ID=AXID And NP>0 IN CLIN 
		
		SELECT CALOR
		Set Order To 1
		Go Top
		Replace ID With DIA.IDN For ID=AXID And NP>0 IN CALOR

		SELECT PROD
		Go Top
		Replace ID With DIA.IDN For ID=AXID And NP>0 IN PROD

		SELECT VACUNAS
		Replace ID With DIA.IDN For ID=AXID And NP>0 IN VACUNAS
		
		SELECT AGENDA
		Set Order To 1
		Go Top
		Replace ID With DIA.IDN For ID=AXID And NP>0 And AGN>0 IN AGENDA

		SELECT LTX
*		Set Order To 2
		Replace ID With DIA.IDN For ID=AXID And NP>0 IN LTX

		SELECT CONDC
		Set Order To 1
		Replace ID With DIA.IDN For ID=AXID And NP>0 IN CONDC

		Select 0
		USE MAST Again
			Replace ID With DIA.IDN For ID=AXID IN MAST
		Use

		Select 0
		USE BST Again
			Replace ID With DIA.IDN For ID=AXID IN BST
		Use
		
		Select 0
		USE CCS Again
			Replace ID With DIA.IDN For ID=AXID IN CCS
		Use

		Select 0
		USE PTB Again
			Replace ID With DIA.IDN For ID=AXID And NP>0 IN PTB
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
		Set Order TO 1
		Go Top
		Replace ID With DIA.IDN For ID=AXID And NP=0 IN CLIN

		Select CALOR
		Set Order To 1
		Go Top
		Replace ID With DIA.IDN For ID=AXID And NP=0 IN CALOR

		Select VACUNAS
		Set Order To 1
		Go Top
		Replace ID With DIA.IDN For ID=AXID And NP=0 IN VACUNAS

		Select MEDIDA
			Replace ID With DIA.IDN For ID=AXID And NP=0 IN MEDIDA

		Select AGENDA
		Set Order TO 1
		Go Top
		Replace ID With DIA.IDN For ID=AXID And NP=0 IN AGENDA

		Select LTX
		Set Order TO 1
		Go Top
		Replace ID With DIA.IDN For ID=AXID And NP=0 IN LTX
			
		Select 0
		Use PTB Again
			Replace ID With DIA.IDN For ID=AXID And NP=0 IN PTB
		Use	
		
Select DIA
Delete			
Endif
Return	

PROCEDURE AGENDATX
* Agrega a la Agenda si tiene TX Programados (Solo para datos capturados en PALM)
* ------------------------------------------
Select DIA
	If DIA.TX>0
	Select DPTX
		GO TOP
		Set Filter To NUM=DIA.TX
		
		xFECHA=DIA.FECHA+(DIA.SPD-1)  &&HOY
		iFECHA=B
		x=0
		Scan For DIA>=0 And Not Empty(M1) 
			* Agrega a la Agenda
			* ------------------
			xFECHA=xFECHA+DPTX.DIA
			
			Select AGENDA
			Append Blank
			Replace ID With REG.ID;
			NP With REG.NP;
			FECHA With xFECHA;
			DIA  With DPTX.DIA;
			AGN  With DIA.AGENDA;
			TX   With DIA.DX;
			N1   WITH DPTX.N1;
			M1   With DPTX.M1;
			D1   With DPTX.D1;
			M2   With DPTX.M2;
			D2   With DPTX.D2;
			M3   With DPTX.M3;
			D3   With DPTX.D3;
			QIA  With DPTX.QIA;
			CORR With REG.CORR;
			DX   With ENFERM.NOMBRE;  
			TIPO WITH DIA.TIPO
				
			If x=0
				iFECHA=xFECHA
				* Agrega al Archivo de Tratamientos Programados
				* ---------------------------------------------
				Select LTX
				GO BOTTOM
				If ID=REG.ID And FECHA=iFECHA  
				Else
					Append Blank
					Replace ID With REG.ID,NP With REG.NP,FECHA With iFECHA,DESC With DIA.DX,TIPO WITH IIF(DIA.TIPO='R','REPRODUCCION','HOSPITAL')
					  &&,TIPO With ENFERM.NOMBRE 
					
					*Replace ID With REG.ID,NP With REG.NP,FECHA With iFECHA,DESC With DTX.NOMBRE  &&,TIPO With ENFERM.NOMBRE 

				EndIf
				
				* Reemplaza el Penultimo e Ultimo Tx en REG
				* -----------------------------------------
				Select REG
				Replace FPRO2 With FPRO,MEDH With MEDP,FPRO With iFECHA,MEDP With DIA.DX
			EndIf

			* Reemplaza la ultima fecha dependiendo si es Reproductivo/Hospital.
			Select REG
			If DIA.TIPO='R'
				Replace FTXR With xFECHA
			Else	
				Replace FTXH With xFECHA	
			EndIf

			x=x+1
			
			Select DPTX
			* ------------------
		EndScan
Else
ENDIF
RETURN