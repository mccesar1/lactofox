** PROGRAMA PARA TRANSFERIR EVENTOS DE DIARIO2
*--------------------------------------------*
** Actualiza MACHOS
** ----------------
Select DIA
Set Filt To xtipo="M" and fecha<=Date()
Go Top
SCAN
 XID=DIA.id
 HOY=DIA.fecha
 UFECHA=B 
  
 Select SREG
 Set Order To 1
 Seek xid
	If found() 
		* Realiza el Cambio de Lote/Corral
		* --------------------------------	
		If DIA.CN#SREG.CORR
			Replace SREG.CORA With SREG.CORR,SREG.CORR With DIA.CN		
		EndIf		
	
		Do Case
		   Case DIA.evento=8 
          Do HOSPITAL3
		   Case DIA.evento=10 
          Do VACUNAS3
		   Case DIA.evento=12 
          Do DESTETES
		   Case DIA.evento=13 
          Do MEDIDAS
		   Case DIA.evento=14
		   	 	Do CAMBIOS3
		   Case DIA.evento=15
		   	 	Do CODIGOS 	
		   Case DIA.evento=16
		   		Do CID03	
		   Case DIA.evento=17
		   		Do IMPLANTE	
		   Case DIA.evento=18
		   		Do CASTRACION	
		   Case DIA.evento=20
		   		Do LECTURA
		ENDCASE
		
		Else
		Select DIA
		Replace PROB With "ID No existe"
	EndIf
	Select DIA
ENDSCAN	
RETURN


** M A C H O S 
** -----------

PROCEDURE HOSPITAL3
****************************************************************************
xhos="N"
xtime=0
xtrat=0

Do Case
 Case DIA.ALTA=0
  Replace ENF With DIA.enf,FTRA With hoy,TRAT With MEDIC.mnom,NTRAT With ntrat+1  			

  If HOSP=0
    Replace HOSP With 1,NTRAT With 1
  EndIf	

    Set Relation To DIA.med Into MEDIC	
    xMEDIC=MEDIC.MNOM
    
    Select SCLIN
	  Append blank
	  Replace ID With xid,FECHA With HOY,ENF With DIA.enf,;
	  TRAT With xMEDIC,CURO With DIA.utec,DoSIS With DIA.Dosis,;
	  MNUM With DIA.med,NTRAT With SREG.ntrat,CLAVE With DIA.evento
      
    Set Relation to DIA.med Into MEDIC
	  Replace MEDH With MEDIC.mnom

** Alta de Hospital
** ----------------
Case DIA.ALTA>0
	If HOSP=1
		Replace HOSP With 0,ENF With 0,fpro2 With B,MEDH With space(14),;
		     ft3 With B,ft4 With B,md2 With space(14)
	  Select SCLIN
		Append blank
		Replace ID With xid,FECHA With hoy,ENF With DIA.enf,;
		CURO With DIA.utec,TRAT With [ALTA],evento With "S",clave With DIA.evento
	Else
	  Select SCLIN
		Append blank
		Replace ID With xid,FECHA With hoy,ENF With DIA.enf,;
		CURO With DIA.utec,TRAT With [ALTA],evento With "S",clave With DIA.evento
	EndIf

EndCase 

Select SREG
*Do MAGENDA
Select DIA
Delete
Return


PROCEDURE VACUNAS3
****************************************************************************
Set Relation To DIA.vac Into GNVACUNA

If DIA.vac>0 and DIA.vac<10
	xcampo="V0"+str(DIA.vac,1)
Else
	xcampo="V"+str(DIA.vac,2)
EndIf

Replace &xcampo With hoy

Select 0
Use SVAC Order 1 Exclusive AGAIN
	Append Blank
	Replace ID With xid,fecha With hoy,numv With DIA.vac,nombre With GNVACUNA.nombre,;
					DIAST With FECHA-SREG.FNAC
Use

Select SREG
	Replace FVAC With HOY,UVAC With DIA.VAC,NVAC With GNVACUNA.NOMBRE

Select DIA
Delete
Return

PROCEDURE DESTETES
******************
Private UFECHA
ufecha=B
xfnac=SREG.fnac
mGANP=0

Do Case
   Case DEST#B 
    Replace DIA.prob With [Ya Destetado] 

   Case DEST=B and (hoy-fnac)>=Q25 

	** Reemplaza Datos en Archivo SREG
	** ------------------------------
	Replace dest With hoy,pdes With DIA.peso,edes With DIA.alt  &&corr With DIA.cn
		 
	** Actualiza AGENDA
	** ----------------
	*Do MAGENDA

	** Da de alta en Archivo de Medidas el Macho
	** -----------------------------------------
	xx=.t.
	Select SMED
	Set Order To 1
		Seek xid
			If Found()
				Scan While xid=ID
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
					Append Blank
					Replace ID With xid,;
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
					
				Endif
			Else
					** Da de Alta en el Archivo (NO Calculo)
					** ------------------------
					Append Blank
					Replace ID With xid,;
					fnac With xfnac,;
 					fecha With hoy,;
 					peso With DIA.peso,;
 					esta With DIA.alt,;
			    edad With (fecha-fnac)
			Endif								 		
	Select SREG
		If xx=.t.
			Seek xid
			Replace FMED With hoy,UPES With DIA.peso,UEST With DIA.alt,GANP With mGANP,UGNP With mGANP
	 	Endif
Select DIA
Delete
Otherwise
Replace DIA.prob With [ID no Valida]
EndCase
Return

PROCEDURE MEDIDAS
*****************
private UFECHA
ufecha=B
xfnac=SREG.fnac
mGANP=0

If FMED>=hoy 
    Replace DIA.prob With [Ya Actualizado] 
Else	 
	** Actualiza AGENDA
	** ----------------
*	Do MAGENDA

	** Da de ALTA en Archivo de Medidas el Macho
	** -----------------------------------------
	xx=.t.
	Select SMED
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
					Append Blank
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
					
				Endif
			Else
					** Da de Alta en el Archivo (NO Calculo)
					** ------------------------
					Append Blank
					Replace id With xid,;
					fnac With xfnac,;
 					fecha With hoy,;
 					peso With DIA.peso,;
 					esta With DIA.alt,;
			   	edad With (fecha-fnac)
			Endi								 		
	Select SREG
		If xx=.t.
			seek xid
			Replace FMED With hoy,UPES With DIA.peso,UEST With DIA.alt,GANP With mGANP,UGNP With mGANP
	 	Endi

Select DIA
Delete
EndIf
return

PROCEDURE CAMBIOS3
****************************************************************************
If DIA.cn>0
*	Replace CORA With CORR,CORR With DIA.cn
	Select DIA
	Delete
EndIf
Return


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


PROCEDURE CID03
* Reemplaza por numero nuevo en MACHOS
* ------------------------------------
Select SREG
Set Order to 1

Seek DIA.IDN
If Found()
	Replace DIA.Prob With "ID Nueva ya Existe"
Else
Seek DIA.ID
	AXID=ID	
	* Reemplaza por el Numero Nuevo
	* -----------------------------
		Replace ID With DIA.IDN,IDAN With AXID
		
		Select SCLIN
			Replace ID With DIA.IDN For ID=AXID IN SCLIN 

		Select SVAC
			Replace ID With DIA.IDN For ID=AXID IN SVAC

		Select 0
		Use SPTB Again
			Replace ID With DIA.IDN For ID=AXID IN SPTB
		Use	
		
		Select SMED
			Replace ID With DIA.IDN For ID=AXID IN SMED

*		Select AGENDA
*			Replace ID With DIA.IDN For ID=AXID And NP=0
			
Select DIA
Delete			
Endif
Return	


PROCEDURE IMPLANTE
****************************************************************************
If FIMP>=hoy 
  Replace DIA.prob With [Ya Actualizado] 
Else
	Replace SREG.FIMP With Hoy,TIMP With DIA.MED
	Select DIA
	Delete			
EndIf		    

RETURN


PROCEDURE CASTRACION
****************************************************************************
If FCAST>=hoy 
  Replace DIA.prob With [Ya Actualizado] 
Else
	Replace SREG.FCAST With Hoy
	Select DIA
	Delete			
EndIf		    

RETURN

PROCEDURE LECTURA
****************************************************************************
If FIMG>HOY 
  Replace DIA.prob With [Ya Actualizado] 
Else
	Replace SREG.FIMG With Hoy,LIMG WITH .SP25.Value
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
		Delete All FOR ID=XID And FECHA<Date() And TIPO='H' &xfilt
	Else
		Delete All FOR ID=XID And FECHA<Date() And TIPO='R' &xfilt
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
	
	* Reemplaza la Fecha de Programacion en SREG
	* -----------------------------------------
	Select SREG
		If DIA.evento=8
			Replace SREG.FTXH With xFECHA	
		Else
			Replace SREG.FTXR With xFECHA	
		EndIf		
	



