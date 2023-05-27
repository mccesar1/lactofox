* PRODUCCION POR CORRAL    
* ---------------------
USE REG IN 1
USE CORRAL ORDER 1 IN 2
USE PESADAS IN 3

Create Table DIGISTAR (COL1 c(6),COL2 c(6),COL3 n(6,1),COL4 c(12))

Select REG
Set Order To 1
Set Filter To FB2=B

Select CORRAL
Set Order To 1
GO TOP

DO WHILE .NOT. EOF()

	NC=CNUM

	Select REG
	SET FILTER TO FB2=B
	COUNT FOR CORR=NC TO TV

	If TV>0
		CALCULATE AVG(PRM) FOR CORR=NC AND PRM>0 AND FSEC=B TO UM1
	EndIf

	IF TV>0
		Select DIGISTAR
		Append Blank
		Replace COL1 WITH Str(NC,6),COL2 With Str(TV,6),COL3 With UM1,COL4 WITH CORRAL.NOTA
	Else
	EndIf

	Select CORRAL
	SKIP
	If EOF()
		Exit
	Endif


ENDDO 

Select DIGISTAR
GO TOP
*BROWSE


** GENERA REPORTE HC PARA DIGISTAR
**********************************
		BANCO1=FCREATE('BANCO1.TXT')
				
		ww=["HC"]
		ww=ww+chr(13)+chr(10)

		=FWRITE(BANCO1,ww)
xS=0
		SCAN
		xS=xS+1 
			ww=""
			ww=ww+STR(xS,2)
			ww=ww+PADR(DIGISTAR.COL1,6)+','
			ww=ww+PADR(DIGISTAR.COL2,6)
			ww=ww+chr(13)+chr(10)

	   =FWRITE(BANCO1,ww)

		ENDSCAN

		ww=""
		ww=ww+"$"
		=FWRITE(BANCO1,ww))	

	   =FCLOSE(BANCO1) 
	   
		COPY FILE BANCO1.TXT TO cmd_nam0+"ARCHIVOS\"+"HC.CSV" CSV
		 	

** GENERA REPORTE MP PARA DIGISTAR
**********************************
SELECT PESADAS
GO BOTTOM
xFECHA=DTOC(FPES)
xFECHA=SUBSTR(xFECHA,6,2)+"-"+SUBSTR(xFECHA,9,2)+"-"+SUBSTR(xFECHA,3,2)


SELECT DIGISTAR
SET FILTER TO COL3>0

		BANCO1=FCREATE('BANCO1.TXT')
		SELECT DIGISTAR
						
		ww=["MP"]
		ww=ww+chr(13)+chr(10)
		=FWRITE(BANCO1,ww)
		
		ww=""
		ww=ww+xFECHA
		ww=ww+chr(13)+chr(10)
		=FWRITE(BANCO1,ww)
xS=0
		SCAN 
		xS=xS+1
			ww=""
			ww=ww+STR(xS,2)+','
			ww=ww+PADR(DIGISTAR.COL1,6)+','
			ww=ww+STR(DIGISTAR.COL3,6,1)
			ww=ww+chr(13)+chr(10)

	   =FWRITE(BANCO1,ww)

		ENDSCAN

		ww=""
		ww=ww+"$"
		=FWRITE(BANCO1,ww))	

	   =FCLOSE(BANCO1) 
	   
		COPY FILE BANCO1.TXT TO cmd_nam0+"ARCHIVOS\"+"MP.CSV" CSV
		RETURN
		
		