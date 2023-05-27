** Asigna el codigo del hato
** --------------------------------------*
Use LDATOS
mlserie=lserie
*mlserie=mlserie+right(pal,3)
Close Databases
** ----------------------------------------*
* Checa el total de campos de ambos archivos 
** ----------------------------------------*
Use Cambio.DEF
nxx=recsize()
Use CAMBIO 
mxx=recsize()
Close Databases
	If mxx#nxx
    Set Defa To (mdir)

		Rename CAMBIO.DBF To CAMBIO3.DBF
		Copy file ..\CAMBIO.DEF To CAMBIO.DBF
		Use CAMBIO Exclusive
		Append From CAMBIO3
			REPLACE LXR With 45,MR With "S",NR With 60,shato With mlserie,OR With 3,PR With 5,QR With 279,RR With 25
			REPLACE SR With "S",TR With "N",TP With "CHR(27)+CHR(77)",ZP With "CHR(18)+CHR(15)",XR With 50

		CLOSE DATABASES
		Delete File CAMBIO3.DBF
    	Set Defa To (cmd_nam0)
	Else
	EndIf
	

USE CAMBIO Exclusive
*-- No hay datos ---*
If Reccount()=0
ZAP
Append Blank

REPLACE shato With mlserie
REPLACE AP With 40,BP With 2.50,CP With 1,DP With 21,EP With 50,FP With 45,GP With 50,HP With 30,IP With 30
REPLACE JP With 638,KP With 15,LP With 30,MP With 240,NP With 900,OP With 7,PP With 200,QP With 1095,RP With 15
REPLACE SP With 1,TP With 0,UP With 18,WP With 2,UP With 0,YP With 27,ZP With 0,YY With 5,DR With 45
REPLACE ER With 15,MONI With "C",FR With 100,GR With 185,HR With 10,IR With 14,JR With 46,KR With 40,LXR With 45
REPLACE MR With "S",NR With 60,OR With 3,PR With 5,QR With 279,RR With 25,SR With "S",TR With "N"

Repl xr With 50
EndIf

If EP=0
Repl EP With 40
Endif

If QR=0
	Repl QR With 279
EndIf

If RR=0
 Repl RR With 25
EndIf	

If XR=0
Repl XR With 50
EndIf
	
Repl shato With mlserie

Q1=AP
Q2=BP
Q3=CP
Q4=DP
Q5=EP
Q6=FP
Q7=GP
Q8=HP
Q9=IP
Q10=JP
Q11=QP
Q12=KP
Q13=LP
Q14=MP
Q15=NP
Q16=OP
Q17=PP
Q18=SP
Q19=TP
Q20=YP
Q21=ZP
Q22=WP
Q23=YY
Q24=RP
Q25=DR
Q26=ER
Q27=FR
Q28=GR
Q29=HR
Q30=IR
Q31=JR
Q32=KR
Q33=UP
Q34=MR
Q35=NR
Q36=SHATO
Q37=OR
Q38=PR
Q39=QR
Q40=RR
Q41=SR
Q42=TR
Q45=XR
Q46=YR
WLI=LXR
USE PARAM 
GO BOTTOM
x305=H305
CLOSE DATABASES
