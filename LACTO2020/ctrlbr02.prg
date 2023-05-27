PROC RV10110
Select REG
Set Order To 3
Set Filt To
GO TOP
TOTAL=0
B0=0
B1=0
B2=0
B3=0
B4=0
B5=0
B6=0
S1=0
S2=0
S3=0
S4=0
MTOT=0
B7=0
B8=0
B9=0

Scan 
	TOTAL=TOTAL+1
	DO CASE
		CASE DEST=B
			B0=B0+1
		CASE DEST<>B .AND. DATE()-FNAC<365 
			B1=B1+1
		CASE DEST<>B .AND. DATE()-FNAC>=12*30.4 .AND. DATE()-FNAC<=15*30.4
			B2=B2+1
		CASE DEST<>B .AND. DATE()-FNAC>15*30.4 
			B3=B3+1
	ENDCASE

DO CASE 
CASE STAT='INSEM'
B4=B4+1

CASE STAT='CARGA'
B5=B5+1
DO CASE
CASE NS=1
S1=S1+1
CASE NS=2
S2=S2+1
CASE NS>=3
S3=S3+1
ENDC

CASE NS>0 .AND. STAT !='INSEM' .AND. STAT !='CARGA'
B6=B6+1
ENDC
ENDS

CALC AVG(NS) FOR NS>0 and stat#"CARGA" TO S01
CALC AVG(NS) FOR STAT='CARGA' TO S02
CALC AVG(NS) FOR NS>0 TO S03
CALC AVG(PNAC) FOR PNAC>0 AND NP=0 TO MPNAC
CALC AVG(PDES) FOR PDES>0 TO MPDES
CALC AVG(DEST-FNAC) FOR DEST<>B TO DEDAD
CALC AVG(PSER-FNAC) FOR NS>0 and pser#B TO DPSER

Select BAJAS
SCAN
MTOT=MTOT+1
DO CASE
CASE MOT='MUERTE' .AND. FECHA>=(DATE()-365)
B7=B7+1
CASE MOT='PARTO' .AND. FECHA>=(DATE()-365) 
B9=B9+1
CASE MOT!='PARTO' .AND. MOT!='MUERTE' and FECHA>=(DATE()-365)
B8=B8+1
ENDC
ENDS
*CALC AVG(FECHA-NAC) FOR MOT='PARTO' .AND. YEAR(FECHA)=YEAR(DATE()) TO DPAR

CALC AVG(FECHA-NAC) FOR MOT='PARTO' .AND. FECHA>=(DATE()-365) AND (FECHA-NAC<1500) TO DPAR

v_fert=100/s03


*-------------------------------------------------------------------------*
Create Cursor REPORTE (concepto c(30),COL1 c(6),COL2 c(6))
	Append Blank
	Replace CONCEPTO With "PARAMETROS"
	Append Blank
	Replace CONCEPTO With "Peso al Nacimiento",COL1 With Str(MPNAC,5,1)
	Append Blank
	Replace CONCEPTO With "Peso Al Destete",COL1 With Str(MPDES,5,1)
	Append Blank
	Replace CONCEPTO With "Ganancia Diaria",COL1 With Str((MPDES-MPNAC)/DEDAD,5,3)
	Append Blank
	Replace CONCEPTO With "Edad Al Destete",COL1 With Str(DEDAD,3),COL2 With Str(INT(DEDAD/30.4),2)+'-'+Str(MOD(DEDAD,30.4),2)
	Append Blank
	Replace CONCEPTO With "Edad a 1er Servicio",COL1 With Str(DPSER,4),COL2 With Str(INT(DPSER/30.4),2)+'-'+Str(MOD(DPSER,30.4),2)
	Append Blank
	Replace CONCEPTO With "Edad Al Parto",COL1 With Str(DPAR,4),COL2 With Str(INT(DPAR/30.4),2)+'-'+Str(MOD(DPAR,30.4),2)
	Append Blank
	Replace CONCEPTO With "Fertilidad %",COL1 With Str(100/S03,5,1)
	Append Blank
	Replace CONCEPTO With "Servicios/Animal",COL1 With Str(S01,5,2)
	Append Blank
	Replace CONCEPTO With "Servicios/Concepcion",COL1 With Str(S02,5,2)

	Append Blank
	Replace CONCEPTO With ""
	Append Blank
	Replace CONCEPTO With "INVENTARIO"
	Append Blank
	Replace CONCEPTO With "Total de Crianza",COL1 With Str(TOTAL,4)
	Append Blank
	Replace CONCEPTO With "Crias Lactantes",COL1 With Str(B0,4),COL2 With Str((B0/TOTAL)*100,4,1)
	Append Blank
	Replace CONCEPTO With "Destete a 12 Meses",COL1 With Str(B1,4),COL2 With Str((B1/TOTAL)*100,4,1)
	Append Blank
	Replace CONCEPTO With "Animales de 12-15 Meses",COL1 With Str(B2,4),COL2 With Str((B2/TOTAL)*100,4,1)
	Append Blank
	Replace CONCEPTO With "Animales de +15 Meses",COL1 With Str(B3,4),COL2 With Str((B3/TOTAL)*100,4,1)

	Append Blank
	Replace CONCEPTO With ""
	Append Blank
	Replace CONCEPTO With "POR ESTADO REPRODUCTIVO"
	Append Blank
	Replace CONCEPTO With "Gestantes",COL1 With Str(B5,4),COL2 With Str((B5/TOTAL)*100,4,1)
	Append Blank
	Replace CONCEPTO With "Inseminadas",COL1 With Str(B4,4),COL2 With Str((B4/TOTAL)*100,4,1)
	Append Blank
	Replace CONCEPTO With "Problema",COL1 With Str(B6,4),COL2 With Str((B6/TOTAL)*100,4,1)

	Append Blank
	Replace CONCEPTO With ""
	Append Blank
	Replace CONCEPTO With "RELACION ACUMULADA ANUAL"
	Append Blank
	Replace CONCEPTO With "Muertes",COL1 With Str(B7,4),COL2 With Str((B7/(TOTAL+B7+B8+B9))*100,4,1)
	Append Blank
	Replace CONCEPTO With "Vendidas",COL1 With Str(B8,4),COL2 With Str((B8/(TOTAL+B7+B8+B9))*100,4,1)
	Append Blank
	Replace CONCEPTO With "Paridas",COL1 With Str(B9,4),COL2 With Str((B9/(TOTAL+B7+B8+B9))*100,4,1)

	Append Blank
	Replace CONCEPTO With ""
	Append Blank
	Replace CONCEPTO With "RELACION DE GESTACION"
	Append Blank
	Replace CONCEPTO With "De 1er Servicio",COL1 With Str(S1,4),COL2 With Str((S1/(B5)*100),4,1)
	Append Blank
	Replace CONCEPTO With "De 2do Servicio",COL1 With Str(S2,4),COL2 With Str((S2/(B5)*100),4,1)
	Append Blank
	Replace CONCEPTO With "De 3+  Servicio",COL1 With Str(S3,4),COL2 With Str((S3/(B5)*100),4,1)
GO TOP
RETURN


