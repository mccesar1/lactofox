PROC RV1041
RELEASE ALL LIKE A1,AW,CW,DW,xtot

zz="N"

if ZZ=[S]
	xx="fb2=b" 
else
	xx="fb2=b and PSV#[S]"
endi

*-------------------------------------------------------------------------*
declare a1(75),a2(10)
xtotal=0
xtot=0
xpro=0
xsec=0
xact=0
xopen=0
xdab=0
xuCCS=ctod("")
a1=0
*-------------------------------------------------------------------------*
Select PESADAS
GO BOTTOM
XFPES=FPES

SELECT REG
SET FILTER TO 
Set Order To 2

SCAN
DO CASE
	
    	** Vientres dadas de baja en 365 dias
    	** -------------------------------
	CASE fb2#b and fb2>=date()-365 
		xtotal=xtotal+1

		do case
			case mot2=[MUERTE]
			a1(1)=a1(1)+1
			case mot2#[MUERTE]
			a1(2)=a1(2)+1
		endcase

			if prin>0 AND FB2>=DATE()-365
				*a1(38)=a1(38)+prin
			endi
			
			if ns>0
				a1(36)=a1(36)+1
				a1(37)=a1(37)+ns  && 20180221  Para calcular los servicios de las dada de BAJA y aumentarlos en el calculo de Fertilidad						
			endi


			if abto#b and abto>=date()-365
				*a1(39)=a1(39)+1
			endi

			if obs#[NORMAL]
				a1(40)=a1(40)+1
			endi




		** Vientres Activas y Vientres candidato Rastro
		** --------------------------------------
	CASE fb2=b
		xtot=xtot+1
		
			if prin>0 
			*	a1(38)=a1(38)+prin
			endi

			if abto#b and abto>=date()-365
				*a1(39)=a1(39)+1
			endi
		
			if obs#[NORMAL] and fpar>=date()-365
				a1(40)=a1(40)+1
			endi

			
			if p305>0 AND m305>0  &&20081206
				a1(5)=a1(5)+1							
				a1(6)=a1(6)+p305
				a1(7)=a1(7)+m305	
				a1(8)=a1(8)+PGRA
				a1(9)=a1(9)+PPRO
			endi

			if PSV="S"
				a1(59)=a1(59)+1
			endi


			** Por Estado Reproductivo
			** -----------------------
			do case
*				case stat=[FRESC] And DPR<=30

				case stat=[FRESC]
					a1(52)=a1(52)+1
				case stat=[ANEST] 
					a1(53)=a1(53)+1
				case stat=[SUCIA] 
					a1(54)=a1(54)+1
				case stat=[LIMPI] 
					a1(55)=a1(55)+1
				case stat=[INSEM] 
					a1(56)=a1(56)+1
				case stat=[VACIA] 
					a1(57)=a1(57)+1
				case stat=[TRATA] 
					a1(58)=a1(58)+1
				case stat=[ABORT] 
					a1(66)=a1(66)+1
				case stat=[CARGA] 
					a1(67)=a1(67)+1
			endcase	

				** Vientres en produccion y Secas
				** ---------------------------
			do case
				case fsec=b
					xpro=xpro+1
					del=(date()-fpar)-(date()-fsec)
					a1(17)=a1(17)+del					
						if prm>0 and pesa>=xfpes-60
							a1(3)=a1(3)+1
							a1(4)=a1(4)+prm
						endi

						if plac>0
							a1(10)=a1(10)+1
							a1(15)=a1(15)+plac
							do case
								case np=1
									a1(11)=a1(11)+1
									a1(12)=a1(12)+plac
								case np>=2
									a1(13)=a1(13)+1
									a1(14)=a1(14)+plac
							endcase
						endi
						
						** Para calculo de Celulas Somaticas.
						** ----------------------------------
						*if fccs>=xuccs and fccs#b					
						if lsc>0 and ccs>0 and fsec=b
							a1(41)=a1(41)+1
							a1(42)=a1(42)+ccs
							a1(43)=a1(43)+lsc
						endi
													
						** Promedio de Grasa y Proteina
						** ---------------------------------------
						if pgra>0 
							a1(44)=a1(44)+1
							a1(45)=a1(45)+pgra
						endi
						if ppro>0
							a1(46)=a1(46)+1
							a1(47)=a1(47)+ppro
						endi
						
						** Relacion por Produccion
						** -----------------------
						do case
							case prm>=1 and prm<=15
							a1(60)=a1(60)+1
							case prm>15 and prm<=22
							a1(61)=a1(61)+1
							case prm>22 and prm<=30
							a1(62)=a1(62)+1
							case prm>30 and prm<=40
							a1(63)=a1(63)+1
							case prm>40 
							a1(64)=a1(64)+1
							case prm=0 
							a1(65)=a1(65)+1
							
						endcase							
						


				case fsec#b
					xsec=xsec+1
			endcase


			do case 

				** PARAMETROS REPRODUCTIVOS
				** Solo Vientres activas
				** ------------------
				case &xx 
				xact=xact+1
				
					if ns>0

						** Dias a Primer Servicio
						** ----------------------
						if pser#B
						a1(20)=a1(20)+1
						a1(21)=a1(21)+(pser-fpar)
						endi
						
						** Calculo de Dias Abiertos
						** ------------------------
						do case
							case stat=[CARGA] or stat=[INSEM]
								a1(22)=a1(22)+1
								a1(23)=a1(23)+(ucal-fpar)
					
							case stat!=[CARGA] or stat!=[INSEM]
								a1(24)=a1(24)+1
								a1(25)=a1(25)+(date()-fpar)
						endcase	
						
						** Servicios por Concepcion y por vientre
						** --------------------------------------
						do case
							case stat=[CARGA] 
								a1(28)=a1(28)+1
								a1(29)=a1(29)+ns
																
										** Por # de Servicio
										do case
											case ns=1
												a1(48)=a1(48)+1
											case ns=2
												a1(49)=a1(49)+1
											case ns=3
												a1(50)=a1(50)+1
											case ns>=4
												a1(51)=a1(51)+1
										endcase

							case stat!=[CARGA]
							a1(30)=a1(30)+1
							a1(31)=a1(31)+ns

						endcase							
						
						** Para calcular fertilidad
						** ------------------------
						if stat!=[CARGA] 
							a1(36)=a1(36)+1
							a1(37)=a1(37)+ns						
						endi
											

					endi
					
					if ns=0 and DPR>=Q5
						a1(26)=a1(26)+1					
						a1(27)=a1(27)+(date()-fpar)
					endi

					if pdsc>0
						a1(32)=a1(32)+1
						a1(33)=a1(33)+pdsc
					endi

					if pcip>0
						a1(34)=a1(34)+1
						a1(35)=a1(35)+pcip
					endi
					
					if pges>=260 
						a1(68)=a1(68)+1
						a1(69)=a1(69)+pges
					endi
					
			endcase	
ENDCASE
ENDSCAN

** NOMBRES DE VARIABLES
** --------------------
xopen=a1(22)+a1(24)+a1(26)
xd1s=a1(21)/a1(20)
xdab=(a1(23)+a1(25)+a1(27))/xopen
xnsc=a1(29)/a1(28)
xprl=A1(4)/A1(3)
xprh=A1(4)/xtot

** Calculos de todos los servicios por vientre
** -------------------------------------------
xnsv=(a1(29)+a1(37))/(a1(28)+a1(36))

* Segun DHI
xbri=(xdab-xd1s)/(xnsc-1)
x3=(21/xbri)*100

xpdsc=a1(33)/a1(32)
xpcip=a1(35)/a1(34)
xip=(xdab+279)

** De acuerdo al # de sevs. en todas las Vientres.
** --------------------------------------------
xfert=(1/xnsv)*100
xtotal=xtotal+xtot
xbdel=((xpcip-xpdsc)/xpcip)*100
xadim=a1(17)/xpro
xnst=a1(31)/a1(30)
xnccs=a1(41)
	if xnccs=0
		a1(41)=.1
	endi
xnser=a1(29)+a1(37)

xpges=a1(69)/a1(68)


SELECT CALOR
SET FILTER TO NP>0 AND RINSE#B AND RINSE>=HOY-365   &&Agregado 13.03.2021
SCAN
	A1(38)=A1(38)+1	
ENDSCAN 

SELECT ABORTOS
SET FILTER TO NP>0 AND FECHA>=HOY-365  && Agregado 13.03.2021
SCAN
	A1(39)=A1(39)+1	
ENDSCAN 

SELECT REG
SET ORDER TO 2

Create Cursor REPORTE (concepto c(30),COL1 c(6),COL2 c(6))
	Append Blank
	Replace CONCEPTO With "PARAMETROS"
	Append Blank
	Replace CONCEPTO With "Dias a Primer Servicio",COL1 With Str(xd1s,5,1)
	Append Blank
	Replace CONCEPTO With "Dias entre Servicios",COL1 With Str(xbri,5,1)
	Append Blank
	Replace CONCEPTO With "Dias Abiertos",COL1 With Str(xdab,5,1)
	Append Blank
	Replace CONCEPTO With "Dias en Leche",COL1 With Str(xadim,5,1)
	Append Blank
	Replace CONCEPTO With "Dias en Leche %",COL1 With Str(xbdel,5,1)
	Append Blank
	Replace CONCEPTO With "Dias en Secas",COL1 With Str(xpdsc,5,1)
	Append Blank
	Replace CONCEPTO With "Inter/Parto Actual",COL1 With Str(xpcip/30.4,5,1)
	Append Blank
	Replace CONCEPTO With "Inter/Parto Proyectado",COL1 With Str(xip/30.4,5,1)
	Append Blank
	Replace CONCEPTO With "Calores Detectados % POST-1S",COL1 With Str(x3,5,1)
	Append Blank
	Replace CONCEPTO With "Servicios por Vientre",COL1 With Str(XNST,5,2)
	Append Blank
	Replace CONCEPTO With "Servicios por Concepcion",COL1 With Str(XNSC,5,2)
	Append Blank
	Replace CONCEPTO With "Fertilidad en Hato %",COL1 With Str(XFERT,5,1)
	Append Blank
	Replace CONCEPTO With "Produccion en Linea",COL1 With Str(A1(4)/A1(3),5,1)
	Append Blank
	Replace CONCEPTO With "Produccion en Hato",COL1 With Str(XPRH,5,1)
	Append Blank
	Replace CONCEPTO With "Pico de Produccion 1er Parto",COL1 With Str(A1(12)/A1(11),5,1)
	Append Blank
	Replace CONCEPTO With "Pico de Produccion 2+ Partos",COL1 With Str(A1(14)/A1(13),5,1)
	Append Blank
	Replace CONCEPTO With "Total del Dia de Prueba",COL1 With Str(A1(4),6)
	Append Blank
	Replace CONCEPTO With "Produccion 305 Dias",COL1 With Str(A1(6)/A1(5),6)
	Append Blank
	Replace CONCEPTO With "Promedio de Hato 305 EM",COL1 With Str(A1(7)/A1(5),6)
	Append Blank
	Replace CONCEPTO With "CCS Score Lineal",COL1 With Str(A1(43)/A1(41),5,1)
	*Append Blank
	*Replace CONCEPTO With "Promedio de Grasa%",COL1 With Str(A1(45)/A1(44),5,1)
	*Append Blank
	*Replace CONCEPTO With "Promedio de Proteina%",COL1 With Str(A1(47)/A1(46),5,1)
	Append Blank
	Replace CONCEPTO With ""
	Append Blank
	Replace CONCEPTO With "INVENTARIO"
	Append Blank
	Replace CONCEPTO With "Total de Vientres",COL1 With Str(XTOT,5)
	Append Blank
	Replace CONCEPTO With "Vientres en Produccion",COL1 With Str(XPRO,5),COL2 With Str(XPRO/XTOT*100,5,1)
	Append Blank
	Replace CONCEPTO With "Vientres Secas",COL1 With Str(XSEC,5),COL2 With Str(XSEC/XTOT*100,5,1)
	APPEND BLANK
	Replace CONCEPTO WITH ""
	Append Blank
	Replace CONCEPTO With "POR ESTADO REPRODUCTIVO"
	Append Blank
	Replace CONCEPTO With "Frescas",COL1 With Str(A1(52),5),COL2 With Str(A1(52)/XTOT*100,5,1)
	Append Blank
	Replace CONCEPTO With "Anestros",COL1 With Str(A1(53),5),COL2 With Str(A1(53)/XTOT*100,5,1)
	Append Blank
	Replace CONCEPTO With "Sucias",COL1 With Str(A1(54),5),COL2 With Str(A1(54)/XTOT*100,5,1)
	Append Blank
	Replace CONCEPTO With "Limpias",COL1 With Str(A1(55),5),COL2 With Str(A1(55)/XTOT*100,5,1)
	Append Blank
	Replace CONCEPTO With "Inseminadas",COL1 With Str(A1(56),5),COL2 With Str(A1(56)/XTOT*100,5,1)
	Append Blank
	Replace CONCEPTO With "Cargadas",COL1 With Str(A1(67),5),COL2 With Str(A1(67)/XTOT*100,5,1)
	Append Blank
	Replace CONCEPTO With "Vacias",COL1 With Str(A1(57),5),COL2 With Str(A1(57)/XTOT*100,5,1)
	Append Blank
	Replace CONCEPTO With "Tratamiento",COL1 With Str(A1(58),5),COL2 With Str(A1(58)/XTOT*100,5,1)
	Append Blank
	Replace CONCEPTO With "Abortadas",COL1 With Str(A1(66),5),COL2 With Str(A1(66)/XTOT*100,5,1)	
	Append Blank
	Replace CONCEPTO With "Probable Rastro",COL1 With Str(A1(59),5),COL2 With Str(A1(59)/XTOT*100,5,1)
	Append Blank
	Replace CONCEPTO With ""
	Append Blank
	Replace CONCEPTO With "EVALUACION ANUAL"
	Append Blank
	Replace CONCEPTO With "Inseminadas/Preñez",COL1 With Str(A1(38),5),COL2 With Str(A1(38)/XTOT*100,5,1)
	Append Blank
	Replace CONCEPTO With "Distocia/Dificultad",COL1 With Str(A1(40),5),COL2 With Str(A1(40)/XTOT*100,5,1)
	Append Blank
	Replace CONCEPTO With "Abortos/Total",COL1 With Str(A1(39),5),COL2 With Str(A1(39)/XTOT*100,5,1)
	Append Blank
	Replace CONCEPTO With "Desecho",COL1 With Str(A1(2),5),COL2 With Str(A1(2)/XTOT*100,5,1)
	Append Blank
	Replace CONCEPTO With "Mortandad",COL1 With Str(A1(1),5),COL2 With Str(A1(1)/XTOT*100,5,1)
	Append Blank
	Replace CONCEPTO With ""
	Append Blank
	Replace CONCEPTO With "RELACION DE VIENTRES GESTANTES"
	Append Blank
	Replace CONCEPTO With "De 1 Servicio",COL1 With Str(A1(48),5),COL2 With Str(A1(48)/A1(28)*100,5,1)
	Append Blank
	Replace CONCEPTO With "De 2 Servicios",COL1 With Str(A1(49),5),COL2 With Str(A1(49)/A1(28)*100,5,1)
	Append Blank
	Replace CONCEPTO With "De 3 Servicios",COL1 With Str(A1(50),5),COL2 With Str(A1(50)/A1(28)*100,5,1)
	Append Blank
	Replace CONCEPTO With "De 4+ Servicios",COL1 With Str(A1(51),5),COL2 With Str(A1(51)/A1(28)*100,5,1)

GO TOP
RETURN



** RESUMEN DE INSEMINACION  (VIENTRES)
** ----------------------------------

PROC RV1064
RELEASE ALL LIKE A1
*-------------------------------------------------------------------------*
DECLARE A1(100)
xTV=0
xTI=0
xTC=0
xS1=0
xSC=0
xPS=0
xDA=0
A1=0
*-------------------------------------------------------------------------*
SELECT TECNIC
SET ORDER TO 1
SET FILTER TO 
REPLACE ALL INSE WITH 0,CARG WITH 0 

SELECT CTOROS
SET FILTER TO 
REPLACE ALL INSE WITH 0,CARG WITH 0 

SELECT GNFERT
INDEX ON ID TO GNFERT
SET RELATION TO ALLTRIM(TORO2) INTO CTOROS,ALLTRIM(TEC) INTO TECNIC

SET FILTER TO FECHA2>=xFECHA1 AND FECHA2<=xFECHA2 AND NP>0
GO TOP

XID=0
SCAN
	IF ID#XID AND CLAVE=5
		xTV=xTV+1

	ENDIF	
	
	IF CLAVE=5	
		xTI=xTI+1
		xS1=xS1+SER
	ENDIF
	
	IF CLAVE=6	
		xTC=xTC+1
		xSC=xSC+SER
		xDA=xDA+(FECHA2-PARTO)
	ENDIF
	
	DO CASE
		CASE CLAVE=5 AND SER=1
		A1(1)=A1(1)+1
		xPS=xPS+(FECHA2-PARTO)
		
		CASE CLAVE=5 AND SER=2
		A1(2)=A1(2)+1

		CASE CLAVE=5 AND SER=3
		A1(3)=A1(3)+1

		CASE CLAVE=5 AND SER=4
		A1(4)=A1(4)+1

		CASE CLAVE=5 AND SER=5
		A1(5)=A1(5)+1

		CASE CLAVE=5 AND SER=6
		A1(6)=A1(6)+1

		CASE CLAVE=5 AND SER=7
		A1(7)=A1(7)+1

		CASE CLAVE=5 AND SER=8
		A1(8)=A1(8)+1

		CASE CLAVE=5 AND SER=9
		A1(9)=A1(9)+1

		CASE CLAVE=5 AND SER>=10
		A1(10)=A1(10)+1
		

	ENDCASE
	
	DO CASE
		CASE CLAVE=6 AND SER=1
		A1(11)=A1(11)+1

		CASE CLAVE=6 AND SER=2
		A1(12)=A1(12)+1

		CASE CLAVE=6 AND SER=3
		A1(13)=A1(13)+1

		CASE CLAVE=6 AND SER=4
		A1(14)=A1(14)+1

		CASE CLAVE=6 AND SER=5
		A1(15)=A1(15)+1

		CASE CLAVE=6 AND SER=6
		A1(16)=A1(16)+1

		CASE CLAVE=6 AND SER=7
		A1(17)=A1(17)+1

		CASE CLAVE=6 AND SER=8
		A1(18)=A1(18)+1

		CASE CLAVE=6 AND SER=9
		A1(19)=A1(19)+1

		CASE CLAVE=6 AND SER>=10
		A1(20)=A1(20)+1

	ENDCASE
			
	DO CASE
		CASE NP=1
			IF CLAVE=5
				A1(21)=A1(21)+1
			ENDIF
			IF CLAVE=6
				A1(22)=A1(22)+1
			ENDIF
		CASE NP=2
			IF CLAVE=5
				A1(23)=A1(23)+1
			ENDIF
			IF CLAVE=6
				A1(24)=A1(24)+1
			ENDIF
		CASE NP=3
			IF CLAVE=5
				A1(25)=A1(25)+1
			ENDIF
			IF CLAVE=6
				A1(26)=A1(26)+1
			ENDIF
		CASE NP=4
			IF CLAVE=5
				A1(27)=A1(27)+1
			ENDIF
			IF CLAVE=6
				A1(28)=A1(28)+1
			ENDIF
		CASE NP=5
			IF CLAVE=5
				A1(29)=A1(29)+1
			ENDIF
			IF CLAVE=6
				A1(30)=A1(30)+1
			ENDIF
		CASE NP>5
			IF CLAVE=5
				A1(31)=A1(31)+1
			ENDIF
			IF CLAVE=6
				A1(32)=A1(32)+1
			ENDIF
	ENDCASE
	

	XID=GNFERT.ID		
		
ENDSCAN


SCAN
	DO CASE
	CASE GNFERT.CLAVE=5  AND ALLTRIM(GNFERT.TORO2)=ALLTRIM(CTOROS.TORO)
		 REPLACE CTOROS.INSE WITH CTOROS.INSE+1 
	CASE GNFERT.CLAVE=6  AND ALLTRIM(GNFERT.TORO2)=ALLTRIM(CTOROS.TORO)
		 REPLACE CTOROS.CARG WITH CTOROS.CARG+1 
	ENDCASE
ENDSCAN

SCAN
	DO CASE
	CASE GNFERT.CLAVE=5  AND ALLTRIM(GNFERT.TEC)=ALLTRIM(TECNIC.TINIC)
		 REPLACE TECNIC.INSE WITH TECNIC.INSE+1 
	CASE GNFERT.CLAVE=6  AND ALLTRIM(GNFERT.TEC)=ALLTRIM(TECNIC.TINIC)
		 REPLACE TECNIC.CARG WITH TECNIC.CARG+1 
	ENDCASE
ENDSCAN


Create Cursor REPORTE (concepto c(30),COL1 c(6),COL2 c(6),COL3 c(5))
	Append Blank
	Replace CONCEPTO With "PARAMETROS"
	Append Blank
	Replace CONCEPTO With "Total de Vacas",COL1 With Str(xTV,4)
	Append Blank
	Replace CONCEPTO With "Total Inseminaciones",COL1 With Str(xTI,4),COL2 WITH STR(xTC,4),COL3 WITH STR((xTC/xTI)*100,4,1)
	Append Blank
	Replace CONCEPTO With ""
	Append Blank
	Replace CONCEPTO With "Servicios Por Vaca",COL1 With Str(xS1/xTI,4,1)
	Append Blank
	Replace CONCEPTO With "Servicios Por Concepcion",COL1 With Str(xSC/xTC,4,1)	
	Append Blank
	Replace CONCEPTO With "Dias a Primer Servicio",COL1 With Str((xPS/A1(1)),5,1)
	Append Blank
	Replace CONCEPTO With "Dias Abiertos (Preñadas)",COL1 With Str((xDA/xTC),5,1)
	Append Blank
	Replace CONCEPTO With ""
	Append Blank
	Replace CONCEPTO With "POR SERVICIO" 
	Append Blank
	Replace CONCEPTO With "Inseminadas 1 Servicio",COL1 With Str(A1(1),4),COL2 WITH STR(A1(11),4),COL3 WITH STR((A1(11)/A1(1))*100,4,1)
	Append Blank
	Replace CONCEPTO With "Inseminadas 2 Servicio",COL1 With Str(A1(2),4),COL2 WITH STR(A1(12),4),COL3 WITH STR((A1(12)/A1(2))*100,4,1)
	Append Blank
	Replace CONCEPTO With "Inseminadas 3 Servicio",COL1 With Str(A1(3),4),COL2 WITH STR(A1(13),4),COL3 WITH STR((A1(13)/A1(3))*100,4,1)
	Append Blank
	Replace CONCEPTO With "Inseminadas 4 Servicio",COL1 With Str(A1(4),4),COL2 WITH STR(A1(14),4),COL3 WITH STR((A1(14)/A1(4))*100,4,1)
	Append Blank
	Replace CONCEPTO With "Inseminadas 5 Servicio",COL1 With Str(A1(5),4),COL2 WITH STR(A1(15),4),COL3 WITH STR((A1(15)/A1(5))*100,4,1)
	Append Blank
	Replace CONCEPTO With "Inseminadas 6 Servicio",COL1 With Str(A1(6),4),COL2 WITH STR(A1(16),4),COL3 WITH STR((A1(16)/A1(6))*100,4,1)
	Append Blank
	Replace CONCEPTO With "Inseminadas 7 Servicio",COL1 With Str(A1(7),4),COL2 WITH STR(A1(17),4),COL3 WITH STR((A1(17)/A1(7))*100,4,1)
	Append Blank
	Replace CONCEPTO With "Inseminadas 8 Servicio",COL1 With Str(A1(8),4),COL2 WITH STR(A1(18),4),COL3 WITH STR((A1(18)/A1(8))*100,4,1)
	Append Blank
	Replace CONCEPTO With "Inseminadas 9 Servicio",COL1 With Str(A1(9),4),COL2 WITH STR(A1(19),4),COL3 WITH STR((A1(19)/A1(9))*100,4,1)
	Append Blank
	Replace CONCEPTO With "Inseminadas 10+ Servicios",COL1 With Str(A1(10),4),COL2 WITH STR(A1(20),4),COL3 WITH STR((A1(20)/A1(10))*100,4,1)
	Append Blank
	Replace CONCEPTO With ""
	Append Blank
	Replace CONCEPTO With "POR LACTANCIA"
	Append Blank
	Replace CONCEPTO With "Lactancia  1 Inseminadas",COL1 With Str(A1(21),4),COL2 WITH STR(A1(22),4),COL3 WITH STR((A1(22)/A1(21))*100,4,1)
	Append Blank
	Replace CONCEPTO With "Lactancia  2 Inseminadas",COL1 With Str(A1(23),4),COL2 WITH STR(A1(24),4),COL3 WITH STR((A1(24)/A1(23))*100,4,1)
	Append Blank
	Replace CONCEPTO With "Lactancia  3 Inseminadas",COL1 With Str(A1(25),4),COL2 WITH STR(A1(26),4),COL3 WITH STR((A1(26)/A1(25))*100,4,1)
	Append Blank
	Replace CONCEPTO With "Lactancia  4 Inseminadas",COL1 With Str(A1(27),4),COL2 WITH STR(A1(28),4),COL3 WITH STR((A1(28)/A1(27))*100,4,1)
	Append Blank
	Replace CONCEPTO With "Lactancia  5 Inseminadas",COL1 With Str(A1(29),4),COL2 WITH STR(A1(30),4),COL3 WITH STR((A1(30)/A1(29))*100,4,1)
	Append Blank
	Replace CONCEPTO With "Lactancia +5 Inseminadas",COL1 With Str(A1(31),4),COL2 WITH STR(A1(32),4),COL3 WITH STR((A1(32)/A1(31))*100,4,1)
	APPEND BLANK
	Replace CONCEPTO WITH ""
	APPEND BLANK
	Replace CONCEPTO WITH "FERTILIDAD POR TORO"

	SELECT CTOROS
	SET FILTER TO INSE>0

	SCAN
		SELECT REPORTE
		APPEND BLANK
		REPLACE CONCEPTO WITH CTOROS.TORO+'  '+CTOROS.NOMBRE,COL1 WITH STR(CTOROS.INSE,4),COL2 WITH STR(CTOROS.CARG,4),COL3 WITH STR((CTOROS.CARG/CTOROS.INSE)*100,5,1)
		SELECT CTOROS
	ENDSCAN
	SET FILTER TO 

	SELECT REPORTE
	APPEND BLANK
	Replace CONCEPTO WITH ""
	APPEND BLANK
	Replace CONCEPTO WITH "EFICIENCIA POR TECNICO"

	SELECT TECNIC
	SET FILTER TO INSE>0
	SCAN
		SELECT REPORTE
		APPEND BLANK
		REPLACE CONCEPTO WITH TECNIC.TNOM,COL1 WITH STR(TECNIC.INSE,4),COL2 WITH STR(TECNIC.CARG,4),COL3 WITH STR((TECNIC.CARG/TECNIC.INSE)*100,5,1)
		SELECT TECNIC
	ENDSCAN
	SET FILTER TO  
	
SELECT REPORTE
GO TOP
RETURN



** RESUMEN DE INSEMINACION  (CRIANZA)
** ----------------------------------

PROC RV10234
RELEASE ALL LIKE A1
*-------------------------------------------------------------------------*
DECLARE A1(100)
xTV=0
xTI=0
xTC=0
xS1=0
xSC=0
xPS=0
xDA=0
A1=0
*-------------------------------------------------------------------------*
SELECT TECNIC
SET ORDER TO 1
SET FILTER TO 
REPLACE ALL INSE WITH 0,CARG WITH 0 

SELECT CTOROS
SET FILTER TO 
REPLACE ALL INSE WITH 0,CARG WITH 0 

SELECT GNFERT
INDEX ON ID TO GNFERT
SET RELATION TO ALLTRIM(TORO2) INTO CTOROS,ALLTRIM(TEC) INTO TECNIC

SET FILTER TO FECHA2>=CTOD(FREPS5.CB1.Value) AND FECHA2<=CTOD(FREPS5.CB2.Value) AND NP=0
GO TOP

XID=0
SCAN
	IF ID#XID AND CLAVE=5
		xTV=xTV+1

	ENDIF	
	
	IF CLAVE=5	
		xTI=xTI+1
		xS1=xS1+SER
	ENDIF
	
	IF CLAVE=6	
		xTC=xTC+1
		xSC=xSC+SER
		IF FNAC#B
		xDA=xDA+((FECHA2-FNAC)+279)/30
		ENDIF
	ENDIF
	
	DO CASE
		CASE CLAVE=5 AND SER=1
		A1(1)=A1(1)+1
		IF FNAC#B
		xPS=xPS+(FECHA2-FNAC)/30
		ENDIF
		
		CASE CLAVE=5 AND SER=2
		A1(2)=A1(2)+1

		CASE CLAVE=5 AND SER=3
		A1(3)=A1(3)+1

		CASE CLAVE=5 AND SER=4
		A1(4)=A1(4)+1

		CASE CLAVE=5 AND SER=5
		A1(5)=A1(5)+1

		CASE CLAVE=5 AND SER>5
		A1(6)=A1(6)+1

	ENDCASE
	
	DO CASE
		CASE CLAVE=6 AND SER=1
		A1(11)=A1(11)+1

		CASE CLAVE=6 AND SER=2
		A1(12)=A1(12)+1

		CASE CLAVE=6 AND SER=3
		A1(13)=A1(13)+1

		CASE CLAVE=6 AND SER=4
		A1(14)=A1(14)+1

		CASE CLAVE=6 AND SER=5
		A1(15)=A1(15)+1

		CASE CLAVE=6 AND SER>5
		A1(16)=A1(16)+1

	ENDCASE
			
	XID=GNFERT.ID		
		
ENDSCAN


SCAN
	DO CASE
	CASE GNFERT.CLAVE=5  AND ALLTRIM(GNFERT.TORO2)=ALLTRIM(CTOROS.TORO)
		 REPLACE CTOROS.INSE WITH CTOROS.INSE+1 
	CASE GNFERT.CLAVE=6  AND ALLTRIM(GNFERT.TORO2)=ALLTRIM(CTOROS.TORO)
		 REPLACE CTOROS.CARG WITH CTOROS.CARG+1 
	ENDCASE
ENDSCAN

SCAN
	DO CASE
	CASE GNFERT.CLAVE=5  AND ALLTRIM(GNFERT.TEC)=ALLTRIM(TECNIC.TINIC)
		 REPLACE TECNIC.INSE WITH TECNIC.INSE+1 
	CASE GNFERT.CLAVE=6  AND ALLTRIM(GNFERT.TEC)=ALLTRIM(TECNIC.TINIC)
		 REPLACE TECNIC.CARG WITH TECNIC.CARG+1 
	ENDCASE
ENDSCAN


Create Cursor REPORTE (concepto c(30),COL1 c(6),COL2 c(6),COL3 c(5))
	Append Blank
	Replace CONCEPTO With "PARAMETROS"
	Append Blank
	Replace CONCEPTO With "Total de Animales",COL1 With Str(xTV,4)
	Append Blank
	Replace CONCEPTO With "Total Inseminaciones",COL1 With Str(xTI,4),COL2 WITH STR(xTC,4),COL3 WITH STR((xTC/xTI)*100,4,1)
	Append Blank
	Replace CONCEPTO With ""
	Append Blank
	Replace CONCEPTO With "Servicios Por Vaquilla",COL1 With Str(xS1/xTI,4,1)
	Append Blank
	Replace CONCEPTO With "Servicios Por Concepcion",COL1 With Str(xSC/xTC,4,1)	
	Append Blank
	Replace CONCEPTO With "Edad a Primer Servicio (Meses)",COL1 With Str((xPS/A1(1)),5,1)
	Append Blank
	Replace CONCEPTO With "Edad al Parto (Preñadas)",COL1 With Str((xDA/xTC),5,1)
	Append Blank
	Replace CONCEPTO With ""
	Append Blank
	Replace CONCEPTO With "POR SERVICIO" 
	Append Blank
	Replace CONCEPTO With "Inseminadas 1  Servicio",COL1 With Str(A1(1),4),COL2 WITH STR(A1(11),4),COL3 WITH STR((A1(11)/A1(1))*100,4,1)
	Append Blank
	Replace CONCEPTO With "Inseminadas 2  Servicio",COL1 With Str(A1(2),4),COL2 WITH STR(A1(12),4),COL3 WITH STR((A1(12)/A1(2))*100,4,1)
	Append Blank
	Replace CONCEPTO With "Inseminadas 3  Servicio",COL1 With Str(A1(3),4),COL2 WITH STR(A1(13),4),COL3 WITH STR((A1(13)/A1(3))*100,4,1)
	Append Blank
	Replace CONCEPTO With "Inseminadas 4 Servicio",COL1 With Str(A1(4),4),COL2 WITH STR(A1(14),4),COL3 WITH STR((A1(14)/A1(4))*100,4,1)
	Append Blank
	Replace CONCEPTO With "Inseminadas 5  Servicio",COL1 With Str(A1(5),4),COL2 WITH STR(A1(15),4),COL3 WITH STR((A1(15)/A1(5))*100,4,1)
	Append Blank
	Replace CONCEPTO With "Inseminadas 6+ Servicios",COL1 With Str(A1(6),4),COL2 WITH STR(A1(16),4),COL3 WITH STR((A1(16)/A1(6))*100,4,1)

	Append Blank
	Replace CONCEPTO With ""

	APPEND BLANK
	Replace CONCEPTO WITH "FERTILIDAD POR TORO"

	SELECT CTOROS
	SET FILTER TO INSE>0

	SCAN
		SELECT REPORTE
		APPEND BLANK
		REPLACE CONCEPTO WITH CTOROS.TORO+'  '+CTOROS.NOMBRE,COL1 WITH STR(CTOROS.INSE,4),COL2 WITH STR(CTOROS.CARG,4),COL3 WITH STR((CTOROS.CARG/CTOROS.INSE)*100,5,1)
		SELECT CTOROS
	ENDSCAN
	SET FILTER TO 

	SELECT REPORTE
	APPEND BLANK
	Replace CONCEPTO WITH ""
	APPEND BLANK
	Replace CONCEPTO WITH "EFICIENCIA POR TECNICO"

	SELECT TECNIC
	SET FILTER TO INSE>0
	SCAN
		SELECT REPORTE
		APPEND BLANK
		REPLACE CONCEPTO WITH TECNIC.TNOM,COL1 WITH STR(TECNIC.INSE,4),COL2 WITH STR(TECNIC.CARG,4),COL3 WITH STR((TECNIC.CARG/TECNIC.INSE)*100,5,1)
		SELECT TECNIC
	ENDSCAN
	SET FILTER TO  
	
SELECT REPORTE
GO TOP
RETURN







