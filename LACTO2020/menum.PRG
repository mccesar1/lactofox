_SCREEN.cls
_SCREEN.CAPTION= "  LACTOFOX  Versión 3.0     "+dtoc(date())
_SCREEN.Closable=.F.
set talk off
set safety off
set century on
set date to short
set exclusive off
*set path to f:\lactowin


cz="B"
set sysmenu automatic
set sysmenu to 

*---Menu Principal----*
DEFI MENU foo 
DEFI PAD foopad_1 OF _MSYSMENU PROMPT [Vientres]
DEFI PAD foopad_2 OF _MSYSMENU PROMPT [Crianza] 


*if Qmac=SYS(2007,+ALLTRIM(xserie)+"TITA")
DEFI PAD foopad_3 OF _MSYSMENU PROMPT [Machos] 
*else
*DEFI PAD foopad_3 OF _MSYSMENU PROMPT [Machos]
*endi

DEFI PAD foopad_4 OF _MSYSMENU PROMPT [Sementales] 
DEFI PAD foopad_5 OF _MSYSMENU PROMPT [Parámetros]  
DEFI PAD foopad_6 OF _MSYSMENU PROMPT [Herramientas] 
DEFI PAD foopad_7 OF _MSYSMENU PROMPT [Salir] 

*--Menu Popup --------*
DEFI POPUP foo1 FROM 3,3 to 23,200 in screen
DEFI BAR 1 OF foo1 PROMPT [    Altas al Archivo           ] MESSAGE [    Alta de Datos Generales, Reproducción, Producción, Hospital etc.]  
DEFI BAR 2 OF foo1 PROMPT [    Captura por Tarjeta        ] MESSAGE [    Captura de eventos diarios como Partos, Revisiones, Tratamientos etc.] 
DEFI BAR 3 OF foo1 PROMPT [    Consulta de Datos] MESSAGE [    Consulta de tarjetas individuales.  (Datos Generales, Reproducción etc.)]
defi bar 4 of foo1 prompt [\-]   
DEFI BAR 5 OF foo1 PROMPT [    Informe de Partos] MESSAGE [    Captura de eventos de parto y alta automática de crias.] 
DEFI BAR 6 OF foo1 PROMPT [    Revisión y Tratamientos] MESSAGE [    Captura de Revisiones de Matriz y Tratamientos con prostaglandinas.] 
DEFI BAR 7 OF foo1 PROMPT [    Inseminación o Celo] MESSAGE [    Alta de Celos ocurridos asi como eventos de Inseminación.] 
DEFI BAR 8 OF foo1 PROMPT [    Diagnóstico de Gestación            ] MESSAGE [    Diagnósticos de gestación efectuados, Tecnico que lo realizo, Resultado etc.] 
DEFI BAR 9 OF foo1 PROMPT [    Vacas a Secas ] MESSAGE [    Captura de Secado de vacas y registro de cambio de corral/lote.] 
DEFI BAR 10 OF foo1 PROMPT [    Reporte de Bajas] MESSAGE [    Captura de Vacas muertas, Enviadas a matanza, Vendidas etc.] 
DEFI BAR 11 OF foo1 PROMPT [    Registro de Abortos] MESSAGE [    Captura de Abortos de acuerdo a los dias establecidos para este evento.] 
DEFI BAR 12 OF foo1 PROMPT [    Condición Corporal] MESSAGE [    Captura de lectura de Condición Corporal. (Individual)] 

DEFI BAR 13 OF foo1 PROMPT [    Curaciones] MESSAGE [    Registro de todas las curaciones o medicamentos preventivos aplicados.]
DEFI BAR 14 OF foo1 PROMPT [    Salidas de Hospital] MESSAGE [    Reporte de vacas dadas de Alta del hospital.]
DEFI BAR 15 OF foo1 PROMPT [    Cambios de Corral] MESSAGE [    Registro de cambios de Corral/Lote. (Individual o Completo)]
DEFI BAR 16 OF foo1 PROMPT [    Vacunaciones] MESSAGE [    Captura de vacunaciones aplicadas. (Individual, por Corral o Total)]
DEFI BAR 17 OF foo1 PROMPT [\-]
DEFI BAR 18 OF foo1 PROMPT [    Control de BST] MESSAGE [    Captura de aplicaciones, Lista de vacas a inyectar, Cambio de parámetros etc.]
ON BAR 18 OF foo1 ACTIVATE POPUP soo1
DEFI BAR 19 OF foo1 PROMPT [\-]
DEFI BAR 20 OF foo1 PROMPT [    Pesada de Leche] MESSAGE [    Registro de producción individual de leche del día de prueba.]
ON BAR 20 OF foo1 ACTIVATE POPUP soo2
DEFI BAR 21 OF foo1 PROMPT [    Lotificación y Corraleo] MESSAGE [    Módulo para lotificación automática de acuerdo a Dias en leche y Producción.]
ON BAR 21 OF foo1 ACTIVATE POPUP soo3 
DEFI BAR 22 OF foo1 PROMPT [\-]
DEFI BAR 23 OF foo1 PROMPT [    Calificación Lineal] MESSAGE [    Registro de caracteristicas de fenotipo y Toros recomendados.]
DEFI BAR 24 OF foo1 PROMPT [    Pruebas de Mastitis] MESSAGE [    Captura del resultado de Prueba California para Mastitis o CMT (Individual).]
DEFI BAR 25 OF foo1 PROMPT [    Pruebas de Salud] MESSAGE [    Captura del resultado de pruebas de salud (TB, Brucellosis etc.)]
DEFI BAR 26 OF foo1 PROMPT [\-]
DEFI BAR 27 OF foo1 PROMPT [    Reportes] MESSAGE [    Consulta y Listados de eventos de manejo, Diseño de reportes nuevos etc.] 
ON BAR 27 OF foo1 ACTIVATE POPUP soo
DEFI BAR 28 OF foo1 PROMPT [\-]
DEFI BAR 29 OF foo1 PROMPT [    Corregir Datos] MESSAGE [    Corregir información de las tarjetas de datos.]
DEFI BAR 30 OF foo1 PROMPT [    Codigos por Grupo] MESSAGE [    Asignar de una manera rapida un código para un grupo de animales.]
DEFI BAR 31 OF foo1 PROMPT [    Cambio de ID] MESSAGE [    Cambiar el numero de identificación. (ID)]
DEFI BAR 32 OF foo1 PROMPT [    Borrar Información] MESSAGE [    Borra todos los datos de una vaca. (Datos Generales, Reproducción etc.)]

DEFI POPUP foo2 FROM 3,10 to 21,41 SHADOW
DEFI BAR 1 OF foo2 PROMPT [   Altas al Archivo] MESSAGE [    Alta de Datos Generales, Vacunaciones, Reproducción, Hospital etc.]  
DEFI BAR 2 OF foo2 PROMPT [   Captura por Tarjeta] MESSAGE [    Captura de eventos diarios como Destetes, Inseminaciones, Partos etc.] 
DEFI BAR 3 OF foo2 PROMPT [   Consulta de Datos] MESSAGE [    Consulta de tarjetas individuales.  (Datos Generales, Reproducción etc.)]
DEFI BAR 4 OF foo2 PROMPT [   Informe de Destetes] MESSAGE [    Captura de eventos de destete y registro de medidas.] 
DEFI BAR 5 OF foo2 PROMPT [   Tratamientos] MESSAGE [    Captura de Tratamientos con prostaglandinas.] 
DEFI BAR 6 OF foo2 PROMPT [   Inseminación o Celo] MESSAGE [    Alta de Celos ocurridos asi como eventos de Inseminación.] 
DEFI BAR 7 OF foo2 PROMPT [   Diagnóstico de Gestación             ] MESSAGE [    Diagnosticos de gestación efectuados, Técnico que lo realizo, Resultado etc.] 
DEFI BAR 8 OF foo2 PROMPT [   Informe de Partos] MESSAGE [    Captura de eventos de parto y alta automática de crías.] 
DEFI BAR 9 OF foo2 PROMPT [   Reporte de Bajas] MESSAGE [    Captura de Crías muertas, Enviadas a matanza, Vendidas etc.] 
DEFI BAR 10 OF foo2 PROMPT [   Registro de Abortos] MESSAGE [    Captura de Abortos de acuerdo a los días establecidos para este evento.] 
DEFI BAR 11 OF foo2 PROMPT [   Registro de Medidas] MESSAGE [    Captura de medidas de Peso (Kgs.) y Estatura (Cms).] 
DEFI BAR 12 OF foo2 PROMPT [   Curaciones] MESSAGE [    Registro de todas las curaciones o medicamentos preventivos aplicados.]
DEFI BAR 13 OF foo2 PROMPT [   Salidas de Hospital] MESSAGE [    Reporte de crías dadas de Alta del hospital.]
DEFI BAR 14 OF foo2 PROMPT [   Cambios de Corral] MESSAGE [    Registro de cambios de Corral/Lote. (Individual o Completo)]
DEFI BAR 15 OF foo2 PROMPT [   Vacunaciones] MESSAGE [    Captura de vacunaciones aplicadas. (Individual, por Corral o Total)] 
DEFI BAR 16 OF foo2 PROMPT [   Lectura IMG] MESSAGE [    Captura de lectura de Inmunoglobulinas.] 
DEFI BAR 17 OF foo2 PROMPT [   Pruebas de Salud] MESSAGE [    Captura del resultado de pruebas de salud (TB, Brucellosis etc.)]
DEFI BAR 18 OF foo2 PROMPT [   Reportes] MESSAGE [    Consulta y Listados de eventos de manejo, Diseño de reportes nuevos etc.] 
ON BAR 18 OF foo2 ACTIVATE POPUP soo5
DEFI BAR 19 OF foo2 PROMPT [   Corregir Datos] MESSAGE [    Corregir información de las tarjetas de datos.]
DEFI BAR 20 OF foo2 PROMPT [   Códigos por Grupo] MESSAGE [    Asignar de una manera rapida un código para un grupo de animales.]
DEFI BAR 21 OF foo2 PROMPT [   Cambio de ID] MESSAGE [    Cambiar el número de Identificación. (ID)]
DEFI BAR 22 OF foo2 PROMPT [   Borrar Información] MESSAGE [    Borra todos los datos de una cría. (Datos Generales, Reproducción etc.)]

DEFI POPUP foo3 FROM 3,20 to 21,51 SHADOW
DEFI BAR 1 OF foo3 PROMPT [   Altas al Archivo]  MESSAGE [    Alta de Datos Generales, Vacunaciones, Hospital etc.]
DEFI BAR 2 OF foo3 PROMPT [   Captura por Tarjeta] MESSAGE [    Captura de eventos diarios como Destetes, Implantes, Agenda etc.]  
DEFI BAR 3 OF foo3 PROMPT [   Consulta de Datos] MESSAGE [    Consulta de tarjetas individuales.  (Datos Generales, Registro etc.)]
DEFI BAR 4 OF foo3 PROMPT [   Informe de Destetes] MESSAGE [    Captura de eventos de destete y registro de medidas.]
DEFI BAR 5 OF foo3 PROMPT [   Implantes] MESSAGE [    Captura de eventos de implantes para ganancia de peso.] 
DEFI BAR 6 OF foo3 PROMPT [   Castraciones] MESSAGE [    Captura de eventos de castración y registro de medidas.] 
DEFI BAR 7 OF foo3 PROMPT [   Reporte de Bajas] MESSAGE [    Captura de Machos muertos, enviados a matanza, vendidos etc.] 
DEFI BAR 8 OF foo3 PROMPT [   Registro de Medidas                  ] MESSAGE [    Captura de medidas de Peso (Kgs.) y Estatura (Cms).] 
DEFI BAR 9 OF foo3 PROMPT [   Curaciones] MESSAGE [    Registro de todas las curaciones o medicamentos preventivos aplicados.]
DEFI BAR 10 OF foo3 PROMPT [   Salidas de Hospital] MESSAGE [    Reporte de machos dados de Alta del hospital.]
DEFI BAR 11 OF foo3 PROMPT [   Cambios de Corral] MESSAGE [    Registro de cambios de Corral/Lote. (Individual o Completo)]
DEFI BAR 12 OF foo3 PROMPT [   Vacunaciones] MESSAGE [    Captura de vacunaciones aplicadas. (Individual, por Corral o Total)] 
DEFI BAR 13 OF foo3 PROMPT [   Lectura IMG] MESSAGE [    Captura de lectura de Inmunoglobulinas.] 
DEFI BAR 14 OF foo3 PROMPT [   Traspaso de Datos] MESSAGE [    Exporta e importa información entre hatos vía diskette.]
ON BAR 14 OF foo3 ACTIVATE POPUP soo7
DEFI BAR 15 OF foo3 PROMPT [   Pruebas de Salud] MESSAGE [    Captura del resultado de pruebas de salud. (Ejem. Tuberculosis)]
DEFI BAR 16 OF foo3 PROMPT [   Reportes] MESSAGE [    Consulta y Listados de eventos de manejo. (Destetes, Hospital etc.)]  
ON BAR 16 OF foo3 ACTIVATE POPUP soo6
DEFI BAR 17 OF foo3 PROMPT [   Corregir Datos] MESSAGE [    Corregir información de las tarjetas de datos.]
DEFI BAR 18 OF foo3 PROMPT [   Códigos por Grupo] MESSAGE [    Asignar de una manera rápida un código para un grupo de animales.]
DEFI BAR 19 OF foo3 PROMPT [   Cambio de ID] MESSAGE [    Cambiar el número de Identificación. (ID)]
DEFI BAR 20 OF foo3 PROMPT [   Borrar Información] MESSAGE [    Borra todos los datos de un animal. (Datos Generales, Hospital etc.)]

DEFI POPUP foo4 FROM 3,30 to 9,65 SHADOW COLO W+/N
DEFI BAR 1 OF foo4 PROMPT [    Mantenimiento] MESSAGE [    Modificar el catálogo e Inventario de toros.]
DEFI BAR 2 OF foo4 PROMPT [    Eficiencia] MESSAGE [    Cálculo de eficiencia reproductiva por rango de fechas.]
DEFI BAR 3 OF foo4 PROMPT [    Inventario de Hijas       ] MESSAGE [    Resumen de producción y estadística de las hijas de un toro.]
DEFI BAR 4 OF foo4 PROMPT [    Cambio de ID] MESSAGE [    Cambiar número de Identificación (ID).]
DEFI BAR 5 OF foo4 PROMPT [    Importar Lista de Sementales] MESSAGE [    Importa de diskette los toros recomendados de diferentes compañias.]

DEFI POPUP foo5 FROM 3,39 to 12,70 SHADOW COLO W+/N
DEFI BAR 1 OF foo5 PROMPT [    Configuración ] MESSAGE [    Cambiar parámetros generales de control, Configurar impresora etc.]
DEFI BAR 2 OF foo5 PROMPT [    Enfermedades] MESSAGE [    Modificar catálogo de Enfermedades.]
DEFI BAR 3 OF foo5 PROMPT [    Causas de Baja] MESSAGE [    Modificar catálogo de Causas de Baja.]
DEFI BAR 4 OF foo5 PROMPT [    Registro de Técnicos] MESSAGE [    Alta de Técnicos inseminadores y de Diagnóstico de preñez.]
DEFI BAR 5 OF foo5 PROMPT [    Carnet de Vacunas] MESSAGE [    Modificar el Carnet de Vacunación utilizado.]
DEFI BAR 6 OF foo5 PROMPT [    Medicamentos/Acciones] MESSAGE [    Modificar el catálogo de Medicamentos utilizados en reproducción.]
DEFI BAR 7 OF foo5 PROMPT [    Corrales/Lotes] MESSAGE [    Alta de Corrales/Lotes por tipo de ganado.]
DEFI BAR 8 OF foo5 PROMPT [    Catálogo de Códigos] MESSAGE [    Modificar el catálogo de Códigos.]

DEFI POPUP foo6 FROM 3,44 to 19,75 SHADOW COLO W+/N
DEFI BAR 1 OF foo6 PROMPT [    Reorganizar Indices] MESSAGE [    Revisa y Reconstruye índices de archivos de datos.]
DEFI BAR 2 OF foo6 PROMPT [    Compactar Archivos] MESSAGE [    Elimina definitivamente información borrada en diferentes areas.]
DEFI BAR 3 OF foo6 PROMPT [    Actualizar Versión] MESSAGE [    Actualiza los archivos de datos a una nueva versión.]
DEFI BAR 4 OF foo6 PROMPT [    Ajustes de Parámetros] MESSAGE [    Recalcula los parámetros individuales y verifica información.]
DEFI BAR 5 OF foo6 PROMPT [    Revisar Bases de Datos] MESSAGE [    Revisa los parámetros individuales tanto de Vacas como de Crianza.]
DEFI BAR 6 OF foo6 PROMPT [\-] colo w/b 
DEFI BAR 7 OF foo6 PROMPT [    Ver Datos en Diskette] MESSAGE [    Examina un diskette para verificar que existan datos.]
DEFI BAR 8 OF foo6 PROMPT [    Exportar a Diskette] MESSAGE [    Crea archivos y copia información de las tarjetas de datos a diskette.]
DEFI BAR 9 OF foo6 PROMPT [    Importar de Diskette] MESSAGE [    Recibe informacin de diskette y actualiza los archivos de datos.]
DEFI BAR 10 OF foo6 prompt [\-]
DEFI BAR 11 OF foo6 PROMPT [    Movimiento Inter-Hatos] MESSAGE [    Enviar la información de las tarjetas de datos a otro hato.]
DEFI BAR 12 OF foo6 prompt [\-]

if file("LACTOIFS.EXE")
DEFI BAR 13 OF foo6 PROMPT [    Interfase de Comunicación] MESSAGE [    Interfase con equipo electrónico de medición de leche.]
else
DEFI BAR 13 OF foo6 PROMPT [    Interfase de Comunicación] MESSAGE [    Interfase con equipo electrónico de medición de leche.] SKIP colo w/b
endi
DEFI BAR 14 OF foo6 prompt [\-]
DEFI BAR 15 OF foo6 PROMPT [    Interfase para P-Eventos ] MESSAGE [    Actualizar información de archivos de dispositivos de mano.] colo w/b 

DEFI POPUP soo  SHADOW colo W+/N FROM 18,33 to 22,63
DEFINE BAR 1 OF soo PROMPT [    Reportes de Manejo     ]  
DEFINE BAR 2 OF soo PROMPT [    Definidos por Usuario  ]
DEFINE BAR 3 OF soo PROMPT [    Activar o Desactivar   ]

DEFI POPUP soo1 SHADOW colo W+/N
DEFINE BAR 1 OF soo1 PROMPT [    Cambiar Parámetros       ]  
DEFINE BAR 2 OF soo1 PROMPT [\-]
DEFINE BAR 3 OF soo1 PROMPT [    Captura de Aplicaciones  ]
DEFINE BAR 4 OF soo1 PROMPT [    Vacas a Inyectar         ]
DEFINE BAR 5 OF soo1 PROMPT [    Programa Actual          ]
DEFINE BAR 6 OF soo1 PROMPT [\-]
DEFINE BAR 7 OF soo1 PROMPT [    Imprimir Tarjeta         ]
DEFINE BAR 8 OF soo1 PROMPT [    Marcar/Desmarcar NO BST  ]

DEFI POPUP soo2 SHADOW colo W+/N from 11,33 to 22,63 
DEFINE BAR 1 OF soo2 PROMPT [    Captura de Datos       ] MESS [    Capturar la información de la medida de producción individual.]
DEFINE BAR 2 OF soo2 PROMPT [    Borrar Datos           ] MESS [    Borrar solo la información de un turno, medida de grasa o proteína.]  
DEFINE BAR 3 OF soo2 PROMPT [    Exportar Archivo       ] MESS [    Crea un archivo para ser utilizado en computadoras de mano.]
DEFINE BAR 4 OF soo2 PROMPT [    Importar Archivo TXT   ] MESS [    Recibe la información de un archivo externo llamado PESAS.TXT]
DEFINE BAR 5 OF soo2 PROMPT [    Importar Archivo DBF   ] MESS [    Recibe la información de un archivo externo llamado PESAS.DBF]
DEFINE BAR 6 OF soo2 PROMPT [    Resumen de Captura     ] MESS [    Analiza la información capturada para verificar errores o vacas no pesadas.]
DEFINE BAR 7 OF soo2 PROMPT [    Listado de Vacas       ] MESS [    Emite una lista de todas las vacas con la producción anotada en los turnos.]
DEFINE BAR 8 OF soo2 PROMPT [    Actualizar Fecha       ] MESS [    Transmite la información al archivo PRINCIPAL y kardex de PRODUCCIÓN.]  
DEFINE BAR 9 OF soo2 PROMPT [   Cambios en Corrales    ] MESS [    Elabora un reporte de todas aquellas cambiadas de corral/lote.]
DEFINE BAR 10 OF soo2 PROMPT [    Hoja de Trabajo       ] MESS [    Imprime una lista de trabajo para anotar las medidas de producción.]

DEFI POPUP soo3 SHADOW colo W+/N FROM 15,33 to 22,63
DEFINE BAR 1 OF soo3 PROMPT [    Cálculo de Inventario    ]  
DEFINE BAR 2 OF soo3 PROMPT [    Primerizas por Separado  ]
DEFINE BAR 3 OF soo3 PROMPT [    Sin Contar # de Parto    ]
DEFINE BAR 4 OF soo3 PROMPT [    Actualizar Lotificación  ]
DEFINE BAR 5 OF soo3 PROMPT [    Cambios por Corral       ]
DEFINE BAR 6 OF soo3 PROMPT [    Marcar/Desmarcar NO LOT. ]

DEFI POPUP soo4 SHADOW colo W+/N
DEFINE BAR 1 OF soo4 PROMPT [    Altas al Archivo         ]  
DEFINE BAR 2 OF soo4 PROMPT [    Captura por Tarjeta      ]
DEFINE BAR 3 OF soo4 PROMPT [    Consulta de Datos        ]
DEFINE BAR 4 OF soo4 PROMPT [    Corregir Tarjetas        ]
DEFINE BAR 5 OF soo4 PROMPT [    Borrar Información       ]

DEFI POPUP soo5  SHADOW colo W+/N FROM 18,42 to 22,72
DEFINE BAR 1 OF soo5 PROMPT [    Reportes de Manejo     ]  
DEFINE BAR 2 OF soo5 PROMPT [    Definidos por Usuario  ]
DEFINE BAR 3 OF soo5 PROMPT [    Activar o Desactivar   ]

DEFI POPUP soo6  SHADOW colo W+/N FROM 18,45 to 22,75
DEFINE BAR 1 OF soo6 PROMPT [    Reportes de Manejo     ]  
DEFINE BAR 2 OF soo6 PROMPT [    Definidos por Usuario  ] SKIP colo w/b
DEFINE BAR 3 OF soo6 PROMPT [    Activar o Desactivar   ]

DEFI POPUP soo7  SHADOW colo W+/N FROM 16,45 to 20,75
DEFI BAR 1 OF soo7 PROMPT [    Ver Datos en Diskette] MESSAGE [    Examina un diskette para verificar que existan datos.]
DEFI BAR 2 OF soo7 PROMPT [    Exportar a Diskette] MESSAGE [    Crea archivos y envía información de las tarjetas de datos a diskette.]
DEFI BAR 3 OF soo7 PROMPT [    Importar de Diskette] MESSAGE [    Recibe información de diskette y actualiza los archivos de datos.]

*-- Menu Actions -----*

*-- Horizontal -------*
ON PAD foopad_1 OF _MSYSMENU ACTIVATE POPUP foo1
ON PAD foopad_2 OF _MSYSMENU ACTIVATE POPUP foo2
ON PAD foopad_3 OF _MSYSMENU ACTIVATE POPUP foo3
ON PAD foopad_4 OF _MSYSMENU ACTIVATE POPUP foo4 
ON PAD foopad_5 OF _MSYSMENU ACTIVATE POPUP foo5
ON PAD foopad_6 OF _MSYSMENU ACTIVATE POPUP foo6

*ON SELECTION PAD foopad_7 OF _MSYSMENU RETURN   
ON SELECTION PAD foopad_7 OF _MSYSMENU CANCEL  && Para Modificar Programas   
CLEAR EVENTS

*-- Ejecuta los Programas de los menus popup.---*

ON SELE POPUP foo1 DO VACAS WITH BAR()
ON SELE POPUP foo2 DO CRIANZA WITH BAR()
ON SELE POPUP foo3 DO LACTOBEC.exe
*ON SELE POPUP foo3 DO CTRLMDIT  && Para Modificar Programas

ON SELE POPUP foo4 DO CTRLTDIA 
ON SELE POPUP foo5 DO CTRLPECT
ON SELE POPUP foo6 DO CTRLARCH

ON SELE POPUP soo1 DO CTRLVBST 
ON SELE POPUP soo2 DO CTRLVPES 
ON SELE POPUP soo3 DO CTRLVLOT 

** Para Reportes
** -------------
*ON SELE POPUP soo  DO LACTOVAC.EXE with "VACAS" 
*ON SELE POPUP soo5 DO LACTOCRI.EXE with "CRIANZA"
*ON SELE POPUP soo6 DO LACTOBRP.exe with "BECERRO"

* Para modificar Programas activar
* --------------------------------
ON SELE POPUP soo  DO CTRLXREP with "VACAS"    && Para Modificar Programas
ON SELE POPUP soo5 DO CTRLXREP with "CRIANZA"  && Para Modificar Programas
ON SELE POPUP soo6 DO CTRLXREP with "BECERRO"  && Para Modificar Programas
* --------------------------------
ON SELE POPUP soo7 DO CTRLMTRA


*........................... INICIA EL PROGRAMA  .....................*
ACTIVATE MENU _MSYSMENU  
READ EVENTS


* PROCEDURES *
**************
Procedure VACAS
parameter BAR_NUM
hide popup foo1
@4,0 clea to 24,79

do case
case BAR_NUM==1
	*do CTRLVALT
 do form fav1
 
	
case BAR_NUM==2
clos all
use C:\lactofox\lx1\reportes
    browse fields x=RECNO(),titu NOEDIT title [ REPORTES ]




    
    *do CTRLVDAT
    
case BAR_NUM==3
defi wind W01 FROM 0,0 TO 38,100 CLOSE TITLE [ VACAS POR REVISAR ] colo sche 5
acti wind W01
select ID,FPAR,PRM,CORR,UCAL,NS,IDT,FNAC from c:\lactofox\lx1\reg order by id where np>0 into cursor DATOS
browse fields ID:H="ID NUMERO",FNAC:H="Nacimiento" 
rele wind W01
	*do CTRLVCONz

case BAR_NUM==4 or BAR_NUM==5 or BAR_NUM==6 or BAR_NUM==7 or BAR_NUM==8 or ;
     BAR_NUM==9 or BAR_NUM==10 or BAR_NUM==11 or BAR_NUM==13 or BAR_NUM==14 or ;
     BAR_NUM=16 or BAR_NUM==21 or BAR_NUM==26
	do CTRLVDIT

case BAR_NUM==15
do RV101 in ctrlvr01
*	do CTRLVMOV

case BAR_NUM==19
	do CTRLVCAL

case BAR_NUM==20
	do CTRLVMAS

case BAR_NUM==23
	do CTRLVCOR

case BAR_NUM==24
	do GCODIGO in CTRLVR16

case BAR_NUM==25
	do RV1049 in CTRLVR08


endcase
RETURN

Procedure CRIANZA
parameter BAR_NUM
hide popup foo2
@4,0 clea to 24,79
@23,0 to 23,79

do case
case BAR_NUM==1
	do CTRLBDIT

case BAR_NUM==2
    do CTRLBDAT
    
case BAR_NUM==3
	do CTRLBCON

case BAR_NUM==4 or BAR_NUM==5 or BAR_NUM==6 or BAR_NUM==7 or BAR_NUM=8 or ;
     BAR_NUM==9 or BAR_NUM=10 or BAR_NUM=12 or BAR_NUM==13 or BAR_NUM==14 or ;
     BAR_NUM==15 or BAR_NUM==16 or BAR_NUM==17 or BAR_NUM==18 or BAR_NUM==22
     do CTRLBDIT

case BAR_NUM==11
	do CTRLBPES

case BAR_NUM==19
	do CTRLBCOR

case BAR_NUM==20
	do GCODIGO2 in CTRLBR05

case BAR_NUM==21
	do RV10CAMB in CTRLBR04

endcase
@4,0 clea to 24,79
@23,0 to 23,79
return

