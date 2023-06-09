CLOSE TABLES
B=CTOD("")
SET PATH TO &cmd_nam5

IF FILE(cmd_nam5+'\KEVENTO.DBF')
*ELSE
CREATE TABLE (cmd_nam5+'\KEVENTO.DBF') (ID N(5),FECHA D(8),NP N(2),ESTADO C(7),CLAVE n(1))

USE C:\LACTO2020\LX6\CALOR    IN 3
USE c:\LACTO2020\LX6\REG      IN 1
USE C:\LACTO2020\LX6\KEVENTO  IN 2 ALIAS KEV EXCLUSIVE

SELECT KEV
INDEX ON ID TAG ID

*SELECT KEV
*ZAP
*INDEX ON ID TAG ID
* -------------------------------

SELECT REG
SET ORDER TO 1

* Agrega los Nacimientos o Partos
* -------------------------------
SCAN
	IF NP=0
		SELECT KEV
		APPEND BLANK
		REPLACE KEV.ID WITH REG.ID,KEV.FECHA WITH REG.FNAC,CLAVE WITH 9,KEV.NP WITH REG.NP,KEV.ESTADO WITH 'NACIM'
	ENDIF	
	
	IF NP>0
		SELECT KEV
		APPEND BLANK
		REPLACE KEV.ID WITH REG.ID,KEV.FECHA WITH REG.FPAR,CLAVE WITH 1,KEV.NP WITH REG.NP,KEV.ESTADO WITH 'PARTO'	
	ENDIF

ENDSCAN


* Actualiza los datos de Inseminacion, Dx Gestacion o Aborto
* -----------------------------------------------------------
SELECT CALOR
SET FILTER TO CLAVE=4 OR CLAVE=5 OR CLAVE=6 OR CLAVE=9
SCAN
		SELECT KEV
		APPEND BLANK
		REPLACE KEV.ID WITH CALOR.ID,KEV.FECHA WITH CALOR.FECHA,KEV.NP WITH CALOR.NP
		
*	IF CALOR.CLAVE=5 OR CALOR.CLAVE=6 OR CALOR.CLAVE=9
		REPLACE KEV.CLAVE WITH ICASE(CALOR.CLAVE=4,2,CALOR.STAT='INSEM',3,CALOR.STAT='CARGA',4,CALOR.STAT='VACIA',5,CALOR.STAT='ABORT',6,'',0)
		REPLACE KEV.ESTADO WITH CALOR.STAT
*	ENDIF
SELECT CALOR
ENDSCAN


* Para Checar si esta Marcada PSV o Ya se dio de Baja
* ---------------------------------------------------
SELECT REG
SET ORDER TO 1
SCAN
	IF REG.FPSV#B
		SELECT KEV
		APPEND BLANK
		REPLACE KEV.ID WITH REG.ID,KEV.FECHA WITH REG.FPSV,KEV.CLAVE WITH 7,KEV.NP WITH REG.NP,KEV.ESTADO WITH 'PVENTA'
	ENDIF
	
	IF REG.FB2#B
	SELECT KEV	
	APPEND BLANK
		REPLACE KEV.ID WITH REG.ID,KEV.FECHA WITH REG.FB2,KEV.CLAVE WITH 8,KEV.NP WITH REG.NP,KEV.ESTADO WITH 'BAJA'
	ENDIF
ENDSCAN


SELECT KEV
GO TOP
SET ORDER TO 1
BROWSE
ENDIF
CLOSE TABLES
		