CLOSE DATABASES
SET SAFETY OFF
CREATE TABLE RDATOS (ID n(5),RFID c(15),NP n(2),CORR n(3))

USE c:\lacto2006\lx4\REG ORDER 9 IN 2 

SELECT REG
xRFID=REG.RFID
xID=REG.ID
xCORR=REG.CORR
xNP=REG.NP

GO TOP
SCAN
	IF REG.RFID=xRFID AND REG.ID#XID
		SELECT RDATOS
		APPEND BLANK
		REPLACE ID WITH XID,RFID WITH XRFID,CORR WITH xCORR,NP WITH xNP
		APPEND BLANK
		REPLACE ID WITH REG.ID,RFID WITH REG.RFID,CORR WITH REG.CORR,NP WITH REG.NP
		APPEND BLANK
		SELECT REG
	ENDIF
	xRFID=REG.RFID
	xID=REG.ID
ENDSCAN
SELECT RDATOS
BROWSE
CLOSE DATABASES



