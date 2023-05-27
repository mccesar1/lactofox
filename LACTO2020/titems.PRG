USE rcamnode IN 1 ALIAS rcn
USE citems2  IN 2

SELECT rcamnode
SCAN
SELECT citems2
APPEND blank
	replace code1 WITH RCN.NOMCAMPO,CODE3 WITH rcn.Texto
ENDSCAN
SELECT citems2
BROWSE
