CLEA
CLEA ALL
CLOSE ALL
USE GNFERT In 1
Index on ID to GID

Select GNFERT
Set Order To GID

Go Top
browse
xID=ID
xFECHA=FECHA
Scan
If xID=ID And FECHA<xFECHA Or xID=ID And FECHA=xFECHA
*		Delete
EndIf

xID=ID
xFECHA=FECHA
EndScan

Go Top
Browse
PACK


Close DAta
		
	