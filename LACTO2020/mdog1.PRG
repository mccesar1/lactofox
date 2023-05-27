DECLARE INTEGER GS_MHDog IN VFP30C STRING @
xDOG=Space(256)

* Checa si esta el MDOG
* ---------------------
ch1=CHR(1)
ch2=CHR(0)

xDOG=STUFF(xDOG,1,1,ch1)
xDOG=STUFF(xDOG,2,1,ch2)

ch3=CHR(0)
xDOG=STUFF(xDOG,3,1,ch3)
xDOG=STUFF(xDOG,4,1,ch2)

* Busca el MicroDog
* -----------------
res=GS_MHDOG(@xDOG)

If res=0
	MessageBox("  El MicroDog fue leido con EXITO !!",0+32,"Lactofox - Aviso")
Else
	MessageBox("  La llave del sistema no esta instalada.  !!",0+16,"Lactofox - Aviso")
EndIf
Clear DLLS
		