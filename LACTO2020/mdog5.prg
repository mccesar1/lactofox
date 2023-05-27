DECLARE INTEGER GS_MHDog IN VFP30C STRING @
xDOG=Space(256)

* Lee el numero de SERIE
* ---------------------
ch1=CHR(5)
ch2=CHR(0)

xDOG=STUFF(xDOG,1,1,ch1)
xDOG=STUFF(xDOG,2,1,ch2)

ch3=CHR(0)
xDOG=STUFF(xDOG,3,1,ch3)
xDOG=STUFF(xDOG,4,1,ch2)

res=GS_MHDOG(@xDOG)


If (res=0) 
	_SERIE=Alltrim(Str(Asc(SubStr(xDOG,24,1))*256*256*256+Asc(SubStr(xDOG,23,1))*256*256+Asc(SubStr(xDOG,22,1))*256+Asc(SubStr(xDOG,21,1))))
Else
	MessageBox("La llave no esta instalada o no es valida.  VERIFICAR !!",0+16,"DAIRYFOX - Aviso")
	QUIT
EndIf
Clear DLLS
		