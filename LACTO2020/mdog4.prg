DECLARE INTEGER GS_MHDog IN VFP30C STRING @
xDOG=Space(256)

* Checa si esta el MDOG
* ---------------------
ch1=CHR(4)
ch2=CHR(0)

xDOG=STUFF(xDOG,1,1,ch1)
xDOG=STUFF(xDOG,2,1,ch2)

ch3=CHR(0)
xDOG=STUFF(xDOG,3,1,ch3)
xDOG=STUFF(xDOG,4,1,ch2)

xDOG=STUFF(xDOG,5,1,ch2)
xDOG=STUFF(xDOG,6,1,ch2)

ch1=CHR(10)
xDOG=STUFF(xDOG,7,1,ch1)
xDOG=STUFF(xDOG,8,1,ch2)

xDOG=STUFF(xDOG,9,1,CHR(778 % 256))
xDOG=STUFF(xDOG,10,1,CHR((778 / 256) % 256))
xDOG=STUFF(xDOG,11,1,CHR(((778 / 256) / 256) % 256))
xDOG=STUFF(xDOG,12,1,CHR(((778 / 256) / 256) / 256))
Str1="MMARIANARV"

xDOG=STUFF(xDOG,19,20,str1)

* Convierte a VALOR
* -----------------
res=GS_MHDOG(@xDOG)
xCLAVE=Alltrim(Str(Asc(SubStr(xDOG,16,1))*256*256*256+Asc(SubStr(xDOG,15,1))*256*256+Asc(SubStr(xDOG,14,1))*256+Asc(SubStr(xDOG,13,1))))

If (res=0) And xCLAVE='2595557190'
	* Continua con el Programa	
Else
	MessageBox("La llave no esta instalada o no es valida.  VERIFICAR !!",0+16,"Lactofox - Aviso")
	QUIT
EndIf
Clear DLLS
		