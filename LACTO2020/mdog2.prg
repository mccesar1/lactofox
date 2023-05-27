Public xLLAVE0
DECLARE INTEGER GS_MHDog IN VFP30C STRING @
xDOG=Space(256)

* Lee el MicroDog
* ---------------
ch1=CHR(2)
ch2=CHR(0)

xDOG=STUFF(xDOG,1,1,ch1)
xDOG=STUFF(xDOG,2,1,ch2)

ch3=CHR(0)
xDOG=STUFF(xDOG,3,1,ch3)
xDOG=STUFF(xDOG,4,1,ch2)

*DogAdrr
xDOG=STUFF(xDOG,5,1,ch2)
xDOG=STUFF(xDOG,6,1,ch2)

*DogBytes
ch1=CHR(200)
xDOG=STUFF(xDOG,7,1,ch1)
xDOG=STUFF(xDOG,8,1,ch2)

xDOG=STUFF(xDOG,9,1,CHR(778 % 256))
xDOG=STUFF(xDOG,10,1,CHR((778 / 256) % 256))
xDOG=STUFF(xDOG,11,1,CHR(((778 / 256) / 256) % 256))
xDOG=STUFF(xDOG,12,1,CHR(((778 / 256) / 256) / 256))

* Checa el Resultado
* ------------------
res=GS_MHDOG(@xDOG)

If (res=0) 
	xLLAVE0=xDOG
	xLLAVE1=Alltrim(SubStr(xDOG,21,5))
	xLLAVE2=Alltrim(SubStr(xDOG,26,4))
	xLLAVE3=Alltrim(SubStr(xDOG,30,4))
	xLLAVE4=CTOD(Alltrim(SubStr(xDOG,101,10)))
Else
	MessageBox("La llave no esta instalada o no es valida.  VERIFICAR !!",0+16,"Lactofox - Aviso")
	QUIT
EndIf
Clear DLLS
		