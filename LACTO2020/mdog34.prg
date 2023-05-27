PARAMETER nVER
DECLARE INTEGER GS_MHDog IN VFP30C STRING @
xDOG=Space(256)

* Escribe el MicroDog
* -------------------
ch1=CHR(3)
ch2=CHR(0)

xDOG=STUFF(xDOG,1,1,ch1)
xDOG=STUFF(xDOG,2,1,ch2)

ch3=CHR(0)
xDOG=STUFF(xDOG,3,1,ch3)
xDOG=STUFF(xDOG,4,1,ch2)

*DogAddr
xDOG=STUFF(xDOG,5,1,CHR(16))
xDOG=STUFF(xDOG,6,1,ch2)

*DogBytes
xDOG=STUFF(xDOG,7,1,CHR(4))
xDOG=STUFF(xDOG,8,1,ch2)

xDOG=STUFF(xDOG,9,1,CHR(778 % 256))
xDOG=STUFF(xDOG,10,1,CHR((778 / 256) % 256))
xDOG=STUFF(xDOG,11,1,CHR(((778 / 256) / 256) % 256))
xDOG=STUFF(xDOG,12,1,CHR(((778 / 256) / 256) / 256))

* Checa el Resultado
* ------------------
str1=nVER

xDOG=STUFF(xDOG,21,4, str1)

res=GS_MHDOG(@xDOG)

If (res=0) 
*		MessageBox("Las modificaciones se efectuaron con exito. !! ",0+48,"Lactofox - Aviso")
		xLLAVE5=Alltrim(nVER)
Else
	MessageBox("La llave no esta instalada o no es valida.  VERIFICAR !!",0+16,"Lactofox - Aviso")
	QUIT
EndIf
Clear DLLS
		