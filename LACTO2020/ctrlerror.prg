* Control de Errores
* ------------------
PARAMETER xERROR,xMensaje,xLine,xProgram
Do Case
	Case xERROR=1
		xCHOICE=MessageBox("Error # "+Alltrim(Str(xERROR,4))+"  "+Message()+Chr(13)+Chr(13)+;
		" Se debera de utilizar la opcion ACTUALIZAR VERSION del Menu de HERRAMIENTAS. !! ",;
		1+16+256,"DAIRYFOX - Aviso de Error")
	
		If xCHOICE=1
			RETRY
		Else	
			RETURN TO MASTER
		EndIF
		
	Case xERROR=4
	
	Case xERROR=15
		xCHOICE=MessageBox("Error # "+Alltrim(Str(xERROR,4))+"  "+xMensaje+Chr(13)+CHR(13)+;
		" Favor de reportarlo via correo electronico a [agroplus01@prodigy.net.mx]",1+16+256,;
		" DAIRYFOX - Aviso de Error")

	Case xERROR=26
		xCHOICE=MessageBox("El Archivo No tiene un Indice establecido, Verificar. !!",1+48+256,"DAIRYFOX - Aviso de Error")
		If xCHOICE=1
			RETURN TO MASTER
		EndIF

	Case xERROR=39
		xCHOICE=MessageBox("Desbordamiento Numerico, Verificar los Parametros por ID. !!",1+48+256,"DAIRYFOX - Aviso de Error")
		If xCHOICE=1
			RETURN TO MASTER
		EndIF
		
	Case xERROR=56
		xCHOICE=MessageBox("No hay espacio suficiente en el disco para crear"+CHR(13)+;
							 "los archivos de datos. !!",0+48+256,"DAIRYFOX - Aviso de Error")
		If xCHOICE=1
			RETURN TO MASTER
		EndIF

	Case xERROR=109
		xCHOICE=MessageBox("* Esta ID esta siendo accesada por otro usuario.  !!",1+48+256," DAIRYFOX - Aviso")
		If xCHOICE=1
			RETRY
		Else
			RETURN
		EndIf		

	Case xERROR=110
		xCHOICE=MessageBox("El Archivo debe abrirse en modo Exclusivo.  !!"+DBF(),1+48+256," DAIRYFOX - Aviso")
		If xCHOICE=1
			RETRY
		Else
			RETURN
		EndIf		

	Case xERROR=125
		xCHOICE=MessageBox("La impresora no funciona o esta desconectada .!!"+DBF(),1+48+256," DAIRYFOX - Aviso")
		If xCHOICE=1
			RETRY
		Else
			RETURN
		EndIf		

	Case xERROR=202
		xCHOICE=MessageBox("El Directorio Seleccionado No Es Valido .!!"+DBF(),1+16+256," DAIRYFOX - Aviso")
		If xCHOICE=1
			RETRY
		Else
			RETURN 
		EndIf		

	Case xERROR=1002
		xCHOICE=MessageBox("Fallo en la operacion de Entrada/Salida.  !!"+DBF(),1+16+256," DAIRYFOX - Aviso de Error")
		If xCHOICE=1
			RETRY
		Else
			RETURN
		EndIf		

	Case xERROR=1705
		MessageBox("El archivo de datos esta siendo utilizado por otro usuario. !!"+DBF(),0+48," DAIRYFOX - Aviso")
		Return To MASTER

Case xERROR=1961
		xCHOICE=MessageBox("El Directorio de Datos a Crear Ya Existe. !! "+DBF(),0+16+256," DAIRYFOX - Aviso")
		If xCHOICE=1
			RETURN
		EndIf		

	OtherWise
	*	xCHOICE=MessageBox("* Error # "+Alltrim(Str(xERROR,4))+"  "+xMensaje+DBF()+'  '+Chr(13)+Chr(13)+;
		"Linea : "+Alltrim(Str(xLine,4))+"  "+xProgram,1+16+256," DAIRYFOX - Aviso de Error")
EndCase

* Respuesta
* ---------
If xCHOICE=1
	RETRY
Else
	RETURN TO MASTER
EndIf		

* Fin

