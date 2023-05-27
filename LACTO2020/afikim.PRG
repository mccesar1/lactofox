
PROCEDURE cdatos2
** --------------
set path to &xlac
set talk off
b=ctod("")
sn="N"
	save screen to xx
	@ 4,2 clea to 18,77
	@22,1 clea to 23,78
	@9,4 say [NOTA :] COLO W+/&CZ
	@11,4 fill to 16,77 colo n/n
	@10,3 fill to 15,76 COLO SCHE 15	
	@11,4 say [ Esta opcion exportara los datos de las vacas del sistema LACTOFOX a un  ] COLO SCHE 15
	@12,4 say [ archivo llamado COW_UPDA.DAT en el directorio ]+xdro                     COLO SCHE 15
	@13,4 say [ Este archivo debera ser utilizado para actualizar la informacion en el  ] colo sche 15
	@14,4 say [ sistema AFIMILK version para Windows.                                   ] colo sche 15

	@22,24 say [EXPORTAR DATOS DE VACAS A AFIMILK] colo gr+/&cz
	@23,29 say [Ultima Fecha  ]
	@23,44 SAY dtoc(xfecha1) colo w+/&cz
	@23,67 say [Continuar] get sn pict '@M N,S' COLO n/w
read

if lastkey()=27 or sn="N" 
else
set century off
if file("REG.DBF")
		set date british
		create table afikim (id C(85))
		clos data
			if file("ARCHIVO.DBF")
				sele 3
				use ARCHIVO
			endi
		sele 2
		use afikim
		zap
		sele 1
		use reg order 1 alias reg
	*	set filt to np>0 and fb2=B .or. np>0 and fb2#b and fb2>date()-180
		
		set filt to np>0 and fb2=B .or. np>0 and fb2#b and fb2>date()-180 ;
		or np=0 and stat="INSEM" or np=0 and stat="CARGA"

		go top
		sp=space(1)
		nd=1
		@22,2 say [ID]
		@23,2 say [Total] 
		scan 
			xid=str(id)
			xide=ide
			xcorr=str(corr)
			xnp=str(np)
			xns=str(ns)
			
			xucal="00/00/00"
			xfpar="00/00/00"
			xfnac="00/00/00"
			xfsec="00/00/00"
			xabto="00/00/00"
			xucal2="00/00/00"
			
			
			if fsec=ctod("") and np>0
				xcod="1"
			else
				xcod="2"
				xfsec=fx(fsec)
			endi
			
			
			
			if ucal#B
				xucal=fx(ucal)
			endi
	
			if ns>0 and stat=[INSEM] or ns>0 and stat=[CARGA]
				xucal2=fx(ucal)
			endi
	
			if fpar#B
				xfpar=fx(fpar)
			endi
			if fnac#B
				xfnac=fx(fnac)
			endi
			if abto#B
				xabto=fx(abto)
			endi
	
			** Si es Vaquilla Inseminada o Cargada
			** -----------------------------------
			if np=0
				xcod="3"
				xfsec="00/00/00"
			endi	

			@22,7 say str(id,5) colo n/w
			@23,7 say nd pict '#####' colo w+/&cz
			sele 2
	if REG.fb2=B 
		appe blank
		repl id with "+"+sp+right("0000"+ltrim(rtrim(xid)),4);
						+sp+right("0000"+ltrim(rtrim(xide)),4);
    		            +sp+right("00"+ltrim(rtrim(xcorr)),2);
    		            +sp+xucal;
        		        +sp+right("00"+ltrim(rtrim(xnp)),2);
        		        +sp+xfpar;
            		    +sp+xucal2;
            		    +sp+xcod;
            		    +sp+right("00"+ltrim(rtrim(xns)),2);
            		    +sp+xfnac;
            		    +sp+xfsec;
            		    +sp+xabto
           do case
           		case REG.stat="CARGA"
           			repl id with rtrim(id)+sp+"+P"

				otherwise
					repl id with rtrim(id)+sp+"-P"
           endcase			  		    
  
  		   if REG.psv="S"
  				repl id with rtrim(id)+sp+"+E"
  		   endi	

			if file("ARCHIVO.DBF")
					sele 3
					go top
					locate for alltrim(id_num)=alltrim(xid)
						if found()
						sele 2
						repl id with rtrim(id)+sp+"+Z"
						endi					         		 
			else
			endi
			
	else
		appe blank
		repl id with "-"+sp+right("0000"+ltrim(rtrim(xid)),4)+sp+chr(13)
		repl id with rtrim(id)
	endi

		sele 2
		repl id with rtrim(id)+sp+chr(13)

			sele 1
			nd=nd+1
		endscan
	sele 2
	copy to xdro+"\"+"cow_upda.dat" fields id delimited with blank
use ifase
repl FECHA1 with date()
xfecha1=FECHA1
clos data
eras afikim.dbf
set date ansi
set century on
else
	??chr(7)
	wait wind [ Archivo no Existente ] timeout 1
	??chr(7)
	wait wind [ Revisar Directorios de Datos ] timeout 2
endi	
endi

restore screen from xx
clos all
if file("ARCHIVO.DBF")
eras ARCHIVO.DBF
endi
return
