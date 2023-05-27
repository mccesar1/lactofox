* ANALISIS ANUAL DE ABORTOS
* ------------------------------
PROCEDURE RV10213
Dimension xs(30),ts(30),mes(12),xg(100),tg(100),xm(100),xl(100),tl(100)
xan=0
xcar=0
xa=0
xl=0
xn=0
xs=0
ts=0
xg=0
tg=0
tl=0
mes=1
xfilt="VT"

** Calculo de Promedio de Vientres Gestantes
** -----------------------------------------
Select REG
Set Order To 2
Calculate CNT() To xn For FB2=B

Select * From ABORTOS Where YEAR(Fecha)=xan and NP>0 Into Cursor ABORTO

x=0
SCAN
xa=xa+1
** Todas las Lactancias
** -------------------
		do case 
			case month(fecha)=1 
					xs(month(fecha))=xs(month(fecha))+1
                    ts(month(fecha))=(xs(month(fecha))/xx)*100
					xm=1
			case month(fecha)=2 
					xs(month(fecha))=xs(month(fecha))+1
                    ts(month(fecha))=(xs(month(fecha))/xx)*100
                    xm=8
			case month(fecha)=3
					xs(month(fecha))=xs(month(fecha))+1
                    ts(month(fecha))=(xs(month(fecha))/xx)*100
                    xm=15
			case month(fecha)=4
					xs(month(fecha))=xs(month(fecha))+1
                    ts(month(fecha))=(xs(month(fecha))/xx)*100
                    xm=22
			case month(fecha)=5
					xs(month(fecha))=xs(month(fecha))+1
                    ts(month(fecha))=(xs(month(fecha))/xx)*100
                    xm=29
			case month(fecha)=6
					xs(month(fecha))=xs(month(fecha))+1
                    ts(month(fecha))=(xs(month(fecha))/xx)*100
                    xm=36
			case month(fecha)=7
					xs(month(fecha))=xs(month(fecha))+1
                    ts(month(fecha))=(xs(month(fecha))/xx)*100
                    xm=43
			case month(fecha)=8
					xs(month(fecha))=xs(month(fecha))+1
                    ts(month(fecha))=(xs(month(fecha))/xx)*100
                    xm=50 
			case month(fecha)=9
					xs(month(fecha))=xs(month(fecha))+1
                    ts(month(fecha))=(xs(month(fecha))/xx)*100
                    xm=57
			case month(fecha)=10 
					xs(month(fecha))=xs(month(fecha))+1
                    ts(month(fecha))=(xs(month(fecha))/xx)*100
                    xm=64
			case month(fecha)=11
					xs(month(fecha))=xs(month(fecha))+1
                    ts(month(fecha))=(xs(month(fecha))/xx)*100
                    xm=71
			case month(fecha)=12
					xs(month(fecha))=xs(month(fecha))+1
                    ts(month(fecha))=(xs(month(fecha))/xx)*100
					xm=78	
		endcase
		do XDGA with xm				
		
		** Total por Dias de Gestacion
		** ---------------------------
		do case 
		   case dga>=45 and dga<=90 
					xs(14)=xs(14)+1
                    ts(14)=(xs(14)/xx)*100

			case dga>90 and dga<=120 
					xs(15)=xs(15)+1
                    ts(15)=(xs(15)/xx)*100

			case dga>120 and dga<=150
					xs(16)=xs(16)+1
                    ts(16)=(xs(16)/xx)*100

			case dga>150 and dga<=180
					xs(17)=xs(17)+1
                    ts(17)=(xs(17)/xx)*100

			case dga>180 and dga>=210
					xs(18)=xs(18)+1
                    ts(18)=(xs(18)/xx)*100

			case dga>210 and dga<=240
					xs(19)=xs(19)+1
                    ts(19)=(xs(19)/xx)*100

			case dga>240
					xs(20)=xs(20)+1
                    ts(20)=(xs(20)/xx)*100
		endcase
do DLAC

ENDSCAN
xs(13)=xa
ts(13)=(xa/xx)*100

** Pantalla Numerica
** -----------------
defi wind calc1 from 3,0 to 20,77 shadow colo sche 16
acti wind calc1
@ 0,1 say [D¡as de] colo n/w
@ 1,1 say [Gestaci¢n   45-90  91-120  121-150  151-180  181-210  211-240  240+  Total]
@ 1,1 say [Gestaci¢n] colo n/w
@ 2,1 to 2,74 
@ 3,1 say [Enero]
@ 4,1 say [Febrero]
@ 5,1 say [Marzo]
@ 6,1 say [Abril]
@ 7,1 say [Mayo]
@ 8,1 say [Junio]
@ 9,1 say [Julio]
@ 10,1 say [Agosto]
@ 11,1 say [Septiembre]
@ 12,1 say [Octubre]
@ 13,1 say [Noviembre]
@ 14,1 say [Diciembre]
@ 15,1 say [Total #]

row=3
w=1
do while row<15
@ row,12 say str(xg(w),5)+'   '+str(xg(w+1),5)+'    '+str(xg(w+2),5)+'    '+str(xg(w+3),5)+'    '+str(xg(w+4),5)+'    '+str(xg(w+5),5)+'  '+str(xg(w+6),5) colo n/w
row=row+1
w=w+7
enddo

@15,12 say str(xs(14),5)+'   '+str(xs(15),5)+'    '+str(xs(16),5)+'    '+str(xs(17),5)+'    '+str(xs(18),5)+'    '+str(xs(19),5)+'  '+str(xs(20),5) colo r/w

@ 3,71 say xs(1) pict'###' colo r/w
@ 4,71 say xs(2) pict'###' colo r/w
@ 5,71 say xs(3) pict'###' colo r/w
@ 6,71 say xs(4) pict'###' colo r/w
@ 7,71 say xs(5) pict'###' colo r/w
@ 8,71 say xs(6) pict'###' colo r/w
@ 9,71 say xs(7) pict'###' colo r/w
@ 10,71 say xs(8) pict'###' colo r/w
@ 11,71 say xs(9) pict'###' colo r/w
@ 12,71 say xs(10) pict'###' colo r/w
@ 13,71 say xs(11) pict'###' colo r/w
@ 14,71 say xs(12) pict'###' colo r/w
@ 15,71 say xs(13) pict'###' colo w+/w

acti screen
@ 24,66 clea to 24,79
@ 24,70 SAY [Enter] COLO N/W
@ 24,76 SAY chr(16) colo w+/&cz
do while inkey()#13
endd

** Pantalla por Dias de Gestacion (Porciento)
** ------------------------------------------
acti wind calc1
@ 3,12 clea to 15,73
@15,1 say [Total %]

row=3
w=1
do while row<15
@ row,12 say str(tg(w),5,1)+'   '+str(tg(w+1),5,1)+'    '+str(tg(w+2),5,1)+'    '+str(tg(w+3),5,1)+'    '+str(tg(w+4),5,1)+'    '+str(tg(w+5),5,1)+'  '+str(tg(w+6),5,1) colo n/w
row=row+1
w=w+7
enddo


@15,12 say str(ts(14),5,1)+'   '+str(ts(15),5,1)+'    '+str(ts(16),5,1)+'    '+str(ts(17),5,1)+'    '+str(ts(18),5,1)+'    '+str(ts(19),5,1)+'  '+str(ts(20),5,1) colo r/w

@ 3,70 say ts(1) pict'##.#' colo r/w
@ 4,70 say ts(2) pict'##.#' colo r/w
@ 5,70 say ts(3) pict'##.#' colo r/w
@ 6,70 say ts(4) pict'##.#' colo r/w
@ 7,70 say ts(5) pict'##.#' colo r/w
@ 8,70 say ts(6) pict'##.#' colo r/w
@ 9,70 say ts(7) pict'##.#' colo r/w
@ 10,70 say ts(8) pict'##.#' colo r/w
@ 11,70 say ts(9) pict'##.#' colo r/w
@ 12,70 say ts(10) pict'##.#' colo r/w
@ 13,70 say ts(11) pict'##.#' colo r/w
@ 14,70 say ts(12) pict'##.#' colo r/w
@ 15,70 say ts(13) pict'##.#' colo w+/w

acti screen
@ 24,66 clea to 24,79
@ 24,70 SAY [Enter] COLO N/W
@ 24,76 SAY chr(16) colo w+/&cz
do while inkey()#13
endd

** Pantalla por Lactancia (Numerica)
** ----------------------------------
acti wind calc1
@ 0,0 clea to 15,74
@ 1,1 say [x Lactancia          1                2                3+            Total ] colo w+/w
@ 1,1 say [x Lactancia] colo n/w
@ 2,1 to 2,74 
@ 3,1 say [Enero]
@ 4,1 say [Febrero]
@ 5,1 say [Marzo]
@ 6,1 say [Abril]
@ 7,1 say [Mayo]
@ 8,1 say [Junio]
@ 9,1 say [Julio]
@ 10,1 say [Agosto]
@ 11,1 say [Septiembre]
@ 12,1 say [Octubre]
@ 13,1 say [Noviembre]
@ 14,1 say [Diciembre]
@ 15,1 say [Total #]

row=3
w=1
do while row<15
@ row,18 say str(xl(w),5)+'            '+str(xl(w+12),5)+'            '+str(xl(w+24),5) colo n/w
row=row+1
w=w+1
enddo

@15,18 say str(xl(49),5)+'            '+str(xl(50),5)+'            '+str(xl(51),5) colo r/w

@ 3,71 say xs(1) pict'##' colo r/w
@ 4,71 say xs(2) pict'##' colo r/w
@ 5,71 say xs(3) pict'##' colo r/w
@ 6,71 say xs(4) pict'##' colo r/w
@ 7,71 say xs(5) pict'##' colo r/w
@ 8,71 say xs(6) pict'##' colo r/w
@ 9,71 say xs(7) pict'##' colo r/w
@ 10,71 say xs(8) pict'##' colo r/w
@ 11,71 say xs(9) pict'##' colo r/w
@ 12,71 say xs(10) pict'##' colo r/w
@ 13,71 say xs(11) pict'##' colo r/w
@ 14,71 say xs(12) pict'##' colo r/w
@ 15,71 say xs(13) pict'##' colo w+/w

acti screen
@ 24,66 clea to 24,79
@ 24,70 SAY [Enter] COLO N/W
@ 24,76 SAY chr(16) colo w+/&cz
do while inkey()#13
endd

** Pantalla por Lactancia (Porciento)
** ----------------------------------
acti wind calc1
@ 3,12 clea to 15,73
@15,1 say [Total %]
row=3
w=1
do while row<15
@ row,18 say str(tl(w),5,1)+'            '+str(tl(w+12),5,1)+'            '+str(tl(w+24),5,1) colo n/w
row=row+1
w=w+1
enddo

@15,18 say str(tl(49),5,1)+'            '+str(tl(50),5,1)+'            '+str(tl(51),5,1) colo r/w

@ 3,70 say ts(1) pict'##.#' colo r/w
@ 4,70 say ts(2) pict'##.#' colo r/w
@ 5,70 say ts(3) pict'##.#' colo r/w
@ 6,70 say ts(4) pict'##.#' colo r/w
@ 7,70 say ts(5) pict'##.#' colo r/w
@ 8,70 say ts(6) pict'##.#' colo r/w
@ 9,70 say ts(7) pict'##.#' colo r/w
@ 10,70 say ts(8) pict'##.#' colo r/w
@ 11,70 say ts(9) pict'##.#' colo r/w
@ 12,70 say ts(10) pict'##.#' colo r/w
@ 13,70 say ts(11) pict'##.#' colo r/w
@ 14,70 say ts(12) pict'##.#' colo r/w
@ 15,70 say ts(13) pict'##.#' colo w+/w

acti screen
??chr(7)
@ 24,66 clea to 24,79
@ 24,70 SAY [Enter] COLO N/W
@ 24,76 SAY chr(4) colo w+/&cz
do while inkey()#13
endd

** Rutina para Impresion
** --------------------*
sn='N'
@ 24,67 SAY [Imprimir] GET SN PICT '@M S,N' colo n/w 
READ
IF SN='N' .OR. LASTKEY()=27
ELSE
IF SN='S' .AND. SYS(13)='READY'
SET PRIN ON
SET CONS OFF
PAG=1
meses=[EneFebMarAbrMayJunJulAgoSepOctNovDic]

NTIT=[ANALISIS ANUAL DE ABORTOS]
NTIT2=""+str(xan,4)
?'  Rancho   : '+Q36+'   '+nom 
?'  Area     : VIENTRES'
?'  Reporte  : '+NTIT
?'  Fecha    : '+DTOC(DATE())+'  Hora : '+TIME()
?'  Pagina   : '+STR(PAG,2,0)+space(70)+[DAIRYFOX]
?'  Periodo  : '+NTIT2
?'  '+REPLI(CHR(254),94)
?'  D¡as de'
?'  Gestaci¢n      45-90   91-120  121-150  151-180  181-210  211-240     240+    Total'
?'  '+REPLI('-',94)
x=1
w=1
mes=1
do while mes<13
?'  '+substr(meses,((mes(x)-1)*3)+1,3)+'           '+str(xg(w),5)+'    '+str(xg(w+1),5)+'    '+str(xg(w+2),5)+'    '+str(xg(w+3),5)+'    '+str(xg(w+4),5)+'    '+str(xg(w+5),5)+'    '+str(xg(w+6),5)+'    '+str(xs(x),5)
x=x+1
w=w+7
mes=mes+1
endd
?
?'  Total #       '+str(xs(14),5)+'    '+str(xs(15),5)+'    '+str(xs(16),5)+'    '+str(xs(17),5)+'    '+str(xs(18),5)+'    '+str(xs(19),5)+'    '+str(xs(20),5)+'    '+str(xs(13),5)
?'  '+REPLI('-',94)
?
?
?'  D¡as de'
?'  Gestaci¢n      45-90   91-120  121-150  151-180  181-210  211-240     240+    Total'
?'  '+REPLI('-',94)
x=1
w=1
mes=1
do while mes<13
?'  '+substr(meses,((mes(x)-1)*3)+1,3)+'           '+str(tg(w),5,1)+'    '+str(tg(w+1),5,1)+'    '+str(tg(w+2),5,1)+'    '+str(tg(w+3),5,1)+'    '+str(tg(w+4),5,1)+'    '+str(tg(w+5),5,1)+'    '+str(tg(w+6),5,1)+'    '+str(ts(x),5,1)
x=x+1
w=w+7
mes=mes+1
endd
?
?'  Total %       '+str(ts(14),5,1)+'    '+str(ts(15),5,1)+'    '+str(ts(16),5,1)+'    '+str(ts(17),5,1)+'    '+str(ts(18),5,1)+'    '+str(ts(19),5,1)+'    '+str(ts(20),5,1)+'    '+str(ts(13),5,1)
?
?'  '+REPLI(CHR(254),94)
?
?'  C lculo sobre '+xtit+'    Promedio :'+str(xx,4)
EJECT

** Imprime por Lactancia
** ---------------------
pag=2
NTIT=[ANALISIS ANUAL DE ABORTOS]
NTIT2=""+str(xan,4)
?'  Rancho   : '+Q36+'   '+nom 
?'  Area     : VIENTRES'
?'  Reporte  : '+NTIT
?'  Fecha    : '+DTOC(DATE())+'  Hora : '+TIME()
?'  Pagina   : '+STR(PAG,2,0)+space(70)+[DAIRYFOX]
?'  Periodo  : '+NTIT2
?'  '+REPLI(CHR(254),94)
?
?'  x Lactancia          1                2                3+            Total '
?'  '+REPLI('-',94)
x=1
w=1
mes=1
do while mes<13
?'  '+substr(meses,((mes(x)-1)*3)+1,3)+'              '+str(xl(w),5)+'            '+str(xl(w+12),5)+'            '+str(xl(w+24),5)+'            '+str(xs(x),5)
x=x+1
w=w+1
mes=mes+1
endd
?
?'  Total #          '+str(xl(49),5)+'            '+str(xl(50),5)+'            '+str(xl(51),5)+'            '+str(xs(13),5)
?'  '+REPLI('-',94)
?
?

?'  x Lactancia          1                2                3+            Total '
?'  '+REPLI('-',94)
x=1
w=1
mes=1
do while mes<13
?'  '+substr(meses,((mes(x)-1)*3)+1,3)+'              '+str(tl(w),5,1)+'            '+str(tl(w+12),5,1)+'            '+str(tl(w+24),5,1)+'            '+str(ts(x),5,1)
x=x+1
w=w+1
mes=mes+1
endd
?
?'  Total %          '+str(tl(49),5,1)+'            '+str(tl(50),5,1)+'            '+str(tl(51),5,1)+'            '+str(ts(13),5,1)

?
?'  '+REPLI(CHR(254),94)
?
?'  C lculo sobre '+xtit+'    Promedio :'+str(xx,4)
** Termina Impresion
** -----------------

EJECT
SET PRIN OFF
SET CONS ON
ELSE
??CHR(7)
WAIT WIND [ IMPRESORA APAGADA O DESCONECTADA ] TIMEOUT 2
ENDI
ENDI
CLOS DATA
rele wind CALC1
set decimals to 2
RETU


PROC XDGA
parameter xm
		do case 
			case dga>=45 and dga<=90 
					xg(xm)=xg(xm)+1
                    tg(xm)=(xg(xm)/xx)*100

			case dga>90 and dga<=120 
					xg(xm+1)=xg(xm+1)+1
                    tg(xm+1)=(xg(xm+1)/xx)*100

			case dga>120 and dga<=150
					xg(xm+2)=xg(xm+2)+1
                    tg(xm+2)=(xg(xm+2)/xx)*100

			case dga>150 and dga<=180
					xg(xm+3)=xg(xm+3)+1
                    tg(xm+3)=(xg(xm+3)/xx)*100

			case dga>180 and dga>=210
					xg(xm+4)=xg(xm+4)+1
                    tg(xm+4)=(xg(xm+4)/xx)*100

			case dga>210 and dga<=240
					xg(xm+5)=xg(xm+5)+1
                    tg(xm+5)=(xg(xm+5)/xx)*100

			case dga>240
					xg(xm+6)=xg(xm+6)+1
                    tg(xm+6)=(xg(xm+6)/xx)*100
		endcase
return

PROCEDURE DLAC
DO CASE
	CASE np=1
		do case 
			case month(fecha)=1 
					xl(1)=xl(1)+1
                    tl(1)=(xl(1)/xx)*100

			case month(fecha)=2 
					xl(2)=xl(2)+1
                    tl(2)=(xl(2)/xx)*100

			case month(fecha)=3
					xl(3)=xl(3)+1
                    tl(3)=(xl(3)/xx)*100

			case month(fecha)=4
					xl(4)=xl(4)+1
                    tl(4)=(xl(4)/xx)*100

			case month(fecha)=5
					xl(5)=xl(5)+1
                    tl(5)=(xl(5)/xx)*100

			case month(fecha)=6
					xl(6)=xl(6)+1
                    tl(6)=(xl(6)/xx)*100

			case month(fecha)=7
					xl(7)=xl(7)+1
                    tl(7)=(xl(7)/xx)*100

			case month(fecha)=8
					xl(8)=xl(8)+1
                    tl(8)=(xl(8)/xx)*100

			case month(fecha)=9
					xl(9)=xl(9)+1
                    tl(9)=(xl(9)/xx)*100

			case month(fecha)=10 
					xl(10)=xl(10)+1
                    tl(10)=(xl(10)/xx)*100

			case month(fecha)=11
					xl(11)=xl(11)+1
                    tl(11)=(xl(11)/xx)*100

			case month(fecha)=12
					xl(12)=xl(12)+1
                    tl(12)=(xl(12)/xx)*100

		endcase

	CASE np=2
		do case 
			case month(fecha)=1 
					xl(13)=xl(13)+1
                    tl(13)=(xl(13)/xx)*100

			case month(fecha)=2 
					xl(14)=xl(14)+1
                    tl(14)=(xl(14)/xx)*100

			case month(fecha)=3
					xl(15)=xl(15)+1
                    tl(15)=(xl(15)/xx)*100

			case month(fecha)=4
					xl(16)=xl(16)+1
                    tl(16)=(xl(16)/xx)*100

			case month(fecha)=5
					xl(17)=xl(17)+1
                    tl(17)=(xl(17)/xx)*100

			case month(fecha)=6
					xl(18)=xl(18)+1
                    tl(18)=(xl(18)/xx)*100

			case month(fecha)=7
					xl(19)=xl(19)+1
                    tl(19)=(xl(19)/xx)*100

			case month(fecha)=8
					xl(20)=xl(20)+1
                    tl(20)=(xl(20)/xx)*100

			case month(fecha)=9
					xl(21)=xl(21)+1
                    tl(21)=(xl(21)/xx)*100

			case month(fecha)=10 
					xl(22)=xl(22)+1
                    tl(22)=(xl(22)/xx)*100

			case month(fecha)=11
					xl(23)=xl(23)+1
                    tl(23)=(xl(23)/xx)*100

			case month(fecha)=12
					xl(24)=xl(24)+1
                    tl(24)=(xl(24)/xx)*100

		endcase

	CASE np>=3
		do case 
			case month(fecha)=1 
					xl(25)=xl(25)+1
                    tl(25)=(xl(25)/xx)*100

			case month(fecha)=2 
					xl(26)=xl(26)+1
                    tl(26)=(xl(26)/xx)*100

			case month(fecha)=3
					xl(27)=xl(27)+1
                    tl(27)=(xl(27)/xx)*100

			case month(fecha)=4
					xl(28)=xl(28)+1
                    tl(28)=(xl(28)/xx)*100

			case month(fecha)=5
					xl(29)=xl(29)+1
                    tl(29)=(xl(29)/xx)*100

			case month(fecha)=6
					xl(30)=xl(30)+1
                    tl(30)=(xl(30)/xx)*100

			case month(fecha)=7
					xl(31)=xl(31)+1
                    tl(31)=(xl(31)/xx)*100

			case month(fecha)=8
					xl(32)=xl(32)+1
                    tl(32)=(xl(32)/xx)*100

			case month(fecha)=9
					xl(33)=xl(33)+1
                    tl(33)=(xl(33)/xx)*100

			case month(fecha)=10 
					xl(34)=xl(34)+1
                    tl(34)=(xl(34)/xx)*100

			case month(fecha)=11
					xl(35)=xl(35)+1
                    tl(35)=(xl(35)/xx)*100

			case month(fecha)=12
					xl(36)=xl(36)+1
                    tl(36)=(xl(36)/xx)*100

		endcase
ENDCASE

** Total por Lactancia
** --------------------
do case 
	case np=1
		xl(49)=xl(49)+1
		tl(49)=(xl(49)/xx)*100
	case np=2
		xl(50)=xl(50)+1
		tl(50)=(xl(50)/xx)*100
	case np>=3
		xl(51)=xl(51)+1
		tl(51)=(xl(51)/xx)*100
endcase	
tl(52)=tl(49)+tl(50)+tl(51)
return
