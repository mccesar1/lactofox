Use CALOR In 1
Use FERTX In 2

*xFilter="NP>0"
*Select ID,FECHA2,SER,STAT From CALOR Where YEAR(fecha2)=xan And ser>0 And fecha2<=xfecha And fecha2#B and (Clave=5 Or Clave=6) And &xFilter Into Cursor xDATOS

Select CALOR
Set Filter To CLAVE=5 Or CLAVE=6


Scan
*?ID
EndScan
xn=reccount()
?xn
Close DataBases
