SET DEFAULT TO c:\lacto2006\lx6
USE REG Exclusive
If FLOCK()
	.PBAR1.Visible=.T.
	.PBAR1.Value=.PBAR1.Value+x
	.Text1.Value=DBF()
	Delete Tag All Of REG.CDX
	inde on ID tag REG 
	inde on ID tag VAC for NP>0
	inde on ID tag CRI for NP=0

	inde on str(CORR)+str(ID) tag CORV for NP>0
	inde on str(CORR)+str(ID) tag CORC for NP=0
	inde on prm tag PRM for NP>0
	inde on str((date()-fpar)-(date()-fsec),5)+str(ID) tag DEL for NP>0
	inde on IDE tag IDE for not empty(IDE)
	INDEX on RFID TAG RFID FOR NOT EMPTY(RFID)	
UNLOCK
Else
	MessageBox("El archivo de datos esta siendo usado por otro usuario. !!",0+32,"Lactofox - Aviso")
EndIf