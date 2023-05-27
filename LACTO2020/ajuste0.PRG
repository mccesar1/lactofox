Set delete off
Use REG   In 1
Use CALOR In 2

Select CALOR
Set Order To 3
	
Scan
@ 20,2 Say [ID ARETE ]+Str(CALOR.ID,5)
	Select REG
	Set Order TO 3
	Seek CALOR.ID

	If Not Found()
		Select CALOR
		Delete Next 1
	EndIf	
	
	Select CALOR
	Set Order To 3
EndScan

GO TOP
BROWSE
CLOSE DATABASES