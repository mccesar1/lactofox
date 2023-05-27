use F:\CALOR Order 2 In 2 Exclusive
use F:\REG   Order 2 in 1 Exclusive

Set Relation To ID Into CALOR

Go Top
Scan
	Select CALOR
	Seek REG.ID
	If Found()
	Scan While REG.ID=CALOR.ID
		If CALOR.NP<REG.NP Or CALOR.FECHA<REG.FPAR
			Delete
		Endif	
	EndScan
	EndIf
	Select REG
EndScan
Set Exclusive On
Select CALOR
Pack
Browse

Close Databases
