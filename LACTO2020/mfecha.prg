Do Case
	Case BAR = 1
		Set Date AMERICAN
	Case BAR=2
		Set Date German
	Case BAR=3
		Set Date ANSI
Endcase				
_SCREEN.Caption="  LACTOFOX  Versión 3.0    "+DTOC(Date())