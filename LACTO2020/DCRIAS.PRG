USE c:\lacto2006\lx3\reg ORDER 2 IN 1

SCAN
	DO case
		CASE sexc="HE"
			Replace SX1 WITH "H"

		CASE sexc="MA"
			replace SX1 WITH "M"

		CASE sexc="HH"
			replace sx1 WITH "H",sx2 WITH  "H"

		CASE sexc="HM"
			replace sx1 WITH "H",sx2 WITH  "M"
		

		CASE sexc="MM"
		
			replace sx1 WITH "M",sx2 WITH  "M"


	ENDCASE
ENDSCAN
	
	

