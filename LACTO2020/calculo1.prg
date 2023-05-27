Do Case
	Case nREP=41
	x1=25
	x2=12
	Create Cursor REPORTE (concepto c(30),col1 n(5),NP n(1))
	Append Blank
	Replace CONCEPTO With "PARAMETROS"

	Append Blank
	Replace CONCEPTO With "Numero de Vacas";
		Col1 With x1;
		Np With 1

	Append Blank
	Replace CONCEPTO With "Vacas Confirmadas Preñadas";
		Col1 With x2;
		Np With 2				

	GO TOP	




EndCase