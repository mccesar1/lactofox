Create Table ALABCCS (ID n(5),NP n(2),DEL n(5),KGS n(5,1),LOTE n(3),GRASA n(5,2),PROTEINA n(5,2),LACTOSA n(5,2),SNG n(5,2),NU n(4,1),CCS n(6),LINEAL n(1))

APPEND FROM laboratorio XLS


Select ALABCCS
		GO TOP
		Delete Next 7
		*Set Relation To ID Into REG


BROWSE
CLOSE DATABASES

		