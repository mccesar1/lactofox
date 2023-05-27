	use REG order 2
	seek XID
	dele while ID=xid
	
	USE LACTS order 1 exclusive
	seek XID
	dele while ID=XID

	USE PROD order 1 exclusive
	seek XID
	dele while ID=XID
	
	USE MAST order 1 exclusive
	seek XID
	dele while ID=XID
	
	USE BST order 1 exclusive
	seek XID
	dele while ID=XID
	
	USE CCS order 1 exclusive
	seek XID
	dele while ID=XID
	
	USE CLIN order 2 exclusive
	seek XID
	dele while ID=XID
	
	USE CALOR order 2 exclusive
	seek XID
	dele while ID=XID
	
	USE VACUNAS order 2 exclusive
	seek XID
	dele while ID=XID
	
	USE PTB order 2 exclusive
	seek XID
	dele while ID=XID
	
	USE CONDC order 1 exclusive
	seek XID
	dele while ID=XID
	
	USE CALIF order 1 exclusive
	seek XID
	dele while ID=XID
	
	USE REG order 2
