Close DAtABASES
*Use AGENDA Order 2 In 1 Alias AG
*Use REG Order 2 in 2

*Select AG
*Set Relation To ID Into REG

*Select AGENDA.ID,AGENDA.AGN,AGENDA.FECHA,AGENDA.M1,AGENDA.NP,REG.CORR From AGENDA,REG WHERE AGENDA.ID=REG.ID And AGENDA.NP>0 ORDER BY 1
xx="UNION ALL Select ID,AGN,FECHA,M1,NP,CORR FROM REG Where AGN=9 And NP>0"


Select AGENDA.ID,AGENDA.AGN,AGENDA.FECHA,AGENDA.M1,AGENDA.NP,REG.CORR From AGENDA,REG WHERE AGENDA.ID=REG.ID And AGENDA.NP>0 ; 
&xx Order By 1 Into Table PAGENDA 

Browse