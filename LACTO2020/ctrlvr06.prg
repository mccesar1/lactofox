PROC RV10148
Declare a1(20)
a1=0
nvtot=0
*-------------------------------------------------------------------------*
Select REG
Set Filter To
Set Order To 2
Calc max(palp) for np>0 and stat=[CARGA] to fpa2
fpa2=fpa2-Q7

GO TOP
SCAN
DO CASE
	
		** Vientres Activas y Vientres candidato Rastro
		** --------------------------------------
	CASE fb2=b 
		nvtot=nvtot+1

	do case
		case fsec#b		
			a1(1)=a1(1)+1
	endcase
	
	** Vientres en calor a 50 dias.
	** -------------------------
	if date()-fpar>=50 
		a1(2)=a1(2)+1

	if pcel-fpar<=50 or pser-fpar<=50 
		a1(3)=a1(3)+1
	endi
	endi
	
	** Vientres inseminadas a 90 dias.
	** ----------------------------
	if ns>0 and date()-fpar>90
		a1(4)=a1(4)+1
	endi
	if pser#b and pser-fpar<=90 and date()-fpar>90
		a1(5)=a1(5)+1
	endi
	
	** Vientres cargadas y Secas.
	** Vientres cargadas y en orde¤a.
	** ---------------------------
	if stat=[CARGA] and fsec#b 
		a1(6)=a1(6)+1
	endi
	if stat=[CARGA] and fsec=b
		a1(7)=a1(7)+1
	endi
	if stat#[CARGA] and fsec=b
		a1(8)=a1(8)+1
	endi
	

	** Vientres cargadas a 150 dias
	** -------------------------
	if stat=[CARGA]
		a1(9)=a1(9)+1
	endi
	if stat=[CARGA] and ucal-fpar<=150
		a1(10)=a1(10)+1
	endi
	
	** Vientres con mas de 90 dias en secas
	** ---------------------------------
	if date()-fsec>90								
		a1(11)=a1(11)+1
	endi
	
	** Vientres Abiertas con mas de 150 dias
	** ----------------------------------
	if stat!=[CARGA] and date()-fpar>150 and stat!=[INSEM] 
		a1(12)=a1(12)+1
	endi				
	
	
	** % Mensual de Partos y Vientres a Secas
	** -----------------------------------
	if fpar>=date()-30
		a1(13)=a1(13)+1
	endi
	if fsec>=date()-30
		a1(14)=a1(14)+1
	endi
	
	** Vientres cargadas por servicio.
	** ---------------------------
	if stat=[CARGA] and ns=1
		a1(15)=a1(15)+1
	endi
	if stat=[CARGA] and ns>0 and ns<4
		a1(16)=a1(16)+1
	endi								
	if ns>3
		a1(17)=a1(17)+1
	endi
	
	** Fertilidad a 1er Servicio
	** -Vientres Inseminadas-------
	if ns>0 and ucal<=fpa2
		a1(18)=a1(18)+1
	endi
			
ENDCASE
ENDSCAN

Create Cursor REPORTE (Concepto c(45),COL1 c(6),COL2 c(6))
	Append Blank
	Replace CONCEPTO With " Detectadas en Celo a 50 Dias en Leche",COL1 With '>80',COL2 With Str((a1(3)/a1(2))*100,4,1)
	Append Blank
	Replace CONCEPTO With " Inseminadas a 90 dias en Leche       ",COL1 With '>90',COL2 With Str((a1(5)/a1(4))*100,4,1)
	Append Blank
	Replace CONCEPTO With " Gestantes Hasta 150 Dias en Leche    ",COL1 With '>80',COL2 With Str((a1(10)/a1(9))*100,4,1)
	Append Blank
	Replace CONCEPTO With " Abiertas con +150 Dias en Leche      ",COL1 With '<10',COL2 With Str((a1(12)/nvtot)*100,4,1)
	Append Blank
	Replace CONCEPTO With " Secas con +90 Dias en Secas          ",COL1 With '<10',COL2 With Str((a1(11)/a1(1))*100,4,1)
	Append Blank
	Replace CONCEPTO With " Gestantes y en Secas                 ",COL1 With '15-17',COL2 With Str((a1(6)/nvtot)*100,4,1)
	Append Blank
	Replace CONCEPTO With " Gestantes y en Leche                 ",COL1 With ' 42',COL2 With Str((a1(7)/nvtot)*100,4,1)
	Append Blank
	Replace CONCEPTO With " No Gestantes y en Leche              ",COL1 With ' 41',COL2 With Str((a1(8)/nvtot)*100,4,1)
	Append Blank
	Replace CONCEPTO With " Mensual de Partos               ",COL1 With '> 8',COL2 With Str((a1(13)/nvtot)*100,4,1)
	Append Blank
	Replace CONCEPTO With " Mensual de Vientres a Secas        ",COL1 With '<10',COL2 With Str((a1(14)/nvtot)*100,4,1)
	Append Blank
	Replace CONCEPTO With " Gestantes a 1er Servicio              ",COL1 With '>40',COL2 With Str((a1(15)/a1(9))*100,4,1)
	Append Blank
	Replace CONCEPTO With " Gestantes de 3 o Menos Servicios      ",COL1 With '85-88',COL2 With Str((a1(16)/a1(9))*100,4,1)
	Append Blank
	Replace CONCEPTO With " Inseminadas con 4+ Servicios         ",COL1 With '<15',COL2 With Str((a1(17)/nvtot)*100,4,1)
	
GO TOP
RETURN	

* ANALISIS DE DIAS A PRIMER SERVICIO
* ----------------------------------
PROC RV10154
declare S1(250)
S1=0
Select REG
Set Order To 2
Set Filter To FB2=B

SCAN
DO CASE 

CASE NP=1 and (pser-fpar)>0 and (pser-fpar)<=60
			s1(1)=s1(1)+1
		
		if stat="CARGA"
			s1(2)=s1(2)+1
			s1(7)=s1(7)+ns
			s1(12)=s1(12)+(ucal-fpar)
		else
			s1(3)=s1(3)+1
		endi	

		if stat="CARGA" and ns=1
			s1(4)=s1(4)+1
		endi
		
		if stat="CARGA" and ns=2
			s1(5)=s1(5)+1
		endi
				
		if stat="CARGA" and (ucal-fpar)+Q39<=395
			s1(6)=s1(6)+1
		endi
		
		if ns>0
			s1(8)=s1(8)+1
			s1(9)=s1(9)+(ucal-fpar)
			s1(13)=s1(13)+(pser-fpar)
		else
			s1(10)=s1(10)+1
			s1(11)=s1(11)+(date()-fpar)
		endi


CASE NP=1 and pser-fpar>60 and pser-fpar<=80
			s1(15)=s1(15)+1
		
		if stat="CARGA"
			s1(16)=s1(16)+1
			s1(17)=s1(17)+ns
			s1(18)=s1(18)+(ucal-fpar)
		else
			s1(19)=s1(19)+1
		endi	

		if stat="CARGA" and ns=1
			s1(20)=s1(20)+1
		endi
		
		if stat="CARGA" and ns=2
			s1(21)=s1(21)+1
		endi
				
			if stat="CARGA" and (ucal-fpar)+Q39<=395
			s1(22)=s1(22)+1
		endi
		
		if ns>0
			s1(23)=s1(23)+1
			s1(24)=s1(24)+(ucal-fpar)
			s1(27)=s1(27)+(pser-fpar)
		else
			s1(25)=s1(25)+1
			s1(26)=s1(26)+(date()-fpar)
		endi

CASE NP=1 and pser-fpar>80 and pser-fpar<=100
			s1(30)=s1(30)+1
		
		if stat="CARGA"
			s1(31)=s1(31)+1
			s1(32)=s1(32)+ns
			s1(33)=s1(33)+(ucal-fpar)
		else
			s1(34)=s1(34)+1
		endi	

		if stat="CARGA" and ns=1
			s1(35)=s1(35)+1
		endi
		
		if stat="CARGA" and ns=2
			s1(36)=s1(36)+1
		endi
				
		if stat="CARGA" and (ucal-fpar)+Q39<=395
			s1(37)=s1(37)+1
		endi
		
		if ns>0
			s1(38)=s1(38)+1
			s1(39)=s1(39)+(ucal-fpar)
			s1(42)=s1(42)+(pser-fpar)
		else
			s1(40)=s1(40)+1
			s1(41)=s1(41)+(date()-fpar)
		endi

CASE NP=1 and pser-fpar>100
			s1(45)=s1(45)+1
		
		if stat="CARGA"
			s1(46)=s1(46)+1
			s1(47)=s1(47)+ns
			s1(48)=s1(48)+(ucal-fpar)
		else
			s1(49)=s1(49)+1
		endi	

		if stat="CARGA" and ns=1
			s1(50)=s1(50)+1
		endi
		
		if stat="CARGA" and ns=2
			s1(51)=s1(51)+1
		endi
				
		if stat="CARGA" and (ucal-fpar)+Q39<=395
			s1(52)=s1(52)+1
		endi
		
		if ns>0
			s1(53)=s1(53)+1
			s1(54)=s1(54)+(ucal-fpar)
			s1(57)=s1(57)+(pser-fpar)
		else
			s1(55)=s1(55)+1
			s1(56)=s1(56)+(date()-fpar)
		endi

** 2a Lactancia
** ------------

CASE NP=2 and (pser-fpar)>0 and (pser-fpar)<=60
			s1(61)=s1(61)+1
		
		if stat="CARGA"
			s1(62)=s1(62)+1
			s1(67)=s1(67)+ns
			s1(72)=s1(72)+(ucal-fpar)
		else
			s1(63)=s1(63)+1
		endi	

		if stat="CARGA" and ns=1
			s1(64)=s1(64)+1
		endi
		
		if stat="CARGA" and ns=2
			s1(65)=s1(65)+1
		endi
				
		if stat="CARGA" and (ucal-fpar)+Q39<=395
			s1(66)=s1(66)+1
		endi
		
		if ns>0
			s1(68)=s1(68)+1
			s1(69)=s1(69)+(ucal-fpar)
			s1(73)=s1(73)+(pser-fpar)
		else
			s1(70)=s1(70)+1
			s1(71)=s1(71)+(date()-fpar)
		endi


CASE NP=2 and pser-fpar>60 and pser-fpar<=80
			s1(75)=s1(75)+1
		
		if stat="CARGA"
			s1(76)=s1(76)+1
			s1(77)=s1(77)+ns
			s1(78)=s1(78)+(ucal-fpar)
		else
			s1(79)=s1(79)+1
		endi	

		if stat="CARGA" and ns=1
			s1(80)=s1(80)+1
		endi
		
		if stat="CARGA" and ns=2
			s1(81)=s1(81)+1
		endi
				
		if stat="CARGA" and (ucal-fpar)+Q39<=395
			s1(82)=s1(82)+1
		endi
		
		if ns>0
			s1(83)=s1(83)+1
			s1(84)=s1(84)+(ucal-fpar)
			s1(87)=s1(87)+(pser-fpar)
		else
			s1(85)=s1(85)+1
			s1(86)=s1(86)+(date()-fpar)
		endi

CASE NP=2 and pser-fpar>80 and pser-fpar<=100
			s1(90)=s1(90)+1
		
		if stat="CARGA"
			s1(91)=s1(91)+1
			s1(92)=s1(92)+ns
			s1(93)=s1(93)+(ucal-fpar)
		else
			s1(94)=s1(94)+1
		endi	

		if stat="CARGA" and ns=1
			s1(95)=s1(95)+1
		endi
		
		if stat="CARGA" and ns=2
			s1(96)=s1(96)+1
		endi
				
		if stat="CARGA" and (ucal-fpar)+Q39<=395
			s1(97)=s1(97)+1
		endi
		
		if ns>0
			s1(98)=s1(98)+1
			s1(99)=s1(99)+(ucal-fpar)
			s1(102)=s1(102)+(pser-fpar)
		else
			s1(100)=s1(100)+1
			s1(101)=s1(101)+(date()-fpar)
		endi

CASE NP=2 and pser-fpar>100
			s1(105)=s1(105)+1
		
		if stat="CARGA"
			s1(106)=s1(106)+1
			s1(107)=s1(107)+ns
			s1(108)=s1(108)+(ucal-fpar)
		else
			s1(109)=s1(109)+1
		endi	

		if stat="CARGA" and ns=1
			s1(110)=s1(110)+1
		endi
		
		if stat="CARGA" and ns=2
			s1(111)=s1(111)+1
		endi
				
		if stat="CARGA" and (ucal-fpar)+Q39<=395
			s1(112)=s1(112)+1
		endi
		
		if ns>0
			s1(113)=s1(113)+1
			s1(114)=s1(114)+(ucal-fpar)
			s1(117)=s1(117)+(pser-fpar)
		else
			s1(115)=s1(115)+1
			s1(116)=s1(116)+(date()-fpar)
		endi

** 3+ Lactancias
** -------------

CASE NP>2 and (pser-fpar)>0 and (pser-fpar)<=60
			s1(121)=s1(121)+1
		
		if stat="CARGA"
			s1(122)=s1(122)+1
			s1(127)=s1(127)+ns
			s1(132)=s1(132)+(ucal-fpar)
		else
			s1(123)=s1(123)+1
		endi	

		if stat="CARGA" and ns=1
			s1(124)=s1(124)+1
		endi
		
		if stat="CARGA" and ns=2
			s1(125)=s1(125)+1
		endi
				
		if stat="CARGA" and (ucal-fpar)+Q39<=395
			s1(126)=s1(126)+1
		endi
		
		if ns>0
			s1(128)=s1(128)+1
			s1(129)=s1(129)+(ucal-fpar)
			s1(133)=s1(133)+(pser-fpar)
		else
			s1(130)=s1(130)+1
			s1(131)=s1(131)+(date()-fpar)
		endi


CASE NP>2 and pser-fpar>60 and pser-fpar<=80
			s1(135)=s1(135)+1
		
		if stat="CARGA"
			s1(136)=s1(136)+1
			s1(137)=s1(137)+ns
			s1(138)=s1(138)+(ucal-fpar)
		else
			s1(139)=s1(139)+1
		endi	

		if stat="CARGA" and ns=1
			s1(140)=s1(140)+1
		endi
		
		if stat="CARGA" and ns=2
			s1(141)=s1(141)+1
		endi
				
		if stat="CARGA" and (ucal-fpar)+Q39<=395
			s1(142)=s1(142)+1
		endi
		
		if ns>0
			s1(143)=s1(143)+1
			s1(144)=s1(144)+(ucal-fpar)
			s1(147)=s1(147)+(pser-fpar)
		else
			s1(145)=s1(145)+1
			s1(146)=s1(146)+(date()-fpar)
		endi

CASE NP>2 and pser-fpar>80 and pser-fpar<=100
			s1(150)=s1(150)+1
		
		if stat="CARGA"
			s1(151)=s1(151)+1
			s1(152)=s1(152)+ns
			s1(153)=s1(153)+(ucal-fpar)
		else
			s1(154)=s1(154)+1
		endi	

		if stat="CARGA" and ns=1
			s1(155)=s1(155)+1
		endi
		
		if stat="CARGA" and ns=2
			s1(156)=s1(156)+1
		endi
				
		if stat="CARGA" and (ucal-fpar)+Q39<=395
			s1(157)=s1(157)+1
		endi
		
		if ns>0
			s1(158)=s1(158)+1
			s1(159)=s1(159)+(ucal-fpar)
			s1(162)=s1(162)+(pser-fpar)
		else
			s1(160)=s1(160)+1
			s1(161)=s1(161)+(date()-fpar)
		endi

CASE NP>2 and pser-fpar>100
			s1(165)=s1(165)+1
		
		if stat="CARGA"
			s1(166)=s1(166)+1
			s1(167)=s1(167)+ns
			s1(168)=s1(168)+(ucal-fpar)
		else
			s1(169)=s1(169)+1
		endi	

		if stat="CARGA" and ns=1
			s1(170)=s1(170)+1
		endi
		
		if stat="CARGA" and ns=2
			s1(171)=s1(171)+1
		endi
				
		if stat="CARGA" and (ucal-fpar)+Q39<=395
			s1(172)=s1(172)+1
		endi
		
		if ns>0
			s1(173)=s1(173)+1
			s1(174)=s1(174)+(ucal-fpar)
			s1(177)=s1(177)+(pser-fpar)
		else
			s1(175)=s1(175)+1
			s1(176)=s1(176)+(date()-fpar)
		endi

ENDCASE


** Todas las Lactancias
** --------------------

DO CASE

CASE NP>=1 and (pser-fpar)>0 and (pser-fpar)<=60
			s1(181)=s1(181)+1
		
		if stat="CARGA"
			s1(182)=s1(182)+1
			s1(187)=s1(187)+ns
			s1(192)=s1(192)+(ucal-fpar)
		else
			s1(183)=s1(183)+1
		endi	

		if stat="CARGA" and ns=1
			s1(184)=s1(184)+1
		endi
		
		if stat="CARGA" and ns=2
			s1(185)=s1(185)+1
		endi
				
		if stat="CARGA" and (ucal-fpar)+Q39<=395
			s1(186)=s1(186)+1
		endi
		
		if ns>0
			s1(188)=s1(188)+1
			s1(189)=s1(189)+(ucal-fpar)
			s1(193)=s1(193)+(pser-fpar)
		else
			s1(190)=s1(190)+1
			s1(191)=s1(191)+(date()-fpar)
		endi


CASE NP>=1 and pser-fpar>60 and pser-fpar<=80
			s1(195)=s1(195)+1
		
		if stat="CARGA"
			s1(196)=s1(196)+1
			s1(197)=s1(197)+ns
			s1(198)=s1(198)+(ucal-fpar)
		else
			s1(199)=s1(199)+1
		endi	

		if stat="CARGA" and ns=1
			s1(200)=s1(200)+1
		endi
		
		if stat="CARGA" and ns=2
			s1(201)=s1(201)+1
		endi
				
		if stat="CARGA" and (ucal-fpar)+Q39<=395
			s1(202)=s1(202)+1
		endi
		
		if ns>0
			s1(203)=s1(203)+1
			s1(204)=s1(204)+(ucal-fpar)
			s1(207)=s1(207)+(pser-fpar)
		else
			s1(205)=s1(205)+1
			s1(206)=s1(206)+(date()-fpar)
		endi

CASE NP>=1 and pser-fpar>80 and pser-fpar<=100
			s1(210)=s1(210)+1
		
		if stat="CARGA"
			s1(211)=s1(211)+1
			s1(212)=s1(212)+ns
			s1(213)=s1(213)+(ucal-fpar)
		else
			s1(214)=s1(214)+1
		endi	

		if stat="CARGA" and ns=1
			s1(215)=s1(215)+1
		endi
		
		if stat="CARGA" and ns=2
			s1(216)=s1(216)+1
		endi
				
		if stat="CARGA" and (ucal-fpar)+Q39<=395
			s1(217)=s1(217)+1
		endi
		
		if ns>0
			s1(218)=s1(218)+1
			s1(219)=s1(219)+(ucal-fpar)
			s1(222)=s1(222)+(pser-fpar)
		else
			s1(220)=s1(220)+1
			s1(221)=s1(221)+(date()-fpar)
		endi

CASE NP>=1 and pser-fpar>100
			s1(225)=s1(225)+1
		
		if stat="CARGA"
			s1(226)=s1(226)+1
			s1(227)=s1(227)+ns
			s1(228)=s1(228)+(ucal-fpar)
		else
			s1(229)=s1(229)+1
		endi	

		if stat="CARGA" and ns=1
			s1(230)=s1(230)+1
		endi
		
		if stat="CARGA" and ns=2
			s1(231)=s1(231)+1
		endi
				
		if stat="CARGA" and (ucal-fpar)+Q39<=395
			s1(232)=s1(232)+1
		endi
		
		if ns>0
			s1(233)=s1(233)+1
			s1(234)=s1(234)+(ucal-fpar)
			s1(237)=s1(237)+(pser-fpar)
		else
			s1(235)=s1(235)+1
			s1(236)=s1(236)+(date()-fpar)
		endi
ENDCASE

ENDSCAN

if s1(2)=0
   s1(2)=.1
endi
if s1(8)=0
   s1(8)=.1
endi
	
if s1(106)=0
   s1(106)=.1
endi

if s1(170)=0
   s1(170)=.1
endi

s1(241)=s1(1)+s1(15)+s1(30)+s1(45)
s1(242)=s1(61)+s1(75)+s1(90)+s1(105)
s1(243)=s1(121)+s1(135)+s1(150)+s1(165)
s1(244)=s1(181)+s1(195)+s1(210)+s1(225)
Set Filter To

Create Cursor REPORTE (concepto c(45),COL1 c(6),COL2 c(6),COL3 c(6),COL4 c(6),COL5 c(6))
	Append Blank
	Replace CONCEPTO With "Lactancia 1 :"
	Append Blank
	Replace CONCEPTO With "Numero de Vientres                     ",COL1 With Str(S1(1),6),COL2 With Str(S1(15),6),COL3 With Str(S1(30),6),COL4 With Str(S1(45),6)
	Append Blank
	Replace CONCEPTO With "Vientres Confirmadas Gestantes         ",COL1 With Str(S1(2),6),COL2 With Str(S1(16),6),COL3 With Str(S1(31),6),COL4 With Str(S1(46),6)
	Append Blank
	Replace CONCEPTO With "Vientres Abiertas o No Confirmadas     ",COL1 With Str(S1(3),6),COL2 With Str(S1(19),6),COL3 With Str(S1(34),6),COL4 With Str(S1(49),6)
	Append Blank
	Replace CONCEPTO With "Confirmadas Gestantes de 1er Serv.  ",COL1 With Str(S1(4),6),COL2 With Str(S1(20),6),COL3 With Str(S1(35),6),COL4 With Str(S1(50),6)
	Append Blank
	Replace CONCEPTO With "Confirmadas Gestantes de 2do Serv.  ",COL1 With Str(S1(5),6),COL2 With Str(S1(21),6),COL3 With Str(S1(36),6),COL4 With Str(S1(51),6)
	Append Blank
	Replace CONCEPTO With "Confirmadas Gestantes de <13 m. IP  ",COL1 With Str(S1(6),6),COL2 With Str(S1(22),6),COL3 With Str(S1(37),6),COL4 With Str(S1(52),6)
	Append Blank
	Replace CONCEPTO With "Concepcion a 1er Servicio / Conf. Gestantes ",COL1 With Str(S1(4)/s1(2)*100,6,1),COL2 With Str(S1(20)/s1(16)*100,6,1),COL3 With Str(S1(35)/s1(31)*100,6,1),COL4 With Str(S1(50)/s1(46)*100,6,1)
	Append Blank
	Replace CONCEPTO With "Concepcion a 2do Servicio / Conf. Gestantes ",COL1 With Str(S1(5)/(s1(2)-s1(4))*100,6,1),COL2 With Str(S1(21)/(s1(16)-s1(20))*100,6,1),COL3 With Str(S1(36)/(s1(31)-s1(35))*100,6,1),COL4 With Str(S1(51)/(s1(46)-s1(50))*100,6,1)
	Append Blank
	Replace CONCEPTO With "Vientres Confirmadas Gestantes (%)     ",COL1 With Str(S1(2)/s1(1)*100,6,1),COL2 With Str(S1(16)/s1(15)*100,6,1),COL3 With Str(S1(31)/s1(30)*100,6,1),COL4 With Str(S1(46)/s1(45)*100,6,1)
	Append Blank
	Replace CONCEPTO With "Servs/Concep. (Vientres Conf. Gestantes)",COL1 With Str(S1(7)/s1(2),6,1),COL2 With Str(S1(17)/s1(16),6,1),COL3 With Str(S1(32)/s1(31),6,1),COL4 With Str(S1(47)/s1(46),6,1)
	Append Blank
	Replace CONCEPTO With "Promedio de Dias a 1er Servicio     ",COL1 With Str(S1(13)/s1(8),6,1),COL2 With Str(S1(27)/s1(23),6,1),COL3 With Str(S1(42)/s1(38),6,1),COL4 With Str(S1(57)/s1(53),6,1)
	Append Blank
	Replace CONCEPTO With "Promedio de Dias Abiertos (Total)   ",COL1 With Str((S1(9)+s1(11))/(s1(8)+s1(10)),6,1),COL2 With Str((S1(24)+s1(26))/(s1(23)+s1(25)),6,1),COL3 With Str((S1(39)+s1(41))/(s1(38)+s1(40)),6,1),COL4 With Str((S1(54)+s1(56))/(s1(53)+s1(55)),6,1)
	Append Blank
	Replace CONCEPTO With "Promedio de Dias Abiertos (Solo Gestantes)",COL1 With Str(S1(12)/s1(2),6,1),COL2 With Str(S1(18)/s1(16),6,1),COL3 With Str(S1(33)/s1(31),6,1),COL4 With Str(S1(48)/s1(46),6,1)
	Append Blank
	Replace CONCEPTO With ""

	Append Blank
	Replace CONCEPTO With "Lactancia 2 :"
	Append Blank
	Replace CONCEPTO With "Numero de Vientres                     ",COL1 With Str(S1(61),6),COL2 With Str(S1(75),6),COL3 With Str(S1(90),6),COL4 With Str(S1(105),6)
	Append Blank
	Replace CONCEPTO With "Vientres Confirmadas Gestantes         ",COL1 With Str(S1(62),6),COL2 With Str(S1(76),6),COL3 With Str(S1(91),6),COL4 With Str(S1(106),6)
	Append Blank
	Replace CONCEPTO With "Vientres Abiertas o No Confirmadas     ",COL1 With Str(S1(63),6),COL2 With Str(S1(79),6),COL3 With Str(S1(94),6),COL4 With Str(S1(109),6)
	Append Blank
	Replace CONCEPTO With "Confirmadas Gestantes de 1er Serv.  ",COL1 With Str(S1(64),6),COL2 With Str(S1(80),6),COL3 With Str(S1(95),6),COL4 With Str(S1(110),6)
	Append Blank
	Replace CONCEPTO With "Confirmadas Gestantes de 2do Serv.  ",COL1 With Str(S1(65),6),COL2 With Str(S1(81),6),COL3 With Str(S1(96),6),COL4 With Str(S1(111),6)
	Append Blank
	Replace CONCEPTO With "Confirmadas Gestantes de <13 m. IP  ",COL1 With Str(S1(66),6),COL2 With Str(S1(82),6),COL3 With Str(S1(97),6),COL4 With Str(S1(112),6)
	Append Blank
	Replace CONCEPTO With "Concepcion a 1er Servicio / Conf. Gestantes ",COL1 With Str(S1(64)/s1(6)*100,6,1),COL2 With Str(S1(80)/s1(75)*100,6,1),COL3 With Str(S1(95)/s1(90)*100,6,1),COL4 With Str(S1(110)/s1(105)*100,6,1)
	Append Blank
	Replace CONCEPTO With "Concepcion a 2do Servicio / Conf. Gestantes ",COL1 With Str(S1(65)/(s1(62)-s1(64))*100,6,1),COL2 With Str(S1(81)/(s1(76)-s1(80))*100,6,1),COL3 With Str(S1(96)/(s1(91)-s1(95))*100,6,1),COL4 With Str(S1(111)/(s1(106)-s1(110))*100,6,1)
	Append Blank
	Replace CONCEPTO With "Vientres Confirmadas Gestantes (%)     ",COL1 With Str(S1(62)/s1(61)*100,6,1),COL2 With Str(S1(76)/s1(75)*100,6,1),COL3 With Str(S1(91)/s1(90)*100,6,1),COL4 With Str(S1(106)/s1(105)*100,6,1)
	Append Blank
	Replace CONCEPTO With "Servs/Concep. (Vientres Conf. Gestantes)",COL1 With Str(S1(67)/s1(62),6,1),COL2 With Str(S1(77)/s1(76),6,1),COL3 With Str(S1(92)/s1(91),6,1),COL4 With Str(S1(107)/s1(106),6,1)
	Append Blank
	Replace CONCEPTO With "Promedio de Dias a 1er Servicio     ",COL1 With Str(S1(73)/s1(68),6,1),COL2 With Str(S1(87)/s1(83),6,1),COL3 With Str(S1(102)/s1(98),6,1),COL4 With Str(S1(117)/s1(113),6,1)
	Append Blank
	Replace CONCEPTO With "Promedio de Dias Abiertos (Total)   ",COL1 With Str((S1(69)+s1(71))/(s1(68)+s1(70)),6,1),COL2 With Str((S1(84)+s1(86))/(s1(83)+s1(85)),6,1),COL3 With Str((S1(99)+s1(101))/(s1(98)+s1(100)),6,1),COL4 With Str((S1(114)+s1(116))/(s1(113)+s1(115)),6,1)
	Append Blank
	Replace CONCEPTO With "Promedio de Dias Abiertos (Solo Gestantes)",COL1 With Str(S1(72)/s1(62),6,1),COL2 With Str(S1(78)/s1(76),6,1),COL3 With Str(S1(93)/s1(91),6,1),COL4 With Str(S1(108)/s1(106),6,1)
	Append Blank
	Replace CONCEPTO With ""
	
	Append Blank
	Replace CONCEPTO With "Lactancia 3+ :"
	Append Blank
	Replace CONCEPTO With "Numero de Vientres                     ",COL1 With Str(S1(121),6),COL2 With Str(S1(135),6),COL3 With Str(S1(150),6),COL4 With Str(S1(165),6)
	Append Blank
	Replace CONCEPTO With "Vientres Confirmadas Gestantes         ",COL1 With Str(S1(122),6),COL2 With Str(S1(136),6),COL3 With Str(S1(151),6),COL4 With Str(S1(166),6)
	Append Blank
	Replace CONCEPTO With "Vientres Abiertas o No Confirmadas     ",COL1 With Str(S1(123),6),COL2 With Str(S1(139),6),COL3 With Str(S1(154),6),COL4 With Str(S1(169),6)
	Append Blank
	Replace CONCEPTO With "Confirmadas Gestantes de 1er Serv.  ",COL1 With Str(S1(124),6),COL2 With Str(S1(140),6),COL3 With Str(S1(155),6),COL4 With Str(S1(170),6)
	Append Blank
	Replace CONCEPTO With "Confirmadas Gestantes de 2do Serv.  ",COL1 With Str(S1(125),6),COL2 With Str(S1(141),6),COL3 With Str(S1(156),6),COL4 With Str(S1(171),6)
	Append Blank
	Replace CONCEPTO With "Confirmadas Gestantes de <13 m. IP  ",COL1 With Str(S1(126),6),COL2 With Str(S1(142),6),COL3 With Str(S1(157),6),COL4 With Str(S1(172),6)
	Append Blank
	Replace CONCEPTO With "Concepcion a 1er Servicio / Conf. Gestantes ",COL1 With Str(S1(124)/s1(122)*100,6,1),COL2 With Str(S1(140)/s1(136)*100,6,1),COL3 With Str(S1(155)/s1(151)*100,6,1),COL4 With Str(S1(170)/s1(166)*100,6,1)
	Append Blank
	Replace CONCEPTO With "Concepcion a 2do Servicio / Conf. Gestantes ",COL1 With Str(S1(125)/(s1(122)-s1(124))*100,6,1),COL2 With Str(S1(141)/(s1(136)-s1(140))*100,6,1),COL3 With Str(S1(156)/(s1(151)-s1(155))*100,6,1),COL4 With Str(S1(171)/(s1(166)-s1(170))*100,6,1)
	Append Blank
	Replace CONCEPTO With "Vientres Confirmadas Gestantes (%)     ",COL1 With Str(S1(122)/s1(121)*100,6,1),COL2 With Str(S1(136)/s1(135)*100,6,1),COL3 With Str(S1(151)/s1(150)*100,6,1),COL4 With Str(S1(166)/s1(165)*100,6,1)
	Append Blank
	Replace CONCEPTO With "Servs/Concep. (Vientres Conf. Gestantes)",COL1 With Str(S1(127)/s1(122),6,1),COL2 With Str(S1(137)/s1(136),6,1),COL3 With Str(S1(152)/s1(151),6,1),COL4 With Str(S1(167)/s1(166),6,1)
	Append Blank
	Replace CONCEPTO With "Promedio de Dias a 1er Servicio     ",COL1 With Str(S1(133)/s1(128),6,1),COL2 With Str(S1(147)/s1(143),6,1),COL3 With Str(S1(162)/s1(158),6,1),COL4 With Str(S1(177)/s1(173),6,1)
	Append Blank
	Replace CONCEPTO With "Promedio de Dias Abiertos (Total)   ",COL1 With Str((S1(129)+s1(131))/(s1(128)+s1(130)),6,1),COL2 With Str((S1(144)+s1(146))/(s1(143)+s1(145)),6,1),COL3 With Str((S1(159)+s1(161))/(s1(158)+s1(160)),6,1),COL4 With Str((S1(174)+s1(176))/(s1(173)+s1(175)),6,1)
	Append Blank
	Replace CONCEPTO With "Promedio de Dias Abiertos (Solo Gestantes)",COL1 With Str(S1(132)/s1(122),6,1),COL2 With Str(S1(138)/s1(136),6,1),COL3 With Str(S1(153)/s1(151),6,1),COL4 With Str(S1(168)/s1(166),6,1)
	Append Blank
	Replace CONCEPTO With ""
	
	Append Blank
	Replace CONCEPTO With "Todas :"
	Append Blank
	Replace CONCEPTO With "Numero de Vientres                     ",COL1 With Str(S1(181),6),COL2 With Str(S1(195),6),COL3 With Str(S1(210),6),COL4 With Str(S1(225),6)
	Append Blank
	Replace CONCEPTO With "Vientres Confirmadas Gestantes         ",COL1 With Str(S1(182),6),COL2 With Str(S1(196),6),COL3 With Str(S1(211),6),COL4 With Str(S1(226),6)
	Append Blank
	Replace CONCEPTO With "Vientres Abiertas o No Confirmadas     ",COL1 With Str(S1(183),6),COL2 With Str(S1(199),6),COL3 With Str(S1(214),6),COL4 With Str(S1(229),6)
	Append Blank
	Replace CONCEPTO With "Confirmadas Gestantes de 1er Serv.  ",COL1 With Str(S1(184),6),COL2 With Str(S1(200),6),COL3 With Str(S1(215),6),COL4 With Str(S1(230),6)
	Append Blank
	Replace CONCEPTO With "Confirmadas Gestantes de 2do Serv.  ",COL1 With Str(S1(185),6),COL2 With Str(S1(201),6),COL3 With Str(S1(216),6),COL4 With Str(S1(231),6)
	Append Blank
	Replace CONCEPTO With "Confirmadas Gestantes de <13 m. IP  ",COL1 With Str(S1(186),6),COL2 With Str(S1(202),6),COL3 With Str(S1(217),6),COL4 With Str(S1(232),6)
	Append Blank
	Replace CONCEPTO With "Concepcion a 1er Servicio / Conf. Gestantes ",COL1 With Str(S1(184)/s1(182)*100,6,1),COL2 With Str(S1(200)/s1(196)*100,6,1),COL3 With Str(S1(215)/s1(211)*100,6,1),COL4 With Str(S1(230)/s1(226)*100,6,1)
	Append Blank
	Replace CONCEPTO With "Concepcion a 2do Servicio / Conf. Gestantes ",COL1 With Str(S1(185)/(s1(182)-s1(184))*100,6,1),COL2 With Str(S1(201)/(s1(196)-s1(200))*100,6,1),COL3 With Str(S1(216)/(s1(211)-s1(215))*100,6,1),COL4 With Str(S1(231)/(s1(226)-s1(230))*100,6,1)
	Append Blank
	Replace CONCEPTO With "Vientres Confirmadas Gestantes (%)     ",COL1 With Str(S1(182)/s1(181)*100,6,1),COL2 With Str(S1(196)/s1(195)*100,6,1),COL3 With Str(S1(211)/s1(210)*100,6,1),COL4 With Str(S1(226)/s1(225)*100,6,1)
	Append Blank
	Replace CONCEPTO With "Servs/Concep. (Vientres Conf. Gestantes)",COL1 With Str(S1(187)/s1(182),6,1),COL2 With Str(S1(197)/s1(196),6,1),COL3 With Str(S1(212)/s1(211),6,1),COL4 With Str(S1(227)/s1(226),6,1)
	Append Blank
	Replace CONCEPTO With "Promedio de Dias a 1er Servicio     ",COL1 With Str(S1(193)/s1(188),6,1),COL2 With Str(S1(207)/s1(203),6,1),COL3 With Str(S1(222)/s1(218),6,1),COL4 With Str(S1(237)/s1(233),6,1)
	Append Blank
	Replace CONCEPTO With "Promedio de Dias Abiertos (Total)   ",COL1 With Str((S1(189)+s1(191))/(s1(188)+s1(190)),6,1),COL2 With Str((S1(204)+s1(206))/(s1(203)+s1(205)),6,1),COL3 With Str((S1(219)+s1(221))/(s1(218)+s1(220)),6,1),COL4 With Str((S1(234)+s1(236))/(s1(233)+s1(235)),6,1)
	Append Blank
	Replace CONCEPTO With "Promedio de Dias Abiertos (Solo Gestantes)",COL1 With Str(S1(192)/s1(182),6,1),COL2 With Str(S1(198)/s1(196),6,1),COL3 With Str(S1(213)/s1(211),6,1),COL4 With Str(S1(228)/s1(226),6,1)
	Append Blank
	Replace CONCEPTO With ""

	* Resultados de ANALI2
	* --------------------
	Append Blank
	Replace CONCEPTO With "Distribucion de Dias a Primer Servicio"
	Append Blank
	Replace CONCEPTO With "Lactancia 1  (%)",COL1 With Str(S1(1)/S1(241)*100,6),COL2 With Str(S1(15)/s1(241)*100,6),COL3 With Str(S1(30)/s1(241)*100,6),COL4 With Str(S1(45)/s1(241)*100,6)
	Append Blank
	Replace CONCEPTO With "Lactancia 2  (%)",COL1 With Str(S1(61)/S1(242)*100,6),COL2 With Str(S1(75)/s1(242)*100,6),COL3 With Str(S1(90)/s1(242)*100,6),COL4 With Str(S1(105)/s1(242)*100,6)
	Append Blank
	Replace CONCEPTO With "Lactancia 3+ (%)",COL1 With Str(S1(121)/S1(243)*100,6),COL2 With Str(S1(135)/s1(243)*100,6),COL3 With Str(S1(150)/s1(243)*100,6),COL4 With Str(S1(165)/s1(243)*100,6)
	Append Blank
	Replace CONCEPTO With "Todas        ",COL1 With Str(S1(181)/S1(244)*100,6),COL2 With Str(S1(195)/s1(244)*100,6),COL3 With Str(S1(210)/s1(244)*100,6),COL4 With Str(S1(225)/s1(244)*100,6)

GO TOP
RETURN


* RESUMEN REPRODUCTIVO ANUAL
* --------------------------
PROC RV10155
DECLARE R1(125),aw(15)
R1=0
aw=0
Select REG
Set Filt To NP>0

GO TOP
SCAN
	
DO CASE 

** Lactancia 1
** -----------
	CASE np=1 and fb2=b
	
		r1(1)=r1(1)+1

			r1(14)=r1(14)+PCIP
		
		
			IF (date()-fpar)>Q5
				r1(2)=r1(2)+1
				if ns>0
				aw(1)=aw(1)+1                
				r1(10)=r1(10)+ns
					if (pser-fpar)>0
						r1(12)=r1(12)+(pser-fpar)
						r1(101)=r1(101)+1
					endi	
					
					r1(13)=r1(13)+1
				endi	

				do case
					case stat="CARGA" or stat="INSEM"
					r1(15)=r1(15)+(ucal-fpar)
					otherwise
					r1(16)=r1(16)+(date()-fpar)
				endcase
			endi

			if stat="CARGA"
				r1(3)=r1(3)+1
				r1(11)=r1(11)+ns
                r1(107)=r1(107)+(ucal-fpar)  
 
				do case
					case (ucal-fpar)+Q39<=365
						r1(17)=r1(17)+1
					case (ucal-fpar)+Q39>365 and (ucal-fpar)+Q39<=395
						r1(18)=r1(18)+1
					case (ucal-fpar)+Q39>395 and (ucal-fpar)+Q39<=426
						r1(19)=r1(19)+1
					case (ucal-fpar)+Q39>426
						r1(20)=r1(20)+1
				endcase
			endi
		
			if stat="CARGA" and ns=1
				r1(4)=r1(4)+1
			endi
				
			if stat="CARGA" and ns=2
				r1(5)=r1(5)+1
			endi
						
			if fpar>=date()-365
				r1(6)=r1(6)+1	
			endi

			if ns=0 and (date()-fpar)-(date()-fsec)>80
				r1(7)=r1(7)+1
			endi	
			
			if ns>3
				r1(8)=r1(8)+1
			endi	

			if stat="CARGA" and (ucal-fpar)+Q39<=395
				r1(9)=r1(9)+1
			endi

			** Calculo de Dias Abiertos sin contar las Vientres abortadas
			** -------------------------------------------------------
			if abto=ctod("") and (date()-fpar)>Q5
				r1(111)=r1(111)+1
				do case
				   case	ns>0 
				   		r1(112)=r1(112)+(ucal-fpar)
					otherwise				   		
					    r1(113)=r1(113)+(date()-fpar)
				endcase
				aw(11)=(r1(112)+r1(113))/r1(111)
			endif
				
** Lactancia 2
** -----------

	CASE np=2 and fb2=b
	
		r1(21)=r1(21)+1
			if PCIP>0
				r1(34)=r1(34)+PCIP
				r1(96)=r1(96)+1
			endi	
			if (date()-fpar)>Q5
				r1(22)=r1(22)+1
				if ns>0
					aw(2)=aw(2)+1                
					r1(30)=r1(30)+ns
					r1(33)=r1(33)+1
					if (pser-fpar)>0
						r1(32)=r1(32)+(pser-fpar)
						r1(102)=r1(102)+1
					endi	
				endi	

				do case
					case stat="CARGA" or stat="INSEM"
					r1(35)=r1(35)+(ucal-fpar)
					otherwise
					r1(36)=r1(36)+(date()-fpar)
				endcase
			endi

			if stat="CARGA"
				r1(23)=r1(23)+1
				r1(31)=r1(31)+ns
                r1(108)=r1(108)+(ucal-fpar)  

				do case
					case (ucal-fpar)+Q39<=365
						r1(37)=r1(37)+1
					case (ucal-fpar)+Q39>365 and (ucal-fpar)+Q39<=395
						r1(38)=r1(38)+1
					case (ucal-fpar)+Q39>395 and (ucal-fpar)+Q39<=426
						r1(39)=r1(39)+1
					case (ucal-fpar)+Q39>426
						r1(40)=r1(40)+1
				endcase


			endi
		
			if stat="CARGA" and ns=1
				r1(24)=r1(24)+1
			endi
				
			if stat="CARGA" and ns=2
				r1(25)=r1(25)+1
			endi
						
			if fpar>=date()-365
				r1(26)=r1(26)+1	
			endi

			if ns=0 and (date()-fpar)-(date()-fsec)>80
				r1(27)=r1(27)+1
			endi	
			
			if ns>3
				r1(28)=r1(28)+1
			endi	

			if stat="CARGA" and (ucal-fpar)+Q39<=395
				r1(29)=r1(29)+1
			endi

			** Calculo de Dias Abiertos sin contar las Vientres abortadas
			** -------------------------------------------------------
			if abto=ctod("") and (date()-fpar)>Q5
				r1(114)=r1(114)+1
				do case
				   case	ns>0 
				   		r1(115)=r1(115)+(ucal-fpar)
					otherwise				   		
					    r1(116)=r1(116)+(date()-fpar)
				endcase
			aw(12)=(r1(115)+r1(116))/r1(114)
			endif

** Lactancia 3+
** -------------

	CASE np>2 and fb2=b
	
		r1(41)=r1(41)+1
			if PCIP>0
				r1(54)=r1(54)+PCIP
				r1(97)=r1(97)+1
			endi	
			if (date()-fpar)>Q5
				r1(42)=r1(42)+1
				if ns>0
	 				aw(3)=aw(3)+1                
					r1(50)=r1(50)+ns
					if (pser-fpar)>0
						r1(52)=r1(52)+(pser-fpar)
						r1(103)=r1(103)+1
					endi
					r1(53)=r1(53)+1
				endi	

				do case
					case stat="CARGA" or stat="INSEM"
					r1(55)=r1(55)+(ucal-fpar)
					otherwise
					r1(56)=r1(56)+(date()-fpar)
				endcase
			endi

			if stat="CARGA"
				r1(43)=r1(43)+1
				r1(51)=r1(51)+ns
                r1(109)=r1(109)+(ucal-fpar)  

				do case
					case (ucal-fpar)+Q39<=365
						r1(57)=r1(57)+1
					case (ucal-fpar)+Q39>365 and (ucal-fpar)+Q39<=395
						r1(58)=r1(58)+1
					case (ucal-fpar)+Q39>395 and (ucal-fpar)+Q39<=426
						r1(59)=r1(59)+1
					case (ucal-fpar)+Q39>426
						r1(60)=r1(60)+1
				endcase

			endi
		
			if stat="CARGA" and ns=1
				r1(44)=r1(44)+1
			endi
				
			if stat="CARGA" and ns=2
				r1(45)=r1(45)+1
			endi
						
			if fpar>=date()-365
				r1(46)=r1(46)+1	
			endi

			if ns=0 and (date()-fpar)-(date()-fsec)>80
				r1(47)=r1(47)+1
			endi	
			
			if ns>3
				r1(48)=r1(48)+1
			endi	

			if stat="CARGA" and (ucal-fpar)+Q39<=395
				r1(49)=r1(49)+1
			endi

			** Calculo de Dias Abiertos sin contar las Vientres abortadas
			** -------------------------------------------------------
			if abto=ctod("") and (date()-fpar)>Q5
				r1(117)=r1(117)+1
				do case
				   case	ns>0 
				   		r1(118)=r1(118)+(ucal-fpar)
					otherwise				   		
					    r1(119)=r1(119)+(date()-fpar)
				endcase
			aw(13)=(r1(118)+r1(119))/r1(117)
			endif

	CASE fb2#b
		
		do case 
			case np=1 and fb2>=date()-365
				r1(91)=r1(91)+1
				
			case np=2 and fb2>=date()-365
				r1(92)=r1(92)+1

			case np>2 and fb2>=date()-365
				r1(93)=r1(93)+1
		endcase

ENDCASE

DO CASE
** Todas las Lactancias
** -------------

	CASE np>=1 and fb2=b
	
		r1(61)=r1(61)+1

			if PCIP>0
				r1(85)=r1(85)+1
				r1(74)=r1(74)+PCIP
			endi
			
			if (date()-fpar)>Q5
				r1(62)=r1(62)+1
				if ns>0   
					aw(4)=aw(4)+1                
					r1(70)=r1(70)+ns
					if (pser-fpar)>0
						r1(72)=r1(72)+(pser-fpar)
						r1(104)=r1(104)+1
					endi	
					
					r1(73)=r1(73)+1
				endi	

				do case
					case stat="CARGA" or stat="INSEM"
					r1(75)=r1(75)+(ucal-fpar)
					otherwise
					r1(76)=r1(76)+(date()-fpar)
				endcase
			endi

			if stat="CARGA"
				r1(63)=r1(63)+1
				r1(71)=r1(71)+ns
                r1(110)=r1(110)+(ucal-fpar)  

				do case
					case (ucal-fpar)+Q39<=365
						r1(77)=r1(77)+1
					case (ucal-fpar)+Q39>365 and (ucal-fpar)+Q39<=395
						r1(78)=r1(78)+1
					case (ucal-fpar)+Q39>395 and (ucal-fpar)+Q39<=426
						r1(79)=r1(79)+1
					case (ucal-fpar)+Q39>426
						r1(80)=r1(80)+1
				endcase

			endi
		
			if stat="CARGA" and ns=1
				r1(64)=r1(64)+1
			endi
				
			if stat="CARGA" and ns=2
				r1(65)=r1(65)+1
			endi
						
			if fpar>=date()-365
				r1(66)=r1(66)+1	
			endi

			if ns=0 and (date()-fpar)-(date()-fsec)>80
				r1(67)=r1(67)+1
			endi	
			
			if ns>3
				r1(68)=r1(68)+1
			endi	

			if stat="CARGA" and (ucal-fpar)+Q39<=395
				r1(69)=r1(69)+1
			endi

			** Calculo de Dias Abiertos sin contar las Vientres abortadas
			** -------------------------------------------------------
			if abto=ctod("") and (date()-fpar)>Q5
				r1(120)=r1(120)+1
				do case
				   case	ns>0 
				   		r1(121)=r1(121)+(ucal-fpar)
					otherwise				   		
					    r1(122)=r1(122)+(date()-fpar)
				endcase
			aw(14)=(r1(121)+r1(122))/r1(120)
			endif
	
	CASE np>=1 and fb2#b and fb2>=date()-365
		r1(94)=r1(94)+1

	OTHERWISE

ENDCASE
ENDSCAN		

** Vientres que entraron al hato
r1(26)=0
r1(46)=0
r1(66)=0

** Dias Abiertos Lact. 1
r1(81)=(r1(15)+r1(16))/r1(2)	
r1(82)=(r1(35)+r1(36))/r1(22)	
r1(83)=(r1(55)+r1(56))/r1(42)	
r1(84)=(r1(75)+r1(76))/r1(62)	



Create Cursor REPORTE (concepto c(45),COL1 c(6),COL2 c(6),COL3 c(6),COL4 c(6),COL5 c(6))
	Append Blank
	Replace CONCEPTO With "Total de Vientres                      ",COL1 With Str(R1(1),5),COL2 With Str(R1(21),5),COL3 With Str(R1(41),5),COL4 With Str(R1(61),5)
	Append Blank
	Replace CONCEPTO With "Vientres Hato Reproductivo             ",COL1 With Str(R1(2),5),COL2 With Str(R1(22),5),COL3 With Str(R1(42),5),COL4 With Str(R1(62),5)
	Append Blank
	Replace CONCEPTO With "Vientres Confirmadas Gestantes         ",COL1 With Str(R1(3),5),COL2 With Str(R1(23),5),COL3 With Str(R1(43),5),COL4 With Str(R1(63),5)
	Append Blank
	Replace CONCEPTO With "% Vientres Confirmadas Gestantes       ",COL1 With Str(R1(3)/R1(2)*100,5,1),COL2 With Str(R1(23)/R1(22)*100,5,1),COL3 With Str(R1(43)/R1(42)*100,5,1),COL4 With Str(R1(63)/R1(62)*100,5,1)
	Append Blank
	Replace CONCEPTO With ""

	Append Blank
	Replace CONCEPTO With "Vientres Gestantes de 1er Servicio     ",COL1 With Str(R1(4),5),COL2 With Str(R1(24),5),COL3 With Str(R1(44),5),COL4 With Str(R1(64),5)
	Append Blank
	Replace CONCEPTO With "Vientres Gestantes de 2do Servicio     ",COL1 With Str(R1(5),5),COL2 With Str(R1(25),5),COL3 With Str(R1(45),5),COL4 With Str(R1(65),5)
	Append Blank
	Replace CONCEPTO With "Tasa de Concep. 1 Servicio (% HR)   ",COL1 With Str(R1(4)/R1(2)*100,5,1),COL2 With Str(R1(24)/R1(22)*100,5,1),COL3 With Str(R1(44)/R1(42)*100,5,1),COL4 With Str(R1(64)/R1(62)*100,5,1)
	Append Blank
	Replace CONCEPTO With "Tasa de Concep. 2 Servicio (% HR)   ",COL1 With Str(R1(5)/R1(2)*100,5,1),COL2 With Str(R1(25)/R1(22)*100,5,1),COL3 With Str(R1(45)/R1(42)*100,5,1),COL4 With Str(R1(65)/R1(62)*100,5,1)
	Append Blank
	Replace CONCEPTO With ""
	
	Append Blank
	Replace CONCEPTO With "Tasa de Concep. 1 Servicio (% VG)   ",COL1 With Str(R1(4)/R1(3)*100,5,1),COL2 With Str(R1(24)/R1(23)*100,5,1),COL3 With Str(R1(44)/R1(43)*100,5,1),COL4 With Str(R1(64)/R1(63)*100,5,1)
	Append Blank
	Replace CONCEPTO With "Tasa de Concep. 2 Servicio (% VG)   ",COL1 With Str(R1(5)/(R1(3)-R1(4))*100,5,1),COL2 With Str(R1(25)/(R1(23)-R1(24))*100,5,1),COL3 With Str(R1(45)/(R1(43)-R1(44))*100,5,1),COL4 With Str(R1(65)/(R1(63)-R1(64))*100,5,1)
	Append Blank
	Replace CONCEPTO With ""

	Append Blank
	Replace CONCEPTO With "Vientres que salieron del Hato         ",COL1 With Str(R1(91),5),COL2 With Str(R1(92),5),COL3 With Str(R1(93),5),COL4 With Str(R1(94),5)
	Append Blank
	Replace CONCEPTO With "Vientres que entraron al Hato          ",COL1 With Str(R1(6),5),COL2 With Str(R1(26),5),COL3 With Str(R1(46),5),COL4 With Str(R1(6),5)
	Append Blank
	Replace CONCEPTO With ""

	Append Blank
	Replace CONCEPTO With "% de Vientres que salieron del Hato    ",COL1 With Str(R1(91)/(R1(1)+R1(91))*100,5,1),COL2 With Str(R1(92)/(R1(21)+R1(92))*100,5,1),COL3 With Str(R1(93)/(R1(41)+R1(93))*100,5,1),COL4 With Str(R1(94)/(R1(61)+R1(94))*100,5,1)
	Append Blank
	Replace CONCEPTO With "% de Vientres que entraron al Hato     ",COL1 With Str(R1(6)/(R1(61)+R1(94))*100,5,1),COL2 With Str(R1(26)/(R1(21)+R1(92))*100,5,1),COL3 With Str(R1(46)/(R1(41)+R1(93))*100,5,1),COL4 With Str(R1(6)/(R1(61)+R1(94))*100,5,1)
	Append Blank
	Replace CONCEPTO With ""

	Append Blank
	Replace CONCEPTO With "Vientres Retrasadas (Total del Hato)   ",COL1 With Str(R1(7),5),COL2 With Str(R1(27),5),COL3 With Str(R1(47),5),COL4 With Str(R1(67),5)
	Append Blank
	Replace CONCEPTO With "Vientres Problema   (Total del Hato)   ",COL1 With Str(R1(8),5),COL2 With Str(R1(28),5),COL3 With Str(R1(48),5),COL4 With Str(R1(68),5)
	Append Blank
	Replace CONCEPTO With "Conf. Gestantes de<13 m I/P Proy.   ",COL1 With Str(R1(9),5),COL2 With Str(R1(29),5),COL3 With Str(R1(49),5),COL4 With Str(R1(69),5)
	Append Blank
	Replace CONCEPTO With ""

	Append Blank
	Replace CONCEPTO With "Vientres Retrasadas (% HR)             ",COL1 With Str(R1(7)/R1(2)*100,5,1),COL2 With Str(R1(27)/R1(22)*100,5,1),COL3 With Str(R1(47)/R1(42)*100,5,1),COL4 With Str(R1(67)/R1(62)*100,5,1)
	Append Blank
	Replace CONCEPTO With "Vientres Problema   (% HR)             ",COL1 With Str(R1(8)/R1(2)*100,5,1),COL2 With Str(R1(28)/R1(22)*100,5,1),COL3 With Str(R1(48)/R1(42)*100,5,1),COL4 With Str(R1(68)/R1(62)*100,5,1)
	Append Blank
	Replace CONCEPTO With "% Conf. Gestantes de<13 m I/P Proy. ",COL1 With Str(R1(9)/R1(3)*100,5,1),COL2 With Str(R1(29)/R1(23)*100,5,1),COL3 With Str(R1(49)/R1(43)*100,5,1),COL4 With Str(R1(69)/R1(63)*100,5,1)
	Append Blank
	Replace CONCEPTO With ""

	Append Blank
	Replace CONCEPTO With "Total # de Servicios (Hato Reprod.) ",COL1 With Str(R1(10),5),COL2 With Str(R1(30),5),COL3 With Str(R1(50),5),COL4 With Str(R1(70),5)
	Append Blank
	Replace CONCEPTO With "Total # de Servicios (Confirmadas)  ",COL1 With Str(R1(11),5),COL2 With Str(R1(31),5),COL3 With Str(R1(51),5),COL4 With Str(R1(71),5)
	Append Blank
	Replace CONCEPTO With ""

	Append Blank
	Replace CONCEPTO With "Servicios x Concepcion (HR)         ",COL1 With Str(R1(10)/AW(1),5,1),COL2 With Str(R1(30)/AW(2),5,1),COL3 With Str(R1(50)/AW(3),5,1),COL4 With Str(R1(70)/AW(4),5,1)
	Append Blank
	Replace CONCEPTO With "Servicios x Concepcion (VG)         ",COL1 With Str(R1(11)/R1(3),5,1),COL2 With Str(R1(31)/R1(23),5,1),COL3 With Str(R1(51)/R1(43),5,1),COL4 With Str(R1(71)/R1(63),5,1)
	Append Blank
	Replace CONCEPTO With ""

	Append Blank
	Replace CONCEPTO With "Tasa de Concepcion % (HR)           ",COL1 With Str(1/(R1(10)/AW(1))*100,5,1),COL2 With Str(1/(R1(30)/AW(2))*100,5,1),COL3 With Str(1/(R1(50)/AW(3))*100,5,1),COL4 With Str(1/(R1(70)/AW(4))*100,5,1)
	Append Blank
	Replace CONCEPTO With "Tasa de Concepcion % (VG)           ",COL1 With Str(1/(R1(11)/R1(3))*100,5,1),COL2 With Str(1/(R1(31)/R1(23))*100,5,1),COL3 With Str(1/(R1(51)/R1(43))*100,5,1),COL4 With Str(1/(R1(71)/R1(63))*100,5,1)
	Append Blank
	Replace CONCEPTO With ""

	Append Blank
	Replace CONCEPTO With "Dias a Concepcion (VG)              ",COL1 With Str(R1(107)/R1(3),5,1),COL2 With Str(R1(108)/R1(23),5,1),COL3 With Str(R1(109)/R1(43),5,1),COL4 With Str(R1(110)/R1(63),5,1)
	Append Blank
	Replace CONCEPTO With "Promedio de Dias a 1er Servicio     ",COL1 With Str(R1(12)/R1(101),5,1),COL2 With Str(R1(32)/R1(102),5,1),COL3 With Str(R1(52)/R1(103),5,1),COL4 With Str(R1(72)/R1(104),5,1)
	Append Blank
	Replace CONCEPTO With "Promedio de Dias Abiertos (HR)      ",COL1 With Str(R1(81),5,1),COL2 With Str(R1(82),5,1),COL3 With Str(R1(83),5,1),COL4 With Str(R1(84),5,1)
	Append Blank
	Replace CONCEPTO With "Dias Abiertos (Sin Contar Abortos)  ",COL1 With Str(AW(11),5,1),COL2 With Str(AW(12),5,1),COL3 With Str(AW(13),5,1),COL4 With Str(AW(14),5,1)
	Append Blank
	Replace CONCEPTO With ""

	Append Blank
	Replace CONCEPTO With "Intervalo entre Partos (Previo)     ",COL1 With Str((R1(14)/R1(1))/30.4,5,1),COL2 With Str((R1(34)/R1(96))/30.4,5,1),COL3 With Str((R1(54)/R1(97))/30.4,5,1),COL4 With Str((R1(74)/R1(85))/30.4,5,1)
	Append Blank
	Replace CONCEPTO With "Intervalo entre Partos (Proyec)     ",COL1 With Str((R1(81)+Q39)/30.4,5,1),COL2 With Str((R1(82)+Q39)/30.4,5,1),COL3 With Str((R1(83)+Q39)/30.4,5,1),COL4 With Str((R1(84)+Q39)/30.4,5,1)
	Append Blank
	Replace CONCEPTO With ""

	Append Blank
	Replace CONCEPTO With "Vientres Gestantes (%)"
	Append Blank
	Replace CONCEPTO With "Intervalo/Partos Proyectado (Meses) ",COL1 With "  <12",COL2 With "12-13",COL3 With "13-14",COL4 With "  >14"
	Append Blank
	Replace CONCEPTO With '-'
	Append Blank
	Replace CONCEPTO With "Lactancia 1                         ",COL1 With Str(R1(17)/R1(3)*100,5),COL2 With Str(R1(18)/R1(3)*100,5),COL3 With Str(R1(19)/R1(3)*100,5),COL4 With Str(R1(20)/R1(3)*100,5)
	Append Blank
	Replace CONCEPTO With "Lactancia 2                         ",COL1 With Str(R1(37)/R1(23)*100,5),COL2 With Str(R1(38)/R1(23)*100,5),COL3 With Str(R1(39)/R1(23)*100,5),COL4 With Str(R1(40)/R1(23)*100,5)
	Append Blank
	Replace CONCEPTO With "Lactancia 3+                        ",COL1 With Str(R1(57)/R1(43)*100,5),COL2 With Str(R1(58)/R1(43)*100,5),COL3 With Str(R1(59)/R1(43)*100,5),COL4 With Str(R1(60)/R1(43)*100,5)
	Append Blank
	Replace CONCEPTO With "Todas                               ",COL1 With Str(R1(77)/R1(63)*100,5),COL2 With Str(R1(78)/R1(63)*100,5),COL3 With Str(R1(79)/R1(63)*100,5),COL4 With Str(R1(80)/R1(63)*100,5)
	
GO TOP
RETURN


* RESUMEN DE CAPTURA DIARIA
* -------------------------
PROCEDURE RV10225

CREATE TABLE RESUMEN (id N(5),fecha D(10),xnp N(2),evento N(2),cevento C(12),;
stat C(5),idt C(9),utec C(3),enf N(2),obs C(6),sexc C(2),c1 N(5),c2 N(5),;
trat C(14),mot2 C(6),cau2 N(2),med N(3))

Select REG
Set Order TO 2
Scan
	Select RESUMEN
  Do case
  	Case REG.FPAR=xCB1
  	 	Appe blank
   	 	repl id with REG.id,fecha with REG.fpar,xnp with REG.np,evento with 1;
   	 	cevento with "Parto",stat with REG.stat,utec with REG.utec,;
   	 	c1 with REG.idc,c2 with REG.idc2,obs with REG.obs,sexc with REG.sexc,;
   	 	idt with REG.idpc,med with REG.med1

 		Case REG.FSEC=xCB1  
  	 	Appe blank
     	Repl id with REG.id,fecha with REG.fsec,xnp with REG.np,evento with 7; 	
     	cevento with "Secado",stat with REG.stat,utec with REG.utec 	

 		Case REG.FB2=xCB1  
  	 appe blank
     repl id with REG.id,fecha with REG.fb2,xnp with REG.np,evento with 15; 	
     cevento with "Baja",stat with REG.stat,mot2 with REG.mot2,cau2 with REG.cau2
	 Endcase
Select REG
Endscan


Select CALOR
Set Order To 2
Scan for fecha=xCB1
Select RESUMEN
Do case
 	Case CALOR.fecha=xCB1  and CALOR.stat="LIMPI" and CALOR.clave=2
  	 appe blank
     repl id with CALOR.id,fecha with CALOR.fecha,xnp with CALOR.np,evento with 2;
     cevento with "Revisi¢n",stat with CALOR.stat,utec with CALOR.tec 	

 	Case CALOR.fecha=xCB1  and CALOR.stat="SUCIA" and CALOR.clave=2
  	 appe blank
     repl id with CALOR.id,fecha with CALOR.fecha,xnp with CALOR.np,evento with 2;
     cevento with "Revisi¢n",stat with CALOR.stat,utec with CALOR.tec,med with CALOR.med 	

 	Case CALOR.fecha=xCB1  and CALOR.stat="TRATA" and CALOR.clave=3
  	 appe blank
     repl id with CALOR.id,fecha with CALOR.fecha,xnp with CALOR.np,evento with 3; 	
     cevento with "Tratamiento",stat with CALOR.stat,utec with CALOR.tec,med with CALOR.med 	

 	Case CALOR.fecha=xCB1  and (CALOR.stat="LIMPI" or CALOR.stat="SUCIA") and CALOR.clave=4
  	 appe blank
     repl id with CALOR.id,fecha with CALOR.fecha,xnp with CALOR.np,evento with 4; 	
     cevento with "Celo",stat with CALOR.stat,utec with CALOR.tec 	
		if CALOR.stat="SUCIA"
			repl med with CALOR.med
		endif	
	
 	Case CALOR.fecha=xCB1  and CALOR.stat="INSEM" and CALOR.clave=5
  	 appe blank
     repl id with CALOR.id,fecha with CALOR.fecha,xnp with CALOR.np,evento with 5; 	
     cevento with "Inseminaci¢n",stat with CALOR.stat,utec with CALOR.tec,idt with CALOR.toro 	
 
 	Case CALOR.fecha=xCB1  and (CALOR.stat="CARGA" or CALOR.stat="VACIA") and CALOR.clave=6
  	 appe blank
     repl id with CALOR.id,fecha with CALOR.fecha,xnp with CALOR.np,evento with 6;
     cevento with "Palpaci¢n",stat with CALOR.stat,utec with CALOR.tecp 	

 	Case CALOR.fecha=xCB1 and CALOR.stat="ABORT"  
  	 appe blank
     repl id with CALOR.id,fecha with CALOR.fecha,xnp with CALOR.np,evento with 9; 	
     cevento with "Aborto",stat with CALOR.stat 	

	Otherwise
Endcase
Select CALOR
Endscan


Select CLIN
Set Order To 2

Scan For Fecha=xCB1
	Select RESUMEN
 		Appe blank
 		Repl id with CLIN.id,fecha with CLIN.fecha,xnp with CLIN.np,evento with 8; 	
 		cevento with "Hospital",stat with CLIN.stat,enf with CLIN.enf,;
 		trat with CLIN.trat,med with CLIN.mnum,utec with CLIN.curo
 Select CLIN
Endscan
RETURN


* RESUMEN DE CAPTURA DIARIA (CRIANZA)
* -------------------------
PROCEDURE RV10226
CREATE TABLE RESUMEN (id N(5),fecha D(8),xnp N(2),evento N(2),cevento C(12),;
stat C(5),idt C(9),utec C(3),enf N(2),trat C(14),mot2 C(6),cau2 N(2),;
med N(3),madre N(5),padre C(10),peso N(3),esta N(3))

	Select REG
	Set Order To 3
	Scan
 		Select RESUMEN
  	Do case
  		Case REG.FNAC=xCB1
  	 		Appe blank
   	 		Repl id with REG.id,fecha with REG.fnac,xnp with REG.np,evento with 14;
   	 		cevento with "Nacimiento",peso with REG.pnac,esta with REG.enac,;
   	 		madre with REG.idm,padre with REG.idp 

  		Case REG.DEST=xCB1
  	 		Appe Blank
   	 		Repl id with REG.id,fecha with REG.dest,xnp with REG.np,evento with 12;
   	 		cevento with "Destete",peso with REG.pdes,esta with REG.edes
 		Endcase
		Select REG
	EndScan


	Select CALOR
	Set Order To 3
	Scan for FECHA=xCB1
		Select RESUMEN
		Appe blank
		Repl id with CALOR.id,fecha with CALOR.fecha,xnp with CALOR.np,med with CALOR.med

		Do case
 			Case CALOR.fecha=xCB1 and CALOR.stat="LIMPI" and CALOR.clave=2
     		Repl evento with 2,cevento with "Revisi¢n",stat with CALOR.stat,utec with CALOR.tec 	

 			Case CALOR.fecha=xCB1 and CALOR.stat="SUCIA" and CALOR.clave=2
     		Repl evento with 2,cevento with "Revisi¢n",stat with CALOR.stat,utec with CALOR.tec,med with CALOR.med 	

 			Case CALOR.fecha=xCB1 and CALOR.stat="TRATA" and CALOR.clave=3
     		Repl evento with 3,cevento with "Tratamiento",stat with CALOR.stat,utec with CALOR.tec,med with CALOR.med 	

 			Case CALOR.fecha=xCB1 and (CALOR.stat="LIMPI" or CALOR.stat="SUCIA") and CALOR.clave=4
     		Repl evento with 4,cevento with "Celo",stat with CALOR.stat,utec with CALOR.tec 	
					If CALOR.stat="SUCIA"
						Repl med with CALOR.med
					Endif	

		 	Case CALOR.fecha=xCB1 and CALOR.stat="INSEM" and CALOR.clave=5
     		Repl evento with 5,cevento with "Inseminaci¢n",stat with CALOR.stat,utec with CALOR.tec,idt with CALOR.toro 	
 
 			Case CALOR.fecha=xCB1 and (CALOR.stat="CARGA" or CALOR.stat="VACIA")
     		Repl evento with 6,cevento with "Palpaci¢n",stat with CALOR.stat,utec with CALOR.tecp 	

 			Case CALOR.fecha=xCB1 and CALOR.stat="ABORT"  and CALOR.clave=9
     		Repl evento with 9,cevento with "Aborto",stat with CALOR.stat 	
	
			Otherwise
				Repl evento with 0,cevento with "Otros"
		Endcase
		Select CALOR
	Endscan


	Select CLIN 
	Set Order To 3
	Scan For FECHA=xCB1
		Select RESUMEN
 		Appe blank
 		Repl id with CLIN.id,fecha with CLIN.fecha,xnp with CLIN.np,evento with 8; 	
 		cevento with "Hospital",stat with CLIN.stat,enf with CLIN.enf,trat with CLIN.trat
 	Select CLIN
	Endscan

	Select BAJAS
	Set Order To 1
	Scan For FECHA=xCB1
		Select RESUMEN
 		Appe blank
 		Repl id with BAJAS.id,fecha with BAJAS.fecha,xnp with BAJAS.np,evento with 15; 	
 		cevento with "Baja",mot2 with BAJAS.mot,cau2 with BAJAS.causa
 		Select BAJAS
	Endscan

	Select RESUMEN

RETURN



