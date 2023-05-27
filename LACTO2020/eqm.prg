************************************************************
*                         SEL                              *
************************************************************

*PROCEDURE SEL
do case
case month(FPAR)=1
do fac1
case month(FPAR)=2
do fac2
case month(FPAR)=3
do fac3
case month(FPAR)=4
do fac4
case month(FPAR)=5
do fac5
case month(FPAR)=6
do fac6
case month(FPAR)=7
do fac7
case month(FPAR)=8
do fac8
case month(FPAR)=9
do fac9
case month(FPAR)=10
do fac10
case month(FPAR)=11
do fac11
case month(FPAR)=12
do fac12
endcase
retu


************************************************************
*                         FAC1                             *
************************************************************

PROCEDURE FAC1
do case
case edad<=18
repl fac with 1.42
case edad=19 
repl fac with 1.39
case edad=20
repl fac with 1.36
case edad=21
repl fac with 1.34
case edad=22
repl fac with 1.31
case edad=23
repl fac with 1.29
case edad=24
repl fac with 1.27
case edad=25
repl fac with 1.25
case edad=26
repl fac with 1.24
case edad=27 .or. edad>200
repl fac with 1.23
case edad=28
repl fac with 1.22
case edad=29
repl fac with 1.21
case edad=30 .or. edad=31 .or. edad>=190 .and. edad<=199
repl fac with 1.20
case edad=32
repl fac with 1.19
case edad=33
repl fac with 1.18
case edad=34
repl fac with 1.17
case edad=35 .or. edad>=180 .and. edad<=189
repl fac with 1.16
case edad=36
repl fac with 1.15
case edad=37
repl fac with 1.14
case edad=38 .or. edad>=170 .and. edad<=179
repl fac with 1.13
case edad=39
repl fac with 1.11
case edad=40 .or. edad>=160 .and. edad<=169
repl fac with 1.10
case edad=41
repl fac with 1.09
case edad=42 .or. edad=43
repl fac with 1.08
case edad=44 .or. edad=45 .or. edad>=150 .and. edad<=159
repl fac with 1.07
case edad=46 .or. edad=47
repl fac with 1.06
case edad=48 .or. edad=49
repl fac with 1.05
case edad=50 .or. edad=51 .or. edad>=140 .and. edad<=149
repl fac with 1.04
case edad=52 .or. edad=53 .or. edad>=135 .and. edad<=139
repl fac with 1.03
case edad=54 .or. edad=55 .or. edad=56 .or. edad>=130 .and. edad<=134
repl fac with 1.02
case edad=57 .or. edad=58 .or. edad=59 .or.edad>=125 .and. edad<=129
repl fac with 1.01
case edad=60 .or. edad=61 .or. edad=62 .or. edad>=120 .and. edad<=124
repl fac with 1.00
case edad>62 .and. edad<67 .or. edad>111 .and. edad<=119
repl fac with 0.99
case edad>66 .and. edad<74 .or. edad>103 .and. edad<112
repl fac with 0.98
case edad>73 .and. edad<104
repl fac with 0.97
endcase
retu


************************************************************
*                         FAC2                             *
************************************************************

PROCEDURE FAC2
do case
case edad<=18
repl fac with 1.43
case edad=19 
repl fac with 1.41
case edad=20
repl fac with 1.38
case edad=21
repl fac with 1.35
case edad=22
repl fac with 1.33
case edad=23
repl fac with 1.31
case edad=24
repl fac with 1.29
case edad=25
repl fac with 1.27
case edad=26
repl fac with 1.25
case edad=27
repl fac with 1.24
case edad=28
repl fac with 1.23
case edad=29 .or. edad>=200
repl fac with 1.22
case edad=30 
repl fac with 1.21
case edad=31
repl fac with 1.20
case edad=32 .or. edad=33 .or. edad>=190 .and. edad<=199
repl fac with 1.19
case edad=34
repl fac with 1.18
case edad=35
repl fac with 1.16
case edad=36 .or. edad>=180 .and. edad<=189
repl fac with 1.15
case edad=37
repl fac with 1.14
case edad=38 .or. edad>=170 .and. edad<=179
repl fac with 1.12
case edad=39
repl fac with 1.11
case edad=40 .or. edad>=160 .and. edad<=169
repl fac with 1.09
case edad=41 .or. edad=42
repl fac with 1.08
case edad=43
repl fac with 1.07
case edad=44 .or. edad=45 .or. edad>=150 .and. edad<=159
repl fac with 1.06
case edad=46 
repl fac with 1.05
case edad=47 .or. edad=48
repl fac with 1.04
case edad>=49 .and. edad<=51 .or. edad>=140 .and. edad<=149 
repl fac with 1.03
case edad=52 .or. edad=53 .or. edad>=135 .and. edad<=139
repl fac with 1.02
case edad=54 .or. edad=55 .or. edad>=130 .and. edad<=134
repl fac with 1.01
case edad>=56 .and. edad<=59 .or. edad>=125 .and. edad<=129
repl fac with 1.00
case edad>=60 .and. edad<=62 .or. edad>=120 .and. edad<=124
repl fac with 0.99
case edad>=63 .and. edad<=66 .or. edad>=111 .and. edad<=119
repl fac with 0.98
case edad>=67 .and. edad<=74 .or. edad>=100 .and. edad<=110
repl fac with 0.97
case edad>=75 .and. edad<=99
repl fac with 0.96
endcase
retu


************************************************************
*                         FAC3                             *
************************************************************

PROCEDURE FAC3
do case
case edad<=18
repl fac with 1.43
case edad=19 
repl fac with 1.40
case edad=20
repl fac with 1.37
case edad=21
repl fac with 1.35
case edad=22
repl fac with 1.32
case edad=23
repl fac with 1.30
case edad=24
repl fac with 1.28
case edad=25
repl fac with 1.26
case edad=26
repl fac with 1.25
case edad=27 .or. edad>=200
repl fac with 1.23
case edad=28
repl fac with 1.22
case edad=29
repl fac with 1.21
case edad=30 
repl fac with 1.20
case edad=31 .or. edad=32 .or. edad>=190 .and. edad<=199
repl fac with 1.19
case edad=33
repl fac with 1.18
case edad=34
repl fac with 1.17
case edad=35 .or. edad>=180 .and. edad<=189
repl fac with 1.16
case edad=36
repl fac with 1.15
case edad=37 .or. edad>=170 .and. edad<=179
repl fac with 1.13
case edad=38
repl fac with 1.12
case edad=39
repl fac with 1.11
case edad>=160 .and. edad<=169
repl fac with 1.10
case edad=40 .or. edad=41
repl fac with 1.09
case edad=42
repl fac with 1.08
case edad=43 .or. edad=44 .or. edad>=150 .and. edad<=159
repl fac with 1.07
case edad=45
repl fac with 1.06
case edad=46 .or. edad=47
repl fac with 1.05
case edad=48 .or. edad=49 .or. edad>=140 .and. edad<=149
repl fac with 1.04
case edad=50 .or. edad=51 .or. edad=52
repl fac with 1.03
case edad=53 .or. edad=54
repl fac with 1.02
case edad>=55 .and. edad<=57 .or. edad>=130 .and. edad<=134
repl fac with 1.01
case edad>=58 .and. edad<=60 .or. edad>=120 .and. edad<=129
repl fac with 1.00
case edad>=61 .and. edad<=64 .or. edad>=115 .and. edad<=119
repl fac with 0.99
case edad>=65 .and. edad<=70 .or. edad>=108 .and. edad<=114
repl fac with 0.98
case edad>=71 .and. edad<=79 .or. edad>=82 .and. edad<=107
repl fac with 0.97
case edad=80 .or. edad=81
repl fac with 0.96
endcase
retu


************************************************************
*                         FAC4                             *
************************************************************

PROCEDURE FAC4
do case
case edad<=18
repl fac with 1.43
case edad=19 
repl fac with 1.41
case edad=20
repl fac with 1.38
case edad=21
repl fac with 1.35
case edad=22
repl fac with 1.33
case edad=23
repl fac with 1.31
case edad=24
repl fac with 1.29
case edad=25
repl fac with 1.27
case edad=26 .or. edad>=200
repl fac with 1.25
case edad=27
repl fac with 1.24
case edad=28
repl fac with 1.23
case edad=29 .or. edad=30 .or. edad>=190 .and. edad<=199
repl fac with 1.21
case edad=31
repl fac with 1.20
case edad=32
repl fac with 1.19
case edad=33 .or. edad>=180 .and. edad<=189
repl fac with 1.18
case edad=34
repl fac with 1.17
case edad=35
repl fac with 1.16
case edad=36
repl fac with 1.15
case edad>=170 .and. edad<=179
repl fac with 1.14
case edad=37
repl fac with 1.13
case edad=38
repl fac with 1.12
case edad=39 .or. edad>=160 .and. edad<=169
repl fac with 1.11
case edad=40
repl fac with 1.10
case edad=41
repl fac with 1.09
case edad=42 .or. edad>=150 .and. edad<=159
repl fac with 1.08
case edad=43 .or. edad=44
repl fac with 1.07
case edad=45 .or. edad=46
repl fac with 1.06
case edad=47 .or. edad=48 .or. edad>=140 .and. edad<=149
repl fac with 1.05
case edad=49 .or. edad=50 .or. edad>=135 .and. edad<=139
repl fac with 1.04
case edad=51 .or. edad=52 .or. edad=53 .or. edad>=130 .and. edad<=134
repl fac with 1.03
case edad=54 .or. edad=55 .or. edad>=125 .and. edad<=129
repl fac with 1.02
case edad>55 .and.edad<60 .or. edad>=120 .and. edad<=124
repl fac with 1.01
case edad>=60 .and. edad <=64 .or. edad>=113 .and. edad<=119
repl fac with 1.00
case edad>=65 .and. edad<=71 .or. edad>=107 .and. edad<=112
repl fac with 0.99
case edad>=72 .and. edad<=106
repl fac with 0.98
endcase
retu


************************************************************
*                         FAC5                             *
************************************************************

PROCEDURE FAC5
do case
case edad<=18
repl fac with 1.44
case edad=19 
repl fac with 1.42
case edad=20
repl fac with 1.39
case edad=21
repl fac with 1.36
case edad=22
repl fac with 1.34
case edad=23
repl fac with 1.31
case edad=24
repl fac with 1.29
case edad=25
repl fac with 1.28
case edad>=200
repl fac with 1.27
case edad=26
repl fac with 1.26
case edad=27
repl fac with 1.25
case edad=28 .or. edad>=190 .and. edad<=199
repl fac with 1.23
case edad=29
repl fac with 1.22
case edad=30 .or. edad=31
repl fac with 1.21
case edad=32 .or. edad>=180 .and. edad<=189
repl fac with 1.20
case edad=33
repl fac with 1.19
case edad=34
repl fac with 1.18
case edad=35
repl fac with 1.17
case edad=36 .or. edad>=170 .and. edad<=179
repl fac with 1.16
case edad=37
repl fac with 1.15
case edad=38
repl fac with 1.14
case edad=39 .or. edad>=160 .and. edad<=169
repl fac with 1.13
case edad=40
repl fac with 1.12
case edad=41
repl fac with 1.11
case edad=42 .or. edad=43 .or. edad>=150 .and. edad<=159
repl fac with 1.10
case edad=44 
repl fac with 1.09
case edad=45 .or. edad=46
repl fac with 1.08
case edad=47 .or. edad=48 .or. edad>=140 .and. edad<=149
repl fac with 1.07
case edad>=49 .and. edad<=51 .or. edad>=135 .and. edad<=139
repl fac with 1.06
case edad=52 .or. edad=53
repl fac with 1.05
case edad=54 .or. edad=55 .or. edad>=130 .and. edad<=134
repl fac with 1.04
case edad>=56 .and. edad<=58 .or. edad>=125 .and. edad<=129
repl fac with 1.03
case edad>=59 .and. edad<=62 .or. edad>=120 .and. edad<=124
repl fac with 1.02
case edad>=63 .and. edad<=67 .or. edad>=110 .and. edad<=119
repl fac with 1.01
case edad>=68 .and. edad<=74 .or. edad>=96 .and. edad<=109
repl fac with 1.00
case edad>=75 .and. edad<=95
repl fac with 0.99
endcase
retu


************************************************************
*                         FAC6                             *
************************************************************

PROCEDURE FAC6
do case
case edad<=18
repl fac with 1.47
case edad=19 
repl fac with 1.44
case edad=20
repl fac with 1.41
case edad=21
repl fac with 1.38
case edad=22
repl fac with 1.36
case edad=23
repl fac with 1.33
case edad=24 .or. edad>=200
repl fac with 1.31
case edad=25
repl fac with 1.29
case edad=26
repl fac with 1.28
case edad>=190 .and. edad<=199
repl fac with 1.27
case edad=27
repl fac with 1.26
case edad=28 
repl fac with 1.25
case edad=29
repl fac with 1.24
case edad=30 .or. edad>=180 .and. edad<=189
repl fac with 1.23
case edad=31
repl fac with 1.22
case edad=32 .or. edad=33
repl fac with 1.21
case edad=34
repl fac with 1.20
case edad=35 .or. edad>=170 .and. edad<=179
repl fac with 1.19
case edad=36 
repl fac with 1.18
case edad=37
repl fac with 1.17
case edad=38 .or. edad>=160 .and. edad<=169
repl fac with 1.16
case edad=39 
repl fac with 1.15
case edad=40
repl fac with 1.14
case edad=41 .or. edad>=150 .and. edad<=159
repl fac with 1.13
case edad=42 .or. edad=43 
repl fac with 1.12
case edad=44 .or. edad=45
repl fac with 1.11
case edad=46 .or. edad=47
repl fac with 1.10
case edad=48 .or. edad=49 .or. edad>=140 .and. edad<=149
repl fac with 1.09
case edad>=50 .and. edad<=51 .or. edad>=135 .and. edad<=139
repl fac with 1.08
case edad=52 .or. edad=53 .or. edad>=130 .and. edad<=134
repl fac with 1.07
case edad=54 .or. edad=55 .or. edad>=125 .and. edad<=129
repl fac with 1.06
case edad>=56 .and. edad<=59 .or. edad>=120 .and. edad<=124
repl fac with 1.05
case edad>=60 .and. edad<=63 .or. edad>=115 .and. edad<=119
repl fac with 1.04
case edad>=64 .and. edad<=68 .or. edad>=109 .and. edad<=114
repl fac with 1.03
case edad>=69 .and. edad<=75 .or. edad>=92 .and. edad<=108
repl fac with 1.02
case edad>=76 .and. edad<=91
repl fac with 1.01
endcase
retu


************************************************************
*                         FAC7                             *
************************************************************

PROCEDURE FAC7
do case
case edad<=18
repl fac with 1.49
case edad=19 
repl fac with 1.46
case edad=20
repl fac with 1.43
case edad=21
repl fac with 1.40
case edad=22
repl fac with 1.38
case edad>=200
repl fac with 1.36
case edad=23
repl fac with 1.35
case edad=24
repl fac with 1.33
case edad>=190 .and. edad<=199
repl fac with 1.32
case edad=25
repl fac with 1.31
case edad=26
repl fac with 1.29
case edad=27 .or. edad>=180 .and. edad<=189
repl fac with 1.28
case edad=28
repl fac with 1.27
case edad=29
repl fac with 1.26
case edad=30
repl fac with 1.25
case edad>=31 .and. edad<=32 .or. edad>=170 .and. edad<=179
repl fac with 1.24 
case edad=33
repl fac with 1.23
case edad=34 .or. edad=35
repl fac with 1.22
case edad=36
repl fac with 1.21
case edad=37 .or. edad>=160 .and. edad<=169
repl fac with 1.20
case edad=38
repl fac with 1.19
case edad=39
repl fac with 1.18
case edad=40
repl fac with 1.17
case edad=41 .or. edad=42 .or. edad>=150 .and. edad<=159
repl fac with 1.16
case edad=43
repl fac with 1.15
case edad=44 .or. edad=45
repl fac with 1.14
case edad=46 .or. edad=47 .or. edad>=140 .and. edad<=149
repl fac with 1.13
case edad=48 .or. edad=49 .or. edad>=135 .and. edad<=139
repl fac with 1.12
case edad=50 .or. edad=51
repl fac with 1.11
case edad>=52 .and. edad<=53 .or. edad>=130 .and. edad<=134
repl fac with 1.10
case edad=54 .or. edad=55 .or. edad>=125 .and. edad<=129
repl fac with 1.09
case edad>=56 .and. edad<=59 .or. edad>=120 .and. edad<=124
repl fac with 1.08
case edad>=60 .and. edad<=63 .or. edad>=113 .and. edad<=119
repl fac with 1.07
case edad>=64 .and. edad<=70 .or. edad>=108 .and. edad<=112
repl fac with 1.06
case edad>=71 .and. edad<=76 .or. edad>=84 .and. edad<=107
repl fac with 1.05
case edad>=77 .and. edad<=83
repl fac with 1.04
endcase
retu


************************************************************
*                         FAC8                             *
************************************************************

PROCEDURE FAC8
do case
case edad<=18
repl fac with 1.50
case edad=19 
repl fac with 1.47
case edad=20
repl fac with 1.45
case edad=21
repl fac with 1.42
case edad=22
repl fac with 1.39
case edad=23
repl fac with 1.37
case edad>=200
repl fac with 1.36
case edad=24
repl fac with 1.34
case edad=25 .or. edad>=190 .and. edad<=199
repl fac with 1.32
case edad=26
repl fac with 1.31
case edad=27
repl fac with 1.29
case edad=28 .or. edad>=180 .and. edad<=189
repl fac with 1.28
case edad=29
repl fac with 1.27
case edad=30 .or. edad=31
repl fac with 1.26
case edad=32
repl fac with 1.25
case edad=33 .or. edad=34 .or. edad>=170 .and. edad<=179
repl fac with 1.24
case edad=35
repl fac with 1.23
case edad=36
repl fac with 1.22
case edad=37
repl fac with 1.21
case edad=38 .or. edad>=160 .and. edad<=169
repl fac with 1.20
case edad=39
repl fac with 1.19
case edad=40 .or. edad=41
repl fac with 1.18
case edad=42 .or. edad>=150 .and. edad<=159
repl fac with 1.17
case edad=43 .or. edad=44
repl fac with 1.16
case edad=45 .or. edad=46
repl fac with 1.15
case edad=47 .or. edad>=140 .and. edad<=149
repl fac with 1.14
case edad=48 .or. edad=49
repl fac with 1.13
case edad=50 .or. edad=51 .or. edad>=135 .and. edad<=139
repl fac with 1.12
case edad>=52 .and. edad<=53
repl fac with 1.11
case edad=54 .or. edad=55 .or. edad>=130 .and. edad<=134
repl fac with 1.10
case edad>=56 .and. edad<=59 .or. edad>=125 .and. edad<=129
repl fac with 1.09
case edad>=60 .and. edad<=62 .or. edad>=120 .and. edad<=124
repl fac with 1.08
case edad>=63 .and. edad<=66 .or. edad>=111 .and. edad<=119
repl fac with 1.07
case edad>=67 .and. edad<=72 .or. edad>=105 .and. edad<=110
repl fac with 1.06
case edad>=73 .and. edad<=104
repl fac with 1.05
endcase
retu


************************************************************
*                         FAC9                             *
************************************************************

PROCEDURE FAC9
do case
case edad<=18
repl fac with 1.48
case edad=19 
repl fac with 1.45
case edad=20
repl fac with 1.42
case edad=21
repl fac with 1.40
case edad=22
repl fac with 1.37
case edad=23 .or. edad>=200
repl fac with 1.34
case edad=24
repl fac with 1.32
case edad=25
repl fac with 1.30
case edad=26 .or. edad>=190 .and. edad<=199
repl fac with 1.29
case edad=27
repl fac with 1.28
case edad=28 
repl fac with 1.27
case edad=29
repl fac with 1.26
case edad=30 .or. edad=31 .or. edad>=180 .and. edad<=189
repl fac with 1.25
case edad=32 
repl fac with 1.24
case edad=33
repl fac with 1.23
case edad=34 .or. edad=35 .or. edad>=170 .and. edad<=179
repl fac with 1.22
case edad=36
repl fac with 1.21
case edad=37
repl fac with 1.20
case edad=38
repl fac with 1.19
case edad=39 .or. edad>=160 .and. edad<=169
repl fac with 1.18
case edad=40
repl fac with 1.17
case edad=41
repl fac with 1.16
case edad=42 .or. edad=43 .or. edad>=150 .and. edad<=159
repl fac with 1.15
case edad=44
repl fac with 1.14
case edad=45 .or. edad=46
repl fac with 1.13
case edad=47 .or. edad=48 .or. edad>=140 .and. edad<=149
repl fac with 1.12
case edad=49 .or. edad=50
repl fac with 1.11
case edad>=51 .and. edad<=52 .or. edad>=135 .and. edad<=139
repl fac with 1.10
case edad=53 .or. edad=54 .or. edad>=130 .and. edad<=134
repl fac with 1.09
case edad=55 .or. edad=56
repl fac with 1.08
case edad>=57 .and. edad<=59 .or. edad>=125 .and. edad<=129
repl fac with 1.07
case edad>=60 .and. edad<=62 .or. edad>=115 .and. edad<=124
repl fac with 1.06
case edad>=63 .and. edad<=67 .or. edad>=110 .and. edad<=114
repl fac with 1.05
case edad>=68 .and. edad<=74 .or. edad>=100 .and. edad<=109
repl fac with 1.04
case edad>=75 .and. edad<=99
repl fac with 1.03
endcase
retu


************************************************************
*                         FAC10                            *
************************************************************

PROCEDURE FAC10
do case
case edad<=18
repl fac with 1.46
case edad=19 
repl fac with 1.44
case edad=20
repl fac with 1.41
case edad=21
repl fac with 1.38
case edad=22
repl fac with 1.35
case edad=23
repl fac with 1.33
case edad=24 .or. edad>=200
repl fac with 1.31
case edad=25
repl fac with 1.29
case edad=26
repl fac with 1.28
case edad=27 .or. edad>=190 .and. edad<=199
repl fac with 1.27
case edad=28 
repl fac with 1.26
case edad=29
repl fac with 1.25
case edad=30 .or. edad=31
repl fac with 1.24
case edad=32 .or. edad>=180 .and. edad<=189
repl fac with 1.23
case edad=33
repl fac with 1.22
case edad=34
repl fac with 1.21
case edad=35
repl fac with 1.20
case edad=36 .or. edad>=170 .and. edad<=179
repl fac with 1.19
case edad=37
repl fac with 1.18
case edad=38
repl fac with 1.17
case edad=39 .or. edad>=160 .and. edad<=169
repl fac with 1.16
case edad=40
repl fac with 1.15
case edad=41
repl fac with 1.14
case edad=42 .or. edad>=150 .and. edad<=159
repl fac with 1.13
case edad=43 .or. edad=44
repl fac with 1.12
case edad=45 
repl fac with 1.11
case edad=46 .or. edad=47 .or. edad>=140 .and. edad<=149
repl fac with 1.10
case edad>=48 .and. edad<=49
repl fac with 1.09
case edad=50 .or. edad=51 .or. edad>=135 .and. edad<=139
repl fac with 1.08
case edad=52 .or. edad=53 .or. edad>=130 .and. edad<=134
repl fac with 1.07
case edad>=54 .and. edad<=56 .or. edad>=125 .and. edad<=129
repl fac with 1.06
case edad>=57 .and. edad<=59 .or. edad>=120 .and. edad<=124
repl fac with 1.05
case edad>=60 .and. edad<=63 .or. edad>=115 .and. edad<=119
repl fac with 1.04
case edad>=64 .and. edad<=69 .or. edad>=109 .and. edad<=114
repl fac with 1.03
case edad>=70 .and. edad<=76 .or. edad>=88 .and. edad<=108
repl fac with 1.02
case edad>=77 .and. edad<=87
repl fac with 1.01
endcase
retu


************************************************************
*                         FAC11                            *
************************************************************

PROCEDURE FAC11
do case
case edad<=18
repl fac with 1.45
case edad=19 
repl fac with 1.42
case edad=20
repl fac with 1.40
case edad=21
repl fac with 1.37
case edad=22
repl fac with 1.35
case edad=23
repl fac with 1.32
case edad=24
repl fac with 1.30
case edad=25 .or. edad>=200
repl fac with 1.28
case edad=26
repl fac with 1.27
case edad=27
repl fac with 1.26
case edad=28 
repl fac with 1.25
case edad=29 .or. edad=30 .or. edad>=190 .and. edad<=199
repl fac with 1.24
case edad=31
repl fac with 1.23
case edad=32 .or. edad=33
repl fac with 1.22
case edad=34 .or. edad>=180 .and. edad<=189
repl fac with 1.21
case edad=35
repl fac with 1.20
case edad=36
repl fac with 1.19
case edad=37 .or. edad>=170 .and. edad<=179
repl fac with 1.17
case edad=38
repl fac with 1.16
case edad=39
repl fac with 1.15
case edad=40 .or. edad>=160 .and. edad<=169
repl fac with 1.14
case edad=41
repl fac with 1.13
case edad=42
repl fac with 1.12
case edad=43 .or. edad=44 .or. edad>=150 .and. edad<=159
repl fac with 1.11
case edad=45 .or. edad=46
repl fac with 1.10
case edad=47 .or. edad=48
repl fac with 1.09
case edad=49 .or. edad=50 .or. edad>=140 .and. edad<=149
repl fac with 1.08
case edad>=51 .and. edad<=52
repl fac with 1.07
case edad=53 .or. edad=54 .or. edad>=135 .and. edad<=139
repl fac with 1.06
case edad=55 .or. edad=56 .or. edad>=130 .and. edad<=134
repl fac with 1.05
case edad>=57 .and. edad<=59 .or. edad>=125 .and. edad<=129
repl fac with 1.04
case edad>=60 .and. edad<=62 .or. edad>=120 .and. edad<=124
repl fac with 1.03
case edad>=63 .and. edad<=65 .or. edad>=112 .and. edad<=119
repl fac with 1.02
case edad>=66 .and. edad<=72 .or. edad>=106 .and. edad<=111
repl fac with 1.01
case edad>=73 .and. edad<=105
repl fac with 1.00
endcase
retu


************************************************************
*                         FAC12                            *
************************************************************

PROCEDURE FAC12
do case
case edad<=18
repl fac with 1.43
case edad=19 
repl fac with 1.40
case edad=20
repl fac with 1.38
case edad=21
repl fac with 1.35
case edad=22
repl fac with 1.33
case edad=23
repl fac with 1.30
case edad=24 .or. edad>=200
repl fac with 1.28
case edad=25
repl fac with 1.27
case edad=26
repl fac with 1.25
case edad=27 .or. edad>=190 .and. edad<=199
repl fac with 1.24
case edad=28 .or. edad=29
repl fac with 1.23
case edad=30
repl fac with 1.22
case edad=31 .or. edad=32
repl fac with 1.21
case edad=33 .or. edad>=180 .and. edad<=189
repl fac with 1.20
case edad=34
repl fac with 1.19
case edad=35
repl fac with 1.18
case edad=36 .or. edad>=170 .and. edad<=179
repl fac with 1.17
case edad=37
repl fac with 1.16
case edad=38 .or. edad>=160 .and. edad<=169
repl fac with 1.14  
case edad=39
repl fac with 1.13
case edad=40
repl fac with 1.12
case edad=41
repl fac with 1.11
case edad=42 .or. edad=43 .or. edad>=150 .and. edad<=159
repl fac with 1.10
case edad=44 .or. edad=45
repl fac with 1.09
case edad=46 .or. edad>=140 .and. edad<=149
repl fac with 1.08
case edad=47 .or. edad=48 .or. edad=49
repl fac with 1.07
case edad=50 .or. edad=51 .or. edad>=135 .and. edad<=139
repl fac with 1.06
case edad>=52 .and. edad<=53 .or. edad>=130 .and. edad<=134
repl fac with 1.05
case edad>=54 .AND. edad<=56 .or. edad>=125 .and. edad<=129
repl fac with 1.04
case edad>=57 .AND. edad<=59 .or. edad>=120 .and. edad<=124
repl fac with 1.03
case edad>=60 .and. edad<=63 .or. edad>=113 .and. edad<=119
repl fac with 1.02
case edad>=64 .and. edad<=70 .or. edad>=108 .and. edad<=112
repl fac with 1.01
case edad>=71 .and. edad<=107
repl fac with 1.00
endcase
retu
