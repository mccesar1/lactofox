do case
case dpr>=15 .and. dpr<20
repl p305 with Qpac*14.83
case dpr>=20 .and. dpr<30
repl p305 with Qpac*11.12
case dpr>=30 .and. dpr<40
repl p305 with Qpac*7.42
case dpr>=40 .and. dpr<50
repl p305 with Qpac*5.57
case dpr>=50 .and. dpr<60
repl p305 with Qpac*4.47
case dpr>=60 .and. dpr<70
repl p305 with Qpac*3.74
case dpr>=70 .and. dpr<80
repl p305 with Qpac*3.23
case dpr>=80 .and. dpr<90
repl p305 with Qpac*2.85
case dpr>=90 .and. dpr<100
repl p305 with Qpac*2.56
case dpr>=100 .and. dpr<110
repl p305 with Qpac*2.32
case dpr>=110 .and. dpr<120
repl p305 with Qpac*2.13
case dpr>=120 .and. dpr<130
repl p305 with Qpac*1.98
case dpr>=130 .and. dpr<140
repl p305 with Qpac*1.85
case dpr>=140 .and. dpr<150
repl p305 with Qpac*1.73
case dpr>=150 .and. dpr<160
repl p305 with Qpac*1.64
case dpr>=160 .and. dpr<170
repl p305 with Qpac*1.55
case dpr>=170 .and. dpr<180
repl p305 with Qpac*1.48
case dpr>=180 .and. dpr<190
repl p305 with Qpac*1.41
case dpr>=190 .and. dpr<200
repl p305 with Qpac*1.35
case dpr>=200 .and. dpr<210
repl p305 with Qpac*1.30
case dpr>=210 .and. dpr<220
repl p305 with Qpac*1.26
case dpr>=220 .and. dpr<230
repl p305 with Qpac*1.22
case dpr>=230 .and. dpr<240
repl p305 with Qpac*1.18
case dpr>=240 .and. dpr<250
repl p305 with Qpac*1.14
case dpr>=250 .and. dpr<260
repl p305 with Qpac*1.11
case dpr>=260 .and. dpr<270
repl p305 with Qpac*1.09
case dpr>=270 .and. dpr<280
repl p305 with Qpac*1.06
case dpr>=280 .and. dpr<290
repl p305 with Qpac*1.04
case dpr>=290 .and. dpr<300
repl p305 with Qpac*1.03
case dpr>=300 .and. dpr<305
repl p305 with Qpac*1.01
case dpr=305
repl p305 with Qpac*1.00

CASE DPR>305 .AND. P305<1
REPL P305 WITH PRX*305
endcase
return