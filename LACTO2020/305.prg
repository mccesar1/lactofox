do case
case dpr>=15 .and. dpr<20
repl p305 with Qpac*16.67
case dpr>=20 .and. dpr<30
repl p305 with Qpac*12.50
case dpr>=30 .and. dpr<40
repl p305 with Qpac*8.32
case dpr>=40 .and. dpr<50
repl p305 with Qpac*6.24
case dpr>=50 .and. dpr<60
repl p305 with Qpac*4.99
case dpr>=60 .and. dpr<70
repl p305 with Qpac*4.16
case dpr>=70 .and. dpr<80
repl p305 with Qpac*3.71
case dpr>=80 .and. dpr<90
repl p305 with Qpac*3.15
case dpr>=90 .and. dpr<100
repl p305 with Qpac*2.82
case dpr>=100 .and. dpr<110
repl p305 with Qpac*2.55
case dpr>=110 .and. dpr<120
repl p305 with Qpac*2.34
case dpr>=120 .and. dpr<130
repl p305 with Qpac*2.16
case dpr>=130 .and. dpr<140
repl p305 with Qpac*2.01
case dpr>=140 .and. dpr<150
repl p305 with Qpac*1.88
case dpr>=150 .and. dpr<160
repl p305 with Qpac*1.77
case dpr>=160 .and. dpr<170
repl p305 with Qpac*1.67
case dpr>=170 .and. dpr<180
repl p305 with Qpac*1.58
case dpr>=180 .and. dpr<190
repl p305 with Qpac*1.51
case dpr>=190 .and. dpr<200
repl p305 with Qpac*1.44
case dpr>=200 .and. dpr<210
repl p305 with Qpac*1.38
case dpr>=210 .and. dpr<220
repl p305 with Qpac*1.32
case dpr>=220 .and. dpr<230
repl p305 with Qpac*1.27
case dpr>=230 .and. dpr<240
repl p305 with Qpac*1.23
case dpr>=240 .and. dpr<250
repl p305 with Qpac*1.19
case dpr>=250 .and. dpr<260
repl p305 with Qpac*1.15
case dpr>=260 .and. dpr<270
repl p305 with Qpac*1.12
case dpr>=270 .and. dpr<280
repl p305 with Qpac*1.08
case dpr>=280 .and. dpr<290
repl p305 with Qpac*1.06
case dpr>=290 .and. dpr<300
repl p305 with Qpac*1.03
case dpr>=300 .and. dpr<305
repl p305 with Qpac*1.01
case dpr=305
repl p305 with Qpac*1

CASE DPR>305 .AND. P305<1
REPL P305 WITH PRX*305

endcase
return
