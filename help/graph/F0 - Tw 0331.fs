\ F0 - Tw 0331
: b [ 3 3 2 2 2 1 2 2 2 ] 3 / ;
: c inf b Dseq 0 0 1 0 TDuty 9 / 1 0 Spring ;
c 50 c 1 + [ 6 3 ] / 0 0 inf c 0.1 Lag 0 > [ 2 4 ] + b / Dseq Duty * 1 Ringz
