\ berlin 1977 (jmcc) #4
: sequ { seq tr } tr 0 inf seq dseq.dr demand.kr ;
: sequr { seq tr } tr 0 inf seq dshuf.dr demand.kr ;
: half-pi pi 0.5 * ;
: clock_rate 5 20 1 0.2 mousex.kr ;
: clock_time 1 clock_rate / ;
: clock clock_rate 0 impulse.kr ;
: clock_16 clock 16 0 pulsedivider ;
: pattern 55 60 63 62 60 67 63 58 8 mce ;
: note pattern clock sequ ;
: freq -12 -7 -5 0 2 5 6 mce clock_16 sequr note + midicps ;
: env clock 0.05 clock_time * 2 clock_time * decay2 ;
: amp env 0.1 * 0.02 + ;
: filt env 0.17 0 fsinosc.kr 800 * * 1400 + ;
: pw  0.08 half-pi sinosc.kr 0.45 * 0.5 + ;
: sig freq pw pulse.ar amp * ;
: berlin-1977 sig filt 0.15 rlpf 0.2 0.2 0.17 2 mce 1.5 combn ;
berlin-1977
