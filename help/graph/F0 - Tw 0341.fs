\ F0 - Tw 0341
: e 2 1 5 / 0 LfTri ^ 0 LfTri 1 8 / 0 LfTri 3 / RoundTo ;
: o e [ 99 150 ] ^ e 1 % 500 1 Bpf SinOsc 6 / ;
o 9 Hpf 99 5 0.1 0.5 15 1 0.7 0.5 300 GVerb Mix
