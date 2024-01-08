\ Comb delay sweeps (Jmcc) #6
: t [ 6 0 do 0.15 i 6 / Impulse.kr loop ] ;
: g [ 6 0 do 0.15 i 6 / 0.5 LfPulse.kr loop ] ;
: e g 1 0 1 doNothing 1.3 0.1 1.3 envSin envAsr EnvGen.kr ;
: d -7 8 t TRand.ir 50 120 Fold ;
: w WhiteNoise.ar 0.005 * 0.01 ;
: cds w 1 d d 4 0 Line.kr MidiCps / 1 d MidiCps / 1000 * CombC -1 1 t TRand.ir e Pan2 ;
cds mix
