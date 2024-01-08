\ String wander-cluster (Jmcc) #6
: t [ 6 0 do 0.15 i 6 / Impulse.kr loop ] ;
: g [ 6 0 do 0.15 i 6 / 0.5 LfPulse.kr loop ] ;
: e g 1 0 1 doNothing 1.3 0.1 1.3 envSin envAsr EnvGen.kr ;
: d 1 50 100 t TRand.kr 0 15 t TRand.kr + 7 - 50 120 Fold MidiCps / ;
: sw WhiteNoise.ar 0.008 * 0.01 d d 1000 * CombC -1 1 t TRand.kr e Pan2 mix ;
sw
