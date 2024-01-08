\ Spe (Jmcc)
: seq [ 0 3 2 7 8 32 16 18 0 12 24 32 ] ;
: env 0.1 0 1 doNothing 0.1 1 envPerc EnvGen.kr ;
: ra 4 0 do 0.05 0 0.05 Rand.ir 2 clone 4 AllpassN loop ;
9 0 Impulse.kr 0 inf seq Dseq.dr Demand.kr 32 + MidiCps
0 LfSaw.ar 9 0 Impulse.kr env * 1 LfNoise1.kr
36 * 110 + MidiCps 0.1 Rlpf ra
