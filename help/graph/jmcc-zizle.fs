\ zizle (jmcc) #SC3d1.5 texture=overlap,4,4,12,inf
: two-pi pi 2 * ;
: a 0.7 1.3 Rand.ir 1 2 mce * 0 two-pi Rand.ir 2 clone SinOsc.ar 0.1 * mix ;
0.3 8 ExpRand.ir a 0 Max
6 24 ExpRand.ir a Abs *
24 108 Rand.ir MidiCps 0 two-pi Rand.ir SinOsc.ar *
-1 1 Rand.ir 1 Pan2.ar

