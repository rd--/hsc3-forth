\ bouncing objects (jmcc) #2
: param 4 0 do 400 8400 Rand.ir 0 1 Rand.ir 0.01 0.11 Rand.ir loop 12 mce ;
5 -2 2 Rand.ir + 600 4 do-nothing XLine.kr 0 Impulse.ar
0.09 epsilon 4 do-nothing XLine.kr * 0.001 Decay
1 0 1 param Klank.ar
-1 1 Rand.ir 1 Pan2.ar
1 1 0 1 remove-synth 1 2 -99 -99 1 3 1 0 0 0.001 1 0 12 mce EnvGen.kr *
0.6 0 0.6 Rand.ir DelayN
0.6 inf spawn-texture
