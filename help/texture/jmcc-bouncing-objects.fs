\ Bouncing objects (Jmcc) #2
: param [ 4 0 do 400 8400 Rand.ir 0 1 Rand.ir 0.01 0.11 Rand.ir loop ] ;
5 -2 2 Rand.ir + 600 4 doNothing XLine.kr 0 Impulse.ar
0.09 epsilon 4 doNothing XLine.kr * 0.001 Decay
1 0 1 param Klank.ar
-1 1 Rand.ir 1 Pan2.ar
1 1 0 1 removeSynth [ 1 2 -99 -99 1 3 1 0 0 0.001 1 0 ] EnvGen.kr *
0.6 0 0.6 Rand.ir DelayN
0.6 inf spawnTexture
