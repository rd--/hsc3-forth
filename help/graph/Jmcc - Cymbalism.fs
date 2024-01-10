\ Cymbalism (Jmcc) #2 texture=overlap,3,6,6,inf
: n 15 ;
: f1 500 2500 Rand.ir ;
: f2 0 8000 Rand.ir ;
: param [ n 0 do f1 f1 f2 + Rand.ir 1 1 5 Rand.ir loop ] ;
0.5 3.5 Rand.ir 0 Impulse.ar 0.004 Decay WhiteNoise.ar 0.03 * * 1 0 1 param Klank.ar 2 clone

