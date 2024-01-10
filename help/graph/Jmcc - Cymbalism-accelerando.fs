\ Cymbalism accelerando (Jmcc) #2 texture=xfade,4,4,inf
: n 15 ;
: f1 500 2500 Rand.ir ;
: f2 0 8000 Rand.ir ;
: param [ n 0 do f1 f1 f2 + Rand.ir 1 1 5 Rand.ir loop ] ;
: tf 0.5 4.5 0 LinRand.ir 0.5 35.5 Rand.ir 12 0 XLine.kr ;
tf 0 Impulse.ar 0.004 Decay WhiteNoise.ar 0.02 * * 1 0 1 param Klank.ar 2 clone

