\ ring modulated klank (jmcc) #2

: p 8 ;
: param p 0 do 100 10000 Rand.ir 1 0.2 1 Rand.ir loop p 3 * mce ;
0.1 0.4 Rand.ir LFNoise2.kr 200 * 350 400 Rand.ir + 0 SinOsc.ar 20 Dust.ar 0.02 *
1 0 1 param Klank.ar * -1 1 Rand.ir 1 Pan2
4 4 4 inf overlap-texture
