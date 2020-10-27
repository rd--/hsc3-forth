\ swept resonant noise (jmcc) #2

: param { p } p 0 do 80 10080 0 LinRand.ir 1 0.5 2.5 Rand.ir loop p 3 * mce ;
WhiteNoise.ar 0.007 *
0.1 0.3 Rand.ir 0 FSinOsc.kr 0 24 Rand.ir * 36 84 Rand.ir + MIDICPS
0.1 Resonz
1 0 1 10 param Klank 2 clone
4 4 5 inf overlap-texture
