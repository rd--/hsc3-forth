\ coolant (jmcc) #2 texture=overlap,4,4,2,inf
: n 20 ;
: param n 0 do 0 2000 Rand.ir 40 + 1 0.1 loop n 3 * mce ;
: src BrownNoise.ar 2 clone 0.0015 * 0.95 OnePole ;
: coolant src 1 0 1 param Klank.ar 2 clone ;
coolant
