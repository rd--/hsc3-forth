\ tremulate (jmcc) #1 overlap-texture=0.5,2,3
: o 0 400 Rand.ir 500 + 1.0 1.2 1.5 1.8 4 mce * 0 FSinOsc.ar ;
: a 0 30 Rand.ir 30 + 4 clone LFNoise2.kr 0.1 * 0 Max ;
: l 0 1 Rand.ir 4 clone ;
: tremulate o l a Pan2 mix ;
: cmb 0.1 0.1 1 CombN ;
tremulate cmb
