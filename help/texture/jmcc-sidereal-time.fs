\ Sidereal time (jmcc) #9
: p 15 ;
: z [ p 0 do 100 6000 ExpRand.ir 1 2 6 Rand.ir loop ] ;
: f 40 300 ExpRand.ir 40 300 ExpRand.ir 12 0 XLine.kr ;
: t f 0 0.1 0.9 Rand.ir LFPulse.ar 0.002 * 0 0 8 Rand.ir LFNoise2.kr Max * ;
: rev items swap 2 array ;
: cmb { z } z 0.6 0.1 0.6 Rand.ir 8 CombN z rev + ;
' cmb texturePostProc
t 1 0 1 z Klank 2 clone Distort 0.1 *
4 4 6 inf overlapTexture
