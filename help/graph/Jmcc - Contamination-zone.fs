\ Contamination zone (Jmcc) #9 texture=overlap,3,8,4,inf
: param [ 4 0 do 50 2000 Rand.ir 1 0.2 4 Rand.ir loop ] ;
: cz
    800 8000 ExpRand.ir { f }
    PinkNoise.ar 0 3 Rand.ir LfNoise1.kr 0.0008 * 0.0022 + *
    1 0 1 param Klank.ar Abs [ -1 1 ] choose *
    0 1 0 LinRand.ir 0 SinOsc.kr 0.7 f * * f + 0.1 Rlpf
    0 1 Rand.ir LfNoise1.kr 0 150 0 LinRand.ir 0 0.2 0.4 Rand.ir LfPulse.kr Pan2
;
: cz-pp 6 0 do 0.04 0 0.04 Rand.ir 2 clone 16 AllpassN loop ;
cz cz-pp
