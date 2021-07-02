\ synthetic piano (jmcc) #3 texture=overlap,18,0,6,inf
: c { o f ph n }
    3000 LFNoise2.ar f ph Impulse.ar
    0.1 * 0.008 0.04 Decay2 * 1 n o + MidiCps / dup
    6 CombL
;
: sp { n }
    0.1 0.5 Rand.ir { f }
    0 1 Rand.ir { ph }
    -0.05 0 0.04 0 3 0 do swap f ph n c + loop n 36 - 27 / 1 - 1 Pan2
;
: spr 36 90 IRand.ir sp ;
spr
