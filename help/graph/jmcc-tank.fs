\ tank (jmcc)
: rloc -1 1 Rand.ir 1 Pan2 ;
: pling 0.2 Dust.ar 0.1 0.5 Decay2 0.1 * 300 2200 ExpRand.ir 0 FSinOsc.ar cubed * rloc ;
: bang 0.01 Dust.ar 0.04 0.3 Decay2 BrownNoise.ar * 0 1 Pan2 ;
: rallpass 0.03 0.005 0.02 Rand.ir 2 clone 1 AllpassN ;
: r2 Rand.ir 2 clone ;
: inp 0 0 2 mce 2 LocalIn.ar 0.98 * ;
: tank-f inp 0.33 OnePole unmce 0.23 Rotate2 0.05 0.01 0.05 r2 2 AllpassN
    0.3 0.17 0.23 2 mce DelayN 0.05 0.03 0.15 r2 2 AllpassN
    0.995 LeakDC + dup LocalOut 2 mrg ;
: tank bang pling 8 clone Mix + 4 0 do rallpass loop tank-f ;
tank
