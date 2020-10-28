\ police state (jmcc) #2
: nd
    0.02 0.12 Rand.ir 0 6.28319 Rand.ir SinOsc.kr
    0 600 Rand.ir * 700 1300 Rand.ir + 0 SinOsc.ar
    80 120 Rand.ir LFNoise2.ar * 0.1 * -1 1 Rand.ir 1 Pan2
;
nd 4 clone mix 0.4 LFNoise2.kr 90 * 620 + LFNoise2.ar
0.3 LFNoise2.kr 0.15 * 0.18 + * + 0.3 0.3 3 CombL
