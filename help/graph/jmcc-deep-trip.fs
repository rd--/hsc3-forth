\ deep trip (jmcc) #9 overlap-texture=4,12,4
: f 0 0.3 Rand.ir LFNoise1.kr 60 * 70 + MIDICPS ;
: s f 0 SinOsc.ar f 0 0.5 Rand.ir * LFNoise2.ar 0 0 8 Rand.ir LFNoise1.kr
    0 40 Rand.ir 0 SinOsc.kr 0.1 * * Max * * 0 5 Rand.ir LFNoise1.kr 1 Pan2 ;
: c s 0.5 0.3 0.5 Rand.ir 2 clone 20 CombN ;
s c + c +
