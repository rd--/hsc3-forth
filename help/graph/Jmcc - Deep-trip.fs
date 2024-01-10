\ Deep trip (Jmcc) #9 texture=overlap,4,12,4,inf
: f 0 0.3 Rand.ir LfNoise1.kr 60 * 70 + MidiCps ;
: s f 0 SinOsc.ar f 0 0.5 Rand.ir * LfNoise2.ar 0 0 8 Rand.ir LfNoise1.kr
    0 40 Rand.ir 0 SinOsc.kr 0.1 * * Max * * 0 5 Rand.ir LfNoise1.kr 1 Pan2 ;
: c s 0.5 0.3 0.5 Rand.ir 2 clone 20 CombN ;
s c + c +
