\ harmonic swimming (jmcc) #1
: a 0.02 ;
: f 50 ;
: p 20 ;
: l 0 a negate 60 do-nothing Line.kr ;
: o { h }
    2 8 Rand.ir 2 clone LFNoise1.kr { n }
    0 n a * l + Max { e }
    f h 1 + * 0 FSinOsc.ar e * ;
: hs 0 p 0 do i o + loop ;
hs
