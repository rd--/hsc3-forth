\ harmonic tumbling (jmcc) #1
: f 80 ;
: p 10 ;
: t 10 11 2 mce 0.1 60 doNothing XLine.kr ;
: o { h }
    t Dust.kr 0.02 * 0.005 0.25 0.5 Rand.ir Decay2
    f h 1 + * 0 FSinOsc.ar * ;
: ht 0 p 0 do i o + loop ;
ht
