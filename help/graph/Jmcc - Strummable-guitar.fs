\ Strummable guitar (Jmcc) #11
: scale 52 57 62 67 71 76 ;
: str { sc ix }
    0 1 linear 0.2 MouseX.kr { x }
    x 0.25 ix 0.1 * + > Hpz1.kr abs { t }
    t 0.05 Decay.kr PinkNoise.ar * { n }
    sc MidiCps Recip { dt }
    n dt dt 4 CombL.ar { s }
    s ix 0.2 * 0.5 - 1 Pan2.ar ;
: strs scale 0 6 0 do swap i str + loop ;
: gtr strs 12000 Lpf.ar 0.995 LeakDC.ar ;
gtr
