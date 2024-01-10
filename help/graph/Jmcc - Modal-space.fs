\ Modal space (Jmcc) #8
: b [ 0 2 3.2 5 7 9 10 ] asLocalBuf ;
: x 0 14 linear 0.1 MouseX.kr ;
: k b x 12 DegreeToKey.kr ;
: c { n r }
    r k + n 0.04 * + MidiCps 0 SinOsc.ar 0.1 * { o }
    [ 48 55 ] MidiCps 0 0.15 LfPulse.ar { t }
    0.1 0 SinOsc.ar 10 * r + MidiCps { f }
    t f 0.1 Rlpf.ar 0.1 * { d }
    o d + { m }
    m 0.31 0.31 2 CombN.ar m + ;
: n [ 3 3 ] LfNoise1.kr ;
: modalSpace n 48 c n 72 c + 0.2 * ;
modalSpace
