\ modal space (jmcc) #8
: b 0 2 3.2 5 7 9 10 7 Mce AsLocalBuf ;
: x 0 14 linear 0.1 MouseX.kr ;
: k b x 12 DegreeToKey.kr ;
: c { n r }
    r k + n 0.04 * + MidiCps 0 SinOsc.ar 0.1 * { o }
    48 55 2 Mce MidiCps 0 0.15 LFPulse.ar { t }
    0.1 0 SinOsc.ar 10 * r + MidiCps { f }
    t f 0.1 RLPF.ar 0.1 * { d }
    o d + { m }
    m 0.31 0.31 2 CombN.ar m + ;
: n 3 3 2 mce LFNoise1.kr ;
: ModalSpace n 48 c n 72 c + 0.2 * ;
ModalSpace
