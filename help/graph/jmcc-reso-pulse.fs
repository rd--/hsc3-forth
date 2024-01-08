\ Reso-pulse (Jmcc) #1 texture=overlap,4,2,4,inf
: resoPulse
    [ 25 30 34 37 41 42 46 49 53 54 58 61 63 66 ] choose MidiCps { f }
    f 0 0.2 LfPulse.ar 2 f * -0.5 0.5 Rand.ir + 0 0.2 LfPulse.ar + 0.02 *
;
: postProc
    6 LfNoise0.kr 1000 * 1200 + 0.2 0.02 1 0.2 MouseX.kr Rlpf { s }
    [ s s 0.33333 0.33333 DelayN ]
;
resoPulse postProc
