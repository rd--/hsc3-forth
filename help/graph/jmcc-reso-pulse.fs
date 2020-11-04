\ reso-pulse (jmcc) #1 texture=overlap,4,2,4,inf
: reso-pulse
    25 30 34 37 41 42 46 49 53 54 58 61 63 66 14 mce choose MIDICPS { f }
    f 0 0.2 LFPulse.ar 2 f * -0.5 0.5 Rand.ir + 0 0.2 LFPulse.ar + 0.02 *
;
: post-proc
    6 LFNoise0.kr 1000 * 1200 + 0.2 0.02 1 0.2 MouseX.kr RLPF { s }
    s s 0.33333 0.33333 DelayN 2 mce
;
reso-pulse post-proc
