\ impulse sequencer (jmcc) SC2
: inf 9e8 ;
: tr 8 0 Impulse.ar ;
: isequ inf swap Dseq.dr tr swap 0 swap Demand.ar tr * ;
: c 1 0 0 1 0 0 1 0 0 0 1 0 1 0 0 0 16 mce isequ
    0.001 0.3 Decay2 1700 0 FSinOsc.ar * 0.1 * ;
: d 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 0 0 0 24 mce isequ
    0.001 0.3 Decay2 2400 0 FSinOsc.ar * 0.04 * ;
: n 1.0 0.1 0.1 1.0 0.1 1.0 0.1 0.1 8 mce isequ
    0.001 0.25 Decay2 BrownNoise.ar * 0.1 * ;
: b 1 0 0.2 0 0.2 0 0.2 0 8 mce isequ
    0.001 0.5 Decay2 100 0 FSinOsc.ar * 0.2 * ;
c d + n + b +
