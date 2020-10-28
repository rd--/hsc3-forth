\ doppler (jmcc)
: l pi 2 / Neg pi 3 * 2 / 12 remove-synth Line.kr ;
200 0 FSinOsc.ar 31.3 0 0.4 LFPulse.ar * 400 0.3 RLPF
0.31977 10 l Cos * 75 l Sin * Hypot 344 / DelayL
40 10 l Cos * 75 l Sin * Hypot Squared / *
