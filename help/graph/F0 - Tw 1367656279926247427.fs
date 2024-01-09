\ F0 - Tw 1367656279926247427
: b [ 20 15 18 19 4 1 7 11 22 -125 -188 12 12 ] ;
: f { z } z b SinOsc 1 RoundTo ;
: t 0.01 0 SinOsc 1 RoundTo 10 + b SinOsc ;
: c t 0.85 0.4 f 0.1 f + 2 + 1 b / f ^ 1 + 99 * 0 -1 512 1 GrainSin 5 / ;
: x 2 c pi * SinOsc c 0.5 1 f 0.1 f * 2 + 0.05 1 PitchShift 0.15 * + 5 Hpf ;
: y 50 0 SinOsc 1 [ 0 1 ] SinOsc 1 RoundTo 1 Hpf * 2 / ;
x y + Splay2 Tanh
