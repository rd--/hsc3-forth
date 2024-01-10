\ F0 - Tw 1395519538008141835
: c 0.004 0 0 inf 1 [ 1 2 ] Dseries 8.192 0 0 1 1 6 upTo 75 * Dseq Duty % Duty ;
: d c 3 MantissaMask 5 Hpf ;
: f { x } x 0 SinOscFb 0 Max ;
: p [ 250 200 ] 4.096 0 0 1 [ 4 6 5 5 5 5 3 ] 4 / Dseq Duty 0.1 Lag * ;
: o p 0.08 f SinOscFb 1 [ 99 60 ] / f * 3 / ;
d Sin 63 0.8 f SinOscFb d * + 9 / o + Tanh 2 / 10000 Lpf 0.2 *
