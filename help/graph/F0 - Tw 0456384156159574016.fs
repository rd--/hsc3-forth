\ F0 - Tw 0456384156159574016
: a 1 [ 3 12 4 1 6 2 ] / ;
: s a 0 SinOsc 2.67 a ^ 0 SinOsc Abs Lag3 99 * ;
: f 1 a / 9 / a SinOsc 0 > 20 * 99 + a / ;
s f 1 Ringz 440 Hpf 0 SinOsc Splay2 0.25 *
