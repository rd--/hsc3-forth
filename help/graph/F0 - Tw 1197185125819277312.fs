\ F0 - Tw 1197185125819277312
: f [ 2 2.2 ] ;
: e f 22 / ;
: g 2 2 << ;
: c e 0 SinOsc 2 / 0 2 LocalIn * 2 2 g CombN [ 22 2 ] 222 * e 2 * Rhpf ;
: o 2.2 2 [ 2.2 2 ] e LfPulse ToggleFf ^ 22 * 2.22 * c e - ToggleFf 0.1 Lag g * SinOsc 2 f 0.5 LfPulse * ;
: d o g e 2 / 2 / 0 SinOsc ^ 2222 * 2 / e 0 SinOsc 2 + 2 pi * / Rlpf c + Splay2 Tanh ;
d 0.25 * d LocalOut <!
