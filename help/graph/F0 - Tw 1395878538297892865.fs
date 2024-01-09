\ F0 - Tw 1395878538297892865
: t 5 0 Impulse ;
: g [ 3 2 ] ;
: e t 0.001 g 99 / 0 SinOscFb 1.08 + LagUd ;
: m [ 24 0 3 5 7 10 36 ] [ 36 48 36 33 60 72 ] + ;
: o t 0 inf m MidiCps Dseq g / Demand.ar 0.02 0 SinOscFb SinOscFb e * ;
: c o 3 0.04 0 SinOscFb ^ e + 2000 * 3 g 9 / 0 SinOscFb ^ 3 / Rlpf 4 * ;
[ c Tanh c ] 1 1 [ 2 3 ] / [ 4 5 ] CombC Mix 8 / c + Tanh 0.1 *
