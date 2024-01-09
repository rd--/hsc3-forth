\ F0 - Tw 1084601286488674304
: b [ 2 5 1 6 3 ] ;
: x 2 0 LocalIn 1.5 0.5 0 0 PitchShift ;
: y 1 b / 0 LfSaw 0 > 4 / 1.25 + b ^ 99 * 0 b 9 / 0 LfSaw 1 % VarSaw ;
: z 1 b / 0 LfSaw 1 RoundTo 1 + 2 / 0 LfSaw 0 > b 33 / 0 LfSaw 4 * 8 + Hpf ;
: d y z * Splay2 1 9 / 0 LfSaw 2 + 4000 * 2 0 MoogFf ;
d d LocalOut <!
