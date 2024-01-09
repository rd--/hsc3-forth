\ F0 - Tw 1454150378241548296
: a { r z n q p } r p Impulse z 5 % [ 0 3 5 7 10 ] n + Select MidiCps q Rlpf ;
: x 0.001 ;
: b 1 8 upTo 8 / ;
: z [ 2 x 0 LfSaw 88 * 51 x 9 * 0 a 1 0 70 x 5 * b a 1 9 / 0 LfSaw 1 + 5 b 9 / 0 LfSaw ^ 82 x 3 * b 3 * a 8 / 3 x 2 * 0.5 LfSaw 88 * 63 x 7 * 0 a 4 / ] ;
: o z Sum Splay2 ;
o o Sum 9 / 50 3 0.5 0.5 15 1 0.7 0.5 300 GVerb +
