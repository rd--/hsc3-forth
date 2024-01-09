\ F0 - Tw 1452599226882535431
: e 0.1 0 LfSaw ;
: b 1 4 upTo 2 / ;
: f [ 0 1 ] ;
: d 11 f LfSaw -1 1 2 0.061 f LfSaw ^ 4 * 1 - 2 0.031 f LfSaw ^ 20 * 8 - LinExp ;
: x 0.001 b LfSaw 0.071 0 LfSaw 0 > 8 / RoundTo 5 * 0 LfSaw ;
: y x -1 1 2 0.041 f LfSaw ^ 120 * 59 - e Latch 2 0.051 0 LfSaw ^ 500 * e Latch LinExp ;
: o y d Blip 1 * d 1 Max / ;
o Splay2 9 Hpf
