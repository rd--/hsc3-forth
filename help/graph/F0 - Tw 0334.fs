\ F0 - Tw 0334
: b 9 1 downTo 4.0015 * ;
: r 2 b / 0 SinOsc 3 * ;
: q b 8 / 0 SinOsc 2 / 1 33 / 0 SinOsc 0 > 8 + b * + 0 SinOsc ;
: ph r 4 + q ^ 6 * ;
0 ph SinOsc 4 1 b / b SinOsc ^ Tanh * 2 / Splay2
