\ F0 - Tw 0337
: b 1 12 upTo 8 / ;
: f1 b b 3.5 / 80 + 999 b - 9 / Formant 3 + 50 * ;
: f2 b 3 / 2.5 5 Formant 0 > 1 + 300 * ;
: f3 b 5 / 2 3 Formant 0 > 1200 * ;
f1 f2 f3 Formant 3 / Splay2 Tanh
