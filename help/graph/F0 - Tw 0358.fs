\ F0 - Tw 0358
: b 2 [ 9 8 7 ] / 0 LfTri ;
: f [ 2 3 4 5 ] 0.1 0 LfTri 0 > 2 + / b 0 LfTri Ceil 0.1 Lag ^ 99 * [ 3 1 8 2 4 ] * ;
: o f 9 999 Fold 2 b - 5 / Pulse ;
o Splay2 2 / 2 /
