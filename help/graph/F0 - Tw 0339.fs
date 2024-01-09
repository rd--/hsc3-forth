\ F0 - Tw 0339
: b 1 1 12 upTo / ;
: f1 2 b 0 LfSaw 8 b LfSaw 0 < 4 / 1 + RoundTo ^ ;
: f2 4 b 12 / 0 LfSaw ^ 2 + ;
: f3 3 b 4 / 0 LfSaw ^ 3 * ;
f1 99 * f2 99 * f3 99 * Formant Splay2 Tanh 2 /
