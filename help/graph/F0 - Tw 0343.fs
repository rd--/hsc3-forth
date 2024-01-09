\ F0 - Tw 0343
: b 0.01 2 6 upTo / ;
: c 1 b - 8 * 1 2 / SinOscFb 1 + 2 / ;
: e c 0.99512 > c 1 + b / 0 SinOscFb * 1 2 / Decay 22 / ;
: o 4 b 0 SinOscFb ^ 99 * c SinOscFb ;
o e * Tanh Splay2
