\ F0 - Tw 0266
: b 1 6 upTo 2 / ;
: e b 99 / 0 SinOsc 2 * 3 + ;
: c b 0 SinOsc 4 b / / e 1 RoundTo + 99 * ;
: d c 0 SinOsc Splay2 ;
d Sin c 1.5 * e d / SinOsc 0.7 * e Sum / 5 / + 5 /
