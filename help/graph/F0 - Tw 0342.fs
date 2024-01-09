\ F0 - Tw 0342
: b 1 8 upTo 128 / ;
: c b 0 LfSaw 1 % ;
: f 2 b 0 LfSaw ^ 256 * 64 RoundTo ;
: e 1 b / 32 / 0 LfSaw 500 1.01 c - Rlpf ;
: o f c SinOscFb e * ;
o 0 1 Clip Splay2 2 /
