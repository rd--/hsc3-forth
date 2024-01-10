\ F0 - Tw 0321
: b 1 6 upTo 60 * ;
: c b 0 6 LocalIn 3 * SinOsc ;
: d c 16 b / 0 SinOsc 3 + b * 1 Bpf ;
: w d 0.66 16 b / Limiter LocalOut ;
c Splay2 2 / w <!