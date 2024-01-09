\ F0 - Tw 1105496695637454848
: b 1 15 upTo 1 + 151 / ;
: w b b 1 5.15 / VarSaw 1 * 1.5 + Ceil ;
: s 1 [ 51 1.5 * 51 151 ] asLocalBuf 1 15 / 1 5 / b VarSaw 5 * Ceil 5 + 5 / 0 1 BufRd ;
: x s b Lag ;
: y 5 b + 0 0.5 VarSaw ;
: z b b b VarSaw b * 5 / [ 1.5 5 1 ] + ;
: m 5 b - b b VarSaw 5 * 5 + ;
: o w x * y + z * b b 0 0.5 VarSaw 5 + 15 / VarSaw m 1.515 > * ;
: f 1515 1 b - 5 / 0 0.5 VarSaw 15 / 1 + b + ^ ;
: rq 1.5 b 0 0.5 VarSaw ^ 5 / ;
o f rq BLowPass Splay2 5 /
