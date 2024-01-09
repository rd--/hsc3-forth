\ https://sccode.org/1-4Qy ; f0 ; 0281
: b 2 2 9 upTo / ;
: o b 0 LfCub b 5 / 0 LfCub % ;
: p b [ 300 303 ] * b 9 / 0 LfCub 0 > 50 * 0.1 Lag2 - ;
: q 2 b 8 / 0 LfCub ^ 0 > 1 + 2 * p * 0 LfCub ;
: r b 6 / b LfCub ;
o q * r * Splay2
