\ https://fredrikolofsson.com/f0blog/sapf-sctweets/ 0062
: a SinOsc ;
: b [ 1 2 3 4 5 6 7 8 9 ] ;
: c b 55 * 0 a ;
: d 2 b / 0 a 0.5 * ;
: e 3 b / 0 a 0.5 * 1 + ;
: f c d e Clip ;
: g b 55 * 4 b / + 0 a ;
: h 1 b / 0 a ;
: k g h 6 * * TanH ;
f k * Splay2 5 /
