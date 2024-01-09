\ https://sccode.org/1-4Qy ; f0 ; 0238
: b [ 7 6 5 4 ] ;
: c b 99 / 0 LfCub 1 % 50 / ;
: o 3.9 b 0.5 Logistic b 0 LfCub b 7 / 0 LfCub + 800 * 999 + c c 2 * Formlet 9 / ;
o 1 0.01 Limiter Splay2
