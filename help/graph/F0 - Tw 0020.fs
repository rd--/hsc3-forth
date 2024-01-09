\ tw 0020 (f0)
: a { freq } freq 0 0.5 LfPulse ;
: b [ 0 1 2 3 ] ;
: a1 b 32 / a 1 + 8 / ;
: a2 a1 a 1 + b * ;
: a3 b 64 / a ;
: a4 b 8 / a a 2 * b + ;
: a5 4 b 1 + / a ;
: a6 a2 a ;
: a7 a6 a3 Sum a5 + a4 * + 100 * a ;
a7 Sum 0 0.1 Pan2
