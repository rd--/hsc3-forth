\ https://sccode.org/1-4Qy ; f0 ; 0283
: b 1 2 6 to / ;
: o1 b 1 SinOscFb b < 500 * 99 + 0 SinOscFb 5 / ;
: o2 999 b * b 1 SinOscFb 0.1 < 1 + 1 SinOscFb b % SinOscFb ;
: o3 0.1 b - 1 SinOscFb 0 Min ;
o1 o2 o3 * + Splay2 2 /
