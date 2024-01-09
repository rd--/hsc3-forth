\ https://sccode.org/1-4Qy ; f0 ; 0272
: b 3 8 upTo 0.05 0 SinOsc 1 RoundTo 4 / 1 + * ;
: c 1.1 b / 0 SinOsc ;
: o 2 b ^ 0 SinOsc c > b 99 * c 0.5 % 0.5 + TwoPole 0 SinOsc 2 / ;
o Splay2 9 Hpf
