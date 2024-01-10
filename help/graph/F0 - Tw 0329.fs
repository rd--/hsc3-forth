\ F0 - Tw 0329
: e 97 1 32 / 0 SinOsc 2.2 + 1 RoundTo 0.1 Lag / 0 SinOsc pi * ;
: c 99 0 9 LocalIn 1 9 upTo 99 / 98 - 0 SinOsc * e * SinOsc ;
c Splay2 9 / c LocalOut <!
