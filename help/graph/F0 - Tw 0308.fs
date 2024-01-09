\ https://sccode.org/1-4Qy ; f0 ; 0308
: b 1 9 upTo ;
: o 2.01 3 Blip 0 > b Blip 1 + b 8 / Blip 2 + 99 * ;
: a o b 50 * 0.01 1 Formlet Tanh Splay2 ;
a 0.01 0 FreqShift a + 7 /
