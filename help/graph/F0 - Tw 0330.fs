\ F0 - Tw 0330
: c [ 2 3 ] 8 / 0 SinOsc 0.5 % ;
: o c 1 [ 9 5 ] / 0 SinOsc c 0.125 > RoundTo 1 + [ 3 2 ] * 99 * 0 FreqShift Tanh 2 / ;
o 200 9 1 1 9 1 0.1 0.5 300 GVerb transpose Mix 3 /
