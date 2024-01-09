\ tw 0049 (f0)
: o0 3 0 LfSaw 9 * 99 + ;
: s0 0 3 9 upTo Sweep o0 % ;
: s1 0 1 31 upTo -1 * 34 + Sweep 128 % s0 BitAnd ;
: o1 s1 33 + MidiCps 0 SinOsc pi * ;
9 o1 SinOsc Splay2 9 /
