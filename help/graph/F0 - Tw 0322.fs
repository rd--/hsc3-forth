\ F0 - Tw 0322
: f [ 1 1 2 3 5 8 13 21 34 55 89 144 ] 1 [ 2 4 ] / 0 SinOscFb 1 % 0.125 + 1 RoundTo / ;
: c f 1 SinOscFb ;
: m c 0 > [ 9 2 3 0 7 5 ] * 55 + ;
m MidiCps c 999 / 0 SinOscFb 1 + 2 / SinOscFb Splay2 3 /
