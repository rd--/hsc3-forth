\ F0 - Tw 1590835403157688320
: b 1 9 upTo ;
: g 1 b / 99 / ;
: z WhiteNoise 0.5 RoundTo 2 + BrownNoise 1.4 + b 999 / 0 SinOsc > Latch ;
: r z 9 g 9 * 0 SinOsc ^ 999 * b 77 / 0 SinOsc 2 / 0.6 + Rlpf ;
: x 999 b * r / 0 SinOsc ;
: w WhiteNoise BrownNoise 1.3 + b 99 / 0 SinOsc > Latch ;
: y w 9 g 0 SinOsc ^ 999 * b 88 / 0 SinOsc 2 / 0.6 + Rlpf ;
7 g 0 SinOsc ^ 9 / x * y * Splay2
