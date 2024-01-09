\ F0 - Tw 1374139774269857798
: b [ 1 0.5 1.25 ] ;
: c 3 0 SinOscFb 0 > ;
: x 1 64 / 0 SinOscFb 0 < 9 * 50 + 1 8 / 0 SinOscFb 1 + 2 / SinOscFb c 4 Hpf * 5 * ;
: f0 1 12 / 0 SinOscFb 0 inf [ 1 1 2 3 4 1 ] 1 96 / 0 SinOscFb 0.5 < 19 * 99 + * Dseq b * Demand 1 3 Lag3Ud ;
: y f0 b 16 / 0 SinOscFb 1 + 3 / SinOscFb Splay2 ;
: z c 0.1 > 9 1 19 / 0 SinOscFb ^ 3 + Hpf [ 12 9 ] 99 * 0 SinOscFb * 6 / ;
[ x x ] y + z + Tanh 0.1 *
