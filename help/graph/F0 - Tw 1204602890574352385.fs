\ F0 - Tw 1204602890574352385
: c 1 1 + ;
: d 1 1 11 upTo 1 + / 1 / ;
: p [ 11 1 - 11 1 << 11 11 1 >> 11 1 + ] 11.1 1 1 11 / VarSaw 1.11111111 + ^ 0 0.5 VarSaw ;
: q d d 1 11 / VarSaw 1.11 d / 1 1.1 d - VarSaw * 11 * 1 + ;
: r d 11 * 111 / 1 1 c / VarSaw 1.1 c / + 0 Max ;
: z p q * Cos d d 1 + 111 1 >> / r CombN c / ;
z c c 11 / d / 1 CombN Splay2 11111 Lpf 0.995 LeakDc Sin 0.5 *
