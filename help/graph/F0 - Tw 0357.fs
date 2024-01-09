\ F0 - Tw 0357
: c 1 [ 31 23 ] / ;
: b c 0 0.5 VarSaw ;
: o [ 7 6 ] 0 0.5 VarSaw 0 < b + 1 > 1 + 99 * 0 1 b - 2 / VarSaw b % ;
o 4 c 0 0.5 VarSaw ^ 666 * 0.08 BLowPass4 Sin 2 / 9 Hpf
