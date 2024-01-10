\ F0 - Tw 1143262368392200192
: c 4 4 / 4 / ;
: d c 4 * 44.4 / ;
: f d c -4 sin VarSaw 4 * 0 Max Ceil 44 * 44 + ;
: b f 4 [ 44 4 - 4 44 4 + ] 4.44 / 0 0.5 VarSaw VarSaw 4 / ;
: o c 4 / c c VarSaw 0 Max 4 * 44 + 4 [ 4 4 44 / c 4 * ] 0 0.5 VarSaw Abs VarSaw 44 44 * c Rlpf ;
: q b c c 4 * f / d + 1 CombC 4 c - [ 4 4 + 4 + 4 4 * 4 4 + ] c c PitchShift c c + * ;
o 0 1 4 0 Line * q + Splay2 4 /
