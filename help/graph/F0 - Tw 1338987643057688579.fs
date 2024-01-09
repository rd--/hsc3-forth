\ F0 - Tw 1338987643057688579
: b 8 18 upTo ;
: x [ 18.1 81.8 1.81 ] b + 1 b 81 / 0 0.5 VarSaw VarSaw ;
: t 8 1.818 / [ 1 8 18 / ] 0.5 VarSaw ;
: d b 181.8 / 0 0.5 VarSaw 1.81 818 / 1.8 181.8 / ExpRange 1.81 818 / RoundTo ;
: c 1 8 / 1 b / 0.5 VarSaw 8.18 + 1.8 * ;
: z 1 81.8 / 0 0.5 VarSaw 1.8 -1 ^ 8.1 8.18 / Range ;
: p x t 1.8 181.8 / d c z Pluck 1.8 b / 0 0.5 VarSaw * 1.8 / ;
p Splay2 18 1.81 / Hpf
