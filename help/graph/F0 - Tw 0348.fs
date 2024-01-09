\ F0 - Tw 0348
: b [ 9 4 3 6 ] 4 / ;
: c 0.1 b / 0 0.5 VarSaw 1 + 9 / ;
: o 9 c ^ c 3 / 0 0.5 VarSaw b + b RoundTo 99 * 0 c VarSaw * ;
o 1 c 9 + 99 / 9 AllpassC Tanh Splay2 2 /
