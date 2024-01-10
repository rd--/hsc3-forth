\ F0 - Tw 0454598285861617665
: b 9 1 downTo ;
: c 3 1 b / b 9 / LfTri ^ 0 LfTri ;
: d 1 b / 0 LfTri 1 % 9 / 0.02 + ;
: f 2 b 99 / 0 LfTri 1 RoundTo ^ 99 * b * ;
: o c d f 0 -1 512 2 GrainSin ;
o Tanh Splay2 transpose Mix 2 /
