\ F0 - Tw 035
: b [ 4 16 3 ] ;
: o b 0 LfTri b b * 0 LfTri * ;
: f1 8 b 50 / 0 LfTri 1 RoundTo ^ 99 * ;
: f2 3 1 b / 0 LfTri ^ 99 * ;
o f1 1 BLowPass4 f2 0.1 BHiPass4 Splay2 Sin
