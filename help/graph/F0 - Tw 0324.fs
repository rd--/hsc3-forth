\ F0 - Tw 0324
: c [ 1 3 5 6 ] ;
: f c 1 + 16 / 0 0 inf 1 c / 8 / 0 LfTri 0 > 3 * c + 99 * Dseq Duty ;
c 0 LfTri f 0.001 BLowPass 12 / 9 Hpf Splay2 Sin 2 /
