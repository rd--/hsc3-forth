\ F0 - Tw 0839296011982024704
: t [ 0.5 0.6 ] 0 LfSaw ;
: f 5 0 LfSaw 0.15 0 LfSaw * 1 + 98 * ;
: m 2 4 0 LfSaw ^ 0.5 RoundTo 99 * ;
: z 2 1 [ 8 9 ] / 0 LfSaw ^ 8 * ;
: g t 16 f m z 0 -1 512 1 GrainFm ;
g 2 / Tanh 0.25 *
