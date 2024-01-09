\ F0 - Tw 727992849527083009
: b 1 3 / 0 LfSaw ;
: c [ 0 1 ] ;
: d b 0 LfSaw ;
: z b 1 + d ^ 99 c + * 0 LfSaw ;
: q b 99 * c LfSaw ;
: k 1 32 / 0 LfSaw ;
: u d 40000 * 2000 % 0 LfSaw ;
: v 6 c LfSaw 0.9 > 2 / ;
z q % k % u v * + 2 /
