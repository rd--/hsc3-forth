\ F0 - Tw 1343283697840226307
: b 2 12 upTo 12 / ;
: f [ 136.1 163.3 181.4 204.1 244.9 272.1 326.5 362.8 408.2 489.8 544.2 ] ;
: z [ 11 0 do WhiteNoise loop ] b 0 LfTri 1 + 2 / * BrownNoise + ;
: t 121 2 121 / b * 0 LfTri ^ 0 LfTri ;
: d 1 212.1 / 0 LfTri 1 212 / <= 2 b / Lag2 12 / 1.2 * 1.2 + f / ;
: c b 12 / 0 LfTri 1 + 2 / ;
: p z t 2 121 / d 1 2 / 12 * 1.2 / c Pluck ;
p 12 Hpf Splay2 2.1 /
