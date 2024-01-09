\ tw 0028 (f0) ; http://www.fredrikolofsson.com/f0blog/?q=node/537 ; wait
: p inf 8 [ 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 ] 8 % 99 * Dshuf Dseq ;
: q 8 0 Impulse.ar 0 p Demand.ar 4 4 16 CombN.ar ;
: o q 0 LfTri 4 / ;
: f 1 16 / 0 LfTri 2000 * 3000 + ;
o f 2 0 MoogFf 0 1 Pan2
