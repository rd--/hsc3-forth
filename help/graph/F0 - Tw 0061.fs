\ F0 - Tw 0061
: t [ 9 9.01 ] Saw ;
: f t 0 inf [ 0 0 0 0 0 0 500 ] Dseq Demand.ar ;
: p t 0 inf 0 7 upTo 1 - 99 * Dshuf Demand.ar 0.04 Lag 0 SinOsc ;
f p SinOsc 4 /
