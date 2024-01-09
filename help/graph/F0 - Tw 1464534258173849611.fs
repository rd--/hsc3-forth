\ F0 - Tw 1464534258173849611
: b [ 11 12 13 1 2 3 4 5 6 7 8 9 10 ] ;
: n GrayNoise 1 b / 99 / 0 SinOsc 0 Max * ;
: f 62.5 b * ;
: c f [ 6 3 7 5 1 8 4 2 6 3 7 5 1 ] * ;
: e b 666 / 0 SinOsc 2 + ;
: x n f 0.5 0 SinOsc b 999 / 0 SinOsc + 99 / 0.1 + BBandPass e * ;
: y n c 9 b 333 / 0 SinOsc ^ 99 / BBandPass 666 * c / ;
: z n c 2 / 9 b 222 / 0 SinOsc ^ 99 / BBandPass 888 * c / ;
x y + z + 0.001 b 9 / SinOsc 0 Max * Splay2
