\ phase modulation with slow beats (jmcc) #6

: x 100 6000 1 0.2 MouseX.kr ;
: y 0 2 0 0.2 MouseY.kr ;
: f 0 x Rand.ir dup -1 1 Rand.ir + 2 mce ;
: o 0 3 0 do f 0 FSinOsc.ar y * + loop ;
: pm f o SinOsc.ar 0.1 * ;
pm 4 4 4 inf overlap-texture
