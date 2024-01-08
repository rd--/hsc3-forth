\ Eggcrate (Rd)
: eggcrate-f { u v } u pi * Cos v pi * Sin * ;
: p [ 64 72 96 128 256 6400 7200 8400 9600 ] ;
: x BrownNoise.kr ;
: y BrownNoise.kr ;
: tr 2.4 Dust.kr ;
: freq x y eggcrate-f -1 1 tr p TChoose tr p TChoose LinLin ;
: amp x -1 1 0 0.1 LinLin ;
: eggcrate freq 0 SinOsc.ar mix y amp Pan2.ar ;
eggcrate
