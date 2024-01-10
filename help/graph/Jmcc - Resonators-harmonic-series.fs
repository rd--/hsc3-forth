\ Resonators harmonic series (Jmcc) #2 texture=xfade,1,7,inf
: p 12 ;
: rat [ 1 1.125 1.25 1.333 1.5 1.667 1.875 2 2.25 2.5 2.667 3 3.333 3.75 4 ] ;
: param { f } [ p 0 do i f * f + -0.5 0.5 Rand.ir + 1 i 1 + / 0.5 4.5 Rand.ir loop p ] ;
BrownNoise.ar 0.001 * 1 0 1 rat choose 120 * param Klank.ar 2 clone
