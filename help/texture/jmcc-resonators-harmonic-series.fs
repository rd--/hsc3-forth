\ Resonators harmonic series (Jmcc) #2
: p 12 ;
: rat [ 1 1.125 1.25 1.333 1.5 1.667 1.875 2 2.25 2.5 2.667 3 3.333 3.75 4 ] ;
: param { f } [ p 0 do i f * f + -0.5 0.5 rand.ir + 1 i 1 + / 0.5 4.5 rand.ir loop ] ;
brownnoise.ar 0.001 * 1 0 1 rat choose 120 * param klank.ar 2 clone
1 7 inf xfadeTexture
