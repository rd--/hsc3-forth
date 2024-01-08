\ Slow beating harmonic sines (Jmcc) #7 texture=overlap,3,6,3,inf
: n 8 ; ( notes )
: m 5 ; ( harmonics )
: d 0.4 ; ( frequency deviation )
: rfreq [ 0 2 4 5 7 9 ] choose 0 7 rand.ir 12 * + 0 12 rand.ir + 24 + midicps ;
: rosc { f } f d rand2.ir + 0 pi rand.ir sinosc.ar 2 clone ;
: rharm { f } 0 m 0 do f i * rosc + loop ;
: sbhs 0 n 0 do rfreq rharm + loop 0.1 n / * ;
sbhs
