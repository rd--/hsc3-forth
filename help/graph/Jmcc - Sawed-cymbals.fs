\ Sawed cymbals (Jmcc) #9 texture=overlap,4,4,6,inf
: r2 Rand.ir ;
: param [ 15 0 do 500 2500 r2 dup 0 8000 r2 + r2 1 2 6 r2 loop ] ;
: exc 0 600 r2 0 600 r2 12 doNothing XLine.kr ;
: cym exc 0 LfSaw.ar 0.0005 * 1 0 1 param Klank ;
cym
