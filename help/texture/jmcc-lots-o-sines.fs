\ Lots-o-sins (Jmcc) #2
: n 60 ;
: param [ n 0 do 0 10000 0 LinRand.ir 40 + 1 1 loop ] ;
: lots-o-sins 1 0 param Klang.ar 2 clone 0.1 * n Recip * ;
lots-o-sins 4 4 inf xfadeTexture
