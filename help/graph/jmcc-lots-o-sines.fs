\ lots-o-sins (jmcc) #2 texture=xfade,4,4,inf
: n 60 ;
: param n 0 do 0 10000 0 LinRand.ir 40 + 1 1 loop n 3 * mce ;
: lots-o-sins 1 0 param Klang.ar 2 clone 0.1 * n Recip * ;
lots-o-sins
