\ Clustered sines (Jmcc) #2
: cs
    80 { n }
    100 1100 Rand.ir { f1 }
    4 f1 *  { f2 }
    [ n 0 do 0 f2 Rand.ir f1 + { y } y f1 y / 0 loop ] { param }
    1 0 param Klang.ar 0.3 n / * 2 clone
;
cs 4 4 inf xfadeTexture
