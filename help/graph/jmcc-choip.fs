\ choip (jmcc) #10 texture=overlap,10,1,8,inf
: dup' 2 clone unmce ;
: t 12 ;
: xl ExpRand.ir dup' t do-nothing XLine.kr ;
: choip
    1 30 xl 0 Impulse.ar { tr }
    600 8000 xl { f }
    tr 0.05 0.5 Decay2 -0.9 * f * f + 0 SinOsc.ar { a }
    -1 1 Rand.ir dup' t do-nothing Line.kr { l }
    tr 0.01 0.5 xl * 0.01 0.2 Decay2 a * l 1 Pan2.ar ;
: apf 4 0 do 0.1 0 0.05 Rand.ir 2 clone 4 AllpassN loop ;
choip apf
