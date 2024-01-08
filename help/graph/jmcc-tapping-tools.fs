\ Tapping tools (Jmcc) #7 texture=overlap,2,1,3,inf
: param [ 4 0 do 400 8400 Rand.ir 1 0.01 0.11 Rand.ir loop ] ;
: tt
    1 21 0 LinRand.ir 64 0.125 60 0 XLine.kr * 0 Impulse.ar
    0.03 * 0.001 Decay 1 0 1 param Klank
    -1 1 Rand.ir 1 1 0 1 2 1 4 1 1 envLinen EnvGen.kr Pan2
;
: rapf 3 0 do 0.05 0 0.05 Rand.ir 2 clone 2 AllpassN loop ;
tt rapf
