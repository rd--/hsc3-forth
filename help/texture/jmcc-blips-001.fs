\ Blips 001 (Jmcc) #SC3d1.5
: dup' 2 clone items drop ;
: blips
    0 1 Rand.ir 0.8 <
    0.25 400 ExpRand.ir 0.25 400 ExpRand.ir 4 0 XLine.kr
    2 100 ExpRand.ir 2 100 ExpRand.ir 4 0 XLine.kr Blip.ar dup' *
    -1 1 Rand.ir -1 1 Rand.ir 4 0 Line.kr 0.3 Pan2.ar * ;
: apf Distort 6 0 do 0.05 0 0.05 Rand.ir 4 AllpassN loop ;
' apf texturePostProc
blips 2 1 12 inf overlapTexture
