\ noise burst sweep (jmcc) #6 overlap-texture=4,2,4
: cf 400 8000 1 0.2 MouseY.kr ;
WhiteNoise.ar 2 clone
0 -1 1 Rand.ir 10 60 1 0.2 MouseX.kr +
-1 LFSaw.kr Max * 0.2 0 SinOsc.kr cf * 1.05 cf * + 0.1 Resonz

