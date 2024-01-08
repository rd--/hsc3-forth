\ landon rose (Jmcc) #8
: f MidiCps 1 3 ;
: param
    [ 32 f 43 f 54 f  89 f ]
    [ 10 f 34 f 80 f 120 f ]
    [ 67 f 88 f 90 f 100 f ]
    [ 76 f 88 f 99 f 124 f ]
;
: k { p z e } PinkNoise.ar 2 clone 0.001 * e * 1 0 1 p klank.ar z + ;
: lr 4 0 do 0.125 i 0.5 * pi * SinOsc.ar Abs k loop ;
param 0 lr
