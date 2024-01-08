\ Hard sync sawtooth with lfo (jmcc) #6
: hsswl
    0 50 Rand.ir 30 + MidiCps { f }
    0.2 [ 0 0 pi 2 * Rand.ir ] SinOsc.kr 2 f * * 3 f * + { o }
    [ f f 0.2 + ] o SyncSaw.ar 0.05 * ;
: rev items swap 2 array ;
: cmb { z } z 0.3 0.3 4 CombN.ar z rev + ;
' cmb texturePostProc
hsswl 4 4 4 inf overlapTexture
