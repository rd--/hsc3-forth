\ hard sync sawtooth with lfo (jmcc) #6 texture=overlap,4,4,4,inf
: hsswl
    0 50 Rand.ir 30 + MidiCps { f }
    0.2 0 0 pi 2 * Rand.ir 2 mce SinOsc.kr 2 f * * 3 f * + { o }
    f f 0.2 + 2 mce o SyncSaw.ar 0.05 * ;
: rev unmce swap 2 mce ;
: cmb { z } z 0.3 0.3 4 CombN.ar z rev + ;
hsswl cmb

