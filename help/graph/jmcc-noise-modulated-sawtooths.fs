\ noise modulated sawtooths (jmcc) #6 texture=overlap,4,4,4,inf
: f 60 100 Rand.ir MIDICPS ;
: nms f f 0.2 + 2 mce 0 LFSaw.ar f 0.15 0.16 2 mce * LFNoise2.kr 0.1 * * ;
: pp { z } z 0.3 0.3 4 CombN.ar z unmce swap 2 mce + ;
nms pp
