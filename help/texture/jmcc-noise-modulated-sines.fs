\ Noise modulated sines (jmcc) #6
: f 60 100 Rand.ir MidiCps ;
: nms [ f f 0.2 + ] 0 FSinOsc.ar f [ 0.15 0.16 ] * LFNoise2.kr 0.1 * * ;
: pp { z } z 0.3 0.3 4 CombN.ar z items swap 2 array + ;
' pp texturePostProc
nms 4 4 4 inf overlapTexture
