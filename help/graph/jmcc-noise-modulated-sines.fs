\ Noise modulated sines (Jmcc) #6 texture=overlap,4,4,4,inf
: f 60 100 Rand.ir MidiCps ;
: nms [ f f 0.2 + ] 0 FSinOsc.ar f [ 0.15 0.16 ] * LfNoise2.kr 0.1 * * ;
: pp { z } z 0.3 0.3 4 CombN.ar z reverse + ;
nms pp
