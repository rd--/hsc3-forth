\ birdies (jmcc) #6 texture=overlap,7,4,4,inf
0.4 0 1 Rand.ir + 0 0 0.8 Rand.ir 0.1 + LFPulse.kr
0 3 Rand.ir 4 + * 2 + 0.4 0 1 Rand.ir + 0 0 0.8 Rand.ir 0.1 + LFPulse.kr
0 3 Rand.ir 4 + * + 0 LFSaw.kr
1000 0 800 Rand.ir + Neg * 4000 -1200 1200 Rand.ir + + 0.05 Lag 0 SinOsc.ar
0.2 0 0.5 Rand.ir + 0 0.4 LFPulse.kr
0.02 * 0.3 Lag * -1 1 Rand.ir 1 Pan2
