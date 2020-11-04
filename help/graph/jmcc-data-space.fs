\ data space (jmcc) #2 texture=overlap,1,6,4,inf
0 200 Rand.ir 0 0 1 Rand.ir LFPulse.kr
0 40 Rand.ir 0 0 1 Rand.ir LFPulse.kr
0 8000 Rand.ir * 0 2000 Rand.ir + * 0 20 Rand.ir 0 0 1 Rand.ir LFPulse.kr
0 4 Rand.ir 0 0 1 Rand.ir LFPulse.kr
0 8000 Rand.ir * 0 2000 Rand.ir + * + 0 20 Rand.ir 0 0 1 Rand.ir LFPulse.kr
0 4 Rand.ir 0 0 1 Rand.ir LFPulse.kr
0 8000 Rand.ir * 0 2000 Rand.ir + * + 0 0.5 LFPulse.ar
0.04 * 0 3 Rand.ir LFNoise0.kr
0.8 * 1 Pan2.ar
0.15 0.35 Rand.ir 0.15 0.35 Rand.ir 3 CombL
