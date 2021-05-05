\ http://sccode.org/1-V (nv) L1
: nd 0 0.05 Rand.ir LFNoise1.kr -1 1 40 15000 LinExp 0.1 2 ExpRand.ir BBandStop ;
: flt 50 0 do nd loop ;
PinkNoise.ar 2 clone flt 100000 LPF
