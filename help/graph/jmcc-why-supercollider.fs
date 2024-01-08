\ Why supercollider? (Jmcc) #0
: rvb 5 0 do 0.05 0 0.05 Rand.ir 2 clone 1 AllpassN loop ;
0.2 Dust.ar 50 * 200 3200 Rand.ir 0.003 Resonz 10 clone mix dup
0.046 0.048 DelayN
0.1 0 0.1 Rand.ir LfNoise1.kr 0.04 * 0.05 + 15 CombL 7 clone mix
rvb 0.2 * +
