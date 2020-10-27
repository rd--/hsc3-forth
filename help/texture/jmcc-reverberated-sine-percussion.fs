\ reverberated sine percussion (jmcc) #3

: rsp { d c a }
  d 0 do 2 d / Dust.ar 50 * 200 0 3000 Rand.ir + 0.003 Resonz loop d mce mix { s }
  s 0.048 0.48 DelayN 0.1 c 0 do 0 0.1 Rand.ir loop c mce LFNoise1.kr
  0.04 * 0.05 + 15 CombL mix
  a 0 do 0.05 0 0.05 Rand.ir 2 clone 1 AllpassN loop 0.2 * s + ;
6 5 4 rsp
10 10 2 inf overlap-texture
