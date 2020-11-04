\ alien meadow (jmcc) #6 texture=overlap,2,6,6,inf
: alien-meadow
  0 20 Rand.ir { a }
  0 5000 Rand.ir { b }
  0 20 Rand.ir { c }
  -1 1 Rand.ir { p }
  a 0 SinOsc.ar b * 0.1 * b + { f }
    f 0 SinOsc.ar p c 0 SinOsc.ar 0.05 * 0.05 + Pan2.ar
;
alien-meadow
