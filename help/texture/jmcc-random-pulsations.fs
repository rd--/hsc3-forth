\ random pulsations (jmcc) #1

0 2000 Rand.ir 0 FSinOsc.ar 1 1 0 1 0 2 5 2 0.02 env-linen EnvGen.kr *
8 88 0 LinRand.ir 0 SinOsc.ar AMClip 0.3 0.8 Rand.ir 0 6.28319 Rand.ir SinOsc.kr 0.7 * 1 Pan2
9 8 / inf spawn-texture
