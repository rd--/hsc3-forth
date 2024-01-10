\ Rand ; Random sine tones
: o 440 880 Rand.ir ;
[ o o ] 0 SinOsc.ar 0.1 *

\ Rand ; Random sine tones using clone
440 880 Rand.ir 2 clone 0 SinOsc.ar 0.1 *
