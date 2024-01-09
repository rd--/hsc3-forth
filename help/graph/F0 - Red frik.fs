\ Red frik (F0)
: tr 0.1 0 Impulse ;
: o1 0.3 5 tr TRand 0 SinOsc 0 0.5 tr TRand 0.49 0.56 tr TRand MulAdd ;
: o2 o1 0 SinOsc 0.3 0.6 tr TRand 0.3 0.5 tr TRand MulAdd ;
BrownNoise 2 clone 0.3 3 tr TRand o2 Rhpf 0.1 *
