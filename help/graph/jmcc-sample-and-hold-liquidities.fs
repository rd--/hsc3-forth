\ sample and hold liquidities (jmcc) #4
: r 1 200 xmousex ;
: t r 1/ ;
: c r 0 Impulse.kr 0.4 * ;
: cf 100 8000 xmousey ;
: f WhiteNoise.kr cf * 0.5 * cf + c Latch ;
: p WhiteNoise.kr c Latch ;
f 0 SinOsc.ar c t 0.1 * t 0.9 * Decay2 * p 1 Pan2 0.3 0.3 2 CombN
