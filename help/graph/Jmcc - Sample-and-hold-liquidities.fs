\ sample and hold liquidities (Jmcc) #4
: clockRate 1 200 xmousex ;
: clockPeriod clockRate 1/ ;
: clock clockRate 0 impulse.kr 0.4 * ;
: centerFreq 100 8000 xmousey ;
: freq whitenoise.kr centerFreq * 0.5 * centerFreq + clock latch ;
: amp clock clockPeriod 0.1 * clockPeriod 0.9 * decay2 ;
: panPos whitenoise.kr clock latch ;
: in freq 0 sinosc amp * panPos 1 pan2 ;
in 0.3 0.3 2 combn

\ sample and hold liquidities (Jmcc) #4
: r 1 200 xmousex ;
: t r 1/ ;
: c r 0 Impulse.kr 0.4 * ;
: cf 100 8000 xmousey ;
: f WhiteNoise.kr cf * 0.5 * cf + c Latch ;
: p WhiteNoise.kr c Latch ;
f 0 SinOsc.ar c t 0.1 * t 0.9 * Decay2 * p 1 Pan2 0.3 0.3 2 CombN
