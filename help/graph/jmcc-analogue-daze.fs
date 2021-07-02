\ analogue daze (jmcc) #3
: twoPi 2 pi * ;
: rev unmce swap 2 mce ;
: sequ { s tr } tr 0 inf s Dseq.dr Demand.kr ;
: ntSeq -5 3 0 3 -3 5 2 5 8 mce 12 / ;
: freq { oct clkRt pwmRt fltRt }
    clkRt 0 Impulse.kr { tr }
    ntSeq oct + ohz tr sequ { sq }
    pwmRt 0 twoPi Rand.ir SinOsc.kr 0.4 * 0.5 + { pwm }
    fltRt 0 twoPi Rand.ir SinOsc.kr 1400 * 2000 + { cf }
    sq 0.05 Lag 0 pwm LFPulse.ar 0.1 * cf 1 15 / RLPF ;
: noise 0.3 LFNoise1.kr 6000 * 8000 + LFNoise0.ar 0.07 0.08 2 mce * ;
: pulses 2 0 Impulse.ar 0.15 Decay noise * ;
: plain 1 8 0.31 0.2 freq 0 2 0.13 0.11 freq 2 mce pulses + ;
: echoed plain 0.375 0.375 5 CombN plain rev + 0.4 * ;
: env 1 1 0 1 removeSynth 2 56 2 1 envLinen EnvGen.kr ;
echoed env *
