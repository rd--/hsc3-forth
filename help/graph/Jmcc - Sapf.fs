\ analog bubbles ((.4 0 lfsaw) 2 * ([8 7.23] 0 lfsaw) .25 * 9.667 + + exp2) 0 sinosc .04 * .2 0 4 combn play

0.4 0 lfsaw 2 * [ 8 7.23 ] 0 lfsaw 0.25 * 9.667 + + exp2 0 sinosc 0.04 * 0.2 0.2 4 combn

\ analog bubbles (([.4 [8 7.23]] 0 lfsaw) [24 3] * +/ 81 + nnhz) 0 sinosc 4c * .2 0 4 combn play

[ 0.4 [ 8 7.23 ] ] 0 lfsaw [ 24 3 ] * +/ 81 + nnhz 0 sinosc 0.04 * 0.2 0.2 4 combn

\ analog bubbles [.4 [8 7.23]] ; LFO freq\n 0 lfsaw ; sawtooth LFO\n [24 3] * ; modulation depth in semitones\n +/ ; add mono and stereo LFOs\n 81 + ; midi note offset\n nnhz ; convert note number to Hertz\n 0 sinosc ; sine wave oscillator\n 4c * ; scale amplitude\n .2 0 4 combn ; comb delay\n ] ! play

[ 0.4 [ 8 7.23 ] ] ( LFO freq ) 0 lfsaw ( sawtooth LFO ) [ 24 3 ] * ( modulation depth in semitones ) +/ ( add mono and stereo LFOs ) 81 + ( midi note offset ) nnhz ( convert note number to Hertz ) 0 sinosc ( sine wave oscillator ) 0.04 * ( scale amplitude ) 0.2 0.2 4 combn ( comb delay )

\ moto rev .2 0 11 31 lfo [0 3c] .1 lfpulse 1h .1 rlpf .4 clip2 play

0.2 0 11 31 lfo [ 0 0.03 ] 0.1 lfpulse 100 0.1 rlpf 0.4 clip2

\ sample and hold liquidities \[ 1 2h xmousex aa 1/ = ckt 0 impulse = ck 1h 8k xmousey = f f .5 * white f + ck sah 0 sinosc ck 1.2 ckt * decay .1 ckt * lag * 1 rand2s ck seq pan2 aa .3 0 2 combn + ] !

: _ 1 200 xmousex dup 1/ { ckt } 0 impulse.kr { ck } 100 8000 xmousey { f } f 0.5 * whitenoise.kr * f + ck latch 0 sinosc ck 1.2 ckt * decay 0.1 ckt * lag * whitenoise.kr ck latch 1 pan2 dup 0.3 0.3 2 combn + ; _ 0.1 *

\ sample and hold liquidities \[ 1 2h xmousex = clockRate clockRate 1/ = clockPeriod clockRate 0 impulse = clock 1h 8k xmousey = centerFreq ((.5 1.5 rands) clock seq) (centerFreq clock sah) * = freq (clock (1.2 clockPeriod *) decay) (.1 clockPeriod *) lag = amp (1 rand2s) clock seq = panpos (freq 0 sinosc) amp * panpos pan2 = in (in .3 0 2 combn) in + aa pr cr ] !

: _ 1 200 xmousex { clockRate } clockRate 1/ { clockPeriod } clockRate 0 impulse.kr { clock } 100 8000 xmousey { centerFreq } whitenoise.kr 0.5 * 1 + clock latch centerFreq clock latch * { freq } clock 1.2 clockPeriod * decay 0.1 clockPeriod * lag { amp } whitenoise.kr clock latch { panpos } freq 0 sinosc amp * panpos 1 pan2 { in } in 0.3 0 2 combn in + ; _ 0.1 *

\ sweepy noise (3c 2X white) ((4 60 xmousex 0 lfsaw) 1.2 + (2h 8k xmousey) *) 2m lag .1 rlpf aa .3 0 2 combn +

whitenoise 2 clone 0.3 * 4 60 xmousex 0 lfsaw 1.2 + 200 8000 xmousey * 0.002 lag 0.1 rlpf dup 0.3 0.3 2 combn + 0.1 *

\ sweepy noise \[ 4 60 xmousex = rate 2h 8k xmousey = centerFreq rate 0 lfsaw = lfo (lfo 1.2 + centerFreq *) 2m lag = fc 3c 2X white = in in fc .1 rlpf aa .3 0 2 combn + ]

: _ 4 60 xmousex { rate } 20 8000 xmousey { centerFreq } rate 0 lfsaw { lfo } lfo 1.2 + centerFreq * 0.002 lag { fc } whitenoise 2 clone 0.3 * { in } in fc 0.1 rlpf dup 0.3 0 2 combn + ; _ 0.1 *

\ lots of sine lfos ((.027 0 .05 .2 xlfo) 0 40 4k xlfo) [-.2 .2] + 0 saw (.17 0 1h 6k xlfo) .1 rlpf (.041 0 2 20 xlfo) [0 .25] sinosc biuni * .2 * .3 0 3 combn

0.027 0 0.05 0.2 xlfo 0 40 4000 xlfo [ -0.2 0.2 ] + saw 0.17 0 100 6000 xlfo 0.1 rlpf 0.041 0 2 20 xlfo [ 0 0.25 ] sinosc biuni * 0.2 * 0.3 0.3 3 combn 0.5 *

\ hell is busy \[ ((4h 2k rand) 0 sinosc) ((1 11 rand) 0 (0 .7 rand) lfpulse) 4m lag 4c * * rpan2 4 quadenv *] .5 1 2 ola play

400 2000 rand 0 sinosc 1 11 rand 0 0 0.7 rand lfpulse 0.004 lag 0.04 * * rpan2 4 4 8 oltx

\ pond life \[((20 50 rand 0 sinosc 1h 4h rand 5h 25h linrand *+) 0 sinosc) (((3 1 9 rand /) 0 (.2 .5 rand) lfpulse) 4m lag 4c *) * rpan2] 8 4 8 2 oltx

20 50 rand 0 sinosc 100 400 rand 500 2500 0 linrand *+ 0 sinosc 3 1 9 rand / 0 0.2 0.5 rand lfpulse 0.004 lag 0.04 * * rpan2 8 4 8 oltx

\ pulse width modulation and resonant filter (((#[48 54 64] cyc 4 1 steps) [0 .5] +) 0 (.2 urand sinosc .98 * biuni) pulse) ((.14 urand sinosc) 100 6k biexp) .1 rlpf .3 * .27 0 2 combn play

48 54 0 0.2 mousey [ 0 0.5 ] + 0.2 urand sinosc 0.98 * biuni pulse 0.14 urand sinosc 100 6000 biexp 0.1 rlpf 0.3 * 0.27 0.27 2 combn 0.5 *

\ scratchy .5 2X brown .49 - 0 | 20 * 5k 1 rhpf play

brownnoise 2 clone 0.5 * 0.49 - 0 max 20 * 5000 1 rhpf

\ rain \[2 .2 dust2 .25 decay .05 lfnoise3 8h 13k biexp .18 bpf .08 lfnoise3 pan2 .08 .01 apverb6] 16 X +/ .01 2X pink + .005 2X brown + play

2 dust2 0.2 * 0.25 decay 0.05 lfnoise2 800 13000 biexp 0.18 bpf 0.08 lfnoise2 1 pan2 0.08 0.01 apverb6 16 clone +/ pinknoise 2 clone 0.01 * + brownnoise 2 clone 0.005 * +

\ fast LFOs with slow beats \[ 40 240 rand = a0 a0 brand + = a1 [a0 a1] = a 0 2k rand = b [a0 brand + a1 brand +] = c a 0 sinosc urand b * * b + 0 sinosc c 0 sinosc 5c * 5c + * ] 8 4 4 2 oltx play

: _ 40 240 rand { a0 } a0 brand + { a1 } [ a0 a1 ] { a } 0 2000 rand { b } [ a0 brand + a1 brand + ] { c } a 0 sinosc urand b * * b + 0 sinosc c 0 sinosc 0.05 * 0.05 + * ; _ 8 4 4 2 oltx

\ reverberated sine percussion \[ 10 = d 7 = c \[2 d / .2 dust 2h 32h rand .003 resonz] d X +/ = s (s .048 aa delayn) (c 0 .1 nrands lfnoise1 4c * 5c +) .1 15 combl +/ 5c 1 apverb4 .2 * s + ] = rsp rsp play

: _ 10 { d } 7 { c } 2 d / dust 200 3200 rand 0.003 resonz d clone +/ { s } s 0.048 dup delayn c 0 0.1 nrands lfnoise1.kr 0.04 * 0.05 + 0.1 15 combl +/ 0.05 1 apverb4 0.2 * s + ; _ 25 *

\ zizle 1 \[ 24 108 irand nnhz 0 1 rand sinosc (.3 8 xrand [.7 1.3 rand 1] *) (2 0 1 nrands) sinosc +/ .1 * 0 | (6 24 xrand [.7 1.3 rand 1] *) (2 0 1 nrands) sinosc +/ .1 * abs * * rpan2 ] 4 4 12 2 oltx play

24 108 irand nnhz 0 1 rand sinosc 0.3 8 exprand [ 0.7 1.3 rand 1 ] * 0 1 rand 2 clone sinosc +/ 0.1 * 0 max 6 24 exprand [ 0.7 1.3 rand 1 ] * 0 1 rand 2 clone sinosc +/ 0.1 * abs * * rpan2 4 4 12 oltx

\ data space \[(0 100 rand 0 urand lfpulse) (0 40 rand 0 urand lfpulse 0 8k rand * 0 2k rand +) *] 3X +/ = freq freq 0 .5 lfpulse 4c *  0 3 rand lfnoise0 .8 * 4m lag pan2  .1 .35 rand aa 3 combl ] 6 1 4 2 oltx ] play

: _ 0 100 rand 0 urand lfpulse 0 40 rand 0 urand lfpulse 0 8000 rand * 0 2000 rand + * 3 clone +/ { freq } freq 0 0.5 lfpulse 0.04 *  0 3 rand lfnoise0 0.8 * 0.004 lag 1 pan2 0.1 0.35 rand dup 3 combl ; _ 6 1 4 oltx

\ birdies \[ ((\[.4 .5 rand 0  .1 .9 rand lfpulse 4 7 rand *] 2X +/ 2 +) 0 lfsaw (-1k -1.8k rand)(28h 52h rand) *+ 5c lag) 0 sinosc  (.2 .7 rand) 0 .4 lfpulse 2c * .3 lag * rpan2] 7 4 4 2 oltx 2c 1.5 apverb4 play

0.4 0.5 rand 0 0.1 0.9 rand lfpulse 4 7 rand * 2 clone +/ 2 + 0 lfsaw -1000 -1800 rand 2800 5200 rand *+ 0.05 lag 0 sinosc 0.2 0.7 rand 0 0.4 lfpulse 0.02 * 0.3 lag * rpan2 0.02 1.5 apverb4 7 4 4 oltx

\ babbling brook \[ \a b c d [ \[(1 brown)(1 brown a lpf b c *+) 3c rhpf d *] 2X] = f  (10 2h 4h 6m f) (14 4h 8h 10m f) +] ! play

: f { a b c d } brownnoise brownnoise a lpf b c *+ 0.03 rhpf d * 2 clone ; 10 200 400 0.006 f 14 400 800 0.010 f +

\ alien meadow \[(0 20 rand 0 sinosc) 0 5k rand aa .1 * ba *+ 0 sinosc 0 20 rand 0 sinosc 5c aa *+ * rpan2] 6 2 6 2 oltx play

0 20 rand 0 sinosc 0 5000 rand dup 0.1 * swap *+ 0 sinosc 0 20 rand 0 sinosc 0.05 dup *+ * rpan2 6 2 6 oltx
