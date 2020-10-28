\ cut-outs (rd)
: t 22 0 Impulse.ar 0.5 0 SinOsc.kr 1 + * ;
: x 0.005 0.12 1 0.1 MouseX.kr ;
: y 0.01 0.52 1 0.1 MouseY.kr ;
: n 2 LFNoise0.kr ( n1 )
    0.05 + 0.4 y * + 0.5 t * + 0.5 t * CoinGate.ar ( n2 )
    500 900 2 mce 1600 t TExpRand.ar ( n3 )
    x Ringz.ar ;
: s n n n 3 mce mix ;
: b 0 1 8 Dust.kr TRand.kr ;
0 1 In.kr s swap Clip2 0.25 *
0 b Out.kr
2 mrg
