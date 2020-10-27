\ slow beating sines (jmcc) #7

( store uid, fix uid of central freq, restore uid )

: n 10 ;
: two-pi 2 pi * ;
: ph 0 two-pi Rand.ir ;
: dev -5 5 Rand.ir + 1 ph ;
: dev-3 { f } 1 0 f 1 ph 2 0 do f dev loop ;
: freq n 0 do get-uid i set-uid 24 60 Rand.ir MIDICPS swap set-uid loop ;
: osc freq 0 n 0 do swap dev-3 9 mce Klang.ar + loop ;
osc osc 2 mce 0.1 n / *
4 4 3 inf overlap-texture
