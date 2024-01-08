\ slow beating sines (Jmcc) #7 texture=overlap,4,4,3,inf
: n 10 ;
: two-pi 2 pi * ;
: ph 0 two-pi Rand.ir ;
: dev -5 5 Rand.ir + 1 ph ;
: dev-3 { f } 1 0 f 1 ph 2 0 do f dev loop ;
: freq n 0 do get-uid i set-uid 24 60 Rand.ir MidiCps swap set-uid loop ;
: osc freq 0 n 0 do swap dev-3 9 array Klang.ar + loop ;
[ osc osc ] 0.1 n / *
( store uid, fix uid of central freq, restore uid )
