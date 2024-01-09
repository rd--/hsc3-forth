\ pkt 00 (f0) ; http://www.fredrikolofsson.com/f0blog/?q=node/490
: f0 [ 100 200 300 ] [ 0.11 0.22 0.33 ] 0 SinOsc + ;
: ph0 [ 0.1 0.2 0.3 ] 0 SinOsc 2 * pi * ;
: a0 [ 0.01 0.02 0.03 ] 0 SinOsc 0.05 * 0.1 + ;
: z f0 ph0 SinOsc a0 * ;
: w 0.13 0 SinOsc 5 * 6 + 0 SinOsc 8 * 50 + 0 SinOsc ;
: s z w 0.7 1.2 0 SinOsc 0.6 * true Splay ;
s 20 5 1 0.5 25 0 1 1 300 GVerb transpose Mix
