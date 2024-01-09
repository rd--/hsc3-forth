\ tw 0045 (f0) ; http://www.fredrikolofsson.com/f0blog/?q=node/537
: f 0.11 0 SinOsc 0 SinOsc ;
: pf 95 0.01 0 SinOsc 1 + * 0 SinOsc 0.005 0 SinOsc 50 * * 100 + ;
: p pf [ 98 97 ] 0 SinOsc SinOsc pi 0.0005 0 SinOsc + * ;
f p SinOsc Tanh 0.1 *
