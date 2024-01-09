\ tw 0077 (f0) - http://sccode.org/1-4Qy
: f 1 [ 5 6 ] / 0 SinOsc [ 798 912 ] + ;
: p 1 16 / 0 SinOsc 19 * 99 + [ 9 8 ] 0 SinOsc * ;
: m 6 0 SinOsc 0.009 0 SinOsc * 0 SinOsc ;
: b f p SinOsc m * ;
[ 201 301 ] b SinOsc Tanh 0.25 *
