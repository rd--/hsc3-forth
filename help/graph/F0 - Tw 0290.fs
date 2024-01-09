\ https://sccode.org/1-4Qy ; f0 ; 0290
: b [ 99 98 ] ;
: c 12 0 SinOsc ;
: o b c SinOsc c 0 > 8 * b * 0 SinOsc 1 b / 0 SinOsc * + ;
o 1 1 [ 3 4 ] / 0 SinOsc 1 % 8 AllpassC 1 64 / [ 0 1 ] SinOsc * 3 * Sin 0.5 *
