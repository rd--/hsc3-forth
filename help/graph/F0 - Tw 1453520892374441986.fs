\ F0 - Tw 1453520892374441986
: b [ 1 2 3 ] 3 / ;
: c 2 pi * b * ;
: t 10 pi b * SinOsc ;
: x b 0 SinOsc 0 > b 50 * 0 SinOsc * 1 + 2 / ;
: y 300 1 32 / c SinOsc ^ 99 + x * ;
: z 1 256 / c SinOsc 1 RoundTo 50 * 99 + y + ;
: f z t 99 1 64 / 0 SinOsc ^ 0 SinOsc % Latch ;
: g 1 16 / 0 SinOsc 3 * 12 + 0 SinOsc 1 + 2 / 1 8 / c SinOsc 50 * 51 + * ;
: h g t 1 4 / 0 SinOsc * Latch ;
: o f h Blip Splay2 3 / ;
o o Sum 3 / 3 / 30 3 0.5 0.5 15 1 0.7 0.5 300 GVerb +
