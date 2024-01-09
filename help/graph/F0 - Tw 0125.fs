\ tw 0125 (f0)
: f 1 [ 8 9 ] / 0 SinOsc 4 * [ 400 202 ] + ;
: u 1 9 / 0 SinOsc 1 + 88 / ;
: d 1 8 / 0 SinOsc 1 + 99 / ;
: z [ 1 0 ] 1 InFeedback ;
: p z u d LagUd 1 0.08 9 CombC ;
f p SinOsc 0.25 *
