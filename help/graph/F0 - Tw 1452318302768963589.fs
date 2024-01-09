\ F0 - Tw 1452318302768963589
: b 2 pi * [ 4 3 1 2 5 ] * 5 / ;
: m 2 1 50 / b SinOsc ^ 99 * ;
: c m 50 RoundTo ;
: t b 2 pi * / b 50 / 1 99 / pi SinOsc 0 Max 0 SinOsc 2 pi * ^ SinOsc 0 Max + ;
: f b 9 / b SinOsc 3 / c + ;
: h c 50 / 2 t 5 CombC ;
: a 50 m / b 50 / 0 SinOsc 0 Max * 0.5 + ;
: z f h Blip a * ;
: o z Splay2 1.5 * 15 Hpf Tanh ;
o o Sum 2 / 0 1 1 0 Line * 50 3 0.5 0.5 15 1 0.7 0.5 300 GVerb + 5 /
