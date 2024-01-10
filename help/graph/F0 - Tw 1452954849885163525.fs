\ F0 - Tw 1452954849885163525
: tr 1 9 / 0 Impulse ;
: z 1 64 tr TRand ;
: x z 9.33 % 0 SinOsc 5 5 MulAdd Ceil ;
: t 2 z 11 % ^ 150 * x / 0 SinOsc ;
: y x Hpz1 Abs 0 > ;
: f t
    z 4.4 % 0 SinOsc -1 1 9 999 LinExp y Latch
    z pi % 0 SinOsc -1 1 99 9000 LinExp y Latch
    ExpRange ;
f t 2 + Blip 1 t - * 0.1 z SinOsc 0.1 Pan2
