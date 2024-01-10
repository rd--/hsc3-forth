\ F0 - Tw 1115745664523218944
: b [ 1 2 6 3 ] 0.0345 0 SinOsc 0 < + ;
: c 2 b 0 SinOsc ^ ;
: d 1 b / 0 SinOsc b 3 / b 0.0234 0 SinOsc 0 < 1 + * MulAdd 0 RoundTo ;
: f c 0 0 inf 99 d * Dseq Duty ;
: n f b SinOsc ;
: o n 1.5 c / 0 SinOsc 0.0123 0 SinOsc 0.5 * 0.5 - Max * ;
: z o 1 7 b - 12 / c 0.6 < 0 SinOsc 1 % 0.1 + * 2 0.1 0 SinOsc 0.1 * b SinOsc 9 * ^ CombC ;
z Splay2 9 Hpf Tanh 5 /
