\ https://sccode.org/1-4Qy ; f0 ; 0270
: d 0 1 InFeedback ;
: f 1 [ 9 8.9 ] / ;
: c d 0.01 0.01 Amplitude f 0 SinOscFb < f 9 / Lag d + ;
98.5 c + d f + SinOscFb c * 1 f 3 * 20 AllpassN Tanh
