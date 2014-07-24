\ integer forth

1 2 3 . . . \ 3 2 1
2 2 + . \ 4
2 1 - . \ 1
7 3 mod . \ 1
3 4 + 5 * . \ 35
3 4 5 * + . \ 23
2 negate . \ -2
7 3 /mod . . \ 2 1
1 2 drop . \ 1
1 2 .s over .s drop drop drop .s \ [1,2] [1,2,1] []
1 2 .s swap .s drop drop .s \ [1,2] [2,1] []
1 2 3 .s rot .s drop drop drop .s \ [1,2,3] [2,3,1] []
1 2 .s nip .s drop .s \ [1,2] [2] []
1 2 .s tuck .s drop drop drop .s \ [1,2] [2,1,2] []
1 2 2dup .s . . . . .s \ [1,2,1,2] 2 1 2 1 []
: squared dup * ;
5 squared . \ 25
7 squared . \ 49
: cubed dup squared * ;
-5 cubed . \ -125
: fourth-power squared squared ;
3 fourth-power . \ 81
: abs dup 0 < if negate then ;
5 abs . \ 5
-5 abs . \ 5
1 2 < . \ -1 ie. true
2 1 < . \ 0 ie. false
1 1 < . \ 0 ie. false
: min 2dup < if drop else nip then ;
2 3 min . \ 2
3 2 min . \ 2

\ ugen forth

440 0 SinOsc.ar 0.1 * play
440 441 2 mce 0 SinOsc.ar 0.1 * play
440 441 2 mce 0 SinOsc.ar 1 0 SinOsc.kr 0.1 * Abs * play
WhiteNoise.ar 0.1 * play
WhiteNoise.ar 0.1 * dup - draw \ silence
WhiteNoise.ar 0.1 * 2 clone unmce - draw \ noise
