\ integer forth

1 2 3 . . . \ 3 2 1
2 2 + . \ 4
2 1 - . \ 1
7 3 mod . \ 1
3 4 + 5 * . \ 35
3 4 5 * + . \ 23
2 negate . \ -2
10 2 / . \ 5
7 3 /mod . . \ 2 1
1 2 drop . \ 1
1 2 .s over .s drop drop drop .s \ <2> 1 2 <3> 1 2 1 <0>
1 2 .s swap .s drop drop .s \ <2> 1 2 <2> 2 1 <0>
1 2 3 .s rot .s drop drop drop .s \ <3> 1 2 3 <3> 2 3 1 <0>
1 2 .s nip .s drop .s \ <2> 1 2 <1> 2 <0>
1 2 .s tuck .s drop drop drop .s \ <2> 1 2 <3> 2 1 2 <0>
1 2 2dup .s . . . . .s \ <4> 1 2 1 2 2 1 2 1 <0>
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
: five 5 0 do 5 loop ;
five .s . . . . . \ <5> 5 5 5 5 5 5 5 5 5 5
: seven-eleven 11 7 do i . loop ;
seven-eleven \ 7 8 9 10
: mtable 11 1 do dup i * . loop drop ;
5 mtable \ 5 10 15 20 25 30 35 40 45 50
: n-dup 0 do dup loop ;
5 4 n-dup .s \ <5> 5 5 5 5 5
: n-drop 0 do drop loop ;
5 n-drop .s \ <0>
: tbl 3 0 do 12 10 do i j + . loop loop ;
tbl \ 10 11 11 12 12 13
: star 42 emit ;
star star star \ ***
: stars 0 do star loop ;
10 stars \ **********
: cr 10 emit ;
: f 5 0 do cr loop ; f \ \n\n\n\n\n
: box 0 do cr dup stars loop drop ;
3 3 box \ \n***\n***\n***
: space 32 emit ;
: spaces 0 do space loop ;
: \stars 0 do cr i spaces 10 stars loop ;
3 \stars

\ ugen forth

440 0 SinOsc.ar 0.1 * play
440 441 2 mce 0 SinOsc.ar 0.1 * play
440 441 2 mce 0 SinOsc.ar 1 0 SinOsc.kr 0.1 * Abs * play
WhiteNoise.ar 0.1 * play
WhiteNoise.ar 0.1 * dup - draw \ silence
WhiteNoise.ar 0.1 * 2 clone unmce - draw \ noise
