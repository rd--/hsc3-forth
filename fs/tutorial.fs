\ EMACS

\ M-x set-variable forth-program-name "hsc3-forth"
\ M-x run-forth
\ M-x forth-send-paragraph

\ LITERAL FORTH

1 2 3 . . . \ 3 2 1 \

\ NUM FORTH

2 2 + . \ 4 \
2 1 - . \ 1 \
3 4 + 5 * . \ 35 \
3 4 5 * + . \ 23 \
: negate Neg ;
2 negate . \ -2 \

\ FRACTIONAL FORTH

( ANS Forth requires floating point literals be written 1.1e0 etc. )
( ANS Forth has a separate floating point stack, printed using f. )

: f. . ;
1.1e0 2.2e0 3.3e0 f. f. f. \ 3.3 2.2 1.1 \

( SC3 has only floating point numbers & only one stack )

1.1 2.2 3.3 . . . \ 3.3 2.2 1.1 \

: f/ / ;
5 2 f/ f. \ 2.5 \

\ INTEGRAL FORTH

: div IDiv ;
10 2 div . \ 5 \
7 3 mod . \ 1 \
7 3 /mod . . \ 2 1 \

\ STDLIB FORTH

( In ANS Forth true is -1.  In SC3 true is 1. )

: true 1 ;
: false 0 ;

\ EQ & ORD FORTH

: = == ;
0 1 = . \ false \
1 1 = . \ true \
1 2 < . \ true \
2 1 < . \ false \
1 1 < . \ false \
1 1 <= . \ true \

\ STACK FORTH

1 2 drop . \ 1 \
1 2 .s over .s drop drop drop .s \ <2> 1 2 <3> 1 2 1 <0> \
1 2 .s swap .s drop drop .s \ <2> 1 2 <2> 2 1 <0> \
1 2 3 .s rot .s drop drop drop .s \ <3> 1 2 3 <3> 2 3 1 <0> \
1 2 .s nip .s drop .s \ <2> 1 2 <1> 2 <0> \
1 2 .s tuck .s drop drop drop .s \ <2> 1 2 <3> 2 1 2 <0> \
1 2 2dup .s . . . . .s \ <4> 1 2 1 2 2 1 2 1 <0> \

\ BLOCK FORTH

: squared dup * ;
5 squared . \ 25 \
7 squared . \ 49 \
: cubed dup squared * ;
-5 cubed . \ -125 \
: fourth-power squared squared ;
3 fourth-power . \ 81 \

\ CONDITIONAL FORTH

: abs dup 0 < if negate then ;
5 abs . \ 5
-5 abs . \ 5

: min 2dup < if drop else nip then ;
2 3 min . \ 2
3 2 min . \ 2

\ DO FORTH

: five 5 0 do 5 loop ;
five .s . . . . . \ <5> 5 5 5 5 5 5 5 5 5 5 \
: seven-eleven 11 7 do i . loop ;
seven-eleven \ 7 8 9 10 \
: mtable 11 1 do dup i * . loop drop ;
5 mtable \ 5 10 15 20 25 30 35 40 45 50 \
: n-dup 0 do dup loop ;
5 4 n-dup .s \ <5> 5 5 5 5 5 \
: n-drop 0 do drop loop ;
5 n-drop .s \ <0> \
: tbl 3 0 do 12 10 do i j + . loop loop ;
tbl \ 10 11 11 12 12 13 \
: star 42 emit ;
star star star \ *** \
: stars 0 do star loop ;
10 stars \ ********** \
: cr 10 emit ;
: f 5 0 do cr loop ; f \ \n\n\n\n\n \
: box 0 do cr dup stars loop drop ;
3 3 box \ \n***\n***\n*** \
: space 32 emit ;
: spaces 0 do space loop ;
: \stars 0 do cr i spaces 10 stars loop ;
3 \stars

\ UGEN FORTH

440 0 SinOsc.ar 0.1 * play
440 441 2 mce 0 SinOsc.ar 0.1 * play
440 441 2 mce 0 SinOsc.ar 1 0 SinOsc.kr 0.1 * Abs * play
WhiteNoise.ar 0.1 * play
stop

\ DRAWING FORTH

WhiteNoise.ar 0.1 * dup - draw \ silence \
WhiteNoise.ar 0.1 * 2 clone unmce - draw \ noise \
