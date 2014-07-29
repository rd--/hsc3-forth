( EMACS FORTH )

\ HSC3-FORTH MODE
\ Commands have a with C-cC- prefix
\ <point> is the word at the cursor
\ > -- Start & see the HSC3-FORTH interpreter
\ c -- Send line the cursor is in
\ d -- Send the current region (selection)
\ g -- Send <point> DRAW
\ a -- Send <point> PLAY
\ u -- Send ? <point>
\ k -- Send STOP
\ s -- Send KILLALL
\ q -- Send BYE

( HELP FORTH )

s" SinOsc" ?
\ SinOsc [AR,KR] freq=440.0 phase=0.0

s" ENVGEN" ?
\ EnvGen [AR,KR] *envelope=0 gate=1 levelScale=1 levelBias=0 timeScale=1 doneAction=0
\     MCE INPUT: #5, REORDERS INPUTS: [5,0,1,2,3,4], ENUMERATION INPUTS: 4=DoneAction

( LITERAL FORTH )

1 2 3 . . . \ 3 2 1 \

( NUM FORTH )

2 2 + . \ 4 \
2 1 - . \ 1 \
3 4 + 5 * . \ 35 \
3 4 5 * + . \ 23 \
2 negate . \ -2 \

( FRACTIONAL FORTH )

\ ANS Forth requires floating point literals be written 1.1e0 etc.
\ ANS Forth has a separate floating point stack, printed using f.

: f. . ;
1.1e0 2.2e0 3.3e0 f. f. f. \ 3.3 2.2 1.1 \

\ SC3 has only floating point numbers & only one data stack

1.1 2.2 3.3 . . . \ 3.3 2.2 1.1 \

: f/ / ;
5 2 f/ f. \ 2.5 \

( INTEGRAL FORTH )

10 2 div . \ 5 \
7 3 mod . \ 1 \
7 3 /mod . . \ 2 1 \

( EQ FORTH )

0 1 = . \ false \
1 1 = . \ true \

( ORD FORTH )

1 2 < . \ true \
2 1 < . \ false \
1 1 < . \ false \
1 1 <= . \ true \

( STACK FORTH )

1 2 drop . \ 1 \
1 2 .s over .s drop drop drop .s \ <2> 1 2 <3> 1 2 1 <0> \
1 2 .s swap .s drop drop .s \ <2> 1 2 <2> 2 1 <0> \
1 2 3 .s rot .s drop drop drop .s \ <3> 1 2 3 <3> 2 3 1 <0> \
1 2 .s nip .s drop .s \ <2> 1 2 <1> 2 <0> \
1 2 .s tuck .s drop drop drop .s \ <2> 1 2 <3> 2 1 2 <0> \
1 2 2dup .s . . . . .s \ <4> 1 2 1 2 2 1 2 1 <0> \
1 2 3 4 5 2 pick .s . . . . . . .s \ <6> 1 2 3 4 5 3 3 5 4 3 2 1 <0> \

( BLOCK FORTH )

: squared dup * ;
5 squared . \ 25 \
7 squared . \ 49 \
: cubed dup squared * ;
-5 cubed . \ -125 \
: fourth-power squared squared ;
3 fourth-power . \ 81 \

( CONDITIONAL FORTH )

5 abs . \ 5
-5 abs . \ 5

2 3 min . \ 2
3 2 min . \ 2

( DO FORTH )

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
: f 5 0 do cr loop ; f \ \n\n\n\n\n \
: box 0 do cr dup stars loop drop ;
3 3 box \ \n***\n***\n*** \
: \stars 0 do cr i spaces 10 stars loop ;
3 \stars

( LOCALS FORTH )

: swap' { a b } b a ;
1 2 swap' . . \ 1 2 \
: pattern { a b c } a b c b c b a ;
1 2 3 pattern . . . . . . . \ 1 2 3 2 3 2 1 \
: f { a } 2 { b } a b a ;
1 f . . . \ 1 2 1 \

( UGEN FORTH )

440 0 SinOsc.ar 0.1 * 0 swap Out -1 add-to-head 1 play-at
440 441 2 mce 0 SinOsc.ar 0.1 * play
440 441 2 mce 0 SinOsc.ar 1 0 SinOsc.kr 0.1 * Abs * play
WhiteNoise.ar 0.1 * play

\ Free all nodes at scsynth (C-cC-k)
stop

( DRAWING FORTH )

\ Draw unit-generator graph, requires graphviz dot language interpreter, (C-cC-g)

WhiteNoise.ar 0.1 * dup - draw \ silence \
WhiteNoise.ar 0.1 * 2 clone unmce - draw \ noise \

( NAMING FORTH )

440 Rand 440 + 0 SinOsc.ar 0.1 * draw \ Rand is a unary operator
440 880 Rand.ir 0 SinOsc.ar 0.1 * draw \ Rand.ir is a UGen
440 0 SinOsc.ar 0.1 * Neg draw \ Neg is a unary operator
440 0 SinOsc.ar 0.1 * negate draw \ negate is a stdlib word

( RANDOM FORTH )

: random-sine 1900 2300 Rand.ir 0 SinOsc.ar -1 1 Rand.ir 0.05 0.15 Rand.ir Pan2.ar ;
: _ 5 0 do random-sine play loop ; _
stop

( UN-RANDOM FORTH )

\ The non-deterministic UGens get identifiers from a counter, which can be set.
1376523 seed

\ The unrand transformation lifts scalar random UGens to constants.
0 1 Rand.ir 2 clone .s \ <1> [UGEN:Rand UGEN:Rand]
unrand . \ [0.6768026553207348 0.21705544209066452]

( ENVELOPED FORTH )

: with-triangle-env { dur lvl } 1 1 0 1 remove-synth dur lvl env-tri EnvGen.kr * ;
WhiteNoise.ar 10 0.1 with-triangle-env play
random-sine 5 0.1 with-triangle-env play
stop

( PAUSING FORTH )

: anon 11 1 do i 4 / dup . cr pause random-sine 5 0.1 with-env play loop ;
anon 5 pause stop

( SCHEDULE FORTH )

\ Since random-sine takes time, there is audible scattering.
: _ 25 0 do random-sine play loop ; _
stop

\ Forward scheduling allows for precise alignment.
: _ time 0.1 + { t } 25 0 do random-sine t sched loop ; _
stop

( TEXTURAL FORTH )

random-sine 2 3 5 xfade-texture
random-sine 2 3 6 12 overlap-texture

( FORK FORTH )

: endless inf 0 do s" MSG" type cr 1 pause loop ;
fork endless .s
kill

\ The forked word can read from the stack, but it does so in the new thread.
: n-messages 0 do i . cr dup pause loop ;
0.5 10 fork n-messages .s \ <3> 0.5 10 THREAD-ID

\ Here the interval and the count remain on the stack, along with the thread-id.
kill . . .s \ 10 0.5 <0>

\ The VM keeps a list of all running threads, and the can be killed altogether (C-cC-s)
killall

( INCLUSIVE FORTH )

s" /home/rohan/sw/hsc3-graphs/gr/why-supercollider.fs" included

\ If the included file is a process we can fork included, with the normal fork stack rules.
s" /home/rohan/sw/hsc3-graphs/gr/alien-meadow.fs" fork included .s
kill . . \ 45 STRING:"/home/rohan/sw/hsc3-graphs/gr/alien-meadow.fs"

( QUOTING FORTH )

' + 1 2 rot execute . \ 3

: cmb 0.1 0.1 1 CombN ;
: post-proc { f } 0 2 In.ar f execute 0 swap Out dup draw -1 add-to-tail 1 play-at ;
' cmb post-proc
stop

( RETURN FORTH )

1 >r .s r> . \ <0> 1
>r \ ERROR

( LABELED FORTH )

s" LABEL" label . \ "LABEL"

( FIBONACCI FORTH )

: fib 0 1 rot 0 do over + swap loop drop ;
: fibs 0 do i fib . loop ;
50 fibs \ 0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 ...
.s

( TROUBLE FORTH )

vmstat \ PRINT VM STATUS
2 trace \ SET TRACE LEVEL PRIORITY, 0=HIGH, 1=MEDIUM, 2=LOW (DEFAULT=-1, NO TRACING)

( FINISHING FORTH )

bye \ C-cC-q
