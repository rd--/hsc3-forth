( HSC3 FORTH -- AMERICAN PRIMITIVE, VOL. 1 )

\ HSC3-FORTH is a simple FORTH-ish interpreter.
\ There is one data type, the SUPERCOLLIDER UNIT GENERATOR, and one data stack.
\ There is IF ELSE THEN, and DO LOOP, and (LOCAL) in the form { VAR ... }.
\ There is a return stack.

( EMACS FORTH )

\ You can write directly to HSC3-FORTH, or you can write to EMACS.

\ HSC3-FORTH MODE
\ Commands are C-cC- prefixed.
\ <point> is the word at the cursor.
\ > -- Start & see the HSC3-FORTH interpreter
\ c -- Send current line
\ d -- Send current region (selection)
\ g -- Send <point> DRAW
\ e -- Send <point> false SEE
\ a -- Send <point> PLAY
\ u -- Send <point> ?
\ k -- Send STOP
\ s -- Send KILLALL
\ q -- Send BYE
\ i -- Send !SIGINT!
\ p -- Send SC3-STATUS

( HELP FORTH )

s" SinOsc" ?

\ SinOsc [AR,KR] freq=440.0 phase=0.0

s" ENVGEN" ?

\ EnvGen [AR,KR] *envelope=0 gate=1 levelScale=1 levelBias=0 timeScale=1 doneAction=0
\     MCE INPUT: #5, REORDERS INPUTS: [5,0,1,2,3,4], ENUMERATION INPUTS: 4=DoneAction

( NUM FORTH )

2 2 + . \ 4 \
2 1 - . \ 1 \
3 4 + 5 * . \ 35 \
3 4 5 * + . \ 23 \
2 negate . \ -2 \

( FRACTIONAL FORTH )

1.1 2.2 3.3 . . . \ 3.3 2.2 1.1 \
5 2 / . \ 2.5 \

( INTEGRAL FORTH )

\ There is an integer division uop, IDiv and a floating point modulo operator %.

10 2 IDiv . \ IDiv 5? \
5 2 IDiv . \ IDiv 2? \
7 3 % . \ 1 \

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

\ There are Abs and Min unary operators, but also abs and min in the stdlib.

5 abs . \ 5
-5 abs . \ 5
-5 Abs . \ 5
2 3 min . \ 2
3 2 Min . \ 2

( DO FORTH )

: five 5 0 do 5 loop ;
five .s . . . . . \ <5> 5 5 5 5 5 5 5 5 5 5 \

: n-dup 0 do dup loop ;
: n-drop 0 do drop loop ;
5 4 n-dup .s \ <5> 5 5 5 5 5 \
5 n-drop .s \ <0> \

\ I fetches the loop counter

: seven-eleven 11 7 do i . loop ;
seven-eleven \ 7 8 9 10 \

: mtable 11 1 do dup i * . loop drop ;
5 mtable \ 5 10 15 20 25 30 35 40 45 50 \

\ J fetches the outer loop counter

: tbl 3 1 do 12 10 do i j / . loop loop ;
tbl \ 10 11 5 5.5 \

( PRINTING FORTH)

\ EMIT prints a character

: star 42 emit ; star star star \ *** \
: stars 0 do star loop ; 10 stars \ ********** \
: f 5 0 do cr loop ; f \ \n\n\n\n\n \
: box 0 do cr dup stars loop drop ; 3 3 box \ \n***\n***\n*** \
: \stars 0 do cr i spaces 10 stars loop ; 3 \stars

\ TYPE prints a string

s" STRING" type \ STRING
: _ s" STRING" type ; _ \ STRING

( LOCALS FORTH )

: swap' { a b } b a ;
1 2 swap' . . \ 1 2 \

: pattern { a b c } a b c b c b a ;
1 2 3 pattern . . . . . . . \ 1 2 3 2 3 2 1 \

: f { a } 2 { b } a b a ;
1 f . . . \ 1 2 1 \

( UGEN FORTH )

440 0 SinOsc.ar 0.1 * 0 swap Out  -1 add-to-head 1 play-at
440 441 2 mce 0 SinOsc.ar 0.1 * play
440 441 2 mce 0 SinOsc.ar 1 0 SinOsc.kr 0.1 * Abs * play
WhiteNoise.ar 0.1 * play

\ Filters may elide rate

WhiteNoise.ar HPZ1.ar 0.1 * .
WhiteNoise.ar HPZ1 0.1 * .

\ Oscillators may not

440 0 SinOsc 0.1 * . \ ERROR

\ STOP frees all nodes at scsynth (C-cC-k)

stop

( SEE FORTH )

\ SEE prints an HSC3-FORTH representation of the graph.
\ true SEE prints the uid of each non-det UGen, false SEE (C-cC-e) doesn't.
\ SEE is likely not perspicuous.

: g 440 441 2 mce 0 SinOsc.ar 1 0 SinOsc.kr 0.1 * * WhiteNoise.ar 0.1 * + ;
g true see
g false see

( DRAWING FORTH )

\ DRAW draws a unit-generator graph via the graphviz dot language interpreter (C-cC-g)

WhiteNoise.ar 0.1 * dup - draw \ silence \
WhiteNoise.ar 0.1 * 2 clone unmce - draw \ noise \

( NAMING FORTH )

440 Rand 440 + 0 SinOsc.ar 0.1 * draw \ Rand is a unary operator
440 880 Rand.ir 0 SinOsc.ar 0.1 * draw \ Rand.ir is a UGen

440 0 SinOsc.ar 0.1 * Neg draw \ Neg is a unary operator
440 0 SinOsc.ar 0.1 * negate draw \ negate is a stdlib word

( RANDOM FORTH )

: random-sine 1900 2300 Rand.ir 0 SinOsc.ar -1 1 Rand.ir 0.05 0.15 Rand.ir Pan2.ar ;
: _ 4 0 do random-sine play loop ; _
stop

\ Non-det operators are not marked

50 Rand 50 Rand - draw

( UN-RANDOM FORTH )

\ The non-deterministic UGens get identifiers from a counter, which can be set.

1376523 set-uid

\ The UNRAND transformation lifts scalar random UGens to constants.

0 1 Rand.ir 2 clone .s \ <1> [UGEN:Rand UGEN:Rand]
unrand . \ [0.6768026553207348 0.21705544209066452]

\ How un-random is it?  Not very.....  This requires attention.

: rnd Rand.ir unrand ;
: _ 100 0 do 0 1 rnd . loop ; _

\ UNRAND is used internally when a constant value is required, ie. for pause.

: _ 20 0 do 0 1 Rand.ir i . dup dup . unrand . cr pause loop ; _

( ENVELOPED FORTH )

: with-triangle-env { dur lvl } 1 1 0 1 remove-synth dur lvl env-tri EnvGen.kr * ;
WhiteNoise.ar 10 0.1 with-triangle-env play
random-sine 5 0.1 with-triangle-env play
stop

( PAUSING FORTH )

\ PAUSE suspends the thread of the current VM.

: anon 11 1 do i 4 / dup . cr pause random-sine 5 0.1 with-triangle-env play loop ;
anon 5 pause stop

\ PAUSE doesn't re-instate interrupts

10 pause \ non-interruptible
.s

( SCHEDULE FORTH )

\ Since random-sine takes time, there is audible scattering.

: _ 25 0 do random-sine play loop ; _
stop

\ SCHED is a variant of play that forward schedules, allowing for precise alignment.

: _ time 0.1 + { t } 25 0 do random-sine t sched loop ; _
stop

( TEXTURAL FORTH )

random-sine 2 3 5 xfade-texture
random-sine 2 3 6 12 overlap-texture

( INTERRUPTING FORTH )

\ SIGINT is caught and the next VM operation will raise an error.

: endless inf 0 do s" MSG: " type i . cr 0.1 pause loop ;
endless

\ To send SIGINT from Emacs type C-cC-i

( FORK FORTH )

\ The VM can be FORKed, and the fork can be KILLed

: endless inf 0 do s" MSG" type cr 1 pause loop ;
fork endless .s
kill .s

\ The forked word can read from the stack, but it does so in the new thread.

: n-messages 0 do i . cr dup pause loop ;
0.5 10 fork n-messages .s \ <3> 0.5 10 THREAD-ID

\ Here the interval and the count remain on the stack, along with the thread-id.

kill . . .s \ 10 0.5 <0>

\ The VM keeps a list of all running threads, and they can call be killed together (C-cC-s)

killall

( INCLUSIVE FORTH )

s" /home/rohan/sw/hsc3-graphs/gr/why-supercollider.fs" included

\ If the included file is a process we can fork included, with the normal fork stack rules.

s" /home/rohan/sw/hsc3-graphs/gr/alien-meadow.fs" fork included .s
kill . . \ 45 STRING:"/home/rohan/sw/hsc3-graphs/gr/alien-meadow.fs"

( QUOTING FORTH )

' + 1 2 rot execute . \ 3

: sig 1 50 20 remove-synth XLine.kr 0 Impulse.ar 0.01 0.2 Decay2 600 0 FSinOsc.ar 0.25 * * ;
: cmb 0.1 0.1 1 CombN ;
: post-proc { f } 0 2 In.ar f execute 0 swap Out -1 add-to-tail 1 play-at ;

sig play ' cmb post-proc
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

( DYNAMIC FORTH )

\ The UGen words are not all pre-defined but are created dynamically.
\ The dictionary initially contains no UGen words.
\ The dynamic lookup happens if the word is not in the dictionary.
\ This is why eventual lookup failure is reported as DYNAMIC FAILED.

Not_A_UGen.ar
mistyped-word

( TROUBLE FORTH )

440 0 SINOSC.AR \ CASE SENSITIVE

0 0.1 Rand.ir pause
440 0 SinOsc.ar pause \ NON-CONSTANT ERROR

: _ WhiteNoise.ar 0 do i . loop ; _ \ THIS _SHOULD_ BE A NON-CONSTANT ERROR

vmstat \ PRINT VM STATUS
2 trace \ SET TRACE LEVEL PRIORITY, 0=HIGH, 1=MEDIUM, 2=LOW (DEFAULT=-1, NO TRACING)

( FINISHING FORTH )

bye \ C-cC-q

( RAT FORTH )

\ RAT-FORTH uses the same Forth interpreter as HSC3-FORTH.
\ RAT-FORTH knows only rational numbers.

5 2 / . \ 5/2 \
5 2 div . \ 2 \
5 2 mod . \ 1 \
5 2 div-mod . . \ 2 1 \
1/10 .  \ 1/10 \
0.1 . \ 3602879701896397/36028797018963968 \

( ANS FORTH )

\ ANS FORTH is something else altogther.
\ HSC3 FORTH uses ANS FORTH names where it makes sense.
\ ANS FORTH requires floating point literals be written 1.1e0 etc.
\ ANS FORTH has a separate floating point stack, printed using f.

: f. . ;
1.1e0 2.2e0 3.3e0 f. f. f. \ 3.3 2.2 1.1 \
