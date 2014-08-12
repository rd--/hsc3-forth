( HSC3 FORTH -- AMERICAN PRIMITIVE, VOL. 1 )

\ HSC3-FORTH is a simple FORTH interpreter.

\ There is one data type, the SUPERCOLLIDER UNIT GENERATOR, a data stack, and a return stack.

\ The primitive words are:
\   - : ;
\   - IF ELSE THEN
\   - DO I J LOOP
\   - { } (LOCAL)
\   - ' EXECUTE

( EMACS FORTH )

\ You can write directly to HSC3-FORTH, or you can write to EMACS.

\ HSC3-FORTH MODE
\ Commands are C-cC- prefixed.
\ <point> is the word at the cursor.
\   > -- Start & see the HSC3-FORTH interpreter
\   c -- Send current line
\   d -- Send current region (selection)
\   g -- Send <point> DRAW
\   e -- Send <point> FALSE SEE
\   a -- Send <point> PLAY
\   u -- Send <point> ?
\   k -- Send STOP
\   s -- Send KILLALL
\   q -- Send BYE
\   i -- Send !SIGINT!
\   p -- Send SC3-STATUS

( HELP FORTH )

\ ? prints information about a UGEN, in EMACS type C-cC-u.

S" SINOSC" ?

\ SinOsc [KR,AR] freq=440.0 phase=0.0

S" ENVGEN" ?

\ EnvGen [KR,AR] gate=1 levelScale=1 levelBias=0 timeScale=1 doneAction=0 *envelope=0
\     MCE, REORDERS INPUTS: [5,0,1,2,3,4,5], ENUMERATION INPUTS: 4=DoneAction, 5=Envelope UGen

( NUM FORTH )

\ SC3 UGENS are numerical.
\ UGENS may be constant, math operations at constants render constants.

2 2 + . \ 4 \
2 1 - . \ 1 \
3 4 + 5 * . \ 35 \
3 4 5 * + . \ 23 \
2 NEGATE . \ -2 \
-1 ABS . \ 1 \

( FRACTIONAL FORTH )

\ UGENS are real valued, / is real valued division, and % real valued modulo.

1.1 2.2 3.3 . . . \ 3.3 2.2 1.1 \
5 2 / . \ 2.5 \
7.5 3.75 % . \ 0 \

( INTEGRAL FORTH )

\ The printer prints integer constants without a fractional part.
\ There is an integer division UGEN.

10 2 DIV . \ 5 \
5 2 DIV . \ 2 \
7 3 % . \ 1 \

( EQ FORTH )

\ SC3 treats signals less than or equal to zero as FALSE and greater than zero as TRUE.
\ HSC3-FORTH adopts 1 as TRUE.

0 1 = . \ FALSE \
1 1 = . \ TRUE \

( ORD FORTH )

\ The comparison operators.

1 2 < . \ TRUE \
2 1 < . \ FALSE \
1 1 < . \ FALSE \
1 1 <= . \ TRUE \
2 3 MIN . \ 2 \
3 2 MIN . \ 2 \
1 3 MAX . \ 3 \

( STACK FORTH )

1 2 DROP . \ 1 \
1 2 .S OVER .S DROP DROP DROP .S \ <2> 1 2 <3> 1 2 1 <0> \
1 2 .S SWAP .S DROP DROP .S \ <2> 1 2 <2> 2 1 <0> \
1 2 3 .S ROT .S DROP DROP DROP .S \ <3> 1 2 3 <3> 2 3 1 <0> \
1 2 .S NIP .S DROP .S \ <2> 1 2 <1> 2 <0> \
1 2 .S TUCK .S DROP DROP DROP .S \ <2> 1 2 <3> 2 1 2 <0> \
1 2 2DUP .S . . . . .S \ <4> 1 2 1 2 2 1 2 1 <0> \
1 2 3 4 5 2 PICK .S . . . . . . .S \ <6> 1 2 3 4 5 3 3 5 4 3 2 1 <0> \

( BLOCK FORTH )

: SQUARED DUP * ;
5 SQUARED . \ 25 \
7 SQUARED . \ 49 \

: CUBED DUP SQUARED * ;
-5 CUBED . \ -125 \

: FOURTH-POWER SQUARED SQUARED ;
3 FOURTH-POWER . \ 81 \

( CONDITIONAL FORTH )

: _ FALSE IF S" TRUE" TYPE ELSE S" FALSE" TYPE THEN ; _ \ FALSE \

( DO FORTH )

: FIVE 5 0 DO 5 LOOP ;
FIVE .S . . . . . \ <5> 5 5 5 5 5 5 5 5 5 5 \

: N-DUP 0 DO DUP LOOP ;
: N-DROP 0 DO DROP LOOP ;
5 4 N-DUP .S \ <5> 5 5 5 5 5 \
5 N-DROP .S \ <0> \

\ I fetches the loop counter

: SEVEN-ELEVEN 11 7 DO I . LOOP ;
SEVEN-ELEVEN \ 7 8 9 10 \

: MTABLE 11 1 DO DUP I * . LOOP DROP ;
5 MTABLE \ 5 10 15 20 25 30 35 40 45 50 \

\ J fetches the outer loop counter

: TBL 3 1 DO 12 10 DO I J / . LOOP LOOP ;
TBL \ 10 11 5 5.5 \

( PRINTING FORTH )

\ EMIT prints a character

: STAR 42 EMIT ; STAR STAR STAR \ *** \
: STARS 0 DO STAR LOOP ; 10 STARS \ ********** \
: F 5 0 DO CR LOOP ; F \ \N\N\N\N\N \
: BOX 0 DO CR DUP STARS LOOP DROP ; 3 3 BOX \ \N***\N***\N*** \
: \STARS 0 DO CR I SPACES 10 STARS LOOP ; 3 \STARS

\ TYPE prints a string

S" STRING" TYPE \ STRING
: _ S" STRING" TYPE ; _ \ STRING

( LOCAL FORTH )

: SWAP' { A B } B A ;
1 2 SWAP' . . \ 1 2 \

: PATTERN { A B C } A B C B C B A ;
1 2 3 PATTERN . . . . . . . \ 1 2 3 2 3 2 1 \

: F { A } 2 { B } A B A ;
1 F . . . \ 1 2 1 \

( UGEN FORTH )

440 0 SINOSC.AR 0.1 * 0 SWAP OUT  -1 ADD-TO-HEAD 1 PLAY-AT
440 441 2 MCE 0 SINOSC.AR 0.1 * PLAY
440 441 2 MCE 0 SINOSC.AR 1 0 SINOSC.KR 0.1 * ABS * PLAY
WHITENOISE.AR 0.1 * PLAY

\ Filters may elide the operating rate.

WHITENOISE.AR HPZ1.AR 0.1 * .
WHITENOISE.AR HPZ1 0.1 * .

\ Oscillators may not.

440 0 SINOSC 0.1 * . \ ERROR

\ Operators have both text and symbolic names.

440 0 SINOSC.AR WHITENOISE.AR ADD -45 DBAMP MUL PLAY \ QUIETLY NOW

\ STOP frees all nodes at SCSYNTH (C-cC-k)

STOP

( INSENSITIVE FORTH )

440 0 SINOSC.AR 0.1 MUL PLAY \ CASE INSENSITIVE
441 0 sinosc.ar 0.1 mul play \ case insensitive
442 0 SinOsc.ar 0.1 Mul Play \ Case Insensitive

( SEE FORTH )

\ SEE prints an HSC3-FORTH representation of the graph.
\ TRUE SEE prints the UID of each NON-DET UGEN, FALSE SEE (C-cC-e) doesn't.
\ SEE is likely not perspicuous.

: G 440 441 2 MCE 0 SINOSC.AR 1 0 SINOSC.KR 0.1 * * WHITENOISE.AR 0.1 * + ;
G TRUE SEE
G FALSE SEE

( DRAWING FORTH )

\ DRAW draws a UGEN graph via the graphviz DOT language interpreter (C-cC-g)

WHITENOISE.AR 0.1 * DUP - DRAW \ SILENCE \
WHITENOISE.AR 0.1 * 2 CLONE UNMCE - DRAW \ NOISE \

( NAMING FORTH )

440 RAND_ 440 + 0 SINOSC.AR 0.1 * DRAW \ RAND_ IS A UNARY OPERATOR
440 880 RAND.IR 0 SINOSC.AR 0.1 * DRAW \ RAND.IR IS A UGEN

( RANDOM FORTH )

: RANDOM-SINE 1900 2300 RAND.IR 0 SINOSC.AR -1 1 RAND.IR 0.05 0.1 RAND.IR PAN2.AR ;
: _ 4 0 DO RANDOM-SINE PLAY LOOP ; _
STOP

\ NON-DET operators are not marked

500 RAND_ 500 RAND_ - 0 SINOSC.AR 0.1 * DRAW

( UN-RANDOM FORTH )

\ The non-deterministic UGENS get identifiers from a counter, which can be set.

1376523 SET-UID

\ The UNRAND transformation lifts scalar random UGENS to constants.

0 1 RAND.IR 2 CLONE .S \ <1> [UGEN:RAND UGEN:RAND]
UNRAND . \ [0.6768026553207348 0.21705544209066452]

\ How un-random is it?  Not very.....  This requires attention.

: RND RAND.IR UNRAND ;
: _ 100 0 DO 0 1 RND . LOOP ; _

\ UNRAND is applied internally when a constant value is required, ie. for PAUSE.

: _ 20 0 DO 0 1 RAND.IR I . DUP DUP . UNRAND . CR PAUSE LOOP ; _

( ENVELOPED FORTH )

: WITH-TRIANGLE-ENV { DUR LVL } 1 1 0 1 REMOVE-SYNTH DUR LVL ENV-TRI ENVGEN.KR * ;
WHITENOISE.AR 10 0.1 WITH-TRIANGLE-ENV PLAY
RANDOM-SINE 5 0.1 WITH-TRIANGLE-ENV PLAY
STOP

( PAUSING FORTH )

\ PAUSE suspends the thread of the current VM.

: ANON 11 1 DO I 4 / DUP . CR PAUSE RANDOM-SINE 5 0.1 WITH-TRIANGLE-ENV PLAY LOOP ;
ANON 5 PAUSE STOP

\ PAUSE doesn't re-instate interrupts

10 PAUSE \ NON-INTERRUPTIBLE
.S

( SCHEDULE FORTH )

\ Since RANDOM-SINE takes time, there is audible scattering.

: _ 25 0 DO RANDOM-SINE PLAY LOOP ; _
STOP

\ SCHED is a variant of PLAY that forward schedules, allowing for precise alignment.

: _ TIME 0.1 + { T } 25 0 DO RANDOM-SINE T SCHED LOOP ; _
STOP

( TEXTURAL FORTH )

\ The SC2 TEXTURE functions are implemented, see OVERLAP-TEXTURE.FS.

RANDOM-SINE 2 3 5 XFADE-TEXTURE
RANDOM-SINE 2 3 6 12 OVERLAP-TEXTURE

( INTERRUPTING FORTH )

\ SIGINT is caught and the next VM operation will raise an error.

: ENDLESS INF 0 DO S" MSG: " TYPE I . CR 0.1 PAUSE LOOP ;
ENDLESS

\ To send SIGINT from EMACS type C-cC-i

( FORK FORTH )

\ The VM can be FORKed, and the fork can be KILLed

: ENDLESS INF 0 DO S" MSG" TYPE CR 1 PAUSE LOOP ;
FORK ENDLESS .S
KILL .S

\ The forked word can read from the stack, but it does so in the new thread.

: N-MESSAGES 0 DO I . CR DUP PAUSE LOOP ;
0.5 10 FORK N-MESSAGES .S \ <3> 0.5 10 THREAD-ID

\ Here the interval and the count remain on the stack, along with the THREAD-ID.

KILL . . .S \ 10 0.5 <0>

\ The VM keeps a list of all running threads, and they can call be killed together (C-cC-s)

KILLALL

( INCLUSIVE FORTH )

s" /home/rohan/sw/hsc3-graphs/gr/why-supercollider.fs" INCLUDED

\ If the file is a process we can FORK INCLUDED, with the normal FORK stack rules.

s" /home/rohan/sw/hsc3-graphs/gr/alien-meadow.fs" FORK INCLUDED .S
KILL . . \ 45 STRING:"/home/rohan/sw/hsc3-graphs/gr/alien-meadow.fs"

( QUOTING FORTH )

\ ' puts the EXECUTION TOKEN (XT) of the subsequent word onto the stack.
\ EXECUTE takes the token and applies it.

' + . \ XT:+ \
' + 1 2 ROT EXECUTE . \ 3 \

: SIG 1 50 20 REMOVE-SYNTH XLINE.KR 0 IMPULSE.AR 0.01 0.2 DECAY2 600 0 FSINOSC.AR 0.25 * * ;
: CMB 0.1 0.1 1 COMBN ;
: POST-PROC { F } 0 2 IN.AR F EXECUTE 0 SWAP OUT -1 ADD-TO-TAIL 1 PLAY-AT ;

SIG PLAY ' CMB POST-PROC
STOP

( RETURN FORTH )

1 >r .s r> . \ <0> 1
>r \ ERROR

( LABELED FORTH )

s" LABEL" LABEL . \ "LABEL"

( FIBONACCI FORTH )

: FIB 0 1 ROT 0 DO OVER + SWAP LOOP DROP ;
: FIBS 0 DO I FIB . LOOP ;
50 FIBS \ 0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 ...
.S

( DYNAMIC FORTH )

\ The UGEN words are not all pre-defined but are created dynamically.
\ The dictionary initially contains no UGEN words.
\ The dynamic lookup happens if the word is not in the dictionary.
\ This is why eventual lookup failure is reported as DYNAMIC FAILED.

NOT_A_UGEN.AR \ ERROR
MISTYPED-WORD \ ERROR

( TROUBLE FORTH )

0 1 RAND.IR DUP . PAUSE \ RAND
440 0 SINOSC.AR PAUSE \ ERROR (NON-CONSTANT)

: _ WHITENOISE.AR 0 DO I . LOOP ; _ \ THIS _SHOULD_ BE A NON-CONSTANT ERROR

VMSTAT \ PRINT VM STATUS
2 TRACE \ SET TRACE LEVEL PRIORITY, 0=HIGH, 1=MEDIUM, 2=LOW (DEFAULT=-1, NO TRACING)

( FINISHING FORTH )

BYE \ C-cC-q

( RAT FORTH )

\ RAT-FORTH uses the same Forth interpreter as HSC3-FORTH.
\ RAT-FORTH knows only rational numbers.

5 2 / . \ 5/2 \
5 2 DIV . \ 2 \
5 2 MOD . \ 1 \
5 2 DIV-MOD . . \ 2 1 \
1/10 .  \ 1/10 \
0.1 . \ 3602879701896397/36028797018963968 \

( ANS FORTH )

\ ANS FORTH is something else altogther.
\ HSC3 FORTH uses ANS FORTH names where it makes sense.
\ ANS FORTH requires floating point literals be written 1.1e0 etc.
\ ANS FORTH has a separate floating point stack, printed using f.

: F. . ;
1.1E0 2.2E0 3.3E0 F. F. F. \ 3.3 2.2 1.1 \
