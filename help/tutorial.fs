( hsc3 forth -- or American Primitive, Vol. 1 )

\ hsc3-forth is a simple forth interpreter.

\ There is one data type, the Supercollider Unit Generator, a data stack, and a return stack.

\ The primitive words are:
\   : ;
\   If Else Then
\   Do I J Loop
\   { } (Local)
\   ' Execute

( Emacs Forth )

\ hsc3-forth is an interpreter, you can use it directly or you can use emacs using hsc3-forth-mode.

\ Commands are listed below and are C-cC- prefixed.  <point> is the word at the cursor.

\   > -- Start hsc3-forth if not running, show interpreter frame
\   c -- Send current line
\   d -- Send current region
\   g -- Send <region> Draw
\   e -- Send Pp
\   a -- Send <region> Play
\   u -- Send <point> ?
\   j -- Send <point> Manual
\   k -- Send Stop
\   s -- Send KillAll
\   q -- Send Bye
\   i -- Send !SigInt!
\   p -- Send Sc3-Status

( Help Forth )

\ Many words have more or less the same meaning as in ANS.
\ In these cases DPANS'94 numbers are given for reference.
\ These can be resolved as http://forth.sf.net/std/dpans/dpans6.htm#6.1.2165

\ ? prints information about a Ugen, in Emacs type C-cC-u.

S" sinosc" ? \ S" = 6.1.2165 \

\ SinOsc kr|ar freq=440 phase=0

S" EnvGen" ?

\ EnvGen kr|ar gate=1 levelScale=1 levelBias=0 timeScale=1 doneAction=0 *envelope=0
\     Mce, Reorders Inputs: [5,0,1,2,3,4,5], Enumeration Inputs: 4=DoneAction, 5=Envelope UGen

\ Manual opens the SuperCollider manual at the required page, in emacs type C-cC-j.

\ Dpans opens the DPANS glossary at the indicated entry, in emacs type C-cC-f

( Discarding Forth )

\ There are two comment forms, ( comments until ) and \ comments until \n.

\ ( = 6.1.0080
\ \ = 6.2.2535

( Number Forth )

\ SC3 UGens are numerical.
\ UGens may be constant, and math operations at constants render constants.

2 2 + . \ 4 \ + = 6.1.0120 \
2 1 - . \ 1 \ - = 6.1.0160 \
3 4 + 5 * . \ 35 \ * = 6.1.0090 \
3 4 5 * + . \ 23 \
2 negate . \ -2 \ Negate = 6.1.1910 \
-1 abs . \ 1 \ Abs = 6.1.0690 \

( Fractional Forth )

\ Ugens are real valued, / is real valued division, and % real valued modulo.

1.1 2.2 3.3 . . . \ 3.3 2.2 1.1 \ . = 6.1.0180 \
5 2 / . \ 2.5 \ F/ = 12.6.1.1430 \
7.5 3.75 % . \ 0 \

( Integral Forth )

\ The printer prints integer constants without a fractional part.
\ There is an integer division UGen.

10 2 div . \ 5 \ / = 6.1.0230 \
5 2 div . \ 2 \
7 3 % . \ 1 \

( Eq Forth )

\ SC3 treats signals less than or equal to zero as False and greater than zero as True.
\ hsc3-forth adopts 1 as True.

0 1 = . \ False \ = = 6.1.0530 False = 6.2.1485 \
1 1 = . \ True \ True = 6.2.2298 \

( Ord Forth )

\ The comparison operators.

1 2 < . \ True \ < = 6.1.0480 \
2 1 < . \ False \
1 1 < . \ False \
1 1 <= . \ True \
2 3 min . \ 2 \ Min = 6.1.1880 \
3 2 min . \ 2 \
1 3 max . \ 3 \ Max = 6.1.1870 \ non-optimising?

( Stack Forth )

1 2 drop . \ 1 \ Drop = 6.1.1260 \
1 2 .s over .s drop drop drop .s \ <2> 1 2 <3> 1 2 1 <0> \ Over = 6.1.1990 .s = 15.6.1.0220 \
1 2 .s swap .s drop drop .s \ <2> 1 2 <2> 2 1 <0> \ Swap = 6.1.2260 \
1 2 3 .s rot .s drop drop drop .s \ <3> 1 2 3 <3> 2 3 1 <0> \ Rot = 6.1.2160 \
1 2 .s nip .s drop .s \ <2> 1 2 <1> 2 <0> \ Nip = 6.2.1930 \
1 2 .s tuck .s drop drop drop .s \ <2> 1 2 <3> 2 1 2 <0> \ Tuck = 6.2.2300 \
1 2 2dup .s . . . . .s \ <4> 1 2 1 2 2 1 2 1 <0> \ 2dup = 6.1.0380 \
1 2 3 4 5 2 pick .s . . . . . . .s \ <6> 1 2 3 4 5 3 3 5 4 3 2 1 <0> \ Pick = 6.2.2030 \

( Block Forth )

\ Colon and Semicolon introduce new words.

: squared dup * ; \ : = 6.1.0450, ; = 6.1.0460 \
5 squared . \ 25 \
7 squared . \ 49 \

: cubed dup squared * ;
-5 cubed . \ -125 \

: fourth-power squared squared ;
3 fourth-power . \ 81 \

( Conditional Forth )

: _ 0 if s" #t" type else s" #f" type then ; \ If = 6.1.1700, Else = 6.1.1310, Then = 6.1.2270 \
_ \ #f \

 ( Do Forth )

: five 5 0 do 5 loop ; \ Do = 6.1.1240, Loop = 6.1.1800 \
five .S . . . . . \ <5> 5 5 5 5 5 5 5 5 5 5 \

: n-dup 0 do dup loop ;
: n-drop 0 do drop loop ;
5 4 n-dup .s \ <5> 5 5 5 5 5 \
5 n-drop .s \ <0> \

\ I fetches the loop counter

: seven-eleven 11 7 do i . loop ; \ I = 6.1.1680 \
seven-eleven \ 7 8 9 10 \

: mtable 11 1 do dup i * . loop drop ;
5 mtable \ 5 10 15 20 25 30 35 40 45 50 \

\ J fetches the outer loop counter

: tbl 3 1 do 12 10 do i j / . loop loop ; \ J = 6.1.1730 \
tbl \ 10 11 5 5.5 \ 10/1 11/1 10/2 11/2

( Printing Forth )

\ emit prints a character.

: star 42 emit ; star star star \ *** \ Emit = 6.1.1320 \
: stars 0 do star loop ; 10 stars \ ********** \
: f 5 0 do cr loop ; f \ \n\n\n\n\n \
: box 0 do cr dup stars loop drop ; 3 3 box \ \n***\n***\n*** \
: \stars 0 do cr i spaces 10 stars loop ; 3 \stars

\ Type prints a string

s" string" type \ string \ Type = 6.1.2310 \
: _ s" string" type ; _ \ string

( Local Forth )

\ hsc3-forth allows Local words using the { Name ... } syntax.

: swap' { a b } b a ; \ (Local) = 13.6.1.0086 \
1 2 swap' . . \ 1 2 \

: pattern { a b c } a b c b c b a ;
1 2 3 pattern . . . . . . . \ 1 2 3 2 3 2 1 \

\ Multiple sets of LOCAL words are allowed.

: f { a } 2 { b } a b a ;
1 f . . . \ 1 2 1 \

( Ugen Forth )

440 0 sinosc.ar 0.1 * 0 swap out  -1 addToHead 1 playAt
440 441 2 mce 0 sinosc.ar 0.1 * play
440 441 2 mce 0 sinosc.ar 1 0 sinosc.kr 0.1 * abs * play
pinknoise.ar 0.05 * play

\ Stop frees all nodes at SCSYNTH (C-cC-k)

stop

\ Filters may elide the operating rate.

whitenoise.ar hpz1.ar 0.1 * play
whitenoise.ar hpz1 0.1 * play

\ Oscillators may also elide the operating rate (defaults to highest allowed rate)

440 0 sinosc 0.1 * play

\ Operators have both text and symbolic names.

440 0 sinosc.ar whitenoise.ar add -45 dbamp mul play \ quietly now

( Insensitive Forth )

441 0 sinosc.ar 0.1 mul play \ case insensitive
442 0 SinOsc.ar 0.1 Mul Play \ Case Insensitive

( Pretty Forth )

\ pretty-print prints an hsc3-forth representation of the graph.
\ A flag tells whether to print the UId of each non-det Ugen.
\ Pp is an abreviation of false pretty-print (C-cC-e).

: g 440 0 sinosc.ar 1 0 sinosc.kr 0.1 * * whitenoise.ar 0.1 * + ;
g true prettyPrint
g pp

\ PP is only moderately perspicuous, it does not see through MCE.

: g 440 441 2 mce 0 sinosc.ar 1 0 sinosc.kr 0.1 * * whitenoise.ar 0.1 * + ;
g pp

( Drawing Forth )

\ Draw draws a UGEN graph via the graphviz DOT language interpreter (C-cC-g)

whitenoise.ar 0.1 * dup - draw \ Silence \
whitenoise.ar 0.1 * 2 clone unmce - draw \ noise \

( Naming Forth )

440 rand_ 440 + 0 sinosc.ar 0.1 * draw \ rand_ is a unary operator
440 880 rand.ir 0 sinosc.ar 0.1 * draw \ rand.ir is a ugen

( Random Forth )

: random-sine 1900 2300 rand.ir 0 sinosc.ar -1 1 rand.ir 0.05 0.1 rand.ir pan2.ar ;
: _ 4 0 do random-sine play loop ; _

stop

\ NON-DET operators are not marked, the below has one Rand_ operator.

500 rand_ 500 rand_ - 0 sinosc.ar 0.1 * draw

( Un-Random Forth )

\ The non-deterministic ugens get identifiers from a counter, which can be set.

1376523 set-uid

\ The Unrand transformation lifts scalar random Ugens to constants.

0 1 rand.ir 2 clone .s \ <1> [Rand Rand]
unrand . \ [0.6768026553207348 0.21705544209066452]

\ How un-random is it?  Not very.....  This requires attention.

: rnd rand.ir unrand ;
: _ 100 0 do 0 1 rnd . loop ; _

\ Unrand is applied internally when a constant value is required, ie. for Pause.

: _ 20 0 do 0 1 rand.ir i . dup dup . unrand . cr pause loop ; _

\ Choose is a composite i-rate Ugen.

: rharm 13 1 do i 100 * loop 12 mce choose 0 sinosc.ar -1 1 rand.ir 0.05 pan2 ;
rharm 1 3 9 inf overlapTexture \ C-cC-i to interrupt

( Enveloped Forth )

: with-triangle-env { dur lvl } 1 1 0 1 removeSynth dur lvl envTri envgen.kr * ;
whitenoise.ar 10 0.1 with-triangle-env play
random-sine 5 0.1 with-triangle-env play
stop

( Interrupting Forth )

\ Sigint is caught and the next Vm operation will raise an error.

: endless inf 0 do s" msg: " type i . cr 0.1 pause loop ;
endless

\ To send Sigint from Emacs type C-cC-i

( Pausing Forth )

\ Pause suspends the thread of the current Vm.

: anon 11 1 do i 4 / dup . cr pause random-sine 5 0.1 with-triangle-env play loop ;
anon 5 pause stop

\ Pause doesn't re-instate interrupts

10 pause \ non-interruptible
.s

( Schedule Forth )

\ Since random-sine takes time, there is audible scattering.

: _ 25 0 do random-sine play loop ; _
stop

\ Sched is a variant of Play that forward schedules, allowing for precise alignment.

: _ time 0.1 + { t } 25 0 do random-sine t sched loop ; _
stop

( Textural Forth )

\ The SC2 Texture functions are implemented, see overlap-texture.fs.

random-sine 2 3 5 xfadeTexture
random-sine 2 3 6 12 overlapTexture

( Fork Forth )

\ The Vm can be Forked, and the fork can be Killed

: endless inf 0 do s" msg" type cr 1 pause loop ;
fork endless .s
kill .s

\ The forked word can read from the stack, but it does so in the new thread.

: n-messages 0 do i . cr dup pause loop ;
0.5 10 fork n-messages .s \ <3> 0.5 10 thread-id

\ Here the interval and the count remain on the stack, along with the THREAD-ID.

kill . . .s \ 10 0.5 <0>

\ The VM keeps a list of all running threads, and they can call be killed together (C-cC-s)

killall

( Inclusive Forth )

s" /home/rohan/sw/hsc3-forth/help/graph/jmcc-why-supercollider.fs" included play
stop

\ If the file is a process we can Fork Included, with the normal Fork stack rules.

s" /home/rohan/sw/hsc3-forth/help/texture/jmcc-alien-meadow.fs" fork included .s
kill . . \ 45 String:"/home/rohan/sw/hsc3-forth/help/texture/jmcc-alien-meadow.fs"

( Quoting Forth )

\ ' puts the Execution Token (XT) of the subsequent word onto the stack.
\ Execute takes the token and applies it.

' + . \ XT:+ \ ' = 6.1.0070 \
' + 1 2 rot execute . \ 3 \

\ Words can be somewhat higher order.

: sig 1 50 20 removeSynth xline.kr 0 impulse.ar 0.01 0.2 decay2 600 0 fsinosc.ar 0.25 * * ;
: cmb 0.1 0.1 1 combn ;
: post-proc { f } 0 2 in.ar f execute 0 swap out -1 addToTail 1 playAt ;

sig play ' cmb post-proc
stop

( Return Forth )

1 >r .s r> . \ <0> 1
>r \ ERROR

( Labeled Forth )

s" label" label . \ "label"

( Fibonacci Forth )

: fib 0 1 rot 0 do over + swap loop drop ;
: fibs 0 do i fib . loop ;
50 fibs \ 0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 ...
.s

( Dynamic Forth )

\ The UGen words are not all pre-defined but are created dynamically.
\ The dictionary initially contains no UGen words.
\ The dynamic lookup happens if the word is not in the dictionary.
\ This is why eventual lookup failure is reported as Dynamic Failed.

not_a_ugen.ar \ Error
mistyped-word \ Error

( Trouble Forth )

0 1 rand.ir dup . pause \ rand
440 0 sinosc.ar pause \ Error (non-constant)

: _ whitenoise.ar 0 do i . loop ; _ \ this _should_ be a non-constant error

0 1 2 3 4 4 mce out.kr pp \ This _should_ add the mce constructor to the mce input

vmstat \ Print Vm status
2 trace \ Set trace level priority, 0=high, 1=medium, 2=low (default=-1, no tracing)

( Finishing Forth )

bye \ C-cC-q

( Collecting Forth )

5 10 2 series . \ [10,12,14,16,18] \
5 3 6 geom . \ [3,18,108,648,3888] \

( Rat Forth )

\ rat-forth uses the same Forth interpreter as hsc3-forth.
\ rat-forth knows only rational numbers.

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
