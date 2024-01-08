\ Operator Aliases

: = == ;
: div IDiv ;
: not 0= ; \ Not is not in Ans Forth, see Dpan for discussion

: negate Neg ; \ stdlib redefine

\ Constants

: inf 9e8 ; \ Pseudo infinity
: epsilon 1e-8 ; \ Psuedo epsilon (instead of zero in Exp contexts)
: pi 3.141592653589793 ;

\ Math

: exp2 2 swap ** ;

\ Abbreviations & Aliases

: <! 2 mrg ;
: play 0 swap Out -1 addToHead 1 playAt ;
: pp false prettyPrint ;

\ Envelope Curve Enumeration

: envStep 0 ;
: envLin 1 ;
: envExp 2 ;
: envSin 3 ;
: envWelch 4 ;
: envNum 5 ;
: envSqr 6 ;
: envCub 7 ;
: envHold 8 ;

\ Done Action Enumeration

: doNothing 0 ;
: pauseSynth 1 ;
: removeSynth 2 ;
: removeGroup 14 ;

\ Add Action Enumeration

: addToHead 0 ;
: addToTail 1 ;
: addBefore 2 ;
: addAfter 3 ;
: addReplace 4 ;

\ Gen Flag Enumeration

: genNormalize 1 ;
: genWavetable 2 ;
: genClear 4 ;

\ Loop Enumeration

: noLoop 0 ;
: withLoop 1 ;

\ Warp Enumeration

: linear 0 ;
: exponential 1 ;

\ Envelope Nodes

: noResetNode -99 ;
: noLoopNode -99 ;

\ Envelope Constructors

: envPerc { atk rel } [ 0 2 -99 -99 1 atk 5 -4 0 rel 5 -4 ] ;
: envLinen { atk sus rel lev } [ 0 3 -99 -99 lev atk 1 0 lev sus 1 0 0 rel 1 0 ] ;
: envTri { dur lvl } dur 2 / { dur' } [ 0 2 -99 -99 lvl dur' envLin 0 0 dur' envLin 0 ] ;
: envAsr { atk lvl rel c } [ 0 2 1 -99 lvl atk c 0 0 rel c 0 ] ;

\ Composite Ugens

: Choose { u } 0 u chan 1 - IRand.ir u Select ;
: LinLinMulAdd { sl sr dl dr } dr dl - sr sl - / { m } m dl m sl * - ;
: LinLin LinLinMulAdd MulAdd ;
: Rand2.ir { n } n Neg n Rand.ir ;
: SoundIn { u } NumOutputBuses.ir u + 1 In.ar ;
: TChoose { t a } 0 a chan t TIRand a Select ;

\ Local Buffers

: asLocalBuf { arr }
    arr chan { len }
    1 len LocalBuf.ir { buf }
    buf 0 len arr SetBuf.ir { set }
    buf set 2 mrg ;

\ Commands

: b_allocRead s" ,isii" s" /b_allocRead" async ;

\ Collection

: series { n z k } [ n 0 do k i * z + loop ] ;
: geom { n z k } [ do k i ** z * loop ] ;
