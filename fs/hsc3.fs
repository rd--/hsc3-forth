\ OPERATOR ALIASES

: = == ;
: div IDiv ;
: not 0= ; \ NOT is not in ANS FORTH, see DPAN for discussion

\ : abs Abs ; \ stdlib REDEFINE
\ : negate Neg ; \ stdlib REDEFINE

\ CONSTANTS

: inf 9e8 ; \ Pseudo infinity
: pi 3.141592653589793 ;

\ ABBREVIATIONS

: mce2 2 mce ;
: mrg2 2 mrg ;
: clone2 2 clone ;
: play -1 add-to-head 1 play-at ;

\ ENVELOPE CURVE ENUMERATION

: env-step 0 ;
: env-lin 1 ;
: env-exp 2 ;
: env-sin 3 ;
: env-welch 4 ;
: env-num 5 ;
: env-sqr 6 ;
: env-cub 7 ;
: env-hold 8 ;

\ DONE ACTION ENUMERATION

: do-nothing 0 ;
: pause-synth 1 ;
: remove-synth 2 ;
: remove-group 14 ;

\ ADD ACTION ENUMERATION

: add-to-head 0 ;
: add-to-tail 1 ;
: add-before 2 ;
: add-after 3 ;
: add-replace 4 ;

\ GEN FLAG ENUMERATION

: gen-normalize 1 ;
: gen-wavetable 2 ;
: gen-clear 4 ;

\ LOOP ENUMERATION

: no-loop 0 ;
: with-loop 1 ;

\ WARP ENUMERATION

: linear 0 ;
: exponential 1 ;

\ ENVELOPE NODES

: no-reset-node -99 ;
: no-loop-node -99 ;

\ ENVELOPE CONSTRUCTORS

: env-tri { dur lvl } dur 2 / { dur' }
  0 2 no-reset-node no-loop-node lvl dur' env-lin 0 0 dur' env-lin 0 12 mce ;
