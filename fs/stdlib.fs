: nip (p q -- q ) swap drop ;
: tuck ( p q -- q p q ) swap over ;

: 1+ 1 + ;
: 1- 1 - ;
: 0= 0 = ;
: negate 0 swap - ;

: true -1 ;
: false 0 ;
: not 0= ;

: abs dup 0 < if negate then ;
: min 2dup < if drop else nip then ;

: bl 32 ;
: space bl emit ;
: spaces 0 do space loop ;
: cr 10 emit ;
