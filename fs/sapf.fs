: xmousex 1 0.2 MouseX.kr ;
: xmousey 1 0.2 MouseY.kr ;
: 1/ Recip ;
: ohz 12 * 60 + midicps ;
: nnhz midicps ;

: reduce { w } items 1 do w execute loop ;
: +/ items 1 do + loop ;
: */ items 1 do * loop ;

: lfo { p f l u } p f sinosc.kr -1 1 l u linlin ;
