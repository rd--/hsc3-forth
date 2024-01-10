\ saucer base (Jmcc) #6 texture=overlap,2,6,4,inf
: r1 0 swap Rand.ir ;
: rloc -1 1 Rand.ir 1 Pan2 ;
: osc SinOsc.ar ;
: drms dup rot * swap 1.1 * + ;
: sb 20 r1 0 osc 1000 r1 drms 0 osc 5000 r1 drms 0 osc 0.1 * rloc ;
sb
