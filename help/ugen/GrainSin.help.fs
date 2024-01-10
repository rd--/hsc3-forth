\ GrainSin
: k 8 ;
: overlap 0 2 0 0.2 MouseY ;
: f 1 220 0 0.2 MouseX ;
: tr f 0 Impulse ;
: a tr overlap f / 440 -1 1 tr TRand -1 512 k GrainSin Splay2 ;
a 0.995 LeakDc 0.1 *
