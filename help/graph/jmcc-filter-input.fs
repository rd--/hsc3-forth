\ filter input (jmcc) #5
: rq 0.01 1 1 0.2 MouseY.kr ;
: cf 100 12000 1 0.2 MouseX.kr ;
0 1 2 mce soundin 0.4 * rq Sqrt * cf rq RLPF
