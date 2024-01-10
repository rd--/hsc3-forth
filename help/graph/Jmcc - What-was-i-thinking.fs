\ What was i thinking? (Jmcc) #2
: z 4 0 sinosc.kr 80 + 0.1 0 0.05 lfpulse.ar 8 0 impulse.ar
    * 500 * 2 decay max 0.157 lfnoise1.kr 0.4 * 0.5 + pulse.ar
    0.04 * 0.2 lfnoise1.kr 2000 * 2400 + 0.2 rlpf ;
: c 0.6 * 0.06 0 0.3 rand.ir lfnoise1.kr 0.025 * 0.035 + 1 combl ;
: y z 0.6 * ;
[ y c mix y c mix ] z +
