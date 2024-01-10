\ Reverberated noise bursts (Jmcc) #3
: s 0.6 dust.ar 0.2 * 0.15 decay pinknoise.ar * 0.6 dust.ar 0.2 * 0.15 decay pinknoise.ar * ;
: z 0.048 0.048 delayn ;
: y 0.1 0 0.1 rand.ir 6 clone lfnoise1.kr 0.04 * 0.05 + 15 combl mix ;
: f 4 0 do 0.05 0 0.05 rand.ir 2 clone 1 allpassn loop ;
s y f +
