\ babbling brook (jmcc) #SC3
: b { f m a g } BrownNoise.ar 0.99 OnePole BrownNoise.ar f LPF m * a + 0.03 RHPF g * ;
14 400 500 0.006 b 2 clone 20 800 1000 0.01 b 2 clone +
