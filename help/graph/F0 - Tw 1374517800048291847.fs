\ F0 - Tw 1374517800048291847
: b 1 9 upTo ;
: f0 0.003 b 9 / 0.5 VarSaw 1 3 / RoundTo 1.9 Lag 91 * 252 + ;
: w0 b 9 / 99 / 0 0.5 VarSaw 1 % ;
: f1 63 b * 0.49 b 9 / 0.5 VarSaw + ;
: f2 b 9 / 9 / 0 b 10 / VarSaw 99 / f0 + b * ;
: w1 f2 0 b 9 / VarSaw 1 + 2 / ;
: m1 6 b 9 / 3 / 0 VarSaw 0.2 b 9 / 0.5 VarSaw 9 / 0.009 0 0.5 VarSaw 0.3 * 0.4 + + Max ;
: o2 f1 0 w1 VarSaw m1 * 60 0 0.5 VarSaw 3 0 0 VarSaw 0 Max * 3 ^ + ;
f0 b 9 / w0 VarSaw 3 / o2 + 5 / Splay2
