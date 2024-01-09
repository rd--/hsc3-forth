\ https://sccode.org/1-4Qy ; f0 ; 0320
: b [ 2 4 9 3 ] 9 * ;
: o 1 b / 0 0.5 VarSaw 0 > 3 + b * 0 b 2000 / 0 0.5 VarSaw 1 + 2 / 1 Lag VarSaw ;
o 1.1 8 b / 0 0.5 VarSaw 1 % 1 RoundTo 0.1 + 8 CombC Splay2 3 /
