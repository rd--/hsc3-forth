\ https://sccode.org/1-4Qy ; f0 ; 0317
: b [ 0 3 7 5 2 9 10 ] 0.2 + ;
: c 0.5 b / 0 0.5 VarSaw ;
: m 2.01 b / 0 0.5 VarSaw 8 * c 0 > 12 * b + 48 + Select ;
m MidiCps 0 c 1 % VarSaw Tanh Splay2 2 /
