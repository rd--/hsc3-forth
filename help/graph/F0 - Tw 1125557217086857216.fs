\ F0 - Tw 1125557217086857216
: scl [ 0 2.94 4.98 7.02 9.96 ] ;
: b -7 6 upTo 0.7 + 2 * 666 / ;
: m scl asLocalBuf b b LfTri b 0 LfTri * 9 * 9 + 32 % 12 DegreeToKey 24 + ;
: e m 440 1 3 / AmpComp b b LfTri * b * 9 * ;
: o m MidiCps 0 b 0 LfTri 1 + 2 / VarSaw e * ;
: s o m 1 b / 1 % Lag2 3 * 1 Rlpf ;
s 0.3 0.2 b - 3 AllpassN Splay2 Tanh
