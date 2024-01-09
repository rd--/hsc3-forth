\ tw 0120 (f0)
: z 1 [ 7 8 ] / 0 LfTri 1 9 / 0 LfTri * 99 * ;
: l 60 79 upTo MidiCps asLocalBuf ;
: f l z 0 1 1 BufRd ;
: w 1 [ 3 4 ] / 0 LfTri 1 % ;
: o f 0 w VarSaw ;
o 1 1 [ 5 6 ] / 8 CombN 8 /
