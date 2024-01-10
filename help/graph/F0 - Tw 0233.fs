\ F0 - T2 0233
: b 1 [ 1 4 6 8 11 ] / ;
: c b 98 / 0 LfTri ;
: s 1 b / 59 + asLocalBuf b 99 / 0 LfTri c + 5 * 0 1 1 BufRd ;
: q inf s Dseq MidiCps ;
b c 0 q Duty c + 0 LfTri 2 / Splay2
