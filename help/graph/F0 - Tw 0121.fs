\ tw 0121 (f0)
: z 1 [ 8 7 ] / 0 SinOsc 1 30 / 0 SinOsc * 9 * ;
: l 0 7 upTo 6 * 56 + asLocalBuf ;
: m 1 [ 4 3 ] / 0 SinOsc ;
: o l z 0 1 1 BufRd MidiCps 0 SinOsc m * ;
o 1 1 [ 6 5 ] / 9 CombN Tanh 0.1 *
