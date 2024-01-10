\ F0 - Tw 0340
: c 1 [ 12 8 ] / 0 SinOscFb 3 + 24 / ;
: m c 0 0 inf [ 0 8 5 1 5 4 5 ] c 18 * 1 RoundTo * Dseq Duty 60 + ;
m MidiCps c 2 * SinOscFb 0.2 0.2 1 AllpassN 2 /
