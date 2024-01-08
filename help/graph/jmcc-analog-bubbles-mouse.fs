\ analog bubbles with mouse control (Jmcc) #3
2 40 1 0.2 MouseX.kr ( lfo 2 rate )
0 LfSaw.kr -3 * 80 + ( lfo 2 = depth & offset in semitones )
0.1 10 1 0.2 MouseY.kr ( lfo 1 rate )
0 LfSaw.kr 24 * + ( depth in semitones, offset is lfo_2 )
MidiCps ( convert to frequency )
0 SinOsc.ar 0.04 *
0.2 0.2 4 CombN ( echoing sine wave )
