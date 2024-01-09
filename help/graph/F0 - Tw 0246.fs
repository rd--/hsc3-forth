\ https://sccode.org/1-4Qy ; f0 ; 0246
: b 1 9 upTo 1.5 * ;
: d [ 2 b / 3 b / ] ;
: e d 0 0.5 LfPulse ;
e 1 b / 0 0.5 LfPulse b * 50 + MidiCps b 9 / Ringz Sum 0.0002 * 2 2 e 50 Lpf - 1 CombC transpose Mix
