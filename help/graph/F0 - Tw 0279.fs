\ https://sccode.org/1-4Qy ; f0 ; 0279
: b [ 1 3 6 8 10 ] ;
: m b asLocalBuf 1 b / 99 / 0 LfSaw pi / 0 LfSaw 12 * 18 + b + 12 DegreeToKey ;
: f m MidiCps 0.2 0.2 1 CombN ;
f 0 LfSaw Splay2 0.6 *
