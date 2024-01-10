\ F0 - Tw 1254441448327479299
: b [ 1 3 5 8 10 ] ;
: e [ 3 2 3 / 4 3 2 / 2 ] ;
: c 0.021 ;
: d b 999 / 0 LfTri 1 % ;
: m b c * 0 LfTri ;
: l m 7 * 20 + inf b m 5 * 6 + % Dseq + ;
: n e 12 m ^ / 0 0 l Duty ;
: f b asLocalBuf n 12 DegreeToKey MidiCps ;
: z [ 2 3 / 1.5 3 1.5 3 ] 0 Impulse c d Decay2 ;
: o f c b / 0 LfTri 1 + 3 / SinOscFb z * d * ;
o Splay2 0.1 1 0.5 FreeVerb
