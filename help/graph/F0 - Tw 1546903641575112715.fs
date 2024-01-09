\ F0 - Tw 1546903641575112715
: b [ 0 3.084 5.028 6.972 10.056 ] ;
: k b asLocalBuf 2 b 1 + 32 / b 1 + 0.5 0 StandardL b 7 / 1 + 0.5 0 StandardL ^ 12 * 12 DegreeToKey ;
: f1 k 36 + 1 32 / 1 0.5 0 StandardL 0 > - MidiCps ;
: f2 b 8 / 8 + b 5 / 1 + 0.5 0 StandardL ;
: fb 1 3 / b 8 / 1 + 0.5 0 StandardL 1 + 3 / ;
: a b 2 + 32 / b 9 / 1 + 0.5 0 StandardL 0 Max 16 / f1 60 MidiCps 1 3 / AmpComp * ;
: x f1 f2 + fb SinOscFb a * Splay2 ;
x x 99 3 0.5 0.5 15 1 0.7 0.5 300 GVerb transpose Sum +
