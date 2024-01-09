\ https://sccode.org/1-4Qy ; f0 ; 0289
: b 1 50 to ;
: t b 49 / 0 Impulse ;
: o 33 b * t 0.005 0.2 LagUd 99 * SinOscFb t 0.007 1 LagUd * ;
o Sum 299 9 0.01 0.5 6 1 1 1 300 GVerb 0.1 *
