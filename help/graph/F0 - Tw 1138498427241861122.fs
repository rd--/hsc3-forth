\ F0 - Tw 1138498427241861122
: f 9 sqrt ;
: b [ f 9.999 ] ;
: d b 99 / 0 SinOscFb 0 Max ;
: t1 9 999 / 0 SinOscFb 0 SinOscFb 9 99 / 0 SinOscFb > f 9 b / Select2 ;
: t2 9 99 / 0 SinOscFb 99 9999 / 0 SinOscFb < b 9 / f Select2 ;
: f1 9.9 b * t1 * t2 * 0.1 Lag ;
: s1 f1 d SinOscFb b 9 / 0 SinOscFb * d * ;
: c s1 9 b / 9 b sqrt / b 999 / b 99 / PitchShift ;
: s2 c d * d * d * 99 9 9 999 / 0.5 15 1 0.7 0.5 300 GVerb ;
: s3 s2 9 / c f 9 / f 9 / 0 0 PitchShift + ;
s3 Splay2 9 Hpf transpose Mix 0.5 *
