\ drummer (tm)
: n WhiteNoise.ar ;
: tempo 4 ;
: tr tempo 0 Impulse.ar ;
: tr_2 tr 4 2 PulseDivider ;
: tr_4 tr 4 0 PulseDivider ;
: snare tr_2 0.005 0.5 Decay2 n * ;
: bass 60 0 SinOsc.ar tr_4 0.005 0.5 Decay2 * ;
: hihat n 10000 HPF tr 0.005 0.5 Decay2 * ;
snare bass + hihat + 0 0.4 Pan2
