\ lz-bf (rd)
: x 1 12 0 0.1 MouseX.kr ;
: param { p q } x LFNoise0.kr p q MulAdd ;
: l SampleRate.ir 2 12 param 20 38 param 1.5 3 param [ 0.05 0.025 ] 0.1 0 0 LorenzL.ar ;
: p 0 l 24 * 0 BufRateScale.kr * 0 0 BufFrames.kr 0 Phasor.ar ;
0 s" /home/rohan/data/audio/pf-c5.aif" 0 0 b_allocRead
0 p noLoop 4 1 BufRd.ar
