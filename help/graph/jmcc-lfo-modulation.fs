\ Lfo modulation (Jmcc) #1
[ 0.6 0.7 ] 0 FSinOsc.kr 3600 * 4000 + ( p )
0.05 0 FSinOsc.kr 80 * 160 + ( o )
0 0.4 LfPulse.ar 0.05 * swap 0.2 Rlpf ( s )
0.3 [ 0.2 0.25 ] 2 CombL
