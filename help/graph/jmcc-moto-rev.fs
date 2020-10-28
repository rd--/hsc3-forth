\ moto rev (jmcc) #1
0.2 0 SinOsc.kr 10 * 21 + ( freq )
0 0.1 2 mce 0.1 LFPulse.ar ( osc )
100 0.1 RLPF 0.4 swap Clip2
