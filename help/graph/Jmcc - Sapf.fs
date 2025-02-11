"((.4 0 lfsaw) 2 * ([8 7.23] 0 lfsaw) .25 * 9.667 + + exp2) 0 sinosc .04 * .2 0 4 combn play"
0.4 0 lfsaw 2 * [ 8 7.23 ] 0 lfsaw 0.25 * 9.667 + + exp2 0 sinosc 0.04 * 0.2 0.2 4 combn

"(([.4 [8 7.23]] 0 lfsaw) [24 3] * +/ 81 + nnhz) 0 sinosc 4c * .2 0 4 combn play"
[ 0.4 [ 8 7.23 ] ] 0 lfsaw [ 24 3 ] * +/ 81 + nnhz 0 sinosc 0.04 * 0.2 0.2 4 combn

".2 0 11 31 lfo [0 3c] .1 lfpulse 1h .1 rlpf .4 clip2 play"
0.2 0 11 31 lfo [ 0 0.03 ] 0.1 lfpulse 100 0.1 rlpf 0.4 clip2
