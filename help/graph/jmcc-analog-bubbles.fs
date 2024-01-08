\ analog bubbles (Jmcc) #1
[ 8 7.23 ] 0 LfSaw.kr 3 * 80 + ( offset )
0.4 0 LfSaw.kr 24 * + MidiCps ( freq )
0 SinOsc.ar 0.04 * 0.2 0.2 4 CombN

\ analog bubbles (Jmcc) #1
[ 8 7.23 ] 0 lfsaw 3 * 80 + 0.4 0 lfsaw 24 * + midicps 0 sinosc 0.04 * 0.2 0.2 4 combn

\ analog bubbles (Jmcc) #1 ; sapf
0.4 0 lfsaw 2 * [ 8 7.23 ] 0 lfsaw 0.25 * 9.667 + + exp2 0 sinosc 0.04 * 0.2 0.2 4 combn

\ analog bubbles (Jmcc) #1
: offset [ 8 7.23 ] 0 lfsaw 3 * 80 + ;
: freq offset 0.4 0 lfsaw 24 * + midicps ;
freq 0 sinosc 0.04 * 0.2 0.2 4 combn

\ analog bubbles (Jmcc) #1
: o [ 8 7.23 ] 0 lfsaw 3 * 80 + ;
: m 0.4 0 lfsaw 24 * o + ;
: s m midicps 0 sinosc 0.05 * ;
s 0.2 0.2 4 combn 0.1 *
