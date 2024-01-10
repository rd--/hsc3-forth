\ SinOsc ; Frequency of 440hz with annotated rate
440 0 SinOsc.ar 0.1 *

\ SinOsc ; implicit rate
440 0 SinOsc 0.1 *

\ SinOsc ; 440hz (left) and 441hz (right)
[ 440 441 ] 0 SinOsc 0.1 *

\ SinOsc ; Pair amplitude modulated by SinOsc
[ 440 441 ] 0 SinOsc 1 0 SinOsc.kr 0.1 * abs *

\ SinOsc ; Helper
: f { x } [ x x 1 + ] 0 SinOsc ;
333 f 555 f + 0.1 *
