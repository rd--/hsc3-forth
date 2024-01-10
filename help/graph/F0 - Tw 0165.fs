\ F0 - Tw 0165
: s [ 1 0 ] 1 InFeedback 0.995 LeakDc 1 0.01 Limiter ;
: dt 1 [ 6 8 ] / 0 LfTri 0.4 * 0.5 + ;
s 2000 1 BLowPass 0 0 Impulse + 1 dt 1 CombC 0.99 *
