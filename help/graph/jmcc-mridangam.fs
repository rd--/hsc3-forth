\ Mridangam (Jmcc) #SPE3
: drum { a } WhiteNoise.ar 70 * a 0.002 0.1 Decay2 * 261.62557 0.02 Resonz 4 * Distort 0.4 * ;
: drone [ 60 60.04 ] MidiCps Saw.ar [ 67 67.04 ] MidiCps Saw.ar + 4186.00904 Lpf 0.007 * ;
: a_solo
    30
    [
    1 [ 0.9 0 0 0.7 0 0.2 0 0.7 0 0 ] Dseq.dr
    1 [ 0.9 0.2 0 0.7 0 0.2 0 0.7 0 0 ] Dseq.dr
    1 [ 0.9 0 0 0.7 0 0.2 0 0.7 0 0.2 ] Dseq.dr
    1 [ 0.9 0 0 0.7 0.2 0.2 0 0.7 0 0 ] Dseq.dr
    1 [ 0.9 0 0 0.7 0 0.2 0.2 0.7 0.2 0 ] Dseq.dr
    1 [ 0.9 0.2 0.2 0.7 0.2 0.2 0.2 0.7 0.2 0.2 ] Dseq.dr
    1 [ 0.9 0.2 0.2 0.7 0.2 0.2 0.2 0.7 0 0 ] Dseq.dr
    1 [ 0.9 0 0 0.7 0.2 0.2 0.2 0.7 0 0 ] Dseq.dr
    1 [ 0.9 0 0.4 0 0.4 0 0.4 0 0.4 0 ] Dseq.dr
    1 [ 0.9 0 0 0.4 0 0 0.4 0.2 0.4 0.2 ] Dseq.dr
    1 [ 0.9 0 0.2 0.7 0 0.2 0 0.7 0 0 ] Dseq.dr
    1 [ 0.9 0 0 0.7 0 0 0 0.7 0 0 ] Dseq.dr
    1 [ 0.9 0.7 0.7 0 0 0.2 0.2 0.2 0 0 ] Dseq.dr
    1 [ 0.9 0 0 0 0 0 0 0 0 0 ] Dseq.dr
    ] Drand.dr
;
: a_seq
    1
    [
    10 0 Dseq.dr
    2 [ 0.9 0 0 0 0 0 0 0 0 0 ] Dseq.dr
    2 [ 0.9 0 0 0.2 0 0 0 0.2 0 0 ] Dseq.dr
    2 [ 0.9 0 0 0.2 0 0.2 0 0.2 0 0 ] Dseq.dr
    2 [ 0.9 0 0 0.2 0 0 0 0.2 0 0.2 ] Dseq.dr
    a_solo
    3 [ 2 0 0.2 0.5 0 0.2 0.9 1.5 0 0.2 0.5 0 0.2 0.9 1.5 0 0.2 0.5 0 0.2 ] Dseq.dr
    1 5 Dseq.dr
    ] Dseq.dr
;
1 8 / 0 doNothing 1 a_seq Dseq.dr 0 TDuty.ar drum drone +
