\ mridangam (jmcc) #SPE3
: drum { a } WhiteNoise.ar 70 * a 0.002 0.1 Decay2 * 261.62557 0.02 Resonz 4 * Distort 0.4 * ;
: drone 60 60.04 2 mce midiCPS Saw.ar 67 67.04 2 mce midiCPS Saw.ar + 4186.00904 LPF 0.007 * ;
: ten 10 mce Dseq.dr ;
: a_solo
    30
    1 0.9 0 0 0.7 0 0.2 0 0.7 0 0 ten
    1 0.9 0.2 0 0.7 0 0.2 0 0.7 0 0 ten
    1 0.9 0 0 0.7 0 0.2 0 0.7 0 0.2 ten
    1 0.9 0 0 0.7 0.2 0.2 0 0.7 0 0 ten
    1 0.9 0 0 0.7 0 0.2 0.2 0.7 0.2 0 ten
    1 0.9 0.2 0.2 0.7 0.2 0.2 0.2 0.7 0.2 0.2 ten
    1 0.9 0.2 0.2 0.7 0.2 0.2 0.2 0.7 0 0 ten
    1 0.9 0 0 0.7 0.2 0.2 0.2 0.7 0 0 ten
    1 0.9 0 0.4 0 0.4 0 0.4 0 0.4 0 ten
    1 0.9 0 0 0.4 0 0 0.4 0.2 0.4 0.2 ten
    1 0.9 0 0.2 0.7 0 0.2 0 0.7 0 0 ten
    1 0.9 0 0 0.7 0 0 0 0.7 0 0 ten
    1 0.9 0.7 0.7 0 0 0.2 0.2 0.2 0 0 ten
    1 0.9 0 0 0 0 0 0 0 0 0 ten
    14 mce Drand.dr
;
: a_seq
    1
    10 0 Dseq.dr
    2 0.9 0 0 0 0 0 0 0 0 0 ten
    2 0.9 0 0 0.2 0 0 0 0.2 0 0 ten
    2 0.9 0 0 0.2 0 0.2 0 0.2 0 0 ten
    2 0.9 0 0 0.2 0 0 0 0.2 0 0.2 ten
    a_solo
    3 2 0 0.2 0.5 0 0.2 0.9 1.5 0 0.2 0.5 0 0.2 0.9 1.5 0 0.2 0.5 0 0.2 20 mce Dseq.dr
    1 5 Dseq.dr
    8 mce Dseq.dr
;
1 8 / 0 do-nothing 1 a_seq Dseq.dr 0 TDuty.ar drum drone +
