\ https://sccode.org/1-4Qy ; f0 ; 0299
: c 3 [ 2 3 ] [ 0 1 ] LfSaw ^ ;
: p 1 16 / 0 LfSaw c RoundTo 3 + 33 * c Ceil * 3 c 0.1 Lag ^ 66 * 3 c ^ 99 * Formant ;
: q c 3 + 1 c - 3000 * 880 Formant ;
p q * 3 /