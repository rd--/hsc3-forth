\ F0 - Tw 1479212835192332289
: q { freq } freq 1 -1 -0.75 0 QuadC ;
: q1 5 q ;
: b [ 5 2.5 6 ] ;
: q2 b 15 / q q1 * ;
: e 1 15 / ;
: q3 500 q q1 * 99 / ;
: q4 999 b * q b 15 / q 0 Max * 999 / b q * ;
: q5 [ 12 16 ] 99 * q 0.1 1 -1 -0.75 [ -0.5 -1 ] QuadC 0 Max * 15 q 0 Max * 15 / ;
: q6 99 b * 1 1 b / [ 15 5 15 ] / q 0 > 0.375 * 1.25 - -0.75 0 QuadC Tanh q2 * 0.5 q2 * 1 Pan2 Sum 2 / ;
q3 q4 Splay2 + q5 q6 + e e 15 AllpassN +
