\ F0 - Tw 0224
: f0Tw0224
    200000 { c }
    2 c BufAlloc BufClear { b }
    b [ 2 3 ] 9 * 0 SinOsc c * 0 2 2 BufRd { d }
    b [ 99 145 ] 0 SinOsc Abs c * 1 3 [ 2 3 ] / 0 SinOsc 3 / BufWr { w }
    d transpose Mix 0.5 * w <!
;
f0Tw0224
