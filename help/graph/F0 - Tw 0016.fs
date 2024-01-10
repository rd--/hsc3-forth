\ F0 - Tw 0016
: f0Tw0016
    2 90000 BufAlloc BufClear { b }
    b 12 Saw 30000 * 40000 + 1 2 2 BufRd 90000 Sweep { z }
    [ 8 9 ] Saw b z 1 BufWrite { w }
    b z 1 2 2 BufRd 2 / w <! transpose Sum
;
f0Tw0016
