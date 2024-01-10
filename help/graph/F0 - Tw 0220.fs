\ F0 - Tw 0220
: f0Tw0220
    1 90000 BufAlloc BufClear { b }
    0 1 InFeedback { c }
    3 0 SinOsc b c 3 + 2 12 0 0.1 4 2 TGrains { g }
    b 0 1 0 1 1 1 0 c RecordBuf { r }
    99 c 6 * SinOsc 9 / g + 9 Hpf 0.25 * { o }
    o r <!
;
f0Tw0220
