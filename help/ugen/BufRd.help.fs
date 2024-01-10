: bufRdHelp
    2 48000 * { numFrames }
    1 numFrames BufAlloc BufClear { buf }
    2 LfNoise1.kr 300 * 400 + 0 SinOsc 0.1 * { osc }
    buf 0 buf BufRateScale 0 buf BufFrames 0 Phasor 1 osc BufWr { wr }
    buf 0.1 1 SinOsc buf BufFrames * 1 2 1 BufRd wr <!
;
bufRdHelp
