\ narrow band filtered crackle noise (jmcc) #2

: nbfcn
    1.97 crackle.ar 0 0.03 rand.ir 0.15 * + 0 2000 rand.ir 80 + { rf1 }
    rf1 rf1 -0.2 0.2 rand.ir rf1 *
    + 9 0 xline.kr 0.2 resonz -1 1 rand.ir 1 1 0 1 2 2 5 2 1 env-linen envgen.ar pan2
;
nbfcn 2 inf spawn-texture
