\ https://sccode.org/1-4Qy ; f0 ; 0251
: b 1 8 upTo ;
: h b 64 / [ 4 8 14 ] b 4 / SyncSaw 1 + SyncSaw 0 Max ;
: o 99 b * b 2.01 * SyncSaw h % ;
o Sum 9 Hpf 1 0.01 Limiter 2 / 70 4 0.5 0.5 15 1 0.7 0.5 300 GVerb
