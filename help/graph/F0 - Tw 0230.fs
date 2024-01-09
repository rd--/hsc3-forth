\ https://sccode.org/1-4Qy ; f0 ; 0230
: b 1 9 upTo 16 / ;
: e b 0 LfSaw b 999 * 1.25 Ringz ;
: d 1 b 999 * / b 120 / b 2 * LfTri 1 % * ;
PinkNoise e * 1 d 3 CombC Splay2 99 /
